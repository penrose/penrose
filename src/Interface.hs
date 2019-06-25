module Interface where
{-# OPTIONS_HADDOCK prune #-}

import           Control.Exception          (ErrorCall, try)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as B
import           Element
import           Env
import           GenOptProblem
import qualified Optimizer
import           Plugins
import           Serializer
import           Style
import           Substance
import           Sugarer
import           System.IO.Unsafe           (unsafePerformIO)
import           Utils

-- | Given Substance, Style, and Element programs, output an initial state.
-- TODO: allow cached intermediate outputs such as ASTs to be passed in?
compileTrio ::
     String -- ^ a Substance program
  -> String -- ^ a Style program
  -> String -- ^ an Element program
  -> Either CompilerError (State, VarEnv) -- ^ an initial state and compiler context for language services
compileTrio substance style element
  -- Parsing and desugaring phase
 = do
  env <- parseElement "" element
  styProg <- parseStyle "" style env
  let subDesugared = sugarStmts substance env -- TODO: errors?
  subOut@(SubOut _ (subEnv, _) _) <- parseSubstance "" subDesugared env
  -- Plugin phase
  pluginRes <- runPlugin subOut style env
  (subOut', styVals) <-
    case pluginRes of
      Nothing -> pure $ (subOut, [])
      Just (subPlugin, styVals) -> do
        subOutPlugin <-
          parseSubstance "" (subDesugared ++ "\n" ++ subPlugin) env
        return (subOutPlugin, styVals)
  -- Compilation phase
  let optConfig = defaultOptConfig
  let styRes =
        unsafePerformIO $ -- HACK: rewrite this such that it's safe
        try (compileStyle styProg subOut' styVals optConfig) :: Either ErrorCall State
  case styRes of
    Right initState -> Right (initState, subEnv)
    Left styRTError -> Left $ StyleTypecheck $ show styRTError

getEnv ::
     String -- ^ a Substance program
  -> String -- ^ an Element program
  -> Either CompilerError VarEnv -- ^ either a compiler error or an environment of the Substance program
getEnv substance element = do
  env <- parseElement "" element
  let subDesugared = sugarStmts substance env -- TODO: errors?
  subOut@(SubOut _ (subEnv, _) _) <- parseSubstance "" subDesugared env
  Right $ subEnv

step ::
     State -- ^ the initial state
  -> Int -- ^ the number of steps n for the optimizer to take
  -> Either RuntimeError State -- ^ the resulting state after the optimizer takes n steps
  -- TODO: rewrite runtime error reporting
step initState steps = Right $ iterate Optimizer.step initState !! (steps + 1) -- `iterate` applies `id` the first time

stepUntilConvergence ::
     State -- ^ the initial state
  -> Either RuntimeError State -- ^ the converged state or optimizer errors
stepUntilConvergence state
  | optStatus (paramsr state) == EPConverged = Right state
  -- TODO: rewrite runtime error reporting
  | otherwise = stepUntilConvergence $ Optimizer.step state

resample ::
     State -- ^ the initial state
  -> Int -- ^ number of samples to choose from (> 0). If it's 1, no selection will occur
  -> Either RuntimeError State -- ^ if the number of samples requested is smaller than 1, return error, else return the resulting state
resample initState numSamples
  | numSamples >= 1 =
    let newState = resampleBest numSamples initState
        (newShapes, _, _) = evalTranslation newState
    in Right $ newState {shapesr = newShapes}
  | otherwise = Left $ RuntimeError "At least 1 sample should be requested."

--------------------------------------------------------------------------------
-- Test
subFile = "sub/tree.sub"

styFile = "sty/venn.sty"

elmFile = "set-theory-domain/setTheory.dsl"

testCompile :: IO ()
testCompile = do
  sub <- readFile subFile
  sty <- readFile styFile
  elm <- readFile elmFile
  let res = compileTrio sub sty elm
  case res of
    Right state -> B.writeFile "state.json" $ A.encode state
    Left err    -> putStrLn $ show err

testStep :: Bool -> IO ()
testStep converge
  | converge = do
    sub <- readFile subFile
    sty <- readFile styFile
    elm <- readFile elmFile
    let s = compileTrio sub sty elm
    case s of
      Right (state, _) ->
        let res = stepUntilConvergence state
        in case res of
             Right state' -> B.writeFile "state-step.json" $ A.encode state'
             Left err     -> putStrLn $ show err
      Left err -> putStrLn $ show err
  | otherwise = do
    sub <- readFile subFile
    sty <- readFile styFile
    elm <- readFile elmFile
    let s = compileTrio sub sty elm
    case s of
      Right (state, _) ->
        let res = step state 2
        in case res of
             Right state' -> B.writeFile "state-step.json" $ A.encode state'
             Left err     -> putStrLn $ show err
      Left err -> putStrLn $ show err
