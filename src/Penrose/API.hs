{-# LANGUAGE DeriveGeneric #-}

module Penrose.API
  ( compileTrio
  , step
  , stepUntilConvergence
  , reconcileNext
  , getEnv
  , getVersion
  , resample
  , energyValues
  , APICall(..)
  ) where

import           Control.Exception          (ErrorCall, try)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.List                  (deleteFirstsBy, partition, (\\))
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (mapMaybe)
import           Data.Version               (showVersion)
import           Debug.Trace                (trace, traceShowId)
import           GHC.Generics
import           Paths_penrose              (version)
import           Penrose.Element
import           Penrose.Env
import           Penrose.Functions
import           Penrose.GenOptProblem
import qualified Penrose.Optimizer          as Optimizer
import           Penrose.Plugins
import           Penrose.Serializer
import           Penrose.Shapes
import           Penrose.Style
import           Penrose.Substance
import           Penrose.Sugarer
import           Penrose.Util
import           System.IO.Unsafe           (unsafePerformIO)
import           Text.Show.Pretty      (pPrint, ppShow)

--------------------------------------------------------------------------------
-- Types for decoding API calls
data APICall
  = Step Int
         State
  | Resample Int
             State
  | StepUntilConvergence State
  | CompileTrio String
                String
                String
  | ReconcileNext State
                  String
                  String
                  String
  | GetEnv String
           String
  | EnergyValues State
  | GetVersion
  deriving (Generic)

instance A.FromJSON APICall

instance A.ToJSON APICall

--------------------------------------------------------------------------------
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
      Nothing -> pure (subOut, [])
      Just (subPlugin, styVals) -> do
        subOutPlugin <-
          parseSubstance "" (subDesugared ++ "\n" ++ subPlugin) env
        return (subOutPlugin, styVals)
  -- Compilation phase
  let optConfig = defaultOptConfig
  -- COMBAK: revert
  state <- trace ("Style AST: \n" ++ ppShow styProg) $ compileStyle styProg subOut' styVals optConfig
  return (state, env)

-- | Given Substance and ELement programs, return a context after parsing Substance and ELement.
getEnv ::
     String -- ^ a Substance program
  -> String -- ^ an Element program
  -> Either CompilerError VarEnv -- ^ either a compiler error or an environment of the Substance program
getEnv substance element = do
  env <- parseElement "" element
  let subDesugared = sugarStmts substance env -- TODO: errors?
  subOut@(SubOut _ (subEnv, _) _) <- parseSubstance "" subDesugared env
  Right subEnv

-- | Take n steps in the optimizer and return a new state
step ::
     State -- ^ the initial state
  -> Int -- ^ the number of steps n for the optimizer to take
  -> Either RuntimeError State -- ^ the resulting state after the optimizer takes n steps
  -- TODO: rewrite runtime error reporting
step initState steps = Right $ iterate Optimizer.step initState !! (steps + 1) -- `iterate` applies `id` the first time

-- | Take multiple steps until the optimizer converges
stepUntilConvergence ::
     State -- ^ the initial state
  -> Either RuntimeError State -- ^ the converged state or optimizer errors
stepUntilConvergence state
  | optStatus (paramsr state) == EPConverged = Right state
  -- TODO: rewrite runtime error reporting
  | otherwise = stepUntilConvergence $ Optimizer.step state

-- | Resample the current state and return the new initial state
resample ::
     State -- ^ the initial state
  -> Int -- ^ number of samples to choose from (> 0). If it's 1, no selection will occur
  -> Either RuntimeError State -- ^ if the number of samples requested is smaller than 1, return error, else return the resulting state
resample initState numSamples
  | numSamples == 1 = Right $ resampleOne initState 
  | numSamples > 1 =
     -- NOTE: resampling will not work if the frontend has a new function name (issue #352) because of evalEnergyOn for resampling
    Left $ RuntimeError "Only 1 resample supported."
    -- NOTE: Deprecated since we've deprecated backend `evalExpr`
    -- let newState = resampleBest numSamples initState
    --     (newShapes, _, _) = evalTranslation newState
    -- in Right $ newState {shapesr = newShapes}
  | otherwise = Left $ RuntimeError "At least 1 sample should be requested."

getVersion :: String -- ^ the current version of the Penrose binary
getVersion = showVersion version

reconcileNext ::
     State -> String -> String -> String -> Either CompilerError (State, VarEnv)
reconcileNext prevState substance style element = do
  (state', varenv) <- compileTrio substance style element
  -- remove varyings from the new state
  let state = diffStates prevState state'
  -- find new fixed values from the previous state
  let fixedValues =
        mapMaybe (getFixed $ shapesr prevState) $ shapeProperties state
  -- insert these new fixed values into the new translation
  newTrans <- fromStyleErrs $ addPaths True (transr state) fixedValues
  -- generate a new set of shapes
  let newState = syncShapes $ state {transr = newTrans}
  return (newState, varenv)
  where
    getFixed shapes (name, field, property) =
      let shape = findShapeSafe (getShapeName name field) shapes
      in case shape of
           Nothing -> Nothing
           Just s ->
             Just (mkPath [name, field, property], Done $ s `get` property)

energyValues :: State -> ([Double], [Double])
energyValues s =
  let varyMap = mkVaryMap (varyingPaths s) (map r2f $ varyingState s)
      fns = objFns s ++ constrFns s
      (fnsE, transE, rng') =
        evalFns evalIterRange fns (castTranslation $ transr s) varyMap (rng s)
      penaltyWeight = r2f $ weight $ paramsr s
      (objfns, constrfns) = partition (\f -> optType_d f == Objfn) fnsE
  in ( map (applyOptFn objFuncDict objSignatures) objfns
     , map (applyOptFn constrFuncDict constrSignatures) constrfns)

--------------------------------------------------------------------------------
-- Helpers
-- | Produce a new state by diff'ing a state generated with new Substance statements and a previous state. This functions (1) moves all occurances of varying paths and corresponding entries in the varying state from the previous state and (2) remove all occurances of uninitialzed paths from the previous state. NOTE: this function is designed to be called only once per edit and is not idempotent.
diffStates :: State -> State -> State
diffStates prevState state =
  let propsWithFixed = zip (varyingPaths state) (varyingState state)
      (paths, vstate) =
        unzip $
        deleteFirstsBy
          (\p1 p2 -> fst p1 == fst p2)
          propsWithFixed
          (zip (varyingPaths prevState) (varyingState prevState))
  in state
     { varyingPaths = paths
     , varyingState = vstate
     , uninitializedPaths =
         uninitializedPaths state \\ uninitializedPaths prevState
     }

-- | Transform style compiler functions to be compatible with the uniformed error types
fromStyleErrs :: Either [Error] a -> Either CompilerError a
fromStyleErrs v =
  case v of
    Left errs -> Left $ StyleTypecheck errs
    Right res -> Right res

syncShapes :: State -> State
syncShapes state =
  let (shapes, _, _) =
        evalShapes
          evalIterRange
          (shapePaths state)
          (transr state)
          M.empty
          (rng state)
  in state {shapesr = shapes}

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
