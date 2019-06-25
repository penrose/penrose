{-# OPTIONS_HADDOCK prune #-}

module Plugins where

import           Control.Exception          (ErrorCall, try)
import           Data.Aeson                 (decode)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map.Strict            as M
import           Env
import           Style
import           Substance
import           SubstanceJSON
import           System.Directory           (getCurrentDirectory,
                                             setCurrentDirectory)
import           System.IO.Unsafe           (unsafePerformIO)
import           System.Process             (callCommand)
import           Utils

--------------------------------------------------------------------------------
-- | 'runPlugin' parses plugin statements from the style program and executes
-- the plugin if one is found. It returns 'Nothing' of there is no plugin found,
-- returns an error if more than one is found, or returns a new Substance program
-- and some Style values if the plugin runs successfully.
runPlugin ::
     SubOut
  -> String
  -> VarEnv
  -> Either CompilerError (Maybe (String, [StyVal]))
runPlugin subOut stySrc elementEnv
    -- Find Substance instantiator plugin (if it exists in Style file + directory)
 = do
  instantiations <- parsePlugins "" stySrc elementEnv
    -- If 1 instantiation, run the plugin, append the resulting Substance program, re-check the full program, and use it in the Style compiler.
    -- If >1 instantiation, throw an error.
  case instantiations of
    [] -> Right $ Nothing
    [pluginName] ->
      let res = unsafePerformIO $ try (instantiateSub pluginName subOut)
      in case res of
           Right (subPlugin, styVals) -> Right $ Just $ (subPlugin, styVals)
           Left err -> Left $ PluginRun $ show (err :: ErrorCall)
    _ ->
      Left $ PluginParse $ "Multiple plugins found in Style; only one allowed."

-- If no instantiations, proceed with Style compiler.
--   putStrLn $ "instantiations found: " ++ (show instantiations)
--------------------------------------------------------------------------------
-- | 'pluginDict' stores the mappings from plugin names to commands to run
-- First entry: plugin name
-- Second entry: plugin directory (relative path OK), no trailing "/"
-- Third entry: command to run, relative to plugin directory
-- TODO: the standard should probably include `configure` and `run`, not just `run`
pluginDict :: M.Map String (String, String)
pluginDict =
  M.fromList
    [ ("haskell-test", ("plugins/haskell-instantiator", "./Main"))
    , ("ddgjs", ("plugins/mesh-plugin", "node mesh-plugin.js"))
    , ("alloy", ("plugins/alloy", "java -cp \"*:.\" AlloyPlugin"))
    , ("raytracing", ("TODO: path", "TODO: command"))
    ]

--------------------------------------------------------------------------------
-- Substance instantiation / plugin calls
-- Don't forget to recompile the plugin!
type SubstanceRaw = String

-- TODO: add more error checking to deal with paths or files that don't exist
-- TODO: this functions requires "values.json", which is not outputed by some plugins
instantiateSub :: String -> SubOut -> IO (SubstanceRaw, [StyVal])
instantiateSub pluginName parsedSub = do
  originalDir <- getCurrentDirectory
  let (dirPath, pluginCmd) =
        catchPathError pluginName (M.lookup pluginName pluginDict)
  putStrLn $ "plugin directory: " ++ dirPath
  putStrLn $ "plugin command: " ++ pluginCmd
    -- NOTE: we are not expecting multiple processes to use these tempfiles
  let outFile = dirPath ++ "/Sub_enduser.json"
  let subInFile = dirPath ++ "/Sub_instantiated.sub"
  let styInFile = dirPath ++ "/values.json"
  writeSubstanceToJSON outFile parsedSub
  setCurrentDirectory dirPath -- Change to plugin dir so the plugin gets the right path. Otherwise pwd sees "penrose/src"
  callCommand pluginCmd
  setCurrentDirectory originalDir -- Return to original directory
  newSubProg <- readFile subInFile
  styVals <- readFile styInFile
  styVals' <- B.readFile styInFile
  putStrLn "Penrose received Sub file: "
  putStrLn newSubProg
  putStrLn "---------------------------"
  putStrLn "Penrose received Sty file: "
  putStrLn styVals
  let styRes = (decode styVals') :: Maybe [StyVal]
  let styJSON =
        case styRes of
          Nothing -> error "couldn't read plugin JSON"
          Just x  -> x
  return (newSubProg, styJSON)

catchPathError :: String -> Maybe (FilePath, String) -> (FilePath, String)
catchPathError name Nothing =
  error $ "path to plugin '" ++ name ++ "' doesn't exist!"
catchPathError _ (Just x) = x
