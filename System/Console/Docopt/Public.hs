module System.Console.Docopt.Public 
  (
    -- everything locally declared
    module System.Console.Docopt.Public,

    -- public types
    Option(),
    Arguments(),
    ParsingOptions(..),

    -- everything else
    defaultParsingOptions
  )
  where

import System.Environment (getArgs)
import System.Exit

import Data.Map as M hiding (null)

import Control.Applicative

import System.Console.Docopt.ParseUtils
import System.Console.Docopt.Types
import System.Console.Docopt.UsageParse (pDocopt)
import System.Console.Docopt.OptParse (getArguments)


-- * Public API

-- ** Main option parsing entry points

optionsWithUsage :: ParsingOptions -> String -> IO Options
optionsWithUsage options usage =
    do rawargs <- getArgs
       case runParser pDocopt M.empty "" usage of
           Left err  -> failure err
           Right dop -> case getOptions dop rawargs of
               Left err         -> failure err
               Right parsedOpts -> return parsedOpts

    where failure err = if showParseErrors options
                        then fail $ show err
                        else do putStrLn usage
                                exitFailure

optionsWithUsageFile :: ParsingOptions -> FilePath -> IO Options
optionsWithUsageFile options path = do usage <- readFile path
                                       optionsWithUsage options usage

-- ** Option lookup methods

isPresent :: Arguments -> Option -> Bool
isPresent args opt = 
  case opt `M.lookup` args of
    Nothing  -> False
    Just val -> case val of
      NoValue    -> False
      NotPresent -> False
      _          -> True

isPresentM :: Monad m => Arguments -> Option -> m Bool
isPresentM args o = return $ isPresent args o

notPresent :: Arguments -> Option -> Bool
notPresent args o = not $ isPresent args o

notPresentM :: Monad m => Arguments -> Option -> m Bool
notPresentM args o = return $ not $ isPresent args o

getArg :: Monad m => Arguments -> Option -> m String
getArg args opt = 
  let failure = fail $ "no argument given: " ++ show opt
  in  case opt `M.lookup` args of
        Nothing  -> failure
        Just val -> case val of
          MultiValue (v:vs) -> return v
          Value v           -> return v
          _                 -> failure          

getFirstArg :: Monad m => Arguments -> Option -> m String
getFirstArg args opt = 
  let failure = fail $ "no argument given: " ++ show opt
  in  case opt `M.lookup` args of
        Nothing  -> failure
        Just val -> case val of
          MultiValue vs -> if null vs then failure else return $ last vs
          Value v       -> return v
          _             -> failure          


getArgWithDefault :: Arguments -> String -> Option -> String
getArgWithDefault args def opt = 
  case args `getArg` opt of
    Just val -> val
    Nothing -> def

getAllArgs :: Arguments -> Option -> [String]
getAllArgs args opt = 
  case opt `M.lookup` args of
     Nothing  -> []
     Just val -> case val of
       MultiValue vs -> reverse vs
       Value v       -> [v] 
       _             -> []

getAllArgsM :: Monad m => Arguments -> Option -> m [String]
getAllArgsM o e = return $ getAllArgs o e


-- ** Public Option constructor functions

command :: String -> Option
command s = Command s

argument :: String -> Option
argument s = Argument s

shortOption :: Char -> Option
shortOption c = ShortOption c

longOption :: String -> Option
longOption s = LongOption s
