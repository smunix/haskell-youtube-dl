module Main where

import System.IO
import System.Console.GetOpt
import System.Environment
import System.Directory
import System.Process

import Control.Exception
import Control.Monad
import Control.Concurrent

import Lib

data Options = Options {
  optInputFile :: Maybe String,
  optOutputDir :: Maybe String
} deriving (Show)

options :: [OptDescr (Options -> IO Options)]
options = [ Option "i" ["input-file"] (ReqArg (\s -> \o -> return $ o {optInputFile = Just s}) "FILE") "input file", Option "o" ["output-dir"] (ReqArg (\s -> \o -> return $ o {optOutputDir = Just s}) "DIR") "output directory" ]

startOptions :: Options
startOptions = Options { optInputFile = Nothing, optOutputDir = Nothing }

-- download :: MVar Int -> String -> String -> (MVar Int -> a -> IO ()) -> IO ()
download fvar ifo odo act = do 
  txt <- readFile ifo
  let lns = ((zip [1..]) . lines) txt
  vfvar <- takeMVar fvar
  putMVar fvar (length lns) >> mapM_ (act) lns

-- runOnFile :: MVar Int -> Options -> (MVar Int -> a -> IO ()) -> IO ()
runOnFile fvar o f = do
  let Options { optInputFile = ifo, optOutputDir = odo } = o
  case ifo of
    Just ifo' -> case odo of
                   Just odo' -> download fvar ifo' odo' f
                   _ -> return ()
    _ -> return ()

-- create thread that will run youtube-dl
mkdir :: String -> IO ()
mkdir d = makeAbsolute d >>= createDirectoryIfMissing True 

ytbDl :: MVar Int -> Options -> (Int, String) -> IO ()
ytbDl fvar o l = do
  -- print l
  case optOutputDir o of
    Just o' -> do od <- (makeAbsolute . (\x -> o' ++ "/" ++ x) . show . fst) l
                  forkIO (do { print od;
                               mkdir od;
                               bracket 
                                 (openFile (od ++ "/" ++ (show . fst $ l) ++ ".log") WriteMode)
                                 (hClose)
                                 (\lgh -> do 
                                   (_, _, _, ph) <-  createProcess $ (proc "youtube-dl" [(snd l)]) { cwd = Just od , std_out = UseHandle lgh }
                                   print $ show (fst l) ++ " started youtube-dl " ++ (snd l)
                                   waitForProcess ph
                                   lvar <- takeMVar fvar
                                   print $ "finished - " ++ show (lvar - 1) ++ " .. " ++ (snd l)
                                   putMVar fvar $ lvar - 1
                                   return ()
                                   )
                   })
                  return ()
    _ -> return ()
    
-- unlessM :: m Bool -> m () -> m ()
whileM cond akt = cond >>= \c -> if c
                                      then do akt
                                              whileM cond akt
                                      else return ()
-- main :: IO ()
main = do
  axs <- getArgs
  let (actions, nonoptions, errors) = getOpt Permute options axs
  fvar <- newMVar 1000
  foldl (>>=) (return startOptions) actions >>= \o -> runOnFile fvar o (ytbDl fvar o) 
  whileM (do lvar <- takeMVar fvar
             print $ "checking fvar = " ++ show (lvar)
             putMVar fvar lvar
             return $ lvar /= 0) (do print "waiting 10 sec ..."
                                     threadDelay 10000000)
