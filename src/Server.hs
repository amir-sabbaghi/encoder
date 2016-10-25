module Server ( server
              ) where

import Network.Simple.TCP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC (unpack)
import Data.List as L
import Data.Char
import Data.Word
import Data.Maybe (fromJust)
import Data.Map as M (Map, delete, (!), null, fromList)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TQueue
import Control.Monad
import Control.Monad.STM (atomically)
import Control.Exception
import System.Process
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import GHC.IO.Handle

import Packet

server :: String -> String -> IO ()
server port mport = do todo <- atomically $ newTQueue
                       done <- atomically $ newTQueue
                       film <- atomically $ newTQueue
                       forkIO $ managementServer mport todo film
                       forkIO $ mergeServer done film
                       queueServer port todo done

managementServer :: String -> TQueue (FilePath, String, Int, [(String, String)], Bool) -> TQueue (String, Int, [String]) -> IO ()
managementServer port todo film = serve HostAny port $ \(socket, remoteAddr) ->
  do bs <- recvAll socket
     let path:name:segmentTime:rest = read $ BSC.unpack bs
         codes = convertListToTriple rest
     print $ [path,name]
     if segmentTime == "no"
       then do atomically $ writeTQueue todo (path, name, 0, codes, False)
               atomically $ writeTQueue film (name, 0, map fst codes)
       else do let args = ["-i",path,"-f","segment","-segment_time",segmentTime,"-c","copy","-y",name ++ "-%d.mkv"]
               callProcess "ffmpeg" args
               num <- numberOfSegments name
               atomically $ mapM_ (\i -> writeTQueue todo (name ++ "-" ++ show i ++ ".mkv", name, i, codes, True)) [0..num-1]
               atomically $ writeTQueue film (name, num-1, map fst codes)
     where recvAll s = do mbs <- recv s 1024
                          case mbs of
                            Nothing -> return BS.empty
                            Just bs -> recvAll s >>= (return . BS.append bs)
           numberOfSegments :: String -> IO Int
           numberOfSegments name = do files <- getCurrentDirectory >>= getDirectoryContents
                                      let mkvs = map (\s -> take (length s - 4) s) $ filter (isSuffixOf ".mkv") $ files
                                          len = length $ filter (all isDigit) $ map (drop (length name + 1)) $ filter (isPrefixOf (name ++ "-")) $ mkvs
                                      return len
           convertListToTriple :: [a] -> [(a, a)]
           convertListToTriple [] = []
           convertListToTriple (f:s:list) = (f, s):convertListToTriple list

queueServer :: String -> TQueue (FilePath, String, Int, [(String, String)], Bool) -> TQueue ([(String, FilePath)], String, Int) -> IO ()
queueServer port todo done = serve HostAny port $ \(socket, remoteAddr) ->
  do putStrLn $ "Worker connected from " ++ show remoteAddr
     worker socket remoteAddr
  where worker socket remoteAddr =
          do (path, name, min, codes, shouldDelete) <- atomically $ readTQueue todo
             let fileNames = map (`addExtension` "mkv") $ map ((name ++ "-") ++) $ map fst codes
                 args = ["-i","pipe:0","-c","copy","-y"]
                 procs = map (\p -> p { std_in = CreatePipe, std_err = CreatePipe }) $ map (proc "ffmpeg") $ map (\f -> args ++ [f]) fileNames
                 unget = do putStrLn $ "Converting " ++ name ++ "-" ++ show min ++ " failed"
                            atomically $ unGetTQueue todo (path, name, min, codes, shouldDelete)
                            clean fileNames
             print fileNames
             handles <- mapM createProcess procs
             let (stdins, _, _, phs) = unzip4 handles
                 ins = map fromJust stdins
                 work = bracket (openFile path ReadMode)
                                (hClose)
                                (transfer socket codes (fromList $ zip [1..] ins))
             succ <- onException work unget
             if succ
               then do exitCodes <- mapM waitForProcess phs
                       if all (== ExitSuccess) exitCodes
                         then do when shouldDelete $ removeFile path
                                 atomically $ writeTQueue done (zip (map fst codes) fileNames, name, min)
                                 worker socket remoteAddr
                         else unget
               else do mapM terminateProcess phs
                       unget
        clean :: [FilePath] -> IO ()
        clean paths = mapM_ (try . removeFile :: FilePath -> IO (Either IOException ())) paths
        transfer :: Socket -> [(String, String)] -> Map Word8 Handle -> Handle -> IO Bool
        transfer s codes i2h h =
          do mapM_ (send s) $ zipWith (command 1) [1..] $ map snd codes
             a <- async $ while $ do bs <- BS.hGetSome h 65535
                                     if BS.null bs then
                                       send s (packet 1 BS.empty) >> return False
                                     else
                                       send s (packet 1 bs) >> return True
             let loop i2h
                   | M.null i2h = return True
                   | otherwise  = do mbs <- readPacket s
                                     case mbs of
                                       Nothing -> return False
                                       Just (i, bs) -> if BS.null bs
                                                         then hClose (i2h ! i) >> loop (M.delete i i2h)
                                                         else BS.hPut (i2h ! i) bs >> loop i2h
             b <- async $ loop i2h
             (_, r) <- waitBoth a b `onException` (cancel a >> cancel b)
             return r

mergeServer :: TQueue ([(String, FilePath)], String, Int) -> TQueue (String, Int, [String]) -> IO ()
mergeServer done film = do (name, num, dirs) <- atomically $ readTQueue film
                           files <- waitForNum name [0..num]
                           let sorted = concat $ map snd $ sort files
                               flt l d = map snd $ filter ((== d) . fst) l
                               outputs = zip dirs $ map (flt sorted) dirs
                           handle (\(e :: SomeException) -> putStrLn $ "merge failed for " ++ name ++ ": " ++ show e)
                                  (do mapM_ (uncurry.merge $ name) outputs
                                      mapM_ (removeFile.snd) sorted)
                           mergeServer done film
                        where waitForNum _ [] = return []
                              waitForNum name nums = do (f, n, i) <- atomically $ readTQueue done
                                                        if n == name
                                                          then do putStrLn $ "Converting " ++ name ++ "-" ++ show i ++ " done. " ++ show (length nums - 1) ++ " to go."
                                                                  rest <- waitForNum name (L.delete i nums)
                                                                  return $ (i, f):rest
                                                          else do atomically $ writeTQueue done (f, n, i)
                                                                  waitForNum name nums
                              merge :: String -> FilePath -> [FilePath] -> IO ()
                              merge name dir files = do let list = unlines $ "ffconcat version 1.0":map ("file "++) files
                                                        writeFile "concat.txt" list
                                                        callProcess "ffmpeg" ["-f", "concat", "-i", "concat.txt", "-c", "copy", "-y", dir </> name <.> "mkv"]

while :: IO Bool -> IO ()
while f = do r <- f
             when r $ while f
