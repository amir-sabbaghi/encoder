module Server ( server
              ) where

import Network.Simple.TCP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC (unpack)
import Data.List as L
import Data.Word
import Data.Maybe (fromJust)
import Data.Map as M (Map, delete, (!), null, fromList)
import Control.Concurrent
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

codes = [("1080p","-map 0:0 -c:v libx265 -x265-params crf=25 -map_metadata -1 -metadata:s:0 language=en -f matroska")
        ,("720p", "-map 0:0 -c:v libx265 -x265-params crf=25 -map_metadata -1 -metadata:s:0 language=en -s 1280x720 -f matroska")
        ,("audio","-map 0:1 -c:a aac -ac 2 -b:a 128k -strict -2 -map_metadata -1 -metadata:s:0 language=en -f matroska")
        ]

server :: String -> String -> IO ()
server port mport = do todo <- atomically $ newTQueue
                       done <- atomically $ newTQueue
                       film <- atomically $ newTQueue
                       forkIO $ managementServer mport todo film
                       forkIO $ mergeServer done film
                       queueServer port todo done

managementServer :: String -> TQueue (FilePath, String, Int) -> TQueue (String, Int) -> IO ()
managementServer port todo film = serve HostAny port $ \(socket, remoteAddr) ->
  do bs <- recvAll socket
     let [path,name] = read $ BSC.unpack bs
         args = ["-i",path,"-f","segment","-segment_time","60","-c","copy",name ++ "-%d.mkv"]
     print $ [path,name]
     callProcess "/usr/bin/ffmpeg" args
     mins <- totalMinutes path
     atomically $ mapM_ (\i -> writeTQueue todo (name ++ "-" ++ show i ++ ".mkv", name, i)) [0..mins]
     atomically $ writeTQueue film (name, mins)
     where recvAll s = do mbs <- recv s 1024
                          case mbs of
                            Nothing -> return BS.empty
                            Just bs -> recvAll s >>= (return . BS.append bs)
           totalMinutes :: FilePath -> IO Int
           totalMinutes path = do let args = ["-show_entries","format=duration","-of","default=noprint_wrappers=1:nokey=1",path]
                                  (exitCode, out, err) <- readProcessWithExitCode "/usr/bin/ffprobe" args ""
                                  assert (exitCode == ExitSuccess) $ return .(`div` 60). ceiling . read $ out

queueServer :: String -> TQueue (FilePath, String, Int) -> TQueue ([(String, FilePath)], String, Int) -> IO ()
queueServer port todo done = serve HostAny port $ \(socket, remoteAddr) ->
  do putStrLn $ "Worker connected from " ++ show remoteAddr
     worker socket remoteAddr
  where worker socket remoteAddr =
          do (path, name, min) <- atomically $ readTQueue todo
             mapM_ (send socket) $ zipWith (command 1) [1..] $ map snd codes
             let fileNames = map (`addExtension` "mkv") $ map ((dropExtension path ++ "-") ++) $ map fst codes
                 unget = do clean fileNames
                            atomically $ unGetTQueue todo (path, name, min)
                 work = bracket (openFile path ReadMode)
                                (hClose)
                                (transfer socket (fromList $ zip [1..] fileNames))
             print fileNames
             clean fileNames
             succ <- onException work unget
             if succ
               then atomically $ writeTQueue done (zip (map fst codes) fileNames, name, min)
               else unget
             worker socket remoteAddr
        clean :: [FilePath] -> IO ()
        clean paths = mapM_ (try . removeFile :: FilePath -> IO (Either IOException ())) paths
        transfer :: Socket -> Map Word8 FilePath -> Handle -> IO Bool
        transfer s i2f h =
          do tid <- myThreadId
             forkIO $ handle (throwTo tid :: SomeException -> IO ()) $
                             while $ do bs <- BS.hGetSome h 65535
                                        if BS.null bs then
                                          send s (packet 1 BS.empty) >> return False
                                        else
                                          send s (packet 1 bs) >> return True
             let loop i2f
                   | M.null i2f = return True
                   | otherwise  = do mbs <- readPacket s
                                     case mbs of
                                       Nothing -> return False
                                       Just (i, bs) -> if BS.null bs
                                                         then loop (M.delete i i2f)
                                                         else BS.appendFile (i2f ! i) bs >> loop i2f
             loop i2f

mergeServer :: TQueue ([(String, FilePath)], String, Int) -> TQueue (String, Int) -> IO ()
mergeServer done film = do (name, num) <- atomically $ readTQueue film
                           files <- waitForNum name [0..num]
                           let sorted = concat $ map snd $ sort files
                               dirs = map fst codes
                               flt l d = map snd $ filter ((== d) . fst) l
                               outputs = zip dirs $ map (flt sorted) dirs
                           handle (\(e :: SomeException) -> putStrLn $ "merge failed for " ++ name ++ ": " ++ show e)
                                  (mapM_ (uncurry.merge $ name) outputs)
                           return ()
                        where waitForNum _ [] = return []
                              waitForNum name nums = do (f, n, i) <- atomically $ readTQueue done
                                                        if n == name
                                                          then do rest <- waitForNum name (L.delete i nums)
                                                                  return $ (i, f):rest
                                                          else do atomically $ writeTQueue done (f, n, i)
                                                                  waitForNum name nums
                              merge :: String -> FilePath -> [FilePath] -> IO ()
                              merge name dir files = do let list = unlines $ "ffconcat version 1.0":map ("file "++) files
                                                        writeFile "concat.txt" list
                                                        callProcess "/usr/bin/ffmpeg" ["-f", "concat", "-i", "concat.txt", "-c", "copy", dir </> name <.> "mp4"]

while :: IO Bool -> IO ()
while f = do r <- f
             when r $ while f
