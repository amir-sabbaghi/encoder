module Server ( server
              ) where

import Network.Simple.TCP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC (unpack)
import Data.List (delete)
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

codes = [("1080p","-map 0:0 -c:v libx265 -x265-params crf=25 -map_metadata -1 -metadata:s:0 language=en")
        ,("720p", "-map 0:0 -c:v libx265 -x265-params crf=25 -map_metadata -1 -metadata:s:0 language=en -s 1280x720")
        ,("audio","-map 0:1 -c:a aac -ac 2 -b:a 128k -strict -2 -map_metadata -1 -metadata:s:0 language=en")
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
     atomically $ writeTQueue film (path, mins+1)
     where recvAll s = do mbs <- recv s 1024
                          case mbs of
                            Nothing -> return BS.empty
                            Just bs -> recvAll s >>= (return . BS.append bs)
           totalMinutes :: FilePath -> IO Int
           totalMinutes path = do let args = ["-show_entries","format=duration","-of","default=noprint_wrappers=1:nokey=1",path]
                                  (exitCode, out, err) <- readProcessWithExitCode "/usr/bin/ffprobe" args ""
                                  assert (exitCode == ExitSuccess) $ return .(`div` 60). ceiling . read $ out

queueServer :: String -> TQueue (FilePath, String, Int) -> TQueue (FilePath, String, Int) -> IO ()
queueServer port todo done = serve HostAny port $ \(socket, remoteAddr) ->
  do putStrLn $ "Worker connected from " ++ show remoteAddr
     worker socket remoteAddr
  where worker socket remoteAddr =
          do (path, name, min) <- atomically $ readTQueue todo
             mapM_ (send socket) $ zipWith (command 1) [1..] $ map snd codes
             let fileNames = map (`addExtension` "mkv") $ map ((dropExtension path ++ "-") ++) $ map fst codes
                 unget = do clean fileNames
                            atomically $ unGetTQueue todo (path, name, min)
             print fileNames
             clean fileNames
             let work = bracket (openFile path ReadMode)
                                (hClose)
                                (transfer socket (fromList $ zip [1..] fileNames))
             succ <- onException work unget
             if succ
               then atomically $ writeTQueue done (path, name, min)
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

mergeServer :: TQueue (FilePath, String, Int) -> TQueue (String, Int) -> IO ()
mergeServer done number = return ()

while :: IO Bool -> IO ()
while f = do r <- f
             when r $ while f
