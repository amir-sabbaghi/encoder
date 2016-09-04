module Server ( server
              ) where

import Network.Simple.TCP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC (unpack)
import Control.Concurrent
import Control.Concurrent.STM.TQueue
import Control.Monad
import Control.Monad.STM (atomically)
import Control.Exception
import System.Process
import System.Directory
import System.Exit
import GHC.IO.Handle

import Packet

codes = [ "-map 0:0 -c:v libx265 -x265-params crf=25 -map_metadata -1 -metadata:s:0 language=en"
        , "-map 0:0 -c:v libx265 -x265-params crf=25 -map_metadata -1 -metadata:s:0 language=en -s 1280x720"
        , "-map 0:1 -c:a aac -ac 2 -b:a 128k -strict -2 -map_metadata -1 -metadata:s:0 language=en"
        ]

server :: String -> String -> IO ()
server port mport = do todo <- atomically $ newTQueue
                       done <- atomically $ newTQueue
                       film <- atomically $ newTQueue
                       forkIO $ managementServer mport todo film
                       forkIO $ mergeServer done film
                       queueServer port todo done

managementServer :: String -> TQueue (FilePath, Int, String) -> TQueue (String, Int) -> IO ()
managementServer port todo film = serve HostAny port $ \(socket, remoteAddr) ->
  do bs <- recvAll socket
     let [path,name] = read $ BSC.unpack bs
     print $ [path,name]
     mins <- totalMinutes path
     atomically $ mapM_ (\i -> writeTQueue todo (path, i, name)) [0..mins]
     atomically $ writeTQueue film (path, mins+1)
     where recvAll s = do mbs <- recv s 1024
                          case mbs of
                            Nothing -> return BS.empty
                            Just bs -> recvAll s >>= (return . BS.append bs)
           totalMinutes :: FilePath -> IO Int
           totalMinutes path = do let args = ["-show_entries","format=duration","-of","default=noprint_wrappers=1:nokey=1",path]
                                  (exitCode, out, err) <- readProcessWithExitCode "/usr/bin/ffprobe" args ""
                                  assert (exitCode == ExitSuccess) $ return . ceiling . read $ out

queueServer :: String -> TQueue (FilePath, Int, String) -> TQueue (String, Int) -> IO ()
queueServer port todo done = serve HostAny port $ \(socket, remoteAddr) ->
  do putStrLn $ "Worker connected from " ++ show remoteAddr
     worker socket remoteAddr
  where worker socket remoteAddr =
          do (path, min, name) <- atomically $ readTQueue todo
             mapM_ (send socket) $ zipWith (command 1) [1..] codes
             mapM_ (try . removeFile :: FilePath -> IO (Either IOException ())) $ map ((name ++).("_" ++).show) [1..length codes]
             bracketOnError (ffmpegProc path min)
                            (clean todo (path, min, name) . snd)
                            (transfer name socket . fst)
             atomically $ writeTQueue done (name, min)
             worker socket remoteAddr
        ffmpegProc :: FilePath -> Int -> IO (Handle, ProcessHandle)
        ffmpegProc path min =
          do let args = ["-ss",show min++":00","-t","1:00","-i",path,"-c","copy","-f","matroska","pipe:1"]
                 ffmpeg = (proc "/usr/bin/ffmpeg" args) { std_out = CreatePipe }
             (Nothing, Just outh, Nothing, ph) <- createProcess ffmpeg
             return (outh, ph)
        clean todo t h = do atomically $ unGetTQueue todo t
                            terminateProcess h
        transfer :: String -> Socket -> Handle -> IO ()
        transfer name s h =
          do tid <- myThreadId
             forkIO $ handle (throwTo tid :: SomeException -> IO ()) $
                             while $ do bs <- BS.hGetSome h 65535
                                        if BS.null bs then
                                          send s (packet 1 BS.empty) >> return False
                                        else
                                          send s (packet 1 bs) >> return True
             while $ do mbs <- readPacket s
                        case mbs of
                          Nothing -> return False
                          Just (i, bs) -> BS.appendFile (name ++ "-" ++ show i) bs >> return True

mergeServer :: TQueue (String, Int) -> TQueue (String, Int) -> IO ()
mergeServer done number = return ()

while :: IO Bool -> IO ()
while f = do r <- f
             when r $ while f
