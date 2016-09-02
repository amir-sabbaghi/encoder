module Client ( client
              ) where

import Network.Simple.TCP
import Data.Word
import System.Process
import GHC.IO.Handle
import qualified Data.ByteString as BS
import System.Exit
import Control.Concurrent (forkIO)

import Packet

client :: String -> String -> IO ()
client host port = connect host port $ \(socket, addr) -> do
    putStrLn $ "Connection established to " ++ show addr
    dispatch socket []
    where dispatch :: Socket -> [(Word8, [(ProcessHandle, Handle)])] -> IO ()
          dispatch s m = do pckt <- readPacket s
                            case pckt of
                              Nothing -> mapM_ terminateProcess $ map fst $ concat $ map snd m
                              Just (0, bs) -> do let (input, output, flags) = parseControlPacket bs
                                                     args = "-i":"-":words flags ++ ["-f","matroska","pipe:1"]
                                                     ffmpeg = (proc "/usr/bin/ffmpeg" args) { std_in = CreatePipe
                                                                                   , std_out = CreatePipe
                                                                                   , std_err = CreatePipe
                                                                                   }
                                                 print args
                                                 (Just inh, Just outh, Just errh, ph) <- createProcess ffmpeg
                                                 putStrLn $ "ffmpeg process created"
                                                 forkIO $ let loop = do bs <- BS.hGetSome errh 65535
                                                                        if BS.null bs then
                                                                          return ()
                                                                        else do
                                                                          BS.appendFile ("err" ++ show output) bs
                                                                          loop
                                                          in loop
                                                 forkIO $ let loop = do bs <- BS.hGetSome outh 65535
                                                                        putStrLn $ "sending " ++ show (BS.length bs) ++ " bytes"
                                                                        if BS.null bs then do
                                                                          exitCode <- waitForProcess ph
                                                                          case exitCode of
                                                                            ExitSuccess -> send s $ packet output BS.empty
                                                                            ExitFailure code -> do putStrLn $ "ffmpeg exited with exit code: " ++ show code
                                                                                                   error "ffmpeg exited abnormally"
                                                                        else do
                                                                          send s $ packet output bs
                                                                          loop
                                                          in loop
                                                 let newlist = case lookup input m of
                                                                 Nothing -> [(ph, inh)]
                                                                 Just list -> (ph, inh):list
                                                     newm = (input, newlist):filter (\(i, _) -> i /= input) m
                                                 dispatch s newm
                              Just (i, bs) -> case lookup i m of
                                                Nothing -> putStrLn $ "Cannot find index " ++ show i
                                                Just list -> if BS.null bs then do
                                                               putStrLn $ "Closing pipes"
                                                               mapM_ hClose $ map snd list
                                                               dispatch s $ filter (\(j, _) -> j /= i) m
                                                             else do let put = flip BS.hPut bs
                                                                     putStrLn $ "Converting " ++ show (BS.length bs) ++ " bytes"
                                                                     mapM_ put $ map snd list
                                                                     dispatch s m

