module Main where

import System.Environment
import Network.Simple.TCP
import Control.Monad (zipWithM)
import Data.Word
import GHC.IO.Handle
import Options.Applicative hiding (command)
import System.Process
import qualified Data.ByteString as BS
import Control.Concurrent (forkIO)

import Packet

data Flags = Flags
    { flagPort :: String
    , flagServer :: Bool
    , flagClient :: Maybe String
--    , flagCert :: String
--    , flagKey  :: String
    }

parseFlags :: Parser Flags
parseFlags = Flags
        <$> strOption (short 'p' <> long "port" <> metavar "PORT" <> help "The port to listen for incoming connections/or connect to server in case of client")
        <*> switch (short 's' <> long "server" <> help "Act as server")
        <*> optional (strOption (short 'c' <> long "client" <> metavar "SERVER" <> help "Act as client"))
--        <*> strOption (long "cert" <> metavar "CERTFILE" <> help "Server's certificate file path")
--        <*> strOption (long "key" <> metavar "KEYFILE" <> help "Server's private key file path")

main :: IO ()
main = execParser opts >>= decide
    where opts = info (helper <*> parseFlags)
               ( fullDesc
              <> header "Encoder distributor"
               )

decide :: Flags -> IO ()
decide (Flags port True Nothing) = server port
decide (Flags port False (Just h)) = client h port
decide (Flags _    False Nothing) = error "One of -s or -c should be specified"
decide (Flags _    True _) = error "Only one of -s or -c should be specified"

server :: String -> IO ()
server port = do let codes = ["-map 0:0 -c:v libx265 -x265-params crf=25 -map_metadata -1 -metadata:s:0 language=en"
                              ,"-map 0:0 -c:v libx265 -x265-params crf=25 -map_metadata -1 -metadata:s:0 language=en -s 1280x720"
                              ,"-map 0:1 -c:a aac -ac 2 -b:a 128k -strict -2 -map_metadata -1 -metadata:s:0 language=en"
                              ]
                 serve HostAny port $ \(socket, remoteAddr) -> do
                     mapM_ (send socket) $ zipWith (command 1) [1..] codes
                     forkIO $ let loop bs = if BS.length bs < 65535 then
                                              send socket $ packet 1 bs
                                            else do
                                              let (b, bss) = BS.splitAt 65535 bs
                                              send socket $ packet 1 b
                                              loop bss
                              in do
                                film <- BS.getContents
                                loop film
                     let loop = do pckt <- readPacket socket
                                   case pckt of
                                     Nothing -> return ()
                                     Just (i, bs) -> do BS.appendFile (show i) bs
                                                        loop
                     loop

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
                                                                          send s $ packet output BS.empty
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
                                                Just list -> do let put = flip BS.hPut bs
                                                                putStrLn $ "Converting " ++ show (BS.length bs) ++ " bytes"
                                                                mapM_ put $ map snd list
                                                                dispatch s m
