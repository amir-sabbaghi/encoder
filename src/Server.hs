module Server ( server
              ) where

import Network.Simple.TCP
import qualified Data.ByteString as BS
import Control.Concurrent (forkIO)

import Packet

server :: String -> IO ()
server port = do let codes = ["-map 0:0 -c:v libx265 -x265-params crf=25 -map_metadata -1 -metadata:s:0 language=en"
                              ,"-map 0:0 -c:v libx265 -x265-params crf=25 -map_metadata -1 -metadata:s:0 language=en -s 1280x720"
                              ,"-map 0:1 -c:a aac -ac 2 -b:a 128k -strict -2 -map_metadata -1 -metadata:s:0 language=en"
                              ]
                 serve HostAny port $ \(socket, remoteAddr) -> do
                     mapM_ (send socket) $ zipWith (command 1) [1..] codes
                     forkIO $ let loop bs = if BS.length bs < 65535 then do
                                              send socket $ packet 1 bs
                                              send socket $ packet 1 BS.empty
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

