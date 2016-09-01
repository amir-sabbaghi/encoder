module Packet ( command
              , packet
              , readPacket
              , parseControlPacket
              ) where

import Data.ByteString.Builder as BS
import Data.ByteString.Lazy (toStrict)
import Data.ByteString as BS
import Data.ByteString.Char8 as BSC (unpack)
import Data.Word
import Data.Monoid ((<>))
import Network.Simple.TCP
import Control.Monad (when)

command :: Word8 -> Word8 -> String -> ByteString
command input output flags =
    toStrict . toLazyByteString $ BS.word8 0
                               <> BS.word16BE (fromIntegral $ Prelude.length flags + 2)
                               <> BS.word8 input
                               <> BS.word8 output
                               <> BS.stringUtf8 flags

packet :: Word8 -> ByteString -> ByteString
packet ch bs = toStrict . toLazyByteString
             $ BS.word8 ch
            <> BS.word16BE (fromIntegral $ BS.length bs)
            <> BS.byteString bs

readPacket :: Socket -> IO (Maybe (Word8, ByteString))
readPacket s = do bs <- read s 3
                  case bs of
                    Nothing -> return Nothing
                    Just header -> do dat <- read s len
                                      case dat of
                                        Nothing -> return Nothing
                                        Just dt -> return $ Just (index, dt)
                      where [index, l1, l2] = BS.unpack header
                            len = fromIntegral l1 * 256 + fromIntegral l2
  where read :: Socket -> Int -> IO (Maybe ByteString)
        read s n = do mbs <- recv s n
                      case mbs of
                        Nothing -> return Nothing
                        Just bs -> if n == BS.length bs then
                                     return $ Just bs
                                   else do rem <- read s (n - BS.length bs)
                                           return $ Just bs <> rem

parseControlPacket :: ByteString -> (Word8, Word8, String)
parseControlPacket bs = (input, output, flags)
  where (h, rem) = BS.splitAt 2 bs
        [input, output] = BS.unpack h
        flags = BSC.unpack rem
