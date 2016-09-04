module Main where

import Options.Applicative hiding (command)
import System.Exit

import Client
import Server

data Flags = Flags
    { flagPort :: String
    , flagServer :: Bool
    , flagManagementPort :: Maybe String
    , flagClient :: Maybe String
--    , flagCert :: String
--    , flagKey  :: String
    }

parseFlags :: Parser Flags
parseFlags = Flags
        <$> strOption (short 'p' <> long "port" <> metavar "PORT" <> help "The port to listen for incoming connections/or connect to server in case of client")
        <*> switch (short 's' <> long "server" <> help "Act as server")
        <*> optional (strOption (short 'm' <> long "management" <> metavar "PORT" <> help "The port to send encoding requests to"))
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
decide (Flags port True  (Just mport) Nothing) = server port mport
decide (Flags port False Nothing      (Just h)) = client h port
decide (Flags _    True  Nothing      Nothing) = die "Management port should be specified with -m"
decide (Flags _    False _            Nothing) = die "One of -s or -c should be specified"
decide (Flags _    True  _            _) = die "Only one of -s or -c should be specified"
