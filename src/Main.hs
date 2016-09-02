module Main where

import Options.Applicative hiding (command)

import Client
import Server

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

