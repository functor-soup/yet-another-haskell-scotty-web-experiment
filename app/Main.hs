{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Lens
import Control.Monad.Trans.Either
import Control.Monad.Writer
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Text.Encoding
import GHC.Generics
import Lib
import Logger
import qualified Network.Wreq as W
import Web.Scotty

type Logger a = EitherT String (WriterT String ActionM) a

data PostBody = PostBody
  { curr :: String
  , val :: Double
  } deriving (Generic)

instance Show PostBody where
  show a = "Currency type of " ++ (curr a) ++ " and value of " ++ (show $ val a)

instance FromJSON PostBody

validationStage :: (Either String PostBody) -> Logger PostBody
validationStage x =
  (lift . tell $
   either
     (const . errorMessage $ "Json Validation failed ")
     (const . debugMessage $ "Json validation passed, commencing further steps") $
   x) >>
  hoistEither x

remoteStage :: PostBody -> Logger B.ByteString
remoteStage obj = do
  tell . debugMessage $ ("Json of form " ++ show obj)
  tell . debugMessage $ ("Sending Json to remote server")
  egg <-
    liftIO $
    W.get ("https://blockchain.info/tobtc?currency=" ++ (curr obj) ++ "&value=" ++ (show $ val obj))
  let response = egg ^. W.responseBody
  tell . debugMessage $ "Server gave back " ++ (show response)
  return response

postAction :: ScottyM ()
postAction =
  post "/" $
  body >>= (return . eitherDecode) >>= (runWriterT . runEitherT . (validationStage >=> remoteStage)) >>=
  (\(a, b) -> (liftIO $ putStrLn b) >> Web.Scotty.json (show a))

main :: IO ()
main = scotty 8080 postAction
