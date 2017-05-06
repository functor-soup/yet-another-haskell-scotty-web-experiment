{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Lens
import Control.Monad.Trans.Either
import Control.Monad.Writer
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B
import Data.Text.Encoding
import GHC.Generics
import Lib
import qualified Network.Wreq as W
import Web.Scotty

type Logger a = EitherT String (WriterT String ActionM) a

data PostStuff = PostStuff
  { curr :: String
  , val :: Double
  } deriving (Generic, Show)

instance FromJSON PostStuff

genLogger :: (Either String PostStuff) -> Logger PostStuff
genLogger x =
  (lift $
   tell $
   either
     (const "Json Validation failed ")
     (const "Json validation passed, commencing further steps")
     x) >>
  hoistEither x

genLogger2 :: PostStuff -> Logger B.ByteString
genLogger2 obj = do
  tell ("Json of form " ++ show obj)
  tell ("Sending Json to remote server")
  egg <-
    liftIO $
    W.get ("https://blockchain.info/tobtc?currency=" ++ (curr obj) ++ "&value=" ++ (show $ val obj))
  let response = egg ^. W.responseBody
  --tell "Server gave back " ++ (show response)
  return response

postAction :: ScottyM ()
postAction = do
  post "/" $
    body >>= (\i -> return $ eitherDecode i) >>=
    (\x -> runWriterT . runEitherT $ (genLogger x >>= genLogger2)) >>=
    (\(a, b) -> (liftIO $ putStrLn b) >> Web.Scotty.json (show a))

main :: IO ()
main = scotty 8080 postAction
