module Logger
  ( debugMessage
  , warningMessage
  , errorMessage
  ) where

message :: String -> String -> String
message a b = "[" ++ a ++"] " ++ b ++ "\n"

debugMessage :: String -> String
debugMessage = message "info"

warningMessage :: String -> String
warningMessage = message "warning"

errorMessage :: String -> String
errorMessage = message "error"
