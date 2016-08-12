{-# LANGUAGE OverloadedStrings #-}

import Pinboard
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

main :: IO ()
main = do
  let config = fromApiToken "api token"
  result <- runPinboard config $ getPostsRecent Nothing Nothing
  case result of
    Right details -> print details
    Left pinboardError -> print ("L: " ++ show pinboardError)
