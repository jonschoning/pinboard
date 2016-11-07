{-# LANGUAGE OverloadedStrings #-}

import Pinboard
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

main :: IO ()
main = do
  let config =
        -- withStdoutLogging
          (fromApiToken "api token")
          -- { filterLoggingT = debugLevelFilter }
  result <- runPinboard config $ getPostsRecent Nothing Nothing
  case result of
    Right details -> print ("Left: " ++ show details)
    Left pinboardError -> print ("Left: " ++ show pinboardError)
