{-# LANGUAGE OverloadedStrings #-}

import Pinboard

main :: IO ()
main = do
  let config = fromApiToken "api token"
  result <- runPinboardJson config $ getPostsRecent Nothing Nothing
  case result of
    Right details -> print details
    Left pinboardError -> print pinboardError
