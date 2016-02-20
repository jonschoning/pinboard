{-# LANGUAGE OverloadedStrings #-}

import Pinboard
import Control.Lens

main :: IO ()
main = do
  let config = fromApiToken "api token"
  result <- runPinboard config $ getPostsRecent (Just ["haskell"]) (Just 3)
  mapM_ print (result ^.. _Right . postsPostsL . traverse . postHrefL)
