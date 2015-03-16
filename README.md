# Pinboard [![Hackage](https://img.shields.io/hackage/v/pinboard.svg?style=flat)](https://hackage.haskell.org/package/pinboard)

The Pinboard API is a way to interact programatically with
your bookmarks, notes and other Pinboard data. This
library wraps the API exposing functions and data
structures suitable for usage in Haskell programs.

## Hackage page and Haddock documentation
<http://hackage.haskell.org/package/pinboard>

## Pinboard Api documentation

<https://pinboard.in/api/>

## Examples: 

### getPostsRecent
``` {.haskell}
{-# LANGUAGE OverloadedStrings #-}

import Pinboard

main :: IO ()
main = do
  let config = fromApiToken "api token"
  result <- runPinboardJson config $ getPostsRecent Nothing Nothing
  case result of
    Right details -> print details
    Left pinboardError -> print pinboardError
```
