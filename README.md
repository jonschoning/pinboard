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
## Modules

[Pinboard.Client](https://hackage.haskell.org/package/pinboard/docs/Pinboard-Client.html)

  Executes the methods defined in Pinboard.Api

[Pinboard.Api](https://hackage.haskell.org/package/pinboard/docs/Pinboard-Api.html)

  Provides Pinboard Api Methods

[Pinboard.ApiTypes](https://hackage.haskell.org/package/pinboard/docs/Pinboard-ApiTypes.html)

  Pinboard Data Structures returned by the Api

## Windows

This package relies on HsOpenSSL which requires a binary distribution of openssl

In order for this install to go smoothly one must install a binary distribution of openssl from here: 

    http://slproweb.com/products/Win32OpenSSL.html 
    
and link during the cabal install process like this (assuing default install directories):

    cabal install HsOpenSSL --extra-include-dirs="c:/OpenSSL-Win32/include" --extra-lib-dirs="c:/OpenSSL-Win32"

