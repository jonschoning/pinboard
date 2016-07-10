# Pinboard [![Hackage](https://img.shields.io/hackage/v/pinboard.svg?style=flat)](https://hackage.haskell.org/package/pinboard) [![Build Status](https://travis-ci.org/jonschoning/pinboard.svg?branch=master)](https://travis-ci.org/jonschoning/pinboard)

The Pinboard API is a way to interact programatically with
your bookmarks, notes and other Pinboard data. This
library wraps the API exposing functions and data
structures suitable for usage in Haskell programs.

## Hackage documentation

<http://hackage.haskell.org/package/pinboard>

## Stackage documentation

<https://www.stackage.org/package/pinboard>

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
  result <- runPinboard config $ getPostsRecent Nothing Nothing
  case result of
    Right details -> print details
    Left pinboardError -> print pinboardError
```

output:
```
Posts{postsDate = 2015 - 10 - 24 16 : 17 : 12 UTC,
       postsUser = "jonschoning",
       postsPosts =
         [Post{postHref = "http://www.reddit.com/r/haskell/comments/25vj62/adventure_with_types_in_haskell_simon_peyton/",
               postDescription = "Adventure with Types in Haskell - Simon Peyton Jones [1:33:36] : haskell",
               postExtended = "", 
               postMeta = "3fe9fb05c7c37f7bb66be7b9d85599eb",
               postHash = "c46b717604ef8b126dabeba97b27a36f",
               postTime = 2014 - 5 - 19 3 : 35 : 55 UTC, 
               postShared = True,
               postToRead = False,
               postTags = ["spj", "video", "haskell", "typetheory"]}]}
```

### getPostsRecent (using Lenses)
``` {.haskell}
{-# LANGUAGE OverloadedStrings #-}

import Pinboard
import Control.Lens

main :: IO ()
main = do
  let config = fromApiToken "api token"
  result <- runPinboard config $ getPostsRecent (Just ["haskell"]) (Just 3)
  mapM_ print (result ^.. _Right . postsPostsL . traverse . postHrefL)

```

output:
```
"http://www.stephendiehl.com/posts/production.html"
"https://mail.haskell.org/mailman/listinfo"
"https://en.wikibooks.org/wiki/Haskell/Applicative_functors#A_sliding_scale_of_power"
```


## Modules

[Pinboard.Types](https://hackage.haskell.org/package/pinboard/docs/Pinboard-Types.html)

  Pinboard typeclasses and type aliases

[Pinboard.Client](https://hackage.haskell.org/package/pinboard/docs/Pinboard-Client.html)

  Executes the methods defined in Pinboard.Api

[Pinboard.Api](https://hackage.haskell.org/package/pinboard/docs/Pinboard-Api.html)

  Provides Pinboard Api Methods

[Pinboard.ApiTypes](https://hackage.haskell.org/package/pinboard/docs/Pinboard-ApiTypes.html)

  Pinboard Data Structures returned by the Api

[Pinboard.ApiTypesLens](https://hackage.haskell.org/package/pinboard/docs/Pinboard-ApiTypesLens.html)

  Lens accessors for Pinboard.ApiTypes
