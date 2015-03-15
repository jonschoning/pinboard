The Pinboard API is a way to interact programatically with
your bookmarks, notes and other Pinboard data. This
library wraps the API exposing functions and data
structures suitable for usage in Haskell programs.

Example:

``` haskell
import Pinboard

main :: IO ()
main = do
  let config = fromApiToken "api token"
  result <- runPinboardJson config $ getPostsRecent Nothing Nothing
  case result of
    Right details -> print details
    Left pinboardError -> print pinboardError
```
