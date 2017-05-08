module Jam.Server where

import Prelude
import Data.String as S
import Control.IxMonad ((:*>), (:>>=))
import Control.Monad.Aff (nonCanceler)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Argonaut (encodeJson)
import Data.Argonaut.Core (stringify)
import Data.List (List(..), (:))
import Data.Maybe (maybe)
import Data.MediaType.Common (textHTML)
import Data.Tuple (Tuple(..))
import Hyper.Node.FileServer (fileServer)
import Hyper.Node.Server (defaultOptionsWithLogging, runServer)
import Hyper.Request (getRequestData)
import Hyper.Response (closeHeaders, contentType, headers, respond, writeStatus)
import Hyper.Status (statusNotFound, statusOK)
import Jam.App (Locations, router)
import Jam.Types (Musician(..))
import Node.Buffer (BUFFER)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.HTTP (HTTP)
import React (ReactClass, createClassStateless, createElement)
import React.DOM (div')
import React.Redox (withStore)
import React.Router (RouteProps, runRouter)
import React.Router.Types (Router)
import ReactDOM (renderToString)
import Redox (Store, mkStoreG)

initialState :: Array Musician
initialState =
  [ Musician
      { id: 1
      , name: "Pat Metheny"
      , description: """Patrick Bruce "Pat" Metheny is an American jazz guitarist and composer."""
      , wiki: "https://en.wikipedia.org/wiki/Pat_Metheny"
      , generes: ("Jazz" : "jazz fusion" : "Latin jazz" : "world" : "experimental" : "avant-gard" : Nil)
      }
  , Musician
      { id: 2
      , name: "John Scofield"
      , description: """John Scofield, often referred to as "Sco", is an American jazz-rock guitarist and composer, who has played and collaborated with Miles Davis, Dave Liebman, Joe Henderson, Charles Mingus, Joey DeFrancesco, Herbie Hancock, Pat Metheny, Bill Frisell, Pat Martino, Mavis Staples, Phil Lesh, Billy Cobham, Medeski Martin & Wood, George Duke, Jaco Pastorius, John Mayer, Robert Glasper, and Gov't Mule."""
      , wiki: "https://en.wikipedia.org/wiki/John_Scofield"
      , generes: ("Jazz" : "jazz fusion" : "acid jazz" : Nil)
    }
  ]


entryCls :: Router RouteProps Locations -> Store (Array Musician) -> ReactClass { url :: String }
entryCls router store = withStore store (\_ _ -> pure nonCanceler) cls
  where
    cls = createClassStateless renderFn
    renderFn { url } = maybe (div' []) id $ runRouter url router



main :: forall e. Eff (console :: CONSOLE, http :: HTTP, fs :: FS, buffer :: BUFFER | e) Unit
main =
  let
    app = 
      getRequestData
      :>>= _.url >>> handle

    store :: Store (Array Musician)
    store = mkStoreG initialState

    -- | On the fronend the top most component is `browserRouterClass` but
    -- | since its render method runs `runRouter url router` we can use
    -- | `runRouter` here directly. This is safe since react is checking if it
    -- | can reuse markup by comparing a checksum (a modified version of
    -- | `adler32` algorithm):
    -- | https://github.com/facebook/react/blob/b1b4a2fb252f26fe10d29ba60d85ff89a85ff3ec/src/renderers/dom/stack/server/ReactMarkupChecksum.js#L50 
    renderRouter :: String -> String
    renderRouter url = renderToString $ createElement (entryCls router store) {url} []

    renderHtml :: Array Musician -> String -> String
    renderHtml state appHtml =
      """
      <!DOCTYPE html>
        <head>
          <meta charset="utf-8" />
          <meta name="viewport" content="width=device-width, initial-scale=1.0" />
          <title>Purescript Isomorphic React App</title>
          <script>window.__REDOX_STATE__ = """ <> (stringify $ encodeJson state) <> """</script>
          <script>console.log(window.__REDOX_STATE__)</script>
          <script defer src="/static/index.js"></script>
        </head>
        <body>
          <div id="app">""" <> appHtml <> """</div>
        </body>
      </html>
      """

    fileNotFound =
      writeStatus statusNotFound
      :*> headers []
      :*> respond (Tuple "<h1>Not Found</h1>" UTF8)

    handle url = if S.take 8 url == "/static/" || url == "/favicon.ico"
                   then fileServer "dist" fileNotFound
                   else 
                     writeStatus statusOK
                     :*> contentType textHTML
                     :*> closeHeaders
                     :*> respond (renderHtml initialState (renderRouter url))

  in runServer defaultOptionsWithLogging {} app
