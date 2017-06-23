module Jam.Server where

import Prelude
import Data.String as S
import Control.IxMonad ((:*>), (:>>=))
import Control.Monad.Aff (Aff, nonCanceler)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Argonaut (decodeJson, encodeJson, jsonParser)
import Data.Argonaut.Core (stringify)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (maybe)
import Data.MediaType.Common (textHTML, applicationJSON)
import Data.Tuple (Tuple(..))
import Hyper.Middleware (Middleware)
import Hyper.Node.Server (HttpRequest, HttpResponse, defaultOptionsWithLogging, runServer)
import Hyper.Request (getRequestData, readBody)
import Hyper.Response (ResponseEnded, StatusLineOpen, closeHeaders, contentType, headers, respond, writeStatus)
import Hyper.Status (statusBadRequest, statusNotFound, statusOK)
import Jam.Actions (MusCmd(..))
import Jam.App (router)
import Jam.Server.FileServer (fileServer)
import Jam.Server.RunDSL (addMusician, removeMusician)
import Jam.Types (ApiResponse(ApiError, ApiRemoveMusician, ApiAddMusician), Locations, Musician, MusicianRouteProps, initialState)
import Node.Buffer (BUFFER)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.HTTP (HTTP)
import React (ReactClass, createClassStateless, createElement)
import React.DOM (div')
import React.Redox (withStore)
import React.Router (runRouter)
import React.Router.Types (Router)
import ReactDOM (renderToString)
import Redox (RedoxStore, ReadRedox, WriteRedox, SubscribeRedox, CreateRedox, Store, getState, mkStore, setState)

type ServerAffM e = Aff
  ( http :: HTTP
  , console :: CONSOLE
  , redox :: RedoxStore (read :: ReadRedox, write :: WriteRedox, subscribe :: SubscribeRedox, create :: CreateRedox)
  , avar :: AVAR
  , buffer :: BUFFER
  , fs :: FS
  | e)

-- | Server side interpreter of MusDSL
handleApiRequest
  :: forall e
   . Store (Array Musician)
  -> Middleware (ServerAffM e)
      { request :: HttpRequest
      , response :: HttpResponse StatusLineOpen
      , components :: {}
      }
      { request :: HttpRequest
      , response :: HttpResponse ResponseEnded
      , components :: {}
      }
      Unit
handleApiRequest store =
  let
    decode :: String -> Either String (MusCmd (Array Musician -> Array Musician))
    decode body = do
      json <- jsonParser body
      decodeJson json

  in
    getRequestData
    :>>= _.method >>>
      case _ of
          Left POST ->
            readBody :>>= decode >>>
              case _ of
                Right (AddMusician m _) ->
                  writeStatus statusOK
                  :*> contentType applicationJSON
                  :*> closeHeaders
                  :*> liftEff do
                        state <- getState store
                        { newMusician, newState } <- addMusician state m
                        _ <- setState store newState
                        pure newMusician
                  :>>= (respond <<< stringify <<< encodeJson <<< ApiAddMusician)
                Right (RemoveMusician m _) ->
                  writeStatus statusOK
                  :*> contentType applicationJSON
                  :*> closeHeaders
                  :*> liftEff do
                        state <- getState store
                        newState <- removeMusician state m
                        liftEff $ setState store newState
                  :*> respond (stringify $ encodeJson ApiRemoveMusician)
                Left err ->
                  writeStatus statusBadRequest
                  :*> contentType applicationJSON
                  :*> closeHeaders
                  :*> respond (stringify $ encodeJson (ApiError err))
          _ ->
            writeStatus statusBadRequest
            :*> contentType applicationJSON
            :*> closeHeaders
            :*> respond (stringify $ encodeJson (ApiError "api only accepts POST requests"))

-- | Server side rendering
handleAppRequest
  :: forall e
   . Store (Array Musician)
  -> String
  -> Middleware (ServerAffM e)
      { request :: HttpRequest
      , response :: HttpResponse StatusLineOpen
      , components :: {}
      }
      { request :: HttpRequest
      , response :: HttpResponse ResponseEnded
      , components :: {}
      }
      Unit
handleAppRequest store url =
  writeStatus statusOK
  :*> contentType textHTML
  :*> closeHeaders
  :*> liftEff (getState store)
  :>>= \state -> respond (renderHtml state (renderApp store url))

  where
  -- | On the fronend the top most component is `browserRouterClass` but
  -- | since its render method runs `runRouter url router` we can use
  -- | `runRouter` here directly. This is safe since react is checking if it
  -- | can reuse markup by comparing a checksum (a modified version of
  -- | `adler32` algorithm):
  -- | https://github.com/facebook/react/blob/b1b4a2fb252f26fe10d29ba60d85ff89a85ff3ec/src/renderers/dom/stack/server/ReactMarkupChecksum.js#L50 
  renderApp :: Store (Array Musician) -> String -> String
  renderApp store_ url_ = renderToString $ createElement (entryCls router store_) { url: url_ } []

  entryCls :: Router MusicianRouteProps Locations -> Store (Array Musician) -> ReactClass { url :: String }
  entryCls router store_ = withStore
    store_
    (\_ _ -> pure nonCanceler)
    (createClassStateless \props -> maybe (div' []) id (runRouter props.url router))

  renderHtml :: Array Musician -> String -> String
  renderHtml state appHtml =
    """
    <!DOCTYPE html>
      <head>
        <meta charset="utf-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1.0" />
        <title>Purescript Isomorphic React App</title>
        <link href="/static/style.css" rel="stylesheet" />
        <script id="redox-state" type="application/json">""" <> (stringify $ encodeJson state) <> """</script>
        <script defer src="/static/index.js"></script>
      </head>
      <body>
        <div id="app">""" <> appHtml <> """</div>
      </body>
    </html>
    """

fileNotFound
  :: forall e
   . Middleware (ServerAffM e)
      { request :: HttpRequest
      , response :: HttpResponse StatusLineOpen
      , components :: {}
      }
      { request :: HttpRequest
      , response :: HttpResponse ResponseEnded
      , components :: {}
      }
      Unit
fileNotFound =
  writeStatus statusNotFound
  :*> headers []
  :*> respond (Tuple "<h1>Not Found</h1>" UTF8)

app
  :: forall e
   . Store (Array Musician)
  -> Middleware (ServerAffM e)
      { request :: HttpRequest
      , response :: HttpResponse StatusLineOpen
      , components :: {}
      }
      { request :: HttpRequest
      , response :: HttpResponse ResponseEnded
      , components :: {}
      }
      Unit
app store =
  getRequestData
  :>>= _.url >>>
    case _ of
      url | S.take 8 url == "/static/" || url == "/favicon.ico"
        -> fileServer "dist" fileNotFound
      url | S.take 4 url == "/api"
        -> handleApiRequest store
      url -> handleAppRequest store url

main :: forall e. Eff (console :: CONSOLE, http :: HTTP, fs :: FS, buffer :: BUFFER, avar :: AVAR, redox :: RedoxStore (read :: ReadRedox, write :: WriteRedox, subscribe :: SubscribeRedox, create :: CreateRedox) | e) Unit
main = do
  log "starting..."
  store <- mkStore initialState
  runServer defaultOptionsWithLogging {} (app store)
