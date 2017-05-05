module Server where

import Prelude
import Data.String as S
import App (router)
import Control.IxMonad ((:*>), (:>>=))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (textHTML)
import Data.Tuple (Tuple(..))
import Hyper.Node.FileServer (fileServer)
import Hyper.Node.Server (defaultOptionsWithLogging, runServer)
import Hyper.Request (getRequestData)
import Hyper.Response (closeHeaders, contentType, headers, respond, writeStatus)
import Hyper.Status (statusNotFound, statusOK)
import Node.Buffer (BUFFER)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.HTTP (HTTP)
import React.Router (runRouter)
import ReactDOM (renderToString)

main :: forall e. Eff (console :: CONSOLE, http :: HTTP, fs :: FS, buffer :: BUFFER | e) Unit
main =
  let
    app = 
      getRequestData
      :>>= _.url >>> handle

    -- | On the fronend the top most component is `browserRouterClass` but
    -- | since its render method runs `runRouter url router` we can use
    -- | `runRouter` here directly. This is safe since react is checking if it
    -- | can reuse markup by comparing a checksum (a modified version of
    -- | `adler32` algorithm):
    -- | https://github.com/facebook/react/blob/b1b4a2fb252f26fe10d29ba60d85ff89a85ff3ec/src/renderers/dom/stack/server/ReactMarkupChecksum.js#L50 
    renderRouter :: String -> String
    renderRouter url = case runRouter url router of
                         Just re -> renderToString re
                         Nothing -> "<div></div>"

    renderHtml :: String -> String
    renderHtml appHtml =
      """
      <!DOCTYPE html>
        <head>
          <meta charset="utf-8" />
          <meta name="viewport" content="width=device-width, initial-scale=1.0" />
          <title>Purescript Isomorphic React App</title>
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
                     :*> respond (renderHtml (renderRouter url))

  in runServer defaultOptionsWithLogging {} app
