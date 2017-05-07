module App where

import Prelude
import Data.Array as A
import React.DOM as D
import React.DOM.Props as P
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)
import Data.Foldable (find, intercalate)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty, tail)
import Partial.Unsafe (unsafePartial)
import React (ReactClass, ReactElement, createClass, createElement, getChildren, getProps, readState, spec)
import React.Router (Route(..), RouteProps, browserRouterClass, link', (:+))
import React.Router.Types (Router)
import ReactDOM (render)
import Routing.Match.Class (int, lit)

data Locations
  = HomeRoute
  | MusicianRoute Int

derive instance eqLocations :: Eq Locations

instance showLocations :: Show Locations where
  show HomeRoute = "/"
  show (MusicianRoute uid) = "/user/" <> show uid

newtype Musician = Musician
  { id :: Int
  , name :: String
  , description :: String
  , wiki :: String
  , generes :: List String
  }

derive instance newtypeMusician :: Newtype Musician _

newtype Store = Store (Array Musician)

derive instance newtypeStore :: Newtype Store _

store :: Store
store = Store 
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


home :: ReactClass (RouteProps Locations)
home = createClass $ (spec { store } renderFn) { displayName = "Home" }
  where

    showMusician :: Musician -> ReactElement
    showMusician (Musician u) = D.li [ P.className "user" ] [ link' ("/user/" <> show u.id) [ D.text u.name ] ]

    renderFn this = do
      mus <- readState this >>= pure <<< unwrap <<< _.store
      chl <- getChildren this
      pure $ D.main'
        [ link' "/" [ D.text "home" ]
        , D.div' (showMusician <$> mus)
        , D.div' chl
        ]

musician :: ReactClass (RouteProps Locations)
musician = createClass $ (spec unit renderFn)
    { displayName = "Musician" }
  where

    unsafeMusicianId :: Partial => Locations -> Int
    unsafeMusicianId l = case l of MusicianRoute id_ -> id_

    renderFn this = do
      id_<- getProps this >>= pure <<< unsafePartial unsafeMusicianId <<< _.arg <<< unwrap
      case find ((id_ == _) <<< _.id <<< unwrap) (unwrap store) of
        Nothing -> pure $ D.div' [ D.text "404" ]
        Just (Musician m) ->
          pure $ D.main'
            [ D.h1' [ D.text m.name ]
            , D.p'
              [ D.text m.description
              , D.a [ P.href m.wiki ] [ D.text "Read more on WikiPedia." ]
              ]
            , D.div' [ D.text $ intercalate ", " m.generes ]
            ]

router :: Router RouteProps Locations
router =
  Route "home" (HomeRoute <$ (lit "")) home :+
    [ Route "musician" (MusicianRoute <$> (lit "user" *> int)) musician :+ []
    ]

main :: forall eff. Eff (dom :: DOM | eff) Unit
main = void $ (elm >>= render ui)
  where
    ui :: ReactElement
    ui = createElement browserRouterClass {router, notFound: Nothing} []

    castDocument = documentToNonElementParentNode <<< htmlDocumentToDocument

    elm = do
      elm' <- window >>= document >>= getElementById (ElementId "app") <<< castDocument
      pure $ unsafePartial fromJust elm'
