module App where

import Prelude
import Data.Array as A
import React.DOM as D
import React.DOM.Props as P
import Actions (Musician(..))
import App.RunDSL (interpret)
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
import Data.Lens (to)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty, tail)
import Partial.Unsafe (unsafePartial)
import React (ReactClass, ReactElement, createClass, createElement, getChildren, getProps, readState, spec)
import React.Redox (connect, withStore)
import React.Router (Route(..), RouteProps, browserRouterClass, link', (:+))
import React.Router.Types (Router)
import ReactDOM (render)
import Redox (REDOX, mkStore)
import Redox (dispatch) as Redox
import Routing.Match.Class (int, lit)

data Locations
  = HomeRoute
  | MusicianRoute Int

derive instance eqLocations :: Eq Locations

instance showLocations :: Show Locations where
  show HomeRoute = "/"
  show (MusicianRoute uid) = "/user/" <> show uid

newtype Store = Store (Array Musician)

derive instance newtypeStore :: Newtype Store _

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

index :: ReactClass {musicians :: Array Musician}
index = createClass $ (spec unit renderFn) { displayName = "Index" }
  where

    showMusician :: Musician -> ReactElement
    showMusician (Musician u) = D.li [ P.className "user" ] [ link' ("/user/" <> show u.id) [ D.text u.name ] ]

    renderFn this = do
      mus <- getProps this >>= pure <<< _.musicians
      pure $ D.div' (showMusician <$> mus)

homeRouteCls :: ReactClass (RouteProps Locations)
homeRouteCls = createClass $  (spec unit renderFn)
    { displayName = "HomeRouteCls" }
  where
    indexConn = connect (to id) (\_ musicians _ -> { musicians }) index

    renderFn this = do
      chlds <- getChildren this
      pure $ D.main' $
        [ link' "/" [ D.text "home" ]
        , createElement indexConn unit []
        ]
        <> chlds

musicianRouteCls :: ReactClass (RouteProps Locations)
musicianRouteCls = createClass $ (spec unit renderFn)
    { displayName = "MusicianRouteCls" }
  where

    unsafeMusicianId :: Partial => Locations -> Int
    unsafeMusicianId l = case l of MusicianRoute id_ -> id_

    findMus :: Array Musician -> Int -> Maybe Musician
    findMus mus mId =
      case A.findIndex (\(Musician m) -> m.id == mId) mus of
        Nothing -> Nothing
        Just idx -> A.index mus idx

    musicianConn :: ReactClass { mId :: Int }
    musicianConn = connect (to id) (\_ mus { mId } -> { musician: findMus mus mId }) musician

    renderFn this = do
      mId <- getProps this >>= pure <<< unsafePartial unsafeMusicianId <<< _.arg <<< unwrap
      pure $ createElement musicianConn { mId } []

musician :: ReactClass { musician :: Maybe Musician }
musician = createClass $ (spec unit renderFn)
    { displayName = "Musician" }
  where

    unsafeMusicianId :: Partial => Locations -> Int
    unsafeMusicianId l = case l of MusicianRoute id_ -> id_

    renderFn this = do
      mm <- _.musician <$> getProps this
      case mm of
        Nothing -> pure $ D.main' []
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
  Route "home" (HomeRoute <$ (lit "")) homeRouteCls :+
    [ Route "musician" (MusicianRoute <$> (lit "user" *> int)) musicianRouteCls :+ []
    ]

main :: forall eff. Eff (dom :: DOM, redox :: REDOX | eff) Unit
main = do
    el <- findElm
    st <- mkStore initialState
    let cls = withStore st dispatch browserRouterClass
    void $ render (createElement cls {router, notFound: Nothing} []) el
  where
    dispatch = Redox.dispatch (const $ pure unit) interpret

    castDocument = documentToNonElementParentNode <<< htmlDocumentToDocument

    findElm = do
      el <- window >>= document >>= getElementById (ElementId "app") <<< castDocument
      pure $ unsafePartial fromJust el
