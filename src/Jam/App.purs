module Jam.App where

import Prelude
import Data.Array as A
import React.DOM as D
import React.DOM.Props as P
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (Window, htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)
import Data.Argonaut (Json, decodeJson, jsonParser)
import Data.Either (Either, either)
import Data.Foldable (intercalate)
import Data.Lens (to)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (class Newtype, unwrap)
import Jam.App.RunDSL (interpret)
import Jam.Types (Musician(..))
import Partial.Unsafe (unsafePartial)
import React (ReactClass, ReactElement, createClass, createElement, getChildren, getProps, spec)
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

foreign import readRedoxState_ :: forall eff. (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Window -> Eff (dom :: DOM | eff) (Maybe Json)

readRedoxState :: forall eff. Window -> Eff (dom :: DOM | eff) (Maybe Json)
readRedoxState = readRedoxState_ Just Nothing

main :: forall eff. Eff (dom :: DOM, redox :: REDOX | eff) Unit
main = do
    w <- window
    ms <- readRedoxState w
    let mstate = (join $ (castToMaybe <<< fromJson) <$> ms) :: Maybe (Array Musician)
    el <- findElm
    st <- mkStore (maybe [] id mstate)
    let cls = withStore st dispatch browserRouterClass
    void $ render (createElement cls {router, notFound: Nothing} []) el
  where
    dispatch = Redox.dispatch (const $ pure unit) interpret

    fromJson :: Json -> Either String (Array Musician)
    fromJson = decodeJson

    castDocument = documentToNonElementParentNode <<< htmlDocumentToDocument

    castToMaybe :: forall a b. Either a b -> Maybe b
    castToMaybe = either (\_ -> Nothing) Just

    findElm = do
      el <- window >>= document >>= getElementById (ElementId "app") <<< castDocument
      pure $ unsafePartial fromJust el
