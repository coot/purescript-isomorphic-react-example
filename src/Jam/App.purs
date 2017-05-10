module Jam.App where

import Prelude
import Data.Array as A
import Data.List as L
import React.DOM as D
import React.DOM.Props as P
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (Window, htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Node (textContent)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode, elementToNode)
import Data.Argonaut (Json, decodeJson, jsonParser)
import Data.Either (Either(..), either)
import Data.Foldable (intercalate)
import Data.Lens (lens, over, to, view)
import Data.Lens.Types (Lens')
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(..), split) as S
import Jam.Actions (addMusician)
import Jam.App.RunDSL (interpret)
import Jam.Types (Musician(..), NewMusician)
import Network.HTTP.Affjax (AJAX)
import Partial.Unsafe (unsafePartial)
import React (Event, EventHandlerContext, ReactClass, ReactElement, ReactSpec, ReactState, ReactThis, ReadWrite, createClass, createElement, getChildren, getProps, preventDefault, readState, spec, transformState, writeState)
import React.Redox (connect, dispatch, withStore)
import React.Router (Route(..), RouteProps, browserRouterClass, link', (:+))
import React.Router.Types (Router)
import ReactDOM (render)
import ReactHocs.Context (accessContext)
import Redox (REDOX, mkStore)
import Redox (dispatch) as Redox
import Routing.Match.Class (int, lit)
import Unsafe.Coerce (unsafeCoerce)

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

    addMusician = accessContext $ createClass addMusicianSpec

    renderFn this = do
      chlds <- getChildren this
      pure $ D.main' $
        [ link' "/" [ D.text "home" ]
        , createElement indexConn unit []
        , createElement addMusician unit []
        ]
        <> chlds

addMusicianSpec :: forall eff. ReactSpec Unit NewMusician  eff
addMusicianSpec = (spec init renderFn) { displayName = "AddMusician" }
  where
    init = { name: "", description: "", wiki: "", generes: Nil }

    nameL = lens (_.name) (_ { name = _ })
    descL = lens (_.description) (_ { description = _ })
    wikiL = lens (_.wiki) (_ { wiki = _ })

    geneL :: forall r. Lens' { generes :: List String | r } String
    geneL = lens (L.intercalate " " <<< _.generes) (\st gstr -> st { generes = L.fromFoldable (S.split (S.Pattern " ") gstr) })

    updateThroughL
      :: forall e
       .ReactThis Unit NewMusician
      -> Lens' NewMusician String
      -> String
      -> Eff (state :: ReactState ReadWrite | e) Unit
    updateThroughL this _lens x =
      transformState this (over _lens (const x))

    targetValue :: Event -> String
    targetValue ev = (unsafeCoerce ev).target.value

    updateHandler
      :: forall e
       . ReactThis Unit NewMusician
      -> Lens' NewMusician String
      -> Event
      -> EventHandlerContext e Unit NewMusician Unit
    updateHandler this _lens ev = do
      value <- pure (targetValue ev)
      updateThroughL this _lens value

    submitFn this ev = do
      _ <- preventDefault ev
      state <- readState this
      _ <- writeState this init
      dispatch this (addMusician state)

    renderFn this = do
      state <- readState this
      pure $ D.form [ P._id "add-musician", P.onSubmit (submitFn this) ]
        [ D.label'
          [ D.text "name"
          , D.input [ P._type "text", P.value state.name, P.onChange (updateHandler this nameL) ] []
          ]
        , D.label'
          [ D.text "description"
          , D.textarea [ P.value state.description, P.onChange (updateHandler this descL) ] []
          ]
        , D.label'
          [ D.text "WikiPedia link"
          , D.input [ P._type "text", P.value state.wiki, P.onChange (updateHandler this wikiL) ] []
          ]
        , D.label'
          [ D.text "geners"
          , D.input [ P._type "text", P.value (view geneL state), P.onChange (updateHandler this geneL) ] []
          ]
        , D.button' [ D.text "add" ]
        ]


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

main :: forall eff. Eff (dom :: DOM, redox :: REDOX, console :: CONSOLE, ajax :: AJAX | eff) Unit
main = do
    w <- window
    ms <- readRedoxState w
    el <- findElmById (ElementId "app")
    jsonStr <- findElmById (ElementId "redox-state") >>= textContent <<< elementToNode
    let estate = parse jsonStr
    logParseErr estate
    st <- mkStore (either (const []) id estate)
    let cls = withStore st dispatch browserRouterClass
    void $ render (createElement cls {router, notFound: Nothing} []) el
  where
    dispatch = Redox.dispatch (const $ pure unit) interpret
    
    parse :: String -> Either String (Array Musician)
    parse str = do
      json <- jsonParser str
      decodeJson json

    logParseErr :: forall e. Either String (Array Musician) -> Eff ( console :: CONSOLE | e) Unit
    logParseErr (Left err) = log err
    logParseErr (Right _) = pure unit

    castDocument = documentToNonElementParentNode <<< htmlDocumentToDocument

    castToMaybe :: forall a b. Either a b -> Maybe b
    castToMaybe = either (\_ -> Nothing) Just

    findElmById :: forall e. ElementId -> Eff (dom :: DOM | e) Element
    findElmById _id = do
      el <- window >>= document >>= getElementById _id <<< castDocument
      pure $ unsafePartial fromJust el
