module Jam.App where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, warn)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (HISTORY, Window, htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Node (textContent)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode, elementToNode)
import Data.Argonaut (Json, decodeJson, jsonParser)
import Data.Array as A
import Data.Either (Either(..), either)
import Data.Foldable (intercalate)
import Data.Lens (lens, over, to, view)
import Data.Lens.Types (Lens')
import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe (Maybe(..), fromJust, maybe, maybe')
import Data.Newtype (class Newtype, un, unwrap)
import Data.StrMap (StrMap, lookup)
import Data.String (Pattern(..), split, trim, null) as S
import Jam.Actions (addMusician, removeMusician)
import Jam.App.RunDSL (mkInterpret)
import Jam.Types (Locations(MusicianRoute, HomeRoute), Musician(Musician), MusicianRouteProps, NewMusician, initialState)
import Network.HTTP.Affjax (AJAX)
import Partial.Unsafe (unsafePartial)
import Prelude hiding (div)
import React (Event, EventHandlerContext, ReactClass, ReactElement, ReactSpec, ReactState, ReactThis, ReadWrite, createClass, createElement, getChildren, getProps, preventDefault, readState, spec, transformState, writeState)
import React.DOM as D
import React.DOM.Props (unsafeFromPropsArray)
import React.DOM.Props as P
import React.ReactTranstionGroup (createCSSTransitionGroupElement, defaultCSSTransitionGroupProps, tagNameToComponent)
import React.Redox (connect, dispatch, withStore)
import React.Router (Route(Route), browserRouterClass, defaultConfig, goTo, link, link', (:+))
import React.Router.Types (Router)
import React.Spaces (children, element, empty, renderIn, text, (!), (^))
import React.Spaces.DOM (button, div, h1, input, label, p, span, textarea)
import ReactDOM (render)
import ReactHocs.Context (accessContext)
import Redox (RedoxStore, ReadRedox, CreateRedox, SubscribeRedox, WriteRedox, mkStore)
import Redox (dispatch) as Redox
import Routing.Match.Class (int, lit)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

foreign import addMusicianCss ::
  { form :: String
  , label :: String
  , labelName :: String
  , addButton :: String
  }

foreign import homeCss ::
  { home :: String
  , musician :: String
  , musicians :: String
  }

foreign import musicianCss ::
  { title :: String
  , description :: String
  , generes :: String
  , wikiLink :: String
  , remove :: String
  , container :: String
  }

foreign import musicianTransitionCss ::
  { enter :: String
  , enterActive :: String
  , leave :: String
  , leaveActive :: String
  , appear :: String
  , appearActive :: String
  }

foreign import styleCss :: {}

unsafeLookup :: String -> StrMap String -> String
unsafeLookup n = maybe' (const err) id <<< lookup n
  where
    err = unsafePerformEff do
      warn ("className lookup failed for '" <> n <> "'")
      pure ""

newtype Store = Store (Array Musician)

derive instance newtypeStore :: Newtype Store _

index :: ReactClass {musicians :: Array Musician}
index = createClass $ (spec unit renderFn) { displayName = "Index" }
  where

    showMusician :: Musician -> ReactElement
    showMusician (Musician u) = D.li [ P.className homeCss.musician ] [ link' defaultConfig ("/user/" <> show u.id) [ D.text u.name ] ]

    renderFn this = do
      { musicians: mus } <- getProps this
      pure $ D.ul [ P.className homeCss.musicians ] (showMusician <$> mus)

homeRouteCls :: ReactClass (MusicianRouteProps Locations)
homeRouteCls = createClass $ (spec unit (map (renderIn D.main') <<< renderFn))
    { displayName = "HomeRouteCls" }
  where
    indexConn = connect (Proxy :: Proxy (Array Musician)) (to id) (\_ musicians _ -> { musicians }) index

    addMusician = accessContext $ createClass addMusicianSpec

    renderFn this = do
      chlds <- getChildren this
      pure $ do
        element (link defaultConfig { to: "/", props: [ P.className homeCss.home ] } [ D.text "home" ])
        indexConn ^ unit
        addMusician ^ unit
        element $
          createCSSTransitionGroupElement
            (defaultCSSTransitionGroupProps
              { component = tagNameToComponent "div"
              , transitionName = musicianTransitionCss
              , transitionEnterTimeout = 300
              , transitionLeaveTimeout = 300
              })
            (unsafeFromPropsArray [])
            chlds

addMusicianSpec :: forall eff. ReactSpec Unit NewMusician (timer :: TIMER | eff)
addMusicianSpec = (spec init renderFn)
    { displayName = "AddMusician" }
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
      -- render using
      -- [purescript-react-spaces](https://github.com/coot/purescript-react-spaces)
      -- combinator library
      pure $ renderIn (D.form [ P._id "add-musician", P.className (addMusicianCss.form), P.onSubmit (submitFn this) ]) $ do
        label ! P.className (addMusicianCss.label) $ do
          span ! P.className (addMusicianCss.labelName) $ do
            text "name"
          input ! P._type "text" ! P.value state.name ! P.onChange (updateHandler this nameL) $ empty
        label ! P.className (addMusicianCss.label) $ do
          span ! P.className (addMusicianCss.labelName) $ do
            text "description"
          textarea ! P.value state.description ! P.onChange (updateHandler this descL) $ empty
        label ! P.className (addMusicianCss.label) $ do
          span ! P.className (addMusicianCss.labelName) $ do
            text "WikiPedia link"
          input ! P._type "text" ! P.value state.wiki ! P.onChange (updateHandler this wikiL) $ empty
        label ! P.className (addMusicianCss.label) $ do
          span ! P.className (addMusicianCss.labelName) $ do
            text "geners"
          input ! P._type "text" ! P.value (view geneL state) ! P.onChange (updateHandler this geneL) $ empty
        button ! P.className addMusicianCss.addButton $ do
          text "add musician"

musicianRouteCls :: ReactClass (MusicianRouteProps Locations)
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
    musicianConn = connect (Proxy :: Proxy (Array Musician)) (to id) (\_ mus { mId } -> { musician: findMus mus mId }) musician

    renderFn this = do
      mId <- getProps this >>= pure <<< unsafePartial unsafeMusicianId <<< _.arg <<< unwrap
      pure $ createElement musicianConn { mId } []

musician :: ReactClass { musician :: Maybe Musician }
musician = accessContext $ createClass $ (spec unit renderFn)
    { displayName = "Musician" }
  where

    unsafeMusicianId :: Partial => Locations -> Int
    unsafeMusicianId l = case l of MusicianRoute id_ -> id_

    removeHandler this ev = do
      { musician: mm } <- getProps this
      case mm of
        Just m -> do
          void $ dispatch this $ removeMusician m
          goTo defaultConfig (show HomeRoute)
        Nothing -> pure unit

    renderFn this = do
      { musician: mm } <- getProps this
      case mm of
        Nothing -> pure $ D.main' []
        Just (Musician m) ->
          let wikiHref = S.trim m.wiki
              wikiElem = if S.null wikiHref
                           then []
                           else [ D.a [ P.href wikiHref, P.className musicianCss.wikiLink ] [ D.text "Read more on WikiPedia." ] ]
          in do
            -- render using
            -- [purescript-react-spaces](https://github.com/coot/purescript-react-spaces)
            -- combinator library
            pure $ renderIn (D.main [ P.key ("musician-" <> (maybe "" (show <<< _.id <<< un Musician) mm)), P.className musicianCss.container ])
              do
                h1 ! P.className musicianCss.title $ do
                  text m.name
                button ! P.className musicianCss.remove ! P.onClick (removeHandler this) $ do
                  text "ㄨ"
                p ! P.className musicianCss.description $ do
                  text m.description
                  children wikiElem
                div ! P.className musicianCss.generes $ do
                  text (intercalate ", " m.generes)

router :: Router MusicianRouteProps Locations
router =
  Route "home" (HomeRoute <$ (lit "")) homeRouteCls :+
    (Route "musician" (MusicianRoute <$> (lit "user" *> int)) musicianRouteCls :+ Nil)
    : Nil

foreign import readRedoxState_ :: forall eff. (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Window -> Eff (dom :: DOM | eff) (Maybe Json)

readRedoxState :: forall eff. Window -> Eff (dom :: DOM | eff) (Maybe Json)
readRedoxState = readRedoxState_ Just Nothing

main
  :: forall eff
   . Eff
      ( dom :: DOM
      , redox :: RedoxStore
        ( read :: ReadRedox
        , write :: WriteRedox
        , subscribe :: SubscribeRedox
        , create :: CreateRedox)
      , console :: CONSOLE
      , ajax :: AJAX
      , err :: EXCEPTION
      , history :: HISTORY
      | eff)
      Unit
main = do
    w <- window
    ms <- readRedoxState w
    el <- findElmById (ElementId "app")
    jsonStr <- findElmById (ElementId "redox-state") >>= textContent <<< elementToNode
    let estate = parse jsonStr
    logParseErr estate
    st <- mkStore (either (const initialState) id estate)
    cls <- withStore st (dispatch st) (browserRouterClass defaultConfig)
    void $ render (createElement cls {router, notFound: Nothing} []) el
  where
    dispatch store = Redox.dispatch (const $ pure unit) (mkInterpret store)
    
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
