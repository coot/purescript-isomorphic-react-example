module Jam.App.RunDSL where

import Prelude
import Data.Array as A
import Control.Comonad.Cofree (Cofree, exploreM, unfoldCofree)
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (error, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import DOM.HTML.Types (HISTORY)
import Data.Argonaut (Json, decodeJson, encodeJson)
import Data.Array (foldMap)
import Data.Either (Either(..))
import Data.Foldable (foldl, sequence_)
import Data.Newtype (ala, un)
import Data.Ord.Max (Max(..))
import Jam.Actions (MusCmd(..))
import Jam.Types (ApiResponse(..), Locations(..), Musician(..), NewMusician)
import Network.HTTP.Affjax (AJAX, Affjax, post)
import React.Router (goTo)
import Redox (REDOX, Store, getState, getSubs)
import Redox.Free (Interp)

newtype RunApp eff a = RunApp
  { addMusician :: NewMusician -> Aff eff a
  , removeMusician :: Musician -> Aff eff a
  }

derive instance functorRunApp :: Functor (RunApp eff)

type AppInterp eff a = Cofree (RunApp eff) a

mkAppInterp
  :: forall eff
   . Store (Array Musician)
  -> Array Musician
  -> AppInterp (ajax :: AJAX, console :: CONSOLE, redox :: REDOX, dom :: DOM, history :: HISTORY, err :: EXCEPTION | eff) (Array Musician)
mkAppInterp store state = unfoldCofree id next state
  where

    runSubscriptions = do
      subs <- getSubs store
      sta <- getState store
      sequence_ ((_ $ sta) <$> subs)

    addM :: Musician -> Array Musician -> Array Musician
    addM m = flip A.snoc m

    removeM :: Int -> Array Musician -> Array Musician
    removeM mId = A.filter (\(Musician m_) -> m_.id /= mId)

    addMusician st m =
      let max = ala Max foldMap (_.id <<< un Musician <$> st)
          mr = { name: m.name, description : m.description, wiki: m.wiki, generes: m.generes }
          mId = max + 1
          mus = Musician { id: mId, name: m.name, description : m.description, wiki: m.wiki, generes: m.generes }

          updateMId :: Int -> Int -> Array Musician -> Array Musician
          updateMId oldId newId = foldl f []
            where
              f acu (m_@Musician r) =
                if r.id == oldId
                  then A.snoc acu (Musician r { id = newId })
                  else A.snoc acu m_

          onError :: forall err. Show err => err -> Eff _ Unit
          onError err = do
            error $ show err
            _ <- pure $ removeM mId <$> store
            runSubscriptions

          onSuccess { response } =
            case decodeJson response of
              Right r@ApiAddMusician (Musician m_) -> do
                log $ "api resp: " <> show r
                _ <- pure $ updateMId mId m_.id <$> store
                runSubscriptions
              Right r -> do
                onError ("api resp: " <> show r)
              Left err -> do
                onError err

          apiRequest = post "/api" (encodeJson $ (AddMusician mr id :: MusCmd (Array Musician -> Array Musician)))
      in do
        -- run api request asynchronously
        _ <- liftEff $ runAff onError onSuccess apiRequest
        pure $ A.snoc st mus

    removeMusician st m@(Musician {id: mId}) =
      let
        apiRequest = post "/api" (encodeJson $ (RemoveMusician m id :: MusCmd (Array Musician -> Array Musician)))

        onError :: forall err. Show err => err -> Eff _ Unit
        onError err = do
          error $ show err
          _ <- pure $ addM m <$> store
          runSubscriptions
          goTo (show $ MusicianRoute mId)

        onSuccess { response } = 
          case decodeJson response of
            Right r@ApiRemoveMusician ->
              log $ "api resp: " <> show r
            Right r ->
              onError ("api resp: " <> show r)
            Left err ->
              onError err
      in do
        _ <- liftEff $ runAff onError onSuccess apiRequest
        pure $ A.filter (\(Musician m_) -> m_.id /= mId) st

    next st = RunApp
      { addMusician: addMusician st
      , removeMusician: removeMusician st
      }

mkInterpret
  :: forall eff
   . Store (Array Musician)
   -> Interp MusCmd (Array Musician) (ajax :: AJAX, console :: CONSOLE, redox :: REDOX, err :: EXCEPTION, history :: HISTORY, dom :: DOM | eff)
mkInterpret store cmds st = exploreM pair cmds $ mkAppInterp store st
  where
    pair :: forall e x y. MusCmd (x -> y) -> RunApp e x -> Aff e y
    pair (AddMusician m f) (RunApp i) = f <$> i.addMusician m
    pair (RemoveMusician mId f) (RunApp i) = f <$> i.removeMusician mId
