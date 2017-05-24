module Jam.App.RunDSL where

import Prelude
import Data.Array as A
import Control.Comonad.Cofree (Cofree, exploreM, unfoldCofree)
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (error, log)
import Data.Argonaut (Json, decodeJson, encodeJson)
import Data.Array (foldMap)
import Data.Either (Either(..))
import Data.Foldable (foldl, sequence_)
import Data.Newtype (ala, un)
import Data.Ord.Max (Max(..))
import Jam.Actions (MusCmd(..))
import Jam.Types (ApiResponse(..), Musician(..), NewMusician)
import Network.HTTP.Affjax (AJAX, Affjax, post)
import Redox (REDOX, Store, getState, getSubs)
import Redox.Free (Interp)
import Redox.Utils (addLogger)

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
  -> AppInterp (ajax :: AJAX, console :: CONSOLE, redox :: REDOX | eff) (Array Musician)
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

    addMusician
      :: Array Musician
      -> NewMusician
      -> Aff (ajax :: AJAX, console :: CONSOLE, redox :: REDOX | eff) (Array Musician)
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

          onError err = do
            error $ show err
            _ <- pure $ removeM mId <$> store
            runSubscriptions

          onSuccess { response } =
            case decodeJson response of
              Right (r@ApiAddMusician (Musician m_)) -> do
                log $ "api resp: " <> show r
                _ <- pure $ updateMId mId m_.id <$> store
                runSubscriptions
              Right r -> do
                log $ "api resp: " <> show r
                _ <- pure $ removeM mId <$> store
                runSubscriptions
              Left err -> do
                error $ show ("Error parsing response: " <> err)
                _ <- pure $ removeM mId <$> store
                runSubscriptions

          apiRequest :: Affjax (console :: CONSOLE, redox :: REDOX | eff) Json
          apiRequest = post "/api" (encodeJson $ (AddMusician mr id :: MusCmd (Array Musician -> Array Musician)))
      in do
        -- run api request asynchronously
        _ <- liftEff $ do
          log $ "AddMusician " <> (show mus)
          runAff onError onSuccess apiRequest
        pure $ A.snoc st mus

    removeMusician :: Array Musician -> Musician -> Aff (ajax :: AJAX, console :: CONSOLE, redox :: REDOX | eff) (Array Musician)
    removeMusician st m@(Musician {id: mId}) =
      let
        apiRequest :: Affjax (console :: CONSOLE, redox :: REDOX | eff) Json
        apiRequest = post "/api" (encodeJson $ (RemoveMusician m id :: MusCmd (Array Musician -> Array Musician)))

        onError err = do
          error $ show err
          _ <- pure $ addM m <$> store
          runSubscriptions

        onSuccess { response } =
          case decodeJson response of
            Right (r@ApiAddMusician (Musician m_)) -> do
              log $ "api resp: " <> show r
              _ <- pure $ addM m <$> store
              runSubscriptions
            Right r -> do
              log $ "api resp: " <> show r
            Left err -> do
              error $ show ("Error parsing response: " <> err)
              _ <- pure $ removeM mId <$> store
              runSubscriptions
      in do
        _ <- liftEff $ do
          log $ "removeMusician " <> (show m)
          runAff onError onSuccess apiRequest
        pure $ A.filter (\(Musician m_) -> m_.id /= mId) st

    next :: Array Musician -> RunApp (ajax :: AJAX, console :: CONSOLE, redox :: REDOX | eff) (Array Musician)
    next st = RunApp
      { addMusician: addMusician st
      , removeMusician: removeMusician st
      }

mkInterpret
  :: forall eff
   . Store (Array Musician)
  -> Interp MusCmd (Array Musician) (ajax :: AJAX, console :: CONSOLE, redox :: REDOX | eff)
mkInterpret store cmds st = exploreM pair cmds $ addLogger (mkAppInterp store st)
  where
    pair :: forall e x y. MusCmd (x -> y) -> RunApp e x -> Aff e y
    pair (AddMusician m f) (RunApp i) = f <$> i.addMusician m
    pair (RemoveMusician mId f) (RunApp i) = f <$> i.removeMusician mId
