module Jam.App.RunDSL where

import Prelude
import Data.Array as A
import Control.Comonad.Cofree (Cofree, exploreM, unfoldCofree)
import Control.Monad.Aff (Aff)
import Data.Argonaut (Json, encodeJson)
import Data.Array (foldMap)
import Data.Newtype (ala, un)
import Data.Ord.Max (Max(..))
import Jam.Actions (MusCmd(..), MusDSL)
import Jam.Types (Musician(..), NewMusician)
import Network.HTTP.Affjax (AJAX, Affjax, post)

newtype RunApp eff a = RunApp
  { addMusician :: NewMusician -> Aff eff a
  , removeMusician :: Int -> Aff eff a
  }

derive instance functorRunApp :: Functor (RunApp eff)

type AppInterp eff a = Cofree (RunApp eff) a

mkAppInterp :: forall eff. Array Musician -> AppInterp (ajax :: AJAX | eff) (Array Musician)
mkAppInterp state = unfoldCofree id next state
  where
    addMusician
      :: Array Musician
      -> NewMusician
      -> Aff (ajax :: AJAX | eff) (Array Musician)
    addMusician st m =
      let max = ala Max foldMap (_.id <<< un Musician <$> st)
          mr = { name: m.name, description : m.description, wiki: m.wiki, generes: m.generes }
          mus = Musician { id: (max + 1), name: m.name, description : m.description, wiki: m.wiki, generes: m.generes }
      in do
        _ <- (post "/api" (encodeJson $ (AddMusician mr id :: MusCmd (Array Musician -> Array Musician))) :: Affjax eff Json)
        pure $ A.snoc st mus

    removeMusician :: Array Musician -> Int -> Aff (ajax :: AJAX | eff) (Array Musician)
    removeMusician st mId = do
      _ <- (post "/api" (encodeJson $ (RemoveMusician mId id :: MusCmd (Array Musician -> Array Musician))) :: Affjax eff Json)
      pure $ A.filter (\(Musician m) -> m.id /= mId) st

    next :: Array Musician -> RunApp (ajax :: AJAX | eff) (Array Musician)
    next st = RunApp
      { addMusician: addMusician st
      , removeMusician: removeMusician st
      }

interpret :: forall eff. MusDSL (Array Musician -> Array Musician) -> Array Musician -> Aff (ajax :: AJAX | eff) (Array Musician)
interpret cmds st = exploreM pair cmds $ mkAppInterp st
  where
    pair :: forall e x y. MusCmd (x -> y) -> RunApp e x -> Aff e y
    pair (AddMusician m f) (RunApp i) = f <$> i.addMusician m
    pair (RemoveMusician mId f) (RunApp i) = f <$> i.removeMusician mId
