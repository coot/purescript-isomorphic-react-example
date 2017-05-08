module Jam.App.RunDSL where

import Prelude
import Data.Array as A
import Control.Comonad.Cofree (Cofree, exploreM, unfoldCofree)
import Control.Monad.Aff (Aff)
import Jam.Actions (MusCmd(..), MusDSL)
import Jam.Types (Musician(..))

newtype RunApp eff a = RunApp
  { addMusician :: Musician -> Aff eff a
  , removeMusician :: Int -> Aff eff a
  }

derive instance functorRunApp :: Functor (RunApp eff)

type AppInterp eff a = Cofree (RunApp eff) a

mkAppInterp :: forall eff. Array Musician -> AppInterp eff (Array Musician)
mkAppInterp state = unfoldCofree id next state
  where
    addMusician :: Array Musician -> Musician -> Aff eff (Array Musician)
    addMusician st = pure <<< A.snoc st

    removeMusician :: Array Musician -> Int -> Aff eff (Array Musician)
    removeMusician st mId = pure $ A.filter (\(Musician m) -> m.id /= mId) st

    next :: Array Musician -> RunApp eff (Array Musician)
    next st = RunApp
      { addMusician: addMusician st
      , removeMusician: removeMusician st
      }

interpret :: forall eff. MusDSL (Array Musician -> Array Musician) -> Array Musician -> Aff eff (Array Musician)
interpret cmds st = exploreM pair cmds $ mkAppInterp st
  where
    pair :: forall e x y. MusCmd (x -> y) -> RunApp e x -> Aff e y
    pair (AddMusician m f) (RunApp i) = f <$> i.addMusician m
    pair (RemoveMusician mId f) (RunApp i) = f <$> i.removeMusician mId
