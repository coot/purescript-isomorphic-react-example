module Jam.Actions where

import Prelude
import Control.Monad.Free (Free, liftF)
import Jam.Types (Musician)

data MusCmd a
  = AddMusician Musician a
  | RemoveMusician Int a

derive instance functorCommand :: Functor MusCmd

type MusDSL a = Free MusCmd a

addMusician :: Musician -> MusDSL (Array Musician -> Array Musician)
addMusician m = liftF $ AddMusician m id

removeMusician :: Int -> MusDSL (Array Musician -> Array Musician)
removeMusician mId = liftF $ RemoveMusician mId id
