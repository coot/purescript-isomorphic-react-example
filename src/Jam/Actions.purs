module Jam.Actions where

import Prelude
import Control.Monad.Free (Free, liftF)
import Jam.Types (Musician, NewMusician)

data MusCmd a
  = AddMusician NewMusician a
  | RemoveMusician Int a

derive instance functorCommand :: Functor MusCmd

type MusDSL a = Free MusCmd a

addMusician :: NewMusician -> MusDSL (Array Musician -> Array Musician)
addMusician m = liftF $ AddMusician m id

removeMusician :: Int -> MusDSL (Array Musician -> Array Musician)
removeMusician mId = liftF $ RemoveMusician mId id
