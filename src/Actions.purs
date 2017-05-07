module Actions where

import Prelude
import Control.Monad.Free (Free, liftF)
import Data.List (List)
import Data.Newtype (class Newtype)

newtype Musician = Musician
  { id :: Int
  , name :: String
  , description :: String
  , wiki :: String
  , generes :: List String
  }

derive instance newtypeMusician :: Newtype Musician _

instance showMusician :: Show Musician where
  show (Musician m) = "Musician " <> m.name

data MusCmd a
  = AddMusician Musician a
  | RemoveMusician Int a

derive instance functorCommand :: Functor MusCmd

type MusDSL a = Free MusCmd a

addMusician :: Musician -> MusDSL (Array Musician -> Array Musician)
addMusician m = liftF $ AddMusician m id

removeMusician :: Int -> MusDSL (Array Musician -> Array Musician)
removeMusician mId = liftF $ RemoveMusician mId id
