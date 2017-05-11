module Jam.Types where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (:=), (~>), (.?))
import Data.List (List(..), toUnfoldable, fromFoldable, (:))
import Data.Newtype (class Newtype)
import Prelude (class Show, bind, pure, (<>), ($))

newtype Musician = Musician
  { id :: Int
  , name :: String
  , description :: String
  , wiki :: String
  , generes :: List String
  }

type NewMusician =
  { name :: String
  , description :: String
  , wiki :: String
  , generes :: List String
  }

derive instance newtypeMusician :: Newtype Musician _

instance showMusician :: Show Musician where
  show (Musician m) = "Musician " <> m.name

instance encodeJsonMusician :: EncodeJson Musician where
  encodeJson (Musician m)
     = "id" := m.id
    ~> "name" := m.name
    ~> "description" := m.description
    ~> "wiki" := m.wiki
    ~> "generes" := (toUnfoldable m.generes :: Array String)
    ~> jsonEmptyObject

instance decodeJsonMusician :: DecodeJson Musician where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .? "id"
    name <- obj .? "name"
    description <- obj .? "description"
    generesA <- obj .? "generes"
    let generes = fromFoldable (generesA :: Array String)
    wiki <- obj .? "wiki"
    pure $ Musician { id, name, description, generes, wiki }

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
