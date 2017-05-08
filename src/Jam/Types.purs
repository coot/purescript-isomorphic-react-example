module Jam.Types where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (:=), (~>), (.?))
import Data.List (List, toUnfoldable, fromFoldable)
import Data.Newtype (class Newtype)
import Prelude (class Show, bind, discard, pure, (<>), ($), (<$>))

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

