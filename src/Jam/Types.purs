module Jam.Types where

import Control.Alt ((<|>))
import Control.Comonad.Cofree (Cofree)
import Control.Monad.Except (throwError)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Either (Either)
import Data.Lens (lens)
import Data.List (List(..), fromFoldable, toUnfoldable, (:))
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Prelude (class Eq, class Show, bind, pure, show, ($), (<>), (==), (>>=))
import React.Router (class RoutePropsClass)
import Routing.Types (Route) as R

data Locations
  = HomeRoute
  | MusicianRoute Int

derive instance eqLocations :: Eq Locations

instance showLocations :: Show Locations where
  show HomeRoute = "/"
  show (MusicianRoute uid) = "/user/" <> show uid

newtype MusicianRouteProps arg = MusicianRouteProps
  { key :: String
  , id :: String
  , arg :: arg
  , args :: List arg
  , query :: Map String String
  , tail :: List (Cofree List { url :: R.Route, arg :: arg })
  }

derive instance newtypeMusicianRouteProps :: Newtype (MusicianRouteProps arg) _

instance routePropsClassMusicianRouteProps :: RoutePropsClass MusicianRouteProps Locations where
  idLens = lens (\(MusicianRouteProps r) -> r.id) (\(MusicianRouteProps r) id_ -> MusicianRouteProps (r { id = id_ }))
  mkProps name arg args query tail = MusicianRouteProps { id: name, key: locToKey arg, arg, args, query, tail }
    where
      locToKey :: Locations -> String
      locToKey HomeRoute = "home"
      locToKey (MusicianRoute id) = "musician"

newtype Musician = Musician
  { id :: Int
  , name :: String
  , description :: String
  , wiki :: String
  , generes :: List String
  }

instance showMusician :: Show Musician where
  show (Musician m) = "Musician { id: " <> show m.id <> ", name: " <> m.name <> ", description: " <> m.description <> ", wiki: " <> m.wiki <> ", generes: " <> show m.generes <> "}"

derive instance newtypeMusician :: Newtype Musician _

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

type NewMusician =
  { name :: String
  , description :: String
  , wiki :: String
  , generes :: List String
  }

initialState :: Array Musician
initialState =
  [ Musician
      { id: 1
      , name: "Pat Metheny"
      , description: """Patrick Bruce "Pat" Metheny is an American jazz guitarist and composer. He is the leader of the Pat Metheny Group and is also involved in duets, solo works and other side projects. His style incorporates elements of progressive and contemporary jazz, Latin jazz, and jazz fusion. Metheny has three gold albums and 20 Grammy Awards and is the only person to win Grammys in ten different categories. He is the brother of jazz flugelhornist and journalist Mike Metheny."""
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

data ApiResponse
  = ApiError String
  | ApiAddMusician Musician
  | ApiRemoveMusician

instance showApiResponse :: Show ApiResponse where
  show (ApiError err) = "ApiError " <> err
  show (ApiAddMusician m) = "ApiAddMusician " <> show m
  show (ApiRemoveMusician) = "RemoveMusician"

instance encodeJson :: EncodeJson ApiResponse where
  encodeJson (ApiError err) =
       "status" := "error"
    ~> "error" := err
    ~> jsonEmptyObject
  encodeJson (ApiAddMusician m) =
       "status" := "ok"
    ~> "type" := "ApiAddMusician"
    ~> "musician" :=  (encodeJson m)
    ~> jsonEmptyObject
  encodeJson (ApiRemoveMusician) =
       "status" := "ok"
    ~> "type" := "ApiRemoveMusician"
    ~> jsonEmptyObject

instance decodeJson :: DecodeJson ApiResponse where
  decodeJson json = (decodeJson json) >>= decode
    where
      decode obj =
        decodeApiAddMusician obj <|> decodeApiError obj <|> decodeApiRemoveMusician obj

      decodeApiAddMusician obj = do
        mr <- obj .? "musician"
        _type <- (obj .? "type") :: Either String String
        if _type == "ApiAddMusician"
          then do
            m <- decodeJson mr
            pure (ApiAddMusician m)
          else throwError $ "expected ApiAddMusician type instead of: " <> _type
      decodeApiRemoveMusician obj = do
        _type <- (obj .? "type") :: Either String String
        if _type == "ApiRemoveMusician"
          then pure (ApiRemoveMusician)
          else throwError $ "expected ApiRemoveMusician type instead of: " <> _type
      decodeApiError obj = do
        err <- obj .? "error"
        pure (ApiError err)
