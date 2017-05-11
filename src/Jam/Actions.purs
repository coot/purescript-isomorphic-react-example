module Jam.Actions where

import Prelude
import Control.Monad.Error.Class (throwError)
import Control.Monad.Free (Free, liftF)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (:=), (~>), (.?))
import Data.List (fromFoldable, toUnfoldable)
import Jam.Types (Musician, NewMusician)

data MusCmd a
  = AddMusician NewMusician a
  | RemoveMusician Int a

derive instance functorCommand :: Functor MusCmd

instance encodeJsonMusCmd :: EncodeJson (MusCmd a) where
  encodeJson (AddMusician nm _)
     = "type" := "AddMusician"
    ~> "newMusician" := (
           "name" := nm.name
        ~> "description" := nm.description
        ~> "wiki" := nm.wiki
        ~> "generes" := (toUnfoldable nm.generes :: Array String)
        ~> jsonEmptyObject
    )
    ~> jsonEmptyObject
  encodeJson (RemoveMusician _id _)
     = "type" := "RemoveMusician"
    ~> "id" := _id
    ~> jsonEmptyObject

instance decodeJsonMusCmd :: DecodeJson (MusCmd (Array Musician -> Array Musician)) where
  decodeJson json = do
    obj <- decodeJson json
    _type <- obj .? "type"
    case _type of
      "AddMusician" -> do
        nm <- obj .? "newMusician"
        name <- nm .? "name"
        description <- nm .? "description"
        wiki <- nm .? "wiki"
        generesA <- nm .? "generes"
        let generes = fromFoldable (generesA :: Array String)
        pure $ AddMusician {name, description, wiki, generes} id
      "RemoveMusician" -> do
        _id <- obj .? "id"
        pure $ RemoveMusician _id id
      _ -> throwError "no such type"

-- | On the client side we interpret this monad. But since it is not
-- | serializable we rather serialize and send to the backend the unlifted
-- | `MusCmd` values.  For each one of those we have a function that
-- | interpretes it on the backend what we do without build a cofree comonad
-- | (see notes in `Server/RunDSL.purs`).
type MusDSL a = Free MusCmd a

addMusician :: NewMusician -> MusDSL (Array Musician -> Array Musician)
addMusician m = liftF $ AddMusician m id

removeMusician :: Int -> MusDSL (Array Musician -> Array Musician)
removeMusician mId = liftF $ RemoveMusician mId id
