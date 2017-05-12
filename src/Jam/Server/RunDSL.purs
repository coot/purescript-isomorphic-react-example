module Jam.Server.RunDSL where

import Prelude
import Data.Array as A
import Control.Monad.Aff (Aff)
import Data.Array (foldMap)
import Data.Newtype (ala, un)
import Data.Ord.Max (Max(..))
import Jam.Types (Musician(..), NewMusician)

-- | You will likely not want to build an interpreter with a cofree comonad,
-- | since you will need to retrieve results of your actions.  For example
-- | `addMusician` takes `NewMusician` type which lacks the `id` field.  Usually
-- | you want to send back the id to the client.  So here `addMusician` returns
-- | a `Tuple` with the newly created `Musician` and new state.  If you'd use
-- | `Cofree` to interpret this action, this wouldn't be possible.
addMusician
  :: forall eff
   . Array Musician
  -> NewMusician
  -> Aff eff {newMusician :: Musician, newState :: (Array Musician)}
addMusician st m =
  let max = ala Max foldMap (_.id <<< un Musician <$> st)
      newMusician = Musician { id: (max + 1), name: m.name, description : m.description, wiki: m.wiki, generes: m.generes }
  in pure $ { newMusician, newState: (A.snoc st newMusician) }

removeMusician :: forall eff. Array Musician -> Musician -> Aff eff (Array Musician)
removeMusician st (Musician {id: mId}) = pure $ A.filter (\(Musician m) -> m.id /= mId) st
