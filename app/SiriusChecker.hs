module SiriusChecker where

import Data.Aeson ( ToJSON, FromJSON )
import Data.Text as T ( Text )
import GHC.Generics ( Generic )
import Servant as S ( Post, JSON, ReqBody, type (:>), Proxy(..) )
import Servant.Types.SourceT (source)
import YandexSpeller as YS ( SpellError(word) )
import Data.List as DL ( map, length )
import Prelude hiding (words)

data TextToCheck where
  TextToCheck :: {textToCheck :: Text} -> TextToCheck
  deriving (Eq, Show, Generic)

instance FromJSON TextToCheck

data SpellResult where
  SpellResult :: {grade :: Integer, words :: [Text]} -> SpellResult
  EmptyResult :: SpellResult
  deriving (Eq, Show, Generic)

instance ToJSON SpellResult

-- POST textk to check and return JSON
type CheckSpellingAPI =
  "check_spelling"
    :> ReqBody '[JSON] TextToCheck
    :> Post '[JSON] SpellResult

spellErrorsToSpellResult :: [SpellError] -> SpellResult
spellErrorsToSpellResult es =
  SpellResult
    { grade = grade',
      words = words'
    }
  where
    grade' | errorCount > 5 = 0
           | otherwise = 5 - errorCount
      where 
        errorCount = toInteger $ DL.length es
    words' = DL.map YS.word es

checkSpellingAPI :: S.Proxy CheckSpellingAPI
checkSpellingAPI = S.Proxy
