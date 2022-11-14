module SiriusChecker where

import Data.Aeson (FromJSON, ToJSON)
import Data.List as DL (length, map)
import Data.Text as T (Text)
import GHC.Generics (Generic)
import Servant as S (JSON, Post, Proxy (..), ReqBody, type (:>))
import Servant.Types.SourceT (source)
import YandexSpeller as YS (SpellError (word))
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

-- POST text to check and return JSON
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
    grade'
      | errorCount > 5 = 0
      | otherwise = 5 - errorCount
      where
        errorCount = toInteger $ DL.length es
    words' = DL.map YS.word es

checkSpellingAPI :: S.Proxy CheckSpellingAPI
checkSpellingAPI = S.Proxy
