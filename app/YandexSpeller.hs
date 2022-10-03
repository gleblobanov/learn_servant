module YandexSpeller where

import Data.List ( map )
import Data.Aeson ( FromJSON )
import Data.Text as TIO ( Text, intercalate, pack )
import GHC.Generics ( Generic )
import Network.HTTP.Client ( defaultManagerSettings, newManager )
import Servant as S
  ( Application,
    Handler,
    JSON,
    Post,
    Proxy (..),
    ReqBody,
    Server,
    serve,
    type (:>),
  )
import Servant.API ( JSON, type (:>), QueryParam, Get )
import Servant.Client
    ( client,
      mkClientEnv,
      runClientM,
      ClientM,
      BaseUrl(BaseUrl, baseUrlScheme, baseUrlHost, baseUrlPath,
              baseUrlPort),
      Scheme(Http) )

highestGrade :: Integer
highestGrade = 5

lowestGrade :: Integer
lowestGrade = 0

spellerURLHost :: String
spellerURLHost = "speller.yandex.net"

spellerURLPort :: Int
spellerURLPort = 80

spellerURLPath :: String
spellerURLPath = "/services/spellservice.json"

spellerCheckTextMethod :: Text
spellerCheckTextMethod = "checkText"

lang :: Text
lang = "ru"

format :: Text
format = "plain"

options :: Integer
options =  sumOptions [FindRepeatWords, IgnoreCapitalization]

data SpellerOptions
  = IgnoreDigits
  | IgnoreUrls
  | FindRepeatWords
  | IgnoreCapitalization

optionToInteger :: SpellerOptions -> Integer
optionToInteger IgnoreDigits = 2
optionToInteger IgnoreUrls = 4
optionToInteger FindRepeatWords = 8
optionToInteger IgnoreCapitalization = 512

sumOptions :: [SpellerOptions] -> Integer
sumOptions = sum . Data.List.map optionToInteger

data SpellerErrors
  = ErrorUnknownWord
  | ErrorRepeatWord
  | ErrorCaptialization
  | ErrorTooManyErrors
  | ErrorUnkown

integerToError :: Integer -> SpellerErrors
integerToError 1 = ErrorUnknownWord
integerToError 2 = ErrorRepeatWord
integerToError 3 = ErrorCaptialization
integerToError 4 = ErrorTooManyErrors
integerToError _ = ErrorUnkown

data SpellError where
  SpellError ::
    { word :: Text,
      s :: [Text],
      code :: Integer,
      pos :: Integer,
      row :: Integer,
      col :: Integer,
      len :: Integer
    } ->
    SpellError
  deriving (Eq, Generic)

instance FromJSON SpellError

prettyPrintSE :: SpellError -> Text
prettyPrintSE se
  = "{"
  <> pack "word: " <> word se
  <> "s: " <> ss
  <> "pos: " <> (pack . show . pos) se
  <> "row: " <> (pack . show . row) se
  <> "col: " <> (pack . show . col) se
  <> "len: " <> (pack . show . len) se
  <> "}"
  where ss = TIO.intercalate ", " $ s se

type YandexSpellerAPI =
  "checkText"
    :> QueryParam "text" Text
    :> QueryParam "lang" Text
    :> QueryParam "options" Integer
    :> QueryParam "format" Text
    :> Get '[JSON] [SpellError]

yandexSpellerApi :: S.Proxy YandexSpellerAPI
yandexSpellerApi = S.Proxy

checkSpellingQuery ::
  Maybe Text ->
  Maybe Text->
  Maybe Integer ->
  Maybe Text ->
  ClientM [SpellError]
checkSpellingQuery = client yandexSpellerApi

runQuery :: ClientM [SpellError] -> IO (Maybe [SpellError])
runQuery query = do
  manager' <- newManager defaultManagerSettings
  let env =
        mkClientEnv manager' $
          BaseUrl
            { baseUrlScheme = Http,
              baseUrlHost = spellerURLHost,
              baseUrlPath = spellerURLPath,
              baseUrlPort = spellerURLPort
            }
  res <- runClientM query env
  case res of
    Left err -> do
      print $ "Error: " ++ show err
      return Nothing
    Right es -> return $ Just es