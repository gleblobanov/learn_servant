module Main where

import Colog
import Control.Monad.Except (MonadIO (liftIO))
import Control.Monad.Reader (liftIO)
import Data.Text as T (Text, intercalate, pack, take)
import Data.Text.IO as TIO (putStrLn)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (Application, Handler, Server, serve)
import Servant.Types.SourceT (source)
import SiriusChecker as SC
  ( CheckSpellingAPI,
    SpellResult (EmptyResult),
    TextToCheck (TextToCheck),
    checkSpellingAPI,
    grade,
    spellErrorsToSpellResult,
    words,
  )
import YandexSpeller as YS
  ( SpellError,
    checkSpellingQuery,
    format,
    lang,
    options,
    runQuery,
  )

server :: Server CheckSpellingAPI
server = checkSpellingHandler
  where
    checkSpellingHandler :: TextToCheck -> Handler SpellResult
    checkSpellingHandler (TextToCheck t) =
      let check :: (WithLog env T.Text m, MonadIO m) => m SpellResult
          check = do
            logMsg $
              "Start querying Yandex Speller. Checking a text starting\
              \ with this lines: \n\""
                <> T.take 50 t
                <> "...\""
            let query =
                  checkSpellingQuery
                    (Just t)
                    (Just YS.lang)
                    (Just YS.options)
                    (Just YS.format)
            mr <- liftIO $ runQuery query
            case mr of
              Just es -> do
                let sr = spellErrorsToSpellResult es
                logMsg $
                  "Text is succesfully checked.\
                  \ The results are:\n"
                    <> "grade is: "
                    <> (T.pack . show . SC.grade) sr
                    <> "\n"
                    <> "words are: "
                    <> (T.intercalate ", " . SC.words) sr
                    <> "\n"
                return sr
              Nothing -> do
                logMsg ("No spell errors returned." :: Text)
                return EmptyResult
       in liftIO $ usingLoggerT (LogAction TIO.putStrLn) check

app :: Application
app = serve checkSpellingAPI server

main :: IO ()
main = run 8081 app