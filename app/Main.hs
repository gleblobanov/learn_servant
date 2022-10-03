module Main where

import Control.Monad.Except ( MonadIO(liftIO) )
import Control.Monad.Reader (liftIO)
import Network.Wai ( Application ) 
import Network.Wai.Handler.Warp ( run )
import Servant ( Application, serve, Server, Handler )
import Servant.Types.SourceT (source)
import YandexSpeller as YS
    ( runQuery, format, options, lang, checkSpellingQuery, SpellError )
import SiriusChecker as SC
    ( CheckSpellingAPI,
      SpellResult (EmptyResult),
      TextToCheck(TextToCheck),
      spellErrorsToSpellResult,
      checkSpellingAPI,
      grade,
      words
      )
import Colog
import Data.Text.IO as TIO ( putStrLn)
import Data.Text as T (pack, take, Text, intercalate)

server :: Server CheckSpellingAPI
server = checkSpellingHandler
  where
    checkSpellingHandler :: TextToCheck -> Handler SpellResult
    checkSpellingHandler (TextToCheck t) =

      let check :: (WithLog env T.Text m, MonadIO m) => m SpellResult
          check = do 
            logMsg $ "Start querying Yandex Speller. Checking a text starting with this lines: \n\"" 
              <> T.take 50 t 
              <> "...\""
            let query = checkSpellingQuery 
                  (Just t)
                  (Just YS.lang)
                  (Just YS.options)
                  (Just YS.format)
            mr <- liftIO $ runQuery query
            case mr of
              Just es -> do let sr = spellErrorsToSpellResult es
                            logMsg $ "Text is succesfully checked. The results are:\n"
                              <> "grade is: " <> (T.pack . show . SC.grade) sr <> "\n"
                              <> "words are: " <> (T.intercalate ", " . SC.words) sr
                            return sr
              Nothing -> do logMsg ("No spell errors returned." :: Text)
                            return EmptyResult
          
      in liftIO $ usingLoggerT (LogAction TIO.putStrLn) check
      

app :: Application
app = serve checkSpellingAPI server

main :: IO ()
main = run 8081 app