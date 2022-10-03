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
      checkSpellingAPI )
import Colog
import Data.Text.IO as TIO ( putStrLn)
import Data.Text as T (pack, take)

server :: Server CheckSpellingAPI
server = checkSpellingHandler
  where
    checkSpellingHandler :: TextToCheck -> Handler SpellResult
    checkSpellingHandler (TextToCheck t) =

      let check :: (WithLog env Message m, MonadIO m) => m SpellResult
          check = do 
            logInfo $ "Start querying Yandex Speller. Checking a text starting with this lines: \n\"" 
              <> T.take 50 t 
              <> "...\""
            let query = checkSpellingQuery 
                  (Just t)
                  (Just YS.lang)
                  (Just YS.options)
                  (Just YS.format)
            mr <- liftIO $ runQuery query
            case mr of
              Just es -> return $ spellErrorsToSpellResult es
              Nothing -> do logError "No spell erorrs returned."
                            return EmptyResult
          
          action = cmap fmtMessage logTextStdout
      in liftIO $ usingLoggerT action check
      

app :: Application
app = serve checkSpellingAPI server

main :: IO ()
main = run 8081 app