module Main where

import Control.Monad.Except ( MonadIO(liftIO) )
import Control.Monad.Reader (liftIO)
import Network.Wai ( Application ) 
import Network.Wai.Handler.Warp ( run )
import Servant ( Application, serve, Server, Handler )
import Servant.Types.SourceT (source)
import YandexSpeller as YS
    ( runQuery, format, options, lang, checkSpellingQuery )
import SiriusChecker as SC
    ( CheckSpellingAPI,
      SpellResult,
      TextToCheck(TextToCheck),
      spellErrorsToSpellResult,
      checkSpellingAPI )

server :: Server CheckSpellingAPI
server = checkSpellingHandler
  where
    checkSpellingHandler :: TextToCheck -> Handler SpellResult
    checkSpellingHandler (TextToCheck t) = do
      let query =
            checkSpellingQuery
              (Just t)
              (Just YS.lang)
              (Just YS.options)
              (Just YS.format)
      mr <- liftIO $ runQuery query
      case mr of
        Just es -> return $ spellErrorsToSpellResult es
        Nothing -> error "No spell erorrs returned."

app :: Application
app = serve checkSpellingAPI server

main :: IO ()
main = run 8081 app