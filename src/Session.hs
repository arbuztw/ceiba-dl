{-# LANGUAGE OverloadedStrings #-}
module Session where

import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as L8

import Network.HTTP.Conduit
import Network.HTTP.Types.Status (statusCode)

type Session a = StateT (Manager, CookieJar) IO a

runSession :: Session a -> IO a
runSession session = do
   manager <- newManager tlsManagerSettings
   evalStateT session (manager, createCookieJar [])

reqLbs :: Request -> Session L8.ByteString
reqLbs req = do
   (manager, cookie) <- get
   let req' = req { cookieJar = Just cookie, redirectCount = 0 }
   res <- httpLbs req' manager
   modify (\s -> (fst s, responseCookieJar res))
   return $ responseBody res

getLbs url = parseRequest url >>= reqLbs

postLbs url params = do
   req <- parseRequest url
   let req' = urlEncodedBody params req
   reqLbs req'

