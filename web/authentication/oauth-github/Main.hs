{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}


{- This is an example of doing OAuth2 authentication via Github (e.g. if you
are writing a site and want your users to be able to login via Github). In
particular, here we run a tiny site which redirects you to Github (where you
can login) and then shows your Github nick and email.

Here's how to test this example:

    1. Run ngrok (<https://ngrok.com>) locally to serve a local site online.
       When you run `ngrok http 8080`, it will show you a URL like
       “https://fe1b0dc1.ngrok.io” – remember it.

    2. Create a Github application at
       <https://github.com/settings/developers>. When it asks you for a
       callback URL, give it “https://fe1b0dc1.ngrok.io/auth/github”.

    3. Fill in the fields in 'githubAuth' (see below).

    4. Run this example and go to <https://fe1b0dc1.ngrok.io>.

-}
module Main where


-- basic imports
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
-- JSON parsing
import           Data.Aeson              (FromJSON (..), withObject, (.:))
-- Spock (web framework which this example uses)
import           Web.Spock               (SpockM, get, param', redirect, root,
                                          runSpock, spock, text, (<//>))
import           Web.Spock.Config        (PoolOrConn (PCNoDatabase),
                                          defaultSpockCfg)
-- http-client-tls (a library for making HTTPS requests)
import           Network.HTTP.Client.TLS (newTlsManager)
-- OAuth library
import           Network.OAuth.OAuth2    (AccessToken (..), ExchangeToken (..),
                                          OAuth2 (..), accessToken,
                                          authGetJSON, authorizationUrl,
                                          fetchAccessToken)
-- Working with URLs
import           URI.ByteString          (serializeURIRef')
import           URI.ByteString.QQ       (uri)


----------------------------------------------------------------------------
-- Github authentication information. You need to fill in 'oauthClientId'
-- and 'oauthClientSecret', everything else should be left as-is.
----------------------------------------------------------------------------

githubAuth :: OAuth2
githubAuth = OAuth2 {
  oauthClientId =
      "xxxxxxxxxxxxxxxxxxxx",
  oauthClientSecret =
      "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
  oauthOAuthorizeEndpoint =
      [uri|https://github.com/login/oauth/authorize|],
  oauthAccessTokenEndpoint =
      [uri|https://github.com/login/oauth/access_token|],
  oauthCallback =
      Nothing
  }

----------------------------------------------------------------------------
-- Here comes the actual OAuth function. A good explanation of the OAuth2
-- process is here: <https://blog.varonis.com/introduction-to-oauth>.
--
--   1. We redirect user to Github.
--   2. The user clicks “Authorize application” on Github.
--   3. Github redirects the user to https://our.site/auth/github
--       and provides "code" as a query parameter.
--   4. We take "code" and send it to Github with 'fetchAccessToken'.
--       Github sends back an 'AccessToken' which we can now send to Github's
--       various API methods to do things (like getting user info). We ought
--       to store this token in a database or something, but this example
--       doesn't do it.
----------------------------------------------------------------------------

getAccessToken
  :: MonadIO m
  => ExchangeToken                    -- ^ code from the query
  -> m (Either Text AccessToken)      -- ^ token we'll be able to use
                                      --    instead of user's login/pass
getAccessToken code = do
  mgr <- newTlsManager
  liftIO (fetchAccessToken mgr githubAuth code) >>= \case
    Left  err    -> pure $ Left $ TL.toStrict (TL.decodeUtf8 err)
    Right otoken -> pure $ Right $ accessToken otoken

----------------------------------------------------------------------------
-- With the token, we can finally access the /user endpoint.
----------------------------------------------------------------------------

-- | A data structure for describing info we got from Github
data GithubUser = GithubUser {
  ghLogin :: Text,
  ghEmail :: Text
  }
  deriving (Show)

-- | A JSON parser for 'GithubUser'
instance FromJSON GithubUser where
  parseJSON = withObject "Github user" $ \o -> do
    ghLogin <- o .: "login"
    ghEmail <- o .: "email"
    return GithubUser{..}

-- | A function to do a query to Github's API.
--
-- Simple authenticated requests can be made with e.g. 'authGetJSON'. For
-- more complicated requests you can use 'updateRequestHeaders' to add
-- authentication to any 'Request'.
getGithubUser :: MonadIO m => AccessToken -> m (Either Text GithubUser)
getGithubUser token = do
  -- TODO: set API version "application/vnd.github.v3+json"
  mgr <- newTlsManager
  liftIO (authGetJSON mgr token [uri|https://api.github.com/user|]) >>= \case
    Left err   -> pure $ Left $ TL.toStrict (TL.decodeUtf8 err)
    Right user -> pure $ Right user

----------------------------------------------------------------------------
-- This code just defines and runs the server.
----------------------------------------------------------------------------

main :: IO ()
main = do
  config <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 $ spock config app

app :: SpockM () () () ()
app = do
  -- /                  the main page
  get root $ do
    redirect $ T.decodeUtf8 (serializeURIRef' (authorizationUrl githubAuth))

  -- /auth/github       the authentication callback for Github
  get ("auth" <//> "github") $ do
    code <- param' "code"
    token <- getAccessToken (ExchangeToken code) >>= \case
      Left err -> text ("Couldn't get Github access token: " <> err)
      Right t  -> return t
    user <- getGithubUser token >>= \case
      Left err -> text ("Couldn't get Github user info: " <> err)
      Right u  -> return u
    text $ "Your username is: " <> ghLogin user <> "\n" <>
           "Your email is: " <> ghEmail user
