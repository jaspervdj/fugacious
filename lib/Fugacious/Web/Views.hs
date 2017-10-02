{-# LANGUAGE OverloadedStrings #-}
module Fugacious.Web.Views
    ( template
    , index
    , exception
    , inbox
    , mail
    ) where

import           Control.Monad               (forM_)
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import qualified Data.Time                   as Time
import           Data.Version                (showVersion)
import qualified Fugacious.Database          as Database
import           Fugacious.ParseMail
import qualified Paths_fugacious
import           Text.Blaze.Html             (Html, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

template :: T.Text -> Html -> Html
template title body = H.docTypeHtml $ do
    H.head $ do
        H.title $ H.toHtml title
        H.link ! A.rel "stylesheet" ! A.type_ "text/css"
            ! A.href "/assets/style.css"
    H.body $ do
        H.header $ H.a ! A.href "/" $ "fugacious"
        body
        H.footer $ H.toHtml $
            "fugacious-v" ++ showVersion Paths_fugacious.version

exception :: Int -> T.Text -> T.Text -> Html
exception code line body = template ("Error " <> T.pack (show code)) $ do
    H.h1 $ H.toHtml code <> " " <> H.toHtml line
    H.p $ H.toHtml body

index :: T.Text -> Html
index domain = template "Home" $ do
    H.p $ do
        "Fugacious is an email service that allows you to claim short-lived "
        "email addresses and receive (not send) email on those. This is useful "
        "to subscribe to spammy services."
    H.form ! A.class_ "login" ! A.action "/users" ! A.method "POST" $ do
        H.input ! A.type_ "text" ! A.size "10" ! A.id "address" ! A.name "address"
        H.code $ "@" <> H.toHtml domain
        " "
        H.input ! A.type_ "submit" ! A.value "Claim"

inbox :: Time.UTCTime -> Database.User -> [Database.Mail] -> Html
inbox now user emails = template (Database.uAddress user <> " - fugacious") $ do
    H.p $ do
        H.form ! A.class_ "logout" ! A.action logout ! A.method "POST" $
            H.input ! A.type_ "submit" ! A.value "Logout"
        "Signed in as "
        H.code (H.toHtml (Database.uAddress user))
        " - "
        "your account expires in "
        H.em $ H.toHtml minutes
        " minutes"
    H.h1 "Inbox"
    if null emails
        then H.p $ do
            "You have no emails.  You can receive email at "
            H.code (H.toHtml (Database.uAddress user))
            "."
        else H.table $ do
            H.tr $ do
                H.th "From"
                H.th "Subject"
            forM_ emails $ \msg -> H.tr $ do
                H.td $ H.toHtml (Database.mFrom msg)
                H.td $ H.a ! A.href (ref msg) $ H.toHtml (Database.mSubject msg)
  where
    ref msg =
        "/inbox/" <> H.toValue (Database.uId user) <> "/" <>
        H.toValue (Database.mId msg)

    logout =
        "/users/" <> H.toValue (Database.uId user) <> "/logout"

    minutes :: Int
    minutes = floor $
        toRational (Database.uExpires user `Time.diffUTCTime` now) / 60

mail :: Database.User -> ParsedMail -> Html
mail user msg = template (Database.uAddress user <> " - fugacious") $ do
    H.p $ "Welcome " <> H.toHtml (Database.uAddress user)
    H.p $ do
        H.toHtml (pmFrom msg)
        " - "
        H.toHtml (pmSubject msg)
    H.div $ H.a ! A.class_ "body" $ H.pre $ H.code $ H.toHtml $ pmBody msg
