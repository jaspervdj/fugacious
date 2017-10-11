{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Fugacious.Web.Views
    ( template
    , index
    , exception
    , inbox
    , mail
    ) where

import           Control.Applicative         ((<|>))
import           Control.Monad               (forM_)
import           Data.Maybe                  (listToMaybe)
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import qualified Data.Time                   as Time
import           Data.Version                (showVersion)
import qualified Fugacious.Database          as Database
import           Fugacious.ParseMail
import           Fugacious.Web.Routes
import qualified Fugacious.Web.Routes        as Routes
import qualified Paths_fugacious
import           Text.Blaze.Html             (Html, (!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.HTML.SanitizeXSS       as SanitizeXSS

template :: T.Text -> Html -> Html
template title body = H.docTypeHtml $ do
    H.head $ do
        H.title $ H.toHtml title
        H.link ! A.rel "stylesheet" ! A.type_ "text/css"
            ! A.href (Routes.renderAttr GetAssetsStyle)
    H.body $ do
        H.header $ H.a ! A.href (Routes.renderAttr GetIndex) $ "fugacious"
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
    H.form ! A.class_ "login" ! A.action (Routes.renderAttr PostUsers)
            ! A.method "POST" $ do
        H.input ! A.type_ "text" ! A.size "10" ! A.id "address" ! A.name "address"
        H.code $ "@" <> H.toHtml domain
        H.br
        H.input ! A.type_ "submit" ! A.value "Claim address"

userHeader
    :: Time.UTCTime
    -> Database.User
    -> H.Html
    -> H.Html
userHeader now user controls = H.p $ do
    H.div ! A.class_ "controls" $ controls
    "Signed in as "
    H.code (H.toHtml (Database.uAddress user))
    " - "
    "your account expires in "
    H.em $ H.toHtml (max 0 minutes)
    " minutes"
  where
    minutes :: Int
    minutes = round $
        toRational (Database.uExpires user `Time.diffUTCTime` now) / 60

inbox :: Time.UTCTime -> Database.User -> [Database.Mail] -> Html
inbox now user@Database.User {..} emails = template (uAddress <> " - fugacious") $ do
    userHeader now user $ do
        H.form ! A.action (Routes.renderAttr $ PostUsersRenew uId) ! A.method "POST" $
            H.input ! A.type_ "submit" ! A.value "Renew"
        H.form ! A.action (Routes.renderAttr $ GetInbox uId) ! A.method "GET" $
            H.input ! A.type_ "submit" ! A.value "Refresh"
        H.form ! A.action (Routes.renderAttr $ PostUsersLogout uId) ! A.method "POST" $
            H.input ! A.type_ "submit" ! A.value "Logout"
    H.h1 "Inbox"
    if null emails
        then H.p $ do
            "You have no emails.  You can receive email at "
            H.code (H.toHtml uAddress)
            "."
        else H.table $ do
            H.tr $ do
                H.th "From"
                H.th "Subject"
            forM_ emails $ \msg -> H.tr $ do
                H.td $ H.toHtml (Database.mFrom msg)
                H.td $ H.a ! A.href (Routes.renderAttr $ GetInboxMail uId (Database.mId msg) False) $
                    H.toHtml (Database.mSubject msg)

mail :: Time.UTCTime -> Database.User -> T.Text -> ParsedMail -> Html
mail now user@Database.User {..} msgId msg = template (uAddress <> " - fugacious") $ do
    userHeader now user $ do
        H.form ! A.action (Routes.renderAttr $ GetInbox uId) ! A.method "GET" $
            H.input ! A.type_ "submit" ! A.value "Back"
    H.h1 $ H.toHtml $ pmSubject msg
    H.p $ do
        "From "
        H.toHtml (pmFrom msg)
    H.div ! A.class_ "body" $ renderBody (pmBody msg)
    H.p $ H.a ! A.href (Routes.renderAttr $ GetInboxMail uId msgId True) $
        "view source"
  where
    renderBody (PlainTextBody txt) = H.pre $ H.code $ H.toHtml txt
    renderBody (HtmlBody txt) = H.preEscapedToHtml $
        SanitizeXSS.sanitizeXSS $ txt
    renderBody (MultipartBody parts) = case preferredPart parts of
        Just b  -> renderBody b
        Nothing -> "Empty multipart body"

    preferredPart parts =
        listToMaybe [b | b@(HtmlBody _) <- parts] <|>
        listToMaybe [b | b@(PlainTextBody _) <- parts]
