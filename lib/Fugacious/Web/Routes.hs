{-# LANGUAGE OverloadedStrings #-}
module Fugacious.Web.Routes
    ( Route (..)
    , parse
    , render
    , renderAttr
    , renderBs
    ) where

import qualified Data.ByteString    as B
import           Data.Maybe         (isJust)
import           Data.Monoid        ((<>))
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Snap.Core.Extended as Snap
import qualified Text.Blaze.Html    as H

data Route
    = GetIndex
    | PostUsers
    | PostUsersLogout !T.Text                -- ^ User ID
    | PostUsersRenew  !T.Text                -- ^ User ID
    | GetInbox        !T.Text                -- ^ User ID
    | GetInboxMail    !T.Text !T.Text !Bool  -- ^ User ID, Mail ID, view source
    | GetAssetsStyle

parse :: Snap.MonadSnap m => m Route
parse = Snap.route
    [ ("", Snap.ifTop $ return GetIndex)
    , ("/users", Snap.method Snap.POST $ return PostUsers)
    , ("/users/:user/logout", Snap.method Snap.POST $
            PostUsersLogout <$> Snap.requireParam "user")
    , ("/users/:user/renew",  Snap.method Snap.POST $
            PostUsersRenew <$> Snap.requireParam "user")
    , ("/inbox/:user", Snap.ifTop $
            GetInbox <$> Snap.requireParam "user")
    , ("/inbox/:user/:mail", GetInboxMail
            <$> Snap.requireParam "user"
            <*> Snap.requireParam "mail"
            <*> (isJust <$> Snap.getParam "source"))
    , ("/assets/style.css", pure GetAssetsStyle)
    ]

render :: Route -> T.Text
render r = case r of
    GetIndex                      -> "/"
    PostUsers                     -> "/users"
    PostUsersLogout uid           -> "/users/" <> uid <> "/logout"
    PostUsersRenew  uid           -> "/users/" <> uid <> "/renew"
    GetInbox        uid           -> "/inbox/" <> uid
    GetInboxMail    uid mid True  -> "/inbox/" <> uid <> "/" <> mid <> "?source"
    GetInboxMail    uid mid False -> "/inbox/" <> uid <> "/" <> mid
    GetAssetsStyle                -> "/assets/style.css"

renderAttr :: Route -> H.AttributeValue
renderAttr = H.toValue . render

renderBs :: Route -> B.ByteString
renderBs = T.encodeUtf8 . render
