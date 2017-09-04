module Fugacious.MailQueue.Internal
    ( DeliverMail
    , Handle (..)
    ) where

import qualified Data.Text as T

type DeliverMail = T.Text -> IO ()

data Handle = Handle
    { -- | We formulate popping mail from the queue indirectly.  The naive
      -- signature here would be something like:
      --
      -- > pop :: IO [T.Text]
      --
      -- This does not work great in practice: if the applications is killed
      -- /after/ we have popped text from the queue, but /before/ it is saved
      -- somewhere else, it is lost.
      --
      -- Using the definition:
      --
      -- > pop :: (T.Text -> IO ()) -> IO ()
      --
      -- Allows implementations to deal with this more gracefully: they can
      -- "peek" at the queue and only delete it after the user-supplied IO
      -- action finishes.
      pop :: DeliverMail -> IO ()
    }
