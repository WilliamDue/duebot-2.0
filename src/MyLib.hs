module MyLib (eventHandler) where

import Control.Monad (when, void)
import UnliftIO.Concurrent
import Data.Text (isPrefixOf, toLower, Text)
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types
import qualified Discord.Requests as R

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
    MessageCreate m -> when (isPing m && not (fromBot m)) $ do
        void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
        threadDelay (2 * 10^(6 :: Int))
        void $ restCall (R.CreateMessage (messageChannelId m) "Pong!")
    _anyOtherFailure -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `isPrefixOf`) . toLower . messageContent
