{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widget.PulseAudioVolumeControl
-- Copyright   : (c) Ulf Jasper
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ulf Jasper <ulf.jasper@web.de>
-- Stability   : unstable
-- Portability : unportable
--
-- Volume control widget for PulseAudio.
--
--------------------------------------------------------------------------------

module System.Taffybar.Widget.PulseAudioVolumeControl (
  -- * Usage
  -- $usage
  pulseAudioVolumeControlNew)
where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import DBus
import DBus.Client
import Data.Word
import GI.Gtk
import System.Taffybar.Context

-- $usage
--
-- In order to use this widget add the following line to your
-- @taffybar.hs@ file:
--
-- > main = do
-- >   let vc = pulseAudioVolumeControlNew
--
-- Now you can use @vc@ like any other Taffybar widget.


-- | Create a new XDG Menu Widget.

pulseAudioVolumeControlNew :: TaffyIO Widget
pulseAudioVolumeControlNew = do
  liftIO $ do
    button <- volumeButtonNew
    mClient <- setup button
    case mClient of
      Nothing -> return ()
      Just paClient -> void $ afterScaleButtonValueChanged button
                       (setVolume paClient)
    toWidget button

setup :: VolumeButton -> IO (Maybe Client)
setup button = do
  client <- connectSession
  eReply <- getPropertyValue client
            (methodCall "/org/pulseaudio/server_lookup1"
             "org.PulseAudio.ServerLookup1" "Address")
            {methodCallDestination = Just "org.PulseAudio1"}
  result <- case (eReply :: Either MethodError String) of
              Left e -> do print e
                           return Nothing
              Right addressString -> do let Just ad = parseAddress addressString
                                        print ad
                                        paClient <- connect ad
                                        enterLoop client button
                                        startListening paClient button
                                        return $ Just paClient
  disconnect client
  return result
                                        

enterLoop :: Client -> VolumeButton -> IO ()
enterLoop paClient button = do
  void $ forkIO $ do doMonitor

      where doMonitor = do
              eReply <- getPropertyValue paClient
                        (methodCall "/org/pulseaudio/core1/sink0"
                         "org.PulseAudio.Core1.Device" "Volume")
              case (eReply :: Either MethodError [Word32]) of
                Left err -> print err
                Right v -> scaleButtonSetValue button $
                           (((fromIntegral (foldl (+) 0 v)) /
                             (fromIntegral ((length v) * 65536)))::Double)
                              
              threadDelay (1000 * 100)
              doMonitor
              return ()

-- FIXME: why does this fail?
startListening :: Client -> VolumeButton -> IO ()
startListening paClient button =
  catch (void $ addMatch paClient rule cb) (\e -> print (e::ClientError))

  where rule = matchAny { -- matchPath = Just "/org/pulseaudio/core1/sink0",
                         matchInterface = Just "org.PulseAudio.Core1.Device",
                         matchMember = Just "VolumeUpdated"
                         --matchDestination = Just "org.PulseAudio1"
                         }
        cb = \sig -> print "asdf"


setVolume :: Client -> Double -> IO ()
setVolume paClient val = do
  mError <- setPropertyValue paClient
    (methodCall "/org/pulseaudio/core1/sink0" "org.PulseAudio.Core1.Device"
     "Volume") $
    toVariant [(floor $ 65536 * val)::Word32]
  case mError of
    Nothing -> return ()
    Just err -> print err
  
