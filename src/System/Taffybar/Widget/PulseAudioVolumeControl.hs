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

import           GI.Gtk
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           System.Taffybar.Context
import Data.Word

import DBus
import DBus.Client

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
      Just paClient -> do _ <- afterScaleButtonValueChanged button (setVolume paClient)
                          return ()
    toWidget button

setup :: VolumeButton -> IO (Maybe Client)
setup button = do
  client <- connectSession
 
  eReply <- getPropertyValue client (methodCall "/org/pulseaudio/server_lookup1" "org.PulseAudio.ServerLookup1" "Address")
            { methodCallDestination = Just "org.PulseAudio1" }
  case (eReply :: Either MethodError String) of
    Left e -> do print e
                 return Nothing
    Right addressString -> do let Just ad = parseAddress addressString
                              paClient <- connect ad
                              enterLoop paClient button
                              return $ Just paClient

enterLoop :: Client -> VolumeButton -> IO ()
enterLoop paClient button = 
  void $ forkIO $ do doMonitor

      where doMonitor = do
              eReply <- getPropertyValue paClient (methodCall "/org/pulseaudio/core1/sink0" "org.PulseAudio.Core1.Device" "Volume")
              case (eReply :: Either MethodError [Word32]) of
                Left err -> print err
                Right v -> do let vol = ((fromIntegral (foldl (+) 0 v)) / (fromIntegral ((length v) * 65536)))::Double
                              scaleButtonSetValue button vol
              threadDelay (1000 * 100)
              doMonitor
              return ()
  

setVolume :: Client -> Double -> IO ()
setVolume paClient val = do
  mError <- setPropertyValue paClient (methodCall "/org/pulseaudio/core1/sink0" "org.PulseAudio.Core1.Device" "Volume") $
    toVariant [(floor $ 65536 * val)::Word32]
  case mError of
    Nothing -> return ()
    Just err -> print err
  
