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

import GI.Gtk
import Control.Monad.IO.Class

-- $usage
--
-- In order to use this widget add the following line to your
-- @taffybar.hs@ file:
--
-- > main = do
-- >   let vc = pulseAudioVolumeControlNew
--
-- Now you can use @menu@ as any other Taffybar widget.


-- | Create a new XDG Menu Widget.
pulseAudioVolumeControlNew :: MonadIO m =>
                              m GI.Gtk.Widget
pulseAudioVolumeControlNew = do
  button <- volumeButtonNew
  _ <- afterScaleButtonValueChanged button $ \value -> print value
  toWidget button
