-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.VirtualBox
-- Copyright   :  (C) 2011 Bernd Stolle
-- License     :  BSD3
--
-- Maintainer  :  bsx@0xcafec0.de
-- Stability   :  unstable
-- Portability :  unportable
--
-- A prompt for XMonad to launch VirtualBox VMs
--
-----------------------------------------------------------------------------

module VirtualBox
--module XMonad.Prompt.VirtualBox
    ( -- * Usage
      -- $usage
      vboxPrompt
    ) where

import Prelude hiding (catch)

import System.IO
import System.Process

import XMonad
import XMonad.Util.Run
import XMonad.Prompt

import Control.Exception

import Control.Monad
import Data.Maybe

econst :: Monad m => a -> IOException -> m a
econst = const . return

-- $usage
-- 1. In your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Prompt
-- > import XMonad.Prompt.VirtualBox
--
-- 2. In your keybindings add something like:
--
-- >   , ((modm .|. controlMask, xK_v), vboxPrompt defaultXPConfig)
--
-- It will only complete VMs that are registered with your VirtualBox.
--
-- For detailed instruction on editing the key binding see
-- "XMonad.Doc.Extending#Editing_key_bindings".

data VirtualBox = VirtualBox

instance XPrompt VirtualBox where
    showXPrompt VirtualBox = "Launch VM: "
    commandToComplete _ c = c
    nextCompletion      _ = getNextCompletion

vboxPrompt :: XPConfig -> X ()
vboxPrompt c = do
  sc <- io vboxComplList
  mkXPrompt VirtualBox c (mkComplFunFromList sc) vbox

vbox :: String -> X ()
vbox s = safeSpawn "VBoxManage" ["startvm", s]

vboxComplList :: IO [String]
vboxComplList = do
  (_, hout, _, _) <- runInteractiveCommand "VBoxManage list vms"
  l <- hGetContents hout
  return $ map (filter (/= '"') . concat . take 1 . words)
         $ lines l

