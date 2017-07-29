import XMonad hiding (Tall, Connection)
import XMonad.Operations

-- import Graphics.X11.Xlib

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Ratio ((%))

import Control.Concurrent
import System.Cmd

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

import XMonad.Config.Mate
import XMonad.Config.Desktop

import XMonad.ManageHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.HintedTile
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Named
import XMonad.Layout.Reflect
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.Maximize

import XMonad.Prompt
import XMonad.Prompt.Ssh
import XMonad.Prompt.Layout
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.VirtualBox

import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect

import XMonad.Util.Run

main = do
  dbus <- D.connectSession
  getWellKnownName dbus
  xmonad $ withUrgencyHook NoUrgencyHook $ mateConfig {
       borderWidth          = 1
       , terminal           = "urxvt"
       , workspaces         = ["1:shells", "2:code", "3:web", "4:chat", "5:music", "6:office", "7:gimp", "8:rdesktop", "9:misc" ]
       , modMask            = mod4Mask
       , normalBorderColor  = "#dddddd"
       , focusedBorderColor = "#0033ff"
       , logHook            = dynamicLogWithPP $ myPrettyPrinter dbus
       , manageHook         = manageHook mateConfig <+> myManageHook <+> manageDocks
       , keys               = \c -> mykeys c `M.union` keys mateConfig c
       , startupHook        = startupHook mateConfig <+> setWMName "LG3D"
       , layoutHook         = desktopLayoutModifiers $ noBorders $ onWorkspace "4:chat" chatL $ onWorkspace "7:gimp" gimpL $ defaultL
       , handleEventHook    = fullscreenEventHook
       }
  where

defaultL   = named "tabbed" $avoidStruts $ smartBorders $ maximize $ tabbed shrinkText myTabConfig
chatL      = named "chat" $ avoidStruts $ smartBorders $ withIM (1%6) (Or (Role "roster") (Or (Title "Buddy-Liste") (Title "Contact List"))) defaultL
gimpL      = named "gimp" $ avoidStruts $ smartBorders $ withIM (1%9) (Role "gimp-toolbox") $ reflectHoriz $ withIM (1%6) (Role "gimp-dock") defaultL

mykeys (XConfig {modMask = modm}) = M.fromList $
  [
  -- rotate workspaces
    ((modm .|. controlMask, xK_Right), nextWS)
  , ((modm .|. controlMask, xK_Left), prevWS)

  -- switch to previous workspace
  , ((modm, xK_z), toggleWS)

  -- lock the screen with xscreensaver
  , ((modm,               xK_l), spawn "slock")

  -- grid select
  , ((modm,               xK_g), goToSelected defaultGSConfig)

  -- some programs to start with keybindings.
  , ((modm .|. shiftMask, xK_b), spawn "firefox")
  , ((modm .|. shiftMask, xK_g), spawn "gajim")

  -- prompts
  , ((modm .|. controlMask, xK_s), sshPrompt xpc)
  , ((modm .|. controlMask, xK_l), layoutPrompt xpc)
  , ((modm .|. controlMask, xK_m), manPrompt xpc)
  , ((modm .|. controlMask, xK_p), shellPrompt xpc)
  , ((modm .|. controlMask, xK_v), vboxPrompt xpc)

  -- window navigation keybindings.
  , ((modm,               xK_Right), sendMessage $ Go R)
  , ((modm,               xK_Left ), sendMessage $ Go L)
  , ((modm,               xK_Up   ), sendMessage $ Go U)
  , ((modm,               xK_Down ), sendMessage $ Go D)
  , ((modm .|. shiftMask, xK_Right), sendMessage $ Swap R)
  , ((modm .|. shiftMask, xK_Left ), sendMessage $ Swap L)
  , ((modm .|. shiftMask, xK_Up   ), sendMessage $ Swap U)
  , ((modm .|. shiftMask, xK_Down ), sendMessage $ Swap D)
  ]

xpc :: XPConfig
xpc = defaultXPConfig { font = "xft:Droid Sans Mono:size=8:antialias=true:hinting=true" }

myTabConfig :: Theme
myTabConfig = defaultTheme { fontName = "xft:Droid Sans Mono:size=8:antialias=true:hinting=true" }

myPrettyPrinter :: D.Client -> PP
myPrettyPrinter dbus = defaultPP {
    ppOutput  = outputThroughDBus dbus
  , ppTitle   = pangoColor "#0066FF" . shorten 80 . pangoSanitize
  , ppCurrent = pangoColor "#FF0000" . wrap "[" "]" . pangoSanitize
  , ppVisible = pangoColor "#663366" . wrap "(" ")" . pangoSanitize
  , ppHidden  = wrap " " " "
  , ppUrgent  = pangoColor "red"
  }

myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , className =? "Pidgin" --> doF(W.shift "4:chat")
    , className =? "Gajim" --> doF(W.shift "4:chat")
    , className =? "Chrome" --> doF(W.shift "3:web")
    , className =? "Chromium-browser" --> doF(W.shift "3:web")
    , className =? "chromium-browser-chromium" --> doF(W.shift "3:web")
    , className =? "Firefox" --> doF(W.shift "3:web")
    , className =? "Iceweasel" --> doF(W.shift "3:web")
    , className =? "Icedove" --> doF(W.shift "3:web")
    , className =? "Quodlibet" --> doF(W.shift "5:music")
    , className =? "OpenOffice.org 3.4" --> doF(W.shift "6:office")
    , className =? "Eclipse" --> doF(W.shift "2:code")
    , className =? "Gimp" --> doF(W.shift "7:gimp")
    , className =? "Wfica" --> doF(W.shift "8:rdesktop")
    , className =? "xfreerdp" --> doF(W.shift "8:rdesktop")
    , className =? "MPlayer" --> doFloat
    , className =? "mpv" --> doFloat
    , className =? "hl2_linux" --> doFullFloat
    ]

-- This retry is really awkward, but sometimes DBus won't let us get our
-- name unless we retry a couple times.
getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()

outputThroughDBus :: D.Client -> String -> IO ()
outputThroughDBus dbus str = do
  let signal = (D.signal (D.objectPath_ "/org/xmonad/Log") (D.interfaceName_ "org.xmonad.Log") (D.memberName_ "Update")) {
          D.signalBody = [D.toVariant ("<span font=\"Terminus 7 Bold\">" ++ (UTF8.decodeString str) ++ "</span>")]
      }
  D.emit dbus signal

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
 where
  left  = "<span foreground=\"" ++ fg ++ "\">"
  right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
 where
  sanitize '>'  acc = "&gt;" ++ acc
  sanitize '<'  acc = "&lt;" ++ acc
  sanitize '\"' acc = "&quot;" ++ acc
  sanitize '&'  acc = "&amp;" ++ acc
  sanitize x    acc = x:acc

