import XMonad
import System.Exit
import System.IO

import XMonad.Actions.CycleWS

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Hooks.RefocusLast

import qualified XMonad.Layout.Fullscreen as FS
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns

import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Decoration

import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

--testing
--import XMonad.Layout.NoFrillsDecoration
--import XMonad.Layout.Decoration
--import XMonad.Layout.SubLayouts
--import XMonad.Layout.WindowNavigation


--testing scratchpads
import qualified XMonad.StackSet as W
import XMonad.ManageHook

--testing Godot floating
import Data.List as L

-- testing polybar
import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (forM_, join)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.NamedWindows (getName)

-- polybar xmonad-log module (github.com/xintron/xmonad-log)
-- import XMonad.Hooks.DynamicLog
-- teste
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

import XMonad.Config.Desktop
-- import XMonad.StackSet as W



--- Basic settings
myTerminal              = "wezterm"
myModMask               = mod4Mask

myBorderWidth           = 2
-- myNormalBorderColor     = "#cccccc" -- default
-- myNormalBorderColor = "#585858" -- srcery xgray-6
myNormalBorderColor = "#575268" -- catppuccin
-- myFocusedBorderColor    = "#673ab7" -- purple
-- myFocusedBorderColor = "#d65d0e" -- gruvbox-dark
-- myFocusedBorderColor    = "#ff8700" -- srcery bright orange
-- myFocusedBorderColor = "ff5f00" -- srcery orange
myFocusedBorderColor = "#C9CBFF"
myPurple                = "#673ab7"
myGray                  = "#cccccc"

topBarTheme             = def
    { inactiveBorderColor   = myGray
    , inactiveColor         = myGray
    , inactiveTextColor     = myGray
    , activeBorderColor     = "#673ab7"
    , activeColor           = "#673ab7"
    , activeTextColor       = "#673ab7"
    , urgentBorderColor     = "#673ab7"
    , urgentTextColor       = "#673ab7"
    , decoHeight            = 5
    }

myWorkspaces            = ["dev", "web", "game", "chat", "5", "6", "7", "8", "9"]


--- Startup hook
myStartupHook = do
  spawn "/home/hz/.config/polybar/launch.sh"
--- Layout settings
mySpacing = spacingRaw True
            (Border 10 10 10 0) -- size of screen edge gaps
            True             -- enable screen edge gaps
            (Border 5 5 5 5) -- size of window gaps
            True             -- enable window gaps

myLayout = avoidStruts
           $ smartBorders
           $ mySpacing
           $ mkToggle (NOBORDERS ?? FULL ?? EOT)
           -- $ addTopBar
           $ tiled
    where
      tiled   = ResizableTall nmaster delta ratio []
      pron    = ThreeColMid nmaster delta ratio
      nmaster = 1
      ratio   = 1/2
      delta   = 0.05
      
      -- addTopBar = noFrillsDeco shrinkText topBarTheme


--- Workspaces


--- Scratchpads
myScratchpads :: [NamedScratchpad]
myScratchpads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "knitting" spawnKnit findKnit manageKnit
                , NS "mpd" spawnMpd findMpd manageMpd
                , NS "vscodium" spawnVscodium findVscodium manageVscodium
                , NS "obsidian" spawnObsidian findObsidian manageObsidian
                ]
    where
        spawnTerm   = myTerminal ++ " start --class=scratchpad"
        findTerm    = resource =? "scratchpad"
        manageTerm  = customFloating $ W.RationalRect l t w h
                      where
                        h = 0.88
                        w = 0.88
                        t = 0.07
                        l = 0.07
        spawnKnit   = myTerminal ++ " start --class=knitting"
        findKnit    = resource =? "knitting"
        manageKnit  = customFloating $ W.RationalRect l t w h
                      where
                        h = 0.3
                        w = 0.45
                        t = 1 -h
                        l = 1 -w
        spawnMpd    = myTerminal ++ " start --class=mpd -e ncmpcpp"
        findMpd     = resource =? "mpd"
        manageMpd   = customFloating $ W.RationalRect l t w h
                      where
                        h = 0.9
                        w = 0.9
                        t = 0.95 -h
                        l = 0.95 -w
        spawnVscodium = "vscodium"
        findVscodium  = resource =? "vscodium"
        manageVscodium = customFloating $ W.RationalRect l t w h
                         where
                           h = 0.9
                           w = 0.9
                           t = 0.95 -h
                           l = 0.95 -w
        spawnObsidian = "obsidian"
        findObsidian = resource =? "obsidian"
        manageObsidian = customFloating $ W.RationalRect l t w h
                         where
                           h = 0.96
                           w = 0.96
                           t = 0.02
                           l = 0.02


--- Hooks
-- Godot test
(~~?) :: Eq a => Query [a] -> [a] -> Query Bool
q ~~? x = fmap (x `L.isInfixOf`) q

myManageHook = composeAll
    [ manageDocks
    , className =? "Godot" --> doFloat
    , isFullscreen          --> doFullFloat
    , className =? "Gimp"   --> doFloat
    , className =? "stalonetray" --> doIgnore
    --, FS.fullscreenManageHook
    ] <+> namedScratchpadManageHook myScratchpads


--- *old* Statusbar settings
myPP            = dzenPP
                  { ppCurrent       = dzenColor "#1b1b1b" "#cccccc" . pad
                  , ppHidden        = dzenColor "#cccccc" "#1b1b1b" . pad
                  , ppWsSep         = ""
                  , ppSep           = " "
                  , ppTitle         = shorten 50
                  , ppLayout        = (\layout -> case layout of
                                      "Spacing ResizableTall" -> "[|]"
                                      "Spacing Full" -> "[F]"
                                      "Spacing ThreeCol" -> "[p]"
                                      layout -> layout
                                      )
                  }



--myLogBar        = "dzen2 -dock -ta l"
-- myLogHook h     = dynamicLogWithPP $ myPP { ppOutput = hPutStrLn h }
--
--
--



myLogHookPolybar = do
  winset <- gets windowset
  title <- maybe (return "") (fmap show . getName) . W.peek $ winset
  let currWs = W.currentTag winset
  let wss = map W.tag $ W.workspaces winset
  let wsStr = join $ map (fmt currWs) $ sort' wss

  io $ appendFile "/tmp/.xmonad-title-log" (title ++ "\n")
  io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")
  --io $ appendFile "/tmp/.xmonad-test-log" (currL)

  where fmt currWs ws
          | currWs == ws = "[" ++ ws ++ "]"
          | otherwise    = " " ++ ws ++ " "
        sort' = sortBy (compare `on` (!! 0))

myLogHookDBus :: D.Client -> PP
myLogHookDBus dbus = def { ppOutput = dbusOutput dbus }
-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
  let signal = (D.signal objectPath interfaceName memberName) {
    D.signalBody = [D.toVariant $ UTF8.decodeString str]
    }
  D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"


myInfoBar       = "conky -c /home/hz/.xmonad/conky_dzen | dzen2 -dock -y 0 -x 1000 -y 1060 -e 'onstart=lower' -ta r"
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)
toggleStrutsss  = do
  spawn "/home/hz/bin/lower-polybar"
  sendMessage (ToggleStruts)



--- Main function
main :: IO ()
main = do
  --meutocohook <- spawnPipe "dzen2 -dock -e 'onstart=lower' -ta l -w 1000 -y 1060"
  --teste <- spawn myInfoBar
  --teste2 <- spawn "/home/hz/.config/polybar/launch.sh"

  -- polybar --
  forM_ [".xmonad-workspace-log", ".xmonad-title-log", ".xmonad-test-log"] $ \file -> do
    safeSpawn "mkfifo" ["/tmp/" ++ file]

  -- xmonad-log polybar module (github.com/xintron/xmonad-log) --
  --dbus <- D.connectSession
  -- Request access to the DBus name
  --D.requestName dbus (D.busName_ "org.xmonad.Log")
      --[ D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]


  xmonad $ docks $ ewmh def {
      terminal              = myTerminal
    , normalBorderColor     = myNormalBorderColor
    , focusedBorderColor    = myFocusedBorderColor
    , borderWidth           = myBorderWidth
    , modMask               = myModMask
    , workspaces            = myWorkspaces
    , layoutHook            = desktopLayoutModifiers $ myLayout
    -- , logHook               = refocusLastLogHook <+> logHook def
    --, logHook               = myLogHook meutocohook
    --, logHook               = myLogHookPolybar
    --, logHook               = dynamicLogWithPP (myLogHookDBus dbus)
    --, handleEventHook       = fullscreenEventHook <+> handleEventHook desktopConfig
    , manageHook            = insertPosition Below Newer <+> myManageHook -- <+> manageHook desktopConfig
    , startupHook           = myStartupHook -- <+> startupHook desktopConfig
    }
    `additionalKeysP`
    [ ("M-q",   kill)
    , ("M-d",   spawn "dmenu_run")
    , ("M-C-<Return>", spawn $ myTerminal ++ " --working-directory $(readlink /proc/$$/cwd)")
    , ("M-,",   moveTo Prev (Not emptyWS))
    , ("M-.",   moveTo Next (Not emptyWS))
    , ("M-z",   prevWS)
    , ("M-x",   nextWS)
    , ("M-u",   sendMessage (MirrorShrink))
    , ("M-S-u", sendMessage (MirrorExpand))
    , ("M-f",   sendMessage (Toggle FULL))
    , ("M-S-h", sendMessage (IncMasterN 1))
    , ("M-S-l", sendMessage (IncMasterN (-1)))
    , ("M-S-r", spawn "killall dzen2; xmonad --recompile; xmonad --restart")
    -- , ("M-S-i", spawn "steam steam://rungameid/250900")
    , ("M-S-c", io (exitWith ExitSuccess))
    -- testing --
    , ("M-b",   toggleStrutsss)
    , ("M-a",   namedScratchpadAction myScratchpads "terminal")
    , ("M-m",   namedScratchpadAction myScratchpads "mpd")
    , ("M-g",   namedScratchpadAction myScratchpads "vscodium")
    , ("M-s",   namedScratchpadAction myScratchpads "knitting")
    , ("M-<Space>", namedScratchpadAction myScratchpads "terminal")
    , ("M-S-<Space>", namedScratchpadAction myScratchpads "obsidian")
    ]
    `removeMouseBindings`
    [ (mod4Mask, button2)
    ]
    `removeKeysP`
    [ ("M-e") ]
