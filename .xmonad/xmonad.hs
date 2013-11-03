import XMonad.Layout.MultiToggle(mkToggle,single)
import XMonad.Layout.MultiToggle  as MTg
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowArranger
import XMonad.Layout.BorderResize
import XMonad.Layout.BoringWindows
import XMonad.Layout.ButtonDecoration
import XMonad.Layout.Decoration
import XMonad.Layout.DecorationAddons
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PositionStoreFloat
import XMonad.Layout.WindowSwitcherDecoration
import XMonad.Layout.WindowNavigation
import XMonad.Layout.DragPane
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Circle
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimpleFloat
import XMonad.Layout.IM
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Reflect
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.ResizeScreen
import XMonad.Layout.Named
import XMonad.Layout.DwmStyle
import XMonad.Layout.Monitor

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.PositionStoreHooks
import XMonad.Hooks.Minimize
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.Place
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops

import XMonad.Actions.CycleWS
import XMonad.Actions.NoBorders

import XMonad.Util.Themes
import XMonad.Util.WorkspaceCompare
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)

import System.Exit
import System.IO
import Graphics.X11.ExtraTypes.XF86
import Data.Monoid(mappend)
import Data.Ratio ((%))
import qualified Data.Map        as M
import qualified XMonad.StackSet as W
import XMonad hiding ( (|||) )
 
myTerminal       = "xfce4-terminal"
myBorderWidth    = 1

myWorkspaces     = ["1:Home", "2:read", "3:web", "4:code" ,"5:chat"] ++ map show [6..6]
startupWorkspace = "3:web" 


--Source : https://github.com/davidbrewer/xmonad-ubuntu-conf/blob/master/xmonad.hs
myCurrentWSColor = "#e6744c" -- color of active workspace
myVisibleWSColor = "#c185a7" -- color of inactive workspace
myUrgentWSColor  = "#cc0000" -- color of workspace with 'urgent' window
myCurrentWSLeft  = "["       -- wrap active workspace
myCurrentWSRight = "]"
myVisibleWSLeft  = "("       -- wrap inactive workspace
myVisibleWSRight = ")"
myUrgentWSLeft   = "{"       -- wrap urgent workspace
myUrgentWSRight  = "}"
 
myModMask        = mod1Mask
--myModMask         = mod4Mask
 
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"


clock = monitor
     { prop = ClassName "Dclock"
     --, rect = Rectangle 0 0 40 20 -- rectangle 40x20 in upper left corner
     , rect = Rectangle (1280-50) (800-20) 50 20
      -- avoid flickering
     , persistent = True
      -- make the window transparent
     , opacity = 0.5
      -- hide on start
     , visible = True
      -- assign it a name to be able to toggle it independently of others
     , name = "clock"
     }
 


myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
 
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((mod4Mask,               xK_space ), sendMessage NextLayout)
    , ((mod4Mask, xK_Return), sendMessage ToggleStruts >> sendMessage ToggleLayout)
    , ((modm,                    xK_a), sendMessage MirrorShrink)
    , ((modm,                    xK_z), sendMessage MirrorExpand)
    , ((modm,                    xK_u), focusUrgent             )
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_n     ), refresh)
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_Down  ), windows W.focusDown)
    , ((modm,               xK_Right ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm,               xK_Up    ), windows W.focusUp  )
    , ((modm,               xK_Left  ), windows W.focusUp  )
    , ((modm,               xK_m     ), windows W.focusMaster  )
    , ((modm,               xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_Left  ), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_Right ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_Down  ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm .|. shiftMask, xK_Up    ), windows W.swapUp    )
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    , ((modm .|. shiftMask, xK_Left  ), sendMessage (MoveLeft      20))
    , ((modm .|. shiftMask, xK_Right ), sendMessage (MoveRight     20))
    , ((modm .|. shiftMask, xK_Down  ), sendMessage (MoveDown      20))
    , ((modm .|. shiftMask, xK_Up    ), sendMessage (MoveUp        20))
    --, ((mod4Mask              , xK_Left ), sendMessage (IncreaseLeft  20))
    --, ((mod4Mask              , xK_Right), sendMessage (IncreaseRight 20))
    --, ((mod4Mask              , xK_Down ), sendMessage (IncreaseDown  20))
    --, ((mod4Mask              , xK_Up   ), sendMessage (IncreaseUp    20))
    --, ((mod4Mask .|. shiftMask, xK_Left ), sendMessage (DecreaseLeft  20))
    --, ((mod4Mask .|. shiftMask, xK_Right), sendMessage (DecreaseRight 20))
    --, ((mod4Mask .|. shiftMask, xK_Down ), sendMessage (DecreaseDown  20))
    --, ((mod4Mask .|. shiftMask, xK_Up   ), sendMessage (DecreaseUp    20))
    --, ((modm     .|. shiftMask, xK_a ), sendMessage Arrange              )
    --, ((modm     .|. shiftMask, xK_d ), sendMessage DeArrange            )
    --, ((modm                  , xK_f ), sendMessage $ MTg.Toggle REFLECTY)
    --, ((modm                  , xK_y ), sendMessage $ MTg.Toggle REFLECTX)
    , ((modm                           ,xK_x), sendMessage $ MTg.Toggle  MIRROR)
    , ((modm                       ,xK_b), sendMessage ToggleStruts)
    , ((modm                       ,xK_g), withFocused toggleBorder)
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm              , xK_q     ), restart "xmonad" True)
  --, ((modm .|. controlMask , xK_t     ),spawn "gnome-terminal")
    , ((modm .|. controlMask , xK_t     ),spawn "terminator")
    , ((modm .|. controlMask , xK_e     ),spawn "thunar")
    , ((modm .|. controlMask , xK_l     ),spawn "gnome-screensaver-command --lock")
    , ((modm .|. controlMask , xK_q     ),spawn "indicator-remindor -q")
    , ((modm .|. controlMask , xK_m     ),spawn "indicator-remindor -m")
    , ((modm .|. controlMask , xK_z     ),spawn "zim")
    , ((modm .|. controlMask , xK_y     ),spawn "terminator")
    , ((mod4Mask              , xK_f    ),spawn "xfe" )
    , ((mod4Mask              , xK_t    ),spawn "xfce4-terminal" )
    , ((mod4Mask              , xK_v    ),spawn "ptoggle tint2" )
    , ((mod4Mask              , xK_c    ),spawn "ptoggle dclock -geometry 53x22-8-50" )
    , ((0                     , xK_Print),spawn "scrot" )
  --, ((0                     , xK_Print),spawn "xfce4-screenshooter -f" ) --https://github.com/liuexp/arch-script/blob/master/.xmonad/easyxmotion.py
    , ((modm                 , xK_v), spawn "easyxmotion.py --colour='#0fff00' --font='-adobe-helvetica-bold-r-normal-*-24-*-*-*-*-*-iso8859-1'")
    -- Switching to layouts
    , ((mod4Mask .|. shiftMask              , xK_1           ), sendMessage $ JumpToLayout "Tiled"       )
    , ((mod4Mask .|. shiftMask              , xK_2           ), sendMessage $ JumpToLayout "ThreeCol"    )
    , ((mod4Mask .|. shiftMask              , xK_3           ), sendMessage $ JumpToLayout "Tiled1"      )
    , ((mod4Mask .|. shiftMask              , xK_4           ), sendMessage $ JumpToLayout "Grid"        )
    , ((mod4Mask .|. shiftMask              , xK_5           ), sendMessage $ JumpToLayout "Circle"      )
    , ((mod4Mask .|. shiftMask              , xK_6           ), sendMessage $ JumpToLayout "Floating"    )
    , ((mod4Mask .|. shiftMask              , xK_7           ), sendMessage $ JumpToLayout "DragPane"    )
    , ((mod4Mask .|. shiftMask              , xK_8           ), sendMessage $ JumpToLayout "Tiled2"      )
    , ((mod4Mask .|. shiftMask              , xK_9           ), sendMessage $ JumpToLayout "MyGrid"      )
    , ((mod4Mask .|. shiftMask              , xK_0           ), sendMessage $ JumpToLayout "Custom"      )
--    , ((mod4Mask .|. shiftMask              , xK_minus       ), sendMessage $ JumpToLayout "SimpleFloat" )
--    , ((mod4Mask .|. shiftMask              , xK_equal       ), sendMessage $ JumpToLayout "Full"        )
--    , ((mod4Mask .|. shiftMask              , xK_BackSpace   ), sendMessage $ JumpToLayout "Tiled2"      )
--    , ((mod4Mask .|. shiftMask              , xK_KP_Divide   ), sendMessage $ JumpToLayout "Fullscreen"  )
--    , ((mod4Mask .|. shiftMask              , xK_KP_Multiply ), sendMessage $ JumpToLayout "MyTabbed"    )
--    , ((mod4Mask .|. shiftMask              , xK_KP_Subtract ), sendMessage $ JumpToLayout "MyGrid"      )



    , ((modm                   , xK_z), withFocused (sendMessage . maximizeRestore))
    , ((mod4Mask               , xK_m), withFocused minimizeWindow)
    , ((mod4Mask  .|. shiftMask, xK_m), sendMessage RestoreNextMinimizedWin)

    , ((mod4Mask, xK_i), sendMessage ShrinkSlave) -- %! Shrink a slave area
    , ((mod4Mask, xK_u), sendMessage ExpandSlave) -- %! Expand a slave area
    , ((mod4Mask, xK_Right), sendMessage $ Go R)
    , ((mod4Mask, xK_Left ), sendMessage $ Go L)
    , ((mod4Mask, xK_Up   ), sendMessage $ Go U)
    , ((mod4Mask, xK_Down ), sendMessage $ Go D)
    , ((mod4Mask .|. shiftMask, xK_Right), sendMessage $ Swap R)
    , ((mod4Mask .|. shiftMask, xK_Left ), sendMessage $ Swap L)
    , ((mod4Mask .|. shiftMask, xK_Up   ), sendMessage $ Swap U)
    , ((mod4Mask .|. shiftMask, xK_Down ), sendMessage $ Swap D)


 , ((0, xF86XK_AudioRaiseVolume), sequence_ [spawn "amixer -q set Master 10%+",spawn "notify-send $(vol)" ])
 , ((0, xF86XK_AudioLowerVolume), sequence_ [spawn "amixer -q set Master 10%-",spawn "notify-send $(vol)" ])
 , ((0, xF86XK_AudioMute), sequence_ [spawn "amixer -q set Master toggle",spawn "notify-send $(vol)" ])
 --, ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master 10%+")
 --, ((0, xF86XK_AudioLowerVolume),spawn "amixer -q set Master 10%-")
 --, ((0, xF86XK_AudioMute), spawn "amixer -q set Master toggle")
 --, ((0, xF86XK_Mail), spawn "urxvtc -e abook")
    --Cycle 
   , ((modm .|. controlMask,              xK_Right), nextWS)
   , ((modm .|. controlMask,              xK_Left ), prevWS)
   , ((modm .|. controlMask .|.shiftMask, xK_Right), shiftToNext >> nextWS )
   , ((modm .|. controlMask .|.shiftMask, xK_Left ), shiftToPrev >> prevWS )
    ]
    ++
 
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((m .|. modm .|. controlMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
 
    [((m .|. modm .|. controlMask, k), windows $ f i)
        | (i, k) <- zip myWorkspaces numPadKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip myWorkspaces numPadKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]



numPadKeys = [ xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down -- 1, 2, 3
             , xK_KP_Left, xK_KP_Begin, xK_KP_Right     -- 4, 5, 6
             , xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up   -- 7, 8, 9
             , xK_KP_Insert]                            -- 0
 
 
------------------------------------------------------------------------
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
 
------------------------------------------------------------------------


--      -- default tiling algorithm partitions the screen into two panes
--      tiled   = ResizableTall nmaster delta ratio [] 
--  
--      -- The default number of windows in the master pane
--      nmaster = 1
--  
--      -- Default proportion of screen occupied by master pane
--      ratio   = 1/2
--  
--      -- Percent of screen to increment by when resizing panes
--      delta   = 3/100
 


customtheme = myTheme
   where
        aTheme = (theme deiflTheme)
        myTheme = aTheme
            {
                activeBorderColor = activeColor aTheme,
                inactiveBorderColor = inactiveColor aTheme,
                urgentBorderColor = urgentColor aTheme,
                fontName = "xft:Monospace:bold:size=9",
                decoWidth = 500,
                decoHeight = 18
            }

tilingDeco l = windowSwitcherDecorationWithButtons shrinkText defaultThemeWithButtons (draggingVisualizer l)
floatingDeco l = buttonDeco shrinkText defaultThemeWithButtons l

tiled         = named "Tiled"       (ResizableTall 1 (3/100) (1/2) [] )
threecol      = named "ThreeCol"    (ThreeColMid 1 (3/100) (3/4))
grid          = named "Grid"        Grid
circle        = named "Circle"      Circle
floating      = named "Floating"    (floatingDeco $ maximize $ borderResize $ positionStoreFloat)
tiled1        = named "Tiled1"      (tilingDeco $ maximize $ mouseResizableTileMirrored)
tiled2        = named "Tiled2"      (tilingDeco $ maximize $ mouseResizableTile)
mygrid        = named "MyGrid"      (dwmStyle shrinkText customtheme Grid)
dragpane      = named "DragPane"    (dragPane Horizontal 0.1 0.5)
--simplefloats  = named "SimpleFloat" simpleFloat
--full          = named "Full"        Full
--fullscreen    = named "Fullscreen"  (tilingDeco $ maximize $ smartBorders Full)
--centre        = named "Centre"      (resizeHorizontal 400 $ resizeHorizontalRight 400 Full)
--mytabbed      = named "MyTabbed"    (tabbedBottom shrinkText customtheme)

commonlayout = (tiled ||| circle ||| grid ||| tiled1 ||| threecol ||| mygrid ||| tiled2 ||| dragpane  ||| floating )

myLayout      = avoidStruts $ commonlayout
codeLayout    = avoidStruts $ (circle         ||| commonlayout)
readLayout    = avoidStruts $ (mygrid         ||| commonlayout)
homeLayout    = avoidStruts $ (tiled1         ||| commonlayout)
chatLayout    = avoidStruts $ (custom         ||| commonlayout)
      where
      custom  = named "Custom" (withIM (1%5) (And (ClassName "Pidgin") (Role "buddy_list")) Grid )

------------------------------------------------------------------------
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
-- Source: https://github.com/kveras/configs/blob/master/.xmonad/xmonad.hs
--http://thinkingeek.com/2011/11/21/simple-guide-configure-xmonad-dzen2-conky/
myManageHooke = composeAll . concat $
 [ [ className   =? c --> doFloat           | c <- myFloats]
      , [ className   =? c --> floatCenter       | c <- myFloatCenter]
      , [ className   =? c --> float             | c <- myFloatSimple]
      , [ className   =? c --> floatFull         | c <- myFloatFull]
      , [ resource    =? r --> doFloat           | r <- myIgnore]
      , [ title       =? t --> doFloat           | t <- myOtherFloats]
      , [ className   =? c --> doF (W.shift "2:read") | c <- readApps]
      , [ className   =? c --> doF (W.shift "3:web")  | c <- webApps]
      , [ className   =? c --> doF (W.shift "4:code") | c <- codeApps]
   -- , [ className   =? c --> doF (W.shift "5:chat") | c <- chatApps]
      ]
   where

     float = placeHook simpleSmart <+> doFloat <+> insertPosition Master Newer 
     floatCenter = doCenterFloat <+> insertPosition Master Newer
     floatFull = floatFull <+> insertPosition Master Newer
 
     myFloats      = ["MPlayer","Indicator-remindor", "Zim","Pavucontrol","Guake.py","Wrapper","Artha","Xfce4-appfinder","xfce4-panel","Gimp","Orage"]
     myFloatCenter = [""]
     myFloatFull   = [""]
     myFloatSimple = [""]
     myOtherFloats = ["Pavucontrol"]
     chatApps      = ["Pidgin"]  -- open on desktop 5
     codeApps      = ["Eclipse","Codeblocks"] -- open on desktop 4
     webApps       = ["Firefox","Chromium-browser"] -- open on desktop 3
     readApps      = ["Evince","Wine"]  -- open on desktop 2
     myIgnore      = ["stalonetray-one"]


myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myStartupHook  = do
   windows $ W.greedyView startupWorkspace
   spawn "nitrogen --restore"
   --spawn "trayer --edge top --align right --widthtype pixel --heighttype pixel --expand true --align right --SetDockType true --SetPartialStrut true --tint 0x000000 --transparent true --alpha 0 --margin 0 --height 8 --width 120"
   ewmhDesktopsStartup

main  = do 

        xmproc  <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobarrc"
        xmproc1 <- spawnPipe "/usr/bin/tint2 ~/.xmonad/tint2rc"
        --xmproc1 <-  spawnPipe "/usr/bin/xmobar ~/.xmobarrc1"

 
--
        xmonad $ withUrgencyHook NoUrgencyHook  defaultConfig {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
 
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
 
        layoutHook = avoidStruts $ mkToggle (single MIRROR) $ --mkToggle (single REFLECTX) $ mkToggle (single REFLECTY)$ windowArrangeAll $ 
        minimize $ boringWindows $ windowNavigation $ toggleLayouts Full $ 
      --onWorkspaces ["3:web" ] customLayout $
        onWorkspaces ["1:Home"] homeLayout $
        onWorkspaces ["4:code"] codeLayout $
        onWorkspaces ["2:read"] readLayout $
        onWorkspaces ["5:chat"] chatLayout $
        myLayout,
--XT
        handleEventHook = ewmhDesktopsEventHook
                                `mappend` fullscreenEventHook
                                `mappend` minimizeEventHook
                                `mappend` positionStoreEventHook,
        manageHook  =  
                     myManageHooke
                <+>  manageHook defaultConfig 
                <+>  manageMonitor clock
                <+>  manageDocks ,
        logHook = dynamicLogWithPP xmobarPP
                    { ppOutput = hPutStrLn xmproc
                    , ppTitle = xmobarColor "green" "" . shorten 50
                    , ppCurrent = xmobarColor myCurrentWSColor ""
                      . wrap myCurrentWSLeft myCurrentWSRight
                    , ppVisible = xmobarColor myVisibleWSColor ""
                      . wrap myVisibleWSLeft myVisibleWSRight
                    , ppUrgent = xmobarColor myUrgentWSColor ""
                      . wrap myUrgentWSLeft myUrgentWSRight
                    } <+> ewmhDesktopsLogHook,

--XT
        startupHook        = myStartupHook
    }
