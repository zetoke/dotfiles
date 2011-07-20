-------------------------------------------------------------------------------
-- xmonad.hs for xmonad-darcs (works for "stable" xmonad at archlinux)
-- Author: Øyvind 'Mr.Elendig' Heggstad <mrelendig AT har-ikkje DOT net>
-- Dmitry 'Zetoke' Ermakov <zetoke AT ya DOT ru>
-------------------------------------------------------------------------------
-- Imports --
-- stuff
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit

-- Regex
-- import Text.Regex.Posix ((=~))

-- actions
import XMonad.Actions.GridSelect

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.FloatNext

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Named
import XMonad.Layout.Tabbed

-------------------------------------------------------------------------------
-- Main --
main :: IO ()
main = xmonad =<< statusBar cmd pp kb conf
  where 
    uhook = withUrgencyHookC NoUrgencyHook urgentConfig
    cmd = "bash -c \"tee >(xmobar -x0) | xmobar -x1\""
    pp = customPP
    kb = toggleStrutsKey
    conf = uhook myConfig

-------------------------------------------------------------------------------
-- Configs --
myConfig = defaultConfig { workspaces = workspaces'
                         , modMask = modMask'
                         , borderWidth = borderWidth'
                         , normalBorderColor = normalBorderColor'
                         , focusedBorderColor = focusedBorderColor'
                         , terminal = terminal'
                         , keys = keys'
                         , layoutHook = layoutHook'
                         , manageHook = floatNextHook <+> manageHook'
                         }

-------------------------------------------------------------------------------
-- Window Management --
manageHook' = composeAll . concat $
	[ [ isFullscreen		--> doFullFloat ]
	, [ className	=? c 		--> doFloat | c <- myFloats ]
	, [ className	=? r 		--> doIgnore | r <- myIgnores ]
	, [ className	=? "Chrome" 	--> doShift "1:www" ]
	, [ className	=? "Tkabber" 	--> doShift "3:im" ]
	, [ insertPosition Below Newer ]
	, [ transience' ]
	]
	where
		
		myFloats	= ["MPlayer", "Gimp", "Pidgin", "Skype", "Idjcgui.py", "Qjackctl"]
		myIgnores	= ["trayer"]
-------------------------------------------------------------------------------
-- Looks --
-- bar
customPP = defaultPP { ppCurrent = xmobarColor "#66A9BA" "" . wrap "<" ">"
                     , ppHidden = xmobarColor "#B3B3B3" "" . wrap "" "²"
                     , ppHiddenNoWindows = xmobarColor "#B3B3B3" ""
                     , ppUrgent = xmobarColor "#FFFFAF" "" . wrap "[" "]" 
                     , ppLayout = xmobarColor "#B3B3B3" ""
                     , ppTitle =  xmobarColor "#B3B3B3" "" . shorten 80
                     , ppSep = xmobarColor "#429942" "" " | "
                     }

-- urgent notification
urgentConfig = UrgencyConfig { suppressWhen = Focused, remindWhen = Dont }

-- borders
borderWidth' = 1
normalBorderColor'  = "#333333"
focusedBorderColor' = "#AFAF87"

-- tabs
tabTheme1 = defaultTheme { decoHeight = 16
                         , activeColor = "#a6c292"
                         , activeBorderColor = "#a6c292"
                         , activeTextColor = "#000000"
                         , inactiveBorderColor = "#000000"
                         }

-- workspaces
workspaces' = ["1:www", "2:dev", "3:im", "4:soc", "5:a/v", "6:mail", "7", "8:wine", "9:tor"]

-- layouts
layoutHook' = tile ||| mtile ||| tab ||| full
  where
    rt = ResizableTall 1 (2/100) (1/2) []
    tile = named "[]=" $ smartBorders rt
    mtile = named "M[]=" $ smartBorders $ Mirror rt
    tab = named "T" $ noBorders $ tabbed shrinkText tabTheme1
    full = named "[]" $ noBorders Full 

-------------------------------------------------------------------------------
-- Terminal --
terminal' = "urxvt"

-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
modMask' = mod4Mask

-- keys
toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask,               xK_Return), spawn $ XMonad.terminal conf) 
    , ((modMask,               xK_p     ), spawn "dmenu_run") 
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun")
    , ((modMask .|. shiftMask, xK_m     ), spawn "claws-mail")
    , ((modMask .|. shiftMask, xK_c     ), kill)
    , ((modMask .|. shiftMask, xK_i	), spawn "tkabber")
    , ((modMask,               xK_c	), spawn "chromium")
    , ((modMask,               xK_w	), spawn "urxvt -e weechat-curses")
    , ((modMask,               xK_Print ), spawn "screenshot scr")

    -- grid
    , ((modMask,               xK_g     ), goToSelected defaultGSConfig)

    -- layouts
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- floating layer stuff
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

    -- refresh
    , ((modMask,               xK_n     ), refresh)

    -- focus
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)
    , ((modMask,               xK_m     ), windows W.focusMaster)

    -- swapping
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modMask,               xK_l     ), sendMessage Shrink)
    , ((modMask,               xK_h     ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorExpand)

    -- for skype
    , ((modMask .|. shiftMask, xK_s	), spawn "amixer sset PCM toggle; amixer sset Side toggle")

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- floatNext
    , ((modMask		     , xK_e	), toggleFloatAllNew)
    ]

    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- ++
    -- mod-[w,e] %! switch to twinview screen 1/2
    -- mod-shift-[w,e] %! move window to screen 1/2
    --[((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --    | (key, sc) <- zip [xK_w, xK_e] [0..]
    --    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-------------------------------------------------------------------------------

