import XMonad
import XMonad.Config.Gnome
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
import XMonad.Actions.Plane
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.Spiral
import XMonad.Layout.Accordion
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.LimitWindows
import XMonad.Layout.FixedColumn
import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.StackSet as W
import XMonad.Prompt.Shell
import XMonad.Config.Desktop
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.ResizableTile
import XMonad.Layout.Dishes
import XMonad.Layout.ToggleLayouts

import Control.OldException(catchDyn,try)
import XMonad.Util.Run
import Control.Concurrent
--import DBus
--import DBus.Connection
--import DBus.Message
import System.Cmd
import XMonad.Prompt
------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
myWorkSpaces = ["1:code1", "2:code2", "3:code3", "4:web", "5:im", "6:logs"] ++ map show [7..9]

------------------------------------------------------------------------
-- Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
myManageHook = composeAll
    [ className =? "Iceweasel" --> doShift "4:web"
    , className =? "Icedove" --> doShift "4:web"
    , resource =? "desktop_window" --> doIgnore
    , className =? "Galculator" --> doFloat
    , className =? "Gimp" --> doFloat
    , className =? "Google-chrome" --> doShift "2:web"
    , resource =? "gpicview" --> doFloat
    , resource =? "kdesktop" --> doIgnore
    , className =? "MPlayer" --> doFloat
    , className =? "skype" --> doFloat
    , className =? "skype" --> doShift "5:im"
    , className =? "Xchat" --> doShift "5:im"
    , className =? "Pidgin" --> doShift "5:im"
    --, className =? "Audacious2" --> doShift "5:media"
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)]

-----------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts. Note that each layout is separated by |||,
-- which denotes layout choice.
--
--
-- myLayout = avoidStruts (
--     withTitles $ tiled |||
--     Mirror tiled |||
--     tabbed shrinkText tabConfig |||
--     Full |||
--     myChat |||
--     myCode |||
--     myBook |||
--     spiral(6/7))
--   where
--     -- default tiling algorithm partitions the screen into two panes.
--     tiled = Tall nmaster delta ratio
--  
--     -- The default number of windows in the master pane.
--     nmaster = 1
--  
--     -- Default proportion of screen occupied by master pane.
--     ratio = 1/2
--  
--     -- Percent of screen to increment by when resizing panes.
--     delta = 3/100
-- --     imLayout = withIM (2/10) (Role "buddy_list") (standardLayouts)
-- --     standardLayouts = tiled ||| Mirror tiled ||| Full
-- -- myLayout = avoidStruts $ onWorkspace "4:vm" Full $ standardLayouts
-- --                where standardLayouts = tiled ||| Mirror tiled ||| Full
-- --                      imLayout = withIM (2/10) (Role "buddy_list") (standardLayouts)
-- --                      tiled = ResizableTall nmaster delta ratio []
-- --                      nmaster = 1
-- --                      delta = 0.03
-- --                      ratio = 0.5
--

myLayout = smartBorders $ toggleLayouts Full perWS
	where
		-- Per workspace layout selection.
		perWS = onWorkspace "1:code1" (withTitles $ codeFirst) $
			onWorkspace "2:code2" (withTitles $ codeFirst) $
			onWorkspace "3:code3" (withTitles $ codeFirst) $
			onWorkspace "4:web" (noTitles $ noBorders $ (rest)) $
			onWorkspace "4:im" (noTitles   $ myChat gridFirst) $
			onWorkspace "6:logs" (withTitles $ logsFirst ) $
			onWorkspace "8" (noTitles $ myBook) $
			-- onWorkspace "book" (noTitles   $ myBook) $
			                   (withTitles $ rest)

		-- Modifies a layout to be desktop friendly with title bars
		-- and avoid the panel.
		withTitles l = noFrillsDeco shrinkText myTheme $ desktopLayoutModifiers l

		-- Modifies a layout to be desktop friendly, but with no title bars
		-- and avoid the panel.
		noTitles l = desktopLayoutModifiers l

		-- Each of these allows toggling through a set of layouts
		-- in the same logical order, but from a different starting
		-- point.
		codeFirst = threeCol ||| myGrid ||| myWide ||| myTabbed ||| myCode
		dishFirst = myDish ||| myCode ||| myWide ||| myGrid
		gridFirst = myGrid ||| myDish ||| myCode ||| myWide
		logsFirst = threeCol ||| myLogs dishFirst ||| myCode ||| myWide
		rest = myTiled ||| Full ||| myTabbed ||| myWide

		myTiled =  Tall nmaster delta ratio
			where
				-- default number of windows in the master pane
				nmaster = 1
				delta   = 3/100
				-- proportion of screen occupied by master pane
				-- proportion of screen occupied by other panes
				ratio = 0.5

		-- This is a tall-like layout with magnification.
		-- The master window is fixed at 80 columns wide, making
		-- this good for coding. Limited to 3 visible windows at
		-- a time to ensure all are a good size.
		myCode = limitWindows 3 $ magnifiercz' 1.4 $
			FixedColumn 1 20 90 10

		-- Stack with one large master window.
		-- It's easy to overflow a stack to the point that windows
		-- are too small, so only show first 5.
		myDish = limitWindows 5 $ Dishes nmaster ratio
			where
				-- default number of windows in the master pane
				nmaster = 1
				-- proportion of screen occupied by other panes
				ratio = 1/5

		-- Wide layout with subwindows at the bottom.
		myWide = Mirror $ Tall nmaster delta ratio
			where
				-- default number of windows in the master pane
				nmaster = 1
				-- Percent of screen to increment by when resizing panes
				delta   = 3/100
				-- proportion of screen occupied by master pane
				ratio   = 80/100

		-- Split screen, optimized for web browsing.
		mySplit = magnifiercz' 1.4 $ Tall nmaster delta ratio
			where
				-- default number of windows in the master pane
				nmaster = 1
				-- Percent of screen to increment by when resizing panes
				delta   = 3/100
				-- proportion of screen occupied by master pane
				ratio   = 60/100

		-- Standard grid.
		myGrid = Grid

		-- The chat workspace has a roster on the right.
		myChat base = mirror base $ withIM size roster
			where
				-- Ratio of screen roster will occupy
				size = 1/ 5
				-- Match roster window
				roster = Title "Buddy List"

		-- The logs workspace has space for procmeter.
		myLogs base = mirror base $ withIM procmeterSize procmeter
			where
				-- Ratio of screen procmeter will occupy
				procmeterSize = 1 / 7
				-- Match procmeter
				procmeter = ClassName "ProcMeter3"

		-- For reading books, I typically want borders on
		-- the margin of the screen.
		myBook = ThreeColMid nmaster delta ratio
			where
				-- default number of windows in the master pane
				nmaster = 1
				-- Percent of screen to increment by when resizing panes
				delta   = 3/100
				-- proportion of screen occupied by master pane
				ratio   = 2/3

		threeCol = ThreeCol nmaster delta ratio
			where
				-- default number of windows in the master pane
				nmaster = 1
				-- Percent of screen to increment by when resizing panes
				delta   = 3/100
				-- proportion of screen occupied by master pane
				ratio   = 1/3

		myTabbed = tabbed shrinkText tabConfig
			where
				-- default number of windows in the master pane
				nmaster = 1
				-- Percent of screen to increment by when resizing panes
				delta   = 3/100
				-- proportion of screen occupied by master pane
				ratio   = 2/3

		-- Applies a layout mirrored.
		mirror base a = reflectHoriz $ a $ reflectHoriz base

myTheme = defaultTheme
	{ activeColor         = blue
	, inactiveColor       = grey
	, activeBorderColor   = blue
	, inactiveBorderColor = grey
	, activeTextColor     = "white"
	, inactiveTextColor   = "black"
	, decoHeight          = 12
	}
	where
		blue = "#4a708b" -- same color used by pager
		grey = "#cccccc"


myChat' l = reflectHoriz $ withIM size roster $ reflectHoriz l
    where
        -- Ratio of screen roster will occupy
        size = 1/5
        -- Match roster window
        roster = Title "Buddy List"
myChat = myChat' Grid

myCode = limitWindows 3 $ magnifiercz' 1.4 $ FixedColumn 1 20 90 10

------------------------------------------------------------------------
-- Colors and borders
-- Currently based on the ir_black theme.
--
myNormalBorderColor = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"


-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = defaultTheme {
    activeBorderColor = "#7C7C7C",
    activeTextColor = "#000000",
    activeColor = "#FF0000",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
}

-- Width of the window border in pixels.
myBorderWidth = 3

------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myTerminal ="gnome-terminal --hide-menubar"
myModMask=mod4Mask

-- main = withConnection Session $ \ dbus -> do
--   getWellKnownName dbus
main = xmonad $ gnomeConfig
        {
        XMonad.workspaces = myWorkSpaces,
        terminal = myTerminal,
        normalBorderColor = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        modMask = myModMask, -- set the mod key to the windows key
        --logHook = dynamicLogWithPP (myPrettyPrinter dbus),
        layoutHook = myLayout,
        manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
        }
        `additionalKeysP` 
                 [ ("M-<Backspace>", withFocused hide) -- N.B. this is an absurd thing to do
                 , ("M-<Left>",    prevWS )
                 , ("M-<Right>",   nextWS )
                 , ("M-S-<Left>",  shiftToPrev )
                 , ("M-S-<Right>", shiftToNext )
                 , ("M-<Up>",   planeMove GConf Finite ToUp )
                 , ("M-<Down>",   planeMove GConf Finite ToDown )
                 , ("M-S-<Up>",  planeShift GConf Finite ToUp )
                 , ("M-S-<Down>",  planeShift GConf Finite ToDown )
                 ,("M-m",shellPrompt defaultXPConfig)
                 ]
    `removeKeysP`     ["M-p"]

--- Combine it all together
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
--defaults = defaultConfig {
    -- key bindings
   -- keys = myKeys,
   -- mouseBindings = myMouseBindings,
 
    -- hooks, layouts
    -- layoutHook = smartBorders $ myLayout,
--}



-- myManageHooks = composeAll
-- -- Allows focusing other monitors without killing the fullscreen
-- --  [ isFullscreen --> (doF W.focusDown <+> doFullFloat)
--  
-- -- Single monitor setups, or if the previous hook doesn't work
--     [ isFullscreen --> doFullFloat
--     -- skipped
--     ]
--
--

--myPrettyPrinter :: Connection -> PP
--myPrettyPrinter dbus = defaultPP {
--    ppOutput  = outputThroughDBus dbus
--  , ppTitle   = pangoColor "#00FF00" . shorten 50 . pangoSanitize
--  , ppCurrent = pangoColor "#FF0000" . wrap "[" "]" . pangoSanitize
--  , ppVisible = pangoColor "#663366" . wrap "(" ")" . pangoSanitize
--  , ppHidden  = wrap " " " "
--  , ppUrgent  = pangoColor "red"
--  }
--
--
--data RDesk = RDesk
--
--instance XPrompt RDesk where
--  showXPrompt     RDesk = "Remote desktop to:"
--  commandToComplete _ c = c
--  nextCompletion      _ = getNextCompletion
--
--
---- This retry is really awkward, but sometimes DBus won't let us get our
---- name unless we retry a couple times.
--getWellKnownName :: Connection -> IO ()
--getWellKnownName dbus = tryGetName `catchDyn` (\ (DBus.Error _ _) ->
--                                                getWellKnownName dbus)
-- where
--  tryGetName = do
--    namereq <- newMethodCall serviceDBus pathDBus interfaceDBus "RequestName"
--    addArgs namereq [String "org.xmonad.Log", Word32 5]
--    sendWithReplyAndBlock dbus namereq 0
--    return ()
--
--outputThroughDBus :: Connection -> String -> IO ()
--outputThroughDBus dbus str = do
--  let str' = "<span font=\"Terminus 9 Bold\">" ++ str ++ "</span>"
--  msg <- newSignal "/org/xmonad/Log" "org.xmonad.Log" "Update"
--  addArgs msg [String str']
--  send dbus msg 0 `catchDyn` (\ (DBus.Error _ _ ) -> return 0)
--  return ()
--
--pangoColor :: String -> String -> String
--pangoColor fg = wrap left right
-- where
--  left  = "<span foreground=\"" ++ fg ++ "\">"
--  right = "</span>"
--
--pangoSanitize :: String -> String
--pangoSanitize = foldr sanitize ""
-- where
--  sanitize '>'  acc = "&gt;" ++ acc
--  sanitize '<'  acc = "&lt;" ++ acc
--  sanitize '\"' acc = "&quot;" ++ acc
--  sanitize '&'  acc = "&amp;" ++ acc
--  sanitize x    acc = x:acc
--
--startNitrogen :: IO ()
--startNitrogen = do
--  threadDelay (5 * 1000 * 1000)
--  try_ $ rawSystem "nitrogen" ["--restore"]
--
--try_ :: MonadIO m => IO a -> m ()
--try_ action = liftIO $ try action >> return ()
--
