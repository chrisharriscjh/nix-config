import XMonad
import XMonad.Actions.CycleWS
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.Paste
import System.IO

main = do
  --h <- spawnPipe "xmobar -d"
  xmonad $ docks def {
    terminal = "alacritty"
    , manageHook = manageDocks <+> myManageHook <+> manageHook def
    , modMask = mod4Mask
    , borderWidth = 1
    , focusedBorderColor = "Dark Blue"
    , workspaces = myWorkspaces
    , layoutHook = myLayoutHook
    , logHook = myLogHook
    } `additionalKeys` myAdditionalKeys

--toggleStrutsKey XConfig { XMonad.modMask = modMask } = (modMask, xK_b)

myWorkspaces = ["Sys", "Msg", "1", "2", "3", "4", "Log"]
workSpaceShortcuts = [xK_y, xK_u, xK_i, xK_o, xK_p, xK_bracketleft, xK_bracketright]

myAdditionalKeys = 
  [ ((mod4Mask, xK_w), spawn "qutebrowser")
  , ((mod4Mask, xK_s), spawn ("rofi-pass"))
  , ((mod4Mask, xK_d), spawn ("rofi -modi drun,ssh,window -show drun -show-icons"))
  , ((mod4Mask, xK_f), spawn ("popupCommands"))
  , ((mod4Mask, xK_g), spawn ("alacritty -e nvim -c ':terminal'"))
  , ((mod4Mask, xK_c), spawn ("popupStatus"))
  , ((mod4Mask, xK_comma), sendMessage Shrink)
  , ((mod4Mask, xK_period), sendMessage Expand)
  , ((mod4Mask, xK_h), prevWS)
  , ((mod4Mask, xK_l), nextWS)
  , ((mod4Mask, xK_v ), kill)
  ] ++ 
  [((m .|. mod4Mask, k), windows $ f i)
    | (i, k) <- zip myWorkspaces workSpaceShortcuts     
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myLayoutHook = avoidStruts $ spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $ layoutHook def

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
  where fadeAmount = 0.90

--myManageHook = isDialog --> doF W.shiftMaster <+> doF W.swapDown
--myManageHook = composeAll []
myManageHook = composeOne
  [ return True -?> doF W.swapDown
  ]
