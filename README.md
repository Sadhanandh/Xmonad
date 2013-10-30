##My Xmonad conf:

modm -> Alt
mod4Mask -> Win Key

If you are using xephyr->

Cntl(R) + Shift -> To UnCapture input.
Alt + Shift + Cntl(R) -> To Capture input 

Add this to your bashrc:
`
alias xphr="Xephyr -ac -br -noreset -host-cursor -screen 1366x764 -fullscreen -dpi 100 :1 &sleep 1 ;DISPLAY=:1;feh --bg-fill ${HOME}/.xmonad/waterfall.jpg;xmonad &" 
`
(Change the image location)
Also This:
`
alias syncx="${HOME}/.xmonad/syncx :0 :1"
`
(Use syncx to sync Xephyr clipboard)

Or if otherwise paste xmonad.desktop (after altering the username) in
"/usr/share/xsessions/"

Or 
`
xinit -- :1
`
Then execute
`
~/.xmonad/xintrc
`

##Required:
`
sudo apt-get install xserver-xephyr xmonad suckless-tools xmobar trayer pcmanfm xfe lxappearance ttf-liberation  xfce4-mixer gmrun feh scrot nitrogen 
`

##Optional:

`
sudo apt-get install zim artha terminator indicator-keylock synapse guake xautolock xrandr indicator-remindor xfce4-volumed xfce4-power-manager pulseaudio thunar xfce4-terminal
`

##Common KeyBindings:

Win + space : change Layout
Alt + Enter : Bring to Master
Alt + h : Shrink Size
Alt + l : Expand Size
Alt + a : Increase Size
Alt + z : Pop Out (Tiled)
Alt + b : Toggle Bar
Alt + g : Toggle Border
Alt + Arrow : Focus

Win + m : Minimize
Win + M : UnMinimize

Alt + p : Run
Alt + Shift + Enter : Terminal

Alt + Ctl + Arrow : Move Workspace
Alt + Shift + Ctl + Arrow : Move Windows to different workspace

For more shortcuts refer xmonad.hs