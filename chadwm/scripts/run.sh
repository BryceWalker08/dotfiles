#!/bin/sh

xrdb merge ~/.Xresources 
xbacklight -set 10 &
feh --bg-scale ~/Pictures/wallpapers/mac.png &
xset r rate 200 50 &
picom --experimental-backends --config ~/.config/picom/picom.conf &
setxkbmap -option "ctrl:swapcaps"
if [ -f "$HOME/.Xmodmap" ]; then
	xmodmap "$HOME/.Xmodmap"
fi
xbindkeys &


dash ~/.config/chadwm/scripts/bar.sh &
while type chadwm >/dev/null; do chadwm && continue || break; done
