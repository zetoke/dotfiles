#!/bin/sh

# wall
feh --bg-scale ~/Data/Images/wallpaper-474320_fxd.png &

# Trayer icon tray -> try different heights
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
--expand true --width 5 --transparent true --tint 0x000000 --alpha 0 --distance 0 --padding 0 --heighttype pixel --height 16 &

# cursor
xsetroot -cursor_name left_ptr &

# loading some fonts
xset +fp /usr/share/fonts/local
xset fp rehash

# nvidia settings
#nvidia-settings --load-config-only &

# switch keyboard layout
setxkbmap -layout 'us,ru' -option 'grp:caps_toggle,grp_led:scroll' &
xxkb &

# WM
exec xmonad > $HOME/.xmonad.log 2>&1
#openbox
#awesome
