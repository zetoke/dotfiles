#!/bin/sh

# wall
feh --bg-scale /mnt/data/Images/wallpaper-474320_fxd.png &

# Trayer icon tray -> try different heights
#trayer --edge top --align right --SetDockType true --SetPartialStrut true \
#--expand true --width 5 --transparent true --tint 0x000000 --alpha 0 --distance 0 --padding 0 --heighttype pixel --height 16 &

# cursor
xsetroot -cursor_name left_ptr &

# nvidia settings
nvidia-settings --load-config-only &

# switch keyboard layout
xxkb &

# apps

# WM
exec xmonad > $HOME/.xmonad.log 2>&1
