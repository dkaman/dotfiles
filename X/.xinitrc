#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

#start awesome WM with dual monitor setup in normal mode
exec awesome
xrandr --output HDMI3 --auto
xrandr --output HDMI1 --auto --right-of HDMI3

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice
