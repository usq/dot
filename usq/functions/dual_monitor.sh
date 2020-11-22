#!/bin/bash


# RESOLUTION SETTINGS
# This sets your VGA1 monitor to its best resolution.
xrandr --output DP-1 --mode 1920x1080
# This sets your laptop monitor to its best resolution.
xrandr --output HDMI-0 --mode 2560x1440 --dpi 90

# MONITOR ORDER
xrandr --output HDMI-0 --right-of DP-1

# PRIMARY MONITOR
xrandr --output HDMI-0 --primary
