#!/usr/bin/env bash
# Set a solid desktop color matching the system appearance on every Space.
#
# Expects these environment variables:
#   COLOR_DARK, COLOR_LIGHT  hex colors (RRGGBB) for dark and light mode
#   SPREAD_WALLPAPER         path to `spread-wallpaper.py`

if [ "$(/usr/bin/defaults read -g AppleInterfaceStyle 2>/dev/null)" = Dark ]; then
	color=$COLOR_DARK
else
	color=$COLOR_LIGHT
fi

start=$(date +%s)
desktoppr color "$color"

# `desktoppr` only updates the current Space, so copy its result to the others in
# the wallpaper store, then restart the agent that reads it.
index="$HOME/Library/Application Support/com.apple.wallpaper/Store/Index.plist"
# Wait (up to 5s) for the store to be written.
for _ in $(seq 10); do
	[ "$(stat -c %Y "$index")" -ge "$start" ] && break
	sleep 0.5
done
python3 -I "$SPREAD_WALLPAPER" "$index"
/usr/bin/killall WallpaperAgent || true
