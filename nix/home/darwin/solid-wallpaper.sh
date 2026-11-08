#!/usr/bin/env bash
# Set a solid desktop color matching the system appearance on every Space.
#
# Expects these environment variables:
#   IMAGE_DARK, IMAGE_LIGHT  1x1 images of the dark and light mode colors
#   SPREAD_WALLPAPER         path to `spread-wallpaper.py`
set -euo pipefail

if [ "$(/usr/bin/defaults read -g AppleInterfaceStyle 2>/dev/null)" = Dark ]; then
	image=$IMAGE_DARK
else
	image=$IMAGE_LIGHT
fi

echo "$(date -Is) style=$(/usr/bin/defaults read -g AppleInterfaceStyle 2>/dev/null || echo Light) image=$image"
# `desktoppr` rejects scale and path in a single invocation.
desktoppr scale fill
desktoppr "$image"

# `desktoppr` only updates the current Space, so copy its result to the others in
# the wallpaper store, then restart the agent that reads it.
# The script waits until the store references `$image` since `desktoppr` writes it asynchronously.
index="$HOME/Library/Application Support/com.apple.wallpaper/Store/Index.plist"
python3 -I "$SPREAD_WALLPAPER" "$index" "$image"
/usr/bin/killall WallpaperAgent || true
echo "$(date -Is) done; desktoppr reports $(desktoppr)"
