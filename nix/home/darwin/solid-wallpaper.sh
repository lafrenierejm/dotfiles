#!/usr/bin/env bash
# Set a solid desktop color matching the system appearance on every Space.
#
# Expects these environment variables:
#   IMAGE_DARK, IMAGE_LIGHT  1x1 images of the dark and light mode colors
set -euo pipefail

if [ "$(/usr/bin/defaults read -g AppleInterfaceStyle 2>/dev/null)" = Dark ]; then
	image=$IMAGE_DARK
else
	image=$IMAGE_LIGHT
fi

echo "$(date -Is) style=$(/usr/bin/defaults read -g AppleInterfaceStyle 2>/dev/null || echo Light) image=$image"
desktoppr scale fill
# `--all-spaces` also copies the new picture to every other Space in the wallpaper store.
# It waits until the store references `$image`, since macOS writes the store asynchronously.
desktoppr picture --all-spaces "$image"

# Restart the agent that reads the store so it picks up the change.
/usr/bin/killall WallpaperAgent || true
echo "$(date -Is) done; desktoppr reports $(desktoppr picture)"
