#!/usr/bin/env python
"""Copy the desktop wallpaper that was just set to every Space.

`desktoppr` (like `NSWorkspace.setDesktopImageURL`) only updates `SystemDefault`,
the top-level `Displays`, and the entries of the current Space in macOS's
wallpaper store.  Every other Space keeps its own `Desktop` entry, so copy
`SystemDefault`'s over them.

`desktoppr` writes the store asynchronously, so first wait until `SystemDefault`
references IMAGE; otherwise a stale entry would be copied over every Space.

Usage: spread-wallpaper.py PATH-TO-Index.plist IMAGE
"""

import copy
import os
import plistlib
import sys
import time

TIMEOUT = 15  # seconds


def references(desktop, image):
    return any(
        image.encode() in choice["Configuration"]
        for choice in desktop["Content"]["Choices"]
    )


def load(path, image):
    deadline = time.monotonic() + TIMEOUT
    while True:
        try:
            with open(path, "rb") as f:
                index = plistlib.load(f)
            if references(index["SystemDefault"]["Desktop"], image):
                return index
        except (plistlib.InvalidFileException, OSError, KeyError):
            pass  # the store is mid-write
        if time.monotonic() >= deadline:
            sys.exit(f"timed out waiting for {path} to reference {image}")
        time.sleep(0.25)


def main(path, image):
    index = load(path, image)

    desktop = index["SystemDefault"]["Desktop"]
    for space in index.get("Spaces", {}).values():
        space["Default"]["Desktop"] = copy.deepcopy(desktop)
        for display in space.get("Displays", {}).values():
            display["Desktop"] = copy.deepcopy(desktop)

    tmp = path + ".tmp"
    with open(tmp, "wb") as f:
        plistlib.dump(index, f, fmt=plistlib.FMT_BINARY)
    os.replace(tmp, path)


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
