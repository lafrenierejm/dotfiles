#!/usr/bin/env python
"""Copy the desktop wallpaper that was just set to every Space.

`desktoppr` (like `NSWorkspace.setDesktopImageURL`) only updates `SystemDefault`,
the top-level `Displays`, and the entries of the current Space in macOS's
wallpaper store.  Every other Space keeps its own `Desktop` entry, so copy
`SystemDefault`'s over them.

Usage: spread-wallpaper.py PATH-TO-Index.plist
"""

import copy
import os
import plistlib
import sys


def main(path):
    with open(path, "rb") as f:
        index = plistlib.load(f)

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
    main(sys.argv[1])
