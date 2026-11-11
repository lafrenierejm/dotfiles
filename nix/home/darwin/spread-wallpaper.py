#!/usr/bin/env python
"""Copy the desktop wallpaper that was just set to every Space.

`desktoppr` (like `NSWorkspace.setDesktopImageURL`) only updates some entries of
macOS's wallpaper store: always the current Space's, and usually `SystemDefault`
and the top-level `Displays` -- but not reliably.  Every other Space keeps its own
`Desktop` entry, so copy the updated entry over all of them.

`desktoppr` also writes the store asynchronously, so first wait until any entry
references IMAGE and use the most recently set one as the source.  (Waiting on one
specific entry, like `SystemDefault`, can time out even though the image was set.)

Usage: spread-wallpaper.py PATH-TO-Index.plist IMAGE
"""

import asyncio
import copy
import plistlib
import sys
import time
from collections.abc import Generator
from pathlib import Path
from typing import Any

# The store is untyped plist data, so these are only as precise as `plistlib` allows.
type Desktop = dict[str, Any]
type Index = dict[str, Any]

TIMEOUT: float = 15  # seconds


def desktops(index: Index) -> Generator[Desktop, None, None]:
    """Yield every `Desktop` dict in the store."""
    yield index["SystemDefault"]["Desktop"]
    for display in index.get("Displays", {}).values():
        yield display["Desktop"]
    for space in index.get("Spaces", {}).values():
        yield space["Default"]["Desktop"]
        for display in space.get("Displays", {}).values():
            yield display["Desktop"]


def references(desktop: Desktop, image: str) -> bool:
    return any(
        image.encode() in choice["Configuration"]
        for choice in desktop["Content"]["Choices"]
    )


async def load(path: Path, image: str) -> tuple[Index, Desktop]:
    """Return the store and its newest entry referencing IMAGE."""
    deadline = time.monotonic() + TIMEOUT
    while True:
        try:
            with path.open("rb") as f:
                index = plistlib.load(f)
            matches = [d for d in desktops(index) if references(d, image)]
            if matches:
                return index, max(matches, key=lambda d: d["LastSet"])
        except (plistlib.InvalidFileException, OSError, KeyError):
            pass  # the store is mid-write
        if time.monotonic() >= deadline:
            sys.exit(f"timed out waiting for {path} to reference {image}")
        await asyncio.sleep(0.25)


async def main(path: Path, image: str) -> None:
    index, source = await load(path, image)

    source = copy.deepcopy(source)
    index["SystemDefault"]["Desktop"] = copy.deepcopy(source)
    for display in index.get("Displays", {}).values():
        display["Desktop"] = copy.deepcopy(source)
    for space in index.get("Spaces", {}).values():
        space["Default"]["Desktop"] = copy.deepcopy(source)
        for display in space.get("Displays", {}).values():
            display["Desktop"] = copy.deepcopy(source)

    tmp = path.with_name(path.name + ".tmp")
    with tmp.open("wb") as f:
        plistlib.dump(index, f, fmt=plistlib.FMT_BINARY)
    tmp.replace(path)


if __name__ == "__main__":
    asyncio.run(main(Path(sys.argv[1]), sys.argv[2]))
