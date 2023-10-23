#!/usr/bin/env sh
directory="$HOME/.volta/bin"
if [ -d "$directory" ]; then
	export PATH="$directory:$PATH"
fi
