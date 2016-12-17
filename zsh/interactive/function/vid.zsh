#!/bin/env zsh
#
# Open files in mpv
#

if [[ -n "${DISPLAY}" ]] && hash 'mpv' >/dev/null 2>&1; then
	vid () {
		mpv --volume 0 "$@" >/dev/null 2>&1 &
		disown
	} >/dev/null
fi
