#!/bin/env zsh
#
# Open files in mplayer
#

if [[ -n "${DISPLAY}" ]] && hash 'mplayer' >/dev/null 2>&1; then
	vid () {
		mplayer \
			-volume 0 \ # mute
			-zoom \     # fit to window
			"$@" >/dev/null 2>&1 &
		disown
	} >/dev/null
fi
