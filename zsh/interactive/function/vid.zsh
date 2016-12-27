#!/bin/env zsh
#
# Open files in mpv
#

if [[ -n "${DISPLAY}" ]] && hash 'mpv' >/dev/null 2>&1; then
	vid () {
		mpv \
			--mute=yes \
			"$@" \
			>/dev/null 2>&1 \
			&
		disown
	}
fi
