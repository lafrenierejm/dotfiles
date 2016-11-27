#!/bin/env zsh
#
# open images in feh
#

if [[ -n "${DISPLAY}" ]] && hash 'feh' >/dev/null 2>&1; then
	img () {
		{
			feh \
				--scale-down \  # scale image to fit window
				"$@" \          # run on all arguments
				>/dev/null 2>&1 # do not display stdout or sterr
			disown
		} >/dev/null # hide PID
	}
fi
