#!/bin/env zsh
#
# open images in feh
#

if [[ -n "${DISPLAY}" ]] && hash 'feh' >/dev/null 2>&1; then
	img () {
		feh \
			--scale-down \
			"$@" \
			>/dev/null 2>&1 \
			&
		disown
	}
fi
