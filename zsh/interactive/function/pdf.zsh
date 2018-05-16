#!/bin/env zsh
#
# open PDFs in mupdf
#

if [[ -n "${DISPLAY}" ]] && hash 'mupdf' >/dev/null 2>&1; then
	pdf () {
		{
			mupdf "$@" >/dev/null 2>&1 &
			disown
		} >/dev/null
	}
fi
