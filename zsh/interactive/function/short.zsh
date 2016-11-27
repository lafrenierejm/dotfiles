#!/bin/env zsh
#
# shorten a URL with https://0x0.st/
#

if hash 'curl' >/dev/null 2>&1; then
	short () {
		curl -F"shorten=$@" 'https://0x0.st/'
	}
fi
