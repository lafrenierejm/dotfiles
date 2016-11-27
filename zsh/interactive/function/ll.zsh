#!/bin/env zsh
#
# `ls` with human-readable size, long format, filetype, and color
#

ll () {
	if [[ "$(pwd)" == "${$(pwd)%*config*}" ]] &&
	   [[  "$(pwd)" == "${$(pwd)%*dotfile*}" ]]; then
		ls -hlF --color=auto "$@"
	else
		# print almost-all in config or dotfile directory
		ls -hlAF --color=auto "$@"
	fi
}
