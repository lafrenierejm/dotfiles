# When in normal or replace modes, use a solid block as the cursor
# When in normal or replace modes, use a vertical bar as the cursor
# ##############################################################################

function zle-keymap-select zle-line-init {
	# If running in TMUX on top of an rxvt-based emulator
	if [ -n "${TMUX}" ] && [ "${TERMINAL#*rxvt}" != "${TERMINAL}" ]; then
		case "${KEYMAP}" in
			'viins'|'main')
				echo -ne "\033Ptmux;\033\033[6 q\033\\" # Line cursor
				;;
			'vicmd')
				echo -ne "\033Ptmux;\033\033[2 q\033\\" # Block cursor
				;;
			*)
				printf "KEYMAP = %s\n" "${KEYMAP}" # Print the unkown $KEYMAP
				echo -ne "\033Ptmux;\033\033[6 q\033\\" # Line cursor
				;;
		esac
	# If running in an rxvt-based emulator
	elif [ "${TERM#*rxvt}" != "${TERM}" ]; then
		case "${KEYMAP}" in
			'viins'|'main')
				echo -ne "\033[6 q" # Line cursor
				;;
			'vicmd')
				echo -ne "\033[2 q" # Block cursor
				;;
			*)
				printf "KEYMAP = %s\n" "${KEYMAP}" # Print the unkown $KEYMAP
				echo -ne "\033[2 q" # Block cursor
				;;
		esac
	fi
	zle reset-prompt
	zle -R
}
function zle-line-finish {
	# If running in TMUX on top of an rxvt-based emulator
	if [ -n "${TMUX}" ] && [ "${TERMINAL#*rxvt}" != "${TERMINAL}" ]; then
		echo -ne "\033Ptmux;\033\033[2 q\033\\" # Block cursor
	# If running in an rxvt-based emulator
	elif [ "${TERM#*rxvt}" != "${TERM}" ]; then
		echo -ne "\033[2 q" # Block cursor
	fi
}

zle -N zle-keymap-select
zle -N zle-line-init
zle -N zle-line-finish
