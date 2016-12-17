# When in normal or replace modes, use a solid block as the cursor
# When in normal or replace modes, use a vertical bar as the cursor
# ##############################################################################

zle-keymap-select () {
	if [[ "${TERM}" != 'linux' ]]; then
		if [[ -n "${TMUX}" ]]; then
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
		else
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
	fi
	zle reset-prompt
	zle -R
}

zle-line-init () {
	if [[ "${TERM}" != 'linux' ]]; then
		if [[ -n "${TMUX}" ]]; then
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
		else
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
	fi
	zle reset-prompt
	zle -R
}

function zle-line-finish {
	if [[ "${TERM}" != 'linux' ]]; then
		if [[ -n "${TMUX}" ]]; then
			echo -ne "\033Ptmux;\033\033[2 q\033\\" # Block cursor
		else
			echo -ne "\033[2 q" # Block cursor
		fi
	fi
}

zle -N zle-keymap-select
zle -N zle-line-init
zle -N zle-line-finish
