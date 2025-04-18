# Copyright: (C) 2017-2020 by Lukas Fürmetz & Contributors
# URL: https://github.com/akermu/emacs-libvterm#shell-side-configuration
# License: GPL-3.0
vterm_printf() {
	if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
		# Tell tmux to pass the escape sequences through
		printf "\ePtmux;\e\e]%s\007\e\\" "$1"
	elif [ "${TERM%%-*}" = "screen" ]; then
		# GNU screen (screen, screen-256color, screen-256color-bce)
		printf "\eP\e]%s\007\e\\" "$1"
	else
		printf "\e]%s\e\\" "$1"
	fi
}

# Copyright: (C) 2017-2020 by Lukas Fürmetz & Contributors
# URL: https://github.com/akermu/emacs-libvterm#directory-tracking-and-prompt-tracking
# License: GPL-3.0
vterm_prompt_end() {
	vterm_printf "51;A$(pwd)"
}

if [[ "$INSIDE_EMACS" == "vterm" ]]; then
	PROMPT+='%{$(vterm_prompt_end)%}'
fi
