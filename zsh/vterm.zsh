# Copyright (C): 2017-2020 by Lukas Fürmetz & Contributors
# URL: https://github.com/akermu/emacs-libvterm
# License: GPL-3.0

# https://github.com/akermu/emacs-libvterm#shell-side-configuration
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

short_pwd() {
	zshrc_git_root="$(git rev-parse --show-toplevel 2>/dev/null)"
	if [ -z "$zshrc_git_root" ]; then
		zshrc_prompt_short_dir='%~'
	else
		zshrc_git_path="$(git rev-parse --show-prefix)"
		zshrc_prompt_path="${${zshrc_git_root%/}##*/}/${zshrc_git_path}"
		zshrc_prompt_short_dir="${zshrc_prompt_path%/}"
	fi
	printf "%s\n" "$zshrc_prompt_short_dir"
}

# https://github.com/akermu/emacs-libvterm#vterm-buffer-name-string
autoload -U add-zsh-hook
add-zsh-hook -Uz chpwd () {
	export TITLE="$(short_pwd)"
	if [[ "$INSIDE_EMACS" == "vterm" ]]; then
	    print -Pn "\e]0;${TITLE}\a"
	fi
}

# https://github.com/akermu/emacs-libvterm#directory-tracking-and-prompt-tracking
vterm_prompt_end() {
    vterm_printf "51;A$(pwd)"
}
setopt PROMPT_SUBST
PROMPT='$(short_pwd) %# %{$(vterm_prompt_end)%}'
RPROMPT=''