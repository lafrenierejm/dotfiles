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

# Copyright: (C) 2017-2020 by Lukas Fürmetz & Contributors
# URL: https://github.com/akermu/emacs-libvterm#vterm-buffer-name-string
# License: GPL-3.0
autoload -U add-zsh-hook
add-zsh-hook -Uz chpwd () {
	if [[ -n "$INSIDE_EMACS" ]]; then
		export TITLE="$(short_pwd)"
		print -Pn "\e]0;${TITLE}\a"
	fi
}

setopt PROMPT_SUBST
PROMPT='$(short_pwd) %(!.#.$) '
RPROMPT=''
