# Customize prompts
setopt prompt_subst
chpwd () {
	zshrc_git_root="$(git rev-parse --show-toplevel 2>/dev/null)"
	if [ -z "$zshrc_git_root" ]; then
		zshrc_prompt_short_dir='%~'
	else
		zshrc_git_path="$(git rev-parse --show-prefix)"
		zshrc_prompt_path="${${zshrc_git_root%/}##*/}/${zshrc_git_path}"
		zshrc_prompt_short_dir="${zshrc_prompt_path%/}"
	fi

	# https://github.com/akermu/emacs-libvterm#vterm-buffer-name-string
	export TITLE="$zshrc_prompt_short_dir"
	if [[ "$INSIDE_EMACS" == "vterm" ]]; then
	    print -Pn "\e]0;%n@%m:${zshrc_prompt_short_dir}\a"
	fi
}

# Set the left-aligned prompt
## Display the current directory, shortening $HOME to '~'
## Display '#' if root, '%' otherwise
chpwd
PROMPT='%n@%m:${zshrc_prompt_short_dir} %# '

# set the right-aligned prompt
## display version control information
RPROMPT=''
