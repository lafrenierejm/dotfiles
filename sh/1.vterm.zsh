if [[ "$INSIDE_EMACS" == "vterm" ]]; then
	# https://github.com/akermu/emacs-libvterm#vterm-buffer-name-string
	autoload -U add-zsh-hook
	add-zsh-hook -Uz chpwd () {
		ROOT_DIR="$(git rev-parse --show-toplevel 2>/dev/null)"
		if [ -n "$ROOT_DIR" ]; then
			CWD="$(git rev-parse --show-prefix)"
			if [ -z "$CWD" ]; then
				TITLE="$(print -Pn '%1d')"
			else
				TITLE="$(basename $ROOT_DIR)/$CWD"
			fi
		else
			TITLE="$(print -Pn '%~')"
		fi
		TITLE="${TITLE%/}"
		export TITLE
		print -Pn "\e]2;$TITLE\a"
	}

	# https://github.com/akermu/emacs-libvterm#directory-tracking-and-prompt-tracking
	vterm_prompt_end() {
		vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
	}
	setopt PROMPT_SUBST
	PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
fi
