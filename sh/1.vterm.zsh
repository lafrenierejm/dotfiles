if [[ "$INSIDE_EMACS" == "vterm" ]]; then
    # https://github.com/akermu/emacs-libvterm#vterm-buffer-name-string
    autoload -U add-zsh-hook
    add-zsh-hook -Uz chpwd (){ print -Pn "\e]2;%m:%2~\a" }

    # https://github.com/akermu/emacs-libvterm#directory-tracking-and-prompt-tracking
    vterm_prompt_end() {
	vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
    }
    setopt PROMPT_SUBST
    PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
fi
