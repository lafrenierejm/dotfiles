if [[ "$INSIDE_EMACS" == "vterm" ]]; then
    # https://github.com/akermu/emacs-libvterm#vterm-buffer-name-string
    PROMPT_COMMAND='echo -ne "\033]0;${HOSTNAME}:${PWD}\007"'

    # https://github.com/akermu/emacs-libvterm#directory-tracking-and-prompt-tracking
    vterm_prompt_end(){
	vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
    }
    export PS1=$PS1'\[$(vterm_prompt_end)\]'
fi
