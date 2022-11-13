if [ command -v emacs >/dev/null 2>&1 ]; then
    export EDITOR="emacsclient --alternate-editor='' --tty %s"
    export VISUAL="emacsclient --alternate-editor='' --create-frame %s"
fi
