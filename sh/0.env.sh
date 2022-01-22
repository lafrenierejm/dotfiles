# Set generic environment variables

if [ -n "$INSIDE_EMACS" ]; then
    export PAGER='cat'
    export EDITOR="emacsclient --alternate-editor='' --no-wait"
    export VISUAL="$EDITOR"
else
    export EDITOR='vi'
    export VISUAL="$EDITOR"
fi
