export PS1='%n@%m %1~ %# '
if [ -z "$INSIDE_EMACS" ]; then
    export PS2='%_>'
else
    export PS2=''
fi
