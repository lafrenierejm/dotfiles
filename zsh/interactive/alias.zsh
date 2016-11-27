# Create command aliases
# This file gets sourced from zshrc
# ##############################################################################

unalias -m '*' # remove all current aliases

alias la='ls -hlAF --color=auto' # human sizes, amost-all, colored
alias mkdir='mkdir -p' # create parent directories as needed
alias rmrf='rm -rf' # recurse, force
alias termbin='nc termbin.com 9999' # send to termbin.com
