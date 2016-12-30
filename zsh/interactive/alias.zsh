# Create command aliases
# This file gets sourced from zshrc
# ##############################################################################

unalias -m '*' # remove all current aliases

alias cp='cp -r' # copy directories recursively
alias la='ls -hlAF --color=auto' # human sizes, amost-all, colored
alias less='less -i' # case sensitive search only when term contains a capital
alias ls='ls --color=auto' # colored
alias mbsync='mbsync -c "${HOME}/.config/isync/mbsyncrc" -a' # sync all channels
alias mkdir='mkdir -p' # create parent directories as needed
alias mutt='cd "${HOME}/download"; mutt -F "${HOME}/.config/mutt/muttrc"'
alias rmrf='rm -rf' # recurse, force
alias scp='scp -r' # copy directories recursively
alias termbin='nc termbin.com 9999' # send to termbin.com
