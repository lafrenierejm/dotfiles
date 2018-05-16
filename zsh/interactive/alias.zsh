# Create command aliases
# This file gets sourced from zshrc
# ##############################################################################

unalias -m '*' # remove all current aliases

alias dotfile='git --git-dir="${HOME}/.dotfiles" --work-tree="${HOME}"'
alias clipboard='xclip -selection clipboard'
alias cp='cp -r' # copy directories recursively
alias la='ls -hlAF --color=auto' # human sizes, amost-all, colored
alias less='less -i' # case sensitive search only when term contains a capital
alias ls='ls --color=auto' # colored
alias mbsync='mbsync -a' # sync all channels
alias mkdir='mkdir -p' # create parent directories as needed
alias mutt='cd "${HOME}/download"; mutt'
alias rmrf='rm -rf' # recurse, force
alias scp='scp -r' # copy directories recursively
alias sprunge='curl -F "sprunge=<-" http://sprunge.us' # send to sprunge.us
alias termbin='nc termbin.com 9999' # send to termbin.com
alias extract='dtrx' # do the right extraction
