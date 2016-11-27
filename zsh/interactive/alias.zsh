# Create command aliases
# This file gets sourced from zshrc
# ##############################################################################

unalias -m '*' # remove all current aliases

alias lla='ls -hlAF --color=auto' # human sizes, amost-all, colored
alias mkdir='mkdir -p' # create parent directories as needed
alias rmrf='rm -rf' # recurse, force
alias termbin='nc termbin.com 9999' # send to termbin.com

# `mail` opens mutt in download directory
if hash 'mutt' >/dev/null 2>&1; then
	mail () {
		# Attempt to get the directory from xdg-user-dir
		if hash 'xdg-user-dir' >/dev/null 2>&1; then
			directory=$(xdg-user-dir DOWNLOAD)
		elif [[ -d "${HOME}/download" ]]; then
			directory="${HOME}/download"
		# Attempt to find a download dir in $HOME
		else
			directory=$(find "${HOME}" -maxdepth 1 -path "${HOME}/[Dd]ownload*")
		fi

		# cd to $directory if it was found
		if [[ -n "${directory}" ]]; then
			cd "${directory}"
		# Otherwise cd to $HOME
		else
			cd "${HOME}"
		fi

		# Run mutt
		mutt -F "${HOME}/.config/mutt/muttrc"
	}
fi
