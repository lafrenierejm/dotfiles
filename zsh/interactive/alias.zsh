# Create command aliases
# This file gets sourced from zshrc
# ##############################################################################

# Remove all current aliases
unalias -m '*'

# Create parent directories as needed
alias mkdir='mkdir -p'

# Remove recursively and forcefully
alias rmrf='rm -rf'

# `img` opens images in feh when inside graphical environment
if [[ -n "${DISPLAY}" ]] && hash 'feh' >/dev/null 2>&1; then
	img () {
		{
			feh -. "$@" >/dev/null 2>&1 &
			# '-.' scales images to fit the window
			disown
		} >/dev/null
	}
fi

# `lla` is `ls` with human-readable size, long, almost all, filetype, colored
alias lla='ls -hlAF --color=auto'

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

# `termbin` sends information to termbin.com
if hash 'nc' >/dev/null 2>&1; then
	alias termbin='nc termbin.com 9999'
fi
