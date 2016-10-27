# Create command aliases
# This file gets sourced from zshrc
# ##############################################################################

alias up='cd ../'
alias mkdir='mkdir -p'
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

# `ll` is `ls` with human-readable size, long, filetype, colored
unalias ll
ll () {
	# Not in config if nothing is replaced
	if [[ "$(pwd)" == "${$(pwd)%${XDG_CONFIG_HOME}*}" ]] &&
	   [[ "$(pwd)" == "${$(pwd)%*dotfile*}" ]]; then
		ls -hlF --color=auto "$@"
	# Otherwise add '-A' option
	else
		ls -hlAF --color=auto "$@"
	fi
}

# `lla` is `ls` with human-readable size, long, almost all, filetype, colored
alias lla='ls -hlAF --color=auto'

# `mail` opens mutt in download directory
if hash 'mutt' >/dev/null 2>&1; then
	mail () {
		# Attempt to get the directory from xdg-user-dir
		if hash 'xdg-user-dir' >/dev/null 2>&1; then
			directory=$(xdg-user-dir DOWNLOAD)
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

# `pdf` opens PDFs in mupdf when inside graphical environment
if [[ -n "${DISPLAY}" ]] && hash 'mupdf' >/dev/null 2>&1; then
	pdf () {
		{
			mupdf "$@" >/dev/null 2>&1 &
			disown
		} >/dev/null
	}
fi

# `termbin` sends information to termbin.com
if hash 'nc' >/dev/null 2>&1; then
	alias termbin='nc termbin.com 9999'
fi

# `vid` opens videos in mplayer when inside graphical environment
## '-volume 0' mutes
## '-zoom' scales video to window
if [[ -n "${DISPLAY}" ]] && hash 'mplayer' >/dev/null 2>&1; then
	vid () {
		mplayer -volume 0 -zoom "$@" >/dev/null 2>&1 &
		disown
	} >/dev/null
fi
