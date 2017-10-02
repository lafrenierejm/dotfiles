# This file is sourced at the beginning of initial progress when zsh is started
# as a login shell
# #############################################################################

# Add unique values to $PATH
typeset -U path
if [[ -d ${HOME}/.local/bin ]]; then
	path=(${HOME}/.local/bin $path)
fi

# Do things if in tty
current_vt=$(tty) # Get the current virtual terminal
if [[ ${current_vt#*/dev/tty} != "${current_vt}" ]]; then
	# Set font to terminus size 14 if font is installed
	if [[ -f /usr/share/kbd/consolefonts/ter-114n.psf.gz ]]; then
		setfont ter-114n -m 8859-2
	fi
	# Start X if in tty1
	if [ ${current_vt: -1} -eq 1 ]; then
		startx
	fi
fi