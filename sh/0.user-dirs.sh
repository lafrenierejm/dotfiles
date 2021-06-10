# Export all of the XDG user dirs as shell variables.
# XDG_CONFIG_HOME should already be exported
if [ ! -z "$XDG_CONFIG_HOME" ]; then
    USER_DIRS="$XDG_CONFIG_HOME"/user-dirs.dirs
    if [[ -f "$USER_DIRS" ]]; then
	while read line; do
	    line="$(sed -e "s|\$HOME|$HOME|" -e 's|"||g' <<<$line)"
	    export "$line"
	done <"$USER_DIRS"
    fi
fi
