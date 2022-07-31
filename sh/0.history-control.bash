# From https://github.com/CeleritasCelery/emacs-native-shell-complete#bash:
# > If the HISTCONTROL environment variable is not set to ignorespace or
# > ignoreboth you will get a lot of garbage in your shell history. We also need
# > to disable bracketed-paste.
export HISTCONTROL=ignoreboth
bind 'set enable-bracketed-paste off'
