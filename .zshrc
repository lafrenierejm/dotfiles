# This is the main user configuration file
# It is sourced when zsh is started as an interactive shell
# ##############################################################################

# always run inside tmux
## attempt to attach if over SSH
if hash 'tmux' >/dev/null 2>&1; then
    if [[ -n "${SSH_CLIENT}" ]] && [[ -z "${TMUX}" ]]; then
	    tmux attach || tmux new-session
    ## else replace shell with tmux session
    elif [[ -z "${TMUX}" ]]; then
	    if [[ -f "${HOME}/.config/base16-shell/scripts/base16-monokai.sh" ]] &&
			    [[ -s "${HOME}/.config/base16-shell/profile_helper.sh" ]]; then
		    source "${HOME}/.config/base16-shell/scripts/base16-monokai.sh"
		    eval "$(${HOME}/.config/base16-shell/profile_helper.sh)"
	    fi
	    exec tmux
    fi
fi

# Automatically cache ssh keys
SSH_AUTH_SOCK="${HOME}/.ssh/auth-socket"
function start_ssh_agent {
	ssh-agent | sed 's/^echo/#echo/' > "${SSH_AUTH_SOCK}"
	chmod 600 "${SSH_AUTH_SOCK}"
	. "${SSH_AUTH_SOCK}"
	ssh-add
}
if [ -f "${SSH_AUTH_SOCK}" ]; then
	. "${SSH_AUTH_SOCK}" >/dev/null
	ps --pid "${SSH_AGENT_PID}" >/dev/null || start_ssh_agent
else
	start_ssh_agent
fi

# Use vi mode
bindkey -v
## Set a 1ms delay on pressing ESC
export KEYTIMEOUT=1

# Enable autocompletion
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*' group-name
autoload -U compinit
compinit

# History settings
mkdir -p "${HOME}/.local/share/zsh"
HISTFILE="${HOME}/.local/share/zsh/history.txt"
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory nomatch
## Ignore duplicate lines in the history
setopt HIST_IGNORE_DUPS
unsetopt autocd beep extendedglob notify

# Source external files
if [ -d "${XDG_CONFIG_HOME}/zsh/interactive" ]; then
	for file in ${XDG_CONFIG_HOME}/zsh/interactive/**/*.zsh; do
		source "${file}"
	done
elif [ -d "${HOME}/.config/zsh/interactive" ]; then
	for file in ${HOME}/.config/zsh/interactive/**/*.zsh; do
		source "${file}"
	done
fi

# version control system
autoload -Uz vcs_info
precmd_vcs_info() {
	vcs_info
}
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
zstyle ':vcs_info:git*' formats "%b" # display branch name
zstyle ':vcs_info:hg*' formats "%b" # display branch name

# Set the left-aligned prompt
## Display the current directory, shortening $HOME to '~'
## Display '#' if root, '%' otherwise
PROMPT='%m:%~%# '
# set the right-aligned prompt
## display version control information
RPROMPT='${vcs_info_msg_0_}'
