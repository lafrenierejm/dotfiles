if command -v pyenv >/dev/null; then
	export PYENV_ROOT="$HOME/.pyenv"
	eval "$(pyenv init -)"
	directory="$PYENV_ROOT/shims"
	mkdir -p "$directory"
	export PATH="$directory:$PATH"
fi
