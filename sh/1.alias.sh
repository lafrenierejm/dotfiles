if command -v exa 1>/dev/null 2>&1; then
    alias ll='exa --long --git --time-style=long-iso'
    alias la='exa --long --git --time-style=long-iso --all'
else
    alias ll='ls -lsh'
    alias la='ls -Alsh'
fi
