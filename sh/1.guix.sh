# Add Guix profile to PATH

GUIX_PROFILE="$XDG_CONFIG_HOME"/guix/current
if [[ -f "$GUIX_PROFILE" ]]; then
    export GUIX_PROFILE
    PATH="$GUIX_PROFILE/bin:$PATH"
    hash guix
fi
