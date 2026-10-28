#!/usr/bin/env bash

set -euo pipefail

# Set COSMIC's background to a solid color, following light/dark mode.
# Expects COSMIC_DIR to be COSMIC's config directory.
# Expects COLOR_LIGHT/COLOR_DARK to be RRGGBB hex colors.
cosmic=${COSMIC_DIR:?}

# COSMIC defaults to dark until it has written `is_dark`.
mode=$(cat "$cosmic/com.system76.CosmicTheme.Mode/v1/is_dark" 2>/dev/null || echo true)
if [ "$mode" = true ]; then
	hex=${COLOR_DARK:?}
else
	hex=${COLOR_LIGHT:?}
fi

dir="$cosmic"/com.system76.CosmicBackground/v1
mkdir -p "$dir"
# COSMIC takes each channel as a 0-1 float.
color=$(awk -v hex="$hex" 'BEGIN {
	for (i = 0; i < 3; i++)
		c[i] = strtonum("0x" substr(hex, 2 * i + 1, 2)) / 255
	printf "%.8g, %.8g, %.8g", c[0], c[1], c[2]
}')

# Write in place (no rename) so `cosmic-bg`'s watcher sees the change.
write_output() {
	cat >"$dir/$2" <<EOF
(
	output: "$1",
	source: Color(Single(($color))),
	filter_by_theme: false,
	rotation_frequency: 900,
	filter_method: Lanczos,
	scaling_mode: Zoom,
	sampling_method: Alphanumeric,
)
EOF
}

# Per-output entries take effect when `same-on-all` is false, so keep
# them in sync too.
if [ -f "$dir/backgrounds" ]; then
	while read -r output; do
		write_output "$output" "output.$output"
	done < <(sed -n 's/^ *"\(.*\)",\? *$/\1/p' "$dir/backgrounds")
fi
write_output all all
printf true >"$dir/same-on-all"

# `cosmic-bg` doesn't reload on config changes.
# `cosmic-session` will respawn it with the new config.
pkill -x cosmic-bg || true
