# shellcheck shell=bash
set -uf # disable globbing
if [[ -z ${OUT_PATHS:-} ]]; then
	echo 'OUT_PATHS is empty' >&2
	exit
else
	echo 'OUT_PATHS is populated' >&2
fi

export IFS=' '
# shellcheck disable=SC2086 # we want OUT_PATHS to be split
CACHIX_AUTH_TOKEN=$(<"$CACHIX_AUTH_TOKEN_FILE") cachix push --verbose lafrenierejm $OUT_PATHS
