#! /usr/bin/env nix-shell
#! nix-shell -p nix-prefetch-git -i bash
HERE=$(dirname $0)
nix-prefetch-git https://github.com/domenkozar/hie-nix $1 > $HERE/hie-nix.json
