#!/bin/sh

if ! command -v nix-shell >/dev/null ; then
  . ~/.nix-profile/etc/profile.d/nix.sh
fi

nix-shell -A env --pure -j 8 -I ../deps --command "cabal configure --ghcjs && return"
