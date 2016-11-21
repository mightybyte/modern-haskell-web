#!/bin/sh

#if ! command -v nix-shell >/dev/null ; then
#  . ~/.nix-profile/etc/profile.d/nix.sh
#fi

cabal build && cp dist/build/modern-haskell-web-gui/modern-haskell-web-gui.jsexe/all.js ../app/static/app.js
