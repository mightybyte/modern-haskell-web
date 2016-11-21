#!/bin/sh

cd frontend && cabal build && cp dist/build/modern-haskell-web-gui/modern-haskell-web-gui.jsexe/all.js ../app/static/app.js
