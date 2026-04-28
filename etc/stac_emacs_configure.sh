#!/bin/bash

set -euo pipefail
IFS=$'\n\t'


CONFIGURE_OPTIONS=(
    "--prefix=$HOME/.local"
    "--disable-gc-mark-trace"
    "--with-cairo"
    "--with-dbus"
    "--with-gif"
    "--with-gnutls"
    "--with-harfbuzz"
    "--with-imagemagick"
    "--with-jpeg"
    "--with-mailutils"
    "--with-modules"
    "--with-png"
    "--with-rsvg"
    "--with-sqlite3"
    "--with-tiff"
    "--with-tree-sitter"
    "--with-webp"
    "--with-wide-int"
    "--with-xinput2"
    "--with-xpm"
    "--with-xml2"
    "--without-gpm"
    "--without-pop"
    "--without-xwidgets"
)

if [[ $(uname) == 'Darwin' ]]; then
    CONFIGURE_OPTIONS+=("--without-native-compilation")
else
    CONFIGURE_OPTIONS+=("--with-native-compilation=aot")
    CONFIGURE_OPTIONS+=("--with-x")
fi

./configure "${CONFIGURE_OPTIONS[@]}" \
            CFLAGS="-O2 -pipe -mtune=native -march=native -fomit-frame-pointer"
