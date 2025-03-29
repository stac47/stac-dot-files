#!/usr/bin/env bash

if [[ -e "$HOME/.bash_profile.local" ]]; then
    source "$HOME/.bash_profile.local"
fi

if [ -f "${HOME}/.bashrc" ] ; then
    source "${HOME}/.bashrc"
fi
