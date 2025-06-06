#!/usr/bin/env bash

if [[ "$STAC_BASH_PROFILE_SOURCED" != '1' ]]; then
    source "$HOME/.bash_profile"
fi

if [[ -z "${INSIDE_EMACS}" ]]; then
    export PAGER=less
    export MANPAGER=less
else
    export PAGER=cat
    export MANPAGER=cat
fi
export GDBHISTFILE=$HOME/.gdb_history
export EDITOR="emacsclient"
export GPG_TTY
GPG_TTY=$(tty)

# -F: automatically exits if the entire file can be displayed on one screen
# -I: causes search to ignore case
# -R: display ANSI colors
# -S: do not wrap long lines
# -X: do not set termcap init to the terminal
export LESS="-FSRXI"

if [[ -e "$HOME/.bashrc.local" ]]; then
    source "$HOME/.bashrc.local"
fi

if ls --version 1>/dev/null 2>&1; then
    # GNU ls
    export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=30;42:ow=30;43'
    alias ls='ls --classify --tabsize=0 --literal --color=auto --show-control-chars --human-readable'
else
    export LSCOLORS='exfxcxdxbxegedabagacad'
    alias ls='ls -G'
fi

alias ll="ls -lh"
alias la='ls -a'
alias lla='ls -la'

if [[ "$OSTYPE" =~ ^linux.* ]] || [[ "$OSTYPE" == 'cygwin' ]]; then
    alias psall='ps -ef'
    alias psmy='ps wwuxf'
fi

if [[ "$(grep --version)" =~ .*GNU.* ]]; then
    alias grep='grep --color'
fi
alias mount='mount |column -t'
alias less='less --quiet -R'

PS1='[\[\033[1;33m\]\D{%F} \t\[\033[0m\]]\[\033[1;31m\]\u\[\033[0m\]@\[\033[1;32m\]\h|\l\[\033[1;36m\](\j)\[\033[0m\]\[\033[1;35m\]$(__git_ps1 "(%s)")\[\033[0m\]:\w\n% '
