#!/usr/bin/env bash

stac_modify_path_like_var() {
    local var=${1:?'Missing variable name'}
    local value=${2:?'Missing value'}
    local modif_type=${3:-prepend}
    if [[ $modif_type != 'prepend' && $modif_type != 'append' ]]; then
        echo "Wrong modification type. $modif_type not in [append, prepend]"
        return
    fi

    local current_value
    current_value=$(eval "echo \$$var")
    case ":${current_value}:" in
        *":$value:"*) :;;
        *) if [[ -z "${current_value}" ]]; then
               eval "export $var=$value"
           elif [[ "$modif_type" == 'prepend' ]]; then
               eval "export $var=$value\${$var:+\":\$$var\"}"
           else
               eval "export $var=\${$var:+\"\$$var:\"}$value"
           fi ;;
    esac
}

export -f stac_modify_path_like_var

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
stac_modify_path_like_var MANPATH "$HOME/.local/share/man"
stac_modify_path_like_var LD_LIBRARY_PATH "$HOME/.local/lib"
stac_modify_path_like_var PATH "$HOME/.local/bin"

if [[ -z "${INSIDE_EMACS}" ]]; then
    export PAGER=less
    export MANPAGER=less
else
    export PAGER=cat
    export MANPAGER=cat
fi
export GDBHISTFILE=$HOME/.gdb_history
export BROWSER="firefox"
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
