# vim: set ts=2 sw=2 et:

#------------------------------
# Key binding
#------------------------------
bindkey -v
bindkey "^R" history-incremental-search-backward

#------------------------------
# Autocompletion
#------------------------------
autoload -U compinit
compinit -u

# completion with hidden files
setopt glob_dots

zstyle ':completion:*' menu select

# Colors in ls autocompletion
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# List of pid for kill
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always

# Use cache
zstyle ':completion::complete:*' use-cache 1

#------------------------------
# History
#------------------------------
HISTFILE=$HOME/.zsh_histfile
HISTSIZE=5000
SAVEHIST=5000

setopt share_history
setopt hist_ignore_all_dups
setopt hist_allow_clobber
setopt hist_reduce_blanks
setopt no_hist_beep

#------------------------------
# Variables
#------------------------------
export BROWSER="firefox"
export EDITOR="vim"

#-----------------------------
# Dircolors
#-----------------------------
LS_COLORS='rs=0:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:tw=30;42:ow=34;42:st=37;44:ex=01;32:';
export LS_COLORS

#------------------------------
# Alias stuff
#------------------------------
alias ls='ls --classify --tabsize=0 --literal --color=auto --show-control-chars --human-readable'
alias ll="ls -lh"
alias la='ls -a'
alias lla='ls -la'
alias less='less --quiet -R'
alias df='df --human-readable'
alias du='du --human-readable'
alias grep='grep --color'
alias psall='ps -ef'
alias psmy='ps uxf --columns 1000'
# To speed up man
# alias man='PATH=/bin:/usr/bin MANPATH= man'

#------------------------------
# zsh options
#------------------------------

# Activate ^D to logout
unsetopt ignoreeof
#
# Print exit code if different from 0
setopt print_exit_value

# cd to directory without cd command
setopt auto_cd

# NEVER beep
unsetopt beep
unsetopt list_beep

# use >| to overwrite existing file with output
setopt no_clobber

# ask confirm for rm *
unsetopt rm_star_silent

# 'cd' pushes home dir to dir stack
setopt auto_pushd
# ignore dupes in dir stack
setopt pushd_ignore_dups
# do not print stack after pushd or popd
setopt pushd_silent
# pushd without argument == pushd $HOME
setopt pushd_to_home

#------------------------------
# Prompt
#------------------------------
autoload -U colors
colors

PROMPT="[$(print '%{\e[1;33m%}%T%{\e[0m%}')]""$(print '%{\e[1;31m%}%n%{\e[0m%}@%{\e[30;32m%}%m%{\e[0m%}:%~>')"
RPROMPT=

#------------------------------
# Java
#------------------------------
if [[ "$OSTYPE" == "linux-gnu" ]]; then
  export JAVA_HOME=$(readlink -f $(which java) | sed "s:bin/java::")
elif [[ "$OSTYPE" == "darwin"* ]]; then
  export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
else
  echo "This is an unknown OS."
fi

#------------------------------
# GDB
#------------------------------
GDBHISTFILE=$HOME/.gdb_history

export PAGER=less
export MANPAGER=less
