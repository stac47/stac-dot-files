# vim: set ts=2 sw=2 et:

#------------------------------
# Key binding
#------------------------------
# Emacs binding
bindkey -e
bindkey "^R" history-incremental-search-backward

#------------------------------
# Word definition for CTRL-W
#------------------------------
autoload -U select-word-style
select-word-style bash
export WORDCHARS='.-'

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
# Git info
#------------------------------
setopt prompt_subst
autoload -Uz vcs_info
zstyle ':vcs_info:git*' formats '%F{magenta}(%s:%b)%f'
zstyle ':vcs_info:git*' actionformats '%F{magenta}(%s:%b|%a)%f'
precmd () { vcs_info }

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
setopt hist_ignore_space

#------------------------------
# Variables
#------------------------------
export BROWSER="firefox"
export EDITOR="vim"


#------------------------------
# Alias stuff
#------------------------------
if ls --version 1>/dev/null 2>&1; then
  export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=30;42:ow=30;43'
  alias ls='ls --classify --tabsize=0 --literal --color=auto --show-control-chars --human-readable'
else
  export LSCOLORS='exfxcxdxbxegedabagacad'
  alias ls='ls -G'
fi
alias ll="ls -lh"
alias la='ls -a'
alias lla='ls -la'
if [[ "$OSTYPE" =~ "^linux.*" ]] || [[ "$OSTYPE" == 'cygwin' ]]; then
  alias psall='ps -ef'
  alias psmy='ps wwuxf'
fi
if [[ "$(grep --version)" =~ .*GNU.* ]]; then
  alias grep='grep --color'
fi
alias mount='mount |column -t'
alias less='less --quiet -R'

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

zsh_system_version=$($SHELL --version | cut -d ' ' -f 2)

if [[ "$(echo -e "$zsh_system_version\n5.0" | sort --version-sort | head -n 1)" = "$zsh_system_version" ]]
then # the current shell version is below 5.0
  PROMPT="[$(print '%{\e[1;33m%}%D %*%{\e[0m%}')]$(print '%{\e[1;31m%}%n%{\e[0m%}@%{\e[30;32m%}%m%{\e[0m%}'):%~
%# "
  RPROMPT=
else
  PROMPT='%B[%F{yellow}%D %*%f]%F{red}%n%f@%F{green}%M|%y%f%F{cyan}(%j)%f:${vcs_info_msg_0_}%~%b
%# '
  RPROMPT=
fi

#------------------------------
# Java
#------------------------------
if [[ -z $(which java) ]]; then
  if [[ "$OSTYPE" =~ "^linux.*" ]]; then
    export JAVA_HOME=$(readlink -f $(which java) | sed "s:bin/java::")
  elif [[ "$OSTYPE" =~ "^darwin.*" ]]; then
    export JAVA_HOME=`/usr/libexec/java_home`
  else
    echo "This is an unknown OS."
  fi
fi

#------------------------------
# GDB
#------------------------------
GDBHISTFILE=$HOME/.gdb_history

export PAGER=less
export MANPAGER=less
