autoload -U compinit promptinit
compinit
promptinit

zstyle ':completion:*' menu select

#------------------------------
# History stuff
#------------------------------
HISTFILE=$HOME/.zsh_histfile
HISTSIZE=5000
SAVEHIST=5000

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
alias ls="ls -GF"
alias ll="ls -lh"

setopt autopushd pushdsilent pushdtohome

## Remove duplicate entries
setopt pushdignoredups

## This reverts the +/- operators.
setopt pushdminus

#------------------------------
# Prompt
#------------------------------
autoload -U colors
colors

PROMPT="$fg_bold[blue][%*]$fg_bold[white][%n@%M]$fg_bold[red][%~]$fg_bold[white]>$fg_no_bold[white] "
RPROMPT=

# OS specific stuff
if [[ "$OSTYPE" == "linux-gnu" ]]; then
    export JAVA_HOME=$(readlink -f /usr/bin/java | sed "s:bin/java::")
elif [[ "$OSTYPE" == "darwin"* ]]; then
    export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
else
    echo "This is an unknown OS."
fi
# vim: set ts=2 sw=2 et:
