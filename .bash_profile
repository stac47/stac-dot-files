#------------------------------
# Variables
#------------------------------
export TERM="xterm-256color"
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

export BROWSER="firefox"
export EDITOR="vim"
export PAGER=less
export MANPAGER=less

export PS1="\[\033[36m\]\u\[\033[m\]@\[\033[32m\]\h:\[\033[33;1m\]\w\[\033[m\]\$ "
export CLICOLOR=1

#------------------------------
# Alias stuff
#------------------------------
if [[ "$OSTYPE" =~ "^linux.*" ]]; then
  export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=30;42:ow=30;43'
  alias ls='ls --classify --tabsize=0 --literal --color=auto --show-control-chars --human-readable'
  alias grep='grep --color'
  alias psall='ps -ef'
  alias psmy='ps uxf --columns 1000'
elif [[ "$OSTYPE" =~ "^darwin.*" ]]; then
  export LSCOLORS='exfxcxdxbxegedabagacad'
  alias ls='ls -G'
fi
alias ll="ls -lh"
alias la='ls -a'
alias lla='ls -la'
alias mount='mount |column -t'
alias less='less --quiet -R'

# OS specific stuff
if [[ "$OSTYPE" == "linux-gnu" ]]; then
    export JAVA_HOME=$(readlink -f /usr/bin/java | sed "s:bin/java::")
elif [[ "$OSTYPE" == "darwin"* ]]; then
    export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
    if [ -f $(brew --prefix)/etc/bash_completion ]; then
      . $(brew --prefix)/etc/bash_completion
    fi
else
    echo "This is an unknown OS."
fi
