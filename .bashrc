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

export PS1='[\e[1;33m\t\e[m]\e[1;31m\u\e[m@\e[1;32m\h:\l\e[m(\e[0;36m\j\e[m):\w\$ '
export CLICOLOR=1

#------------------------------
# Alias stuff
#------------------------------
case ${OSTYPE} in
    linux.* | cygwin)
        export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=30;42:ow=30;43'
        alias ls='ls --classify --tabsize=0 --literal --color=auto --show-control-chars --human-readable'
        alias grep='grep --color'
        alias psall='ps -ef'
        alias psmy='ps uxf --columns 1000'
        export JAVA_HOME=$(readlink -f /usr/bin/java | sed "s:bin/java::")
        ;;
    ^darwin.* )
        export LSCOLORS='exfxcxdxbxegedabagacad'
        alias ls='ls -G'
        export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
        if [ -f $(brew --prefix)/etc/bash_completion ]; then
          . $(brew --prefix)/etc/bash_completion
        fi
        ;;
    *)
        echo "This is an unknown OS"
        ;;
esac
alias ll="ls -lh"
alias la='ls -a'
alias lla='ls -la'
alias mount='mount |column -t'
alias less='less --quiet -R'
