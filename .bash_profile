export TMP=$HOME/tmp/
export PATH=/usr/local/bin:$PATH
export PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
export PATH=$PATH:~/bin
[[ -s "$HOME/.profile" ]] && source "$HOME/.profile" # Load the default

export PS1="\[\033[36m\]\u\[\033[m\]@\[\033[32m\]\h:\[\033[33;1m\]\w\[\033[m\]\$ "
export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad

# pip should only run if there is a virtualenv currently activated
export PIP_REQUIRE_VIRTUALENV=true
# cache pip-installed packages to avoid re-downloading
export PIP_DOWNLOAD_CACHE=$HOME/.pip/cache

syspip(){
   PIP_REQUIRE_VIRTUALENV="" pip3 "$@"
}

alias ls='ls -GFh'
alias ll='ls -l'
alias psmy='ps -fu $USER | sort | egrep -v "ps -fu|sort|grep"'
# Always 256 colors
alias tmux='tmux -2'
alias grep='grep --color'

# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" 

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

if [[ "$TERM" != "screen-256color" ]]
then
    tmux attach-session -t "$USER" || tmux new-session -s "$USER"
    exit
fi

cowsay "Welcome my lord"
alias dev='cd ~;source virtualenvs/roomies-env/bin/activate;cd development/django-myroomies'
export PATH=/usr/local/sbin:$PATH
