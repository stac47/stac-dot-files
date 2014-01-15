export PATH=/usr/local/bin:$PATH
export PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
export PATH=$PATH:~/bin
[[ -s "$HOME/.profile" ]] && source "$HOME/.profile" # Load the default

export PS1="\[\033[36m\]\u\[\033[m\]@\[\033[32m\]\h:\[\033[33;1m\]\w\[\033[m\]\$ "
export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad
export LANG=en_US.UTF-8

alias ls='ls -GFh'
alias ll='ls -l'
alias psmy='ps -fu $USER | sort | egrep -v "ps -fu|sort|grep"'

# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" 

# OS specific stuff
if [[ "$OSTYPE" == "linux-gnu" ]]; then
    export JAVA_HOME=$(readlink -f /usr/bin/java | sed "s:bin/java::")
elif [[ "$OSTYPE" == "darwin"* ]]; then
    export JAVA_HOME=`/usr/libexec/java_home -v 1.7`
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

echo "Welcome my lord"
