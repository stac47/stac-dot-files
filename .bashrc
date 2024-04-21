export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export GPG_TTY=$(tty)
export BROWSER="firefox"
export EDITOR="vim"
export PAGER=less
export MANPAGER=less

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
  LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=30;42:ow=30;43'
  alias ls='ls --classify --tabsize=0 --literal --color=auto --show-control-chars --human-readable'
else
  LSCOLORS='exfxcxdxbxegedabagacad'
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

PS1='[\[\033[1;33m\]\D{%F} \t\[\033[0m\]]\[\033[1;31m\]\u\[\033[0m\]@\[\033[1;32m\]\h|\l\[\033[1;36m\](\j)\[\033[0m\]\[\033[1;35m\]$(__git_ps1 "(%s)")\[\033[0m\]:\w\n% '

[[ -x $(command -v direnv) ]] && eval "$(direnv hook bash)"
[[ -x ~/.rbenv/bin/rbenv ]] && eval "$(~/.rbenv/bin/rbenv init - bash)"

function vim_configure() {
  make distclean
  ./configure \
    --prefix="$HOME/.local" \
    --enable-multibyte \
    --with-tlib=ncurses \
    --with-compiledby=stac47 \
    --enable-cscope \
    --enable-terminal \
    --disable-perlinterp \
    --disable-rubyinterp \
    --disable-python3interp \
    --disable-gui \
    --without-x \
    --disable-luainterp \
    --disable-gtktest \
    --disable-netbeans \
    --enable-fail-if-missing
}
