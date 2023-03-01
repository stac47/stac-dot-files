if [[ $(uname) == 'Darwin' ]]; then
    eval "$(PATH='/usr/local/bin:/opt/homebrew/bin' brew shellenv)"
fi

GDBHISTFILE=$HOME/.gdb_history

MANPATH="$HOME/.local/share/man:$MANPATH"
LD_LIBRARY_PATH="$HOME/.local/lib:$LD_LIBRARY_PATH"
PATH="$HOME/.local/bin:$PATH"

if [[ -e "$HOME/.zprofile.local" ]]; then
    source "$HOME/.zprofile.local"
fi
