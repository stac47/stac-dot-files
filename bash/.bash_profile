GDBHISTFILE=$HOME/.gdb_history

MANPATH="$HOME/.local/share/man:$MANPATH"
LD_LIBRARY_PATH="$HOME/.local/lib:$LD_LIBRARY_PATH"
PATH="$HOME/.local/bin:$PATH"

if [[ -e "$HOME/.bash_profile.local" ]]; then
    source "$HOME/.bash_profile.local"
fi

if [ -f "${HOME}/.bashrc" ] ; then
  source "${HOME}/.bashrc"
fi
