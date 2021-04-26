export TERM="xterm-256color"
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export GPG_TTY=$(tty)
export MANPATH="$HOME/.local/share/man:/usr/local/man:/usr/local/share/man:/usr/share/man:/usr/man"

[[ -z "$SHELL" ]] && export SHELL="/bin/zsh"

# Add a path in the *PATH environment variable only if the value does not have
# it. This is useful to avoid reseting all the path when running another shell
# via tmux or screen.
function stac_add_path() {
    local var_name=${1:?"Missing variable name"}
    local value=${2:?"Missing value"}
    if [[ ! ${(P)var_name} =~ ${value} ]]; then
        export ${var_name}=${value}:${(P)var_name}
    fi
}

stac_add_path 'LD_LIBRARY_PATH' "$HOME/.local/lib"
stac_add_path 'LIBRARY_PATH' "$HOME/.local/include"
stac_add_path 'CPATH' "$HOME/.local/include"
stac_add_path 'PATH' "$HOME/.local/bin"
stac_add_path 'PATH' "$HOME/.local/go/bin"
stac_add_path 'PATH' "$HOME/.cargo/bin"
stac_add_path 'PATH' "/usr/local/opt/ruby/bin"
if [[ -n "${GOPATH}" ]]; then
    stac_add_path 'PATH' "${GOPATH}/bin"
else
    stac_add_path 'PATH' "${HOME}/go/bin"
fi
