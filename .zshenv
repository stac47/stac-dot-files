export TERM="xterm-256color"
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

[[ -z "$SHELL" ]] && export SHELL="/bin/zsh"

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
