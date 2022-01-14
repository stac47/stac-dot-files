export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export GPG_TTY=$(tty)
export MANPATH="$HOME/.local/share/man:/usr/local/man:/usr/local/share/man:/usr/share/man:/usr/man"

[[ -z "$SHELL" ]] && export SHELL="/bin/zsh"

# Add a path in the *PATH environment variable only if the value does not have
# it. This is useful to avoid reseting all the path when running another shell
# via tmux or screen.
function stac_add_to_string() {
    local mode=${1:?'Missing mode append/prepend'}
    local var_name=${2:?"Missing variable name"}
    local value=${3:?"Missing value"}
    if [[ -z "${(P)var_name}" ]]; then
        export ${var_name}=${value}
        return
    fi
    if [[ ! ${(P)var_name} =~ ${value} ]]; then
        case "${mode}" in
            "append")
                export ${var_name}=${(P)var_name}:${value}
                ;;
            "prepend")
                export ${var_name}=${value}:${(P)var_name}
                ;;
            *)
                echo "Unknown mode: ${mode}"
                ;;
        esac
    fi
}

stac_add_to_string 'prepend' 'LD_LIBRARY_PATH' "$HOME/.local/lib"
stac_add_to_string 'prepend' 'LIBRARY_PATH' "$HOME/.local/include"
stac_add_to_string 'prepend' 'CPATH' "$HOME/.local/include"
stac_add_to_string 'prepend' 'PATH' "$HOME/.local/bin"
stac_add_to_string 'prepend' 'PATH' "$HOME/.local/go/bin"
stac_add_to_string 'prepend' 'PATH' "$HOME/.cargo/bin"
stac_add_to_string 'prepend' 'PATH' "/usr/local/opt/ruby/bin"

if [[ -n "${GOPATH}" ]]; then
    stac_add_to_string 'prepend' 'PATH' "${GOPATH}/bin"
else
    stac_add_to_string 'prepend' 'PATH' "${HOME}/go/bin"
fi
