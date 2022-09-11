export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export GPG_TTY=$(tty)

[[ -z "$SHELL" ]] && export SHELL="/bin/zsh"

function test_colors {
    awk 'BEGIN{
        s="/\\/\\/\\/\\/\\"; s=s s s s s s s s s s s s s s s s s s s s s s s;
        for (colnum = 0; colnum<256; colnum++) {
            r = 255-(colnum*255/255);
            g = (colnum*510/255);
            b = (colnum*255/255);
            if (g>255) g = 510-g;
            printf "\033[48;2;%d;%d;%dm", r,g,b;
            printf "\033[38;2;%d;%d;%dm", 255-r,255-g,255-b;
            printf "%s\033[0m", substr(s,colnum+1,1);
        }
        printf "\n";
    }'
}

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

stac_add_to_string 'prepend' 'MANPATH' "$HOME/.local/share/man"
stac_add_to_string 'prepend' 'LD_LIBRARY_PATH' "$HOME/.local/lib"
stac_add_to_string 'prepend' 'PATH' "$HOME/.local/bin"
