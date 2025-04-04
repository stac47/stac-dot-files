#!/usr/bin/env bash

stac_modify_path_like_var() {
    local var=${1:?'Missing variable name'}
    local value=${2:?'Missing value'}
    local modif_type=${3:-prepend}
    if [[ $modif_type != 'prepend' && $modif_type != 'append' ]]; then
        echo "Wrong modification type. $modif_type not in [append, prepend]"
        return
    fi

    local current_value
    current_value=$(eval "echo \$$var")
    case ":${current_value}:" in
        *":$value:"*) :;;
        *) if [[ -z "${current_value}" ]]; then
               eval "export $var=$value"
           elif [[ "$modif_type" == 'prepend' ]]; then
               eval "export $var=$value\${$var:+\":\$$var\"}"
           else
               eval "export $var=\${$var:+\"\$$var:\"}$value"
           fi ;;
    esac
}

export -f stac_modify_path_like_var

stac_modify_path_like_var MANPATH "$HOME/.local/share/man"
stac_modify_path_like_var LD_LIBRARY_PATH "$HOME/.local/lib"
stac_modify_path_like_var PATH "$HOME/.local/bin"

if [[ -e "$HOME/.bash_profile.local" ]]; then
    source "$HOME/.bash_profile.local"
fi
