#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

function install_tools() {
    local os="$(uname)"
    case ${os} in
        Darwin)
            _install_tools_darwin
            ;;
        Linux)
            _install_tools_linux
            ;;
        *)
            echo "Unknown OS"
            exit 1
            ;;
    esac
}

function _install_tools_darwin() {
    echo "TODO"
    exit 1
}

function _install_tools_linux() {
    local distrib=$(lsb_release -d | awk -F"\t" '{print $2}' | cut -d' ' -f 1)
    case ${distrib} in
        Ubuntu | Debian)
            apt update && \
            apt install -y \
                apt-transport-https \
                autoconf \
                automake \
                build-essential \
                bzip2 \
                ca-certificates \
                chrpath \
                cmake \
                cscope \
                curl \
                dos2unix \
                exuberant-ctags \
                gcc-doc \
                gdb \
                gdb-doc \
                glibc-doc \
                gnupg-agent \
                htop \
                jq \
                libncurses-dev \
                libtool \
                make-doc \
                mosh \
                pkg-config \
                python3 \
                python3-venv \
                python-dev-is-python3 \
                software-properties-common \
                valgrind \
                vim \
                silversearcher-ag \
                tmux \
                tree \
                zsh
            ;;
        *)
            echo "Unknown Linux distribution"
            exit 1
            ;;
    esac

}

install_tools
