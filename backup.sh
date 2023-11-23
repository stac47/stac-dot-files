#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

BACKUP_DATE=$(date +'%Y_%m_%d_%H_%M_%S_%N')
BACKUP_FILENAME="/tmp/backup_$BACKUP_DATE.tar.gz"

SYNC_DIRECTORIES=(
    'Documents'
    'Downloads'
    'Music'
    'Pictures'
    'Videos'
    'fsgt_backup'
)

GIT_REPOSITORIES=(
    'development'
    'Papiers'
    '.password-store'
)

EXCLUSIONS=(
    ${SYNC_DIRECTORIES[@]}
    ${GIT_REPOSITORIES[@]}
    '.alacritty.yml'
    '.bundle'
    '.cache'
    '.config'
    '.gitconfig'
    '.gitignore_global'
    '.irbrc'
    '.local'
    '.mailcap'
    '.mozilla'
    '.mutt'
    '.muttrc'
    '.procmailrc'
    '.rbenv'
    '.stac-dot-files'
    '.tmux'
    '.tmux.conf'
    '.vim'
    '.vimrc'
    '.zlogin'
    '.zprofile'
    '.zshenv'
    '.zshrc'
    'opensource'
)

function backup_tar() {
    cmd=("tar" "-cvpzf" ${BACKUP_FILENAME})
    cmd+=("--directory=$HOME")
    for f in "${EXCLUSIONS[@]}"; do
        cmd+=("--exclude=${f}")
    done
    cmd+=('--warning=no-file-changed')
    cmd+=('.')
    set -x
    "${cmd[@]}"
    set +x
}

function backup_git_bundle() {
    local path="${1}"
    local project_name=$(basename $path)
    local bundle_name="$project_name.bundle"
    git -C "$path" bundle create "$project_name.bundle" --all
    tar --append --file="${BACKUP_FILENAME} "$path/
}

# backup_git_bundle "/home/stac/development/advent_of_code"
backup_tar
