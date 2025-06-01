#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

DESTINATION=${1:?'Missing destination'}
GIT_DESTINATION="${DESTINATION}/git"
mkdir -p "${GIT_DESTINATION}"
BACKUP_DATE=$(date +'%Y_%m_%d_%H_%M_%S_%N')
BACKUP_FILENAME="/tmp/backup_$BACKUP_DATE.tar.gz"
GPG_BACKUP_FILENAME="${BACKUP_FILENAME}.gpg"

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
    '.bashrc'
    '.bash_profile'
    '.bitcoin'
    '.cache'
    '.cert'
    '.config'
    '.emacs.d'
    '.gitconfig'
    '.gitignore_global'
    '.git-credential-cache'
    '.gem'
    '.inputrc'
    '.irbrc'
    '.kube'
    '.local'
    '.mailcap'
    '.minikube'
    '.mozilla'
    '.mutt'
    '.muttrc'
    '.npm'
    '.nvm'
    '.procmailrc'
    '.python_history'
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
    'fsgtfiles'
    'opensource'
    'music_flat'
    'scroll_stealer_data'
    'Mail'
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
    gpg --output "${GPG_BACKUP_FILENAME}" \
        --recipient "laurent.stacul@gmail.com" \
        --encrypt "${BACKUP_FILENAME}"
    mv -v "${GPG_BACKUP_FILENAME}" "${DESTINATION}"
    rm "${BACKUP_FILENAME}"
    set +x
}

function backup_git_bundle() {
    local path="${1:?'Missing path'}"
    echo "Bundling git repository: ${path}"
    if [[ -d "$path/.git" ]] then
        local project_name=$(basename $path)
        local bundle_name="$project_name.bundle"
        git -C "$path" bundle create "${GIT_DESTINATION}/$project_name.bundle" --all
    else
        find "$path" -mindepth 1 -maxdepth 1 -type d | while IFS= read -r d; do
            backup_git_bundle "$d"
        done
    fi
}

function backup_git() {
    for git_dir in "${GIT_REPOSITORIES[@]}"; do
        backup_git_bundle "$HOME/$git_dir"
    done
}

function backup_sync() {
    local cmd
    cmd=("rsync" "-avz" "--delete" "--safe-links")
    for directory in "${SYNC_DIRECTORIES[@]}"; do
        cmd+=("$HOME/$directory")
    done
    cmd+=($DESTINATION)
    set -x
    "${cmd[@]}"
    set +x
}

backup_tar
backup_sync
backup_git
