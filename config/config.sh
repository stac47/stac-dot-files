#! /usr/bin/env bash
# author: stac47
#
# This script perform a full configuration of a Unix/Linux environment.
# Depending upon the $OSTYPE variable it will performs the following setup:
# * Common
#   - Recursively pull the git submodules
#   - Symlink the following config files to $HOME folder
#      + .bashrc
#      + .bash_profile
#      + .bash_logout
#      + .gitignore_global
#      + .gitconfig
#      + .irbrc
#      + .vimrc
#      + scripts/battery/bin/battery
#   - Symlink the following folder in $HOME folder
#      + .vim
# * Mac OSX specific
#   - Symlink the following config files to $HOME folder
#      + .tmux.config.osx -> .tmux.conf
# *  Linux specific
#   - Symlink the following config files to $HOME folder
#      + .tmux.config.linux -> .tmux.conf

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )"
cd $DIR
echo "Working directory: $DIR"

echo "Updating submodules..."
git submodule update --recursive --init

echo "Symlinking the configuration files."
ln -s $DIR/.bashrc $HOME/.bashrc
ln -s $DIR/.bash_profile $HOME/.bash_profile
ln -s $DIR/.bash_logout $HOME/.bash_logout
ln -s $DIR/.gitconfig $HOME/.gitconfig
ln -s $DIR/.gitignore_global $HOME/.gitignore_global
ln -s $DIR/.irbrc $HOME/.irbrc
ln -s $DIR/.vimrc $HOME/.vimrc
ln -sh $DIR/.vim $HOME/.vim
ln -s $DIR/scripts/battery/bin/battery $HOME/battery

if [[ $OSTYPE == darwin* ]] ; then
    echo "Mac OSX tmux config sopecial case."
    ln -s $DIR/.tmux.conf.osx $HOME/.tmux.conf
else
    echo "Tmux configuration standard case."
    ln -s $DIR/.tmux.conf.linux $HOME/.tmux.conf
fi
