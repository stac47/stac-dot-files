stac-dot-files
==============

All my dot files. They are commons for GNU/Linux and MacOSX.

Installation
------------

First clone this repo in your home. I usually clone it in ~/.stac-dot-files.

    git clone https://github.com/stac47/stac-dot-files.git <project_folder>

### Manual

#### Getting the config files and plugins

Go to the project's folder and download all the git submodules:

    cd <project_folder>
    git submodule update --recursive --init

#### Creating symbolic links to the config files

Finally, simply symlink all the files you need:

    ln -s <project_folder>/.zshrc ~/.zshrc
    ln -s <project_folder>/.zshenv ~/.zshenv
    ln -s <project_folder>/.gitconfig ~/.gitconfig
    ln -s <project_folder>/.gitignore_global ~/.gitignore_global
    ln -s <project_folder>/.irbrc ~/.irbrc
    ln -s <project_folder>/.vimrc ~/.vimrc
    ln -s <project_folder>/.vim ~/.vim
    ln -s <project_folder>/scripts/battery/bin/battery ~/battery

Maintainance
------------

### Vim plugins

To add a new plugin:

    git submodule add <address of the modules's repo>

To upgrade all the plugins to there latest version:

    git submodule update --remote --rebase
    git commit ...

To remove a plugin (from git 1.8.5)

    git submodule deinit <module>
    git rm <module>
    git commit ...
