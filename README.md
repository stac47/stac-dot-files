stac-dot-files
==============

All my dot files. They are commons for GNU/Linux and MacOSX.

Installation
------------

First clone this repo in your home. I usually clone it in ~/.stac-dot-files.

    git clone https://github.com/stac47/stac-dot-files.git <project_folder>

### Automatic

Simply execute the shell script in <project_folder>/config/config.sh.

### Manual

#### Getting the config files and plugins

Go to the project's folder and download all the git submodules:

    cd <project_folder>
    git submodule update --recursive --init

#### Creating symbolic links to the config files

Finally, simply symlink all the files you need:

    ln -s <project_folder>/.bashrc ~/.bashrc
    ln -s <project_folder>/.bash_profile ~/.bash_profile
    ln -s <project_folder>/.bash_logout ~/.bash_logout
    ln -s <project_folder>/.gitconfig ~/.gitconfig
    ln -s <project_folder>/.gitignore_global ~/.gitignore_global
    ln -s <project_folder>/.irbrc ~/.irbrc
    ln -s <project_folder>/.vimrc ~/.vimrc
    ln -s <project_folder>/.vim ~/.vim
    ln -s <project_folder>/scripts/battery/bin/battery ~/battery

Regarding tmux, as it is a bit more difficult to copy to the Mac OSX clipboard,
we have two choices of ".tmux.conf" file:

    ln -s <project_folder>/.tmux.conf.osx ~/.tmux.conf
    or
    ln -s <project_folder>/.tmux.conf.linux ~/.tmux.conf

### Mac OSX specifics

This will configure your mac as I like it:

    <project_folder>/config/macosx/macosx-setup.sh

Installation of all my programs can be done with Homebrew thanks to the
following command:

    brew update
    cat <project_folder>/config/macosx/brew.list | xargs brew install

Maintainance
------------

### Vim plugins

To add a new plugin:

    git submodule add <address of the modules's repo>

To upgrade all the plugins to there latest version:

    git submodule update --remote --merge
    git commit ...

To remove a plugin (from git 1.8.5)

    git submodule deinit <module>
    git rm <module>
    git commit ...
