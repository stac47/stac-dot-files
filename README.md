stac-vimrc
==========

My .vimrc which contains the procedure to configure Vim everywhere.  This one
is tested under Linux, Mac OS X and Windows. I use this one to work on following project
types:

* Python
* Ruby
* Javascript
* XML
* CSS
* HTML
* Markdown

Installation
============

Method 1: downloading the raw main files
----------------------------------------

This method should be prefered on Windows.

The main files are of course the .vimrc file and the pathogen vim plugin (This
one allow vim to load automatically the plugins that stand in the .vim/bundle
folder.

Simply go to your home and type in your terminal:

    wget "https://raw.github.com/stac47/stac-vimrc/master/.vimrc"

or

    curl -O https://raw.github.com/stac47/stac-vimrc/master/.vimrc

If you start from scratch, start by creating ".vim" directory in your home then
a "bundle" and "autoload" subfolder:

    mkdir .vim
    cd .vim; mkdir autoload

Then, you have to install pathogen in the autoload folder:

    cd ~/.vim/autoload
    curl -Sso ~/.vim/autoload/pathogen.vim https://raw.github.com/tpope/vim-pathogen/master/autoload/pathogen.vim

Method 2: cloning git projects then symlinks
--------------------------------------------

This method should be prefered under Linux/MacOS X.

The previous method is less convenient if you want to propose a pull request on
the two main projects. Updating them is also a bit rude. This method simple
create clones somewhere and you simply create symbolic links to the main files.
Hereafter is how to proceed.

First, create a folder which will contain the two main projects then jump into
it:

    mkdir ~/.vim_git;cd .vim_git

Next, simply clone the two projects:

    git clone https://github.com/stac47/stac-vimrc.git
    git clone https://github.com/tpope/vim-pathogen.git

And the last step consists in linking everything from your home folder.

    cd
    mkdir .vim
    ln -s ~/.vim_git/stac-vimrc/.vimrc .vimrc
    ln -s ~/.vim_git/vim-pathogen/autoload .vim/autoload

Finalization: Installing the plugins
------------------------------------

Then install the plugins in the bundle folder:

    mkdir ~/.vim/bundle;cd ~/.vim/bundle

Hereafter is the list of the plugins I use.

* To see the file tree and easily open files:

    git clone https://github.com/scrooloose/nerdtree.git

* To fuzzy search files and easily open them:
    
    git clone https://github.com/kien/ctrlp.vim.git

* All needed tools for python development:

    git clone https://github.com/klen/python-mode.git

* Markdown syntax highlighting:

    git clone https://github.com/plasticboy/vim-markdown.git

* Plugins to enhance pleasure with Ruby:

    git clone https://github.com/vim-ruby/vim-ruby.git
