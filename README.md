stac-vimrc
==========

My .vimrc which contains the procedure to configure Vim everywhere.  This one
is tested under Linux and Windows. I use this one to work on following project
types:

* Python
* Javascript
* XML
* CSS
* HTML

Installation
============

Simply type in your terminal:

    wget "https://raw.github.com/stac47/stac-vimrc/master/.vimrc"
    
If you start from scratch, start by creating ".vim" directory in your home then
a "bundle" and "autoload" subfolder:

    mkdir .vim
    cd .vim; mkdir bundle autoload
    
First, you have to install pathogen in the autoload folder:

    cd ~/.vim/autoload
    curl -Sso ~/.vim/autoload/pathogen.vim https://raw.github.com/tpope/vim-pathogen/master/autoload/pathogen.vim
    
Then install the plugins in the bundle folder:

    cd ~/.vim/bundle
    git clone https://github.com/scrooloose/nerdtree.git
    git clone git://github.com/klen/python-mode.git
