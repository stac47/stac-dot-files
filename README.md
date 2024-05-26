# stac-dot-files

All my dot files. They are commons for GNU/Linux and MacOSX.

## Installation

Install [GNU Stow](https://www.gnu.org/software/stow/).

Then clone this repo.

```
git clone https://github.com/stac47/stac-dot-files.git
```

Finally use `stow` to symlink only what you need:

```
cd stac-dot-file
stow --no-folding -t $HOME emacs
```
