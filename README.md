# stac-dot-files

All my dot files. They are commons for GNU/Linux and MacOSX.

## Installation

Install [GNU Stow](https://www.gnu.org/software/stow/).

Then clone this repository and use `stow` to symlink only what you need:

```
cd <repo-folder>
stow -t $HOME bash
```

For programs like `vim` or `emacs` that need a configuration directories
(respectively `.vim` and `.emacs.d`, make sure those directories exist in your
`HOME` before running the `stow` command, otherwise it will pollute this
repository.
