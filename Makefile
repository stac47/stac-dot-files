# Technical prelude
# See: https://tech.davis-hansson.com/p/make/
SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules
ifeq ($(origin .RECIPEPREFIX), undefined)
  $(error This Make does not support .RECIPEPREFIX. Please use GNU Make 4.0 or later)
endif
.RECIPEPREFIX = >

DOT_ZSH := $(HOME)/.zshrc $(HOME)/.zshenv
DOT_VIM := $(HOME)/.vimrc $(HOME)/.vim
DOT_GIT := $(HOME)/.gitconfig $(HOME)/.gitconfig_common $(HOME)/.gitignore_global
DOT_TMUX := $(HOME)/.tmux $(HOME)/.tmux.conf
DOT_MAIL := $(HOME)/.procmailrc $(HOME)/.muttrc $(HOME)/.mutt $(HOME)/.mailcap
DOT_MISC := $(HOME)/.alacritty.yml $(HOME)/.gdbinit $(HOME)/.clang-format

ALL_DOT_FILES := $(DOT_ZSH) $(DOT_VIM) $(DOT_GIT) $(DOT_TMUX) $(DOT_MAIL) $(DOT_MISC)

.PHONY: zsh
zsh: $(DOT_ZSH)

.PHONY: vim
vim: $(DOT_VIM)

.PHONY: git
git: $(DOT_GIT)

.PHONY: tmux
tmux: $(DOT_TMUX)

.PHONY: mail
mail: $(DOT_MAIL)

.PHONY: misc
misc: $(DOT_MISC)

.PHONY: all
all: zsh vim git tmux mail misc

$(HOME)/%: %
>ln -sf $(shell realpath $<) $@

.PHONY: clean
clean:
> for dotfile in $(ALL_DOT_FILES); do
>	if [[ -h $${dotfile} ]]; then
>        rm $${dotfile}
>    fi
>done
