# Technical prelude
# See: https://tech.davis-hansson.com/p/make/
SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

DOT_ZSH := $(HOME)/.zshrc $(HOME)/.zshenv
DOT_VIM := $(HOME)/.vimrc $(HOME)/.vim
DOT_GIT := $(HOME)/.gitconfig $(HOME)/.gitconfig_common $(HOME)/.gitignore_global
DOT_TMUX := $(HOME)/.tmux $(HOME)/.tmux.conf
DOT_MAIL := $(HOME)/.procmailrc $(HOME)/.muttrc $(HOME)/.mutt $(HOME)/.mailcap
DOT_ALACRITTY := $(HOME)/.alacritty.yml 
DOT_MISC := $(HOME)/.gdbinit $(HOME)/.clang-format

BREW_LIST := brew.list

ALL_DOT_FILES := $(DOT_ZSH) $(DOT_VIM) $(DOT_GIT) $(DOT_TMUX) $(DOT_MAIL) $(DOT_MISC)

.PHONY: zsh
zsh: $(DOT_ZSH) ## Configure zsh

.PHONY: vim
vim: $(DOT_VIM) ## Configure vim

.PHONY: git
git: $(DOT_GIT) ## Configure git

.PHONY: tmux
tmux: $(DOT_TMUX) ## Configure tmux

.PHONY: mail
mail: $(DOT_MAIL) ## Configure main (procmail, mutt...)

.PHONY: alacritty
alacritty: $(DOT_ALACRITTY) ## Configure alacrity terminal

.PHONY: misc
misc: $(DOT_MISC) ## Configure other tools (gdb, clang-format...)

.PHONY: all
all: zsh vim git tmux mail alacritty misc ## Configure all

$(HOME)/%: %
	ln -sf $(shell realpath $<) $@

.PHONY: update-brew-list
update-brew-list: ## Update the list of packages installed by homebrew
	brew list | sort > "$(BREW_LIST)"

.PHONY: clean
clean:
	 for dotfile in $(ALL_DOT_FILES); do
		if [[ -h $${dotfile} ]]; then
	        rm $${dotfile}
	    fi
	done

help: ## Prints help for targets with comments
	@cat $(MAKEFILE_LIST) | grep -E '^[a-zA-Z_-]+:.*?## .*$$' | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

