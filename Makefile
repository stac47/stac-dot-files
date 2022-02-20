# Technical prelude
# See: https://tech.davis-hansson.com/p/make/
SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

GIT := git

STAC_DOT_FILES_DIR := $(shell pwd)

DOT_ZSH := $(HOME)/.zshrc $(HOME)/.zshenv
DOT_VIM := $(HOME)/.vimrc $(HOME)/.vim
DOT_GIT := $(HOME)/.gitconfig $(HOME)/.gitignore_global
DOT_TMUX := $(HOME)/.tmux $(HOME)/.tmux.conf
DOT_MAIL := $(HOME)/.procmailrc $(HOME)/.muttrc $(HOME)/.mutt $(HOME)/.mailcap
DOT_ALACRITTY := $(HOME)/.alacritty.yml
DOT_MISC := $(HOME)/.gdbinit $(HOME)/.clang-format

BREW_LIST := brew.list

# Quick all setup
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

$(HOME)/%: %.template
	m4 -D STAC_DOT_FILES_DIR=$(STAC_DOT_FILES_DIR) $(shell realpath $<) > $@

$(HOME)/%: %
	ln -sf $(shell realpath $<) $@

# Vim plugin management
VIM_PLUGINS_TARGET_PREFIX := vim-plugins-
VIM_PLUGINS_DIR := $(STAC_DOT_FILES_DIR)/.vim/pack/stac/start

VIM_PLUGINS_INSTALL_CMD_PREFIX = VIM_PLUGINS_INSTALL_CMD_

define $(VIM_PLUGINS_INSTALL_CMD_PREFIX)vim_ruby
$(GIT) clone git://github.com/vim-ruby/vim-ruby.git
endef

define $(VIM_PLUGINS_INSTALL_CMD_PREFIX)jedi_vim
$(GIT) clone --recursive \
	https://github.com/davidhalter/jedi-vim.git
endef

define $(VIM_PLUGINS_INSTALL_CMD_PREFIX)black
$(GIT) clone https://github.com/psf/black.git
endef

define $(VIM_PLUGINS_INSTALL_CMD_PREFIX)vim_go
$(GIT) clone https://github.com/fatih/vim-go.git
endef

define $(VIM_PLUGINS_INSTALL_CMD_PREFIX)vim_lilypond
git clone https://github.com/sersorrel/vim-lilypond.git
endef

.PHONY: $(VIM_PLUGINS_TARGET_PREFIX)ruby
$(VIM_PLUGINS_TARGET_PREFIX)ruby: $(VIM_PLUGINS_DIR)/vim-ruby ## Install vim plugins for ruby

.PHONY: $(VIM_PLUGINS_TARGET_PREFIX)python
$(VIM_PLUGINS_TARGET_PREFIX)python: $(VIM_PLUGINS_DIR)/black $(VIM_PLUGINS_DIR)/jedi-vim ## Install vim plugins for python

.PHONY: $(VIM_PLUGINS_TARGET_PREFIX)go
$(VIM_PLUGINS_TARGET_PREFIX)go: $(VIM_PLUGINS_DIR)/vim-go ## Install vim plugins for go

.PHONY: $(VIM_PLUGINS_TARGET_PREFIX)lilypond
$(VIM_PLUGINS_TARGET_PREFIX)lilypond: $(VIM_PLUGINS_DIR)/vim-lilypond ## Install vim plugins for lilypond

$(VIM_PLUGINS_DIR)/%:
	$($(VIM_PLUGINS_INSTALL_CMD_PREFIX)$(shell basename "$@" | sed 's/-/_/g')) "$@"

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
	@cat $(MAKEFILE_LIST) | grep -E '^[a-zA-Z_-]+:.*?## .*$$' | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

