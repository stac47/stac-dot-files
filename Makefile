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
DOT_MISC := $(HOME)/.gdbinit $(HOME)/.clang-format $(HOME)/.mdlrc
DOT_RUBY := $(HOME)/.rubocop.yml $(HOME)/.irbrc

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

.PHONY: ruby
ruby: $(DOT_RUBY) ## Configure Ruby specific tools

.PHONY: all
all: zsh vim git tmux mail alacritty misc ruby ## Configure all

$(HOME)/%: %.template
	m4 -D STAC_DOT_FILES_DIR=$(STAC_DOT_FILES_DIR) $(shell realpath $<) > $@

$(HOME)/%: %
	ln -sf $(shell realpath $<) $@

# Vim plugins management (except plugins that are submodules)
# To install the set of plugins related to programming languages I use.
#
#   make vim-plugins-for-<programming language>
#
# For example: make vim-plugins-for-ruby
#
# To update all the plugins:
#
#   make vim-plugins-update

VIM_PLUGINS_TARGET_PREFIX := vim-plugins-for-
VIM_PLUGINS_UPDATE_TARGET := vim-plugins-update
VIM_PLUGINS_DIR := $(STAC_DOT_FILES_DIR)/.vim/pack/stac/start

$(VIM_PLUGINS_DIR)/rust.vim:
	$(GIT) clone https://github.com/rust-lang/rust.vim.git "$@"

$(VIM_PLUGINS_TARGET_PREFIX)rust: $(VIM_PLUGINS_DIR)/rust.vim

$(VIM_PLUGINS_DIR)/vim-ruby:
	$(GIT) clone https://github.com/vim-ruby/vim-ruby.git "$@"

$(VIM_PLUGINS_TARGET_PREFIX)ruby: $(VIM_PLUGINS_DIR)/vim-ruby

$(VIM_PLUGINS_DIR)/bats.vim:
	$(GIT) clone https://github.com/aliou/bats.vim.git "$@"

$(VIM_PLUGINS_TARGET_PREFIX)bats: $(VIM_PLUGINS_DIR)/bats.vim

$(VIM_PLUGINS_DIR)/jedi-vim:
	$(GIT) clone --recursive \
		https://github.com/davidhalter/jedi-vim.git "$@"

$(VIM_PLUGINS_DIR)/black:
	$(GIT) clone https://github.com/psf/black.git "$@"

$(VIM_PLUGINS_TARGET_PREFIX)python: $(VIM_PLUGINS_DIR)/jedi-vim $(VIM_PLUGINS_DIR)/black

$(VIM_PLUGINS_DIR)/vim-go:
	$(GIT) clone https://github.com/fatih/vim-go.git "$@"

$(VIM_PLUGINS_TARGET_PREFIX)go: $(VIM_PLUGINS_DIR)/vim-go

$(VIM_PLUGINS_DIR)/vim-lilypond:
	$(GIT) clone https://github.com/sersorrel/vim-lilypond.git "$@"

.PHONY: $(VIM_PLUGINS_UPDATE_TARGET)
$(VIM_PLUGINS_UPDATE_TARGET):
	@for plugin in $$($(GIT) ls-files --others --exclude-standard "$(VIM_PLUGINS_DIR)"); do \
		$(GIT) -C "$${plugin}" pull; \
	done

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

