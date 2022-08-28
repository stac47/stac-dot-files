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
# To install the set of plugins related to one of the topics listed in the
# variable VIM_PLUGINS_TARGETS, simply issue a command like:
#   make vim-plugins-<plugins set name>
#
# For example: make vim-plugins-ruby
#
VIM_PLUGINS_TARGET_PREFIX := vim-plugins-
VIM_PLUGINS_DIR := $(STAC_DOT_FILES_DIR)/.vim/pack/stac/start
VIM_PLUGINS_INSTALL_CMD_PREFIX = VIM_PLUGINS_INSTALL_CMD_

VIM_PLUGINS_FOR_PREFIX := VIM_PLUGINS_FOR_
$(VIM_PLUGINS_FOR_PREFIX)ruby := vim-ruby
$(VIM_PLUGINS_FOR_PREFIX)python := black jedi-vim
$(VIM_PLUGINS_FOR_PREFIX)bats := bats
$(VIM_PLUGINS_FOR_PREFIX)go := vim-go
$(VIM_PLUGINS_FOR_PREFIX)lilypond := vim-lilypond

VIM_PLUGINS_TARGETS := ruby python bats go lilypond

define $(VIM_PLUGINS_INSTALL_CMD_PREFIX)vim_ruby
$(GIT) clone https://github.com/vim-ruby/vim-ruby.git
endef

define $(VIM_PLUGINS_INSTALL_CMD_PREFIX)bats
$(GIT) clone https://github.com/aliou/bats.vim.git
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

define vim_plugins_template
.PHONY: $(VIM_PLUGINS_TARGET_PREFIX)$(1)
$(VIM_PLUGINS_TARGET_PREFIX)$(1): $(foreach plugin,$($(VIM_PLUGINS_FOR_PREFIX)$(1)),$(VIM_PLUGINS_DIR)/$(plugin))

endef

$(foreach target,$(VIM_PLUGINS_TARGETS),$(eval $(call vim_plugins_template,$(target))))

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

