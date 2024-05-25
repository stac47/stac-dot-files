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

DOT_VIM := $(HOME)/.vimrc $(HOME)/.vim
DOT_MAIL := $(HOME)/.procmailrc $(HOME)/.muttrc $(HOME)/.mutt $(HOME)/.mailcap
DOT_ALACRITTY := $(HOME)/.alacritty.toml $(HOME)/.alacritty_common.toml
DOT_RUBY := $(HOME)/.irbrc

# Quick all setup
ALL_DOT_FILES := $(DOT_VIM) $(DOT_MAIL)

.PHONY: vim
vim: $(DOT_VIM) ## Configure vim

.PHONY: mail
mail: $(DOT_MAIL) ## Configure main (procmail, mutt...)

.PHONY: alacritty
alacritty: $(DOT_ALACRITTY) ## Configure alacrity terminal

.PHONY: ruby
ruby: $(DOT_RUBY) ## Configure Ruby specific tools

.PHONY: all
all: vim mail alacritty ruby ## Configure all

$(HOME)/%: %.template
	m4 -D STAC_DOT_FILES_DIR=$(STAC_DOT_FILES_DIR) $(shell realpath $<) > $@

$(HOME)/%: %.$(shell uname | tr '[:upper:]' '[:lower:]')
	ln -sf $(shell realpath $<) $@

$(HOME)/%: %
	ln -sf $(shell realpath $<) $@

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

include vim-plugins.mk
