# Technical prelude
# See: https://tech.davis-hansson.com/p/make/
SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

GIT := git

DOT_VIM := $(HOME)/.vimrc $(HOME)/.vim
DOT_MAIL := $(HOME)/.procmailrc $(HOME)/.muttrc $(HOME)/.mutt $(HOME)/.mailcap

# Quick all setup
ALL_DOT_FILES := $(DOT_VIM) $(DOT_MAIL)

.PHONY: vim
vim: $(DOT_VIM) ## Configure vim

.PHONY: mail
mail: $(DOT_MAIL) ## Configure main (procmail, mutt...)

.PHONY: all
all: vim mail ## Configure all

$(HOME)/%: %.$(shell uname | tr '[:upper:]' '[:lower:]')
	ln -sf $(shell realpath $<) $@

$(HOME)/%: %
	ln -sf $(shell realpath $<) $@

include vim-plugins.mk
