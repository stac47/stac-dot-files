# Technical prelude
# See: https://tech.davis-hansson.com/p/make/
SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

DOT_MAIL := $(HOME)/.procmailrc $(HOME)/.muttrc $(HOME)/.mutt $(HOME)/.mailcap

# Quick all setup
ALL_DOT_FILES := $(DOT_MAIL)

.PHONY: mail
mail: $(DOT_MAIL) ## Configure main (procmail, mutt...)

.PHONY: all
all: mail ## Configure all

$(HOME)/%: %
	ln -sf $(shell realpath $<) $@

include vim-plugins.mk
