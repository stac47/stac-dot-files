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

$(VIM_PLUGINS_TARGET_PREFIX)lilypond: $(VIM_PLUGINS_DIR)/vim-lilypond

.PHONY: $(VIM_PLUGINS_UPDATE_TARGET)
$(VIM_PLUGINS_UPDATE_TARGET):
	@for plugin in $$($(GIT) ls-files --others --exclude-standard "$(VIM_PLUGINS_DIR)"); do \
		$(GIT) -C "$${plugin}" pull; \
	done


