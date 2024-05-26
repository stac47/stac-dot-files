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

GIT := git

VIM_PLUGINS_TARGET_PREFIX := vim-plugins-for-
VIM_PLUGINS_UPDATE_TARGET := vim-plugins-update
VIM_PLUGINS_DIR := $(HOME)/.vim/pack/stac/start

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

$(VIM_PLUGINS_DIR)/terraform:
	$(GIT) clone https://github.com/hashivim/vim-terraform.git "$@"

$(VIM_PLUGINS_TARGET_PREFIX)terraform: $(VIM_PLUGINS_DIR)/terraform

$(VIM_PLUGINS_DIR)/vim-commentary:
	$(GIT) clone https://github.com/tpope/vim-commentary.git "$@"

$(VIM_PLUGINS_DIR)/syntastic:
	$(GIT) clone https://github.com/scrooloose/syntastic.git "$@"

$(VIM_PLUGINS_DIR)/vim-surround:
	$(GIT) clone https://github.com/tpope/vim-surround.git "$@"

$(VIM_PLUGINS_DIR)/vim-characterize:
	$(GIT) clone https://github.com/tpope/vim-characterize.git "$@"

$(VIM_PLUGINS_DIR)/vim-bbye:
	$(GIT) clone https://github.com/moll/vim-bbye.git "$@"

$(VIM_PLUGINS_DIR)/tlib_vim:
	$(GIT) clone https://github.com/tomtom/tlib_vim.git "$@"

$(VIM_PLUGINS_DIR)/vim-addon-mw-utils:
	$(GIT) clone https://github.com/MarcWeber/vim-addon-mw-utils.git "$@"

$(VIM_PLUGINS_DIR)/vim-snipmate:
	$(GIT) clone https://github.com/garbas/vim-snipmate.git "$@"

$(VIM_PLUGINS_DIR)/vim-snippets:
	$(GIT) clone https://github.com/honza/vim-snippets.git "$@"

$(VIM_PLUGINS_DIR)/ctrlp.vim:
	$(GIT) clone https://github.com/ctrlpvim/ctrlp.vim.git "$@"

$(VIM_PLUGINS_DIR)/vim-unimpaired:
	$(GIT) clone https://github.com/tpope/vim-unimpaired.git "$@"

$(VIM_PLUGINS_DIR)/tabular:
	$(GIT) clone https://github.com/godlygeek/tabular.git "$@"

$(VIM_PLUGINS_DIR)/vim-fugitive:
	$(GIT) clone https://github.com/tpope/vim-fugitive.git "$@"

.PHONY: vim-essential-plugins
vim-essential-plugins: $(VIM_PLUGINS_DIR)/vim-commentary \
$(VIM_PLUGINS_DIR)/syntastic \
$(VIM_PLUGINS_DIR)/vim-surround \
$(VIM_PLUGINS_DIR)/vim-characterize \
$(VIM_PLUGINS_DIR)/vim-bbye \
$(VIM_PLUGINS_DIR)/tlib_vim \
$(VIM_PLUGINS_DIR)/vim-addon-mw-utils \
$(VIM_PLUGINS_DIR)/vim-snipmate \
$(VIM_PLUGINS_DIR)/vim-snippets \
$(VIM_PLUGINS_DIR)/ctrlp.vim \
$(VIM_PLUGINS_DIR)/vim-unimpaired \
$(VIM_PLUGINS_DIR)/tabular \
$(VIM_PLUGINS_DIR)/vim-fugitive
	echo "Vim essential plugins installed"

.PHONY: $(VIM_PLUGINS_UPDATE_TARGET)
$(VIM_PLUGINS_UPDATE_TARGET):
	@for plugin in $$($(GIT) ls-files --others --exclude-standard "$(VIM_PLUGINS_DIR)"); do \
		$(GIT) -C "$${plugin}" pull; \
	done


