" Author: Laurent Stacul
" URL: https://stac47.github.io

set nocompatible

" ************************************************************************
" Utilitity functions

" Returns true if v1 is lower than v2
function! s:version_lower_than(v1, v2) abort
    let l:splitted_version_1 = split(a:v1, '\D\+')
    let l:splitted_version_2 = split(a:v2, '\D\+')
    let l:max_length = max([len(l:splitted_version_1), len(l:splitted_version_2)])
    for i in range(l:max_length)
        if +get(l:splitted_version_1, i, 0) == +get(l:splitted_version_2, i, 0)
            continue
        elseif +get(l:splitted_version_1, i, 0) < +get(l:splitted_version_2, i, 0)
            return 1
        else
            return 0
        endif
    endfor
endfunction

"  EDIFACT to Ascii
function! s:edifact_to_ascii()
    silent! exec '%s/\%x1d/+/ge'
    silent! exec '%s/\%x19/*/ge'
    silent! exec '%s/\%x1f/:/ge'
    silent! exec '%s/\%x1c/''\r/ge'
    normal gg
    set ft=tracer
endfunction

" End of Utility functions
" ************************************************************************

" ************************************************************************
" General configuration

filetype indent on    " Enable filetype-specific indenting
filetype plugin on    " Enable filetype-specific plugins

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif
set autoindent        " always set autoindenting on

" Automatic reload of .vimrc file on save event.
autocmd! bufwritepost .vimrc source %

" Don't put the Vim Swap files in the same place as the edited files.
" ... if activated of course.
if has("win32") || has("win64")
    let temp_dir = $TMP
else
    let temp_dir = $TMPDIR
end
set directory=temp_dir
set undodir=temp_dir
set backupdir=temp_dir
set backupskip=temp_dir

set nobackup
set history=500
set nowritebackup
set noswapfile

" Activate folding by default
set foldenable
set foldmethod=indent
set foldlevel=99

" Write automatically in case of :make
set autowrite

" Automatically read a modified file
set autoread

" Visual wrap activated my default
set wrap
" allow backspacing over everything in insert mode
set backspace=indent,eol,start
" show the cursor position all the time
set ruler
" display incomplete commands
set showcmd
" do incremental searching
set incsearch
" display line numbers on the left
set number
" displayed tab width = 4 caracters
set shiftwidth=4
set tabstop=4
" Spaces instead of tabs
set expandtab
set softtabstop=4
set textwidth=79
" Show the line where the cursor is
set cursorline
" Show the mode
set showmode
" Wild menu
set wildmenu
set wildmode=list:longest,list:full
" Set ignorecase on (set ic)
set ignorecase
" smart search (override 'ic' when pattern has uppers)
set smartcase
" Set status line
set statusline=[%02n]\ %f\ %(\[%M%R%H]%)
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%{FugitiveStatusline()}
set statusline+=%=\ %4l,%02c%2V\ [\\x%04B\ (%05b)]\ %P%*
" Always display a status line at the bottom of the window
set laststatus=2
" showmatch: Show the matching bracket for the last ')'?
set showmatch
set mps+=<:>
" allow tilde (~) to act as an operator -- ~w, etc.
set notildeop

" Complete options
set completeopt=menuone,longest,popup

" From Vim 8.1.360, more powerful options were added
" https://vimways.org/2018/the-power-of-diff/
set diffopt+=algorithm:patience,indent-heuristic

" Avoid looking for some known binaries files
set wildignore+=*.o,*.so,*.swp,*.zip,*.class,*.exe
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*        " Linux/MacOSX
set wildignore+=*\\.git\\*,*\\.hg\\*,*\\.svn\\*  " Windows ('noshellslash')

" By default spell checking language is french
set spelllang=fr

" File encoding set to UTF-8
if has("multi_byte")
 set encoding=utf-8
 setglobal fileencoding=utf-8
 "set bomb
 set termencoding=utf-8
 set fileencodings=utf-8
else
 echoerr "Sorry, this version of (g)vim was not compiled with +multi_byte"
endif

" grep settings
set grepformat=%f:%l:%c:%m,%f:%l:%m,%f:%l%m,%f\ \ %l%m
if executable("ag")
  set grepprg=ag\ --nogroup\ --column\ --nocolor\ --ignore='tags'\ --ignore='cscope.*'\ --ignore='*.rex'\ --ignore='*.res'\ $*
  set grepformat=%f:%l:%c:%m
  let g:agprg="ag --follow --column"
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
endif

" Cscope settings
if has("cscope")
    set csto=1
    set cst
    " add any database in current directory
    if filereadable("cscope.out")
        cs add cscope.out
    " else add database pointed to by environment
    elseif $CSCOPE_DB != ""
        cs add $CSCOPE_DB
    endif
    set csverb
endif

" End of general configuration
" ************************************************************************

" ************************************************************************
" Colors settings

set background=dark

" Colors settings for terminal mode.
highlight Search ctermbg=243 ctermfg=11
highlight Visual ctermbg=184 ctermfg=1
" highlight CursorLine ctermbg=236 cterm=NONE term=NONE
highlight CursorLineNr ctermbg=1 ctermfg=white
highlight LineNr ctermbg=0 ctermfg=10
highlight Pmenu ctermbg=240
highlight PmenuSel ctermbg=100 ctermfg=190
highlight SpellBad ctermbg=0 ctermfg=1
highlight StatusLineNC ctermbg=0 ctermfg=243
highlight DiffChange term=bold ctermbg=7 ctermfg=0
highlight DiffText term=reverse cterm=bold ctermbg=1 ctermfg=15

" highlight the status bar when in insert mode
if version >= 700
  au InsertEnter * hi StatusLine ctermbg=0 ctermfg=red
  au InsertLeave * hi StatusLine ctermbg=black ctermfg=white
endif

" End of colors settings
" ************************************************************************

" ************************************************************************
" Per filetype configuration

" Mail
autocmd FileType mail setlocal tw=72 noautoindent

" HTML
autocmd FileType html setlocal sw=2 ts=2 sts=2 textwidth=0 omnifunc=htmlcomplete#CompleteTags

" Python
autocmd FileType python setlocal sw=4 ts=4 sts=4 textwidth=88 sta autoindent

" CSS
autocmd FileType css setlocal sw=2 ts=2 sts=2 textwidth=79 omnifunc=csscomplete#CompleteCSS

" JavaScript
autocmd FileType javascript setlocal sw=2 ts=2 sts=2 textwidth=79 omnifunc=javascriptcomplete#CompleteJS

" TypeScript
autocmd FileType typescript setlocal sw=2 ts=2 sts=2 textwidth=79

" Markdown
autocmd FileType markdown setlocal sw=2 ts=2 sts=2 textwidth=79

" C++
autocmd FileType cpp setlocal sw=4 ts=4 sts=4 textwidth=79 foldmethod=syntax
let c_space_errors=1

"Go
autocmd FileType go setlocal omnifunc=go#complete#Complete
autocmd FileType go inoremap <buffer> . .<C-x><C-o>

" YAML
au! BufNewFile,BufReadPost *.{yaml,yml} set filetype=yaml foldmethod=indent
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab

" XML
" Folding feature activated for XML
let g:xml_syntax_folding=1
au FileType xml setlocal foldmethod=syntax

" Do not automatically fold the XML
au FileType xml setlocal foldlevel=999999

" Groovy
autocmd FileType groovy setlocal sw=4 ts=4 sts=4 textwidth=119 sta autoindent

" End of per filetype configuration
" ************************************************************************

" ************************************************************************
" Plugins configuration

" Syntastic
" Toggle off SyntasticCheck on save
let g:syntastic_mode_map = {
    \ "mode": "passive",
    \ "active_filetypes": [],
    \ "passive_filetypes": [] }

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_check_on_open=0
let g:syntastic_enable_signs=1
let g:syntastic_aggregate_errors=1
let g:syntastic_enable_balloons = 0
let g:syntastic_cursor_column = 0
let g:syntastic_debug = 0
let g:syntastic_debug_file = "~/syntastic.log"

let g:syntastic_cpp_include_dirs = ['include', 'src']
let g:syntastic_cpp_checkers = ['gcc', 'cppcheck']
let g:syntastic_cpp_compiler_options = '--std=c++14'
let g:syntastic_cpp_check_header = 0
let g:syntastic_cpp_auto_refresh_includes = 1
let g:syntastic_cpp_remove_include_errors = 1

let g:syntastic_python_checkers = ['pylint']

let g:syntastic_sh_checkers = ['shellcheck']

let g:syntastic_ruby_checkers = ['rubocop']
let g:syntastic_ruby_rubocop_exe = 'bundle exec rubocop'

let g:syntastic_yaml_checkers = ['yamllint']
" End of Syntastic

" Snippets
let g:snips_author = 'Laurent Stacul'
let g:snips_email = 'laurent.stacul@gmail.com'
let g:snips_github = 'https://github.com/stac47'
let g:snipMate = { 'snippet_version' : 0 }
" End of Snippets

" go-vim
let g:go_metalinter_command="golangci-lint"
let g:go_metalinter_enabled = [
    \'deadcode',
    \'errcheck',
    \'errname',
    \'gocritic',
    \'gosimple',
    \'gofmt',
    \'govet',
    \'ifshort',
    \'importas',
    \'ineffassign',
    \'staticcheck',
    \'structcheck',
    \'thelper',
    \'typecheck',
    \'unconvert',
    \'unparam',
    \'unused',
    \'varcheck',
    \'wastedassign',
    \'whitespace',
    \'wsl']
" End of go-vim

" Rust plugin
let g:rustfmt_autosave = 1
" End of Rust plugin

" CtrlP plugin
let g:ctrlp_arg_map = 1
" End of CtrlP plugin

" End of plugins configuration
" ************************************************************************

" ************************************************************************
" User defined functions

function! BuildIndex()
    silent !find . -type f -print | grep -E '\.(c(pp)?|h(pp)?|py|java)$' > cscope.files
    silent !cscope -b -q -i cscope.files
    silent !ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .
    cs add cscope.out
    redraw!
endfunction

function! BuildIndexRuby()
    silent !starscope
    silent !starscope -e ctags
    silent !starscope -e cscope
    redraw!
endfunction

" End of user defined functions
" ************************************************************************

" ************************************************************************
" User defined commands

command! -narg=0 Index :call BuildIndex()
command! -narg=0 IndexRuby :call BuildIndexRuby()

" End of user defined commands
" ************************************************************************

" ************************************************************************
" Key mappings

" Explorer
nnoremap <leader>e :Explore<CR>

" Pretty print
nnoremap <leader>Pj :%!jq '.'<CR>
nnoremap <leader>Px :%!XMLLINT_INDENT='    ' xmllint % --format<CR>
nnoremap <leader>Pe :call <SID>edifact_to_ascii()<CR>

" Up and Down arrows mapping
nnoremap <Up> gk
nnoremap <Down> gj

" Buffer control shortcuts
nnoremap <leader>c :Bdelete<CR>
nnoremap <leader>C :Bdelete!<CR>

" Clear the current search pattern to disable the current highlight
nnoremap <leader>h :let @/=""<CR>

" End of key mappings
" ************************************************************************

