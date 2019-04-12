set nocompatible

" ************************************************************************
" Utilitity functions

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
set sw=4
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
set wildmode=list:full
" Set ignorecase on (set ic)
set ignorecase
" smart search (override 'ic' when pattern has uppers)
set scs
" Set status line
set statusline=[%02n]\ %f\ %(\[%M%R%H]%)
set statusline+=%=\ %4l,%02c%2V\ [\\x%04B\ (%05b)]\ %P%*
" Always display a status line at the bottom of the window
set laststatus=2
" showmatch: Show the matching bracket for the last ')'?
set showmatch
set mps+=<:>
" allow tilde (~) to act as an operator -- ~w, etc.
set notildeop

" Avoid looking for some known binaries files
set wildignore+=*.o,*.so,*.swp,*.zip,*.class,*.exe
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*        " Linux/MacOSX
set wildignore+=*\\.git\\*,*\\.hg\\*,*\\.svn\\*  " Windows ('noshellslash')

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
colorscheme desert

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

" Highlight bad spaces that should be removed from a code file
"   - the tabs (they should be banned)
"   - the trailing spaces
highlight BadSpaces ctermbg=red guibg=red
autocmd FileType * match BadSpaces /\s\+$/

" highlight the status bar when in insert mode
if version >= 700
  au InsertEnter * hi StatusLine ctermbg=0 ctermfg=red
  au InsertLeave * hi StatusLine ctermbg=black ctermfg=white
endif

" End of colors settings
" ************************************************************************

" ************************************************************************
" Per filetype configuration

" HTML
autocmd FileType html setlocal sw=2 ts=2 sts=2 textwidth=0 omnifunc=htmlcomplete#CompleteTags
autocmd FileType html match BadSpaces /\t/

" Python
autocmd FileType python setlocal sw=4 ts=4 sts=4 textwidth=79 sta autoindent
autocmd FileType python match BadSpaces /\t/

" CSS
autocmd FileType css setlocal sw=2 ts=2 sts=2 textwidth=79 omnifunc=csscomplete#CompleteCSS
autocmd FileType css match BadSpaces /\t/

" JavaScript
autocmd FileType javascript setlocal sw=2 ts=2 sts=2 textwidth=79 omnifunc=javascriptcomplete#CompleteJS
autocmd FileType javascript match BadSpaces /\t/

" Markdown
autocmd FileType mkd setlocal textwidth=79
autocmd FileType mkd match BadSpaces /\t/

" C++
autocmd FileType cpp setlocal sw=4 ts=4 sts=4 textwidth=79 foldmethod=syntax
autocmd FileType cpp match BadSpaces /\t/
let c_space_errors=1

" YAML
au! BufNewFile,BufReadPost *.{yaml,yml} set filetype=yaml foldmethod=indent
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
autocmd FileType yaml match BadSpaces /\t/

"  EDIFACT to Ascii
function! s:EdiToAscii()
    silent! exec '%s/\%x1d/+/ge'
    silent! exec '%s/\%x19/*/ge'
    silent! exec '%s/\%x1f/:/ge'
    silent! exec '%s/\%x1c/''\r/ge'
    normal gg
    set ft=tracer
endfunction

" XML
" Folding feature activated for XML
let g:xml_syntax_folding=1
au FileType xml setlocal foldmethod=syntax

" Do not automatically fold the XML
au FileType xml setlocal foldlevel=999999

" Python function to validate a XML file (faster than vim script)
function! s:XmlValidate()
py << EOF
import vim
import re
from xml.dom.minidom import parseString
from xml.parsers.expat import ExpatError
from xml.parsers.expat import ErrorString
sXml = "\n".join(vim.current.buffer)
try:
    xml = parseString(sXml)
except ExpatError as e:
    # Jump to the invalid line
    vim.command(str(e.lineno))
    print(e)
else:
    print("Valid XML")
EOF
endfunction

" Python function to pretty print an XML
function! s:XmlPretty()
py << EOF
import vim
import re
from xml.dom.minidom import parseString

def removeSpace(s):
    xmlLine = s.strip()
    if not xmlLine.startswith("<"):
        xmlLine = " " + xmlLine
    return xmlLine

sXml = "".join(map(removeSpace,vim.current.buffer))
try:
    xml = parseString(sXml)
    s = xml.toprettyxml(indent = "  ", encoding = "UTF-8")
    RE_PATTERN = re.compile('>\n\s+([^<>\s].*?)\n\s+</', re.DOTALL)
    prettyXml = RE_PATTERN.sub(">\g<1></", s)
    #prettyXml = s
    vim.current.buffer[:] = prettyXml.split("\n")
except Exception as e:
    print(e)
EOF
endfunction
" End of XML

" End of per filetype configuration
" ************************************************************************


" ************************************************************************
" Key mappings

" Moving between buffers
nnoremap <F2> :bp<CR>
nnoremap <F3> :bn<CR>

nnoremap <F4> :cs find 0 <C-R><C-W><CR>
nnoremap <F6> :Explore<CR>
nnoremap <F8> :call <SID>PrettyXml()<CR>
nnoremap <F9> :%!python -m json.tool<CR>
nnoremap <F10> :call <SID>EdiToAscii()<CR>

" Up and Down arrows mapping
nnoremap <Up> gk
nnoremap <Down> gj

" Mapping with leader
nnoremap <leader>c :Bdelete<CR>
nnoremap <leader>C :Bdelete!<CR>
nnoremap <leader>b :ls<CR>

" End of key mappings
" ************************************************************************

" ************************************************************************
" Plugins configuration

" vim-markdown plugin: no folding
let g:vim_markdown_folding_disabled=1

" Syntastic
let g:syntastic_always_populate_loc_list = 0
let g:syntastic_check_on_open=0
let g:syntastic_enable_signs=1
let g:syntastic_aggregate_errors=1
let g:syntastic_enable_balloons = 0
let g:syntastic_cursor_column = 0

let g:syntastic_cpp_include_dirs = ['include', 'src']
let g:syntastic_cpp_checkers = ['gcc', 'cppcheck']
let g:syntastic_cpp_compiler_options = '--std=c++14'
let g:syntastic_cpp_check_header = 0
let g:syntastic_cpp_auto_refresh_includes = 1
let g:syntastic_cpp_remove_include_errors = 1

let g:syntastic_python_checkers = ['pyflakes', 'python']
" End of Syntastic

" Snippets
let g:snips_author = 'Laurent Stacul'
let g:snips_email = 'laurent.stacul@gmail.com'
let g:snips_github = 'https://github.com/stac47'
" End of Snippets

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

function! GitGrep(arg) abort
	if !executable("git")
        echom 'Git is not available on your system'
        return
    endif
    let l:git_version = split(system('git --version'))[2]
    let l:git_result = system('git rev-parse')
    if v:shell_error
        echom 'The current working directory is not a Git repository'
        return
    endif
    try
        let l:grepprg = &grepprg
        let l:grepformat = &grepformat
        let &grepformat = '%f:%l:%c:%m,%f:%l:%m,%m %f match%ts,%f'
        let &grepprg='git --no-pager grep -n --no-color'
        if s:version_lower_than('2.19', l:git_version)
            let &grepprg .= ' --column'
        endif
        " exe 'grep! '.escape(a:arg, '|#%')
        exe 'grep! '.a:arg
    finally
        let &grepprg = l:grepprg
        let &grepformat = l:grepformat
    endtry
endfunc

" End of user defined functions
" ************************************************************************

" ************************************************************************
" User defined commands

command! -narg=1 Grep :call GitGrep(<q-args>)
command! -narg=0 Index :call BuildIndex()

" End of user defined commands
" ************************************************************************
