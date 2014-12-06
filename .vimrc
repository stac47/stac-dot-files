set nocompatible

" Temporary for pathogen load
filetype off

" pathogen plugin allows to have a plugin inside a single folder.
" mkdir -p ~/.vim/autoload ~/.vim/bundle
" curl -Sso ~/.vim/autoload/pathogen.vim https://raw.github.com/tpope/vim-pathogen/master/autoload/pathogen.vim
call pathogen#infect()
call pathogen#helptags()

filetype on           " Enable filetype detection

filetype indent on    " Enable filetype-specific indenting
filetype plugin on    " Enable filetype-specific plugins

let mapleader="-"

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
set wildmode=list:longest,list
" Set ignorecase on (set ic)
set ignorecase
" smart search (override 'ic' when pattern has uppers)
set scs
" Set status line
set statusline=[%02n]\ %f\ %(\[%M%R%H]%)%=\ %4l,%02c%2V\ %P%*
" Always display a status line at the bottom of the window
set laststatus=2
" showmatch: Show the matching bracket for the last ')'?
set showmatch
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

" I like this colors
set background=dark
colorscheme desert

" Small tweak for terminal mode.
highlight Search ctermbg=243 ctermfg=11
highlight Visual ctermbg=184 ctermfg=1
highlight CursorLine ctermbg=236 cterm=NONE term=NONE
highlight CursorLineNr ctermbg=1 ctermfg=white
highlight Cursor ctermbg=225 ctermfg=225
highlight LineNr ctermbg=0 ctermfg=10
highlight Pmenu ctermbg=240
highlight PmenuSel ctermbg=100 ctermfg=190
highlight SpellBad ctermbg=0 ctermfg=1

" highlight the status bar when in insert mode
if version >= 700
  au InsertEnter * hi StatusLine ctermbg=0 ctermfg=red
  au InsertLeave * hi StatusLine ctermbg=black ctermfg=white
endif

" ============================================================================
" OVERRIDE PER FILE TYPE
" HTML
autocmd FileType html set sw=2
autocmd FileType html set ts=2
autocmd FileType html set sts=2
autocmd FileType html set textwidth=0
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags

" Python
autocmd FileType python set sw=4
autocmd FileType python set ts=4
autocmd FileType python set sts=4
autocmd FileType python set textwidth=79
autocmd FileType python set sta
autocmd FileType python set autoindent
autocmd BufRead python set smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class
autocmd BufWritePre python set normal m`:%s/\s\+$//e ``

" Django Html template
autocmd FileType htmldjango set sw=2
autocmd FileType htmldjango set ts=2
autocmd FileType htmldjango set sts=2
autocmd FileType htmldjango set textwidth=0
autocmd FileType htmldjango set omnifunc=htmlcomplete#CompleteTags

" CSS
autocmd FileType css set sw=2
autocmd FileType css set ts=2
autocmd FileType css set sts=2
autocmd FileType css set textwidth=79
autocmd FileType css set omnifunc=csscomplete#CompleteCSS

" JavaScript
autocmd FileType javascript set sw=2
autocmd FileType javascript set ts=2
autocmd FileType javascript set sts=2
autocmd FileType javascript set textwidth=79
autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS

" Markdown
autocmd FileType mkd set textwidth=79


" APT Maven Site format
autocmd FileType apt set sw=2
autocmd FileType apt set ts=2
autocmd FileType apt set sts=2
autocmd FileType apt set textwidth=79

" C++
autocmd FileType cpp set sw=4
autocmd FileType cpp set ts=4
autocmd FileType cpp set sts=4
autocmd FileType cpp set textwidth=79
"autocmd FileType cpp set omnifunc=omni#cpp#complete#Main
autocmd FileType cpp set foldmethod=syntax

" Ruby
autocmd FileType ruby,eruby set sw=2
autocmd FileType ruby,eruby set ts=2
autocmd FileType ruby,eruby set sts=2
autocmd FileType ruby,eruby set textwidth=79
autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1 
autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
autocmd FileType ruby,eruby let g:rubycomplete_rails = 1
autocmd FileType ruby compiler ruby
" ============================================================================

" C++ support
function! CppInsertGates()
  let gatename = "__" . substitute(toupper(expand("%:t")), "\\.", "_", "g") . "__"
  execute "normal! i#ifndef " . gatename
  execute "normal! o#define " . gatename . " "
  execute "normal! Go#endif /* " . gatename . " */"
  normal! kk
endfunction

" ============================================================================

" Indexing a py|cpp project
function! BuildIndex()
    echom "File indexing started..."
    silent !find -L . -type f -print | grep -E '\.(c(pp)?|h(pp)?|py)$' > cscope.files
    silent !cscope -b -q -i cscope.files
    silent !ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .
    redraw!
    echom "File indexing finished"
endfunction

" Python support
function! PythonAddHeader()
python << EOF
import vim
from datetime import date
header = "#!/usr/bin/env python3\n"
header += "# vi:set fileencoding=utf-8 :\n\n"
header += "\"\"\"\n"
header += "Created on %s\n\n" % (date.today().isoformat())
header += "@author : Laurent Stacul\n"
header += "\"\"\""
vim.current.buffer[:] = header.split("\n") + vim.current.buffer[:]
EOF
endfunction

"================XML==========================
" Folding feature activated for XML
let g:xml_syntax_folding=1
au FileType xml setlocal foldmethod=syntax

" Do not automatically fold the XML
au FileType xml setlocal foldlevel=999999

" Python function to validate a XML file (faster than vim script)
function! XmlValidate()
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
"===========End of XML==========================


" ************************************************************************
" K E Y   M A P P I N G S

" Moving between buffers
nnoremap <F2> :MBEbp<CR>
nnoremap <F3> :MBEbn<CR>

" Xml Pretty Print
nnoremap <F4> :cscope find c <C-r><C-w><CR>
nnoremap <F5> :NERDTreeToggle<CR>
nnoremap <F6> :TagbarToggle<CR>
nnoremap <F7> :call <SID>PrettyXml()<CR>
nnoremap <F8> :%!python -m json.tool<CR>:w<CR>

" Up and Down arrows mapping
nnoremap <Up> gk
nnoremap <Down> gj

" Redraw the screen after removing the highlight search elements
" Remap CTRL-l to CTRL-x to be reused in windows navigation.
" nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>
nnoremap <silent> <c-x> :nohlsearch<CR><c-l>

" Mapping with leader
nnoremap <leader>c :MBEbw<CR>

" Easy move between each window
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l
nnoremap <c-h> <c-w>h

" E N D    K E Y  M A P P I N G
" ************************************************************************

" ************************************************************************
" P L U G I N S 

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
" End of Cscope configuration

" NERDTree
let NERDTreeIgnore=['\.pyc$', '\~$']
" NERDTree end.

" vim-markdown plugin: no folding
let g:vim_markdown_folding_disabled=1

" Ctrlp
" Activate caching in ~/.cache/ctrlp
let g:ctrlp_use_caching=1
" Not removing the cache on exit
let g:ctrlp_clear_cache_on_exit=0
" Use the WD where vim was opened
let g:ctrlp_working_path_mode='rw'
" Display of propositions
let g:ctrlp_match_window='bottom,order:btt,min:1,max:10,results:20'
" Using ag with ctrlp if ag is available
if executable("ag")
  let g:agprg="ag --follow --column"
  set grepprg=ag\ --nogroup\ --column\ --nocolor\ --ignore\ 'tags'\ --ignore\ 'cscope.*'\ $*
  set grepformat=%f:%l:%c:%m
  let g:ctrlp_user_command='ag %s -l --nocolor --follow -g ""'
endif
" Follow the symbolic links in case ag not available on this system
let g:ctrlp_follow_symlinks=2
" End of Ctrlp

" minibuffexpl.vim
" Cycle on the buffers.
let g:miniBufExplCycleArround = 1
" End ofminibuffexpl.vim

" Syntastic
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_check_on_open=0
let g:syntastic_enable_signs=1
let g:syntastic_aggregate_errors=1
let g:syntastic_enable_balloons = 0
let g:syntastic_cursor_column = 0

let g:syntastic_cpp_include_dirs = ['include', 'src']
let g:syntastic_cpp_checkers = ['gcc', 'cppcheck']
let g:syntastic_cpp_check_header = 0
let g:syntastic_cpp_auto_refresh_includes = 1
let g:syntastic_cpp_remove_include_errors = 1

let g:syntastic_python_checkers = ['pyflakes', 'python']
" End of Syntastic

" DoxygenToolkit
let g:DoxygenToolkit_authorName="Laurent Stacul"
let g:doxygentoolkit_commentType = "C++"
let g:DoxygenToolkit_blockHeader = ""
let g:DoxygenToolkit_blockFooter = ""

" End of DoxygenToolkit

" E N D   P L U G I N S 
" ************************************************************************
