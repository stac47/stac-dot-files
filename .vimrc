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

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif
set autoindent        " always set autoindenting on

" Automatic reload of .vimrc file on save event.
autocmd! bufwritepost .vimrc source %

if has("gui_running")
  " GUI is running or is about to start.
  " Maximize gvim window.
  set lines=60 columns=160
  set mouse=a
  " Remove menu bar
  set guioptions-=m

  " Remove toolbar
  set guioptions-=T
else
  " This is console Vim.
  if exists("+lines")
    set lines=50
  endif
  if exists("+columns")
    set columns=160
  endif
endif

" Set <leader> to 'ù' for my AZERTY keyboard.
let mapleader="ù"

" Don't put the Vim SwaP files in the same place as the edited files.
" ... if activated of course.
if has("win32") || has("win64")
   set directory=$TMP
   set undodir=$TMP
   set backupdir=$TMP
else
   set directory=~/.vim/tmp/swap//
   set undodir=~/.vim/tmp/undo//
   set backupdir=~/.vim/tmp/backup//
   set backupskip=/tmp/*
end 

set nobackup
set history=100
set nowritebackup
set noswapfile

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

" Show  tab characters. Visual Whitespace.
"set list
"set listchars=tab:>.

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
colorscheme desert

" Small tweak for terminal mode.
highlight Search ctermbg=8 ctermfg=11
highlight Visual ctermbg=3 ctermfg=1
highlight CursorLine ctermbg=236 cterm=NONE term=NONE
highlight CursorLineNr ctermbg=236 ctermfg=240
highlight Cursor ctermbg=11 ctermfg=11
highlight LineNr ctermbg=240 ctermfg=0

" highlight the status bar when in insert mode
if version >= 700
  au InsertEnter * hi StatusLine ctermfg=235 ctermbg=5
  au InsertLeave * hi StatusLine ctermbg=2 ctermfg=235
endif


" ============================================================================
" OVERRIDE PER FILTE TYPE
" HTML (tab width 2 chr, no wrapping)
autocmd FileType html set sw=2
autocmd FileType html set ts=2
autocmd FileType html set sts=2
autocmd FileType html set textwidth=0
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags

" CSS (tab width 2 chr, wrap at 79th char)
autocmd FileType css set sw=2
autocmd FileType css set ts=2
autocmd FileType css set sts=2
autocmd FileType css set textwidth=79
autocmd FileType css set omnifunc=csscomplete#CompleteCSS

" JavaScript (tab width 4 chr, wrap at 79th)
autocmd FileType javascript set sw=4
autocmd FileType javascript set ts=4
autocmd FileType javascript set sts=4
autocmd FileType javascript set textwidth=79
autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS

" Ruby (tab width 2 chr, wrap at 79th)
autocmd FileType ruby,eruby set sw=2
autocmd FileType ruby,eruby set ts=2
autocmd FileType ruby,eruby set sts=2
autocmd FileType ruby,eruby set textwidth=79
autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1 
autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
autocmd FileType ruby,eruby let g:rubycomplete_rails = 1
autocmd FileType ruby compiler ruby

" ============================================================================

" A function to add my python headers
function! AddPythonHeader()
python << EOF
import vim
from datetime import date
header = "#!/usr/bin/python3\n"
header += "# vi:set fileencoding=utf-8 :\n\n"
header += "\"\"\"\n"
header += "Created on %s\n\n" % (date.today().isoformat())
header += "@author : Laurent Stacul\n"
header += "\"\"\""
vim.current.buffer[:] = header.split("\n") + vim.current.buffer[:]
EOF
endfunction

function! AddRubyHeader()
python << EOF
import vim
from datetime import date
header =  "#!/usr/bin/ruby -w\n"
header += "# vi:set fileencoding=utf-8 :\n"
header += "#\n"
header += "# Created on %s\n" % (date.today().isoformat())
header += "#\n"
header += "# @author : Laurent Stacul\n\n"
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
function! ValidateXml()
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
function! PrettyXml()
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

" Buffers
map <M-Left> :bprevious!<CR>
map <M-Right> :bnext!<CR>
"map <F2> :NERDTreeToggle<CR>

" Xml Pretty Print
nnoremap <F6> :call PrettyXml()<CR>
nnoremap <F7> :call ValidateXml()<CR>

" Gundo
nmap <F5> :GundoToggle<CR>

" Up and Down arrows mapping
imap <Up> gk
imap <Down> gj

" Call make with <Ctrl> + F11
map <C-F11> :make %<CR>

" E N D    K E Y  M A P P I N G
" ************************************************************************

" ************************************************************************
" P L U G I N S 

" NERDTree
" cd ~/.vim/bundle
" git clone https://github.com/scrooloose/nerdtree.git
" To hide some files in NERDTree
let NERDTreeIgnore=['\.pyc$', '\~$']
" NERDTree end.

" python-mode
" Disable python folding
let g:pymode_folding = 0

" Switch pylint, pyflakes, pep8, mccabe code-checkers
let g:pymode_lint_checker = "pyflakes,pep8,mccabe"

let pymode_rope_extended_complete=1
let pymode_rope_vim_completion=1
let pymode_rope_guess_project=0
" End python-mode

" vim-markdown plugin
let g:vim_markdown_folding_disabled=1

" Using ag with ctrlp if ag is available
if executable("ag")
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
endif


" E N D   P L U G I N S 
" ************************************************************************
