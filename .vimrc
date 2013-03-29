set nocompatible

" Automatic reload of .vimrc file on save event.
autocmd! bufwritepost .vimrc source %

" I like this colors
colorscheme desert

" Temporary for pathogen load
filetype off

" pathogen plugin allows to have a plugin inside a single folder.
" mkdir -p ~/.vim/autoload ~/.vim/bundle
" curl -Sso ~/.vim/autoload/pathogen.vim https://raw.github.com/tpope/vim-pathogen/master/autoload/pathogen.vim
call pathogen#infect()
call pathogen#helptags()

filetype plugin indent on

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
    set columns=100
  endif
endif

" Don't put the Vim SwaP files in the same place as the edited files.
if has("win32") || has("win64")
   set directory=$TMP
else
   set directory=/tmp
end 

" allow backspacing over everything in insert mode
set backspace=indent,eol,start
" do not keep a backup file, use versions instead
set nobackup        
set nowritebackup
" keep 50 lines of command line history
set history=50
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

" ============================================================================
" OVERRIDE PER FILTE TYPE
" HTML (tab width 2 chr, no wrapping)
autocmd FileType html set sw=2
autocmd FileType html set ts=2
autocmd FileType html set sts=2
autocmd FileType html set textwidth=0
" Python (tab width 4 chr, wrap at 79th char)
"autocmd FileType python set sw=4
"autocmd FileType python set ts=4
"autocmd FileType python set sts=4
"autocmd FileType python set textwidth=79
" autocmd FileType python set foldlevel=0
" autocmd FileType python set foldnestmax=1
" CSS (tab width 2 chr, wrap at 79th char)
autocmd FileType css set sw=2
autocmd FileType css set ts=2
autocmd FileType css set sts=2
autocmd FileType css set textwidth=79
" JavaScript (tab width 4 chr, wrap at 79th)
autocmd FileType javascript set sw=4
autocmd FileType javascript set ts=4
autocmd FileType javascript set sts=4
autocmd FileType javascript set textwidth=79

" autocmd FileType python set omnifunc=pythoncomplete#Complete
autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
autocmd FileType css set omnifunc=csscomplete#CompleteCSS
" ============================================================================

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif
set autoindent        " always set autoindenting on


" A function to add my python headers
function! AddPythonHeader()
python << EOF
import vim
from datetime import date
header = "#!/usr/bin/python3\n"
header += "# -*- coding: utf-8 -*-\n\n"
header += "\"\"\"\n"
header += "Created on %s\n\n" % (date.today().isoformat())
header += "@author : Laurent Stacul\n"
header += "\"\"\""
vim.current.buffer[:] = header.split("\n") + vim.current.buffer[:]
EOF
endfunction

"================XML==========================
let g:xml_syntax_folding=1
au FileType xml setlocal foldmethod=syntax

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
nnoremap <silent> <F5> :call PrettyXml()<CR>
nnoremap <silent> <F6> :call ValidateXml()<CR>

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
" cd ~/.vim/bundle
" git clone git://github.com/klen/python-mode.git
" Enable python folding
let g:pymode_folding = 0

" Switch pylint, pyflakes, pep8, mccabe code-checkers
" Can have multiply values "pep8,pyflakes,mcccabe"
let g:pymode_lint_checker = "pyflakes,pep8,mccabe"

let pymode_rope_extended_complete=1
let pymode_rope_vim_completion=1
let pymode_rope_guess_project=0
" End python-mode

" minibuffexpl
" cd ~/.vim/bundle
" git clone https://github.com/fholgado/minibufexpl.vim.git
" End minibuffexpl

" E N D   P L U G I N S 
" ************************************************************************
