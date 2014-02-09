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

" Set <Leader> to 'ù' for my AZERTY keyboard.
let mapleader="ù"

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
header =  "#!/usr/bin/env ruby -w\n"
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
nmap <silent> <S-Tab> :MBEbn<CR>
"nmap <silent> <Tab> :MBEbp<CR>
"map <F2> :NERDTreeToggle<CR>

" Xml Pretty Print
nnoremap <F6> :call PrettyXml()<CR>
nnoremap <F7> :call ValidateXml()<CR>

" Gundo
nmap <F2> :GundoToggle<CR>

" Up and Down arrows mapping
imap <Up> gk
imap <Down> gj

" Call make with <Ctrl> + F11
map <F3> :make %<CR>

" Redraw the screen after removing the highlight search elements
nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>

" E N D    K E Y  M A P P I N G
" ************************************************************************

" ************************************************************************
" P L U G I N S 

" NERDTree
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

" minibuffexpl.vim
let g:miniBufExplCycleArround = 1

" E N D   P L U G I N S 
" ************************************************************************
