set nocompatible

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

" I like this colors
set background=dark
colorscheme desert

" Colors settings for terminal mode.
highlight Search ctermbg=243 ctermfg=11
highlight Visual ctermbg=184 ctermfg=1
highlight CursorLine ctermbg=236 cterm=NONE term=NONE
highlight CursorLineNr ctermbg=1 ctermfg=white
highlight Cursor ctermbg=225 ctermfg=225
highlight LineNr ctermbg=0 ctermfg=10
highlight Pmenu ctermbg=240
highlight PmenuSel ctermbg=100 ctermfg=190
highlight SpellBad ctermbg=0 ctermfg=1

" Highlight the tabs in any file (they should be banned).
highlight ShowTab ctermbg=red guibg=red

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
autocmd BufWinEnter html match ShowTab /\t/

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
autocmd BufWinEnter htmldjango match ShowTab /\t/

" CSS
autocmd FileType css set sw=2
autocmd FileType css set ts=2
autocmd FileType css set sts=2
autocmd FileType css set textwidth=79
autocmd FileType css set omnifunc=csscomplete#CompleteCSS
autocmd BufWinEnter css match ShowTab /\t/


" JavaScript
autocmd FileType javascript set sw=2
autocmd FileType javascript set ts=2
autocmd FileType javascript set sts=2
autocmd FileType javascript set textwidth=79
autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
autocmd BufWinEnter JavaScript match ShowTab /\t/

" Markdown
autocmd FileType mkd set textwidth=79
autocmd BufWinEnter mkd match ShowTab /\t/

" C++
autocmd FileType cpp set sw=4
autocmd FileType cpp set ts=4
autocmd FileType cpp set sts=4
autocmd FileType cpp set textwidth=79
autocmd FileType cpp set foldmethod=syntax
let c_space_errors=1
autocmd BufWinEnter cpp match ShowTab /\t/

"  EDIFACT to Ascii
function! s:EdiToAscii()
    silent! exec '%s/\%x1d/+/ge'
    silent! exec '%s/\%x19/*/ge'
    silent! exec '%s/\%x1f/:/ge'
    silent! exec '%s/\%x1c/''\r/ge'
    normal gg
    set ft=tracer
endfunction

" Indexing a py|cpp project
function! BuildIndex()
    echom "File indexing started..."
    silent !find -L . -type f -print | grep -E '\.(c(pp)?|h(pp)?|py)$' > cscope.files
    silent !cscope -b -q -i cscope.files
    silent !ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .
    redraw!
    echom "File indexing finished"
endfunction

"================XML==========================
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
"===========End of XML==========================


" ************************************************************************
" K E Y   M A P P I N G S

" Moving between buffers
nnoremap <F2> :bp<CR>
nnoremap <F3> :bn<CR>

nnoremap <F4> :cs find 0 <C-R><C-W><CR>
nnoremap <F7> :TagbarToggle<CR>
nnoremap <F8> :call <SID>PrettyXml()<CR>
nnoremap <F9> :%!python -m json.tool<CR>
nnoremap <F10> :call <SID>EdiToAscii()<CR>

" Up and Down arrows mapping
nnoremap <Up> gk
nnoremap <Down> gj

" Mapping with leader
nnoremap <leader>c :Bdelete<CR>
nnoremap <leader>C :Bdelete!<CR>

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

" DoxygenToolkit
let g:DoxygenToolkit_authorName="Laurent Stacul"
let g:doxygentoolkit_commentType = "C++"
let g:DoxygenToolkit_blockHeader = ""
let g:DoxygenToolkit_blockFooter = ""
" End of DoxygenToolkit

" The silver searcher
if executable("ag")
  set grepprg=ag\ --nogroup\ --column\ --nocolor\ --ignore='tags'\ --ignore='cscope.*'\ --ignore='*.rex'\ --ignore='*.res'\ $*
  set grepformat=%f:%l:%c:%m
  let g:agprg="ag --follow --column"
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
  " Use ag in unite grep source.
  let g:unite_source_grep_command = 'ag'
  let g:unite_source_grep_default_opts =
  \ '-i --nocolor --nogroup --hidden --ignore ''.hg'' ' .
  \ '--ignore ''.svn'' --ignore ''.git'' --ignore ''.bzr'' ' .
  \ '--ignore ''cscope.*'' --ignore ''tags'' --ignore ''*.log'' ' .
  \ '--ignore-dir="*.res" --ignore-dir="*.rex" '
  let g:unite_source_grep_recursive_opt = ''
endif
" End of Silver searcher

" Unite
let g:unite_source_history_yank_enable = 1
let g:unite_source_rec_max_cache_files = 0

" Unite
nnoremap <leader>b :<C-u>Unite -buffer-name=buffers buffer<cr>
" End of Unite

" VimFiler
let g:vimfiler_safe_mode_by_default = 0
let g:vimfiler_force_overwrite_statusline = 0
let g:vimfiler_as_default_explorer = 1

let g:vimfiler_tree_leaf_icon = ' '
let g:vimfiler_tree_opened_icon = '▾'
let g:vimfiler_tree_closed_icon = '▸'
let g:vimfiler_file_icon = '-'
let g:vimfiler_readonly_file_icon = '✗'
let g:vimfiler_marked_file_icon = '✓'
" End of VimFiler

" Snippets
let g:snips_author = 'Laurent Stacul'
let g:snips_email = 'laurent.stacul@gmail.com'
let g:snips_github = 'https://github.com/stac47'
" End of Snippets

" E N D   P L U G I N S 
" ************************************************************************
