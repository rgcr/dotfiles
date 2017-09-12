"set nocompatible
filetype off

if has("nvim")
    call plug#begin('~/.local/share/nvim/plugged')
else
    call plug#begin('~/.vim/plugged')
end

""""""""""""""""""""""""""""""""""""""""""""""
"               PLUGINS
""""""""""""""""""""""""""""""""""""""""""""""

" Themes,colors & status lines
"Plug 'bling/vim-airline'                  " statusline
"Plug 'vim-airline/vim-airline-themes'     " themes for statusline
Plug 'itchyny/lightline.vim'
Plug 'kshenoy/vim-signature'              " show marks
Plug 'flazz/vim-colorschemes'             " colorscheme pack
"}}}
"
" Utils
Plug 'tomtom/tlib_vim'                    " library for vim plugins
Plug 'tpope/vim-repeat'                   " library for vim plugins
Plug 'ap/vim-buftabline'
Plug 'rking/ag.vim'                       " silver searcher plugin
Plug 'ctrlpvim/ctrlp.vim'                 " fuzzy finder, like sublime (ctrl + p)
Plug 'tacahiroy/ctrlp-funky'              " function navigator for ctrlp.vim
Plug 'scrooloose/nerdcommenter'           " easy way to comment text for multiple languages
Plug 'majutsushi/tagbar'                  " tagbar
Plug 'Yggdroot/indentLine'                " show identation
Plug 'editorconfig/editorconfig-vim'      " .
Plug 'moll/vim-bbye'                      " delete buffers (close files) without closing the window
Plug 'scrooloose/nerdtree'
Plug 'netrw.vim'                          " remote files
Plug 'Shougo/vimproc.vim', {'do' : 'make'}
Plug 'Shougo/vimshell.vim'                " shell in vim
Plug 'sjl/gundo.vim'                      " undo history
Plug 'suan/vim-instant-markdown'          " Preview markdown needs to run npm -g install instant-markdown-d
Plug 'wesQ3/vim-windowswap'               " swap windows easily
Plug 'simeji/winresizer'                  " resizing splits easily
Plug 't9md/vim-choosewin'                 " Navigate to the window you choose

" Syntax
Plug 'scrooloose/syntastic'               " syntax checker
Plug 'mattn/emmet-vim'                    " emmet, fast html development
Plug 'Ntpeters/vim-better-whitespace'     " highlight unnecessary spaces

" Completions
Plug 'Shougo/neocomplete.vim'             " cache autocompletion
Plug 'ervandew/supertab'                  " autocomplete typing tab
Plug 'tpope/vim-surround'                 " to easily delete, change and add such surroundings in pairs
Plug 'Raimondi/delimitMate'               " awesome and simple plugin to automatic close quotes, parenthesis, brackets
Plug 'MarcWeber/vim-addon-mw-utils'       " cache file to autocomplete

" Snippets
if has('python')
    Plug 'Valloric/YouCompleteMe'
    Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
    Plug 'nvie/vim-flake8'
end

" Powershell
Plug 'PProvost/vim-ps1'

" Git
Plug 'tpope/vim-fugitive'                 " git plugin
Plug 'airblade/vim-gitgutter'             " git gutter plugin
"}}}

" HTML5
Plug 'othree/html5.vim'                   " html syntax

" JS
Plug 'pangloss/vim-javascript'            " js syntax
Plug 'myhere/vim-nodejs-complete'         " nodejs syntax
Plug 'nono/jquery.vim'                    " jquery syntax

" Add plugins to &runtimepath
call plug#end()



""""""""""""""""""""""""""""""""""""""""""""""
"               SETS
""""""""""""""""""""""""""""""""""""""""""""""


filetype plugin indent on

set enc=utf-8                " default encoding
set fileformats=unix,dos,mac " unix over windows over os9 formats
set scrolloff=5              " keep some more lines for scope

set ruler           " cursorline and column
set number          " linenumber

set hlsearch        " highlight matches with last search pattern
set smartindent     " smart autoindenting
set ignorecase      " ignore case in search patterns
set smartcase       " no ignore case when pattern has uppercase
set incsearch       " highlight match while typing search pattern

set showmode        " message on status line to show current mode
set showcmd         " show (partial) command in status line
set visualbell      " use visual bell instead of beeping
set ttyfast         " indicates a fast terminal connection
set autoread        " Reload files changed outside vim

set cursorline      " highlight the screen line of the cursor
set gcr=a:blinkon0  " disable cursor blink
set wildmenu        " use menu for command line completion
set wildmode=list:longest " mode for 'wildchar' command-line expansion
set wildignore+=*.dll,*.o,*.pyc,*.bak,*.exe,*.jpg,*.jpeg,*.png,*.gif,*$py.class,*.class,*/*.dSYM/*,*.dylib

set laststatus=2    " when last window has status lines
set tabstop=4       " number of spaces that <tab> in file uses
set shiftwidth=4    " number of spaces to use for (auto)indent
set softtabstop=4   " number of spaces that <tab> uses while editing
set expandtab       " use spaces when <tab> is inserted

set nowrap          " long lines
set noswapfile      " whether to use a swapfile for a buffer
set nocompatible    " behave vi-compatible as much as possible
set modeline        " recognize modelines at start or end of file
set modelines=5     " number of lines checked for modelines

set backspace=indent,eol,start
"turn on syntax highlighting
syntax on
syntax enable

try
    colorscheme Monokai
catch /^Vim\%((\a\+)\)\=:E185/
    colorscheme elflord
endtry
" hls color
hi Search cterm=NONE ctermfg=White ctermbg=DarkYellow


if has("autocmd")
    " remember line position
    au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

    " Custom task tags, highlight TODO, FIXME, NOTE, etc.
    if v:version > 701
        "autocmd Syntax * call matchadd('Todo',  '\W\zs\(TODO\|FIXME\|CHANGED\|XXX\|BUG\|HACK\)')
        "autocmd Syntax * call matchadd('Debug', '\W\zs\(NOTE\|INFO\|IDEA\)')
        autocmd Syntax * call matchadd('Todo', '\W\zs\(TODO\|FIXME\|CHANGED\|XXX\|BUG\|HACK\|NOTE\|INFO\|IDEA\)')
    endif

    " yaml files
    autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab

    " highlight for bats files
    au BufRead,BufNewFile *.bats set filetype=sh

    " close vim if the only window is a NERDTree
    autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

endif



""""""""""""""""""""""""""""""""""""""""""""""
"               MAPS
""""""""""""""""""""""""""""""""""""""""""""""

let mapleader = ","

" Switch to command mode
inoremap jj <Esc>
inoremap jk <Esc>
inoremap <leader><leader> <Esc>
inoremap <Up><Up> <Esc><Up>
inoremap <Down><Down> <Esc><Down>
"inoremap <Down> <Esc>k

"Reload .vimrc
noremap <silent> <leader>r :source ~/.vimrc<CR>:filetype detect<CR>:exe ":echo 'vimrc reloaded'"<CR>


" Append modeline after last line in buffer.
" Use substitute() instead of printf() to handle '%%s' modeline in LaTeX
" files.
function! AppendModeline()
  let l:modeline = printf(" vim: set ts=%d sw=%d tw=%d %set :",
        \ &tabstop, &shiftwidth, &textwidth, &expandtab ? '' : 'no')
  let l:modeline = substitute(&commentstring, "%s", l:modeline, "")
  call append(line("$"), l:modeline)
endfunction
nnoremap <silent> <Leader>ml :call AppendModeline()<CR>


" Fast saving
nmap <leader>w :w!<cr>

" Delete all buffers
map <C-d> :bufdo bd<cr>

" Toggle Paste
nmap <leader>pp :set invpaste paste?<CR>

" Toggle column numbers
map <Leader>n :set invnumber<CR>


""" copy in visual mode
vmap <C-c> "+yi
""" cut in visual mode
vmap <C-x> "+c
""" replace in visual mode
vmap <C-v> c<ESC>"+p
""" paste in insert mode
imap <C-v> <ESC>"+pa
""" copy line to clipboard
map <Leader>yy <ESC>"+yy


"to create a new line cmd mode without going to insert
nmap <leader>k O<esc>k0
nmap <leader>j o<esc>j0

" break line
nmap <leader>b i<cr><esc>k$

" Hidde matches
map <Leader><CR> :nohls<CR>


" Surround
map <Leader>{ <ESC>ysiw}
map <Leader>( <ESC>ysiw)
map <Leader>" <ESC>ysiw"
map <Leader>' <ESC>ysiw'
map <Leader>[ <ESC>ysiw]
map <Leader>< <ESC>ysiw>


"" netrw {{{
"" Toggle :Vexplore with <leader>t
"function! ToggleVExplorer()
"  if exists("t:expl_buf_num")
"      let expl_win_num = bufwinnr(t:expl_buf_num)
"      if expl_win_num != -1
"          let cur_win_nr = winnr()
"          exec expl_win_num . 'wincmd w'
"          close
"          exec cur_win_nr . 'wincmd w'
"          unlet t:expl_buf_num
"      else
"          unlet t:expl_buf_num
"      endif
"  else
"      exec '1wincmd w'
"      Vexplore
"      let t:expl_buf_num = bufnr("%")
"  endif
"endfunction
"" keep the browsing directory the same as the current directory
"let g:netrw_keepdir= 0
""
"set autochdir
"" tree view
"let g:netrw_liststyle = 3
"" open in previous window
"let g:netrw_browse_split = 4
"let g:netrw_altv = 1
"" percentage size
"let g:netrw_winsize = 45
"" ignore files in netrw
"let g:netrw_list_hide='^\.,.\(pyc\|pyo\|o\)$'
"map <silent> <leader>t :call ToggleVExplorer()<CR>
"map <leader>e :Explore!<CR>
"" }}}


"NERDTree
map <leader>t :NERDTreeToggle<CR>
" let NERDTreeShowHidden=1
" automatically close NerdTree when you open a file
let NERDTreeQuitOnOpen = 1
let NERDTreeIgnore = ['\.pyc$', '\.swp$']
"let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1
let g:NERDTreeWinSize=40
" custom directory color
hi Directory guifg=#ffee00 ctermfg=yellow
let g:NERDTreeDirArrowExpandable = '+'
let g:NERDTreeDirArrowCollapsible = '~'



"CtrlP & Silver searcher {{{
let g:ctrlp_map = '<Leader>p'
if executable('ag')
    " Use ag over grep
    set grepprg=ag\ --nogroup\ --nocolor
    " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
    let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
    " ag is fast enough that CtrlP doesn't need to cache
    let g:ctrlp_use_caching = 0
    " bind \ (backward slash) to grep shortcut
    "command -nargs=+ -complete=file -bar Ag silent! grep! <args>|cwindow|redraw!
    nnoremap \ :Ag<SPACE> -i<SPACE>
    " Setup some default ignores
    "\ 'dir':  '\v[\/](node_modules)|(\.(swp|git|hg|svn))$',
	let g:ctrlp_custom_ignore = {
        \ 'dir':  '\v[\/](node_modules)|(\.(swp|git|hg|svn))$',
        \ 'file': '\v\.(exe|so|dll|class|png|jpg|jpeg)$',
    \}
else
    noremap <F5> :CtrlPClearCache<CR>
endif
" }}}


" Toggle mark bar
nnoremap <Leader>m :SignatureToggle<CR>

"Toggle tagbar
nnoremap <Leader>T :TagbarToggle<CR>


" vim-better-whitespace
" Toggle whitespace
nnoremap <Leader>S :ToggleWhitespace<CR>
""" Delete whitespaces
nnoremap <Leader>ds :StripWhitespace<CR>
""" strip all trailing whitespace everytime
autocmd BufWritePre * StripWhitespace


" Toggle gundo
noremap <leader>u :GundoToggle<CR>

" Toggle git gutter
nnoremap <Leader>g :GitGutterToggle<Cr>


" Fugitive
map <leader>gs :Gstatus<CR><C-w>20+
map <leader>gc :Gcommit<CR>
"map <leader>gw :Gwrite<CR>


" ctrlp-funky
nnoremap <Leader>fu :CtrlPFunky<Cr>
"" narrow the list down with a word under cursor
nnoremap <Leader>fU :execute 'CtrlPFunky ' . expand('<cword>')<Cr>
let g:ctrlp_funky_multi_buffers = 1
let g:ctrlp_funky_matchtype = 'path'


" Neocomplcache
let g:neocomplcache_enable_at_startup = 1


" airline {{{
"let g:airline_theme='kalisi'
"let g:airline#extensions#tabline#buffer_nr_format = '%s: '
"let g:airline#extensions#tabline#enabled          = 1       " Airline - Enable the list of buffers
"let g:airline#extensions#tabline#fnamemod         = ':t'    " Airline Show just the filename
"let g:airline#extensions#tabline#show_tab_nr      = 1
"let g:airline#extensions#tabline#buffer_idx_mode  = 1
" Quick switch between tabs
"nmap <leader>1 <Plug>AirlineSelectTab1
"nmap <leader>2 <Plug>AirlineSelectTab2
"nmap <leader>3 <Plug>AirlineSelectTab3
"nmap <leader>4 <Plug>AirlineSelectTab4
"nmap <leader>5 <Plug>AirlineSelectTab5
"nmap <leader>6 <Plug>AirlineSelectTab6
"nmap <leader>7 <Plug>AirlineSelectTab7
"nmap <leader>8 <Plug>AirlineSelectTab8
"nmap <leader>9 <Plug>AirlineSelectTab9
"nmap <leader>. <Plug>AirlineSelectPrevTab
"nmap <leader>- <Plug>AirlineSelectNextTab
" }}}


" delimitmate
let g:delimitMate_jump_expansion=0
let g:delimitMate_expand_cr=2


" Nerdcommenter
let g:NERDSpaceDelims = 0


" Syntastic {{{
"" Toggle syntastic mode
nmap <Leader>s :SyntasticToggleMode<CR>
nnoremap <leader>S :SyntasticToggleMode<CR> :SyntasticCheck<CR>
let g:syntastic_error_symbol = '✘'
let g:syntastic_warning_symbol = '⚠'
let g:syntastic_style_error_symbol = '⚡'
let g:syntastic_style_warning_symbol = '⚡'
let g:syntastic_ignore_files = ['\.xml$']
let g:syntastic_mode_map = { "mode": "pasive", "active_filetypes": [], "passive_filetypes": [] }
" }}}


" PythonMode
let g:pymode_doc = 1

" bbyte
nnoremap <Leader>q :Bdelete<CR>


" choosewin
nmap <leader>-  <Plug>(choosewin)
let g:choosewin_overlay_enable = 1


" vimproc
let g:vimshell_prompt = 'vimshell % '
nnoremap <Leader>x :VimShellExecute<SPACE>
nnoremap <Leader>$ :VimShell<CR>
nnoremap <Leader>xq :VimShellClose<CR>


" Use neocomplete.
let g:neocomplete#enable_at_startup = 1
" Set minimum syntax keyword length.
let g:neocomplete#sources#syntax#min_keyword_length = 3

" snippets {{{
"let g:ycm_auto_trigger = 0
" make YCM compatible with UltiSnips (using supertab)
let g:SuperTabDefaultCompletionType     = '<C-n>'
let g:SuperTabCrMapping                 = 0
let g:ycm_key_list_select_completion    = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion  = ['<C-p>', '<Up>']
" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger        = "<tab>"
let g:UltiSnipsJumpForwardTrigger   = "<tab>"
let g:UltiSnipsJumpBackwardTrigger  = "<s-tab>"
let g:UltiSnipsListSnippets         = "<c-e>"
" }}}


" Statusline
let g:lightline = {
  \ 'colorscheme': 'wombat',
  \ 'active': {
  \   'left': [ [ 'mode', 'paste', 'gitbranch' ],
  \             [ 'readonly', 'filename', 'modified' ] ]
  \ },
  \ 'component_function': {
  \   'gitbranch': 'fugitive#head'
  \ },
\ }


" indent-guides
map <leader>i :IndentLinesToggle<CR>
let g:indentLine_enabled = 0



" Buftabline
let g:buftabline_numbers=2
let g:buftabline_indicators=1
" quick switch between buffers
nmap <leader>1 <Plug>BufTabLine.Go(1)
nmap <leader>2 <Plug>BufTabLine.Go(2)
nmap <leader>3 <Plug>BufTabLine.Go(3)
nmap <leader>4 <Plug>BufTabLine.Go(4)
nmap <leader>5 <Plug>BufTabLine.Go(5)
nmap <leader>6 <Plug>BufTabLine.Go(6)
nmap <leader>7 <Plug>BufTabLine.Go(7)
nmap <leader>8 <Plug>BufTabLine.Go(8)
nmap <leader>9 <Plug>BufTabLine.Go(9)
nmap <leader>0 <Plug>BufTabLine.Go(10)


"let g:tagbar_type_ps1 = {
    "\ 'ctagstype' : 'powershell',
    "\ 'kinds'     : [
        "\ 'f:function',
        "\ 'i:filter',
        "\ 'a:alias'
    "\ ]
"\ }

