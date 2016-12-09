set nocompatible
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

"Bundle
Plugin 'gmarik/vundle'

""""""""""""""""""""""""""
""""""""""""""""""""""""""
"       PLUGINS
""""""""""""""""""""""""""
""""""""""""""""""""""""""


"Visual
""""""""""""""""""""""""""""""""""""
Plugin 'bling/vim-airline'                  " statusline
Plugin 'vim-airline/vim-airline-themes'     " theme for statusline
Plugin 'nathanaelkane/vim-indent-guides'    " identation
Plugin 'kshenoy/vim-signature'              " show marks
Plugin 'flazz/vim-colorschemes'             " Colorscheme pack

"Develop tools
""""""""""""""""""""""""""""""""""""
Plugin 'scrooloose/syntastic'               " syntax checker
Plugin 'Shougo/neocomplcache.vim'           " cache autocompletion
Plugin 'ervandew/supertab'                  " autocomplete typing tab
Plugin 'tpope/vim-fugitive'                 " git plugin
Plugin 'airblade/vim-gitgutter'             " git gutter plugin
Plugin 'mattn/emmet-vim'                    " emmet, fast html development
Plugin 'majutsushi/tagbar'                  " tagbar
Plugin 'tpope/vim-surround'                 " to easily delete, change and add such surroundings in pairs
Plugin 'Raimondi/delimitMate'               " awesome and simple plugin to automatic close quotes, parenthesis, brackets
Plugin 'heavenshell/vim-jsdoc'              " auto jsdoc for functions

"Syntax
""""""""""""""""""""""""""""""""""""
Plugin 'othree/html5.vim'                   " html syntax
Plugin 'pangloss/vim-javascript'            " js syntax
Plugin 'myhere/vim-nodejs-complete'         " nodejs syntax
Plugin 'nono/jquery.vim'                    " jquery syntax
"""nerd commenter
Plugin 'scrooloose/nerdcommenter'"

"Awesome Tools
""""""""""""""""""""""""""""""""""""
Plugin 'tpope/vim-repeat'                   " used for a lot of vim plugins
Plugin 'rking/ag.vim'                       " silver searcher plugin
Plugin 'kien/ctrlp.vim'                     " fuzzy finder like sublime (ctrl + p)
Plugin 'tacahiroy/ctrlp-funky'              " function navigator for ctrlp.vim
Plugin 'scrooloose/nerdtree'                " File explorer
Plugin 'troydm/easybuffer.vim'              " list of buffers
Plugin 'MarcWeber/vim-addon-mw-utils'       " cache file to autocomplete
Plugin 'tomtom/tlib_vim'                    " vim libs
Plugin 'netrw.vim'                          " remote files
Plugin 'Ntpeters/vim-better-whitespace'     " highlight unnecessary spaces
Plugin 'suan/vim-instant-markdown'          " Preview markdown needs to run npm -g install instant-markdown-d
Plugin 'benmills/vimux'                     " run commands
Plugin 'sjl/gundo.vim'                      " undo history
"Plugin 'terryma/vim-multiple-cursors'       " multiple cursors
Plugin 'wesQ3/vim-windowswap'               " swap windows easily
Plugin 'SirVer/ultisnips'                   " Snippet engine
Plugin 'honza/vim-snippets'                 " Snippets


""""""""""""""""""""""""""
""""""""""""""""""""""""""
"       SETS
""""""""""""""""""""""""""
""""""""""""""""""""""""""


filetype plugin indent on

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
"
"set undofile
set cursorline      " highlight the screen line of the cursor
set gcr=a:blinkon0  " disable cursor blink
set wildmenu        " use menu for command line completion
set wildmode=list:longest " mode for 'wildchar' command-line expansion

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

"turn on syntax highlighting
syntax on
syntax enable

try
    colorscheme triplejelly
catch /^Vim\%((\a\+)\)\=:E185/
    colorscheme elflord
endtry
" let g:rehash256 = 1
"hi CursorLine term=bold cterm=bold guibg=Grey40
"hi CursorLine  term=bold cterm=bold ctermbg=8


""""""""""""""""""""""""""
""""""""""""""""""""""""""
"       MAPS
""""""""""""""""""""""""""
""""""""""""""""""""""""""

let mapleader = ","

"Reload .vimrc
noremap <silent> <leader>R :source ~/.vimrc<CR>:filetype detect<CR>:exe ":echo 'vimrc reloaded'"<CR>

" Toggle Paste
set pastetoggle=<C-p>

" Hidden search hl
map <Leader>F :nohls<CR>

" Hidden column number
map <Leader>n :set invnumber<CR>

" copy and paste {
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
" }

" Surround
map <Leader>{ <ESC>ysiw}
map <Leader>( <ESC>ysiw)
map <Leader>" <ESC>ysiw"
map <Leader>' <ESC>ysiw'
map <Leader>[ <ESC>ysiw]
map <Leader>< <ESC>ysiw>
" }

" NERD Tree {
map <Leader>e :NERDTreeToggle<CR>
let NERDTreeIgnore = ['\.pyc$']
" }

"Resize current buffer +/-5 {
map <Leader>h :vertical resize -5<cr>
map <Leader>j :resize +5<cr>
map <Leader>k :resize -5<cr>
map <Leader>l :vertical resize +5<cr>
" }

" Move lines normal mode {
nnoremap <C-j> :m .+1<CR>==
nnoremap <C-k> :m .-2<CR>==
" }

" Move lines insert mode {
inoremap <C-j> <ESC>:m .+1<CR>==gi
inoremap <C-k> <ESC>:m .-2<CR>==gi
" }

" Move lines visual mode {
vnoremap <C-j> :m '>+1<CR>gv=gv
vnoremap <C-k> :m '<-2<CR>gv=gv
" }

"CtrlP & Silver searcher {
let g:ctrlp_map = '<Leader>p'
if executable('ag')
    " Use ag over grep
    set grepprg=ag\ --nogroup\ --nocolor
    " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
    let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
    " ag is fast enough that CtrlP doesn't need to cache
    let g:ctrlp_use_caching = 0
    " bind \ (backward slash) to grep shortcut
    " command -nargs=+ -complete=file -bar Ag silent! grep! <args>|cwindow|redraw!
    nnoremap \ :Ag<SPACE> -i<SPACE>
    " Setup some default ignores
				"\ 'dir':  '\v[\/](node_modules)|(\.(swp|git|hg|svn))$',
	let g:ctrlp_custom_ignore = {
				\ 'dir':  '\v[\/](node_modules)|(\.(swp|git|hg|svn))$',
				\ 'file': '\v\.(exe|so|dll|class|png|jpg|jpeg)$',
				\}
else
    map <F5> :CtrlPClearCache<CR>
endif
" }

" bind K to grep word under cursor
"nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

"Easybuffer
nmap <Leader>b :EasyBuffer<CR>

"Toggle syntastic mode
nmap <Leader>ss :SyntasticToggleMode<CR>

" Toggle mark bar
nmap <Leader>mm :SignatureToggle<CR>

"Toggle tagbar
nmap <Leader>tt :TagbarToggle<CR>

" Toggle whitespace (vim-better-whitespace) {
nmap <Leader>ts :ToggleWhitespace<CR>
""" Strip whitespaces
nmap <Leader>ds :StripWhitespace<CR>
""" strip all trailing whitespace everytime
autocmd BufWritePre * StripWhitespace
" }

"vimux shell
map <leader>x :VimuxPromptCommand<CR>

"gundo toggle
map <leader>uu :GundoToggle<CR>

" ctrlp-funky {
nnoremap <Leader>fu :CtrlPFunky<Cr>
"" narrow the list down with a word under cursor
nnoremap <Leader>fU :execute 'CtrlPFunky ' . expand('<cword>')<Cr>
" }

" git gutter
nnoremap <Leader>gg :GitGutterToggle<Cr>


""""""""""""""""""""""""""
""""""""""""""""""""""""""
"       OPTIONS
""""""""""""""""""""""""""
""""""""""""""""""""""""""


" ctrlp-funky {
let g:ctrlp_funky_multi_buffers = 1
let g:ctrlp_funky_matchtype = 'path'
" let g:ctrlp_funky_syntax_highlight = 1
" }

" Neocomplcache {
let g:neocomplcache_enable_at_startup = 1
" }

" airline {
let g:airline_theme='badwolf'                       " Airline - select theme
let g:airline#extensions#tabline#enabled = 1        " Airline - Enable the list of buffers
let g:airline#extensions#tabline#fnamemod = ':t'    " Airline Show just the filename
" }

" delimitmate {
let g:delimitMate_jump_expansion=1
let g:delimitMate_expand_cr=2
" let g:delimitMateSmartQuotes=1
"}

" Snippet configuration {
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"
" If you want :UltiSnipsEdit to split your window.
"let g:UltiSnipsEditSplit="vertical"
" }

" Custom task tags {
if has("autocmd")
  " Highlight TODO, FIXME, NOTE, etc.
  if v:version > 701
    "autocmd Syntax * call matchadd('Todo',  '\W\zs\(TODO\|FIXME\|CHANGED\|XXX\|BUG\|HACK\)')
    "autocmd Syntax * call matchadd('Debug', '\W\zs\(NOTE\|INFO\|IDEA\)')
    " I prefer the Todo style for triplejelly theme
    autocmd Syntax * call matchadd('Todo', '\W\zs\(TODO\|FIXME\|CHANGED\|XXX\|BUG\|HACK\|NOTE\|INFO\|IDEA\)')
  endif
endif
" }

" Nerdcommenter {
let g:NERDSpaceDelims = 0
" }
