set nocompatible
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

"Bundle
Plugin 'gmarik/vundle'

"Visual
""""""""""""""""""""""""""""""""""""
"""Statusline
Plugin 'bling/vim-airline'
"""Show the identation
Plugin 'nathanaelkane/vim-indent-guides'
"""Show marks
Plugin 'kshenoy/vim-signature'
""" color
Plugin 'tomasr/molokai'

"Develop
""""""""""""""""""""""""""""""""""""
"""Syntax checker
Plugin 'scrooloose/syntastic'
"""Html5 syntax and autocomplete
Plugin 'othree/html5.vim'
"""Javascript syntax 
Plugin 'pangloss/vim-javascript'
"""Jquery syntax
Plugin 'nono/jquery.vim'
"""For fast html development, tags
Plugin 'mattn/emmet-vim'
"""Parentheses, brackets, tags
Plugin 'tpope/vim-surround'
"""Autocomplete by tab
Plugin 'ervandew/supertab'
"""Cache autocomplete
Plugin 'Shougo/neocomplcache.vim'
"""Snippet engine
"Plugin 'garbas/vim-snipmate'
"""Snippets
"Plugin 'honza/vim-snippets'
"""Chef syntax and snippets
"Plugin 'vadv/vim-chef'
"""Git plugin for vim
Plugin 'tpope/vim-fugitive'
"""nodejs
Plugin 'myhere/vim-nodejs-complete'
"""nerd commenter
Plugin 'scrooloose/nerdcommenter'"
"""Auto close brackets
Plugin 'jiangmiao/auto-pairs'"

"Awesome Tools
""""""""""""""""""""""""""""""""""""
"""Find files like sublime ctrl+p
Plugin 'kien/ctrlp.vim'
"""File explorer
Plugin 'scrooloose/nerdtree'
"""Change to any buffer by index number'
Plugin 'troydm/easybuffer.vim'
"""cache file to autocomplete
Plugin 'MarcWeber/vim-addon-mw-utils'
"""vim's utilities
Plugin 'tomtom/tlib_vim'
"""Tagbar 
Plugin 'majutsushi/tagbar'
"""Network reding and writing
Plugin 'netrw.vim'


"Config
""""""""""""""""""""""""""""""""""""
filetype plugin indent on

set ruler 
set number

set hlsearch 
set smartindent
set ignorecase
set incsearch
set smartcase

set showmode
set showcmd
set visualbell
set ttyfast
"set undofile
set cursorline
set wildmenu
set wildmode=list:longest

set laststatus=2
set tabstop=4 
set shiftwidth=4  
set softtabstop=4 
set expandtab 

set nowrap
set noswapfile
set nocompatible 
set modeline
set modelines=5

""" COLORS
syntax on
syntax enable

try
    colorscheme molokai
catch /^Vim\%((\a\+)\)\=:E185/
    colorscheme elflord
endtry

"let g:rehash256 = 1



"##########  General ############
let mapleader = ","

"hi CursorLine term=bold cterm=bold guibg=Grey40
"hi CursorLine  term=bold cterm=bold ctermbg=8

"Toggle Paste
set pastetoggle=<C-p>

"Hidden search hl
map <Leader>F :nohls<CR>

"Hidden column number
map <Leader>n :set invnumber<CR>

"NERD Tree
map <Leader>e :NERDTreeToggle<CR>
let NERDTreeIgnore = ['\.pyc$']

"Reload vim shortcut
noremap <silent> <leader>V :source ~/.vimrc<CR>:filetype detect<CR>:exe ":echo 'vimrc reloaded'"<CR>

"Resize current buffer +/-5
map <Leader>h :vertical resize -5<cr>
map <Leader>j :resize +5<cr>
map <Leader>k :resize -5<cr>
map <Leader>l :vertical resize +5<cr>

" Move lines normal mode
nnoremap <C-j> :m .+1<CR>==
nnoremap <C-k> :m .-2<CR>==
" Move lines insert mode
inoremap <C-j> <ESC>:m .+1<CR>==gi
inoremap <C-k> <ESC>:m .-2<CR>==gi
" Move lines visual mode
vnoremap <C-j> :m '>+1<CR>gv=gv
vnoremap <C-k> :m '<-2<CR>gv=gv

"CtrlP & Silver searcher
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
    nnoremap \ :Ag<SPACE> -i
else
    map <F5> :CtrlPClearCache<CR>
endif

" bind K to grep word under cursor
"nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>
"

"Easybuffe
nmap <Leader>b :EasyBuffer<CR>

"Neocomplcache
let g:neocomplcache_enable_at_startup = 1

"show identation
"<Leader>ig


"Toggle mark bar 
nmap <Leader>m :SignatureToggle<CR>

"Toggle tagbar
nmap <Leader>tt :TagbarToggle<CR>

