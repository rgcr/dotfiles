set nocompatible
filetype off

if has("nvim")
    call plug#begin('~/.local/share/nvim/plugged')
else
    call plug#begin('~/.vim/plugged')
end

""""""""""""""""""""""""""
""""""""""""""""""""""""""
"       PLUGINS
""""""""""""""""""""""""""
""""""""""""""""""""""""""

"Themes,colors & status lines {{{
Plug 'bling/vim-airline'                  " statusline
Plug 'vim-airline/vim-airline-themes'     " theme for statusline
Plug 'kshenoy/vim-signature'              " show marks
Plug 'flazz/vim-colorschemes'             " colorscheme pack
"}}}
"
"Utils {{{
Plug 'tomtom/tlib_vim'                    " library for vim plugins
Plug 'tpope/vim-repeat'                   " library for vim plugins
Plug 'rking/ag.vim'                       " silver searcher plugin
Plug 'ctrlpvim/ctrlp.vim'                 " fuzzy finder like sublime (ctrl + p)
Plug 'scrooloose/nerdtree'                " File explorer
Plug 'scrooloose/nerdcommenter'           " easy way to comment text for multiple languages
Plug 'majutsushi/tagbar'                  " tagbar
Plug 'nathanaelkane/vim-indent-guides'    " show identation
Plug 'editorconfig/editorconfig-vim'      "
"Plug 'troydm/easybuffer.vim'              " list of buffers
Plug 'moll/vim-bbye' 					  " delete buffers (close files) without closing your windows
Plug 'netrw.vim'                          " remote files
Plug 'benmills/vimux'                     " run commands
Plug 'sjl/gundo.vim'                      " undo history
Plug 'suan/vim-instant-markdown'          " Preview markdown needs to run npm -g install instant-markdown-d
Plug 'wesQ3/vim-windowswap'               " swap windows easily
Plug 'junegunn/goyo.vim'                  " free distraction
Plug 'simeji/winresizer'                  " resizing splits easily
Plug 't9md/vim-choosewin'                 " Navigate to the window you choose
"}}}

"Syntax {{{
Plug 'scrooloose/syntastic'               " syntax checker
Plug 'mattn/emmet-vim'                    " emmet, fast html development
Plug 'Ntpeters/vim-better-whitespace'     " highlight unnecessary spaces
"}}}

"Completions {{{
Plug 'Shougo/neocomplcache.vim'           " cache autocompletion
Plug 'ervandew/supertab'                  " autocomplete typing tab
Plug 'tpope/vim-surround'                 " to easily delete, change and add such surroundings in pairs
Plug 'Raimondi/delimitMate'               " awesome and simple plugin to automatic close quotes, parenthesis, brackets
Plug 'tacahiroy/ctrlp-funky'              " function navigator for ctrlp.vim
Plug 'MarcWeber/vim-addon-mw-utils'       " cache file to autocomplete
" Snippets
if has('python')
    Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
end
"}}}

"Git {{{
Plug 'tpope/vim-fugitive'                 " git plugin
Plug 'airblade/vim-gitgutter'             " git gutter plugin
"}}}

" HTML5 {{{
Plug 'othree/html5.vim'                   " html syntax
"}}}

"JS {{{
Plug 'pangloss/vim-javascript'            " js syntax
Plug 'myhere/vim-nodejs-complete'         " nodejs syntax
Plug 'nono/jquery.vim'                    " jquery syntax
"}}}

" Add plugins to &runtimepath
 call plug#end()

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
    colorscheme Monokai
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

" Fast saving
nmap <leader>w :w!<cr>

" Moving around, tabs {
" delete all buffers
map <C-d> :bufdo bd<cr>
" moving arround
map <C-l> :bnext<cr>
map <C-h> :bprevious<cr>
"}



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

" Resize current buffer +/-5 {
" XXX: Don't need this anymore,
" simeji/winresizer does the job
"map <Leader>h :vertical resize -5<cr>
"map <Leader>j :resize +5<cr>
"map <Leader>k :resize -5<cr>
"map <Leader>l :vertical resize +5<cr>
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
"nmap <Leader>b :EasyBuffer<CR>

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

" ctrlp-funky {
let g:ctrlp_funky_multi_buffers = 1
let g:ctrlp_funky_matchtype = 'path'
" let g:ctrlp_funky_syntax_highlight = 1
" }

" Neocomplcache {
let g:neocomplcache_enable_at_startup = 1
" }

" airline {
let g:airline_theme='kalisi'                       " Airline - select theme
let g:airline#extensions#tabline#buffer_nr_format = '%s: '
let g:airline#extensions#tabline#enabled = 1        " Airline - Enable the list of buffers
"let g:airline#extensions#tabline#exclude_preview = 1
"let g:airline#extensions#tabline#tab_nr_type = 2 " splits and tab number
"let g:airline#extensions#tabline#fnamemod = ':~:.'   " Airline Show just the filename
let g:airline#extensions#tabline#fnamemod = ':t'   " Airline Show just the filename
let g:airline#extensions#tabline#show_tab_nr = 1
let g:airline#extensions#tabline#buffer_idx_mode = 1
nmap <leader>1 <Plug>AirlineSelectTab1
nmap <leader>2 <Plug>AirlineSelectTab2
nmap <leader>3 <Plug>AirlineSelectTab3
nmap <leader>4 <Plug>AirlineSelectTab4
nmap <leader>5 <Plug>AirlineSelectTab5
nmap <leader>6 <Plug>AirlineSelectTab6
nmap <leader>7 <Plug>AirlineSelectTab7
nmap <leader>8 <Plug>AirlineSelectTab8
nmap <leader>9 <Plug>AirlineSelectTab9
nmap <leader>. <Plug>AirlineSelectPrevTab
nmap <leader>- <Plug>AirlineSelectNextTab
" }

" delimitmate {
let g:delimitMate_jump_expansion=0
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

" Syntastic {
let g:syntastic_error_symbol = '✘'
let g:syntastic_warning_symbol = '⚠'
let g:syntastic_style_error_symbol = '⚡'
let g:syntastic_style_warning_symbol = '⚡'
"}

" PythonMode {
let g:pymode_doc = 1
" }

" bbyte {
nnoremap <Leader>q :Bdelete<CR>
" }

" choosewin {
nmap  -  <Plug>(choosewin)
let g:choosewin_overlay_enable = 1

" }


