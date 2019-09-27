"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Leader
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let mapleader = ","
let maplocalleader = "\<space>"


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Plugin management with vim-plug
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Auto-install vim-plug
function! VimPlugInstall()
  if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
    silent !mkdir ~/.config/nvim/ > /dev/null 2>&1
    silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    " autocmd VimEnter * PlugInstall
  endif
endfunction

" Custom command to install vim-plug => :VimPlugInstall
command! VimPlugInstall :call VimPlugInstall()<bar>PlugInstall<CR>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Global flags
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" enable only one of the below variables
" to use LSP (coc.nvim) or YCM
let g:nvim_use_lsp = 1
let g:nvim_use_ycm = 0


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Plugins
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Required to install plugins
filetype off

" Load Vim-Plug
call plug#begin('~/.local/share/nvim/plugged')

" Libraries for some vim plugins
".......................................
Plug 'tomtom/tlib_vim'
Plug 'tpope/vim-repeat'
".......................................


" UI
".......................................
Plug 'flazz/vim-colorschemes'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'guns/xterm-color-table.vim'
".......................................


" Utilities
"......................................................
Plug 'jceb/vim-orgmode'
Plug 'tpope/vim-projectionist'

"" File explorer
Plug 'scrooloose/NERDTree', { 'on': 'NERDTreeToggle'  }

" Switch between buffers easily
Plug 'ap/vim-buftabline'

" Shows marks in the number column
Plug 'kshenoy/vim-signature'

" Resize windows easily
Plug 'simeji/winresizer'

" Switch between windows easily
Plug 't9md/vim-choosewin'

" Undo tree visualizer
Plug 'mbbill/undotree'

" A dynamic calendar in vim
Plug 'itchyny/calendar.vim'

" fzf, like help or ivy in emacs
Plug 'junegunn/fzf', { 'do': './install --no-bash' }
Plug 'junegunn/fzf.vim'

" Like General or Hydra for emacs
"   it show a menu with the list of maps
Plug 'liuchengxu/vim-which-key' " {{{
  " register dictionaries
  let g:which_key_map_leader =  {}
  let g:which_key_map_localleader =  {}
"}}}

"......................................................


" Dev Utilities
"......................................................
Plug 'editorconfig/editorconfig-vim'

" Comment code faster
Plug 'scrooloose/nerdcommenter'

" For brackets parens quoutes
Plug 'jiangmiao/auto-pairs'

"" Tagbar
Plug 'majutsushi/tagbar'

"" Show indent lines
Plug 'yggdroot/indentLine'

"" Auto generate tags
" Plug 'ludovicchabant/vim-gutentags'

"" snippets
Plug 'sirver/ultisnips'
Plug 'honza/vim-snippets'

"" completion engine
" Plug 'maralla/completor.vim'

"" git
Plug 'tpope/vim-fugitive'
Plug 'mhinz/vim-signify', { 'on': 'SignifyToggle'  }

" html
Plug 'mattn/emmet-vim'
Plug 'alvan/vim-closetag'

"" toml
Plug 'cespare/vim-toml', {'for': 'toml'}
"" ansible
Plug 'pearofducks/ansible-vim'
"" html5
Plug 'othree/html5.vim'
"" js
Plug 'pangloss/vim-javascript', {'for': 'javscript'}
Plug 'myhere/vim-nodejs-complete', {'for': 'javscript'}
"" vuejs
Plug 'posva/vim-vue'

"" python
Plug 'vim-python/python-syntax', {'for': 'python'}

"" php
Plug 'jwalton512/vim-blade', {'for': 'php'}
Plug 'StanAngeloff/php.vim', { 'for': 'php' }
" Plug 'noahfrederick/vim-laravel', {'for': 'php'}

if g:nvim_use_ycm && ( has('python') || has('python3') )
  " python
  Plug 'Valloric/YouCompleteMe', { 'do': './install.py --all'  }
endif

if g:nvim_use_lsp
  " LSP - intellisense engine for vim8/neovim
  Plug 'neoclide/coc.nvim', {'branch': 'release'}
else
  " asynchronous lint engine
  Plug 'w0rp/ale'
  Plug 'python-mode/python-mode', { 'branch': 'develop' }
  if executable('black')
    " python formatter
    Plug 'psf/black', {'for': 'python'}
  elseif executable('yapf')
    " python formatter
    Plug 'mindriot101/vim-yapf', {'for': 'python'}
  endif
endif

" add plugins to &runtimepath
call plug#end()
filetype plugin indent on


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
syntax enable
syntax on

set nocompatible              " be improbed

set enc=utf-8                 " default encoding
set termencoding=utf8

if exists('g:gui_oni')
    set mouse=a
    set noshowmode
    set noruler
    set laststatus=0
    set noshowcmd
else
    set ruler                 " cursorline and column
    set showmode              " message on status line to show current mode
    set laststatus=2          " when last window has status lines
    set showcmd               " show (partial) command in status line
endif


set novb
set t_vb=
set background=dark

set fileformats=unix,dos,mac  " unix over windows over os9 formats
set scrolloff=5               " keep some more lines for scope

set number                    " linenumber

set hlsearch                  " highlight matches with last search pattern
set smartindent               " smart autoindenting
set ignorecase                " ignore case in search patterns
set smartcase                 " no ignore case when pattern has uppercase
set incsearch                 " highlight match while typing search pattern

set visualbell                " use visual bell instead of beeping
set ttyfast                   " indicates a fast terminal connection
set autoread                  " reload files changed outside vim

set cursorline                " highlight the screen line of the cursor
set gcr=a:blinkon0            " disable cursor blink

set tabstop=4                 " number of spaces that <TAB> in file uses
set shiftwidth=4              " number of spaces to use for (auto)indent
set softtabstop=4             " number of spaces that <TAB> uses while editing
set expandtab                 " use spaces when <TAB> is inserted

set nowrap                    " long lines
set noswapfile                " whether to use a swapfile for a buffer
set modeline                  " recognize modelines at start or end of file
set modelines=5               " number of lines checked for modelines
set backspace=indent,eol,start

" set completeopt-=preview    " disable docs functions
" set shortmess+=c            " don't give |ins-completion-menu| messages.
set splitbelow                " preview windows at bottom
set updatetime=300

set wildmenu                  " use menu for command line completion
set wildmode=list:longest     " mode for 'wildchar' command-line expansion

set wildignore+=*.dll,*.o,
set wildignore+=*.pyc,*.bak,
set wildignore+=*$py.class,
set wildignore=*.exe,*.jpg,
set wildignore+=*.jpeg,*.png,
set wildignore+=.gif,
set wildignore+=.class,
set wildignore+=*/*.dsym/*,
set wildignore+=*.dylib

set tags+=.tags,./.git/tags

set undodir=~/.config/undodir.nvim
set undofile


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors and Colorschemes
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" enable true colors
if has("termguicolors")
  " set vim-specific sequences for rgb colors
  let &t_8f = "\<esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<esc>[48;2;%lu;%lu;%lum"
  set termguicolors
endif

set t_ut=
set t_co=256

try
    colorscheme deep-space
    " fix for deep-space, I hate the original matchparen
    hi! MatchParen cterm=NONE,bold gui=NONE,bold ctermfg=NONE guifg=NONE ctermbg=7 guibg=#eee8d5
    " original
    " MatchParen     xxx cterm=bold ctermbg=6 gui=bold guifg=#232936 guibg=#c47ebd

    " highlight linenr
          " \ term=bold cterm=none ctermfg=darkgrey ctermbg=none
          " \ gui=none guifg=darkgrey guibg=none
catch
    colorscheme elflord
endtry


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => FileType-specific configurations
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup vimrc
  autocmd!

  " custom task tags
  if v:version > 701
    autocmd syntax * call matchadd('Todo',
          \ '\W\zs\(TODO\|FIXME\|CHANGED\|XXX\|BUG\|HACK\|NOTE\|INFO\|IDEA\)')
  endif

  " remember line position
  autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
        \ | exe "normal! g`\"" | endif

  " remove all trailing whitespaces
  " autocmd FileType c,cpp,perl,python,sh,yml
        " \ autocmd bufwritepre <buffer> %s/\s\+$//e

  " http://vim.wikia.com/wiki/Highlight_unwanted_spaces
  " autocmd BufNewFile,BufRead,InsertLeave * silent! match ExtraWhitespace /\s\+$/
  " autocmd InsertEnter * silent! match ExtraWhitespace /\s\+\%#\@<!$/

  " warn for lines that exceed 80 column line
  " autocmd BufWinEnter * let w:m1=matchadd('search', '\%<82v.\%>81v', -1)
  " autocmd BufWinEnter * let w:m2=matchadd('errormsg', '\%>81v.\+', -1)

  " unset paste mode on InsertLeave
  autocmd InsertLeave * silent! set nopaste

  " Disable automatic comment insertion after a comment line
  autocmd FileType * set formatoptions-=r formatoptions-=c formatoptions-=o


  " hide cursor line in inactive windows
  " augroup cursorline
    " autocmd!
    " autocmd VimEnter * setlocal cursorline
    " autocmd WinEnter * setlocal cursorline
    " autocmd BufWinEnter * setlocal cursorline
    " autocmd winleave * setlocal nocursorline
  " augroup end

  " automatic rename of tmux window
  " if exists('$TMUX') && !exists('$NORENAME')
    " autocmd BufEnter * if empty(&buftype)
          " \ | call system('tmux rename-window '.expand('%:t:s')) | endif
    " autocmd VimLeave * call system('tmux set-window automatic-rename on')
  " endif
augroup end

augroup vuejs
  autocmd!
  autocmd FileType vue setlocal ts=2 sts=2 sw=2 expandtab
augroup end

augroup yaml
  autocmd!
  autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
augroup end

augroup bats
  autocmd!
  autocmd bufread,bufnewfile *.bats set filetype=sh
augroup end

augroup bladephp
  autocmd BufNewFile,BufRead *.blade.php set ft=blade
augroup end

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Functions
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" append modeline after last line in buffer
function! ModeLineAppend()
  let l:modeline = printf(" vim: ts=%d sw=%d tw=%d %set :",
        \ &tabstop, &shiftwidth, &textwidth, &expandtab ? '' : 'no')
  let l:modeline = substitute(&commentstring, "%s", l:modeline, "")
  call append(line("$"), l:modeline)
endfunction

" zoom / restore window(buffer)
function! BufferZoomToggle() abort
    if exists('t:zoomed') && t:zoomed
        execute t:zoom_winrestcmd
        let t:zoomed = 0
    else
        let t:zoom_winrestcmd = winrestcmd()
        resize
        vertical resize
        let t:zoomed = 1
    endif
endfunction

"coc.nvim {{{
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction
"}}}


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Commands and Mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" go to previous window
nnoremap <silent> <leader><leader> <ESC><C-W><C-W>

" modeline command {{{
command! ModeLineAppend :call ModeLineAppend()
" modeline map
nnoremap <leader>ml :ModeLineAppend<CR>
" }}}

" vim config commands {{{
command! VimConfigEdit :e $MYVIMRC
command! VimConfigSource :source $MYVIMRC <bar>
      \ :filetype detect <bar> :exe ":echo 'config reloaded!!'"
" vim config maps
nnoremap <silent> <localleader>vr :<C-U>VimConfigSource<CR>
nnoremap <silent> <localleader>ve :<C-U>VimConfigEdit<CR>
" }}}

" buffer commands {{{
command! BufferDeleteCurrent :bp<bar>bd#
command! BufferDeleteAll     :bufdo bd
command! BufferSaveForce     :w!
command! BufferZoomToggle    :call BufferZoomToggle()
command! BufferGoToPath      :cd %:p:h<bar>:pwd
command! BufferSplitLine     normal!i<CR><esc>k$
"}}}

"buffer maps {{{
"" close buffer quickly
nnoremap <silent> <leader>x :q<CR>
"" fast saving
nnoremap <silent> <leader>w :<C-U>BufferSaveForce<CR>
"" close buffer without close the window
nnoremap <leader>q :<C-U>BufferDeleteCurrent<CR>
"" delete (close) all buffers
noremap <leader>qa :<C-U>BufferDeleteAll<CR>

"" zoom buffer
"" note: we can use: c-w-| (vsplits), c-w-_ (hsplits), c-w-= (restore)
"" noremap <silent> zz <C-W>_ \| <C-W>\|
"" noremap <silent> zo <C-W>=
nnoremap <leader>z :<C-U>BufferZoomToggle<CR>
"" switch cwd to the directory of the open buffer
nnoremap <leader>cd :<C-U>BufferGoToPath<CR>
" split current line
nmap <leader>b :<C-U>BufferSplitLine<CR>
"}}}

" " format json
command! JSONformat %!python -m json.tool

" " put : command on ; for easer access
" nnoremap ; :

" " redo
" nnoremap <C-R> r

" " qq to record macro, q to replay
nnoremap q @q

" clipboard, copy & paste {{{
" windows
if executable("clip.exe")
    func! SelectedText()
        normal gv"xy
        let result = getreg("x")
        return result
    endfunc
    """ copy visual selection to clipboard
    vnoremap <c-c> :call system('clip.exe', SelectedText())<CR>
    """ cut
    noremap <c-x> :call system('clip.exe', SelectedText())<CR>gvx
" linux
else
    """ copy line to clipboard
    map <C-c> <ESC>"+yy
    """ copy in visual mode
    vmap <C-c> "+yi
    """ cut in visual mode
    vmap <C-x> "+c
    """ replace in visual mode
    vmap <C-v> c<ESC>"+p
    """ paste in insert mode
    imap <C-v> <ESC>"+pa
endif
"}}}

" toggle paste mode
nnoremap <leader>pp :set invpaste paste?<CR>

" toggle column numbers
nnoremap  <leader>n :set invnumber<CR>

" hidde matches
command! HideMatches :nohls
map <leader><CR> :<C-U>HideMatches<CR>

" select last paste
nnoremap <expr> lp '`[' . strpart(getregtype(), 0, 1) . '`]'
" find current word
nnoremap <leader>fw :execute "vimgrep ".expand("<cword>")." %"<CR>:copen<CR>

" tags {{{
  nnoremap tg <ESC><C-]>
  nnoremap tb <ESC><C-t>
  nnoremap ts :tselect<CR>
" }}}

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Plugin settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" NERDTree {{{
  nmap <silent><leader>e :NERDTreeToggle<CR>
  "" automatically close NERDTree when you open a file
  let NERDTreeQuitOnOpen = 1
  let NERDTreeIgnore = ['\.pyc$', '\.swp$']
  let NERDTreedirArrows = 1
  let g:NERDTreeWinSize=40
  "" custom directory color
  hi directory guifg=#ffee00 ctermfg=yellow
  let g:NERDTreedirarrowexpandable = '+'
  let g:NERDTreedirarrowcollapsible = '='
  " let NERDTreeminimalui = 1
  " let NERDTreeshowhidden=1
  augroup NERDTree
    autocmd!
    " close vim if the only window is a NERDTree
    autocmd BufEnter * if (winnr("$") == 1 &&
          \ exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
  augroup end
"}}}

"vim-BufTabLine {{{
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
"}}}

"vim-ignature {{{
  nnoremap <leader>m :SignatureToggleSigns<CR>
"}}}


"winresizer {{{
  let g:winresizer_start_key = '<leader>r'
"}}}

"vim-choosewin {{{
  nmap -  <Plug>(choosewin)
  let g:choosewin_overlay_enable = 1
  " NOTE: type '-' then 's' to swap windows
  "       type '-' '-'  to go to the previous window
"}}}

"undotree {{{
  nnoremap <silent> <leader>u :UndotreeToggle<CR>
  " open undotree in the right side
  let g:undotree_WindowLayout=3
  " short indicators for time (5 seconds ago) -> (5s)
  let g:undotree_ShortIndicators=1
  " undotree windows size
  let g:undotree_SplitWidt=50
  " autofocus undotree window
  let g:undotree_SetFocusWhenToggle=1
" }}}

"fzf.vim {{{
  "" easier to remember
  command! Search :BLines<space>
  " like ctrl+p
  nnoremap <leader>p :<C-U>Files<CR>
  " ctrl+p for tags
  nnoremap <leader>t :Tags <C-R><C-W><CR>

  " search in all buffers
  nnoremap <C-F> :<C-U>Lines<CR>
  " search only in current buffer
  nnoremap <C-S> :<C-U>BLines<CR>
  " buffer file switching
  nnoremap <TAB><TAB> :<C-U>Buffers<CR>
  " git commits
  nnoremap <localleader>g :<C-U>Commits<CR>
  " search current word using ripgrep
  nnoremap <silent> \ :Rg <C-R><C-W><CR>
  " commands
  nnoremap <silent> <localleader><localleader> :<C-U>Commands<CR>
"}}}

"ctrlp.vim {{{
"   let g:ctrlp_map = '<C-p>'
"   let g:ctrlp_custom_ignore = {
"         \ 'dir':  '\v[\/](node_modules)|(\.(swp|git|hg|svn))$',
"         \ 'file': '\v\.(exe|so|dll|class|png|jpg|jpeg|pyc)$'
"         \ }
"   if executable('ripgrep')
"     set grepprg=rg\ --color=never
"     " listing files
"     let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
"     " doesn't need cache
"     let g:ctrlp_use_caching = 0
"   elseif executable('ag')
"     " use ag over grep
"     set grepprg=ag\ --nogroup\ --nocolor
"     " use ag in ctrlp for listing files and respects .gitignore
"     let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
"     " doesn't need to cache
"     let g:ctrlp_use_caching = 0
"   else
"     noremap <F5> :CtrlPClearCache<CR>
"   endif
" "}}}

"vim-which-key {{{
  " by default timeoutlen is 1000 ms
  set timeoutlen=500
  " register dictionaries
  call which_key#register(',', "g:which_key_map_leader")
  call which_key#register('<space>', "g:which_key_map_localleader")
  " maps
  nnoremap <silent> <leader> :WhichKey ','<CR>
  noremap <silent> <localleader> :WhichKey '<space>'<CR>
  " hide windows swapping mappings
  let g:which_key_map_leader.1 = 'which_key_ignore'
  let g:which_key_map_leader.2 = 'which_key_ignore'
  let g:which_key_map_leader.3 = 'which_key_ignore'
  let g:which_key_map_leader.4 = 'which_key_ignore'
  let g:which_key_map_leader.5 = 'which_key_ignore'
  let g:which_key_map_leader.6 = 'which_key_ignore'
  let g:which_key_map_leader.7 = 'which_key_ignore'
  let g:which_key_map_leader.8 = 'which_key_ignore'
  let g:which_key_map_leader.9 = 'which_key_ignore'
  let g:which_key_map_leader.0 = 'which_key_ignore'
" }}}

"nerdcommenter {{{
  " add spaces after comment delimiters by default
  let g:NERDSpaceDelims = 1
  " enable trimming of trailing whitespace when uncommenting
  let g:NERDTrimTrailingWhitespace = 1
  " Allow commenting and inverting empty lines(useful when commenting a region)
  let g:NERDCommentEmptyLines = 1
"}}}

"auto-pairs {{{
  let g:AutoPairs =  {'(':')', '[':']', '{':'}'}
"}}}

"tagbar {{{
  nnoremap <silent><leader>tt :TagbarToggle<CR>
  " autofocus on tagbar open
  let g:tagbar_autofocus = 1
"}}}

"indentline {{{
  map <leader>i :IndentLinesToggle<CR>
  let g:indentLine_enabled = 0
"}}}

" " vim-gutentags {{{
"   " enable gtags module
"   let g:gutentags_modules = ['ctags']
"   " config project root markers.
"   let g:gutentags_project_root = ['.root']
"   " generate datebases in my cache directory,
"   " prevent gtags files polluting my project
"   let g:gutentags_cache_dir = expand('~/.cache/tags')
" " }}}

"ultisnips | vim-snippets {{{
  set rtp+=$HOME/.config/.snippets.vim/
  let g:UltiSnipsExpandTrigger="<TAB>"
  let g:UltiSnipsJumpBackwardTrigger="<c-j>"
  let g:UltiSnipsJumpForwardTrigger="<c-k>"
  let g:UltiSnipsListSnippets = "<c-l>"
  let g:ultisnips_python_style = "google"
  " let g:UltiSnipssnippetdirectories = ['~/.vim/ultisnips', 'ultisnips']
"}}}

"vim-signify {{{
  let g:signify_disable_by_default = 0
  nnoremap <leader>g :<C-U>SignifyToggle<CR>
"}}}

"vim-closetag {{{
  let g:closetag_filenames = "*.html,*.xhtml,*.phtml,*.blade.php"
" }}}

"python-syntax {{{
  let g:python_highlight_all = 1
"}}}

"completor.vim {{{
  " tab to trigger completion
  " let g:completor_auto_trigger = 0
  " inoremap <expr> <TAB> pumvisible() ?
        " \ "<C-N>" : "<C-R>=completor#do('complete')<CR>"
  " let g:completor_complete_options = 'menuone,noselect,preview'
" }}}

if !g:nvim_use_lsp
  "ale {{{
  let g:ale_fix_on_save = 0
  let g:ale_lint_on_enter = 0
  let g:ale_lint_on_text_changed = 'never'
  let g:ale_fixers = {
        \   '*': ['remove_trailing_lines', 'trim_whitespace'],
        \   'python': ['isort', 'black', 'yapf', 'autopep8']
        \ }
  nnoremap <localleader>al :ALELint<CR>
  nnoremap <localleader>af :ALEFix<CR>
  nnoremap <leader>= :ALEFix<CR>
  " let g:ale_python_flake8_options =
        " \ '--ignore=e129,e501,e302,e265,e241,e305,e402,w503'
  "}}}

  if has('python3') && executable('black')
    "black formatter {{{
      let g:black_linelength = 80
    "}}}
  elseif has('python') && executable('yapf')
    "yapf formatter {{{
      let g:yapf_style = "facebook"
      nnoremap <C-Y> :call Yapf()<CR>
    "}}}
  endif
endif

"YouCompleteme (ycm) {{{
if g:nvim_use_ycm
  " let g:ycm_auto_trigger = 0
  let g:ycm_autoclose_preview_window_after_insertion = 1
  let g:ycm_key_list_previous_completion  = ['<C-p>', '<Up>']
  let g:ycm_key_list_select_completion    = ['<C-n>', '<Down>']
  " let g:ycm_key_list_select_completion = ['<TAB>', '<Down>']

  nnoremap gd :YcmCompleter goto<CR>
  nnoremap gh :YcmCompleter getdoc<CR>

  " point ycm to the pipenv created virtualenv
  function! PipenvVenv()
    let l:pipenv_venv_path = system('pipenv --venv')
    if shell_error == 0
      let l:venv_path = substitute(pipenv_venv_path, '\n', '', '')
      let g:ycm_python_binary_path = l:venv_path . '/bin/python'
      " else
      " let g:ycm_python_binary_path = 'python'
    endif
  endfunction
  command! PipenvVenv call PipenvVenv()
  " nnoremap <silent> <leader>ve :PipenvVenv<CR>
endif
"}}}

"coc.nvim (LSP) {{{
if g:nvim_use_lsp
  " extensions to install when they aren't installed
  let g:coc_global_extensions = [
        \ 'coc-eslint', 'coc-prettier',
        \ 'coc-tsserver', 'coc-tslint', 'coc-tslint-plugin',
        \ 'coc-css', 'coc-json', 'coc-yaml', 'coc-python'
        \ ]

  " use tab for trigger completion with characters ahead and navigate.
  " use command ':verbose imap <TAB>' to make sure
  " tab is not mapped by other plugin.
  inoremap <silent><expr> <TAB>
        \ pumvisible() ? "\<C-n>" :
        \ <SID>check_back_space() ? "\<TAB>" :
        \ coc#refresh()
  inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

  command! LspExtension   :CocList extensions
  command! LspDiagnostics :CocList diagnostics
  command! LspCommands    :CocList commands
  command! LspInstall     :CocInstall
  command! LspUninstall   :CocUninstall
  command! LspConfig      :CocConfig
  command! LspYankHistory :CocList yank
  command! Format         :call CocAction('format')

  nnoremap <silent> <leader>= :call CocAction('format')<CR>

  " manage extensions
  nnoremap <silent> <localleader>lI :<C-U>CocInstall<space>
  nnoremap <silent> <localleader>lU :<C-U>CocUninstall<space>
  " list extensions
  nnoremap <silent> <localleader>le :<C-U>LspExtension<CR>
  " show diagnostic
  nnoremap <silent> <localleader>lw :<C-U>LspDiagnostics<CR>
  " edit config
  nnoremap <silent> <localleader>lC :<C-U>LspConfig<CR>
  " show commands
  nnoremap <silent> <localleader>lc :<C-U>LspCommands<CR>
  " fix
  nnoremap <silent> <localleader>lf :<C-U>Format<CR>
  " fix current
  nnoremap <silent> <localleader>lF <Plug>(coc-fix-current)
  " yank history
  nnoremap <silent> <space>p  :<C-U>LspYankHistory<CR>

  " use `[c` and `]c` to navigate diagnostics
  nnoremap <silent> gd :call CocAction('jumpDefinition')<CR>
  nnoremap <silent> gy :call CocAction('jumpTypeDefinition')<CR>
  nnoremap <silent> gi <Plug>(coc-implementation)<CR>
  nnoremap <silent> gr :call CocAction('jumpReferences')<CR>
  nnoremap <silent> gh :call <SID>show_documentation()<CR>

  " jump to next or prev diagnostic
  nmap <silent> <localleader>j <Plug>(coc-diagnostic-next)
  nmap <silent> <localleader>k <Plug>(coc-diagnostic-prev)

  " " search workspace symbols
  " nnoremap <silent> <localleader>s  :<C-U>CocList -i symbols<CR>

  nnoremap <silent> H :call <SID>show_documentation()<CR>

  " " remap for do codeaction of selected region,
  " " ex: `<leader>aap` for current paragraph
  " xmap <leader>a  <Plug>(coc-codeaction-selected)
endif
"}}}

"------------------------------------------------------------------------------
" colors & statusline
"------------------------------------------------------------------------------
" highlight search
hi search cterm=none ctermfg=white ctermbg=darkyellow
" vertical split color
hi vertsplit guibg=white guifg=white ctermbg=white ctermfg=white

 " statusline {{{{
 if !exists('g:gui_oni')

   function! GitBranch()
     if exists('*fugitive#head')
       return fugitive#head()
     endif
     return ''
   endfunction


   function! StatusLineGit()
     let l:branchname = GitBranch()
     return strlen(l:branchname) > 0?' '.l:branchname.' ':''
   endfunction

   function! StatusLineMode()
     let l:mode_map = {
           \ "n": 'NORMAL',
           \ "i": 'INSERT',
           \ 'R': 'REPLACE',
           \ 'v': 'VISUAL',
           \ 'V': 'V-LINE',
           \ "\<C-v>": 'V-BLOCK',
           \ 'c': 'COMMAND',
           \ 's': 'SELECT',
           \ 'S': 'S-LINE',
           \ "\<C-s>": 'S-BLOCK',
           \ 't': 'TERMINAL'
           \ }
     return get(l:mode_map, mode(), '')
   endfunction


   hi StMode
         \ term=bold cterm=none ctermbg=7 ctermfg=255
         \ guibg=#808080 guifg=#eeeeee
   hi StPasteMode
         \ term=bold cterm=none ctermbg=202 ctermfg=255
         \ guibg=#ff5f00 guifg=#eeeeee
   hi StBranch
         \ term=bold cterm=none ctermbg=107 ctermfg=255
         \ guibg=#87af5f guifg=#eeeeee
   hi StFilename
         \ term=bold cterm=none ctermbg=109 ctermfg=0
         \ guibg=#87afaf guifg=#000000
   hi StSeparator
         \ term=bold cterm=none ctermbg=236 ctermfg=255
         \ guibg=#303030 guifg=#eeeeee
   hi stposition
         \ term=bold cterm=none ctermbg=253 ctermfg=0
         \ guibg=#dadada guifg=#000000


   set statusline=
   " paste mode
   set statusline+=%#StPasteMode#
   set statusline+=%{&paste?\"\ \ paste\ \":\"\"}
   " vim mode
   " set statusline+=%#pmenusel#
   set statusline+=%#StMode#
   set statusline+=\ %{StatusLineMode()}\ 
   " git
   set statusline+=%#StBranch#
   set statusline+=%.90{StatusLineGit()}
   " filename
   set statusline+=%#StFilename#
   set statusline+=\ %f\ %m
   " blank space
   set statusline+=%#StSeparator#
   set statusline+=\ %=

   " " coc.nvim {{{
   if g:nvim_use_lsp
     set statusline+=\ %{coc#status()}%{get(b:,'coc_current_function','')}
   "   set statusline+=\ %{get(b:,'coc_current_function','')}\ \|
   " set statusline+=\ %{coc#status()}
   endif
   " " }}}

   " file encoding and file format
   set statusline+=\ %{&fileencoding?&fileencoding:&encoding}\ \|
   set statusline+=\ %{&fileformat}
   " FileType
   set statusline+=\ %y
   " set statusline+=%#position#
   " percentage
   set statusline+=\ \[%p%%\]
   " set statusline+=%#pmenusel#
   " line and column
   set statusline+=\ %l:%c\  " .
   " end of statusline
   set statusline+=%0*

   " syntax clear statuslinenc
   " hi! statuslinenc term=none cterm=none ctermbg=white ctermfg=white
 endif
 "-----------------------------------------------------------------------------

" vim: ft=vim et ts=2 sts=2 sw=2:
