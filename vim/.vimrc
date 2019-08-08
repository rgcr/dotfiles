"------------------------------------------------------------------------------
" Enable only one of the below variables

" variable to enable/disable coc.nvim plugins, settings and shorcuts easily
let g:vimrc_use_coc_nvim = 1
" variable to enable/disable YouCompleteMe plugins, settings and shortcuts easily
let g:vimrc_use_ycm = 0
"------------------------------------------------------------------------------


filetype off                 

if has("nvim")
  call plug#begin('~/.local/share/nvim/plugged')
else
  call plug#begin('~/.vim/plugged')
endif


" Plugins
"------------------------------------------------------------------------------
" Libraries for vim plugins 
Plug 'tomtom/tlib_vim'
Plug 'tpope/vim-repeat'

" UI 
Plug 'flazz/vim-colorschemes'

"Utilities
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle'  }   " file explorer
Plug 'ap/vim-buftabline'                                  " switch between buffers
Plug 'kshenoy/vim-signature'                              " show marks
Plug 'ctrlpvim/ctrlp.vim'                                 " ctrlp, rg / silver searcher
Plug 'wesQ3/vim-windowswap'                               " swap windows easily
Plug 'simeji/winresizer'                                  " resize windows
Plug 't9md/vim-choosewin'                                 " switch between windows 

"" Edit
Plug 'editorconfig/editorconfig-vim'
Plug 'scrooloose/nerdcommenter'                 " comment code
Plug 'jiangmiao/auto-pairs'                     " for brackets, parens, quoutes
Plug 'mattn/emmet-vim'                          " for html

"" Dev utilities 
" Plug 'maralla/completor.vim'                    " completion
Plug 'tacahiroy/ctrlp-funky'                    " ctrlp for functions
Plug 'majutsushi/tagbar'                        " show tags
Plug 'Yggdroot/indentLine'                      " show indentation
" Plug 'plytophogy/vim-virtualenv'

"" Git
" Plug 'airblade/vim-gitgutter'                   " shows git diff in the sign column

"" Snippets
Plug 'SirVer/ultisnips'                         
Plug 'honza/vim-snippets'                       


" collection of language packs
" Plug 'sheerun/vim-polyglot'
"
"" Toml
Plug 'cespare/vim-toml', {'for': 'toml'}
"" Ansible
Plug 'pearofducks/ansible-vim' 
"" HTML5
Plug 'othree/html5.vim'
"" JS
Plug 'pangloss/vim-javascript', {'for': 'javscript'}
Plug 'myhere/vim-nodejs-complete', {'for': 'javscript'}
"" VueJS
Plug 'posva/vim-vue'

if g:vimrc_use_ycm
  "" Python
  if has('python') || has('python3')
    Plug 'Valloric/YouCompleteMe', { 'do': './install.py --all'  }
  endif
endif

if g:vimrc_use_coc_nvim
  "" Intellisense engine for Vim8/Neovim
  Plug 'neoclide/coc.nvim', {'branch': 'release'}
else
  "" Asynchronous lint engine
  Plug 'w0rp/ale'
  "" Python
  Plug 'vim-python/python-syntax', {'for': 'python'}
  Plug 'python-mode/python-mode', { 'branch': 'develop' }
  if executable('black')
    Plug 'psf/black', {'for': 'python'}
  elseif executable('yapf')
    Plug 'mindriot101/vim-yapf', {'for': 'python'}
  endif
endif

" Add plugins to &runtimepath
call plug#end()
filetype plugin indent on
"------------------------------------------------------------------------------


" Vim settings
"------------------------------------------------------------------------------
if exists('g:gui_oni')
    set nocompatible        " be iMprobed, required
    set mouse=a
    set noshowmode
    set noruler
    set laststatus=0
    set noshowcmd
else
    set ruler               " cursorline and column
    set showmode            " message on status line to show current mode
    set laststatus=2        " when last window has status lines
    set showcmd             " show (partial) command in status line
endif

set novb
set t_vb=
set background=dark
set enc=utf-8                " default encoding
set fileformats=unix,dos,mac " unix over windows over os9 formats
set scrolloff=5              " keep some more lines for scope

set number                  " linenumber

set hlsearch                " highlight matches with last search pattern
set smartindent             " smart autoindenting
set ignorecase              " ignore case in search patterns
set smartcase               " no ignore case when pattern has uppercase
set incsearch               " highlight match while typing search pattern

set visualbell              " use visual bell instead of beeping
set ttyfast                 " indicates a fast terminal connection
set autoread                " Reload files changed outside vim

set cursorline              " highlight the screen line of the cursor
set gcr=a:blinkon0          " disable cursor blink
set wildmenu                " use menu for command line completion
set wildmode=list:longest   " mode for 'wildchar' command-line expansion

set wildignore+=*.dll,*.o,*.pyc,*.bak,*.exe,*.jpg,
set wildignore+=*.jpeg,*.png,*.gif,*$py.class,
set wildignore+=.class,*/*.dSYM/*,*.dylib

set tabstop=4               " number of spaces that <tab> in file uses
set shiftwidth=4            " number of spaces to use for (auto)indent
set softtabstop=4           " number of spaces that <tab> uses while editing
set expandtab               " use spaces when <tab> is inserted

set nowrap                  " long lines
set noswapfile              " whether to use a swapfile for a buffer
set nocompatible            " behave vi-compatible as much as possible
set modeline                " recognize modelines at start or end of file
set modelines=5             " number of lines checked for modelines
set backspace=indent,eol,start

" set completeopt-=preview  " disable docs functions
set splitbelow              " preview windows at bottom

" set cmdheight=2
set updatetime=300
" don't give |ins-completion-menu| messages.
" set shortmess+=c


syntax enable
syntax on

set t_ut=
set t_Co=256

if !has("gui_running") && !has('nvim')
    set term=xterm
    set t_Co=256
    let &t_AB="\e[48;5;%dm"
    let &t_AF="\e[38;5;%dm"
    inoremap <Char-0x07F> <BS>
    nnoremap <Char-0x07F> <BS>
endif

try
    colorscheme Monokai
    " highlight LineNr term=bold cterm=NONE ctermfg=DarkGrey ctermbg=NONE gui=NONE guifg=DarkGrey guibg=NONE
catch /^Vim\%((\a\+)\)\=:E185/
    colorscheme elflord
endtry
"------------------------------------------------------------------------------


" Filetype-Specific Configurations
"------------------------------------------------------------------------------
" Custom task tags
if v:version > 701
    autocmd Syntax * call matchadd('Todo', '\W\zs\(TODO\|FIXME\|CHANGED\|XXX\|BUG\|HACK\|NOTE\|INFO\|IDEA\)')
endif

" Remember line position
au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

" Remove all trailing whitespaces
autocmd FileType c,cpp,perl,python,sh,yml autocmd BufWritePre <buffer> %s/\s\+$//e

" vuejs
autocmd FileType vue setlocal ts=2 sts=2 sw=2 expandtab
" yaml
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
" bats 
au BufRead,BufNewFile *.bats set filetype=sh
"------------------------------------------------------------------------------


" Functions
"------------------------------------------------------------------------------
" Append modeline after last line in buffer
function! AppendModeline()
  let l:modeline = printf(" vim: set ts=%d sw=%d tw=%d %set :",
        \ &tabstop, &shiftwidth, &textwidth, &expandtab ? '' : 'no')
  let l:modeline = substitute(&commentstring, "%s", l:modeline, "")
  call append(line("$"), l:modeline)
endfunction

" Zoom / Restore window 
function! s:ZoomToggle() abort
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



"------------------------------------------------------------------------------


" Custom commands
"------------------------------------------------------------------------------
" json format
command! JSONFormat %!python -m json.tool

if g:vimrc_use_coc_nvim
  " coc.nvim
  command! -nargs=0 Format :call CocAction('format')
endif

"------------------------------------------------------------------------------


" Custom Mappings
"------------------------------------------------------------------------------
let mapleader = ","

" Put : command on ; for easer access
" nnoremap ; :

"Reload .vimrc
noremap <silent> <leader>rr :source ~/.vimrc<CR>:filetype detect<CR>:exe ":echo 'vimrc reloaded'"<CR>

" delete buffers (close files) without closing the window
" like vim-bbye 
nnoremap <Leader>q :bp\|bd #<CR>    

" clipboard, copy & paste {{{
" Windows
if executable("clip.exe")
    func! SelectedText()
        normal gv"xy
        let result = getreg("x")
        return result
    endfunc
    """ copy visual selection to clipboard
    vnoremap <C-c> :call system('clip.exe', SelectedText())<CR>
    """ cut
    noremap <C-x> :call system('clip.exe', SelectedText())<CR>gvx
" Linux
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

" append modeline
nnoremap <silent> <Leader>ml :call AppendModeline()<CR>

" zoom buffer
command! ZoomToggle call s:ZoomToggle()
nnoremap <silent> <Leader>zz :ZoomToggle<CR>
" NOTE: we can use: c-w-| (vsplits), c-w-_ (hsplits), C-w-= (restore)
" noremap <silent> zz <c-w>_ \| <c-w>\|
" noremap <silent> zo <c-w>=

" Fast saving
nmap <leader>w :w!<cr>

" Delete all buffers
map  <leader>qa :bufdo bd<cr>

" Toggle paste mode
nmap <leader>pp :set invpaste paste?<CR>

" Toggle column numbers
map  <Leader>n :set invnumber<CR>

" break line
nmap <leader>b i<cr><esc>k$

" Hidde matches
map <Leader><CR> :nohls<CR>

" Switch CWD to the directory of the open buffer
map <leader>cd :cd %:p:h<cr>:pwd<cr>

" select last paste
nnoremap <expr> gp '`[' . strpart(getregtype(), 0, 1) . '`]'
" find current word
nnoremap <leader>fw :execute "vimgrep ".expand("<cword>")." %"<cr>:copen<cr>


"coc.nvim {{{
if g:vimrc_use_coc_nvim
  nnoremap <silent> cc :CocConfig<CR>

  " Use `:Format` to format current buffer
  noremap <silent> <leader>= :call CocAction('format')<CR>

  nnoremap <silent> K :call <SID>show_documentation()<CR>

  " Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
  " xmap <leader>a  <Plug>(coc-codeaction-selected)
  " nmap <leader>a  <Plug>(coc-codeaction-selected)

  " Use tab for trigger completion with characters ahead and navigate.
  " Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
  inoremap <silent><expr> <TAB>
        \ pumvisible() ? "\<C-n>" :
        \ <SID>check_back_space() ? "\<TAB>" :
        \ coc#refresh()
  inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

  nmap <silent> gd <Plug>(coc-definition)
  nmap <silent> gy <Plug>(coc-type-definition)
  nmap <silent> gi <Plug>(coc-implementation)
  nmap <silent> gr <Plug>(coc-references)

  "coc-snippets {{{
  " inoremap <silent><expr> <TAB>
  " \ pumvisible() ? coc#_select_confirm() :
  " \ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
  " \ <SID>check_back_space() ? "\<TAB>" :
  " \ coc#refresh()
  "let g:coc_snippet_next = '<tab>'
  "}}}

  " coc-yank {{{
  " nnoremap <silent> <space>y  :<C-u>CocList -A --normal yank<cr>
  nnoremap <silent> <leader>p :CocList yank<cr>
  "}}}
  
endif
"}}}

"------------------------------------------------------------------------------


" Plugin settings
"------------------------------------------------------------------------------
"nerdfree {{{
  map <leader>t :NERDTreeToggle<CR>
  "" automatically close NerdTree when you open a file
  let NERDTreeQuitOnOpen = 1
  let NERDTreeIgnore = ['\.pyc$', '\.swp$']
  let NERDTreeDirArrows = 1
  let g:NERDTreeWinSize=40
  "" custom directory color
  hi Directory guifg=#ffee00 ctermfg=yellow
  let g:NERDTreeDirArrowExpandable = '+'
  let g:NERDTreeDirArrowCollapsible = '='
  " let NERDTreeMinimalUI = 1
  " let NERDTreeShowHidden=1
  " close vim if the only window is a NERDTree
  autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
"}}}

"vim-buftabline {{{
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

"vim-signature {{{
  nnoremap <Leader>m :SignatureToggleSigns<CR>
"}}}

"ctrlp.vim {{{
  let g:ctrlp_map = '<C-p>'
  let g:ctrlp_custom_ignore = {
        \ 'dir':  '\v[\/](node_modules)|(\.(swp|git|hg|svn))$',
        \ 'file': '\v\.(exe|so|dll|class|png|jpg|jpeg|pyc)$'
        \ }
  if executable('ripgrep')
    set grepprg=rg\ --color=never
    " listing files
    let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
    " doesn't need cache
    let g:ctrlp_use_caching = 0
  elseif executable('ag')
    " Use ag over grep
    set grepprg=ag\ --nogroup\ --nocolor
    " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
    let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
    " doesn't need to cache
    let g:ctrlp_use_caching = 0
  else
    noremap <F5> :CtrlPClearCache<CR>
  endif
"}}}

"winresizer {{{
  let g:winresizer_start_key = '<Leader>r'
"}}}

"vim-choosewin {{{
  nmap -  <Plug>(choosewin)
  let g:choosewin_overlay_enable = 1
"}}}

"nerdcommenter {{{
  " Add spaces after comment delimiters by default
  let g:NERDSpaceDelims = 1
  " Enable trimming of trailing whitespace when uncommenting
  let g:NERDTrimTrailingWhitespace = 1
"}}}

"auto-pairs {{{
  let g:AutoPairs =  {'(':')', '[':']', '{':'}'}
"}}}


"completor.vim {{{
  " tab to trigger completion
  " let g:completor_auto_trigger = 0
  " inoremap <expr> <Tab> pumvisible() ? "<C-N>" : "<C-R>=completor#do('complete')<CR>"
  " let g:completor_complete_options = 'menuone,noselect,preview'
"}}}

"ctrlp-funky {{{
  nnoremap <Leader>fu :CtrlPFunky<Cr>
  " narrow the list down with a word under cursor
  nnoremap <Leader>fU :execute 'CtrlPFunky ' . expand('<cword>')<Cr>
  let g:ctrlp_funky_multi_buffers = 1
  let g:ctrlp_funky_matchtype = 'path'
"}}}

"tagbar {{{
  nnoremap <Leader>tt :TagbarToggle<CR>
"}}}

"indentLine {{{
  map <leader>i :IndentLinesToggle<CR>
  let g:indentLine_enabled = 0
"}}}

"vim-gitgutter {{{
  nnoremap <Leader>g :GitGutterToggle<Cr>
  let g:gitgutter_override_sign_column_highlight = 0
"}}}

"ultisnips | vim-snippets {{{
  let g:UltiSnipsExpandTrigger="<tab>"
  let g:UltiSnipsJumpForwardTrigger="<c-j>"
  let g:UltiSnipsJumpBackwardTrigger="<c-k>"
  let g:UltiSnipsListSnippets       = "<c-l>"
  let g:ultisnips_python_style = "google"
  " " let g:UltiSnipsSnippetDirectories = ['~/.vim/UltiSnips', 'UltiSnips']
"}}}

if !g:vimrc_use_coc_nvim 
  "ale {{{ 
  let g:ale_fix_on_save = 0
  let g:ale_lint_on_enter = 0
  let g:ale_lint_on_text_changed = 'never'
  let g:ale_fixers = {
        \   '*': ['remove_trailing_lines', 'trim_whitespace'],
        \   'python': ['isort', 'black', 'yapf', 'autopep8']
        \ }
  nnoremap <Leader>al :ALELint<CR>
  nnoremap <Leader>af :ALEFix<CR>
  nnoremap <Leader>= :ALEFix<CR>
  " " let g:ale_python_flake8_options = '--ignore=E129,E501,E302,E265,E241,E305,E402,W503'
  "}}}
  
  ""python-syntax {{{
  let g:python_highlight_all = 1
  "}}}

  if has('python3') && executable('black')
    "black settings here:
    let g:black_linelength = 80 
  elseif has('python') && executable('yapf')
    "yapf settings here:
    let g:yapf_style = "facebook"     
    nnoremap <C-Y> :call Yapf()<cr>
  endif
endif


"YouCompleteMe {{{
if g:vimrc_use_ycm
  " let g:ycm_auto_trigger = 0
  let g:ycm_autoclose_preview_window_after_insertion = 1
  let g:ycm_key_list_previous_completion  = ['<C-p>', '<Up>']
  let g:ycm_key_list_select_completion    = ['<C-n>', '<Down>']
  " let g:ycm_key_list_select_completion = ['<TAB>', '<Down>']

  nnoremap <leader>jd :YcmCompleter GoTo<CR>
  nnoremap <leader>jh :YcmCompleter GetDoc<CR>

  " Point YCM to the Pipenv created virtualenv
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
  " nnoremap <silent> <Leader>ve :PipenvVenv<CR>
endif
"}}}
  
"coc.nvim {{{
if g:vimrc_use_coc_nvim
  let g:coc_global_extensions = [
        \ 'coc-eslint', 'coc-prettier',
        \ 'coc-tsserver', 'coc-tslint', 'coc-tslint-plugin',
        \ 'coc-css', 'coc-json', 'coc-yaml', 'coc-python'
        \ ]
endif
"}}}

"------------------------------------------------------------------------------


" Colors & Statusline
"------------------------------------------------------------------------------
" hls color
hi Search cterm=NONE ctermfg=White ctermbg=DarkYellow
" vertical split color
hi VertSplit ctermbg=White ctermfg=White

" STATUSLINE {{{{
if !exists('g:gui_oni')
  function! GitBranch()
    return system("git rev-parse --abbrev-ref HEAD 2>/dev/null | tr -d '\n'")
  endfunction

  function! StatuslineGit()
    let l:branchname = GitBranch()
    return strlen(l:branchname) > 0?'| '.l:branchname.' ':''
  endfunction

  function! StatuslineMode()
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

  hi User1 term=bold cterm=None ctermbg=202 ctermfg=255
  hi User2 term=bold cterm=None ctermbg=249 ctermfg=233
  hi User3 term=bold cterm=None ctermbg=253 ctermfg=0
  hi User4 term=bold cterm=None ctermbg=109 ctermfg=0

  set statusline=
  set statusline+=%1*
  set statusline+=%{&paste?\"\ \ PASTE\ \":\"\"}
  set statusline+=%#PmenuSel#
  "set statusline+=%2*
  set statusline+=\ %{StatuslineMode()}
  " XXX: too slow
  "set statusline+=\ %.90{StatuslineGit()}
  set statusline+=\ %3*
  set statusline+=\ %f
  set statusline+=\ %m
  "set statusline+=%#LineNr#
  set statusline+=\ %4*
  set statusline+=\ %=
  
  " coc.nvim {{{
  " if g:vimrc_use_coc_nvim
    " set statusline+=\ %{coc#status()}%{get(b:,'coc_current_function','')}\ \|
    " set statusline+=\ %{get(b:,'coc_current_function','')}\ \|
    " set statusline+=\ %{coc#status()}
  " endif
  "" }}}
  " set statusline+=\ %{virtualenv#statusline()}\ \|
  
  set statusline+=%4*
  "set statusline+=%#CursorColumn#
  set statusline+=\ %{&fileencoding?&fileencoding:&encoding}\ \|
  set statusline+=\ %{&fileformat}
  set statusline+=\ %y
  set statusline+=\ %3*
  set statusline+=\ %p%%\  " .
  set statusline+=%#PmenuSel#
  set statusline+=\ %l:%c\  " .
  "set statusline+=%2*
  set statusline+=%0*  " end of statusline

  syntax clear StatusLineNC
  hi! StatusLineNC term=None cterm=None ctermbg=white ctermfg=white
endif
"------------------------------------------------------------------------------

" vim: ft=vim et ts=2 sts=2 sw=2:
