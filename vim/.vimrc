filetype off                 

" leader
let mapleader = ","


if has("nvim")
    call plug#begin('~/.local/share/nvim/plugged')
else
    call plug#begin('~/.vim/plugged')
endif

"---------------------------------------------------------------------------------------------------

" LIBRARIES FOR VIM PLUGINS {{{
    Plug 'tomtom/tlib_vim'
    Plug 'tpope/vim-repeat'
"}}}


" UI {{{
    Plug 'flazz/vim-colorschemes'
"}}}


" UTILITIES {{{

    Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle'  } " {{{
        " close vim if the only window is a NERDTree
        autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
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
    "}}}

    " Show marks
    Plug 'kshenoy/vim-signature' " {{{
        nnoremap <Leader>m :SignatureToggleSigns<CR>
    "}}}

    " undo history
    Plug 'mbbill/undotree' " {{{
        nnoremap <leader>u :UndotreeToggle<cr>
        if has("persistent_undo")
            set undodir=$HOME."/.vim/undodir"
            set undofile
        endif
    "}}}

    " Switch between buffers
    Plug 'ap/vim-buftabline' " {{{
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

    " Plug 'rking/ag.vim'

    " CtrlP, ripgrep / Silver searcher
    Plug 'ctrlpvim/ctrlp.vim' " {{{
        let g:ctrlp_map = '<Leader>p'
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

    " delete buffers (close files) without closing the window
    " Plug 'moll/vim-bbye' " {{{
    "   nnoremap <Leader>q :Bdelete<CR>
    nnoremap <Leader>q :bp\|bd #<CR>
    "}}}

    " swap windows easily
    Plug 'wesQ3/vim-windowswap'

    " resizing splits easily
    Plug 'simeji/winresizer' " {{{
        let g:winresizer_start_key = '<Leader>r'
    "}}}

    " Navigate to the window you choose
    Plug 't9md/vim-choosewin' " {{{
        nmap -  <Plug>(choosewin)
        let g:choosewin_overlay_enable = 1
    "}}}

"}}}


" DEV TOOLS {{{

    " Asynchronous Lint Engine
    Plug 'w0rp/ale' " {{{
        " if you don't want linters to run on opening a file
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
        " let g:ale_python_flake8_options = '--ignore=E129,E501,E302,E265,E241,E305,E402,W503'
    "}}}

    Plug 'maralla/completor.vim' " {{{
        " tab to trigger completion
        " let g:completor_auto_trigger = 0
        " inoremap <expr> <Tab> pumvisible() ? "<C-N>" : "<C-R>=completor#do('complete')<CR>"
        " let g:completor_complete_options = 'menuone,noselect,preview'
    "}}}

    Plug 'scrooloose/nerdcommenter' " {{{
        " Add spaces after comment delimiters by default
        let g:NERDSpaceDelims = 1
        " Enable trimming of trailing whitespace when uncommenting
        let g:NERDTrimTrailingWhitespace = 1
    "}}}

    " ctrlp for functions
    Plug 'tacahiroy/ctrlp-funky' " {{{
        nnoremap <Leader>fu :CtrlPFunky<Cr>
        " narrow the list down with a word under cursor
        nnoremap <Leader>fU :execute 'CtrlPFunky ' . expand('<cword>')<Cr>
        let g:ctrlp_funky_multi_buffers = 1
        let g:ctrlp_funky_matchtype = 'path'
    "}}}

    Plug 'majutsushi/tagbar' " {{{
        nnoremap <Leader>tt :TagbarToggle<CR>
    "}}}

    " show identation
    Plug 'Yggdroot/indentLine' " {{{
        map <leader>i :IndentLinesToggle<CR>
        let g:indentLine_enabled = 0
    "}}}

    Plug 'jiangmiao/auto-pairs' " {{{
        let g:AutoPairs =  {'(':')', '[':']', '{':'}'}
    "}}}

    Plug 'mattn/emmet-vim'

    Plug 'airblade/vim-gitgutter' " {{{
        nnoremap <Leader>g :GitGutterToggle<Cr>
        let g:gitgutter_override_sign_column_highlight = 0
    "}}}

    Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets' " {{{
        let g:UltiSnipsExpandTrigger="<tab>"
        let g:UltiSnipsJumpForwardTrigger="<c-j>"
        let g:UltiSnipsJumpBackwardTrigger="<c-k>"
        let g:UltiSnipsListSnippets       = "<c-l>"
        let g:ultisnips_python_style = "google"
        " let g:UltiSnipsSnippetDirectories = ['~/.vim/UltiSnips', 'UltiSnips']
    "}}}

    Plug 'editorconfig/editorconfig-vim'

"}}}


" PYTHON {{{

    Plug 'vim-python/python-syntax', {'for': 'python'} " {{{
        let g:python_highlight_all = 1
    "}}}

    " if has python3 & black
    if has('python3') && executable('black')
        Plug 'psf/black', {'for': 'python'} " {{{
            let g:black_linelength = 80 
        "}}}
        
    " if has python2 & yapf
    elseif has('python') && executable('yapf')
        " Plug 'google/yapf', { 'rtp': 'plugins/vim', 'for': 'python' } " {{{
            " let g:pymode_doc = 1
            " map <C-Y> :call yapf#YAPF()<cr>
            " imap <C-Y> <c-o>:call yapf#YAPF()<cr>
        "}}}
        Plug 'mindriot101/vim-yapf', {'for': 'python'} "{{{
            let g:yapf_style = "facebook"     
            nnoremap <C-Y> :call Yapf()<cr>
        "}}}
    endif

    if has('python') || has('python3')
        Plug 'Valloric/YouCompleteMe', { 'do': './install.py'  } " {{{
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
        "}}}

        if has("nvim")
            Plug 'numirias/semshi'
        endif
    end
"}}}


" ANSIBLE {{{
    Plug 'pearofducks/ansible-vim' "{{{
        au BufRead,BufNewFile */playbooks/*.yml set filetype=yaml.ansible
    "}}}
"}}}


"BATS {{{
    " highlight for bats files
    au BufRead,BufNewFile *.bats set filetype=sh
"}}}


" HTML5
Plug 'othree/html5.vim'


" JS {{{
    Plug 'pangloss/vim-javascript', {'for': 'javscript'}
    Plug 'myhere/vim-nodejs-complete', {'for': 'javscript'}
"}}}

" Vue {{{
    Plug 'posva/vim-vue'
    autocmd FileType vue setlocal ts=2 sts=2 sw=2 expandtab
"}}}

" YAML {{{
    autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
"}}}


" TOML {{{
    Plug 'cespare/vim-toml', {'for': 'toml'}
"}}}


" JSON {{{
    command! JSONFormat %!python -m json.tool
    " noremap <leader>json :%!python -m json.tool
"}}}


" Add plugins to &runtimepath
call plug#end()


"---------------------------------------------------------------------------------------------------


filetype plugin indent on

if exists('g:gui_oni')
    set nocompatible    " be iMprobed, required
    set mouse=a
    set noshowmode
    set noruler
    set laststatus=0
    set noshowcmd
else
    set ruler           " cursorline and column
    set showmode        " message on status line to show current mode
    set laststatus=2    " when last window has status lines
    set showcmd         " show (partial) command in status line
endif

set novb
set t_vb=
set background=dark
set enc=utf-8                " default encoding
set fileformats=unix,dos,mac " unix over windows over os9 formats
set scrolloff=5              " keep some more lines for scope

set number          " linenumber

set hlsearch        " highlight matches with last search pattern
set smartindent     " smart autoindenting
set ignorecase      " ignore case in search patterns
set smartcase       " no ignore case when pattern has uppercase
set incsearch       " highlight match while typing search pattern

set visualbell      " use visual bell instead of beeping
set ttyfast         " indicates a fast terminal connection
set autoread        " Reload files changed outside vim

set cursorline      " highlight the screen line of the cursor
set gcr=a:blinkon0  " disable cursor blink
set wildmenu        " use menu for command line completion
set wildmode=list:longest " mode for 'wildchar' command-line expansion
set wildignore+=*.dll,*.o,*.pyc,*.bak,*.exe,*.jpg,*.jpeg,*.png,*.gif,*$py.class,*.class,*/*.dSYM/*,*.dylib

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
" set completeopt-=preview    " disable docs functions
set splitbelow      " preview windows at bottom

syntax enable
syntax on

set t_ut=
set t_Co=256

if !has("gui_running") && !has('nvim')
    set term=xterm
    set t_Co=256
    let &t_AB="\e[48;5;%dm"
    let &t_AF="\e[38;5;%dm"
    " " colorscheme zenburn
    inoremap <Char-0x07F> <BS>
    nnoremap <Char-0x07F> <BS>
endif

try
    colorscheme Monokai
    " highlight LineNr term=bold cterm=NONE ctermfg=DarkGrey ctermbg=NONE gui=NONE guifg=DarkGrey guibg=NONE
catch /^Vim\%((\a\+)\)\=:E185/
    colorscheme elflord
endtry


"---------------------------------------------------------------------------------------------------


" Remember line position
au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"" | endif

" Remove all trailing whitespaces
autocmd FileType c,cpp,perl,python,sh,yml autocmd BufWritePre <buffer> %s/\s\+$//e

" Custom task tags, highlight TODO, FIXME, NOTE, etc. {{{
if v:version > 701
    "autocmd Syntax * call matchadd('Todo',  '\W\zs\(TODO\|FIXME\|CHANGED\|XXX\|BUG\|HACK\)')
    "autocmd Syntax * call matchadd('Debug', '\W\zs\(NOTE\|INFO\|IDEA\)')
    autocmd Syntax * call matchadd('Todo', '\W\zs\(TODO\|FIXME\|CHANGED\|XXX\|BUG\|HACK\|NOTE\|INFO\|IDEA\)')
endif
"}}}


"---------------------------------------------------------------------------------------------------


"Reload .vimrc
noremap <silent> <leader>rr :source ~/.vimrc<CR>:filetype detect<CR>:exe ":echo 'vimrc reloaded'"<CR>

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
    """ copy in visual mode
    vmap <C-c> "+yi
    """ cut in visual mode
    vmap <C-x> "+c
    """ replace in visual mode
    vmap <C-v> c<ESC>"+p
    """ paste in insert mode
    imap <C-v> <ESC>"+pa
    """ copy line to clipboard
    map <Leader>y <ESC>"+yy
endif
"}}}

" Append modeline after last line in buffer {{{
function! AppendModeline()
  let l:modeline = printf(" vim: set ts=%d sw=%d tw=%d %set :",
        \ &tabstop, &shiftwidth, &textwidth, &expandtab ? '' : 'no')
  let l:modeline = substitute(&commentstring, "%s", l:modeline, "")
  call append(line("$"), l:modeline)
endfunction

nnoremap <silent> <Leader>ml :call AppendModeline()<CR>
"}}}

" Zoom / Restore window. {{{
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

command! ZoomToggle call s:ZoomToggle()
nnoremap <silent> <Leader>zz :ZoomToggle<CR>
" NOTE: we can use: c-w-| (vsplits), c-w-_ (hsplits), C-w-= (restore)
" noremap <silent> zz <c-w>_ \| <c-w>\|
" noremap <silent> zo <c-w>=
"}}}

" Fast saving
nmap <leader>w :w!<cr>

" Delete all buffers
map <leader>qa :bufdo bd<cr>

" Toggle paste mode
nmap <leader>pp :set invpaste paste?<CR>

" Toggle column numbers
map <Leader>n :set invnumber<CR>

" select last paste
nnoremap <expr> gp '`[' . strpart(getregtype(), 0, 1) . '`]'
" find current word
nnoremap <leader>fw :execute "vimgrep ".expand("<cword>")." %"<cr>:copen<cr>

" break line
nmap <leader>b i<cr><esc>k$

" Hidde matches
map <Leader><CR> :nohls<CR>

" Switch CWD to the directory of the open buffer
map <leader>cd :cd %:p:h<cr>:pwd<cr>


"---------------------------------------------------------------------------------------------------


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
"}}}
