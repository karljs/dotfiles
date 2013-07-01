"         _                    
"  __   _(_)_ __ ___  _ __ ___ 
"  \ \ / / | '_ ` _ \| '__/ __|
"   \ V /| | | | | | | | | (__ 
"  (_)_/ |_|_| |_| |_|_|  \___|
" 

"-------------------------------------------------------------------------------
" Vundle
set nocompatible
filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

" github
Bundle 'altercation/vim-colors-solarized'
" Bundle 'kien/ctrlp.vim'
Bundle 'kien/rainbow_parentheses.vim'
Bundle 'tpope/vim-markdown'
" Bundle 'tpope/vim-surround'
" Bundle 'tpope/vim-fugitive'
" Bundle 'xolox/vim-easytags'

" vim-scripts
Bundle 'Align'
Bundle 'darktango.vim'
Bundle 'inkpot'
Bundle 'tComment'
Bundle 'twilight'
Bundle 'Wombat'
Bundle 'Zenburn'

" other
" Bundle 'git://git.wincent.com/command-t.git'

filetype plugin indent on

"-------------------------------------------------------------------------------
" Simple tweaks
set ttyfast
set shortmess+=I
set noesckeys

"-------------------------------------------------------------------------------
" Tabs & Indent
set expandtab
set softtabstop=2
set shiftwidth=2
set autoindent

"-------------------------------------------------------------------------------
" Programming tool
set nowrap
set backspace=indent,eol,start
set nostartofline
set hidden
set autochdir
set mouse=a
set autoread

"-------------------------------------------------------------------------------
" Aesthetics
set number
set laststatus=2
set ruler
set showcmd
set showmatch
" set colorcolumn=80


"-------------------------------------------------------------------------------
" Searching
set incsearch
set smartcase

"-------------------------------------------------------------------------------
" Wildmenu and Ctrl-P
set wildmenu
set wildmode=list:longest,full
set wildignore+=.*,*.so,*.swp,*.zip
set wildignore+=*/tmp/*,*/Library/*,*/Music/*,*/Pictures/*,/Users/karl/src/*,*/Downloads/*

"-------------------------------------------------------------------------------
" Tagbar 
nmap <c-s> :TagbarToggle<CR>

"-------------------------------------------------------------------------------
" Ctrl-P
nmap <leader>p :CtrlP ~<CR>
nmap <leader>b :CtrlPBuffer<CR>
let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']

"-------------------------------------------------------------------------------
" EasyTags
let g:easytags_languages = {
\   'haskell': {
\       'cmd': '~/.cabal/bin/lushtags',
\       'args': [],
\       'fileoutput_opt': '-f',
\       'stdout_opt': '-f-',
\       'recurse_flag': '-R'
\   }
\}

"-------------------------------------------------------------------------------
" Colors
syntax on
let g:solarized_italic=0
let g:solarized_bold=0
set background=light
colorscheme solarized

"-------------------------------------------------------------------------------
" GUI/Terminal specific tweaks
if has("gui_running")
  set gfn=Source\ Code\ Pro:h15
  set guioptions-=T
  set guioptions-=r
  set guioptions-=L
else
  set t_Co=256
endif

"-------------------------------------------------------------------------------
" Keybindings
nnoremap <silent> [b :bprevious<CR>
nnoremap <silent> ]b :bnext<CR>
nnoremap <silent> [a :previous<CR>
nnoremap <silent> ]a :next<CR>

nmap <leader>v :setlocal paste! paste?<cr>
