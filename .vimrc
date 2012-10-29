"         _                    
"  __   _(_)_ __ ___  _ __ ___ 
"  \ \ / / | '_ ` _ \| '__/ __|
"   \ V /| | | | | | | | | (__ 
"  (_)_/ |_|_| |_| |_|_|  \___|
" 

set nocompatible
filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

" github
Bundle 'altercation/vim-colors-solarized'
Bundle 'kien/ctrlp.vim'
Bundle 'scrooloose/nerdcommenter'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-fugitive'

" vim-scripts
Bundle 'Align'
Bundle 'desert.vim'
Bundle 'jellybeans.vim'
Bundle 'Wombat'
Bundle 'Zenburn'

" other
" Bundle 'git://git.wincent.com/command-t.git'

filetype plugin indent on
set ttyfast

set nowrap
set ls=2
set backspace=indent,eol,start
set nostartofline
set showcmd
set autoindent
"set number
set softtabstop=2
set shiftwidth=2
set expandtab
set hidden

set wildmenu
set wildmode=list:longest,full

" These are in the nerdcommenter documentation, but don't work
let NERD_haskell_alt_style=1
let NERD_c_alt_style=1
let g:NERDCustomDelimiters = { 'haskell': { 'leftAlt': '{-', 'rightAlt': '-}', 'left': '-- ', 'right': '' } }

set incsearch
set smartcase
set mouse=a

set background=dark

if has("gui_running")
  colorscheme wombat
  set gfn=Source\ Code\ Pro\ 10
  set guioptions-=T
  set guioptions-=r
  set guioptions-=L
else
  set t_Co=256
  colorscheme solarized
endif
