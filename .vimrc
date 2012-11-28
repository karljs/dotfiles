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
Bundle 'bitc/lushtags'
Bundle 'jpo/vim-railscasts-theme'
Bundle 'kien/ctrlp.vim'
Bundle 'majutsushi/tagbar'
" Bundle 'scrooloose/nerdcommenter'
" Bundle 'tpope/vim-surround'
" Bundle 'tpope/vim-fugitive'

" vim-scripts
Bundle 'Align'
Bundle 'darktango.vim'
Bundle 'desert.vim'
Bundle 'inkpot'
Bundle 'jellybeans.vim'
Bundle 'peaksea'
Bundle 'tComment'
Bundle 'Wombat'
Bundle 'Zenburn'

" other
" Bundle 'git://git.wincent.com/command-t.git'

filetype plugin indent on
set ttyfast

set shortmess+=I
syntax on
set nowrap
set ls=2
set backspace=indent,eol,start
set nostartofline
set showcmd
set autoindent
set number
set softtabstop=2
set shiftwidth=2
set expandtab
set hidden

set wildmenu
set wildmode=list:longest,full
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*/Library/*,*/.ghc/*,*/.macports/*,*/.cabal/*,*/Music/*

set incsearch
set smartcase
set mouse=a


nmap <c-s> :TagbarToggle<CR>

let g:solarized_italic=0
let g:solarized_bold=0
set background=light
colorscheme solarized

if has("gui_running")
  set gfn=Source\ Code\ Pro:h14
  set guioptions-=T
  set guioptions-=r
  set guioptions-=L
else
  set t_Co=256
endif



