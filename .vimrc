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
Bundle 'scrooloose/nerdcommenter'
Bundle 'altercation/vim-colors-solarized'

" vim-scripts
Bundle 'Align'

" other
Bundle 'git://git.wincent.com/command-t.git'

filetype plugin indent on
set ttyfast
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

let NERD_haskell_alt_style=1
let NERD_c_alt_style=1

nnoremap <leader>f :CommandT<CR>

set incsearch
set smartcase
set mouse=a

set t_Co=256
set background=light
" colorscheme solarized

if has("gui_running")
    " set noanti gfn=Terminus_\(TTF\):h18
    set gfn=Consolas:h16
    set guioptions-=T
    set guioptions-=r
    set guioptions-=L
endif

