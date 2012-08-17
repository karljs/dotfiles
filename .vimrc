"         _                    
"  __   _(_)_ __ ___  _ __ ___ 
"  \ \ / / | '_ ` _ \| '__/ __|
"   \ V /| | | | | | | | | (__ 
"  (_)_/ |_|_| |_| |_|_|  \___|
" 


call pathogen#infect()

filetype plugin indent on

set nocompatible
set ttyfast
set autoread
set nowrap
set ls=2
set backspace=indent,eol,start
set nostartofline
set showcmd
set autoindent
set number
set autochdir
set wildmenu
set showtabline=2
set softtabstop=4
set shiftwidth=4
set expandtab

let NERD_haskell_alt_style=1
let NERD_c_alt_style=1

set incsearch
set smartcase

" set t_Co=256
set background=light
colorscheme solarized
syntax enable

if has("gui_running")
    " set noanti gfn=Terminus_\(TTF\):h18
    set gfn=Inconsolata:h18
    set guioptions-=T
    set guioptions-=r
    set guioptions-=L
endif


                             
