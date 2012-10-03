"         _                    
"  __   _(_)_ __ ___  _ __ ___ 
"  \ \ / / | '_ ` _ \| '__/ __|
"   \ V /| | | | | | | | | (__ 
"  (_)_/ |_|_| |_| |_|_|  \___|
" 


call pathogen#infect()

" VimClojure specific stuff
filetype off
call pathogen#runtime_append_all_bundles()

let g:vimclojure#HighlightBUiltins = 1
let g:vimclojure#ParenRainbow = 1
let vimclojure#NailgunClient = "/Users/karl/bin/ng"
let vimclojure#WantNailgun = 1

filetype plugin indent on
syntax on

set nocompatible
set ttyfast
" set autoread
set nowrap
set ls=2
set backspace=indent,eol,start
set nostartofline
set showcmd
set autoindent
set number
" set autochdir
" set showtabline=2
set softtabstop=4
set shiftwidth=4
set expandtab

set wildmenu
" set wildmode=list:longest,full

let NERD_haskell_alt_style=1
let NERD_c_alt_style=1

nnoremap <leader>f :CommandT<CR>

set incsearch
set smartcase
set mouse=a

set t_Co=256
set background=light
colorscheme solarized

if has("gui_running")
    " set noanti gfn=Terminus_\(TTF\):h18
    set gfn=Consolas:h16
    set guioptions-=T
    set guioptions-=r
    set guioptions-=L
endif

