"         _
"  __   _(_)_ __ ___  _ __ ___
"  \ \ / / | '_ ` _ \| '__/ __|
"   \ V /| | | | | | | | | (__
"  (_)_/ |_|_| |_| |_|_|  \___|

" If you read this, switch to Emacs

" Vundle stuff first
set nocompatible
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'
Bundle 'VimClojure'
Bundle 'scrooloose/nerdcommenter'
filetype plugin indent on

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
