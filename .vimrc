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
Bundle 'bling/vim-airline'
Bundle 'kien/ctrlp.vim'
Bundle 'kien/rainbow_parentheses.vim'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-surround'
" Bundle 'tpope/vim-fugitive'
" Bundle 'xolox/vim-easytags'

" vim-scripts
Bundle 'Align'
Bundle 'inkpot'
Bundle 'tComment'

filetype plugin indent on

"-------------------------------------------------------------------------------
" Simple tweaks
set ttyfast
set shortmess+=I
set noesckeys

"-------------------------------------------------------------------------------
" Tabs & Indent
set expandtab
set softtabstop=4
set shiftwidth=4
set autoindent
autocmd FileType haskell setlocal shiftwidth=2 softtabstop=2

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
set colorcolumn=80

"-------------------------------------------------------------------------------
" Airline
let g:airline_theme='solarized'

"-------------------------------------------------------------------------------
" Searching
set incsearch
set smartcase

"-------------------------------------------------------------------------------
" Wildmenu and Ctrl-P
set wildmenu
set wildmode=list:longest,full
let g:ctrlp_custom_ignore = {
    \ 'dir': '\.git$\|\.svn$\|Library\|Music\|Pictures\|Downloads',
    \ 'file': '\.so$\|\.hi$\|\.o$\|\.swp$\|.zip$'
\ }

"-------------------------------------------------------------------------------
" Tags
set tags=./tags;
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
  set gfn=Inconsolata:h14
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
