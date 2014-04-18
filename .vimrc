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
Bundle 'idris-hackers/idris-vim'
Bundle 'kien/ctrlp.vim'
Bundle 'kien/rainbow_parentheses.vim'
Bundle 'lambdatoast/elm.vim'
" Bundle 'LaTeX-Box-Team/LaTeX-Box'
Bundle 'tomasr/molokai'
Bundle 'tomtom/tcomment_vim'
" Bundle 'tpope/vim-markdown'
" Bundle 'tpope/vim-surround'
" Bundle 'tpope/vim-fugitive'
Bundle 'wikitopian/hardmode'
Bundle 'w0ng/vim-hybrid'

" vim-scripts
" Bundle 'Align'
Bundle 'DeleteTrailingWhitespace'
Bundle 'inkpot'
Bundle 'twilight'

" other plugins
Bundle 'git://git.code.sf.net/p/vim-latex/vim-latex'

filetype plugin indent on

"-------------------------------------------------------------------------------
" Simple tweaks
set ttyfast
set shortmess+=I
set noesckeys
set shell=/usr/local/bin/bash

"-------------------------------------------------------------------------------
" Tabs & Indent
set expandtab
set softtabstop=4
set shiftwidth=4
set tabstop=4
set noautoindent
autocmd FileType haskell setlocal shiftwidth=2 softtabstop=2

"-------------------------------------------------------------------------------
" Programming tool
" set nowrap
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
set foldmethod=syntax
set foldlevelstart=99


"-------------------------------------------------------------------------------
" LaTeX Box
" let g:LatexBox_latexmk_async=1
let g:LatexBox_Folding=1

"-------------------------------------------------------------------------------
" Airline
let g:airline_theme='solarized'

"-------------------------------------------------------------------------------
" Searching
set ignorecase
set smartcase
set incsearch

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
" let g:easytags_autorecurse = 1
" set tags=./tags;
" let g:easytags_languages = {
" \   'haskell': {
" \       'cmd': '~/.cabal/bin/hasktags',
" \       'args': [],
" \       'fileoutput_opt': '-f',
" \       'stdout_opt': '-f-',
" \       'recurse_flag': '-R'
" \   }
" \}

"-------------------------------------------------------------------------------
" GUI/Terminal specific tweaks
if has("gui_running")
  set gfn=Source\ Code\ Pro:h14
  set guioptions-=T
  set guioptions-=r
  set guioptions-=L
else
  set t_Co=256
endif

"-------------------------------------------------------------------------------
" Colors
syntax on
let g:solarized_italic=0
let g:solarized_bold=0
set background=dark
colorscheme solarized

"-------------------------------------------------------------------------------
" Keybindings
nnoremap <silent> [b :bprevious<CR>
nnoremap <silent> ]b :bnext<CR>
nnoremap <silent> [a :previous<CR>
nnoremap <silent> ]a :next<CR>

nmap <leader>v :setlocal paste! paste?<cr>

nnoremap <silent> C-P :CtrlPBuffer<CR>
