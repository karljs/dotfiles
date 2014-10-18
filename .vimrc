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

Plugin 'gmarik/vundle'

" github
Plugin 'altercation/vim-colors-solarized'
Plugin 'bling/vim-airline'
Plugin 'chriskempson/base16-vim'
Plugin 'chriskempson/vim-tomorrow-theme'
Plugin 'idris-hackers/idris-vim'
Plugin 'kien/ctrlp.vim'
Plugin 'kien/rainbow_parentheses.vim'
Plugin 'lambdatoast/elm.vim'
Plugin 'LaTeX-Box-Team/LaTeX-Box'
Plugin 'majutsushi/tagbar'
Plugin 'lsdr/monokai'
Plugin 'tomasr/molokai'
Plugin 'tomtom/tcomment_vim'
" Plugin 'tpope/vim-markdown'
" Plugin 'tpope/vim-surround'
" Plugin 'tpope/vim-fugitive'
Plugin 'wting/rust.vim'

" vim-scripts
" Plugin 'Align'
" Plugin 'colorsupport.vim'
Plugin 'DeleteTrailingWhitespace'
Plugin 'inkpot'
Plugin 'twilight'
Plugin 'zenburn'

" other plugins
" Plugin 'git://git.code.sf.net/p/vim-latex/vim-latex'

filetype plugin indent on

"-------------------------------------------------------------------------------
" Global keys and such
let mapleader = ","
let localmapleader = "\\"
nnoremap <leader>ev :edit $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>

"-------------------------------------------------------------------------------
" Simple tweaks
set ttyfast
set shortmess+=I
set noesckeys
set shell=/usr/local/bin/bash
set dir=~/tmp
set backupdir=~/tmp//

"-------------------------------------------------------------------------------
" Tabs & Indent
set expandtab
set softtabstop=4
set shiftwidth=4
set tabstop=4
set autoindent
autocmd FileType haskell setlocal shiftwidth=2 softtabstop=2
autocmd FileType idris setlocal shiftwidth=2 softtabstop=2
autocmd BufWritePre * DeleteTrailingWhitespace

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
" set colorcolumn=80
" set foldmethod=syntax
" set foldlevelstart=99

"-------------------------------------------------------------------------------
" Airline
" let g:airline_powerline_fonts = 1
let g:airline_theme = 'solarized'

"-------------------------------------------------------------------------------
" Searching
set ignorecase
set smartcase
set incsearch

"-------------------------------------------------------------------------------
" Wildmenu and Ctrl-P
let g:ctrlp_cache_dir = $HOME . '/.cache/ctrlp'
if executable('ag')
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
endif
" set wildmenu
" set wildmode=list:longest,full
" let g:ctrlp_custom_ignore = {
"     \ 'dir': '\.git$\|\.svn$\|Library\|Music\|Pictures\|Downloads',
"     \ 'file': '\.so$\|\.hi$\|\.o$\|\.swp$\|.zip$'
" \ }
"


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
  set gfn=Anonymous\ Pro:h18
  set guioptions-=T
  set guioptions-=r
  set guioptions-=L
else
  set t_Co=16
endif

"-------------------------------------------------------------------------------
" Colors
syntax on
let g:solarized_italic=0
let g:solarized_bold=0
" let g:solarized_termcolors=16
colorscheme solarized
set background=dark

"-------------------------------------------------------------------------------
" Keybindings
nnoremap <silent> [b :bprevious<CR>
nnoremap <silent> ]b :bnext<CR>
nnoremap <silent> [a :previous<CR>
nnoremap <silent> ]a :next<CR>

nmap <leader>v :setlocal paste! paste?<cr>

