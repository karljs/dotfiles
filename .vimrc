"         _
"  __   _(_)_ __ ___  _ __ ___
"  \ \ / / | '_ ` _ \| '__/ __|
"   \ V /| | | | | | | | | (__
"  (_)_/ |_|_| |_| |_|_|  \___|
"


execute pathogen#infect()

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
let g:netrw_liststyle=3

"-------------------------------------------------------------------------------
" Tabs & Indent
set expandtab
set softtabstop=4
set shiftwidth=4
set tabstop=4
set autoindent
autocmd FileType haskell setlocal shiftwidth=2 softtabstop=2
autocmd FileType idris setlocal shiftwidth=2 softtabstop=2
" autocmd BufWritePre * DeleteTrailingWhitespace

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

"-------------------------------------------------------------------------------
" Airline
let g:airline_theme = 'molokai'

"-------------------------------------------------------------------------------
" Searching
set ignorecase
set smartcase
set incsearch

"-------------------------------------------------------------------------------
" Wildmenu and Ctrl-P
" let g:ctrlp_cache_dir = $HOME . '/.cache/ctrlp'
" if executable('ag')
"   let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
" endif
" set wildmenu
" set wildmode=list:longest,full
" let g:ctrlp_custom_ignore = {
"     \ 'dir': '\.git$\|\.svn$\|Library\|Music\|Pictures\|Downloads',
"     \ 'file': '\.so$\|\.hi$\|\.o$\|\.swp$\|.zip$'
" \ }
"

"-------------------------------------------------------------------------------
" GUI/Terminal specific tweaks
if has("gui_running")
  set gfn=Source\ Code\ Pro:h18
  set guioptions-=T
  set guioptions-=r
  set guioptions-=L
else
  set t_Co=16
endif

"-------------------------------------------------------------------------------
" Colors
syntax on
"let g:solarized_italic=0
"let g:solarized_bold=0
"let g:solarized_termcolors=16
colorscheme base16-monokai
set background=dark

"-------------------------------------------------------------------------------
" Keybindings
nnoremap <silent> [b :bprevious<CR>
nnoremap <silent> ]b :bnext<CR>
nnoremap <silent> [a :previous<CR>
nnoremap <silent> ]a :next<CR>

nmap <leader>v :setlocal paste! paste?<cr>

