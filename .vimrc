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
Bundle 'bitc/lushtags'
Bundle 'guns/vim-clojure-static'
Bundle 'jasonkuhrt/Tomorrow-Theme'
Bundle 'kien/ctrlp.vim'
Bundle 'kien/rainbow_parentheses.vim'
Bundle 'laurilehmijoki/haskellmode-vim.git'
Bundle 'majutsushi/tagbar'
Bundle 'tpope/vim-foreplay'
" Bundle 'tpope/vim-surround'
" Bundle 'tpope/vim-fugitive'

" vim-scripts
Bundle 'Align'
Bundle 'darktango.vim'
Bundle 'inkpot'
Bundle 'tComment'
Bundle 'Wombat'
Bundle 'Zenburn'

" other
" Bundle 'git://git.wincent.com/command-t.git'

filetype plugin indent on

"-------------------------------------------------------------------------------
" Simple tweaks
set ttyfast
" set nocompatible   " vundle requires this above
set shortmess+=I

"-------------------------------------------------------------------------------
" Tabs & Indent
set expandtab
set softtabstop=2
set shiftwidth=2
set autoindent

"-------------------------------------------------------------------------------
" Programming tool
set nowrap
set ls=2
set backspace=indent,eol,start
set nostartofline
set showcmd
set number
set hidden
set autochdir
set colorcolumn=80
set mouse=a

"-------------------------------------------------------------------------------
" Searching
set incsearch
set smartcase

"-------------------------------------------------------------------------------
" Wildmenu and Ctrl-P
set wildmenu
set wildmode=list:longest,full
set wildignore+=.*,*/tmp/*,*.so,*.swp,*.zip,*/Library/*,*/Music/*


"-------------------------------------------------------------------------------
" Tagbar 
nmap <c-s> :TagbarToggle<CR>

"-------------------------------------------------------------------------------
" Ctrl-P
nnoremap <leader>p :CtrlP ~<CR>

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
  set gfn=Source\ Code\ Pro:h14
  set guioptions-=T
  set guioptions-=r
  set guioptions-=L
else
  set t_Co=256
endif

"-------------------------------------------------------------------------------
" Haskell
let g:haddock_browser = "open"
let g:haddock_browser_callformat = "%s %s"
au Bufenter *.hs compiler ghc

"-------------------------------------------------------------------------------
" Refresh all buffers from disk.  This is useful when using version control.
fun! PullAndRefresh()
  set noconfirm
  bufdo e!
  set confirm
endfun

