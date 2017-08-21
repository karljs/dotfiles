"         _
"  __   _(_)_ __ ___  _ __ ___
"  \ \ / / | '_ ` _ \| '__/ __|
"   \ V /| | | | | | | | | (__
"  (_)_/ |_|_| |_| |_|_|  \___|
"

" let $NVIM_TUI_ENABLE_TRUE_COLOR=1

" Plug package manager
call plug#begin()

Plug 'ctrlpvim/ctrlp.vim'
Plug 'derekelkins/agda-vim'
Plug 'epdtry/neovim-coq'
Plug 'idris-hackers/idris-vim'
Plug 'lervag/vimtex'
Plug 'morhetz/gruvbox'
" Plug 'neovimhaskell/haskell-vim'
Plug 'ntpeters/vim-better-whitespace'
Plug 'parsonsmatt/intero-neovim'
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

call plug#end()

"-------------------------------------------------------------------------------
" Global keys and such
let mapleader = ","
let localmapleader = "\\"
nnoremap <leader>ev :edit $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>


"-------------------------------------------------------------------------------
" Terminal mode stuff
tnoremap <Esc> <C-\><C-n>
tnoremap <C-[> <C-\><C-n>
tnoremap <A-h> <C-\><C-n><C-w>h
tnoremap <A-j> <C-\><C-n><C-w>j
tnoremap <A-k> <C-\><C-n><C-w>k
tnoremap <A-l> <C-\><C-n><C-w>l
nnoremap <A-h> <C-w>h
nnoremap <A-j> <C-w>j
nnoremap <A-k> <C-w>k
nnoremap <A-l> <C-w>l

autocmd BufWinEnter,WinEnter term://* startinsert
autocmd BufLeave term://* stopinsert

" "-------------------------------------------------------------------------------
" " Simple tweaks
filetype plugin indent on
"
" "-------------------------------------------------------------------------------
" " Tabs & Indent
set textwidth=80
set expandtab
set softtabstop=4
set shiftwidth=4
set tabstop=4
set autoindent
autocmd FileType haskell setlocal shiftwidth=2 softtabstop=2
autocmd FileType idris setlocal shiftwidth=2 softtabstop=2
autocmd FileType html setlocal shiftwidth=2 softtabstop=2
autocmd BufWritePre * StripWhitespace
"
" "-------------------------------------------------------------------------------
" " Programming tool
" " set nowrap
" set backspace=indent,eol,start
" set nostartofline
" set hidden
" set autochdir
" set mouse=a
" set autoread
"
" "-------------------------------------------------------------------------------
" " Airline
let g:airline_powerline_fonts = 1
" let g:airline_left_sep=''
" let g:airline_right_sep=''
let g:airline_theme = 'base16'
"
" "-------------------------------------------------------------------------------
" " Haskell
" let g:haddock_browser="open"
"
" "-------------------------------------------------------------------------------
" " Searching
set ignorecase
set smartcase
set incsearch
"
" "-------------------------------------------------------------------------------
" " Wildmenu and Ctrl-P
" " let g:ctrlp_cache_dir = $HOME . '/.cache/ctrlp'
" " if executable('ag')
" "   let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
" " endif
" " set wildmenu
" " set wildmode=list:longest,full
" " let g:ctrlp_custom_ignore = {
" "     \ 'dir': '\.git$\|\.svn$\|Library\|Music\|Pictures\|Downloads',
" "     \ 'file': '\.so$\|\.hi$\|\.o$\|\.swp$\|.zip$'
" " \ }
" "
"
" "-------------------------------------------------------------------------------
" " GUI/Terminal specific tweaks
" if has("gui_running")
"   set gfn=Source\ Code\ Pro:h18
"   set guioptions-=T
"   set guioptions-=r
"   set guioptions-=L
" else
"   set t_Co=16
" endif

" "-------------------------------------------------------------------------------
" " Colors
syntax on
" let g:solarized_italic=0
" "let g:solarized_bold=0
" "let g:solarized_termcolors=16
colorscheme base16-solarized
set background=light

" "-------------------------------------------------------------------------------
" " Aesthetics
" " hi StatusLine ctermbg=10
" hi Visual ctermbg=10
" set number
" set laststatus=2
" set ruler
" set showcmd
" set showmatch
"
"
" "-------------------------------------------------------------------------------
" " Keybindings
" nnoremap <silent> [b :bprevious<CR>
" nnoremap <silent> ]b :bnext<CR>
" nnoremap <silent> [a :previous<CR>
" nnoremap <silent> ]a :next<CR>
" nmap <leader>v :setlocal paste! paste?<cr>
"
" "-------------------------------------------------------------------------------
" " Haskell
