set title
set background=dark
set mouse=a
set nohlsearch
set clipboard+=unnamedplus
set autoindent
set tabstop=4
set shiftwidth=4
set expandtab
set ignorecase
set smartcase
set formatoptions-=r formatoptions-=o
set incsearch

" " don't continue comments on newline
autocmd FileType * set formatoptions-=cro

noremap j gj
noremap k gk
vnoremap p "_c<C-r><C-o>+<Esc> " paste and replace selected without adding deleted text to clipboard
map Y y$

let mapleader =" "
noremap <Leader>w :write<CR>
noremap <Leader>q :q<CR>
noremap <Leader>r :%s/
noremap <Leader>d vipJ

xnoremap > >gv
xnoremap < <gv

xnoremap <c-a> <c-a>gv
xnoremap <c-x> <c-x>gv

set autoread
au CursorHold * checktime

" Don't let Vim's "Found a swap file" message block input
set shortmess=A
