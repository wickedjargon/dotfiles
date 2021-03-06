call plug#begin(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/plugged"'))
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'terryma/vim-expand-region'
call plug#end()

set title
set bg=light
set go=a
set mouse=a
set nohlsearch
set clipboard+=unnamedplus
syntax on
set number

noremap Q q
noremap q :q<CR>
noremap j gj
noremap k gk

let mapleader =" "
noremap <Leader>fq :q!<CR>
noremap <Leader>s :update<CR>
noremap <Leader>w :update<CR>
noremap <Leader>xh ggVG<CR>
map <C-K> <Plug>(expand_region_expand)
map <Leader>k <Plug>(expand_region_shrink)
noremap <Leader><Leader> :
nnoremap <leader>r :%s//g<Left><Left>

" comment out a line with C-/
if !hasmapto('<Plug>Commentary') || maparg('gc','n') ==# ''
  xmap <Leader>c  <Plug>Commentary
  nmap <Leader>c  <Plug>Commentary
  omap <Leader>c  <Plug>Commentary
  nmap <Leader>c  <Plug>CommentaryLine
  if maparg('c','n') ==# '' && !exists('v:operator')
    nmap cgc <Plug>ChangeCommentary
  endif
  nmap gcu <Plug>Commentary<Plug>Commentary
endif
map <C-_> <Leader>cj0
