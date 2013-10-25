" File: .vimrc
" Author: Dallas Kaman
" Created: Wed Oct 23 22:26:09 CDT 2013

"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Set to auto read when file is modified from another source
set autoread

" Set line numbers
set nu

" Make the backspace work correctly
set backspace=indent,eol,start

" show current mode
set showmode

" Enable filetype plugins
filetype plugin on
filetype indent on

" Enable Pathogen to work
execute pathogen#infect()

"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Colors and fonts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""

"Set vim to use 256 colors
set t_Co=256

"Set syntax highlighting
syntax on

"Set the default colorscheme
colorscheme darkocean

"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Indentation and such
"""""""""""""""""""""""""""""""""""""""""""""""""""""""

set autoindent
set smartindent
set shiftwidth=4
set tabstop=4
set softtabstop=4

"""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Key mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""

"---Colon commands
cmap w!! !sudo tee % >/dev/null
