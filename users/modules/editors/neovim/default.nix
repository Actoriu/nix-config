{ config
, lib
, pkgs
, ...
}:

with lib;

let
  cfg = config.custom.editors.neovim;
in
{
  options.custom.editors.neovim = {
    enable = mkEnableOption "Enable support for neovim.";
  };

  config = mkIf cfg.enable {
    programs = {
      neovim = {
        enable = cfg.enable;
        # package = pkgs.neovim-nightly;
        viAlias = true;
        vimAlias = true;
        vimdiffAlias = true;
        extraConfig = ''
          "" Encoding
          set encoding=utf-8
          set fileencoding=utf-8
          set fileencodings=utf-8,iso-8859-1

          "" Tabs. May be overriten by autocmd rules
          set tabstop=4
          set softtabstop=0
          set shiftwidth=4
          set expandtab
          set autoindent
          set smartindent

          "" Enable filetype detection:
          filetype indent plugin on | syn on

          autocmd Filetype make set noexpandtab
          autocmd Filetype c set expandtab
          autocmd Filetype cpp set expandtab
          autocmd Filetype tex set tabstop=2 shiftwidth=2 textwidth=119

          let g:tex_flavor = 'latex'

          "" Remove trailing whitespaces on save
          autocmd BufWritePre * %s/\s\+$//e

          "" Directories for swp files
          set nobackup
          set noswapfile
          set nowb

          "" Automatically update a file if it is changed externally
          set autoread

          "" Visual settings
          syntax on
          let g:enable_bold_font = 1
          set ruler
          set number
          set cursorline
          set cursorcolumn
          set colorcolumn=121

          "" Enable modelines
          set modeline
          set modelines=5

          "" Airline
          set laststatus=2

          let g:airline_powerline_fonts = 1
          let g:airline_theme='onedark'

          if !exists('g:airline_symbols')
          let g:airline_symbols = {}
          endif

          " unicode symbols
          let g:airline_left_sep = '»'
          let g:airline_left_sep = '▶'
          let g:airline_right_sep = '«'
          let g:airline_right_sep = '◀'
          let g:airline_symbols.linenr = '␊'
          let g:airline_symbols.linenr = '␤'
          let g:airline_symbols.linenr = '¶'
          let g:airline_symbols.branch = '⎇'
          let g:airline_symbols.paste = 'ρ'
          let g:airline_symbols.paste = 'Þ'
          let g:airline_symbols.paste = '∥'
          let g:airline_symbols.whitespace = 'Ξ'

          " airline symbols
          let g:airline_left_sep = ''
          let g:airline_left_alt_sep = ''
          let g:airline_right_sep = ''
          let g:airline_right_alt_sep = ''
          let g:airline_symbols.branch = ''
          let g:airline_symbols.readonly = ''
          let g:airline_symbols.linenr = ''

          "" Mappings
          let mapleader=","

          " turn off search highlight with ,-<space>
          nnoremap <leader><space> :nohlsearch<CR>

          :map <leader>iso :w ++enc=iso-8859-1<C-M>

          "" Buffers

          " Enable the list of buffers
          let g:airline#extensions#tabline#enabled = 1

          " Show just the filename
          let g:airline#extensions#tabline#fnamemod = ':t'

          " This allows buffers to be hidden if you've modified a buffer.
          " This is almost a must if you wish to use buffers in this way.
          set hidden

          " To open a new empty buffer
          " This replaces :tabnew which I used to bind to this mapping
          nmap <leader>w :enew<cr>

          " Move to the next buffer
          nmap <leader>n :bnext<CR>

          " Move to the previous buffer
          nmap <leader>p :bprevious<CR>

          " Close the current buffer and move to the previous one
          " This replicates the idea of closing a tab
          nmap <leader>q :bp <BAR> bd #<CR>

          " Show all open buffers and their status
          nmap <leader>l :ls<CR>

          " Disables formatting in paste mode
          set pastetoggle=<F3>
        '';
        # settings = {
        #   ignorecase = true;
        #   relativenumber = true;
        #   number = true;
        # };
        coc.enable = true;
        plugins = with pkgs.vimPlugins; [
          # coc-nvim
          coc-highlight
          coc-pyright
          {
            plugin = coc-snippets;
            config = ''
              inoremap <silent><expr> <TAB>
                \ pumvisible() ? coc#_select_confirm() :
                \ coc#expandableOrJumpable() ?
                \ "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump','''])\<CR>" :
                \ <SID>check_back_space() ? "\<TAB>" :
                \ coc#refresh()

              function! s:check_back_space() abort
                let col = col('.') - 1
                return !col || getline('.')[col - 1]  =~# '\s'
              endfunction

              let g:coc_snippet_next = '<tab>'
              let g:coc_snippet_prev = '<S-Tab>'
            '';
          }
          vim-nix
          vim-airline
          vim-airline-themes
          vim-hybrid-material
          {
            plugin = onedark-vim;
            config = ''
              set background=dark
              packadd! onedark-vim
              colorscheme onedark
            '';
          }
        ];
      };
    };
  };
}
