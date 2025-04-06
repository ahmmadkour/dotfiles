-- This file can be loaded by calling `lua require('plugins')` from your init.vim

-- Only required if you have packer configured as `opt`
vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  -- telescope -- for fuzzy finding
  use {
	  'nvim-telescope/telescope.nvim', tag = '0.1.8',
	  -- or                            , branch = '0.1.x',
	  requires = { {'nvim-lua/plenary.nvim'} }
  }

  -- colorscheme
  use 'folke/tokyonight.nvim'

  -- treesitter -- is a parser generator tool and an incremental parsing library.
  use('nvim-treesitter/nvim-treesitter', {run = ':TSUpdate'})


  -- harpool -- for file navigation
  use 'nvim-lua/plenary.nvim' -- don't forget to add this one if you don't have it yet!
  use 'ThePrimeagen/harpoon'

  -- undotree
  use 'mbbill/undotree'

  -- fugitive -- is the premier Vim plugin for Git
  use 'tpope/vim-fugitive'

  -- lsp zero
  use({'hrsh7th/nvim-cmp'})
  use({'hrsh7th/cmp-nvim-lsp'})

  -- manson
  use {
	  "williamboman/mason.nvim",
	  "williamboman/mason-lspconfig.nvim",
	  "neovim/nvim-lspconfig",
  }

  use {'kevinhwang91/nvim-ufo', requires = 'kevinhwang91/promise-async'}

  -- Enable :GBrowse
  use( 'tpope/vim-rhubarb' )
end)
