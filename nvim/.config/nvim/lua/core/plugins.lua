-----------------------------------------------------------
-- Plugin manager configuration file
------------------------------------p
----------------------

-- Plugin manager: lazy.nvim
-- URL: https://github.com/folke/lazy.nvim

-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- Use a protected call so we don't error out on first use
local status_ok, lazy = pcall(require, 'lazy')
if not status_ok then
  return
end

-- Start setup
lazy.setup({
  spec = {

    -- Colorscheme:
    -- The colorscheme should be available when starting Neovim.
    -- { 'folke/tokyonight.nvim', lazy = false, priority = 1000, },
    { 'NTBBloodbath/doom-one.nvim', lazy = false, priority = 1000, },
    -- { 'folke/tokyonight.nvim', lazy = false, priority = 1000, },
    -- { 'navarasu/onedark.nvim', lazy = false, priority = 1000, },

    -- Icons
    { 'kyazdani42/nvim-web-devicons', lazy = true },

    -- Dashboard (start screen)
    {
      'goolord/alpha-nvim',
      dependencies = { 'kyazdani42/nvim-web-devicons' },
    },

    {
      'akinsho/bufferline.nvim',
      version = "v3.*",
      dependencies = 'kyazdani42/nvim-web-devicons',
      config = function()
        require("bufferline").setup{
          options = {
            numbers = "ordinal"
          }
        }
      end

    },

    -- Git labels
    {
      'lewis6991/gitsigns.nvim',
      lazy = true,
      dependencies = {
        'nvim-lua/plenary.nvim',
        'kyazdani42/nvim-web-devicons',
      },
    },

    -- File explorer
    {
      'kyazdani42/nvim-tree.lua',
      dependencies = { 'kyazdani42/nvim-web-devicons' },
    },

    -- Statusline
    {
      -- 'freddiehaddad/feline.nvim',
      'nvim-lualine/lualine.nvim',
      dependencies = {
        'kyazdani42/nvim-web-devicons',
        'lewis6991/gitsigns.nvim',
      },
    },

    -- Treesitter
    { 'nvim-treesitter/nvim-treesitter', build = ':TSUpdate' },

    -- Indent line
    { 'lukas-reineke/indent-blankline.nvim' },

    -- Tag viewer
    { 'preservim/tagbar' },

    -- Autopair
    {
      'windwp/nvim-autopairs',
      event = 'InsertEnter',
      config = function()
        require('nvim-autopairs').setup({})
      end
    },

    -- Telescope
    {
      'nvim-telescope/telescope.nvim', tag = '0.1.1',
      dependencies = {
          'nvim-lua/plenary.nvim',
          "debugloop/telescope-undo.nvim",
      },
      config = function()
          require("telescope").setup({
              extensions = {
                  undo = {
                  },
              },
          })
          require("telescope").load_extension("undo")
      end
    },

    -- LSP
    {
      'neovim/nvim-lspconfig',
      config = function()
        require('lspconfig').pylsp.setup{
          settings = {
            pylsp = {
              plugins = {
                pycodestyle = { enabled = false },
                yapf = { enabled = false },
                flake8 = { enabled = true },
                black = { enabled = true, preview = true },
              },
            }
          }
        }
      end
    },

    -- Autocomplete
    {
      'hrsh7th/nvim-cmp',
      -- load cmp on InsertEnter
      event = 'InsertEnter',
      -- these dependencies will only be loaded when cmp loads
      -- dependencies are always lazy-loaded unless specified otherwise
      dependencies = {
        'L3MON4D3/LuaSnip',
        'hrsh7th/cmp-nvim-lsp',
        'hrsh7th/cmp-path',
        'hrsh7th/cmp-buffer',
        'saadparwaiz1/cmp_luasnip',
        'honza/vim-snippets',
      },
    },

    -- Switch between window splits easily
    {
        'yorickpeterse/nvim-window',
        config = function()
            require('nvim-window').setup({
                -- The characters available for hinting windows.
                chars = {
                    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
                    'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
                    'u', 'v', 'w', 'x', 'y', 'z'
                },
                -- normal_hl = 'Substitute',
                normal_hl = 'PmenuSel',
                hint_hl = 'Bold',
                -- The border style to use for the floating window.
                border = 'single',
            })
        end
    },

    -- Comment code
    {
      'numToStr/Comment.nvim',
      config = function()
        require('Comment').setup{
          padding = true,
          sticky = true,
          toggler = {
            -- Line-comment toggle keymap
            line = ',ci',
            -- Block-comment toggle keymap
            block = ',cb',
          },
          -- LHS of operator-pending mappings in NORMAL and VISUAL mode
          opleader = {
            -- Line-comment keymap
            line = ',ci',
            -- Block-comment keymap
            block = ',cb',
          },
        }
      end
    },

    -- Generate docstrings
    {
      'danymat/neogen',
      config = function()
        require('neogen').setup{
          enabled = true,
          languages = {
            python = {
              template = { annotation_convention = "google_docstrings"}
            }
          }
        }
      end
    },

    -- VIM Plugins

    { 'tpope/vim-repeat'  },

    -- Switch between buffers easily
    -- {
    --   'ap/vim-buftabline',
    --   config = function()
    --     vim.g.buftabline_numbers=2
    --     vim.g.buftabline_indicators=1
    --   end
    -- },

    -- Shows marks in the number column
    { 'kshenoy/vim-signature'  },

    -- Windows resize
    { 'simeji/winresizer', },

    -- enable and disable hls automatically
    { 'romainl/vim-cool'},

    -- zoom window
    {'dhruvasagar/vim-zoom'},

  },
})
