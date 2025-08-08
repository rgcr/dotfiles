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

    -- ===================================================================
    -- UI/APPEARANCE/THEME
    -- ===================================================================

    -- Colorscheme: The colorscheme should be available when starting Neovim
    -- { 'folke/tokyonight.nvim', lazy = false, priority = 1000, },
    { 'NTBBloodbath/doom-one.nvim', lazy = false, priority = 1000, },
    -- { 'folke/tokyonight.nvim', lazy = false, priority = 1000, },
    -- { 'navarasu/onedark.nvim', lazy = false, priority = 1000, },

    -- Dashboard (start screen)
    {
      'goolord/alpha-nvim',
      lazy = false,
      dependencies = { 'kyazdani42/nvim-web-devicons' },
    },

    -- Tab/buffer line (like centaur tabs)
    {
      'akinsho/bufferline.nvim',
      event = 'VeryLazy',
      dependencies = 'kyazdani42/nvim-web-devicons',
      config = function()
        require("bufferline").setup{
          options = {
            numbers = "ordinal"
          }
        }
      end
    },

    -- Statusline
    {
      'nvim-lualine/lualine.nvim',
      event = 'VeryLazy',
      dependencies = {
        'kyazdani42/nvim-web-devicons',
        'lewis6991/gitsigns.nvim',
      },
    },

    -- Indent guides
    {
      'lukas-reineke/indent-blankline.nvim',
      event = { 'BufReadPost', 'BufNewFile' },
      main = 'ibl',
    },

    -- Rainbow delimiters
    {
      'HiPhish/rainbow-delimiters.nvim',
      event = { 'BufReadPost', 'BufNewFile' },
    },

    -- Notifications
    {
      'rcarriga/nvim-notify',
      event = 'VeryLazy',
      config = function()
        local notify = require("notify")
        -- Set default timeout globally
        notify.setup({
          timeout = 500,  -- milliseconds
          -- stages = "fade", -- optional
        })
      end
    },

    -- ===================================================================
    -- FILE MANAGEMENT/NAVIGATION
    -- ===================================================================

    -- File tree
    {
      'nvim-neo-tree/neo-tree.nvim',
      branch = "v3.x",
      cmd = { 'Neotree' },
      dependencies = {
        'nvim-lua/plenary.nvim',
        'kyazdani42/nvim-web-devicons',
        'MunifTanjim/nui.nvim',
      },
      config = function()
        require('neo-tree').setup({
          close_if_last_window = true,
          sort_case_insensitive = true,
          sort_function = nil,
          enable_git_status = true,
          enable_diagnostics = false,
          open_on_tab = false,
          update_cwd = true,
          hijack_directories = {
            auto_open = true
          },
          view = {
            width = 30,
            side = 'left',
            auto_resize = true
          },
        })
      end
    },

    -- File explorer
    {
      'stevearc/oil.nvim',
      cmd = { 'Oil' },
      dependencies = {
        'nvim-tree/nvim-web-devicons',
      },
      opts = {
        default_file_explorer = true,
        view_options = {
          show_hidden = true,
        },
      },
      config = function(_, opts)
        require('oil').setup(opts)
      end,
    },

    -- Fuzzy finder
    {
      'nvim-telescope/telescope.nvim',
      cmd = { 'Telescope' },
      dependencies = {
        'nvim-lua/plenary.nvim',
        "debugloop/telescope-undo.nvim",
      },
      config = function()
        require("telescope").load_extension("undo")
        require("telescope").setup({
          extensions = {
            undo = {},
          },
          pickers = {
            find_files = {
              -- find_command = { "fd", "--type", "f", "--strip-cwd-prefix", "-L" }
              find_command = { "rg", "--files", "--hidden","--ignore-vcs", "-L" }
            },
          }
        })
      end
    },

    -- Window switching
    {
      'yorickpeterse/nvim-window',
      event = 'VeryLazy',
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

    -- ===================================================================
    -- GIT INTEGRATION
    -- ===================================================================

    -- Git signs in gutter
    {
      'lewis6991/gitsigns.nvim',
      event = { 'BufReadPre', 'BufNewFile' },
      dependencies = {
        'nvim-lua/plenary.nvim',
        'kyazdani42/nvim-web-devicons',
      },
      -- config = function()
      --   require('gitsigns').setup({
      --     signs = {
      --       add          = { text = '+' },
      --       change       = { text = '~' },
      --       delete       = { text = '_' },
      --       topdelete    = { text = '‾' },
      --       changedelete = { text = '~' },
      --     },
      --   })
      -- end,
    },

    -- Git commands
    {
      'tpope/vim-fugitive',
      cmd = { 'Git', 'Gdiffsplit', 'Gread', 'Gwrite', 'Ggrep', 'GMove', 'GDelete', 'GBrowse' },
    },

    -- ===================================================================
    -- LSP/LANGUAGE SUPPORT
    -- ===================================================================

    -- LSP configuration
    {
      'neovim/nvim-lspconfig',
      event = { 'BufReadPre', 'BufNewFile' },
    },

    -- Better diagnostic display
    {
      'dgagn/diagflow.nvim',
      event = { 'BufReadPost', 'BufNewFile' },
      opts = {
        enable = true,
        max_width = 60,  -- The maximum width of the diagnostic messages
        severity_colors = {
          error = "DiagnosticFloatingError",
          warning = "DiagnosticFloatingWarn",
          info = "DiagnosticFloatingInfo",
          hint = "DiagnosticFloatingHint",
        },
        gap_size = 1,
        scope = 'cursor', -- 'cursor', 'line' - show only for cursor position
        padding_top = 0,
        padding_right = 0,
        text_align = 'right', -- 'left', 'right'
        placement = 'top', -- 'top', 'inline'
      }
    },

    -- Syntax highlighting
    {
      'nvim-treesitter/nvim-treesitter',
      event = { 'BufReadPost', 'BufNewFile' },
      build = ':TSUpdate',
      config = function()
        require'nvim-treesitter.configs'.setup {
          ensure_installed = {
            'lua', 'vim', 'vimdoc', -- Essential for nvim config
          },
          auto_install = true, -- Install parsers when opening files
          sync_install = false,
          highlight = { enable = true },
          indent = { enable = true },
        }
      end
    },

    -- ===================================================================
    -- CODE COMPLETION/SNIPPETS
    -- ===================================================================

    -- Snippet engine
    {
      "L3MON4D3/LuaSnip",
      event = 'InsertEnter',
      build = "make install_jsregexp"
    },

    -- Autocompletion plugin
    {
      'hrsh7th/nvim-cmp',
      -- load cmp on InsertEnter
      event = 'InsertEnter',
      -- these dependencies will only be loaded when cmp loads
      -- dependencies are always lazy-loaded unless specified otherwise
      dependencies = {
        'hrsh7th/cmp-nvim-lsp',
        'hrsh7th/cmp-path',
        'hrsh7th/cmp-buffer',
        'saadparwaiz1/cmp_luasnip',
        'honza/vim-snippets',
      },
    },

    -- ===================================================================
    -- AI
    -- ===================================================================

    -- GitHub Copilot
    {
      'github/copilot.vim',
      event = 'InsertEnter',
      cmd = { 'Copilot' },
      config = function()
        vim.g.copilot_no_tab_map = true
      end
    },

    -- ===================================================================
    -- CODE EDITING/MANIPULATION
    -- ===================================================================

    -- Auto pairs
    {
      'windwp/nvim-autopairs',
      event = 'InsertEnter',
      config = function()
        require('nvim-autopairs').setup({})
      end
    },

    -- Code commenting
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

    -- Surround text objects
    {
      'kylechui/nvim-surround',
      event = 'VeryLazy',
      config = function()
        require('nvim-surround').setup({
          -- Configuration here, or leave empty to use defaults
        })
      end
    },

    -- Incremental renaming
    {
      'smjonas/inc-rename.nvim',
      cmd = "IncRename",
      opts = {}
    },

    -- Highlight word under cursor
    {
      'RRethy/vim-illuminate',
      config = function()
        require('illuminate').configure({
          delay = 100,
          filetypes_denylist = {
            'alpha',
            'dashboard',
            'NvimTree',
            'Outline',
            'help',
            'packer',
            'lspinfo',
            'TelescopePrompt',
            'TelescopeResults',
          },
          providers = {
            'lsp',
            'treesitter',
            'regex',
          },
        })
      end
    },

    -- ===================================================================
    -- DOCUMENTATION/COMMENTS
    -- ===================================================================

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

    -- Better TODO comments
    {
      'folke/todo-comments.nvim',
      dependencies = { 'nvim-lua/plenary.nvim' },
      config = function()
        require('todo-comments').setup({
          signs = true,
          sign_priority = 8,
          keywords = {
            FIX = { icon = " ", color = "error" , alt = { "FIXME", "FIXIT", "FIX" , "XXX"} },
            TODO = { icon = "", color = "info" },
            HACK = { icon = " ", color = "warning" },
            WARN = { icon = " ", color = "warning" },
            PERF = { icon = " ", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" }, color = "hint" },
            NOTE = { icon = " ", color = "hint" },
            TEST = { icon = "", color = "#9fbb58", alt = { "TESTING", "TESTING!" } },
          }
        })
      end
    },

    -- ===================================================================
    -- NOTE TAKING/ORGANIZATION
    -- ===================================================================

    -- Org-mode
    {
      'nvim-orgmode/orgmode',
      event = 'VeryLazy',
      ft = { 'org' },
      config = function()
        -- Setup orgmode
        require('orgmode').setup({
          -- org_agenda_files = '~/orgfiles/**/*',
          -- org_default_notes_file = '~/orgfiles/refile.org',
        })
      end,
    },

    -- Neorg
    {
      'nvim-neorg/neorg',
      ft = { 'norg' },
      version = "*",
      dependencies = { 'nvim-lua/plenary.nvim' },
    },

    -- ===================================================================
    -- CODE NAVIGATION/SYMBOLS
    -- ===================================================================

    -- Symbol viewer
    {
      'liuchengxu/vista.vim',
      cmd = { 'Vista' },
      config = function()
        vim.g.vista_echo_cursor = 0
      end
    },

    -- ===================================================================
    -- PROJECT MANAGEMENT
    -- ===================================================================
    -- add workspaces.nvim
    {
      'natecraddock/workspaces.nvim',
      event = 'VeryLazy',
      dependencies = {
        'nvim-lua/plenary.nvim',
        'nvim-tree/nvim-web-devicons',
      },
      config = function()
        require('workspaces').setup({
          -- Default workspace directory
          workspace_dir = vim.fn.stdpath('data') .. '/workspaces',
          -- Automatically open the workspace when entering a directory
          auto_open = true,
        })
      end
    },

    -- ===================================================================
    -- UTILITY/HELPERS
    -- ===================================================================

    -- Keymap hints
    {
      'folke/which-key.nvim',
      event = "VeryLazy",
      config = function()
        vim.o.timeout = true
        vim.o.timeoutlen = 400

        require("which-key").setup({
          opts = {
            delay = 900,
            -- preset = "helix",
          },
          win = {
            no_overlap = false, -- don't overlap with other floating windows
          },
          layout = {
            height = { min = 5, max = 25 }, -- min and max height of the columns
            width = { min = 20, max = 50 }, -- min and max width of the columns
            spacing = 3, -- spacing between columns
          },
        })
      end
    },

    -- Hydra keymaps (like Emacs)
    {
      'nvimtools/hydra.nvim',
      event = 'VeryLazy',
    },

    -- Repeat commands
    {
      'tpope/vim-repeat',
      event = 'VeryLazy',
    },

    -- Show marks in number column
    {
      'kshenoy/vim-signature',
      event = { 'BufReadPost', 'BufNewFile' },
    },

    -- Auto disable search highlight
    {
      'romainl/vim-cool',
      event = { 'BufReadPost', 'BufNewFile' },
    },

    -- Zoom windows
    {
      'dhruvasagar/vim-zoom',
      event = 'VeryLazy',
    },

    -- ===================================================================
    -- COPILOT CHAT (COMMENTED OUT)
    -- ===================================================================

    -- CopilotC-Nvim/CopilotChat.nvim
    -- {
    --   "CopilotC-Nvim/CopilotChat.nvim",
    --   dependencies = {
    --     { "github/copilot.vim" }, -- or zbirenbaum/copilot.lua
    --     { "nvim-lua/plenary.nvim", branch = "master" }, -- for curl, log and async functions
    --   },
    --   build = "make tiktoken", -- Only on MacOS or Linux
    -- },

  }, -- end lazy.spec
}) -- end lazy.setup
