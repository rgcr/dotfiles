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

    -- Dashboard (start screen)
    {
      'goolord/alpha-nvim',
      dependencies = { 'kyazdani42/nvim-web-devicons' },
    },

    -- like centaur tabs
    {
      'akinsho/bufferline.nvim',
      -- version = "v3.*",
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

    -- neotree
    {
      'nvim-neo-tree/neo-tree.nvim',
      branch = "v3.x",
      dependencies = {
        'nvim-lua/plenary.nvim',
        'kyazdani42/nvim-web-devicons',
        'MunifTanjim/nui.nvim',
      },
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

    -- simple floating input cmdline
    -- {
    --   'VonHeikemen/fine-cmdline.nvim',
    --   dependencies = {
    --     'MunifTanjim/nui.nvim',
    --   },
    --   config = function()
    --     require("fine-cmdline").setup({
    --       cmdheight = 1,
    --       enable_autocmd = true,
    --       -- cmdheight = 0,
    --       -- enable_autocmd = false,
    --     })
    --   end
    -- },

    -- Treesitter
    {
      'nvim-treesitter/nvim-treesitter',
      build = ':TSUpdate',
      config = function()
        require'nvim-treesitter.configs'.setup {
          highlight = { enable = true },
          indent = { enable = true },
        }
      end
    },

    -- Indent line
    {
      'lukas-reineke/indent-blankline.nvim',
      main = 'ibl',
    },

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
      'nvim-telescope/telescope.nvim',
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

    -- Project management
    {
      "ahmedkhalf/project.nvim",
      opts = {
        manual_mode = false, -- auto-detect root
        detection_methods = { "lsp", "pattern" },
        patterns = { ".project-root", ".git", "Makefile", "package.json", "pyproject.toml", ".project" },
          exclude_dirs = {}, -- include all folders
        silent_chdir = true, -- don't show messages when changing cwd
        datapath = vim.fn.stdpath("data"),
      },
      config = function(_, opts)
        require("project_nvim").setup(opts)
        -- Optional: integrate with Telescope
        require("telescope").load_extension("projects")
      end,
    },


    -- snippets
    {
      "L3MON4D3/LuaSnip",
      -- follow latest release.
      -- version = "v2.*", -- Replace <CurrentMajor> by the latest released major (first number of latest release)
      -- install jsregexp (optional!).
      build = "make install_jsregexp"
    },

    -- autocompletion plugin
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

    -- LSP
    {
      'neovim/nvim-lspconfig'
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

    -- show keymap hints
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
    -- hydras like emacs!!
    {
      'nvimtools/hydra.nvim',
    },


    {
      'rcarriga/nvim-notify',
      config = function()
        local notify = require("notify")
        -- Set default timeout globally
        notify.setup({
          timeout = 500,  -- milliseconds
          -- stages = "fade", -- optional
        })

        -- vim.notify = function(msg, level, opts)
        --   opts = opts or {}
        --   opts.timeout = 300
        --   -- Ensure msg is a string
        --   -- local m = type(msg) == "string" and msg:lower() or ""
        --   -- Suppress known LSP-related deprecation warnings
        --   if m:lower():match("deprecated") then
        --     do return end
        --   end
        --   -- Fallback to notify
        --   return notify(msg, level, opts)
        -- end
      end
    },

    -- surround | example -->  cs'" --> changes single quotes to double quotes
    {
      'kylechui/nvim-surround',
      event = 'VeryLazy',
      config = function()
        require('nvim-surround').setup({
          -- Configuration here, or leave empty to use defaults
        })
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
            FIX = { icon = " ", color = "error" , alt = { "FIXME", "FIXIT", "FIX" , "XXX"} },
            TODO = { icon = "", color = "info" },
            HACK = { icon = " ", color = "warning" },
            WARN = { icon = " ", color = "warning" },
            PERF = { icon = " ", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" }, color = "hint" },
            NOTE = { icon = " ", color = "hint" },
            TEST = { icon = "", color = "#9fbb58", alt = { "TESTING", "TESTING!" } },
          }
        })
      end
    },

    -- Incremental renaming
    {
      'smjonas/inc-rename.nvim',
      opts = {}
    },

    -- Rainbow delimiters for neovim
    { 'HiPhish/rainbow-delimiters.nvim' },


    -- preview definition of functions
    -- {
    --   "rmagatti/goto-preview",
    --   dependencies = { "rmagatti/logger.nvim" },
    --   event = "BufEnter",
    --   config = function()
    --     require("goto-preview").setup({
    --       -- width = 120,
    --       -- height = 20,
    --       default_mappings = false,
    --       debug = false,
    --     })
    --   end
    --
    -- },

    -- orgmode
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

    -- neorg
    {
      'nvim-neorg/neorg',
      lazy = false, -- Disable lazy loading as some `lazy.nvim` distributions set `lazy = true` by default
      version = "*", -- Replace with the latest tag/release version
      dependencies = { 'nvim-lua/plenary.nvim' },
    },

    -- -- -- -- -- -- -- -- --
    -- VIM Plugins
    -- -- -- -- -- -- -- -- --

    { 'tpope/vim-repeat'  },

    -- Tag viewer
    -- { 'preservim/tagbar' },

    -- Shows marks in the number column
    { 'kshenoy/vim-signature'  },

    -- Windows resize
    -- { 'simeji/winresizer', },

    -- enable and disable hls automatically
    { 'romainl/vim-cool'},

    -- zoom window
    {'dhruvasagar/vim-zoom'},

    -- github copilot
    {
      'github/copilot.vim',
      config = function()
        vim.g.copilot_no_tab_map = true
      end
    },

    -- CopilotC-Nvim/CopilotChat.nvim
    -- {
    --   "CopilotC-Nvim/CopilotChat.nvim",
    --   dependencies = {
    --     { "github/copilot.vim" }, -- or zbirenbaum/copilot.lua
    --     { "nvim-lua/plenary.nvim", branch = "master" }, -- for curl, log and async functions
    --   },
    --   build = "make tiktoken", -- Only on MacOS or Linux
    -- },

    -- vista.vim | View and search LSP symbols, tags in Vim/NeoVim.
    {
      'liuchengxu/vista.vim',
      config = function()
        vim.g.vista_echo_cursor = 0
      end
    },

    -- vim-illuminate | Highlight the word under the cursor, and all its references
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

  }, -- end lazy.spec
}) -- end lazy.setup
