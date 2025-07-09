-----------------------------------------------------------
-- Color schemes configuration file
-----------------------------------------------------------

local opt = vim.opt
local cmd = vim.cmd
local set_hl = vim.api.nvim_set_hl

opt.termguicolors = true

-- doom-one
vim.g.doom_one_cursor_coloring = false
vim.g.doom_one_terminal_colors = true
vim.g.doom_one_enable_treesitter = true
vim.g.doom_one_plugin_whichkey = true
vim.g.doom_one_plugin_indent_blankline = true
vim.g.doom_one_plugin_neorg = true
cmd('colorscheme doom-one')

-- tokyonight
-- cmd('colorscheme tokyonight')

-- custom highlight groups
set_hl(0, "CursorLineNr", { fg = "#61afef", bold = true })
set_hl(0, "HydraRed", { fg = "#ff5555", bold = true })
set_hl(0, "HydraBlue", { fg = "#61afef", bold = true })
set_hl(0, "HydraAmaranth", { fg = "#ff79c6", bold = true })
set_hl(0, "HydraTeal", { fg = "#00cccc", bold = true })
set_hl(0, "HydraPink", { fg = "#ff6ec7", bold = true })
