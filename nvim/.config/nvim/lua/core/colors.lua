-----------------------------------------------------------
-- Color schemes configuration file
-----------------------------------------------------------
local opt = vim.opt
local cmd = vim.cmd

opt.termguicolors = true

local M = {}

-- See: https://github.com/brainfucksec/neovim-lua#appearance

M['theme'] = 'doom-one'

-- Neovim UI color scheme.
-- Add the selected color scheme in the `require` values below.
-- Current available color schemes: onedark, monokai, rose-pine.
local status_ok, color_scheme = pcall(require, M.theme)
if not status_ok then
  return
end

-- Note: The instruction to load the color scheme may vary.
-- See the README of the selected color scheme for the instruction
-- to use.
-- e.g.: require('color_scheme').setup{}, vim.cmd('color_scheme') ...

if M.theme == 'onedark' then
  require(M.theme).setup {
    -- styles: dark, darker, cool, deep, warm, warmer, light
    style = 'darker',
    colors = { fg = '#b2bbcc' },
  }
  require(M.theme).load()
elseif M.theme == 'doom-one' then
  vim.g.doom_one_cursor_coloring = false
  vim.g.doom_one_terminal_colors = true
  vim.g.doom_one_enable_treesitter = true
  cmd[[colorscheme doom-one]]
elseif M.theme == 'tokyonight' then
  cmd[[colorscheme tokyonight]]
else
  require(M.theme).setup{}
end


-- Highlight the line number
vim.cmd[[hi CursorLineNr guifg=cyan]]
-- vim.cmd[[highlight LineNr guibg=black]]


return M

