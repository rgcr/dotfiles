----------------------------------------------------------
-- Statusline configuration file
-----------------------------------------------------------

local status_ok, lualine = pcall(require, 'lualine')
if not status_ok then
  return
end

-- Set colorscheme (from core/colors.lua/colorscheme_name)
-- local cc = require('core/colors')

local hide_in_width = function()
	return vim.fn.winwidth(0) > 80
end

local diagnostics = {
	"diagnostics",
	sources = { "nvim_diagnostic" },
	sections = { "error", "warn" },
	symbols = { error = " ", warn = " " },
	colored = false,
	update_in_insert = false,
	always_visible = true,
}

local diff = {
	"diff",
	colored = true,
	symbols = { added = " ", modified = " ", removed = " " }, -- changes diff symbols
  cond = hide_in_width
}

local mode = {
	"mode",
	fmt = function(str) return str end,
}

local filetype = {
	"filetype",
	icons_enabled = true,
}

local branch = {
	"branch",
	icons_enabled = true,
	icon = "",
}

local location = {
	"location",
	padding = 1,
}

local filename = {
  "filename",
  file_status = true,
  symbols = {
    modified = " ",
    readonly = " ",
    unnamed = "unnamed",
  },
}

local function pastemode()
  if vim.o.paste then
    return '%#IncSearch#[P]'
  end
  return ''
end

lualine.setup({
  global_status = true,
	options = {
		icons_enabled = true,
		theme = "dracula",
		-- section_separators = { left = "", right = "" },
    section_separators = { left = '', right = '' },
    -- component_separators = { left = '', right = '' },
		component_separators = { left = "|", right = "" },
		disabled_filetypes = { "alpha", "dashboard", "NvimTree", "neo-tree","Outline" },
		always_divide_middle = true,
	},
	sections = {
		lualine_a = { branch, diagnostics },
		lualine_b = { mode },
		lualine_c = { pastemode, filename },
		-- lualine_x = { "encoding", "fileformat", "filetype" },
		lualine_x = { "vim.fn['zoom#statusline']()",diff, filetype },
		lualine_y = { location },
		lualine_z = { },
	},
	inactive_sections = {
		lualine_a = {},
		lualine_b = {},
		lualine_c = { "filename" },
		lualine_x = { "location" },
		lualine_y = {},
		lualine_z = {},
	},
	tabline = {},
	extensions = { },
})

options = {
}
