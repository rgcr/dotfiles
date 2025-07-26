-----------------------------------------------------------
-- General Neovim settings and configuration
-----------------------------------------------------------

local g = vim.g       -- Global variables
local opt = vim.opt   -- Set options (global/buffer/windows-scoped)
local fn = vim.fn

-----------------------------------------------------------
-- General
-----------------------------------------------------------
opt.mouse = 'a'                       -- Enable mouse support
opt.scrolloff=5                       -- Keep some extra lines
opt.encoding="utf8"
opt.clipboard = 'unnamedplus'         -- Copy/paste to system clipboard
opt.swapfile = false                  -- Don't use swapfile
opt.completeopt = 'menuone,noinsert,noselect'  -- Autocomplete options
opt.autoread = true                   -- reload files changed outside vim
opt.wrap = false 											-- Long lines
opt.modeline = true                   -- Modelines
opt.modelines = 5                     -- Number of lines to check modelines
-- opt.backspace=indent,eol,start
opt.wildignore = {
	'*.dll', '*.o', '*.class',
	'*.pyc', '__pycache__', '*.bak', '*.py.class',
	'*.exe', '*.jpg', '*.jpeg', '*.png', '*.gif',
	'*/*.dsym/*', '*.dylib'
}

opt.tags = "./tags;,./.tags;,./.git/tags;"
opt.undodir = fn.expand('~/.config/undodir.nvim')
opt.undofile = true

-----------------------------------------------------------
-- Neovim UI
-----------------------------------------------------------
opt.cursorline = true       -- Cursor line
-- opt.gcr = a:blinkon0          -- Disable cursor blink
opt.number = true           -- Show line number
opt.relativenumber = true   -- Show relative number
opt.showmatch = true        -- Highlight matching parenthesis
opt.hlsearch = true         -- Highlight matches with last search pattern
opt.incsearch = true        -- Highlight match while typing search pattern
-- opt.foldmethod = 'marker'   -- Enable folding (default 'foldmarker')
-- opt.colorcolumn = '80'      -- Line lenght marker at 80 columns
opt.splitright = true       -- Vertical split to the right
opt.splitbelow = true       -- Horizontal split to the bottom
opt.ignorecase = true       -- Ignore case letters when search
opt.smartcase = true        -- Ignore lowercase for the whole pattern
opt.linebreak = true        -- Wrap on word boundary
opt.termguicolors = true    -- Enable 24-bit RGB colors
-- opt.laststatus = 2            -- statusline
opt.laststatus = 3            -- Set global statusline
opt.list = false -- Show whitespace characters

-----------------------------------------------------------
-- Tabs, indent
-----------------------------------------------------------
opt.expandtab = true        -- Use spaces instead of tabs
opt.shiftwidth = 4          -- Shift 4 spaces when tab
opt.tabstop = 4             -- 1 tab == 4 spaces
opt.smartindent = true      -- Autoindent new lines

-----------------------------------------------------------
-- Memory, CPU
-----------------------------------------------------------
opt.hidden = true           -- Enable background buffers
opt.history = 100           -- Remember N lines in history
opt.lazyredraw = false      -- Faster scrolling
opt.synmaxcol = 240         -- Max column for syntax highlight
opt.updatetime = 250        -- ms to wait for trigger an event

-----------------------------------------------------------
-- Startup
-----------------------------------------------------------
-- Disable nvim intro
opt.shortmess:append "sI"

-- -- Disable builtin plugins
local disabled_built_ins = {
   "2html_plugin",
   "getscript",
   "getscriptPlugin",
   "gzip",
   "logipat",
   -- "netrw",
   "netrwPlugin",
   "netrwSettings",
   "netrwFileHandlers",
   "matchit",
   "tar",
   "tarPlugin",
   "rrhelper",
   "spellfile_plugin",
   "vimball",
   "vimballPlugin",
   "zip",
   "zipPlugin",
   "tutor",
   "rplugin",
   "synmenu",
   "optwin",
   "compiler",
   "bugreport",
   "ftplugin",
}


for _, plugin in pairs(disabled_built_ins) do
   g["loaded_" .. plugin] = 1
end
