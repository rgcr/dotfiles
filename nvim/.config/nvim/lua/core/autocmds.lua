-----------------------------------------------------------
-- Autocommand functions
-----------------------------------------------------------

-- Define autocommands/user commands with Lua APIs

local augroup = vim.api.nvim_create_augroup   -- Create/get autocommand group
local autocmd = vim.api.nvim_create_autocmd   -- Create autocommand
local usercmd = vim.api.nvim_create_user_command -- Create user command

-- General settings:
--------------------
-- go to last loc when opening a buffer
augroup("ResumeCursorPosition", {clear = true})
autocmd(
    "BufReadPost",
    {
      group = "ResumeCursorPosition",
      command = [[if line("'\"") > 0 && line("'\"") <= line("$") | execute "normal! g`\"" | endif]]
    }
)

-- unset paste on InsertLeave
autocmd(
  "InsertLeave",
  { pattern = "*", command = "silent! set nopaste"}
)

-- Highlight on yank
augroup('YankHighlight', { clear = true })
autocmd('TextYankPost', {
  group = 'YankHighlight',
  callback = function()
    vim.highlight.on_yank({ higroup = 'IncSearch', timeout = '1000' })
  end
})

-- Remove trailing whitespace on save
autocmd('BufWritePre', {
  pattern = '',
  command = ":%s/\\s\\+$//e"
})

-- Don't auto commenting new lines
autocmd('BufEnter', {
  pattern = '',
  command = 'set fo-=c fo-=r fo-=o'
})

-- show cursor line only in active window
augroup("CursorLine", { clear = true })
autocmd(
  { "InsertLeave", "WinEnter" },
  { pattern = "*", command = "set cursorline", group = "CursorLine"}
)
autocmd(
  { "InsertEnter", "WinLeave" },
  { pattern = "*", command = "set nocursorline", group = "CursorLine"}
)


-- Settings for filetypes:
--------------------------

-- Disable line length marker
augroup('setLineLength', { clear = true })
autocmd('Filetype', {
  group = 'setLineLength',
  pattern = { 'text', 'markdown', 'html', 'xhtml', 'javascript', 'typescript' },
  command = 'set cc=0'
})

-- Set indentation to 2 spaces
augroup('setIndent', { clear = true })
autocmd('Filetype', {
    group = 'setIndent',
    pattern = { 'xml', 'html', 'xhtml', 'css', 'scss', 'yaml', 'lua'},
  command = 'set shiftwidth=2 tabstop=2 sts=2 expandtab'
})
-- Set indentation to 4 spaces
-- autocmd('Filetype', {
--   group = 'setIndent',
--   pattern = { 'javascript', 'typescript', 'vue', 'json'},
--   command = 'set shiftwidth=4 tabstop=4 sts=4 expandtab'
-- })

-- Enable spell checking for text filetypes
augroup('setSpell', { clear = true })
autocmd('Filetype', {
  group = 'setSpell',
  pattern = { 'text', 'markdown', 'gitcommit', 'help' },
  command = 'setlocal spell spelllang=en_us'
})


-- Terminal Autocommands:
autocmd('BufLeave', {
  pattern = 'term://*',
  command = 'stopinsert'
})
autocmd('TermOpen', {
  pattern = 'term://*',
  command = 'startinsert'
})
autocmd("TermClose", {
  pattern = "term://*",
  command = "bdelete!"
})

-- Toggle relative line numbers
augroup("NumberToggle", {clear = true})
autocmd(
  {"BufEnter", "FocusGained", "InsertLeave", "WinEnter"},
  {
    pattern = "*",
    group = "NumberToggle",
    command = [[if &nu && mode() != 'i'| set rnu | endif]],
  }
)
autocmd(
  {"BufLeave", "FocusLost", "InsertEnter", "WinLeave"},
  {
    pattern = "*",
    group = "NumberToggle",
    command = [[if &nu | set nornu | endif]],
  }
)

-- Close help and quickfix windows with 'q'
augroup("QuickCloseHelp", {clear = true})
autocmd("FileType", {
  pattern = { "help", "qf", "man", "lspinfo" },
  group = "QuickCloseHelp",
  callback = function()
    vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = true, silent = true })
  end,
})

-- Replace word under cursor globally, requires confirmation
usercmd("ReplaceWord", function()
  local word = vim.fn.expand("<cword>")
  local replacement = vim.fn.input("Replace \"" .. word .. "\" with: ")
  if replacement ~= "" then
    -- vim.cmd(string.format("%%s/\\<%s\\>/%s/g", vim.fn.escape(word, "/\\"), vim.fn.escape(replacement, "/\\")))
    vim.cmd(string.format("%%s/\\<%s\\>/%s/gc", vim.fn.escape(word, "/\\"), vim.fn.escape(replacement, "/\\")))
  end
end, { desc = "Replace word under cursor globally" })


-- Ruff commands for Python filetypes
augroup("RuffCmds", { clear = true })
autocmd("FileType", {
  group = "RuffCmds",
  pattern = "python",
  callback = function()
    usercmd(
      'RuffFormat',
      function()
        vim.cmd("silent! !ruff format %")
        vim.cmd("edit!") -- reload the file to see changes
      end,
      { desc = "Run ruff format on current file" }
    )
  end
})

-- isort command for Python filetypes
augroup("IsortCommand", { clear = true })
autocmd("FileType", {
  group = "IsortCommand",
  pattern = "python",
  callback = function()
    usercmd(
      'Isort',
      function()
        vim.cmd("silent! !isort %")
        vim.cmd("edit!") -- reload the file to see changes
      end,
      { desc = "Run isort on current file" }
    )
  end
})

-- json commands for JSON filetypes
augroup("JsonCommands", { clear = true })
autocmd("FileType", {
  group = "JsonCommands",
  pattern = "json",
  callback = function()
    usercmd(
      'JsonFormat',
      function()
        -- use python -m json.tool for formatting
        vim.cmd("%!python3 -m json.tool")
      end,
      { desc = "Format JSON file with python3" }
    )
  end
})

-- Define a user command to reload the configuration
usercmd(
  'ReloadConfig',
  function()
    vim.cmd("source $MYVIMRC")
    vim.cmd("echo 'Configuration reloaded'")
  end,
  { desc = "Reload Neovim configuration" }
)
-- Define a user command to open the configuration file
usercmd(
  'EditConfig',
  function()
    vim.cmd("edit $MYVIMRC")
  end,
  { desc = "Open Neovim configuration file" }
)

-- Define a user command to open the plugins file
usercmd(
  'EditPlugins',
  function()
    local plugins_file = vim.fn.expand("~/.config/nvim/lua/core/plugins.lua")
    vim.cmd("edit " .. plugins_file)
  end,
  { desc = "Open Neovim plugins file" }
)
-- Define a user command to open the keymaps file
usercmd(
  'EditKeymaps',
  function()
    local keymaps_file = vim.fn.expand("~/.config/nvim/lua/core/keymaps.lua")
    vim.cmd("edit " .. keymaps_file)
  end,
  { desc = "Open Neovim keymaps file" }
)
-- Define a user command to open the autocmds file
usercmd(
  'EditAutocmds',
  function()
    local autocmds_file = vim.fn.expand("~/.config/nvim/lua/core/autocmds.lua")
    vim.cmd("edit " .. autocmds_file)
  end,
  { desc = "Open Neovim autocommands file" }
)
-- Define a user command to open the options file
usercmd(
  'EditOptions',
  function()
    local options_file = vim.fn.expand("~/.config/nvim/lua/core/options.lua")
    vim.cmd("edit " .. options_file)
  end,
  { desc = "Open Neovim options file" }
)

-- Rename the current file
usercmd(
  'RenameFile',
  function(opts)
    local new_name = opts.args
    if new_name == "" then
      print("Please provide a new name for the file.")
      return
    end
    local current_file = vim.fn.expand("%:p")
    local new_file = vim.fn.fnamemodify(current_file, ":h") .. "/" .. new_name
    vim.cmd("saveas " .. new_file)
    vim.cmd("bdelete " .. current_file)
  end,
  { nargs = 1, desc = "Rename the current file" }
)

-- Custom CMD LSP formatting
usercmd(
  'LspFormat',
  function()
    if vim.lsp.buf.format then
      vim.lsp.buf.format({ async = true })
    else
      print("No LSP formatting available for this buffer.")
    end
  end,
  { desc = "Format the current buffer using LSP" }
)

-- fold <tab> for norg
augroup("NorgGroup", { clear = true })
autocmd("FileType", {
  group = "NorgGroup",
  pattern = "norg",
  callback = function()
    vim.keymap.set("n", "<tab>", "za", { buffer = true, silent = true, desc = "Toggle fold" })
    vim.keymap.set("n", "<S-tab>", "zR", { buffer = true, silent = true, desc = "Open all folds" })
  end,
})

-- augroup("OilGroup", { clear = true })
-- autocmd("FileType", {
--   group = "OilGroup",
--   pattern = "oil",
--   callback = function()
--     vim.keymap.set("n", "<Esc>", "<cmd>q<cr>", { buffer = true, silent = true, desc = "Quit oil" })
--   end,
-- })
