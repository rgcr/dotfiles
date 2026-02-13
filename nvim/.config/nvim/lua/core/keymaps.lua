-- Keymaps for Neovim

-----------------------------------------------------------
-- Default leader
-----------------------------------------------------------
vim.g.mapleader = ','

local ok, utils = pcall(require, 'core/utils')
if not ok then
  return
end

-----------------------------------------------------------
-- Extra Functions
-----------------------------------------------------------
function AppendModeline()
  local modeline = string.format(
  "vim: set ft=%s ts=%d sw=%d tw=%d %set :",
  vim.bo.filetype,
  vim.bo.tabstop,
  vim.bo.shiftwidth,
  vim.bo.textwidth,
  vim.bo.expandtab and '' or 'no'
  )
  modeline = { string.format(vim.bo.commentstring, modeline) }
  vim.api.nvim_buf_set_lines(0, -1, -1, true, modeline)
end

-----------------------------------------------------------
-- Neovim shortcuts
-----------------------------------------------------------

-- copy to Clipboard only with yank commands
utils.nnoremap('y', '"+y', { desc = "Yank to System Clipboard" })
utils.vnoremap('y', '"+y', { desc = "Yank to System Clipboard" })
utils.nnoremap('Y', '"+Y', { desc = "Yank Line to System Clipboard" })
utils.vnoremap('Y', '"+Y', { desc = "Yank Line to System Clipboard" })

-- Esc to jk, jj, kk
utils.inoremap('jk', '<Esc>', { desc = "Exit Insert Mode with jk" })
utils.inoremap('jj', '<Esc>', { desc = "Exit Insert Mode with jj" })
utils.inoremap('kk', '<Esc>', { desc = "Exit Insert Mode with kk" })

-- Replace word under cursor
utils.nnoremap('<leader>r', function()
  local word = vim.fn.expand('<cword>')
  local cmd = ':%s/' .. word .. '//g'
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(cmd .. '<Left><Left>', true, false, true), 'n', false)
end, { desc = "Replace word under cursor" })

-- Toggle auto-rndenting for code paste
utils.nnoremap('<leader>p', ':silent! set invpaste paste?<CR>', { desc = "Toggle Paste Mode" })

-- Toggle column number
utils.nnoremap('<leader>n', ':set rnu!<CR> | :set nu!<CR>', { desc = "Toggle Line Numbers" })

-- Change split orientation
-- utils.nnoremap('<leader>-', '<C-w>t<C-w>K', { desc = "Change vertical to horizontal" })
-- utils.nnoremap('<leader>|', '<C-w>t<C-w>H', { desc = "Change horizontal to vertical" })

-- Move around splits using Ctrl + hjkl
utils.nnoremap('<C-h>', '<C-w>h', { desc = "Move to left split" })
utils.nnoremap('<C-j>', '<C-w>j', { desc = "Move to bottom split" })
utils.nnoremap('<C-k>', '<C-w>k', { desc = "Move to top split" })
utils.nnoremap('<C-l>', '<C-w>l', { desc = "Move to right split" })

-- Move winwdows using Ctrl +
-- utils.nnoremap('', '<C-w>H')
-- utils.nnoremap('', '<C-w>L')

-- Reload configuration without restart nvim
utils.nnoremap( '<leader>R', ':ReloadConfig<CR>', { desc = "Reload Configuration" })

--  Save file
utils.nnoremap('<leader>w', ':w<CR>', { desc = "Save File" })

-- Close current buffer (without close the window)
-- utils.nnoremap('<leader>q', ':bp<bar>bd#<CR>', { desc = "Close Current Buffer" }) -- this is not like vim
utils.nnoremap('<leader>q', utils.close_buffer, { desc = "Close Current Buffer" }) -- this is like vim


-- Close all windows and exit from Neovim with <leader> and q
utils.nnoremap('<leader>Q', ':qa!<CR>', { desc = "Quit Neovim" })

-- append modeline
utils.nnoremap('<leader>ml', '<cmd>lua AppendModeline()<CR>', { desc = "Append Modeline" })

-- zoom-in and zoom-out current window
utils.nnoremap('<leader>z', '<Plug>(zoom-toggle)', { desc = "Toggle Zoom" })

-- Move line up and down
utils.nnoremap('<M-DOWN>', ':m .+1<CR>==')
utils.nnoremap('<M-UP>', ':m .-2<CR>==')

-- Move multiple lines up and M-DOWN
utils.vnoremap('<M-UP>', ":m '<-2<CR>gv=gv")
utils.vnoremap('<M-DOWN>', ":m '>+1<CR>gv=gv")

-- split current line
utils.nnoremap('B', 'i<CR><esc>k$' , { desc = "Split Current Line" })

-- show trilling spaces
-- utils.nnoremap('<leader>ts', ':set list!<CR>') -- show trailing spaces

-----------------------------------------------------------
-- Applications and Plugins shortcuts
-----------------------------------------------------------

-- Open a terminal (ctrl-t)
utils.nnoremap('<C-t>', utils.open_terminal, { desc = "Open Terminal" })
utils.nnoremap('<leader>t', utils.open_terminal, { desc = "Open Terminal" })

-- Esc to exit terminal mode (this doesn't close the terminal)
utils.tnoremap('<Esc>', '<C-\\><C-n>', { desc = "Exit Terminal Mode" })

-- Close terminal (ctrl-t or ctrl-x)
utils.tnoremap('<C-t>', utils.close_terminal, { desc = "Close Terminal" })
utils.tnoremap('<C-x>', utils.close_terminal, { desc = "Close Terminal" })

-- bufferline.nvim, move buffer positions
-- utils.nnoremap('<A-<>', ':BufferLineMovePrev<CR>', { desc = "Move Buffer Left" })key
-- utils.nnoremap('<A->>', ':BufferLineMoveNext<CR>', { desc = "Move Buffer Left" })
-- utils.nnoremap('<A-.>', ':BufferLinePick<CR>', { desc = "Pick Buffer" }


-- NvimTree
utils.nnoremap('<space>n', ':Neotree toggle=true<CR>', { desc = "Toggle Neotree" })

-- IBL
utils.nnoremap('<leader>i', ':IBLToggle<CR>', { desc = "Toggle Indent Blankline" })

-- Tagbar
utils.nnoremap('<space>T', ':Vista!!<CR>', { desc = "Toggle Tagbar" })

-- nvim-window
utils.nnoremap('-', ':lua require("nvim-window").pick()<CR>', { desc = "Pick Window" })
-- switch quickly to other window or use 'nvim-window' if more there is more than two windows
utils.nnoremap('<leader><leader>', function()
  local wins = vim.api.nvim_tabpage_list_wins(0)
  if #wins == 2 then
    vim.cmd('wincmd w')
  else
    require("nvim-window").pick()
  end
end, { desc = "Switch Window" })

-- Telescope / ripgrep
utils.nnoremap('<C-P>', '<cmd>Telescope find_files<cr>', { desc = "Find Files" })
utils.nnoremap('<C-F>', '<cmd>Telescope current_buffer_fuzzy_find<cr>', { desc = "Search in Current Buffer" })
utils.nnoremap('<C-S>', '<cmd>Telescope current_buffer_fuzzy_find<cr>', { desc = "Search in Current Buffer" })
utils.nnoremap('\\', '<cmd>Telescope live_grep<cr>', { desc = "Search in Current Directory" })
utils.nnoremap('<SPACE><SPACE>', '<cmd>Telescope commands<cr>', { desc = "Telescope Commands" })

utils.nnoremap('<leader>u', '<cmd>Telescope undo<cr>', { desc = "Show Undo History" })
utils.nnoremap('<leader>g', '<cmd>Telescope git_commits<cr>', { desc = "Show Git Commits" })

utils.nnoremap('<leader>b', '<cmd>Telescope buffers<cr>', { desc = "Show Buffers" })
utils.nnoremap('<leader>o', '<cmd>Telescope oldfiles<cr>', { desc = "Show Old Files" })
utils.nnoremap('<C-O>', '<cmd>Telescope oldfiles<cr>', { desc = "Show Old Files" })

-- bufferline.nvim
utils.nnoremap('<leader>1', '<cmd>BufferLineGoToBuffer 1<CR>', { desc = "Go to Buffer 1" })
utils.nnoremap('<leader>2', '<cmd>BufferLineGoToBuffer 2<CR>', { desc = "Go to Buffer 2" })
utils.nnoremap('<leader>3', '<cmd>BufferLineGoToBuffer 3<CR>', { desc = "Go to Buffer 3" })
utils.nnoremap('<leader>4', '<cmd>BufferLineGoToBuffer 4<CR>', { desc = "Go to Buffer 4" })
utils.nnoremap('<leader>5', '<cmd>BufferLineGoToBuffer 5<CR>', { desc = "Go to Buffer 5" })
utils.nnoremap('<leader>6', '<cmd>BufferLineGoToBuffer 6<CR>', { desc = "Go to Buffer 6" })
utils.nnoremap('<leader>7', '<cmd>BufferLineGoToBuffer 7<CR>', { desc = "Go to Buffer 7" })
utils.nnoremap('<leader>8', '<cmd>BufferLineGoToBuffer 8<CR>', { desc = "Go to Buffer 8" })
utils.nnoremap('<leader>9', '<cmd>BufferLineGoToBuffer 9<CR>', { desc = "Go to Buffer 9" })
utils.nnoremap('<leader>0', '<cmd>BufferLineGoToBuffer 10<CR>', { desc = "Go to Buffer 10" })


-- vim-signature (vim)
utils.nnoremap('<leader>s', ':SignatureToggleSigns<CR>', { desc = "Toggle Signatures" })

-- winresizer (vim)
-- utils.nnoremap('<leader>r', ':WinResizerStartResize<CR>', { desc = "Start Resize Window" })

-- neogen
utils.nnoremap('<leader>cc', ":lua require('neogen').generate()<CR>", { desc = "Generate Documentation" })

-- oil.nvim
utils.nnoremap('<space>o', function()
  require('oil').open_float()
end, { desc = "Open Oil" })
-- utils.nnoremap('<space>o', ':Oil<CR>', { desc = "Open Oil" })


-- goto-preview keeymaps
-- utils.nnoremap('gpd', ':lua require("goto-preview").goto_preview_definition()<CR>') -- preview definition
-- utils.nnoremap('gpt', ':lua require("goto-preview").goto_preview_type_definition()<CR>') -- preview type goto_preview_definition
-- utils.nnoremap('gpi', ':lua require("goto-preview").goto_preview_implementation()<CR>') -- preview goto_preview_implementation
-- utils.nnoremap('gpD', ':lua require("goto-preview").goto_preview_declaration()<CR>') -- preview goto_preview_declaration
-- utils.nnoremap('gQ', ':lua require("goto-preview").close_all_win()<CR>') -- close all preview winwdows
-- utils.nnoremap('gpr', ':lua require("goto-preview").goto_preview_references()<CR>') -- preview goto_preview_references

-- Fold / unfold with tab for norg files
vim.api.nvim_create_autocmd("FileType", {
  pattern = "norg",
  callback = function()
    utils.nnoremap("<Tab>", "za")
  end,
})

--  Copilot keymaps
utils.inoremap("<M-p>", "<Plug>(copilot-previous)")
utils.inoremap("<M-n>", "<Plug>(copilot-next)")
-- utils.inoremap("<C-]>", "<Plug>(copilot-dismiss)")
utils.inoremap("<M-q>", "<Plug>(copilot-dismiss)")
utils.inoremap("<M-l>", "<Plug>(copilot-accept-line)")
utils.exprinoremap("<M-CR>", 'copilot#Accept("\\<CR>")', { replace_keycodes = false })


-----------------------------------------------------------
-- HYDRAS
-----------------------------------------------------------

local ok, Hydra = pcall(require, 'hydra')
if not ok then
  return
end

local cmd = require('hydra.keymap-util').cmd

-- Hydra for resizing windows
local hint_resize = [[
  Resize Window
  -------------

       _k_: up
_h_: left   _l_: right
       _j_: down

_=_ : equalize

  -------------
  _<Esc>_ | _q_: quit
]]
Hydra({
   name = '+Resize Window',
   hint = hint_resize,
   config = {
      color = 'red',
      invoke_on_body = true,
      hint = {
         position = 'bottom',
         float_opts = {
           -- overridden
           style ="minimal",
           focusable = false,
           noautocmd = true,
           border = 'rounded',
         },
       },
     },
   mode = 'n',
   body = '<space>r',
   heads = {
      { 'k', '<C-w>-' },
      { 'j', '<C-w>+' },
      { 'l', '<C-w><' },
      { 'h', '<C-w>>' },
      { '=', '<C-w>='},
      { 'q', nil, { exit = true, nowait = true } },
      { '<Esc>', nil, { exit = true, nowait = true } },
   }
})


-- Hydra for Telescope
local hint_telescope = [[
_f_: files              _m_: marks
_o_: old files          _g_: search in current directory(ripgrep)
_u_: undotree           _/_: search in file


_c_: commands           _;_: commands history

_h_: vim help           _k_: keymaps
_O_: vim options        _?_: search history

_<Enter>_: Telescope   _<Esc>_ | _q_
]]

Hydra({
   name = 'Telescope+',
   hint = hint_telescope,
   config = {
      color = 'teal',
      invoke_on_body = true,
      hint = {
         position = 'bottom',
         float_opts = {
           -- overridden
           style = "minimal",
           focusable = false,
           noautocmd = true,
           border = 'rounded',
         },
       },
     },
   mode = 'n',
   body = '<space>t',
   heads = {
      { 'f', cmd 'Telescope find_files' },
      { 'g', cmd 'Telescope live_grep' },
      { 'o', cmd 'Telescope oldfiles', { desc = 'recently opened files' } },
      { 'h', cmd 'Telescope help_tags', { desc = 'vim help' } },
      { 'm', cmd 'Telescope marks', { desc = 'marks' } },
      { 'k', cmd 'Telescope keymaps' },
      { 'O', cmd 'Telescope vim_options' },
      -- { 'r', cmd 'Telescope resume' },
      -- { 'p', cmd 'Telescope projects', { desc = 'projects' } },
      { '/', cmd 'Telescope current_buffer_fuzzy_find', { desc = 'search in file' } },
      { '?', cmd 'Telescope search_history',  { desc = 'search history' } },
      { ';', cmd 'Telescope command_history', { desc = 'command-line history' } },
      { 'c', cmd 'Telescope commands', { desc = 'execute command' } },
      { 'u', cmd 'Telescope undo', { desc = 'undotree' }},
      { '<Enter>', cmd 'Telescope', { exit = true, desc = 'list all pickers' } },
      { 'q', nil, { exit = true, nowait = true } },
      { '<Esc>', nil, { exit = true, nowait = true } },
   }
})


-- Hydra for Copilot
local hint_copilot = [[
  Copilot
  --------------------
  _e_: Enable
  _d_: Disable
  _s_: Status

  --------------------
  _<Esc>_ | _q_: Quit
]]

Hydra({
   name = 'Copilot+',
   hint = hint_copilot,
   config = {
      color = 'teal',
      invoke_on_body = true,
      hint = {
         position = 'bottom',
         float_opts = {
           -- overridden
           style = "minimal",
           focusable = false,
           noautocmd = true,
           border = 'rounded',
         },
       },
     },
   mode = 'n',
  body = '<space>c',
  heads = {
    { 'e',
      function()
        require('lazy').load({plugins = {'copilot.vim'}})
        vim.g.copilot_enabled = true
        vim.cmd('Copilot enable')
        vim.cmd('Copilot restart')
        vim.notify("Copilot enabled", "info", { title = "Copilot", timeout = 300 })
      end,
      { desc = 'enable' } },
    { 'd', function()
        require('lazy').load({plugins = {'copilot.vim'}})
        vim.cmd('Copilot disable')
        vim.notify("Copilot disabled for current buffer", "info", { title = "Copilot", timeout = 300 })
      end, { desc = 'disable current buffer' } },
    { 's',
      function()
        if vim.g.copilot_enabled then
          vim.notify("Copilot enabled", "info", { title = "Copilot", timeout = 300 })
        else
          vim.notify("Copilot disabled", "info", { title = "Copilot", timeout = 300 })
        end
      end,
      { desc = 'status' }
    },
    -- { 's', function() require('lazy').load({plugins = {'copilot.vim'}}); vim.cmd('Copilot status') end, { desc = 'status' } },
    { 'q', nil, { exit = true, nowait = true } },
    { '<Esc>', nil, { exit = true, nowait = true } },
  }
})

-- -- hydra for lsp
local hint_lsp = [[
                LSP
----------------------------------------
 _d_: Definition     _D_: Declaration
 _h_: Hover          _k_: Signature Help
 _r_: References
 ^
 _c_: Code Action
 ^
 _R_: Rename         _F_: Format
  ^
 _i_: LSP Info
---------------------------------------
  _q_ | _<ESC>_: Quit
^^^^
]]
Hydra({
  name = 'LSP',
  hint = hint_lsp,
  config = {
    color = 'teal',
    invoke_on_body = true,
    on_key = function()
      vim.lsp.buf.format { async = true }
    end,
    hint = {
      position = 'bottom',
      float_opts = {
        -- overridden
        style = "minimal",
        focusable = false,
        noautocmd = true,
        border = 'rounded',
      },
    },
  },
  mode = 'n',
  body = '<space>l',
  heads = {
    { 'd', vim.lsp.buf.definition, { desc = 'LSP Definition' } },
    { 'D', vim.lsp.buf.declaration, { desc = 'LSP Declaration' } },
    { 'h', vim.lsp.buf.hover, { desc = 'LSP Hover' } },
    { 'i', '<cmd>LspInfo<CR>', { desc = 'LSP Info' } },
    { 'k', vim.lsp.buf.signature_help, { desc = 'LSP Signature Help' } },
    { 'R', vim.lsp.buf.rename, { desc = 'LSP Rename' } },
    { 'r', vim.lsp.buf.references, { desc = 'LSP References' } },
    { 'c', vim.lsp.buf.code_action, { desc = 'LSP Code Action' } },
    -- { 'f', vim.lsp.buf. , { desc = 'LSP Fix (Code Action)' } },
    { 'F', function() vim.lsp.buf.format { async = true } end, { desc = 'LSP Format' } },
    { 'q', nil, { exit = true, nowait = true } },
    { '<ESC>', nil, { exit = true, nowait = true } },
  }
})

-- hydra for workspaces.nvim
local hint_workspaces = [[
  Workspaces
  ----------
  _l_: List Workspaces
  _a_: Add Workspace
  _r_: Remove Workspace
  _s_: Switch Workspace

  ----------
  _<Esc>_ | _q_: Quit
]]
Hydra({
   name = 'Workspaces',
   hint = hint_workspaces,
   config = {
      color = 'teal',
      invoke_on_body = true,
      hint = {
         position = 'bottom',
         float_opts = {
           -- overridden
           style = "minimal",
           focusable = false,
           noautocmd = true,
           border = 'rounded',
         },
       },
     },
   mode = 'n',
   body = '<space>w',
   heads = {
      { 'a', cmd 'WorkspacesAdd', { desc = 'add workspace' } },
      { 'l', cmd 'WorkspacesList', { desc = 'list workspaces' } },
      { 'r', cmd 'WorkspacesRemove', { desc = 'remove workspace' } },
      { 's', cmd 'WorkspacesOpen', { desc = 'switch workspace' } },
      { 'q', nil, { exit = true, nowait = true } },
      { '<Esc>', nil, { exit = true, nowait = true } },
   }
})

-- hydra for bufferline.nvim
local hint_bufferline = [[
  Bufferline (Buffer Actions)
  ---------------------------
  _p_: Pick Buffer  _c_: Close Others
  ^
  _<_: Move Left    _>_: Move Right
  ^
  ---------------------------
  _<Esc>_ | _q_: Quit
]]
Hydra({
   name = 'Bufferline',
   hint = hint_bufferline,
   config = {
      color = 'red',
      invoke_on_body = true,
      hint = {
         position = 'bottom',
         float_opts = {
           -- overridden
           style = "minimal",
           focusable = false,
           noautocmd = true,
           border = 'rounded',
         },
       },
     },
   mode = 'n',
   body = '<space>b',
   heads = {
      { 'p', cmd('BufferLinePick'), { desc = "pick buffer",  exit = true, nowait = true } },
      { 'c', cmd('BufferLineCloseOthers'), { desc = "close other buffers", exit = true, nowait = true} },
      { '<', cmd('BufferLineMovePrev'), { desc = "move buffer left" } },
      { '>', cmd('BufferLineMoveNext'), { desc = "move buffer right" } },
      { '<Esc>', nil, { exit = true, nowait = true } },
      { 'q', nil, { exit = true, nowait = true } },
   }
})

-- hydra for diffview
local hint_diffview = [[
  Diffview
  --------------------
  _t_: Toggle Diffview
  _h_: File History
  _r_: Refresh View
  _n_: Next Change
  _p_: Previous Change

  --------------------
  _q_ | _<ESC>_: Quit
]]
Hydra({
   name = 'Diffview',
   hint = hint_diffview,
   config = {
      color = 'teal',
      invoke_on_body = true,
      hint = {
         position = 'bottom',
         float_opts = {
           -- overridden
           style = "minimal",
           focusable = false,
           noautocmd = true,
           border = 'rounded',
         },
       },
     },
   mode = 'n',
   body = '<space>d',
    heads = {
      { 't', cmd 'DiffviewOpen', { desc = 'toggle diffview' } },
      { 'h', cmd 'DiffviewFileHistory', { desc = 'file history' } },
      { 'r', cmd 'DiffviewRefresh', { desc = 'refresh view' } },
      { 'n', cmd 'DiffviewNextChange', { desc = 'next change' } },
      { 'p', cmd 'DiffviewPrevChange', { desc = 'previous change' } },
      { 'q', nil, { exit = true, nowait = true } },
      { '<ESC>', nil, { exit = true, nowait = true } },
   }
})
