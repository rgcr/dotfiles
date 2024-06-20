-----------------------------------------------------------
-- Default leader
-----------------------------------------------------------
vim.g.mapleader = ','


-----------------------------------------------------------
-- MAP Functions
-----------------------------------------------------------
local api = vim.api
-- local keymap_set = vim.keymap.set

local function map(mode, lhs, rhs)
    api.nvim_set_keymap(mode, lhs, rhs, {silent =  true})
end

local function noremap(mode, lhs, rhs)
    api.nvim_set_keymap(mode, lhs, rhs, {noremap = true, silent = true})
end

local function exprnoremap(mode, lhs, rhs)
    api.nvim_set_keymap(mode, lhs, rhs, {noremap = true, silent = true, expr = true})
end

local function nmap(lhs, rhs) map('n', lhs, rhs) end
local function xmap(lhs, rhs) map('x', lhs, rhs) end
local function nnoremap(lhs, rhs) noremap('n', lhs, rhs) end
local function vnoremap(lhs, rhs) noremap('v', lhs, rhs) end
local function xnoremap(lhs, rhs) noremap('x', lhs, rhs) end
local function inoremap(lhs, rhs) noremap('i', lhs, rhs) end
local function tnoremap(lhs, rhs) noremap('t', lhs, rhs) end
local function exprnnoremap(lhs, rhs) exprnoremap('n', lhs, rhs) end
local function exprinoremap(lhs, rhs) exprnoremap('i', lhs, rhs) end


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
  api.nvim_buf_set_lines(0, -1, -1, true, modeline)
end

-----------------------------------------------------------
-- Neovim shortcuts
-----------------------------------------------------------

-- Esc to jk, jj, kk
inoremap('jk', '<Esc>')
inoremap('jj', '<Esc>')
inoremap('kk', '<Esc>')

-- Toggle auto-indenting for code paste
nnoremap('<leader>pp', ':set invpaste paste?<CR>')

-- Toggle column number
-- nnoremap('<leader>n', ':set invnumber<CR>')
nnoremap('<leader>n', ':set rnu!<CR> | :set nu!<CR>')

-- Change split orientation
nnoremap('<leader>-', '<C-w>t<C-w>K') -- change vertical to horizontal
nnoremap('<leader>|', '<C-w>t<C-w>H') -- change horizontal to vertical

-- Move around splits using Ctrl + {h,j,k,l}
nnoremap('<C-h>', '<C-w>h')
nnoremap('<C-j>', '<C-w>j')
nnoremap('<C-k>', '<C-w>k')
nnoremap('<C-l>', '<C-w>l')

-- Reload configuration without restart nvim
nnoremap('<leader>R', ':so %<CR>|:echo "File Reloaded!"<CR>')

-- Fast saving with <leader> and s
nnoremap('<leader>s', ':w<CR>')

-- Close current buffer (without close the window)
nnoremap('<leader>q', ':bp<bar>bd#<CR>')

-- Close all windows and exit from Neovim with <leader> and q
nnoremap('<leader>Q', ':qa!<CR>')

-- split current line
nnoremap('<leader>B', 'i<CR><esc>k$')

-- append modeline
nnoremap('<leader>ml', '<cmd>lua AppendModeline()<CR>')

-- zoom in current buffer
-- nnoremap('<leader>z', ':tabnew %<CR>')
-- zoom out current buffer
-- nnoremap('<leader>zo', ':tabclose<CR>')
nnoremap('<leader>z', '<Plug>(zoom-toggle)')


-----------------------------------------------------------
-- Applications and Plugins shortcuts
-----------------------------------------------------------


-- Terminal mappings
nnoremap('<C-t>', ':Term<CR>')
tnoremap('<Esc>', '<C-\\><C-n>')

-- NvimTree
-- map('n', '<C-n>', ':NvimTreeToggle<CR>')    -- open/close
-- map('n', '<leader>n', ':NvimTreeFindFile<CR>') -- search file
nnoremap('<leader>e', ':NvimTreeToggle<CR>')   -- open/close
nnoremap('<leader>f', ':NvimTreeRefresh<CR>')  -- refresh

-- Tagbar
nnoremap('<leader>t', ':TagbarToggle<CR>')  -- open/close

-- nvim-window
nnoremap('-', ':lua require("nvim-window").pick()<CR>')

-- Telescope / ripgrep
nnoremap('<C-P>', '<cmd>Telescope find_files<cr>') -- find files using ripgrep
nnoremap('<C-F>', '<cmd>Telescope live_grep<cr>')  -- search in current working directory
nnoremap('\\', '<cmd>Telescope grep_string<cr>')   -- search word under the cursor or selection
nnoremap('<leader>g', '<cmd>Telescope git_commits<cr>')
nnoremap('<SPACE><SPACE>', '<cmd>Telescope commands<cr>')
nnoremap('<leader>u', '<cmd>Telescope undo<cr>')
nnoremap('<leader>b', '<cmd>Telescope buffers<cr>')


-- vim-buftabline (vim)
-- nnoremap('<leader>1', '<Plug>BufTabLine.Go(1)')
-- nnoremap('<leader>2', '<Plug>BufTabLine.Go(2)')
-- nnoremap('<leader>3', '<Plug>BufTabLine.Go(3)')
-- nnoremap('<leader>4', '<Plug>BufTabLine.Go(4)')
-- nnoremap('<leader>5', '<Plug>BufTabLine.Go(5)')
-- nnoremap('<leader>6', '<Plug>BufTabLine.Go(6)')
-- nnoremap('<leader>7', '<Plug>BufTabLine.Go(7)')
-- nnoremap('<leader>8', '<Plug>BufTabLine.Go(8)')
-- nnoremap('<leader>9', '<Plug>BufTabLine.Go(9)')
-- nnoremap('<leader>0', '<Plug>BufTabLine.Go(10)')

-- bufferline.nvim
nnoremap('<leader>1', '<cmd>BufferLineGoToBuffer 1<CR>')
nnoremap('<leader>2', '<cmd>BufferLineGoToBuffer 2<CR>')
nnoremap('<leader>3', '<cmd>BufferLineGoToBuffer 3<CR>')
nnoremap('<leader>4', '<cmd>BufferLineGoToBuffer 4<CR>')
nnoremap('<leader>5', '<cmd>BufferLineGoToBuffer 5<CR>')
nnoremap('<leader>6', '<cmd>BufferLineGoToBuffer 6<CR>')
nnoremap('<leader>7', '<cmd>BufferLineGoToBuffer 7<CR>')
nnoremap('<leader>8', '<cmd>BufferLineGoToBuffer 8<CR>')
nnoremap('<leader>9', '<cmd>BufferLineGoToBuffer 9<CR>')
nnoremap('<leader>0', '<cmd>BufferLineGoToBuffer 10<CR>')


-- vim-signature (vim)
nnoremap('<leader>m', ':SignatureToggleSigns<CR>')  -- open/close marks in the number column

-- winresizer (vim)
nnoremap('<leader>r', ':WinResizerStartResize<CR>') -- start resizing window

-- neogen
nnoremap('<leader>cc', ":lua require('neogen').generate()<CR>")

