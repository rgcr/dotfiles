-- Keymap functions
-- https://github.com/miltonllera/neovim-config/blob/master/lua/utils.lua

-- modes ->
-- 'n' = NORMAL,
-- 'i' = INSERT,
-- 'x' = 'VISUAL',
-- 'v' = VISUAL + SELECT,
-- 't' = TERMINAL

local M = {}

function M.close_buffer()
  local bufnr = vim.api.nvim_get_current_buf()
  local listed = vim.fn.getbufinfo({ buflisted=1 })
  if #listed > 1 then
    vim.cmd('bp | bd' .. bufnr)
  else
    vim.cmd('enew | bd' .. bufnr)
  end
end

function M.map(mode, lhs, rhs, opts)
    opts = opts or {}
    vim.keymap.set(mode, lhs, rhs, opts)
end

function M.noremap(mode, lhs, rhs, opts)
    opts = opts or {}
    opts.silent = true
    opts.noremap = true
    M.map(mode, lhs, rhs, opts)
end

function M.exprnoremap(mode, lhs, rhs, opts)
    opts = opts or {}
    opts.silent = true
    opts.noremap = true
    opts.expr = true
    M.map(mode, lhs, rhs, opts)
end

function M.inoremap(lhs, rhs, opts) M.noremap('i', lhs, rhs, opts) end
function M.nnoremap(lhs, rhs, opts) M.noremap('n', lhs, rhs, opts) end
function M.vnoremap(lhs, rhs, opts) M.noremap('v', lhs, rhs, opts) end
function M.xnoremap(lhs, rhs, opts) M.noremap('x', lhs, rhs, opts) end
function M.tnoremap(lhs, rhs, opts) M.noremap('t', lhs, rhs, opts) end
function M.exprnnoremap(lhs, rhs, opts) M.exprnoremap('n', lhs, rhs, opts) end
function M.exprinoremap(lhs, rhs, opts) M.exprnoremap('i', lhs, rhs, opts) end

return M
