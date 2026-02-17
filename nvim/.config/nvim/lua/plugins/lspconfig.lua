-----------------------------------------------------------
-- Neovim LSP configuration file
-----------------------------------------------------------

-- Using native vim.lsp.config API (Neovim 0.11+)
-- Server definitions provided by nvim-lspconfig plugin

local cmp_status_ok, cmp_nvim_lsp = pcall(require, 'cmp_nvim_lsp')
if not cmp_status_ok then
  return
end

-- Add additional capabilities supported by nvim-cmp
local capabilities = cmp_nvim_lsp.default_capabilities()

-- Use LspAttach autocommand for buffer-local keymaps and settings
-- (replaces the old on_attach callback)
vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('lsp_attach_config', { clear = true }),
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    local bufnr = args.buf

    -- Highlighting references.
    -- See: https://sbulav.github.io/til/til-neovim-highlight-references/
    -- for the highlight trigger time see: `vim.opt.updatetime`
    if client and client.server_capabilities.documentHighlightProvider then
      local highlight_group = vim.api.nvim_create_augroup("lsp_document_highlight", { clear = false })
      vim.api.nvim_clear_autocmds { buffer = bufnr, group = highlight_group }
      vim.api.nvim_create_autocmd("CursorHold", {
        callback = vim.lsp.buf.document_highlight,
        buffer = bufnr,
        group = highlight_group,
        desc = "Document Highlight",
      })
      vim.api.nvim_create_autocmd("CursorMoved", {
        callback = vim.lsp.buf.clear_references,
        buffer = bufnr,
        group = highlight_group,
        desc = "Clear All the References",
      })
    end

    -- Mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    local bufopts = { noremap = true, silent = true, buffer = bufnr }

    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
    vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
    vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, bufopts)
    vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
    vim.keymap.set('n', '<space>wl', function()
      print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, bufopts)
    vim.keymap.set('n', '<leader>f', function() vim.lsp.buf.format { async = true } end, bufopts)
  end,
})

-- Diagnostic settings:
-- see: `:help vim.diagnostic.config`
-- Customizing how diagnostics are displayed
vim.diagnostic.config({
  virtual_text = false,     -- Disable default virtual text (using diagflow instead)
  signs = true,             -- Show signs in gutter
  underline = true,         -- Underline diagnostic text
  update_in_insert = false, -- Disable for performance
  severity_sort = true,     -- Sort by severity
  float = {
    focusable = false,
    style = "minimal",
    border = "rounded",
    source = "always",
    header = "",
    prefix = "",
  },
})

-- Mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
local opts = { noremap = true, silent = true }
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
-- XXX: change this shortcuts later, <space>d collision with diffview
-- vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
-- vim.keymap.set('n', '<space>d', vim.diagnostic.setloclist, opts)

--[[
Language servers setup:

For language servers list see:
https://github.com/neovim/nvim-lspconfig/blob/master/doc/configs.md

Language server installed:

Bash          -> bashls
Python        -> pyright, pylsp
C-C++         -> clangd
CSS           -> cssls
JavaScript/TypeScript -> ts_ls
PHP           -> phpactor
Go            -> gopls
--]]

-- Shared configuration for all servers
vim.lsp.config('*', {
  capabilities = capabilities,
  root_markers = { '.git' },
})

-- Server-specific configurations
vim.lsp.config('bashls', {
  filetypes = { "bash", "sh", "zsh" },
  settings = {
    bashIde = {
      globPattern = "*@(.sh|.zsh|.bash|.inc|.bash|.command)"
    }
  },
})

vim.lsp.config('pyright', {
  filetypes = { "python" },
  settings = {
    python = {
      analysis = {
        typeCheckingMode = "basic",
        autoSearchPaths = true,
        useLibraryCodeForTypes = true,
      },
    }
  },
})

vim.lsp.config('ts_ls', {
  filetypes = { "typescript", "typescriptreact", "typescript.tsx", "javascript", "javascriptreact", "javascript.jsx" },
  settings = {
    typescript = {
      format = {
        indentSize = 4,
        tabSize = 4,
        convertTabsToSpaces = true,
      }
    },
    javascript = {
      format = {
        indentSize = 4,
        tabSize = 4,
        convertTabsToSpaces = true,
      }
    }
  },
})

-- Enable all language servers
vim.lsp.enable({
  'bashls',
  'clangd',
  'cssls',
  'pyright',
  'pylsp',
  'ts_ls',
  'phpactor',
  'gopls',
})
