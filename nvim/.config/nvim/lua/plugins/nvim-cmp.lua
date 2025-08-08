-----------------------------------------------------------
-- Autocomplete configuration file
-----------------------------------------------------------

-- Plugin: nvim-cmp
-- url: https://github.com/hrsh7th/nvim-cmp


local cmp_status_ok, cmp = pcall(require, 'cmp')
if not cmp_status_ok then
  return
end

local luasnip_status_ok, luasnip = pcall(require, 'luasnip')
if not luasnip_status_ok then
  return
end

cmp.setup {
  -- Load snippet support
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },

-- Completion settings
  completion = {
    --completeopt = 'menu,menuone,noselect'
    keyword_length = 2
  },

  -- Key mapping
  mapping = {
    ['<C-n>'] = cmp.mapping.select_next_item(),
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },

    -- Tab mapping
    ['<Tab>'] = function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end,
    ['<S-Tab>'] = function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end
  },

  -- Load sources, see: https://github.com/topics/nvim-cmp
  sources = {
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
    { name = 'path' },
    { name = 'buffer' },
    { name = 'emoji' },
  },
}
-- Load snippets lazily on first use
vim.api.nvim_create_autocmd("InsertEnter", {
  once = true,
  callback = function()
    require("luasnip.loaders.from_snipmate").load() -- vim-snippets collection
    require("luasnip.loaders.from_snipmate").load({paths = "~/.config/nvim/snippets"}) -- custom snippets
  end,
})

-- Custom command to list available snippets
vim.api.nvim_create_user_command('SnippetsList', function()
  local luasnip = require('luasnip')
  local ft = vim.bo.filetype

  -- Get all snippets for current filetype + 'all' filetype
  local snippets = {}
  local filetypes = { ft, 'all' }

  for _, filetype in ipairs(filetypes) do
    local ft_snippets = luasnip.get_snippets(filetype)
    if ft_snippets then
      for _, snippet in pairs(ft_snippets) do
        table.insert(snippets, {
          trigger = snippet.trigger,
          description = snippet.name or snippet.dscr or "No description",
          filetype = filetype
        })
      end
    end
  end

  if #snippets == 0 then
    print("No snippets available for filetype: " .. ft)
    return
  end

  -- Create a buffer to display snippets
  local buf = vim.api.nvim_create_buf(false, true)
  local lines = {}

  table.insert(lines, "Available Snippets for " .. ft .. ":")
  table.insert(lines, string.rep("=", 50))
  table.insert(lines, "")

  for _, snippet in ipairs(snippets) do
    local line = string.format("%-15s | %-10s | %s",
      snippet.trigger,
      snippet.filetype,
      snippet.description
    )
    table.insert(lines, line)
  end

  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.api.nvim_buf_set_option(buf, 'modifiable', false)
  vim.api.nvim_buf_set_option(buf, 'filetype', 'text')

  -- Open in a split window
  vim.cmd('split')
  vim.api.nvim_win_set_buf(0, buf)
  vim.api.nvim_buf_set_keymap(buf, 'n', 'q', ':q<CR>', { noremap = true, silent = true })

end, { desc = "List available snippets for current filetype" })
