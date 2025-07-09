-----------------------------------------------------------
-- File manager configuration file
-----------------------------------------------------------

-- Plugin: neorg

-- Keybindings are defined in `core/keymaps.lua`:
local status_ok, neorg = pcall(require, 'neorg')
if not status_ok then
  return
end

neorg.setup({
  load = {
    ["core.defaults"] = {},
    ["core.dirman"] = {
      config = {},
    },
    ["core.keybinds"] = {
      config = {
        default_keybinds = true,
        neorg_leader = "<leader>o",
      },
    },
    ["core.concealer"] = {
      config = {
        icons_preset = "basic",
        icons = {
          todo = {
            done = { icon = "", hl = "NeorgDone" },
            pending = { icon = "", hl = "NeorgPending" },
            undone = { icon = "", hl = "NeorgUndone" },
            on_hold = { icon = "", hl = "NeorgOnHold" },
            cancelled = { icon = "", hl = "NeorgCancelled" },
            recurring = { icon = "", hl = "NeorgRecurring" },
            uncertain = { icon = "", hl = "NeorgUncertain" },
          },
        },
      },
    },
  },
})

vim.wo.foldlevel = 99
vim.wo.foldenable = true
vim.wo.foldmethod = "expr"
vim.wo.foldexpr = "nvim_treesitter#foldexpr()"
