-----------------------------------------------------------
-- File manager configuration file
-----------------------------------------------------------

-- Plugin: neo-tree

-- Keybindings are defined in `core/keymaps.lua`:
local status_ok, neotree = pcall(require, 'neo-tree')
if not status_ok then
  return
end

neotree.setup({
  close_if_last_window = true,
  sort_case_insensitive = true,
  sort_function = nil,
  enable_git_status = true,
  enable_diagnostics = false,
  -- open_on_setup = false,
  -- open_on_setup_file = false,
  open_on_tab = false,
  update_cwd = true,
  hijack_directories = {
    auto_open = true
  },
  view = {
    width = 30,
    side = 'left',
    auto_resize = true
  },
}) -- end setup
