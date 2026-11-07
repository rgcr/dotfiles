-- Helper function to ensure a spoon is installed
local function ensure_spoon(name)
  local path = string.format("%s/.hammerspoon/Spoons/%s.spoon", os.getenv("HOME"), name)
  if not hs.fs.attributes(path, "mode") then
    hs.alert.show("Installing " .. name)
    hs.execute(string.format("curl -fLo '%s.zip' https://github.com/Hammerspoon/Spoons/raw/master/Spoons/%s.spoon.zip && unzip -o '%s.zip' -d ~/.hammerspoon/Spoons", path, name, path))
  end
  hs.loadSpoon(name)
end

-- Install and load the SpoonInstall
ensure_spoon("SpoonInstall")
-- Set SpoonInstall to use synchronous installation
spoon.SpoonInstall.use_syncinstall = true

-- Load the SpoonInstall spoon
local Install=spoon.SpoonInstall


-- key definitions
------------------
local cmd_ctrl = { "cmd", "ctrl" }
local cmd_alt = { "cmd", "alt" }
local cmd_shift = { "cmd", "shift" }
local ctrl = { "ctrl" }
local ctrl_shift = { "ctrl", "shift" }
local ctrl_alt = { "ctrl", "alt" }

local hyper = { "cmd", "ctrl", "alt"}
local hyper_shift = { "cmd", "ctrl", "alt", "shift" }


-- Keybindings
------------------

-- Reload Hammerspoon config | Cmd + Ctrl + Alt + R
hs.hotkey.bind(hyper, "R", "Reloading config", function()
  hs.reload()
end)

-- (Space indicator moved to end of file)

-- Quit the frontmost app | Cmd + Shift + Q
hs.hotkey.bind(cmd_shift, "q", function()
    local app = hs.application.frontmostApplication()
    if app then
        app:kill()
    end
end)

-- Close focused window | Cmd + Q (overrides macOS default Quit)
hs.hotkey.bind({"cmd"}, "q", function()
    local win = hs.window.focusedWindow()
    if win then win:close() end
end)

-- Focus on spceficic apps
local app_bindings = {
    ["return"] = "iTerm",
    ["."] = "iTerm",
    -- [","] = "iTerm",
    f = "Firefox",
    h = "Herdr",
    e = "Microsoft Edge",
    x = "Microsoft Excel",
    m = "Microsoft Outlook",
    o = "Obsidian",
    n = "Microsoft OneNote",
    t = "Microsoft Teams",
    c = "Google Chrome",
    -- v = "VsCodium",
    v = "Code",
}

-- Focus on specific applications using hyper key | Cmd + Ctrl + Alt + Key
for key, appName in pairs(app_bindings) do
  hs.hotkey.bind(hyper, key, function()
    -- Check if the app is already running
    local app = hs.application.find(appName)
    if app and app:isRunning() then
        -- If running, focus the app
        app:activate()
        return
    end
    -- hs.application.launchOrFocus(appName)
  end)
end

-- -- Window management
-- local window_management_bindings = {
--    left_half   = { {"ctrl", "alt"}, "Left" },
--    right_half  = { {"ctrl", "alt"}, "Right" },
--    top_half    = { {"ctrl", "alt"}, "Up" },
--    bottom_half = { {"ctrl", "alt"}, "Down" },
--    top_left    = { {"ctrl", "alt"}, "1" },
--    top_right   = { {"ctrl", "alt"}, "2" },
--    bottom_left = { {"ctrl", "alt"}, "3" },
--    bottom_right= { {"ctrl", "alt"}, "4" },
--
--    third_left  = { {"ctrl", "alt", "shift"}, "h" },
--    third_right = { {"ctrl", "alt", "shift"}, "l" },
--    third_up    = { {"ctrl", "alt", "shift"}, "k" },
--    third_down  = { {"ctrl", "alt", "shift"}, "j" },
--
--    max_toggle  = { {"ctrl", "alt"}, "f" },
--    max         = { {"ctrl", "alt", "shift"}, "Up" },
--    undo        = { {"ctrl", "alt"}, "z" },
--    center      = { {"ctrl", "alt"}, "c" },
--    larger      = { {"ctrl", "alt", "shift"}, "Right" },
--    smaller     = { {"ctrl", "alt", "shift"}, "Left" },
-- }

-- Install:andUse("WindowHalfsAndThirds", {
--     hotkeys = window_management_bindings,
-- })


-- move window to next screen
hs.hotkey.bind(hyper, "right", function()
    local win = hs.window.focusedWindow()
    if win then
        local nextScreen = win:screen():next()
        if nextScreen then
            win:moveToScreen(nextScreen)
        end
    end
end)
--
-- move window to previous screen
hs.hotkey.bind(hyper, "left", function()
    local win = hs.window.focusedWindow()
    if win then
        local prevScreen = win:screen():previous()
        if prevScreen then
            win:moveToScreen(prevScreen)
        end
    end
end)


-- Lock screen
hs.hotkey.bind(hyper, "l", function()
  hs.caffeinate.lockScreen()
end)

-- App switch - hydra mode: ctrl -> . -> key
local prefix_apps = {}
for key, app in pairs(app_bindings) do
  if key ~= "." then prefix_apps[key] = app end
end
local app_hydra = require("app_hydra").setup(prefix_apps)
hs.hotkey.bind({"ctrl"}, ".", function()
  app_hydra.show()
end)

-- Ctrl+., then . toggles the two most recently focused real windows.
do
  local recent = hs.window.filter.new(true):setOverrideFilter({
    allowRoles = "AXStandardWindow",
  }):setAppFilter("Hammerspoon", false)
  -- Keep focus history live, including changes made outside this shortcut.
  recent:subscribe(hs.window.filter.windowFocused, function() end)
  app_hydra.bind(".", function()
    local current = hs.window.focusedWindow()
    local currentID = current and current:id()
    for _, win in ipairs(recent:getWindows(hs.window.filter.sortByFocusedLast)) do
      if win:id() ~= currentID then
        local app = win:application()
        if app then app:unhide(); app:activate() end
        if win:isMinimized() then win:unminimize() end
        win:raise():focus()
        return
      end
    end
  end)
end

-- I have my custom engines in a custom file ~/.hammerspoon/search_engines.lua
-- local searcher = require("searcher").setup()
-- hs.hotkey.bind(hyper, "space", function()
--   searcher.show()
-- end)

-- Search all windows with Ctrl+., then Space, prioritizing the current desktop.
do
  local windows = hs.window.filter.new(true):setOverrideFilter({
    allowRoles = "AXStandardWindow",
  })
  local currentDesktop = hs.window.filter.new():setOverrideFilter({
    currentSpace = true,
    visible = true,
    allowRoles = "AXStandardWindow",
  })
  local chooser = require("window_picker").new(function(choice)
    if not choice then return end
    -- Keep the actual window: hs.window.get() may not find other Spaces.
    -- Let the webview finish hiding before activating another application.
    hs.timer.doAfter(0.1, function()
      local win = choice.window
      if win and win:id() == choice.windowID then
        local app = win:application()
        if app then app:unhide(); app:activate() end
        if win:isMinimized() then win:unminimize() end
        win:raise():focus()
      else
        hs.alert.show("That window has closed")
      end
    end)
  end)

  local function showWindowSearch()
    if chooser:isVisible() then chooser:hide(true); return end
    local currentIDs = {}
    for _, win in ipairs(currentDesktop:getWindows()) do
      currentIDs[win:id()] = true
    end
    local choices, others = {}, {}
    for _, win in ipairs(windows:getWindows(hs.window.filter.sortByFocusedLast)) do
      -- A window may close while we build the list; skip stale/helper entries.
      local ok, choice = pcall(function()
        local app = win:application()
        local bundle = app and app:bundleID()
        local icon = bundle and hs.image.imageFromAppBundle(bundle)
        local name = app and app:name()
        if not icon or not name or name == "" then return nil end
        return {
          text = name,
          subText = win:title() or "",
          image = icon,
          windowID = win:id(),
          window = win,
          currentDesktop = currentIDs[win:id()] == true,
        }
      end)
      if ok and choice then
        local group = currentIDs[choice.windowID] and choices or others
        group[#group + 1] = choice
      end
    end
    for _, choice in ipairs(others) do choices[#choices + 1] = choice end
    chooser:show(choices)
  end

  app_hydra.bind("space", showWindowSearch)
  hs.hotkey.bind(hyper, "space", showWindowSearch)
end

-- Menubar indicator for current Desktop Space (focused screen)
-- Keep references globally to avoid Lua GC removing the item/watchers
spaceIndicator = spaceIndicator or {}
spaceIndicator.menu = spaceIndicator.menu or hs.menubar.new()

local function spaceIndexForScreen(screen)
  local focused = hs.spaces.focusedSpace()
  if not (screen and focused) then return nil end
  local ok, spaces = pcall(hs.spaces.spacesForScreen, screen)
  spaces = (ok and spaces) or {}
  local idx, count = nil, 0
  for _, sid in ipairs(spaces) do
    local t = hs.spaces.spaceType and hs.spaces.spaceType(sid) or "user"
    if t == "user" then
      count = count + 1
      if sid == focused then idx = count end
    end
  end
  return idx
end

local function updateSpaceIndicator()
  if not spaceIndicator.menu then return end
  local idx = spaceIndexForScreen(hs.screen.mainScreen())
  local title = idx and ("[" .. tostring(idx) .. "]") or "[?]"
  spaceIndicator.menu:setTitle(title)
  spaceIndicator.menu:setTooltip("Current Space (focused screen)")
end

updateSpaceIndicator()

if not spaceIndicator.spacesWatcher then
  spaceIndicator.spacesWatcher = hs.spaces.watcher.new(function()
    updateSpaceIndicator()
  end)
  spaceIndicator.spacesWatcher:start()
end

if not spaceIndicator.screenWatcher then
  spaceIndicator.screenWatcher = hs.screen.watcher.new(function()
    hs.timer.doAfter(0.2, updateSpaceIndicator)
  end)
  spaceIndicator.screenWatcher:start()
end
