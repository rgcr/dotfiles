local M = {}

local app_hydra = nil
local menubar_indicator = nil
local text_indicator = nil
local timeout_timer = nil

local function create_app_hydra(app_bindings)
  if app_hydra then
    app_hydra:exit()
  end

  -- Clean up any existing indicators
  if menubar_indicator then
    menubar_indicator:delete()
    menubar_indicator = nil
  end
  if text_indicator then
    text_indicator:delete()
    text_indicator = nil
  end
  if timeout_timer then
    timeout_timer:stop()
    timeout_timer = nil
  end

  local hydra_bindings = {}

  -- Create hydra bindings from app_bindings table
  for key, appName in pairs(app_bindings) do
    hydra_bindings[#hydra_bindings + 1] = {key, function()
      local app = hs.application.find(appName)
      if app and app:isRunning() then
        app:activate()
      -- else
        -- hs.application.launchOrFocus(appName)

      end
      app_hydra:exit()
    end, appName}
  end

  -- Add escape key to exit hydra
  hydra_bindings[#hydra_bindings + 1] = {'escape', function()
    app_hydra:exit()
  end, 'Exit'}

  app_hydra = hs.hotkey.modal.new({}, nil)

  -- Bind all the keys
  for _, binding in pairs(hydra_bindings) do
    app_hydra:bind({}, binding[1], binding[2])
  end

  -- Show indicators when entering hydra
  app_hydra.entered = function()
    -- Clean up any existing indicators first
    if menubar_indicator then
      menubar_indicator:delete()
      menubar_indicator = nil
    end
    if text_indicator then
      text_indicator:delete()
      text_indicator = nil
    end
    if timeout_timer then
      timeout_timer:stop()
      timeout_timer = nil
    end

    -- Menubar indicator
    menubar_indicator = hs.menubar.new()
    if menubar_indicator then
      menubar_indicator:setTitle("⚡")
    end

    -- Set timeout to exit hydra after 1 seconds
    timeout_timer = hs.timer.doAfter(1, function()
      if app_hydra then
        app_hydra:exit()
      end
    end)
  end

  app_hydra.exited = function()
    if menubar_indicator then
      menubar_indicator:delete()
      menubar_indicator = nil
    end
    if text_indicator then
      text_indicator:delete()
      text_indicator = nil
    end
    if timeout_timer then
      timeout_timer:stop()
      timeout_timer = nil
    end
  end

  return app_hydra
end

function M.setup(app_bindings)
  -- Initialize the app hydra
  create_app_hydra(app_bindings)

  return {
    show = function()
      app_hydra:enter()
    end
  }
end

return M
