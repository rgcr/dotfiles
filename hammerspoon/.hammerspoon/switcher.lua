-- buggy version, sometimes it doesn't work due to key event tap issues
-- for example inside iTerm and nvim/vim
-- so I keep it disabled for now

local M = {}

local cmdTapCount = 0
local inAppMode = false
local appTimer = nil

-- App bindings: tecla → nombre de app
local appBindings = {
    ["."] = "iTerm",
    [","] = "iTerm",
    f = "Firefox",
    e = "Microsoft Excel",
    m = "Microsoft Outlook",
    o = "Microsoft Outlook",
    n = "Microsoft OneNote",
    t = "Microsoft Teams",
    c = "Google Chrome",
    v = "Visual Studio Code",
}

local function resetAppMode()
    cmdTapCount = 0
    inAppMode = false
end

function M.start()
    -- Double tap
    local cmdWatcher = hs.eventtap.new({hs.eventtap.event.types.flagsChanged}, function(event)
        local flags = event:getFlags()
        if flags.cmd and not hs.eventtap.checkKeyboardModifiers().cmd then
            cmdTapCount = cmdTapCount + 1

            if cmdTapCount == 2 then
                inAppMode = true
                appTimer = hs.timer.doAfter(1, resetAppMode)
            elseif appTimer then
                appTimer:stop()
                appTimer = hs.timer.doAfter(1, resetAppMode)
            end
        end
        return false
    end)

    cmdWatcher:start()

    -- key tap to switch to apps
    local keyTap = hs.eventtap.new({hs.eventtap.event.types.keyDown}, function(event)
        if not inAppMode then return false end

        local key = hs.keycodes.map[event:getKeyCode()]
        local appName = appBindings[key]

        if appName then
            hs.application.launchOrFocus(appName)
            resetAppMode()
            return true
        end

        return false
    end)

    keyTap:start()
end

return M
