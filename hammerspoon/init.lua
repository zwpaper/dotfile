-- Reload config automatically
function reloadConfig()
    configFileWatcher:stop()
    configFileWatcher = nil
    hs.reload()
    hs.notify.show("hammerspoon reload suc", "", "")
end
configFileWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig)
configFileWatcher:start()


-- Misc setup
hs.window.animationDuration = 0


-- Keyboard Settting
---- general setting
------- caps to ctrl and esc
sendESC = true
maxFlag = 0
controlKeyTimer =
hs.timer.delayed.new(0.15, function() sendESC = false end)

controlHandler = function(evt)
    local newMods = evt:getFlags()
    local count = 0
    for _ in pairs(newMods) do
        count = count + 1
    end
    if maxFlag < count then maxFlag = count end
    if  1 == maxFlag and newMods["ctrl"] then
        sendESC = true
        controlKeyTimer:start()
        return true
    end
    if 0 == count then
        if 1 == maxFlag and sendESC then
            hs.eventtap.keyStroke({}, "ESCAPE", 5)
            sendESC = false
            maxFlag = 0
            controlKeyTimer:stop()
            return true
        end
        sendESC = false
        maxFlag = 0
    end
    return false
end
controlTap = hs.eventtap.new(
    {hs.eventtap.event.types.flagsChanged}, controlHandler)
controlTap:start()
-- end caps to ctrl and esc

-- space to space and option


---- poker
poker = require('poker')


-- testing
testHandler = function(evt)
    local key = evt:getCharacters(true)
    hs.alert.show(key)
    local flags = evt:getFlags()
    local count = 0
    for _ in pairs(flags) do
        count = count + 1
    end
    hs.alert.show(count)
    hs.alert.show(key)
end
testTap = hs.eventtap.new(
    {hs.eventtap.event.types.keyDown,
     hs.eventtap.event.types.flagsChanged},
    testHandler)
-- testTap:start()

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "W",
    function()
        test = hs.usb.attachedDevices()
        for index, usbDevice in pairs(test) do
            hs.alert.show("test")
            hs.alert.show(usbDevice["productID"])
        end
end)
