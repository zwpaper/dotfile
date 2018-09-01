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
---- karabiner-elements
keyboardWatcher = nil
deleteKarabinerCMD = "tell application \"Finder\" to delete \"/Users/didi/.config/karabiner/karabiner.json\" as POSIX file"
copyKarabinerHHKBcmd = "do shell script \"cp /Users/didi/.config/karabiner/karabiner.json.hhkb /Users/didi/.config/karabiner/karabiner.json\""
copyKarabinerPokerCMD = "do shell script \"cp /Users/didi/.config/karabiner/karabiner.json.poker /Users/didi/.config/karabiner/karabiner.json\""
copyKarabinerMacCMD = "do shell script \"cp /Users/didi/.config/karabiner/karabiner.json.mac /Users/didi/.config/karabiner/karabiner.json\""
function keyboardCallback(data)
    if (data["eventType"] == "added") then
        if (data["productID"] == 256) then
            hs.notify.show("hhkb inserted", "", "")
            local deleteResult = hs.osascript.applescript(deleteKarabinerCMD)
            local copyResult = hs.osascript.applescript(copyKarabinerHHKBcmd)
            hs.notify.show("Load HHKB karabiner config",
                           "delete old "..(deleteResult and "succ" or "fail"),
                           "load "..(copyResult and "succ" or "fail"))
        elseif (data["productID"] == 1553) then
            hs.notify.show("poker inserted", "", "")
            local deleteResult = hs.osascript.applescript(deleteKarabinerCMD)
            local copyResult = hs.osascript.applescript(copyKarabinerPokerCMD)
            hs.notify.show("Load Poker karabiner config",
                           "delete old "..(deleteResult and "succ" or "fail"),
                           "load "..(copyResult and "succ" or "fail"))
        end
    elseif (data["productID"] == 256) or (data["productID"] == 1553) then
            hs.notify.show("keyboar unplugged", "", "")
            local deleteResult = hs.osascript.applescript(deleteKarabinerCMD)
            local copyResult = hs.osascript.applescript(copyKarabinerMacCMD)
            hs.notify.show("Load HHKB karabiner config",
                           "delete old "..(deleteResult and "succ" or "fail"),
                           "load "..(copyResult and "succ" or "fail"))
    end
end
keyboardWatcher = hs.usb.watcher.new(keyboardCallback)
keyboardWatcher:start()

-- right option to chinese
noInput = "com.apple.keylayout.US"
qinggeInput = "com.aodaren.inputmethod.Qingg"
hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "z",
    function()
        source = hs.keycodes.currentSourceID()
        if source ~= qinggeInput then
            if not hs.keycodes.currentSourceID(qinggeInput) then
                hs.alert.show("Can not change input source to qingge")
            end
        end
end)

-- right command to english
hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "e",
    function()
        source = hs.keycodes.currentSourceID()
--        hs.alert.show(source)
        if source ~= noInput then
            if not hs.keycodes.currentSourceID(noInput) then
                hs.alert.show("Can not change input source to us")
            end
        end
end)



---- poker
-- poker = require('poker')


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

-- hs.hotkey.bind({"cmd", "alt", "ctrl"}, "W",
--     function()
--         test = hs.usb.attachedDevices()
--         for index, usbDevice in pairs(test) do
--             hs.alert.show("test")
--             hs.alert.show(usbDevice["productID"])
--         end
-- end)
