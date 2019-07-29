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

--- input source
---
-- right option to Chinese
noInput = "com.apple.keylayout.ABC"
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

-- right command to English
hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "e",
    function()
        source = hs.keycodes.currentSourceID()
        if source ~= noInput then
           if not hs.keycodes.currentSourceID(noInput) then
               hs.alert.show("Can not change input source to US")
            end
        end
end)

--- changing input source depends on apps
hs.application.watcher.new(
   function(name, e, app)
      if e == hs.application.watcher.activated then
         if name == "Emacs" and not hs.keycodes.currentSourceID(noInput) then
            hs.alert.show("Can not change input source to US")
         end
      end
end):start()
