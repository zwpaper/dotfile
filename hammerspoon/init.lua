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
--- hs.keycodes.currentSourceID()
-- right option to Chinese
noInput = "com.apple.keylayout.ABC"
rimeInput = "im.rime.inputmethod.Squirrel.Rime"
hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "z",
   function()
      if not hs.keycodes.currentSourceID(rimeInput) then
         hs.alert.show("Can not change input source to rime")
      end
end)

-- right command to English
hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "e",
    function()
       if not hs.keycodes.currentSourceID(noInput) then
          hs.alert.show("Can not change input source to US")
       end
end)

--- changing input source depends on apps
inputWatcher = hs.application.watcher.new(
   function(name, e, app)
      if e == hs.application.watcher.activated then
         if (name == "Emacs" or name == "iTerm2") and
         not hs.keycodes.currentSourceID(noInput) then
            hs.alert.show("Can not change input source to US")
         elseif (name == "WeChat" or name == "WeChat Work") and
         not hs.keycodes.currentSourceID(rimeInput) then
            hs.alert.show("Can not change input source to Rime")
         end
      end
end)
inputWatcher:start()

--- bind super+0 => change input source -> activate alfred
hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "0",
   function()
      if not hs.keycodes.currentSourceID(noInput) then
         hs.alert.show("Can not change input source to us")
      end
      hs.eventtap.keyStroke({"cmd", "alt", "ctrl", "shift"}, "8")
end)

hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "9",
   function()
      if not hs.keycodes.currentSourceID(noInput) then
         hs.alert.show("Can not change input source to us")
      end
      hs.eventtap.keyStroke({"cmd", "alt", "ctrl", "shift"}, "7")
end)
