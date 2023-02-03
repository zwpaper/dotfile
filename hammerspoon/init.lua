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
hs.alert.defaultStyle['atScreenEdge'] = 2

--- input source
---
--- hs.keycodes.currentSourceID()
-- right option to Chinese
noInput = "com.apple.keylayout.ABC"
rimeInput = "im.rime.inputmethod.Squirrel.Hans"
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
      --- hs.alert.show(hs.keycodes.currentSourceID())
      if e == hs.application.watcher.activated then
         if (name == "Emacs" or name == "iTerm2") and
         not hs.keycodes.currentSourceID(noInput) then
            hs.alert.show("Can not change input source to US")
         elseif (name == "WeChat" or name == "WeCom") and
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

--- Model Window Ops
--Predicate that checks if a window belongs to a screen
function isInScreen(screen, win)
   return win:screen() == screen
end

function focusScreen(screen)
   --Get windows within screen, ordered from front to back.
   --If no windows exist, bring focus to desktop. Otherwise, set focus on
   --front-most application window.
   local windows = hs.fnutils.filter(
      hs.window.orderedWindows(),
      hs.fnutils.partial(isInScreen, screen))
   local windowToFocus = #windows > 0 and windows[1] or hs.window.desktop()
   windowToFocus:focus()

   -- Move mouse to center of screen
   local pt = hs.geometry.rectMidPoint(screen:fullFrame())
   hs.mouse.setAbsolutePosition(pt)
end

k = hs.hotkey.modal.new('cmd-ctrl', 'm')
function k:entered() hs.alert('Entered window mode') end
function k:exited()  hs.alert('Exited window mode')  end
k:bind('', 'escape', function() k:exit() end)
k:bind('', 'o', 'Other screen', function()
          hs.window.focusedWindow():moveToScreen(hs.screen.mainScreen():next(),ensureInScreenBounds)
end)
k:bind('', 'e', 'East Screen', function()
          hs.window.focusedWindow():moveOneScreenEast(ensureInScreenBounds, 0)
end)
k:bind('', 'w', 'West Screen', function()
          hs.window.focusedWindow():moveOneScreenWest(ensureInScreenBounds, 0)
end)
k:bind('', 'm', 'Maximize', function()
          hs.window.focusedWindow():maximize(0)
end)
k:bind('', 'f', 'Focus Other Screen', function()
          focusScreen(hs.window.focusedWindow():screen():next())
end)
k:bind('', 'k', 'Kill Focused Screen', function()
          hs.window.focusedWindow():close()
end)
k:bind('', 'n', 'Next Space', function()
          hs.eventtap.keyStroke({"ctrl"}, 'right')
end)
k:bind('', 'p', 'Previous Space', function()
          hs.eventtap.keyStroke({"ctrl", "fn"}, ';')
end)
k:bind('', 'a', 'Previous Space', nil, function()
          hs.eventtap.keyStroke({"ctrl"}, 'right')
end)

hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "5", nil, function()
      hs.eventtap.keyStroke({"cmd", "alt", "ctrl", "shift"}, "2")
end)
