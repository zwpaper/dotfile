print("Loading Poker config")
local poker = {}

-- Monitor The poker connection
usbWatcher = nil
function usbDeviceCallback(data)
    hs.alert.show(data.productID)
    hs.alert.show(data.productName)
    hs.alert.show(data.vendorName)
    hs.alert.show(data.vendorID)
    if (data["productID"] == 1553) then
        if (data["eventType"] == "added") then
            pokerToHHKB()
            hs.alert.show("IKBC键盘插进来了˚¬˚")
        elseif (data["eventType"] == "removed") then
            hs.alert.show("IKBC键盘插拔出来了˚¬˚")
        end
    end
end
usbWatcher = hs.usb.watcher.new(usbDeviceCallback)
usbWatcher:start()

local function pressFn(mods, key)
	if key == nil then
		key = mods
		mods = {}
	end
	return function() hs.eventtap.keyStroke(mods, key, 10) end
end

function pokerToHHKB()
    -- \ to delete
    hs.hotkey.bind({}, 0x2a, pressFn(0x33), nil, pressFn(0x33))

end

return poker
