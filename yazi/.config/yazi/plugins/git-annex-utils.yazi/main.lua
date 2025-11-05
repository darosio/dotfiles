-- ~/.config/yazi/plugins/git-annex-utils.yazi/main.lua

-- Define the function inside the module
local function show_annex_size_action()
    -- The core logic to get the size
    local cmd = "git annex info --fast . | grep 'annex size' | head -n 1 | awk -F ':' '{print $2}' | xargs"
    
    local handle = io.popen(cmd)
    local output = handle and handle:read("*a") or ""
    if handle then handle:close() end

    -- Strip leading/trailing whitespace
    output = output:gsub("^%s*(.-)%s*$", "%1")

    -- Construct the final message
    if output == "" or output == "0 bytes" then
        output = "No annexed content found."
    else
        output = "Annex size: " .. output
    end

    -- Display the notification (5 seconds)
    -- 'ya' is the global Yazi API context
    ya.notify("info", "git-annex", output, 5)
end

-- Return the exposed functions in a table
return {
    -- Expose the function under a key that will be used in keymap.toml
    show_size = show_annex_size_action,
}
