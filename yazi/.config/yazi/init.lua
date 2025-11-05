require("git"):setup()

-- show symlint in statusbar
Status:children_add(function(self)
	local h = self._current.hovered
	if h and h.link_to then
		return " -> " .. tostring(h.link_to)
	else
		return ""
	end
end, 3300, Status.LEFT)

-- Show git-annex total size (fast)
local function show_annex_size()
	local cmd = "git annex info --fast . 2>/dev/null | grep 'annex size' | cut -f 2 -d: | xargs"
	local handle = io.popen(cmd)
	if not handle then
		ya.notify({ title = "git-annex", content = "Failed to run git-annex info", level = "warn" })
		return
	end

	local output = handle:read("*a") or ""
	handle:close()

	output = output:gsub("\n", "")
	if output == "" then
		ya.notify({ title = "git-annex", content = "No annex data found here", level = "info" })
	else
		ya.notify({ title = "git-annex", content = "Annex size: " .. output, level = "info" })
	end
end

-- Expose it as a global so keymap.toml can call it
_G.show_annex_size = show_annex_size
