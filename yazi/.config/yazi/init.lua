require("git"):setup()

require("starship"):setup({
    -- Hide flags (such as filter, find and search). This is recommended for starship themes which
    -- are intended to go across the entire width of the terminal.
    hide_flags = false, -- Default: false
    -- Whether to place flags after the starship prompt. False means the flags will be placed before the prompt.
    flags_after_prompt = true, -- Default: true
    -- Custom starship configuration file to use
    config_file = "~/.config/starship_full.toml", -- Default: nil
})

-- show symlink in statusbar
Status:children_add(function(self)
	local h = self._current.hovered
	if h and h.link_to then
		return " -> " .. tostring(h.link_to)
	else
		return ""
	end
end, 3300, Status.LEFT)

-- Real git-annex size in the `size` linemode (see yazi.toml).
--
-- An annexed file is a symlink into .git/annex/objects whose target encodes the
-- content size: SHA256E-s2500000--<hash>.bin. Once the content is dropped that
-- name is the only surviving record of it, and the size column falls back to
-- stat'ing the symlink itself - reporting ~200 B for a 2.4 MB file. Reading the
-- size back out of the target keeps the column meaningful for dropped content,
-- which is the state most of an annex is in.
--
-- Restricted to annex object links: any other symlink keeps the preset's
-- behaviour, so a target that merely happens to contain -s<digits>-- (say
-- ../linux-6.12-s390x/vmlinuz) is not misreported.
function Linemode:size()
	local target = self._file.link_to
	if target then
		local bytes = tostring(target):match("%.git/annex/objects/.*%-s(%d+)%-%-")
		if bytes then
			return ya.readable_size(tonumber(bytes))
		end
	end

	-- Preset behaviour from here down: directories have no size of their own,
	-- so they show how many entries they hold once that is known.
	local size = self._file:size()
	if size then
		return ya.readable_size(size)
	end

	local folder = cx.active:history(self._file.url)
	return folder and tostring(#folder.files) or ""
end
