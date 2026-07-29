#!/usr/bin/env sh
#
# Check if the script is already running in an isolated environment
if [ -z "$ISOLATED_ENV" ]; then
  # Re-run the script in an isolated environment without 'exec' so it continues
  env -i HOME="$HOME" USER="$USER" PATH="/usr/bin:$PATH" ISOLATED_ENV=1 "$0" "$@"
  exit # Ensure we don't run the original environment after re-execution
fi
# Clear any lingering variables that might interfere
unset PYTHONPATH

yay -S --noconfirm yazi

yay -S --noconfirm zoxide
yay -S --noconfirm ffmpegthumbnailer
yay -S --noconfirm imagemagick
yay -S --noconfirm perl-image-exiftool
yay -S --noconfirm poppler # pdftoppm, also used by the office previewer
yay -S --noconfirm resvg
yay -S --noconfirm mediainfo
yay -S --noconfirm imv
yay -S --noconfirm bat  # for piper
yay -S --noconfirm eza  # for piper
yay -S --noconfirm 7zip # for decompressing
yay -S --noconfirm ouch # for previews

# Previewers and openers referenced by yazi.toml / keymap.toml
yay -S --noconfirm csvlens           # *.csv opener
yay -S --noconfirm w3m               # html previewer (pandoc is the fallback)
yay -S --noconfirm python-html2text  # view_mail_html.py, *.eml previewer
yay -S --noconfirm calibre           # ebook-meta, epub previewer
yay -S --noconfirm chmlib            # extract_chmLib, *.chm previewer
yay -S --noconfirm djvulibre         # ddjvu, djvu-view plugin
yay -S --noconfirm libreoffice-fresh # office previewer (doc/docx -> pdf)
yay -S --noconfirm zenity            # git-annex size popup (<A-TAB>)
yay -S --noconfirm wl-clipboard      # Y yanks to the system clipboard
yay -S --noconfirm fd                # search --via=fd
yay -S --noconfirm ripgrep-all       # search --via=rga
yay -S --noconfirm handlr-regex      # handlr, the `open` opener
# `mu` powers the message/rfc822 previewer but pulls in a full mail setup;
# install it via mu.stow.sh rather than here.

mkdir -p "$HOME"/.bashrc.d
# Create the plugins directory *before* stowing: it makes stow link each plugin
# individually instead of linking the whole directory into this repo, which
# would make `ya pkg install` write third-party plugins into the dotfiles.
mkdir -p "$HOME"/.config/yazi/plugins
mkdir -p "$HOME"/.local/share/applications
stow -t "$HOME" yazi

ya pkg install
