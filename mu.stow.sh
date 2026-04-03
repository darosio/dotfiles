#!/usr/bin/env sh
#

yay -S --noconfirm mu
yay -S --noconfirm isync
yay -S --noconfirm cyrus-sasl-xoauth2-git # XOAUTH2 SASL for M365 IMAP
# yay -S --noconfirm mb2md

stow -t "$HOME" mbsync

mkdir -p "$HOME"/Maildir/cnr
mkdir -p "$HOME"/Maildir/gmail
mkdir -p "$HOME"/Maildir/pec
# CNR subfolders created by first mbsync run; refs/keepup need pre-creation
# so messages can be migrated before syncing upstream
mkdir -p "$HOME"/Maildir/cnr/refs/cur "$HOME"/Maildir/cnr/refs/new "$HOME"/Maildir/cnr/refs/tmp
mkdir -p "$HOME"/Maildir/cnr/keepup/cur "$HOME"/Maildir/cnr/keepup/new "$HOME"/Maildir/cnr/keepup/tmp

# OAuth2 token directory for mutt_oauth2.py
mkdir -p "$HOME"/.config/mutt_oauth2
chmod 700 "$HOME"/.config/mutt_oauth2

echo ""
echo "=== CNR OAuth2 authorisation (manual step — requires browser) ==="
echo "Run once to obtain the initial token:"
echo ""
echo "  mutt_oauth2.py ~/.config/mutt_oauth2/cnr.tokens --verbose --authorize --authflow localhostauthcode"
echo ""
echo "Choose: registration=microsoft, email=daniele.arosio@cnr.it"
echo "A browser window opens — log in with CNR credentials, then paste the redirect URL back."
echo "Verify with: mutt_oauth2.py ~/.config/mutt_oauth2/cnr.tokens --verbose --test"
echo "================================================================="
echo ""

mbsync -a

mu init --maildir "$HOME"/Maildir/ --my-address=daniele.arosio@cnr.it \
  --my-address danielepietroarosio@gmail.com \
  --my-address daniele.arosio@postecert.it
mu index

cd "$HOME"/Maildir || exit
ln -sf "$HOME"/Sync/Maildir/archive .
ln -sf "$HOME"/Sync/Maildir/personal .
ln -sf "$HOME"/Sync/Maildir/mailrc .
