# shellcheck shell=bash

# Text Editors
alias e='emacsclient -a="" -c'
alias vi='vim'
alias viw='vim -c "match DiffDelete /\[-.\{-}-]/" -c "2match DiffAdd /{+.\{-}+}/"'
alias ee='$EDITOR $HOME/.emacs.d/init.el'
alias ea='$EDITOR $HOME/.bashrc.d/aliases.sh'

# File and system Operations
alias o='rifle'
alias t='tree'
alias lb='lsblk'
alias ls='ls --color=auto'
alias l='ls -F --color=always'
alias ll='ls -Flh'
alias la='ls -Fah'
alias lla='ls -Flah'
alias bigcp='rsync -Phrltz --info=progress2'
# alias rm='trash-put'
alias rmlintt='rmlint --keep-all-tagged --must-match-tagged -r -f -T "defaults -bl"'

# System Commands
alias du.='du -ksh .[!.]* * | sort -h'
alias grep='grep --color'
alias stow='stow -t ~/'
alias pandoc='pandoc --pdf-engine=xelatex'
alias impress='impressive -t none --nologo'

# Package Management
alias pyp='sudo pacmatic -Syu'
alias pya='pacaur -Syua'
alias pyu='sudo pacmatic -Syu; pya'

# Git
alias g='git st; git in; git out'
alias git-ever-existed='git log --pretty=format: --name-status | cut -f2- | sort -u'

# Display Management
alias xr0='xrandr --output HDMI-1 --rotate normal'
alias xr1='xrandr --output HDMI-1 --rotate inverted'
alias xr2='xrandr --output HDMI-2 --left-of HDMI-1 --rotate left'
alias xr='xrandr --output HDMI-1 --rotate inverted --output HDMI-2 --left-of HDMI-1 --rotate left'
alias xrr='xrandr --output eDP-1 --scale 1.24x1.24 --auto'
alias xrr2='xrandr --output DP-1 --left-of eDP-1 --auto'
alias vga_ont='xrandr --output VGA1 --auto --above LVDS1'
alias vga='xrandr --output VGA1 --auto --same-as LVDS1'
alias vga_onr='xrandr --output VGA1 --auto --right-of LVDS1'
alias 1left='xrandr --output DP-1 --auto --left-of eDP-1'
alias vga_off='xrandr --output VGA1 --off'
alias 1off='xrandr --output DP-1 --off'

alias xrv='ffmpeg -f v4l2 -video_size   1280x720 -pixel_format yuyv422 -i /dev/video1 -vf "transpose=2" -pix_fmt yuyv422 -f v4l2 /dev/video0'

# Translation Helper
alias tj='trans -j'
alias tji='trans -j :it'

# Miscellaneous
alias mu_tags_localTB='rg --no-line-number "X-Keywords: \S+" ~/Sync/Maildir/Local\ Folders/'
alias mu_tags_all_rg='rg --no-filename --no-line-number "X-Keywords: \S+" ~/Maildir/ ~/Sync/Maildir/{personal,archive} | cut -d: -f2 | tr "," "\n" | tr -s " " "\n" | sort | uniq | sed "/^$/d"'
alias mu_tags='mu find "" --format=json | rg ":tags" | tr -d "\n" | rg -oP ":tags\":\[\K[^\]]*" | tr -d "\"" | tr "," "\n" | tr -s " " "\n" | sort | uniq'
alias xev_arch="xev | grep -A2 --line-buffered '^KeyRelease' | sed -n '/keycode /s/^.*keycode \([0-9]*\).* (.*, \(.*\)).*$/\1 \2/p'"
alias xmm='xmodmap ~/.Xmodmap'

# WiFi Scan
alias wifiscan='sudo iw dev wlan0 scan | grep SSID'

# Pacman Helper
alias paclist-time='expac --timefmt="%Y-%m-%d %T" "%l\t%n" | sort'
alias paclist-size_d='expac -HM "%011m\t%-20n\t%10d" $( comm -23 <(pacman -Qqen|sort) <(pacman -Qqg base base-devel|sort) ) | sort -n'
# shellcheck disable=SC2142
alias paclist-size="expac -s \"%-30n %m\" | sort -hk 2 | awk '{printf \"%s %.0f MiB\n\", \$1, \$2/1024/1024}' | column -t"
alias pup='sudo pacmatic -Syu'
alias yy='pacaur -Ss'

# Shortcut for running 'mr --config $HOME/.mrconfig-progs'
alias mrp='mr --config $HOME/.mrconfig-progs'

# Python and pip update all
alias simple-server='python -m SimpleHTTPServer'
# http://stackoverflow.com/questions/2720014/upgrading-all-packages-with-pip
alias pip_updatealluser='pip freeze --user | grep -v '\''^-e'\'' | cut -d "=" -f 1 | xargs -I {} pip install -U --user {}'
# shellcheck disable=SC2142
alias pip_updateall='pip freeze --local | awk '\''!/^-e/ { print $1 }'\'' | xargs -I {} pip install -U {}'

# Web Browsing
alias chromep='chromium --proxy-server="localhost:8118"'
alias qfn='firefox -qfn'

# System Utilities
alias hide='export HISTSIZE=0'
alias windowsXP='VBoxManage startvm "winXPvb"'
alias lsfonts="fc-list | cut -d: -f2|  sed 's/.*,//' | sed 's/^ //' | sort -u | less"

# SSH and Remote Access
alias omero='sshpass -p CEaMW2oSQY2 ssh darosio%omero@gate.fbk.eu'
#alias omero-data='sshfs  darosio%omero@gate.fbk.eu:/data /media/omero/'
alias swan-data='sshfs swan:/hardmnt/swan1/data /media/swan/'
alias vigolana-data='sshfs -p 23456 vigolana:/data /media/vigolana/ -o IdentityFile=$HOME/.ssh/dan@sterzing-2013-06-03'
alias mmarzola='sshfs -p 23456 mmedia@marzola:/home/MM /media/marzola-mm'
alias omero-data0='sshfs  omero:/hardmnt/data0 /media/omero0/'
alias omero-data1='sshfs  omero:/hardmnt/data1 /media/omero1/'
alias svigolana='urxvt -bg "#111020" -e ssh -i ~/.ssh/dan@sterzing-2013-06-03 -p 23456 vigolana &'
alias smarzola='urxvt -bg "#111020" -e ssh -i .ssh/dan@hunza-2013-06-03 -p 23456 -l mmedia 192.168.1.10 &'
alias smarzola='urxvt -bg "#111020" -e ssh -i ~/.ssh/dan@hunza-2013-06-03 -p 23456 -l mmedia marzola &'
alias stresero='urxvt -bg "#122021" -e ssh -i ~/.ssh/mmedia@-2013-06-03 -p 23456 -l mmedia tresero &'
alias mtresero='sshfs -p 23456 mmedia@tresero:/home/MM /mnt/tresero-mm'
alias dpa-x='xterm -bg lightgrey -fg midnightblue -fn 10x20 -e ssh dan@192.168.1.2 &'
alias kngur-x='nohup xterm -bg lightgreen -fn 10x20 -e ssh dan@10.0.0.2 &'
