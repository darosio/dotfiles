For Calibre just remember: /Sync/media/books

In general store:

    .local/share/applications/***.desktop
    .config/***

MAYBE:

cat > .git/hooks/post-commit: #!/usr/bin/env sh git push origin master

# ApE

.ApE/ApE_Defaults.txt .ApE/DNA_ladders.txt

# pcmanfm

.config/libfm/libfm.conf

# thunderbird

.thunderbird/

# VirtualBox

.VirtualBox VMs/XP32-2015/XP32-2015.vbox .VirtualBox
VMs/XP32-2015/XP32-2015.vbox-prev .VirtualBox
VMs/XP32-MO2007_Origin7/XP32-MO2007_Origin7.vbox .VirtualBox
VMs/XP32-MO2007_Origin7/XP32-MO2007_Origin7.vbox-prev .VirtualBox
VMs/old64/old64.vbox .VirtualBox VMs/old64/old64.vbox-prev
.config/VirtualBox/VirtualBox.xml .config/VirtualBox/VirtualBox.xml-prev
.config/VirtualBox/compreg.dat .config/VirtualBox/xpti.dat

## and ignore:

.config/VirtualBox/_.log .config/VirtualBox/_.log._ .VirtualBox\ VMs/_/Logs/\*
