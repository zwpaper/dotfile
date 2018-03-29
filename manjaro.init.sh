# sudo to run init.sh
pacman-mirrors --fasttrack 5 && pacman -Syyu
pacman-optimize && sync

# aur
pacman -S base-devel yaourt --noconfirm

# keyboard
# space to alt and space
## // swap alt_r and space
echo 'partial modifier_keys
xkb_symbols "space_to_alt" {
    key <SPCE> { [ Alt_R, Meta_R ] };
    key <RALT> { [ space, space ] };
};
' > /usr/share/X11/xkb/symbols/ctrl

sed -i '/ctrl:nocaps/i \
  ctrl:space_to_alt     =       +ctrl(space_to_alt)
' /usr/share/X11/xkb/rules/evdev

sed -i '/ctrl:nocaps/i \
  ctrl:space_to_alt    space to alt
' /usr/share/X11/xkb/rules/evdev.lst

# set in .xprofile
setxkbmap -option ctrl:space_to_alt
xcape -e 'Alt_R=space'
## caps to ctrl, ctrl_l to ese when click
setxkbmap -option ctrl:nocaps
xcape -e 'Control_L=Escape'


# append setxkbmap and xcape to .zshrc after get dot file

# dot file

# emacs-nox

# tools
pacman -S tcpdump gcc gcc-c++ --noconfirm

read withX
if [[ X$withX == Xy ]]; then
    pacman -S chromium --noconfirm

fi
