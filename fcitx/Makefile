all: danhebrew dankorean danrussian

danhebrew: danhebrew.txt
	cp conf/danhebrew.conf ~/.config/fcitx/table/
	txt2mb danhebrew.txt ~/.config/fcitx/table/danhebrew.mb
	echo "Now run fcitx -r somewhere (it may spew errors there later)>"
	echo "You don't need to restart programs; even fcitx-configtool auto"
	echo "reloads after fcitx -r. However fcitx -r may cause your xterm to"
	echo "hang and require xkill to kill it."

dankorean.txt: make-dankorean.hs
	runghc -cpp -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints make-dankorean.hs > dankorean.txt

dankorean: dankorean.txt
	cp conf/dankorean.conf ~/.config/fcitx/table/
	txt2mb dankorean.txt ~/.config/fcitx/table/dankorean.mb
	echo "Now run fcitx -r somewhere (it may spew errors there later)>"
	echo "You don't need to restart programs; even fcitx-configtool auto"
	echo "reloads after fcitx -r. However fcitx -r may cause your xterm to"
	echo "hang and require xkill to kill it."

danrussian: danrussian.txt
	cp conf/danrussian.conf ~/.config/fcitx/table/
	txt2mb danrussian.txt ~/.config/fcitx/table/danrussian.mb
	echo "Now run fcitx -r somewhere (it may spew errors there later)>"
	echo "You don't need to restart programs; even fcitx-configtool auto"
	echo "reloads after fcitx -r. However fcitx -r may cause your xterm to"
	echo "hang and require xkill to kill it."
