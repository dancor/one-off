all: dan1345 danhebrew dankorean danrussian

dan1345: dan1345.txt
	mkdir -p ~/.config/fcitx/table/
	cp conf/dan1345.conf ~/.config/fcitx/table/
	txt2mb dan1345.txt ~/.config/fcitx/table/dan1345.mb
	echo "Now run fcitx -r somewhere (it may spew errors there later)>"
	echo "You don't need to restart programs; even fcitx-configtool auto"
	echo "reloads after fcitx -r. However fcitx -r may cause your xterm to"
	echo "hang and require xkill to kill it."

danarabic: danarabic.txt
	mkdir -p ~/.config/fcitx/table/
	cp conf/danarabic.conf ~/.config/fcitx/table/
	txt2mb danarabic.txt ~/.config/fcitx/table/danarabic.mb
	echo "Now run fcitx -r somewhere (it may spew errors there later)>"
	echo "You don't need to restart programs; even fcitx-configtool auto"
	echo "reloads after fcitx -r. However fcitx -r may cause your xterm to"
	echo "hang and require xkill to kill it."

danhebrew: danhebrew.txt
	mkdir -p ~/.config/fcitx/table/
	cp conf/danhebrew.conf ~/.config/fcitx/table/
	txt2mb danhebrew.txt ~/.config/fcitx/table/danhebrew.mb
	echo "Now run fcitx -r somewhere (it may spew errors there later)>"
	echo "You don't need to restart programs; even fcitx-configtool auto"
	echo "reloads after fcitx -r. However fcitx -r may cause your xterm to"
	echo "hang and require xkill to kill it."

dankorean.txt: make-dankorean.hs
	runghc -cpp -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints make-dankorean.hs > dankorean.txt

dankorean: dankorean.txt
	mkdir -p ~/.config/fcitx/table/
	cp conf/dankorean.conf ~/.config/fcitx/table/
	txt2mb dankorean.txt ~/.config/fcitx/table/dankorean.mb
	echo "Now run fcitx -r somewhere (it may spew errors there later)>"
	echo "You don't need to restart programs; even fcitx-configtool auto"
	echo "reloads after fcitx -r. However fcitx -r may cause your xterm to"
	echo "hang and require xkill to kill it."

danrussian: danrussian.txt
	mkdir -p ~/.config/fcitx/table/
	cp conf/danrussian.conf ~/.config/fcitx/table/
	txt2mb danrussian.txt ~/.config/fcitx/table/danrussian.mb
	echo "Now run fcitx -r somewhere (it may spew errors there later)>"
	echo "You don't need to restart programs; even fcitx-configtool auto"
	echo "reloads after fcitx -r. However fcitx -r may cause your xterm to"
	echo "hang and require xkill to kill it."
