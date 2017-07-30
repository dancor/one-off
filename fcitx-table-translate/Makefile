install: build/wubi2008.mb
	sudo cp build/wubi2008.mb /usr/share/fcitx/tables

build/wubi2008.mb: build/wubi2008.txt
	txt2mb build/wubi2008.txt build/wubi2008.mb

build/wubi2008.txt:
	mkdir -p build && runghc -cpp src/translate.hs < original/wubi2008.conf > build/wubi2008.txt
