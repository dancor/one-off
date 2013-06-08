install: install-transl install-2008-transl install-plain install-2008-plain


install-transl: obj/wubi86-user.db
	sudo cp obj/wubi86-user.db /usr/share/ibus-table/tables

obj/wubi86-user.db: obj/wubi86-user.txt
	mkdir -p obj && cd obj && ibus-table-createdb -s wubi86-user.txt

obj/wubi86-user.txt: obj/transl src/wubi86.txt
	cat src/wubi86.txt | obj/transl > obj/wubi86-user.txt

obj/transl: src/transl.hs
	ghc -cpp --make src/transl.hs -o obj/transl -hidir obj -odir obj

install-plain: obj/wubi86.db
	sudo cp obj/wubi86.db /usr/share/ibus-table/tables

obj/wubi86.db: src/wubi86.txt
	mkdir -p obj && cd obj && ibus-table-createdb -s ../src/wubi86.txt

install-2008-transl: obj/wubi2008-user.db
	sudo cp obj/wubi2008-user.db /usr/share/ibus-table/tables

obj/wubi2008-user.db: obj/wubi2008-user.txt
	mkdir -p obj && cd obj && ibus-table-createdb -s wubi2008-user.txt

obj/wubi2008-user.txt: obj/transl src/wubi2008.txt
	cat src/wubi2008.txt | obj/transl > obj/wubi2008-user.txt

install-2008-plain: obj/wubi2008.db
	sudo cp obj/wubi2008.db /usr/share/ibus-table/tables

obj/wubi2008.db: src/wubi2008.txt
	mkdir -p obj && cd obj && ibus-table-createdb -s ../src/wubi2008.txt
