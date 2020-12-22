ddo: src/ddo.hs src/Ddo.hs ~/l/MyDdos/MyDdos.hs
	mkdir -p build; ghc -package HDBC-sqlite3 -package HDBC \
		-package unordered-containers -o ddo -cpp \
		-hidir build -odir build -isrc --make src/ddo.hs
