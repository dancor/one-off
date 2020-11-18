1-pull-templates: src/1-pull-templates.hs
	mkdir -p build; \
		ghc -cpp -O3 -o $@ --make $< -isrc -hidir build -odir build

2-pull-lang: src/2-pull-lang.hs
	mkdir -p build; \
		ghc -cpp -O3 -o $@ --make $< -isrc -hidir build -odir build

a.out: src/pull-lang.c
	gcc -O3 -o $@ -march=native $<

a: src/a.hs
	mkdir -p build; \
		ghc -cpp -O3 -o $@ --make $< -isrc -hidir build -odir build
