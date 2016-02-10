v1: v1.c
	gcc -o v1 v1.c -lm -O3

v2: v2.c
	gcc -o v2 v2.c -lm -lOpenCL -O3

cube-sum: cube-sum.c
	gcc -o cube-sum cube-sum.c -lm -O3

#cube-sum: cube-sum.hs
#	ghc -v0 --make -fforce-recomp -Odph -O2 -funbox-strict-fields \
#-fllvm \
#-optlo-disable-inlining -optlo-basicaa -optlo-basiccg \
#-optlo-count-aa -optlo-domfrontier -optlo-domtree \
#-optlo-globalsmodref-aa -optlo-memdep -optlo-no-aa \
#-optlo-postdomtree -optlo-codegenprepare \
#-optlo-functionattrs -optlo-constmerge \
#-optlo-constprop -optlo-die -optlo-dse -optlo-globaldce \
#-optlo-globalopt -optlo-indvars -optlo-inline -optlo-ipconstprop \
#-optlo-ipsccp -optlo-lcssa -optlo-loop-deletion \
#-optlo-loop-unroll -optlo-loop-unswitch -optlo-loop-simplify \
#-optlo-mem2reg -optlo-memcpyopt -optlo-scalarrepl \
#-optlo-tailcallelim cube-sum

#	ghc -v0 --make -fforce-recomp -Odph -O2 -funbox-strict-fields \
#-fllvm -optlo-disable-inlining -optlo-basicaa -optlo-basiccg \
#-optlo-count-aa -optlo-domfrontier -optlo-domtree \
#-optlo-globalsmodref-aa -optlo-memdep -optlo-no-aa \
#-optlo-postdomtree -optlo-codegenprepare -optlo-abcd \
#-optlo-functionattrs -optlo-block-placement -optlo-constmerge \
#-optlo-constprop -optlo-die -optlo-dse -optlo-globaldce \
#-optlo-globalopt -optlo-indvars -optlo-inline -optlo-ipconstprop \
#-optlo-ipsccp -optlo-lcssa -optlo-loop-deletion -optlo-loop-index-split \
#-optlo-loop-unroll -optlo-loop-unswitch -optlo-loopsimplify \
#-optlo-mem2reg -optlo-memcpyopt -optlo-scalarrepl \
#-optlo-tailcallelim cube-sum
