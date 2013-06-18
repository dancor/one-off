run: data ProcData
	bash -c 'time < data ./ProcData > out'

run-single: data ProcDataSingle
	bash -c 'time < data ./ProcDataSingle > out'

clean: 
	rm data ProcData

data: GenData.hs
	runghc GenData.hs 100000 > data

ProcData: ProcData.hs
	ghc -Wall -O2 -threaded -with-rtsopts=-N --make ProcData

ProcDataSingle: ProcDataSingle.hs
	ghc -Wall -O2 --make ProcDataSingle
