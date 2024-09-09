HC = ghc
SRC = $(wildcard Src/*.hs)
EXE = Bin/prog
FLAGS = -no-keep-hi-files -no-keep-o-files

$(EXE): $(SRC)
	mkdir -p Bin
	$(HC) $(SRC) $(FLAGS) -o $(EXE)
