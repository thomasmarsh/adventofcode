SRCS = $(wildcard */*.hs)
EXES = $(SRCS:.hs=)

all: $(EXES)

build:
	mkdir -p build

%: %.hs build
	ghc -outputdir build -O2 -Wall $< -o $@


clean:
	$(RM) -rf build
	$(RM) -f $(EXES)
