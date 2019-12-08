FC=gfortran
FFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -Wno-unused-function -static-libgfortran -flto
FOPT=-O3 -ftree-vectorize -fopt-info-vec

all:
	mkdir -p bin obj
	$(FC) $(FFLAGS) -c src/solve.f -J obj/ -o obj/solve.o
	$(FC) $(FFLAGS) -c src/decomp.f -J obj/ -o obj/decomp.o
	$(FC) $(FFLAGS) $(FOPT) -c src/main.f90 -I obj/ -o obj/main.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/app ./obj/solve.o ./obj/decomp.o obj/main.o

	cd ./bin; ./app;

clean:
	rm -rf obj/*
	rm -rf bin/*

run:
	cd ./bin; ./app;
