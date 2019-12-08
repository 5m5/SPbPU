FC=gfortran
FFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -Wno-unused-function -static-libgfortran -flto
FOPT=-O3 -ftree-vectorize -fopt-info-vec

all:
	mkdir -p bin obj
	$(FC) $(FFLAGS) -c src/spline.f -J obj/ -o obj/spline.o
	$(FC) $(FFLAGS) -c src/seval.f -J obj/ -o obj/seval.o
	$(FC) $(FFLAGS) $(FOPT) -c src/main.f90 -I obj/ -o obj/main.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/app ./obj/spline.o ./obj/seval.o obj/main.o

	cd ./bin; ./app;

clean:
	rm -rf obj/*
	rm -rf bin/*

run:
	cd ./bin; ./app;
