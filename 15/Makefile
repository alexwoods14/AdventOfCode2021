FC=				ifort
FFLAGS=		-O3 -qopenmp -Warn all


SRC=			day15.f90
EXE=			day15
MOD=			binheap.f90

OBJ=	$(MOD:.f90=.o) $(SRC:.f90=.o)

$(EXE) : $(OBJ)
	$(FC) $(FFLAGS) -o $@ $^

%.o : %.f90
	$(FC) $(FFLAGS) -c $<

.PHONY : clean
clean:
	rm -f $(EXE)
	rm -f $(OBJ)
	rm -f *.mod
