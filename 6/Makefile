FC=				ifort
FFLAGS=		-O3 -qopenmp


SRC=			day6.f90
EXE=			day6

OBJ=	$(SRC:.f90=.o)

$(EXE) : $(OBJ) Makefile
	$(FC) $(FFLAGS) -o $@ $<

%.o : %.f90
	$(FC) $(FFLAGS) -c $<

.PHONY : clean
clean:
	rm -f $(EXE)
	rm -f $(OBJ)
