FC=				ifort
FFLAGS=		-O3 -qopenmp


SRC=			day14.f90
EXE=			day14

OBJ=	$(SRC:.f90=.o)

$(EXE) : $(OBJ) Makefile
	$(FC) $(FFLAGS) -o $@ $<

%.o : %.f90
	$(FC) $(FFLAGS) -c $<

.PHONY : clean
clean:
	rm -f $(EXE)
	rm -f $(OBJ)
