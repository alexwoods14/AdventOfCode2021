FC=				gfortran
FFLAGS=		-O3 -fopenmp
#FC=				ifort
#FFLAGS=		-O3 -qopenmp


SRC=			day16.f90
EXE=			day16

OBJ=	$(SRC:.f90=.o)

$(EXE) : $(OBJ) Makefile
	$(FC) $(FFLAGS) -o $@ $<

%.o : %.f90
	$(FC) $(FFLAGS) -c $<

.PHONY : clean
clean:
	rm -f $(EXE)
	rm -f $(OBJ)
