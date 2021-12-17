FC=				gfortran
FFLAGS=		-O3 -fopenmp
#FC=				ifort
#FFLAGS=		-O3 -qopenmp


SRC=			day17.f90
EXE=			day17

OBJ=	$(SRC:.f90=.o)

$(EXE) : $(OBJ)
	$(FC) $(FFLAGS) -o $@ $<

%.o : %.f90
	$(FC) $(FFLAGS) -c $<

.PHONY : clean
clean:
	rm -f $(EXE)
	rm -f $(OBJ)
