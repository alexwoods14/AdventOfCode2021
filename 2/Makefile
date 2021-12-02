FC=				ifort
FFLAGS=		-O3


SRC=			day2.f90
EXE=			day2

OBJ=	$(SRC:.f90=.o)

$(EXE) : $(OBJ)
	$(FC) $(FFLAGS) -o $@ $<

%.o : %.f90
	$(FC) $(FFLAGS) -c $<

.PHONY : clean
clean:
	rm -f $(EXE)
	rm -f $(OBJ)
