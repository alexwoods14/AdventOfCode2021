FC=				ifort
FFLAGS=		-O0 -g


SRC=			day4.f90
EXE=			day4

OBJ=	$(SRC:.f90=.o)

$(EXE) : $(OBJ) Makefile
	$(FC) $(FFLAGS) -o $@ $<

%.o : %.f90
	$(FC) $(FFLAGS) -c $<

.PHONY : clean
clean:
	rm -f $(EXE)
	rm -f $(OBJ)
