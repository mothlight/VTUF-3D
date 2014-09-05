##
## Makefile for building a Maespa, could be nicer...
## Martin De Kauwe, 05/12/2013
##

PROG =    maespa.out

SRCS =    default_conditions.f90 switches.f90 getmet.f90 maindeclarations.f90 \
        inout.f90 maespa.f90 maestcom.f90 metcom.f90 physiol.f90 \
        radn.f90 unstor.f90 utils.f90 watbal.f90

OBJS =    default_conditions.o switches.o getmet.o maindeclarations.o inout.o \
        maespa.o maestcom.o metcom.o physiol.o radn.o \
        unstor.o utils.o watbal.o

LIBS =    

INCLS = 

F90 = gfortran

FFLAGS = -g -fbounds-check -finit-local-zero -Wuninitialized -ftrapv -ffree-form -ffree-line-length-none -O3

all: $(PROG)

$(PROG): $(OBJS)
	$(F90) $(FFLAGS) -o $@ $(OBJS) $(LIBS) $(INCLS)

clean:
	rm -f $(PROG) $(OBJS) *.mod

.SUFFIXES: $(SUFFIXES) .f90

.f90.o:
	$(F90) $(FFLAGS) -c $<

default_conditions.o: switches.o
getmet.o: maestcom.o metcom.o switches.o
maindeclarations.o: maestcom.o
inout.o: maestcom.o switches.o
maespa.o: maestcom.o metcom.o switches.o maindeclarations.o
physiol.o: maestcom.o metcom.o
radn2.o: maestcom.o
unstor.o: maestcom.o
utils.o: maestcom.o
watbal.o: maestcom.o metcom.o
