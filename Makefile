maespa := maespa

NETCDFCDEPS=-I/usr/local/include/ 
NETCDFLDEPS=-L/usr/local/lib/ 
DEBUGFLAG=

FC      := /usr/bin/gfortran
FCFLAGS =$(NETCDFLDEPS) $(DEBUGFLAG) -c -fdefault-real-8 -fbacktrace -fno-align-commons -ftrace=full -fcheck=all  
FLFLAGS =$(NETCDFCDEPS) -J maespa $(DEBUGFLAG)   -std=f2003 -Warray-bounds  -Waliasing -Wampersand -Wsurprising -Wintrinsics-std -Wno-tabs -Wintrinsic-shadow -Wline-truncation -Wunderflow -fbounds-check -ffree-line-length-none
PROGRAM = TUF3Dradiation

LIBS = 

INCLS = 

SRCS=UTCI.f90 MaespaConfigStateUtils.f90 Constants.f90 maestcom2.f90 MaespaConfigState.f90 ReadMaespaConfigs.f90 Dyn_Array.f90  maespa/maestcom.f90 maespa/utils.f90 maespa/switches.f90 maespa/maindeclarations.f90 maespa/inout.f90 maespa/radn.f90 maespa/physiol.f90 maespa/getmet.f90 maespa/default_conditions.f90 maespa/unstor.f90 maespa/watbal.f90    MaespaResult.f90 MaespaState.f90  maespa_modules.f90 TUFConstants.f90 barray_cube.f90 crosspro.f90 intrinsics.f90 ReverseRay.f90 TUFreg3D.f90 dotpro.f90  
  
   
all: project_code $(PROGRAM) 

$(PROGRAM): $(SRCS)
	$(FC) $(FLFLAGS) -o $@ $^ $(NETCDFLDEPS)

%.o: %.f90
	$(FC) $(FCFLAGS) -o $@ $< 

clean:
	rm -f $(PROGRAM) $(OBJS) *.mod ./maespa/*.mod *.o ./maespa/*.o

project_code:
	cd maespa && make

%.o : %.mod
