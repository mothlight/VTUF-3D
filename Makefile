maespa := maespa

# compiler
FC      := /usr/bin/gfortran
#FCFLAGS = -g -c -fdefault-real-8 -fbacktrace -fno-align-commons -ftrace=full -fcheck=all -std=f95
FCFLAGS = -g -c -fdefault-real-8 -fbacktrace -fno-align-commons -ftrace=full -fcheck=all  
##FLFLAGS = -g   -std=f95 
FLFLAGS =  -J maespa -g   -std=f2003 -Warray-bounds  -Waliasing -Wampersand -Wsurprising -Wintrinsics-std -Wno-tabs -Wintrinsic-shadow -Wline-truncation -Wunderflow -fbounds-check -ffree-line-length-none
#  -fmax-errors=4
PROGRAM = TUF3Dradiation

LIBS = 

INCLS = 

# source files and objects
#SRCS=switches.f90 maestcom.f90 maindeclarations.f90 MaespaConfigState.f90 MaespaConfigStateUtils.f90  metcom.f90 default_conditions.f90 physiol.f90 watbal.f90 unstor.f90 maespa_modules.f90 MaespaState.f90 MaespaResult.f90 Dyn_Array.f90 Maespa_Dyn_Array.f90 utils.f90  inout.f90  radn.f90 getmet.f90 TUFConstants.f90 for2r.f90 barray_cube.f90 ReadMaespaConfigs.f90 barray_cube_replacement.f90 crosspro.f90 intrinsics.f90 TUFreg3D_Radiation_Sectional.f90 dotpro.f90 
SRCS= ReadMaespaConfigs.f90 Dyn_Array.f90  maespa/maestcom.f90 maespa/utils.f90 maespa/switches.f90 maespa/maindeclarations.f90 MaespaConfigState.f90 MaespaConfigStateUtils.f90  MaespaResult.f90 MaespaState.f90  maespa_modules.f90 TUFConstants.f90 barray_cube.f90 crosspro.f90 intrinsics.f90 TUFreg3D_Radiation_Sectional.f90 dotpro.f90  
  
#   
   
all: project_code $(PROGRAM) 

$(PROGRAM): $(SRCS)
	$(FC) $(FLFLAGS) -o $@ $^

%.o: %.f90
	$(FC) $(FCFLAGS) -o $@ $<

clean:
	rm -f $(PROGRAM) $(OBJS) *.mod ./maespa/*.mod *.o ./maespa/*.o

project_code:
	cd maespa && make

