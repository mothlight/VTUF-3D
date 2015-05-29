maespa := maespa

# compiler
#NETCDFDEPS=-I/usr/local/include -L/usr/local/lib -lnetcdff 
#DIR1=/usr/local
#DIR2=/usr/local
#NETCDFDEPS=-I${DIR1}/include -L${DIR1}/lib -lnetcdff -lnetcdf -L${DIR2}/lib -lhdf5_hl -lhdf5 -lz -lcurl -lm

#NETCDFDEPS=-I/usr/include/ simple_xy_wr.f90 -L/usr/lib/  -lnetcdf -lnetcdff
NETCDFCDEPS=-I/usr/local/include/ 
NETCDFLDEPS=-L/usr/local/lib/ -lnetcdf -lnetcdff

FC      := /usr/bin/gfortran
#FCFLAGS = -g -c -fdefault-real-8 -fbacktrace -fno-align-commons -ftrace=full -fcheck=all -std=f95
FCFLAGS =$(NETCDFLDEPS) -g -c -fdefault-real-8 -fbacktrace -fno-align-commons -ftrace=full -fcheck=all  
##FLFLAGS = -g   -std=f95 
FLFLAGS =$(NETCDFCDEPS) -J maespa -g   -std=f2003 -Warray-bounds  -Waliasing -Wampersand -Wsurprising -Wintrinsics-std -Wno-tabs -Wintrinsic-shadow -Wline-truncation -Wunderflow -fbounds-check -ffree-line-length-none
#  -fmax-errors=4
PROGRAM = TUF3Dradiation

LIBS = 

INCLS = 

# source files and objects
#SRCS=switches.f90 maestcom.f90 maindeclarations.f90 MaespaConfigState.f90 MaespaConfigStateUtils.f90  metcom.f90 default_conditions.f90 physiol.f90 watbal.f90 unstor.f90 maespa_modules.f90 MaespaState.f90 MaespaResult.f90 Dyn_Array.f90 Maespa_Dyn_Array.f90 utils.f90  inout.f90  radn.f90 getmet.f90 TUFConstants.f90 for2r.f90 barray_cube.f90 ReadMaespaConfigs.f90 barray_cube_replacement.f90 crosspro.f90 intrinsics.f90 TUFreg3D_Radiation_Sectional.f90 dotpro.f90 
#SRCS=maestcom2.f90 ReadMaespaConfigs.f90 Dyn_Array.f90  maespa/maestcom.f90 maespa/utils.f90 maespa/switches.f90 maespa/maindeclarations.f90 maespa/inout.f90 maespa/radn.f90 maespa/physiol.f90 maespa/getmet.f90 maespa/default_conditions.f90 maespa/unstor.f90 maespa/watbal.f90 MaespaConfigState.f90 MaespaConfigStateUtils.f90  MaespaResult.f90 MaespaState.f90  maespa_modules.f90 TUFConstants.f90 barray_cube.f90 crosspro.f90 intrinsics.f90 TUFreg3D_Radiation_Sectional.f90 dotpro.f90  


#radiation only
#SRCS=Constants.f90 maestcom2.f90 ReadMaespaConfigs.f90 Dyn_Array.f90  maespa/maestcom.f90 maespa/utils.f90 maespa/switches.f90 maespa/maindeclarations.f90 maespa/inout.f90 maespa/radn.f90 maespa/physiol.f90 maespa/getmet.f90 maespa/default_conditions.f90 maespa/unstor.f90 maespa/watbal.f90 MaespaConfigState.f90 MaespaConfigStateUtils.f90  MaespaResult.f90 MaespaState.f90  maespa_modules.f90 TUFConstants.f90 barray_cube.f90 crosspro.f90 intrinsics.f90 ReverseRay.f90 TUFreg3D_Radiation_Sectional.f90 dotpro.f90  
#regular version
SRCS=MaespaConfigStateUtils.f90 Constants.f90 maestcom2.f90 MaespaConfigState.f90 ReadMaespaConfigs.f90 Dyn_Array.f90  maespa/maestcom.f90 maespa/utils.f90 maespa/switches.f90 maespa/maindeclarations.f90 maespa/inout.f90 maespa/radn.f90 maespa/physiol.f90 maespa/getmet.f90 maespa/default_conditions.f90 maespa/unstor.f90 maespa/watbal.f90    MaespaResult.f90 MaespaState.f90  maespa_modules.f90 TUFConstants.f90 barray_cube.f90 crosspro.f90 intrinsics.f90 ReverseRay.f90 TUFreg3D.f90 dotpro.f90  
  
#   
   
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