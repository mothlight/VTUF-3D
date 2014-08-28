# compiler
 

FC      := /usr/bin/gfortran
#FCFLAGS = -g -c -fdefault-real-8 -fbacktrace -fno-align-commons -ftrace=full -fcheck=all -std=f95
FCFLAGS = -g -c -fdefault-real-8 -fbacktrace -fno-align-commons -ftrace=full -fcheck=all  
##FLFLAGS = -g   -std=f95 
FLFLAGS = -g   -std=f2003 -Warray-bounds  -Waliasing -Wampersand -Wsurprising -Wintrinsics-std -Wno-tabs -Wintrinsic-shadow -Wline-truncation -Wunderflow -fbounds-check
PROGRAM = TUF3Dradiation


##FC = /opt/g95/bin/x86_64-unknown-linux-gnu-g95
##FCFLAGS = -g -c -fdefault-real-8 -fbacktrace -fno-align-commons -fbounds-check -ftrace=full 
##FLFLAGS = -g -fbounds-check -ftrace=full 
#PROGRAM = TUF3DMaespa-g95

#FC = f95
#FCFLAGS = -g -c -fdefault-real-8 -fbacktrace -fno-align-commons -fbounds-check 
#FLFLAGS = -g -fbounds-check 
#PROGRAM = TUF3DMaespa-f95

#FC = /usr/bin/ifort
#FCFLAGS = -g -c -fdefault-real-8 -fbacktrace -fno-align-commons -fbounds-check -ftrace=full 
#FLFLAGS = -g   


    


# compile flags

# link flags
#FLFLAGS = -g -fbounds-check -Warray-bounds


# source files and objects
SRCS=switches.f90 maestcom.f90 maindeclarations.f90 MaespaConfigState.f90 MaespaConfigStateUtils.f90  metcom.f90 default_conditions.f90 physiol.f90 watbal.f90 unstor.f90 maespa_modules.f90 MaespaState.f90 MaespaResult.f90 Dyn_Array.f90 Maespa_Dyn_Array.f90 utils.f90  inout.f90  radn.f90 getmet.f90 TUFConstants.f90 for2r.f90 barray_cube.f90 ReadMaespaConfigs.f90 barray_cube_replacement.f90 crosspro.f90 intrinsics.f90 TUFreg3D_Radiation_Sectional.f90 dotpro.f90 
   
######SRCS=  maestcom.f90 MaespaState.f90 MaespaResult.f90 for2r.f90 Dyn_Array.f90 Constants.f90 per.f90 pll.f90 f2.f90 f3.f90 f4.f90 f5.f90 f7.f90 f8.f90 f9.f90 ray.f90 clrsky.f90  Util_functions.f90 TUF_util.f90 Init_functions.f90   barray_cube.f90  crosspro.f90  dotpro.f90  intrinsics.f90  switches.f90 metcom.f90  maindeclarations.f90 getmet.f90 maespa_modules.f90 utils.f90 MaespaSingleTreeSingleLoop.F90 TUFreg3D.f90  tufinit.f90   default_conditions.f90   radn.f90 inout.f90  unstor.f90  physiol.f90 watbal.f90 
#SRCS = $(patsubst %.f90, %.o, $(wildcard *.f90))
#       $(patsubst %.h, %.mod, $(wildcard *.h))

# program name
#PROGRAM = TUFreg3D


all: $(PROGRAM)

$(PROGRAM): $(SRCS)
	$(FC) $(FLFLAGS) -o $@ $^

%.o: %.f90
	$(FC) $(FCFLAGS) -o $@ $<

clean:
	rm -f *.o *.mod $(PROGRAM)


