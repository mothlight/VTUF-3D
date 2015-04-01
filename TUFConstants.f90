module TUFConstants

!     
! File:   TUFConstants.f90
! Author: kerryn
!
! Created on 26 February 2014, 12:02 PM
    
      !! write to file lengths (halve these if you are compiling on 32 bit)
!      integer vfinfoDatRECL 
!      integer vfRECL
    
    !! these are for g95 - 64 bit
      INTEGER, PARAMETER ::  vfinfoDatRECL=64
      INTEGER, PARAMETER ::  vfRECL = 16
      INTEGER, PARAMETER ::  REAL8LEN = 8
    
!      INTEGER, PARAMETER ::  vfinfoDatRECL=32
!      INTEGER, PARAMETER ::  vfRECL = 8

    !file constants
     INTEGER, PARAMETER ::  parametersRadiationDat = 299
     INTEGER, PARAMETER ::  inputsStoreOut = 802
     INTEGER, PARAMETER ::  forcingRadiationDat = 981
     INTEGER, PARAMETER ::  vfinfoDat = 991
     INTEGER, PARAMETER ::  EnergyBalanceOverallOut = 837
     INTEGER, PARAMETER ::  RadiationBalanceFacetsOut = 847
     INTEGER, PARAMETER ::  RadiationBalanceSectionalOut = 857
     INTEGER, PARAMETER ::  RadiationBalanceSectionalDayAvgOut = 867
     INTEGER, PARAMETER ::  RadiationBalance_SectionalDayStrorAvgOut = 877
     INTEGER, PARAMETER ::  VerticesToMatlabOut = 92
     INTEGER, PARAMETER ::  FacesToMatlabOut = 95
     INTEGER, PARAMETER ::  ToMatlabSVFYdOut = 97     
     INTEGER, PARAMETER ::  ToMatlabTbrightYdOut = 98
     INTEGER, PARAMETER ::  ToMatlabKabsYdOut = 99
     INTEGER, PARAMETER ::  ToMatlabKreflYdOut= 96
     INTEGER, PARAMETER ::  vf1Dat= 228
     
     
     INTEGER, PARAMETER ::  faceup=1
     INTEGER, PARAMETER ::  facenorth=2
     INTEGER, PARAMETER ::  faceeast=3
     INTEGER, PARAMETER ::  facesouth=4
     INTEGER, PARAMETER ::  facewest=5
     INTEGER, PARAMETER ::  facevegetation = 6
     INTEGER, PARAMETER ::  sfc_ab_i=1
     INTEGER, PARAMETER ::  sfc_ab_f=2
     INTEGER, PARAMETER ::  sfc_ab_z=3
     INTEGER, PARAMETER ::  sfc_ab_y=4
     INTEGER, PARAMETER ::  sfc_ab_x=5
     INTEGER, PARAMETER ::  sfc_ab_layer_temp=6
     
     INTEGER, PARAMETER :: sfc_surface_type=1
     INTEGER, PARAMETER :: sfc_sunlight_fact=2
     !INTEGER, PARAMETER :: sfc_albedo=3
     !INTEGER, PARAMETER :: sfc_emiss=4
     INTEGER, PARAMETER :: sfc_evf=5
     INTEGER, PARAMETER :: sfc_x_vector=6
     INTEGER, PARAMETER :: sfc_y_vector=7
     INTEGER, PARAMETER :: sfc_z_vector=8
     INTEGER, PARAMETER :: sfc_in_array=9
     INTEGER, PARAMETER :: sfc_x_value_patch_center=10
     INTEGER, PARAMETER :: sfc_y_value_patch_center=11
     INTEGER, PARAMETER :: sfc_z_value_patch_center=12
      
     INTEGER, PARAMETER ::  sfc_type=1
     INTEGER, PARAMETER ::  sfc_sunlitfact=2
     INTEGER, PARAMETER ::  sfc_albedo=3
     INTEGER, PARAMETER ::  sfc_emiss=4
     INTEGER, PARAMETER ::  sfc_svf=5
     INTEGER, PARAMETER ::  sfc_x_vect=6
     INTEGER, PARAMETER ::  sfc_y_vect=7
     INTEGER, PARAMETER ::  sfc_z_vect=8
     INTEGER, PARAMETER ::  sfc_in_init_array=9
     INTEGER, PARAMETER ::  sfc_x=10
     INTEGER, PARAMETER ::  sfc_y=11
     INTEGER, PARAMETER ::  sfc_z=12
     INTEGER, PARAMETER ::  not_in_array=0
     INTEGER, PARAMETER ::  in_initial_input=1
     INTEGER, PARAMETER ::  in_area_of_interest=2
     INTEGER, PARAMETER ::  roof =1
     INTEGER, PARAMETER ::  street=2
     INTEGER, PARAMETER ::  wall=3
     
     INTEGER, PARAMETER ::  vfcalc_false=0
     INTEGER, PARAMETER ::  vfcalc_true=1
     
     INTEGER, PARAMETER :: beginF =1
     INTEGER, PARAMETER :: endF =6
     
     INTEGER, PARAMETER :: testDAYCONST=1
     INTEGER, PARAMETER :: testHRCONST=2
     INTEGER, PARAMETER :: testPTCONST=3
     INTEGER, PARAMETER :: testXCONST=4
     INTEGER, PARAMETER :: testYCONST=5
     INTEGER, PARAMETER :: testZCONST=6
     INTEGER, PARAMETER :: testPARCONST=7
     INTEGER, PARAMETER :: testFBEAMCONST=8
     INTEGER, PARAMETER :: testSUNLACONST=9
     INTEGER, PARAMETER :: testTDCONST=10
     INTEGER, PARAMETER :: testTSCATCONST=11
     INTEGER, PARAMETER :: testTTOTCONST=12
     INTEGER, PARAMETER :: testAPARSUNCONST=13
     INTEGER, PARAMETER :: testAPARSHCONST=14
     INTEGER, PARAMETER :: testAPARCONST=15
     
    
END module TUFConstants