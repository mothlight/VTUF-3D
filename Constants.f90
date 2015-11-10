module constants

! Code converted using TO_F90 by Alan Miller
! Date: 2012-04-27  Time: 07:31:23

INTEGER, PARAMETER :: parameters_dat_file=299
INTEGER, PARAMETER :: inputs_store_out=802
INTEGER, PARAMETER :: forcing_dat=843
INTEGER, PARAMETER :: energybalancetsfctimeaverage_out=832
INTEGER, PARAMETER :: tsfcfacetssunshade_out=833
INTEGER, PARAMETER :: tsfcfacets_out=835
INTEGER, PARAMETER :: energybalancefacets_out=836
INTEGER, PARAMETER :: energybalanceoverall_out=837
INTEGER, PARAMETER :: forcing_out=843
!INTEGER :: forcing_out
INTEGER, PARAMETER :: radiationbalancefacets_out=847
INTEGER, PARAMETER :: TsfcSolarSVF_Patch_yd=87 
!INTEGER, PARAMETER :: FORCING_DAT=981
INTEGER, PARAMETER :: VFINFO_DAT=991
!INTEGER :: vfinfo_dat
!INTEGER :: tsfcstreet_out
!INTEGER :: tsfcnwall_out
!INTEGER :: tsfcswall_out
!INTEGER :: tsfcewall_out
!INTEGER :: tsfcwwall_out
!
!INTEGER, PARAMETER  :: vertices_toMatlab_out = 92
!INTEGER, PARAMETER  :: faces_toMatlab_out = 95
!INTEGER, PARAMETER  :: toMatlab_Tsfc_yd_out = 97
!INTEGER, PARAMETER  :: toMatlab_Tbright_yd_out = 98
!INTEGER, PARAMETER  :: toMatlab_Kabs_yd_out = 99
!INTEGER, PARAMETER  :: toMatlab_Labs_yd_out = 499
!INTEGER, PARAMETER  :: toMatlab_Krefl_yd_out = 96
!INTEGER, PARAMETER  :: toMatlab_Lrefl_yd_out = 496
!INTEGER, PARAMETER  :: Tsfc_yd_out = 197
!INTEGER, PARAMETER  :: Tbright_yd_out = 198
!INTEGER :: vf_FILE_OUT ! 228
!
!INTEGER :: up_face
!INTEGER :: north_face
!INTEGER :: east_face
!INTEGER :: south_face
!INTEGER :: west_face

REAL, PARAMETER :: roof_surface=1
REAL, PARAMETER :: street_surface=2
REAL, PARAMETER :: wall_surface=3

REAL, PARAMETER :: not_in_init_array=0
REAL, PARAMETER :: in_init_input_array=1
REAL, PARAMETER :: in_area_of_interest=2

!INTEGER, PARAMETER :: sfc_surface_type=1
!INTEGER, PARAMETER :: sfc_sunlight_fact=2
!INTEGER, PARAMETER :: sfc_albedo=3
!INTEGER, PARAMETER :: sfc_emiss=4
!INTEGER, PARAMETER :: sfc_evf=5
!INTEGER, PARAMETER :: sfc_x_vector=6
!INTEGER, PARAMETER :: sfc_y_vector=7
!INTEGER, PARAMETER :: sfc_z_vector=8
!INTEGER, PARAMETER :: sfc_in_array=9
!INTEGER, PARAMETER :: sfc_x_value_patch_center=10
!INTEGER, PARAMETER :: sfc_y_value_patch_center=11
!INTEGER, PARAMETER :: sfc_z_value_patch_center=12

!INTEGER, PARAMETER :: sfc_ab_i=1
!INTEGER, PARAMETER :: sfc_ab_f=2
!INTEGER, PARAMETER :: sfc_ab_z=3
!INTEGER, PARAMETER :: sfc_ab_y=4
!INTEGER, PARAMETER :: sfc_ab_x=5


END module constants
