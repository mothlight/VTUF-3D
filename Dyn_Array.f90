module dyn_array
    use MaespaState
    use MaespaResult
    use MaespaConfigState, only : maespaConfigTreeMapState,maespaDataResults,maespaArrayOfDataResults,maespaArrayOfTestDataResults


! Code converted using TO_F90 by Alan Miller
! Date: 2012-04-27  Time: 07:31:34

! allocatable arrays defined here so they can be passed between modules

INTEGER, DIMENSION(:,:), ALLOCATABLE :: bldht
LOGICAL, DIMENSION(:,:,:), ALLOCATABLE :: surf_shade
LOGICAL, DIMENSION(:,:,:,:), ALLOCATABLE :: surf
INTEGER, DIMENSION(:,:), ALLOCATABLE :: bldhti
REAL, DIMENSION(:,:), ALLOCATABLE :: sfc
INTEGER, DIMENSION(:), ALLOCATABLE :: ind_ab
REAL, DIMENSION(:,:), ALLOCATABLE :: sfc_ab
INTEGER, DIMENSION(:), ALLOCATABLE :: vffile
INTEGER, DIMENSION(:), ALLOCATABLE :: vfppos
INTEGER, DIMENSION(:), ALLOCATABLE :: vfipos
INTEGER, DIMENSION(:), ALLOCATABLE :: mend
REAL, DIMENSION(:), ALLOCATABLE :: refl_emist
REAL, DIMENSION(:), ALLOCATABLE :: absbs
REAL, DIMENSION(:), ALLOCATABLE :: absbl
REAL, DIMENSION(:), ALLOCATABLE :: tots
REAL, DIMENSION(:), ALLOCATABLE :: totl
REAL, DIMENSION(:), ALLOCATABLE :: refls
REAL, DIMENSION(:), ALLOCATABLE :: refll
REAL, DIMENSION(:), ALLOCATABLE :: reflts
REAL, DIMENSION(:), ALLOCATABLE :: refltl
REAL, DIMENSION(:), ALLOCATABLE :: reflps
REAL, DIMENSION(:), ALLOCATABLE :: reflpl
REAL, DIMENSION(:), ALLOCATABLE :: vf2
REAL, DIMENSION(:), ALLOCATABLE :: vf3
INTEGER, DIMENSION(:), ALLOCATABLE :: vf3j
INTEGER, DIMENSION(:), ALLOCATABLE :: vf2j
REAL, DIMENSION(:,:), ALLOCATABLE :: vertex
INTEGER, DIMENSION(:,:), ALLOCATABLE :: face
REAL, DIMENSION(:), ALLOCATABLE :: lambda
REAL, DIMENSION(:), ALLOCATABLE :: lambdaav
REAL, DIMENSION(:), ALLOCATABLE :: htcap
REAL, DIMENSION(:), ALLOCATABLE :: thick
REAL, DIMENSION(:), ALLOCATABLE :: tlayer
REAL, DIMENSION(:), ALLOCATABLE :: tlayerp
REAL, DIMENSION(:), ALLOCATABLE :: gam
REAL, DIMENSION(:), ALLOCATABLE :: denom
REAL, DIMENSION(:), ALLOCATABLE :: a
REAL, DIMENSION(:), ALLOCATABLE :: b
REAL, DIMENSION(:), ALLOCATABLE :: d
REAL, DIMENSION(:), ALLOCATABLE :: r
REAL, DIMENSION(:), ALLOCATABLE :: lambda_sfc
REAL, DIMENSION(:), ALLOCATABLE :: tsfc
REAL, DIMENSION(:), ALLOCATABLE :: trad
REAL, DIMENSION(:), ALLOCATABLE :: pressfrc !! (mb) air pressure near the surface
REAL, DIMENSION(:), ALLOCATABLE :: udirfrc !! direction from which the wind is blowing at 'zref', in degrees from north. The UDIRFRC(k) are only important for calculating lambdaf (frontal area index) and z0 - for momentum and heat exchange. Thus, if you prescribe these values in parameters.dat, the values for Udirfrc will have no impact on the simulation.
REAL, DIMENSION(:), ALLOCATABLE :: kdnfrc !!  (W/m2) downwelling shortwave radiation flux density; if Kdnfrc(1)<-90.0 then the model will use its internal solar model to calculate downwelling shortwave (Iqbal/Stull)
REAL, DIMENSION(:), ALLOCATABLE :: ldnfrc !! (W/m2) downwelling longwave radiation flux density; if Ldnfrc(1)<0.0 then the model will use its internal longwave model to calculate downwelling shortwave (Prata)
REAL, DIMENSION(:), ALLOCATABLE :: tafrc !! (deg C) air temperature at 'zref' (see input file - .dat)
REAL, DIMENSION(:), ALLOCATABLE :: eafrc !! (mb) water vapour pressure at 'zref'
REAL, DIMENSION(:), ALLOCATABLE :: uafrc !! (m/s) wind SPEED (not velocity) at 'zref'
REAL, DIMENSION(:), ALLOCATABLE :: timefrc
!REAL, DIMENSION(:), ALLOCATABLE :: qh !!KN, darn, looks like this is never used
!REAL, DIMENSION(:), ALLOCATABLE :: qe  !! KN- add array for latent energy
!INTEGER, DIMENSION(:), ALLOCATABLE :: Maespa_surface !! KN- add array to contain mapespa surfaces
!TYPE(maespavariablesstate), DIMENSION(:), ALLOCATABLE :: Maespa_surface_states !! KN- add array to contain mapespa surfaces-their stored states
!INTEGER, DIMENSION(:), ALLOCATABLE :: MaespaCalculationProgress !! KN store which hours have been calculated and stored in Maespa_surface_states
REAL, DIMENSION(:), ALLOCATABLE :: lambdar !! thermal conductivity (W/m/K) of roof layer k, where k=1 is the surface layer, and k=numlayers is the deepest layer
REAL, DIMENSION(:), ALLOCATABLE :: lambdaavr
REAL, DIMENSION(:), ALLOCATABLE :: htcapr !! volumetric heat capacity (J/m3/K) of roof layer k, where k=1 is the surface layer, and k=numlayers is the deepest layer
REAL, DIMENSION(:), ALLOCATABLE :: thickr !! thickness (m) of roof layer k, where k=1 is the surface layer, and k=numlayers is the deepest layer
REAL, DIMENSION(:), ALLOCATABLE :: depthr
REAL, DIMENSION(:), ALLOCATABLE :: lambdas !! thermal conductivity (W/m/K) of street layer k, where k=1 is the surface layer, and k=numlayers is the deepest layer
REAL, DIMENSION(:), ALLOCATABLE :: lambdaavs
REAL, DIMENSION(:), ALLOCATABLE :: htcaps  !! volumetric heat capacity (J/m3/K) of street layer k, where k=1 is the surface layer, and k=numlayers is the deepest layer
REAL, DIMENSION(:), ALLOCATABLE :: thicks !! thickness (m) of street layer k, where k=1 is the surface layer, and k=numlayers is the deepest layer
REAL, DIMENSION(:), ALLOCATABLE :: depths
REAL, DIMENSION(:), ALLOCATABLE :: lambdaw !! thermal conductivity (W/m/K) of wall layer k, where k=1 is the surface layer, and k=numlayers is the deepest layer
REAL, DIMENSION(:), ALLOCATABLE :: lambdaavw
REAL, DIMENSION(:), ALLOCATABLE :: htcapw !! volumetric heat capacity (J/m3/K) of wall layer k, where k=1 is the surface layer, and k=numlayers is the deepest layer
REAL, DIMENSION(:), ALLOCATABLE :: thickw !! thickness (m) of wall layer k, where k=1 is the surface layer, and k=numlayers is the deepest layer
REAL, DIMENSION(:), ALLOCATABLE :: depthw
REAL, DIMENSION(:), ALLOCATABLE :: uwrite
REAL, DIMENSION(:), ALLOCATABLE :: twrite
REAL, DIMENSION(:), ALLOCATABLE :: lpin !! the lambdap ratios (from k=1 to k=numlp)
REAL, DIMENSION(:), ALLOCATABLE :: bh_o_bl !! the bh/bl ratios (from l=1 to l=numbhbl)


REAL, DIMENSION(:,:), ALLOCATABLE :: sdT
REAL, DIMENSION(:,:), ALLOCATABLE :: sdN
REAL, DIMENSION(:,:), ALLOCATABLE :: sdS
REAL, DIMENSION(:,:), ALLOCATABLE :: sdE
REAL, DIMENSION(:,:), ALLOCATABLE :: sdW

!REAL, DIMENSION(:), ALLOCATABLE :: Timeis_for_maespa
!INTEGER, DIMENSION(:), ALLOCATABLE :: yd_for_maespa
INTEGER, DIMENSION(:), ALLOCATABLE :: bh_aw2_al2_a1_a2_b1_b2
TYPE(maesparesulttype) :: MaespaResultPointer !! KN- add dynamic variable to pass the results back 
TYPE(maesparesulttype), DIMENSION(:), ALLOCATABLE :: MaespaResults !! KN- store calculated results for each Maespa surface

      !! changing this for differential tree shading, adding second dimension, 1=diffuse only, 2=50% radiation, 3=100% shading
      TYPE(maespaArrayOfDataResults),allocatable,dimension(:,:) :: maespaDataArray
      TYPE(maespaArrayOfTestDataResults),allocatable,dimension(:) :: maespaTestDataArray
      integer,allocatable,dimension(:,:) :: treeXYMap
      integer,allocatable,dimension(:,:) :: treeXYTreeMap
      !! new variable to keep track of how much the vertical tree is receiving sun to properly allocatate differential shading
      !real,allocatable,dimension(:) :: treesSunlightPercentageTotal
      !real,allocatable,dimension(:) :: treesSunlightPercentagePoints
      
      real,allocatable,dimension(:,:) :: treeXYMapSunlightPercentageTotal
      !integer,allocatable,dimension(:,:) :: treeXYMapSunlightPercentagePoints

END module dyn_array
