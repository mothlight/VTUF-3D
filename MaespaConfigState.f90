MODULE MaespaConfigState
   
    !USE maestcom, only : MAXSOILLAY, MAXT, MAXANG, MAXDATE, MAXP, MAXC, MAXHRS, MAXLAY, MAXMET, MAXSP, MAXHISTO
    use switches
    USE maindeclarations
        
    IMPLICIT NONE
    

    type maespaDataResults
        
        real transmissionPercentage
        real maespaWatQh
        real maespaWatQe
        real maespaWatQn
        real maespaWatQc
        real lai
        real maespaLe
        INTEGER x
        INTEGER y
        INTEGER z
        INTEGER f
        real timeis
        INTEGER yd_actual
        real maespaPar,maespaTcan
        real etInMM
        real leFromEt
        real leFromHrLe
        real qeCalc
        real qhCalc
        
        real watday
        real wathour
        real wsoil
        real wsoilroot
        real ppt
        real canopystore
        real evapstore
        real drainstore
        real tfall
        real et
        real etmeas
        real discharge
        real overflow
        real weightedswp
        real ktot
        real drythick
        real soilevap
        real soilmoist
        real fsoil
        real qh
        real qe
        real qn
        real qc
        real rglobund
        real rglobabv
        real radinterc
        real rnet
        real totlai
        real wattair
        real soilt1
        real soilt2
        real fracw1
        real fracw2
        real fracaPAR
        
        real DOY
        real Tree
        real Spec
        real HOUR
        real hrPAR
        real hrNIR
        real hrTHM
        real hrPs
        real hrRf
        real hrRmW
        real hrLE
        real LECAN
        real Gscan
        real Gbhcan
        real hrH
        real TCAN
        real ALMAX
        real PSIL
        real PSILMIN
        real CI
        real TAIR
        real VPD
        real PAR
        real ZEN
        real AZ
        
        
        !!uspar.dat
        real usparday 
        real usparhour 
        real usparpoint 
        real usparX 
        real usparY 
        real usparZ 
        real usparPARbeam 
        real usparPARdiffuse 
        real usparPARtotal 
        real usparAPAR 
        real usparhrPSus 
        real usparhrETus
        real leFromUspar
        
    end type maespaDataResults
    
    
    type maesapaTestDataResults
    
        real testDAY
        real testHR
        real testPT
        real testX
        real testY
        real testZ
        real testPAR
        real testFBEAM
        real testSUNLA
        real testTD
        real testTSCAT
        real testTTOT
        real testAPARSUN
        real testAPARSH
        real testAPAR
        
    end type maesapaTestDataResults
    
    type maespaArrayOfDataResults
        TYPE(maespaDataResults),allocatable,dimension(:) :: maespaOverallDataArray
    end type maespaArrayOfDataResults
    
    type maespaArrayOfTestDataResults
        TYPE(maesapaTestDataResults),allocatable,dimension(:) :: maespaOverallTestDataArray
    end type maespaArrayOfTestDataResults
    
    
    
!    type maespaConfigvariablesstate
!!! MAXLAY = max number of layers in crowns
!        
!        REAL WLEAFTABLE(maxdate)
!        INTEGER DATESWLEAF(maxdate)
!        INTEGER NOWLEAFDATES
!        REAL WLEAFTABLESPEC(maxdate,MAXSP)
!        INTEGER DATESWLEAFSPEC(maxdate,MAXSP)
!        INTEGER NOWLEAFDATESSPEC(MAXSP)
!        
!        INTEGER DATESLIA(maxdate,maxsp), NOLIADATES(maxsp),DATESLAD(maxdate,maxsp),NOLADDATES(maxsp)
!        REAL BPT(8,MAXC,MAXSP,maxdate)
!     
!     REAL TRUNK(maxdate,MAXT),FLT(maxdate,MAXT)
!     REAL R1(maxdate,MAXT),R2(maxdate,MAXT),R3(maxdate,MAXT)
!     REAL DIAMA(maxdate,MAXT)
!     REAL TU(MAXP)
!     INTEGER NUMTESTPNT
!        
!!        
!!    ! List of trees for which to do calculations
!    INTEGER ITARGETS(MAXT)
!    INTEGER ISPECIES(MAXT) !! species of each tree, array of ints -> species definitions in confile.dat
!    INTEGER ISPECIEST(MAXT)
!    INTEGER ISPECIESTUS(MAXT)
!!
!!    ! Tree positions and dimensions - all trees, all dates
!    REAL DXT1(MAXT)
!    REAL DYT1(MAXT)
!    REAL DZT1(MAXT)
!    REAL DX(MAXT)
!    REAL DY(MAXT)
!    REAL DZ(MAXT)
!    REAL RXTABLE1(maxdate,MAXT)
!    REAL RYTABLE1(maxdate,MAXT)
!    REAL RZTABLE1(maxdate,MAXT)
!    REAL ZBCTABLE1(maxdate,MAXT)
!    REAL FOLTABLE1(maxdate,MAXT)
!    REAL DIAMTABLE1(maxdate,MAXT)
!!    ! Tree positions and dimensions - sorted trees, all dates
!    REAL RXTABLE(maxdate,MAXT)
!    REAL RYTABLE(maxdate,MAXT)
!    REAL RZTABLE(maxdate,MAXT)
!    REAL ZBCTABLE(maxdate,MAXT)
!    REAL FOLTABLE(maxdate,MAXT)
!    REAL TOTLAITABLE(maxdate)  
!!    
!    REAL DIAMTABLE(maxdate,MAXT)
!    INTEGER IT(MAXT)
!!    ! Dates for tree dimensions
!    INTEGER DATESX(maxdate)
!    INTEGER DATESY(maxdate)
!    INTEGER DATESZ(maxdate)
!    INTEGER DATEST(maxdate)
!    INTEGER DATESLA(maxdate)
!    INTEGER DATESD(maxdate)
!!    ! Tree dimensions on simulation date (by interpolation)
!    REAL DXT(MAXT)
!    REAL DYT(MAXT)
!    REAL DZT(MAXT)
!    REAL RX(MAXT)
!    REAL RY(MAXT)
!    REAL RZ(MAXT)
!    REAL ZBC(MAXT)
!    REAL FOLT(MAXT)
!    REAL DIAM(MAXT)
!    REAL WEIGHTS(MAXT)
!    REAL CANOPYDIMS(6)
!!    ! Positions of grid points, associated volume & leaf area, etc
!    REAL XL(MAXP)
!    REAL YL(MAXP)
!    REAL ZL(MAXP)
!    REAL VL(MAXP)
!    REAL DLT(MAXP)
!    REAL DLI(MAXC,MAXP)
!    INTEGER LGP(MAXP)
!    INTEGER PPLAY !! number of points per layer (in tree crown)
!    REAL FOLLAY(MAXLAY)
!!    ! Met data
!    INTEGER SOILDATA
!    INTEGER USEMEASSW !! use measured soil water content
!    REAL TAIR(MAXHRS) !! air temperature - met file
!    REAL RADABV(MAXHRS,3)
!    REAL FBEAM(MAXHRS,3) !! fraction of incident PAR which is direct beam
!!
!!    ! Physiology inputs by layer
!    REAL ABSRP(MAXLAY,3)
!    REAL ARHO(MAXLAY,3)
!    REAL ATAU(MAXLAY,3)
!    REAL RHOSOL(3)
!    REAL JMAXTABLE(maxdate,MAXLAY,MAXC)
!    REAL VCMAXTABLE(maxdate,MAXLAY,MAXC)
!    REAL RDTABLE(maxdate,MAXLAY,MAXC)
!    REAL SLATABLE(maxdate,MAXLAY,MAXC)
!    REAL AJQTABLE(maxdate,MAXLAY,MAXC)
!    REAL Q10FTABLE(maxdate)
!    REAL Q10WTABLE(maxdate)
!    INTEGER DATESFQ(maxdate)
!    INTEGER DATESWQ(maxdate)
!    INTEGER DATESJ(maxdate)
!    INTEGER DATESV(maxdate)
!    INTEGER DATESRD(maxdate)
!    INTEGER DATESSLA(maxdate)
!    INTEGER DATESA(maxdate)
!    REAL JMAX25(MAXLAY,MAXC)
!    REAL VCMAX25(MAXLAY,MAXC)
!    REAL RD0(MAXLAY,MAXC)
!    REAL SLA(MAXLAY,MAXC)
!    REAL AJQ(MAXLAY,MAXC)
!    REAL DIFZEN(MAXANG)
!    REAL DEXT(MAXANG)
!    REAL ZEN(MAXHRS)
!    REAL AZ(MAXHRS)
!    REAL TD(MAXP)
!    REAL RELDF(MAXP)
!    REAL DIFDN(MAXP,3)
!    REAL DIFUP(MAXP,3)
!    REAL SCLOST(MAXP,3)
!    REAL DOWNTH(MAXP)
!    REAL SOILTEMP(MAXSOILLAY)
!
!!    ! Multi-species       
!    CHARACTER(30) SPECIESNAMES(MAXSP) !! array of names of species in stand
!    CHARACTER(30) PHYFILES(MAXSP) !! array of names of physiological input files
!    CHARACTER(30) STRFILES(MAXSP) !! array of structure input files (overrides str.dat)
!!
!!    ! STR arrays, multi-species versions.
!    REAL ALPHASPEC(MAXANG,MAXSP)
!    REAL FALPHASPEC(MAXANG,MAXSP)
!    REAL BPTSPEC(8,MAXC,MAXSP)
!    REAL BPTT(8,MAXC,MAXT)
!    REAL SHAPESPEC(MAXSP)
!    REAL EXTWINDSPEC(MAXSP)
!    REAL RANDOMSPEC(MAXSP)
!    REAL COEFFTSPEC(MAXSP)
!    REAL EXPONTSPEC(MAXSP)
!    REAL WINTERCSPEC(MAXSP)
!    REAL BCOEFFTSPEC(MAXSP)
!    REAL BEXPONTSPEC(MAXSP)
!    REAL BINTERCSPEC(MAXSP)
!    REAL DEXTSPEC(MAXSP,MAXANG)
!    REAL DEXTT(MAXT,MAXANG)
!    REAL BEXTSPEC(MAXSP)
!    REAL BEXTANGSPEC(MAXSP,MAXANG)
!    REAL BEXTT(MAXT)
!    REAL RCOEFFTSPEC(MAXSP)
!    REAL REXPONTSPEC(MAXSP)
!    REAL RINTERCSPEC(MAXSP)
!    REAL FRFRACSPEC(MAXSP)
!    INTEGER NOAGECSPEC(MAXSP)
!    INTEGER NOAGECT(MAXT)
!    INTEGER JLEAFSPEC(MAXSP)
!    INTEGER JLEAFT(MAXT)
!    INTEGER JSHAPESPEC(MAXSP)
!    INTEGER NALPHASPEC(MAXSP)
!    INTEGER JSHAPET(MAXT)
!    REAL SHAPET(MAXT)
!    REAL VPDMINSPEC(MAXSP)
!!
!!    ! PHY arrays, multi-species versions.
!    REAL ABSRPSPEC(MAXLAY,3,MAXSP)
!    REAL ARHOSPEC(MAXLAY,3,MAXSP)
!    REAL ATAUSPEC(MAXLAY,3,MAXSP)
!    REAL RHOSOLSPEC(3,MAXSP)
!    REAL PROPPSPEC(MAXC,MAXSP)
!    REAL PROPCSPEC(MAXC,MAXSP)
!    REAL PROPPT(MAXC,MAXT)
!    REAL PROPCT(MAXC,MAXT)
!    REAL LEAFNSPEC(maxdate,MAXLAY,MAXC,MAXSP)
!    REAL JMAXTABLESPEC(maxdate,MAXLAY,MAXC,MAXSP)
!    REAL VCMAXTABLESPEC(maxdate,MAXLAY,MAXC,MAXSP)
!    REAL RDTABLESPEC(maxdate,MAXLAY,MAXC,MAXSP)
!    REAL SLATABLESPEC(maxdate,MAXLAY,MAXC,MAXSP)
!    REAL AJQTABLESPEC(maxdate,MAXLAY,MAXC,MAXSP)
!    REAL Q10FTABLESPEC(maxdate,MAXSP)
!    REAL Q10WTABLESPEC(maxdate,MAXSP)
!    INTEGER DATESNSPEC(maxdate,MAXSP)
!    INTEGER DATESJSPEC(maxdate,MAXSP)
!    INTEGER DATESVSPEC(maxdate,MAXSP)
!    INTEGER DATESRDSPEC(maxdate,MAXSP)
!    INTEGER DATESSLASPEC(maxdate,MAXSP)
!    INTEGER DATESASPEC(maxdate,MAXSP)
!    INTEGER DATESFQSPEC(maxdate,MAXSP)
!    INTEGER DATESWQSPEC(maxdate,MAXSP)
!    INTEGER NOAGEPSPEC(MAXSP)
!    INTEGER NSIDESSPEC(MAXSP)
!    REAL GSREFSPEC(MAXSP)
!    REAL GSMINSPEC(MAXSP)
!    REAL PAR0SPEC(MAXSP)
!    REAL D0SPEC(MAXSP)
!    REAL VK1SPEC(MAXSP)
!    REAL VK2SPEC(MAXSP)
!    REAL VPD1SPEC(MAXSP)
!    REAL VPD2SPEC(MAXSP)
!    REAL VMFD0SPEC(MAXSP) 
!    REAL GSJASPEC(MAXSP)
!    REAL GSJBSPEC(MAXSP)
!    REAL T0SPEC(MAXSP)
!    REAL TREFSPEC(MAXSP)
!    REAL TMAXSPEC(MAXSP)
!    REAL SMD1SPEC(MAXSP)
!    REAL SMD2SPEC(MAXSP)
!    REAL WC1SPEC(MAXSP)
!    REAL WC2SPEC(MAXSP)
!    REAL SWPEXPSPEC(MAXSP)
!    REAL D0LSPEC(MAXSP)  
!    REAL GAMMASPEC(MAXSP)
!    REAL WLEAFSPEC(MAXSP)   
!    REAL SFSPEC(MAXSP)
!    REAL PSIVSPEC(MAXSP)
!!
!    INTEGER NOJDATESSPEC(MAXSP)
!    INTEGER NOVDATESSPEC(MAXSP)
!    INTEGER NOADATESSPEC(MAXSP)
!    INTEGER NOSLADATESSPEC(MAXSP)
!    INTEGER NORDATESSPEC(MAXSP)  
!    INTEGER NOWQDATESSPEC(MAXSP)
!    INTEGER NOFQDATESSPEC(MAXSP)
!    INTEGER IECOSPEC(MAXSP)
!    REAL EAVJSPEC(MAXSP)
!    REAL EDVJSPEC(MAXSP)
!    REAL DELSJSPEC(MAXSP)
!    REAL EAVCSPEC(MAXSP)
!    REAL EDVCSPEC(MAXSP)
!    REAL DELSCSPEC(MAXSP)
!    REAL TVJUPSPEC(MAXSP)
!    REAL TVJDNSPEC(MAXSP)
!    REAL THETASPEC(MAXSP)
!    REAL RTEMPSPEC(MAXSP)
!    REAL DAYRESPSPEC(MAXSP)
!    REAL EFFYRFSPEC(MAXSP)
!    REAL TBELOWSPEC(MAXSP)
!    REAL EFFYRWSPEC(MAXSP)
!    REAL RMWSPEC(MAXSP)
!    REAL RTEMPWSPEC(MAXSP)
!    REAL COLLASPEC(MAXSP)
!    REAL COLLKSPEC(MAXSP)
!    REAL STEMSDWSPEC(MAXSP)
!    REAL RMWAREASPEC(MAXSP)
!    REAL STEMFORMSPEC(MAXSP)
!    REAL Q10RSPEC(MAXSP)
!    REAL RTEMPRSPEC(MAXSP)
!    REAL Q10BSPEC(MAXSP)
!    REAL RTEMPBSPEC(MAXSP)
!    REAL RMCRSPEC(MAXSP)
!    REAL RMFRSPEC(MAXSP)
!    REAL RMBSPEC(MAXSP)
!    REAL K10FSPEC(MAXSP)
!    REAL G0TABLESPEC(maxdate,MAXSP)
!    REAL G1TABLESPEC(maxdate,MAXSP)
!    INTEGER NOGSDATESSPEC(MAXSP)
!    INTEGER DATESGSSPEC(maxdate,MAXSP)
!    INTEGER DATESGS(maxdate)
!    REAL G0TABLE(maxdate)
!    REAL G1TABLE(maxdate)
!    REAL APP
!    REAL BEAR
!    REAL BEXT
!    REAL BINSIZE !! size of classes in histogram
!    REAL BMULT
!    REAL CO2INC !! amount of increase in CO2, climate change scenario
!    REAL DAYL
!    REAL DEC
!    INTEGER KEEPZEN
!    REAL DIFSKY !! controls distribution of diffuse radiation incident from sky
!    REAL DMULT2
!    REAL DT1
!    REAL DT2
!    REAL DT3
!    REAL DT4
!    REAL EXPAN
!    REAL EXPTIME
!    REAL FBEAMOTC !! fractional reduction in beam factor of PAR (simulate growth in chamber)
!    REAL G0 !! stomatal conductance when PAR is zero (part of tuzet model)
!    REAL G1 !! slope parameter (part of tuzet model)
!    INTEGER ICC
!    INTEGER IDAY
!    INTEGER IEND
!    INTEGER IFLUSH
!    INTEGER IOTC
!    INTEGER IPROG
!    INTEGER IPROGUS
!    INTEGER ISIMUS
!    INTEGER ISPEC
!    INTEGER ISTART
!    INTEGER ITERMAX !! controls iterations in combined photosynthesis-transpiration model, 0=leaf temp assumed=air temp, >0 iterative method used, the number of iterations
!    INTEGER IUSTFILE
!    INTEGER MODELGS !! which model to calculate stomatal conductance, 2=Ball-Berry, 3=Ball-Berry-Leuning, 4=Ball-Berry-Opti, otherwise Jarvis model 
!    INTEGER MODELJM !! How JMAX and VCMAX parameters read in, 0=read from file, 1=calculate from leaf content
!    INTEGER MODELRD !! how RDO parameters read in, 0=read from file, 1=calculate from leaf content 
!    INTEGER MODELRW !! how wood respiration parameters read in
!    INTEGER MODELSS !! whether photosynthesis calculation done for sun/shade leaves separately
!    INTEGER NAZ !! number of azimuth angles calculated 
!    INTEGER NEWCANOPY
!    INTEGER NEWTUTD
!    INTEGER NOADATES
!    INTEGER NOAGEP
!    INTEGER NODDATES
!    INTEGER NOFQDATES
!    INTEGER NOGSDATES
!    INTEGER NOJDATES
!    INTEGER NOLADATES
!    INTEGER NOLAY !! number of layers in the crown
!    INTEGER NORDATES
!    INTEGER NOSLADATES
!    INTEGER NOTARGETS !! number of target trees
!    INTEGER NOTDATES
!    INTEGER NOTREES !! number of target trees, single target tree, see also itargets
!    INTEGER NOVDATES
!    INTEGER NOWQDATES
!    INTEGER NOXDATES
!    INTEGER NOYDATES
!    INTEGER NOZDATES
!    INTEGER NUMPNT
!    INTEGER NZEN !! number of zenith angles for which diffuse transmittances calculated
!    INTEGER NSTEP
!    REAL PAROTC !! reduction of incident PAR (simulate growth in chamber)
!    REAL PREVTSOIL
!    REAL Q10W
!    REAL Q10F
!    REAL SHADEHT !! height of external shading of plot
!    REAL SOMULT
!    REAL STOCKING
!    REAL SUNLA
!    REAL SWMAX !! maximum soil water content
!    REAL SWMIN !! minimum soil water content
!    REAL TINC !! amount of increase in air temperature, climate change scenario
!    REAL TOTC !! increase of air temp (simulate growth in chamber)
!    CHARACTER(len=256) :: VTITLE !! program title and version
!    CHARACTER(len=256) :: in_path
!    CHARACTER(len=256) :: out_path
!    INTEGER NSPECIES !! number of species in the stand
!    REAL WINDOTC !! absolute wind speed (simulate growth in chamber)
!    REAL XSLOPE !! slopes of tree plots in degrees
!    REAL YSLOPE !! slopes of tree plots in degrees
!    REAL X0 !! offset values of tree locations
!    REAL Y0 !! offset values of tree locations
!    REAL XMAX !! max x coordinate of tree locations
!    REAL YMAX !! max y coordinate of tree locations
!    REAL ZHT !! roughness measurement height (m)
!    REAL Z0HT !! roughness length (m)
!    REAL ZPD !! (roughness) zero plane displacement (m)
!    REAL TOTLAI
!    REAL WLEAF !! effective leaf width (part of tuzet model)
!    REAL G0SPEC(MAXSP),G1SPEC(MAXSP)
!    REAL GKSPEC(MAXSP)
!    REAL XLP(MAXP)
!    REAL YLP(MAXP)
!    REAL ZLP(MAXP)
!    REAL VPARASPEC(MAXSP)
!    REAL VPARBSPEC(MAXSP)
!    REAL VPARCSPEC(MAXSP)
!
!    INTEGER VFUNSPEC(MAXSP)
!         
!    INTEGER IOHRLY    ! Controls daily, hourly, and/or layer outp
!    INTEGER IOTUTD    ! Controls transmittance file output
!    INTEGER IOHIST    ! Controls histogram output
!    INTEGER IORESP    ! Controls respiration output
!    INTEGER IODAILY   ! Controls daily output: FIXED HERE 
!    INTEGER IOWATBAL  ! Controls water balance output
!    INTEGER IOFORMAT  ! Dump mode...
!    
!    integer ISUNLA
!    REAL PLOTAREA
!    INTEGER NOALLTREES  
!end type maespaConfigvariablesstate

    
type maespaConfigTreeMapState
        INTEGER numberTreePlots
        INTEGER, DIMENSION(:), ALLOCATABLE :: xlocation
        INTEGER, DIMENSION(:), ALLOCATABLE :: ylocation
        INTEGER, DIMENSION(:), ALLOCATABLE :: phyfileNumber
        INTEGER, DIMENSION(:), ALLOCATABLE :: strfileNumber
        INTEGER, DIMENSION(:), ALLOCATABLE :: treesfileNumber
        INTEGER, DIMENSION(:), ALLOCATABLE :: treesHeight
        INTEGER, DIMENSION(:), ALLOCATABLE :: trees
        
        INTEGER numberBuildingPlots
        INTEGER, DIMENSION(:), ALLOCATABLE :: xBuildingLocation
        INTEGER, DIMENSION(:), ALLOCATABLE :: yBuildingLocation
        INTEGER, DIMENSION(:), ALLOCATABLE :: buildingsHeight
        
        INTEGER width
        INTEGER length
        
        INTEGER configTreeMapCentralArrayLength
        INTEGER configTreeMapCentralWidth
        INTEGER configTreeMapCentralLength
        INTEGER configTreeMapX
        INTEGER configTreeMapY
        INTEGER configTreeMapX1
        INTEGER configTreeMapX2
        INTEGER configTreeMapY1
        INTEGER configTreeMapY2
        INTEGER configTreeMapNumsfcab
        REAL configTreeMapGridSize
        INTEGER configTreeMapHighestBuildingHeight
        
end type maespaConfigTreeMapState
    

 
    
END MODULE MaespaConfigState
