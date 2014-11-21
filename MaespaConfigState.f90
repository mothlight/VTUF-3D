MODULE MaespaConfigState
   
    USE maestcom, only : MAXSOILLAY, MAXT, MAXANG, MAXDATE, MAXP, MAXC, MAXHRS, MAXLAY, MAXMET, MAXSP, MAXHISTO
    use switches
    USE maindeclarations
        
    IMPLICIT NONE
    
    
    
    type maespaConfigvariablesstate
!! MAXLAY = max number of layers in crowns
        
        
        INTEGER DATESLIA(maxdate,maxsp), NOLIADATES(maxsp),DATESLAD(maxdate,maxsp),NOLADDATES(maxsp)
        REAL BPT(8,MAXC,MAXSP,maxdate)
     
     REAL TRUNK(maxdate,MAXT),FLT(maxdate,MAXT)
     REAL R1(maxdate,MAXT),R2(maxdate,MAXT),R3(maxdate,MAXT)
     REAL DIAMA(maxdate,MAXT)
        
!        
!    ! List of trees for which to do calculations
    INTEGER ITARGETS(MAXT)
    INTEGER ISPECIES(MAXT) !! species of each tree, array of ints -> species definitions in confile.dat
    INTEGER ISPECIEST(MAXT)
    INTEGER ISPECIESTUS(MAXT)
!    INTEGER J
!    INTEGER USESTAND !! if =0, use plot dimensions to scale up stand water use
!
!    ! Tree positions and dimensions - all trees, all dates
    REAL DXT1(MAXT)
    REAL DYT1(MAXT)
    REAL DZT1(MAXT)
    REAL DX(MAXT)
    REAL DY(MAXT)
    REAL DZ(MAXT)
    REAL RXTABLE1(maxdate,MAXT)
    REAL RYTABLE1(maxdate,MAXT)
    REAL RZTABLE1(maxdate,MAXT)
    REAL ZBCTABLE1(maxdate,MAXT)
    REAL FOLTABLE1(maxdate,MAXT)
    REAL DIAMTABLE1(maxdate,MAXT)
!    ! Tree positions and dimensions - sorted trees, all dates
!    REAL RXTABLE(maxdate,MAXT)
!    REAL RYTABLE(maxdate,MAXT)
!    REAL RZTABLE(maxdate,MAXT)
!    REAL ZBCTABLE(maxdate,MAXT)
!    REAL FOLTABLE(maxdate,MAXT)
    REAL TOTLAITABLE(maxdate)  
!    
    REAL DIAMTABLE(maxdate,MAXT)
    INTEGER IT(MAXT)
!    INTEGER ITUUS(MAXT) ! Sorted tree numbers.
!    ! Dates for tree dimensions
    INTEGER DATESX(maxdate)
    INTEGER DATESY(maxdate)
    INTEGER DATESZ(maxdate)
    INTEGER DATEST(maxdate)
    INTEGER DATESLA(maxdate)
    INTEGER DATESD(maxdate)
!    ! Tree dimensions on simulation date (by interpolation)
    REAL DXT(MAXT)
    REAL DYT(MAXT)
    REAL DZT(MAXT)
    REAL RX(MAXT)
    REAL RY(MAXT)
    REAL RZ(MAXT)
    REAL ZBC(MAXT)
    REAL FOLT(MAXT)
!    REAL DIAM(MAXT)
!    REAL EXPFACTORS(MAXT)
    REAL WEIGHTS(MAXT)
!    REAL CANOPYDIMS(6)
!    ! Positions of grid points, associated volume & leaf area, etc
    REAL XL(MAXP)
    REAL YL(MAXP)
    REAL ZL(MAXP)
    REAL VL(MAXP)
    REAL DLT(MAXP)
    REAL DLI(MAXC,MAXP)
!    REAL XL2(MAXP)
!    REAL YL2(MAXP)
!    REAL ZL2(MAXP)
!    REAL VL2(MAXP)
!    
    INTEGER LGP(MAXP)
!    INTEGER LAYER(MAXP)
!    INTEGER MLAYER(MAXP)
    INTEGER PPLAY !! number of points per layer (in tree crown)
    REAL FOLLAY(MAXLAY)
!    REAL WINDLAY(MAXLAY)
!    REAL LGP2(MAXP)
!    REAL FOLLAY2(MAXLAY)
!      
!    ! Understorey arrays.
!    REAL XLU(MAXP)
!    REAL YLU(MAXP)
!    REAL ZLU(MAXP) ! Understorey points.
!    INTEGER LAYERUS(MAXP)
!    INTEGER MLAYERUS(MAXP) ! not implemented yet
!    REAL UIBEAM(MAXHRS,MAXP)
!    REAL UIDIFF(MAXHRS,MAXP)
!    REAL PARUS(MAXHRS,MAXP)
!    REAL APARUS(MAXHRS,MAXP)
!    REAL PSUS(MAXHRS,MAXP)
!    REAL ETUS(MAXHRS,MAXP)
!    REAL PARUNDER(MAXHRS,MAXP)
!    REAL USLAITAB(maxdate,MAXT)
!    REAL USLAI(MAXP)
!    REAL HTUS(maxdate,MAXT)
!    REAL FOLNUS(maxdate,MAXT)
!    INTEGER DATESFU(maxdate)
!    INTEGER DATESHU(maxdate)
!    INTEGER DATESNU(maxdate)
!    REAL JMAXN25
!    REAL JMAX25M
!    REAL RESCALE
!    REAL FUS(MAXP)
!    REAL AREAUS(MAXP)
!    REAL FN0US(MAXP)
!    REAL DXTUS(MAXT)
!    REAL DYTUS(MAXT)
!    REAL DZTUS(MAXT)
!    REAL RXTABLEUS(maxdate,MAXT)
!    REAL RYTABLEUS(maxdate,MAXT)
!    REAL RZTABLEUS(maxdate,MAXT)
!    REAL FOLTABLEUS(maxdate,MAXT)
!    REAL ZBCTABLEUS(maxdate,MAXT)
!    REAL DIAMTABLEUS(maxdate,MAXT)
!    REAL RXUS(MAXT)
!    REAL RYUS(MAXT)
!    REAL RZUS(MAXT)
!    REAL ZBCUS(MAXT)
!    REAL FOLTUS(MAXT)
!    REAL PARUSMEAN(MAXHRS)
!    REAL PARUSSD(MAXHRS)
!    REAL THRABUS(MAXHRS)
!    REAL FCO2US(MAXHRS)
!    REAL FH2OUS(MAXHRS)
!
!    ! Met data
!    INTEGER METCOLS(MAXMET)
    INTEGER SOILDATA
!    INTEGER TSOILDATA
!    INTEGER REASSIGNRAIN !! if =1, hourly precip from met.dat reassigned more realistic values
!    INTEGER SIMTSOIL !! if =0, soil temperature will not be simulated
!    INTEGER WSOILMETHOD
!    INTEGER RETFUNCTION !! which soil water retention curve used
!    INTEGER USEMEASET !! calculate canopy transpiration (or simulate)
    INTEGER USEMEASSW !! use measured soil water content
!    REAL WINDAH(MAXHRS)
!    REAL TSOIL(MAXHRS) !! soil temperature
!    REAL TAIR(MAXHRS) !! air temperature - met file
!    REAL RADABV(MAXHRS,3)
    REAL FBEAM(MAXHRS,3) !! fraction of incident PAR which is direct beam
!    REAL RH(MAXHRS) !! relative humidity
!    REAL VPD(MAXHRS) !! vapour pressure deficit
!    REAL VMFD(MAXHRS) !! vapour pressure mole fraction deficit
!    REAL ETMEAS(MAXHRS)
!    REAL CA(MAXHRS) !! atmospheric co2 concentration
!    REAL PRESS(MAXHRS) !! atmospheric pressure
!    REAL PPT(MAXHRS)  !! precipitation
!    REAL SOILMOIST(MAXHRS)
!    REAL DELTAT(12) !! if radiation values missing, calculate incident PAR from air temp
!    REAL TAIRMEM(10000)
!    REAL TAIRR(MAXHRS)
!
!    ! Physiology inputs by layer
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
!
!    ! Structural data inputs
!    REAL BPT(8,MAXC)
!    REAL PROPP(MAXC)
!    REAL PROPC(MAXC)
!    REAL ALPHA(MAXANG)
!    REAL FALPHA(MAXANG)
!    ! Intermediate calculations
    REAL DIFZEN(MAXANG)
!    REAL DEXT(MAXANG)
!    REAL BEXTANG(MAXANG)
    REAL ZEN(MAXHRS)
    REAL AZ(MAXHRS)
!    REAL ZEN0(MAXHRS)
!    REAL AZ0(MAXHRS)
!    REAL TU(MAXP)
!    REAL TD(MAXP)
!    REAL RELDF(MAXP)
!    REAL TUUS(MAXP)
!    REAL TDUS(MAXP)
!    REAL RELDFUS(MAXP)  ! Understorey.
!    REAL DIFDN(MAXP,3)
!    REAL DIFUP(MAXP,3)
!    REAL SCLOST(MAXP,3)
!    REAL BFLUX(MAXP,3)
!    REAL DFLUX(MAXP,3)
!    REAL SCATFX(MAXP,3)
!    REAL SCLOSTTREE(MAXT,3)
!    REAL DOWNTH(MAXP)
!    REAL DOWNTHTREE(MAXT)
!    REAL TUAR(MAXT,MAXP)
!    REAL TDAR(MAXT,MAXP)
!    REAL RELDFAR(MAXT,MAXP)
!    REAL PLANTKCR(MAXT,MAXP)
!
!    ! Outputs for each tree - NOW TREE ARRAYS! (RAD JUNE 2008)
!    REAL THRAB(MAXT,MAXHRS,3)
!    REAL TDYAB(MAXT,3)
!    REAL TCAN(MAXT,MAXHRS)
!    REAL FCO2(MAXT,MAXHRS)
!    REAL FH2O(MAXT,MAXHRS)
!    REAL GSCAN(MAXT,MAXHRS)
!    REAL FHEAT(MAXT,MAXHRS)
!    REAL FH2OCAN(MAXT,MAXHRS)
!    REAL GBHCAN(MAXT,MAXHRS)
!    REAL FRESPF(MAXT,MAXHRS)
!    REAL FRESPW(MAXT,MAXHRS)
!    REAL FRESPB(MAXT,MAXHRS)
!    REAL FRESPFR(MAXT,MAXHRS)
!    REAL FRESPCR(MAXT,MAXHRS)
!    REAL PPAR(MAXT,MAXLAY,MAXHRS)
!    REAL PPS(MAXT,MAXLAY,MAXHRS)
!    REAL PTRANSP(MAXT,MAXLAY,MAXHRS)
!
!    ! Daily totals are now also tree arrays (June 2008 RAD).
!    REAL TOTCO2(MAXT)
!    REAL TOTRESPF(MAXT)
!    REAL TOTRESPWM(MAXT)
!    REAL TOTRESPB(MAXT)
!    REAL TOTRESPFR(MAXT)
!    REAL TOTRESPCR(MAXT)
!    REAL TOTH2O(MAXT)
!    REAL TOTH2OCAN(MAXT)
!    REAL TOTHFX(MAXT)
!
!    ! Water balance related pars.
!    REAL ROOTRESIST
!    REAL ROOTRAD
!    REAL MINROOTWP
!    REAL KTOT
!    REAL PLOTAREA
!    REAL MAXSTORAGE !! parameter for Rutter rainfall interception model
!    REAL DRAINLIMIT(MAXSOILLAY)
!    REAL DISCHARGE
!    REAL FRACORGANIC(MAXSOILLAY)
!    REAL ROOTMASS(MAXSOILLAY)
!    REAL ROOTLEN(MAXSOILLAY)
!    INTEGER NLAYER
!    INTEGER NROOTLAYER
!    INTEGER EQUALUPTAKE !! use equal relative water update from all soil layers
!    INTEGER NSUMMED
!    REAL BPAR(MAXSOILLAY)
!    REAL PSIE(MAXSOILLAY)
!    REAL KSAT(MAXSOILLAY)
!    REAL LAYTHICK(MAXSOILLAY)
!    REAL INITWATER(MAXSOILLAY)
!    REAL FRACROOT(MAXSOILLAY)
!    REAL POREFRAC(MAXSOILLAY)
!    REAL SOILWP(MAXSOILLAY)
!    REAL FRACWATER(MAXSOILLAY)
!    REAL SOILCOND(MAXSOILLAY)
!    REAL SOILRRES(MAXSOILLAY)
!    REAL ICEPROP(MAXSOILLAY)
!    REAL FRACUPTAKE(MAXSOILLAY)
!    REAL WATERGAIN(MAXSOILLAY)
!    REAL WATERLOSS(MAXSOILLAY)
!    REAL PPTGAIN(MAXSOILLAY)
!    REAL SOILTEMP(MAXSOILLAY)
!    REAL WETTINGBOT(10)
!    REAL WETTINGTOP(10)
!    REAL THERMCOND(MAXSOILLAY)
!    REAL TESTER(MAXP)
!
!    ! Multi-species       
    CHARACTER(30) SPECIESNAMES(MAXSP) !! array of names of species in stand
    CHARACTER(30) PHYFILES(MAXSP) !! array of names of physiological input files
    CHARACTER(30) STRFILES(MAXSP) !! array of structure input files (overrides str.dat)
!
!    ! STR arrays, multi-species versions.
    REAL ALPHASPEC(MAXANG,MAXSP)
    REAL FALPHASPEC(MAXANG,MAXSP)
    REAL BPTSPEC(8,MAXC,MAXSP)
    REAL BPTT(8,MAXC,MAXT)
!    REAL BPTTUS(8,MAXC,MAXT)
    REAL SHAPESPEC(MAXSP)
    REAL EXTWINDSPEC(MAXSP)
    REAL RANDOMSPEC(MAXSP)
    REAL COEFFTSPEC(MAXSP)
    REAL EXPONTSPEC(MAXSP)
    REAL WINTERCSPEC(MAXSP)
    REAL BCOEFFTSPEC(MAXSP)
    REAL BEXPONTSPEC(MAXSP)
    REAL BINTERCSPEC(MAXSP)
    REAL DEXTSPEC(MAXSP,MAXANG)
    REAL DEXTT(MAXT,MAXANG)
!    REAL DEXTTUS(MAXT,MAXANG)
    REAL BEXTSPEC(MAXSP)
    REAL BEXTANGSPEC(MAXSP,MAXANG)
!    REAL BEXTANGT(MAXP,MAXANG)
!    REAL BEXTANGTUS(MAXP,MAXANG)
    REAL BEXTT(MAXT)
!    REAL BEXTTUS(MAXT)
    REAL RCOEFFTSPEC(MAXSP)
    REAL REXPONTSPEC(MAXSP)
    REAL RINTERCSPEC(MAXSP)
    REAL FRFRACSPEC(MAXSP)
    INTEGER NOAGECSPEC(MAXSP)
    INTEGER NOAGECT(MAXT)
!    INTEGER NOAGECTUS(MAXT)
    INTEGER JLEAFSPEC(MAXSP)
    INTEGER JLEAFT(MAXT)
!    INTEGER JLEAFTUS(MAXT)
    INTEGER JSHAPESPEC(MAXSP)
    INTEGER NALPHASPEC(MAXSP)
    INTEGER JSHAPET(MAXT)
!    INTEGER JSHAPETUS(MAXT)
    REAL SHAPET(MAXT)
!    REAL SHAPETUS(MAXT)
    REAL VPDMINSPEC(MAXSP)
!    REAL VPDMIN
!
!    ! PHY arrays, multi-species versions.
    REAL ABSRPSPEC(MAXLAY,3,MAXSP)
    REAL ARHOSPEC(MAXLAY,3,MAXSP)
    REAL ATAUSPEC(MAXLAY,3,MAXSP)
    REAL RHOSOLSPEC(3,MAXSP)
    REAL PROPPSPEC(MAXC,MAXSP)
    REAL PROPCSPEC(MAXC,MAXSP)
    REAL PROPPT(MAXC,MAXT)
    REAL PROPCT(MAXC,MAXT)
!    REAL PROPPTUS(MAXC,MAXT)
!    REAL PROPCTUS(MAXC,MAXT)
    REAL LEAFNSPEC(maxdate,MAXLAY,MAXC,MAXSP)
    REAL JMAXTABLESPEC(maxdate,MAXLAY,MAXC,MAXSP)
    REAL VCMAXTABLESPEC(maxdate,MAXLAY,MAXC,MAXSP)
    REAL RDTABLESPEC(maxdate,MAXLAY,MAXC,MAXSP)
    REAL SLATABLESPEC(maxdate,MAXLAY,MAXC,MAXSP)
    REAL AJQTABLESPEC(maxdate,MAXLAY,MAXC,MAXSP)
    REAL Q10FTABLESPEC(maxdate,MAXSP)
    REAL Q10WTABLESPEC(maxdate,MAXSP)
    INTEGER DATESNSPEC(maxdate,MAXSP)
    INTEGER DATESJSPEC(maxdate,MAXSP)
    INTEGER DATESVSPEC(maxdate,MAXSP)
    INTEGER DATESRDSPEC(maxdate,MAXSP)
    INTEGER DATESSLASPEC(maxdate,MAXSP)
    INTEGER DATESASPEC(maxdate,MAXSP)
    INTEGER DATESFQSPEC(maxdate,MAXSP)
    INTEGER DATESWQSPEC(maxdate,MAXSP)
    INTEGER NOAGEPSPEC(MAXSP)
    INTEGER NSIDESSPEC(MAXSP)
!    INTEGER K
    REAL GSREFSPEC(MAXSP)
    REAL GSMINSPEC(MAXSP)
    REAL PAR0SPEC(MAXSP)
    REAL D0SPEC(MAXSP)
    REAL VK1SPEC(MAXSP)
    REAL VK2SPEC(MAXSP)
    REAL VPD1SPEC(MAXSP)
    REAL VPD2SPEC(MAXSP)
    REAL VMFD0SPEC(MAXSP) 
    REAL GSJASPEC(MAXSP)
    REAL GSJBSPEC(MAXSP)
    REAL T0SPEC(MAXSP)
    REAL TREFSPEC(MAXSP)
    REAL TMAXSPEC(MAXSP)
    REAL SMD1SPEC(MAXSP)
    REAL SMD2SPEC(MAXSP)
    REAL WC1SPEC(MAXSP)
    REAL WC2SPEC(MAXSP)
    REAL SWPEXPSPEC(MAXSP)
    REAL D0LSPEC(MAXSP)  
    REAL GAMMASPEC(MAXSP)
    REAL WLEAFSPEC(MAXSP)   
    REAL SFSPEC(MAXSP)
    REAL PSIVSPEC(MAXSP)
!
    INTEGER NOJDATESSPEC(MAXSP)
    INTEGER NOVDATESSPEC(MAXSP)
    INTEGER NOADATESSPEC(MAXSP)
    INTEGER NOSLADATESSPEC(MAXSP)
    INTEGER NORDATESSPEC(MAXSP)  
    INTEGER NOWQDATESSPEC(MAXSP)
    INTEGER NOFQDATESSPEC(MAXSP)
    INTEGER IECOSPEC(MAXSP)
    REAL EAVJSPEC(MAXSP)
    REAL EDVJSPEC(MAXSP)
    REAL DELSJSPEC(MAXSP)
    REAL EAVCSPEC(MAXSP)
    REAL EDVCSPEC(MAXSP)
    REAL DELSCSPEC(MAXSP)
    REAL TVJUPSPEC(MAXSP)
    REAL TVJDNSPEC(MAXSP)
    REAL THETASPEC(MAXSP)
    REAL RTEMPSPEC(MAXSP)
    REAL DAYRESPSPEC(MAXSP)
    REAL EFFYRFSPEC(MAXSP)
    REAL TBELOWSPEC(MAXSP)
    REAL EFFYRWSPEC(MAXSP)
    REAL RMWSPEC(MAXSP)
    REAL RTEMPWSPEC(MAXSP)
    REAL COLLASPEC(MAXSP)
    REAL COLLKSPEC(MAXSP)
    REAL STEMSDWSPEC(MAXSP)
    REAL RMWAREASPEC(MAXSP)
    REAL STEMFORMSPEC(MAXSP)
    REAL Q10RSPEC(MAXSP)
    REAL RTEMPRSPEC(MAXSP)
    REAL Q10BSPEC(MAXSP)
    REAL RTEMPBSPEC(MAXSP)
    REAL RMCRSPEC(MAXSP)
    REAL RMFRSPEC(MAXSP)
    REAL RMBSPEC(MAXSP)
    REAL K10FSPEC(MAXSP)
!    REAL K10F
    REAL G0TABLESPEC(maxdate,MAXSP)
    REAL G1TABLESPEC(maxdate,MAXSP)
    INTEGER NOGSDATESSPEC(MAXSP)
    INTEGER DATESGSSPEC(maxdate,MAXSP)
!    INTEGER DATESGS(maxdate)
!    REAL G0TABLE(maxdate)
!    REAL G1TABLE(maxdate)
!    REAL TARGETFOLS(MAXT)
!    REAL ABSRPU
!    REAL AJQU
!    REAL ALAT
!    REAL ALEAF
!    REAL ANIR
!    REAL APAR
    REAL APP
!    REAL AREA
!    REAL ATHR
!    REAL AX
!    REAL AY
!    REAL BALPHA
!    REAL BLAMBDA
!    REAL BBINC
!    REAL BBIOM
!    REAL BCOEFFT
!    REAL BEAMP
    REAL BEAR
!    REAL BEXPONT
    REAL BEXT
!    REAL BEXTUS
    REAL BINSIZE !! size of classes in histogram
!    REAL BINTERC
!    REAL BMULT
!    REAL CAK
!    REAL CANOPY_STORE
!    REAL CICARAT
    REAL CO2INC !! amount of increase in CO2, climate change scenario
!    REAL COEFFT
!    REAL COLLA
!    REAL COLLK
!    REAL D0
!    REAL D0L
!    REAL DAYRESP
!    REAL DAYL
!    REAL DEC
!    REAL DELSC
!    REAL DELSCU
!    REAL DELSJ
!    REAL DELSJU
!    REAL DISCHARGETOT
!    REAL DAYL0
!    REAL DEC0
!    REAL SUNSET0
!    REAL EQNTIM0
!    REAL SF
!    REAL PSIV
!    REAL HMSHAPE
!    REAL PSILIN
    INTEGER KEEPZEN
!    REAL DIFSKY !! controls distribution of diffuse radiation incident from sky
!    REAL DLAI
!    REAL DMULT2
!    REAL DRAINSTORE
!    REAL DRYTHICK
    REAL DT1
    REAL DT2
!    REAL DRYTHICKMIN  !! minimum thickness of layer of dry soil
    REAL DT3
    REAL DT4
!    REAL EAVC
!    REAL EAVCU
!    REAL EAVJU
!    REAL EDVC
!    REAL EAVJ
!    REAL EDVJ
!    REAL DVJU
!    REAL EDVCU
!    REAL EFFY
!    REAL EFFYRF
!    REAL EDVJU
!    REAL EMAXLEAF
!    REAL EFFYRW
!    REAL EQNTIM
!    REAL ESOIL
!    REAL ET
!    REAL ETEST
!    REAL ETMEASTOT
!    REAL ETMM
!    REAL ETMM2
!    REAL ETMMTOT
!    REAL ETUSMM
!    REAL EVAPSTORE
    REAL EXPAN
!    REAL EXPDIF
!    REAL EXPINF
!    REAL EXPONT
    REAL EXPTIME
!    REAL EXTKUS
!    REAL EXTWIND
!    REAL FAREA
    REAL FBEAMOTC !! fractional reduction in beam factor of PAR (simulate growth in chamber)
!    REAL FBINC
!    REAL FBIOM
!    REAL FRFRAC
!    REAL FSOIL
!    REAL FTSOIL1
!    REAL FSOILMEAN
!    REAL G0 !! stomatal conductance when PAR is zero (part of tuzet model)
!    REAL G1 !! slope parameter (part of tuzet model)
!    REAL GAMMA !! optional CO2 compenstation point ((part of tuzet model)
!    REAL GAMSOIL
!    REAL GBH
!    REAL GRDAREAI
!    REAL GSBG0U
!    REAL GSBG1U
!    REAL FSOIL1
!    REAL GSC
!    REAL GSIPT
!    REAL GSJA
!    REAL GSJB
!    REAL GSREF
!    REAL HFX
!    REAL GSMIN
!    REAL RD0US
!    REAL SLAUS
!    INTEGER I
!    INTEGER IAGE
    INTEGER ICC
!    INTEGER IDAY
!    INTEGER IECO
!    INTEGER IECOU
    INTEGER IEND
    INTEGER IFLUSH
!    INTEGER IHOUR
    INTEGER IOTC
    INTEGER IPROG
    INTEGER IPROGUS
!    INTEGER IPT
!    INTEGER IPTUS
    INTEGER ISIMUS
    INTEGER ISPEC
    INTEGER ISTART
!    INTEGER ISUNLIT
!    INTEGER ITAR
    INTEGER ITERMAX !! controls iterations in combined photosynthesis-transpiration model, 0=leaf temp assumed=air temp, >0 iterative method used, the number of iterations
!    INTEGER ITREE
    INTEGER IUSTFILE
!    INTEGER SIMSOILEVAP !! if =1, soil evaporation is calculated
!    INTEGER IWATFILE
!    INTEGER IWAVE
!    INTEGER IWHICH
!    INTEGER JLEAF
!    INTEGER JSHAPE
!    INTEGER KEEPWET !! used for testing, soil water balance calculated but soil water content updated
!    INTEGER MASPDATE
!    INTEGER MFLAG
    INTEGER MODELGS !! which model to calculate stomatal conductance, 2=Ball-Berry, 3=Ball-Berry-Leuning, 4=Ball-Berry-Opti, otherwise Jarvis model 
    INTEGER MODELJM !! How JMAX and VCMAX parameters read in, 0=read from file, 1=calculate from leaf content
    INTEGER MODELRD !! how RDO parameters read in, 0=read from file, 1=calculate from leaf content 
    INTEGER MODELRW !! how wood respiration parameters read in
    INTEGER MODELSS !! whether photosynthesis calculation done for sun/shade leaves separately
!    INTEGER MOSS
!    INTEGER MOVEWINDOW
!    INTEGER MSTART
!    INTEGER NALPHA
    INTEGER NAZ !! number of azimuth angles calculated 
!    INTEGER NEWCANOPY
!    INTEGER NEWTUTD
!    INTEGER NOADTES
!    INTEGER NOAGEC
!    INTEGER NOADATES
!    INTEGER NOAGEP
!    INTEGER NOALLTREES
    INTEGER NODDATES
!    INTEGER NOFQDATES
!    INTEGER NOFUDATES
!    INTEGER NOGSDATES
!    INTEGER NOHUDATES
!    INTEGER NOJDATES
    INTEGER NOLADATES
    INTEGER NOLAY !! number of layers in the crown
!    INTEGER NOMETCOLS
!    INTEGER NONUDATES
!    INTEGER NORDATES
!    INTEGER NOSLADATES
    INTEGER NOTARGETS !! number of target trees
    INTEGER NOTDATES
    INTEGER NOTREES !! number of target trees, single target tree, see also itargets
!    INTEGER NOUSPOINTS
!    INTEGER NOVDATES
!    INTEGER NOWQDATES
    INTEGER NOXDATES
    INTEGER NOYDATES
    INTEGER NOZDATES
!    INTEGER NSIDES !! number of sides of leaf with stomata (part of tuzet model)
!    INTEGER NSUMMEDW
!    INTEGER NTAIRADD
    INTEGER NUMPNT
    INTEGER NZEN !! number of zenith angles for which diffuse transmittances calculated
    INTEGER NSTEP
!    REAL OUTFLOW
!    REAL OVERFLOW
!    REAL PAR
!    REAL PAR0
    REAL PAROTC !! reduction of incident PAR (simulate growth in chamber)
!    REAL PLANTK
!    REAL KSCALING
!    REAL PPTDAY
!    REAL PPTTOT
!    REAL PRESSK
!    REAL PREVTSOIL
!    REAL PSIL
!    REAL Q10B
!    REAL Q10R
!    REAL Q10W
!    REAL Q10F
!    REAL QC
!    REAL QCTOT
!    REAL QE
!    REAL QETOT
!    REAL QH
!    REAL QHTOT
!    REAL QN
!    REAL QNTOT
!    REAL RADINTERC
!    REAL RADINTERC1
!    REAL RADINTERC2
!    REAL RADINTERC3
!    REAL RADINTERCTOT
!    REAL RANDOM
!    REAL RBINC
!    REAL RBINOM
!    REAL RBIOM
!    REAL RCOEFFT
!    REAL RD
!    REAL RD0ACC
!    REAL RDK
!    REAL RDT
!    REAL RESPF
!    REAL REXPONT
!    REAL RGLOBABV
!    REAL RGLOBABV12
!    REAL RGLOBUND
!    REAL RINTERC
!    REAL RMB
!    REAL RMCR
!    REAL RMFR
!    REAL RMW
!    REAL RMWAREA
!    REAL RNET
!    REAL ROOTRESFRAC
!    REAL ROOTXSECAREA
!    REAL RTEMP
!    REAL RTEMPB
!    REAL RTEMPR
!    REAL RUNOFF
!    REAL RUTTERB !! parameter for Rutter rainfall interception model
!    REAL RUTTERD !! parameter for Rutter rainfall interception model
!    REAL SCLOSTTOT
    REAL SHADEHT !! height of external shading of plot
!    REAL SHAPE
!    REAL SMD1
!    REAL SMD2
!    REAL SOILDEPTH
!    REAL SOILEVAP
!    REAL SOILEVAPTOT
!    REAL SOILMOISTURE
!    REAL SOILTK
!    REAL SOMULT
!    REAL STEMFORM
!    REAL STEMSDW
    REAL STOCKING
    REAL SUNLA
!    REAL SUNSET
!    REAL RTEMPW
!    REAL TMAX
!    REAL TMOVE
!    REAL SURFACE_WATERMM
!    REAL SWMAX !! maximum soil water content
!    REAL SWMIN !! minimum soil water content
!    REAL SWPEXP
!    REAL T0
!    REAL THETAM
!    REAL THROUGHFALL !! parameter for Rutter rainfall interception model
    REAL TINC !! amount of increase in air temperature, climate change scenario
!    REAL TLEAF
    REAL TOTC !! increase of air temp (simulate growth in chamber)
!    REAL TOTESTEVAPMM
!    REAL TBELOW
!    REAL TDIFF
!    REAL TFALLTOT
!    REAL THETA
!    REAL TOTRESPRG
!    REAL TOTRESPBG
!    CHARACTER(len=256) :: CTITLE
!    CHARACTER(len=256) :: TTITLE
!    CHARACTER(len=256) :: PTITLE
!    CHARACTER(len=256) :: STITLE
!    CHARACTER(len=256) :: WTITLE
!    CHARACTER(len=256) :: UTITLE
    CHARACTER(len=256) :: VTITLE !! program title and version
!    CHARACTER(len=256) :: MTITLE
    CHARACTER(len=256) :: in_path
    CHARACTER(len=256) :: out_path
!    
!    ! Outputs for PAR histogram
!    REAL HISTO(MAXT,MAXHISTO)  ! Note dim change (RAD Sept. 08).
!   
!    ! mgdk...
    INTEGER NSPECIES !! number of species in the stand
!
!    !
    REAL WINDOTC !! absolute wind speed (simulate growth in chamber)
    REAL XSLOPE !! slopes of tree plots in degrees
    REAL YSLOPE !! slopes of tree plots in degrees
    REAL X0 !! offset values of tree locations
    REAL Y0 !! offset values of tree locations
    REAL XMAX !! max x coordinate of tree locations
    REAL YMAX !! max y coordinate of tree locations
    REAL ZHT !! roughness measurement height (m)
    REAL Z0HT !! roughness length (m)
    REAL ZPD !! (roughness) zero plane displacement (m)
!    REAL TORTPAR !! parameter describing tortuosity of soil
!    REAL TTIMD
!    REAL TVJUPU
!    REAL TVJDNU
!    REAL VCMAXN25
!    REAL UNMIN
!    REAL VCMAX25M
!    REAL WSOIL
!    REAL WSOILROOT
!    REAL WSOILMEAN
!    REAL WSOILROOTMEAN
!    REAL SWPMEAN
!    REAL TOTTMP
!    REAL TOTLAI
!    REAL WINTERC
!    REAL TVJUP
!    REAL TVJDN
!    REAL VK1
!    REAL VK2
!    REAL VPD1
!    REAL VPD2
!    REAL VMFD0
!    REAL TREF
!    REAL WC1
!    REAL WC2
!    REAL WLEAF !! effective leaf width (part of tuzet model)
!    REAL WBIOM
!    REAL WBINC
!    REAL WEIGHTEDSWP
!    REAL TSCAT
!    REAL FRACAPAR
!    REAL VIEWFACTOR
!    REAL TOTRESPWG
!    REAL TOTRESPFRG
!    REAL TOTRESPCRG
!    REAL TOTRESPFG
!    REAL TOTSOILRES
!    REAL MINLEAFWP
!    REAL TMP
!    REAL CI
!
!
    REAL G0SPEC(MAXSP),G1SPEC(MAXSP)
    REAL GKSPEC(MAXSP)
!    REAL GK
!    REAL XLP(MAXP)
!    REAL YLP(MAXP)
!    REAL ZLP(MAXP)
!    INTEGER LAYERP(MAXP)
!    INTEGER MLAYERP(MAXP)
!    REAL RELDFP(MAXP)
!    REAL TUP(MAXP)
!    REAL TDP(MAXP)
!    
!    REAL EFFK
!    REAL TOTESTEVAP
!    INTEGER LAITHROUGHF
!    REAL PSILCAN(MAXT,MAXHRS)
!    REAL CICAN(MAXT,MAXHRS)
!    REAL PSILCANMIN(MAXT,MAXHRS)
!    REAL ECANMAX(MAXT,MAXHRS)
!    REAL ACANMAX(MAXT,MAXHRS)
!    
    REAL VPARASPEC(MAXSP)
    REAL VPARBSPEC(MAXSP)
    REAL VPARCSPEC(MAXSP)
!    REAL VPARA,VPARB,VPARC
!    INTEGER VFUN
    INTEGER VFUNSPEC(MAXSP)
         
    INTEGER IOHRLY    ! Controls daily, hourly, and/or layer outp
    INTEGER IOTUTD    ! Controls transmittance file output
    INTEGER IOHIST    ! Controls histogram output
    INTEGER IORESP    ! Controls respiration output
    INTEGER IODAILY   ! Controls daily output: FIXED HERE 
    INTEGER IOWATBAL  ! Controls water balance output
    INTEGER IOFORMAT  ! Dump mode...
    
    integer ISUNLA
    REAL PLOTAREA
    INTEGER NOALLTREES  
end type maespaConfigvariablesstate

    
type maespaConfigTreeMapState
        INTEGER numberTreePlots
        INTEGER, DIMENSION(:), ALLOCATABLE :: xlocation
        INTEGER, DIMENSION(:), ALLOCATABLE :: ylocation
        INTEGER, DIMENSION(:), ALLOCATABLE :: phyfileNumber
        INTEGER, DIMENSION(:), ALLOCATABLE :: strfileNumber
        INTEGER, DIMENSION(:), ALLOCATABLE :: treesfileNumber
        INTEGER, DIMENSION(:), ALLOCATABLE :: treesHeight
        
        INTEGER numberBuildingPlots
        INTEGER, DIMENSION(:), ALLOCATABLE :: xBuildingLocation
        INTEGER, DIMENSION(:), ALLOCATABLE :: yBuildingLocation
        INTEGER, DIMENSION(:), ALLOCATABLE :: buildingsHeight
        
        INTEGER width
        INTEGER length
        !TYPE(maespaConfigvariablesstate) , DIMENSION(:), ALLOCATABLE :: config
end type maespaConfigTreeMapState
    


!    type maespaConfigTreeMaps
!        TYPE(maespaConfigTreeMapState) treeMap
!        TYPE(maespaConfigvariablesstate) config
!    end type maespaConfigTreeMaps
!    
    
END MODULE MaespaConfigState
