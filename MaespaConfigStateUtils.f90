!     
! File:   MaespaConfigStateUtils.f90
! Author: kerryn
!
! Created on 28 March 2014, 3:30 PM
!
Module MaespaConfigStateUtils
    contains
    
        
!    function convertIntToString(number)
!        INTEGER number
!        
!            character(len=1024) :: filename
!    character(len=1024) :: format_string
!    integer :: i
!
!    do i=1, 10
!        if (i < 10) then
!            format_string = "(A5,I1)"
!        else
!            format_string = "(A5,I2)"
!        endif
!
!        write (filename,format_string) "hello", i
!        print *, trim(filename)
!    enddo
!    convertIntToString = filename
!    return
!    
!    end function convertIntToString
    
 function constructFilename(prefix, number, suffix)
        
    INTEGER number
    character(len=*) :: prefix
    character(len=1024) :: suffix
    character(len=1024) :: filename
    character(len=1024) :: constructFilename
    character(len=1024) :: format_string
    
    if (number < 10) then
        format_string = "(A5,I1)"
    else
        format_string = "(A5,I2)"
    endif

    write (filename,format_string) trim(prefix), number
!    print *, trim(filename)
    constructFilename = trim(filename)//trim(suffix)
    return
    
 end function constructFilename
    
    
    SUBROUTINE SaveMaespaConfigState(state)
  
        use MaespaConfigState
!    USE MaespaState
    USE switches
!    USE metcom
!    USE maestcom
!
    USE maindeclarations
!    !use for2r
    
    TYPE(maespaConfigvariablesstate), intent(OUT) :: state
    

    
    
!  ! List of trees for which to do calculations
    state%ITARGETS=ITARGETS
    state%ISPECIES=ISPECIES
    state%ISPECIEST=ISPECIEST
    state%ISPECIESTUS=ISPECIESTUS
!    state%J=J
!    state%USESTAND=USESTAND
!
!    ! Tree positions and dimensions - all trees + all dates
    state%DXT1=DXT1
    state%DYT1=DYT1
    state%DZT1=DZT1
!    state%RXTABLE1=RXTABLE1
!    state%RYTABLE1=RYTABLE1
!    state%RZTABLE1=RZTABLE1
!    state%ZBCTABLE1=ZBCTABLE1
!    state%FOLTABLE1=FOLTABLE1
!    state%DIAMTABLE1=DIAMTABLE1
!    ! Tree positions and dimensions - sorted trees + all dates
!    state%RXTABLE=RXTABLE
!    state%RYTABLE=RYTABLE
!    state%RZTABLE=RZTABLE
!    state%ZBCTABLE=ZBCTABLE
!    state%FOLTABLE=FOLTABLE
    state%TOTLAITABLE=TOTLAITABLE
!    
!    state%DIAMTABLE=DIAMTABLE
!    state%IT=IT
!    state%ITUUS=ITUUS
!    ! Dates for tree dimensions
    state%DATESX=DATESX
    state%DATESY=DATESY
    state%DATESZ=DATESZ
    state%DATEST=DATEST
    state%DATESLA=DATESLA
    state%DATESD=DATESD
!    ! Tree dimensions on simulation date (by interpolation)
    state%DXT=DXT
    state%DYT=DYT
    state%DZT=DZT
    state%RX=RX
    state%RY=RY
    state%RZ=RZ
    state%ZBC=ZBC
    state%FOLT=FOLT
!    state%DIAM=DIAM
!    state%EXPFACTORS=EXPFACTORS
    state%WEIGHTS=WEIGHTS
!    state%CANOPYDIMS=CANOPYDIMS
!    ! Positions of grid points + associated volume & leaf area etc
    state%XL=XL
    state%YL=YL
    state%ZL=ZL
!    state%VL=VL
!    state%DLT=DLT
!    state%DLI=DLI
!    state%XL2=XL2
!    state%YL2=YL2
!    state%ZL2=ZL2
!    state%VL2=VL2
!   
!    state%LGP=LGP
!    state%LAYER=LAYER
!    state%MLAYER=MLAYER
    state%PPLAY=PPLAY
!    state%FOLLAY=FOLLAY
!    state%WINDLAY=WINDLAY
!    state%LGP2=LGP2
!    state%FOLLAY2=FOLLAY2
!      
!    ! Understorey arrays.
!    state%XLU=XLU
!    state%YLU=YLU
!    state%ZLU=ZLU
!    state%LAYERUS=LAYERUS
!    state%MLAYERUS= MLAYERUS
!    state%UIBEAM=UIBEAM
!    state%UIDIFF=UIDIFF
!    state%PARUS=PARUS
!    state%APARUS=APARUS
!    state%PSUS=PSUS
!    state%ETUS=ETUS
!    state%PARUNDER=PARUNDER
!    state%USLAITAB=USLAITAB
!    state%USLAI=USLAI
!    state%HTUS=HTUS
!    state%FOLNUS=FOLNUS
!    state%DATESFU=DATESFU
!    state%DATESHU=DATESHU
!    state%DATESNU=DATESNU
!    state%JMAXN25=JMAXN25
!    state%JMAX25M=JMAX25M
!    state%RESCALE=RESCALE
!    state%FUS=FUS
!    state%AREAUS=AREAUS
!    state%FN0US=FN0US
!    state%DXTUS=DXTUS
!    state%DYTUS=DYTUS
!    state%DZTUS=DZTUS
!    state%RXTABLEUS=RXTABLEUS
!    state%RYTABLEUS=RYTABLEUS
!    state%RZTABLEUS=RZTABLEUS
!    state%FOLTABLEUS=FOLTABLEUS
!    state%ZBCTABLEUS=ZBCTABLEUS
!    state%DIAMTABLEUS=DIAMTABLEUS
!    state%RXUS=RXUS
!    state%RYUS=RYUS
!    state%RZUS=RZUS
!    state%ZBCUS=ZBCUS
!    state%FOLTUS=FOLTUS
!    state%PARUSMEAN=PARUSMEAN
!    state%PARUSSD=PARUSSD
!    state%THRABUS=THRABUS
!    state%FCO2US=FCO2US
!    state%FH2OUS=FH2OUS
!
!    ! Met data
!    state%METCOLS=METCOLS
    state%SOILDATA=SOILDATA
!    state%TSOILDATA=TSOILDATA
!    state%REASSIGNRAIN=REASSIGNRAIN
!    state%SIMTSOIL=SIMTSOIL
!    state%WSOILMETHOD=WSOILMETHOD
!    state%RETFUNCTION=RETFUNCTION
!    state%USEMEASET=USEMEASET
    state%USEMEASSW=USEMEASSW
!    state%WINDAH=WINDAH
!    state%TSOIL=TSOIL
!    state%TAIR=TAIR
!    state%RADABV=RADABV
    state%FBEAM=FBEAM
!    state%RH=RH
!    state%VPD=VPD
!    state%VMFD=VMFD
!    state%ETMEAS=ETMEAS
!    state%CA=CA
!    state%PRESS=PRESS
!    state%PPT=PPT
!    state%SOILMOIST=SOILMOIST
!    state%DELTAT=DELTAT
!    state%TAIRMEM=TAIRMEM
!    state%TAIRR=TAIRR
!
!    ! Physiology inputs by layer
!    state%ABSRP=ABSRP
!    state%ARHO=ARHO
!    state%ATAU=ATAU
!    state%RHOSOL=RHOSOL
!    state%JMAXTABLE=JMAXTABLE
!    state%VCMAXTABLE=VCMAXTABLE
!    state%RDTABLE=RDTABLE
!    state%SLATABLE=SLATABLE
!    state%AJQTABLE=AJQTABLE
!    state%Q10FTABLE=Q10FTABLE
!    state%Q10WTABLE=Q10WTABLE
!    state%DATESFQ=DATESFQ
!    state%DATESWQ=DATESWQ
!    state%DATESJ=DATESJ
!    state%DATESV=DATESV
!    state%DATESRD=DATESRD
!    state%DATESSLA=DATESSLA
!    state%DATESA=DATESA
!    state%JMAX25=JMAX25
!    state%VCMAX25=VCMAX25
!    state%RD0=RD0
!    state%SLA=SLA
!    state%AJQ=AJQ
!
!    ! Structural data inputs
!    state%BPT=BPT
!    state%PROPP=PROPP
!    state%PROPC=PROPC
!    state%ALPHA=ALPHA
!    state%FALPHA=FALPHA
!    ! Intermediate calculations
    state%DIFZEN=DIFZEN
!    state%DEXT=DEXT
!    state%BEXTANG=BEXTANG
    state%ZEN=ZEN
    state%AZ=AZ
!    state%ZEN0=ZEN0
!    state%AZ0=AZ0
!    state%TU=TU
!    state%TD=TD
!    state%RELDF=RELDF
!     ! Understorey.
!    state%TUUS=TUUS
!    state%TDUS=TDUS
!    state%RELDFUS=RELDFUS
!    state%DIFDN=DIFDN
!    state%DIFUP=DIFUP
!    state%SCLOST=SCLOST
!    state%BFLUX=BFLUX
!    state%DFLUX=DFLUX
!    state%SCATFX=SCATFX
!    state%SCLOSTTREE=SCLOSTTREE
!    state%DOWNTH=DOWNTH
!    state%DOWNTHTREE=DOWNTHTREE
!    state%TUAR=TUAR
!    state%TDAR=TDAR
!    state%RELDFAR=RELDFAR
!    state%PLANTKCR=PLANTKCR
!
!    ! Outputs for each tree - NOW TREE ARRAYS! (RAD JUNE 2008)
!    state%THRAB=THRAB
!    state%TDYAB=TDYAB
!    state%TCAN=TCAN
!    state%FCO2=FCO2
!    state%FH2O=FH2O
!    state%GSCAN=GSCAN
!    state%FHEAT=FHEAT
!    state%FH2OCAN=FH2OCAN
!    state%GBHCAN=GBHCAN
!    state%FRESPF=FRESPF
!    state%FRESPW=FRESPW
!    state%FRESPB=FRESPB
!    state%FRESPFR=FRESPFR
!    state%FRESPCR=FRESPCR
!    state%PPAR=PPAR
!    state%PPS=PPS
!    state%PTRANSP=PTRANSP
!
!    ! Daily totals are now also tree arrays (June 2008 RAD).
!    state%TOTCO2=TOTCO2
!    state%TOTRESPF=TOTRESPF
!    state%TOTRESPWM=TOTRESPWM
!    state%TOTRESPB=TOTRESPB
!    state%TOTRESPFR=TOTRESPFR
!    state%TOTRESPCR=TOTRESPCR
!    state%TOTH2O=TOTH2O
!    state%TOTH2OCAN=TOTH2OCAN
!    state%TOTHFX=TOTHFX
!
!    ! Water balance related pars.
!    state%ROOTRESIST=ROOTRESIST
!    state%ROOTRAD=ROOTRAD
!    state%MINROOTWP=MINROOTWP
!    state%KTOT=KTOT
!    state%PLOTAREA=PLOTAREA
!    state%MAXSTORAGE=MAXSTORAGE
!    state%DRAINLIMIT=DRAINLIMIT
!    state%DISCHARGE=DISCHARGE
!    state%FRACORGANIC=FRACORGANIC
!    state%ROOTMASS=ROOTMASS
!    state%ROOTLEN=ROOTLEN
!    state%NLAYER=NLAYER
!    state%NROOTLAYER=NROOTLAYER
!    state%EQUALUPTAKE=EQUALUPTAKE
!    state%NSUMMED=NSUMMED
!    state%BPAR=BPAR
!    state%PSIE=PSIE
!    state%KSAT=KSAT
!    state%LAYTHICK=LAYTHICK
!    state%INITWATER=INITWATER
!    state%FRACROOT=FRACROOT
!    state%POREFRAC=POREFRAC
!    state%SOILWP=SOILWP
!    state%FRACWATER=FRACWATER
!    state%SOILCOND=SOILCOND
!    state%SOILRRES=SOILRRES
!    state%ICEPROP=ICEPROP
!    state%FRACUPTAKE=FRACUPTAKE
!    state%WATERGAIN=WATERGAIN
!    state%WATERLOSS=WATERLOSS
!    state%PPTGAIN=PPTGAIN
!    state%SOILTEMP=SOILTEMP
!    state%WETTINGBOT=WETTINGBOT
!    state%WETTINGTOP=WETTINGTOP
!    state%THERMCOND=THERMCOND
!    state%TESTER=TESTER
!
!    ! Multi-species       
    state%SPECIESNAMES=SPECIESNAMES
    state%PHYFILES=PHYFILES
    state%STRFILES=STRFILES
!
!    ! STR arrays + multi-species versions.
    state%ALPHASPEC=ALPHASPEC
    state%FALPHASPEC=FALPHASPEC
    state%BPTSPEC=BPTSPEC
    state%BPTT=BPTT
!    state%BPTTUS=BPTTUS
    state%SHAPESPEC=SHAPESPEC
    state%EXTWINDSPEC=EXTWINDSPEC
    state%RANDOMSPEC=RANDOMSPEC
    state%COEFFTSPEC=COEFFTSPEC
    state%EXPONTSPEC=EXPONTSPEC
    state%WINTERCSPEC=WINTERCSPEC
    state%BCOEFFTSPEC=BCOEFFTSPEC
    state%BEXPONTSPEC=BEXPONTSPEC
    state%BINTERCSPEC=BINTERCSPEC
    state%DEXTSPEC=DEXTSPEC
!    state%DEXTT=DEXTT
!    state%DEXTTUS=DEXTTUS
    state%BEXTSPEC=BEXTSPEC
    state%BEXTANGSPEC=BEXTANGSPEC
!    state%BEXTANGT=BEXTANGT
!    state%BEXTANGTUS=BEXTANGTUS
    state%BEXTT=BEXTT
!    state%BEXTTUS=BEXTTUS
!    state%RCOEFFTSPEC=RCOEFFTSPEC
!    state%REXPONTSPEC=REXPONTSPEC
!    state%RINTERCSPEC=RINTERCSPEC
!    state%FRFRACSPEC=FRFRACSPEC
    state%NOAGECSPEC=NOAGECSPEC
    state%NOAGECT=NOAGECT
!    state%NOAGECTUS=NOAGECTUS
    state%JLEAFSPEC=JLEAFSPEC
    state%JLEAFT=JLEAFT
!    state%JLEAFTUS=JLEAFTUS
    state%JSHAPESPEC=JSHAPESPEC
    state%NALPHASPEC=NALPHASPEC
    state%JSHAPET=JSHAPET
!    state%JSHAPETUS=JSHAPETUS
    state%SHAPET=SHAPET
!    state%SHAPETUS=SHAPETUS
    state%VPDMINSPEC=VPDMINSPEC
!    state%VPDMIN=VPDMIN
!
!    ! PHY arrays + multi-species versions.
    state%ABSRPSPEC=ABSRPSPEC
    state%ARHOSPEC=ARHOSPEC
    state%ATAUSPEC=ATAUSPEC
    state%RHOSOLSPEC=RHOSOLSPEC
    state%PROPPSPEC=PROPPSPEC
    state%PROPCSPEC=PROPCSPEC
!    state%PROPPT=PROPPT
    state%PROPCT=PROPCT
!    state%PROPPTUS=PROPPTUS
!    state%PROPCTUS=PROPCTUS
    state%LEAFNSPEC=LEAFNSPEC
    state%JMAXTABLESPEC=JMAXTABLESPEC
    state%VCMAXTABLESPEC=VCMAXTABLESPEC
    state%RDTABLESPEC=RDTABLESPEC
    state%SLATABLESPEC=SLATABLESPEC
    state%AJQTABLESPEC=AJQTABLESPEC
    state%Q10FTABLESPEC=Q10FTABLESPEC
    state%Q10WTABLESPEC=Q10WTABLESPEC
!   
    state%DATESJSPEC=DATESJSPEC
    state%DATESVSPEC=DATESVSPEC
    state%DATESRDSPEC=DATESRDSPEC
    state%DATESSLASPEC=DATESSLASPEC
    state%DATESASPEC=DATESASPEC
    state%DATESFQSPEC=DATESFQSPEC
    state%DATESWQSPEC=DATESWQSPEC
    state%NOAGEPSPEC=NOAGEPSPEC
    state%NSIDESSPEC=NSIDESSPEC
!    state%K=K
    state%GSREFSPEC=GSREFSPEC
    state%GSMINSPEC=GSMINSPEC
    state%PAR0SPEC=PAR0SPEC
    state%D0SPEC=D0SPEC
    state%VK1SPEC=VK1SPEC
    state%VK2SPEC=VK2SPEC
    state%VPD1SPEC=VPD1SPEC
    state%VPD2SPEC=VPD2SPEC
    state%VMFD0SPEC=VMFD0SPEC
    state%GSJASPEC=GSJASPEC
    state%GSJBSPEC=GSJBSPEC
    state%T0SPEC=T0SPEC
    state%TREFSPEC=TREFSPEC
    state%TMAXSPEC=TMAXSPEC
    state%SMD1SPEC=SMD1SPEC
    state%SMD2SPEC=SMD2SPEC
    state%WC1SPEC=WC1SPEC
    state%WC2SPEC=WC2SPEC
    state%SWPEXPSPEC=SWPEXPSPEC
    state%D0LSPEC=D0LSPEC
    state%GAMMASPEC=GAMMASPEC
    state%WLEAFSPEC=WLEAFSPEC
    state%SFSPEC=SFSPEC
    state%PSIVSPEC=PSIVSPEC
!   
    state%NOJDATESSPEC=NOJDATESSPEC
    state%NOVDATESSPEC=NOVDATESSPEC
    state%NOADATESSPEC=NOADATESSPEC
    state%NOSLADATESSPEC=NOSLADATESSPEC
    state%NORDATESSPEC=NORDATESSPEC
    state%NOWQDATESSPEC=NOWQDATESSPEC
    state%NOFQDATESSPEC=NOFQDATESSPEC
    state%IECOSPEC=IECOSPEC
    state%EAVJSPEC=EAVJSPEC
    state%EDVJSPEC=EDVJSPEC
    state%DELSJSPEC=DELSJSPEC
    state%EAVCSPEC=EAVCSPEC
    state%EDVCSPEC=EDVCSPEC
    state%DELSCSPEC=DELSCSPEC
    state%TVJUPSPEC=TVJUPSPEC
    state%TVJDNSPEC=TVJDNSPEC
    state%THETASPEC=THETASPEC
    state%RTEMPSPEC=RTEMPSPEC
    state%DAYRESPSPEC=DAYRESPSPEC
    state%EFFYRFSPEC=EFFYRFSPEC
    state%TBELOWSPEC=TBELOWSPEC
    state%EFFYRWSPEC=EFFYRWSPEC
    state%RMWSPEC=RMWSPEC
    state%RTEMPWSPEC=RTEMPWSPEC
    state%COLLASPEC=COLLASPEC
    state%COLLKSPEC=COLLKSPEC
    state%STEMSDWSPEC=STEMSDWSPEC
    state%RMWAREASPEC=RMWAREASPEC
    state%STEMFORMSPEC=STEMFORMSPEC
    state%Q10RSPEC=Q10RSPEC
    state%RTEMPRSPEC=RTEMPRSPEC
    state%Q10BSPEC=Q10BSPEC
    state%RTEMPBSPEC=RTEMPBSPEC
    state%RMCRSPEC=RMCRSPEC
    state%RMFRSPEC=RMFRSPEC
    state%RMBSPEC=RMBSPEC
    state%K10FSPEC=K10FSPEC
!    state%K10F=K10F
    state%G0TABLESPEC=G0TABLESPEC
    state%G1TABLESPEC=G1TABLESPEC
    state%NOGSDATESSPEC=NOGSDATESSPEC
    state%DATESGSSPEC=DATESGSSPEC
!    state%DATESGS=DATESGS
!    state%G0TABLE=G0TABLE
!    state%G1TABLE=G1TABLE
!    state%TARGETFOLS=TARGETFOLS
!    state%ABSRPU=ABSRPU
!    state%AJQU=AJQU
!    state%ALAT=ALAT
!    state%ALEAF=ALEAF
!    state%ANIR=ANIR
!    state%APAR=APAR
    state%APP=APP
!    state%AREA=AREA
!    state%ATHR=ATHR
!    state%AX=AX
!    state%AY=AY
!    state%BALPHA=BALPHA
!    state%BLAMBDA=BLAMBDA
!    state%BBINC=BBINC
!    state%BBIOM=BBIOM
!    state%BCOEFFT=BCOEFFT
!    state%BEAMP=BEAMP
!    state%BEAR=BEAR
!    state%BEXPONT=BEXPONT
    state%BEXT=BEXT
!    state%BEXTUS=BEXTUS
    state%BINSIZE=BINSIZE
!    state%BINTERC=BINTERC
!    state%BMULT=BMULT
!    state%CAK=CAK
!    state%CANOPY_STORE=CANOPY_STORE
!    state%CICARAT=CICARAT
    state%CO2INC=CO2INC
!    state%COEFFT=COEFFT
!    state%COLLA=COLLA
!    state%COLLK=COLLK
!    state%D0=D0
!    state%D0L=D0L
!    state%DAYRESP=DAYRESP
!    state%DAYL=DAYL
!    state%DEC=DEC
!    state%DELSC=DELSC
!    state%DELSCU=DELSCU
!    state%DELSJ=DELSJ
!    state%DELSJU=DELSJU
!    state%DISCHARGETOT=DISCHARGETOT
!    state%DAYL0=DAYL0
!    state%DEC0=DEC0
!    state%SUNSET0=SUNSET0
!    state%EQNTIM0=EQNTIM0
!    state%SF=SF
!    state%PSIV=PSIV
!    state%HMSHAPE=HMSHAPE
!    state%PSILIN=PSILIN
    state%KEEPZEN=KEEPZEN
!    state%DIFSKY=DIFSKY
!    state%DLAI=DLAI
!    state%DMULT2=DMULT2
!    state%DRAINSTORE=DRAINSTORE
!    state%DRYTHICK=DRYTHICK
!    state%DT1=DT1
!    state%DT2=DT2
!    state%DRYTHICKMIN=DRYTHICKMIN
!    state%DT3=DT3
!    state%DT4=DT4
!    state%EAVC=EAVC
!    state%EAVCU=EAVCU
!    state%EAVJU=EAVJU
!    state%EDVC=EDVC
!    state%EAVJ=EAVJ
!    state%EDVJ=EDVJ
!    state%DVJU=DVJU
!    state%EDVCU=EDVCU
!    state%EFFY=EFFY
!    state%EFFYRF=EFFYRF
!    state%EDVJU=EDVJU
!    state%EMAXLEAF=EMAXLEAF
!    state%EFFYRW=EFFYRW
!    state%EQNTIM=EQNTIM
!    state%ESOIL=ESOIL
!    state%ET=ET
!    state%ETEST=ETEST
!    state%ETMEASTOT=ETMEASTOT
!    state%ETMM=ETMM
!    state%ETMM2=ETMM2
!    state%ETMMTOT=ETMMTOT
!    state%ETUSMM=ETUSMM
!    state%EVAPSTORE=EVAPSTORE
    state%EXPAN=EXPAN
!    state%EXPDIF=EXPDIF
!    state%EXPINF=EXPINF
!    state%EXPONT=EXPONT
    state%EXPTIME=EXPTIME
!    state%EXTKUS=EXTKUS
!    state%EXTWIND=EXTWIND
!    state%FAREA=FAREA
    state%FBEAMOTC=FBEAMOTC
!    state%FBINC=FBINC
!    state%FBIOM=FBIOM
!    state%FRFRAC=FRFRAC
!    state%FSOIL=FSOIL
!    state%FTSOIL1=FTSOIL1
!    state%FSOILMEAN=FSOILMEAN
!    state%G0=G0
!    state%G1=G1
!    state%GAMMA=GAMMA
!    state%GAMSOIL=GAMSOIL
!    state%GBH=GBH
!    state%GRDAREAI=GRDAREAI
!    state%GSBG0U=GSBG0U
!    state%GSBG1U=GSBG1U
!    state%FSOIL1=FSOIL1
!    state%GSC=GSC
!    state%GSIPT=GSIPT
!    state%GSJA=GSJA
!    state%GSJB=GSJB
!    state%GSREF=GSREF
!    state%HFX=HFX
!    state%GSMIN=GSMIN
!    state%RD0US=RD0US
!    state%SLAUS=SLAUS
!    state%I=I
!    state%IAGE=IAGE
    state%ICC=ICC
!    state%IDAY=IDAY
!    state%IECO=IECO
!    state%IECOU=IECOU
    state%IEND=IEND
    state%IFLUSH=IFLUSH
!    state%IHOUR=IHOUR
    state%IOTC=IOTC
    state%IPROG=IPROG
    state%IPROGUS=IPROGUS
!    state%IPT=IPT
!    state%IPTUS=IPTUS
    state%ISIMUS=ISIMUS
    state%ISPEC=ISPEC
    state%ISTART=ISTART
!    state%ISUNLIT=ISUNLIT
!    state%ITAR=ITAR
!    state%ITERMAX=ITERMAX
!    state%ITREE=ITREE
    state%IUSTFILE=IUSTFILE
!    state%SIMSOILEVAP=SIMSOILEVAP
!    state%IWATFILE=IWATFILE
!    state%IWAVE=IWAVE
!    state%IWHICH=IWHICH
!    state%JLEAF=JLEAF
!    state%JSHAPE=JSHAPE
!    state%KEEPWET=KEEPWET
!    state%MASPDATE=MASPDATE
!    state%MFLAG=MFLAG
    state%MODELGS=MODELGS
    state%MODELJM=MODELJM
    state%MODELRD=MODELRD
    state%MODELRW=MODELRW
    state%MODELSS=MODELSS
!    state%MOSS=MOSS
!    state%MOVEWINDOW=MOVEWINDOW
!    state%MSTART=MSTART
!    state%NALPHA=NALPHA
    state%NAZ=NAZ
!    state%NEWCANOPY=NEWCANOPY
!    state%NEWTUTD=NEWTUTD
!    state%NOADTES=NOADTES
!    state%NOAGEC=NOAGEC
!    state%NOADATES=NOADATES
!    state%NOAGEP=NOAGEP
!    state%NOALLTREES=NOALLTREES
    state%NODDATES=NODDATES
!    state%NOFQDATES=NOFQDATES
!    state%NOFUDATES=NOFUDATES
!    state%NOGSDATES=NOGSDATES
!    state%NOHUDATES=NOHUDATES
!    state%NOJDATES=NOJDATES
    state%NOLADATES=NOLADATES
    state%NOLAY=NOLAY
!    state%NOMETCOLS=NOMETCOLS
!    state%NONUDATES=NONUDATES
!    state%NORDATES=NORDATES
!    state%NOSLADATES=NOSLADATES
    state%NOTARGETS=NOTARGETS
!    state%NOTDATES=NOTDATES
    state%NOTREES=NOTREES
!    state%NOUSPOINTS=NOUSPOINTS
!    state%NOVDATES=NOVDATES
!    state%NOWQDATES=NOWQDATES
    state%NOXDATES=NOXDATES
    state%NOYDATES=NOYDATES
    state%NOZDATES=NOZDATES
!    state%NSIDES=NSIDES
!    state%NSUMMEDW=NSUMMEDW
!    state%NTAIRADD=NTAIRADD
    state%NUMPNT=NUMPNT
    state%NZEN=NZEN
    state%NSTEP=NSTEP
!    state%OUTFLOW=OUTFLOW
!    state%OVERFLOW=OVERFLOW
!    state%PAR=PAR
!    state%PAR0=PAR0
    state%PAROTC=PAROTC
!    state%PLANTK=PLANTK
!    state%KSCALING=KSCALING
!    state%PPTDAY=PPTDAY
!    state%PPTTOT=PPTTOT
!    state%PRESSK=PRESSK
!    state%PREVTSOIL=PREVTSOIL
!    state%PSIL=PSIL
!    state%Q10B=Q10B
!    state%Q10R=Q10R
!    state%Q10W=Q10W
!    state%Q10F=Q10F
!    state%QC=QC
!    state%QCTOT=QCTOT
!    state%QE=QE
!    state%QETOT=QETOT
!    state%QH=QH
!    state%QHTOT=QHTOT
!    state%QN=QN
!    state%QNTOT=QNTOT
!    state%RADINTERC=RADINTERC
!    state%RADINTERC1=RADINTERC1
!    state%RADINTERC2=RADINTERC2
!    state%RADINTERC3=RADINTERC3
!    state%RADINTERCTOT=RADINTERCTOT
!    state%RANDOM=RANDOM
!    state%RBINC=RBINC
!    state%RBINOM=RBINOM
!    state%RBIOM=RBIOM
!    state%RCOEFFT=RCOEFFT
!    state%RD=RD
!    state%RD0ACC=RD0ACC
!    state%RDK=RDK
!    state%RDT=RDT
!    state%RESPF=RESPF
!    state%REXPONT=REXPONT
!    state%RGLOBABV=RGLOBABV
!    state%RGLOBABV12=RGLOBABV12
!    state%RGLOBUND=RGLOBUND
!    state%RINTERC=RINTERC
!    state%RMB=RMB
!    state%RMCR=RMCR
!    state%RMFR=RMFR
!    state%RMW=RMW
!    state%RMWAREA=RMWAREA
!    
!    state%RNET=RNET
!    state%ROOTRESFRAC=ROOTRESFRAC
!    state%ROOTXSECAREA=ROOTXSECAREA
!    state%RTEMP=RTEMP
!    state%RTEMPB=RTEMPB
!    state%RTEMPR=RTEMPR
!    state%RUNOFF=RUNOFF
!    state%RUTTERB=RUTTERB
!    state%RUTTERD=RUTTERD
!    state%SCLOSTTOT=SCLOSTTOT
    state%SHADEHT=SHADEHT
!    state%SHAPE=SHAPE
!    state%SMD1=SMD1
!    state%SMD2=SMD2
!    state%SOILDEPTH=SOILDEPTH
!    state%SOILEVAP=SOILEVAP
!    state%SOILEVAPTOT=SOILEVAPTOT
!    state%SOILMOISTURE=SOILMOISTURE
!    state%SOILTK=SOILTK
!    state%SOMULT=SOMULT
!    state%STEMFORM=STEMFORM
!    state%STEMSDW=STEMSDW
    state%STOCKING=STOCKING
    state%SUNLA=SUNLA
!    state%SUNSET=SUNSET
!    state%RTEMPW=RTEMPW
!    state%TMAX=TMAX
!    state%TMOVE=TMOVE
!    state%SURFACE_WATERMM=SURFACE_WATERMM
!    state%SWMAX=SWMAX
!    state%SWMIN=SWMIN
!    state%SWPEXP=SWPEXP
!    state%T0=T0
!    state%THETAM=THETAM
!    state%THROUGHFALL=THROUGHFALL
    state%TINC=TINC
!    state%TLEAF=TLEAF
    state%TOTC=TOTC
!    state%TOTESTEVAPMM=TOTESTEVAPMM
!    state%TBELOW=TBELOW
!    state%TDIFF=TDIFF
!    state%TFALLTOT=TFALLTOT
!    state%THETA=THETA
!    state%TOTRESPRG=TOTRESPRG
!    state%TOTRESPBG=TOTRESPBG
!    state%CTITLE=CTITLE
!    state%TTITLE=TTITLE
!    state%PTITLE=PTITLE
!    state%STITLE=STITLE
!    state%WTITLE=WTITLE
!    state%UTITLE=UTITLE
    state%VTITLE=VTITLE
!    state%MTITLE=MTITLE
    state%in_path=in_path
    state%out_path=out_path
!    
!    ! Outputs for PAR histogram
!    state%HISTO=HISTO
!   
!    ! mgdk...
    state%NSPECIES=NSPECIES
!
    state%WINDOTC=WINDOTC
    state%XSLOPE=XSLOPE
    state%YSLOPE=YSLOPE
    state%X0=X0
    state%Y0=Y0
    state%XMAX=XMAX
    state%YMAX=YMAX
    state%ZHT=ZHT
    state%Z0HT=Z0HT
    state%ZPD=ZPD
!    state%TORTPAR=TORTPAR
!    state%TTIMD=TTIMD
!    state%TVJUPU=TVJUPU
!    state%TVJDNU=TVJDNU
!    state%VCMAXN25=VCMAXN25
!    state%UNMIN=UNMIN
!    state%VCMAX25M=VCMAX25M
!    state%WSOIL=WSOIL
!    state%WSOILROOT=WSOILROOT
!    state%WSOILMEAN=WSOILMEAN
!    state%WSOILROOTMEAN=WSOILROOTMEAN
!    state%SWPMEAN=SWPMEAN
!    state%TOTTMP=TOTTMP
!    state%TOTLAI=TOTLAI
!    state%WINTERC=WINTERC
!    state%TVJUP=TVJUP
!    state%TVJDN=TVJDN
!    state%VK1=VK1
!    state%VK2=VK2
!    state%VPD1=VPD1
!    state%VPD2=VPD2
!    state%VMFD0=VMFD0
!    state%TREF=TREF
!    state%WC1=WC1
!    state%WC2=WC2
!    state%WLEAF=WLEAF
!    state%WBIOM=WBIOM
!    state%WBINC=WBINC
!    state%WEIGHTEDSWP=WEIGHTEDSWP
!    state%TSCAT=TSCAT
!    state%FRACAPAR=FRACAPAR
!    state%VIEWFACTOR=VIEWFACTOR
!    state%TOTRESPWG=TOTRESPWG
!    state%TOTRESPFRG=TOTRESPFRG
!    state%TOTRESPCRG=TOTRESPCRG
!    state%TOTRESPFG=TOTRESPFG
!    state%TOTSOILRES=TOTSOILRES
!    state%MINLEAFWP=MINLEAFWP
!    state%TMP=TMP
!    state%CI=CI
!
!
    state%G0SPEC=G0SPEC
    state%G1SPEC=G1SPEC
    state%GKSPEC=GKSPEC
!    state%GK=GK
!    state%XLP=XLP
!    state%YLP=YLP
!    state%ZLP=ZLP
!    state%LAYERP=LAYERP
!    state%MLAYERP=MLAYERP
!    state%RELDFP=RELDFP
!    state%TUP=TUP
!    state%TDP=TDP
!    
!    state%EFFK=EFFK
!    state%TOTESTEVAP=TOTESTEVAP
!    state%LAITHROUGHF=LAITHROUGHF
!    state%PSILCAN=PSILCAN
!    state%CICAN=CICAN
!    state%PSILCANMIN=PSILCANMIN
!    state%ECANMAX=ECANMAX
!    state%ACANMAX=ACANMAX
!    
    state%VPARASPEC=VPARASPEC
    state%VPARBSPEC=VPARBSPEC
    state%VPARCSPEC=VPARCSPEC
!    state%VPARA=VPARA
!    state%VPARB=VPARB
!    state%VPARC=VPARC
!    state%VFUN=VFUN
    state%VFUNSPEC=VFUNSPEC

    
    !call open_r_file("/home/kerryn/workspace/TUF-3DMaespa/maespaInitValues.rdat", digits=6)
    !call wrt_r_matrix("bldht", ix=bldht)
    !call WRT_R_MATRIX_THREE_DIM("surf_shade", l=surf_shade)
    !call WRT_R_MATRIX_FOUR_DIM("surf", l=surf)
    !call close_r_file()
    
    state%IOHRLY=IOHRLY
    state%IOTUTD=IOTUTD   
    state%IOHIST=IOHIST    
    state%IORESP=IORESP    
    state%IODAILY=IODAILY    
    state%IOWATBAL=IOWATBAL  
    state%IOFORMAT=IOFORMAT 
    
    state%ISUNLA=ISUNLA
    state%ITERMAX=ITERMAX 
    state%BEAR=BEAR 
    state%PLOTAREA=PLOTAREA 
    state%NOALLTREES=NOALLTREES 
    state%NOTDATES=NOTDATES 
    
    state%RXTABLE1=RXTABLE1
    state%RYTABLE1=RYTABLE1
    state%RZTABLE1=RZTABLE1
    state%ZBCTABLE1=ZBCTABLE1
    state%FOLTABLE1=FOLTABLE1
    state%DIAMTABLE1=DIAMTABLE1
    

END  SUBROUTINE SaveMaespaConfigState  


SUBROUTINE UpdateMaespaConfigFromState(state)
  
    USE MaespaConfigState
!    USE MaespaState
    USE switches
!    USE metcom
!    USE maestcom
!
    USE maindeclarations
!    !use for2r
    
    TYPE(maespaConfigvariablesstate), intent(IN) :: state
    
    
    
    
!  ! List of trees for which to do calculations
    ITARGETS=state%ITARGETS
    ISPECIES=state%ISPECIES
    ISPECIEST=state%ISPECIEST
    ISPECIESTUS=state%ISPECIESTUS
!    J=state%J
!    USESTAND=state%USESTAND
!
!    ! Tree positions and dimensions - all trees + all dates
!    DXT1=state%DXT1
!    DYT1=state%DYT1
!    DZT1=state%DZT1
!    RXTABLE1=state%RXTABLE1
!    RYTABLE1=state%RYTABLE1
!    RZTABLE1=state%RZTABLE1
!    ZBCTABLE1=state%ZBCTABLE1
!    FOLTABLE1=state%FOLTABLE1
!    DIAMTABLE1=state%DIAMTABLE1
!    ! Tree positions and dimensions - sorted trees + all dates
!    RXTABLE=state%RXTABLE
!    RYTABLE=state%RYTABLE
!    RZTABLE=state%RZTABLE
!    ZBCTABLE=state%ZBCTABLE
!    FOLTABLE=state%FOLTABLE
    TOTLAITABLE=state%TOTLAITABLE
!    
!    DIAMTABLE=state%DIAMTABLE
!    IT=state%IT
!    ITUUS=state%ITUUS
!    ! Dates for tree dimensions
    DATESX=state%DATESX
    DATESY=state%DATESY
    DATESZ=state%DATESZ
    DATEST=state%DATEST
    DATESLA=state%DATESLA
    DATESD=state%DATESD
!    ! Tree dimensions on simulation date (by interpolation)
!    DXT=state%DXT
!    DYT=state%DYT
!    DZT=state%DZT
    DXT1=state%DXT1
    DYT1=state%DYT1
    DZT1=state%DZT1
    RX=state%RX
    RY=state%RY
    RZ=state%RZ
    ZBC=state%ZBC
    FOLT=state%FOLT
!    DIAM=state%DIAM
!    EXPFACTORS=state%EXPFACTORS
    WEIGHTS=state%WEIGHTS
!    CANOPYDIMS=state%CANOPYDIMS
!    ! Positions of grid points + associated volume & leaf area etc
    XL=state%XL
    YL=state%YL
    ZL=state%ZL
!    VL=state%VL
!    DLT=state%DLT
!    DLI=state%DLI
!    XL2=state%XL2
!    YL2=state%YL2
!    ZL2=state%ZL2
!    VL2=state%VL2
!   
!    LGP=state%LGP
!    LAYER=state%LAYER
!    MLAYER=state%MLAYER  
    PPLAY=state%PPLAY
!    FOLLAY=state%FOLLAY
!    WINDLAY=state%WINDLAY
!    LGP2=state%LGP2
!    FOLLAY2=state%FOLLAY2
!      
!    ! Understorey arrays.
!    XLU=state%XLU
!    YLU=state%YLU
!    ZLU=state%ZLU
!    LAYERUS=state%LAYERUS
!    MLAYERUS= MLAYERUS
!    UIBEAM=state%UIBEAM
!    UIDIFF=state%UIDIFF
!    PARUS=state%PARUS
!    APARUS=state%APARUS
!    PSUS=state%PSUS
!    ETUS=state%ETUS
!    PARUNDER=state%PARUNDER
!    USLAITAB=state%USLAITAB
!    USLAI=state%USLAI
!    HTUS=state%HTUS
!    FOLNUS=state%FOLNUS
!    DATESFU=state%DATESFU
!    DATESHU=state%DATESHU
!    DATESNU=state%DATESNU
!    JMAXN25=state%JMAXN25
!    JMAX25M=state%JMAX25M
!    RESCALE=state%RESCALE
!    FUS=state%FUS
!    AREAUS=state%AREAUS
!    FN0US=state%FN0US
!    DXTUS=state%DXTUS
!    DYTUS=state%DYTUS
!    DZTUS=state%DZTUS
!    RXTABLEUS=state%RXTABLEUS
!    RYTABLEUS=state%RYTABLEUS
!    RZTABLEUS=state%RZTABLEUS
!    FOLTABLEUS=state%FOLTABLEUS
!    ZBCTABLEUS=state%ZBCTABLEUS
!    DIAMTABLEUS=state%DIAMTABLEUS
!    RXUS=state%RXUS
!    RYUS=state%RYUS
!    RZUS=state%RZUS
!    ZBCUS=state%ZBCUS
!    FOLTUS=state%FOLTUS
!    PARUSMEAN=state%PARUSMEAN
!    PARUSSD=state%PARUSSD
!    THRABUS=state%THRABUS
!    FCO2US=state%FCO2US
!    FH2OUS=state%FH2OUS
!
!    ! Met data
!    METCOLS=state%METCOLS
    SOILDATA=state%SOILDATA
!    TSOILDATA=state%TSOILDATA
!    REASSIGNRAIN=state%REASSIGNRAIN
!    SIMTSOIL=state%SIMTSOIL
!    WSOILMETHOD=state%WSOILMETHOD
!    RETFUNCTION=state%RETFUNCTION
!    USEMEASET=state%USEMEASET
    USEMEASSW=state%USEMEASSW
!    WINDAH=state%WINDAH
!    TSOIL=state%TSOIL
!    TAIR=state%TAIR
!    RADABV=state%RADABV
    FBEAM=state%FBEAM
!    RH=state%RH
!    VPD=state%VPD
!    VMFD=state%VMFD
!    ETMEAS=state%ETMEAS
!    CA=state%CA
!    PRESS=state%PRESS
!    PPT=state%PPT
!    SOILMOIST=state%SOILMOIST
!    DELTAT=state%DELTAT
!    TAIRMEM=state%TAIRMEM
!    TAIRR=state%TAIRR
!
!    ! Physiology inputs by layer
!    ABSRP=state%ABSRP
!    ARHO=state%ARHO
!    ATAU=state%ATAU
!    RHOSOL=state%RHOSOL
!    JMAXTABLE=state%JMAXTABLE
!    VCMAXTABLE=state%VCMAXTABLE
!    RDTABLE=state%RDTABLE
!    SLATABLE=state%SLATABLE
!    AJQTABLE=state%AJQTABLE
!    Q10FTABLE=state%Q10FTABLE
!    Q10WTABLE=state%Q10WTABLE
!    DATESFQ=state%DATESFQ
!    DATESWQ=state%DATESWQ
!    DATESJ=state%DATESJ
!    DATESV=state%DATESV
!    DATESRD=state%DATESRD
!    DATESSLA=state%DATESSLA
!    DATESA=state%DATESA
!    JMAX25=state%JMAX25
!    VCMAX25=state%VCMAX25
!    RD0=state%RD0
!    SLA=state%SLA
!    AJQ=state%AJQ
!
!    ! Structural data inputs
!    BPT=state%BPT
!    PROPP=state%PROPP
!    PROPC=state%PROPC
!    ALPHA=state%ALPHA
!    FALPHA=state%FALPHA
!    ! Intermediate calculations
    DIFZEN=state%DIFZEN
!    DEXT=state%DEXT
!    BEXTANG=state%BEXTANG
    ZEN=state%ZEN
    AZ=state%AZ
!    ZEN0=state%ZEN0
!    AZ0=state%AZ0
!    TU=state%TU
!    TD=state%TD
!    RELDF=state%RELDF
!     ! Understorey.
!    TUUS=state%TUUS
!    TDUS=state%TDUS
!    RELDFUS=state%RELDFUS
!    DIFDN=state%DIFDN
!    DIFUP=state%DIFUP
!    SCLOST=state%SCLOST
!    BFLUX=state%BFLUX
!    DFLUX=state%DFLUX
!    SCATFX=state%SCATFX
!    SCLOSTTREE=state%SCLOSTTREE
!    DOWNTH=state%DOWNTH
!    DOWNTHTREE=state%DOWNTHTREE
!    TUAR=state%TUAR
!    TDAR=state%TDAR
!    RELDFAR=state%RELDFAR
!    PLANTKCR=state%PLANTKCR
!
!    ! Outputs for each tree - NOW TREE ARRAYS! (RAD JUNE 2008)
!    THRAB=state%THRAB
!    TDYAB=state%TDYAB
!    TCAN=state%TCAN
!    FCO2=state%FCO2
!    FH2O=state%FH2O
!    GSCAN=state%GSCAN
!    FHEAT=state%FHEAT
!    FH2OCAN=state%FH2OCAN
!    GBHCAN=state%GBHCAN
!    FRESPF=state%FRESPF
!    FRESPW=state%FRESPW
!    FRESPB=state%FRESPB
!    FRESPFR=state%FRESPFR
!    FRESPCR=state%FRESPCR
!    PPAR=state%PPAR
!    PPS=state%PPS
!    PTRANSP=state%PTRANSP
!
!    ! Daily totals are now also tree arrays (June 2008 RAD).
!    TOTCO2=state%TOTCO2
!    TOTRESPF=state%TOTRESPF
!    TOTRESPWM=state%TOTRESPWM
!    TOTRESPB=state%TOTRESPB
!    TOTRESPFR=state%TOTRESPFR
!    TOTRESPCR=state%TOTRESPCR
!    TOTH2O=state%TOTH2O
!    TOTH2OCAN=state%TOTH2OCAN
!    TOTHFX=state%TOTHFX
!
!    ! Water balance related pars.
!    ROOTRESIST=state%ROOTRESIST
!    ROOTRAD=state%ROOTRAD
!    MINROOTWP=state%MINROOTWP
!    KTOT=state%KTOT
!    PLOTAREA=state%PLOTAREA
!    MAXSTORAGE=state%MAXSTORAGE
!    DRAINLIMIT=state%DRAINLIMIT
!    DISCHARGE=state%DISCHARGE
!    FRACORGANIC=state%FRACORGANIC
!    ROOTMASS=state%ROOTMASS
!    ROOTLEN=state%ROOTLEN
!    NLAYER=state%NLAYER
!    NROOTLAYER=state%NROOTLAYER
!    EQUALUPTAKE=state%EQUALUPTAKE
!    NSUMMED=state%NSUMMED
!    BPAR=state%BPAR
!    PSIE=state%PSIE
!    KSAT=state%KSAT
!    LAYTHICK=state%LAYTHICK
!    INITWATER=state%INITWATER
!    FRACROOT=state%FRACROOT
!    POREFRAC=state%POREFRAC
!    SOILWP=state%SOILWP
!    FRACWATER=state%FRACWATER
!    SOILCOND=state%SOILCOND
!    SOILRRES=state%SOILRRES
!    ICEPROP=state%ICEPROP
!    FRACUPTAKE=state%FRACUPTAKE
!    WATERGAIN=state%WATERGAIN
!    WATERLOSS=state%WATERLOSS
!    PPTGAIN=state%PPTGAIN
!    SOILTEMP=state%SOILTEMP
!    WETTINGBOT=state%WETTINGBOT
!    WETTINGTOP=state%WETTINGTOP
!    THERMCOND=state%THERMCOND
!    TESTER=state%TESTER
!
!    ! Multi-species       
    SPECIESNAMES=state%SPECIESNAMES
    PHYFILES=state%PHYFILES
    STRFILES=state%STRFILES
!
!    ! STR arrays + multi-species versions.
    ALPHASPEC=state%ALPHASPEC
    FALPHASPEC=state%FALPHASPEC
    BPTSPEC=state%BPTSPEC
    BPTT=state%BPTT
!    BPTTUS=state%BPTTUS
    SHAPESPEC=state%SHAPESPEC
    EXTWINDSPEC=state%EXTWINDSPEC
    RANDOMSPEC=state%RANDOMSPEC
    COEFFTSPEC=state%COEFFTSPEC
    EXPONTSPEC=state%EXPONTSPEC
    WINTERCSPEC=state%WINTERCSPEC
    BCOEFFTSPEC=state%BCOEFFTSPEC
    BEXPONTSPEC=state%BEXPONTSPEC
    BINTERCSPEC=state%BINTERCSPEC
    DEXTSPEC=state%DEXTSPEC
!    DEXTT=state%DEXTT
!    DEXTTUS=state%DEXTTUS
    BEXTSPEC=state%BEXTSPEC
    BEXTANGSPEC=state%BEXTANGSPEC
!    BEXTANGT=state%BEXTANGT
!    BEXTANGTUS=state%BEXTANGTUS
    BEXTT=state%BEXTT
!    BEXTTUS=state%BEXTTUS
    RCOEFFTSPEC=state%RCOEFFTSPEC
    REXPONTSPEC=state%REXPONTSPEC
    RINTERCSPEC=state%RINTERCSPEC
    FRFRACSPEC=state%FRFRACSPEC
    NOAGECSPEC=state%NOAGECSPEC
    NOAGECT=state%NOAGECT
!    NOAGECTUS=state%NOAGECTUS
    JLEAFSPEC=state%JLEAFSPEC
    JLEAFT=state%JLEAFT
!    JLEAFTUS=state%JLEAFTUS
    JSHAPESPEC=state%JSHAPESPEC
    NALPHASPEC=state%NALPHASPEC
    JSHAPET=state%JSHAPET
!    JSHAPETUS=state%JSHAPETUS
    SHAPET=state%SHAPET
!    SHAPETUS=state%SHAPETUS
    VPDMINSPEC=state%VPDMINSPEC
!    VPDMIN=state%VPDMIN
!
!    ! PHY arrays + multi-species versions.
    ABSRPSPEC=state%ABSRPSPEC
    ARHOSPEC=state%ARHOSPEC
    ATAUSPEC=state%ATAUSPEC
    RHOSOLSPEC=state%RHOSOLSPEC
    PROPPSPEC=state%PROPPSPEC
    PROPCSPEC=state%PROPCSPEC
!    PROPPT=state%PROPPT
    PROPCT=state%PROPCT
!    PROPPTUS=state%PROPPTUS
!    PROPCTUS=state%PROPCTUS
    LEAFNSPEC=state%LEAFNSPEC
    JMAXTABLESPEC=state%JMAXTABLESPEC
    VCMAXTABLESPEC=state%VCMAXTABLESPEC
    RDTABLESPEC=state%RDTABLESPEC
    SLATABLESPEC=state%SLATABLESPEC
    AJQTABLESPEC=state%AJQTABLESPEC
    Q10FTABLESPEC=state%Q10FTABLESPEC
    Q10WTABLESPEC=state%Q10WTABLESPEC
!   
    DATESJSPEC=state%DATESJSPEC
    DATESVSPEC=state%DATESVSPEC
    DATESRDSPEC=state%DATESRDSPEC
    DATESSLASPEC=state%DATESSLASPEC
    DATESASPEC=state%DATESASPEC
    DATESFQSPEC=state%DATESFQSPEC
    DATESWQSPEC=state%DATESWQSPEC
    NOAGEPSPEC=state%NOAGEPSPEC
    NSIDESSPEC=state%NSIDESSPEC
!    K=state%K
    GSREFSPEC=state%GSREFSPEC
    GSMINSPEC=state%GSMINSPEC
    PAR0SPEC=state%PAR0SPEC
    D0SPEC=state%D0SPEC
    VK1SPEC=state%VK1SPEC
    VK2SPEC=state%VK2SPEC
    VPD1SPEC=state%VPD1SPEC
    VPD2SPEC=state%VPD2SPEC
    VMFD0SPEC=state%VMFD0SPEC
    GSJASPEC=state%GSJASPEC
    GSJBSPEC=state%GSJBSPEC
    T0SPEC=state%T0SPEC
    TREFSPEC=state%TREFSPEC
    TMAXSPEC=state%TMAXSPEC
    SMD1SPEC=state%SMD1SPEC
    SMD2SPEC=state%SMD2SPEC
    WC1SPEC=state%WC1SPEC
    WC2SPEC=state%WC2SPEC
    SWPEXPSPEC=state%SWPEXPSPEC
    D0LSPEC=state%D0LSPEC
    GAMMASPEC=state%GAMMASPEC
    WLEAFSPEC=state%WLEAFSPEC
    SFSPEC=state%SFSPEC
    PSIVSPEC=state%PSIVSPEC
!   
    NOJDATESSPEC=state%NOJDATESSPEC
    NOVDATESSPEC=state%NOVDATESSPEC
    NOADATESSPEC=state%NOADATESSPEC
    NOSLADATESSPEC=state%NOSLADATESSPEC
    NORDATESSPEC=state%NORDATESSPEC
    NOWQDATESSPEC=state%NOWQDATESSPEC
    NOFQDATESSPEC=state%NOFQDATESSPEC
    IECOSPEC=state%IECOSPEC
    EAVJSPEC=state%EAVJSPEC
    EDVJSPEC=state%EDVJSPEC
    DELSJSPEC=state%DELSJSPEC
    EAVCSPEC=state%EAVCSPEC
    EDVCSPEC=state%EDVCSPEC
    DELSCSPEC=state%DELSCSPEC
    TVJUPSPEC=state%TVJUPSPEC
    TVJDNSPEC=state%TVJDNSPEC
    THETASPEC=state%THETASPEC
    RTEMPSPEC=state%RTEMPSPEC
    DAYRESPSPEC=state%DAYRESPSPEC
    EFFYRFSPEC=state%EFFYRFSPEC
    TBELOWSPEC=state%TBELOWSPEC
    EFFYRWSPEC=state%EFFYRWSPEC
    RMWSPEC=state%RMWSPEC
    RTEMPWSPEC=state%RTEMPWSPEC
    COLLASPEC=state%COLLASPEC
    COLLKSPEC=state%COLLKSPEC
    STEMSDWSPEC=state%STEMSDWSPEC
    RMWAREASPEC=state%RMWAREASPEC
    STEMFORMSPEC=state%STEMFORMSPEC
    Q10RSPEC=state%Q10RSPEC
    RTEMPRSPEC=state%RTEMPRSPEC
    Q10BSPEC=state%Q10BSPEC
    RTEMPBSPEC=state%RTEMPBSPEC
    RMCRSPEC=state%RMCRSPEC
    RMFRSPEC=state%RMFRSPEC
    RMBSPEC=state%RMBSPEC
    K10FSPEC=state%K10FSPEC
!    K10F=state%K10F
    G0TABLESPEC=state%G0TABLESPEC
    G1TABLESPEC=state%G1TABLESPEC
    NOGSDATESSPEC=state%NOGSDATESSPEC
    DATESGSSPEC=state%DATESGSSPEC
!    DATESGS=state%DATESGS
!    G0TABLE=state%G0TABLE
!    G1TABLE=state%G1TABLE
!    TARGETFOLS=state%TARGETFOLS
!    ABSRPU=state%ABSRPU
!    AJQU=state%AJQU
!    ALAT=state%ALAT
!    ALEAF=state%ALEAF
!    ANIR=state%ANIR
!    APAR=state%APAR
    APP=state%APP
!    AREA=state%AREA
!    ATHR=state%ATHR
!    AX=state%AX
!    AY=state%AY
!    BALPHA=state%BALPHA
!    BLAMBDA=state%BLAMBDA
!    BBINC=state%BBINC
!    BBIOM=state%BBIOM
!    BCOEFFT=state%BCOEFFT
!    BEAMP=state%BEAMP
!    BEAR=state%BEAR
!    BEXPONT=state%BEXPONT
    BEXT=state%BEXT
!    BEXTUS=state%BEXTUS
    BINSIZE=state%BINSIZE
!    BINTERC=state%BINTERC
!    BMULT=state%BMULT
!    CAK=state%CAK
!    CANOPY_STORE=state%CANOPY_STORE
!    CICARAT=state%CICARAT
    CO2INC=state%CO2INC
!    COEFFT=state%COEFFT
!    COLLA=state%COLLA
!    COLLK=state%COLLK
!    D0=state%D0
!    D0L=state%D0L
!    DAYRESP=state%DAYRESP
!    DAYL=state%DAYL
!    DEC=state%DEC
!    DELSC=state%DELSC
!    DELSCU=state%DELSCU
!    DELSJ=state%DELSJ
!    DELSJU=state%DELSJU
!    DISCHARGETOT=state%DISCHARGETOT
!    DAYL0=state%DAYL0
!    DEC0=state%DEC0
!    SUNSET0=state%SUNSET0
!    EQNTIM0=state%EQNTIM0
!    SF=state%SF
!    PSIV=state%PSIV
!    HMSHAPE=state%HMSHAPE
!    PSILIN=state%PSILIN
    KEEPZEN=state%KEEPZEN
!    DIFSKY=state%DIFSKY
!    DLAI=state%DLAI
!    DMULT2=state%DMULT2
!    DRAINSTORE=state%DRAINSTORE
!    DRYTHICK=state%DRYTHICK
    DT1=state%DT1
    DT2=state%DT2
!    DRYTHICKMIN=state%DRYTHICKMIN
    DT3=state%DT3
    DT4=state%DT4
!    EAVC=state%EAVC
!    EAVCU=state%EAVCU
!    EAVJU=state%EAVJU
!    EDVC=state%EDVC
!    EAVJ=state%EAVJ
!    EDVJ=state%EDVJ
!    DVJU=state%DVJU
!    EDVCU=state%EDVCU
!    EFFY=state%EFFY
!    EFFYRF=state%EFFYRF
!    EDVJU=state%EDVJU
!    EMAXLEAF=state%EMAXLEAF
!    EFFYRW=state%EFFYRW
!    EQNTIM=state%EQNTIM
!    ESOIL=state%ESOIL
!    ET=state%ET
!    ETEST=state%ETEST
!    ETMEASTOT=state%ETMEASTOT
!    ETMM=state%ETMM
!    ETMM2=state%ETMM2
!    ETMMTOT=state%ETMMTOT
!    ETUSMM=state%ETUSMM
!    EVAPSTORE=state%EVAPSTORE
    EXPAN=state%EXPAN
!    EXPDIF=state%EXPDIF
!    EXPINF=state%EXPINF
!    EXPONT=state%EXPONT
    EXPTIME=state%EXPTIME
!    EXTKUS=state%EXTKUS
!    EXTWIND=state%EXTWIND
!    FAREA=state%FAREA
    FBEAMOTC=state%FBEAMOTC
!    FBINC=state%FBINC
!    FBIOM=state%FBIOM
!    FRFRAC=state%FRFRAC
!    FSOIL=state%FSOIL
!    FTSOIL1=state%FTSOIL1
!    FSOILMEAN=state%FSOILMEAN
!    G0=state%G0
!    G1=state%G1
!    GAMMA=state%GAMMA
!    GAMSOIL=state%GAMSOIL
!    GBH=state%GBH
!    GRDAREAI=state%GRDAREAI
!    GSBG0U=state%GSBG0U
!    GSBG1U=state%GSBG1U
!    FSOIL1=state%FSOIL1
!    GSC=state%GSC
!    GSIPT=state%GSIPT
!    GSJA=state%GSJA
!    GSJB=state%GSJB
!    GSREF=state%GSREF
!    HFX=state%HFX
!    GSMIN=state%GSMIN
!    RD0US=state%RD0US
!    SLAUS=state%SLAUS
!    I=state%I
!    IAGE=state%IAGE
    ICC=state%ICC
!    IDAY=state%IDAY
!    IECO=state%IECO
!    IECOU=state%IECOU
    IEND=state%IEND
    IFLUSH=state%IFLUSH
!    IHOUR=state%IHOUR
    IOTC=state%IOTC
    IPROG=state%IPROG
    IPROGUS=state%IPROGUS
!    IPT=state%IPT
!    IPTUS=state%IPTUS
    ISIMUS=state%ISIMUS
    ISPEC=state%ISPEC
    ISTART=state%ISTART
!    ISUNLIT=state%ISUNLIT
!    ITAR=state%ITAR
!    ITERMAX=state%ITERMAX
!    ITREE=state%ITREE
    IUSTFILE=state%IUSTFILE
!    SIMSOILEVAP=state%SIMSOILEVAP
!    IWATFILE=state%IWATFILE
!    IWAVE=state%IWAVE
!    IWHICH=state%IWHICH
!    JLEAF=state%JLEAF
!    JSHAPE=state%JSHAPE
!    KEEPWET=state%KEEPWET
!    MASPDATE=state%MASPDATE
!    MFLAG=state%MFLAG
    MODELGS=state%MODELGS
    MODELJM=state%MODELJM
    MODELRD=state%MODELRD
    MODELRW=state%MODELRW
    MODELSS=state%MODELSS
!    MOSS=state%MOSS
!    MOVEWINDOW=state%MOVEWINDOW
!    MSTART=state%MSTART
!    NALPHA=state%NALPHA
    NAZ=state%NAZ
!    NEWCANOPY=state%NEWCANOPY
!    NEWTUTD=state%NEWTUTD
!    NOADTES=state%NOADTES
!    NOAGEC=state%NOAGEC
!    NOADATES=state%NOADATES
!    NOAGEP=state%NOAGEP
!    NOALLTREES=state%NOALLTREES
    NODDATES=state%NODDATES
!    NOFQDATES=state%NOFQDATES
!    NOFUDATES=state%NOFUDATES
!    NOGSDATES=state%NOGSDATES
!    NOHUDATES=state%NOHUDATES
!    NOJDATES=state%NOJDATES
    NOLADATES=state%NOLADATES
    NOLAY=state%NOLAY
!    NOMETCOLS=state%NOMETCOLS
!    NONUDATES=state%NONUDATES
!    NORDATES=state%NORDATES
!    NOSLADATES=state%NOSLADATES
    NOTARGETS=state%NOTARGETS
!    NOTDATES=state%NOTDATES
    NOTREES=state%NOTREES
!    NOUSPOINTS=state%NOUSPOINTS
!    NOVDATES=state%NOVDATES
!    NOWQDATES=state%NOWQDATES
    NOXDATES=state%NOXDATES
    NOYDATES=state%NOYDATES
    NOZDATES=state%NOZDATES
!    NSIDES=state%NSIDES
!    NSUMMEDW=state%NSUMMEDW
!    NTAIRADD=state%NTAIRADD
    NUMPNT=state%NUMPNT
    NZEN=state%NZEN
    NSTEP=state%NSTEP
!    OUTFLOW=state%OUTFLOW
!    OVERFLOW=state%OVERFLOW
!    PAR=state%PAR
!    PAR0=state%PAR0
    PAROTC=state%PAROTC
!    PLANTK=state%PLANTK
!    KSCALING=state%KSCALING
!    PPTDAY=state%PPTDAY
!    PPTTOT=state%PPTTOT
!    PRESSK=state%PRESSK
!    PREVTSOIL=state%PREVTSOIL
!    PSIL=state%PSIL
!    Q10B=state%Q10B
!    Q10R=state%Q10R
!    Q10W=state%Q10W
!    Q10F=state%Q10F
!    QC=state%QC
!    QCTOT=state%QCTOT
!    QE=state%QE
!    QETOT=state%QETOT
!    QH=state%QH
!    QHTOT=state%QHTOT
!    QN=state%QN
!    QNTOT=state%QNTOT
!    RADINTERC=state%RADINTERC
!    RADINTERC1=state%RADINTERC1
!    RADINTERC2=state%RADINTERC2
!    RADINTERC3=state%RADINTERC3
!    RADINTERCTOT=state%RADINTERCTOT
!    RANDOM=state%RANDOM
!    RBINC=state%RBINC
!    RBINOM=state%RBINOM
!    RBIOM=state%RBIOM
!    RCOEFFT=state%RCOEFFT
!    RD=state%RD
!    RD0ACC=state%RD0ACC
!    RDK=state%RDK
!    RDT=state%RDT
!    RESPF=state%RESPF
!    REXPONT=state%REXPONT
!    RGLOBABV=state%RGLOBABV
!    RGLOBABV12=state%RGLOBABV12
!    RGLOBUND=state%RGLOBUND
!    RINTERC=state%RINTERC
!    RMB=state%RMB
!    RMCR=state%RMCR
!    RMFR=state%RMFR
!    RMW=state%RMW
!    RMWAREA=state%RMWAREA
!    
!    RNET=state%RNET
!    ROOTRESFRAC=state%ROOTRESFRAC
!    ROOTXSECAREA=state%ROOTXSECAREA
!    RTEMP=state%RTEMP
!    RTEMPB=state%RTEMPB
!    RTEMPR=state%RTEMPR
!    RUNOFF=state%RUNOFF
!    RUTTERB=state%RUTTERB
!    RUTTERD=state%RUTTERD
!    SCLOSTTOT=state%SCLOSTTOT
    SHADEHT=state%SHADEHT
!    SHAPE=state%SHAPE
!    SMD1=state%SMD1
!    SMD2=state%SMD2
!    SOILDEPTH=state%SOILDEPTH
!    SOILEVAP=state%SOILEVAP
!    SOILEVAPTOT=state%SOILEVAPTOT
!    SOILMOISTURE=state%SOILMOISTURE
!    SOILTK=state%SOILTK
!    SOMULT=state%SOMULT
!    STEMFORM=state%STEMFORM
!    STEMSDW=state%STEMSDW
    STOCKING=state%STOCKING
    SUNLA=state%SUNLA
!    SUNSET=state%SUNSET
!    RTEMPW=state%RTEMPW
!    TMAX=state%TMAX
!    TMOVE=state%TMOVE
!    SURFACE_WATERMM=state%SURFACE_WATERMM
!    SWMAX=state%SWMAX
!    SWMIN=state%SWMIN
!    SWPEXP=state%SWPEXP
!    T0=state%T0
!    THETAM=state%THETAM
!    THROUGHFALL=state%THROUGHFALL
    TINC=state%TINC
!    TLEAF=state%TLEAF
    TOTC=state%TOTC
!    TOTESTEVAPMM=state%TOTESTEVAPMM
!    TBELOW=state%TBELOW
!    TDIFF=state%TDIFF
!    TFALLTOT=state%TFALLTOT
!    THETA=state%THETA
!    TOTRESPRG=state%TOTRESPRG
!    TOTRESPBG=state%TOTRESPBG
!    CTITLE=state%CTITLE
!    TTITLE=state%TTITLE
!    PTITLE=state%PTITLE
!    STITLE=state%STITLE
!    WTITLE=state%WTITLE
!    UTITLE=state%UTITLE
    VTITLE=state%VTITLE
!    MTITLE=state%MTITLE
    in_path=state%in_path
    out_path=state%out_path
!    
!    ! Outputs for PAR histogram
!    HISTO=state%HISTO
!   
!    ! mgdk...
    NSPECIES=state%NSPECIES
!
    WINDOTC=state%WINDOTC
    XSLOPE=state%XSLOPE
    YSLOPE=state%YSLOPE
    X0=state%X0
    Y0=state%Y0
    XMAX=state%XMAX
    YMAX=state%YMAX
    ZHT=state%ZHT
    Z0HT=state%Z0HT
    ZPD=state%ZPD
!    TORTPAR=state%TORTPAR
!    TTIMD=state%TTIMD
!    TVJUPU=state%TVJUPU
!    TVJDNU=state%TVJDNU
!    VCMAXN25=state%VCMAXN25
!    UNMIN=state%UNMIN
!    VCMAX25M=state%VCMAX25M
!    WSOIL=state%WSOIL
!    WSOILROOT=state%WSOILROOT
!    WSOILMEAN=state%WSOILMEAN
!    WSOILROOTMEAN=state%WSOILROOTMEAN
!    SWPMEAN=state%SWPMEAN
!    TOTTMP=state%TOTTMP
!    TOTLAI=state%TOTLAI
!    WINTERC=state%WINTERC
!    TVJUP=state%TVJUP
!    TVJDN=state%TVJDN
!    VK1=state%VK1
!    VK2=state%VK2
!    VPD1=state%VPD1
!    VPD2=state%VPD2
!    VMFD0=state%VMFD0
!    TREF=state%TREF
!    WC1=state%WC1
!    WC2=state%WC2
!    WLEAF=state%WLEAF
!    WBIOM=state%WBIOM
!    WBINC=state%WBINC
!    WEIGHTEDSWP=state%WEIGHTEDSWP
!    TSCAT=state%TSCAT
!    FRACAPAR=state%FRACAPAR
!    VIEWFACTOR=state%VIEWFACTOR
!    TOTRESPWG=state%TOTRESPWG
!    TOTRESPFRG=state%TOTRESPFRG
!    TOTRESPCRG=state%TOTRESPCRG
!    TOTRESPFG=state%TOTRESPFG
!    TOTSOILRES=state%TOTSOILRES
!    MINLEAFWP=state%MINLEAFWP
!    TMP=state%TMP
!    CI=state%CI
!
!
    G0SPEC=state%G0SPEC
    G1SPEC=state%G1SPEC
    GKSPEC=state%GKSPEC
!    GK=state%GK
!    XLP=state%XLP
!    YLP=state%YLP
!    ZLP=state%ZLP
!    LAYERP=state%LAYERP
!    MLAYERP=state%MLAYERP
!    RELDFP=state%RELDFP
!    TUP=state%TUP
!    TDP=state%TDP
!    
!    EFFK=state%EFFK
!    TOTESTEVAP=state%TOTESTEVAP
!    LAITHROUGHF=state%LAITHROUGHF
!    PSILCAN=state%PSILCAN
!    CICAN=state%CICAN
!    PSILCANMIN=state%PSILCANMIN
!    ECANMAX=state%ECANMAX
!    ACANMAX=state%ACANMAX
!    
    VPARASPEC=state%VPARASPEC
    VPARBSPEC=state%VPARBSPEC
    VPARCSPEC=state%VPARCSPEC
!    VPARA=state%VPARA
!    VPARB=state%VPARB
!    VPARC=state%VPARC
!    VFUN=state%VFUN
    VFUNSPEC=state%VFUNSPEC

    IOHRLY=state%IOHRLY
    IOTUTD=state%IOTUTD   
    IOHIST=state%IOHIST    
    IORESP=state%IORESP    
    IODAILY=state%IODAILY    
    IOWATBAL=state%IOWATBAL  
    IOFORMAT=state%IOFORMAT 
   
    ISUNLA=state%ISUNLA
    ITERMAX=state%ITERMAX 
    BEAR=state%BEAR 
    PLOTAREA=state%PLOTAREA
    NOALLTREES=state%NOALLTREES
    NOTDATES=state%NOTDATES
    
    RXTABLE1=state%RXTABLE1
    RYTABLE1=state%RYTABLE1
    RZTABLE1=state%RZTABLE1
    ZBCTABLE1=state%ZBCTABLE1
    FOLTABLE1=state%FOLTABLE1
    DIAMTABLE1=state%DIAMTABLE1
    

END  SUBROUTINE UpdateMaespaConfigFromState     























































   SUBROUTINE SaveMaespaTreeConfigState(state,&
                        tmpXSLOPE,tmpYSLOPE,tmpBEAR,tmpX0,tmpY0,tmpXMAX,tmpYMAX,tmpPLOTAREA,tmpSTOCKING,   &
                        tmpZHT,tmpZ0HT,tmpZPD,                                           &
                        tmpNOALLTREES,tmpNOTREES,tmpNOTARGETS,tmpITARGETS,tmpSHADEHT,          &
                        tmpNOXDATES,tmpNOYDATES,tmpNOZDATES,tmpNOTDATES,tmpNOLADATES,tmpNODDATES, &
                        tmpDATESX,tmpDATESY,tmpDATESZ,tmpDATEST,tmpDATESLA,tmpDATESD,             &
                        tmpDX,tmpDY,tmpDZ,tmpR1,tmpR2,tmpR3,tmpTRUNK,tmpFLT,tmpTOTLAITABLE,tmpDIAMA,          &
                        tmpIFLUSH,tmpDT1,tmpDT2,tmpDT3,tmpDT4,tmpEXPTIME,tmpAPP,tmpEXPAN,               &
                        tmpWEIGHTS,tmpNSPECIES,tmpISPECIES)
  
        use MaespaConfigState
!    USE MaespaState
    USE switches
!    USE metcom
!    USE maestcom
!
    USE maindeclarations
!    !use for2r
    
    INTEGER tmpIOERROR
    INTEGER tmpDATESX(maxdate),tmpDATESY(maxdate),tmpDATESZ(maxdate)
    INTEGER tmpDATEST(maxdate),tmpDATESLA(maxdate),tmpDATESD(maxdate)
    INTEGER tmpITARGETS(MAXT),tmpISPECIES(MAXT),tmpIPLOTSHAPE,tmpNOALLTREES
    INTEGER tmpNOXDATES,tmpNOYDATES,tmpNOZDATES,tmpNOTDATES,tmpIFLUSH,tmpNOLADATES
    INTEGER tmpNODDATES,tmpNOTREES,tmpNOTARGETS,tmpNSPECIES
    REAL tmpDX(MAXT),tmpDY(MAXT),tmpDZ(MAXT),tmpWEIGHTS(MAXT), tmpEXPFACTORS(MAXT)
    REAL tmpR1(maxdate,MAXT),tmpR2(maxdate,MAXT),tmpR3(maxdate,MAXT)
    REAL tmpTRUNK(maxdate,MAXT),tmpFLT(maxdate,MAXT),tmpTOTLAITABLE(maxdate)
    REAL tmpDIAMA(maxdate,MAXT),tmpPLOTAREA
    REAL tmpX0,tmpY0,tmpXMAX,tmpYMAX,tmpXSLOPE,tmpYSLOPE,tmpBEAR,tmpSHADEHT,tmpSTOCKING
    REAL tmpZHT,tmpZ0HT,tmpZPD,tmpDT1,tmpDT2,tmpDT3,tmpDT4,tmpEXPTIME,tmpAPP,tmpEXPAN
    
    
!    INTEGER tmpITARGETS(MAXT),tmpISPECIES(MAXT)
!    REAL tmpTOTLAITABLE(maxdate)
!    INTEGER tmpDATESX(maxdate),tmpDATESY(maxdate),tmpDATESZ(maxdate)
!    INTEGER tmpDATESLA(maxdate),tmpDATESD(maxdate)
!    REAL tmpWEIGHTS,tmpAPP,tmpEXPAN
    
    TYPE(maespaConfigvariablesstate), intent(OUT) :: state
    

    

    
    
!    state%ABSRPSPEC=ABSRPSPEC
!    state%AJQTABLESPEC=AJQTABLESPEC
!    state%ALPHASPEC=tmpALPHASPEC
    state%APP=tmpAPP
!    state%ARHOSPEC=ARHOSPEC
!    state%ATAUSPEC=ATAUSPEC
!    state%AZ=tmpAZ
!    state%BCOEFFTSPEC=tmpBCOEFFTSPEC
    state%BEAR=tmpBEAR 
!    state%BEXPONTSPEC=tmpBEXPONTSPEC
!    state%BEXT=BEXT
!    state%BEXTANGSPEC=tmpBEXTANGSPEC
!    state%BEXTSPEC=tmpBEXTSPEC
!    state%BEXTT=tmpBEXTT
!    state%BINSIZE=BINSIZE
!    state%BINTERCSPEC=tmpBINTERCSPEC
!    state%BPTSPEC=tmpBPTSPEC
!    state%BPTT=tmpBPTT
!    state%CO2INC=CO2INC
!    state%COEFFTSPEC=tmpCOEFFTSPEC
!    state%COLLASPEC=COLLASPEC
!    state%COLLKSPEC=COLLKSPEC
!    state%D0LSPEC=D0LSPEC
!    state%D0SPEC=D0SPEC
!    state%DATESASPEC=DATESASPEC
    state%DATESD=tmpDATESD
!    state%DATESFQSPEC=DATESFQSPEC
!    state%DATESGSSPEC=DATESGSSPEC
!    state%DATESJSPEC=DATESJSPEC
    state%DATESLA=tmpDATESLA
!    state%DATESRDSPEC=DATESRDSPEC
!    state%DATESSLASPEC=DATESSLASPEC
    state%DATEST=tmpDATEST
!    state%DATESVSPEC=DATESVSPEC
!    state%DATESWQSPEC=DATESWQSPEC
    state%DATESX=tmpDATESX
    state%DATESY=tmpDATESY
    state%DATESZ=tmpDATESZ
!    state%DAYRESPSPEC=DAYRESPSPEC
!    state%DELSCSPEC=DELSCSPEC
!    state%DELSJSPEC=DELSJSPEC
!    state%DEXTSPEC=tmpDEXTSPEC
!    state%DIAMTABLE1=DIAMTABLE1
!    state%DIFZEN=tmpDIFZEN
!    state%DXT1=tmpDXT1
!    state%DXT=tmpDXT
!    state%DYT1=tmpDYT1
!    state%DYT=tmpDYT
!    state%DZT1=tmpDZT1
!    state%DZT=tmpDZT
     state%DX=tmpDX
     state%DY=tmpDY
     state%DZ=tmpDZ
!    state%EAVCSPEC=EAVCSPEC
!    state%EAVJSPEC=EAVJSPEC
!    state%EDVCSPEC=EDVCSPEC
!    state%EDVJSPEC=EDVJSPEC
!    state%EFFYRFSPEC=EFFYRFSPEC
!    state%EFFYRWSPEC=EFFYRWSPEC
    state%EXPAN=tmpEXPAN
!    state%EXPONTSPEC=tmpEXPONTSPEC
    state%EXPTIME=tmpEXPTIME
!    state%EXTWINDSPEC=tmpEXTWINDSPEC
!    state%FALPHASPEC=tmpFALPHASPEC
!    state%FBEAM=tmpFBEAM
!    state%FBEAMOTC=FBEAMOTC
!    state%FOLT=tmpFOLT
!    state%FOLTABLE1=FOLTABLE1
    state%FLT=tmpFLT
!    state%G0SPEC=G0SPEC
!    state%G0TABLESPEC=G0TABLESPEC
!    state%G1SPEC=G1SPEC
!    state%G1TABLESPEC=G1TABLESPEC
!    state%GAMMASPEC=GAMMASPEC
!    state%GKSPEC=GKSPEC
!    state%GSJASPEC=GSJASPEC
!    state%GSJBSPEC=GSJBSPEC
!    state%GSMINSPEC=GSMINSPEC
!    state%GSREFSPEC=GSREFSPEC
!    state%ICC=ICC
!    state%IECOSPEC=IECOSPEC
!    state%IEND=IEND
    state%IFLUSH=tmpIFLUSH
!    state%IODAILY=IODAILY    
!    state%IOFORMAT=IOFORMAT 
!    state%IOHIST=IOHIST    
!    state%IOHRLY=IOHRLY
!    state%IORESP=IORESP    
!    state%IOTC=IOTC
!    state%IOTUTD=IOTUTD   
!    state%IOWATBAL=IOWATBAL  
!    state%IPROG=IPROG
!    state%IPROGUS=IPROGUS
!    state%ISIMUS=ISIMUS
!    state%ISPEC=ISPEC
    state%ISPECIES=tmpISPECIES
!    state%ISPECIEST=tmpISPECIEST
!    state%ISPECIESTUS=tmpISPECIESTUS
!    state%ISTART=ISTART
!    state%ISUNLA=ISUNLA
    state%ITARGETS=tmpITARGETS
!    state%ITERMAX=ITERMAX 
!    state%IUSTFILE=IUSTFILE
!    state%JLEAFSPEC=tmpJLEAFSPEC
!    state%JLEAFT=tmpJLEAFT
!    state%JMAXTABLESPEC=JMAXTABLESPEC
!    state%JSHAPESPEC=tmpJSHAPESPEC
!    state%JSHAPET=tmpJSHAPET
!    state%K10FSPEC=K10FSPEC
!    state%KEEPZEN=KEEPZEN
!    state%LEAFNSPEC=LEAFNSPEC
!    state%MODELGS=MODELGS
!    state%MODELJM=MODELJM
!    state%MODELRD=MODELRD
!    state%MODELRW=MODELRW
!    state%MODELSS=MODELSS
!    state%NALPHASPEC=tmpNALPHASPEC
!    state%NAZ=NAZ
!    state%NOADATESSPEC=NOADATESSPEC
!    state%NOAGECSPEC=tmpNOAGECSPEC
!    state%NOAGECT=tmpNOAGECT
!    state%NOAGEPSPEC=NOAGEPSPEC
    state%NOALLTREES=tmpNOALLTREES 
    state%NODDATES=tmpNODDATES
!    state%NOFQDATESSPEC=NOFQDATESSPEC
!    state%NOGSDATESSPEC=NOGSDATESSPEC
!    state%NOJDATESSPEC=NOJDATESSPEC
    state%NOLADATES=tmpNOLADATES
!    state%NOLAY=NOLAY
!    state%NORDATESSPEC=NORDATESSPEC
!    state%NOSLADATESSPEC=NOSLADATESSPEC
    state%NOTARGETS=tmpNOTARGETS
    state%NOTDATES=NOTDATES 
    state%NOTREES=tmpNOTREES
!    state%NOVDATESSPEC=NOVDATESSPEC
!    state%NOWQDATESSPEC=NOWQDATESSPEC
    state%NOXDATES=tmpNOXDATES
    state%NOYDATES=tmpNOYDATES
    state%NOZDATES=tmpNOZDATES
!    state%NSIDESSPEC=NSIDESSPEC
!    state%NSPECIES=NSPECIES
!    state%NSTEP=NSTEP
!    state%NUMPNT=NUMPNT
!    state%NZEN=NZEN
!    state%PAR0SPEC=PAR0SPEC
!    state%PAROTC=PAROTC
!    state%PHYFILES=tmpPHYFILES
    state%PLOTAREA=PLOTAREA 
!    state%PPLAY=tmpPPLAY
!    state%PROPCSPEC=PROPCSPEC
!    state%PROPCT=PROPCT
!    state%PROPPSPEC=PROPPSPEC
!    state%PSIVSPEC=PSIVSPEC
!    state%Q10BSPEC=Q10BSPEC
!    state%Q10FTABLESPEC=Q10FTABLESPEC
!    state%Q10RSPEC=Q10RSPEC
!    state%Q10WTABLESPEC=Q10WTABLESPEC
!    state%RANDOMSPEC=tmpRANDOMSPEC
!    state%RDTABLESPEC=RDTABLESPEC
!    state%RHOSOLSPEC=RHOSOLSPEC
!    state%RMBSPEC=RMBSPEC
!    state%RMCRSPEC=RMCRSPEC
!    state%RMFRSPEC=RMFRSPEC
!    state%RMWAREASPEC=RMWAREASPEC
!    state%RMWSPEC=RMWSPEC
!    state%RTEMPBSPEC=RTEMPBSPEC
!    state%RTEMPRSPEC=RTEMPRSPEC
!    state%RTEMPSPEC=RTEMPSPEC
!    state%RTEMPWSPEC=RTEMPWSPEC
    state%R1=tmpR1
    state%R2=tmpR2
    state%R3=tmpR3
!    state%RX=tmpRX
!    state%RXTABLE1=RXTABLE1
!    state%RY=tmpRY
!    state%RYTABLE1=RYTABLE1
!    state%RZ=tmpRZ
!    state%RZTABLE1=RZTABLE1
!    state%SFSPEC=SFSPEC
    state%SHADEHT=tmpSHADEHT
!    state%SHAPESPEC=tmpSHAPESPEC
!    state%SHAPET=tmpSHAPET
!    state%SLATABLESPEC=SLATABLESPEC
!    state%SMD1SPEC=SMD1SPEC
!    state%SMD2SPEC=SMD2SPEC
!    state%SOILDATA=tmpSOILDATA
!    state%SPECIESNAMES=tmpSPECIESNAMES
!    state%STEMFORMSPEC=STEMFORMSPEC
!    state%STEMSDWSPEC=STEMSDWSPEC
    state%STOCKING=tmpSTOCKING
!    state%STRFILES=tmpSTRFILES
!    state%SUNLA=SUNLA
!    state%SWPEXPSPEC=SWPEXPSPEC
!    state%T0SPEC=T0SPEC
!    state%TBELOWSPEC=TBELOWSPEC
!    state%THETASPEC=THETASPEC
!    state%TINC=TINC
!    state%TMAXSPEC=TMAXSPEC
!    state%TOTC=TOTC
    state%TOTLAITABLE=tmpTOTLAITABLE
!    state%TREFSPEC=TREFSPEC
    state%TRUNK=tmpTRUNK
!    state%TVJDNSPEC=TVJDNSPEC
!    state%TVJUPSPEC=TVJUPSPEC
!    state%USEMEASSW=tmpUSEMEASSW
!    state%VCMAXTABLESPEC=VCMAXTABLESPEC
!    state%VFUNSPEC=VFUNSPEC
!    state%VK1SPEC=VK1SPEC
!    state%VK2SPEC=VK2SPEC
!    state%VMFD0SPEC=VMFD0SPEC
!    state%VPARASPEC=VPARASPEC
!    state%VPARBSPEC=VPARBSPEC
!    state%VPARCSPEC=VPARCSPEC
!    state%VPD1SPEC=VPD1SPEC
!    state%VPD2SPEC=VPD2SPEC
!    state%VPDMINSPEC=tmpVPDMINSPEC
!    state%VTITLE=VTITLE
!    state%WC1SPEC=WC1SPEC
!    state%WC2SPEC=WC2SPEC
    state%WEIGHTS=tmpWEIGHTS
!    state%WINDOTC=WINDOTC
!    state%WINTERCSPEC=tmpWINTERCSPEC
!    state%WLEAFSPEC=WLEAFSPEC
    state%X0=tmpX0
!    state%XL=tmpXL
    state%XMAX=tmpXMAX
    state%XSLOPE=tmpXSLOPE
    state%Y0=tmpY0
!    state%YL=tmpYL
    state%YMAX=tmpYMAX
    state%YSLOPE=tmpYSLOPE
    state%Z0HT=tmpZ0HT
!    state%ZBC=tmpZBC
!    state%ZBCTABLE1=ZBCTABLE1
!    state%ZEN=tmpZEN
    state%ZHT=tmpZHT
!    state%ZL=tmpZL
    state%ZPD=tmpZPD
!    state%in_path=in_path
!    state%out_path=out_path
    
    

!    state%ABSRP=ABSRP
!    state%ABSRPU=ABSRPU
!    state%ACANMAX=ACANMAX
!    state%AJQ=AJQ
!    state%AJQTABLE=AJQTABLE
!    state%AJQU=AJQU
!    state%ALAT=ALAT
!    state%ALEAF=ALEAF
!    state%ALPHA=ALPHA
!    state%ANIR=ANIR
!    state%APAR=APAR
!    state%APARUS=APARUS
!    state%AREA=AREA
!    state%AREAUS=AREAUS
!    state%ARHO=ARHO
!    state%ATAU=ATAU
!    state%ATHR=ATHR
!    state%AX=AX
!    state%AY=AY
!    state%AZ0=AZ0
!    state%BALPHA=BALPHA
!    state%BBINC=BBINC
!    state%BBIOM=BBIOM
!    state%BCOEFFT=BCOEFFT
!    state%BEAMP=BEAMP
!    state%BEAR=BEAR
!    state%BEXPONT=BEXPONT
!    state%BEXTANG=BEXTANG
!    state%BEXTANGT=BEXTANGT
!    state%BEXTANGTUS=BEXTANGTUS
!    state%BEXTTUS=BEXTTUS
!    state%BEXTUS=BEXTUS
!    state%BFLUX=BFLUX
!    state%BINTERC=BINTERC
!    state%BLAMBDA=BLAMBDA
!    state%BMULT=BMULT
!    state%BPAR=BPAR
!    state%BPT=BPT
!    state%BPTTUS=BPTTUS
!    state%CA=CA
!    state%CAK=CAK
!    state%CANOPYDIMS=CANOPYDIMS
!    state%CANOPY_STORE=CANOPY_STORE
!    state%CI=CI
!    state%CICAN=CICAN
!    state%CICARAT=CICARAT
!    state%COEFFT=COEFFT
!    state%COLLA=COLLA
!    state%COLLK=COLLK
!    state%CTITLE=CTITLE
!    state%D0=D0
!    state%D0L=D0L
!    state%DATESA=DATESA
!    state%DATESFQ=DATESFQ
!    state%DATESFU=DATESFU
!    state%DATESGS=DATESGS
!    state%DATESHU=DATESHU
!    state%DATESJ=DATESJ
!    state%DATESNU=DATESNU
!    state%DATESRD=DATESRD
!    state%DATESSLA=DATESSLA
!    state%DATESV=DATESV
!    state%DATESWQ=DATESWQ
!    state%DAYL0=DAYL0
!    state%DAYL=DAYL
!    state%DAYRESP=DAYRESP
!    state%DEC0=DEC0
!    state%DEC=DEC
!    state%DELSC=DELSC
!    state%DELSCU=DELSCU
!    state%DELSJ=DELSJ
!    state%DELSJU=DELSJU
!    state%DELTAT=DELTAT
!    state%DEXT=DEXT
!    state%DEXTT=DEXTT
!    state%DEXTTUS=DEXTTUS
!    state%DFLUX=DFLUX
    state%DIAMA=tmpDIAMA
!    state%DIAMTABLE1=DIAMTABLE1
!    state%DIAMTABLE=DIAMTABLE
!    state%DIAMTABLEUS=DIAMTABLEUS
!    state%DIFDN=DIFDN
!    state%DIFSKY=DIFSKY
!    state%DIFUP=DIFUP
!    state%DISCHARGE=DISCHARGE
!    state%DISCHARGETOT=DISCHARGETOT
!    state%DLAI=DLAI
!    state%DLI=DLI
!    state%DLT=DLT
!    state%DMULT2=DMULT2
!    state%DOWNTH=DOWNTH
!    state%DOWNTHTREE=DOWNTHTREE
!    state%DRAINLIMIT=DRAINLIMIT
!    state%DRAINSTORE=DRAINSTORE
!    state%DRYTHICK=DRYTHICK
!    state%DRYTHICKMIN=DRYTHICKMIN
    state%DT1=tmpDT1
    state%DT2=tmpDT2
    state%DT3=tmpDT3
    state%DT4=tmpDT4
!    state%DVJU=DVJU
!    state%DXTUS=DXTUS
!    state%DYTUS=DYTUS
!    state%DZTUS=DZTUS
!    state%EAVC=EAVC
!    state%EAVCU=EAVCU
!    state%EAVJ=EAVJ
!    state%EAVJU=EAVJU
!    state%ECANMAX=ECANMAX
!    state%EDVC=EDVC
!    state%EDVCU=EDVCU
!    state%EDVJ=EDVJ
!    state%EDVJU=EDVJU
!    state%EFFK=EFFK
!    state%EFFY=EFFY
!    state%EFFYRF=EFFYRF
!    state%EFFYRW=EFFYRW
!    state%EMAXLEAF=EMAXLEAF
!    state%EQNTIM0=EQNTIM0
!    state%EQNTIM=EQNTIM
!    state%EQUALUPTAKE=EQUALUPTAKE
!    state%ESOIL=ESOIL
!    state%ET=ET
!    state%ETEST=ETEST
!    state%ETMEAS=ETMEAS
!    state%ETMEASTOT=ETMEASTOT
!    state%ETMM2=ETMM2
!    state%ETMM=ETMM
!    state%ETMMTOT=ETMMTOT
!    state%ETUS=ETUS
!    state%ETUSMM=ETUSMM
!    state%EVAPSTORE=EVAPSTORE
!    state%EXPDIF=EXPDIF
!    state%EXPFACTORS=EXPFACTORS
!    state%EXPINF=EXPINF
!    state%EXPONT=EXPONT
!    state%EXTKUS=EXTKUS
!    state%EXTWIND=EXTWIND
!    state%FALPHA=FALPHA
!    state%FAREA=FAREA
!    state%FBINC=FBINC
!    state%FBIOM=FBIOM
!    state%FCO2=FCO2
!    state%FCO2US=FCO2US
!    state%FH2O=FH2O
!    state%FH2OCAN=FH2OCAN
!    state%FH2OUS=FH2OUS
!    state%FHEAT=FHEAT
!    state%FN0US=FN0US
!    state%FOLLAY2=FOLLAY2
!    state%FOLLAY=FOLLAY
!    state%FOLNUS=FOLNUS
!    state%FOLTABLE1=FOLTABLE1
!    state%FOLTABLE=FOLTABLE
!    state%FOLTABLEUS=FOLTABLEUS
!    state%FOLTUS=FOLTUS
!    state%FRACAPAR=FRACAPAR
!    state%FRACORGANIC=FRACORGANIC
!    state%FRACROOT=FRACROOT
!    state%FRACUPTAKE=FRACUPTAKE
!    state%FRACWATER=FRACWATER
!    state%FRESPB=FRESPB
!    state%FRESPCR=FRESPCR
!    state%FRESPF=FRESPF
!    state%FRESPFR=FRESPFR
!    state%FRESPW=FRESPW
!    state%FRFRAC=FRFRAC
!    state%FRFRACSPEC=FRFRACSPEC
!    state%FSOIL1=FSOIL1
!    state%FSOIL=FSOIL
!    state%FSOILMEAN=FSOILMEAN
!    state%FTSOIL1=FTSOIL1
!    state%FUS=FUS
!    state%G0=G0
!    state%G0TABLE=G0TABLE
!    state%G1=G1
!    state%G1TABLE=G1TABLE
!    state%GAMMA=GAMMA
!    state%GAMSOIL=GAMSOIL
!    state%GBH=GBH
!    state%GBHCAN=GBHCAN
!    state%GK=GK
!    state%GRDAREAI=GRDAREAI
!    state%GSBG0U=GSBG0U
!    state%GSBG1U=GSBG1U
!    state%GSC=GSC
!    state%GSCAN=GSCAN
!    state%GSIPT=GSIPT
!    state%GSJA=GSJA
!    state%GSJB=GSJB
!    state%GSMIN=GSMIN
!    state%GSREF=GSREF
!    state%HFX=HFX
!    state%HISTO=HISTO
!    state%HMSHAPE=HMSHAPE
!    state%HTUS=HTUS
!    state%I=I
!    state%IAGE=IAGE
!    state%ICEPROP=ICEPROP
!    state%IDAY=IDAY
!    state%IECO=IECO
!    state%IECOU=IECOU
!    state%IHOUR=IHOUR
!    state%INITWATER=INITWATER
!    state%IPT=IPT
!    state%IPTUS=IPTUS
!    state%ISUNLIT=ISUNLIT
!    state%IT=IT
!    state%ITAR=ITAR
!    state%ITERMAX=ITERMAX
!    state%ITREE=ITREE
!    state%ITUUS=ITUUS
!    state%IWATFILE=IWATFILE
!    state%IWAVE=IWAVE
!    state%IWHICH=IWHICH
!    state%J=J
!    state%JLEAF=JLEAF
!    state%JLEAFTUS=JLEAFTUS
!    state%JMAX25=JMAX25
!    state%JMAX25M=JMAX25M
!    state%JMAXN25=JMAXN25
!    state%JMAXTABLE=JMAXTABLE
!    state%JSHAPE=JSHAPE
!    state%JSHAPETUS=JSHAPETUS
!    state%K10F=K10F
!    state%K=K
!    state%KEEPWET=KEEPWET
!    state%KSAT=KSAT
!    state%KSCALING=KSCALING
!    state%KTOT=KTOT
!    state%LAITHROUGHF=LAITHROUGHF
!    state%LAYER=LAYER
!    state%LAYERP=LAYERP
!    state%LAYERUS=LAYERUS
!    state%LAYTHICK=LAYTHICK
!    state%LGP2=LGP2
!    state%LGP=LGP
!    state%MASPDATE=MASPDATE
!    state%MAXSTORAGE=MAXSTORAGE
!    state%METCOLS=METCOLS
!    state%MFLAG=MFLAG
!    state%MINLEAFWP=MINLEAFWP
!    state%MINROOTWP=MINROOTWP
!    state%MLAYER=MLAYER
!    state%MLAYERP=MLAYERP
!    state%MLAYERUS= MLAYERUS
!    state%MOSS=MOSS
!    state%MOVEWINDOW=MOVEWINDOW
!    state%MSTART=MSTART
!    state%MTITLE=MTITLE
!    state%NALPHA=NALPHA
!    state%NEWCANOPY=NEWCANOPY
!    state%NEWTUTD=NEWTUTD
!    state%NLAYER=NLAYER
!    state%NOADATES=NOADATES
!    state%NOADTES=NOADTES
!    state%NOAGEC=NOAGEC
!    state%NOAGECTUS=NOAGECTUS
!    state%NOAGEP=NOAGEP
!    state%NOALLTREES=NOALLTREES
!    state%NOFQDATES=NOFQDATES
!    state%NOFUDATES=NOFUDATES
!    state%NOGSDATES=NOGSDATES
!    state%NOHUDATES=NOHUDATES
!    state%NOJDATES=NOJDATES
!    state%NOMETCOLS=NOMETCOLS
!    state%NONUDATES=NONUDATES
!    state%NORDATES=NORDATES
!    state%NOSLADATES=NOSLADATES
!    state%NOTDATES=NOTDATES
!    state%NOUSPOINTS=NOUSPOINTS
!    state%NOVDATES=NOVDATES
!    state%NOWQDATES=NOWQDATES
!    state%NROOTLAYER=NROOTLAYER
!    state%NSIDES=NSIDES
!    state%NSUMMED=NSUMMED
!    state%NSUMMEDW=NSUMMEDW
!    state%NTAIRADD=NTAIRADD
!    state%OUTFLOW=OUTFLOW
!    state%OVERFLOW=OVERFLOW
!    state%PAR0=PAR0
!    state%PAR=PAR
!    state%PARUNDER=PARUNDER
!    state%PARUS=PARUS
!    state%PARUSMEAN=PARUSMEAN
!    state%PARUSSD=PARUSSD
!    state%PLANTK=PLANTK
!    state%PLANTKCR=PLANTKCR
!    state%PLOTAREA=PLOTAREA
!    state%POREFRAC=POREFRAC
!    state%PPAR=PPAR
!    state%PPS=PPS
!    state%PPT=PPT
!    state%PPTDAY=PPTDAY
!    state%PPTGAIN=PPTGAIN
!    state%PPTTOT=PPTTOT
!    state%PRESS=PRESS
!    state%PRESSK=PRESSK
!    state%PREVTSOIL=PREVTSOIL
!    state%PROPC=PROPC
!    state%PROPCTUS=PROPCTUS
!    state%PROPP=PROPP
!    state%PROPPT=PROPPT
!    state%PROPPTUS=PROPPTUS
!    state%PSIE=PSIE
!    state%PSIL=PSIL
!    state%PSILCAN=PSILCAN
!    state%PSILCANMIN=PSILCANMIN
!    state%PSILIN=PSILIN
!    state%PSIV=PSIV
!    state%PSUS=PSUS
!    state%PTITLE=PTITLE
!    state%PTRANSP=PTRANSP
!    state%Q10B=Q10B
!    state%Q10F=Q10F
!    state%Q10FTABLE=Q10FTABLE
!    state%Q10R=Q10R
!    state%Q10W=Q10W
!    state%Q10WTABLE=Q10WTABLE
!    state%QC=QC
!    state%QCTOT=QCTOT
!    state%QE=QE
!    state%QETOT=QETOT
!    state%QH=QH
!    state%QHTOT=QHTOT
!    state%QN=QN
!    state%QNTOT=QNTOT
!    state%RADABV=RADABV
!    state%RADINTERC1=RADINTERC1
!    state%RADINTERC2=RADINTERC2
!    state%RADINTERC3=RADINTERC3
!    state%RADINTERC=RADINTERC
!    state%RADINTERCTOT=RADINTERCTOT
!    state%RANDOM=RANDOM
!    state%RBINC=RBINC
!    state%RBINOM=RBINOM
!    state%RBIOM=RBIOM
!    state%RCOEFFT=RCOEFFT
!    state%RCOEFFTSPEC=RCOEFFTSPEC
!    state%RD0=RD0
!    state%RD0ACC=RD0ACC
!    state%RD0US=RD0US
!    state%RD=RD
!    state%RDK=RDK
!    state%RDT=RDT
!    state%RDTABLE=RDTABLE
!    state%REASSIGNRAIN=REASSIGNRAIN
!    state%RELDF=RELDF
!    state%RELDFAR=RELDFAR
!    state%RELDFP=RELDFP
!    state%RELDFUS=RELDFUS
!    state%RESCALE=RESCALE
!    state%RESPF=RESPF
!    state%RETFUNCTION=RETFUNCTION
!    state%REXPONT=REXPONT
!    state%REXPONTSPEC=REXPONTSPEC
!    state%RGLOBABV12=RGLOBABV12
!    state%RGLOBABV=RGLOBABV
!    state%RGLOBUND=RGLOBUND
!    state%RH=RH
!    state%RHOSOL=RHOSOL
!    state%RINTERC=RINTERC
!    state%RINTERCSPEC=RINTERCSPEC
!    state%RMB=RMB
!    state%RMCR=RMCR
!    state%RMFR=RMFR
!    state%RMW=RMW
!    state%RMWAREA=RMWAREA
!    state%RNET=RNET
!    state%ROOTLEN=ROOTLEN
!    state%ROOTMASS=ROOTMASS
!    state%ROOTRAD=ROOTRAD
!    state%ROOTRESFRAC=ROOTRESFRAC
!    state%ROOTRESIST=ROOTRESIST
!    state%ROOTXSECAREA=ROOTXSECAREA
!    state%RTEMP=RTEMP
!    state%RTEMPB=RTEMPB
!    state%RTEMPR=RTEMPR
!    state%RTEMPW=RTEMPW
!    state%RUNOFF=RUNOFF
!    state%RUTTERB=RUTTERB
!    state%RUTTERD=RUTTERD
!    state%RXTABLE1=RXTABLE1
!    state%RXTABLE=RXTABLE
!    state%RXTABLEUS=RXTABLEUS
!    state%RXUS=RXUS
!    state%RYTABLE1=RYTABLE1
!    state%RYTABLE=RYTABLE
!    state%RYTABLEUS=RYTABLEUS
!    state%RYUS=RYUS
!    state%RZTABLE1=RZTABLE1
!    state%RZTABLE=RZTABLE
!    state%RZTABLEUS=RZTABLEUS
!    state%RZUS=RZUS
!    state%SCATFX=SCATFX
!    state%SCLOST=SCLOST
!    state%SCLOSTTOT=SCLOSTTOT
!    state%SCLOSTTREE=SCLOSTTREE
!    state%SF=SF
!    state%SHAPE=SHAPE
!    state%SHAPETUS=SHAPETUS
!    state%SIMSOILEVAP=SIMSOILEVAP
!    state%SIMTSOIL=SIMTSOIL
!    state%SLA=SLA
!    state%SLATABLE=SLATABLE
!    state%SLAUS=SLAUS
!    state%SMD1=SMD1
!    state%SMD2=SMD2
!    state%SOILCOND=SOILCOND
!    state%SOILDEPTH=SOILDEPTH
!    state%SOILEVAP=SOILEVAP
!    state%SOILEVAPTOT=SOILEVAPTOT
!    state%SOILMOIST=SOILMOIST
!    state%SOILMOISTURE=SOILMOISTURE
!    state%SOILRRES=SOILRRES
!    state%SOILTEMP=SOILTEMP
!    state%SOILTK=SOILTK
!    state%SOILWP=SOILWP
!    state%SOMULT=SOMULT
!    state%STEMFORM=STEMFORM
!    state%STEMSDW=STEMSDW
!    state%STITLE=STITLE
!    state%SUNSET0=SUNSET0
!    state%SUNSET=SUNSET
!    state%SURFACE_WATERMM=SURFACE_WATERMM
!    state%SWMAX=SWMAX
!    state%SWMIN=SWMIN
!    state%SWPEXP=SWPEXP
!    state%SWPMEAN=SWPMEAN
!    state%T0=T0
!    state%TAIR=TAIR
!    state%TAIRMEM=TAIRMEM
!    state%TAIRR=TAIRR
!    state%TARGETFOLS=TARGETFOLS
!    state%TBELOW=TBELOW
!    state%TCAN=TCAN
!    state%TD=TD
!    state%TDAR=TDAR
!    state%TDIFF=TDIFF
!    state%TDP=TDP
!    state%TDUS=TDUS
!    state%TDYAB=TDYAB
!    state%TESTER=TESTER
!    state%TFALLTOT=TFALLTOT
!    state%THERMCOND=THERMCOND
!    state%THETA=THETA
!    state%THETAM=THETAM
!    state%THRAB=THRAB
!    state%THRABUS=THRABUS
!    state%THROUGHFALL=THROUGHFALL
!    state%TLEAF=TLEAF
!    state%TMAX=TMAX
!    state%TMOVE=TMOVE
!    state%TMP=TMP
!    state%TORTPAR=TORTPAR
!    state%TOTCO2=TOTCO2
!    state%TOTESTEVAP=TOTESTEVAP
!    state%TOTESTEVAPMM=TOTESTEVAPMM
!    state%TOTH2O=TOTH2O
!    state%TOTH2OCAN=TOTH2OCAN
!    state%TOTHFX=TOTHFX
!    state%TOTLAI=TOTLAI
!    state%TOTRESPB=TOTRESPB
!    state%TOTRESPBG=TOTRESPBG
!    state%TOTRESPCR=TOTRESPCR
!    state%TOTRESPCRG=TOTRESPCRG
!    state%TOTRESPF=TOTRESPF
!    state%TOTRESPFG=TOTRESPFG
!    state%TOTRESPFR=TOTRESPFR
!    state%TOTRESPFRG=TOTRESPFRG
!    state%TOTRESPRG=TOTRESPRG
!    state%TOTRESPWG=TOTRESPWG
!    state%TOTRESPWM=TOTRESPWM
!    state%TOTSOILRES=TOTSOILRES
!    state%TOTTMP=TOTTMP
!    state%TREF=TREF
!    state%TSCAT=TSCAT
!    state%TSOIL=TSOIL
!    state%TSOILDATA=TSOILDATA
!    state%TTIMD=TTIMD
!    state%TTITLE=TTITLE
!    state%TU=TU
!    state%TUAR=TUAR
!    state%TUP=TUP
!    state%TUUS=TUUS
!    state%TVJDN=TVJDN
!    state%TVJDNU=TVJDNU
!    state%TVJUP=TVJUP
!    state%TVJUPU=TVJUPU
!    state%UIBEAM=UIBEAM
!    state%UIDIFF=UIDIFF
!    state%UNMIN=UNMIN
!    state%USEMEASET=USEMEASET
!    state%USESTAND=USESTAND
!    state%USLAI=USLAI
!    state%USLAITAB=USLAITAB
!    state%UTITLE=UTITLE
!    state%VCMAX25=VCMAX25
!    state%VCMAX25M=VCMAX25M
!    state%VCMAXN25=VCMAXN25
!    state%VCMAXTABLE=VCMAXTABLE
!    state%VFUN=VFUN
!    state%VIEWFACTOR=VIEWFACTOR
!    state%VK1=VK1
!    state%VK2=VK2
!    state%VL2=VL2
!    state%VL=VL
!    state%VMFD0=VMFD0
!    state%VMFD=VMFD
!    state%VPARA=VPARA
!    state%VPARB=VPARB
!    state%VPARC=VPARC
!    state%VPD1=VPD1
!    state%VPD2=VPD2
!    state%VPD=VPD
!    state%VPDMIN=VPDMIN
!    state%WATERGAIN=WATERGAIN
!    state%WATERLOSS=WATERLOSS
!    state%WBINC=WBINC
!    state%WBIOM=WBIOM
!    state%WC1=WC1
!    state%WC2=WC2
!    state%WEIGHTEDSWP=WEIGHTEDSWP
!    state%WETTINGBOT=WETTINGBOT
!    state%WETTINGTOP=WETTINGTOP
!    state%WINDAH=WINDAH
!    state%WINDLAY=WINDLAY
!    state%WINTERC=WINTERC
!    state%WLEAF=WLEAF
!    state%WSOIL=WSOIL
!    state%WSOILMEAN=WSOILMEAN
!    state%WSOILMETHOD=WSOILMETHOD
!    state%WSOILROOT=WSOILROOT
!    state%WSOILROOTMEAN=WSOILROOTMEAN
!    state%WTITLE=WTITLE
!    state%XL2=XL2
!    state%XLP=XLP
!    state%XLU=XLU
!    state%YL2=YL2
!    state%YLP=YLP
!    state%YLU=YLU
!    state%ZBCTABLE1=ZBCTABLE1
!    state%ZBCTABLE=ZBCTABLE
!    state%ZBCTABLEUS=ZBCTABLEUS
!    state%ZBCUS=ZBCUS
!    state%ZEN0=ZEN0
!    state%ZL2=ZL2
!    state%ZLP=ZLP
!    state%ZLU=ZLU

    

END  SUBROUTINE SaveMaespaTreeConfigState  


end Module MaespaConfigStateUtils