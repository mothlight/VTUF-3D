!     
! File:   MaespaConfigStateUtils.f90
! Author: kerryn
!
! Created on 28 March 2014, 3:30 PM
!
Module MaespaConfigStateUtils
    contains
    
 function dummy()
     integer x
     
     print *,x
     
 end function dummy
!    
! function constructFilename(prefix, number, suffix)
!        
!    INTEGER number
!    character(len=*) :: prefix
!    character(len=1024) :: suffix
!    character(len=1024) :: filename
!    character(len=1024) :: constructFilename
!    character(len=1024) :: format_string
!    
!    if (number < 10) then
!        format_string = "(A5,I1)"
!    else
!        format_string = "(A5,I2)"
!    endif
!
!    write (filename,format_string) trim(prefix), number
!!    print *, trim(filename)
!    constructFilename = trim(filename)//trim(suffix)
!    return
!    
! end function constructFilename
!    
!    
!    SUBROUTINE SaveMaespaConfigState(state)
!  
!        use MaespaConfigState
!!    USE MaespaState
!    USE switches
!!    USE metcom
!!    USE maestcom
!!
!    USE maindeclarations
!!    !use for2r
!    
!    TYPE(maespaConfigvariablesstate), intent(OUT) :: state
!    
!
!    
!    
!!  ! List of trees for which to do calculations
!    state%ITARGETS=ITARGETS
!    state%ISPECIES=ISPECIES
!    state%ISPECIEST=ISPECIEST
!    state%ISPECIESTUS=ISPECIESTUS
!!
!!    ! Tree positions and dimensions - all trees + all dates
!    state%DXT1=DXT1
!    state%DYT1=DYT1
!    state%DZT1=DZT1
!    state%TOTLAITABLE=TOTLAITABLE
!!    ! Dates for tree dimensions
!    state%DATESX=DATESX
!    state%DATESY=DATESY
!    state%DATESZ=DATESZ
!    state%DATEST=DATEST
!    state%DATESLA=DATESLA
!    state%DATESD=DATESD
!!    ! Tree dimensions on simulation date (by interpolation)
!    state%DXT=DXT
!    state%DYT=DYT
!    state%DZT=DZT
!    state%RX=RX
!    state%RY=RY
!    state%RZ=RZ
!    state%ZBC=ZBC
!    state%FOLT=FOLT
!    state%WEIGHTS=WEIGHTS
!!    ! Positions of grid points + associated volume & leaf area etc
!    state%XL=XL
!    state%YL=YL
!    state%ZL=ZL
!    state%PPLAY=PPLAY
!!
!!    ! Met data
!    state%SOILDATA=SOILDATA
!    state%USEMEASSW=USEMEASSW
!    state%FBEAM=FBEAM
!!
!!    ! Structural data inputs
!!    ! Intermediate calculations
!    state%DIFZEN=DIFZEN
!
!    state%ZEN=ZEN
!    state%AZ=AZ
!
!!
!!    ! Multi-species       
!    state%SPECIESNAMES=SPECIESNAMES
!    state%PHYFILES=PHYFILES
!    state%STRFILES=STRFILES
!!
!!    ! STR arrays + multi-species versions.
!    state%ALPHASPEC=ALPHASPEC
!    state%FALPHASPEC=FALPHASPEC
!    state%BPTSPEC=BPTSPEC
!    state%BPTT=BPTT
!    state%SHAPESPEC=SHAPESPEC
!    state%EXTWINDSPEC=EXTWINDSPEC
!    state%RANDOMSPEC=RANDOMSPEC
!    state%COEFFTSPEC=COEFFTSPEC
!    state%EXPONTSPEC=EXPONTSPEC
!    state%WINTERCSPEC=WINTERCSPEC
!    state%BCOEFFTSPEC=BCOEFFTSPEC
!    state%BEXPONTSPEC=BEXPONTSPEC
!    state%BINTERCSPEC=BINTERCSPEC
!    state%DEXTSPEC=DEXTSPEC
!    state%BEXTSPEC=BEXTSPEC
!    state%BEXTANGSPEC=BEXTANGSPEC
!    state%BEXTT=BEXTT
!    state%NOAGECSPEC=NOAGECSPEC
!    state%NOAGECT=NOAGECT
!    state%JLEAFSPEC=JLEAFSPEC
!    state%JLEAFT=JLEAFT
!    state%JSHAPESPEC=JSHAPESPEC
!    state%NALPHASPEC=NALPHASPEC
!    state%JSHAPET=JSHAPET
!    state%SHAPET=SHAPET
!    state%VPDMINSPEC=VPDMINSPEC
!!
!!    ! PHY arrays + multi-species versions.
!    state%ABSRPSPEC=ABSRPSPEC
!    state%ARHOSPEC=ARHOSPEC
!    state%ATAUSPEC=ATAUSPEC
!    state%RHOSOLSPEC=RHOSOLSPEC
!    state%PROPPSPEC=PROPPSPEC
!    state%PROPCSPEC=PROPCSPEC
!    state%PROPCT=PROPCT
!    state%LEAFNSPEC=LEAFNSPEC
!    state%JMAXTABLESPEC=JMAXTABLESPEC
!    state%VCMAXTABLESPEC=VCMAXTABLESPEC
!    state%RDTABLESPEC=RDTABLESPEC
!    state%SLATABLESPEC=SLATABLESPEC
!    state%AJQTABLESPEC=AJQTABLESPEC
!    state%Q10FTABLESPEC=Q10FTABLESPEC
!    state%Q10WTABLESPEC=Q10WTABLESPEC
!!   
!    state%DATESJSPEC=DATESJSPEC
!    state%DATESVSPEC=DATESVSPEC
!    state%DATESRDSPEC=DATESRDSPEC
!    state%DATESSLASPEC=DATESSLASPEC
!    state%DATESASPEC=DATESASPEC
!    state%DATESFQSPEC=DATESFQSPEC
!    state%DATESWQSPEC=DATESWQSPEC
!    state%NOAGEPSPEC=NOAGEPSPEC
!    state%NSIDESSPEC=NSIDESSPEC
!    state%GSREFSPEC=GSREFSPEC
!    state%GSMINSPEC=GSMINSPEC
!    state%PAR0SPEC=PAR0SPEC
!    state%D0SPEC=D0SPEC
!    state%VK1SPEC=VK1SPEC
!    state%VK2SPEC=VK2SPEC
!    state%VPD1SPEC=VPD1SPEC
!    state%VPD2SPEC=VPD2SPEC
!    state%VMFD0SPEC=VMFD0SPEC
!    state%GSJASPEC=GSJASPEC
!    state%GSJBSPEC=GSJBSPEC
!    state%T0SPEC=T0SPEC
!    state%TREFSPEC=TREFSPEC
!    state%TMAXSPEC=TMAXSPEC
!    state%SMD1SPEC=SMD1SPEC
!    state%SMD2SPEC=SMD2SPEC
!    state%WC1SPEC=WC1SPEC
!    state%WC2SPEC=WC2SPEC
!    state%SWPEXPSPEC=SWPEXPSPEC
!    state%D0LSPEC=D0LSPEC
!    state%GAMMASPEC=GAMMASPEC
!    state%WLEAFSPEC=WLEAFSPEC
!    state%SFSPEC=SFSPEC
!    state%PSIVSPEC=PSIVSPEC
!!   
!    state%NOJDATESSPEC=NOJDATESSPEC
!    state%NOVDATESSPEC=NOVDATESSPEC
!    state%NOADATESSPEC=NOADATESSPEC
!    state%NOSLADATESSPEC=NOSLADATESSPEC
!    state%NORDATESSPEC=NORDATESSPEC
!    state%NOWQDATESSPEC=NOWQDATESSPEC
!    state%NOFQDATESSPEC=NOFQDATESSPEC
!    state%IECOSPEC=IECOSPEC
!    state%EAVJSPEC=EAVJSPEC
!    state%EDVJSPEC=EDVJSPEC
!    state%DELSJSPEC=DELSJSPEC
!    state%EAVCSPEC=EAVCSPEC
!    state%EDVCSPEC=EDVCSPEC
!    state%DELSCSPEC=DELSCSPEC
!    state%TVJUPSPEC=TVJUPSPEC
!    state%TVJDNSPEC=TVJDNSPEC
!    state%THETASPEC=THETASPEC
!    state%RTEMPSPEC=RTEMPSPEC
!    state%DAYRESPSPEC=DAYRESPSPEC
!    state%EFFYRFSPEC=EFFYRFSPEC
!    state%TBELOWSPEC=TBELOWSPEC
!    state%EFFYRWSPEC=EFFYRWSPEC
!    state%RMWSPEC=RMWSPEC
!    state%RTEMPWSPEC=RTEMPWSPEC
!    state%COLLASPEC=COLLASPEC
!    state%COLLKSPEC=COLLKSPEC
!    state%STEMSDWSPEC=STEMSDWSPEC
!    state%RMWAREASPEC=RMWAREASPEC
!    state%STEMFORMSPEC=STEMFORMSPEC
!    state%Q10RSPEC=Q10RSPEC
!    state%RTEMPRSPEC=RTEMPRSPEC
!    state%Q10BSPEC=Q10BSPEC
!    state%RTEMPBSPEC=RTEMPBSPEC
!    state%RMCRSPEC=RMCRSPEC
!    state%RMFRSPEC=RMFRSPEC
!    state%RMBSPEC=RMBSPEC
!    state%K10FSPEC=K10FSPEC
!    state%G0TABLESPEC=G0TABLESPEC
!    state%G1TABLESPEC=G1TABLESPEC
!    state%NOGSDATESSPEC=NOGSDATESSPEC
!    state%DATESGSSPEC=DATESGSSPEC
!    state%APP=APP
!
!    state%BEXT=BEXT
!    state%BINSIZE=BINSIZE
!    state%CO2INC=CO2INC
!    state%KEEPZEN=KEEPZEN
!    state%EXPAN=EXPAN
!    state%EXPTIME=EXPTIME
!    state%FBEAMOTC=FBEAMOTC
!    state%ICC=ICC
!    state%IEND=IEND
!    state%IFLUSH=IFLUSH
!    state%IOTC=IOTC
!    state%IPROG=IPROG
!    state%IPROGUS=IPROGUS
!    state%ISIMUS=ISIMUS
!    state%ISPEC=ISPEC
!    state%ISTART=ISTART
!    state%IUSTFILE=IUSTFILE
!    state%MODELGS=MODELGS
!    state%MODELJM=MODELJM
!    state%MODELRD=MODELRD
!    state%MODELRW=MODELRW
!    state%MODELSS=MODELSS
!    state%NAZ=NAZ
!    state%NODDATES=NODDATES
!    state%NOLADATES=NOLADATES
!    state%NOLAY=NOLAY
!    state%NOTARGETS=NOTARGETS
!    state%NOTREES=NOTREES
!    state%NOXDATES=NOXDATES
!    state%NOYDATES=NOYDATES
!    state%NOZDATES=NOZDATES
!    state%NUMPNT=NUMPNT
!    state%NZEN=NZEN
!    state%NSTEP=NSTEP
!    state%PAROTC=PAROTC
!    state%SHADEHT=SHADEHT
!    state%STOCKING=STOCKING
!    state%SUNLA=SUNLA
!    state%TINC=TINC
!    state%TOTC=TOTC
!    state%VTITLE=VTITLE
!    state%in_path=in_path
!    state%out_path=out_path
!    state%NSPECIES=NSPECIES
!!
!    state%WINDOTC=WINDOTC
!    state%XSLOPE=XSLOPE
!    state%YSLOPE=YSLOPE
!    state%X0=X0
!    state%Y0=Y0
!    state%XMAX=XMAX
!    state%YMAX=YMAX
!    state%ZHT=ZHT
!    state%Z0HT=Z0HT
!    state%ZPD=ZPD
!    state%G0SPEC=G0SPEC
!    state%G1SPEC=G1SPEC
!    state%GKSPEC=GKSPEC 
!    state%VPARASPEC=VPARASPEC
!    state%VPARBSPEC=VPARBSPEC
!    state%VPARCSPEC=VPARCSPEC
!    state%VFUNSPEC=VFUNSPEC
!    
!    !call open_r_file("/home/kerryn/workspace/TUF-3DMaespa/maespaInitValues.rdat", digits=6)
!    !call wrt_r_matrix("bldht", ix=bldht)
!    !call WRT_R_MATRIX_THREE_DIM("surf_shade", l=surf_shade)
!    !call WRT_R_MATRIX_FOUR_DIM("surf", l=surf)
!    !call close_r_file()
!    
!    state%IOHRLY=IOHRLY
!    state%IOTUTD=IOTUTD   
!    state%IOHIST=IOHIST    
!    state%IORESP=IORESP    
!    state%IODAILY=IODAILY    
!    state%IOWATBAL=IOWATBAL  
!    state%IOFORMAT=IOFORMAT 
!    
!    state%ISUNLA=ISUNLA
!    state%ITERMAX=ITERMAX 
!    state%BEAR=BEAR 
!    state%PLOTAREA=PLOTAREA 
!    state%NOALLTREES=NOALLTREES 
!    state%NOTDATES=NOTDATES 
!    
!    state%RXTABLE1=RXTABLE1
!    state%RYTABLE1=RYTABLE1
!    state%RZTABLE1=RZTABLE1
!    state%ZBCTABLE1=ZBCTABLE1
!    state%FOLTABLE1=FOLTABLE1
!    state%DIAMTABLE1=DIAMTABLE1
!    
!
!END  SUBROUTINE SaveMaespaConfigState  

!
!SUBROUTINE UpdateMaespaConfigFromState(state)
!  
!    USE MaespaConfigState
!    USE switches!
!    USE maindeclarations
!    
!    TYPE(maespaConfigvariablesstate), intent(IN) :: state
!
!    ITARGETS=state%ITARGETS
!    ISPECIES=state%ISPECIES
!    ISPECIEST=state%ISPECIEST
!    ISPECIESTUS=state%ISPECIESTUS
!    TOTLAITABLE=state%TOTLAITABLE
!    DATESX=state%DATESX
!    DATESY=state%DATESY
!    DATESZ=state%DATESZ
!    DATEST=state%DATEST
!    DATESLA=state%DATESLA
!    DATESD=state%DATESD
!    DXT1=state%DXT1
!    DYT1=state%DYT1
!    DZT1=state%DZT1
!    RX=state%RX
!    RY=state%RY
!    RZ=state%RZ
!    ZBC=state%ZBC
!    FOLT=state%FOLT
!    WEIGHTS=state%WEIGHTS
!    XL=state%XL
!    YL=state%YL
!    ZL=state%ZL
!    PPLAY=state%PPLAY
!    SOILDATA=state%SOILDATA
!    USEMEASSW=state%USEMEASSW
!    FBEAM=state%FBEAM
!    DIFZEN=state%DIFZEN
!    ZEN=state%ZEN
!    AZ=state%AZ
!!    ! Multi-species       
!    SPECIESNAMES=state%SPECIESNAMES
!    PHYFILES=state%PHYFILES
!    STRFILES=state%STRFILES
!!
!!    ! STR arrays + multi-species versions.
!    ALPHASPEC=state%ALPHASPEC
!    FALPHASPEC=state%FALPHASPEC
!    BPTSPEC=state%BPTSPEC
!    BPTT=state%BPTT
!    SHAPESPEC=state%SHAPESPEC
!    EXTWINDSPEC=state%EXTWINDSPEC
!    RANDOMSPEC=state%RANDOMSPEC
!    COEFFTSPEC=state%COEFFTSPEC
!    EXPONTSPEC=state%EXPONTSPEC
!    WINTERCSPEC=state%WINTERCSPEC
!    BCOEFFTSPEC=state%BCOEFFTSPEC
!    BEXPONTSPEC=state%BEXPONTSPEC
!    BINTERCSPEC=state%BINTERCSPEC
!    DEXTSPEC=state%DEXTSPEC
!    BEXTSPEC=state%BEXTSPEC
!    BEXTANGSPEC=state%BEXTANGSPEC
!    BEXTT=state%BEXTT
!    RCOEFFTSPEC=state%RCOEFFTSPEC
!    REXPONTSPEC=state%REXPONTSPEC
!    RINTERCSPEC=state%RINTERCSPEC
!    FRFRACSPEC=state%FRFRACSPEC
!    NOAGECSPEC=state%NOAGECSPEC
!    NOAGECT=state%NOAGECT
!    JLEAFSPEC=state%JLEAFSPEC
!    JLEAFT=state%JLEAFT
!    JSHAPESPEC=state%JSHAPESPEC
!    NALPHASPEC=state%NALPHASPEC
!    JSHAPET=state%JSHAPET
!    SHAPET=state%SHAPET
!    VPDMINSPEC=state%VPDMINSPEC
!    ABSRPSPEC=state%ABSRPSPEC
!    ARHOSPEC=state%ARHOSPEC
!    ATAUSPEC=state%ATAUSPEC
!    RHOSOLSPEC=state%RHOSOLSPEC
!    PROPPSPEC=state%PROPPSPEC
!    PROPCSPEC=state%PROPCSPEC
!    PROPCT=state%PROPCT
!    LEAFNSPEC=state%LEAFNSPEC
!    JMAXTABLESPEC=state%JMAXTABLESPEC
!    VCMAXTABLESPEC=state%VCMAXTABLESPEC
!    RDTABLESPEC=state%RDTABLESPEC
!    SLATABLESPEC=state%SLATABLESPEC
!    AJQTABLESPEC=state%AJQTABLESPEC
!    Q10FTABLESPEC=state%Q10FTABLESPEC
!    Q10WTABLESPEC=state%Q10WTABLESPEC
!!   
!    DATESJSPEC=state%DATESJSPEC
!    DATESVSPEC=state%DATESVSPEC
!    DATESRDSPEC=state%DATESRDSPEC
!    DATESSLASPEC=state%DATESSLASPEC
!    DATESASPEC=state%DATESASPEC
!    DATESFQSPEC=state%DATESFQSPEC
!    DATESWQSPEC=state%DATESWQSPEC
!    NOAGEPSPEC=state%NOAGEPSPEC
!    NSIDESSPEC=state%NSIDESSPEC
!    GSREFSPEC=state%GSREFSPEC
!    GSMINSPEC=state%GSMINSPEC
!    PAR0SPEC=state%PAR0SPEC
!    D0SPEC=state%D0SPEC
!    VK1SPEC=state%VK1SPEC
!    VK2SPEC=state%VK2SPEC
!    VPD1SPEC=state%VPD1SPEC
!    VPD2SPEC=state%VPD2SPEC
!    VMFD0SPEC=state%VMFD0SPEC
!    GSJASPEC=state%GSJASPEC
!    GSJBSPEC=state%GSJBSPEC
!    T0SPEC=state%T0SPEC
!    TREFSPEC=state%TREFSPEC
!    TMAXSPEC=state%TMAXSPEC
!    SMD1SPEC=state%SMD1SPEC
!    SMD2SPEC=state%SMD2SPEC
!    WC1SPEC=state%WC1SPEC
!    WC2SPEC=state%WC2SPEC
!    SWPEXPSPEC=state%SWPEXPSPEC
!    D0LSPEC=state%D0LSPEC
!    GAMMASPEC=state%GAMMASPEC
!    WLEAFSPEC=state%WLEAFSPEC
!    SFSPEC=state%SFSPEC
!    PSIVSPEC=state%PSIVSPEC
!!   
!    NOJDATESSPEC=state%NOJDATESSPEC
!    NOVDATESSPEC=state%NOVDATESSPEC
!    NOADATESSPEC=state%NOADATESSPEC
!    NOSLADATESSPEC=state%NOSLADATESSPEC
!    NORDATESSPEC=state%NORDATESSPEC
!    NOWQDATESSPEC=state%NOWQDATESSPEC
!    NOFQDATESSPEC=state%NOFQDATESSPEC
!    IECOSPEC=state%IECOSPEC
!    EAVJSPEC=state%EAVJSPEC
!    EDVJSPEC=state%EDVJSPEC
!    DELSJSPEC=state%DELSJSPEC
!    EAVCSPEC=state%EAVCSPEC
!    EDVCSPEC=state%EDVCSPEC
!    DELSCSPEC=state%DELSCSPEC
!    TVJUPSPEC=state%TVJUPSPEC
!    TVJDNSPEC=state%TVJDNSPEC
!    THETASPEC=state%THETASPEC
!    RTEMPSPEC=state%RTEMPSPEC
!    DAYRESPSPEC=state%DAYRESPSPEC
!    EFFYRFSPEC=state%EFFYRFSPEC
!    TBELOWSPEC=state%TBELOWSPEC
!    EFFYRWSPEC=state%EFFYRWSPEC
!    RMWSPEC=state%RMWSPEC
!    RTEMPWSPEC=state%RTEMPWSPEC
!    COLLASPEC=state%COLLASPEC
!    COLLKSPEC=state%COLLKSPEC
!    STEMSDWSPEC=state%STEMSDWSPEC
!    RMWAREASPEC=state%RMWAREASPEC
!    STEMFORMSPEC=state%STEMFORMSPEC
!    Q10RSPEC=state%Q10RSPEC
!    RTEMPRSPEC=state%RTEMPRSPEC
!    Q10BSPEC=state%Q10BSPEC
!    RTEMPBSPEC=state%RTEMPBSPEC
!    RMCRSPEC=state%RMCRSPEC
!    RMFRSPEC=state%RMFRSPEC
!    RMBSPEC=state%RMBSPEC
!    K10FSPEC=state%K10FSPEC
!    G0TABLESPEC=state%G0TABLESPEC
!    G1TABLESPEC=state%G1TABLESPEC
!    NOGSDATESSPEC=state%NOGSDATESSPEC
!    DATESGSSPEC=state%DATESGSSPEC
!    APP=state%APP
!    BEXT=state%BEXT
!    BINSIZE=state%BINSIZE
!    CO2INC=state%CO2INC
!    KEEPZEN=state%KEEPZEN
!    DT1=state%DT1
!    DT2=state%DT2
!    DT3=state%DT3
!    DT4=state%DT4
!    EXPAN=state%EXPAN
!    EXPTIME=state%EXPTIME
!    FBEAMOTC=state%FBEAMOTC
!    ICC=state%ICC
!    IEND=state%IEND
!    IFLUSH=state%IFLUSH
!    IOTC=state%IOTC
!    IPROG=state%IPROG
!    IPROGUS=state%IPROGUS
!    ISIMUS=state%ISIMUS
!    ISPEC=state%ISPEC
!    ISTART=state%ISTART
!    IUSTFILE=state%IUSTFILE
!    MODELGS=state%MODELGS
!    MODELJM=state%MODELJM
!    MODELRD=state%MODELRD
!    MODELRW=state%MODELRW
!    MODELSS=state%MODELSS
!    NAZ=state%NAZ
!    NODDATES=state%NODDATES
!    NOLADATES=state%NOLADATES
!    NOLAY=state%NOLAY
!    NOTARGETS=state%NOTARGETS
!    NOTREES=state%NOTREES
!    NOXDATES=state%NOXDATES
!    NOYDATES=state%NOYDATES
!    NOZDATES=state%NOZDATES
!    NUMPNT=state%NUMPNT
!    NZEN=state%NZEN
!    NSTEP=state%NSTEP
!    PAROTC=state%PAROTC
!    SHADEHT=state%SHADEHT
!    STOCKING=state%STOCKING
!    SUNLA=state%SUNLA
!    TINC=state%TINC
!    TOTC=state%TOTC
!    VTITLE=state%VTITLE
!    in_path=state%in_path
!    out_path=state%out_path
!    NSPECIES=state%NSPECIES
!    WINDOTC=state%WINDOTC
!    XSLOPE=state%XSLOPE
!    YSLOPE=state%YSLOPE
!    X0=state%X0
!    Y0=state%Y0
!    XMAX=state%XMAX
!    YMAX=state%YMAX
!    ZHT=state%ZHT
!    Z0HT=state%Z0HT
!    ZPD=state%ZPD
!    G0SPEC=state%G0SPEC
!    G1SPEC=state%G1SPEC
!    GKSPEC=state%GKSPEC 
!    VPARASPEC=state%VPARASPEC
!    VPARBSPEC=state%VPARBSPEC
!    VPARCSPEC=state%VPARCSPEC
!    VFUNSPEC=state%VFUNSPEC
!
!    IOHRLY=state%IOHRLY
!    IOTUTD=state%IOTUTD   
!    IOHIST=state%IOHIST    
!    IORESP=state%IORESP    
!    IODAILY=state%IODAILY    
!    IOWATBAL=state%IOWATBAL  
!    IOFORMAT=state%IOFORMAT 
!   
!    ISUNLA=state%ISUNLA
!    ITERMAX=state%ITERMAX 
!    BEAR=state%BEAR 
!    PLOTAREA=state%PLOTAREA
!    NOALLTREES=state%NOALLTREES
!    NOTDATES=state%NOTDATES
!    
!    RXTABLE1=state%RXTABLE1
!    RYTABLE1=state%RYTABLE1
!    RZTABLE1=state%RZTABLE1
!    ZBCTABLE1=state%ZBCTABLE1
!    FOLTABLE1=state%FOLTABLE1
!    DIAMTABLE1=state%DIAMTABLE1
!    
!
!END  SUBROUTINE UpdateMaespaConfigFromState     



!
!   SUBROUTINE SaveMaespaTreeConfigState(state,&
!                        tmpXSLOPE,tmpYSLOPE,tmpBEAR,tmpX0,tmpY0,tmpXMAX,tmpYMAX,tmpPLOTAREA,tmpSTOCKING,   &
!                        tmpZHT,tmpZ0HT,tmpZPD,                                           &
!                        tmpNOALLTREES,tmpNOTREES,tmpNOTARGETS,tmpITARGETS,tmpSHADEHT,          &
!                        tmpNOXDATES,tmpNOYDATES,tmpNOZDATES,tmpNOTDATES,tmpNOLADATES,tmpNODDATES, &
!                        tmpDATESX,tmpDATESY,tmpDATESZ,tmpDATEST,tmpDATESLA,tmpDATESD,             &
!                        tmpDX,tmpDY,tmpDZ,tmpR1,tmpR2,tmpR3,tmpTRUNK,tmpFLT,tmpTOTLAITABLE,tmpDIAMA,          &
!                        tmpIFLUSH,tmpDT1,tmpDT2,tmpDT3,tmpDT4,tmpEXPTIME,tmpAPP,tmpEXPAN,               &
!                        tmpWEIGHTS,tmpNSPECIES,tmpISPECIES)
!  
!    use MaespaConfigState
!    USE switches
!    USE maindeclarations
!    
!    !INTEGER tmpIOERROR
!    INTEGER tmpDATESX(maxdate),tmpDATESY(maxdate),tmpDATESZ(maxdate)
!    INTEGER tmpDATEST(maxdate),tmpDATESLA(maxdate),tmpDATESD(maxdate)
!    INTEGER tmpITARGETS(MAXT),tmpISPECIES(MAXT),tmpIPLOTSHAPE,tmpNOALLTREES
!    INTEGER tmpNOXDATES,tmpNOYDATES,tmpNOZDATES,tmpNOTDATES,tmpIFLUSH,tmpNOLADATES
!    INTEGER tmpNODDATES,tmpNOTREES,tmpNOTARGETS,tmpNSPECIES
!    REAL tmpDX(MAXT),tmpDY(MAXT),tmpDZ(MAXT),tmpWEIGHTS(MAXT), tmpEXPFACTORS(MAXT)
!    REAL tmpR1(maxdate,MAXT),tmpR2(maxdate,MAXT),tmpR3(maxdate,MAXT)
!    REAL tmpTRUNK(maxdate,MAXT),tmpFLT(maxdate,MAXT),tmpTOTLAITABLE(maxdate)
!    REAL tmpDIAMA(maxdate,MAXT),tmpPLOTAREA
!    REAL tmpX0,tmpY0,tmpXMAX,tmpYMAX,tmpXSLOPE,tmpYSLOPE,tmpBEAR,tmpSHADEHT,tmpSTOCKING
!    REAL tmpZHT,tmpZ0HT,tmpZPD,tmpDT1,tmpDT2,tmpDT3,tmpDT4,tmpEXPTIME,tmpAPP,tmpEXPAN
!    
!    
!    TYPE(maespaConfigvariablesstate), intent(OUT) :: state
!    
!    state%APP=tmpAPP
!    state%BEAR=tmpBEAR 
!    state%DATESD=tmpDATESD
!    state%DATESLA=tmpDATESLA
!    state%DATEST=tmpDATEST
!    state%DATESX=tmpDATESX
!    state%DATESY=tmpDATESY
!    state%DATESZ=tmpDATESZ
!     state%DX=tmpDX
!     state%DY=tmpDY
!     state%DZ=tmpDZ
!    state%EXPAN=tmpEXPAN
!    state%EXPTIME=tmpEXPTIME
!    state%FLT=tmpFLT
!    state%IFLUSH=tmpIFLUSH
!    state%ISPECIES=tmpISPECIES
!    state%ITARGETS=tmpITARGETS
!    state%NOALLTREES=tmpNOALLTREES 
!    state%NODDATES=tmpNODDATES
!    state%NOLADATES=tmpNOLADATES
!    state%NOTARGETS=tmpNOTARGETS
!    state%NOTDATES=NOTDATES 
!    state%NOTREES=tmpNOTREES
!    state%NOXDATES=tmpNOXDATES
!    state%NOYDATES=tmpNOYDATES
!    state%NOZDATES=tmpNOZDATES
!    state%PLOTAREA=PLOTAREA 
!    state%R1=tmpR1
!    state%R2=tmpR2
!    state%R3=tmpR3
!    state%SHADEHT=tmpSHADEHT
!    state%STOCKING=tmpSTOCKING
!    state%TOTLAITABLE=tmpTOTLAITABLE
!    state%TRUNK=tmpTRUNK
!    state%WEIGHTS=tmpWEIGHTS
!    state%X0=tmpX0
!    state%XMAX=tmpXMAX
!    state%XSLOPE=tmpXSLOPE
!    state%Y0=tmpY0
!    state%YMAX=tmpYMAX
!    state%YSLOPE=tmpYSLOPE
!    state%Z0HT=tmpZ0HT
!    state%ZHT=tmpZHT
!    state%ZPD=tmpZPD
!    state%DIAMA=tmpDIAMA
!    state%DT1=tmpDT1
!    state%DT2=tmpDT2
!    state%DT3=tmpDT3
!    state%DT4=tmpDT4
!
!
!    
!
!END  SUBROUTINE SaveMaespaTreeConfigState  


end Module MaespaConfigStateUtils