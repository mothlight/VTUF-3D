!     
! File:   ReadMaespaConfigs.f90
! Author: kerryn
!
! Created on 14 March 2014, 12:42 PM
!

MODULE ReadMaespaConfigs

    contains
    
  subroutine readMaespaTreeConfig(phyFileNumber, strFileNumber, treeFileNumber, config)
    use MAINDECLARATIONS
    use MaespaConfigState
    use MaespaConfigStateUtils
    !use radn
    
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
    
    character (len=1024)  :: phyFileName
    character (len=1024)  :: strFileName
    character (len=1024)  :: treeFileName
    character (len=1024)  :: suffix
    
    INTEGER phyFileNumber, strFileNumber, treeFileNumber
    TYPE(maespaConfigvariablesstate), intent(OUT) :: config
    
    suffix = '.dat'
    
    phyFileName = trim(constructFilename(trim(in_path)//'phy', phyFileNumber, suffix))
    strFileName = trim(constructFilename(trim(in_path)//'str', strFileNumber, suffix))
    treeFileName = trim(constructFilename(trim(in_path)//'trees', treeFileNumber, suffix))
    
    !print *,phyFileName,strFileName,treeFileName
    
    ! Input file with data on tree position and size
    !OPEN (UTREES, FILE = trim(in_path)//'trees.dat', STATUS='OLD', IOSTAT=IOERROR)
    OPEN (UTREES, FILE = treeFileName, STATUS='OLD', IOSTAT=IOERROR)
    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('ERROR: TREES.DAT DOES NOT EXIST', IFATAL, 0)
    ENDIF
    
        ! Read titles from input files
    !READ (UTREES, 990) TTITLE
    
    ! Read in number of trees & number of target tree
    CALL READPLOT(UTREES, tmpX0, tmpY0, tmpXMAX, tmpYMAX, tmpNOALLTREES,tmpXSLOPE, &
            tmpYSLOPE, tmpBEAR, tmpSHADEHT, tmpSTOCKING, tmpIPLOTSHAPE)
    tmpPLOTAREA = (tmpXMAX - tmpX0) * (tmpYMAX - tmpY0)

    ! Read in aerodynamic properties of canopy
    CALL READZPD(UTREES,tmpZHT,tmpZ0HT,tmpZPD)

    ! Get x, y, z co-ords of each tree
    CALL READXYZ(UTREES,tmpNOALLTREES,tmpX0,tmpY0,tmpXMAX,tmpYMAX,tmpXSLOPE,tmpYSLOPE,tmpDX,tmpDY,tmpDZ)

    ! Get radii in x & y directions of each tree
    CALL READTREEARRAY(UTREES,1,tmpNOALLTREES,tmpNOXDATES,tmpDATESX,tmpR1)
    CALL READTREEARRAY(UTREES,2,tmpNOALLTREES,tmpNOYDATES,tmpDATESY,tmpR2)
    ! Get green crown height of each tree
    CALL READTREEARRAY(UTREES,3,tmpNOALLTREES,tmpNOZDATES,tmpDATESZ,tmpR3)
    ! Get trunk length of each tree
    CALL READTREEARRAY(UTREES,4,tmpNOALLTREES,tmpNOTDATES,tmpDATEST,tmpTRUNK)

    ! Get leaf area parameters
    CALL GETLEAFAREA(UTREES,tmpIFLUSH,tmpDT1,tmpDT2,tmpDT3,tmpDT4,tmpEXPTIME,&
            tmpAPP,tmpEXPAN,tmpNOALLTREES,tmpNOLADATES,tmpDATESLA,tmpFLT)

    ! Get diameter of each tree
    CALL READTREEARRAY(UTREES,6,tmpNOALLTREES,tmpNODDATES,tmpDATESD,tmpDIAMA)

    ! Calculate total LAI
    CALL CALCLAI(tmpNOLADATES,tmpFLT,tmpNOALLTREES,tmpXMAX,tmpYMAX,tmpXSLOPE,tmpYSLOPE,tmpTOTLAITABLE)
!
!    ! Read in how many of the trees form the subplot (from confile)
!    CALL READCONTREES(UCONTROL,NOALLTREES,DX,DY,XMAX,YMAX,NOTREES,NOTARGETS,ITARGETS,IPLOTSHAPE,WEIGHTS)
!       
!    ! Read species array, if provided.
!    CALL READSPECLIST(UTREES, NSPECIES, ISPECIES)
    
!    call INPUTTREE(XSLOPE,YSLOPE,BEAR,X0,Y0,XMAX,YMAX,PLOTAREA,STOCKING,      &
!                        ZHT,Z0HT,ZPD,                                           &
!                        NOALLTREES,NOTREES,NOTARGETS,ITARGETS,SHADEHT,          &
!                        NOXDATES,NOYDATES,NOZDATES,NOTDATES,NOLADATES,NODDATES, &
!                        DATESX,DATESY,DATESZ,DATEST,DATESLA,DATESD,             &
!                        DX,DY,DZ,R1,R2,R3,TRUNK,FLT,TOTLAITABLE,DIAMA,          &
!                        IFLUSH,DT1,DT2,DT3,DT4,EXPTIME,APP,EXPAN,               &
!                        WEIGHTS,NSPECIES,ISPECIES)
    
    call SaveMaespaTreeConfigState(config,&
            tmpXSLOPE,tmpYSLOPE,tmpBEAR,tmpX0,tmpY0,tmpXMAX,tmpYMAX,tmpPLOTAREA,tmpSTOCKING,      &
                        tmpZHT,tmpZ0HT,tmpZPD,                                           &
                        tmpNOALLTREES,tmpNOTREES,tmpNOTARGETS,tmpITARGETS,tmpSHADEHT,          &
                        tmpNOXDATES,tmpNOYDATES,tmpNOZDATES,tmpNOTDATES,tmpNOLADATES,tmpNODDATES, &
                        tmpDATESX,tmpDATESY,tmpDATESZ,tmpDATEST,tmpDATESLA,tmpDATESD,             &
                        tmpDX,tmpDY,tmpDZ,tmpR1,tmpR2,tmpR3,tmpTRUNK,tmpFLT,tmpTOTLAITABLE,tmpDIAMA,          &
                        tmpIFLUSH,tmpDT1,tmpDT2,tmpDT3,tmpDT4,tmpEXPTIME,tmpAPP,tmpEXPAN,               &
                        tmpWEIGHTS,tmpNSPECIES,tmpISPECIES)
    
    CLOSE(UTREES)

    end subroutine readMaespaTreeConfig
    
    !! this version shouldn't need all the initialization first, and no in_path
subroutine readMaespaTreeConfigFromConfig(phyFileNumber, strFileNumber, treeFileNumber, config)
    use MAINDECLARATIONS
    use MaespaConfigState
    use MaespaConfigStateUtils
    !use radn
    
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
    
    character (len=1024)  :: phyFileName
    character (len=1024)  :: strFileName
    character (len=1024)  :: treeFileName
    character (len=1024)  :: suffix
    !CHARACTER(30) STRFILES(MAXSP)
    
    INTEGER phyFileNumber, strFileNumber, treeFileNumber
    TYPE(maespaConfigvariablesstate), intent(OUT) :: config
    INTEGER UTREESFILE
    
    suffix = '.dat'
    
    phyFileName = adjustl(trim(constructFilename('phy', phyFileNumber, suffix)))
    strFileName = adjustl(trim(constructFilename('str', strFileNumber, suffix)))
    treeFileName = adjustl(trim(constructFilename('trees', treeFileNumber, suffix)))
    
!    print *,phyFileName,strFileName,treeFileName
    
    
    !!
 
        ! Get input from canopy structure file
    CALL INPUTSTR(NSPECIES,strFileName,JLEAFSPEC,BPTTABLESPEC,RANDOMSPEC,NOAGECSPEC,    &
                    JSHAPESPEC,SHAPESPEC,EXTWINDSPEC,NALPHASPEC,ALPHASPEC,      &
                    FALPHASPEC,COEFFTSPEC,EXPONTSPEC,WINTERCSPEC,BCOEFFTSPEC,   &
                    BEXPONTSPEC,BINTERCSPEC,RCOEFFTSPEC,REXPONTSPEC,RINTERCSPEC,&
                    FRFRACSPEC,in_path,DATESLIA,NOLIADATES,DATESLAD,NOLADDATES)
    
    ! Get input from physiology file
    CALL INPUTPHY(NSPECIES,phyFileName,MODELJM,MODELRD,MODELGS,MODELRW,NOLAY,NOAGECSPEC,           &
                    NOAGEPSPEC,PROPCSPEC,PROPPSPEC,ABSRPSPEC,ARHOSPEC,ATAUSPEC,RHOSOLSPEC,      &
                    JMAXTABLESPEC,DATESJSPEC,NOJDATESSPEC,IECOSPEC,EAVJSPEC,EDVJSPEC,           &
                    DELSJSPEC,THETASPEC,VCMAXTABLESPEC,DATESVSPEC,NOVDATESSPEC,EAVCSPEC,        &
                    EDVCSPEC,DELSCSPEC,TVJUPSPEC,TVJDNSPEC,SLATABLESPEC,DATESSLASPEC,           &
                    NOSLADATESSPEC,NOADATESSPEC,DATESASPEC,AJQTABLESPEC,RDTABLESPEC,            &
                    DATESRDSPEC,NORDATESSPEC,RTEMPSPEC,DAYRESPSPEC,TBELOWSPEC,EFFYRWSPEC,       &
                    RMWSPEC,RTEMPWSPEC,COLLASPEC,COLLKSPEC,STEMSDWSPEC,RMWAREASPEC,STEMFORMSPEC,&
                    NOFQDATESSPEC,DATESFQSPEC,Q10FTABLESPEC,K10FSPEC,NOWQDATESSPEC,DATESWQSPEC, &
                    Q10WTABLESPEC,RMFRSPEC,RMCRSPEC,Q10RSPEC,RTEMPRSPEC,EFFYRFSPEC,RMBSPEC,     &
                    Q10BSPEC,RTEMPBSPEC,GSREFSPEC,GSMINSPEC,PAR0SPEC,D0SPEC,VK1SPEC,VK2SPEC,    &
                    VPD1SPEC,VPD2SPEC,VMFD0SPEC,GSJASPEC,GSJBSPEC,T0SPEC,TREFSPEC,TMAXSPEC,     &
                    SMD1SPEC,SMD2SPEC,WC1SPEC, WC2SPEC,SWPEXPSPEC,G0TABLESPEC,G1TABLESPEC,      &
                    GKSPEC,NOGSDATESSPEC,DATESGSSPEC,D0LSPEC,GAMMASPEC,VPDMINSPEC,WLEAFTABLESPEC,&
                    DATESWLEAFSPEC,NOWLEAFDATESSPEC,NSIDESSPEC,           &
                    SFSPEC,PSIVSPEC,VPARASPEC,VPARBSPEC,VPARCSPEC,VFUNSPEC,in_path)    
    !!!    
    ! Input file with data on tree position and size
    OPEN (UTREESFILE, FILE = treeFileName, STATUS='OLD', IOSTAT=IOERROR)
    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('ERROR: TREES.DAT DOES NOT EXIST', IFATAL, 0)
    ENDIF
    
    ! Read in number of trees & number of target tree
    CALL READPLOT(UTREESFILE, tmpX0, tmpY0, tmpXMAX, tmpYMAX, tmpNOALLTREES,tmpXSLOPE, &
            tmpYSLOPE, tmpBEAR, tmpSHADEHT, tmpSTOCKING, tmpIPLOTSHAPE)
    tmpPLOTAREA = (tmpXMAX - tmpX0) * (tmpYMAX - tmpY0)

    ! Read in aerodynamic properties of canopy
    CALL READZPD(UTREESFILE,tmpZHT,tmpZ0HT,tmpZPD)

    ! Get x, y, z co-ords of each tree
    CALL READXYZ(UTREESFILE,tmpNOALLTREES,tmpX0,tmpY0,tmpXMAX,tmpYMAX,tmpXSLOPE,tmpYSLOPE,tmpDX,tmpDY,tmpDZ)

    ! Get radii in x & y directions of each tree
    CALL READTREEARRAY(UTREESFILE,1,tmpNOALLTREES,tmpNOXDATES,tmpDATESX,tmpR1)
    CALL READTREEARRAY(UTREESFILE,2,tmpNOALLTREES,tmpNOYDATES,tmpDATESY,tmpR2)
    ! Get green crown height of each tree
    CALL READTREEARRAY(UTREESFILE,3,tmpNOALLTREES,tmpNOZDATES,tmpDATESZ,tmpR3)
    ! Get trunk length of each tree
    CALL READTREEARRAY(UTREESFILE,4,tmpNOALLTREES,tmpNOTDATES,tmpDATEST,tmpTRUNK)

    ! Get leaf area parameters
    CALL GETLEAFAREA(UTREESFILE,tmpIFLUSH,tmpDT1,tmpDT2,tmpDT3,tmpDT4,tmpEXPTIME,&
            tmpAPP,tmpEXPAN,tmpNOALLTREES,tmpNOLADATES,tmpDATESLA,tmpFLT)

    ! Get diameter of each tree
    CALL READTREEARRAY(UTREESFILE,6,tmpNOALLTREES,tmpNODDATES,tmpDATESD,tmpDIAMA)

    ! Calculate total LAI
    CALL CALCLAI(tmpNOLADATES,tmpFLT,tmpNOALLTREES,tmpXMAX,tmpYMAX,tmpXSLOPE,tmpYSLOPE,tmpTOTLAITABLE)
    
    
    ! Read in how many of the trees form the subplot (from confile)
    !CALL READCONTREES(UCONTROL,NOALLTREES,DX,DY,XMAX,YMAX,NOTREES,NOTARGETS,ITARGETS,IPLOTSHAPE,WEIGHTS)
    !   
    ! Read species array, if provided.
    CALL READSPECLIST(UTREESFILE, NSPECIES, ISPECIES)
    
    call INPUTTREENEW(UTREESFILE,tmpXSLOPE,tmpYSLOPE,tmpBEAR,tmpX0,tmpY0,tmpXMAX,tmpYMAX,tmpPLOTAREA,tmpSTOCKING,      &
                        tmpZHT,tmpZ0HT,tmpZPD,                                           &
                        tmpNOALLTREES,tmpNOTREES,tmpNOTARGETS,tmpITARGETS,tmpSHADEHT,          &
                        tmpNOXDATES,tmpNOYDATES,tmpNOZDATES,tmpNOTDATES,tmpNOLADATES,tmpNODDATES, &
                        tmpDATESX,tmpDATESY,tmpDATESZ,tmpDATEST,tmpDATESLA,tmpDATESD,             &
                        tmpDX,tmpDY,tmpDZ,tmpR1,tmpR2,tmpR3,tmpTRUNK,tmpFLT,tmpTOTLAITABLE,tmpDIAMA,          &
                        tmpIFLUSH,tmpDT1,tmpDT2,tmpDT3,tmpDT4,tmpEXPTIME,tmpAPP,tmpEXPAN,               &
                        tmpWEIGHTS,nspecies,tmpISPECIES)

    
    call SaveMaespaTreeConfigState(config,&
            tmpXSLOPE,tmpYSLOPE,tmpBEAR,tmpX0,tmpY0,tmpXMAX,tmpYMAX,tmpPLOTAREA,tmpSTOCKING,      &
                        tmpZHT,tmpZ0HT,tmpZPD,                                           &
                        tmpNOALLTREES,tmpNOTREES,tmpNOTARGETS,tmpITARGETS,tmpSHADEHT,          &
                        tmpNOXDATES,tmpNOYDATES,tmpNOZDATES,tmpNOTDATES,tmpNOLADATES,tmpNODDATES, &
                        tmpDATESX,tmpDATESY,tmpDATESZ,tmpDATEST,tmpDATESLA,tmpDATESD,             &
                        tmpDX,tmpDY,tmpDZ,tmpR1,tmpR2,tmpR3,tmpTRUNK,tmpFLT,tmpTOTLAITABLE,tmpDIAMA,          &
                        tmpIFLUSH,tmpDT1,tmpDT2,tmpDT3,tmpDT4,tmpEXPTIME,tmpAPP,tmpEXPAN,               &
                        tmpWEIGHTS,nspecies,tmpISPECIES)
    config%JSHAPESPEC=JSHAPESPEC
    
    config%NALPHASPEC=NALPHASPEC
    config%ALPHASPEC=ALPHASPEC
    config%FALPHASPEC=FALPHASPEC
    config%RANDOMSPEC=RANDOMSPEC
    
    config%NSPECIES=nspecies
    config%ISPECIES=ISPECIES

    !print *,config%NSPECIES
    !print *,config%ISPECIES
    
    config%DXT1=tmpDX
    config%DYT1=tmpDY
    config%DZT1=tmpDZ
    config%R1=tmpR1
    config%R2=tmpR2
    config%R3=tmpR3
    config%TRUNK=tmpTRUNK
    config%FLT=tmpFLT
    config%DIAMA=tmpDIAMA
    config%JLEAFSPEC=JLEAFSPEC
  
    
    
    config%NSPECIES=NSPECIES
    config%BPT=BPTTABLESPEC
    config%RANDOMSPEC=RANDOMSPEC
    config%NOAGECSPEC=NOAGECSPEC
    config%JSHAPESPEC=JSHAPESPEC
    config%SHAPESPEC=SHAPESPEC
    config%EXTWINDSPEC=EXTWINDSPEC
    config%NALPHASPEC=NALPHASPEC
    config%ALPHASPEC=ALPHASPEC
    config%FALPHASPEC=FALPHASPEC
    config%COEFFTSPEC=COEFFTSPEC
    config%EXPONTSPEC=EXPONTSPEC
    config%WINTERCSPEC=WINTERCSPEC
    config%BCOEFFTSPEC=BCOEFFTSPEC
    config%BEXPONTSPEC=BEXPONTSPEC
    config%BINTERCSPEC=BINTERCSPEC
    config%RCOEFFTSPEC=RCOEFFTSPEC
    config%REXPONTSPEC=REXPONTSPEC
    config%RINTERCSPEC=RINTERCSPEC
    config%FRFRACSPEC=FRFRACSPEC
    config%DATESLIA=DATESLIA
    config%NOLIADATES=NOLIADATES
    config%DATESLAD=DATESLAD
    config%NOLADDATES=NOLADDATES
    
    
    config%NOLAY=NOLAY
    config%PPLAY=PPLAY
    config%PROPCSPEC=PROPCSPEC
    config%PROPPSPEC=PROPPSPEC
    config%NOJDATESSPEC=NOJDATESSPEC
    config%DATESJSPEC=DATESJSPEC
    config%JMAXTABLESPEC=JMAXTABLESPEC
    
    config%VCMAXTABLESPEC=VCMAXTABLESPEC
    config%DATESVSPEC=DATESVSPEC        
    config%NOVDATESSPEC=NOVDATESSPEC
    
    
    config%VCMAXTABLESPEC=VCMAXTABLESPEC
    config%DATESVSPEC=DATESVSPEC   
    config%NOVDATESSPEC=NOVDATESSPEC  
    config%SLATABLESPEC=SLATABLESPEC
    config%DATESSLASPEC=DATESSLASPEC
    config%NOSLADATESSPEC=NOSLADATESSPEC
    config%NOADATESSPEC=NOADATESSPEC
    config%DATESASPEC=DATESASPEC
    config%AJQTABLESPEC=AJQTABLESPEC
    config%RDTABLESPEC=RDTABLESPEC
    config%DATESRDSPEC=DATESRDSPEC
    config%NORDATESSPEC=NORDATESSPEC
    config%NOFQDATESSPEC=NOFQDATESSPEC
    config%DATESFQSPEC=DATESFQSPEC
    config%Q10FTABLESPEC=Q10FTABLESPEC
    config%NOWQDATESSPEC=NOWQDATESSPEC
    config%DATESWQSPEC=DATESWQSPEC
    config%Q10WTABLESPEC=Q10WTABLESPEC
    config%NOAGEPSPEC=NOAGEPSPEC
    config%NOGSDATESSPEC=NOGSDATESSPEC
    config%G0TABLESPEC=G0TABLESPEC
    config%G1TABLESPEC=G1TABLESPEC 
    config%DATESGSSPEC=DATESGSSPEC
    config%NOGSDATESSPEC=NOGSDATESSPEC     
    config%WLEAFTABLESPEC=WLEAFTABLESPEC
    config%DATESWLEAFSPEC=DATESWLEAFSPEC
    config%NOWLEAFDATESSPEC=NOWLEAFDATESSPEC
    
    
    
    config%IFLUSH=tmpIFLUSH
    config%DT1=tmpDT1
    config%DT2=tmpDT2
    config%DT3=tmpDT3
    config%DT4=tmpDT4
    config%EXPTIME=tmpEXPTIME
    config%APP=tmpAPP
    config%EXPAN=tmpEXPAN
    
    
    config%XSLOPE=tmpXSLOPE
    config%YSLOPE=tmpYSLOPE
    
!    print *,config%NOLAY
!    print *,config%PPLAY
!    !print *,config%JLEAF !!
!    print *,config%JLEAFSPEC
!    !print *,config%JSHAPE !!
!    print *,config%JSHAPESPEC
!    !print *,config%SHAPE !!
!    print *,config%SHAPESPEC
!    print *,config%RX(1)
!    print *,config%RY(1)
!    print *,config%RZ(1)
!    print *,config%ZBC(1)
!    print *,config%DXT(1)
!    print *,config%DYT(1)
!    print *,config%DZT(1)
!    print *,config%FOLT(1)
!    !print *,config%PROPC !!
!    print *,config%PROPCSPEC
!    !print *,config%PROPP !!
!    print *,config%PROPPSPEC
!    print *,config%BPT !!
!    print *,config%NOAGECT(1)
!    !print *,config%NOAGEP    !!        
!    print *,config%NOAGEPSPEC
!    print *,config%NOAGECSPEC
    
  
    CLOSE(UTREESFILE)
    
    
    
    DO I=1,NSPECIES
        CALL EXDIFF(NALPHASPEC(I),ALPHASPEC(1:MAXANG,I),FALPHASPEC(1:MAXANG,I),NZEN,DIFZEN,&
                                    RANDOMSPEC(I),DEXTSPEC(I,1:MAXANG))
                                    
        CALL EXDIFF(NALPHASPEC(I),ALPHASPEC(1:MAXANG,I),FALPHASPEC(1:MAXANG,I),&       ! dans la boucle Mathias février 2012
                NZEN,DIFZEN,RANDOMSPEC(I),DEXTSPEC(I,1:MAXANG))
    END DO
    config%DEXTSPEC=DEXTSPEC
!    print *,DEXTSPEC
    
    config%ARHOSPEC=ARHOSPEC
    config%ATAUSPEC=ATAUSPEC
    config%RHOSOLSPEC=RHOSOLSPEC
    config%ABSRPSPEC=ABSRPSPEC
!    print *,ABSRPSPEC
    
    
    CALL RESTARTMETF(IDAY+ISTART,MSTART,MFLAG)
    ! Open met data file (must be done after ISTART & IEND read)
    CALL OPENMETF(ISTART,IEND,CAK,PRESSK,SWMIN,SWMAX,USEMEASET,DIFSKY,ALAT,TTIMD,DELTAT,&
                    MFLAG,METCOLS,NOMETCOLS,MTITLE,MSTART,in_path)  
    ! Calculate zenith angle of sun
    CALL SUN(IDAY+ISTART,ALAT,TTIMD,DEC,EQNTIM,DAYL,SUNSET)
    print *,'ALAT',ALAT
    print *,'TTIMD',TTIMD
    !! ok, see what happens if this is 0
    TTIMD=0
    print *,'BEAR',BEAR
    print *,'DEC',DEC
    print *,'EQNTIM',EQNTIM
  
    CALL ZENAZ(ALAT,TTIMD,BEAR,DEC,EQNTIM,ZEN,AZ)

    
    config%zen=zen
    config%az=az
    config%DEC=DEC
    config%DAYL=DAYL
    config%SWMIN=SWMIN
    config%SWMAX=SWMAX
    
    IF(IPOINTS .EQ. 1)THEN
      CALL GETPOINTSF(NUMTESTPNT,XLP,YLP,ZLP,X0,Y0,XMAX,YMAX, &
         CTITLE,TTITLE,MTITLE,STITLE,VTITLE)
    ENDIF
    config%XLP=XLP
    config%YLP=YLP
    config%ZLP=ZLP
    
    !print *,tmpDATESZ
    
    
        ! Get input from the water balance file
    IF(ISMAESPA)THEN        
        CALL INPUTWATBAL(BPAR, PSIE, KSAT, ROOTRESIST, ROOTRESFRAC, ROOTRADTABLE, ROOTDENSTABLE,ROOTMASSTOTTABLE,              &
                        MINROOTWP,MINLEAFWPSPEC,PLANTKTABLE,KSCALING,THROUGHFALL,REASSIGNRAIN,RUTTERB,RUTTERD, MAXSTORAGE, &
                        DRAINLIMIT,ROOTXSECAREA,EQUALUPTAKE,NLAYER, NROOTLAYER, LAYTHICK, INITWATER,    & 
                        FRACROOTTABLE, POREFRAC, SOILTEMP, KEEPWET,DRYTHICKMIN,TORTPAR, SIMTSOIL,RETFUNCTION,&
                        FRACORGANIC, EXPINF, WSOILMETHOD, USEMEASET,USEMEASSW,SIMSOILEVAP,USESTAND,ALPHARET,WS,WR,NRET,&
                        DATESKP,NOKPDATES,DATESROOT,NOROOTDATES,NOROOTSPEC)
    ENDIF
    config%SOILTEMP=SOILTEMP


 end subroutine readMaespaTreeConfigFromConfig
 
   
subroutine readMaespaTreeMap(state, treeStates)
    use MAINDECLARATIONS
    use MaespaConfigState
    use MaespaConfigStateUtils
    use maestcom2
    !use radn
    
    character(len=1024) :: tmpfilename1
    character(len=1024) :: tmpfilename2
    character(len=1024) :: tmpfilename3
    character(len=1024) :: format_string
    integer loopcount
!    INTEGER UTREESMAP 
    INTEGER numberTreePlots
    
    NAMELIST /COUNT/ numberTreePlots
    
    INTEGER, DIMENSION(:), ALLOCATABLE :: xlocation, ylocation
    INTEGER, DIMENSION(:), ALLOCATABLE :: phyfileNumber,strfileNumber,treesfileNumber, treesHeight
    !INTEGER xlocation(MAXT),ylocation(MAXT)
    NAMELIST /location/   xlocation, ylocation, phyfileNumber, strfileNumber, treesfileNumber, treesHeight
    
    
    !INTEGER phyfileNumber(MAXT),strfileNumber(MAXT),treesfileNumber(MAXT)
    !NAMELIST /files/ phyfileNumber, strfileNumber, treesfileNumber
    
    TYPE(maespaConfigvariablesstate) :: treeState  !! 
    TYPE(maespaConfigTreeMapState), intent(OUT) :: state   !!
    TYPE(maespaConfigvariablesstate) , DIMENSION(:), ALLOCATABLE :: treeStates    !! sub configs to store individual state of trees
     
    format_string = "(A5,I1)"
    
!    print *,in_path
    OPEN (UTREESMAP, FILE = trim(in_path)//'treemap.dat', STATUS='OLD', IOSTAT=IOERROR)
    REWIND (UTREESMAP)
    READ (UTREESMAP, COUNT, IOSTAT = IOERROR)
    IF (IOERROR.NE.0) CALL SUBERROR('ERROR READING count DETAILS',IFATAL,IOERROR)
!    print *,numberTreePlots
    state%numberTreePlots=numberTreePlots
    
    allocate(xlocation(numberTreePlots))
    allocate(ylocation(numberTreePlots))
    allocate(phyfileNumber(numberTreePlots))
    allocate(strfileNumber(numberTreePlots))
    allocate(treesfileNumber(numberTreePlots))
    allocate(treesHeight(numberTreePlots))
    
!    xLocation=0
!    yLocation=0
!    phyfileNumber=0
!    strfileNumber=0
!    treesfileNumber=0
    
    REWIND (UTREESMAP)
    READ (UTREESMAP, location, IOSTAT = IOERROR)
    IF (IOERROR.NE.0) CALL SUBERROR('ERROR READING location DETAILS',IFATAL,IOERROR)
!    print *,xLocation
!    print *,yLocation
    state%xLocation=xLocation
    state%yLocation=yLocation
    
!    print *,state%xLocation
!    print *,state%yLocation
    
!    REWIND (UTREESMAP)
!    READ (UTREESMAP, files, IOSTAT = IOERROR)
!    IF (IOERROR.NE.0) CALL SUBERROR('ERROR READING file DETAILS',IFATAL,IOERROR)
!    print *,phyfileNumber
!    print *, strfileNumber
!    print *, treesfileNumber
    state%phyfileNumber=phyfileNumber
    state%strfileNumber=strfileNumber
    state%treesfileNumber=treesfileNumber
    state%treesHeight=treesHeight
    
!    print *, state%phyfileNumber
!    print *, state%strfileNumber
!    print *, state%treesfileNumber
    
    allocate(treeStates(numberTreePlots))
    do loopCount= 1,state%numberTreePlots
        call readMaespaTreeConfig(state%phyfileNumber(loopCount), state%strfileNumber(loopCount),&
            state%treesfileNumber(loopCount), treeStates(loopCount))  !! returns config
    end do
    
    end subroutine readMaespaTreeMap
    
    
    
    
    !**********************************************************************
      SUBROUTINE EXBEAMNEW(NALPHA,ALPHA,FALPHA,RANDOM,BZEN, &
        BEXT,BEXTANG)
!   calculate the extinction coefficients for beam radiation
!**********************************************************************

      USE maestcom
      IMPLICIT NONE
      INTEGER NALPHA,IALP
      REAL FALPHA(MAXANG),ALPHA(MAXANG),BEXTANG(MAXANG)
      REAL RANDOM,BZEN,BEXT,RSUM
      REAL, EXTERNAL :: COSDEL

      RSUM = 0.0
      DO 10 IALP = 1,NALPHA
        BEXTANG(IALP) = COSDEL(RANDOM,BZEN,ALPHA(IALP))
        RSUM = RSUM + BEXTANG(IALP)*FALPHA(IALP)
   10 CONTINUE
      BEXT = RSUM

      RETURN
      END SUBROUTINE EXBEAMNEW



!**********************************************************************
      
      
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    subroutine calculateTransmissionsOfTree(IDOY,timeis,treeState,treeConfigLocation,transmissionPercentage)
        use MAINDECLARATIONS
    use MaespaConfigState
    use MaespaConfigStateUtils
    !use radn
    
    INTEGER xtestRev,ytestRev,ztestRev
    INTEGER vegHeight
    INTEGER loopCount
    INTEGER phyFile, strFile, treeFile
    TYPE(maespaConfigTreeMapState) :: treeState     
    TYPE(maespaConfigvariablesstate) :: config
    real timeis
    INTEGER yd_actual
!    real ZENlocal
    integer treeConfigLocation
    real transmissionPercentage
    REAL FBEAM1HR(3) !! KN taking out the first dimension for FBEAM for just one hour. It is used in the following part to see
                     !! if it is daylight FBEAM>0
    
    !integer TIME,IDOY,KHRS
    !real ALAT,BEARlocal,DEClocal,ZENlocal,AZlocal
    
    NAMELIST /CONTROL/ IOHRLY,IOTUTD,IOHIST,IORESP,IOWATBAL,IOFORMAT,ISUNLA,KEEPZEN
    
    transmissionPercentage = 1.0
    
    ! Output file for errors and warnings
    OPEN (UERROR, FILE = 'Maeserr.dat', STATUS = 'UNKNOWN')
    OPEN (UCONTROL, FILE = 'confile.dat', STATUS = 'OLD',IOSTAT=IOERROR)
    IF(IOERROR.NE.0)THEN
        CALL SUBERROR('ERROR: CONFILE.DAT DOES NOT EXIST' ,IFATAL,0)
    ENDIF
    
                
     !call readMaespaStrConfigIntoState(state, treeState, treeStates)
     call InitMaespaSingleTreeSingleLoop    
     call READZEN(UCONTROL,NUMPNT,NOLAYI,PPLAYI,NZENI,NAZI,DIFZEN)
     call readMaespaTreeConfigFromConfig(treeState%phyFileNumber(treeConfigLocation), treeState%strFileNumber(treeConfigLocation), &
         treeState%treesfileNumber(treeConfigLocation), config)
         
    ! Open met data file (must be done after ISTART & IEND read)
    CALL OPENMETF(ISTART,IEND,CAK,PRESSK,SWMIN,SWMAX,USEMEASET,DIFSKY,ALAT,TTIMD,DELTAT,&
                    MFLAG,METCOLS,NOMETCOLS,MTITLE,MSTART,in_path)  
            ! to account for the looping order change.
    CALL RESTARTMETF(config%IDAY+ISTART,MSTART,MFLAG)                
    ! Get meteorological data
!    print *,config%IDAY
    
!    print *,'IDAY+ISTART',config%IDAY+ISTART
!    print *,'MFLAG',MFLAG
!    print *,'ZEN',config%ZEN
!    print *,'METCOLS',METCOLS
!    print *,'NOMETCOLS',NOMETCOLS
!    print *,'CAK',CAK
!    print *,'PRESSK',PRESSK
!    print *,'SWMIN',SWMIN
!    print *,'SWMAX',SWMAX
!    print *,'DELTAT',DELTAT
!    print *,'ALAT',ALAT
!    print *,'DEC',DEC
!    print *,'DAYL',DAYL
!    print *,'WINDAH',WINDAH
!    print *,'TSOIL',TSOIL
!    print *,'TAIR',TAIR
!    print *,'RADABV',RADABV
!    print *,'FBEAM',FBEAM
!    print *,'RH',RH
!    print *,'VPD',VPD
!    print *,'VMFD',VMFD
!    print *,'CA',CA
!    print *,'PRESS',PRESS
!    print *,'PPT',PPT
!    print *,'SOILMOIST',SOILMOIST
!    print *,'SOILDATA',SOILDATA
!    print *,'TSOILDATA',TSOILDATA
!    print *,'ETMEAS',ETMEAS
    
    
    CALL GETMET(config%IDAY+ISTART,MFLAG,config%ZEN,METCOLS,NOMETCOLS,CAK,PRESSK,SWMIN,SWMAX,DELTAT,  &
                    ALAT,DEC,DAYL,WINDAH,TSOIL,TAIR,RADABV,FBEAM,RH,VPD,VMFD,CA,PRESS,      &
                    PPT,SOILMOIST,SOILDATA,TSOILDATA,ETMEAS)                      
                    
!    do ipt = 1,48                
!        print *,ipt,FBEAM(ipt,1)  
!    end do
         
     config%NZEN=NZENI
     config%DIFZEN=DIFZEN
!     print *,NUMPNT
     DO IPT = 1,NUMPNT
         print *,'SUNLA/BEXT before',config%SUNLA,config%BEXT
         IHOUR=amod(timeis,24.)
         print *,timeis
         print *,ihour
         ihour = ihour * 2
         print *,ihour
         IDOY=ISTART !! this should account for running a few days

         !! calculate before hand?
         BEARlocal=0
         call ZENAZ_1HOUR(timeis,IDOY,24,ALAT,BEARlocal,DEClocal,ZENlocal,AZlocal)
         
         !!try replacing this with config value
         ZENlocal=config%zen(ihour)
         AZlocal=config%az(ihour)
         
!         print *,timeis,IDOY,24,ALAT,BEARlocal,DEClocal,ZENlocal,AZlocal
         
         ! should set time, idoy, khrs=24, alat
!         print *,radabv
         ! RADABV(IHR,1) = DATAIN(IHR,METCOLS(MHPAR)) / UMOLPERJ
         ! or 
         ! RADABV(IHR,1) = DATAIN(IHR,METCOLS(MHRAD)) * FPAR
         ! then for (*:,2)
         ! CALL CALCNIR(RADABV,FBEAM)
         ! then for (*:,3)
         ! CALL THERMAL(TAIR,VPD,FSUN,RADABV)


!         print *,IHOUR
         !! FBEAM(IHR,1) = CALCFBMH(IDATE,ZEN(IHR),RADABV(IHR,1))
         
         !! just setting these to somethign for now
         FBEAM1HR(1) =FBEAM(ihour,1)
         FBEAM1HR(2) =FBEAM(ihour,2)
         FBEAM1HR(3) =FBEAM(ihour,3)
         config%NOTREES=1
         
         !print *,config%NALPHASPEC
         !print *,config%ALPHASPEC
         !print *,config%FALPHASPEC
         !print *,config%RANDOMSPEC
         
!         print *,config%NOALLTREES !! in
!         print *,config%NOTREES !! in
!         print *,config%ITARGETS !! in (notarget)
!         print *,config%DXT1 !! in (dx)
!         print *,config%DYT1 !! in (dy)
!         print *,config%DZT1 !! in (dz)
!         print *,config%R1(1,1) !! in (r1)
!         print *,config%R2(1,1) !! in (r2)
!         print *,config%R3(1,1) !! in (r3)
!         print *,config%TRUNK(1,1) !! in (trunk)
!         print *,config%FLT(1,1) !! in (flt)
!         print *,config%DIAMA(1,1) !! in (diama)
!         print *,config%DXT !! out
!         print *,config%DYT !! out
!         print *,config%DZT !! out
!         !print *,config%RXTABLE !! out (rx)
!         print *,config%RX !! out (rx)
!!         print *,config%RYTABLE !! out (ry)
!         print *,config%RY !! out (ry)
!!         print *,config%RZTABLE !! out (rz)
!         print *,config%RZ !! out (rz)
!!         print *,config%FOLTABLE !! out (folt)
!         print *,config%FOLT !! out (folt)
!!         print *,config%ZBCTABLE !! out (zbc)
!         print *,config%ZBC !! out (zbc)
!         print *,config%DIAMTABLE  !! out (diam)
!         print *,config%ISPECIES  !! in
!         print *,config%ISPECIEST !! out
!         print *,config%IT  !! out
         
         
         !! not sure how many of these I need (or additional ones)
         ! Sort the trees every timestep.
                ! This should be done outside the hourly loop, and stored in an array.
        CALL SORTTREES(config%NOALLTREES,config%NOTREES,config%ITARGETS,config%DXT1,config%DYT1,config%DZT1,&
                        config%R1,config%R2,config%R3,config%TRUNK,&
                        config%FLT,config%DIAMA,config%DXT,config%DYT,config%DZT,config%RX,&
                        config%RY,config%RZ,config%FOLT,config%ZBC, &
                        config%DIAMTABLE,config%ISPECIES,config%ISPECIEST,config%IT)
                        
!        print *,config%RX(1)
!        print *,config%RY(1)
!        print *,config%RZ(1)

        DO I = 1,config%NOTREES
            config%JSHAPET(I) = config%JSHAPESPEC(config%ISPECIEST(I))
            config%SHAPET(I) = config%SHAPESPEC(config%ISPECIEST(I))
            config%DEXTT(I,1:MAXANG) = config%DEXTSPEC(config%ISPECIEST(I),1:MAXANG)
            config%JLEAFT(I) = config%JLEAFSPEC(config%ISPECIEST(I))
            config%NOAGECT(I) = config%NOAGECSPEC(config%ISPECIEST(I))
            config%BPTT(1:8,1:MAXC,I) = config%BPTSPEC(1:8,1:MAXC,config%ISPECIEST(I))
            config%PROPPT(1:MAXC,I) = config%PROPPSPEC(1:MAXC,config%ISPECIEST(I))
            config%PROPCT(1:MAXC,I) = config%PROPCSPEC(1:MAXC,config%ISPECIEST(I))
        END DO
!
!        ! Interpolate to get daily values of parameters
!        ! This we can probably also do outside the hourly loop.
        
        !! assume IDAY = 0
        config%IDAY=0
        !! I think NEWCANOPY can be 0
        !! maybe IPROG = INORMAL
        
        ITREE=1
        ISPEC = ISPECIES(ITREE)
        !print *,ispecies
        !print *,ISPEC
        
        
        
       !! DO ITAR = 1,NOTARGETS
        ITAR=1
                ITREE = config%ITARGETS(ITAR)
                ISPEC = config%ISPECIES(ITREE)
!                NOSPEC = config%MAXVAL(ISPECIES(ITARGETS(1:NOTARGETS)))                
!                config%JLEAF = config%JLEAFSPEC(ISPEC)
!                config%JSHAPE = config%JSHAPESPEC(ISPEC)
!                config%SHAPE = config%SHAPESPEC(ISPEC)
!                config%BPT(1:8,1:MAXC) = config%BPTSPEC(1:8,1:MAXC,ISPEC)
!                config%RANDOM = config%RANDOMSPEC(ISPEC)
!                config%NOAGEC = config%NOAGECSPEC(ISPEC)
!                config%EXTWIND = config%EXTWINDSPEC(ISPEC)
!                config%NALPHA = config%NALPHASPEC(ISPEC)
!                config%ALPHA(1:MAXANG) = config%ALPHASPEC(1:MAXANG,ISPEC)
!                config%FALPHA(1:MAXANG) = config%FALPHASPEC(1:MAXANG,ISPEC)
!                config%COEFFT = config%COEFFTSPEC(ISPEC)
!                config%EXPONT = config%EXPONTSPEC(ISPEC)
!                config%WINTERC = config%WINTERCSPEC(ISPEC)
!                config%BCOEFFT = config%BCOEFFTSPEC(ISPEC)
!                config%BEXPONT = config%BEXPONTSPEC(ISPEC)
!                config%BINTERC = config%BINTERCSPEC(ISPEC)
!                config%RCOEFFT = config%RCOEFFTSPEC(ISPEC)
!                config%REXPONT = config%REXPONTSPEC(ISPEC)
!                config%RINTERC = config%RINTERCSPEC(ISPEC)
!                config%FRFRAC = config%FRFRACSPEC(ISPEC)
                config%NOAGEP = config%NOAGEPSPEC(ISPEC)       
!                config%PROPC(1:MAXC) = config%PROPCSPEC(1:MAXC,ISPEC)
!                config%PROPP(1:MAXC) = config%PROPPSPEC(1:MAXC,ISPEC)    
                config%ABSRP(1:MAXLAY,1:3) = config%ABSRPSPEC(1:MAXLAY,1:3,ISPEC)  
                config%ARHO(1:MAXLAY,1:3) = config%ARHOSPEC(1:MAXLAY,1:3,ISPEC)      
                config%ATAU(1:MAXLAY,1:3) = config%ATAUSPEC(1:MAXLAY,1:3,ISPEC)       
                config%RHOSOL(1:3) = config%RHOSOLSPEC(1:3,ISPEC)    
!                config%JMAXTABLE(1:MAXDATE,1:MAXLAY,1:MAXC) = config%JMAXTABLESPEC(1:MAXDATE,1:MAXLAY,1:MAXC,ISPEC) 
!                config%DATESJ(1:MAXDATE) = config%DATESJSPEC(1:MAXDATE,ISPEC)       
!                config%NOJDATES = config%NOJDATESSPEC(ISPEC)     
!                config%IECO = config%IECOSPEC(ISPEC)         
!                config%EAVJ = config%EAVJSPEC(ISPEC)         
!                config%EDVJ = config%EDVJSPEC(ISPEC)         
!                config%DELSJ = config%DELSJSPEC(ISPEC)        
!                config%THETA = config%THETASPEC(ISPEC)        
                config%VCMAXTABLE(1:MAXDATE,1:MAXLAY,1:MAXC) = config%VCMAXTABLESPEC(1:MAXDATE,1:MAXLAY,1:MAXC,ISPEC)   
                config%DATESV(1:MAXDATE) =  config%DATESVSPEC(1:MAXDATE,ISPEC)         
                config%NOVDATES =  config%NOVDATESSPEC(ISPEC)     
!                config%EAVC = config%EAVCSPEC(ISPEC)         
!                config%EDVC = config%EDVCSPEC(ISPEC)         
!                config%DELSC = config%DELSCSPEC(ISPEC)        
!                config%TVJUP = config%TVJUPSPEC(ISPEC)        
!                config%TVJDN = config%TVJDNSPEC(ISPEC)        
                config%SLATABLE(1:maxdate,1:MAXLAY,1:MAXC) = config%SLATABLESPEC(1:MAXDATE,1:MAXLAY,1:MAXC,ISPEC)
                config%DATESSLA(1:maxdate) =  config%DATESSLASPEC(1:maxdate,ISPEC)       
                config%NOSLADATES = config%NOSLADATESSPEC (ISPEC)  
                config%NOADATES = config%NOADATESSPEC(ISPEC)   
                config%DATESA(1:MAXDATE) = config%DATESASPEC(1:MAXDATE,ISPEC)         
                config%AJQTABLE(1:MAXDATE,1:MAXLAY,1:MAXC) = config%AJQTABLESPEC(1:MAXDATE,1:MAXLAY,1:MAXC,ISPEC)     
                config%RDTABLE(1:MAXDATE,1:MAXLAY,1:MAXC) = config%RDTABLESPEC(1:MAXDATE,1:MAXLAY,1:MAXC,ISPEC)      
                config%DATESRD(1:MAXDATE) = config%DATESRDSPEC(1:MAXDATE,ISPEC)        
                config%NORDATES = config%NORDATESSPEC(ISPEC)     
!                config%RTEMP = config%RTEMPSPEC(ISPEC)        
!                config%DAYRESP = config%DAYRESPSPEC(ISPEC)      
!                config%TBELOW = config%TBELOWSPEC(ISPEC)       
!                config%EFFYRW = config%EFFYRWSPEC(ISPEC)       
!                config%RMW = config%RMWSPEC(ISPEC)          
!                config%RTEMPW = config%RTEMPWSPEC(ISPEC)       
!                config%COLLA = config%COLLASPEC(ISPEC)        
!                config%COLLK = config%COLLKSPEC(ISPEC)        
!                config%STEMSDW = config%STEMSDWSPEC(ISPEC)      
!                config%RMWAREA = config%RMWAREASPEC(ISPEC)      
!                config%STEMFORM = config%STEMFORMSPEC(ISPEC)     
                config%NOFQDATES = config%NOFQDATESSPEC(ISPEC)    
                config%DATESFQ(1:MAXDATE) =  config%DATESFQSPEC(1:maxdate,ISPEC)        
                config%Q10FTABLE(1:MAXDATE) =  config%Q10FTABLESPEC(1:maxdate,ISPEC)    
!                config%K10F = config%K10FSPEC(ISPEC)
                config%NOWQDATES = config%NOWQDATESSPEC(ISPEC)    
                config%DATESWQ = config%DATESWQSPEC(1:MAXDATE,ISPEC)      
                config%Q10WTABLE(1:maxdate) =  config%Q10WTABLESPEC(1:MAXDATE,ISPEC)    
!                config%RMFR = config%RMFRSPEC(ISPEC)         
!                config%RMCR = config%RMCRSPEC(ISPEC)         
!                config%Q10R = config%Q10RSPEC(ISPEC)         
!                config%RTEMPR = config%RTEMPRSPEC(ISPEC)       
!                config%EFFYRF = config%EFFYRFSPEC(ISPEC)       
!                config%RMB = config%RMBSPEC(ISPEC)          
!                config%Q10B = config%Q10BSPEC(ISPEC)         
!                config%RTEMPB = config%RTEMPBSPEC(ISPEC)      
!                config%GSREF = config%GSREFSPEC(ISPEC)        
!                config%GSMIN = config%GSMINSPEC(ISPEC)        
!                config%PAR0 = config%PAR0SPEC(ISPEC)         
!                config%D0 = config%D0SPEC(ISPEC)           
!                config%VK1 = config%VK1SPEC(ISPEC)         
!                config%VK2 = config%VK2SPEC(ISPEC)          
!                config%VPD1 = config%VPD1SPEC(ISPEC)         
!                config%VPD2 = config%VPD2SPEC(ISPEC)         
!                config%VMFD0 = config%VMFD0SPEC(ISPEC)        
!                config%GSJA = config%GSJASPEC(ISPEC)         
!                config%GSJB = config%GSJBSPEC(ISPEC)         
!                config%T0 = config%T0SPEC(ISPEC)           
!                config%TREF = config%TREFSPEC(ISPEC)        
!                config%TMAX = config%TMAXSPEC(ISPEC)         
!                config%SMD1 = config%SMD1SPEC(ISPEC)         
!                config%SMD2 = config%SMD2SPEC(ISPEC)        
!                config%WC1 = config%WC1SPEC(ISPEC)          
!                config%WC2 = config%WC2SPEC(ISPEC)          
!                config%SWPEXP = config%SWPEXPSPEC(ISPEC)       
                config%G0TABLE = config%G0TABLESPEC(1:maxdate,ISPEC)
                config%G1TABLE = config%G1TABLESPEC(1:maxdate,ISPEC)              
                config%DATESGS = config%DATESGSSPEC(1:maxdate,ISPEC)
                config%NOGSDATES = config%NOGSDATESSPEC(ISPEC)                
                config%WLEAFTABLE = config%WLEAFTABLESPEC(1:maxdate,ISPEC)
                config%DATESWLEAF = config%DATESWLEAFSPEC(1:maxdate,ISPEC)
                config%NOWLEAFDATES = config%NOWLEAFDATESSPEC(ISPEC)                
!                config%D0L = config%D0LSPEC(ISPEC)          
!                config%GAMMA = config%GAMMASPEC(ISPEC)     
!                config%VPDMIN = config%VPDMINSPEC(ISPEC)   
!                config%SF = config%SFSPEC(ISPEC)
!                config%PSIV = config%PSIVSPEC(ISPEC)                
!                config%NSIDES = config%NSIDESSPEC(ISPEC)                
!                config%VPARA = config%VPARASPEC(ISPEC)
!                config%VPARB = config%VPARBSPEC(ISPEC)
!                config%VPARC = config%VPARCSPEC(ISPEC)
!                config%VFUN  = config%VFUNSPEC(ISPEC)
                
        
        config%JMAXTABLE = config%JMAXTABLESPEC(1:MAXDATE,1:MAXLAY,1:MAXC,ISPEC) 
        
!        print *,config%IDAY  !! for now, leave these as 0
!        print *,config%ISTART !! for now, leave these as 0
!        print *,config%NOJDATESSPEC !! NOJDATESSPEC(ispecies) is NOJDATES
!        print *,config%NOJDATES !! below
!        print *,config%DATESJ  !! DATESJ(1:MAXDATE) = DATESJSPEC(1:MAXDATE,ISPEC)     
!        print *,config%DATESJSPEC(1:MAXDATE,ISPEC)     
!        print *,config%JMAXTABLE  !! JMAXTABLE(1:MAXDATE,1:MAXLAY,1:MAXC) = JMAXTABLESPEC(1:MAXDATE,1:MAXLAY,1:MAXC,ISPEC) 
!        print *,config%NOVDATES  !!NOVDATES =  NOVDATESSPEC(ISPEC)   
!        print *,config%DATESV  !! DATESV(1:MAXDATE) =  DATESVSPEC(1:MAXDATE,ISPEC)    
!        print *,config%VCMAXTABLE  !!VCMAXTABLE(1:MAXDATE,1:MAXLAY,1:MAXC) = VCMAXTABLESPEC(1:MAXDATE,1:MAXLAY,1:MAXC,ISPEC)  
!        print *,config%NORDATES !!NORDATES = NORDATESSPEC(ISPEC) 
!        print *,config%DATESRD !!DATESRD(1:MAXDATE) =  DATESRDSPEC(1:MAXDATE,ISPEC)  
!        print *,config%RDTABLE !!RDTABLE(1:MAXDATE,1:MAXLAY,1:MAXC) = RDTABLESPEC(1:MAXDATE,1:MAXLAY,1:MAXC,ISPEC) 
!        print *,config%NOSLADATES
!        print *,config%DATESSLA
!        print *,config%SLATABLE
!        print *,config%NOADATES
!        print *,config%DATESA
!        print *,config%AJQTABLE
!        print *,config%NOFQDATES
!        print *,config%DATESFQ
!        print *,config%Q10FTABLE
!        print *,config%NOWQDATES
!        print *,config%DATESWQ
!        print *,config%Q10WTABLE !! all above
!        print *,config%NOLAY
!        print *,config%NOAGEP !! below
!        print *,config%JMAX25  !! out
!        print *,config%VCMAX25  !! out
!        print *,config%RD0 !! above  !! out
!        print *,config%SLA !!  !! out
!        print *,config%AJQ !!  !! out
!        print *,config%Q10F !!  !! out
!        print *,config%Q10W !!  !! out
!        print *,config%NOGSDATES !!
!        print *,config%DATESGS !!
!        print *,config%G0TABLE !!
!        print *,config%G1TABLE !!
!        print *,config%G0 !!  !! out
!        print *,config%G1 !!  !! out
!        print *,config%NOWLEAFDATES !!
!        print *,config%DATESWLEAF !!
!        print *,config%WLEAFTABLE !!
!        print *,config%WLEAF !! above  !! out
        
        
        CALL INTERPOLATEP(config%IDAY,config%ISTART,config%NOJDATESSPEC(1),config%DATESJSPEC(1:MAXDATE,ISPEC) &
                                            ,config%JMAXTABLE,config%NOVDATES,config%DATESV,config%VCMAXTABLE, &
                                            config%NORDATES,config%DATESRD,config%RDTABLE,config%NOSLADATES,&
                                            config%DATESSLA,config%SLATABLE,config%NOADATES, &
                                            config%DATESA,config%AJQTABLE,config%NOFQDATES,config%DATESFQ,&
                                            config%Q10FTABLE,config%NOWQDATES,config%DATESWQ,  &
                                            config%Q10WTABLE,config%NOLAY,config%NOAGEP,&
                            config%JMAX25,config%VCMAX25,config%RD0,config%SLA,config%AJQ,config%Q10F,config%Q10W,& !! this are all out (JMAX25,VCMAX25,RD0,SLA,AJQ,Q10F,Q10W)
                                            config%NOGSDATES,config%DATESGS,config%G0TABLE,config%G1TABLE,&
                                            config%G0,config%G1,config%NOWLEAFDATES,config%DATESWLEAF,&
                                            config%WLEAFTABLE,config%WLEAF) !! out are G0, G1, and WLEAF
         
                                            
                                            
!        print *,IHOUR  !! is set above
!        print *,config%NOXDATES  !!ok
!        print *,config%DATESX !!ok
!        print *,config%RXTABLE  !! config%RX from sorttrees? !! or actually R1, R2, R3
!        print *,config%R1
        !!print *,config%RX
!        print *,config%NOYDATES !!ok
!        print *,config%DATESY !!ok
        !print *,config%RYTABLE  !! !! config%RY from sorttrees?
        !!print *,config%RY
!        print *,config%NOZDATES !!ok
!        print *,config%DATESZ !!ok
        !print *,config%RZTABLE  !! !! config%RZ from sorttrees?
        !!print *,config%RY
!        print *,config%NOTDATES !!ok
!        print *,config%DATEST !!ok
        !print *,config%ZBCTABLE  !!  !! config%ZBC from sorttrees?
        !!print *,config%ZBC
!        print *,config%NODDATES !!ok
!        print *,config%DATESD !!ok
!        print *,config%DIAMTABLE !!ok
!        print *,config%NOLADATES !!ok
!        print *,config%DATESLA !!ok
        !print *,config%FOLTABLE  !!  !! config%FOLT from sorttrees?
        !!print *,config%FOLT
!        print *,config%TOTLAITABLE !!ok
!        print *,config%NOTREES !!ok
!        print *,config%RX !! out
!        print *,config%RY !! out
!        print *,config%RZ !! out
!        print *,config%ZBC !! out
!        print *,config%FOLT !! out 
!        print *,config%TOTLAI  !! !! out
!        print *,config%DIAM  !!  !! out
!        print *,config%STOCKING !!ok
!        print *,config%IFLUSH  !!check  !! from NAMELIST /PHENOLOGY/ FLUSHDATE, DT1, DT2, DT3, DT4, EXPTIME, MAXLEAVES, SIZELEAF in trees.dat
!        print *,config%DT1  !!check !! from NAMELIST /PHENOLOGY/ FLUSHDATE, DT1, DT2, DT3, DT4, EXPTIME, MAXLEAVES, SIZELEAF in trees.dat
!        print *,config%DT2  !!check !! from NAMELIST /PHENOLOGY/ FLUSHDATE, DT1, DT2, DT3, DT4, EXPTIME, MAXLEAVES, SIZELEAF in trees.dat
!        print *,config%DT3  !!check !! from NAMELIST /PHENOLOGY/ FLUSHDATE, DT1, DT2, DT3, DT4, EXPTIME, MAXLEAVES, SIZELEAF in trees.dat
!        print *,config%DT4  !!check !! from NAMELIST /PHENOLOGY/ FLUSHDATE, DT1, DT2, DT3, DT4, EXPTIME, MAXLEAVES, SIZELEAF in trees.dat
!        print *,config%EXPTIME  !!check !! from NAMELIST /PHENOLOGY/ FLUSHDATE, DT1, DT2, DT3, DT4, EXPTIME, MAXLEAVES, SIZELEAF in trees.dat
!        print *,config%APP  !!check !! from NAMELIST /PHENOLOGY/ FLUSHDATE, DT1, DT2, DT3, DT4, EXPTIME, MAXLEAVES, SIZELEAF in trees.dat
!        print *,config%EXPAN  !!check !! from NAMELIST /PHENOLOGY/ FLUSHDATE, DT1, DT2, DT3, DT4, EXPTIME, MAXLEAVES, SIZELEAF in trees.dat
!        print *,config%NEWCANOPY !! out
!        print *,config%CANOPYDIMS     !!     !! out   
                                            
                                            
!        print *,config%RX(1)
!        print *,config%RY(1)
!        print *,config%RZ(1)
        
        CALL INTERPOLATET(config%IDAY,config%ISTART,IHOUR,config%NOXDATES,config%DATESX,config%R1,&
                            config%NOYDATES,config%DATESY,config%R2,    &
                            config%NOZDATES,config%DATESZ,config%R3,config%NOTDATES,config%DATEST,&
                            config%ZBCTABLE,config%NODDATES,config%DATESD,   &
                            config%DIAMTABLE,config%NOLADATES,config%DATESLA,config%FOLTABLE,config%TOTLAITABLE,&
                            config%NOTREES,config%RX,config%RY,config%RZ,  &
                            config%ZBC,config%FOLT,config%TOTLAI,config%DIAM,config%STOCKING,config%IFLUSH,&
                            config%DT1,config%DT2,config%DT3,config%DT4,config%EXPTIME,config%APP,   &
                            config%EXPAN,config%NEWCANOPY,config%CANOPYDIMS)                            

!        print *,config%RX(1)
!        print *,config%RY(1)
!        print *,config%RZ(1)
                            
!
! This subroutine is used to set up to 120 grid points through
! the crown. There are 12 grid points per layer and a minimum of
! 3 layers (36 points) is recommended. It is also recommended that
! the number of grid points used is a multiple of 36.
! The inputs required are:
! NUMPNT: the number of gridpoints
! JLEAF: 0 - no leaf area dist; 1 - vertical only; 2 - horiz. & vert.
! JSHAPE,SHAPE: indicate crown shape
! RX,RY,RZ: radius & green crown length of target crown
! ZBC: height to base of crown in target crown
! DXT,DYT,DZT: x,y,z co-ordinates of target crown
! FOL: leaf area of target crown
! BPT: coefficients of beta distributions of leaf area
! NOAGEC: no of age classes for which beta distributions are specified
! NOAGEP: no of age classes for which physiological params specified
! PROP: proportion of leaf area in each age class
! NOLAY: no of layers of crown
! Routine outputs are:
! XL,YL,ZL: the co-ordinates of each grid point
! VL: the volume of crown associated with each grid point
! DLT, DLI: the amount of leaf area associated with each grid point
! LGP: the physiological layer corresponding to each grid point
! FOLLAY: the amount of foliage in each layer
! CANOPYDIMS: canopy dimensions when these gridpoints were calculated
                        
!        print *,config%NOLAY
!        print *,config%PPLAY
!        !print *,config%JLEAF !!
!        print *,config%JLEAFSPEC
!        !print *,config%JSHAPE !!
!        print *,config%JSHAPESPEC
!        !print *,config%SHAPE !!
!        print *,config%SHAPESPEC
!        print *,config%RX(1)
!        print *,config%RY(1)
!        print *,config%RZ(1)
!        print *,config%ZBC(1)
!        print *,config%DXT(1)
!        print *,config%DYT(1)
!        print *,config%DZT(1)
!        print *,config%FOLT(1)
!        !print *,config%PROPC !!
!        print *,config%PROPCSPEC
!        !print *,config%PROPP !!
!        print *,config%PROPPSPEC
!        print *,config%BPT !!
!        !print *,config%NOAGECT(1)  !!
!        print *,config%NOAGECSPEC(1)
!        !! I think NOAGECT(1) can be NOAGECSPEC(ISPECIEST(I))
!        !print *,config%NOAGEP    !!        
!        print *,config%NOAGEPSPEC
                             
        CALL POINTSNEW(config%NOLAY,config%PPLAY,config%JLEAFSPEC,config%JSHAPESPEC,config%SHAPESPEC,&
                        config%RX(1),config%RY(1),config%RZ(1),config%ZBC(1),config%DXT(1),config%DYT(1),&
                        config%DZT(1),config%FOLT(1),config%PROPCSPEC,config%PROPPSPEC,config%BPT,&
                        config%NOAGECSPEC(1),config%NOAGEPSPEC,   &
                        config%XL,config%YL,config%ZL,config%VL,config%DLT,config%DLI,&
                        config%LGP,config%FOLLAY)
          
!        print *,IDOY
!        print *,ALAT
!        print *,TTIMD
!        print *,DEC  !! out
!        print *,EQNTIM  !! out
!        print *,DAYL  !! out
!        print *,SUNSET  !! out
        CALL SUN(IDOY,ALAT,TTIMD,DEC,EQNTIM,DAYL,SUNSET)
                        
!        print *,IHOUR
!        print *,TTIMD  !!  TTIMD - time difference between longitude of plot & longitude of time zone, in hours
!        print *,EQNTIM  !! from sun()
!        print *,ALAT  !! from sun()
!        print *,DEC  !! from sun()
!        print *,config%XSLOPE
!        print *,config%YSLOPE
!        print *,BEARlocal
!        print *,ZENlocal
!        print *,BMULT !! out
!        print *,DMULT2 !! out
!        print *,SOMULT  !! out
        
        CALL SLOPES(IHOUR,TTIMD,EQNTIM,ALAT,DEC,config%XSLOPE,config%YSLOPE,BEARlocal,ZENlocal,BMULT,DMULT2,SOMULT) 
!        print *,BMULT !! out
!        print *,DMULT2 !! out
!        print *,SOMULT  !! out
         

        ! Calculate diffuse transmittances
        !! assume IDAY = 0
        !! I think NEWCANOPY can be 0
        !! maybe IPROG = INORMAL
                        
!        print *,config%IDAY !!
!        print *,config%NEWCANOPY !!
!        print *,config%IPROG
!        print *,config%NOTREES
!        print *,config%XSLOPE
!        print *,config%YSLOPE
!        print *,config%NZEN
!        print *,config%DIFZEN
!        print *,NAZ
!        print *,NUMPNT
!        print *,config%DEXTT
!        print *,config%DEXTSPEC
!        print *,DIFSKY !!    from CALL OPENMETF
!        print *,config%XL  !! output from POINTSNEW
!        print *,config%YL  !! output from POINTSNEW
!        print *,config%ZL  !! output from POINTSNEW
!        print *,config%RX !!used in POINTSNEW
!        print *,config%RY !!used in POINTSNEW
!        print *,config%RZ !!used in POINTSNEW
!        print *,config%DXT !!used in POINTSNEW
!        print *,config%DYT !!used in POINTSNEW
!        print *,config%DZT !!used in POINTSNEW
!        print *,config%XMAX
!        print *,config%YMAX
!        print *,config%SHADEHT
!        print *,config%FOLT
!        print *,config%ZBC
!        print *,config%JLEAFT
!        print *,config%BPTT
!        print *,config%NOAGECT
        !! I think NOAGECT(1) can be NOAGECSPEC(ISPECIEST(I))
!        print *,config%PROPCT
!        print *,config%JSHAPET
!        print *,config%SHAPET
!        print *,config%NEWTUTD  !! out
!        print *,config%TU !! out
!        print *,config%TD !! out
!        print *,config%RELDF !! out
!        print *,config%DEXT  !!    out      
                        
         
                        
        CALL TRANSD(config%IDAY,config%NEWCANOPY,config%IPROG,config%NOTREES,config%XSLOPE,config%YSLOPE,&
                        config%NZEN,config%DIFZEN,NAZ,NUMPNT,config%DEXTT, &
                    DIFSKY,config%XL,config%YL,config%ZL,config%RX,config%RY,config%RZ,&
                        config%DXT,config%DYT,config%DZT,config%XMAX,config%YMAX,config%SHADEHT,&
                        config%FOLT,config%ZBC,config%JLEAFT,config%BPTT,config%NOAGECT,config%PROPCT, &
                    config%JSHAPET,config%SHAPET,config%NEWTUTD,config%TU,config%TD,config%RELDF,config%DEXT)   
!        print *,config%NEWTUTD  !! out from TRANSD
!        print *,config%TU(1) !! out from TRANSD  ! DIFFUSE TRANSMITTANCES FOR UPPER AND LOWER HEMISPHERES
!        print *,config%TD(1) !! out from TRANSD   ! DIFFUSE TRANSMITTANCES FOR UPPER AND LOWER HEMISPHERES
!        print *,config%RELDF(1) !! out from TRANSD   ! DIFFUSE TRANSMITTANCES FOR UPPER AND LOWER HEMISPHERES
!        print *,config%DEXT(1)  !!    out from TRANSD        !!the extinction coefficient weighted by pathlengths   
                    
    IF(IPOINTS .EQ. 1)THEN
      CALL GETPOINTSF(NUMTESTPNT,XLP,YLP,ZLP,X0,Y0,XMAX,YMAX, &
         CTITLE,TTITLE,MTITLE,STITLE,VTITLE)
    ENDIF
    config%NUMTESTPNT=NUMTESTPNT
                    
print *,'MLAYERP in',MLAYER(1)   
!print *,NUMPNT
print *,NUMTESTPNT
print *,config%TU(1)
print *,config%TD(1)
print *,config%TOTLAI
print *,config%XSLOPE
print *,config%YSLOPE,NAZ
print *,config%NZEN
print *,config%DIFZEN,config%DEXT
        ! If the diffuse transmittances have changed, must set up the EHC
        IF (config%NEWTUTD.EQ.1.AND.config%TOTLAI.GT.0) THEN
            CALL EHC(NUMTESTPNT,config%TU,config%TD,config%TOTLAI,config%XSLOPE,config%YSLOPE,NAZ,config%NZEN,&
                    config%DIFZEN,config%DEXT,&
                    DLAI,EXPDIF,LAYER,MLAYER) !! are these out?
        END IF
         !!!!!
print *,'MLAYERP out',MLAYER(1)        
         !! need to set up BEXTT (and probably lots of others)
         CALL EXBEAMNEW(config%NALPHASPEC(1),config%ALPHASPEC(1:MAXANG,1),config%FALPHASPEC(1:MAXANG,1),config%RANDOMSPEC(1),&
                                    ZENlocal,BEXTSPEC(1),BEXTANGSPEC(1,1:MAXANG))
         
        !print *,config%NOTREES
        !print *,config%ISPECIES
        DO I=1,config%NOTREES
            BEXTT(I) = BEXTSPEC(config%ISPECIES(I))
            !print *,BEXTSPEC
            BEXTANGT(I,1:MAXANG) = BEXTANGSPEC(config%ISPECIES(I),1:MAXANG)
        END DO    
        config%BEXTT=BEXTT
        
        !print *,BEXTT
        !print *,config%BEXTT
        !print *,BEXTANGT(1,1:MAXANG)
        
        !! checking values for interpolatet
                print *,IHOUR  !! is set above
        print *,config%NOXDATES  !!ok
        print *,config%DATESX !!ok
        print *,config%RXTABLE(1,1)  !! config%RX from sorttrees? !! or actually R1, R2, R3
        print *,config%R1
        !print *,config%RX
        print *,config%NOYDATES !!ok
        print *,config%DATESY !!ok
        print *,config%RYTABLE(1,1)  !! !! config%RY from sorttrees?
        !print *,config%RY
        print *,config%NOZDATES !!ok
        print *,config%DATESZ !!ok
        print *,tmpDATESZ
        print *,config%RZTABLE(1,1)  !! !! config%RZ from sorttrees?
        !print *,config%RY
        print *,config%NOTDATES !!ok
        print *,config%DATEST !!ok
        print *,config%ZBCTABLE(1,1)  !!  !! config%ZBC from sorttrees?
        !print *,config%ZBC
        print *,config%NODDATES !!ok
        print *,config%DATESD !!ok
        print *,config%DIAMTABLE(1,1) !!ok
        print *,config%NOLADATES !!ok
        print *,config%DATESLA !!ok
        print *,config%FOLTABLE(1,1)  !!  !! config%FOLT from sorttrees?
        !print *,config%FOLT
        print *,config%TOTLAITABLE !!ok
        print *,config%NOTREES !!ok
        !print *,config%RX !! out
        !print *,config%RY !! out
        !print *,config%RZ !! out
        !print *,config%ZBC !! out
        !print *,config%FOLT !! out 
        !print *,config%TOTLAI  !! !! out
        !print *,config%DIAM  !!  !! out
        !print *,config%STOCKING !!ok
        print *,config%IFLUSH  !!check  !! from NAMELIST /PHENOLOGY/ FLUSHDATE, DT1, DT2, DT3, DT4, EXPTIME, MAXLEAVES, SIZELEAF in trees.dat
        print *,config%DT1  !!check !! from NAMELIST /PHENOLOGY/ FLUSHDATE, DT1, DT2, DT3, DT4, EXPTIME, MAXLEAVES, SIZELEAF in trees.dat
        print *,config%DT2  !!check !! from NAMELIST /PHENOLOGY/ FLUSHDATE, DT1, DT2, DT3, DT4, EXPTIME, MAXLEAVES, SIZELEAF in trees.dat
        print *,config%DT3  !!check !! from NAMELIST /PHENOLOGY/ FLUSHDATE, DT1, DT2, DT3, DT4, EXPTIME, MAXLEAVES, SIZELEAF in trees.dat
        print *,config%DT4  !!check !! from NAMELIST /PHENOLOGY/ FLUSHDATE, DT1, DT2, DT3, DT4, EXPTIME, MAXLEAVES, SIZELEAF in trees.dat
        print *,config%EXPTIME  !!check !! from NAMELIST /PHENOLOGY/ FLUSHDATE, DT1, DT2, DT3, DT4, EXPTIME, MAXLEAVES, SIZELEAF in trees.dat
        print *,config%APP  !!check !! from NAMELIST /PHENOLOGY/ FLUSHDATE, DT1, DT2, DT3, DT4, EXPTIME, MAXLEAVES, SIZELEAF in trees.dat
        print *,config%EXPAN  !!check !! from NAMELIST /PHENOLOGY/ FLUSHDATE, DT1, DT2, DT3, DT4, EXPTIME, MAXLEAVES, SIZELEAF in trees.dat
        print *,config%NEWCANOPY !! out
        print *,config%CANOPYDIMS     !!     !! out   
        !! end checking values for interpolatet

        
        ! Interpolate overstorey dimensions for use in test point calcs.
        CALL INTERPOLATET(IDAY,ISTART,1, &
         NOXDATES,DATESX,RXTABLEP,NOYDATES,DATESY,RYTABLEP, &
         NOZDATES,DATESZ,RZTABLEP,NOTDATES,DATEST,ZBCTABLEP, &
         NODDATES,DATESD,DIAMTABLEP, &
         NOLADATES,DATESLA,FOLTABLEP,TOTLAITABLE,NOTREESTEST, &
         RXP,RYP,RZP,ZBCP,FOLTP,TOTLAI,DIAM,STOCKING, &
         IFLUSH,DT1,DT2,DT3,DT4,EXPTIME,APP,EXPAN, &
         NEWCANOPY,CANOPYDIMS)
        
        ! NUMPNT: the number of gridpoints
        ! JLEAF: 0 - no leaf area dist; 1 - vertical only; 2 - horiz. & vert.
        ! JSHAPE,SHAPE: indicate crown shape
        ! RX,RY,RZ: radius & green crown length of target crown
        ! ZBC: height to base of crown in target crown
        ! DXT,DYT,DZT: x,y,z co-ordinates of target crown
        ! FOL: leaf area of target crown
        ! BPT: coefficients of beta distributions of leaf area
        ! NOAGEC: no of age classes for which beta distributions are specified
        ! NOAGEP: no of age classes for which physiological params specified
        ! PROP: proportion of leaf area in each age class
        ! NOLAY: no of layers of crown
        ! Routine outputs are:
        ! XL,YL,ZL: the co-ordinates of each grid point
        ! VL: the volume of crown associated with each grid point
        ! DLT, DLI: the amount of leaf area associated with each grid point
        ! LGP: the physiological layer corresponding to each grid point
        ! FOLLAY: the amount of foliage in each layer
        ! CANOPYDIMS: canopy dimensions when these gridpoints were calculated
        
        !print *,ZENlocal
!        print *,FBEAM1HR
!        print *,IHOUR,IWAVE
!        print *,FBEAM(IHOUR,IWAVE)
!        print *,AZlocal
        print *,config%XLP(IPT),config%YLP(IPT),config%ZLP(IPT)
        print *,config%RX(1)
        print *,config%RY(1)
        print *,config%RZ(1)!!!!not right yet
        print *,config%DXT(1)
        print *,config%DYT(1)
        print *,config%DZT(1)
        print *,config%XMAX,config%YMAX,config%SHADEHT
        !! ok, AZlocal is working now, next xslope, etc
        
        ! Calculate the weighted pathlengths for beam radiation.
!         CALL TRANSB_1hour(IHOUR,config%IPROG,ZENlocal,AZlocal,config%XSLOPE,&
!             config%YSLOPE,FBEAM1HR,config%BEXTT,config%XLP(IPT),config%YLP(IPT),config%ZLP(IPT),&
!             config%RX,config%RY,config%RZ,config%DXT,config%DYT,config%DZT,config%XMAX,config%YMAX,config%SHADEHT,&
!             config%FOLT,config%ZBC,config%JLEAFT,config%BPTT,config%NOAGECT,config%PROPCT,config%JSHAPET,&
!             config%SHAPET,config%NOTREES,config%SUNLA,config%BEXT)  
        
        print *,'~~~~~~~~~~~~'
        print *,ihour,config%IPROG,ZENlocal,AZlocal
        print *,config%xslope,config%yslope,fbeam(1,1),config%BEXTT(1)
        print *,config%XLP(IPT),config%YLP(IPT),config%ZLP(IPT)
        
         CALL TRANSB(IHOUR,config%IPROG,ZENlocal,AZlocal,config%XSLOPE,&
             config%YSLOPE,FBEAM,config%BEXTT,config%XLP(IPT),config%YLP(IPT),config%ZLP(IPT),&
             config%RX,config%RY,config%RZ,config%DXT,config%DYT,config%DZT,config%XMAX,config%YMAX,config%SHADEHT,&
             config%FOLT,config%ZBC,config%JLEAFT,config%BPTT,config%NOAGECT,config%PROPCT,config%JSHAPET,&
             config%SHAPET,config%NOTREES,config%SUNLA,config%BEXT)      
             
             
         print *,'SUNLA/BEXT after',config%SUNLA,config%BEXT

 !! do I need to also run these to get PREVTSOIL,ARHO,ATAU,RHOSOL        
        ! Decide whether to simulate the water balance (MAESPA) or not (MAESTRA)    
        IF(IWATFILE .EQ. 0)THEN
           ISMAESPA = .FALSE.
        ELSE
           ISMAESPA = .TRUE.
        ENDIF      
        ! Soil surface T for SCATTER routine:
        IF(SIMTSOIL.EQ.0)THEN  ! No Tsoil simulated.
            PREVTSOIL = TK(TSOIL(IHOUR))
        ELSE
            PREVTSOIL = SOILTEMP(1)
        ENDIF       
         
!! just getting to this part         
        ! Loop over the 3 wavelengths
        DO IWAVE = 1,3
            ! Calculate the scattered radiation
            print *,'before scatter',iwave,DIFUP(ipt,iwave),DIFDN(ipt,iwave),SCLOST(ipt,iwave)
            

            
            print *,'',IHOUR
            print *,'',IWAVE
            
            print *,'IPT',IPT
            print *,'IWAVE',IWAVE
            print *,'MLAYER',MLAYER(IPT)
            print *,'LAYER',LAYER(IPT)
            print *,'DLAI',DLAI
            print *,'EXPDIF',EXPDIF
            print *,'ZENlocal',ZENlocal
            print *,'BEXT',config%BEXT
            print *,'DMULT2',DMULT2 !! from slopes()
            print *,'SOMULT',SOMULT !! from slopes()
            print *,'BMULT',BMULT !! from slopes()
            print *,'RADABV',RADABV(IHOUR,IWAVE) !! from getmet()
            print *,'FBEAM1HR',FBEAM1HR(IWAVE)
            print *,'FBEAM',FBEAM(IHOUR,IWAVE) !! from getmet()
            print *,'TAIR',TAIR(IHOUR) !! from getmet()
            print *,'TSOIL',TSOIL(ihour)
            print *,'ARHO',config%ARHO(config%LGP(IPT),IWAVE)
            print *,'ATAU',config%ATAU(config%LGP(IPT),IWAVE)
            print *,'RHOSOL',config%RHOSOL(IWAVE)
!            print *,config%DIFUP !! out
!            print *,config%DIFDN !! out
!            print *,config%SCLOST !! out
!            print *,config%DOWNTH !! out
                                  
            CALL SCATTER(IPT,IWAVE,MLAYER(IPT),LAYER(IPT),DLAI,EXPDIF,ZENlocal,config%BEXT,DMULT2,SOMULT,BMULT,&
                            RADABV(IHOUR,IWAVE),FBEAM1HR(IWAVE),TAIR(IHOUR),TSOIL(ihour),config%ARHO(config%LGP(IPT),IWAVE),&
                            config%ATAU(config%LGP(IPT),IWAVE),config%RHOSOL(IWAVE),DIFUP,DIFDN,SCLOST,DOWNTH)
            print *,'after scatter',iwave,DIFUP(ipt,iwave),DIFDN(ipt,iwave),SCLOST(ipt,iwave),RADABV(IHOUR,IWAVE),DOWNTH(1)
!
!            ! Lost scattered radiation for each tree (W m-2), averaged over the grid points.
!            ! RAD June 2008.              
!            SCLOSTTREE(ITAR,1) = SUM(SCLOST(1:NUMPNT,1)) / NUMPNT
!            SCLOSTTREE(ITAR,2) = SUM(SCLOST(1:NUMPNT,2)) / NUMPNT
!        call readLEFromMaespaWatBalFiles(sfc_ab_map_x(iab),sfc_ab_map_y(iab),sfc_ab_map_z(iab),sfc_ab_map_f(iab),timeis,&
!                            yd_actual,maespaLE)
!
!            ! Assume zero reflectance in TR waveband (Norman 1979)
!            ! But store in the same array the lost tranmission at top of canopy.
!            SCLOSTTREE(ITAR,3) = SUM(SCLOST(1:NUMPNT,2)) / NUMPNT
!
!            ! Downwelling longwave radiation (calculated for each gridpoint
!            ! with the EHC) averaged across the grid points.
!            IF(IWAVE.EQ.3)DOWNTHTREE(ITAR) = SUM(DOWNTH) / NUMPNT
!
!            ! Calculate absorbed radiation
            print *,'before absrad',iwave,dflux(ipt,iwave),bflux(ipt,iwave),scatfx(ipt,iwave)
            
!            print *,IPT
!            print *,IWAVE
!            print *,NZEN
!            print *,config%DEXT
!            print *,config%BEXT
!            print *,BMULT
!            print *,config%RELDF(IPT)
!            print *,RADABV(IHOUR,IWAVE)
!            print *,FBEAM1HR(IWAVE)
!            print *,ZENlocal
!            print *,ABSRP(config%LGP(IPT),IWAVE)
!            print *,DIFDN(IPT,IWAVE)
!            print *,DIFUP(IPT,IWAVE)
!            print *,DFLUX  !! out
!            print *,BFLUX  !! out
!            print *,SCATFX  !! out
            CALL ABSRAD(IPT,IWAVE,NZEN,config%DEXT,config%BEXT,BMULT,config%RELDF(IPT),RADABV(IHOUR,IWAVE),&
                        FBEAM1HR(IWAVE),ZENlocal,ABSRP(config%LGP(IPT),IWAVE),DIFDN(IPT,IWAVE),&
                        DIFUP(IPT,IWAVE),DFLUX,BFLUX,SCATFX)
                        
            print *,'after absrad',iwave,dflux(ipt,iwave),bflux(ipt,iwave),scatfx(ipt,iwave),RADABV(IHOUR,IWAVE)
            
            
            if (IWAVE .eq. 1) THEN
          
                !!!! does ihour need to be doubled?
                !code from testpoints
                IPTEST=IPT
                !print *,IPT
                PAR = RADABV(IHOUR,1)*UMOLPERJ  
                !print *,RADABV
                !print *,UMOLPERJ  
                TBEAM = FBEAM1HR(IWAVE)*PAR*config%SUNLA
                !print *,config%SUNLA
                !print *,FBEAM1HR
                TDIFF = (1-FBEAM1HR(IWAVE))*PAR*config%TD(IPTEST)
                !print *,config%TD
                TSCAT = DIFDN(IPTEST,IWAVE)
                !print *,DIFDN
                TTOT = TBEAM + TDIFF + TSCAT
    
                APARSUN = (BFLUX(IPTEST,1)*config%BEXT + DFLUX(IPTEST,1))*UMOLPERJ
                APARSH = DFLUX(IPTEST,1)*UMOLPERJ
                APARMEAN = config%SUNLA * APARSUN + (1-config%SUNLA)*APARSH

                print *,'PAR',PAR
                print *,'FBEAM: beam fraction of PAR',FBEAM1HR(1)
                print *,'SUNLA: sunlit leaf area at grid point (fraction)',config%SUNLA
                print *,'TD: diffuse transmittance to grid point (fraction)',TDP(IPTEST)
                print *,'TSCAT: scattered radiation (umol m-2 s-1)',TSCAT
                print *,'TTOT: total radiation (umol m-2 s-1)',TTOT
                print *,'APARSUN : Absorped PAR for sunlit foliage (umol m-2 s-1)',APARSUN
                print *,'APARSH : Absorped PAR for shaded foliage (umol m-2 s-1)',APARSH
                print *,'APAR : Absorped PAR (umol m-2 s-1)',APARMEAN
                print *,''

                !!  IDAY,IHOUR,IPTEST,XLP(IPTEST),YLP(IPTEST),ZLP(IPTEST),PAR,FBEAM(IHOUR,1),SUNLAP,TDP(IPTEST),TSCAT,TTOT,APARSUN,APARSH,APARMEAN

    !            WRITE (UPOINTSO, 993) 'DAY: day number'
    !            WRITE (UPOINTSO, 993) 'HR: hour number'
    !            WRITE (UPOINTSO, 993) 'PT: point number'
    !            WRITE (UPOINTSO, 993) 'X,Y,Z, : coordinates of test point'
    !            WRITE (UPOINTSO, 993) 'PAR: incident PAR (umol m-2 s-1)'
    !            WRITE (UPOINTSO, 993) 'FBEAM: beam fraction of PAR'
    !            WRITE (UPOINTSO, 993) 'SUNLA: sunlit leaf area at grid point (fraction)'
    !            WRITE (UPOINTSO, 993) 'TD: diffuse transmittance to grid point (fraction)'
    !            WRITE (UPOINTSO, 993) 'TSCAT: scattered radiation (umol m-2 s-1)'
    !            WRITE (UPOINTSO, 993) 'TTOT: total radiation (umol m-2 s-1)'
    !            WRITE (UPOINTSO, 993) 'APARSUN : Absorped PAR for sunlit foliage (umol m-2 s-1)'
    !            WRITE (UPOINTSO, 993) 'APARSH : Absorped PAR for shaded foliage (umol m-2 s-1)'
    !            WRITE (UPOINTSO, 993) 'APAR : Absorped PAR (umol m-2 s-1)'
            
            end if
            
            
        END DO








     enddo
    
    
    
    
!    treeConfigLocation = -1
    
!    !only load it once 
!    !if (treeState%numberTreePlots.lt.1 .or. treeState%numberTreePlots.gt.100000000) then 
!        call readMaespaTreeMapFromConfig(treeState)
!    !endif
    
!    print *,'number of tree plots', treeState%numberTreePlots
    
!    vegHeight = 0 !if the tree location isn't found, then it will be 0 high
!    !first check if the x,y is in the location list
!    do loopCount = 1,treeState%numberTreePlots
!        !print *,'xtestrev,ytestrev ',loopcount,xtestrev,ytestrev,treeState%xLocation(loopCount),treeState%yLocation(loopCount)
!        !! this is +1 because of arrays 0 and 1 indexing problem
!        if ( (xtestRev).eq.treeState%xLocation(loopCount)+1 .AND. (ytestRev).eq.treeState%yLocation(loopCount)+1 ) then
!            print *,'found tree at ',xtestRev,ytestRev
!            vegHeight = treeState%treesHeight(loopCount)
!            print *,'tree config files ',treeState%treesfileNumber(loopCount)
!            treeConfigLocation = loopCount
!            
!            !! instead of below, need to keep track of what tree
!            !! find allocation and store it
!            
!!           call readMaespaStrConfigIntoState(state, treeState, treeStates)
!            !call InitMaespaSingleTreeSingleLoop
! !           call READZEN(UCONTROL,NUMPNT,NOLAYI,PPLAYI,NZENI,NAZI,DIFZEN)
! !           call readMaespaTreeConfigFromConfig(treeState%phyFileNumber(loopCount), treeState%strFileNumber(loopCount), &
! !               treeState%treesfileNumber(loopCount), config)
!!            print *,NUMPNT
!!            DO IPT = 1,NUMPNT
!!                print *,'SUNLA/BEXT before',config%SUNLA,config%BEXT
!!                IHOUR=amod(timeis,24.)
!!                
!!                !! calculate before hand?
!!                
!!                print *,idate
!!                print *,zen
!!                print *,idoy
!!                print *,alat
!!                print *,yd_actual
!!                call ZENAZ_1HOUR(timeis*2.0,IDOY,24,ALAT,BEARlocal,DEClocal,ZENlocal,AZlocal)
!!                ! should set time, idoy, khrs=24, alat
!!                
!!                print *,idate
!!                print *,zen
!!                print *,idoy
!!                print *,alat
!!                print *,yd_actual
!!                
!!                
!!                print *,radabv
!!                ! RADABV(IHR,1) = DATAIN(IHR,METCOLS(MHPAR)) / UMOLPERJ
!!                ! or 
!!                ! RADABV(IHR,1) = DATAIN(IHR,METCOLS(MHRAD)) * FPAR
!!                ! then for (*:,2)
!!                ! CALL CALCNIR(RADABV,FBEAM)
!!                ! then for (*:,3)
!!                ! CALL THERMAL(TAIR,VPD,FSUN,RADABV)
!!
!!                
!!                !print *,IHOUR
!!                !! FBEAM(IHR,1) = CALCFBMH(IDATE,ZEN(IHR),RADABV(IHR,1))
!!               
!!                CALL TRANSB_1hour(IHOUR,config%IPROG,config%ZEN(IHOUR),config%AZ(IHOUR),config%XSLOPE,&
!!                    config%YSLOPE,config%FBEAM,config%BEXTT,config%XL(IPT),YL(IPT),ZL(IPT),&
!!                    config%RX,config%RY,config%RZ,config%DXT,config%DYT,config%DZT,config%XMAX,config%YMAX,config%SHADEHT,&
!!                    config%FOLT,config%ZBC,config%JLEAFT,config%BPTT,config%NOAGECT,config%PROPCT,config%JSHAPET,&
!!                    config%SHAPET,config%NOTREES,config%SUNLA,config%BEXT)  
!!                print *,'SUNLA/BEXT after',config%SUNLA,config%BEXT
!!                
!!                
!!                
!!                
!!                
!!                
!!!                ! Calculate the weighted pathlengths for beam radiation.
!!!                        CALL TRANSB(IHOUR,IPROG,ZEN(IHOUR),AZ(IHOUR),XSLOPE,YSLOPE,FBEAM,BEXTT,XL(IPT),YL(IPT),ZL(IPT),&
!!!                                    RX,RY,RZ,DXT,DYT,DZT,XMAX,YMAX,SHADEHT,FOLT,ZBC,JLEAFT,BPTT,NOAGECT,PROPCT,JSHAPET,&
!!!                                    SHAPET,NOTREES,SUNLA,BEXT)                   
!!!
!!!
!!!                                    
!!!                        ! Loop over the 3 wavelengths
!!!                        DO IWAVE = 1,3
!!!                            ! Calculate the scattered radiation
!!!                            !print *,'before scatter',iwave,DIFUP(ipt,iwave),DIFDN(ipt,iwave),SCLOST(ipt,iwave)
!!!                            CALL SCATTER(IPT,IWAVE,MLAYER(IPT),LAYER(IPT),DLAI,EXPDIF,ZEN(IHOUR),BEXT,DMULT2,SOMULT,BMULT,&
!!!                                            RADABV(IHOUR,IWAVE),FBEAM(IHOUR,IWAVE),TAIR(IHOUR),PREVTSOIL,ARHO(LGP(IPT),IWAVE),&
!!!                                            ATAU(LGP(IPT),IWAVE),RHOSOL(IWAVE),DIFUP,DIFDN,SCLOST,DOWNTH)
!!!                            print *,'after scatter',iwave,DIFUP(ipt,iwave),DIFDN(ipt,iwave),SCLOST(ipt,iwave),RADABV(IHOUR,IWAVE)
!!!                  
!!!                            ! Lost scattered radiation for each tree (W m-2), averaged over the grid points.
!!!                            ! RAD June 2008.              
!!!                            SCLOSTTREE(ITAR,1) = SUM(SCLOST(1:NUMPNT,1)) / NUMPNT
!!!                            SCLOSTTREE(ITAR,2) = SUM(SCLOST(1:NUMPNT,2)) / NUMPNT
!!!                            
!!!                            ! Assume zero reflectance in TR waveband (Norman 1979)
!!!                            ! But store in the same array the lost tranmission at top of canopy.
!!!                            SCLOSTTREE(ITAR,3) = SUM(SCLOST(1:NUMPNT,2)) / NUMPNT
!!!
!!!                            ! Downwelling longwave radiation (calculated for each gridpoint
!!!                            ! with the EHC) averaged across the grid points.
!!!                            IF(IWAVE.EQ.3)DOWNTHTREE(ITAR) = SUM(DOWNTH) / NUMPNT
!!!                  
!!!                            ! Calculate absorbed radiation
!!!                            !print *,'before absrad',iwave,dflux(ipt,iwave),bflux(ipt,iwave),scatfx(ipt,iwave)
!!!                            CALL ABSRAD(IPT,IWAVE,NZEN,DEXT,BEXT,BMULT,RELDF(IPT),RADABV(IHOUR,IWAVE),&
!!!                                        FBEAM(IHOUR,IWAVE),ZEN(IHOUR),ABSRP(LGP(IPT),IWAVE),DIFDN(IPT,IWAVE),&
!!!                                        DIFUP(IPT,IWAVE),DFLUX,BFLUX,SCATFX)
!!!                            print *,'after absrad',iwave,dflux(ipt,iwave),bflux(ipt,iwave),scatfx(ipt,iwave),RADABV(IHOUR,IWAVE)
!!!                        END DO
!!                
!!                
!!                
!!                
!!                
!!                
!!                
!!                
!!            enddo
!            
!            
!            
!        end if
!
!        
!    end do
     
     transmissionPercentage = TDP(IPTEST)
    
    

   end subroutine calculateTransmissionsOfTree
        
    
   subroutine findTreeFromConfig(xtestRev,ytestRev,ztestRev,treeState,timeis,yd_actual,treeConfigLocation)
    use MAINDECLARATIONS
    use MaespaConfigState
    use MaespaConfigStateUtils
    !use radn
    
    INTEGER xtestRev,ytestRev,ztestRev
    INTEGER vegHeight
    INTEGER loopCount
    INTEGER phyFile, strFile, treeFile
    TYPE(maespaConfigTreeMapState) :: treeState     
    !TYPE(maespaConfigvariablesstate) :: config
    real timeis
    INTEGER yd_actual
    real ZENlocal
    integer treeConfigLocation
    
    !integer TIME,IDOY,KHRS
    !real ALAT,BEARlocal,DEClocal,ZENlocal,AZlocal
    
    NAMELIST /CONTROL/ IOHRLY,IOTUTD,IOHIST,IORESP,IOWATBAL,IOFORMAT,ISUNLA,KEEPZEN
    
    ! Output file for errors and warnings
    OPEN (UERROR, FILE = 'Maeserr.dat', STATUS = 'UNKNOWN')
    OPEN (UCONTROL, FILE = 'confile.dat', STATUS = 'OLD',IOSTAT=IOERROR)
    IF(IOERROR.NE.0)THEN
        CALL SUBERROR('ERROR: CONFILE.DAT DOES NOT EXIST' ,IFATAL,0)
    ENDIF
    
    treeConfigLocation = -1
    
    !only load it once 
    !if (treeState%numberTreePlots.lt.1 .or. treeState%numberTreePlots.gt.100000000) then 
!        call readMaespaTreeMapFromConfig(treeState)
    !endif
    
    !print *,'number of tree plots', treeState%numberTreePlots
    
    vegHeight = 0 !if the tree location isn't found, then it will be 0 high
    !first check if the x,y is in the location list
    do loopCount = 1,treeState%numberTreePlots
        !print *,'xtestrev,ytestrev ',loopcount,xtestrev,ytestrev,treeState%xLocation(loopCount),treeState%yLocation(loopCount)
        !! this is +1 because of arrays 0 and 1 indexing problem
        if ( (xtestRev).eq.treeState%xLocation(loopCount)+1 .AND. (ytestRev).eq.treeState%yLocation(loopCount)+1 ) then
            !print *,'found tree at ',xtestRev,ytestRev
            vegHeight = treeState%treesHeight(loopCount)
            !print *,'tree config files ',treeState%treesfileNumber(loopCount)
            treeConfigLocation = loopCount
            
            !! instead of below, need to keep track of what tree
            !! find allocation and store it
            
!           call readMaespaStrConfigIntoState(state, treeState, treeStates)
            !call InitMaespaSingleTreeSingleLoop
 !           call READZEN(UCONTROL,NUMPNT,NOLAYI,PPLAYI,NZENI,NAZI,DIFZEN)
 !           call readMaespaTreeConfigFromConfig(treeState%phyFileNumber(loopCount), treeState%strFileNumber(loopCount), &
 !               treeState%treesfileNumber(loopCount), config)
!            print *,NUMPNT
!            DO IPT = 1,NUMPNT
!                print *,'SUNLA/BEXT before',config%SUNLA,config%BEXT
!                IHOUR=amod(timeis,24.)
!                
!                !! calculate before hand?
!                
!                print *,idate
!                print *,zen
!                print *,idoy
!                print *,alat
!                print *,yd_actual
!                call ZENAZ_1HOUR(timeis*2.0,IDOY,24,ALAT,BEARlocal,DEClocal,ZENlocal,AZlocal)
!                ! should set time, idoy, khrs=24, alat
!                
!                print *,idate
!                print *,zen
!                print *,idoy
!                print *,alat
!                print *,yd_actual
!                
!                
!                print *,radabv
!                ! RADABV(IHR,1) = DATAIN(IHR,METCOLS(MHPAR)) / UMOLPERJ
!                ! or 
!                ! RADABV(IHR,1) = DATAIN(IHR,METCOLS(MHRAD)) * FPAR
!                ! then for (*:,2)
!                ! CALL CALCNIR(RADABV,FBEAM)
!                ! then for (*:,3)
!                ! CALL THERMAL(TAIR,VPD,FSUN,RADABV)
!
!                
!                !print *,IHOUR
!                !! FBEAM(IHR,1) = CALCFBMH(IDATE,ZEN(IHR),RADABV(IHR,1))
!               
!                CALL TRANSB_1hour(IHOUR,config%IPROG,config%ZEN(IHOUR),config%AZ(IHOUR),config%XSLOPE,&
!                    config%YSLOPE,config%FBEAM,config%BEXTT,config%XL(IPT),YL(IPT),ZL(IPT),&
!                    config%RX,config%RY,config%RZ,config%DXT,config%DYT,config%DZT,config%XMAX,config%YMAX,config%SHADEHT,&
!                    config%FOLT,config%ZBC,config%JLEAFT,config%BPTT,config%NOAGECT,config%PROPCT,config%JSHAPET,&
!                    config%SHAPET,config%NOTREES,config%SUNLA,config%BEXT)  
!                print *,'SUNLA/BEXT after',config%SUNLA,config%BEXT
!                
!                
!                
!                
!                
!                
!!                ! Calculate the weighted pathlengths for beam radiation.
!!                        CALL TRANSB(IHOUR,IPROG,ZEN(IHOUR),AZ(IHOUR),XSLOPE,YSLOPE,FBEAM,BEXTT,XL(IPT),YL(IPT),ZL(IPT),&
!!                                    RX,RY,RZ,DXT,DYT,DZT,XMAX,YMAX,SHADEHT,FOLT,ZBC,JLEAFT,BPTT,NOAGECT,PROPCT,JSHAPET,&
!!                                    SHAPET,NOTREES,SUNLA,BEXT)                   
!!
!!
!!                                    
!!                        ! Loop over the 3 wavelengths
!!                        DO IWAVE = 1,3
!!                            ! Calculate the scattered radiation
!!                            !print *,'before scatter',iwave,DIFUP(ipt,iwave),DIFDN(ipt,iwave),SCLOST(ipt,iwave)
!!                            CALL SCATTER(IPT,IWAVE,MLAYER(IPT),LAYER(IPT),DLAI,EXPDIF,ZEN(IHOUR),BEXT,DMULT2,SOMULT,BMULT,&
!!                                            RADABV(IHOUR,IWAVE),FBEAM(IHOUR,IWAVE),TAIR(IHOUR),PREVTSOIL,ARHO(LGP(IPT),IWAVE),&
!!                                            ATAU(LGP(IPT),IWAVE),RHOSOL(IWAVE),DIFUP,DIFDN,SCLOST,DOWNTH)
!!                            print *,'after scatter',iwave,DIFUP(ipt,iwave),DIFDN(ipt,iwave),SCLOST(ipt,iwave),RADABV(IHOUR,IWAVE)
!!                  
!!                            ! Lost scattered radiation for each tree (W m-2), averaged over the grid points.
!!                            ! RAD June 2008.              
!!                            SCLOSTTREE(ITAR,1) = SUM(SCLOST(1:NUMPNT,1)) / NUMPNT
!!                            SCLOSTTREE(ITAR,2) = SUM(SCLOST(1:NUMPNT,2)) / NUMPNT
!!                            
!!                            ! Assume zero reflectance in TR waveband (Norman 1979)
!!                            ! But store in the same array the lost tranmission at top of canopy.
!!                            SCLOSTTREE(ITAR,3) = SUM(SCLOST(1:NUMPNT,2)) / NUMPNT
!!
!!                            ! Downwelling longwave radiation (calculated for each gridpoint
!!                            ! with the EHC) averaged across the grid points.
!!                            IF(IWAVE.EQ.3)DOWNTHTREE(ITAR) = SUM(DOWNTH) / NUMPNT
!!                  
!!                            ! Calculate absorbed radiation
!!                            !print *,'before absrad',iwave,dflux(ipt,iwave),bflux(ipt,iwave),scatfx(ipt,iwave)
!!                            CALL ABSRAD(IPT,IWAVE,NZEN,DEXT,BEXT,BMULT,RELDF(IPT),RADABV(IHOUR,IWAVE),&
!!                                        FBEAM(IHOUR,IWAVE),ZEN(IHOUR),ABSRP(LGP(IPT),IWAVE),DIFDN(IPT,IWAVE),&
!!                                        DIFUP(IPT,IWAVE),DFLUX,BFLUX,SCATFX)
!!                            print *,'after absrad',iwave,dflux(ipt,iwave),bflux(ipt,iwave),scatfx(ipt,iwave),RADABV(IHOUR,IWAVE)
!!                        END DO
!                
!                
!                
!                
!                
!                
!                
!                
!            enddo
            
            
            
        end if

        
    end do
    
    
     ! Read in an array of tree parameters from UFILE.
! NARRAY is the number of the array to be read (1 = RADX; 2 = RADY;
! 3 = HTCROWN; 4 = HTTRUNK; 5 = AREALEAF; 6 = DIAM; 7 = LEAFN (for understorey))
! Either read in values for all trees (NOALLTREES, up to MAXT trees) or
! read in average values. All values can be given for a series of dates.
!         ! Get radii in x & y directions of each tree
!    CALL READTREEARRAY(UTREES,1,NOALLTREES,NOXDATES,DATESX,R1)
!    CALL READTREEARRAY(UTREES,2,NOALLTREES,NOYDATES,DATESY,R2)
!    ! Get green crown height of each tree
!    CALL READTREEARRAY(UTREES,3,NOALLTREES,NOZDATES,DATESZ,R3)
!    ! Get trunk length of each tree
!    CALL READTREEARRAY(UTREES,4,NOALLTREES,NOTDATES,DATEST,TRUNK)
!    ! Get diameter of each tree
!    CALL READTREEARRAY(UTREES,6,NOALLTREES,NODDATES,DATESD,DIAMA)    


   end subroutine findTreeFromConfig
   
!subroutine saveMaespaTreeMapToFile(state)
!    use MAINDECLARATIONS
!    use MaespaConfigState
!    use MaespaConfigStateUtils
!    use maestcom2
!    
!    TYPE(maespaConfigTreeMapState), intent(IN) :: state
!    integer(kind=8) :: rec_len
!    
!    !inquire (IOLENGTH=rec_len) state
!    rec_len = c_sizeof(state)
!    open(1,file='MaespaTreeMapStateData.bin',status = 'unknown',form='unformatted', access='direct',recl=rec_len)
!    write(1,rec=1) state
!    close(1)
!       
!end subroutine saveMaespaTreeMapToFile
    
    
 subroutine readMaespaTreeMapFromConfig(state)
    use MAINDECLARATIONS
    use MaespaConfigState
    use MaespaConfigStateUtils
    use maestcom2

    
    character(len=1024) :: tmpfilename1
    character(len=1024) :: tmpfilename2
    character(len=1024) :: tmpfilename3
    character(len=1024) :: format_string
    integer loopcount

    INTEGER numberTreePlots
    INTEGER numberBuildingPlots
    INTEGER width,length
    
    NAMELIST /COUNT/ numberTreePlots
    NAMELIST /buildingcount/ numberBuildingPlots
    
    INTEGER, DIMENSION(:), ALLOCATABLE :: xlocation, ylocation
    INTEGER, DIMENSION(:), ALLOCATABLE :: xBuildingLocation, yBuildingLocation
    INTEGER, DIMENSION(:), ALLOCATABLE :: phyfileNumber,strfileNumber,treesfileNumber, treesHeight,trees
    INTEGER, DIMENSION(:), ALLOCATABLE :: buildingsHeight
    
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

    NAMELIST /location/ xlocation, ylocation, phyfileNumber, strfileNumber, treesfileNumber, treesHeight,trees
    NAMELIST /buildinglocation/   xBuildingLocation, yBuildingLocation, buildingsHeight
    
    NAMELIST /domain/ width,length,configTreeMapCentralArrayLength,configTreeMapCentralWidth,configTreeMapCentralLength,configTreeMapX,configTreeMapY,configTreeMapX1,configTreeMapX2,configTreeMapY1,configTreeMapY2,configTreeMapGridSize,configTreeMapNumsfcab,configTreeMapHighestBuildingHeight

!    TYPE(maespaConfigvariablesstate) :: treeState  !! 
    TYPE(maespaConfigTreeMapState), intent(OUT) :: state   !!
!    TYPE(maespaConfigvariablesstate) , DIMENSION(:), ALLOCATABLE :: treeStates    !! sub configs to store individual state of trees
     
    format_string = "(A5,I1)"
    
!    print *,in_path
    OPEN (UTREESMAP, FILE = 'treemap.dat', STATUS='OLD', IOSTAT=IOERROR)
    REWIND (UTREESMAP)
    READ (UTREESMAP, COUNT, IOSTAT = IOERROR)
    IF (IOERROR.NE.0) CALL SUBERROR('ERROR READING count DETAILS',IFATAL,IOERROR)

    state%numberTreePlots=numberTreePlots
    
    REWIND (UTREESMAP)
    READ (UTREESMAP, buildingcount, IOSTAT = IOERROR)
    IF (IOERROR.NE.0) CALL SUBERROR('ERROR READING buildingcount DETAILS',IFATAL,IOERROR)
    
    state%numberBuildingPlots=numberBuildingPlots
    
    allocate(xlocation(numberTreePlots))
    allocate(ylocation(numberTreePlots))
    allocate(phyfileNumber(numberTreePlots))
    allocate(strfileNumber(numberTreePlots))
    allocate(treesfileNumber(numberTreePlots))
    allocate(treesHeight(numberTreePlots))
    allocate(trees(numberTreePlots))
    
    allocate(xBuildingLocation(numberBuildingPlots))
    allocate(yBuildingLocation(numberBuildingPlots))
    allocate(buildingsHeight(numberBuildingPlots))

    
    REWIND (UTREESMAP)
    READ (UTREESMAP, location, IOSTAT = IOERROR)
    IF (IOERROR.NE.0) CALL SUBERROR('ERROR READING location DETAILS',IFATAL,IOERROR)

    state%xLocation=xLocation
    state%yLocation=yLocation
    state%phyfileNumber=phyfileNumber
    state%strfileNumber=strfileNumber
    state%treesfileNumber=treesfileNumber
    state%treesHeight=treesHeight
    state%trees=trees
    
    REWIND (UTREESMAP)
    READ (UTREESMAP, buildinglocation, IOSTAT = IOERROR)
    IF (IOERROR.NE.0) CALL SUBERROR('ERROR READING buildinglocation DETAILS',IFATAL,IOERROR)

    state%xBuildingLocation=xBuildingLocation
    state%yBuildingLocation=yBuildingLocation
    state%buildingsHeight=buildingsHeight    
       
    REWIND (UTREESMAP)
    READ (UTREESMAP, domain, IOSTAT = IOERROR)
    IF (IOERROR.NE.0) CALL SUBERROR('ERROR READING domain DETAILS',IFATAL,IOERROR)

    state%width=width
    state%length=length 
    state%configTreeMapCentralArrayLength=configTreeMapCentralArrayLength
    state%configTreeMapCentralWidth=configTreeMapCentralWidth
    state%configTreeMapCentralLength=configTreeMapCentralLength
    state%configTreeMapX=configTreeMapX
    state%configTreeMapY=configTreeMapY
    state%configTreeMapX1=configTreeMapX1
    state%configTreeMapX2=configTreeMapX2
    state%configTreeMapY1=configTreeMapY1
    state%configTreeMapY2=configTreeMapY2
    state%configTreeMapGridSize=configTreeMapGridSize
    state%configTreeMapNumsfcab=configTreeMapNumsfcab
    state%configTreeMapHighestBuildingHeight=configTreeMapHighestBuildingHeight
    
    
!    allocate(treeStates(numberTreePlots))
!    do loopCount= 1,state%numberTreePlots
!        call readMaespaTreeConfig(state%phyfileNumber(loopCount), state%strfileNumber(loopCount),&
!            state%treesfileNumber(loopCount), treeStates(loopCount))  !! returns config
!    end do
    
    end subroutine readMaespaTreeMapFromConfig
    
    
       
   subroutine getVegHeight(state, treeState, &
       !treeStates, &
       x, y, vegHeight)
    use MAINDECLARATIONS
    use MaespaConfigState
    use MaespaConfigStateUtils
    !use radn
    
    INTEGER x,y
    INTEGER vegHeight
    INTEGER loopCount
    INTEGER phyFile, strFile, treeFile
    TYPE(maespaConfigvariablesstate), intent(OUT) :: state
    TYPE(maespaConfigTreeMapState), intent(OUT) :: treeState     
!    TYPE(maespaConfigvariablesstate) , DIMENSION(:), ALLOCATABLE, intent(OUT) :: treeStates    !! sub configs to store individual state of trees
    
    !print *,'treeState%numberTreePlots',treeState%numberTreePlots
!    print *,x,y
    
    vegHeight = 0 !if the tree location isn't found, then it will be 0 high
    !first check if the x,y is in the location list
    do loopCount = 1,treeState%numberTreePlots
        if ( x.eq.treeState%xLocation(loopCount) .AND. y.eq.treeState%yLocation(loopCount) ) then
            print *,'yes at ',loopCount,x,y
            !figure out how tall the tree is
!            print *,treeStates(loopCount)%NOALLTREES
!            print *,treeStates(loopCount)%NOZDATES
!            print *,treeStates(loopCount)%DATESZ
!            print *,treeStates(loopCount)%R3
            print *,''
        end if

        
    end do
    
    
     ! Read in an array of tree parameters from UFILE.
! NARRAY is the number of the array to be read (1 = RADX; 2 = RADY;
! 3 = HTCROWN; 4 = HTTRUNK; 5 = AREALEAF; 6 = DIAM; 7 = LEAFN (for understorey))
! Either read in values for all trees (NOALLTREES, up to MAXT trees) or
! read in average values. All values can be given for a series of dates.
!         ! Get radii in x & y directions of each tree
!    CALL READTREEARRAY(UTREES,1,NOALLTREES,NOXDATES,DATESX,R1)
!    CALL READTREEARRAY(UTREES,2,NOALLTREES,NOYDATES,DATESY,R2)
!    ! Get green crown height of each tree
!    CALL READTREEARRAY(UTREES,3,NOALLTREES,NOZDATES,DATESZ,R3)
!    ! Get trunk length of each tree
!    CALL READTREEARRAY(UTREES,4,NOALLTREES,NOTDATES,DATEST,TRUNK)
!    ! Get diameter of each tree
!    CALL READTREEARRAY(UTREES,6,NOALLTREES,NODDATES,DATESD,DIAMA)    
    
!        call InitMaespaSingleTreeSingleLoop()
!        call InitDailyMaespaSingleTreeSingleLoop()
!        call SaveMaespaConfigState(state)
!        
!        call readMaespaTreeMap(treeState, treeStates)

    end subroutine getVegHeight

    
    
    
    
    
   subroutine getVegHeightFromConfig(x, y, vegHeight, treeState)
    use MAINDECLARATIONS
    use MaespaConfigState
    use MaespaConfigStateUtils
    !use radn
    
    INTEGER x,y
    INTEGER vegHeight
    INTEGER loopCount
    INTEGER phyFile, strFile, treeFile
!    TYPE(maespaConfigvariablesstate), intent(OUT) :: state
    TYPE(maespaConfigTreeMapState) :: treeState     
!    TYPE(maespaConfigvariablesstate) , DIMENSION(:), ALLOCATABLE, intent(OUT) :: treeStates    !! sub configs to store individual state of trees
    TYPE(maespaConfigvariablesstate) :: config
    
!    INTEGER LEN1,IOERROR,IWATFILE,IUSTFILE, ISUNLA, KEEPZEN
!    CHARACTER(LEN=*) CTITLE, TTITLE, PTITLE, STITLE, WTITLE, UTITLE
!    CHARACTER(LEN=*) in_path, out_path
!    CHARACTER(LEN=256) :: fin_dir, fout_dir
!    LOGICAL EXT
    NAMELIST /CONTROL/ IOHRLY,IOTUTD,IOHIST,IORESP,IOWATBAL,IOFORMAT,ISUNLA,KEEPZEN
    
    ! Output file for errors and warnings
    OPEN (UERROR, FILE = 'Maeserr.dat', STATUS = 'UNKNOWN')
    OPEN (UCONTROL, FILE = 'confile.dat', STATUS = 'OLD',IOSTAT=IOERROR)
    IF(IOERROR.NE.0)THEN
        CALL SUBERROR('ERROR: CONFILE.DAT DOES NOT EXIST' ,IFATAL,0)
    ENDIF
    
    !only load it once 
!    print *,treeState%numberTreePlots
    if (treeState%numberTreePlots.lt.1 .or. treeState%numberTreePlots.gt.100000000) then 
!        print *,'loading treestate'
        call readMaespaTreeMapFromConfig(treeState)
    endif
    
    
!    print *,'treeState%numberTreePlots',treeState%numberTreePlots
!    print *,x,y
    
    vegHeight = 0 !if the tree location isn't found, then it will be 0 high
    !first check if the x,y is in the location list
    do loopCount = 1,treeState%numberTreePlots
        if ( (x-1).eq.treeState%xLocation(loopCount) .AND. (y-1).eq.treeState%yLocation(loopCount) ) then
!            print *,'yes at ',loopCount,x,y, treeState%treesHeight(loopCount)
            
!            print *,'tree type',&
!              treeState%phyfileNumber,treeState%strfileNumber,treeState%treesfileNumber
            
!            print *,'tree type',&
!              treeState%phyfileNumber(loopCount),treeState%strfileNumber(loopCount),treeState%treesfileNumber(loopCount)
              
!!!            call readMaespaTreeConfigFromConfig(treeState%phyfileNumber(loopCount), treeState%strfileNumber(loopCount),&
!!!                treeState%treesfileNumber(loopCount), config)  !! returns config
              
            !figure out how tall the tree is
!            print *,'radii in x direction',config%R1(1,1)
!            print *,'radii in y direction',config%R2(1,1)
!            print *,'green crown height',config%R3(1,1)
!            print *,'trunk length',config%TRUNK(1,1)
!            print *,'diameter',config%DIAMA(1,1)
!            print *,''
!!!            vegHeight = nint(config%R3(1,1) + config%TRUNK(1,1) )
            vegHeight = treeState%treesHeight(loopCount)
        end if

        
    end do
    
    
     ! Read in an array of tree parameters from UFILE.
! NARRAY is the number of the array to be read (1 = RADX; 2 = RADY;
! 3 = HTCROWN; 4 = HTTRUNK; 5 = AREALEAF; 6 = DIAM; 7 = LEAFN (for understorey))
! Either read in values for all trees (NOALLTREES, up to MAXT trees) or
! read in average values. All values can be given for a series of dates.
!         ! Get radii in x & y directions of each tree
!    CALL READTREEARRAY(UTREES,1,NOALLTREES,NOXDATES,DATESX,R1)
!    CALL READTREEARRAY(UTREES,2,NOALLTREES,NOYDATES,DATESY,R2)
!    ! Get green crown height of each tree
!    CALL READTREEARRAY(UTREES,3,NOALLTREES,NOZDATES,DATESZ,R3)
!    ! Get trunk length of each tree
!    CALL READTREEARRAY(UTREES,4,NOALLTREES,NOTDATES,DATEST,TRUNK)
!    ! Get diameter of each tree
!    CALL READTREEARRAY(UTREES,6,NOALLTREES,NODDATES,DATESD,DIAMA)    
    
!        call InitMaespaSingleTreeSingleLoop()
!        call InitDailyMaespaSingleTreeSingleLoop()
!        call SaveMaespaConfigState(state)
!        
!        call readMaespaTreeMap(treeState, treeStates)

    end subroutine getVegHeightFromConfig
    
    
   subroutine getBuildingHeightFromConfig(x, y, buildingHeight, treeState)
    use MAINDECLARATIONS
    use MaespaConfigState
    use MaespaConfigStateUtils
    !use radn
    
    INTEGER x,y
    INTEGER buildingHeight
    INTEGER loopCount
    TYPE(maespaConfigTreeMapState) :: treeState     
    TYPE(maespaConfigvariablesstate) :: config

    NAMELIST /CONTROL/ IOHRLY,IOTUTD,IOHIST,IORESP,IOWATBAL,IOFORMAT,ISUNLA,KEEPZEN
    
    ! Output file for errors and warnings
    OPEN (UERROR, FILE = 'Maeserr.dat', STATUS = 'UNKNOWN')
    OPEN (UCONTROL, FILE = 'confile.dat', STATUS = 'OLD',IOSTAT=IOERROR)
    IF(IOERROR.NE.0)THEN
        CALL SUBERROR('ERROR: CONFILE.DAT DOES NOT EXIST' ,IFATAL,0)
    ENDIF
    
    !only load it once 
    if (treeState%numberTreePlots.lt.1 .or. treeState%numberTreePlots.gt.100000000) then 
        call readMaespaTreeMapFromConfig(treeState)
    endif
    
    buildingHeight = 0 !if the tree location isn't found, then it will be 0 high
    !first check if the x,y is in the location list
    do loopCount = 1,treeState%numberBuildingPlots
        if ( (x-1).eq.treeState%xBuildingLocation(loopCount) .AND. (y-1).eq.treeState%yBuildingLocation(loopCount) ) then
            buildingHeight = treeState%buildingsHeight(loopCount)
        end if
    end do
   
    end subroutine getBuildingHeightFromConfig    

  subroutine readMaespaStrConfigIntoState(state, treeState, treeStates)
    use MAINDECLARATIONS
    use MaespaConfigState
    use MaespaConfigStateUtils
!    use maespa_dyn_array
    !use radn
    
    INTEGER loopCount
    TYPE(maespaConfigvariablesstate), intent(OUT) :: state
    TYPE(maespaConfigTreeMapState), intent(OUT) :: treeState     
    INTEGER phyFile, strFile, treeFile
    TYPE(maespaConfigvariablesstate) , DIMENSION(:), ALLOCATABLE, intent(OUT) :: treeStates    !! sub configs to store individual state of trees
    
        call InitMaespaSingleTreeSingleLoop()
        call InitDailyMaespaSingleTreeSingleLoop()
        call SaveMaespaConfigState(state)
        
        call readMaespaTreeMap(treeState, treeStates)
        
        
    end subroutine readMaespaStrConfigIntoState

!subroutine readMaespaStrConfig
!    use MAINDECLARATIONS
!    !use radn
!    
!    call InitMaespaSingleTreeSingleLoop()
!    call InitDailyMaespaSingleTreeSingleLoop()
!    
!!    KHRS=24
!!    ITAR =1
!!    IDAY=0
!!    IHOUR = 15
!!    call MaespaSingleTreeSingleLoop()
!
!     
!!        KHRS=24
!!    ITAR =1
!!    do IDAY=0,30
!!     DO IHOUR = 1,KHRS
!!         print *,'loop start ',IHOUR
!!         call MaespaSingleTreeSingleLoop()
!
!    
!
!!    int NSPECIES, String STRFILES, int[] JLEAF, double[][][] BPT, double[] RANDOM, int[] NOAGEC,  
!!		int[] JSHAPE, double[] SHAPEC, double[] EXTWIND,                      
!!                    int[] NALPHA, double[][] ALPHA, double[][] FALPHA,                        
!!                    double[] COEFFT, double[] EXPONT, double[] WINTERC,                      
!!                    double[] BCOEFFT, double[] BEXPONT, double[] BINTERC,                    
!!                    double[] RCOEFFT, double[] REXPONT, double[] RINTERC, double[] FRFRAC, String in_path)
!!                    
!!                    
!!                    String in_path = "/home/kerryn/git/Maespa-OliveTree/";
!    
!!    KHRS=48
!!    HHRS = (KHRS) / 2.0
!!    SPERHR = 3600 * 24.0 / KHRS
!
!
!    
!    ! Get input from control file
!!    CALL INPUTCON(ISTART, IEND, NSTEP,NUMPNT, NOLAY, PPLAY, NZEN, DIFZEN, NAZ,      &
!!                    MODELGS, MODELJM, MODELRD, MODELSS, MODELRW, ITERMAX, IOHIST,   &
!!                    BINSIZE,ICC, CO2INC, TINC,IOTC, TOTC, WINDOTC, PAROTC,          &
!!                    FBEAMOTC, IWATFILE, IUSTFILE, ISIMUS, NSPECIES, SPECIESNAMES,   &
!!                    PHYFILES, STRFILES )
!  
!!    NSPECIES = 1
!!    PHYFILES(1) = 'phy.dat'
!!    STRFILES(1) = 'strolive.dat'
!!    in_path = '/home/kerryn/git/Maespa-OliveTree/'
!    
!!    CALL INPUTSTR(NSPECIES,STRFILES,JLEAFSPEC,BPTSPEC,RANDOMSPEC,NOAGECSPEC,    &
!!                    JSHAPESPEC,SHAPESPEC,EXTWINDSPEC,NALPHASPEC,ALPHASPEC,      &
!!                    FALPHASPEC,COEFFTSPEC,EXPONTSPEC,WINTERCSPEC,BCOEFFTSPEC,   &
!!                    BEXPONTSPEC,BINTERCSPEC,RCOEFFTSPEC,REXPONTSPEC,RINTERCSPEC,&
!!                    FRFRACSPEC,in_path)
!                    
!    !CALL RESTARTMETF(ISTART,MSTART,MFLAG)
!                    
!!    IDAY=19149
!!    ALAT=-0.622209907
!!    TTIMD=0
!!    DEC=0
!!    EQNTIM=0
!!    DAYL=0
!!    SUNSET=0             
!                    
!    !CALL SUN(IDAY,ALAT,TTIMD,DEC,EQNTIM,DAYL,SUNSET)
!    
!!    print *,IDAY
!!    print *,ALAT
!!    print *,TTIMD
!!    print *,DEC
!!    print *,EQNTIM
!!    print *,DAYL
!!    print *,SUNSET 
!    
!!    ALAT=-0.622209907
!!    TTIMD=0
!!    BEAR=0.785398185
!!    DEC=0.397277355
!!    EQNTIM=-0.020124469
!    
!    !CALL ZENAZ(ALAT,TTIMD,BEAR,DEC,EQNTIM,ZEN,AZ)
!    
!    !print *,zen
!    !print *,az
!    
!
!    ihour =20
!   
!   ! print *,NUMPNT
!!    nt=2
!!    print *,PID2
!
!    !CALL SLOPES(IHOUR, TTIMD, EQNTIM, ALAT, DEC, XSLOPE, YSLOPE, BEAR, ZEN(IHOUR), BMULT, DMULT2, SOMULT)
!!    print *,ZEN(IHOUR)
!!    print *,AZ(IHOUR),XSLOPE,YSLOPE
!!    print *,FBEAM
!!    print *,FBEAM(IHOUR,1),FBEAM(IHOUR,2)
!!    print *,BEXTT
!!
!!    print *,'start transb'  
!!    print *,IHOUR,IPROG,ZEN(IHOUR),AZ(IHOUR),XSLOPE,YSLOPE,FBEAM,BEXTT,XL(IPT),YL(IPT),ZL(IPT),&
!!          RX,RY,RZ,DXT,DYT,DZT,XMAX,YMAX,SHADEHT,FOLT,ZBC,JLEAFT,BPTT,NOAGECT,PROPCT,JSHAPET,&
!!          SHAPET,NOTREES,SUNLA,BEXT
!    DO IPT = 1,NUMPNT
!        
!        print *,IHOUR,IPROG,ZEN(IHOUR),AZ(IHOUR),XSLOPE,YSLOPE,FBEAM
!        print *,BEXTT
!        print *,XL(IPT),YL(IPT),ZL(IPT),&
!              RX,RY,RZ,DXT,DYT,DZT,XMAX,YMAX,SHADEHT,FOLT,ZBC,JLEAFT,BPTT,NOAGECT,PROPCT,JSHAPET,&
!              SHAPET,NOTREES,SUNLA,BEXT
!        CALL TRANSB_1hour(IHOUR,IPROG,ZEN(IHOUR),AZ(IHOUR),XSLOPE,YSLOPE,FBEAM,BEXTT,XL(IPT),YL(IPT),ZL(IPT),&
!              RX,RY,RZ,DXT,DYT,DZT,XMAX,YMAX,SHADEHT,FOLT,ZBC,JLEAFT,BPTT,NOAGECT,PROPCT,JSHAPET,&
!              SHAPET,NOTREES,SUNLA,BEXT)  
!    
!        CALL TRANSB(IHOUR,IPROG,ZEN(IHOUR),AZ(IHOUR),XSLOPE,YSLOPE,FBEAM,BEXTT,XL(IPT),YL(IPT),ZL(IPT),&
!              RX,RY,RZ,DXT,DYT,DZT,XMAX,YMAX,SHADEHT,FOLT,ZBC,JLEAFT,BPTT,NOAGECT,PROPCT,JSHAPET,&
!              SHAPET,NOTREES,SUNLA,BEXT)              
!        print *,sunla,bext,BPATH  
!    
!!        ihour = 12
!!        iwave = 1
!        CALL EHC(NUMPNT, TU, TD, TOTLAI, XSLOPE, YSLOPE, NAZ, NZEN, DIFZEN, DEXT, DLAI, EXPDIF, LAYER, MLAYER)
!        
!        print *,MLAYER(:)
!!        print *,LAYER(IPT)
!!        print *,ZEN(IHOUR)
!!        print *,RADABV(IHOUR,IWAVE)
!!        print *,FBEAM(IHOUR,IWAVE)
!!        print *,TAIR(IHOUR)
!!        print *,ARHO(LGP(IPT),IWAVE)
!!        print *,ATAU(LGP(IPT),IWAVE)
!!        print *,RHOSOL(IWAVE)
!!        print *,IPT,IWAVE,MLAYER(IPT),LAYER(IPT),DLAI,EXPDIF,ZEN(IHOUR),BEXT,DMULT2,SOMULT,BMULT,&
!!                          RADABV(IHOUR,IWAVE),FBEAM(IHOUR,IWAVE),TAIR(IHOUR),PREVTSOIL,ARHO(LGP(IPT),IWAVE),&
!!                          ATAU(LGP(IPT),IWAVE),RHOSOL(IWAVE),DIFUP,DIFDN,SCLOST,DOWNTH
!        
!        DO IWAVE = 1,3
!          ! Calculate the scattered radiation
!          CALL SCATTER(IPT,IWAVE,MLAYER(IPT),LAYER(IPT),DLAI,EXPDIF,ZEN(IHOUR),BEXT,DMULT2,SOMULT,BMULT,&
!                          RADABV(IHOUR,IWAVE),FBEAM(IHOUR,IWAVE),TAIR(IHOUR),PREVTSOIL,ARHO(LGP(IPT),IWAVE),&
!                          ATAU(LGP(IPT),IWAVE),RHOSOL(IWAVE),DIFUP,DIFDN,SCLOST,DOWNTH)
!
!!          ! Lost scattered radiation for each tree (W m-2), averaged over the grid points.
!!          ! RAD June 2008.              
!!          SCLOSTTREE(ITAR,1) = SUM(SCLOST(1:NUMPNT,1)) / NUMPNT
!!          SCLOSTTREE(ITAR,2) = SUM(SCLOST(1:NUMPNT,2)) / NUMPNT
!!
!!          ! Assume zero reflectance in TR waveband (Norman 1979)
!!          ! But store in the same array the lost tranmission at top of canopy.
!!          SCLOSTTREE(ITAR,3) = SUM(SCLOST(1:NUMPNT,2)) / NUMPNT
!!
!!          ! Downwelling longwave radiation (calculated for each gridpoint
!!          ! with the EHC) averaged across the grid points.
!!          IF(IWAVE.EQ.3)DOWNTHTREE(ITAR) = SUM(DOWNTH) / NUMPNT
!
!          ! Calculate absorbed radiation
!          CALL ABSRAD(IPT,IWAVE,NZEN,DEXT,BEXT,BMULT,RELDF(IPT),RADABV(IHOUR,IWAVE),&
!                      FBEAM(IHOUR,IWAVE),ZEN(IHOUR),ABSRP(LGP(IPT),IWAVE),DIFDN(IPT,IWAVE),&
!                      DIFUP(IPT,IWAVE),DFLUX,BFLUX,SCATFX)
!      END DO
!    enddo
!        print *,sunla,bext     
!
!end subroutine readMaespaStrConfig
















SUBROUTINE InitMaespaSingleTreeSingleLoop

    USE switches
    USE metcom
    USE maestcom
    
    USE maindeclarations
    
    IMPLICIT NONE
    REAL, EXTERNAL :: AVERAGEVAL,CALCRMW,TK,ETCAN,RESP,GRESP
   
    ! Set program flag
    IPROG = INORMAL
    IPROGUS = ITEST  ! Understorey setting.
    
    ! Did MGDK add these? Are they needed?
    USEMEASSW = 0
    SOILDATA = 0
    
    ! Temporary stuff ... will go into respiratory T acclimation routines.
    TAIRMEM = -999.99 ! All array elements...
    NTAIRADD = 0
    
    ! Set all the defaults stuff up
    CALL default_conditions(in_path, out_path)
   
    ! Open input files
    CALL OPENINPUTF(CTITLE,TTITLE,PTITLE,STITLE,WTITLE,UTITLE,IWATFILE, &
                    KEEPZEN,IPOINTS,ISIMUS,in_path,out_path)
   
    ! Decide whether to simulate the water balance (MAESPA) or not (MAESTRA)    
    IF(IWATFILE .EQ. 0)THEN
       ISMAESPA = .FALSE.
    ELSE
       ISMAESPA = .TRUE.
    ENDIF
    
    IF(ISMAESPA)THEN
        VTITLE = 'MAESPA'
    ELSE
        VTITLE = 'MAESTRA'
    ENDIF
    VTITLE = VTITLE(1:LEN_TRIM(VTITLE))
   
    ! Get input from control file
    CALL INPUTCON(ISTART, IEND, NSTEP,NUMPNT, NOLAY, PPLAY, NZEN, DIFZEN, NAZ,      &
                    MODELGS, MODELJM, MODELRD, MODELSS, MODELRW, ITERMAX, IOHIST,   &
                    BINSIZE,ICC, CO2INC, TINC,IOTC, TOTC, WINDOTC, PAROTC,          &
                    FBEAMOTC, IWATFILE, NSPECIES, SPECIESNAMES,   &
                    PHYFILES, STRFILES )
    
    ! Get input from canopy structure file
    CALL INPUTSTR(NSPECIES,STRFILES,JLEAFSPEC,BPTTABLESPEC,RANDOMSPEC,NOAGECSPEC,    &
                    JSHAPESPEC,SHAPESPEC,EXTWINDSPEC,NALPHASPEC,ALPHASPEC,      &
                    FALPHASPEC,COEFFTSPEC,EXPONTSPEC,WINTERCSPEC,BCOEFFTSPEC,   &
                    BEXPONTSPEC,BINTERCSPEC,RCOEFFTSPEC,REXPONTSPEC,RINTERCSPEC,&
                    FRFRACSPEC,in_path,DATESLIA,NOLIADATES,DATESLAD,NOLADDATES)
    
    ! Get input from physiology file
    CALL INPUTPHY(NSPECIES,PHYFILES,MODELJM,MODELRD,MODELGS,MODELRW,NOLAY,NOAGECSPEC,           &
                    NOAGEPSPEC,PROPCSPEC,PROPPSPEC,ABSRPSPEC,ARHOSPEC,ATAUSPEC,RHOSOLSPEC,      &
                    JMAXTABLESPEC,DATESJSPEC,NOJDATESSPEC,IECOSPEC,EAVJSPEC,EDVJSPEC,           &
                    DELSJSPEC,THETASPEC,VCMAXTABLESPEC,DATESVSPEC,NOVDATESSPEC,EAVCSPEC,        &
                    EDVCSPEC,DELSCSPEC,TVJUPSPEC,TVJDNSPEC,SLATABLESPEC,DATESSLASPEC,           &
                    NOSLADATESSPEC,NOADATESSPEC,DATESASPEC,AJQTABLESPEC,RDTABLESPEC,            &
                    DATESRDSPEC,NORDATESSPEC,RTEMPSPEC,DAYRESPSPEC,TBELOWSPEC,EFFYRWSPEC,       &
                    RMWSPEC,RTEMPWSPEC,COLLASPEC,COLLKSPEC,STEMSDWSPEC,RMWAREASPEC,STEMFORMSPEC,&
                    NOFQDATESSPEC,DATESFQSPEC,Q10FTABLESPEC,K10FSPEC,NOWQDATESSPEC,DATESWQSPEC, &
                    Q10WTABLESPEC,RMFRSPEC,RMCRSPEC,Q10RSPEC,RTEMPRSPEC,EFFYRFSPEC,RMBSPEC,     &
                    Q10BSPEC,RTEMPBSPEC,GSREFSPEC,GSMINSPEC,PAR0SPEC,D0SPEC,VK1SPEC,VK2SPEC,    &
                    VPD1SPEC,VPD2SPEC,VMFD0SPEC,GSJASPEC,GSJBSPEC,T0SPEC,TREFSPEC,TMAXSPEC,     &
                    SMD1SPEC,SMD2SPEC,WC1SPEC, WC2SPEC,SWPEXPSPEC,G0TABLESPEC,G1TABLESPEC,      &
                    GKSPEC,NOGSDATESSPEC,DATESGSSPEC,D0LSPEC,GAMMASPEC,VPDMINSPEC,WLEAFTABLESPEC,DATESWLEAFSPEC,NOWLEAFDATESSPEC,NSIDESSPEC,           &
                    SFSPEC,PSIVSPEC,VPARASPEC,VPARBSPEC,VPARCSPEC,VFUNSPEC,in_path)
    
    ! Cannot use Tuzet with MAESTRA (because plantk is in watpars.dat!)
    IF(.NOT.ISMAESPA.AND.MODELGS.EQ.6)THEN
        CALL SUBERROR('Error: Cannot use Tuzet model in MAESTRA. Use MAESPA!', IFATAL, 0)
    ENDIF
    
    ! Get input from trees file
    CALL INPUTTREE(XSLOPE,YSLOPE,BEAR,X0,Y0,XMAX,YMAX,PLOTAREA,STOCKING,ZHT,Z0HT,ZPD, &
                    NOALLTREES,NOTREES,NOTARGETS,ITARGETS,SHADEHT,NOXDATES, &
                    NOYDATES,NOZDATES,NOTDATES,NOLADATES,NODDATES,DATESX,   &
                    DATESY,DATESZ,DATEST,DATESLA,DATESD,DXT1,DYT1,DZT1,     &
                    RXTABLE1,RYTABLE1,RZTABLE1,ZBCTABLE1,FOLTABLE1,         &
                    TOTLAITABLE,DIAMTABLE1,IFLUSH,DT1,DT2,DT3,DT4,EXPTIME,  &
                    APP,EXPAN,WEIGHTS,NSPECIES,ISPECIES)
    
    ! Get input from the water balance file
    IF(ISMAESPA)THEN        
        CALL INPUTWATBAL(BPAR, PSIE, KSAT, ROOTRESIST, ROOTRESFRAC, ROOTRADTABLE, ROOTDENSTABLE,ROOTMASSTOTTABLE,              &
                        MINROOTWP,MINLEAFWPSPEC,PLANTKTABLE,KSCALING,THROUGHFALL,REASSIGNRAIN,RUTTERB,RUTTERD, MAXSTORAGE, &
                        DRAINLIMIT,ROOTXSECAREA,EQUALUPTAKE,NLAYER, NROOTLAYER, LAYTHICK, INITWATER,    & 
                        FRACROOTTABLE, POREFRAC, SOILTEMP, KEEPWET,DRYTHICKMIN,TORTPAR, SIMTSOIL,RETFUNCTION,&
                        FRACORGANIC, EXPINF, WSOILMETHOD, USEMEASET,USEMEASSW,SIMSOILEVAP,USESTAND,ALPHARET,WS,WR,NRET,&
                        DATESKP,NOKPDATES,DATESROOT,NOROOTDATES,NOROOTSPEC)
    ENDIF
    


                
    
    ! Open met data file (must be done after ISTART & IEND read)
    CALL OPENMETF(ISTART,IEND,CAK,PRESSK,SWMIN,SWMAX,USEMEASET,DIFSKY,ALAT,TTIMD,DELTAT,&
                    MFLAG,METCOLS,NOMETCOLS,MTITLE,MSTART,in_path)
    
    ! Open output files
    CALL open_output_files(ISIMUS,CTITLE,TTITLE,PTITLE,STITLE,MTITLE,VTITLE,WTITLE,NSPECIES,SPECIESNAMES,out_path,ISMAESPA)
    
    
    IF(ISIMUS.EQ.1)THEN
        CALL INPUTUSSTR(NOUSPOINTS,X0,Y0,GRDAREAI,XLU,YLU,ZLU,USLAITAB,NOFUDATES,DATESFU,&
                        HTUS,NOHUDATES,DATESHU,FOLNUS,NONUDATES,DATESNU,EXTKUS)
        
        CALL INPUTUSPHY(JMAXN25,IECOU,EAVJU,EDVJU,DELSJU,TVJUPU,TVJDNU,VCMAXN25,EAVCU,    &
                        EDVCU,DELSCU,UNMIN,AJQU,ABSRPU,GSBG0U,GSBG1U,CICARAT,RD0US,RDK,   &
                        RDT,SLAUS,EFFY,MOSS,JMAX25M,VCMAX25M,THETAM,C4FRAC,               &
                        VCMAXC4,TVJUPC4, TVJDNC4, DELSCC4, EAVCC4, EDVCC4, CICAC4)
    ENDIF
    
    ! Read MAESTEST input file.
    ! Open files and read information about points
    IF(IPOINTS .EQ. 1)THEN
      CALL GETPOINTSF(NUMTESTPNT,XLP,YLP,ZLP,X0,Y0,XMAX,YMAX, &
         CTITLE,TTITLE,MTITLE,STITLE,VTITLE)
    ENDIF
      
    ! Initialize various variables related to water balance calculations.
    IF(ISMAESPA)THEN
    CALL INITWATBAL(LAYTHICK,WETTINGBOT,WETTINGTOP,POREFRAC,WATERGAIN,WATERLOSS,PPTGAIN,    &
                    INITWATER,DRYTHICKMIN,DRYTHICK,CANOPY_STORE,SURFACE_WATERMM,FRACWATER,  &
                    WSOIL,WSOILROOT,NLAYER,NROOTLAYER,ICEPROP,QE,RUNOFF,OUTFLOW,SOILDEPTH,  &
                    SOILDATA,USEMEASSW)
    ENDIF
    
END  SUBROUTINE InitMaespaSingleTreeSingleLoop  





SUBROUTINE InitDailyMaespaSingleTreeSingleLoop


    USE switches
    USE metcom
    USE maestcom
    use maespa_modules
!    use utils
!    
    USE maindeclarations
    
    IMPLICIT NONE
    
    REAL, EXTERNAL :: AVERAGEVAL

    
    
     !***********************************************************************!
    !                       Begin daily loop                                !
    !***********************************************************************!
    
    ! Initialize met file
    CALL RESTARTMETF(ISTART,MSTART,MFLAG)
    
    IDAY = 0
!!    DO WHILE (ISTART + IDAY <= IEND)
!!        WRITE(*,105) IDAY
!!        105 FORMAT('  DAY:',I5)
       
        !**********************************************************************

        ! Added 29-3-2008 (RAD): reposition met file correctly,
        ! to account for the looping order change.
        CALL RESTARTMETF(IDAY+ISTART,MSTART,MFLAG)
       
        ! Prepare histogram
        CALL ZEROSTART(HISTO,CANOPYDIMS)

        ! Calculate zenith angle of sun
        CALL SUN(IDAY+ISTART,ALAT,TTIMD,DEC,EQNTIM,DAYL,SUNSET)
        CALL ZENAZ(ALAT,TTIMD,BEAR,DEC,EQNTIM,ZEN,AZ)
!        print *,ALAT,TTIMD,BEAR,DEC,EQNTIM,ZEN,AZ
 
        ! If requested in the confile, reset zenith angle to that read on the first day.
        ! (For simulation experiments; keep day of year the same).
        IF(KEEPZEN.EQ.1.AND.IDAY.EQ.0)THEN  ! Save first day.
           DEC0 = DEC
           EQNTIM0 = EQNTIM
           DAYL0 = DAYL
           SUNSET0 = SUNSET
           ZEN0 = ZEN
           AZ0 = AZ
        ENDIF
        IF(KEEPZEN.EQ.1.AND.IDAY.GT.0)THEN  ! Read first day.
           DEC = DEC0
           EQNTIM = EQNTIM0
           DAYL = DAYL0
           SUNSET = SUNSET0
           ZEN = ZEN0
           AZ = AZ0
        ENDIF
 
        ! Get meteorological data
        CALL GETMET(IDAY+ISTART,MFLAG,ZEN,METCOLS,NOMETCOLS,CAK,PRESSK,SWMIN,SWMAX,DELTAT,  &
                    ALAT,DEC,DAYL,WINDAH,TSOIL,TAIR,RADABV,FBEAM,RH,VPD,VMFD,CA,PRESS,      &
                    PPT,SOILMOIST,SOILDATA,TSOILDATA,ETMEAS)
                    
      
        ! Moving average air temperature (for acclimation of respiration - not currently documented feature).
        MOVEWINDOW = 7 * KHRS
        TAIRMEM = CSHIFT(TAIRMEM, -KHRS)
        CALL REVARRAY(TAIR, MAXHRS, KHRS, TAIRR)
        TAIRMEM(1:KHRS) = TAIRR(1:KHRS)
        NTAIRADD = NTAIRADD + KHRS ! Keep track of how many tairs remembered.
        IWHICH = MIN(MOVEWINDOW,NTAIRADD)
        TMOVE = SUM(TAIRMEM(1:IWHICH))/REAL(IWHICH)

        ! Rain is halfhourly, re-assign it here to half-hourly values if option is set.
        ! Useful if rain data is really daily, and want to make realistic HH data.
        IF(REASSIGNRAIN.EQ.1)THEN
            PPTDAY = SUM(PPT)
            CALL ASSIGNRAIN(PPTDAY,PPT)
        ENDIF
                
        IF (ICC.EQ.0) CALL ALTERMETCC(CA,TAIR,TSOIL,RH,VPD,VMFD,PRESS,CO2INC,TINC)
        IF (IOTC.EQ.0) CALL ALTERMETOTC(TOTC,WINDOTC,PAROTC,FBEAMOTC,TAIR,TSOIL,WINDAH,RADABV,&
                                        FBEAM,RH,VPD,VMFD,PRESS)
         
        !********************************************************************
        ! Zero daily fluxes
       
        CALL ZEROD(TDYAB,TOTCO2,TOTRESPF,TOTRESPWM,TOTRESPB,TOTRESPCR,TOTRESPFR,TOTH2O,TOTHFX,&
                    WSOILMEAN,WSOILROOTMEAN,SWPMEAN,PPTTOT,ETMMTOT,ETMEASTOT,DISCHARGETOT,SOILEVAPTOT,&
                    FSOILMEAN,TFALLTOT,QHTOT,QETOT,QNTOT,QCTOT,RADINTERCTOT)
        
        NSUMMEDW = 0
         
        ! Zero hourly fluxes
        CALL ZEROHR(THRAB,FCO2,FRESPF,FRESPW,FRESPB,FRESPFR,FRESPCR,FH2O,GSCAN,GBHCAN,FHEAT,PPAR,PPS,&
                    PTRANSP,TCAN,FSOIL1,PSILCAN,PSILCANMIN,CICAN,NSUMMED,TOTTMP,ECANMAX,ACANMAX)

        !**********************************************************************
        ! Do understorey calculations
        IF(ISIMUS .EQ. 1)THEN
            IF((MOD(IDAY,IOTUTD).EQ.0).OR.IDAY.EQ.0)THEN

                ! Sort trees to middle of understorey points.
                AX = AVERAGEVAL(XLU,NOUSPOINTS)
                AY = AVERAGEVAL(YLU,NOUSPOINTS)
                
                ! Sort overstorey dimensions, save in separate arrays.
                ! Can move this out of loop, only needs to be done once.
                CALL SORTTREESP(AX,AY,NOALLTREES,NOTREES,DXT1,DYT1,DZT1,RXTABLE1,RYTABLE1,RZTABLE1,     &
                                ZBCTABLE1,FOLTABLE1,DIAMTABLE1,DXTUS,DYTUS,DZTUS,RXTABLEUS,RYTABLEUS,   &
                                RZTABLEUS,FOLTABLEUS,ZBCTABLEUS,DIAMTABLEUS,ISPECIES,ISPECIESTUS,ITUUS)
    
                ! Interpolate overstorey dimensions for use in understorey calcs.
                CALL INTERPOLATET(IDAY,ISTART,IHOUR,NOXDATES,DATESX,RXTABLEUS,NOYDATES,DATESY,RYTABLEUS,    &
                                    NOZDATES,DATESZ,RZTABLEUS,NOTDATES,DATEST,ZBCTABLEUS,NODDATES,DATESD,   &
                                    DIAMTABLEUS,NOLADATES,DATESLA,FOLTABLEUS,TOTLAITABLE,NOTREES,           &
                                    RXUS,RYUS,RZUS,ZBCUS,FOLTUS,TOTLAI,DIAM,STOCKING,IFLUSH,DT1,DT2,DT3,    &
                                    DT4,EXPTIME,APP,EXPAN,NEWCANOPY,CANOPYDIMS)
         
                ! Interpolate understorey dimensions
                CALL INTERPUS(IDAY,ISTART,NOUSPOINTS,UNMIN,EXTKUS,GRDAREAI,DATESFU,NOFUDATES,USLAITAB,USLAI,&
                                DATESHU,NOHUDATES,HTUS,ZLU,DATESNU,NONUDATES,FOLNUS,FN0US,AREAUS)

                ! Make tree arrays of radiation-extinction related parameters,
                ! that may vary between species.
                DO I = 1,NOTREES
                    JSHAPETUS(I) = JSHAPESPEC(ISPECIESTUS(I))
                    SHAPETUS(I) = SHAPESPEC(ISPECIESTUS(I))
                    DEXTTUS(I,1:MAXANG) = DEXTSPEC(ISPECIESTUS(I),1:MAXANG)
                    JLEAFTUS(I) = JLEAFSPEC(ISPECIESTUS(I))
                    NOAGECTUS(I) = NOAGECSPEC(ISPECIESTUS(I))
                    BPTTUS(1:8,1:MAXC,I) = BPTSPEC(1:8,1:MAXC,ISPECIESTUS(I))
                    PROPPTUS(1:MAXC,I) = PROPPSPEC(1:MAXC,ISPECIESTUS(I))
                    PROPCTUS(1:MAXC,I) = PROPCSPEC(1:MAXC,ISPECIESTUS(I))
                END DO
                
                ! Diffuse transmission to the understorey points.
                CALL TRANSD(IDAY,IOTUTD,NEWCANOPY,IPROGUS,NOTREES,XSLOPE,YSLOPE,NZEN,DIFZEN,NAZ,NOUSPOINTS, &
                            DEXTTUS,DIFSKY,XLU,YLU,ZLU,RXUS,RYUS,RZUS,DXTUS,DYTUS,DZTUS,XMAX,YMAX,SHADEHT,  &
                            FOLTUS,ZBCUS,JLEAFTUS,BPTTUS,NOAGECTUS,PROPCT,JSHAPETUS,SHAPETUS,NEWTUTD,TUUS,  &
                            TDUS,RELDFUS,DEXT)   
            ENDIF
        ENDIF  ! ISIMUS.EQ.1
    
        ! Loop through all trees, calculate diffuse transmittances.
        ! Once every IOTUTD days:    
        IF((MOD(IDAY,IOTUTD).EQ.0).OR.IDAY.EQ.0)THEN
            DO ITAR = 1,NOTARGETS
                ITREE = ITARGETS(ITAR)
                ISPEC = ISPECIES(ITREE)

                JLEAF = JLEAFSPEC(ISPEC)
                JSHAPE = JSHAPESPEC(ISPEC)
                SHAPE = SHAPESPEC(ISPEC)

                BPT(1:8,1:MAXC) = BPTSPEC(1:8,1:MAXC,ISPEC)
                RANDOM = RANDOMSPEC(ISPEC)
                NOAGEC = NOAGECSPEC(ISPEC)
                EXTWIND = EXTWINDSPEC(ISPEC)
                NALPHA = NALPHASPEC(ISPEC)
                ALPHA(1:MAXANG) = ALPHASPEC(1:MAXANG,ISPEC)
                FALPHA(1:MAXANG) = FALPHASPEC(1:MAXANG,ISPEC)
                COEFFT = COEFFTSPEC(ISPEC)
                EXPONT = EXPONTSPEC(ISPEC)
                WINTERC = WINTERCSPEC(ISPEC)
                BCOEFFT = BCOEFFTSPEC(ISPEC)
                BEXPONT = BEXPONTSPEC(ISPEC)
                BINTERC = BINTERCSPEC(ISPEC)
                RCOEFFT = RCOEFFTSPEC(ISPEC)
                REXPONT = REXPONTSPEC(ISPEC)
                RINTERC = RINTERCSPEC(ISPEC)
                FRFRAC = FRFRACSPEC(ISPEC)

                NOAGEP = NOAGEPSPEC(ISPEC)       
                PROPC(1:MAXC) = PROPCSPEC(1:MAXC,ISPEC)
                PROPP(1:MAXC) = PROPPSPEC(1:MAXC,ISPEC)    
                ABSRP(1:MAXLAY,1:3) = ABSRPSPEC(1:MAXLAY,1:3,ISPEC)  
                ARHO(1:MAXLAY,1:3) = ARHOSPEC(1:MAXLAY,1:3,ISPEC)      
                ATAU(1:MAXLAY,1:3) = ATAUSPEC(1:MAXLAY,1:3,ISPEC)       
                RHOSOL(1:3) = RHOSOLSPEC(1:3,ISPEC)    
                JMAXTABLE(1:MAXDATE,1:MAXLAY,1:MAXC) = JMAXTABLESPEC(1:MAXDATE,1:MAXLAY,1:MAXC,ISPEC) 
                DATESJ(1:MAXDATE) = DATESJSPEC(1:MAXDATE,ISPEC)       
                NOJDATES = NOJDATESSPEC(ISPEC)     
                IECO = IECOSPEC(ISPEC)         
                EAVJ = EAVJSPEC(ISPEC)         
                EDVJ = EDVJSPEC(ISPEC)         
                DELSJ = DELSJSPEC(ISPEC)        
                THETA = THETASPEC(ISPEC)        
                VCMAXTABLE(1:MAXDATE,1:MAXLAY,1:MAXC) = VCMAXTABLESPEC(1:MAXDATE,1:MAXLAY,1:MAXC,ISPEC)   
                DATESV(1:MAXDATE) =  DATESVSPEC(1:MAXDATE,ISPEC)         
                NOVDATES =  NOVDATESSPEC(ISPEC)     
                EAVC = EAVCSPEC(ISPEC)         
                EDVC = EDVCSPEC(ISPEC)         
                DELSC = DELSCSPEC(ISPEC)        
                TVJUP = TVJUPSPEC(ISPEC)        
                TVJDN = TVJDNSPEC(ISPEC)        
                SLATABLE(1:maxdate,1:MAXLAY,1:MAXC) = SLATABLESPEC(1:MAXDATE,1:MAXLAY,1:MAXC,ISPEC)
                DATESSLA(1:maxdate) =  DATESSLASPEC(1:maxdate,ISPEC)       
                NOSLADATES = NOSLADATESSPEC (ISPEC)  
                NOADATES = NOADATESSPEC(ISPEC)   
                DATESA(1:MAXDATE) =  DATESASPEC(1:MAXDATE,ISPEC)         
                AJQTABLE(1:MAXDATE,1:MAXLAY,1:MAXC) = AJQTABLESPEC(1:MAXDATE,1:MAXLAY,1:MAXC,ISPEC)     
                RDTABLE(1:MAXDATE,1:MAXLAY,1:MAXC) = RDTABLESPEC(1:MAXDATE,1:MAXLAY,1:MAXC,ISPEC)      
                DATESRD(1:MAXDATE) =  DATESRDSPEC(1:MAXDATE,ISPEC)        
                NORDATES = NORDATESSPEC(ISPEC)     
                RTEMP = RTEMPSPEC(ISPEC)        
                DAYRESP = DAYRESPSPEC(ISPEC)      
                TBELOW = TBELOWSPEC(ISPEC)       
                EFFYRW = EFFYRWSPEC(ISPEC)       
                RMW = RMWSPEC(ISPEC)          
                RTEMPW = RTEMPWSPEC(ISPEC)       
                COLLA = COLLASPEC(ISPEC)        
                COLLK = COLLKSPEC(ISPEC)        
                STEMSDW = STEMSDWSPEC(ISPEC)      
                RMWAREA = RMWAREASPEC(ISPEC)      
                STEMFORM = STEMFORMSPEC(ISPEC)     
                NOFQDATES = NOFQDATESSPEC(ISPEC)    
                DATESFQ(1:MAXDATE) =  DATESFQSPEC(1:maxdate,ISPEC)        
                Q10FTABLE(1:MAXDATE) =  Q10FTABLESPEC(1:maxdate,ISPEC)    
                K10F = K10FSPEC(ISPEC)
                NOWQDATES = NOWQDATESSPEC(ISPEC)    
                DATESWQ = DATESWQSPEC(1:MAXDATE,ISPEC)      
                Q10WTABLE(1:maxdate) =  Q10WTABLESPEC(1:MAXDATE,ISPEC)    
                RMFR = RMFRSPEC(ISPEC)         
                RMCR = RMCRSPEC(ISPEC)         
                Q10R = Q10RSPEC(ISPEC)         
                RTEMPR = RTEMPRSPEC(ISPEC)       
                EFFYRF = EFFYRFSPEC(ISPEC)       
                RMB = RMBSPEC(ISPEC)          
                Q10B = Q10BSPEC(ISPEC)         
                RTEMPB = RTEMPBSPEC(ISPEC)      
                GSREF = GSREFSPEC(ISPEC)        
                GSMIN = GSMINSPEC(ISPEC)        
                PAR0 = PAR0SPEC(ISPEC)         
                D0 = D0SPEC(ISPEC)           
                VK1 = VK1SPEC(ISPEC)         
                VK2 = VK2SPEC(ISPEC)          
                VPD1 = VPD1SPEC(ISPEC)         
                VPD2 = VPD2SPEC(ISPEC)         
                VMFD0 = VMFD0SPEC(ISPEC)        
                GSJA = GSJASPEC(ISPEC)         
                GSJB = GSJBSPEC(ISPEC)         
                T0 = T0SPEC(ISPEC)           
                TREF = TREFSPEC(ISPEC)        
                TMAX = TMAXSPEC(ISPEC)         
                SMD1 = SMD1SPEC(ISPEC)         
                SMD2 = SMD2SPEC(ISPEC)        
                WC1 = WC1SPEC(ISPEC)          
                WC2 = WC2SPEC(ISPEC)          
                SWPEXP = SWPEXPSPEC(ISPEC)       
                G0TABLE = G0TABLESPEC(1:maxdate,ISPEC)
                G1TABLE = G1TABLESPEC(1:maxdate,ISPEC)              
                DATESGS = DATESGSSPEC(1:maxdate,ISPEC)
                NOGSDATES = NOGSDATESSPEC(ISPEC)

                D0L = D0LSPEC(ISPEC)          
                GAMMA = GAMMASPEC(ISPEC)     
                VPDMIN = VPDMINSPEC(ISPEC)   
                SF = SFSPEC(ISPEC)
                PSIV = PSIVSPEC(ISPEC)
                
                WLEAF = WLEAFSPEC(ISPEC)        
                NSIDES = NSIDESSPEC(ISPEC)
                
                VPARA = VPARASPEC(ISPEC)
                VPARB = VPARBSPEC(ISPEC)
                VPARC = VPARCSPEC(ISPEC)
                VFUN  = VFUNSPEC(ISPEC)

                ! Sort the trees every timestep.
                !! Arrays DXT through IT should be assigned here, sorting to be done
                !! outside loop, stored in larger array.
                CALL SORTTREES(NOALLTREES,NOTREES,ITREE,DXT1,DYT1,DZT1,RXTABLE1,RYTABLE1,RZTABLE1,  &
                                ZBCTABLE1,FOLTABLE1,DIAMTABLE1,DXT,DYT,DZT,RXTABLE,RYTABLE,RZTABLE, &
                                FOLTABLE,ZBCTABLE,DIAMTABLE,ISPECIES,ISPECIEST,IT)
                
                ! Interpolate to get daily values of parameters
                ! This we can probably also do outside the hourly loop.
                CALL INTERPOLATEP(IDAY,ISTART,NOJDATES,DATESJ,JMAXTABLE,NOVDATES,DATESV,VCMAXTABLE, &
                                    NORDATES,DATESRD,RDTABLE,NOSLADATES,DATESSLA,SLATABLE,NOADATES, &
                                    DATESA,AJQTABLE,NOFQDATES,DATESFQ,Q10FTABLE,NOWQDATES,DATESWQ,  &
                                    Q10WTABLE,NOLAY,NOAGEP,JMAX25,VCMAX25,RD0,SLA,AJQ,Q10F,Q10W,    &
                                    NOGSDATES,DATESGS,G0TABLE,G1TABLE,G0,G1)
         
                CALL INTERPOLATET(IDAY,ISTART,IHOUR,NOXDATES,DATESX,RXTABLE,NOYDATES,DATESY,RYTABLE,    &
                                    NOZDATES,DATESZ,RZTABLE,NOTDATES,DATEST,ZBCTABLE,NODDATES,DATESD,   &
                                    DIAMTABLE,NOLADATES,DATESLA,FOLTABLE,TOTLAITABLE,NOTREES,RX,RY,RZ,  &
                                    ZBC,FOLT,TOTLAI,DIAM,STOCKING,IFLUSH,DT1,DT2,DT3,DT4,EXPTIME,APP,   &
                                    EXPAN,NEWCANOPY,CANOPYDIMS)
               
                !!!! can move this outside the loop as well... !!!!
                DO I = 1,NOTREES
                    JSHAPET(I) = JSHAPESPEC(ISPECIEST(I))
                    SHAPET(I) = SHAPESPEC(ISPECIEST(I))
                    DEXTT(I,1:MAXANG) = DEXTSPEC(ISPECIEST(I),1:MAXANG)
                    JLEAFT(I) = JLEAFSPEC(ISPECIEST(I))
                    NOAGECT(I) = NOAGECSPEC(ISPECIEST(I))
                    BPTT(1:8,1:MAXC,I) = BPTSPEC(1:8,1:MAXC,ISPECIEST(I))
                    PROPPT(1:MAXC,I) = PROPPSPEC(1:MAXC,ISPECIEST(I))
                    PROPCT(1:MAXC,I) = PROPCSPEC(1:MAXC,ISPECIEST(I))
                END DO

                CALL POINTSNEW(NOLAY,PPLAY,JLEAF,JSHAPE,SHAPE,RX(1),RY(1),RZ(1),ZBC(1),DXT(1),DYT(1),DZT(1),  &
                                FOLT(1),PROPC,PROPP, &
                                BPT,NOAGEC,NOAGEP, XL,YL,ZL,VL,DLT,DLI,LGP,FOLLAY)
            
                ! Output xyz coordinates of crown grid points.
                ! This is actually useful; might make this an output option.
!                 Note in R: 
!                  wattestColnames <- c("x","y","z"); wattestFilename <- "wattest.dat";wattestdata_table <- read.table(wattestFilename,header = FALSE, col.names=wattestColnames);require(rgl);plot3d(wattestdata_table$x,wattestdata_table$y,wattestdata_table$z);
                                ! can plot the points nicely...
                        DO I =1,NUMPNT
                            WRITE(UWATTEST,891)XL(I),YL(I),ZL(I)
                        END DO
                891     FORMAT (6(F6.2,1X))
                
                close(UWATTEST)

                ! Calculate diffuse transmittances
                CALL TRANSD(IDAY,IOTUTD,NEWCANOPY,IPROG,NOTREES,XSLOPE,YSLOPE,NZEN,DIFZEN,NAZ,NUMPNT,DEXTT,             &
                            DIFSKY,XL,YL,ZL,RX,RY,RZ,DXT,DYT,DZT,XMAX,YMAX,SHADEHT,FOLT,ZBC,JLEAFT,BPTT,NOAGECT,PROPCT, &
                            JSHAPET,SHAPET,NEWTUTD,TU,TD,RELDF,DEXT)
                                   
                TUAR(ITAR, 1:NUMPNT) = TU
                TDAR(ITAR, 1:NUMPNT) = TD
                RELDFAR(ITAR, 1:NUMPNT) = RELDF

            END DO  ! End precalculate diffuse transmittance.
            
            ! Undocumented feature:
            ! Assign plant hydraulic conductance cf. Peltoniemi. This is assumed to scale with fractional diffuse
            ! radiation (because Kplant is proportional to APPFD in an optimal plant).
            !PLANTKCR = 0.0
            !DO ITAR = 1,NOTARGETS
            !  DO J = 1,NUMPNT
            !    PLANTKCR(ITAR,J) = PLANTK * RELDFAR(ITAR,J) ** KSCALING
            !  ENDDO
            !ENDDO
            ! Make sure that value given in input file (PLANTK) corresponds to maximum 
            ! -actual- plantk in the crown.
            !RESCALE = PLANTK / MAXVAL(PLANTKCR)
            !PLANTKCR = PLANTKCR * RESCALE
         
            
        ENDIF  ! IDAY=0 or IOTUTDth day.
        
    
    
    
    
    

    
    
END  SUBROUTINE InitDailyMaespaSingleTreeSingleLoop  



!**********************************************************************
SUBROUTINE ZEROSTART(HISTO, CANOPYDIMS)
    ! Set initial values of histogram to zero. 
    !**********************************************************************

    USE maestcom
    IMPLICIT NONE

    REAL HISTO(MAXT, MAXHISTO)
    REAL CANOPYDIMS(6)

    HISTO = 0.0
    CANOPYDIMS = 0.0

    RETURN
END SUBROUTINE ZEROSTART

!**********************************************************************
SUBROUTINE ZEROD(TDYAB, TOTCO2, TOTRESPF, TOTRESPWM, TOTRESPB, TOTRESPCR, TOTRESPFR, &
    TOTH2O, TOTHFX, WSOILMEAN, WSOILROOTMEAN, SWPMEAN, PPTTOT, ETMMTOT, ETMEASTOT, &
    DISCHARGETOT, SOILEVAPTOT, FSOILMEAN, TFALLTOT, QHTOT, QETOT, QNTOT, &
    QCTOT, RADINTERCTOT)
    ! This is subroutine to set the initial values of daily total variables
    ! to zero.
    !**********************************************************************
    USE maestcom
    IMPLICIT NONE

    REAL TDYAB(MAXT, 3)
    REAL TOTCO2(MAXT), TOTRESPF(MAXT), TOTRESPWM(MAXT)
    REAL TOTRESPB(MAXT), TOTRESPFR(MAXT), TOTRESPCR(MAXT)
    REAL TOTH2O(MAXT), TOTHFX(MAXT)
    REAL WSOILMEAN, WSOILROOTMEAN, PPTTOT, ETMMTOT, ETMEASTOT
    REAL DISCHARGETOT, SOILEVAPTOT, FSOILMEAN, TFALLTOT, QHTOT, QETOT, QNTOT
    REAL QCTOT, RADINTERCTOT, SWPMEAN

    TDYAB = 0.0
    TOTCO2 = 0.0
    TOTRESPF = 0.0
    TOTRESPWM = 0.0
    TOTRESPB = 0.0
    TOTRESPFR = 0.0
    TOTRESPCR = 0.0
    TOTH2O = 0.0
    TOTHFX = 0.0

    WSOILMEAN = 0.0
    WSOILROOTMEAN = 0.0
    SWPMEAN = 0.0
    PPTTOT = 0.0
    ETMMTOT = 0.0
    ETMEASTOT = 0.0
    DISCHARGETOT = 0.0
    SOILEVAPTOT = 0.0
    FSOILMEAN = 0.0
    TFALLTOT = 0.0
    QHTOT = 0.0
    QETOT = 0.0
    QNTOT = 0.0
    QCTOT = 0.0
    RADINTERCTOT = 0.0

    RETURN
END SUBROUTINE ZEROD

!**********************************************************************
SUBROUTINE ZEROHR(THRAB, FCO2, FRESPF, FRESPW, FRESPB, FRESPFR, FRESPCR, &
    FH2O, GSCAN, GBHCAN, FHEAT, PPAR, PPS, PTRANSP, TCAN, FSOIL1, &
    PSILCAN, PSILCANMIN, CICAN, NSUMMED, TOTTMP, ECANMAX, ACANMAX)
    ! This is subroutine to set the initial values of hourly total variables
    ! to zero.
    ! Note changes to dimensions of arrays (June 2008 RAD).
    !**********************************************************************
    USE maestcom
    IMPLICIT NONE

    REAL THRAB(MAXT, MAXHRS, 3), TCAN(MAXT, MAXHRS)
    REAL FCO2(MAXT, MAXHRS), FRESPF(MAXT, MAXHRS), FRESPW(MAXT, MAXHRS)
    REAL FRESPB(MAXT, MAXHRS), FRESPCR(MAXT, MAXHRS), FRESPFR(MAXT, MAXHRS)
    REAL GSCAN(MAXT, MAXHRS), FH2O(MAXT, MAXHRS), FHEAT(MAXT, MAXHRS)
    REAL PPAR(MAXT, MAXLAY, MAXHRS), PPS(MAXT, MAXLAY, MAXHRS)
    REAL PTRANSP(MAXT, MAXLAY, MAXHRS), FSOIL1, TOTTMP
    REAL PSILCAN(MAXT, MAXHRS), PSILCANMIN(MAXT, MAXHRS), CICAN(MAXT, MAXHRS)
    REAL GBHCAN(MAXT, MAXHRS)
    REAL ECANMAX(MAXT, MAXHRS), ACANMAX(MAXT, MAXHRS)
    INTEGER NSUMMED

    ! Note that we can set arrays to zero without a do-loop (RAD June 2008).
    FCO2 = 0.0
    FRESPF = 0.0
    FRESPW = 0.0
    FRESPB = 0.0
    FRESPFR = 0.0
    FRESPCR = 0.0
    FH2O = 0.0
    GSCAN = 0.0
    GBHCAN = 0.0
    FHEAT = 0.0
    TCAN = 0.0
    THRAB = 0.0
    PPAR = 0.0
    PPS = 0.0
    PTRANSP = 0.0
    FSOIL1 = 0.0
    TOTTMP = 0.0
    NSUMMED = 0
    PSILCAN = 0.0
    PSILCANMIN = 0.0
    ECANMAX = 0.0
    ACANMAX = 0.0
    CICAN = 0.0

    RETURN
END SUBROUTINE ZEROHR


!**********************************************************************     
SUBROUTINE ZEROFSOIL(FSOIL1, NSUMMED, TOTTMP)
    ! Set FSOIL1 to zero, and NSUMMED.
    !**********************************************************************     
    IMPLICIT NONE
    INTEGER NSUMMED
    REAL FSOIL1, TOTTMP

    FSOIL1 = 0.0
    NSUMMED = 0
    TOTTMP = 0.0

END SUBROUTINE ZEROFSOIL


!**********************************************************************
SUBROUTINE SUMHR(APAR, ANIR, ATHR, ALEAF, RD, GSC, GBH, ET, HFX, TLEAF, FSOIL, PSIL, CI, &
    AREA, IHOUR, ILAY, ITAR, NOTARGETS, NUMPNT, NSUMMED, TOTTMP, &
    PPAR, PPS, PTRANSP, THRAB, FCO2, FRESPF, GSCAN, GBHCAN, FH2O, FHEAT, TCAN, FSOIL1, &
    PSILCAN, PSILCANMIN, CICAN, ECANMAX, ACANMAX)
    ! Sum fluxes from each point to give hourly fluxes.
    ! Modified version of SUMHR to account for new looping order (June 2008 RAD).
    !**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER ITAR, ILAY, IHOUR, NOTARGETS, NUMPNT, NSUMMED

    REAL THRAB(MAXT, MAXHRS, 3)
    REAL FCO2(MAXT, MAXHRS), FRESPF(MAXT, MAXHRS), TCAN(MAXT, MAXHRS)
    REAL PSILCAN(MAXT, MAXHRS), PSILCANMIN(MAXT, MAXHRS), CICAN(MAXT, MAXHRS)
    REAL GSCAN(MAXT, MAXHRS), FH2O(MAXT, MAXHRS), FHEAT(MAXT, MAXHRS)
    REAL GBHCAN(MAXT, MAXHRS)
    REAL ACANMAX(MAXT, MAXHRS), ECANMAX(MAXT, MAXHRS)
    REAL PPAR(MAXT, MAXLAY, MAXHRS), PPS(MAXT, MAXLAY, MAXHRS)
    REAL PTRANSP(MAXT, MAXLAY, MAXHRS)
    REAL APAR, AREA, ALEAF, ET, ANIR, ATHR, RD, GSC, HFX, TLEAF, FSOIL1, FSOIL, TOTTMP
    REAL PSIL, CI, GBH

    ! Sum PAR, photosynthesis, & transpiration by layer
    PPAR(ITAR, ILAY, IHOUR) = PPAR(ITAR, ILAY, IHOUR) + APAR * UMOLPERJ * AREA
    PPS(ITAR, ILAY, IHOUR) = PPS(ITAR, ILAY, IHOUR) + ALEAF * AREA
    PTRANSP(ITAR, ILAY, IHOUR) = PTRANSP(ITAR, ILAY, IHOUR) + ET * AREA

    ! Sum all fluxes for the hour
    THRAB(ITAR, IHOUR, 1) = THRAB(ITAR, IHOUR, 1) + APAR * AREA
    THRAB(ITAR, IHOUR, 2) = THRAB(ITAR, IHOUR, 2) + ANIR * AREA
    THRAB(ITAR, IHOUR, 3) = THRAB(ITAR, IHOUR, 3) + ATHR * AREA
    FCO2(ITAR, IHOUR) = FCO2(ITAR, IHOUR) + ALEAF * AREA

    ! Foliage respiration in umol tree-1 s-1
    FRESPF(ITAR, IHOUR) = FRESPF(ITAR, IHOUR) + RD * AREA
    ! Transpiration in umol tree-1 s-1
    FH2O(ITAR, IHOUR) = FH2O(ITAR, IHOUR) + ET * AREA
    ! Maximum rates of transpiration and photosynthesis
    IF (ET .GT. ECANMAX(ITAR, IHOUR))THEN
        ECANMAX(ITAR, IHOUR) = ET
    ENDIF
    IF (ALEAF .GT. ACANMAX(ITAR, IHOUR))THEN
        ACANMAX(ITAR, IHOUR) = ALEAF
    ENDIF
    ! Stom cond in mol tree-1 s-1
    GSCAN(ITAR, IHOUR) = GSCAN(ITAR, IHOUR) + GSC * AREA
    ! Boundary layer conductance to heat
    GBHCAN(ITAR, IHOUR) = GBHCAN(ITAR, IHOUR) + GBH * AREA
    ! Heat flux in mol tree-1 s-1
    FHEAT(ITAR, IHOUR) = FHEAT(ITAR, IHOUR) + HFX * AREA
    ! Average leaf temperature - will be divided by total leaf area later. 
    TCAN(ITAR, IHOUR) = TCAN(ITAR, IHOUR) + TLEAF * AREA
    ! Average leaf water potential
    PSILCAN(ITAR, IHOUR) = PSILCAN(ITAR, IHOUR) + PSIL * AREA
    ! Lowest leaf water potential for the target tree.
    IF (PSIL .LT. PSILCANMIN(ITAR, IHOUR))THEN
        PSILCANMIN(ITAR, IHOUR) = PSIL
    ENDIF
    ! Average ci.
    CICAN(ITAR, IHOUR) = CICAN(ITAR, IHOUR) + CI * AREA

    ! Average FSOIL across all target trees and grid points.
    FSOIL1 = FSOIL1 + FSOIL * ET * AREA
    TOTTMP = TOTTMP + ET * AREA
    NSUMMED = NSUMMED + 1

    RETURN
END SUBROUTINE SUMHR


!**********************************************************************
SUBROUTINE SUMHRUS(IHOUR, NOUSPOINTS, GRDAREAI, AREAUS, PARUS, PARUSMEAN, PARUSSD, &
    APARUS, PSUS, ETUS, THRABUS, FCO2US, FH2OUS)

    ! Sum fluxes from each understorey point to give hourly fluxes.
    ! Taken from MAESUS, Feb. '09 (RAD).
    !**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER IHOUR, NOUSPOINTS
    REAL THRABUS(MAXHRS), FCO2US(MAXHRS), FH2OUS(MAXHRS)
    REAL PARUS(MAXHRS, MAXP), APARUS(MAXHRS, MAXP)
    REAL PSUS(MAXHRS, MAXP), ETUS(MAXHRS, MAXP)
    REAL AREAUS(MAXP), PARUSMEAN(MAXHRS), PARUSSD(MAXHRS)
    REAL GRDAREAI
    REAL, EXTERNAL :: STDEV

    THRABUS(IHOUR) = 0.0
    FCO2US(IHOUR) = 0.0
    FH2OUS(IHOUR) = 0.0

    ! Average all fluxes across understorey points.
    FCO2US(IHOUR) = SUM(PSUS(IHOUR, 1:NOUSPOINTS)) / REAL(NOUSPOINTS)
    FH2OUS(IHOUR) = SUM(ETUS(IHOUR, 1:NOUSPOINTS)) / REAL(NOUSPOINTS)
    THRABUS(IHOUR) = SUM(APARUS(IHOUR, 1:NOUSPOINTS)) / REAL(NOUSPOINTS)

    ! Average PAR above understorey
    PARUSMEAN(IHOUR) = SUM(PARUS(IHOUR, 1:NOUSPOINTS)) / REAL(NOUSPOINTS)
    PARUSSD(IHOUR) = STDEV(PARUS(IHOUR, 1:NOUSPOINTS), NOUSPOINTS)

    RETURN
END SUBROUTINE SUMHRUS


!**********************************************************************
SUBROUTINE SUMDAILY(NOTARGETS, THRAB, FCO2, FRESPF, FRESPW, FRESPB, FRESPCR, FRESPFR, &
    FH2O, FH2OCAN, FHEAT, TDYAB, TOTCO2, TOTRESPF, TOTRESPWM, TOTRESPB, &
    TOTRESPCR, TOTRESPFR, TOTH2O, TOTH2OCAN, TOTHFX)
    ! Sum hourly fluxes to give daily ones
    !**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER NOTARGETS, J, IHOUR, ITAR
    REAL THRAB(MAXT, MAXHRS, 3), FCO2(MAXT, MAXHRS), FRESPF(MAXT, MAXHRS)
    REAL FRESPW(MAXT, MAXHRS)
    REAL FRESPB(MAXT, MAXHRS), FRESPCR(MAXT, MAXHRS), FRESPFR(MAXT, MAXHRS)
    REAL FH2O(MAXT, MAXHRS), FH2OCAN(MAXT, MAXHRS), FHEAT(MAXT, MAXHRS)
    REAL TDYAB(MAXT, 3)
    REAL TOTCO2(MAXT), TOTRESPF(MAXT), TOTRESPWM(MAXT)
    REAL TOTRESPB(MAXT), TOTRESPFR(MAXT), TOTRESPCR(MAXT)
    REAL TOTH2O(MAXT), TOTH2OCAN(MAXT), TOTHFX(MAXT), CONVERT

    DO ITAR = 1, NOTARGETS
        DO IHOUR = 1, KHRS
            TOTCO2(ITAR) = TOTCO2(ITAR) + FCO2(ITAR, IHOUR)
            !print *, TOTCO2(ITAR), FCO2(ITAR,IHOUR)
            TOTRESPF(ITAR) = TOTRESPF(ITAR) + FRESPF(ITAR, IHOUR)
            TOTRESPWM(ITAR) = TOTRESPWM(ITAR) + FRESPW(ITAR, IHOUR)
            TOTRESPB(ITAR) = TOTRESPB(ITAR) + FRESPB(ITAR, IHOUR)
            TOTRESPCR(ITAR) = TOTRESPCR(ITAR) + FRESPCR(ITAR, IHOUR)
            TOTRESPFR(ITAR) = TOTRESPFR(ITAR) + FRESPFR(ITAR, IHOUR)
            TOTH2O(ITAR) = TOTH2O(ITAR) + FH2O(ITAR, IHOUR)
            TOTH2OCAN(ITAR) = TOTH2OCAN(ITAR) + FH2OCAN(ITAR, IHOUR)
            TOTHFX(ITAR) = TOTHFX(ITAR) + FHEAT(ITAR, IHOUR)
            DO J = 1, 3
                TDYAB(ITAR, J) = TDYAB(ITAR, J) + THRAB(ITAR, IHOUR, J)
            END DO
        END DO
    END DO

    CONVERT = SPERHR*1E-6

    TOTCO2 = TOTCO2 * CONVERT
    TOTRESPF = TOTRESPF * CONVERT
    TOTRESPB = TOTRESPB * CONVERT
    TOTRESPWM = TOTRESPWM * CONVERT
    TOTRESPCR = TOTRESPCR * CONVERT
    TOTRESPFR = TOTRESPFR * CONVERT
    TOTH2O = TOTH2O * CONVERT
    TOTH2OCAN = TOTH2OCAN * CONVERT
    TOTHFX = TOTHFX * CONVERT
    TDYAB = TDYAB * CONVERT

    RETURN
END SUBROUTINE SUMDAILY







SUBROUTINE MaespaSingleTreeSingleLoop

    USE switches
    USE metcom
    USE maestcom
    USE MaespaResult
    USE dyn_array, only : MaespaResultPointer
!    USE tufinit

    USE maindeclarations

    IMPLICIT NONE
    REAL, EXTERNAL :: AVERAGEVAL, CALCRMW, TK, ETCAN, RESP, GRESP
    
    !! define some variables which I will need to pass as I figure them out
    !REAL FSOIL1,TOTTMP !gets zeroed at beginning
    !INTEGER NSUMMED !gets zeroed at beginning
    INTEGER NNOTARGETS !! number of trees in subplot
    TYPE(maesparesulttype) :: result 
    

     
    
    !! end variables
    !NOTREES = 1
    
    !! end of variables passed from TUF


    !IHOUR = 1

    CALL ZEROFSOIL(FSOIL1, NSUMMED, TOTTMP)

    ! Loop over all chosen trees within subplot
    DO ITAR = 1, NOTARGETS
        ITREE = ITARGETS(ITAR)
  
        ! Read diffuse transmittance from precalculated array
        TU = TUAR(ITAR, 1:NUMPNT)
        TD = TDAR(ITAR, 1:NUMPNT)
        RELDF = RELDFAR(ITAR, 1:NUMPNT)

        ! Assign arrays.
        ISPEC = ISPECIES(ITREE)

        JLEAF = JLEAFSPEC(ISPEC)
        BPT(1:8, 1:MAXC) = BPTSPEC(1:8, 1:MAXC, ISPEC)
        RANDOM = RANDOMSPEC(ISPEC)
        NOAGEC = NOAGECSPEC(ISPEC)
        JSHAPE = JSHAPESPEC(ISPEC)
        SHAPE = SHAPESPEC(ISPEC)
        EXTWIND = EXTWINDSPEC(ISPEC)
        NALPHA = NALPHASPEC(ISPEC)
        ALPHA(1:MAXANG) = ALPHASPEC(1:MAXANG, ISPEC)
        FALPHA(1:MAXANG) = FALPHASPEC(1:MAXANG, ISPEC)
        COEFFT = COEFFTSPEC(ISPEC)
        EXPONT = EXPONTSPEC(ISPEC)
        WINTERC = WINTERCSPEC(ISPEC)
        BCOEFFT = BCOEFFTSPEC(ISPEC)
        BEXPONT = BEXPONTSPEC(ISPEC)
        BINTERC = BINTERCSPEC(ISPEC)
        RCOEFFT = RCOEFFTSPEC(ISPEC)
        REXPONT = REXPONTSPEC(ISPEC)
        RINTERC = RINTERCSPEC(ISPEC)
        FRFRAC = FRFRACSPEC(ISPEC)

        NOAGEP = NOAGEPSPEC(ISPEC)
        PROPC(1:MAXC) = PROPCSPEC(1:MAXC, ISPEC)
        PROPP(1:MAXC) = PROPPSPEC(1:MAXC, ISPEC)
        ABSRP(1:MAXLAY, 1:3) = ABSRPSPEC(1:MAXLAY, 1:3, ISPEC)
        ARHO(1:MAXLAY, 1:3) = ARHOSPEC(1:MAXLAY, 1:3, ISPEC)
        ATAU(1:MAXLAY, 1:3) = ATAUSPEC(1:MAXLAY, 1:3, ISPEC)
        RHOSOL(1:3) = RHOSOLSPEC(1:3, ISPEC)
        JMAXTABLE(1:maxdate, 1:MAXLAY, 1:MAXC) = JMAXTABLESPEC(1:maxdate, 1:MAXLAY, 1:MAXC, ISPEC)
        DATESJ(1:maxdate) = DATESJSPEC(1:maxdate, ISPEC)
        NOJDATES = NOJDATESSPEC(ISPEC)
        IECO = IECOSPEC(ISPEC)
        EAVJ = EAVJSPEC(ISPEC)
        EDVJ = EDVJSPEC(ISPEC)
        DELSJ = DELSJSPEC(ISPEC)
        THETA = THETASPEC(ISPEC)
        VCMAXTABLE(1:maxdate, 1:MAXLAY, 1:MAXC) = VCMAXTABLESPEC(1:maxdate, 1:MAXLAY, 1:MAXC, ISPEC)
        DATESV(1:maxdate) = DATESVSPEC(1:maxdate, ISPEC)
        NOVDATES = NOVDATESSPEC(ISPEC)
        EAVC = EAVCSPEC(ISPEC)
        EDVC = EDVCSPEC(ISPEC)
        DELSC = DELSCSPEC(ISPEC)
        TVJUP = TVJUPSPEC(ISPEC)
        TVJDN = TVJDNSPEC(ISPEC)
        SLATABLE(1:maxdate, 1:MAXLAY, 1:MAXC) = SLATABLESPEC(1:maxdate, 1:MAXLAY, 1:MAXC, ISPEC)
        DATESSLA(1:maxdate) = DATESSLASPEC(1:maxdate, ISPEC)
        NOSLADATES = NOSLADATESSPEC(ISPEC)
        NOADATES = NOADATESSPEC(ISPEC)
        DATESA(1:maxdate) = DATESASPEC(1:maxdate, ISPEC)
        AJQTABLE(1:maxdate, 1:MAXLAY, 1:MAXC) = AJQTABLESPEC(1:maxdate, 1:MAXLAY, 1:MAXC, ISPEC)
        RDTABLE(1:maxdate, 1:MAXLAY, 1:MAXC) = RDTABLESPEC(1:maxdate, 1:MAXLAY, 1:MAXC, ISPEC)
        DATESRD(1:maxdate) = DATESRDSPEC(1:maxdate, ISPEC)
        NORDATES = NORDATESSPEC(ISPEC)
        RTEMP = RTEMPSPEC(ISPEC)
        DAYRESP = DAYRESPSPEC(ISPEC)
        TBELOW = TBELOWSPEC(ISPEC)
        EFFYRW = EFFYRWSPEC(ISPEC)
        RMW = RMWSPEC(ISPEC)
        RTEMPW = RTEMPWSPEC(ISPEC)
        COLLA = COLLASPEC(ISPEC)
        COLLK = COLLKSPEC(ISPEC)
        STEMSDW = STEMSDWSPEC(ISPEC)
        RMWAREA = RMWAREASPEC(ISPEC)
        STEMFORM = STEMFORMSPEC(ISPEC)
        NOFQDATES = NOFQDATESSPEC(ISPEC)
        DATESFQ(1:maxdate) = DATESFQSPEC(1:maxdate, ISPEC)
        Q10FTABLE(1:maxdate) = Q10FTABLESPEC(1:maxdate, ISPEC)
        K10F = K10FSPEC(ISPEC)
        NOWQDATES = NOWQDATESSPEC(ISPEC)
        DATESWQ = DATESWQSPEC(1:maxdate, ISPEC)
        Q10WTABLE(1:maxdate) = Q10WTABLESPEC(1:maxdate, ISPEC)
        RMFR = RMFRSPEC(ISPEC)
        RMCR = RMCRSPEC(ISPEC)
        Q10R = Q10RSPEC(ISPEC)
        RTEMPR = RTEMPRSPEC(ISPEC)
        EFFYRF = EFFYRFSPEC(ISPEC)
        RMB = RMBSPEC(ISPEC)
        Q10B = Q10BSPEC(ISPEC)
        RTEMPB = RTEMPBSPEC(ISPEC)
        GSREF = GSREFSPEC(ISPEC)
        GSMIN = GSMINSPEC(ISPEC)
        PAR0 = PAR0SPEC(ISPEC)
        D0 = D0SPEC(ISPEC)
        VK1 = VK1SPEC(ISPEC)
        VK2 = VK2SPEC(ISPEC)
        VPD1 = VPD1SPEC(ISPEC)
        VPD2 = VPD2SPEC(ISPEC)
        VMFD0 = VMFD0SPEC(ISPEC)
        GSJA = GSJASPEC(ISPEC)
        GSJB = GSJBSPEC(ISPEC)
        T0 = T0SPEC(ISPEC)
        TREF = TREFSPEC(ISPEC)
        TMAX = TMAXSPEC(ISPEC)
        SMD1 = SMD1SPEC(ISPEC)
        SMD2 = SMD2SPEC(ISPEC)
        WC1 = WC1SPEC(ISPEC)
        WC2 = WC2SPEC(ISPEC)
        SWPEXP = SWPEXPSPEC(ISPEC)
        G0TABLE = G0TABLESPEC(1:maxdate, ISPEC)
        G1TABLE = G1TABLESPEC(1:maxdate, ISPEC)
        GK = GKSPEC(ISPEC)
        DATESGS = DATESGSSPEC(1:MASPDATE, ISPEC)
        NOGSDATES = NOGSDATESSPEC(ISPEC)
        D0L = D0LSPEC(ISPEC)
        GAMMA = GAMMASPEC(ISPEC)
        SF = SFSPEC(ISPEC)
        PSIV = PSIVSPEC(ISPEC)
        WLEAF = WLEAFSPEC(ISPEC)
        NSIDES = NSIDESSPEC(ISPEC)

        ! Sort the trees every timestep.
        ! This should be done outside the hourly loop, and stored in an array.
        CALL SORTTREES(NOALLTREES, NOTREES, ITREE, DXT1, DYT1, DZT1, RXTABLE1, RYTABLE1, RZTABLE1, ZBCTABLE1, &
        FOLTABLE1, DIAMTABLE1, DXT, DYT, DZT, RXTABLE, RYTABLE, RZTABLE, FOLTABLE, ZBCTABLE, &
        DIAMTABLE, ISPECIES, ISPECIEST, IT)

        DO I = 1, NOTREES
            JSHAPET(I) = JSHAPESPEC(ISPECIEST(I))
            SHAPET(I) = SHAPESPEC(ISPECIEST(I))
            DEXTT(I, 1:MAXANG) = DEXTSPEC(ISPECIEST(I), 1:MAXANG)
            JLEAFT(I) = JLEAFSPEC(ISPECIEST(I))
            NOAGECT(I) = NOAGECSPEC(ISPECIEST(I))
            BPTT(1:8, 1:MAXC, I) = BPTSPEC(1:8, 1:MAXC, ISPECIEST(I))
            PROPPT(1:MAXC, I) = PROPPSPEC(1:MAXC, ISPECIEST(I))
            PROPCT(1:MAXC, I) = PROPCSPEC(1:MAXC, ISPECIEST(I))
        END DO

        ! Interpolate to get daily values of parameters
        ! This we can probably also do outside the hourly loop.
        CALL INTERPOLATEP(IDAY, ISTART, NOJDATES, DATESJ, JMAXTABLE, NOVDATES, DATESV, VCMAXTABLE, NORDATES, &
        DATESRD, RDTABLE, NOSLADATES, DATESSLA, SLATABLE, NOADATES, DATESA, AJQTABLE, &
        NOFQDATES, DATESFQ, Q10FTABLE, NOWQDATES, DATESWQ, Q10WTABLE, NOLAY, NOAGEP, &
        JMAX25, VCMAX25, RD0, SLA, AJQ, Q10F, Q10W, NOGSDATES, DATESGS, G0TABLE, G1TABLE, G0, G1)

        CALL INTERPOLATET(IDAY, ISTART, IHOUR, NOXDATES, DATESX, RXTABLE, NOYDATES, DATESY, RYTABLE, NOZDATES, &
        DATESZ, RZTABLE, NOTDATES, DATEST, ZBCTABLE, NODDATES, DATESD, DIAMTABLE, &
        NOLADATES, DATESLA, FOLTABLE, TOTLAITABLE, NOTREES, RX, RY, RZ, ZBC, FOLT, &
        TOTLAI, DIAM, STOCKING, IFLUSH, DT1, DT2, DT3, DT4, EXPTIME, APP, EXPAN, NEWCANOPY, &
        CANOPYDIMS)

        CALL POINTSNEW(NOLAY, PPLAY, JLEAF, JSHAPE, SHAPE, RX(1), RY(1), RZ(1), ZBC(1), DXT(1), DYT(1), &
        DZT(1), FOLT(1), PROPC, PROPP, BPT, NOAGECT(1), NOAGEP, XL, YL, ZL, VL, DLT, DLI, &
        LGP, FOLLAY)

        ! Following function s need FOLLAY. 
        CALL GETWIND(FOLLAY, FOLT(1), TOTLAI, EXTWIND, WINDLAY)

        ! Calculate woody biomass and woody biomass increment
        CALL CALCWBIOM(IDAY, RZ(1) + ZBC(1), DIAM(1), COEFFT, EXPONT, WINTERC, WBIOM, WBINC)
        CALL CALCWBIOM(IDAY, RZ(1) + ZBC(1), DIAM(1), BCOEFFT, BEXPONT, BINTERC, BBIOM, BBINC)
        CALL CALCWBIOM(IDAY, RZ(1) + ZBC(1), DIAM(1), RCOEFFT, REXPONT, RINTERC, RBIOM, RBINC)

        ! Calculate foliar biomass and increment
        CALL CALCFBIOM(IDAY, NOSLADATES, FOLLAY, SLA, PROPP, NOLAY, NOAGEP, FBIOM, FBINC)

        ! Calculate stem respiration rate per unit biomas
        RMW = CALCRMW(MODELRW, COLLA, COLLK, STEMSDW, DIAM(1), RZ(1) + ZBC(1), STEMFORM, RMWAREA, WBIOM, RMW)

        ! Output information to layer flux file if required
        IF (IOHRLY .GT. 1) CALL OUTPUTLAY(ULAY, FOLLAY, JMAX25, VCMAX25, NOLAY)
        
!        print *,MLAYER(1)

        ! If the diffuse transmittances have changed, must set up the EHC
        IF (NEWTUTD .EQ. 1.AND.TOTLAI .GT. 0) THEN
            CALL EHC(NUMPNT, TU, TD, TOTLAI, XSLOPE, YSLOPE, NAZ, NZEN, DIFZEN, DEXT, DLAI, EXPDIF, LAYER, MLAYER)
        END IF
        
!        print *,MLAYER(1)

        ! Assign soil water measurement to SOILMOIST, depending on settings.
        CALL ASSIGNSOILWATER(WSOILMETHOD, USEMEASSW, SWMIN, SWMAX, SOILMOIST(IHOUR), WSOILROOT, &
        SOILDEPTH, SOILDATA, SOILMOISTURE)
               
        ! Soil water potential, conductivity & conductance, fractional uptake (but no uptake yet).
        CALL CALCSOILPARS(NLAYER, NROOTLAYER, SOILWP, FRACWATER, FRACORGANIC, POREFRAC, SOILCOND, THERMCOND, &
        ROOTMASS, ROOTLEN, LAYTHICK, ICEPROP, EQUALUPTAKE, RETFUNCTION, USEMEASSW, &
        SOILDATA, SOILMOISTURE, PSIE, BPAR, KSAT, ROOTRESIST, ROOTRESFRAC, &
        ROOTRAD, MINROOTWP, TOTLAI, WINDAH(IHOUR), ZHT, Z0HT, GAMSOIL, &
        WEIGHTEDSWP, FRACUPTAKE, TOTSOILRES)
        
        ! Soil surface T for SCATTER routine:
        IF (SIMTSOIL .EQ. 0)THEN ! No Tsoil simulated.
            PREVTSOIL = TK(TSOIL(IHOUR))
        ELSE
            PREVTSOIL = SOILTEMP(1)
        END IF
        
!        print *,ihour
!        print *,'RADADV',RADABV(IHOUR, 1)
!        print *,'ZEN',ABS(ZEN(IHOUR))
!        print *,'PI',PI/2.0
!        print *,'FOLT',FOLT(1)
!        print *,RADABV(:, 1)
        ! Test to see if daylight hours or if any foliage
        IF ((ABS(ZEN(IHOUR)) < PI/2.0) .AND. (RADABV(IHOUR, 1) > 1.0) .AND. (FOLT(1) > 0.0)) THEN

            ! Get slope correction factor
            CALL SLOPES(IHOUR, TTIMD, EQNTIM, ALAT, DEC, XSLOPE, YSLOPE, BEAR, ZEN(IHOUR), BMULT, DMULT2, SOMULT)

            ! Get extinction coefficients
            DO I = 1, NSPECIES
                CALL EXDIFF(NALPHASPEC(I), ALPHASPEC(1:MAXANG, I), FALPHASPEC(1:MAXANG, I), NZEN, DIFZEN, &
                RANDOMSPEC(I), DEXTSPEC(I, 1:MAXANG))

                CALL EXBEAM(NALPHASPEC(I), ALPHASPEC(1:MAXANG, I), FALPHASPEC(1:MAXANG, I), RANDOMSPEC(I), &
                ZEN(IHOUR), BEXTSPEC(I), BEXTANGSPEC(I, 1:MAXANG))
            END DO

            DO I = 1, NOTREES
                BEXTT(I) = BEXTSPEC(ISPECIEST(I))
                BEXTANGT(I, 1:MAXANG) = BEXTANGSPEC(ISPECIEST(I), 1:MAXANG)
            END DO
            
            ! Understorey calculations.
            IF (ISIMUS .EQ. 1)THEN

                ! Find extinction coefficients for beam component,
                ! sorted to understorey midpoint (ISPECIESTUS).
                DO I = 1, NOTREES
                    BEXTTUS(I) = BEXTSPEC(ISPECIESTUS(I))
                    BEXTANGTUS(I, 1:MAXANG) = BEXTANGSPEC(ISPECIESTUS(I), 1:MAXANG)
                END DO

                DO IPTUS = 1, NOUSPOINTS

                    ! Beam radiation on the understorey points.
                    CALL TRANSB(IHOUR, IPROGUS, ZEN(IHOUR), AZ(IHOUR), XSLOPE, YSLOPE, FBEAM, BEXTTUS, XLU(IPTUS), &
                    YLU(IPTUS), ZLU(IPTUS), RXUS, RYUS, RZUS, DXTUS, DYTUS, DZTUS, XMAX, YMAX, SHADEHT, &
                    FOLTUS, ZBCUS, JLEAFTUS, BPTTUS, NOAGECTUS, PROPCTUS, JSHAPETUS, SHAPETUS, NOTREES, &
                    SUNLA, BEXTUS) ! BEXTUS not used...
                    
                    ! Output transmittances (Note IWAVE=1 only).
                    PAR = RADABV(IHOUR, 1)

                    TDIFF = (1 - FBEAM(IHOUR, 1)) * PAR * TDUS(IPTUS)
                    TSCAT = 0 !DIFDN(IPTUS,1)*UMOLPERJ

                    ! PAR at all understorey points, diffuse, direct, and total.
                    UIBEAM(IHOUR, IPTUS) = FBEAM(IHOUR, 1) * PAR * SUNLA
                    UIDIFF(IHOUR, IPTUS) = TDIFF + TSCAT
                    PARUS(IHOUR, IPTUS) = UIDIFF(IHOUR, IPTUS) + UIBEAM(IHOUR, IPTUS)

                    ! Global radiation to understorey points
                    !NIRUS(IHOUR,IPTUS) = FBEAM(IHOUR,1)*RADABV(IHOUR,1)*SUNLA


                    ! Get parameters of BEWDY model
                    CALL BEWDYPARMS(IHOUR, TAIR(IHOUR), RH(IHOUR), CA(IHOUR), JMAXN25, IECOU, EAVJU, EDVJU, DELSJU, &
                    TVJUPU, TVJDNU, VCMAXN25, EAVCU, EDVCU, DELSCU, AJQU, GSBG0U, GSBG1U, CICARAT, &
                    BALPHA, BLAMBDA)

                    IF (MOSS .EQ. 1) THEN
                        CALL PSMOSS(UMOLPERJ * PARUS(IHOUR, IPTUS), TAIR(IHOUR), RH(IHOUR), CA(IHOUR), JMAX25M, IECOU, &
                        EAVJU, EDVJU, DELSJU, TVJUPU, TVJDNU, VCMAX25M, EAVCU, EDVCU, DELSCU, AJQU, THETAM, &
                        PSUS(IHOUR, IPTUS))
                        GSIPT = 0.0
                        ETUS = 0.0
                    ELSE
                        ! Otherwise call BEWDY model to calculate understorey photosynthesis
                        !! Note UIBEAM is divided by SUNLA, gets back-converted in BEWDY subr.

                        IF (SUNLA .GT. 0.0) THEN
                        BEAMP = UMOLPERJ * UIBEAM(IHOUR, IPTUS)/SUNLA
                    ELSE
                        BEAMP = 0.0
                        ENDIF

                        CALL BEWDY(IHOUR, BEAMP, UMOLPERJ * UIDIFF(IHOUR, IPTUS), SUNLA, BALPHA, BLAMBDA, FN0US(IPTUS), &
                        UNMIN, EXTKUS, ABSRPU, USLAI(IPTUS), APARUS(IHOUR, IPTUS), PSUS(IHOUR, IPTUS), &
                        PARUNDER(IHOUR, IPTUS))

                        ! Net photosynthesis
                        PSUS(IHOUR, IPTUS) = PSUS(IHOUR, IPTUS) - RD0US

                        ! Bewdy outputs in mu mol, convert to W m-2
                        APARUS(IHOUR, IPTUS) = APARUS(IHOUR, IPTUS) / UMOLPERJ
                        PARUNDER(IHOUR, IPTUS) = PARUNDER(IHOUR, IPTUS) / UMOLPERJ

                        ! Stomatal conductance and transpiration - estimated at whole-clump level, mol CO2 m-2 s-1
                        GSIPT = GSBG0U + GSBG1U * PSUS(IHOUR, IPTUS) * RH(IHOUR)/CA(IHOUR)

                        ! mmol H2O m-2 s-1
                        ETUS(IHOUR, IPTUS) = GSIPT * 1.6 * VPD(IHOUR)/PRESS(IHOUR) * 1E3
                    END IF
                END DO ! Loop over understorey points.

                ! Sum understorey arrays over all points:
                CALL SUMHRUS(IHOUR, NOUSPOINTS, GRDAREAI, AREAUS, PARUS, PARUSMEAN, PARUSSD, APARUS, PSUS, ETUS, THRABUS, &
                FCO2US, FH2OUS)
            ENDIF ! Understorey calculations
                        
            ! Loop over grid points
            DO IPT = 1, NUMPNT
                
!                print *,'ipt=', XL(IPT), YL(IPT), ZL(IPT)
                ! Calculate the weighted pathlengths for beam radiation.
                CALL TRANSB(IHOUR, IPROG, ZEN(IHOUR), AZ(IHOUR), XSLOPE, YSLOPE, FBEAM, BEXTT, XL(IPT), YL(IPT), ZL(IPT), &
                RX, RY, RZ, DXT, DYT, DZT, XMAX, YMAX, SHADEHT, FOLT, ZBC, JLEAFT, BPTT, NOAGECT, PROPCT, JSHAPET, &
                SHAPET, NOTREES, SUNLA, BEXT)
                
!                print *,'ipt=', XL(IPT), YL(IPT), ZL(IPT)
!                print *,'ipt=',RX, RY, RZ, DXT, DYT, DZT, XMAX, YMAX, SHADEHT, FOLT, ZBC, JLEAFT, BPTT, NOAGECT, PROPCT, JSHAPET, &
!                SHAPET, NOTREES, SUNLA, BEXT
                

                ! Assign plant hydraulic conductance
                !PLANTK = PLANTKCR(ITAR,IPT)

                ! Loop over the 3 wavelengths
                DO IWAVE = 1, 3
                    !print *,MLAYER
                    !print *,'mlayer(ipt)',IPT, MLAYER(IPT)
                    ! Calculate the scattered radiation
                    CALL SCATTER(IPT, IWAVE, MLAYER(IPT), LAYER(IPT), DLAI, EXPDIF, ZEN(IHOUR), BEXT, DMULT2, SOMULT, BMULT, &
                    RADABV(IHOUR, IWAVE), FBEAM(IHOUR, IWAVE), TAIR(IHOUR), PREVTSOIL, ARHO(LGP(IPT), IWAVE), &
                    ATAU(LGP(IPT), IWAVE), RHOSOL(IWAVE), DIFUP, DIFDN, SCLOST, DOWNTH)

                    ! Lost scattered radiation for each tree (W m-2), averaged over the grid points.
                    ! RAD June 2008.              
                    SCLOSTTREE(ITAR, 1) = SUM(SCLOST(1:NUMPNT, 1)) / NUMPNT
                    SCLOSTTREE(ITAR, 2) = SUM(SCLOST(1:NUMPNT, 2)) / NUMPNT

                    ! Assume zero reflectance in TR waveband (Norman 1979)
                    ! But store in the same array the lost tranmission at top of canopy.
                    SCLOSTTREE(ITAR, 3) = SUM(SCLOST(1:NUMPNT, 2)) / NUMPNT

                    ! Downwelling longwave radiation (calculated for each gridpoint
                    ! with the EHC) averaged across the grid points.
                    IF (IWAVE .EQ. 3) DOWNTHTREE(ITAR) = SUM(DOWNTH) / NUMPNT
                    
                    ! Calculate absorbed radiation
                    CALL ABSRAD(IPT, IWAVE, NZEN, DEXT, BEXT, BMULT, RELDF(IPT), RADABV(IHOUR, IWAVE), &
                    FBEAM(IHOUR, IWAVE), ZEN(IHOUR), ABSRP(LGP(IPT), IWAVE), DIFDN(IPT, IWAVE), &
                    DIFUP(IPT, IWAVE), DFLUX, BFLUX, SCATFX)
                    
!                    print *,'dflux after',dflux
                END DO

                ! Calculation of photosynthesis may be done for sunlit & shaded leaves
                ! separately, or by averaging PAR over the total leaf area
                IF ((MODELSS .EQ. 0).AND.(FBEAM(IHOUR, 1) .NE. 0.0)) THEN

                    ! Do calculations separately for sunlit & shaded leaves
                    DO ISUNLIT = 1, 2 ! Loop over sunlit & shaded leaves

                        IF (ISUNLIT .EQ. 1) THEN
                            APAR = (BFLUX(IPT, 1) * BEXT + DFLUX(IPT, 1)) * UMOLPERJ
                            ANIR = BFLUX(IPT, 2) * BEXT + DFLUX(IPT, 2)
                            FAREA = SUNLA
                        ELSE
                            APAR = DFLUX(IPT, 1) * UMOLPERJ
                            ANIR = DFLUX(IPT, 2)
                            FAREA = 1.0 - SUNLA
                        END IF

                        ATHR = DFLUX(IPT, 3)
                        RNET = APAR/UMOLPERJ + ANIR + ATHR

                        DO IAGE = 1, NOAGEP ! Loop over age classes
                            AREA = FAREA * DLI(IAGE, IPT) * VL(IPT) ! m2
                            IF (IOHIST .EQ. 1) CALL CATEGO(AREA, APAR, HISTO, BINSIZE, ITAR)

                            ! Call physiology routine
                            CALL PSTRANSPIF(iday, ihour, RELDF(IPT), TU(IPT), TD(IPT), RNET, WINDAH(IHOUR) * WINDLAY(LGP(IPT)),APAR&
                            , TAIR(IHOUR), TMOVE, CA(IHOUR), RH(IHOUR), VPD(IHOUR), VMFD(IHOUR), PRESS(IHOUR), &
                            JMAX25(LGP(IPT), IAGE), IECO, EAVJ, EDVJ, DELSJ, VCMAX25(LGP(IPT), IAGE), EAVC, &
                            EDVC, DELSC, TVJUP, TVJDN, THETA, AJQ(LGP(IPT), IAGE), RD0(LGP(IPT), IAGE), Q10F, &
                            K10F, RTEMP, DAYRESP, TBELOW, MODELGS, WSOILMETHOD, EMAXLEAF, SOILMOISTURE, &
                            SMD1, SMD2, WC1, WC2, SOILDATA, SWPEXP, FSOIL, G0, D0L, GAMMA, VPDMIN, G1, GK, WLEAF, NSIDES&
                            , VPARA, VPARB, VPARC, VFUN, SF, PSIV, ITERMAX, GSC, ALEAF, RD, ET, HFX, TLEAF, GBH, PLANTK&
                            , TOTSOILRES, MINLEAFWP, WEIGHTEDSWP, KTOT, HMSHAPE, PSIL, ETEST, CI)

                            892 FORMAT (1(I10), 6(F10.5, 1X))
                            ! Sum (or average) outputs for the hour
                            CALL SUMHR(APAR/UMOLPERJ, ANIR, ATHR, ALEAF, RD, GSC, GBH, ET, HFX, TLEAF, FSOIL, PSIL, CI, AREA, &
                            IHOUR, LGP(IPT), ITAR, NOTARGETS, NUMPNT, NSUMMED, TOTTMP, PPAR, PPS, PTRANSP, THRAB, FCO2, FRESPF, &
                            GSCAN, GBHCAN, FH2O, FHEAT, TCAN, FSOIL1, PSILCAN, PSILCANMIN, CICAN, ECANMAX, ACANMAX)
                        END DO
                    END DO ! End loop over sunlit / shaded leaves
                ELSE IF ((MODELSS .EQ. 1).OR.(FBEAM(IHOUR, 1) .EQ. 0.0)) THEN

                    ! Do calculations for PAR averaged over all leaf area
                    APAR = (BFLUX(IPT, 1) * BEXT * SUNLA + DFLUX(IPT, 1)) * UMOLPERJ
                    ANIR = BFLUX(IPT, 2) * BEXT * SUNLA + DFLUX(IPT, 2)
                    ATHR = DFLUX(IPT, 3)
                    RNET = APAR/UMOLPERJ + ANIR + ATHR
                    DO IAGE = 1, NOAGEP ! Loop over age classes
                        AREA = DLI(IAGE, IPT) * VL(IPT)
                        IF (IOHIST .EQ. 1) CALL CATEGO(AREA, APAR, HISTO, BINSIZE, ITAR)

                        ! Call physiology routine
                        CALL PSTRANSPIF(iday, ihour, RELDF(IPT), TU(IPT), TD(IPT), RNET, WINDAH(IHOUR) * WINDLAY(LGP(IPT)), &
                        APAR, TAIR(IHOUR), TMOVE, CA(IHOUR), RH(IHOUR), VPD(IHOUR), &
                        VMFD(IHOUR), PRESS(IHOUR), JMAX25(LGP(IPT), IAGE), &
                        IECO, EAVJ, EDVJ, DELSJ, VCMAX25(LGP(IPT), IAGE), EAVC, EDVC, DELSC, &
                        TVJUP, TVJDN, THETA, AJQ(LGP(IPT), IAGE), RD0(LGP(IPT), IAGE), Q10F, &
                        K10F, RTEMP, DAYRESP, TBELOW, MODELGS, &
                        WSOILMETHOD, EMAXLEAF, &
                        SOILMOISTURE, SMD1, SMD2, WC1, WC2, SOILDATA, SWPEXP, FSOIL, G0, D0L, &
                        GAMMA, VPDMIN, G1, GK, WLEAF, NSIDES, VPARA, VPARB, VPARC, VFUN, SF, PSIV, &
                        ITERMAX, GSC, ALEAF, RD, ET, HFX, TLEAF, &
                        GBH, PLANTK, TOTSOILRES, MINLEAFWP, WEIGHTEDSWP, KTOT, HMSHAPE, PSIL, ETEST, CI)

                        ! Sum outputs for the hour
                        CALL SUMHR(APAR/UMOLPERJ, ANIR, ATHR, ALEAF, RD, GSC, GBH, ET, HFX, TLEAF, FSOIL, PSIL, CI, AREA, IHOUR, &
                            LGP(IPT),ITAR, NOTARGETS, NUMPNT, NSUMMED, TOTTMP, PPAR, PPS, PTRANSP, THRAB, FCO2, FRESPF, GSCAN, &
                            GBHCAN, FH2O, FHEAT, TCAN, FSOIL1, PSILCAN, PSILCANMIN, CICAN, ECANMAX, ACANMAX)

                    END DO ! End loop over age classes
                ELSE IF ((MODELSS .EQ. 2).AND.(FBEAM(IHOUR, 1) .NE. 0.0)) THEN

                    ! Do calculations separately for sunlit & shaded. Further separate sunlit
                    ! into leaf angle classes.
                    DO ISUNLIT = 1, NALPHA + 1
                        IF (ISUNLIT .GT. NALPHA) THEN
                            APAR = DFLUX(IPT, 1) * UMOLPERJ
                            ANIR = DFLUX(IPT, 2)
                            FAREA = 1.0 - SUNLA
                        ELSE
                            APAR = (BFLUX(IPT, 1) * BEXTANG(ISUNLIT) + DFLUX(IPT, 1)) * UMOLPERJ
                            ANIR = BFLUX(IPT, 2) * BEXTANG(ISUNLIT) + DFLUX(IPT, 2)
                            FAREA = SUNLA * FALPHA(ISUNLIT)
                        END IF

                        ATHR = DFLUX(IPT, 3)
                        RNET = APAR/UMOLPERJ + ANIR + ATHR

                        DO IAGE = 1, NOAGEP ! Loop over age classes
                            AREA = FAREA * DLI(IAGE, IPT) * VL(IPT) ! m2
                            IF (IOHIST .EQ. 1) CALL CATEGO(AREA, APAR, HISTO, BINSIZE, ITAR)

                            ! Call physiology routine
                            CALL PSTRANSPIF(iday, ihour, RELDF(IPT), TU(IPT), TD(IPT), RNET, WINDAH(IHOUR) * WINDLAY(LGP(IPT)), &
                            APAR, TAIR(IHOUR), TMOVE, CA(IHOUR), RH(IHOUR), VPD(IHOUR), &
                            VMFD(IHOUR), PRESS(IHOUR), JMAX25(LGP(IPT), IAGE), IECO, &
                            EAVJ, EDVJ, DELSJ, VCMAX25(LGP(IPT), IAGE), &
                            EAVC, EDVC, DELSC, TVJUP, TVJDN, THETA, AJQ(LGP(IPT), IAGE), &
                            RD0(LGP(IPT), IAGE), Q10F, K10F, RTEMP, DAYRESP, TBELOW, MODELGS, &
                            WSOILMETHOD, EMAXLEAF, SOILMOISTURE, SMD1, SMD2, WC1, WC2, &
                            SOILDATA, SWPEXP, FSOIL, G0, D0L, GAMMA, VPDMIN, G1, GK, WLEAF, NSIDES, VPARA, VPARB, &
                            VPARC, VFUN, SF, PSIV, ITERMAX, GSC, ALEAF, RD, ET, HFX, TLEAF, GBH, PLANTK, TOTSOILRES, &
                            MINLEAFWP, WEIGHTEDSWP, KTOT, HMSHAPE, PSIL, ETEST, CI)


                            ! Sum outputs for the hour
                            CALL SUMHR(APAR/UMOLPERJ, ANIR, ATHR, ALEAF, RD, GSC, GBH, ET, HFX, TLEAF, FSOIL, PSIL, CI, AREA,IHOUR,&
                            LGP(IPT), ITAR, NOTARGETS, NUMPNT, NSUMMED, TOTTMP, PPAR, PPS, PTRANSP, &
                            THRAB, FCO2, FRESPF, GSCAN, GBHCAN, FH2O, FHEAT, TCAN, FSOIL1, PSILCAN, PSILCANMIN, CICAN, &
                            ECANMAX, ACANMAX)
                        END DO
                    END DO ! End loop over sunlit / shaded leaves
                END IF ! Separating sunlit & shaded foliage, or not



                ! Output grid-level data.
                !             WRITE(UWATTEST, 891)IHOUR,IPT,ALEAF,ET/1000,ETEST/1000,  &
                !                 KTOT, WEIGHTEDSWP, PSIL, EMAXLEAF
                !     891     FORMAT (I7, I7, 1X, 9(F8.5,1X))

            END DO ! End loop over grid points

            ! Calculate transpiration by applying Penman-Monteith to canopy
            FH2OCAN(ITAR, IHOUR) = ETCAN(WINDAH(IHOUR), ZHT, Z0HT, ZPD, PRESS(IHOUR), TAIR(IHOUR), &
            THRAB(ITAR, IHOUR, 1) + THRAB(ITAR, IHOUR, 2) + THRAB(ITAR, IHOUR, 3), &
            VPD(IHOUR), GSCAN(ITAR, IHOUR), STOCKING)

            ! Normalise to get average foliage temperature
            TCAN(ITAR, IHOUR) = TCAN(ITAR, IHOUR)/FOLT(1)
            ! Same for leaf water potential
            PSILCAN(ITAR, IHOUR) = PSILCAN(ITAR, IHOUR)/FOLT(1)
            ! And ci
            CICAN(ITAR, IHOUR) = CICAN(ITAR, IHOUR)/FOLT(1)

        ELSE ! Night-time

            ! Canopy T at nighttime is same as air temperature. 
            TCAN(ITAR, IHOUR) = TAIR(IHOUR)
            ! Leaf water potential same as soil water potential
            PSILCAN(ITAR, IHOUR) = WEIGHTEDSWP
            PSILCANMIN(ITAR, IHOUR) = WEIGHTEDSWP
            ! And ci the same as ca
            CICAN(ITAR, IHOUR) = CA(IHOUR)

            ! Loop over grid points 
            DO IPT = 1, NUMPNT
!                print *,'mlayer(ipt)-night',IPT, MLAYER(IPT)
                ! Calculate the scattered radiation, for thermal only.
                CALL SCATTER(IPT, 3, MLAYER(IPT), LAYER(IPT), DLAI, EXPDIF, ZEN(IHOUR), BEXT, DMULT2, SOMULT, &
                    BMULT, RADABV(IHOUR, 3), &
                    FBEAM(IHOUR, 3), TAIR(IHOUR), PREVTSOIL, ARHO(LGP(IPT), 3), &
                    ATAU(LGP(IPT), 3), RHOSOL(3), DIFUP, &
                    DIFDN, SCLOST, DOWNTH)

                ! Lost scattered radiation for each tree (W m-2), averaged over the grid points.
                ! RAD June 2008.              
                SCLOSTTREE(ITAR, 1) = SUM(SCLOST(1:NUMPNT, 1)) / NUMPNT
                SCLOSTTREE(ITAR, 2) = SUM(SCLOST(1:NUMPNT, 2)) / NUMPNT
                ! Assume zero reflectance in TR waveband (Norman 1979)
                SCLOSTTREE(ITAR, 3) = 0.
                ! Downward thermal flux, averaged across grid points in this tree:
                DOWNTHTREE(ITAR) = SUM(DOWNTH) / NUMPNT
                
                !print *,'DFLUX before ',DFLUX(IPT,3)

                ! Calculate absorbed radiation
                CALL ABSRAD(IPT, 3, NZEN, DEXT, BEXT, BMULT, RELDF(IPT), RADABV(IHOUR, 3), FBEAM(IHOUR, 3), ZEN(IHOUR), &
                ABSRP(LGP(IPT), 3), DIFDN(IPT, 3), DIFUP(IPT, 3), DFLUX, BFLUX, SCATFX)
                
                !print *,'DFLUX after ',DFLUX(IPT,3)

                ! Absorbed thermal radiation
                ATHR = DFLUX(IPT, 3)

                DO IAGE = 1, NOAGEP ! Loop over age classes
                    AREA = DLI(IAGE, IPT) * VL(IPT) ! m2

                    ! Sum outputs for the hour (Added RAD, Aug. 2008).
                    CALL SUMHR(0.0, 0.0, ATHR, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, AREA, IHOUR, LGP(IPT), &
                    ITAR, NOTARGETS, NUMPNT, NSUMMED, TOTTMP, PPAR, PPS, PTRANSP, THRAB, &
                    FCO2, FRESPF, GSCAN, GBHCAN, FH2O, FHEAT, TCAN, FSOIL1, PSILCAN, PSILCANMIN, CICAN, &
                    ECANMAX, ACANMAX)
                END DO ! End loop over age classes.
            END DO ! Loop over gridpoints (nighttime).

            ! Calculate night-time foliage respiration
            DO IPT = 1, NUMPNT
                DO IAGE = 1, NOAGEP
                    AREA = DLI(IAGE, IPT) * VL(IPT)
                    RESPF = AREA * RESP(RD0(LGP(IPT), IAGE), RD0ACC, TAIR(IHOUR), TMOVE, Q10F, K10F, RTEMP, 1.0, TBELOW)
                    FCO2(ITAR, IHOUR) = FCO2(ITAR, IHOUR) - RESPF
                    FRESPF(ITAR, IHOUR) = FRESPF(ITAR, IHOUR) + RESPF
                END DO
            END DO

        END IF ! If day or night

        !DO 80 IPT = 1,NUMPNT

        ! No good (does not run for Tumbarumba, and has not been updated since looping order change).
        !! Calculate non-foliage maintenance respiration (in umol tree-1 s-1)
        !        FRESPW(ITAR,IHOUR) = RESP(RMW,TAIR(IHOUR),
        !     &      Q10W,RTEMPW,1.0,TBELOW) 
        !     &    * WBIOM
        !        FRESPB(ITAR,IHOUR) = RESP(RMB,TAIR(IHOUR),
        !     &      Q10B,RTEMPB,1.0,TBELOW) 
        !     &    * BBIOM
        !        FRESPFR(ITAR,IHOUR) = RESP(RMFR,TSOIL(IHOUR),
        !     &      Q10R,RTEMPR,1.0,TBELOW) 
        !     &    * RBIOM * FRFRAC
        !        FRESPCR(ITAR,IHOUR) = RESP(RMCR,TSOIL(IHOUR),
        !     &      Q10R,RTEMPR,1.0,TBELOW) 
        !     &    * RBIOM * (1. - FRFRAC)

        !write(*,*)TAIR(IHOUR), TCAN(ITAR,IHOUR)

    END DO ! End loop over trees

    ! Do the water balance.
    ! Throughfall, soil evaporation, root water uptake, infiltration of surface water,
    ! gravitational drainage.

    ! Get the leaf areas of all the target trees this timestep.
    ! Because Maestra works like this : 
    ! 1) sort trees around current target tree
    ! 2) interpolate leaf area if not directly input
    ! we have to 'unsort' the leaf areas to find all current target tree leaf areas.
    ! This is needed in the SCALEUP routine below.   
    TARGETFOLS = 0
    K = 1
    DO I = 1, NOTREES
        IF (IT(I) .EQ. ITARGETS(K))THEN
            TARGETFOLS(K) = FOLT(I)
            K = K + 1
        ENDIF
    ENDDO

    ! Get area-based estimates of radiation interception and transpiration rate.
    CALL SCALEUP(IHOUR, USESTAND, NOTARGETS, NOALLTREES, TARGETFOLS, ITARGETS, TOTLAI, STOCKING, &
    SCLOSTTREE, THRAB, RADABV, FH2O, PLOTAREA, &
    DOWNTHTREE, RGLOBABV, RGLOBUND, RADINTERC, FRACAPAR, ISIMUS, FH2OUS(IHOUR), THRABUS(IHOUR), &
    PARUSMEAN(IHOUR), SCLOSTTOT, GSCAN, WINDAH(IHOUR), ZHT, Z0HT, ZPD, PRESS(IHOUR), TAIR(IHOUR), &
    VPD(IHOUR), ETMM, ETUSMM)

    ! Find soil surface temperature, unless this is input data.
    ! Note this uses DRYTHICK from previous timestep (or initial value in first go).
    IF (SIMTSOIL .EQ. 1) THEN
        VIEWFACTOR = 1.0 ! OBSOLETE...
        CALL FINDSOILTK(iday, TAIR(IHOUR) + FREEZE, GAMSOIL, PRESS(IHOUR), SOILTK, SOILTEMP(2), VPD(IHOUR)/1000, &
        RGLOBUND, THERMCOND(1), LAYTHICK(1), POREFRAC(1), SOILWP(1), DRYTHICK, TORTPAR, VIEWFACTOR)
    ELSE
        SOILTK = TSOIL(IHOUR) + FREEZE
    ENDIF

    ! Calculate components of heat balance. (latent heat flux (QE) is needed for water balance).
    IF (USEMEASET .EQ. 0.AND.SIMSOILEVAP .EQ. 1) THEN
        CALL ENERGYCALC(SOILTK, GAMSOIL, PRESS(IHOUR), SOILTEMP(2), &
        VPD(IHOUR)/1000, RGLOBUND, TAIR(IHOUR) + FREEZE, THERMCOND(1), &
        LAYTHICK(1), POREFRAC(1), SOILWP(1), DRYTHICK, TORTPAR, VIEWFACTOR, &
        QH, QE, QN, QC, ESOIL)
        !ENDIF

        ! Or, do not calculate heat balance. Either if using measured ET for water balance,
        ! or if not simulating soil evaporation (which would render heat balance meaningless anyway).
        !IF(USEMEASET.EQ.1.OR.SIMSOILEVAP.EQ.0)THEN
    ELSE
        QE = 0
        QH = 0
        QN = 0
        QC = 0
    ENDIF

    ! Layered water balance, outputs new soil water content, discharge,
    ! soil evaporation, overflow, thickness of dry layer.
    CALL WATBALLAY(IDAY, IHOUR, PPT(IHOUR), RUTTERB, RUTTERD, MAXSTORAGE, THROUGHFALL, RADINTERC, CANOPY_STORE, &
    EVAPSTORE, DRAINSTORE, SURFACE_WATERMM, POREFRAC, WETTINGBOT, WETTINGTOP, NLAYER, NROOTLAYER, &
    LAYTHICK, SOILTK, QE, TAIR(IHOUR) + FREEZE, VPD(IHOUR), WINDAH(IHOUR), ZHT, Z0HT, ZPD, PRESS(IHOUR), &
    ETMM, USEMEASET, ETMEAS(IHOUR), FRACUPTAKE, ICEPROP, FRACWATER, DRAINLIMIT, KSAT, BPAR, &
    WSOIL, WSOILROOT, DISCHARGE, DRYTHICKMIN, DRYTHICK, SOILEVAP, OVERFLOW, WATERGAIN, WATERLOSS, &
    PPTGAIN, KEEPWET, EXPINF)

    ! Heat balance: soil T profile (SOILTEMP).
    IF (USEMEASET .EQ. 0.AND.USEMEASSW .EQ. 0)THEN
        CALL HEATBALANCE(NLAYER, FRACWATER, POREFRAC, TAIR(IHOUR) + FREEZE, SOILTK, SOILTEMP, LAYTHICK, WATERGAIN, &
        WATERLOSS, PPTGAIN, THERMCOND)
    ELSE IF (USEMEASET .EQ. 1)THEN
        ! Do nothing.
        SOILTEMP = SOILTK
    ENDIF

    ! Output water balance
    CALL OUTPUTWATBAL(IDAY, IHOUR, NROOTLAYER, NLAYER, WSOIL, &
    WSOILROOT, PPT(IHOUR), &
    CANOPY_STORE, EVAPSTORE, DRAINSTORE, &
    SURFACE_WATERMM, ETMM, ETMM2, USEMEASET, ETMEAS(IHOUR), DISCHARGE, &
    FRACWATER, WEIGHTEDSWP, TOTESTEVAP, &
    DRYTHICK, SOILEVAP, OVERFLOW, THERMCOND, FRACUPTAKE, SOILMOISTURE, &
    FSOIL1, NSUMMED, TOTTMP, SOILTEMP - FREEZE, &
    TAIR(IHOUR), QH, QE, QN, QC, RGLOBUND, RGLOBABV, RGLOBABV12, RADINTERC, &
    ESOIL, TOTLAI, WTITLE, &
    RADINTERC1, RADINTERC2, RADINTERC3, SCLOSTTOT)

    ! Output hourly totals
    CALL OUTPUTHRANDSAVE(IDAY + 1, IHOUR, NOTARGETS, ITARGETS, ISPECIES, TCAN, NOLAY, PPAR, &
    PPS, PTRANSP, FOLLAY, THRAB, FCO2, FRESPF, FRESPW, FRESPB, FH2O, GSCAN, GBHCAN, &
    FH2OCAN, FHEAT, VPD, TAIR, UMOLPERJ * RADABV(1:KHRS, 1), PSILCAN, PSILCANMIN, CICAN, &
    ECANMAX, ACANMAX, ZEN, AZ, result)
    MaespaResultPointer = result
    
!   print *,'FH20 (in MaespaSingleTreeSingleLoop)',FH2O(1,IHOUR)*1e-3

    CALL SUMDAILYWAT(WSOIL, WSOILROOT, WEIGHTEDSWP, PPT, ETMM, ETMEAS, DISCHARGE, SOILEVAP, FSOIL1, SURFACE_WATERMM, QH, QE, QN,QC,&
    RADINTERC, WSOILMEAN, WSOILROOTMEAN, SWPMEAN, PPTTOT, ETMMTOT, ETMEASTOT, DISCHARGETOT, SOILEVAPTOT, &
    FSOILMEAN, TFALLTOT, QHTOT, QETOT, QNTOT, QCTOT, RADINTERCTOT)


    !**********************************************************************





END SUBROUTINE MaespaSingleTreeSingleLoop





!**********************************************************************
SUBROUTINE OUTPUTHRANDSAVE(IDAY,IHOUR,NOTARGETS,ITARGETS,ISPECIES,         &
                    TCAN,NOLAY,PPAR,PPS,PTRANSP,FOLLAY,             &
                    THRAB,FCO2,FRESPF,FRESPW,FRESPB,                &
                    FH2OT,GSCAN,GBHCAN,FH2OCAN,FHEAT,VPD,TAIR,PAR,  &
                    PSILCAN,PSILCANMIN,CICAN,ECANMAX,ACANMAX,ZEN,AZ, result)
! Output the hourly totals
!**********************************************************************
    USE switches
    USE maestcom
    USE MaespaResult
   
    IMPLICIT NONE
    INTEGER NOTARGETS,IDAY,IHOUR,ITAR,ITREE,ISPEC,I,NOLAY
    INTEGER ITARGETS(MAXT),ISPECIES(MAXT)
    
    REAL THRAB(MAXT,MAXHRS,3),FCO2(MAXT,MAXHRS),FRESPF(MAXT,MAXHRS)
    REAL FRESPW(MAXT,MAXHRS),FRESPB(MAXT,MAXHRS)
    REAL GSCAN(MAXT,MAXHRS),FH2OT(MAXT,MAXHRS),FH2OCAN(MAXT,MAXHRS)
    REAL FHEAT(MAXT,MAXHRS)
    REAL GBHCAN(MAXT,MAXHRS)
    REAL PPAR(MAXT,MAXLAY,MAXHRS),PPS(MAXT,MAXLAY,MAXHRS)
    REAL PTRANSP(MAXT,MAXLAY,MAXHRS)
    REAL FOLLAY(MAXLAY),TCAN(MAXT,MAXHRS),VPD(MAXHRS)
    REAL TAIR(MAXHRS),PAR(MAXHRS)
    REAL ECANMAX(MAXT,MAXHRS),ACANMAX(MAXT,MAXHRS)
    REAL PSILCAN(MAXT,MAXHRS),PSILCANMIN(MAXT,MAXHRS),CICAN(MAXT,MAXHRS)
    REAL ZEN(MAXHRS), AZ(MAXHRS)

   TYPE(maesparesulttype) :: result !! type to store output from maespa loop

    IF (IOHRLY.GE.1) THEN
        DO ITAR=1,NOTARGETS
            ITREE = ITARGETS(ITAR)
            ISPEC = ISPECIES(ITREE)



             !store MAESPA output for TUF, assuming that there will only be one tree
                result%DOY=IDAY        ! simulation date
                result%Tree=ITREE       ! tree number
                result%Spec=ISPEC       ! tree species number
                result%Hour=IHOUR       ! hour of the day
                result%hrPAR=THRAB(ITAR,IHOUR,1)*UMOLPERJ      ! absorbed PAR              umol tree-1 s-1
                result%hrNIR=THRAB(ITAR,IHOUR,2)      ! absorbed NIR              W tree-1
                result%hrTHM=THRAB(ITAR,IHOUR,3)      ! absorbed thermal          W tree-1
                result%hrPS=FCO2(ITAR,IHOUR)       ! photosynthesis (net of leaf resp) umol tree-1 s-1
                result%hrRf=FRESPF(ITAR,IHOUR)       ! hourly leaf respiration   umol tree-1 s-1
                result%hrRmW=FRESPW(ITAR,IHOUR)+FRESPB(ITAR,IHOUR)      ! hourly stem + branch Rm   umol tree-1 s-1
                result%hrLE=FH2OT(ITAR,IHOUR)*1e-3       ! hourly transpiration      mmol tree-1 s-1
                result%LECAN=FH2OCAN(ITAR,IHOUR)*1E-3      ! hourly transpirn: CANOPY calc : mmol H2O m-2 s-1
                result%Gscan=GSCAN(ITAR,IHOUR)      ! canopy stomatal conductance : mol CO2 tree-1 s-1
                result%Gbhcan=GBHCAN(ITAR,IHOUR)     ! canopy boundary layer conductance to heat : mol tree-1 s-1
                result%hrH=FHEAT(ITAR,IHOUR)*1E-3        ! hourly sensible heat flux:  MJ tree-1 s-1
                result%TCAN=TCAN(ITAR,IHOUR)       ! Average foliage temperature (deg C)
                result%ALMAX=ACANMAX(ITAR,IHOUR)      ! Canopy maximum leaf photosynthesis rate (umol m-2 s-1)
                result%PSIL=PSILCAN(ITAR,IHOUR)       ! Canopy average leaf water potential (MPa)
                result%PSILMIN=PSILCANMIN(ITAR,IHOUR)    ! Canopy minimum leaf water potential (MPa)
                result%CI=CICAN(ITAR,IHOUR)         ! Canopy average intercellular CO2 conc. (ppm)
                result%TAIR=TAIR(IHOUR)       ! Air temperature (deg C)
                result%VPD=VPD(IHOUR)/1000        ! vapor pressure deficit (kPa)
                result%PAR=PAR(IHOUR)        ! Above-canopy incident PAR (umol m-2 s-1)
                print *,'stored some results'



            IF (IOFORMAT .EQ. 0) THEN
                WRITE (UHRLY,500) IDAY,ITREE,ISPEC,IHOUR,                                       &
                                    THRAB(ITAR,IHOUR,1)*UMOLPERJ,THRAB(ITAR,IHOUR,2),           &
                                    THRAB(ITAR,IHOUR,3),FCO2(ITAR,IHOUR),FRESPF(ITAR,IHOUR),    &
                                    FRESPW(ITAR,IHOUR)+FRESPB(ITAR,IHOUR),                      &
                                    FH2OT(ITAR,IHOUR)*1E-3,                                     &
                                    FH2OCAN(ITAR,IHOUR)*1E-3,GSCAN(ITAR,IHOUR),GBHCAN(ITAR,IHOUR),  &
                                    FHEAT(ITAR,IHOUR)*1E-3,TCAN(ITAR,IHOUR),                    &
                                    ACANMAX(ITAR,IHOUR),                    &   
                                    PSILCAN(ITAR,IHOUR),PSILCANMIN(ITAR,IHOUR),CICAN(ITAR,IHOUR),  &
                                    TAIR(IHOUR),VPD(IHOUR)/1000,PAR(IHOUR), &
                                    ZEN(IHOUR),AZ(IHOUR)                    !rjout mathias mars 2013
                500 FORMAT (I7,1X,3(I4,1X),3(F12.5,1X),16(F12.5,1X),2(F12.5,1X))    ! rajout de 2, mathias mars 2013
            ELSE IF (IOFORMAT .EQ. 1) THEN
                WRITE (UHRLY) REAL(IDAY),REAL(ITREE),REAL(ISPEC),REAL(IHOUR),               &
                                THRAB(ITAR,IHOUR,1)*UMOLPERJ,THRAB(ITAR,IHOUR,2),           &
                                THRAB(ITAR,IHOUR,3),FCO2(ITAR,IHOUR),FRESPF(ITAR,IHOUR),    &
                                FRESPW(ITAR,IHOUR)+FRESPB(ITAR,IHOUR),                      &
                                FH2OT(ITAR,IHOUR)*1e-3,                                     &
                                FH2OCAN(ITAR,IHOUR)*1E-3,GSCAN(ITAR,IHOUR),GBHCAN(ITAR,IHOUR),        &
                                FHEAT(ITAR,IHOUR)*1E-3,TCAN(ITAR,IHOUR),                    &
                                ACANMAX(ITAR,IHOUR),                    &   
                                PSILCAN(ITAR,IHOUR),PSILCANMIN(ITAR,IHOUR),CICAN(ITAR,IHOUR),  &
                                TAIR(IHOUR),VPD(IHOUR)/1000,PAR(IHOUR)
            END IF                        
        END DO
    END IF
    IF (IOFORMAT .EQ. 0) THEN
        IF (IOHRLY.GE.2) THEN

            WRITE (ULAY,610) 'DAY',IDAY,'HOUR',IHOUR
            610   FORMAT (A5,I5,A5,I5)
            IF (FOLLAY(1).GT.0.0) THEN
                WRITE (ULAY,600) (PPAR(1,I,IHOUR)/FOLLAY(I),I=1,NOLAY)
                WRITE (ULAY,600) (PPS(1,I,IHOUR)/FOLLAY(I),I=1,NOLAY)
                WRITE (ULAY,600) (PTRANSP(1,I,IHOUR)/FOLLAY(I),I=1,NOLAY)
                600   FORMAT (10(F10.2,1X))
            ELSE
                WRITE (ULAY,*) 'NO FOLIAGE AT THIS TIME'
            END IF
        END IF
    ELSE IF (IOFORMAT .EQ. 1) THEN
        IF (IOHRLY.GE.2) THEN
            WRITE (ULAY) REAL(IDAY),REAL(IHOUR)
        
            IF (FOLLAY(1).GT.0.0) THEN
                WRITE (ULAY) (PPAR(1,I,IHOUR)/FOLLAY(I),I=1,NOLAY)
                WRITE (ULAY) (PPS(1,I,IHOUR)/FOLLAY(I),I=1,NOLAY)
                WRITE (ULAY) (PTRANSP(1,I,IHOUR)/FOLLAY(I),I=1,NOLAY)
            ELSE
                ! No foliage at this time
                WRITE (ULAY) -999.9
            END IF
        END IF
    END IF    
    RETURN
END SUBROUTINE OUTPUTHRANDSAVE




!**********************************************************************
      SUBROUTINE TRANSBDISABLE(IHOUR,IPROG, &
        ZENITH,AZMTH,XSLOPE,YSLOPE,FBEAM,BEXTT, &
        XPT,YPT,ZPT,RX,RY,RZ,DXT,DYT,DZT, &
        XMAX,YMAX,SHADEHT, &
        FOLT,ZBC,JLEAFT,BPTT,NOAGECT,PROPCT,JSHAPET,SHAPET, &
        NT,SLA,BEXT)
! a subroutine to calculate the transmittances of direct radiation
! and also the within and between-tree shading for beam
!**********************************************************************

      USE maestcom
      IMPLICIT NONE
      INTEGER IHOUR,IPROG,NT,IFLAG,IRAD
      INTEGER JSHAPET(MAXT),JLEAFT(MAXT),NOAGECT(MAXT)
      REAL DXT(MAXT),DYT(MAXT),DZT(MAXT),RX(MAXT),RY(MAXT), &
           RZ(MAXT),ZBC(MAXT),FOLT(MAXT)
      REAL BPT(8,MAXC),PROPCT(MAXC,MAXT)
      REAL FBEAM(MAXHRS,3)
      REAL SHAPET(MAXT),BEXTT(MAXT)
      REAL BPTT(8,MAXC,MAXT)
      REAL SLA,BEXT,SLOPE,AZMTH,XSLOPE,YSLOPE,ZENITH,XPT,YPT,ZPT
      REAL XMAX,YMAX,SHADEHT,S,S1,BPATH,SHU1,BTEMP
      LOGICAL, EXTERNAL :: SHADED

!  Set flag so that only upper hemisphere will be considered.
      IFLAG = 1
      IRAD = 1

! Default value
      SLA = 0.0

! Default value for BEXT (output!) is simple average across trees.
! This needs to be done, because SCATTER needs a BEXT estimate, even
! if path length = 0 (in which case weighted BEXT estimate is not
! calculated in TREDST).
      BEXT = SUM(BEXTT(1:NT))/REAL(NT)

      SLOPE = ATAN(COS(AZMTH)*TAN(XSLOPE)+SIN(AZMTH)*TAN(YSLOPE))
      IF ((PID2-ZENITH).LT.SLOPE) then
          print *,'pid-zenith lt slope'
          RETURN
      endif
      IF (SHADED(XPT,YPT,ZPT,ZENITH,AZMTH,XMAX,YMAX,SHADEHT)) then
!          print *,'shaded'
          RETURN
      endif
      
!      print *,FBEAM(:,1)
!      print *,FBEAM(:,2)
!      print *,IHOUR
!      print *,FBEAM(IHOUR,1)
!      print *,FBEAM(IHOUR,2)
      IF ((FBEAM(IHOUR,1).GT.0.0).OR.(FBEAM(IHOUR,2).GT.0.00)) THEN
!        print *,'call tredst'
        CALL TREDST(IFLAG,IPROG,IRAD,ZENITH,AZMTH, &
            XPT,YPT,ZPT,RX,RY,RZ,DXT,DYT,DZT, &
            FOLT,ZBC,JLEAFT,BPTT,NOAGECT,PROPCT,JSHAPET,SHAPET, &
            BEXTT,NT,S,S1,BEXT)
!       BPATH = the total weighted beam pathlength
        BPATH = S + S1
!       SHU1= the weighted pathlength in the target tree.
        SHU1 = S
!       SLA is the sunlit leaf area associated with the grid point.
        BTEMP = -BEXT*BPATH
        IF (BTEMP.LT.-180.0) THEN
          SLA = 0.0
        ELSE          
          SLA = EXP(BTEMP)
        END IF
      else
!          print *,'not call tredst'
      ENDIF

      RETURN
      END SUBROUTINE TRANSBDISABLE

            
!**********************************************************************
      SUBROUTINE ZENAZ_1HOUR(TIME,IDOY,KHRSlocal,ALAT,BEAR,DEC,ZEN,AZ)
! Calculates hourly zenith and azimuth angles.
! Corrected 07/05 BM
! Code from ORIGMAES from Ying Ping
! Note: BEAR is the bearing (in degrees clockwise) of the x-axis from south
! If the x-axis is S, BEAR = 0; x-axis W, BEAR = 90; x-axis N, BEAR 180; x-axis E, BEAR -90
! AZ, the azimuth angle, is in radians anticlockwise from S (trig convention)
! e.g. azimuth S, AZ = 0; azimuth W, AZ = -pi/2; azimuth N, AZ = pi, azimuth E, AZ = +pi/2
!
!          
!  KN -adapted to just generate a single value for a specific time.          
!**********************************************************************
          ! ALAT - latitude, in radians (-ve for S hemisphere)
          ! TTIMD - time difference between longitude of plot & longitude of time zone, in hours
          !    REAL, PARAMETER :: HHRS = (KHRS) / 2.0           ! Half a day length

      USE maestcom
      IMPLICIT NONE
      REAL I
      REAL ALAT,TTIMD,BEAR,DEC,EQNTIM
      REAL ZEN,AZ,SINAZ
      real ZEN1,AZ1,SINAZ1
      REAL R,HI,AI,AIP1
      INTEGER IDOY
      REAL HHRSlocal
      INTEGER KHRSlocal
      REAL time
      REAL DAYL,SUNSET
      
      !! so this will likely be 24
      !HHRSlocal = KHRSlocal/2.0
      HHRSlocal = KHRSlocal
      
!      REAL TM,LAT,ZEN_return,AZIM,CZ,INOT,CA
!      INTEGER JDAY
      
      !!assume TTIMD
      TTIMD=0
      !! this should set DEC, EQTIM, DAYL, and SUNSET
      CALL SUN(IDOY,ALAT,TTIMD,DEC,EQNTIM,DAYL,SUNSET)

      !! this would normally run from 1 to 48 (perhaps 24? depending on KHRS)
      !! modify so that TIME passed in is something like 15.5 which should become 30, so 15.5*2, just assuing KHRS will be 48
!      DO 10 I = 1,KHRS
      I=(TIME)
          
      R = I - 0.5
      HI = (R-TTIMD-EQNTIM-HHRSlocal)*PI/HHRSlocal
      ZEN = ACOS(SIN(ALAT)*SIN(DEC) + COS(ALAT)*COS(DEC)*COS(HI))
      SINAZ = COS(DEC)*SIN(HI)/SIN(ZEN)
      IF (SINAZ.GT.1.0) THEN
        SINAZ = 1.0
      ELSE IF (SINAZ.LT.-1.0) THEN
        SINAZ = -1.0
      END IF
      AZ = ASIN(SINAZ)
      
      !! one more loop to calculate AZ1
      R = R + 1
      R = I - 0.5
      HI = (R-TTIMD-EQNTIM-HHRSlocal)*PI/HHRSlocal
      ZEN1 = ACOS(SIN(ALAT)*SIN(DEC) + COS(ALAT)*COS(DEC)*COS(HI))
      SINAZ1 = COS(DEC)*SIN(HI)/SIN(ZEN1)
      IF (SINAZ1.GT.1.0) THEN
        SINAZ1 = 1.0
      ELSE IF (SINAZ1.LT.-1.0) THEN
        SINAZ1 = -1.0
      END IF
      AZ1 = ASIN(SINAZ1)
            
!10    CONTINUE

!      DO 20 I = 1,KHRS

        IF (ABS(ZEN).LE.1.57) THEN      ! Daytime calcs only
          AI=AZ
          AIP1 = AZ1
          IF (AI.GE.0.0) THEN
            IF (AIP1.LT.AI) AZ1=PI-AIP1
          ELSE
            IF (AI.GE.AIP1) AZ=-PI-AI
          END IF
          IF (ALAT.LT.0.0) THEN
          AZ = AZ+PI+BEAR            ! Southern hemisphere
          ELSE
          AZ = -AZ+BEAR                  ! Northern hemisphere
          END IF
        END IF


!        print *,'TIME,IDOY,KHRSlocal,ALAT,BEAR,DEC,ZEN,AZ',TIME,IDOY,KHRSlocal,ALAT,BEAR,DEC,ZEN*180./pi,AZ*180./pi
!        print *,'TTIMD,DEC,EQNTIM,DAYL,SUNSET',TTIMD,DEC,EQNTIM,DAYL,SUNSET
       
!20    CONTINUE

      RETURN
      END SUBROUTINE ZENAZ_1HOUR

      
!**********************************************************************
      SUBROUTINE ZENAZ(ALAT,TTIMD,BEAR,DEC,EQNTIM,ZEN,AZ)
! Calculates hourly zenith and azimuth angles.
! Corrected 07/05 BM
! Code from ORIGMAES from Ying Ping
! Note: BEAR is the bearing (in degrees clockwise) of the x-axis from south
! If the x-axis is S, BEAR = 0; x-axis W, BEAR = 90; x-axis N, BEAR 180; x-axis E, BEAR -90
! AZ, the azimuth angle, is in radians anticlockwise from S (trig convention)
! e.g. azimuth S, AZ = 0; azimuth W, AZ = -pi/2; azimuth N, AZ = pi, azimuth E, AZ = +pi/2
!**********************************************************************

      USE maestcom
      IMPLICIT NONE
      INTEGER I
      REAL ALAT,TTIMD,BEAR,DEC,EQNTIM
      REAL ZEN(MAXHRS),AZ(MAXHRS),SINAZ
      REAL R,HI,AI,AIP1
      
      REAL TM,LAT,ZEN_return,AZIM,CZ,INOT,CA
      INTEGER JDAY
      
      REAL TIME
      INTEGER IDOY
      INTEGER KHRSlocal
      REAL ALATlocal,BEARlocal,DEClocal,ZENlocal,AZlocal

      DO 10 I = 1,KHRS
          
        R = REAL(I) - 0.5
        HI = (R-TTIMD-EQNTIM-HHRS)*PI/HHRS

        ZEN(I) = ACOS(SIN(ALAT)*SIN(DEC) + COS(ALAT)*COS(DEC)*COS(HI))
        SINAZ = COS(DEC)*SIN(HI)/SIN(ZEN(I))
        IF (SINAZ.GT.1.0) THEN
          SINAZ = 1.0
        ELSE IF (SINAZ.LT.-1.0) THEN
          SINAZ = -1.0
        END IF
        AZ(I) = ASIN(SINAZ)
            
10    CONTINUE

      DO 20 I = 1,KHRS

        IF (ABS(ZEN(I)).LE.1.57) THEN      ! Daytime calcs only
          AI=AZ(I)
          AIP1 = AZ(I+1)
          IF (AI.GE.0.0) THEN
            IF (AIP1.LT.AI) AZ(I+1)=PI-AIP1
          ELSE
            IF (AI.GE.AIP1) AZ(I)=-PI-AI
          END IF
          IF (ALAT.LT.0.0) THEN
          AZ(I) = AZ(I)+PI+BEAR            ! Southern hemisphere
          ELSE
          AZ(I) = -AZ(I)+BEAR                  ! Northern hemisphere
          END IF
        END IF

      
!       JDAY=82
!       TM=(i-1)*24/khrs
!       LAT=-0.6544985
!        
!      CALL SUNPOS(JDAY,TM,LAT,ZEN_return,AZIM,CZ,INOT,CA)
!      
!      
!      !SUBROUTINE SUNPOS(JDAY,TM,LAT,ZEN_return ,AZIM,CZ,INOT,CA)
!!       az=AZIM*180./pi
!!       zen=zeni*180./pi
!!       ralt=90.-zen
!
!      IDOY=22685
!      TIME=i
!      call ZENAZ_1HOUR(TIME,IDOY,KHRS,ALAT,BEARlocal,DEClocal,ZENlocal,AZlocal)
!      
!      print *,'i=',i,'tm=',tm, 'ALAT,TTIMD',ALAT,TTIMD
!      print *,'az=',az(i),'azd=',az(i)*180./pi,'azdT=',azim*180./pi,'z=',zen(i),'zd=',zen(i)*180/PI,'zdT=',zen_return*180./pi
!      print *,'90-zd=',90-zen(i)*180/PI,'90-zdT=',90-zen_return*180./pi,'sin',sin(pid2-zen(i)),'sinT',sin(pid2-zen_return)
       
20    CONTINUE

      RETURN
      END SUBROUTINE ZENAZ

      
!**********************************************************************
      SUBROUTINE TREDST_1hour(IFLAG,IPROG,IRAD,DZ,DAZ, &
        XPT,YPT,ZPT,RX,RY,RZ,DXT,DYT,DZT, &
        FOLT,ZBC,JLEAFT,BPTT,NOAGECT,PROPCT,JSHAPET,SHAPET, &
        EXTC,NT,S,S1,EFFK)
! This subroutine is used to calculate the weighted pathlength,
! which is also kernel function of radiation penetration.
!
! EXTC is an array with tree extinction coefficients. (new, Feb. 2009).
! DEXT is now output; it is the extinction coefficient weighted by
! pathlengths. This is the effective K for the whole path.
!**********************************************************************

      USE maestcom
      IMPLICIT NONE
      INTEGER JSHAPET(MAXT),JLEAFT(MAXT),NOAGECT(MAXT)
      INTEGER M,NT,IPROG,IRAD,IFLAG
      
      REAL DXT(MAXT),DYT(MAXT),DZT(MAXT)
      REAL RX(MAXT),RY(MAXT),RZ(MAXT),ZBC(MAXT),FOLT(MAXT)
      REAL BPT(8,MAXC),PROPCT(MAXC,MAXT)
      REAL SHAPET(MAXT),EXTC(MAXT),BPTT(8,MAXC,MAXT)
      REAL TANAZ,SINAZ,S,S1,SW,S1W,DAZ
      REAL PATH, XTPOS, YTPOS, XPT,YPT,DZ,ZPT,X1,Y1,Z1
      REAL X2,Y2,Z2,AVGDL,SS,SSW,EFFK
      LOGICAL, EXTERNAL :: POSSIBLE

      TANAZ = TAN(DAZ)
      SINAZ = SIN(DAZ)

! Zero the pathlengths.
! S1 is distance in target tree, S is distance in other trees.
! Note: in normal program, gridpoints are all in target tree. In
! test program, gridpoints may be outside. Flag IPROG indicates this.
      S = 0.0
      S1 = 0.0
      SW = 0.0
      S1W = 0.0

!  Loop over each tree to see if it affects the ray passing through the point.
      DO 200 M = 1,NT

!  choose the largest of the x and y radii for use in a quick test to
!  see if the tree could possibly be in the way.
        PATH = 0.00
        XTPOS = DXT(M) - XPT
        YTPOS = DYT(M) - YPT

        IF ((M.EQ.1).AND.(IPROG.NE.ITEST)) THEN
          !if(irad.eq.1)write(uwattest,*)m
          CALL DISTIN(IRAD,JSHAPET(M),IFLAG,DZ,DAZ,XPT,YPT,ZPT, &
            RX(M),RY(M),RZ(M),ZBC(M),DXT(M),DYT(M),DZT(M), &
            PATH,X1,Y1,Z1,X2,Y2,Z2)

        ELSE IF (POSSIBLE(XTPOS,YTPOS,RX(M),RY(M),SINAZ,TANAZ,DAZ)) THEN

          CALL DIST(JSHAPET(M),IFLAG,DZ,DAZ,XPT,YPT,ZPT, &
            RX(M),RY(M),RZ(M),ZBC(M),DXT(M),DYT(M),DZT(M), &
            PATH,X1,Y1,Z1,X2,Y2,Z2)
         END IF

        IF (PATH.EQ.0.00) GO TO 200

        IF (JLEAFT(M).EQ.0) THEN
          AVGDL = FOLT(M)/ (RX(M)*RY(M)*RZ(M)*SHAPET(M)*PI)
          SS = PATH*AVGDL
          ! Path length multiplied by K for current tree.
          SSW = SS*EXTC(M)
        ELSE
          CALL WPATH(JSHAPET(M),SHAPET(M),JLEAFT(M),BPTT(1:8,1:MAXC,M), &
            NOAGECT(M),PROPCT(1:MAXC,M),X1,Y1,Z1,X2,Y2,Z2,PATH, &
            RX(M),RY(M),RZ(M),ZBC(M),DXT(M),DYT(M),DZT(M),FOLT(M),SS)
            ! Path length multiplied by K for current tree.
            SSW = SS*EXTC(M)
        END IF

        IF ((M.GT.1).OR.(IPROG.EQ.ITEST)) THEN
           S = S + SS
           SW = SW + SSW
        ELSE
           S1 = S1 + SS
           S1W = S1W + SSW
        END IF

200     CONTINUE

! Calculate weighted effective extinction coefficient.
      IF((S+S1).GT.0.0)THEN
        EFFK = (S1W + SW) / (S + S1)
      ELSE
        EFFK = 0.0
      ENDIF

      RETURN
      END SUBROUTINE TREDST_1hour  !Tredst

      

!**********************************************************************
      SUBROUTINE TRANSB_1hour(IHOUR,IPROG, &
        ZENITH,AZMTH,XSLOPE,YSLOPE,FBEAM1HR,BEXTT, &
        XPT,YPT,ZPT,RX,RY,RZ,DXT,DYT,DZT, &
        XMAX,YMAX,SHADEHT, &
        FOLT,ZBC,JLEAFT,BPTT,NOAGECT,PROPCT,JSHAPET,SHAPET, &
        NT,SLA,BEXT)
        !! KN, of these SLA and BEXT are calculated locally
! a subroutine to calculate the transmittances of direct radiation
! and also the within and between-tree shading for beam
!**********************************************************************
        
        !! IHOUR = time
        !! IPROG = INORMAL |   INTEGER, PARAMETER :: INORMAL = 0 
        !! FBEAM - need FBEAM(IHOUR,1) and FBEAM(IHOUR,2)
        !! BEXTT - need BEXTT(1:NT)

      USE maestcom
      IMPLICIT NONE
      INTEGER IHOUR,IPROG,NT,IFLAG,IRAD
      INTEGER JSHAPET(MAXT),JLEAFT(MAXT),NOAGECT(MAXT)
      REAL DXT(MAXT),DYT(MAXT),DZT(MAXT),RX(MAXT),RY(MAXT), &
           RZ(MAXT),ZBC(MAXT),FOLT(MAXT)
      REAL BPT(8,MAXC),PROPCT(MAXC,MAXT)
      REAL FBEAM1HR(3)  !! KN, changing FBEAM to just have a single hour in it, so eliminating the first dimension
      REAL SHAPET(MAXT),BEXTT(MAXT)
      REAL BPTT(8,MAXC,MAXT)
      REAL SLA,BEXT,SLOPE,AZMTH,XSLOPE,YSLOPE,ZENITH,XPT,YPT,ZPT
      REAL XMAX,YMAX,SHADEHT,S,S1,BPATH,SHU1,BTEMP
      LOGICAL, EXTERNAL :: SHADED
      
!  Set flag so that only upper hemisphere will be considered.
      IFLAG = 1
      IRAD = 1

! Default value
      SLA = 0.0

! Default value for BEXT (output!) is simple average across trees.
! This needs to be done, because SCATTER needs a BEXT estimate, even
! if path length = 0 (in which case weighted BEXT estimate is not
! calculated in TREDST).
      BEXT = SUM(BEXTT(1:NT))/REAL(NT)

      SLOPE = ATAN(COS(AZMTH)*TAN(XSLOPE)+SIN(AZMTH)*TAN(YSLOPE))

      IF ((PID2-ZENITH).LT.SLOPE) then
          !print *,'pid-zenith lt slope'
          RETURN
      endif
      IF (SHADED(XPT,YPT,ZPT,ZENITH,AZMTH,XMAX,YMAX,SHADEHT)) then
          print *,'shaded'
          RETURN
      endif
      
      IF ((FBEAM1HR(1).GT.0.0).OR.(FBEAM1HR(2).GT.0.00)) THEN
        print *,'call tredst'
        CALL TREDST_1hour(IFLAG,IPROG,IRAD,ZENITH,AZMTH, &
            XPT,YPT,ZPT,RX,RY,RZ,DXT,DYT,DZT, &
            FOLT,ZBC,JLEAFT,BPTT,NOAGECT,PROPCT,JSHAPET,SHAPET, &
            BEXTT,NT,S,S1,BEXT)
!       BPATH = the total weighted beam pathlength
        BPATH = S + S1
!       SHU1= the weighted pathlength in the target tree.
        SHU1 = S
!       SLA is the sunlit leaf area associated with the grid point.
        BTEMP = -BEXT*BPATH
        IF (BTEMP.LT.-180.0) THEN
          SLA = 0.0
        ELSE          
          SLA = EXP(BTEMP)
        END IF
      else
          print *,'not call tredst'
      ENDIF

      RETURN
      END SUBROUTINE TRANSB_1hour
      
!**********************************************************************
SUBROUTINE INPUTTREENEW(UTREESFILE,XSLOPE,YSLOPE,BEAR,X0,Y0,XMAX,YMAX,PLOTAREA,STOCKING,      &
                        ZHT,Z0HT,ZPD,                                           &
                        NOALLTREES,NOTREES,NOTARGETS,ITARGETS,SHADEHT,          &
                        NOXDATES,NOYDATES,NOZDATES,NOTDATES,NOLADATES,NODDATES, &
                        DATESX,DATESY,DATESZ,DATEST,DATESLA,DATESD,             &
                        DX,DY,DZ,R1,R2,R3,TRUNK,FLT,TOTLAITABLE,DIAMA,          &
                        IFLUSH,DT1,DT2,DT3,DT4,EXPTIME,APP,EXPAN,               &
                        WEIGHTS,NSPECIES,ISPECIES)
! This subroutine should read in the data from the trees.dat file on
! crown positions and dimensions. Some variables can change with time:
! radii, height, diameter & leaf area - for these, arrays of dates & values at
! those dates may be read in, & interpolated during the program.
! x- and y- co-ordinates of the crowns may be read in or calculated from
! plot size and stocking density.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER IOERROR
    INTEGER DATESX(maxdate),DATESY(maxdate),DATESZ(maxdate)
    INTEGER DATEST(maxdate),DATESLA(maxdate),DATESD(maxdate)
    INTEGER ITARGETS(MAXT),ISPECIES(MAXT),IPLOTSHAPE,NOALLTREES
    INTEGER NOXDATES,NOYDATES,NOZDATES,NOTDATES,IFLUSH,NOLADATES
    INTEGER NODDATES,NOTREES,NOTARGETS,NSPECIES
    REAL DX(MAXT),DY(MAXT),DZ(MAXT),WEIGHTS(MAXT), EXPFACTORS(MAXT)
    REAL R1(maxdate,MAXT),R2(maxdate,MAXT),R3(maxdate,MAXT)
    REAL TRUNK(maxdate,MAXT),FLT(maxdate,MAXT),TOTLAITABLE(maxdate)
    REAL DIAMA(maxdate,MAXT),PLOTAREA
    REAL X0,Y0,XMAX,YMAX,XSLOPE,YSLOPE,BEAR,SHADEHT,STOCKING
    REAL ZHT,Z0HT,ZPD,DT1,DT2,DT3,DT4,EXPTIME,APP,EXPAN
    INTEGER UTREESFILE
    
    ! Read in number of trees & number of target tree
    CALL READPLOT(UTREESFILE, X0, Y0, XMAX, YMAX, NOALLTREES,XSLOPE, YSLOPE, BEAR, SHADEHT, STOCKING, IPLOTSHAPE)
    PLOTAREA = (XMAX - X0) * (YMAX - Y0)

    ! Read in aerodynamic properties of canopy
    CALL READZPD(UTREESFILE,ZHT,Z0HT,ZPD)

    ! Get x, y, z co-ords of each tree
    CALL READXYZ(UTREESFILE,NOALLTREES,X0,Y0,XMAX,YMAX,XSLOPE,YSLOPE,DX,DY,DZ)

    ! Get radii in x & y directions of each tree
    CALL READTREEARRAY(UTREESFILE,1,NOALLTREES,NOXDATES,DATESX,R1)
    CALL READTREEARRAY(UTREESFILE,2,NOALLTREES,NOYDATES,DATESY,R2)
    ! Get green crown height of each tree
    CALL READTREEARRAY(UTREESFILE,3,NOALLTREES,NOZDATES,DATESZ,R3)
    ! Get trunk length of each tree
    CALL READTREEARRAY(UTREESFILE,4,NOALLTREES,NOTDATES,DATEST,TRUNK)

    ! Get leaf area parameters
    CALL GETLEAFAREA(UTREESFILE,IFLUSH,DT1,DT2,DT3,DT4,EXPTIME,APP,EXPAN,NOALLTREES,NOLADATES,DATESLA,FLT)

    ! Get diameter of each tree
    CALL READTREEARRAY(UTREESFILE,6,NOALLTREES,NODDATES,DATESD,DIAMA)

    ! Calculate total LAI
    CALL CALCLAI(NOLADATES,FLT,NOALLTREES,XMAX,YMAX,XSLOPE,YSLOPE,TOTLAITABLE)

    ! Read in how many of the trees form the subplot (from confile)
    CALL READCONTREES(UCONTROL,NOALLTREES,DX,DY,XMAX,YMAX,NOTREES,NOTARGETS,ITARGETS,IPLOTSHAPE,WEIGHTS)
       
    ! Read species array, if provided.
    CALL READSPECLIST(UTREESFILE, NSPECIES, ISPECIES)

    RETURN
END SUBROUTINE INPUTTREENEW

!**********************************************************************      




!**********************************************************************
    subroutine  readTranmissionFromMaespaFiles(yd_actual,timeis,treeConfigLocation,transmissionPercentage,&
        maespaPAR,maespaFBEAM,maespaSUNLA,maespaTD,maespaTSCAT,maespaTTOT,maespaAPARSUN,maespaAPARSH,maespaAPAR)
! The transmittance of diffuse radiation through one elementary layer
! in the EHC is taken as the minimum of the diffuse transmittances of
! all grid points. This subroutine finds this minimum transmittance.
!**********************************************************************

      USE maestcom
      IMPLICIT NONE
      INTEGER yd_actual,treeState,treeConfigLocation
      INTEGER timeisInt
      INTEGER IOERROR
      INTEGER TESTFLXDAT_FILE 
      INTEGER linesToSkip
      REAL transmissionPercentage,timeis
      real :: dummy
      integer :: n,i, nr_lines, nr_elements, stat
      real, allocatable :: DAY(:),HR(:),PT(:),X(:),Y(:),Z(:),PAR(:),FBEAM(:),SUNLA(:),TD(:),TSCAT(:),TTOT(:),&
                            APARSUN(:),APARSH(:),APAR(:)
      real maespaPAR,maespaFBEAM,maespaSUNLA,maespaTD,maespaTSCAT,maespaTTOT,maespaAPARSUN,maespaAPARSH,maespaAPAR                     
                            
      TESTFLXDAT_FILE = 1245
      linesToSkip = 21
      
      !   DAY HR PT  X  Y  Z  PAR  FBEAM   SUNLA  TD   TSCAT  TTOT APARSUN APARSH APAR 

        !OPEN (UTREES, FILE = trim(in_path)//'trees.dat', STATUS='OLD', IOSTAT=IOERROR)
        OPEN (TESTFLXDAT_FILE, FILE = '/home/kerryn/git/MaespaBaseTesting/maespa/TestMaespa32-33/testflx.dat', STATUS='OLD', &
                        IOSTAT=IOERROR)
        IF (IOERROR.NE.0) THEN
            CALL SUBERROR('ERROR: testflx.dat DOES NOT EXIST', IFATAL, 0)
        ENDIF
        
        do n=1,linesToSkip
            read(TESTFLXDAT_FILE,*) 
        end do
        
        nr_lines = 0
        do
          read(TESTFLXDAT_FILE,*,end=10,err=20) dummy
          nr_lines = nr_lines + 1
        end do
        
        ! rewind back to the beginning of the file for unit=1
        10 rewind(TESTFLXDAT_FILE)
        do n=1,linesToSkip
            read(TESTFLXDAT_FILE,*) 
        end do
        ! number of array elements = number of data lines in the file
        nr_elements=nr_lines
        ! allocate the arrays of given size 
        allocate (DAY(nr_elements) )
        allocate (HR(nr_elements) )
        allocate (PT(nr_elements) )
        allocate (X(nr_elements) )
        allocate (Y(nr_elements) )
        allocate (Z(nr_elements) )
        allocate (PAR(nr_elements) )
        allocate (FBEAM(nr_elements) )
        allocate (SUNLA(nr_elements) )
        allocate (TD(nr_elements) )
        allocate (TSCAT(nr_elements) )
        allocate (TTOT(nr_elements) )
        allocate (APARSUN(nr_elements) )
        allocate (APARSH(nr_elements) )
        allocate (APAR(nr_elements) )
        
        ! read array elements form the file
        do i = 1, nr_elements
          read(TESTFLXDAT_FILE,*,err=20) DAY(i),HR(i),PT(i),X(i),Y(i),Z(i),PAR(i),FBEAM(i),SUNLA(i),TD(i),TSCAT(i),TTOT(i),&
                    APARSUN(i),APARSH(i),APAR(i)
        end do
        close(TESTFLXDAT_FILE)
        
        !timeis is 24 hour, hr is 48 1/2 hours
!        print *,timeis
        timeisInt = int(timeis)
!        print *,timeisInt
        do i = 1, nr_elements
!            print *,int(hr(i)/2),timeisInt
            if (int(hr(i)/2).eq.timeisInt) then
!                print *,TD(i)
                transmissionPercentage = TD(i)
                
                maespaPAR = PAR(i)
                maespaFBEAM = FBEAM(i)
                maespaSUNLA = SUNLA(i)
                maespaTD = TD(i)
                maespaTSCAT = TSCAT(i)
                maespaTTOT = TTOT(i)
                maespaAPARSUN = APARSUN(i)
                maespaAPARSH = APARSH(i)
                maespaAPAR = APAR(i)
                
                return
                
            endif
        end do
        
        
        ! print reading results 
!        write(*,*) 'number of data lines in the file =', nr_lines
!        write(*,*)
!        write(*,*) 'Array of APAR-values: '
!        call print_array('APAR', APAR, nr_elements)

  
      
      RETURN
20 write(*,*)'I/O error reading file !'  
      END subroutine readTranmissionFromMaespaFiles


!**********************************************************************
subroutine  getLEForSurfacexyz(treeState,x,y,z,f,timeis,yd_actual,maespaLE)    
    use MAINDECLARATIONS
    use MaespaConfigState
    use MaespaConfigStateUtils
      implicit none
      

    INTEGER :: x,y,z,f
      
    INTEGER xtestRev,ytestRev,ztestRev
    INTEGER vegHeight
    INTEGER loopCount
    INTEGER phyFile, strFile, treeFile
    TYPE(maespaConfigTreeMapState) :: treeState     
    !TYPE(maespaConfigvariablesstate) :: config
    real timeis
    INTEGER yd_actual
    real ZENlocal
    integer treeConfigLocation
    real maespaLE   
    REAL transmissionPercentage
    real lai  !!TODO this isn't accessed yet
    real maespaPar,maespaTcan
      
      !! if = street, then might have tree, otherwise return 

    call findTreeFromConfig(x,y,z,treeState,timeis,yd_actual,treeConfigLocation)
    if (treeConfigLocation.eq.-1)then
        !print *,'no tree in ',x,y,z
        maespaLE=0
    else
        call readLEFromMaespaFiles(lai,yd_actual,timeis,treeConfigLocation,transmissionPercentage,maespaLE,maespaPar,maespaTcan)
    endif


   
    end subroutine getLEForSurfacexyz
          
      
!**********************************************************************
    subroutine  readLEFromMaespaFiles(lai,yd_actual,timeis,treeConfigLocation,transmissionPercentage,&
        maespaLE,maespaPar,maespaTcan)
! The transmittance of diffuse radiation through one elementary layer
! in the EHC is taken as the minimum of the diffuse transmittances of
! all grid points. This subroutine finds this minimum transmittance.
!**********************************************************************

      USE maestcom
      IMPLICIT NONE
      INTEGER yd_actual,treeState,treeConfigLocation
      INTEGER timeisInt
      INTEGER IOERROR
      INTEGER HRFLXDAT_FILE 
      INTEGER linesToSkip
      REAL transmissionPercentage,timeis
      real :: dummy
      integer :: n,i, nr_lines, nr_elements, stat
      real, allocatable :: DOY(:),Tree(:),Spec(:),HOUR(:),hrPAR(:),hrNIR(:),hrTHM(:),hrPs(:),hrRf(:),hrRmW(:),hrLE(:),&
                           LECAN(:),Gscan(:),Gbhcan(:),hrH(:),TCAN(:),ALMAX(:),PSIL(:),PSILMIN(:),CI(:),TAIR(:),&
                           VPD(:),PAR(:),ZEN(:),AZ(:)
                                                                       
      real maespaLE   
      real lai
      real maespaPar,maespaTcan
                       
      HRFLXDAT_FILE = 1246
      linesToSkip = 35
      
      !   Columns: DOY Tree Spec HOUR hrPAR hrNIR hrTHM hrPs hrRf hrRmW hrLE LECAN Gscan Gbhcan hrH TCAN ALMAX PSIL PSILMIN CI TAIR VPD PAR ZEN AZ

        !OPEN (UTREES, FILE = trim(in_path)//'trees.dat', STATUS='OLD', IOSTAT=IOERROR)
        OPEN (HRFLXDAT_FILE, FILE = '/home/kerryn/git/MaespaBaseTesting/maespa/TestMaespa32-33/hrflux.dat', STATUS='OLD', &
                        IOSTAT=IOERROR)
        IF (IOERROR.NE.0) THEN
            CALL SUBERROR('ERROR: hrflx.dat DOES NOT EXIST', IFATAL, 0)
        ENDIF
        
        do n=1,linesToSkip
            read(HRFLXDAT_FILE,*) 
        end do
        
        nr_lines = 0
        do
          read(HRFLXDAT_FILE,*,end=10,err=20) dummy
          nr_lines = nr_lines + 1
        end do
        
        ! rewind back to the beginning of the file for unit=1
        10 rewind(HRFLXDAT_FILE)
        do n=1,linesToSkip
            read(HRFLXDAT_FILE,*) 
        end do
        ! number of array elements = number of data lines in the file
        nr_elements=nr_lines
        ! allocate the arrays of given size 
        allocate (DOY(nr_elements) )
        allocate (Tree(nr_elements) )
        allocate (Spec(nr_elements) )
        allocate (HOUR(nr_elements) )
        allocate (hrPAR(nr_elements) )
        allocate (hrNIR(nr_elements) )
        allocate (hrTHM(nr_elements) )
        allocate (hrPs(nr_elements) )
        allocate (hrRf(nr_elements) )
        allocate (hrRmW(nr_elements) )
        allocate (hrLE(nr_elements) )
        allocate (LECAN(nr_elements) )
        allocate (Gscan(nr_elements) )
        allocate (Gbhcan(nr_elements) )
        allocate (hrH(nr_elements) )
        allocate (TCAN(nr_elements) )
        allocate (ALMAX(nr_elements) )
        allocate (PSIL(nr_elements) )
        allocate (PSILMIN(nr_elements) )
        allocate (CI(nr_elements) )
        allocate (TAIR(nr_elements) )
        allocate (VPD(nr_elements) )
        allocate (PAR(nr_elements) )
        allocate (ZEN(nr_elements) )
        allocate (AZ(nr_elements) )
        
        
        ! read array elements form the file
        do i = 1, nr_elements
          read(HRFLXDAT_FILE,*,err=20) DOY(i),Tree(i),Spec(i),HOUR(i),hrPAR(i),hrNIR(i),hrTHM(i),hrPs(i),hrRf(i),hrRmW(i),hrLE(i),&
                           LECAN(i),Gscan(i),Gbhcan(i),hrH(i),TCAN(i),ALMAX(i),PSIL(i),PSILMIN(i),CI(i),TAIR(i),&
                           VPD(i),PAR(i),ZEN(i),AZ(i)
        end do
        close(HRFLXDAT_FILE)
        
        !timeis is 24 hour, hr is 48 1/2 hours
!        print *,timeis
        timeisInt = int(timeis)
!        print *,timeisInt
        do i = 1, nr_elements
!            print *,int(hr(i)/2),timeisInt
            if (int(HOUR(i)/2).eq.timeisInt) then
!                print *,TD(i)
                !! convert from mmol/ms to w/m2
                !maespaLE = hrLE(i)*0.001*44*1000/lai
                maespaLE = hrLE(i)
                maespaPar=PAR(i)
                maespaTcan=TCAN(i)

                
                return
                
            endif
        end do

      RETURN
20 write(*,*)'I/O error reading file !'  
      END subroutine readLEFromMaespaFiles
      
      function convertmmolsecToWm2(mmol,width1,width2,hours)
          real mmol
          real convertmmolsecToWm2
          real hours
          
          ! mmol/sec (which is hrLE from Maespa hrflux)
          ! width1,2 are dimensions of plot (x meters, y meters)
          ! hours - length of time 
          
          ! 1 mole of water = 18.0152g
          ! 1mm H2O = 1kg/m2
          ! 1mm/day = 2.45 MJ m-2 day-1
          ! mmol/sec *  1 mol/1000mmol * 18.0152g/mol * 1kg/1000g * width1 (m) * width2 (m) * 60sec/1min * 60min/1hour * 24hour/day
          !    * 2.45 MJ/m2*day * 10E06J/MJ * 1W/1J * 1day/24hour * 1hour/60min * 1hour/60sec 
          
          !convertmmolsecToWm2 = mmol /1000 * 18.0152 /1000 * width1 * width2 * 60 * 60 * 24 *2.45 * 10E06 /24 / 60 /60 * hours
          convertmmolsecToWm2 = mmol * 18.0152 * width1 * width2 * 2.45 * hours
          !print *,'mmol,convertmmolsecToWm2',mmol,convertmmolsecToWm2
          return 

          
          
      end function convertmmolsecToWm2

      
      function convertMMETToLEWm2(mm,width1,width2,hours)
          real mm
          real convertMMETToLEWm2
          real hours
          
          !! width1,2 are dimensions of plot, x meters, y meters
          !! mm is mm of ET
          !!  18.0152ml/mol of water
          !! heat of vaporization 40.7 KJ/mol
          
          !! time (hours) * mm ET * 1M/1000mm * width1 (m) * width2 (m) 10E+06ml/m^3 * 1 mol/18.0152ml * 40.7 KJ/mol *  1W/1000KJ/sec * 60 sec/1 min * 60 min/hour * hours
          
          convertMMETToLEWm2=hours * mm * width1 * width2 / 18.0152 * 40.7 * 60*60 
          !print *,'convertMMETToLEWm2=mm,le',mm,convertMMETToLEWm2
          return 
          
      end function convertMMETToLEWm2

!**********************************************************************    
      function calculateParWm2FromPar(inPar)
          
          real inPar
          real calculateParWm2FromPar
          real mol,avo,quanta
          
          mol = 10E-06
          avo = 6.02 * 10E+23
          quanta = 4.97 * 10E-19
          
          !! 10**-6 * 10**23 * 10**19
                    
          !lw=(tcan* 10 ** (-6)) * (6.02*10**23) * (4.97*10 ** (-19))
          calculateParWm2FromPar=(tcan* mol) * (avo) * (quanta)
          !calculateParWm2FromPar=inPar * 6.02 * 4.97 * 10**(-6+23-19)
          !print *,calculateParWm2FromPar
          return
          
      end function calculateParWm2FromPar
      
      !**********************************************************************    
      function calculateLWFromTCan(tcan)
          
          real tcan
          real calculateLWFromTCan
          real k
          real sigma 
          
          !! =(R2+273.16)^4*5.67*(10^-8)
          k=(tcan+273.16)
          sigma = 5.67E-08

          calculateLWFromTCan=k**4 * sigma
          return
          
      end function calculateLWFromTCan
      
!!**********************************************************************
!subroutine  getLEForSurfacexyzFromWatBal(treeState,x,y,z,f,timeis,yd_actual,maespaWatQh,maespaWatQe,maespaWatQn,maespaWatQc,maespaLE,maespaPar,maespaTcan,leFromEt,leFromHrLe)    
!    use MAINDECLARATIONS
!    use MaespaConfigState
!    use MaespaConfigStateUtils
!      implicit none
!      
!
!    INTEGER :: x,y,z,f
!      
!    INTEGER xtestRev,ytestRev,ztestRev
!    INTEGER vegHeight
!    INTEGER loopCount
!    INTEGER phyFile, strFile, treeFile
!    TYPE(maespaConfigTreeMapState) :: treeState     
!    !TYPE(maespaConfigvariablesstate) :: config
!    real timeis
!    INTEGER yd_actual
!    real ZENlocal
!    integer treeConfigLocation
!    real maespaWatQh,maespaWatQe,maespaWatQn,maespaWatQc
!    real maespaLE
!    
!    REAL transmissionPercentage
!    real lai
!    real maespaPar,maespaTcan
!    real etInMM
!    real width1,width2,hours
!    real leFromEt
!    real leFromHrLe
!
!      
!      !! if = street, then might have tree, otherwise return 
!
!    call findTreeFromConfig(x,y,z,treeState,timeis,yd_actual,treeConfigLocation)
!    
!    !! TODO for now, just assume plot is 1m x 1m. need to get this value from trees.dat
!    width1=1
!    width2=1
!    hours=0.5
!    !! to get mm ET -> LE, need width1, width2 of plot
!    !! crown radius in the x-direction (metres). &allradx values	= 0.5
!    !! crown radius in the y-direction. &allrady values	= 0.5				
!
!    
!    
!    if (treeConfigLocation.eq.-1)then
!        !print *,'no tree in ',x,y,z
!        maespaWatQh=0
!        maespaWatQe=0
!        maespaWatQn=0
!        maespaWatQc=0
!    else
!        call readLEFromMaespaWatBalFiles(yd_actual,timeis,treeConfigLocation,transmissionPercentage,maespaWatQh,maespaWatQe,maespaWatQn,maespaWatQc,lai,etInMM)
!        leFromEt = convertMMETToLEWm2(etInMM,width1,width2,hours)        
!        call readLEFromMaespaFiles(lai,yd_actual,timeis,treeConfigLocation,transmissionPercentage,maespaLE,maespaPar,maespaTcan)
!        leFromHrLe = convertmmolsecToWm2(maespaLE,width1,width2,hours)
!    endif
!
!
!   
!    end subroutine getLEForSurfacexyzFromWatBal    
    
    
!!**********************************************************************
! subroutine  readLEFromMaespaUSParFiles(yd_actual,timeis,treeConfigLocation,transmissionPercentage,&
!        maespaWatQh,maespaWatQe,maespaWatQn,maespaWatQc,lai,etInMM)
!
!    USE maestcom
!     IMPLICIT NONE
!     INTEGER yd_actual,treeState,treeConfigLocation
!     INTEGER timeisInt
!     INTEGER IOERROR
!     INTEGER USPARDAT_FILE 
!     INTEGER linesToSkip
!     REAL transmissionPercentage,timeis
!     real :: dummy
!     integer :: n,i, nr_lines, nr_elements, stat
!     real, allocatable :: day(:),hour(:),point(:),X(:),Y(:),Z(:),PARbeam(:),PARdiffuse(:),PARtotal(:),APAR(:),hrPSus(:),hrETus(:) 
!     character(len=200) :: usparfilestr    
!     character(len=99) :: treeConfigLocationStr
!
!     real maespaWatQh,maespaWatQe,maespaWatQn,maespaWatQc
!     real lai
!     real etInMM
!       
!     USPARDAT_FILE = 1257
!     linesToSkip = 11
!     
!     write(unit=treeConfigLocationStr,fmt=*) treeConfigLocation
!     usparfilestr = trim(adjustl('./'))//trim(adjustl(treeConfigLocationStr))//trim(adjustl('/uspar.dat'))
!      
!     OPEN (USPARDAT_FILE, FILE = usparfilestr, STATUS='OLD', IOSTAT=IOERROR)
!     IF (IOERROR.NE.0) THEN
!           CALL SUBERROR('ERROR: uspar.dat DOES NOT EXIST', IFATAL, 0)
!     ENDIF
!        
!     do n=1,linesToSkip
!          read(USPARDAT_FILE,*) 
!     end do
!        
!     nr_lines = 0
!     do
!       read(USPARDAT_FILE,*,end=14,err=25) dummy
!       nr_lines = nr_lines + 1
!     end do
!        
!     ! rewind back to the beginning of the file for unit=1
!     14 rewind(USPARDAT_FILE)
!     do n=1,linesToSkip
!         read(USPARDAT_FILE,*) 
!     end do
!     ! number of array elements = number of data lines in the file
!     nr_elements=nr_lines
!     ! allocate the arrays of given size 
!             
!     allocate (day(nr_elements) )
!     allocate (hour(nr_elements) )
!     allocate (point(nr_elements) )
!     allocate (X(nr_elements) )
!     allocate (Y(nr_elements) )
!     allocate (Z(nr_elements) )
!     allocate (PARbeam(nr_elements) )
!     allocate (PARdiffuse(nr_elements) )
!     allocate (PARtotal(nr_elements) )
!     allocate (APAR(nr_elements) )
!     allocate (hrPSus(nr_elements) )
!     allocate (hrETus(nr_elements) )
!       
!     ! read array elements form the file
!     do i = 1, nr_elements
!       read(USPARDAT_FILE,*,err=25) day(i),hour(i),point(i),X(i),Y(i),Z(i),PARbeam(i),PARdiffuse(i),PARtotal(i),APAR(i),hrPSus(i),hrETus(i) 
!     end do
!     close(USPARDAT_FILE)
!        
!     !timeis is 24 hour, hr is 48 1/2 hours
!!        print *,timeis
!     timeisInt = int(timeis)
!!        print *,timeisInt
!     do i = 1, nr_elements
!!            print *,int(hr(i)/2),timeisInt
!         if (int(wathour(i)/2).eq.timeisInt) then
!!                print *,TD(i)
!             maespaWatQh=qh(i)
!             maespaWatQe=qe(i)
!             maespaWatQn=qn(i)
!             maespaWatQc=qc(i)
!             lai=totlai(i)
!             etInMM=et(i)
!             if (etInMM .eq. -999) etInMM=0
!                !print *,'maespaWatQh,maespaWatQe,maespaWatQn,maespaWatQc,lai,etInMM',maespaWatQh,maespaWatQe,maespaWatQn,maespaWatQc,lai,etInMM 
!            return
!               
!         endif
!     end do
!
!   RETURN
!25 write(*,*)'I/O error reading file !'  
!  
!   END subroutine readLEFromMaespaUSParFiles

    
    
    
!!**********************************************************************
!    subroutine  readLEFromMaespaWatBalFiles(yd_actual,timeis,treeConfigLocation,transmissionPercentage,&
!        maespaWatQh,maespaWatQe,maespaWatQn,maespaWatQc,lai,etInMM)
!
!
!
!      USE maestcom
!      IMPLICIT NONE
!      INTEGER yd_actual,treeState,treeConfigLocation
!      INTEGER timeisInt
!      INTEGER IOERROR
!      INTEGER HRFLXDAT_FILE 
!      INTEGER WATBALDAT_FILE 
!      INTEGER linesToSkip
!      REAL transmissionPercentage,timeis
!      real :: dummy
!      integer :: n,i, nr_lines, nr_elements, stat
!      real, allocatable :: DOY(:),Tree(:),Spec(:),HOUR(:),hrPAR(:),hrNIR(:),hrTHM(:),hrPs(:),hrRf(:),hrRmW(:),hrLE(:),&
!                           LECAN(:),Gscan(:),Gbhcan(:),hrH(:),TCAN(:),ALMAX(:),PSIL(:),PSILMIN(:),CI(:),TAIR(:),&
!                           VPD(:),PAR(:),ZEN(:),AZ(:)
!                           
!     real, allocatable::watday(:),wathour(:),wsoil(:),wsoilroot(:),ppt(:),canopystore(:),evapstore(:),drainstore(:),tfall(:),et(:),&
!                           etmeas(:),discharge(:),overflow(:),weightedswp(:),ktot(:),drythick(:),soilevap(:),soilmoist(:),&
!                           fsoil(:),qh(:),qe(:),qn(:),qc(:),rglobund(:),rglobabv(:),radinterc(:),rnet(:),totlai(:),wattair(:),&
!                           soilt1(:),soilt2(:),fracw1(:),fracw2(:),fracaPAR(:)                      
!                           
!      
!      real maespaWatQh,maespaWatQe,maespaWatQn,maespaWatQc
!      real lai
!      real etInMM
!      
!      
!     WATBALDAT_FILE = 1247
!     linesToSkip = 36
!      
!      OPEN (WATBALDAT_FILE, FILE = '/home/kerryn/git/MaespaBaseTesting/maespa/TestMaespa32-33/watbal.dat', STATUS='OLD', &
!                        IOSTAT=IOERROR)
!        IF (IOERROR.NE.0) THEN
!            CALL SUBERROR('ERROR: watbal.dat DOES NOT EXIST', IFATAL, 0)
!        ENDIF
!        
!        do n=1,linesToSkip
!            read(WATBALDAT_FILE,*) 
!        end do
!        
!        nr_lines = 0
!        do
!          read(WATBALDAT_FILE,*,end=11,err=21) dummy
!          nr_lines = nr_lines + 1
!        end do
!        
!        ! rewind back to the beginning of the file for unit=1
!        11 rewind(WATBALDAT_FILE)
!        do n=1,linesToSkip
!            read(WATBALDAT_FILE,*) 
!        end do
!        ! number of array elements = number of data lines in the file
!        nr_elements=nr_lines
!        ! allocate the arrays of given size 
!        allocate (watday(nr_elements) )
!        allocate (wathour(nr_elements) )
!        allocate (wsoil(nr_elements) )
!        allocate (wsoilroot(nr_elements) )
!        allocate (ppt(nr_elements) )
!        allocate (canopystore(nr_elements) )
!        allocate (evapstore(nr_elements) )
!        allocate (drainstore(nr_elements) )
!        allocate (tfall(nr_elements) )
!        allocate (et(nr_elements) )
!        allocate (etmeas(nr_elements) )
!        allocate (discharge(nr_elements) )
!        allocate (overflow(nr_elements) )
!        allocate (weightedswp(nr_elements) )
!        allocate (ktot(nr_elements) )
!        allocate (drythick(nr_elements) )
!        allocate (soilevap(nr_elements) )
!        allocate (soilmoist(nr_elements) )
!        allocate (fsoil(nr_elements) )
!        allocate (qh(nr_elements) )
!        allocate (qe(nr_elements) )
!        allocate (qn(nr_elements) )
!        allocate (qc(nr_elements) )
!        allocate (rglobund(nr_elements) )
!        allocate (rglobabv(nr_elements) )
!        allocate (radinterc(nr_elements) )
!        allocate (rnet(nr_elements) )
!        allocate (totlai(nr_elements) )
!        allocate (wattair(nr_elements) )
!        allocate (soilt1(nr_elements) )
!        allocate (soilt2(nr_elements) )
!        allocate (fracw1(nr_elements) )
!        allocate (fracw2(nr_elements) )
!        allocate (fracaPAR(nr_elements) )
!        
!        
!        ! read array elements form the file
!        do i = 1, nr_elements
!          read(WATBALDAT_FILE,*,err=21) watday(i),wathour(i),wsoil(i),wsoilroot(i),ppt(i),canopystore(i),evapstore(i),drainstore(i),tfall(i),et(i),&
!                           etmeas(i),discharge(i),overflow(i),weightedswp(i),ktot(i),drythick(i),soilevap(i),soilmoist(i),&
!                           fsoil(i),qh(i),qe(i),qn(i),qc(i),rglobund(i),rglobabv(i),radinterc(i),rnet(i),totlai(i),wattair(i),&
!                           soilt1(i),soilt2(i),fracw1(i),fracw2(i),fracaPAR(i) 
!        end do
!        close(WATBALDAT_FILE)
!        
!        !timeis is 24 hour, hr is 48 1/2 hours
!!        print *,timeis
!        timeisInt = int(timeis)
!!        print *,timeisInt
!        do i = 1, nr_elements
!!            print *,int(hr(i)/2),timeisInt
!            if (int(wathour(i)/2).eq.timeisInt) then
!!                print *,TD(i)
!                maespaWatQh=qh(i)
!                maespaWatQe=qe(i)
!                maespaWatQn=qn(i)
!                maespaWatQc=qc(i)
!                lai=totlai(i)
!                etInMM=et(i)
!                if (etInMM .eq. -999) etInMM=0
!                !print *,'maespaWatQh,maespaWatQe,maespaWatQn,maespaWatQc,lai,etInMM',maespaWatQh,maespaWatQe,maespaWatQn,maespaWatQc,lai,etInMM
!                
!
!                
!               return
!                
!            endif
!        end do
!
!      RETURN
!21 write(*,*)'I/O error reading file !'  
!  
!      END subroutine readLEFromMaespaWatBalFiles
      
      subroutine readMaespaDataFiles(treeMapFromConfig,maespaDataArray,treeXYMap,treeXYTreeMap)
          use MaespaConfigState, only : maespaConfigTreeMapState,maespaDataResults,maespaArrayOfDataResults
!          use netcdf
          implicit none
          
        TYPE(maespaConfigTreeMapState) :: treeMapFromConfig
        TYPE(maespaArrayOfDataResults),allocatable,dimension(:) :: maespaDataArray
        integer,allocatable,dimension(:,:) :: treeXYMap !! this one is the xy of the tree configs
        integer,allocatable,dimension(:,:) :: treeXYTreeMap !! this one is the xy of the actual tree (only one tree per 1 or more grid squares)
        integer loopCount,timeCount,loopCount2
        integer x,y
        integer,allocatable,dimension(:) :: treeLocationMap !! only load each tree config once
        logical dataLoaded
        integer numberOfTreePlots,width,length
        real gridSize
        integer treeFilesNumber
        integer treeNumber
        
!        integer, parameter :: NDIMS = 2
!        integer :: NX , NY 
!        character (len = *), parameter :: FILE_NAME = "treeXYMap.nc"
!        character (len = *), parameter :: FILE_NAME2 = "maespaDataArray.nc"
!        integer :: status
!        logical :: foundTreeXYMap
!        logical :: foundMaespaDataArray
!
!        ! When we create netCDF files, variables and dimensions, we get back
!        ! an ID for each one.
!        integer :: ncid, varid, dimids(NDIMS)
!        integer :: x_dimid, y_dimid

!        foundTreeXYMap = .FALSE.
!        foundMaespaDataArray = .FALSE.

!        NX=treeMapFromConfig%width
!        NY=treeMapFromConfig%length
          
!        print *,'treeMapFromConfig%numberTreePlots',treeMapFromConfig%numberTreePlots
!        print *,'width,length',treeMapFromConfig%width,treeMapFromConfig%length
!        print *,'xlocation,ylocation',treeMapFromConfig%xlocation,treeMapFromConfig%ylocation
!        print *,' '
!        print *,'treesfileNumber',treeMapFromConfig%treesfileNumber
        
        numberOfTreePlots = treeMapFromConfig%numberTreePlots
        width = treeMapFromConfig%width
        length = treeMapFromConfig%length
        gridSize = treeMapFromConfig%configTreeMapGridSize
        
        allocate (maespaDataArray(numberOfTreePlots) )    
        allocate (treeLocationMap(numberOfTreePlots))
        allocate (treeXYMap(width,length))
        allocate (treeXYTreeMap(width,length))
        treeXYMap =0
        treeXYTreeMap =0
        treeLocationMap=0
                
!         ! Open the file. NF90_NOWRITE tells netCDF we want read-only access to
!         ! the file.
!         status = nf90_open(FILE_NAME, NF90_NOWRITE, ncid)
!         !print *,'1 status',status
!         if (status .eq. 0 ) then             
!             ! Get the varid of the data variable, based on its name.
!             status = nf90_inq_varid(ncid, "treeXYMap", varid) 
!             !print *,'2 status',status
!             if (status .eq. 0 ) then
!                 ! Read the data.
!                 status = nf90_get_var(ncid, varid, treeXYMap)
!                 !print *,'3 status',status
!                 if (status .eq. 0 ) then
!                     foundTreeXYMap = .TRUE.
!                 endif
!             endif             
!         endif
        
         !print *,'first treeXYMap',treeXYMap
        
        
        print *,'numberOfTreePlots',numberOfTreePlots
        do loopCount= 1,numberOfTreePlots
!            if (foundTreeXYMap .neqv. .TRUE.) then
                x=treeMapFromConfig%xlocation(loopCount)+1
                y=treeMapFromConfig%ylocation(loopCount)+1
                treeFilesNumber=treeMapFromConfig%treesfileNumber(loopCount)
                treeNumber=treeMapFromConfig%trees(loopCount)
                treeXYMap(x,y)=treeFilesNumber  
                treeXYTreeMap(x,y)=treeNumber
!            endif
!            print *,'loopCount,treeMapFromConfig%treesfileNumber(loopCount)',loopCount,treeMapFromConfig%treesfileNumber(loopCount) 
            
            dataLoaded = .FALSE.
            
            if (loopCount .gt. 1) then            
              do 999 loopCount2=1,loopCount
!                 print *,'loopCount2',loopCount2
!                 print *,'treeLocationMap(loopCount2)',treeLocationMap(loopCount2)
                 treeFilesNumber=treeMapFromConfig%treesfileNumber(loopCount)
                 if ( treeFilesNumber .eq. treeLocationMap(loopCount2) ) then
                     !! copy 
!                     print *,'copy ',treeLocationMap(loopCount2) 
                     treeLocationMap(loopCount)=treeLocationMap(loopCount2)
                     maespaDataArray(loopCount)%maespaOverallDataArray = maespaDataArray(loopCount2)%maespaOverallDataArray
                     dataLoaded = .TRUE.
                     !! copied now, so stop looking
                     exit
!                 else
!                     !! load
!                     print *,'load ',treeMapFromConfig%treesfileNumber(loopCount),loopCount
!                     treeLocationMap(loopCount)=treeMapFromConfig%treesfileNumber(loopCount)leFromUspar
!                     call readMaespaHRWatDataFiles(treeMapFromConfig%treesfileNumber(loopCount),&
!                        maespaDataArray(loopCount)%maespaOverallDataArray,treeMapFromConfig%configTreeMapGridSize)
                 endif
              999 continue
            else
                treeFilesNumber=treeMapFromConfig%treesfileNumber(loopCount)
!                print *,'load2 ',treeMapFromConfig%treesfileNumber(loopCount),loopCount
                treeLocationMap(loopCount)=treeFilesNumber
                call readMaespaHRWatDataFiles(treeFilesNumber,&
                    maespaDataArray(loopCount)%maespaOverallDataArray,gridSize)
                dataLoaded = .TRUE.
            endif 
            
            !! ok, didn't find it, load next item
            if (dataLoaded .eqv. .FALSE.) then
                 treeFilesNumber=treeMapFromConfig%treesfileNumber(loopCount)
!                 print *,'load ',treeMapFromConfig%treesfileNumber(loopCount),loopCount
                 treeLocationMap(loopCount)=treeFilesNumber
                 call readMaespaHRWatDataFiles(treeFilesNumber,&
                        maespaDataArray(loopCount)%maespaOverallDataArray,gridSize)
            endif

            
!            print *,'before readMaespaHRWatDataFiles'
!            treeLocationMap(loopCount)=treeMapFromConfig%treesfileNumber(loopCount)
!            call readMaespaHRWatDataFiles(treeMapFromConfig%treesfileNumber(loopCount),&
!                    maespaDataArray(loopCount)%maespaOverallDataArray,treeMapFromConfig%configTreeMapGridSize)
            !print *,maespaData
                    
            
            
        end do
        
        
        !print *,'second treeXYMap',treeXYMap
        
!    if (foundMaespaDataArray .neqv. .TRUE.) then
!        status = nf90_create(FILE_NAME2, NF90_CLOBBER, ncid)
!        status = nf90_def_dim(ncid, "x", treeMapFromConfig%numberTreePlots, x_dimid)
!        dimids =  (/ x_dimid /)
!        status = nf90_def_var(ncid, "maespaDataArray", NF90_INT, dimids, varid)
!        status = nf90_enddef(ncid)
!        status = nf90_put_var(ncid, varid, maespaDataArray)
!        status = nf90_close(ncid) 
!    endif
        
        
!    if (foundTreeXYMap .neqv. .TRUE.) then      
!    !! write out treeXYMap to netcdf to read in faster in future runs
!      ! Create the netCDF file. The nf90_clobber parameter tells netCDF to
!      ! overwrite this file, if it already exists.
!      status = nf90_create(FILE_NAME, NF90_CLOBBER, ncid)
!      ! Define the dimensions. NetCDF will hand back an ID for each. 
!      status = nf90_def_dim(ncid, "x", NX, x_dimid)
!      status = nf90_def_dim(ncid, "y", NY, y_dimid)
!      ! The dimids array is used to pass the IDs of the dimensions of
!      ! the variables. Note that in fortran arrays are stored in
!      ! column-major format.
!      dimids =  (/ y_dimid, x_dimid /)
!      ! Define the variable. The type of the variable in this case is
!      ! NF90_INT (4-byte integer).
!      status = nf90_def_var(ncid, "treeXYMap", NF90_INT, dimids, varid)
!      ! End define mode. This tells netCDF we are done defining metadata.
!      status = nf90_enddef(ncid)
!      ! Write the pretend data to the file. Although netCDF supports
!      ! reading and writing subsets of data, in this case we write all the
!      ! data in one operation.
!      status = nf90_put_var(ncid, varid, treeXYMap)
!      ! Close the file. This frees up any internal netCDF resources
!      ! associated with the file, and flushes any buffers.
!      status = nf90_close(ncid) 
!    endif
        
      end subroutine readMaespaDataFiles
      
  
      
!   subroutine check(status)
!    integer, intent ( in) :: status
!    
!    return
!!    if(status /= nf90_noerr) then 
!!      print *, nf90_strerror(status)
!!        print *,'Stopped in check(status)',status 
!!      stop 'Stopped in check(status)'
!!    end if
!  end subroutine check       
 
!**********************************************************************
    subroutine  readMaespaHRWatDataFiles(treeConfigLocation,maespaData,width1)

      USE maestcom
      use MaespaConfigState, only : maespaConfigTreeMapState,maespaDataResults
      IMPLICIT NONE
      INTEGER treeState,treeConfigLocation
      INTEGER IOERROR
      INTEGER HRFLXDAT_FILE 
      INTEGER WATBALDAT_FILE 
      INTEGER USPARDAT_FILE
      INTEGER linesToSkip
      INTEGER hrlinesToSkip
      INTEGER usparlinesToSkip
      REAL timeis
      real :: dummy
      integer :: n,i, nr_lines, nr_elements
      integer :: hr_nr_lines,uspar_nr_lines
      TYPE(maespaDataResults),allocatable,dimension(:) :: maespaData
      real width1,width2,hours
      character(len=99) :: treeConfigLocationStr
      character(len=200) :: watbalfilestr
      character(len=200) :: hrflxfilestr
      character(len=200) :: usparfilestr  
      logical loadUspar
      real area

      loadUspar = .TRUE.
      !!TODO hardcoded for now
      width2=width1
!      width1=1.0
!      width2=1.0
      hours=0.5
      area = width1*width2
                       
     HRFLXDAT_FILE = 1246
     hrlinesToSkip = 35 
     WATBALDAT_FILE = 1247
     linesToSkip = 37
     USPARDAT_FILE = 1257
     usparlinesToSkip = 11
     
     
     write(unit=treeConfigLocationStr,fmt=*) treeConfigLocation
     watbalfilestr = trim(adjustl('./'))//trim(adjustl(treeConfigLocationStr))//trim(adjustl('/watbal.dat'))

     !write(unit=treeConfigLocationStr,fmt=*) treeConfigLocation
     hrflxfilestr = trim(adjustl('./'))//trim(adjustl(treeConfigLocationStr))//trim(adjustl('/hrflux.dat'))
     
     !write(unit=treeConfigLocationStr,fmt=*) treeConfigLocation
     usparfilestr = trim(adjustl('./'))//trim(adjustl(treeConfigLocationStr))//trim(adjustl('/uspar.dat'))
     print *,usparfilestr
  
        !OPEN (WATBALDAT_FILE, FILE = '/home/kerryn/git/MaespaBaseTesting/maespa/TestMaespa32-33/watbal.dat', STATUS='OLD', IOSTAT=IOERROR)
        OPEN (WATBALDAT_FILE, FILE = watbalfilestr, STATUS='OLD', IOSTAT=IOERROR)
        IF (IOERROR.NE.0) THEN
            CALL SUBERROR('ERROR: watbal.dat DOES NOT EXIST', IFATAL, 0)
        ENDIF
        
        !OPEN (HRFLXDAT_FILE, FILE = '/home/kerryn/git/MaespaBaseTesting/maespa/TestMaespa32-33/hrflux.dat', STATUS='OLD', IOSTAT=IOERROR)
        OPEN (HRFLXDAT_FILE, FILE = hrflxfilestr, STATUS='OLD', IOSTAT=IOERROR)
        IF (IOERROR.NE.0) THEN
            CALL SUBERROR('ERROR: hrflx.dat DOES NOT EXIST', IFATAL, 0)
        ENDIF
        
        OPEN (USPARDAT_FILE, FILE = usparfilestr, STATUS='OLD', IOSTAT=IOERROR)
        IF (IOERROR.NE.0) THEN
           !CALL SUBERROR('ERROR: uspar.dat DOES NOT EXIST', IFATAL, 0)
            loadUspar = .FALSE.
        ENDIF
        
        do n=1,linesToSkip
            read(WATBALDAT_FILE,*) 
        end do
        
        do n=1,hrlinesToSkip
            read(HRFLXDAT_FILE,*) 
        end do
        
        nr_lines = 0
        do
          read(WATBALDAT_FILE,*,end=11,err=23) dummy
          nr_lines = nr_lines + 1
        end do
        
        ! rewind back to the beginning of the file for unit=1
        11 rewind(WATBALDAT_FILE)
        do n=1,linesToSkip
            read(WATBALDAT_FILE,*) 
        end do
        
        !        print *,'WATBALDAT_FILE lines',nr_lines
        
        hr_nr_lines = 0
        do
          read(HRFLXDAT_FILE,*,end=10,err=23) dummy
          hr_nr_lines = hr_nr_lines + 1
        end do
  
        ! rewind back to the beginning of the file for unit=1
        10 rewind(HRFLXDAT_FILE)
        do n=1,hrlinesToSkip
            read(HRFLXDAT_FILE,*) 
        end do
        
        
        if (loadUspar .eqv. .TRUE.) then
            do n=1,usparlinesToSkip
              read(USPARDAT_FILE,*) 
            end do

            uspar_nr_lines = 0
            do
             read(USPARDAT_FILE,*,end=14,err=23) dummy
             uspar_nr_lines = uspar_nr_lines + 1
            end do

            ! rewind back to the beginning of the file for unit=1
            14 rewind(USPARDAT_FILE)
            do n=1,usparlinesToSkip
             read(USPARDAT_FILE,*) 
            end do
        endif 
        
        
        ! number of array elements = number of data lines in the file
        !nr_elements=nr_lines
        
!        print *,'HRFLXDAT_FILE lines',hr_nr_lines
        
        ! number of array elements = number of data lines in the file
        nr_elements=nr_lines
     
        allocate (maespaData(nr_elements) )
        
        
        ! read array elements form the file
!        do i = 1, nr_elements
!          read(WATBALDAT_FILE,*,err=23) watday(i),wathour(i),wsoil(i),wsoilroot(i),ppt(i),canopystore(i),evapstore(i),drainstore(i),tfall(i),et(i),&
!                           etmeas(i),discharge(i),overflow(i),weightedswp(i),ktot(i),drythick(i),soilevap(i),soilmoist(i),&
!                           fsoil(i),qh(i),qe(i),qn(i),qc(i),rglobund(i),rglobabv(i),radinterc(i),rnet(i),totlai(i),wattair(i),&
!                           soilt1(i),soilt2(i),fracw1(i),fracw2(i),fracaPAR(i) 
!        end do
!        close(WATBALDAT_FILE)
        
        do i = 1, nr_elements
          read(WATBALDAT_FILE,*,err=23) maespaData(i)%watday,maespaData(i)%wathour,maespaData(i)%wsoil,maespaData(i)%wsoilroot,&
                           maespaData(i)%ppt,maespaData(i)%canopystore,maespaData(i)%evapstore,maespaData(i)%drainstore,&
                           maespaData(i)%tfall,maespaData(i)%et,maespaData(i)%etmeas,maespaData(i)%discharge,&
                           maespaData(i)%overflow,maespaData(i)%weightedswp,maespaData(i)%ktot,maespaData(i)%drythick,&
                           maespaData(i)%soilevap,maespaData(i)%soilmoist,maespaData(i)%fsoil,maespaData(i)%qh,&
                           maespaData(i)%qe,maespaData(i)%qn,maespaData(i)%qc,maespaData(i)%rglobund,maespaData(i)%rglobabv,&
                           maespaData(i)%radinterc,maespaData(i)%rnet,maespaData(i)%totlai,maespaData(i)%wattair,&
                           maespaData(i)%soilt1,maespaData(i)%soilt2,maespaData(i)%fracw1,maespaData(i)%fracw2,&
                           maespaData(i)%fracaPAR
           read(HRFLXDAT_FILE,*,err=24) maespaData(i)%DOY,maespaData(i)%Tree,maespaData(i)%Spec,maespaData(i)%HOUR,&
                           maespaData(i)%hrPAR,maespaData(i)%hrNIR,maespaData(i)%hrTHM,maespaData(i)%hrPs,maespaData(i)%hrRf,&
                           maespaData(i)%hrRmW,maespaData(i)%hrLE,maespaData(i)%LECAN,maespaData(i)%Gscan,maespaData(i)%Gbhcan,&
                           maespaData(i)%hrH,maespaData(i)%TCAN,maespaData(i)%ALMAX,maespaData(i)%PSIL,maespaData(i)%PSILMIN,&
                           maespaData(i)%CI,maespaData(i)%TAIR,maespaData(i)%VPD,maespaData(i)%PAR,maespaData(i)%ZEN,&
                           maespaData(i)%AZ
           
           if (loadUspar .eqv. .TRUE.) then
                read(USPARDAT_FILE,*,err=25) maespaData(i)%usparday,maespaData(i)%usparhour,maespaData(i)%usparpoint,&
                               maespaData(i)%usparX,maespaData(i)%usparY,maespaData(i)%usparZ,maespaData(i)%usparPARbeam,&
                               maespaData(i)%usparPARdiffuse,maespaData(i)%usparPARtotal,maespaData(i)%usparAPAR,&
                               maespaData(i)%usparhrPSus,maespaData(i)%usparhrETus
           endif
           
           
!! new source of Maespa fluxes

! watbalQe<-hours * data_tableWatbal$et  / 18.0152 * 40.7 * 60*60 /area/area
! qc<- zoo(data_tableWatbal$qc, datestimesWatbal )
! rnet<- zoo(data_tableWatbal$rnet, datestimesWatbal )
! qewatbal <- zoo(watbalQe, datestimesWatbal )
! canopystore<- zoo(hours*data_tableWatbal$canopystore/18.0152*40.7*60*60/area/area, datestimesWatbal )
! evapstore<- zoo(hours*data_tableWatbal$evapstore/18.0152*40.7*60*60/area/area, datestimesWatbal )
! qeTotal <-qewatbal+canopystore+evapstore
! qHRes <- rnet  - qc -qeTotal
! balance<- rnet  - qc - qeTotal - qHRes
                
                           
                           
           maespaData(i)%leFromEt = convertMMETToLEWm2(maespaData(i)%et,width1,width2,hours)/area/area            
           maespaData(i)%leFromHrLe = convertmmolsecToWm2(maespaData(i)%hrLE,width1,width2,hours)/area/area
           
           !! new change, qe is now total from watbal of et, canopystore, evapstore
           maespaData(i)%qeCalc = maespaData(i)%leFromEt + convertMMETToLEWm2(maespaData(i)%canopystore,width1,width2,hours)/area/area + convertMMETToLEWm2(maespaData(i)%evapstore,width1,width2,hours)/area/area  
           !! Qg is maespaData(i)%qc
           !! rnet is maespaData(i)%rnet
           !! Qh is residual from the above values
           maespaData(i)%qhCalc = maespaData(i)%rnet - maespaData(i)%qc - maespaData(i)%qeCalc
           
           !! qe = maespaData(i)%qeCalc
           !! qg = maespaData(i)%qc
           !! rnet = maespaData(i)%rnet
           !! qh = maespaData(i)%qhCalc
                                      
           
           if (loadUspar .eqv. .TRUE.) then
                maespaData(i)%leFromUspar = convertmmolsecToWm2(maespaData(i)%usparhrETus,width1,width2,hours)/area/area
!                print *,'i,maespaData(i)%leFromUspar',i,maespaData(i)%leFromUspar
           else
               maespaData(i)%leFromUspar = -0
           endif
           
!        print *,'i,maespaData(i)%leFromEt',i,maespaData(i)%leFromEt
                           
                           
        end do
        close(WATBALDAT_FILE)
        close(HRFLXDAT_FILE)
        if (loadUspar .eqv. .TRUE.) then
            close(USPARDAT_FILE)
        endif
        

      RETURN
      23 write(*,*)'I/O error reading file !'  ,watbalfilestr, i
      
      24 write(*,*)'I/O error reading file !'  ,hrflxfilestr, i
      
      25 write(*,*)'I/O error reading file !'  ,usparfilestr, i
  
      END subroutine readMaespaHRWatDataFiles
      
      
      
      
    subroutine readMaespaTestData(treeMapFromConfig,maespaTestDataArray,treeXYMap)
          use MaespaConfigState, only : maespaConfigTreeMapState,maesapaTestDataResults,maespaArrayOfTestDataResults
          implicit none
          
        TYPE(maespaConfigTreeMapState) :: treeMapFromConfig  
        TYPE(maespaArrayOfTestDataResults),allocatable,dimension(:) :: maespaTestDataArray
        integer,allocatable,dimension(:,:) :: treeXYMap
        integer loopCount,timeCount
        integer x,y
        INTEGER NOPOINTS,INPUTTYPE
        NAMELIST /CONTROL/ NOPOINTS,INPUTTYPE
        integer IFATAL
        integer IOERROR
        integer :: nr_points
        integer POINTSDAT_FILE    
        integer treeConfigLocation
        character(len=99) ::  treeConfigLocationStr
        character(len=200) :: pointsfilestr
          
!        print *,'treeMapFromConfig%numberTreePlots',treeMapFromConfig%numberTreePlots
!        print *,'width,length',treeMapFromConfig%width,treeMapFromConfig%length
!        print *,'xlocation,ylocation',treeMapFromConfig%xlocation,treeMapFromConfig%ylocation
!        print *,' '
!        print *,'treesfileNumber',treeMapFromConfig%treesfileNumber
        
        ifatal = 100
        POINTSDAT_FILE = 1250
        
        treeConfigLocation=1       
        write(unit=treeConfigLocationStr,fmt=*) treeConfigLocation
        pointsfilestr = trim(adjustl('./'))//trim(adjustl(treeConfigLocationStr))//trim(adjustl('/points.dat'))
        !print *,pointsfilestr
        
        !OPEN (POINTSDAT_FILE, FILE = '/home/kerryn/git/MaespaBaseTesting/maespa/TestMaespa32-33/points.dat', STATUS='OLD', IOSTAT=IOERROR)
        OPEN (POINTSDAT_FILE, FILE = pointsfilestr, STATUS='OLD', IOSTAT=IOERROR)                                                                                          
        IF (IOERROR.NE.0) THEN
            CALL SUBERROR('ERROR: points.dat DOES NOT EXIST', IFATAL, 0)
        ENDIF
        
        REWIND(POINTSDAT_FILE)
        
      READ (POINTSDAT_FILE, CONTROL, IOSTAT = IOERROR)
      IF ((IOERROR.NE.0).OR.(NOPOINTS.EQ.0)) &
        CALL SUBERROR('ERROR: MISSING CONTROL INFO IN POINTS FILE', &
        IFATAL,IOERROR)
        
        !print *,NOPOINTS
        nr_points = NOPOINTS
        
        allocate(maespaTestDataArray(treeMapFromConfig%numberTreePlots))
        
        
        do loopCount= 1,treeMapFromConfig%numberTreePlots  
            call readMaespaTestDataFiles(nr_points,treeMapFromConfig%treesfileNumber(loopCount),maespaTestDataArray(loopCount)%maespaOverallTestDataArray)
            !print *,maespaData
        end do
        
       
                
      end subroutine readMaespaTestData 
      
     subroutine  readMaespaTestDataFiles(nr_points,treeConfigLocation,maespaData)

      USE maestcom
      use MaespaConfigState, only : maespaConfigTreeMapState,maesapaTestDataResults
      IMPLICIT NONE
      INTEGER treeState,treeConfigLocation
      INTEGER IOERROR
      INTEGER TESTFLXDAT_FILE
      INTEGER linesToSkip
      INTEGER hrlinesToSkip
      REAL timeis
      real :: dummy1, dummy2, dummy3, dummy4, dummy5, dummy6, dummy7, dummy8, dummy9, dummy10, dummy11, dummy12, dummy13, dummy14, dummy15
      integer :: n,i, nr_lines, nr_elements
      integer :: hr_nr_lines
      integer nr_points
      character(len=99) ::  treeConfigLocationStr
      character(len=200) :: testflxfilestr
      
      TYPE(maesapaTestDataResults),allocatable,dimension(:) :: maespaData
      
      write(unit=treeConfigLocationStr,fmt=*) treeConfigLocation
      testflxfilestr = trim(adjustl('./'))//trim(adjustl(treeConfigLocationStr))//trim(adjustl('/testflx.dat'))
!      print *,testflxfilestr                
 
        TESTFLXDAT_FILE = 1249
     
        linesToSkip = 21 
         
        !OPEN (TESTFLXDAT_FILE, FILE = '/home/kerryn/git/MaespaBaseTesting/maespa/TestMaespa32-33/testflx.dat', STATUS='OLD', IOSTAT=IOERROR)
        OPEN (TESTFLXDAT_FILE, FILE = testflxfilestr, STATUS='OLD', IOSTAT=IOERROR)
        IF (IOERROR.NE.0) THEN
            CALL SUBERROR('ERROR: textflx.dat DOES NOT EXIST', IFATAL, 0)
        ENDIF
        
        do n=1,linesToSkip
            read(TESTFLXDAT_FILE,*) 
        end do
        
        nr_lines = 0
        do
          read(TESTFLXDAT_FILE,*,end=13,err=24) dummy1, dummy2, dummy3, dummy4, dummy5, dummy6, dummy7, dummy8, dummy9, dummy10, dummy11, dummy12, dummy13, dummy14, dummy15
          !print *,dummy1, dummy2, dummy3, dummy4, dummy5, dummy6, dummy7, dummy8, dummy9, dummy10, dummy11, dummy12, dummy13, dummy14, dummy15
          nr_lines = nr_lines + 1
        end do
        
        13 rewind(TESTFLXDAT_FILE)
        do n=1,linesToSkip
            read(TESTFLXDAT_FILE,*) 
        end do
        
        allocate (maespaData(nr_lines))
        ! rewind back to the beginning of the file for unit=1
        
        do i = 1, nr_lines
            read(TESTFLXDAT_FILE,*,end=15,err=24) dummy1, dummy2, dummy3, dummy4, dummy5, dummy6, dummy7, dummy8, dummy9, dummy10, dummy11, dummy12, dummy13, dummy14, dummy15
!          read(TESTFLXDAT_FILE,*,end=15,err=24) maespaData(i)%testDAY,maespaData(i)%testHR,maespaData(i)%testPT,maespaData(i)%testX,&
!                           maespaData(i)%testY,maespaData(i)%testZ,maespaData(i)%testPAR,maespaData(i)%testFBEAM,&
!                           maespaData(i)%testSUNLA,maespaData(i)%testTD,maespaData(i)%testTSCAT,maespaData(i)%testTTOT ,&
!                           maespaData(i)%testAPARSUN,maespaData(i)%testAPARSH,maespaData(i)%testAPAR     
            maespaData(i)%testDAY=dummy1
            maespaData(i)%testHR=dummy2
            maespaData(i)%testPT=dummy3
            maespaData(i)%testX=dummy4
            maespaData(i)%testY=dummy5
            maespaData(i)%testZ=dummy6
            maespaData(i)%testPAR=dummy7
            maespaData(i)%testFBEAM=dummy8
            maespaData(i)%testSUNLA=dummy9
            maespaData(i)%testTD=dummy10
            maespaData(i)%testTSCAT=dummy11
            maespaData(i)%testTTOT=dummy12
            maespaData(i)%testAPARSUN=dummy13
            maespaData(i)%testAPARSH=dummy14
            maespaData(i)%testAPAR=dummy15
          !print *,maespaData(i)%testDAY,maespaData(i)%testHR,maespaData(i)%testPT
        end do
        15 close(TESTFLXDAT_FILE)

      RETURN
      24 write(*,*)'I/O error reading file !'  
  
     END subroutine readMaespaTestDataFiles     
      
      
      ! ************** Functions/Procedures **************
    subroutine print_array(array_name, array, n)
      implicit none
      integer :: n, i
      real :: array(n)
      character :: array_name

      do i = 1, n
        write (*,*) array_name, '[', i, '] = ', array(i)
      end do
    end subroutine print_array
    
    
    function getDataForTimeAndDayAndPoint(treeLocation,day,hour,point,dataItem)
      use MaespaConfigState, only : maespaConfigTreeMapState,maesapaTestDataResults,maespaArrayOfTestDataResults  
      use TUFConstants
      use Dyn_Array, only: maespaDataArray,maespaTestDataArray,treeXYMap
      
        real getDataForTimeAndDayAndPoint          
        TYPE(maespaConfigTreeMapState) :: treeMapFromConfig  
        !TYPE(maespaArrayOfTestDataResults),allocatable,dimension(:) :: maespaTestDataArray
        integer loopCount
        integer arraySize
        real testDAY
        real testHR
        real testPT
        
        integer treeLocation
        real day
        real hour
        real point
        integer dataItem

        if (treeLocation.eq.0)then
            print *,'location=0 in getDataForTimeAndDayAndPoint'
            getDataForTimeAndDayAndPoint=0
            return
        endif
        
        
        arraySize = size( maespaTestDataArray(treeLocation)%maespaOverallTestDataArray )
        
        !maespaTestDataArray(1)%maespaOverallTestDataArray(1)%testTD
        do loopCount= 1,arraySize
            !print *,maespaTestDataArray(treeLocation)%maespaOverallTestDataArray(loopCount)%testDAY
            !print *,maespaTestDataArray(treeLocation)%maespaOverallTestDataArray(loopCount)
            
            testDAY=maespaTestDataArray(treeLocation)%maespaOverallTestDataArray(loopCount)%testDAY
            testHR=maespaTestDataArray(treeLocation)%maespaOverallTestDataArray(loopCount)%testHR
            testPT=maespaTestDataArray(treeLocation)%maespaOverallTestDataArray(loopCount)%testPT
            
            if (testDAY.eq.day .and. testHR.eq.hour .and. testPT.eq.point) then
                if (dataItem.eq.testDAYCONST) then
                    getDataForTimeAndDayAndPoint = maespaTestDataArray(treeLocation)%maespaOverallTestDataArray(loopCount)%testDAY
                endif
                if (dataItem.eq.testHRCONST) then
                    getDataForTimeAndDayAndPoint = maespaTestDataArray(treeLocation)%maespaOverallTestDataArray(loopCount)%testHR
                endif
                if (dataItem.eq.testPTCONST) then
                    getDataForTimeAndDayAndPoint = maespaTestDataArray(treeLocation)%maespaOverallTestDataArray(loopCount)%testPT
                endif                
                 if (dataItem.eq.testXCONST) then
                    getDataForTimeAndDayAndPoint = maespaTestDataArray(treeLocation)%maespaOverallTestDataArray(loopCount)%testX
                endif               
                 if (dataItem.eq.testYCONST) then
                    getDataForTimeAndDayAndPoint = maespaTestDataArray(treeLocation)%maespaOverallTestDataArray(loopCount)%testY
                endif
                 if (dataItem.eq.testZCONST) then
                    getDataForTimeAndDayAndPoint = maespaTestDataArray(treeLocation)%maespaOverallTestDataArray(loopCount)%testZ
                endif               
                if (dataItem.eq.testPARCONST) then
                    getDataForTimeAndDayAndPoint = maespaTestDataArray(treeLocation)%maespaOverallTestDataArray(loopCount)%testPAR
                endif                
                 if (dataItem.eq.testFBEAMCONST) then
                    getDataForTimeAndDayAndPoint = maespaTestDataArray(treeLocation)%maespaOverallTestDataArray(loopCount)%testFBEAM
                endif               
                 if (dataItem.eq.testSUNLACONST) then
                    getDataForTimeAndDayAndPoint = maespaTestDataArray(treeLocation)%maespaOverallTestDataArray(loopCount)%testSUNLA
                endif
                 if (dataItem.eq.testTDCONST) then
                    getDataForTimeAndDayAndPoint = maespaTestDataArray(treeLocation)%maespaOverallTestDataArray(loopCount)%testTD
                endif               
                 if (dataItem.eq.testTSCATCONST) then
                    getDataForTimeAndDayAndPoint = maespaTestDataArray(treeLocation)%maespaOverallTestDataArray(loopCount)%testTSCAT
                endif               
                 if (dataItem.eq.testTTOTCONST) then
                    getDataForTimeAndDayAndPoint = maespaTestDataArray(treeLocation)%maespaOverallTestDataArray(loopCount)%testTTOT
                endif               
                 if (dataItem.eq.testAPARSUNCONST) then
                    getDataForTimeAndDayAndPoint = maespaTestDataArray(treeLocation)%maespaOverallTestDataArray(loopCount)%testAPARSUN
                endif               
                 if (dataItem.eq.testAPARSHCONST) then
                    getDataForTimeAndDayAndPoint = maespaTestDataArray(treeLocation)%maespaOverallTestDataArray(loopCount)%testAPARSH
                endif               
                 if (dataItem.eq.testAPARCONST) then
                    getDataForTimeAndDayAndPoint = maespaTestDataArray(treeLocation)%maespaOverallTestDataArray(loopCount)%testAPAR
                endif               
            endif
          
        end do
        
        return
        
    end function getDataForTimeAndDayAndPoint

END MODULE ReadMaespaConfigs
