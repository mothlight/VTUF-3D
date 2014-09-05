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
    
    INTEGER phyFileNumber, strFileNumber, treeFileNumber
    TYPE(maespaConfigvariablesstate), intent(OUT) :: config
    
    suffix = '.dat'
    
    phyFileName = trim(constructFilename('phy', phyFileNumber, suffix))
    strFileName = trim(constructFilename('str', strFileNumber, suffix))
    treeFileName = trim(constructFilename('trees', treeFileNumber, suffix))
    
!    print *,phyFileName,strFileName,treeFileName
    
    ! Input file with data on tree position and size
    OPEN (UTREES, FILE = treeFileName, STATUS='OLD', IOSTAT=IOERROR)
    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('ERROR: TREES.DAT DOES NOT EXIST', IFATAL, 0)
    ENDIF
    
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

 end subroutine readMaespaTreeConfigFromConfig
    
    
subroutine readMaespaTreeMap(state, treeStates)
    use MAINDECLARATIONS
    use MaespaConfigState
    use MaespaConfigStateUtils
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
    
   subroutine findTreeFromConfig(xtestRev,ytestRev,ztestRev,treeState,timeis,yd_actual)
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
    real ZENlocal
    
    !integer TIME,IDOY,KHRS
    !real ALAT,BEARlocal,DEClocal,ZENlocal,AZlocal
    
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
    
    print *,'number of tree plots', treeState%numberTreePlots
    vegHeight = 0 !if the tree location isn't found, then it will be 0 high
    !first check if the x,y is in the location list
    do loopCount = 1,treeState%numberTreePlots
        if ( (xtestRev).eq.treeState%xLocation(loopCount) .AND. (ytestRev).eq.treeState%yLocation(loopCount) ) then
            print *,'found tree at ',xtestRev,ytestRev
            vegHeight = treeState%treesHeight(loopCount)
            print *,'tree config files ',treeState%treesfileNumber(loopCount)
            
!           call readMaespaStrConfigIntoState(state, treeState, treeStates)
            !call InitMaespaSingleTreeSingleLoop
            call READZEN(UCONTROL,NUMPNT,NOLAYI,PPLAYI,NZENI,NAZI,DIFZEN)
            call readMaespaTreeConfigFromConfig(treeState%phyFileNumber(loopCount), treeState%strFileNumber(loopCount), &
                treeState%treesfileNumber(loopCount), config)
            print *,NUMPNT
            DO IPT = 1,NUMPNT
                print *,'SUNLA/BEXT before',config%SUNLA,config%BEXT
                IHOUR=amod(timeis,24.)
                
                !! calculate before hand?
                
                print *,idate
                print *,zen
                print *,idoy
                print *,alat
                print *,yd_actual
                call ZENAZ_1HOUR(timeis*2.0,IDOY,24,ALAT,BEARlocal,DEClocal,ZENlocal,AZlocal)
                ! should set time, idoy, khrs=24, alat
                
                
                print *,radabv
                ! RADABV(IHR,1) = DATAIN(IHR,METCOLS(MHPAR)) / UMOLPERJ
                ! or 
                ! RADABV(IHR,1) = DATAIN(IHR,METCOLS(MHRAD)) * FPAR
                ! then for (*:,2)
                ! CALL CALCNIR(RADABV,FBEAM)
                ! then for (*:,3)
                ! CALL THERMAL(TAIR,VPD,FSUN,RADABV)

                
                !print *,IHOUR
                !! FBEAM(IHR,1) = CALCFBMH(IDATE,ZEN(IHR),RADABV(IHR,1))
               
                CALL TRANSB_1hour(IHOUR,config%IPROG,config%ZEN(IHOUR),config%AZ(IHOUR),config%XSLOPE,&
                    config%YSLOPE,config%FBEAM,config%BEXTT,config%XL(IPT),YL(IPT),ZL(IPT),&
                    config%RX,config%RY,config%RZ,config%DXT,config%DYT,config%DZT,config%XMAX,config%YMAX,config%SHADEHT,&
                    config%FOLT,config%ZBC,config%JLEAFT,config%BPTT,config%NOAGECT,config%PROPCT,config%JSHAPET,&
                    config%SHAPET,config%NOTREES,config%SUNLA,config%BEXT)  
                print *,'SUNLA/BEXT after',config%SUNLA,config%BEXT
                
                
                
                
                
                
!                ! Calculate the weighted pathlengths for beam radiation.
!                        CALL TRANSB(IHOUR,IPROG,ZEN(IHOUR),AZ(IHOUR),XSLOPE,YSLOPE,FBEAM,BEXTT,XL(IPT),YL(IPT),ZL(IPT),&
!                                    RX,RY,RZ,DXT,DYT,DZT,XMAX,YMAX,SHADEHT,FOLT,ZBC,JLEAFT,BPTT,NOAGECT,PROPCT,JSHAPET,&
!                                    SHAPET,NOTREES,SUNLA,BEXT)                   
!
!
!                                    
!                        ! Loop over the 3 wavelengths
!                        DO IWAVE = 1,3
!                            ! Calculate the scattered radiation
!                            !print *,'before scatter',iwave,DIFUP(ipt,iwave),DIFDN(ipt,iwave),SCLOST(ipt,iwave)
!                            CALL SCATTER(IPT,IWAVE,MLAYER(IPT),LAYER(IPT),DLAI,EXPDIF,ZEN(IHOUR),BEXT,DMULT2,SOMULT,BMULT,&
!                                            RADABV(IHOUR,IWAVE),FBEAM(IHOUR,IWAVE),TAIR(IHOUR),PREVTSOIL,ARHO(LGP(IPT),IWAVE),&
!                                            ATAU(LGP(IPT),IWAVE),RHOSOL(IWAVE),DIFUP,DIFDN,SCLOST,DOWNTH)
!                            print *,'after scatter',iwave,DIFUP(ipt,iwave),DIFDN(ipt,iwave),SCLOST(ipt,iwave),RADABV(IHOUR,IWAVE)
!                  
!                            ! Lost scattered radiation for each tree (W m-2), averaged over the grid points.
!                            ! RAD June 2008.              
!                            SCLOSTTREE(ITAR,1) = SUM(SCLOST(1:NUMPNT,1)) / NUMPNT
!                            SCLOSTTREE(ITAR,2) = SUM(SCLOST(1:NUMPNT,2)) / NUMPNT
!                            
!                            ! Assume zero reflectance in TR waveband (Norman 1979)
!                            ! But store in the same array the lost tranmission at top of canopy.
!                            SCLOSTTREE(ITAR,3) = SUM(SCLOST(1:NUMPNT,2)) / NUMPNT
!
!                            ! Downwelling longwave radiation (calculated for each gridpoint
!                            ! with the EHC) averaged across the grid points.
!                            IF(IWAVE.EQ.3)DOWNTHTREE(ITAR) = SUM(DOWNTH) / NUMPNT
!                  
!                            ! Calculate absorbed radiation
!                            !print *,'before absrad',iwave,dflux(ipt,iwave),bflux(ipt,iwave),scatfx(ipt,iwave)
!                            CALL ABSRAD(IPT,IWAVE,NZEN,DEXT,BEXT,BMULT,RELDF(IPT),RADABV(IHOUR,IWAVE),&
!                                        FBEAM(IHOUR,IWAVE),ZEN(IHOUR),ABSRP(LGP(IPT),IWAVE),DIFDN(IPT,IWAVE),&
!                                        DIFUP(IPT,IWAVE),DFLUX,BFLUX,SCATFX)
!                            print *,'after absrad',iwave,dflux(ipt,iwave),bflux(ipt,iwave),scatfx(ipt,iwave),RADABV(IHOUR,IWAVE)
!                        END DO
                
                
                
                
                
                
                
                
            enddo
            
            
            
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
    
    
 subroutine readMaespaTreeMapFromConfig(state&
     !, treeStates&
     )
    use MAINDECLARATIONS
    use MaespaConfigState
    use MaespaConfigStateUtils

    
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
    INTEGER, DIMENSION(:), ALLOCATABLE :: phyfileNumber,strfileNumber,treesfileNumber, treesHeight
    INTEGER, DIMENSION(:), ALLOCATABLE :: buildingsHeight

    NAMELIST /location/ xlocation, ylocation, phyfileNumber, strfileNumber, treesfileNumber, treesHeight
    NAMELIST /buildinglocation/   xBuildingLocation, yBuildingLocation, buildingsHeight
    
    NAMELIST /domain/ width,length

    TYPE(maespaConfigvariablesstate) :: treeState  !! 
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

    
    VTITLE = 'MAESPA: version February 2011'
    VTITLE = VTITLE(1:LEN_TRIM(VTITLE))
   
    ! Set program flag
    IPROG = INORMAL
    IPROGUS = ITEST  ! Understorey setting.
    
    ! Did MGDK add these? Are they needed?
    USEMEASSW = 0
    SOILDATA = 0
    
    ! Temporary stuff ... will go into respitatory T acclimation routines.
    TAIRMEM = -999.99 ! All array elements...
    NTAIRADD = 0

    ! Set all the defaults stuff up
    CALL default_conditions(in_path, out_path)
   
    ! Open input files
    CALL OPENINPUTF(CTITLE,TTITLE,PTITLE,STITLE,WTITLE,UTITLE,IWATFILE, &
                    KEEPZEN,IUSTFILE,in_path,out_path)

    ! Get input from control file
    CALL INPUTCON(ISTART, IEND, NSTEP,NUMPNT, NOLAY, PPLAY, NZEN, DIFZEN, NAZ,      &
                    MODELGS, MODELJM, MODELRD, MODELSS, MODELRW, ITERMAX, IOHIST,   &
                    BINSIZE,ICC, CO2INC, TINC,IOTC, TOTC, WINDOTC, PAROTC,          &
                    FBEAMOTC, IWATFILE, IUSTFILE, ISIMUS, NSPECIES, SPECIESNAMES,   &
                    PHYFILES, STRFILES )
    
    ! Get input from canopy structure file
    CALL INPUTSTR(NSPECIES,STRFILES,JLEAFSPEC,BPTSPEC,RANDOMSPEC,NOAGECSPEC,    &
                    JSHAPESPEC,SHAPESPEC,EXTWINDSPEC,NALPHASPEC,ALPHASPEC,      &
                    FALPHASPEC,COEFFTSPEC,EXPONTSPEC,WINTERCSPEC,BCOEFFTSPEC,   &
                    BEXPONTSPEC,BINTERCSPEC,RCOEFFTSPEC,REXPONTSPEC,RINTERCSPEC,&
                    FRFRACSPEC,in_path)
    
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
                    GKSPEC,NOGSDATESSPEC,DATESGSSPEC,D0LSPEC,GAMMASPEC,VPDMINSPEC,WLEAFSPEC,NSIDESSPEC,           &
                    SFSPEC,PSIVSPEC,VPARASPEC,VPARBSPEC,VPARCSPEC,VFUNSPEC,in_path)

    ! Get input from trees file
    CALL INPUTTREE(XSLOPE,YSLOPE,BEAR,X0,Y0,XMAX,YMAX,PLOTAREA,STOCKING,ZHT,Z0HT,ZPD, &
                    NOALLTREES,NOTREES,NOTARGETS,ITARGETS,SHADEHT,NOXDATES, &
                    NOYDATES,NOZDATES,NOTDATES,NOLADATES,NODDATES,DATESX,   &
                    DATESY,DATESZ,DATEST,DATESLA,DATESD,DXT1,DYT1,DZT1,     &
                    RXTABLE1,RYTABLE1,RZTABLE1,ZBCTABLE1,FOLTABLE1,         &
                    TOTLAITABLE,DIAMTABLE1,IFLUSH,DT1,DT2,DT3,DT4,EXPTIME,  &
                    APP,EXPAN,WEIGHTS,NSPECIES,ISPECIES)
     
    ! Do calculations which are not day-dependent.
    DO I=1,NSPECIES
        CALL EXDIFF(NALPHASPEC(I),ALPHASPEC(1:MAXANG,I),FALPHASPEC(1:MAXANG,I),&
                    NZEN,DIFZEN,RANDOMSPEC(I),DEXTSPEC(I,1:MAXANG))
    END DO
   
    ! Get input from the water balance file
    CALL INPUTWATBAL(BPAR, PSIE, KSAT, ROOTRESIST, ROOTRESFRAC, ROOTRAD, ROOTLEN,ROOTMASS,              &
                        MINROOTWP,MINLEAFWP,PLANTK,KSCALING,THROUGHFALL,REASSIGNRAIN,RUTTERB,RUTTERD, MAXSTORAGE, &
                        DRAINLIMIT,ROOTXSECAREA,EQUALUPTAKE,NLAYER, NROOTLAYER, LAYTHICK, INITWATER,    & 
                        FRACROOT, POREFRAC, SOILTEMP, KEEPWET,DRYTHICKMIN,TORTPAR, SIMTSOIL,RETFUNCTION,&
                        FRACORGANIC, EXPINF, WSOILMETHOD, USEMEASET,USEMEASSW,SIMSOILEVAP,USESTAND)
    
    ! Open met data file (must be done after ISTART & IEND read)
    CALL OPENMETF(ISTART,IEND,CAK,PRESSK,SWMIN,SWMAX,USEMEASET,DIFSKY,ALAT,TTIMD,DELTAT,&
                    MFLAG,METCOLS,NOMETCOLS,MTITLE,MSTART,in_path)
    
    ! Open output files
    CALL open_output_files(ISIMUS,CTITLE,TTITLE,PTITLE,STITLE,MTITLE,VTITLE,WTITLE,NSPECIES,SPECIESNAMES,out_path)
    CALL open_file(trim(out_path)//'wattest.dat', UWATTEST, 'write', 'asc', 'replace')
    
    
    IF(ISIMUS.EQ.1)THEN
        CALL INPUTUSSTR(NOUSPOINTS,X0,Y0,GRDAREAI,XLU,YLU,ZLU,USLAITAB,NOFUDATES,DATESFU,&
                        HTUS,NOHUDATES,DATESHU,FOLNUS,NONUDATES,DATESNU,EXTKUS)
        
        CALL INPUTUSPHY(JMAXN25,IECOU,EAVJU,EDVJU,DELSJU,TVJUPU,TVJDNU,VCMAXN25,EAVCU,  &
                        EDVCU,DELSCU,UNMIN,AJQU,ABSRPU,GSBG0U,GSBG1U,CICARAT,RD0US,RDK,   &
                        RDT,SLAUS,EFFY,MOSS,JMAX25M,VCMAX25M,THETAM)
    ENDIF
    
    ! Initialize various variables related to water balance calculations.
    CALL INITWATBAL(LAYTHICK,WETTINGBOT,WETTINGTOP,POREFRAC,WATERGAIN,WATERLOSS,PPTGAIN,    &
                    INITWATER,DRYTHICKMIN,DRYTHICK,CANOPY_STORE,SURFACE_WATERMM,FRACWATER,  &
                    WSOIL,WSOILROOT,NLAYER,NROOTLAYER,ICEPROP,QE,RUNOFF,OUTFLOW,SOILDEPTH,  &
                    SOILDATA,USEMEASSW)
    
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
    ECANMAX, ACANMAX, result)
    MaespaResultPointer = result
    
!   print *,'FH20 (in MaespaSingleTreeSingleLoop)',FH2O(1,IHOUR)*1e-3

    CALL SUMDAILYWAT(WSOIL, WSOILROOT, WEIGHTEDSWP, PPT, ETMM, ETMEAS, DISCHARGE, SOILEVAP, FSOIL1, SURFACE_WATERMM, QH, QE, QN,QC,&
    RADINTERC, WSOILMEAN, WSOILROOTMEAN, SWPMEAN, PPTTOT, ETMMTOT, ETMEASTOT, DISCHARGETOT, SOILEVAPTOT, &
    FSOILMEAN, TFALLTOT, QHTOT, QETOT, QNTOT, QCTOT, RADINTERCTOT)


    !**********************************************************************





END SUBROUTINE MaespaSingleTreeSingleLoop






!**********************************************************************
      SUBROUTINE TRANSB(IHOUR,IPROG, &
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
      END SUBROUTINE TRANSB

            
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
      HHRSlocal = KHRSlocal/2.0
      
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
        ZENITH,AZMTH,XSLOPE,YSLOPE,FBEAM,BEXTT, &
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
          print *,'shaded'
          RETURN
      endif
      
      print *,FBEAM(:,1)
      print *,FBEAM(:,2)
      print *,IHOUR
      print *,FBEAM(IHOUR,1)
      print *,FBEAM(IHOUR,2)
      IF ((FBEAM(IHOUR,1).GT.0.0).OR.(FBEAM(IHOUR,2).GT.0.00)) THEN
        print *,'call tredst'
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
          print *,'not call tredst'
      ENDIF

      RETURN
      END SUBROUTINE TRANSB_1hour



END MODULE ReadMaespaConfigs
