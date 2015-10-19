!     
! File:   ReadMaespaConfigs.f90
! Author: kerryn
!
! Created on 14 March 2014, 12:42 PM
!

MODULE ReadMaespaConfigs

    contains
     
   subroutine findTreeFromConfig(xtestRev,ytestRev,ztestRev,treeState,timeis,yd_actual,treeConfigLocation)
    use MAINDECLARATIONS
    use MaespaConfigState
 
    INTEGER xtestRev,ytestRev,ztestRev
    INTEGER vegHeight
    INTEGER loopCount
    INTEGER phyFile, strFile, treeFile
    TYPE(maespaConfigTreeMapState) :: treeState     
    real timeis
    INTEGER yd_actual
    real ZENlocal
    integer treeConfigLocation
       
    NAMELIST /CONTROL/ IOHRLY,IOTUTD,IOHIST,IORESP,IOWATBAL,IOFORMAT,ISUNLA,KEEPZEN
    
    ! Output file for errors and warnings
    OPEN (UERROR, FILE = 'Maeserr.dat', STATUS = 'UNKNOWN')
    OPEN (UCONTROL, FILE = 'confile.dat', STATUS = 'OLD',IOSTAT=IOERROR)
    IF(IOERROR.NE.0)THEN
        CALL SUBERROR('ERROR: CONFILE.DAT DOES NOT EXIST' ,IFATAL,0)
    ENDIF
    
    treeConfigLocation = -1
   
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
   

 subroutine readMaespaTreeMapFromConfig(state)
    use MAINDECLARATIONS
    use MaespaConfigState
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
    INTEGER, DIMENSION(:), ALLOCATABLE ::  partitioningMethod
    
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
    !! switch to try different methods of partitioning 0=Tsfc only from Maespa, 1=Maespa fluxes
    NAMELIST /runSwitches/ partitioningMethod 
    
    
    NAMELIST /domain/ width,length,configTreeMapCentralArrayLength,configTreeMapCentralWidth,configTreeMapCentralLength,configTreeMapX,configTreeMapY,configTreeMapX1,configTreeMapX2,configTreeMapY1,configTreeMapY2,configTreeMapGridSize,configTreeMapNumsfcab,configTreeMapHighestBuildingHeight

    TYPE(maespaConfigTreeMapState), intent(OUT) :: state   !!
     
    format_string = "(A5,I1)"
    
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
    
    
    allocate(partitioningMethod(1))
    REWIND (UTREESMAP)
    READ (UTREESMAP, runSwitches, IOSTAT = IOERROR)
    IF (IOERROR.NE.0) CALL SUBERROR('ERROR READING runSwitches DETAILS',IFATAL,IOERROR)
    state%configPartitioningMethod=partitioningMethod(1)
    
    end subroutine readMaespaTreeMapFromConfig
    
    
       
   subroutine getVegHeight(state, treeState, &
       x, y, vegHeight)
    use MAINDECLARATIONS
    use MaespaConfigState
    
    INTEGER x,y
    INTEGER vegHeight
    INTEGER loopCount
    INTEGER phyFile, strFile, treeFile
    TYPE(maespaConfigTreeMapState), intent(OUT) :: treeState     
    
    
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
    
    INTEGER x,y
    INTEGER vegHeight
    INTEGER loopCount
    INTEGER phyFile, strFile, treeFile
    TYPE(maespaConfigTreeMapState) :: treeState     
    
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
    
    INTEGER x,y
    INTEGER buildingHeight
    INTEGER loopCount
    TYPE(maespaConfigTreeMapState) :: treeState     

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
      

         subroutine readMaespaDataFiles(treeMapFromConfig,maespaDataArray,treeXYMap,treeXYTreeMap)
          use MaespaConfigState, only : maespaConfigTreeMapState,maespaDataResults,maespaArrayOfDataResults
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
                call readMaespaHRWatDataFiles(treeFilesNumber,maespaDataArray(loopCount)%maespaOverallDataArray,gridSize)
                dataLoaded = .TRUE.
            endif 
            
            !! ok, didn't find it, load next item
            if (dataLoaded .eqv. .FALSE.) then
                 treeFilesNumber=treeMapFromConfig%treesfileNumber(loopCount)
!                 print *,'load ',treeMapFromConfig%treesfileNumber(loopCount),loopCount
                 treeLocationMap(loopCount)=treeFilesNumber
                 call readMaespaHRWatDataFiles(treeFilesNumber,maespaDataArray(loopCount)%maespaOverallDataArray,gridSize)
            endif

        end do
          
      end subroutine readMaespaDataFiles

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
      INTEGER TREESDAT_FILE
      INTEGER STRDAT_FILE
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
      character(len=200) :: treesdatfilestr  
      character(len=200) :: strdatfilestr  
      logical loadUspar
      real area
      REAL COEFFT,EXPONT,WINTERC, BCOEFFT,BEXPONT,BINTERC, RCOEFFT,REXPONT,RINTERC,FRFRAC
      
      CHARACTER(8) DATES(maxdate)
      INTEGER NODATES
      REAL VALUES
      REAL HTCROWN,HTTRUNK,DIAM
      REAL WBIOM, deltaQVeg
 
      NAMELIST /ALLOM/ COEFFT, EXPONT, WINTERC
      NAMELIST /ALLOMB/ BCOEFFT, BEXPONT, BINTERC
      NAMELIST /ALLOMR/ RCOEFFT, REXPONT, RINTERC, FRFRAC
      
      NAMELIST /ALLHTCROWN/ NODATES, DATES, VALUES
      NAMELIST /ALLHTTRUNK/ NODATES, DATES, VALUES
      NAMELIST /ALLDIAM/ NODATES, DATES, VALUES
      
      real mVeg, cVeg, deltaTveg, deltaTime

      loadUspar = .TRUE.
      width2=width1
      hours=0.5
      area = width1*width2
                       
     HRFLXDAT_FILE = 1246
     hrlinesToSkip = 35 
     WATBALDAT_FILE = 1247
     linesToSkip = 37
     USPARDAT_FILE = 1257
     usparlinesToSkip = 11
     
     TREESDAT_FILE = 1258
     STRDAT_FILE = 1259
     
     !! cVeg = 2928 J/kg K, taken from Oliphant (2004)
     cVeg = 2928
     deltaTime = hours * 60 * 60
     
     
     write(unit=treeConfigLocationStr,fmt=*) treeConfigLocation
     watbalfilestr = trim(adjustl('./'))//trim(adjustl(treeConfigLocationStr))//trim(adjustl('/watbal.dat'))

     hrflxfilestr = trim(adjustl('./'))//trim(adjustl(treeConfigLocationStr))//trim(adjustl('/hrflux.dat'))
     
     usparfilestr = trim(adjustl('./'))//trim(adjustl(treeConfigLocationStr))//trim(adjustl('/uspar.dat'))
     print *,usparfilestr
     
    ! Input file with data on tree position and size
    treesdatfilestr = trim(adjustl('./'))//trim(adjustl(treeConfigLocationStr))//trim(adjustl('/trees.dat'))
    OPEN (TREESDAT_FILE, FILE = treesdatfilestr, STATUS='OLD', IOSTAT=IOERROR)
    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('ERROR: TREES.DAT DOES NOT EXIST', IFATAL, 0)
    ENDIF
    
    strdatfilestr = trim(adjustl('./'))//trim(adjustl(treeConfigLocationStr))//trim(adjustl('/str1.dat'))
    OPEN (STRDAT_FILE, FILE = strdatfilestr, STATUS='OLD', IOSTAT=IOERROR)
    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('ERROR: STR1.DAT DOES NOT EXIST', IFATAL, 0)
    ENDIF
    
    REWIND (STRDAT_FILE)
    READ (STRDAT_FILE, ALLOM, IOSTAT = IOERROR)    

    REWIND (STRDAT_FILE)
    READ (STRDAT_FILE, ALLOMB, IOSTAT = IOERROR)

    REWIND (STRDAT_FILE)
    READ (STRDAT_FILE, ALLOMR, IOSTAT = IOERROR)    

!    COEFFTI = COEFFT
!    EXPONTI = EXPONT
!    WINTERCI = WINTERC
!    BCOEFFTI = BCOEFFT
!    BEXPONTI = BEXPONT
!    BINTERCI = BINTERC
!    RCOEFFTI = RCOEFFT
!    REXPONTI = REXPONT
!    RINTERCI = RINTERC
!    FRFRACI = FRFRAC
  
    ! Get green crown height of each tree
    !CALL READTREEARRAY(TREESDAT_FILE,3,NOALLTREES,NOZDATES,DATESZ,R3)
    REWIND (TREESDAT_FILE)
    READ(TREESDAT_FILE,ALLHTCROWN,IOSTAT=IOERROR)
    HTCROWN=VALUES
    
    ! Get trunk length of each tree
    !CALL READTREEARRAY(TREESDAT_FILE,4,NOALLTREES,NOTDATES,DATEST,TRUNK)   
    REWIND (TREESDAT_FILE)
    READ(TREESDAT_FILE,ALLHTTRUNK,IOSTAT=IOERROR)
    HTTRUNK=VALUES
        
    ! Get diameter of each tree
    !CALL READTREEARRAY(TREESDAT_FILE,6,NOALLTREES,NODDATES,DATESD,DIAMA)
    REWIND (TREESDAT_FILE)
    READ(TREESDAT_FILE,ALLDIAM,IOSTAT=IOERROR)
    DIAM=VALUES
    
    !! calculate woody biomass for the tree, units are kg dry weight/tree
    WBIOM = COEFFT * (HTCROWN + HTTRUNK) * (DIAM ** EXPONT) + WINTERC
    print *,'COEFFT,HTCROWN,HTTRUNK,DIAM,EXPONT,WINTERC',COEFFT,HTCROWN,HTTRUNK,DIAM,EXPONT,WINTERC
    !! convert to kg/m2
    mVeg = WBIOM / area
    print *,'WBIOM,mVeg',WBIOM,mVeg
    
        OPEN (WATBALDAT_FILE, FILE = watbalfilestr, STATUS='OLD', IOSTAT=IOERROR)
        IF (IOERROR.NE.0) THEN
            CALL SUBERROR('ERROR: watbal.dat DOES NOT EXIST', IFATAL, 0)
        ENDIF
        
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
           maespaData(i)%qeCalc2 = maespaData(i)%leFromEt + convertMMETToLEWm2(maespaData(i)%canopystore,width1,width2,hours)/area + convertMMETToLEWm2(maespaData(i)%evapstore,width1,width2,hours)/area  
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
           
           if (i .eq. 1) then
               deltaTveg = 0
           else
               deltaTveg = maespaData(i)%TCAN - maespaData(i-1)%TCAN
           endif
           !! According to McCaughey (1985), the biomass heat storage flux, DQv, can be obtained with a simple calorimetric approach
           !! Eq 7 from Oliphant (2004)
           !! this will make J/m2 sec
           !! make deltaTveg 1, so that this can be calculated when Tsfc and Tconv are available
           deltaTVeg = 1
           deltaQVeg = mVeg * cVeg * (deltaTveg / deltaTime)
           maespaData(i)%deltaQVeg = deltaQVeg
           print *,'i,deltaQVeg,deltaTveg',i,deltaQVeg,deltaTveg
           
!        print *,'i,maespaData(i)%leFromEt',i,maespaData(i)%leFromEt
                           
                           
        end do
        close(WATBALDAT_FILE)
        close(HRFLXDAT_FILE)
        if (loadUspar .eqv. .TRUE.) then
            close(USPARDAT_FILE)
        endif
        close(TREESDAT_FILE)
        close(STRDAT_FILE)
        

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
                
        ifatal = 100
        POINTSDAT_FILE = 1250
        
        treeConfigLocation=1       
        write(unit=treeConfigLocationStr,fmt=*) treeConfigLocation
        pointsfilestr = trim(adjustl('./'))//trim(adjustl(treeConfigLocationStr))//trim(adjustl('/points.dat'))
        
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
 
        TESTFLXDAT_FILE = 1249
     
        linesToSkip = 21 
         
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
