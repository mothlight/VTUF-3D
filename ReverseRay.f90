! _____________________________________________________________________________________
!
!   VTUF-3D model
!
! -------------------------------------------------------------------------------------

!   This model is written primarily in Fortran 77 but uses some Fortran 2003. Therefore
!   a Fortran 2003 compiler is required.
!
!
!
! -------------------------------------------------------------------------------------
!   Original references:
!
!   Krayenhoff ES, Voogt JA (2007) A microscale three-dimensional urban energy balance
!   model for studying surface temperatures. Boundary-Layer Meteorol 123:433-461
!
!   Krayenhoff ES (2005) A micro-2scale 3-D urban energy balance model for studying
!   surface temperatures. M.Sc. Thesis, University of Western Ontario, London, Canada
! -------------------------------------------------------------------------------------
!
!   *** This model is for research and teaching purposes only ***
!
! -------------------------------------------------------------------------------------
!
!   Last updated:
!     September 2011 by Scott Krayenhoff
!     2013-2016, modified heavily by Kerry Nice
!
! _____________________________________________________________________________________

!! if vegetation is found, reverse ray trace from the top of the domain to find vegetation intersections and
!! distribute sunlit factors to sunlit vegetation and ultimately to the original surface where the
!! forward ray trace originated.
      subroutine reverseRayTrace(xt,xinc,yt,yinc,zt,zinc,xtest,ytest,ztest,bh,al2,aw2,veg_shade,timeis,yd_actual,&
                                finalTransmissionPercentage)
          use TUFConstants
          use MaespaConfigState , only :  maespaConfigTreeMapState,maespaArrayOfTestDataResults
          use ReadMaespaConfigs
          use Dyn_Array, only: maespaDataArray,maespaTestDataArray,treeXYMap !,treeXYMapSunlightPercentageTotal !,treeXYMapSunlightPercentagePoints
                   
      implicit none

      INTEGER AL2,AW2,BH
      !integer x,y,z,f,i,
      integer xtest,ytest,ztest
      real xt,yt,zt,xinc,yinc,zinc
            
      real xincRev, yincRev, zincRev
      real xtRev, ytRev, ztRev
      INTEGER xtestRev, ytestRev, ztestRev
      real timeis
      integer yd_actual
      integer treeConfigLocation
      real transmissionPercentage
      real finalTransmissionPercentage
      real maespaPAR,maespaFBEAM,maespaSUNLA,maespaTD,maespaTSCAT,maespaTTOT,maespaAPARSUN,maespaAPARSH,maespaAPAR
      real maespaLE
      
      logical veg_shade
      dimension veg_shade(0:al2+1,0:aw2+1,0:bh+1)
      TYPE(maespaConfigTreeMapState) :: treeState  
      integer i
      integer lastTreeProcessed
      integer maespaHour
      
      !! these are now in Dyn_array, so do not need to be passed 
!      TYPE(maespaArrayOfTestDataResults),allocatable,dimension(:),intent(in) :: maespaTestDataArray
!      integer,allocatable,dimension(:,:),intent(in) :: treeXYMap

      !! TUF is 0-24 and Maespa 0-48
      maespaHour = int(timeis * 2)
      !only calculate transmission through each tree once
      lastTreeProcessed = -1
      !radiation percentage through each tree
      transmissionPercentage = 1.0
      !final result after passing through all the trees
      finalTransmissionPercentage = 1.0

      ztestRev = ZTEST
      ytestRev = YTEST
      xtestRev = XTEST
      
      xtRev = xt
      ytRev = yt
      ztRev = zt
       
      xincRev = xinc*(-1.0)
      yincRev = yinc*(-1.0)
      zincRev = zinc*(-1.0)
      if(xincRev.gt.0) xincRev=xincRev*(-1.0)
      if(yincRev.gt.0) yincRev=yincRev*(-1.0)
      if(zincRev.gt.0) zincRev=zincRev*(-1.0)
           
      !! loop until you reach the ground
      DO WHILE (ztestRev.GT.0)
 
          ztRev=(ztRev+zincRev)
          xtRev=(xtRev+xincRev)
          ytRev=(ytRev+yincRev)
          ztestRev=NINT(ztRev)
          xtestRev=NINT(xtRev)
          ytestRev=NINT(ytRev)
!          ztestRev=INT(ztRev)  !! change this to rounding down, KN
!          xtestRev=INT(xtRev)
!          ytestRev=INT(ytRev)

          if (ztestRev.LT.0)then
              exit
          ENDIF
          if (ytestRev.LT.0)then
              exit
          ENDIF
          if (xtestRev.LT.0)then
              exit
          ENDIF
    
          !! bound check   veg_shade(0:al2+1,0:aw2+1,0:bh+1)
          if (xtestRev >al2+1 .or. ytestRev > aw2+1 .or. ztestRev > bh+1) then
              !print *,'out of bounds',xtestRev,ytestRev,ztestRev,'for veg_shade in reverseRayTrace()'
          else if (veg_shade(xtestRev,ytestRev,ztestRev))then
              !print *,'reverse ray found vegetation at ',xtestRev,ytestRev,ztestRev              
              !! find what tree this is
              !call findTreeFromConfig(xtestRev,ytestRev,ztestRev,treeState,timeis,yd_actual,treeConfigLocation)
              !! now have tree locations in treeXYMap
              treeConfigLocation=treeXYMap(xtestRev,ytestRev)            
           
              if (treeConfigLocation.eq.-1) then
                   print *,'did not find tree ', xtestRev,ytestRev,ztestRev
              else             
                print *,'tree found ', treeConfigLocation,lastTreeProcessed,treeConfigLocation,xtestRev,ytestRev,ztestRev,getTransmissionForTree(treeConfigLocation)
                  
                !! at this point, treeConfigLocation gives pointers to the tree configuration. Calculate transmission, etc 
                !! and pass it back to the calling function so it can update sunlit factor
                  
                ! find how much transmits through each tree and get final result (only process each tree once)
                if (treeConfigLocation.eq.lastTreeProcessed) then
                    print *,'already processed tree ',treeConfigLocation
                else

                    !maespa doesn't have data for non day light hours. This case shouldn't arise but set it to 0 to make sure
                    transmissionPercentage = 0
                   
                    !treeXYMapSunlightPercentageTotal(xtestRev,ytestRev) = treeXYMapSunlightPercentageTotal(xtestRev,ytestRev) + finalTransmissionPercentage
                    !treeXYMapSunlightPercentagePoints(xtestRev,ytestRev) = treeXYMapSunlightPercentagePoints(xtestRev,ytestRev) + 1
           
                    !! get transmission here
                    transmissionPercentage = 1.0 - getTransmissionForTree(treeConfigLocation)
                   
                    lastTreeProcessed = treeConfigLocation

                    finalTransmissionPercentage = finalTransmissionPercentage * transmissionPercentage                      
                 
                    !! probably also directly update the tree surfaces with absorbed radiation and disregard the reflected
                    !! radiation (for now, maybe in the future see if it can be reallocated)                
                endif     !if (treeConfigLocation.eq.lastTreeProcessed) then              
              endif   ! if (treeConfigLocation.eq.-1) then
          endif    !  if (veg_shade(xtestRev,ytestRev,ztestRev))then    
          ! end of  if (xtestRev >al2+1 .or. ytestRev > aw2+1 .or. ztestRev > bh+1) then
      end do
      

      
    !! these are example end points  
!x  y  z f i    xt         xinc           yt        yinc       zt       zinc      xtest ytest ztest bh al2 aw2
!36 35 0 1 1754 35.749046 -0.000020880278 41.221718 0.11943572 8.521086 0.16042165 36 41 9 8 77 77
!36 35 0 1 1754 36.249046 -0.000020880278 41.221718 0.11943572 8.521086 0.16042165 36 41 9 8 77 77 
!36 35 0 1 1754 36.249046 -0.000020880278 40.721718 0.11943572 8.521086 0.16042165 36 41 9 8 77 77
!36 35 0 1 1754 35.749046 -0.000020880278 40.721718 0.11943572 8.521086 0.16042165 36 41 9 8 77 77
!43 35 0 1 1761 42.749046 -0.000020880278 41.221718 0.11943572 8.521086 0.16042165 43 41 9 8 77 77
!43 35 0 1 1761 43.249046 -0.000020880278 41.221718 0.11943572 8.521086 0.16042165 43 41 9 8 77 77
!43 35 0 1 1761 43.249046 -0.000020880278 40.721718 0.11943572 8.521086 0.16042165 43 41 9 8 77 77
!43 35 0 1 1761 42.749046 -0.000020880278 40.721718 0.11943572 8.521086 0.16042165 43 41 9 8 77 77
!36 36 0 1 1831 35.749046 -0.000020880278 42.221718 0.11943572 8.521086 0.16042165 36 42 9 8 77 77
!36 36 0 1 1831 36.249046 -0.000020880278 42.221718 0.11943572 8.521086 0.16042165 36 42 9 8 77 77
!36 36 0 1 1831 36.249046 -0.000020880278 41.721718 0.11943572 8.521086 0.16042165 36 42 9 8 77 77
!36 36 0 1 1831 35.749046 -0.000020880278 41.721718 0.11943572 8.521086 0.16042165 36 42 9 8 77 77
!42 41 0 1 2017 41.749046 -0.000020880278 47.221718 0.11943572 8.521086 0.16042165 42 47 9 8 77 77
!42 41 0 1 2017 42.249046 -0.000020880278 47.221718 0.11943572 8.521086 0.16042165 42 47 9 8 77 77 
!42 41 0 1 2017 42.249046 -0.000020880278 46.721718 0.11943572 8.521086 0.16042165 42 47 9 8 77 77
!42 41 0 1 2017 41.749046 -0.000020880278 46.721718 0.11943572 8.521086 0.16042165 42 47 9 8 77 77
!42 42 0 1 2074 41.749046 -0.000020880278 48.221718 0.11943572 8.521086 0.16042165 42 48 9 8 77 77
!42 42 0 1 2074 42.249046 -0.000020880278 48.221718 0.11943572 8.521086 0.16042165 42 48 9 8 77 77
!42 42 0 1 2074 42.249046 -0.000020880278 47.721718 0.11943572 8.521086 0.16042165 42 48 9 8 77 77
!42 42 0 1 2074 41.749046 -0.000020880278 47.721718 0.11943572 8.521086 0.16042165 42 48 9 8 77 77
!35 43 0 1 2144 34.749046 -0.000020880278 49.221718 0.11943572 8.521086 0.16042165 35 49 9 8 77 77
!35 43 0 1 2144 35.249046 -0.000020880278 49.221718 0.11943572 8.521086 0.16042165 35 49 9 8 77 77
!42 43 0 1 2151 41.749046 -0.000020880278 49.221718 0.11943572 8.521086 0.16042165 42 49 9 8 77 77
!42 43 0 1 2151 42.249046 -0.000020880278 49.221718 0.11943572 8.521086 0.16042165 42 49 9 8 77 77
!42 43 0 1 2151 42.249046 -0.000020880278 48.721718 0.11943572 8.521086 0.16042165 42 49 9 8 77 77
!42 43 0 1 2151 41.749046 -0.000020880278 48.721718 0.11943572 8.521086 0.16042165 42 49 9 8 77 77
!43 43 0 1 2152 42.749046 -0.000020880278 49.221718 0.11943572 8.521086 0.16042165 43 49 9 8 77 77
!43 43 0 1 2152 43.249046 -0.000020880278 49.221718 0.11943572 8.521086 0.16042165 43 49 9 8 77 77
!39 41 3 2 6852 38.74937  -0.000020880278 45.441334 0.11943572 8.543919 0.16042165 39 45 9 8 77 77
!39 41 3 2 6852 39.24937  -0.000020880278 45.441334 0.11943572 8.543919 0.16042165 39 45 9 8 77 77
      
      RETURN
      END

