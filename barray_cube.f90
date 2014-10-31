      SUBROUTINE BARRAY_CUBE(BW,BL,SW,SW2,AL,AW,BH,bldht,veght)

USE MaespaConfigStateUtils
use MaespaConfigState
use ReadMaespaConfigs

      implicit none


! INITIATE ARRAY (bldht) OF GIVEN SIZE
      INTEGER X,Y,xa,xc,ya,yb,xd
      INTEGER AL,AW,BH,bldht
      INTEGER BW,BL,SW,SW2

      INTEGER veght
      TYPE(maespaConfigTreeMapState) :: treeState 
      INTEGER calculatedVegHeight
      !INTEGER, EXTERNAL :: IDATE50
      !CHARACTER(10) STARTDATE
      
      INTEGER N1, N2      
      INTEGER :: loopCount
      INTEGER calculatedBuildingHeight

      DIMENSION bldht(0:AL+1,0:AW+1)
      DIMENSION veght(0:AL+1,0:AW+1)

! BW - Building width in y (AW) direction
! BL - Building length in x (AL) direction
! SW - street width in x directioon
! SW2 - street width in y direction

      !STARTDATE = '09/02/12'
      !ISTART = IDATE50(STARTDATE)  
      calculatedVegHeight = 0
      call readMaespaTreeMapFromConfig(treeState)

!       y=1
!
!   1  continue
!
!! now do the loop for one building
!        x=1
!        do 110 ya=1,BW
!         if (y.gt.AW) goto 101     
!   5      continue
!         do 10 xa=1,BL
!          if (x.gt.AL) goto 51
!            bldht(x,y)=bh
!            x=x+1
!  10     continue
!
!! now comes a street
!          do 30 xc=1,SW
!            if (x.gt.AL) goto 51
!            bldht(x,y)=0
!            x=x+1
!  30      continue
!
!! now the pattern repeats
!          goto 5
!  51      continue
!
!          x=1
!          y=y+1
! 110    continue
!
!! now insert the street
!          do 120 yb=1,sw2
!            x=1
!            if (y.gt.AW) goto 101
!              do 130 xd=1,al+1
!                if (x.gt.AL) goto 121
!                bldht(x,y)=0
!                x=x+1
!  130         continue
!  121        continue
!            y=y+1
!  120     continue
!  
!      goto 1
!            
!  101 continue
! end the height loop  
      
      N1=treeState%width-1
      N2=treeState%length-1
!      print *,N1,N2
      
      do I = 1,N1
        do J = 1, N2   
            call getBuildingHeightFromConfig(I, J, calculatedBuildingHeight, treeState)           
            bldht(I,J) = calculatedBuildingHeight
            
            !! temp hack, make sure trees are not put on buildings
            if (calculatedBuildingHeight.eq.0) then                
                call getVegHeightFromConfig(I, J, calculatedVegHeight, treeState)
                veght(I,J) = calculatedVegHeight 
            endif

            
            !print *,i,j,calculatedVegHeight,calculatedBuildingHeight
        end do
      end do
  
!  do i=15,19
!      do j=15,19
!          print *,i,j,bldht(i,j)
!      end do
!  end do
      
  do i=15,19
      do j=15,19
          print *,i,j,veght(i,j)
      end do
  end do      
    
      

      return
      end
