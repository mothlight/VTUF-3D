      SUBROUTINE BARRAY_CUBE(BW,BL,SW,SW2,AL,AW,BH,bldht)

USE MaespaConfigStateUtils
use MaespaConfigState
!use ReadMaespaConfigs

      implicit none


! INITIATE ARRAY (bldht) OF GIVEN SIZE
      INTEGER X,Y,xa,xc,ya,yb,xd
      INTEGER AL,AW,BH,bldht
      INTEGER BW,BL,SW,SW2

      INTEGER veght

      DIMENSION bldht(0:AL+1,0:AW+1)
      DIMENSION veght(0:AL+1,0:AW+1)

! BW - Building width in y (AW) direction
! BL - Building length in x (AL) direction
! SW - street width in x directioon
! SW2 - street width in y direction


       y=1

   1  continue

! now do the loop for one building
        x=1
        do 110 ya=1,BW
         if (y.gt.AW) goto 101     
   5      continue
         do 10 xa=1,BL
          if (x.gt.AL) goto 51
            bldht(x,y)=bh
            x=x+1
  10     continue

! now comes a street
          do 30 xc=1,SW
            if (x.gt.AL) goto 51
            bldht(x,y)=0
            x=x+1
  30      continue

! now the pattern repeats
          goto 5
  51      continue

          x=1
          y=y+1
 110    continue

! now insert the street
          do 120 yb=1,sw2
            x=1
            if (y.gt.AW) goto 101
              do 130 xd=1,al+1
                if (x.gt.AL) goto 121
                bldht(x,y)=0
                x=x+1
  130         continue
  121        continue
            y=y+1
  120     continue
  
      goto 1
            
  101 continue
! end the height loop  

      return
      end
