      SUBROUTINE DOTPRO(v1,v2,ndim,dp,g)
! subroutine dotpro returns the angle 'g' (in radians) and the dotproduct 'dp'
! of two vectors v1, v2 with dimensions ndim each
      REAL*8 v1(ndim),v2(ndim),g,dp,v1mag,v2mag,dparg
      dp = 0.
      v1mag = 0.
      v2mag = 0.
      do j=1,ndim
        dp = dp + v1(j)*v2(j)
        v1mag = v1mag + v1(j)**2
        v2mag = v2mag + v2(j)**2
      enddo

      v1mag = sqrt(v1mag)
      v2mag = sqrt(v2mag)
      
      dparg=dp/(v1mag*v2mag)
      
      if (abs(dparg).gt.1) then
        write(*,*) 'warning acos argument > 1'
        write(*,*) 'arg = ', dparg,dp,v1mag,v2mag
        if (dparg.lt.-1) dparg=-1.0
        if (dparg.gt.1) dparg=1.0
      endif
      g = acos(dparg)
      
      RETURN
      END        

