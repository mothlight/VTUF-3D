! some intrinsics that GNU Fortran doesn't impliment
! real sind,cosd,tand,asind,acosd,atand
! external sind,cosd,tand,asind,acosd,atand
! --- sin function, taking argument in degrees
      real function sind(arg)
      real arg, pi
      pi = 4.0*atan(1.0)   ! or 3.14159265358979
      sind = sin(arg*pi/180.0)
      return
      end
! --- cos function, taking argument in degrees
      real function cosd(arg)
      real arg, pi
      pi = 4.0*atan(1.0)   ! or 3.14159265358979
      cosd = cos(arg*pi/180.0)
      return
      end
! --- tan function, taking argument in degrees
      real function tand(arg)
      real arg, pi
      pi = 4.0*atan(1.0)   ! or 3.14159265358979
      tand = tan(arg*pi/180.0)
      return
      end
! --- arcsin function, returning argument in degrees
      real function asind(arg)
      real arg, pi
      pi = 4.0*atan(1.0)   ! or 3.14159265358979
      asind = asin(arg)*180.0/pi
      return
      end
! --- arccos function, returning argument in degrees
      real function acosd(arg)
      real arg, pi
      pi = 4.0*atan(1.0)   ! or 3.14159265358979
      acosd = acos(arg)*180.0/pi
      return
      end
! --- arctan function, returning argument in degrees
      real function atand(arg)
      real arg, pi
      pi = 4.0*atan(1.0)   ! or 3.14159265358979
      atand = atan(arg)*180.0/pi
      return
      end
      
