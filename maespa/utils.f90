!**********************************************************************
! UTILS.FOR
!
! This file contains all the 'utility' functions which are called throughout
! the other files. I have set the files up so that, when writing a test program
! which calls one of the program subroutines in file xxx.for, the program
! can be compiled by compiling test.for, xxx.for, and utils.for. 
!
! The functions are:
! IDATE50 - translates string-format date in days-since-1950 format
! IDATEYR - given string-format date, finds Julian day
! JDATE - given days-since-1950-format date, finds Julian day
! SUBERROR - error-handling subroutine
! BETA - implements beta function
! QUADM, QUADP - solve quadratic
! AVERAGEVAL - find mean of array of values
! NIGHT - whether an hour qualifies as night-time or not
!
! SATUR - calculate water vapour saturation pressure at given T
! TK - calculate T in Kelvin given T in Celsius
! RHOFUN - Calculate density of air.
! ZBRENT - bisection routine (used in water balance routines)
! ODEINT - integrator for ordinary diff. eq.
! RK4,RKQS,RKCK - called by ODEINT.
!
! HEATEVAP - Calculates latent heat of evaporation
! FINDLENGTH - Finds length of array that is initialized.
! FILLWITHLAST - Fills array with last value that was entered in inputfile.
!**********************************************************************

!**********************************************************************
INTEGER FUNCTION IDATE50(STRDATE)
! This function translates a string-format date (DD/MM/YY) into the number 
! of days since 1/1/1950. Provides a numerical way of handling dates.
!**********************************************************************

    IMPLICIT NONE
    CHARACTER(8) STRDATE
    INTEGER IDAY,IYEAR,IYRD
    INTEGER, EXTERNAL :: IDATEYR

    ! Get the day of year and the year number from function IDATEYR.
    IDAY = IDATEYR(STRDATE,IYEAR)
    
    ! Calculate how many days in full years have passed since 1950.
    IYRD = 365*(IYEAR - 50)
    IYRD = IYRD + (IYEAR - 49)/4
    IDATE50 = IYRD + IDAY
    
    RETURN
END FUNCTION IDATE50


!**********************************************************************
INTEGER FUNCTION IDATEYR(STRDATE,IYEAR)
! Given a date strdate, this function calculates the number of the day 
! from 1 on 1st Jan to 365/366 on 31st Dec.
!**********************************************************************
    IMPLICIT NONE
    CHARACTER(8) STRDATE
    LOGICAL LEAPYR
    INTEGER IFD(12),IDAY,IMON,IYEAR
    DATA IFD/0,31,59,90,120,151,181,212,243,273,304,334/

    READ (STRDATE,10) IDAY,IMON,IYEAR
    10    FORMAT(T1,I2,T4,I2,T7,I2)
    
    ! Code to handle passage to 2000: note this will only work until 2050
    ! I hope this model has been superseded by then!
    IF (IYEAR.LT.50) IYEAR = IYEAR+100

    LEAPYR = .FALSE.
    IF (4 * (IYEAR/4).EQ.IYEAR) LEAPYR = .TRUE.

    IDATEYR = IFD(IMON) + IDAY
    IF (LEAPYR .AND. (IMON.GE.3)) IDATEYR = IDATEYR + 1

    RETURN
END FUNCTION IDATEYR

!**********************************************************************
INTEGER FUNCTION MONTH(IDATE50)
! This function returns the month given a date in days-since-1950 format.
!**********************************************************************
    IMPLICIT NONE
    INTEGER JD,IDATE50
    INTEGER, EXTERNAL :: JDATE
    JD = JDATE(IDATE50)
    MONTH = JD/30 + 1
    IF (MONTH.GT.12) MONTH = 12

    RETURN
END FUNCTION MONTH


!**********************************************************************
INTEGER FUNCTION JDATE(IDATE50)
! This function returns the julian day given a date in days-since-1950
! format.
!**********************************************************************
    IMPLICIT NONE
    INTEGER IYR,IYRD,IDATE50
    
    IYR = IDATE50 / 365
    IYRD = 365*IYR
    IYRD = IYRD + (IYR - 1)/4 
    JDATE = IDATE50 - IYRD + 1

    RETURN
END FUNCTION JDATE


!**********************************************************************
SUBROUTINE SUBERROR(MESSAGE,IFLAG,IOERROR)
! The error-handling subroutine. When called, writes MESSAGE to the
! error file (UERROR). Uses IFLAG to determine whether to terminate program
! or not. IOERROR is a FORTRAN error number, reported in the error file. 
!**********************************************************************
    USE maestcom
    IMPLICIT NONE
    CHARACTER MESSAGE *(*)
    INTEGER IFLAG,IOERROR

    WRITE(UERROR,*) MESSAGE
    IF (IOERROR.NE.0.AND.IOERROR.NE.-1) WRITE(UERROR,20) IOERROR
    10    FORMAT (A80)
    20    FORMAT ('FORTRAN ERROR CODE NO: ',I10)

    IF (IFLAG.EQ.IFATAL) THEN
        STOP 'FATAL ERROR - SEE MAESERR.DAT FOR DETAILS.'
    END IF
      
    RETURN
END SUBROUTINE SUBERROR


!**********************************************************************
REAL FUNCTION BETA(BC1,BC2,BC3,BC4,RP)
! This a function to define the vertical and horizontal leaf area
! density distribution.
! BM 11/99 added fourth parameter BC4
!**********************************************************************
    IMPLICIT NONE
    REAL BC1,BC2,BC3,BC4,RP
    
    IF (RP.EQ.0.0) RP=0.0000001
    IF (RP.EQ.1.0) RP=0.9999999
    IF (RP.GE.BC4) THEN
        BETA = 0.0
    ELSE
        BETA = BC1* (RP**BC2)* ((BC4-RP)**BC3)
    END IF

    RETURN
END FUNCTION BETA


!**********************************************************************
REAL FUNCTION QUADM(A, B, C, IQERROR)
! Solves the quadratic equation - finds smaller root.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    REAL A, B, C
    INTEGER IQERROR
    
    IQERROR = 0
    
    IF ((B*B - 4.*A*C).LT.0.0) THEN
        CALL SUBERROR('WARNING:IMAGINARY ROOTS IN QUADRATIC',IWARN,0)
        IQERROR = 1
        QUADM = 0.0
    ELSE
        IF (A.EQ.0.0) THEN
            IF (B.EQ.0.0) THEN
                QUADM = 0.0
                IF (C.NE.0.0) CALL SUBERROR('ERROR: CANT SOLVE QUADRATIC',IWARN,0)
            ELSE
                QUADM = -C/B
            END IF
        ELSE
            QUADM = (- B - SQRT(B*B - 4*A*C)) / (2.*A)
        END IF
    END IF

    RETURN
END FUNCTION QUADM


!**********************************************************************
REAL FUNCTION QUADP(A,B,C,IQERROR)
! Solves the quadratic equation - finds larger root.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    REAL A,B,C
    INTEGER IQERROR  
    
    IQERROR = 0
    
    IF ((B*B - 4.*A*C).LT.0.0) THEN
        CALL SUBERROR('WARNING:IMAGINARY ROOTS IN QUADRATIC',IWARN,0)
        IQERROR = 1
        QUADP = 0.0
    ELSE
        IF (A.EQ.0.0) THEN
            IF (B.EQ.0.0) THEN
                QUADP = 0.0
                IF (C.NE.0.0) CALL SUBERROR('ERROR: CANT SOLVE QUADRATIC',IWARN,0)
            ELSE
                QUADP = -C/B
            END IF
        ELSE
            QUADP = (- B + SQRT(B*B - 4*A*C)) / (2.*A)
        END IF
    END IF

    RETURN
END FUNCTION QUADP

         
!**********************************************************************
REAL FUNCTION AVERAGEVAL(ARR,NUMVAL)
! Finds mean value of an array of NUMVAL values.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER NUMVAL
    REAL ARR(MAXP),ARRSUM

    ARRSUM = SUM(ARR(1:NUMVAL))

    AVERAGEVAL = ARRSUM/REAL(NUMVAL)

    RETURN
END FUNCTION AVERAGEVAL


!**********************************************************************
INTEGER FUNCTION NIGHT(ZEN,PAR)
! Function to determine whether a particular hour is at night or not
!**********************************************************************
    
    IMPLICIT NONE
    REAL ZEN,PAR
    
    IF ((ABS(ZEN).LE.1.57).AND.(PAR.GT.0.1)) THEN
        NIGHT = 0
    ELSE
        NIGHT = 1    
    END IF
    
    RETURN
END FUNCTION NIGHT

!**********************************************************************
REAL FUNCTION SATUR(TAC)
! Calculate saturated water vapour pressure (Pa) at temperature TAC (Celsius)
! from Jones 1992 p 110 (note error in a - wrong units)
!**********************************************************************
    IMPLICIT NONE
    REAL TAC
    SATUR = 613.75*EXP(17.502*TAC/(240.97+TAC))
    RETURN
END FUNCTION SATUR

!**********************************************************************
REAL FUNCTION TK(TCELSIUS)
! Converts Celsius temperature to Kelvin.
!**********************************************************************
    USE maestcom
    IMPLICIT NONE
    REAL TCELSIUS
    
    TK = TCELSIUS - ABSZERO
    RETURN
END FUNCTION TK


!**********************************************************************
REAL FUNCTION RHOFUN(TAIRK)
! Density of air (kg m-3) (T-dependent)
! Tested against Jones 1992. 
! RAD 2008.
!**********************************************************************
    IMPLICIT NONE      
    REAL TAIRK
    
    RHOFUN = 353. / TAIRK
      
    RETURN
END FUNCTION RHOFUN


!**********************************************************************
 REAL FUNCTION ZBRENT(FUNC,X1,X2,TOLZ,EXTRAPARS,EXTRAINT)
! Function taken from SPA.
! Bisection routine; finds the root of FUNC in the interval (X1,X2)
!**********************************************************************
    USE maestcom
    IMPLICIT NONE

    INTEGER ITMAX,ITER
    REAL TOLZ,X1,X2,EPS
    REAL EXTRAPARS(EXTRAPARDIM)  ! Parameters passed to FUNC.
    INTEGER EXTRAINT(10)  ! Integer parameters passed to FUNC.
    PARAMETER (ITMAX=30,EPS=3.E-8)
    REAL A,B,C,D,E,FA,FB,FC,P,Q,R,S,TOL1,XM
    REAL, EXTERNAL :: FUNC
    
    A = X1
    B = X2
    FA = FUNC(A, EXTRAPARS, EXTRAINT)
    FB = FUNC(B, EXTRAPARS, EXTRAINT)
    IF((FA.GT.0..AND.FB.GT.0.).OR.(FA.LT.0..AND.FB.LT.0.))THEN
        FA=FUNC(A, EXTRAPARS, EXTRAINT)
        FB=FUNC(B, EXTRAPARS, EXTRAINT)
        !WRITE(*,*)'         FA          FB          X1            X2'
        !WRITE(*,*)FA,FB,X1,X2
        !WRITE(*,*)'ROOT MUST BE BRACKETED FOR ZBRENT'
        FA=FUNC(A, EXTRAPARS, EXTRAINT)
        FB=FUNC(B, EXTRAPARS, EXTRAINT)
    ENDIF
    C=B
    FC=FB
    DO ITER=1,ITMAX
        IF((FB.GT.0..AND.FC.GT.0.).OR.(FB.LT.0..AND.FC.LT.0.))THEN
            C=A
            FC=FA
            D=B-A
            E=D
        ENDIF
        IF(ABS(FC).LT.ABS(FB)) THEN
            A=B
            B=C
            C=A
            FA=FB
            FB=FC
            FC=FA
        ENDIF
        TOL1=2.*EPS*ABS(B)+0.5*TOLZ
        XM=.5*(C-B)
        IF(ABS(XM).LE.TOL1 .OR. FB.EQ.0.)THEN
            ZBRENT=B      
            RETURN        
        ENDIF
        IF(ABS(E).GE.TOL1 .AND. ABS(FA).GT.ABS(FB)) THEN
            S=FB/FA           
            IF(A.EQ.C) THEN   
                P=2.*XM*S
                Q=1.-S
            ELSE
                Q=FA/FC
                R=FB/FC
                P=S*(2.*XM*Q*(Q-R)-(B-A)*(R-1.))
                Q=(Q-1.)*(R-1.)*(S-1.)
            ENDIF
            IF(P.GT.0.) Q=-Q
            P=ABS(P)
            IF(2.*P .LT. MIN(3.*XM*Q-ABS(TOL1*Q),ABS(E*Q))) THEN
                E=D
                D=P/Q
            ELSE
                D=XM
                E=D
            ENDIF
        ELSE
            D=XM
            E=D
        ENDIF
        A=B
        FA=FB
        IF(ABS(D) .GT. TOL1) THEN
            B=B+D
        ELSE
            B=B+SIGN(TOL1,XM)
        ENDIF
        FB=FUNC(B, EXTRAPARS, EXTRAINT)
    END DO
    !WRITE(*,*) 'ZBRENT EXCEEDING MAXIMUM ITERATIONS'
    ZBRENT=B
    RETURN
END FUNCTION ZBRENT
      
!**********************************************************************      
SUBROUTINE ODEINT(YSTART,NVAR,X1,X2,EPS,H1,HMIN,NOK,NBAD,DERIVS,EXTRAPARS)
! Integrator for ordinary differential equations.
! Taken from SPA, April 2008 (RAD).
! Added an extra arguments: EXTRAPARS, an array of parameters to be passed to the 
! function DERIVS. SPA used to rely on globally declared variables, so it did
! not need to pass parameters.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE

    INTEGER NBAD,NOK,NVAR
    INTEGER I,KMAX,KOUNT,NSTP
    REAL EPS,H1,HMIN,X1,X2,YSTART(NVAR)
    REAL DXSAV,H,HDID,HNEXT,X,XSAV,DYDX(NMAX),XP(KMAXX),Y(NMAX)
    REAL YP(NMAX,KMAXX),YSCAL(NMAX)
    REAL EXTRAPARS(EXTRAPARDIM)
    EXTERNAL DERIVS
    
    COMMON /PATH/ KMAX,KOUNT,DXSAV,XP,YP
    X = X1
    H = SIGN(H1,X2-X1)
    NOK = 0
    NBAD = 0
    KOUNT = 0
    DO I=1,NVAR
        Y(I) = YSTART(I)
    END DO
    IF (KMAX.GT.0) XSAV = X-2.*DXSAV
    DO NSTP=1,MAXSTP
        ! Add as argument an array with all parameters. This array can then be passed between functions,
        ! without need for global declarations.
        CALL DERIVS(X,Y,DYDX,EXTRAPARS)
        DO I=1,NVAR
            YSCAL(I)=ABS(Y(I))+ABS(H*DYDX(I))+TINY
        END DO
        IF(KMAX.GT.0)THEN
            IF(ABS(X-XSAV).GT.ABS(DXSAV)) THEN
                IF(KOUNT.LT.KMAX-1)THEN
                    KOUNT=KOUNT+1
                    XP(KOUNT)=X
                    DO I=1,NVAR
                        YP(I,KOUNT)=Y(I)
                    END DO
                    XSAV=X
                ENDIF
            ENDIF
        ENDIF
        IF((X+H-X2)*(X+H-X1).GT.0.) H=X2-X
        CALL RKQS(Y,DYDX,NVAR,X,H,EPS,YSCAL,HDID,HNEXT,DERIVS,EXTRAPARS)  ! RAD change
        IF(HDID.EQ.H)THEN
            NOK=NOK+1
        ELSE
            NBAD=NBAD+1
        ENDIF
        IF((X-X2)*(X2-X1).GE.0.)THEN
        DO I=1,NVAR
            YSTART(I)=Y(I)
        END DO
        IF(KMAX.NE.0)THEN
            KOUNT=KOUNT+1
            XP(KOUNT)=X
            DO I=1,NVAR
                YP(I,KOUNT)=Y(I)    
            END DO
        ENDIF
        RETURN
    ENDIF
    IF(ABS(HNEXT).LT.HMIN) WRITE(*,*)'STEPSIZE SMALLER THAN MINIMUM IN ODEINT'
        H=HNEXT
    END DO
    RETURN
END SUBROUTINE ODEINT

!**********************************************************************
SUBROUTINE RK4(Y,DYDX,N,X,H,YOUT,DERIVS,EXTRAPARS)
! Util function for ODEINT.
! Taken from SPA, April 2008 (RAD).
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER N
    REAL H,X,DYDX(N),Y(N),YOUT(N)
    EXTERNAL DERIVS
    INTEGER I
    REAL H6,HH,XH,DYM(NMAX),DYT(NMAX),YT(NMAX)
    REAL EXTRAPARS(EXTRAPARDIM)
    
    HH = H * 0.5
    H6 = H / 6.0
    XH = X + HH
    DO I=1,N
        YT(I)=Y(I)+HH*DYDX(I)
    END DO
    CALL DERIVS(XH,YT,DYT,EXTRAPARS)
    DO I=1,N
        YT(I)=Y(I)+HH*DYT(I)
    END DO
    CALL DERIVS(XH,YT,DYM,EXTRAPARS)
    DO I=1,N
        YT(I)=Y(I)+H*DYM(I)
        DYM(I)=DYT(I)+DYM(I)
    END DO
    CALL DERIVS(X+H,YT,DYT,EXTRAPARS)
    DO I=1,N
        YOUT(I)=Y(I)+H6*(DYDX(I)+DYT(I)+2.*DYM(I))
    END DO
    RETURN
END SUBROUTINE RK4
        
!**********************************************************************
SUBROUTINE RKQS(Y,DYDX,N,X,HTRY,EPS,YSCAL,HDID,HNEXT,DERIVS,EXTRAPARS)
! Util function for ODEINT.
! Taken from SPA, April 2008 (RAD).
!**********************************************************************

    USE maestcom
    IMPLICIT NONE                                                            
    INTEGER N                                                                     
    REAL EPS,HDID,HNEXT,HTRY,X,DYDX(N),Y(N),YSCAL(N)                              
    EXTERNAL DERIVS                                                               
    INTEGER I                                                                     
    REAL ERRMAX,H,HTEMP,XNEW,YERR(NMAX),YTEMP(NMAX),SAFETY                        
    REAL PGROW,PSHRNK,ERRCON                                                      
    REAL EXTRAPARS(EXTRAPARDIM)                                                            
    PARAMETER (SAFETY=0.9,PGROW=-.2,PSHRNK=-.25,ERRCON=1.89E-4)
                 
    H = HTRY                                                                        
            
    111  CALL RKCK(Y,DYDX,N,X,H,YTEMP,YERR,DERIVS,EXTRAPARS)
    ERRMAX=0.
    DO  I=1,N
        ! MGDK, might not be quite what was intended, but in its
        ! original format this could result in a divide by zero, so...
        IF (YSCAL(I) < 0.0000005) THEN
            ERRMAX = ERRMAX
        ELSE    
            ERRMAX=MAX(ERRMAX,ABS(YERR(I)/YSCAL(I)))
        END IF
    END DO
    ERRMAX=ERRMAX/EPS
    IF(ERRMAX.GT.1.)THEN
        HTEMP=SAFETY*H*(ERRMAX**PSHRNK)
        H=SIGN(MAX(ABS(HTEMP),0.1*ABS(H)),H)
        XNEW=X+H
        IF(XNEW.EQ.X)THEN
            WRITE(*,*) 'STEPSIZE UNDERFLOW IN RKQS'
        ENDIF
        GOTO 111
    ELSE
        IF(ERRMAX.GT.ERRCON)THEN
            HNEXT=SAFETY*H*(ERRMAX**PGROW)
        ELSE
            HNEXT=5.*H
        ENDIF
        HDID=H
        X=X+H
        DO I=1,N
            Y(I)=YTEMP(I)
        END DO
        RETURN
    ENDIF
END SUBROUTINE RKQS
        

!**********************************************************************
SUBROUTINE RKCK(Y,DYDX,N,X,H,YOUT,YERR,DERIVS,EXTRAPARS)
! Util function for ODEINT.
! Taken from SPA, April 2008 (RAD).
!**********************************************************************
    USE maestcom
    IMPLICIT NONE
    INTEGER N
    REAL H,X,DYDX(N),Y(N),YERR(N),YOUT(N)
    EXTERNAL DERIVS
    INTEGER I
    REAL AK2(NMAX),AK3(NMAX),AK4(NMAX),AK5(NMAX),AK6(NMAX),YTEMP(NMAX),A2,A3,A4,A5,A6,              &
            B21,B31,B32,B41,B42,B43,B51,B52,B53,B54,B61,B62,B63,B64,B65,C1,C3,C4,C6,DC1,            &
            DC3,DC4,DC5,DC6
    REAL EXTRAPARS(EXTRAPARDIM)
    PARAMETER (A2=.2,A3=.3,A4=.6,A5=1.,A6=.875,B21=.2,B31=3./40.,B32=9./40.,B41=.3,B42=-.9,         &
                B43=1.2,B51=-11./54.,B52=2.5,B53=-70./27.,B54=35./27.,B61=1631./55296.,             &
                B62=175./512.,B63=575./13824.,B64=44275./110592.,B65=253./4096.,C1=37./378.,        &
                C3=250./621.,C4=125./594.,C6=512./1771.,DC1=C1-2825./27648.,DC3=C3-18575./48384.,   &
                DC4=C4-13525./55296.,DC5=-277./14336.,DC6=C6-.25)
    DO I=1,N
        YTEMP(I)=Y(I)+B21*H*DYDX(I)
    END DO
    CALL DERIVS(X+A2*H,YTEMP,AK2,EXTRAPARS)
    DO I=1,N
        YTEMP(I)=Y(I)+H*(B31*DYDX(I)+B32*AK2(I))
    END DO
    CALL DERIVS(X+A3*H,YTEMP,AK3,EXTRAPARS)
    DO I=1,N
        YTEMP(I)=Y(I)+H*(B41*DYDX(I)+B42*AK2(I)+B43*AK3(I))
    END DO
    CALL DERIVS(X+A4*H,YTEMP,AK4,EXTRAPARS)
    DO I=1,N
        YTEMP(I)=Y(I)+H*(B51*DYDX(I)+B52*AK2(I)+B53*AK3(I)+B54*AK4(I))
    END DO
    CALL DERIVS(X+A5*H,YTEMP,AK5,EXTRAPARS)
    DO I=1,N
        YTEMP(I)=Y(I)+H*(B61*DYDX(I)+B62*AK2(I)+B63*AK3(I)+B64*AK4(I)+B65*AK5(I))
    END DO
    CALL DERIVS(X+A6*H,YTEMP,AK6,EXTRAPARS)
    DO I=1,N
        YOUT(I)=Y(I)+H*(C1*DYDX(I)+C3*AK3(I)+C4*AK4(I)+C6*AK6(I))
    END DO
    DO I=1,N
        YERR(I)=H*(DC1*DYDX(I)+DC3*AK3(I)+DC4*AK4(I)+DC5*AK5(I) + DC6*AK6(I))
    END DO
    RETURN
END SUBROUTINE RKCK

!**********************************************************************
REAL FUNCTION HEATEVAP(TAIR)
! Latent heat of vaporisation of water (J kg-1)
! RAD, May 2008.
! TAIR is in celcius.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE    
    REAL TAIR
    
    ! H2OLV is a constant in MAESTCOM        
    HEATEVAP = (H2OLV0 - 2.365E3 * TAIR)
        
    ! A warning    
    IF(TAIR.GT.100.0)WRITE(*,*)'You are passing T in K to HEATEVAP'
        
END FUNCTION HEATEVAP



!**********************************************************************
! changed by mgdk just to compile, clearly this needs sorting!
INTEGER FUNCTION FINDLENGTH(ARR, ARRLEN, MAXLEN, EMPTYVAL)
! This function finds the length of an array (ARR) that is filled
! with values other than 'EMPTYVAL'. For example, if an array is
! (1,1,1,1,-999,-999), ARRLEN is 6, EMPTYVAL is -999, then the function
! will return 4.
! RAD, Nov. 2008
!**********************************************************************
    
    IMPLICIT NONE
    INTEGER LASTENTRY,K,MAXLEN, ARRLEN
    REAL ARR(ARRLEN),EMPTYVAL
    
    FINDLENGTH = 0
    LASTENTRY = 0
    K = 1
    
    DO WHILE (LASTENTRY.EQ.0)
        IF(ARR(K) .LE. EMPTYVAL)THEN
            LASTENTRY = 1
            FINDLENGTH = K - 1
        ENDIF
        
        IF(K.EQ.MAXLEN.AND.LASTENTRY.EQ.0)THEN
            LASTENTRY = 1
            FINDLENGTH = MAXLEN
        ENDIF
        K = K + 1
    END DO
    
    RETURN
END FUNCTION FINDLENGTH

!**********************************************************************
 SUBROUTINE FILLWITHLAST(ARR, ARRLEN, MAXLEN, EMPTYVAL)
!**********************************************************************
      IMPLICIT NONE
      INTEGER MAXLEN,ARRLEN, FILLEDLEN
      INTEGER, EXTERNAL :: FINDLENGTH
      REAL ARR(ARRLEN),EMPTYVAL
      
      FILLEDLEN = FINDLENGTH(ARR, ARRLEN, MAXLEN, EMPTYVAL)
      
      ! the func will return 0 if empty, but fortran indexes from
      ! 1, so this will seg fault unless we set it to 1 (MGDK, Nov. 2010)
      IF (FILLEDLEN == 0) FILLEDLEN = 1
      
      ARR(FILLEDLEN:MAXLEN) = ARR(FILLEDLEN) 
      
      RETURN
END SUBROUTINE FILLWITHLAST

!**********************************************************************
REAL FUNCTION STDEV(ARR,N)
!**********************************************************************
    IMPLICIT NONE      
    INTEGER N,I
    REAL ARR(N)
    REAL ARRMEAN2,ARR2MEAN

    ARRMEAN2 = (SUM(ARR(1:N))/REAL(N))**2
    ARR2MEAN = 0.0
    DO I = 1,N
        ARR2MEAN = ARR2MEAN + ARR(I)**2 / REAL(N)
    END DO

    ! May happen due to floating point inaccuracies.
    IF(ARRMEAN2.GT.ARR2MEAN)THEN
        STDEV = 0.0
    ELSE
        STDEV = SQRT(ARR2MEAN - ARRMEAN2)
    ENDIF

    RETURN
END FUNCTION STDEV

!**********************************************************************
INTEGER FUNCTION DIVFOURNEAR(NUM)
! Finds the nearest smaller number that can be divided by 4.
! RAD, March 2009.      
!********************************************************************
    
    IMPLICIT NONE
    INTEGER NUM,NEWNUM
    
    NEWNUM = NUM
    DO WHILE (MOD(NEWNUM,4).GT.0)
        NEWNUM = NEWNUM - 1
    END DO

    DIVFOURNEAR = NEWNUM

    RETURN
END FUNCTION DIVFOURNEAR
      

!**********************************************************************
SUBROUTINE REVARRAY(ARR, ARRLEN, NEL, NEWARR) 
! Reverse the elements of a one-dimensional array.
! RAD, December 2009.      
!**********************************************************************
    
    USE maestcom
    IMPLICIT NONE
    INTEGER ARRLEN, NEL, I
    REAL ARR(ARRLEN), NEWARR(ARRLEN)
        
    DO I=1,NEL
        NEWARR(I) = ARR(NEL - I + 1)
    END DO
      
    RETURN
END SUBROUTINE REVARRAY
  
      
