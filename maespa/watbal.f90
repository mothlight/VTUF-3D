!**********************************************************************
! WATBAL.FOR
! RAD November 2008.
!
!     This file contains all the subroutines for calculating the water
!     balance. Based on SPA water balance routines, with various modifications.
!
!     Modifications mostly apply to soil water potential, hydraulic and thermal
!     conductivity functions, and format of input parameters. The code is also organized
!     very differently into subroutines and functions than the original SPA code.
!
!     The subroutines are:
!
!     INITWATBAL - initializes water balance variables.
!     CALCSOILPARS - calculates relative water uptake from soil layers
!     WATBALLAY - calls water balance routines, for layered soil.
!     HEATBALANCE - redistributes heat, and calls the soil T profile calculation.
!     FINDSOILTK - estimates soil surface temperature from heat balance.
!     ENERGYFUN - returns the soil surface energy balance closure given soil T.
!     ENERGYCALC - calculates the components of the soil surface energy balance.
!     SOILWPFUN - soil water potential function.
!     SOILCONDFUN - soil hydraulic conductivity function.
!     SOILRESCALC - soil to root hydraulic conductance function.
!     WATERUPTAKELAYER - called by CALCSOILPARS.
!     CANOPY_BALANCE - canopy interception, canopy drainage and wet ET.
!     CANSTOR - canopy storage function, integrated by CANOPY_BALANCE
!     WETTINGLAYERS - surface wetting and drying, calculates dry layer thickness.
!     QEFLUX - soil evaporation (latent heat flux, QE).
!     SOIL_BALANCE - integrates soil gravitational drainage.
!     SOILSTOR - soil storage function, integrated by SOIL_BALANCE.
!     INFILTRATE - infiltration of rainfall into top layers of soil.
!     WATERTHERMAL - redistributes heat according to water movement.
!     CRANKNICHOLS - calculates soil T profile.
!     THERMCONDSUB - soil thermal conductivity.
!     VOLHCFUN - Calculate volumetric heat capacity.
!     SCALEUP - scales single-tree estimates of ET and radiation absorbed
!               to stand-average values.
!     ASSIGNSOILWATER - Assign soil water content, depending on parameters
!           IWATBALSIM, WSOILMETHOD and USEMEASSW
!
!**********************************************************************


!**********************************************************************
SUBROUTINE INITWATBAL(LAYTHICK,WETTINGBOT,WETTINGTOP, &
                            POREFRAC,WATERGAIN,WATERLOSS,PPTGAIN, &
                            INITWATER,DRYTHICKMIN,DRYTHICK, &
                            CANOPY_STORE, SURFACE_WATERMM, &
                            FRACWATER,WSOIL,WSOILROOT, &
                            NLAYER,NROOTLAYER,ICEPROP, &
                            QE,RUNOFF,OUTFLOW,SOILDEPTH, &
                            SOILDATA,USEMEASSW)

! Initializes various water balance related variables.
! RAD, May 2008
!**********************************************************************

    USE maestcom
    USE switches
    IMPLICIT NONE
    INTEGER SOILDATA,USEMEASSW,IOERROR
    INTEGER NLAYER,I,NROOTLAYER,IZEROVAR
    REAL LAYTHICK(MAXSOILLAY),WETTINGBOT(10),WETTINGTOP(10)
    REAL POREFRAC(MAXSOILLAY),WATERGAIN(MAXSOILLAY)
    REAL WATERLOSS(MAXSOILLAY),INITWATER(MAXSOILLAY)
    REAL FRACWATER(MAXSOILLAY),PPTGAIN(MAXSOILLAY)
    REAL ICEPROP(MAXSOILLAY)
    REAL DRYTHICK,DRYTHICKMIN,QE,RUNOFF,OUTFLOW
    REAL CANOPY_STORE,SURFACE_WATERMM,SOILDEPTH
    REAL WSOIL,WSOILROOT,ZEROVAR

    FRACWATER = INITWATER
    ICEPROP = 0.

    DRYTHICK = DRYTHICKMIN

    WETTINGTOP = 0.
    WETTINGBOT = 0.
    WETTINGBOT(1) = LAYTHICK(1) ! See io.for in SPA

    QE = 0.  ! Soil evaporation (as latent heat flux)
    WATERGAIN = 0.  ! array of water gains per layer (m)
    WATERLOSS = 0.  ! array of water losses per layer (m)
    PPTGAIN = 0.

    RUNOFF = 0.   ! Cumulative Runoff
    OUTFLOW = 0.

    CANOPY_STORE = 0.
    SURFACE_WATERMM = 0.

    ! Soil properties in the layer below the 'lowest layer'. This is the 'soil core'
    ! that water drains into. In SPA, the 'core' counter is used for this layer.
    POREFRAC(NLAYER+1) = POREFRAC(NLAYER)
    FRACWATER(NLAYER+1) = FRACWATER(NLAYER)
    LAYTHICK(NLAYER+1) = LAYTHICK(NLAYER)

    ! Total depth of rooted soil (m), for conversion of WSOIL to water content.
    SOILDEPTH = SUM(LAYTHICK(1:NROOTLAYER))

    ! Initial total soil water storage in the whole soil, and in the rooted profile.
    WSOIL = 0.
    WSOILROOT = 0.
    DO I = 1,NLAYER
        WSOIL = WSOIL + INITWATER(I)*LAYTHICK(I)*1000
    END DO

    DO I = 1,NROOTLAYER
        WSOILROOT = WSOILROOT + INITWATER(I)*LAYTHICK(I)*1000
    END DO

    ! If there is soil data in the met.dat file, but the user has set usemeassw=0
    ! as well as iwatbalsim=0, set usemeassw to 1, and print a warning.
    IF(SOILDATA.GT.0.AND.USEMEASSW.EQ.0)THEN
        USEMEASSW = 1
        CALL SUBERROR('WARNING: USEMEASSW is set to 1, measured soil water is used.',IWARN,IOERROR)
    ENDIF

    ! Write initial water storage and fraction to output file, all others set to 0
    ! Move this to inout.for, use same routine as for output somehow...
    ZEROVAR = -999.00
    IZEROVAR = 0
    IF (IOFORMAT .EQ. 1) THEN
        WRITE (UWATBAL) REAL(IZEROVAR),REAL(IZEROVAR),WSOIL,ZEROVAR,ZEROVAR, &
                        ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR, &
                        ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR, &
                        ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR, &
                        ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR, &
                        ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR
    ELSE IF (IOFORMAT .EQ. 0) THEN
        WRITE (UWATBAL,520)IZEROVAR,IZEROVAR,WSOIL,ZEROVAR,ZEROVAR, &
                        ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR, &
                        ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR, &
                        ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR, &
                        ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR, &
                        ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR,ZEROVAR
            520   FORMAT (I7,I7,51(F14.4,1X))
    END IF            
    RETURN
END SUBROUTINE INITWATBAL


!**********************************************************************

      SUBROUTINE CALCSOILPARS(NLAYER,NROOTLAYER,ISPEC,SOILWP,FRACWATER, &
                            FRACORGANIC,POREFRAC,SOILCOND,THERMCOND, &
                            ROOTMASS,ROOTLEN,LAYTHICK,ICEPROP, &
                            EQUALUPTAKE,RETFUNCTION, &
                            USEMEASSW, SOILDATA, SOILMOISTURE, &
                            PSIE,BPAR,KSAT,ROOTRESIST, &
                            ROOTRESFRAC, &
                            ROOTRAD,MINROOTWP,TOTLAI, &
                            WIND, ZHT, Z0HT, GAMSOIL, &
                            WEIGHTEDSWP,TOTESTEVAP,   &
                            FRACUPTAKE,TOTSOILRES,ALPHARET,WS,WR,NRET) 

! Calculates soil water potential, hydraulic conductivity,
! soil-to-root hydraulic conductance (leaf specific), thermal
! conductivity, and fraction water uptake from each soil layer
! (that has roots). No uptake takes place yet (see WATBALLAY subroutine).

! RAD, May 2008
!**********************************************************************

      USE maestcom
      USE metcom
      IMPLICIT NONE
      INTEGER I, NLAYER, NROOTLAYER, EQUALUPTAKE, RETFUNCTION, SOILDATA
      INTEGER USEMEASSW,ISPEC
      REAL BPAR(MAXSOILLAY),PSIE(MAXSOILLAY),KSAT(MAXSOILLAY)
      REAL ALPHARET(MAXSOILLAY),WS(MAXSOILLAY),WR(MAXSOILLAY),NRET(MAXSOILLAY)
      REAL SOILWP(MAXSOILLAY),FRACWATER(MAXSOILLAY),POREFRAC(MAXSOILLAY)
      REAL SOILCOND(MAXSOILLAY),ROOTMASS(MAXSOILLAY),ROOTLEN(MAXSOILLAY)
      REAL LAYTHICK(MAXSOILLAY), ICEPROP(MAXSOILLAY)
      REAl SOILRRES1(MAXSOILLAY), SOILRRES2(MAXSOILLAY)
      REAL FRACUPTAKE(MAXSOILLAY),SOILRRES(MAXSOILLAY)
      REAL THERMCOND(MAXSOILLAY), FRACORGANIC(MAXSOILLAY)
      REAL MINROOTWP,KTOT,SOILZPD,GAMSOIL,TOTESTEVAP
      REAL SOILMOISTURE,ROOTRESIST,ROOTRESFRAC,ROOTRAD,TOTLAI
      REAL WIND,ZHT,Z0HT,WEIGHTEDSWP,TOTSOILRES
      REAL, EXTERNAL :: SOILCONDFUN
      REAL, EXTERNAL :: THERMCONDFUN
      REAL, EXTERNAL :: SOILWPFUN
      REAL, EXTERNAL :: SOILCONDFUN2
      REAL, EXTERNAL :: GBCANMS
      
! Update soil water potential,soil conductivity, soil thermal conductivity and
! soil to root conductance for each soil layer.
      DO I=1,NLAYER
          SOILWP(I) = SOILWPFUN(FRACWATER(I),PSIE(I),BPAR(I), &
             POREFRAC(I),ALPHARET(I),WS(I),WR(I),NRET(I),RETFUNCTION)
          SOILCOND(I) = SOILCONDFUN(FRACWATER(I),KSAT(I), &
             BPAR(I),POREFRAC(I),WS(I),WR(I),NRET(I),RETFUNCTION)
          THERMCOND(I) = THERMCONDFUN(I, SOILWP(I), FRACWATER(I), &
                                      POREFRAC(I), BPAR(I), &
                                      FRACORGANIC(I),RETFUNCTION)

      ENDDO
      
! Calculate soil conductivity and soil-to-root resistance if using measured soil water content.
      IF(USEMEASSW.EQ.1)THEN

        ! Only supported (sofar) for soil water potential
        IF(SOILDATA.EQ.POTENTIAL)THEN

           SOILCOND(1) = SOILCONDFUN2(SOILMOISTURE,KSAT(1),BPAR(1), &
              PSIE(1))

        ENDIF
      ENDIF
      
      
    CALL SOILRESCALC(USEMEASSW, SOILCOND,ROOTRESIST, &
                            ROOTMASS,ROOTLEN,LAYTHICK,ROOTRAD, &
                            NROOTLAYER,SOILRRES1,SOILRRES2)

    ! Fractional water uptake from each soil layer
    ! Note that water is not actually taken up yet.
    CALL WATERUPTAKELAYER(SOILWP,SOILRRES1,SOILRRES2, &
                                ROOTRESFRAC, &
                                MINROOTWP,TOTLAI, &
                                ICEPROP,EQUALUPTAKE, &
                                USEMEASSW, SOILDATA, SOILMOISTURE, &
                                ROOTLEN,NROOTLAYER,WEIGHTEDSWP, &
                                FRACUPTAKE,TOTSOILRES,LAYTHICK, &
                                TOTESTEVAP)

      
! Aerodynamic conductance between soil surface and air,
! assuming turbulent transfer (so that conductance is the same for
! momentum, heat and mass transfer (Jones 1992)).
      SOILZPD = 0.0  ! Reference height is the soil surface.
      GAMSOIL = GBCANMS(WIND,ZHT,Z0HT,SOILZPD)

      RETURN
      END


!**********************************************************************

        SUBROUTINE WATBALLAY(IDAY,IHOUR,PPT,RUTTERB,RUTTERD,MAXSTORAGE, &
                              THROUGHFALL,RADINTERC, &
                              CANOPY_STORE, EVAPSTORE, DRAINSTORE, &
                              SURFACE_WATERMM, &
                              POREFRAC,WETTINGBOT,WETTINGTOP,NLAYER, &
                              NROOTLAYER,LAYTHICK,SOILTK,QE, &
                              TAIRK,VPDPA,WIND, &
                              ZHT,Z0HT,ZPD,PRESS,ETMM,ETMMSPEC,NOSPEC, &
                              USEMEASET,ETMEAS,FRACUPTAKESPEC, &
                              ICEPROP,FRACWATER,DRAINLIMIT, &
                              KSAT,BPAR,WSOIL,WSOILROOT,DISCHARGE, &
                              DRYTHICKMIN,DRYTHICK,QEMM,OVERFLOW, &
                              WATERGAIN,WATERLOSS,PPTGAIN,KEEPWET, &
                              EXPINF,WS,WR,NRET,RETFUNCTION)

! Do water balance for layered soil.
! Replaces subroutines WATERFLUXES and WATERTHERMAL in SPA
! No calculation of heat balance yet, but should be placed in separate routine.
!
! RAD, July 2008
!**********************************************************************

        USE maestcom
        IMPLICIT NONE
        INTEGER I,J,RR,NLAYER,NROOTLAYER,USEMEASET,IDAY,IHOUR
        INTEGER KEEPWET
        INTEGER RETFUNCTION,NOSPEC,ISPEC
        REAL POREFRAC(MAXSOILLAY),WETTINGBOT(10),WETTINGTOP(10)
        REAL LAYTHICK(MAXSOILLAY)
        REAL WATERGAIN(MAXSOILLAY),WATERLOSS(MAXSOILLAY)
        REAL FRACUPTAKESPEC(MAXSOILLAY,MAXSP),FRACWATER(MAXSOILLAY)
        REAL FRACUPTAKE(MAXSOILLAY)
        REAL ICEPROP(MAXSOILLAY),PPTGAIN(MAXSOILLAY)
        REAL BPAR(MAXSOILLAY), KSAT(MAXSOILLAY)
        REAL DRAINLIMIT(MAXSOILLAY)
        real check1, check2, check3, check4
        REAL MAXSTORAGE,LAMBDASOIL,ETMMSPEC(MAXSP)
        REAL SOILTK,TAIRK,VPDPA,VPDKPA,QEM,QEMM,SURFACE_WATERMM
        REAL PPT,EVAPSTORE,DRAINSTORE,WSOILROOT,TOTESTEVAPMM
        REAL SOILTC,TAIRC,WSOIL,SNOW,QE,THROUGHFALL,WIND,ZHT,Z0HT
        REAL ZPD,PRESS,RADINTERC,RUTTERB,RUTTERD,CANOPY_STORE,DRYTHICKMIN
        REAL DRYTHICK,ETLOSS,ETMEAS,ETMM,DISCHARGE,EXPINF,OVERFLOW
        REAL WATERCONTENT,ICECONTENT
        REAL, EXTERNAL :: HEATEVAP
        REAL WS(MAXSOILLAY),WR(MAXSOILLAY),NRET(MAXSOILLAY)
        
!       Conversions
        SOILTC = SOILTK - FREEZE
        TAIRC = TAIRK - FREEZE
        VPDKPA = VPDPA / 1000

! Latent heat of soil evaporation.
        LAMBDASOIL = HEATEVAP(SOILTC)

!       Init.
        WSOIL = 0.
        WATERLOSS = 0.
        WATERGAIN = 0.
        SNOW = 0.

!       mm t-1 (mm per timestep)
        QEMM = (-QE/LAMBDASOIL)*SPERHR
!       m t-1
        QEM = QEMM / 1000

        ! Assume that water infiltrates in one timestep.
        SURFACE_WATERMM = 0.

! Determine throughfall.
        IF(THROUGHFALL.EQ.1.0.OR.MAXSTORAGE.EQ.0.0)THEN
            SURFACE_WATERMM = PPT
            EVAPSTORE = 0.0
            DRAINSTORE = 0.0
        ELSE
            CALL CANOPY_BALANCE(PPT,WIND,ZHT,Z0HT,ZPD, &
                            PRESS,TAIRC,RADINTERC, &
                            VPDPA,THROUGHFALL, &
                            RUTTERB,RUTTERD,MAXSTORAGE, &
                            CANOPY_STORE, SURFACE_WATERMM, &
                            EVAPSTORE, DRAINSTORE)
        ENDIF

!       Calculates the thickness of the top dry layer (if any).
        CALL WETTINGLAYERS(POREFRAC,WETTINGBOT,WETTINGTOP, &
                               SURFACE_WATERMM,SNOW,TAIRK,QE, &
                               NLAYER,LAYTHICK,DRYTHICKMIN,DRYTHICK)

!       Option to keep soil wet, also keep DRYTHICK = 0
        IF(KEEPWET.EQ.1)DRYTHICK = 0.001

!       From which layer is soil evaporation withdrawn?
!       Note that it currently can only come from 1st or 2nd layer!
!       This probably puts constraints on the thickness of soil layers (default = 10cm)
        IF(DRYTHICK.LT.LAYTHICK(1))THEN
              RR=1      !the dry zone does not extend beneath the top layer
        ELSE
              RR=2      !it does
        ENDIF

!       Evaporation
        IF(QE.LT.0.)THEN
            WATERLOSS(RR) = WATERLOSS(RR) + QEM
!       Dew formation (positive values of QE are now actually prevented in QEFLUX, see there).
        ELSE
            WATERGAIN(1) = WATERGAIN(1) - QEM
        ENDIF

!       Use measured or modelled ET for water balance calculations:
        IF(USEMEASET.EQ.1)THEN
            ETLOSS = MAX(0.0, ETMEAS / 1000)
            DO I=1,NROOTLAYER
                WATERLOSS(I) = WATERLOSS(I) + ETLOSS*FRACUPTAKE(I)
            ENDDO
! Use modelled; allow for multiple species.
        ELSE
        
            DO ISPEC=1,NOSPEC
                
                !       Water loss from each rooted layer (i.e. *root water uptake)
                ETLOSS = ETMMSPEC(ISPEC) / 1000
                FRACUPTAKE(1:MAXSOILLAY) = FRACUPTAKESPEC(1:MAXSOILLAY, ISPEC)

                DO I=1,NROOTLAYER
                    WATERLOSS(I) = WATERLOSS(I) + ETLOSS*FRACUPTAKE(I)
                ENDDO
            
            ENDDO
        ENDIF

        check1 = sum(fracuptake(1:nrootlayer))
        check2 = etmm/1000 + qem
        check3 = sum(etmmspec(1:nospec))/1000 + qem
        check4 = sum(waterloss(1:nrootlayer))
        
  
        
!       Calculate drainage for each soil layer
        DO J = 1,NLAYER
        CALL SOIL_BALANCE(J, POREFRAC, ICEPROP, FRACWATER, &
                          LAYTHICK, DRAINLIMIT(J), WATERLOSS, WATERGAIN, &
                          KSAT(J), BPAR(J),WS(J),WR(J),NRET(J),RETFUNCTION)
        ENDDO
        
! Loss of water at lowest soil layer (discharge, or deep drainage) (mm).
        DISCHARGE = 1000 * WATERGAIN(NLAYER + 1)

!       Infiltration of water reaching the surface
!       NOTE: WATERGAIN and WATERLOSS are not updated here.
!       Instead, PPTGAIN is an output array that is added to water balance.
!       This is because of heat balance: temperature of rain equals air temperature,
!       temperature of soil water equals soil temperature.
        CALL INFILTRATE(SURFACE_WATERMM,NLAYER,POREFRAC,FRACWATER, &
                        LAYTHICK,WATERGAIN,WATERLOSS, &
                        EXPINF,PPTGAIN,OVERFLOW)
        
!       Add and subtract gains and losses for each layer
        DO J = 1,NLAYER

          ! Option to not change initial soil water.
          IF(KEEPWET.NE.1)THEN

          ! M of water
          WATERCONTENT = (FRACWATER(J)*(1. - ICEPROP(J)))*LAYTHICK(J)

          ICECONTENT = (FRACWATER(J)*ICEPROP(J))*LAYTHICK(J)

          WATERCONTENT = MAX(0., WATERCONTENT + WATERGAIN(J) + &
              PPTGAIN(J) - WATERLOSS(J))

          ! Volumetric water content
          FRACWATER(J) = (WATERCONTENT+ICECONTENT) / LAYTHICK(J)

          IF(WATERCONTENT.EQ.0.0)THEN
            ICEPROP(J) = 0.0
          ELSE
            ICEPROP(J) = ICECONTENT / (WATERCONTENT+ICECONTENT)
          ENDIF

          ENDIF

!         Total soil water storage (mm)
          WSOIL = WSOIL + FRACWATER(J)*LAYTHICK(J)*1000

! Do error checking here...
        ENDDO

! Total soil water storage in rooted zone
        WSOILROOT = 0.
        DO J = 1,NROOTLAYER
          WSOILROOT = WSOILROOT + FRACWATER(J)*LAYTHICK(J)*1000
        ENDDO

        RETURN
        END


!**********************************************************************

      SUBROUTINE HEATBALANCE(NLAYER,FRACWATER,POREFRAC,TAIRK,SOILTK, &
                             SOILTEMP,LAYTHICK,WATERGAIN,WATERLOSS, &
                             PPTGAIN,THERMCOND)

! Does the heat balance calculation: soil surface temperature,
! which is based on energy balance closure (net radiation,
! sensible and latent heat flux, soil heat flux),
! soil T profile, redistribution of heat due to water movement.
! Based on various bits of code from SPA, in subroutines
! SOILDAY, WATERTHERMAL, ENERGY, ETC.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
      INTEGER NLAYER
      REAL THERMCOND(MAXSOILLAY),SOILTEMP(MAXSOILLAY)
      REAL FRACWATER(MAXSOILLAY),POREFRAC(MAXSOILLAY)
      REAL LAYTHICK(MAXSOILLAY),WATERGAIN(MAXSOILLAY)
      REAL WATERLOSS(MAXSOILLAY),PPTGAIN(MAXSOILLAY)
      REAL VOLHC(MAXSOILLAY)
      REAL TAIRK,SOILTK

! Update soil temperature based on movement (and uptake) of soil water.
! This should be a small component of the heat balance.
      CALL WATERTHERMAL(NLAYER, FRACWATER, POREFRAC, &
                        SOILTEMP, LAYTHICK, WATERGAIN, &
                        TAIRK, WATERLOSS, PPTGAIN, VOLHC)

! Calculate soil temperature profile with Crank-Nicholson scheme.
      CALL CRANKNICHOLS(NLAYER, LAYTHICK, SOILTK, SOILTEMP, &
                        VOLHC, THERMCOND)

      RETURN
      END

!**********************************************************************
      SUBROUTINE FINDSOILTK(IDAY,TAIRK, GAMSOIL, &
                            PRESSPA, SOILTK, SOILTK2, VPDKPA, RGLOB, &
                            THERMCOND1, LAYTHICK1, POREFRAC1, &
                            SOILWP1,DRYTHICK,TORTPAR,VIEWFACTOR)

! Finds the soil surface temperature from energy balance, by
! calling the ZBRENT routine, which uses the ENERGY function.
! Based on SPA (bits in SOILDAY mostly). (June 2008, RAD).
!**********************************************************************

      USE maestcom
      IMPLICIT NONE
      INTEGER IDAY
      INTEGER EXTRAINT(10)
      REAL EXTRAPARS(EXTRAPARDIM)
      REAL LAYTHICK1,VIEWFACTOR
      REAL TAIRK,GAMSOIL,PRESSPA,SOILTK,SOILTK2,VPDKPA
      REAL RGLOB,THERMCOND1,POREFRAC1,SOILWP1
      REAL DRYTHICK,TORTPAR
      REAL T1,T2,XACC,TEST
      REAL, EXTERNAL :: ENERGYFUN
      REAL, EXTERNAL :: ZBRENT
      
      ! Put parameters into EXTRAPARS array:
      EXTRAPARS(1) = GAMSOIL
      EXTRAPARS(2) = PRESSPA
      EXTRAPARS(3) = SOILTK2
      EXTRAPARS(4) = VPDKPA
      EXTRAPARS(5) = RGLOB
      EXTRAPARS(6) = TAIRK
      EXTRAPARS(7) = THERMCOND1
      EXTRAPARS(8) = LAYTHICK1
      EXTRAPARS(9) = POREFRAC1
      EXTRAPARS(10) = SOILWP1
      EXTRAPARS(11) = DRYTHICK
      EXTRAPARS(12) = TORTPAR
      EXTRAPARS(13) = VIEWFACTOR
     
! Set bounds for root-finding (quite liberal bounds!).
      T1 = TAIRK - 50.
      T2 = TAIRK + 50.

! Error tolerance for soil temperature numerical solution (degrees C).
      XACC = 0.0001

! Find the soil surface temperature that gives closure in energy balance:
      SOILTK = ZBRENT(ENERGYFUN,T1,T2,XACC,EXTRAPARS, EXTRAINT)

! Print warning if the solution is at the bounds.
      IF(SOILTK.EQ.T1.OR.SOILTK.EQ.T2) &
         CALL SUBERROR('WARNING: SOIL T SOLUTION AT BOUNDS.', &
           IWARN,0)

      RETURN
      END


!**********************************************************************

        REAL FUNCTION ENERGYFUN(SOILTK,EXTRAPARS,EXTRAINT)

! Calculates the energy balance closure (based on Hinzmann et al. 1998).
! Used to find the soil surface temperature that achieves closure
! (i.e. zero net balance),in the function FINDSOILTK.
! Taken from SPA (June 2008, RAD).
!**********************************************************************

      USE maestcom
      IMPLICIT NONE
      REAL EXTRAPARS(EXTRAPARDIM)
      INTEGER EXTRAINT(10)
      REAL LAYTHICK1,VIEWFACTOR,GAMSOIL,PRESSPA,SOILTK2,VPDKPA
      REAL RGLOB,TAIRK,THERMCOND1,POREFRAC1,SOILWP1,DRYTHICK
      REAL TORTPAR,QE,QH,QN,QC,ESOIL,SOILTK

      ! Get parameters from EXTRAPARS array:
      GAMSOIL = EXTRAPARS(1)
      PRESSPA = EXTRAPARS(2)
      SOILTK2 = EXTRAPARS(3)
      VPDKPA  = EXTRAPARS(4)
      RGLOB   = EXTRAPARS(5)
      TAIRK   = EXTRAPARS(6)
      THERMCOND1 = EXTRAPARS(7)
      LAYTHICK1 = EXTRAPARS(8)
      POREFRAC1 = EXTRAPARS(9)
      SOILWP1 = EXTRAPARS(10)
      DRYTHICK = EXTRAPARS(11)
      TORTPAR = EXTRAPARS(12)
      VIEWFACTOR = EXTRAPARS(13)

! Subroutine that actually does all the work.
      CALL ENERGYCALC(SOILTK,GAMSOIL,PRESSPA,SOILTK2, &
                        VPDKPA,RGLOB,TAIRK,THERMCOND1,LAYTHICK1, &
                        POREFRAC1,SOILWP1,DRYTHICK,TORTPAR, &
                        VIEWFACTOR, &
                        QH,QE,QN,QC,ESOIL)

! Energy balance:
      ENERGYFUN = QH + QE + QN + QC

      RETURN
      END


!**********************************************************************
      SUBROUTINE ENERGYCALC(SOILTK,GAMSOIL,PRESSPA,SOILTK2, &
                            VPDKPA,RGLOB,TAIRK,THERMCOND1,LAYTHICK1, &
                            POREFRAC1,SOILWP1, &
                            DRYTHICK,TORTPAR, &
                            VIEWFACTOR,QH,QE,QN,QC,ESOIL)

! Calculate components of the soil energy balance.
! For arguments, subscript (1 or 2) refers to soil layer (1 = surface).
! RAD, June 2008.
!**********************************************************************

      USE maestcom
      IMPLICIT NONE
      REAL LAYTHICK1,VIEWFACTOR,SOILTK,GAMSOIL,PRESSPA,SOILTK2
      REAL VPDKPA,RGLOB,TAIRK,THERMCOND1,POREFRAC1,SOILWP1
      REAL DRYTHICK,TORTPAR,QH,QE,QN,QC,ESOIL,TAIRC
      REAL RHO,QEFLUX
      REAL, EXTERNAL :: RHOFUN
      REAL, EXTERNAL :: ESOILFUN

      ! Conversions
      TAIRC = TAIRK - FREEZE

      ! Density of air (kg m-3)
      RHO = RHOFUN(TAIRK)

      ! Sensible heat flux (W m-2)
      QH = CPAIR * RHO * GAMSOIL * (TAIRK - SOILTK)

      ! Latent heat flux (W m-2)
      QE = QEFLUX(SOILTK,TAIRK,VPDKPA,POREFRAC1,SOILWP1, &
                  GAMSOIL,PRESSPA,DRYTHICK,TORTPAR)

      ! No soil evap if surface is frozen
      IF(SOILTK.LE.FREEZE)QE = 0.

      ! Net radiation - emitted longwave varies with surface temp.
      ESOIL = ESOILFUN(SOILTK)
      QN = (1-SOILALBEDO)*RGLOB - ESOIL

      ! Soil heat flux (flux of heat out of layer 1 into layer 2).
      QC = -THERMCOND1 * (SOILTK - SOILTK2)/LAYTHICK1
      ! Was 0.5*LAYTHICK1; but actually has to transport to middle of next layer (otherwise we get a mismatch!)
      ! This actually should be the depth from the middle of layer 1 to the middle of layer 1, which
      ! only is LAYTHICK1 when 1 and 2 have the same thickness!!!

      RETURN
      END

!**********************************************************************
      REAL FUNCTION SOILWPFUN(WATERCONTENT,PSIE,BPAR, &
                              POROSITY,ALPHARET,WS,WR,NRET,RETFUNCTION)

! Soil water potential (MPa), using the Campbell (1974) equation.
! PSIE (MPa),BPAR(-),POROSITY(m3 m-3),WATERCONTENT (m3 m-3)
! Output in MPa
! Or using van Genutchen (1980) equation, alpha (MPa-1),WS and WR (m3 m-3)
! RAD, April 2008.
!**********************************************************************

      USE maestcom
      IMPLICIT NONE
      INTEGER RETFUNCTION,IOFATAL
      REAL WATERCONTENT,PSIE,BPAR,POROSITY
      REAL ALPHARET,WS,WR,NRET
      
      SOILWPFUN = 1.0  ! INIT WITH WRONG VALUE.

    IF(WATERCONTENT.EQ.0.) THEN
      SOILWPFUN = -999.0           ! bug mathias décembre 2012 pour éviter valeure -Infinity qui crée NA dans WEIGHTEDSMP
    ELSE 
      IF(RETFUNCTION.EQ.1)THEN
        SOILWPFUN = PSIE*(WATERCONTENT/POROSITY)**(-BPAR)
      ENDIF
      IF(RETFUNCTION.EQ.2)THEN
        SOILWPFUN = PSIE*(WATERCONTENT)**(-BPAR)
      ENDIF
      IF(RETFUNCTION.EQ.3) THEN
          IF(WATERCONTENT.LT.WR)THEN
              SOILWPFUN = -999.0
          ELSE IF (WATERCONTENT.GT.WS) THEN
              SOILWPFUN = -0.0001
          ELSE
              SOILWPFUN = - 1/ALPHARET * (((WS-WR)/(WATERCONTENT-WR))**(NRET/(NRET-1)) - 1)**(1/NRET) 
          END IF
      END IF   
    ENDIF
      
      IF(SOILWPFUN.GT.0) THEN
        SOILWPFUN = 0.0
        CALL SUBERROR('ERROR IN SOIL WATER POTENTIAL CALCULATION', &
           IOFATAL,0)
      ENDIF

    IF(SOILWPFUN.LT.-999) THEN      ! modification mathias decembre 2012 pour eviter bug
        SOILWPFUN = -999.0
    ENDIF
      
      RETURN
      END ! SOILWPFUN


!**********************************************************************

      REAL FUNCTION SOILCONDFUN(WATERCONTENT,KSAT,BPAR,POROSITY,WS,WR,N,RETFUNCTION)

! Soil hydraulic conductivity, using the Campbell (1974) equation. or the van Genutchen equation
! Inputs:
! KSAT (ms-1),BPAR(-),POROSITY(m3 m-3),WATERCONTENT (m3 m-3)
! Output in m s-1
! RAD, April 2008
!**********************************************************************

      USE maestcom
      IMPLICIT NONE
      INTEGER RETFUNCTION
      REAL KSAT,WATERCONTENT,BPAR,POROSITY
      REAL WS,WR,N,ALPHAPOT

      IF (RETFUNCTION.EQ.1.OR.RETFUNCTION.EQ.2) THEN
        ! Campbell 1974
          SOILCONDFUN = KSAT*(WATERCONTENT/POROSITY)**(2*BPAR + 3)
      ELSE IF (RETFUNCTION.EQ.3) THEN
        ! van genuchten
          IF (WATERCONTENT.LT.WR) THEN
              SOILCONDFUN = 0
          ELSE IF (WATERCONTENT.GT.WS) THEN
              SOILCONDFUN = KSAT
          ELSE
              ALPHAPOT = (( (WS - WR)/(WATERCONTENT-WR) )**(N/(N-1)) - 1) ** (1/N)
              SOILCONDFUN = KSAT * (1 - ALPHAPOT**(N-1) * ( 1+ ALPHAPOT**N )**(-1+1/N) ) **2   &
                            / (1 + ALPHAPOT**N)**(1/2 - 1/(2*N))
           END IF
      END IF    
            
! Avoid underflow (avoiding very small numbers).
      IF(SOILCONDFUN.LT.1E-30) SOILCONDFUN = 1E-30

      END !SOILCONDFUN


!**********************************************************************
      REAL FUNCTION SOILCONDFUN2(SOILWP,KSAT,BPAR,PSIE)

! Soil hydraulic conductivity, using the Campbell (1974) equation.
! This is an alternatve to SOILCONDFUN, which uses water content directly.
! Inputs:
! KSAT (ms-1),BPAR(-),PSIE(MPa),SOILWP(MPa)
! Output in m s-1
! RAD, April 2008
!**********************************************************************

      USE maestcom
      IMPLICIT NONE
      REAL KSAT, SOILWP, PSIE, BPAR

! Campbell 1974
      SOILCONDFUN2 = KSAT*(PSIE/SOILWP)**(2+3/BPAR)

! Avoid underflow (avoiding very small numbers).
      IF(SOILCONDFUN2.LT.1E-30) SOILCONDFUN2 = 1E-30

      END !SOILCONDFUN2

!**********************************************************************

      SUBROUTINE SOILRESCALC(USEMEASSW,SOILCOND,ROOTRESIST,ROOTMASS, &
                             ROOTLEN,LAYTHICK,ROOTRAD,NROOTLAYER, &
                             SOILR1,SOILR2)

! Calculate Soil-to-root hydraulic resistance
! Output is SOILR in MPa s m2 mmol-1
! Modified from SPA (RAD, April 2008).
!**********************************************************************

      USE maestcom
      IMPLICIT NONE
      INTEGER NROOTLAYER,USEMEASSW,NITER,I
      REAL LSOIL,MPAM,LA
      REAL SOILCOND(MAXSOILLAY),ROOTMASS(MAXSOILLAY),ROOTLEN(MAXSOILLAY)
      REAL LAYTHICK(MAXSOILLAY), SOILRRES(MAXSOILLAY)
      REAL SOILR1(MAXSOILLAY), SOILR2(MAXSOILLAY)
      REAL ROOTRESIST,ROOTRAD,DEPTH
      REAL ROOTRESCONS,RS,RS2,SPAROOTRESIST
      REAL KSOIL,KS,LOGRR,TOTROOT,MEANROOTLEN
      REAL, EXTERNAL :: SOILCONDFUN

! Hydraulic head for conversion (MPa / m)
      MPAM = 0.009807

! New representation of root resistance following Hacke et al.
! Root resistance in a layer is proportional to 1/rootmass and proportional
! to root length (i.e. depth of layer)
      LA = 0.0
      DO I = 1,NROOTLAYER
        DEPTH = SUM(LAYTHICK(1:I)) - LAYTHICK(I)/2
        IF(ROOTLEN(I).GT.0.0)THEN
            LA = LA +  DEPTH/ROOTLEN(I)
        ENDIF
      ENDDO
      IF(LA.GT.0.0)THEN
        ROOTRESCONS = ROOTRESIST / LA
      ELSE
        ROOTRESCONS = 1E09   ! Arbitrarily large number
      ENDIF

       DO I=1,NROOTLAYER

              ! Depth to middle of layer (ca. root path length)
              DEPTH = SUM(LAYTHICK(1:I)) - LAYTHICK(I)/2

              ! Soil hydraulic conductivity in m2 s-1 MPa-1
              !LSOIL = SOILCOND(I)/MPAM   !converts from ms-1 to m2 s-1 MPa-1
          
              ! ... in mol m-1 s-1 MPa-1. Note that original KSAT was given in same units. 
              KSOIL = SOILCOND(I)  / (H2OVW * GRAV * 1E-03)
          
            IF(KSOIL.LT.1E-35)THEN      !prevent floating point error
                    SOILRRES(I) = 1e35
            ELSE
                
                    ! Reformulated to match Duursma et al. 2008.
                    IF(ROOTLEN(I).GT.0.0)THEN
                        
                        ! Radius of soil cylinder around root in single-root model               
                        RS = SQRT(1./(ROOTLEN(I)*PI))
                
                        LOGRR = LOG(RS/ROOTRAD)
                        IF(LOGRR.LT.0)CALL SUBERROR( &
                        'Root radius larger than soil-root radius - fine root density too high!', &
                         IWARN,0)
                
                        KS = ROOTLEN(I)*LAYTHICK(I)*2.0*pi*KSOIL/LOGRR
                
                        SOILR1(I) = 1/KS
                
                        ! As in SPA:
                        !RS2 = LOG(RS/ROOTRAD)/(2.0*PI*ROOTLEN(I)*LAYTHICK(I)*LSOIL)
                        ! convert from MPa s m2 m-3 to MPa s m2 mmol-1
                        !SOILR1(I) = RS2*1E-6*18*0.001

                        ! Note : this component is calculated but not used (see wateruptakelayer proc). More research needed!
                        ! Second component of below ground resistance related to root hydraulics.
                    
                        SOILR2(I) = ROOTRESCONS * DEPTH / ROOTLEN(I)
                    ELSE
                        SOILR2(I) = 0.0
                        SOILR1(I) = 0.0
                    ENDIF
                    
                    SOILRRES(I) = SOILR1(I) + SOILR2(I)
            ENDIF

       ENDDO

    
      ! When using measured soil water, use water content of top layer,
      ! but all fine roots to calculate total soil conductance
      IF(USEMEASSW.EQ.1)THEN
          
          KSOIL = SOILCOND(1)  / (H2OVW * GRAV * 1E-03)
          
          ! Total average fine root density
          TOTROOT = 0
          DO I=1,NROOTLAYER
              TOTROOT = TOTROOT + ROOTLEN(I)*LAYTHICK(I)
          ENDDO
          MEANROOTLEN = TOTROOT / SUM(LAYTHICK(1:NROOTLAYER))
          
          ! Average soil cylinder around roots.
          RS = SQRT(1./(MEANROOTLEN*PI))      
          LOGRR = LOG(RS/ROOTRAD)
          
          ! Total soil conductance
          KS = TOTROOT*2.0*pi*KSOIL/LOGRR
          
          SOILR1(1) = 1/KS
          
          SOILR2(1) = ROOTRESCONS * (SUM(LAYTHICK(1:NROOTLAYER))/2) / MEANROOTLEN

          SOILRRES(1) = SOILR1(1) + SOILR2(1)
      ENDIF
      
      RETURN
      END !SOILRESCALC


!**********************************************************************

      SUBROUTINE WATERUPTAKELAYER(SOILWP,SOILRRES1,SOILRRES2, &
                                  ROOTRESFRAC, &
                                  MINROOTWP,TOTLAI, &
                                  ICEPROP,EQUALUPTAKE, &
                                  USEMEASSW, SOILDATA, SOILMOISTURE, &
                                  ROOTLEN,NROOTLAYER,WEIGHTEDSWP, &
                                  FRACUPTAKE,TOTSOILRES,LAYTHICK, &
                                  TOTESTEVAP) ! rajout laythick mathias décembre 2012

! Function for deciding from which layer water is withdrawn,
! and to calculate soil water potential weighted by root fraction.
! Taken from SPA, April 2008 (RAD), but heavily modified.
!**********************************************************************

        USE maestcom
        USE metcom
        IMPLICIT NONE
    
        INTEGER I,NROOTLAYER,EQUALUPTAKE,SOILDATA,USEMEASSW
        REAL ESTEVAP(MAXSOILLAY),FRACUPTAKE(MAXSOILLAY),RSUM
        REAL FRACUPTAKE2(MAXSOILLAY), LAYTHICK(MAXSOILLAY) 
        REAL ICEPROP(MAXSOILLAY), SOILWP(MAXSOILLAY)
        REAL SOILRRES1(MAXSOILLAY),SOILRRES2(MAXSOILLAY)
        REAL SOILRRES(MAXSOILLAY),RESTOT(MAXSOILLAY)
        REAL ROOTLEN(MAXSOILLAY),SWPDIFF
        REAL MINROOTWP,KTOT,KTOT2,TOTESTEVAP,TOTESTEVAPMM
        REAL WEIGHTEDSWP,TOTSOILRES,SOILMOISTURE
        REAL EMAXLEAF,TOTLAI,FRACSUM,ROOTRESFRAC,TOTESTEVAPWET

        !Reset.
        TOTESTEVAP = 0.
        TOTESTEVAPWET = 0.
        WEIGHTEDSWP = 0.
        ESTEVAP = 0.
        FRACUPTAKE = 0.

        ! Total soil conductance at the leaf level; including soil and root component.
        ! SOILRRES2 is commented out : don't use root-component of resistance (is part of plant resistance)
        SOILRRES = SOILRRES1 !+ SOILRRES2

        ! Estimated max transpiration from gradient-gravity / soil resis
        IF(EQUALUPTAKE.EQ.0)THEN
        
        DO I=1,NROOTLAYER

                ! Estimated maximum uptake rate with the current soil watyer
                ! if aboveground resistance is zero.
                IF(SOILRRES(I).GT.0.0)THEN
                    ESTEVAP(I)=(SOILWP(I)-MINROOTWP)/SOILRRES(I)
                ELSE
                    ESTEVAP(I)=0.0   !  When no roots present
                ENDIF

                ! No negative uptake.
                ESTEVAP(I)=MAX(0.,ESTEVAP(I))

                ! Soil water potential weighted by layer Emax (from SPA)
                WEIGHTEDSWP = WEIGHTEDSWP + SOILWP(I)*ESTEVAP(I)

                ! No uptake from frozen soils
                IF(ICEPROP(I).GT.0.)ESTEVAP(I)=0.
        ENDDO
    
    
        ! Option 1 (not currently used) : SPA method to figure out relative water uptake.
        ! Fraction uptake in each layer by Emax in each layer (SPA)
        IF(SUM(ESTEVAP(1:NROOTLAYER)).EQ.0.0)THEN
            FRACUPTAKE = 0.0
        ELSE
            DO I=1,NROOTLAYER
                FRACUPTAKE(I) = ESTEVAP(I)/SUM(ESTEVAP(1:NROOTLAYER))
            ENDDO
        ENDIF

        ! sum of EMAX for each layer
        TOTESTEVAP = SUM(ESTEVAP(1:NROOTLAYER))  ! modification mathias décembre 2012

        ! Soil water potential is weighted by ESTEVAP.
        IF(TOTESTEVAP.GT.0.)THEN
            WEIGHTEDSWP = WEIGHTEDSWP / TOTESTEVAP       
        ELSE
            WEIGHTEDSWP = 0.0
            DO I=1,NROOTLAYER
                WEIGHTEDSWP = WEIGHTEDSWP + SOILWP(I) * LAYTHICK(I)
            ENDDO
            WEIGHTEDSWP = WEIGHTEDSWP / SUM(LAYTHICK(1:NROOTLAYER))
        
        ENDIF        

        ! Resistances are in parallel. This variable is also not used.
        RSUM = 0.0
        DO I=1,NROOTLAYER
            IF(SOILRRES(I).GT.0.0)THEN
                RSUM = RSUM + 1/SOILRRES(I)
            ENDIF
        ENDDO
        TOTSOILRES = 1/RSUM
        
        ENDIF

        ! Debugging option; equal uptake in all layers.
        ! Also used (for now) when USEMEASET=1.
        IF(EQUALUPTAKE.EQ.1)THEN
            FRACUPTAKE = 1.0 / REAL(NROOTLAYER)
            TOTSOILRES = SOILRRES1(1)
        ENDIF

        ! Option 2 (currently used) : 
        ! Taylor and Keppler: relative water uptake is
        ! proportional to root length density and Psi difference.
        ! See : Taylor, H.M. and B. Keppler. 1975. Water uptake by cotton root systems: 
        ! an examination of assumptions in the single root model. Soil Science. 120:57-67.
        DO I=1,NROOTLAYER
          IF(SUM(ESTEVAP(1:NROOTLAYER)).GT.0.)THEN
              SWPDIFF = MAX(0., (SOILWP(I)-MINROOTWP))
              FRACUPTAKE2(I) = ROOTLEN(I)*SWPDIFF
          ELSE 
              ! No water uptake possible.
              FRACUPTAKE2(I) = 0.
          ENDIF
        ENDDO
        
        IF(SUM(FRACUPTAKE2(1:NROOTLAYER)).GT.0)THEN
          FRACUPTAKE2 = FRACUPTAKE2 / SUM(FRACUPTAKE2(1:NROOTLAYER))  ! Make sure that it sums to 1.
        ELSE
          FRACUPTAKE2 = 0.0
        ENDIF
        
        ! If using measured soil water, replace with measurement.
        IF(USEMEASSW.EQ.1)THEN
            IF(SOILDATA.EQ.POTENTIAL)THEN
                WEIGHTEDSWP = SOILMOISTURE
                TOTSOILRES = SOILRRES1(1)
            ENDIF
        ENDIF

        ! Convert total soil to root resistance to leaf-specific resistance.
        IF(TOTLAI.GT.0.0)THEN
            TOTSOILRES = TOTSOILRES * TOTLAI
        ENDIF

        ! Error check.
        FRACSUM = SUM(FRACUPTAKE(1:NROOTLAYER))
        IF(FRACSUM.GT.0.AND.FRACSUM.LT.(1.0 - 1E-05))THEN
            WRITE(*,*)'Warning: FRACUPTAKE sum is',FRACSUM
        ENDIF

        ! Use Taylor-Keppler root water uptake distribution.
        ! Comment this line to use the SPA option ('Option 1' above).
        FRACUPTAKE = FRACUPTAKE2

        RETURN
        END

!**********************************************************************
        SUBROUTINE CANOPY_BALANCE(PPT,WIND,ZHT,Z0HT,ZPD,PRESSPA, &
                                  TAIRC,RNET,VPDPA,THROUGHFALL, &
                                  RUTTERB,RUTTERD,MAXSTORAGE, &
                                  CANOPY_STORE, SURFACE_WATERMM, &
                                  EVAPSTORE, DRAINSTORE)

! Determines canopy water storage (CANOPY_STORE) and water reaching the
! soil surface (SURFACE_WATERMM), by integrating the function CANSTOR.
! Taken from SPA, April 2008 (RAD)
!**********************************************************************

        USE maestcom
        IMPLICIT NONE
        INTEGER KMAX,KOUNT,NBAD,NOK,NVAR
        REAL X(KMAXX),Y(NMAX,KMAXX)
        REAL YSTART(2)
        REAL EXTRAPARS(EXTRAPARDIM)
        REAL MAXSTORAGE
        REAL PPT,WIND,ZHT,Z0HT,ZPD,PRESSPA,TAIRC,RNET
        REAL VPDPA,THROUGHFALL,RUTTERB,RUTTERD
        REAL CANOPY_STORE, SURFACE_WATERMM, EVAPSTORE
        REAL DRAINSTORE,X1,X2,HMIN,H1,EPS,DXSAV
        REAL CANSTORPREV,DELTASTORE
        
        EXTERNAL CANSTOR
        REAL, EXTERNAL :: RKQS
        COMMON /PATH/ KMAX,KOUNT,DXSAV,X,Y
        
        NVAR = 2

        EPS     = 1.0E-3  ! was 1.0E-4
        H1      = 0.00001    ! was 0.001
        HMIN    = 0.0
        KMAX    = 100
        X1      = 1.
        X2      = 2.
        DXSAV   = (X2-X1)/20.0

        EXTRAPARS(1)  = THROUGHFALL
        EXTRAPARS(2)  = RUTTERB
        EXTRAPARS(3)  = RUTTERD
        EXTRAPARS(4)  = MAXSTORAGE
        EXTRAPARS(5)  = PPT
        EXTRAPARS(6)  = WIND
        EXTRAPARS(7)  = ZHT
        EXTRAPARS(8)  = Z0HT
        EXTRAPARS(9)  = ZPD
        EXTRAPARS(10) = PRESSPA
        EXTRAPARS(11) = TAIRC
        EXTRAPARS(12) = RNET
        EXTRAPARS(13) = VPDPA

        ! Empty store if it's tiny.
        IF(CANOPY_STORE.LT.1E-6*MAXSTORAGE)CANOPY_STORE = 0.

        ! Store previous value of canstore:
        CANSTORPREV = CANOPY_STORE

        ! Initial conditions.
        YSTART(1) = CANOPY_STORE
        YSTART(2) = SURFACE_WATERMM

        ! Integrate the canopy storage diff. eq. (CANSTOR function).
        CALL ODEINT(YSTART,NVAR,X1,X2,EPS,H1,HMIN,NOK,NBAD,CANSTOR, &
                    EXTRAPARS)

        CANOPY_STORE = YSTART(1)
        SURFACE_WATERMM = YSTART(2)

        ! Get wet evaporation and canopy drainage
        DELTASTORE = CANOPY_STORE - CANSTORPREV
        DRAINSTORE = SURFACE_WATERMM - THROUGHFALL*PPT
        EVAPSTORE = (1-THROUGHFALL)*PPT - DELTASTORE - DRAINSTORE

        RETURN
        END


!**********************************************************************
        SUBROUTINE CANSTOR(TIME,Y,DYDT,EXTRAPARS)

! Determines canopy water storage and evaporation, and water reaching soil surface
! Taken from SPA, April 2008 (RAD)
!**********************************************************************

        USE maestcom
        IMPLICIT NONE
        INTEGER I
        REAL DYDT(NMAX),Y(NMAX)
        REAL EXTRAPARS(EXTRAPARDIM)
        REAL MAXSTORAGE,TIME
        REAL PPT,WIND,ZHT,Z0HT,ZPD,PRESSPA,TAIRC,RNET
        REAL VPDPA,THROUGHFALL,RUTTERB,RUTTERD
        REAL NUMMIN,ADDSTORE,ADDGROUND,RUTTERA
        REAL EVAPSTORE,DRAINSTORE
        
        THROUGHFALL = EXTRAPARS(1)
        RUTTERB     = EXTRAPARS(2)
        RUTTERD     = EXTRAPARS(3)
        MAXSTORAGE  = EXTRAPARS(4)
        PPT     = EXTRAPARS(5)
        WIND    = EXTRAPARS(6)
        ZHT     = EXTRAPARS(7)
        Z0HT    = EXTRAPARS(8)
        ZPD     = EXTRAPARS(9)
        PRESSPA = EXTRAPARS(10)
        TAIRC   = EXTRAPARS(11)
        RNET    = EXTRAPARS(12)
        VPDPA   = EXTRAPARS(13)

        ! minutes per timestep
        NUMMIN = SPERHR / 60

        ! rate of input of water to canopy storage per min
        ADDSTORE = (1 - THROUGHFALL) * PPT

        ! rate of input of water to ground
        ADDGROUND = THROUGHFALL * PPT

        ! Wet evaporation rate (returns EVAPSTORE).
        CALL WETEVAP(WIND,ZHT,Z0HT,ZPD, &
                           PRESSPA,TAIRC,RNET, &
                           VPDPA,Y(1),MAXSTORAGE, &
                           EVAPSTORE,PPT)

        ! Drainage from canopy store (Rutter et al. 1975)
        ! RUTTERB is in mm  min-1.
        RUTTERA = LOG(RUTTERD) - RUTTERB*MAXSTORAGE

        IF(Y(1).GT.MAXSTORAGE)THEN
            ! rate of drainage from store, mm t-1
            DRAINSTORE = REAL(EXP(RUTTERA + RUTTERB * Y(1))*NUMMIN)
        ELSE
            DRAINSTORE = 0.
        ENDIF

        ! Derivatives returned to ODEINT.
        ! change in canopy storage
        DYDT(1) = ADDSTORE - DRAINSTORE - EVAPSTORE

        ! addition to soilwater
        DYDT(2) = ADDGROUND + DRAINSTORE

        RETURN
        END


!**********************************************************************

        SUBROUTINE WETEVAP(WIND,ZHT,Z0HT,ZPD, &
                           PRESSPA,TAIRC,RNET, &
                           VPDPA,CANSTORE,MAXSTORAGE, &
                           EVAPSTORE,PPT)

! Penman-monteith equation without stomatal limitation
! (= Penman equation) for wet canopies.
! Modified from SPA version; now calls the maestra function ETCAN
! with infinite canopy conductance.
! RAD June 2008.
!**********************************************************************

        USE maestcom
        IMPLICIT NONE
        REAL MAXSTORAGE
        REAL STOCK,GCAN,EVAPSTORE,PPT
        REAL WIND,ZHT,Z0HT,ZPD
        REAL PRESSPA,TAIRC,RNET
        REAL VPDPA,CANSTORE,ETCAN
        REAL POTEVAPMUMOL,POTENTIALEVAP,RATIO

        STOCK = 1.   ! Dummy (to avoid unit conversion in ETCAN).
        GCAN = 1E09  ! an arbitrary large number (~Inf).

! Set VPD to near zero if it is raining (otherwise get very high wet evaporation rates).
        IF(PPT.GT.0.0)VPDPA = 1

! Potential evaporation from a wet canopy in mu mol m-2 s-1.
        POTEVAPMUMOL = ETCAN(WIND,ZHT,Z0HT,ZPD, &
                              PRESSPA,TAIRC,RNET, &
                              VPDPA,GCAN,STOCK)

! Convert to mm timestep-1.
        POTENTIALEVAP = POTEVAPMUMOL * SPERHR * 18 * 1E-09

! Modifier to potential ET: canopy storage / maximum storage (Rutter).
! rate of evaporation from storage nb store cannot exceed max storage
        RATIO = MIN(1.,CANSTORE/MAXSTORAGE)
        EVAPSTORE = POTENTIALEVAP * RATIO

        RETURN
        END
        
        
!**********************************************************************
      SUBROUTINE WETTINGLAYERS(POREFRAC,WETTINGBOT,WETTINGTOP, &
                               SURFACE_WATERMM,SNOW,SOILTK,QE,NLAYER, &
                               LAYTHICK,DRYTHICKMIN,DRYTHICK)

! Surface wetting and drying determines thickness of dry layer and thus Qe
! Taken from SPA, April 2008 (RAD). Code virtually unchanged from original.
!**********************************************************************

      USE maestcom
      IMPLICIT NONE
      INTEGER I,RR,AR1(1),AR2,AR1B,NLAYER
      REAL POREFRAC(NLAYER),WETTINGBOT(10),WETTINGTOP(10)
      REAL LAYTHICK(NLAYER)
      REAL LAMBDASOIL,NETC
      REAL SURFACE_WATERMM,SNOW,SOILTK,QE,DRYTHICKMIN
      REAL DRYTHICK,SOILTC,DIFF
      REAL, EXTERNAL :: HEATEVAP

      ! latent heat of vapourisation, j kg-1
      SOILTC = SOILTK - FREEZE
      LAMBDASOIL = HEATEVAP(SOILTC)

      ! from which wetting layer should soil LE be withdrawn? the one wi
      AR1 = MINLOC(WETTINGBOT,MASK = WETTINGBOT.GT.0.)
      AR1B = SUM(AR1)   ! convert ar1 to scalar

      ! what is the net change (NETC) in wetting in the top zone?
      ! Used below to determine whether top layer of soil is in a wettin
      NETC = (0.001*QE/LAMBDASOIL*SPERHR)/POREFRAC(1) + &
           (SURFACE_WATERMM*0.001+SNOW)/POREFRAC(1)   !M

      IF(NETC.GT.0.)THEN      ! Wetting
              ! Resaturate the layer if top is dry and recharge is great
              IF((NETC.GT.WETTINGTOP(AR1B)) .AND. &
                (WETTINGTOP(AR1B).GT.0.))THEN

                      ! extra water to deepen wetting layer
                      DIFF = NETC - WETTINGTOP(AR1B)
                      WETTINGTOP(AR1B)=0.

                      ! not in primary layer (primary layer can't extend
                      IF(AR1B.GT.1)THEN
                              WETTINGBOT(AR1B)=WETTINGBOT(AR1B)+DIFF
                      ENDIF
                      DRYTHICK=DRYTHICKMIN
              ELSE
                      ! surface is already wet, so extend depth of this
                      IF(WETTINGTOP(AR1B).EQ.0.)THEN

                             ! not in primary layer (primary layer can't
                             IF(AR1B.GT.1)THEN
                                      WETTINGBOT(AR1B) = &
                                         WETTINGBOT(AR1B) + NETC
                                      IF(WETTINGBOT(AR1B) .GE.WETTINGTOP(AR1B-1))  THEN
                                          ! layers are conterminous &
                                         WETTINGTOP(AR1B-1) = WETTINGTOP(AR1B)
                                             
                                         ! remove layer
                                         WETTINGTOP(AR1B)=0.
                                         WETTINGBOT(AR1B)=0.
                                      ENDIF
                              ENDIF
                      ELSE    ! or create a new wetting zone
                                WETTINGTOP(AR1B+1)=0.
                                WETTINGBOT(AR1B+1)=NETC
                        ENDIF
                        DRYTHICK=DRYTHICKMIN
                ENDIF
        ELSE    ! DRYING
                ! drying increases the wettingtop depth
                WETTINGTOP(AR1B)=WETTINGTOP(AR1B)-NETC

                ! wetting layer is dried out
                IF(WETTINGTOP(AR1B).GE.WETTINGBOT(AR1B))THEN

                        ! how much more drying is there?
                        DIFF=WETTINGTOP(AR1B)-WETTINGBOT(AR1B)
                        WETTINGTOP(AR1B)=0.
                        WETTINGBOT(AR1B)=0.
                        AR2=AR1B-1

                        ! move to deeper wetting layer
                        IF(AR2.GT.0)THEN

                           ! dry out deeper layer
                           WETTINGTOP(AR2)=WETTINGTOP(AR2)+DIFF
                           DRYTHICK=MAX(DRYTHICKMIN,WETTINGTOP(AR2))
                        ELSE    ! no deeper layer
                           ! layer 1 is dry
                           DRYTHICK = LAYTHICK(1)
                        ENDIF
                ELSE
                        DRYTHICK = MAX(DRYTHICKMIN,WETTINGTOP(AR1B))
                ENDIF
        ENDIF

        RETURN
        END


!**********************************************************************

        REAL FUNCTION QEFLUX(SOILTK,TAIRK,VPDKPA,POREFRAC1,SOILWP1, &
                             GAMSOIL, PRESSPA, DRYTHICK, TORTPAR)

! Latent energy loss from soil surface based on Choudhury and Monteith
! (1988, Quart. J. Roy. Met. Soc.). Evaporation is assumed to occur at
! the wet surface in the soil, after which diffusion occurs through
! the overlaying dry soil. The thickness of this dry soil layer thus
! determines soil evaporation rate.
! Note that QE < 0 when evaporation occurs!
! Taken from SPA, April 2008 (RAD)
!**********************************************************************

        USE maestcom
        IMPLICIT NONE
        REAL LAMBDASOIL
        REAL SOILTK,TAIRK,VPDKPA,POREFRAC1,SOILWP1
        REAL GAMSOIL, PRESSPA, DRYTHICK, TORTPAR
        REAL TAIRC,SOILTC,PRESSKPA,RHO,ESAT,EA,ESURF
        REAL DIFF,EFFDIFF,GWS,GWSTOT
        REAL, EXTERNAL :: HEATEVAP
        REAL, EXTERNAL :: RHOFUN
        REAL, EXTERNAL :: SATUR

        ! Conversions
        TAIRC = TAIRK - FREEZE
        SOILTC = SOILTK - FREEZE
        PRESSKPA = PRESSPA * 1E-03

        ! Latent heat of vaporisation (j kg-1)
        LAMBDASOIL = HEATEVAP(SOILTC)

        ! Density of air (kg m-3)
        RHO = RHOFUN(TAIRK)

        ! Saturation vapour pressure of air (kPa)
        ESAT = SATUR(TAIRC) / 1000

        ! Vapor pressure of air (kPa)
        EA = ESAT - VPDKPA

        ! Saturation vapour pressure at surface (kPa)
        ESAT = SATUR(SOILTC) / 1000

        ! Vapor pressure in soil airspace (kpa) depends on soil water po
        ! See Jones 1992 p.110 (Eq. 5.11).
        ESURF = ESAT*EXP(1E6*SOILWP1*H2OVW/(RCONST*SOILTK))

        ! Diffusion coefficient for water vapor (m2 s-1)
        ! Jones 1992, Appendix 2.
        DIFF = 24.2E-06 * (SOILTK/293.2)**1.75

        ! Effective diffusion coefficient, adjusted for air space fracti
        ! See Hillel 1998, p.295-296 (Penman 1940 equation, giving the 0
        ! Note that in our case, the airspace is equal to the total poro
        ! because we are considering the diffusion in the dry layer only
        ! This correction is close to SPA's TORT=2.5.
        EFFDIFF = DIFF * TORTPAR * POREFRAC1

        ! Soil conductance to water vapour diffusion, m s-1.
        ! Choudhury and Monteith (1988), Eq. 41b.
        GWS = EFFDIFF * (POREFRAC1 / DRYTHICK)

        ! Total conductance.
        GWSTOT = 1. / (1./GAMSOIL + 1./GWS)

        ! Latent energy flux from soil surface (Choudhury and Monteith 1
        QEFLUX = GWSTOT * LAMBDASOIL * (RHO / PRESSKPA) * (H2OMW/AIRMA) &
                 * (EA - ESURF)

        ! Turn off potential dew formation here if QEFLUX is larger than
        ! Problems arise when soil water potential in top layer is very
        IF(QEFLUX.GT.0.)QEFLUX = 0.

        ! No evap if surface is frozen.
        ! RAD: what about evaporation of ice? This may be in the THAWDEP
        IF(SOILTK.LT.FREEZE)QEFLUX=0.

        RETURN
        END



!**********************************************************************
        SUBROUTINE SOIL_BALANCE(J, POREFRAC,ICEPROP,FRACWATER, &
                                LAYTHICK, &
                                DRAINLIMIT, WATERLOSS, WATERGAIN, &
                                KSAT, BPAR,WS,WR,NRET,RETFUNCTION)

! Integrator for soil gravitational drainage.
! J is the current soil layer.
! Taken from SPA, April 2008 (RAD)
!**********************************************************************

        USE maestcom
        IMPLICIT NONE
        INTEGER NVAR,J
        INTEGER KMAX,KOUNT,NBAD,NOK
        INTEGER RETFUNCTION
        REAL DXSAV,EPS,H1,HMIN,X(KMAXX),Y(NMAX,KMAXX),X1,X2
        REAL YSTART(1),KSAT,BPAR,LIQUID,UNSAT,NEWWF,CHANGE
        REAL WS,WR,NRET
        REAL POREFRAC(MAXSOILLAY),ICEPROP(MAXSOILLAY)
        REAL LAYTHICK(MAXSOILLAY)
        REAL FRACWATER(MAXSOILLAY)
        REAL WATERLOSS(MAXSOILLAY), WATERGAIN(MAXSOILLAY)
        REAL EXTRAPARS(EXTRAPARDIM)   ! Soil parameters to be passed to ODEINT.
        REAL DRAINLIMIT
        
        ! Names of functions/subroutines to be passed to ODEINT
        EXTERNAL SOILSTOR
        
        ! I probably want to get rid of common blocks,
        ! instead make these pars in maestcom???
        COMMON /PATH/ KMAX,KOUNT,DXSAV,X,Y
        
        NVAR = 1
        
        EPS=1.0E-4
        H1=.001
        HMIN=0.0
        KMAX=100
        X1=1.
            X2=2.
        DXSAV=(X2-X1)/20.0

        ! Liquid fraction
        LIQUID = FRACWATER(J)*(1.-ICEPROP(J))

        ! Unsaturated volume of layer below, m3 m-2
        UNSAT = MAX(0.,(POREFRAC(J+1) - FRACWATER(J+1)) * &
             LAYTHICK(J+1)/LAYTHICK(J))

        ! Array of parameters to be passed to ODEINT
        EXTRAPARS(1) = KSAT
        EXTRAPARS(2) = BPAR
        EXTRAPARS(3) = POREFRAC(J)
        EXTRAPARS(4) = DRAINLIMIT
        EXTRAPARS(5) = LIQUID
        EXTRAPARS(6) = UNSAT
        EXTRAPARS(7) = WS
        EXTRAPARS(8) = WR
        EXTRAPARS(9) = NRET 
        EXTRAPARS(10)= RETFUNCTION

        ! If there is liquid water, integrate the drainage routine.
        IF((LIQUID.GT.0.).AND.(FRACWATER(J).GT. &
                          DRAINLIMIT*POREFRAC(J)))THEN

                ! Initial value of volumetric water content for layer J.
                YSTART(1) = FRACWATER(J)

                ! Integrate (Note this passes the function SOILSTOR,
                ! and an array of parameters EXTRAPARS)
                CALL ODEINT(YSTART,NVAR,X1,X2,EPS,H1,HMIN,NOK, &
                            NBAD,SOILSTOR,EXTRAPARS)
                !Solution of odeint ('new water fraction')
                NEWWF = YSTART(1)

                ! Convert from waterfraction to absolute amount (m)
                CHANGE = (FRACWATER(J) - NEWWF) * LAYTHICK(J)
                WATERGAIN(J+1) = WATERGAIN(J+1) + CHANGE
                WATERLOSS(J) = WATERLOSS(J) + CHANGE

        ENDIF
        
        RETURN
        END


!**********************************************************************
        SUBROUTINE SOILSTOR(TIME,Y,DYDT,EXTRAPARS)

! Determines gravitational water drainage
! Taken from SPA, April 2008 (RAD)
!**********************************************************************

        USE maestcom
        IMPLICIT NONE
        INTEGER I, RETFUNCTION
        REAL DYDT(NMAX),Y(NMAX)
        REAL TIME
        REAL DRAINAGE,KSAT,LIQUID,KSOIL
        REAL EXTRAPARS(EXTRAPARDIM)
        REAL BPAR,SOILPOR,DRAINLIMIT,UNSAT,WS,WR,NRET
        REAL, EXTERNAL :: SOILCONDFUN

        ! Extra parameters in an array:
        KSAT = EXTRAPARS(1)
        BPAR = EXTRAPARS(2)
        SOILPOR = EXTRAPARS(3)
        DRAINLIMIT = EXTRAPARS(4)
        LIQUID = EXTRAPARS(5)
        UNSAT = EXTRAPARS(6)
        WS = EXTRAPARS(7)
        WR=EXTRAPARS(8)
        NRET=EXTRAPARS(9)
        RETFUNCTION = EXTRAPARS(10)

        ! Soil hydraulic conductivity and drainage.
        KSOIL = SOILCONDFUN(Y(1),KSAT,BPAR,SOILPOR,WS,WR,NRET,RETFUNCTION)
        DRAINAGE = KSOIL*SPERHR

        ! Gravitational drainage above a given percentage of porosity.
        IF(Y(1).LE.DRAINLIMIT*SOILPOR)THEN
                DRAINAGE=0.
        ENDIF

        ! Ice does not drain.
        IF(DRAINAGE.GT.LIQUID)THEN
                DRAINAGE = LIQUID
        ENDIF

        ! Layer below cannot accept more water than unsat.
        IF(DRAINAGE.GT.UNSAT)THEN
                DRAINAGE = UNSAT
        ENDIF

        ! Waterloss from this layer
        DYDT(1)= -DRAINAGE

        RETURN
        END


!**********************************************************************
        SUBROUTINE INFILTRATE(SURFACE_WATERMM,NLAYER,POREFRAC, &
                               FRACWATER,LAYTHICK,WATERGAIN,WATERLOSS, &
                               EXPINF,PPTGAIN,OVERFLOW)

! Takes surface_watermm and distrubutes it among top layers, allowing
! for a proportion of the total.
! Assumes total infiltration in timestep (no ponding).
! Taken from SPA, May 2008 (RAD)
!**********************************************************************

        USE maestcom
        IMPLICIT NONE
        INTEGER I,NLAYER
        REAL POREFRAC(MAXSOILLAY),LAYTHICK(MAXSOILLAY)
        REAL FRACWATER(MAXSOILLAY),PPTGAIN(MAXSOILLAY)
        REAL WATERLOSS(MAXSOILLAY), WATERGAIN(MAXSOILLAY)
        REAL FRACIN(MAXSOILLAY)
        REAL SURFACE_WATERM,SURFACE_WATERMM,SOILDEPTH,EXPINF
        REAL CHECKSUM,WLEFTOVER,WATINF,WDIFF,FLUXIN
        REAL OVERFLOW,DIFF

        ! Reset PPTGAIN (infiltration of rainwater).
        PPTGAIN = 0.

        ! Water for infiltration (snow melt + throughfall) in m
        SURFACE_WATERM = SURFACE_WATERMM * 1E-03

        ! Infiltration a la BROOK90, subroutine INFPAR.
        ! "Federer, C.A. 2002. BROOK 90: A simulation model for evaporation
        ! soil water, and streamflow. http://home.roadrunner.com/~stfederer/brook/brook90.htm
        ! Calculates fraction of surface water (i.e. rainfall) going int
        SOILDEPTH = SUM(LAYTHICK(1:NLAYER))
        FRACIN(1) = ( LAYTHICK(1) / SOILDEPTH ) ** EXPINF

!        FRACIN(1) = 0.3
!        FRACIN(2) = 0.2
!        FRACIN(3) = 0.2
!        FRACIN(4) = 0.2
!        FRACIN(5) = 0.1
        
        DO I=2,NLAYER
           FRACIN(I) = ( SUM(LAYTHICK(1:I)) / SOILDEPTH ) ** EXPINF - &
                       ( SUM(LAYTHICK(1:(I-1))) / SOILDEPTH) ** EXPINF
        ENDDO

        
        ! Error check:
        CHECKSUM = SUM(FRACIN(1:NLAYER))
        IF(ABS(CHECKSUM - 1.0).GT.1E-06)WRITE(*,*) &
                   'WARNING: FRACIN does not sum to one'

        ! Initialize pool of water that does not fit, and gets carried t
        WLEFTOVER = 0.0

        ! Do infiltration, accounting for whether water actually fits in
        DO I=1,NLAYER

              !  Water for this layer: infiltration plus upper layer lef
              WATINF = FRACIN(I) * SURFACE_WATERM !+ WLEFTOVER

              ! Meters of water that can fit in current soil layer
              WDIFF = MAX(0.,(POREFRAC(I) - FRACWATER(I))*LAYTHICK(I) - &
                 WATERGAIN(I) + WATERLOSS(I))

              ! Put all infiltration in top layer, unless it does not fi
              FLUXIN = WATINF+WLEFTOVER
              WLEFTOVER = 0.0

              IF(FLUXIN.GT.WDIFF)THEN
                    PPTGAIN(I) = WDIFF
                    WLEFTOVER = WLEFTOVER + (FLUXIN - WDIFF)
              ELSE
                    PPTGAIN(I) = FLUXIN
              ENDIF

        ENDDO

        
        ! There may still be water left over; soil is saturated.
        OVERFLOW = WLEFTOVER

        ! Check whether PPTGAIN and SURFACE_WATERM are equal
        DIFF = SURFACE_WATERM - SUM(PPTGAIN) - OVERFLOW

        RETURN
        END

!**********************************************************************
        SUBROUTINE WATERTHERMAL(NLAYER,FRACWATER, POREFRAC, &
                                SOILTEMP, LAYTHICK, WATERGAIN, &
                                TAIRK, WATERLOSS, PPTGAIN, &
                                VOLHC)

! Redistribute heat according to water movement in the soil.
! SPA subroutine also did water balance updating, this is now done in
! subroutine WATBALLAY.
! Output (and input) is the array SOILTEMP (K), VOLHC is output.

! Taken from SPA (modified), RAD (2008).
!**********************************************************************

        USE maestcom
        IMPLICIT NONE
        INTEGER I,NLAYER
        REAL FRACWATER(MAXSOILLAY),POREFRAC(MAXSOILLAY)
        REAL SOILTEMP(MAXSOILLAY),LAYTHICK(MAXSOILLAY)
        REAL WATERGAIN(MAXSOILLAY), WATERLOSS(MAXSOILLAY)
        REAL PPTGAIN(MAXSOILLAY), VOLHC(MAXSOILLAY)
        REAL NEWHEAT,FRACWATEROLD,OLDVOLHC,HEAT
        REAL HEATLOSS,HEATGAIN,TAIRK
        REAL, EXTERNAL :: VOLHCFUN

        DO I = 1,NLAYER

              ! Previous water fraction (note that, in contrast to SPA,
              ! already updated here!). For correct calculation of heat
              ! rainfall and drainage, use previous and current FRACWATE
              FRACWATEROLD = FRACWATER(I) - (PPTGAIN(I) + WATERGAIN(I) - &
                                             WATERLOSS(I))/LAYTHICK(I)

              ! Volumetric heat capacity of previous timestep, and curre
              OLDVOLHC = VOLHCFUN(POREFRAC(I), FRACWATEROLD)
              VOLHC(I) = VOLHCFUN(POREFRAC(I), FRACWATER(I))

              ! Heat content of the layer (J m-2).
              HEAT = SOILTEMP(I) * OLDVOLHC * LAYTHICK(I)

              ! Heat loss : water loss in m * heat capacity of water * t
              HEATLOSS = WATERLOSS(I) * CPH2O * SOILTEMP(I)

              ! Heat gain: water gain in m * heat capacity of water * te
              HEATGAIN = WATERGAIN(I) * CPH2O * SOILTEMP(I) + &
                             PPTGAIN(I) * CPH2O * TAIRK

              ! Net redistribution of heat (IF clause deleted here).
              NEWHEAT = HEAT - HEATLOSS + HEATGAIN

              ! New soil temperature.
              SOILTEMP(I) = NEWHEAT / (VOLHC(I) * LAYTHICK(I))
 
        ENDDO

        RETURN
        END

!**********************************************************************
        SUBROUTINE CRANKNICHOLS(NLAYER, LAYTHICK, SOILTK, &
                                SOILTEMP, VOLHC, THERMCOND)

! Finite difference PDE solver for soil temperature profile.
! SOILTK is soil *surface* temperature, SOILTEMP array of soil
! temperatures with depth.
! Taken from SPA, June 2008 (RAD).
!**********************************************************************

         USE maestcom
         IMPLICIT NONE
         INTEGER I,ITER_NO,NLAYER
         REAL SOILTEMP(MAXSOILLAY), VOLHC(MAXSOILLAY)
         REAL THERMCOND(MAXSOILLAY), SOILTEMP_NPLUS1(MAXSOILLAY)
         REAL LAYTHICK(MAXSOILLAY)
         REAL MAX_ERROR,BETA,SOILTK,ERROR
         REAL TDIFFUSE,D,OLD_VALUE

         ITER_NO = 0
         MAX_ERROR = 0.0000005
         BETA = 0.5

! Initialize soiltemp at t+1.
         SOILTEMP_NPLUS1 = 0.
         SOILTEMP(1) = SOILTK
         SOILTEMP_NPLUS1(1) = SOILTK
         SOILTEMP_NPLUS1(NLAYER + 1) = SOILTEMP(NLAYER + 1)

         ERROR = 1.
         DO WHILE (ERROR.GT.MAX_ERROR)
             ERROR = 0.  ! reset error
             I = 2

           ! loop for all x-dimension nodes, except first and last.
           DO WHILE (I .LT. (NLAYER+1))

           ! Thermal conductivity, w m-1 k-1 is converted to j m-1 k-1 t
           TDIFFUSE = SPERHR * THERMCOND(I) / VOLHC(I)
           D = TDIFFUSE / LAYTHICK(I)**2

           ! Store value of previous iteration.
           OLD_VALUE = SOILTEMP_NPLUS1(I)

                 ! Calculate the temperature at the new time step using
                 SOILTEMP_NPLUS1(I) = (D / (1 + 2*BETA*D)) &
                                 * (BETA*(SOILTEMP_NPLUS1(I+1) &
                                 + SOILTEMP_NPLUS1(I-1)) &
                                 + (1-BETA) * (SOILTEMP(I+1) &
                                 - 2*SOILTEMP(I) + SOILTEMP(I-1))) &
                                 +  SOILTEMP(I)/(1+2*BETA*D)

                 ! calculate the error.
                 ERROR = ERROR + ABS(OLD_VALUE - SOILTEMP_NPLUS1(I))

             I=I+1
             ENDDO

         ENDDO

         ! Loop for all x-dimension nodes, except first and last.
         ! set the values at time n equal to the values at
         ! time  t+1 for the next time step.
         DO I= 2,NLAYER
             SOILTEMP(I) = SOILTEMP_NPLUS1(I)
         ENDDO

         RETURN
         END


!**********************************************************************

        REAL FUNCTION THERMCONDFUN(I, SOILWP, FRACWATER, &
                                   POREFRAC, BPAR, FRACORGANIC,RETFUNCTION)

! (Differs from SPA - RAD June 2008).
!**********************************************************************

        USE maestcom
        IMPLICIT NONE
        INTEGER RETFUNCTION
        REAL KE,ALPHA,BPAR,DRYLAMBDA,POREFRAC,TCSOLID
        REAL FRACORGANIC,WETLAMBDA,SR,FRACWATER,SOILWP
        INTEGER I

! Seemed to give unrealistically high values in wet soils (see also
! Peters-Lidard et al. 1998 (J. Atmosph. Sci.).
!C Soil thermal conductivity from McCumber and Pielke (1981, JGR),
!C see also e.g. Xinmei and Lyans (1995, JAppMet).
!C       For conversion of soil water potential from MPa to pF.
!        CONV = -GRAV * 1E-05
!
!        PF = LOG10(SOILWP / CONV)
!
!        IF(PF.LE.5.1) THEN
!           THERMCONDFUN = 418 * EXP(-(PF+2.7))
!        ELSE
!           THERMCONDFUN = 0.171
!        ENDIF


! Lu et al 2007 (SSSAJ). They test their model against lots of data, and base it
! on an earlier model by Johansson (1975). See also Peters-Lidard et al. 1998 (J.
! Atm. Sci.) for support of the Johansson model.

    IF (RETFUNCTION.EQ.3) THEN
        ALPHA = 0.96  ! waiting more information
    ELSE
! The alpha parameter is different for coarse and fine soils. I have set the cutoff
! to 5.3 for the b parameter, based on their Table 1 and Figure 2.
        IF(BPAR.LT.5.3)THEN
            ALPHA = 0.96
        ELSE
            ALPHA = 0.27
        ENDIF
    END IF
    
! Saturated and dry thermal conductivities:
        DRYLAMBDA = -0.56 * POREFRAC + 0.51

! TC of solids, for now only quartz and organic matter.
! How about 'other' mineral constituents? Lu et al., Eq. 5.
        TCSOLID = TCORG**FRACORGANIC * TCQUARTZ**(1-FRACORGANIC)
        WETLAMBDA = TCSOLID**(1-POREFRAC) * TCH2O**POREFRAC

! Unsaturated component
        SR = FRACWATER / POREFRAC
        KE = EXP(ALPHA*(1-SR**(ALPHA-1.33)))

! Combined (W m-1 K-1).
        THERMCONDFUN = (WETLAMBDA - DRYLAMBDA)*KE + DRYLAMBDA

        RETURN
        END

!**********************************************************************

      REAL FUNCTION VOLHCFUN(POREFRAC,FRACWATER)

! Volumetric heat capacity of soil layer (J m-3 K-1).
! From De Vries 1963 (see also Ogee et al 2001 AFM; Hillel 1998 p. 315).
! (Note: SPA had a correction for organic content).
!**********************************************************************

      USE maestcom
      IMPLICIT NONE
      REAL POREFRAC,FRACWATER

      VOLHCFUN = CPQUARTZ * (1 - POREFRAC) + CPH2O * FRACWATER

      RETURN
      END



!**********************************************************************

      SUBROUTINE SCALEUP(IHOUR,USESTAND,NOTARGETS,NOALLTREES,FOLT, &
                         ITARGETS,ISPECIES,NOSPEC,TOTLAI,STOCKING,SCLOSTTREE, &
                         THRAB,RADABV,FH2O,PLOTAREA,DOWNTHTREE, &
                         RGLOBABV,RGLOBUND,RADINTERC,FRACAPAR,&
                         ISIMUS,FH2OUS,THRABUS,PARUSMEAN, &
                         SCLOSTTOT,GSCAN,WIND,ZHT,Z0HT,ZPD, &
                         PRESS,TAIR,VPD,ETMM,ETUSMM,ETMMSPEC)

! Scale up individual tree transpiration and radiation interception to
! a per m2 basis for use in water/heat balance calculations.
! For global radiation above and underneath the canopy, GETRGLOB is called.
! RAD, Sept. 2008
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER NOTARGETS,NOALLTREES,ITAR
    INTEGER ITARGETS(MAXT),I,IHOUR,ISIMUS
    INTEGER USESTAND,ISPECIES(MAXT),NOSPEC,ISPEC
    REAL FOLT(MAXT),EXPFACTORS(MAXT)
    REAL SCLOSTTREE(MAXT,3),GSCAN(MAXT,MAXHRS)
    REAL THRAB(MAXT,MAXHRS,3),RADABV(MAXHRS,3)
    REAL DOWNTHTREE(MAXT)
    REAL FH2O(MAXT,MAXHRS),PLOTAREA,TOTSPECET
    REAL TOTLATAR,TREELAMEAN,ALLTREELAMEAN,TOTLAI
    REAL STOCKING,THRABUS,PARUSMEAN
    REAL RGLOBABV,RGLOBABV12,RGLOBUND,RADINTERC12,RADINTERC
    REAL RADINTERC1,RADINTERC2,RADINTERC3,SCLOSTTOT
    REAL FRACAPAR,GSCANAV,RADINTERCTREE,CONV
    REAL ETMM,  WIND, ZHT,Z0HT,ZPD,PRESS,TAIR,VPD,ETCAN
    REAL ETUSMM,FH2OUS,WTOT,ETMMSPEC(MAXSP)
    
! Get average leaf area of target trees
      TOTLATAR = 0.0
      DO I = 1,NOTARGETS
          TOTLATAR = TOTLATAR + FOLT(I)
      ENDDO
      TREELAMEAN = TOTLATAR / REAL(NOTARGETS)

! Get average leaf area of all trees in the stand:
      ALLTREELAMEAN = TOTLAI / STOCKING

! If USESTAND=1, uses entire stand to determine water balance (not just target trees).
      IF(USESTAND.GT.0)THEN
          IF(TREELAMEAN.GT.0)THEN
    ! Expansion factors (each target tree represents X trees in the stand)
           EXPFACTORS(1:NOTARGETS) = (ALLTREELAMEAN / TREELAMEAN) * &
                                      (NOALLTREES / NOTARGETS)
          ELSE
           EXPFACTORS(1:NOTARGETS) = 0.0
          ENDIF
      ELSE
          EXPFACTORS(1:NOTARGETS) = 1.0
      ENDIF

! If multiple species, calculate ET by species. If USESTAND, this is used to divide total recalculated
! ET into the species (an approximate method anyway!), if USESTAND=1, it is the final result for ETMMSPEC.
      IF(NOSPEC.GT.1)THEN
          
        DO ISPEC=1,NOSPEC
            
            WTOT = 0.0
            DO I=1,NOTARGETS
              IF(ISPECIES(ITARGETS(I)).EQ.ISPEC)THEN
                 WTOT = WTOT + FH2O(I,IHOUR)
              ENDIF
              
            ENDDO
            
            ETMMSPEC(ISPEC) = WTOT * SPERHR * 1E-06 * 18 * 1E-03 / PLOTAREA
        
        ENDDO
      
      ENDIF
      
! Get total radiation above and under the canopy.
      CALL GETRGLOB(IHOUR,SCLOSTTREE,THRAB,RADABV, &
                      NOTARGETS,NOALLTREES,PLOTAREA, &
                      ISIMUS,THRABUS,PARUSMEAN, &
                      DOWNTHTREE,EXPFACTORS, &
                      RGLOBABV,RGLOBABV12,RGLOBUND,RADINTERC12, &
                      RADINTERC1,RADINTERC2,RADINTERC3, &
                      SCLOSTTOT,FRACAPAR,RADINTERC)
                           
! Get average canopy conductance across target trees:
      GSCANAV = 0.
      DO ITAR = 1,NOTARGETS
          GSCANAV = GSCANAV + FOLT(ITAR) * GSCAN(ITAR,IHOUR)
      ENDDO
      GSCANAV = GSCANAV / TOTLATAR
      

! Recalculate canopy water use, if scaling to the entire stand.
      IF(USESTAND.GT.0)THEN

! Estimate average conductance for all trees, based on
! leaf area difference:
        IF(TREELAMEAN.GT.0)THEN
            GSCANAV = GSCANAV * (ALLTREELAMEAN / TREELAMEAN)
        ELSE
            GSCANAV = 0.0
        ENDIF

! Total radiation interception from GETRGLOB was in W m-2 (soil),
! convert to per tree (for use in ETCAN):
        RADINTERCTREE = RADINTERC / STOCKING
        ! Note that this is inconsequential : gets converted back in ETCAN

! Conversion to kg m-2 t-1.
        CONV = SPERHR * 1E-06 * 18 * 1E-03
        ETMM = ETCAN(WIND,ZHT,Z0HT,ZPD, &
            PRESS,TAIR, &
            RADINTERCTREE,   &
            VPD,GSCANAV,STOCKING) * CONV
        
! Recalculate ETMMSPEC to arrive at same total; this is an approximate method to apportion
! total ET into species components.
        IF(NOSPEC.GT.1)THEN
            TOTSPECET = SUM(ETMMSPEC(1:NOSPEC))
            IF(TOTSPECET.GT.0.0)THEN
                DO I=1,NOSPEC
                    ETMMSPEC(I) = ETMMSPEC(I) * ETMM / TOTSPECET
                ENDDO
            ENDIF
        ELSE
            ETMMSPEC(1) = ETMM
            
        ENDIF
        
      ENDIF
    
! Alternatively, do water balance only based on the target trees (no scaling to stand).
      IF(USESTAND.EQ.0) THEN
      
        ! Total water use, based on FH2O (not recalculated!)
          WTOT = 0.0
          DO I=1,NOTARGETS
              WTOT = WTOT + FH2O(I,IHOUR)
          ENDDO
        
          ! Simple conversion
          ETMM = WTOT * SPERHR * 1E-06 * 18 * 1E-03 / PLOTAREA
      
      ENDIF
      
      
! Add understorey ET to ETMM, if simulated:
      IF(ISIMUS.EQ.1)THEN
          ETUSMM = FH2OUS * SPERHR * 18 * 1E-06
          ETMM = ETMM + ETUSMM
      ENDIF
    
      RETURN
      END


!**********************************************************************

      SUBROUTINE ASSIGNSOILWATER(WSOILMETHOD,USEMEASSW, &
                                 SWMIN, SWMAX, &
                                 SOILMOIST, WSOILROOT, SOILDEPTH, &
                                 SOILDATA, SOILMOISTURE)

! Assign soil water content, depending on parameters
! WSOILMETHOD and USEMEASSW
!**********************************************************************

        USE maestcom
        USE metcom
        IMPLICIT NONE
        INTEGER WSOILMETHOD,USEMEASSW,SOILDATA,IOERROR
        REAL SWMIN,SWMAX,SOILMOIST,WSOILROOT,SOILDEPTH
        REAL SOILMOISTURE

        ! Use soil water data, don't simulate water balance,
        ! but soil water not available in met.dat file: error.
        IF(SOILDATA.EQ.NONE.AND.USEMEASSW.EQ.1)THEN
          CALL SUBERROR( &
          'Soil water not available, set USEMEASSW to 0 in watpars.dat', &
          IFATAL,IOERROR)
        ENDIF

        ! Don't simulate water balance, don't use measured soil water:
        ! Option not possible, because fixed in INITWATBAL.

        ! Simulate water content, don't use measurements.
        IF(USEMEASSW.EQ.0)THEN
            SOILMOISTURE = WSOILROOT / SOILDEPTH / 1000
            SOILDATA = SIMULATED
        ENDIF

        ! Simulate water content, but do use measurements.
        IF(USEMEASSW.EQ.1)THEN
            SOILMOISTURE = SOILMOIST
        ENDIF

        ! Use defict method to calculate FSOIL, but data is in volumetri
        IF(WSOILMETHOD.EQ.4.AND.SOILDATA.NE.DEFICIT)THEN
            IF(SOILDATA.EQ.POTENTIAL)THEN
           CALL SUBERROR( &
          'I cannot convert soil water potential to deficit.', &
          IFATAL,IOERROR)
            ENDIF
            IF(SOILDATA.EQ.CONTENT.OR.SOILDATA.EQ.SIMULATED)THEN
                SOILMOISTURE = (SWMAX - SOILMOIST) / (SWMAX - SWMIN)
            ENDIF
        ENDIF

        RETURN
        END


!**********************************************************************

      SUBROUTINE SUMDAILYWAT(WSOIL,WSOILROOT,WEIGHTEDSWP,PPT,ETMM,ETMEAS, &
                       DISCHARGE,SOILEVAP,FSOIL1,SURFACE_WATERMM, &
                       QH,QE,QN,QC,RADINTERC, &
                       WSOILMEAN,WSOILROOTMEAN,SWPMEAN,PPTTOT,ETMMTOT,ETMEASTOT, &
                       DISCHARGETOT,SOILEVAPTOT,FSOILMEAN,TFALLTOT, &
                       QHTOT,QETOT,QNTOT,QCTOT,RADINTERCTOT)

! Make daily water balance output file.
!**********************************************************************

      USE maestcom
      IMPLICIT NONE
      REAL ETMEAS(MAXHRS),PPT(MAXHRS)
      REAL WSOIL,WSOILROOT,ETMM
      REAL DISCHARGE,SOILEVAP,FSOIL1,SURFACE_WATERMM
      REAL QH,QE,QN,QC,RADINTERC
      REAL WSOILMEAN,WSOILROOTMEAN,PPTTOT,ETMMTOT,ETMEASTOT
      REAL DISCHARGETOT,SOILEVAPTOT,FSOILMEAN,TFALLTOT
      REAL QHTOT,QETOT,QNTOT,QCTOT,RADINTERCTOT
      REAL CONVERT,SWPMEAN,WEIGHTEDSWP

      ! Sum fluxes that are hourly arrays:
      ETMEASTOT = SUM(ETMEAS(1:KHRS))
      PPTTOT = SUM(PPT(1:KHRS))

      WSOILMEAN = WSOILMEAN + WSOIL  / REAL(KHRS)
      WSOILROOTMEAN = WSOILROOTMEAN + WSOILROOT / REAL(KHRS)
      SWPMEAN = SWPMEAN + WEIGHTEDSWP / REAL(KHRS)
      ETMMTOT = ETMMTOT + ETMM
      DISCHARGETOT = DISCHARGETOT + DISCHARGE
      SOILEVAPTOT = SOILEVAPTOT + SOILEVAP
      FSOILMEAN = FSOILMEAN + FSOIL1 / REAL(KHRS)
      TFALLTOT = TFALLTOT + SURFACE_WATERMM

      ! From W m-2 to MJ m-2 day-1.
      CONVERT = 1E-06 * 24*60*60 / REAL(KHRS)
      QHTOT = QHTOT + CONVERT*QH
      QETOT = QETOT + CONVERT*QE
      QNTOT = QNTOT + CONVERT*QN
      QCTOT = QCTOT + CONVERT*QC
      RADINTERCTOT = RADINTERCTOT + CONVERT*RADINTERC

      RETURN
      END


