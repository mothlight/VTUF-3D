PROGRAM maespa
    ! =======================================================
    !  MAESPA: Version 2013
    !  For more information see www.bio.mq.edu.au/maespa 
    ! =======================================================

    !------------------------------------------------------------------------
    ! This file contains the main program.
    ! It also contains subroutines to zero arrays (ZEROHR, ZEROHIST, ZEROD)
    ! and to sum arrays (SUMHR, SUMDAILY).
    ! All other code is contained in the additional files: -
    !   radn.for - calculation of radiation interception
    !   physiol.for - physiology subroutines
    !   getmet.for - read in met data
    !   inout.for - handle input & output data
    !   utils.for - utility subroutines
    !   unstor.for - understorey calculations
    !   watbal.for -  water balance and soil surface energy budget calculations.
    !------------------------------------------------------------------------

    !-----------------------------------------------------------------------
    !     An array model to  predict radiation regime inside a plant
    !  community. Especially suited to forest stands where crown shape
    !  is described using a cone or elipsoid.
    !     This program consists of three main parts, i.e.
    !  1: radiation in the atmosphere , daylength and the position of
    !     the sun at different hours of the day;
    !  2: plant canopy architecture.3-D leaf area distribution and random
    !     and nonrandom leaf area dispersion inside crown has been incorporated
    !     into the radiation model with the spherical leaf angular distribution
    !     assumption;radiation penetration and scattering processes.
    !     A multi-scattering model proposed by John Norman is used.
    !  3: both photosynthesis and transpiration are calculated using the
    !     combined gs-A-E-H model.
    !  4: Water balance based on SPA routines: root water uptake, soil evaporation,
    !     drainage, overflow, rainfall interception, soil heat balance (RAD).
    !------------------------------------------------------------------------
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
                    FALPHATABLESPEC,COEFFTSPEC,EXPONTSPEC,WINTERCSPEC,BCOEFFTSPEC,   &
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

    !***********************************************************************!
    !                       Begin daily loop                                !
    !***********************************************************************!
    
    ! Initialize met file
    CALL RESTARTMETF(ISTART,MSTART,MFLAG)
    
    IDAY = 0
    DO WHILE (ISTART + IDAY <= IEND)
        WRITE(*,105) IDAY
        105 FORMAT('  DAY:',I5)
       
        !**********************************************************************
    
        CALL INTERPOLATEDIST(IDAY,ISTART,FRACROOTTABLE,NOROOTDATES,NOROOTSPEC,DATESROOT,FRACROOTSPEC,NROOTLAYER, NALPHASPEC, &
                                FALPHATABLESPEC,DATESLIA,NOLIADATES,FALPHASPEC,NSPECIES, &
                                ISMAESPA)
        

        IF(ISMAESPA)THEN
            CALL INTERPOLATEW(IDAY,ISTART,NOKPDATES,DATESKP,PLANTKTABLE,PLANTK,  &
                              NOROOTDATES,DATESROOT,NOROOTSPEC,ROOTRADTABLE,ROOTDENSTABLE,ROOTMASSTOTTABLE, &
                              FRACROOTSPEC,LAYTHICK,ROOTRESFRAC,ROOTXSECAREA, ROOTLENSPEC, ROOTRESIST, ROOTMASSSPEC,  &
                              NROOTLAYER,ROOTRAD)    
        ENDIF
        
        CALL LADCHOOSE(IDAY,ISTART,NSPECIES,NOLADDATES,DATESLAD,BPTTABLESPEC,BPTSPEC)
                    
!       FAIRE SORTIR BPTSPEC - English comments please
        DO I=1,NSPECIES
            CALL EXDIFF(NALPHASPEC(I),ALPHASPEC(1:MAXANG,I),FALPHASPEC(1:MAXANG,I),&       ! dans la boucle Mathias février 2012
                        NZEN,DIFZEN,RANDOMSPEC(I),DEXTSPEC(I,1:MAXANG))
        END DO
        
        ! to account for the looping order change.
        CALL RESTARTMETF(IDAY+ISTART,MSTART,MFLAG)
       
        ! Prepare histogram
        CALL ZEROSTART(HISTO,CANOPYDIMS)

        ! Calculate zenith angle of sun
        CALL SUN(IDAY+ISTART,ALAT,TTIMD,DEC,EQNTIM,DAYL,SUNSET)
        CALL ZENAZ(ALAT,TTIMD,BEAR,DEC,EQNTIM,ZEN,AZ)

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
                CALL TRANSD(IDAY,NEWCANOPY,IPROGUS,NOTREES,XSLOPE,YSLOPE,NZEN,DIFZEN,NAZ,NOUSPOINTS, &
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
                NOSPEC = MAXVAL(ISPECIES(ITARGETS(1:NOTARGETS)))
                
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
                
                WLEAFTABLE = WLEAFTABLESPEC(1:maxdate,ISPEC)
                DATESWLEAF = DATESWLEAFSPEC(1:maxdate,ISPEC)
                NOWLEAFDATES = NOWLEAFDATESSPEC(ISPEC)
                
                D0L = D0LSPEC(ISPEC)          
                GAMMA = GAMMASPEC(ISPEC)     
                VPDMIN = VPDMINSPEC(ISPEC)   
                SF = SFSPEC(ISPEC)
                PSIV = PSIVSPEC(ISPEC)
                
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
                                    NOGSDATES,DATESGS,G0TABLE,G1TABLE,G0,G1,NOWLEAFDATES,DATESWLEAF,WLEAFTABLE,WLEAF)
         
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
            
                
                
                ! Do Maestest calculations (only once - when the loop is at the first tree (ITAR), and on the first day).
                RANPOINTS = 0
                IF(IPOINTS.EQ.1.AND.RANPOINTS.EQ.0)THEN
   
                        ! Sort trees to middle of test points (has no real effect at the moment; since all trees in the plot are used).
                        AX = AVERAGEVAL(XLP,NUMTESTPNT)
                        AY = AVERAGEVAL(YLP,NUMTESTPNT)

                        ! Use *all trees* as target trees for the test points.
                        IPROGCUR = ITEST
                        NOTREESTEST = NOALLTREES

                        !! Sort overstorey dimensions, save in separate arrays.
                        !! Can move this out of loop, only needs to be done once.
                        CALL SORTTREESP( &
                         AX,AY,NOALLTREES,NOTREES, &  ! NOTREES no longer used in SORTTREESP
                         DXT1,DYT1,DZT1,RXTABLE1,RYTABLE1,RZTABLE1, &
                         ZBCTABLE1,FOLTABLE1,DIAMTABLE1, &
                         DXTP,DYTP,DZTP,RXTABLEP,RYTABLEP,RZTABLEP, &
                         FOLTABLEP,ZBCTABLEP,DIAMTABLEP,ISPECIES,ISPECIESTP,ITP)
    
                        ! Interpolate overstorey dimensions for use in test point calcs.
                        CALL INTERPOLATET(IDAY,ISTART,1, &
                         NOXDATES,DATESX,RXTABLEP,NOYDATES,DATESY,RYTABLEP, &
                         NOZDATES,DATESZ,RZTABLEP,NOTDATES,DATEST,ZBCTABLEP, &
                         NODDATES,DATESD,DIAMTABLEP, &
                         NOLADATES,DATESLA,FOLTABLEP,TOTLAITABLE,NOTREESTEST, &
                         RXP,RYP,RZP,ZBCP,FOLTP,TOTLAI,DIAM,STOCKING, &
                         IFLUSH,DT1,DT2,DT3,DT4,EXPTIME,APP,EXPAN, &
                         NEWCANOPY,CANOPYDIMS)
     
                        ! Make tree arrays of radiation-extinction related parameters,
                        ! that may vary between species.
                       DO I = 1,NOTREESTEST
                        JSHAPETP(I) = JSHAPESPEC(ISPECIESTP(I))
                        SHAPETP(I) = SHAPESPEC(ISPECIESTP(I))
                        DEXTTP(I,1:MAXANG) = DEXTSPEC(ISPECIESTP(I),1:MAXANG)
                        JLEAFTP(I) = JLEAFSPEC(ISPECIESTP(I))
                        NOAGECTP(I) = NOAGECSPEC(ISPECIESTP(I))
                        BPTTP(1:8,1:MAXC,I) = BPTSPEC(1:8,1:MAXC,ISPECIESTP(I))
                        PROPPTP(1:MAXC,I) = PROPPSPEC(1:MAXC,ISPECIESTP(I))
                        PROPCTP(1:MAXC,I) = PROPCSPEC(1:MAXC,ISPECIESTP(I))
                       ENDDO

                       CALL TRANSD( &
                        IDAY,NEWCANOPY,IPROGCUR,NOTREESTEST,XSLOPE,YSLOPE, &
                        NZEN,DIFZEN,NAZ,NUMTESTPNT,DEXTTP,DIFSKY, &
                        XLP,YLP,ZLP,RXP,RYP,RZP,DXTP,DYTP,DZTP, &
                        XMAX,YMAX,SHADEHT, &
                        FOLTP,ZBCP,JLEAFTP,BPTTP,NOAGECTP,PROPCTP,JSHAPETP,SHAPETP, &
                        NEWTUTD,TUP,TDP,RELDFP,DEXTP)
      
                        CALL EHC(NUMTESTPNT,TUP,TDP, &
                         TOTLAI,XSLOPE,YSLOPE,NAZ,NZEN,DIFZEN,DEXTP, &
                         DLAIP,EXPDIFP,LAYERP,MLAYERP &
                       )     
                       RANPOINTS = 1
                    
                ENDIF  ! MAESTEST (Diffuse calculations)
                
                ! Output xyz coordinates of crown grid points.
                ! This is actually useful; might make this an output option.
                ! Note in R: require(rgl);plot3d(x,y,z) can plot the points nicely...
                !        DO I =1,NUMPNT
                !            WRITE(UWATTEST,891)XL(I),YL(I),ZL(I)
                !        END DO
                !891     FORMAT (6(F6.2,1X))

                ! Calculate diffuse transmittances
                CALL TRANSD(IDAY,NEWCANOPY,IPROG,NOTREES,XSLOPE,YSLOPE,NZEN,DIFZEN,NAZ,NUMPNT,DEXTT,             &
                            DIFSKY,XL,YL,ZL,RX,RY,RZ,DXT,DYT,DZT,XMAX,YMAX,SHADEHT,FOLT,ZBC,JLEAFT,BPTT,NOAGECT,PROPCT, &
                            JSHAPET,SHAPET,NEWTUTD,TU,TD,RELDF,DEXT)

                TUAR(ITAR, 1:NUMPNT) = TU(1:NUMPNT)
                TDAR(ITAR, 1:NUMPNT) = TD(1:NUMPNT)
                RELDFAR(ITAR, 1:NUMPNT) = RELDF(1:NUMPNT)

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
        
        !**********************************************************************!
        !                       Begin hourly loop                              !
        !**********************************************************************!
        DO IHOUR = 1,KHRS
            
            CALL ZEROFSOIL(FSOIL1,NSUMMED,TOTTMP)
            
            ! Loop over all chosen trees within subplot
            DO ITAR = 1,NOTARGETS
                ITREE = ITARGETS(ITAR)
                
                ! Read diffuse transmittance from precalculated array
                TU(1:NUMPNT) = TUAR(ITAR, 1:NUMPNT)
                TD(1:NUMPNT) = TDAR(ITAR, 1:NUMPNT)
                RELDF(1:NUMPNT) = RELDFAR(ITAR, 1:NUMPNT)

                ! Assign arrays.
                ISPEC = ISPECIES(ITREE)
                
                JLEAF = JLEAFSPEC(ISPEC)
                BPT(1:8,1:MAXC) = BPTSPEC(1:8,1:MAXC,ISPEC)
                RANDOM = RANDOMSPEC(ISPEC)
                NOAGEC = NOAGECSPEC(ISPEC)
                JSHAPE = JSHAPESPEC(ISPEC)
                SHAPE = SHAPESPEC(ISPEC)
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
                JMAXTABLE(1:maxdate,1:MAXLAY,1:MAXC) = JMAXTABLESPEC(1:maxdate,1:MAXLAY,1:MAXC,ISPEC) 
                DATESJ(1:maxdate) = DATESJSPEC(1:maxdate,ISPEC)       
                NOJDATES = NOJDATESSPEC(ISPEC)     
                IECO = IECOSPEC(ISPEC)         
                EAVJ = EAVJSPEC(ISPEC)         
                EDVJ = EDVJSPEC(ISPEC)         
                DELSJ = DELSJSPEC(ISPEC)        
                THETA = THETASPEC(ISPEC)        
                VCMAXTABLE(1:maxdate,1:MAXLAY,1:MAXC) = VCMAXTABLESPEC(1:maxdate,1:MAXLAY,1:MAXC,ISPEC)   
                DATESV(1:maxdate) =  DATESVSPEC(1:maxdate,ISPEC)         
                NOVDATES = NOVDATESSPEC(ISPEC)     
                EAVC = EAVCSPEC(ISPEC)         
                EDVC = EDVCSPEC(ISPEC)         
                DELSC = DELSCSPEC(ISPEC)        
                TVJUP = TVJUPSPEC(ISPEC)        
                TVJDN = TVJDNSPEC(ISPEC)        
                SLATABLE(1:maxdate,1:MAXLAY,1:MAXC) = SLATABLESPEC(1:maxdate,1:MAXLAY,1:MAXC,ISPEC)
                DATESSLA(1:maxdate) =  DATESSLASPEC(1:maxdate,ISPEC)       
                NOSLADATES = NOSLADATESSPEC (ISPEC)  
                NOADATES = NOADATESSPEC(ISPEC)   
                DATESA(1:maxdate) =  DATESASPEC(1:maxdate,ISPEC)         
                AJQTABLE(1:maxdate,1:MAXLAY,1:MAXC) =  AJQTABLESPEC(1:maxdate,1:MAXLAY,1:MAXC,ISPEC)     
                RDTABLE(1:maxdate,1:MAXLAY,1:MAXC) =  RDTABLESPEC(1:maxdate,1:MAXLAY,1:MAXC,ISPEC)      
                DATESRD(1:maxdate) =  DATESRDSPEC(1:maxdate,ISPEC)        
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
                DATESFQ(1:maxdate) =  DATESFQSPEC(1:maxdate,ISPEC)        
                Q10FTABLE(1:maxdate) =  Q10FTABLESPEC(1:maxdate,ISPEC) 
                K10F = K10FSPEC(ISPEC)   
                NOWQDATES = NOWQDATESSPEC(ISPEC)    
                DATESWQ = DATESWQSPEC(1:maxdate,ISPEC)      
                Q10WTABLE(1:maxdate) =  Q10WTABLESPEC(1:maxdate,ISPEC)    
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
                WLEAFTABLE = WLEAFTABLESPEC(1:maxdate,ISPEC)
                GK = GKSPEC(ISPEC)
                !DATESGS = DATESGSSPEC(1:MASPDATE,ISPEC)
                DATESGS = DATESGSSPEC(1:maxdate,ISPEC)
                !DATESWLEAF = DATESWLEAFSPEC(1:MASPDATE,ISPEC)
                DATESWLEAF = DATESWLEAFSPEC(1:maxdate,ISPEC)
                NOGSDATES= NOGSDATESSPEC(ISPEC)
                NOWLEAFDATES= NOWLEAFDATESSPEC(ISPEC)
                D0L = D0LSPEC(ISPEC)          
                GAMMA = GAMMASPEC(ISPEC)     
                SF = SFSPEC(ISPEC)
                PSIV = PSIVSPEC(ISPEC)   
                NSIDES = NSIDESSPEC(ISPEC)
    
                ! Assign water balance and hydraulics.
                MINLEAFWP = MINLEAFWPSPEC(ISPEC)
                ! If multiple species but only one root distribution.
                IF(NOROOTSPEC.EQ.1.AND.NOSPEC.GT.1)THEN
                    ICHOOSE = 1
                ELSE
                    ICHOOSE = ISPEC
                ENDIF
                FRACROOT = FRACROOTSPEC(1:MAXSOILLAY,ICHOOSE)
                ROOTMASS = ROOTMASSSPEC(1:MAXSOILLAY,ISPEC)
                ROOTLEN = ROOTLENSPEC(1:MAXSOILLAY,ISPEC)
                
                ! Sort the trees every timestep.
                ! This should be done outside the hourly loop, and stored in an array.
                CALL SORTTREES(NOALLTREES,NOTREES,ITREE,DXT1,DYT1,DZT1,RXTABLE1,RYTABLE1,RZTABLE1,ZBCTABLE1,&
                                FOLTABLE1,DIAMTABLE1,DXT,DYT,DZT,RXTABLE,RYTABLE,RZTABLE,FOLTABLE,ZBCTABLE, &
                                DIAMTABLE,ISPECIES,ISPECIEST,IT)
                
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
                
                ! Interpolate to get daily values of parameters
                ! This we can probably also do outside the hourly loop.
                CALL INTERPOLATEP(IDAY,ISTART,NOJDATES,DATESJ,JMAXTABLE,NOVDATES,DATESV,VCMAXTABLE,NORDATES,&
                                    DATESRD,RDTABLE,NOSLADATES,DATESSLA,SLATABLE,NOADATES,DATESA,AJQTABLE,  &
                                    NOFQDATES,DATESFQ,Q10FTABLE,NOWQDATES,DATESWQ,Q10WTABLE,NOLAY,NOAGEP,   &
                                    JMAX25,VCMAX25,RD0,SLA,AJQ,Q10F,Q10W,NOGSDATES,DATESGS,G0TABLE,G1TABLE,G0,G1, &
                                    NOWLEAFDATES,DATESWLEAF,WLEAFTABLE,WLEAF)
                
                CALL INTERPOLATET(IDAY,ISTART,IHOUR,NOXDATES,DATESX,RXTABLE,NOYDATES,DATESY,RYTABLE,NOZDATES,   &
                                    DATESZ,RZTABLE,NOTDATES,DATEST,ZBCTABLE,NODDATES,DATESD,DIAMTABLE,          &
                                    NOLADATES,DATESLA,FOLTABLE,TOTLAITABLE,NOTREES,RX,RY,RZ,ZBC,FOLT,           &
                                    TOTLAI,DIAM,STOCKING,IFLUSH,DT1,DT2,DT3,DT4,EXPTIME,APP,EXPAN,NEWCANOPY,    &
                                    CANOPYDIMS)
            
                CALL POINTSNEW(NOLAY,PPLAY,JLEAF,JSHAPE,SHAPE,RX(1),RY(1),RZ(1),ZBC(1),DXT(1),DYT(1), &
                                DZT(1),FOLT(1),PROPC,PROPP,BPT,NOAGECT(1),NOAGEP,XL,YL,ZL,VL,DLT,DLI, &
                                LGP,FOLLAY)          
                
                ! Following functions need FOLLAY. 
                CALL GETWIND(FOLLAY,FOLT(1),TOTLAI,EXTWIND,WINDLAY)
          
                ! Calculate woody biomass and woody biomass increment
                CALL CALCWBIOM(IDAY,RZ(1)+ZBC(1),DIAM(1),COEFFT,EXPONT,WINTERC,WBIOM,WBINC)
                CALL CALCWBIOM(IDAY,RZ(1)+ZBC(1),DIAM(1),BCOEFFT,BEXPONT,BINTERC,BBIOM,BBINC)
                CALL CALCWBIOM(IDAY,RZ(1)+ZBC(1),DIAM(1),RCOEFFT,REXPONT,RINTERC,RBIOM,RBINC)
                
                ! Calculate foliar biomass and increment
                CALL CALCFBIOM(IDAY,NOSLADATES,FOLLAY,SLA,PROPP,NOLAY,NOAGEP,FBIOM,FBINC)
                
                ! Calculate stem respiration rate per unit biomas
                RMW = CALCRMW(MODELRW,COLLA,COLLK,STEMSDW,DIAM(1),RZ(1)+ZBC(1),STEMFORM,RMWAREA,WBIOM,RMW)

                ! Output information to layer flux file if required
                IF (IOHRLY.GT.1) CALL OUTPUTLAY(ULAY,FOLLAY,JMAX25,VCMAX25,NOLAY)
                   
                ! If the diffuse transmittances have changed, must set up the EHC
                IF (NEWTUTD.EQ.1.AND.TOTLAI.GT.0) THEN
                    CALL EHC(NUMPNT,TU,TD,TOTLAI,XSLOPE,YSLOPE,NAZ,NZEN,DIFZEN,DEXT,DLAI,EXPDIF,LAYER,MLAYER)
                END IF
                
                IF(ISMAESPA)THEN
                    
                    ! Assign soil water measurement to SOILMOIST, depending on settings.
                    CALL ASSIGNSOILWATER(WSOILMETHOD,USEMEASSW,SWMIN,SWMAX,SOILMOIST(IHOUR), WSOILROOT, &
                                            SOILDEPTH,SOILDATA, SOILMOISTURE)
                
                    ! Soil water potential, conductivity & conductance, fractional uptake (but no uptake yet).
                    CALL CALCSOILPARS(NLAYER,NROOTLAYER,ISPEC,SOILWP,FRACWATER,FRACORGANIC,POREFRAC,SOILCOND,THERMCOND,   &
                                        ROOTMASS,ROOTLEN,LAYTHICK,ICEPROP,EQUALUPTAKE,RETFUNCTION,USEMEASSW,        &
                                        SOILDATA, SOILMOISTURE,PSIE,BPAR,KSAT,ROOTRESIST,ROOTRESFRAC,    &
                                        ROOTRAD,MINROOTWP,TOTLAI,WINDAH(IHOUR),ZHT,Z0HT,GAMSOIL,   &
                                        WEIGHTEDSWP,TOTESTEVAP, &
                                        FRACUPTAKESPEC(1:MAXSOILLAY, ISPEC),TOTSOILRES,ALPHARET,WS,WR,NRET)
                    

                    
                    ! Soil surface T for SCATTER routine:
                    IF(SIMTSOIL.EQ.0)THEN  ! No Tsoil simulated.
                        PREVTSOIL = TK(TSOIL(IHOUR))
                    ELSE
                        PREVTSOIL = SOILTEMP(1)
                    ENDIF
                ELSE
                    WEIGHTEDSWP = 0
                ENDIF
                
                
                ! Test to see if daylight hours or if any foliage
                IF ((ABS(ZEN(IHOUR)) <  PI/2.0 ) .AND. (RADABV(IHOUR,1) > 1.0) .AND. (FOLT(1) > 0.0)) THEN
                    
                    ! Get slope correction factor
                    CALL SLOPES(IHOUR,TTIMD,EQNTIM,ALAT,DEC,XSLOPE,YSLOPE,BEAR,ZEN(IHOUR),BMULT,DMULT2,SOMULT)

                    ! Get extinction coefficients
                    DO I=1,NSPECIES
                        CALL EXDIFF(NALPHASPEC(I),ALPHASPEC(1:MAXANG,I),FALPHASPEC(1:MAXANG,I),NZEN,DIFZEN,&
                                    RANDOMSPEC(I),DEXTSPEC(I,1:MAXANG))
  
                        CALL EXBEAM(NALPHASPEC(I),ALPHASPEC(1:MAXANG,I),FALPHASPEC(1:MAXANG,I),RANDOMSPEC(I),&
                                    ZEN(IHOUR),BEXTSPEC(I),BEXTANGSPEC(I,1:MAXANG))
                    END DO

                    DO I=1,NOTREES
                        BEXTT(I) = BEXTSPEC(ISPECIEST(I))
                        BEXTANGT(I,1:MAXANG) = BEXTANGSPEC(ISPECIEST(I),1:MAXANG)
                    END DO

                    ! Understorey calculations.
                    IF(ISIMUS.EQ.1)THEN

                        ! Find extinction coefficients for beam component,
                        ! sorted to understorey midpoint (ISPECIESTUS).
                        DO I=1,NOTREES
                            BEXTTUS(I) = BEXTSPEC(ISPECIESTUS(I))
                            BEXTANGTUS(I,1:MAXANG) = BEXTANGSPEC(ISPECIESTUS(I),1:MAXANG)
                        END DO

                        DO IPTUS = 1,NOUSPOINTS

                            ! Beam radiation on the understorey points.
                            CALL TRANSB(IHOUR,IPROGUS,ZEN(IHOUR),AZ(IHOUR),XSLOPE,YSLOPE,FBEAM,BEXTTUS,XLU(IPTUS),  &
                                        YLU(IPTUS),ZLU(IPTUS),RXUS,RYUS,RZUS,DXTUS,DYTUS,DZTUS,XMAX,YMAX,SHADEHT,   &
                                        FOLTUS,ZBCUS,JLEAFTUS,BPTTUS,NOAGECTUS,PROPCTUS,JSHAPETUS,SHAPETUS,NOTREES, &
                                        SUNLA,BEXTUS,BEXTANGTUS,BEXTANGUS)  ! BEXTUS not used...
                
                            ! Output transmittances (Note IWAVE=1 only).
                            PAR = RADABV(IHOUR,1)
               
                            TDIFF = (1-FBEAM(IHOUR,1))*PAR*TDUS(IPTUS)
                            TSCAT = 0 !DIFDN(IPTUS,1)*UMOLPERJ
                  
                            ! Recalculate sunlit leaf area (TRANSB returns it but that is at a point just above the understorey,
                            ! not sunlit leaf area for the understorey canopy (since IPROG=ITEST).
                            ! From Campbell&Norman (2000, p. 259)
                            SUNLA = (1 - EXP(-EXTKUS*USLAI(IPTUS)))/EXTKUS
                                  
                            ! PAR at all understorey points, diffuse, direct, and total.
                            UIBEAM(IHOUR,IPTUS) = FBEAM(IHOUR,1)*PAR*SUNLA
                            UIDIFF(IHOUR,IPTUS) = TDIFF + TSCAT
                            PARUS(IHOUR,IPTUS) = UIDIFF(IHOUR,IPTUS) + UIBEAM(IHOUR,IPTUS)
                            
                            ! Get parameters of BEWDY model
                            CALL BEWDYPARMS(IHOUR,TAIR(IHOUR),RH(IHOUR),CA(IHOUR),JMAXN25,IECOU,EAVJU,EDVJU,DELSJU, &
                                            TVJUPU,TVJDNU,VCMAXN25,EAVCU,EDVCU,DELSCU,AJQU,GSBG0U,GSBG1U,CICARAT,   &
                                            BALPHA,BLAMBDA)
              
                            IF (MOSS.EQ.1) THEN
                                CALL PSMOSS(UMOLPERJ*PARUS(IHOUR,IPTUS),TAIR(IHOUR),RH(IHOUR),CA(IHOUR),JMAX25M,IECOU,  &
                                            EAVJU,EDVJU,DELSJU,TVJUPU,TVJDNU,VCMAX25M,EAVCU,EDVCU,DELSCU,AJQU,THETAM,   &
                                            PSUS(IHOUR,IPTUS))
                                GSIPT = 0.0
                                ETUS = 0.0
                            ELSE
                                
                                ! Otherwise call BEWDY model to calculate understorey photosynthesis
                                !! Note UIBEAM is divided by SUNLA, gets back-converted in BEWDY subr.
                                IF(SUNLA.GT.0.0) THEN
                                    BEAMP = UMOLPERJ*UIBEAM(IHOUR,IPTUS)/SUNLA
                                ELSE    
                                    BEAMP = 0.0
                                ENDIF
                                
                                IF(C4FRAC.LT.1.0)THEN
                                    CALL BEWDY(IHOUR,BEAMP,UMOLPERJ*UIDIFF(IHOUR,IPTUS),SUNLA,BALPHA,BLAMBDA,FN0US(IPTUS),  &
                                            UNMIN,EXTKUS,ABSRPU,USLAI(IPTUS), APARUS(IHOUR,IPTUS),PSC3,                 &
                                            PARUNDER(IHOUR,IPTUS))
                                    
                                    ! Stomatal conductance and transpiration - estimated at whole-clump level, mol CO2 m-2 s-1
                                    IF(GSBG1U.GT.0.0)THEN
                                        GSIPT = GSBG0U + GSBG1U* PSC3 * RH(IHOUR)/CA(IHOUR) 
                                    ELSE
                                        GSIPT = PSC3/(CA(IHOUR) - CICARAT*CA(IHOUR))
                                    ENDIF
                                
                                    ! mmol H2O m-2 s-1
                                    ETC3 = GSIPT*GSVGSC*VPD(IHOUR)/PRESS(IHOUR)*1E3
                                    
                                ELSE
                                    PSC3 = 0.0
                                    ETC3 = 0.0
                                ENDIF
                                
                                ! 
                                IF(C4FRAC.GT.0.0)THEN
                                    CALL COLLATZC4(VCMAXC4, TAIR(IHOUR), TVJUPC4, TVJDNC4, DELSCC4, EAVCC4, EDVCC4,    &
                                               UIDIFF(IHOUR,IPTUS), SUNLA, BEAMP, ABSRPU, EXTKUS, USLAI(IPTUS),        &
                                               CICAC4,CA(IHOUR), APARUS(IHOUR,IPTUS), PSC4, GSC4)
                                    
                                    ! mmol H2O m-2 s-1
                                    ETC4 = GSC4*GSVGSC*VPD(IHOUR)/PRESS(IHOUR)*1E3
                                ELSE
                                    PSC4 = 0.0
                                    ETC4 = 0.0
                                ENDIF
                                
                                ! Net photosynthesis
                                PSUS(IHOUR,IPTUS) = PSUS(IHOUR,IPTUS) + C4FRAC*PSC4 + (1.0 - C4FRAC)*PSC3 - RD0US
                                
                                ! Transpiration
                                ETUS(IHOUR,IPTUS) = C4FRAC*ETC4 + (1.0 - C4FRAC)*PSC3
                                
                                ! Bewdy outputs in mu mol, convert to W m-2
                                APARUS(IHOUR,IPTUS) = APARUS(IHOUR,IPTUS) / UMOLPERJ
                                PARUNDER(IHOUR,IPTUS) = PARUNDER(IHOUR,IPTUS) / UMOLPERJ


                            END IF
                        END DO ! Loop over understorey points.
              
                        ! Sum understorey arrays over all points:
                        CALL SUMHRUS(IHOUR,NOUSPOINTS,GRDAREAI,AREAUS,PARUS,PARUSMEAN,PARUSSD,APARUS,PSUS,ETUS,THRABUS,&
                                    FCO2US,FH2OUS)
                    ENDIF ! Understorey calculations
                    
                    ! Output PAR transmittance for test points.
                    IF(IPOINTS.EQ.1)THEN      
                        DO IPTEST = 1,NUMTESTPNT
          
                        IPROGCUR = ITEST

                        DO I=1,NOTREESTEST
                            BEXTTP(I) = BEXTSPEC(ISPECIESTP(I))
                            BEXTANGTP(I,1:MAXANG) = BEXTANGSPEC(ISPECIESTP(I),1:MAXANG)
                        ENDDO

                        CALL TRANSB(IHOUR,IPROGCUR, &
                                ZEN(IHOUR),AZ(IHOUR),XSLOPE,YSLOPE,FBEAM,BEXTTP, &
                                XLP(IPTEST),YLP(IPTEST),ZLP(IPTEST),RXP,RYP,RZP,DXTP,DYTP,DZTP, &
                                XMAX,YMAX,SHADEHT, &
                                FOLTP,ZBCP,JLEAFTP,BPTTP,NOAGECTP,PROPCTP,JSHAPETP,SHAPETP, &
                                NOTREESTEST,SUNLAP,BEXTP,BEXTANGTP,BEXTANGP)

                        IWAVE = 1 !(As in original MAESTEST, use only PAR.
                        CALL SCATTER(IPTEST,IWAVE, &
                                MLAYERP(IPTEST),LAYERP(IPTEST),DLAIP,EXPDIFP, &
                                ZEN(IHOUR),BEXTP, &
                                DMULT2,SOMULT,BMULT, &
                                RADABV(IHOUR,IWAVE),FBEAM(IHOUR,IWAVE), &
                                TAIR(IHOUR),TSOIL(IHOUR), &
                                ARHO(1,IWAVE),ATAU(1,IWAVE), &    ! Note 1 here instead of LGP().
                                RHOSOL(IWAVE), &
                                DIFUP,DIFDN,SCLOST,THDOWNP)

                        CALL ABSRAD(IPTEST,IWAVE, &
                                NZEN,DEXT,BEXT,BMULT,RELDFP(IPTEST), &
                                RADABV(IHOUR,IWAVE),FBEAM(IHOUR,IWAVE),ZEN(IHOUR), &
                                ABSRP(1,IWAVE),DIFDN(IPTEST,IWAVE), &
                                DIFUP(IPTEST,IWAVE), &
                                DFLUX,BFLUX,SCATFX)

                        ! Output transmittances
                        PAR = RADABV(IHOUR,1)*UMOLPERJ
                        TBEAM = FBEAM(IHOUR,IWAVE)*PAR*SUNLAP
                        TDIFF = (1-FBEAM(IHOUR,IWAVE))*PAR*TDP(IPTEST)
                        TSCAT = DIFDN(IPTEST,IWAVE)
                        TTOT = TBEAM + TDIFF + TSCAT
      
                        APARSUN = (BFLUX(IPTEST,1)*BEXTP + DFLUX(IPTEST,1))*UMOLPERJ
                        APARSH = DFLUX(IPTEST,1)*UMOLPERJ
                        APARMEAN = SUNLAP * APARSUN + (1-SUNLAP)*APARSH

                        WRITE (UPOINTSO,501) IDAY,IHOUR,IPTEST,XLP(IPTEST),YLP(IPTEST),ZLP(IPTEST), &
                            PAR,FBEAM(IHOUR,1),SUNLAP,TDP(IPTEST),TSCAT,TTOT,APARSUN,APARSH,APARMEAN
501                         FORMAT(3(I5,1X),12(F12.5,1X))
                            
                        ENDDO  
                    ENDIF  !MAESTEST          
   
                    
                    ! Loop over grid points
                    DO IPT = 1,NUMPNT
                        ! Calculate the weighted pathlengths for beam radiation.
                        CALL TRANSB(IHOUR,IPROG,ZEN(IHOUR),AZ(IHOUR),XSLOPE,YSLOPE,FBEAM,BEXTT,XL(IPT),YL(IPT),ZL(IPT), &
                                    RX,RY,RZ,DXT,DYT,DZT,XMAX,YMAX,SHADEHT,FOLT,ZBC,JLEAFT,BPTT,NOAGECT,PROPCT,JSHAPET, &
                                    SHAPET,NOTREES,SUNLA,BEXT,BEXTANGT,BEXTANG)
                        ! Assign plant hydraulic conductance
                        !PLANTK = PLANTKCR(ITAR,IPT)
                        

                        ! Loop over the 3 wavelengths
                        DO IWAVE = 1,3
                            
                            ! Calculate the scattered radiation
                            CALL SCATTER(IPT,IWAVE,MLAYER(IPT),LAYER(IPT),DLAI,EXPDIF,ZEN(IHOUR),BEXT,DMULT2,SOMULT,BMULT,&
                                            RADABV(IHOUR,IWAVE),FBEAM(IHOUR,IWAVE),TAIR(IHOUR),PREVTSOIL,ARHO(LGP(IPT),IWAVE),&
                                            ATAU(LGP(IPT),IWAVE),RHOSOL(IWAVE),DIFUP,DIFDN,SCLOST,DOWNTH)

                            ! Lost scattered radiation for each tree (W m-2), averaged over the grid points.
                            ! RAD June 2008.              
                            SCLOSTTREE(ITAR,1) = SUM(SCLOST(1:NUMPNT,1)) / NUMPNT
                            SCLOSTTREE(ITAR,2) = SUM(SCLOST(1:NUMPNT,2)) / NUMPNT
                            
                            ! Assume zero reflectance in TR waveband (Norman 1979)
                            ! But store in the same array the lost tranmission at top of canopy.
                            SCLOSTTREE(ITAR,3) = SUM(SCLOST(1:NUMPNT,2)) / NUMPNT

                            ! Downwelling longwave radiation (calculated for each gridpoint
                            ! with the EHC) averaged across the grid points.
                            IF(IWAVE.EQ.3)DOWNTHTREE(ITAR) = SUM(DOWNTH) / NUMPNT
                  
                            ! Calculate absorbed radiation
                            CALL ABSRAD(IPT,IWAVE,NZEN,DEXT,BEXT,BMULT,RELDF(IPT),RADABV(IHOUR,IWAVE),&
                                        FBEAM(IHOUR,IWAVE),ZEN(IHOUR),ABSRP(LGP(IPT),IWAVE),DIFDN(IPT,IWAVE),&
                                        DIFUP(IPT,IWAVE),DFLUX,BFLUX,SCATFX)

                        END DO
                        
                        ! Calculation of photosynthesis may be done for sunlit & shaded leaves
                        ! separately, or by averaging PAR over the total leaf area
                        IF ((MODELSS.EQ.0).AND.(FBEAM(IHOUR,1).NE.0.0)) THEN 

                            ! Do calculations separately for sunlit & shaded leaves
                            DO ISUNLIT = 1,2 ! Loop over sunlit & shaded leaves

                                IF (ISUNLIT.EQ.1) THEN
                                    APAR = (BFLUX(IPT,1)*BEXT + DFLUX(IPT,1))*UMOLPERJ
                                    ANIR = BFLUX(IPT,2)*BEXT + DFLUX(IPT,2)
                                    FAREA = SUNLA
                                ELSE
                                    APAR = DFLUX(IPT,1)*UMOLPERJ
                                    ANIR = DFLUX(IPT,2)
                                    FAREA = 1.0 - SUNLA
                                END IF
                                ATHR = DFLUX(IPT,3)
                                RNET = APAR/UMOLPERJ + ANIR + ATHR


                                DO IAGE = 1,NOAGEP ! Loop over age classes
                                    AREA = FAREA * DLI(IAGE,IPT) * VL(IPT) ! m2
                                    IF (IOHIST.EQ.1) CALL CATEGO(AREA,APAR,HISTO,BINSIZE,ITAR)
                                    
                                    ! Call physiology routine
                                    CALL PSTRANSPIF(iday,ihour,RELDF(IPT),TU(IPT),TD(IPT),RNET,WINDAH(IHOUR)*WINDLAY(LGP(IPT)),APAR,         &
                                                    TAIR(IHOUR),TMOVE,CA(IHOUR),RH(IHOUR),VPD(IHOUR),VMFD(IHOUR),PRESS(IHOUR),  &
                                                    JMAX25(LGP(IPT),IAGE),IECO,EAVJ,EDVJ,DELSJ,VCMAX25(LGP(IPT),IAGE),EAVC,     &
                                                    EDVC,DELSC,TVJUP,TVJDN,THETA,AJQ(LGP(IPT),IAGE),RD0(LGP(IPT),IAGE),Q10F,    &
                                                    K10F,RTEMP,DAYRESP,TBELOW,MODELGS,WSOILMETHOD,EMAXLEAF,SOILMOISTURE,        &
                                                    SMD1,SMD2,WC1,WC2,SOILDATA,SWPEXP,FSOIL,G0,D0L,GAMMA,VPDMIN,G1,GK,WLEAF,NSIDES,VPARA,VPARB,VPARC,VFUN, &
                                                    SF,PSIV,ITERMAX,GSC,ALEAF,RD,ET,HFX,TLEAF,GBH,PLANTK,TOTSOILRES,MINLEAFWP,WEIGHTEDSWP,KTOT,                    &
                                                    HMSHAPE,PSIL,ETEST,CI,ISMAESPA)                                    
                                    
892     FORMAT (1(I10),6(F10.5,1X))
                                    ! Sum (or average) outputs for the hour
                                    CALL SUMHR(APAR/UMOLPERJ,ANIR,ATHR,ALEAF,RD,GSC,GBH,ET,HFX,TLEAF,FSOIL,PSIL,CI,AREA,IHOUR,LGP(IPT),ITAR,&
                                                NOTARGETS,NUMPNT,NSUMMED,TOTTMP,PPAR,PPS,PTRANSP,THRAB,FCO2,FRESPF,GSCAN,GBHCAN,FH2O,  &
                                                FHEAT,TCAN,FSOIL1,PSILCAN,PSILCANMIN,CICAN,ECANMAX,ACANMAX)
                                END DO
                            END DO ! End loop over sunlit / shaded leaves
                        ELSE IF ((MODELSS.EQ.1).OR.(FBEAM(IHOUR,1).EQ.0.0)) THEN 
                            
                            ! Do calculations for PAR averaged over all leaf area
                            APAR = (BFLUX(IPT,1)*BEXT*SUNLA + DFLUX(IPT,1))*UMOLPERJ
                            ANIR = BFLUX(IPT,2)*BEXT*SUNLA + DFLUX(IPT,2)
                            ATHR = DFLUX(IPT,3)
                            RNET = APAR/UMOLPERJ + ANIR + ATHR
                            DO IAGE = 1,NOAGEP ! Loop over age classes
                                AREA = DLI(IAGE,IPT) * VL(IPT)
                                IF (IOHIST.EQ.1) CALL CATEGO(AREA,APAR,HISTO,BINSIZE,ITAR)
                                
                                ! Call physiology routine
                                CALL PSTRANSPIF(iday,ihour,RELDF(IPT),TU(IPT),TD(IPT),RNET,WINDAH(IHOUR)*WINDLAY(LGP(IPT)),  &
                                                APAR,TAIR(IHOUR),TMOVE,CA(IHOUR),RH(IHOUR),VPD(IHOUR),          &
                                                VMFD(IHOUR),PRESS(IHOUR),JMAX25(LGP(IPT),IAGE),                 &
                                                IECO,EAVJ,EDVJ,DELSJ,VCMAX25(LGP(IPT),IAGE),EAVC,EDVC,DELSC,    &
                                                TVJUP,TVJDN,THETA,AJQ(LGP(IPT),IAGE),RD0(LGP(IPT),IAGE),Q10F,   &
                                                K10F,RTEMP,DAYRESP,TBELOW,MODELGS,                              &
                                                WSOILMETHOD,EMAXLEAF,                                           &
                                                SOILMOISTURE,SMD1,SMD2,WC1,WC2,SOILDATA,SWPEXP,FSOIL,G0,D0L,    &
                                                GAMMA,VPDMIN,G1,GK,WLEAF,NSIDES,VPARA,VPARB,VPARC,VFUN,SF,PSIV,            &
                                                ITERMAX,GSC,ALEAF,RD,ET,HFX,TLEAF,                              &
                                                GBH,PLANTK,TOTSOILRES,MINLEAFWP,WEIGHTEDSWP,KTOT,HMSHAPE,PSIL,ETEST,CI,ISMAESPA)                                        

                                ! Sum outputs for the hour
                                CALL SUMHR(APAR/UMOLPERJ,ANIR,ATHR,ALEAF,RD,GSC,GBH,ET,HFX,TLEAF,FSOIL,PSIL,CI,AREA,IHOUR,LGP(IPT),ITAR,&
                                            NOTARGETS,NUMPNT,NSUMMED,TOTTMP,PPAR,PPS,PTRANSP,THRAB,FCO2,FRESPF,GSCAN,GBHCAN,FH2O,  &
                                            FHEAT,TCAN,FSOIL1,PSILCAN,PSILCANMIN,CICAN,ECANMAX,ACANMAX)
   
                     
                            END DO ! End loop over age classes
                        ELSE IF ((MODELSS.EQ.2).AND.(FBEAM(IHOUR,1).NE.0.0)) THEN 
                            
                            ! Do calculations separately for sunlit & shaded. Further separate sunlit
                            ! into leaf angle classes.
                            DO ISUNLIT = 1,NALPHA+1
                                IF (ISUNLIT.GT.NALPHA) THEN
                                    APAR = DFLUX(IPT,1)*UMOLPERJ
                                    ANIR = DFLUX(IPT,2)
                                    FAREA = 1.0 - SUNLA
                                ELSE
                                    APAR = (BFLUX(IPT,1)*BEXTANG(ISUNLIT) + DFLUX(IPT,1))*UMOLPERJ
                                    ANIR = BFLUX(IPT,2)*BEXTANG(ISUNLIT) + DFLUX(IPT,2)
                                    FAREA = SUNLA*FALPHA(ISUNLIT)
                                END IF

                                ATHR = DFLUX(IPT,3)
                                RNET = APAR/UMOLPERJ + ANIR + ATHR

                                DO IAGE = 1,NOAGEP ! Loop over age classes
                                    AREA = FAREA * DLI(IAGE,IPT) * VL(IPT) ! m2
                                    IF (IOHIST.EQ.1) CALL CATEGO(AREA,APAR,HISTO,BINSIZE,ITAR)

                                        ! Call physiology routine
                                        CALL PSTRANSPIF(iday,ihour,RELDF(IPT),TU(IPT),TD(IPT),RNET,WINDAH(IHOUR)*WINDLAY(LGP(IPT)),  &
                                                        APAR,TAIR(IHOUR),TMOVE,CA(IHOUR),RH(IHOUR),VPD(IHOUR),          &
                                                        VMFD(IHOUR),PRESS(IHOUR),JMAX25(LGP(IPT),IAGE),IECO,            &
                                                        EAVJ,EDVJ,DELSJ,VCMAX25(LGP(IPT),IAGE),                         &
                                                        EAVC,EDVC,DELSC,TVJUP,TVJDN,THETA,AJQ(LGP(IPT),IAGE),           &
                                                        RD0(LGP(IPT),IAGE),Q10F,K10F,RTEMP,DAYRESP,TBELOW,MODELGS,      &
                                                        WSOILMETHOD,EMAXLEAF,SOILMOISTURE,SMD1,SMD2,WC1,WC2,            &
                                                        SOILDATA,SWPEXP,FSOIL,G0,D0L,GAMMA,VPDMIN,G1,GK,WLEAF,NSIDES,VPARA,VPARB,VPARC,VFUN,SF,PSIV,     &
                                                        ITERMAX,GSC,ALEAF,RD,ET,HFX,TLEAF,GBH,PLANTK,TOTSOILRES,MINLEAFWP,WEIGHTEDSWP,KTOT,   &
                                                        HMSHAPE,PSIL,ETEST,CI,ISMAESPA)
                                    
                                        ! Sum outputs for the hour
                                        CALL SUMHR(APAR/UMOLPERJ,ANIR,ATHR,ALEAF,RD,GSC,GBH,ET,HFX,TLEAF,FSOIL,PSIL,CI,AREA,IHOUR,  &
                                                    LGP(IPT),ITAR,NOTARGETS,NUMPNT,NSUMMED,TOTTMP, PPAR,PPS,PTRANSP,    &
                                                    THRAB,FCO2,FRESPF,GSCAN,GBHCAN,FH2O,FHEAT,TCAN,FSOIL1,PSILCAN,PSILCANMIN,CICAN,  &
                                                    ECANMAX,ACANMAX)
                                END DO
                            END DO ! End loop over sunlit / shaded leaves
                        END IF ! Separating sunlit & shaded foliage, or not

                         ! Write sunlit leaf area to file.
                         IF(ISUNLA.EQ.1)THEN
                             AREA = DLT(IPT) * VL(IPT)  ! LEAF AREA FOR THIS GRIDPOINT               ! Modification Mathias 27/11/2012
                             WRITE(USUNLA, 112) IDAY, IHOUR, ITREE, IPT, SUNLA, &
                                  AREA, BEXT, &
                                  FBEAM(IHOUR,1), ZEN,ABSRP(LGP(IPT),1),ABSRP(LGP(IPT),2),ABSRP(LGP(IPT),3), &
                                  BFLUX(IPT,1), DFLUX(IPT,1), BFLUX(IPT,2),DFLUX(IPT,2),DFLUX(IPT,3), &
                                  SCLOST(IPT,1),SCLOST(IPT,2),SCLOST(IPT,3),DOWNTH(IPT),RADABV(IHOUR,1),RADABV(IHOUR,2),RADABV(IHOUR,3)
                                    
                         ENDIF
112      FORMAT(4(1X,I4), 7(1X,F12.3), 13(1X,F12.3))
                    
                    END DO ! End loop over grid points
                    

                    ! Calculate transpiration by applying Penman-Monteith to canopy
                    FH2OCAN(ITAR,IHOUR) = ETCAN(WINDAH(IHOUR),ZHT,Z0HT,ZPD,PRESS(IHOUR),TAIR(IHOUR),    &
                                            THRAB(ITAR,IHOUR,1)+THRAB(ITAR,IHOUR,2)+THRAB(ITAR,IHOUR,3),&
                                            VPD(IHOUR),GSCAN(ITAR,IHOUR),STOCKING)
            
                    ! Normalise to get average foliage temperature
                    TCAN(ITAR,IHOUR) = TCAN(ITAR,IHOUR)/FOLT(1)
                    
                    ! Same for leaf water potential
                    PSILCAN(ITAR,IHOUR) = PSILCAN(ITAR,IHOUR)/FOLT(1)
                    
                    ! And ci
                    CICAN(ITAR,IHOUR) = CICAN(ITAR,IHOUR)/FOLT(1)
                    
                ELSE ! Night-time
    
                    ! Canopy T at nighttime is same as air temperature. 
                    TCAN(ITAR,IHOUR) = TAIR(IHOUR)
                    
                    ! Leaf water potential same as soil water potential
                    PSILCAN(ITAR,IHOUR) = WEIGHTEDSWP
                    PSILCANMIN(ITAR,IHOUR) = WEIGHTEDSWP
                    
                    ! And ci the same as ca
                    CICAN(ITAR,IHOUR) = CA(IHOUR)

                    ! Loop over grid points 
                    DO IPT = 1,NUMPNT
                        ! Calculate the scattered radiation, for thermal only.
                        CALL SCATTER(IPT,3,MLAYER(IPT),LAYER(IPT),DLAI,EXPDIF,ZEN(IHOUR),BEXT,DMULT2,SOMULT,&
                                        BMULT,RADABV(IHOUR,3),                                              &
                                        FBEAM(IHOUR,3),TAIR(IHOUR),PREVTSOIL, ARHO(LGP(IPT),3),             &
                                        ATAU(LGP(IPT),3),RHOSOL(3),DIFUP,                                   &
                                        DIFDN,SCLOST,DOWNTH)
                  
                        ! Lost scattered radiation for each tree (W m-2), averaged over the grid points.
                        ! RAD June 2008.              
                        SCLOSTTREE(ITAR,1) = SUM(SCLOST(1:NUMPNT,1)) / NUMPNT
                        SCLOSTTREE(ITAR,2) = SUM(SCLOST(1:NUMPNT,2)) / NUMPNT
                        ! Assume zero reflectance in TR waveband (Norman 1979)
                        SCLOSTTREE(ITAR,3) = 0.
                        ! Downward thermal flux, averaged across grid points in this tree:
                        DOWNTHTREE(ITAR) = SUM(DOWNTH) / NUMPNT
                  
                        ! Calculate absorbed radiation
                        CALL ABSRAD(IPT,3,NZEN,DEXT,BEXT,BMULT,RELDF(IPT),RADABV(IHOUR,3),FBEAM(IHOUR,3),ZEN(IHOUR),&
                                    ABSRP(LGP(IPT),3),DIFDN(IPT,3),DIFUP(IPT,3),DFLUX,BFLUX,SCATFX)

                        ! Absorbed thermal radiation
                        ATHR = DFLUX(IPT,3)
              
                        DO IAGE = 1,NOAGEP ! Loop over age classes
                            AREA = DLI(IAGE,IPT) * VL(IPT) ! m2
                                 
                            ! Sum outputs for the hour (Added RAD, Aug. 2008).
                            CALL SUMHR(0.0,0.0,ATHR,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.0,0.0,0.0,AREA,IHOUR,LGP(IPT),&
                                        ITAR,NOTARGETS,NUMPNT,NSUMMED,TOTTMP,PPAR,PPS,PTRANSP,THRAB,&
                                        FCO2,FRESPF,GSCAN,GBHCAN,FH2O,FHEAT,TCAN,FSOIL1,PSILCAN,PSILCANMIN,CICAN,  &
                                        ECANMAX,ACANMAX)
                        END DO ! End loop over age classes.
                    END DO ! Loop over gridpoints (nighttime).
                    
                    ! Calculate night-time foliage respiration
                    DO IPT = 1,NUMPNT
                        DO IAGE = 1,NOAGEP
                            AREA = DLI(IAGE,IPT) * VL(IPT)
                            RESPF = AREA * RESP(RD0(LGP(IPT),IAGE),RD0ACC,TAIR(IHOUR),TMOVE,Q10F,K10F,RTEMP,1.0,TBELOW)
                            FCO2(ITAR,IHOUR) = FCO2(ITAR,IHOUR) - RESPF
                            FRESPF(ITAR,IHOUR) = FRESPF(ITAR,IHOUR) + RESPF 
                        END DO
                    END DO
                    
                END IF ! If day or night
                    
               
                ! No good (does not run for Tumbarumba, and has not been updated since looping order change).
                ! Calculate non-foliage maintenance respiration (in umol tree-1 s-1)
                FRESPW(ITAR,IHOUR) = RESP(RMW,RMW,TAIR(IHOUR),TAIR(IHOUR),Q10W,0.0,RTEMPW,1.0,TBELOW) * WBIOM
                FRESPB(ITAR,IHOUR) = RESP(RMB,RMB,TAIR(IHOUR),TAIR(IHOUR),Q10B,0.0,RTEMPB,1.0,TBELOW) * BBIOM
                FRESPFR(ITAR,IHOUR) = RESP(RMFR,RMFR,TSOIL(IHOUR),TAIR(IHOUR),Q10R,0.0,RTEMPR,1.0,TBELOW) * RBIOM * FRFRAC
                FRESPCR(ITAR,IHOUR) = RESP(RMCR,RMCR,TSOIL(IHOUR),TAIR(IHOUR),Q10R,0.0,RTEMPR,1.0,TBELOW) * RBIOM * (1. - FRFRAC)


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
            DO K=1,NOTARGETS
                DO I = 1,NOALLTREES
                    IF(IT(I).EQ.ITARGETS(K)) THEN
                        TARGETFOLS(K)=FOLT(I)
                    ENDIF
                ENDDO
            ENDDO
           
!            K = 1
!            DO I = 1,NOALLTREES
!               IF(IT(I).EQ.ITARGETS(K))THEN
!               TARGETFOLS(K) = FOLT(I)
!               K = K + 1
!               ENDIF
!            ENDDO
            
            !
            IF(ISMAESPA) THEN
                
                ! Get area-based estimates of radiation interception and transpiration rate.
                CALL SCALEUP(IHOUR, USESTAND, NOTARGETS, NOALLTREES, TARGETFOLS,ITARGETS,ISPECIES,NOSPEC,TOTLAI,STOCKING,  &
                                SCLOSTTREE,THRAB,RADABV,FH2O,PLOTAREA,  &
                                DOWNTHTREE,RGLOBABV,RGLOBUND,RADINTERC,FRACAPAR,ISIMUS,FH2OUS(IHOUR),THRABUS(IHOUR),   &
                                PARUSMEAN(IHOUR),SCLOSTTOT,GSCAN,WINDAH(IHOUR),ZHT,Z0HT,ZPD,PRESS(IHOUR),TAIR(IHOUR),       &
                                VPD(IHOUR),ETMM,ETUSMM,ETMMSPEC)            

                ! Find soil surface temperature, unless this is input data.
                ! Note this uses DRYTHICK from previous timestep (or initial value in first go).
                IF(SIMTSOIL.EQ.1) THEN
                    VIEWFACTOR = 1.0  ! OBSOLETE...
                    CALL FINDSOILTK(iday, TAIR(IHOUR) + FREEZE, GAMSOIL, PRESS(IHOUR),SOILTK, SOILTEMP(2), VPD(IHOUR)/1000, &
                                    RGLOBUND,THERMCOND(1), LAYTHICK(1), POREFRAC(1),SOILWP(1),DRYTHICK,TORTPAR,VIEWFACTOR)
                ELSE
                    SOILTK = TSOIL(IHOUR) + FREEZE
                ENDIF

                ! Calculate components of heat balance. (latent heat flux (QE) is needed for water balance).
                IF(USEMEASET.EQ.0.AND.SIMSOILEVAP.EQ.1) THEN
                    CALL ENERGYCALC(SOILTK,GAMSOIL,PRESS(IHOUR),SOILTEMP(2),                                &
                                    VPD(IHOUR)/1000,RGLOBUND,TAIR(IHOUR) + FREEZE,THERMCOND(1),             &
                                    LAYTHICK(1),POREFRAC(1),SOILWP(1),DRYTHICK,TORTPAR,VIEWFACTOR,          &
                                    QH,QE,QN,QC,ESOIL)
                
                ! Or, do not calculate heat balance. Either if using measured ET for water balance,
                ! or if not simulating soil evaporation (which would render soil heat balance meaningless anyway).
                ELSE 
                    QE = 0
                    QH = 0
                    QN = 0
                    QC = 0
                ENDIF
            
                ! Layered water balance, outputs new soil water content, discharge,
                ! soil evaporation, overflow, thickness of dry layer.
                CALL WATBALLAY(IDAY,IHOUR,PPT(IHOUR),RUTTERB,RUTTERD,MAXSTORAGE,THROUGHFALL,RADINTERC,CANOPY_STORE,         &
                                EVAPSTORE, DRAINSTORE,SURFACE_WATERMM,POREFRAC,WETTINGBOT,WETTINGTOP,NLAYER,NROOTLAYER,     &
                                LAYTHICK,SOILTK,QE,TAIR(IHOUR) + FREEZE,VPD(IHOUR),WINDAH(IHOUR),ZHT,Z0HT,ZPD,PRESS(IHOUR), &
                                ETMM,ETMMSPEC,NOSPEC,USEMEASET,ETMEAS(IHOUR),FRACUPTAKESPEC,ICEPROP,FRACWATER,DRAINLIMIT,KSAT,BPAR,             &
                                WSOIL,WSOILROOT,DISCHARGE,DRYTHICKMIN,DRYTHICK,SOILEVAP,OVERFLOW,WATERGAIN,WATERLOSS,       &
                                PPTGAIN,KEEPWET,EXPINF,WS,WR,NRET,RETFUNCTION)
 
                ! Heat balance: soil T profile (SOILTEMP).
                IF(USEMEASET.EQ.0.AND.USEMEASSW.EQ.0)THEN
                    CALL HEATBALANCE(NLAYER,FRACWATER,POREFRAC,TAIR(IHOUR)+FREEZE,SOILTK,SOILTEMP,LAYTHICK,WATERGAIN,&
                                        WATERLOSS,PPTGAIN,THERMCOND)
                ELSE IF(USEMEASET.EQ.1)THEN
                    ! Do nothing.
                    SOILTEMP = SOILTK
                ENDIF
           
        
                ! Output water balance
                CALL OUTPUTWATBAL(IDAY,IHOUR,NROOTLAYER,NLAYER,WSOIL,                             &
                                  WSOILROOT,PPT(IHOUR),                                           &
                                  CANOPY_STORE,EVAPSTORE,DRAINSTORE,                              &
                                  SURFACE_WATERMM,ETMM,ETMM2,USEMEASET,ETMEAS(IHOUR),DISCHARGE,   &
                                  FRACWATER,WEIGHTEDSWP,KTOT,                                     &
                                  DRYTHICK,SOILEVAP,OVERFLOW,THERMCOND,FRACUPTAKE,SOILMOISTURE,   &
                                  FSOIL1,NSUMMED,TOTTMP,SOILTEMP-FREEZE,                          &
                                  TAIR(IHOUR),QH,QE,QN,QC,RGLOBUND,RGLOBABV,RGLOBABV12,RADINTERC, &
                                  ESOIL, TOTLAI, WTITLE,                                          &
                                  RADINTERC1, RADINTERC2, RADINTERC3,SCLOSTTOT,SOILWP,FRACAPAR)
                
                CALL SUMDAILYWAT(WSOIL,WSOILROOT,WEIGHTEDSWP,PPT,ETMM,ETMEAS,DISCHARGE,SOILEVAP,FSOIL1,SURFACE_WATERMM,QH,QE,QN,QC, &
                                RADINTERC,WSOILMEAN,WSOILROOTMEAN,SWPMEAN,PPTTOT,ETMMTOT,ETMEASTOT,DISCHARGETOT,SOILEVAPTOT,&
                                FSOILMEAN,TFALLTOT,QHTOT,QETOT,QNTOT,QCTOT,RADINTERCTOT)
            ENDIF

            ! Output hourly totals
            CALL OUTPUTHR(IDAY+1,IHOUR,NOTARGETS,ITARGETS,ISPECIES,TCAN,NOLAY,PPAR, &
                                PPS,PTRANSP,FOLLAY,THRAB,FCO2,FRESPF,FRESPW,FRESPB,FH2O,GSCAN,GBHCAN, &
                                FH2OCAN,FHEAT,VPD,TAIR,UMOLPERJ*RADABV(1:KHRS,1),PSILCAN,PSILCANMIN,CICAN,  &
                                ECANMAX,ACANMAX,ZEN,AZ)             ! rajout ZEN AZ mathias mars 2013


   
       
        !**********************************************************************
        END DO ! End hourly loop

        ! Calculate daily growth respiration
        TOTRESPWG = GRESP(WBINC,EFFYRW)
        TOTRESPBG = GRESP(BBINC,EFFYRW)
        TOTRESPFRG = GRESP(RBINC*FRFRAC,EFFYRF)
        TOTRESPCRG = GRESP(RBINC*(1.-FRFRAC),EFFYRW)
        TOTRESPFG = GRESP(FBINC,EFFYRF)

        ! Output daily totals
       
        CALL SUMDAILY(NOTARGETS,THRAB,FCO2,FRESPF,FRESPW,FRESPB,FRESPCR,FRESPFR,FH2O,FH2OCAN,FHEAT,TDYAB,TOTCO2,TOTRESPF,&
                        TOTRESPWM,TOTRESPB,TOTRESPCR,TOTRESPFR,TOTH2O,TOTH2OCAN,TOTHFX)
        
        
        CALL OUTPUTDY(IDAY+1,NOTARGETS,ITARGETS,ISPECIES,TDYAB,TOTCO2,TOTRESPF,TOTRESPWM,&
                                TOTRESPWG,TOTH2O,TOTH2OCAN,TOTHFX,TOTRESPCR,TOTRESPFR,TOTRESPFRG,&
                                TOTRESPCRG,TOTRESPFG,TOTRESPB,TOTRESPBG)
        
        IF(ISMAESPA)THEN
            CALL OUTPUTDYWAT(IDAY+1,WSOILMEAN,WSOILROOTMEAN,SWPMEAN,PPTTOT,ETMMTOT,ETMEASTOT,DISCHARGETOT,&
                                SOILEVAPTOT,FSOILMEAN,TFALLTOT,QHTOT,QETOT,QNTOT,QCTOT,RADINTERCTOT)
        ENDIF
         
        IF(ISIMUS.EQ.1)THEN
            CALL OUTPUTUS(IDAY+1,NOUSPOINTS,XLU,YLU,ZLU,UIBEAM,UIDIFF,PARUS,APARUS,PSUS,ETUS)
        ENDIF

        ! Go to next day of simulation
        IDAY = IDAY + NSTEP
        IF ((ISTART+IDAY) <= IEND) THEN
            CALL SKIPMET(MFLAG,NSTEP)
        END IF
        
    END DO ! End daily loop
    !**********************************************************************

    IF (IOHIST.EQ.1) CALL OUTPUTHIST(UHIST,HISTO,BINSIZE,NOTARGETS)
    
    !**********************************************************************
    ! Write diffuse transmittances to file
    REWIND (UTUTD)
    WRITE (UTUTD,1313) (IPT,TU(IPT),TD(IPT),RELDF(IPT),IPT=1,NUMPNT)
    1313  FORMAT (1X,I3,5X,F8.4,1X,F8.4,1X,F8.4)
    
    CALL SUBERROR('SIMULATION COMPLETED SUCCESSFULLY',IWARN,-1)
    
    CALL CLOSEF()
        
END PROGRAM MAESPA
!**********************************************************************


!**********************************************************************
SUBROUTINE ZEROSTART(HISTO,CANOPYDIMS)
! Set initial values of histogram to zero. 
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    
    REAL HISTO(MAXT,MAXHISTO)
    REAL CANOPYDIMS(6)
    
    HISTO = 0.0
    CANOPYDIMS = 0.0
    
    RETURN
END SUBROUTINE ZEROSTART

!**********************************************************************
SUBROUTINE ZEROD(TDYAB,TOTCO2,TOTRESPF,TOTRESPWM,TOTRESPB,TOTRESPCR,TOTRESPFR,      &
                    TOTH2O,TOTHFX,WSOILMEAN,WSOILROOTMEAN,SWPMEAN,PPTTOT,ETMMTOT,ETMEASTOT, &
                    DISCHARGETOT,SOILEVAPTOT,FSOILMEAN,TFALLTOT,QHTOT,QETOT,QNTOT,  &
                    QCTOT,RADINTERCTOT)
! This is subroutine to set the initial values of daily total variables
! to zero.
!**********************************************************************
    USE maestcom
    IMPLICIT NONE
    
    REAL TDYAB(MAXT,3)
    REAL TOTCO2(MAXT),TOTRESPF(MAXT),TOTRESPWM(MAXT)
    REAL TOTRESPB(MAXT),TOTRESPFR(MAXT),TOTRESPCR(MAXT)
    REAL TOTH2O(MAXT),TOTHFX(MAXT)
    REAL WSOILMEAN,WSOILROOTMEAN,PPTTOT,ETMMTOT,ETMEASTOT
    REAL DISCHARGETOT,SOILEVAPTOT,FSOILMEAN,TFALLTOT,QHTOT,QETOT,QNTOT
    REAL QCTOT,RADINTERCTOT,SWPMEAN

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
                    FH2O, GSCAN, GBHCAN, FHEAT, PPAR, PPS, PTRANSP, TCAN, FSOIL1,&
                    PSILCAN,PSILCANMIN,CICAN,NSUMMED,TOTTMP,ECANMAX,ACANMAX)
! This is subroutine to set the initial values of hourly total variables
! to zero.
! Note changes to dimensions of arrays (June 2008 RAD).
!**********************************************************************
    USE maestcom
    IMPLICIT NONE

    REAL THRAB(MAXT,MAXHRS,3),TCAN(MAXT,MAXHRS)
    REAL FCO2(MAXT,MAXHRS),FRESPF(MAXT,MAXHRS),FRESPW(MAXT,MAXHRS)
    REAL FRESPB(MAXT,MAXHRS),FRESPCR(MAXT,MAXHRS),FRESPFR(MAXT,MAXHRS)
    REAL GSCAN(MAXT,MAXHRS),FH2O(MAXT,MAXHRS),FHEAT(MAXT,MAXHRS)
    REAL PPAR(MAXT,MAXLAY,MAXHRS),PPS(MAXT,MAXLAY,MAXHRS)
    REAL PTRANSP(MAXT,MAXLAY,MAXHRS),FSOIL1,TOTTMP
    REAL PSILCAN(MAXT,MAXHRS),PSILCANMIN(MAXT,MAXHRS),CICAN(MAXT,MAXHRS)
    REAL GBHCAN(MAXT,MAXHRS)
    REAL ECANMAX(MAXT,MAXHRS),ACANMAX(MAXT,MAXHRS)
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
SUBROUTINE ZEROFSOIL(FSOIL1,NSUMMED,TOTTMP)
! Set FSOIL1 to zero, and NSUMMED.
!**********************************************************************     
    IMPLICIT NONE
    INTEGER NSUMMED
    REAL FSOIL1,TOTTMP
      
    FSOIL1 = 0.0
    NSUMMED = 0
    TOTTMP = 0.0
      
END SUBROUTINE ZEROFSOIL   
      
      
!**********************************************************************
SUBROUTINE SUMHR(APAR,ANIR,ATHR,ALEAF,RD,GSC,GBH,ET,HFX,TLEAF,FSOIL, PSIL,CI,        &
                    AREA,IHOUR,ILAY,ITAR,NOTARGETS,NUMPNT,NSUMMED,TOTTMP,&
                    PPAR,PPS,PTRANSP,THRAB,FCO2,FRESPF,GSCAN,GBHCAN,FH2O,FHEAT,TCAN,FSOIL1,  &
                    PSILCAN,PSILCANMIN,CICAN, ECANMAX, ACANMAX)
! Sum fluxes from each point to give hourly fluxes.
! Modified version of SUMHR to account for new looping order (June 2008 RAD).
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER ITAR,ILAY,IHOUR,NOTARGETS,NUMPNT,NSUMMED

    REAL THRAB(MAXT,MAXHRS,3)
    REAL FCO2(MAXT,MAXHRS),FRESPF(MAXT,MAXHRS),TCAN(MAXT,MAXHRS)
    REAL PSILCAN(MAXT,MAXHRS),PSILCANMIN(MAXT,MAXHRS),CICAN(MAXT,MAXHRS)
    REAL GSCAN(MAXT,MAXHRS),FH2O(MAXT,MAXHRS),FHEAT(MAXT,MAXHRS)
    REAL GBHCAN(MAXT,MAXHRS)
    REAL ACANMAX(MAXT,MAXHRS),ECANMAX(MAXT,MAXHRS)
    REAL PPAR(MAXT,MAXLAY,MAXHRS),PPS(MAXT,MAXLAY,MAXHRS)
    REAL PTRANSP(MAXT,MAXLAY,MAXHRS)
    REAL APAR,AREA,ALEAF,ET,ANIR,ATHR,RD,GSC,HFX,TLEAF,FSOIL1,FSOIL,TOTTMP
    REAL PSIL,CI,GBH

    ! Sum PAR, photosynthesis, & transpiration by layer
    PPAR(ITAR,ILAY,IHOUR) = PPAR(ITAR,ILAY,IHOUR) + APAR*UMOLPERJ*AREA
    PPS(ITAR,ILAY,IHOUR) = PPS(ITAR,ILAY,IHOUR) + ALEAF*AREA
    PTRANSP(ITAR,ILAY,IHOUR) = PTRANSP(ITAR,ILAY,IHOUR) + ET*AREA
  
    ! Sum all fluxes for the hour
    THRAB(ITAR,IHOUR,1) = THRAB(ITAR,IHOUR,1) + APAR*AREA
    THRAB(ITAR,IHOUR,2) = THRAB(ITAR,IHOUR,2) + ANIR*AREA
    THRAB(ITAR,IHOUR,3) = THRAB(ITAR,IHOUR,3) + ATHR*AREA
    FCO2(ITAR,IHOUR) = FCO2(ITAR,IHOUR) + ALEAF*AREA
    
    ! Foliage respiration in umol tree-1 s-1
    FRESPF(ITAR,IHOUR) = FRESPF(ITAR,IHOUR) + RD*AREA
    ! Transpiration in umol tree-1 s-1
    FH2O(ITAR,IHOUR) = FH2O(ITAR,IHOUR) + ET*AREA
    ! Maximum rates of transpiration and photosynthesis
    IF(ET.GT.ECANMAX(ITAR,IHOUR))THEN
        ECANMAX(ITAR, IHOUR) = ET
    ENDIF
    IF(ALEAF.GT.ACANMAX(ITAR,IHOUR))THEN
        ACANMAX(ITAR, IHOUR) = ALEAF
    ENDIF
    ! Stom cond in mol tree-1 s-1
    GSCAN(ITAR,IHOUR) = GSCAN(ITAR,IHOUR) + GSC*AREA
    ! Boundary layer conductance to heat
    GBHCAN(ITAR,IHOUR) = GBHCAN(ITAR,IHOUR) + GBH*AREA
    ! Heat flux in mol tree-1 s-1
    FHEAT(ITAR,IHOUR) = FHEAT(ITAR,IHOUR) + HFX*AREA
    ! Average leaf temperature - will be divided by total leaf area later. 
    TCAN(ITAR,IHOUR) = TCAN(ITAR,IHOUR) + TLEAF*AREA
    ! Average leaf water potential
    PSILCAN(ITAR,IHOUR) = PSILCAN(ITAR,IHOUR) + PSIL*AREA
    ! Lowest leaf water potential for the target tree.
    IF(PSIL.LT.PSILCANMIN(ITAR,IHOUR))THEN
       PSILCANMIN(ITAR,IHOUR) = PSIL
    ENDIF
    ! Average ci.
    CICAN(ITAR,IHOUR) = CICAN(ITAR,IHOUR) + CI*AREA
    
    ! Average FSOIL across all target trees and grid points.
    FSOIL1 = FSOIL1 + FSOIL*ET*AREA
    TOTTMP = TOTTMP + ET*AREA
    NSUMMED = NSUMMED + 1
    
    RETURN
END SUBROUTINE SUMHR


!**********************************************************************
SUBROUTINE SUMHRUS(IHOUR,NOUSPOINTS,GRDAREAI,AREAUS,PARUS,PARUSMEAN,PARUSSD,&
                    APARUS,PSUS,ETUS,THRABUS,FCO2US,FH2OUS)
     
! Sum fluxes from each understorey point to give hourly fluxes.
! Taken from MAESUS, Feb. '09 (RAD).
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER IHOUR,NOUSPOINTS
    REAL THRABUS(MAXHRS),FCO2US(MAXHRS),FH2OUS(MAXHRS)
    REAL PARUS(MAXHRS,MAXP),APARUS(MAXHRS,MAXP)
    REAL PSUS(MAXHRS,MAXP),ETUS(MAXHRS,MAXP)
    REAL AREAUS(MAXP),PARUSMEAN(MAXHRS),PARUSSD(MAXHRS)
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
    PARUSSD(IHOUR) = STDEV(PARUS(IHOUR, 1:NOUSPOINTS),NOUSPOINTS)

    RETURN
END SUBROUTINE SUMHRUS


!**********************************************************************
SUBROUTINE SUMDAILY(NOTARGETS,THRAB,FCO2,FRESPF,FRESPW,FRESPB,FRESPCR,FRESPFR,   &  
                    FH2O,FH2OCAN,FHEAT,TDYAB,TOTCO2,TOTRESPF,TOTRESPWM,TOTRESPB, &
                    TOTRESPCR,TOTRESPFR,TOTH2O,TOTH2OCAN,TOTHFX)
! Sum hourly fluxes to give daily ones
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER NOTARGETS,J,IHOUR,ITAR
    REAL THRAB(MAXT,MAXHRS,3),FCO2(MAXT,MAXHRS),FRESPF(MAXT,MAXHRS)
    REAL FRESPW(MAXT,MAXHRS)
    REAL FRESPB(MAXT,MAXHRS),FRESPCR(MAXT,MAXHRS),FRESPFR(MAXT,MAXHRS)
    REAL FH2O(MAXT,MAXHRS),FH2OCAN(MAXT,MAXHRS),FHEAT(MAXT,MAXHRS)
    REAL TDYAB(MAXT,3)
    REAL TOTCO2(MAXT),TOTRESPF(MAXT),TOTRESPWM(MAXT)
    REAL TOTRESPB(MAXT),TOTRESPFR(MAXT),TOTRESPCR(MAXT)
    REAL TOTH2O(MAXT),TOTH2OCAN(MAXT),TOTHFX(MAXT),CONVERT
       
    DO ITAR = 1,NOTARGETS
        DO IHOUR = 1,KHRS
            TOTCO2(ITAR) = TOTCO2(ITAR) + FCO2(ITAR,IHOUR)
            !print *, TOTCO2(ITAR), FCO2(ITAR,IHOUR)
            TOTRESPF(ITAR) = TOTRESPF(ITAR) + FRESPF(ITAR,IHOUR)
            TOTRESPWM(ITAR) = TOTRESPWM(ITAR) + FRESPW(ITAR,IHOUR)
            TOTRESPB(ITAR)  = TOTRESPB(ITAR) + FRESPB(ITAR,IHOUR)
            TOTRESPCR(ITAR) = TOTRESPCR(ITAR) + FRESPCR(ITAR,IHOUR) 
            TOTRESPFR(ITAR) = TOTRESPFR(ITAR) + FRESPFR(ITAR,IHOUR)
            TOTH2O(ITAR) = TOTH2O(ITAR) + FH2O(ITAR,IHOUR)
            TOTH2OCAN(ITAR) = TOTH2OCAN(ITAR) + FH2OCAN(ITAR,IHOUR)
            TOTHFX(ITAR) = TOTHFX(ITAR) + FHEAT(ITAR,IHOUR)
            DO J = 1,3
                TDYAB(ITAR,J) = TDYAB(ITAR,J) + THRAB(ITAR,IHOUR,J)
            END DO
        END DO
    END DO    

    CONVERT = SPERHR*1E-6

    TOTCO2 = TOTCO2*CONVERT
    TOTRESPF = TOTRESPF*CONVERT
    TOTRESPB = TOTRESPB*CONVERT
    TOTRESPWM = TOTRESPWM*CONVERT
    TOTRESPCR = TOTRESPCR*CONVERT
    TOTRESPFR = TOTRESPFR*CONVERT
    TOTH2O = TOTH2O*CONVERT
    TOTH2OCAN = TOTH2OCAN*CONVERT
    TOTHFX = TOTHFX*CONVERT      
    TDYAB = TDYAB*CONVERT
    
    RETURN
END SUBROUTINE SUMDAILY



