!**********************************************************************
! INOUT.FOR
! This file contains all the subroutines for reading and writing to
! input and output files.
! The main subroutines (called externally) are:
! OPENINPUTF - opens the input files
! open_output_files - opens the output files
! CLOSEF - closes the files
! INPUTSTR - reads the canopy structure file str.dat
! INPUTPHY - reads the physiology file phy.dat
! INPUTTREE - reads the trees.dat file
! INPUTCON - reads the control file confile.dat
! INPUTSOIL - reads the soil/understorey file soil.dat // DOES NOT EXIST! (RAD)
! INPUTWATBAL - read the water balance / soil file watpars.dat (RAD, 2008).
! OUTPUTDY - outputs daily fluxes
! OUTPUTHR - outputs hourly fluxes
! OUTPUTLAY - outputs to layer flux file
! OUTPUTHIST - outputs PAR histogram
! SORTTREES - sorts the trees into order of distance from the target tree
! INTERPOLATEP - calls the daily interpolation routines for physiology
! INTERPOLATET - calls the daily interpolation routines for tree dimensions
! NB The subroutines to do with reading the met file are in getmet.for.

! Subsidiary subroutines are:
! INPUTSTR
!   READCROWN - read crown shape 
!   READAERO - read aerodynamic properties (e.g. extinction of wind) 
!   READBETA - read leaf area density distribution
!   READALLOM - read parameters for biomass vs height & diameter
!   READLIA - read leaf incidence angle 
! READLIA uses the following
!   ANGLE - calculate the fraction of leaf area in each angle class
!   AVGLIA - calculate the average LIA for a given ellipsoidal parameter
!   INTEG - integrate leaf angle dist over specified range
!   FANG - ellipsoidal leaf angle density function
!   CALCELP - calculate ellipsoidal parameter from average LIA
! INPUTCON
!   READDATES - read in start & end dates of simulation
!   READMODEL - read in options for which model to use
!   READZEN - read in number of angle classes & layers to use
!   READHIST - read in details of PAR histogram required
!   READCCSCEN - read in climate change scenario
!   READOTC - read in effects of OTC on met data
! INPUTPHY
!   READAGEP - read number of age classes for physiology parameters
!   READPROP - read proportions of leaf area in each age class
!   READABSRP - get leaf absorptance, reflectance & transmittance arrays
!   READGS - read parameters of stomatal model
!   READLEAFN - read in leaf nitrogen concentrations
!   READJMAX - read in Jmax and Vcmax values
!   READRD - read in parameters of leaf respiration
!   READPHYARRAY - read array of physiological parameters 
!     (called by READLEAFN, READJMAX, READRD)
!   READRW - read in parameters for woody respiration
!   READRR - read in parameters for root respiration
!   READRB - read in parameters for branch respiration
!   RINTEG - convert physiology parameters spec. in layers to layers used
!     (called by READBASRP, READPHYARRAY)
! INPUTTREE
!   READPLOT - read dimensions of plot
!   READXYZ - read co-ordinates of trees
!   READTREEARRAY - read arrays of tree dimensions eg height, diameter
!   GETLEAFAREA - A subroutine to read in leaf area array.
!   CALCLAI - calculate plot LAI
!   READZPD - read canopy dimensions (roughness length etc)
!   READCONTREES - get list of target trees
!   INEDGES - determine whether given tree is in plot edges or not
! INTERPOLATEP
!   PHYINTERP - do interpolation of physiological parameters
! INTERPOLATET
!   PHENOL - calculate leaf area from phenology parameters
!   TREEINTERP - do interpolation of tree dimensions
! Unused: NOTREEGET, GETTREENO, READRSOIL
!**********************************************************************


!**********************************************************************
SUBROUTINE OPENINPUTF(CTITLE,TTITLE,PTITLE,STITLE,WTITLE,UTITLE,IWATFILE,KEEPZEN, &
                      IPOINTSI,ISIMUSI, in_path,out_path)
! This routine opens the input files.
! The filenames are defined in this routine.
!**********************************************************************
    USE switches
    USE maestcom
    
    IMPLICIT NONE
    INTEGER LEN1,IOERROR,IWATFILE, KEEPZEN, IPOINTSI,IPOINTS,ISIMUS,ISIMUSI
    CHARACTER(LEN=*) CTITLE, TTITLE, PTITLE, STITLE, WTITLE, UTITLE
    CHARACTER(LEN=*) in_path, out_path
    CHARACTER(LEN=256) :: fin_dir, fout_dir
    LOGICAL EXT
    
    ! Modified RAD
    NAMELIST /CONTROL/ IOHRLY,IOTUTD,IOHIST,IORESP,IOWATBAL,IOFORMAT,ISUNLA,KEEPZEN,IPOINTS,ISIMUS
    NAMELIST /flocations/ fin_dir, fout_dir   ! MGDK
    
    990 FORMAT (A80)     ! For reading titles in input files.
    
    ! Output file for errors and warnings
    OPEN (UERROR, FILE = 'Maeserr.dat', STATUS = 'UNKNOWN')
    
    ! Read input file as tradition from confile.dat with control switches
    OPEN (UCONTROL, FILE = 'confile.dat', STATUS = 'OLD',IOSTAT=IOERROR)
    IF(IOERROR.NE.0)THEN
        CALL SUBERROR('ERROR: CONFILE.DAT DOES NOT EXIST' ,IFATAL,0)
    ENDIF
    
    ! get locations of input files and where to write the output files
    READ (UCONTROL, flocations, IOSTAT=IOERROR)
    IF (IOERROR == -1) THEN
        ! i.e. it has reached the end of the file and not found the tag, I am sure 
        ! there is a nicer way to do this
        WRITE(*,*) 'You have chosen not to set the in/out directories, so using current dir'
        REWIND(UCONTROL)
    ENDIF
    
    READ (UCONTROL, 990) CTITLE
    CTITLE = TRIM(CTITLE)
    
    ! Default
    KEEPZEN = 0 
    IPOINTS = 0
    
    READ (UCONTROL, CONTROL, IOSTAT = IOERROR)
    IF (IOERROR.NE.0) CALL SUBERROR('WARNING: USING DEFAULT VALUES FOR CONTROL FILE', IWARN, IOERROR)  
    IPOINTSI = IPOINTS
    ISIMUSI = ISIMUS
    
    ! Input file with data on tree position and size
    OPEN (UTREES, FILE = trim(in_path)//'trees.dat', STATUS='OLD', IOSTAT=IOERROR)
    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('ERROR: TREES.DAT DOES NOT EXIST', IFATAL, 0)
    ENDIF
    
    ! Input file with water balance parameters (RAD)
    OPEN (UWATPARS, FILE = trim(in_path)//'watpars.dat', STATUS='OLD',IOSTAT=IOERROR)      
    IF(IOERROR.NE.0)THEN
        IWATFILE = 0
    ELSE 
        IWATFILE = 1
    ENDIF
    
    ! Input/output file with diffuse transmittances
    OPEN (UTUTD, FILE = 'tutd.dat', STATUS='UNKNOWN')  
       
    ! Input file for understorey parameters.
    ! Or if filename is missing:
    INQUIRE (FILE=trim(in_path)//'USTOREY.DAT', EXIST=EXT)
    IF(.NOT.EXT)THEN
        CALL SUBERROR('USTOREY.DAT NOT FOUND. NO UNDERSTOREY SIMULATED.',IWARN, 0)
        ISIMUSI = 0
    ENDIF
    
    IF(EXT)THEN 
        OPEN(USTOREYI, FILE=trim(in_path)//'ustorey.dat', STATUS='UNKNOWN',IOSTAT=IOERROR)
    ENDIF
      
    IF(ISIMUS.EQ.1.AND.IOERROR.EQ.0)THEN
        UTITLE = ' ' !READ (USTOREYI, 990) UTITLE  ! Why removed?
    ELSE
        UTITLE = ' '
    ENDIF
    
    ! Read titles from input files
    READ (UTREES, 990) TTITLE
    
    IF(IWATFILE.EQ.1)THEN
        READ (UWATPARS, 990) WTITLE  !RAD
        WTITLE = TRIM(WTITLE)
    ELSE
        WTITLE = ' '
    ENDIF
    
    RETURN
END SUBROUTINE OPENINPUTF



!**********************************************************************
SUBROUTINE open_output_files(ISIMUS,CTITLE,TTITLE,PTITLE,&
                            STITLE,MTITLE,VTITLE,WTITLE,    &
                            NSPECIES,SPECIESNAMES,OUT_PATH,ISMAESPA)
! This routine opens the output files.
! The filenames are defined in this routine.
! It also writes initial comments to the output files.
!**********************************************************************
    USE switches
    USE maestcom
    IMPLICIT NONE
    INTEGER NSPECIES, IOERROR, DUMMY, ISIMUS
    !CHARACTER(*), INTENT(IN) :: CTITLE,TTITLE,PTITLE,STITLE,MTITLE,WTITLE,VTITLE,out_path
    CHARACTER(*) :: CTITLE,TTITLE,PTITLE,STITLE,MTITLE,WTITLE,VTITLE,OUT_PATH
    LOGICAL ISMAESPA
    CHARACTER(30) SPECIESNAMES(MAXSP)
    
    ! Output file with daily fluxes
    IF (IODAILY .GT. 0 .AND. IOFORMAT .EQ. 0) THEN
        CALL open_file(trim(out_path)//'Dayflx.dat', UDAILY, 'write', 'asc', 'replace')
    ELSE IF (IODAILY .GT. 0 .AND. IOFORMAT .EQ. 1) THEN
        CALL open_file(trim(out_path)//'Dayflx.bin', UDAILY, 'write', 'bin', 'replace')
        CALL open_file(trim(out_path)//'Dayflx.hdr', UDAYHDR, 'write', 'asc', 'replace')  
    END IF
    
    ! Output file with hourly fluxes (if required).
    IF (IOHRLY.GT.0 .AND. IOFORMAT .EQ. 0) THEN
        CALL open_file(trim(out_path)//'hrflux.dat', UHRLY, 'write', 'asc', 'replace')
    ELSE IF (IOHRLY.GT.0 .AND. IOFORMAT .EQ. 1) THEN
        CALL open_file(trim(out_path)//'hrflux.bin', UHRLY, 'write', 'bin', 'replace')
        CALL open_file(trim(out_path)//'hrflux.hdr', UHRLYHDR, 'write', 'asc', 'replace')  
    ENDIF
    
    ! Output file with layer fluxes (if required).
    IF (IOHRLY.GT.1 .AND. IOFORMAT .EQ. 0) THEN
        CALL open_file(trim(out_path)//'layflx.dat', ULAY, 'write', 'asc', 'replace')
    ELSE IF (IOHRLY.GT.1 .AND. IOFORMAT .EQ. 1) THEN
        CALL open_file(trim(out_path)//'layflx.bin', ULAY, 'write', 'bin', 'replace')
        CALL open_file(trim(out_path)//'layflx.hdr', ULAYHDR, 'write', 'asc', 'replace')   
    ENDIF

    ! Output file with histogram (if required).
    IF (IOHIST.EQ.1 .AND. IOFORMAT .EQ. 0) THEN
        CALL open_file(trim(out_path)//'histo.dat', UHIST, 'write', 'asc', 'replace')
    ELSE IF (IOHIST.EQ.1 .AND. IOFORMAT .EQ. 1) THEN
        CALL open_file(trim(out_path)//'histo.bin', UHIST, 'write', 'bin', 'replace')
        CALL open_file(trim(out_path)//'histo.hdr', UHISTHDR, 'write', 'asc', 'replace')
    END IF

    ! Output file for respiration (if required).
    IF (IORESP.EQ.1 .AND. IOFORMAT .EQ. 0) THEN
        CALL open_file(trim(out_path)//'resp.dat', URESP, 'write', 'asc', 'replace')
        CALL open_file(trim(out_path)//'resphr.dat', URESPHR, 'write', 'asc', 'replace')
    ELSE IF (IORESP .EQ. 1 .AND. IOFORMAT .EQ. 1) THEN
        CALL open_file(trim(out_path)//'resp.bin', URESP, 'write', 'bin', 'replace')
        CALL open_file(trim(out_path)//'resp.hdr', URESPHDR, 'write', 'asc', 'replace')
        CALL open_file(trim(out_path)//'resphr.bin', URESPHR, 'write', 'bin', 'replace')
        CALL open_file(trim(out_path)//'resphr.hdr', URESPHRHDR, 'write', 'asc', 'replace')
    END IF
    
    CALL open_file(trim(out_path)//'wattest.dat', UWATTEST, 'write', 'asc', 'replace')
    
    ! Output file for water balance (if requested by setting IOWATBAL = 1 in confile.dat).
    IF (ISMAESPA .AND. IOFORMAT .EQ. 0) THEN
        CALL open_file(trim(out_path)//'watbal.dat', UWATBAL, 'write', 'asc', 'replace')
        CALL open_file(trim(out_path)//'watlay.dat', UWATLAY, 'write', 'asc', 'replace')
        CALL open_file(trim(out_path)//'swplay.dat', USWPLAY, 'write', 'asc', 'replace')    ! mathias décembre 2012
        CALL open_file(trim(out_path)//'watsoilt.dat', USOILT, 'write', 'asc', 'replace')
        !CALL open_file(trim(out_path)//'wattest.dat', UWATTEST, 'write', 'asc', 'replace')
        CALL open_file(trim(out_path)//'watupt.dat', UWATUPT, 'write', 'asc', 'replace')
        CALL open_file(trim(out_path)//'watbalday.dat', UWATDAY, 'write', 'asc', 'replace')   
    ELSE IF (ISMAESPA .AND. IOFORMAT .EQ. 1) THEN
        CALL open_file(trim(out_path)//'watbal.bin', UWATBAL, 'write', 'bin', 'replace')
        CALL open_file(trim(out_path)//'watbal.hdr', UWATBALHDR, 'write', 'asc', 'replace')
        CALL open_file(trim(out_path)//'watlay.bin', UWATLAY, 'write', 'bin', 'replace')
        CALL open_file(trim(out_path)//'watlay.hdr', UWATLAYHDR, 'write', 'asc', 'replace')
        CALL open_file(trim(out_path)//'watsoilt.bin', USOILT, 'write', 'bin', 'replace')
        CALL open_file(trim(out_path)//'watsoilt.hdr', USOILTHDR, 'write', 'asc', 'replace')
        CALL open_file(trim(out_path)//'wattest.bin', UWATTEST, 'write', 'bin', 'replace')
        !CALL open_file(trim(out_path)//'wattest.hdr', UWATTESTHDR, 'write', 'asc', 'replace')
        CALL open_file(trim(out_path)//'watupt.bin', UWATUPT, 'write', 'bin', 'replace')
        CALL open_file(trim(out_path)//'watupt.hdr', UWATUPTHDR, 'write', 'asc', 'replace')
        CALL open_file(trim(out_path)//'watbalday.bin', UWATDAY, 'write', 'bin', 'replace')
        CALL open_file(trim(out_path)//'watbalday.hdr', UWATDAYHDR, 'write', 'asc', 'replace')
    END IF
    
    ! Write to sunla flux     !!!!!
      IF(ISUNLA.EQ.1)THEN               ! Mathias 27/11/12
        CALL open_file(trim(out_path)//'sunla.dat', USUNLA, 'write', 'asc', 'replace')
      ENDIF

      
    IF(ISIMUS.EQ.1 .AND. IOFORMAT .EQ. 0) THEN
        CALL open_file(trim(out_path)//'uspar.dat', UPARUS, 'write', 'asc', 'replace')
    ELSE IF(ISIMUS.EQ.1 .AND. IOFORMAT .EQ. 1) THEN
        CALL open_file(trim(out_path)//'uspar.bin', UPARUS, 'write', 'bin', 'replace')
    ENDIF

    CALL write_header_information(NSPECIES,SPECIESNAMES, &
                                  CTITLE,TTITLE,PTITLE,STITLE, &
                                  MTITLE,WTITLE,VTITLE,ISMAESPA,ISIMUS)

    RETURN
END SUBROUTINE open_output_files

!**********************************************************************
SUBROUTINE write_header_information(NSPECIES,SPECIESNAMES, &
                                    CTITLE,TTITLE,PTITLE,STITLE,MTITLE,WTITLE,VTITLE, &
                                    ISMAESPA,ISIMUS)
!**********************************************************************

    USE switches
    USE maestcom
    
    IMPLICIT NONE
    INTEGER I,NSPECIES,ISIMUS
    CHARACTER(*), INTENT(IN) :: CTITLE,TTITLE,PTITLE,STITLE,MTITLE,WTITLE,VTITLE
    CHARACTER(30) SPECIESNAMES(MAXSP)
    LOGICAL ISMAESPA
    
  
    ! write headers to single ascii file
    IF (IOFORMAT .EQ. 0) THEN
        
        IF (ISUNLA.EQ.1) THEN
            WRITE (USUNLA,461)    ! MAthias 27/11/12
        END IF
        
        ! Write headings to daily flux file
        IF (IODAILY.GT.0) THEN
            WRITE (UDAILY, 991) 'Program:    ', VTITLE
            WRITE (UDAILY, 991) 'Control:    ', CTITLE
            WRITE (UDAILY, 991) 'Trees:      ', TTITLE
            WRITE (UDAILY, 991) 'Structure:  ', 'Multispecies' ! STITLE
            WRITE (UDAILY, 991) 'Physiology: ', 'Multispecies' !PTITLE
            WRITE (UDAILY, 991) 'Met data:   ', MTITLE
            IF(IOWATBAL.EQ.1)WRITE (UDAILY, 991) 'Water bal.: ', WTITLE
            WRITE (UDAILY, 990) ' '
    
            IF(NSPECIES.GT.1)THEN
                WRITE(UDAILY, *)'Species codes:'
                DO I = 1,NSPECIES
                    WRITE (UDAILY, *) I,  " : " // SPECIESNAMES(I)
                END DO
                WRITE(UDAILY,990) ' '
            ENDIF
    
            WRITE (UDAILY,501)
            WRITE (UDAILY,502)
            WRITE (UDAILY,503)
            WRITE (UDAILY,504)
            WRITE (UDAILY,505)
            WRITE (UDAILY,506)
            WRITE (UDAILY,507)
            WRITE (UDAILY,508)
            WRITE (UDAILY,509)
            WRITE (UDAILY,510)
            WRITE (UDAILY,511)
            WRITE (UDAILY,512)
            WRITE (UDAILY,990) ' '
            WRITE (UDAILY,513)
        END IF
        
        ! Comments to hourly output file (if required).
        IF (IOHRLY.gt.0) THEN
            WRITE (UHRLY, 991) 'Program:    ', VTITLE
            WRITE (UHRLY, 991) 'Control:    ', CTITLE
            WRITE (UHRLY, 991) 'Trees:      ', TTITLE
            WRITE (UHRLY, 991) 'Structure:  ', 'Multispecies' !STITLE
            WRITE (UHRLY, 991) 'Physiology: ', 'Multispecies' !PTITLE
            WRITE (UHRLY, 991) 'Met data:   ', MTITLE
            IF(IOWATBAL.EQ.1)WRITE (UHRLY, 991) 'Water bal.: ', WTITLE
            WRITE (UHRLY, 990) ' '
            IF(NSPECIES.GT.1)THEN
                WRITE(UHRLY, *)'Species codes:'
                DO I = 1,NSPECIES
                    WRITE (UHRLY, *) I,  " : " // SPECIESNAMES(I)
                END DO
                WRITE(UHRLY,990) ' '
            END IF
            WRITE (UHRLY,701)
            WRITE (UHRLY,702)
            WRITE (UHRLY,703)
            WRITE (UHRLY,704)
            WRITE (UHRLY,705)
            WRITE (UHRLY,706)
            WRITE (UHRLY,707)
            WRITE (UHRLY,708)
            WRITE (UHRLY,709)
            WRITE (UHRLY,710)
            WRITE (UHRLY,711)
            WRITE (UHRLY,712)
            WRITE (UHRLY,713)
            WRITE (UHRLY,714)
            WRITE (UHRLY,715)
            WRITE (UHRLY,716)
!            WRITE (UHRLY,717)
            WRITE (UHRLY,718)
            WRITE (UHRLY,719)
            WRITE (UHRLY,720)
            WRITE (UHRLY,721)
            WRITE (UHRLY,722)
            WRITE (UHRLY,723)
            WRITE (UHRLY,724)
            WRITE (UHRLY,726)
            WRITE (UHRLY,727)
            WRITE (UHRLY, 990) ' '
            WRITE (UHRLY,725)
        END IF
    
        ! Comments to respiration output file (if required).    
        IF (IORESP.gt.0) THEN
            WRITE (URESP, 991) 'Program:    ', VTITLE
            WRITE (URESP, 991) 'Control:    ', CTITLE
            WRITE (URESP, 991) 'Trees:      ', TTITLE
            WRITE (URESP, 991) 'Structure:  ', 'Multispecies'  !STITLE
            WRITE (URESP, 991) 'Physiology: ', 'Multispecies' !PTITLE
            WRITE (URESP, 991) 'Met data:   ', MTITLE
            WRITE (URESP, 990) ' '
            WRITE (URESP,601)
            WRITE (URESP,602)
            WRITE (URESP,603)
            WRITE (URESP,604)
            WRITE (URESP,605)
            WRITE (URESP,606)
            WRITE (URESP,607)
            WRITE (URESP,608)
            WRITE (URESP,609)
            WRITE (URESP,610)
            WRITE (URESP,611)
            WRITE (URESP, 990) ' '
            WRITE (URESP,612)
    
            WRITE (URESPHR, 991) 'Program:    ', VTITLE
            WRITE (URESPHR, 991) 'Control:    ', CTITLE
            WRITE (URESPHR, 991) 'Trees:      ', TTITLE
            WRITE (URESPHR, 991) 'Structure:  ', 'Multispecies' !STITLE
            WRITE (URESPHR, 991) 'Physiology: ', 'Multispecies' !PTITLE
            WRITE (URESPHR, 991) 'Met data:   ', MTITLE
            WRITE (URESPHR, 990) ' '
            WRITE (URESPHR,301)
            WRITE (URESPHR,302)
            WRITE (URESPHR,303)
            WRITE (URESPHR,304)
            WRITE (URESPHR,305)
            WRITE (URESPHR,306)
            WRITE (URESPHR, 990) ' '
            WRITE (URESPHR,307)
          END IF
    
        ! Write comments to layer output file (if required)
        IF (IOHRLY.GT.1) THEN
            WRITE (ULAY, 991) 'Program:    ', VTITLE
            WRITE (ULAY, 801)
            WRITE (ULAY, 802)
            WRITE (ULAY, 803)
            WRITE (ULAY, 804)
            WRITE (ULAY, *)
        END IF
        
        ! Write comments to water balance output file (if required). (RAD).
        IF (ISMAESPA) THEN
            WRITE (UWATBAL, 991) 'Program:                  ', VTITLE
            WRITE (UWATBAL, 992) 'Water balance parameters: ', WTITLE
            WRITE (UWATBAL, 990) '  '
            WRITE (UWATBAL, 401)
            WRITE (UWATBAL, 402)
            WRITE (UWATBAL, 403)
            WRITE (UWATBAL, 404)
            WRITE (UWATBAL, 405)
            WRITE (UWATBAL, 406)
            WRITE (UWATBAL, 407)
            WRITE (UWATBAL, 408)
            WRITE (UWATBAL, 409)
            WRITE (UWATBAL, 410)
            WRITE (UWATBAL, 411)
            WRITE (UWATBAL, 412)
            WRITE (UWATBAL, 413)
            WRITE (UWATBAL, 414)
            WRITE (UWATBAL, 415)
            WRITE (UWATBAL, 416)
            WRITE (UWATBAL, 417)
            WRITE (UWATBAL, 418)
            WRITE (UWATBAL, 419)
            WRITE (UWATBAL, 420)
            WRITE (UWATBAL, 421)
            WRITE (UWATBAL, 422)
            WRITE (UWATBAL, 423)
            WRITE (UWATBAL, 424)
            WRITE (UWATBAL, 425)
            WRITE (UWATBAL, 426)
            WRITE (UWATBAL, 427)
            WRITE (UWATBAL, 428)
            WRITE (UWATBAL, 429)
            WRITE (UWATBAL, 430)
            WRITE (UWATBAL, 432)
            WRITE (UWATBAL, 990) '  '
            WRITE (UWATBAL, 431)
    
            ! Daily water balance output file.
            WRITE (UWATDAY, 991) 'Program:                  ', VTITLE
            WRITE (UWATDAY, 992) 'Water balance parameters: ', WTITLE
            WRITE (UWATDAY, 990) '  '
            WRITE (UWATDAY, 441)
            WRITE (UWATDAY, 442)
            WRITE (UWATDAY, 443)
            WRITE (UWATDAY, 444)
            WRITE (UWATDAY, 445)
            WRITE (UWATDAY, 446)
            WRITE (UWATDAY, 447)
            WRITE (UWATDAY, 448)
            WRITE (UWATDAY, 449)
            WRITE (UWATDAY, 450)
            WRITE (UWATDAY, 451)
            WRITE (UWATDAY, 452)
            WRITE (UWATDAY, 453)
            WRITE (UWATDAY, 454)
            WRITE (UWATDAY, 455)
            WRITE (UWATDAY, 456)
            WRITE (UWATDAY, 990) '  '
            WRITE (UWATDAY, 457)
        END IF
        
        IF(ISIMUS.EQ.1)THEN
            WRITE(UPARUS, 991) 'Program:                  ', VTITLE
            WRITE(UPARUS, 990) '  '
            WRITE(UPARUS, 201)
            WRITE (UWATDAY, 990) '  '
            WRITE(UPARUS, 203)
            WRITE(UPARUS, 204)
            WRITE(UPARUS, 205)
            WRITE(UPARUS, 206)
            WRITE(UPARUS, 207)
            WRITE(UPARUS, 208)
            WRITE(UPARUS, 209)
            WRITE (UWATDAY, 990) '  '
            WRITE(UPARUS, 202)
        ENDIF
        
        
    ! write headers to a .hdr file, binary out
    ELSE IF (IOFORMAT .EQ. 1) THEN      
        ! Write headings to daily flux file
        IF (IODAILY.GT.0) THEN
            WRITE (UDAYHDR, 991) 'Program:    ', VTITLE
            WRITE (UDAYHDR, 991) 'Control:    ', CTITLE
            WRITE (UDAYHDR, 991) 'Trees:      ', TTITLE
            WRITE (UDAYHDR, 991) 'Structure:  ', 'Multispecies' ! STITLE
            WRITE (UDAYHDR, 991) 'Physiology: ', 'Multispecies' !PTITLE
            WRITE (UDAYHDR, 991) 'Met data:   ', MTITLE
            IF(IOWATBAL.EQ.1)WRITE (UDAYHDR, 991) 'Water bal.: ', WTITLE
            WRITE (UDAYHDR, 990) ' '
    
            IF(NSPECIES.GT.1)THEN
                WRITE(UDAYHDR, *)'Species codes:'
                DO I = 1,NSPECIES
                    WRITE (UDAYHDR, *) I,  " : " // SPECIESNAMES(I)
                END DO
                WRITE(UDAYHDR,990) ' '
            ENDIF
    
            WRITE (UDAYHDR,501)
            WRITE (UDAYHDR,502)
            WRITE (UDAYHDR,503)
            WRITE (UDAYHDR,504)
            WRITE (UDAYHDR,505)
            WRITE (UDAYHDR,506)
            WRITE (UDAYHDR,507)
            WRITE (UDAYHDR,508)
            WRITE (UDAYHDR,509)
            WRITE (UDAYHDR,510)
            WRITE (UDAYHDR,511)
            WRITE (UDAYHDR,512)
            WRITE (UDAYHDR,990) ' '
            WRITE (UDAYHDR,513)
        END IF
        
        ! Comments to hourly output file (if required).
        IF (IOHRLY.gt.0) THEN
            WRITE (UHRLYHDR, 991) 'Program:    ', VTITLE
            WRITE (UHRLYHDR, 991) 'Control:    ', CTITLE
            WRITE (UHRLYHDR, 991) 'Trees:      ', TTITLE
            WRITE (UHRLYHDR, 991) 'Structure:  ', 'Multispecies' !STITLE
            WRITE (UHRLYHDR, 991) 'Physiology: ', 'Multispecies' !PTITLE
            WRITE (UHRLYHDR, 991) 'Met data:   ', MTITLE
            IF(IOWATBAL.EQ.1)WRITE (UHRLYHDR, 991) 'Water bal.: ', WTITLE
            WRITE (UHRLYHDR, 990) ' '
            IF(NSPECIES.GT.1)THEN
                WRITE(UHRLYHDR, *)'Species codes:'
                DO I = 1,NSPECIES
                    WRITE (UHRLYHDR, *) I,  " : " // SPECIESNAMES(I)
                END DO
                WRITE(UHRLYHDR,990) ' '
            END IF
            WRITE (UHRLYHDR,701)
            WRITE (UHRLYHDR,702)
            WRITE (UHRLYHDR,703)
            WRITE (UHRLYHDR,704)
            WRITE (UHRLYHDR,705)
            WRITE (UHRLYHDR,706)
            WRITE (UHRLYHDR,707)
            WRITE (UHRLYHDR,708)
            WRITE (UHRLYHDR,709)
            WRITE (UHRLYHDR,710)
            WRITE (UHRLYHDR,711)
            WRITE (UHRLYHDR,712)
            WRITE (UHRLYHDR,713)
            WRITE (UHRLYHDR,714)
            WRITE (UHRLYHDR,715)
            WRITE (UHRLYHDR,716)
            !WRITE (UHRLYHDR,717)
            WRITE (UHRLYHDR,718)
            WRITE (UHRLYHDR,719)
            WRITE (UHRLYHDR,720)
            WRITE (UHRLYHDR,721)
            WRITE (UHRLYHDR,722)
            WRITE (UHRLYHDR,724)
            WRITE (UHRLYHDR, 990) ' '
            WRITE (UHRLYHDR,725)
        END IF
    
        ! Comments to respiration output file (if required).    
        IF (IORESP.gt.0) THEN
            WRITE (URESPHDR, 991) 'Program:    ', VTITLE
            WRITE (URESPHDR, 991) 'Control:    ', CTITLE
            WRITE (URESPHDR, 991) 'Trees:      ', TTITLE
            WRITE (URESPHDR, 991) 'Structure:  ', 'Multispecies'  !STITLE
            WRITE (URESPHDR, 991) 'Physiology: ', 'Multispecies' !PTITLE
            WRITE (URESPHDR, 991) 'Met data:   ', MTITLE
            WRITE (URESPHDR, 990) ' '
            WRITE (URESPHDR,601)
            WRITE (URESPHDR,602)
            WRITE (URESPHDR,603)
            WRITE (URESPHDR,604)
            WRITE (URESPHDR,605)
            WRITE (URESPHDR,606)
            WRITE (URESPHDR,607)
            WRITE (URESPHDR,608)
            WRITE (URESPHDR,609)
            WRITE (URESPHDR,610)
            WRITE (URESPHDR,611)
            WRITE (URESPHDR, 990) ' '
            WRITE (URESPHDR,612)
    
            WRITE (URESPHRHDR, 991) 'Program:    ', VTITLE
            WRITE (URESPHRHDR, 991) 'Control:    ', CTITLE
            WRITE (URESPHRHDR, 991) 'Trees:      ', TTITLE
            WRITE (URESPHRHDR, 991) 'Structure:  ', 'Multispecies' !STITLE
            WRITE (URESPHRHDR, 991) 'Physiology: ', 'Multispecies' !PTITLE
            WRITE (URESPHRHDR, 991) 'Met data:   ', MTITLE
            WRITE (URESPHRHDR, 990) ' '
            WRITE (URESPHRHDR,301)
            WRITE (URESPHRHDR,302)
            WRITE (URESPHRHDR,303)
            WRITE (URESPHRHDR,304)
            WRITE (URESPHRHDR,305)
            WRITE (URESPHRHDR,306)
            WRITE (URESPHRHDR, 990) ' '
            WRITE (URESPHRHDR,307)
          END IF
    
        ! Write comments to layer output file (if required)
        IF (IOHRLY.GT.1) THEN
            WRITE (ULAYHDR, 991) 'Program:    ', VTITLE
            WRITE (ULAYHDR, 801)
            WRITE (ULAYHDR, 802)
            WRITE (ULAYHDR, 803)
            WRITE (ULAYHDR, 804)
            WRITE (ULAYHDR, *)
        END IF
    
        ! Write comments to water balance output file (if required). (RAD).
        IF (ISMAESPA) THEN
            WRITE (UWATBALHDR, 991) 'Program:                  ', VTITLE
            WRITE (UWATBALHDR, 992) 'Water balance parameters: ', WTITLE
            WRITE (UWATBALHDR, 990) '  '
            WRITE (UWATBALHDR, 401)
            WRITE (UWATBALHDR, 402)
            WRITE (UWATBALHDR, 403)
            WRITE (UWATBALHDR, 404)
            WRITE (UWATBALHDR, 405)
            WRITE (UWATBALHDR, 406)
            WRITE (UWATBALHDR, 407)
            WRITE (UWATBALHDR, 408)
            WRITE (UWATBALHDR, 409)
            WRITE (UWATBALHDR, 410)
            WRITE (UWATBALHDR, 411)
            WRITE (UWATBALHDR, 412)
            WRITE (UWATBALHDR, 413)
            WRITE (UWATBALHDR, 414)
            WRITE (UWATBALHDR, 415)
            WRITE (UWATBALHDR, 416)
            WRITE (UWATBALHDR, 417)
            WRITE (UWATBALHDR, 418)
            WRITE (UWATBALHDR, 419)
            WRITE (UWATBALHDR, 420)
            WRITE (UWATBALHDR, 421)
            WRITE (UWATBALHDR, 422)
            WRITE (UWATBALHDR, 423)
            WRITE (UWATBALHDR, 424)
            WRITE (UWATBALHDR, 425)
            WRITE (UWATBALHDR, 426)
            WRITE (UWATBALHDR, 427)
            WRITE (UWATBALHDR, 428)
            WRITE (UWATBALHDR, 429)
            WRITE (UWATBALHDR, 430)
            WRITE (UWATBALHDR, 990) '  '
            WRITE (UWATBALHDR, 431)
    
            ! Daily water balance output file.
            WRITE (UWATDAYHDR, 991) 'Program:                  ', VTITLE
            WRITE (UWATDAYHDR, 992) 'Water balance parameters: ', WTITLE
            WRITE (UWATDAYHDR, 990) '  '
            WRITE (UWATDAYHDR, 441)
            WRITE (UWATDAYHDR, 442)
            WRITE (UWATDAYHDR, 443)
            WRITE (UWATDAYHDR, 444)
            WRITE (UWATDAYHDR, 445)
            WRITE (UWATDAYHDR, 446)
            WRITE (UWATDAYHDR, 447)
            WRITE (UWATDAYHDR, 448)
            WRITE (UWATDAYHDR, 449)
            WRITE (UWATDAYHDR, 450)
            WRITE (UWATDAYHDR, 451)
            WRITE (UWATDAYHDR, 452)
            WRITE (UWATDAYHDR, 453)
            WRITE (UWATDAYHDR, 454)
            WRITE (UWATDAYHDR, 455)
            WRITE (UWATDAYHDR, 456)
            WRITE (UWATDAYHDR, 990) '  '
            WRITE (UWATDAYHDR, 457)
        END IF
    END IF
    
    461   format('DOY   Hour    Tree    IPT    SUNLA   AREA    BEXT    FBEAM   ZEN  ABSRPPAR  ABSRPNIR ABSRPTH BFPAR DFPAR BFNIR DFNIR DFTHR SCLOSTPAR SCLOSTNIR SCLOSTTH DOWNTH PARABV NIRABV THRABV')   ! Modification Mathias 27/11/12

    990 FORMAT (A80)
    991 FORMAT (A12,A80) ! For writing comments to output files.
    992 FORMAT (A25, A55)
    
    501 FORMAT('DOY: simulation date')
    502 FORMAT('Tree: tree number')
    503 FORMAT('Spec: tree species number')
    504 FORMAT('absPAR:   absorbed PAR              MJ tree-1 d-1')
    505 FORMAT('absNIR:   absorbed NIR              MJ  tree-1 d-1')
    506 FORMAT('absTherm: absorbed thermal          MJ tree-1 d-1')
    507 FORMAT('totPs: gross photosynthesis         mol tree-1 d-1')
    508 FORMAT('totRf: daily foliar respiration     mol tree-1 d-1')
    509 FORMAT('netPs: photosyn. net of foliar resp   mol tree-1 d-1')
    510 FORMAT('totLE1: daily transpiration         mol H2O tree-1 d-1')
    511 FORMAT('totLE2: daily transpirn (CANOPY calc) mol H2O m-2 d-1')
    512 FORMAT('totH:  daily sensible heat flux     MJ tree-1 d-1')
    513 FORMAT('Columns: DOY Tree Spec absPAR absNIR absTherm', &
                &' totPs totRf netPs', &
                &' totLE1 totLE2 totH')

    701 FORMAT('DOY: simulation date')
    702 FORMAT('Tree: tree number')
    703 FORMAT('Spec: tree species number')
    704 FORMAT('Hour:  hour of the day')
    705 FORMAT('hrPAR: absorbed PAR              umol tree-1 s-1')
    706 FORMAT('hrNIR: absorbed NIR              W tree-1')
    707 FORMAT('hrTHM: absorbed thermal          W tree-1')
    708 FORMAT('hrPS: photosynthesis (net of leaf resp) umol tree-1 s-1')
    709 FORMAT('hrRf:  hourly leaf respiration   umol tree-1 s-1')
    710 FORMAT('hrRmW: hourly stem + branch Rm   umol tree-1 s-1')
    711 FORMAT('hrLE:  hourly transpiration      mmol tree-1 s-1')
    712 FORMAT('LECAN: hourly transpirn: CANOPY calc : mmol H2O m-2 s-1')
    713 FORMAT('Gscan: canopy stomatal conductance : mol CO2 tree-1 s-1')
    714 FORMAT('Gbhcan: canopy boundary layer conductance to heat : mol tree-1 s-1')
    715 FORMAT('hrH:   hourly sensible heat flux:  MJ tree-1 s-1')
    716 FORMAT('TCAN: Average foliage temperature (deg C)')
   ! 717 FORMAT('ELMAX: Canopy maximum leaf transpiration rate (mmol m-2 s-1)')
    718 FORMAT('ALMAX: Canopy maximum leaf photosynthesis rate (umol m-2 s-1)')
    719 FORMAT('PSIL: Canopy average leaf water potential (MPa)')
    720 FORMAT('PSILMIN: Canopy minimum leaf water potential (MPa)')
    721 FORMAT('CI : Canopy average intercellular CO2 conc. (ppm)')
    722 FORMAT('TAIR: Air temperature (deg C)')
    723 FORMAT('VPD: vapor pressure deficit (kPa)')
    724 FORMAT('PAR: Above-canopy incident PAR (umol m-2 s-1)')
    726 FORMAT('ZEN: Zenithal angle (rad)')                      ! mathias mars 2013
    727 FORMAT('AZ: Asimutal angle (rad)')
    725 FORMAT('Columns: DOY Tree Spec HOUR hrPAR hrNIR hrTHM', &
               ' hrPs hrRf hrRmW hrLE', &
               ' LECAN Gscan Gbhcan hrH TCAN ALMAX PSIL PSILMIN CI TAIR VPD PAR ZEN AZ')   !10E-3*ECANMAX(ITAR,IHOUR),

    801 FORMAT(' Fluxes for each layer on an hourly basis')
    802 FORMAT(' Rows: absorbed PAR (umol m-2 leaf s-1) ')
    803 FORMAT('       photosynthesis net of Rleaf (umol m-2 leaf s-1) ')
    804 FORMAT('       transpiration (umol m-2 leaf s-1) ')

    601 FORMAT('Daily maintenance and growth respiration components')
    602 FORMAT('Rmf: Foliage maintenance resp.    mol m-2 d-1')
    603 FORMAT('Rmw: Stem maintenance resp.       mol m-2 d-1')
    604 FORMAT('RmB: Branch maintenance resp.     mol m-2 d-1')
    605 FORMAT('Rmcr: Coarse root maintenance resp. mol m-2 d-1')
    606 FORMAT('Rmfr: Fine root maintenance resp. mol m-2 d-1')
    607 FORMAT('Rgf: Foliage growth resp.         mol m-2 d-1')
    608 FORMAT('Rgw: Stem growth resp.            mol m-2 d-1')
    609 FORMAT('Rgb: Branch growth resp.          mol m-2 d-1')
    610 FORMAT('Rgcr: Coarse root growth resp.    mol m-2 d-1')
    611 FORMAT('Rgfr: Fine root growth resp.      mol m-2 d-1')
    612 FORMAT('Columns: DOY Tree Species Rmf Rmw Rmb Rmcr Rmfr Rgf Rgw Rgb Rgcr Rgfr')

    301 FORMAT('Hourly maintenance respiration components')
    302 FORMAT('Rmf: Foliage maintenance resp.    umol m-2 s-1')
    303 FORMAT('Rmw: Stem maintenance resp.       umol m-2 s-1')
    304 FORMAT('RmB: Branch maintenance resp.     umol m-2 s-1')
    305 FORMAT('Rmcr: Coarse root maintenance resp. umol m-2 s-1')
    306 FORMAT('Rmfr: Fine root maintenance resp. umol m-2 s-1')
    307 FORMAT('Columns: DOY, Hr, Rmf, Rmw, Rmb, Rmcr, Rmfr')

    401 FORMAT('Half-hourly water and heat balance components.')
    402 FORMAT('wsoil: total soil water storage                       mm')
    403 FORMAT('wsoilroot: soil water storage in rooted zone          mm')
    404 FORMAT('ppt : precipitation                                   mm')
    405 FORMAT('canopystore : storage of intercepted rain             mm')
    406 FORMAT('evapstore : evaporation of wet canopy                 mm')
    407 FORMAT('drainstore : drainage of wet canopy                   mm')
    408 FORMAT('tfall : throughfall of rain                           mm')
    409 FORMAT('et : modelled canopy transpiration                    mm')
    410 FORMAT('etmeas: measured ET, if provided in input             mm')
    411 FORMAT('discharge: drainage at bottom of profile              mm')
    412 FORMAT('overflow: over-land flow                              mm')
    413 FORMAT('weightedswp: soil water potential weighted by roots  MPa')
    414 FORMAT('ktot: soil to leaf hydr. cond.        mmol m-2 s-1 MPa-1')
    415 FORMAT('drythick: thickness of dry surface layer              mm')
    416 FORMAT('soilevap: soil evaporation                            mm')
    417 FORMAT('soilmoist: measured soil water content      (units vary)')
    418 FORMAT('fsoil: soil water modifier function                (0-1)')
    419 FORMAT('qh: sensible heat flux                             W m-2')
    420 FORMAT('qe: latent heat flux                               W m-2')
    421 FORMAT('qn: net radiation                                  W m-2')
    422 FORMAT('qc: soil heat transport                            W m-2')
    423 FORMAT('rglobund: net radiation underneath canopy          W m-2')
    424 FORMAT('rglobabv: net radiation above canopy               W m-2')
    425 FORMAT('radinterc: total radiation intercepted by canopy   W m-2')
    426 FORMAT('rnet: net radiation above the canopy               W m-2')
    427 FORMAT('totlai: leaf area index                           m2 m-2')
    428 FORMAT('tair: air temperature                              deg C')
    429 FORMAT('soilt1, soilt2: soil T in 1st and 2nd layer        deg C')
    430 FORMAT('fracw1,fracw2: water content 1st and 2nd layer    m3 m-3')

    431 FORMAT('Columns: day hour wsoil wsoilroot ppt canopystore         &
                &evapstore drainstore tfall et etmeas discharge overflow  &
                &weightedswp ktot drythick soilevap                 &
                &soilmoist fsoil qh qe qn qc rglobund                     &
                &rglobabv radinterc rnet totlai tair soilt1 soilt2        &
                &fracw1 fracw2 fracaPAR')
    432 FORMAT('FracaPAR: fraction of absorbed PAR')   
                
    441 FORMAT('Daily water and heat balance components.')
    442 FORMAT('wsoil: total soil water storage                       mm')
    443 FORMAT('wsoilroot: soil water storage in rooted zone          mm')
    444 FORMAT('swp: weighted soil water potential                   MPa')
    445 FORMAT('ppt : precipitation                                   mm')
    446 FORMAT('tfall : throughfall of rain                           mm')
    447 FORMAT('et : modelled canopy transpiration                    mm')
    448 FORMAT('etmeas: measured ET, if provided in input             mm')
    449 FORMAT('discharge: drainage at bottom of profile              mm')
    450 FORMAT('soilevap: soil evaporation                            mm')
    451 FORMAT('fsoil: soil water modifier function                (0-1)')
    452 FORMAT('qh: sensible heat flux                      MJ m-2 day-1')
    453 FORMAT('qe: latent heat flux                        MJ m-2 day-1')
    454 FORMAT('qn: net radiation                           MJ m-2 day-1')
    455 FORMAT('qc: soil heat transport                     MJ m-2 day-1')
    456 FORMAT('radinterc: total radiation intercepted      MJ m-2 day-1')

457     FORMAT('Columns: day wsoil wsoilroot swp ppt &
              &tfall et etmeas discharge soilevap &
              &fsoil qh qe qn qc radinterc')
        
201           FORMAT('Understorey simulation results by timestep and point')
202           FORMAT('Columns: day hour point X Y Z PARbeam PARdiffuse PARtotal APAR hrPSus hrETus')
203           FORMAT('day : day of simulation (1,2,etc.)')
204           FORMAT('hour: timestep (1,2,etc.)')
205           FORMAT('X,Y,Z: spatial coordinates of understorey test point (m)')
206           FORMAT('PARbeam, PARdiffuse, PARtot : PAR reaching the top of the understorey, in direct, diffuse or total (mu mol m-2 s-1)')
207           FORMAT('APAR : absorbed PAR by the understorey (mu mol m-2 s-1) ')
208           FORMAT('hrPSus : photosynthesis for the understorey point (mu mol m-2 s-1)')            
209           FORMAT('hrETus : transpiration for the understorey point (If zero it is not calculated) (mmol m-2 s-1)')

    
END SUBROUTINE write_header_information




!**********************************************************************
SUBROUTINE CLOSEF()
! This routine closes the open files.
!**********************************************************************
    USE switches
    USE maestcom
    IMPLICIT NONE
    
    CLOSE(USUNLA)  ! Mathias 27/11/12
    CLOSE(STDIN)
    CLOSE(UTREES)
    CLOSE(USTR)
    CLOSE(UPHY)
    CLOSE(UMET)
    CLOSE(UTUTD)
    CLOSE(UERROR)
    CLOSE(UWATPARS) !RAD
    IF (IODAILY.GT.0 .AND. IOFORMAT .EQ. 0) THEN
        CLOSE(UDAILY)
    ELSE IF (IODAILY.GT.0 .AND. IOFORMAT .EQ. 1) THEN
        CLOSE(UDAILY)
        CLOSE(UDAYHDR)
    END IF
    
    IF (IOHRLY.GT.0 .AND. IOFORMAT .EQ. 0) THEN
        CLOSE(UHRLY)
    ELSE IF (IOHRLY.GT.0 .AND. IOFORMAT .EQ. 1) THEN
        CLOSE(UHRLY)
        CLOSE(UHRLYHDR)
    END IF    
    
    IF (IOHRLY.GT.1 .AND. IOFORMAT .EQ. 0) THEN
        CLOSE(ULAY)
    ELSE IF (IOHRLY.GT.1 .AND. IOFORMAT .EQ. 1) THEN
        CLOSE(ULAY)
        CLOSE(ULAYHDR)
    END IF
    
    IF (IOHIST.EQ.1 .AND. IOFORMAT .EQ. 0) THEN
        CLOSE(UHIST)
    ELSE IF (IOHIST.EQ.1 .AND. IOFORMAT .EQ. 1) THEN
        CLOSE(UHIST)
        CLOSE(UHISTHDR)
    END IF
    
    IF (IORESP.EQ.1 .AND. IOFORMAT .EQ. 0) THEN
        CLOSE(URESP)
    ELSE IF (IORESP.EQ.1 .AND. IOFORMAT .EQ. 1) THEN
        CLOSE(URESP)
        CLOSE(URESPHDR)
    END IF
  
    IF (IOWATBAL.EQ.1 .AND. IOFORMAT .EQ. 0) THEN
        CLOSE(UWATBAL)     
        CLOSE(UWATDAY)     
        CLOSE(UWATLAY)     
        CLOSE(USWPLAY) ! mathias décembre 2012     
        CLOSE(USOILT)      
        CLOSE(UWATUPT)
    ELSE IF (IOWATBAL.EQ.1 .AND. IOFORMAT .EQ. 1) THEN
        CLOSE(UWATBAL)
        CLOSE(UWATBALHDR)
        CLOSE(UWATDAY) 
        CLOSE(UWATDAYHDR) 
        CLOSE(UWATLAY) 
        CLOSE(USWPLAY) ! mathias décembre 2012     
        CLOSE(UWATLAYHDR) 
        CLOSE(USOILT)  
        CLOSE(USOILTHDR)  
        CLOSE(UWATUPT) 
        CLOSE(UWATUPTHDR) 
    ENDIF

    RETURN
END SUBROUTINE CLOSEF


!**********************************************************************
SUBROUTINE INPUTCON(ISTART, IEND, NSTEP,                                    &
                    NUMPNT,NOLAY,PPLAY,NZEN,DIFZEN,NAZ,                     &
                    MODELGS, MODELJM, MODELRD, MODELSS, MODELRW, ITERMAX,   &
                    IOHIST, BINSIZE, ICC, CO2INC, TINC,                     &
                    IOTC, TOTC, WINDOTC, PAROTC, FBEAMOTC,                  &
                    IWATFILE,                            &
                    NSPECIES, SPECIESNAMES, PHYFILES, STRFILES)
! Read in the information from the control file.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE

    REAL DIFZEN(MAXANG)
    INTEGER PPLAY,ISTART,IEND,NSTEP,NUMPNT,NOLAY,PPLY,NZEN,NAZ,MODELGS
    INTEGER MODELJM,MODELRD,MODELSS,MODELRW,ITERMAX,NSPECIES
    INTEGER IOTC,IOHIST,IWATFILE,ICC
    CHARACTER(30) SPECIESNAMES(MAXSP)
    CHARACTER(30) PHYFILES(MAXSP)
    CHARACTER(30) STRFILES(MAXSP)
    !CHARACTER STITLE*60
    REAL BINSIZE,CO2INC,TINC,TOTC,WINDOTC,PAROTC,FBEAMOTC
    
    ! if reading from the confile.dat  
    CALL READDATES(UCONTROL, ISTART, IEND, NSTEP)
    CALL READSPECIES(UCONTROL, NSPECIES, SPECIESNAMES,PHYFILES, STRFILES)
    CALL READZEN(UCONTROL, NUMPNT, NOLAY, PPLAY, NZEN, NAZ, DIFZEN)
    CALL READMODEL(UCONTROL, MODELGS, MODELJM, MODELRD,MODELSS, MODELRW, ITERMAX)
    CALL READHIST(UCONTROL,IOHIST,BINSIZE)
    CALL READCCSCEN(UCONTROL,ICC,CO2INC,TINC)
    CALL READOTC(UCONTROL,IOTC,TOTC,WINDOTC,PAROTC,FBEAMOTC)
    
    RETURN
END SUBROUTINE INPUTCON


!**********************************************************************
SUBROUTINE INPUTWATBAL(BPAR, PSIE, KSAT, ROOTRESIST, ROOTRESFRAC,   &
                        ROOTRADTABLE, ROOTDENSTABLE,ROOTMASSTOTTABLE,                  &
                        MINROOTWP,MINLEAFWP,PLANTKTABLE, KSCALING, THROUGHFALL,    &
                        REASSIGNRAIN,RUTTERB,RUTTERD,MAXSTORAGE,    &
                        DRAINLIMIT,ROOTXSECAREA,EQUALUPTAKE,        &
                        NLAYER, NROOTLAYER, LAYTHICK, INITWATER,    &
                        FRACROOTTABLE, POREFRAC, SOILTEMP, KEEPWET,      &
                        DRYTHICKMIN,TORTPAR,SIMTSOIL,RETFUNCTION,   &
                        FRACORGANIC, EXPINF,WSOILMETHOD,USEMEASET,  &
                        USEMEASSW,SIMSOILEVAP,USESTAND,ALPHARET,WS,WR,  &
                        NRET,DATESKP,NOKPDATES,DATESROOT,NOROOTDATES,NOROOTSPEC)
! Read in water balance parameters
!**********************************************************************

    USE maestcom
    IMPLICIT NONE

    INTEGER NLAYER,NROOTLAYER,REASSIGNRAIN, SIMTSOIL, EQUALUPTAKE
    INTEGER WSOILMETHOD,FRACROOTLEN,LASTENTRY,RETFUNCTION
    INTEGER USEMEASET,USEMEASSW,KEEPWET,I,SIMSOILEVAP
    INTEGER USESTAND
    INTEGER NOKPDATES, DATESKP(maxdate), DATESROOT(maxdate), NOROOTDATES, NOROOTSPEC
    REAL MINLEAFWP(MAXSP),MINROOTWP
    REAL FRACROOTTABLE(MAXSOILLAY,maxdate,MAXSP),LAYTHICK(MAXSOILLAY)
    REAL INITWATER(MAXSOILLAY),POREFRAC(MAXSOILLAY)
    REAL BPAR(MAXSOILLAY), PSIE(MAXSOILLAY), KSAT(MAXSOILLAY)
    REAL SOILTEMP(MAXSOILLAY),DRAINLIMIT(MAXSOILLAY)
    REAL FRACORGANIC(MAXSOILLAY),FRACSUM
    REAL ROOTRESIST,ROOTRADTABLE(maxdate),ROOTDENSTABLE(maxdate),ROOTMASSTOTTABLE(maxdate)
    REAL MAXSTORAGE,COREWATER,EXPINF,DRYTHICKMIN,RUTTERB,RUTTERD
    REAL THROUGHFALL,ROOTRESFRAC,PLANTKTABLE(maxdate),TORTPAR,ROOTXSECAREA
    REAL KSCALING,ROOTBETA
    REAL ALPHARET(MAXSOILLAY),WS(MAXSOILLAY),WR(MAXSOILLAY),NRET(MAXSOILLAY)
    
    LOGICAL ANYFIX

    ! Init.
    POREFRAC = 0.0
    INITWATER = 0.0
    BPAR = 0.0
    PSIE = 0.0
    KSAT = 0.0
    SOILTEMP = 0.0
    DRAINLIMIT = 0.0
    KEEPWET = 0
    SIMTSOIL = 0
    FRACORGANIC = 0.0
    COREWATER = 0.0
    EXPINF = 0.0
    ROOTRESIST = 0.0
    KSCALING = 0.0
    ROOTBETA = 0.0
    ALPHARET =0.0
    WS=0.0
    WR=0.0
    NRET=0.0
    
    ! Read all namelists in the "watpars.dat" file.
    CALL READWATCONTROL(UWATPARS,KEEPWET,SIMTSOIL,REASSIGNRAIN,WSOILMETHOD,RETFUNCTION,EQUALUPTAKE, &
                        USEMEASET,USEMEASSW,SIMSOILEVAP,USESTAND)
    
    CALL READWATTFALL(UWATPARS, RUTTERB,RUTTERD,MAXSTORAGE,THROUGHFALL)

    CALL READINFILT(UWATPARS, EXPINF)

    CALL READLAYPARS(UWATPARS, NLAYER, LAYTHICK, COREWATER,POREFRAC, DRAINLIMIT, FRACORGANIC)

    ! Fill LAYTHICK; needed for root calculations.
    CALL FILLWITHLAST(LAYTHICK, MAXSOILLAY, NLAYER, -900.0)    
    
    CALL READROOTPARS(UWATPARS,ROOTRESFRAC, ROOTRADTABLE,ROOTDENSTABLE,ROOTMASSTOTTABLE, NROOTLAYER,FRACROOTTABLE, &
        LAYTHICK, ROOTBETA,DATESROOT,NOROOTDATES,NOROOTSPEC) 
        
    CALL READPLANTPARS(UWATPARS,MINROOTWP,MINLEAFWP,PLANTKTABLE, KSCALING,DATESKP,NOKPDATES)
    
    CALL READSOILRET(UWATPARS, BPAR, PSIE, KSAT,ALPHARET,WS,WR,NRET)

    IF(NROOTLAYER.GT.NLAYER)THEN
        NLAYER = NROOTLAYER
    ENDIF
    
    CALL READINITPARS (UWATPARS, INITWATER, SOILTEMP)
    
    CALL READSOILEVAPPARS (UWATPARS, DRYTHICKMIN, TORTPAR)


    ! Some arrays can be given a few values, rest is filled with last entered value:
!    CALL FILLWITHLAST(FRACROOT, MAXSOILLAY, NROOTLAYER, -900.0)
    CALL FILLWITHLAST(POREFRAC, MAXSOILLAY, NLAYER, -900.0)
    CALL FILLWITHLAST(DRAINLIMIT, MAXSOILLAY, NLAYER, -900.0)
    CALL FILLWITHLAST(KSAT, MAXSOILLAY, NLAYER, -900.0)
    CALL FILLWITHLAST(BPAR, MAXSOILLAY, NLAYER, -900.0)
    CALL FILLWITHLAST(PSIE, MAXSOILLAY, NLAYER, -900.0)
    CALL FILLWITHLAST(SOILTEMP, MAXSOILLAY, NLAYER, -900.0)
    CALL FILLWITHLAST(INITWATER, MAXSOILLAY, NLAYER, -900.0)
    CALL FILLWITHLAST(ALPHARET,MAXSOILLAY,NLAYER,-900.0)
    CALL FILLWITHLAST(WS,MAXSOILLAY,NLAYER,-900.0)
    CALL FILLWITHLAST(WR,MAXSOILLAY,NLAYER,-900.0)
    CALL FILLWITHLAST(NRET,MAXSOILLAY,NLAYER,-900.0)
    
    
    ! Set (constant!) water content of layer below those simulated, if provided:
    IF(COREWATER.GT.0.0)THEN
        INITWATER(NLAYER+1) = COREWATER
    ENDIF
    
    ! Soil temperature input is in C, within the model in K.
    SOILTEMP = SOILTEMP + FREEZE
    
    ! Soil temperature of the core:
    SOILTEMP(NLAYER+1) = SOILTEMP(NLAYER)
    
    ! Check if initwater is lower than porefrac, if not fix it;
    
    DO I = 1,NLAYER
        ANYFIX = .FALSE.
        IF(INITWATER(I).GT.POREFRAC(I))THEN
            INITWATER(I) = POREFRAC(I)
            ANYFIX = .TRUE.
        END IF
    END DO
    
    
    IF(ANYFIX)THEN
        CALL SUBERROR('WARNING : ONE OR MORE INITWATER > POREFRAC - SET TO POREFRAC.', IWARN,0)
    ENDIF

    ! If we use measured ET for water balance, set EQUALUPTAKE=1
    ! Otherwise we are in trouble when soil gets very dry.
    IF(USEMEASET.EQ.1)THEN
        EQUALUPTAKE = 1
        SIMTSOIL = 0
    ENDIF

    ! For fracroot, make sure adds up to 1, or otherwise assume that
    ! provided values are weights (i.e. make them add to one).
!    FRACSUM = SUM(FRACROOT(1:NROOTLAYER))
!             ! modification mathias décembre 2012              ! maintenant calculé dans INTERPOLATEDIST
!    FRACROOT = FRACROOT / FRACSUM
    
    ! Root cross-sectional area (m2)
!        ROOTXSECAREA = PI*ROOTRAD**2                                   ! maintenant calculé dans INTERPOLATEW !!!!!!!!!!!!
         ROOTXSECAREA = 0
         
    ! Prepare root mass and length arrays (from SPA, io.f90, RAD).
!    DO I=1,NROOTLAYER
!        ROOTMASS(I) = FRACROOT(I) * ROOTMASSTOT / LAYTHICK(I)
!        ! m m-3 soil
!        ROOTLEN(I) = ROOTMASS(I) / (ROOTDENS*ROOTXSECAREA)
    
!    END DO

    ! Get total root resistance from fraction resistance in roots (ROOTRESFRAC)
    ! and total plant conductance (which includes root conductance).
!    ROOTRESIST = ROOTRESFRAC * (1./PLANTK)
     
    ! Convert hydraulic conductivity to m s-1 (from mol m-1 s-1 MPa-1)
    KSAT = KSAT * H2OVW * GRAV * 1E-03
    
    RETURN
END SUBROUTINE INPUTWATBAL

!**********************************************************************
SUBROUTINE READSOILEVAPPARS(UFILE, DRYTHICKMINI, TORTPARI)
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE,IOERROR
    REAL DRYTHICKMIN,DRYTHICKMINI,TORTPAR,TORTPARI
    NAMELIST /SOILETPARS/ DRYTHICKMIN, TORTPAR

    ! Default values:
    DRYTHICKMIN = 0.001 ! SPA defaults.
    TORTPAR = 0.66

    REWIND(UFILE)
    READ (UFILE, SOILETPARS, IOSTAT = IOERROR)

    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('ERROR READING SOIL EVAP PARAMETERS',IFATAL,IOERROR)
    END IF

    DRYTHICKMINI=DRYTHICKMIN
    TORTPARI=TORTPAR

    RETURN
END SUBROUTINE READSOILEVAPPARS

!**********************************************************************
SUBROUTINE READINITPARS(UFILE, INITWATERI, SOILTEMPI)
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE,IOERROR
    REAL INITWATER(MAXSOILLAY),SOILTEMP(MAXSOILLAY)
    REAL INITWATERI(MAXSOILLAY),SOILTEMPI(MAXSOILLAY)
    NAMELIST /INITPARS/   INITWATER, SOILTEMP
    
    SOILTEMP = -999.9
    INITWATER = -999.9

    REWIND(UFILE)
    READ (UFILE, INITPARS, IOSTAT = IOERROR)

    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('ERROR READING WATBAL INITIAL PARAMETERS',IFATAL,IOERROR)
    END IF

    SOILTEMPI = SOILTEMP
    INITWATERI = INITWATER

    RETURN
END SUBROUTINE READINITPARS

!**********************************************************************
SUBROUTINE READLAYPARS(UFILE, NLAYERI, LAYTHICKI, COREWATERI, &
                       POREFRACI, DRAINLIMITI, FRACORGANICI)
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE, NLAYER, NLAYERI, IOERROR
    
    REAL LAYTHICK(MAXSOILLAY), POREFRAC(MAXSOILLAY)
    REAL DRAINLIMIT(MAXSOILLAY), FRACORGANIC(MAXSOILLAY)
    REAL LAYTHICKI(MAXSOILLAY), POREFRACI(MAXSOILLAY)
    REAL DRAINLIMITI(MAXSOILLAY), FRACORGANICI(MAXSOILLAY)
    REAL COREWATER,COREWATERI
    NAMELIST /LAYPARS/    NLAYER, LAYTHICK, COREWATER, &
                          POREFRAC, DRAINLIMIT, FRACORGANIC
    
    ! Initial.
    LAYTHICK = -999.9
    POREFRAC = -999.9
    DRAINLIMIT = -999.9

    REWIND(UFILE)
    READ (UFILE, LAYPARS, IOSTAT = IOERROR)

    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('ERROR READING SOIL LAYERED PARAMETERS',IFATAL,IOERROR)
    END IF

    NLAYERI=NLAYER
    LAYTHICKI=LAYTHICK
    COREWATERI=COREWATER
    POREFRACI=POREFRAC
    DRAINLIMITI=DRAINLIMIT
    FRACORGANICI=FRACORGANIC

    RETURN
END SUBROUTINE READLAYPARS

!**********************************************************************
SUBROUTINE READSOILRET(UFILE, BPAR, PSIE, KSAT,ALPHA,WS,WR,N)
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    
    INTEGER UFILE,IOERROR,RETFUNCTION
    REAL BPAR(MAXSOILLAY),PSIE(MAXSOILLAY),KSAT(MAXSOILLAY)
    REAL BPARI(MAXSOILLAY),PSIEI(MAXSOILLAY),KSATI(MAXSOILLAY)
    REAL ALPHA(MAXSOILLAY),WS(MAXSOILLAY),WR(MAXSOILLAY),N(MAXSOILLAY)
    REAL ALPHAI(MAXSOILLAY),WSI(MAXSOILLAY),WRI(MAXSOILLAY),NI(MAXSOILLAY)
    NAMELIST /SOILRET/ BPAR, PSIE, KSAT,ALPHA,WS,WR,N

    
    ! Initial.
    KSAT = -999.9
    BPAR = -999.9
    PSIE = -999.9
    ALPHA = -999.9
    WS = -999.9
    WR = -999.9
    N = -999.9

    REWIND(UFILE)
    READ (UFILE, SOILRET, IOSTAT = IOERROR)

    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('ERROR READING WATER RETENTION PARAMETERS',IFATAL,IOERROR)
    END IF

    BPARI = BPAR
    PSIEI = PSIE
    KSATI = KSAT
    ALPHAI = ALPHA
    WSI = WR
    WRI = WS
    NI = N

    
    RETURN
END SUBROUTINE READSOILRET
!**********************************************************************
SUBROUTINE READPLANTPARS(UFILE,MINROOTWPI,MINLEAFWPI,PLANTKTABLEI,KSCALINGI,DATESKPI,NOKPDATES)
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE,IOERROR
    INTEGER IDATE
    INTEGER, EXTERNAL :: IDATE50
    CHARACTER(10) DATESKP(maxdate)
    INTEGER NODATESKP,DATESKPI(maxdate),NOKPDATES
    REAL MINROOTWP,PLANTK(maxdate),MINROOTWPI,PLANTKTABLEI(maxdate)
    REAL MINLEAFWP(MAXSP),MINLEAFWPI(MAXSP)
    REAL KSCALINGI,KSCALING
    NAMELIST /PLANTPARS/  DATESKP,NODATESKP, MINROOTWP,MINLEAFWP,PLANTK,KSCALING

    !default
    NODATESKP = 1
    MINLEAFWP = -999.0
    MINROOTWP = -999.0
    DATESKP(1)= '01/01/99'

    REWIND(UFILE)
    READ (UFILE, PLANTPARS, IOSTAT = IOERROR)

    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('ERROR READING PLANT PARAMETERS IN WATPARS.DAT',IFATAL,IOERROR)
    END IF
    
    
    DO IDATE = 1,NODATESKP
            DATESKPI(IDATE)=IDATE50(DATESKP(IDATE))
    END DO
    
    MINLEAFWPI = MINLEAFWP
    MINROOTWPI = MINROOTWP
    IF(MINROOTWPI.LT.-900.0)MINROOTWPI = MINLEAFWP(1)
    
    CALL FILLWITHLAST(MINLEAFWPI,MAXSP,MAXSP,-999.0)
    
    PLANTKTABLEI = PLANTK
    KSCALINGI = KSCALING
    NOKPDATES = NODATESKP
    

    RETURN
END SUBROUTINE READPLANTPARS


!**********************************************************************
SUBROUTINE READROOTPARS(UFILE, ROOTRESFRACI,ROOTRADTABLEI,ROOTDENSTABLEI, &  
                         ROOTMASSTOTTABLEI, NROOTLAYERI,FRACROOTI, &
                         LAYTHICK,ROOTBETA,DATESROOTI,NOROOTDATES,NOROOTSPEC) 
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE,NROOTLAYER,NROOTLAYERI,IOERROR,NOROOTSPEC
    INTEGER IDATE, ILAY, INDEX, ISPEC
    INTEGER, EXTERNAL :: IDATE50
    CHARACTER(10) DATESROOT(maxdate)
    INTEGER NODATESROOT,DATESROOTI(maxdate),NOROOTDATES

    REAL FRACROOT(MAXSOILLAY*MAXDATE*MAXSP),FRACROOTI(MAXSOILLAY,MAXDATE,MAXSP)
    REAL LAYTHICK(MAXSOILLAY)
    REAL ROOTRESFRAC,ROOTRAD(maxdate),ROOTDENS(maxdate),ROOTMASSTOT(maxdate)
    REAL ROOTRADTABLEI(maxdate),ROOTDENSTABLEI(maxdate),ROOTMASSTOTTABLEI(maxdate)
    REAL ROOTRESFRACI
    REAL ROOTBETA
    NAMELIST /ROOTPARS/   DATESROOT, NODATESROOT,NOROOTSPEC,ROOTRESFRAC,ROOTRAD,ROOTDENS,ROOTMASSTOT, &
                          NROOTLAYER,FRACROOT,ROOTBETA
    
    ! Initial.
    ROOTBETA = 0
    FRACROOT = -999.9
    ROOTRESFRAC = 1.0
    DATESROOT = '01/01/99'
    NODATESROOT = 1
    NOROOTSPEC = 1

    REWIND(UFILE)
    READ (UFILE, ROOTPARS, IOSTAT = IOERROR)
        
    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('ERROR READING ROOT PARAMETERS IN WATPARS.DAT',IFATAL,IOERROR)
    END IF

    ! If ROOTBETA is given, assign FRACROOT using Jackson's root distribution model.
    IF(ROOTBETA.GT.0.0)THEN
      CALL ASSIGNFRACROOT(ROOTBETA,FRACROOT,NROOTLAYER,LAYTHICK)
    ENDIF

    DO IDATE = 1,NODATESROOT
            DATESROOTI(IDATE)=IDATE50(DATESROOT(IDATE))
    END DO

    ROOTRESFRACI = ROOTRESFRAC
    ROOTRADTABLEI=ROOTRAD
    ROOTDENSTABLEI=ROOTDENS
    ROOTMASSTOTTABLEI=ROOTMASSTOT
    NROOTLAYERI=NROOTLAYER
    NOROOTDATES = NODATESROOT    

    INDEX=1
    DO ISPEC = 1,NOROOTSPEC
        DO IDATE = 1,NOROOTDATES
            DO ILAY = 1,NROOTLAYERI
                IF (FRACROOT(INDEX).LT.0) CALL SUBERROR('MISSING DATA, OR ONE OF MAXT, MAXDATE TOO SMALL.',IFATAL,0)
                FRACROOTI(ILAY,IDATE,ISPEC) = FRACROOT(INDEX)
                INDEX = INDEX+1
            END DO
        END DO
    END DO
        
    RETURN
END SUBROUTINE READROOTPARS


!**********************************************************************
SUBROUTINE ASSIGNFRACROOT(ROOTBETA,FRACROOT,NROOTLAYER,LAYTHICK)
!**********************************************************************

    USE maestcom   
    IMPLICIT NONE
    INTEGER NROOTLAYER,I
    REAL ROOTBETA,FRACROOT(MAXSOILLAY),LAYTHICK(MAXSOILLAY)
    REAL CUMLAYTHICK(MAXSOILLAY),TOTFRAC
    
    ! Cumulative depth to bottom of layer i.
    CUMLAYTHICK(1) = LAYTHICK(1)
    DO I=2,NROOTLAYER
        CUMLAYTHICK(I) = LAYTHICK(I) + CUMLAYTHICK(I-1)    
    ENDDO
    ! Convert to cm; to be consistent with Jackson et al. 1996
    CUMLAYTHICK = 100*CUMLAYTHICK
   
    ! Fraction roots.
    FRACROOT(1) = 1 - ROOTBETA ** CUMLAYTHICK(1)
    DO I=2,NROOTLAYER
        FRACROOT(I) = (1 - ROOTBETA ** CUMLAYTHICK(I)) - (1 - ROOTBETA ** CUMLAYTHICK(I-1))
    ENDDO
    
    ! Make sure total FRACROOT adds up to unity.
    TOTFRAC = SUM(FRACROOT(1:NROOTLAYER))
    DO I=1,NROOTLAYER
        FRACROOT(I) = FRACROOT(I) / TOTFRAC
    ENDDO
    
    
    RETURN
END SUBROUTINE ASSIGNFRACROOT





!**********************************************************************
SUBROUTINE READINFILT(UFILE,EXPINFI)
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE,IOERROR
    REAL EXPINF, EXPINFI
    NAMELIST /WATINFILT/  EXPINF
    

    REWIND(UFILE)
    READ (UFILE, WATINFILT, IOSTAT = IOERROR)

    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('ERROR READING INFILTRATION PARAMETERS',IFATAL,IOERROR)
    END IF

    EXPINFI =  EXPINF

    RETURN
END SUBROUTINE READINFILT


!**********************************************************************
SUBROUTINE READWATTFALL(UFILE, RUTTERBI,RUTTERDI,MAXSTORAGEI,THROUGHFALLI)
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE,IOERROR
    REAL MAXSTORAGE,MAXSTORAGEI,RUTTERB,RUTTERBI,RUTTERD,RUTTERDI
    REAL THROUGHFALL,THROUGHFALLI
    
    NAMELIST /WATTFALL/   RUTTERB,RUTTERD,MAXSTORAGE, &
                          THROUGHFALL


    REWIND(UFILE)
    READ (UWATPARS, WATTFALL, IOSTAT = IOERROR)

    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('ERROR READING THROUGHFALL PARAMETERS',IFATAL,IOERROR)
    END IF

    RUTTERBI=RUTTERB
    RUTTERDI=RUTTERD
    MAXSTORAGEI=MAXSTORAGE
    THROUGHFALLI=THROUGHFALL

    RETURN
END SUBROUTINE READWATTFALL


!**********************************************************************
SUBROUTINE READWATCONTROL(UFILE,KEEPWETI,SIMTSOILI,REASSIGNRAINI, &
                          WSOILMETHODI,RETFUNCTIONI,EQUALUPTAKEI, &
                          USEMEASETI,USEMEASSWI,SIMSOILEVAPI,USESTANDI)
!**********************************************************************
    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE,KEEPWET,SIMTSOIL,REASSIGNRAIN,WSOILMETHOD
    INTEGER RETFUNCTION,EQUALUPTAKE,USEMEASET
    INTEGER KEEPWETI,SIMTSOILI,REASSIGNRAINI
    INTEGER WSOILMETHODI,RETFUNCTIONI,EQUALUPTAKEI
    INTEGER USEMEASETI,USEMEASSWI,USEMEASSW,IOERROR
    INTEGER SIMSOILEVAP,SIMSOILEVAPI
    INTEGER USESTAND,USESTANDI
    
    NAMELIST /WATCONTROL/ KEEPWET,SIMTSOIL,REASSIGNRAIN,WSOILMETHOD, &
                          RETFUNCTION,EQUALUPTAKE,USEMEASET,USEMEASSW, &
                          SIMSOILEVAP,USESTAND
    
    WSOILMETHOD = 1 ! Emax approach
    USEMEASET = 0   ! Don't use measured ET for water balance calc.
    USEMEASSW = 0   ! Don't use measured soil water content.
    SIMSOILEVAP = 1 ! Do simulate soil evaporation.
    EQUALUPTAKE = 0 ! Do not set debugging option.
    USESTAND = 1    ! Do use information on non-target trees to scale up to the stand.
 
    REWIND(UFILE)
    READ (UWATPARS, WATCONTROL, IOSTAT = IOERROR)

    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('ERROR READING WATER BALANCE CONTROL PARAMETERS.', IFATAL,IOERROR)
    END IF

    KEEPWETI=KEEPWET
    SIMTSOILI=SIMTSOIL
    REASSIGNRAINI=REASSIGNRAIN
    WSOILMETHODI=WSOILMETHOD
    RETFUNCTIONI=RETFUNCTION
    EQUALUPTAKEI=EQUALUPTAKE
    USEMEASETI=USEMEASET
    USEMEASSWI=USEMEASSW
    SIMSOILEVAPI=SIMSOILEVAP
    USESTANDI=USESTAND
    
    RETURN
END SUBROUTINE READWATCONTROL

!**********************************************************************
SUBROUTINE INPUTSTR(NSPECIES,STRFILES,JLEAF,BPT,RANDOM,NOAGEC,  &
                    JSHAPE,SHAPEC,EXTWIND,                      &
                    NALPHA,ALPHA,FALPHA,                        &
                    COEFFT,EXPONT,WINTERC,                      &
                    BCOEFFT,BEXPONT,BINTERC,                    &
                    RCOEFFT,REXPONT,RINTERC,FRFRAC,in_path,DATESLIA,NOLIADATES,DATESLAD,NOLADDATES)
! This routine reads in input data on canopy structure (from USTR)
! which is required to begin the simulation.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER NSPECIES,I,IOERROR
    INTEGER DATESLIA(maxdate,maxsp), NOLIADATES(maxsp),DATESLAD(maxdate,maxsp),NOLADDATES(maxsp)
    REAL ALPHA(MAXANG,MAXSP),FALPHA(MAXANG,maxdate,MAXSP)
    REAL BPT(8,MAXC,MAXSP,maxdate)
    REAL SHAPEC(MAXSP),EXTWIND(MAXSP)
    REAL RANDOM(MAXSP)
    REAL COEFFT(MAXSP),EXPONT(MAXSP),WINTERC(MAXSP)
    REAL BCOEFFT(MAXSP),BEXPONT(MAXSP),BINTERC(MAXSP)
    REAL RCOEFFT(MAXSP),REXPONT(MAXSP)
    REAL RINTERC(MAXSP),FRFRAC(MAXSP)
    INTEGER NOAGEC(MAXSP),JLEAF(MAXSP),JSHAPE(MAXSP)
    INTEGER NALPHA(MAXSP)
    CHARACTER(30) STRFILES(MAXSP)
    !CHARACTER(*), INTENT(IN) :: in_path
    CHARACTER(*) in_path
    
    DO I=1,NSPECIES
        OPEN(USTR, FILE=trim(in_path)//STRFILES(I), STATUS='OLD', IOSTAT=IOERROR)
        IF(IOERROR.NE.0)THEN
            CALL SUBERROR('ERROR: STR FILE ' // TRIM(STRFILES(I)) // ' DOES NOT EXIST' ,IFATAL,0)
        ENDIF

        CALL READCROWN(USTR, JSHAPE(I), SHAPEC(I))

        CALL READAERO(USTR, EXTWIND(I))
        CALL READLIA(USTR, NALPHA(I), ALPHA(1:MAXANG,I),FALPHA(1:MAXANG,1:maxdate,I),DATESLIA(1:maxdate,I),NOLIADATES(I))

        CALL READBETA(USTR, NOAGEC(I), JLEAF(I), BPT(1:8,1:MAXC,I,1:maxdate), RANDOM(I),DATESLAD(1:maxdate,I),NOLADDATES(I))

        CALL READALLOM(USTR, COEFFT(I), EXPONT(I), WINTERC(I),  &
             BCOEFFT(I), BEXPONT(I), BINTERC(I),                &
             RCOEFFT(I), REXPONT(I), RINTERC(I), FRFRAC(I))

        CLOSE(USTR)

    END DO

    RETURN
END SUBROUTINE INPUTSTR


!**********************************************************************
SUBROUTINE INPUTPHY(NSPECIES,PHYFILES,MODELJM,MODELRD,MODELGS,MODELRW,              &
                    NOLAY,NOAGEC,NOAGEP,PROPC,PROPP,ABSRP,REFLEC,TRANS,RHOSOL,      &
                    JMAXTABLE,DATESJ,NOJDATES,IECO,EAVJ,EDVJ,DELSJ,THETA,           &
                    VCMAXTABLE,DATESV,NOVDATES,EAVC,EDVC,DELSC,TVJUP,TVJDN,         &
                    SLATABLE,DATESSLA,NOSLADATES,NOADATES,DATESA,AJQTABLE,          &
                    RDTABLE,DATESRD,NORDATES,RTEMP,DAYRESP,TBELOW,                  &
                    EFFYRW,RMW,RTEMPW,COLLA,COLLK,STEMSDW,RMWAREA,STEMFORM,         &
                    NOFQDATES,DATESFQ,Q10FTABLE,K10F,NOWQDATES,DATESWQ,Q10WTABLE,   &
                    RMFR,RMCR,Q10R,RTEMPR,EFFYRF, RMB,Q10B,RTEMPB,                  &
                    GSREF,GSMIN,PAR0,D0,VK1,VK2,VPD1,VPD2,VMFD0,                    &
                    GSJA,GSJB,T0,TREF,TMAX,SMD1,SMD2,WC1, WC2, SWPEXP,              &
                    G0TABLE,G1TABLE,GK,NOGSDATES,DATESGS,D0L,GAMMA,VPDMIN,WLEAFTABLE,DATESWLEAF,NOWLEAFDATES,NSIDES,       &
                    SF,PSIV,VPARA,VPARB,VPARC,VFUN,in_path)
! This routine reads in input data on physiology (from UPHY).
! Some parameters can change with time: JMAX25, VCMAX25, RD0
! - for these, arrays of dates and values are read in, and must
! be interpolated.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER NSPECIES,IOERROR,I,NOLAY,MODELGS,MODELJM,MODELRD,MODELRW
    INTEGER DATESN(maxdate,MAXSP),DATESJ(maxdate,MAXSP)
    INTEGER DATESV(maxdate,MAXSP),DATESRD(maxdate,MAXSP)
    INTEGER DATESSLA(maxdate,MAXSP),DATESA(maxdate,MAXSP)
    INTEGER DATESFQ(maxdate,MAXSP),DATESWQ(maxdate,MAXSP)
    INTEGER DATESGS(maxdate,MAXSP),DATESWLEAF(maxdate,MAXSP)
    INTEGER NOAGEC(MAXSP),NOAGEP(MAXSP),NSIDES(MAXSP)
    INTEGER NOJDATES(MAXSP),NOVDATES(MAXSP)
    INTEGER NOADATES(MAXSP),NOSLADATES(MAXSP)
    INTEGER NONDATES(MAXSP),NORDATES(MAXSP)
    INTEGER NOWQDATES(MAXSP),NOFQDATES(MAXSP)
    INTEGER NOGSDATES(MAXSP),NOWLEAFDATES(MAXSP)
    INTEGER IECO(MAXSP),VFUN(MAXSP)
    REAL ABSRP(MAXLAY,3,MAXSP),REFLEC(MAXLAY,3,MAXSP)
    REAL TRANS(MAXLAY,3,MAXSP),RHOSOL(3,MAXSP)
    REAL PROPP(MAXC,MAXSP),PROPC(MAXC,MAXSP)
    REAL LEAFN(maxdate,MAXLAY,MAXC,MAXSP)
    REAL JMAXTABLE(maxdate,MAXLAY,MAXC,MAXSP)
    REAL VCMAXTABLE(maxdate,MAXLAY,MAXC,MAXSP)
    REAL RDTABLE(maxdate,MAXLAY,MAXC,MAXSP)
    REAL SLATABLE(maxdate,MAXLAY,MAXC,MAXSP)
    REAL AJQTABLE(maxdate,MAXLAY,MAXC,MAXSP)
    REAL Q10FTABLE(maxdate,MAXSP),Q10WTABLE(maxdate,MAXSP)
    REAL G0TABLE(maxdate,MAXSP),G1TABLE(maxdate,MAXSP)
    REAL WLEAFTABLE(maxdate,MAXSP)
    REAL GSREF(MAXSP), GSMIN(MAXSP), PAR0(MAXSP)
    REAL D0(MAXSP), VK1(MAXSP), VK2(MAXSP)
    REAL VPD1(MAXSP), VPD2(MAXSP), VMFD0(MAXSP)
    REAL GSJA(MAXSP), GSJB(MAXSP), T0(MAXSP)
    REAL TREF(MAXSP), TMAX(MAXSP), SMD1(MAXSP)
    REAL SMD2(MAXSP), WC1(MAXSP), WC2(MAXSP)
    REAL SWPEXP(MAXSP), G0(MAXSP), D0L(MAXSP)
    REAL GAMMA(MAXSP), G1(MAXSP), WLEAF(MAXSP)
    REAL EAVJ(MAXSP), EDVJ(MAXSP)
    REAL DELSJ(MAXSP), EAVC(MAXSP)
    REAL EDVC(MAXSP), DELSC(MAXSP), TVJUP(MAXSP)
    REAL TVJDN(MAXSP), THETA(MAXSP)
    REAL RTEMP(MAXSP), DAYRESP(MAXSP), EFFYRF(MAXSP)
    REAL TBELOW(MAXSP),EFFYRW(MAXSP),RMW(MAXSP)
    REAL RTEMPW(MAXSP),COLLA(MAXSP),COLLK(MAXSP)
    REAL STEMSDW(MAXSP),RMWAREA(MAXSP),STEMFORM(MAXSP)
    REAL Q10R(MAXSP),RTEMPR(MAXSP),Q10B(MAXSP),RTEMPB(MAXSP)
    REAL RMCR(MAXSP),RMFR(MAXSP),RMB(MAXSP)
    REAL K10F(MAXSP),SF(MAXSP),PSIV(MAXSP)
    REAL VPARA(MAXSP),VPARB(MAXSP),VPARC(MAXSP)
    REAL VPDMIN(MAXSP),GK(MAXSP)
    
    !CHARACTER(*), INTENT(IN) :: in_path
    CHARACTER(*) in_path
    CHARACTER(30) PHYFILES(MAXSP)

    DO I = 1,NSPECIES
        OPEN(UPHY, FILE=trim(in_path)//PHYFILES(I), STATUS='OLD',IOSTAT=IOERROR)
        IF(IOERROR.NE.0)THEN
            CALL SUBERROR('ERROR: PHY FILE ' // TRIM(PHYFILES(I)) // ' DOES NOT EXIST' ,IFATAL,0)
        ENDIF
        CALL READRR(UPHY,RMCR(I),RMFR(I),Q10R(I),RTEMPR(I))
        
        CALL READRB(UPHY,RMB(I),Q10B(I),RTEMPB(I))
        
        ! Note that NOAGEC is passed to, not read from.
        CALL READAGEP(UPHY, NOAGEC(I), NOAGEP(I))
        
        CALL READPROP(UPHY, NOAGEC(I), NOAGEP(I), PROPC(1,I),PROPP(1,I))
        
        CALL READABSRP(UPHY,NOLAY,ABSRP(1,1,I), &
             REFLEC(1,1,I),TRANS(1,1,I),RHOSOL(1,I))
        
        CALL READGS(UPHY,I,MODELGS,                                 &
                    GSREF(I),GSMIN(I),PAR0(I),D0(I),VK1(I),VK2(I),VPD1(I),  &
                    VPD2(I), VMFD0(I), GSJA(I), GSJB(I), T0(I), TREF(I),    &
                    TMAX(I), SMD1(I), SMD2(I), WC1(I), WC2(I), SWPEXP(I),   &
                    G0TABLE(1,I), G1TABLE(1,I),  GK(I),         &
                    NOGSDATES(I), DATESGS(1,I), D0L(I),GAMMA(I),VPDMIN(I),   &
                    WLEAFTABLE(1,I), NSIDES(I), SF(I), PSIV(I),DATESWLEAF(1,I),NOWLEAFDATES(I))
        
          CALL READLEAFN(UPHY, MODELJM, MODELRD, NOLAY, NOAGEP(I),  &
                            NONDATES(I), DATESN(1,I),      &
                            LEAFN(1,1,1,I))

          CALL READJMAX(UPHY, MODELJM, NOLAY, NOAGEP(I), NONDATES(I),               &
                        DATESN(1,I), LEAFN(1,1,1,I),  &
                        NOJDATES(I), DATESJ(1,I),                          &
                        JMAXTABLE(1,1,1,I),                    &
                        NOVDATES(I), DATESV(1,I),                          &
                        VCMAXTABLE(1,1,1,I),                   &
                        NOADATES(I), DATESA(1,I),                          &
                        AJQTABLE(1,1,1,I),                     &
                        IECO(I), EAVJ(I), EDVJ(I), DELSJ(I), EAVC(I), EDVC(I),      &
                        DELSC(I), TVJUP(I), TVJDN(I), THETA(I))

          CALL READVSTRESS(UPHY, VPARA(I), VPARB(I), VPARC(I), VFUN(I))

          CALL READPHYARRAY(UPHY,5,NOLAY,NOAGEP(I),                 &
                              NOSLADATES(I),DATESSLA(1,I), &
                              SLATABLE(1,1,1,I))

          CALL READRD(UPHY, MODELRD, NOLAY, NOAGEP(I), NONDATES(I), &
                        DATESN(1,I), LEAFN(1,1,1,I), &
                        NORDATES(I), DATESRD(1,I),  &
                        RDTABLE(1,1,1,I), &
                        NOFQDATES(I), DATESFQ(1,I), &
                        Q10FTABLE(1,I), K10F(I), &
                        RTEMP(I), DAYRESP(I), EFFYRF(I), TBELOW(I))

          CALL READRW(UPHY,MODELRW,EFFYRW(I),RMW(I),RTEMPW(I),  &
                        NOWQDATES(I), DATESWQ(1,I),    &
                        Q10WTABLE(1,I),                &
                        COLLA(I),COLLK(I),STEMSDW(I),RMWAREA(I),STEMFORM(I))

          CALL READRR(UPHY,RMCR(I),RMFR(I),Q10R(I),RTEMPR(I))
          CALL READRB(UPHY,RMB(I),Q10B(I),RTEMPB(I))

          CLOSE(UPHY)
    END DO

    RETURN
END SUBROUTINE INPUTPHY


!**********************************************************************
SUBROUTINE INPUTTREE(XSLOPE,YSLOPE,BEAR,X0,Y0,XMAX,YMAX,PLOTAREA,STOCKING,      &
                        ZHT,Z0HT,ZPD,                                           &
                        NOALLTREES,NOTREES,NOTARGETS,ITARGETS,SHADEHT,          &
                        NOXDATES,NOYDATES,NOZDATES,NOTDATES,NOLADATES,NODDATES, &
                        DATESX,DATESY,DATESZ,DATEST,DATESLA,DATESD,             &
                        DX,DY,DZ,R1,R2,R3,TRUNK,FLT,TOTLAITABLE,DIAMA,          &
                        IFLUSH,DT1,DT2,DT3,DT4,EXPTIME,APP,EXPAN,               &
                        WEIGHTS,NSPECIES,ISPECIES)
! This subroutine should read in the data from the trees.dat file on
! crown positions and dimensions. Some variables can change with time:
! radii, height, diameter & leaf area - for these, arrays of dates & values at
! those dates may be read in, & interpolated during the program.
! x- and y- co-ordinates of the crowns may be read in or calculated from
! plot size and stocking density.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER IOERROR
    INTEGER DATESX(maxdate),DATESY(maxdate),DATESZ(maxdate)
    INTEGER DATEST(maxdate),DATESLA(maxdate),DATESD(maxdate)
    INTEGER ITARGETS(MAXT),ISPECIES(MAXT),IPLOTSHAPE,NOALLTREES
    INTEGER NOXDATES,NOYDATES,NOZDATES,NOTDATES,IFLUSH,NOLADATES
    INTEGER NODDATES,NOTREES,NOTARGETS,NSPECIES
    REAL DX(MAXT),DY(MAXT),DZ(MAXT),WEIGHTS(MAXT), EXPFACTORS(MAXT)
    REAL R1(maxdate,MAXT),R2(maxdate,MAXT),R3(maxdate,MAXT)
    REAL TRUNK(maxdate,MAXT),FLT(maxdate,MAXT),TOTLAITABLE(maxdate)
    REAL DIAMA(maxdate,MAXT),PLOTAREA
    REAL X0,Y0,XMAX,YMAX,XSLOPE,YSLOPE,BEAR,SHADEHT,STOCKING
    REAL ZHT,Z0HT,ZPD,DT1,DT2,DT3,DT4,EXPTIME,APP,EXPAN
    
    ! Read in number of trees & number of target tree
    CALL READPLOT(UTREES, X0, Y0, XMAX, YMAX, NOALLTREES,XSLOPE, YSLOPE, BEAR, SHADEHT, STOCKING, IPLOTSHAPE)
    PLOTAREA = (XMAX - X0) * (YMAX - Y0)

    ! Read in aerodynamic properties of canopy
    CALL READZPD(UTREES,ZHT,Z0HT,ZPD)

    ! Get x, y, z co-ords of each tree
    CALL READXYZ(UTREES,NOALLTREES,X0,Y0,XMAX,YMAX,XSLOPE,YSLOPE,DX,DY,DZ)

    ! Get radii in x & y directions of each tree
    CALL READTREEARRAY(UTREES,1,NOALLTREES,NOXDATES,DATESX,R1)
    CALL READTREEARRAY(UTREES,2,NOALLTREES,NOYDATES,DATESY,R2)
    ! Get green crown height of each tree
    CALL READTREEARRAY(UTREES,3,NOALLTREES,NOZDATES,DATESZ,R3)
    ! Get trunk length of each tree
    CALL READTREEARRAY(UTREES,4,NOALLTREES,NOTDATES,DATEST,TRUNK)

    ! Get leaf area parameters
    CALL GETLEAFAREA(UTREES,IFLUSH,DT1,DT2,DT3,DT4,EXPTIME,APP,EXPAN,NOALLTREES,NOLADATES,DATESLA,FLT)

    ! Get diameter of each tree
    CALL READTREEARRAY(UTREES,6,NOALLTREES,NODDATES,DATESD,DIAMA)

    ! Calculate total LAI
    CALL CALCLAI(NOLADATES,FLT,NOALLTREES,XMAX,YMAX,XSLOPE,YSLOPE,TOTLAITABLE)

    ! Read in how many of the trees form the subplot (from confile)
    CALL READCONTREES(UCONTROL,NOALLTREES,DX,DY,XMAX,YMAX,NOTREES,NOTARGETS,ITARGETS,IPLOTSHAPE,WEIGHTS)
       
    ! Read species array, if provided.
    CALL READSPECLIST(UTREES, NSPECIES, ISPECIES)

    RETURN
END SUBROUTINE INPUTTREE

!**********************************************************************
SUBROUTINE SORTTREES(NOALLTREES,NOTREES,NOTARGET,DX,DY,DZ,R1,R2,R3,TRUNK,FLT,DIAMA, &
                        DXT,DYT,DZT,RX,RY,RZ,FOLT,ZBC,DIAM,ISPECIES,ISPECIEST,IT)
! This routine selects the 'NOTREES' trees closest to the target tree.
! It sorts the information about each tree into order of distance from
! the target tree. Does this simply by calling SORTTREESP.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER IT(MAXT),ISPECIES(MAXT),ISPECIEST(MAXT)
    INTEGER NOALLTREES,NOTREES,NOTARGET
    REAL DX(MAXT),DY(MAXT),DZ(MAXT)
    REAL R1(maxdate,MAXT),R2(maxdate,MAXT),R3(maxdate,MAXT)
    REAL TRUNK(maxdate,MAXT),FLT(maxdate,MAXT)
    REAL DXT(MAXT),DYT(MAXT),DZT(MAXT)
    REAL RX(maxdate,MAXT),RY(maxdate,MAXT),RZ(maxdate,MAXT)
    REAL ZBC(maxdate,MAXT),FOLT(maxdate,MAXT)
    REAL DIAMA(maxdate,MAXT),DIAM(maxdate,MAXT)
    REAL X,Y

    X = DX(NOTARGET)
    Y = DY(NOTARGET)
    CALL SORTTREESP(X,Y,NOALLTREES,NOTREES,DX,DY,DZ,R1,R2,R3,TRUNK,FLT,DIAMA, &
                    DXT,DYT,DZT,RX,RY,RZ,FOLT,ZBC,DIAM,ISPECIES,ISPECIEST,IT)

    RETURN
END SUBROUTINE SORTTREES


!**********************************************************************
SUBROUTINE SORTTREESP(X,Y,NOALLTREES,NOTREES,DX,DY,DZ,R1,R2,R3,TRUNK,FLT,DIAMA, &
                        DXT,DYT,DZT,RX,RY,RZ,FOLT,ZBC,DIAM,ISPECIES,ISPECIEST,IT)
! This routine selects the 'NOTREES' trees closest to the point (x,y,z).
! It sorts the information about each tree into order of distance from
! this point.
! Now outputs IT (index of sorted tree numbers).
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER N,NOALLTREES,I,J,ITEMP,NOTREES,MM,IDATE
    INTEGER IT(MAXT),ISPECIES(MAXT),ISPECIEST(MAXT),MT1,MT2
    REAL DX(MAXT),DY(MAXT),DZ(MAXT)
    REAL R1(maxdate,MAXT),R2(maxdate,MAXT),R3(maxdate,MAXT)
    REAL TRUNK(maxdate,MAXT),FLT(maxdate,MAXT)
    REAL DXT(MAXT),DYT(MAXT),DZT(MAXT)
    REAL RX(maxdate,MAXT),RY(maxdate,MAXT),RZ(maxdate,MAXT)
    REAL ZBC(maxdate,MAXT),FOLT(maxdate,MAXT)
    REAL DIAMA(maxdate,MAXT),DIAM(maxdate,MAXT)
    REAL DNT(MAXT),X,Y,TEMP

    ! We select NT trees which are closest to the tree (NOTARGET) we
    ! are concerned with.
    DO N = 1,NOALLTREES
        DNT(N) = SQRT((X-DX(N))**2 + (Y-DY(N))**2)
        IT(N) = N
    END DO

    MT1 = NOALLTREES - 1
    DO 20 I = 1,MT1
         MT2 = I + 1
         DO 20 J = MT2,NOALLTREES
            IF (DNT(I).LE.DNT(J)) GO TO 20
            TEMP = DNT(I)
            DNT(I) = DNT(J)
            DNT(J) = TEMP
            ITEMP = IT(I)
            IT(I) = IT(J)
            IT(J) = ITEMP
20  CONTINUE

    ! Produce new arrays containing the information about the closest trees.
    DO I = 1,NOTREES
        MM = IT(I)
        DXT(I) = DX(MM)
        DYT(I) = DY(MM)
        DZT(I) = DZ(MM)
        ISPECIEST(I) = ISPECIES(MM)
        DO IDATE = 1,maxdate
            RX(IDATE,I) =  R1(IDATE,MM)
            RY(IDATE,I) = R2(IDATE,MM)
            RZ(IDATE,I) = R3(IDATE,MM)
            ZBC(IDATE,I) = TRUNK(IDATE,MM)
            FOLT(IDATE,I) = FLT(IDATE,MM)
            DIAM(IDATE,I) = DIAMA(IDATE,MM)
        END DO
    END DO
    RETURN
    
                        
END SUBROUTINE SORTTREESP


!**********************************************************************
SUBROUTINE ANGLE(ELP,NALPHA,FALPHA)
! This routine calculates the fraction of leaf area in each leaf
! angle class.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER NALPHA,N
    REAL FALPHA(MAXANG)
    REAL ELP,ECTR,TEMP,TOTAL,DALP,ALP1,ALP2,RSUM

    DALP = PID2/REAL(NALPHA)
    IF (ELP.NE.1) THEN
        IF (ELP.LT.1) THEN
            ECTR = SQRT(1.0-ELP*ELP)
            TEMP = 1.0 + ASIN(ECTR)/ (ECTR*ELP)
        END IF

        IF (ELP.GT.1) THEN
            ECTR = SQRT(1.0- (1.0/ELP)**2)
            TEMP = 1.0 + LOG((1.0+ECTR)/ (1.0-ECTR))/ (2.0*ELP*ELP*ECTR)
        END IF

        TOTAL = 0.000
        DO N = 1,NALPHA
            ALP1 = (N-1)*DALP                           
            ALP2 = N*DALP                               
            RSUM = 0.000                                 
            CALL INTEG(ALP1,ALP2,ELP,TEMP,RSUM)          
            FALPHA(N) = RSUM                             
            TOTAL = TOTAL + RSUM                         
        END DO
        DO N = 1,NALPHA
            FALPHA(N) = FALPHA(N)/TOTAL
        END DO
    ELSE
        DO N = 1,NALPHA
            FALPHA(N) = COS((N-1)*DALP) - COS(N*DALP)
        END DO
    END IF

    RETURN
END SUBROUTINE ANGLE

!**********************************************************************
SUBROUTINE INTEG(ALP1,ALP2,ELP,TEMP,RSUM)
!     this is subroutine to integrate the leaf area density
!     function over a specified angle range
!**********************************************************************
    IMPLICIT NONE
    INTEGER I
    REAL H,UP,ALP1,ALP2,DOWN,RSUM,ELP,TEMP,FANG
    
    H = (ALP2-ALP1)/50.0
    UP = ALP1 + H*0.42265
    DOWN = ALP1 + H*1.57735
    RSUM = FANG(ELP,TEMP,UP) + FANG(ELP,TEMP,DOWN)
    DO 100 I = 1,24
        UP = UP + 2.0*H
        DOWN = DOWN + 2.0*H
    100 RSUM = RSUM + FANG(ELP,TEMP,UP) + FANG(ELP,TEMP,DOWN)
    RSUM = H*RSUM

    RETURN
END SUBROUTINE INTEG


!**********************************************************************
REAL FUNCTION AVGLIA(ELP)
!     this function is the relation between the parameter ELP and the
!     average leaf angle. (see notes for details)
!**********************************************************************
    IMPLICIT NONE
    REAL ELP
    
    IF (ELP.GT.1.0) THEN
        AVGLIA = 1.0/ (0.5901*ELP+0.3037)
    ELSE
        AVGLIA = 1.0/ (0.3782*ELP+0.6131)
    END IF

    RETURN
END FUNCTION AVGLIA


!**********************************************************************
REAL FUNCTION CALCELP(AVGANG)
!     this function is the relation between the average leaf angle and
!     the parameter ELP. This is the inverse of function AVGLIA.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    REAL AVGANG

    AVGANG = AVGANG*PID180    !Convert to radians
    
    IF (AVGANG.LT.1.0) THEN
        CALCELP = (1./AVGANG - 0.3037)/0.5901
    ELSE
        CALCELP = (1./AVGANG - 0.6131)/0.3782
    END IF

    RETURN
END FUNCTION CALCELP


!**********************************************************************
REAL FUNCTION FANG(ELP,TEMP,ALP)
!     this is the ellipsoidal leaf angular density function
!     reference: Campbell,G.S. (personal comm.)
!**********************************************************************
    IMPLICIT NONE
    REAL ELP,TEMP,ALP
    
    FANG = 2.0*ELP*ELP*SIN(ALP)/ (TEMP*(((COS(ALP))**2.0+ (ELP*SIN(ALP))**2)**2))

    RETURN
END FUNCTION FANG


!**********************************************************************
SUBROUTINE OUTPUTHR(IDAY,IHOUR,NOTARGETS,ITARGETS,ISPECIES,         &
                    TCAN,NOLAY,PPAR,PPS,PTRANSP,FOLLAY,             &
                    THRAB,FCO2,FRESPF,FRESPW,FRESPB,                &
                    FH2OT,GSCAN,GBHCAN,FH2OCAN,FHEAT,VPD,TAIR,PAR,  &
                    PSILCAN,PSILCANMIN,CICAN,ECANMAX,ACANMAX,ZEN,AZ)
! Output the hourly totals
!**********************************************************************
    USE switches
    USE maestcom
   
    IMPLICIT NONE
    INTEGER NOTARGETS,IDAY,IHOUR,ITAR,ITREE,ISPEC,I,NOLAY
    INTEGER ITARGETS(MAXT),ISPECIES(MAXT)
    
    REAL THRAB(MAXT,MAXHRS,3),FCO2(MAXT,MAXHRS),FRESPF(MAXT,MAXHRS)
    REAL FRESPW(MAXT,MAXHRS),FRESPB(MAXT,MAXHRS)
    REAL GSCAN(MAXT,MAXHRS),FH2OT(MAXT,MAXHRS),FH2OCAN(MAXT,MAXHRS)
    REAL FHEAT(MAXT,MAXHRS)
    REAL GBHCAN(MAXT,MAXHRS)
    REAL PPAR(MAXT,MAXLAY,MAXHRS),PPS(MAXT,MAXLAY,MAXHRS)
    REAL PTRANSP(MAXT,MAXLAY,MAXHRS)
    REAL FOLLAY(MAXLAY),TCAN(MAXT,MAXHRS),VPD(MAXHRS)
    REAL TAIR(MAXHRS),PAR(MAXHRS)
    REAL ECANMAX(MAXT,MAXHRS),ACANMAX(MAXT,MAXHRS)
    REAL PSILCAN(MAXT,MAXHRS),PSILCANMIN(MAXT,MAXHRS),CICAN(MAXT,MAXHRS)
    REAL ZEN(MAXHRS), AZ(MAXHRS)

    IF (IOHRLY.GE.1) THEN
        DO ITAR=1,NOTARGETS
            ITREE = ITARGETS(ITAR)
            ISPEC = ISPECIES(ITREE)
            IF (IOFORMAT .EQ. 0) THEN
                WRITE (UHRLY,500) IDAY,ITREE,ISPEC,IHOUR,                                       &
                                    THRAB(ITAR,IHOUR,1)*UMOLPERJ,THRAB(ITAR,IHOUR,2),           &
                                    THRAB(ITAR,IHOUR,3),FCO2(ITAR,IHOUR),FRESPF(ITAR,IHOUR),    &
                                    FRESPW(ITAR,IHOUR)+FRESPB(ITAR,IHOUR),                      &
                                    FH2OT(ITAR,IHOUR)*1E-3,                                     &
                                    FH2OCAN(ITAR,IHOUR)*1E-3,GSCAN(ITAR,IHOUR),GBHCAN(ITAR,IHOUR),  &
                                    FHEAT(ITAR,IHOUR)*1E-3,TCAN(ITAR,IHOUR),                    &
                                    ACANMAX(ITAR,IHOUR),                    &   
                                    PSILCAN(ITAR,IHOUR),PSILCANMIN(ITAR,IHOUR),CICAN(ITAR,IHOUR),  &
                                    TAIR(IHOUR),VPD(IHOUR)/1000,PAR(IHOUR), &
                                    ZEN(IHOUR),AZ(IHOUR)                    !rjout mathias mars 2013
                500 FORMAT (I7,1X,3(I4,1X),3(F12.5,1X),16(F12.5,1X),2(F12.5,1X))    ! rajout de 2, mathias mars 2013
            ELSE IF (IOFORMAT .EQ. 1) THEN
                WRITE (UHRLY) REAL(IDAY),REAL(ITREE),REAL(ISPEC),REAL(IHOUR),               &
                                THRAB(ITAR,IHOUR,1)*UMOLPERJ,THRAB(ITAR,IHOUR,2),           &
                                THRAB(ITAR,IHOUR,3),FCO2(ITAR,IHOUR),FRESPF(ITAR,IHOUR),    &
                                FRESPW(ITAR,IHOUR)+FRESPB(ITAR,IHOUR),                      &
                                FH2OT(ITAR,IHOUR)*1e-3,                                     &
                                FH2OCAN(ITAR,IHOUR)*1E-3,GSCAN(ITAR,IHOUR),GBHCAN(ITAR,IHOUR),        &
                                FHEAT(ITAR,IHOUR)*1E-3,TCAN(ITAR,IHOUR),                    &
                                ACANMAX(ITAR,IHOUR),                    &   
                                PSILCAN(ITAR,IHOUR),PSILCANMIN(ITAR,IHOUR),CICAN(ITAR,IHOUR),  &
                                TAIR(IHOUR),VPD(IHOUR)/1000,PAR(IHOUR)
            END IF                        
        END DO
    END IF
    IF (IOFORMAT .EQ. 0) THEN
        IF (IOHRLY.GE.2) THEN

            WRITE (ULAY,610) 'DAY',IDAY,'HOUR',IHOUR
            610   FORMAT (A5,I5,A5,I5)
            IF (FOLLAY(1).GT.0.0) THEN
                WRITE (ULAY,600) (PPAR(1,I,IHOUR)/FOLLAY(I),I=1,NOLAY)
                WRITE (ULAY,600) (PPS(1,I,IHOUR)/FOLLAY(I),I=1,NOLAY)
                WRITE (ULAY,600) (PTRANSP(1,I,IHOUR)/FOLLAY(I),I=1,NOLAY)
                600   FORMAT (10(F10.2,1X))
            ELSE
                WRITE (ULAY,*) 'NO FOLIAGE AT THIS TIME'
            END IF
        END IF
    ELSE IF (IOFORMAT .EQ. 1) THEN
        IF (IOHRLY.GE.2) THEN
            WRITE (ULAY) REAL(IDAY),REAL(IHOUR)
        
            IF (FOLLAY(1).GT.0.0) THEN
                WRITE (ULAY) (PPAR(1,I,IHOUR)/FOLLAY(I),I=1,NOLAY)
                WRITE (ULAY) (PPS(1,I,IHOUR)/FOLLAY(I),I=1,NOLAY)
                WRITE (ULAY) (PTRANSP(1,I,IHOUR)/FOLLAY(I),I=1,NOLAY)
            ELSE
                ! No foliage at this time
                WRITE (ULAY) -999.9
            END IF
        END IF
    END IF    
    RETURN
END SUBROUTINE OUTPUTHR

!**********************************************************************
SUBROUTINE OUTPUTDY(IDAY,NOTARGETS,ITARGETS,ISPECIES,TDYAB,             &
                    TOTCO2,TOTRESPF,TOTRESPW,TOTRESPWG,TOTH2O,TOTH2OCAN,TOTHFX, &
                    TOTRESPCR,TOTRESPFR,TOTRESPFRG,TOTRESPCRG,TOTRESPFG, &
                    TOTRESPB,TOTRESPBG)

! Output the daily totals
!**********************************************************************

    USE maestcom
    USE switches
    IMPLICIT NONE
    INTEGER NOTARGETS,IDAY,ITAR,ITREE,ISPEC
    INTEGER ITARGETS(MAXT),ISPECIES(MAXT)
    
    
    REAL TDYAB(MAXT,3)
    REAL TOTCO2(MAXT),TOTRESPF(MAXT),TOTRESPWM(MAXT)
    REAL TOTRESPB(MAXT),TOTRESPFR(MAXT),TOTRESPCR(MAXT)
    REAL TOTH2O(MAXT),TOTH2OCAN(MAXT),TOTHFX(MAXT)
    REAL TOTRESPW(MAXT)
    
    REAL TOTRESPFG,TOTRESPWG,TOTRESPBG
    REAL TOTRESPCRG,TOTRESPFRG
    
    IF (IODAILY.EQ.1 .AND. IOFORMAT .EQ. 0) THEN
        DO ITAR=1,NOTARGETS
            ITREE = ITARGETS(ITAR)
            ISPEC = ISPECIES(ITREE)
            WRITE (UDAILY,500) IDAY,ITREE,ISPEC,                    &
                        TDYAB(ITAR,1),TDYAB(ITAR,2),TDYAB(ITAR,3),  &
                        TOTCO2(ITAR)+TOTRESPF(ITAR),TOTRESPF(ITAR), &
                        TOTCO2(ITAR),TOTH2O(ITAR),TOTH2OCAN(ITAR),  &
                        TOTHFX(ITAR)
            500 FORMAT (I7,1X,I4,1X,I4,1X,9(F12.4,1X))        
        END DO
    ELSE IF (IODAILY.EQ.1 .AND. IOFORMAT .EQ. 1) THEN
        DO ITAR=1,NOTARGETS
            ITREE = ITARGETS(ITAR)
            ISPEC = ISPECIES(ITREE)
            WRITE(UDAILY) REAL(IDAY), REAL(ITREE), REAL(ISPEC),     &
                        TDYAB(ITAR,1),TDYAB(ITAR,2),TDYAB(ITAR,3),  &
                        TOTCO2(ITAR)+TOTRESPF(ITAR),TOTRESPF(ITAR), &
                        TOTCO2(ITAR),TOTH2O(ITAR),TOTH2OCAN(ITAR),  &
                        TOTHFX(ITAR)    
        END DO
    END IF
    
    IF (IORESP.EQ.1 .AND. IOFORMAT .EQ. 0) THEN
        DO ITAR=1,NOTARGETS
            ITREE = ITARGETS(ITAR)
            ISPEC = ISPECIES(ITREE)
            WRITE (URESP,510) IDAY,ITREE,ISPEC,                                 &
                                TOTRESPF(ITAR),TOTRESPW(ITAR),TOTRESPB(ITAR),   &
                                TOTRESPCR(ITAR),TOTRESPFR(ITAR),TOTRESPFG,      &
                                TOTRESPWG,TOTRESPBG,                            &
                                TOTRESPCRG,TOTRESPFRG
            510   FORMAT (I7,1X,I4,1X,I4,1X,10(F10.5,1X))
        END DO
    ELSE IF (IORESP.EQ.1 .AND. IOFORMAT .EQ. 1) THEN
        DO ITAR=1,NOTARGETS
            ITREE = ITARGETS(ITAR)
            ISPEC = ISPECIES(ITREE)
            WRITE (URESP) REAL(IDAY),REAL(ITREE),REAL(ISPEC),           &
                        TOTRESPF(ITAR),TOTRESPW(ITAR),TOTRESPB(ITAR),   &
                        TOTRESPCR(ITAR),TOTRESPFR(ITAR),TOTRESPFG,      &
                        TOTRESPWG,TOTRESPBG,                            &
                        TOTRESPCRG,TOTRESPFRG
        END DO
    END IF
    
    RETURN
END SUBROUTINE OUTPUTDY


!**********************************************************************
SUBROUTINE OUTPUTDYWAT(IDAY,WSOILMEAN,WSOILROOTMEAN,SWPMEAN,                 &
                       PPTTOT,ETMMTOT,ETMEASTOT,DISCHARGETOT,               &
                       SOILEVAPTOT,FSOILMEAN,TFALLTOT,QHTOT,QETOT,QNTOT,    &
                       QCTOT,RADINTERCTOT)

! Write the daily water balance output file.
!**********************************************************************

    USE maestcom
    USE switches
    IMPLICIT NONE
    INTEGER IDAY
    REAL WSOILMEAN,WSOILROOTMEAN,PPTTOT,ETMMTOT,ETMEASTOT,DISCHARGETOT
    REAL SOILEVAPTOT,FSOILMEAN,TFALLTOT,QHTOT,QETOT,QNTOT,QCTOT,RADINTERCTOT
    REAL SWPMEAN
    
    IF (IOFORMAT .EQ. 0) THEN
        WRITE(UWATDAY, 523)IDAY,WSOILMEAN,WSOILROOTMEAN,SWPMEAN,             &
                           PPTTOT,TFALLTOT,ETMMTOT,ETMEASTOT,DISCHARGETOT,  &
                           SOILEVAPTOT,FSOILMEAN,QHTOT,                     &
                           QETOT,QNTOT,QCTOT,RADINTERCTOT
        523 FORMAT(I7,15(F12.4))
    ELSE IF (IOFORMAT .EQ. 1) THEN
        WRITE(UWATDAY) REAL(IDAY),WSOILMEAN,WSOILROOTMEAN,SWPMEAN,        &
                       PPTTOT,TFALLTOT,ETMMTOT,ETMEASTOT,DISCHARGETOT,  &
                       SOILEVAPTOT,FSOILMEAN,QHTOT,                     &
                       QETOT,QNTOT,QCTOT,RADINTERCTOT    
    END IF
    RETURN
END SUBROUTINE OUTPUTDYWAT


!**********************************************************************
SUBROUTINE OUTPUTWATBAL(IDAY,IHOUR,NROOTLAYER,NLAYER,          &
                              WSOIL,WSOILROOT,PPT,CANOPY_STORE,         &
                              EVAPSTORE,DRAINSTORE,                     &
                              SURFACE_WATERMM,ETMM,ETMM2,               &
                              USEMEASET,ETMEAS,DISCHARGE,               &
                              FRACWATER,WEIGHTEDSWP,KTOT,       &
                              DRYTHICK,SOILEVAP,OVERFLOW,THERMCOND,     &
                              FRACUPTAKE,SOILMOISTURE,FSOIL1,NSUMMED,   &
                              TOTTMP,SOILTEMP,TAIR,                     &
                              QH,QE,QN,QC,                              &
                              RGLOBUND,RGLOBABV,RGLOBABV12,RADINTERC,   &
                              ESOIL,TOTLAI, WTITLE,                     &
                              RADINTERC1,RADINTERC2,RADINTERC3,         &
                              SCLOSTTOT,SOILWP,FRACAPAR) !rajout soilwp mathias décembre 2012
! Outputs water balance results.
! RAD, May 2008
!**********************************************************************
    USE switches
    USE maestcom
    IMPLICIT NONE
    INTEGER IDAY,IHOUR,NROOTLAYER,NLAYER
    INTEGER NSUMMED,USEMEASET,IHOWMANY
    REAL FRACWATER(MAXSOILLAY),FRACUPTAKE(MAXSOILLAY)
    REAL SOILTEMP(MAXSOILLAY), SOILWP(MAXSOILLAY) ! modificatin mathias décembre 2012
    !REAL, INTENT(IN) :: THERMCOND(MAXSOILLAY)
    REAL THERMCOND(MAXSOILLAY)
    
    REAL WSOIL,WSOILROOT,PPT,CANOPY_STORE,EVAPSTORE,DRAINSTORE
    REAL SURFACE_WATERMM,ETMM,ETMM2,ETMEAS,DISCHARGE
    REAL WEIGHTEDSWP,KTOT,DRYTHICK,SOILEVAP,OVERFLOW
    REAL SOILMOISTURE,FSOIL1,TOTTMP,TAIR
    REAL QH,QE,QN,QC,RGLOBUND,RGLOBABV,RGLOBABV12,RADINTERC,ESOIL,TOTLAI
    REAL RADINTERC1,RADINTERC2,RADINTERC3,SCLOSTTOT
    REAL RNET,FRACAPAR

    CHARACTER(70) WTITLE
    
    ! Calculate net radiation at a plane above the canopy.
    ! SCLOSTTOT includes leaf and soil reflection that is not re-absor
    ! ESOIL is upward longwave transmission by the soil. It is here as
    ! that canopy T equals air T, so that the canopy is not a net radi
    RNET = RGLOBABV - SCLOSTTOT - ESOIL

    ! Average FSOIL
    !FSOIL1 = FSOIL1 / REAL(NSUMMED)
    IF(TOTTMP.EQ.0.0)THEN
        FSOIL1 = 1.0
    ELSE
        FSOIL1 = FSOIL1 / TOTTMP
    ENDIF

    !WRITE(UWATTEST,*)FSOIL1,FSOIL2

    ! Output measured ET, if it was input:
    IF(USEMEASET.EQ.1)THEN
        ETMM2 = ETMEAS
    ELSE
        ETMM2 = 0.0
    ENDIF

    IF (IOWATBAL .EQ. 1 .AND. IOFORMAT .EQ. 0) THEN
        WRITE (UWATBAL,520) IDAY+1,IHOUR,WSOIL,WSOILROOT,PPT,       &
                           CANOPY_STORE,EVAPSTORE,DRAINSTORE,       &
                           SURFACE_WATERMM,ETMM,ETMM2,DISCHARGE,    &
                           OVERFLOW*1000,WEIGHTEDSWP,KTOT,          &
                           DRYTHICK,SOILEVAP,SOILMOISTURE,          &
                           FSOIL1,QH,QE,QN,QC,                      &
                           RGLOBUND, RGLOBABV, RADINTERC, RNET,     &
                           TOTLAI,TAIR, SOILTEMP(1),SOILTEMP(2),    &
                           FRACWATER(1),FRACWATER(2),FRACAPAR

        ! Write volumetric water content by layer:
        WRITE (UWATLAY, 521) FRACWATER(1:NLAYER)

        ! write swp by layer
        WRITE (USWPLAY, 521) SOILWP(1:NLAYER) ! mathias décembre 2012

        ! Write soil temperature by layer:
        WRITE (USOILT, 522) SOILTEMP(1:NLAYER)

        ! Write fractional water uptake by layer (but no more than 40 laye
        IHOWMANY = MIN(40, NROOTLAYER)
        WRITE(UWATUPT,998)FRACUPTAKE(1:IHOWMANY)

        998   FORMAT (60(F7.4,1X))  ! 60 au lieyu de 40
        520   FORMAT (I7,I7,51(F14.4,1X))
        521   FORMAT (60(F10.4,1X)) !60 au liey de 15
        522   FORMAT (25(F10.2,1X))
    ELSE IF (IOWATBAL .EQ. 1 .AND. IOFORMAT .EQ. 1) THEN
        WRITE (UWATBAL) REAL(IDAY+1),REAL(IHOUR),WSOIL,WSOILROOT,PPT,       &
                           CANOPY_STORE,EVAPSTORE,DRAINSTORE,               &
                           SURFACE_WATERMM,ETMM,ETMM2,DISCHARGE,            &
                           OVERFLOW*1000,WEIGHTEDSWP,KTOT,          &
                           DRYTHICK,SOILEVAP,SOILMOISTURE,                  &
                           FSOIL1,QH,QE,QN,QC,                              &
                           RGLOBUND, RGLOBABV, RADINTERC, RNET,             &
                           TOTLAI,TAIR, SOILTEMP(1),SOILTEMP(2),            &
                           FRACWATER(1),FRACWATER(2)
        ! Write volumetric water content by layer:
        WRITE (UWATLAY) FRACWATER(1:NLAYER)

        ! write swp by layer
        WRITE (USWPLAY) SOILWP(1:NLAYER) ! mathias décembre 2012

        ! Write soil temperature by layer:
        WRITE (USOILT) SOILTEMP(1:NLAYER)

        ! Write fractional water uptake by layer (but no more than 40 laye
        IHOWMANY = MIN(40, NROOTLAYER)
        WRITE(UWATUPT)FRACUPTAKE(1:IHOWMANY)
    END IF
    
    RETURN
END SUBROUTINE OUTPUTWATBAL



!**********************************************************************
SUBROUTINE READDATES(UFILE, ISTART, IEND, NSTEPI)
! The routine must return start and end dates for the simulation,
! in days-since-1950 format. The function IDATE50 converts a DD/MM/YY
! date to this format. The routine must also return NSTEP where the
! program is to use met data for every NSTEP'th day (default 1).
!**********************************************************************

    IMPLICIT NONE
    INTEGER UFILE,ISTART,IEND,NSTEPI,NSTEP,IOERROR,IWARN
    INTEGER, EXTERNAL :: IDATE50
    CHARACTER(10) STARTDATE,ENDDATE
    NAMELIST /DATES/ STARTDATE,ENDDATE,NSTEP

    STARTDATE = '01/01/50'
    ENDDATE = '01/01/50'
    NSTEP = 1
    REWIND (UFILE)
    READ (UFILE, DATES, IOSTAT = IOERROR)
    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('WARNING: MISSING DATES: USING ALL MET DATA',IWARN,IOERROR)
        ISTART = 0
        IEND = 0
    ELSE
        ISTART = IDATE50(STARTDATE)
        IEND = IDATE50(ENDDATE)
    END IF
    NSTEPI = NSTEP

    RETURN
END SUBROUTINE READDATES


!**********************************************************************
SUBROUTINE READPLOT(UFILE, X0I, Y0I, XMAXI, YMAXI, NOALLTREESI, &
                    XSLOPEI, YSLOPEI, BEARI, SHADEHTI, STOCKING, IPLOTSHAPE)
! Read in plot details. Subroutine must return:
! X0,Y0 - if plot is offset from origin. All tree co-ords referenced to this.
! XMAX, YMAX - dimensions of plot, in m
! NOALLTREES - no of trees in plot
! XSLOPE, YSLOPE - slope of plot, in radians
! SHADEHT - height of shadecloth surrounding plot, if any
! STOCKING - no of stems per ha
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE,IOERROR,IPLOTSHAPE,PLOTSHAPE,NOTREES,NOALLTREESI
    
    REAL X0I, Y0I, XMAXI, YMAXI, XSLOPEI, YSLOPEI, BEARI
    REAL SHADEHTI, STOCKING, X0,Y0,XMAX,YMAX,XSLOPE,YSLOPE,BEARING,SHADEHT
    
    NAMELIST /PLOT/ X0,Y0,XMAX,YMAX,NOTREES,XSLOPE,YSLOPE,BEARING,SHADEHT,PLOTSHAPE
    

    SHADEHT = 0.0
    X0 = 0.0
    Y0 = 0.0
    PLOTSHAPE = 0

    REWIND (UFILE)
    READ (UFILE, PLOT, IOSTAT = IOERROR)
    IF (IOERROR.NE.0) CALL SUBERROR('ERROR READING PLOT DETAILS',IFATAL,IOERROR)
    X0I = X0
    Y0I = Y0
    XMAXI = XMAX
    YMAXI = YMAX
    XSLOPEI = XSLOPE*PID180
    YSLOPEI = YSLOPE*PID180
    BEARI = BEARING*PID180
    SHADEHTI = SHADEHT
    IPLOTSHAPE = PLOTSHAPE

    IF (NOTREES.GT.MAXT) THEN
        CALL SUBERROR('WARNING: NO OF TREES IN TREES FILE EXCEEDED MAXIMUM',IWARN,IOERROR)
        NOTREES = MAXT
    END IF
    NOALLTREESI = NOTREES

    IF (IPLOTSHAPE.EQ.0) THEN
        STOCKING = NOTREES/(XMAXI*YMAXI)
    ELSE
        CALL SUBERROR('ONLY SQUARE PLOT SUPPORTED IN MAESPA (SET PLOTSHAPE=0).',IFATAL,0)
        !STOCKING = NOTREES/(PI*XMAXI*YMAXI)
    END IF      !NB Round plot shape not fully supported - XY co-ords

    RETURN
END SUBROUTINE READPLOT


!**********************************************************************
SUBROUTINE READSPECLIST(UFILE,NSPECIES,ISPECIESI)
! Read species array from the trees file.
! March 2009, RAD.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER ISPECIES(MAXT),ISPECIESI(MAXT)
    INTEGER UFILE,NSPECIES,IOERROR
    NAMELIST /SPECLIST/ ISPECIES

    ISPECIES = 1

    REWIND(UFILE)
    READ (UFILE, SPECLIST, IOSTAT = IOERROR)

    IF(MAXVAL(ISPECIES).GT.NSPECIES)THEN
        CALL SUBERROR('WARNING: NSPECIES DOES NOT MATCH SPECLIST. ALL SET TO SPECIES 1', IWARN,IOERROR)
        ISPECIES = 1
    ENDIF

    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('WARNING: SPECLIST NOT READ, ALL SET TO SPECIES 1', IWARN,IOERROR)
    END IF

    ISPECIESI = ISPECIES

    RETURN
END SUBROUTINE READSPECLIST


!**********************************************************************
SUBROUTINE READZPD(UFILE,ZHTI,Z0HTI,ZPDI)
! Read in z, z0 and d for the canopy boundary layer conductance calcn.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE,IOERROR
    REAL ZHT,ZHTI,Z0HT,Z0HTI,ZPD,ZPDI
    NAMELIST /AERODYN/ ZHT,Z0HT,ZPD

    Z0HT = 0.0
    REWIND (UFILE)
    READ (UFILE, AERODYN, IOSTAT = IOERROR)

    ! Unlike MAESTRA, MAESPA stops if this namelist is not provided:
    IF ((IOERROR.NE.0).OR.(Z0HT.EQ.0.0)) THEN
        CALL SUBERROR('FATAL ERROR: AERODYN namelist required',IFATAL,IOERROR)
    END IF
    ZHTI = ZHT
    Z0HTI = Z0HT
    ZPDI = ZPD

    RETURN
END SUBROUTINE READZPD


!**********************************************************************
SUBROUTINE READCROWN(UFILE, JSHAPE, SHAPE)
! Read in crown shape parameters.
! JSHAPE indicates the shape of the crowns:
!   JCONE = conical,
!   JHELIP = half-ellipsoid,  (default)
!   JPARA = paraboloid,
!   JFELIP = full ellipsoid,
!   JCYL = cylinder,
!   JBOX = box.
! SHAPE is the factor needed to calculate volume for that crown shape:
!   VOLUME = PI * R2 * H * SHAPE (1/3, 2/3, 1/2, 2/3, 1, 4/PI).
!**********************************************************************

    USE maestcom
    IMPLICIT NONE

    INTEGER UFILE,IOERROR,JSHAPE
    REAL SHAPE
    CHARACTER(5) CSHAPE
    NAMELIST /CANOPY/ CSHAPE
    LOGICAL SHAPEREAD

    ! Modification (RAD), can now print warning when CSHAPE is miss-ty
    CSHAPE = 'EMPTY'
    SHAPEREAD = .FALSE.

    REWIND (UFILE)
    READ (UFILE, CANOPY, IOSTAT = IOERROR)
    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('WARNING: USING DEFAULT VALUE FOR CROWN SHAPE', IWARN,IOERROR)
    END IF
    IF (CSHAPE.EQ.'CONE') THEN
        JSHAPE = JCONE
        SHAPE = 1.0/3.0
        SHAPEREAD = .TRUE.
    ELSE IF (CSHAPE.EQ.'ELIP') THEN
        JSHAPE = JHELIP
        SHAPE = 2.0/3.0
        SHAPEREAD = .TRUE.
    ELSE IF (CSHAPE.EQ.'PARA') THEN
        JSHAPE = JPARA
        SHAPE = 0.5
        SHAPEREAD = .TRUE.
    ELSE IF (CSHAPE.EQ.'ROUND') THEN
        JSHAPE = JFELIP
        SHAPE = 2.0/3.0
        SHAPEREAD = .TRUE.
    ELSE IF (CSHAPE.EQ.'CYL') THEN
        JSHAPE = JCYL
        SHAPE = 1.0
        SHAPEREAD = .TRUE.
    ELSE IF (CSHAPE.EQ.'BOX') THEN
        JSHAPE = JBOX
        SHAPE = 4.0/PI
        SHAPEREAD = .TRUE.
    END IF

    IF(SHAPEREAD .EQV. .FALSE.)THEN
        CALL SUBERROR('WARNING: CROWN SHAPE NOT READ, USING DEFAULT',IWARN,IOERROR)
        JSHAPE = JHELIP
        SHAPE = 2.0/3.0
    ENDIF

    RETURN
END SUBROUTINE READCROWN


!**********************************************************************
SUBROUTINE READAGEP(UFILE, NOAGEC, NOAGEPI)
! Age classes:
! NOAGEC is the number of age classes of foliage for which distributions
! of leaf area density are provided (read from str.dat).
! NOAGEP is the number of age classes
! of foliage for which (some) physiological parameters are provided.
! If neither NOAGEC nor NOAGEP = 1, then NOAGEC must = NOAGEP.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE,NOAGEC,NOAGEPI,NOAGEP,IOERROR
    NAMELIST /NOAGES/ NOAGEP

    NOAGEP = 1  !Default value

    REWIND (UFILE)
    READ (UFILE, NOAGES, IOSTAT = IOERROR)
    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('WARNING: DEFAULT VALUE: NOAGEP=1',IWARN,IOERROR)
    END IF

    ! Check value does not exceed maximum.
    IF (NOAGEP.GT.MAXC) THEN
        CALL SUBERROR('WARNING: NOAGEP EXCEEDED MAXIMUM AGE CLASSES',IWARN,0)
        NOAGEP = MAXC
    END IF

    ! Check same as no of structural age classes
    IF ((NOAGEC.NE.1).AND.(NOAGEP.NE.1).AND.(NOAGEC.NE.NOAGEP)) THEN
        CALL SUBERROR('ERROR IN SPECIFICATION OF NO OF AGE CLASSES',IFATAL,IOERROR)
    END IF

    NOAGEPI = NOAGEP

    RETURN
END SUBROUTINE READAGEP


!**********************************************************************
SUBROUTINE READAERO(UFILE, EXTWINDI)
! Read in aerodynamic info about canopy
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE,IOERROR
    REAL EXTWIND,EXTWINDI
    NAMELIST /AERO/ EXTWIND
   
    EXTWIND = 1.0
    REWIND (UFILE)
    READ (UFILE,AERO,IOSTAT = IOERROR)

    ! Modified, RAD August 2008.
    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('WARNING: WIND EXTINCTION SET TO 1.0',IWARN,IOERROR)
    END IF

    EXTWINDI = EXTWIND

    RETURN
END SUBROUTINE READAERO 


!**********************************************************************
SUBROUTINE READLIA(UFILE, NALPHAI, ALPHA, FALPHAI,DATESLIAOUT,NOLIADATES)
! Read in leaf incidence angles.
! Must return: NALPHA = number of angle classes,
!              ALPHA = angle of each angle class,
!              FALPHA = fraction of leaf area in each angle class.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    CHARACTER(10) DATESLIA(maxdate)
    INTEGER, EXTERNAL :: IDATE50

    INTEGER NOLIADATES,NODATESLIA
    INTEGER DATESLIAOUT(maxdate)
    INTEGER UFILE,NALPHA,IOERROR,IALP,NALPHAI,IANG,IDATE,INDEX
    REAL FALPHA(MAXANG*maxdate),FALPHAI(MAXANG,maxdate),ALPHA(MAXANG)
    REAL ELP,AVGANG,AVGLIA,DALPHA
    REAL, EXTERNAL :: CALCELP
    NAMELIST /LIA/ ELP,NALPHA,FALPHA,AVGANG,NODATESLIA,DATESLIA

    ! Alternatives: (a) AVGANG > 0
    ! The mean leaf inclination angle is given by AVGANG. This is used to
    ! generate the LIA distribution assuming an elliptical distribution.
    !               (b) ELP > 0.0
    !    NALPHA = 1: there is just one leaf angle class, average angle = AVGLIA(ELP)
    !    NALPHA > 1 (max 9): there are NALPHA leaf angle classes, the distribution
    ! of angles is elliptical with parameter ELP.
    !               (c) ELP < 0.0
    ! The proportion of leaf area in each angle class is read in. Number of angle
    ! classes is given by NALPHA.
    ! The distribution of angles is read into array FALPHA(MAXANG).

    ! Default values
    NODATESLIA = 1
    DATESLIA = '01/01/99'
    AVGANG = -1.0
    NALPHA = 1
    ELP = 1.0
    FALPHA(1) = 1.0
    
    ! Read file
    REWIND (UFILE)
    READ (UFILE,LIA,IOSTAT = IOERROR)
    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('WARNING: USING DEFAULT VALUES FOR LEAF INCIDENCE ANGLE',IWARN,IOERROR)
    END IF
    
    
    DO IDATE = 1,NODATESLIA
            DATESLIAOUT(IDATE)=IDATE50(DATESLIA(IDATE))
    END DO
    
    IF (NALPHA.EQ.1) THEN
        IF (AVGANG.GT.0.0) THEN
            ALPHA(1) = AVGANG*PID180
        ELSE IF (ELP.GT.0.0) THEN
            ALPHA(1) = AVGLIA(ELP)
        END IF
        DO IDATE = 1,NODATESLIA
            FALPHAI(1,IDATE) = 1.0
        END DO
    ELSE
        IF (AVGANG.GT.0.0) ELP = CALCELP(AVGANG)
        IF (ELP.GT.0.00) THEN
            DALPHA = PID2/FLOAT(NALPHA)
            DO IALP = 1,NALPHA
                ALPHA(IALP) = (IALP-0.5)*DALPHA     ! option multi date non entré pour cas elp >0
            END DO
            CALL ANGLE(ELP,NALPHA,FALPHA)
        END IF
        DO  IANG = 1,MAXANG
            FALPHAI(IANG,1) = FALPHA(IANG)
        END DO

    END IF

    NALPHAI = NALPHA                       
    NOLIADATES = NODATESLIA
    
    IF (AVGANG.LT.0.0.AND.ELP.LT.0.0) THEN        
        DALPHA = PID2/FLOAT(NALPHAI)
        DO IALP = 1,NALPHAI
            ALPHA(IALP) = (IALP-0.5)*DALPHA     ! modification mathias février 2013 oublie du calcul des alpha
        END DO
        INDEX = 1
        DO IDATE = 1,NODATESLIA
            DO IANG = 1,NALPHAI                     
            FALPHAI(IANG,IDATE) = FALPHA(INDEX)
            INDEX = INDEX+1
            END DO
        END DO                                  
   ENDIF
        
    RETURN
END SUBROUTINE READLIA


!**********************************************************************
SUBROUTINE READSPECIES(UFILE,NSPECIES,SPECIESNAMES,PHYFILES,STRFILES)

! Read species namelist (RAD, March 2009).
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE,IOERROR,NSPECIES
    NAMELIST /SPECIES/ NSPECIES, SPECIESNAMES, PHYFILES, STRFILES

    CHARACTER(30) SPECIESNAMES(MAXSP)
    CHARACTER(30) PHYFILES(MAXSP)
    CHARACTER(30) STRFILES(MAXSP)

    ! Set defaults
    NSPECIES = 1
    PHYFILES(1) = 'phy.dat'
    STRFILES(1) = 'str.dat'

    ! Read file
    REWIND (UFILE)
    READ (UFILE, SPECIES, IOSTAT = IOERROR)
    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('WARNING: NO MULTI SPECIES OPTIONS READ IN CONFILE.DAT',IWARN,0)
    END IF

    RETURN
END SUBROUTINE READSPECIES



!**********************************************************************
SUBROUTINE READZEN(UFILE,NUMPNT,NOLAYI,PPLAYI,NZENI,NAZI,DIFZEN)
! Read in number of layers and angles to consider.
! Vertical layers:
!   NOLAY is the number of vertical layers used to calculate radiation
! interception (NUMPNT = NOLAY * points per layer).
! Angles for diffuse radiation:
!   NZEN = no. of zenith angles (default 5)
!   NAZ = no. of azimuth angles (default 11) - no maximum enforced
!   DIFZEN = array containing the NZEN zenith angles (radians).
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    NAMELIST /DIFFANG/ NOLAY,PPLAY,NZEN,NAZ
    INTEGER UFILE,PPLAY,PPLAYI,NOLAY,NZEN,NAZ
    INTEGER IOERROR,NOLD,I,NUMPNT,NOLAYI,NZENI,NAZI
    INTEGER, EXTERNAL :: DIVFOURNEAR
    REAL DIFZEN(MAXANG),DFZ

    ! Default values
    PPLAY = 12
    NOLAY = 6
    NAZ = 11
    NZEN = 5

    ! Read file
    REWIND (UFILE)
    READ (UFILE, DIFFANG, IOSTAT = IOERROR)
    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('WARNING: USING DEFAULT VALUES FOR NOLAY,PPLAY,NAZ & NZEN', IWARN,IOERROR)
    END IF

    ! Make sure PPLAY can be divided by 4.
    NOLD = PPLAY
    PPLAY = DIVFOURNEAR(PPLAY)
    IF(NOLD.NE.PPLAY)THEN
        CALL SUBERROR('WARNING: PPLAY SET TO NEAREST SMALLER VALUE DIVISIBLE BY 4.',IWARN,0)
    ENDIF

    ! Check values do not exceed maxima.
    IF (NOLAY.GT.MAXLAY) THEN
        CALL SUBERROR('WARNING: EXCEEDED MAXIMUM NO OF LAYERS',IWARN,0)
        NOLAY = MAXLAY
    END IF
    IF (NZEN.GT.MAXANG) THEN
        CALL SUBERROR('WARNING: EXCEEDED MAXIMUM NO OF ZENITH ANGLES',IWARN,0)
        NZEN = MAXANG
    END IF

    ! Set zenith angle array
    DFZ = PID2/FLOAT(NZEN)
    DO I = 1,NZEN
        DIFZEN(I) = (I-0.5)*DFZ
    END DO

    NUMPNT = NOLAY * PPLAY
    PPLAYI = PPLAY
    NOLAYI = NOLAY
    NZENI = NZEN
    NAZI = NAZ

    RETURN
END SUBROUTINE READZEN


!**********************************************************************
SUBROUTINE READALLOM(UFILE, COEFFTI, EXPONTI, WINTERCI,     &
                        BCOEFFTI, BEXPONTI, BINTERCI,       &
                        RCOEFFTI, REXPONTI, RINTERCI, FRFRACI)
! Read in coefficients for allometric relation between stem
! height, diameter, and stem mass.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE,IOERROR
    REAL COEFFT,WINTERC,EXPONT,RCOEFFT,RINTERC,REXPONT
    REAL FRFRAC,BCOEFFT,BEXPONT,BINTERC,COEFFTI,EXPONTI
    REAL WINTERCI,BCOEFFTI,BEXPONTI,BINTERCI,RCOEFFTI
    REAL REXPONTI,RINTERCI,FRFRACI
    NAMELIST /ALLOM/ COEFFT, EXPONT, WINTERC
    NAMELIST /ALLOMB/ BCOEFFT, BEXPONT, BINTERC
    NAMELIST /ALLOMR/ RCOEFFT, REXPONT, RINTERC, FRFRAC
    
    ! Default values
    COEFFT = 0.0
    WINTERC = 0.0
    EXPONT = 2.
    RCOEFFT = 0.0
    RINTERC = 0.0
    REXPONT = 2.
    FRFRAC = 1.0
    BCOEFFT = 0.0
    BEXPONT = 2.

    ! Read file
    REWIND (UFILE)
    READ (UFILE, ALLOM, IOSTAT = IOERROR)
    !IF (COEFFT.GT.0.0) THEN
        !        CALL SUBERROR('CALCULATING WOODY RESPIRATION',
        !     &  IWARN,IOERROR)
    !END IF

    REWIND (UFILE)
    READ (UFILE, ALLOMB, IOSTAT = IOERROR)
    !IF (RCOEFFT.GT.0.0) THEN
        !        CALL SUBERROR('CALCULATING BRANCH RESPIRATION',
        !     &  IWARN,IOERROR)
    !END IF

    REWIND (UFILE)
    READ (UFILE, ALLOMR, IOSTAT = IOERROR)
    !IF (RCOEFFT.GT.0.0) THEN
        !       CALL SUBERROR('CALCULATING ROOT RESPIRATION',
        !    &  IWARN,IOERROR)
    !END IF

    COEFFTI = COEFFT
    EXPONTI = EXPONT
    WINTERCI = WINTERC
    BCOEFFTI = BCOEFFT
    BEXPONTI = BEXPONT
    BINTERCI = BINTERC
    RCOEFFTI = RCOEFFT
    REXPONTI = REXPONT
    RINTERCI = RINTERC
    FRFRACI = FRFRAC

    RETURN
END SUBROUTINE READALLOM


!**********************************************************************
SUBROUTINE READPROP(UFILE, NOAGEC, NOAGEP, PROPCI, PROPPI)
! Read in the proportion of leaf area in each age class. The number
! of age classes is given by NOAGEC (for beta distributions) and
! NOAGEP (for physiological parameters).
! BM 7/03 Getting into troble with PROP when NOAGEC = 1 and NOAGEP > 1:
! Fix by having different proportions
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER IOERROR,UFILE,I,NAGE,NOAGEP,NOAGEC,IAGE
    NAMELIST /PHENOL/ PROP
    REAL PROP(MAXC),PROPCI(MAXC),PROPPI(MAXC)
    REAL TOTAL
    
    DO I = 1,MAXC
        PROP(I) = 0.0
    END DO

    ! Find number of age classes
    NAGE = NOAGEC
    IF (NOAGEP.GT.NAGE) NAGE = NOAGEP

    ! Only need to read in proportions if there is > 1 age class.
    IF (NAGE.GT.1) THEN
        REWIND (UFILE)
        READ (UFILE, PHENOL, IOSTAT = IOERROR)
        IF (IOERROR.NE.0) THEN
            CALL SUBERROR('ERROR: NEED DATA ON AGE CLASS PROPORTIONS',IFATAL,IOERROR)
        END IF
    ELSE
        PROP(1) = 1.0
    END IF

    ! Check proportions sum to one
    TOTAL = 0.0
    DO I = 1,NAGE
        TOTAL = TOTAL + PROP(I)
    END DO
    IF ((TOTAL.LT.0.9999).OR.(TOTAL.GT.1.0001))CALL SUBERROR('ERROR: AGE CLASS PROPORTIONS DO NOT SUM TO ONE',IFATAL,0)

    IF (NOAGEC.EQ.1) THEN
        PROPCI(1) = 1.0
    ELSE
        DO IAGE = 1,MAXC
            PROPCI(IAGE) = PROP(IAGE)
        END DO
    END IF

    IF (NOAGEP.EQ.1) THEN
        PROPPI(1) = 1.0
    ELSE
        DO  IAGE = 1,MAXC
            PROPPI(IAGE) = PROP(IAGE)
        END DO
    END IF

    RETURN
END  SUBROUTINE READPROP


!**********************************************************************
SUBROUTINE READBETA(UFILE, NOAGECI, JLEAFI, BPTI, RANDOMI,DATESLADOUT,NOLADDATES)
! Read in beta distributions for leaf area. The number of age classes
! for which beta distributions are specified is given by NOAGEC.
! Function returns:
! switch JLEAF:
!   JLEAF = 0: Uniform leaf area density assumed
!   JLEAF = 1: Leaf area density is variable in vertical direction
!   JLEAF = 2: Leaf area density is variable in both horizontal & vertical
! array BPT: gives the coefficients of the beta distributions
! the clumping factor RANDOM (= shoot projected area: leaf projected area).
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    CHARACTER(10) DATESLAD(maxdate)
    INTEGER, EXTERNAL :: IDATE50

    INTEGER NOLADDATES,NODATESLAD
    INTEGER DATESLADOUT(maxdate)
    INTEGER JLEAF,JLEAFI,UFILE,NOAGECI
    INTEGER NOAGEC,I,J,IOERROR,IDATE
    REAL RANDOM,RANDOMI
    
    REAL BPT(6*MAXC*maxdate),BPTEXT(2*MAXC*MAXDATE),BPTI(8,MAXC,maxdate)
    NAMELIST /LADD/ NOAGEC,JLEAF,BPT,BPTEXT,RANDOM,NODATESLAD,DATESLAD
    
    ! BM 11/99 added another parameter to BETA function - input via BPTEXT - default value 1

    ! Default values
    NODATESLAD=1
    DATESLAD = '01/01/99'
    NOAGEC = 1
    JLEAF = 0
    RANDOM = 1.0
    DO J = 1,2*MAXC*MAXDATE
        BPTEXT(J) = 1.0
        DO I = 1,6*MAXC*MAXDATE
            BPT(I) = 0.0
        END DO
    END DO        

    ! Read file
    REWIND (UFILE)
    READ (UFILE, LADD, IOSTAT = IOERROR)
    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('WARNING: DEFAULT VALUE: UNIFORM LEAF AREA DENSITY', IWARN,IOERROR)
    END IF
       
    DO IDATE = 1,NODATESLAD
         DATESLADOUT(IDATE)=IDATE50(DATESLAD(IDATE))
    END DO
    NOLADDATES = NODATESLAD
    NOAGECI = NOAGEC
    JLEAFI = JLEAF
    RANDOMI = RANDOM
    
    DO IDATE = 1,NOLADDATES
        DO J = 1,MAXC
            DO I = 0,1
                BPTI(I*4+1,J,IDATE) = BPT((IDATE-1)*6*NOAGECI+(I*3+1)+(J-1)*6)           
                BPTI(I*4+2,J,IDATE) = BPT((IDATE-1)*6*NOAGECI+(I*3+2)+(J-1)*6)           
                BPTI(I*4+3,J,IDATE) = BPT((IDATE-1)*6*NOAGECI+(I*3+3)+(J-1)*6)           
                BPTI(I*4+4,J,IDATE) = BPTEXT((IDATE-1)*2*NOAGECI+I+1+(J-1)*2)          
            END DO
        END DO    
    END DO
    
    ! Elementary error checking:
    ! If not using uniform leaf area density
    IF (JLEAF.NE.0) THEN
        ! If the first coefficient of the first age class = 0, or
        IF ((BPT(1).EQ.0.0).OR.((NOAGEC.GT.1).AND.(BPT(7).EQ.0.0))) THEN
            ! If there's >1 age class and the first coefft of the second age class = 0
           CALL SUBERROR('ERROR: MISSING BETA FUNCTION COEFFICIENTS', IFATAL,IOERROR)
        END IF
    END IF
    RETURN
END SUBROUTINE READBETA


!**********************************************************************
SUBROUTINE READMODEL(UFILE, GSMODI, JMMODI, RDMODI, &
                    SSMODI, RWMODI, ITERMAXI)
! Read in flags which control the physiological models used.
! MODELGS - controls model of stomatal conductance
! MODELJM - whether JMAX,VCMAX read in (0) or calculated from leaf N (1)
! MODELRD - whether RD is a function of leaf area (0) or leaf N (1)
! MODELSS - whether photosynthesis is calculated for sunlit & shaded leaves
!   together or separately
! ITERMAX - no. of iterations to be used in solving for leaf temperature
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE,GSMODI,JMMODI,RDMODI,SSMODI,RWMODI,ITERMAXI
    INTEGER MODELGS,MODELJM,MODELRD,MODELRW,MODELSS
    INTEGER ITERMAX,IOERROR
    
    NAMELIST /MODEL/ MODELGS,MODELJM,MODELRD,MODELRW, &
                        MODELSS,ITERMAX
    

    ! Default values
    MODELGS = 0     ! The Jarvis model of stom cond
    MODELJM = 0     ! Jmax & Vcmax read in directly
    MODELRD = 0     ! Rd0 read in directly
    MODELRW = 0     ! RW values read in directly
    MODELSS = 0     ! sunlit & shade calculations separate
    ITERMAX = 0     ! The leaf temperature is not calculated

    ! Read file
    REWIND (UFILE)
    READ (UFILE, MODEL, IOSTAT = IOERROR)
    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('WARNING: DEFAULT VALUES USED FOR PHYSIOLOGICAL CONTROL',IWARN,IOERROR)
    END IF
    GSMODI = MODELGS
    RDMODI = MODELRD
    RWMODI = MODELRW
    JMMODI = MODELJM
    SSMODI = MODELSS
    ITERMAXI = ITERMAX

    RETURN
END SUBROUTINE READMODEL


!**********************************************************************
SUBROUTINE READHIST(UFILE, IOHIST, BINSIZEI)
! Read in information for the PAR histogram if required.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE,IOHIST,IOERROR
    REAL BINSIZE,BINSIZEI
    NAMELIST /HISTO/ BINSIZE
    
    IF (IOHIST.EQ.1) THEN
        REWIND (UFILE)
        READ (UFILE, HISTO, IOSTAT = IOERROR)
        IF (IOERROR.NE.0) THEN
            CALL SUBERROR('ERROR: MISSING INFO FOR HISTOGRAM',IFATAL,IOERROR)
        END IF
        BINSIZEI = BINSIZE
    END IF
    RETURN
END SUBROUTINE READHIST


!**********************************************************************
SUBROUTINE READCCSCEN(UFILE, ICC, CO2INCI, TINCI)
! Read in details of climate change scenario.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE,ICC
    REAL CO2INC,TINC,CO2INCI,TINCI
    NAMELIST /CCSCEN/ CO2INC, TINC
    
    ! Default values
    ICC = 0
    CO2INC = 0.0
    TINC = 0.0
    ! Read file
    REWIND (UFILE)
    READ (UFILE, CCSCEN, IOSTAT = ICC)
    IF (ICC.EQ.0) CALL SUBERROR('APPLYING CLIMATE CHANGE SCENARIO',IWARN,0)
    CO2INCI = CO2INC
    TINCI = TINC

    RETURN
END SUBROUTINE READCCSCEN


!**********************************************************************
SUBROUTINE READOTC(UFILE, IOTC, TOTCI, WINDOTCI, PAROTCI, FBEAMOTCI)
! Subroutine to read in parameters describing effect of OTC on met data.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE,IOTC
    REAL TOTCI,WINDOTCI,PAROTCI,FBEAMOTCI
    REAL TOTC,WINDOTC,PAROTC,FBEAMOTC
    NAMELIST /OTC/ TOTC,WINDOTC,PAROTC,FBEAMOTC
    
    ! Default values
    IOTC = 0
    TOTC = 0
    WINDOTC = -1
    PAROTC = 1.0
    FBEAMOTC = 1.0
    ! Read file
    REWIND (UFILE)
    READ (UFILE, OTC, IOSTAT = IOTC)
    IF (IOTC.EQ.0)CALL SUBERROR('APPLYING EFFECTS OF OTC ON MET DATA',IWARN,0)
    TOTCI = TOTC
    WINDOTCI = WINDOTC
    PAROTCI = PAROTC
    FBEAMOTCI = FBEAMOTC

    RETURN
END SUBROUTINE READOTC

!**********************************************************************
SUBROUTINE READABSRP(UFILE,NOLAY,ABSRP,REFLEC,TRANS,RHOSOLI)
! Read leaf absorptances and reflectances for 3 wavelengths.
! Required input: File unit (UFILE), No of layers (NOLAY).
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE,IOERROR,NOLAY,NOLAYERS,IP,J,I
    REAL ABSRP(MAXLAY,3),REFLEC(MAXLAY,3),TRANS(MAXLAY,3)
    REAL ARHO(MAXLAY*MAXC*maxdate),ATAU(MAXLAY*MAXC*maxdate)
    REAL RHOSOL(3),RHOSOLI(3),X(MAXLAY+1)
    REAL D1,D2
    REAL, EXTERNAL :: RINTEG
    NAMELIST /ABSORP/ NOLAYERS,RHOSOL,ATAU,ARHO
    
    ! Read file
    REWIND (UFILE)
    READ (UFILE,ABSORP,IOSTAT = IOERROR)
    IF (IOERROR.NE.0) CALL SUBERROR('ERROR READING REFLECTANCE / TRANSMITTANCE', IFATAL,IOERROR)

    DO IP = 1,NOLAYERS+1
        X(IP) = (REAL(IP) - 1.0)/REAL(NOLAYERS)
    END DO

    ! Check values & set absorptance array.
      DO J = 1,3
        RHOSOLI(J) = RHOSOL(J)
        DO I = 1,NOLAY
            D1 = (REAL(I) - 1)/REAL(NOLAY)                           
            D2 = (REAL(I))/REAL(NOLAY)                               
            REFLEC(I,J) = RINTEG(D1,D2,X,ARHO,(J-1)*NOLAYERS,NOLAY)  
            TRANS(I,J) = RINTEG(D1,D2,X,ATAU,(J-1)*NOLAYERS,NOLAY)   
            ABSRP(I,J) = 1.0 - REFLEC(I,J) - TRANS(I,J)              
        END DO
    END DO        

    100   FORMAT(10(1X,F8.3))
    RETURN
END SUBROUTINE READABSRP


!**********************************************************************
SUBROUTINE READGS(UFILE,I,MODELGS,                                                  &
                    GSREFI,GSMINI,PAR0I,D0I,VK1I,VK2I,VPD1I,VPD2I,VMFD0I,           &
                    GSJAI,GSJBI,T0I,TREFI,TMAXI,SMD1I,SMD2I,WC1I,WC2I,SWPEXPI,      &
                    G0TABLEI,G1TABLEI,GKI,NOGSDATES,DATESGSI,D0LI,GAMMAI,VPDMINI,       &
                    WLEAFTABLEI,NSIDESI,SF,PSIV,DATESWLEAFI,NOWLEAFDATES)
! Get stomatal conductance parameters.
! Required input: File unit (UFILE), Stom cond model (MODELGS).
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    
    INTEGER UFILE,NODATESGS,MODELGS,NSIDES,NSIDESI,I,NODATESWLEAF
    INTEGER DATESGSI(maxdate),IOERROR,IDATE,NOGSDATES,NOWLEAFDATES,DATESWLEAFI(maxdate)
    INTEGER, EXTERNAL :: IDATE50
    CHARACTER(10) DATES(maxdate),DATESWLEAF(maxdate)
    CHARACTER(3) CONDUNITS
    REAL G0TABLEI(maxdate),G1TABLEI(maxdate),WLEAFTABLEI(maxdate)
    REAL G0(maxdate),G1(maxdate),WLEAF(maxdate)
    REAL GK,GKI
    REAL G0I(maxdate),G1I(maxdate)
    REAL GSREFI,GSMINI,PAR0,D0,VPD1,VPD2,VMFD0,GSJA,GSJB
    REAL T0,TREF,TMAX,SMD1,SMD2,VK1,VK2,SWPEXP
    REAL GSREF,GSMIN,GAMMA,WC1,WC2,D0L,GAMMAI
    REAL SMD1I,SMD2I,WC1I,WC2I,SWPEXPI,D0LI
    REAL PAR0I,D0I,VPD1I,VPD2I,VK1I,VK2I,VMFD0I
    REAL GSJAI,GSJBI,T0I,TREFI,TMAXI,WLEAFI,SF,PSIV,SFI,PSIVI
    REAL VPDMIN,VPDMINI
    
!    NAMELIST /JARGS/ GSREF,GSMIN,PAR0,D0,VPD1,VPD2,VMFD0,GSJA,GSJB, &
!                     T0,TREF,TMAX,SMD1,SMD2,VK1,VK2,WLEAF,NSIDES,SWPEXP
    NAMELIST /BBGS/ DATES,G0,G1,GAMMA,WLEAF,NSIDES, &
                    SMD1,SMD2,WC1,WC2,SWPEXP,DATESWLEAF
    NAMELIST /BBLGS/DATES, G0,G1,D0L,GAMMA,WLEAF,NSIDES, &
                    SMD1,SMD2,WC1,WC2,SWPEXP,DATESWLEAF
    NAMELIST /BBMGS/DATES, G0,G1,GAMMA,VPDMIN,GK,WLEAF,NSIDES,WC1,WC2,DATESWLEAF
    NAMELIST /BBTUZ/DATES, G0,G1,SF,PSIV,GAMMA,VPDMIN,WLEAF,NSIDES,DATESWLEAF
    NAMELIST /BBGSCON/ NODATESGS,CONDUNITS,NODATESWLEAF
    
    ! Defaults
    GSMIN = 0.001
    SMD1 = 0.0
    SMD2 = 0.0
    G0 = 0.0
    G1 = 0.0
    WLEAF = 0.0
    WC1 = 0.0
    WC2 = 0.0
    VPDMIN = 50  ! Minimum VPD to feed to GS model (caps gs at low VPD)
    GK = 0.5
    NODATESGS = 1
    NODATESWLEAF = 1
    DATES(1) = '01/01/99'
    DATESWLEAF(1) = '01/01/99'   
    
    
    REWIND(UFILE)

    ! Ball-Berry and BBL Dates, and optionally units of conductance.
    CONDUNITS = 'CO2'  ! DEFAULT
    IF (MODELGS.GE.1) THEN
        READ (UFILE,BBGSCON,IOSTAT = IOERROR)
    ENDIF

    IF (MODELGS.EQ.1) THEN
        CALL SUBERROR('JARVIS STOMATAL CONDUCTANCE MODEL IS NOT SUPPORTED IN MAESPA (BUT IT IS IN MAESTRA).', &
            IFATAL,IOERROR)
    ENDIF
    
    IF (MODELGS.EQ.2)THEN
        READ (UFILE,BBGS,IOSTAT = IOERROR)
        DO IDATE = 1,NODATESGS
            DATESGSI(IDATE) = IDATE50(DATES(IDATE))
        END DO
        DO IDATE = 1,NODATESWLEAF
            DATESWLEAFI(IDATE)=IDATE50(DATESWLEAF(IDATE))
        END DO

        ! If conductance pars given for water:
        IF(CONDUNITS.EQ.'H2O'.OR.CONDUNITS.EQ.'h2o')THEN
            G0 = G0 / GSVGSC
            G1 = G1 / GSVGSC
            CALL SUBERROR('GS PARAMETERS FOR H2O WERE CONVERTED TO CO2.', IWARN,0)
        ELSE
            CALL SUBERROR('GS PARAMETERS ARE ASSUMED TO BE FOR CO2.',IWARN,0)
        ENDIF

        G0TABLEI = G0
        G1TABLEI = G1
        GAMMAI = GAMMA
        VPDMINI = VPDMIN
        SMD1I = SMD1
        SMD2I = SMD2
        WC1I = WC1
        WC2I = WC2
        SWPEXPI = SWPEXP
        NOGSDATES = NODATESGS
        NOWLEAFDATES = NODATESWLEAF
        WLEAFTABLEI = WLEAF
    ELSE IF (MODELGS.EQ.3) THEN   ! Ball-Berry Leuning model parameter
        READ (UFILE, BBLGS, IOSTAT = IOERROR)
        DO IDATE = 1,NODATESGS
            DATESGSI(IDATE) = IDATE50(DATES(IDATE))
        ENDDO
        DO IDATE = 1,NODATESWLEAF
            DATESWLEAFI(IDATE)=IDATE50(DATESWLEAF(IDATE))
        END DO

        ! If conductance pars given for water:
        IF(CONDUNITS.EQ.'H2O'.OR.CONDUNITS.EQ.'h2o')THEN
            G0 = G0 / GSVGSC
            G1 = G1 / GSVGSC
            CALL SUBERROR('GS PARAMETERS FOR H2O WERE CONVERTED TO CO2.',IWARN,0)
        ELSE
            CALL SUBERROR('GS PARAMETERS ARE ASSUMED TO BE FOR CO2.', IWARN,0)
        ENDIF

        G0TABLEI = G0
        G1TABLEI = G1
        GAMMAI = GAMMA
        D0LI = D0L
        SMD1I = SMD1
        SMD2I = SMD2
        WC1I = WC1
        WC2I = WC2
        SWPEXPI = SWPEXP
        NOGSDATES = NODATESGS
        NOWLEAFDATES = NODATESWLEAF
        WLEAFTABLEI = WLEAF
        IF (D0L.LT.0.0) CALL SUBERROR('ERROR IN GS PARAMETERS: D0L MUST BE > 0', IFATAL,0)

    ELSE IF (MODELGS.EQ.4) THEN   ! Ball-Berry-Medlyn parameters
        READ (UFILE, BBMGS, IOSTAT = IOERROR)

        DO IDATE = 1,NODATESGS
            DATESGSI(IDATE) = IDATE50(DATES(IDATE))
        END DO
        DO IDATE = 1,NODATESWLEAF
            DATESWLEAFI(IDATE)=IDATE50(DATESWLEAF(IDATE))
        END DO

        ! New Medlyn et al 2011 model (corrigendum):
        ! g1 MUST be for H2O
        CALL SUBERROR('GS PARAMETERS ARE ASSUMED TO BE FOR H2O!!!', IWARN,0)
        
        G0TABLEI = G0
        G1TABLEI = G1
        GAMMAI = GAMMA
        GKI = GK
        VPDMINI = VPDMIN*1000
        NOGSDATES = NODATESGS
        NOWLEAFDATES = NODATESWLEAF
        WLEAFTABLEI = WLEAF
    ELSE IF (MODELGS.EQ.6) THEN   ! Ball-Berry-Tuzet parameters
        
        READ (UFILE, BBTUZ, IOSTAT = IOERROR)

        DO IDATE = 1,NODATESGS
            DATESGSI(IDATE) = IDATE50(DATES(IDATE))
        END DO
        
        DO IDATE = 1,NODATESWLEAF
            DATESWLEAFI(IDATE) = IDATE50(DATESWLEAF(IDATE))
        END DO
        
        ! If conductance pars given for water:
        IF(CONDUNITS.EQ.'H2O'.OR.CONDUNITS.EQ.'h2o')THEN
            G0 = G0 / GSVGSC
            G1 = G1 / GSVGSC
            CALL SUBERROR('GS PARAMETERS FOR H2O WERE CONVERTED TO CO2.', IWARN,0)
        ELSE
            CALL SUBERROR('GS PARAMETERS ARE ASSUMED TO BE FOR CO2.', IWARN,0)
        ENDIF

        G0TABLEI = G0
        G1TABLEI = G1
        GAMMAI = GAMMA
        VPDMINI = VPDMIN*1000
        SFI = SF
        PSIVI = PSIV
        NOGSDATES = NODATESGS
        NOWLEAFDATES = NODATESWLEAF
        WLEAFTABLEI = WLEAF
    END IF
    
    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('ERROR READING STOMATAL CONDUCTANCE PARAMETERS', IFATAL,IOERROR)
    END IF
    NSIDESI = NSIDES
    
    RETURN
END SUBROUTINE READGS


!**********************************************************************
SUBROUTINE READLEAFN(UFILE,MODELJM,MODELRD,NOLAY,NOAGEP, &
                        NONDATES,DATESN,VALUESN)
! If MODELJM or MODELRD = 1, then leaf N contents are required to
! calculate Jmax or Rd. This subroutine then reads in the leaf N.
! Leaf N may be specified for a maximum of maxdate dates -
! linear interpolation is used between those dates.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER DATESN(maxdate)
    INTEGER UFILE,MODELJM,MODELRD,NOLAY,NOAGEP,NONDATES
    REAL VALUESN(maxdate,MAXLAY,MAXC)

    IF ((MODELJM.EQ.1).OR.(MODELRD.EQ.1)) THEN  !Leaf n contents reqd.
        CALL READPHYARRAY(UFILE,1,NOLAY,NOAGEP,NONDATES,DATESN,VALUESN)
    END IF

    RETURN
END SUBROUTINE READLEAFN


!**********************************************************************
SUBROUTINE READPHYARRAY(UFILE,NARRAY,NOLAY,NOAGEP,NDATE,IDATES,VALUESI)
! Read in an array of physiological parameters from UFILE.
! NARRAY is the number of the array to be read (1 = LEAFN; 2 = JMAX;
! 3 = VCMAX; 4 = RD; 5 = SLA; 6 = ALPHAJ)
! Parameters are given for up to maxdate dates, up to MAXLAY layers,
! and up to MAXC age classes.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER IDATES(maxdate),NOLAY,NOAGEP,NDATE,NOLAYERS,NODATES,NOAGES
    INTEGER UFILE,I,NARRAY,IOERROR,IP,IDATE,IAGE,ILAY
    REAL VALUES(maxdate*MAXLAY*MAXC),VALUESI(maxdate,MAXLAY,MAXC)
    REAL X(MAXLAY+1),D1,D2
    CHARACTER(10) DATES(maxdate)
    INTEGER, EXTERNAL :: IDATE50
    REAL, EXTERNAL :: RINTEG
    
    NAMELIST /NFOLCON/ NODATES,NOLAYERS,NOAGES
    NAMELIST /NFOL/ DATES,VALUES
    NAMELIST /JMAXCON/ NODATES,NOLAYERS,NOAGES
    NAMELIST /JMAX/ DATES,VALUES
    NAMELIST /VCMAXCON/ NODATES,NOLAYERS,NOAGES
    NAMELIST /VCMAX/ DATES,VALUES
    NAMELIST /RDCON/ NODATES,NOLAYERS,NOAGES
    NAMELIST /RD/ DATES,VALUES
    NAMELIST /SLACON/ NODATES,NOLAYERS,NOAGES
    NAMELIST /SLA/ DATES,VALUES
    NAMELIST /AJQCON/ NODATES,NOLAYERS,NOAGES
    NAMELIST /AJQ/ DATES,VALUES

    ! Default values
    NODATES = 1
    NOLAYERS = 1
    NOAGES = 1
    DATES(1) = '01/01/50'
    DO I = 1,maxdate*MAXLAY*MAXC
        VALUES(I) = -1.0
    END DO

    ! Read array size from file
    REWIND(UFILE)
    IF (NARRAY.EQ.1) THEN
        READ(UFILE,NFOLCON,IOSTAT=IOERROR)
        !CALL SUBERROR('LEAF N ARRAY:',IWARN,0)
    ELSE IF (NARRAY.EQ.2) THEN
        READ(UFILE,JMAXCON,IOSTAT=IOERROR)
        !CALL SUBERROR('JMAX ARRAY:',IWARN,0)
    ELSE IF (NARRAY.EQ.3) THEN
        READ(UFILE,VCMAXCON,IOSTAT=IOERROR)
        !CALL SUBERROR('VCMAX ARRAY:',IWARN,0)
    ELSE IF (NARRAY.EQ.4) THEN
        READ(UFILE,RDCON,IOSTAT=IOERROR)
        !CALL SUBERROR('RD ARRAY:',IWARN,0)
    ELSE IF (NARRAY.EQ.5) THEN
        READ(UFILE,SLACON,IOSTAT=IOERROR)
        IF (IOERROR.EQ.0) THEN
        !CALL SUBERROR('SLA ARRAY:',IWARN,0)
        ELSE
            CALL SUBERROR('NO SLA VALUES: FOLIAGE GROWTH RESP NOT CALCULATED', IWARN,IOERROR)
            NDATE = 0
            RETURN ! No SLA values - end subroutine here
        END IF
    ELSE IF (NARRAY.EQ.6) THEN
        READ(UFILE,AJQCON,IOSTAT=IOERROR)
        IF (IOERROR.EQ.0) THEN
            !CALL SUBERROR('AJQ ARRAY:',IWARN,0)
        ELSE
            CALL SUBERROR('NO AJQ ARRAY; DEFAULT VALUE USED', IWARN,0)
            NODATES = 0
            RETURN ! No AJQ values - end subroutine here
        END IF
    END IF

    ! Error handling
    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('WARNING: PROBLEM READING ARRAY SIZE',IWARN,IOERROR)
    END IF
    IF ((NODATES.GT.maxdate).OR.(NOLAYERS.GT.MAXLAY).OR.(NOAGES.GT.MAXC)) THEN
        CALL SUBERROR('WARNING: ARRAY EXCEEDED BOUNDS: SOME DATA LOST',IWARN,IOERROR)
        IF (NODATES.GT.maxdate) NODATES = maxdate
        IF (NOLAYERS.GT.MAXLAY) NOLAYERS = MAXLAY
        IF (NOAGES.GT.MAXC) NOAGES = MAXC
    END IF
    IF ((NOAGES.GT.1).AND.(NOAGEP.NE.NOAGES)) THEN
        CALL SUBERROR('ERROR: PHYSIOLOGY CLASSES & PARAMETER CLASSES DO NOT COINCIDE',IWARN,IOERROR)
    END IF

    ! Read arrays from file
    REWIND(UFILE)
    IF (NARRAY.EQ.1) THEN
        READ(UFILE,NFOL,IOSTAT=IOERROR)
    ELSE IF (NARRAY.EQ.2) THEN
        READ(UFILE,JMAX,IOSTAT=IOERROR)
    ELSE IF (NARRAY.EQ.3) THEN
        READ(UFILE,VCMAX,IOSTAT=IOERROR)
    ELSE IF (NARRAY.EQ.4) THEN
        READ(UFILE,RD,IOSTAT=IOERROR)
    ELSE IF (NARRAY.EQ.5) THEN
        READ(UFILE,SLA,IOSTAT=IOERROR)
    ELSE IF (NARRAY.EQ.6) THEN
        READ(UFILE,AJQ,IOSTAT=IOERROR)
    END IF

    ! Error handling
    IF (IOERROR.NE.0) THEN
        CALL SUBERROR('ERROR: PROBLEM READING ARRAY',IFATAL,IOERROR)
    END IF

    ! Check values
    DO I = 1,NOAGES*NOLAYERS*NODATES
        IF (VALUES(I).LT.0.0) CALL SUBERROR('ERROR: VALUES MISSING FROM ARRAY',IFATAL,0)
    END DO

    ! Set up x array
    DO IP = 1,NOLAYERS+1
        X(IP) = (REAL(IP) - 1.0)/REAL(NOLAYERS)
    END DO

    ! Assign arrays to data
    DO IDATE = 1,NODATES
        IDATES(IDATE) = IDATE50(DATES(IDATE))
    END DO
    DO IDATE = 1,NODATES
        DO IAGE = 1,NOAGES
            DO I = 1,NOLAY
                D1 = (REAL(I) - 1)/REAL(NOLAY)
                D2 = (REAL(I))/REAL(NOLAY)
                VALUESI(IDATE,I,IAGE) = RINTEG(D1,D2,X,VALUES,(IDATE-1)*NOAGES*NOLAYERS+(IAGE-1)*NOLAYERS,NOLAY)
            END DO
        END DO
    END DO                    

    ! Fill in array in case no of ages is less than NOAGEP
    IF (NOAGES.LT.NOAGEP) THEN
    DO IDATE = 1,NODATES
        DO ILAY = 1,NOLAY
            DO I = NOAGES+1,NOAGEP
                VALUESI(IDATE,ILAY,I) = VALUESI(IDATE,ILAY,1)
            END DO
        END DO
    END DO                    
    END IF

    NDATE = NODATES

    RETURN
END SUBROUTINE READPHYARRAY


!**********************************************************************
SUBROUTINE READVSTRESS(UFILE, VPARAI, VPARBI, VPARCI, VFUNI)
!**********************************************************************

USE maestcom
IMPLICIT NONE
INTEGER VFUN, VFUNI, UFILE, IOERROR
REAL VPARA, VPARB, VPARC
REAL VPARAI, VPARBI, VPARCI

NAMELIST /VJMAXSTRESS/ VFUN,VPARA,VPARB,VPARC

    ! Default values (no effect of SWP on Vcmax, Jmax).
    VPARA = -200
    VPARB = -199
    VPARC = 1
    VFUN = 0

    ! Read namelist
    REWIND(UFILE)
    READ (UFILE,VJMAXSTRESS,IOSTAT = IOERROR)

    VPARAI = VPARA
    VPARBI = VPARB
    VPARCI = VPARC
    VFUNI = VFUN
    
    IF(VFUNI.GT.2)CALL SUBERROR('ERROR: ONLY VFUN=0,1,2 CURRENTLY SUPPORTED',IFATAL,1)

RETURN
END SUBROUTINE READVSTRESS

!**********************************************************************
SUBROUTINE READJMAX(UFILE, MODELJM, NOLAY, NOAGEP,                      &
                    NONDATES, DATESN, LEAFN,                            &
                    NOJDATES, DATESJ, JMAXTABLE,                        &
                    NOVDATES, DATESV, VCMAXTABLE,                       &
                    NOADATES, DATESA, AJQTABLE,                         &
                    IECOI, EAVJI, EDVJI, DELSJI, EAVCI, EDVCI, DELSCI,  &
                    TVJUPI, TVJDNI, THETAI)
! Read in all parameters to do with Jmax and Vcmax.
! If MODELJM = 1, use the leaf N contents to calculate Jmax and Vcmax;
! otherwise read in arrays directly.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER DATESN(maxdate), DATESJ(maxdate)
    INTEGER DATESV(maxdate), DATESA(maxdate)
    INTEGER UFILE,IECO,MODELJM,IOERROR,NOJDATES
    INTEGER NONDATES,NOVDATES,IDATE,ILAY,NOLAY,IAGE
    INTEGER NOAGEP,IECOI,NOADATES
    REAL LEAFN(maxdate,MAXLAY,MAXC)
    REAL JMAXTABLE(maxdate,MAXLAY,MAXC)
    REAL VCMAXTABLE(maxdate,MAXLAY,MAXC)
    REAL AJQTABLE(maxdate,MAXLAY,MAXC)
    REAL JMAXA,JMAXB,THETA,EAVJ,EDVJ,DELSJ,AJQ
    REAL EAVC,EDVC,DELSC,TVJUP,TVJDN,VCMAXA,VCMAXB
    REAL EAVCI,EDVCI,DELSCI,EAVJI,EDVJI,DELSJI,THETAI
    REAL TVJUPI,TVJDNI

    NAMELIST /JMAXPARS/ THETA,EAVJ,EDVJ,DELSJ,AJQ,IECO
    NAMELIST /VCMAXPARS/ EAVC,EDVC,DELSC,TVJUP,TVJDN
    NAMELIST /JMAXN/ JMAXA,JMAXB,VCMAXA,VCMAXB

    IF (MODELJM.EQ.1) THEN   ! Calculate Jmax, Vcmax from leaf N
        REWIND(UFILE)
        READ (UFILE,JMAXN,IOSTAT = IOERROR)
        IF (IOERROR.NE.0)CALL SUBERROR('ERROR: MISSING CONSTANTS FOR JMAX/N',IFATAL,IOERROR)
        NOJDATES = NONDATES
        NOVDATES = NONDATES
        DO IDATE = 1,NONDATES
            DATESJ(IDATE) = DATESN(IDATE)
            DATESV(IDATE) = DATESN(IDATE)
            DO ILAY = 1,NOLAY
                DO IAGE = 1,NOAGEP
                    JMAXTABLE(IDATE,ILAY,IAGE) = JMAXA*LEAFN(IDATE,ILAY,IAGE) + JMAXB
                    VCMAXTABLE(IDATE,ILAY,IAGE) = VCMAXA*LEAFN(IDATE,ILAY,IAGE) + VCMAXB
                END DO
            END DO
        END DO            
    ELSE ! Read in Jmax, Vcmax arrays
        CALL READPHYARRAY(UFILE,2,NOLAY,NOAGEP,NOJDATES,DATESJ,JMAXTABLE)
        CALL READPHYARRAY(UFILE,3,NOLAY,NOAGEP,NOVDATES,DATESV,VCMAXTABLE)  
    END IF

    ! Read in additional parameters
    REWIND (UFILE)
    AJQ = ALPHAQ !Default values
    EDVC = 0.0
    DELSC = 0.0
    IECO = 1 ! Ecocraft formulation of T-deps of Km and Gamma. For M
    TVJUP = -100.0
    TVJDN = -100.0

    READ (UFILE,JMAXPARS,IOSTAT = IOERROR)
    IF (IOERROR.NE.0) CALL SUBERROR('INPUT ERROR: MISSING JMAXPARS',IFATAL,IOERROR)
    REWIND (UFILE)
    READ (UFILE,VCMAXPARS,IOSTAT = IOERROR)
    IF (IOERROR.NE.0) CALL SUBERROR('INPUT ERROR: MISSING VCMAXPARS',IFATAL,IOERROR)

    EAVCI = EAVC
    EDVCI = EDVC
    DELSCI = DELSC
    EAVJI = EAVJ
    EDVJI = EDVJ
    DELSJI = DELSJ
    THETAI = THETA
    IECOI = IECO
    TVJUPI = TVJUP
    TVJDNI = TVJDN

    ! Read in quantum yield array
    NOADATES = 0
    CALL READPHYARRAY(UFILE,6,NOLAY,NOAGEP,NOADATES,DATESA,AJQTABLE)
    IF (NOADATES.EQ.0) THEN
        NOADATES = 1
        DO ILAY = 1,NOLAY
            DO IAGE = 1,NOAGEP
                AJQTABLE(1,ILAY,IAGE) = AJQ
            END DO
        END DO    
    END IF

    RETURN
END SUBROUTINE READJMAX


!**********************************************************************
SUBROUTINE READRD(UFILE, MODELRD, NOLAY, NOAGEP, NONDATES,      &
                    DATESN, LEAFN, NORDATES, DATESRD, RDTABLE,  &
                    NOFQDATES, DATESRFQ, Q10FTABLE, K10F,       &
                    RTEMPI, DAYRESPI, EFFYRFI, TBELOWI)
! Read in all parameters to do with leaf respiration rate.
! If MODELRD = 1, use the leaf N contents to calculate Rd0;
! otherwise read in array directly.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER DATESN(maxdate), DATESRD(maxdate), DATESRFQ(maxdate)
    INTEGER UFILE,MODELRD,NOLAY,NOAGEP,NONDATES,NOFQDATES
    INTEGER IOERROR,NORDATES,IDATE,ILAY,IAGE
    
    REAL LEAFN(maxdate,MAXLAY,MAXC)
    REAL RDTABLE(maxdate,MAXLAY,MAXC), Q10FTABLE(maxdate)
    REAL K10F,Q10F,RTEMP,TBELOW,DAYRESP,EFFYRF,RDA,RDB
    REAL K10FI,RTEMPI,DAYRESPI,EFFYRFI,TBELOWI

    NAMELIST /RDPARS/ Q10F, K10F, RTEMP, TBELOW, DAYRESP, EFFYRF
    NAMELIST /RDN/ RDA, RDB

    IF (MODELRD.EQ.1) THEN   ! Calculate Jmax, Vcmax from leaf N
        REWIND(UFILE)
        READ (UFILE,RDN,IOSTAT = IOERROR)
        IF (IOERROR.NE.0)CALL SUBERROR('ERROR: MISSING CONSTANTS FOR RD/N',IFATAL,IOERROR)
        NORDATES = NONDATES
        DO IDATE = 1,NONDATES
            DATESRD(IDATE) = DATESN(IDATE)
            DO ILAY = 1,NOLAY
                DO IAGE = 1,NOAGEP
                    RDTABLE(IDATE,ILAY,IAGE) = RDA*LEAFN(IDATE,ILAY,IAGE) + RDB
                END DO
            END DO
        END DO        
    ELSE ! Read in RD0 arrays
        CALL READPHYARRAY(UFILE,4,NOLAY,NOAGEP,NORDATES,DATESRD,RDTABLE)
    END IF

    DAYRESP = 1.0            ! Default value - no effect of light
    RTEMP = 0.0              ! Default value - Rd at zero degrees
    EFFYRF = 0.0             ! Default value - don't calc growth respn
    Q10F = 0.0               ! Indicates missing value
    K10F = 0.0               ! Zero acclimation
    TBELOW = -100.0          ! Default value - Rd continues at all Ts
    NOFQDATES = 1
    Q10FTABLE(1) = 0.0
    DATESRFQ(1) = 0

    ! Read in additional parameters
    REWIND (UFILE)
    READ (UFILE,RDPARS,IOSTAT = IOERROR)
    Q10FTABLE(1) = Q10F
    K10FI = K10F
    RTEMPI = RTEMP
    DAYRESPI = DAYRESP
    EFFYRFI = EFFYRF
    TBELOWI = TBELOW

    CALL READARRAY(UFILE,4,NOFQDATES,DATESRFQ,Q10FTABLE)

    IF (Q10FTABLE(1).EQ.0.0)CALL SUBERROR('INPUT ERROR: MISSING Q10F',IFATAL,IOERROR)

    RETURN
END SUBROUTINE READRD


!**********************************************************************
SUBROUTINE READRW(UFILE,MODELRW,EFFYRWI,RMWI,RTEMPWI,   & 
                    NOWQDATES, DATESRWQ, Q10WTABLE,     &
                    COLLA,COLLK,STEMSDWI,RMAI,STEMFORMI)
! Read in or calculate parameters to do with woody respiration rate.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE, DATESRWQ(maxdate), IOERROR, MODELRW
    INTEGER NOWQDATES
    
    REAL Q10WTABLE(maxdate)
    REAL COLLA, COLLK, STEMSDWI, RMAI, STEMFORMI
    REAL EFFY,RM,RMA,RTEMP,Q10W, STEMFORM,CA,CK
    REAL STEMSDW,RTEMPWI,EFFYRWI,RMWI
    
    NAMELIST /WRESP/ EFFY,RM,RMA,RTEMP,Q10W,STEMFORM
    NAMELIST /COLLWRESP/ CA,CK,STEMSDW
    
    EFFY = 0.0        ! Missing value - growth respn not calculated
    RM = 0.0          ! Missing value - maint respn not calculated
    RMA = 0.0         ! Missing value - maint respn not calculated
    RTEMP = 0.0       ! Default temp at which maint resp specified
    NOWQDATES = 1
    DATESRWQ(1) = 0
    Q10WTABLE(1) = 0.0 ! Missing value - will cause error if RM spec

    REWIND (UFILE)
    READ (UFILE,WRESP,IOSTAT = IOERROR)
    Q10WTABLE(1) = Q10W
    CALL READARRAY(UFILE,5,NOWQDATES,DATESRWQ,Q10WTABLE)

    !      IF (EFFY.EQ.0.0) CALL SUBERROR('WARNING: WOODY GROWTH RESP NOT CALCULATED',IWARN,IOERROR)
    IF ((MODELRW.NE.1).AND.RM.EQ.0.0.AND.RMA.EQ.0.0) THEN
    !        CALL SUBERROR('WARNING: WOODY MAINT RESP NOT CALCULATED',IWARN,IOERROR)
    ELSE IF (Q10WTABLE(1).EQ.0.0) THEN
        CALL SUBERROR('INPUT ERROR: MISSING Q10W',IFATAL,IOERROR)
    END IF

    RTEMPWI = RTEMP
    EFFYRWI = EFFY
    RMWI = RM
    RMAI = RMA
    STEMFORMI = STEMFORM

    IF (MODELRW.EQ.1) THEN ! Collelongo model
        REWIND(UFILE)
        READ(UFILE,COLLWRESP,IOSTAT = IOERROR)
    IF (IOERROR.NE.0) CALL SUBERROR('ERROR: MISSING INFO FOR COLLELONGO RW MODEL',IFATAL,IOERROR)
        COLLA = CA
        COLLK = CK
        STEMSDWI = STEMSDW
    END IF
    
    IF (RM.EQ.0.0.AND.RMA.GT.0.0) MODELRW = 2

    RETURN
END SUBROUTINE READRW


!**********************************************************************
SUBROUTINE READRR(UFILE,RMCRI,RMFRI,Q10RI,RTEMPRI)
! Read in or calculate parameters to do with root respiration rate.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE,IOERROR
    REAL RMCRI,RMCR,RMFRI,RMFR,Q10R,Q10RI
    REAL RTEMPR,RTEMPRI

    NAMELIST /RRESP/ RMCR,RMFR,Q10R,RTEMPR
    
    REWIND (UFILE)
    RMCR = 0.0        ! Missing value - maint respn not calculated
    RMFR = 0.0        ! Missing value - maint respn not calculated
    RTEMPR = 0.0      ! Default temp at which maint resp specified
    Q10R = 0.0        ! Missing value - will cause error if RM spec
    READ (UFILE,RRESP,IOSTAT = IOERROR)
    IF ((RMCR.EQ.0.0).AND.(RMFR.EQ.0.0)) THEN
        !        CALL SUBERROR('WARNING: ROOT MAINT RESP NOT CALCULATED',
        !     &  IWARN,IOERROR)
    ELSE IF (Q10R.EQ.0.0) THEN
        CALL SUBERROR('INPUT ERROR: MISSING Q10R',IFATAL,IOERROR)
    END IF

    Q10RI = Q10R
    RTEMPRI = RTEMPR
    RMCRI = RMCR
    RMFRI = RMFR

END  SUBROUTINE READRR


!**********************************************************************
SUBROUTINE READRB(UFILE,RMBI,Q10BI,RTEMPBI)
! Read in or calculate parameters to do with branch respiration rate.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE,IOERROR
    REAL RMBI,RMB,Q10BI,Q10B,RTEMPB,RTEMPBI
    
    NAMELIST /BRESP/ RMB,Q10B,RTEMPB

    REWIND (UFILE)
    RMB = 0.0        ! Missing value - maint respn not calculated
    RTEMPB = 0.0     ! Default temp at which maint resp specified
    Q10B = 0.0       ! Missing value - will cause error if RM spec
    READ (UFILE,BRESP,IOSTAT = IOERROR)
    IF (RMB.EQ.0.0) THEN
        !        CALL SUBERROR('WARNING: BRANCH MAINT RESP NOT CALCULATED',
        !     &  IWARN,IOERROR)
    ELSE IF (Q10B.EQ.0.0) THEN
        CALL SUBERROR('INPUT ERROR: MISSING Q10B',IFATAL,IOERROR)
    END IF

    Q10BI = Q10B
    RTEMPBI = RTEMPB
    RMBI = RMB

END SUBROUTINE READRB

!**********************************************************************
SUBROUTINE READARRAY(UFILE,INDEX,NDATESI,DATESI,RATES)
! Used to read in arrays of values specified by date only -
! only foliage and wood Q10's at present
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE,DATESI(maxdate),IOERROR,INDEX,NDATES,I,NDATESI
    REAL RATES(maxdate),RATESI(maxdate)
    CHARACTER(10) DATES(maxdate)
    INTEGER, EXTERNAL :: IDATE50

    NAMELIST /WOODRESP/NDATES, DATES, RATES
    NAMELIST /BRANRESP/NDATES, DATES, RATES
    NAMELIST /SOILRESP/NDATES, DATES, RATES
    NAMELIST /FOLQ10/NDATES, DATES, RATES
    NAMELIST /WOODQ10/NDATES, DATES, RATES
    NAMELIST /BRANQ10/NDATES, DATES, RATES
    NAMELIST /SOILQ10/NDATES, DATES, RATES

    REWIND(UFILE)
    IF (INDEX.EQ.1) THEN
        READ(UFILE, WOODRESP, IOSTAT = IOERROR)
    ELSE IF (INDEX.EQ.2) THEN
        READ(UFILE, BRANRESP, IOSTAT = IOERROR)
    ELSE IF (INDEX.EQ.3) THEN
        READ(UFILE, SOILRESP, IOSTAT = IOERROR)
    ELSE IF (INDEX.EQ.4) THEN
        READ(UFILE, FOLQ10, IOSTAT = IOERROR)
    ELSE IF (INDEX.EQ.5) THEN
        READ(UFILE, WOODQ10, IOSTAT = IOERROR)
    ELSE IF (INDEX.EQ.6) THEN
        READ(UFILE, BRANQ10, IOSTAT = IOERROR)
    ELSE IF (INDEX.EQ.7) THEN
        READ(UFILE, SOILQ10, IOSTAT = IOERROR)
    END IF

    IF (IOERROR.EQ.0) THEN
        NDATESI = NDATES
        DO I = 1,NDATES
            DATESI(I) = IDATE50(DATES(I))
            RATESI(I) = RATES(I)
        END DO
        DATESI(NDATES+1) = 100000 ! Limit Date
    END IF

    RETURN
END SUBROUTINE READARRAY


!!**********************************************************************
!! CURRENTLY UNUSED 3/6/98 BEM
!SUBROUTINE READRSOIL(UFILE,RSI,RTEMPSI,Q10SI)
!! Read in or calculate parameters to do with soil respiration rate.
!! Initially assuming Q10 relationship - to be replaced by Hanson et al 1993.
!!**********************************************************************
!
!    USE maestcom
!    IMPLICIT NONE
!    NAMELIST /SRESP/ RSOIL, RTEMPS, Q10S
!    INTEGER UFILE
!
!    REWIND (UFILE)
!    RSOIL = 0.0       ! Missing value - soil respn not calculated
!    RTEMPS = 0.0      ! Default temp at which soil resp specified
!    Q10S = 0.0        ! Missing value - will cause error if RS spec
!    READ (UFILE,SRESP,IOSTAT = IOERROR)
!    IF (RSOIL.EQ.0.0) THEN
!        CALL SUBERROR('WARNING: SOIL RESPIRATION NOT CALCULATED',IWARN,IOERROR)
!    ELSE IF (Q10S.EQ.0.0) THEN
!        CALL SUBERROR('INPUT ERROR: MISSING Q10S',IFATAL,IOERROR)
!    END IF
!
!    Q10SI = Q10S
!    RTEMPSI = RTEMPS
!    RSI = RSOIL
!
!    RETURN
!END SUBROUTINE READRSOIL

!**********************************************************************
SUBROUTINE PHYINTERP(IDATE,NODATES,IDATEARR,PARAMTABLE,NOLAY,NOAGEP,PARAMS)
! Interpolate physiological parameters for this date from the
! date arrays read in from physiology file.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER IDATEARR(maxdate),IOERROR,NODATES,IDATE
    INTEGER NOLAY,NOAGEP,IAGE,ILAY,INDEX
    REAL PARAMTABLE(maxdate,MAXLAY,MAXC)
    REAL PARAMS(MAXLAY,MAXC),SLOPE,Y1,Y2


    ! If only one date, or before the starting date, take first value
    IF ((NODATES.EQ.1).OR.IDATE.LE.IDATEARR(1)) THEN
        DO IAGE = 1,NOAGEP
            DO ILAY = 1,NOLAY
                PARAMS(ILAY,IAGE) = PARAMTABLE(1,ILAY,IAGE)
            END DO
        END DO        

    ! If after the final date, take last value
    ELSE IF (IDATE.GT.IDATEARR(NODATES)) THEN
        DO IAGE = 1,NOAGEP
            DO ILAY = 1,NOLAY
                PARAMS(ILAY,IAGE) = PARAMTABLE(NODATES,ILAY,IAGE)
            END DO
        END DO            

    ! Otherwise have to interpolate
    ELSE
        INDEX = 1
        DO WHILE (IDATE.GT.IDATEARR(INDEX))
            INDEX = INDEX + 1
        END DO
        SLOPE = REAL((IDATE - IDATEARR(INDEX-1)))/ REAL((IDATEARR(INDEX) - IDATEARR(INDEX-1)))
        DO IAGE = 1,NOAGEP
            DO ILAY = 1,NOLAY
                Y1 = PARAMTABLE(INDEX-1, ILAY, IAGE)
                Y2 = PARAMTABLE(INDEX, ILAY, IAGE)
                PARAMS(ILAY,IAGE) = Y1 + SLOPE*(Y2 - Y1)
            END DO
        END DO            
    END IF
    RETURN
END SUBROUTINE PHYINTERP

!**********************************************************************
SUBROUTINE PHYINTERP2(IDATE,NODATES,IDATEARR,PARAMTABLE,PARAM)
! Same as PHYINTERP but for parameter which is not specified by age & layer
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER IDATEARR(maxdate),IDATE,NODATES,INDEX
    REAL PARAMTABLE(maxdate),PARAM,SLOPE,Y1,Y2

    ! If only one date, or before the starting date, take first value
    IF ((NODATES.EQ.1).OR.IDATE.LE.IDATEARR(1)) THEN
        PARAM = PARAMTABLE(1)
    ! If after the final date, take last value
    ELSE IF (IDATE.GT.IDATEARR(NODATES)) THEN
        PARAM = PARAMTABLE(NODATES)
    ! Otherwise have to interpolate
    ELSE
        INDEX = 1
        DO WHILE (IDATE.GT.IDATEARR(INDEX))
            INDEX = INDEX + 1
        END DO
        SLOPE = REAL((IDATE - IDATEARR(INDEX-1)))/ REAL((IDATEARR(INDEX) - IDATEARR(INDEX-1)))
        Y1 = PARAMTABLE(INDEX-1)
        Y2 = PARAMTABLE(INDEX)
        PARAM = Y1 + SLOPE*(Y2 - Y1)
    END IF

    RETURN
END SUBROUTINE PHYINTERP2



!**********************************************************************
REAL FUNCTION RINTEG(D1,D2,X,VALUES,IOFFSET,NOLAY)
! Estimate physiological parameters for each canopy layer by averaging.
! At the moment, this is done by integrating the physiological parameter
! over the height of the layer, assuming foliage is evenly distributed.
! Strictly speaking, should probably take the beta-distribution of
! foliage into account - but since the specification of physiology for
! different layers is woolly anyway, this is a good approx.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER IP,IOFFSET,NOLAY
    REAL X(MAXLAY+1),VALUES(MAXLAY*MAXC*maxdate)
    REAL D1,D2
    
    ! NB The array VALUES can be of variable size - since this routine is used
    ! to handle reflectance and transmittance, and leaf N, Jmax, Vcmax and Rd
    ! arrays (as of 8/97). Here it should be set to the maximum size of
    ! any array to be passed.
    
    RINTEG = 0.0
    IP = 1

    DO WHILE (X(IP+1).LT.D1)
        IP = IP+1
    END DO

    DO WHILE (D2.GT.X(IP+1))
        RINTEG = RINTEG + NOLAY* VALUES(IP+IOFFSET)*(X(IP+1)-AMAX1(D1,X(IP)))
        IP = IP+1
    END DO

    RINTEG = RINTEG + NOLAY* VALUES(IP+IOFFSET)*(D2-AMAX1(D1,X(IP)))
    
    ! mgdk, no idea what this format statment is here for I assumed a relict
    !100 FORMAT(4(1X,F8.3))

    RETURN
END FUNCTION RINTEG


!**********************************************************************
SUBROUTINE READCONTREES(UFILE,NOALLTREES,DX,DY,XMAX,YMAX,&
                        NOTREESI,NOTARGETSI,ITARGETSI,IPLOTSHAPE,WEIGHTSI)
! Read in controls about tree numbers: need
! (1) no of trees to be included in the shading calculations
! (2) the numbers of the target trees.
! (1) NOTREES is the number of trees to be considered in the calculation.
! (Default: NOTREES = all trees in plot).
! (2) ITARGETS is an array with the numbers of the target trees.
! Just one can be specified with NOTARGET.
! If NORANDOM is defined, then calculations are done for NORANDOM
! randomly-chosed target trees.
! If none of NORANDOM, NOTARGET and ITARGETS is defined, calculations
! are done for all trees.
! Trees within EDGEDIST m of the plot edges are exempted.
!**********************************************************************

!    USE IFPORT    ! For Intel Visual Fortran - allows use of random function
    USE maestcom
    IMPLICIT NONE
    INTEGER ITARGETS(MAXT),ITARGETSI(MAXT)
    INTEGER UFILE,NOALLTREES,IPLOTSHAPE,NOTREES,NOTARGET,NORANDOM
    INTEGER IOERROR,IRAN,ITREE,NOTARGETS,ITAR,NOTREESI,NOTARGETSI
    INTEGER(4) IFLAG
    INTEGER, EXTERNAL :: INEDGES
    REAL(4) RANVAL
    REAL DX(MAXT),DY(MAXT),WEIGHTSI(MAXT),WEIGHTS(MAXT)
    REAL XMAX,YMAX,EDGEDIST
    NAMELIST /TREESCON/ NOTREES,NOTARGET,ITARGETS,WEIGHTS,NORANDOM,EDGEDIST

    ! Default values
    NOTREES = 0
    NOTARGET = 0
    NORANDOM = 0
    EDGEDIST = 0
    ITARGETS = 0    ! ARRAY
    WEIGHTS = 0     ! ARRAY

    ! Read file
    REWIND (UFILE)
    READ (UFILE,TREESCON,IOSTAT = IOERROR)

    ! Check the namelist was there
    IF (IOERROR.NE.0) CALL SUBERROR('INPUT ERROR: MISSING TREES IN CONTROL FILE',IFATAL,IOERROR)

    ! Set number of trees used in calculations
    IF (NOTREES.EQ.0.OR.NOTREES.GT.NOALLTREES) NOTREES = NOALLTREES
    IF (NOTREES.GT.MAXT) THEN
        CALL SUBERROR('WARNING: NOTREES IN CONTROL FILE EXCEEDED MAXIMUM',IWARN,0)
        NOTREES = MAXT
    END IF

    ! Set up the array of target tree numbers ITARGETS
    ! Case 1: One target tree specified
    IF (NOTARGET.GT.0) THEN
        IF (NOTARGET.GT.NOALLTREES)CALL SUBERROR('INCORRECT TARGET TREE NUMBER', IFATAL,0)
        NOTARGETS = 1
        ITARGETS(1) = NOTARGET

    ! Case 2: Trees to be chosen randomly
    ELSE IF (NORANDOM.GT.0) THEN
        IFLAG = 0
        IF (NORANDOM.GT.NOALLTREES) CALL SUBERROR('TOO MANY TARGET TREES SPECIFIED', IFATAL,0)
        NOTARGETS = NORANDOM
        DO IRAN = 1,NORANDOM
30          CALL RANDOM_NUMBER(RANVAL)
            RANVAL = RANVAL*REAL(NOALLTREES+1)
            ITREE = NINT(RANVAL)
            IF (ITREE.EQ.0) ITREE = 1
            IF (INEDGES(DX(ITREE),DY(ITREE),XMAX,YMAX,EDGEDIST).LT.0) GOTO 30
            ITARGETS(IRAN) = ITREE
        END DO
    ! Case 3: All trees are to be used, except those in the edges
    ELSE IF (ITARGETS(1).EQ.0) THEN
        NOTARGETS = NOALLTREES
        DO ITAR = 1,NOALLTREES
            IF (INEDGES(DX(ITAR),DY(ITAR),XMAX,YMAX,EDGEDIST).LT.0.AND.IPLOTSHAPE.EQ.0) THEN
                NOTARGETS = NOTARGETS - 1
            ELSE
                ITARGETS(ITAR + NOTARGETS - NOALLTREES) = ITAR
            END IF
        END DO
    ELSE
    ! Case 4: A series of target trees is given.
        NOTARGETS = 0
        DO WHILE (ITARGETS(NOTARGETS+1).GT.0)
            NOTARGETS = NOTARGETS + 1
        END DO
    END IF

    NOTREESI = NOTREES
    NOTARGETSI = NOTARGETS
    DO ITAR = 1,MAXT
        ITARGETSI(ITAR) = ITARGETS(ITAR)
    END DO
    WEIGHTSI = WEIGHTS

    RETURN
END SUBROUTINE READCONTREES


!**********************************************************************
 SUBROUTINE READXYZ(UFILE,NOALLTREES,X0,Y0,XMAX,YMAX,XSLOPE,YSLOPE,DX,DY,DZ)
! Read in the X, Y co-ordinates of each crown.
! Calculate the Z co-ordinates of each crown from slope.
! If they are not in the file, then the co-ordinates should be
! calculated from the stocking and the size of the plot.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE,NOALLTREES,IOERROR,I
    REAL DX(MAXT),DY(MAXT),DZ(MAXT),XYCOORDS(MAXT*2)
    REAL X0,Y0,XMAX,YMAX,XSLOPE,YSLOPE,SPACING,NOX,ZADD
    NAMELIST /XY/ XYCOORDS
    
    REWIND (UFILE)
    READ (UFILE,XY,IOSTAT = IOERROR)

    IF (IOERROR.NE.0) THEN
        ! Must calculate x, y co-ordinates
        CALL SUBERROR('WARNING: CALCULATING X, Y CO-ORDINATES', IWARN,IOERROR)
        SPACING = SQRT(XMAX*YMAX/NOALLTREES)
        NOX = XMAX/SPACING + 1
        DO I = 1,NOALLTREES
            DX(I) = (REAL(I)-1)/NOX * SPACING
            DY(I) = MOD(REAL(I)-1,NOX) * SPACING
        END DO
    ELSE
        ! Read in x, y co-ordinates
        DO I = 1,NOALLTREES
            DX(I) = XYCOORDS(2*I - 1) - X0
            DY(I) = XYCOORDS(2*I) - Y0
        END DO
    END IF

    ! Move x0,y0 to origin
    XMAX = XMAX - X0
    YMAX = YMAX - Y0
    X0 = 0.0
    Y0 = 0.0

    ! Calculate the z co-ordinates from slopes
    ZADD=0.0
    IF (XSLOPE.LT.0.0) ZADD=XMAX*SIN(XSLOPE)
    IF (YSLOPE.LT.0.0) ZADD=ZADD+YMAX*SIN(YSLOPE)
    ZADD=ABS(ZADD)
    
    !  X and Y distances are measured on the slope so the height is based
    !  on the sin(slope).
    DO I = 1,NOALLTREES
        DZ(I) = DX(I)*SIN(XSLOPE) + DY(I)*SIN(YSLOPE) + ZADD
    END DO

    RETURN
END SUBROUTINE READXYZ


!**********************************************************************
SUBROUTINE GETLEAFAREA(UFILE,IFLUSH,DT1I,DT2I,DT3I,DT4I, &
                        EXPTIMEI,APP,EXPAN,NOALLTREES,NOLADATES,DATESLA,FLT)
! A subroutine to read in leaf area array.
! First tries to calculate it from phenology (Wang, Rey & Jarvis 1998).
! Otherwise reads in array directly.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE,MAXLEAVES,IFLUSH,IOERROR,NOLADATES,NOALLTREES
    INTEGER, EXTERNAL :: IDATE50
    INTEGER DATESLA(maxdate)
    CHARACTER(10) FLUSHDATE
    REAL FLT(maxdate,MAXT)
    REAL DT1,DT1I,DT2,DT2I,DT3,DT3I,DT4,DT4I
    REAL EXPTIME,SIZELEAF,EXPTIMEI,APP,EXPAN
    
    NAMELIST /PHENOLOGY/ FLUSHDATE,DT1,DT2,DT3,DT4,EXPTIME,MAXLEAVES,SIZELEAF

    REWIND(UFILE)
    IFLUSH = 0
    READ(UFILE,PHENOLOGY,IOSTAT=IOERROR)
    IF (IOERROR.EQ.0) THEN
        CALL SUBERROR('LEAF AREA FROM PHENOLOGY:',IWARN,0)
        NOLADATES = 0
        IFLUSH = IDATE50(FLUSHDATE)
        DT1I = DT1
        DT2I = DT2
        DT3I = DT3
        DT4I = DT4
        EXPTIMEI = EXPTIME
        ! Rate of leaf appearance (leaf per day)
        APP = 2. * REAL(MAXLEAVES) / (DT1*(2.0*DT2-DT1))
        ! Rate of leaf area expansion (m2 leaf per day)
        EXPAN = SIZELEAF / EXPTIME
    ELSE
        CALL READTREEARRAY(UFILE,5,NOALLTREES,NOLADATES,DATESLA,FLT)
    END IF
    RETURN
END SUBROUTINE GETLEAFAREA

!**********************************************************************
SUBROUTINE PHENOL(IDAY,ISTART,IFLUSH,DT1,DT2,DT3,DT4,                       &
                    EXPTIME,APP,EXPAN,STOCKING,NOTREES,THRESH_FOLT,FOLT,    &
                    TOTLAI,NEWCANOPY)
! Implements phenology routine of Wang et al (1998) GCB to appear.
! Parameters:
!   IFLUSH = date of flushing (in days-since-1950 format)
!   DT1 = time from flushing to end of first flush (d)
!   DT2 = time from flushing to end of second flush (d)
!   DT3 = time from flushing to beginning of senescencs (d)
!   DT4 = time from flushing to leaf fall (d)
!   EXPTIME = time a leaf takes to expand (d)
!   APP = rate of leaf appearance (leaves / day)
!   EXPAN = rate of leaf expansion (m2 leaf / day)
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER I,IDAY,ISTART,IFLUSH,NEWCANOPY,ITREE,NOTREES
    REAL FOLT(MAXT)
    REAL EXPTIME, APP,EXPAN
    REAL DT1,DT2,DT3,DT4,TOTLAI,STOCKING
    REAL THRESH_FOLT

    ! Apply 7-part formula in Wang et al 1998
    I = IDAY + ISTART - IFLUSH
    IF (I.LE.0) THEN
         FOLT(1) = 0.0
    ELSE IF (I.LE.EXPTIME) THEN
        FOLT(1) = APP*EXPAN*(I**3)/6.
        NEWCANOPY = 1
    ELSE IF (I.LE.DT1) THEN
        FOLT(1) = APP*EXPAN*(I**3 - (I - EXPTIME)**3)/6.
        NEWCANOPY = 1
    ELSE IF (I.LE.(DT1+EXPTIME)) THEN
        FOLT(1) = APP*EXPAN*(DT1*(3.*I**2 + DT1**2 - 3.*DT1*I) -(I - EXPTIME)**3)/6.
        NEWCANOPY = 1
    ELSE IF (I.LE.DT2) THEN
        FOLT(1) = APP*EXPAN*DT1*EXPTIME*(I-0.5*(DT1+EXPTIME))
        NEWCANOPY = 1
    ELSE IF (I.LE.(DT2+EXPTIME)) THEN
        FOLT(1) = 0.5*APP*EXPAN*DT1*(2.*DT2*I-DT2**2-DT1*EXPTIME - (EXPTIME - I)**2)
        NEWCANOPY = 1
    ELSE IF (I.LE.DT3) THEN
        FOLT(1) = APP*EXPAN*DT1*EXPTIME*(DT2-0.5*DT1)
    ELSE IF (I.LE.DT4) THEN
        FOLT(1) = (DT4-I)*(DT2-0.5*DT1)*APP*EXPAN*DT1*EXPTIME / (DT4 - DT3)
        NEWCANOPY = 1
    ELSE
        FOLT(1) = 0.0
    END IF

    DO ITREE = 2,NOTREES
        FOLT(ITREE) = FOLT(ITREE)
    END DO
    TOTLAI = FOLT(1)*STOCKING

    RETURN
END SUBROUTINE PHENOL

!**********************************************************************
SUBROUTINE READTREEARRAY(UFILE,NARRAY,NOALLTREES,NDATE,IDATES,VALUESI)
! Read in an array of tree parameters from UFILE.
! NARRAY is the number of the array to be read (1 = RADX; 2 = RADY;
! 3 = HTCROWN; 4 = HTTRUNK; 5 = AREALEAF; 6 = DIAM; 7 = LEAFN (for understorey))
! Either read in values for all trees (NOALLTREES, up to MAXT trees) or
! read in average values. All values can be given for a series of dates.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER IDATES(maxdate)
    INTEGER UFILE, I, IDATE, ITREE,IOERROR,INDEX,ITREES,NDATE
    INTEGER NODATES,NARRAY,NOALLTREES
    INTEGER, EXTERNAL :: IDATE50
    CHARACTER(8) DATES(maxdate)
    REAL VALUES(maxdate*MAXT),VALUESI(maxdate,MAXT)

    NAMELIST /INDIVRADX/ NODATES, DATES, VALUES
    NAMELIST /ALLRADX/ NODATES, DATES, VALUES
    NAMELIST /INDIVRADY/ NODATES, DATES, VALUES
    NAMELIST /ALLRADY/ NODATES, DATES, VALUES
    NAMELIST /INDIVHTCROWN/ NODATES, DATES, VALUES
    NAMELIST /ALLHTCROWN/ NODATES, DATES, VALUES
    NAMELIST /INDIVHTTRUNK/ NODATES, DATES, VALUES
    NAMELIST /ALLHTTRUNK/ NODATES, DATES, VALUES
    NAMELIST /INDIVLAREA/ NODATES, DATES, VALUES
    NAMELIST /ALLLAREA/ NODATES, DATES, VALUES
    NAMELIST /INDIVDIAM/ NODATES, DATES, VALUES
    NAMELIST /ALLDIAM/ NODATES, DATES, VALUES
    NAMELIST /INDIVFOLN/ NODATES, DATES, VALUES
    NAMELIST /ALLFOLN/ NODATES, DATES, VALUES
    NAMELIST /INDIVUSLAI/ NODATES, DATES, VALUES ! not implemented
    NAMELIST /ALLUSLAI/ NODATES,DATES,VALUES

    ! Default values
    NODATES = 0
    DATES(1) = '01/01/50'  ! If only one date, doesn't matter what it
    DO I = 1,maxdate*MAXT
        VALUES(I) = -1.0
    END DO

    ! Try to read arrays for individual trees first
    REWIND(UFILE)
    IF (NARRAY.EQ.1) THEN
        READ(UFILE,INDIVRADX,IOSTAT=IOERROR)
        !CALL SUBERROR('X RADII ARRAY:',IWARN,0)
    ELSE IF (NARRAY.EQ.2) THEN
        READ(UFILE,INDIVRADY,IOSTAT=IOERROR)
        !CALL SUBERROR('Y RADII ARRAY:',IWARN,0)
    ELSE IF (NARRAY.EQ.3) THEN
        READ(UFILE,INDIVHTCROWN,IOSTAT=IOERROR)
        !CALL SUBERROR('CROWN HEIGHT ARRAY:',IWARN,0)
    ELSE IF (NARRAY.EQ.4) THEN
        READ(UFILE,INDIVHTTRUNK,IOSTAT=IOERROR)
        !CALL SUBERROR('TRUNK HEIGHT ARRAY:',IWARN,0)
    ELSE IF (NARRAY.EQ.5) THEN
        READ(UFILE,INDIVLAREA,IOSTAT=IOERROR)
        !CALL SUBERROR('LEAF AREA ARRAY:',IWARN,0)
    ELSE IF (NARRAY.EQ.6) THEN
        READ(UFILE,INDIVDIAM,IOSTAT=IOERROR)
        !CALL SUBERROR('DIAMETER ARRAY:',IWARN,0)
    ELSE IF (NARRAY.EQ.7) THEN
        READ(UFILE,INDIVFOLN,IOSTAT=IOERROR)
        !CALL SUBERROR('UNDERSTOREY N ARRAY:',IWARN,0)
    ELSE IF (NARRAY.EQ.8) THEN
        READ(UFILE,INDIVUSLAI,IOSTAT=IOERROR)
        !CALL SUBERROR('UNDERSTOREY LAI ARRAY:',IWARN,0)
    END IF

    IF (IOERROR.NE.-1) THEN
        ! Process arrays, if read in
        IF (NODATES.GT.maxdate) THEN
            CALL SUBERROR('WARNING: TOO MANY DATES: SOME DATA LOST',IWARN,IOERROR)
            NODATES = maxdate
        END IF
        INDEX = 1
        DO IDATE = 1,NODATES
            IDATES(IDATE) = IDATE50(DATES(IDATE))
        END DO
        DO ITREE = 1,NOALLTREES
            DO IDATE = 1,NODATES
                IF (VALUES(INDEX).LT.0) CALL SUBERROR('MISSING DATA, OR ONE OF MAXT, MAXDATE TOO SMALL.',IFATAL,0)
                VALUESI(IDATE,ITREE) = VALUES(INDEX)
                INDEX = INDEX+1
            END DO
        END DO
    ELSE
        ! Read in values for all trees
        REWIND(UFILE)
        IF (NARRAY.EQ.1) THEN
            READ(UFILE,ALLRADX,IOSTAT=IOERROR)
        ELSE IF (NARRAY.EQ.2) THEN
            READ(UFILE,ALLRADY,IOSTAT=IOERROR)
        ELSE IF (NARRAY.EQ.3) THEN
            READ(UFILE,ALLHTCROWN,IOSTAT=IOERROR)
        ELSE IF (NARRAY.EQ.4) THEN
            READ(UFILE,ALLHTTRUNK,IOSTAT=IOERROR)
        ELSE IF (NARRAY.EQ.5) THEN
            READ(UFILE,ALLLAREA,IOSTAT=IOERROR)
        ELSE IF (NARRAY.EQ.6) THEN
            READ(UFILE,ALLDIAM,IOSTAT=IOERROR)
        ELSE IF (NARRAY.EQ.7) THEN
            READ(UFILE,ALLFOLN,IOSTAT=IOERROR)
        ELSE IF (NARRAY.EQ.8) THEN
            READ(UFILE,ALLUSLAI,IOSTAT=IOERROR)
        END IF
        ! Missing diam, leaf N i 
        IF ((IOERROR.NE.0).AND.(NARRAY.LT.6))CALL SUBERROR('ERROR: MISSING DATA',IFATAL,IOERROR)
        IF (NODATES.GT.maxdate) THEN
            CALL SUBERROR('WARNING: TOO MANY DATES: SOME DATA LOST',IWARN,IOERROR)
            NODATES = maxdate
        END IF
        ! Assign arrays to data
        DO IDATE = 1,NODATES
            IDATES(IDATE) = IDATE50(DATES(IDATE))
        END DO
        DO ITREES = 1,NOALLTREES
            DO IDATE = 1,NODATES
                VALUESI(IDATE,ITREES) = VALUES(IDATE)
            END DO
        END DO    
    END IF
    
    NDATE = NODATES
    RETURN
END SUBROUTINE READTREEARRAY


!**********************************************************************
SUBROUTINE CALCLAI(NOLADATES,FLT,NOALLTREES,XMAX,YMAX,XSLOPE,YSLOPE,TOTLAITABLE)
! Calculate total LAI of the plot - based on horizontal plot area
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER IDATE, ITREE
    INTEGER NOLADATES,NOALLTREES
    REAL FLT(maxdate,MAXT),TOTLAITABLE(maxdate)
    REAL XMAX,YMAX,XSLOPE,YSLOPE,GROUNDAREA

    GROUNDAREA = XMAX*COS(XSLOPE)*YMAX*COS(YSLOPE)
    DO IDATE = 1,NOLADATES
        TOTLAITABLE(IDATE) = 0.0
        DO ITREE = 1,NOALLTREES
            TOTLAITABLE(IDATE) = TOTLAITABLE(IDATE) + FLT(IDATE,ITREE)
        END DO
        TOTLAITABLE(IDATE) = TOTLAITABLE(IDATE)/GROUNDAREA
    END DO
    RETURN
END SUBROUTINE CALCLAI


!**********************************************************************
SUBROUTINE TREEINTERP(IDAY,ISTART,NODATES,IDATEARR,PARAMTABLE,NOTREES,PARAMS)
! Interpolate crown dimensions for this date from the
! date arrays read in from the trees file.
! Parameter NEWCANOPY indicates whether crown has changed - in which
! case grid points need to be reassigned.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER ITREE, INDEX, NOTREES,IDATE,IDAY,ISTART
    INTEGER IDATEARR(maxdate),NODATES
    
    REAL PARAMTABLE(maxdate,MAXT)
    REAL PARAMS(MAXT),SLOPE,Y1,Y2
    
    IDATE = IDAY + ISTART

    ! If no dates, take 0.0
    IF (NODATES.EQ.0) THEN
        DO ITREE = 1,NOTREES
            PARAMS(ITREE) = 0.0
        END DO
    ! If only one date, or before the starting date, take first value
    ELSE IF ((NODATES.EQ.1).OR.IDATE.LE.IDATEARR(1)) THEN
        DO ITREE = 1,NOTREES
            PARAMS(ITREE) = PARAMTABLE(1,ITREE)
        END DO
    ! If after the final date, take last value
    ELSE IF (IDATE.GT.IDATEARR(NODATES)) THEN
        DO ITREE = 1,NOTREES
            PARAMS(ITREE) = PARAMTABLE(NODATES,ITREE)
        END DO
    ! Otherwise have to interpolate
    ELSE
        INDEX = 1
        DO WHILE (IDATE.GT.IDATEARR(INDEX))
            INDEX = INDEX + 1
        END DO
        SLOPE = REAL((IDATE - IDATEARR(INDEX-1)))/ REAL((IDATEARR(INDEX) - IDATEARR(INDEX-1)))
        DO ITREE = 1,NOTREES
            Y1 = PARAMTABLE(INDEX-1, ITREE)
            Y2 = PARAMTABLE(INDEX, ITREE)
            PARAMS(ITREE) = Y1 + SLOPE*(Y2 - Y1)
        END DO
    END IF
    RETURN
END SUBROUTINE TREEINTERP


!**********************************************************************
SUBROUTINE TREEINTERP2(IDAY,ISTART,NODATES,IDATEARR,PARAMTABLE,NOTREES,PARAMS)
! Like TREEINTERP, but for arrays that have dimension (maxdate), not
! (maxdate,MAXT), like most of them. 
! Addition to fix TOTLAI problems, Nov2010 (RAD).
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER IDATEARR(maxdate),NODATES
    REAL PARAMTABLE(maxdate)
    REAL PARAMS,SLOPE,Y1,Y2
    INTEGER ITREE, INDEX, NOTREES,IDATE,IDAY,ISTART

    IDATE = IDAY + ISTART

    ! If no dates, take 0.0
    IF (NODATES.EQ.0) THEN
        PARAMS = 0.0
        
    ! If only one date, or before the starting date, take first value
    ELSE IF ((NODATES.EQ.1).OR.IDATE.LE.IDATEARR(1)) THEN
        PARAMS = PARAMTABLE(1)

    ! If after the final date, take last value
    ELSE IF (IDATE.GT.IDATEARR(NODATES)) THEN
            PARAMS = PARAMTABLE(NODATES)
    
    ! Otherwise have to interpolate
    ELSE
        INDEX = 1
        DO WHILE (IDATE.GT.IDATEARR(INDEX))
            INDEX = INDEX + 1
        END DO
        SLOPE = REAL((IDATE - IDATEARR(INDEX-1)))/ REAL((IDATEARR(INDEX) - IDATEARR(INDEX-1)))
            Y1 = PARAMTABLE(INDEX-1)
            Y2 = PARAMTABLE(INDEX)
            PARAMS = Y1 + SLOPE*(Y2 - Y1)
    END IF
    RETURN
END SUBROUTINE TREEINTERP2


!**********************************************************************
SUBROUTINE INTERPOLATEP(IDAY, ISTART,                           &
                        NOJDATES,DATESJ,JMAXTABLE,              &
                        NOVDATES,DATESV,VCMAXTABLE,             &
                        NORDATES,DATESRD,RDTABLE,               &
                        NOSLADATES,DATESSLA,SLATABLE,           &
                        NOADATES,DATESA,AJQTABLE,               &
                        NOFQDATES,DATESFQ,Q10FTABLE,            &
                        NOWQDATES,DATESWQ,Q10WTABLE,            &
                        NOLAY,NOAGEP,                           &
                        JMAX25,VCMAX25,RD0,SLA,AJQ,Q10F,Q10W,   &
                        NOGSDATES,DATESGS,G0TABLE,G1TABLE,G0,G1,NOWLEAFDATES,DATESWLEAF,WLEAFTABLE,WLEAF)
! Controls the calling of the interpolation routines to get daily values
! of Jmax, Vcmax, SLA, Rd.
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER DATESJ(maxdate), DATESV(maxdate)
    INTEGER DATESRD(maxdate), DATESSLA(maxdate)
    INTEGER DATESA(maxdate), DATESGS(maxdate),DATESWLEAF(maxdate)
    INTEGER DATESFQ(maxdate),DATESWQ(maxdate)
    INTEGER IDAY,ISTART,NOJDATES,NOLAY,NOAGEP
    INTEGER NOVDATES,NORDATES,NOSLADATES,NOADATES,NOGSDATES,NOWLEAFDATES
    INTEGER NOFQDATES,NOWQDATES
    
    REAL JMAXTABLE(maxdate,MAXLAY,MAXC)
    REAL VCMAXTABLE(maxdate,MAXLAY,MAXC)
    REAL RDTABLE(maxdate,MAXLAY,MAXC)
    REAL SLATABLE(maxdate,MAXLAY,MAXC)
    REAL AJQTABLE(maxdate,MAXLAY,MAXC)
    REAL Q10FTABLE(maxdate),Q10WTABLE(maxdate)
    REAL G0TABLE(maxdate),G1TABLE(maxdate),WLEAFTABLE(maxdate)
    REAL JMAX25(MAXLAY,MAXC),VCMAX25(MAXLAY,MAXC)
    REAL RD0(MAXLAY,MAXC),SLA(MAXLAY,MAXC),AJQ(MAXLAY,MAXC)
    REAL G0,G1,Q10F,Q10W,WLEAF

    ! Interpolate to get daily values of physiological params
    CALL PHYINTERP(IDAY+ISTART,NOJDATES,DATESJ,JMAXTABLE,NOLAY,NOAGEP,JMAX25)
    CALL PHYINTERP(IDAY+ISTART,NOVDATES,DATESV,VCMAXTABLE,NOLAY,NOAGEP,VCMAX25)
    CALL PHYINTERP(IDAY+ISTART,NORDATES,DATESRD,RDTABLE, NOLAY,NOAGEP,RD0)
    IF (NOSLADATES.GT.0) CALL PHYINTERP(IDAY+ISTART,NOSLADATES,DATESSLA,SLATABLE, NOLAY,NOAGEP,SLA)
    CALL PHYINTERP(IDAY+ISTART,NOADATES,DATESA,AJQTABLE,NOLAY,NOAGEP,AJQ)
    CALL PHYINTERP2(IDAY+ISTART,NOGSDATES,DATESGS,G0TABLE,G0)
    CALL PHYINTERP2(IDAY+ISTART,NOGSDATES,DATESGS,G1TABLE,G1)
    CALL PHYINTERP2(IDAY+ISTART,NOWLEAFDATES,DATESWLEAF,WLEAFTABLE,WLEAF)
    CALL PHYINTERP2(IDAY+ISTART,NOFQDATES,DATESFQ,Q10FTABLE, Q10F)
    CALL PHYINTERP2(IDAY+ISTART,NOWQDATES,DATESWQ,Q10WTABLE,Q10W)

    RETURN
END SUBROUTINE INTERPOLATEP


!**********************************************************************
SUBROUTINE INTERPOLATET(IDAY, ISTART, IHOUR,                            &
                        NOXDATES,DATESX,RXTABLE,                        &
                        NOYDATES,DATESY,RYTABLE,                        &
                        NOZDATES,DATESZ,RZTABLE,                        &
                        NOTDATES,DATEST,ZBCTABLE,                       &
                        NODDATES,DATESD,DIAMTABLE,                      &
                        NOLADATES,DATESLA,FOLTABLE,TOTLAITABLE,NOTREES, &
                        RX,RY,RZ,ZBC,FOLT,TOTLAI,DIAM,STOCKING,         &
                        IFLUSH,DT1,DT2,DT3,DT4,EXPTIME,APP,EXPAN,       &
                        NEWCANOPY,CANOPYDIMS)

! RAD change
! Controls the calling of the interpolation routines to get daily values
! of crown heights and radii and leaf area.
! RXTABLE, RYTABLE, etc have all values of dimensions, for all trees and dates
! These are interpolated to give values for this date, for all trees, in RX, RY etc
!**********************************************************************

    USE maestcom
    IMPLICIT NONE
    INTEGER DATESX(maxdate),DATESY(maxdate),DATESZ(maxdate)
    INTEGER DATEST(maxdate),DATESLA(maxdate),DATESD(maxdate)
    INTEGER IDAY,ISTART,NOXDATES,NOTREES,NOYDATES,NOZDATES,NOTDATES
    INTEGER NOLADATES,IFLUSH,NEWCANOPY,NODDATES,IHOUR

    REAL RX(MAXT),RY(MAXT),RZ(MAXT),ZBC(MAXT),FOLT(MAXT)
    REAL RXTABLE(maxdate,MAXT),RYTABLE(maxdate,MAXT)
    REAL RZTABLE(maxdate,MAXT),ZBCTABLE(maxdate,MAXT)
    REAL FOLTABLE(maxdate,MAXT),TOTLAITABLE(maxdate)  !!!
    REAL DIAMTABLE(maxdate,MAXT),DIAM(MAXT), TOTLAI  !(MAXT)
    REAL CANOPYDIMS(6),DT1,DT2,DT3,DT4,EXPTIME,APP,EXPAN
    REAL STOCKING,THRESH_FOLT,THRESH_RX,THRESH_RY,THRESH_RZ
    REAL THRESH_ZBC,THRESH_LAI

    ! Interpolate to get daily values of crown dimensions
    CALL TREEINTERP(IDAY,ISTART,NOXDATES,DATESX,RXTABLE,NOTREES,RX)

    CALL TREEINTERP(IDAY,ISTART,NOYDATES,DATESY,RYTABLE,NOTREES,RY)

    CALL TREEINTERP(IDAY,ISTART,NOZDATES,DATESZ,RZTABLE,NOTREES,RZ)

    CALL TREEINTERP(IDAY,ISTART,NOTDATES,DATEST,ZBCTABLE,NOTREES,ZBC)

    IF (NOLADATES.GT.0) THEN
        CALL TREEINTERP(IDAY,ISTART,NOLADATES,DATESLA,FOLTABLE,NOTREES,FOLT)
        CALL TREEINTERP2(IDAY,ISTART,NOLADATES,DATESLA,TOTLAITABLE,1,TOTLAI)
    ELSE
        CALL PHENOL(IDAY,ISTART,IFLUSH,DT1,DT2,DT3,DT4,EXPTIME,APP,EXPAN,&
                    STOCKING,NOTREES,THRESH_FOLT,FOLT,TOTLAI,NEWCANOPY)
    END IF

    IF (NODDATES.GT.0) CALL TREEINTERP(IDAY,ISTART,NODDATES,DATESD,DIAMTABLE,NOTREES, DIAM)

    ! BM 11/07/07 Fixed bug: we were not re-calculating points unless the DAILY change in the canopy
    ! was large. That allowed small changes to turn into a big change, without POINTS being recalculated.
    ! Now, check the change in canopy size since POINTS were last calculated.
    ! Thresholds for changes are set here: could potentially set elsewhere to make them easier to change?
    NEWCANOPY = 0
    IF (IDAY.EQ.0.AND.IHOUR.EQ.1) THEN
        NEWCANOPY = 1
    ELSE
        THRESH_RX = RXTABLE(1,1)*0.01 ! 1% OF INITIAL CROWN RADIUS
        IF (RX(1)-CANOPYDIMS(1).GT.THRESH_RX) NEWCANOPY = 1
        THRESH_RY = RYTABLE(1,1)*0.01
        IF (RY(1)-CANOPYDIMS(2).GT.THRESH_RY) NEWCANOPY = 1
        THRESH_RZ = RZTABLE(1,1)*0.01
        IF (RZ(1)-CANOPYDIMS(3).GT.THRESH_RZ) NEWCANOPY = 1
        THRESH_ZBC = ZBCTABLE(1,1)*0.01 !
        IF (ZBC(1)-CANOPYDIMS(4).GT.THRESH_ZBC) NEWCANOPY = 1
        THRESH_FOLT = 0.05
        IF (FOLT(1)-CANOPYDIMS(5).GT.THRESH_FOLT) NEWCANOPY = 1
        IF (FOLT(1)-CANOPYDIMS(5).LT.-THRESH_FOLT) NEWCANOPY = 1
        THRESH_LAI = 0.1
        IF (TOTLAI-CANOPYDIMS(6).GT.THRESH_LAI) NEWCANOPY = 1
        IF (TOTLAI-CANOPYDIMS(6).LT.-THRESH_LAI) NEWCANOPY = 1
    END IF
    IF (NEWCANOPY.EQ.1) THEN
        CANOPYDIMS(1) = RX(1)
        CANOPYDIMS(2) = RY(1)
        CANOPYDIMS(3) = RZ(1)
        CANOPYDIMS(4) = ZBC(1)
        CANOPYDIMS(5) = FOLT(1)
        CANOPYDIMS(6) = TOTLAI
    END IF

    RETURN
                        END SUBROUTINE INTERPOLATET

!****************************************************************
SUBROUTINE INTERPOLATEW(IDAY,ISTART,NOKPDATES,DATESKP,PLANTKTABLE,PLANTK,   &
                        NOROOTDATES,DATESROOT,NOROOTSPEC,ROOTRADTABLE,ROOTDENSTABLE,ROOTMASSTOTTABLE, &
                        FRACROOTSPEC,LAYTHICK,ROOTRESFRAC,ROOTXSECAREA, ROOTLEN, ROOTRESIST,   &
                        ROOTMASS, NROOTLAYER, ROOTRAD)

    USE maestcom
    IMPLICIT NONE
    INTEGER DATESKP(maxdate),NOKPDATES,IDAY,ISTART,INDEX,IDATE,ISPEC
    INTEGER NOROOTDATES,DATESROOT(maxdate), NROOTLAYER,I,NOROOTSPEC
    REAL PLANTKTABLE(maxdate),PLANTK
    REAL ROOTRAD,ROOTDENS,ROOTMASSTOT,ROOTRADTABLE(maxdate),ROOTDENSTABLE(maxdate),ROOTMASSTOTTABLE(maxdate)
    REAL FRACROOTSPEC(maxsoillay,MAXSP),LAYTHICK(maxsoillay),ROOTRESFRAC
    REAL ROOTXSECAREA, ROOTLEN(maxsoillay,MAXSP),ROOTRESIST, ROOTMASS(maxsoillay,MAXSP)   
    
    IF (NOKPDATES.GT.1) THEN
        CALL WATINTERP(IDAY,ISTART,NOKPDATES,DATESKP,PLANTKTABLE,PLANTK)
    ELSE
        PLANTK = PLANTKTABLE(1)
    END IF
    IF (NOROOTDATES.GT.1) THEN
        CALL WATINTERP(IDAY,ISTART,NOROOTDATES,DATESROOT,ROOTRADTABLE,ROOTRAD)
        CALL WATINTERP(IDAY,ISTART,NOROOTDATES,DATESROOT,ROOTDENSTABLE,ROOTDENS)
        CALL WATINTERP(IDAY,ISTART,NOROOTDATES,DATESROOT,ROOTMASSTOTTABLE,ROOTMASSTOT)
    ELSE
        ROOTRAD = ROOTRADTABLE(1)
        ROOTDENS = ROOTDENSTABLE(1)
        ROOTMASSTOT = ROOTMASSTOTTABLE(1)
    END IF
    
    ! Root cross-sectional area (m2)
    ROOTXSECAREA = PI*ROOTRAD**2
         
    ! Prepare root mass and length arrays (from SPA, io.f90, RAD).
    ROOTLEN = 0.
    DO ISPEC=1, NOROOTSPEC
        DO I=1, NROOTLAYER
            ! g m-3 
            ROOTMASS(I,ISPEC) = FRACROOTSPEC(I,ISPEC) * ROOTMASSTOT / LAYTHICK(I)      
        
            ! m m-3 soil
            ROOTLEN(I,ISPEC) = ROOTMASS(I,ISPEC) / (ROOTDENS*ROOTXSECAREA)
        END DO
    END DO


    ! Get total root resistance from fraction resistance in roots (ROOTRESFRAC)
    ! and total plant conductance (which includes root conductance).
    ROOTRESIST = ROOTRESFRAC * (1./PLANTK)

    RETURN
END SUBROUTINE INTERPOLATEW

SUBROUTINE INTERPOLATEDIST(IDAY,ISTART,FRACROOTTABLE,NOROOTDATES,NOROOTSPEC,DATESROOT,FRACROOTSPEC,NROOTLAYER, &
                                NALPHASPEC,FALPHATABLESPEC,DATESLIA,NOLIADATES,FALPHASPEC,NSPECIES, &
                                ISMAESPA)

    USE maestcom
    IMPLICIT NONE
    INTEGER NSPECIES,I
    INTEGER IDAY,ISTART,NROOTLAYER,NOROOTDATES,NOROOTSPEC,DATESROOT(maxdate)
    REAL FRACROOTTABLE(maxsoillay,maxdate,MAXSP),FRACROOTSPEC(maxsoillay,MAXSP),FRACSUM
    INTEGER NALPHASPEC(maxsp), DATESLIA(maxdate,maxsp),NOLIADATES(maxsp)
    REAL FALPHATABLESPEC(maxang,maxdate,maxsp), FALPHASPEC(maxang, maxsp)
    LOGICAL ISMAESPA
    
    IF(ISMAESPA)THEN
        IF (NOROOTDATES.GT.1) THEN
            DO I = 1,NOROOTSPEC    
                CALL DISTINTERP(IDAY,ISTART,NOROOTDATES,NROOTLAYER,DATESROOT,   &
                FRACROOTTABLE(1:MAXSOILLAY,1:MAXDATE,I),FRACROOTSPEC(1:MAXSOILLAY,I))
            ENDDO
        
        ELSE
            FRACROOTSPEC = FRACROOTTABLE(1:MAXSOILLAY,1,1:MAXSP)
        ENDIF
        
        DO I = 1,NOROOTSPEC
            CALL FILLWITHLAST(FRACROOTSPEC(1:MAXSOILLAY,I), MAXSOILLAY, NROOTLAYER, -900.0)
        
            ! check if it sums to one
            FRACSUM = SUM(FRACROOTSPEC(1:MAXSOILLAY,I))
                 ! modification mathias décembre 2012
            FRACROOTSPEC(1:MAXSOILLAY,I) = FRACROOTSPEC(1:MAXSOILLAY,I) / FRACSUM
        ENDDO
    ENDIF
    
    DO I = 1,NSPECIES
        IF (NOLIADATES(I).GT.1) THEN
            CALL DISTINTERP2(IDAY,ISTART,NOLIADATES(I),NALPHASPEC(I),DATESLIA(1:maxdate,I), &
            FALPHATABLESPEC(1:maxang,1:maxdate,I), FALPHASPEC(1:maxang,I))
        ELSE
            FALPHASPEC(1:maxang,I) = FALPHATABLESPEC(1:maxang,1,I)
        ENDIF
    END DO
    
END SUBROUTINE INTERPOLATEDIST
                        
SUBROUTINE WATINTERP(IDAY,ISTART,NODATES,IDATEARR,PARAMTABLE,PARAMS)

    USE maestcom
    IMPLICIT NONE
    INTEGER INDEX, NOTREES,IDATE,IDAY,ISTART
    INTEGER IDATEARR(maxdate),NODATES

    REAL PARAMTABLE(maxdate)
    REAL PARAMS,SLOPE,Y1,Y2

    IDATE = IDAY + ISTART

    ! If no dates, take 0.0
    IF (NODATES.EQ.0) THEN
            PARAMS = 0.0
    ! If only one date, or before the starting date, take first value
    ELSE IF ((NODATES.EQ.1).OR.IDATE.LE.IDATEARR(1)) THEN
            PARAMS = PARAMTABLE(1)
    ! If after the final date, take last value
    ELSE IF (IDATE.GT.IDATEARR(NODATES)) THEN
            PARAMS = PARAMTABLE(NODATES)
    ! Otherwise have to interpolate
    ELSE
        INDEX = 1
        DO WHILE (IDATE.GT.IDATEARR(INDEX))
            INDEX = INDEX + 1
        END DO
        SLOPE = REAL((IDATE - IDATEARR(INDEX-1)))/ REAL((IDATEARR(INDEX) - IDATEARR(INDEX-1)))
            Y1 = PARAMTABLE(INDEX-1)
            Y2 = PARAMTABLE(INDEX)
            PARAMS = Y1 + SLOPE*(Y2 - Y1)
    END IF
    RETURN
END SUBROUTINE WATINTERP

SUBROUTINE  DISTINTERP(IDAY,ISTART,NODATES,NOMAX,IDATEARR,PARAMTABLE,PARAM)

    USE maestcom
    IMPLICIT NONE
    INTEGER INDEX, IDATE,IDAY,ISTART,I
    INTEGER IDATEARR(maxdate),NODATES, NOMAX

    REAL PARAMTABLE(maxsoillay,maxdate)
    REAL PARAM(maxsoillay),SLOPE,Y1,Y2

    IDATE = IDAY + ISTART

    ! If no dates, take 0.0
    IF (NODATES.EQ.0) THEN
            PARAM = 0.0
    ! If only one date, or before the starting date, take first value
    ELSE IF ((NODATES.EQ.1).OR.IDATE.LE.IDATEARR(1)) THEN
            PARAM = PARAMTABLE(1:maxsoillay,1)
    ! If after the final date, take last value
    ELSE IF (IDATE.GT.IDATEARR(NODATES)) THEN
            PARAM = PARAMTABLE(1:maxsoillay,NODATES)
    ! Otherwise have to interpolate
    ELSE
        INDEX = 1
        DO WHILE (IDATE.GT.IDATEARR(INDEX))
            INDEX = INDEX + 1
        END DO
        SLOPE = REAL((IDATE - IDATEARR(INDEX-1)))/ REAL((IDATEARR(INDEX) - IDATEARR(INDEX-1)))
            
            DO I = 1,NOMAX
                Y1 = PARAMTABLE(I,INDEX-1)
                Y2 = PARAMTABLE(I,INDEX)
            PARAM(I) = Y1 + SLOPE*(Y2 - Y1)
            END DO
    END IF
    RETURN


END SUBROUTINE DISTINTERP

SUBROUTINE  DISTINTERP2(IDAY,ISTART,NODATES,NOMAX,IDATEARR,PARAMTABLE,PARAM)

    USE maestcom
    IMPLICIT NONE
    INTEGER INDEX, IDATE,IDAY,ISTART,I
    INTEGER IDATEARR(maxdate),NODATES, NOMAX

    REAL PARAMTABLE(maxang,maxdate)
    REAL PARAM(maxang),SLOPE,Y1,Y2

    IDATE = IDAY + ISTART

    ! If no dates, take 0.0
    IF (NODATES.EQ.0) THEN
            PARAM = 0.0
    ! If only one date, or before the starting date, take first value
    ELSE IF ((NODATES.EQ.1).OR.IDATE.LE.IDATEARR(1)) THEN
            PARAM = PARAMTABLE(1:maxang,1)
    ! If after the final date, take last value
    ELSE IF (IDATE.GT.IDATEARR(NODATES)) THEN
            PARAM = PARAMTABLE(1:maxang,NODATES)
    ! Otherwise have to interpolate
    ELSE
        INDEX = 1
        DO WHILE (IDATE.GT.IDATEARR(INDEX))
            INDEX = INDEX + 1
        END DO
        SLOPE = REAL((IDATE - IDATEARR(INDEX-1)))/ REAL((IDATEARR(INDEX) - IDATEARR(INDEX-1)))
            
            DO I = 1,NOMAX
                Y1 = PARAMTABLE(I,INDEX-1)
                Y2 = PARAMTABLE(I,INDEX)
            PARAM(I) = Y1 + SLOPE*(Y2 - Y1)
            END DO
    END IF
    RETURN


END SUBROUTINE DISTINTERP2

SUBROUTINE LADCHOOSE(IDAY,ISTART,NSPECIES,NOLADDATES,DATESLAD,BPTTABLESPEC,BPTSPEC)

    USE maestcom
    IMPLICIT NONE
    INTEGER IDAY,ISTART,NOLADDATES(maxsp), DATESLAD(maxdate,maxsp),IDATE, IOERROR,I,J
    REAL BPTTABLESPEC(8,maxc,maxsp,maxdate),BPTSPEC(8,maxc,maxsp)
    INTEGER ISPEC,NSPECIES,NDATE
    
    IDATE = IDAY + ISTART

    DO ISPEC = 1,NSPECIES
        
        NDATE = NOLADDATES(ISPEC)
        
        IF (NDATE.EQ.1) THEN
            BPTSPEC(1:8,1:maxc,ISPEC) = BPTTABLESPEC(1:8,1:maxc,ISPEC,1)
        
        ELSE
            IF (IDATE.LT.DATESLAD(1,ISPEC)) THEN
                    BPTSPEC(1:8,1:maxc,ISPEC) = BPTTABLESPEC(1:8,1:maxc,ISPEC,1)
            
            ELSE IF (IDATE.GE.DATESLAD(NDATE,ISPEC)) THEN
                    BPTSPEC(1:8,1:maxc,ISPEC) = BPTTABLESPEC(1:8,1:maxc,ISPEC,NDATE)
        
            ELSE
                DO I =1,(NDATE-1)
                    IF (IDATE.LT.DATESLAD(I+1,ISPEC).AND.IDATE.GE.DATESLAD(I,ISPEC)) THEN
                            BPTSPEC(1:8,1:maxc,ISPEC) = BPTTABLESPEC(1:8,1:maxc,ISPEC,I)
                    END IF
                ENDDO        
            END IF
        END IF
        
    END DO
END SUBROUTINE LADCHOOSE

!**********************************************************************
SUBROUTINE OUTPUTLAY(UFILE,FOLLAY,JMAX25,VCMAX25,NOLAY)
! Daily output to layer flux file.
!**********************************************************************
    
    USE switches
    USE maestcom
    IMPLICIT NONE
    INTEGER UFILE,I,NOLAY
    REAL FOLLAY(MAXLAY)
    REAL JMAX25(MAXLAY,MAXC),VCMAX25(MAXLAY,MAXC)
    
    IF (IOFORMAT .EQ. 0) THEN
        WRITE(UFILE,*) 'LEAF AREA OF TARGET TREE IN EACH LAYER IN M2'
        WRITE(UFILE,500) (FOLLAY(I),I=1,NOLAY)
        WRITE(UFILE,*)
        WRITE(UFILE,*) 'JMAX (CURRENT) IN EACH LAYER IN UMOL M-2 S-1'
        WRITE(UFILE,500) (JMAX25(I,1),I=1,NOLAY)
        WRITE(UFILE,*)
        WRITE(UFILE,*) 'VCMAX (CURRENT) IN EACH LAYER IN UMOL M-2 S-1'
        WRITE(UFILE,500) (VCMAX25(I,1),I=1,NOLAY)
        WRITE(UFILE,*)
    
        500 FORMAT(10(F10.5,1X))
    ELSE IF (IOFORMAT .EQ. 1) THEN
    
    END IF
    
    RETURN
END SUBROUTINE OUTPUTLAY


!**********************************************************************
SUBROUTINE OUTPUTHIST(UFILE,HISTO,BINSIZE,NOTARGETS)
! Write PAR histogram to file.
!**********************************************************************
    
    USE maestcom
    USE switches
    IMPLICIT NONE
    INTEGER UFILE,NOTARGETS,ITAR,IBIN
    REAL HISTO(MAXT,MAXHISTO)
    REAL BINSIZE
    
    IF (IOFORMAT .EQ. 0) THEN
        DO ITAR = 1,NOTARGETS
            WRITE (UFILE,500) ITAR
            WRITE (UFILE,510)
            500 FORMAT ('TREE NO: ',I6)
            510 FORMAT ('  PAR:         FREQUENCY (M^2.HR): ')
            DO IBIN = 1,MAXHISTO
                WRITE (UFILE,520) (IBIN-0.5)*BINSIZE,HISTO(ITAR,IBIN)
                520 FORMAT (F8.2,1X,F12.6)
            END DO
        END DO
    ELSE IF (IOFORMAT .EQ. 1) THEN
        DO ITAR = 1,NOTARGETS
            WRITE (UFILE) REAL(ITAR)
            !WRITE (UFILE,510)
            !500 FORMAT ('TREE NO: ',I6)
            !510 FORMAT ('  PAR:         FREQUENCY (M^2.HR): ')
            DO IBIN = 1,MAXHISTO
                WRITE (UFILE) (IBIN-0.5)*BINSIZE,HISTO(ITAR,IBIN)
            END DO
        END DO
    END IF
    
    RETURN
END SUBROUTINE OUTPUTHIST

!**********************************************************************
INTEGER FUNCTION INEDGES(DX,DY,XMAX,YMAX,EDGEDIST)
! Checks to see if the chosen tree is within EDGEDIST m of the edge
! of the plot. Returns -1 if yes and 1 if no.
!**********************************************************************
    
    IMPLICIT NONE
    REAL DX,DY,XMAX,YMAX,EDGEDIST
    
    IF ((DX.LT.EDGEDIST).OR.(DY.LT.EDGEDIST).OR.(XMAX-DX.LT.EDGEDIST).OR.(YMAX-DY.LT.EDGEDIST)) THEN
        INEDGES = -1
    ELSE
        INEDGES = 1
    END IF

    RETURN
END FUNCTION INEDGES

!**********************************************************************
SUBROUTINE open_file(fname, unit, action, file_format, status)
! More general file interface, opening files...
! MGDK, Nov. 2010.
!**********************************************************************    
    USE maestcom
    IMPLICIT NONE
    
    INTEGER length, ioerror
    !INTEGER, INTENT(inout) ::  unit   !  logical file unit
    INTEGER  unit   !  logical file unit
    !CHARACTER(len=*), INTENT(in) ::  action, file_format, fname, status
    CHARACTER(len=*)  action, file_format, fname, status
    
    SELECT CASE (file_format)
    CASE (format_ascii)
        OPEN(unit, file=fname, action=action, status=status, iostat=ioerror)
        IF (ioerror /= 0) THEN
            WRITE(STDOUT,*) 'Cant open ascii file:', fname
            STOP
        END IF
    CASE (format_binary)
        ! workout record length, assume float.
        OPEN(unit, file=fname, form='unformatted', access='stream', action=action, &
            status=status, iostat=ioerror)
        IF (ioerror /= 0) THEN
            WRITE(STDOUT,*) 'Cant open binary file:', fname
            STOP
        END IF            
    CASE default
        WRITE(*,*) 'Error opening file, dont understand the format:', TRIM(file_format)
    END SELECT

    END SUBROUTINE open_file

    
    
!**********************************************************************
      SUBROUTINE GETPOINTSF(NUMTESTPNT,XL,YL,ZL,X0,Y0,XMAX,YMAX, &
        CTITLE,TTITLE,MTITLE,STITLE,VTITLE)
! Subroutine for testing radiation interception routines.
! Open input & output files and read information about sensor positions.
!**********************************************************************

      USE maestcom
      IMPLICIT NONE
      INTEGER NOPOINTS,INPUTTYPE,IOERROR,NUMTESTPNT,N,I
      
      CHARACTER(70) CTITLE, TTITLE, PTITLE, STITLE, MTITLE, VTITLE
      REAL XL(MAXP),YL(MAXP),ZL(MAXP),COORDS(MAXP*3)
      REAL X0,Y0,ANGLE,SPACING,ZHEIGHT,COSANG,SINANG,DIST
      REAL XMAX,YMAX

      NAMELIST /CONTROL/ NOPOINTS,INPUTTYPE
      NAMELIST /XYZ/ COORDS
      NAMELIST /TRANSECT/ ANGLE,SPACING,ZHEIGHT

! Open input file
      OPEN (UPOINTSI, FILE = 'points.dat', STATUS = 'OLD', &
         IOSTAT=IOERROR)
      
      IF (IOERROR.NE.0) &
        CALL SUBERROR('ERROR: POINTS INPUT FILE DOES NOT EXIST', &
        IFATAL,IOERROR)

! Read title from input file
990   FORMAT (A60)     ! For reading titles in input files.
      READ (UPOINTSI, 990) PTITLE

! Default values
      NOPOINTS = 0
      INPUTTYPE = 1

! Read control flags: no of points and type of input
      READ (UPOINTSI, CONTROL, IOSTAT = IOERROR)
      IF ((IOERROR.NE.0).OR.(NOPOINTS.EQ.0)) &
        CALL SUBERROR('ERROR: MISSING CONTROL INFO IN POINTS FILE', &
        IFATAL,IOERROR)
      IF (NOPOINTS.GT.MAXP) THEN
        CALL SUBERROR('WARNING: TOO MANY TEST POINTS SPECIFIED', &
        IWARN,IOERROR)
        NUMTESTPNT = MAXP
      ELSE 
        NUMTESTPNT = NOPOINTS
      END IF

! Read in list of points
      IF (INPUTTYPE.EQ.1) THEN
        READ (UPOINTSI, XYZ, IOSTAT = IOERROR)
        IF (IOERROR.NE.0) &
        CALL SUBERROR('ERROR READING GRID POINTS', &
        IFATAL,IOERROR)
        DO  N = 1,NUMTESTPNT
          XL(N) = COORDS((N-1)*3 + 1) - X0
          YL(N) = COORDS((N-1)*3 + 2) - Y0
          ZL(N) = COORDS(N*3)
        ENDDO

! Read in details of transect & construct points
      ELSE IF (INPUTTYPE.EQ.2) THEN
        READ (UPOINTSI, TRANSECT, IOSTAT = IOERROR)
        IF (IOERROR.NE.0) &
          CALL SUBERROR('ERROR READING TRANSECT DETAILS', &
          IFATAL,IOERROR)
        ANGLE = ANGLE*PID180
        COSANG = COS(ANGLE)
        SINANG = SIN(ANGLE)
        DIST = SPACING/2.0
        DO N = 1,NUMTESTPNT
          XL(N) = DIST*COSANG
          YL(N) = DIST*SINANG
          ZL(N) = ZHEIGHT
          DIST =  DIST + SPACING
        ENDDO
      END IF
  
      DO I = 1,NUMTESTPNT
         IF( (XL(I).LT.X0.OR.XL(I).GT.XMAX) .OR. (YL(I).LT.Y0.OR.YL(I).GT.YMAX) ) THEN
            CALL SUBERROR('WARNING: MAESTEST MAY CRASH WHEN POINTS ARE OUTSIDE PLOT BOUNDS. FIX POINTS.DAT IF INFINITE LOOP OCCURS!', IWARN, -1)
         ENDIF
      ENDDO
  
! Open output file
      OPEN (UPOINTSO, FILE = 'testflx.dat', STATUS = 'UNKNOWN')
! Write headings to output file
991   FORMAT (A12,A60) ! For writing comments to output files.
992   FORMAT (1X,3(A3,1X),9(A12,1X))
993   FORMAT (A60)
994   FORMAT (A90)

      WRITE (UPOINTSO, 991) 'Program:    ', VTITLE
      WRITE (UPOINTSO, 991) 'Control:    ', CTITLE
      WRITE (UPOINTSO, 991) 'Trees:      ', TTITLE
      !WRITE (UPOINTSO, 991) 'Structure:  ', STITLE
      WRITE (UPOINTSO, 991) 'Points:     ', PTITLE
      WRITE (UPOINTSO, 991) 'Met data:   ', MTITLE
      WRITE (UPOINTSO, *)
      WRITE (UPOINTSO, 993) 'DAY: day number'
      WRITE (UPOINTSO, 993) 'HR: hour number'
      WRITE (UPOINTSO, 993) 'PT: point number'
      WRITE (UPOINTSO, 993) 'X,Y,Z, : coordinates of test point'
      WRITE (UPOINTSO, 993) 'PAR: incident PAR (umol m-2 s-1)'
      WRITE (UPOINTSO, 993) 'FBEAM: beam fraction of PAR'
      WRITE (UPOINTSO, 993)  &
        'SUNLA: sunlit leaf area at grid point (fraction)'
      WRITE (UPOINTSO, 993) &
         'TD: diffuse transmittance to grid point (fraction)'
      WRITE (UPOINTSO, 993) 'TSCAT: scattered radiation (umol m-2 s-1)'
      WRITE (UPOINTSO, 993) 'TTOT: total radiation (umol m-2 s-1)'
      WRITE (UPOINTSO, 993) 'APARSUN : Absorped PAR for sunlit foliage (umol m-2 s-1)'
      WRITE (UPOINTSO, 993) 'APARSH : Absorped PAR for shaded foliage (umol m-2 s-1)'
      WRITE (UPOINTSO, 993) 'APAR : Absorped PAR (umol m-2 s-1)'
      WRITE(UPOINTSO,993)' '
      WRITE (UPOINTSO,994)'DAY HR PT  X  Y  Z  PAR  FBEAM   SUNLA  TD   TSCAT  TTOT APARSUN APARSH APAR ' 

      RETURN
      END ! GetPointsF


