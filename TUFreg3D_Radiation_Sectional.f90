! _____________________________________________________________________________________
!
!   TUFreg3D Radiation (ONLY) model -- Sectional version
!
! -------------------------------------------------------------------------------------
!   "reg" refers to the fact that this version of the model is an optimized version
!   of the original model, in that it models regularly-spaced arrays of identical
!   square-footprint buildings, and does so witsh significantly less computational
!   expense relative to the original version (which has a bounding wall, and allows
!   for any plane parallel geometry). This version also includes the option to run
!   a given simulation (with given material properties and forcing data) multiple
!   times in a row, varying the geometry (lambdap, H/L and street orientation) and
!   the latitude

!   This version only models radiation, given a geometry, radiative parameters, Tsfc
!   distribution (for longwave), and times of day (for solar)

!   This version now includes the sectional radiation balance (i.e. averages over all
!   patches at each patch height)

!   This model is written primarily in Fortran 77 but uses some Fortran 90. Therefore
!   a Fortran 90 compiler is required.

! -------------------------------------------------------------------------------------
!   Original references:
!
!   Krayenhoff ES, Voogt JA (2007) A microscale three-dimensional urban energy balance               
!   model for studying surface temperatures. Boundary-Layer Meteorol 123:433-461
!
!   Krayenhoff ES (2005) A micro-scale 3-D urban energy balance model for studying
!   surface temperatures. M.Sc. Thesis, University of Western Ontario, London, Canada
! -------------------------------------------------------------------------------------
!
!   *** This model is for research and teaching purposes only ***
!
! -------------------------------------------------------------------------------------
!
!   Last updated:
!     June 2009 by Scott Krayenhoff
!
! _____________________________________________________________________________________
use TUFConstants
      implicit none

!     FORTRAN 77 variables

      logical ray,matlab_out
      logical first_write,newlp,newbhbl
      logical ywrite
      logical swv,lwv,frcKdn,frcLdn,frcTsfc,emiss_var

      character numc,lpwrite1,strorwrite1,latwrite1,ydwrite1,bhblwrite1
      character(2) numc2,lpwrite,strorwrite,latwrite,ydwrite2,bhblwrite2
      character(3) numc3,time1,ydwrite,bhblwrite,latwrite2
      character(4) time2

      integer i,numsfc,xx,yy,zz,ff,k,par,iij,l,bhiter,nKgrid,iii
      integer x,y,z,f,iv,m,n,p,time_out,numres,resiter
      integer j,numvf,vfiend,a1,a2,b1,b2,avg_cnt,zj
      integer numvertex,timewrite,numcany,numstror
      integer numNwall2,numSwall2,numEwall2,numWwall2,numstreet2
      integer numroof2,numwall2,numabovezH,counter2,numbhbl
      integer iab,par_ab,numsfc_ab,numsfc2,jab,minres_bh
      integer numlp,lpiter,minres,lptowrite
      integer bhbltowrite,nbuildx,nbuildy
      integer numfrc,timefrc_index,maxbh,tim,numout
      integer al,aw,bh,bh3,numfiles,vfcalc
      integer q,yd,yd_actual,bl,bw,sw,sw2
      integer numroof,al2,aw2,badKdn,cloudtype

      real fx(5),fy(5),fz(5),fxx(5),fyy(5),fzz(5),Troof,Troad,Twall
      real pi,vx,vy,vz,vxx,vyy,vzz,xpinc,ypinc,vftot,tmp(30)
      real stror,az,ralt,ypos,Kdir,Ldn,solarin,vfsum2
      real dx(3),y1,z1,z2,x2,yyy,Lup_refl,Lup_refl_old
      real Lemit5,vftot5,Kup_refl,Kup_refl_old
      real cz,inot,press,Td,Ktot,alb_sfc,abs_aero
      real lat,tm,zeni,azim,angdif,HWactual,Kdn_wall_tot_day
      real HW_avg2,wavelenx,waveleny,Kdn_tot
      real stror_in,strorint,strormax,xlatint,xlatmax,xlat_in
      real dta_starttime,dta_timeend,lpactual,Kdir_NoAtm
      real Kdir_Calc,Kdif_Calc,Kbeam,bhblactual
      real outpt_tm,Ldn_fact,start_time,end_time,Ta,CA
      real deltatfrc,zH,Ktotfrc,Tsfc_R,Rnet_tot,Tsfc_cplt
      real Tsfc_bird,Tsfc_N,Tsfc_S,Tsfc_E,Tsfc_W,Tsfc_T
      real Rnet,Aplan,Kdn_diff,nKdndiff,lambdapR
      real Rnet_R,Rnet_T,Rnet_N,Rnet_S,Rnet_E,Rnet_W
      real Kdn_R,Kup_R,Ldn_R,Lup_R,Kdn_T,Kup_T,Ldn_T,Lup_T
      real Kdn_N,Kup_N,Ldn_N,Lup_N,Kdn_S,Kup_S,Ldn_S,Lup_S
      real Kdn_E,Kup_E,Ldn_E,Lup_E,Kdn_W,Kup_W,Ldn_W,Lup_W
      real svf_roof,svf_road,svf_north,svf_south,svf_east,svf_west
      real Ta_sol,Td_sol,Emit_W,Absbl_W,Absbs_W,ea,svf_canopy
      real Trad_R,Trad_T,Trad_N,Trad_S,Trad_E,Trad_W
      real vf,fact2,xt,yt,zt,zen,bldht_tot,htmean_HrStr
      real htmean_wall_HrStr,Kdn_road_HrStr,Kdn_wall_HrStr
      real sigma,Kup,Lup,xlat,Kdn_re_store,Kdn_ae_store
      real timeis,timeend,deltat,separat,Kdn_roof_HrStr
      real albr,albw,albs,emisr,emisw,emiss,lambdac
      real Kdn_grid,Kdif,dalb,DR1F,emissInter,emissNS,emissEW
      real Tsfcr,Tsfcw,Tsfcs,refldiff,svferror,svfe_store
      real Kdn_tothr_wall,Kdn_tothr,htmean_Kdn,htmean_Kdn_day
      real htmean_Kdn_wall,htmean_Kdn_wall_day,Ldn_in

      REAL(KIND=8) corner(4,3),v(4,3),vmag(4),vangle(4)
      REAL(KIND=8) vns(3),cp(3),vtemp1(3),vtemp2(3),vfsum
      REAL(KIND=8) magvns,mcp,dp,g,vecti(3),vectj(3)
      REAL(KIND=8) pll,F3,F5,F7,F9
      REAL(KIND=8) angsun(3),angsfc(3)


!     FORTRAN 90 variables


      integer,allocatable,dimension(:,:) :: bldht
      integer,allocatable,dimension(:,:) :: veght
      logical,allocatable,dimension(:,:,:) :: surf_shade
      logical,allocatable,dimension(:,:,:) :: veg_shade
      logical,allocatable,dimension(:,:,:,:) :: surf
      integer,allocatable,dimension(:,:) :: bldhti
      integer,allocatable,dimension(:,:) :: veghti
      real,allocatable,dimension(:,:) :: sfc
      integer,allocatable,dimension(:) :: ind_ab
      real,allocatable,dimension(:,:) :: sfc_ab
      integer,allocatable,dimension(:) :: vffile
      integer,allocatable,dimension(:) :: vfppos
      integer,allocatable,dimension(:) :: vfipos
      integer,allocatable,dimension(:) :: mend
      real,allocatable,dimension(:) :: refl_emist
      real,allocatable,dimension(:) :: absbs
      real,allocatable,dimension(:) :: absbl
      real,allocatable,dimension(:) :: refls
      real,allocatable,dimension(:) :: refll
      real,allocatable,dimension(:) :: reflts
      real,allocatable,dimension(:) :: refltl
      real,allocatable,dimension(:) :: reflps
      real,allocatable,dimension(:) :: reflpl
      real,allocatable,dimension(:) :: vf2
      real,allocatable,dimension(:) :: vf3
      integer,allocatable,dimension(:) :: vf3j
      integer,allocatable,dimension(:) :: vf2j
      real,allocatable,dimension(:,:) :: vertex
      integer,allocatable,dimension(:,:) :: face
!      real,allocatable,dimension(:) :: lambda
!      real,allocatable,dimension(:) :: lambdaav
!      real,allocatable,dimension(:) :: htcap
!      real,allocatable,dimension(:) :: thick
!      real,allocatable,dimension(:) :: tlayer
!      real,allocatable,dimension(:) :: tlayerp
!      real,allocatable,dimension(:) :: gam
!      real,allocatable,dimension(:) :: denom
!      real,allocatable,dimension(:) :: A
!      real,allocatable,dimension(:) :: B
!      real,allocatable,dimension(:) :: D
!      real,allocatable,dimension(:) :: R
!      real,allocatable,dimension(:) :: lambda_sfc
      real,allocatable,dimension(:) :: Tsfc
      real,allocatable,dimension(:) :: Trad
      real,allocatable,dimension(:) :: Kdnfrc
      real,allocatable,dimension(:) :: Ldnfrc
      real,allocatable,dimension(:) :: Tafrc
      real,allocatable,dimension(:) :: eafrc
      real,allocatable,dimension(:) :: Trooffrc
      real,allocatable,dimension(:) :: Troadfrc
      real,allocatable,dimension(:) :: Twallfrc
      real,allocatable,dimension(:) :: timefrc
!      real,allocatable,dimension(:) :: Qh
!      real,allocatable,dimension(:) :: lambdar
!      real,allocatable,dimension(:) :: lambdaavr
!      real,allocatable,dimension(:) :: htcapr
!      real,allocatable,dimension(:) :: thickr
!      real,allocatable,dimension(:) :: depthr
!      real,allocatable,dimension(:) :: lambdas
!      real,allocatable,dimension(:) :: lambdaavs
!      real,allocatable,dimension(:) :: htcaps
!      real,allocatable,dimension(:) :: thicks
!      real,allocatable,dimension(:) :: depths
!      real,allocatable,dimension(:) :: lambdaw
!      real,allocatable,dimension(:) :: lambdaavw
!      real,allocatable,dimension(:) :: htcapw
!      real,allocatable,dimension(:) :: thickw
!      real,allocatable,dimension(:) :: depthw
!      real,allocatable,dimension(:) :: Uwrite
!      real,allocatable,dimension(:) :: Twrite
!      real,allocatable,dimension(:,:) :: sdT
!      real,allocatable,dimension(:,:) :: sdN
!      real,allocatable,dimension(:,:) :: sdS
!      real,allocatable,dimension(:,:) :: sdE
!      real,allocatable,dimension(:,:) :: sdW
      real,allocatable,dimension(:) :: resin
      real,allocatable,dimension(:) :: lpin
      real,allocatable,dimension(:) :: bh_o_bl
! sectional
      real,allocatable,dimension(:) :: Rnet_walls
      real,allocatable,dimension(:) :: Kdn_walls
      real,allocatable,dimension(:) :: Kdn_walls_plan
      real,allocatable,dimension(:) :: Kup_walls  
      real,allocatable,dimension(:) :: Ldn_walls
      real,allocatable,dimension(:) :: Lup_walls
      real,allocatable,dimension(:) :: svf_walls
      real,allocatable,dimension(:) :: Rnet_Ns
      real,allocatable,dimension(:) :: Kdn_Ns
      real,allocatable,dimension(:) :: Kup_Ns  
      real,allocatable,dimension(:) :: Ldn_Ns
      real,allocatable,dimension(:) :: Lup_Ns
      real,allocatable,dimension(:) :: svf_norths
      real,allocatable,dimension(:) :: Rnet_Ss
      real,allocatable,dimension(:) :: Kdn_Ss
      real,allocatable,dimension(:) :: Kup_Ss  
      real,allocatable,dimension(:) :: Ldn_Ss
      real,allocatable,dimension(:) :: Lup_Ss
      real,allocatable,dimension(:) :: svf_souths
      real,allocatable,dimension(:) :: Rnet_Es
      real,allocatable,dimension(:) :: Kdn_Es
      real,allocatable,dimension(:) :: Kup_Es 
      real,allocatable,dimension(:) :: Ldn_Es
      real,allocatable,dimension(:) :: Lup_Es
      real,allocatable,dimension(:) :: svf_easts
      real,allocatable,dimension(:) :: Rnet_Ws
      real,allocatable,dimension(:) :: Kdn_Ws
      real,allocatable,dimension(:) :: Kup_Ws  
      real,allocatable,dimension(:) :: Ldn_Ws
      real,allocatable,dimension(:) :: Lup_Ws
      real,allocatable,dimension(:) :: svf_wests
      real,allocatable,dimension(:) :: Kdnhr

      
      real sind,cosd,tand,asind,acosd,atand
      external sind,cosd,tand,asind,acosd,atand


! constants:
      sigma=5.67e-8
!      cpair=1004.67
!      vK=0.4
      PI=ACOS(-1.0)

! initialization
      Kdn_diff=0.
      nKdndiff=0
      badKdn=0
      svfe_store=0.
      Kdn_ae_store=0.


! MAIN PARAMETER AND INITIAL CONDITION INPUT FILE
!  read in the input file values
      open (parametersRadiationDat,file='parameters_Radiation.dat')

! output file recording inputs:
      open(unit=inputsStoreOut,file='Inputs_Store.out',status='unknown',form='formatted')

! model/integration parameters
      read(parametersRadiationDat,*)vfcalc
!      read(parametersRadiationDat,*)yd,deltat,outpt_tm
      read(parametersRadiationDat,*)yd,outpt_tm,start_time,end_time
!      read(parametersRadiationDat,*)Tthreshold
!      read(parametersRadiationDat,*)facet_out,matlab_out,sum_out
      read(parametersRadiationDat,*)matlab_out

! radiative forcing and surface temperature and air pressure
      read(parametersRadiationDat,*)lwv,swv
      read(parametersRadiationDat,*)frcKdn,frcLdn,frcTsfc
      read(parametersRadiationDat,*)press

! radiative parameters
      read(parametersRadiationDat,*)dalb
      read(parametersRadiationDat,*)albr,albs,albw
      read(parametersRadiationDat,*)emisr,emiss,emisw
      read(parametersRadiationDat,*)emiss_var
      read(parametersRadiationDat,*)emissInter,emissNS,emissEW
      read(parametersRadiationDat,*)cloudtype

! conduction parameters
!      read(parametersRadiationDat,*)IntCond,Intresist
!      read(parametersRadiationDat,*)uc,numlayers


!      allocate(lambda(1:numlayers))
!      allocate(lambdaav(1:numlayers))
!      allocate(htcap(1:numlayers))
!      allocate(thick(1:numlayers))
!      allocate(lambdar(1:numlayers))
!      allocate(lambdaavr(1:numlayers))
!      allocate(htcapr(1:numlayers))
!      allocate(thickr(1:numlayers))
!      allocate(lambdas(1:numlayers))
!      allocate(lambdaavs(1:numlayers))
!      allocate(htcaps(1:numlayers))
!      allocate(thicks(1:numlayers))
!      allocate(lambdaw(1:numlayers))
!      allocate(lambdaavw(1:numlayers))
!      allocate(htcapw(1:numlayers))
!      allocate(thickw(1:numlayers))
!      allocate(tlayer(1:numlayers))
!      allocate(tlayerp(1:numlayers))
!      allocate(gam(1:numlayers))
!      allocate(denom(1:numlayers))
!      allocate(A(1:numlayers))
!      allocate(B(1:numlayers))
!      allocate(D(1:numlayers))
!      allocate(R(1:numlayers))
!      allocate(depthr(1:numlayers))
!      allocate(depths(1:numlayers))
!      allocate(depthw(1:numlayers))


!      do k=1,numlayers
!       read(parametersRadiationDat,*)thickr(k),lambdar(k),htcapr(k)
!      enddo
!      do k=1,numlayers
!       read(parametersRadiationDat,*)thicks(k),lambdas(k),htcaps(k)
!      enddo
!      do k=1,numlayers
!       read(parametersRadiationDat,*)thickw(k),lambdaw(k),htcapw(k)
!      enddo

! convection parameters
!      read(parametersRadiationDat,*)z0,lambdaf,zrooffrc
!      read(parametersRadiationDat,*)z0roofm,z0roadm,z0roofh,z0roadh,moh,rw

! domain geometry
!      read(parametersRadiationDat,*)buildht_m,zref
      read(parametersRadiationDat,*)numres

      allocate(resin(1:numres))

      do k=1,numres
       read(parametersRadiationDat,*)resin(k)
      enddo
!      read(parametersRadiationDat,*)minres

! initial temperatures
      read(parametersRadiationDat,*)Tsfcr,Tsfcs,Tsfcw
!      read(parametersRadiationDat,*)Tintw,Tints,Tfloor,Tbuild_min
!      read(parametersRadiationDat,*)Ldn_in
! loop parameters
      read(parametersRadiationDat,*)stror_in,strorint,strormax
      read(parametersRadiationDat,*)xlat_in,xlatint,xlatmax
      read(parametersRadiationDat,*)numlp

      allocate(lpin(1:numlp))

      do k=1,numlp
       read(parametersRadiationDat,*)lpin(k)
      enddo
      read(parametersRadiationDat,*)numbhbl

      allocate(bh_o_bl(1:numbhbl))
      
      do l=1,numbhbl
       read(parametersRadiationDat,*)bh_o_bl(l)
      enddo



      close(parametersRadiationDat)

!      if(z0.lt.0.) then
!       calcz0=.true.
!      endif

      deltat=outpt_tm

      if(vfcalc.eq.0.and.(numlp.gt.1.or.numbhbl.gt.1)) then
      write(6,*)'must turn on calculation of view factors (i.e.vfcalc=1) if more than one lambdap or H/L ratio is chosen'
      write(6,*)'vfcalc, numlp =',vfcalc,numlp
       stop
      endif

      if(abs(xlat_in).gt.90.0.or.abs(xlatmax).gt.90.0) then
       write(6,*)'one of xlat_in or xlatmax is greater than 90 or less than -90, xlat_in, xlatmax =',xlat_in,xlatmax
       stop
      endif
      if(xlatint.lt.1e-9) then
       write(6,*)'xlatint must be greater than 0, xlatint=',xlatint
       write(6,*)'if you do not want to simulate more than one latitude, set xlat_in=xlatmax'
       stop
      endif

      Tsfcr=Tsfcr+273.15
      Tsfcs=Tsfcs+273.15
      Tsfcw=Tsfcw+273.15
!      Tintw=Tintw+273.15
!      Tints=Tints+273.15
!      Tfloor=Tfloor+273.15


! write to output file that records the inputs

! model/integration parameters
      write(inputsStoreOut,*)'vfcalc,yd,outpt_tm'
      write(inputsStoreOut,*)vfcalc,yd,outpt_tm
!      write(inputsStoreOut,*)'facet_out,matlab_out,sum_out'
!      write(inputsStoreOut,*)facet_out,matlab_out,sum_out
      write(inputsStoreOut,*)'matlab_out'
      write(inputsStoreOut,*)matlab_out

! radiative parameters
      write(inputsStoreOut,*)'dalb'
      write(inputsStoreOut,*)dalb
      write(inputsStoreOut,*)'albr,albs,albw,emisr,emiss,emisw'
      write(inputsStoreOut,*)albr,albs,albw,emisr,emiss,emisw
      write(inputsStoreOut,*)'emiss_var,emissInter,emissNS,emissEW'
      write(inputsStoreOut,*)emiss_var,emissInter,emissNS,emissEW
      write(inputsStoreOut,*)'cloudtype'
      write(inputsStoreOut,*)cloudtype

! conduction parameters
!      write(inputsStoreOut,*)'IntCond,Intresist,uc,numlayers'
!      write(inputsStoreOut,*)IntCond,Intresist,uc,numlayers
!      write(inputsStoreOut,*)'thickr(k),lambdar(k),htcapr(k)'
!      do k=1,numlayers
!       write(inputsStoreOut,*)thickr(k),lambdar(k),htcapr(k)
!      enddo
!      write(inputsStoreOut,*)'thicks(k),lambdas(k),htcaps(k)'
!      do k=1,numlayers
!       write(inputsStoreOut,*)thicks(k),lambdas(k),htcaps(k)
!      enddo
!      write(inputsStoreOut,*)'thickw(k),lambdaw(k),htcapw(k)'
!      do k=1,numlayers
!       write(inputsStoreOut,*)thickw(k),lambdaw(k),htcapw(k)
!      enddo

! convection parameters
!      write(inputsStoreOut,*)'z0,lambdaf,zrooffrc'
!      write(inputsStoreOut,*)z0,lambdaf,zrooffrc
!      write(inputsStoreOut,*)'z0roofm,z0roadm,z0roofh,z0roadh,moh,rw'
!      write(inputsStoreOut,*)z0roofm,z0roadm,z0roofh,z0roadh,moh,rw

! domain geometry
!      write(inputsStoreOut,*)'buildht_m,zref,minres'
!      write(inputsStoreOut,*)buildht_m,zref,minres

! initial temperatures
!      write(inputsStoreOut,*)'Tsfcr,Tsfcs,Tsfcw,Tintw,Tints,Tfloor,Tbuild_min'
!      write(inputsStoreOut,*)Tsfcr,Tsfcs,Tsfcw,Tintw,Tints,Tfloor,Tbuild_min
      write(inputsStoreOut,*)'Tsfcr,Tsfcs,Tsfcw'
      write(inputsStoreOut,*)Tsfcr,Tsfcs,Tsfcw

! loop parameters
      write(inputsStoreOut,*)'stror_in,strorint,strormax'
      write(inputsStoreOut,*)stror_in,strorint,strormax
      write(inputsStoreOut,*)'xlat_in,xlatint,xlatmax'
      write(inputsStoreOut,*)xlat_in,xlatint,xlatmax
      write(inputsStoreOut,*)'numres'
      write(inputsStoreOut,*)numres
      write(inputsStoreOut,*)'resin(k)'
      do k=1,numres
       write(inputsStoreOut,*)resin(k)
      enddo
      write(inputsStoreOut,*)'numlp'
      write(inputsStoreOut,*)numlp
      write(inputsStoreOut,*)'lpin(k)'
      do k=1,numlp
       write(inputsStoreOut,*)lpin(k)
      enddo
      write(inputsStoreOut,*)'bh_o_bl(k)'
      do l=1,numbhbl
       write(inputsStoreOut,*)bh_o_bl(l)
      enddo



! ATMOSPHERIC FORCING

!  open input atmospheric data file
      open (forcingRadiationDat,file='forcing_Radiation.dat')

      read(forcingRadiationDat,*)numfrc,dta_starttime,deltatfrc

      dta_timeend=dta_starttime+deltatfrc*real(numfrc)

      if(dta_starttime.gt.start_time.or.dta_timeend.lt.end_time) then
       write(6,*)'Start time (end time) of forcing data is too '&
                  ,'late (early) relative to the start and end times '&
                  ,'in the parameter file.'
       write(6,*)'At a minimum you need the air temperature (Tafrc) and'&
               ,' vapour pressure (eafrc) at the time(s) that you are'&
               ,' asking for radiation output; or these need to be'&
               ,' provided for times before and after the radiation'&
               ,' output time(s) so that the model can interpolate'
       write(6,*)'start time in parameter file:',start_time
       write(6,*)'start time in forcing file:',dta_starttime
       write(6,*)'end time in parameter file:',end_time
       write(6,*)'end time in forcing file (= start time + deltatfrc * numfrc):',dta_timeend
       stop
      endif


      allocate(Kdnfrc(1:numfrc+1))
      allocate(Ldnfrc(1:numfrc+1))
      allocate(Tafrc(1:numfrc+1))
      allocate(eafrc(1:numfrc+1))
      allocate(Trooffrc(1:numfrc+1))
      allocate(Troadfrc(1:numfrc+1))
      allocate(Twallfrc(1:numfrc+1))
      allocate(timefrc(1:numfrc+1))


      do k=1,numfrc+1
       read(forcingRadiationDat,*)Kdnfrc(k),Ldnfrc(k),Tafrc(k),eafrc(k),Trooffrc(k),Troadfrc(k),Twallfrc(k)
       if(Kdnfrc(k).ge.-90.) Kdnfrc(k)=max(Kdnfrc(k),0.)
       timefrc(k)=dta_starttime+real(k-1)*deltatfrc
       Trooffrc(k)=Trooffrc(k)+273.15
       Troadfrc(k)=Troadfrc(k)+273.15
       Twallfrc(k)=Twallfrc(k)+273.15
      enddo

! Initial values:
!      Press=Pressfrc(1)
!      Udir=Udirfrc(1)
      Ktotfrc=Kdnfrc(1)
      Ldn=Ldnfrc(1)
      Ta=Tafrc(1)+273.15
      ea=eafrc(1)
!      Ua=max(0.1,Uafrc(1))
      write(6,*)'initial forcing data:'
      write(6,*)'temperature (C), vapour pressure (mb) = ',Ta,ea
!      write(6,*)'wind speed (m/s), wind direction (degrees) = ',Ua,Udir
!      write(6,*)'pressure (mb) = ',Press
      write(inputsStoreOut,*)'initial forcing data:'
      write(inputsStoreOut,*)'temperature (C), vapour pressure (mb) = ',Ta,ea
!      write(inputsStoreOut,*)'wind speed (m/s), wind direction (degrees) = ',Ua,Udir
!      write(inputsStoreOut,*)'pressure (mb) = ',Press


      if(.not.frcLdn) then
! Prata's clear sky formula (QJRMS 1996)
       Ldn=(1.-(1.+46.5*ea/Ta)*exp(-((1.2+3.*46.5*ea/Ta)**(0.5))))*sigma*Ta**4
! Sellers (1965) modification of Ldown based on cloud type (cloud base height) in Oke (1987)
      if(cloudtype.eq.0) then
! clear:
       Ldn_fact=1.00
      elseif(cloudtype.eq.1) then
! cirrus:
       Ldn_fact=1.04
      elseif(cloudtype.eq.2) then
! cirrostratus:
       Ldn_fact=1.08
      elseif(cloudtype.eq.3) then
! altocumulus:
       Ldn_fact=1.17
      elseif(cloudtype.eq.4) then
! altostratus:
       Ldn_fact=1.20
      elseif(cloudtype.eq.7) then
! cumulonimbus:
       Ldn_fact=1.21
      elseif(cloudtype.eq.5) then
! stratocumulus/cumulus:
       Ldn_fact=1.22
      elseif(cloudtype.eq.6) then
! thick stratus (Ns?):
       Ldn_fact=1.24
      else
       write(6,*)'cloudtype must be between 0 and 7, cloudtype = ',cloudtype
      endif
      
      Ldn=Ldn*Ldn_fact
      write(6,*)'Ldown calc Prata & Sellers, Ldown (W/m2) = ',Ldn
      write(inputsStoreOut,*)'Ldown calc Prata & Sellers, Ldown (W/m2) = ',Ldn
      endif
      Td=(4880.357-29.66*alog(ea))/(19.48-alog(ea))

      if (frcLdn) then
       write(6,*)'Ldown (W/m2) = ',Ldn
       write(inputsStoreOut,*)'Ldown (W/m2) = ',Ldn
      endif

      if (.not.frcKdn) then
       write(6,*)'Kdown (W/m2) = to be calculated'
       write(inputsStoreOut,*)'Kdown (W/m2) = to be calculated'
      else
       write(6,*)'Kdown (W/m2) = ',Ktotfrc
       write(inputsStoreOut,*)'Kdown (W/m2) = ',Ktotfrc
      endif


! assume initial Tcan!!!
!      Tcan=Ta+0.5

      close(forcingRadiationDat)


!      timeis=starttime
!      dta_timeend=dta_starttime+deltatfrc*real(numfrc)
      timeis=start_time
      timeend=end_time

! number of times output will be written in Matlab output section:
      numout=int((dta_timeend-dta_starttime)/outpt_tm)+1

      write(inputsStoreOut,*)'numfrc,dta_starttime,deltatfrc'
      write(inputsStoreOut,*)numfrc,dta_starttime,deltatfrc

!      calclf=.false.
!      frcwrite=.false.


! OPEN OUTPUT FILES
!      open(unit=832,file='EnergyBalance_Tsfc_TimeAverage.out',
!     &status='unknown',form='formatted')
!      open(unit=833,file='Tsfc_Facets_SunShade.out',status='unknown',
!     &form='formatted')
!      open(unit=835,file='Tsfc_Facets.out',status='unknown',
!     &form='formatted')
!      open(unit=836,file='EnergyBalance_Facets.out',status='unknown',
!     &form='formatted')
      open(unit=EnergyBalanceOverallOut,file='EnergyBalance_Overall.out',status='unknown',form='formatted')
!      if(frcwrite) then
!       open(unit=843,file='Forcing.out',status='unknown',
!     &form='formatted')
!      endif
      open(unit=RadiationBalanceFacetsOut,file='RadiationBalance_Facets.out',status='unknown',form='formatted')

      open(unit=RadiationBalanceSectionalOut,file='RadiationBalance_Sectional.out',status='unknown',form='formatted')
      open(unit=RadiationBalanceSectionalDayAvgOut,file='RadiationBalance_Sectional_DayAvg.out',status='unknown',form='formatted')
      open(unit=RadiationBalance_SectionalDayStrorAvgOut,file='RadiationBalance_Sectional_DayStrorAvg.out',&
     status='unknown',form='formatted')

!      write(832,630)'lambdap,H/L,H/W,latitude,streetdir,julian_day,
!     &time_of_day(centre),time(continuous&centre),
!     &time_of_day(end),time(continuous&end),
!     &Kuptot_avg,Luptot_avg,Rntot_avg,
!     &Qhtot_avg,Qgtot_avg,Qanthro_avg,Qac_avg,Qdeep_avg,
!     &Qtau,TR_avg,TT_avg,
!     &TN_avg,TS_avg,TE_avg,TW_avg'
!      write(833,630)'lambdap,H/L,H/W,latitude,streetdir,julian_day,
!     &time_of_day,time(continuous),TTsun,TTsh,
!     &TNsun,TNsh,TSsun,TSsh,TEsun,TEsh,TWsun,TWsh'
!      write(835,887)'lambdap,H/L,H/W,latitude,streetdir,julian_day,
!     &time_of_day,time(continuous),Tcomplete,
!     &Tbirdeye,Troof,Troad,Tnorth,Tsouth,Teast,Twest,Tcan,Ta,Tint,httcR,
!     &httcT,httcW,TbrightR,TbrightT,TbrightN,TbrightS,
!     &TbrightE,TbrightW'
!      write(836,630)'lambdap,H/L,H/W,latitude,streetdir,julian_day,
!     &time_of_day,time(continuous),QR,HR,GR,
!     & QT,HT,GT,QN,HN,GN,QS,HS,GS,QE,HE,GE,QW,HW,GW'
      write(EnergyBalanceOverallOut,777)&
     'lambdap,H/L,H/W,latitude,streetdir,minres,bh,bl,' &
     ,'sw,julian_day,time_of_day,time(continuous),Rnet_tot,'&
     ,'Rnet_can,'&
     ,'Kdn,Kup,Ldn,Lup,Lup(corr),Kdir_Calc,Kdif_Calc,Kdir,Kdif,'&
     ,'Kup_can,Lup_can,Lup_can(corr),az,zen,Kdn(NoAtm)'
    
!      if(frcwrite)write(843,630)'lambdap,H/L,H/W,latitude,streetdir,
!     &time,Kdir,Kdif,Ldn,Ta,ea,Troof,Troad,Twall,az,zen'


       write(RadiationBalanceFacetsOut,777)&
     'lambdap,H/L,H/W,latitude,streetdir,minres,bh,bl,' &
     ,'sw,julian_day,time_of_day,time(continuous),SKd,SKup,' &
     ,'SLd,SLup,EKd,EKup,ELd,ELup,NKd,NKup,NLd,NLup,WKd,WKup,' &
     ,'WLd,WLup,RfKd,RfKup,RfLd,RfLup,FKd,FKup,FLd,FLup,svf_roof,' &
     ,'svf_road,svf_north,svf_south,svf_east,svf_west,svf_canopy'
     
      write(RadiationBalanceSectionalOut,777)&
     'lambdap,H/L,H/W,latitude,streetdir,minres,bh,bl,' &
     ,'sw,julian_day,time_of_day,time(continuous),MeanKdnHeightWall,' &
     ,'MeanKdnHeight,Kdn_street(PerPlanArea),' &
     ,'Kdn_walls_PerPlanArea(i=1-bh),Kdn_roof(PerPlanArea)'
     
      write(RadiationBalanceSectionalDayAvgOut,777)&
     'lambdap,H/L,H/W,latitude,streetdir,minres,bh,bl,' &
     ,'sw,julian_day,starttime,endtime,MeanKdnHeightWall,' &
     ,'MeanKdnHeight,Kdn_street(AvgFraction),' &
     ,'Kdn_walls(AvgFraction),Kdn_roof(AvgFraction)'
     
      write(RadiationBalance_SectionalDayStrorAvgOut,777)&
     'lambdap,H/L,H/W,latitude,strormin,strorint,' &
     ,'strormax,minres,bh,bl,sw,julian_day,starttime,endtime,' &
     ,'MeanKdnHeightWall,MeanKdnHeight,Kdn_street(AvgFraction),' &
     ,'Kdn_walls(AvgFraction),Kdn_roof(AvgFraction)'     

! MAIN LOOP THROUGH BUILDING GEOMETRIES (lp and bhbl)

      do resiter=1,numres
       minres=resin(resiter)
      do lpiter=1,numlp
      do bhiter=1,numbhbl
      minres_bh=minres
        newlp=.true.
      newbhbl=.true.
        vfcalc=1
 538  continue

!  NOTE that these formulae assume that bl=bw (i.e. buildings with square footprints)
!  AND that sw=sw2 (street widths are equal in both directions)

       if(lpin(lpiter).gt.0.25) then
        sw=minres_bh
        bl=nint(real(sw)*sqrt(lpin(lpiter))*(sqrt(lpin(lpiter))+1.)/(1.-lpin(lpiter)))
       else
        bl=minres_bh
        sw=nint(real(bl)*(1./sqrt(lpin(lpiter))-1.))
       endif

       bw=bl
       bh=nint((real(bl)+real(bw))/2.*bh_o_bl(bhiter))
       if(bh.lt.minres) then
        write(6,*)'INCREASING MINRES; old minres = ',minres_bh
        minres_bh=minres_bh+1
        write(6,*)'new minres = ',minres_bh
        goto 538
       endif

!       patchlen=buildht_m/real(bh)
       write(6,*)'------------------------------------------'
!       write(6,*)'patch length (m) = ',patchlen
!       write(6,*)'building height (m) = ',buildht_m
       write(6,*)'building height (patches) = ',bh
!       write(6,*)'reference or forcing height (m) = ',zref
!       write(inputsStoreOut,*)'patchlen,buildht_m,bh,zref'
!       write(inputsStoreOut,*)patchlen,buildht_m,bh,zref
       write(inputsStoreOut,*)'bh'
       write(inputsStoreOut,*)bh


!cc FOR TESTING ONLY!!!  4, 5 or 6 should be used instead of 2 for runs
!cc                      where accuracy is desired
        if(bh.lt.minres.or.sw.lt.minres.or.bl.lt.minres.or.bw.lt.minres) then
         write(6,*)'resolution too low; bh,sw,bl,bw =',bh,sw,bl,bw
         minres_bh=minres_bh+1
         goto 538
        endif

       sw2=sw


!     The following expressions control the size of the domain, which must be large
!     enough so that the radiation is properly calculated (i.e., so that
!     building walls and street in the central urban unit don't see past the
!     edge of the domain below roof level) - the expression currently used was
!     arrived at by educated guess (essentially, either large sw or large bh
!     relative to bl or bw is a problem, and requires a larger domain)
       
       print *,bh,bl,sw,sw2

      nbuildx=nint(2.*real(bh)/(real(bl)+real(sw))*5./sqrt(real(bh)/real(sw)))
      nbuildy=nint(2.*real(bh)/(real(bw)+real(sw2))*5./sqrt(real(bh)/real(sw2)))
      if(mod(nbuildx,2).eq.0) nbuildx=nbuildx+1
      if(mod(nbuildy,2).eq.0) nbuildy=nbuildy+1

! Added in the Radiation Only version to yield better accuracy
!      nbuildx=nbuildx+2
!      nbuildy=nbuildy+2

      write(inputsStoreOut,*)'nbuildx,nbuildy'
      write(inputsStoreOut,*)nbuildx,nbuildy

!    Dimensions of the domain
      aw=max(bw*5+sw2*4,nbuildy*bw+(nbuildy-1)*sw2)     
      al=max(bl*5+sw*4,nbuildx*bl+(nbuildx-1)*sw) 

      write(6,*)'________________________________'
      write(6,*)'number of buildings across domain in x, y directions:',max(5,nbuildx),max(5,nbuildy)

!   Defining the central 'urban unit' from which output is derived
       b1=nint(max(2.,(real(nbuildy)-1.)/2.)*real(bw))+nint((max(2.,(real(nbuildy)-1.)/2.)-0.4999)*real(sw2))+1   
       b2=b1+bw+sw2-1
       a1=nint(max(2.,(real(nbuildx)-1.)/2.)*real(bl))+nint((max(2.,(real(nbuildx)-1.)/2.)-0.4999)*real(sw))+1
       a2=a1+bl+sw-1

!   Geometric ratios of the central 'urban unit'
       lpactual=real(bl)*real(bw)/real(bl+sw)/real(bw+sw2)
       hwactual=real(bh)*2./(real(sw)+real(sw2))
       bhblactual=real(bh)*2./(real(bl)+real(bw))
       write(6,*)'building height, length, street width (patches) = ',bh,bl,sw
       write(6,*)'domain dimension in x, y (patches) = ',al,aw
       write(6,*)'urban unit start & end in x (patches) = ',a1,a2
       write(6,*)'urban unit start & end in y (patches) = ',b1,b2
       write(inputsStoreOut,*)'bh,bl,sw,lpactual,bhblactual,hwactual,aw,al,a1,a2'
       write(inputsStoreOut,*)bh,bl,sw,lpactual,bhblactual,hwactual,aw,al,a1,a2

      do i=1,bh
       tmp(i)=(real(i)-0.5)/real(bh)
      enddo
      write(RadiationBalanceSectionalOut,817)&
   'lambdap,H/L,H/W,latitude,streetdir,minres,bh,bl,sw,julian_day,time_of_day,time(continuous),MeanKdnHeightWall,MeanKdnHeight,0.0'&
      ,(tmp(i),i=1,bh),real(bh)/real(bh)
!      write(RadiationBalanceSectionalDayAvgOut,817)'lambdap,H/L,H/W,latitude,streetdir,minres,bh,bl,sw,
!     & julian_day,starttime,endtime,MeanKdnHeightWall,
!     & MeanKdnHeight,0.0',(tmp(i),i=1,bh),real(bh)/real(bh)


! Initial atmospheric values:
!      Press=Pressfrc(1)
!      Udir=Udirfrc(1)
      Ktotfrc=Kdnfrc(1)
      Ldn=Ldnfrc(1)
      Ta=Tafrc(1)+273.15
      ea=eafrc(1)
!      Ua=max(0.1,Uafrc(1))


! Determine inter-layer thermal conductivities
!      do k=1,numlayers-1
!       lambdaavr(k)=(thickr(k)+thickr(k+1))/(thickr(k)/lambdar(k)
!     &+thickr(k+1)/lambdar(k+1))
!      enddo
! adding additional resistance (0.123 W/m2/K) at building interiors
!       lambdaavr(numlayers)=thickr(numlayers)/2./(Intresist
!     &+thickr(numlayers)/2./lambdar(numlayers))
!      do k=1,numlayers-1
!       lambdaavs(k)=(thicks(k)+thicks(k+1))/(thicks(k)/lambdas(k)
!     &+thicks(k+1)/lambdas(k+1))
!      enddo
!      lambdaavs(numlayers)=lambdas(numlayers)
!      do k=1,numlayers-1
!       lambdaavw(k)=(thickw(k)+thickw(k+1))/(thickw(k)/lambdaw(k)
!     &+thickw(k+1)/lambdaw(k+1))
!      enddo
! adding additional resistance (0.123 W/m2/K) at building interiors
!       lambdaavw(numlayers)=thickw(numlayers)/2./(Intresist
!     &+thickw(numlayers)/2./lambdaw(numlayers))


!  uc is the explicit/implicit/Crank-Nicholson control (uc=1 is implicit,
!  uc=0 is Forward Euler or explicit)
!  For explicit or partially-explicit diffusion, make sure that the
!  time step is small enough, using the diffusivity and
!  the thicknesses (thick); based on CFL criterion analog, see
!  Jacobson p. 165

!       if(uc.le.1.0.and.uc.ge.0.0) then
!        Fourmin=6.*999.
! middle layers
!        do k=1,numlayers-1
!         factR=((thickr(k)+thickr(k+1))/2.)**2/lambdaavr(k)
!     &*min(htcapr(k),htcapr(k+1))
!         if(factR.lt.Fourmin) Fourmin=factR
!         factR=((thicks(k)+thicks(k+1))/2.)**2/lambdaavs(k)
!     &*min(htcaps(k),htcaps(k+1))
!         if(factR.lt.Fourmin) Fourmin=factR
!         factR=((thickw(k)+thickw(k+1))/2.)**2/lambdaavw(k)
!     &*min(htcapw(k),htcapw(k+1))
!         if(factR.lt.Fourmin) Fourmin=factR
!        enddo
! deep half-layer
!         factR=(thickr(numlayers)/2.)**2/lambdaavr(numlayers)
!     &*htcapr(numlayers)
!         if(factR.lt.Fourmin) Fourmin=factR
!         factR=(thicks(numlayers)/2.)**2/lambdaavs(numlayers)
!     &*htcaps(numlayers)
!         if(factR.lt.Fourmin) Fourmin=factR
!         factR=(thickw(numlayers)/2.)**2/lambdaavw(numlayers)
!     &*htcapw(numlayers)
!         if(factR.lt.Fourmin) Fourmin=factR
! surface half-layer
!         factR=(thickr(1)/2.)**2/lambdar(1)*htcapr(1)
!         if(factR.lt.Fourmin) Fourmin=factR
!         factR=(thicks(1)/2.)**2/lambdas(1)*htcaps(1)
!         if(factR.lt.Fourmin) Fourmin=factR
!         factR=(thickw(1)/2.)**2/lambdaw(1)*htcapw(1)
!         if(factR.lt.Fourmin) Fourmin=factR
        
!  so as to not have excessive timesteps with implicit or near
!  implicit values of uc, which would still be stable but not
!  very temporally accurate (for typical urban parameters):
!        uc_temp=min(uc,0.9)
!        if(deltat.gt.Fourmin/(1.-uc_temp)/6.) then
!         write(6,*)'------------------------------------------'
!         write(6,*)'REDUCING TIME STEP FOR STABILITY OR ACCURACY OF THE 
!      &CONDUCTION SCHEME (this is the maximum for the whole simulation)'
!         write(6,*)'old time step =',deltat,'  new time step = ',
!     &Fourmin/(1.-uc_temp)/6.
!         deltat=Fourmin/(1.-uc_temp)/6.
!        endif
!       else
!        write(6,*)'invalid uc value:',uc,'must be 0.0-1.0 (inclusive)'
!        stop
!       endif


!  Various settings and calculations

!      solar_refl_done=.false.

      par=12
!      par_ab=5+4*numlayers
      par_ab=5

      ralt=90.-zen

!  so that output will be written at the final timestep
!      timeend=timeend+1.5*deltat/3600.

! for Matlab visualization output
      first_write=.true.


! Create the domain (call barray_cube)


      allocate(bldhti(0:al+1,0:aw+1))
      allocate(veghti(0:al+1,0:aw+1))
! sectional variables
      allocate(Rnet_walls(1:bh))
      allocate(Kdn_walls(1:bh))
      allocate(Kdn_walls_plan(1:bh))
      allocate(Kup_walls(1:bh))  
      allocate(Ldn_walls(1:bh))
      allocate(Lup_walls(1:bh))
      allocate(svf_walls(1:bh))
      allocate(Rnet_Ns(1:bh))
      allocate(Kdn_Ns(1:bh))
      allocate(Kup_Ns(1:bh))  
      allocate(Ldn_Ns(1:bh))
      allocate(Lup_Ns(1:bh))
      allocate(svf_norths(1:bh))
      allocate(Rnet_Ss(1:bh))
      allocate(Kdn_Ss(1:bh))
      allocate(Kup_Ss(1:bh))  
      allocate(Ldn_Ss(1:bh))
      allocate(Lup_Ss(1:bh))
      allocate(svf_souths(1:bh))
      allocate(Rnet_Es(1:bh))
      allocate(Kdn_Es(1:bh))
      allocate(Kup_Es(1:bh))  
      allocate(Ldn_Es(1:bh))
      allocate(Lup_Es(1:bh))
      allocate(svf_easts(1:bh))
      allocate(Rnet_Ws(1:bh))
      allocate(Kdn_Ws(1:bh))
      allocate(Kup_Ws(1:bh))  
      allocate(Ldn_Ws(1:bh))
      allocate(Lup_Ws(1:bh))
      allocate(svf_wests(1:bh))
      allocate(Kdnhr(0:bh+1))
 

      write(6,*)'a1'

      do x=0,al+1
       do y=0,aw+1
        bldhti(x,y)=0
        veghti(x,y)=0
       enddo
      enddo

      call barray_cube(bw,bl,sw,sw2,al,aw,bh,bldhti,veghti)

      maxbh=0
      numroof=0
      bldht_tot=0.
      do y=1,aw
       do x=1,al
        if(bldhti(x,y).gt.0) then
         bldht_tot = bldht_tot + bldhti(x,y)
         if(bldhti(x,y).gt.maxbh) maxbh=bldhti(x,y)
         numroof = numroof + 1
        endif
       enddo
      enddo

!       if(real(maxbh)*patchlen.gt.zref-0.1*zH) then
!        write(6,*)'zref must be at least 0.1*zH above highest roof'
!        write(6,*)'maxbh, zref, 0.1*zH (all in m) = ',maxbh*patchlen
!     &,zref,0.1*zH
!        stop
!       endif

      zH=real(bldht_tot)/real(numroof)

! in metres:
!      zH=zH*patchlen

! thermal roughness lengths if not specified:
!      if (z0roofh.lt.0.) z0roofh=z0roofm/moh
!      if (z0roadh.lt.0.) z0roadh=z0roadm/moh

!      if (z0roofh/z0roofm.lt.(1./210.).or.z0roadh/z0roadm.lt.
!     &(1./210.)) then
!      write(6,*)'Problem; ratio too small: z0roof(h/m), z0road(h/m) = ',
!     &z0roofh/z0roofm,z0roadh/z0roadm
!       stop
!      endif

!      dTcan_old=0.

      lambdapR=0.
!      canyair=0.
      do y=1,aw
       do x=1,al
! determine the total air volume below zH in the central urban unit
        if(x.ge.a1.and.x.le.a2.and.y.ge.b1.and.y.le.b2) then
!         canyair=canyair+max(0.,zH-real(bldhti(x,y))*patchlen)
!         if(real(bldhti(x,y))*patchlen.ge.zH-0.01) lambdapR=lambdapR+1.
         if(real(bldhti(x,y)).ge.zH-0.01) lambdapR=lambdapR+1.
        endif
       enddo
      enddo
      lambdapR=lambdapR/real((a2-a1+1)*(b2-b1+1))

      al2 = al
      aw2 = aw
      
! now declare:

      allocate(veght(0:al2+1,0:aw2+1))
      allocate(bldht(0:al2+1,0:aw2+1))      
      allocate(surf_shade(0:al2+1,0:aw2+1,0:bh+1))
      allocate(veg_shade(0:al2+1,0:aw2+1,0:bh+1))
      allocate(surf(1:al2,1:aw2,0:bh,1:6)) !! KN, added new f dimension, vegatation = 6
!      allocate(Uwrite(0:nint(zref-0.5)))
!      allocate(Twrite(0:nint(zref-0.5)))

      do x=0,al+1
       do y=0,aw+1
        bldht(x,y)=0
        veght(x,y)=0
       enddo
      enddo
 
!  here, copy the bldhti array to bldht
!  then deallocate bldhti array
      do y=1,aw2
       do x=1,al2
        bldht(x,y)=bldhti(x,y)
        veght(x,y)=veghti(x,y)
       enddo
      enddo


      deallocate(bldhti)
      deallocate(veghti)
      
      
      numsfc=0

!  steps:
!  make surf_shade array from bldht array

!  faces: 1=up, 2=north, 3=east, 4=south, 5=west (subject to
!  rotation up to 90 degrees, of course)

!  general conversion from building height array to shading
!  array of cells; true=street&building interior; false=ambient air:
      do z=0,bh+1
       do y=0,aw2+1
        do x=0,al2+1
         surf_shade(x,y,z)=.false.
         if (bldht(x,y).ge.z) surf_shade(x,y,z)=.true.
         if (veght(x,y).gt.z) then
            !print *,x,y,z,veght(x,y),'veg shade'
            veg_shade(x,y,z)=.true.
         endif
        enddo
       enddo
      enddo
      
!  print *,'veg_shade array'
!  do i=15,19
!      do j=15,19
!          print *,i,j,veg_shade(i,j,0)
!      end do
!  end do    

!  general conversion from shading array to surface
!  array (no parameter values yet though):

!  initialize array to contain no faces; surf=T means it is a surface patch,
!  surf=F means it is nothing (e.g. border between 2 building interior
!  cells or border between 2 ambient air cells)
      do f=1,5
       do z=0,bh
        do y=1,aw2
         do x=1,al2
          surf(x,y,z,f)=.false.
         enddo
        enddo
       enddo
      enddo
!  streets
      z=0
      do y=1,aw2
       do x=1,al2
        surf(x,y,z,1)=.true.
        if (bldht(x,y).gt.0) then
         surf(x,y,z,1)=.false.
        else
         numsfc=numsfc+1
        endif
       enddo
      enddo
!  roofs and walls
      f=1
      do z=1,bh
       do y=1,aw2
        do x=1,al2
         if(surf_shade(x,y,z))then
          f=1
          if (.not.surf_shade(x,y,z+1).or.z.eq.bh) then
           surf(x,y,z,f)=.true.
           numsfc=numsfc+1
          endif
          f=2
          if ((.not.surf_shade(x,y+1,z)).and.(y.ne.aw)) then
           surf(x,y,z,f)=.true.
           numsfc=numsfc+1 
          endif
          f=3
          if (.not.(surf_shade(x+1,y,z)).and.(x.ne.al)) then
           surf(x,y,z,f)=.true.
           numsfc=numsfc+1 
          endif
          f=4
          if ((.not.surf_shade(x,y-1,z)).and.(y.ne.1)) then
           surf(x,y,z,f)=.true.
           numsfc=numsfc+1
          endif
          f=5
          if ((.not.surf_shade(x-1,y,z)).and.(x.ne.1)) then
           surf(x,y,z,f)=.true.
           numsfc=numsfc+1
          endif
         endif
        enddo
       enddo
      enddo

      write(6,*)'------------------------------------------'
      write(6,*)'number of patches (domain) = ',numsfc
      numsfc_ab = (a2-a1+1)*(b2-b1+1)+bh*2*(bl+bw)
      write(6,*)'number of patches (central urban unit) = ',numsfc_ab


      allocate(sfc_ab(numsfc_ab,par_ab))
      allocate(sfc(numsfc,par))
      allocate(ind_ab(numsfc))
      allocate(vffile(numsfc_ab))
      allocate(vfppos(numsfc_ab+1))
      allocate(vfipos(numsfc_ab+1))
      allocate(mend(numsfc_ab))
      allocate(refl_emist(numsfc_ab))
      allocate(absbs(numsfc_ab))
      allocate(absbl(numsfc_ab))
      allocate(refls(numsfc_ab))
      allocate(refll(numsfc_ab))
      allocate(reflts(numsfc_ab))
      allocate(refltl(numsfc_ab))
      allocate(reflps(numsfc_ab))
      allocate(reflpl(numsfc_ab))
      allocate(Tsfc(numsfc_ab))
      allocate(Trad(numsfc_ab))
!      allocate(lambda_sfc(numsfc_ab))
!      allocate(Qh(numsfc_ab))


!  SFC_AB ARRAY (second dimension) - central urban unit; only patches to have 'history'
!   1: i (sfc array)
!   2: f (sfc array)
!   3: z (sfc array)
!   4: y (sfc array)
!   5: x (sfc array)
!NOLONGERc   6 to 5+numlayers: layer temperatures (starting with layer closest to surface)
!NOLONGERc   5+numlayers+1 to 5+2*numlayers: layer thermal conductivities (avg)
!NOLONGERc   5+2*numlayers+1 to 5+3*numlayers: layer heat capacities
!NOLONGERc   5+3*numlayers+1 to 5+4*numlayers: layer thicknesses

!  SFC ARRAY (second dimension) - all patches      in the domain
!   1: surface type (1=roof,2=street,3=wall)
!   2: sunlit fraction (0 to 4 out of 4)
!   3: albedo
!   4: emissivity
!   5: environment view factor (1-SVF)
!   6: component of surface's normal vector pointing in x-direction
!   7: component of surface's normal vector pointing in y-direction
!   8: component of surface's normal vector pointing in z-direction
!   9: 0-not in initial array, 1-in initial input array, 2-in area
!       of interest for calculations (generally where output will come
!       from
!   10: x-value of patch center
!   11: y-value of patch center
!   12: z-value of patch center

! Layer depths for three sfcs
!      depthr(1)=thickr(1)/2.
!      thick_totr=thickr(1)
!      do k=2,numlayers
!       depthr(k)=depthr(k-1)+(thickr(k-1)+thickr(k))/2.
!       thick_totr=thick_totr+thickr(k)
!      enddo
!      depths(1)=thicks(1)/2.
!      thick_tots=thicks(1)
!      do k=2,numlayers
!       depths(k)=depths(k-1)+(thicks(k-1)+thicks(k))/2.
!       thick_tots=thick_tots+thicks(k)
!      enddo
!      depthw(1)=thickw(1)/2.
!      thick_totw=thickw(1)
!      do k=2,numlayers
!       depthw(k)=depthw(k-1)+(thickw(k-1)+thickw(k))/2.
!       thick_totw=thick_totw+thickw(k)
!      enddo

! building + street widths in each horizontal dimension
      wavelenx=real(bl+sw)
      waveleny=real(bw+sw2)

      iij=1

!  POPULATE THE MAIN PARAMETER ARRAY (SFC)
!  ideally this would be up with the initial surf array assignment
!  to reduce looping, but the sfc array is not defined yet at that
!  point
      numroof2=0
      numstreet2=0
      numwall2=0
      numNwall2=0
      numSwall2=0
      numEwall2=0
      numWwall2=0
      i=0
      iab=0
      sfc(1,9)=0.
      avg_cnt=real((a2-a1+1)*(b2-b1+1))
!      canyair=canyair/real(avg_cnt)/(1.-lambdapR)
      do f=1,5
       do z=0,bh
        do y=1,aw2
         do x=1,al2
          if(surf(x,y,z,f))then

          i=i+1

           sfc(i,9)=1.

!  if the patch is in the central urban unit, sfc(i,9)=2.
          if(x.ge.a1.and.x.le.a2.and.y.ge.b1.and.y.le.b2) then

           iab=iab+1
           sfc(i,9)=2.
           sfc_ab(iab,1)=i
           sfc_ab(iab,2)=f
           sfc_ab(iab,3)=z
           sfc_ab(iab,4)=y
           sfc_ab(iab,5)=x
          endif

!  set the roof, wall, and road albedos and emissivities
!  and temperatures
          if (f.eq.1.and.z.eq.0) then
           sfc(i,1)=2.
           sfc(i,3)=albs
           sfc(i,4)=emiss
           if (emiss_var) then
            if (amod(real(x)-0.5,wavelenx).gt.real(bl)) then
             if (amod(real(y)-0.5,waveleny).gt.real(bw)) then
              sfc(i,4)=emissInter
             else
              sfc(i,4)=emissNS
             endif
            else
             sfc(i,4)=emissEW
            endif
           endif
!cc testing
!           write(93,*)x,y,sfc(i,4),wavelenx,real(bl)
!cc
           sfc(i,6)=0.
           sfc(i,7)=0.
           sfc(i,8)=1.
           if(sfc(i,9).gt.1.5) then
            numstreet2=numstreet2+1
!            do k=1,numlayers
!             sfc_ab(iab,k+3*numlayers+5)=thicks(k)
!             sfc_ab(iab,k+numlayers+5)=lambdaavs(k)
!             sfc_ab(iab,k+2*numlayers+5)=htcaps(k)
!            enddo
!            lambda_sfc(iab)=lambdas(1)
            Tsfc(iab)=Tsfcs
           endif
          elseif (f.eq.1.and.z.gt.0) then
           sfc(i,1)=1.
           sfc(i,3)=albr
           sfc(i,4)=emisr
           sfc(i,6)=0.
           sfc(i,7)=0.
           sfc(i,8)=1.
           if(sfc(i,9).gt.1.5) then
            numroof2=numroof2+1
!            do k=1,numlayers
!             sfc_ab(iab,k+3*numlayers+5)=thickr(k)
!             sfc_ab(iab,k+numlayers+5)=lambdaavr(k)
!             sfc_ab(iab,k+2*numlayers+5)=htcapr(k)
!            enddo
!            lambda_sfc(iab)=lambdar(1)
            Tsfc(iab)=Tsfcr
           endif
          else
           sfc(i,1)=3.
           sfc(i,3)=albw
           sfc(i,4)=emisw
           sfc(i,8)=0.
            if(sfc(i,9).gt.1.5) then
             numwall2=numwall2+1
!             do k=1,numlayers
!              sfc_ab(iab,k+3*numlayers+5)=thickw(k)
!              sfc_ab(iab,k+numlayers+5)=lambdaavw(k)
!              sfc_ab(iab,k+2*numlayers+5)=htcapw(k)
!             enddo
             Tsfc(iab)=Tsfcw
!             lambda_sfc(iab)=lambdaw(1)
            endif
           if(f.eq.2) then
            if(sfc(i,9).gt.1.5) numNwall2=numNwall2+1
             sfc(i,6)=0.
             sfc(i,7)=1.
           elseif (f.eq.3) then
            if(sfc(i,9).gt.1.5) numEwall2=numEwall2+1
             sfc(i,6)=1.
             sfc(i,7)=0.
           elseif (f.eq.4) then
            if(sfc(i,9).gt.1.5) numSwall2=numSwall2+1
             sfc(i,6)=0.
             sfc(i,7)=-1.
           elseif (f.eq.5) then
            if(sfc(i,9).gt.1.5) numWwall2=numWwall2+1
             sfc(i,6)=-1.
             sfc(i,7)=0.
           else
            write(6,*)'PROBL w/ sfc(i, ) assignment'
            stop
           endif
          endif
          endif
         enddo
        enddo
       enddo
      enddo

      numsfc2=iab
      if (numsfc2.ne.numsfc_ab) then
       write(6,*)'number of patches in the central urban unit incorrect'
       write(6,*)'numsfc2.ne.numsfc_ab',numsfc2,numsfc_ab
       stop
      endif


! Match each patch not in the central urban unit with its corresponding patch
! in the central urban unit. Its temperature will then evolve according to that
! patch in the central urban unit. This is the optimization that allows this
! version of the model to run much more quickly. 
      i=0
      do f=1,5
       do z=0,bh
        do y=1,aw2
         do x=1,al2
          if(surf(x,y,z,f))then
           i=i+1
           do iab=1,numsfc2
            if(f.eq.sfc_ab(iab,2)) then
             if(z.eq.sfc_ab(iab,3)) then     
            if(mod(real(abs(y-sfc_ab(iab,4))),waveleny).lt.0.0001) then
            if(mod(real(abs(x-sfc_ab(iab,5))),wavelenx).lt.0.0001) then
               ind_ab(i)=iab
               goto 329
               endif
              endif
             endif
            endif
           enddo
           write(6,*)'an i did not find an iab,i=',i
           stop
 329       continue
          endif
         enddo
        enddo
       enddo
      enddo

      numroof2=max(1,numroof2)
      numstreet2=max(1,numstreet2)
      numwall2=max(1,numwall2)
      numNwall2=max(1,numNwall2)
      numSwall2=max(1,numSwall2)
      numEwall2=max(1,numEwall2)
      numWwall2=max(1,numWwall2)

!  For roof heat transfer - find average roof length
      HW_avg2 = real(numwall2)/real(numstreet2)/2.
!      Lroof=zH/patchlen/HW_avg2*lambdapR/(1.-lambdapR)*2.

      lambdac=real(numwall2+numroof2+numstreet2)/real(numroof2+numstreet2)

!  from Macdonald, displacement height:
!      zd=zH*(1.+4.43**(-(lambdapR+lpactual)/2.)
!     & *((lambdapR+lpactual)/2.-1.))

!  frontal index:
!      if(lambdaf.lt.0.0.or.calclf) then
!       lambdaf=real(numwall2)/4./real(numstreet2+numroof2)
!       calclf=.true.
!       write(6,*)'lambdaf will be calculated by the model = ',lambdaf
!      endif

! z0:
!      if(calcz0) then
! Macdonald's method for z0 (estimate lambdaf for now - both it and z0
! will be calculated for all future timesteps)
!       z0=zH*(1.-zd/zH)*exp(-(0.5*1.2/(0.4)**2
!     &*(1.-zd/zH)*lambdaf)**(-0.5))
!       write(6,*)'domain z0 will be calculated by the model (m) = ',z0
!      endif

! for radiation (multiple refl by atm of sfc reflected solar back to sfc)
! (just an estimate - it has only a minor impact, and will be replaced with
! the actual overall surface albedo after the first timestep)
      alb_sfc=(albr*real(numroof2)+albs*real(numstreet2))/real(numroof2+numstreet2)

      vftot5=0.

!      write(6,*)'zH,zd,z0 = ',zH,zd,z0
!      write(6,*)'lambdap,lambdac,lambdaf = ',lambdapR,lambdac,lambdaf
      write(6,*)'lambdap,lambdac = ',lambdapR,lambdac
      write(6,*)'H/L, H/W ratios = ',bhblactual,hwactual                                         

!      write(inputsStoreOut,*)'zH,zd,z0,lambdapR,lambdac,lambdaf'
!      write(inputsStoreOut,*)zH,zd,z0,lambdapR,lambdac,lambdaf
!      write(inputsStoreOut,*)'Lroof,HW_avg2,al2,aw2'
!      write(inputsStoreOut,*)Lroof,HW_avg2,al2,aw2
      write(inputsStoreOut,*)'lambdapR,lambdac'
      write(inputsStoreOut,*)lambdapR,lambdac
      write(inputsStoreOut,*)'HW_avg2,al2,aw2'
      write(inputsStoreOut,*)HW_avg2,al2,aw2


! direction vectors
      do k=1,5
       fx(k)=0.
       fy(k)=0.
       fz(k)=0.
      enddo
      fx(3)=0.5
      fx(5)=-0.5
      fy(2)=0.5
      fy(4)=-0.5
      fz(1)=0.5

      i=0
      DO f=1,5
       DO Z=0,BH
        DO Y=1,aw2
         DO X=1,al2
       
          if(.not.surf(x,y,z,f)) goto 284
           i=i+1

!  patch i surface center:
           sfc(i,10)=real(x) + fx(f)
           sfc(i,11)=real(y) + fy(f)
           sfc(i,12)=real(z) + fz(f)

 284      continue
         enddo
        enddo
       enddo
      enddo

      write(6,*)'------------------------------------------'
!------------------------------------------------------------------
!  View Factor Calculations (or read in from file)

      if (vfcalc.eq.0) then

!  must read in vffile(i),sfc(i,5),vfipos(i),mend(i) etc
!  from file if view factors are already calculated and stored
!  in files
       open(unit=vfinfoDat,file='vfinfo.dat',access='DIRECT',recl=vfinfoDatRECL) !! KN changed this from 32 to 64
       read(unit=vfinfoDat,rec=1)numfiles,numvf
       do iab=1,numsfc2
        i=sfc_ab(iab,1)
       read(unit=vfinfoDat,rec=iab+1)vffile(iab),vfipos(iab),mend(iab),sfc(i,5),sfc(i,10),sfc(i,11),sfc(i,12)
       enddo
       read(unit=vfinfoDat,rec=numsfc2+2)vfipos(numsfc2+1)
       close(vfinfoDat)

      else

      write(6,*)'CALCULATING VIEW FACTORS...'

      allocate(vf2(numsfc*numsfc2))
      allocate(vf2j(numsfc*numsfc2))


      X=0
      Y=0
      Z=0
      numvf=0

! direction vectors
      do k=1,5
       fx(k)=0.
       fy(k)=0.
       fz(k)=0.
      enddo
       fx(3)=0.5
       fx(5)=-0.5
       fy(2)=0.5
       fy(4)=-0.5
       fz(1)=0.5

      do k=1,5
       fxx(k)=fx(k)
       fyy(k)=fy(k)
       fzz(k)=fz(k)
      enddo

      i=0

      fact2=500000.

      n=11
      write(numc,'(i1)')n-10
      open(unit=n,file='vf'//numc,access='DIRECT',recl=vfRECL)
      m=1
      p=1
      iab=0
           
      do k=1,numsfc2
       mend(k)=0.
      enddo

! RUN THROUGH ALL ARRAY POSITIONS AND ONLY PERFORM
! CALCULATIONS ON POINTS SEEN
      DO f=1,5
       if (f.eq.1) then
      write(6,*)'calculating view factors of horizontal patches...'
       elseif (f.eq.2) then
      write(6,*)'calculating view factors of north-facing patches...'
       elseif (f.eq.3) then
        write(6,*)'calculating view factors of east-facing patches...'
       elseif (f.eq.4) then
        write(6,*)'calculating view factors of south-facing patches...'
       elseif (f.eq.5) then
      write(6,*)'calculating view factors of west-facing patches...'
       endif
       DO Z=0,BH
        DO Y=b1,b2
         DO X=a1,a2
       
          if(.not.surf(x,y,z,f)) goto 41

           iab=iab+1
           i=sfc_ab(iab,1)

!  patch surface i center:
                vx = real(x) + fx(f)
            vy = real(y) + fy(f)
            vz = real(z) + fz(f)

          vftot=0.

          j=0

        if (m.gt.fact2)then
           mend(iab-1)=m-1
         m=1
           close(unit=n)
         n=n+1
         if(n.le.19) then
          write(numc,'(i1)')n-10
            open(unit=n,file='vf'//numc,access='DIRECT',recl=vfRECL)
         elseif(n.ge.20.and.n.le.109) then
          write(numc2,'(i2)')n-10
            open(unit=n,file='vf'//numc2,access='DIRECT',recl=vfRECL)
         else
          write(6,*)'need to program in more vf files or increase # vfs'
          write(6,*)'each one can hold'
          stop
         endif
        endif

        vffile(iab)=n
        vfipos(iab)=m

          vfppos(iab)=p

          do ff=1,5
           do zz=0,bh
            do yy=1,aw2
             do xx=1,al2

              if(.not.surf(xx,yy,zz,ff)) goto 81

              vfsum=0.0

            j=j+1

!  patch surface j center:
            vxx = real(xx) + fxx(ff)
            vyy = real(yy) + fyy(ff)
            vzz = real(zz) + fzz(ff)

                dx(1)=abs(vx-vxx)
                dx(2)=abs(vy-vyy)
                dx(3)=abs(vz-vzz)
                separat=sqrt((dx(1))**2+(dx(2))**2+(dx(3))**2)

!  a surface cannot see itself (no concave surfaces, only flat)
!  also, ray tracing (function ray) to determine if the 2 sfcs can
!  see each other

              if(i.eq.j.or..not.ray(x,y,z,f,xx,yy,zz,ff,surf_shade,fx,fy,fz,fxx,fyy,fzz,al2,aw2,maxbh)) then

             vf=0.

             goto 81
          
            else

               if(vfcalc.eq.1) then
!  calculation of exact view factors for PLANE PARALLEL facets ONLY
                vecti(1)=dble(sfc(i,6))
                vecti(2)=dble(sfc(i,7))
                vecti(3)=dble(sfc(i,8))
                vectj(1)=dble(sfc(j,6))
                vectj(2)=dble(sfc(j,7))
                vectj(3)=dble(sfc(j,8))

                 call dotpro(vecti,vectj,3,dp,g)

                if(abs(dp).lt.0.0001)then
!  perpendicular
                   z1=0.
                   y1=0.
!  patch separation distances in the three dimensions:
                   do k=1,3
                    z1=z1+abs(real(vecti(k))*dx(k))
                    y1=y1+abs(real(vectj(k))*dx(k))
!                    if(real(vecti(k)).eq.0.0.and.real(vectj(k)).eq.
!     &0.0) x2=dx(k)
                   enddo
                   x2=dx(1)+dx(2)+dx(3)-z1-y1
                  if(x2.lt.0.1) then
! use F7, the patches are aligned in one dimension
                   vf=real(F7(dble(1.0),dble(y1-0.5),dble(1.0),dble(z1-0.5),dble(1.0)))
                  else
! use F9, the patches aren't aligned in any of the three dimensions
!  subtract 0.5 or 1 to get distance to patch edge instead of patch center
                   vf=real(F9(dble(1.0),dble(x2-1.0),dble(1.0),dble(y1-0.50),dble(1.0),dble(z1-0.5),dble(1.0)))
                  endif
                else
!  parallel
                  x2=0.
!  patch separation distances in the three dimensions:
                  do k=1,3
                   if(abs(real(vecti(k))).lt.0.1) then
                    if(x2.eq.0.0) x2=dx(k)+0.1
                    z2=dx(k)
                   endif
                  enddo
                  x2=x2-0.1
                  yyy=dx(1)+dx(2)+dx(3)-x2-z2

                  if(z2.eq.0.0.or.x2.eq.0.0) then
! use F3, the patches are aligned in one dimension, or pll if they are
! directly opposite
                   vf=real(pll(dble(1.0),dble(yyy),dble(1.0)))
              if(z2+x2.gt.0.1) vf=real(F3(dble(1.0),dble(yyy),dble(1.0),dble(max(x2,z2)-1.0),dble(1.0)))
                  else
! use F5, the patches aren't aligned in any of the three dimensions
                 vf=real(F5(dble(1.0),dble(x2-1.0),dble(1.0),dble(yyy),dble(1.0),dble(z2-1.0),dble(1.0)))
                  endif
                endif

                if(vf.gt.1.0.or.vf.lt.0.0) then
                 write(6,*)'vfprobexact',i,j,vf
                 stop
                endif

                vf=abs(vf)

               else
! calculate normal vector of cell face (patch) i
! to get a positive answer, the progression of corner points around the patch should be CLOCKWISE

                vns(1) = dble(fx(f))
                vns(2) = dble(fy(f))
                vns(3) = dble(fz(f))
                magvns = sqrt(vns(1)**2+vns(2)**2+vns(3)**2)     

! normalize the normal vector from point i
                vns(1) = vns(1)/magvns
                vns(2) = vns(2)/magvns
                vns(3) = vns(3)/magvns            
! Find polygon corners for patch j and define the vectors to these vertices
! These are contained in the array v(iv,k) with k=1,3 corresponding to x,y,z respectively
! loop through the four corner points of this patch     
                do iv=1,4
                 xpinc=-0.5
                 ypinc=-0.5
                 if ((iv.ge.2).and.(iv.le.3)) xpinc=0.5 
                 if ((iv.ge.1).and.(iv.le.2)) ypinc=0.5
! set corner points                   
                 if (ff.eq.1)then
                  corner(iv,1)=dble(vxx)+dble(xpinc)
                  corner(iv,2)=dble(vyy)+dble(ypinc)
                  corner(iv,3)=dble(vzz)
             elseif (ff.eq.2)then
                  corner(iv,1)=dble(vxx)-dble(xpinc)
                  corner(iv,2)=dble(vyy)
                  corner(iv,3)=dble(vzz)+dble(ypinc)
             elseif (ff.eq.4)then
                  corner(iv,1)=dble(vxx)+dble(xpinc)
                  corner(iv,2)=dble(vyy)
                  corner(iv,3)=dble(vzz)+dble(ypinc)
             elseif (ff.eq.3)then
                  corner(iv,1)=dble(vxx)
                  corner(iv,2)=dble(vyy)+dble(xpinc)
                  corner(iv,3)=dble(vzz)+dble(ypinc)
             elseif (ff.eq.5)then
                  corner(iv,1)=dble(vxx)
                  corner(iv,2)=dble(vyy)-dble(xpinc)
                  corner(iv,3)=dble(vzz)+dble(ypinc)
             else
              write(6,*)'PROBLEM, ff not properly defined'
              stop
             endif

! set vector between point i and corner point of j
                 v(iv,1)=corner(iv,1)-dble(vx)
                 v(iv,2)=corner(iv,2)-dble(vy)
                 v(iv,3)=corner(iv,3)-dble(vz)
                 vmag(iv)=sqrt(v(iv,1)**2+v(iv,2)**2+v(iv,3)**2)                     
!                 write(*,*) 'v(iv,1-3) ',v(iv,1),v(iv,2),v(iv,3)
                enddo                   


! The following section is common for different surfaces.                  
! Find angles between the vectors using the dot product rule: cos angle = u dot v / |u||v|
! dotproducts are: x1*x2+y1*y2+z1*z2 where x1,y1,z1 and x2,y2,z2 are two vectors

! Calculate the cross products between the vectors. This defines a vector normal
! to the plane between the two vectors as required by the view factor calculation.
                do iv=1,4
                  do k=1,3
                    vtemp1(k)=v(iv,k)
                    if (iv.le.3) then
                      vtemp2(k)=v(iv+1,k)
                    else
                      vtemp2(k)=v(1,k)
                    endif
                  enddo

! Use the dot product to define the angle between vectors between two adjacent corners of the patch
                  call dotpro(vtemp1,vtemp2,3,dp,g)
                  vangle(iv)=g 
!                if ((x.eq.164).and.(y.eq.230).and.(z.eq.9)) then
!                  write(*,*) 'ic iv vangle(iv) ',ic,iv,vangle(iv)
!                endif                  
! Now do the cross product
                  call crosspro(vtemp1,vtemp2,3,cp,mcp)
                  if (mcp.le.0) then
                    write(*,*) 'warning: mcp <=0'
                    write(*,*) 'x y z ',x,y,z                    
                  endif
!                if ((x.eq.164).and.(y.eq.230).and.(z.eq.9)) then
!                  write(*,*) 'crossproduct iv ',iv,(cp(k),k=1,3)
!                endif                                    
! normalize the cross product by the |vtemp1 x vtemp2|
                  do k=1,3 
                    cp(k)=cp(k)/mcp
                  enddo
                                      
! Now find the dot product of the vector normal to the plane between the two vectors and the vector
! normal to the plane of surface i. 
                  call dotpro(cp,vns,3,dp,g) 

! Here is the view factor definition (as per Ashdown 1994, eqn 5.6; see also Baum et al. 1989, and
! Hottel and Sarofim 1967)                   
                  vfsum=vfsum+dp*vangle(iv)
                enddo
! Here is the view factor for this patch (can use this to test for limits); this is the "normal"
! view factor definition (i.e. for an entire hemisphere).
! The direction in which the corner points are processed matters - it yields a positive or
! negative number. Convert negatives to positives. 
                vf=real(vfsum)/(2.*pi)

! Taking absolute sum is necessary because relative progression around patch is sometimes clockwise (+)
! and sometimes counterclockwise (-) depending on relative position of the patches
! These could be pre-defined, but probably easier to take absolute sum here.
                if (vf.lt.0) vf=abs(vf)

!  exact or contour integration view factors 'if'
               endif

!  whether or not patch i sees patch j 'if'
            endif
            
            if (vf.gt.0.)then
             write(unit=n,rec=m)ind_ab(j),vf
               vftot5=vftot5+vf
               numvf=numvf+1
               vf2(p)=vf
               vf2j(p)=ind_ab(j)
               p=p+1
             m=m+1
            endif

              vftot=vftot+vf

 81           continue
              enddo
             xx=1
             enddo
            xx=1
            yy=1
            enddo
           xx=1
           yy=1
           zz=0
           enddo

           sfc(i,5)=vftot
 41       continue
          enddo
         x=a1
         enddo
        x=a1
        y=b1
        enddo
       X=a1
       y=b1
       z=0
       enddo

      vfipos(numsfc2+1) = m
      vfppos(numsfc2+1) = p
      write(6,*)'total number of inter-patch view factors = ',numvf

!  close files
      close(unit=n)
      do k=n+1,numfiles+10
       close(unit=k)
      enddo
      numfiles=n-10


      allocate(vf3(numvf))
      allocate(vf3j(numvf))


! arrays of view factors
      do k=1,numvf
       vf3(k)=vf2(k)
       vf3j(k)=vf2j(k)
      enddo


      deallocate(vf2)
      deallocate(vf2j)


      if(iab.ne.numsfc2) then
       write(6,*)'number of surfaces in view factor calculation wrong'
       write(6,*)'PROB w/ numsfc2: iab, numsfc2 =',iab,numsfc2
       stop
      endif

!  write file so that view factors need not be recomputed
       open(unit=vfinfoDat,file='vfinfo.dat',access='DIRECT',recl=vfinfoDatRECL)
       write(unit=vfinfoDat,rec=1)numfiles,numvf
       do iab=1,numsfc2
        i=sfc_ab(iab,1)
        write(unit=vfinfoDat,rec=iab+1)vffile(iab),vfipos(iab),mend(iab),sfc(i,5),sfc(i,10),sfc(i,11),sfc(i,12)
       enddo
       write(unit=vfinfoDat,rec=numsfc2+2)vfipos(numsfc2+1)
       close(vfinfoDat)

      endif

!------------------------------------------------------------------

      if(vfcalc.eq.0) then


      allocate(vf3(numvf))
      allocate(vf3j(numvf))


      p=1

!  open view factor files
       do iab=1,numsfc2
        vfppos(iab)=p
        if(iab.eq.1.or.vffile(iab).ne.vffile(iab-1))then
         close(unit=vf1Dat)
         if(vffile(iab).lt.20)then
          write(numc,'(i1)')(vffile(iab)-10)
          open(unit=vf1Dat,file='vf'//numc,access='DIRECT',recl=vfRECL)
         elseif(vffile(iab).lt.110)then
          write(numc2,'(i2)')(vffile(iab)-10)
          open(unit=vf1Dat,file='vf'//numc2,access='DIRECT',recl=vfRECL)
         elseif(vffile(iab).lt.1010)then
          write(numc3,'(i3)')(vffile(iab)-10)
          open(unit=vf1Dat,file='vf'//numc3,access='DIRECT',recl=vfRECL)
         else
          write(6,*)'TOO MANY VF FILES'
          stop
         endif
        endif

        if(vfipos(iab+1).eq.1)then
         vfiend=mend(iab)
        else
         vfiend=vfipos(iab+1)-1
        endif

! write the view factors into memory
        do q=vfipos(iab),vfiend
         read(unit=vf1Dat,rec=q)j,vf
         vf3(p)=vf
         vf3j(p)=j
         vftot5=vftot5+vf
         p=p+1
          if(vf.gt.1.0.or.vf.lt.0.0)then
           write(6,*)'VF PROB'
           stop
          endif
        enddo

       enddo

       vfppos(numsfc2+1)=p

      close(unit=vf1Dat)

      endif

      if (numvf.ne.p-1) then
       write(6,*)'PROBLEM WITH VFs IN MEM',p-1,numvf
      endif


! facet statistical output arrays:
!      allocate(sdT(1:numout,1:numstreet2+1))
!      allocate(sdN(1:numout,1:numNwall2+1))
!      allocate(sdS(1:numout,1:numSwall2+1))
!      allocate(sdE(1:numout,1:numEwall2+1))
!      allocate(sdW(1:numout,1:numWwall2+1))



! -----------------------------------
! Latitude and street orientation loops (because geometry and view factors
! need not be re-computed for new latitudes or street orientations)
! New forcing data may be advisable for new latitudes, however

      xlat=xlat_in
      do while (xlat.le.xlatmax)

      htmean_wall_HrStr=0.
      htmean_HrStr=0.
      Kdn_road_HrStr=0.
      Kdn_wall_HrStr=0.
      Kdn_roof_HrStr=0.
      numstror=0

      stror=stror_in
      do while (stror.le.strormax) 

      write(inputsStoreOut,*)'________________________________________'

      timeis=start_time
      timeend=end_time
      write(6,*)'------------------------------------------'
      write(6,*)'simulation start time, end time (h) = ',start_time,end_time

!      starttime=timeis
! number of times output will be written in Matlab output section:
      numout=int((timeend-start_time)/outpt_tm)+min(100,max(1,int(1./outpt_tm)))

      first_write=.true.
!      last_write=.false.



! INTITIAL SUBSTRATE TEMPERATURE PROFILES SUCH THAT Gin=Gout for each layer
! (i.e. a nonlinear initial T profile)
!      do iab=1,numsfc2

!       i=sfc_ab(iab,1)

!  implicit initial T profile assuming Gin=Gout for each
!  layer, based on the input Tsfc and the input Tint/Tg

! roofs and walls
!       Tint=Tintw
! streets
!       if (abs(sfc(i,1)-2.).lt.0.5) Tint=Tints

!  first calculate the thermal conductivities between layer centers by adding
!  thermal conductivities (or resistivities) in series

!       do k=1,numlayers
!        lambdaav(k)=sfc_ab(iab,k+numlayers+5)
!        thick(k)=sfc_ab(iab,k+3*numlayers+5)
!       enddo

!  surface matrix values:
!       lambd_o_thick=lambdaav(1)/(thick(1)+thick(2))
!       A(1)=0.
!       B(1)=2.*((lambd_o_thick)+lambda_sfc(iab)/thick(1))
!       D(1)=-2.*lambd_o_thick
!       R(1)=Tsfc(iab)*lambda_sfc(iab)/thick(1)*2.

!  interior matrix values:
!       do k=2,numlayers-1
!        lambd_o_thick=lambdaav(k-1)/(thick(k-1)+thick(k))
!        lambd_o_thick2=lambdaav(k)/(thick(k)+thick(k+1))
!        A(k)=-2.*lambd_o_thick
!        B(k)=2.*(lambd_o_thick+lambd_o_thick2)
!        D(k)=-2.*lambd_o_thick2
!        R(k)=0.
!       enddo

!  values for conduction between innermost layer and inner air:
!       lambd_o_thick=lambdaav(numlayers-1)/(thick(numlayers-1)
!     &+thick(numlayers))
!       A(numlayers)=-2.*lambd_o_thick
!       B(numlayers)=2.*(lambd_o_thick+lambdaav(numlayers)
!     &/thick(numlayers)*IntCond)
!       D(numlayers)=0.
!       R(numlayers)=2.*lambdaav(numlayers)
!     &*Tint/thick(numlayers)*IntCond

!  TRIDIAGONAL MATRIX SOLUTION FROM JACOBSON, p. 166
!       gam(1)=-D(1)/B(1)
!       tlayer(1)=R(1)/B(1)

!       do k=2,numlayers
!        denom(k)=B(k)+A(k)*gam(k-1)
!        tlayer(k)=(R(k)-A(k)*tlayer(k-1))/denom(k)
!        gam(k)=-D(k)/denom(k)
!       enddo

!       do k=numlayers-1,1,-1
!        tlayer(k)=tlayer(k)+gam(k)*tlayer(k+1)
!       enddo

!       do k=1,numlayers
!        sfc_ab(iab,k+5)=tlayer(k)
!       enddo

!      enddo



! INITIALIZATION BEFORE TIME INTEGRATION

      numabovezH=0
      numcany=0
      do iab=1,numsfc_ab
       if(sfc(i,9).gt.1.5) then
        i=sfc_ab(iab,1)
!        if((sfc(i,12)-0.5)*patchlen.lt.zH-0.01) then
        if((sfc(i,12)-0.5).lt.zH-0.01) then
         numcany=numcany+1
        else
         numabovezH=numabovezH+1
        endif
       endif
      enddo

! initial values:
!      Tcan=Tafrc(1)+273.15+0.5
!      Qhcan=0.
      Tsfc_R=Tsfcr*real(numroof2)

!      rhocan=press*100./287.04/Tcan

! This is the average heat capacity of air per m2 below zH (and only
! for the fraction of the plan area for which building heights < zH)
! for 3-D geometries: (canyair is the height of the air column below
! zH if all the buildings below zH were put into one massive building of height
! much lower than zH, of course - covering the all vertical columns
! up to zH for which building height < zH in the entire area of interest)
! units of Cairavg: J/K/m2/unit square of area for which building height<0 or street
!       Cairavg=canyair*rhocan*cpair

! So that Tcan changes will not be larger than 0.1C (unstable?)
! in one timestep (there are often instabilities in the ICs)...will
! attempt to increase it later on
!      deltat2=0.1*Cairavg*real(avg_cnt)/(real(numcany)*
!     & max(abs(Tcan-Tsfcs),abs(Tcan-Tsfcw))*(7.8+4.2*Ua))
!      deltat_cond=deltat
!      deltat=min(deltat,deltat2)
!      write(6,*)'new time step for Tcan stability (s) = ',deltat

!      timeis=timeis+deltat/3600.

!      timefrc_index=2
      counter2=0
      tim=1


! write out saved surface temperatures for intra-facet statistical calcs:
!      if(facet_out) then
!      open(unit=745,file='Tsfc_street.out',status='unknown',
!     &form='formatted')
!      open(unit=746,file='Tsfc_Nwall.out',status='unknown',
!     &form='formatted')
!      open(unit=747,file='Tsfc_Swall.out',status='unknown',
!     &form='formatted')
!      open(unit=748,file='Tsfc_Ewall.out',status='unknown',
!     &form='formatted')
!      open(unit=749,file='Tsfc_Wwall.out',status='unknown',
!     &form='formatted')

!      write(745,629)'lp=',lpactual,' bh/bl=',real(2*bh)/real(bl+bw),
!     &' h/w=',hwactual,' stror=',stror
!      write(746,629)'lp=',lpactual,' bh/bl=',real(2*bh)/real(bl+bw),
!     &' h/w=',hwactual,' stror=',stror
!      write(747,629)'lp=',lpactual,' bh/bl=',real(2*bh)/real(bl+bw),
!     &' h/w=',hwactual,' stror=',stror
!      write(748,629)'lp=',lpactual,' bh/bl=',real(2*bh)/real(bl+bw),
!     &' h/w=',hwactual,' stror=',stror
!      write(749,629)'lp=',lpactual,' bh/bl=',real(2*bh)/real(bl+bw),
!     &' h/w=',hwactual,' stror=',stror
!      endif

      write(6,*)'------------------------------------------'                   
      write(6,*)'------------------------------------------'
      write(6,*)'lambdap=',lpactual,' H/L=',bhblactual,' lat=',xlat,' stror=',stror           

! plan area in patches
       Aplan=real(numroof2+numstreet2)


!      goto 922

! continue here if changing the time step
! 937  continue

!      timeis=timeis+deltat/3600.

! 922  continue

       ywrite=.true.
       Kdn_tothr=0.
       Kdn_tothr_wall=0.
       do iii=0,bh
        Kdnhr(iii)=0.        
       enddo
       htmean_Kdn_wall_day=0.
       htmean_Kdn_day=0.

! START OF MAIN TIME LOOP----------------------------------------
      do 309 while (timeis.le.timeend)

! try to increase the timestep for the first 2 hours of simulation
! because often the disequilibrium of the ICs causes the above two
! tests to reduce the timestep drastically in the early going
!        if(counter.gt.25) then
!         if(deltat.lt.8.0.and.3.*deltat.lt.deltat_cond) then
!          timeis=timeis-deltat/3600.
!          deltat=deltat*3.
!          write(6,*)'INCREASING TIMESTEP BY 200% TO:',deltat
!          counter=0
!          goto 937
! try to increase the timestep every so often throughout the simulation
! fast increase if the timestep is small:
!         elseif (timeis.lt.starttime+2.0.and.
!     &1.5*deltat.lt.deltat_cond) then
!          timeis=timeis-deltat/3600.
!          deltat=deltat*1.5
!          write(6,*)'INCREASING TIMESTEP BY 50% TO:',deltat
!          counter=0
!          goto 937
!         endif
!        endif
! slower increase otherwise:
!        if(counter.gt.100.and.1.3*deltat.lt.deltat_cond) then
!         timeis=timeis-deltat/3600.
!         counter=0
!          deltat=deltat*1.3
!          write(6,*)'INCREASING TIMESTEP BY 30% TO:',deltat
!         goto 937
!        endif


! INTERPOLATE FORCING DATA

      i=1
      do 335 while (timefrc(i).le.timeis)
       i=i+1
 335  continue

        timefrc_index=min(numfrc+1,i)
        write(6,*)'forcing data from times (h):',timefrc(timefrc_index-1),timefrc(timefrc_index)

        if(frcKdn) then
         Ktotfrc=Kdnfrc(timefrc_index-1)+(timeis-timefrc(timefrc_index-1))/deltatfrc*&
     (Kdnfrc(timefrc_index)-Kdnfrc(timefrc_index-1))
        endif
        Ta=Tafrc(timefrc_index-1)+(timeis-timefrc(timefrc_index-1))/deltatfrc*(Tafrc(timefrc_index)-&
     Tafrc(timefrc_index-1))
        Ta=Ta+273.15
        ea=eafrc(timefrc_index-1)+(timeis-timefrc(timefrc_index-1))/deltatfrc*(eafrc(timefrc_index)-&
     eafrc(timefrc_index-1))
        if(frcLdn) then
         Ldn=Ldnfrc(timefrc_index-1)+(timeis-timefrc(timefrc_index-1))/deltatfrc*(Ldnfrc(timefrc_index)-&
     Ldnfrc(timefrc_index-1))
        else
! Prata's formula (QJRMS 1996)
         Ldn=(1.-(1.+46.5*ea/Ta)*exp(-((1.2+3.*46.5*ea/Ta)**(0.5))))*sigma*Ta**4
         Ldn=Ldn*Ldn_fact
        endif
        Td=(4880.357-29.66*alog(ea))/(19.48-alog(ea))
        if(frcTsfc)then
         Troof=max(0.1,Trooffrc(timefrc_index-1)+(timeis-timefrc(timefrc_index-1))/deltatfrc*&
     (Trooffrc(timefrc_index)-Trooffrc(timefrc_index-1)))
         Troad=Troadfrc(timefrc_index-1)+(timeis-timefrc(timefrc_index-1))/deltatfrc*&
     (Troadfrc(timefrc_index)-Troadfrc(timefrc_index-1))
         Twall=Twallfrc(timefrc_index-1)+(timeis-timefrc(timefrc_index-1))/deltatfrc*&
     (Twallfrc(timefrc_index)-Twallfrc(timefrc_index-1))
        endif


      i=0
      iab=0
      do f=1,5
       do z=0,bh
        do y=1,aw2
         do x=1,al2
          if(surf(x,y,z,f))then

          i=i+1

          if(x.ge.a1.and.x.le.a2.and.y.ge.b1.and.y.le.b2) then
           iab=iab+1

!  set the roof, wall, and road temperatures

!  forced Tsfc
          if(frcTsfc) then
           if (f.eq.1.and.z.eq.0) then
            Tsfc(iab)=Troad
           elseif (f.eq.1.and.z.gt.0) then
            Tsfc(iab)=Troof
           else
            Tsfc(iab)=Twall
           endif
          else
!  constant Tsfc
           if (f.eq.1.and.z.eq.0) then
            Tsfc(iab)=Tsfcs
           elseif (f.eq.1.and.z.gt.0) then
            Tsfc(iab)=Tsfcr
           else
            Tsfc(iab)=Tsfcw
           endif
          endif

          endif

          endif
         enddo
        enddo
       enddo
      enddo


!      Udir=amod(Udir,360.)
! wind direction relative to the domain
!      Udirdom=Udir-stror
!      if(Udirdom.lt.0.) Udirdom=Udir+(360.-stror)

! calculate frontal area index, taking into account the wind direction
!      if(calclf) then
!      if (Udirdom.lt.180.) then
!       if (Udirdom.lt.90.) then
!        lambdaf=(sind(Udirdom)*real(numEwall2)+cosd(Udirdom)
!     &*real(numNwall2))/real(numstreet2+numroof2)
!       else
!        lambdaf=(sind(Udirdom-90.)*real(numSwall2)+cosd(Udirdom-90.)
!     &*real(numEwall2))/real(numstreet2+numroof2)
!       endif
!      else
!       if (Udirdom.lt.270.) then
!        lambdaf=(sind(Udirdom-180.)*real(numWwall2)+cosd(Udirdom-180.)
!     &*real(numSwall2))/real(numstreet2+numroof2)
!       else
!        lambdaf=(sind(Udirdom-270.)*real(numNwall2)+cosd(Udirdom-270.)
!     &*real(numWwall2))/real(numstreet2+numroof2)
!       endif
!      endif
!      endif

!      if(calcz0) then
! Macdonald's method for z0
!       z0=zH*(1.-zd/zH)*exp(-(0.5*1.2/(0.4)**2
!     &*(1.-zd/zH)*lambdaf)**(-0.5))
!      endif

! canyon-atm exchange:
!       call SFC_RI(zref-zH+z0,Ta,Tcan,Ua,Ri)
!       call HTC(Ri,Ua,zref-zH+z0,z0,z0,httc_top,Fh)
!       Tlog_fact=0.74*httc_top*(Tcan-Ta)/vK**2/Fh


! -------------------------------------------
! Solar angle and incoming shortwave (direct & diffuse) routines
       LAT=xlat*pi/180.
       TM=amod(timeis,24.)
       yd_actual=yd+int(timeis/24.)
       yd_actual=mod(yd_actual,365)
! SUNPOS calculates the solar angles
       CALL SUNPOS(yd_actual,TM,LAT,zeni,AZIM,CZ,INOT,CA)
       az=AZIM*180./pi
       zen=zeni*180./pi
       ralt=90.-zen
       Ta_sol=Ta-273.15
       Td_sol=Td-273.15
! CLRSKY accounts for attenuation by and multiple reflection with the atmosphere
! It essentially calculates direct and diffuse shortwave reaching the surface
! There is also a basic cloud parameterization in it
       CALL CLRSKY(CZ,PRESS/10.,zeni,Ta_sol,Td_sol,INOT,Kdir,Kdif,Ktot,CA,yd_actual,&
     alb_sfc,cloudtype,abs_aero,Ktotfrc,DR1F)

       Kdir_NoAtm=INOT*cos(zeni)
       Kdir_Calc=Kdir
       Kdif_Calc=Kdif

! to allow the solar radiation routine to calc solar radiation
! amounts if they are not input
!       if (calcKdn) Ktotfrc=Ktot
!       Kdif=Ktotfrc*Kdif/(Ktot+1.e-9)
!       Kdir=Ktotfrc*Kdir/(Ktot+1.e-9)
       if(frcKdn) then
         if (Ktotfrc.gt.0.) then
! average of solar scheme DF/Ktot and that calculated from the Orgill/Hollands param
          Kdif=(Ktotfrc-DR1F+Ktotfrc*Kdif/(Ktot+1.e-9))/2.
!          Kdif=Ktotfrc-DR1F
!         Kdif=Ktotfrc*Kdif/(Ktot+1.e-9)
          Kdir=Ktotfrc-Kdif
         else
          Kdif=0.
          Kdir=0.
         endif
       endif

       Ktot=Kdir+Kdif

! SO THAT KBEAM (I.E. FLUX DENSITY PERP TO SUN) DOES NOT GET TOO BIG
! FOR LOW SUN ANGLES
! (IN CASE OBSERVED KDN AND CALCULATED KDN DO NOT AGREE EXACTLY)
       if (frcKdn.and.(Kdir-Kdir_Calc)/max(1.e-9,Kdir_Calc).gt.0.15.and.ralt.lt.10.0) then
!       if (.not.calcKdn.and.abs(Kdir-Kdir_Calc)/
!     &max(1.e-9,Kdir_Calc).gt.0.50) then
          Kbeam=min(INOT*Kdir_Calc/max(1.e-9,Kdir_NoAtm),Kdir/max(1.e-9,sind(ralt)))
          Kdir=Kbeam*sind(ralt)
          Kdif=Ktotfrc-Kdir
       else
        Kbeam=Kdir/max(1.e-9,sind(ralt))
        if(Kbeam.gt.1390.) then
         write(6,*)'KBEAM unreasonable; Kbeam,Kdir,ralt,sind(ralt) = ',Kbeam,Kdir,ralt,sind(ralt)
       write(inputsStoreOut,*)'KBEAM unreasonable; Kbeam,Kdir,ralt,sind(ralt) = ',Kbeam,Kdir,ralt,sind(ralt)
         if(Kbeam.gt.1370.0*2.0.or.Ktot.gt.1370.) then
       write(6,*)'KBEAM or KTOT unreasonable; Ktot,Kbeam,Kdir,ralt,sind(ralt) = ',Ktot,Kbeam,Kdir,ralt,sind(ralt)
       write(inputsStoreOut,*)'KBEAM or KTOT unreasonable; Ktot,Kbeam,Kdir,ralt,sind(ralt) = ',Ktot,Kbeam,Kdir,ralt,sind(ralt)
          stop
         endif
        endif
       endif

      if(.not.swv) Ktot=0.

      if(Ktot.gt.1.0E-3) then
!  Solar shading of patches -----------------------------------------
       !call shade(stror,az,ralt,ypos,surf,surf_shade,al2,aw2,maxbh,par,sfc,numsfc,a1,a2,b1,b2,numsfc2,sfc_ab,par_ab)
       call shade(stror,az,ralt,ypos,surf,surf_shade,al2,aw2,maxbh,par,sfc,numsfc,a1,a2,b1,b2,numsfc2,sfc_ab,par_ab,veg_shade,&
                    timeis,yd_actual)
      endif


! CONTINUATION POINT FOR Tsfc-Lup balance iterations (below)--------
! 898  continue

!      Tdiffmax=0.

      do iab=1,numsfc_ab
       absbs(iab)=0.
       refls(iab)=0.
       reflts(iab)=0.
       refltl(iab)=0.
      enddo

!      if (.not.swv) then
! ---------------------------------
!  LONGWAVE ONLY (solar has already been done in previous Tsfc-Lup
!  iteration)
!  RADIATION INITIALIZATION

!  zeroth longwave reflection (i.e. emission)
!      vfsum2=0.

!      do iab=1,numsfc2
!       i=sfc_ab(iab,1)
!       refltl(iab)=0.
!       refll(iab)=sfc(i,4)*sigma*Tsfc(iab)**4
!       absbl(iab)=0.
!       vfsum2=vfsum2+(1.-sfc(i,5))
!      enddo

!      if(abs(vfsum2-real(avg_cnt))/real(avg_cnt).gt.0.05
!     &.and.first_write) then
!       write(6,*)'patch sky view factor sum > 5% inaccurate'
!       write(6,*)'value = ',vfsum2,'should be = ',avg_cnt
!       stop
!      endif
!      if(first_write) then
!       svferror=100.*abs(vfsum2-real(avg_cnt))/real(avg_cnt)
!       if (svferror.gt.svfe_store) svfe_store=svferror
!       write(6,*)'ABSOLUTE VALUE OF RELATIVE SKY VIEW FACTOR ERROR ->',
!     &svferror,'%'
!       write(inputsStoreOut,*)'-----lambdap,H/L,latitude,streetdir',
!     &lpin(lpiter),bh_o_bl(bhiter),xlat,stror,'-----'
!       write(inputsStoreOut,*)'ABSOLUTE VALUE OF RELATIVE SVF ERROR ->',
!     & svferror,'% (for the central urban unit)'
!       write(6,*)'------------------------------------------'
!      endif

!  MULTIPLE REFLECTION
!      Lup=0.
!      Lup_refl=0.
!      Lup_refl_old=0.
!      refldiff=1.1
!      Lup_refl=0.
!      Lemit5=0.
!      k=0

! MAIN reflection loop: does at least 1 longwave
! reflection, and goes until change in overall (1-emis)
! is less than dalb multiplied by a factor that recognizes that there is
! little or no multiple reflection at roof level and above (lambdapR is
! lambdap at roof level)
!      do 313 while (k.lt.2.or.refldiff.ge.dalb*(1.-lambdapR))

!       k=k+1

!  save reflected values from last reflection
!       do iab=1,numsfc2
!        i=sfc_ab(iab,1)
!        reflpl(iab)=refll(iab)
!        refll(iab)=0.
!        if (k.eq.1) then
!         absbl(iab)=sfc(i,4)*(1.-sfc(i,5))*Ldn
!         if(absbl(iab).gt.2000.) write(6,*)'1,iab,absbl(iab)',
!     &iab,absbl(iab)
!         refll(iab)=(1.-sfc(i,4))*(1.-sfc(i,5))*Ldn
!         Lup_refl=Lup_refl-sfc(i,4)
!     &*(1.-sfc(i,5))*sigma*Tsfc(iab)**4
!         Lemit5=Lemit5+sfc(i,4)
!     & *sfc(i,5)*sigma*Tsfc(iab)**4
!         refltl(iab)=0.
!        endif
!       enddo

!  open view factor files
!       do iab=1,numsfc2
!        i=sfc_ab(iab,1)
!        do p=vfppos(iab),vfppos(iab+1)-1
!         vf=vf3(p)
!         jab=vf3j(p)
!         absbl(iab)=absbl(iab)+vf*reflpl(jab)*sfc(i,4)
!         if(absbl(iab).gt.2000.) write(6,*)'2,iab,absbl(iab)',
!     &iab,absbl(iab)
!         refll(iab)=refll(iab)+vf*reflpl(jab)*(1.-sfc(i,4))
!        enddo

!        if(sfc(i,9).gt.1.5) then
!         Lup=Lup+(1.-sfc(i,5))*reflpl(iab)
!         Lup_refl=Lup_refl+(1.-sfc(i,5))*reflpl(iab)
!        endif

!       enddo

!       do iab=1,numsfc2
!        refltl(iab)=refltl(iab)+refll(iab)
!       enddo

!       refldiff=(Lup_refl-Lup_refl_old)/real(avg_cnt)/(Ldn+Lemit5/
!     &real(avg_cnt))

!       Lup_refl_old=Lup_refl

! 313  continue

!      do iab=1,numsfc2
!       i=sfc_ab(iab,1)
!       refltl(iab)=refltl(iab)-sfc(i,5)*refll(iab)
!       absbl(iab)=absbl(iab)+sfc(i,5)*refll(iab)
!      enddo

! ------------------------------------

!      else
!  SOLAR and LONGWAVE (solar has NOT already been done in previous
!  Tsfc-Lup iteration)
!  RADIATION INITIALIZATION

!  the unit vector pointing from the surface towards the sun
      ANGDIF=AZ-stror
      if(ANGDIF.lt.0.) ANGDIF=AZ+(360.-stror)
      angsun(1)=dble(SIND(ANGDIF)*COSD(RALT))
      angsun(2)=dble(COSD(ANGDIF)*COSD(RALT))
      angsun(3)=dble(SIND(RALT))

!  first solar absorption and reflection, and zeroth longwave
!  reflection (i.e. emission)
      solarin=0.
      Kdn_grid=0.
      nKgrid=0
      vfsum2=0.

      do iab=1,numsfc_ab
       i=sfc_ab(iab,1)
       refll(iab)=sfc(i,4)*sigma*Tsfc(iab)**4
       absbl(iab)=0.
       if(first_write) vfsum2=vfsum2+(1.-sfc(i,5))

       if (Ktot.gt.1.0e-3) then
         absbs(iab)=(1.-sfc(i,3))*Kdif*(1.-sfc(i,5))
         refls(iab)=sfc(i,3)*Kdif*(1.-sfc(i,5))
        Kdn_grid=Kdn_grid+Kdif*(1.-sfc(i,5))
        nKgrid=nKgrid+1
        if(sfc(i,9).gt.1.5) solarin=solarin+Kdif*(1.-sfc(i,5))

! if patch is at least partly sunlit:
        if(sfc(i,2).gt.0.5)then
         angsfc(1)=dble(sfc(i,6))
         angsfc(2)=dble(sfc(i,7))
         angsfc(3)=dble(sfc(i,8))
!  if we stay with plane parallel surfaces, the following dot product
!  need only be computed 3-4 times (roof/street plus 2-3 sunlit walls)
         call dotpro(angsun,angsfc,3,dp,g)

          absbs(iab)=absbs(iab)+(1.-sfc(i,3))*Kbeam*cos(real(g))*sfc(i,2)/4.
          refls(iab)=refls(iab)+sfc(i,3)*Kbeam*cos(real(g))*sfc(i,2)/4.

        Kdn_grid=Kdn_grid+Kbeam*cos(real(g))*sfc(i,2)/4.                         

        if(sfc(i,9).gt.1.5) solarin=solarin+Kbeam*cos(real(g))*sfc(i,2)/4.
        endif
       else
        absbs(iab)=0.
        refls(iab)=0.
       endif
!         write(inputsStoreOut,*)'C,reflts(iab),refls(iab)',reflts(iab),refls(iab)
       reflts(iab)=refls(iab)
      enddo

      if(abs(vfsum2-real(avg_cnt))/real(avg_cnt).gt.0.05.and.first_write) then
       write(6,*)'patch sky view factor sum > 5% inaccurate'
       write(6,*)'value = ',vfsum2,'should be = ',avg_cnt
       stop
      endif
      if(first_write) then
       svferror=100.*abs(vfsum2-real(avg_cnt))/real(avg_cnt)
       if (svferror.gt.svfe_store) svfe_store=svferror
       write(6,*)'ABSOLUTE VALUE OF RELATIVE SKY VIEW FACTOR ERROR ->',svferror,'%'
       write(inputsStoreOut,*)'-----lambdap,H/L,latitude,streetdir',lpin(lpiter),bh_o_bl(bhiter),xlat,stror,'-----'
       write(inputsStoreOut,*)'ABSOLUTE VALUE OF RELATIVE SVF ERROR ->',svferror,'% (for the central urban unit)'
       write(6,*)'------------------------------------------'
      endif

! compare input Kdn (wrong due to raster grid causing too many
! or too few patches to be sunlit - representing patches by their center)
! the resolution for only the shading routine could be increased
! to help deal with this problem
      Kdn_grid=Kdn_grid/(real(wavelenx*waveleny))
      if(Kdir+Kdif.gt.0.0) then
       Kdn_diff=Kdn_diff+100.*abs(Kdn_grid-Kdir-Kdif)/(Kdir+Kdif+1.e-9)
       nKdndiff=nKdndiff+1
      endif
      if(abs(Kdn_grid-Kdir-Kdif).gt.Kdn_ae_store) then
        Kdn_ae_store=abs(Kdn_grid-Kdir-Kdif)
        Kdn_re_store=abs(Kdn_grid-Kdir-Kdif)/(Kdir+Kdif+1.e-9)
      endif
      if(abs(Kdn_grid-Kdir-Kdif)/(Kdir+Kdif+1.e-9).gt.0.05.and.abs(Kdn_grid-Kdir-Kdif).gt.10.0) then 
       write(6,*)'ralt=',ralt
       write(6,*)'Received solar radiation does not match incoming, need to increase resolution?'
       write(6,*)'Kdn_grid,Kdir+Kdif =',Kdn_grid,Kdir+Kdif
       write(6,*)'wavelenx,waveleny,Kdir,Kdif',wavelenx,waveleny,Kdir,Kdif
       write(6,*)'nKgrid',nKgrid
       write(inputsStoreOut,*)'-------------------------------------'
       write(inputsStoreOut,*)'TIME, solar elevation = ',timeis,ralt
       write(inputsStoreOut,*)'Received solar radiation does not match incoming, need to increase resolution?'
       write(inputsStoreOut,*)'Kdown (model), Kdown (actual) =',Kdn_grid,Kdir+Kdif
       write(inputsStoreOut,*)'wavelenx,waveleny,Kdir,Kdif',wavelenx,waveleny,Kdir,Kdif
       write(inputsStoreOut,*)'nKgrid',nKgrid
       badKdn=badKdn+1
      endif

!  MULTIPLE REFLECTION
!  do the same number of reflections for both solar and longwave,
!  doing the long- and short-wave reflections together is for
!  efficiency reasons: view factors then only have to be read in
!  once instead of twice
      Kup=0.
      Lup=0.
      Kup_refl=0.
      Lup_refl=0.
      Kup_refl_old=0.
      Lup_refl_old=0.
      refldiff=1.1
      Lup_refl=0.
      Lemit5=0.
      k=0

! MAIN reflection loop: does at least 2 shortwave and 1 longwave
! reflection, and goes until change in both overall albedo and overall (1-emis)
! are less than dalb multiplied by a factor that recognizes that there is
! little or no multiple reflection at roof level and above (lambdapR is
! lambdap at roof level)
      do 314 while (k.lt.2.or.refldiff.ge.dalb*(1.-lambdapR))

       k=k+1

!  save reflected values from last reflection
       do iab=1,numsfc_ab
        i=sfc_ab(iab,1)
        reflps(iab)=refls(iab)
        reflpl(iab)=refll(iab)
        refls(iab)=0.
        refll(iab)=0.
        if (k.eq.1) then
         absbl(iab)=sfc(i,4)*(1.-sfc(i,5))*Ldn
         refll(iab)=(1.-sfc(i,4))*(1.-sfc(i,5))*Ldn
         if(sfc(i,9).gt.1.5) then
          Lup_refl=Lup_refl-sfc(i,4)*(1.-sfc(i,5))*sigma*Tsfc(iab)**4
          Lemit5=Lemit5+sfc(i,4)*sfc(i,5)*sigma*Tsfc(iab)**4
         endif
         refltl(iab)=0.
        endif
       enddo

!  open view factor files
       do iab=1,numsfc2
        i=sfc_ab(iab,1)
        do p=vfppos(iab),vfppos(iab+1)-1
          vf=vf3(p)
          jab=vf3j(p)
          if(jab.lt.1) then
           write(6,*)'jab.lt.1,jab,p,vf,iab,i,f,z,y,x',jab,p,vf,iab,i,sfc_ab(iab,2),sfc_ab(iab,3),&
     sfc_ab(iab,4),sfc_ab(iab,5)
           stop
          endif
          absbs(iab)=absbs(iab)+vf*reflps(jab)*(1.-sfc(i,3))
          refls(iab)=refls(iab)+vf*reflps(jab)*sfc(i,3)
         absbl(iab)=absbl(iab)+vf*reflpl(jab)*sfc(i,4)
         refll(iab)=refll(iab)+vf*reflpl(jab)*(1.-sfc(i,4))
        enddo

        if(sfc(i,9).gt.1.5) then
         Kup=Kup+(1.-sfc(i,5))*reflps(iab)
         Lup=Lup+(1.-sfc(i,5))*reflpl(iab)
         Lup_refl=Lup_refl+(1.-sfc(i,5))*reflpl(iab)
         Kup_refl=Kup_refl+(1.-sfc(i,5))*reflps(iab)
        endif

       enddo

       do iab=1,numsfc2
!         write(inputsStoreOut,*)'B,reflts(iab),refls(iab)',reflts(iab),refls(iab)
        reflts(iab)=reflts(iab)+refls(iab)
        refltl(iab)=refltl(iab)+refll(iab)
       enddo

! parameter that determines whether or not to do another reflection
       refldiff=max((Lup_refl-Lup_refl_old)/real(avg_cnt)/(Ldn+Lemit5/real(avg_cnt)),&
     (Kup_refl-Kup_refl_old)/real(avg_cnt)/max(1.e-9,(Kdir+Kdif)))

       Lup_refl_old=Lup_refl
       Kup_refl_old=Kup_refl

 314  continue

!      solar_refl_done=.true.

!      endif


      alb_sfc=min(albr*lpactual+albs*(1.-lpactual),Kup/real(avg_cnt)/max(1.e-9,(Kdir+Kdif)))

!  remaining reflected radiation is partitioned by assuming that sfcs with
!  larger environmental view factors will absorb an amount of this radiation
!  proportional to their total view of other surfaces (approx.), and the 
!  remainder will leave the system (to the sky)
      do iab=1,numsfc2
       i=sfc_ab(iab,1)
!         write(inputsStoreOut,*)'A,reflts(iab),refls(iab)',reflts(iab),refls(iab)
         reflts(iab)=reflts(iab)-sfc(i,5)*refls(iab)
         absbs(iab)=absbs(iab)+sfc(i,5)*refls(iab)
         refltl(iab)=refltl(iab)-sfc(i,5)*refll(iab)
         absbl(iab)=absbl(iab)+sfc(i,5)*refll(iab)
         Kup=Kup+(1.-sfc(i,5))*refls(iab)
         Lup=Lup+(1.-sfc(i,5))*refll(iab)
        enddo

! -------------------------------------------------------------
! CONVECTION and Tsfc

!      rhoa=press*100./287.04/Ta
!      rhocan=press*100./287.04/Tcan
! this is the average heat capacity of air per m2 below zH
! for 3-D geometries: (canyair is the height of the air column below
! zH if all the buildings were put into one massive building of height
! much lower than zH, of course - covering the entire area of interest)
!       Cairavg=canyair*rhocan*cpair

! momentum transfer, log wind profile
!      Tzd=lambdapR*Tsfc_R/real(numroof2)+(1.-lambdapR)*Tcan
!      call SFC_RI(zref-zd,Ta,Tzd,Ua,Ri)
!      call CD (Ri,zref-zd,z0,z0/moh,cdtown,Fm)
!      ustar=sqrt(cdtown)*Ua
!      Qhcan_kin=max(0.,Qhcan/rhocan/cpair)
!      wstar=(9.806/Tcan*Qhcan_kin*zH)**(1./3.)

! BISECTION METHOD FOR U PROFILE!!!
!      bp=ustar/vK/sqrt(Fm)
!      bm=zH-zd
! The following is what Masson uses (but his model is an area average), so
! I've replaced it with an equivalent 3-D expression
!      bn=-2.*lambdaf/(1.-lambdapR)/4.
!      bq=z0

!      if(ustar/vK*alog((zH-zd)/z0)/sqrt(Fm).gt.Ua.or.
!     &ustar/vK*alog((zH-zd)/z0)/sqrt(Fm)
!     &*exp(-2.*lambdaf/(1.-lambdapR)/4.).gt.ustar/vK
!     &*alog((zH-zd)/z0)/sqrt(Fm)) then
!       write(6,*)'Utop larger than Ua, or Ucan larger than Utop'
!       stop
!      endif

!      CL=0.01
!      CR=CL+0.1
!      FR=bp*exp(-CR*zH)/bm/CR-bp*alog(bm/bq)*(1.-exp(bn))/
!     &(exp(CR*zH)-exp(CR*zH/2.))
!      do 957 while (FR.ge.1.e-20)
!       CR=CR+0.1
!       FR=bp*exp(-CR*zH)/bm/CR-bp*alog(bm/bq)*(1.-exp(bn))/
!     &(exp(CR*zH)-exp(CR*zH/2.))
! 957  continue

!      do 958 while (CR-CL.gt.0.001)
!       Cmid=(CR+CL)/2.
!       Fmid=bp*exp(-Cmid*zH)/bm/Cmid-bp*alog(bm/bq)*(1.-exp(bn))/
!     &(exp(Cmid*zH)-exp(Cmid*zH/2.))
!       if (Fmid.gt.0.) then
!        CL=Cmid
!       elseif (Fmid.lt.0.) then
!        CR=Cmid
!       elseif (Fmid.eq.0.) then
!        Ccan=Cmid
!        goto 959
!       else
!        write(6,*)'problem in bisection method'
!        stop
!       endif
! 958  continue
!       Ccan=(CR+CL)/2.
! 959  continue

! constants for the canyon wind profile (Ccan also)
!       Bcan=bp*exp(-Ccan*zH)/bm/Ccan
!       Acan=-Bcan*exp(Ccan*zH)+bp*alog(bm/bq)

!       do iii=0,nint(zH-0.5)
!        zzz=real(iii)
!        Ucantst=Acan+Bcan*exp(Ccan*zzz)
!        if(Ucantst.gt.Ua.or.Ucantst.lt.0.) then
!         write(6,*)'bad Ucan at z=',zzz,Ucan
!         stop
!        endif
!       enddo

!       do iii=0,nint(zref-0.5)
!        zzz=real(iii)
!        if(zzz.lt.zH) then
!         Uwrite(iii)=Acan+Bcan*exp(Ccan*zzz)
!         Twrite(iii)=Tcan
!        else
!         Uwrite(iii)=ustar/vK*alog((zzz-zd)/z0)/sqrt(Fm)
!        Twrite(iii)=Tcan-Tlog_fact/Uwrite(iii)*(alog((zzz-zH+z0)/z0))**2
!        endif
!       enddo

!      Ucanpy=Acan+Bcan*exp(Ccan*zH/2.)


! Loop throught the patches in the central urban unit and calculate
! net radiation, convection at each patch, and solve the energy balance
!      Tp=0.
      Trad_R=0.
      Trad_T=0.
      Trad_N=0.
      Trad_S=0.
      Trad_E=0.
      Trad_W=0.
!      httcT=0.
!      httcW=0.
!      httcR=0.
      Absbs_W=0.
      Absbl_W=0.
      Emit_W=0.
!      Qg_T=0.
      Rnet_T=0.
!      Qh_T=0.
!      Qg_N=0.
      Rnet_N=0.
!      Qh_N=0.
!      Qg_S=0.
      Rnet_S=0.
!      Qh_S=0.
!      Qg_E=0.
      Rnet_E=0.
!      Qh_E=0.
!      Qg_W=0.
      Rnet_W=0.
!      Qh_W=0.
!      Qg_R=0.
      Rnet_R=0.
!      Qh_R=0.
!      Qg_tot=0.
      Rnet_tot=0.
!      Qh_tot=0.
!      Qhcantmp=0.
!      Qh_abovezH=0.
!      Qanthro=0.
!      Qac=0.
!      Qdeep=0.
      Tsfc_cplt=0.
      Tsfc_bird=0.
      Tsfc_R=0.
      Tsfc_N=0.
      Tsfc_S=0.
      Tsfc_E=0.
      Tsfc_W=0.
      Tsfc_T=0.
!      TTsun=0.
!      TTsh=0.
!      TNsun=0.
!      TNsh=0.
!      TSsun=0.
!      TSsh=0.
!      TEsun=0.
!      TEsh=0.
!      TWsun=0.
!      TWsh=0.
!      numTsun=0
!      numTsh=0
!      numNsun=0
!      numNsh=0
!      numSsun=0
!      numSsh=0
!      numEsun=0
!      numEsh=0
!      numWsun=0
!      numWsh=0
      Kdn_R=0.
      Kup_R=0.
      Ldn_R=0.
      Lup_R=0.
      Kdn_T=0.
      Kup_T=0.
      Ldn_T=0.
      Lup_T=0.
      Kdn_N=0.
      Kup_N=0.
      Ldn_N=0.
      Lup_N=0.
      Kdn_S=0.
      Kup_S=0.
      Ldn_S=0.
      Lup_S=0.
      Kdn_E=0.
      Kup_E=0.
      Ldn_E=0.
      Lup_E=0.
      Kdn_W=0.
      Kup_W=0.
      Ldn_W=0.
      Lup_W=0.
      svf_roof=0.
      svf_road=0.
      svf_north=0.
      svf_south=0.
      svf_east=0.
      svf_west=0.
      do zj=1,bh
       Rnet_Ns(zj)=0.
       Kdn_Ns(zj)=0.
       Kup_Ns(zj)=0.
       Ldn_Ns(zj)=0.
       Lup_Ns(zj)=0.
       svf_norths(zj)=0.
       Rnet_Ss(zj)=0.
       Kdn_Ss(zj)=0.
       Kup_Ss(zj)=0.
       Ldn_Ss(zj)=0.
       Lup_Ss(zj)=0.
       svf_souths(zj)=0.
       Rnet_Es(zj)=0.
       Kdn_Es(zj)=0.
       Kup_Es(zj)=0.
       Ldn_Es(zj)=0.
       Lup_Es(zj)=0.
       svf_easts(zj)=0.
       Rnet_Ws(zj)=0.
       Kdn_Ws(zj)=0.
       Kup_Ws(zj)=0.
       Ldn_Ws(zj)=0.
       Lup_Ws(zj)=0.
       svf_wests(zj)=0.
      enddo

      iij=1

      do iab=1,numsfc2
          i=sfc_ab(iab,1)
          y=sfc_ab(iab,4)
          x=sfc_ab(iab,5)

!           if (sfc(i,1).gt.2.5) then
! WALLS - convection coefficients
!            zwall=(sfc(i,12)-0.5)*patchlen
!            if(zwall.ge.zH) then
!             Ucan=ustar/vK*alog((zwall-zd)/z0)/sqrt(Fm)
!             Ueff=sqrt(Ucan**2+wstar**2)
! for Tconv in Newton's method for Tsfc below:
!             Thorz=Tcan-Tlog_fact/Ucan*(alog((zwall-zH+z0)/z0))**2
!            else
!             Ucan=Acan+Bcan*exp(Ccan*zwall)
!             Ueff=sqrt(Ucan**2+wstar**2)
!            endif
!            httc=rw*(11.8+4.2*Ueff)-4.

!           else

! STREETS & ROOFS - convection coefficients
! use the windspeed 0.5*patchlen above the surface (changed to Harman:
! 0.1*average roof length)

! streets:
!            zhorz=0.1*zH
! roofs:
!            if(sfc(i,1).lt.1.5) then
!             zhorz=min(zref,(sfc(i,12)-0.5+0.1*Lroof)*patchlen)
!            if (zrooffrc.gt.0.) zhorz=min(zref,(sfc(i,12)-0.5)
!     &*patchlen+zrooffrc)
!            endif

! assume wstar is not relevant for roofs above zH
!            if(zhorz.gt.zH) then
!             Uhorz=ustar/vK*alog((zhorz-zd)/z0)/sqrt(Fm)
!             Thorz=Tcan-Tlog_fact/Uhorz*(alog((zhorz-zH+z0)/z0))**2
!             rhohorz=press*100./287.04/Thorz

!         if(max(abs(Thorz-Tcan),abs(Thorz-Ta)).gt.abs(Tcan-Ta)+0.01)then
!            write(6,*)'Thorz outside of Ta,Tcan range, Thorz,i=',Thorz,i
!              stop
!             endif

!            else
! effective canyon wind is only for HTC calc, not Ri calc too!
!             Uhorz=Acan+Bcan*exp(Ccan*zhorz)
!             Thorz=Tcan
!             rhohorz=rhocan
!            endif

!            if(sfc(i,1).lt.1.5) then
! roofs:
! Harman et al. 2004 approach: 0.1*average roof length
!             call SFC_RI(zhorz-(sfc(i,12)-0.5)*patchlen,
!     &Thorz,Tsfc(iab),Uhorz,Ri)
!             if ((sfc(i,12)-0.5)*patchlen.lt.zH-0.01) then
!              call HTC(Ri,sqrt(Uhorz**2+wstar**2),
!     &zhorz-(sfc(i,12)-0.5)*patchlen,z0roofm,z0roofh,httc,Fh)
!              aaaa=1.
!             else
!              call HTC(Ri,Uhorz,zhorz-(sfc(i,12)-0.5)*patchlen,
!     &z0roofm,z0roofh,httc,Fh)
!              aaaa=2.
!             endif
!            else
! streets:
! Harman et al. 2004 approach: 0.1*average building height
!          call SFC_RI(0.1*zH,Thorz,Tsfc(iab),Uhorz,Ri)
!          call HTC(Ri,sqrt(Uhorz**2+wstar**2),0.1*zH,z0roadm,z0roadh,
!     &httc,Fh)
!            endif
!            httc=httc*cpair*rhohorz

!           endif

!       if(httc.lt.0.0.or.httc.gt.500.) then
!         write(6,*)'httc too big or neg, i',httc,i
!         stop
!       endif

! This is actually Kdown-Kup+eps*Ldown (the Lup term is calculated in the iteration below)
       Rnet=absbl(iab)+absbs(iab)

!       Tconv=Tcan
!       if ((sfc(i,12)-0.5)*patchlen+0.001.ge.zH) Tconv=Thorz

!       if (abs(Tsfc(iab)-Tconv).gt.60.) then
!         write(6,*)'iab,Tsfc(iab),Tconv',iab,Tsfc(iab),Tconv
!         stop
!       endif
       if (Rnet.gt.2000.0.or.Rnet.lt.-500.0) then
         write(6,*)'Rnet is too big, Rnet = ',Rnet
         write(6,*)'Problem is at patch x,y,z,f = ',sfc(i,5),sfc(i,4),sfc(i,3),sfc(i,2)
         stop
       endif

!       Tnew=Tsfc(iab)
!       Told=Tnew+999.

! ITERATION to solve individual patch Tsfc(i) by Newton's method----
!       do 899 while (abs(Tnew-Told).gt.0.001)
!        Told=Tnew
!        Fold=sfc(i,4)*sigma*Told**4+(httc+lambda_sfc(iab)
!     &*2./sfc_ab(iab,6+3*numlayers))
!     &*Told-Rnet-httc*Tconv-lambda_sfc(iab)*sfc_ab(iab,6)
!     &*2./sfc_ab(iab,6+3*numlayers)
!        Fold_prime=4.*sfc(i,4)*sigma*Told**3+httc
!     &+lambda_sfc(iab)*2./sfc_ab(iab,6+3*numlayers)
!        Tnew=-Fold/Fold_prime+Told
! 899   continue

!       if(abs(Tnew-Tsfc(iab)).gt.Tdiffmax) Tdiffmax=abs(Tnew-Tsfc(iab))
!       Tsfc(iab)=Tnew


       Trad(iab)=((1./sigma)*(sfc(i,4)*sigma*Tsfc(iab)**4+refltl(iab)))**(0.25)


! STORE OUTPUT: (only the chosen subdomain)
       if(sfc(i,9).gt.1.5) then
! overall energy balance (per unit plan area):
        Rnet_tot=Rnet_tot+Rnet-sfc(i,4)*sigma*Tsfc(iab)**4
!        Qh_tot=Qh_tot+httc*(Tsfc(iab)-Tconv)
!        Qg_tot=Qg_tot+lambda_sfc(iab)*(Tsfc(iab)-sfc_ab(iab,6))
!     &*2./sfc_ab(iab,6+3*numlayers)
! canyon only:
!        if((sfc(i,12)-0.5)*patchlen.lt.zH-0.01) then
!         Qhcantmp=Qhcantmp+httc*(Tsfc(iab)-Tconv)
!        else
!         Qh_abovezH=Qh_abovezH+httc*(Tsfc(iab)-Tconv)
!        endif

! for evolution of internal building temperature:
!        if(sfc(i,1).gt.2.5) then
! wall internal T
!         Tp=Tp+sfc_ab(iab,5+numlayers)
!        elseif(sfc(i,1).lt.1.5) then
! roof internal T; also add internal of floor (user-defined)
!         Tp=Tp+sfc_ab(iab,5+numlayers)+Tfloor
!        endif

! Surface temperatures and energy balance components.
! Averaging patch values to get facet-average values
! complete (per unit total area)
        Tsfc_cplt=Tsfc_cplt+Tsfc(iab)
! bird's eye view sfc T
        if(sfc(i,1).lt.2.5) Tsfc_bird=Tsfc_bird+Tsfc(iab)
! roof sfc T and energy balance
        if(sfc(i,1).lt.1.5) then
!         httcR=httcR+httc
         Tsfc_R=Tsfc_R+Tsfc(iab)
         Trad_R=Trad_R+((1./sigma)*(sfc(i,4)*sigma*Tsfc(iab)**4+refltl(iab)))**(0.25)
         Rnet_R=Rnet_R+Rnet-sfc(i,4)*sigma*Tsfc(iab)**4
         Kdn_R=Kdn_R+absbs(iab)/(1.-sfc(i,3))
         Kup_R=Kup_R+reflts(iab)         
         Ldn_R=Ldn_R+absbl(iab)/sfc(i,4)
         Lup_R=Lup_R+refltl(iab)+sfc(i,4)*sigma*Tsfc(iab)**4
         svf_roof=svf_roof+(1.-sfc(i,5))
!         Qh_R=Qh_R+httc*(Tsfc(iab)-Tconv)
!         Qg_R=Qg_R+lambda_sfc(iab)*(Tsfc(iab)-sfc_ab(iab,6))
!     &*2./sfc_ab(iab,6+3*numlayers)
!         Qanthro=Qanthro+max(0.,(Tintw-sfc_ab(iab,5+numlayers))
!     &*lambdaavr(numlayers)*2./thickr(numlayers))
!         Qac=Qac+max(0.,(sfc_ab(iab,5+numlayers)-Tintw)
!     &*lambdaavr(numlayers)*2./thickr(numlayers))
        endif
! street energy balance (sfc T calc below)
        if(sfc(i,1).gt.1.5.and.sfc(i,1).lt.2.5) then
!         httcT=httcT+httc
         Tsfc_T=Tsfc_T+Tsfc(iab)
         Trad_T=Trad_T+((1./sigma)*(sfc(i,4)*sigma*Tsfc(iab)**4+refltl(iab)))**(0.25)
         Rnet_T=Rnet_T+Rnet-sfc(i,4)*sigma*Tsfc(iab)**4
         Kdn_T=Kdn_T+absbs(iab)/(1.-sfc(i,3))
         Kup_T=Kup_T+reflts(iab)         
         Ldn_T=Ldn_T+absbl(iab)/sfc(i,4)
         Lup_T=Lup_T+refltl(iab)+sfc(i,4)*sigma*Tsfc(iab)**4
         svf_road=svf_road+(1.-sfc(i,5))
!         Qh_T=Qh_T+httc*(Tsfc(iab)-Tconv)
!         Qg_T=Qg_T+lambda_sfc(iab)*(Tsfc(iab)-sfc_ab(iab,6))
!     &*2./sfc_ab(iab,6+3*numlayers)
!         Qdeep=Qdeep+(sfc_ab(iab,5+numlayers)-Tints)
!     &*lambdaavs(numlayers)*2./thicks(numlayers)
!         if (sfc(i,2).gt.3.5) then
!          TTsun=TTsun+Tsfc(iab)
!          numTsun=numTsun+1
!         elseif (sfc(i,2).lt.0.5) then
!          TTsh=TTsh+Tsfc(iab)
!          numTsh=numTsh+1
!         endif
        endif
!        if(sfc(i,1).gt.2.5) httcW=httcW+httc
         zj=nint(sfc_ab(iab,3))
! N wall sfc T and energy balance
        if(sfc(i,7).gt.0.5) then
         Tsfc_N=Tsfc_N+Tsfc(iab)
         Trad_N=Trad_N+((1./sigma)*(sfc(i,4)*sigma*Tsfc(iab)**4+refltl(iab)))**(0.25)
         Rnet_N=Rnet_N+Rnet-sfc(i,4)*sigma*Tsfc(iab)**4
         Kdn_N=Kdn_N+absbs(iab)/(1.-sfc(i,3))
         Kup_N=Kup_N+reflts(iab)         
         Ldn_N=Ldn_N+absbl(iab)/sfc(i,4)
         Lup_N=Lup_N+refltl(iab)+sfc(i,4)*sigma*Tsfc(iab)**4
         svf_north=svf_north+(1.-sfc(i,5))
! sectional
         Rnet_Ns(zj)=Rnet_Ns(zj)+Rnet-sfc(i,4)*sigma*Tsfc(iab)**4
         Kdn_Ns(zj)=Kdn_Ns(zj)+absbs(iab)/(1.-sfc(i,3))
         Kup_Ns(zj)=Kup_Ns(zj)+reflts(iab)         
         Ldn_Ns(zj)=Ldn_Ns(zj)+absbl(iab)/sfc(i,4)
         Lup_Ns(zj)=Lup_Ns(zj)+refltl(iab)+sfc(i,4)*sigma*Tsfc(iab)**4
         svf_norths(zj)=svf_norths(zj)+(1.-sfc(i,5))
!         Qh_N=Qh_N+httc*(Tsfc(iab)-Tconv)
!         Qg_N=Qg_N+lambda_sfc(iab)*(Tsfc(iab)-sfc_ab(iab,6))
!     &*2./sfc_ab(iab,6+3*numlayers)
!         Qanthro=Qanthro+max(0.,(Tintw-sfc_ab(iab,5+numlayers))
!     &*lambdaavw(numlayers)*2./thickw(numlayers))
!         Qac=Qac+max(0.,(sfc_ab(iab,5+numlayers)-Tintw)
!     &*lambdaavw(numlayers)*2./thickw(numlayers))
!         if (sfc(i,2).gt.3.5) then
!          TNsun=TNsun+Tsfc(iab)
!          numNsun=numNsun+1
!         elseif (sfc(i,2).lt.0.5) then
!          TNsh=TNsh+Tsfc(iab)
!          numNsh=numNsh+1
!         endif
        endif
! S wall sfc T and energy balance
        if(sfc(i,7).lt.-0.5) then
         Tsfc_S=Tsfc_S+Tsfc(iab)
         Trad_S=Trad_S+((1./sigma)*(sfc(i,4)*sigma*Tsfc(iab)**4+refltl(iab)))**(0.25)
         Rnet_S=Rnet_S+Rnet-sfc(i,4)*sigma*Tsfc(iab)**4
         Kdn_S=Kdn_S+absbs(iab)/(1.-sfc(i,3))
         Kup_S=Kup_S+reflts(iab)         
         Ldn_S=Ldn_S+absbl(iab)/sfc(i,4)
         Lup_S=Lup_S+refltl(iab)+sfc(i,4)*sigma*Tsfc(iab)**4
         svf_south=svf_south+(1.-sfc(i,5))
! sectional
         Rnet_Ss(zj)=Rnet_Ss(zj)+Rnet-sfc(i,4)*sigma*Tsfc(iab)**4
         Kdn_Ss(zj)=Kdn_Ss(zj)+absbs(iab)/(1.-sfc(i,3))
         Kup_Ss(zj)=Kup_Ss(zj)+reflts(iab)         
         Ldn_Ss(zj)=Ldn_Ss(zj)+absbl(iab)/sfc(i,4)
         Lup_Ss(zj)=Lup_Ss(zj)+refltl(iab)+sfc(i,4)*sigma*Tsfc(iab)**4
         svf_souths(zj)=svf_norths(zj)+(1.-sfc(i,5))
!         Qh_S=Qh_S+httc*(Tsfc(iab)-Tconv)
!         Qg_S=Qg_S+lambda_sfc(iab)*(Tsfc(iab)-sfc_ab(iab,6))
!     &*2./sfc_ab(iab,6+3*numlayers)
!         Qanthro=Qanthro+max(0.,(Tintw-sfc_ab(iab,5+numlayers))
!     &*lambdaavw(numlayers)*2./thickw(numlayers))
!         Qac=Qac+max(0.,(sfc_ab(iab,5+numlayers)-Tintw)
!     &*lambdaavw(numlayers)*2./thickw(numlayers))
!         if (sfc(i,2).gt.3.5) then
!          TSsun=TSsun+Tsfc(iab)
!          numSsun=numSsun+1
!         elseif (sfc(i,2).lt.0.5) then
!          TSsh=TSsh+Tsfc(iab)
!          numSsh=numSsh+1
!         endif
        endif
! E wall sfc T and energy balance
        if(sfc(i,6).gt.0.5) then
         Tsfc_E=Tsfc_E+Tsfc(iab)
         Trad_E=Trad_E+((1./sigma)*(sfc(i,4)*sigma*Tsfc(iab)**4+refltl(iab)))**(0.25)
         Rnet_E=Rnet_E+Rnet-sfc(i,4)*sigma*Tsfc(iab)**4
         Kdn_E=Kdn_E+absbs(iab)/(1.-sfc(i,3))
         Kup_E=Kup_E+reflts(iab)         
         Ldn_E=Ldn_E+absbl(iab)/sfc(i,4)
         Lup_E=Lup_E+refltl(iab)+sfc(i,4)*sigma*Tsfc(iab)**4
         svf_east=svf_east+(1.-sfc(i,5))
! sectional
         Rnet_Es(zj)=Rnet_Es(zj)+Rnet-sfc(i,4)*sigma*Tsfc(iab)**4
         Kdn_Es(zj)=Kdn_Es(zj)+absbs(iab)/(1.-sfc(i,3))
         Kup_Es(zj)=Kup_Es(zj)+reflts(iab)         
         Ldn_Es(zj)=Ldn_Es(zj)+absbl(iab)/sfc(i,4)
         Lup_Es(zj)=Lup_Es(zj)+refltl(iab)+sfc(i,4)*sigma*Tsfc(iab)**4
         svf_easts(zj)=svf_easts(zj)+(1.-sfc(i,5))
!         Qh_E=Qh_E+httc*(Tsfc(iab)-Tconv)
!         Qg_E=Qg_E+lambda_sfc(iab)*(Tsfc(iab)-sfc_ab(iab,6))
!     &*2./sfc_ab(iab,6+3*numlayers)
!         Qanthro=Qanthro+max(0.,(Tintw-sfc_ab(iab,5+numlayers))
!     &*lambdaavw(numlayers)*2./thickw(numlayers))
!         Qac=Qac+max(0.,(sfc_ab(iab,5+numlayers)-Tintw)
!     &*lambdaavw(numlayers)*2./thickw(numlayers))
!         if (sfc(i,2).gt.3.5) then
!          TEsun=TEsun+Tsfc(iab)
!          numEsun=numEsun+1
!         elseif (sfc(i,2).lt.0.5) then
!          TEsh=TEsh+Tsfc(iab)
!          numEsh=numEsh+1
!         endif
        endif
! W wall sfc T and energy balance
        if(sfc(i,6).lt.-0.5) then
         Tsfc_W=Tsfc_W+Tsfc(iab)
         Trad_W=Trad_W+((1./sigma)*(sfc(i,4)*sigma*Tsfc(iab)**4+refltl(iab)))**(0.25)
         Absbs_W=Absbs_W+absbs(iab)
         Absbl_W=Absbl_W+absbl(iab)
         Emit_W=Emit_W+sfc(i,4)*sigma*Tsfc(iab)**4
         Rnet_W=Rnet_W+Rnet-sfc(i,4)*sigma*Tsfc(iab)**4
         Kdn_W=Kdn_W+absbs(iab)/(1.-sfc(i,3))
         Kup_W=Kup_W+reflts(iab)         
         Ldn_W=Ldn_W+absbl(iab)/sfc(i,4)
         Lup_W=Lup_W+refltl(iab)+sfc(i,4)*sigma*Tsfc(iab)**4
         svf_west=svf_west+(1.-sfc(i,5))
! sectional
         Rnet_Ws(zj)=Rnet_Ws(zj)+Rnet-sfc(i,4)*sigma*Tsfc(iab)**4
         Kdn_Ws(zj)=Kdn_Ws(zj)+absbs(iab)/(1.-sfc(i,3))
         Kup_Ws(zj)=Kup_Ws(zj)+reflts(iab)         
         Ldn_Ws(zj)=Ldn_Ws(zj)+absbl(iab)/sfc(i,4)
         Lup_Ws(zj)=Lup_Ws(zj)+refltl(iab)+sfc(i,4)*sigma*Tsfc(iab)**4
         svf_wests(zj)=svf_wests(zj)+(1.-sfc(i,5))
!         Qh_W=Qh_W+httc*(Tsfc(iab)-Tconv)
!         Qg_W=Qg_W+lambda_sfc(iab)*(Tsfc(iab)-sfc_ab(iab,6))
!     &*2./sfc_ab(iab,6+3*numlayers)
!         Qanthro=Qanthro+max(0.,(Tintw-sfc_ab(iab,5+numlayers))
!     &*lambdaavw(numlayers)*2./thickw(numlayers))
!         Qac=Qac+max(0.,(sfc_ab(iab,5+numlayers)-Tintw)
!     &*lambdaavw(numlayers)*2./thickw(numlayers))
!         if (sfc(i,2).gt.3.5) then
!          TWsun=TWsun+Tsfc(iab)
!          numWsun=numWsun+1
!         elseif (sfc(i,2).lt.0.5) then
!          TWsh=TWsh+Tsfc(iab)
!          numWsh=numWsh+1
!         endif
        endif
       endif

! END OF ITERATIVE TSFC LOOP
      enddo

! BUT, UNLESS EQUILIBRIUM ACHIEVED IN TERMS OF LONGWAVE EXCHANGE AND TSFC,
! GO BACK AND DO IT AGAIN (as in Arnfield)
!      if (Tdiffmax.gt.Tthreshold) then
!       goto 898
!      endif

      Kdn_tot=(Kdn_R+Kdn_T+Kdn_N+Kdn_S+Kdn_E+Kdn_W)/Aplan
      htmean_Kdn=0.
      htmean_Kdn_wall=0.

! sectional
       do zj=1,bh
        Rnet_walls(zj)=(Rnet_Ws(zj)+Rnet_Es(zj)+Rnet_Ss(zj)+Rnet_Ns(zj))/real(numwall2)*real(bh)
        Kdn_walls(zj)=(Kdn_Ws(zj)+Kdn_Es(zj)+Kdn_Ss(zj)+Kdn_Ns(zj))/real(numwall2)*real(bh)
        htmean_Kdn_wall=htmean_Kdn_wall+(Kdn_Ws(zj)+Kdn_Es(zj)+Kdn_Ss(zj)+Kdn_Ns(zj))/&
     max(1.e-9,Kdn_N+Kdn_S+Kdn_E+Kdn_W)*(real(zj)-0.5)/real(bh)
        Kdn_walls_plan(zj)=(Kdn_Ws(zj)+Kdn_Es(zj)+Kdn_Ss(zj)+Kdn_Ns(zj))/real(numstreet2+numroof2)/max(1.e-9,Kdn_tot)
        htmean_Kdn=htmean_Kdn+Kdn_walls_plan(zj)*(real(zj)-0.5)/real(bh)
        Kup_walls(zj)=(Kup_Ws(zj)+Kup_Es(zj)+Kup_Ss(zj)+Kup_Ns(zj))/real(numwall2)*real(bh)        
        Ldn_walls(zj)=(Ldn_Ws(zj)+Ldn_Es(zj)+Ldn_Ss(zj)+Ldn_Ns(zj))/real(numwall2)*real(bh)
        Lup_walls(zj)=(Lup_Ws(zj)+Lup_Es(zj)+Lup_Ss(zj)+Lup_Ns(zj))/real(numwall2)*real(bh)
        svf_walls(zj)=(svf_wests(zj)+svf_easts(zj)+svf_souths(zj)+svf_norths(zj))/real(numwall2)*real(bh)
       enddo

! assuming all roofs are at same height:
       htmean_Kdn=htmean_Kdn+Kdn_R/Aplan/max(1.e-9,Kdn_tot)
      
       Kup=Kup/real(avg_cnt)
       Lup=Lup/real(avg_cnt)

!      solar_refl_done=.false.

!  update internal building air temperature: (Masson et al. 2002)
!  86400 is the number of seconds in a day
!      Tintw=Tintw*(86400.-deltat)/86400.+Tp/(numwall2+2.*numroof2)
!     &*deltat/86400.
! put minimum on internal building temperature
!      Tintw=max(Tintw,273.15+Tbuild_min)

!      Qhcan=Qhcantmp/real(numroof2+numstreet2)/(1.-lambdapR)

! canyon-atm exchange:
!        call SFC_RI(zref-zH+z0,Ta,Tcan,Ua,Ri)
!        call HTC(Ri,Ua,zref-zH+z0,z0,z0,httc_top,Fh)
!        Qhtop=cpair*rhoa*httc_top*(Tcan-Ta)

! Checking for oscillations: (0.05 is, from experience, a number that
! cuts off oscillations early enough without reacting to normal changes
! in canyon temperature)
! Here I'm assuming that the canyon temperature cannot be unstable at
! timesteps of 1-2 seconds or less...if this is removed, the simulation
! sometimes reaches a timestep of 0 simply because dTcan_old is so big
! relative to the other term - this is particularly a problem right after
! the forcing causes the canyon temperature to reverse trend
!       if (abs(deltat*(Qhcan-Qhtop)/Cairavg-dTcan_old)
!     &.gt.0.05.and.deltat.gt.2.) then
!        timeis=timeis-deltat/3600.
!        deltat=deltat*5./8.
!        counter=10
!       write(6,*)'Oscill. Tcan, starting over with 5/8*deltat=',deltat
!        dTcan_old=5./8.*dTcan_old
!        goto 937
!       endif

!        counter=counter+1

! NEW Tcan:
!       Tcan=Tcan+deltat/Cairavg*(Qhcan-Qhtop)

!       dTcan_old=deltat/Cairavg*(Qhcan-Qhtop)


! WRITE OUTPUT
!      if(frcwrite) write(843,873)lpactual,real(2*bh)/real(bl+bw),
!     &hwactual,stror,timeis,Kdir,Kdif,Ldn,Ta,ea,Ua,Udir,
!     &Press,az,zen

! street sfc T
!       Tsfc_T=Tsfc_bird-Tsfc_R
       
! to output averages (every outpt_tm time interval)
!       counter2=counter2+1
!       Kuptot_avg=Kuptot_avg+Kup
!       Luptot_avg=Luptot_avg+Lup
!       Rntot_avg=Rntot_avg+Rnet_tot/Aplan
!       Qhtot_avg=Qhtot_avg+Qh_tot/Aplan
!       Qgtot_avg=Qgtot_avg+Qg_tot/Aplan
!       Qanthro_avg=Qanthro_avg+Qanthro/Aplan
!       Qac_avg=Qac_avg+Qac/Aplan
!       Qdeep_avg=Qdeep_avg+Qdeep/Aplan
!       Qtau_avg=Qtau_avg+rhoa*ustar*ustar
!       TR_avg=TR_avg+Tsfc_R/real(numroof2)-273.15
!       TT_avg=TT_avg+Tsfc_T/real(numstreet2)-273.15
!       TN_avg=TN_avg+Tsfc_N/real(numNwall2)-273.15
!       TS_avg=TS_avg+Tsfc_S/real(numSwall2)-273.15
!       TE_avg=TE_avg+Tsfc_E/real(numEwall2)-273.15
!       TW_avg=TW_avg+Tsfc_W/real(numWwall2)-273.15


! Conduction Loop
!      do iab=1,numsfc2

!  CONDUCTION - combination of Arnfield (198X), Masson (2000), Jacobson (1999)
!  thermal conductivities (in W/K/m2) are added in series instead of
!  plain averaging, Tsfc calculated iteratively above acts as the surface
!  boundary condition

! roofs and walls
!       Tint=Tintw
! streets
!       if (abs(sfc(i,1)-2.).lt.0.5) Tint=Tints

!  first calculate the thermal conductivities between layer centers by adding
!  thermal conductivities (or resistivities) in series

!       do k=1,numlayers
!        tlayer(k)=sfc_ab(iab,k+5)
!        tlayerp(k)=tlayer(k)
!        lambdaav(k)=sfc_ab(iab,k+numlayers+5)
!        htcap(k)=sfc_ab(iab,k+2*numlayers+5)
!        thick(k)=sfc_ab(iab,k+3*numlayers+5)
!       enddo

!  surface matrix values:
!       lambd_o_thick=lambdaav(1)/(thick(1)+thick(2))
!       A(1)=0.
!       B(1)=thick(1)*htcap(1)/deltat+2.*uc*(lambd_o_thick)
!       D(1)=-2.*uc*lambd_o_thick
!       R(1)=-2.*(1.-uc)*lambd_o_thick
!     &*(tlayerp(1)-tlayerp(2))
!     &+tlayerp(1)*thick(1)*htcap(1)/deltat
!     &+(Tsfc(iab)-tlayerp(1))*lambda_sfc(iab)/thick(1)*2.

! what I have done above is make the surface boundary condition
! "QGsfc" completely explicit, as written below, even though the
! conduction can have any level of implicitness, it must conform
! to this explicit boundary condition - prior, I had this BC in
! the uc and 1-uc brackets to make the BC dependent on the implicitness
! but then since the Tsfc solution assumes explicit conduction at
! the sfc (i.e. BC using tlayerp(1)), this would mean a loss or gain
! of energy, since the condution solution would assume a different
! amount of energy being conducted than the Tsfc solution


!  interior matrix values:
!       do k=2,numlayers-1
!        lambd_o_thick=lambdaav(k-1)/(thick(k-1)+thick(k))
!        lambd_o_thick2=lambdaav(k)/(thick(k)+thick(k+1))
!        A(k)=-2.*uc*lambd_o_thick
!        B(k)=thick(k)*htcap(k)/deltat+2.*uc*(lambd_o_thick
!     &+lambd_o_thick2)
!        D(k)=-2.*uc*lambd_o_thick2
!        R(k)=-2.*(1.-uc)*(lambd_o_thick
!     & *(tlayerp(k)-tlayerp(k-1))
!     & +lambd_o_thick2*(tlayerp(k)-tlayerp(k+1)))
!     & +tlayerp(k)*thick(k)*htcap(k)/deltat
!       enddo

!  values for conduction (+ convection + radiation - Masson et al 2002)
!  between innermost layer and inner air
!       lambd_o_thick=lambdaav(numlayers-1)/(thick(numlayers-1)
!     &+thick(numlayers))
!       A(numlayers)=-2.*uc*lambd_o_thick
!       B(numlayers)=thick(numlayers)*htcap(numlayers)/deltat+2.*uc
!     &*(lambd_o_thick+lambdaav(numlayers)
!     &/thick(numlayers)*IntCond)
!       D(numlayers)=0.
!       R(numlayers)=-2.*(1.-uc)
!     &*(lambd_o_thick
!     &*(tlayerp(numlayers)-tlayerp(numlayers-1))
!     &+lambdaav(numlayers)*tlayerp(numlayers)
!     &/thick(numlayers)*IntCond)
!     &+2.*lambdaav(numlayers)
!     &*Tint/thick(numlayers)*IntCond
!     &+tlayerp(numlayers)*thick(numlayers)
!     &*htcap(numlayers)/deltat
 
!  TRIDIAGONAL MATRIX SOLUTION FROM JACOBSON, p. 166
!       gam(1)=-D(1)/B(1)
!       tlayer(1)=R(1)/B(1)
 
!       do k=2,numlayers
!        denom(k)=B(k)+A(k)*gam(k-1)
!        tlayer(k)=(R(k)-A(k)*tlayer(k-1))/denom(k)
!        gam(k)=-D(k)/denom(k)
!       enddo
 
!       do k=numlayers-1,1,-1
!        tlayer(k)=tlayer(k)+gam(k)*tlayer(k+1)
!       enddo

!       do k=1,numlayers
!        sfc_ab(iab,k+5)=tlayer(k)
!       enddo

!      enddo


! 324  continue

 349  continue


!------------------------------------------------------------------
! VISUALIZATION - output for Matlab

!      if(ywrite.and.(first_write.or.(amod(timeis,outpt_tm)*3600.0.lt.
!     &deltat.and.int(timeis*100.).ne.timewrite).or.last_write)) then

!       ywrite=.false.       
       timewrite=int(timeis*100.)

       write(6,*)'------------------------------------------'
       write(6,*)'TIME (hours) = ',timeis
       write(6,*)'TIMESTEP (s) = ',deltat
!       write(6,*)amod(timeis,outpt_tm)*3600.,deltat
       if (ralt.lt.0.) then
       write(6,*)'NIGHTTIME: solar azimuth, elevation angles = ',az,ralt
       else
        write(6,*)'DAYTIME: solar azimuth, elevation angles = ',az,ralt
       endif

       if(ralt.gt.0.0) write(6,*)'average relative Kdown absorption error = ',Kdn_diff/(real(nKdndiff)+1.e-9),'%'
       if(Kdn_diff/(real(nKdndiff)+1.e-9).gt.5.0.and.nKdndiff.gt.10)then
        write(6,*)'time average relative Kdn error = ',Kdn_diff/(real(nKdndiff)+1.e-9),'%'
        write(inputsStoreOut,*)'-------------------------------------'
        write(inputsStoreOut,*)'time, time average relative Kdn error = ',timeis,Kdn_diff/(real(nKdndiff)+1.e-9),'%'
       endif
       Kdn_diff=0.
       nKdndiff=0
       write(6,*)'Kdif,Kdir,Kdown(total) = ',Kdif,Kdir,Ktot
!       write(6,*)'time,Troof,Tstreet,Tnorth,Tsouth,Teast,Twest',timeis,
!     &Tsfc_R/real(numroof2),Tsfc_T/real(numstreet2),
!     &Tsfc_N/real(numNwall2),
!     &Tsfc_S/real(numSwall2),Tsfc_E/real(numEwall2),
!     &Tsfc_W/real(numWwall2)

! WRITE OUTPUT
      svf_canopy=svf_road+svf_north+svf_south+svf_east+svf_west
       write(EnergyBalanceOverallOut,844)lpactual,real(2*bh)/real(bl+bw),hwactual,xlat,stror,minres,bh,bl,sw,yd_actual,&
     amod(timeis,24.),timeis,Rnet_tot/Aplan,(Rnet_tot/Aplan-lambdapR*Rnet_R/real(numroof2))/(1.-lambdapR),&
     Kdir+Kdif,Kup,Ldn,Lup,(Lup-lambdapR*Lup_R/real(numroof2))/(svf_canopy/real(numstreet2))+&
     lambdapR*Lup_R/real(numroof2),Kdir_Calc,Kdif_Calc,Kdir,Kdif,(Kup-lambdapR*Kup_R/real(numroof2))&
     /(1.-lambdapR),(Lup-lambdapR*Lup_R/real(numroof2))/(1.-lambdapR),(Lup-lambdapR*Lup_R/real(numroof2))&
     /(1.-lambdapR)/(svf_canopy/real(numstreet2)),az,zen,max(Kdir_NoAtm,0.)

!       write(836,844)lpactual,real(2*bh)/real(bl+bw),hwactual,xlat,
!     &stror,yd_actual,amod(timeis,24.),
!     &timeis,Rnet_R/real(numroof2),Qh_R/real(numroof2),
!     &Qg_R/real(numroof2),
!     &Rnet_T/real(numstreet2),Qh_T/real(numstreet2),
!     &Qg_T/real(numstreet2),
!     &Rnet_N/real(numNwall2),Qh_N/real(numNwall2),
!     &Qg_N/real(numNwall2),
!     &Rnet_S/real(numSwall2),Qh_S/real(numSwall2),
!     &Qg_S/real(numSwall2),
!     &Rnet_E/real(numEwall2),Qh_E/real(numEwall2),
!     &Qg_E/real(numEwall2),
!     &Rnet_W/real(numWwall2),Qh_W/real(numWwall2),
!     &Qg_W/real(numWwall2)

      write(RadiationBalanceFacetsOut,874)lpactual,real(2*bh)/real(bl+bw),hwactual,xlat,stror,minres,bh,bl,sw,yd_actual,&
     amod(timeis,24.),timeis,Kdn_S/real(numSwall2),Kup_S/real(numSwall2),Ldn_S/real(numSwall2),&
     Lup_S/real(numSwall2),Kdn_E/real(numEwall2),Kup_E/real(numEwall2),Ldn_E/real(numEwall2),&
     Lup_E/real(numEwall2),Kdn_N/real(numNwall2),Kup_N/real(numNwall2),Ldn_N/real(numNwall2),&
     Lup_N/real(numNwall2),Kdn_W/real(numWwall2),Kup_W/real(numWwall2),Ldn_W/real(numWwall2),&
     Lup_W/real(numWwall2),Kdn_R/real(numroof2),Kup_R/real(numroof2),Ldn_R/real(numroof2),&
     Lup_R/real(numroof2),Kdn_T/real(numstreet2),Kup_T/real(numstreet2),Ldn_T/real(numstreet2),&
     Lup_T/real(numstreet2),svf_roof/real(numroof2),svf_road/real(numstreet2),svf_north/real(numstreet2),&
     svf_south/real(numstreet2),svf_east/real(numstreet2),svf_west/real(numstreet2),svf_canopy/real(numstreet2)

      bh3=bh
      write(RadiationBalanceSectionalOut,854)lpactual,real(2*bh)/real(bl+bw),hwactual,xlat,stror,minres,bh,bl,sw,yd_actual,&
     amod(timeis,24.),timeis,htmean_Kdn_wall,htmean_Kdn,Kdn_T/real(numstreet2)* &
     (1.-lambdapR)/max(1.e-9,Kdn_tot),(Kdn_walls_plan(iii),iii=1,bh),Kdn_R/real(numroof2)&
     *lambdapR/max(1.e-9,Kdn_tot)     

      Kdnhr(0)=Kdnhr(0)+Kdn_T/real(numstreet2)*(1.-lambdapR)
      do iii=1,bh
       Kdnhr(iii)=Kdnhr(iii)+Kdn_walls_plan(iii)*Kdn_tot
      enddo
      Kdnhr(bh+1)=Kdnhr(bh+1)+Kdn_R/real(numroof2)*lambdapR
      Kdn_tothr=Kdn_tothr+Kdn_tot
      htmean_Kdn_day=htmean_Kdn_day+htmean_Kdn*Kdn_tot
      Kdn_tothr_wall=Kdn_tothr_wall+(Kdn_N+Kdn_S+Kdn_E+Kdn_W)/real(numwall2)
      htmean_Kdn_wall_day=htmean_Kdn_wall_day+htmean_Kdn_wall*(Kdn_N+Kdn_S+Kdn_E+Kdn_W)/real(numwall2)


!       write(835,844)lpactual,real(2*bh)/real(bl+bw),hwactual,xlat,
!     &stror,yd_actual,amod(timeis,24.),timeis,
!     &Tsfc_cplt/real(numroof2+numwall2+numstreet2)
!     &-273.15,Tsfc_bird/Aplan-273.15,
!     &Tsfc_R/real(numroof2)-273.15,Tsfc_T/real(numstreet2)-273.15,
!     &Tsfc_N/real(numNwall2)-273.15,
!     &Tsfc_S/real(numSwall2)-273.15,Tsfc_E/real(numEwall2)-273.15,
!     &Tsfc_W/real(numWwall2)-273.15,
!     &Tcan-273.15,Ta-273.15,Tintw-273.15,httcR/real(numroof2),
!     &httcT/real(numstreet2),httcW/real(numwall2),
!     &Trad_R/real(numroof2)-273.15,Trad_T/real(numstreet2)-273.15,
!     &Trad_N/real(numNwall2)-273.15,
!     &Trad_S/real(numSwall2)-273.15,Trad_E/real(numEwall2)-273.15,
!     &Trad_W/real(numWwall2)-273.15
!       write(833,844)lpactual,real(2*bh)/real(bl+bw),hwactual,xlat,
!     &stror,yd_actual,amod(timeis,24.),timeis,
!     &TTsun/max(0.01,real(numTsun))-273.15,
!     &TTsh/max(0.01,real(numTsh))-273.15,
!     &TNsun/max(0.01,real(numNsun))-273.15,
!     &TNsh/max(0.01,real(numNsh))-273.15,
!     &TSsun/max(0.01,real(numSsun))-273.15,
!     &TSsh/max(0.01,real(numSsh))-273.15,
!     &TEsun/max(0.01,real(numEsun))-273.15,
!     &TEsh/max(0.01,real(numEsh))-273.15,
!     &TWsun/max(0.01,real(numWsun))-273.15,
!     &TWsh/max(0.01,real(numWsh))-273.15


! to output time averages
!      if(.not.first_write) then
!       write(832,844)lpactual,real(2*bh)/real(bl+bw),hwactual,xlat,
!     & stror,yd+int((timeis-outpt_tm/2.)/24.),
!     & amod((timeis-outpt_tm/2.),24.),timeis-outpt_tm/2.,
!     & amod(timeis,24.),timeis,Kuptot_avg/real(counter2),
!     & Luptot_avg/real(counter2),Rntot_avg/real(counter2),Qhtot_avg
!     & /real(counter2),Qgtot_avg/real(counter2),
!     & Qanthro_avg/real(counter2),Qac_avg/real(counter2),
!     & Qdeep_avg/real(counter2),Qtau_avg/real(counter2),
!     &TR_avg/real(counter2),TT_avg/real(counter2),TN_avg/real(counter2),
!     &TS_avg/real(counter2),TE_avg/real(counter2),
!     &TW_avg/real(counter2)
!       counter2=0
!       Kuptot_avg=0.
!       Luptot_avg=0.
!       Rntot_avg=0.
!       Qhtot_avg=0.
!       Qgtot_avg=0.
!       Qanthro_avg=0.
!       Qac_avg=0.
!       Qdeep_avg=0.
!       Qtau_avg=0.
!       TR_avg=0.
!       TT_avg=0.
!       TN_avg=0.
!       TS_avg=0.
!       TE_avg=0.
!       TW_avg=0.
!      endif

! write out individual surface temperatures of each facet type and
! of overall surface
! but all only for the sub-domain
!      if(facet_out) then
!      sdT(tim,1)=timeis
!      sdN(tim,1)=timeis
!      sdS(tim,1)=timeis
!      sdE(tim,1)=timeis
!      sdW(tim,1)=timeis
!      cntT=2
!      cntN=2
!      cntS=2
!      cntE=2
!      cntW=2
!      do iab=1,numsfc2
!       i=sfc_ab(iab,1)
!       if(sfc(i,9).gt.1.5) then
!        if(sfc(i,1).gt.1.5.and.sfc(i,1).lt.2.5) then
!         sdT(tim,cntT)=Tsfc(iab)
!         cntT=cntT+1
!        elseif(sfc(i,7).gt.0.5) then
!         sdN(tim,cntN)=Tsfc(iab)
!         cntN=cntN+1
!        elseif(sfc(i,7).lt.-0.5) then
!         sdS(tim,cntS)=Tsfc(iab)
!         cntS=cntS+1
!        elseif(sfc(i,6).gt.0.5) then
!         sdE(tim,cntE)=Tsfc(iab)
!         cntE=cntE+1
!        elseif(sfc(i,6).lt.-0.5) then
!         sdW(tim,cntW)=Tsfc(iab)
!         cntW=cntW+1
!        endif
!       endif
!      enddo
!      if(tim.lt.numout)tim=tim+1
!      endif


      time_out=(nint(timeis*10.))*10
      lptowrite=nint(lpin(lpiter)*100.)
      write(lpwrite,'(i2)')lptowrite
      if(lpin(lpiter).lt.0.095) then
       write(lpwrite1,'(i1)')lptowrite
       lpwrite='0'//lpwrite1
      endif
      bhbltowrite=nint(bh_o_bl(bhiter)*100.)
      write(bhblwrite,'(i3)')bhbltowrite
      if(bhbltowrite.lt.100) then
       write(bhblwrite2,'(i2)')bhbltowrite
       bhblwrite='0'//bhblwrite2
      endif
      if(bhbltowrite.lt.10) then
       write(bhblwrite1,'(i1)')bhbltowrite
       bhblwrite='00'//bhblwrite1
      endif
      write(strorwrite,'(i2)')nint(stror)
      if(stror.lt.9.5) then
       write(strorwrite1,'(i1)')nint(stror)
       strorwrite='0'//strorwrite1
      endif
      write(latwrite,'(i2)')nint(abs(xlat))
      if(abs(xlat).lt.9.5) then
       write(latwrite1,'(i1)')nint(abs(xlat))
       latwrite='0'//latwrite1
      endif
      if(xlat.ge.0.) then
       latwrite2=latwrite//'N'
      else
       latwrite2=latwrite//'S'
      endif
      write(ydwrite,'(i3)')yd
      if(yd.lt.10) then
       write(ydwrite1,'(i1)')yd
       ydwrite='00'//ydwrite1
      elseif(yd.lt.100.and.yd.gt.9)then
       write(ydwrite2,'(i2)')yd
       ydwrite='0'//ydwrite2
      endif 

!      if(sum_out)      then
!      if (time_out.lt.1000.) then
!       write(time1,'(i3)')time_out
!       if(time_out.eq.0)time1='000'
!       open(unit=197,file='Tsfc_yd'//ydwrite//'_lp'//lpwrite//
!     &'_bhbl'//bhblwrite//
!     &'_lat'//latwrite2//'_stror'//strorwrite//'_tim0'//time1//'.out')
!       open(unit=198,file='Tbright_yd'//ydwrite//'_lp'//lpwrite//
!     &'_bhbl'//bhblwrite//
!     &'_lat'//latwrite2//'_stror'//strorwrite//'_tim0'//time1//'.out')
!      elseif (time_out.lt.10000) then
!       write(time2,'(i4)')time_out
!       open(unit=197,file='Tsfc_yd'//ydwrite//'_lp'//lpwrite//
!     &'_bhbl'//bhblwrite//
!     &'_lat'//latwrite2//'_stror'//strorwrite//'_tim'//time2//'.out')
!       open(unit=198,file='Tbright_yd'//ydwrite//'_lp'//lpwrite//
!     &'_bhbl'//bhblwrite//
!     &'_lat'//latwrite2//'_stror'//strorwrite//'_tim'//time2//'.out')
!      else                                                
!       write(6,*) 'coded to only write output up to hour 99'
!       stop
!      endif

! metadata at the top of output files
!      write(197,*)numsfc2,lpactual,xlat,stror
!      write(198,*)numsfc2,lpactual,xlat,stror
!      write(197,*)bh,bl,bw,sw,sw2
!      write(198,*)bh,bl,bw,sw,sw2
!      write(197,*)al2,aw2,patchlen,yd,ralt
!      write(198,*)al2,aw2,patchlen,yd,ralt


!      i=0

!      DO f=1,5
!       DO Z=0,BH
!        DO Y=1,aw2
!         DO X=1,al2
!          if(surf(x,y,z,f))then
!           i=i+1
!           jab=ind_ab(i)
!           if(sfc(i,9).gt.1.5) then
!            write(197,*)Tsfc(jab)
!            write(198,*)Trad(jab)
!           endif
!          endif
!         enddo
!        enddo
!       enddo
!      enddo

!      close(197)
!      close(198)
!      endif

! postprocessing for Matlab visualization...
      if(matlab_out) then
      if (time_out.lt.1000.) then
       write(time1,'(i3)')time_out
       if(time_out.eq.0)time1='000'
       if(first_write) then
        open(unit=VerticesToMatlabOut,file='lp'//lpwrite//'_bhbl'//bhblwrite//'_vertices_toMatlab.out')
        open(unit=FacesToMatlabOut,file='lp'//lpwrite//'_bhbl'//bhblwrite//'_faces_toMatlab.out')
       endif
!       open(unit=ToMatlabSVFYdOut,file='toMatlab_Tsfc_yd'//ydwrite//'_lp'//lpwrite//
       open(unit=ToMatlabSVFYdOut,file='toMatlab_SVF_yd'//ydwrite//'_lp'//lpwrite//'_bhbl'//bhblwrite//'_lat'&
     //latwrite2//'_stror'//strorwrite//'_tim0'//time1//'.out')
      open(unit=ToMatlabTbrightYdOut,file='toMatlab_Tbright_yd'//ydwrite//'_lp'//lpwrite//'_bhbl'//bhblwrite//'_lat'&
     //latwrite2//'_stror'//strorwrite//'_tim0'//time1//'.out')
       open(unit=ToMatlabKabsYdOut,file='toMatlab_Kabs_yd'//ydwrite//'_lp'//lpwrite//'_bhbl'//bhblwrite//'_lat'&
     //latwrite2//'_stror'//strorwrite//'_tim0'//time1//'.out')
       open(unit=ToMatlabKreflYdOut,file='toMatlab_Krefl_yd'//ydwrite//'_lp'//lpwrite//'_bhbl'//bhblwrite//'_lat'&
     //latwrite2//'_stror'//strorwrite//'_tim0'//time1//'.out')
       elseif (time_out.lt.10000) then
        write(time2,'(i4)')time_out
        if(first_write) then
         open(unit=VerticesToMatlabOut,file='lp'//lpwrite//'_bhbl'//bhblwrite//'_vertices_toMatlab.out')
         open(unit=FacesToMatlabOut,file='lp'//lpwrite//'_bhbl'//bhblwrite//'_faces_toMatlab.out')
        endif
!       open(unit=ToMatlabSVFYdOut,file='toMatlab_Tsfc_yd'//ydwrite//'_lp'//lpwrite//
        open(unit=ToMatlabSVFYdOut,file='toMatlab_SVF_yd'//ydwrite//'_lp'//lpwrite//'_bhbl'//bhblwrite//'_lat'&
     //latwrite2//'_stror'//strorwrite//'_tim'//time2//'.out')
      open(unit=ToMatlabTbrightYdOut,file='toMatlab_Tbright_yd'//ydwrite//'_lp'//lpwrite//'_bhbl'//bhblwrite//'_lat'&
     //latwrite2//'_stror'//strorwrite//'_tim'//time2//'.out')
      open(unit=ToMatlabKabsYdOut,file='toMatlab_Kabs_yd'//ydwrite//'_lp'//lpwrite//'_bhbl'//bhblwrite//'_lat'&
     //latwrite2//'_stror'//strorwrite//'_tim'//time2//'.out')
        open(unit=ToMatlabKreflYdOut,file='toMatlab_Krefl_yd'//ydwrite//'_lp'//lpwrite//'_bhbl'//bhblwrite//'_lat'&
     //latwrite2//'_stror'//strorwrite//'_tim'//time2//'.out')
       else
        write(6,*) 'coded to only write output up to hour 99'
        stop
       endif         


      if (first_write.and.(newlp.or.newbhbl)) then

      allocate(vertex(1:numsfc*2,3))
      allocate(face(1:numsfc,4))

       newlp=.false.
       newbhbl=.false.
      endif

      numvertex=0
      i=0

      DO f=1,5
       DO Z=0,BH
        DO Y=1,AW2
         DO X=1,AL2
          if(surf(x,y,z,f))then
           i=i+1
           if (first_write) then
           do iv=1,4
             xpinc=-0.5
             ypinc=-0.5
             if ((iv.ge.2).and.(iv.le.3)) xpinc=0.5
             if ((iv.ge.1).and.(iv.le.2)) ypinc=0.5
             if(f.eq.5)then
               XT=sfc(i,10)
               YT=sfc(i,11)+xpinc
               ZT=sfc(i,12)+ypinc
             elseif(f.eq.3)then
               XT=sfc(i,10)
               YT=sfc(i,11)+xpinc
               ZT=sfc(i,12)+ypinc
             elseif(f.eq.4)then
               XT=sfc(i,10)+xpinc
               YT=sfc(i,11)
               ZT=sfc(i,12)+ypinc
             elseif(f.eq.2)then
               XT=sfc(i,10)+xpinc
               YT=sfc(i,11)
               ZT=sfc(i,12)+ypinc
             elseif(f.eq.1)then
               XT=sfc(i,10)+xpinc
               YT=sfc(i,11)+ypinc
               ZT=sfc(i,12)
             else
               write(*,*)'PROBLEM with wall orientation'
             endif
! test if the current vertex already exists
             do k=1,numvertex
              if (abs(XT-vertex(k,1)).lt.0.01) then
               if (abs(YT-vertex(k,2)).lt.0.01) then
                if (abs(ZT-vertex(k,3)).lt.0.01) then
                 face(i,iv)=k
                 goto 48
                endif
               endif
              endif
             enddo
! the current vertex is a new one - add it
             numvertex=numvertex+1
             vertex(numvertex,1)=XT
             vertex(numvertex,2)=YT
             vertex(numvertex,3)=ZT
             write(VerticesToMatlabOut,*)XT,YT,ZT
             face(i,iv)=numvertex
 48          continue
           enddo
            write(FacesToMatlabOut,*)(face(i,iv),iv=1,4)
           endif
             jab=ind_ab(i)
!             write(ToMatlabSVFYdOut,*)Tsfc(jab)-273.15
             write(ToMatlabSVFYdOut,*)1.-sfc(i,5)
             write(ToMatlabTbrightYdOut,*)Trad(jab)-273.15
             write(ToMatlabKabsYdOut,*)absbs(jab)
             write(ToMatlabKreflYdOut,*)reflts(jab)
          endif
         enddo
        enddo
       enddo
      enddo

      if(first_write) then
       close(VerticesToMatlabOut)
       close(FacesToMatlabOut)
      endif
      close(ToMatlabSVFYdOut)
      close(ToMatlabTbrightYdOut)
      close(ToMatlabKabsYdOut)
      close(ToMatlabKreflYdOut)

! whether or not to write Matlab files
      endif

      first_write=.false.

! whether or not it is a timestep to write outputs
!      endif

!      if (last_write) goto 351

!      timeis = timeis + deltat/3600.
      timeis = timeis + outpt_tm

!      if(amod(timeis,outpt_tm).ge.outpt_tm-3.5*deltat/3600.) then
!       ywrite=.true.
!      endif

 309  continue

      Kdn_wall_tot_day=0.
      do iii=0,bh+1
       Kdnhr(iii)=Kdnhr(iii)/max(1.e-9,Kdn_tothr)
       Kdn_wall_tot_day=Kdn_wall_tot_day+Kdnhr(iii)
      enddo
      Kdn_wall_tot_day=Kdn_wall_tot_day-Kdnhr(0)-Kdnhr(bh+1)

      htmean_Kdn_day=htmean_Kdn_day/Kdn_tothr
      htmean_Kdn_wall_day=htmean_Kdn_wall_day/Kdn_tothr_wall

      write(RadiationBalanceSectionalDayAvgOut,854)lpactual,real(2*bh)/real(bl+bw),hwactual,xlat,stror,minres,bh,bl,sw,yd_actual,&
     start_time,end_time,htmean_Kdn_wall_day,htmean_Kdn_day,Kdnhr(0),Kdn_wall_tot_day,Kdnhr(bh+1)
!     &htmean_Kdn_wall_day,htmean_Kdn_day,(Kdnhr(iii),iii=0,bh+1)
     

      htmean_wall_HrStr=htmean_wall_HrStr+htmean_Kdn_wall_day
      htmean_HrStr=htmean_HrStr+htmean_Kdn_day
      Kdn_road_HrStr=Kdn_road_HrStr+Kdnhr(0)
      Kdn_wall_HrStr=Kdn_wall_HrStr+Kdn_wall_tot_day
      Kdn_roof_HrStr=Kdn_roof_HrStr+Kdnhr(bh+1)
      numstror=numstror+1

!      if(ywrite) then
!       last_write=.true.
!       goto 349
!      endif

! 351  continue
!      last_write=.false.

!      if(facet_out) then
!      write(745,744)(sdT(iii,1),iii=1,numout)
!      do jjj=2,numstreet2+1
!       write(745,744)(sdT(iii,jjj),iii=1,numout)
!      enddo
!      write(746,744)(sdN(iii,1),iii=1,numout)
!      do jjj=2,numNwall2+1
!       write(746,744)(sdN(iii,jjj),iii=1,numout)
!      enddo
!      write(747,744)(sdS(iii,1),iii=1,numout)
!      do jjj=2,numSwall2+1
!       write(747,744)(sdS(iii,jjj),iii=1,numout)
!      enddo
!      write(748,744)(sdE(iii,1),iii=1,numout)
!      do jjj=2,numEwall2+1
!       write(748,744)(sdE(iii,jjj),iii=1,numout)
!      enddo
!      write(749,744)(sdW(iii,1),iii=1,numout)
!      do jjj=2,numWwall2+1
!       write(749,744)(sdW(iii,jjj),iii=1,numout)
!      enddo
!      endif


!      frcwrite=.false.

      stror=stror+strorint
! this is the enddo for the street orientation iteration
      enddo

      write(RadiationBalance_SectionalDayStrorAvgOut,855)lpactual,real(2*bh)/real(bl+bw),hwactual,&
     xlat,stror_in,strorint,strormax,minres,&
     bh,bl,sw,yd_actual,start_time,end_time,htmean_wall_HrStr/real(numstror),htmean_HrStr/real(numstror),&
     Kdn_road_HrStr/real(numstror),Kdn_wall_HrStr/real(numstror),Kdn_roof_HrStr/real(numstror)

      xlat=xlat+xlatint
! this is the enddo for the latitude iteration
      enddo



      deallocate(bldht)
      deallocate(surf_shade)
      deallocate(surf)
      deallocate(sfc_ab)
      deallocate(sfc)
      deallocate(ind_ab)
      deallocate(vffile)
      deallocate(vfppos)
      deallocate(vfipos)
      deallocate(mend)
      deallocate(refl_emist)
      deallocate(absbs)
      deallocate(absbl)
      deallocate(refls)
      deallocate(refll)
      deallocate(reflts)
      deallocate(refltl)
      deallocate(reflps)
      deallocate(reflpl)
      deallocate(Tsfc)
      deallocate(Trad)
!      deallocate(lambda_sfc)
      deallocate(vf3)
      deallocate(vf3j)

      if(matlab_out) then
       deallocate(vertex)
       deallocate(face)
      endif

!      deallocate(sdT)
!      deallocate(sdN)
!      deallocate(sdS)
!      deallocate(sdE)
!      deallocate(sdW)
      deallocate(Rnet_walls)
      deallocate(Kdn_walls)
      deallocate(Kdn_walls_plan)
      deallocate(Kup_walls)  
      deallocate(Ldn_walls)
      deallocate(Lup_walls)
      deallocate(svf_walls)
      deallocate(Rnet_Ns)
      deallocate(Kdn_Ns)
      deallocate(Kup_Ns)  
      deallocate(Ldn_Ns)
      deallocate(Lup_Ns)
      deallocate(svf_norths)
      deallocate(Rnet_Ss)
      deallocate(Kdn_Ss)
      deallocate(Kup_Ss)  
      deallocate(Ldn_Ss)
      deallocate(Lup_Ss)
      deallocate(svf_souths)
      deallocate(Rnet_Es)
      deallocate(Kdn_Es)
      deallocate(Kup_Es)  
      deallocate(Ldn_Es)
      deallocate(Lup_Es)
      deallocate(svf_easts)
      deallocate(Rnet_Ws)
      deallocate(Kdn_Ws)
      deallocate(Kup_Ws)  
      deallocate(Ldn_Ws)
      deallocate(Lup_Ws)
      deallocate(svf_wests)
      deallocate(Kdnhr)
!      deallocate(Uwrite)
!      deallocate(Twrite)

!      deallocate(Qh)

 


! this is the enddo for the bh iteration
      enddo

! this is the enddo for the lp iteration
      enddo

       write(6,*)'------------------------------------------'
       write(6,*)'minres=',minres
       write(6,*)'------------------------------------------'
       write(6,*)'absolute value of relative sky view factor error (maximum of all simulations) was:'&
     ,svfe_store,'%(averaged over the central urban unit)'
       write(6,*)'------------------------------------------'
       write(6,*)'absolute value of absolute received Kdown error (maximum of all simulations) was:'&
     ,Kdn_ae_store,' W/m2 (averaged over the central urban unit) and the absolute value of the relative ' &
     ,'received Kdown error at this time step was:',100.*Kdn_re_store,'%'

       write(6,*)'------------------------------------------'
       write(6,*)'Received solar radiation was at least 10 W/m2 AND 5.0% in error during '&
     ,badKdn,' time steps over the course of the simulation(s)'
       if(badKdn.gt.0) write(6,*)'...you may need to increase the resolution; the file Inputs_Store.out ' &
     ,'will tell you which simulations (if you performed more than one) suffered the most from a lack of resolution'

! this is the enddo for the minres iteration
      enddo

 
!      deallocate(lambdar)
!      deallocate(lambdas)
!      deallocate(lambdaw)
!      deallocate(htcapr)
!      deallocate(htcaps)
!      deallocate(htcapw)
!      deallocate(thickr)
!      deallocate(thicks)
!      deallocate(thickw)
!      deallocate(depthr)
!      deallocate(depths)
!      deallocate(depthw)

!      deallocate(lambda)
!      deallocate(lambdaav)
!      deallocate(htcap)
!      deallocate(thick)
!      deallocate(lambdaavr)
!      deallocate(lambdaavs)
!      deallocate(lambdaavw)
!      deallocate(tlayer)
!      deallocate(tlayerp)
!      deallocate(gam)
!      deallocate(denom)
!      deallocate(A)
!      deallocate(B)
!      deallocate(D)
!      deallocate(R)
 

!      close(832)
!      close(833)
!      close(835)
!      close(836)
      close(EnergyBalanceOverallOut)
!      close(843)
      close(RadiationBalanceFacetsOut)
      close(RadiationBalanceSectionalOut)
      close(RadiationBalanceSectionalDayAvgOut)
      close(inputsStoreOut)

 743  format(1x,9(f8.3,1x))
 744  format(1x,99(f8.3,1x))
 817  format(1x,a180,1x,30(f9.3,1x))
 844  format(1x,3(f7.3,1x),f7.2,1x,f8.4,1x,5(i4,1x),30(f9.3,1x))
 854  format(1x,4(f8.3,1x),f8.4,1x,5(i4,1x),50(f10.4,1x))
 855  format(1x,4(f8.3,1x),3(f8.4,1x),1x,5(i4,1x),50(f10.4,1x))
 874  format(1x,4(f8.3,1x),f8.4,1x,5(i4,1x),26(f9.3,1x),7(f8.5,1x))
 873  format(1x,3(f7.3,1x),f7.2,1x,f8.4,1x,24(f8.3,1x))
 884  format(1x,f8.4,1x,30(f8.3,1x))
 886  format(1x,a1,1x,30(i3,1x))
 887  format(1x,a300,1x)
 894  format(1x,a9,1x,49(i2,1x))
 629  format(4(1x,a8,1x,f8.3))
 630  format(1x,a500)
 777  format(1x,999A)


! The end of the main routine
      END




      logical function ray(x,y,z,f,xx,yy,zz,ff,surf_shade,fx,fy,fz,fxx,fyy,fzz,al,aw,bh)

!  function 'ray' determines if surface 1 (vx,vy,vz,f) can see surface 2
!  (vxx,vyy,vzz,ff) using ray tracing and testing for obstructions
      real vx,vy,vz,vxx,vyy,vzz
      real xinc,yinc,zinc,xt,yt,zt,mag,dist,inc
      real fx(5),fy(5),fz(5),fxx(5),fyy(5),fzz(5)
      integer xtest,ytest,ztest
      integer x,y,z,f,xx,yy,zz,ff,al,aw,bh
      logical surf_shade
      dimension surf_shade(0:al+1,0:aw+1,0:bh)

! patch surface centers:
!  patch surface i:
          vx = real(x) + fx(f)
          vy = real(y) + fy(f)
          vz = real(z) + fz(f)
!  patch surface j:
          vxx = real(xx) + fxx(ff)
          vyy = real(yy) + fyy(ff)
          vzz = real(zz) + fzz(ff)

! these tests will need to be changed for non plane-parallel sfcs
      if(f.eq.ff.or.(f.eq.1.and.zz.le.z).or.(f.eq.2.and.yy.le.y).or.(f.eq.4.and.yy.ge.y).or.&
     (f.eq.3.and.xx.le.x).or.(f.eq.5.and.xx.ge.x).or.(ff.eq.1.and.z.le.zz).or.(ff.eq.2.and.y.le.yy)&
     .or.(ff.eq.4.and.y.ge.yy).or.(ff.eq.3.and.x.le.xx).or.(ff.eq.5.and.x.ge.xx))then
       ray=.false.
       goto 67
      else
       xinc=vxx-vx
       yinc=vyy-vy
       zinc=vzz-vz
       mag=sqrt(xinc**2+yinc**2+zinc**2)
       xinc=xinc/mag*0.25
       yinc=yinc/mag*0.25
       zinc=zinc/mag*0.25
       inc=sqrt(xinc**2+yinc**2+zinc**2)

       xt=vx+xinc
       yt=vy+yinc
       zt=vz+zinc
       dist=inc
       xtest=nint(xt)
       ytest=nint(yt)
       ztest=nint(zt)

       if(surf_shade(xtest,ytest,ztest))then
        write(6,*)'problem: cell not empty that should be'
        write(6,*)xt,yt,zt,xtest,ytest,ztest,vx,vy,vz,x,y,z,f,xinc,yinc,zinc,surf_shade(xtest,ytest,ztest)
        stop
       endif

       do 302 while (dist.lt.mag)
        if(surf_shade(xtest,ytest,ztest))then
       ray=.false.
       goto 67
      else
         xt=xt+xinc
         yt=yt+yinc
         zt=zt+zinc
         dist=dist+inc
         xtest=nint(xt)
         ytest=nint(yt)
         ztest=nint(zt)
      endif
 302   continue

       ray=.true.

      endif

 67   continue

      return
      end


! ----------------------------------------------------------
!  Subroutine to determine which patches are shaded and which
!  are sunlit
      subroutine shade(stror,az,ralt,ypos,surf,surf_shade,al2,aw2,bh,par,sfc,numsfc,a1,a2,b1,b2,numsfc2,sfc_ab,par_ab, veg_shade,&
          timeis,yd_actual)
use TUFConstants
      implicit none

      INTEGER AL2,AW2,BH,PAR,a1,a2,b1,b2
      integer x,y,z,f,xtest,ytest,ztest,numsfc,iv
      real az,ralt,xpos,ypos,dir1,dir2,stror,xpinc,ypinc
      real xt,yt,zt,xinc,yinc,zinc,sfc(numsfc,par)
      
      logical vegetationInRay
      real timeis
      integer yd_actual

! FOR PARAMETER 2 THE ELEMENT IS SUNLIT SURF(X,Y,Z,f,2)=1 OR SHADED
! SURF (X,Y,Z,f,2)=2

! ARRAY DECLARATIONS

      REAL ANGDIF,HH
      integer i,is,k,iab,numsfc2,par_ab
      REAL dmin, sor, sorsh(2),sfc_ab(numsfc2,par_ab)
      logical surf_shade,surf,veg_shade

      dimension sor(2:5)
      dimension surf_shade(0:al2+1,0:aw2+1,0:bh+1)
      dimension veg_shade(0:al2+1,0:aw2+1,0:bh+1)
      DIMENSION SURF(1:AL2,1:AW2,0:BH,1:5)
      
      real sind,cosd,tand,asind,acosd,atand
      external sind,cosd,tand,asind,acosd,atand

      X=0
      Y=0
      Z=0

      az = amod(az,360.)
! ensure that stror is a positive angle between 0 and 360
      stror=amod(stror+360.,360.)

! xpos and ypos are the orientations of the x and y axes
      ypos=stror
      xpos=stror+90

! DECIDE WHETHER WALL ORIENTATION IS FACING THE SUN OR AWAY FROM IT

      DIR1=amod(AZ+90.,360.)
      DIR2=amod(AZ+270.,360.)

! set a minimum distance for ray to go before it can hit
! an obstacle (just longer than the distance from the center
! of a cell face to an opposite corner (1.225) to prevent
! self shading).  Having a ray tracing increment of 0.25 still 
! allows for shading by a cell adjacent to the air volume above
! the surface
      dmin=1.23
            
! the actual surface (i.e. wall) orientations are only 4
      sor(2)=stror
      sor(3)=amod(stror+90.,360.)
      sor(4)=amod(stror+180.,360.)
      sor(5)=amod(stror+270.,360.)
! only a maximum of two of these orientations can be shaded
      is=0
      if ((dir1+180).lt.360) then
         do k=2,5
          if ((sor(k).ge.dir1).and.(sor(k).lt.dir2)) then
            is=is+1          
            sorsh(is)=sor(k)  

          endif
         enddo
      else
         do k=2,5
          if (((sor(k).ge.dir1).and.(sor(k).lt.360)).or.sor(k).lt.dir2) then
            is=is+1     
            sorsh(is)=sor(k)  
          endif
         enddo
      endif   
      if (is.eq.1) sorsh(2)=sorsh(1)
      
      
! SETUP NECESSARY EQUATIONS TO CALCULATE THE XINC,YINC AND ZINC(INCREMENTS 
! REQUIRED FOR TESTING SUNLIT OR SHADED)

! ANGDIF is the difference between the solar azimuth and the
! direction of the 'north' facing street
      ANGDIF=AZ-Ypos
      if(ANGDIF.lt.0.) ANGDIF=AZ+(360.-Ypos)

      HH=COSD(RALT)*0.2
      XINC=SIND(ANGDIF)*HH
      YINC=COSD(ANGDIF)*HH
      ZINC=SIND(RALT)*0.2
            
! RUN THROUGH THE ARRAY TO DETERMINE WHICH FACES ARE SHADED AND SUNLIT    
! IF FACING SUN DECIDE WHETHER LOCATION IS BLOCKED BY OTHER BUILDINGS
! ROOF IS not ALWAYS SUNLIT

      iab=0
      !do f=beginF,endF
      do f=1,5  !! KN switching this to 2,5 from 1,5 since sor(2:5)
        DO Z=0,BH
          DO Y=b1,b2
            DO X=a1,a2

              if(.not.surf(x,y,z,f))then 
!c if the cell face is not a surface:               
                goto 41
              !elseif(f.gt.1.and.(sor(f).Eq.sorsh(1).or.sor(f).eq.sorsh(2))) THEN
                !! restructure this because sor(1) crashes with array out of bounds
              elseif(f.gt.1) then
                  if ( (sor(f).Eq.sorsh(1).or.sor(f).eq.sorsh(2))) THEN    
                    iab=iab+1
                    i=sfc_ab(iab,sfc_ab_i)
                    if (i.gt.numsfc.or.iab.gt.numsfc2) then
                        write(6,*)'PROB1:i,numsfc',i,numsfc
                        stop
                    endif
!C IF NEXT TRUE THEN ORIENTATION OF SUN AND SURFACE ELEMENT MAKES LOCATION SHADED
                    sfc(i,sfc_sunlitfact)=0.
                  endif
       
              ELSE
                iab=iab+1
                i=sfc_ab(iab,1)
!               write(6,*)'i2=',i
                if (i.gt.numsfc.or.iab.gt.numsfc2) then
                 write(6,*)'PROB2:i,numsfc',i,numsfc
                 stop
                endif
! case where the wall orientation is such that it is facing towards the sun
! the following defines steps that climb along the ray towards the sun

! subdivide each patch into 4 to calculate partial shading
                  sfc(i,2)=0.
                  do iv=1,4
                   vegetationInRay=.false.
                   xpinc=-0.25
                   ypinc=-0.25
                   if ((iv.ge.2).and.(iv.le.3)) xpinc=0.25
                   if ((iv.ge.1).and.(iv.le.2)) ypinc=0.25

                     ZT=(real(Z)+ZINC)
                     XT=(real(X)+XINC)
                     YT=(real(Y)+YINC)
! start the ray tracing from the wall element surface
! ACTUALLY from the center of four smaller patches that the
! original patch is subdivided into
                     if(f.eq.5)then
                       XT=XT-0.5
                       YT=YT+xpinc
                       ZT=ZT+ypinc
                 elseif(f.eq.3)then
                       XT=XT+0.5
                       YT=YT+xpinc
                       ZT=ZT+ypinc
                 elseif(f.eq.4)then
                       XT=XT+xpinc
                       YT=YT-0.5
                       ZT=ZT+ypinc
                 elseif(f.eq.2)then
                       XT=XT+xpinc
                       YT=YT+0.5
                       ZT=ZT+ypinc
                 elseif(f.eq.1)then
                       XT=XT+xpinc
                       YT=YT+ypinc
                       ZT=ZT+0.5
                 else
                   write(*,*)'PROBLEM with wall orientation'
                 endif

                     ZTEST=NINT(ZT)
                     XTEST=NINT(XT)
                     YTEST=NINT(YT)

                     
                     DO 300 WHILE((XTEST.EQ.X).AND.(YTEST.EQ.Y).AND.(ZTEST.EQ.Z))
     
                             ZT=(ZT+ZINC)
                             XT=(XT+XINC)
                             YT=(YT+YINC)
                             ZTEST=NINT(ZT)
                             XTEST=NINT(XT)
                             YTEST=NINT(YT)

                             
 300                 CONTINUE
                     
                     DO 100 WHILE ((ZTEST.LE.BH).AND.(XTEST.GE.1).AND.(XTEST.LE.AL2).AND.(YTEST.GE.1).AND.(YTEST.LE.AW2))
                          
                            IF (surf_shade(xtest,ytest,ztest))then

                                goto 46

                            END IF
                            
                            !print *,'test veg_shade ',xtest,ytest,ztest
                            ! set vegetation flag if not already set
                            if (veg_shade(xtest,ytest,ztest).AND..NOT.vegetationInRay)then
                                vegetationInRay=.true.
                                !! KN randomly subtract some for now
!                                sfc(i,sfc_sunlitfact)=sfc(i,sfc_sunlitfact)-0.4
!                                if (sfc(i,sfc_sunlitfact).lt.0)then
!                                    sfc(i,sfc_sunlitfact)=0
!                                endif
                                print *,'veg at ',xtest,ytest,ztest,' from x,y,z,f ',x,y,z,f, &
                                    'with sfc(i,sfc_sunlitfact)', sfc(i,sfc_sunlitfact)
                            END IF
                            
                            ZT=(ZT+ZINC)
                            XT=(XT+XINC)
                            YT=(YT+YINC)
                            ZTEST=NINT(ZT)
                            XTEST=NINT(XT)
                            YTEST=NINT(YT)
                           
 100                  CONTINUE
!  sunlit                 
                 if (vegetationInRay)then
                     sfc(i,sfc_sunlitfact)=sfc(i,sfc_sunlitfact)+0.4  !! TODO set this to the return value from reverseRayTrace()
                     !print *,'veg found,',x,y,z,f,i,xt,xinc,yt,yinc,zt,zinc,xtest,ytest,ztest,bh,al2,aw2
                     !call reverseRayTrace(x,y,z,f,i,xt,xinc,yt,yinc,zt,zinc,xtest,ytest,ztest,bh,al2,aw2,veg_shade,timeis,yd_actual)
                     call reverseRayTrace(xt,xinc,yt,yinc,zt,zinc,xtest,ytest,ztest,bh,al2,aw2,veg_shade,timeis,yd_actual)
                 else
                    sfc(i,sfc_sunlitfact)=sfc(i,sfc_sunlitfact)+1.
                 endif
                 vegetationInRay=.false.
                 
 46              continue
                enddo
               endif
 41            continue
               enddo
 
              x=1
              enddo
             
             x=1
             y=1
           enddo
          x=1
          y=1
          z=0
        enddo
       if(iab.ne.numsfc2)write(6,*)'PROBLEM: iab,numsfc2 = ',iab,numsfc2

      RETURN
      END

!------------------------------------------------------------------
! conversion to radians

      REAL FUNCTION ANGRAD(DEG)
      REAL DEG

      ANGRAD=DEG*1745.3293E-5

      RETURN
      END

!c------------------------------------------------------------------
!c  view factor between two identical, parallel rectangular surfaces
!c  of dimensions x by z, separated by a distance y; the surfaces
!c  directly oppose each other
!c  this is Lin Wu's F1
       real(8) function pll(xa,ya,za)
       real(8) x,y,z,pi
       real(8) xa,ya,za

       x=abs(xa)
       y=abs(ya)
       z=abs(za)
       if(x.eq.0.0d0.or.y.eq.0.0d0.or.z.eq.0.0d0)goto 134

       pi = 4.0d0 * atan(1.0d0)
!c  Hottel / Sparrow and Cess
       pll=2.0d0/(x*z*pi) * ( x*(z*z+y*y)**(0.5d0)*atan(x/(z*z+y*y)**(0.5d0))&
       +z*(x*x+y*y)**(0.5d0)*atan(z/(x*x+y*y)**(0.5d0))-x*y*atan(x/y) &
       -z*y*atan(z/y)+y*y/2.0d0*dlog((x*x+y*y)*(z*z+y*y)/(y*y*(x*x+z*z+y*y))) )

       if(pll.lt.0.0d0)write(6,*)'pll<0',pll,x,y,z
       if(pll.gt.1.0d0)write(6,*)'pll>1',pll,x,y,z
       pll=max(min(pll,1.0d0),0.0d0)

       goto 135
 134   continue
      pll=0.0d0
      write(6,*)'x or y or z is 0 in pll',x,y,z
 135   continue

      return
      end


!c------------------------------------------------------------------
!c  this is Lin Wu's F2: two parallel faces, one is x by z1, looking at
!c  the other (x by z2), and sharing a common boundary in the z
!c  dimension; y is the separation distance
      real(8) function F2(x,y,z1,z2)

       real(8) x,y,z1,z2,pll

       x=abs(x)
       y=abs(y)
       z1=abs(z1)
       z2=abs(z2)

       F2 = 1.0d0/2.0d0/x/z1* ( x*(z1+z2)*pll(x,y,(z1+z2))- x*z1*pll(x,y,z1)- x*z2*pll(x,y,z2) )

      return
      end


!c------------------------------------------------------------------
!c  this is Lin Wu's F3: two parallel faces, one is x by z1, looking at
!c  the other (x by z3), not sharing a common boundary in the z
!c  dimension (separated by z2), but still in the same x dimension;
!c  y is the separation distance
      real(8) function F3(xa,ya,z1a,z2a,z3a)

       real(8) x,y,z1,z2,z3,F2,pll
       real(8) xa,ya,z1a,z2a,z3a !! need fresh variables so passing a constant doesn't crash on reassign

       x=abs(xa)
       y=abs(ya)
       z1=abs(z1a)
       z2=abs(z2a)
       z3=abs(z3a)

       if(z2.eq.0.) then
        F3 = F2 (x,y,z1,z3)
       else
        F3 = (z1+z2+z3)/z1*pll(x,y,(z1+z2+z3)) - pll(x,y,z1)- F2(x,y,z1,z2) - (z2+z3)/z1* ( pll(x,y,(z2+z3))+ F2(x,y,(z2+z3),z1) )
       endif
      return
      end


!c------------------------------------------------------------------
!c  this is Lin Wu's F4: two parallel faces, one is x1 by z1, looking at
!c  the other (x2 by z2), sharing a common corner in the z and x
!c  dimensions; y is the separation distance
      real(8) function F4(x1,x2,y,z1,z2)

       real(8) x1,x2,y,z1,z2,pll,F2

       x1=abs(x1)
       x2=abs(x2)
       y=abs(y)
       z1=abs(z1)
       z2=abs(z2)

      F4 = 1.0d0/2.0d0/x1/z1* ( (x1+x2)*(z1+z2)*pll((x1+x2),y,(z1+z2))- (x1+x2)*z1*pll((x1+x2),y,z1)- &
      (x1+x2)*z2*pll((x1+x2),y,z2)- (x1+x2)*z2*F2((x1+x2),y,z2,z1)- x1*z1*F2(x1,y,z1,z2)- x2*z1*F2(x2,y,z1,z2) )

      return
      end


!c------------------------------------------------------------------
!c  this is Lin Wu's F5: two parallel faces, one is x1 by z1, looking at
!c  the other (x3 by z3), sharing no dimensions in common, x2 and z2 are
!c  their separation distances in the x and z dimensions; y is the
!c  separation distance
      real(8) function F5(x1a,x2a,x3a,ya,z1a,z2a,z3a)

       real(8) x1,x2,x3,y,z1,z2,z3,pt2,pt3,pt4,F4
       real(8) x1a,x2a,x3a,ya,z1a,z2a,z3a !! need local variables so passing a constant doesn't crash on reassign

       x1=abs(x1a)
       x2=abs(x2a)
       x3=abs(x3a)
       y=abs(ya)
       z1=abs(z1a)
       z2=abs(z2a)
       z3=abs(z3a)

       pt2 = 0.0d0
       pt3 = 0.0d0
       pt4 = 0.0d0

       if(x2.gt.0.1) pt3=F4(x1,x2,y,z1,(z2+z3))
       if(z2.gt.0.1) pt2=F4(x1,(x2+x3),y,z1,z2)
       if(x2.gt.0.1.and.z2.gt.0.1) pt4=F4(x1,x2,y,z1,z2)

       F5 = F4(x1,(x2+x3),y,z1,(z2+z3)) - pt2 - pt3 + pt4

      return
      end



!c------------------------------------------------------------------
!c  view factor from surface 1 of length y perpendicular to surface 2
!c  of length z, sharing a common edge of length x
      real(8) function per(x,y,z)
       real(8) x,y,z,pi,W,H

       x=abs(x)
       y=abs(y)
       z=abs(z)
       if(x.eq.0.0d0.or.y.eq.0.0d0.or.z.eq.0.0d0)goto 136

       W=y/x
       H=z/x

       pi = 4.0d0 * atan(1.0d0)
 
!c  Siegel and Howell / Modest
       per=1.0d0/pi/W*(W*atan(1.0d0/W)+H*atan(1.0d0/H)- (H*H+W*W)**(0.5d0)*atan(1.0d0/sqrt(H*H+W*W))&
       + 1.0d0/4.0d0*dlog((1.0d0+W*W)*(1.0d0+H*H)/(1.0d0+W*W+H*H)*(W*W*(1.0d0+W*W+H*H)/&
       (1.0d0+W*W)/(W*W+H*H))**(W*W)*(H*H*(1.0d0+H*H+W*W)/(1.0d0+H*H)/(H*H+W*W))**(H*H)))

       if(per.lt.0.0d0)write(6,*)'per<0',per,H,W,x,y,z
       if(per.gt.1.0d0)write(6,*)'per>1',per,H,W,x,y,z
       per=max(min(per,1.0d0),0.0d0)

       goto 137
 136   continue
      per=0.0d0
      write(6,*)'x or y or z is 0 in per',x,y,z
 137   continue

      return
      end


!c------------------------------------------------------------------
!c  Lin Wu's F7 (modified)
!c  view factor from surface 1 of length y2 perpendicular to surface 2
!c  of length z2, separated in the y dimension by y1, and in the z
!c  dimension by z1; both surfaces are aligned in the x dimension, and
!c  are have width x
      real(8) function F7(xa,y1a,y2a,z1a,z2a)
       real(8) x,y1,y2,z1,z2,per
       real(8) xa,y1a,y2a,z1a,z2a !! adding these because if you pass a constant value, the function crashes trying to reassign it

       x=abs(xa)
       y1=abs(y1a)
       y2=abs(y2a)
       z1=abs(z1a)
       z2=abs(z2a)

       if(y1*z1.eq.0.) then
        F7 = x*(y1+y2)*per(x,y1+y2,z1+z2)
        if(y1.ne.0.0d0) F7 = F7 - x*y1*per(x,y1,z1+z2)
        if(z1.ne.0.0d0) F7 = F7 - x*(y1+y2)*per(x,y1+y2,z1)
        F7 = F7 * 1.0d0/x/y2
       else
        F7 = 1.0d0/x/y2 * (x*(y1+y2)*per(x,y1+y2,z1+z2)+ x*y1*per(x,y1,z1) - x*(y1+y2)*per(x,y1+y2,z1)- x*y1*per(x,y1,z1+z2))
       endif

      return
      end


!c------------------------------------------------------------------
!c  Lin Wu's F8
!c  view factor from surface 1 of length y and width x1 perpendicular
!c  to surface 2 of length z and width x2, with corners touching in the
!c  x dimension
      real(8) function F8(x1,x2,y,z)
       real(8) x1,x2,y,z,per

       x1=abs(x1)
       x2=abs(x2)
       y=abs(y)
       z=abs(z)

       F8 = 1.0d0/2.0d0/x1/y * ((x1+x2)*y*per(x1+x2,y,z)- x1*y*per(x1,y,z)- x2*y*per(x2,y,z))

      return
      end

!    real(8) function absFunction(number)
!      real(8) number
!      if (number.lt.0) then
!          absFunction = -1 * number
!      else 
!          absFunction = number
!      endif
!      
!      return
!    end function absFunction

!c------------------------------------------------------------------
!c  Lin Wu's F9
!c  view factor from surface 1 of length y2 and width x1 perpendicular
!c  to surface 2 of length z2 and width x3, separate in the x,y, and z
!c  dimensions by x2, y1, and z1, respectively
      real(8) function F9(x1a,x2a,x3a,y1a,y2a,z1a,z2a)
       real(8) x1,x2,x3,y1,y2,z1,z2,F8
       real(8) x1a,x2a,x3a,y1a,y2a,z1a,z2a
       
!       if (x1a.lt.0) then
!          x1 = -1 * x1a
!       else 
!          x1 = x1a
!       endif
   
       x1=abs(x1a)
       x2=abs(x2a)
       x3=abs(x3a)
       y1=abs(y1a)
       y2=abs(y2a)
       z1=abs(z1a)
       z2=abs(z2a)

!c to get rid of terms that cause problems if one or more of z1,y1,x2
!c are zero (otherwise nan's generated by 'per' aren't eliminated)

       if(y1*z1*x2.eq.0.0d0) then
        F9=(y1+y2)/y2 * F8(x1,x2+x3,y1+y2,z1+z2)
        if(y1.ne.0.0d0) F9 = F9 - y1/y2 * F8(x1,x2+x3,y1,z1+z2)
        if(z1.ne.0.0d0) F9 = F9 - (y1+y2)/y2 * F8(x1,x2+x3,y1+y2,z1)
        if(x2.ne.0.0d0) F9 = F9 - (y1+y2)/y2 * F8(x1,x2,y1+y2,z1+z2)
        if(y1.ne.0.0d0.and.z1.ne.0.0d0) F9 = F9 + y1/y2 * F8(x1,x2+x3,y1,z1)
        if(y1.ne.0.0d0.and.x2.ne.0.0d0) F9 = F9 + y1/y2 * F8(x1,x2,y1,z1+z2)
        if(x2.ne.0.0d0.and.z1.ne.0.0d0) F9 = F9 + (y1+y2)/y2* F8(x1,x2,y1+y2,z1)

       else
        F9 = (y1+y2)/y2 * (F8(x1,x2+x3,y1+y2,z1+z2)- F8(x1,x2,y1+y2,z1+z2)- F8(x1,x2+x3,y1+y2,z1)+ F8(x1,x2,y1+y2,z1))- y1/y2 * &
        (F8(x1,x2+x3,y1,z1+z2) - F8(x1,x2+x3,y1,z1)- F8(x1,x2,y1,z1+z2) + F8(x1,x2,y1,z1))

       endif

      return
      end

!------------------------------------------------------------------

      SUBROUTINE SUNPOS(JDAY,TM,LAT,ZEN,AZIM,CZ,INOT,CA)

      implicit none

      REAL DEC,HL,CZ,CA,THETA,PI,LAT,ZEN,AZIM,INOT,TM,THETA_INOT
      INTEGER JDAY
      logical SH
      REAL HR_RAD
      PI=ACOS(-1.0)
      SH=.false.
      HR_RAD=15.*PI/180.
      THETA = real(JDAY-1)*(2.*PI)/365.
      THETA_INOT=THETA
! for southern hemisphere:
      if(LAT.lt.0) then
       THETA=amod(THETA+PI,2.*PI)
       SH=.true.
       LAT=abs(LAT)
      endif 
! declination angle
      DEC = 0.006918-0.399912*COS(THETA)+0.070257*SIN(THETA)-0.006758*COS(2*THETA)+0.000907*SIN(2*THETA)-&
     0.002697*COS(3*THETA)+0.00148*SIN(3*THETA)

! all the changes are from Stull, Meteorology for Scientists and
! Engineers 2000 - NOTE: the current definition of HL give the solar
! position based on local mean solar time - to have solar position as
! as function of standard time in the time zone, must use Stull's
! equation 2.9 on p. 26
      HL=TM*HR_RAD
! cos(solar zenith)
      CZ = (SIN(LAT)*SIN(DEC))-(COS(LAT)*COS(DEC)*COS(HL))
! solar zenith
      ZEN=ACOS(CZ)
! cos(azimuth angle)
      CA = max(-1.,min(1.,(SIN(DEC)-SIN(LAT)*CZ)/(COS(LAT)*SIN(ZEN))))
! azimuth angle
      AZIM=ACOS(CA)
      IF(TM.gt.12.) AZIM=2.*PI-AZIM
! southern hemisphere:
      if(SH) then
       if(azim.le.PI) then
        azim=PI-azim
       else
        azim=3.*PI-azim
       endif
       LAT=-LAT
      endif
! incoming flux density based on solar geometry (no atmosphere yet)
      INOT=1365.*(1.0001+0.034221*COS(THETA_INOT)+0.001280*SIN(THETA_INOT)+0.000719&
     *COS(2.*THETA_INOT)+0.000077*SIN(2.*THETA_INOT))

      END

!------------------------------------------------------
!
!     SUBROUTINE CLRSKY DETERMINES THE SOLAR RADIATION RECEIVED AT THE
!     SURFACE UNDER CLEAR SKY CONDITIONS (clouds have been added)
!
      SUBROUTINE CLRSKY(CZ,PRESS,ZEN,AIR,DEW,INOT,DR1,DF1,GL1,CA,JDAY,alb_sfc,cloudtype,abs_aero,Ktotfrc,DR1F)

      implicit none

      REAL PI,ZEND,TR,AHAT,UW,AW,TA,WO,BAA,ALPHAB,XO,TO,M
      REAL DRR,DA,DS,ZEN,PRESS,AIR,DEW,DR1,DF1,Ktotfrc,DR1F,DF1F,I_I0F
      REAL GL1,INOT,AO,CZ,X2,CA,DR1_save,zendiff,weight
      real alb_sfc,visib,TAprime,ozone,MR,I_I0,TCL,abs_aero
      INTEGER JDAY,cloudtype

      PI=3.141593

      DR1=0.
      DF1=0.
      GL1=0.
      abs_aero=0.

! this is the cloud transmissivity!!! (Haurwitz, 1948), in Atwater and
! Brown JAM March 1974 (all assumed at 100% cloud cover)
      if(cloudtype.eq.0) then
! clear: (also affects ALPHAB (below) and comment out Orgill and
!  Hollands (below)
       TCL=1.
      elseif(cloudtype.eq.1) then
! cirrus:
       TCL=0.8717-0.0179*(PRESS/(101.325*CZ))
      elseif(cloudtype.eq.2) then
! cirrostratus:
       TCL=0.9055-0.0638*(PRESS/(101.325*CZ))
      elseif(cloudtype.eq.3) then
! altocumulus:
       TCL=0.5456-0.0236*(PRESS/(101.325*CZ))
      elseif(cloudtype.eq.4) then
! altostratus:
       TCL=0.4130-0.0014*(PRESS/(101.325*CZ))
      elseif(cloudtype.eq.7) then
! cumulonimbus:
       TCL=0.2363+0.0145*(PRESS/(101.325*CZ))
      elseif(cloudtype.eq.5) then
! stratocumulus/cumulus:
       TCL=0.3658-0.0149*(PRESS/(101.325*CZ))
      elseif(cloudtype.eq.6) then
! thick stratus (Ns?):
       TCL=0.2684-0.0101*(PRESS/(101.325*CZ))
      else
       write(6,*)'cloudtype must be between 0 and 7, cloudtype = ',cloudtype
      endif

      ZEND=ZEN*180/PI
!CC see Iqbal p. 175-195 for much of this parameterization
!      M repr. thickness of atm. through which solar beam travels(rel. mass)
      if(zend.ge.90.0) goto 945
      MR=1./(CZ+(0.15*((93.885-ZEND)**(-1.253))))
      if(MR.lt.1.) then
       write(6,*)'problem with solar routine (MR>0), MR = ',MR
       stop
      endif
      M=(PRESS/101.325)*MR
!     TR is the transmissivity due to Rayleigh scattering

! SO THAT SMALL SUN ELEVATION ANGLES DON'T GIVE LARGE KDIR FOR VERTICAL
! SURFACES DUE TO TR INCREASING ABOVE 1 FOR ZENITH ANGLES APPROACHING 90
        TR=EXP(-0.0903*(M**0.84)*(1.+M-M**(1.0074)))
        if(TR.gt.1.0) then
         write(6,*)'TR.gt.1,TR,M,MR,ZEND=',TR,M,MR,ZEND
         stop
      endif

      AHAT=2.2572
!     UW is a water vapour factor...DEW is dewpoint in Celsius
      UW=0.1*EXP((0.05454*DEW)+AHAT)
      X2=UW*MR*((PRESS/101.325)**0.75)*((273.15/(273.15+AIR))**0.5)
      AW=(2.9*X2)/(((1.+141.5*X2)**0.635)+5.925*X2)

!  visibility in km (70=clean,20=turbid, 6=very turbid)
      visib=20.
      TA=(0.97-1.265*visib**(-0.66))**(M**0.9)

!  the following value is fraction scattered (vs absorbed)
!  (0.9 for rural/agricultural,0.6 for cities)
      WO=0.70

!  for multiple reflection of diffuse, assume rel. air mass 1.66
      TAprime=(0.97-1.265*visib**(-0.66))**((1.66)**(0.9))

!  ratio of forward scattering
      BAA=-0.3012*CZ**2+0.7368*CZ+0.4877
      ALPHAB=max(0.0685+(1.-TCL),0.0685+(1.-BAA)*(1.-TAprime)*WO)

!  ozone (in cm) from Table in Iqbal p.89
!  first is for lat=40, second for lat=50
      ozone=1.595e-8*jday**3-0.9642e-5*jday**2+0.001458*jday+0.2754
!      ozone=2.266e-8*jday**3-1.347e-5*jday**2+0.001958*jday+0.2957
      XO=10.*ozone*MR
      AO=((0.1082*XO)/((1.+13.86*XO)**0.805))+(0.00658*XO)/(1.+(10.36*XO)**3)+(0.002118*XO)&
     /(1.+(0.0042*XO)+(0.0000323*XO*XO))
      TO=1.-AO

!
!     CALCULATE DIRECT, DIFFUSE, AND GLOBAL RADIATION
!
!  direct
      DR1=INOT*CZ*(TO*TR-AW)*TA*TCL
!  contributions to diffuse
      DRR=INOT*CZ*TO*(TA/2.0)*(1.-TR)*TCL
      DA=INOT*CZ*WO*BAA*(1.-TA)*(TO*TR-AW)*TCL
!  diffuse due to multiple reflection between the surface and the atmosphere
      DS=alb_sfc*ALPHAB*(DR1+DRR+DA)/(1.-alb_sfc*ALPHAB)
!  total incident diffuse
      DF1=DRR+DA+DS
!  aerosol absorption
      ABS_AERO=INOT*CZ*(TO*TR-AW)*(1.-TA)*(1.-WO)

      DR1_save=DR1

      if(DR1.lt.0.0.or.DR1.gt.1400.)then
       DR1=0.
       ABS_AERO=0.
      endif
      if(DF1.lt.0.0.or.DF1.gt.1400.)then
       DF1=0.
       ABS_AERO=0.
      endif

      GL1=DR1+DF1
      if(cloudtype.gt.0)then
!  Orgill and Hollands correlation taken from Iqbal p.269
       if(INOT*CZ.le.0.)then
        goto 47
       else
        I_I0=GL1/(INOT*CZ)
       endif
!  This is a polynomial fit to Orgill and Hollands
       if(I_I0.ge.0.85)then
        DF1=0.159873*GL1
       else
        DF1=GL1*(-27.59*I_I0**6+66.449*I_I0**5-48.232*I_I0**4+7.7246*I_I0**3+1.3433*I_I0**2-0.5357*I_I0+1.)
       endif
       DR1=GL1-DF1
      endif

!  Orgill and Hollands changes DR1, which can cause problems later when we
!  find the beam solar flux density (perpendicular to the incoming solar)
!  from the direct solar flux density (perpendicular to a horizontal surface);
!  Therefore, for small solar elevation angles we use DR1 as originally
!  calculated above (before the Orgill and Hollands parameterization), and
!  we interpolate between the two so that they match at solar elevation of 10 degrees
      if(zend.gt.85.0) then
       DR1=min(DR1_save,GL1)
       DF1=GL1-DR1
      elseif(zend.gt.80.0.and.zend.le.85.) then
       zendiff=85.-zend
       weight=1.-sqrt(1.-zendiff*zendiff/25.)
       DR1=min(DR1*weight+DR1_save*(1.-weight),GL1)
       DF1=GL1-DR1
      else
!      should be fine as is
      endif


!  Now Orgill and Hollands correlation for observed (forcing) radiation
      if (Ktotfrc.gt.0.) then 
       if(INOT*CZ.le.0.)then
        goto 47
       else
        I_I0F=Ktotfrc/(INOT*CZ)
       endif
!  This is a polynomial fit to Orgill and Hollands
       if(I_I0F.ge.0.85)then
        DF1F=0.159873*Ktotfrc
       else
        DF1F=Ktotfrc*(-27.59*I_I0F**6+66.449*I_I0F**5-48.232*I_I0F**4+7.7246*I_I0F**3+1.3433*I_I0F**2-0.5357*I_I0F+1.)
       endif
       DR1F=Ktotfrc-DF1F
      endif


 47   continue
 945  continue
      END


!------------------------------------------------------

      SUBROUTINE SFC_RI(dz,Thi,Tlo,Uhi,Ri)

      implicit none

      real dz,Thi,Tlo,Uhi,g,Ri,Tcorrhi

! correct for lapse rate (equivalent to using theta values for
! Thi and Tlo for T difference)
      Tcorrhi=Thi+9.806/1004.67*dz

      Ri=9.806*dz*(Tcorrhi-Tlo)*2./(Thi+Tlo)/(max(Uhi,1e-3))**2

      return
      end

!---------------------------------------------------------
! Mascart (1995) BLM heat and momentum transfer coefficients

      SUBROUTINE HTC(Ri,u,z,z0m,z0h,httc_out,Fh)

! inputs
      real Ri,u,z,z0m,z0h
! outputs
      real httc_out,Fh
! other variables
      real R,mu,Cstarh,ph,lnzz0m,lnzz0h,aa,Ch


! from Louis (1979):
      R=0.74

! checks: Mascart procedure not so good if these to conditions
! are not met (i.e. z0m/z0h must be between 1 and 200)
      z0h=max(z0m/200.,z0h)
      mu=max(0.,alog(z0m/z0h))

      Cstarh=3.2165+4.3431*mu+0.536*mu**2-0.0781*mu**3
      ph=0.5802-0.1571*mu+0.0327*mu**2-0.0026*mu**3

      lnzz0m=alog(z/z0m)
      lnzz0h=alog(z/z0h)
      aa=(0.4/lnzz0m)**2

      Ch=Cstarh*aa*9.4*(lnzz0m/lnzz0h)*(z/z0h)**ph

      if(Ri.gt.0.) then
       Fh=lnzz0m/lnzz0h*(1.+4.7*Ri)**(-2)
      else
       Fh=lnzz0m/lnzz0h*(1.-9.4*Ri/(1.+Ch*(abs(Ri))**(0.5)))
      endif

      httc_out=u*aa/R*Fh

      return
      end


!---------------------------------------------------------------
      SUBROUTINE CD (Ri,z,z0m,z0h,cd_out,Fm)

! inputs
      real Ri,z,z0m,z0h
! outputs
      real cd_out,Fm
! other variables
      real mu,Cstarm,pm,lnzz0m,aa,Cm


! checks: Mascart procedure not so good if these to conditions
! are not met (i.e. z0m/z0h must be between 1 and 200)
      z0h=max(z0m/200.,z0h)
      mu=max(0.,alog(z0m/z0h))

      Cstarm=6.8741+2.6933*mu-0.3601*mu**2+0.0154*mu**3
      pm=0.5233-0.0815*mu+0.0135*mu**2-0.001*mu**3

      lnzz0m=alog(z/z0m)
      aa=(0.4/(lnzz0m))**2

      Cm=Cstarm*aa*9.4*(z/z0m)**pm

      if(Ri.gt.0.) then
       Fm=(1.+4.7*Ri)**(-2)
      else
       Fm=1.-9.4*Ri/(1.+Cm*(abs(Ri))**(0.5))
      endif

      cd_out=aa*Fm

      return
      end

!! if vegetation is found, reverse ray trace from the top of the domain to find vegetation intersections and
!! distribute sunlit factors to sunlit vegetation and ultimately to the original surface where the
!! forward ray trace originated.
     !subroutine reverseRayTrace(x,y,z,f,i,xt,xinc,yt,yinc,zt,zinc,xtest,ytest,ztest,bh,al2,aw2,veg_shade,timeis,yd_actual)
      subroutine reverseRayTrace(xt,xinc,yt,yinc,zt,zinc,xtest,ytest,ztest,bh,al2,aw2,veg_shade,timeis,yd_actual)
          use TUFConstants
          use MaespaConfigState , only :  maespaConfigTreeMapState
          use ReadMaespaConfigs
          

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
      
      logical veg_shade
      dimension veg_shade(0:al2+1,0:aw2+1,0:bh+1)
      TYPE(maespaConfigTreeMapState) :: treeState  
      integer i
      
      transmissionPercentage = 1.0

      ztestRev = ZTEST
      ytestRev = YTEST
      xtestRev = XTEST
      
      xtRev = xt
      ytRev = yt
      ztRev = zt
      
      xincRev = xinc*(-1.0)
      yincRev = yinc*(-1.0)
      zincRev = zinc*(-1.0)
      
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
          
          !print *,'reverse ray,',x,y,z,f,i,xtrev,xincrev,ytrev,yincrev,ztrev,zincrev,xtestrev,ytestrev,ztestrev
          print *,'reverse ray,',xtrev,xincrev,ytrev,yincrev,ztrev,zincrev,xtestrev,ytestrev,ztestrev
          
          
!          print *,'veg shade 0'
!          do i = 15,19
!            print *,veg_shade(15:19,i,0)
!          enddo
!          
!          print *,'veg shade 1'
!          do i = 15,19
!            print *,veg_shade(15:19,i,1)
!          enddo
!          
!          print *,'veg shade 2'
!          do i = 15,19
!            print *,veg_shade(15:19,i,2)
!          enddo
!          
!          print *,'veg shade 3'
!          do i = 15,19
!            print *,veg_shade(15:19,i,3)
!          enddo     
!
!          print *,'veg shade 4'
!          do i = 15,19
!            print *,veg_shade(15:19,i,4)
!          enddo 
!          
!          print *,'veg shade 5'
!          do i = 15,19
!            print *,veg_shade(15:19,i,5)
!          enddo                 
          
          if (veg_shade(xtestRev,ytestRev,ztestRev))then
              print *,'reverse ray found vegetation at ',xtestRev,ytestRev,ztestRev
              
              !! find what tree this is
              call findTreeFromConfig(xtestRev,ytestRev,ztestRev,treeState,timeis,yd_actual,treeConfigLocation)
             ! veg found,          36          35           0           1        1754   35.749046     -2.08802776E-05   41.221718      0.11943572       8.5210857      0.16042165              36          41           9           8          77          77
             !   reverse ray found vegetation at           36          35           1
             !   reverse ray found vegetation at           36          35           1
             !   reverse ray found vegetation at           36          35           0
              if (treeConfigLocation.eq.-1) then
                   print *,'did not find tree ', xtestRev,ytestRev,ztestRev
              else
!                print *,'tree found ', treeConfigLocation,treeState%phyFileNumber(treeConfigLocation), &
!                  treeState%strFileNumber(treeConfigLocation), treeState%treesfileNumber(treeConfigLocation)
              endif
              
              !! at this point, treeConfigLocation gives pointers to the tree configuration. Calculate transmission, etc 
              !! and pass it back to the calling function so it can update sunlit factor
              call calculateTransmissionsOfTree(yd_actual,timeis,treeState,treeConfigLocation,transmissionPercentage)
              !! probably also directly update the tree surfaces with absorbed radiation and disregard the reflected
              !! radiation (for now, maybe in the future see if it can be reallocated)
              
              
          endif
          
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


