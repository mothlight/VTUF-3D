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

MODULE UTCI

    contains

function calculateRH(tempC,  vapor)      
    implicit none      
    real tempC
    real vapor
    real calculateRH
      
    calculateRH = 100 * vapor / (7.5152E8 * exp(-42809/(8.314*(tempC + 273.0))) )
    
    RETURN
END function calculateRH


function calcTmrt(kd,ld,ku,lu)   
    implicit none      
    real kd,ld,ku,lu   
    real calcTmrt
    real downAngular
    real upAngular
    real AbsorbCoeffSW
    real EmmisHumanBody
    real k 
    real l 
    real s 
    
    downAngular = 0.5
    upAngular = 0.5
    AbsorbCoeffSW=0.7
    EmmisHumanBody=0.97

    k = kd * downAngular + ku * upAngular
    l = ld * downAngular + lu * upAngular
    s = AbsorbCoeffSW * k + EmmisHumanBody * l
    calcTmrt = ( (s / (EmmisHumanBody * 5.67E-8) ) **0.25 ) -273.15

    RETURN
END function calcTmrt


function fTd(Ta, RH)
    implicit none      
    real Ta, RH 
    real fTd
    real RHD
    !Calculation of dew point from RH
    RHD=RH/100
    fTd = 237.3 * (log(RHD) / 17.27 + Ta / (237.3 + Ta)) / (1 - log(RHD) / 17.27 - Ta / (237.3 + Ta))
    
    return 
end function fTd


function fUTCI2(Ta, ws, RH, Tmrt)
    implicit none      
    real Ta, ws, RH, Tmrt 
    real fUTCI2
    real D_Tmrt
    real PA
    real Td
    real ET1, ET3, ET4, ET5, ET6, ET7, ET8, ET9
    
    
    D_Tmrt = 0
    !UTCI calculation using full formula from utci.org website
    
    Td = fTd(Ta, RH)
    

    !# UTCI, Version a 0.002, October 2009
    !# Copyright (C) 2009  Peter Broede
    !# Program for calculating UTCI Temperature (UTCI)
    !# released for public use after termination of COST Action 730
    !# Copyright (C) 2009  Peter Broede
    !# This program is distributed in the hope that it will be useful,
    !# but WITHOUT ANY WARRANTY; without even the implied warranty of
    !# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 !# DOUBLE PRECISION Function value is the UTCI in degree Celsius
 !# computed by a 6th order approximating polynomial from the 4 Input paramters
 !#
 !# Input parameters (all of type DOUBLE PRECISION)
 !# - Ta       : air temperature, degree Celsius
 !# - ehPa    : water vapour presure, hPa=hecto Pascal
 !# - Tmrt   : mean radiant temperature, degree Celsius
 !# - va10m  : wind speed 10 m above ground level in m/s
 !#
 !  UTCI_approx, Version a 0.002, October 2009
 !#  Copyright (C) 2009  Peter Broede


    PA = 0.6108 * exp(17.29 * Td / (Td + 237.3)) !takes Td and changes it into Pa - vapour pressure in hPa)
    D_Tmrt = Tmrt - Ta
    
        !# calculate 6th order polynomial as approximation
    ET1 = Ta + 0.607562052 - 0.0227712343 * Ta + 0.000806470249 * Ta * Ta - 0.000154271372 * Ta * Ta * Ta - 0.00000324651735 * Ta * Ta * Ta * Ta &
    + 7.32602852E-08 * Ta * Ta * Ta * Ta * Ta + 1.35959073E-09 * Ta * Ta * Ta * Ta * Ta * Ta - 2.2583652 * ws + 0.0880326035 * Ta * ws &
    + 0.00216844454 * Ta * Ta * ws - 0.0000153347087 * Ta * Ta * Ta * ws - 0.000000572983704 * Ta * Ta * Ta * Ta * ws - 2.55090145E-09 * Ta * Ta * Ta * Ta * Ta * ws &
    - 0.751269505 * ws * ws - 0.00408350271 * Ta * ws * ws - 0.0000521670675 * Ta * Ta * ws * ws + 0.00000194544667 * Ta * Ta * Ta * ws * ws &
    + 1.14099531E-08 * Ta * Ta * Ta * Ta * ws * ws + 0.158137256 * ws * ws * ws - 0.0000657263143 * Ta * ws * ws * ws + 0.000000222697524 * Ta * Ta * ws * ws * ws &
    - 4.16117031E-08 * Ta * Ta * Ta * ws * ws * ws - 0.0127762753 * ws * ws * ws * ws + 0.00000966891875 * Ta * ws * ws * ws * ws + 2.52785852E-09 * Ta * Ta * ws * ws * ws * ws &
    + 0.000456306672 * ws * ws * ws * ws * ws - 0.000000174202546 * Ta * ws * ws * ws * ws * ws - 0.00000591491269 * ws * ws * ws * ws * ws * ws &
    + 0.398374029 * D_Tmrt + 0.000183945314 * Ta * D_Tmrt - 0.00017375451 * Ta * Ta * D_Tmrt - 0.000000760781159 * Ta * Ta * Ta * D_Tmrt &
    + 3.77830287E-08 * Ta * Ta * Ta * Ta * D_Tmrt + 5.43079673E-10 * Ta * Ta * Ta * Ta * Ta * D_Tmrt - 0.0200518269 * ws * D_Tmrt + 0.000892859837 * Ta * ws * D_Tmrt &
    + 0.00000345433048 * Ta * Ta * ws * D_Tmrt - 0.000000377925774 * Ta * Ta * Ta * ws * D_Tmrt - 1.69699377E-09 * Ta * Ta * Ta * Ta * ws * D_Tmrt &
    + 0.000169992415 * ws * ws * D_Tmrt - 0.0000499204314 * Ta * ws * ws * D_Tmrt + 0.000000247417178 * Ta * Ta * ws * ws * D_Tmrt &
    + 1.07596466E-08 * Ta * Ta * Ta * ws * ws * D_Tmrt + 0.0000849242932 * ws * ws * ws * D_Tmrt + 0.00000135191328 * Ta * ws * ws * ws * D_Tmrt &
    - 6.21531254E-09 * Ta * Ta * ws * ws * ws * D_Tmrt - 0.00000499410301 * ws * ws * ws * ws * D_Tmrt - 1.89489258E-08 * Ta * ws * ws * ws * ws * D_Tmrt &
    + 8.15300114E-08 * ws * ws * ws * ws * ws * D_Tmrt + 0.00075504309 * D_Tmrt * D_Tmrt
    
    ET3 = -0.0000565095215 * Ta * D_Tmrt * D_Tmrt + (-0.000000452166564) * Ta * Ta * D_Tmrt * D_Tmrt + (2.46688878E-08) * Ta * Ta * Ta * D_Tmrt * D_Tmrt &
    + (2.42674348E-10) * Ta * Ta * Ta * Ta * D_Tmrt * D_Tmrt + (0.00015454725) * ws * D_Tmrt * D_Tmrt + (0.0000052411097) * Ta * ws * D_Tmrt * D_Tmrt &
    + (-8.75874982E-08) * Ta * Ta * ws * D_Tmrt * D_Tmrt + (-1.50743064E-09) * Ta * Ta * Ta * ws * D_Tmrt * D_Tmrt + (-0.0000156236307) * ws * ws * D_Tmrt * D_Tmrt &
    + (-0.000000133895614) * Ta * ws * ws * D_Tmrt * D_Tmrt + (2.49709824E-09) * Ta * Ta * ws * ws * D_Tmrt * D_Tmrt + (0.000000651711721) * ws * ws * ws * D_Tmrt * D_Tmrt &
    + (1.94960053E-09) * Ta * ws * ws * ws * D_Tmrt * D_Tmrt + (-1.00361113E-08) * ws * ws * ws * ws * D_Tmrt * D_Tmrt + (-0.0000121206673) * D_Tmrt * D_Tmrt * D_Tmrt &
    + (-0.00000021820366) * Ta * D_Tmrt * D_Tmrt * D_Tmrt + (7.51269482E-09) * Ta * Ta * D_Tmrt * D_Tmrt * D_Tmrt + (9.79063848E-11) * Ta * Ta * Ta * D_Tmrt * D_Tmrt * D_Tmrt &
    + (0.00000125006734) * ws * D_Tmrt * D_Tmrt * D_Tmrt + (-1.81584736E-09) * Ta * ws * D_Tmrt * D_Tmrt * D_Tmrt + (-3.52197671E-10) * Ta * Ta * ws * D_Tmrt * D_Tmrt * D_Tmrt &
    + (-0.000000033651463) * ws * ws * D_Tmrt * D_Tmrt * D_Tmrt + (1.35908359E-10) * Ta * ws * ws * D_Tmrt * D_Tmrt * D_Tmrt + (4.1703262E-10) * ws * ws * ws * D_Tmrt * D_Tmrt * D_Tmrt &
    + (-1.30369025E-09) * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt
        
    ET4 = 4.13908461E-10 * Ta * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt + (9.22652254E-12) * Ta * Ta * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt &
    + (-5.08220384E-09) * ws * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt + (-2.24730961E-11) * Ta * ws * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt &
    + (1.17139133E-10) * ws * ws * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt + (6.62154879E-10) * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt &
    + (4.0386326E-13) * Ta * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt + (1.95087203E-12) * ws * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt &
    + (-4.73602469E-12) * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt + (5.12733497) * PA + (-0.312788561) * Ta * PA &
    + (-0.0196701861) * Ta * Ta * PA + (0.00099969087) * Ta * Ta * Ta * PA + (0.00000951738512) * Ta * Ta * Ta * Ta * PA + (-0.000000466426341) * Ta * Ta * Ta * Ta * Ta * PA &
    + (0.548050612) * ws * PA + (-0.00330552823) * Ta * ws * PA + (-0.0016411944) * Ta * Ta * ws * PA + (-0.00000516670694) * Ta * Ta * Ta * ws * PA &
    + (0.000000952692432) * Ta * Ta * Ta * Ta * ws * PA + (-0.0429223622) * ws * ws * PA + (0.00500845667) * Ta * ws * ws * PA + (0.00000100601257) * Ta * Ta * ws * ws * PA &
    + (-0.00000181748644) * Ta * Ta * Ta * ws * ws * PA + (-0.00125813502) * ws * ws * ws * PA
        
    ET5 = -0.000179330391 * Ta * ws * ws * ws * PA + (0.00000234994441) * Ta * Ta * ws * ws * ws * PA + (0.000129735808) * ws * ws * ws * ws * PA &
    + (0.0000012906487) * Ta * ws * ws * ws * ws * PA + (-0.00000228558686) * ws * ws * ws * ws * ws * PA + (-0.0369476348) * D_Tmrt * PA &
    + (0.00162325322) * Ta * D_Tmrt * PA + (-0.000031427968) * Ta * Ta * D_Tmrt * PA + (0.00000259835559) * Ta * Ta * Ta * D_Tmrt * PA &
    + (-4.77136523E-08) * Ta * Ta * Ta * Ta * D_Tmrt * PA + (0.0086420339) * ws * D_Tmrt * PA + (-0.000687405181) * Ta * ws * D_Tmrt * PA &
    + (-0.00000913863872) * Ta * Ta * ws * D_Tmrt * PA + (0.000000515916806) * Ta * Ta * Ta * ws * D_Tmrt * PA + (-0.0000359217476) * ws * ws * D_Tmrt * PA &
    + (0.0000328696511) * Ta * ws * ws * D_Tmrt * PA + (-0.000000710542454) * Ta * Ta * ws * ws * D_Tmrt * PA + (-0.00001243823) * ws * ws * ws * D_Tmrt * PA &
    + (-0.000000007385844) * Ta * ws * ws * ws * D_Tmrt * PA + (0.000000220609296) * ws * ws * ws * ws * D_Tmrt * PA + (-0.00073246918) * D_Tmrt * D_Tmrt * PA &
    + (-0.0000187381964) * Ta * D_Tmrt * D_Tmrt * PA + (0.00000480925239) * Ta * Ta * D_Tmrt * D_Tmrt * PA + (-0.000000087549204) * Ta * Ta * Ta * D_Tmrt * D_Tmrt * PA &
    + (0.000027786293) * ws * D_Tmrt * D_Tmrt * PA
        
    ET6 = -0.00000506004592 * Ta * ws * D_Tmrt * D_Tmrt * PA + (0.000000114325367) * Ta * Ta * ws * D_Tmrt * D_Tmrt * PA + (0.00000253016723) * ws * ws * D_Tmrt * D_Tmrt * PA &
    + (-1.72857035E-08) * Ta * ws * ws * D_Tmrt * D_Tmrt * PA + (-3.95079398E-08) * ws * ws * ws * D_Tmrt * D_Tmrt * PA + (-0.000000359413173) * D_Tmrt * D_Tmrt * D_Tmrt * PA &
    + (0.000000704388046) * Ta * D_Tmrt * D_Tmrt * D_Tmrt * PA + (-1.89309167E-08) * Ta * Ta * D_Tmrt * D_Tmrt * D_Tmrt * PA + (-0.000000479768731) * ws * D_Tmrt * D_Tmrt * D_Tmrt * PA &
    + (7.96079978E-09) * Ta * ws * D_Tmrt * D_Tmrt * D_Tmrt * PA + (1.62897058E-09) * ws * ws * D_Tmrt * D_Tmrt * D_Tmrt * PA + (3.94367674E-08) * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt * PA &
    + (-1.18566247E-09) * Ta * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt * PA + (3.34678041E-10) * ws * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt * PA &
    + (-1.15606447E-10) * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt * PA + (-2.80626406) * PA * PA + (0.548712484) * Ta * PA * PA + (-0.0039942841) * Ta * Ta * PA * PA &
    + (-0.000954009191) * Ta * Ta * Ta * PA * PA + (0.0000193090978) * Ta * Ta * Ta * Ta * PA * PA + (-0.308806365) * ws * PA * PA + (0.0116952364) * Ta * ws * PA * PA &
    + (0.000495271903) * Ta * Ta * ws * PA * PA + (-0.0000190710882) * Ta * Ta * Ta * ws * PA * PA + (0.00210787756) * ws * ws * PA * PA
        
    ET7 = -0.000698445738 * Ta * ws * ws * PA * PA + (0.0000230109073) * Ta * Ta * ws * ws * PA * PA + (0.00041785659) * ws * ws * ws * PA * PA &
    + (-0.0000127043871) * Ta * ws * ws * ws * PA * PA + (-0.00000304620472) * ws * ws * ws * ws * PA * PA + (0.0514507424) * D_Tmrt * PA * PA &
    + (-0.00432510997) * Ta * D_Tmrt * PA * PA + (0.0000899281156) * Ta * Ta * D_Tmrt * PA * PA + (-0.000000714663943) * Ta * Ta * Ta * D_Tmrt * PA * PA &
    + (-0.000266016305) * ws * D_Tmrt * PA * PA + (0.000263789586) * Ta * ws * D_Tmrt * PA * PA + (-0.00000701199003) * Ta * Ta * ws * D_Tmrt * PA * PA &
    + (-0.000106823306) * ws * ws * D_Tmrt * PA * PA + (0.00000361341136) * Ta * ws * ws * D_Tmrt * PA * PA + (0.000000229748967) * ws * ws * ws * D_Tmrt * PA * PA &
    + (0.000304788893) * D_Tmrt * D_Tmrt * PA * PA + (-0.0000642070836) * Ta * D_Tmrt * D_Tmrt * PA * PA + (0.00000116257971) * Ta * Ta * D_Tmrt * D_Tmrt * PA * PA &
    + (0.00000768023384) * ws * D_Tmrt * D_Tmrt * PA * PA + (-0.000000547446896) * Ta * ws * D_Tmrt * D_Tmrt * PA * PA + (-0.000000035993791) * ws * ws * D_Tmrt * D_Tmrt * PA * PA &
    + (-0.00000436497725) * D_Tmrt * D_Tmrt * D_Tmrt * PA * PA + (0.000000168737969) * Ta * D_Tmrt * D_Tmrt * D_Tmrt * PA * PA + (2.67489271E-08) * ws * D_Tmrt * D_Tmrt * D_Tmrt * PA * PA &
    + (3.23926897E-09) * D_Tmrt * D_Tmrt * D_Tmrt * D_Tmrt * PA * PA
        
    ET8 = -0.0353874123 * PA * PA * PA + (-0.22120119) * Ta * PA * PA * PA + (0.0155126038) * Ta * Ta * PA * PA * PA + (-0.000263917279) * Ta * Ta * Ta * PA * PA * PA &
    + (0.0453433455) * ws * PA * PA * PA + (-0.00432943862) * Ta * ws * PA * PA * PA + (0.000145389826) * Ta * Ta * ws * PA * PA * PA &
    + (0.00021750861) * ws * ws * PA * PA * PA + (-0.0000666724702) * Ta * ws * ws * PA * PA * PA + (0.000033321714) * ws * ws * ws * PA * PA * PA &
    + (-0.00226921615) * D_Tmrt * PA * PA * PA + (0.000380261982) * Ta * D_Tmrt * PA * PA * PA + (-5.45314314E-09) * Ta * Ta * D_Tmrt * PA * PA * PA &
    + (-0.000796355448) * ws * D_Tmrt * PA * PA * PA + (0.0000253458034) * Ta * ws * D_Tmrt * PA * PA * PA + (-0.00000631223658) * ws * ws * D_Tmrt * PA * PA * PA &
    + (0.000302122035) * D_Tmrt * D_Tmrt * PA * PA * PA + (-0.00000477403547) * Ta * D_Tmrt * D_Tmrt * PA * PA * PA + (0.00000173825715) * ws * D_Tmrt * D_Tmrt * PA * PA * PA &
    + (-0.000000409087898) * D_Tmrt * D_Tmrt * D_Tmrt * PA * PA * PA + (0.614155345) * PA * PA * PA * PA + (-0.0616755931) * Ta * PA * PA * PA * PA &
    + (0.00133374846) * Ta * Ta * PA * PA * PA * PA + (0.00355375387) * ws * PA * PA * PA * PA + (-0.000513027851) * Ta * ws * PA * PA * PA * PA 
        
    ET9 = 0.000102449757 * ws * ws * PA * PA * PA * PA + (-0.00148526421) * D_Tmrt * PA * PA * PA * PA + (-0.0000411469183) * Ta * D_Tmrt * PA * PA * PA * PA &
    + (-0.00000680434415) * ws * D_Tmrt * PA * PA * PA * PA + (-0.00000977675906) * D_Tmrt * D_Tmrt * PA * PA * PA * PA + (0.0882773108) * PA * PA * PA * PA * PA &
    + (-0.00301859306) * Ta * PA * PA * PA * PA * PA + (0.00104452989) * ws * PA * PA * PA * PA * PA + (0.000247090539) * D_Tmrt * PA * PA * PA * PA * PA &
    + (0.00148348065) * PA * PA * PA * PA * PA * PA

    fUTCI2 = ET1 + ET3 + ET4 + ET5 + ET6 + ET7 + ET8 + ET9
  
    RETURN
END function fUTCI2

function calcLup(Tsfc, obsLdown)
    implicit none      
    real Tsfc, obsLdown 
    real calcLup
    real groundEmis,TsfcK
    
    groundEmis=0.97
    TsfcK = Tsfc+273.15
    calcLup = groundEmis * 5.67E-08 * TsfcK**4 + (1-groundEmis) * obsLdown
    
    return 
end function calcLup


function getTmrtForGrid(forcingLdown,CalcLdn,kdn,kup,ldn,lup,tsfc)
    implicit none 
    real forcingLdown
    real getTmrtForGrid
    real CalcLdn,kdn,kup,ldn,lup,tsfc
    real returnvalue
    
    ! if value is -9999 then we don't have observed ldown in the forcing, use the calculated ldn
    if (forcingLdown .lt. 0) then        
        !  UTCI.calcTmrt(kd, ld, ku, lu)
        returnvalue=calcTmrt(kdn,ldn,kup,lup) 
    else        
        !  UTCI.calcTmrt(kd, ld2, ku, lupNew2)
        ldn=forcingLdown
        lup=calcLup(tsfc, forcingLdown)       
        returnvalue=calcTmrt(kdn,ldn,kup,lup)         
    endif
    
    getTmrtForGrid=returnvalue
    
    return
end function getTmrtForGrid



function getUTCIForGrid(Ta,ws,vapor,tmrt)
    implicit none 
    real forcingLdown
    real getUTCIForGrid
    real RH,Ta,ws,vapor,tmrt
    


!  UTCI.fUTCI2(Ta, ws, RH, tmrt)
    RH=calculateRH(Ta, vapor)
    getUTCIForGrid=fUTCI2(Ta, ws, RH, Tmrt)
    
    return
end function getUTCIForGrid

END MODULE UTCI