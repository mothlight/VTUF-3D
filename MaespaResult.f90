MODULE MaespaResult
    
    USE maestcom, only : MAXSOILLAY, MAXT, MAXANG, MAXDATE, MAXP, MAXC, MAXHRS, MAXLAY, MAXMET, MAXSP, MAXHISTO
        
    IMPLICIT NONE

    type maesparesulttype   
        
        
    REAL DOY        ! simulation date
    REAL Tree       ! tree number
    REAL Spec       ! tree species number
    REAL Hour       ! hour of the day
    REAL hrPAR      ! absorbed PAR              umol tree-1 s-1
    REAL hrNIR      ! absorbed NIR              W tree-1
    REAL hrTHM      ! absorbed thermal          W tree-1
    REAL hrPS       ! photosynthesis (net of leaf resp) umol tree-1 s-1
    REAL hrRf       ! hourly leaf respiration   umol tree-1 s-1
    REAL hrRmW      ! hourly stem + branch Rm   umol tree-1 s-1
    REAL hrLE       ! hourly transpiration      mmol tree-1 s-1
    REAL LECAN      ! hourly transpirn: CANOPY calc : mmol H2O m-2 s-1
    REAL Gscan      ! canopy stomatal conductance : mol CO2 tree-1 s-1
    REAL Gbhcan     ! canopy boundary layer conductance to heat : mol tree-1 s-1
    REAL hrH        ! hourly sensible heat flux:  MJ tree-1 s-1
    REAL TCAN       ! Average foliage temperature (deg C)
    REAL ALMAX      ! Canopy maximum leaf photosynthesis rate (umol m-2 s-1)
    REAL PSIL       ! Canopy average leaf water potential (MPa)
    REAL PSILMIN    ! Canopy minimum leaf water potential (MPa)
    REAL CI         ! Canopy average intercellular CO2 conc. (ppm)
    REAL TAIR       ! Air temperature (deg C)
    REAL VPD        ! vapor pressure deficit (kPa)
    REAL PAR        ! Above-canopy incident PAR (umol m-2 s-1)
    
    

    
    end type

END MODULE MaespaResult
