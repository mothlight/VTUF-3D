SUBROUTINE default_conditions(in_path, out_path)
    USE SWITCHES
    IMPLICIT NONE
    CHARACTER(len=*), INTENT(INOUT) :: in_path, out_path
    
    IOHRLY = 0   ! Controls daily, hourly, and/or layer outp
    IOTUTD = 9   ! Controls transmittance file output
    IOHIST = 0   ! Controls histogram output
    IORESP = 0   ! Controls respiration output
    IODAILY = 1  ! Controls daily output: FIXED HERE 
    IOWATBAL = 1 ! Controls water balance output
    IOFORMAT = 0 ! Dump mode...
    ISUNLA = 0 ! Mathias 27/11/12
    
    ! current working directory
    in_path = ''
    out_path = ''
    
END SUBROUTINE default_conditions
