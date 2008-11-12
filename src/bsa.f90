PROGRAM BasicSimulationApplication

  USE, INTRINSIC :: ISO_C_BINDING
  USE CELLML_MODEL_DEFINITION

  IMPLICIT NONE
  INTEGER, PARAMETER :: DP=SELECTED_REAL_KIND(15,307) ! from opencmiss kinds.f90
  INTEGER, PARAMETER :: INTG=SELECTED_INT_KIND(9) ! from opencmiss kinds.f90

  CHARACTER(256) buf
  CHARACTER(256) uri
  REAL(DP) times(4)
  INTEGER(INTG) i,count,uriL
  count = NARGS()
  CALL GETARG(0,buf)
  IF (count .lt. 2) THEN
     WRITE(*,*) 'usage: ',buf(1:status),' <uri> <tStart[0]> <tEnd[1]> <tabT[0.1]> <dt[0.01]>'
     STOP
  END IF
  CALL GETARG(1,uri,uriL) ! Should check status/uriL...
  ! default time data
  times(1) = 0.0_DP !tStart
  times(2) = 1.0_DP !tEnd
  times(3) = 0.1_DP !tabT
  times(4) = 0.01_DP !dt
  DO i=2,count-1
     CALL GETARG(i,buf)
     READ(buf,*) times(i-1)
  END DO
  WRITE(*,*) 'Input URI: ',uri(1:uriL)
  WRITE(*,*) '  integrate from ',times(1),' to ',times(2)
  WRITE(*,*) '  with - output steps of ',times(3)
  WRITE(*,*) '       - integration steps ',times(4)

  CALL CREATE_CELLML_MODEL_DEFINITION(uri(1:uriL))

  STOP
END PROGRAM BasicSimulationApplication
