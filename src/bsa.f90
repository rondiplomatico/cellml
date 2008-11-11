PROGRAM BasicSimulationApplication
  CHARACTER(256) buf
  CHARACTER(256) uri
  REAL(8) tStart,tEnd,tabT,dt
  INTEGER(4) count,status
  count = NARGS()
  CALL GETARG(0,buf,status)
  IF (count .lt. 2) THEN
     WRITE(*,*) 'usage: ',buf(1:status),' <uri> <tStart> <tEnd> <tabT> <dt>'
     STOP
  END IF
  CALL GETARG(1,uri,status)
  ! Should check status...
  WRITE(*,*) 'Input URI: ',uri(1:status)
END PROGRAM BasicSimulationApplication
