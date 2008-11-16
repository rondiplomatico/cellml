PROGRAM BasicSimulationApplication

  USE, INTRINSIC :: ISO_C_BINDING
  USE CELLML_MODEL_DEFINITION

  IMPLICIT NONE
  INTEGER, PARAMETER :: DP=SELECTED_REAL_KIND(15,307) ! from opencmiss kinds.f90
  INTEGER, PARAMETER :: INTG=SELECTED_INT_KIND(9) ! from opencmiss kinds.f90

  CHARACTER(256) buf
  CHARACTER(256) uri_arg
  CHARACTER(256) URI
  REAL(DP) times(4)
  INTEGER(INTG) i,count,status,uriL

  INTEGER (C_INT) ci
  INTEGER (C_INT) CODE,N_CONSTANTS,N_RATES,N_ALGEBRAIC
  TYPE (C_PTR) :: CELLML_MODEL
  REAL (C_DOUBLE) :: VOI,te
  REAL (C_DOUBLE), ALLOCATABLE :: CONSTANTS(:)
  REAL (C_DOUBLE), ALLOCATABLE :: RATES(:)
  REAL (C_DOUBLE), ALLOCATABLE :: STATES(:)
  REAL (C_DOUBLE), ALLOCATABLE :: ALGEBRAIC(:)
  TYPE (C_FUNPTR) :: SETUP_FIXED_CONSTANTS

  count = NARGS()
  CALL GETARG(0,buf,status)
  IF (count .lt. 2) THEN
     WRITE(*,*) 'usage: ',buf(1:status),' <uri> <tStart[0]> <tEnd[1]> <tabT[0.1]> <dt[0.01]>'
     STOP
  END IF
  CALL GETARG(1,uri_arg,uriL) ! Should check status/uriL...
  ! default time data
  times(1) = 0.0_DP !tStart
  times(2) = 1.0_DP !tEnd
  times(3) = 0.1_DP !tabT
  times(4) = 0.01_DP !dt
  DO i=2,count-1
     CALL GETARG(i,buf)
     READ(buf,*) times(i-1)
  END DO
  WRITE(*,*) 'Input URI: ',uri_arg(1:uriL)
  WRITE(*,*) '  integrate from ',times(1),' to ',times(2)
  WRITE(*,*) '  with - output steps of ',times(3)
  WRITE(*,*) '       - integration steps ',times(4)

  ! create the C string version
  WRITE(URI,'(a,a)') uri_arg(1:uriL),C_NULL_CHAR

  CELLML_MODEL = CREATE_CELLML_MODEL_DEFINITION(uri)

  ! the default value should be not to save
  CODE = CELLML_MODEL_DEFINITION_GET_SAVE_TEMP_FILES(CELLML_MODEL)
  WRITE(*,*) 'F Current save state: ',CODE

  ! Make sure we save/don't save the generated files
  CALL CELLML_MODEL_DEFINITION_SET_SAVE_TEMP_FILES(CELLML_MODEL,0)
  CODE = CELLML_MODEL_DEFINITION_GET_SAVE_TEMP_FILES(CELLML_MODEL)
  WRITE(*,*) 'F Current save state: ',CODE

  ! instantiate the CellML model
  CODE = CELLML_MODEL_DEFINITION_INSTANTIATE(CELLML_MODEL)
  IF (CODE.EQ.0) THEN
     WRITE(*,*) 'F Instantiated the model with no error'
     ! allocate memory
     N_CONSTANTS = CELLML_MODEL_DEFINITION_GET_N_CONSTANTS(CELLML_MODEL)
     N_RATES = CELLML_MODEL_DEFINITION_GET_N_RATES(CELLML_MODEL)
     N_ALGEBRAIC = CELLML_MODEL_DEFINITION_GET_N_ALGEBRAIC(CELLML_MODEL)
     ALLOCATE(CONSTANTS(N_CONSTANTS))
     ALLOCATE(RATES(N_RATES))
     ALLOCATE(STATES(N_RATES))
     ALLOCATE(ALGEBRAIC(N_ALGEBRAIC))
     ! initialise
     OPEN(2,FILE='results')
     VOI = times(0)
     CALL CELLML_MODEL_DEFINITION_CALL_SETUP_FIXED_CONSTANTS &
          (CELLML_MODEL,CONSTANTS,RATES,STATES)
     CALL CELLML_MODEL_DEFINITION_CALL_COMPUTE_RATES &
          (CELLML_MODEL,VOI,STATES,RATES,CONSTANTS,ALGEBRAIC)
     CALL CELLML_MODEL_DEFINITION_CALL_EVALUATE_VARIABLES &
          (CELLML_MODEL,VOI,STATES,RATES,CONSTANTS,ALGEBRAIC)
     WRITE(2,'(F12.8)',advance='no') VOI
     DO ci=1,N_RATES
        WRITE(2,'(F12.8)',advance='no') STATES(ci)
     END DO
     DO ci=1,N_ALGEBRAIC
        WRITE(2,'(a,F12.8)',advance='no') ' ',ALGEBRAIC(ci)
     END DO
     WRITE(2,'') 
     ! basic Euler integration
     DO WHILE (VOI .LT. times(2))
        te = VOI + times(3)
        DO WHILE (.TRUE.)
           ! integrate the model
           CALL CELLML_MODEL_DEFINITION_CALL_COMPUTE_RATES &
                (CELLML_MODEL,VOI,STATES,RATES,CONSTANTS,ALGEBRAIC)
           DO ci=1,N_RATES
              STATES(ci) = STATES(ci) + RATES(ci) * times(4)
           END DO
           IF (ABS(te-VOI) .LT. 1.0E-10_C_DOUBLE) EXIT
           VOI = VOI + times(4)
           IF (VOI .GT. te) VOI = te
        END DO
        CALL CELLML_MODEL_DEFINITION_CALL_EVALUATE_VARIABLES &
             (CELLML_MODEL,VOI,STATES,RATES,CONSTANTS,ALGEBRAIC)
        WRITE(2,'(F12.8)',advance='no') VOI
        DO ci=1,N_RATES
           WRITE(2,'(F12.8)',advance='no') STATES(ci)
        END DO
        DO ci=1,N_ALGEBRAIC
           WRITE(2,'(F12.8)',advance='no') ALGEBRAIC(ci)
        END DO
        WRITE(2,'') 
        VOI = te
     END DO
     CLOSE(2)
  ELSE
     WRITE(*,*) 'F There were errors instantiating the model'
  END IF

  CALL DESTROY_CELLML_MODEL_DEFINITION(CELLML_MODEL)

  STOP
END PROGRAM BasicSimulationApplication
