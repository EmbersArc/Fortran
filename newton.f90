
  PROGRAM newton

  IMPLICIT NONE

  REAL, EXTERNAL :: computeStep
  REAL, EXTERNAL :: initArray
  REAL, EXTERNAL :: computeNewton


  INTEGER, PARAMETER :: Nmax = 100
  INTEGER :: errorFlag
  INTEGER :: arrayElement = 0
  REAL, PARAMETER :: Gamma = 1.4
  REAL :: M,ARatio  

!A/A* Array
  INTEGER :: i = 0
  REAL :: start = 1, end = 10
  INTEGER :: size = 92
  REAL :: ARatioArray(0:92)

  initializeLoop: DO
    ARatioArray(i) = start + (end-start)/size*(i)
    i = i+1
    IF (i == size + 1) THEN
        EXIT initializeLoop
    END IF
  END DO initializeLoop
!A/A* Array

  OPEN(UNIT=420,FILE=TRIM('newtonOutput'),STATUS='UNKNOWN',IOSTAT=errorFlag)

  ALoop: DO

    ARatio = ARatioArray(arrayElement)

    M = computeNewton(ARatio,Gamma)

    WRITE(420,*) 'A/A* = ',ARatio,'  M = ',M
    WRITE(*,*) 'A/A* = ',ARatio,'  M = ',M

    arrayElement = arrayElement + 1

    IF (arrayElement == size + 1) THEN
      EXIT ALoop
    END IF

  END DO ALoop


  CLOSE(UNIT=420,IOSTAT=errorFlag)
  IF ( errorFlag /= 0 ) THEN
    WRITE(*,*) 'ERROR - Could not close file!'   
    STOP
  END IF

  END PROGRAM newton