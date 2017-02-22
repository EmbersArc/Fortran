
  PROGRAM newton

  USE ModVariables
  IMPLICIT NONE

!A/A* Array
  REAL :: start = 1, end = 10
  INTEGER :: size = 100
  REAL :: ARatioArray(0:100)

  INTEGER :: i = 0
  
  initializeLoop: DO
    ARatioArray(i) = start + (end-start)/size*(i)
    i = i+1
    IF (i == size + 1) THEN
        EXIT initializeLoop
    END IF
  END DO initializeLoop
!A/A* Array


!open file
  OPEN(UNIT=420,FILE=TRIM('newtonOutput'),STATUS='UNKNOWN',IOSTAT=errorFlagOpen)
  IF ( errorFlagOpen /= 0 ) THEN
    WRITE(*,*) 'ERROR - Could not open file!'   
    STOP
  END IF



!compute M for each A/A* in array
  WriteLoop: DO

    ARatio = ARatioArray(arrayElement)

    M = computeNewton(ARatio,Gamma)

    WRITE(420,*) 'A/A* = ',ARatio,'  M = ',M
    WRITE(*,*) 'A/A* = ',ARatio,'  M = ',M

    arrayElement = arrayElement + 1

    IF (arrayElement == size + 1) THEN
      EXIT WriteLoop
    END IF

  END DO WriteLoop

!close file
  CLOSE(UNIT=420,IOSTAT=errorFlagClose)
  IF ( errorFlagClose /= 0 ) THEN
    WRITE(*,*) 'ERROR - Could not close file!'   
    STOP
  END IF


  END PROGRAM newton