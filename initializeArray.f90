MODULE initializeArray

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

END MODULE initializeArray