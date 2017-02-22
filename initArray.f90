MODULE initArray

!A/A* Array
INTEGER :: i = 0
REAL :: start = 0.9, end = 10
INTEGER :: size = 92
REAL :: ARatioArray(0:92)

initializeLoop: DO
    ARatioArray(i) = start + (end-start)/size*i
    i = i+1
    IF (i == size + 1) THEN
        EXIT initializeLoop
    END IF
END DO initializeLoop
!A/A* Array

END MODULE initArray