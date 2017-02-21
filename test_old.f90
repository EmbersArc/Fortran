
  PROGRAM test
  
  IMPLICIT NONE
  
  INTEGER :: ef,i
  REAL :: r

  emptyLoop: DO   
    WRITE(*,*) 'Hello, please enter an integer:'
    READ(*,*,IOSTAT=ef) i
    
    IF ( ef /= 0 ) THEN 
      WRITE(*,*) 'ERROR - I said please enter an INTEGER!'
    ELSE 
      EXIT emptyLoop
    END IF ! ef
  END DO emptyLoop
  
  r = SQRT(REAL(i))
  
  WRITE(*,*) 'The square root of the integer is:',r
  
  END PROGRAM test