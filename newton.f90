
  PROGRAM newton

  IMPLICIT NONE

  INTEGER, PARAMETER ::  Nmax = 100
  INTEGER :: iteration,i=1,errorFlag
  INTEGER :: arrayElement = 1
  !REAL, PARAMETER :: ARatio = 1
  REAL, PARAMETER :: Gamma = 1.4
  REAL :: xn0,xn1,M,df,f,df1,df2,ARatio
  REAL :: ARatioArray(92)
  
  initializeLoop: DO
    ARatioArray(i) = 0.9 + 0.1*i
    i = i+1
    IF (i == 92) THEN
      EXIT initializeLoop
    END IF
  END DO initializeLoop

  OPEN(UNIT=420,FILE=TRIM('newtonOutput'),STATUS='UNKNOWN',IOSTAT=errorFlag)

  ALoop: DO

    ARatio = ARatioArray(arrayElement)

    xn0 = 6
    iteration = 0

    newtonLoop: DO   

      f = (ARatio-((1/xn0)*(2/(Gamma+1)*(1+(Gamma-1)/2*(xn0**2)))**((Gamma+1)/(2*(Gamma-1)))))
      df1 = ((1/xn0**2) * (2/(Gamma+1)*(1+(Gamma-1)/2*(xn0**2)))**((Gamma+1)/(2*(Gamma-1))))
      df2 = ((2/(Gamma+1) * (1+(Gamma-1)/2*(xn0**2)))**((3-Gamma)/(2*(Gamma-1))))
      df = (  df1  -  df2  )
    
      xn1 = ( xn0 - f/df)   
      
      xn0 = xn1

      iteration = iteration + 1

      IF ( Nmax == iteration ) THEN
        EXIT newtonLoop

      END IF

    END DO newtonLoop

    M = xn1

    WRITE(420,*) 'A/A* = ',Aratio,'  M = ',M
    WRITE(*,*) 'A/A* = ',Aratio,'  M = ',M

    arrayElement = arrayElement + 1

    IF (arrayElement == 92) THEN
      EXIT ALoop
    END IF

  END DO ALoop


  CLOSE(UNIT=420,IOSTAT=errorFlag)
  IF ( errorFlag /= 0 ) THEN
    WRITE(*,*) 'ERROR - Could not close file!'   
    STOP
  END IF

  END PROGRAM newton