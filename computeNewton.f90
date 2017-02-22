REAL FUNCTION computeNewton(ARatio,Gamma)

REAL :: xn0 = 6,xn1
REAL, INTENT(IN) :: ARatio,Gamma

    newtonLoop: DO   

      xn1 = computeStep(xn0,Gamma,ARatio) 
      IF ( abs(xn1-xn0) < 0.000001 ) THEN
        EXIT newtonLoop
      ELSE 
        xn0 = xn1
      END IF      

    END DO newtonLoop

computeNewton = xn1

END FUNCTION computeNewton