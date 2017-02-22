REAL FUNCTION computeStep(xn,Gamma,ARatio)

IMPLICIT NONE

REAL, INTENT(IN) :: xn,Gamma
REAL :: f,df1,df2,df,ARatio


f = (ARatio-((1/xn)*(2/(Gamma+1)*(1+(Gamma-1)/2*(xn**2)))**((Gamma+1)/(2*(Gamma-1)))))
df1 = ((1/xn**2) * (2/(Gamma+1)*(1+(Gamma-1)/2*(xn**2)))**((Gamma+1)/(2*(Gamma-1))))
df2 = ((2/(Gamma+1) * (1+(Gamma-1)/2*(xn**2)))**((3-Gamma)/(2*(Gamma-1))))
df = (  df1  -  df2  )

computeStep = ( xn - f/df )   


END FUNCTION computeStep
