  MODULE ModVariables
  
  REAL, EXTERNAL :: computeStep
  REAL, EXTERNAL :: initArray
  REAL, EXTERNAL :: computeNewton

  INTEGER, PARAMETER :: Nmax = 100
  INTEGER :: errorFlagOpen,errorFlagClose
  INTEGER :: arrayElement = 0
  REAL, PARAMETER :: Gamma = 1.4
  REAL :: M,ARatio  


  END MODULE ModVariables

