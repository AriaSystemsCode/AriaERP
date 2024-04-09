*-- MAFXOPMG : Delete the mafxopmg.* file
*-- Author   : AHMED MAHER (AMH)
PARAMETERS lcDataDir

IF FILE(gcAppHome+'MAOPMG.FXP')
  ERASE (gcAppHome+'MAOPMG.FXP')
ENDIF
IF FILE(gcAppHome+'MAOPMG.PRG')
  ERASE (gcAppHome+'MAOPMG.PRG')
ENDIF
IF FILE(gcAppHome+'MAOPMG.FRX')
  ERASE (gcAppHome+'MAOPMG.FRX')
ENDIF
IF FILE(gcAppHome+'MAOPMG.FRT')
  ERASE (gcAppHome+'MAOPMG.FRT')
ENDIF