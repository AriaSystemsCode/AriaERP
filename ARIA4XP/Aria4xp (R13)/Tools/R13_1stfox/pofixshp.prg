*-- POFIXSHP : Delete the poshp.fxp file
*-- Author   : AHMED MAHER (AMH)
PARAMETERS lcDataDir
IF FILE(gcAppHome+'PO\POSHP.FXP')
  ERASE (gcAppHome+'PO\POSHP.FXP')
ENDIF
IF FILE(gcAppHome+'PO\POSHP.PRG')
  ERASE (gcAppHome+'PO\POSHP.PRG')
ENDIF