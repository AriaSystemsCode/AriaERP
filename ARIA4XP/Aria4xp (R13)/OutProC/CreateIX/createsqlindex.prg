*:************************************************************************
*: Modifications : 
*! E037241,2 MAH 04/17/2005 Browse User Defined Sort.
*:************************************************************************

LPARAMETERS lcParameter

LOCAL lcConnectionString, lcInputCommandRemove, lcInputCommandAdd
lcConnectionString   = SUBSTR(lcParameter, 1, AT("|", lcParameter) - 1)
lcInputCommandRemove = SUBSTR(lcParameter, AT("|", lcParameter)    + 1, AT("|", lcParameter, 2) - AT("|", lcParameter) - 1)
lcInputCommandAdd    = SUBSTR(lcParameter, AT("|", lcParameter, 2) + 1, AT("|", lcParameter, 3) - AT("|", lcParameter, 2) - 1)
lcOutPutFile         = SUBSTR(lcParameter, AT("|", lcParameter, 3) + 1)

LOCAL lnConHandle
lnConHandle = SQLSTRINGCONNECT(lcConnectionString)
IF lnConHandle <= 0 
  gfQuit()
  RETURN
ENDIF

LOCAL lnSQLHandle
lnSQLHandle = SQLEXEC(lnConHandle, lcInputCommandRemove)
lnSQLHandle = SQLEXEC(lnConHandle, lcInputCommandAdd)

IF lnSQLHandle <= 0 
  = gfQuit()
ENDIF

STRTOFILE("T", lcOutPutFile)