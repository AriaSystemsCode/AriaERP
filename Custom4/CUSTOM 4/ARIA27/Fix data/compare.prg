LOCAL lcPath,lcKey,lcAccount,lcDate
CLOSE ALL
lcPath = GETDIR('','Select company DB Folder like Aria27\DBFS\99\')
IF !USED(lcPath+"EDITRANS.DBF")
  USE lcPath+"EDITRANS.DBF" IN 1 SHARED ORDER TYPEKEY
ENDIF
IF !USED(lcPath+"\INVHDR.DBF")
  USE lcPath+"\INVHDR.DBF" IN 2 SHARED
ENDIF
SELECT 2
ln = 0
SCAN
  WAIT WINDOW  INVHDR.Invoice nowait
  lcKey = ALLTRIM(INVHDR.Invoice)
  lcAccount = ALLTRIM(INVHDR.ACCOUNT)
  SELECT 1
  IF SEEK('810'+PADR(lcKey,40,' ')+'A'+PADR(lcAccount,8,' '),'EDITRANS','TYPEKEY')
    REPLACE EDITRANS.Dadd_date WITH INVHDR.InvDate
  ENDIF   
  SELECT 2
  ln = ln + 1
ENDSCAN
WAIT WINDOW STR(ln)+ 'Records are tested for fix' 