lcGetDBFsDire = GETDIR('','Select Company DBFS Folder')
IF EMPTY(ALLTRIM(lcGetDBFsDire)) OR !DIRECTORY(lcGetDBFsDire)
  MESSAGEBOX('Invalid directory')
  RETURN .F.
ENDIF
lcFilePath = GETFILE('XLS', 'Select Customers XLS file')
IF EMPTY(ALLTRIM(lcFilePath )) OR !FILE(lcFilePath)
  MESSAGEBOX('Invalid XLS File')
 RETURN .F.
ENDIF
SET STEP ON 
USE (ADDBS(lcGetDBFsDire)+"Customer.DBF") SHARED ORDER CUSTOMER
SELECT 0
IMPORT from (lcFilePath) TYPE XLS 
lcAlis = ALIAS()
LOCATE
SCAN FOR RECNO()>1
  SELECT CUSTOMER
  LOCATE FOR ALLTRIM(customer.usr_dfnd1) == ALLTRIM(&lcAlis..B)
  IF FOUND()
    lcAcount = Customer.Account
    REPLACE Account WITH  IIF(EMPTY(Allt(&lcAlis..A)),SUBSTR(Allt(&lcAlis..B),1,5),Allt(&lcAlis..A))  FOR Account = lcAcount 
  ENDIF
  SELECT(lcAlis)
ENDSCAN 
CLOSE ALL
