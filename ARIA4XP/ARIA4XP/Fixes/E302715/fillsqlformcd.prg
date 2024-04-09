_Screen.Visible = .F. 
CLOSE ALL 
lcAria27SysFiles = GETDIR()
IF !DIRECTORY(lcAria27SysFiles) OR !FILE(addbs(lcAria27SysFiles)+'syfrmcdh.dbf')
  MESSAGEBOX('Invalid SysFiles Folder')
  RETURN .F.
ENDIF 

lnSqlConnect = SQLSTRINGCONNECT()

IF lnSqlConnect > 0
  
  lnCheckSql = SQLEXEC(lnSqlConnect ,"Select * from FORMS_T ",'CheckRec')
  IF lnCheckSql < 0
    lnCheckSql = SQLEXEC(lnSqlConnect ,"create table FORMS_T (ID Char(20),DESCRIPTION Char(100),PARTNER_TYPES Char(10),FORM_KEY UniqueIdentifier default NewID())",'CheckRec')
  ENDIF 
  
*!*    lnCheckSql = SQLEXEC(lnSqlConnect ,"Select * from EMAIL_FORMS",'CheckRec')
*!*    IF lnCheckSql < 0
*!*      lnCheckSql = SQLEXEC(lnSqlConnect ,"create table EMAIL_FORMS (PARTNER_TYPES Char(10),PARTNER_ID Char(20),STORE_ID Char(8),EMAIL_ADDRESSES TEXT,Overwritten bit,ID Char(20),EMAIL_FORMS_KEY UniqueIdentifier default NewID())",'CheckRec')
*!*    ENDIF 
  
  USE (addbs(lcAria27SysFiles)+'syfrmcdh.dbf') SHARED IN 0
  SELECT syfrmcdh
  SCAN 
    lnSuc = SQLEXEC(lnSqlConnect ,"Select * from FORMS_T where ID = '"+ALLTRIM(syfrmcdh.cformmaj)+"'",'CheckRec')
    IF lnSuc > 0 
      SELECT 'CheckRec'
      LOCATE 
      IF EOF()
        lnUuc = SQLEXEC(lnSqlConnect ,"INSERT INTO FORMS_T VALUES('"+ALLTRIM(syfrmcdh.cformmaj)+ ;
                       "','"+syfrmcdh.cformmjdes+"','C',NEWID ())",'CheckRec')
      ENDIF 
    ENDIF
  ENDSCAN 
ELSE  
  MESSAGEBOX('Could not connect to SQL')
  RETURN .F.
ENDIF 