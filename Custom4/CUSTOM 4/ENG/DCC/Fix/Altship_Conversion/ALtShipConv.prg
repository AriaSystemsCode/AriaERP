_Screen.Visible = .F. 
lcStyleDataPath = GETDIR("","Select Aria27 Dbfs directory")
IF EMPTY(lcStyleDataPath)
  MESSAGEBOX('Selected Directory is Incorrect')
  RETURN .F.
ENDIF 
IF !FILE(lcStyleDataPath+"\altsHip.DBF")
  MESSAGEBOX('Selected Directory is Incorrect')
  RETURN .F.
ENDIF 
USE lcStyleDataPath+"\altsHip.DBF" IN 0 EXCLUSIVE 
ALTER TABLE  altsHip    ADD COLUMN  CADDCODE  c(6)
ALTER TABLE  altsHip     ADD COLUMN  ccont_code c(6)
SELECT altsHip
REPLACE ccont_code WITH 'ENG   ' ALL
SET ORDER TO ALTSHIP   && ACCOUNT 
LOCATE 
SELECT Distinct account FROM ALTSHIP   INTO CURSOR 'Temp'
SELECT 'Temp'
SCAN 
  SELECT altsHip
  =SEEK(Temp.Account,'ALTSHIP')
  lnAddC = 1
  SCAN REST WHILE Account = Temp.Account
    REPLACE CADDCODE  WITH PADL(ALLTRIM(STR(lnAddC)),6,'0')
    lnAddC = lnAddC + 1 
  ENDSCAN 
ENDSCAN 
SELECT altsHip
INDEX on ACCOUNT +CADDCODE TAG ALTSHIP   
CLOSE ALL 


lcSqlDataPath = GETDIR("","Select Aria4XP SQLdirectory")
IF EMPTY(lcSqlDataPath)
  MESSAGEBOX('Selected Directory is Incorrect')
  RETURN .F.
ENDIF 
USE lcSqlDataPath +"\SYcnvlib.DBF" IN 0 Shared 
SELECT SYcnvlib
SET ORDER TO SYCNVLIB   && CFILE
=SEEK('ALTSHIP')
REPLACE lext_call WITH .T.
REPLACE cfile_a26 WITH 'ALTSHIP'
CLOSE ALL 

