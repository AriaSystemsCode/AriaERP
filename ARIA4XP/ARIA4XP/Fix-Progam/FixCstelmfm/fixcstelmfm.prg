SET CPDIALOG OFF 
SET RESOURCE OFF 
SET TALK OFF
_Screen.Visible = .F. 
CLOSE ALL 
lcAria27SysFiles = GETDIR('',"Select Aria27 SysFiles Folder")
IF !DIRECTORY(lcAria27SysFiles) OR !FILE(addbs(lcAria27SysFiles)+'syfrmcdh.dbf')
  MESSAGEBOX('Invalid SysFiles Folder')
  RETURN .F.
ENDIF 
lcAria27SysFiles = ADDBS(lcAria27SysFiles)
USE lcAria27SysFiles +'Syccomp.dbf' Shar IN 0 ORDER 1
SELECT SYCCOMP 
SCAN FOR lrunfroma4
 lcConnStr = "Driver={SQL Server};server="+ALLTRIM(SYCCOMP.CCONSERVER)+";DATABASE="+ALLTRIM(SYCCOMP.CCONDBNAME)+;
             ";uid="+ALLTRIM(SYCCOMP.CCONUSERID)+";pwd="+ALLTRIM(SYCCOMP.CCONPASWRD)
 lnConHand=SQLSTRINGCONNECT(lcConnStr) 
 IF lnConHand>0
   lnBomHeader = SQLEXEC(lnConHand,"select * from bomheadr where ccosttype ='P'",'BomHeadr')
   IF lnBomHeader>0
     SELECT 'BomHeadr'
     SCAN
       lnCstTmp = SQLEXEC(lnConHand,;
       					 "update cstelmfm set ccosttype ='P',ccstshttyp ='"+BomHeadr.ccstshttyp+;
       					 "',cinvtype ='"+BomHeadr.cinvtype +"',citmmajor = '"+BomHeadr.citmmajor +;
       					 "',ccstsht_id ='' WHERE CCSTELMTMP='"+BomHeadr.ccstsht_id +"'")
     ENDSCAN 
     USE IN 'BomHeadr'
     =SQLDISCONNECT(lnConHand)
   ENDIF
 ENDIF
ENDSCAN 
MESSAGEBOX("CSTELMFM is updated Successfully")                