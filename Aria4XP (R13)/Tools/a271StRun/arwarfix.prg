*************************************************************************
*: Program file  : WareFix.Prg
*: Program desc. : Added the cWareCode to the invline file
*:                 so this program will fill the new field with data 
*:                 for coslidated invoice only.   
*:                  
*:        
*: Developer     : NAD - Nader Anis Mitry
*: Ref           : B603760,1  08/01/2000   
*:************************************************************************
*B603909,1 NAD 09/21/2000 the fix program takes a long time if you waited till it finished
*B603909,1                you may found more than 10000 temporary file.    

PARAMETERS  lcDataDir
*B603909,1 NAD 09/21/2000 (Start) Make the variables private.
PRIVATE lnLen, laIndex,laFields,lnFields,lcTmpName,lnCnt
*B603909,1 NAD (End)
IF TYPE('lcDataDir') # 'C' OR EMPTY(lcDataDir)
  lcDataDir = GETDIR('','Select data directory')
ENDIF

lnLen=LEN(lcDataDir)
IF !SUBSTR (lcDataDir,lnLen)='\'
  lcDataDir=lcDataDir+'\'
ENDIF  
IF FILE(lcDataDir+"OrdHdr.dbf") AND  FILE(lcDataDir+"InvLine.dbf") 
  
  USE lcDataDir+"OrdHdr.dbf" IN 0 ORDER OrdHdr 
  USE lcDataDir+"ConsInvL.dbf" IN 0 ORDER ConsInvL  
  USE lcDataDir+"InvLine.dbf" IN 0 
  USE lcDataDir+"InvHdr.dbf" IN 0 ORDER InvHdr
  SELECT InvLine
  
  DIME laIndex[3,2]
  laIndex = ''
  laIndex[1,1] = 'invoice+STR(lineno,6)'
  laIndex[1,2] = 'InvLine'
  laIndex[2,1] = 'order+STR(lineno,6)+invoice'
  laIndex[2,2]=  'InvLineo'
  laIndex[3,1] = 'style+invoice+STR(lineno,6)'
  laIndex[3,2]=  'InvLines'
  =AFIELDS(laFields)
  lnFields = ALEN(laFields,1)

  IF ASCAN(laFields,'CWARECODE') = 0
    DIMENSION laFields[lnFields+1,4]
    laFields[lnFields+1,1]='cWareCode'
    laFields[lnFields+1,2]='C'
    laFields[lnFields+1,3]=6
    laFields[lnFields+1,4]=0
  ENDIF
    
  lcTmpName = ("X"+SUBSTR(SYS(2015),4))
  CREATE TABLE (lcDataDir+lcTmpName) FROM ARRAY  laFields 
  SELECT InvLine
  SCAN
    WAIT WINDOW Invoice +'\'+Style NOWAIT
    SCATTER MEMVAR MEMO
    SELECT (lcTmpName) 
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDSCAN
  WAIT CLEAR 
  USE IN (lcTmpName)
  
  USE (lcDataDir+lcTmpName) IN 0 EXCLUSIVE 
  SELECT  (lcTmpName) 
  
  *B603909,1 NAD 09/21/2000 Use the private variable because the GFRUNFIX Function which call this 
  *              program uses the same variable (lnCount).  
  *FOR lnCount = 1 TO ALEN(laIndex,1)
  *   INDEX ON &laIndex[lnCount,1] TAG &laIndex[lnCount,2] ADDITIVE
  
  FOR lnCnt = 1 TO ALEN(laIndex,1)
     INDEX ON &laIndex[lnCnt,1] TAG &laIndex[lnCnt,2] ADDITIVE
  *B603909,1 NAD(End)
  ENDFOR
  
  REINDEX  
  
  USE IN (lcTmpName)
  USE IN InvLine
  ERASE  (lcDataDir+'InvLine.DBF')
  ERASE  (lcDataDir+'InvLine.CDX')
  *B603909,1 NAD 09/21/2000 (Start) Erase the .FPT also.
  ERASE  (lcDataDir+'InvLine.FPT')
  *B603909,1 NAD 09/21/2000 (End)
  
  RENAME (lcDataDir+lcTmpName+'.DBF') TO (lcDataDir+'InvLine.DBF')
  RENAME (lcDataDir+lcTmpName+'.CDX') TO (lcDataDir+'InvLine.CDX')
  *B603909,1 NAD 09/21/2000 (Start) Rename the .FPT also.
  RENAME (lcDataDir+lcTmpName+'.FPT') TO (lcDataDir+'InvLine.FPT')
  *B603909,1 NAD 09/21/2000 (End)
  USE lcDataDir+"InvLine.dbf" IN 0 ORDER InvLine
  SELECT InvHdr 
  SCAN
    WAIT WINDOW 'Updating Invoice No. ' + InvHdr.Invoice  NOWAIT  
    
    IF InvHdr.Consol='Y'             
      IF SEEK(InvHdr.Invoice,'InvLine')        
        SELECT InvLine
        SCAN REST WHILE Invoice=InvHdr.invoice
          SELECT ConsInvL
          =SEEK (InvHdr.Invoice) 
          LOCATE REST WHILE INVOICE=InvHdr.Invoice FOR Style=Invline.style
          IF FOUND () AND SEEK('O'+ConsInvL.Order,'OrdHdr') 
            SELECT INVLINE 
            REPLACE CwareCode WITH OrdHdr.cWareCode
          ENDIF          
        ENDSCAN     
      ENDIF   
    ENDIF
  ENDSCAN 
  WAIT CLEAR
  USE IN OrdHdr 
  USE IN ConsInvL   
  USE IN InvLine
  USE IN InvHdr 
  
ELSE
  IF !EMPTY(lcDataDir) 
    WAIT WINDOW 'Wrong dbfs Directory.'
  ENDIF  
ENDIF   