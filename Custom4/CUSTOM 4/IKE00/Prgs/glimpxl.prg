************************************************************************************************************************
*: Program File  : GLIMPXL.PRG
*: Program Desc. : Convert a custome program to import Excel sheet to update GL files 
*: System : Aria4xp R12
*: Module : GL
*  06/13/2013
*: Developer : TMI - Tarek Mohammed Ibrahim
*: Original Tracking # : C201310.122  [T20110118.0014 ] ( a27 - before R13 in A4 )
*: Convert  Tracking # : C201582.exe  [T20130606.0005 ]
************************************************************************************************************************
*Modifications:
*B610888,1 MMT 10/19/2014 Custom Import program does not accept some date formats[T20140922.0012]
*B610894,1 MMT 10/23/2014 Custom import program accept number has more than 2 decimals[T20141020.0006]
*B611099,1 MMT 01/04/2016 Custom Import Journal entries program rounding issue[T20151228.0001]
*B611099,2 MMT 01/05/2016 Error while selecting files because of saved file path in mem file[T20151228.0001]
************************************************************************************************************************
*!**!**!**!**
*!*	steps *!*
*!*
*!*	import
*!*	validate
*!*	Update
*!**!**!**!**

lcRunScx = oAriaApplication.clientscreenhome+"gl\glimprxl.scx"
DO FORM (lcRunScx) 

*!*************************************************************
*! Name      : lfFormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 06/13/2013
*! Purpose   : called from the Screen init Method
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet

loFormSet.Ariaform1.Caption = 'Import Excel files to update GL entries'

SET MULTILOCKS ON 
*- Open files
=lfOpenFls()
loFormSet.AddProperty('lcCr', CHR(13) )
loFormSet.AddProperty('lcNxtBatch','')
loFormSet.AddProperty('lcErrLog','')

loFormSet.AddProperty('lcTmpGLXLS', gfTempName())

loFormSet.Ariaform1.edtErrorlist.ControlSource = 'Thisformset.lcErrLog'
loFormSet.Ariaform1.Refresh()

*****************************************************************************************************************************
*
*   FUNCTION lfImport
*   06/13/2013
*
*****************************************************************************************************************************
FUNCTION lfImport
PARAMETERS loFormSet

lcTmpGLXLS = loFormSet.lcTmpGLXLS
IF FILE(oAriaApplication.WorkDir+lcTmpGLXLS+'.DBF')
  IF gfModalGen('INM00000B00006',.F.,.F.,.F.,'File already has been selected, Select again?')<>1
    RETURN
  ENDIF 
ENDIF

*- Be sure to close the imported file
IF USED(lcTmpGLXLS)
  USE IN &lcTmpGLXLS
ENDIF


SET SAFETY OFF
CD (oariaapplication.clienta4path)
IF FILE(oariaapplication.clientprogramhome+'GL\GLIMPVXL.MEM')
   RESTORE FROM (oariaapplication.clientprogramhome+'GL\GLIMPVXL.MEM') ADDITIVE
   *B611099,2 MMT 01/05/2016 Error while selecting files because of saved file path in mem file[T20151228.0001][Start]
   IF DIRECTORY(lcopndir)
   *B611099,2 MMT 01/05/2016 Error while selecting files because of saved file path in mem file[T20151228.0001][End]
     CD (lcopndir)
   *B611099,2 MMT 01/05/2016 Error while selecting files because of saved file path in mem file[T20151228.0001][Start]
   ENDIF
   *B611099,2 MMT 01/05/2016 Error while selecting files because of saved file path in mem file[T20151228.0001][End]
ENDIF
llcontinue = .T.
lcgetdir = GETFILE('xls')
IF EMPTY(lcgetdir)
   MESSAGEBOX('No file is selected', 0, 'Aria27')
   llcontinue = .F.
ELSE
   lcopndir = ADDBS(JUSTPATH(lcgetdir))
   SAVE TO (oariaapplication.clientprogramhome+'GL\GLIMPVXL.MEM') ALL LIKE lcOpnDir*
ENDIF
IF llcontinue
   CD (oAriaApplication.WorkDir)
   SELECT 0
   ON ERROR llcontinue = .F.
   IMPORT FROM (lcgetdir) TYPE XL5
   ON ERROR
   IF llcontinue
      COPY TO (oAriaApplication.WorkDir+lctmpglxls) 
      lcdbf = DBF()
      USE
      ERASE (lcdbf)
   ELSE
      MESSAGEBOX('Invalid file format', 0, 'Aria4xp')
   ENDIF
ENDIF
*- End of lfImport.

*!*************************************************************
*! Name      : lfValidate
*! Developer : TMI - TAREK MOHAMMED IBRAHIM
*! Date      : 06/13/2013
*! Purpose   : Validate the selected file
*!*************************************************************
FUNCTION lfValidate
PARAMETERS loFormSet

lcTmpGLXLS = loFormSet.lcTmpGLXLS
IF FILE(oAriaApplication.WorkDir+lcTmpGLXLS+'.DBF')

  loFormSet.lcErrLog = ''
  
  IF !USED(lcTmpGLXLS)
    USE (oAriaApplication.WorkDir+lcTmpGLXLS) IN 0
  ENDIF
  
  SELECT (lcTmpGLXLS)
  
  *-Account # (column A) is valid in the chart of accounts.
  LOCATE FOR !EMPTY(B)
  SKIP
  SCAN REST FOR !EMPTY(A)
    IF !SEEK( PADR(ALLT(A),24) ,'GLACCHAR')
      =lfErrLog(loFormSet,'ACC',ALLT(A))
    ENDIF
    *B610888,1 MMT 10/19/2014 Custom Import program does not accept some date formats[T20140922.0012][Start]
    IF "/" $ D
      REPLACE D WITH STR(CTOD(D)-{^1899-12-30})
    ENDIF    
    *B610888,1 MMT 10/19/2014 Custom Import program does not accept some date formats[T20140922.0012][End]
    *B610894,1 MMT 10/23/2014 Custom import program accept number has more than 2 decimals[T20141020.0006][Start]
    IF "." $ E OR "." $ F OR "." $ G
      IF "." $ E 
        lnDotLoc = ATC(".",E)
        lnAfterDot = SUBSTR(E,lnDotLoc+1)
        IF LEN(ALLTRIM(lnAfterDot)) > 2 AND !"9999" $ ALLTRIM(lnAfterDot)
          lnBeforeDot = SUBSTR(E,1,lnDotLoc)
          REPLACE E WITH lnBeforeDot+ SUBSTR(ALLTRIM(lnAfterDot),1,2)
         ENDIF  
       ENDIF 
       
       IF "." $ F 
        lnDotLoc = ATC(".",F)
        lnAfterDot = SUBSTR(F,lnDotLoc+1)
        IF LEN(ALLTRIM(lnAfterDot)) > 2 AND !"9999" $ ALLTRIM(lnAfterDot)
          lnBeforeDot = SUBSTR(F,1,lnDotLoc)
          REPLACE F WITH lnBeforeDot+ SUBSTR(ALLTRIM(lnAfterDot),1,2)
         ENDIF  
       ENDIF 

       IF "." $ G 
        lnDotLoc = ATC(".",G)
        lnAfterDot = SUBSTR(G,lnDotLoc+1)
        IF LEN(ALLTRIM(lnAfterDot)) > 2 AND !"9999" $ ALLTRIM(lnAfterDot)
          lnBeforeDot = SUBSTR(G,1,lnDotLoc)
          REPLACE G WITH lnBeforeDot+ SUBSTR(ALLTRIM(lnAfterDot),1,2)
         ENDIF  
       ENDIF 
    ENDIF    
    *B610894,1 MMT 10/23/2014 Custom import program accept number has more than 2 decimals[T20141020.0006][End]
  ENDSCAN
  
  *-Each transaction's total Debit = total Credit for the related set of entries.2
  =SEEK('CTRANNO   ','SEQUENCE')
  loFormSet.lcNxtBatch = PADL(SEQUENCE.NSEQ_NO,8,'0')
  SELECT (lcTmpGLXLS)
  LOCATE
  LOCATE REST FOR !EMPTY(B)
  SKIP
  DO WHILE !EOF()
    LOCATE REST FOR !EMPTY(B)
    lnBalance = 0
    lcBal = ''
    SCAN REST WHILE !EMPTY(B)
      lcBal = A + ',' + B + ',' + E + ',' + F + loFormSet.lcCr
      *B611099,1 MMT 01/04/2016 Custom Import Journal entries program rounding issue[T20151228.0001][Start]
      *lnBalance = lnBalance + VAL(E) - VAL(F)      
      lnBalance = lnBalance + ROUND(VAL(E),2) - ROUND(VAL(F),2)
      *B611099,1 MMT 01/04/2016 Custom Import Journal entries program rounding issue[T20151228.0001][End]
    ENDSCAN
    IF lnBalance <> 0
      =lfErrLog(loFormSet,'BAL',lcBal)
    ENDIF
  ENDDO
  
  *- Enable/Disable buttons
  IF EMPTY(loFormSet.lcErrLog)
    WITH loFormset.AriaForm1 
      .pbImport.Enabled = .F.
      .pbVld.Enabled = .F.
      .pbUpd.Enabled = .T.
    ENDWITH 
  ELSE
    loFormSet.lcErrLog = 'Error List ' + loFormSet.lcCr  + ;
               loFormSet.lcErrLog              
  ENDIF
  
  loFormset.AriaForm1.Refresh()

ELSE
  
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No file has been selected.')

ENDIF  

*********************************************************************************************************
*
*   FUNCTION lfUpdate
*   06/13/2013
*   TMI - TAREK MOHAMMED IBRAHIM
*   Update the GL files
*
*********************************************************************************************************
FUNCTION lfUpdate
PARAMETERS loFormSet
PRIVATE lnSlct
lnSlct = SELECT(0)

lcTmpGLXLS = loFormSet.lcTmpGLXLS
IF FILE(oAriaApplication.WorkDir+lcTmpGLXLS+'.DBF')
  lcCent = SET('CENTU')
  SET CENTU ON
  
  STORE '' TO lcCrFrm,lcCrTo
  
  SELECT (lcTmpGLXLS)
  LOCATE
  LOCATE REST FOR !EMPTY(B)
  SKIP
  LOCATE REST FOR !EMPTY(B)        
  
  DO WHILE !EOF(lcTmpGLXLS)
    SELECT (lcTmpGLXLS)
    LOCATE REST FOR !EMPTY(B)        
    STORE 0 TO lnDebit,lnCrdit
    SCAN REST WHILE !EMPTY(B)
      lnDebit = lnDebit + VAL(&lcTmpGLXLS..E)
      lnCrdit = lnCrdit + VAL(&lcTmpGLXLS..F)
                
      SELECT GLTRNSDT
      APPEND BLANK
      REPLACE CBATCHNO  WITH '000000';
              CTRANNO   WITH loFormSet.lcNxtBatch ;
              CACCTCODE WITH &lcTmpGLXLS..A ;
              CTRDTEXP  WITH &lcTmpGLXLS..C ;
              CDRORCR   WITH IIF(!EMPTY(&lcTmpGLXLS..E),'D','C') ;
              NAMOUNT   WITH IIF(CDRORCR='D',VAL(&lcTmpGLXLS..E),VAL(&lcTmpGLXLS..F)) ;
              DTRNPDATE WITH {^1899-12-30} + INT(VAL(&lcTmpGLXLS..D)) ;
              CTRNPYR   WITH STR(YEAR(DTRNPDATE ),4) ;
              CTRNPPRD  WITH PADL(MONTH(DTRNPDATE),2,'0') ;
              CADD_USER WITH oAriaApplication.USER_ID ;
              CADD_TIME WITH TIME() ;
              DADD_DATE WITH DATE() ;
              COWNER    WITH 'imported'
              
    ENDSCAN
    
    SELECT GLTRNSHD
    APPEND BLANK
    REPLACE CBATCHNO  WITH '000000' ;
            CTRANNO   WITH loFormSet.lcNxtBatch ; 
            CTRNDESC  WITH 'Created by Aria Systems from Excel' ;
            CTRNREFER WITH 'On ' + DTOC(DATE()) ;
            DTRNPDATE WITH GLTRNSDT.DTRNPDATE ;
            CTRNPYR   WITH STR(YEAR(DTRNPDATE ),4) ;
            CTRNPPRD  WITH PADL(MONTH(DTRNPDATE),2,'0') ;
            CTRNSTAT  WITH 'U' ;
            CTRNTYPE  WITH 'N' ;
            CTRNREVER WITH 'N' ;
            NTRNTOTDR WITH lnDebit ;
            NTRNTOTCR WITH lnCrdit ;
            CSRCMODUL WITH 'GL';
            CSTANDARD WITH 'Y' ;
            CSRCJRNL  WITH IIF(!EMPTY(&lcTmpGLXLS..H),ALLT(&lcTmpGLXLS..H),'GJ') ;
            CADD_USER WITH oAriaApplication.USER_ID ;
            CADD_TIME WITH TIME() ;
            DADD_DATE WITH DATE() ;
            COWNER    WITH 'imported'
    *- update the CCOMP_ID field
    REPLACE CCOMP_ID  WITH oariaapplication.activecompanyid
    
    lcCrFrm = IIF(EMPTY(lcCrFrm),loFormSet.lcNxtBatch,lcCrFrm)
    lcCrTo  = loFormSet.lcNxtBatch
    
    *- the loFormSet.lcNxtBatch should not take a decimal value
    loFormSet.lcNxtBatch = PADL(INT(VAL(loFormSet.lcNxtBatch))+ 1,8,'0')
    
    SELECT (lcTmpGLXLS)
    LOCATE REST FOR !EMPTY(B)
  ENDDO          

  SELECT SEQUENCE
  REPLACE NSEQ_NO   WITH VAL(loFormSet.lcNxtBatch)
  
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'File has been imported, transactions created from ' + lcCrFrm + ' to ' + lcCrTo)
  
  *- finalizing / erase / close    
  lcDbf = DBF(lcTmpGLXLS)
  USE IN &lcTmpGLXLS
  ERASE (lcDbf)
  
  *- update
  SELECT SEQUENCE
  =gfTableUpdate()

  SELECT GLTRNSDT
  =gfTableUpdate()

  SELECT GLTRNSHD
  =gfTableUpdate()

 
  *- close   
  =gfCloseTable('SEQUENCE')
  =gfCloseTable('GLTRNSDT')
  =gfCloseTable('GLTRNSHD')
  =gfCloseTable('GLACCHAR')
        

  *- restore century setting
  SET CENTU ON &lcCent 
  SELECT (lnSlct)
  CLEAR READ    

ELSE

  =gfModalgen("INM000000B00000",.F.,.F.,.F.,"No imported file to update")
  
ENDIF   
    
SELECT (lnSlct)


*!*************************************************************
*! Name      : lfErrLog
*! Developer : TMI - TAREK MOHAMMED IBRAHIM
*! Date      : 06/13/2013
*! Purpose   : Create an error log, if any
*!*************************************************************
FUNCTION lfErrLog
PARAMETERS loFormset,lcRes,lcVal
PRIVATE lcMsg

DO CASE
CASE lcRes = 'ACC'
  lcMsg = ' Account is not found in the Chart of Accounts.'
  loFormSet.lcErrLog = loFormSet.lcErrLog + lcVal + lcMsg + loFormSet.lcCr 

CASE lcRes = 'BAL'
  lcMsg = ' Debit/Credits are Not balanced.'
  loFormSet.lcErrLog = loFormSet.lcErrLog + lcMsg + loFormSet.lcCr + ;
                        lcVal + loFormSet.lcCr 
ENDCASE

*!*************************************************************
*! Name      : lfOpenFls
*! Developer : TMI - TAREK MOHAMMED IBRAHIM
*! Date      : 06/13/2013
*! Purpose   : * Open the needed file for the import and update to complete
*!*************************************************************
FUNCTION lfOpenFls

gcDataDir = oAriaApplication.DataDir
=gfOpenTable(gcDataDir+'SEQUENCE','CSEQ_TYPE','SH')
=gfOpenTable(gcDataDir+'GLTRNSDT','BATCHTRN','SH')
=gfOpenTable(gcDataDir+'GLTRNSHD','BATCHTRN','SH')
=gfOpenTable(gcDataDir+'GLACCHAR','ACCTCODE','SH')


