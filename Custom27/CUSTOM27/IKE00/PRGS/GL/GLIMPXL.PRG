************************************************************************************************************************
*: Program File  : GLIMP2XL.PRG
*: Program Desc. : Create a custome program to import Excel sheet to update GL files 
*: For Screen    : no screen , just a popup dialoge window
*: System : Aria27
*: Module : GL
*: Developer : TMI - Tarek Mohammed Ibrahim
*: Tracking # : C201310  [T20110118.0014 ]
** Create an exe and call it from A27 , as per discussed with Alaa there is no point to do this program in A4 as the team
** that will use this program is the accounting and they do not use A4 and he does not need to give them access to it
************************************************************************************************************************

*!**!**!**!**
*!*	steps *!*
*!**!**!**!**

	*!*	import
	*!*	validate
	*!*	create the menu and the related 
*!*	create the attachemtns and the related 
*!*	test
*!*	close the task

*- Open files
=lfOpenFls()

lcCr = CHR(13)
STORE '' TO lcErrLog,lcNxtBatch
lcSvMem = gfTempName()
lcTmpGLXLS = gfTempName()
SAVE TO (gcWorkDir+lcSvMem+'.MEM')
lcCommLine = (gcWorkDir+lcSvMem+'.MEM')

DO (GCSCRDIR+'GL\GLIMPXL.SPX')

*****************************************************************************************************************************
*
*   FUNCTION lfImport
*
*****************************************************************************************************************************
FUNCTION lfImport

IF FILE(gcWorkDir+lcTmpGLXLS+'.DBF')
  =gfModalGen('INM00000B00006',.F.,.F.,.F.,'File already has been selected, Select again?')<>1
  RETURN
ENDIF

*- Be sure to close the imported file
IF USED(lcTmpGLXLS)
  USE IN &lcTmpGLXLS
ENDIF

*- Call the exe
lcLib=SYS(2004)+"foxtools.fll"
IF FILE(lcLib)
	SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
	SW_HIDE = 0
	lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")
	IF FILE(gcAppHome+'GL\GLIMPVXL.EXE')
      =EVALUATE("CALLFN("+STR(lnFnWinExec)+",gcAppHome+'GL\'+[GLIMPVXL.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
	ENDIF	
	RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")
ELSE
	WAIT "LIBRARY NOT FOUND" WINDOW
	RETURN .F.
ENDIF

lcErrLog = ''
SHOW GETS

*!*************************************************************
*! Name      : lfValidate
*! Developer : TMI - TAREK MOHAMMED IBRAHIM
*! Date      : 03/08/2011
*! Purpose   : Validate the selected file
*!*************************************************************
FUNCTION lfValidate

IF FILE(gcWorkDir+lcTmpGLXLS+'.DBF')

  lcErrLog = ''
  
  IF !USED(lcTmpGLXLS)
    USE (gcWorkDir+lcTmpGLXLS) IN 0
  ENDIF
  
  SELECT (lcTmpGLXLS)
  
  *-Account # (column A) is valid in the chart of accounts.
  LOCATE FOR !EMPTY(B)
  SKIP
  SCAN REST FOR !EMPTY(A)
    IF !SEEK( PADR(ALLT(A),24) ,'GLACCHAR')
      =lfErrLog('ACC',ALLT(A))
    ENDIF
  ENDSCAN
  
  *-Each transaction's total Debit = total Credit for the related set of entries.2
  =SEEK('CTRANNO   ','SEQUENCE')
  lcNxtBatch = PADL(SEQUENCE.NSEQ_NO,8,'0')
  SELECT (lcTmpGLXLS)
  LOCATE
  LOCATE REST FOR !EMPTY(B)
  SKIP
  DO WHILE !EOF()
    LOCATE REST FOR !EMPTY(B)
    lnBalance = 0
    lcBal = ''
    SCAN REST WHILE !EMPTY(B)
      lcBal = A + ',' + B + ',' + E + ',' + F + lcCr
      lnBalance = lnBalance + VAL(E) - VAL(F)
    ENDSCAN
    IF lnBalance <> 0
      =lfErrLog('BAL',lcBal)
    ENDIF
  ENDDO
  
  *- Enable/Disable buttons
  IF EMPTY(lcErrLog)
    SHOW GET pbImport DISABLE
    SHOW GET pbVld    DISABLE
    SHOW GET pbUpd    ENABLE
  ELSE
*!*	    SHOW GET pbImport ENABLE
*!*	    SHOW GET pbUpd    ENABLE
*!*	    SHOW GET pbVld    DISABLE
    lcErrLog = 'Error List ' + lcCr  + ;
               lcErrLog              
  ENDIF
  
  SHOW GETS

ELSE
  
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No file has been selected.')

ENDIF  

*********************************************************************************************************
*
*   FUNCTION lfUpdate
*   03/08/2011
*   TMI - TAREK MOHAMMED IBRAHIM
*   Update the GL files
*
*********************************************************************************************************
FUNCTION lfUpdate
PRIVATE lnSlct
lnSlct = SELECT(0)

IF FILE(gcWorkDir+lcTmpGLXLS+'.DBF')
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
      *C201310,3 TMI 03/15/2011 [Start] do not use the VFP formate the represent the date
      *REPLACE CBATCHNO  WITH '000000';
              CTRANNO   WITH lcNxtBatch ;
              CACCTCODE WITH &lcTmpGLXLS..A ;
              CTRDTEXP  WITH &lcTmpGLXLS..C ;
              CDRORCR   WITH IIF(!EMPTY(&lcTmpGLXLS..E),'D','C') ;
              NAMOUNT   WITH IIF(CDRORCR='D',VAL(&lcTmpGLXLS..E),VAL(&lcTmpGLXLS..F)) ;
              DTRNPDATE WITH {^1899-12-30} + VAL(&lcTmpGLXLS..D) ;
              CTRNPYR   WITH STR(YEAR(DTRNPDATE ),4) ;
              CTRNPPRD  WITH PADL(MONTH(DTRNPDATE),2,'0') ;
              CADD_USER WITH gcUSER_ID ;
              CADD_TIME WITH TIME() ;
              DADD_DATE WITH DATE() ;
              COWNER    WITH 'imported'
      REPLACE CBATCHNO  WITH '000000';
              CTRANNO   WITH lcNxtBatch ;
              CACCTCODE WITH &lcTmpGLXLS..A ;
              CTRDTEXP  WITH &lcTmpGLXLS..C ;
              CDRORCR   WITH IIF(!EMPTY(&lcTmpGLXLS..E),'D','C') ;
              NAMOUNT   WITH IIF(CDRORCR='D',VAL(&lcTmpGLXLS..E),VAL(&lcTmpGLXLS..F)) ;
              DTRNPDATE WITH {12/30/1899} + INT(VAL(&lcTmpGLXLS..D)) ;
              CTRNPYR   WITH STR(YEAR(DTRNPDATE ),4) ;
              CTRNPPRD  WITH PADL(MONTH(DTRNPDATE),2,'0') ;
              CADD_USER WITH gcUSER_ID ;
              CADD_TIME WITH TIME() ;
              DADD_DATE WITH DATE() ;
              COWNER    WITH 'imported'
      *C201310,3 TMI 03/15/2011 [End  ]         
              
    ENDSCAN
    
    SELECT GLTRNSHD
    APPEND BLANK
    *C201310,4 TMI 03/15/2011 [Start]         
    *REPLACE CBATCHNO  WITH '000000' ;
            CTRANNO   WITH lcNxtBatch ; 
            CTRNDESC  WITH 'Created by Aria Systems from Excel' ;
            CTRNREFER WITH 'On ' + DTOC(DATE()) ;
            DTRNPDATE WITH GLTRNSHD.DTRNPDATE ;
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
            CADD_USER WITH gcUSER_ID ;
            CADD_TIME WITH TIME() ;
            DADD_DATE WITH DATE() ;
            COWNER    WITH 'imported'
    REPLACE CBATCHNO  WITH '000000' ;
            CTRANNO   WITH lcNxtBatch ; 
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
            CADD_USER WITH gcUSER_ID ;
            CADD_TIME WITH TIME() ;
            DADD_DATE WITH DATE() ;
            COWNER    WITH 'imported'
    *C201310,4 TMI 03/15/2011 [Start]         
    *C201310,3 TMI 03/15/2011 [Start] update the CCOMP_ID field
    REPLACE CCOMP_ID  WITH gcAct_comp
    *C201310,3 TMI 03/15/2011 [End  ] 
    
    lcCrFrm = IIF(EMPTY(lcCrFrm),lcNxtBatch,lcCrFrm)
    lcCrTo  = lcNxtBatch
    
    *C201310,3 TMI 03/15/2011 [Start] the lcNxtBatch should not take a decimal value
    *lcNxtBatch = PADL(VAL(lcNxtBatch)+ 1,8,'0')
    lcNxtBatch = PADL(INT(VAL(lcNxtBatch))+ 1,8,'0')
    *C201310,3 TMI 03/15/2011 [End  ] 
    
    SELECT (lcTmpGLXLS)
    LOCATE REST FOR !EMPTY(B)
  ENDDO          

  SELECT SEQUENCE
  REPLACE NSEQ_NO   WITH VAL(lcNxtBatch)
  
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'File has been imported, transactions created from ' + lcCrFrm + ' to ' + lcCrTo)
  
  *- finalizing / erase / close    
  lcDbf = DBF(lcTmpGLXLS)
  USE IN &lcTmpGLXLS
  ERASE (lcDbf)
  ERASE (lcCommLine)    
  =gfCloseFile('SEQUENCE')
  =gfCloseFile('GLTRNSDT')
  =gfCloseFile('GLTRNSHD')
  =gfCloseFile('GLACCHAR')
        
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
*! Date      : 03/08/2011
*! Purpose   : Create an error log, if any
*!*************************************************************
FUNCTION lfErrLog
PARAMETERS lcRes,lcVal
PRIVATE lcMsg

DO CASE
CASE lcRes = 'ACC'
  lcMsg = ' Account is not found in the Chart of Accounts.'
  lcErrLog = lcErrLog + lcVal + lcMsg + lcCr 

CASE lcRes = 'BAL'
  lcMsg = ' Debit/Credits are Not balanced.'
  lcErrLog = lcErrLog + lcMsg + lcCr + ;
                        lcVal + lcCr 
ENDCASE

*!*************************************************************
*! Name      : lfOpenFls
*! Developer : TMI - TAREK MOHAMMED IBRAHIM
*! Date      : 03/08/2011
*! Purpose   : * Open the needed file for the import and update to complete
*!*************************************************************
FUNCTION lfOpenFls

=gfOpenFile(gcDataDir+'SEQUENCE','CSEQ_TYPE','SH')
=gfOpenFile(gcDataDir+'GLTRNSDT','BATCHTRN','SH')
=gfOpenFile(gcDataDir+'GLTRNSHD','BATCHTRN','SH')
=gfOpenFile(gcDataDir+'GLACCHAR','ACCTCODE','SH')


