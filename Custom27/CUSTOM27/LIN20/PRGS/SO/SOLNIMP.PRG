*:**************************************************************************
*: Program file  : SOLNIMP.PRG
*: Program desc. : Import Customer Information for LIN20 ( LIN20 - Linda Segal )
*: Date          : 05/20/2004
*: System        : Aria Advantage Series.
*: Module        : SO2
*: Developer     : TMI - TAREK MOHAMED IBRAHIM
*: Purpose       : This entity will import customer's information from 
*:               : spreadsheet to Aria's customer file.
*: Entry #       : *C122832,1 
*:**************************************************************************
*--Calling the option grid

DIMENSION laDefCode[6,3]
laDefCode[1,1] = 'REGION    '      && Code ID
laDefCode[1,2] = 'Region'          && Code Desctiption , code value is saved in column 3
laDefCode[2,1] = 'CLASS     '
laDefCode[2,2] = 'Class'
laDefCode[3,1] = 'CTERMCODE '
laDefCode[3,2] = 'Terms'
laDefCode[4,1] = 'SHIPVIA   '
laDefCode[4,2] = 'Ship Via'
laDefCode[5,1] = 'CDIVISION '
laDefCode[5,2] = 'Division'
laDefCode[6,1] = 'SPCINST   '
laDefCode[6,2] = 'Spc. Inst.'

*- Log window title
lcWinTitl = 'Import Log Errors Screen.'

*- Open needed files
=lfOpenFls()

IF NOT lfGtDefCod()
  RETURN
ENDIF  

lcTmpFl = gfTempName()
lcTmpLog = gfTempName()
lcSlctFl = ' '
lnFilHandl = 0
llOpenRep = .F.

ON KEY LABEL ESC DO lfvCancel

DO (gcScrDir+'SO\SOLNIMP.SPX')

*:**************************************************************************
*:* Name        : lfOpenFls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/22/2004
*:* Purpose     : Open needed files
*:***************************************************************************
*C122832,1
FUNCTION lfOpenFls
=gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER')
=gfOpenFile(gcDataDir+'ARCUSHST','ACTHST')
=gfOpenFile(gcDataDir+'CODES'   ,'CCODE_NO')
=gfOpenFile(gcDataDir+'CONTACT' ,'CONTACT')
=gfOpenFile(gcDataDir+'NOTEPAD' ,'NOTEPAD')
=gfOpenFile(gcDataDir+'SALESREP','SALESREP')
=gfOpenFile(gcDataDir+'ARCUSHST','ACTHST')
*-- end of lfOpenFls.

*:**************************************************************************
*:* Name        : lfGtDefCod
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/20/2004
*:* Purpose     : Get default code for selected codes
*:***************************************************************************
*C122832,1
FUNCTION lfGtDefCod

&& Codes file key : CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM

FOR lnk = 1 TO ALEN(laDefCode,1)
  IF SEEK('D'+laDefCode[lnK,1],'CODES')
    laDefCode[lnK,3] = CODES.CCODE_NO  
  ELSE
    lcX = laDefCode[lnK,2]
    =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Code &lcX does not have a default value and the program will not proceed.')
    RETURN .F.
  ENDIF
ENDFOR

*-- end of lfGtDefCod.
*:**************************************************************************
*:* Name        : lfvSlctFl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/20/2004
*:* Purpose     : Valid fn for selecting file button
*:***************************************************************************
*C122832,1
FUNCTION lfvSlctFl
PARAMETERS llSlctBtn
IF llSlctBtn .OR. (!EMPTY(lcSlctFl) AND !FILE(lcSlctFl))
  lcSlctFl = GETFILE('XLS','Imported file','Select')
ENDIF
lcProcStat = IIF(EMPTY(lcSlctFl) , 'DISABLE' , 'ENABLE')
SHOW GET pbProceed &lcProcStat
IF lcProcStat = 'ENABLE'
  _CUROBJ = OBJNUM(pbProceed)
ENDIF
SHOW GET lcSlctFl
=lfRefresh('SOLNIMP')
*-- end of lfvSlctFl.

*:**************************************************************************
*:* Name        : lfvProceed
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/20/2004
*:* Purpose     : Importing function.
*:***************************************************************************
*C122832,1
FUNCTION lfvProceed
PRIVATE llErr,lcSvErr

WAIT WINDOW NOWAIT 'Prepare to Import..'
*- Create temp file
=lfCrTmp()


llErr = .F.
lcSvErr = ON('ERROR')
ON ERROR llErr = .T.

WAIT WINDOW NOWAIT 'Get Data from Excel file.'
SELECT &lcTmpFl
APPEND FROM (lcSlctFl) XLS
WAIT CLEAR

ON ERROR &lcSvErr

IF llErr
  lcMsg = 'Invalid Excel file format!' + CHR(13) +;
          'The file must be saved in Excel 4.0 format.'
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcMsg)
  RETURN
ENDIF

*-Disable Proceed button
SHOW GET pbProceed DISABLE

SELECT &lcTmpFl
GO TOP
SCAN FOR !EMPTY(&lcTmpFl..CLMN_A)
  
  WAIT WINDOW NOWAIT 'Importing Data for customer : '+ PADL(ALLTRIM(&lcTmpFl..CLMN_A),5,'0')

  SELECT CUSTOMER
  SCATTER MEMVAR BLANK

  SELECT &lcTmpFl
  SCATTER MEMVAR
  
  M.TYPE       = "M"  
  M.BILLTO     = "M"
  M.ACCOUNT    = PADL(ALLTRIM(&lcTmpFl..CLMN_A),5,'0')
  M.BTNAME     = ALLTRIM(&lcTmpFl..CLMN_B)
  M.STNAME     = ALLTRIM(&lcTmpFl..CLMN_B)

  M.CADDRESS1  = ALLTRIM(&lcTmpFl..CLMN_D)
  M.CADDRESS12 = ALLTRIM(&lcTmpFl..CLMN_D)
  M.cAddress3  = ALLTRIM(&lcTmpFl..CLMN_E)
  M.cAddress32 = ALLTRIM(&lcTmpFl..CLMN_E)
  M.cAddress4  = ALLTRIM(&lcTmpFl..CLMN_F)
  M.cAddress42 = ALLTRIM(&lcTmpFl..CLMN_F)
  M.cAddress6  = ALLTRIM(&lcTmpFl..CLMN_G)
  M.cAddress62 = ALLTRIM(&lcTmpFl..CLMN_G)
  M.cAddress5  = ALLTRIM(&lcTmpFl..CLMN_H)
  M.cAddress52 = ALLTRIM(&lcTmpFl..CLMN_H)
  M.Phone1     = ALLTRIM(&lcTmpFl..CLMN_I)
  
  M.SALESREP   = ALLTRIM(&lcTmpFl..CLMN_N)
  M.CrLimit    = VAL(&lcTmpFl..CLMN_O)

  M.STORE       = '        '
  M.STATUS      = "A"
  M.LINK_CODE   = "DEFDEF"
  M.CONSOL      = "N"
  M.PRIORITY    = "5"
  M.Region      = laDefCode[1,3] 
  M.Class       = laDefCode[2,3]
  M.cTermCode   = laDefCode[3,3]
  M.ShipVia     = laDefCode[4,3]
  M.cDivision   = laDefCode[5,3]
  M.SpcInst     = laDefCode[6,3]           

  M.CINSUR      = "Y"  
  M.PRICELVL    = "A"
  M.SkuTmpl     = 'DEF'
  M.Prnt_Statm  = 'Y'
  M.cTaxRule    = ' 1'
  M.cCont_Code  = GCCONTCODE
  M.cCont_Cod2  = GCCONTCODE
  M.cCurrCode   = gcBaseCurr
  M.CslsGlLink = 'DEF'
  M.CBackOrd = ' '

  M.CAdd_User = "IMPORT_PRG"
  M.cAdd_Time = TIME()
  M.dAdd_Date = DATE()
  M.CADD_VER  = 'A27'

  && Save errors in log file
  IF EMPTY(M.BTNAME)
    =lfSvErr('Empty name.')
    LOOP
  ENDIF
  
  *-The program check that the account code does not exist in Aria's customer file
  IF SEEK('M'+M.ACCOUNT,'CUSTOMER')
    =lfSvErr('Customer Already exists.')  
    LOOP
  ENDIF


  SELECT CUSTOMER  
  INSERT INTO CUSTOMER FROM MEMVAR

  *- Add notepad
  IF !EMPTY(&lcTmpFl..CLMN_K)
    SELECT NOTEPAD
    IF !SEEK('A'+PADR(M.ACCOUNT,20),'NOTEPAD')
      APPEND BLANK      
      REPLACE TYPE     WITH 'A' ;
              KEY      WITH M.ACCOUNT ;
              CDESC    WITH 'Notes For Account Number : ' + M.ACCOUNT ;
              MNOTES   WITH &lcTmpFl..CLMN_K
      =lfUsrTmDt()
    ENDIF    
  ENDIF
  
  *- Add contact
  && Contact Key : CCONTTYPE+CCONT_ID+STORE+CONTACT
  IF !EMPTY(&lcTmpFl..CLMN_L)
    IF !SEEK('C'+M.ACCOUNT+M.STORE+&lcTmpFl..CLMN_L,'CONTACT')
      INSERT INTO CONTACT (CCONTTYPE,CCONT_ID,CONTACT) VALUES ('C',M.ACCOUNT,&lcTmpFl..CLMN_L)
      =lfUsrTmDt('CONTACT')
    ENDIF
  ENDIF
  
  *- Add repcode to salesrep file
  IF !SEEK(M.SALESREP,'SALESREP')
    SELECT SALESREP
    APPEND BLANK
    REPLACE REPCODE    WITH M.SALESREP ;
            NAME       WITH M.SALESREP ;
            REGION     WITH laDefCode[1,3] ;
            CTERMCODE  WITH laDefCode[3,3] ;
            PAY_TYPE   WITH 'R' ;
            CCONT_CODE WITH GCCONTCODE ;
            CADDRESS6  WITH M.cAddress6
    =lfUsrTmDt('SALESREP')
  ENDIF
  
  IF !SEEK(M.ACCOUNT+gcCurrYear,'ARCUSHST')
    SELECT ARCUSHST
    FOR lnFsYear = -1 TO 1
      APPEND BLANK
      REPLACE ACCOUNT   WITH M.ACCOUNT ;
              CFISFYEAR WITH STR(VAL(gcCurrYear)+lnFsYear,4)
      =lfUsrTmDt()
    ENDFOR 
  ENDIF

ENDSCAN
WAIT CLEAR

*-Show log error file 
=lfViewRLog()

*-- end of lfvProceed.

*:**************************************************************************
*:* Name        : lfCrTmp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/20/2004
*:* Purpose     : Create temp file to import data to
*:***************************************************************************
*C122832,1
FUNCTION lfCrTmp
PRIVATE lnI,lnK,laFileSt,lnSlct
lnSlct = SELECT()

IF !USED(lcTmpFl)
  lnI = 0 
  FOR lnK = ASC('A') TO ASC('Z')
    lnI = lnI + 1
    DIMENSION laFileSt[lnI,4]
    laFileSt[lnI,1] = 'CLMN_' + CHR(lnK)
    laFileSt[lnI,2] = 'C'
    laFileSt[lnI,3] = 35
    laFileSt[lnI,4] = 0
  ENDFOR  
  
  CREATE TABLE (gcWorkDir+lcTmpFl) FROM ARRAY laFileSt
  INDEX ON CLMN_A TAG &lcTmpFl
  
ELSE

  SELECT &lcTmpFl
  ZAP

ENDIF  

SELECT (lnSlct)
*-- end of lfCrTmp.

*:**************************************************************************
*:* Name        : lfvCancel
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/20/2004
*:* Purpose     : Cancel Import
*:***************************************************************************
*C122832,1
FUNCTION lfvCancel
IF USED(lcTmpFl)
  USE IN &lcTmpFl
ENDIF
ERASE (gcWorkDir+lcTmpFl+'.DBF')
ERASE (gcWorkDir+lcTmpFl+'.CDX')
CLEAR READ
*-- end of lfvCancel.

*:**************************************************************************
*:* Name        : lfSvErr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/20/2004
*:* Purpose     : Record an error in log file
*:***************************************************************************
*C122832,1
FUNCTION lfSvErr
PARAMETERS lcErrMsg
IF NOT llOpenRep
  =lfCrtLog()
ENDIF
=FPUTS(lnFilHandl,M.ACCOUNT+'    '+PADR(M.BTNAME,31)+lcErrMsg)

*-- end of lfSvErr.

*:**************************************************************************
*:* Name        : lfCrtLog
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/20/2004
*:* Purpose     : Create import log file
*:***************************************************************************
*C122832,1
FUNCTION lfCrtLog
llOpenRep = .T.
lnFilHandl = FCREAT(gcWorkDir+lcTmpLog+'.TXT')
=FPUTS(lnFilHandl,REPLICATE('*',60))
=FPUTS(lnFilHandl,"*                  Import Errors Log File                  *")
=FPUTS(lnFilHandl,REPLICATE('*',60))
=FPUTS(lnFilHandl,'Acc.Code '+PADR('Account Name',31)+'Rejection Reason')
=FPUTS(lnFilHandl,' ')
=FPUTS(lnFilHandl,' ')
*-- end of lfCrtLog.

*!*************************************************************
*! Function  : lfViewRLog
*! Developer : Amin Khodary Amin
*! Date      : 08/23/1999
*! Purpose   : - Preview rebalance log file (ReBalLog.txt). 
*!*************************************************************
*C122832,1
FUNCTION lfViewRLog

*** If there is a report created dispaly it to the user
IF llOpenRep
  =FFLUSH(lnFilHandl)
  * Close the text file
  =FCLOSE(lnFilHandl)
  lnFilHandl = 0
  llOpenRep = .F.
  lcSlctFl = ' '
  =lfRefresh('SOLNIMP')
  
  CREATE CURSOR TMPSTR (mStrRep M(10))
  APPEND BLANK
  APPEND MEMO mStrRep FROM (gcWorkDir+lcTmpLog+'.TXT') OVERWRITE
  DO (gcScrDir + 'SO\SOLNLOG.SPX')
  USE IN TMPSTR
  ERASE (gcWorkDir+lcTmpLog+'.TXT')
ELSE
  =gfModalGen("INM00000B00000",.F.,.F.,.F.,'File Imported Successfully.')   
ENDIF

*:**************************************************************************
*:* Name        : lfUsrTmDt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/22/2004
*:* Purpose     : Add User ,Time and Date to files
*:***************************************************************************
*C122832,1
FUNCTION lfUsrTmDt
PARAMETERS lcAlias
lnSlct = SELECT()
IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF  
REPLACE CAdd_User WITH "IMPORT_PRG" ;
        cAdd_Time WITH TIME() ;
        dAdd_Date WITH DATE() ;
        CADD_VER WITH 'A27'
SELECT (lnSlct)        
*-- end of lfUsrTmDt.
*:**************************************************************************
*:* Name        : lfvPrnt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/22/2004
*:* Purpose     : print log file
*:***************************************************************************
FUNCTION lfvPrnt

IF pSetup(.T.)
  gcOutFile = gcWorkDir+gfTempName()+'.TXT'
  COPY MEMO TMPSTR.mStrRep TO &gcOutFile
  gcDevice = 'PRINTER'
  DO ENDREPORT
  gcDevice = 'SCREEN'
ENDIF

*-- end of lfvPrnt.

*:**************************************************************************
*:* Name        : lfvSaveAs
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/23/2004
*:* Purpose     : Save the log file
*:***************************************************************************
*:* Called from : Solnlog.spr
*:***************************************************************************
FUNCTION lfvSaveAs
PRIVATE lcSaveAs,llCopy
lcSaveAs = GETFILE('TXT')
IF !EMPTY(lcSaveAs)  
  llCopy = .T.
  IF FILE(lcSaveAs)  
    llCopy = (gfModalGen('INM00356B00006','DIALOG',lcSaveAs)=1)
  ENDIF
  IF llCopy
    COPY MEMO TMPSTR.mStrRep TO &lcSaveAs
  ENDIF
ENDIF

*-- end of lfvSaveAs.