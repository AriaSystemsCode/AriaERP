*:************************************************************************
*:   
*: Procedure file: GLGNALO.PRG
*:
*:         System: ARIA BUSINESS SYSTEMS
*:         Module: General Ledger
*:         Author: Mohamed Hassan Mohamed
*:      Copyright (c) 
*:  Last modified: 12/26/1993
*:
*:  Procs & Fncts: lfwLine
*:               : lfwGenDate
*:               : lfvGenDate
*:               : lfvGenerate
*:               : lfGenBefor
*:               : lfNetAct
*:               : lfUnPosted
*:               : lfRecStat
*:               : lfvSelect
*:               : lfvAll
*:               : lfvInvert
*:               : lfvNone
*:               : lfvStatus
*:               : lfFillDet
*:               : lfvView
*:               : lfvUpDate
*:               
*:      Documented  /  /   at   :  
*:************************************************************************
*600537,1 ORABY 07/05/95 The Select push button does not function if used directly after generation
*600538,1 ORABY 07/05/95 After generation change the Select/UnSelect prompt to "SELECT"
*E300663,1  HS 04/23/97 Change the calling of the function [gfSequence]
*E300663,1              for the changes we have made to that function.
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*E300692,1 ESSMAT 06/30/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*E300789,6 MAB 03/04/1999 Index does not have company Id.
*B604054,1 SSE 12/05/2000 Always retrieves the balance of the first preriod.  
*:************************************************************************
*
*** Variables declaration 

lcYear     = ' '  && Variable to hgld the year sending date
lcBatchStat= ' '  && Variable to hold the Batch Status.
lcLine     = ' '  && Variable to hold the fields of the list.
lcDetail   = ' '  && Variable to hold the fields of the list.
lctAllo    = ' '  && Variable to hold the Child window title
lcBatchNo  = ' '  && Variable to hold the Batch No. 
lcTmpAlo   = ' '  && Variable to hold the TEMP file Name
lcTmpDtl   = ' '  && Variable to hold the TEMP file Name
lcTmpBatch = ' '  && Variable to hold the TEMP file Name for Batch
lcTmpTrnHd = ' '  && Variable to hold the TEMP file Name for Transaction Header
lcTmpTrnDt = ' '  && Variable to hold the TEMP file Name for Transaction Detail
lcTmpSelCd = ' '  && Variable to hold the TEMP file Name for the selected codes

lcSavTag1  = ' '  && Variable to hold the tag
lcSavTag2  = ' '  && Variable to hold the tag
lcSavTag3  = ' '  && Variable to hold the tag
lcSavTag4  = ' '  && Variable to hold the tag
lcGenStat  = 'ENABLE'
lcObjStat  = 'DISABLE'
lcSelect   = ' '
lcStandard = ' '
lcUserName = ' '

lcTAllocat = ''  && Variable already declared in the screen object file.
lcTGenAllo = ''  && Variable is already declared in the screen object file
lcTAlocat  = ''  && Variable to be declared in the screen object file
lcTSelect  = ''  && Variable to be declared in the screen object file
lcTUnSelct = ''  && Variable to be declared in the screen object file
lcTDefTxt1 = ''  && Variable to hold 'Allocation Trn.'
lcTDefTxt2 = ''  && Variable to hold 'Allocation code'
lcTDefTxt3 = ''  && Variable to hold 'Created by'
lcTDefTxt4 = ''  && Variable to hold 'On'
lcTUpdMast = ''  && Variable already declared in the screen object file.

ldGenDate  = {}   && Variable to hold the Generation Date
ldOldDate  = {}   && Variable to hold the Previos Generated Date
ldPrBegin  = {}   && Variable to hold the period begin date
ldPrEnd    = {}   && Variable to hold the period End date

lsline     = 1    && Variable of the header list
lnline     = 1    && Variable of the header list
lsDetail   = 1    && Variable of the detail list
lnGenPrd   = 0    && Variable to hold the period
lnNetAct   = 0    && Variable to hold the net activity
lnTotDb    = 0    && Variable to hold the total debit
lnTotCr    = 0    && Variable to hold the total credit
rbAllocat  = 1    && Set default to the first choice of the radio button
lnTotRec   = 0
lnCurRec   = 0

llLockSt   = .F.  && Variable to hold the period lock stat
llSelCode  = .F.  && Varibale to hold if the is a selection done or not 
llFirstSel = .T.  && Variable to hold if there is any selection befor
llUpdate   = .F.  && Variable to hold if there is a Update done in the selection screen
laCtrStat  = "DISABLE"  && To disable all bars of record popoup 
llNoContrl = .T.

DECLARE laWndObj   [2,3]  && Have screen name & first & last obj. for each screen

IF !gfSetup()    
  RETURN
ENDIF  

laWndObj [1,1] = gcBAseWind
laWndObj [1,2] = "LDGENDATE"
laWndObj [1,3] = "PBCLOSE"

laWndObj [2,1] = "CWDGLGALO0"
laWndObj [2,2] = "LSDETAIL"
laWndObj [2,3] = "PBSGCLOSE"

SELECT GLAUTHD
SET ORDER TO TAG TYPECODE

*** Seek just for one allocation code in the automaic header file. ***
IF !SEEK("A")
  *** There is no Allocation transaction ***
  *** for the process generate...
  *** < Ok > ***
  =gfModalGen("TRM02109B00000","DIALOG","allocation")
  glQuitting  = .T.  
  RETURN 
ENDIF

IF !WEXIST(gcBaseWind)
  
  *** Get the end date of the current period ***
  *** as a default date...
  
*E300692,1 Change file name from SYCFSPRD to FSPRD
  *IF SEEK(gcPrnt_Cmp+lcCurr_yer+lcCurr_prd,"SYCFSPRD")
    *ldGenDate = SYCFSPRD.dfsppendt

    *E300789,6 Index does not have company Id [Begin
    *IF SEEK(gcPrnt_Cmp+lcCurr_yer+lcCurr_prd,"FSPRD")
    IF SEEK(lcCurr_yer+lcCurr_prd,"FSPRD")
    *E300789,6 Index does not have company Id [End
    ldGenDate = FSPRD.dfsppendt
*E300692,1 end

    lnGenPrd = gfPeriod(ldGenDate,gcPrnt_Cmp,@lcYear,@ldPrBegin,@ldPrEnd,@llLockSt)
  ENDIF

  *** Create a name for the temporary file 

  lcTmpAlo   = gfTempName() && Varible to hold the TEMP file Name
  lcTmpDtl   = gfTempName() && Varible to hold the TEMP file Name
  lcTmpBatch = gfTempName() && Varible to hold the TEMP file Name for Batch
  lcTmpTrnHd = gfTempName() && Varible to hold the TEMP file Name for Transaction Header
  lcTmpTrnDt = gfTempName() && Varible to hold the TEMP file Name for Transaction Detail
  lcTmpSelCd = gfTempName() && Varible to hold the TEMP file Name for the selected codes

  SELECT GLBATCH
  COPY STRUCTURE TO &gcWorkDir.&lcTmpBatch CDX
  SELECT 0
  USE &gcWorkDir.&lcTmpBatch EXCLUSIVE
  
  SELECT GLTRNSHD
  COPY STRUCTURE TO &gcWorkDir.&lcTmpTrnHd CDX
  SELECT 0
  USE &gcWorkDir.&lcTmpTrnHd EXCLUSIVE
  
  SELECT GLTRNSDT
  COPY STRUCTURE TO &gcWorkDir.&lcTmpTrnDt CDX
  SELECT 0
  USE &gcWorkDir.&lcTmpTrnDt EXCLUSIVE
  
  SELECT SPACE(lnAcsSegSz) AS 'CACCTCODE',;
         SPACE(15) AS 'NDEBIT',SPACE(15) AS 'NCREDIT';         
    FROM &gcDataDir.GLAUTDT;
   WHERE cauttype+cautcode = ' ';
    INTO DBF &gcWorkDir.&lcTmpDtl

  SELECT (lcTmpDtl)
  
  DO CASE
    CASE _DOS
      lcDetail ="SUBSTR(&lcTmpDtl..CACCTCODE,1,lnAcsSegSz)+' '+"+;
                "SUBSTR(LOOKUP(glAcchar.cAccnldes,&lcTmpDtl..cAcctCode,"+;
                "glAcchar.cAcctCode,'ACCTCODE'),1,40-lnAcsSegSz)+' '+"+;
                "&lcTmpDtl..NDEBIT+' '+&lcTmpDtl..NCREDIT"
    CASE _WINDOWS
      lcDetail ="SUBSTR(&lcTmpDtl..CACCTCODE,1,lnAcsSegSz)+' '+"+;
                "SUBSTR(LOOKUP(glAcchar.cAccnldes,&lcTmpDtl..cAcctCode,"+;
                "glAcchar.cAcctCode,'ACCTCODE'),1,40-lnAcsSegSz)+' '+"+;
                "&lcTmpDtl..NDEBIT+' '+&lcTmpDtl..NCREDIT"
  ENDCASE
 
  SELECT GLAUTHD.CAUTTYPE,GLAUTHD.CAUTCODE,;
         GLAUTHD.CAUTREF,GLAUTHD.CAUTBASE,;
         GLAUTHD.CAUTDES,SPACE(7) AS 'cStatus',;
         SPACE(3) AS 'cGnBefor',SPACE(6) AS 'cGnBatNo',;
         SPACE(1) AS 'cGnBatSt',GLAUTHD.CACTCODE,;
         000000000000.00 AS 'nNetAct',;
         SPACE(3) AS 'cUnPostTr',0 AS 'nRecStat',;
         SPACE(1) AS 'cInclude',CSRCJRNL,SPACE(1) AS CSTANDARD;
    FROM &gcDataDir.GLAUTHD;
   WHERE glauthd.cautcode = ' ';
     AND glauthd.CAUTTYPE = ' ';
    INTO DBF &gcWorkDir.&lcTmpAlo
  
  SELECT(lcTmpAlo)

  DO CASE
    CASE _DOS
      lcline ="&lcTmpAlo..cAutCode+' '+&lcTmpAlo..cStatus+'   '+&lcTmpAlo..cInclude+'    '+SUBSTR(&lcTmpAlo..cAutDes,1,32)+' '+&lcTmpAlo..cAutRef"
    CASE _WINDOWS
      lcline ="&lcTmpAlo..cAutCode+' '+&lcTmpAlo..cStatus+'  '+&lcTmpAlo..cInclude+'  '+SUBSTR(&lcTmpAlo..cAutDes,1,28)+' '+&lcTmpAlo..cAutRef"
  ENDCASE  
ENDIF

SELECT GLAUTHD
lcSavTag1 = SYS(22)
SET ORDER TO TAG TYPECODE 

SELECT GLAUTDT
lcSavTag2 = SYS(22)
SET ORDER TO TAG TYPCODACC 
  
SELECT GLACCHAR
lcSavTag3 = SYS(22)
SET ORDER TO TAG ACCTCODE 
  
SELECT GLTRNSDT
lcSavTag4 = SYS(22)
SET ORDER TO TAG ACCTCODE 
  
SELECT GLTRNSHD
lcSavTag5 = SYS(22)
SET ORDER TO TAG BATCHTRN 

SELECT GLAUTHD
SET RELATION TO glauthd.cauttype + glauthd.cautcode INTO GLAUTDT ADDITIVE

SELECT GLAUTDT
SET RELATION TO glautdt.cacctcode INTO GLACCHAR ADDITIVE

SELECT GLAUTHD
SET RELATION TO glauthd.cactcode INTO GLTRNSDT ADDITIVE

SELECT GLTRNSDT
SET RELATION TO gltrnsdt.cbatchno + gltrnsdt.ctranno INTO GLTRNSHD ADDITIVE

SELECT(lcTmpAlo)  
SET RELATION TO cauttype + cautcode INTO GLAUTHD ADDITIVE

*E300683,5 Call *.SPR from screens directory
* DO GLGNALO.SPR 
DO (gcScrDir + gcWinAppl + '\GLGNALO.SPR')
*E300683,5 end   
SELECT GLAUTHD
SET RELATION TO 

SELECT GLAUTDT
SET RELATION TO 

SELECT GLAUTHD
SET RELATION TO 

SELECT GLTRNSDT
SET RELATION TO 

SELECT(lcTmpAlo)  
SET RELATION TO 


IF glQuitting 
  IF WEXIST("CWDGLGALO0")
    RELEASE WINDOW CWDGLGALO0
  ENDIF
  
  SELECT GLAUTHD
  SET RELATION TO
  SET ORDER TO &lcSavTag1 

  SELECT GLAUTDT
  SET RELATION TO
  SET ORDER TO &lcSavTag2
   
  SELECT GLACCHAR
  SET ORDER TO &lcSavTag3
  
  SELECT GLTRNSDT
  SET RELATION TO
  SET ORDER TO &lcSavTag4
  
  SELECT GLTRNSHD
  SET ORDER TO &lcSavTag5

  SELECT(lcTmpAlo)  
  SET RELATION TO 

  IF USED('&lcTmpAlo')
    USE IN &lcTmpAlo
  ENDIF  
  ERASE &gcWorkDir.&lcTmpAlo+'.DBF'

  IF USED('&lcTmpDtl')
    USE IN &lcTmpDtl
  ENDIF  
  ERASE &gcWorkDir.&lcTmpDtl+'.DBF'

  IF USED('&lcTmpBatch')
    USE IN &lcTmpBatch
  ENDIF  
  ERASE &gcWorkDir.&lcTmpBatch+'.DBF'
  ERASE &gcWorkDir.&lcTmpBatch+'.CDX'
  ERASE &gcWorkDir.&lcTmpBatch+'.FPT'


  IF USED('&lcTmpTrnHd')
    USE IN &lcTmpTrnHd 
  ENDIF  
  ERASE &gcWorkDir.&lcTmpTrnHd+'.DBF'
  ERASE &gcWorkDir.&lcTmpTrnHd+'.CDX'
  ERASE &gcWorkDir.&lcTmpTrnHd+'.FPT'

  IF USED('&lcTmpTrnDt')
    USE IN &lcTmpTrnDt 
  ENDIF  
  ERASE &gcWorkDir.&lcTmpTrnDt+'.DBF'
  ERASE &gcWorkDir.&lcTmpTrnDt+'.CDX'
  ERASE &gcWorkDir.&lcTmpTrnDt+'.FPT'
 
  IF llSelCode
    IF USED('&lcTmpSelCd')
      USE IN &lcTmpSelCd 
    ENDIF  
    ERASE &gcWorkDir.&lcTmpSelCd+'.DBF'
    ERASE &gcWorkDir.&lcTmpSelCd+'.CDX'
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function: lfwGenDate
*!
*!**************************************************************************
*
FUNCTION lfwGenDate

**Saving the old genration date 
ldOldDate = ldGenDate

*!**************************************************************************
*!
*!      Function: lfvGenDate
*!
*!**************************************************************************
*
FUNCTION lfvGenDate

IF ldGenDate <> ldOldDate
 
  *lnGenPrd = gfPeriod(ldGenDate,gcAct_Comp,@lcYear,@ldPrBegin,@ldPrEnd,@llLockSt)
  lnGenPrd = gfPeriod(ldGenDate,gcPrnt_Cmp,@lcYear,@ldPrBegin,@ldPrEnd,@llLockSt)

  IF !EMPTY(ldGenDate) 

    IF !BETWEEN(ldGenDate,ldPyBgDate,ldNyEnDate) 
      *** The generation date should be within the posting window. ***
      *** <  Ok  > ***
      =gfModalGen("TRM02074B00000","DIALOG")
      ldGenDate = ldOldDate
      _CUROBJ=OBJNUM(ldGenDate)
    ELSE    
      IF lnGenPrd = 0 
        *** The generation date has no period in the fisical year. ***
        *** <  Ok  > ***
        =gfModalGen("TRM02106B00000","DIALOG")
        ldGenDate = ldOldDate 
        _CUROBJ=OBJNUM(ldGenDate)
      ELSE
        IF llLockSt
          *** Period NO.-YEAR is Locked. ***
          *** <  Ok  > ***
          =gfModalGen("TRM02107B00000","DIALOG",PADL(lnGenPrd,2,'0')+'-'+lcYear)
          ldGenDate = ldOldDate
          _CUROBJ=OBJNUM(ldGenDate)
        ELSE
          llFirstSel = .T.
          rbAllocat  = 1
          SHOW GET rbAllocat
          =lfReGenrat()
        ENDIF  
      ENDIF  
    ENDIF
  ELSE
    ldGenDate = ldOldDate
    _CUROBJ=OBJNUM(ldGenDate)
    SHOW GET ldGenDate
  ENDIF  
ENDIF  

SELECT(lcTmpAlo)

*!**************************************************************************
*!
*!      Procedure: lfwLine
*!
*!**************************************************************************
*
FUNCTION lfwLine

IF RECCOUNT(lcTmpAlo) > 0
  IF lnline <> lsline
  
    =lfRefresh()  && Refresh the total amount of debit & credit.

    =lfUpdPrmpt()

    IF WVISIBLE('CWDGLGALO0')
      =lfFillDet()
    ENDIF

    SELECT(lcTmpAlo)
    lnline = lsline
  ENDIF  
ENDIF  

*!**************************************************************************
*!
*!      Function : lfvAllocat
*!
*!**************************************************************************
*
FUNCTION lfvAllocat

llSelCode = .T.   && Variable to hold if there is a Selection done or not

IF rbAllocat = 2
  IF llFirstSel 
    *E300683,5 Call programs from PRGS directory
    *DO glSelCd.PRG WITH lcTmpSelCd,'A',.T.
    DO (gcAppHome + gcWinAppl + '\glSelCd.PRG ') WITH lcTmpSelCd,'A',.T.
    *E300683,5 end
    llFirstSel = .F.
  ELSE
    *E300683,5 Call programs from PRGS directory
    *DO glSelCd.PRG WITH lcTmpSelCd,'A'
    DO (gcAppHome + gcWinAppl + '\glSelCd.PRG ') WITH lcTmpSelCd,'A'
    *E300683,5 end
  ENDIF  
  SELECT (lcTmpSelCd)
  IF llUpdate
    =lfReGenrat()
  ENDIF
ELSE
  IF USED('&lcTmpSelCd')
    SELECT (lcTmpSelCd)  
    GO TOP
    REPLACE ALL &lcTmpSelCd..cSelect WITH ' '
  ENDIF
  =lfReGenrat()
ENDIF

llUpdate = .F.

*!**************************************************************************
*!
*!      Function: lfvGenerate
*!
*!**************************************************************************
*
FUNCTION lfvGenerate

IF rbAllocat = 1

  SELECT GLAUTHD

  lnTotRec = 0
  lnCurRec = 0

  COUNT FOR cAutType = 'A' TO lnTotRec

  IF SEEK('A')
    SCAN REST WHILE cAutType = 'A'
      lnCurRec = lnCurRec + 1
      =gfThermo(lnTotRec,lnCurRec,lcTGenAllo,CAUTCODE)
      INSERT INTO &gcWorkDir.&lcTmpAlo;
            (CAUTTYPE,CAUTCODE,CAUTREF,CAUTBASE,CAUTDES,CACTCODE,;
             CINCLUDE,CSRCJRNL);
      VALUES(GLAUTHD.CAUTTYPE,GLAUTHD.CAUTCODE,GLAUTHD.CAUTREF,;
             GLAUTHD.CAUTBASE,GLAUTHD.CAUTDES,GLAUTHD.CACTCODE,;
             ' ',GLAUTHD.CSRCJRNL)
      =lfGenBefor() 
      =lfNetAct() 
      =lfUnPosted()
      =lfRecStat() 
      SELECT GLAUTHD
    ENDSCAN
    IF lnTotRec > lnCurRec
      FOR lnThCount = lnCurRec TO lnTotRec
        =gfThermo(lnTotRec,lnThCount,lcTGenAllo,"")
      ENDFOR
    ENDIF  
  ELSE
    =gfModalGen("TRM02034B00000","DIALOG",lcTAllocat)
  ENDIF
ELSE
  SELECT (lcTmpSelCd)
  lnTotRec = 0
  lnCurRec = 0
  COUNT FOR cInclude = 'û' .AND. cAutType = 'A' TO lnTotRec
  
  IF lnTotRec > 0
    SCAN FOR cInclude = 'û' .AND. cAutType = 'A'
      lnCurRec = lnCurRec + 1
      =gfThermo(lnTotRec,lnCurRec,lcTGenAllo,CAUTCODE)
      INSERT INTO &gcWorkDir.&lcTmpAlo;
            (CAUTTYPE,CAUTCODE,CAUTREF,CAUTBASE,CAUTDES,CACTCODE,;
             CINCLUDE,CSRCJRNL);
      VALUES(&lcTmpSelCd..CAUTTYPE,&lcTmpSelCd..CAUTCODE,;
             &lcTmpSelCd..CAUTREF,&lcTmpSelCd..CAUTBASE,;
             &lcTmpSelCd..CAUTDES,&lcTmpSelCd..CACTCODE,;
             ' ',&lcTmpSelCd..CSRCJRNL)
      =lfGenBefor() 
      =lfNetAct() 
      =lfUnPosted()
      =lfRecStat() 
      SELECT (lcTmpSelCd)
    ENDSCAN
    IF lnTotRec > lnCurRec
      FOR lnThCount = lnCurRec TO lnTotRec
        =gfThermo(lnTotRec,lnThCount,lcTGenAllo,"")
      ENDFOR
    ENDIF  
  ELSE
    =gfModalGen("TRM02053B00000","DIALOG",lcTAllocat)
  ENDIF
ENDIF

lsLine = 1

IF lnCurRec >= 1

  lcGenStat  = 'DISABLE'
  lcObjStat  = 'ENABLE'

  SHOW GET lsLine     
  SHOW GET lsDetail   
  SHOW GET pbGenerate DISABLE
  SHOW GET pbUpdate   ENABLE
  *600538,1 After generation change the Select/UnSelect prompt to "SELECT"
* SHOW GET pbSelect   ENABLE
  SHOW GET pbSelect,1 PROMPT lcTSelect ENABLE
  SHOW GET pbAll      ENABLE
  SHOW GET pbNone     ENABLE
  SHOW GET pbInvert   ENABLE
  SHOW GET pbView     ENABLE
  SHOW GET pbStatus   ENABLE
ENDIF  

IF WVISIBLE('CWDGLGALO0')
  =lfFillDet()
ENDIF  

=lfRefresh()   && Refresh the say fields.

_CUROBJ = OBJNUM(lsLine)
*600537,1 Go to the record number same as the pointed on the list
GOTO lnCurRec

*!**************************************************************************
*!
*!      Function: lfGenBefor
*!
*!**************************************************************************
*
FUNCTION lfGenBefor

REPLACE &lcTmpAlo..cGnBefor WITH 'No',;
        &lcTmpAlo..cGnBatNo WITH ' ' ,;
        &lcTmpAlo..cGnBatSt WITH ' '

SELECT GLTRNSHD

lcAutCode = IIF(rbAllocat = 1,GLAUTHD.cAutCode,&lcTmpSelCd..cAutCode)

LOCATE FOR cAutType = 'A'       .AND.;
           cAutCode = lcAutCode .AND.;
           cTrnpyr  = lcYear    .AND.;
           cTrnpprd = PADL(lnGenPrd,2,'0')
IF FOUND()
  REPLACE &lcTmpAlo..cGnBefor WITH 'Yes',;
          &lcTmpAlo..cGnBatNo WITH GLTRNSHD.cBatchNo,;
          &lcTmpAlo..cGnBatSt WITH ;
          LOOKUP(glBatch.cBatStat,GLTRNSHD.cBatchNo,glBatch.cBatchNo,'BATCHNO')
ENDIF

*!**************************************************************************
*!
*!      Function: lfNetAct
*!
*!**************************************************************************
*
FUNCTION lfNetAct

REPLACE &lcTmpAlo..nNetAct WITH 0

lcSavTag = ' '   && Variable to hold the tag

SELECT GLACBALS
lcSavTag = SYS(22)
SET ORDER TO TAG ACCYRPRD
*MAN 11/28/200 Start
*B604054,1 SSE 12/05/2000 [Start]
*IF SEEK(&lcTmpAlo..cActCode+ALLTRIM(STR(YEAR(ldGenDate)))+SUBSTR('0'+ALLTRIM(STR(lnGenPrd)),1,2))
IF SEEK(&lcTmpAlo..cActCode+ALLTRIM(STR(YEAR(ldGenDate)))+PADL(lnGenPrd,2,'0'))
*MAN 11/28/200 End
*B604054,1 SSE 12/05/2000 [End]
  lnNetAct = (glAcBals.nAcBptddr - glAcBals.nAcBptdcr)
ENDIF

SET ORDER TO &lcSavTag

REPLACE &lcTmpAlo..nNetAct WITH lnNetAct

*!**************************************************************************
*!
*!      Function: lfUnPosted
*!
*!**************************************************************************
*
FUNCTION lfUnPosted

SELECT(lcTmpAlo)

**Replacing the Unposted transaction field with 'No'
REPLACE cUnPostTr WITH 'No '

SELECT GLTRNSDT
*ymo
SEEK &lcTmpAlo..cActCode

SCAN REST WHILE GLTRNSDT.cAcctCode = &lcTmpAlo..cActCode
  IF GLTRNSHD.cTrnStat = 'U'
    ** Replacing the Unposted transaction with 'Yes'
    ** if the transaction stat = 'U'
    REPLACE &lcTmpAlo..cUnPostTr WITH 'Yes'
    EXIT
  ENDIF
ENDSCAN

IF EMPTY(&lcTmpAlo..cUnPostTr)
  REPLACE &lcTmpAlo..cUnPostTr WITH 'No '
ENDIF

SELECT(lcTmpAlo)

*!**************************************************************************
*!
*!      Function: lfRecStat
*!
*!**************************************************************************
*
FUNCTION lfRecStat

lnRecNo   = 0
lnAloAmnt = 0   && Variable to hold the Allocation Amount

SELECT GLACCHAR

IF SEEK(&lcTmpAlo..cActCode)
  REPLACE &lcTmpAlo..CSTANDARD WITH GLACCHAR.CSTANDARD
  DO CASE
    CASE GLACCHAR.cSegActiv = 'I'
      REPLACE &lcTmpAlo..nRecStat WITH 3,;
              &lcTmpAlo..cStatus  WITH 'Skipped'
    CASE GLACCHAR.cSegAlPos = 'N'
      REPLACE &lcTmpAlo..nRecStat WITH 4,;
              &lcTmpAlo..cStatus  WITH 'Skipped'
  ENDCASE
ENDIF

IF &lcTmpAlo..nRecStat = 0
  IF &lcTmpAlo..cAutBase = 'P'
    SELECT(lcTmpAlo)
    lnRecNo = RECNO()
    GO TOP
    SUM GLAUTDT.nAmount WHILE &lcTmpAlo..CAUTTYPE+&lcTmpAlo..CAUTCODE=;
                           GLAUTDT.CAUTTYPE+GLAUTDT.CAUTCODE TO lnAloAmnt
    IF lnRecNo > 0
      GO lnRecNo       
    ENDIF
      
    IF ABS(lnNetAct) < (lnAloAmnt/100)
      REPLACE &lcTmpAlo..nRecStat WITH 2,;
              &lcTmpAlo..cStatus  WITH 'Skipped'
      RETURN
    ENDIF
  ENDIF

  SELECT GLAUTDT

  SCAN REST WHILE &lcTmpAlo..cAutType + &lcTmpAlo..cAutCode =;
                     GLAUTDT.cAutType + GLAUTDT.cAutCode
    DO CASE
      CASE GLACCHAR.cSegActiv = 'I'
        REPLACE &lcTmpAlo..nRecStat WITH 3,;
                &lcTmpAlo..cStatus  WITH 'Skipped'
        EXIT
      CASE GLACCHAR.cSegAlPos = 'N'
        REPLACE &lcTmpAlo..nRecStat WITH 4,;
                &lcTmpAlo..cStatus  WITH 'Skipped'
        EXIT
    ENDCASE
  ENDSCAN

  IF &lcTmpAlo..nRecStat = 0
    lcSavAlias = ALIAS()
    SELECT(lcTmpAlo)
    REPLACE nRecStat WITH 1,;
            cStatus  WITH 'Ready'
    SELECT(lcSavAlias)        
  ENDIF
ENDIF

SELECT(lcTmpAlo)

*!**************************************************************************
*!
*!      Function: lfvSelect
*!
*!**************************************************************************
*
FUNCTION lfvSelect

DO CASE

  CASE ALLTRIM(&lcTmpAlo..cStatus) = 'Ready' .AND. EMPTY(&lcTmpAlo..cInclude)
    ** Select if the record status = 'Ready' **
    REPLACE &lcTmpAlo..cInclude WITH 'û'

  CASE !EMPTY(&lcTmpAlo..cInclude)
    REPLACE &lcTmpAlo..cInclude WITH ' '

  CASE ALLTRIM(&lcTmpAlo..cStatus) = 'Skipped'
  ** This generated allocation cannot be selected **
  ** Ok **
  =gfModalGen("INM02088B00000","DIALOG")  

ENDCASE

_CUROBJ = OBJNUM(lsLine)
SHOW GET lsLine
=lfUpdPrmpt()

*!**************************************************************************
*!
*!      Function: lfvAll
*!
*!**************************************************************************
*
FUNCTION lfvAll

SELECT(lcTmpAlo)
GO TOP

** Select all the records with status = 'Ready' **
REPLACE ALL cInclude WITH 'û' FOR ALLTRIM(cStatus) = 'Ready'
  
_CUROBJ = OBJNUM(lsLine)
SHOW GET lsLine
=lfUpdPrmpt()

*!**************************************************************************
*!
*!      Function: lfvInvert
*!
*!**************************************************************************
*
FUNCTION lfvInvert

SELECT(lcTmpAlo)
GO TOP

** Select all the records with status = 'Ready' & not selected befor 
** and unselect the records witch was selected And status = 'Ready'
REPLACE ALL cInclude WITH IIF(cInClude='û',' ','û') FOR ALLTRIM(cStatus) = 'Ready'

_CUROBJ = OBJNUM(lsLine)
SHOW GET lsLine
=lfUpdPrmpt()

*!**************************************************************************
*!
*!      Function: lfvNone
*!
*!**************************************************************************
*
FUNCTION lfvNone

SELECT(lcTmpAlo)
GO TOP

** Unselect the records **
REPLACE ALL cInclude WITH ' '

_CUROBJ = OBJNUM(lsLine)
SHOW GET lsLine
=lfUpdPrmpt()

*!**************************************************************************
*!
*!      Function: lfvStatus
*!
*!**************************************************************************
*
FUNCTION lfvStatus

DO CASE
  CASE &lcTmpAlo..nRecStat = 1
    ** Allocation transaction ð is ready to be generated **
    ** Ok **
    =gfModalGen("TRM02089B00000","DIALOG",ALLTRIM(&lcTmpAlo..CAUTCODE))

  CASE &lcTmpAlo..nRecStat = 2
    ** The allocated account for allocation ð has no enough 
    ** activity to allocate. You cannot generate this allocation.
    ** Ok **
    =gfModalGen("TRM02090B00000","DIALOG",ALLTRIM(&lcTmpAlo..CAUTCODE))

  CASE &lcTmpAlo..nRecStat = 3 
    ** Allocation transaction ð has one or more accounts inactive.
    ** You cannot generate this allocation.
    ** Ok **
    =gfModalGen("TRM02091B00000","DIALOG",ALLTRIM(&lcTmpAlo..CAUTCODE))

  CASE &lcTmpAlo..nRecStat = 4
    ** Allocation transaction ð has one or more accounts not 
    ** allowed to recieve posting from the G/L. You cannot 
    ** generate this allocation.
    ** Ok **
    =gfModalGen("TRM02092B00000","DIALOG",ALLTRIM(&lcTmpAlo..CAUTCODE))

ENDCASE

_CUROBJ = OBJNUM(lsLine)
SHOW GET lsLine

*!**************************************************************************
*!
*!      Function: lfvView
*!
*!**************************************************************************
*
FUNCTION lfvView

IF !WVISIBLE('CWDGLGALO0')
  =lfFillDet()
ENDIF  

=gfActWind('CWDGLGALO0',lctAllo)

*!**************************************************************************
*!
*!      Function: lfFillDet
*!
*!**************************************************************************
*
FUNCTION lfFillDet

SELECT(lcTmpAlo)

lsDetail = 1
SHOW GET lsDetail 
SELECT (lcTmpDtl)
ZAP
SHOW GETS WINDOW CWDGLGALO0

lcSavAlias = ' '
lnTotPer   = 0
lnTotCr    = 0
lnTotDb    = 0

SELECT(lcTmpAlo)
GO RECNO()
INSERT INTO &gcWorkDir.&lcTmpDtl (cAcctCode) ;
       VALUES (&lcTmpAlo..cActCode)

SELECT GLAUTDT

IF &lcTmpAlo..cAutBase='A'
  SCAN REST WHILE cAutType+cAutCode = 'A'+&lcTmpAlo..cAutCode
    SCATTER MEMVAR 
    INSERT INTO &gcWorkDir.&lcTmpDtl FROM MEMVAR
    REPLACE &lcTmpDtl..NCREDIT WITH IIF(cdrorcr='C',;
            STR(namount,15,2),"           0.00"),;
            &lcTmpDtl..NDEBIT  WITH IIF(cdrorcr='D',;
            STR(namount,15,2),"           0.00")
    lnTotCr  = lnTotCr  +IIF(cdrorcr='C',namount,0)
    lnTotDb  = lnTotDb  +IIF(cdrorcr='D',namount,0) 
  ENDSCAN
  lcSavAlias = ALIAS()
  lnBalanc   = ABS(lnTotDb-lnTotCr)
  SELECT (lcTmpDtl)
  GO TOP
  REPLACE &lcTmpDtl..NCREDIT WITH IIF(lnTotDb > lnTotCr,;
          STR(lnBalanc,15,2),"           0.00"),;
          &lcTmpDtl..NDEBIT  WITH IIF(lnTotDb < lnTotCr,;
          STR(lnBalanc,15,2),"           0.00")
  SELECT (lcSavAlias)
  IF lnTotDb > lnTotCr
    lnTotCr = lnTotCr + lnBalanc
  ELSE
    lnTotDb = lnTotDb + lnBalanc
  ENDIF
ELSE
  SCAN REST WHILE cAutType+cAutCode = 'A'+&lcTmpAlo..cAutCode
    SCATTER MEMVAR 
    INSERT INTO &gcWorkDir.&lcTmpDtl FROM MEMVAR
    DO CASE
      CASE SIGN(&lcTmpAlo..nNetAct) = 0  
        REPLACE &lcTmpDtl..NCREDIT WITH "           0.00" ,;
                &lcTmpDtl..NDEBIT  WITH "           0.00"

      CASE SIGN(&lcTmpAlo..nNetAct) > 0  
        REPLACE &lcTmpDtl..NCREDIT WITH "           0.00" ,;
                &lcTmpDtl..NDEBIT  WITH ;
                STR(GLAUTDT.namount * ABS(&lcTmpAlo..nNetAct)/100,15,2)

      CASE SIGN(&lcTmpAlo..nNetAct) < 0  
        REPLACE &lcTmpDtl..NCREDIT WITH ;
                STR(GLAUTDT.namount * ABS(&lcTmpAlo..nNetAct)/100,15,2),;
                &lcTmpDtl..NDEBIT  WITH "           0.00"
    ENDCASE
    lnTotPer = lnTotPer + namount 
  ENDSCAN
  lcSavAlias = ALIAS()
  SELECT (lcTmpDtl)
  GO TOP
  REPLACE &lcTmpDtl..NCREDIT WITH IIF(SIGN(&lcTmpAlo..nNetAct) > 0 ,;
          STR(lnTotPer * ABS(&lcTmpAlo..nNetAct)/100,15,2),"           0.00"),;
          &lcTmpDtl..NDEBIT  WITH IIF(SIGN(&lcTmpAlo..nNetAct) < 0 ,;
          STR(lnTotPer * ABS(&lcTmpAlo..nNetAct)/100,15,2),"           0.00")
  SELECT (lcSavAlias)
  lnTotCr  = lnTotPer * ABS(&lcTmpAlo..nNetAct)/100
  lnTotDb  = lnTotPer * ABS(&lcTmpAlo..nNetAct)/100
ENDIF
          
SHOW GET lsDetail 
SHOW GET lnTotCr  
SHOW GET lnTotDb  

SELECT(lcTmpAlo)

*!**************************************************************************
*!
*!      Function: lfvUpDate
*!
*!**************************************************************************
*
FUNCTION lfvUpDate

SELECT(lcTmpAlo)
COUNT FOR !EMPTY(cInclude) TO lnTotRec

GO 1

SCAN FOR !EMPTY(cInclude)
  REPLACE GLAUTHD.NAUTFRQNO WITH GLAUTHD.NAUTFRQNO + 1 
  SELECT(lcTmpAlo)
ENDSCAN

lnCurRec = 0

IF lnTotRec = 0
  ** No ð selected for this criteria. **
  =gfModalGen("TRM02095B00000","DIALOG",lcTAllocat)
  _CUROBJ = OBJNUM(lsLine)
  SHOW GET lsLine
  RETURN
ENDIF

lcUserName = LOOKUP(SYUUSER.cUsr_Name,gcUser_ID,SYUUSER.cUser_ID,'cUser_ID')

LOCATE FOR !EMPTY(cInclude) .AND. cStandard = 'Y'
IF FOUND()
  lcStandard = 'Y'
  =lfUpdTmp()
ENDIF  

LOCATE FOR !EMPTY(cInclude) .AND. cStandard = 'N'
IF FOUND()
  lcStandard = 'N'
  =lfUpdTmp()
ENDIF  

IF gfFLOCK("GLBATCH,GLTRNSHD,GLTRNSDT",.T.)

  *** Update master batch/transactions header/transaction details
  SET ORDER TO TAG BATCHNO  IN &lcTmpBatch
  SET ORDER TO TAG BATCHTRN IN &lcTmpTrnHd
  SET ORDER TO TAG BATCHTRN IN &lcTmpTrnDt

  SELECT(lcTmpBatch)
  SET RELATION TO

  SELECT(lcTmpTrnHd)
  SET RELATION TO
  SET RELATION TO &lcTmpTrnHd..cbatchno INTO &lcTmpBatch ADDITIVE

  SELECT(lcTmpTrnDt)
  SET RELATION TO &lcTmpTrnDt..cbatchno+&lcTmpTrnDt..ctranno INTO &lcTmpTrnHd ADDITIVE

  *** Set variables for the Thermometer
  lnCurtrns  = 0
  lnTotTrns  = RECCOUNT()

  *** Variable to compair if there is a change in the batch No.
  lcBatchKey = ' '

  *** Variable to compair if there is a change in the Transaction No.
  lcTrnNoKey = ' '

  *** Variables to sum the total debit & credit per Batch
  lnTotBatDr = 0
  lnTotBatCr = 0

  *** Variables to sum the total debit & credit per Transaction
  lnTotTrnDr = 0
  lnTotTrnCr = 0

  SCAN
    *** Assign the thermometer variable with the Record no.
    lnCurtrns  = lnCurtrns + 1
    =gfThermo(lnTotTrns,lnCurtrns,lcTUpdMast,&lcTmpTrnHd..CAUTCODE)

    *** Compair if the batch no = the old batch or not. If not
    *** we are going to creat a new batch no.
    IF lcBatchKey <> cBatchNo

      lcBatchKey = cBatchNo

      ** Creating New batch No.

      *E300663,1 Change this line for the changes we have 
      *          made to (gfSequence) [Begin]
      *lcBatchNo = PADL(gfSequence("BATCH",1,gcAct_Comp),6,'0')
      lcBatchNo = gfSequence('CBATCHNO')
      *E300663,1 Change this line [End]
      
      SELECT(lcTmpBatch)
      SCATTER MEMVAR MEMO
      m.cBatchNo = lcBatchNo
      m.nBatCnTot= 0
      m.nBatotDr = 0
      m.nBatotCr = 0

      SELECT GLBATCH
      APPEND BLANK
      GATHER MEMVAR MEMO
      =gfAdd_Info('GLBATCH')

      SELECT(lcTmpTrnDt)
      lnTotBatDr = 0
      lnTotBatCr = 0

    ENDIF

    *** Compair if the batch no = the old transaction or not.  If not
    *** we are going to creat a new transaction no.
    IF lcTrnNoKey <> cTranNo

      lcTrnNoKey = cTranNo

      *** Create New Transaction No.

      *E300663,1 Change this line for the changes we have 
      *          made to (gfSequence) [Begin]
      *lcTranNo = PADL(gfSequence("TRANSACT",1,gcAct_Comp),8,'0')
      lcTranNo = gfSequence('CTRANNO')
      *E300663,1 Change this line [End]
      
      SELECT(lcTmpTrnHd)
      SCATTER MEMVAR MEMO
      m.cBatchNo = lcBatchNo
      m.cTranNo  = lcTranNo
      m.nTrnTotDr= 0
      m.nTrnTotCr= 0

      SELECT GLTRNSHD
      APPEND BLANK
      GATHER MEMVAR MEMO
      =gfAdd_Info('GLTRNSHD')
 
      SELECT(lcTmpTrnDt)
      lnTotTrnDr = 0
      lnTotTrnCr = 0
    ENDIF

    SELECT(lcTmpTrnDt)
    SCATTER MEMVAR MEMO
    m.cBatchNo = lcBatchNo
    m.cTranNo  = lcTranNo

    IF &lcTmpTrnDt..cDrorcr = 'D'
      lnTotTrnDr = lnTotTrnDr+nAmount
      lnTotBatDr = lnTotBatDr+nAmount
    ELSE
      lnTotTrnCr = lnTotTrnCr+nAmount
      lnTotBatCr = lnTotBatCr+nAmount
    ENDIF

    SELECT GLTRNSDT
    APPEND BLANK
    GATHER MEMVAR MEMO
    =gfAdd_Info('GLTRNSDT')

    SELECT GLTRNSHD
    REPLACE nTrnTotDr WITH lnTotTrnDr,;
            nTrnTotCr WITH lnTotTrnCr

    SELECT GLBATCH 
    REPLACE nBatCnTot WITH lnTotBatDr,;
            nBatotDr  WITH lnTotBatDr,;
            nBatotCr  WITH lnTotBatCr 

    SELECT(lcTmpTrnDt)

  ENDSCAN

  SELECT GLTRNSHD
  REPLACE nTrnTotDr WITH lnTotTrnDr,;
          nTrnTotCr WITH lnTotTrnCr

  SELECT GLBATCH
  REPLACE nBatCnTot WITH lnTotBatDr,;
          nBatotDr  WITH lnTotBatDr,;
          nBatotCr  WITH lnTotBatCr 

  IF lnCurtrns < lnTotTrns
    FOR lnCounter = lnCurtrns TO lnTotTrns
      =gfThermo(lnTotTrns,lnCurtrns,lcTUpdMast,"")
    ENDFOR
  ENDIF

  *** Set the relation off ***
  SELECT(lcTmpBatch)
  SET RELATION TO

  SELECT(lcTmpTrnHD)
  SET RELATION TO

  SELECT(lcTmpTrnDt)
  SET RELATION TO

  *** Unlock the locked file
  =gfFLOCK("GLBATCH,GLTRNSHD,GLTRNSDT",.F.)
ELSE 
  =gfFLOCK("GLBATCH,GLTRNSHD,GLTRNSDT",.F.)
ENDIF  

SELECT (lcTmpBatch)
ZAP

SELECT (lcTmpTrnHd)
ZAP

SELECT (lcTmpTrnDt)
ZAP

SELECT(lcTmpAlo)
lsLine = 1
SHOW GET lsLine 
ZAP
=lfRefresh()   && Refresh the say fields.
SHOW GETS WINDOW (gcBaseWind)
    
*** Zap the Temp. file that hold the details information. ***    
SELECT (lcTmpDtl)
lsDetail = 1
SHOW GET lsDetail
ZAP
*** Set total debit & total credit variables to zero. ***
lnTotCr  = 0
lnTotDb  = 0
lnTgtRec = 0

SHOW GETS WINDOW CWDGLGALO0

lcGenStat  = 'ENABLE'
lcObjStat  = 'DISABLE'

SHOW GET lsDetail 
SHOW GET lnTotCr 
SHOW GET lnTotDb
SHOW GET pbGenerate ENABLE
SHOW GET lsLine 
SHOW GET pbUpdate   DISABLE
SHOW GET pbAll      DISABLE
SHOW GET pbNone     DISABLE
SHOW GET pbInvert   DISABLE
SHOW GET pbView     DISABLE
SHOW GET pbStatus   DISABLE

SHOW GET pbSelect,1 PROMPT lcTSelect DISABLE
lcSelect = lcTSelect

*** Show the view screen that has the list to clean ***
*** it from the previous data...

IF WVISIBLE('CWDGLGALO0') 
  =gfChClose('CWDGLGALO0')
ENDIF

_CUROBJ=OBJNUM(ldGenDate)
SHOW GET ldGenDate ENABLE

SELECT(lcTmpAlo)

*!**************************************************************************
*!
*!      Function: lfUpdTmp
*!
*!**************************************************************************
*
FUNCTION lfUpdTmp

lnTotBatDb = 0    && Variable to hold the total batch debit
lnTotBatCr = 0    && Variable to hold the total batch Credit
lnTotTrnDb = 0    && Variable to hold the total Transaction debit
lnTotTrnCr = 0    && Variable to hold the total Transaction Credit
lnBalanc   = 0
lnTotPer   = 0
lnRecNo    = 0
lnSavRec   = 0
lnBatchNo  = 0    && Variable to hold the batch no.
lnTranNo   = 0    && Variable to hold the transaction no.

** Create New Batch No. **
lnBatchNo = lnBatchNo + 1

*** Add new record in the temp. batch file. ***
INSERT INTO &gcWorkDir.&lcTmpBatch ; 
       (CBATCHNO,CBATSTAT,CBATTYPE,CBATPYR,DBATPBEG,;
        DBATPEND,CBATREFER,CBATDESC,CSRCMODUL,CCOMP_ID) ;
VALUES (PADL(lnBatchNo,FSIZE('CBATCHNO'),'0'),'U',;
        IIF(lcStandard = 'Y','N','S'),lcYear,;
        ldPrBegin,ldPrEnd,lcTDefTxt1,;
        lcTDefTxt3+' '+lcUserName,gcAct_Appl,gcAct_Comp)

SELECT (lcTmpBatch)
=gfAdd_Info()

SELECT(lcTmpAlo)

SCAN FOR !EMPTY(cInclude) .AND. &lcTmpAlo..CSTANDARD = lcStandard
  lnCurRec = lnCurRec + 1
  =gfThermo(lnTotRec,lnCurRec,lcTGenAllo,CAUTCODE)
  lnTotTrnDb = 0
  lnTotTrnCr = 0
  lnTotPer   = 0
  ** Create Transaction No. **
  lnTranNo = lnTranNo + 1
  INSERT INTO &gcWorkDir.&lcTmpTrnHd ;
         (CBATCHNO,CTRANNO,CTRNDESC,CTRNREFER,;
          DTRNPDATE,CTRNPYR,CTRNPPRD,CTRNSTAT,;
          CTRNTYPE,CSRCMODUL,CTRNREVER,CCOMP_ID,;
          CAUTTYPE,CAUTCODE,CSTANDARD,CSRCJRNL);
  VALUES (PADL(lnBatchNo,FSIZE('CBATCHNO'),'0'),;
          PADL(lnTranNo,FSIZE('CTRANNO'),'0'),;
          lcTDefTxt2+' '+&lcTmpAlo..cAutcode,;
          lcTDefTxt4+' '+;
          IIF(SET('CENTURY')='ON',DTOC(gdSysDate),LEFT(DTOC(gdSysDate),6)+STR(YEAR(gdSysDate),4)),;
          ldGenDate,lcYear,;
          PADL(lnGenPrd,2,'0'),;
          'U','A',gcAct_Appl,'N',gcAct_Comp,'A',;
          &lcTmpAlo..cAutCode,GLACCHAR.CSTANDARD,;
          &lcTmpAlo..CSRCJRNL)
  SELECT (lcTmpTrnHd)
  =gfAdd_Info()

  SELECT(lcTmpAlo)
  GO RECNO()

  SELECT GLAUTDT
  IF &lcTmpAlo..cAutBase='A'
    SCAN REST WHILE cAutType+cAutCode = 'A'+&lcTmpAlo..cAutCode
      INSERT INTO &gcWorkDir.&lcTmpTrnDt;
             (CBATCHNO,CTRANNO,CACCTCODE,CDRORCR,NAMOUNT,;
              DTRNPDATE,CTRNPYR,CTRNPPRD);
      VALUES (PADL(lnBatchNo,FSIZE('CBATCHNO'),'0'),;
              PADL(lnTranNo,FSIZE('CTRANNO'),'0'),;
              GLAUTDT.CACCTCODE,GLAUTDT.CDRORCR,;
              GLAUTDT.NAMOUNT,ldGenDate,lcYear,;
              PADL(lnGenPrd,2,'0'))
     SELECT (lcTmpTrnDt)
     =gfAdd_Info()
     SELECT GLAUTDT
      *** Calculate all the debits & credits amounts ***
      *** for this group of details... 
      lnTotTrnCr = lnTotTrnCr + IIF(&lcTmpTrnDt..cdrorcr='C',GLAUTDT.NAMOUNT,0)
      lnTotTrnDb = lnTotTrnDb + IIF(&lcTmpTrnDt..cdrorcr='D',GLAUTDT.NAMOUNT,0) 
    ENDSCAN
    lnBalanc   = ABS(lnTotTrnDb-lnTotTrnCr)
    INSERT INTO &gcWorkDir.&lcTmpTrnDt;
                (CBATCHNO,CTRANNO,CACCTCODE,CDRORCR,NAMOUNT,;
                DTRNPDATE,CTRNPYR,CTRNPPRD);
    VALUES (PADL(lnBatchNo,FSIZE('CBATCHNO'),'0'),;
            PADL(lnTranNo,FSIZE('CTRANNO'),'0'),;
            &lcTmpAlo..CACTCODE,;
            IIF(lnTotTrnDb > lnTotTrnCr,'C','D'),;
            lnBalanc,ldGenDate,lcYear,;
            PADL(lnGenPrd,2,'0'))
    
    SELECT (lcTmpTrnDt)
    =gfAdd_Info()
    SELECT GLAUTDT
    
    lnTotTrnCr = lnTotTrnCr + IIF(&lcTmpTrnDt..cdrorcr='C',lnBalanc,0)
    lnTotTrnDb = lnTotTrnDb + IIF(&lcTmpTrnDt..cdrorcr='D',lnBalanc,0) 
  ELSE
    SCAN REST WHILE cAutType + cAutCode = 'A'+&lcTmpAlo..cAutCode
      INSERT INTO &gcWorkDir.&lcTmpTrnDt;
             (CBATCHNO,CTRANNO,CACCTCODE,CDRORCR,NAMOUNT,;
              DTRNPDATE,CTRNPYR,CTRNPPRD);
      VALUES (PADL(lnBatchNo,FSIZE('CBATCHNO'),'0'),;
              PADL(lnTranNo,FSIZE('CTRANNO'),'0'),;
              GLAUTDT.CACCTCODE,' ',0,ldGenDate,lcYear,;
              PADL(lnGenPrd,2,'0'))
    
      SELECT (lcTmpTrnDt)
      =gfAdd_Info()

      SELECT GLAUTDT
    
      DO CASE
        CASE SIGN(&lcTmpAlo..nNetAct) = 0  
          REPLACE &lcTmpTrnDt..CDRORCR WITH ' ',;
                  &lcTmpTrnDt..NAMOUNT WITH 0.00
      
        CASE SIGN(&lcTmpAlo..nNetAct) > 0  
          REPLACE &lcTmpTrnDt..CDRORCR WITH 'D',;
                  &lcTmpTrnDt..NAMOUNT WITH ;
                  (GLAUTDT.NAMOUNT * ABS(&lcTmpAlo..nNetAct)/100)

        CASE SIGN(&lcTmpAlo..nNetAct) < 0  
          REPLACE &lcTmpTrnDt..CDRORCR WITH 'C',;
                  &lcTmpTrnDt..NAMOUNT WITH ;
                  (GLAUTDT.NAMOUNT * ABS(&lcTmpAlo..nNetAct)/100)
      ENDCASE
      lnTotPer = lnTotPer + NAMOUNT 
    ENDSCAN
    INSERT INTO &gcWorkDir.&lcTmpTrnDt;
                (CBATCHNO,CTRANNO,CACCTCODE,CDRORCR,NAMOUNT,;
                DTRNPDATE,CTRNPYR,CTRNPPRD);
    VALUES (PADL(lnBatchNo,FSIZE('CBATCHNO'),'0'),;
            PADL(lnTranNo,FSIZE('CTRANNO'),'0'),;
            &lcTmpAlo..CACTCODE,;
            IIF(SIGN(&lcTmpAlo..nNetAct) < 0 ,'D','C'),;
            (lnTotPer * ABS(&lcTmpAlo..nNetAct)/100),;
            ldGenDate,lcYear,PADL(lnGenPrd,2,'0'))

    SELECT (lcTmpTrnDt)
    =gfAdd_Info()

    SELECT GLAUTDT
    *** Calculate all the debits & credits amounts ***
    *** for this group of details... 
    lnTotTrnCr = lnTotPer * ABS(&lcTmpAlo..nNetAct)/100
    lnTotTrnDb = lnTotPer * ABS(&lcTmpAlo..nNetAct)/100
  ENDIF

  SELECT (lcTmpTrnHd)
  REPLACE NTRNTOTDR WITH lnTotTrnDb;
          NTRNTOTCR WITH lnTotTrnCr
  
  *** Calculate all the debit & credit amounts for the ***
  *** group of transaction that have the same period & ***
  *** including in the selecting  records that  has to ***
  *** be update...
  lnTotBatDb = lnTotBatDb + lnTotTrnDb
  lnTotBatCr = lnTotBatCr + lnTotTrnCr

  SELECT(lcTmpAlo) 

ENDSCAN

SELECT (lcTmpBatch)
REPLACE NBATCNTOT WITH lnTotBatDb;
        NBATOTDR  WITH lnTotBatDb; 
        NBATOTCR  WITH lnTotBatCr
IF lnTotRec > lnCurRec
  FOR lnThCount = lnCurRec TO lnTotRec
    =gfThermo(lnTotRec,lnThCount,lcTGenAllo,"")
  ENDFOR
ENDIF
SELECT(lcTmpAlo) 

*!**************************************************************************
*!
*!      Function: lfBatStat
*!
*!**************************************************************************
*
FUNCTION lfBatStat

DO CASE
  CASE &lcTmpAlo..cGnBatSt = 'U'
       RETURN('Unposted')
  CASE &lcTmpAlo..cGnBatSt = 'O'
       RETURN('Out of Balance')
  CASE &lcTmpAlo..cGnBatSt = 'D'
       RETURN('Deleted')
  CASE &lcTmpAlo..cGnBatSt = 'P'
       RETURN('Posted')
  CASE &lcTmpAlo..cGnBatSt = 'H'
       RETURN('Hold')
  OTHERWISE
       RETURN(' ')     
ENDCASE       

*!**************************************************************************
*!
*!      Function: lfUpdPrmpt
*!
*!**************************************************************************
*
FUNCTION lfUpdPrmpt

IF EMPTY(&lcTmpAlo..cInclude)
  SHOW GET pbSelect,1 PROMPT lcTSelect 
  lcSelect = lcTSelect
ELSE
  SHOW GET pbSelect,1 PROMPT lcTUnSelct 
  lcSelect = lcTUnSelct
ENDIF

*!**************************************************************************
*!
*!      Function: lfReGenrat
*!
*!**************************************************************************
*
FUNCTION lfReGenrat

*** If change the generating date with valid date, ***
*** zap the previous data from the temp. file...   
SELECT(lcTmpAlo)
lsLine = 1
SHOW GET lsLine 
ZAP
SHOW GETS WINDOW (gcBaseWind)
=lfRefresh()   && Refresh the say fields.
    
*** Zap the Temp. file that hold the details information. ***    
SELECT (lcTmpDtl)
lsDetail = 1
SHOW GET lsDetail 
ZAP
SHOW GETS WINDOW CWDGLGALO0
          
*** Set total debit & total credit variables to zero. ***
lnTotCr   = 0  
lnTotDb   = 0
          
lcGenStat  = 'ENABLE'
lcObjStat  = 'DISABLE'

** Set the variable to allow new selection **

SHOW GET lsDetail 
SHOW GET lsLine   
SHOW GET lnTotCr 
SHOW GET lnTotDb
SHOW GET pbGenerate ENABLE
SHOW GET pbUpdate   DISABLE
SHOW GET pbSelect   DISABLE
SHOW GET pbAll      DISABLE
SHOW GET pbNone     DISABLE
SHOW GET pbInvert   DISABLE
SHOW GET pbView     DISABLE
SHOW GET pbStatus   DISABLE

SELECT(lcTmpAlo)

