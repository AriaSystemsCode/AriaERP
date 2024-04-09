*:************************************************************************
*:
*: Procedure file: GLTRABAL.RPR
*:               : Statment of cash flow
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Albert Raif
*:      Copyright (c) 
*:  Last modified:10/07/94
*:
*:  Procs & Fncts: 
*:                lfChngCond    
*:                
*:
*:      Documented   /  /
*:************************************************************************
*
*B600346,1 Reham On 05/29/95 Spelling mistakes.
*B600356,1 Reham On 05/30/95 The posting year should not accept 199 as valid value.

*B600361,1 Reham On 05/31/95 Validate the period if the year is empty
*B600361,1 to prevent make it accept anything.
*B605850,1 ASH 04/11/2002 Fix the bug of wrong browsing the periods.
*:************************************************************************

*** Report Setup

IF (EMPTY(ALLTRIM(laOGFxFlt[1,6])) .OR. EMPTY(ALLTRIM(laOGFxFlt[2,6])))
  ** NO recoeds hove been collected
  =gfModalGen("INM00052B00000","DIALOG")
  RETURN
ENDIF
             
&& Get slected files name 
lcRpFiles = "GLCFITEM,GLACCHAR,GLACBALS"
***   Get the field order ****

lnOldAlias = SELECT()

IF llOGFltCh OR glEscPrs
  *** Save escape setting
  lcSaveEsc = SET('ESCAP')
  lcSaveOnEs = ON('ESCAPE')
  SET ESCAP ON
  ON ESCAP DO gpSQLBrak
        
  *** Intialize the varliable that count rows selected
  _TALLY = 0
        
  *** Activate the system select therom.
  SET TALK ON
 
  WAIT 'Collecting data...' WINDOW NOWAIT
  
  *** Create select  statment and
  *** Select data from file(s)
  SELECT  &lcRpFields.;
   FROM  &lcRpFiles. ;
   WHERE  &lcRpExp.  .AND. lfWaitMsg();
   &lcRpOrder.;
   INTO CURSOR (lcRpTargt)
    
  *** Restore all enviroment 
  WAIT CLEAR
  SET TALK OFF
  ON ESCAPE  &lcSaveOnEs
  SET ESCAPE &lcSaveEsc
  
  *** Display the report to screen , file or printer
  *** and check if there is any record or not
  IF _TALLY = 0        && No records collected
    ** NO recoeds hove been collected
    =gfModalGen("INM00052B00000","DIALOG")
  ELSE
    STORE 0 TO lnTotoBal,lnTotcBal
    lnNetInc=lfGetNet()
    =lfGetCBal()
    DO gfDispRe WITH EVAL('lcRpForm')
  ENDIF
ELSE
  STORE 0 TO lnTotoBal,lnTotcBal
  lnNetInc=lfGetNet()
  =lfGetCBal()
  SELECT (lcRpTargt)
  DO gfDispRe WITH EVAL('lcRpForm')
ENDIF 
SELECT (lnOldAlias)    

glEscPrs = .F.

*!************************************************************************
*!
*!      FUNCTION lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep

IF USED(lcRpTargt)
  USE IN ALIAS(lcRpTargt)
ENDIF

glEscPrs = .F.

*!************************************************************************
*!
*!      Function lfvFisYer
*!
*!************************************************************************
*
****  Check if current company has this entried year or not
FUNCTION lfvFisYer

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",DFisBgDat:H="Begin date",DFisEnDat:H="End date"'
laRpRetFld[1] = ''

&& Check If year field is empty
*B600356,1 Move the alltrim from the year value to seek right.
*lcRpFicsYr = ALLTRIM(EVALUATE(SYS(18)))
lcRpFicsYr = EVALUATE(SYS(18))

lcRpCurFld = VARREAD()
IF .NOT.EMPTY(lcRpFicsYr)  
  lcOldAlias = SELECT()    && Save the current alias
  llUesdBefo = .F.        && Check if used before or this the first time
  
  *HAYTHAR Change these lines to Use FISHD instead of SYCFISHD [Begin]
  
  *IF NOT USED("SYCFISHD") 
  *  SELECT 0
  *  USE &gcSysHome.SYCFISHD ORDER TAG compfyear
  *  llUesdBefo = .T.
  *ENDIF
  *SELECT SYCFISHD
  IF NOT USED("FISHD") 
    SELECT 0
    USE &gcDataDir.FISHD ORDER TAG compfyear
    llUesdBefo = .T.
  ENDIF
  SELECT FISHD
  
  *HAYTHAR Change these lines to Use FISHD instead of SYCFISHD [End]
  
  *** Search for the current company+year
  *B605850,1 ASH 04/11/2002 (Begin) Remove the company code from the seek expr.
  *IF ('?' $ &lcRpCurFld. .OR. !SEEK(gcPrnt_Cmp+lcRpFicsYr)) 
  IF ('?' $ &lcRpCurFld. .OR. !SEEK(lcRpFicsYr)) 
  *B605850,1 ASH 04/11/2002 (End)
    =gfBrows('','CFisFyear',"laRpRetFld",'Fiscal year ',.F.)
    &lcRpCurFld. = laRpRetFld[1]
    SHOW GET (lcRpCurFld)
  ENDIF
  
  IF llUesdBefo       && .F.- this file used by the system
    
    *HAYTHAR Change this line to Use FISHD instead of SYCFISHD [Begin]
    *USE IN SYCFISHD
    USE IN FISHD
    *HAYTHAR Change this line to Use FISHD instead of SYCFISHD [End]
    
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN 

*!************************************************************************
*!
*!      Function lfvFisPrd
*!
*!************************************************************************
*
****  Check if current company has this entried period or not
FUNCTION lfvFisPrd

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = [CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"]
laRpRetFld[1] = ''

lcRpCurFld      = VARREAD()
lcRpFicsYr      = laOGFxFlt[1,6]
&& Check If year field is empty
IF .NOT. EMPTY(lcRpFicsYr) 
  IF .NOT.EMPTY(ALLTRIM(&lcRpCurFld.))  
    lcOldAlias = SELECT()    && Save the current alias
    llUesdBefo = .F.        && Check if used before or this the first time
    
    *HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [Begin]
    
    *IF NOT USED("SYCFSPRD") 
    *  SELECT 0
    *  USE &gcSysHome.SYCFSPRD ORDER TAG comfyrprdi
    *  llUesdBefo = .T.
    *ENDIF
    *SELECT SYCFSPRD
    IF NOT USED("FSPRD") 
      SELECT 0
      USE &gcDataDir.FSPRD ORDER TAG comfyrprdi
      llUesdBefo = .T.
    ENDIF
    SELECT FSPRD
    
    *HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [End]
    
    *** Search for the current company+year+Prd
    *B605850,1 ASH 04/11/2002 (Begin) Remove the company code from the seek expr.
    *IF ('?' $ &lcRpCurFld. .OR.;  
      !SEEK(gcAct_comp+ALLTRIM(lcRpFicsYr)+ALLTRIM(&lcRpCurFld.)))
        *=gfBrows([gcAct_comp+ALLTRIM(lcRpFicsYr)],'CFsppRdid',"laRpRetFld","Fiscal period",.F.)
    IF ('?' $ &lcRpCurFld. .OR.;  
      !SEEK(ALLTRIM(lcRpFicsYr)+ALLTRIM(&lcRpCurFld.)))
        =gfBrows([ALLTRIM(lcRpFicsYr)],'CFsppRdid',"laRpRetFld","Fiscal period",.F.)
    *B605850,1 ASH 04/11/2002 (End)
      &lcRpCurFld = laRpRetFld[1]
      SHOW GET (lcRpCurFld)
    ENDIF
    IF llUesdBefo       && .F.- this file used by the system
      
      *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [Begin]
      *USE IN SYCFSPRD
      USE IN FSPRD
      *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [End]
      
    ENDIF
    SELECT (lcOldAlias)
  ENDIF
ELSE
  *B600361,1 Reham On 05/31/95 Validate the period if the year is empty
  *B600361,1 to prevent make it accept anything.
  &lcRpCurFld = "  "
  SHOW GET (lcRpCurFld)
ENDIF
RETURN 

*!************************************************************************
*!
*!      FUNCTION lfGetNet
*!
*!************************************************************************
*
FUNCTION lfGetNet

lcOldAlias = SELECT()
lnRecNo = RECNO()
DIMENSION laResult(1)
laResult[1] = 0
SELECT SUM(IIF(lcRpRepFor='PTD',NACBPTDCR-NACBPTDDR,NACBPTDCR-NACBPTDDR-NACBOPBAL));
  FROM GLACCHAR,GLACBALS;
  WHERE GLACBALS.CACCTCODE+GLACBALS.CFISFYEAR+GLACBALS.CFSPPRDID =;
        GLACCHAR.CACCTCODE+laOGFxFlt[1,6]+laOGFxFlt[2,6];
        AND LEFT(GLACCHAR.CTYPECODE,1) $ 'SETIC';
  INTO ARRAY laResult


SELECT(lcOldAlias)
IF !EMPTY(ALIAS(lcOldAlias))
  GO lnRecNo
ENDIF  
RETURN laResult[1]

*!************************************************************************
*!
*!      FUNCTION lfGetCBal
*!
*!************************************************************************
*
FUNCTION lfGetCBal

lcOldAlias = SELECT()
lnRecNo = RECNO()
DIMENSION laResult[1,2]
STORE 0 TO laResult
IF lcRpRepFor='PTD' 
  SELECT SUM(GLACBALS.NACBOPBAL),;
         SUM(GLACBALS.NACBOPBAL+GLACBALS.NACBPTDDR-GLACBALS.NACBPTDCR);
     FROM &gcDataDir.GLACBALS,&gcDataDir.GLACCHAR;
     WHERE GLACCHAR.CSEGCAFLO='C00' AND;
           GLACBALS.CACCTCODE+GLACBALS.CFISFYEAR+GLACBALS.CFSPPRDID=;
           GLACCHAR.CACCTCODE+laOGFxFlt[1,6]+laOGFxFlt[2,6];
           INTO ARRAY laResult
           

  lnTotoBal=laResult[1,1]
  lnTotcBal=laResult[1,2]

ELSE

  SELECT SUM(GLACBALS.NACBOPBAL);
     FROM &gcDataDir.GLACBALS,&gcDataDir.GLACCHAR;
     WHERE GLACCHAR.CSEGCAFLO='C00' AND;
           GLACBALS.CACCTCODE+GLACBALS.CFISFYEAR+GLACBALS.CFSPPRDID=;
           GLACCHAR.CACCTCODE+laOGFxFlt[1,6]+'01';
           INTO ARRAY laResult

  lnTotoBal=laResult[1]

  SELECT SUM(GLACBALS.NACBOPBAL+GLACBALS.NACBPTDDR-GLACBALS.NACBPTDCR);
     FROM &gcDataDir.GLACBALS,&gcDataDir.GLACCHAR;
     WHERE GLACCHAR.CSEGCAFLO='C00' AND;
           GLACBALS.CACCTCODE+GLACBALS.CFISFYEAR+GLACBALS.CFSPPRDID=;
           GLACCHAR.CACCTCODE+laOGFxFlt[1,6]+laOGFxFlt[2,6];
           INTO ARRAY laResult
           
           
  lnTotcBal=laResult[1]
           
ENDIF         
SELECT(lcOldAlias)
IF !EMPTY(ALIAS(lcOldAlias))
 GO lnRecNo
ENDIF 
RETURN 
