*:************************************************************************
*:
*: Procedure file: GLPBATRN.RPR
*:               : Posted transactions
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Albert Raif
*:      Copyright (c) 
*:  Last modified:25/04/94
*:
*:  Procs & Fncts: 
*:                lfChngCond    
*:                
*:
*:      Documented   /  /
*:************************************************************************
*
*B600338,1 Reham On 05/28/95 Increase the date width to 10 ch.
*B600339,1 Reham On 05/28/95 "Added By" new object in Option Grid
*B600344,1 Reham On 05/28/95 Fix the upper case for : S.J.
*B600346,1 Reham On 05/29/95 Spelling mistakes.

*B600354,1 Reham On 05/29/95 Fix the displaying of the trans. type in the 
*B600354,1 Reham On 05/29/95 report layout {automatic trans. printed 
*B600354,1 Reham On 05/29/95 as Approved trans.}
*B600356,1 Reham On 05/30/95 The posting year should not accept 199 as valid value.

*B600361,1 Reham On 05/31/95 Validate the period if the year is empty
*B600361,1 to prevent make it accept anything.
*E300789,7 WALID 03/04/99 Make all needed modification due to the 
*          removing of ccomp_id field .
*E300789,6 MAB Adjust order of some files.
*B602244,1 KAM 09/07/2000 right justify for debit,credit title,also justify
*B602244,1                the header of the report in GLPOTRAD.FRX,GLPOTRAS.FRX
*B605683,1 SSE 03/25/2002 Fix bug of not printing entries that have same amount and account no.
*B607055,1 ABD 03/19/2003 Variable "CACCTCODE " Not Found.
***************************************************************************
*** Report Setup

DO CASE
  CASE lcRpForm = "GLPOTRAS"
    lcRpFiles   = "GLPTRNHD"  && Get slected files name 
    lnTotal     =  RECCOUNT('GLPTRNHD')
  CASE lcRpForm = "GLPOTRAD"
    lnTotal     =  RECCOUNT('GLPTRNDT')
*    lcRpFiles   = "GLPTRNHD,GLPTRNDT,GLACCHAR"  && Get slected files name 
     lcRpFiles   = "GLPTRNHD,GLPTRNDT"  && Get slected files name 
ENDCASE   

*B607055,1 ABD - assign all field again to handel Variable "CACCTCODE " Not Found. [Begin]
lcrpFields = GetRpField()
*B607055,1 ABD - [End]

***   Get the field order ****

lnCount    = 0
lcPrOrder  = ''
lnOldAlias = SELECT()     
lcPrOrder = IIF(lcRpGroup='lcRpDum',lcRpOrder,;
                'ORDER BY '+lcRpGroup+;
                IIF(EMPTY(ALLTRIM(lcRpOrder)),'',',')+STRTRAN(lcRpOrder,'ORDER BY',''))
                
***   Create select  statment
IF llOGFltCh .OR. (lcRpOldOrd <> lcPrOrder) .OR. glEscPrs

  lcRpOldOrd = lcPrOrder
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
       
  *** Select data from file(s)
  
  *B605683,1 Remove the distinct. [Begin]
  *SELECT DISTINCT &lcRpFields;
  *  FROM  &lcRpFiles ;
  *  WHERE  &lcRpExp  .AND. lfWaitMsg();
  *  &lcPrOrder.;
  *  INTO CURSOR (lcRpTargt)
  IF lcRpForm = "GLPOTRAD"
    SELECT &lcRpFields;
    FROM  &lcRpFiles ;
    WHERE  &lcRpExp  .AND. lfWaitMsg();
    &lcPrOrder.;
    INTO CURSOR (lcRpTargt)
  ELSE
    SELECT DISTINCT &lcRpFields;
    FROM  &lcRpFiles ;
    WHERE  &lcRpExp  .AND. lfWaitMsg();
    &lcPrOrder.;
    INTO CURSOR (lcRpTargt)
  ENDIF
  *B605683,1 Remove the distinct. [End]
    
    IF lcRpForm = "GLPOTRAD"
      SET ORDER TO TAG ACCTCODE IN GLACCHAR
      SET RELATION TO &lcRpTargt..cacctcode INTO GLACCHAR ADDITIVE    
    ENDIF      
    
  *** Restore the old setting
  WAIT CLEAR
  SET TALK OFF  
  SET ESCAPE &lcSaveEsc 
  ON  ESCAPE &lcSaveOnEs 
  
  *** Display the report to screen , file or printer
  *** and check if there is any record or not
  IF _TALLY = 0        && No records collected
    ** NO recoeds hove been collected
    =gfModalGen("INM00052B00000","DIALOG")
  ELSE
    DO gfDispRe WITH EVAL('lcRpForm')
  ENDIF
ELSE
  SELECT (lcRpTargt)
  IF lcRpForm = "GLPOTRAD"
    SET ORDER TO TAG ACCTCODE IN GLACCHAR
    SET RELATION TO &lcRpTargt..cacctcode INTO GLACCHAR ADDITIVE    
  ENDIF        
  DO gfDispRe WITH EVAL('lcRpForm')
ENDIF 
SET RELATION TO
SELECT (lnOldAlias)  
glEscPrs = .F.

*!************************************************************************
*!
*!      FUNCTION lfChngCond
*!
*!************************************************************************
*
***  Function to switch between two FRX reports
FUNCTION lfChngCond

DO CASE
  CASE lcRpForm =   "GLPOTRAD"
      =lfChangeGrid('GLPOTRA2')
  CASE lcRpForm =   "GLPOTRAS"
      =lfChangeGrid('GLPOTRAN')  
ENDCASE  

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
*!      Function lfvPosYer
*!
*!************************************************************************
*
****  Check if current company has this entried year or not
FUNCTION lfvPosYer

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",DFisBgDat:H="Begin date",DFisEnDat:H="End date"'
laRpRetFld[1] = ''

&& Check If year field is empty
*B600356,1 Move the alltrim from the year value to seek right.
*lcRpPostYr = ALLTRIM(EVALUATE(SYS(18)))
lcRpPostYr = EVALUATE(SYS(18))
lcRpCurFld = VARREAD()
IF .NOT.EMPTY(lcRpPostYr)  
  lcOldAlias = ALIAS()    && Save the current alias
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
    *E300789,7 [BEGIN]
    *IF ('?' $ &lcRpCurFld. .OR.;  
    *  !SEEK(gcPrnt_Cmp+lcRpPostYr)) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
    *    =gfBrows([gcPrnt_Cmp],'CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)
    IF ('?' $ &lcRpCurFld. .OR.;  
      !SEEK(lcRpPostYr)) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
        =gfBrows('','CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)
    *E300789,7 [END..]
      &lcRpCurFld. = laRpRetFld[1]
      lcRpPostYr   = laRpRetFld[1]
      SHOW GET (lcRpCurFld)
    ENDIF
  
  IF llUesdBefo       && .F.- this file used by the system
    
    *HAYTHAR Change this line to Use FISHD instead of SYCFISHD [Begin]
    *USE IN SYCFISHD
    USE IN FISHD
    *HAYTHAR Change this line to Use FISHD instead of SYCFISHD [End]
    
  ENDIF
  IF NOT EMPTY(lcOldAlias)
    SELECT (lcOldAlias)
  ENDIF    
ENDIF
RETURN 

*!************************************************************************
*!
*!      Function lfvPosPrd
*!
*!************************************************************************
*
****  Check if current company has this entried period or not
FUNCTION lfvPosPrd

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
laRpRetFld[1] = ''

lcRpCurFld      = VARREAD()
&& Check If year field is empty
IF .NOT. EMPTY(lcRpPostYr) 
  IF .NOT.EMPTY(ALLTRIM(&lcRpCurFld.))  
    lcOldAlias = ALIAS()    && Save the current alias
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
    *E300789,7 [BEGIN]
    *IF ('?' $ &lcRpCurFld. .OR.;  
    *  !SEEK(gcPrnt_Cmp+ALLTRIM(lcRpPostYr)+ALLTRIM(&lcRpCurFld.)));
    *  .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
    *    =gfBrows([gcPrnt_Cmp+ALLTRIM(lcRpPostYr)],'CFsppRdid',"laRpRetFld",'Transaction Codes ',.F.)
    IF ('?' $ &lcRpCurFld. .OR.;  
      !SEEK(ALLTRIM(lcRpPostYr)+ALLTRIM(&lcRpCurFld.)));
      .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
        =gfBrows([ALLTRIM(lcRpPostYr)],'CFsppRdid',"laRpRetFld",'Transaction Codes ',.F.)    
    *E300789,7 [END..]
      &lcRpCurFld = laRpRetFld[1]
      SHOW GET (lcRpCurFld)
    ENDIF
    IF llUesdBefo       && .F.- this file used by the system
      
      *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [Begin]
      *USE IN SYCFSPRD
      USE IN FSPRD
      *HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [End]
      
    ENDIF
    IF NOT EMPTY(lcOldAlias)
      SELECT (lcOldAlias)
    ENDIF    
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
*!      Function lfRpName
*!
*!************************************************************************
* Return the expersion accourding to its character
Function lfRpName
PARAMETERS lcRpValue

RETURN  SUBSTR(lcRpVldEnt,;
                  ATC('~',lcRpVldEnt,ATC(lcRpValue,'NSBLZEOUAVPYH'))+1,;
                 (ATC('~',lcRpVldEnt,ATC(lcRpValue,'NSBLZEOUAVPYH')+1)-1)-;
                 (ATC('~',lcRpVldEnt,ATC(lcRpValue,'NSBLZEOUAVPYH'))))

*!************************************************************************
*!
*!      FUNCTION lfvTrnCode
*!
*!************************************************************************
*
FUNCTION lfvTrnCode

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CTranNO:H="Code",CTrnDesc:H="Description"'
laRpRetFld[1] = ''
lcRpCurFld      = VARREAD()
SELECT GLPTRNHD
*E300789,6 [BEGIN
SET ORDER TO 1
*E300789,6 [END

SET FILTER TO ALLTRIM(CTRNSTAT) $ 'PZ'
LOCATE FOR ALLTRIM(CTRANNO) = &lcRpCurFld.
IF ('?' $ &lcRpCurFld. .OR. !FOUND()) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
  
  *E300789,6 [BEGIN
  *=gfBrows([''],'CTRANNO',"laRpRetFld",'Transaction Codes ',.F.)
  =gfBrows('','CTRANNO',"laRpRetFld",'Transaction Codes ',.F.)
  *E300789,6 [END 
  
  &lcRpCurFld = laRpRetFld[1]
  SHOW GET (lcRpCurFld)
ENDIF
lcBrFields = lcOldBrFld
SET FILTER TO
