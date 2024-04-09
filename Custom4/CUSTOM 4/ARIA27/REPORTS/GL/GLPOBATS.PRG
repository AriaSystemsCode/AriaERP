*:************************************************************************
*:
*: Procedure file: GLPOBATS.RPR
*:               : Posted batches
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
*B600344,1 Reham On 05/28/95 Fix the upper case for : S.M.
*B600346,1 Reham On 05/29/95 Spelling mistakes.
*B600350,1 Reham On 05/29/95 In O.G. make the batch no. X99999
*B600339,1 Reham On 05/28/95 "Added By" new object in Option Grid
*B600356,1 Reham On 05/30/95 The posting year should not accept 199 as valid value.
*B600360,1 Reham On 05/30/95 Fix the error if grouping by batch type.
*B800703,1 Hesham El-Sheltawi On 08/29/96
*B800703,1 batch code if exist check for the type of the batch
*E300789,6 MAB 03/06/99 Index tags.
*B602244,1 KAM 09/07/2000 justify the header of report in GLPOBATS.FRX
*:************************************************************************


*** Report Setup
lcRpFiles  = "GLBATCH"  && Get slected files name 
lnTotal    = RECCOUNT('GLBATCH')
lnCount    = 0
lnOldAlias = SELECT()     
lcPrOrder  = IIF(lcRpGroup='lcRpDum',lcRpOrder,;
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
  SELECT  &lcRpFields;
    FROM  &lcRpFiles ;
    WHERE  &lcRpExp  .AND. lfWaitMsg();
    &lcPrOrder.;
    INTO CURSOR &lcRpTargt


    
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
lcRpCurFld = VARREAD()
IF .NOT.EMPTY(&lcRpCurFld.)  
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
*B600356,1 Move the alltrim from the year value to seek right.
*    IF ('?' $ &lcRpCurFld. .OR.;  
      !SEEK(gcPrnt_Cmp+ALLTRIM(&lcRpCurFld.))) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))

    *E300789,6 Index tags [Begin
    *IF ('?' $ &lcRpCurFld. .OR.;  
    *  !SEEK(gcPrnt_Cmp+&lcRpCurFld.)) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))

    IF ('?' $ &lcRpCurFld. .OR.;  
      !SEEK(&lcRpCurFld.)) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))

        *=gfBrows([gcPrnt_Cmp],'CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)
        =gfBrows('','CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)
        
      &lcRpCurFld = laRpRetFld[1]
      SHOW GET (lcRpCurFld)
    ENDIF
    *E300789,6 Index tags [Begin
  
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
*!      Function lfRpName
*!
*!************************************************************************
* Return the expersion accourding to its character
Function lfRpName
PARAMETERS lcRpValue

*B600360,1 Reham On 05/30/95 The batch type "C" --> "Closing end year"
*B600360,1 Reham On 05/30/95 was not consider in the command.

*RETURN SUBSTR(lcRpVldEnt,;
              ATC('~',lcRpVldEnt,ATC(lcRpValue,'NSBLZEOUAVPYH'))+1,;
             (ATC('~',lcRpVldEnt,ATC(lcRpValue,'NSBLZEOUAVPYH')+1)-1)-;
             (ATC('~',lcRpVldEnt,ATC(lcRpValue,'NSBLZEOUAVPYH'))))
RETURN SUBSTR(lcRpVldEnt,;
              ATC('~',lcRpVldEnt,ATC(lcRpValue,'NSBLZEOUAVPYHC'))+1,;
             (ATC('~',lcRpVldEnt,ATC(lcRpValue,'NSBLZEOUAVPYHC')+1)-1)-;
             (ATC('~',lcRpVldEnt,ATC(lcRpValue,'NSBLZEOUAVPYHC'))))
             
*!************************************************************************
*!
*!      FUNCTION lfvBatCode
*!
*!************************************************************************
*
FUNCTION lfvBatCode

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CBATCHNO:H="Code",CBatDesc:H="Description"'
laRpRetFld[1] = ''
lcRpCurFld      = VARREAD()
SELECT GLBATCH
SET ORDER TO TAG BATCHNO
*B800703,1 Hesham El-Sheltawi (Start)
*IF ('?' $ &lcRpCurFld. .OR. !SEEK(&lcRpCurFld.)) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
IF (('?' $ &lcRpCurFld. .OR. !SEEK(&lcRpCurFld.)) OR (SEEK(&lcRpCurFld.) AND !(ALLTRIM(CBATSTAT) $ 'PZ'))) ;
   .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
*B800703,1 Hesham El-Sheltawi (End)
  SET ORDER TO TAG BATSTAT
  =gfBrows([FOR ALLTRIM(CBATSTAT) $ 'PZ'],'CBATCHNO',"laRpRetFld",'Batches Codes ',.F.)
  &lcRpCurFld = laRpRetFld[1]
  SHOW GET (lcRpCurFld)
ENDIF
lcBrFields = lcOldBrFld
SET ORDER TO
