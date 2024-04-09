*:************************************************************************
*:
*: Procedure file: GLUBATRN.RPR
*:               : Unposted batches\transactions
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
*B600346,1 Reham On 05/29/95 Spelling mistakes.
*B600344,1 Reham On 05/28/95 Fix the upper case for : S.J.
*B600352,1 Reham On 05/29/95 1- Fix the batch browse, it has to be filtered
*B600352,1 Reham On 05/29/95    for (Out of balance,On hold,Empty,Approved).
*B600352,1 Reham On 05/29/95 2- The batch status was not printed in the summary form.
*B600338,1 Reham On 05/28/95 Increase the date width to 10 ch.
*B600339,1 Reham On 05/28/95 "Added By" new object in Option Grid
*B600356,1 Reham On 05/30/95 The posting year should not accept 199 as valid value.

*B600344,4 Reham On 06/11/95 Change the batch no. pic in the SYREPUVR file
*B600344,4 from "999999" to be "@! X99999"
*E300789,6 MAB 03/06/99 Remove ccomp_id from files
*B603800,1 SSE 08/13/2000 Enh# of Updating GlBatch File fields cBatElUsr , dBatElDat , cBatElTim
*B603800,1                After Previewing or Running Report
*B602244,1 KAM 09/05/2000 Increase the width of Debit, Credit fields and 
*B602244,1                Righ Justify for them in GLUBATRS.FRX,GLUBATRD.FRX
*B803679,1 KAM 09/21/2000 we update mrepavfld field in sydreprt file
*B606559,1 ASH 10/23/2002 Update GlBatch File fields cBatElUsr , dBatElDat , cBatElTim only if printing not previewing.
*:************************************************************************
*** Report Setup
SELECT GLBATCH
DO CASE
  CASE lcRpGroup == 'cBatType'
    SET ORDER TO TAG BATTYPE
  CASE lcRpGroup == 'cBatStat'
    SET ORDER TO TAG BATSTAT
  OTHERWISE
    SET ORDER TO TAG BATCHNO
ENDCASE
SET ORDER TO TAG BATCHTRN IN GLTRNSHD
SET RELATION TO glbatch.cbatchno INTO GLTRNSHD ADDITIVE
IF lcRpForm = "GLUBATRS"
  SET SKIP TO GLTRNSHD
ELSE
  SELECT GLTRNSHD
  SET ORDER TO TAG BATCHTRN IN GLTRNSDT
  SET ORDER TO TAG ACCTCODE IN GLACCHAR
  SET RELATION TO gltrnshd.cbatchno + gltrnshd.ctranno INTO GLTRNSDT ADDITIVE
  SELECT GLTRNSDT
  SET RELATION TO gltrnsdt.cacctcode INTO GLACCHAR ADDITIVE
  SELECT GLBATCH
  SET SKIP TO GLTRNSDT, GLTRNSHD
ENDIF
DO gfDispRe WITH EVAL('lcRpForm'),IIF(EMPTY(lcRpExp),'','FOR ')+lcRpExp
SET RELATION TO
SET SKIP TO
IF lcRpForm = "GLUBATRD"
  SELECT GLTRNSHD
  SET RELATION TO
  SELECT GLTRNSDT
  SET RELATION TO  
ENDIF

*B603800,1 [Begin]
SELECT GlBatch
*B606559,1 ASH 10/23/2002 (Begin) Update GlBatch File fields cBatElUsr , dBatElDat , cBatElTim only if printing not previewing.
*IF !EMPTY(ALLTRIM(lcRpExp))
IF !EMPTY(ALLTRIM(lcRpExp)) AND gcDevice <> 'SCREEN'
*B606559,1 ASH 10/23/2002 (End)
  SCAN FOR &lcRpExp
    =RLOCK()
    REPLACE cbatElUsr WITH gcUser_ID ,;
            dBatElDat WITH DATE()    ,;
            cBatElTim WITH gfGetTime() 
    UNLOCK
  ENDSCAN
ENDIF
*B603800,1 [End]

RETURN

DO CASE
  CASE lcRpForm = "GLUBATRS"
      SELECT GLBATCH
      DO CASE
        CASE lcRpGroup == 'cBatType'
          SET ORDER TO TAG BATTYPE
        CASE lcRpGroup == 'cBatStat'
          SET ORDER TO TAG BATSTAT
        OTHERWISE
          SET ORDER TO TAG BATCHNO
      ENDCASE
      SET ORDER TO TAG BATCHTRN IN GLTRNSHD
      SET RELATION TO glbatch.cbatchno INTO GLTRNSHD ADDITIVE
      SET SKIP TO GLTRNSHD
      DO gfDispRe WITH EVAL('lcRpForm'),IIF(EMPTY(lcRpExp),'','FOR ')+lcRpExp
      SET RELATION TO
      SET SKIP TO
      RETURN
    lcRpFiles  = "GLBATCH,GLTRNSHD"  && Get slected files name 
  CASE lcRpForm = "GLUBATRD"
    lcRpFiles  = "GLBATCH,GLTRNSHD,GLTRNSDT,GLACCHAR"  && Get slected files name 
ENDCASE   


***   Get the field order ****

lnOldAlias = SELECT()     
lcPrOrder = IIF(lcRpGroup='lcRpDum',lcRpOrder,;
                'ORDER BY '+'GLBATCH.'+lcRpGroup+;
                IIF(EMPTY(ALLTRIM(lcRpOrder)),'',',')+STRTRAN(lcRpOrder,'ORDER BY',''))
***   Create select  statment
IF llOGFltCh .OR. (lcRpOldOrd <> lcPrOrder) .OR. glEscPrs
  lcRpOldOrd = lcPrOrder
  
  *** Save escape setting
  lcSaveEsc = SET('ESCAPE')
  lcSaveOnEs = ON('ESCAPE')
  SET ESCAPE ON
  ON ESCAPE DO gpSQLBrak
        
  *** Intialize the varliable that count rows selected
  _TALLY = 0
       
  *** Activate the system select therom.
  SET TALK ON
  
  WAIT 'Collecting data...' WINDOW NOWAIT     
  
  *** Select data from file(s)
  SELECT  DISTINCT  &lcRpFields;
    FROM  &lcRpFiles ;
    WHERE  &lcRpExp  ;
    &lcPrOrder.;
    INTO CURSOR (lcRpTargt)

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
*!      FUNCTION lfChngCond
*!
*!************************************************************************
*
***  Function to switch between two FRX reports
FUNCTION lfChngCond

DO CASE
  CASE lcRpForm =   "GLUBATRD"
      =lfChangeGrid('GLUBATR2')
  CASE lcRpForm =   "GLUBATRS"
      =lfChangeGrid('GLUBATRN')  
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
*    IF ('?' $ &lcRpCurFld. .OR.;  
      !SEEK(gcPrnt_Cmp+ALLTRIM(&lcRpCurFld.))) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
*B600356,1 Reham On 05/30/95 The posting year should not accept 199 as valid value.
*B600356,1 Move the alltrim from the year value to seek right.

    *E300789,6 [begin
    *E IF ('?' $ &lcRpCurFld. .OR.;  
      *E !SEEK(gcPrnt_Cmp+&lcRpCurFld.)) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))

    IF ('?' $ &lcRpCurFld. .OR.;  
      !SEEK(&lcRpCurFld.)) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
        *E =gfBrows([gcPrnt_Cmp],'CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)
        =gfBrows('','CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)
      &lcRpCurFld = laRpRetFld[1]
      SHOW GET (lcRpCurFld)
    ENDIF
    *E300789,6 [end
  
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

RETURN IIF(EMPTY(lcRpValue) OR !(lcRpValue $ 'NSBLZEOUAVPYH'),'',SUBSTR(lcRpVldEnt,;
                  ATC('|',lcRpVldEnt,ATC(lcRpValue,'NSBLZEOUAVPYH'))+1,;
                 (ATC('|',lcRpVldEnt,ATC(lcRpValue,'NSBLZEOUAVPYH')+1)-1)-;
                 (ATC('|',lcRpVldEnt,ATC(lcRpValue,'NSBLZEOUAVPYH')))))
                 
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
IF !EMPTY(EVAL(lcRpCurFld))
  &lcRpCurFld=trans(VAL(EVAL(lcRpCurFld)),'@l 999999')
  SHOW GET &lcRpCurFld
ENDIF  

SELECT GLBATCH
SET ORDER TO TAG BATCHNO
IF ('?' $ &lcRpCurFld. .OR. !SEEK(&lcRpCurFld.)) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
  SET ORDER TO TAG BATSTAT
  =gfBrows([FOR ALLTRIM(CBATSTAT) $ 'EOUHA'],'CBATCHNO',"laRpRetFld",'Batches Codes ',.F.)
  &lcRpCurFld = laRpRetFld[1]
  SHOW GET (lcRpCurFld)
ENDIF
lcBrFields = lcOldBrFld
SET ORDER TO

*!************************************************************************
*!
*!      FUNCTION lfvSorMod
*!
*!************************************************************************
*
FUNCTION lfvSorMod

DECLARE laRpRetFld(1)
IF EMPTY(EVAL(VARREAD()))
  RETURN
ENDIF
lcOldBrFld    = lcBrFields
lcBrFields    = 'Capp_id:H="Modul code",Capp_name:H="Modul name"'
laRpRetFld[1] = ''
lcRpCurFld      = VARREAD()
IF !USED('SYDAPPL')
  SELECT 0
  USE (gcSysHome+'SYDAPPL')
ENDIF
SELECT sydAPPL
SET ORDER TO TAG CAPP_ID
IF !SEEK(EVAL(lcRpCurFld)) AND LASTKEY()=13
  =gfBrows([FOR !INLIST(CAPP_ID,'SY','SM')],'CAPP_ID',"laRpRetFld",'Moduls',.F.)
  &lcRpCurFld = laRpRetFld[1]
ELSE
  IF !SEEK(EVAL(lcRpCurFld))    
    &lcRpCurFld=""
  ENDIF
ENDIF
SHOW GET (lcRpCurFld)
lcBrFields = lcOldBrFld
SET ORDER TO

