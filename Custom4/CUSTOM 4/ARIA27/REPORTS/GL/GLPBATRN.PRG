*:************************************************************************
*:
*: Procedure file: GLPBATRN.RPR
*:               : Posted batches\transactions
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
*B600350,1 Reham On 05/29/95 Aline the headings with its objects.
*B600350,1 Reham On 05/29/95 Fix the period/year pic. in the detaild form
*B600344,1 Reham On 05/28/95 Fix the upper case for : S.J.
*B600338,1 Reham On 05/28/95 Increase the date width to 10 ch.
*B600339,1 Reham On 05/28/95 "Added By" new object in Option Grid
*B600356,1 Reham On 05/30/95 The posting year should not accept 199 as valid value.
*B600360,1 Reham On 05/30/95 Fix the error if grouping by batch type.
*B800332,1 Reham On 04/11/96 Sort the transactions by batch # + transaction #
*B800332,1 in the detailed form.
*B601070,1 M.H   On 05/20/96 If a transaction is created from AP (payable invoice) as follows :
*B601070,1 M.H   On 05/20/96              APDIST.cApdTrTyp     (Dr/Cr) 
*B601070,1 M.H   On 05/20/96 Cash account (instead of AP/ account) as       'A'             Credit
*B601070,1 M.H   On 05/20/96 A/P account  (instead of Exp. account) as      'D'             Debit
*B601070,1 M.H   On 05/20/96 then paid, we get the following.
*B601070,1 M.H   On 05/20/96 Debit the cash account (instead of the A/P account)
*B601070,1 M.H   On 05/20/96 and credit the cash account (default action)
*B601070,1 M.H   On 05/20/96 This adds the following records in the APDIST
*B601070,1 M.H   On 05/20/96                                APDIST.cApdTrTyp     (Dr/Cr) 
*B601070,1 M.H   On 05/20/96 Cash account (instead of AP/ account) as       'A'             Debit
*B601070,1 M.H   On 05/20/96 Cash account as                                'C'             Credit
*B601070,1 M.H   On 05/20/96 When the above transaction is released to G/L and posted,
*B601070,1 M.H   On 05/20/96 The Posted batched/transactions report shows only 3 transactions of the above. Researching, this is due to having two similare records for Cash
*B601070,1 M.H   On 05/20/96 account ((both credit), and the report includes a 'SELECT DISTINCT' SQL.
*B800703,1 Hesham El-Sheltawi On 08/29/96
*B800703,1 batch code if exist check for the type of the batch
*B602244,1 KAM 07/09/2000 right justify for debit,credit title,also the 
*B602244,1                header of the report in the files GLPBATRD,GLPBATRS
*B607055,1 ABD 03/19/2003 Variable "CACCTCODE " Not Found.
*B126585,1 MHM 04/01/2005 Fix Bug of it doesn't give option to select the years because there are no years to select.
*:************************************************************************

*** Report Setup


DO CASE
  CASE lcRpForm = "GLPBATRS"
    lcRpFiles  = "GLBATCH,GLPTRNHD"  && Get slected files name 
    lnTotal    = RECCOUNT('GLPTRNHD')
  CASE lcRpForm = "GLPBATRD"
    lnTotal    = RECCOUNT('GLPTRNDT')
*    lcRpFiles  = "GLBATCH,GLPTRNHD,GLPTRNDT,GLACCHAR"  && Get slected files name 
     lcRpFiles  = "GLBATCH,GLPTRNHD,GLPTRNDT"  && Get slected files name    
     
ENDCASE   

*B607055,1 ABD - assign all field again to handel Variable "CACCTCODE " Not Found. [Begin]
lcrpFields = GetRpField()
*B607055,1 ABD - [End]
***   Get the field order ****

lnCount    = 0
lnOldAlias = SELECT()     

*B800332,1 Reham On 04/11/96    *** Begin ***
*B800332,1 Sort the transactions by transaction # in the detailed form.
*lcPrOrder = IIF(lcRpGroup='lcRpDum',lcRpOrder,;
                'ORDER BY '+'GLBATCH.'+lcRpGroup+;
                IIF(EMPTY(ALLTRIM(lcRpOrder)),'',',')+STRTRAN(lcRpOrder,'ORDER BY',''))

*B800332,1 If it's the detailed form.
IF lcRpForm = "GLPBATRD"
  *B800332,1 If there is no group order by batch # + trans. # & if there
  *B800332,1 is group order by the (group) + batch # + trans. #
  lcPrOrder = IIF(lcRpGroup='lcRpDum','ORDER BY GLBATCH.cBatchNo,GLPTRNHD.cTranNo',;
                  'ORDER BY GLBATCH.'+lcRpGroup+',GLBATCH.cBatchNo,GLPTRNHD.cTranNo')
ELSE
  *B800332,1 Keep the variable the same if it's the summary form.
  lcPrOrder = IIF(lcRpGroup='lcRpDum',lcRpOrder,;
                  'ORDER BY '+'GLBATCH.'+lcRpGroup+;
                  IIF(EMPTY(ALLTRIM(lcRpOrder)),'',',')+STRTRAN(lcRpOrder,'ORDER BY',''))
ENDIF
*B800332,1 Reham On 04/11/96    *** End   ***

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
*B601070,1 M.H   On 05/20/96 Begin.
*  SELECT DISTINCT  &lcRpFields;
    FROM  &lcRpFiles ;
    WHERE  &lcRpExp  .AND. lfWaitMsg();
    &lcPrOrder.;
    INTO CURSOR &lcRpTargt

  SELECT &lcRpFields;
    FROM &lcRpFiles ;
   WHERE &lcRpExp  .AND. lfWaitMsg();
         &lcPrOrder.;
    INTO CURSOR &lcRpTargt



*B601070,1 M.H   On 05/20/96 End.

    IF lcRpForm = "GLPBATRD"
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
  
  *** If detail report
  IF lcRpForm = "GLPBATRD"  
    SET ORDER TO TAG ACCTCODE IN GLACCHAR
    SET RELATION TO &lcRpTargt..cacctcode INTO GLACCHAR ADDITIVE      
  ENDIF    
  
  DO gfDispRe WITH EVAL('lcRpForm')
ENDIF 

*** Clear relation
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
  CASE lcRpForm =   "GLPBATRD"
      =lfChangeGrid('GLPBATR2')
  CASE lcRpForm =   "GLPBATRS"
      =lfChangeGrid('GLPBATRN')  
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
  *  IF ('?' $ &lcRpCurFld. .OR.;  
      !SEEK(gcPrnt_Cmp+ALLTRIM(&lcRpCurFld.))) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
    *B126585,1 MHM 04/01/2005 remove filtering by Company because we are using FISHD instead of SYCFISHD Now [Start]       
    *IF ('?' $ &lcRpCurFld. .OR.;  
    *  !SEEK(gcPrnt_Cmp+&lcRpCurFld.)) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
    *    =gfBrows([gcPrnt_Cmp],'CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)
    IF ('?' $ &lcRpCurFld. .OR.;  
      !SEEK(&lcRpCurFld.)) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
        =gfBrows('','CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)
    *B126585,1 MHM 04/01/2005 [End]      
        
      &lcRpCurFld = laRpRetFld[1]
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
*!      Function lfRpName
*!
*!************************************************************************
* Return the expersion accourding to its character
Function lfRpName
PARAMETERS lcRpValue

*B600360,1 Reham On 05/30/95 The batch type "C" --> "Closing end year"
*B600360,1 Reham On 05/30/95 was not consider in the command.
*RETURN  SUBSTR(lcRpVldEnt,;
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

*!************************************************************************
*!
*!      FUNCTION lfEditList
*!
*!************************************************************************
*
FUNCTION lfEditList

RETURN (DTOS(glbatch.dbateldat)+ glbatch.cbateltim > DTOS(glbatch.dadd_date)+ glbatch.cadd_time)

*!************************************************************************
*!
*!      FUNCTION lfAuditList
*!
*!************************************************************************
*
FUNCTION lfAuditList

RETURN (DTOS(glbatch.dbatatdat)+ glbatch.cbatattim > DTOS(glbatch.dpostdate)+ glbatch.cposttime)
