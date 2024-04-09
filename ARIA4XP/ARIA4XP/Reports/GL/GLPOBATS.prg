*:************************************************************************
*:  Program File: \ARIA4XP\REPORTS\GL\GLPOBATS.PRG
*:  Module      : General Ledger
*:  Desc.       : Posted batches
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 09/27/2012
*:  Reference   : E303239,1
*:************************************************************************
*:************************************************************************

*** Report Setup

* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables
gcUser_Id = oAriaApplication.User_ID
gcCom_Name = oAriaApplication.ActiveCompanyName
gdSysDate = oAriaApplication.SystemDate
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables

lcRpFiles  = "GLBATCH"  && Get slected files name 
lnTotal    = RECCOUNT('GLBATCH')
lnCount    = 0
lnOldAlias = SELECT()     
lcPrOrder  = IIF(lcRpGroup='lcRpDum',lcRpOrder,;
                 'ORDER BY '+lcRpGroup+;
                 IIF(EMPTY(ALLTRIM(lcRpOrder)),'',',')+STRTRAN(lcRpOrder,'ORDER BY',''))
                
IF llOGFltCh .OR. (lcRpOldOrd <> lcPrOrder) 

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

*!************************************************************************
*!
*!      FUNCTION lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep


*!************************************************************************
*!
*!      Function lfRpName
*!
*!************************************************************************
* Return the expersion accourding to its character
Function lfRpName
PARAMETERS lcRpValue

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

loFld = loOgScroll.ActiveControl
IF loFld.Value == loFld.OldValue OR EMPTY(loFld.Value)
  RETURN 
ENDIF   
lcRpCurFld = loOgScroll.ActiveControl.Parent.oItem.cAssociate

DECLARE laRpRetFld(1)
lcBrFields    = 'CBATCHNO:H="Code",CBatDesc:H="Description"'
laRpRetFld[1] = ''

SELECT GLBATCH
SET ORDER TO TAG BATCHNO
IF (('?' $ &lcRpCurFld. .OR. !SEEK(&lcRpCurFld.)) OR (SEEK(&lcRpCurFld.) AND !(ALLTRIM(CBATSTAT) $ 'PZ'))) ;
   .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
  SET ORDER TO TAG BATSTAT
  =gfBrows([FOR ALLTRIM(CBATSTAT) $ 'PZ'],'CBATCHNO',"laRpRetFld",'Batches Codes ',.F.)
  if Empty(laRpRetFld[1])
    laRpRetFld[1] = loFld.OldValue
  endif 
  loOgScroll.&lcRpCurFld = laRpRetFld[1]
ENDIF
SET ORDER TO

************************************************************
*! Name      : lfRepWhen
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/30/2012
*! Purpose   : When Function
************************************************************
FUNCTION lfRepWhen
SET PROCEDURE TO (loOgScroll.gcRepHome + loOgScroll.gcAct_Appl + '\glrepfnc.fxp') ADDITIVE 

*- End of lfRepWhen.