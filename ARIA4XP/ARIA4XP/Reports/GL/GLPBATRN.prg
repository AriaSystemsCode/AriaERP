*:************************************************************************
*:  Program File: \ARIA4XP\REPORTS\GL\GLPBATRN.PRG
*:  Module      : General Ledger
*:  Desc.       : Posted batches\transactions
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 09/27/2012
*:  Reference   : E303240,1
*:************************************************************************
*B610132,1 [T20121023.0022] TMI 10/24/2012 remove the calling to the function lfChangeGrid
*:************************************************************************

*** Report Setup

* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables
gcUser_Id = oAriaApplication.User_ID
gcCom_Name = oAriaApplication.ActiveCompanyName
gdSysDate = oAriaApplication.SystemDate
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables


DO CASE
  CASE lcRpForm = "GLPBATRS"
    lcRpFiles  = "GLBATCH,GLPTRNHD"  && Get slected files name 
    lnTotal    = RECCOUNT('GLPTRNHD')
    *E303240,1 TMI 10/02/2012 [Start] 
    lcrpFields = GetRpField()
    *E303240,1 TMI 10/02/2012 [End  ] 
  CASE lcRpForm = "GLPBATRD"

    lcAddExpr = " AND GLPTRNDT.CBATCHNO+GLPTRNDT.CTRANNO = GLPTRNHD.CBATCHNO+GLPTRNHD.CTRANNO"
    IF !lcAddExpr $ loOgScroll.lcRpExp
      loOgScroll.lcRpExp = loOgScroll.lcRpExp + lcAddExpr
    ENDIF   

    lnTotal    = RECCOUNT('GLPTRNDT')
     lcRpFiles  = "GLBATCH,GLPTRNHD,GLPTRNDT"  && Get slected files name    

     *E303240,1 TMI 10/02/2012 [Start] 
     lcrpFields='GLBATCH.CBATSTAT  ,GLBATCH.LBATIND   ,GLBATCH.CBATREFER ,GLBATCH.CBATDESC  ,GLBATCH.NBATCNTOT ,GLBATCH.NBATOTDR  ,'+;
                'GLBATCH.NBATOTCR  ,GLBATCH.CBATTYPE  ,GLBATCH.CBATELUSR ,GLBATCH.DBATELDAT ,GLBATCH.CBATELTIM ,GLBATCH.CBATARUSR ,GLBATCH.DBATATDAT ,'+;
                'GLBATCH.CBATATTIM ,GLBATCH.CBATCHNO  ,GLBATCH.CBATPYR   ,GLBATCH.DBATPBEG  ,GLBATCH.DBATPEND  ,GLBATCH.CSRCMODUL ,GLBATCH.CPOSTSESS ,'+;
                'GLBATCH.CPOSTPROG ,GLBATCH.CPOSTUSER ,GLBATCH.DPOSTDATE ,GLBATCH.CPOSTTIME ,GLBATCH.CADD_USER ,GLBATCH.DADD_DATE ,GLBATCH.CADD_TIME ,'+;
                'GLBATCH.LLOK_STAT ,GLBATCH.CLOK_USER ,GLBATCH.DLOK_DATE ,GLBATCH.CLOK_TIME ,GLBATCH.CCOMP_ID  ,GLPTRNHD.CTRNDESC  ,GLPTRNHD.CTRNREFER ,'+;
                'GLPTRNHD.CTRNTYPE  ,GLPTRNHD.CTRNREVER ,GLPTRNHD.DTRNREVDT ,GLPTRNHD.CTRNREVYR ,GLPTRNHD.CTRNREVPR ,GLPTRNHD.CTRNSTAT  ,GLPTRNHD.NTRNTOTDR ,'+;
                'GLPTRNHD.NTRNTOTCR ,GLPTRNHD.NTRNINDIC ,GLPTRNHD.CAUTCODE  ,GLPTRNHD.CAUTTYPE  ,GLPTRNHD.CSTANDARD ,GLPTRNHD.CTRANNO   ,GLPTRNHD.CTRNSLEDN ,'+;
                'GLPTRNHD.DTRNPDATE ,GLPTRNHD.CTRNPYR   ,GLPTRNHD.CTRNPPRD  ,GLPTRNHD.CSRCJRNL  ,GLPTRNHD.CPOSTSESS ,GLPTRNHD.CPOSTPROG ,GLPTRNHD.CPOSTUSER ,'+;
                'GLPTRNHD.CPOSTTIME ,GLPTRNDT.CACCTCODE ,GLPTRNDT.CTRDTEXP  ,GLPTRNDT.CDRORCR   ,GLPTRNDT.NAMOUNT   '
ENDCASE   

lnCount    = 0
lnOldAlias = SELECT()     

IF lcRpForm = "GLPBATRD"
  lcPrOrder = IIF(lcRpGroup='lcRpDum','ORDER BY GLBATCH.cBatchNo,GLPTRNHD.cTranNo',;
                  'ORDER BY GLBATCH.'+lcRpGroup+',GLBATCH.cBatchNo,GLPTRNHD.cTranNo')
ELSE
  lcPrOrder = IIF(lcRpGroup='lcRpDum',lcRpOrder,;
                  'ORDER BY '+'GLBATCH.'+lcRpGroup+;
                  IIF(EMPTY(ALLTRIM(lcRpOrder)),'',',')+STRTRAN(lcRpOrder,'ORDER BY',''))
ENDIF

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
  SELECT &lcRpFields;
    FROM &lcRpFiles ;
   WHERE &lcRpExp  .AND. lfWaitMsg();
         &lcPrOrder.;
    INTO CURSOR &lcRpTargt
  
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
  IF RECCOUNT(lcRpTargt) = 0        && No records collected
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
*B610132,1 [T20121023.0022] TMI 10/24/2012 [Start] 
*      =lfChangeGrid('GLPBATR2')
*B610132,1 [T20121023.0022] TMI 10/24/2012 [End  ] 
  CASE lcRpForm =   "GLPBATRS"
*B610132,1 [T20121023.0022] TMI 10/24/2012 [Start] 
*      =lfChangeGrid('GLPBATRN')  
*B610132,1 [T20121023.0022] TMI 10/24/2012 [End  ] 
ENDCASE  
ClearRead()

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

************************************************************
*! Name      : lfRepWhen
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/02/2012
*! Purpose   : When function
************************************************************
FUNCTION lfRepWhen
SET PROCEDURE TO (loOgScroll.gcRepHome + loOgScroll.gcAct_Appl + '\glrepfnc.fxp') ADDITIVE 

*- End of lfRepWhen.