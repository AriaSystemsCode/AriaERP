*:************************************************************************
*:  Program File: \ARIA4XP\REPORTS\GL\GLPOTRAN.PRG
*:  Module      : General Ledger
*:  Desc.       : Posted transactions
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 09/27/2012
*:  Reference   : E303241,1
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
  CASE lcRpForm = "GLPOTRAS"
    lcRpFiles   = "GLPTRNHD"  && Get slected files name 
    lnTotal     =  RECCOUNT('GLPTRNHD')
    *E303241,1 TMI 10/02/2012 [Start] 
    lcrpFields = GetRpField()
    *E303241,1 TMI 10/02/2012 [End  ] 
  CASE lcRpForm = "GLPOTRAD"
    lnTotal     =  RECCOUNT('GLPTRNDT')
    lcRpFiles   = "GLPTRNHD,GLPTRNDT"  && Get slected files name 
    *E303241,1 TMI 10/02/2012 [Start] 
    lcrpFields = GetRpField() + ;
                 ',GLPTRNDT.CACCTCODE ,GLPTRNDT.CTRDTEXP  ,GLPTRNDT.CDRORCR   ,GLPTRNDT.NAMOUNT'
    *E303241,1 TMI 10/02/2012 [End  ] 
ENDCASE   

***   Get the field order ****
lnCount    = 0
lcPrOrder  = ''
lnOldAlias = SELECT()     
lcPrOrder = IIF(lcRpGroup='lcRpDum',lcRpOrder,;
                'ORDER BY '+lcRpGroup+;
                IIF(EMPTY(ALLTRIM(lcRpOrder)),'',',')+STRTRAN(lcRpOrder,'ORDER BY',''))
                
***   Create select  statment
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
  
  IF lcRpForm = "GLPOTRAD"
    *E303241,1 TMI 10/02/2012 [Start] 
    lcRpExp = lcRpExp + ;
              ' AND GLPTRNHD.CBATCHNO = GLPTRNDT.CBATCHNO    AND  GLPTRNHD.CTRANNO = GLPTRNDT.CTRANNO'
    *E303241,1 TMI 10/02/2012 [End  ] 
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
*B610132,1 [T20121023.0022] TMI 10/24/2012 [Start] 
*      =lfChangeGrid('GLPOTRA2')
*B610132,1 [T20121023.0022] TMI 10/24/2012 [End  ] 
  CASE lcRpForm =   "GLPOTRAS"
*B610132,1 [T20121023.0022] TMI 10/24/2012 [Start] 
*      =lfChangeGrid('GLPOTRAN')  
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
*!      Function lfvPosYer
*!
*!************************************************************************
*
****  Check if current company has this entried year or not
FUNCTION x_lfvPosYer

DECLARE laRpRetFld(1)
lcBrFields    = 'CFisFYear:H="Year",DFisBgDat:H="Begin date",DFisEnDat:H="End date"'
laRpRetFld[1] = ''

&& Check If year field is empty
lcRpPostYr  = loOgScroll.ActiveControl.Value
lcRpCurFld = loOgScroll.ActiveControl.Parent.cOgArray
IF .NOT.EMPTY(lcRpPostYr)  
  lcOldAlias = ALIAS()    && Save the current alias
  llUesdBefo = .F.        && Check if used before or this the first time
  
  IF NOT USED("FISHD") 
    SELECT 0
    gcDataDir = oAriaApplication.DataDir
    USE &gcDataDir.FISHD ORDER TAG compfyear
    llUesdBefo = .T.
  ENDIF
  SELECT FISHD
  
  
  *** Search for the current company+year
    IF ('?' $ &lcRpCurFld. .OR.;  
      !SEEK(lcRpPostYr)) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
        =gfBrows('','CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)
      &lcRpCurFld. = laRpRetFld[1]
      lcRpPostYr   = laRpRetFld[1]
    ENDIF
  
  IF llUesdBefo       && .F.- this file used by the system
    
    USE IN FISHD
    
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
FUNCTION x_lfvPosPrd

DECLARE laRpRetFld(1)
lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
laRpRetFld[1] = ''

lcRpCurFld = loOgScroll.ActiveControl.Parent.cOgArray
&& Check If year field is empty
IF .NOT. EMPTY(lcRpPostYr) 
  IF .NOT.EMPTY(ALLTRIM(&lcRpCurFld.))  
    lcOldAlias = ALIAS()    && Save the current alias
    llUesdBefo = .F.        && Check if used before or this the first time
    
    IF NOT USED("FSPRD") 
      SELECT 0
      USE &gcDataDir.FSPRD ORDER TAG comfyrprdi
      llUesdBefo = .T.
    ENDIF
    SELECT FSPRD
    
    
    *** Search for the current company+year+Prd
    IF ('?' $ &lcRpCurFld. .OR.;  
      !SEEK(ALLTRIM(lcRpPostYr)+ALLTRIM(&lcRpCurFld.)));
      .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
        =gfBrows([ALLTRIM(lcRpPostYr)],'CFsppRdid',"laRpRetFld",'Transaction Codes ',.F.)    
      &lcRpCurFld = laRpRetFld[1]
    ENDIF
    IF llUesdBefo       && .F.- this file used by the system
      
      USE IN FSPRD
      
    ENDIF
    IF NOT EMPTY(lcOldAlias)
      SELECT (lcOldAlias)
    ENDIF    
  ENDIF
ELSE
  &lcRpCurFld = "  "
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

loFld = loOgScroll.ActiveControl
IF loFld.Value == loFld.OldValue OR EMPTY(loFld.Value)
  RETURN 
ENDIF   

DECLARE laRpRetFld(1)
lcBrFields    = 'CTranNO:H="Code",CTrnDesc:H="Description"'
laRpRetFld[1] = ''
lcRpCurFld = loOgScroll.ActiveControl.Parent.oItem.cAssociate
SELECT GLPTRNHD
SET ORDER TO 1

SET FILTER TO ALLTRIM(CTRNSTAT) $ 'PZ'
LOCATE FOR ALLTRIM(CTRANNO) = &lcRpCurFld.
IF ('?' $ &lcRpCurFld. .OR. !FOUND()) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
  IF !FOUND() AND BETWEEN(RECNO(0),1,RECCOUNT(ALIAS()))
    GOTO RECNO(0)
  ENDIF 
  =gfBrows('','CTRANNO',"laRpRetFld",'Transaction Codes ',.F.)
  IF EMPTY(laRpRetFld[1])
    laRpRetFld[1] = loFld.OldVAlue
  ENDIF 
  loOgScroll.&lcRpCurFld = laRpRetFld[1]
ENDIF
SET FILTER TO

************************************************************
*! Name      : lfRepWhen
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/30/2012
*! Purpose   : When Function
************************************************************
FUNCTION lfRepWhen
SET PROCEDURE TO (loOgScroll.gcRepHome + loOgScroll.gcAct_Appl + '\glrepfnc.fxp') ADDITIVE 

*- End of lfRepWhen.