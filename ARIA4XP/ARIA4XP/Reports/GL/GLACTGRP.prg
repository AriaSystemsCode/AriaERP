*:************************************************************************
*:  Program File: \ARIA4XP\REPORTS\GL\GLACTGRP.PRG
*:  Module      : General Ledger
*:  Desc.       : Account groups
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 09/27/2012
*:  Reference   : *E303232,1 
*:************************************************************************
* Modifications
*B610132,1 [T20121023.0022] TMI 10/24/2012 [Start] do not call clearread if comining from lfrepwhen
*:************************************************************************
*** Report Setup

* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables
gcUser_Id = oAriaApplication.User_ID
gcCom_Name = oAriaApplication.ActiveCompanyName
gdSysDate = oAriaApplication.SystemDate
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables

DO CASE
  CASE lcRpForm = "GLACTGRS"

    SELECT GLGRPHD
    GO TOP
    *- Data collection
    =lfCollect()
    SELECT (lcTmpFile)
    DO gfDispRe WITH EVAL('lcRpForm')

  CASE lcRpForm = "GLACTGRD"
    SET ORDER TO TAG ACCTCODE IN GLACCHAR
    
    IF llOGFltCh
        
      *** Save escape setting
      lcSaveEsc = SET('ESCAP')
      lcSaveOnEs = ON('ESCAPE')
      SET ESCAP ON
      ON ESCAP DO gpSQLBrak
        
      *** Intialize the varliable that count rows selected
      _TALLY = 0
        
      *** Activate the system select therom.
      SET TALK ON
      lcRpFiles ="GLGRPHD, GLGRPDT "
      
      WAIT 'Collecting data...' WINDOW NOWAIT     
      
      *SELECT DISTINCT &lcRpFields;
      FROM  &lcRpFiles;
      WHERE &lcRpExp .AND. lfWaitMsg();
      INTO CURSOR &lcRpTargt
      
      lcTmpFile = lcRpTargt
      SELECT GLGRPDT
      SET RELATION TO CGRPCODE INTO glgrphd
      lfCollect()
      SELECT(lcRpTargt)
      
      *** Restore all enviroment 
      WAIT CLEAR
      SET TALK OFF
      ON ESCAPE  &lcSaveOnEs
      SET ESCAPE &lcSaveEsc
      
      IF RECCOUNT(lcRpTargt)= 0        && No records collected
        ** NO recoeds hove been collected
        =gfModalGen("INM00052B00000","DIALOG")
      ELSE
        SET RELATION TO &lcRpTargt..cacctcode INTO GLACCHAR ADDITIVE             
        DO gfDispRe WITH EVAL('lcRpForm')
      ENDIF
    ELSE
      SELECT (lcRpTargt)              

      SET RELATION TO &lcRpTargt..cacctcode INTO GLACCHAR ADDITIVE             

      DO gfDispRe WITH EVAL('lcRpForm')
    ENDIF  
    SET RELATION TO
ENDCASE

************************************************************
*! Name      : lfCollect
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/03/2012
*! Purpose   : Collect data
************************************************************
FUNCTION lfCollect
PARAMETERS lcFile
lcFile = IIF(EMPTY(lcFile),ALIAS(),lcFile)

LOCAL lnSlct
lnSlct = SELECT(0)

  LOCAL laStru[3,4],lnLen,i

  lnI = 0  
  lnI = lnI + 1
  laStru[lnI,1] = "CGRPCODE"
  laStru[lnI,2] = "C"
  laStru[lnI,3] = 8
  laStru[lnI,4] = 0

  lnI = lnI + 1
  laStru[lnI,1] = "CGRPLNHED"
  laStru[lnI,2] = "C"
  laStru[lnI,3] = 40
  laStru[lnI,4] = 0
  
  lnI = lnI + 1
  laStru[lnI,1] = "CGRPSHHED"
  laStru[lnI,2] = "C"
  laStru[lnI,3] = 15
  laStru[lnI,4] = 0
  
  lcIndx = "CGRPCODE"
  
  IF lcRpForm = "GLACTGRD"
    DIMENSION laStru[ALEN(laStru,1)+2,4]
    lnI = lnI + 1
    laStru[lnI,1] = "CACCTCODE"
    laStru[lnI,2] = "C"
    laStru[lnI,3] = 24
    laStru[lnI,4] = 0
    
    lnI = lnI + 1
    laStru[lnI,1] = "CGRDSTAT"
    laStru[lnI,2] = "C"
    laStru[lnI,3] = 1
    laStru[lnI,4] = 0
    
    lcIndx = lcIndx + "+CACCTCODE"
  ENDIF 
  
  CREATE TABLE (oAriaApplication.WorkDir+lcTmpFile) FROM ARRAY laStru
  INDEX ON &lcIndx TAG &lcTmpFile
  
SELECT &lcFile
lcFor = IIF(!EMPTY(ALLTRIM(lcRpExp)),'FOR ','')+lcRpExp
SCAN &lcFor
  SCATTER MEMVAR 
  IF lcRpForm = "GLACTGRD"
    m.CACCTCODE = GLGRPDT.CACCTCODE 
    m.CGRDSTAT  = GLGRPDT.CGRDSTAT  
  ENDIF 
  INSERT INTO (lcTmpFile) FROM MEMVAR 
ENDSCAN 
  
SELECT (lnSlct)
*- End of lfCollect.
      

*!************************************************************************
*!
*!      Function lfChngCond
*!
*!************************************************************************
*
FUNCTION lfChngCond
*B610132,1 TMI 10/24/2012 [Start] add a parameter
PARAMETERS llFromWhen
*B610132,1 TMI 10/24/2012 [End  ] 

llOGFilter=IIF(lcRpForm="GLACTGRD",.F.,.T.)
DO CASE
  CASE lcRpForm="GLACTGRD"
    SELECT SYDREPRT
    SEEK('GLACTGRP')

    LOCATE  WHILE cRep_id= "GLACTGRP" FOR cVer<>"A40"
    IF FOUND('SYDREPRT')
      IF !EMPTY(MREPHDFLT)
	    RESTORE FROM MEMO MREPHDFLT ADDI
	  ENDIF
    ENDIF 
  CASE lcRpForm="GLACTGRS"
    DIMENSION laOGHDFlt[1,7]
    STORE '' TO laOGHdFlt
ENDCASE  

*B610132,1 TMI 10/24/2012 [Start] do not call clearread if comining from lfrepwhen
IF llFromWhen
  RETURN 
ENDIF   
*B610132,1 TMI 10/24/2012 [End  ] 
ClearRead()
    


*!************************************************************************
*!
*!      FUNCTION lfvGrpCode
*!
*!************************************************************************
*
FUNCTION lfvGrpCode

loFld = loOgScroll.ActiveControl
IF loFld.Value == loFld.OldValue OR EMPTY(loFld.Value)
  RETURN 
ENDIF   

lnPos = lfGetPos('laOgFxFlt','GLGRPHD.CGRPCODE')

DECLARE laRpRetFld(1)
lcBrFields    = 'CGrpCode:H="Code",CGrplnhed:H="Description"'
laRpRetFld[1] = ''
lcRpCurFld = loOgScroll.ActiveControl.Parent.cOgArray

SELECT GLGRPHD
SET ORDER TO TAG GRPCODE

IF !EMPTY(&lcRpCurFld.[lnPos,6]) AND ('?' $ &lcRpCurFld.[lnPos,6] .OR. !SEEK(&lcRpCurFld.[lnPos,6],"GLGRPHD"))  
  IF !FOUND() AND BETWEEN(RECNO(0),1,RECCOUNT(ALIAS()))
    GOTO RECNO(0)
  ENDIF 
  =gfBrows(" ",'CGrpCode',"laRpRetFld",'Codes File',.F.)
  loOgScroll.&lcRpCurFld.[lnPos,6] = laRpRetFld[1]
ENDIF

*!************************************************************************
*!
*!      Function lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep


************************************************************
*! Name      : lfGetPos
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/30/2012
*! Purpose   : Get array position
************************************************************
FUNCTION lfGetPos
PARAMETERS lcArr,lcFld
LOCAL lnPos
lnPos = ASUBSCRIPT(loOgScroll.&lcArr.,ASCAN(loOgScroll.&lcArr.,lcFld),1)
RETURN lnPos
*- End of lfGetPos.

************************************************************
*! Name      : lfRepWhen
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/30/2012
*! Purpose   : When Function
************************************************************
FUNCTION lfRepWhen
SET PROCEDURE TO (loOgScroll.gcRepHome + loOgScroll.gcAct_Appl + '\glrepfnc.fxp') ADDITIVE 
lfChngCond(.T.)
*- End of lfRepWhen.