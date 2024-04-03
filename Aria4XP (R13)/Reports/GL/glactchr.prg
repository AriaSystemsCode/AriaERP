*:************************************************************************
*:  Program File: \ARIA4XP\REPORTS\GL\GLACTCHR.PRG
*:  Module      : General Ledger
*:  Desc.       : Chart of accounts
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 09/27/2012
*:  Reference   : *E303230,1 TMI 09/27/2012 [Start] 
*:************************************************************************

*:************************************************************************
*** Report Setup
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables
gcUser_Id = oAriaApplication.User_ID
gcCom_Name = oAriaApplication.ActiveCompanyName
gdSysDate = oAriaApplication.SystemDate
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables
   
lcRpFiles  = IIF(EMPTY(laOGHDFlt[1,1]),"GLACCHAR","GLACCHAR,GLGRPDT")  && Get slected files name

IF llOGFltCh
       
  *** Save escape setting
  lcSaveDel = SET('DELETE')
  lcSaveEsc = SET('ESCAP')
  lcSaveOnEs = ON('ESCAPE')

  SET ESCAP ON
  ON ESCAP DO gpSQLBrak
        
  *** Intialize the varliable that count rows selected
  _TALLY = 0
      
  *** Activate the system select therom.
  SET TALK ON
  
  WAIT 'Collecting data...' WINDOW NOWAIT
  
  ***   Create select  statment
  SELECT   &lcRpFields;
    FROM  &lcRpFiles ;
    WHERE  &lcRpExp ;
     &lcRpOrder.;
    INTO CURSOR (lcRpTargt)
    
  *** Restore all enviroment 
  WAIT CLEAR
  SET TALK OFF
  SET DELETE &lcSaveDel
  ON ESCAPE  &lcSaveOnEs
  SET ESCAPE &lcSaveEsc
  
  *** Display the report to screen , file or printer
  *** and check if there is any record or not
  *** before that check if press Escape or not
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

*!************************************************************************
*!
*!      FUNCTION lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep

*--*
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
lnPos = lfGetPos('laOgFxFlt','GLGRPDT.CGRPCODE')

DECLARE laRpRetFld(1)
lcBrFields    = 'CGrpCode:H="Code",CGrplnhed:H="Description"'
laRpRetFld[1] = ''
lcRpCurFld = loOgScroll.ActiveControl.Parent.cOgArray

SELECT GLGRPHD
SET ORDER TO TAG grpcode
IF '?' $ &lcRpCurFld.[lnPos,6] .OR. !SEEK(&lcRpCurFld.[lnPos,6])
  IF !FOUND() AND BETWEEN(RECNO(0),1,RECCOUNT(ALIAS()))
    GOTO RECNO(0)
  ENDIF 
  =gfBrows([""],'CGrpCode',"laRpRetFld",'Codes File',.F.)
  loOgScroll.&lcRpCurFld.[lnPos,6] = laRpRetFld[1]
ENDIF
WITH loOgScroll
IF EMPTY(&lcRpCurFld)
  STORE '' TO .laOGHdFlt
  _CUROBJ = _CUROBJ + 1
ELSE
  .laOGHDFlt[1,1]='GLACCHAR.CACCTCODE'
  .laOGHDFlt[1,2]='F'
  .laOGHDFlt[1,3]='C'
  .laOGHDFlt[1,4]=.T.      
  .laOGHDFlt[1,5]='Like'
  .laOGHDFlt[1,6]='GLGRPDT.CACCTCODE'
  .laOGHDFlt[1,7]='F'      
ENDIF
ENDWITH 
SET ORDER TO

*!************************************************************************
*!
*!      FUNCTION lfGetPic
*!
*!*************************************************************************
*
FUNCTION lfGetPic

lcOldAlias = ALIAS()    && Save the current alias
llUesdBefo = .F.        && Check if used before or this the first time

IF NOT USED("ACCOD") 
  SELECT 0
    gcDataDir = oAriaApplication.DataDir
  USE &gcDataDir.ACCOD 
  llUesdBefo = .T.
ENDIF
SELECT ACCOD

GO TOP
IF !EOF()
  lcRpSegMas = ALLTRIM(ACCOD.cacsmask)
  
  
  lcRpSegMas = STRTRAN(lcRpSegMas, '#', '9',2) 
  lcRpSegMas = STRTRAN(lcRpSegMas, '#', 'X',1,1) 
ELSE
  lcRpSegMas = " "
ENDIF
IF llUesdBefo       && .F.- this file used by the system
  
  USE IN ACCOD
  
  
ENDIF
IF NOT EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF    
RETURN lcRpSegMas

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

*- End of lfRepWhen.