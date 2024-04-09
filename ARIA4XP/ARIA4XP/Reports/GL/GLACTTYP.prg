*:************************************************************************
*:  Program File: \ARIA4XP\REPORTS\GL\GLACTTYP.PRG
*:  Module      : General Ledger
*:  Desc.       : Account types
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 09/27/2012
*:  Reference   : E303233,1
*:************************************************************************
*:************************************************************************

*** Report Setup

* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables
gcUser_Id = oAriaApplication.User_ID
gcCom_Name = oAriaApplication.ActiveCompanyName
gdSysDate = oAriaApplication.SystemDate
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables

SELECT GLTYPES
*- Data collection
=lfCollect()
SELECT (lcTmpFile)

DO gfDispRe WITH EVAL('lcRpForm')


*!************************************************************************
*!
*!      FUNCTION lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep


*!************************************************************************
*!
*!      FUNCTION lfGetPic
*!
*!************************************************************************
*
FUNCTION lfGetPic

PRIVATE llAccod
llAccod = .F.

lnOldAlias = SELECT()

IF !USED('ACCOD')
  SELECT 0
    gcDataDir = oAriaApplication.DataDir
  USE &gcDataDir.ACCOD
  llAccod = .T.
ENDIF
SELECT ACCOD
GO TOP


IF !EOF()
  SKIP
  
  *lnRpSegTall = SYCACCOD.Nacssize
  lnRpSegTall = ACCOD.Nacssize
  
  
  SELECT (lnOldAlias)
  lcRpSegPic = 'X'+REPLICATE('9',lnRpSegTall-1)
  RETURN lcRpSegPic
ELSE
  lnRpSegTall = 0
  SELECT (lnOldAlias)
  lcRpSegPic = ""
  RETURN lcRpSegPic
ENDIF

IF llAccod
  USE IN ACCOD
ENDIF

*!************************************************************************
*!
*!      FUNCTION lfvAcCode
*!
*!************************************************************************
*
FUNCTION lfvAcCode

loFld = _Screen.ActiveForm.ActiveControl
IF loFld.Value == loFld.OldValue OR EMPTY(loFld.Value)
  RETURN 
ENDIF   

DECLARE laRpRetFld(1)
lcOldAlias    = ALIAS()
lcBrFields    = [CTYPECODE:H="Type code",CtypeDesc:H="Type description"]+;
                [,ctyplacno:H="Account lower lim."]
laRpRetFld[1] = ''
lcRpCurFld = 'loFld.Value'
IF !USED("GLSEGVAL")
  USE GLSEGVAL
ENDIF

IF !EMPTY(ALLTRIM(&lcRpCurFld.))
  IF '?' $ &lcRpCurFld. .OR. VAL(&lcRpCurFld.)=0
    SELECT GLtypes
    SET ORDER TO TAG typlacno
    =gfBrows(.f.,'Ctyplacno',"laRpRetFld",' Types ',.F.)
    &lcRpCurFld = laRpRetFld[1]
  ENDIF
ENDIF
SET ORDER TO
SELECT (lcOldAlias)

************************************************************
*! Name      : lfRepWhen
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/30/2012
*! Purpose   : When Function
************************************************************
FUNCTION lfRepWhen
SET PROCEDURE TO (loOgScroll.gcRepHome + loOgScroll.gcAct_Appl + '\glrepfnc.fxp') ADDITIVE 

*- End of lfRepWhen.