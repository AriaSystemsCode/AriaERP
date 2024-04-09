*:************************************************************************
*:  Program File: \ARIA4XP\REPORTS\GL\GLCASHFL.PRG
*:  Module      : General Ledger
*:  Desc.       : Convert the Cash Flow Code report to A4xp
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 09/03/2012
*:  Reference   : E303237,1
*:************************************************************************
*** Report Setup

* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables
gcUser_Id = oAriaApplication.User_ID
gcCom_Name = oAriaApplication.ActiveCompanyName
gdSysDate = oAriaApplication.SystemDate
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables

SELECT GLCFITEM
SET ORDER TO TAG CFICODE

GO TOP
IF RECNO() < RECCOUNT()
  SKIP 2
ENDIF  

=lfCollect()
SELECT (lcTmpFIle)
DO gfDispRe WITH EVAL('lcRpForm')

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

IF USED(lcTmpFile)
  SELECT (lcTmpFile)
  ZAP
ELSE 
  SELECT &lcFile
  AFIELDS(laStru)
  CREATE TABLE (oAriaApplication.WorkDir+lcTmpFile) FROM ARRAY laStru
  
ENDIF

SELECT &lcFile
lcRel = SET("Relation")
SELECT (lcTmpFile)
SET RELATION TO &lcRel


SELECT &lcFile
LOCATE 
IF RECNO() < RECCOUNT()
  SKIP 2
ENDIF  

lcFor = IIF(!EMPTY(ALLTRIM(lcRpExp)),'FOR ','')+lcRpExp
SCAN REST &lcFor
  SCATTER MEMVAR 
  INSERT INTO (lcTmpFile) FROM MEMVAR 
ENDSCAN 
  
SELECT (lnSlct)
*- End of lfCollect.

*!************************************************************************
*!
*!      FUNCTION lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep

************************************************************
*! Name      : lfRepWhen
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/30/2012
*! Purpose   : When Function
************************************************************
FUNCTION lfRepWhen
SET PROCEDURE TO (loOgScroll.gcRepHome + loOgScroll.gcAct_Appl + '\glrepfnc.fxp') ADDITIVE 

*- End of lfRepWhen.
