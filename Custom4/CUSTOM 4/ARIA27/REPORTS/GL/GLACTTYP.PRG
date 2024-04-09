*:************************************************************************
*:
*: Procedure file: GLACTTYP.RPR
*:               : Account types
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Albert Raif
*:      Copyright (c) 
*:  Last modified:29/03/1994
*:
*:  Procs & Fncts: 
*:               
*:      Documented   /  /
*:************************************************************************
*
*B600339,1 Reham On 05/28/95 "Added By" new object in Option Grid
*E301077,72 WALID File opening

*** Report Setup

SELECT GLTYPES
DO gfDispRe WITH EVAL('lcRpForm'),IIF(!EMPTY(ALLTRIM(lcRpExp)),"FOR ",'')+lcRpExp 

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
*!      FUNCTION lfvTypeCode
*!
*!************************************************************************
*
FUNCTION lfvTypeCode

DECLARE laRpRetFld(1)
IF EMPTY(EVAL(VARREAD()))
  RETURN
ENDIF
lcOldBrFld    = lcBrFields
lcBrFields    = 'CTypeCode:H="Code",CTypeDesc:H="Description"'
laRpRetFld[1] = ''
lcRpCurFld      = VARREAD()
SELECT GLTypes
SET ORDER TO TAG typecode
IF !SEEK(EVAL(lcRpCurFld)) AND LASTKEY()=13
  =gfBrows([""],'CTypeCode',"laRpRetFld",'Types File',.F.)
  &lcRpCurFld = laRpRetFld[1]
ELSE
  IF !SEEK(EVAL(lcRpCurFld))    
    &lcRpCurFld=""
  ENDIF
ENDIF
SHOW GET (lcRpCurFld)
lcBrFields = lcOldBrFld
SET ORDER TO

*!************************************************************************
*!
*!      FUNCTION lfGetPic
*!
*!************************************************************************
*
FUNCTION lfGetPic

*E301077,72 begin
PRIVATE llAccod
llAccod = .F.
*E301077,72 end

lnOldAlias = SELECT()

*HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]

*IF !USED('SYCACCOD')
*  SELECT 0
*  USE &gcSysHome.SYCACCOD ORDER TAG COMPID
*ENDIF
*SELECT SYCACCOD

*E300789,6 Adjust account code file index [Begin
IF !USED('ACCOD')
  SELECT 0
  *E USE &gcDataDir.ACCOD ORDER TAG COMPID
  USE &gcDataDir.ACCOD
  llAccod = .T.
ENDIF
SELECT ACCOD
*E 
GO TOP
*HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]

*E IF SEEK(gcAct_Comp)
IF !EOF()
*E 
  SKIP
  
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]
  
  *lnRpSegTall = SYCACCOD.Nacssize
  lnRpSegTall = ACCOD.Nacssize
  
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]
  
  SELECT (lnOldAlias)
  lcRpSegPic = 'X'+REPLICATE('9',lnRpSegTall-1)
  RETURN lcRpSegPic
ELSE
  lnRpSegTall = 0
  SELECT (lnOldAlias)
  lcRpSegPic = ""
  RETURN lcRpSegPic
ENDIF

*E
IF llAccod
  USE IN ACCOD
ENDIF

*E300789,6 Adjust account code file index [End

*!************************************************************************
*!
*!      FUNCTION lfvAcCode
*!
*!************************************************************************
*
FUNCTION lfvAcCode

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcOldAlias    = ALIAS()
lcBrFields    = [CTYPECODE:H="Type code",CtypeDesc:H="Type description"]+;
                [,ctyplacno:H="Account lower lim."]
laRpRetFld[1] = ''
lcRpCurFld      = VARREAD()
IF !USED("GLSEGVAL")
  USE GLSEGVAL
ENDIF

IF !EMPTY(ALLTRIM(&lcRpCurFld.))
  IF '?' $ &lcRpCurFld. .OR. VAL(&lcRpCurFld.)=0
    SELECT GLtypes
    SET ORDER TO TAG typlacno
    =gfBrows(.f.,'Ctyplacno',"laRpRetFld",' Types ',.F.)
    &lcRpCurFld = laRpRetFld[1]
    SHOW GET (lcRpCurFld)
  ENDIF
ENDIF
lcBrFields = lcOldBrFld
SET ORDER TO
SELECT (lcOldAlias)

