*:************************************************************************
*:  Program File: \ARIA4XP\REPORTS\GL\GLAUTALT.PRG
*:  Module      : General Ledger
*:  Desc.       : Automatic allocations
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 09/27/2012
*:  Reference   : E303234,1
*:************************************************************************
*B610132,1 [T20121023.0022] TMI 10/24/2012 remove the calling to the function lfChangeGrid
*:************************************************************************
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
#INCLUDE r:\aria4xp\reports\gl\glautalt.H
*N000682,1 MMT 02/10/2013 Globalization changes[End]
*** Report Setup

* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables
gcUser_Id = oAriaApplication.User_ID
gcCom_Name = oAriaApplication.ActiveCompanyName
gdSysDate = oAriaApplication.SystemDate
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables

lcRpFiles  = "GLAUTDT,GLAUTHD,GLACCHAR"  && Get slected files name

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
  *N000682,1 MMT 02/10/2013 Globalization changes[Start]
  *WAIT 'Collecting data...' WINDOW NOWAIT
  WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COLLDATA,oAriaApplication.GetHeaderText("LANG_COLLDATA",AHEADERFILE)) WINDOW NOWAIT
  *N000682,1 MMT 02/10/2013 Globalization changes[End]
  lcRpFields=IIF(EMPTY(lcRpFields),"*",lcRpFields)
  ***   Create select  statment
  SELECT   DISTINCT &lcRpFields;
    FROM  &lcRpFiles ;
    WHERE  &lcRpExp  .AND. lfWaitMsg();
     &lcRpOrder.;
    INTO CURSOR &lcRpTargt

  *** Restore all enviroment
  WAIT CLEAR
  SET TALK OFF
  ON ESCAPE  &lcSaveOnEs
  SET ESCAPE &lcSaveEsc

  *** Display the report to screen , file or printer
  *** and check if there is any record or not
  *** before that check if press Escape or not
  IF _TALLY = 0        && No records collected
    *** NO recoeds hove been collected
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
*!      FUNCTION lfChngCond
*!
*!************************************************************************
*
***  Function to switch between two FRX reports
FUNCTION lfChngCond

llOGFilter=IIF(lcRpForm="GLAUTALD",.F.,.T.)
DO CASE
  CASE lcRpForm =   "GLAUTALD"
*B610132,1 [T20121023.0022] TMI 10/24/2012 [Start]
*      =lfChangeGrid('GLAUTAL2')
*B610132,1 [T20121023.0022] TMI 10/24/2012 [End  ]
  CASE lcRpForm =   "GLAUTALS"
*B610132,1 [T20121023.0022] TMI 10/24/2012 [Start]
*      =lfChangeGrid('GLAUTALT')
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
*!      FUNCTION lfvAutCode
*!
*!************************************************************************
*
FUNCTION lfvAutCode

loFld = loOgScroll.ActiveControl
IF loFld.Value == loFld.OldValue OR EMPTY(loFld.Value)
  RETURN
ENDIF

DECLARE laRpRetFld(1)
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
*lcBrFields    = 'CAutCode:H="Code",CAutDes:H="Description"'
lcBrFields    = 'CAutCode:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CODE,oAriaApplication.GetHeaderText("LANG_CODE",AHEADERFILE))+;
				'",CAutDes:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DESC,oAriaApplication.GetHeaderText("LANG_DESC",AHEADERFILE))+'"'
*N000682,1 MMT 02/10/2013 Globalization changes[ENd]
laRpRetFld[1] = ''
lcRpCurFld = loOgScroll.ActiveControl.Parent.cOgArray

*-  Get user position in the laOg??Flt
LOCAL i,lnPos
FOR i=1 TO ALEN(&lcRpCurFld,1)
  IF '.CAUTCODE' $ UPPER(PADR(&lcRpCurFld.[i,1],100))
    lnPos = i
    EXIT
  ENDIF
ENDFOR
lcRpCurFld = lcRpCurFld + '['+ALLTRIM(STR(lnPos))+',6]'

SELECT GLAUTHD
SET ORDER TO TAG typecode
IF ('?' $ &lcRpCurFld. .OR. !SEEK('A'+&lcRpCurFld.)) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
  *N000682,1 MMT 02/10/2013 Globalization changes[Start]
  *=gfBrows(["A"],'CAutCode',"laRpRetFld",'Codes File',.F.)
  =gfBrows(["A"],'CAutCode',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TTL,oAriaApplication.GetHeaderText("LANG_TTL",AHEADERFILE)),.F.)
  *N000682,1 MMT 02/10/2013 Globalization changes[END]
  IF EMPTY(laRpRetFld[1])
    laRpRetFld[1] = loFld.OldValue
  ENDIF
  &lcRpCurFld = laRpRetFld[1]
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
