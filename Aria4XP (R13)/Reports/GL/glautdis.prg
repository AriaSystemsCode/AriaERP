*:************************************************************************
*:  Program File: \ARIA4XP\REPORTS\GL\GLAUTDIS.PRG
*:  Module      : General Ledger
*:  Desc.       : Automatic distributions
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 09/27/2012
*:  Reference   : E303235,1
*:************************************************************************
*B610132,1 [T20121023.0022] TMI 10/24/2012 remove the calling to the function lfChangeGrid
*:************************************************************************
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
#INCLUDE r:\aria4xp\reports\gl\glautdis.H
*N000682,1 MMT 02/10/2013 Globalization changes[End]
*** Report Setup

* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables
gcUser_Id = oAriaApplication.User_ID
gcCom_Name = oAriaApplication.ActiveCompanyName
gdSysDate = oAriaApplication.SystemDate
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables

lcRpFiles  = "GLAUTDT,GLAUTHD,GLACCHAR"  && Get slected files name

***   Create select  statment
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
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT LANG_COLLDATA WINDOW NOWAIT
WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COLLDATA,oAriaApplication.GetHeaderText("LANG_COLLDATA",AHEADERFILE)) WINDOW NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 02/10/2013 Globalization changes[End]
  SELECT &lcRpDist   &lcRpFields.;
    FROM  &lcRpFiles. ;
    WHERE  &lcRpExp.  .AND. lfWaitMsg();
    &lcRpOrder.;
    INTO CURSOR (lcRpTargt)

  *** Restore the old setting
  WAIT CLEAR
  SET TALK OFF
  SET ESCAPE &lcSaveEsc
  ON ESCAPE &lcSaveOnEs

  *** Display the report to screen , file or printer
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
*!      Function : lfChngCond
*!
*!************************************************************************
*
***  Function to switch between two FRX reports
FUNCTION lfChngCond

llOGFilter=IIF(lcRpForm="GLAUTDSD",.F.,.T.)

DO CASE
  CASE lcRpForm =   "GLAUTDSD"
*B610132,1 [T20121023.0022] TMI 10/24/2012 [Start]
*       =lfChangeGrid('GLAUTDS2')
*B610132,1 [T20121023.0022] TMI 10/24/2012 [End  ]
  CASE lcRpForm =   "GLAUTDSS"
*B610132,1 [T20121023.0022] TMI 10/24/2012 [Start]
*       =lfChangeGrid('GLAUTDIS')
*B610132,1 [T20121023.0022] TMI 10/24/2012 [End  ]
ENDCASE
ClearRead()

*!************************************************************************
*!
*!      Function : lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep

*!************************************************************************
*!
*!      FUNCTION : lfvAutCode
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
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcBrFields    = 'CAutCode:H="'+LANG_CODE+'",CAutDes:H="'+LANG_DESC+'"'
lcBrFields    = 'CAutCode:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CODE,oAriaApplication.GetHeaderText("LANG_CODE",AHEADERFILE))+'",CAutDes:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DESC,oAriaApplication.GetHeaderText("LANG_DESC",AHEADERFILE))+'"'
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 02/10/2013 Globalization changes[End]
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
IF ('?' $ &lcRpCurFld. .OR. !SEEK('D'+&lcRpCurFld.)) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
  *N000682,1 MMT 02/10/2013 Globalization changes[Start]
  *=gfBrows(["D"],'CAutCode',"laRpRetFld",'Codes File',.F.)
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfBrows(["D"],'CAutCode',"laRpRetFld",LANG_TTL,.F.)
=gfBrows(["D"],'CAutCode',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TTL,oAriaApplication.GetHeaderText("LANG_TTL",AHEADERFILE)),.F.)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 02/10/2013 Globalization changes[END]
  IF EMPTY(laRpRetFld[1])
    laRpRetFld[1] = loFld.OldValue
  ENDIF
  loOgScroll.&lcRpCurFld. = laRpRetFld[1]
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
