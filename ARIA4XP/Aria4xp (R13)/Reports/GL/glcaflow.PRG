*:************************************************************************
*:  Program File: \ARIA4XP\REPORTS\GL\GLCAFLOW.PRG
*:  Module      : General Ledger
*:  Desc.       : Statment of cash flow
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 09/27/2012
*:  Reference   : E303237,1
*:************************************************************************
*:************************************************************************
* N000682 ,1 Thabet Handle globalization issues [Start]
#INCLUDE r:\aria4xp\reports\gl\glcaflow.H
* N000682 ,1 Thabet Handle globalization issues [END]
*** Report Setup

* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables
gcUser_Id = oAriaApplication.User_ID
gcCom_Name = oAriaApplication.ActiveCompanyName
gdSysDate = oAriaApplication.SystemDate
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables

IF (EMPTY(ALLTRIM(laOGFxFlt[1,6])) .OR. EMPTY(ALLTRIM(laOGFxFlt[2,6])))
  ** NO recoeds hove been collected
  =gfModalGen("INM00052B00000","DIALOG")
  RETURN
ENDIF

&& Get slected files name
lcRpFiles = "GLCFITEM,GLACCHAR,GLACBALS"
***   Get the field order ****

lnOldAlias = SELECT()

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


  * N000682 ,1 Thabet Handle globalization issues [Start]
  *  WAIT 'Collecting data...' WINDOW NOWAIT
  WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Collecting_data,oAriaApplication.GetHeaderText("LANG_Collecting_data",AHEADERFILE)) WINDOW NOWAIT
  * N000682 ,1 Thabet Handle globalization issues [END]

  *** Create select  statment and
  *** Select data from file(s)
  SELECT  &lcRpFields.;
   FROM  &lcRpFiles. ;
   WHERE  &lcRpExp.  .AND. lfWaitMsg();
   &lcRpOrder.;
   INTO CURSOR (lcRpTargt)

  *** Restore all enviroment
  WAIT CLEAR
  SET TALK OFF
  ON ESCAPE  &lcSaveOnEs
  SET ESCAPE &lcSaveEsc

  *** Display the report to screen , file or printer
  *** and check if there is any record or not
  IF _TALLY = 0        && No records collected
    ** NO recoeds hove been collected
    =gfModalGen("INM00052B00000","DIALOG")
  ELSE
    STORE 0 TO lnTotoBal,lnTotcBal
    lnNetInc=lfGetNet()
    =lfGetCBal()
    DO gfDispRe WITH EVAL('lcRpForm')
  ENDIF
ELSE
  STORE 0 TO lnTotoBal,lnTotcBal
  lnNetInc=lfGetNet()
  =lfGetCBal()
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
*!      Function lfvFisYer
*!
*!************************************************************************
*
****  Check if current company has this entried year or not
FUNCTION lfvFisYer

loFld = loOgScroll.ActiveControl
lcRpCurFld = loOgScroll.ActiveControl.Parent.oItem.cAssociate
IF loFld.Value == loFld.OldValue OR EMPTY(loFld.Value)
  *- empty the period ID if the year is empty
  IF EMPTY(&lcRpCurFld.)
    lcArr = loOgScroll.ActiveControl.Parent.cOgArray
    lnPos = lfNav2GetPos(lcArr,'CFSPPRDID')
    IF lnPos>0
      lcRpPrdFld = lcArr + '['+ALLTRIM(STR(lnPos))+',6]'
      loOgScroll.&lcRpPrdFld. = ''
    ENDIF
  ENDIF

  RETURN
ENDIF


DECLARE laRpRetFld(1)
* N000682 ,1 Thabet Handle globalization issues [Start]
*lcBrFields    = 'CFisFYear:H="Year",DFisBgDat:H="Begin date",DFisEnDat:H="End date"'
lcBrFields    = 'CFisFYear:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_YEAR,oAriaApplication.GetHeaderText("LANG_YEAR",AHEADERFILE))+;
                '",DFisBgDat:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BEGDATE,oAriaApplication.GetHeaderText("LANG_BEGDATE",AHEADERFILE))+;
				'",DFisEnDat:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ENDDATE,oAriaApplication.GetHeaderText("LANG_ENDDATE",AHEADERFILE))+'"'
* N000682 ,1 Thabet Handle globalization issues [END]
laRpRetFld[1] = ''
&& Check If year field is empty
lcRpFicsYr  = loOgScroll.ActiveControl.Value

IF .NOT.EMPTY(lcRpFicsYr)
  lcOldAlias = SELECT()    && Save the current alias
  llUesdBefo = .F.        && Check if used before or this the first time

  IF NOT USED("FISHD")
    SELECT 0
    gcDataDir = oAriaApplication.DataDir
    USE &gcDataDir.FISHD ORDER TAG compfyear
    llUesdBefo = .T.
  ENDIF
  SELECT FISHD

  *** Search for the current company+year
  IF ('?' $ &lcRpCurFld. .OR. !SEEK(lcRpFicsYr))
    * N000682 ,1 Thabet Handle globalization issues [Start]
    *=gfBrows('','CFisFyear',"laRpRetFld",'Fiscal year ',.F.)
	=gfBrows('','CFisFyear',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_FISYEAR,oAriaApplication.GetHeaderText("LANG_FISYEAR",AHEADERFILE)),.F.)
    * N000682 ,1 Thabet Handle globalization issues [END]
    IF EMPTY(laRpRetFld[1])
      laRpRetFld[1] = loFld.OldValue
    ENDIF
    loOgScroll.&lcRpCurFld. = laRpRetFld[1]
  ENDIF

  IF llUesdBefo       && .F.- this file used by the system

    USE IN FISHD

  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN

*!************************************************************************
*!
*!      FUNCTION lfGetNet
*!
*!************************************************************************
*
FUNCTION lfGetNet

lcOldAlias = SELECT()
lnRecNo = RECNO()
DIMENSION laResult(1)
laResult[1] = 0
SELECT SUM(IIF(lcRpRepFor='PTD',NACBPTDCR-NACBPTDDR,NACBPTDCR-NACBPTDDR-NACBOPBAL));
  FROM GLACCHAR,GLACBALS;
  WHERE GLACBALS.CACCTCODE+GLACBALS.CFISFYEAR+GLACBALS.CFSPPRDID =;
        GLACCHAR.CACCTCODE+laOGFxFlt[1,6]+laOGFxFlt[2,6];
        AND LEFT(GLACCHAR.CTYPECODE,1) $ 'SETIC';
  INTO ARRAY laResult


SELECT(lcOldAlias)
IF !EMPTY(ALIAS(lcOldAlias))
  GO lnRecNo
ENDIF
RETURN laResult[1]

*!************************************************************************
*!
*!      FUNCTION lfGetCBal
*!
*!************************************************************************
*
FUNCTION lfGetCBal

    gcDataDir = oAriaApplication.DataDir
lcOldAlias = SELECT()
lnRecNo = RECNO()
DIMENSION laResult[1,2]
STORE 0 TO laResult
IF lcRpRepFor='PTD'
  SELECT SUM(GLACBALS.NACBOPBAL),;
         SUM(GLACBALS.NACBOPBAL+GLACBALS.NACBPTDDR-GLACBALS.NACBPTDCR);
     FROM &gcDataDir.GLACBALS,&gcDataDir.GLACCHAR;
     WHERE GLACCHAR.CSEGCAFLO='C00' AND;
           GLACBALS.CACCTCODE+GLACBALS.CFISFYEAR+GLACBALS.CFSPPRDID=;
           GLACCHAR.CACCTCODE+laOGFxFlt[1,6]+laOGFxFlt[2,6];
           INTO ARRAY laResult


  lnTotoBal=laResult[1,1]
  lnTotcBal=laResult[1,2]

ELSE

  SELECT SUM(GLACBALS.NACBOPBAL);
     FROM &gcDataDir.GLACBALS,&gcDataDir.GLACCHAR;
     WHERE GLACCHAR.CSEGCAFLO='C00' AND;
           GLACBALS.CACCTCODE+GLACBALS.CFISFYEAR+GLACBALS.CFSPPRDID=;
           GLACCHAR.CACCTCODE+laOGFxFlt[1,6]+'01';
           INTO ARRAY laResult

  lnTotoBal=laResult[1]

  SELECT SUM(GLACBALS.NACBOPBAL+GLACBALS.NACBPTDDR-GLACBALS.NACBPTDCR);
     FROM &gcDataDir.GLACBALS,&gcDataDir.GLACCHAR;
     WHERE GLACCHAR.CSEGCAFLO='C00' AND;
           GLACBALS.CACCTCODE+GLACBALS.CFISFYEAR+GLACBALS.CFSPPRDID=;
           GLACCHAR.CACCTCODE+laOGFxFlt[1,6]+laOGFxFlt[2,6];
           INTO ARRAY laResult


  lnTotcBal=laResult[1]

ENDIF
SELECT(lcOldAlias)
IF !EMPTY(ALIAS(lcOldAlias))
 GO lnRecNo
ENDIF
RETURN


************************************************************
*! Name      : lfRepWhen
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/01/2012
*! Purpose   : When function
************************************************************
FUNCTION lfRepWhen
SET PROCEDURE TO (loOgScroll.gcRepHome + loOgScroll.gcAct_Appl + '\glrepfnc.fxp') ADDITIVE

*- End of lfRepWhen.
