*:************************************************************************
*:  Program File: \ARIA4XP\REPORTS\GL\GLUNPREC.PRG
*:  Module      : General Ledger
*:  Desc.       : Account reconciliation - Unposted entries
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 09/27/2012
*:  Reference   : *E303264,1
*:************************************************************************
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
#include r:\aria4xp\reports\gl\glunprec.h
*N000682,1 MMT 02/11/2013 Globalization changes[End]
*** Report Setup

* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables
gcUser_Id = oAriaApplication.User_ID
gcCom_Name = oAriaApplication.ActiveCompanyName
gdSysDate = oAriaApplication.SystemDate
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables

SELECT GLTRNSDT
*--- To optomize the speed in case you selected 1 account
lcRest = IIF("GLTRNSDT.CACCTCODE" $ UPPER(lcRpExp) ,;
              ' REST WHILE GLTRNSDT.CACCTCODE = "'+laOGFxFlt[3,6]+'"','')
SEEK ALLTRIM(laOGFxFlt[3,6])

*--- If the user selected 1 of the apparel module
*--- Because the AAS updates the source module in this case with AS
lcRpExp = STRTRAN(lcRpExp,'"ZA"','"AS"')
lcRpExp = STRTRAN(lcRpExp,'"ZM"','"AS"')
lcRpExp = STRTRAN(lcRpExp,'"ZR"','"AS"')
lcRpExp = STRTRAN(lcRpExp,'"ZB"','"AS"')
lcRpExp = STRTRAN(lcRpExp,'"ZS"','"AS"')

*- Data Collection
=lfCollect()

SELECT (lcTmpFile)
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
  LOCAL laStru[1,18],lnLen,i
  AFIELDS(laStru)
  lnLen = ALEN(laStru,1)
  DIMENSION laStru[lnLen+3,18]

  *- insert more three fields after the account field
  FOR i=1 TO 3
    AINS(laStru,4)
  ENDFOR

  lnI = 3
  lnI = lnI + 1
  laStru[lnI,1] = 'caccnldes'
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 65
  laStru[lnI,4] = 0

  lnI = lnI + 1
  laStru[lnI,1] = 'CSRC'
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 2
  laStru[lnI,4] = 0

  lnI = lnI + 1
  laStru[lnI,1] = 'CCOMP_ID'
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 2
  laStru[lnI,4] = 0

  FOR i= 4 TO lnI
    STORE .F. TO laStru[i,5],laStru[i,6]
    STORE '' TO laStru[i,7],laStru[i,8],laStru[i,9],laStru[i,10],laStru[i,11],laStru[i,12],laStru[i,13],laStru[i,14],laStru[i,15],laStru[i,16]
    STORE 0 TO laStru[i,17],laStru[i,18]
  ENDFOR
  CREATE TABLE (oAriaApplication.WorkDir+lcTmpFile) FROM ARRAY laStru
ENDIF

SELECT &lcFile
lcRel = SET("Relation")
SELECT (lcTmpFile)
SET RELATION TO &lcRel

SELECT &lcFile
lcFor = IIF(EMPTY(lcRpExp),'',' FOR ' +lcRpExp)
SCAN &lcRest &lcFor
  SCATTER MEMVAR
  *N000682,1 MMT 02/11/2013 Globalization changes[Start]
  *m.cbatchno = IIF(gltrnsdt.cbatchno='000000','SINGLE',gltrnsdt.cbatchno)
  m.cbatchno = IIF(gltrnsdt.cbatchno='000000',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SINGLE,oAriaApplication.GetHeaderText("LANG_SINGLE",AHEADERFILE)),gltrnsdt.cbatchno)
  *N000682,1 MMT 02/11/2013 Globalization changes[End]
  m.CSRC = IIF((gltrnsdt.cbatchno='000000' .OR. glbatch.cbattype<>'L') .AND.( !EMPTY(gltrnshd.csrcjrnl)),gltrnshd.csrcjrnl,gltrnshd.csrcmodul)
  m.ccomp_id = glbatch.ccomp_id
  m.caccnldes = glacchar.caccnldes
  INSERT INTO (lcTmpFile) FROM MEMVAR
ENDSCAN

SELECT (lnSlct)
*- End of lfCollect.

*!************************************************************************
*!
*!      Function lfvFisYer
*!
*!************************************************************************
*

FUNCTION lfvFisYer

DECLARE laRpRetFld(1)
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
*lcBrFields    = 'CFisFYear:H="Year",DFisBgDat:H="Begin date",DFisEnDat:H="End date"'
lcBrFields    = 'CFisFYear:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_YEAR,oAriaApplication.GetHeaderText("LANG_YEAR",AHEADERFILE))+;
                '",DFisBgDat:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BEGINDATE,oAriaApplication.GetHeaderText("LANG_BEGINDATE",AHEADERFILE))+;
'",DFisEnDat:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ENDDATE,oAriaApplication.GetHeaderText("LANG_ENDDATE",AHEADERFILE))+'"'
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
laRpRetFld[1] = ''

lcRpFicsYr = loOgScroll.ActiveControl.Value

lcRpCurFld = loOgScroll.ActiveControl.Parent.cOgArray
IF .NOT.EMPTY(lcRpFicsYr)
  lcOldAlias = SELECT()    && Save the current alias
  llUesdBefo = .F.        && Check if used before or this the first time

  IF NOT USED("FISHD")
    SELECT 0
    gcDataDir = oAriaApplication.DataDir
    USE (gcDataDir+'FISHD') ORDER TAG compfyear
    llUesdBefo = .T.
  ENDIF
  SELECT FISHD


  *** Search for the current company+year
    IF ('?' $ &lcRpCurFld. .OR. !SEEK(lcRpFicsYr))
      *N000682,1 MMT 02/11/2013 Globalization changes[Start]
      *=gfBrows('','CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)
     =gfBrows('','CFisFyear',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TRANCODE,oAriaApplication.GetHeaderText("LANG_TRANCODE",AHEADERFILE)),.F.)
      *N000682,1 MMT 02/11/2013 Globalization changes[End]
      &lcRpCurFld. = laRpRetFld[1]
      lcRpFicsYr   = laRpRetFld[1]
    ENDIF

  IF llUesdBefo       && .F.- this file used by the system

    USE IN FISHD

  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN

*!************************************************************************
*!
*!      FUNCTION lfGetPic
*!
*!************************************************************************
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

*!************************************************************************
*!
*!      FUNCTION : lfvActBy
*!
*!************************************************************************
*
FUNCTION lfvActBy
SET PROCEDURE TO (loOgScroll.gcRepHome + loOgScroll.gcAct_Appl + '\glrepfnc.fxp') ADDITIVE

IF !EMPTY(LAOGFXFLT[1,6]) AND !EMPTY(LAOGFXFLT[2,6])
  RETURN
ENDIF

lnCurAlias = SELECT()
llCompUsd=.f.
IF !USED('SYCCOMP')
  USE &gcSyshome.SYCCOMP IN SELECT(1)
  llCompUsd=.T.
ENDIF

SET ORDER TO TAG CCOMP_ID IN SYCCOMP
IF SEEK(oAriaApplication.ActiveCompanyID,'SYCCOMP')
 lcRetVal=SYCCOMP.CCURR_YER+SYCCOMP.CCURR_PRD
ENDIF

IF llCompUsd
  USE IN SYCCOMP
ENDIF

loOgScroll.LAOGFXFLT[1,6] = LEFT(lcRetVal,4)
loOgScroll.LAOGFXFLT[2,6] = RIGHT(lcRetVal,2)+loOgScroll.lcElmSep+RIGHT(lcRetVal,2)

SELECT (lnCurAlias)
************************************************************
*! Name      : lfClearRep
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/11/2012
*! Purpose   : Clear Rep , do not remove
************************************************************
FUNCTION lfClearRep

*- End of lfClearRep.
