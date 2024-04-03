*E303241,1 This program contains some functions uses so much in the GL reports instead of writing them in each program separetly
*T20141002.0017,task, adding/correcting the main fix # for this entity object TMI 10/12/2014 08:56 [Start] 
*E303250,1 TMI 09/03/2012 Convert the Unposted Transactions report to A4xp
*E303256,1 TMI 09/20/2012 the common files between GLINCSTA,GLBALSHT                  
*T20141002.0017,task, adding/correcting the main fix # for this entity object TMI 10/12/2014 08:56 [End  ] 
*!************************************************************************
*Modifications
*!************************************************************************
*B610176,1 TMI 12/16/2012 [T20121209.0006] if this is the chart of accounts and the account structure has nore than one segment , exit if it is empty
*!************************************************************************
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
#include  r:\aria4xp\reports\gl\glrepfnc.H
*N000682,1 MMT 02/10/2013 Globalization changes[End]


*!************************************************************************
*!
*!      FUNCTION lfvRatGrp
*!
*!************************************************************************
*
FUNCTION lfvRatGrp

loFld = loOgScroll.ActiveControl
IF loFld.Value == loFld.OldValue OR EMPTY(loFld.Value)
  RETURN
ENDIF
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
IF oAriaApplication.oActiveLang.cLang_ID <> 'EN'
  BHeaderFile = oAriaApplication.GetClassHeaderFile(UPPER(ALLTRIM(oAriaApplication.LangPath))+'\REPORTS\GL\GLREPFNC_H.XML')
ENDIF  
*N000682,1 MMT 02/10/2013 Globalization changes[END]
lcRpCurFld = loOgScroll.ActiveControl.Value
SELECT GLRACOD
LOCATE FOR GLRACOD.csegratio == lcRpCurFld
IF !FOUND()
  DECLARE laRpRetFld(1)
  *N000682,1 MMT 02/10/2013 Globalization changes[Start]
  *lcBrFields    = 'CSEGRATIO:H="Ratio group code",CRATDESC:H="Ratio group description"'
  lcBrFields    = 'CSEGRATIO:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_RATIOCODE,oAriaApplication.GetHeaderText("LANG_RATIOCODE",BHeaderFile))+;
	'",CRATDESC:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_RATIOGRP,oAriaApplication.GetHeaderText("LANG_RATIOGRP",BHeaderFile))+'"'
  *N000682,1 MMT 02/10/2013 Globalization changes[END]
  laRpRetFld[1] = ''

  lcRpCurFld = loOgScroll.ActiveControl.Parent.cOgArray

  *-  Get user position in the laOg??Flt
  LOCAL i,lnPos
  FOR i=1 TO ALEN(&lcRpCurFld,1)
    IF '.CSEGRATIO' $ UPPER(PADR(&lcRpCurFld.[i,1],100))
      lnPos = i
      EXIT
    ENDIF
  ENDFOR

  IF !EMPTY(&lcRpCurFld.[lnPos,6])
    SELECT GLRACOD
    LOCATE FOR csegratio = LEFT(&lcRpCurFld.[lnPos,6],1)
    IF !FOUND()
      LOCATE
    ENDIF
    *N000682,1 MMT 02/10/2013 Globalization changes[Start]
    *=gfBrows([],'CSEGRATIO',"laRpRetFld",'Ratio Group File',.F.)
	=gfBrows([],'CSEGRATIO',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_RATIOGRPFILE,oAriaApplication.GetHeaderText("LANG_RATIOGRPFILE",BHeaderFile)),.F.)
    *N000682,1 MMT 02/10/2013 Globalization changes[End]
    IF EMPTY(laRpRetFld[1])
      laRpRetFld[1] = loFld.OldValue
    ENDIF
    loOgScroll.&lcRpCurFld.[lnPos,6] = laRpRetFld[1]
  ENDIF
ENDIF

SELECT GLACCHAR


************************************************************
*! Name      : lfvUserID
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/30/2012
*! Purpose   : Get user ID
************************************************************
FUNCTION lfvUserID
*GFVLFLD('SYUUSER','CUser_ID',VARREAD(),'',.F.,.F.,[CUser_ID:H="User ID",CUsr_name:H="User name"],'1',.F.,.t.)
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
*lcBrFields    = [CUser_ID:H="User ID",CUsr_name:H="User name"]
*lfGetBrow('CADD_USER','SYUUSER','CUSER_ID','Users','CUSER_ID')
IF oAriaApplication.oActiveLang.cLang_ID <> 'EN'
  BHeaderFile = oAriaApplication.GetClassHeaderFile(UPPER(ALLTRIM(oAriaApplication.LangPath))+'\REPORTS\GL\GLREPFNC_H.XML')
ENDIF  
lcBrFields    = [CUser_ID:H="]+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_USERID,oAriaApplication.GetHeaderText("LANG_USERID",BHeaderFile))+[",CUsr_name:H="]+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_USERNAME,oAriaApplication.GetHeaderText("LANG_USERNAME",BHeaderFile))+["]
lfGetBrow('CADD_USER','SYUUSER','CUSER_ID',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_USERS,oAriaApplication.GetHeaderText("LANG_USERS",BHeaderFile)) ,'CUSER_ID')
*N000682,1 MMT 02/10/2013 Globalization changes[End]


*- End of FUNCTION lfvUserID

************************************************************
*! Name      : lfvCashFl
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/30/2012
*! Purpose   : Get Cash flow id
************************************************************
FUNCTION lfvCashFl
*GFVLFLD('GLCFITEM','CSEGCAFLO',VARREAD(),IIF(EMPTY(EVAL(VARREAD())),'',lcRpOld),.F.,.F.,[CSEGCAFLO:H="Statement of cash flow Id",CCFIDESC:H="Cash flow discription"],'1',.F.,.t.)
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
*!*	lcBrFields    = [CSEGCAFLO:H="Statement of cash flow Id",CCFIDESC:H="Cash flow Description"]
*!*	lfGetBrow('CSEGCAFLO','GLCFITEM','CFICODE','Cash Flow Items')
IF oAriaApplication.oActiveLang.cLang_ID <> 'EN'
  BHeaderFile = oAriaApplication.GetClassHeaderFile(UPPER(ALLTRIM(oAriaApplication.LangPath))+'\REPORTS\GL\GLREPFNC_H.XML')
ENDIF  
lcBrFields    = [CSEGCAFLO:H="]+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_STATEMENTCSHID,oAriaApplication.GetHeaderText("LANG_STATEMENTCSHID",BHeaderFile)) +[",CCFIDESC:H="]+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CASHFLOWDESC,oAriaApplication.GetHeaderText("LANG_CASHFLOWDESC",BHeaderFile))+["]
lfGetBrow('CSEGCAFLO','GLCFITEM','CFICODE',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CASHFLOWITEM,oAriaApplication.GetHeaderText("LANG_CASHFLOWITEM",BHeaderFile)))
*N000682,1 MMT 02/10/2013 Globalization changes[End]
*- End of FUNCTION lfvUserID



************************************************************
*! Name      : lfGetBrow
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/30/2012
*! Purpose   : Get browse
************************************************************
FUNCTION lfGetBrow
PARAMETERS lcFld,lcBrwFl,lcBrwFlIdx,lcTitle,lcRetFlt,lcKey
lcKey = IIF(EMPTY(lcKey),'',ALLTRIM(lcKey))
LOCAL lnSlct,lcPath
lnSlct = SELECT(0)

loFld = loOgScroll.ActiveControl
IF loFld.Value == loFld.OldValue OR EMPTY(loFld.Value)
  RETURN
ENDIF

lcRpCurFld = loOgScroll.ActiveControl.Parent.cOgArray

*- Get user position in the laOg??Flt
IF UPPER(LEFT(lcRpCurFld,4)) == 'LAOG'
  LOCAL i,lnPos
  FOR i=1 TO ALEN(&lcRpCurFld,1)
    IF '.'+lcFld $ UPPER(PADR(&lcRpCurFld.[i,1],100))
      lnPos = i
      EXIT
    ENDIF
  ENDFOR
  lcRpCurFld = lcRpCurFld + '['+ALLTRIM(STR(lnPos))+',6]'
ENDIF
lcRetFlt = IIF(EMPTY(lcRetFlt),lcFld,lcRetFlt)
DECLARE laRpRetFld(1)
laRpRetFld[1] = ''

llUsed = .F.
IF !USED(lcBrwFl)
  lcPath = IIF(LEFT(lcBrwFl,2)='SY',oAriaApplication.SysPath,oAriaApplication.DataDir)
  =gfOpenFile(lcPath+lcBrwFl,lcBrwFlIdx,'SH')
  llUsed = .T.
ENDIF
SELECT (lcBrwFl)
SET ORDER TO (lcBrwFlIdx)
*IF '?' $ &lcRpCurFld.[lnPos,6] .OR. !SEEK(&lcRpCurFld.[lnPos,6])

*B610176,1 TMI 12/16/2012 [Start] if this is the chart of accounts and the account structure has nore than one segment , exit if it is empty
IF lcBrwFl = 'GLACCHAR'
  IF EMPTY(CHRTRAN(&lcRpCurFld.,'0-',''))
    IF llUsed
      USE IN (lcBrwFl)
    ENDIF
    RETURN
  ENDIF
ENDIF
*B610176,1 TMI 12/16/2012 [End  ]
IF '?' $ &lcRpCurFld. .OR. !SEEK(lcKey+&lcRpCurFld.)
  IF !FOUND() AND BETWEEN(RECNO(0),1,RECCOUNT(ALIAS()))
    GOTO RECNO(0)
  ENDIF
  =gfBrows(["&lcKey"],lcRetFlt,"laRpRetFld",lcTitle,.F.)
  *loOgScroll.&lcRpCurFld.[lnPos,6] = laRpRetFld[1]
  IF EMPTY(laRpRetFld[1])
    laRpRetFld[1] = loFld.OldValue
  ENDIF
  loOgScroll.&lcRpCurFld. = laRpRetFld[1]
  &lcRpCurFld. = laRpRetFld[1]
  *loFld.Value = laRpRetFld[1]
ENDIF

IF llUsed
  USE IN (lcBrwFl)
ENDIF
SELECT (lnSlct)
*- End of lfvUserID.


*!************************************************************************
*!
*!      FUNCTION lfvActCod
*!
*!************************************************************************
FUNCTION lfvActCod
PRIVATE lcOldOrder
lcOldOrder = ''

loFld = loOgScroll.ActiveControl
IF loFld.Value == loFld.OldValue OR EMPTY(loFld.Value)
  RETURN
ENDIF

DECLARE laRpRetFld(1)
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
*lcBrFields    = 'CACCTCODE:H="Account code",CACCNLDES:H="Long report description"'
IF oAriaApplication.oActiveLang.cLang_ID <> 'EN'
  BHeaderFile = oAriaApplication.GetClassHeaderFile(UPPER(ALLTRIM(oAriaApplication.LangPath))+'\REPORTS\GL\GLREPFNC_H.XML')
ENDIF  
lcBrFields    = 'CACCTCODE:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ACCCODE,oAriaApplication.GetHeaderText("LANG_ACCCODE",BHeaderFile)) +;
'",CACCNLDES:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_LONGDESC,oAriaApplication.GetHeaderText("LANG_LONGDESC",BHeaderFile))+'"'
*N000682,1 MMT 02/10/2013 Globalization changes[End]
laRpRetFld[1] = ''
lcRpCurFld = loOgScroll.ActiveControl.Parent.cOgArray
lnPos = lfNav2GetPos(lcRpCurFld,'CACCTCODE')
IF !EMPTY(&lcRpCurFld.[lnPos,6])
  SELECT GLACCHAR
  lcOldOrder = ORDER()
  SET ORDER TO ACCTCODE
  IF "?" $ &lcRpCurFld.[lnPos,6] .OR. !SEEK(&lcRpCurFld.[lnPos,6])
    *N000682,1 MMT 02/10/2013 Globalization changes[Start]
    *=gfBrows([],'CACCTCODE',"laRpRetFld",'Accounts File',.F.)
	=gfBrows([],'CACCTCODE',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ACCFILE,oAriaApplication.GetHeaderText("LANG_ACCFILE",BHeaderFile)),.F.)
    *N000682,1 MMT 02/10/2013 Globalization changes[End]
    loOgScroll.&lcRpCurFld.[lnPos,6] = laRpRetFld[1]
  ENDIF
  SET ORDER TO &lcOldOrder
ENDIF


************************************************************
*! Name      : lfNav2GetPos
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/30/2012
*! Purpose   : navigate the system array to get position
************************************************************
FUNCTION lfNav2GetPos
PARAMETERS lcArr,lcFld
  *-  Get user position in the laOg??Flt
  LOCAL i,lnPos
  FOR i=1 TO ALEN(&lcArr,1)
    IF '.&lcFld' $ UPPER(PADR(&lcArr.[i,1],100))
      lnPos = i
      EXIT
    ENDIF
  ENDFOR
RETURN lnPos
*- End of lfNav2GetPos.


*!************************************************************************
*!
*!      FUNCTION lfvTypeCode
*!
*!************************************************************************
*
FUNCTION lfvTypeCode
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
*!*	lcBrFields    = [CTypeCode:H="Code",CTypeDesc:H="Description"]
*!*	lfGetBrow('CTYPECODE','GLTypes','TYPECODE','Types File')
IF oAriaApplication.oActiveLang.cLang_ID <> 'EN'
  BHeaderFile = oAriaApplication.GetClassHeaderFile(UPPER(ALLTRIM(oAriaApplication.LangPath))+'\REPORTS\GL\GLREPFNC_H.XML')
ENDIF  
lcBrFields    = [CTypeCode:H="]+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CODE,oAriaApplication.GetHeaderText("LANG_CODE",BHeaderFile))+[",CTypeDesc:H="]+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DESC,oAriaApplication.GetHeaderText("LANG_DESC",BHeaderFile))+["]
lfGetBrow('CTYPECODE','GLTypes','TYPECODE',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TYPEFILE,oAriaApplication.GetHeaderText("LANG_TYPEFILE",BHeaderFile)))
*N000682,1 MMT 02/10/2013 Globalization changes[End]
*- End of lfvTypeCode.


*!************************************************************************
*!
*!      FUNCTION lfvSorMod
*!
*!************************************************************************
*
FUNCTION lfvSorMod

loFld = loOgScroll.ActiveControl
IF loFld.Value == loFld.OldValue OR EMPTY(loFld.Value)
  RETURN
ENDIF

PRIVATE llApplFile
llApplFile = .F.

DECLARE laRpRetFld(1)
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
*lcBrFields    = 'Capp_id:H="Modul code",Capp_name:H="Modul name"'
IF oAriaApplication.oActiveLang.cLang_ID <> 'EN'
  BHeaderFile = oAriaApplication.GetClassHeaderFile(UPPER(ALLTRIM(oAriaApplication.LangPath))+'\REPORTS\GL\GLREPFNC_H.XML')
ENDIF
lcBrFields    = 'Capp_id:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MODCODE,oAriaApplication.GetHeaderText("LANG_MODCODE",BHeaderFile))+;
'",Capp_name:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MODNAME,oAriaApplication.GetHeaderText("LANG_MODNAME",BHeaderFile))+'"'
*N000682,1 MMT 02/10/2013 Globalization changes[End]
laRpRetFld[1] = ''
lcRpCurFld = loOgScroll.ActiveControl.Parent.cOgArray
lnPos = lfNav2GetPos(lcRpCurFld,'CSRCMODUL')
IF !USED('SYDAPPL')
  llApplFile = .T.
  SELECT 0
  USE (oAriaApplication.SysPath+'SYDAPPL')
ENDIF

SELECT sydAPPL
SET ORDER TO TAG CAPP_ID

IF !SEEK(&lcRpCurFld.[lnPos,6])
  *N000682,1 MMT 02/10/2013 Globalization changes[Start]
  *=gfBrows([FOR !INLIST(CAPP_ID,'SY','SM')],'CAPP_ID',"laRpRetFld",'Moduls',.F.)
  =gfBrows([FOR !INLIST(CAPP_ID,'SY','SM')],'CAPP_ID',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MODULS,oAriaApplication.GetHeaderText("LANG_MODULS",BHeaderFile)),.F.)
  *N000682,1 MMT 02/10/2013 Globalization changes[End]
  IF EMPTY(laRpRetFld[1])
    laRpRetFld[1] = loFld.OldValue
  ENDIF
  loOgScroll.&lcRpCurFld.[lnPos,6] = laRpRetFld[1]
ELSE
  IF !SEEK(&lcRpCurFld.[lnPos,6])
    loOgScroll.&lcRpCurFld.[lnPos,6]=""
  ENDIF
ENDIF
SET ORDER TO

IF llApplFile
  USE IN SYDAPPL
ENDIF


*!************************************************************************
*!
*!      Function lfvFisPrd
*!
*!************************************************************************
*
****  Check if current company has this entried period or not
FUNCTION lfvFisPrd
loFld = _Screen.ActiveForm.ActiveControl
IF loFld.Value == loFld.OldValue OR EMPTY(loFld.Value)
  RETURN
ENDIF
loFld.Value = PADL(ALLTRIM(loFld.Value),2,'0')

DECLARE laRpRetFld(1)
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
*lcBrFields    = [CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"]
IF oAriaApplication.oActiveLang.cLang_ID <> 'EN'
  BHeaderFile = oAriaApplication.GetClassHeaderFile(UPPER(ALLTRIM(oAriaApplication.LangPath))+'\REPORTS\GL\GLREPFNC_H.XML')
ENDIF
lcBrFields    = [CFisFYear:H="]+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_YEAR,oAriaApplication.GetHeaderText("LANG_YEAR",BHeaderFile ))+;
                [",CFspprdid:H="]+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PERIOD,oAriaApplication.GetHeaderText("LANG_PERIOD",BHeaderFile ))+;
[",CFsppDesc:H="]+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MONTH,oAriaApplication.GetHeaderText("LANG_MONTH",BHeaderFile ))+["]
*N000682,1 MMT 02/10/2013 Globalization changes[End]
laRpRetFld[1] = ''

IF TYPE('loOgScroll.ActiveControl.Parent.cOgArray')='U'
  lcRpCurFld = 'loFld.Value'
ELSE
  lcRpCurFld = loOgScroll.ActiveControl.Parent.oItem.cAssociate
ENDIF

lcRpFicsYr      = laOGFxFlt[1,6]
&& Check If year field is empty
IF .NOT. EMPTY(lcRpFicsYr)
  IF .NOT.EMPTY(ALLTRIM(&lcRpCurFld.))
    lcOldAlias = SELECT()    && Save the current alias
    llUesdBefo = .F.        && Check if used before or this the first time

    IF NOT USED("FSPRD")
      SELECT 0
    gcDataDir = oAriaApplication.DataDir
      USE &gcDataDir.FSPRD ORDER TAG comfyrprdi
      llUesdBefo = .T.
    ENDIF
    SELECT FSPRD

    *** Search for the current company+year+Prd
    IF ('?' $ &lcRpCurFld. .OR.;
      !SEEK(ALLTRIM(lcRpFicsYr)+ALLTRIM(&lcRpCurFld.)))
        *N000682,1 MMT 02/10/2013 Globalization changes[Start]
        *=gfBrows([ALLTRIM(lcRpFicsYr)],'CFsppRdid',"laRpRetFld","Fiscal period",.F.)
		=gfBrows([ALLTRIM(lcRpFicsYr)],'CFsppRdid',"laRpRetFld",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_FISPERIOD,oAriaApplication.GetHeaderText("LANG_FISPERIOD",BHeaderFile )),.F.)
        *N000682,1 MMT 02/10/2013 Globalization changes[End]
        IF EMPTY(laRpRetFld[1])
          laRpRetFld[1] = loFld.OldValue
        ENDIF
        IF TYPE('loOgScroll.ActiveControl.Parent.cOgArray')='U'
          &lcRpCurFld = laRpRetFld[1]
        ELSE
          loOgScroll.&lcRpCurFld = laRpRetFld[1]
        ENDIF

    ENDIF
    IF llUesdBefo       && .F.- this file used by the system

      USE IN FSPRD

    ENDIF
    SELECT (lcOldAlias)
  ENDIF
ELSE
  IF TYPE('loOgScroll.ActiveControl.Parent.cOgArray')='U'
    &lcRpCurFld = "  "
  ELSE
    loOgScroll.&lcRpCurFld = "  "
  ENDIF
ENDIF
RETURN

************************************************************
*! Name      : lfGLSUBJOR
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/30/2012
*! Purpose   : GLSUBJOR
************************************************************
FUNCTION lfGLSUBJOR
*GFVLFLD('GLSUBJOR','CSRCJRNL',VARREAD(),'',.F.,.F.,[CSRCJRNL:H="Source journal entry",CJORLNDES:H="Source journal description"],'1',.F.,.t.)
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
*!*	lcBrFields    = [CSRCJRNL:H="Source journal entry",CJORLNDES:H="Source journal description"]
*!*	lfGetBrow('CSRCJRNL','GLSUBJOR','SRCJRNL','Source Journal Entry')
IF oAriaApplication.oActiveLang.cLang_ID <> 'EN'
  BHeaderFile = oAriaApplication.GetClassHeaderFile(UPPER(ALLTRIM(oAriaApplication.LangPath))+'\REPORTS\GL\GLREPFNC_H.XML')
ENDIF
lcBrFields    = [CSRCJRNL:H="]+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SJENTRY,oAriaApplication.GetHeaderText("LANG_SJENTRY",BHeaderFile))+;
[",CJORLNDES:H="]+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SJDESC,oAriaApplication.GetHeaderText("LANG_SJDESC",BHeaderFile))+["]
lfGetBrow('CSRCJRNL','GLSUBJOR','SRCJRNL',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SJTTL,oAriaApplication.GetHeaderText("LANG_SJTTL",BHeaderFile)))
*N000682,1 MMT 02/10/2013 Globalization changes[End]

*- End of lfGLSUBJOR.
************************************************************
*! Name      : lfBudgCode
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/01/2012
*! Purpose   : Budget Code browse
************************************************************
FUNCTION lfBudgCode
*GFVLFLD('GLBUDHD','CBUDCODE',VARREAD(),'',.F.,.F.,[CBUDCODE:H="Budget code",CBUDDES:H="Budget discription"],'1',.F.,.t.)
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
*!*	lcBrFields    = [CBUDCODE:H="Budget code",CBUDDES:H="Budget discription"]
*!*	lfGetBrow('CBUDCODE','GLBUDHD','BDCODYR','Budget Codes')
IF oAriaApplication.oActiveLang.cLang_ID <> 'EN'
  BHeaderFile = oAriaApplication.GetClassHeaderFile(UPPER(ALLTRIM(oAriaApplication.LangPath))+'\REPORTS\GL\GLREPFNC_H.XML')
ENDIF
lcBrFields    = [CBUDCODE:H="]+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BUDCODE,oAriaApplication.GetHeaderText("LANG_BUDCODE",BHeaderFile))+[",CBUDDES:H="]+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BUGDESC,oAriaApplication.GetHeaderText("LANG_BUGDESC",BHeaderFile))+["]
lfGetBrow('CBUDCODE','GLBUDHD','BDCODYR',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BUDCODETTL,oAriaApplication.GetHeaderText("LANG_BUDCODETTL",BHeaderFile)))
*N000682,1 MMT 02/10/2013 Globalization changes[End]
*- End of lfBudgCode.

*!************************************************************************
*!
*!      Function lfvBudYear
*!
*!************************************************************************
*
****  Check if current company has this entried year or not
FUNCTION lfvBudYear

*GFVLFLD('GLBUDHD','CBUDCODE',VARREAD(),'',.F.,.F.,[CBUDCODE:H="Budget code",CBUDDES:H="Budget discription"],'1',.F.,.t.)
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
*!*	lcBrFields    = [CBUDCODE:H="Budget code",CBUDDES:H="Budget discription"]
*!*	lfGetBrow('CBUDYEAR','GLBUDHD','BDYRCOD','Budget Codes')
IF oAriaApplication.oActiveLang.cLang_ID <> 'EN'
  BHeaderFile = oAriaApplication.GetClassHeaderFile(UPPER(ALLTRIM(oAriaApplication.LangPath))+'\REPORTS\GL\GLREPFNC_H.XML')
ENDIF
lcBrFields    = [CBUDCODE:H="]+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BUDCODE,oAriaApplication.GetHeaderText("LANG_BUDCODE",BHeaderFile))+;
[",CBUDDES:H="]+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BUGDESC,oAriaApplication.GetHeaderText("LANG_BUGDESC",BHeaderFile))+["]
lfGetBrow('CBUDYEAR','GLBUDHD','BDYRCOD',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BUDCODETTL,oAriaApplication.GetHeaderText("LANG_BUDCODETTL",BHeaderFile)))
*N000682,1 MMT 02/10/2013 Globalization changes[End]
*- End of FUNCTION lfvBudYear
************************************************************
*! Name      : lfvPosYer
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/01/2012
*! Purpose   : Posting Year
************************************************************
FUNCTION lfvPosYer
LPARAMETERS lcFld
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
*!*	lcBrFields    = 'CFISFYEAR:H="Year",DFisBgDat:H="Begin date",DFisEnDat:H="End date"'
*!*	lfGetBrow(lcFld,'fishd','COMPFYEAR','Transaction Codes','CFISFYEAR')
IF oAriaApplication.oActiveLang.cLang_ID <> 'EN'
  BHeaderFile = oAriaApplication.GetClassHeaderFile(UPPER(ALLTRIM(oAriaApplication.LangPath))+'\REPORTS\GL\GLREPFNC_H.XML')
ENDIF
lcBrFields    = 'CFISFYEAR:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_YEAR,oAriaApplication.GetHeaderText("LANG_YEAR",BHeaderFile ))+;
                '",DFisBgDat:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BEGINDATE,oAriaApplication.GetHeaderText("LANG_BEGINDATE",BHeaderFile ))+;
'",DFisEnDat:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ENDDATE,oAriaApplication.GetHeaderText("LANG_ENDDATE",BHeaderFile ))+'"'
lfGetBrow(lcFld,'fishd','COMPFYEAR',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TRANCODES,oAriaApplication.GetHeaderText("LANG_TRANCODES",BHeaderFile )),'CFISFYEAR')
*N000682,1 MMT 02/10/2013 Globalization changes[End]
*- if the posting year is empitied then also empty the posting period
LOCAL lcArr,lnPos
lcRpCurFld = loOgScroll.ActiveControl.Parent.oItem.cAssociate
IF EMPTY(&lcRpCurFld.)
  lcArr = loOgScroll.ActiveControl.Parent.cOgArray
  lnPos = lfNav2GetPos(lcArr,'CTRNPPRD')
  IF lnPos>0
    lcRpPrdFld = lcArr + '['+ALLTRIM(STR(lnPos))+',6]'
    loOgScroll.&lcRpPrdFld. = ''
  ENDIF
ENDIF

*- End of lfvPosYer.
************************************************************
*! Name      : lfvPosPrd
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/01/2012
*! Purpose   : Posting Prd
************************************************************
FUNCTION lfvPosPrd
LPARAMETERS lcYearFld
LOCAL lnPos,lcArr,lcKey
lcArr = loOgScroll.ActiveControl.Parent.cOgArray
lnPos = ASUBSCRIPT(&lcArr,ASCAN(&lcArr,lcYearFld),1)
lcKey = ALLTRIM(loOgScroll.&lcArr.[lnPos,6])
IF .NOT. EMPTY(lcKey)
  *N000682,1 MMT 02/10/2013 Globalization changes[Start]
*!*	  lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
*!*	  =lfGetBrow('CTRNPPRD','FSPRD','COMFYRPRDI','Transaction Codes','CFsppRdid',lcKey)
  IF oAriaApplication.oActiveLang.cLang_ID <> 'EN'
    BHeaderFile = oAriaApplication.GetClassHeaderFile(UPPER(ALLTRIM(oAriaApplication.LangPath))+'\REPORTS\GL\GLREPFNC_H.XML')
  ENDIF
  lcBrFields    = 'CFisFYear:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_YEAR,oAriaApplication.GetHeaderText("LANG_YEAR",BHeaderFile))+;
                 '",CFspprdid:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PERIOD,oAriaApplication.GetHeaderText("LANG_PERIOD",BHeaderFile))+;
'",CFsppDesc:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MONTH,oAriaApplication.GetHeaderText("LANG_MONTH",BHeaderFile))+'"'
=lfGetBrow('CTRNPPRD','FSPRD','COMFYRPRDI',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TRANCODES,oAriaApplication.GetHeaderText("LANG_TRANCODES",BHeaderFile)),'CFsppRdid',lcKey)
*N000682,1 MMT 02/10/2013 Globalization changes[End]
ELSE
  lcRpCurFld = loOgScroll.ActiveControl.Parent.oItem.cAssociate
  loOgScroll.&lcRpCurFld. = ''
ENDIF

*- End of lfvPosPrd.

*!************************************************************************
*!
*!      FUNCTION lfWaitMsg
*!
*!************************************************************************
FUNCTION lfWaitMsg
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
*WAIT "Collecting data... " WINDOW NOWAIT
IF oAriaApplication.oActiveLang.cLang_ID <> 'EN'
  BHeaderFile = oAriaApplication.GetClassHeaderFile(UPPER(ALLTRIM(oAriaApplication.LangPath))+'\REPORTS\GL\GLREPFNC_H.XML')
ENDIF
WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COLLDATA,oAriaApplication.GetHeaderText("LANG_COLLDATA",BHeaderFile)) WINDOW NOWAIT
*N000682,1 MMT 02/10/2013 Globalization changes[End]

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
lcFor = IIF(!EMPTY(ALLTRIM(lcRpExp)),'FOR ','')+lcRpExp
SCAN &lcFor
  SCATTER MEMVAR
  INSERT INTO (lcTmpFile) FROM MEMVAR
ENDSCAN

SELECT (lnSlct)
*- End of lfCollect.
