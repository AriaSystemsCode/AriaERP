*SYS(18) search this and remove it
*!*************************************************************
*! Name      : APAPROVE.PRG
*! Developer : Hesham Elmasry (HES)
*! Date      : 07/07/2009
*! Tracking  : E302628
*!*************************************************************
*! Example   : Do APAPROVE.PRG
*!*************************************************************
*! Modifications :
*E302628,4 Approved to Pay TMI 09/21/2011 Enable the report to work in A4xp
*E302975,1 AP Conv.Proj. Attaching all files the phase to TMI 10/23/2011
*:************************************************************************
#include r:\aria4xp\reports\ap\apaprove.h
lcSavCurSm = SET('CURRENCY',1)    &&Varible to save the setting of the currency symbol
PRIVATE llCurISopen

SELECT       cCurrCode , cCurrSmbl ;
  FROM       (oAriaApplication.SysPath + 'SYCCURR') ;
  INTO ARRAY laCurrSmbl

IF loOgScroll.llOGFltCh
  lfCrtTemp()
  lfClctData()
ENDIF
SELECT(lcPriTmp)

DO gfDispRe WITH EVAL('lcRpForm')

SET CURRENCY TO lcSavCurSm   && Reset the currency symbol

*!**************************************************************************
*!
*!      Function: lfShowRep
*!
*!**************************************************************************
*
FUNCTION lfShowRep

*!**************************************************************************
*!
*!      Function: lfvInvoice
*!
*!**************************************************************************
FUNCTION lfvInvoice

DO CASE
  CASE lcRpStatus = 'O'
    DIMENSION laOGHDFlt[IIF(!llRpIncInv,3,2),7]
    laOGHDFlt[2,1] = [ABS(APINVHDR.NINVAMNT)]
    laOGHDFlt[2,2] = 'F'
    laOGHDFlt[2,3] = 'N'
    laOGHDFlt[2,4] = .T.
    laOGHDFlt[2,5] = 'Greater Than'
    laOGHDFlt[2,6] = [ABS(APINVHDR.NINVPAID)+ABS(APINVHDR.NINVADJ)+ABS(APINVHDR.NINVDISTK)]
    laOGHDFlt[2,7] = 'E'
    IF !llRpIncInv
      laOGHDFlt[3,1] = [APINVHDR.CINVSTAT]
      laOGHDFlt[3,2] = 'F'
      laOGHDFlt[3,3] = 'C'
      laOGHDFlt[3,4] = .F.
      laOGHDFlt[3,5] = 'Like'
      laOGHDFlt[3,6] = 'V'
      laOGHDFlt[3,7] = 'V'
    ENDIF

  CASE lcRpStatus = 'P'
    llRpIncInv=.F.
    DIMENSION laOGHDFlt[2,7]
    laOGHDFlt[2,1] = [APINVHDR.NINVAMNT]
    laOGHDFlt[2,2] = 'F'
    laOGHDFlt[2,3] = 'N'
    laOGHDFlt[2,4] = .T.
    laOGHDFlt[2,5] = 'Like'
    laOGHDFlt[2,6] = [APINVHDR.NINVPAID+APINVHDR.NINVADJ+APINVHDR.NINVDISTK]
    laOGHDFlt[2,7] = 'E'

  CASE lcRpStatus = 'A'
    DIMENSION laOGHDFlt[IIF(!llRpIncInv,2,1),7]
    IF !llRpIncInv
      laOGHDFlt[2,1] = [APINVHDR.CINVSTAT]
      laOGHDFlt[2,2] = 'F'
      laOGHDFlt[2,3] = 'C'
      laOGHDFlt[2,4] = .F.
      laOGHDFlt[2,5] = 'Like'
      laOGHDFlt[2,6] = 'V'
      laOGHDFlt[2,7] = 'V'
    ENDIF
ENDCASE

*!**************************************************************************
*!
*!      Function: lfBnkCode
*!
*!**************************************************************************
FUNCTION lfBnkCode
PRIVATE lcRetVal

lcRetVal = ' '

lcOldAlias = ALIAS()    && Save the current alias
SELECT APSETUP
lcRetVal   = APSETUP.CBNKCODE

llRpGlLink = IIF(APSETUP.CAPSGLLINK='Y',.T.,.F.)
lcRpActPic = IIF(llRpGlLink,STRTRAN(ALLTRIM(STRTRAN(lcApsAcMas,'#','X',1)),'X','9',2),;
                 ALLTRIM(STRTRAN(lcApsAcMas,'#','9',1)))
IF llRpGlLink
  SELECT SYCCOMP
  =SEEK(oAriaApplication.ActiveCompanyID)
  lcParent   = SYCCOMP.CCOMPPRNT
  IF EMPTY(lcParent)
    lcRpParDir = oAriaApplication.DataDir
  ELSE
    =SEEK(lcParent)
    lcRpParDir = SYCCOMP.CCOM_DDIR
    =SEEK(oAriaApplication.ActiveCompanyID)
  ENDIF
ENDIF

SELECT (lcOldAlias)
IF EMPTY(&lcOGVarName)
  loOgScroll.&lcOGVarName=lcRetVal
ENDIF

RETURN REPLI('X',8)
*!**************************************************************************
*!
*!      Function: lfChkAct
*!
*!**************************************************************************
FUNCTION lfChkAct
PARAMETERS llFirsTime

PRIVATE lcRetVal

lcRetVal = ' '

lcOldAlias = ALIAS()    && Save the current alias

IF llFirsTime
  SELECT APSETUP
  lcRetVal = APSETUP.CCHKACCT
ELSE
  SELECT APCHECKS
  gfSetOrder('BANKCHECK')
  =Seek(laOGFxFlt[2,6])
  lcRetVal = APCHECKS.CCHKACCT
ENDIF

SELECT (lcOldAlias)
IF EMPTY(&lcOGVarName) AND llFirsTime
  &lcOGVarName=lcRetVal
ENDIF
RETURN IIF(llFirsTime,REPL('X',12),lcRetVAl)

*!**************************************************************************
*!
*!      Function: lfGlCkActD
*!
*!**************************************************************************
FUNCTION lfGlCkActD

PARAMETERS llFirsTime
PRIVATE lcRetVal

lcRetVal = ' '
lcRetVal = APSETUP.CCASHACCT

IF EMPTY(&lcOGVarName) AND llFirsTime
  loOgScroll.&lcOGVarName=lcRetVal
ENDIF
RETURN IIF(llFirsTime,lcRpActPic,lcRetVAl)

*!**************************************************************************
*!
*!      Function: lfvBank
*!
*!**************************************************************************
FUNCTION lfvBank

LOCAL loFld
loFld = loOgScroll.ActiveControl

IF loFld.OldValue = loFld.Value
  RETURN
ENDIF

DECLARE laRpRetFld(1)

* N000682 ,1 Thabet Handle globalization issues [Start]
*lcBrFields    = 'CBnkCode:H="Code",CBNKLNDES:H="Description"'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcBrFields    = "CBnkCode:H='"+LANG_CODE+"',CBNKLNDES:H='"+LANG_DESCRIPTION+"'"
lcBrFields    = "CBnkCode:H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CODE,oAriaApplication.GetHeaderText("LANG_CODE",AHEADERFILE))+;
                "',CBNKLNDES:H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DESCRIPTION,oAriaApplication.GetHeaderText("LANG_DESCRIPTION",AHEADERFILE))+"'"
*N000682,1 11/20/2012 MMT Globlization changes[End]

* N000682 ,1 Thabet Handle globalization issues  [END]

laRpRetFld[1] = ''

lcOldAlias = ALIAS()    && Save the current alias
llUesdBefo = .F.        && Check if used before or this the first time

SELECT APBANKS
gfSetOrder('BANKCODE')

  IF loFld.OldValue <> loFld.Value
    *** Search for the current Group code
    IF '?' $ loFld.Value .OR. (!EMPTY(loFld.Value) AND !SEEK(loFld.Value))
      =gfBrows([],'CBnkCode',"laRpRetFld",'Bank Codes ',.F.)
      lnPOS = ASCAN(loOgScroll.laOGFxFlt,"APINVHDR.CBNKCODE")
      IF lnPos > 0
        lnPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      ENDIF
      IF !EMPTY(laRpRetFld[1])
        loFld.Value = laRpRetFld[1]
        loOgScroll.laOGFxFlt[lnPOS,6] = laRpRetFld[1]
      ELSE
        loFld.Value = loFld.OldValue
        loOgScroll.laOgFxFlt[lnPOS,6] = loFld.OldValue
      ENDIF
      loFld.Refresh
    ENDIF

    lnPOS = ASCAN(loOgScroll.laOGFxFlt,"APINVHDR.CCHKACCT")
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      loOgScroll.laOGFxFlt[lnPOS,6] = lfchkact()
    ENDIF
  ENDIF

IF NOT EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF
*!**************************************************************************
*!
*!      Function: lfvChkAct
*!
*!**************************************************************************
FUNCTION lfvChkAct

IF loOgScroll.ActiveControl.OldValue = loOgScroll.ActiveControl.Value
  RETURN
ENDIF
DECLARE laRpRetFld(1)

* N000682 ,1 Thabet Handle globalization issues [Start]
*lcBrFields    = 'CBnkCode:H="Bank Code",CChkAcct:H="Checking account"'
lcBrFields    = 'CBnkCode:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Bank_Code,oAriaApplication.GetHeaderText("LANG_Bank_Code",AHEADERFILE))+;
                '",CChkAcct:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Checking_account,oAriaApplication.GetHeaderText("LANG_Checking_account",AHEADERFILE))+'"'
* N000682 ,1 Thabet Handle globalization issues [END]
laRpRetFld[1] = ''

lcOldAlias = ALIAS()    && Save the current alias
llUesdBefo = .F.        && Check if used before or this the first time

SELECT APCHECKS
gfSetOrder('BANKCHECK')

LOCAL lcCurVal
lcCurVal = loOgScroll.ActiveControl.Value
&& Check If year field is empty
IF loOgScroll.ActiveControl.OldValue <> loOgScroll.ActiveControl.Value
  *** Search for the current Group code
  LOCAL lnPos
  lnPOS = ASCAN(loOgScroll.laOGFxFlt,"APINVHDR.CCHKACCT")
  IF lnPos > 0
    lnPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
  ENDIF
  IF '?' $ loOgScroll.ActiveControl.Value .OR. (!EMPTY(lcCurVal) AND !SEEK(laOGFxFlt[2,6]+lcCurVal))
    lcKey = IIF(EMPTY(laOGFxFlt[2,6]),'','"'+laOGFxFlt[2,6]+'"')
    =gfBrows(lcKey,'CBnkCode,CChkAcct',"laRpRetFld",'Bank & Check Accounts ',.F.)
    IF !EMPTY(laRpRetFld[1]) AND loOgScroll.ActiveControl.Value <> laRpRetFld[2]
      loOgScroll.laOGFxFlt[2,6] = laRpRetFld[1]
      loOgScroll.ActiveControl.Value = laRpRetFld[2]
      loOgScroll.laOGFxFlt[lnPOS,6] = laRpRetFld[2]
    ELSE
      loOgScroll.ActiveControl.Value = loOgScroll.ActiveControl.OldValue
      loOgScroll.laOGFxFlt[lnPOS,6] = loOgScroll.ActiveControl.OldValue
    ENDIF
    loOgScroll.ActiveControl.REFRESH
  ENDIF
ENDIF
IF NOT EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF

*!**************************************************************************
*!
*!      Function: lfGlLink
*!
*!**************************************************************************
FUNCTION lfGlLink

IF !USED('APSETUP')
  =gfOpenTABLE(oAriaApplication.DATADIR+'APSETUP',oAriaApplication.DATADIR+'APSETUP','SH')
  SELECT APSETUP
  Seek('')
ENDIF

IF !USED('ACCOD')
  =gfOpenTABLE(oAriaApplication.DATADIR+'ACCOD',oAriaApplication.DATADIR+'ACCSEGNO','SH')
  SELECT ACCOD
  Seek('')
ENDIF

IF !USED('SYCCOMP')
  =gfOpenTABLE(oAriaApplication.SysPath+'SYCCOMP',oAriaApplication.DATADIR+'CCOMP_ID','SH')
  SELECT SYCCOMP
  Seek('')
ENDIF

SELECT ACCOD
lcApsAcMas = ACCOD.cAcsMask
lcApsAcMas = STRTRAN(lcApsAcMas,'#',IIF(APSETUP.cApsgllink='Y','9','X'))
lcApsAcMas = ALLTRIM("X"+SUBSTR(lcApsAcMas,2))

lcApTmpAlias=ALIAS()
SELECT APSETUP
GO TOP
llRpGlLink = capsgllink = 'Y'
lcRpActPic = IIF(llRpGlLink,STRTRAN(ALLTRIM(STRTRAN(lcApsAcMas,'#','X',1)),'X','9',2),;
                 ALLTRIM(STRTRAN(lcApsAcMas,'#','9',1)))
IF llRpGlLink
  SELECT SYCCOMP
  =SEEK(oAriaApplication.ActiveCompanyID)
  lcParent   = SYCCOMP.CCOMPPRNT
  IF EMPTY(lcParent)
    lcRpParDir = oAriaApplication.DataDir
  ELSE
    =SEEK(lcParent)
    lcRpParDir = SYCCOMP.CCOM_DDIR
    =SEEK(oAriaApplication.ActiveCompanyID)
  ENDIF
ENDIF

SELECT (lcApTmpAlias)

*!**************************************************************************
*!
*!      Function: lfvGlAct
*!
*!**************************************************************************
FUNCTION lfvGlAct

LOCAL loFld
loFld = loOgScroll.ActiveControl
IF loFld.OldValue = loFld.Value
  RETURN
ENDIF

DECLARE laRpRetFld(1)

* N000682 ,1 Thabet Handle globalization issues [Start]
*lcBrFields    = 'CACCTCODE:H="Code",CACCNLDES:H="Description"'
lcBrFields    = 'CACCTCODE:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Code,oAriaApplication.GetHeaderText("LANG_Code",AHEADERFILE))+;
                '" ,CACCNLDES:H="'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Description,oAriaApplication.GetHeaderText("LANG_Description",AHEADERFILE))+'"'
*N000682,1 11/20/2012 MMT Globlization changes[End]

* N000682 ,1 Thabet Handle globalization issues [END]
laRpRetFld[1] = ''
IF llRpGlLink
  lcOldAlias = ALIAS()    && Save the current alias

  SELECT GLACCHAR

  && Check If year field is empty
  IF .NOT. EMPTY(loFld.value)
    *** Search for the current Group code
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,"APINVHDR.CCHKGLACC")
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
    ENDIF
    IF ('?' $ loFld.value .OR.!SEEK(loFld.value))
      =gfBrows([],'CACCTCODE',"laRpRetFld",'Accoonts ',.F.)
      IF !EMPTY(laRpRetFld[1])
        loFld.value = laRpRetFld[1]
        loOgScroll.laOGFxFlt[lnPOS,6] = laRpRetFld[1]
      ELSE
        loFld.value = loFld.oldvalue
        loOgScroll.laOGFxFlt[lnPOS,6] = loFld.oldvalue
      ENDIF
      loFld.Refresh
    ENDIF
  ENDIF

  IF NOT EMPTY(lcOldAlias)
    SELECT (lcOldAlias)
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfvpmethod
*! Developer : Hesham Elmasry(HES)
*! Date      : 07/07/2009
*! Purpose   : Validate the Payment Method Choices
*!*************************************************************
FUNCTION lfvpmethod
LOCAL lcVarName
lcVenPmthd   = lfCheckFilter(1,"APINVHDR.CVENPMETH")

lnBnkCode = ASUBSCRIPT(loOGScroll.laOGFxFlt, ASCAN(laOGFxFlt,'APINVHDR.CBNKCODE'),1)
lnChkAcct = ASUBSCRIPT(loOGScroll.laOGFxFlt, ASCAN(laOGFxFlt,'APINVHDR.CCHKACCT'),1)
lnGLACCNT = ASUBSCRIPT(loOGScroll.laOGFxFlt, ASCAN(laOGFxFlt,'APINVHDR.CCHKGLACC'),1)

lnBnkPOS = ASCAN(loOgScroll.laOGFxFlt,"APINVHDR.CBNKCODE")
IF lnBnkPOS > 0
  lnBnkPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnBnkPOS,1)
ENDIF

lnChAccPOS = ASCAN(loOgScroll.laOGFxFlt,"APINVHDR.CCHKACCT")
IF lnChAccPOS > 0
  lnChAccPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnChAccPOS,1)
ENDIF

lnGlAccPOS = ASCAN(loOgScroll.laOGFxFlt,"APINVHDR.CCHKGLACC")
IF lnGlAccPOS > 0
  lnGlAccPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnGlAccPOS,1)
ENDIF

DO CASE
  CASE lcVenPmthd ='H'
    IF lnBnkPOS > 0
      loOgScroll.laOGFxFlt[lnBnkPOS,6] = ''
    ENDIF

    IF lnChAccPOS > 0
      loOgScroll.laOGFxFlt[lnChAccPOS,6] = ''
    ENDIF

    IF lnGlAccPOS > 0
      loOgScroll.laOGFxFlt[lnGlAccPOS,6] = lfGlCkActD()
    ENDIF

    lcVarName = 'laOgFxFlt[' + ALLTRIM(STR(lnBnkCode)) + ',6]'
    =lfCallShowGet(lcVarName,.F.)

    lcVarName = 'laOgFxFlt[' + ALLTRIM(STR(lnChkAcct)) + ',6]'
    =lfCallShowGet(lcVarName,.F.)

    lcVarName = 'laOgFxFlt[' + ALLTRIM(STR(lnGLACCNT)) + ',6]'
    =lfCallShowGet(lcVarName,.T.)

  CASE (lcVenPmthd ='C' OR lcVenPmthd = 'A')
    IF lnBnkPOS > 0
      loOgScroll.laOGFxFlt[lnBnkPOS ,6] = ''
    ENDIF

    IF lnChAccPOS > 0
      loOgScroll.laOGFxFlt[lnChAccPOS ,6] = ''
    ENDIF

    IF lnGlAccPOS > 0
      loOgScroll.laOGFxFlt[lnGlAccPOS ,6] = ''
    ENDIF

    lcVarName = 'laOgFxFlt[' + ALLTRIM(STR(lnChkAcct)) + ',6]'
    =lfCallShowGet(lcVarName,.F.)

    lcVarName = 'laOgFxFlt[' + ALLTRIM(STR(lnBnkCode)) + ',6]'
    =lfCallShowGet(lcVarName,.F.)

    lcVarName = 'laOgFxFlt[' + ALLTRIM(STR(lnGLACCNT)) + ',6]'
    =lfCallShowGet(lcVarName,.F.)
  OTHERWISE
    IF lnGlAccPOS > 0
      loOgScroll.laOGFxFlt[lnGlAccPOS ,6] = ''
    ENDIF

    lcChkAccnt = ''
    IF lnChAccPOS > 0
      lcChkAccnt = loOgScroll.laOGFxFlt[lnChAccPOS ,6]
    ENDIF

    IF EMPTY(lcChkAccnt)
      lcOGVarName='laOGFxFlt[2,6]'
      =lfBnkCode()

      IF lnChAccPOS > 0
        loOgScroll.laOGFxFlt[lnChAccPOS ,6] = lfchkact()
      ENDIF
    ENDIF

    lcVarName = 'laOgFxFlt[' + ALLTRIM(STR(lnBnkCode)) + ',6]'
    =lfCallShowGet(lcVarName,.T.)
    lcVarName = 'laOgFxFlt[' + ALLTRIM(STR(lnChkAcct)) + ',6]'
    =lfCallShowGet(lcVarName,.T.)
    lcVarName = 'laOgFxFlt[' + ALLTRIM(STR(lnGLACCNT)) + ',6]'
    =lfCallShowGet(lcVarName,.F.)
ENDCASE

****************************************************************************
*
*   lfCallShowGet
*   to call the global function lfOgShowGet
*
****************************************************************************
FUNCTION lfCallShowGet
LPARAMETERS lcVarName,llShowVal
LOCAL lnVarPos
lnVarPos = ASCAN(loOgScroll.laOgObjType,lcVarName,1, ALEN(loOgScroll.laOgObjType,1),1,9)
laOGObjCnt[lnVarPos] = llShowVal
=lfOgShowGet(lcVarName)

*!**************************************************************************
*!
*!      Function: lfvCurDisp
*!
*!**************************************************************************
FUNCTION lfvCurDisp

lcOldCurDs = lcRpCurr

llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

lcRpForm = IIF(lcRpCurr = 'F' , IIF(gfGetMemVar('LLMULCURR') , 'APAPROVF' , 'APAPROVE') , 'APAPROVE')

*!*************************************************************
*! Name      : lfvInvCurr
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/15/2011
*! Purpose   : VALID function of Invoice currency field in the Option Grid
*!*************************************************************
FUNCTION lfvInvCurr

lcInvCurr = VARREAD()

lcOldAlias = ALIAS()
IF EMPTY(&lcInvCurr)
  RETURN
ENDIF

SELECT SYCCURR

IF ATC("?" , &lcInvCurr) > 0 .OR. !SEEK(&lcInvCurr)
  DIMENSION laTemp[1]
  laTemp     = ''

  * N000682 ,1 Thabet Handle globalization issues [Start]
  * lcFile_Ttl = "Currency"
  *lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
               "CCURRDESC :R :H= 'Description',  " +;
               "CCURRSMBL :R :H= 'Symbol'"
  lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Currency,oAriaApplication.GetHeaderText("LANG_Currency",AHEADERFILE))
  lcBrFields = "CCURRCODE :R :H='"+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Currency_Code,oAriaApplication.GetHeaderText("LANG_Currency_Code",AHEADERFILE))+"'," +;
               "CCURRDESC :R :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Description,oAriaApplication.GetHeaderText("LANG_Description",AHEADERFILE))+"',  " +;
  "CCURRSMBL :R :H= LANG_Symbol"
  * N000682 ,1 Thabet Handle globalization issues [END]
  =gfBrows('','CCURRCODE','laTemp')
  IF EMPTY(laTemp[1])
    &lcInvCurr = lcRpOldCur
  ELSE
    &lcInvCurr = laTemp[1]
  ENDIF
ENDIF

SELECT &lcOldAlias

*!*************************************************************
*! Name      : lfwOpGrid
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/15/2011
*! Purpose   : WHEN function of the Option Grid
*!*************************************************************
FUNCTION lfwOpGrid

IF !USED('GLACCHAR')
  =gfOpenTABLE(oAriaApplication.DATADIR+'GLACCHAR',oAriaApplication.DATADIR+'ACCTCODE','SH')
  SELECT GLACCHAR
  =Seek('')
ENDIF

IF !USED('APCHECKS')
  =gfOpenTABLE(oAriaApplication.DATADIR+'APCHECKS',oAriaApplication.DATADIR+'BANKCHECK','SH')
  SELECT APCHECKS
  =Seek('')
ENDIF

IF !USED('APBANKS')
  =gfOpenTABLE(oAriaApplication.DATADIR+'APBANKS',oAriaApplication.DATADIR+'BANKCODE','SH')
  SELECT APBANKS
  Seek('')
ENDIF

IF !USED('FSPRD')
  =gfOpenTABLE(oAriaApplication.DATADIR+'FSPRD',oAriaApplication.DATADIR+'COMFYRPRDI','SH')
  SELECT FSPRD
  Seek('')
ENDIF

IF gfGetMemVar('LLMULCURR')
  LnCurr = ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'LNREPCURR'),1)
  LAOGOBJCNT[LnCurr] = gfGetMemVar('LLMULCURR')
  LFOGSHOWGET('LNREPCURR')
ENDIF

IF !gfGetMemVar('LLMULCURR')
  lcRpForm = 'APAPROVE'
  *laOGObjCnt[8] = .F.
ENDIF

*- Assign the Default Base Currency
LOCAL lnPOS
lnPOS = ASCAN(loOgScroll.laOGFxFlt,"APINVHDR.CCURRCODE")
IF lnPos > 0
  lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
ENDIF
loOgScroll.laOGFxFlt[lnPOS,6] = oAriaapplication.BaseCurrency

*- issue payment method function
=lfvPMethod()


*!**************************************************************************
*!
*!      Function: lfClearRep
*!
*!**************************************************************************
FUNCTION lfClearRep
IF FILE(oAriaApplication.WorkDir +lcApInvHdr+ '.CDX')
  SELECT APINVHDR
  CLOSE INDEX
  ERASE (oAriaApplication.WorkDir +lcApInvHdr+ '.CDX')
ENDIF

*!**************************************************************************
*!
*!      Function: lfvYearPrd
*!
*!**************************************************************************
FUNCTION lfvYearPrd

lcRpArea  = ALIAS()

LOCAL loFld
loFld = _screen.ActiveForm.ActiveControl

PRIVATE llPrdIsUsed
llPrdIsUsed = USED("FSPRD")

IF (AT('?' , loFld.Value) <> 0) .OR. (!EMPTY(STRTRAN(loFld.Value, '-')) AND;
   !SEEK(LEFT(loFld.Value,4) + RIGHT(loFld.Value, 2) , 'FSPRD'))

  DECLARE laRpRetFld(2)

  lcSavFilds = lcBrFields      && Varible to save the old browse fields

  lcBrFields    = [FSPPRDID=CFISFYEAR+'-'+CFSPPRDID:H="Year-Period",DFSPPBGDT:H="Begining date",dfsppendt:H="Ending date"]

  STORE '' TO laRpRetFld[2]

  =gfBrows("",'CFISFYEAR,CFSPPRDID',"laRpRetFld",'Fiscal Periods',.F.)

  IF !EMPTY(laRpRetFld[1])
    loFld.Value = laRpRetFld[1]+'-'+laRpRetFld[2]
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,"APINVHDR.CCHKGLACC")
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      IF EMPTY(loOgScroll.laOGFxFlt[lnPOS,6])
        loOgScroll.laOGFxFlt[lnPOS,6] = laRpRetFld[1]+'-'+laRpRetFld[2]
      ELSE
        loOgScroll.laOGFxFlt[lnPOS,6] = loOgScroll.laOGFxFlt[lnPOS,6] + "|" +laRpRetFld[1]+'-'+laRpRetFld[2]
      ENDIF
    ENDIF
  ELSE
    loFld.Value = loFld.OldValue
  ENDIF
  lcBrFields = lcSavFilds    && Restore the old browse fields
ENDIF

IF EMPTY(STRTRAN(loFld,'-'))
  loFld.Value=''
ENDIF

SELECT FSPRD

IF !llPrdIsUsed
  USE
ELSE
  SET ORDER TO &lcSavOrd
ENDIF

SELECT (lcRpArea)

*!*************************************************************
*! Name      : lfCrtTemp
*! Developer : Hesham Elmasry(HES)
*! Date      : 07/20/2009
*! Purpose   : Create the Temp used as a bas file for the
*!             report and the export
*!*************************************************************
FUNCTION lfCrtTemp

IF USED(lcPriTmp) AND RECCOUNT(lcPriTmp) > 0
  SELECT (lcPriTmp)
  ZAP
ENDIF
*-- Create File
IF !USED(lcPriTmp)
  DIMENSION laTempStru1[19,4]
  lnI = 0

  lnI = lnI + 1
  laTempStru1[lnI,1] = 'CVENDCODE'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 8
  laTempStru1[lnI,4] = 0

  lnI = lnI + 1
  laTempStru1[lnI,1] = 'CVENCOMP'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 30
  laTempStru1[lnI,4] = 0

  lnI = lnI + 1
  laTempStru1[lnI,1] = 'CINVNO'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 12
  laTempStru1[lnI,4] = 0

  lnI = lnI + 1
  laTempStru1[lnI,1] = 'DINVDATE'
  laTempStru1[lnI,2] = 'D'
  laTempStru1[lnI,3] = 8
  laTempStru1[lnI,4] = 0

  lnI = lnI + 1
  laTempStru1[lnI,1] = 'DINVDUDAT'
  laTempStru1[lnI,2] = 'D'
  laTempStru1[lnI,3] = 8
  laTempStru1[lnI,4] = 0

  lnI = lnI + 1
  laTempStru1[lnI,1] = 'CVENPRIOR'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 1
  laTempStru1[lnI,4] = 0

  lnI = lnI + 1
  laTempStru1[lnI,1] = 'PMETHOD'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 0

  lnI = lnI + 1
  laTempStru1[lnI,1] = 'NINVAMNT'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2

  lnI = lnI + 1
  laTempStru1[lnI,1] = 'nOpenBal'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2

  lnI = lnI + 1
  laTempStru1[lnI,1] = 'nInvFAAp'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2

  lnI = lnI + 1
  laTempStru1[lnI,1] = 'NINVPAID'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2

  lnI = lnI + 1
  laTempStru1[lnI,1] = 'NINVADJ'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2

  lnI = lnI + 1
  laTempStru1[lnI,1] = 'NINVDISTK'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2

  lnI = lnI + 1
  laTempStru1[lnI,1] = 'ninvdisap'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2

  lnI = lnI + 1
  laTempStru1[lnI,1] = 'ninvadjap'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2

  lnI = lnI + 1
  laTempStru1[lnI,1] = 'ninva1099'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2

  lnI = lnI + 1
  laTempStru1[lnI,1] = 'CCURRCODE'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 3
  laTempStru1[lnI,4] = 0

  lnI = lnI + 1
  laTempStru1[lnI,1] = 'NINVAMTAP'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 15
  laTempStru1[lnI,4] = 2

  lnI = lnI + 1
  laTempStru1[lnI,1] = 'cAprCurCod'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 3
  laTempStru1[lnI,4] = 0

  DECLARE LAIndeces[1,2]
  LAIndeces[1,1] = 'cvendcode+ccurrcode+cinvno'
  LAIndeces[1,2] = 'cVenCurTag'

  gfCrtTmp(lcPriTmp,@laTempStru1,@LAIndeces,lcPriTmp,.T.)
ENDIF
* End of lfCrtTemp

*!*************************************************************
*! Name      : lfCheckFilter
*! Developer : Hesham Elmasry(HES)
*! Date      : 07/21/2009
*! Purpose   : check filters
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter

LOCAL lcReturn, lnPOS
DO CASE
  CASE lnArrayType = 1
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  CASE lnArrayType = 3
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  OTHERWISE
    lcReturn = ""
ENDCASE

RETURN lcReturn
* End of lfCheckFilter

*!*************************************************************
*! Name      : lfClctData
*! Developer : Hesham Elmasry(HES)
*! Date      : 07/26/2009
*! Purpose   : Get Filter Expression used to collect data
*!*************************************************************
FUNCTION lfClctData

PRIVATE lcVenPmthd,lcBnkCode,lcChkAcnt,lcChkGlAcnt,lcCurrCod,ldInvDat,ldHInvDat,ldLInvDat,ldInvDuDat,ldHInvDuDat, ;
        ldLInvDuDat,lcVenCode,lcDivision,lnHVenPrior,lnLVenPrior,lnFFisFYear,lnFFisPprdID,lnVendrs, ;
        lnLFisFYear,lnLFisPprdID,lcFisFPYear,lnCnt

STORE 0 TO lnHVenPrior,lnLVenPrior,lnFFisFYear,lnFFisPprdID,lnVendrs,lnLFisFYear,lnLFisPprdID,lnCnt

ldInvDat     = lfCheckFilter(1,"APINVHDR.DINVDATE")
ldInvDuDat   = lfCheckFilter(1,"APINVHDR.DINVDUDAT")
lcFisFPYear  = lfCheckFilter(3,"APINVHDR.CFISFYEAR+'-'+APINVHDR.CFSPPRDID")
lcVenPrior   = lfCheckFilter(3,"APINVHDR.CVENPRIOR")

ldLInvDat    = CTOD(SUBSTR(ldInvDat,1,    ATC('|',ldInvDat)-1))
ldHInvDat    = CTOD(SUBSTR(ldInvDat,      ATC('|',ldInvDat)+1))

ldLInvDuDat  = CTOD(SUBSTR(ldInvDuDat,1,  ATC('|',ldInvDuDat)-1))
ldHInvDuDat  = CTOD(SUBSTR(ldInvDuDat,    ATC('|',ldInvDuDat)+1))

STORE '' TO lcPrdFrom,lcPrdTo
IF !EMPTY(lcFisFPYear) AND USED(lcFisFPYear)
  SELECT(lcFisFPYear)
  LOCATE
  lcPrdFrom = KeyExp
  GO BOTTOM
  lcPrdTo   = KeyExp
ENDIF
lcLVenPrior = SUBSTR(lcVenPrior, 1, ATC('|', lcVenPrior)-1)
lcHVenPrior = SUBSTR(lcVenPrior, ATC('|', lcVenPrior)+1)
*- if priority is from 0 to 9 then do not include it in the criteria, this is the default
IF lcLVenPrior == '0' AND lcHVenPrior == '9'
  STORE '' TO lcLVenPrior,lcHVenPrior
ENDIF

lcVenPmthd   = lfCheckFilter(1,"APINVHDR.CVENPMETH")
lcBnkCode    = lfCheckFilter(1,"APINVHDR.CBNKCODE")
lcChkAcnt    = lfCheckFilter(1,"APINVHDR.CCHKACCT")
lcChkGlAcnt  = lfCheckFilter(1,"APINVHDR.CCHKGLACC")
lcCurrCod    = lfCheckFilter(1,"APINVHDR.CCURRCODE")
lcVenCode    = lfCheckFilter(3,"APINVHDR.CVENDCODE")
lcDivision   = lfCheckFilter(3,"APINVHDR.CDIVISION")

*E302628,4 Approved to Pay TMI 10/17/2011 [Start] remove all the criteria from lcRpExp to use it in the advanced option
lcRpExp = STRTRAN(lcRpExp,"  "," ")
=lfReplExp(@lcRpExp,"APINVHDR.CBNKCODE","APINVHDR.CBNKCODE = '&lcBnkCode'")
=lfReplExp(@lcRpExp,"APINVHDR.CCHKACCT","APINVHDR.CCHKACCT = '&lcChkAcnt'")
=lfReplExp(@lcRpExp,"APINVHDR.CCHKGLACC","APINVHDR.CCHKGLACC = '&lcChkGlAcnt'")
lcCurr = IIF(llMultCurr AND !EMPTY(lcCurrCod), "AND APINVHDR.CCURRCODE = '&lcCurrCod'", '')
=lfReplExp(@lcRpExp,"AND APINVHDR.CCURRCODE",lcCurr)
=lfPolishExp(@lcRpExp,"(APINVHDR.CVENDCODE")
=lfPolishExp(@lcRpExp,"(APINVHDR.CDIVISION")
lcRpExp = lcRpExp + IIF(EMPTY(lcDivision),'',IIF(!EMPTY(lcRpExp)," AND ","")+"APINVHDR.CDIVISION "+IIF(LEN(lcDivision)>7,"$","=")+" lcDivision")
=lfReplExp(@lcRpExp,"AND BETWEEN(APINVHDR.CVENPRIOR,'0','9')","")
=lfReplExp(@lcRpExp,"AND APINVHDR.CVENPMETH = 'A'","")
lcRpExp = lcRpExp + IIF(EMPTY(lcRpRefnce),''," AND LIKE('"+STRTRAN(lcRpRefnce,' ','?')+"',APINVHDR.cinvref)")
*E302628,4 Approved to Pay TMI 10/17/2011 [End  ]

STORE 0 TO lnVendrs
IF !EMPTY(lcVenCode)
  SELECT(lcVenCode)
  COUNT FOR !DELETED() TO lnVendrs
ENDIF

SELECT APINVHDR
IF lnVendrs > 0
  SELECT(lcVenCode)
  SCAN FOR !EOF()
    SELECT APINVHDR
    IF Seek(&lcVenCode..cVendCode)
      =lfaddln(&lcVenCode..cVendCode)
    ENDIF
  ENDSCAN
ELSE
  Seek('')
  =lfAddLn() && Add lines for All Vendors in the primary temp
ENDIF
WAIT CLEAR


*!*************************************************************
*! Name      : lfReplExp
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 10/17/2011
*! Purpose   : I created this function as I faced the following problem
* The lcRpExp is created with expression contains
*                      APINVHDR.CBNKCODE = 'CHASE'
* which it should be
*                      APINVHDR.CBNKCODE = 'CHASE   '
* But this would cause problems especially in case if there are more than one bank code with same initials
*!*************************************************************
FUNCTION lfReplExp
LPARAMETERS lcExp,lcFind,lcRepl
LOCAL lnPos,lnAndPos
lnPos = AT(lcFind,lcExp)
IF lnPos>0
  lnAndPos = AT(' AND ',SUBSTR(lcExp,lnPos))
  lnAndPos = IIF(lnAndPos<>0,lnAndPos-1,LEN(SUBSTR(lcExp,lnPos)))
  lcExp = STUFF(lcExp,lnPos,lnAndPos,lcRepl)
ENDIF
*- end of lfReplExp

************************************************************************************************
* Name        : lfPolishExp
* Developer   : Tarek Mohammed Ibrahim - TMI
* Date        : 10/03/2011
* Purpose     : to remove a part of the filter from the lcRpExp
************************************************************************************************
FUNCTION lfPolishExp
PARAMETERS lcExp,lcRmv
LOCAL lnPos,lcRight
lcRight = ")"
lnPos = AT(lcRmv,lcExp)
DO WHILE lnPos>0
  lnAndPos = RAT(' AND ',SUBSTR(lcExp,1,lnPos))
  lcLeftStr = LEFT(lcExp,lnAndPos-1)
  lnPranth = AT(lcRight,SUBSTR(lcExp,lnAndPos))
  lcRightStr = SUBSTR(lcExp,lnAndPos+lnPranth+LEN(lcRight)-1)
  lcExp = lcLeftStr+lcRightStr
  lnPos = AT(lcRmv,lcExp)
ENDDO

*!*************************************************************
*! Name      : lfAddLn
*! Developer : Hesham Elmasry (HES)
*! Date      : 09/09/2009
*! Purpose   : Add Line to the Primary Temp.
*!*************************************************************
FUNCTION lfAddLn
PARAMETER lcVendCode
lcVendCode = IIF(!EMPTY(lcVendCode),lcVendCode,'')

SELECT APINVHDR
=IIF(!EMPTY(lcVendCode),SEEK(lcVendCode,'APVENDOR'),'')
SCAN REST WHILE CVENDCODE+CINVNO = lcVendCode ;
   FOR &lcRpExp
  SCATTER MEMVAR memo
  SELECT APVENDOR
  =IIF(!EMPTY(lcVendCode),'',SEEK(APINVHDR.CVENDCODE,'APVENDOR'))
  WAIT WINDOW NOWAIT APINVHDR.CVENDCODE

  m.CVENCOMP   = APVENDOR.CVENCOMP
  m.PMETHOD    = SUBSTR(lcRpVldEnt,ATC('~',lcRpVldEnt,ATC(cvenpmeth,lcRpVldVal))+1, ;
                   (ATC('~',lcRpVldEnt,ATC(cvenpmeth,lcRpVldVal)+1)-1)-(ATC('~',lcRpVldEnt,ATC(cvenpmeth,lcRpVldVal))))
  m.NINVAMNT = IIF(llMultCurr,APINVHDR.NINVAMNT,gfAmntDisp(APINVHDR.NINVAMNT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.))
  m.NOPENBAL = APINVHDR.NINVAMNT-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ
  m.NOPENBAL = IIF(llMultCurr,m.NOPENBAL,gfAmntDisp(m.NOPENBAL,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.))
  m.nInvFAAp = IIF(llMultCurr,APINVHDR.nInvFAAp,gfAmntDisp(APINVHDR.nInvFAAp,lcRpCurr,ldRpExDate,lcRpTmpNam,.T.))
  m.ninvdisap = IIF(llMultCurr,APINVHDR.ninvdisap,gfAmntDisp(APINVHDR.ninvdisap,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.))
  m.ninvadjap = IIF(llMultCurr,APINVHDR.ninvadjap,gfAmntDisp(APINVHDR.ninvadjap,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.))
  m.ninva1099 = IIF(llMultCurr,APINVHDR.ninva1099,gfAmntDisp(APINVHDR.ninva1099,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.))
  m.cAprCurCod = lfSetCurSm(cAprCurCod)

  SELECT(lcPriTmp)
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN
* End of lfAddLn()

*!*************************************************************
*! Name      : lfSetCurSm
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/15/2011
*! Purpose   : Function to set the currency symbol
*!*************************************************************
FUNCTION lfSetCurSm
PARAMETER lcCrCod

PRIVATE lcExactSt , lnCurSEl

lcExactSt = SET('EXACT')
SET EXACT ON
lnCurSEl = ASCAN(laCurrSmbl , lcCrCod)

SET CURRENCY TO IIF(lnCurSEl <> 0 , ALLTRIM(laCurrSmbl(lnCurSEl + 1)) , '')

SET EXACT &lcExactSt

RETURN ''

**************************************************
*! Called from : Option Grid
*!*************************************************************
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/15/2011
*! Calls       : .....
*!*************************************************************
FUNCTION lfvVendor

lcVenFld = VARREAD()
lcVendor = EVAL(lcVenFld)
SELECT APVENDOR
gfSetOrder('VenCode')
IF !EMPTY(lcVendor) .AND. ('?' $ lcVendor .OR. !SEEK(lcVendor , 'APVENDOR'))
  =gfApVnBrow(@lcVendor)
ENDIF
&lcVenFld = lcVendor
loOgScroll.&lcObjNam. = lcObjVal

*!*************************************************************
*! Name      : lfvCurCode
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 08/15/2011
*! Purpose   : This function called from the currency field to
*!             validate the currency.
*!*************************************************************
FUNCTION lfvCurCode
LOCAL loFld
loFld = loOgScroll.ActiveControl

IF EMPTY(loFld.Value)
  llAllCurr  = .T.
  RETURN
ENDIF
llAllCurr  = .F.

IF !SEEK(loFld.Value,'SYCCURR') .OR. ATC("?",loFld.Value) > 0
  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp     = ''
  lcSavBrFld = lcBrFields
  lcSavTitle = lcFile_Ttl

  * N000682 ,1 Thabet Handle globalization issues [Start]
  *lcFile_Ttl = "Currency"
  *lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
               "CCURRDESC :R :H= 'Description',  " +;
               "CCURRSMBL :R :H= 'Symbol'"
  lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Currency,oAriaApplication.GetHeaderText("LANG_Currency",AHEADERFILE))
  lcBrFields = "CCURRCODE :R :H='"+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Currency_Code,oAriaApplication.GetHeaderText("LANG_Currency_Code",AHEADERFILE))+"'," +;
               "CCURRDESC :R :H='"+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Description,oAriaApplication.GetHeaderText("LANG_Description",AHEADERFILE))+"',  " +;
               "CCURRSMBL :R :H='"+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Symbol,oAriaApplication.GetHeaderText("LANG_Symbol",AHEADERFILE))+"'"
  *N000682 ,1 Thabet Handle globalization issues [END]
  =gfBrows('','CCURRCODE','laTemp')
  lcBrFields = lcSavBrFld
  lcFile_Ttl = lcSavTitle

  lnPOS = ASCAN(loOgScroll.laOGFxFlt,"APINVHDR.CCURRCODE")
  IF lnPos > 0
    lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
  ENDIF
  IF EMPTY(laTemp[1])
    loFld.Value = loFld.OldValue
    loOgScroll.laOGFxFlt[lnPOS,6] = loFld.OldValue
  ELSE
    loFld.Value = laTemp[1]
    loOgScroll.laOGFxFlt[lnPOS,6] = laTemp[1]
  ENDIF
ENDIF
SELECT APINVHDR


*:**************************************************************************
*:* Name        : lfSRVPerd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/30/2010
*:* Purpose     : Set-Reset-Valid function for the periods selected
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfSRVPerd
PARAMETERS lcSRV
LOCAL lnPos,lnSlct,lcSlcPrd
lnSlct = SELECT()

DO CASE
CASE lcSRV = 'R'
  *- be sure that only two id's are selected at most
  lnPrdPos = lfGetPos("APINVHDR.CFISFYEAR+'-'+APINVHDR.CFSPPRDID",'laOgVrFlt')
  SELECT (loOgScroll.laOGVrFlt[lnPrdPos,6])
  LOCATE
  COUNT TO lnCnt
  IF lnCnt>2
    LOCATE
    FOR lnI = 2 TO lnCnt-1
      SKIP
      DELETE
    ENDFOR
  ENDIF
ENDCASE

SELECT(lnSlct)
*-- end of lfSRVPerd.
*!**************************************************************************
*! Function      : lfGetPos
*! Purpose       : Getting the number of element from array
*! Developer     : AHMED MOUSTAFA (AHS)
*! Date          : 08/03/2009
*!**************************************************************************
FUNCTION lfGetPos
PARAMETERS lcOpt,lcArray
LOCAL lnPos
lnPos = ASCAN(loOGScroll.&lcArray,lcOpt)
lnPos = ASUBSCRIPT(loOGScroll.&lcArray,lnPos,1)
RETURN lnPos

******************************************************************************************************
*
*   FUNCTION lfvPriority
*   check if the From Priority is no set , then force it to be '1'
******************************************************************************************************
FUNCTION lfvPriority
LOCAL loFrom,loTo
loFrom = _SCREEN.ActiveForm.ACTIVECONTROL.PARENT.OBJ_FROM
loTo   = _SCREEN.ActiveForm.ACTIVECONTROL.PARENT.OBJ_TO

IF !EMPTY(loTo.VALUE) AND EMPTY(loFrom.VALUE)
  loFrom.VALUE = '1'
ENDIF

*****************************************************************************************************
*
*   lfClearRep
*
*****************************************************************************************************
FUNCTION lfClearRep

IF USED('GLACCHAR')
  =gfCloseTable('GLACCHAR')
ENDIF
IF USED('APCHECKS')
  =gfCloseTable('APCHECKS')
ENDIF
IF USED('APBANKS')
  =gfCloseTable('APBANKS')
ENDIF
IF USED('FSPRD')
  =gfCloseTable('FSPRD')
ENDIF
IF USED('APSETUP')
  =gfCloseTable('APSETUP')
ENDIF
IF USED('ACCOD')
  =gfCloseTable('ACCOD')
ENDIF
IF USED('SYCCOMP')
  =gfCloseTable('SYCCOMP')
ENDIF
