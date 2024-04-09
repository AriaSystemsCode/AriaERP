*:***************************************************************************
*: Program file  : SMREBAL
*: Program desc. : Rebalance Databases OG
*: For screen    :
*:        System : Aria 5 CLIENT
*:        Module : System Manager (SM)
*:     Developer : Mostafa Eid (MOS)
*:***************************************************************************
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
#include  r:\aria4xp\reports\sm\smrebal.h
*N000682,1 MMT 02/11/2013 Globalization changes[End]
IF llOgForAut
    llClrReadN = .T.
    llExpr1 = gfOpGrid('SMREBAL' , .T.)  && Run selection grid.
    RETURN
  ENDIF
IF !llAutoBal
    llExpr1 = gfOpGrid('SMREBAL' , .T.)  && Run selection grid.
  ELSE
    llExpr1 = lfUpBalance()
ENDIF

*!*************************************************************
*! Name      : lfvCompany
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/01/2005
*! Purpose   : Call Companies mover function
*!*************************************************************
*! Example   : = lfvCompany()
*!*************************************************************
FUNCTION lfvCompany
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
*= gfMover(@laRpSorCmp, @laRpTarCmp, 'Select Company', .T., '')  && call mover function.
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfMover(@laRpSorCmp, @laRpTarCmp, LANG_SELECTCOMPANY, .T., '')  && call mover function.
= gfMover(@laRpSorCmp, @laRpTarCmp, IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SELECTCOMPANY,oAriaApplication.GetHeaderText("LANG_SELECTCOMPANY",AHEADERFILE)), .T., '')  && call mover function.
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 02/11/2013 Globalization changes[End]
= lfCmpExpr()
loOgScroll.RefreshScroll()
*-- end of lfvCompany.

*!*************************************************************
*! Name      : lfvModule
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/23/1999
*! Purpose   : - Call Categories mover function
*!*************************************************************
*! Example   : = lfvModule()
*!*************************************************************
FUNCTION lfvModule
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
*= gfMover(@laRpSorMod,@laRpTarMod,'Select Module',.T.,'')  && call mover function.
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfMover(@laRpSorMod,@laRpTarMod,LANG_SELECTMODULE,.T.,'')  && call mover function.
= gfMover(@laRpSorMod,@laRpTarMod,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SELECTMODULE,oAriaApplication.GetHeaderText("LANG_SELECTMODULE",AHEADERFILE)),.T.,'')  && call mover function.
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 02/11/2013 Globalization changes[End]
=lfModExpr()
loOgScroll.RefreshScroll()
*-- end of lfvModule.

*!*************************************************************
*! Name      : lfvUpdLog
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 03/30/2005
*! Purpose   : Replace Use Log File Field...
*!*************************************************************
*! Example   : = lfvUpdLog()
*!*************************************************************
FUNCTION lfvUpdLog

LOCAL lnDataSess, lnAlias
lnAlias    = SELECT(0)
lnDataSess = SET("Datasession")
SET DATASESSION TO (lnPrgSession)

*-- Now change the laInfoTemp.cUpdVryIgn  with the value of (Update/verify/Ignore)
SELECT (lcRebalHdr)
IF SEEK('LCLOGFILE')
  REPLACE cLogFile WITH IIF(llLogFile, 'Y', 'N')
ENDIF
SET DATASESSION TO (lnDataSess)
SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfvUVIAll
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/01/2005
*! Purpose   : Change the stauts (Update / Verify /Ignore) of all
*!             rows of option grid.
*!*************************************************************
*! Example   : = lfvUVIAll()
*!*************************************************************
FUNCTION lfvUVIAll
LOCAL lnAlias, lnDataSess, lnOGTypLen
lnAlias    = SELECT(0)
lnDataSess = SET("Datasession")

*-- Get Length of Visiable rows in Option Grid
lnOGTypLen = ALEN(laOGObjType,1)

SET DATASESSION TO (lnPrgSession)

*-- Now change the laInfoTemp.cUpdVryIgn  with the value of (Update all / vrify all / Ignore all)
SELECT (lcRebalHdr)
IF SEEK('LCLOGFILE')
  REPLACE cLogFile WITH 'Y'
ENDIF

LOCATE
SCAN WHILE !EOF() FOR  'LCRPITEM' $ UPPER(&lcRebalHdr..cItemName)
  REPLACE &lcRebalHdr..cUpdVryIgn WITH lcUpdVry
ENDSCAN

SET DATASESSION TO (lnDataSess)

FOR lnI = 1  TO lnOGTypLen
  IF 'LCRPITEM' $ UPPER(laOGObjType[lnI ,1])
    &laOGObjType[lnI,1] = lcUpdVry
    =lfOGShowGet("&laOGObjType[lnI ,1]")
  ENDIF
ENDFOR

SELECT (lnAlias)
RETURN
*B127969,1 WSH 05/25/2005 [End]

*-- end of lfvUVIAllf.
*!*************************************************************
*! Name      : lfvUpVrIg
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/01/2005
*! Purpose   : Replace the stauts flag (Update / Verify /Ignore) in temp.
*!             file with the value of Option Grid.
*!*************************************************************
*! Example   : = lfvUpVrIg()
*!*************************************************************
FUNCTION lfvUpVrIg
PARAMETER  lcCurRow

LOCAL lnDataSess, lcGetValue
lnDataSess = SET("Datasession")
SET DATASESSION TO (lnPrgSession)

* Now change the laInfoTemp.cUpdVryIgn  with the value of (Update/verify/Ignore)
IF SEEK( UPPER(lcCurRow) , lcRebalHdr)
  lcGetValue = &lcCurRow
  REPLACE &lcRebalHdr..cUpdVryIgn WITH lcGetValue
ENDIF
SET DATASESSION TO (lnDataSess)
RETURN
*--- End of lfvUpVrIg()
*!*************************************************************
*! Name      : lfvDclTarg
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/01/2005
*! Purpose   : Declare target array to avoid error message
*!             'lcRpTarCat' not an array'
*!*************************************************************
*! Example   : = lfvDclTarg()
*!*************************************************************
FUNCTION lfvDclTarg

DECLARE laRpTarMod[1]
*-- end of lfvDclTarg

*!*************************************************************
*! Name      : lfFillRepVars
*! Developer : Wael M. Abo-Shawareb
*! Date      : 03/30/2005
*! Purpose   : Fill Variables and Create Temp cursors needed in the Program
*!*************************************************************
*! Example   : = lfFillRepVars()
*!*************************************************************
FUNCTION lfFillRepVars

IF EMPTY(laRpCmpCod[1,1]) OR EMPTY(laRpSorCmp[1,1])
  LOCAL lnDataSess
  lnDataSess = SET("Datasession")
  SET DATASESSION TO (lnPrgSession)

  *-- Define companies array that be used in company mover
  DECLARE laRpCmpCod[1,3]
  STORE '' TO lcRpCmpExp

  *-- Collect all companies
  SELECT ccomp_id+" - "+cCom_Name,cCom_dDir,mModlSet ;
    FROM (lcSYCCOMP)                            ;
    INTO ARRAY laRpCmpCod                   ;
    ORDER BY 1
  DECLARE laRpSorCmp[ALEN(laRpCmpCod,1),1],laRpTarCmp[ALEN(laRpCmpCod,1),1]
  FOR lnI = 1 TO ALEN(laRpCmpCod,1)
    STORE laRpCmpCod[lnI,1] TO laRpSorCmp[lnI,1],laRpTarCmp[lnI,1]
  ENDFOR
*  lnPrgSession = SET("Datasession")
  =lfCmpExpr()
  =lfvDfinMod()
  =lfvUVIAll()
  SET DATASESSION TO (lnDataSess)
ENDIF

*-- Open Remote Tables needed by the Option Grid...
=lfOpenRemote()

*!*************************************************************
*! Name      : lfGetCust
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/02/2005
*! Purpose   : To get the Customer name
*!*************************************************************
*! Example   : = lfGetCust()
*!*************************************************************
FUNCTION lfGetCust
LPARAMETERS lcAccount
IF loCustomer.SEEK('M' + lcAccount)
  RETURN EVALUATE(lcCustomer + '.btName')
ENDIF
*!*************************************************************
*! Name      : lfGetVendor
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 10/10/2004
*! Purpose   : Get Vendor Name...
*!*************************************************************
*! Example     : = lfGetVendor()
*!*************************************************************
FUNCTION lfGetVendor
LPARAMETERS lcVenCode, lcStyType

LOCAL lcRetVal
lcRetVal = ''

IF lcStyType = 'N'
  IF loWareHous.SEEK(RTRIM(lcVenCode))
    lcRetVal = EVALUATE(lcWareHous + '.cDesc')
  ENDIF
ELSE
  IF loVendor.SEEK(RTRIM(lcVenCode))
    lcRetVal = EVALUATE(lcVendor + '.cVenComp')
  ENDIF
ENDIF

RETURN lcRetVal
!*************************************************************
*! Name      : lfStySum
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 10/13/2004
*! Purpose   : Sum Quantities from Style file
*!*************************************************************
*! Example     : = lfStySum()
*!*************************************************************
FUNCTION lfStySum
LPARAMETERS lcSty, lccomp, lnAddToVar

LOCAL lnTotcomp, lnAlias
lnAlias   = SELECT(0)
lnTotcomp = 0

*B130510,1 WSH 12/05/2005 Style browse is slow. [Start]
*IF loStyle_X.SEEK(ALLTRIM(lcSty))
IF loStyle_X.SEEK(lcSty)
*B130510,1 WSH 12/05/2005 [End]
  SELECT (lcStyle_X)
  *B130510,1 WSH 12/05/2005 Style browse is slow. [Start]
  *SUM REST &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
  SUM REST &lcCOMP TO lnTotcomp WHILE Style = lcSty
  *B130510,1 WSH 12/05/2005 [End]

ENDIF

DO CASE
  CASE lnAddToVar = 1
    lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
    lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
    lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE

SELECT (lnAlias)

*B130510,1 WSH 12/05/2005 Style browse is slow. [Start]
*RETURN INT(lnTotcomp)
RETURN lnTotcomp
*B130510,1 WSH 12/05/2005 [End]

*!*************************************************************
*! Name      : lfvChkModul
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/03/2005
*! Purpose   : Appear or disappear each item in OG
*!*************************************************************
*! Example   : =lfvChkModul()
*!*************************************************************
FUNCTION lfvChkModul
LPARAMETER lcChkModul, lcFldName


IF EMPTY(lcChkModul)
  RETURN .T.
ENDIF

*B127969,1 WSH 05/25/2005 Reassign the llNComp variable. [Start]
llNComp = LEN(lcRpCmpExp) > 2 OR EMPTY(oAriaApplication.ActiveCompanyID) OR !(oAriaApplication.ActiveCompanyID $ lcRpCmpExp)
*B127969,1 WSH 05/25/2005 [End]

LOCAL lnDataSess, lnAlias
lnAlias    = SELECT(0)
lnDataSess = SET("Datasession")

SET DATASESSION TO (lnPrgSession)
SELECT (lcRebalHdr)

*-- If module array not defined , build it first
=lfvDfinMod()

*-- Variable to hold module array length
PRIVATE lnModLen
lnModLen = ALEN(laRpCodMod,1)

*-- Variable to hold Module name
PRIVATE lcModlName
lcModlName = ''

*-- Get module name from module array to compare it with module target array
FOR lnI = 1  TO  lnModLen
*!*	  IF lcChkModul = laRpCodMod[lnI, 1]+" - "+ laRpCodMod[lnI , 2]
*!*	    lcModlName  = laRpCodMod[lnI, 2]+" - "+ laRpCodMod[lnI , 2]
  IF  lcChkModul = laRpCodMod[lnI, 1]
      lcModlName = laRpCodMod[lnI, 2]
    EXIT
  ENDIF
ENDFOR

*-- If module name is empty disable option grid
IF EMPTY (lcModlName)
  IF SEEK(UPPER(lcFldName), lcRebalHdr)
    REPLACE &lcRebalHdr..cUpdVryIgn WITH 'I'
    *MMT2
    lcVarToUpd = UPPER(lcFldName)
    &lcVarToUpd = 'I'
    *MMt2
    IF llSaveAut
      lcItemV    = UPPER(lcFldName)
      lcOldValue = EVALUATE(lcItemV)
      &lcItemV   = 'I'
    ENDIF
  ENDIF

  SET DATASESSION TO (lnDataSess)
  SELECT (lnAlias)
  RETURN .T.
ENDIF

*-- If passed module id (lcChkModul) is not defined in module array, disable the
*--  variable from option grid.
IF ASCAN(laRpTarMod, lcModlName) = 0
  IF SEEK(UPPER(lcFldName), lcRebalHdr)
    REPLACE &lcRebalHdr..cUpdVryIgn WITH 'I'

    *MMT2
    lcVarToUpd = UPPER(lcFldName)
    &lcVarToUpd = 'I'
    *MMt2


    IF llSaveAut
      lcItemV  = UPPER(lcFldName)
      &lcItemV = 'I'
    ENDIF
  ENDIF
  SET DATASESSION TO (lnDataSess)
  SELECT (lnAlias)
  RETURN .T.
ELSE
  IF SEEK(UPPER(lcFldName), lcRebalHdr)

    *B127969,1 WSH 05/25/2005 Get the value from the memory variable. [Start]
    *REPLACE &lcRebalHdr..cUpdVryIgn WITH IIF(&lcRebalHdr..cUpdVryIgn = 'I', 'V', &lcRebalHdr..cUpdVryIgn)
    REPLACE &lcRebalHdr..cUpdVryIgn WITH IIF(TYPE(lcFldName) = 'C', EVALUATE(lcFldName), &lcRebalHdr..cUpdVryIgn)
    *B127969,1 WSH 05/25/2005 [End]

    IF llSaveAut
      lcItemV  = UPPER(lcFldName)
      &lcItemV = lcOldValue
    ENDIF
  ENDIF
ENDIF

SET DATASESSION TO (lnDataSess)
SELECT (lnAlias)
RETURN .F.

*!*************************************************************
*! Name      : lfvCtNo
*! Developer : Mariam Mazhar (MMT)
*! Date      : 03/27/2007
*! Purpose   : Valid function for the C/T number.
*!*************************************************************
*! Example   : = lfvCtNo()
*!*************************************************************

FUNCTION lfvCtNo
LPARAMETERS lcMode

=lfCreatePOCursor(1, 'lcCTN', @lcSelCTNo)

*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[End]
*!*************************************************************
*! Name      : lfvInvNo
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/02/2005
*! Purpose   : Valid function for the invoice number.
*!*************************************************************
*! Example   : = lfvInvNo()
*!*************************************************************
FUNCTION lfvInvNo
LPARAMETERS lcMode

=lfCreateCursor(1, 'lcInvN', @lcSelInvNo, 'Invoice C(6)', 'Invoice', 'Invoice')

*!*************************************************************
*! Name      : lfvOrdNo
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/02/2005
*! Purpose   : Valid function for the order number.
*!*************************************************************
*! Example   : = lfvOrdNo()
*!*************************************************************
FUNCTION lfvOrdNo
LPARAMETERS lcMode

=lfCreateCursor(1, 'lcOrdN', @lcSelOrdNo, 'Order C(6)', 'Order', 'Order')
*!*************************************************************
*! Name      : lfvPONO
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/02/2005
*! Purpose   : Valid function for the Style P/O number.
*!*************************************************************
*! Example   : = lfvPONO()
*!*************************************************************
FUNCTION lfvPONO
LPARAMETERS lcMode

*B129821,1 WSH 09/29/2005 Use the complete key value for POSHDR file browse [Start]
*=lfCreateCursor(1, 'lcPON', @lcSelPONo, 'PO C(6)', 'PO', 'PO')
=lfCreatePOCursor(1, 'lcPON', @lcSelPONo)
*B129821,1 WSH 09/29/2005 [End]

*!***************************************************************************
*! Name      : lfvAccount
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/01/2005
*! Purpose   : Validation for Account code.
*!***************************************************************************
*! Example   : = lfvAccount()
*!***************************************************************************
FUNCTION lfvAccount
LPARAMETERS lcMode

=lfCreateCursor(1, 'lcAccount', @lcSelAccount, 'Account C(5)', 'Account', 'Account')
*!*************************************************************
*! Name      : lfvFisYer
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 08/23/1999
*! Purpose   : - Browse fiscal years
*!*************************************************************
FUNCTION lfvFisYer
LPARAMETERS lcMode

=lfCreateCursor(1, 'lcFisYear', @lcSelYear, 'cFisfYear C(4)', 'cFisfYear', 'cFisfYear')

*!***************************************************************************
*! Name      : lfvStyle
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/02/2005
*! Purpose   : Validation for Style code.
*!***************************************************************************
*! Example   : = lfvStyle()
*!***************************************************************************


FUNCTION lfvStyle
LPARAMETERS lcMode
=lfCreateCursor(1, 'lcAllStyle', @lcSelStyle, 'cStyMajor C(19)', 'cStyMajor', 'cStyMajor')
*-- End of lfvStyle.

*!***************************************************************************
*! Name      : lfvFabric
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/02/2005
*! Purpose   : Validation for Style code.
*!***************************************************************************
*! Example   : = lfvStyle()
*!***************************************************************************
FUNCTION lfvFabric
LPARAMETERS lcMode

*B608487,1 WAM 03/19/2008 Have a different temp table for selected fabrics
*=lfCreateCursor(1, 'lcAllFabric', @lcSelStyle, 'cStyMajor C(19)', 'cStyMajor', 'cStyMajor')
=lfCreateCursor(1, 'lcAllFabric', @lcSelFabric, 'cStyMajor C(19)', 'cStyMajor', 'cStyMajor')
*B608487,1 WAM 03/19/2008 (End)
*-- End of lfvStyle.

*!*************************************************************
*! Name      : lfvMPONO
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/02/2005
*! Purpose   : Valid function for the Material P/O number.
*!*************************************************************
*! Example   : = lfvMPONO()
*!*************************************************************
FUNCTION lfvMPONO
LPARAMETERS lcMode

*B129821,1 WSH 09/29/2005 Use the complete key value for POSHDR file browse [Start]
*=lfCreateCursor(1, 'lcMPON', @lcSelPONo, 'PO C(6)', 'PO', 'PO')
=lfCreatePOCursor(1, 'lcMPON', @lcSelPONo)
*B129821,1 WSH 09/29/2005 [End]
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/01/2005
*! Purpose   : When function of Option Grid
*!*************************************************************
*! Example   : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
=lfGetVarVl()

*!*************************************************************
*! Function  : lfwShowRep
*! Developer : Amin Khodary Amin
*! Date      : 08/23/1999
*! Purpose   : - Show report
*!*************************************************************
FUNCTION lfwShowRep

LOCAL lnDataSess
lnDataSess = SET("Datasession")
SET DATASESSION TO (lnPrgSession)

lcRpFiscYr = IIF(SEEK(oAriaApplication.ActiveCompanyID, lcSYCCOMP), &lcSYCCOMP..cCurr_Yer, '')
lcFiscYear = lcRpFiscYr

SET DATASESSION TO (lnDataSess)
RETURN

*!***************************************************************************
*! Name      : lfCmpExpr
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/01/2005
*! Purpose   : -Evaluate Company expression.
*!***************************************************************************
*! Example   : = lfCmpExpr()
*!***************************************************************************

FUNCTION lfCmpExpr

PRIVATE laTarget

IF EMPTY(laRpTarCmp)
  = ACOPY(laRpSorCmp,laTarget)
ELSE
  = ACOPY(laRpTarCmp,laTarget)
ENDIF

= ASORT(laTarget)
lcRpCmpExp = ''
FOR lnI = 1 TO ALEN(laTarget,1)
  lcRpCmpExp = IIF(EMPTY(lcRpCmpExp),PADR(laTarget[lnI],2),;
                    lcRpCmpExp + ','+PADR(laTarget[lnI],2))
ENDFOR
*B127969,1 WSH 05/25/2005 Hide Fitlers if the selected company is not the Active Company or no Active company. [Start]
*!*  IF LEN(lcRpCmpExp) > 2
*!*    llNComp = .T.
*!*  ELSE
*!*    llNComp = .F.
*!*  ENDIF
llNComp = LEN(lcRpCmpExp) > 2 OR EMPTY(oAriaApplication.ActiveCompanyID) OR !(oAriaApplication.ActiveCompanyID $ lcRpCmpExp)
*B127969,1 WSH 05/25/2005 [End]

*!***************************************************************************
*! Name      : lfModExpr
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/01/2005
*! Purpose   : Evaluate Category expression.
*!***************************************************************************
*! Example   : = lfModExpr()
*!***************************************************************************

FUNCTION lfModExpr
PRIVATE laTarget

IF EMPTY(laRpTarMod)
  =ACOPY(laRpSorMod,laTarget)
ELSE
  =ACOPY(laRpTarMod,laTarget)
ENDIF

=ASORT(laTarget)
lcRpModExp = ''

FOR lnI = 1 TO ALEN(laTarget,1)
    POS = ASCAN(laRpCodMod,laTarget[lnI])
    POS = POS - 1
    lcRpModExp = IIF(EMPTY(lcRpModExp),laRpCodMod[POS],lcRpModExp + ','+laRpCodMod[POS])
ENDFOR

*!*	FOR lnI = 1 TO ALEN(laRpCodMod)-(ALEN(laRpCodMod)/2)
*!*	  lcRpModExp = IIF(EMPTY(lcRpModExp),laRpCodMod[lnI,1],lcRpModExp + ','+laRpCodMod[lnI,1])
*!*	
*!*	ENDFOR

*!*	FOR lnI = 1 TO ALEN(laTarget)
*!*	  lcRpModExp = IIF(EMPTY(lcRpModExp),PADR(laTarget[lnI],2),;
*!*	                    lcRpModExp + ','+PADR(laTarget[lnI],2))
*!*	ENDFOR

*-- end of lfModExpr.

*!*************************************************************
*! Name      : lfvDfinMod
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/01/2005
*! Purpose   : Build source & target module array from main module array.
*!*************************************************************
*! Example   : =lfvDfinMod()
*!*************************************************************
FUNCTION lfvDfinMod

IF EMPTY(laRpCodMod[1,1]) OR EMPTY(laRpSorMod[1,1])
  *-- Declare and define modules.
  =lfvGetModl()

  * Declare soruce & Target of module array
  DECLARE laRpSorMod[1] , laRpTarMod[1]

  * Copy module array to source module array.
  lnModLen = ALEN(laRpCodMod,1)
  FOR lnI = 1  TO  lnModLen
      DECLARE laRpSorMod[lnI] , laRpTarMod[lnI]
    laRpSorMod[lnI] = laRpCodMod[lnI , 2]
    laRpTarMod[lnI] = laRpCodMod[lnI , 2]
  ENDFOR
  =lfModExpr()
ENDIF

*!*************************************************************
*! Name      : lfOpenRemote
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/02/2005
*! Purpose   : Open Remote Cursors needed by the Option Grid
*!*************************************************************
FUNCTION lfOpenRemote

LOCAL lnAlias
lnAlias = SELECT(0)

STORE .NULL. TO loVendor, loWareHous, loStyle_X, loCustomer

*--Create Vendor table Object...
loVendor = CREATEOBJECT('RemoteTable', 'APVENDOR', 'VENCODE', lcVendor, SET("Datasession"))

*--Create WareHous table Object...
loWareHous = CREATEOBJECT('RemoteTable', 'WAREHOUS', 'WAREHOUS', lcWareHous, SET("Datasession"))

*--Create Style table Object...

*B130510,1 WSH 12/05/2005 Style browse is slow. [Start]
*loStyle_X = CREATEOBJECT('RemoteTable', 'STYLE', 'STYLE', lcStyle_X, SET("Datasession"))
loStyle_X = CREATEOBJECT('RemoteTable', 'STYLE', 'CSTYLE', lcStyle_X, SET("Datasession"))
*B130510,1 WSH 12/05/2005 [End]

*--Create Customer table Object...
loCustomer = CREATEOBJECT('RemoteTable', 'CUSTOMER', 'CUSTOMER', lcCustomer, SET("Datasession"))

IF FILE(oAriaApplication.WorkDir + lcSelInvNo + '.DBF')
  USE (oAriaApplication.WorkDir + lcSelInvNo + '.DBF') EXCLUSIVE
  ZAP
  USE
ENDIF

*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
IF FILE(oAriaApplication.WorkDir + lcPackSelect + '.DBF')
  USE (oAriaApplication.WorkDir + lcPackSelect + '.DBF') EXCLUSIVE
  ZAP
  USE
ENDIF
*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]


IF FILE(oAriaApplication.WorkDir + lcSelOrdNo + '.DBF')
  USE (oAriaApplication.WorkDir + lcSelOrdNo + '.DBF') EXCLUSIVE
  ZAP
  USE
ENDIF

IF FILE(oAriaApplication.WorkDir + lcSelPONo + '.DBF')
  USE (oAriaApplication.WorkDir + lcSelPONo + '.DBF') EXCLUSIVE
  ZAP
  USE
ENDIF

IF FILE(oAriaApplication.WorkDir + lcSelAccount + '.DBF')
  USE (oAriaApplication.WorkDir + lcSelAccount + '.DBF') EXCLUSIVE
  ZAP
  USE
ENDIF

IF FILE(oAriaApplication.WorkDir + lcSelStyle + '.DBF')
  USE (oAriaApplication.WorkDir + lcSelStyle + '.DBF') EXCLUSIVE
  ZAP
  USE
ENDIF

IF FILE(oAriaApplication.WorkDir + lcSelYear + '.DBF')
  USE (oAriaApplication.WorkDir + lcSelYear + '.DBF') EXCLUSIVE
  ZAP
  USE
ENDIF

SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfCreatePOCursor
*! Developer : Wael M. Abo-Shawareb
*! Date      : 09/29/2005
*! Purpose   : Build SQL Cursor for PO
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! B129821,1
*!*************************************************************
FUNCTION lfCreatePOCursor
LPARAMETERS lnFiltType, lcFiltExp, lcCursName

LOCAL lcTmpCurs, lcSQLRet, llFound, lnRet
llFound = .F.
lnRet   = 0  && Filter not found

lcTmpCurs = lfCheckFilter(lnFiltType, lcFiltExp)
IF !EMPTY(lcTmpCurs)
  SELECT (lcTmpCurs)
  llFound = (RECCOUNT() > 0)
  IF llFound
    IF !FILE(oAriaApplication.WorkDir + lcCursName + '.DBF')
      CREATE TABLE (oAriaApplication.WorkDir + lcCursName + '.DBF') (cBusDocu C(1), cStyType C(1), PO C(6))
      INDEX ON cBusDocu+cStyType+PO TAG (lcCursName)
      USE IN (lcCursName)
    ENDIF

    USE (oAriaApplication.WorkDir + lcCursName) EXCLUSIVE IN 0 ALIAS (lcCursName)
    SELECT (lcCursName)
    ZAP

    SELECT (lcTmpCurs)
    SCAN
      SCATTER MEMO TO laTmp
      SELECT (lcCursName)
      APPEND BLANK
      REPLACE cBusDocu WITH SUBSTR(laTmp[1], 1, 1),;
              cStyType WITH SUBSTR(laTmp[1], 2, 1),;
              PO       WITH SUBSTR(laTmp[1], 3)
    ENDSCAN
    USE IN (lcCursName)
  ENDIF
ENDIF

RETURN lnRet

*!*************************************************************
*! Name      : lfGetVarVl
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/01/2005
*! Purpose   : Get the default values of OG variables.
*!*************************************************************
*! Example   : = lfGetVarVl()
*!*************************************************************
FUNCTION lfGetVarVl

LOCAL lnDataSess, lnI, lcGetValue
lnDataSess = SET("Datasession")
SET DATASESSION TO (lnPrgSession)

FOR lnI = 1 TO ALEN(laOGObjType,1)
  IF UPPER(laOGObjType[lnI,1]) = 'LCRPITEM' .AND. SEEK(UPPER(laOGObjType[lnI,1]), lcRebalHdr)
    lcGetValue = EVALUATE(laOGObjType[lnI,1])
    REPLACE &lcRebalHdr..cUpdVryIgn WITH lcGetValue
  ENDIF

  IF UPPER(laOGObjType[lnI,1]) = 'LCLOGFILE' .AND. SEEK(UPPER(laOGObjType[lnI,1]), lcRebalHdr)
    lcGetValue = EVALUATE(laOGObjType[lnI,1])
    REPLACE &lcRebalHdr..cLogFile WITH lcGetValue
  ENDIF
ENDFOR

SET DATASESSION TO (lnDataSess)

*!*************************************************************
*! Name      : lfvGetModl
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 03/31/2005
*! Purpose   : Check and assign the active module to module array
*!*************************************************************
*! Example   : =lfvGetModl()
*!*************************************************************
FUNCTION lfvGetModl

LOCAL lnDataSess, lnRemResult, lcSYDAPPL, lcModId, lnModlNo, lcAreaNo
lnDataSess = SET("Datasession")
lcAreaNo   = SELECT(0)

SET DATASESSION TO (lnPrgSession)

lcSYDAPPL  = gfTempName()

DECLARE laRpCodMod[1,2]

lnRemResult = oAriaApplication.RemoteSystemData.Execute("Select * from SYDAPPL ORDER BY CAPP_ID", '', lcSYDAPPL, "", oAriaApplication.SystemConnectionString, 3, "", lnPrgSession)
IF lnRemResult <> 1
  SET DATASESSION TO (lnDataSess)
  RETURN .F.
ENDIF

SELECT (lcSYCCOMP)
LOCATE

SCAN WHILE !EOF()
  *-- Get & assign all modules to string varibale
  lcComp_mdl = ALLTRIM(&lcSYCCOMP..mComp_mdl)

  *-- If no module has been defined in this record skip to next record
  IF EMPTY(ALLTRIM(lcComp_mdl))
    SKIP
    LOOP
  ENDIF

  *-- Variable used as a counter to redclare the module array.
  lnModlNo = 0

  DO WHILE !EMPTY(lcComp_mdl)

    *-- Get position of '|' character
    lnPosition = ATC('|', lcComp_mdl)

    *-- Accumulate no. of modules that found in SYCCOMP
    lnModlNo = lnModlNo + 1
    IF lnModlNo  = 1
      *-- Split module id from whole module string
      lcModId = SUBSTR(lcComp_mdl, 1, 2)

      *-- Remove module ID from modules string
      lcComp_mdl = SUBSTR(lcComp_mdl, 3)
    ELSE
      *-- Split module id from whole module string
      lcModId = SUBSTR(lcComp_mdl, 2, 2)

      *-- Remove module ID from modules string
      lcComp_mdl = SUBSTR(lcComp_mdl, 4)
    ENDIF

    *--Check if Module exisit in lcAria4Modules
    IF !lcModId $ lcAria4Modules
      LOOP
    ENDIF

    *-- Check duplicated module id , If this module id was inserted before in
    *-- module array, skip it.
    IF ASCAN(laRpCodMod, lcModId) = 0 AND !(lcModId $ 'SUEBPSRGSP')
      *-- Get module name , then svae module id & module name.
      SELECT (lcSYDAPPL)
      LOCATE FOR cApp_Id = lcModId

      IF FOUND()
        *-- Declare module code array
        IF EMPTY(laRpCodMod)
           DECLARE laRpCodMod[1,2]
        ELSE
           DECLARE laRpCodMod[ALEN(laRpCodMod,1)+1,2]
        ENDIF

        laRpCodMod[ALEN(laRpCodMod,1),1] = lcModId
        laRpCodMod[ALEN(laRpCodMod,1),2] = ALLTRIM(&lcSYDAPPL..cApp_Name)
      ENDIF
    ENDIF
  ENDDO
ENDSCAN

IF !EMPTY(laRpCodMod)
  =ASORT(laRpCodMod,2)
ENDIF

USE IN (lcSYDAPPL)

SET DATASESSION TO (lnDataSess)
SELECT (lcAreaNo)

*!*************************************************************
*! Name      : lfInfoHdr
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/01/2005
*! Purpose   : Create one temporary file
*!             Fill it with header info. that will need it while process
*!*************************************************************
*! Example   : = lfInfoHdr()
*!*************************************************************
FUNCTION lfInfoHdr

LOCAL lnDataSess
lnDataSess = SET("Datasession")
SET DATASESSION TO (lnPrgSession)

* lcUpVrIg is a varibale represents the default value of Update all / Verify all/ Ignore all.
PRIVATE  lcUpVrIg
lcUpVrIg = 'V'

IF USED(lcRebalHdr)
  SELECT (lcRebalHdr)
  USE
ENDIF
IF USED(lcRebalDtl)
  SELECT (lcRebalDtl)
  USE
ENDIF

* Get temporary file header and index name.
lcInfoHdr = gfTempName()

CREATE TABLE (oAriaApplication.WorkDir + lcInfoHdr ) ;
  (cItemName  C(10)  ,;
   cUpdFunNam C(10)  ,;
   cVryFunNam C(10)  ,;
   cIgnFunNam C(10)  ,;
   cItemDesc  C(30)  ,;
   cAddSetup  C(10)  ,;
   cValidFun  C(10)  ,;
   cUpdVryIgn C(1)   ,;
   cLogFile   C(1)   ,;
   cTempName  C(8)   ,;
   cFileStr   C(180) ,;
   cMFiles    C(80)  ,;
   cInvType   C(4))

* Create index on temporary file
INDEX ON  cItemName TAG ItemHdr  OF (oAriaApplication.WorkDir + lcInfoHdr + '.CDX')
USE     && Colse the temp. file and open it with alias name
USE (oAriaApplication.WorkDir + lcInfoHdr ) ALIAS (lcRebalHdr) IN 0
SET ORDER TO TAG ItemHdr

* Get temporary file detail and index name.
lcInfoDtl  = gfTempName()

*Build the information file structure.
CREATE TABLE (oAriaApplication.WorkDir + lcInfoDtl ) ;
   (cItemName  C(10) ,;
    cFileName  C(10) ,;
    cNdxName   C(10) ,;
    cAliasName C(10) ,;
    lOpenBefor L(1)  ,;
    cFileObj   C(10) ,;
    lSQLFile   L(1))

* Fields description
*Field name         Description
*----------     -----------------------------------------------------------
*cItemName     Represents SyRepUvr.mfld_name field
*cFileName     File name
*cNdxName      index name
*cAliasName    Alias name
*lOpenBefor    '.T.' file was open before , '.F.' file was not open before
*----------     -----------------------------------------------------------
* Create index on temporary file

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
*INDEX ON cItemName TAG ItemDtl
*INDEX ON cFileName TAG cFileName
INDEX ON cItemName+cFileName TAG ItemDtl
*B128464,2 WSH 08/21/2005 [End]

USE     && Colse the temp. file and open it with alias name
USE (oAriaApplication.WorkDir + lcInfoDtl) ALIAS (lcRebalDtl) IN 0
SET ORDER TO ItemDtl

* Insert generate rebalance log information (LCLOGFILE).
* ------------------------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCLOGFILE' ,;
        cUpdFunNam WITH 'UpdName'   ,;
        cVryFunNam WITH 'VryName'   ,;
        cIgnFunNam WITH 'IgnName'   ,;
        cLogFile   WITH  'Y'

*----------  Not in  this Release
*!*  * Header info. about GL relabance information (lcRpItem1).
*!*  * --------------------------------------------------------
*!*  SELECT (lcRebalHdr)
*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM1'  ,;
*!*          cUpdFunNam WITH 'lfUpdItem1' ,;
*!*          cVryFunNam WITH 'lfVryItem1' ,;
*!*          cIgnFunNam WITH 'IgnName'    ,;
*!*          cUpdVryIgn WITH lcUpVrIg     ,;
*!*          cAddSetup  WITH 'lfStupItm1' ,;
*!*          cValidFun  WITH 'lfValdItm1'

*!*  * Detail info. about GL rebalance master files 'lcRpItem1'.
*!*  * --------------------------------------------------------
*!*  SELECT (lcRebalDtl)
*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM1' ,;
*!*          cFileName  WITH 'GLBATCH'   ,;
*!*           cNdxName   WITH 'BATSTAT'   ,;
*!*          cAliasName WITH 'GLBATCH'   ,;
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM1' ,;
*!*          cFileName  WITH 'GLPTRNHD'  ,;
*!*           cNdxName   WITH ''          ,;
*!*          cAliasName WITH 'GLTRNSHD'  ,;
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM1' ,;
*!*          cFileName  WITH 'GLPTRNDT'  ,;
*!*           cNdxName   WITH 'BATCHTRN'  ,;
*!*          cAliasName WITH 'GLTRNSDT'  ,;
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM1' ,;
*!*          cFileName  WITH 'GLACBALS'  ,;
*!*           cNdxName   WITH 'ACCYRPRD'  ,;
*!*          cAliasName WITH 'GLTMPBAL'  ,;
*!*          lOpenBefor WITH .F.


*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM1' ,;
*!*          cFileName  WITH 'GLACCHAR' ,;
*!*           cNdxName   WITH 'ACCTCODE'  ,;
*!*          cAliasName WITH 'GLACCHAR'  ,;
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM1' ,;
*!*          cFileName  WITH 'FSPRD'     ,;
*!*           cNdxName   WITH 'COMFYRPRDI',;
*!*          cAliasName WITH 'FSPRD'     ,;
*!*          lOpenBefor WITH .F.

*-- End of Setup Header and detail files of GL (lcRpItem1) --*

*!*  * Header info. about AP relabance information (lcRpItem2).
*!*  * --------------------------------------------------------
*!*  SELECT (lcRebalHdr)
*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM2'  ,;
*!*          cUpdFunNam WITH 'lfUpdItem2' ,;
*!*          cVryFunNam WITH 'lfVryItem2' ,;
*!*          cIgnFunNam WITH 'IgnName'    ,;
*!*          cUpdVryIgn WITH lcUpVrIg     ,;
*!*          cAddSetup  WITH 'lfStupItm2' ,;
*!*          cValidFun  WITH 'lfValdItm2'

*!*  * Detail info. about GL rebalance master files (lcRpItem2).
*!*  * --------------------------------------------------------
*!*  SELECT (lcRebalDtl)
*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM2' ,;
*!*          cFileName  WITH 'APVENDOR'  ,;
*!*           cNdxName   WITH 'VENCODE'   ,;
*!*          cAliasName WITH 'APVENDOR'  ,;
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM2' ,;
*!*          cFileName  WITH 'FISHD'     ,;
*!*           cNdxName   WITH 'COMPFYEAR' ,;
*!*          cAliasName WITH 'FISHD'     ,;
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM2' ,;
*!*        cFileName  WITH 'APDIST'    ,;
*!*           cNdxName   WITH 'INVVEND'   ,;
*!*          cAliasName WITH 'APDIST'    ,;
*!*        lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM2' ,;
*!*          cFileName  WITH 'APINVHDR'  ,;
*!*           cNdxName   WITH 'VENDINV'   ,;
*!*          cAliasName WITH 'APINVHDR'  ,;
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM2' ,;
*!*          cFileName  WITH 'APVENHST'  ,;
*!*           cNdxName   WITH 'VENDYEAR' ,;
*!*          cAliasName WITH 'APVENHST'  ,;
*!*          lOpenBefor WITH .F.
*!*  *-- End of Setup Header and detail files of AP (lcRpItem2) --*

* Insert Material on-hand information (lcRpItem3).
* ------------------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM3' ,;
        cUpdFunNam WITH 'lfUpdt'    ,;
        cVryFunNam WITH 'lfVerf'    ,;
        cIgnFunNam WITH 'lfIgn'     ,;
        cValidFun  WITH 'lfVldMat'  ,;
        cInvType   WITH '0002'      ,;
        cUpdVryIgn WITH  lcUpVrIg

* Insert Material on-order information (lcRpItem4).
* ------------------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM4' ,;
        cUpdFunNam WITH 'lfUpdt'    ,;
        cVryFunNam WITH 'lfVerf'    ,;
        cIgnFunNam WITH 'lfIgn'     ,;
        cValidFun  WITH 'lfVldMat'  ,;
        cInvType   WITH '0002'      ,;
        cUpdVryIgn WITH  lcUpVrIg

* Insert Style on-hand information (lcRpItem5).
* ----------------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM5' ,;
        cUpdFunNam WITH 'lfUpdt'    ,;
        cVryFunNam WITH 'lfVerf'    ,;
        cIgnFunNam WITH 'lfIgn'     ,;
        cValidFun  WITH 'lfVldMat'  ,;
        cInvType   WITH '0001'      ,;
        cUpdVryIgn WITH  lcUpVrIg

* Insert Style in transit information (lcRpItem6).
* ------------------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM6' ,;
        cUpdFunNam WITH 'lfUpdt'    ,;
        cVryFunNam WITH 'lfVerf'    ,;
        cIgnFunNam WITH 'lfIgn'     ,;
        cValidFun  WITH 'lfVldMat'  ,;
        cInvType   WITH '0001'      ,;
        cUpdVryIgn WITH  lcUpVrIg

* Insert Style work orders information (lcRpItem7).
* -------------------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM7' ,;
        cUpdFunNam WITH 'lfUpdt'    ,;
        cVryFunNam WITH 'lfVerf'    ,;
        cIgnFunNam WITH 'lfIgn'     ,;
        cValidFun  WITH 'lfVldMat'  ,;
        cInvType   WITH '0001'      ,;
        cUpdVryIgn WITH  lcUpVrIg

* Insert Style work in process information (lcRpItem8).
* -----------------------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM8' ,;
        cUpdFunNam WITH 'lfUpdt'    ,;
        cVryFunNam WITH 'lfVerf'    ,;
        cIgnFunNam WITH 'lfIgn'     ,;
        cValidFun  WITH 'lfVldMat'  ,;
        cInvType   WITH '0001'      ,;
        cUpdVryIgn WITH  lcUpVrIg

* Insert Style shipped quantity information (lcRpItem9).
* ------------------------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM9' ,;
        cUpdFunNam WITH 'lfUpdt'    ,;
        cVryFunNam WITH 'lfVerf'    ,;
        cIgnFunNam WITH 'lfIgn'     ,;
        cValidFun  WITH 'lfVldMat'  ,;
        cInvType   WITH '0001'      ,;
        cUpdVryIgn WITH  lcUpVrIg



*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM10' ,;
	      cUpdFunNam WITH 'lfUpdt'     ,;
        cVryFunNam WITH 'lfVerf'     ,;
        cIgnFunNam WITH 'lfIgn'      ,;
        cValidFun  WITH 'lfVldMat'   ,;
        cUpdVryIgn WITH  lcUpVrIg
*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]


*----------  Not in  this Release
*!*  * Insert Style allocated quantity information (lcRpItem10).
*!*  * ------------------------------------------------------
*!*  SELECT (lcRebalHdr)
*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM10' ,;
*!*          cUpdFunNam WITH 'lfUpdt'     ,;
*!*          cVryFunNam WITH 'lfVerf'     ,;
*!*          cIgnFunNam WITH 'lfIgn'      ,;
*!*          cValidFun  WITH 'lfVldMat'   ,;
*!*          cUpdVryIgn WITH  lcUpVrIg

* Insert Style ordered quantity information (lcRpItem11).
* ------------------------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM11' ,;
        cUpdFunNam WITH 'lfUpdt'     ,;
        cVryFunNam WITH 'lfVerf'     ,;
        cIgnFunNam WITH 'lfIgn'      ,;
        cValidFun  WITH 'lfVldMat'   ,;
        cInvType   WITH '0001'      ,;
        cUpdVryIgn WITH  lcUpVrIg

*----------  Not in  this Release
*!*  * Insert Return Authorization (lcRpItem12).
*!*  * -----------------------------------------
*!*  SELECT (lcRebalHdr)
*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM12' ,;
*!*          cUpdFunNam WITH 'lfUpdt'     ,;
*!*          cVryFunNam WITH 'lfVerf'     ,;
*!*          cIgnFunNam WITH 'lfIgn'      ,;
*!*          cValidFun  WITH 'lfVldMat'   ,;
*!*          cUpdVryIgn WITH  lcUpVrIg

*!*  * Insert Return Credit Memo (lcRpItem13).
*!*  * ---------------------------------------
*!*  SELECT (lcRebalHdr)
*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM13' ,;
*!*          cUpdFunNam WITH 'lfUpdt'     ,;
*!*          cVryFunNam WITH 'lfVerf'     ,;
*!*          cIgnFunNam WITH 'lfIgn'      ,;
*!*          cValidFun  WITH 'lfVldMat'   ,;
*!*          cUpdVryIgn WITH  lcUpVrIg


*: B608393,1 MMT 12/27/2007 Add RM to Reblanace Program[Start]
* Insert Return Authorization (lcRpItem12).
* -----------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM12' ,;
        cUpdFunNam WITH 'lfUpdt'     ,;
        cVryFunNam WITH 'lfVerf'     ,;
        cIgnFunNam WITH 'lfIgn'      ,;
        cValidFun  WITH 'lfVldMat'   ,;
        cUpdVryIgn WITH  lcUpVrIg

* Insert Return Credit Memo (lcRpItem13).
* ---------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM13' ,;
        cUpdFunNam WITH 'lfUpdt'     ,;
        cVryFunNam WITH 'lfVerf'     ,;
        cIgnFunNam WITH 'lfIgn'      ,;
        cValidFun  WITH 'lfVldMat'   ,;
        cUpdVryIgn WITH  lcUpVrIg
*: B608393,1 MMT 12/27/2007 Add RM to Reblanace Program[End]



* Header info. of AR invoice header (lcRpItem14).
* --------------------------------------------------------
lcFName  = '(Invoice C(6)|Ship N(7)|ShipAmt N(14,2)|cAdd_time C(11)|dAdd_Date D)'
lcMFiles = 'InvHdr,InvLine'

SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM14' ,;
        cUpdFunNam WITH 'lfUpdHdDtl' ,;
        cVryFunNam WITH 'lfVryHdDtl' ,;
        cIgnFunNam WITH 'IgnName'    ,;
        cUpdVryIgn WITH lcUpVrIg     ,;
        cAddSetup  WITH 'lfStpHdDtl' ,;
        cValidFun  WITH 'lfVldHdDtl' ,;
        cTempName  WITH lcArInv      ,;
        cInvType   WITH '0001'       ,;
        cFileStr   WITH lcFName      ,;
        cMFiles    WITH lcMFiles

* Detail info. of AR invoice header (lcRpItem14).
* --------------------------------------------------------
SELECT (lcRebalDtl)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM14' ,;
        cFileName  WITH 'INVHDR'     ,;
        cNdxName   WITH 'INVHDR'     ,;
        cAliasName WITH gfTempName() ,;
        cFileObj   WITH 'loInvHdr'   ,;
        lOpenBefor WITH .F.

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM14' ,;
        cFileName  WITH 'INVLINE'    ,;
        cNdxName   WITH 'INVLINE'    ,;
        cAliasName WITH gfTempName() ,;
        cFileObj   WITH 'loInvLine'  ,;
        lOpenBefor WITH .F.

* Header info. of SO sales order header (lcRpItem15).
* --------------------------------------------------------
lcFName  = '(cOrdType C(1)|Order C(6)|Open N(8)|OpenAmt N(13,2)|Cancel N(8)|CancelAmt N(13,2)|Book N(8)|BookAmt N(11,2)|Ship N(8)|ShipAmt N(14,2)|cAdd_time C(11)|dAdd_Date D)'
lcMFiles = 'OrdHdr,OrdLine'

SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM15' ,;
        cUpdFunNam WITH 'lfUpdHdDtl' ,;
        cVryFunNam WITH 'lfVryHdDtl' ,;
        cIgnFunNam WITH 'IgnName'    ,;
        cUpdVryIgn WITH lcUpVrIg     ,;
        cAddSetup  WITH 'lfStpHdDtl' ,;
        cValidFun  WITH 'lfVldHdDtl' ,;
        cTempName  WITH lcSoOrd      ,;
        cFileStr   WITH lcFName      ,;
        cInvType   WITH '0001'      ,;
        cMFiles    WITH lcMFiles

* Detail info. of SO sales order header (lcRpItem15).
* --------------------------------------------------------
SELECT (lcRebalDtl)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM15',;
        cFileName  WITH 'ORDHDR'    ,;
        cNdxName   WITH 'ORDHDR'    ,;
        cAliasName WITH gfTempName(),;
        cFileObj   WITH 'loOrdHdr'  ,;
        lOpenBefor WITH .F.

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM15' ,;
        cFileName  WITH 'ORDLINE'    ,;
        cNdxName   WITH 'ORDLINE'    ,;
        cAliasName WITH gfTempName() ,;
        cFileObj   WITH 'loOrdLine'  ,;
        lOpenBefor WITH .F.

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM15' ,;
        cFileName  WITH 'INVHDR'     ,;
        cNdxName   WITH 'INVHDR'     ,;
        cAliasName WITH gfTempName() ,;
        cFileObj   WITH 'loInvHdr'   ,;
        lOpenBefor WITH .F.

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM15' ,;
        cFileName  WITH 'INVLINE'    ,;
        cNdxName   WITH 'INVLINEO'   ,;
        cAliasName WITH gfTempName() ,;
        cFileObj   WITH 'loInvLine'  ,;
        lOpenBefor WITH .F.

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM15' ,;
        cFileName  WITH 'ORDCANLN'   ,;
        cNdxName   WITH 'ORDCANLN'   ,;
        cAliasName WITH gfTempName() ,;
        cFileObj   WITH 'loOrdCanLn' ,;
        lOpenBefor WITH .F.

*----------  Not in  this Release
*!*  * Header info. of CT header (lcRpItem16).
*!*  * ---------------------------------------
*!*  lcFName  = '(CutTkt C(6)|Pcs_Bud N(7)|Pcs_Rec N(7)|cAdd_time C(11)|dAdd_Date D)'
*!*  lcMFiles = 'CutTktH,CutTktL'

*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[Start]
lcFName  = '(cBusDocu C(1)|cStyType C(1)|Po C(6)|NStyOrder N(7)|Receive N(7)|Open N(7)|Cancel N(7)|Damage N(7)|cAdd_time C(11)|dAdd_Date D)'
lcMFiles = 'POSHDR,POSLN'
*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[End]



*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[Start]
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM16' ,;
        cUpdFunNam WITH 'lfUpdHdDtl' ,;
        cVryFunNam WITH 'lfVryHdDtl' ,;
        cIgnFunNam WITH 'IgnName'    ,;
        cUpdVryIgn WITH lcUpVrIg     ,;
        cAddSetup  WITH 'lfStpHdDtl' ,;
        cValidFun  WITH 'lfVldHdDtl' ,;
        cTempName  WITH lcMfCt     ,;
        cFileStr   WITH lcFName      ,;
        cMFiles    WITH lcMFiles   ,;
        cInvType   WITH '0001'

*!*  * Detail info. of CT header (lcRpItem16).
* ---------------------------------------
SELECT (lcRebalDtl)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM16',;
        cFileName  WITH 'POSHDR'   ,;
        cNdxName   WITH 'POSHDR'   ,;
        cAliasName WITH gfTempName()   ,;
        lOpenBefor WITH .F.  ,;
        cFileObj   WITH 'loCUTTKTH'  ,;
        lSQLFile   WITH .T.


APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM16' ,;
        cFileName  WITH 'POSLN'    ,;
        cNdxName   WITH 'POSLN'    ,;
        cAliasName WITH gfTempName()    ,;
        lOpenBefor WITH .F.           ,;
        cFileObj   WITH 'loCUTTKTL'  ,;
        lSQLFile   WITH .T.

*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[End]

* Header info. of Style PO header (lcRpItem17).
* ---------------------------------------
lcFName  = '(cBusDocu C(1)|cStyType C(1)|Po C(6)|NStyOrder N(7)|Receive N(7)|Open N(7)|Cancel N(7)|Damage N(7)|cAdd_time C(11)|dAdd_Date D)'
lcMFiles = 'POSHDR,POSLN'

SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM17' ,;
        cUpdFunNam WITH 'lfUpdHdDtl' ,;
        cVryFunNam WITH 'lfVryHdDtl' ,;
        cIgnFunNam WITH 'IgnName'    ,;
        cUpdVryIgn WITH lcUpVrIg     ,;
        cAddSetup  WITH 'lfStpHdDtl' ,;
        cValidFun  WITH 'lfVldHdDtl' ,;
        cTempName  WITH lcSPO        ,;
        cFileStr   WITH lcFName      ,;
        cInvType   WITH '0001'      ,;
        cMFiles    WITH lcMFiles

* Detail info. of CT header (lcRpItem17).
* ---------------------------------------
SELECT (lcRebalDtl)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM17',;
        cFileName  WITH 'POSHDR'    ,;
        cNdxName   WITH 'POSHDR'    ,;
        cAliasName WITH gfTempName(),;
        lOpenBefor WITH .F.         ,;
        cFileObj   WITH 'loPOSHDR'  ,;
        lSQLFile   WITH .T.

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM17',;
        cFileName  WITH 'POSLN'     ,;
        cNdxName   WITH 'POSLN'     ,;
        cAliasName WITH gfTempName(),;
        lOpenBefor WITH .F.         ,;
        cFileObj   WITH 'loPOSLN'   ,;
        lSQLFile   WITH .T.

* Header info. of Material PO header (lcRpItem18).
* ---------------------------------------

*B128464,2 WSH 08/21/2005 Add decimals for the Material PO Quantities. [Start]
lcFName  = '(cBusDocu C(1)|cStyType C(1)|Po C(6)|NStyOrder N(11,3)|Receive N(11,3)|Open N(11,3)|Cancel N(11,3)|Damage N(11,3)|cAdd_time C(11)|dAdd_Date D)'
lcMFiles = 'POSHDR,POSLN'
*B128464,2 WSH 08/21/2005 [End]

SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM18' ,;
        cUpdFunNam WITH 'lfUpdHdDtl' ,;
        cVryFunNam WITH 'lfVryHdDtl' ,;
        cIgnFunNam WITH 'IgnName'    ,;
        cUpdVryIgn WITH lcUpVrIg     ,;
        cAddSetup  WITH 'lfStpHdDtl' ,;
        cValidFun  WITH 'lfVldHdDtl' ,;
        cTempName  WITH lcSPO        ,;
        cFileStr   WITH lcFName      ,;
        cInvType   WITH '0002'       ,;
        cMFiles    WITH lcMFiles

* Detail info. of CT header (lcRpItem17).
* ---------------------------------------
SELECT (lcRebalDtl)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM18',;
        cFileName  WITH 'POSHDR'    ,;
        cNdxName   WITH 'POSHDR'    ,;
        cAliasName WITH gfTempName(),;
        lOpenBefor WITH .F.         ,;
        cFileObj   WITH 'loPOSHDR'  ,;
        lSQLFile   WITH .T.

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM18',;
        cFileName  WITH 'POSLN'     ,;
        cNdxName   WITH 'POSLN'     ,;
        cAliasName WITH gfTempName(),;
        lOpenBefor WITH .F.         ,;
        cFileObj   WITH 'loPOSLN'   ,;
        lSQLFile   WITH .T.

*----------  Not in  this Release
*!*  *-- This item is related to the AR Module.
*!*  SELECT (lcRebalHdr)

*!*  lcMFiles = 'InvHdr,InvLine,OrdHdr,RetHdr,Debit,Credit,ArHist,ArCusHst'

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM18' ,;
*!*          cUpdFunNam WITH 'lfUpdCsHst' ,;
*!*          cVryFunNam WITH 'lfVryCsHst' ,;
*!*          cIgnFunNam WITH 'IgnName'    ,;
*!*          cUpdVryIgn WITH lcUpVrIg     ,;
*!*          cAddSetup  WITH 'lfStpCsHst' ,;
*!*          cValidFun  WITH 'lfVldHdDtl' ,;
*!*          cTempName  WITH 'lcArCsHst'  ,;
*!*          cFileStr   WITH ''           ,;
*!*          cMFiles    WITH lcMFiles
*!*
*!*  * Detail info. of AR Customer History (lcRpItem18).
*!*  * --------------------------------------------------------
*!*  SELECT (lcRebalDtl)
*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM18' ,;
*!*          cFileName  WITH 'INVHDR'     ,;
*!*           cNdxName   WITH ''           ,;
*!*          cAliasName WITH 'INVHDR'     ,;
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM18' ,;
*!*          cFileName  WITH 'INVLINE'    ,;
*!*           cNdxName   WITH 'INVLINE'    ,;
*!*          cAliasName WITH 'INVLINE'    ,;
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM18' ,;
*!*          cFileName  WITH 'ORDHDR'     ,;
*!*           cNdxName   WITH ''           ,;
*!*          cAliasName WITH 'ORDHDR'     ,;
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM18' ,;
*!*          cFileName  WITH 'RETHDR'     ,;
*!*           cNdxName   WITH ''           ,;
*!*          cAliasName WITH 'RETHDR'     ,;
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM18' ,;
*!*          cFileName  WITH 'DEBIT'      ,;
*!*           cNdxName   WITH ''           ,;
*!*          cAliasName WITH 'DEBIT'      ,;
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM18' ,;
*!*          cFileName  WITH 'CREDIT'     ,;
*!*           cNdxName   WITH ''           ,;
*!*          cAliasName WITH 'CREDIT'     ,;
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM18' ,;
*!*          cFileName  WITH 'ARHIST'     ,;
*!*           cNdxName   WITH ''           ,;
*!*          cAliasName WITH 'ARHIST'     ,;
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM18' ,;
*!*          cFileName  WITH 'ARCUSHST'   ,;
*!*           cNdxName   WITH 'ACTHST'     ,;
*!*          cAliasName WITH 'ARCUSHST'   ,;
*!*          lOpenBefor WITH .F.

*!*  *-- This item is related to the IC Module.
*!*  SELECT (lcRebalHdr)
*!*  lcMFiles = 'InvHdr,InvLine,OrdHdr,OrdLine,RetHdr,RetLine,ICStyHst'

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM19' ,;
*!*          cUpdFunNam WITH 'lfUpdStHst' ,;
*!*          cVryFunNam WITH 'lfVryStHst' ,;
*!*          cIgnFunNam WITH 'IgnName'    ,;
*!*          cUpdVryIgn WITH lcUpVrIg     ,;
*!*          cAddSetup  WITH 'lfStpStHst' ,;
*!*          cValidFun  WITH 'lfVldHdDtl' ,;
*!*          cTempName  WITH 'lcIcStHst'   ,;
*!*          cFileStr   WITH ''           ,;
*!*          cMFiles    WITH lcMFiles

*!*  * Detail info. of IC Style History (lcRpItem19).
*!*  * --------------------------------------------------------
*!*  SELECT (lcRebalDtl)
*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM19' ,;
*!*          cFileName  WITH 'INVHDR'     ,;
*!*           cNdxName   WITH 'INVHDR'     ,;
*!*          cAliasName WITH 'INVHDR'     ,;
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM19' ,;
*!*          cFileName  WITH 'INVLINE'    ,;
*!*           cNdxName   WITH ''           ,;
*!*          cAliasName WITH 'INVLINE'    ,;
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM19' ,;
*!*          cFileName  WITH 'ORDHDR'     ,;
*!*           cNdxName   WITH 'ORDHDR'     ,;
*!*          cAliasName WITH 'ORDHDR'     ,;
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM19' ,;
*!*          cFileName  WITH 'ORDLINE'    ,;
*!*           cNdxName   WITH ''           ,;
*!*          cAliasName WITH 'ORDLINE'    ,;
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM19' ,;
*!*          cFileName  WITH 'RETHDR'     ,;
*!*           cNdxName   WITH 'RETHDR'     ,;
*!*          cAliasName WITH 'RETHDR'     ,;
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM19' ,;
*!*          cFileName  WITH 'RETLINE'    ,;
*!*           cNdxName   WITH ''           ,;
*!*          cAliasName WITH 'RETLINE'    ,;
*!*          lOpenBefor WITH .F.

*!*  APPEND BLANK
*!*  REPLACE cItemName  WITH 'LCRPITEM19' ,;
*!*          cFileName  WITH 'ICSTYHST'   ,;
*!*           cNdxName   WITH 'STYHST'     ,;
*!*          cAliasName WITH 'ICSTYHST'   ,;
*!*          lOpenBefor WITH .F.

*B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[Start]
* Insert Material Usage information (lcRpItem21).
* ------------------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM21' ,;
        cUpdFunNam WITH 'lfUpdt'    ,;
        cVryFunNam WITH 'lfVerf'    ,;
        cIgnFunNam WITH 'lfIgn'     ,;
        cValidFun  WITH 'lfVldMat'  ,;
        cInvType   WITH '0002'      ,;
        cUpdVryIgn WITH  lcUpVrIg
		
SELECT (lcRebalDtl)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM21' ,;
	    cFileName  WITH 'BOMCOST'     ,;
   	    cNdxName   WITH 'BOMCOST'     ,;
        cAliasName WITH gfTempName(),;
        lOpenBefor WITH .F.         ,;
        cFileObj   WITH 'loBOMCOST'  ,;
        lSQLFile   WITH .T.


APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM21' ,;
	    cFileName  WITH 'POSHDR'     ,;
   	    cNdxName   WITH 'POSHDR'     ,;
        cAliasName WITH gfTempName(),;
        lOpenBefor WITH .F.         ,;
        cFileObj   WITH 'loPOSHDR'  ,;
        lSQLFile   WITH .T.


*: B607982,1 MMT 12/18/2006 Fix bug of Wrong update of material usage[Start]
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM21' ,;
        cFileName  WITH 'BOMLINE'     ,;
        cNdxName   WITH 'BOMLINEU'     ,;
        cAliasName WITH gfTempName(),;
        lOpenBefor WITH .F.         ,;
        cFileObj   WITH 'loBOMLINE'  ,;
        lSQLFile   WITH .T.
*: B607982,1 MMT 12/18/2006 Fix bug of Wrong update of material usage[End]


* Insert Material In Transit information (lcRpItem22).
* ------------------------------------------------
SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM22' ,;
        cUpdFunNam WITH 'lfUpdt'    ,;
        cVryFunNam WITH 'lfVerf'    ,;
        cIgnFunNam WITH 'lfIgn'     ,;
        cValidFun  WITH 'lfVldMat'  ,;
        cInvType   WITH '0002'      ,;
        cUpdVryIgn WITH  lcUpVrIg
		
SELECT (lcRebalDtl)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM22' ,;
   	    cFileName  WITH 'POSLN'     ,;
   	    cNdxName   WITH 'POSLN'     ,;
        cAliasName WITH gfTempName(),;
        lOpenBefor WITH .F.         ,;
        cFileObj   WITH 'MATSHPDT'  ,;
        lSQLFile   WITH .T.
*B607883,1 MMT 12/18/2006 Fix bug of not updating material usage and intransit[End]

*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
lcFName  = '(PACK_NO C(6)|TOT_CART N(8)|TOT_PCS N(8)|TOT_WGHT N(13,2)|cAdd_time C(11)|dAdd_Date D)'
lcMFiles = 'PACK_HDR,PACK_LIN'

SELECT (lcRebalHdr)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM23' ,;
        cUpdFunNam WITH 'lfUpdHdDtl' ,;
        cVryFunNam WITH 'lfVryHdDtl' ,;
        cIgnFunNam WITH 'IgnName'    ,;
        cUpdVryIgn WITH lcUpVrIg     ,;
        cAddSetup  WITH 'lfStpHdDtl' ,;
        cValidFun  WITH 'lfVldHdDtl' ,;
        cTempName  WITH lcALPack ,;
        cFileStr   WITH lcFName      ,;
        cMFiles    WITH lcMFiles   ,;
        cInvType   WITH '0001'

SELECT (lcRebalDtl)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM23' ,;
         cFileName  WITH 'PACK_LIN'     ,;
         cNdxName   WITH 'PACK_LIN'     ,;
        cAliasName WITH gfTempName(),;
        lOpenBefor WITH .F.         ,;
        cFileObj   WITH 'loPACK_LIN'  ,;
        lSQLFile   WITH .F.

SELECT (lcRebalDtl)
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM23' ,;
         cFileName  WITH 'PACK_HDR'     ,;
         cNdxName   WITH 'PACK_HDR'     ,;
        cAliasName WITH gfTempName(),;
        lOpenBefor WITH .F.         ,;
        cFileObj   WITH 'loPACK_HDR'  ,;
        lSQLFile   WITH .F.
*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]

SET DATASESSION TO (lnDataSess)
*-- End of lfInfoHdr()
*!*************************************************************
*! Name      : lfCreateCursor
*! Developer : Wael M. Abo-Shawareb
*! Date      : 04/02/2005
*! Purpose   : Build SQL Cursor
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
FUNCTION lfCreateCursor
LPARAMETERS lnFiltType, lcFiltExp, lcCursName, lcStruct, lcRetFld, lcIndex

LOCAL lcTmpCurs, lcSQLRet, llFound, lnRet
llFound = .F.
lnRet   = 0  && Filter not found

lcTmpCurs = lfCheckFilter(lnFiltType, lcFiltExp)
IF !EMPTY(lcTmpCurs)
  SELECT (lcTmpCurs)
  llFound = (RECCOUNT() > 0)
  IF llFound
    IF !FILE(oAriaApplication.WorkDir + lcCursName + '.DBF')
      CREATE TABLE (oAriaApplication.WorkDir + lcCursName + '.DBF') ( &lcStruct. )
      INDEX ON &lcIndex TAG (lcCursName)
      USE IN (lcCursName)
    ENDIF

    USE (oAriaApplication.WorkDir + lcCursName) EXCLUSIVE IN 0 ALIAS (lcCursName)
    SELECT (lcCursName)
    ZAP

    SELECT (lcTmpCurs)
    SCAN
      SCATTER MEMVAR MEMO
      SELECT (lcCursName)
      APPEND BLANK
      GATHER MEMVAR MEMO
    ENDSCAN

    USE IN (lcCursName)
  ENDIF
ENDIF

RETURN lnRet

*************************************************************
*! Name      : lfCheckFilter
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/02/2005
*! Purpose   : Check if the filter was selected
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter

LOCAL lcReturn, lnPOS

DO CASE
  CASE lnArrayType = 1              && Fixed Filter
    lnPOS = ASCAN(loOgScroll.laOGFxFlt, lcFilter)
    IF lnPos > 0
      lnPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt, lnPos, 1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  CASE lnArrayType = 2             && Hidden Filter
    lnPOS = ASCAN(loOgScroll.laOGHDFlt, lcFilter)
    IF lnPos > 0
      lnPOS = ASUBSCRIPT(loOgScroll.laOGHDFlt, lnPos, 1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  CASE lnArrayType = 3           && Variable Filter
    lnPOS = ASCAN(loOgScroll.laOGvrFlt, lcFilter)
    IF lnPos > 0
      lnPOS = ASUBSCRIPT(loOgScroll.laOGvrFlt, lnPos, 1)
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  OTHERWISE :
    lcReturn = ""
ENDCASE
RETURN lcReturn

************************
FUNCTION gfGetOptionGridVars



RETURN "lcAria4Modules, lcToReb, lcCompCode, llPurge, llOgForAut, llAutoBal ,LANG_SMREBAL_REBLOG ,laRebMsg ,lcAria4Modules ,lcWinTitl,"+;
		"llOpenRep ,lcFilHandl ,llSaveAut ,laTempName ,lcInfoHdr ,lcInfoDtl ,"+;
		"lnInvAmt   ,lnDiscTakn ,lnDiscOffr,lnAdjtAmt ,lnAmtPaid,lnPurchAmt ,lcInvNo,"+;
		"lcTmpBBtch, lcTmpJBtch, lcTmpCBtch, lcTmpTran, lcTmpGLTHd, lcTmpGLTDt,"+;
		"llBalnBfor , lnPostTran ,lcRpFiscYr ,lcFiscYear,lcCurr_yer ,lnCurr_yer ,lcAcsMask ,lnAcsSegSz ,lcRpCmpExp ,lcRpModExp,"+;
		"lcCurrComp_ID,lcFilePath ,lcKeyVal ,laOldVal ,lcKeyType ,llAdoDye , llViewLog ,llChkInvAmt ,lcRebalHdr ,lcRebalDtl,"+;
		"lcSQLConStr, lnCompConnHand ,lcCurrComp_ID ,lcCompCode ,lcToReb ,llAutoBal, llExpr1 ,llClrReadN, "+;
		"laFChck,lcHigh,lcLow,lcInvN,lcOrdN,lcCTN,lcPON,lcPackNo,laFXFLT,llNComp ,"+;
		"loVendor , loWareHous ,loStyle_X ,lcCustomer,lcVendor ,lcWareHous ,lcStyle_X ,lcCustomer , lcSYCCOMP,"+;
		"lcSelInvNo,lcSelOrdNo,lcSelPONo ,lcSelAccount ,lcSelStyle ,lcSelFabric ,lcSelYear,lcSelCTNo,lcSPO,lcSoOrd,lcArInv,lcMfCt,"+;
		"lcPackSelect , lcALPack ,lnRemResult ,oProgress ,lnPrgSession ,lcComps ,lcModls ,lcPathStat ,lcAutoBal ,llExpr1,llUpdt,laRpSorCmp,"+;
		"laRpCmpCod,laRpCodMod"

	


FUNCTION gfInitOptionGridVars

*PUBLIC  lcToReb, lcCompCode, llPurge, llOgForAut, llAutoBal ,LANG_SMREBAL_REBLOG ,laRebMsg ,lcAria4Modules ,lcWinTitl
laRebMsg  = '' && Array holds the verify rebalance log message
*lcWinTitl = LANG_SMREBAL_REBLOG
lcToReb= .F.
lcCompCode= .F.
llPurge= .F.
llOgForAut= .F.
 llAutoBal = .F.
*-- Modules to convert in Aria4.
*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[Start]
*lcAria4Modules = 'AR,IC,MA,PO,SO'

*: B608393,1 MMT 12/27/2007 Add RM to Reblanace Program[Start]
*lcAria4Modules = 'AR,IC,MA,PO,SO,MF'

*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
*lcAria4Modules = 'AR,IC,MA,PO,SO,MF,RM'

*public lcAria4Modules
lcAria4Modules = 'AR,IC,MA,PO,SO,MF,RM,AL'

*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]

*: B608393,1 MMT 12/27/2007 Add RM to Reblanace Program[End]

*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[End]

*PUBLIC llOpenRep ,lcFilHandl ,llSaveAut ,laTempName ,lcInfoHdr ,lcInfoDtl ,LANG_SMREBAL_REB , LANG_SMREBAL_PROCESS

*!*		IF !llAutoBal
*!*		  llOpenRep  = .F.
*!*		  lcFilHandl = ''
*!*		 ELSE
*!*		  =FPUTS(lcFilHandl, LANG_SMREBAL_REB)
*!*		  =FPUTS(lcFilHandl, LANG_SMREBAL_PROCESS + DTOC(DATE()) + SPACE(5) + TIME())
*!*	    ENDIF

llSaveAut  = llOgForAut
laTempName = ""
lcInfoHdr  = ""
lcInfoDtl  = ""

*-- AP variables
*public lnInvAmt
       lnInvAmt   = 0                  && Varible to hold the invoice amount
*public lnDiscTakn
       lnDiscTakn = 0                   && Varible to hold the total discount taken per invoice
*public lnDiscOffr                    && Varible to hold the discount offeramount per invoice
       lnDiscOffr = 0
*public lnAdjtAmt
       lnAdjtAmt  = 0                  && Varible to hold the total adjustment amount per invoice
*public lnAmtPaid                    && Varible to hold the total paid amount per invoice
       lnAmtPaid  = 0
*public lnPurchAmt
	   lnPurchAmt = 0                   && Varible to hold the purchase amount per invoice
*public lcInvNo
       lcInvNo    = ""
                 && Varible to hold the A/P invoice no.

*-- GL variables
 *public lcTmpBBtch, lcTmpJBtch, lcTmpCBtch, lcTmpTran, lcTmpGLTHd, lcTmpGLTDt
 STORE '' TO lcTmpBBtch, lcTmpJBtch, lcTmpCBtch, lcTmpTran, lcTmpGLTHd, lcTmpGLTDt

*PUBLIC llBalnBfor , lnPostTran ,lcRpFiscYr ,lcFiscYear,lcCurr_yer ,lnCurr_yer ,lcAcsMask ,lnAcsSegSz ,lcRpCmpExp ,lcRpModExp

llBalnBfor = .F.
lnPostTran = 0           && Varible to hold the number of transactions that was Reposted in the Rebalance process
lcRpFiscYr = ""          && Variable holds the fiscal year if GL module was selected
lcFiscYear = ""          && Variable holds the fiscal year if GL module was selected
lcCurr_yer = ""
lnCurr_yer = 0
lcAcsMask  = ""
lnAcsSegSz = 0
lcRpCmpExp = ''
lcRpModExp = ''

*PUBLIC  lcCurrComp_ID,lcFilePath ,lcKeyVal ,laOldVal ,lcKeyType ,llAdoDye , llViewLog ,llChkInvAmt ,lcRebalHdr ,lcRebalDtl
lcCurrComp_ID = ''
lcFilePath  = oAriaApplication.DataDir
lcKeyVal    = ''            && Variable to hold the key that we seek with in the detail file while rebalance header&detail (ex. Invoice#)
laOldVal    = ''            && Varible to hold the old value in the option grid.
lcKeyType   = ''            && Variable to hold the order type.
llAdoDye    = .F.
llViewLog   = .F.

*B127969,1 WSH 05/25/2005 Add variable to not check Shipment Amount if the GMA Trigger exests. [Start]
llChkInvAmt = .T.
*B127969,1 WSH 05/25/2005 [End]
*PUBLIC laFChck
DIMENSION laFChck [2,6]  && Array to hold the fields that we will check while rebalancing header&detail.

*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
*STORE SPACE(6) TO lcHigh,lcLow,lcInvN,lcOrdN,lcCTN,lcPON
*PUBLIC lcHigh,lcLow,lcInvN,lcOrdN,lcCTN,lcPON,lcPackNo
STORE SPACE(6) TO lcHigh,lcLow,lcInvN,lcOrdN,lcCTN,lcPON,lcPackNo
*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]

IF !llAutoBal
  *PUBLIC laFXFLT
  DIMENSION laFXFLT [1,1]  && Array to hold the fix filter comming from the option grid.
ENDIF

DECLARE laRpCmpCod[1,3]  && Array to hold the companies information that will be rebalance.
DECLARE laRpCodMod[1,3]
DECLARE laRpSorCmp[1,3]
*-- Rebalance Options and Functions Tables...
lcRebalHdr = gfTempName()
lcRebalDtl = gfTempName()

*PUBLIC loVendor , loWareHous ,loStyle_X ,lcCustomer,lcVendor ,lcWareHous ,lcStyle_X ,lcCustomer , lcSYCCOMP
*-- Global variables to hold Remote Tables Object References and Cursors...
loVendor   = .F.
loWareHous = .F.
loStyle_X  = .F.
loCustomer = .F.
lcVendor   = gfTempName()
lcWareHous = gfTempName()
lcStyle_X  = gfTempName()
lcCustomer = gfTempName()
lcSYCCOMP  = gfTempName()

*PUBLIC lcSelInvNo,lcSelOrdNo,lcSelPONo ,lcSelAccount ,lcSelStyle ,lcSelFabric ,lcSelYear,lcSelCTNo,lcSPO,lcSoOrd,lcArInv,lcMfCt

*-- Global Cursors hold Selected Filter Values
lcSelInvNo   = gfTempName()
lcSelOrdNo   = gfTempName()
lcSelPONo    = gfTempName()
lcSelAccount = gfTempName()
lcSelStyle   = gfTempName()

*B608487,1 WAM 03/19/2008 Have a different temp table for selected fabrics
lcSelFabric  = gfTempName()
*B608487,1 WAM 03/19/2008 (End)
lcSelYear    = gfTempName()

*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[Start]
lcSelCTNo    = gfTempName()
*: B608024,1 MMT 03/29/2007 Fix bug of Not rebalancing manfucating module[End]

*-- TempCursors used for verifying data in Header files...
lcSPO    = gfTempName()
lcSoOrd  = gfTempName()
lcArInv  = gfTempName()
lcMfCt   = gfTempName()

*PUBLIC lcPackSelect , lcALPack ,lnRemResult ,oProgress ,lnPrgSession ,lcComps ,lcModls ,lcPathStat ,lcAutoBal ,llExpr1,llUpdt
*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [Start]
lcPackSelect   = gfTempName()
lcALPack       = gfTempName()
*: B608518,1 MMT 04/17/2008 Add Allocation Module To Reblanace [End]

lnPrgSession = SET("Datasession")

*-- Open Company System File...
lnRemResult = oAriaApplication.RemoteSystemData.Execute("SELECT * FROM SYCCOMP", '', lcSYCCOMP, "", oAriaApplication.SystemConnectionString, 3, "", lnPrgSession)
IF lnRemResult <> 1
  RETURN .F.
ENDIF

*-- Initialize the progress bar.
*oProgress = NEWOBJECT('AriaProgressBar', oAriaApplication.classdir + 'Utility.VCX')

STORE '' TO lcComps, lcModls
lcPathStat = SET('FULLPATH ')     && Varible to save the SET FULLPATH status
SET FULLPATH ON

*--Get Data Session number for the program as OG deals with its own Data Session


*-- Build tempoaray files and gathering all info. into this temp.
=lfInfoHdr()

lcAutoBal = ''
llExpr1   = .F.
llUpdt    = .F.

*B128464,2 WSH 08/21/2005 Use Fox Native SQL Commands insteed of RDAC to Enhance Performance. [Start]
* PUBLIC lcSQLConStr, lnCompConnHand ,lcCurrComp_ID ,lcCompCode ,lcToReb ,llAutoBal, llExpr1 ,llClrReadN
*B128464,2 WSH 08/21/2005 [End]


IF llAutoBal
   lcAutoBal = lcToReb
   lcToReb   = ''
ENDIF

IF TYPE('lcToReb') = 'C' AND !EMPTY(lcToReb)
  lcCurrComp_ID = lcCompCode
  = lpMainReb(lcToReb, '', 'U')
ELSE
  llNComp = .F.
ENDIF
llExpr1 = '.F.'

