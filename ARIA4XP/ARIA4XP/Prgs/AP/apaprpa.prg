***********************************************************************
*:  Program File: APAPRPA.prg
*:  Desc.       : Approve for Payment
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 12/13/2011
*:  Reference   : *E303011,1
*:************************************************************************
*:Mofications***********************************************************
*E303011,1 TMI 12/22/2011 [Start] refer to the local until the entry closes
*!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004]
*!B609890,1 SAB 04/26/2012 Diabled buttons after saving problemFix Forign [T20120304.0004]
*!B609971,1 MMT 06/21/2012 Fix bug of Approve for payment screen open scope OG after saving[T20120511.0006]
*!B610215,1 HIA 01/23/2013 Aria4xp - AP - Approve for payment bank account issue [T20130107.0001]
*!B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[T20140821.0012]
*:************************************************************************
#INCLUDE R:\aria4xp\PRGS\ap\APAPRPA.H
DO FORM (oAriaApplication.ScreenHome+"\AP\APAPRPA.SCX")

*!*************************************************************
*! Name      : lfFormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : E303011,1 TMI 12/13/2011
*! Purpose   : called from the Screen init Method
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet

SET MULTILOCKS ON
*- Open tables
=gfOpenTable('APSETUP','APSETUP','SH')
=gfOpenTable('ACCOD','ACCSEGNO','SH')
=gfOpenTable('APDIV','DIVISION','SH')
=gfOpenTable('apvendor','vencode','SH')
=gfOpenTable('apinvhdr','VENDINV','SH')
=gfOpenTable('APVENHST','VENDYEAR','SH')
=gfOpenTable('APDIST','INVVEND','SH')
=gfOpenTable('APCHECKS','BANKCHECK','SH')   && CBNKCODE+CCHKACCT
*E303011,1 TMI 01/29/2012 [Start]
=gfOpenTable(oAriaApplication.SysPath + 'SYCCURR' , 'CCURRCODE' , 'SH')
*E303011,1 TMI 01/29/2012 [End  ]

*- initializations
WITH loFormSet
  .nWorkArea = 'APINVHDR'
  .otoolbar.nWorkArea = 'APINVHDR'
  .DATAENVIRONMENT.INITIALSELECTEDALIAS = 'APINVHDR'
ENDWITH

*- Define variables
=lfDefineVars(loFormset)

*-- Define custom tool bar buttons
DECLARE loFormSet.lapanelobj[1,6]
STORE '' TO loFormSet.lapanelobj
*-- Scope Button
loFormSet.laPanelObj[1,1] = 'pbScop'
loFormSet.laPanelObj[1,2] = oAriaApplication.BitMapHome+"SCOPE.BMP"
loFormSet.laPanelObj[1,3] = 'lfGetData'
loFormSet.laPanelObj[1,4] = "Option Grid"
loFormSet.laPanelObj[1,5] = "Option Grid"
loFormSet.laPanelObj[1,6] = 'V'

*- Variable holding Filter expresion
lcExprsion = "((APINVHDR.NINVAMNT-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ) <> 0)"

*!*    *** Prepare an array to hold bank objects to be used for
*!*    *** global bank and checking accounts validations as follows :
*!*    *** One row for every object, such that
*!*    *** row no. 1 holds bank object names,
*!*    *** row no. 2 holds checking account object names
*!*    *** row no. 3 holds the corresponding G/L account object names,
*!*    *** Columns are ordered as follows :
*!*    *** Column no. 1 : invisible button name for corresponding object
*!*    *** Column no. 2 : object name (e.g. bank object name)
*!*    *** Column no. 3 : object description name(if required)
*!*    laBankObjs  = ' '
*!*    laBankObjs[1,1] = 'ibBank'         && Bank code invisible button
*!*    laBankObjs[1,2] = 'lcBankCode'     && Bank code
*!*    laBankObjs[2,1] = 'ibChecks'       && Checking account invisible button
*!*    laBankObjs[2,2] = 'lcCheckCode'     && Checking account
*!*    laBankObjs[3,1] = 'ibGlAcc'        && G/L account invisible button
*!*    laBankObjs[3,2] = 'lcGlAcct'     && G/L account
*!*
*!*  ELSE && If window exist
*!*    rbScopeOn=lnScopeOn  && restoring radio button value.
*!*    lcMethod   = lcOMethod
*!*    rbScope    = lnOScope
*!*    rbSort     = lnOSort
*!*    IF rbScope = 1
*!*      lcExprsion = "((APINVHDR.NINVAMNT -APINVHDR.NINVPAID - "+;
*!*                    "APINVHDR.NINVDISTK -APINVHDR.NINVADJ) <> 0)"

*!*    ELSE
*!*      =lfvOk() && returning the existing browse filter.
*!*    ENDIF
*!*  ENDIF

*!*  *** store scape trap.
*!*  lcEscTrp=ON("KEY","ESC")
*!*  *** Trap some keys before browse. ***
*!*  PUSH KEY
*!*  =lfPushKey()
*E303011,1 TMI 12/21/2011 [End  ]

SELECT CODES
=lfAddProp(loFormSet,'lcCodeFilt','CDIVISION ')
lcCodeFilt = loFormSet.lcCodeFilt
SET FILTER TO (CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N'+'&lcCodeFilt') OR;
  (CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N'+'N/A') OR;
  (CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N'+'ALL')

*- Set relation between invoice header, vendor, and division file
SELECT APVENHST
SET ORDER TO TAG VENDYEAR

SELECT APVENDOR
SET ORDER TO TAG VENCODE

SELECT APDIV
SET ORDER TO TAG DIVISION

SELECT APINVHDR
SET ORDER TO TAG VENDINV
lcCurYear=loFormSet.lcCurYear
SET RELATION TO APINVHDR.CVENDCODE+'&lcCurYear' INTO APVENHST ADDITIVE

SET RELATION TO APINVHDR.CVENDCODE           INTO APVENDOR ADDITIVE
SET RELATION TO APINVHDR.CDIVISION           INTO APDIV ADDITIVE

lcSavOrd = ORDER()
SET ORDER TO 0
SET FILTER TO APINVHDR.CVENDCODE+APINVHDR.CINVNO = '' .AND.;
  CINVSTAT<>'V' .AND.  EVALUATE(lcExprsion)
SET ORDER TO &lcSavOrd
GO TOP    &&Go to the top of the file.

*FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
*FUNCTION  to exchange currency from invoice currency to approved currency.
*FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
lcExSin4 = ' '

*- Change the calling of gfGetExSin so if the [BEGIN]
* Invoice currency is the base currency I make it the To currency
* and if not the Approve currency is the To currency
* lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,APINVHDR.cAprCurCod)
*- IF Statment to check if the Invoice currency is the
* same as the base currency
IF APINVHDR.cCurrCode = oAriaApplication.BaseCurrency
  lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cAprCurCod,APINVHDR.cCurrCode)
  lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
  lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
ELSE   && Else
  lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,APINVHDR.cAprCurCod)
ENDIF  && End of IF
*- Change the calling of gfGetExSin [END]

*lcExSin4 = IIF(lcExSin3 = '*','/','*')

glQuitting = .F.
IF EOF()
  ** MESSAGE : "There are no open invoices."
  **           "        � Ok �            "
  =gfModalGen("TRM04101B00000","DIALOG",'open invoices')
  glQuitting=.T.
ELSE
  IF EMPTY(APSETUP.CBNKCODE) .OR. EMPTY(APSETUP.CCHKACCT)
    ** MESSAGE : "You have to enter default bank code and checking account"
    **           "in the accounts payable setup. "
    **           "            � Ok �             "
    =gfModalGen("TRM04103B00000","DIALOG")
    glQuitting=.T.
  ELSE
    *,1 Call *.SPR from screens directory
    * DO APAPRPA.SPR
    *E303011,1 TMI 12/21/2011 [Start]
    *DO (gcScrDir + gcWinAppl + '\APAPRPA.SPR')
    *E303011,1 TMI 12/21/2011 [End  ]
    *,1 end

  ENDIF
ENDIF

SELECT CODES
SET FILTER TO

SELECT APINVHDR

*** Clear relation ***
SET FILTER TO
SET RELATION TO

*
***Release the browse window.
*RELEASE WINDOW (lcBrTtl)

IF glQuitting
  *!*    RELEASE WINDOW (lcVenSum)
  *!*    RELEASE WINDOW (lcInvSum)
  *!*    RELEASE POPUPS puDivision
  RETURN .F.
ENDIF
glFromBrow  = .F.

*- Create the temp table that holds the invoices that are ready to be approved
=lfAddProp(loFormSet,'lcAPINVHDR',gfTempName())
LOCAL laStru
DIMENSION laStru[1,18]
SELECT APINVHDR
=AFIELDS(laStru)

DIMENSION laIndex[2,2]
laIndex[1,1] = 'CINVNO+CVENDCODE'
laIndex[1,2] = 'INVVEND'
laIndex[2,1] = 'CVENDCODE+CINVNO'
laIndex[2,2] = 'VENDINV'
=gfCrtTmp(loFormSet.lcAPINVHDR,@laStru,@laIndex,loFormSet.lcAPINVHDR)

*- add property to create in it the class to call the vendor/ap invoice  screen
=lfAddProp(loFormSet,'clsVendCall' ,'')
loFormSet.clsVendCall = CREATEOBJECT('clsVendCall',loFormSet.lcAPINVHDR)

* Browse fields list is :
* CINVNO,CVENDCODE ,CVENPRIOR ,NINVAMTAP  ,NINVDISAP   ,NINVADJAP  ,NINVA1099,NINVAMNT,CVENPMETH,DINVDATE,DINVDUDAT  ,NTERDISCD  ,CDIVISION,CBNKCODE   ,CCHKACCT   ,CCHKGLACC
WITH loFormSet.ariaform1.grdAPROVE
  .RECORDSOURCE = ''
  .RECORDSOURCE = loFormSet.lcAPINVHDR
  LOCAL lcAPINVHDR
  lcAPINVHDR = loFormSet.lcAPINVHDR

  .Column1.CONTROLSOURCE = '&lcAPINVHDR..CINVNO'
  .Column1.Header1.CAPTION = LANG_APAPRPA_Inv_No           &&"Inv. No."

  .Column2.CONTROLSOURCE = ''
  *!*    .Column2.RemoveObject('Text1')
  *!*    .Column2.AddObject('Command1','commandbutton')
  *!*    .Column2.Sparse = .F.
  *!*    .Column2.CurrentControl = 'Command1'
  BINDEVENT(.Column2.Command1,"Click",loFormSet.clsVendCall,"lfInvoice")

  .Column3.CONTROLSOURCE = '&lcAPINVHDR..CVENDCODE'
  .Column3.Header1.CAPTION = LANG_APAPRPA_Vendor           &&"Vendor"

  .Column4.CONTROLSOURCE = ''
  *!*    .Column4.RemoveObject('Text1')
  *!*    .Column4.AddObject('Command1','commandbutton')
  *!*    .Column4.Sparse = .F.
  *!*    .Column4.CurrentControl = 'Command1'
  BINDEVENT(.Column4.Command1,"Click",loFormSet.clsVendCall,"lfVendor")

  .Column5.CONTROLSOURCE = '&lcAPINVHDR..CVENPRIOR'
  .Column5.Header1.CAPTION = LANG_APAPRPA_Priority         &&"P"

  .Column6.CONTROLSOURCE = '&lcAPINVHDR..NINVAMTAP'  &&
  .Column6.Header1.CAPTION = LANG_APAPRPA_Appr_to_pay      &&"Appr. to pay"

  .Column7.CONTROLSOURCE = '&lcAPINVHDR..NINVDISAP'  &&"
  .Column7.Header1.CAPTION = LANG_APAPRPA_Appr_Disc        &&"Appr. disc."

  .Column8.CONTROLSOURCE = '&lcAPINVHDR..NINVADJAP'  &&
  .Column8.Header1.CAPTION = LANG_APAPRPA_Appr_adj         &&"Appr. adj."

  .Column9.CONTROLSOURCE = IIF(loFormset.ap1.llApS1099,'','&lcAPINVHDR..NINVA1099')
  .Column9.Header1.CAPTION = LANG_APAPRPA_Approved_1099    &&"Approved 1099"

  .Column10.CONTROLSOURCE = '&lcAPINVHDR..NINVAMNT - &lcAPINVHDR..NINVPAID - &lcAPINVHDR..NINVDISTK - &lcAPINVHDR..NINVADJ'
  .Column10.Header1.CAPTION = LANG_APAPRPA_Open_Amount      &&"Open amount"
  *B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[T20140821.0012][Start]
  *.Column11.CONTROLSOURCE = 'lfGetPayMeth(Thisformset)'  
  .Column11.CONTROLSOURCE = 'Thisformset.lfGetAPRPayMeth()'
  *B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[T20140821.0012][End]
  .Column11.Header1.CAPTION = LANG_APAPRPA_Pay_Meth         &&"  Pay.  Meth  "

  .Column12.CONTROLSOURCE = '&lcAPINVHDR..DINVDATE'
  .Column12.Header1.CAPTION = LANG_APAPRPA_Inv_Date         &&"Inv. Date"

  .Column13.CONTROLSOURCE = '&lcAPINVHDR..DINVDUDAT'  &&
  .Column13.Header1.CAPTION = LANG_APAPRPA_Due_Date              &&"Due Date"

  .Column14.CONTROLSOURCE = '&lcAPINVHDR..NTERDISCD'  &&
  .Column14.Header1.CAPTION = LANG_APAPRPA_Discount_Days         &&"Discount Days"

  .Column15.CONTROLSOURCE = 'gfCodDes(&lcAPINVHDR..cDivision,"CDIVISION")'
  .Column15.Header1.CAPTION = LANG_APAPRPA_Division         &&"Division"

  .Column16.CONTROLSOURCE = '&lcAPINVHDR..CBNKCODE'  &&
  .Column16.Header1.CAPTION = LANG_APAPRPA_Bank             &&"Bank"

  .Column17.CONTROLSOURCE = '&lcAPINVHDR..CCHKACCT'  &&
  .Column17.Header1.CAPTION = LANG_APAPRPA_Checking_Account      &&"Checking Account"

  .Column18.CONTROLSOURCE = '&lcAPINVHDR..CCHKGLACC'  &&
  .Column18.Header1.CAPTION = LANG_APAPRPA_GL_Account       &&"GL Account"

  .READONLY = .T.
  .REFRESH()
  .DOSCROLL(2)
ENDWITH

*- Initialize the session fields
WITH loFormset.Ariaform1

  STORE .T. TO ;
    .lnSesAmnt.READONLY,;
    .lnSesDisc.READONLY,;
    .lnSesAdj.READONLY,;
    .lnSes1099.READONLY,;
    .txtInvoiceCur.READONLY,;
    .txtApprovCur.READONLY,;
    .txtApprovedAmount.READONLY

  STORE loFormset.llMultiCr TO ;
    .lblInvoiceCur.VISIBLE,;
    .lblApprovCur.VISIBLE,;
    .lblApprovedAmount.VISIBLE,;
    .txtInvoiceCur.VISIBLE,;
    .txtApprovCur.VISIBLE,;
    .txtApprovedAmount.VISIBLE

  IF !loFormset.llMultiCr
    .grdAprove.TOP = .grdAprove.TOP - 22
    .grdAprove.HEIGHT = .grdAprove.HEIGHT + 22
  ENDIF

  lcBaseSmbl = loFormset.lcBaseSmbl
  .lblApprovedAmt.CAPTION      = ALLTRIM('Appr. amount '+lcBaseSmbl)
  .lblApprovedDiscount.CAPTION = ALLTRIM('Appr. disc. ' +lcBaseSmbl)
  .lblApprovedAdj.CAPTION      = ALLTRIM('Appr. adj. '  +lcBaseSmbl)
  .lblApproved1099.CAPTION     = ALLTRIM('Appr. 1099 '  +lcBaseSmbl)

ENDWITH

*- add two properties for lcRpSortBy & lcRpListFor
=lfAddProp(loFormSet,'lcRpSortBy,lcRpListFor','')

*- fill up the temp file and collect data
=lfGetData(loFormset)

*- End of lfFormInit


***********************************************************************************
*Name    : lfGetData
*Developer : TMI - Tarek Mohamed Inbrahim
*Date    : 12/24/2011
*Purpose : called from the AfterRowColChange event in the grdApprove on the screen
***********************************************************************************
FUNCTION lfGetData
PARAMETERS loFormSet
LOCAL lcExpr

*- Change screen to the view mode
WITH loFormset.Ariaform1
  STORE 0 TO ;
    .lnSesAmnt.VALUE,;
    .lnSesDisc.VALUE,;
    .lnSesAdj.VALUE,;
    .lnSes1099.VALUE
ENDWITH

lcRpSortBy  = loFormSet.lcRpSortBy
lcRpListFor = loFormSet.lcRpListFor

*- Call the "Approve to Pay" OG
*!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004][Start]
*lcExpr = gfOpGrid('APAPRPA',.T. ,.F. ,.F. ,.T. ,.T.)
LOCAL lcSetProc
lcSetProc = SET("Procedure")
lcExpr = gfOpGrid('APAPRPA',.T. ,.F. ,.F. ,.T. ,.T.)
SET PROCEDURE TO &lcSetProc.
*!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004][End]

*-empty the temp file
SELECT (loFormSet.lcAPINVHDR)
ZAP

*- if cancel clicked , go to Select Mode
IF lcExpr == '.F.'
  loFormSet.ChangeMode('S')
  RETURN
ENDIF

loFormSet.lcRpSortBy  = lcRpSortBy
loFormSet.lcRpListFor = lcRpListFor
oAriaApplication.oToolBar.ChangeButtonStatus('pbRel','DISABLED')

*- Set the comparison operator based on if this is a Debit memo, Invoice or both
LOCAL lcCmpOpr,lcOrder
lcCmpOpr = IIF(lcRpListFor='I',">",IIF(lcRpListFor='D',"<","<>"))
lcOrder = IIF(lcRpSortBy='I','INVVEND','VENDINV')

SELECT APINVHDR
SCAN FOR Apinvhdr.CVENDCODE+Apinvhdr.CINVNO='' .AND.CINVSTAT<>"V" .AND. ;
    APINVHDR.NINVAMNT-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ &lcCmpOpr 0 AND ;
    &lcExpr

  SCATTER MEMVAR
  m.COWNER = ''  && use it later in the save process
  SELECT (loFormSet.lcAPINVHDR)
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN
SELECT (loFormSet.lcAPINVHDR)
SET ORDER TO &lcOrder
LOCATE

*- go to Edit mode when data is selected
loFormSet.ChangeMode('E')


*- End of lfGetData.

***********************************************************************************
*Name    : lfAfterRowColChange
*Developer : TMI - Tarek Mohamed Inbrahim
*Date    : 12/22/2011
*Purpose : called from the AfterRowColChange event in the grdApprove on the screen
***********************************************************************************
FUNCTION lfAfterRowColChange
PARAMETERS loFormset

LOCAL lcAPINVHDR
lcAPINVHDR = loFormSet.lcAPINVHDR

lcExSin4 = ' '
IF &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
  lcExSin3 = gfGetExSin(@lcExSin4,&lcAPINVHDR..cAprCurCod,&lcAPINVHDR..cCurrCode)
  lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
  lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
ELSE   && Else
  lcExSin3 = gfGetExSin(@lcExSin4,&lcAPINVHDR..cCurrCode,&lcAPINVHDR..cAprCurCod)
ENDIF  && End of IF

WITH loFormSet.Ariaform1
  .txtInvoiceCur.VALUE = &lcAPINVHDR..cCurrCode
  .txtApprovCur.VALUE = &lcAPINVHDR..cAprCurCod
  .txtApprovedAmount.VALUE = ;
    IIF(&lcAPINVHDR..nAprCurUnt > 0 ,;
    ROUND(&lcAPINVHDR..nInvAmtAp &lcEXSin3 &lcAPINVHDR..nAprExRat &lcEXSin4 &lcAPINVHDR..nAprCurUnt, 2), 0)
ENDWITH
*- End of lfAfterRowColChange.

***************************************************************************************
* Name      : clsVendCall
* Developer : Tmi Trek mohammed Ibfrahim
* Date      : 12/21/2011
* Purpose   : Define a class to bind its method to the button added to the grdApprove to call the vendor and appyinv screens
***************************************************************************************
DEFINE CLASS clsVendCall AS CUSTOM
  lcApinvhdr = ''
  PROCEDURE INIT
  PARAMETERS lcApinvhdr
  THIS.lcApinvhdr = lcApinvhdr
  ENDPROC

  PROCEDURE lfVendor
  *call the vendor screen here for vendor# :CVENDCODE
  LOCAL lcCVENDCODE
  lcCVENDCODE = EVALUATE(THIS.lcAPINVHDR+".CVENDCODE")
  oAriaApplication.DoProgram("AWRAPVENDR",'"&lcCVENDCODE"',.F.,"")
  ENDPROC

  PROCEDURE lfInvoice
  *call the Invoice screen here for Inv# :CINVNO
  LOCAL lcCVENDCODE,lcCINVNO
  SET STEP ON
  lcCVENDCODE = EVALUATE(THIS.lcAPINVHDR+".CVENDCODE")
  lcCINVNO    = EVALUATE(THIS.lcAPINVHDR+".CINVNO")
  *!B609857,1 SAB 03/11/2012 Solve Media reported Problems[Start]
  *oAriaApplication.DoProgram("AWRAPPYINV",'"&lcCVENDCODE.","&lcCINVNO."',.F.,"")
  oAriaApplication.DoProgram("AWRAPPYINV",'"&lcCVENDCODE.","&lcCINVNO."',.F.,"AP")
  *!B609857,1 SAB 03/11/2012 Solve Media reported Problems[End]
  ENDPROC

ENDDEFINE

***************************************************************************************
* Name      : lfDefineVars
* Developer : Tmi Trek mohammed Ibfrahim
* Date      : 12/21/2011
* Purpose   : Define all variables to be used in the screen
***************************************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormset
*---  Variables used
** laBankObjs has bank objects names for validations

** ibmethod     Variable to hold ib Method of payment.
** laCtrStat    To disable the browse pad in the menu
** cMarker      Vairable to mark current record.
**
** rbSort       Variable to hold rb Sort By.
** rbDuDsDt     Variable to hold rb due date and discount date.
** rbODuDsDt    Variable to hold rb due date and discount date.
** rbScope      Variable to hold rb Scope.
** rbAprAll     Variable to hold rb Apr with default or not in approve all.
** rbScopeOn    Variable to hold rb Scope On in rb Scope.
**
** puDivDes     Variable to hold pop up division.
** puDiv        Variable to hold pop up division.
** puDivision   Variable to hold pop up division.
**
** lnOSort      Variable to hold old rb Sort By.
** lnOScope     Variable to hold old rb Scope.
** lnMethod     Variable to hold ib Method of payment.
** lnDivDes     Variable to hold pu Division description.
** lnPyChMN     Variable to Display payment method in scope screen.
** lnTotPay     Variable to hold Total Payment.
** lnTotDisc    Variable to hold Total Discount.
** lnTotAdj     Variable to hold Total Adjustment.
** lnBrRecNo    Browse record number
** lnAprToPay   Variable to hold approve to pay amount.
** lnAprDisc    Variable to hold approve discount.
** lnAprAdj     Variable to hold approve adjustment.
** ln1099amnt   Variable to hold 1099 ammount.
** lnOldAprTPy  Variable to hold old approve to pay amount.
** lnOldAprDisc Variable to hold old approve discount.
** lnOldAprAdj  Variable to hold old approve adjustment.
** lnO1099amnt  Variable to hold old 1099 amount.
** lnSesAmnt    Variable to hold sesion approve amount.
** lnSesDisc    Variable to hold sesion approve discount.
** lnSesAdj     Variable to hold sesion approve adjustment.
** lnSes1099    Variable to hold sesion approve 1099.
** lnScopeOn    Variable to hole rb scope old value.
**
** lc1099St     Variable to hold approve 1099 state.
** lcFuncName   Variable to hold Refresh window.
** lcVenSum     Vendor  summary window name
** lcInvSum     Invoice summary window name
** lcBrowName   Variable to hold Brows Window name.
** lcReadNam1   Variable to hold Read Window name.
** lcReadNam2   Variable to hold Read Window name.
** lcExprsion   Variable to hold filter expresion.
** lcOldBank    Variable to hold old bank code.
** lcOldCheck   Variable to hold old check code.
** lcOldGlAcc   Variable to hold old GL account.
** lcHOldGlAc   Variable to hold old cash account.
** lcOldAcc     Variable to hold
** lcOldGLDes   Variable to hold old Account description.
** lcTAprAmnt   The Approve amount in approve message.
** lcDivision   Variable to hold division description.
** lcCInvRef    Variable to hold current Inv. Ref.
** lcPriority   Variable to hold Paroirity.
** lcPaymeth    Variable to hold pay method.
** lcOldDvCod   Variable to hold old division code.
** lcOMethod    Variable to hold old method.
** lcVend       Variable to hold Vendor Code to get tot payment for each vend.
** lcVendor     Variable to hold vendor code.
** lcTInvoice   Variable to hold word Invoice.
** lcTDebitM    Variable to hold word debit memo.
** lcTThrou     Variable to hold word through.
** lcTFrom      Variable to hold word From.
** lcOldPrty    Variable to hold old Paroirity.
** lcOldpymth   Variable to hold old pay method.
** lcTBlnkCode  Variable to hold text not aplicable.
** lcDivDisc    Variable to hold division discription.
** lcTInvNo     Variable to hold text
** lcTVendor    Variable to hold text
** lcTPrior     Variable to hold text
** lcTAprPay    Variable to hold text
** lcTAprDisc   Variable to hold text
** lcTAprAdj    Variable to hold text
** lcTApr1099   Variable to hold text
** lcTOpnAmnt   Variable to hold text
** lcTPayMeth   Variable to hold text
** lcTInvDate   Variable to hold text
** lcTDivDes    Variable to hold text
** lcTBank      Variable to hold text
** lcTGlAcct    Variable to hold text
** lcCurYear    Variable to hold current year.
** lcDivCode    Variable to hold division code.
** lcBrowFile   Vairable to hold the Browse File.
** lcVendTtl    Variable to hold vendor window title.
** lcInvoice    Variable to hold invoice window title.
** lcBrTtl      Variable to hold Browse Window Title.
** lcBankCode   Variable to hold bank code.
** lcCheckcode  Variable to hold check code.
** lcGlAcct     Variable to hold GL account.
** lcHGlAct     Variable to hold cash account.
** lcInvRef     Variable to hold Inv. Ref.
** lcOldInvRf   Variable to hold old Inv. Ref.
** lcCodeFilt   Variable to hold field filter.
** lcVendCode   Variable to hold vendor code initial val. must be null.
** lcOldVndcd   Variable to hold old vendor code initial val. must be null.
** lcVendComp   Variable to hold vendor company.
** lcOVendCod   Variable to hold old vendor code.
** lcOldVnCmp   Variable to hold old vendor company.
** lcOldDivDs   Variable to hold old division description.
** lcEscTrp     Variable to hold escape trap.
** lcOldVal     Variable to hold old field value from when functions

*  Added currency equation sign variables. (Begin)
** in case of exchange currency from invoice currency to company base currency.
** lcExSin1             Variable to hold the first sign in the equation.
** lcExSin2             Variable to hold the second sign in the equation.
** in case of exchange currency from invoice currency to approved currency.
** lcExSin3             Variable to hold the first sign in the equation.
** lcExSin4             Variable to hold the second sign in the equation.

** ldFrDueDat   Variable to hold from due date.
** ldToDueDat   Variable to hold to due date.
** ldFrDisDat   Variable to hold from discount date.
** ldToDisDat   Variable to hold to discount date.
** ldOFrDueDt   Variable to hold old from due date.
** ldOToDueDt   Variable to hold old to due date.
** ldOFrDisDt   Variable to hold old from discount date.
** ldOToDisDt   Variable to hold old to discount date.
**
** llScope      Variable to hold if you want scope to be done.
** llAprPart    Variable to hold parameter to branch between appr. fully and appr. part.
** llBrowse     Variable to hold flag in case of activate browse by mouse.
** llCanclAp    True if 'Cancel' is selected
** llVendSumm   True if vendor screen is open
** llExit       Flag indecate if the user want to exit thermo.
** llVendExst   If Vendor object is exist in scope screen.
** llNoContrl   No control panel (AP.PRG)
* lcTAprCurCod Text of field header of approved currency field
* lcTPayAcct   Text of cash payment account check
*********************

*- Create a property to hold the AP class as a reference
=lfAddProp(loFormSet,'AP1' ,'')
loFormSet.ap1 = CREATEOBJECT('AP')

*- Put all variables in a two diminsional array, one column for the variable name , the other for its initial value.
*- In the start of each function that changes the APINVHDR file initialize all the variables.
=lfAddProp(loFormSet,'laPropArr[1]' ,'Temp')

*- Define variables, initialize them with SPACE(1)
lcVars = "lc1099St   , lcFuncName , lcVenSum   , lcInvSum   ,"+;
  "cMarker    , lcBrowName , lcReadNam1 , lcReadNam2 ,"+;
  "lcExprsion , lcOldBank  , lcOldCheck , lcOldGlAcc ,"+;
  "lcHOldGlAc , lcOldAcc   , lcOldGLDes , lcTAprAmnt ,"+;
  "lcDivision , lcCInvRef  , lcPriority , lcPaymeth  ,"+;
  "lcOldDvCod , lcOMethod  , lcVend     , lcVendor   ,"+;
  "lcTInvoice , lcTDebitM  , lcTThrou   , lcTFrom    ,"+;
  "lcOldPrty  , lcOldpymth , lcOldVal   , lcTBlnkCode,"+;
  "lcDivDisc  , lcTInvNo   , lcTVendor  , lcTPrior   ,"+;
  "lcTAprPay  , lcTAprDisc , lcTAprAdj  , lcTApr1099 ,"+;
  "lcTOpnAmnt , lcTPayMeth , lcTInvDate , lcTDivDes  ,"+;
  "lcTBank    , lcTGlAcct  , lcCurYear  , puDiv      ,"+;
  "puDivision , lcTAprCurCod, lcTPayAcct, lcExSin1   ,"+;
  "lcExSin2   , lcExSin3    , lcExSin4"
=lfPopPropArray(loFormSet,'laPropArr',lcVars , ' ')

*-
lcVars = "lcVendCode , lcOldVndcd , lcVendComp , lcOVendCod ,"+;
  "lcOldVnCmp , lcOldDivDs , lcEscTrp"
=lfPopPropArray(loFormSet,'laPropArr',lcVars , '')

*-
lcVars = "rbSort     , lnOSort    , rbScope    , lnOScope   ,"+;
  "rbAprAll   , ibmethod   , puDivDes   , lnMethod   ,"+;
  "rbDuDsDt   , rbODuDsDt  , lnDivDes"
=lfPopPropArray(loFormSet,'laPropArr',lcVars , 1)

*-
lcVars = "lnPyChMN   , lnTotPay   , lnTotDisc  , lnTotAdj   ,"+;
  "lnBrRecNo"
=lfPopPropArray(loFormSet,'laPropArr',lcVars , 0)

*-
lcVars = "lnAprToPay , lnAprDisc  , lnAprAdj   , ln1099amnt ,"+;
  "lnOldAprTPy, lnOldAprAdj, lnO1099amnt, lnSesAmnt  ,"+;
  "lnSesDisc  , lnSesAdj   , lnSes1099  , lnOldAprDisc"
=lfPopPropArray(loFormSet,'laPropArr',lcVars , 0.00)

*-
lcVars = "rbScopeOn  , lnScopeOn"
=lfPopPropArray(loFormSet,'laPropArr',lcVars , 3)

*-
lcVars = "ldFrDueDat , ldToDueDat , ldFrDisDat , ldToDisDat ,"+;
  "ldOFrDueDt , ldOToDueDt , ldOFrDisDt , ldOToDisDt"
=lfPopPropArray(loFormSet,'laPropArr',lcVars , {})

*-
*lcVars = "llScope    , llAprPart  , llBrowse   , llCanclAp  ,"+;
"llVendSumm , llExit"
lcVars = "llScope    , llBrowse   , llCanclAp  ,"+;
  "llVendSumm , llExit"
=lfPopPropArray(loFormSet,'laPropArr',lcVars , .F.)

*-
lcVars = "llVendExst ,llNoContrl"
=lfPopPropArray(loFormSet,'laPropArr',lcVars , .T.)

*- single assigned vars
=lfPopPropArray(loFormSet,'laPropArr','lcBankCode' , SPACE(8))

=lfPopPropArray(loFormSet,'laPropArr','lcCheckcode',SPACE(12)    )

=lfPopPropArray(loFormSet,'laPropArr','lcInvRef,lcOldInvRf',SPACE(15)    )

=lfPopPropArray(loFormSet,'laPropArr','lcGlAcct,lcHGlAct', loFormset.ap1.lcEmptyAcc )

=lfPopPropArray(loFormSet,'laPropArr','lcInvoice,lcBrTtl',"Invoices"   )

=lfPopPropArray(loFormSet,'laPropArr','lcDivCode',"*")

=lfPopPropArray(loFormSet,'laPropArr','lcBrowFile',"APINVHDR")

=lfPopPropArray(loFormSet,'laPropArr','lcVendTtl',"Vendors")

=lfPopPropArray(loFormSet,'laPropArr','laCtrStat',"DISABLE")

*- Prepare currency fields
*- lcAprCurCod  : Approval currency code
*- lcAprCshCod  : Approval currency code
*- lnAprExRat   : Approval currency exchange rate between the
*-                approval currency and the invoice currency
*- lnAprCshRat  : Cash payment exchange rate field in APAPRALL.SPR
*- lnAprCurUnt  : Approval currency unit
*- lnExchAmnt   : Approved amount in approval currency
*- llMultiCr    : .T. if multi currency
*- llEdirEx     : .T. if editing an exchange rate per transaction
*-                is allowed
*- llExchRt     : .T. if adding an exchange rate on the fly is allowed
*- lcRateDisp   : Display status of the exchange rate field,
*-                in Approve partially screen, whether enabled
*-                or disabled
*- lnOldVal     : old exchange rate value
*- lcOldCurr    : old lcAprCurCod
*- lnOldUnt     : old lnAprCurUnt
*- lnOldExRat   : old lnAprExRat
*- lcTExRateMsg : message text

*-
lcVars = "lcAprCurCod, lcOldCurr, lcAprCshCod"
=lfPopPropArray(loFormSet,'laPropArr',lcVars , SPACE(5) )

*-
lcVars = "lnAprExRat, lnExchAmnt, lnAprCurUnt, lnOldVal,"+;
  "lnOldUnt, lnOldExRat, lnAprCshRat"
=lfPopPropArray(loFormSet,'laPropArr',lcVars , 0)
=lfPopPropArray(loFormSet,'laPropArr','lcTExRateMsg','The exchange rate|zero')
=lfAddProp(loFormSet,'lcTExRateMsg','')  && *x NOTE I added lcTExRateMsg as an element in the array laPropArr and as a property, this should be reviewed to check if it would be used

*-
lcVars = "llMultiCr, llEditEx, llExchRt"
=lfAddProp(loFormSet,lcVars,.F.)
=lfAddProp(loFormSet,'lcBaseSmbl','')

WITH loFormSet
  .lcTExRateMsg = 'The exchange rate|zero'
  IF gfGetMemVar('LLMULCURR')
    .llMultiCr  = .T.
    .llEditEx    = gfGetMemVar('LLEDITEXRA')
    .llExchRt    = gfGetMemVar('LLEXCHRATE')
    .lcBaseSmbl  = IIF(SEEK(oAriaApplication.BaseCurrency, 'SYCCURR'),;
      ALLTRIM(SYCCURR.cCurrSmbl), '')
  ENDIF
ENDWITH

*** Get current year form parent company.
lcCurYear = ''
SELECT SYCCOMP
LOCATE FOR CCOMP_ID = oAriaapplication.PRntcompanyid
IF FOUND()
  lcCurYear = CCURR_YER
ENDIF
=lfAddProp(loFormSet,'lcCurYear',lcCurYear)
=lfAddProp(loFormSet,'laPayMeth[1]' ,'')   && Array to hold Popups.
=lfAddProp(loFormSet,'laBankObjs[1]' ,'')
DIMENSION loFormSet.laBankObjs[3,3]

*** Get payment methods array
DIMENSION laPayMeth[1,2]
lnPayMLen = gfGetVld('CVENPMETH',@laPayMeth,.T.)
lnAryPos = ASCAN(laPayMeth,'C',1)
IF lnAryPos > 0
  =ADEL(laPayMeth,ASUBSCRIPT(laPayMeth, lnAryPos, 1))
  DIMENSION laPayMeth[ALEN(laPayMeth,1)-1,2]
ENDIF
DIMENSION loFormset.laPayMeth[ALEN(laPayMeth,1),ALEN(laPayMeth,2)]
=ACOPY(laPayMeth,loFormset.laPayMeth)

=lfPopPropArray(loFormSet,'laPropArr','lcPayMeth', laPayMeth[1,1] ) && Variable to hold method.
=lfPopPropArray(loFormSet,'laPropArr','lcMethod' , laPayMeth[1,2] ) && Variable to hold method code.
=lfAddProp(loFormSet,'lcTBlnkCode' ,gfCodDes(' ',' '))

*- End of lfDefineVars.

*!*************************************************************
*! Name      : lfAddProp
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : *E303011,1 TMI 12/17/2011
*! Purpose   : A function to add properties to the object that passed as a parameter
*!*************************************************************
FUNCTION lfAddProp
PARAMETERS loObj,lcPropName,PropValue
*- check if lcPropname is not just a variable, but a list of variables, then create a loop to walk around
LOCAL lnI,lnLen,lcPropToCreate
lcPropName = lcPropName + ','
lnLen = OCCURS(',',lcPropName)
FOR lnI = 1 TO lnLen
  lcPropToCreate = ALLTRIM(SUBSTR(lcPropName,1,AT(',',lcPropName)-1))
  IF TYPE('loObj.&lcPropToCreate')='U'
    loObj.ADDPROPERTY(lcPropToCreate,PropValue)
  ENDIF
  lcPropName = SUBSTR(lcPropName,AT(',',lcPropName)+1)
ENDFOR

************************************************************************************************
* Name         : lfPopPropArray
* Developer    : tmi - Tarek Mohamed Ibrahim
* Date         : 12/21/2011
* Purpose      : Put all variables in a two diminsional array, one column for the variable name , the other for its initial value.
*-             : In the start of each function that changes the APINVHDR file initialize all the variables.
************************************************************************************************
FUNCTION lfPopPropArray
LPARAMETERS loObj,lcArrPropName,lcVarsString,InitVal
LOCAL lnLen,lnOldLen,lnI,lnCommaPos

lcVarsString = lcVarsString+','
lnLen = OCCURS(',',lcVarsString)
lnOldLen = ALEN(loObj.&lcArrPropName,1)
DIMENSION loObj.&lcArrPropName.[lnOldLen+lnLen,2]
FOR lnI = 1 TO lnLen
  lnCommaPos = AT(',',lcVarsString)
  loObj.&lcArrPropName.[lnOldLen+lnI,1] = ALLTRIM(SUBSTR(lcVarsString,1,lnCommaPos-1))
  loObj.&lcArrPropName.[lnOldLen+lnI,2] = InitVal
  lcVarsString = SUBSTR(lcVarsString,lnCommaPos+1)
ENDFOR

*- End of lfPopPropArray.

*!*************************************************************
*! Name      : lfChangeMode
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : *E303011,1 TMI 12/13/2011
*! Purpose   : called from the Screen Change mode Method
*!*************************************************************
FUNCTION lfChangeMode
PARAMETERS loFormSet
DO CASE
CASE loFormSet.ActiveMode = 'S'

  loFormSet.oToolBar.cmdEdit.ENABLED = .F.
  oAriaApplication.oToolBar.ChangeButtonStatus('pbScop','ENABLED')

CASE loFormSet.ActiveMode = 'V'
  loFormSet.oToolBar.cmdEdit.ENABLED = .T.
  *!B609890,1 SAB 04/26/2012 Diabled buttons after saving problemFix Forign [T20120304.0004][Start]
  IF loFormSet.llAfterSave
    loFormSet.ChangeMode('E')
    loFormSet.llAfterSave = .F.
  ENDIF
  *!B609890,1 SAB 04/26/2012 Diabled buttons after saving problemFix Forign [T20120304.0004][End]

CASE loFormSet.ActiveMode = 'E'
  oAriaApplication.oToolBar.ChangeButtonStatus('pbScop','DISABLED')

ENDCASE

LOCAL llEdt
llEdt = (loFormSet.ActiveMode = 'E' AND !EOF(loFormset.lcAPINVHDR))
WITH loFormSet.AriaForm1
  STORE llEdt TO ;
    .cmdAprov.ENABLED ,;
    .cmdAprovePartially.ENABLED  ,;
    .cmdDisapproveAll.ENABLED  ,;
    .cmdApproveAll.ENABLED  ,;
    .cmdDisApprove.ENABLED
ENDWITH

WITH loFormSet.oToolBar
  .cmdSelect.ENABLED = .F.
  .cmdFind.ENABLED = .F.
  .cmdTop.ENABLED = .F.
  .cmdPrev.ENABLED = .F.
  .cmdNext.ENABLED = .F.
  .cmdEnd.ENABLED = .F.
ENDWITH
*- End of lfChangeMode.

*!*************************************************************
*! Name      : lfFormSavefiles
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/13/2011
*! Purpose   : Save data
*!*************************************************************
FUNCTION lfFormSavefiles
PARAMETERS loFormSet

LOCAL lnSlct,lnRec
lnSlct = SELECT(0)
SELECT (loFormSet.lcAPINVHDR)
lnRec = RECNO()

LOCATE
SCAN FOR COWNER = 'ARIA'
  *REPLACE LLOK_STAT WITH .F.
  SCATTER MEMVAR
  m.cOwner = ''
  lcKey = CVENDCODE+CINVNO

  SELECT APINVHDR
  =SEEK(lcKey,'APINVHDR','VENDINV')

  RLOCK()
  REPLACE CBNKCODE   WITH m.CBNKCODE   ;
    CCHKACCT   WITH m.CCHKACCT   ;
    CCHKGLACC  WITH m.CCHKGLACC  ;
    NINVAMTAP  WITH m.NINVAMTAP  ;
    NINVDISAP  WITH m.NINVDISAP  ;
    NINVADJAP  WITH m.NINVADJAP  ;
    nInvA1099  WITH m.nInvA1099  ;
    cAprCurCod WITH m.cAprCurCod ;
    nAprExRat  WITH m.nAprExRat  ;
    nAprCurUnt WITH m.nAprCurUnt ;
    nInvFAAp   WITH m.nInvFAAp   ;
    LLOK_STAT  WITH m.LLOK_STAT  ;
    CLOK_USER  WITH m.CLOK_USER  ;
    DLOK_DATE  WITH m.DLOK_DATE  ;
    CLOK_TIME  WITH m.CLOK_TIME  ;
    cEdit_User WITH oAriaApplication.User_ID ;
    dEdit_Date  WITH DATE()    ;
    cEdit_Time  WITH gfGetTime()

  *=gfAdd_Info('APINVHDR')
  UNLOCK
ENDSCAN

GOTO lnRec IN (loFormSet.lcAPINVHDR)
loFormSet.Ariaform1.grdAPROVE.REFRESH

*- Update
SELECT APINVHDR
=gfTableUpdate()
*!B609971,1 MMT 06/21/2012 Fix bug of Approve for payment screen open scope OG after saving[T20120511.0006][Start]
*!*	SELECT (loFormSet.lcAPINVHDR)
*!*	ZAP
*!*	=lfGetData(loFormset)
*!B609971,1 MMT 06/21/2012 Fix bug of Approve for payment screen open scope OG after saving[T20120511.0006][End]
*!B609890,1 SAB 04/26/2012 Diabled buttons after saving problemFix Forign [T20120304.0004][Start]
loFormSet.llAfterSave = .T.
*!B609890,1 SAB 04/26/2012 Diabled buttons after saving problemFix Forign [T20120304.0004][End]
*!B609971,1 MMT 06/21/2012 Fix bug of Approve for payment screen open scope OG after saving[T20120511.0006][Start]
loFormSet.ChangeMode('V')
*!B609971,1 MMT 06/21/2012 Fix bug of Approve for payment screen open scope OG after saving[T20120511.0006][END]
SELECT (lnSlct)
*- End of lfFormSavefiles

************************************************************************************************
*Name : lfFormUndo
*Developer :TMI - Tarek Mohamed Ibrahim
*Date:12/24/2011
*Purpose:undo procedure
************************************************************************************************
FUNCTION lfFormUndo
PARAMETERS loFormSet
lcAPINVHDR = loFormSet.lcAPINVHDR

*- Check if there are any updates, then notify the user with
SELECT (loFormSet.lcAPINVHDR)
LOCATE
LOCATE FOR COWNER = 'ARIA'  && this means that a line has been edited
IF FOUND()
  * Are you sure you want to �?
  * \!\<Yes;\?\<No
  IF gfModalGen('QRM40169B00006','DIALOG','lose all your changes') <> 1  &&
    RETURN
  ENDIF
ENDIF

*- Clear the locks
SELECT (loFormSet.lcAPINVHDR)
SCAN
  =SEEK(&lcAPINVHDR..CVENDCODE+&lcAPINVHDR..CINVNO,'APINVHDR','VENDINV')
  SELECT APINVHDR
  =gfObj_Lock(.F.)
ENDSCAN

*- Delete data
SELECT (loFormSet.lcAPINVHDR)
ZAP

*- Call the lfGetData again
*!*  loFormSet.ChangeMode('S')
*!*  loFormSet.ARIAFORM1.refresh()
=lfGetData(loFormSet)
*- End of lfFormUndo.


******************************************************************************************************************<
************************** from here the start of the original program's functions *******************************<
******************************************************************************************************************<




*!**************************************************************************
*!
*!      Function: lfGetPayMeth
*!
*!**************************************************************************
*B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[T20140821.0012][Start]
*FUNCTION lfGetPayMeth
FUNCTION lfGetAPRPayMeth
*B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[T20140821.0012][End]
PARAMETER loFormSet
*B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[T20140821.0012][Start]
SET DATASESSION TO loFormSet.DataSessionID
*B610850,1 MMT 09/11/2014 Error when user opens Payment, void payment, approve payment screens at the same time[T20140821.0012][End]

lcAPINVHDR = loFormSet.lcAPINVHDR
lcPayCode = &lcAPINVHDR..CVENPMETH

lnposition= ASCAN(loFormSet.laPayMeth,lcPayCode,1)

IF lnposition <> 0
  RETURN loFormSet.laPayMeth[ASUBSCRIPT(loFormSet.laPayMeth,lnposition,1),1]
ELSE
  RETURN " "
ENDIF


***********************************************************************************
*Name    : lfvDisAppr
*Developer : TMI - Tarek Mohamed Inbrahim
*Date    : 12/22/2011
*Purpose : * Disaprove the pointed record.
***********************************************************************************
FUNCTION lfvDisAppr
PARAMETERS loFormSet

*E303011,1 TMI 12/21/2011 [Start] define variables
LOCAL lnI,lcVar
FOR lnI = 1 TO ALEN(loFormset.laPropArr,1)
  lcVar = loFormset.laPropArr[lnI,1]
  &lcVar = loFormset.laPropArr[lnI,2]
ENDFOR
*- locate the same line in the APINVHDR file
lcAPINVHDR = loFormset.lcAPINVHDR
=SEEK(&lcAPINVHDR..CVENDCODE+&lcAPINVHDR..CINVNO,'APINVHDR','VENDINV')
=SEEK(&lcAPINVHDR..CVENDCODE,'APVENDOR','VENCODE')
*E303011,1 TMI 12/22/2011 [End  ]

lcExSin2 = ' '
lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
*lcExSin2 = IIF(lcExSin1 = '*','/','*')


*IF gfObj_lock(.T.) && lock the pointed record.
*E303011,1 TMI 12/22/2011 [Start] use the global function gfObj_lock
*IF lfObj_lock(.T.) && lock the pointed record.
SELECT APINVHDR
IF &lcAPINVHDR..COWNER='ARIA' OR gfObj_lock(.T.) && lock the pointed record.
  SELECT &lcAPINVHDR
  REPLACE COWNER WITH 'ARIA'  && this is used as a tag, this means that this record has been changed by the approve to pay program
  *E303011,1 TMI 12/22/2011 [End  ]

  lnRateVal = &lcAPINVHDR..nExRate &lcExSin2 &lcAPINVHDR..nCurrUnit
  WITH loFormSet.Ariaform1
    .lnSesAmnt.VALUE = .lnSesAmnt.VALUE  - ROUND(&lcAPINVHDR..nInvAmtAp &lcExSin1 lnRateVal, 2)
    .lnSesDisc.VALUE = .lnSesDisc.VALUE  - ROUND(&lcAPINVHDR..nInvDisAp &lcExSin1 lnRateVal, 2)
    .lnSesAdj.VALUE  = .lnSesAdj.VALUE   - ROUND(&lcAPINVHDR..nInvAdjAp &lcExSin1 lnRateVal, 2)
    .lnSes1099.VALUE = .lnSes1099.VALUE  - ROUND(&lcAPINVHDR..nInvA1099 &lcExSin1 lnRateVal, 2)
    .REFRESH()
  ENDWITH

  REPLACE &lcAPINVHDR..NINVAMTAP  WITH 0 ,;
    &lcAPINVHDR..NINVDISAP  WITH 0 ,;
    &lcAPINVHDR..NINVADJAP  WITH 0 ,;
    &lcAPINVHDR..NINVA1099  WITH 0 ,;
    &lcAPINVHDR..CBNKCODE   WITH '',;
    &lcAPINVHDR..CCHKACCT   WITH '',;
    &lcAPINVHDR..CCHKGLACC  WITH '',;
    &lcAPINVHDR..cAprCurCod WITH '',;
    &lcAPINVHDR..nAprCurUnt WITH 0,;
    &lcAPINVHDR..nAprExRat  WITH 0,;
    &lcAPINVHDR..nInvFAAp   WITH 0

  *=gfAdd_Info()  && Add the audit information to the record.
  *E303011,1 TMI 12/22/2011 [Start]
  *  =gfObj_lock(.F.)  && unlocking the pointed record
  *E303011,1 TMI 12/22/2011 [End  ]
  IF !BOF()
    SKIP -1
    ***Refresh the record pointer. ***
    lnBrRecNo  = RECNO('&lcAPINVHDR.')
    ***Refresh the invoice push button. ***
    ***Refresh say field open apen amount. ***
    loFormset.ariaform1.REFRESH()
  ENDIF
  *Old : SHOW WINDOW (lcBrTtl) REFRESH
ENDIF

loFormSet.Ariaform1.grdAprove.AFTERROWCOLCHANGE()
loFormSet.Ariaform1.REFRESH()
*- End of lfvDisAppr

***********************************************************************************
*Name      : lfvDApprAll
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Disapprove all records in browse.
***********************************************************************************
FUNCTION lfvDApprAll
PARAMETERS loFormSet
PRIVATE lnTotInv1 , lnTotInv2

*E303011,1 TMI 12/24/2011 [Start] define variables
LOCAL lnI,lcVar
FOR lnI = 1 TO ALEN(loFormset.laPropArr,1)
  lcVar = loFormset.laPropArr[lnI,1]
  &lcVar = loFormset.laPropArr[lnI,2]
ENDFOR
*- locate the same line in the APINVHDR file
lcAPINVHDR = loFormset.lcAPINVHDR
*E303011,1 TMI 12/24/2011 [End  ]

lnTotInv1   = 0
lnTotInv2   = 0
SELECT &lcAPINVHDR
lcSavOrd = ORDER()
SET ORDER TO 0
COUNT TO lnTotInv1
SET ORDER TO &lcSavOrd
***Exit from disapproving loop.
lcEscap=SET("ESCAPE")
SET ESCAPE ON
ON ESCAPE STORE .T. TO llExit

oProgress = NEWOBJECT('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
oProgress.TotalProgress = RECCOUNT(loFormset.lcAPINVHDR)
oProgress.lblFirstLabel.CAPTION = 'Approve All'
lnThermNo = 0
oProgress.SHOW()

SCAN
  =SEEK(&lcAPINVHDR..CVENDCODE+&lcAPINVHDR..CINVNO,'APINVHDR','VENDINV')
  =SEEK(&lcAPINVHDR..CVENDCODE,'APVENDOR','VENCODE')

  lnTotInv2 = lnTotInv2 + 1
  *=gfThermo(lnTotInv1,lnTotInv2,"Disapproving invoices...",;
  "Vendor: "+CVENDCODE+"  Invoice: "+CINVNO)



  *FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
  *FUNCTION  to exchange currency from invoice currency to company base currency.
  *FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
  lcExSin2 = ' '
  lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
  *lcExSin2 = IIF(lcExSin1 = '*','/','*')


  *IF gfObj_lock(.T.) && lock the pointed record.
  *E303011,1 TMI 12/22/2011 [Start]
  *IF lfObj_lock(.T.) && lock the pointed record.
  SELECT APINVHDR
  IF &lcAPINVHDR..COWNER='ARIA' OR gfObj_lock(.T.) && lock the pointed record.
    SELECT &lcAPINVHDR
    REPLACE COWNER WITH 'ARIA'  && this is used as a tag, this means that this record has been changed by the approve to pay program
    *E303011,1 TMI 12/22/2011 [End  ]

    lnRateVal = &lcAPINVHDR..nExRate &lcExSin2 &lcAPINVHDR..nCurrUnit
    WITH loFormSet.Ariaform1
      .lnSesAmnt.VALUE = .lnSesAmnt.VALUE  - ROUND(&lcAPINVHDR..nInvAmtAp &lcExSin1 lnRateVal, 2)
      .lnSesDisc.VALUE = .lnSesDisc.VALUE  - ROUND(&lcAPINVHDR..nInvDisAp &lcExSin1 lnRateVal, 2)
      .lnSesAdj.VALUE  = .lnSesAdj.VALUE   - ROUND(&lcAPINVHDR..nInvAdjAp &lcExSin1 lnRateVal, 2)
      .lnSes1099.VALUE = .lnSes1099.VALUE  - ROUND(&lcAPINVHDR..nInvA1099 &lcExSin1 lnRateVal, 2)
      .REFRESH()
    ENDWITH

    REPLACE &lcAPINVHDR..NINVAMTAP  WITH 0 ,;
      &lcAPINVHDR..NINVDISAP  WITH 0 ,;
      &lcAPINVHDR..NINVADJAP  WITH 0 ,;
      &lcAPINVHDR..NINVA1099  WITH 0 ,;
      &lcAPINVHDR..CBNKCODE   WITH '',;
      &lcAPINVHDR..CCHKACCT   WITH '',;
      &lcAPINVHDR..CCHKGLACC  WITH '',;
      &lcAPINVHDR..cAprCurCod WITH '',;
      &lcAPINVHDR..nAprCurUnt WITH 0,;
      &lcAPINVHDR..nAprExRat  WITH 0,;
      &lcAPINVHDR..nInvFAAp   WITH 0

    *=gfAdd_Info()  && Add the audit information to the record.
    *E303011,1 TMI 12/22/2011 [Start]
    *    =gfObj_lock(.F.) && lock the pointed record.
    *E303011,1 TMI 12/22/2011 [End  ]
  ENDIF
  lnThermNo = lnThermNo + 1
  oProgress.CurrentProgress(lnThermNo)
  oProgress.lblFirstLabel.CAPTION = APINVHDR.CVENDCODE
  IF llExit  &&If Escape pressed.
    lnTotInv1 = lnTotInv2
    *IF wvisible("GWDTHERMO")
    *  RELEASE WINDOW ("GWDTHERMO")
    *ENDIF
    llExit    = .F.
    EXIT
  ENDIF
ENDSCAN

oProgress = NULL

*- Check if the Thermometer window exist then force it to close
*!*  IF WEXIST('gwdThermo')
*!*    =gfThermo(lnTotInv1,lnTotInv1,"Disapproving invoices ...",;
*!*            "Vendor: "+CVENDCODE+"  Invoice: "+CINVNO)
*!*  ENDIF
GO BOTTOM
SET ESCAPE &lcEscap

*B500787,1 Removed browse activation and replaced it with a call to the
*B500787,1 BROWSE COMMAND WHEN function so as to refresh all data, as
*B500787,1 well as the browse and the file pointer with the values of the
*B500787,1 current record. (the last record in the browse in this case)
*ACTIVATE WINDOW (lcBrTtl)
*=lfwInvBrow()

loFormSet.Ariaform1.grdAprove.AFTERROWCOLCHANGE()
loFormSet.Ariaform1.REFRESH()
*- End of lfvDApprAll

***********************************************************************************
*Name      : lfvApprAll
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Saving the old values and calling approve all window
***********************************************************************************
FUNCTION lfvApprAll
PARAMETERS loFormSet
*E303011,1 TMI 12/21/2011 [Start] define variables
LOCAL lnI,lcVar
FOR lnI = 1 TO ALEN(loFormset.laPropArr,1)
  lcVar = loFormset.laPropArr[lnI,1]
  &lcVar = loFormset.laPropArr[lnI,2]
ENDFOR
*- locate the same line in the APINVHDR file
lcAPINVHDR = loFormset.lcAPINVHDR
=SEEK(&lcAPINVHDR..CVENDCODE+&lcAPINVHDR..CINVNO,'APINVHDR','VENDINV')
=SEEK(&lcAPINVHDR..CVENDCODE,'APVENDOR','VENCODE')
*E303011,1 TMI 12/22/2011 [End  ]

*** get default bank, checks, and gl account.
lcBankCode = APSETUP.CBNKCODE
lcCheckCode= APSETUP.CCHKACCT
*B600808,1 Default the G/L account whether or not it is empty.
*** get account if there is a bank code and check code available. ***
*IF EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')) ;
*  .AND. !EMPTY(lcBankCode) .AND. !EMPTY(lcCheckCode)
IF SEEK(lcBankCode+lcCheckCode,'APCHECKS')
  lcGlAcct = IIF(!EMPTY(APCHECKS.CCHKGLACC),APCHECKS.CCHKGLACC, loFormset.ap1.lcEmptyAcc)
  *B600808,1 Cash payment account is edfaulted by the
  *B600808,1 cash payment account in APSETUP and not bey lcGLAcct
  *lcHGlAct = lcGlAcct
  *- Get the currency of the checking account.
  lcAprCurCod = APCHECKS.cCurrCode
ENDIF

*- Get default Cash payment account.
lcHGlAct    = APSETUP.cCashAcct

*- Default the cash payment currency with the base currency.
lcAprCshCod = oAriaApplication.BaseCurrency

*B600808,1 There is no need to store the following values if we default
*B600808,1 every time we get into the screen.
*lcOldBank   = lcBankCode
*lcOldCheck  = lcCheckCode
*lcOldGlAcc  = lcGLAcct
*lcHOldGlAc  = lcHGlAct

*Old : PUSH KEY
*Old : ON KEY      && refresh the key traped

*,1 Call *.SPR from screens directory
* DO APAPRALL.SPR  && calling the approved all window.
DO FORM (oAriaApplication.ScreenHome+"\AP\APAPRALL.SCX") WITH loFormSet
*,1 end

*** Push the same keys again after coming ***
*** from global function browse. ***
* old : POP KEY

*B500787,1 Removed browse activation and replaced it with a call to the
*B500787,1 BROWSE COMMAND WHEN function so as to refresh all data, as
*B500787,1 well as the browse and the file pointer with the values of the
*B500787,1 current record. (the last record in the browse in this case)
*ACTIVATE WINDOW (lcBrTtl)
*Old : =lfwInvBrow()

loFormSet.Ariaform1.grdAprove.AFTERROWCOLCHANGE()
loFormSet.Ariaform1.REFRESH()
*- End of lfvApprAll

***********************************************************************************
*Name      : lfShAprAll
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : control showing objects in case of approve default or approve accounts entered.
***********************************************************************************
FUNCTION lfShAprAll
PARAMETERS loAprAllFormSet
loFormset = loAprAllFormSet.Callingform
*** in case of approve default disable all objects.
WITH loAprAllFormSet.Ariaform1
  IF rbAprAll = 1
    *Old :   SHOW GET lcBankCode  DISABLE
    *Old :   SHOW GET ibBank      DISABLE
    *Old :   SHOW GET lcCheckCode DISABLE
    *Old :   SHOW GET ibChecks    DISABLE
    .BankChk.ENABLED = .F.
    *Old :   SHOW GET lcGlAcct    DISABLE
    *Old :   SHOW GET lcHGlAct    DISABLE
    *Old :   SHOW GET ibGLAcc     DISABLE
    *Old :   SHOW GET ibHGLAcc    DISABLE
    .glChkActCode.ENABLED = .F.
    .glCashPayAcc.ENABLED = .F.
    *- Disable currency fields as well
    *Old :   SHOW GET ibAprCurCod DISABLE
    *Old :   SHOW GET lcAprCshCod DISABLE
    .kbAprCurrCode.ENABLED = .F.
    .kbCashPayCurr.ENABLED = .F.
    *** in case of approve accounts the user entered. ***
  ELSE
    lcOldBank   = lcBankCode   && get the bank old value.
    lcOldCheck  = lcCheckCode  && get the check old value.
    lcOldGlAcc  = lcGLAcct     && get the account old value.
    lcHOldGlAc  = lcHGlAct
    IF EMPTY(lcBankCode) && if empty of bank.
      *Old :     SHOW GET lcBankCode ENABLE
      *Old :     SHOW GET ibBank     ENABLE
      .BankChk.kbBanks.ENABLED = .T.
    ELSE
      IF EMPTY(lcCheckCode) && if empty of check.
        *Old :       SHOW GET lcBankCode  ENABLE
        *Old :       SHOW GET ibBank      ENABLE
        *Old :       SHOW GET lcCheckCode ENABLE
        *Old :       SHOW GET ibChecks    ENABLE
        .BankChk.ENABLED = .T.
      ELSE                  && if empty of account.
        *Old : SHOW GET lcBankCode  ENABLE
        *Old : SHOW GET ibBank      ENABLE
        *Old : SHOW GET lcCheckCode ENABLE
        *Old : SHOW GET ibChecks    ENABLE
        *Old : SHOW GET lcGlAcct    ENABLE
        *Old : SHOW GET ibGLAcc     ENABLE
        .BankChk.ENABLED = .T.
        .glChkActCode.ENABLED = .T.
        .glCashPayAcc.ENABLED = .T.

      ENDIF
    ENDIF

    *- Enable cash payment account any way
    *Old : SHOW GET lcHGlAct    ENABLE
    *Old : SHOW GET ibHGLAcc    ENABLE
    .glCashPayAcc.ENABLED = .T.

    IF lcHGlAct <> loFormset.ap1.lcEmptyAcc

      *Old : SHOW GET ibAprCurCod ENABLE
      *Old : SHOW GET lcAprCshCod ENABLE
      .kbCashPayCurr.ENABLED = .T.
    ENDIF

  ENDIF
  *Old : SHOW GET pbCancel ENABLE
  .cmdCancel.ENABLED = .T.
ENDWITH
*- End of lfShAprAll.

***********************************************************************************
*Name      : lfvpbAprAl
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : approve the all browse record in the file.
***********************************************************************************
FUNCTION lfvpbAprAl
PARAMETERS loAprAllFormSet
PRIVATE lcCurAlias, lnTotInv1 , lnTotInv2
loFormSet = loAprAllFormSet.Callingform

lnTotInv1   = 0
lnTotInv2   = 0

*B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
lcVen0Prior = ''

* Old : SELECT APINVHDR
*E303011,1 TMI 12/24/2011 [Start]
SELECT &lcAPINVHDR
*E303011,1 TMI 12/24/2011 [End  ]

lcSavOrd = ORDER()

SET ORDER TO 0
COUNT TO lnTotInv1
SET ORDER TO &lcSavOrd
**Exit from approving loop.**
lcEscap=SET("ESCAPE")
SET ESCAPE ON
ON ESCAPE STORE .T. TO llExit

*** in case of appove with default values. ***
WITH loFormSet.Ariaform1
  .lnSesAmnt.VALUE = 0
  .lnSesDisc.VALUE = 0
  .lnSesAdj.VALUE  = 0
  .lnSes1099.VALUE = 0
ENDWITH

* Old : lnSes1099 = 0
* Old : SHOW GET lnSes1099 LEVEL RDLEVEL()-1

IF rbAprAll = 1

  oProgress = NEWOBJECT('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
  oProgress.TotalProgress = RECCOUNT(loFormset.lcAPINVHDR)
  oProgress.lblFirstLabel.CAPTION = 'Approve All'
  lnThermNo = 0
  oProgress.SHOW()

  SCAN
    *E303011,1 TMI 12/24/2011 [Start]
    =SEEK(&lcAPINVHDR..CVENDCODE+&lcAPINVHDR..CINVNO,'APINVHDR','VENDINV')
    =SEEK(&lcAPINVHDR..CVENDCODE,'APVENDOR','VENCODE')
    *E303011,1 TMI 12/24/2011 [End  ]

    *B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
    IF !((CVENPRIOR = '0') .OR. (APVENDOR.CVENPRIOR = '0'))

      lnTotInv2 = lnTotInv2 + 1
      *=gfThermo(lnTotInv1,lnTotInv2,"Approving invoices...","Vendor: "+CVENDCODE+"  Invoice: "+CINVNO)

      *IF gfObj_lock(.T.) && lock the pointed record.
      *E303011,1 TMI 12/22/2011 [Start]
      *IF lfObj_lock(.T.) && lock the pointed record.
      SELECT APINVHDR
      IF &lcAPINVHDR..COWNER='ARIA' OR gfObj_lock(.T.) && lock the pointed record.
        SELECT &lcAPINVHDR
        REPLACE COWNER WITH 'ARIA'  && this is used as a tag, this means that this record has been changed by the approve to pay program
        *E303011,1 TMI 12/22/2011 [End  ]

        ***If the vendor or invoice is on hold. ***
        *B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
        *IF (CVENPRIOR <> '0') .AND. (APVENDOR.CVENPRIOR <> '0')
        IF &lcAPINVHDR..CVENPMETH <> 'H'
          lcBankCode   = CBNKCODE && get bank initial value.
          lcCheckCode  = CCHKACCT
          lcGlAcct     = IIF(!EMPTY(CCHKGLACC),CCHKGLACC,loFormset.ap1.lcEmptyAcc)
          *B600808,1 Cash payment account does not default from
          *B600808,1 the bank/checking account.
          *lcHGlAct   = lcGlAcct

          IF EMPTY(lcBankCode) && if there is no initial value.
            DO CASE
            CASE !EMPTY(APVENDOR.CBNKCODE) && try to get approve value form vendor file.
              lcBankCode = APVENDOR.CBNKCODE
              lcCheckCode= APVENDOR.CCHKACCT
              *B600808,1 Clear G/L account
              lcGlAcct   = loFormset.ap1.lcEmptyAcc

            CASE !EMPTY(APDIV.CBNKCODE)    && try to get approve value form division file.
              lcBankCode = APDIV.CBNKCODE
              lcCheckCode= APDIV.CCHKACCT
              *B600808,1 Clear G/L account
              lcGlAcct   = loFormset.ap1.lcEmptyAcc

            OTHERWISE                      && try to get approve value form setup file.
              lcBankCode = APSETUP.CBNKCODE
              lcCheckCode= APSETUP.CCHKACCT
              *B600808,1 Clear G/L account
              lcGlAcct   = loFormset.ap1.lcEmptyAcc
            ENDCASE

            *B600808,1 Add an ENDIFto close the empty bank condition
            *B600808,1 here. Get the defaults anyway.
          ENDIF

          *** get account value in case of empty and the is bank and check.
          *B600808,1 Move condition below.
          * IF EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')) ;
          .AND. !EMPTY(lcBankCode) .AND. !EMPTY(lcCheckCode)
          IF SEEK(lcBankCode+lcCheckCode,'APCHECKS')
            *lcGlAcct = IIF(!EMPTY(APCHECKS.CCHKGLACC),APCHECKS.CCHKGLACC,lcEmptyAcc)
            *B600808,1 No cash payment account
            *lcHGlAct = lcGlAcct
            *** replace the default values got to the file. ***
            *B600808,1 Check here for empty account
            IF EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0'))
              lcGlAcct = IIF(!EMPTY(APCHECKS.CCHKGLACC),APCHECKS.CCHKGLACC,loFormset.ap1.lcEmptyAcc)
            ENDIF

            *- Get default currency of the checking account.
            lcAprCurCod   = APCHECKS.cCurrCode

            *- Get a corresponding exchange rate
            *- between the approval currency and the invoice currency.
            *- If the currency is different, implies that the system
            *- is multi currency, call the global function gfChkRate
            *- that validates an exchange rate.

            IF &lcAPINVHDR..cCurrCode <> lcAPrCurCod
              IF &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
                lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCurCod ,&lcAPINVHDR..dInvDate, ;
                  .T., .F., &lcAPINVHDR..cCurrCode, .T.)
              ELSE      && Else
                lnAprExRat = gfChkRate('lnAprCurUnt', &lcAPINVHDR..cCurrCode ,&lcAPINVHDR..dInvDate, ;
                  .T., .F., lcAprCurCod, .T.)
              ENDIF     && End of IF
              *- Change the calling of gfChkRate [END]

              *B600849,1 (End)
            ELSE
              STORE 1 TO lnAprExRat, lnAprCurUnt
            ENDIF      && ENDIF APINVHDR.cCurrCode <> lcAPrCurCod
          ENDIF

          *FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
          *FUNCTION  to exchange currency from invoice currency to company base currency.
          *FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
          lcExSin2 = ' '
          lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
          *lcExSin2 = IIF(lcExSin1 = '*','/','*')
          *FUNCTION  HISH 12/05/95. to exchange currency from invoice currency to approved currency.
          lcExSin4 = ' '

          *- Change the calling of gfGetExSin so if the [BEGIN]
          * Invoice currency is the base currency I make it the To currency
          * and if not the Approve currency is the To currency
          *lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
          *- IF Statment to check if the Invoice currency is the
          * same as the base currency
          IF &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
            lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,&lcAPINVHDR..cCurrCode)
            lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
            lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
          ELSE   && Else
            lcExSin3 = gfGetExSin(@lcExSin4,&lcAPINVHDR..cCurrCode,lcAprCurCod)
          ENDIF  && End of IF
          *- Change the calling of gfGetExSin [END]

          *lcExSin4 = IIF(lcExSin3 = '*','/','*')


          *- If the exchange rate = 0, do not approve, and present
          *- the following message
          IF lnAprExRat = 0
            IF &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
              =gfModalGen("INM04157B00000", "DIALOG",;
                ALLTRIM(lcAprCurCod)+'|' ;
                +ALLTRIM(&lcAPINVHDR..cCurrCode)+;
                '|'+DTOC(&lcAPINVHDR..dInvDate))
            ELSE    && Else
              =gfModalGen("INM04157B00000", "DIALOG",;
                ALLTRIM(&lcAPINVHDR..cCurrCode)+'|' ;
                +ALLTRIM(lcAprCurCod)+;
                '|'+DTOC(&lcAPINVHDR..dInvDate))
            ENDIF     && End of IF

          ELSE

            REPLACE CBNKCODE   WITH lcBankCode ,;
              CCHKACCT   WITH lcCheckCode,;
              CCHKGLACC  WITH IIF(EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')),;
              SPACE(lnApsAcLen),lcGlAcct),;
              NINVAMTAP  WITH (&lcAPINVHDR..NINVAMNT-&lcAPINVHDR..NINVPAID-&lcAPINVHDR..NINVDISTK-&lcAPINVHDR..NINVADJ-&lcAPINVHDR..NINVDISOF),;
              NINVDISAP  WITH &lcAPINVHDR..NINVDISOF,;
              NINVADJAP  WITH 0,;
              nInvA1099  WITH 0,;
              cAprCurCod WITH lcAprCurCod,;
              nAprCurUnt WITH lnAprCurUnt,;
              nAprExRat  WITH lnAprExRat,;
              nInvFAAp   WITH ROUND((&lcAPINVHDR..NINVAMNT-&lcAPINVHDR..NINVPAID-&lcAPINVHDR..NINVDISTK-&lcAPINVHDR..NINVADJ-&lcAPINVHDR..NINVDISOF) &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2)

            *B601526,1 Change this line [End]

            *=gfAdd_Info('&lcAPINVHDR.')  && Add the audit information to the record.
          ENDIF
        ELSE     && If Cash Payment
          DO CASE
            *B600808,1 Add a case to get the cash payment account of
            *B600808,1 the invoice,
          CASE !EMPTY(&lcAPINVHDR..cChkGLAcc)
            lcHGlAcct   = CCHKGLACC
          CASE !EMPTY(APVENDOR.CCASHACCT)
            lcHGlAct= APVENDOR.CCASHACCT
          CASE !EMPTY(&lcAPINVHDR..CDIVISION) .AND. !EMPTY(APDIV.CCASHACCT)
            lcHGlAct= APDIV.CCASHACCT
          OTHERWISE
            lcHGlAct = IIF(!EMPTY(APSETUP.CCASHACCT),;
              APSETUP.CCASHACCT,loFormset.ap1.lcEmptyAcc)
          ENDCASE

          REPLACE CCHKGLACC  WITH IIF(EMPTY(STRTRAN(STRTRAN(lcHGlAct,'-'),'0')),;
            SPACE(lnApsAcLen), lcHGlAct),;
            NINVAMTAP  WITH (&lcAPINVHDR..NINVAMNT-&lcAPINVHDR..NINVPAID-&lcAPINVHDR..NINVDISTK-&lcAPINVHDR..NINVADJ-&lcAPINVHDR..NINVDISOF),;
            NINVDISAP  WITH &lcAPINVHDR..NINVDISOF,;
            NINVADJAP  WITH 0,;
            nInvA1099  WITH 0,;
            cAprCurCod WITH &lcAPINVHDR..cCurrCode,;
            nAprCurUnt WITH 1,;
            nAprExRat  WITH 1,;
            nInvFAAp   WITH ROUND((&lcAPINVHDR..NINVAMNT-&lcAPINVHDR..NINVPAID-&lcAPINVHDR..NINVDISTK-&lcAPINVHDR..NINVADJ-&lcAPINVHDR..NINVDISOF) &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2)

          *=gfAdd_Info('&lcAPINVHDR.')  && Add the audit information to the record.
        ENDIF

        *B601013- HISH  04/18/96. Got the equation signs. (Begin)
        *B601013-  to exchange currency from invoice currency to company base currency.
        lcExSin2 = ' '
        lcExSin1 = gfGetExSin(@lcExSin2,&lcAPINVHDR..cCurrCode)

        lnRateVal = &lcAPINVHDR..nExRate &lcExSin2 &lcAPINVHDR..nCurrUnit
        WITH loFormSet.Ariaform1
          .lnSesAmnt.VALUE = .lnSesAmnt.VALUE + ROUND(&lcAPINVHDR..nInvAmtAp &lcExSin1 lnRateVal, 2)
          .lnSesDisc.VALUE = .lnSesDisc.VALUE + ROUND(&lcAPINVHDR..nInvDisAp &lcExSin1 lnRateVal, 2)
        ENDWITH

        *E303011,1 TMI 12/22/2011 [Start]
        *=gfObj_lock(.F.) && lock the pointed record.
        *E303011,1 TMI 12/22/2011 [End  ]
      ENDIF
      IF llExit  &&If Escape pressed.
        lnTotInv1 = lnTotInv2
        IF WVISIBLE("GWDTHERMO")
          RELEASE WINDOW ("GWDTHERMO")
        ENDIF
        llExit    = .F.
        EXIT
      ENDIF
    ELSE
      *B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
      DO CASE
      CASE CVENPRIOR = '0'
        lcvendorr=ALLTRIM(APVENDOR.CVENDCODE)+' Invoice No. '+ALLTRIM(CINVNO)
        ** MESSAGE : " Vendor XXXXXX has payment priority 0."
        **           " This vendor in on hold.              "
        **           "                � OK �                "
        =gfModalGen("TRM04060B00000","DIALOG",lcvendorr)

      CASE APVENDOR.CVENPRIOR = '0' .AND. lcVen0Prior <> CVENDCODE
        lcvendorr=ALLTRIM(APVENDOR.CVENDCODE)
        ** MESSAGE : " Vendor XXXXXX has payment priority 0."
        **           " This vendor in on hold.              "
        **           "                � OK �                "
        =gfModalGen("TRM04060B00000","DIALOG",lcvendorr)
      ENDCASE
      lcVen0Prior = CVENDCODE
      lnTotInv2 = lnTotInv2 + 1
    ENDIF

    lnThermNo = lnThermNo + 1
    oProgress.CurrentProgress(lnThermNo)
    oProgress.lblFirstLabel.CAPTION = APINVHDR.CVENDCODE

  ENDSCAN
  oProgress = NULL

  *- Check if the Thermometer window exist then force it to close
  *!*    IF WEXIST('gwdThermo')
  *!*      =gfThermo(lnTotInv1,lnTotInv1,"Approving invoices ...","Vendor: "+CVENDCODE+"  Invoice: "+CINVNO)
  *!*    ENDIF
  GO BOTTOM
  *Old : CLEAR READ

ELSE && in case of appove with entered values.


  ** if any approved values are empty. **
  IF(EMPTY(lcBankCode) .OR. EMPTY(lcCheckCode))
    *!*                         .OR. EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0'));
    *!*                         .OR. (ATC("?",lcGlAcct)>0));
    *!*                         .OR. EMPTY(STRTRAN(STRTRAN(lcHGlAct,'-'),'0'));
    *!*                         .OR. (ATC("?",lcHGlAct)>0))
    ** MESSAGE : " You have to enter the bank code,the     "
    **           " checking account,the GL account,and Cash payment."
    **           "                    � Ok �               "
    =gfModalGen("TRM04090B00000","DIALOG")

    IF EMPTY(lcBankCode)
      * Old : SHOW GET lcBankCode
      * Old : _CUROBJ = OBJNUM(lcBankCode)
      loAprAllFormSet.Ariaform1.BankChk.kbBanks.keyTextBox.SETFOCUS()

    ELSE
      IF EMPTY(lcCheckCode)
        *Old : SHOW GET lcCheckCode
        * Old : _CUROBJ = OBJNUM(lcCheckCode)
        loAprAllFormSet.Ariaform1.BankChk.kbBanks.keyTextBox.SETFOCUS()
      ELSE
        * Old : SHOW GET lcGlAcct
        * Old : SHOW GET lcHGlAcct
        * Old : _CUROBJ = OBJNUM(lcGlAcct)
        loAprAllFormSet.Ariaform1.glChkActCode.keyTextBox.SETFOCUS()
      ENDIF
    ENDIF
    *- Add checks for currency fields

    RETURN .F.
  ENDIF
  *!B610215,1 HIA 01/23/2013 Aria4xp - AP - Approve for payment bank account issue [T20130107.0001][Start]

  lcBankCode  = loAprAllFormSet.Ariaform1.BankChk.kbBanks.keyTextBox.value 
  lcCheckCode = loAprAllFormSet.Ariaform1.BankChk.kbChkAccount.keyTextBox.value 
  *!B610215,1 HIA 01/23/2013 Aria4xp - AP - Approve for payment bank account issue [T20130107.0001][End]
  
  *- Add checks for currency fields
  IF loFormSet.llMultiCr .AND. EMPTY(lcAprCshCod)
    *** Message : " You have to enter the �."
    ***                      <   OK   >
    =gfModalGen("TRM04066B00000","DIALOG",'cash payment approved currency')
    * Old : _CUROBJ = OBJNUM(lcAprCshCod)
    loAprAllFormSet.Ariaform1.kbCashPayCurr.Keytextbox.SETFOCUS()
    RETURN .F.
  ELSE && if all approved values is full.
    *CLEAR READ
  ENDIF

  lcCurAlias  = ALIAS()

  *E303011,1 TMI 12/24/2011 [Start]
  *SELECT APINVHDR
  SELECT &lcAPINVHDR
  *E303011,1 TMI 12/24/2011 [End  ]

  oProgress = NEWOBJECT('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
  oProgress.TotalProgress = RECCOUNT(loFormset.lcAPINVHDR)
  oProgress.lblFirstLabel.CAPTION = 'Approve All'
  lnThermNo = 0
  oProgress.SHOW()

  SCAN

    *E303011,1 TMI 12/24/2011 [Start]
    =SEEK(&lcAPINVHDR..CVENDCODE+&lcAPINVHDR..CINVNO,'APINVHDR','VENDINV')
    =SEEK(&lcAPINVHDR..CVENDCODE,'APVENDOR','VENCODE')
    *E303011,1 TMI 12/24/2011 [End  ]

    *B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
    IF !((CVENPRIOR = '0') .OR. (APVENDOR.CVENPRIOR = '0'))

      lnTotInv2 = lnTotInv2 + 1
      *=gfThermo(lnTotInv1,lnTotInv2,"Approving invoices...","Vendor: "+CVENDCODE+"  Invoice: "+CINVNO)

      *IF gfObj_lock(.T.) && lock the pointed record.
      *E303011,1 TMI 12/22/2011 [Start]
      *IF lfObj_lock(.T.) && lock the pointed record.
      SELECT APINVHDR
      IF &lcAPINVHDR..COWNER='ARIA' OR gfObj_lock(.T.) && lock the pointed record.
        SELECT &lcAPINVHDR
        REPLACE COWNER WITH 'ARIA'  && this is used as a tag, this means that this record has been changed by the approve to pay program
        *E303011,1 TMI 12/22/2011 [End  ]

        ***If the vendor or invoice is on hold. ***
        *B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
        * IF (CVENPRIOR <> '0') .AND. (APVENDOR.CVENPRIOR <> '0')
        IF &lcAPINVHDR..CVENPMETH <> 'H'
          *- Get exchange rate between the checking account
          *- currency and the invoice currency.
          *- If the currency is different, implies that the system
          *- is multi currency, call the global function gfChkRate
          *- that validates an exchange rate.
          IF &lcAPINVHDR..cCurrCode <> lcAPrCurCod
            IF &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
              lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCurCod ,&lcAPINVHDR..dInvDate, ;
                .T., .F., &lcAPINVHDR..cCurrCode, .T.)
            ELSE      && Else
              lnAprExRat = gfChkRate('lnAprCurUnt', &lcAPINVHDR..cCurrCode ,&lcAPINVHDR..dInvDate, ;
                .T., .F., lcAprCurCod, .T.)
            ENDIF     && End of IF
            *- Change the calling of gfChkRate [END]

            *B600849,1 (End)
            *FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
            *FUNCTION  to exchange currency from invoice currency to company base currency.
            *FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
            lcExSin2 = ' '
            lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
            *lcExSin2 = IIF(lcExSin1 = '*','/','*')
            *FUNCTION  HISH 12/05/95. to exchange currency from invoice currency to approved currency.
            lcExSin4 = ' '

            *- Change the calling of gfGetExSin so if the [BEGIN]
            * Invoice currency is the base currency I make it the To currency
            * and if not the Approve currency is the To currency
            *lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
            *- IF Statment to check if the Invoice currency is the
            * same as the base currency
            IF &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
              lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,&lcAPINVHDR..cCurrCode)
              lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
              lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
            ELSE   && Else
              lcExSin3 = gfGetExSin(@lcExSin4,&lcAPINVHDR..cCurrCode,lcAprCurCod)
            ENDIF  && End of IF
            *- Change the calling of gfGetExSin [END]

            *lcExSin4 = IIF(lcExSin3 = '*','/','*')

          ELSE
            STORE 1 TO lnAprExRat, lnAprCurUnt
            *FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
            *FUNCTION  to exchange currency from invoice currency to company base currency.
            *FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
            lcExSin2 = ' '
            lcExSin1 = gfGetExSin(@lcExSin2,&lcAPINVHDR..cCurrCode)
            *lcExSin2 = IIF(lcExSin1 = '*','/','*')
            *FUNCTION  HISH 12/05/95. to exchange currency from invoice currency to approved currency.
            lcExSin4 = ' '

            *- Change the calling of gfGetExSin so if the [BEGIN]
            * Invoice currency is the base currency I make it the To currency
            * and if not the Approve currency is the To currency
            *lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
            *- IF Statment to check if the Invoice currency is the
            * same as the base currency
            IF &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
              lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,&lcAPINVHDR..cCurrCode)
              lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
              lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
            ELSE   && Else
              lcExSin3 = gfGetExSin(@lcExSin4,&lcAPINVHDR..cCurrCode,lcAprCurCod)
            ENDIF  && End of IF
            *- Change the calling of gfGetExSin [END]

            *lcExSin4 = IIF(lcExSin3 = '*','/','*')

          ENDIF      && ENDIF APINVHDR.cCurrCode <> lcAPrCurCod

          *- If the exchange rate = 0, do not approve, and present
          *- the following message
          IF lnAprExRat = 0
            *- IF editing the exchange rate on the fly is not
            *- allowed, present the following message:
            *- Message : " A valid � to � exchange rate could not "
            *-           " be found for �.                        "
            *-           "                  � Ok �                "

            *B601667,1 Change this line to fix the parameters [Begin]
            *=gfModalGen("INM04157B00000", "DIALOG",;
            *             ALLTRIM(APINVHDR.cCurrCode)+'|' ;
            *             +ALLTRIM(lcAprCurCod)+;
            *             '|'+DTOC(APINVHDR.dInvDate))

            *B601667,1 IF Statment to check if the Invoice currency is the
            *          Base Currency
            IF &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
              =gfModalGen("INM04157B00000", "DIALOG",;
                ALLTRIM(lcAprCurCod)+'|' ;
                +ALLTRIM(&lcAPINVHDR..cCurrCode)+;
                '|'+DTOC(&lcAPINVHDR..dInvDate))
            ELSE    && Else
              =gfModalGen("INM04157B00000", "DIALOG",;
                ALLTRIM(&lcAPINVHDR..cCurrCode)+'|' ;
                +ALLTRIM(lcAprCurCod)+;
                '|'+DTOC(&lcAPINVHDR..dInvDate))
            ENDIF    && End of IF
            *B601667,1 Change this line to fix the parameters [End]

          ELSE
            *- Include currency fields update.
            *B600808,1 Clear Approved 1099 amount field as well.
            *REPLACE CBNKCODE  WITH lcBankCode ,;
            CCHKACCT  WITH lcCheckCode,;
            CCHKGLACC WITH IIF(EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')),;
            SPACE(lnApsAcLen),lcGlAcct),;
            NINVAMTAP WITH (NINVAMNT-NINVPAID-NINVDISTK-NINVADJ-NINVDISOF),;
            NINVDISAP WITH NINVDISOF,;
            NINVADJAP WITH 0

            *B601526,1 Change this line [Begin]
            *REPLACE CBNKCODE   WITH lcBankCode ,;
            *        CCHKACCT   WITH lcCheckCode,;
            *        CCHKGLACC  WITH IIF(EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')),;
            *                                  SPACE(lnApsAcLen),lcGlAcct),;
            *        NINVAMTAP  WITH (NINVAMNT-NINVPAID-NINVDISTK-NINVADJ-NINVDISOF),;
            *        NINVDISAP  WITH NINVDISOF,;
            *        NINVADJAP  WITH 0,;
            *        nInvA1099  WITH 0,;
            *        cAprCurCod WITH lcAprCurCod,;
            *        nAprCurUnt WITH lnAprCurUnt,;
            *        nAprExRat  WITH lnAprExRat

            REPLACE CBNKCODE   WITH lcBankCode ,;
              CCHKACCT   WITH lcCheckCode,;
              CCHKGLACC  WITH IIF(EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')),;
              SPACE(lnApsAcLen),lcGlAcct),;
              NINVAMTAP  WITH (NINVAMNT-NINVPAID-NINVDISTK-NINVADJ-NINVDISOF),;
              NINVDISAP  WITH NINVDISOF,;
              NINVADJAP  WITH 0,;
              nInvA1099  WITH 0,;
              cAprCurCod WITH lcAprCurCod,;
              nAprCurUnt WITH lnAprCurUnt,;
              nAprExRat  WITH lnAprExRat,;
              nInvFAAp   WITH ROUND((&lcAPINVHDR..NINVAMNT-&lcAPINVHDR..NINVPAID-&lcAPINVHDR..NINVDISTK-&lcAPINVHDR..NINVADJ-&lcAPINVHDR..NINVDISOF) &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2)

            *B601526,1 Change this line [End]

            *=gfAdd_Info()  && Add the audit information to the record.
          ENDIF
        ELSE    && Else if payment method is cash payment.
          *- Get exchange rate between the checking account
          *- currency and the invoice currency.
          *- If the currency is different, implies that the system
          *- is multi currency, call the global function gfChkRate
          *- that validates an exchange rate.
          IF &lcAPINVHDR..cCurrCode <> lcAprCshCod
            *B600849,1 HISH 11/29/95. Change parameter to get rate and unit to change from invoice (Begin)
            *B600849,1                currency to approved currency.
            *lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCshCod,APINVHDR.dInvDate, ;
            .T., .F., APINVHDR.cCurrCode, .T.)

            *- Change the calling of gfChkRate so if the [BEGIN]
            * Invoice currency is the base currency I make it the To currency
            * and if not the Approve currency is the To currency
            *lnAprExRat = gfChkRate('lnAprCurUnt', APINVHDR.cCurrCode ,APINVHDR.dInvDate, ;
            *                        .T., .F., lcAprCshCod, .T.)
            *- IF Statment to check if the Invoice currency is the
            * same as the base currency
            IF &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
              lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCshCod ,&lcAPINVHDR..dInvDate, ;
                .T., .F., &lcAPINVHDR..cCurrCode, .T.)
            ELSE      && Else
              lnAprExRat = gfChkRate('lnAprCurUnt', &lcAPINVHDR..cCurrCode ,&lcAPINVHDR..dInvDate, ;
                .T., .F., lcAprCshCod, .T.)
            ENDIF     && End of IF
            *- Change the calling of gfChkRate [END]

            *B600849,1 (End)
            *FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
            *FUNCTION  to exchange currency from invoice currency to company base currency.
            *FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
            lcExSin2 = ' '
            lcExSin1 = gfGetExSin(@lcExSin2,&lcAPINVHDR..cCurrCode)
            *lcExSin2 = IIF(lcExSin1 = '*','/','*')
            *FUNCTION  HISH 12/05/95. to exchange currency from invoice currency to approved currency.
            lcExSin4 = ' '

            *- Change the calling of gfGetExSin so if the [BEGIN]
            * Invoice currency is the base currency I make it the To currency
            * and if not the Approve currency is the To currency
            *lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCshCod)
            *- IF Statment to check if the Invoice currency is the
            * same as the base currency
            IF &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
              lcExSin3 = gfGetExSin(@lcExSin4,lcAprCshCod,&lcAPINVHDR..cCurrCode)
              lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
              lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
            ELSE   && Else
              lcExSin3 = gfGetExSin(@lcExSin4,&lcAPINVHDR..cCurrCode,lcAprCshCod)
            ENDIF  && End of IF
            *- Change the calling of gfGetExSin [END]

            *lcExSin4 = IIF(lcExSin3 = '*','/','*')

          ELSE
            STORE 1 TO lnAprExRat, lnAprCurUnt
            *B601013- HISH  04/18/96. Got the equation signs. (Begin)
            *B601013- exchange currency from invoice currency to company base currency.
            lcExSin2 = ' '
            lcExSin1 = gfGetExSin(@lcExSin2,&lcAPINVHDR..cCurrCode)
            *B601013- exchange currency from invoice currency to approved currency.
            lcExSin4 = ' '

            *- Change the calling of gfGetExSin so if the [BEGIN]
            * Invoice currency is the base currency I make it the To currency
            * and if not the Approve currency is the To currency
            *lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCshCod)
            *- IF Statment to check if the Invoice currency is the
            * same as the base currency
            IF &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
              lcExSin3 = gfGetExSin(@lcExSin4,lcAprCshCod,&lcAPINVHDR..cCurrCode)
              lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
              lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
            ELSE   && Else
              lcExSin3 = gfGetExSin(@lcExSin4,&lcAPINVHDR..cCurrCode,lcAprCshCod)
            ENDIF  && End of IF
            *- Change the calling of gfGetExSin [END]

            *B601013- (End)
          ENDIF      && ENDIF APINVHDR.cCurrCode <> lcAprCshCod

          *- If the exchange rate = 0, do not approve, and present
          *- the following message
          IF lnAprExRat = 0
            *- IF editing the exchange rate on the fly is not
            *- allowed, present the following message:
            *- Message : " A valid � to � exchange rate could not "
            *-           " be found for �.                        "
            *-           "                  � Ok �                "

            *B601667,1 Change this line to fix the parameters [Begin]
            *=gfModalGen("INM04157B00000", "DIALOG",;
            *             ALLTRIM(APINVHDR.cCurrCode)+'|' ;
            *             +ALLTRIM(lcAprCshCod)+;
            *             '|'+DTOC(APINVHDR.dInvDate))

            *B601667,1 IF Statment to check if the Invoice currency is the
            *          Base Currency
            IF &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
              =gfModalGen("INM04157B00000", "DIALOG",;
                ALLTRIM(lcAprCshCod)+'|' ;
                +ALLTRIM(&lcAPINVHDR..cCurrCode)+;
                '|'+DTOC(&lcAPINVHDR..dInvDate))
            ELSE     && Else
              =gfModalGen("INM04157B00000", "DIALOG",;
                ALLTRIM(&lcAPINVHDR..cCurrCode)+'|' ;
                +ALLTRIM(lcAprCshCod)+;
                '|'+DTOC(&lcAPINVHDR..dInvDate))
            ENDIF    && End of IF
            *B601667,1 Change this line to fix the parameters [End]

          ELSE
            *- Include currency fields update.
            *B600808,1 Clear Approved 1099 amount field as well.
            *REPLACE CCHKGLACC WITH IIF(EMPTY(STRTRAN(STRTRAN(lcHGlAct,'-'),'0')),;
            SPACE(lnApsAcLen),lcHGlAct),;
            NINVAMTAP WITH (NINVAMNT-NINVPAID-NINVDISTK-NINVADJ-NINVDISOF),;
            NINVDISAP WITH NINVDISOF,;
            NINVADJAP WITH 0

            *B601526,1 Change this line [Begin]
            *REPLACE CCHKGLACC WITH IIF(EMPTY(STRTRAN(STRTRAN(lcHGlAct,'-'),'0')),;
            *                                 SPACE(lnApsAcLen),lcHGlAct),;
            *        NINVAMTAP WITH (NINVAMNT-NINVPAID-NINVDISTK-NINVADJ-NINVDISOF),;
            *        NINVDISAP WITH NINVDISOF,;
            *        NINVADJAP WITH 0,;
            *        nInvA1099  WITH 0,;
            *        cAprCurCod WITH lcAprCshCod,;
            *        nAprCurUnt WITH lnAprCurUnt,;
            *        nAprExRat  WITH lnAprExRat

            REPLACE CCHKGLACC WITH IIF(EMPTY(STRTRAN(STRTRAN(lcHGlAct,'-'),'0')),;
              SPACE(lnApsAcLen),lcHGlAct),;
              NINVAMTAP WITH (NINVAMNT-NINVPAID-NINVDISTK-NINVADJ-NINVDISOF),;
              NINVDISAP WITH NINVDISOF,;
              NINVADJAP WITH 0,;
              nInvA1099  WITH 0,;
              cAprCurCod WITH lcAprCshCod,;
              nAprCurUnt WITH lnAprCurUnt,;
              nAprExRat  WITH lnAprExRat,;
              nInvFAAp   WITH ROUND((&lcAPINVHDR..NINVAMNT-&lcAPINVHDR..NINVPAID-&lcAPINVHDR..NINVDISTK-&lcAPINVHDR..NINVADJ-&lcAPINVHDR..NINVDISOF) &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2)

            *B601526,1 Change this line [End]

            *=gfAdd_Info()  && Add the audit information to the record.
          ENDIF
        ENDIF
        * Get the totals in base currency.
        lnRateVal = &lcAPINVHDR..nExRate &lcExSin2 &lcAPINVHDR..nCurrUnit
        WITH loFormSet.Ariaform1
          .lnSesAmnt.VALUE = .lnSesAmnt.VALUE + ROUND(&lcAPINVHDR..nInvAmtAp &lcExSin1 lnRateVal, 2)
          .lnSesDisc.VALUE = .lnSesDisc.VALUE + ROUND(&lcAPINVHDR..nInvDisAp &lcExSin1 lnRateVal, 2)
        ENDWITH
        *E303011,1 TMI 12/22/2011 [Start]
        *        =gfObj_lock(.F.) && lock the pointed record.
        *E303011,1 TMI 12/22/2011 [End  ]
      ENDIF
      IF llExit  &&If Escape pressed.
        lnTotInv1 = lnTotInv2
        IF WVISIBLE("GWDTHERMO")
          RELEASE WINDOW ("GWDTHERMO")
        ENDIF
        llExit    = .F.
        EXIT
      ENDIF
    ELSE
      *B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
      DO CASE
      CASE CVENPRIOR = '0'
        lcvendorr=ALLTRIM(APVENDOR.CVENDCODE)+' Invoice No. '+ALLTRIM(CINVNO)
        ** MESSAGE : " Vendor XXXXXX has payment priority 0."
        **           " This vendor in on hold.              "
        **           "                � OK �                "
        =gfModalGen("TRM04060B00000","DIALOG",lcvendorr)

      CASE APVENDOR.CVENPRIOR = '0' .AND. lcVen0Prior <> CVENDCODE
        lcvendorr=ALLTRIM(APVENDOR.CVENDCODE)
        ** MESSAGE : " Vendor XXXXXX has payment priority 0."
        **           " This vendor in on hold.              "
        **           "                � OK �                "
        =gfModalGen("TRM04060B00000","DIALOG",lcvendorr)
      ENDCASE
      lcVen0Prior = CVENDCODE
      lnTotInv2 = lnTotInv2 + 1

      lnThermNo = lnThermNo + 1
      oProgress.CurrentProgress(lnThermNo)
      oProgress.lblFirstLabel.CAPTION = APINVHDR.CVENDCODE
    ENDIF
  ENDSCAN
  oProgress = NULL
  *- Check if the Thermometer window exist then force it to close
  *!*    IF WEXIST('gwdThermo')
  *!*      =gfThermo(lnTotInv1,lnTotInv1,"Approving invoices ...","Vendor: "+CVENDCODE+"  Invoice: "+CINVNO)
  *!*    ENDIF
  SELECT (lcCurAlias)
  GO BOTTOM
ENDIF

SET ESCAPE &lcEscap

***********************************************************************************
*Name      : lfGetDefu
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : get dufault bank, check, and gl account.
***********************************************************************************
FUNCTION lfGetDefu
PARAMETERS loFormSet
*B600808,1 Remove the following check. Get the default anyway.
*IF EMPTY(lcBankCode)
*B600808,1 Get the defaul bank code, checking account and G/L checking
*B600808,1 account if the payment method is not cash payment,
*B600808,1 otherwise get the default cash payment account.
IF APINVHDR.cVenPMeth <> 'H'
  DO CASE
  CASE !EMPTY(APINVHDR.CBNKCODE) && if found values in invoice header file.
    lcBankCode = APINVHDR.CBNKCODE
    lcCheckCode= APINVHDR.CCHKACCT
    lcGlAcct   = IIF(!EMPTY(APINVHDR.CCHKGLACC),APINVHDR.CCHKGLACC, loFormset.ap1.lcEmptyAcc)
    *B600808,1 Get cash payment account below. Removed.
    *lcHGlAct   = lcGlAcct
  CASE !EMPTY(APVENDOR.CBNKCODE) && if found values in vendor file.
    lcBankCode = APVENDOR.CBNKCODE
    lcCheckCode= APVENDOR.CCHKACCT
    *B600808,1 Clear G/L account
    lcGlAcct   = loFormset.ap1.lcEmptyAcc
  CASE !EMPTY(APDIV.CBNKCODE)    && if found values in division file.
    lcBankCode = APDIV.CBNKCODE
    lcCheckCode= APDIV.CCHKACCT
    *B600808,1 Clear G/L account
    lcGlAcct   = loFormset.ap1.lcEmptyAcc
  OTHERWISE                      && get value from setup file.
    lcBankCode = APSETUP.CBNKCODE
    lcCheckCode= APSETUP.CCHKACCT
    *B600808,1 Clear G/L account
    lcGlAcct   = loFormset.ap1.lcEmptyAcc
  ENDCASE
  *** get account if there is a bank code and check code available. ***
  *B600808,1 Seek the checking account whether or not the G/L account
  *B600808,1 is empty.
  *IF  EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')) ;
  .AND. !EMPTY(lcBankCode) .AND. !EMPTY(lcCheckCode)
  IF !EMPTY(lcBankCode) .AND. !EMPTY(lcCheckCode)
    IF SEEK(lcBankCode+lcCheckCode,'APCHECKS')
      IF EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0'))
        lcGlAcct    = IIF(!EMPTY(APCHECKS.CCHKGLACC),APCHECKS.CCHKGLACC, loFormset.ap1.lcEmptyAcc)
        *B600808,1 Get cash payment account below. Removed.
        *lcHGlAct   = lcGlAcct
      ENDIF    && ENDIF EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0'))

      *- Get default currency of the checking account.
      lcAprCurCod   = APCHECKS.cCurrCode
      *- Get a corresponding exchange rate
      *- between the approval currency and the invoice currency.
      *- If the currency is different, implies that the system
      *- is multi currency, call the global function gfChkRate
      *- that validates an exchange rate.
      IF APINVHDR.cCurrCode <> lcAPrCurCod
        *B600849,1 HISH 11/29/95. Change parameter to get rate and unit to change from invoice (Begin)
        *B600849,1                currency to approved currency.
        *lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCurCod,APINVHDR.dInvDate, ;
        !llAprPart, .F., APINVHDR.cCurrCode, .T.)

        *- Change the calling of gfChkRate so if the [BEGIN]
        * Invoice currency is the base currency I make it the To currency
        * and if not the Approve currency is the To currency
        *lnAprExRat = gfChkRate('lnAprCurUnt', APINVHDR.cCurrCode ,APINVHDR.dInvDate, ;
        *                        !llAprPart, .F., lcAprCurCod, .T.)
        *- IF Statment to check if the Invoice currency is the
        * same as the base currency
        IF APINVHDR.cCurrCode = oAriaApplication.BaseCurrency
          lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCurCod ,APINVHDR.dInvDate, ;
            !llAprPart, .F., APINVHDR.cCurrCode, .T.)
        ELSE      && Else
          lnAprExRat = gfChkRate('lnAprCurUnt', APINVHDR.cCurrCode ,APINVHDR.dInvDate, ;
            !llAprPart, .F., lcAprCurCod, .T.)
        ENDIF     && End of IF
        *- Change the calling of gfChkRate [END]

        *B600849,1 (End)
        *FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
        *FUNCTION  to exchange currency from invoice currency to company base currency.
        *FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
        lcExSin2 = ' '
        lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
        *lcExSin2 = IIF(lcExSin1 = '*','/','*')
        *FUNCTION  HISH 12/05/95. to exchange currency from invoice currency to approved currency.
        lcExSin4 = ' '

        *- Change the calling of gfGetExSin so if the [BEGIN]
        * Invoice currency is the base currency I make it the To currency
        * and if not the Approve currency is the To currency
        *lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
        *- IF Statment to check if the Invoice currency is the
        * same as the base currency
        IF APINVHDR.cCurrCode = oAriaApplication.BaseCurrency
          lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,APINVHDR.cCurrCode)
          lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
          lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
        ELSE   && Else
          lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
        ENDIF  && End of IF
        *- Change the calling of gfGetExSin [END]

        *lcExSin4 = IIF(lcExSin3 = '*','/','*')

      ELSE
        STORE 1 TO lnAprExRat, lnAprCurUnt
        *FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
        *FUNCTION  to exchange currency from invoice currency to company base currency.
        *FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
        lcExSin2 = ' '
        lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
        *lcExSin2 = IIF(lcExSin1 = '*','/','*')
        *FUNCTION  to exchange currency from invoice currency to approved currency.
        lcExSin4 = ' '

        *- Change the calling of gfGetExSin so if the [BEGIN]
        * Invoice currency is the base currency I make it the To currency
        * and if not the Approve currency is the To currency
        *lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
        *- IF Statment to check if the Invoice currency is the
        * same as the base currency
        IF APINVHDR.cCurrCode = oAriaApplication.BaseCurrency
          lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,APINVHDR.cCurrCode)
          lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
          lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
        ELSE   && Else
          lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
        ENDIF  && End of IF
        *- Change the calling of gfGetExSin [END]

        *lcExSin4 = IIF(lcExSin3 = '*','/','*')

      ENDIF      && ENDIF APINVHDR.cCurrCode <> lcAPrCurCod
    ENDIF      && ENDIF SEEK(lcBankCode+lcCheckCode,'APCHECKS')
  ENDIF      && ENDIF !EMPTY(lcBankCode) .AND. !EMPTY(lcCheckCode)

  *B600808,1 Else, if payment method is cash payment
ELSE
  *B600808,1 Get the default cash payment account
  lcGlAcct = IIF(!EMPTY(APINVHDR.cChkGLAcc), APINVHDR.cChkGLAcc,;
    IIF(!EMPTY(APVENDOR.cCashAcct), APVENDOR.cCashAcct, ;
    IIF(!EMPTY(APDIV.cCashAcct), APDIV.cCashAcct, ;
    IIF(!EMPTY(APSETUP.cCashAcct), APSETUP.cCashAcct, ;
    loFormset.ap1.lcEmptyAcc))))
  *- If not multi currency,
  *- or it the payment method is cash payment,
  *- Default currency with that of the invoice.
  lcAprCurCod = APINVHDR.cCurrCode
  STORE 1 TO lnAprExRat, lnAprCurUnt
  *FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
  *FUNCTION  to exchange currency from invoice currency to company base currency.
  *FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
  lcExSin2 = ' '
  lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
  *lcExSin2 = IIF(lcExSin1 = '*','/','*')
  *FUNCTION   to exchange currency from invoice currency to approved currency.
  lcExSin4 = ' '

  *- Change the calling of gfGetExSin so if the [BEGIN]
  * Invoice currency is the base currency I make it the To currency
  * and if not the Approve currency is the To currency
  *lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
  *- IF Statment to check if the Invoice currency is the
  * same as the base currency
  IF APINVHDR.cCurrCode = oAriaApplication.BaseCurrency
    lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,APINVHDR.cCurrCode)
    lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
    lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
  ELSE   && Else
    lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
  ENDIF  && End of IF
  *- Change the calling of gfGetExSin [END]

ENDIF

***********************************************************************************
*Name      : lfvApprPart
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : this function calling from push button approve fully and partialy
***********************************************************************************
FUNCTION lfvApprPart
PARAMETERS loFormset,llAprPart

*E303011,1 TMI 12/22/2011 [Start] do locking when clicking the approve button itself
*IF !lfObj_lock(.T.)
*  RETURN
*ENDIF
*E303011,1 TMI 12/22/2011 [End  ]

*E303011,1 TMI 12/21/2011 [Start] define variables
LOCAL lnI,lcVar
FOR lnI = 1 TO ALEN(loFormset.laPropArr,1)
  lcVar = loFormset.laPropArr[lnI,1]
  &lcVar = loFormset.laPropArr[lnI,2]
ENDFOR
*- locate the same line in the APINVHDR file

lcAPINVHDR = loFormset.lcAPINVHDR
=SEEK(&lcAPINVHDR..CVENDCODE+&lcAPINVHDR..CINVNO,'APINVHDR','VENDINV')
=SEEK(&lcAPINVHDR..CVENDCODE,'APVENDOR','VENCODE')
*E303011,1 TMI 12/22/2011 [End  ]

*** get the default values from approved values. ***
=lfGetDefu(loFormSet)
*** Get approve to pay and approve discount defalt. ***
*B600846,1 HISH 11/29/95. Moved this part below with the same bug number (Begin)
*lnAprToPay=APINVHDR.NINVAMNT-NINVPAID-NINVADJ-;
IIF(APINVHDR.NINVDISTK<APINVHDR.NINVDISOF,;
APINVHDR.NINVDISOF-APINVHDR.NINVDISTK,0.00)
*lnAprDisc=IIF(APINVHDR.NINVDISTK<APINVHDR.NINVDISOF,;
APINVHDR.NINVDISOF-APINVHDR.NINVDISTK,0.00)
*B600846,1. (End)

lcOldBank   = lcBankCode
lcOldCheck  = lcCheckCode
lcOldGlAcc  = lcGLAcct
lcHOldGlAc  = lcHGlAct

*- Store old currency fields values
lcOldCurr   = lcAprCurCod
lnOldUnt    = lnAprCurUnt
lnOldExRat  = lnAprExRat

*E303011,1 TMI 12/22/2011 [Start]
*!*  PUSH KEY
*!*  ON KEY
*E303011,1 TMI 12/22/2011 [End  ]

*** In case of approve partially get old values of payment, discount, and adjustment.
*** calling approved partially window.
IF APVENDOR.CVENPRIOR = '0' .OR. APINVHDR.CVENPRIOR = '0'
  lcvendorr=ALLTRIM(APVENDOR.CVENDCODE)
  ** MESSAGE : " Vendor XXXXXX has payment priority 0."
  **           " This vendor in on hold.              "
  **           "                � OK �                "
  =gfModalGen("TRM04060B00000","DIALOG",lcvendorr)
  RETURN
ELSE
  IF APINVHDR.CINVSTAT = 'A'
    lcCanApr= "approve"
    ** MESSAGE : " You Can not CCCC a debit memo created by an advanced"
    **           " payment.                                            "
    **           "                    � Ok �                           "
    =gfModalGen("TRM04058B00000","DIALOG",lcCanApr)
    RETURN
  ELSE
    *B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
    *    IF (APINVHDR.NINVAMNT < 0) .AND. (APINVHDR.CVENPRIOR = '0')
    IF (APINVHDR.NINVAMNT < 0) .AND. (APINVHDR.CVENPRIOR = '0' .OR. APVENDOR.CVENPRIOR = '0')
      lcDebit="Debit memo "+APINVHDR.CINVNO
      ** MESSAGE : " YYYY XXXXX has payment priority 0 "
      **           " YYYY is on hold.                  "
      **           "                    � Ok �         "
      =gfModalGen("TRM04059B00000","DIALOG",lcDebit+"|"+lcTDebitM)
      RETURN
    ELSE
      *B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
      *      IF (APINVHDR.NINVAMNT > 0) .AND. (APINVHDR.CVENPRIOR = '0')
      IF (APINVHDR.NINVAMNT > 0) .AND. (APINVHDR.CVENPRIOR = '0' .OR. APVENDOR.CVENPRIOR = '0')
        lcDebit="Invoice "+ALLTRIM(APINVHDR.CINVNO)
        ** MESSAGE : " YYYY XXXXX has payment priority 0 "
        **           " YYYY is on hold.                  "
        **           "                    � Ok �         "
        =gfModalGen("TRM04059B00000","DIALOG",lcDebit+"|"+lcTInvoice)
        RETURN
      ELSE
        IF llAprPart  && Case approve part.
          IF EMPTY(APVENDOR.CVEN1099t)  && If the vendor 1099 amount is empty.
            lc1099St = 'DISABLE'
          ELSE
            lc1099St = 'ENABLE'
          ENDIF

          lnOldAprTPy = lnAprToPay
          lnOldAprDisc= lnAprDisc
          lnOldAprAdj = lnAprAdj
          lnO1099amnt = ln1099amnt

          *B600846,1 HISH 11/29/95. Put variable defualt values in a condition, (Begin)
          *B600846,1                that if this invoice aproved priviously put
          *B600846,1                the last approve amount other with put the
          *B600846,1                open amount - paid amount - adjusted amount -discount.
          IF &lcAPINVHDR..NINVAMTAP = 0
            lnAprToPay=&lcAPINVHDR..NINVAMNT-&lcAPINVHDR..NINVPAID-&lcAPINVHDR..NINVADJ-;
              IIF(&lcAPINVHDR..NINVDISTK<&lcAPINVHDR..NINVDISOF,;
              &lcAPINVHDR..NINVDISOF-&lcAPINVHDR..NINVDISTK,0.00)
            lnAprDisc=IIF(&lcAPINVHDR..NINVDISTK<&lcAPINVHDR..NINVDISOF,;
              &lcAPINVHDR..NINVDISOF-&lcAPINVHDR..NINVDISTK,0.00)
            lnAprAdj   = 0
            ln1099amnt = 0
          ELSE
            *B600808,1 Initialize variables prior to the screen entry
            *B600808,1 instead of the when of the browse.
            lnAprToPay  = &lcAPINVHDR..NINVAMTAP
            lnAprDisc   = &lcAPINVHDR..NINVDISAP
            lnAprAdj    = &lcAPINVHDR..NINVADJAP
            ln1099amnt  = &lcAPINVHDR..NINVA1099
          ENDIF
          *B600846,1 (END)

          llCanclAp   = .F.

          *- Get the approved to pay amount in approval currency.
          *FUNCTION  HISH 12/05/95. Used the variables hold signs in the equation. (Begin)
          *lnExchAmnt  = IIF(lnAprCurUnt > 0,;
          ROUND(lnAprToPay * lnAprExRat / lnAprCurUnt, 2),;
          0)
          lnExchAmnt  = IIF(lnAprCurUnt > 0 AND lnAprExRat > 0,;
            ROUND(lnAprToPay &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2),;
            0)

          *- Disable rate field if editing is not allowed,
          *- or if the currency code is that of the invoice.
          *- or if the checking account is empty,
          lcRateDisp  = IIF(!loFormset.llEditEx .OR. ;
            &lcAPINVHDR..cCurrcode = &lcAPINVHDR..cAprCurCod .OR.;
            (&lcAPINVHDR..CVENPMETH <> 'H' .AND. ;
            EMPTY(lcCheckCode)),;
            'DISABLE', 'ENABLE')

          IF APINVHDR.CVENPMETH <> 'H'
            *,1 Call *.SPR from screens directory
            * DO APAPRPRT.SPR  && calling the approved all window.
            *Old : DO (gcScrDir + gcWinAppl + '\APAPRPRT.SPR')
            *,1 end

            *B608903,1 TMI [Start] Release the lock after the screen has been closed
            *SELECT APINVHDR
            *E303011,1 TMI 12/22/2011 [Start]
            *=lfObj_lock(.F.)
            *E303011,1 TMI 12/22/2011 [Start]
            *=gfObj_lock(.F.)
            *E303011,1 TMI 12/22/2011 [End  ]
            *E303011,1 TMI 12/22/2011 [End  ]
            *B608903,1 TMI [End  ] Release the lock after the screen has been closed

          ELSE


            *B601569,1 Add this line to set the default currency and
            *          exchange rate [Begin]
            =lfSetDef()
            *B601569,1 Add this line [End]

            *,1 Call *.SPR from screens directory
            * DO APAPPRT2.SPR
            *Old : DO (gcScrDir + gcWinAppl + '\APAPPRT2.SPR')
            *,1 end

          ENDIF

          DO FORM (oAriaApplication.ScreenHome+"\AP\APAPRPRT.SCX") WITH loFormSet

          ***Refresh the invoice push button. ***
          ***Refresh say field open apen amount. ***
          *Old : =lfRefresh()
          lnBrRecNo  = RECNO(lcAPINVHDR)
          *Old : SHOW WINDOW (lcBrTtl) REFRESH
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDIF

loFormSet.Ariaform1.grdAprove.AFTERROWCOLCHANGE()
loFormSet.Ariaform1.REFRESH()
*- End of lfvApprPart

***********************************************************************************
*Name      : lfvAprov
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Validation function of the <ok> push button of the Approved for payment.
***********************************************************************************
FUNCTION lfvAprov
PARAMETERS loAprFormSet,llAprPart

*- this function is called either from ARAPRPRT.SCX or APAPRPA.SCX
IF TYPE('loAprFormSet.CallingForm')='O'
  * called from ARAPRPRT.SCX
  loFormSet = loAprFormSet.CallingForm
ELSE
  * called from APAPRPA.SCX
  loFormSet = loAprFormSet
ENDIF

*E303011,1 TMI 12/18/2011 [Start]
*!*  As there is a temp file to hold the lines of APINVHDR to approve , any line that has been updated will be marked in the LLOC_STAT field
*!*  This field will be cleared when collecting data into the temp file at the first time
*!*  When saving, we loop over this field and update the APINVHDR accordingly
*!*  When IGNORE is clicked, data would be recollected from APINVHDR
*E303011,1 TMI 12/18/2011 [End  ]

*** in case of approve partially. ***
IF llAprPart            && approve part validation.
  ***if there is bankcode, check, and account but there isn't any payment, discount, or adjustment.
  *B600808,1 Change validation sequence
  *IF !EMPTY(lcBankCode+lcCheckCode+STRTRAN(STRTRAN(lcGlAcct,'-'),'0')) .AND.;
  lnAprToPay+lnAprDisc+lnAprAdj  = 0
  DO CASE
    *B600808,1 Copied from below.
    *B600808,1 Check if approved to pay amount is not zero.
    *** in case of not enter approved to pay amount. ***
    *CASE !EMPTY(lcBankCode+lcCheckCode+STRTRAN(STRTRAN(lcGlAcct,'-'),'0')) ;
    .AND. lnAprToPay = 0
  CASE lnAprToPay = 0
    ** MESSAGE : " You have to enter the approved to pay   "
    **           " amount.                                 "
    **           "                    � Ok �               "
    =gfModalGen("TRM04022B00000","DIALOG")
    *SHOW GET lnAprToPay
    *old :_CUROBJ = OBJNUM(lnAprToPay)
    loAprFormSet.Ariaform1.txtinvamtap.SETFOCUS()
    RETURN .F.

    *B600808,1 Remove. There is no check for non check payment,
    *B600808,1 probably meant cash payment. Substitute by the
    *B600808,1 following case. In case of cash payments.
    *** in case of non check payment. ***
    *CASE CVENPMETH = 'N' .AND. EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0'))
    ** MESSAGE : You have to enter the � account.
    **           "                    � Ok �               "
    *=gfModalGen("TRM04020B00000","DIALOG",'GL')
  CASE &lcAPINVHDR..cVenPMeth = 'H' .AND. EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0'))
    *B600808,1  Message : " You have to enter the � account.     "
    *B600808,1                       �  Ok  �
    =gfModalGen("TRM04020B00000","DIALOG",lcTPayAcct)
    *old : _CUROBJ = OBJNUM(lcGlAcct)
    loAprFormSet.Ariaform1.glChkActCode.keyTextBox.SETFOCUS()
    RETURN .F.

    *B600808,1 Remove. There is no special case for non check payment,
    *B600808,1 probably meant cash payment. Substitute by the
    *B600808,1 following case (in case of payment methods other than
    *B600808,1 cash payments).
    *** in case of there isn't bank, or check, or account. ***
    *CASE CVENPMETH <> 'N' .AND.;
    *     lnAprToPay+lnAprDisc+lnAprAdj > 0 .AND.;
    *     (EMPTY(lcBankCode) .OR. EMPTY(lcCheckCode);
    *                        .OR. EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0'));
    *                        .OR. (ATC("?",lcGlAcct)>0) )
  CASE &lcAPINVHDR..cVenPMeth <> 'H'  .AND.;
      (EMPTY(lcBankCode) .OR.;
      EMPTY(lcCheckCode) .OR.;
      EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')))
    ** MESSAGE : " You have to enter the bank code,the     "
    **           " checking account,and the GL account.    "
    **           "                    � Ok �               "
    =gfModalGen("TRM04021B00000","DIALOG")
    IF EMPTY(lcBankCode)
      *SHOW GET lcBankCode
      *_CUROBJ = OBJNUM(lcBankCode)
      loAprFormSet.Ariaform1.BankChk.kbBanks.keyTextBox.SETFOCUS()
    ELSE
      IF EMPTY(lcCheckCode)
        *SHOW GET lcCheckCode
        *_CUROBJ = OBJNUM(lcCheckCode)
        loAprFormSet.Ariaform1.BankChk.kbChkAccount.keyTextBox.SETFOCUS()
      ELSE
        *SHOW GET lcGlAcct
        *_CUROBJ = OBJNUM(lcGlAcct)
        loAprFormSet.Ariaform1.glChkActCode.keyTextBox.SETFOCUS()
      ENDIF
    ENDIF
    RETURN .F.

    *B600808,1 Copied above to match the payable invoices validation.
    *** in case of not enter approved to pay amount. ***
    *CASE !EMPTY(lcBankCode+lcCheckCode+STRTRAN(STRTRAN(lcGlAcct,'-'),'0')) ;
    *     .AND. lnAprToPay = 0
    *  ** MESSAGE : " You have to enter the approved to pay   "
    *  **           " amount.                                 "
    *  **           "                    � Ok �               "
    *  =gfModalGen("TRM04022B00000","DIALOG")
    *  SHOW GET lnAprToPay
    *  _CUROBJ = OBJNUM(lnAprToPay)
    *  RETURN

    *B600808,1 Check for 1099 amount. Change check condition.
    *CASE !EMPTY(lcBankCode+lcCheckCode+STRTRAN(STRTRAN(lcGlAcct,'-'),'0')) ;
    .AND. (!BETWEEN(ln1099amnt,lnAprToPay,0);
    .AND. !BETWEEN(ln1099amnt,0,lnAprToPay))
  CASE &lcAPINVHDR..cVenPMeth <> 'H'  .AND. !loFormset.ap1.llApS1099 .AND.;
      (!BETWEEN(ln1099amnt,lnAprToPay,0) .AND. ;
      !BETWEEN(ln1099amnt,0,lnAprToPay))
    ** MESSAGE : " The 1099 amount cannot be greater than  "
    **           " the �.             "
    **           "                 � Ok �                  "
    =gfModalGen("TRM04061B00000","DIALOG","approved amount to pay")
    ln1099amnt=lnO1099amnt
    SHOW GET ln1099amnt
    * Old : _CUROBJ = OBJNUM(ln1099amnt)
    loAprFormSet.Ariaform1.txtinva1099.SETFOCUS()
    RETURN .F.

    *- Add a case for empty exchange rate
  CASE (&lcAPINVHDR..CVENPMETH = 'H' .OR. !EMPTY(lcBankCode)) .AND. lnAprExRat = 0
    IF lcAprCurCod <> &lcAPINVHDR..cCurrCode
      *B600849,1 HISH 11/29/95. Change parameter to get rate and unit to change from invoice (Begin)
      *B600849,1                currency to approved currency.
      *lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCurCod,APINVHDR.dInvDate, ;
      .T., .F., APINVHDR.cCurrCode, .T.)

      *- Change the calling of gfChkRate so if the [BEGIN]
      * Invoice currency is the base currency I make it the To currency
      * and if not the Approve currency is the To currency
      *lnAprExRat = gfChkRate('lnAprCurUnt', APINVHDR.cCurrCode ,APINVHDR.dInvDate, ;
      *                        .T., .F., lcAprCurCod, .T.)
      *- IF Statment to check if the Invoice currency is the
      * same as the base currency
      IF &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
        lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCurCod ,&lcAPINVHDR..dInvDate, ;
          .T., .F., &lcAPINVHDR..cCurrCode, .T.)
      ELSE      && Else
        lnAprExRat = gfChkRate('lnAprCurUnt', &lcAPINVHDR..cCurrCode ,&lcAPINVHDR..dInvDate, ;
          .T., .F., lcAprCurCod, .T.)
      ENDIF     && End of IF
      *- Change the calling of gfChkRate [END]

      *FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
      *FUNCTION  to exchange currency from invoice currency to company base currency.
      *FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
      lcExSin2 = ' '
      lcExSin1 = gfGetExSin(@lcExSin2,&lcAPINVHDR..cCurrCode)
      *lcExSin2 = IIF(lcExSin1 = '*','/','*')
      *FUNCTION  to exchange currency from invoice currency to approved currency.
      lcExSin4 = ' '

      *- Change the calling of gfGetExSin so if the [BEGIN]
      * Invoice currency is the base currency I make it the To currency
      * and if not the Approve currency is the To currency
      *lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
      *- IF Statment to check if the Invoice currency is the
      * same as the base currency
      IF &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
        lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,&lcAPINVHDR..cCurrCode)
        lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
        lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
      ELSE   && Else
        lcExSin3 = gfGetExSin(@lcExSin4,&lcAPINVHDR..cCurrCode,lcAprCurCod)
      ENDIF  && End of IF
      *- Change the calling of gfGetExSin [END]

      *lcExSin4 = IIF(lcExSin3 = '*','/','*')

      *B600849,1 (End)
      *- If the exchange rate = 0, do not approve, and present
      *- the following message
      IF lnAprExRat = 0
        *- IF editing the exchange rate on the fly is not
        *- allowed, present the following message:
        *- Message : " A valid � to � exchange rate could not "
        *-           " be found for �.                        "
        *-           "                  � Ok �                "

        *B601667,1 Change this line to fix the parameters [Begin]
        *=gfModalGen("INM04157B00000", "DIALOG",;
        *             ALLTRIM(APINVHDR.cCurrCode)+'|' ;
        *             +ALLTRIM(lcAprCurCod)+;
        *             '|'+DTOC(APINVHDR.dInvDate))

        *B601667,1 IF Statment to check if the Invoice currency is the
        *          Base Currency
        IF &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
          =gfModalGen("INM04157B00000", "DIALOG",;
            ALLTRIM(lcAprCurCod)+'|' ;
            +ALLTRIM(&lcAPINVHDR..cCurrCode)+;
            '|'+DTOC(&lcAPINVHDR..dInvDate))
        ELSE     && Else
          =gfModalGen("INM04157B00000", "DIALOG",;
            ALLTRIM(&lcAPINVHDR..cCurrCode)+'|' ;
            +ALLTRIM(lcAprCurCod)+;
            '|'+DTOC(&lcAPINVHDR..dInvDate))
        ENDIF     && End of IF
        *B601667,1 Change this line to fix the parameters [End]

        RETURN .F.
      ENDIF
    ELSE
      STORE 1 TO lnAprExRat, lnAprCurUnt
    ENDIF

    *B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
    *old     CASE &lcAPVENDOR..CVENPRIOR = '0' .OR. CVENPRIOR = '0'
  CASE APVENDOR.CVENPRIOR = '0' .OR. &lcAPINVHDR..CVENPRIOR = '0'
    lcvendorr=ALLTRIM(APVENDOR.CVENDCODE)
    ** MESSAGE : " Vendor XXXXXX has payment priority 0."
    **           " This vendor in on hold.              "
    **           "                � OK �                "
    =gfModalGen("TRM04060B00000","DIALOG",lcvendorr)
    RETURN .F.
  OTHERWISE
    * Old:     CLEAR READ
  ENDCASE

  *B600808,1 Removed IF..ELSE..ENDIF condition
  *ELSE
  *  IF (EMPTY(lcBankCode) .OR. EMPTY(lcCheckCode);
  *                          .OR.  EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0'));
  *                          .OR. (ATC("?",lcGlAcct)>0) )
  *    ** MESSAGE : " You have to enter the bank code,the     "
  *    **           " checking account,and the GL account.    "
  *    **           "                    � Ok �               "
  *    =gfModalGen("TRM04021B00000","DIALOG")
  *    RETURN
  *  ENDIF
  *CLEAR READ
  *ENDIF

ELSE && in case of approved fully

  *E303011,1 TMI 12/21/2011 [Start] define variables
  LOCAL lnI,lcVar
  FOR lnI = 1 TO ALEN(loFormset.laPropArr,1)
    lcVar = loFormset.laPropArr[lnI,1]
    &lcVar = loFormset.laPropArr[lnI,2]
  ENDFOR
  *E303011,1 TMI 12/21/2011 [End  ]

  *E303011,1 TMI 12/22/2011 [Start] locate the same line in the APINVHDR file
  lcAPINVHDR = loFormSet.lcAPINVHDR
  =SEEK(&lcAPINVHDR..CVENDCODE+&lcAPINVHDR..CINVNO,'APINVHDR','VENDINV')
  =SEEK(&lcAPINVHDR..CVENDCODE,'APVENDOR','VENCODE')
  =lfGetDefu(loFormSet)
  *E303011,1 TMI 12/22/2011 [End  ]

  *B600808,1 If payment method is not cash payment,
  IF &lcAPINVHDR..cVenPMeth <> 'H'
    IF (EMPTY(lcBankCode) .OR. EMPTY(lcCheckCode);
        .OR.  EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0'));
        .OR. (ATC("?",lcGlAcct)>0) )
      ** MESSAGE : " You have to enter the bank code,the     "
      **           " checking account,and the GL account.    "
      **           "                    � Ok �               "
      =gfModalGen("TRM04021B00000","DIALOG")
      RETURN
    ENDIF

    *- If multi currency and the exchange rate is zero,
    *- do not approve
    IF loFormset.llMultiCr .AND. lnAprExRat = 0
      *- IF editing the exchange rate on the fly is not
      *- allowed, present the following message:
      *- Message : " A valid � to � exchange rate could not "
      *-           " be found for �.                        "
      *-           "                  � Ok �                "

      *B601667,1 IF Statment to check if the Invoice currency is the
      *          Base Currency
      IF &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
        =gfModalGen("INM04157B00000", "DIALOG",;
          ALLTRIM(lcAprCurCod)+'|' ;
          +ALLTRIM(&lcAPINVHDR..cCurrCode)+;
          '|'+DTOC(&lcAPINVHDR..dInvDate))
      ELSE    && Else
        =gfModalGen("INM04157B00000", "DIALOG",;
          ALLTRIM(&lcAPINVHDR..cCurrCode)+'|' ;
          +ALLTRIM(lcAprCurCod)+;
          '|'+DTOC(&lcAPINVHDR..dInvDate))
      ENDIF     && End of IF
      RETURN
    ENDIF    && ENDIF llMultiCr .AND. lnAprExRat = 0
  ENDIF    && ENDIF APINVHDR.cVenPMeth <> 'H'
  *B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
  * old  IF APVENDOR.CVENPRIOR = '0' .OR. CVENPRIOR = '0'
  IF APVENDOR.CVENPRIOR = '0'  .OR. &lcAPINVHDR..CVENPRIOR = '0'
    lcvendorr=ALLTRIM(APVENDOR.CVENDCODE)
    ** MESSAGE : " Vendor XXXXXX has payment priority 0."
    **           " This vendor in on hold.              "
    **           "                � OK �                "
    =gfModalGen("TRM04060B00000","DIALOG",lcvendorr)
    RETURN
  ENDIF
ENDIF

SELECT APINVHDR

*** lock the pointed record and replaced the approved values. ***
*B128400,1 WAM 10/12/2006 Commented out. Invoice already locked when click Partial approve
*IF lfObj_lock(.T.) && Added by TMI 08/24/2006
*E303011,1 TMI 12/22/2011 [Start] use the global function gfObj_lock
*IF IIF(llAprPart, .T., lfObj_lock(.T.) )
IF &lcAPINVHDR..COWNER='ARIA' OR gfObj_lock(.T.)
  *E303011,1 TMI 12/22/2011 [End  ]
  *E303011,1 TMI 12/22/2011 [Start]
  SELECT (loFormSet.lcAPINVHDR)
  *replace LLOK_STAT WITH .T.
  REPLACE COWNER WITH 'ARIA'  && this is used as a tag, this means that this record has been changed by the approve to pay program
  *E303011,1 TMI 12/22/2011 [End  ]
  *B128400,1 WAM 10/12/2006 (End)
  IF &lcAPINVHDR..CVENPMETH <> "H"

    *B601519,1 IF Statment to check if the Approved amount in the Approved
    *             currency is not 0 or this vlidation function is not for
    *             the Push Button pbApprov
    IF lnExchAmnt <> 0 .OR. SYS(18) <> 'PBAPPROV'
      REPLACE CBNKCODE  WITH lcBankCode ,;
        CCHKACCT  WITH lcCheckCode,;
        CCHKGLACC WITH IIF(EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')),;
        SPACE(lnApsAcLen), lcGlAcct)
    ENDIF       && End of IF
    *B601519,1 Change this line [End]

  ELSE
    IF !EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0'))

      *B601519,1 Change this line [Begin]
      *REPLACE CCHKGLACC WITH lcGlAcct

      *B601519,1 IF Statment to check if the Approved amount in the Approved
      *             currency is not 0 or this vlidation function is not for
      *             the Push Button pbApprov
      IF lnExchAmnt <> 0 .OR. SYS(18) <> 'PBAPPROV'
        REPLACE CCHKGLACC WITH lcGlAcct
      ENDIF
      *B601519,1 Change this line [End]

    ELSE
      ** MESSAGE : " You have to enter the bank code,the     "
      **           " checking account,and the GL account.    "
      **           "                    � Ok �               "
      =gfModalGen("TRM04021B00000","DIALOG")
    ENDIF
  ENDIF
  ***in case of approved partially replaced the rest of approved values. ***
  IF llAprPart   && approve partially.

    lnRateVal = &lcAPINVHDR..nExRate &lcExSin2 &lcAPINVHDR..nCurrUnit

    IF lnExchAmnt <> 0 .OR. SYS(18) <> 'PBAPPROV'
      loFormSet.Ariaform1.lnSesAmnt.VALUE = loFormSet.Ariaform1.lnSesAmnt.VALUE + ROUND((lnAprToPay - NINVAMTAP) &lcExSin1 lnRateVal, 2)
    ENDIF    && End of IF


    WITH loFormSet.Ariaform1
      .lnSesDisc.VALUE = .lnSesDisc.VALUE + ROUND((lnAprDisc  - NINVDISAP) &lcExSin1 lnRateVal, 2)
      .lnSesAdj.VALUE  = .lnSesAdj.VALUE  + ROUND((lnAprAdj   - NINVADJAP) &lcExSin1 lnRateVal, 2)
      .lnSes1099.VALUE = .lnSes1099.VALUE + ROUND((ln1099amnt - NINVA1099) &lcExSin1 lnRateVal, 2)
    ENDWITH

    *B601519,1 IF Statment to check if the Approved amount in the Approved
    *             currency is not 0
    IF lnExchAmnt <> 0

      REPLACE NINVAMTAP  WITH lnAprToPay ,;
        NINVDISAP  WITH lnAprDisc  ,;
        NINVADJAP  WITH lnAprAdj   ,;
        NINVA1099  WITH ln1099amnt ,;
        cAprCurCod WITH lcAprCurCod,;
        nAprExRat  WITH lnAprExRat ,;
        nAprCurUnt WITH lnAprCurUnt,;
        nInvFAAp   WITH lnExchAmnt

    ENDIF   && End of IF

  ELSE           && approve fully.

    lnRateVal = &lcAPINVHDR..nExRate &lcExSin2 &lcAPINVHDR..nCurrUnit
    WITH loFormSet.Ariaform1
      .lnSesAmnt.VALUE = .lnSesAmnt.VALUE + ROUND((&lcAPINVHDR..NINVAMNT ;
        - &lcAPINVHDR..NINVPAID ;
        - &lcAPINVHDR..NINVDISTK;
        - &lcAPINVHDR..NINVADJ  ;
        - &lcAPINVHDR..NINVDISOF;
        - &lcAPINVHDR..NINVAMTAP) &lcExSin1 lnRateVal, 2)
      .lnSesDisc.VALUE = .lnSesDisc.VALUE + ROUND((&lcAPINVHDR..NINVDISOF;
        - &lcAPINVHDR..NINVDISAP) &lcExSin1 lnRateVal, 2)
      .lnSesAdj.VALUE  = .lnSesAdj.VALUE  - ROUND(&lcAPINVHDR..NINVADJAP   &lcExSin1 lnRateVal, 2)
      .lnSes1099.VALUE = .lnSes1099.VALUE - ROUND(&lcAPINVHDR..nInvA1099   &lcExSin1 lnRateVal, 2)
      .REFRESH()
    ENDWITH

    REPLACE NINVAMTAP  WITH (NINVAMNT-NINVPAID-NINVDISTK-NINVADJ-NINVDISOF),;
      NINVDISAP  WITH NINVDISOF,;
      NINVADJAP  WITH 0,;
      nInvA1099  WITH 0,;
      cAprCurCod WITH lcAprCurCod,;
      nAprExRat  WITH lnAprExRat ,;
      nAprCurUnt WITH lnAprCurUnt,;
      nInvFAAp   WITH ROUND((NINVAMNT-NINVPAID-NINVDISTK-NINVADJ-NINVDISOF) &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2)

  ENDIF
  *=gfAdd_Info()  && Add the audit information to the record.
  *E303011,1 TMI 12/22/2011 [Start] unlock as a whole when saving
  *=gfObj_lock(.F.)  && unlock the pointed record.
  *E303011,1 TMI 12/22/2011 [End  ]

  *B601519,1 IF Statment to check if the Approved amount in the Approved
  *             currency is not 0 or this vlidation function is not for
  *             the Push Button pbApprov
  IF lnExchAmnt <> 0 .OR. SYS(18) <> 'PBAPPROV'
    SKIP IIF(EOF(),0,1)   && if !eof go to the next record.
    SKIP IIF(EOF(),-1,0)  && if went to eof return to the last record.
  ENDIF   && End of IF
  *B601519,1 Change this lines [Begin]

  IF !llAprPart
    ***Refresh say field open apen amount. ***
    loFormset.Ariaform1.REFRESH()
    lnBrRecNo  = RECNO('&lcAPINVHDR')
  ENDIF

ENDIF

lnExchAmnt  = IIF(lnAprCurUnt > 0 AND lnAprExRat > 0,;
  ROUND(lnAprToPay &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2), 0)
SHOW GET lnAprExRat
SHOW GET lnExchAmnt    &&

loFormSet.Ariaform1.grdAprove.AFTERROWCOLCHANGE()
loFormSet.Ariaform1.REFRESH()
*- End of lfvAprov

***********************************************************************************
*Name      : lfvCanPay
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Validation function of the cancel push button of the Approved for payment.
***********************************************************************************
FUNCTION lfvCanPay

*** restore the old approved values. ***
lcBankCode  = lcOldBank
lcCheckCode = lcOldCheck
lcGLAcct    = lcOldGlAcc
lcHGlAct    = lcHOldGlAc

lfvCanPay  = .T.

*** restore the old approved values in case of approved partially. ***
IF llAprPart

  *- Restore old currency fields values
  lcAprCurCod = lcOldCurr
  lnAprCurUnt = lnOldUnt
  lnAprExRat  = lnOldExRat

  lnAprToPay = lnOldAprTPy
  lnAprDisc  = lnOldAprDisc
  lnAprAdj   = lnOldAprAdj
  ln1099amnt = lnO1099amnt
ENDIF

*SELECT APINVHDR
llCanclAp   = .T.      &&set cancelled flag
*old : CLEAR READ


***********************************************************************************
*Name      : lfvAprDisc
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Valid function of the discount.
***********************************************************************************
FUNCTION lfvAprDisc
*E303011,1 TMI 12/23/2011 [Start]
PARAMETERS loAprFormset,loObj

*** ifinvoice amount is positive. ***
IF &lcAPINVHDR..NINVAMNT >= 0
  *** if the total approved ammount not between zero and invoice amount. ***
  *B600808,1 Compare the new approved amount with the open invoice amount
  *B600808,1 excluding the old approved amounts,
  *B600808,1 i.e. remove the approved amounts field from the open invoice
  *B600808,1 amount calculation.
  IF !BETWEEN(lnAprToPay + lnAprDisc + lnAprAdj, 0, ;
      &lcAPINVHDR..nInvAmnt  - &lcAPINVHDR..nInvPaid ;
      - &lcAPINVHDR..nInvDisTk - &lcAPINVHDR..nInvAdj)

    ** MESSAGE : " Total approved amount can not be greater than "
    **           " the open amount.                              "
    **           "                       � Ok �
    =gfModalGen("TRM04015B00000","DIALOG",lcTAprAmnt+"|"+lcTInvoice)
    *lnAprDisc = lnOldAprDisc       && get the old discount amount.
    *SHOW GET lnAprToPay
    loObj.VALUE = loObj.OldValue

  ENDIF
ELSE && if the invoice amount is negative. ***
  *** if the total approved ammount not between invoice amount and zero. ***
  *B600808,1 Compare the new approved amount with the open invoice amount
  *B600808,1 excluding the old approved amounts,
  *B600808,1 i.e. remove the approved amounts field from the open invoice
  *B600808,1 amount calculation.
  IF !BETWEEN(lnAprToPay + lnAprDisc + lnAprAdj, ;
      &lcAPINVHDR..nInvAmnt  - &lcAPINVHDR..nInvPaid ;
      - &lcAPINVHDR..nInvDisTk - &lcAPINVHDR..nInvAdj, 0)

    ** MESSAGE : " Total approved amount can not be greater than "
    **           " the open amount.                              "
    **           "                       � Ok �
    =gfModalGen("TRM04015B00000","DIALOG",lcTAprAmnt+"|"+lcTDebitM)
    *lnAprDisc = lnOldAprDisc        && get the old discount amount.
    *SHOW GET lnAprToPay
    loObj.VALUE = loObj.OldValue
  ENDIF
ENDIF
loAprFormset.Ariaform1.REFRESH()
*- End of lfvAprDisc.

***********************************************************************************
*Name      : lfvAprToPay
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Validation function of to pay.
***********************************************************************************
FUNCTION lfvAprToPay
PARAMETERS loAprFormset,loObj
*** if the invoice amount is positive. ***
IF &lcAPINVHDR..NINVAMNT >= 0
  *** if the total approved ammount not between zero and invoice amount. ***
  *B600808,1 Compare the new approved amount with the open invoice amount
  *B600808,1 excluding the old approved amounts,
  *B600808,1 i.e. remove the approved amounts field from the open invoice
  *B600808,1 amount calculation.
  IF !BETWEEN(lnAprToPay + lnAprDisc + lnAprAdj, 0, ;
      &lcAPINVHDR..nInvAmnt  - &lcAPINVHDR..nInvPaid ;
      - &lcAPINVHDR..nInvDisTk - &lcAPINVHDR..nInvAdj)
    ** MESSAGE : " Total approved amount can not be greater than "
    **           " the open amount.                              "
    **           "                       � Ok �
    =gfModalGen("TRM04015B00000","DIALOG",lcTAprAmnt+"|"+lcTInvoice)
    *lnAprToPay = lnOldAprTPy   && save the old payment amount.
    *SHOW GET lnAprToPay
    loObj.VALUE = loObj.OldValue
  ENDIF
  *** if the invoice amount is negative
ELSE
  *** if the total approved ammount not between invoice amount and zero. ***
  *B600808,1 Compare the new approved amount with the open invoice amount
  *B600808,1 excluding the old approved amounts,
  *B600808,1 i.e. remove the approved amounts field from the open invoice
  *B600808,1 amount calculation.
  IF !BETWEEN(lnAprToPay + lnAprDisc + lnAprAdj,;
      &lcAPINVHDR..nInvAmnt - &lcAPINVHDR..nInvPaid  - ;
      &lcAPINVHDR..nInvDisTk - &lcAPINVHDR..nInvAdj, 0)
    ** MESSAGE : " Total approved amount can not be greater than "
    **           " the open amount.                              "
    **           "                       � Ok �
    =gfModalGen("TRM04015B00000","DIALOG",lcTAprAmnt+"|"+lcTDebitM)
    *lnAprToPay = lnOldAprTPy    && get the old payment amount.
    *old : SHOW GET lnAprToPay
    loObj.VALUE = loObj.OldValue
  ENDIF
ENDIF

*- Get the approved to pay amount in approval currency.
*FUNCTION  HISH 12/05/95. Used the variables hold signs in the equation. (Begin)
*lnExchAmnt  = IIF(lnAprCurUnt > 0,;
ROUND(lnAprToPay * lnAprExRat / lnAprCurUnt, 2), 0)
lnExchAmnt  = IIF(lnAprCurUnt > 0 AND lnAprExRat > 0,;
  ROUND(lnAprToPay &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2), 0)
*old : SHOW GET lnExchAmnt
loAprFormset.Ariaform1.REFRESH()
*- End of lfvAprToPay

***********************************************************************************
*Name      : lfv1099amnt
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Validation function of 1099 AMOUNT.
***********************************************************************************
FUNCTION lfv1099amnt
*E303011,1 TMI 12/23/2011 [Start]
PARAMETERS loAprformset,loObj

IF !BETWEEN(ln1099amnt,lnAprToPay,0) .AND. !BETWEEN(ln1099amnt,0,lnAprToPay)
  ** MESSAGE : " The 1099 amount cannot be greater than  "
  **           " the �.     "
  **           "                 � Ok �                  "
  =gfModalGen("TRM04061B00000","DIALOG","approved amount to pay")
  *ln1099amnt=lnO1099amnt
  *SHOW GET ln1099amnt
  loObj.VALUE = loObj.OldValue
  loAprformset.Ariaform1.REFRESH()
ENDIF

***********************************************************************************
*Name      : lfvAprAdj
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Valid function of the Appr. Adjust.
***********************************************************************************
FUNCTION lfvAprAdj
*E303011,1 TMI 12/23/2011 [Start]
PARAMETERS loAprformset,loObj

*** if the invoice ammount is positive. ***
IF &lcAPINVHDR..NINVAMNT >= 0
  *** if the total approved ammount not between zero and invoice amount. ***
  *B600808,1 Compare the new approved amount with the open invoice amount
  *B600808,1 excluding the old approved amounts,
  *B600808,1 i.e. remove the approved amounts field from the open invoice
  *B600808,1 amount calculation.
  IF !BETWEEN(lnAprToPay + lnAprDisc + lnAprAdj, 0, ;
      &lcAPINVHDR..nInvAmnt  - &lcAPINVHDR..nInvPaid ;
      - &lcAPINVHDR..nInvDisTk - &lcAPINVHDR..nInvAdj)
    ** MESSAGE : " Total approved amount can not be greater than "
    **           " the open amount.                              "
    **           "                       � Ok �
    =gfModalGen("TRM04015B00000","DIALOG",lcTAprAmnt+"|"+lcTInvoice)
    *lnAprAdj = lnOldAprAdj            && get the old adjustment amount.
    *SHOW GET lnAprToPay
    loObj.VALUE = loObj.OldValue
  ENDIF
  *** if the invoice ammount is negative. ***
ELSE
  *** if the total approved ammount not between invoice amount and zero. ***
  *B600808,1 Compare the new approved amount with the open invoice amount
  *B600808,1 excluding the old approved amounts,
  *B600808,1 i.e. remove the approved amounts field from the open invoice
  *B600808,1 amount calculation.
  IF !BETWEEN(lnAprToPay + lnAprDisc + lnAprAdj, ;
      &lcAPINVHDR..nInvAmnt  - &lcAPINVHDR..nInvPaid ;
      - &lcAPINVHDR..nInvDisTk - &lcAPINVHDR..nInvAdj, 0)

    ** MESSAGE : " Total approved amount can not be greater than "
    **           " the open amount.                              "
    **           "                       � Ok �
    =gfModalGen("TRM04015B00000","DIALOG",lcTAprAmnt+"|"+lcTDebitM)
    *lnAprAdj = lnOldAprAdj              && get the old adjustment value.
    *SHOW GET lnAprToPay
    loObj.VALUE = loObj.OldValue
  ENDIF
ENDIF
loAprformset.Ariaform1.REFRESH()
*- End of lfvAprAdj.

***********************************************************************************
*Name      : lfvCancel
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Restore old values if push cancel from scope.
***********************************************************************************
FUNCTION lfvCancel

llScope    = .F.

*** RESTORE OLD VAR. IN CURRENT VAR.
ldToDisDat = ldOToDisDt
ldFrDisDat = ldOFrDisDt
ldToDueDat = ldOToDueDt
ldFrDueDat = ldOFrDueDt
lcPriority = lcOldPrty
lcVendCode = lcOVendCod
lcVendComp = lcOldVnCmp
lcMethod   = lcOMethod
lcDivCode  = lcOldDvCod
* Remark the next line because it gives an error
* if we go in the Select Invoice scope screen again
* and at the same time the variable lcMethod was reinitialized
* Previously
*lcMethod   = lcOldpymth

lcInvRef   = lcOldInvRf
rbODuDsDt  = rbDuDsDt

***********************************************************************************
*Name      : lfvBnkChk
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Valid function for get fields lcBankCode (bank code), and lcCheckCode (checking account code)
***********************************************************************************
FUNCTION lfvBnkChk
PARAMETERS loAprFormset,loObj
loFormSet = loAprFormset.CallingForm
*- Store the old currency field values before validation
*PRIVATE llVldObj
*Old : PRIVATE lcOldCurr, lnOldRate, lnOldUnit, llVldObj
* old : IF llBrowse .OR. EVALUATE(SYS(18)) <> lcOldVal
IF loObj.VALUE <> loObj.OldValue
  *- lcOldCurr : old currency code
  *- lnOldRate : old exchange rate
  *- lnOldUnit : old currency unit
  lcOldCurr = lcAprCurCod
  *lnOldRate = lnAprExRat
  lnOldExRat = lnAprExRat
  *lnOldUnit = lnAprCurUnt
  lnOldUnt = lnAprCurUnt

  *old : llVldObj = lfBnkChk(@laBankObjs, lcOldVal, @llBrowse)

  *- Get the currency of the checking account.
  *old : IF llVldObj .AND. EVALUATE(SYS(18)) <> lcOldVal
  lcAprCurCod = APCHECKS.cCurrCode
  IF lcAprCurCod <> APINVHDR.cCurrCode
    *B600849,1 HISH 11/29/95. Change parameter to get rate and unit to change from invoice (Begin)
    *B600849,1                currency to approved currency.
    *lnAprExRat  = gfChkRate('lnAprCurUnt'    , lcAprCurCod,;
    APINVHDR.dInvDate, .F., .F., ;
    APINVHDR.cCurrCode, .T.)

    *- Change the calling of gfChkRate so if the [BEGIN]
    * Invoice currency is the base currency I make it the To currency
    * and if not the Approve currency is the To currency
    *lnAprExRat = gfChkRate('lnAprCurUnt', APINVHDR.cCurrCode ,APINVHDR.dInvDate, ;
    *                        .F., .F., lcAprCurCod, .T.)
    *- IF Statment to check if the Invoice currency is the
    * same as the base currency
    *!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004][Start]
    *IF APINVHDR.cCurrCode = gcBaseCurr
    IF APINVHDR.cCurrCode = oAriaApplication.BaseCurrency
      *!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004][End]
      lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCurCod ,APINVHDR.dInvDate, ;
        .F., .F., APINVHDR.cCurrCode, .T.)
    ELSE      && Else
      lnAprExRat = gfChkRate('lnAprCurUnt', APINVHDR.cCurrCode ,APINVHDR.dInvDate, ;
        .F., .F., lcAprCurCod, .T.)
    ENDIF     && End of IF
    *- Change the calling of gfChkRate [END]

    *B600849,1 (End)
    *FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
    *FUNCTION  to exchange currency from invoice currency to company base currency.
    *FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
    lcExSin2 = ' '
    lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
    *lcExSin2 = IIF(lcExSin1 = '*','/','*')
    *FUNCTION  to exchange currency from invoice currency to approved currency.
    lcExSin4 = ' '

    *- Change the calling of gfGetExSin so if the [BEGIN]
    * Invoice currency is the base currency I make it the To currency
    * and if not the Approve currency is the To currency
    *lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
    *- IF Statment to check if the Invoice currency is the
    * same as the base currency
    *!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004][Start]
    *IF APINVHDR.cCurrCode = gcBaseCurr
    IF APINVHDR.cCurrCode = oAriaApplication.BaseCurrency
      *!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004][End]
      lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,APINVHDR.cCurrCode)
      lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
      lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
    ELSE   && Else
      lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
    ENDIF  && End of IF
    *- Change the calling of gfGetExSin [END]

    *lcExSin4 = IIF(lcExSin3 = '*','/','*')

    *FUNCTION  HISH 12/05/95. Used the variables hold signs in the equation. (Begin)
    *lnExchAmnt  = IIF(lnAprCurUnt > 0,;
    ROUND(lnAprToPay * lnAprExRat / lnAprCurUnt, 2), 0)
    lnExchAmnt  = IIF(lnAprCurUnt > 0 AND lnAprExRat > 0,;
      ROUND(lnAprToPay &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2), 0)

  ELSE
    STORE 1 TO lnAprExRat, lnAprCurUnt
    lnExchAmnt  = lnAprToPay
  ENDIF
  SHOW GET lcAprCurCod
  SHOW GET lnExchAmnt
  *Old : IF loFormSet.llEditEx .AND. lcAprCurCod <> APINVHDR.cCurrCode
  *Old :   SHOW GET lnAprExRat ENABLE
  *Old : ELSE
  *Old :   SHOW GET lnAprExRat DISABLE
  *Old : ENDIF
  *!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004][Start]
  *loAprFormset.AriaForm1.txtaprexrat.Enabled  =  loFormSet.llEditEx .AND. lcAprCurCod <> APINVHDR.cCurrCode
  IF TYPE('loAprFormset.AriaForm1.txtaprexrat') == 'O'
    loAprFormset.AriaForm1.txtaprexrat.ENABLED  =  loFormSet.llEditEx .AND. lcAprCurCod <> APINVHDR.cCurrCode
  ENDIF
  *!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004][End]
ELSE
  IF !EMPTY(lcBankCode)
    *- Restore old currency fields
    lcAprCurCod  = lcOldCurr
    *lnAprExRat   = lnOldRate
    lnAprExRat   = lnOldExRat
    *lnAprCurUnt  = lnOldUnit
    lnAprCurUnt  = lnOldUnt

  ELSE
    lcAprCurCod = SPACE(5)
    STORE 0 TO lnAprExRat, lnAprCurUnt, lnExchAmnt
    *Old : SHOW GET lcAprCurCod
    *Old : SHOW GET lnAprExRat DISABLE
    *!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004][Start]
    *loAprFormset.AriaForm1.txtaprexrat.Enabled  =  .F.
    IF TYPE('loAprFormset.AriaForm1.txtaprexrat')
      loAprFormset.AriaForm1.txtaprexrat.ENABLED  =  .F.
    ENDIF
    *!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004][End]
    *Old : SHOW GET lnExchAmnt
  ENDIF
  * Old : ENDIF

  *old : =lfRefresh()
  *SELECT APINVHDR
  *RETURN IIF(llVldObj, .T., 1)
  loAprFormset.Ariaform1.REFRESH()
ENDIF

***********************************************************************************
*Name      : lfvExRate
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Valid function for lnAprExRat field
***********************************************************************************
FUNCTION lfvExRate
PARAMETERS loArpFormset,loObj &&
*- if the entered value is not greater than zero,
*- Message :  "   � should be greater than �.   "
*-                 �   OK   �
lnOldVal = loObj.OldValue
IF lnAprExRat <> lnOldVal
  IF lnAprExRat < 0 .AND. ;
      gfModalGen("TRM04072B00000","DIALOG", lcTExRateMsg) > 0
    lnAprExRat = lnOldVal
  ENDIF
  *FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
  *FUNCTION  to exchange currency from invoice currency to approved currency.
  *FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
  lcExSin4 = ' '

  *- Change the calling of gfGetExSin so if the [BEGIN]
  * Invoice currency is the base currency I make it the To currency
  * and if not the Approve currency is the To currency
  *lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
  *- IF Statment to check if the Invoice currency is the
  * same as the base currency
  IF &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
    lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,&lcAPINVHDR..cCurrCode)
    lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
    lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
  ELSE   && Else
    lcExSin3 = gfGetExSin(@lcExSin4,&lcAPINVHDR..cCurrCode,lcAprCurCod)
  ENDIF  && End of IF
  *- Change the calling of gfGetExSin [END]

  *- Get the approved to pay amount in approval currency.
  *FUNCTION  HISH 12/05/95. Used the variables hold signs in the equation. (Begin)
  *lnExchAmnt  = IIF(lnAprCurUnt > 0,;
  ROUND(lnAprToPay * lnAprExRat / lnAprCurUnt, 2), 0)
  lnExchAmnt  = IIF(lnAprCurUnt > 0 AND lnAprExRat > 0,;
    ROUND(lnAprToPay &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2), 0)
  SHOW GET lnExchAmnt
  loArpFormset.Ariaform1.REFRESH()
ENDIF
*- end

***********************************************************************************
*Name      : lfvAprCurCod
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Valid function for lcAprCurCod field
***********************************************************************************
FUNCTION lfvAprCurCod
PARAMETERS loArpFormset,llGetExRate
LOCAL loFormSet,loAprov,loAprExRat
loFormSet = loArpFormset.callingform
loAprov = loArpFormset.Ariaform1
*!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004][Start]
*loAprExRat = loArpFormset.Ariaform1.txtaprexrat
IF TYPE('loArpFormset.Ariaform1.txtaprexrat') == 'O'
  loAprExRat = loArpFormset.Ariaform1.txtaprexrat
ENDIF
*!B609860,1 SAB 04/17/2012 Fix Forign Currency Problem In Partially Approve [T20120304.0004][End]

PRIVATE laRetVal, lcCurObj
DECLARE laRetVal[1]
laRetVal     = " "
*old : lcCurObj = SYS(18)
*E303011,1 TMI 01/29/2012 [Start] move above with the lfforminit function
*=gfOpenTable(oAriaApplication.SysPath + 'SYCCURR' , 'CCURRCODE' , 'SH')
*E303011,1 TMI 01/29/2012 [End  ]

llBrowse = loAprov.kbAprCurrCode.Selectedfrombrowse
IF llBrowse .OR. !SEEK(loAprov.kbAprCurrCode.keytextbox.VALUE,'SYCCURR') .OR. ;
    ATC("?",loAprov.kbAprCurrCode.KeyTextBox.VALUE) > 0
  SELECT SYCCURR
  lcFile_Ttl = "Currency"
  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
    "CCURRDESC :R :H= 'Description',  " +;
    "CCURRSMBL :R :H= 'Symbol'"
  =ARIABROW('', lcFile_Ttl , .F., .F., .F., .F., .F., .T., ;
    'CCURRCODE', 'laRetVal', .F., .F., .F., ;
    .F., .F., .F., .F., .F., ;
    .F., .F.)
  IF EMPTY(laRetVal[1]) &&
    loAprov.kbAprCurrCode.KeyTextBox.VALUE = loAprov.kbAprCurrCode.KeyTextBox.OldValue
  ELSE
    loAprov.kbAprCurrCode.KeyTextBox.VALUE = laRetVal[1]
  ENDIF
ENDIF

*Old :IF lfGetExt('SYCCURR', 'CCURRCODE', loCur.OldValue, llBrowse,  'cCurrCode',;
@laRetVal)
IF loAprov.kbAprCurrCode.KeyTextBox.VALUE <> loAprov.kbAprCurrCode.KeyTextBox.OldValue
  *old : &lcCurObj   = laRetVal[1]
  IF llGetExRate
    IF laRetVal[1] <> &lcAPINVHDR..cCurrCode
      *B600849,1 HISH 11/29/95. Change parameter to get rate and unit to change from invoice (Begin)
      *B600849,1                currency to approved currency.
      *STORE gfChkRate('lnAprCurUnt'    , laRetVal[1],     ;
      APINVHDR.dInvDate, .T., .F.,;
      APINVHDR.cCurrCode, .T.)    ;
      TO lnAprExRat

      *- Change the calling of gfChkRate so if the [BEGIN]
      * Invoice currency is the base currency I make it the To currency
      * and if not the Approve currency is the To currency
      *STORE gfChkRate('lnAprCurUnt'    ,APINVHDR.cCurrCode,     ;
      *                        APINVHDR.dInvDate, .T., .F.,;
      *                        laRetVal[1], .T.)    ;
      *      TO lnAprExRat
      *- IF Statment to check if the Invoice currency is the
      * same as the base currency
      IF &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
        STORE gfChkRate('lnAprCurUnt'    ,laRetVal[1],     ;
          &lcAPINVHDR..dInvDate, .T., .F.,;
          &lcAPINVHDR..cCurrCode, .T.)    ;
          TO lnAprExRat
      ELSE      && Else
        STORE gfChkRate('lnAprCurUnt'    ,&lcAPINVHDR..cCurrCode,     ;
          &lcAPINVHDR..dInvDate, .T., .F.,;
          laRetVal[1], .T.)    ;
          TO lnAprExRat
      ENDIF     && End of IF
      *- Change the calling of gfChkRate [END]


      *B600849,1 (End)
      *FUNCTION  HISH 12/05/95. Got the equation signs. (Begin)
      *FUNCTION  to exchange currency from invoice currency to company base currency.
      *FUNCTION  HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
      lcExSin2 = ' '
      lcExSin1 = gfGetExSin(@lcExSin2,&lcAPINVHDR..cCurrCode)
      *lcExSin2 = IIF(lcExSin1 = '*','/','*')
      *FUNCTION  to exchange currency from invoice currency to approved currency.
      lcExSin4 = ' '

      *- Change the calling of gfGetExSin so if the [BEGIN]
      * Invoice currency is the base currency I make it the To currency
      * and if not the Approve currency is the To currency
      *lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,laRetVal[1])
      *- IF Statment to check if the Invoice currency is the
      * same as the base currency
      IF &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
        lcExSin3 = gfGetExSin(@lcExSin4,laRetVal[1],&lcAPINVHDR..cCurrCode)
        lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
        lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
      ELSE   && Else
        lcExSin3 = gfGetExSin(@lcExSin4,&lcAPINVHDR..cCurrCode,laRetVal[1])
      ENDIF  && End of IF
      *- Change the calling of gfGetExSin [END]

      *lcExSin4 = IIF(lcExSin3 = '*','/','*')


      *FUNCTION  HISH 12/05/95. Used the variables hold signs in the equation. (Begin)
      *lnExchAmnt  = IIF(lnAprCurUnt > 0,;
      ROUND(lnAprToPay * lnAprExRat ;
      / lnAprCurUnt, 2), 0)
      lnExchAmnt  = IIF(lnAprCurUnt > 0 AND lnAprExRat > 0,;
        ROUND(lnAprToPay &lcExSin3 lnAprExRat ;
        &lcExSin4 lnAprCurUnt, 2), 0)
      *IF loFormset.llEditEx
      *old : SHOW GET lnAprExRat ENABLE
      *ELSE
      *old : SHOW GET lnAprExRat DISABLE
      *ENDIF
      loAprExRat.ENABLE = loFormset.llEditEx
    ELSE
      STORE 1 TO lnAprExRat, lnAprCurUnt
      lnExchAmnt = lnAprToPay
      *old : SHOW GET lnAprExRat DISABLE
    ENDIF
    *old : SHOW GET lnExchAmnt
  ENDIF
ENDIF
llBrowse     = .F.
loArpFormset.Ariaform1.REFRESH()
*- End of lfvAprCurCod

***********************************************************************************
*Name      : lfSetDef
*Developer : TMI - Tarek Mohamed Inbrahim
*Date      : 12/24/2011
*Purpose   : Function to set the default currency and exchange rate
***********************************************************************************
FUNCTION lfSetDef

IF !EMPTY(&lcAPINVHDR..nInvAmtAp)
  lcAprCurCod = &lcAPINVHDR..cAprCurCod
  lnAprExRat = &lcAPINVHDR..nAprExRat
  lnExchAmnt = &lcAPINVHDR..nInvFAAp
  lnAprToPay = &lcAPINVHDR..nInvAmtAp
  lnAprDisc = &lcAPINVHDR..nInvDisAp
  lnAprAdj = &lcAPINVHDR..nInvAdjAp
  ln1099Amnt = &lcAPINVHDR..nInvA1099
  lnAprCurUnt = &lcAPINVHDR..nCurrUnit

  IF &lcAPINVHDR..cCurrCode = oAriaApplication.BaseCurrency
    lcExSin3 = gfGetExSin(@lcExSin4 , lcAprCurCod , &lcAPINVHDR..cCurrCode)
    lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
    lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
  ELSE   && Else
    lcExSin3 = gfGetExSin(@lcExSin4 , &lcAPINVHDR..cCurrCode , lcAprCurCod)
  ENDIF  && End of IF
ENDIF