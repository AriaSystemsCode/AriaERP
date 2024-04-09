*:****************************************************************
*: Program file  : SOREORD
*: Program desc. : Re-Order Processing by Store.
*: System        : Aria Apparel System - Version 40.
*: Module        : (SO)
*: Developer     : SABER A.Razek -  (SAB)
*: Date          : 07/26/2012
*: Tracking#     : C201503
*:****************************************************************
*:Modifications  :
*:****************************************************************

#INCLUDE R:\aria4xp\PRGS\SO\SOREORD.H  
SET STEP ON 
DO FORM (oAriaApplication.ClientScreenHome+"\SO\SOREORD.SCX")


*:**************************************************************************************************************************
*														 END of Code.
*:**************************************************************************************************************************
*:*************************************************************
*: Name      : lfFormInit
*: Developer : SABER A.Razek -  (SAB)
*: Date      : 07/26/2012
*: Purpose   : called from the Screen init Method
*:*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet
          
SET MULTILOCKS ON
*- Open tables 
=gfOpenTable('CUSTOMER','CUSTOMER','SH')
=gfOpenTable('ORDHDR','ORDHDR','SH')    && CORDTYPE+ORDER
=gfOpenTable('ORDLINE','ORDLINE','SH')  && CORDTYPE+ORDER+STR(LINENO,6)

*- Initializations
WITH loFormSet
  .nWorkArea = 'CUSTOMER'
  .otoolbar.nWorkArea = 'CUSTOMER'
  .DataEnvironment.InitialSelectedAlias = 'CUSTOMER'
ENDWITH 

*-- Define custom tool bar buttons (Scope Button)
DECLARE loFormSet.laPanelObj[1,6] 
STORE '' TO loFormSet.laPanelObj
loFormSet.laPanelObj[1,1] = 'pbScop'
loFormSet.laPanelObj[1,2] = oAriaApplication.BitMapHome+"SCOPE.BMP"
loFormSet.laPanelObj[1,3] = 'lfGetData'    && the loFormSet is sent by default as a parameter 
loFormSet.laPanelObj[1,4] = "Option Grid"
loFormSet.laPanelObj[1,5] = "Option Grid"
loFormSet.laPanelObj[1,6] = 'S'

*- Create temp files
=lfCrTmpFls()

=lfAddProp(loFormSet, 'lf1StRun', .T.)
=lfAddProp(loFormSet, 'lnRpPrePack', 0)
=lfAddProp(loFormSet, 'lcRpOrdMode', '')
=lfAddProp(loFormSet, 'lcAccount', '')
=lfAddProp(loFormSet, 'llSelectAllEna', .F.)
=lfAddProp(loFormSet, 'llSelectNoneEna', .F.)
=lfAddProp(loFormSet, 'llSelectIvertEna', .F.)
=lfAddProp(loFormSet, 'lcSelectCap', LANG_SELECT)
=lfAddProp(loFormSet, 'lcDepositTemp', gfTempName())  && This propery is added for the SOUPDATE.PRG


LOCAL lcReOrdLines
lcReOrdLines = loFormSet.lcReOrdLines
  
WITH loFormSet.AriaForm1
  .keyStyle.ControlSource  = lcReOrdLines+".STYLE"
  .txtStore.ControlSource  = lcReOrdLines+".STORE"
  .txtMinQty.ControlSource = lcReOrdLines+".NMININVQTY"
  .txtMaxQty.ControlSource = lcReOrdLines+".NMAXINVQTY"
  
  FOR lnIndx = 1 TO 8
    lcIndx = ALLTRIM(STR(lnIndx))
    .cntOpenOrder.txtQty&lcIndx..ControlSource    = lcReOrdLines+".OPEN"+lcIndx
    .cntOnHand.txtQty&lcIndx..ControlSource       = lcReOrdLines+".ONHAND"+lcIndx
    .cntTotAvailable.txtQty&lcIndx..ControlSource = lcReOrdLines+".AVLBL"+lcIndx
    .cntReOrder.txtQty&lcIndx..ControlSource      = lcReOrdLines+".REORDER"+lcIndx
  ENDFOR
  .cntOpenOrder.txtTotQty.ControlSource    = lcReOrdLines+".TOTOPEN"
  .cntOnHand.txtTotQty.ControlSource       = lcReOrdLines+".TOTONHAND"
  .cntTotAvailable.txtTotQty.ControlSource = lcReOrdLines+".TOTAVLBL"
  .cntReOrder.txtTotQty.ControlSource      = lcReOrdLines+".TOTREORDER"
ENDWITH

WITH loFormSet.AriaForm1.AriaMultiSelectionGrid1.grdMultiSelectionGrid  
  .RecordSource = ''
  .ColumnCount = 11
  .RecordSource = loFormSet.lcReOrdLines
  LOCAL lnI,lcI
  lnI = 0
  oGrid = loFormSet.AriaForm1.AriaMultiSelectionGrid1.grdMultiSelectionGrid
 
  oGrid.Column1.CurrentControl = 'Ariacheckbox1'  
  =lfAddColumn(@lnI,oGrid, '&lcReOrdLines..LSELECT', '', 20)
  =lfAddColumn(@lnI,oGrid, '&lcReOrdLines..ORDER', LANG_GRID_ORDER, 50)           && Order No.
  =lfAddColumn(@lnI,oGrid, '&lcReOrdLines..ACCOUNT', LANG_GRID_ACCOUNT, 50)       && Account Code
  =lfAddColumn(@lnI,oGrid, '&lcReOrdLines..STORE', LANG_GRID_STORE, 60)           && Store Code
  =lfAddColumn(@lnI,oGrid, '&lcReOrdLines..STYLE', LANG_GRID_STYLE, 130)          && Style Code
  =lfAddColumn(@lnI,oGrid, '&lcReOrdLines..NMININVQTY', LANG_GRID_NMININVQTY, 50) && Minimum Qty.
  =lfAddColumn(@lnI,oGrid, '&lcReOrdLines..NMAXINVQTY', LANG_GRID_NMAXINVQTY, 50) && Maximum Qty.
  =lfAddColumn(@lnI,oGrid, '&lcReOrdLines..TOTAVLBL', LANG_GRID_TOTAVLBL, 50)     && Toatal Available Qty.
  =lfAddColumn(@lnI,oGrid, '&lcReOrdLines..TOTOPEN', LANG_GRID_OPEN, 50)          && Open Qty.
  =lfAddColumn(@lnI,oGrid, '&lcReOrdLines..TOTONHAND', LANG_GRID_ONHAND, 55)      && On Han Qty.
  =lfAddColumn(@lnI,oGrid, '&lcReOrdLines..TOTREORDER', LANG_GRID_REORDER, 55)    && Re-Order Qty.
  *- Remove extra columns 
  FOR lnX = lnI+1 TO oGrid.ColumnCount
    lcX = ALLTRIM(STR(lnX))
    oGrid.RemoveObject("Column&lcX")
  ENDFOR
   
  .READONLY = .T.
  .Refresh()
  *.DoScroll(2)
ENDWITH 

*- fill up the temp file and collect data
IF ! loFormSet.lf1StRun
  =lfGetData(loFormSet)
  lfAfterRowColChange(loFormSet.AriaForm1.AriaMultiSelectionGrid1.grdMultiSelectionGrid)
ENDIF

ENDFUNC
*- End of lfFormInit


*:*************************************************************
*: Name      : lfChangeMode
*: Developer : SABER A.Razek -  (SAB)
*: Date      : 07/26/2012
*: Purpose   : called from the Screen Change mode Method
*:*************************************************************
PROCEDURE lfChangeMode
PARAMETERS loFormSet

LOCAL llEnable
llEnable = .F.
DO CASE
  CASE loFormSet.ActiveMode = 'S'
    llEnable = .F.
    oAriaApplication.oToolBar.ChangeButtonStatus('pbScop','ENABLED')

  CASE loFormSet.ActiveMode = 'E'
    llEnable = .T.
    oAriaApplication.oToolBar.ChangeButtonStatus('pbScop','DISABLED')    
    
ENDCASE   

WITH loFormSet.Ariaform1  
  .keyStyle.Enabled        = .F.
  .txtDescription.Enabled  = .F.
  .txtStore.Enabled        = .F.
  .txtMinQty.Enabled       = .F.
  .txtMaxQty.Enabled       = .F.
  .cntOpenOrder.Enabled    = .F.
  .cntOnHand.Enabled       = .F.
  .cntTotAvailable.Enabled = .F.
ENDWITH 

loFormSet.AriaForm1.AriaMultiSelectionGrid1.grdMultiSelectionGrid.ReadOnly = .T.
loFormset.Ariaform1.cntReOrder.txtTotQty.ReadOnly = .T.

ENDPROC
*- End of lfChangeMode.


 ************************************************************************************
*:Name      : lfFormUndo
*:Developer : SABER A.Razek -  (SAB)
*:Date      : 07/26/2012
*:Purpose   : undo procedure
************************************************************************************
PROCEDURE lfFormUndo
PARAMETERS loFormSet
=lfGetData(loFormset)

ENDPROC
*- End of lfFormUndo


************************************************************************************
*:Name      : lfFormActivate
*:Developer : SABER A.Razek -  (SAB)
*:Date      : 07/26/2012
*:Purpose   : Run lfGetData in the activate method for the first time the screen runs
************************************************************************************
PROCEDURE lfFormActivate
PARAMETERS loFormSet
IF loFormSet.lf1StRun
  loFormSet.lf1StRun = .F.
  =lfGetData(loFormSet)
ENDIF

ENDPROC
 *- End of lfFormActivate.
 
 
*:*************************************************************
*: Name      : lfvProceed
*: Developer : SABER A.Razek -  (SAB)
*: Date      : 07/26/2012
*: Purpose   : Validate Option Grid
*:*************************************************************
FUNCTION lfvProceed
PARAMETERS loFormSet

IF EMPTY(lfGetCrVal("CUSTOMER.ACCOUNT"))
  **  Message: "You have to enter The ð.       "
  **  Choices: "              ® Ok ¯           "  
  =gfModalGen("TRM04066B00000","DIALOG",'account')  
  RETURN .F.  
ENDIF
IF EMPTY(lfGetCrVal("PRODACTI.DPRD_END"))
  **  Message: "You have to enter The ð.       "
  **  Choices: "              ® Ok ¯           "  
  =gfModalGen("TRM04066B00000","DIALOG",'852 activity period end date')  
  RETURN .F.  
ENDIF

RETURN .T.

ENDFUNC 
*- End of lfvProceed


*:*************************************************************
*: Name      : lfvAccount
*: Developer : SABER A.Razek -  (SAB)
*: Date      : 07/26/2012
*: Purpose   : Validate Account
*:*************************************************************
PROCEDURE lfvAccount

LOCAL loFld
loFld = loOgScroll.ActiveControl

IF EMPTY(loFld.Value) OR loFld.OldValue = loFld.Value
  RETURN
ENDIF
IF gfSeek('M'+PADR(loFld.Value, 5), 'CUSTOMER', 'CUSTOMER')  && TYPE+ACCOUNT+STORE
  RETURN
ENDIF

DECLARE laRpRetFld(1)
lcBrFields    = "ACCOUNT :R :H= 'Acct #' , BTNAME :R :H= 'Name' , PHONE1 :R :H= 'Phone' , CADDRESS6 :R :H= 'Country' , NETBAL :R :H= 'Balance'"
laRpRetFld[1] = ''

lcOldAlias = ALIAS()    && Save the current alias

SELECT CUSTOMER
gfSetOrder('CUSTOMER')
IF loFld.OldValue <> loFld.Value
  lnPOS = ASCAN(loOgScroll.laOgFxFlt,"CUSTOMER.ACCOUNT")
  IF lnPos > 0
    lnPos = ASUBSCRIPT(loOgScroll.laOgFxFlt,lnPos,1)
  ENDIF

  *** Search for the current Account code
  loFld.Value = PADR(ALLTRIM(loFld.Value),5)
  IF !EMPTY(loFld.Value) AND ('?' $ loFld.Value .OR.!SEEK(loFld.Value))
    LOCAL lcKey
    lcKey = SET("Key")
    SET KEY TO 'M'
    =gfBrows([],'ACCOUNT',"laRpRetFld",'Accounts',.F.)
    SET KEY TO &lcKey.
    IF !EMPTY(laRpRetFld[1])
      * Validate that the selected account is an EDI Partner for 852
      IF gfSeek(PADR(laRpRetFld[1], 5), 'EDIACPRT', 'ACCFACT')  && TYPE+CPARTNER
        IF gfSeek(EDIACPRT.CPARTCODE, 'EDIPD', 'PARTTRANS')  && CPARTCODE+CEDITRNTYP+CTRANACTV+CPARTID
          loFld.Value = laRpRetFld[1]
        ELSE
          =gfModalGen('INM00000B00000', .F., .F., .F., LANG_ACCOUNT_NOT_PARTNER)
          loFld.Value = loFld.OldValue
        ENDIF
      ELSE
        =gfModalGen('INM00000B00000', .F., .F., .F., LANG_ACCOUNT_NOT_PARTNER)
        loFld.Value = loFld.OldValue
      ENDIF      
    ELSE
      loFld.Value = loFld.OldValue
    ENDIF
    loFld.Refresh
  ENDIF
  loOgScroll.laOgFxFlt[lnPOS,6] = loFld.Value
  
  lnStorePOS = ASCAN(loOgScroll.laOgFxFlt,"CUSTOMER.STORE")
  IF lnStorePOS > 0
    lnStorePOS = ASUBSCRIPT(loOgScroll.laOgFxFlt,lnStorePOS,1)
    loOgScroll.laOgFxFlt[lnStorePOS,6] = ''
  ENDIF  
ENDIF

IF !EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF

ENDPROC 
* End of lfvBank


******************************************************************************
*:Name      : lfAfterRowColChange
*:Developer : SABER A.Razek -  (SAB)
*:Date      : 07/26/2012
*:Purpose   : After Row Col Change of the Grid
******************************************************************************
PROCEDURE lfAfterRowColChange
PARAMETERS loGrid
LOCAL lcReOrdLines
lcReOrdLines = loGrid.RecordSource

WITH loGrid.Parent.Parent  
  .keyStyle.txtItem.Value = &lcReOrdLines..STYLE
  =gfSEEK(&lcReOrdLines..STYLE,'STYLE','STYLE')
  .txtDescription.Value = Style.Desc1
  .cntOnHand.Scale = Style.Scale
  .cntOnHand.Refresh()
  
  .txtStore.Refresh()
  .txtMinQty.Refresh()
  .txtMaxQty.Refresh()
  .cntOpenOrder.Refresh()
  .cntOnHand.Refresh()
  .cntTotAvailable.Refresh()
  .cntReOrder.Refresh()
ENDWITH

WITH loGrid.Parent
  =lfvSelect('', .Parent.Parent.lcReOrdLines, 'LSELECT', .Parent.Parent)
  
  .cmdSelect.Caption     = .Parent.Parent.lcSelectCap
  .cmdSelectAll.Enabled  = .Parent.Parent.llSelectAllEna
  .cmdSelectNone.Enabled = .Parent.Parent.llSelectNoneEna
  .cmdInvert.Enabled     = .Parent.Parent.llSelectIvertEna
ENDWITH

ENDPROC
 *- End of lfAfterRowColChange.
 

*!*************************************************************
*:Name      : lfAddColumn
*:Developer : SABER A.Razek -  (SAB)
*:Date      : 07/26/2012
*:Purpose   : Procedure to add columns to the passed grid object
*!*************************************************************
PROCEDURE lfAddColumn
LPARAMETERS lnI,oGrid,lcFld,lcTitle,lnWidth
LOCAL lcI
lnI = lnI + 1
lcI = ALLTRIM(STR(lnI))
WITH oGrid
  .Column&lcI..ControlSource    = lcFld
  .Column&lcI..Header1.Caption  = lcTitle
  IF !EMPTY(lnWidth)
    .Column&lcI..Width  = lnWidth
  ENDIF
ENDWITH 

ENDPROC
*- End of lfAddColumn.


*:*************************************************************
*: Name      : lfCallSalesOrd
*: Developer : SABER A.Razek -  (SAB)
*: Date      : 07/26/2012
*: Purpose   : Call Sales Order Screen
*:*************************************************************
PROCEDURE lfCallSalesOrd
LPARAMETERS lcType, lcOrder
*DO FORM (oAriaApplication.ScreenHome+"\SO\SOORD.SCX") WITH lcType,loFormSet
lcExeKeyValue = "'"+lcType+"','"+lcOrder+"'"
oAriaApplication.DoProgram("AWRSOORD",lcExeKeyValue,.F.,'SO')

ENDPROC 
*- End of lfCallSalesOrd


********************************************************************************
*:Name      : lfCrTmpFls
*:Developer : SABER A.Razek -  (SAB)
*:Date      : 07/26/2012
*:Purpose   : Create the temp table that holds the invoices that are ready to be approved
********************************************************************************
PROCEDURE lfCrTmpFls

=lfAddProp(loFormSet,'lcReOrdLines',gfTempName())

LOCAL laStru
DIMENSION laStru[44, 18]
lnIndex = 0

lnIndex = lnIndex + 1
laStru[lnIndex, 1] = 'LSELECT'
laStru[lnIndex, 2] = 'L'
laStru[lnIndex, 3] = 1
laStru[lnIndex, 4] = 0

lnIndex = lnIndex + 1
laStru[lnIndex, 1] = 'ORDER'
laStru[lnIndex, 2] = 'C'
laStru[lnIndex, 3] = 6
laStru[lnIndex, 4] = 0

lnIndex = lnIndex + 1
laStru[lnIndex, 1] = 'ACCOUNT'
laStru[lnIndex, 2] = 'C'
laStru[lnIndex, 3] = 5
laStru[lnIndex, 4] = 0

lnIndex = lnIndex + 1
laStru[lnIndex, 1] = 'STORE'
laStru[lnIndex, 2] = 'C'
laStru[lnIndex, 3] = 8
laStru[lnIndex, 4] = 0

lnIndex = lnIndex + 1
laStru[lnIndex, 1] = 'STYLE'
laStru[lnIndex, 2] = 'C'
laStru[lnIndex, 3] = 19
laStru[lnIndex, 4] = 0

lnIndex = lnIndex + 1
laStru[lnIndex, 1] = 'NMININVQTY'
laStru[lnIndex, 2] = 'N'
laStru[lnIndex, 3] = 6
laStru[lnIndex, 4] = 0

lnIndex = lnIndex + 1
laStru[lnIndex, 1] = 'NMAXINVQTY'
laStru[lnIndex, 2] = 'N'
laStru[lnIndex, 3] = 6
laStru[lnIndex, 4] = 0

FOR lnI = 1 TO 8
  lcI  = ALLTRIM(STR(lnI))
  lnIndex = lnIndex + 1
  laStru[lnIndex, 1] = 'AVLBL' + lcI
  laStru[lnIndex, 2] = 'N'
  laStru[lnIndex, 3] = 6
  laStru[lnIndex, 4] = 0
ENDFOR

lnIndex = lnIndex + 1
laStru[lnIndex, 1] = 'TOTAVLBL'
laStru[lnIndex, 2] = 'N'
laStru[lnIndex, 3] = 6
laStru[lnIndex, 4] = 0

FOR lnI = 1 TO 8
  lcI  = ALLTRIM(STR(lnI))
  lnIndex = lnIndex + 1
  laStru[lnIndex, 1] = 'OPEN' + lcI
  laStru[lnIndex, 2] = 'N'
  laStru[lnIndex, 3] = 6
  laStru[lnIndex, 4] = 0
ENDFOR

lnIndex = lnIndex + 1
laStru[lnIndex, 1] = 'TOTOPEN'
laStru[lnIndex, 2] = 'N'
laStru[lnIndex, 3] = 6
laStru[lnIndex, 4] = 0

FOR lnI = 1 TO 8
  lcI  = ALLTRIM(STR(lnI))
  lnIndex = lnIndex + 1
  laStru[lnIndex, 1] = 'ONHAND' + lcI
  laStru[lnIndex, 2] = 'N'
  laStru[lnIndex, 3] = 6
  laStru[lnIndex, 4] = 0
ENDFOR

lnIndex = lnIndex + 1
laStru[lnIndex, 1] = 'TOTONHAND'
laStru[lnIndex, 2] = 'N'
laStru[lnIndex, 3] = 6
laStru[lnIndex, 4] = 0

FOR lnI = 1 TO 8
  lcI  = ALLTRIM(STR(lnI))
  lnIndex = lnIndex + 1
  laStru[lnIndex, 1] = 'REORDER' + lcI
  laStru[lnIndex, 2] = 'N'
  laStru[lnIndex, 3] = 6
  laStru[lnIndex, 4] = 0
ENDFOR

lnIndex = lnIndex + 1
laStru[lnIndex, 1] = 'TOTREORDER'
laStru[lnIndex, 2] = 'N'
laStru[lnIndex, 3] = 6
laStru[lnIndex, 4] = 0

lnIndex = lnIndex + 1
laStru[lnIndex, 1] = 'CDIVISION'
laStru[lnIndex, 2] = 'C'
laStru[lnIndex, 3] = 6
laStru[lnIndex, 4] = 0


*- Update other array fields 
FOR lnI = 1 TO ALEN(laStru,1)
  STORE .F. TO laStru[lnI,5],laStru[lnI,6]
  FOR lnJ = 7 TO 16
    laStru[lnI,lnJ] = ""
  ENDFOR 
  STORE 0 TO laStru[lnI,17],laStru[lnI,18]  
ENDFOR 

DIMENSION laIndex[1,2]
laIndex[1,1] = 'ORDER+ACCOUNT+CDIVISION+STORE+STYLE'
laIndex[1,2] = 'ORDSTYLE'
=gfCrtTmp(loFormSet.lcReOrdLines, @laStru, @laIndex, loFormSet.lcReOrdLines)
SELECT (loFormSet.lcReOrdLines)
SET ORDER TO ORDSTYLE

ENDPROC
*- End of lfCrTmpFls.


*!*************************************************************
*:Name      : lfAddProp
*:Developer : SABER A.Razek -  (SAB)
*:Date      : 07/26/2012
*:Purpose   : A function to add properties to the object that passed as a parameter
*!*************************************************************
PROCEDURE lfAddProp
PARAMETERS loObj,lcPropName,PropValue
*- check if lcPropname is not just a variable, but a list of variables, then create a loop to walk around
LOCAL lnI,lnLen,lcPropToCreate
lcPropName = lcPropName + ','
lnLen = OCCURS(',',lcPropName)
FOR lnI = 1 TO lnLen
  lcPropToCreate = ALLTRIM(SUBSTR(lcPropName,1,AT(',',lcPropName)-1))
  IF TYPE('loObj.&lcPropToCreate')='U'
    loObj.AddProperty(lcPropToCreate,PropValue)
  ENDIF 
  lcPropName = SUBSTR(lcPropName,AT(',',lcPropName)+1)
ENDFOR 

ENDPROC
*- End of lfAddProp


*:*************************************************************
*: Name      : lfPopPropArray
*: Developer : SABER A.Razek -  (SAB)
*: Date      : 07/26/2012
*: Purpose   : Put all variables in a two diminsional array, one column for the variable name , the other for its initial value.
*:           : In the start of each function that changes the APINVHDR file initialize all the variables.
*:*************************************************************
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


*:**********************************************************************************
*: Name      : lfGetData
*: Developer : SABER A.Razek -  (SAB)
*: Date      : 07/26/2012
*: Purpose   : called from the AfterRowColChange event in the grdApprove on the screen
*:**********************************************************************************
PROCEDURE lfGetData
PARAMETERS loFormSet

*-Clear data from temp files
SELECT (loFormSet.lcReOrdLines)
ZAP

*- Call the "SOREORD" OG
LOCAL lcExpr
DECLARE laOgFxFlt[1,2],laOgVrFlt[1,2]    && define filter arrays to use after the OG is closed
lnRpPrePack = loFormSet.lnRpPrePack
lcRpOrdMode = loFormSet.lcRpOrdMode
lcExpr = gfOpGrid('SOREORD',.T. ,.F. ,.F. ,.T. ,.T.)


*- if cancel clicked , go to Select Mode
IF lcExpr == '.F.'
  loFormSet.ChangeMode('S')
  RETURN
ENDIF

loFormSet.lnRpPrePack = lnRpPrePack
loFormSet.lcRpOrdMode = lcRpOrdMode

* Divide filter expresion to Customer Filter, Style Filter and 852 Active Period Filter
LOCAL lcCustomerFlt, lcStyleFlt, lcPeriodFlt
STORE '' TO lcCustomerFlt, lcStyleFlt, lcPeriodFlt
IF !EMPTY(lcExpr) AND lcExpr <> ".T."
  lnCount = OCCURS(" AND ", lcExpr) + 1
  FOR lcIndx = 1 TO lnCount
    lnStartPos = IIF(lcIndx = 1, 1, AT(" AND ", lcExpr, lcIndx-1))
    lnLength   = IIF(lcIndx <> lnCount, (AT(" AND ", lcExpr, lcIndx) - lnStartPos), (LEN(lcExpr) - lnStartPos))
    lcExpPrt   = ALLTRIM(STRTRAN(SUBSTR(lcExpr, lnStartPos, lnLength), " AND ", ""))
    
    DO CASE
      CASE "CUSTOMER." $ UPPER(lcExpPrt)
        lcCustomerFlt = lcCustomerFlt + IIF(EMPTY(lcCustomerFlt), "", " AND ") + lcExpPrt
        IF "CUSTOMER.ACCOUNT" $ UPPER(lcExpPrt)
          loFormSet.lcAccount = PADR(ALLTRIM(STRTRAN(STRTRAN(STRTRAN(lcExpPrt, "CUSTOMER.ACCOUNT", ""), "=", ""), "'", "")), 5)
        ENDIF 
      CASE "STYLE." $ UPPER(lcExpPrt)
        lcStyleFlt = lcStyleFlt + IIF(EMPTY(lcStyleFlt), "", " AND ") + lcExpPrt
      CASE "PRODACTI.DPRD_END" $ UPPER(lcExpPrt)
        lcPeriodFlt = lcPeriodFlt + IIF(EMPTY(lcPeriodFlt), "", " AND ") + lcExpPrt
    ENDCASE
  ENDFOR 
ENDIF
lcCustomerFlt = IIF(EMPTY(lcCustomerFlt), '.T.', lcCustomerFlt)
lcStyleFlt    = IIF(EMPTY(lcStyleFlt), '.T.', lcStyleFlt)
lcPeriodFlt   = IIF(EMPTY(lcPeriodFlt), '.T.', lcPeriodFlt)



IF !USED('CUSTOMER')
  =gfOpenTable('CUSTOMER','CUSTOMER','SH')  && TYPE+ACCOUNT+STORE
ENDIF
IF !USED('STYLE')
  =gfOpenTable('STYLE','STYLE','SH')  && STYLE
ENDIF

SELECT CUSTOMER
=SEEK('S'+loFormSet.lcAccount)
SCAN REST WHILE TYPE+ACCOUNT+STORE = 'S'+loFormSet.lcAccount FOR &lcCustomerFlt.
  SELECT STYLE
  SCAN FOR &lcStyleFlt.    
    DIMENSION laOpenQty[1,1]
    DIMENSION laOnHandQty[1,1]
    =lfGetOpenQty(CUSTOMER.STORE, STYLE.STYLE, @laOpenQty)
    =lfGetOnHandQty(CUSTOMER.ACCOUNT, CUSTOMER.STORE, STYLE.STYLE, lcPeriodFlt, @laOnHandQty)
    *- Show wait window collecting data
    WAIT WINDOW LANG_COLECT_MSG1 + Customer.Store + LANG_COLECT_MSG2 + Style.Style + " ...!" NOWAIT
    SELECT (loFormSet.lcReOrdLines)
    APPEND BLANK
        lcRplcStat = ""
    lcRplcStat = lcRplcStat + " LSELECT     WITH .F.,"+;
                              " ORDER       WITH '******',"+;
                              " ACCOUNT     WITH CUSTOMER.ACCOUNT,"+;
                              " STORE       WITH CUSTOMER.STORE,"+;
                              " STYLE       WITH STYLE.STYLE,"+;
                              " NMININVQTY  WITH CUSTOMER.NMININVQTY,"+;
                              " NMAXINVQTY  WITH CUSTOMER.NMAXINVQTY"
    REPLACE &lcRplcStat.
    
    *- Open Qty
    lcRplcStat = ""
    FOR lnI = 1 TO 8
      lcI = ALLTRIM(STR(lnI))      
      lcRplcStat = lcRplcStat + " OPEN"+lcI+"    WITH " + ALLTRIM(STR(laOpenQty[1, lnI])) + ","
    ENDFOR    
    lcRplcStat = lcRplcStat + " TOTOPEN    WITH " + ALLTRIM(STR(laOpenQty[1, 9]))
    REPLACE &lcRplcStat.
    
    *- On Hand Qty
    lcRplcStat = ""
    FOR lnI = 1 TO 8
      lcI = ALLTRIM(STR(lnI))
      lcRplcStat = lcRplcStat + " ONHAND"+lcI+"  WITH " + ALLTRIM(STR(laOnHandQty[1, lnI])) + ","
    ENDFOR
    lcRplcStat = lcRplcStat + " TOTONHAND  WITH " + ALLTRIM(STR(laOnHandQty[1, 9]))
    REPLACE &lcRplcStat.
    
    *- Total Available Qty
    lcRplcStat = ""
    FOR lnI = 1 TO 8
      lcI = ALLTRIM(STR(lnI))
      lcRplcStat = lcRplcStat + " AVLBL"+lcI+"   WITH OPEN"+lcI+" + ONHAND"+lcI+","
    ENDFOR
    lcRplcStat = lcRplcStat + " TOTAVLBL   WITH TOTOPEN + TOTONHAND"
    REPLACE &lcRplcStat.
    
    *- Re-Order Qty
    lnScalCnt = 0
    IF gfSeek('S'+Style.Scale, 'SCALE', 'SCALE')  && TYPE+SCALE+PREPAK
      lnScalCnt = Scale.Cnt
    ENDIF    
    lcRplcStat = ""
    FOR lnI = 1 TO 8
      lcI = ALLTRIM(STR(lnI))
      IF lnI <= lnScalCnt        
        lcRplcStat = lcRplcStat + " REORDER"+lcI+" WITH " + ALLTRIM(STR(lfGetReOrdQty(NMININVQTY, NMAXINVQTY, OPEN&lcI. + ONHAND&lcI. , lnRpPrePack))) + ","
      ELSE
        lcRplcStat = lcRplcStat + " REORDER"+lcI+" WITH 0,"
      ENDIF
    ENDFOR
    lcRplcStat = lcRplcStat + " TOTREORDER WITH REORDER1 + REORDER2 + REORDER3 + REORDER4 + REORDER5 + REORDER6 + REORDER7 + REORDER8, "
    
    lcRplcStat = lcRplcStat + " CDIVISION WITH STYLE.CDIVISION"
    
    REPLACE &lcRplcStat.
    
  ENDSCAN
ENDSCAN

*- check if there is any collected lines or not
SELECT (loFormSet.lcReOrdLines)
LOCATE
IF EOF()
  =gfModalGen('TRM00052B40011','ALERT')  
  loFormSet.ChangeMode('S')
ELSE
  *- Go to Edit mode when data is selected
  loFormSet.AriaForm1.AriaMultiSelectionGrid1.grdMultiSelectionGrid.Column1.CurrentControl = 'Ariacheckbox1'  
  loFormset.Ariaform1.cboWarehouse.Value = lfGetDefWareh()
  loFormSet.ChangeMode('E')
ENDIF

ENDPROC
*- End of lfGetData.


*:***********************************************************************************
*: Name      : lfGetCrVal
*: Developer : SABER A.Razek -  (SAB)
*: Date      : 07/26/2012
*: Purpose   : Get the criteria value from the filter arrays
*:***********************************************************************************
FUNCTION lfGetCrVal
PARAMETERS lcFltFld
LOCAL lnPos
lnPos = ASCAN(laOgFxFLt,lcFltFld)
IF lnPos > 0
  lnPOS = ASUBSCRIPT(laOgFxFLt,lnPos,1)
  RETURN laOgFxFLt[lnPos,6]
ENDIF
lnPos = ASCAN(laOgVrFLt,lcFltFld)
IF lnPos > 0
  lnPOS = ASUBSCRIPT(laOgVrFLt,lnPos,1)
  RETURN laOgVrFLt[lnPos,6]
ENDIF

ENDPROC
*- End of lfGetCrVal.


*:***********************************************************************************
*: Name      : lfGetVrPos
*: Developer : SABER A.Razek -  (SAB)
*: Date      : 07/26/2012
*: Purpose   : Get variable position 
*:***********************************************************************************
FUNCTION lfGetVrPos
PARAMETERS lcVrArr,lcFld
LOCAL lnPos
lnPOS = ASCAN(loOgScroll.&lcVrArr.,lcFld) 
IF lnPos > 0
  lnPos = ASUBSCRIPT(loOgScroll.laOGVrFlt,lnPos,1)
ENDIF
RETURN lnPos

ENDFUNC
 *- End of lfGetVrPos.
 

*:***********************************************************************************
*: Name      : lfGetOpenQty
*: Developer : SABER A.Razek -  (SAB)
*: Date      : 07/26/2012
*: Purpose   : Get open Qty from orline for the selected Store/Style
*:***********************************************************************************
PROCEDURE lfGetOpenQty
PARAMETERS lcStore, lcStyle, laOpenQty
LOCAL lcOldAlias
lcOldAlias = SELECT(0)

DIMENSION laOpenQty[1, 9]
FOR lnI = 1 TO 9
  STORE 0 TO laOpenQty[1, lnI]
ENDFOR 

IF !USED('OHDRETMP') AND !USED('OLINETMP')
  =gfOpenTable('ORDHDR','ORDHDR','SH', 'OHDRETMP')  && CORDTYPE+ORDER
  =gfOpenTable('ORDLINE','ORDLINE','SH', 'OLINETMP')  && CORDTYPE+ORDER+STR(LINENO,6)
  SELECT OLINETMP
  SET RELATION TO OLINETMP.CORDTYPE+OLINETMP.ORDER INTO OHDRETMP ADDITIVE
ENDIF

SELECT OLINETMP
SCAN FOR OLINETMP.Store = PADR(lcStore, 8) AND OLINETMP.Style = PADR(lcStyle,19) AND INLIST(OHDRETMP.STATUS, 'H', 'O')
  FOR lnI = 1 TO 8
    lcI = ALLTRIM(STR(lnI))
    laOpenQty[1, lnI] = laOpenQty[1, lnI] + OLINETMP.QTY&lcI
  ENDFOR
  laOpenQty[1, 9] = laOpenQty[1, 9] + OLINETMP.TOTQTY
ENDSCAN 
SELECT(lcOldAlias)

ENDPROC
*- End of lfGetOpenQty.


*:***********************************************************************************
*: Name      : lfGetOnHandQty
*: Developer : SABER A.Razek -  (SAB)
*: Date      : 07/26/2012
*: Purpose   : Get On Hand Qty from ProcActi table during 852 Acitivity Period
*:***********************************************************************************
PROCEDURE lfGetOnHandQty
PARAMETERS lcAccount, lcStore, lcStyle, lcPeriodFlt, laOnHandQty
LOCAL lcOldAlias
lcOldAlias = SELECT(0)

DIMENSION laOnHandQty[1, 9]
FOR lnI = 1 TO 9
  STORE 0 TO laOnHandQty[1, lnI]
ENDFOR 

IF !USED('PRODACTI')
  =gfOpenTable('PRODACTI','ACTIPRD','SH', 'PRODACTI')  && ACCOUNT+STYLE+TRAN_TYPE+STORE+DTOS(DPRD_BGN)
ENDIF

SELECT PRODACTI
=SEEK(lcAccount+lcStyle+'QA'+lcStore)
SCAN REST WHILE ACCOUNT+STYLE+TRAN_TYPE+STORE = lcAccount+lcStyle+'QA'+lcStore&& FOR &lcPeriodFlt.
  FOR lnI = 1 TO 8
    lcI = ALLTRIM(STR(lnI))
    laOnHandQty[1, lnI] = laOnHandQty[1, lnI] + PRODACTI.NACTIQTY&lcI
  ENDFOR
  laOnHandQty[1, 9] = laOnHandQty[1, 9] + PRODACTI.NTOTACTQTY
ENDSCAN 
SELECT(lcOldAlias)

ENDPROC
*- End of lfGetOnHandQty.


*:***********************************************************************************
*: Name      : lfGetReOrdQty
*: Developer : SABER A.Razek -  (SAB)
*: Date      : 07/26/2012
*: Purpose   : Calulate the Re-Order quantity bassed on Min, Max and Available Qty
*:***********************************************************************************
FUNCTION lfGetReOrdQty
LPARAMETERS lnMinQty, lnMaxQty, lnAvlblQty, lnPrePack

LOCAL lnReOrdQty
lnReOrdQty = 0
IF lnAvlblQty < lnMinQty
  DO WHILE lnReOrdQty < lnMaxQty
    lnReOrdQty = lnReOrdQty + lnPrePack
  ENDDO
ENDIF
RETURN lnReOrdQty

ENDFUNC
*- End of lfGetReOrdQty


*****************************************************************
*: Name      : lfFormBeforeSave
*: Developer : SABER A.Razek -  (SAB)
*: Date      : 07/26/2012
*: Purpose   : Check if there is any data to save
*****************************************************************
FUNCTION lfFormBeforeSave
PARAMETERS loFormSet
LOCAL lnRec,lcFlt
SELECT (loFormSet.lcReOrdLines)
lnRec = RECNO()
lcFlt = FILTER()
SET FILTER TO 
LOCATE 
LOCATE FOR LSELECT AND TOTREORDER > 0 
IF !FOUND()
  SET FILTER TO &lcFlt
  GO lnRec
  =gfModalGen('INM00000B00000', .F., .F., .F., LANG_NO_SELECTED_ORDERS)
  RETURN .F.
ENDIF 

 *- End of lfFormBeforeSave.
 
 
*:*************************************************************
*: Name      : lfFormSavefiles
*: Developer : SABER A.Razek -  (SAB)
*: Date      : 07/26/2012
*: Purpose   : Save data
*:*************************************************************
FUNCTION lfFormSavefiles
PARAMETERS loFormSet

DIMENSION laOrderCreated[1]
STORE '' TO  laOrderCreated
DIMENSION loFormSet.laEvntTrig[1]
STORE PADR('SOIMPRO', 10) TO loFormSet.laEvntTrig[1]

LOCAL lcStore, lcCustCurr, lnExRate, lnCurrUnit, lcOrdHdr, lcOrdline, lnLineNo, lcTimes, lcCatSel

lcTimes = gfGetTime()
lcReOrdLines = loFormSet.lcReOrdLines

* Get Default Order Category Code
lcCatSel = ''
=gfOpenTable('CODES','CCODE_NO','SH')   && CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM
IF gfSEEK("D"+"CORDERCAT",'Codes','CCODE_NO')
  lcCatSel = CODES.CCODE_NO
ENDIF

* Get lcCustCurr, lnExRate, lnCurrUnit
=gfSEEK('M'+loFormSet.lcAccount,'CUSTOMER')
lcCustCurr = IIF(EMPTY(CUSTOMER.cCurrCode),oAriaApplication.BaseCurrency,CUSTOMER.cCurrCode)
STORE 1 TO lnExRate, lnCurrUnit
IF lcCustCurr <> oAriaApplication.BaseCurrency
  lnExRate = gfChkRate('lnCurrUnit',lcCustCurr ,oAriaApplication.SystemDate,.T.,.F.,.F.,.F.)
  IF lnExRate = 0
    lcCustCurr = oAriaApplication.BaseCurrency
    STORE 1 TO lnExRate, lnCurrUnit
  ENDIF
ENDIF

IF !USED('STYDYE')
  =gfOpenTable('STYDYE', 'STYDYE', 'SH')   && STYLE+CWARECODE+DYELOT
ENDIF  
  
* Get Rep Commesion
lnComm1 = 0
IF (gfGetMemVar('M_DIRUNPRO',oAriaApplication.ActiveCompanyID)='D')
  IF !USED('REP_DIV')
    =gfOpenTable('REP_DIV','REP_DIV')
  ENDIF   
  IF gfSEEK(CUSTOMER.SalesRep +'DCC','REP_DIV','REP_DIV')
    STORE REP_DIV.Comm_Rate TO lnComm1
  ENDIF   
ENDIF
IF lnComm1 = 0
  lnComm1 = CUSTOMER.Comm
ENDIF 

lcOrdHdr  = gfTempName()
lcOrdline = gfTempName()     

SELECT ORDHDR
=AFIELDS(laFileStru)
=gfCrtTmp(lcOrdHdr,@laFileStru,[cOrdType+ORDER],lcOrdHdr)  

SELECT ORDLINE
=AFIELDS(laFileStru)
=gfCrtTmp(lcOrdLine,@laFileStru,[cOrdType+ORDER],lcOrdline)
  
LOCAL lcKey, lnLineNo, lnTotQty, ldComplete
lcKey      = ''
ldComplete = oAriaApplication.SystemDate + gfGetMemVar('M_COMPDATE', oAriaApplication.ActiveCompanyID)

SELECT DISTINCT IIF(loFormSet.lcRpOrdMode = 'M', ORDER+ACCOUNT+CDIVISION, ORDER+ACCOUNT+CDIVISION+STORE) ;
       FROM (lcReOrdLines) WHERE LSELECT AND TOTREORDER > 0 INTO ARRAY laOrdKeys

FOR lnItem = 1 TO ALEN(laOrdKeys)
  SELECT (lcReOrdLines)
  lcKey = laOrdKeys[lnItem]
  =SEEK(lcKey)
  lnLineNo   = 1
  lnTotQty   = 0

  *- Add Order Header
  SELECT (lcOrdHdr)
  APPEND BLANK 
  m.CDIVISION = &lcReOrdLines..CDIVISION
  m.MULTI     = IIF(loFormSet.lcRpOrdMode = 'M', 'Y', 'N')
  m.SPCINST   = CUSTOMER.SPCINST
  m.ENTERED   = oAriaApplication.SystemDate
  m.START     = oAriaApplication.SystemDate
  m.COMPLETE  = ldComplete
  m.LINK_CODE = 'DEFDEF'
  m.GL_SALES  = 'DEF'
  m.CWARECODE = loFormSet.AriaForm1.cboWareHouse.Value
  m.NEXRATE   = lnExRate
  m.CCURRCODE = lcCustCurr
  m.NCURRUNIT = lnCurrUnit        
  m.SEASON    = "*"
  m.ACCOUNT   = loFormSet.lcAccount
  m.SHIPVIA   = CUSTOMER.SHIPVIA
  m.CORDERCAT = lcCatSel
  m.CCONTREF  = ''
  m.CORDTYPE  = 'O'
  m.STATUS    = 'H'
  m.PRIORITY  = CUSTOMER.Priority
  m.CUSTPO    = ''
  m.CADD_USER = OAriaApplication.User_ID
  m.DADD_DATE = oAriaApplication.SystemDate   
  m.CADD_TIME = lcTimes
  m.STORE     = IIF(loFormSet.lcRpOrdMode = 'M', '', &lcReOrdLines..STORE)
  m.CTERMCODE = CUSTOMER.cTermCode
  m.PRIORITY  = CUSTOMER.Priority
  m.CINSUR    = CUSTOMER.Cinsur
  m.REP1      = CUSTOMER.SalesRep
  m.CREORDER  = 'N'
  m.BULK 	  = 'N'
  m.Comm1     = lnComm1
    
  m.Open      = lnTotQty
  m.Book      = lnTotQty  
  GATHER MEMO MEMVAR 
  
  SELECT (lcReOrdLines)
  SCAN REST WHILE IIF(loFormSet.lcRpOrdMode = 'M', ORDER+ACCOUNT+CDIVISION, ORDER+ACCOUNT+CDIVISION+STORE) = lcKey FOR LSELECT AND TOTREORDER > 0 
    * Add Order Lines
    SELECT (lcOrdLine)
    APPEND BLANK
    m.CORDTYPE  = 'O'
    m.ACCOUNT   = loFormSet.lcAccount
    m.CWARECODE = loFormSet.AriaForm1.cboWareHouse.Value
    m.STORE     = &lcReOrdLines..STORE
    m.LINENO    = lnLineNo
    m.STYLE     = &lcReOrdLines..STYLE
    m.SEASON    = IIF(gfSeek(&lcReOrdLines..STYLE,'STYLE','STYLE'), STYLE.Scale, '')
    m.SCALE     = IIF(gfSeek(&lcReOrdLines..STYLE,'STYLE','STYLE'), STYLE.Season, '')
    m.START     = oAriaApplication.SystemDate
    m.COMPLETE  = ldComplete
    m.FLAG      = 'N'
    m.EMPLOYEE  = ''
    m.CADD_USER = oAriaApplication.User_ID
    m.DADD_DATE = oAriaApplication.SystemDate
    m.CADD_TIME = lcTimes
    m.GL_SALES  = 'DEFDEF'
    m.DESC1     = IIF(gfSeek(&lcReOrdLines..STYLE,'STYLE','STYLE'), STYLE.Desc1, '')
    *-SAB
    m.PRICE     = IIF(gfSeek(&lcReOrdLines..STYLE,'STYLE','STYLE'), STYLE.PriceA, 0)
    *-SAB
    FOR lnI = 1 TO 8
      lcI = ALLTRIM(STR(lnI))
      m.QTY&lcI.  = &lcReOrdLines..REORDER&lcI.
      m.BOOK&lcI. = &lcReOrdLines..REORDER&lcI.
    ENDFOR
    m.TOTQTY  = &lcReOrdLines..TOTREORDER
    m.TOTBOOK = &lcReOrdLines..TOTREORDER    
  
    GATHER MEMVAR MEMO    
    lnLineNo = lnLineNo + 1
    lnTotQty = lnTotQty + &lcReOrdLines..TOTREORDER
  ENDSCAN
  
  REPLACE Open WITH lnTotQty, Book WITH lnTotQty IN (lcOrdHdr)
  
  DO lfSavScr IN (oAriaApplication.ApplicationHome + 'SO\SOUPDATE.FXP') WITH .F., 'A', lcOrdHdr, lcOrdLine, '', '', '', loFormSet
    
  SELECT ORDHDR
  =gfTableUpdate()    
  SELECT ORDLINE
  =gfTableUpdate()
      
  SELECT (lcOrdLine)
  ZAP
  SET ORDER TO (lcOrdLine)
  SELECT (lcOrdHdr)
  ZAP
  SET ORDER TO (lcOrdHdr)
ENDFOR

=lfCustMsg(.T.)

loFormSet.ChangeMode('S')
SELECT (loFormSet.lcReOrdLines)
ZAP
loFormSet.AriaForm1.AriaMultiSelectionGrid1.grdMultiSelectionGrid.Refresh()

ENDFUNC
*- End of lfFormSavefiles

*:*************************************************************
*: Name      : lfvSelect
*: Developer : SABER A.Razek -  (SAB)
*: Date      : 07/26/2012
*: Purpose   : Procedure to validate Selection buttons
*:*************************************************************
PROCEDURE lfvSelect
PARAMETERS lcType, lcTmpFile, lcSelFld, loFormSet
PRIVATE llSelAll 
llSelAll  = .F.
SELECT (lcTmpFile)
lnRecNo = RECNO() 

DO CASE 
  CASE lcType = 'S'
    SELECT (lcTmpFile)
    REPLACE &lcSelFld. WITH !&lcSelFld.
  CASE lcType = 'A'
     llSelAll  = .F.
	 SELECT (lcTmpFile)
     REPLACE  ALL &lcSelFld. WITH .T.
     loFormSet.llSelectAllEna =.F. 
  CASE lcType = 'N'
     llSelAll  = .T.
	 SELECT (lcTmpFile)
     REPLACE  ALL &lcSelFld. WITH .F.
     loFormSet.llSelectnOneEna =.F. 
 
  CASE lcType = 'V'
     llSelAll  = !llSelAll
	 SELECT (lcTmpFile)
     REPLACE  ALL &lcSelFld. WITH !&lcSelFld.
 ENDCASE    
 
 loFormSet.llSelectAllEna = llSelAll
 SELECT (lcTmpFile)
 LOCATE FOR &lcSelFld.
 
 IF FOUND()
   loFormSet.llSelectNoneEna =.T. 
 ELSE
   loFormSet.llSelectNoneEna =.F. 
 ENDIF 
 
 LOCATE FOR !&lcSelFld.
 
 IF FOUND()
   loFormSet.llSelectAllEna = .T.
 ELSE 
   loFormSet.llSelectAllEna = .F.
 ENDIF 
 
 IF BETWEEN(lnRecNo,1,RECCOUNT())
   GO RECORD lnRecNo
 ENDIF

loFormSet.llSelectIvertEna =.T.
loFormSet.lcSelectCap = IIF(&lcSelFld., LANG_UNSELECT, LANG_SELECT)

ENDPROC
*- End of lfvSelect

*:*************************************************************
*: Name      : lfGetScale
*: Developer : SABER A.Razek -  (SAB)
*: Date      : 07/26/2012
*: Purpose   : Get Style Scale
*:*************************************************************
FUNCTION lfGetScale
LPARAMETERS lcStyle

LOCAL lnOldAlias, lcScale
lnOldAlias = SELECT()
IF !USED('STYLETMP')
  =gfOpenTable('STYLE','STYLE','SH', 'STYLETMP')  && STYLE
ENDIF
SELECT STYLETMP
IF SEEK(lcStyle)
  lcScale = STYLE.Scale
ELSE
  lcScale = ''
ENDIF

SELECT (lnOldAlias)
RETURN lcScale

ENDFUNC
*- End of lfGetScale


*:*************************************************************
*: Name      : lfGetDefWareh
*: Developer : SABER A.Razek -  (SAB)
*: Date      : 07/26/2012
*: Purpose   : Get Default Warehouse
*:*************************************************************
FUNCTION lfGetDefWareh
LOCAL lcDefWareh
lcDefWareh = ''
IF !USED('WAREHOUS')
  =gfOpenTable('WAREHOUS','WAREHOUS','SH')  && CWARECODE
ENDIF

SELECT cWareCode FROM WAREHOUS WHERE lDefWare INTO  ARRAY laResult
lcDefWareh = laResult[1]

RETURN lcDefWareh

ENDFUNC
*End of lfGetDefWareh



*:**************************************************************************
*: Name        : lfCustMsg  
*: Developer   : SABER A.Razek -  (SAB)
*: Date        : 07/26/2012
*: Purpose     : display so saving cust msf
*: Called From : SOUPDATE  
*:***************************************************************************
FUNCTION lfCustMsg
LPARAMETERS llShowMsg


IF TYPE('lcOrderNo') = 'C'
  IF EMPTY(laOrderCreated[1])
    laOrderCreated[1]= lcOrderNo
  ELSE
    DIMENSION laOrderCreated[ALEN(laOrderCreated,1)+1]
    laOrderCreated[ALEN(laOrderCreated,1)]= lcOrderNo
  ENDIF 
ENDIF
  
IF llShowMsg
  IF ALEN(laOrderCreated,1) > 1
    lcMsg = "Sales Orders from  " + laOrderCreated[1] + " to " + laOrderCreated[ALEN(laOrderCreated,1)] + "  have been Created. Do you like to view the generated orders ?"
  ELSE
    lcMsg = "Sales Order  " + laOrderCreated[1] + "  has been Created. Do you like to view the generated order ?"
  ENDIF
  
  IF gfModalGen('QRM00000B42002',.F.,.F.,.F.,lcMsg) = 1 
    =lfCallSalesOrd('O', laOrderCreated[1])
  ENDIF
ENDIF

*- End of lfCustMsg