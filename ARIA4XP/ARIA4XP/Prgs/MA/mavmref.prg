*:**********************************************************************************************
*: Program file  : MAVMREF.PRG 
*: Program desc. : Vendor Item\Color screen
*: System        : Aria 4 XP
*: Module        : MA
*: Developer     : Wael M. Abo-Shawareb (WSH)
*: Date          : 04/03/2006
*: Refer to      : N037655
*:**********************************************************************************************
*: Modifications:
*: B608396,1 MMT 12/31/2007 Convert VENDMATH,VENDMATL tables to SQL[T20070117.0002]
*: B608918,1 MMT 07/05/2009 Fix bug of not updatng DB if Item Itself changed [T20081006.0008]
*! B609236,1 MMT 05/05/2010 material vendor ref. does not accept same sup. code   even for same item major[T20100422.0006]
*! B609236,2 MMT 05/10/2010 When one more item added to Vendor in Same Session "Error while updating" Error[T20100422.0006]
*! C201577,1 MMT 05/13/2013 Add trigger to allow user to add multiple Vendor reference to the same item[T20130312.0031]
*! E303387,1 MMT 05/13/2013 Call the Material Vendor reference screen from material PO screen options menu[T20130312.0031]
*:**********************************************************************************************
*! E303387,1 MMT 05/13/2013 Call the Material Vendor reference screen from material PO screen options menu[Start]
LPARAMETERS lcVendCodeVal,lcFabricVal
*! E303387,1 MMT 05/13/2013 Call the Material Vendor reference screen from material PO screen options menu[End]
#INCLUDE R:\Aria4XP\Prgs\MA\MAVMREF.H

PRIVATE lcAllVar
lcAllVar = 'lcTmpFile,lcPrCur,lcItemTitle,lnMajLen,' +;
           'lcItemSep,lNewLine,llVenRef,lnFabLenInMATL'

DIMENSION laAllVar[1,1]
STORE '' TO laAllVar
=gfSubStr(lcAllVar,@laAllVar,',')

LOCAL lnI
FOR lnI = 1 TO ALEN(laAllVar,1)
  PRIVATE &laAllVar[lnI,1].
ENDFOR

*--Initialize needed variables
STORE ' ' TO lcTmpFile, lcPrCur, lcItemTitle, lcItemSep
STORE 0   TO lnMajLen, lnFabLenInMATL
STORE .F. TO lNewLine, llVenRef

*--Does the company use "Vendor Material Reference"?
llVenRef  = ALLTRIM(gfGetMemVar('M_VENREF')) = 'Y'
IF !llVenRef
  =gfModalGen('INM36188B36000', 'DIALOG', oAriaApplication.ActiveCompanyId)
  *wael
  RETURN
  *wael

ENDIF
*! E303387,1 MMT 05/13/2013 Call the Material Vendor reference screen from material PO screen options menu[Start]
*DO FORM (oAriaApplication.ScreenHome +  'MA\MAVMREF')
DO FORM (oAriaApplication.ScreenHome +  'MA\MAVMREF') WITH lcVendCodeVal,lcFabricVal
*! E303387,1 MMT 05/13/2013 Call the Material Vendor reference screen from material PO screen options menu[End]
*!*************************************************************
*! Name      : lfFormInit
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/03/2006
*! Purpose   : Initialize Screen Properties.
*!*************************************************************
*! Example   : =lfFormInit(ThisFormSet)
*!*************************************************************
FUNCTION lfFormInit
LPARAMETERS loFormSet

=lfAddPro(loFormSet)




WITH loFormSet
  LOCAL lcCursPos
  lcCursPos = gfGetRemoteTable(SET("Datasession"), 'VENDMATH')
  
  .lnFabLenInMATL = IIF(oAriaApplication.laRemoteTable[lcCursPos].llNative, 7, 19)
  .lcItemTitle    = gfItemMask('HI', '', '0002')
  .lnMajLen       = LEN(gfItemMask('PM', '', '0002'))
  .lcItemSep      = SUBSTR(gfItemMask('PI', '', '0002'), .lnMajLen + 1, 1)
  
  *--Assign SQL Browse Properties
  lcCursPos = gfGetRemoteTable(SET("Datasession"), 'VENDMATH')
  
  .cBrowseTableDBEngine   = IIF(oAriaApplication.laRemoteTable[lcCursPos].llNative, 'FOX', 'SQL')
  .cBrowseAliasName       = 'VENDMATH'
  .cBrowseTableName       = 'VENDMATH'
  .cBrowseIndexName       = 'VENDMATH'
  .cBrowseIndexExpression = 'VENDOR'
  .cBrowseIndexFields     = 'VENDOR'
  .cBrowseKey             = ''
  .BrowseTitle            = LANG_MAVMREF_APVENDORS
  
  *--Set value for FormSet Master File and Data Environment Initial Selected Cursor
  .DataEnvironment.InitialSelectedAlias = 'VENDMATH'
  
  *--Create the Temp Grid Lines Cursor
  .lcTmpFile = gfTempName()
  
  =lfCrtTempLine(loFormSet)
  
  *--Adjust Grid Columns
  =lfAdjustGrid(loFormSet)
ENDWITH

*!*************************************************************
*! Name      : lpShow
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/03/2006
*! Purpose   : to control the show of all objects on the screen with all modes
*!*************************************************************
*! Example   : =lpShow(ThisFormSet)
*!*************************************************************
PROCEDURE lpShow
LPARAMETERS loFormSet

WITH loFormSet
  DO CASE
    CASE .ActiveMode = 'S'
      SELECT (.lcTmpFile)
      =TABLEREVERT(.T.)
      =CURSORSETPROP("Buffering", 3)
      ZAP
      =CURSORSETPROP("Buffering", 5)
      
      .AriaForm1.kbVendor.Enabled = .T.
      
      .lNewLine = .F.
      .AriaForm1.txtVendName.Value = ""
      
      =lfAdjustGrid(loFormSet)
      =lfRefreshLine(loFormSet)
      
      .AriaForm1.kbVendor.KeyTextBox.SetFocus()
      
    CASE .ActiveMode $ 'VE'
      SELECT (.lcTmpFile)
      =TABLEREVERT(.T.)
      =CURSORSETPROP("Buffering", 3)
      ZAP
      =CURSORSETPROP("Buffering", 5)
      
      IF !lfGetLines(loFormSet)
        RETURN .ChangeMode("A")
      ENDIF
      
      .AriaForm1.kbVendor.Enabled = .F.
      
      .lNewLine = .F.
      .AriaForm1.txtVendName.Value = APVENDOR.cVenComp
      
      =lfAdjustGrid(loFormSet)
      =lfRefreshLine(loFormSet)
      
      IF .ActiveMode $ 'E'
        .AriaForm1.grdLines.SetFocus()
      ENDIF
      
    CASE .ActiveMode = 'A'
      .AriaForm1.kbVendor.Enabled = .F.
      
      .lNewLine = .T.
      .AriaForm1.txtVendName.Value = APVENDOR.cVenComp
      
      =lfRefreshLine(loFormSet)
  ENDCASE
ENDWITH

*!*************************************************************
*! Name      : lfRefreshLine
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/03/2006
*! Purpose   : refresh the fields when changing the record in the browse
*!*************************************************************
*! Example   : =lfRefreshLine()
*!*************************************************************
FUNCTION lfRefreshLine
LPARAMETERS loFormSet

LOCAL lnAlias, lcItemVal
lnAlias   = SELECT(0)
lcItemVal = loFormSet.AriaForm1.kbItem.Value

WITH loFormSet.AriaForm1
  SELECT (loFormSet.lcTmpFile)
  
  .kbItem.Value       = IIF(loFormSet.lNewLine, "", Item)
  .txtSuppCode.Value  = IIF(loFormSet.lNewLine, "", cVenFab)
  .txtSuppColor.Value = IIF(loFormSet.lNewLine, "", cVenColr)
  .txtPrice.Value     = IIF(loFormSet.lNewLine, 0, nFabCost)
  .txtQuantity.Value  = IIF(loFormSet.lNewLine, 0, nFabTotQty)
  .txtLeadTime.Value  = IIF(loFormSet.lNewLine, 0, LeadTime)
  .txtItemDesc.Value  = IIF(loFormSet.lNewLine, "", cItemDesc)
  .txtColorDesc.Value = IIF(loFormSet.lNewLine, "", cColorDesc)
  
  .lblCurrCode.Caption = loFormSet.lcPrCur
  
  .kbItem.Enabled       = loFormSet.ActiveMode $ 'AE'
  .txtSuppCode.Enabled  = loFormSet.ActiveMode $ 'AE' AND !loFormSet.lNewLine AND !EOF()
  .txtSuppColor.Enabled = loFormSet.ActiveMode $ 'AE' AND !loFormSet.lNewLine AND !EOF()
  .txtPrice.Enabled     = loFormSet.ActiveMode $ 'AE' AND !loFormSet.lNewLine AND !EOF()
  .txtQuantity.Enabled  = loFormSet.ActiveMode $ 'AE' AND !loFormSet.lNewLine AND !EOF()
  .txtLeadTime.Enabled  = loFormSet.ActiveMode $ 'AE' AND !loFormSet.lNewLine AND !EOF()
  .cmdNew.Enabled       = loFormSet.ActiveMode $ 'AE'
  .cmdRemove.Enabled    = loFormSet.ActiveMode $ 'AE' AND !EOF()
ENDWITH

SELECT(lnAlias)
RETURN

*!**************************************************************************
*! Name      : lfGetLines
*! Developer : Wael M. ABo-Shawareb (WSH)
*! Date      : 04/03/2006
*! Purpose   : Collect Data
*!**************************************************************************
*! Example   : =lfGetLines()
*!**************************************************************************
FUNCTION lfGetLines
LPARAMETERS loFormSet

LOCAL lnAlias, lcVenCode, llUpdateItm
lnAlias     = SELECT(0)
lcVenCode   = PADR(loFormSet.AriaForm1.kbVendor.KeyTextBox.Value, 8)
llUpdateItm = .F.

IF !EMPTY(lcVenCode) AND gfSEEK(lcVenCode,'VENDMATL')
  SELECT APVendor
  =gfSeek(lcVenCode, "APVENDOR", "VENCODE")
  
  SELECT VENDMATL
  SCAN REST WHILE vendor+fabric+color+cvenfab+cvencolr = lcVenCode
    
    m.Item = VENDMATL.Item
    IF EMPTY(m.Item)
      m.Item = PADR(VENDMATL.FABRIC, loFormSet.lnMajLen) + loFormSet.lcItemSep + RTRIM(VENDMATL.Color)
      
      SELECT VENDMATL
      =gfReplace("Item WITH m.Item")
      llUpdateItm = .T.
    ENDIF
    
    =gfSEEK('0002' + m.Item, 'FABRIC', 'Style', .T.)
    
    SELECT VENDMATL
    SCATTER MEMVAR MEMO
    
    m.OldFbCl    = PADR(m.FABRIC, loFormSet.lnMajLen) + loFormSet.lcItemSep + m.Color
    m.cItemDesc  = FABRIC.DESC
    m.cColorDesc = gfCodDes(m.Color, 'COLOR')
    
    SELECT (loFormSet.lcTmpFile)
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDSCAN
  
  IF llUpdateItm
    SELECT VENDMATL
    =gfTableUpdate(.T.)
  ENDIF
  
  *--Always get the last record for the current vendor for the navigation button to work properly.
  IF !gfSEEK(lcVenCode, 'VENDMATH')
    m.Vendor   = lcVenCode
    m.cVenComp = APVENDOR.cVenComp
    
    SELECT VENDMATH
    gfAppend()
    gfREPLACE("Vendor WITH m.Vendor, cVenComp with m.cVenComp")
    
    =gfAdd_info('VENDMATH', loFormSet)
    gfReplace()
  ENDIF
  
  SELECT (loFormSet.lcTmpFile)
  =TABLEUPDATE(.T.)
  =SEEK(lcVenCode)
  
  SELECT (lnAlias)
  RETURN .T.
ELSE
  SELECT (lnAlias)
  RETURN .F.
ENDIF

*!**************************************************************************
*! Name      : lfvVendor
*! Developer : Wael M. ABo-Shawareb (WSH)
*! Date      : 04/03/2006
*! Purpose   : Validating the vendor code
*!**************************************************************************
*! Example   : =lfvVendor()
*!**************************************************************************
FUNCTION lfvVendor
LPARAMETERS loFormSet, llFromBrowse

LOCAL lnAlias, lcValue, loVendObj
lnAlias   = SELECT(0)
loVendObj = loFormSet.AriaForm1.kbVendor
lcValue   = PADR(loVendObj.KeyTextbox.Value, 8)

IF !EMPTY(lcValue) AND !llFromBrowse
  llFromBrowse = .T.
  
  SELECT APVendor
  IF gfSeek(lcValue, "APVENDOR", "VENCODE") AND 'M' $ APVENDOR.CVENSUPTYP
    llFromBrowse = .F.
  ENDIF
ENDIF

IF llFromBrowse AND gfAPVnBrow(@lcValue, .T., 'M')
  loVendObj.KeyTextBox.Value = lcValue
ENDIF

IF !EMPTY(lcValue)
  =gfSEEK(lcValue, 'ApVendor')
  loFormSet.lcPrCur = ApVendor.cCurrCode
  
  IF gfSEEK(lcValue, 'VENDMATH')
    loFormSet.ChangeMode('V')
  ELSE
    loFormSet.ChangeMode('A')
  ENDIF
ENDIF

SELECT (lnAlias)
RETURN !EMPTY(lcValue)

*!*********************************************************************************************
*! Name      : lfvItem
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/03/2006
*! Purpose   : validate the Item field
*!*********************************************************************************************
*! Example   : =lfvItem()
*!*********************************************************************************************
FUNCTION lfvItem
LPARAMETERS loFormSet

LOCAL lnAlias, lcItemVal, lcVenCode, lnOldRec, lcOldItem
lnAlias   = SELECT(0)
lcItemVal = loFormSet.AriaForm1.kbItem.Value
lcVenCode = loFormSet.AriaForm1.kbVendor.KeyTextBox.Value
lcOldItem = EVALUATE(loFormSet.lcTmpFile + '.OldFbCl')

*--If modifying a line, check if the original Fabric\Color has POs
IF !loFormSet.lNewLine AND !EMPTY(lcOldItem) AND lfChkPos(loFormSet, lcOldItem)
  =gfModalGen('INM36190B36000', 'DIALOG', ALLTRIM(lcVenCode) + LANG_MAVMREF_NOMODIFY)
  
  =lfRefreshLine(loFormSet)
  SELECT (lnAlias)
  RETURN .F.
ENDIF

IF !EMPTY(lcItemVal)
  =gfSEEK('0002' + lcItemVal, 'FABRIC', 'Style')
  
  IF FABRIC.CSTYGRADE <> '1' 
    =gfModalGen('INM36192B36000', 'DIALOG', ALLTRIM(lcVenCode) + LANG_MAVMREF_NOMODIFY)
    
    IF !loFormSet.lNewLine
      =lfRefreshLine(loFormSet)
    ENDIF
    
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
  
  IF Fabric.Make .AND. gfModalGen('INM36009B36000', 'DIALOG') = 1
    loFormSet.lNewLine = .F.
    =lfRefreshLine(loFormSet)
    SELECT (lnAlias)
    RETURN
  ENDIF
  
  lnOldRec = IIF(EOF(loFormSet.lcTmpFile), 0, RECNO(loFormSet.lcTmpFile))
  
  
  
  *: B608396,1 MMT 12/31/2007 Convert VENDMATH,VENDMATL tables to SQL[Start]
  *IF SEEK(lcVenCode+lcItemVal, loFormSet.lcTmpFile)
  IF SEEK(PADR(lcVenCode,8)+PADR(lcItemVal,19), loFormSet.lcTmpFile)
  *: B608396,1 MMT 12/31/2007 Convert VENDMATH,VENDMATL tables to SQL[End]
    *! C201577,1 MMT 05/13/2013 Add trigger to allow user to add multiple Vendor reference to the same item[Start]
    IF ASCAN(loFormSet.laEvntTrig , PADR('CHNGKEYV',10)) = 0
    *! C201577,1 MMT 05/13/2013 Add trigger to allow user to add multiple Vendor reference to the same item[end]
      *--This Item/Color already exists for the same vendor ð.
      =gfModalGen('INM36186B36000', 'DIALOG', ALLTRIM(lcVenCode))
      loFormSet.lNewLine = .F.
      =lfRefreshLine(loFormSet)
      SELECT(lnAlias)
      RETURN
    *! C201577,1 MMT 05/13/2013 Add trigger to allow user to add multiple Vendor reference to the same item[Start]
    ENDIF
    *! C201577,1 MMT 05/13/2013 Add trigger to allow user to add multiple Vendor reference to the same item[End]
  ENDIF
  
  IF !EMPTY(lnOldRec)
    GOTO lnOldRec IN (loFormSet.lcTmpFile)
  ENDIF
  
  LOCAL lcClrDesc, lcFabric, lcColor, lnPrice
  lcFabric  = SUBSTR(lcItemVal, 1, loFormSet.lnMajLen)
  lcColor   = SUBSTR(lcItemVal, loFormSet.lnMajLen + 2)
  lcClrDesc = gfCodDes(lcColor, 'COLOR')
  lnPrice   = IIF(APVendor.cCurrCode = FABRIC.cPriceCur, FABRIC.nICost1, 0)
  *! B609236,2 MMT 05/10/2010 When one more item added to Vendor in Same Session "Error while updating" Error[Start]
  *IF !gfSEEK(lcVenCode, 'VENDMATH')
  lcVenHdCursPos = gfGetRemoteTable(SET("Datasession"), 'VENDMATH')
  lcVenHDCursUpd = oAriaApplication.laRemoteTable[lcVenHdCursPos].lcCursorUpdate
  IF !SEEK(lcVenCode, lcVenHDCursUpd) AND !gfSEEK(lcVenCode, 'VENDMATH')  
  *! B609236,2 MMT 05/10/2010 When one more item added to Vendor in Same Session "Error while updating" Error[End]
    m.Vendor   = lcVenCode
    m.cVenComp = APVENDOR.cVenComp
    
    SELECT VENDMATH
    gfAppend()
    gfREPLACE("Vendor WITH m.Vendor, cVenComp with m.cVenComp")
    
    =gfAdd_info('VENDMATH', loFormSet)
    gfReplace()
  ENDIF
  
  SELECT (loFormSet.lcTmpFile)
  IF loFormSet.lNewLine
    APPEND BLANK
    REPLACE nFabTotQty WITH 1
  *: B608918,1 MMT 07/05/2009 Fix bug of not updatng DB if Item Itself changed [Start]
  ELSE
    SCATTER MEMO MEMVAR 
    DELETE 
    APPEND BLANK 
    m.OldFbCl = lcItemVal  
    GATHER MEMO MEMVAR  
  *: B608918,1 MMT 07/05/2009 Fix bug of not updatng DB if Item Itself changed [End]  
  ENDIF
  
  REPLACE VENDOR     WITH VENDMATH.Vendor ,;
          Item       WITH lcItemVal  ,;
          cItemDesc  WITH FABRIC.DESC,;
          cColorDesc WITH lcClrDesc  ,;
          FABRIC     WITH lcFabric   ,;
          COLOR      WITH lcColor    ,;
          nFabCost   WITH lnPrice    ,;
          LeadTime   WITH Fabric.LeadTime
  
  loFormSet.lNewLine = .F.
  
  loFormSet.AriaForm1.grdLines.Refresh()
  =lfRefreshLine(loFormSet)
ENDIF

SELECT(lnAlias)
RETURN .T.

*!**********************************************************************************************
*! Name       : lfvVenFab
*! Developer  : Wael M. Abo-Shawareb (WSH)
*! Date       : 04/03/2006
*! Purpose    : Validte Vendor Item
*!**********************************************************************************************
*! Example    : =lfvVenFab()
*!**********************************************************************************************
FUNCTION lfvVenFab
LPARAMETERS loFormSet

LOCAL lnAlias, lnRecNo, lcVenFab, lcOldOrd, lcVenCode
lnAlias  = SELECT(0)
lcVenFab = loFormSet.AriaForm1.txtSuppCode.Value
*! B609236,1 MMT 05/05/2010 material vendor ref. does not accept same sup. code   even for same item major[Start]
lcCurrItemMaj = SUBSTR(loFormSet.AriaForm1.kbItem.Value, 1, loFormSet.lnMajLen)
*! B609236,1 MMT 05/05/2010 material vendor ref. does not accept same sup. code   even for same item major[End]
SELECT (loFormSet.lcTmpFile)
lnRecNo   = IIF(EOF(), 0, RECNO())
lcOldOrd  = ORDER()
lcItem    = Item
lcVenCode = Vendor

SET ORDER TO VenMat

IF !EMPTY(lcVenFab) AND SEEK(lcVenCode+lcVenFab)
  *! B609236,1 MMT 05/05/2010 material vendor ref. does not accept same sup. code   even for same item major[Start]
  *LOCATE REST WHILE Vendor+cVenFab+cVenColr+Item = lcVenCode+lcVenFab FOR RECNO() <> lnRecNo  
  LOCATE REST WHILE Vendor+cVenFab+cVenColr+Item = lcVenCode+lcVenFab FOR RECNO() <> lnRecNo AND SUBSTR(Item , 1, loFormSet.lnMajLen) <> lcCurrItemMaj 
  *! B609236,1 MMT 05/05/2010 material vendor ref. does not accept same sup. code   even for same item major[End]
  IF FOUND()
    =gfModalGen('INM36189B36000', 'DIALOG', ' : ' + ALLTRIM(Fabric))
    
    SET ORDER TO (lcOldOrd)
    IF !EMPTY(lnRecNo)
      GOTO lnRecNo
    ENDIF
    
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
ENDIF  

SET ORDER TO (lcOldOrd)
IF !EMPTY(lnRecNo)
  GOTO lnRecNo
ENDIF

*! C201577,1 MMT 05/13/2013 Add trigger to allow user to add multiple Vendor reference to the same item[Start]
IF ASCAN(loFormSet.laEvntTrig , PADR('UPVENOLDF',10)) <> 0
  loFormSet.mDoTrigger(PADR('UPVENOLDF',10))
ENDIF
*! C201577,1 MMT 05/13/2013 Add trigger to allow user to add multiple Vendor reference to the same item[End]


REPLACE cvenfab WITH lcVenFab

loFormSet.AriaForm1.grdLines.Refresh()
=lfRefreshLine(loFormSet)

SELECT (lnAlias)
RETURN .T.

*!**********************************************************************************************
*! Name       : lfvVenCol
*! Developer  : Wael M. Abo-Shawareb (WSH)
*! Date       : 04/03/2006
*! Purpose    : Valid function for Supplier Color Field
*!**********************************************************************************************
*! Example    : =lfvVenCol()
*!**********************************************************************************************
FUNCTION lfvVenCol
LPARAMETERS loFormSet

LOCAL lnAlias, lnRecNo, lcVenCol, lcOldOrd, cVenFab, lcVenCode
lnAlias  = SELECT(0)
lcVenCol = loFormSet.AriaForm1.txtSuppColor.Value

SELECT (loFormSet.lcTmpFile)
lnRecNo   = IIF(EOF(), 0, RECNO())
lcOldOrd  = ORDER()
lcItem    = Item
lcVenFab  = cVenFab
lcVenCode = Vendor

SET ORDER TO VenMat

IF !EMPTY(lcVenFab) AND !EMPTY(lcVenCol) AND SEEK(lcVenCode+lcVenFab+lcVenCol)
  LOCATE REST WHILE Vendor+cVenFab+cVenColr+Item = lcVenCode+lcVenFab+lcVenCol FOR RECNO() <> lnRecNo
  IF FOUND()
    =gfModalGen('INM36189B36000', 'DIALOG', ' : ' + ALLTRIM(Fabric))
    
    SET ORDER TO (lcOldOrd)
    IF !EMPTY(lnRecNo)
      GOTO lnRecNo
    ENDIF
    
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
ENDIF  

SET ORDER TO (lcOldOrd)
IF !EMPTY(lnRecNo)
  GOTO lnRecNo
ENDIF

*! C201577,1 MMT 05/13/2013 Add trigger to allow user to add multiple Vendor reference to the same item[Start]
IF ASCAN(loFormSet.laEvntTrig , PADR('UPVENOLDC',10)) <> 0
  loFormSet.mDoTrigger(PADR('UPVENOLDC',10))
ENDIF
*! C201577,1 MMT 05/13/2013 Add trigger to allow user to add multiple Vendor reference to the same item[End]

REPLACE cvencolr WITH lcVenCol

loFormSet.AriaForm1.grdLines.Refresh()
=lfRefreshLine(loFormSet)

SELECT (lnAlias)
RETURN .T.

*!**********************************************************************************************
*! Name       : lfvPrice
*! Developer  : Wael M. Abo-Shawareb (WSH)
*! Date       : 04/04/2006
*! Purpose    : Validte Price
*!**********************************************************************************************
*! Example    : =lfvPrice()
*!**********************************************************************************************
FUNCTION lfvPrice
LPARAMETERS loFormSet

LOCAL lnPrice
lnPrice = loFormSet.AriaForm1.txtPrice.Value

IF lnPrice < 0
  =gfModalGen('INM36014B36000', 'DIALOG', LANG_MAVMREF_PRICE)
  RETURN .F.
ENDIF  

REPLACE nFabCost WITH lnPrice IN (loFormSet.lcTmpFile)

loFormSet.AriaForm1.grdLines.Refresh()
=lfRefreshLine(loFormSet)

RETURN .T.

*!**********************************************************************************************
*! Name       : lfvQty
*! Developer  : Wael M. Abo-Shawareb (WSH)
*! Date       : 04/04/2006
*! Purpose    : Validate the Qty 
*!**********************************************************************************************
*! Example    : =lfvQty()
*!**********************************************************************************************
FUNCTION lfvQty
LPARAMETERS loFormSet

LOCAL lnQty
lnQty = loFormSet.AriaForm1.txtQuantity.Value

IF lnQty <= 0
  =gfModalGen('INM36014B36000', 'DIALOG', LANG_MAVMREF_QUANTITY)
  RETURN .F.
ENDIF

REPLACE nFabTotQty WITH lnQty IN (loFormSet.lcTmpFile)

loFormSet.AriaForm1.grdLines.Refresh()
=lfRefreshLine(loFormSet)

RETURN .T.

*!**********************************************************************************************
*! Name       : lfvLead
*! Developer  : Wael M. Abo-Shawareb (WSH)
*! Date       : 04/04/2006
*! Purpose    : Validate the Lead time
*!**********************************************************************************************
*! Example    : =lfvLead()
*!**********************************************************************************************
FUNCTION lfvLead
LPARAMETERS loFormSet

LOCAL lnAlias, lnLeadTim
lnAlias   = SELECT(0)
lnLeadTim = loFormSet.AriaForm1.txtLeadTime.Value

SELECT (loFormSet.lcTmpFile)
REPLACE leadtime WITH lnLeadTim 

loFormSet.AriaForm1.grdLines.Refresh()
=lfRefreshLine(loFormSet)

IF loFormSet.lAddAfterLead
  SELECT FABRIC
  
  IF gfGoNext() AND lfvNew(loFormSet)
    loFormSet.AriaForm1.kbItem.Value = Fabric.Style
  ENDIF
ENDIF

SELECT (lnAlias)
RETURN .T.

*!**********************************************************************************************
*! Name       : lfvNew
*! Developer  : Wael M. Abo-Shawareb (WSH)
*! Date       : 04/03/2006
*! Purpose    : Validate the New button
*!**********************************************************************************************
*! Example    : =lfvNew()
*!**********************************************************************************************
FUNCTION lfvNew
LPARAMETERS loFormSet

loFormSet.lNewLine = .T.
=lfRefreshLine(loFormSet)

RETURN

*!**********************************************************************************************
*! Name       : lfvRemLine
*! Developer  : Wael M. Abo-Shawareb (WSH)
*! Date       : 04/04/2006
*! Purpose    : Validate the Remove button
*!**********************************************************************************************
*! Example    : =lfvRemLine()
*!**********************************************************************************************
FUNCTION lfvRemLine
LPARAMETERS loFormSet

LOCAL lnAlias, lcVenCode
lnAlias   = SELECT(0)
lcVenCode = loFormSet.AriaForm1.kbVendor.KeyTextBox.Value

SELECT (loFormSet.lcTmpFile)
IF EOF()
  SELECT (lnAlias)
  RETURN .F.
ENDIF

IF !loFormSet.lNewLine AND !EMPTY(Item) AND Item == OldFbCl AND lfChkPos(loFormSet, Item)
  =gfModalGen('INM36190B36000', 'DIALOG', ALLTRIM(lcVenCode) + LANG_MAVMREF_NOREMOVE)
  SELECT (lnAlias)
  RETURN .F.
ENDIF

IF !loFormSet.lNewLine
  *-- Are you sure you want to delete this Item/Color?
  IF gfModalGen('INM36017B36001','DIALOG') = 1
    DELETE
    LOCATE
  ELSE
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
ENDIF

loFormSet.lNewLine = .F.

=lfRefreshLine(loFormSet)
loFormSet.AriaForm1.grdLines.SetFocus()

IF EOF(loFormSet.lcTmpFile)
  loFormSet.AriaForm1.cmdNew.Click()
ENDIF

SELECT(lnAlias)
RETURN .T.

*!*************************************************************
*! Name      : lfBeforeDelete
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/04/2006
*! Purpose   : Before Delete Event
*!*************************************************************
*! Example   : =lfBeforeDelete()
*!*************************************************************
FUNCTION lfBeforeDelete
LPARAMETERS loFormSet

LOCAL lnAlias, llDelete
lnAlias  = SELECT(0)
llDelete = .T.

PRIVATE lcVenCode
lcVenCode = PADR(loFormSet.AriaForm1.kbVendor.KeyTextBox.Value, 8)

SELECT VENDMATL
IF gfSeek(lcVenCode)
  *--Check if Fabric has POs
  SCAN REST WHILE VENDOR+FABRIC+COLOR+CVENFAB+CVENCOLR = lcVenCode
    IF lfChkPos(loFormSet, Item)
      =gfModalGen('INM36191B36000', 'DIALOG', ALLTRIM(lcVenCode))
      llDelete = .F.
      EXIT
    ENDIF
  ENDSCAN
ENDIF

SELECT (lnAlias)
RETURN llDelete

*!*************************************************************
*! Name      : lfBeforeSave
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/04/2006
*! Purpose   : Before Save Event
*!*************************************************************
*! Example   : =lfBeforeSave()
*!*************************************************************
FUNCTION lfBeforeSave
LPARAMETERS loFormSet

LOCAL lnAlias, lcVenCode, lnOldRec
lnAlias   = SELECT(0)
lcVenCode = PADR(loFormSet.AriaForm1.kbVendor.KeyTextBox.Value, 8)
lnOldRec  = IIF(EOF(loFormSet.lcTmpFile), 0, RECNO(loFormSet.lcTmpFile))

SELECT (loFormSet.lcTmpFile)
LOCATE

IF EOF()
  *--You cannot save this record with no details.
  =gfModalGen('INM36015B36000', 'DIALOG', lcVenCode)
  
  SELECT (lnAlias)
  RETURN .F.
ENDIF

*--Check if any Item\Color doesn't have correspondent Vendor Item\Color
SELECT (loFormSet.lcTmpFile)
SET ORDER TO VenMat

=SEEK(lcVenCode)
LOCATE REST WHILE vendor+cvenfab+cvencolr+fabric+color = lcVenCode FOR EMPTY(cvenfab) OR EMPTY(cvenColr)
IF FOUND()
  *--One or more  Items/Colors don't have any Vendor Items\Colors assigned to them. Cannot proceed.
  =gfModalGen('INM36187B36000', 'DIALOG', lcVenCode)
  
  SELECT (loFormSet.lcTmpFile)
  SET ORDER TO MatCol
  
  IF !EMPTY(lnOldRec)
    GOTO lnOldRec
  ENDIF
  
  SELECT (lnAlias)
  RETURN .F.
ENDIF

IF !EMPTY(lnOldRec)
  GOTO lnOldRec
ENDIF

SELECT (lnAlias)
RETURN .T.

*!*************************************************************
*! Name      : lpSavScr
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/04/2006
*! Purpose   : Save / Delete in Master Files
*!*************************************************************
*! Example   : =lpSavScr()
*!*************************************************************
FUNCTION lpSavScr
LPARAMETERS loFormSet

LOCAL lnAlias, lcSetDel, lcKey, lcVenCode
lnAlias   = SELECT(0)
lcSetDel  = SET("Deleted")
lcVenCode = PADR(loFormSet.AriaForm1.kbVendor.KeyTextBox.Value, 8)

*! C201577,1 MMT 05/13/2013 Add trigger to allow user to add multiple Vendor reference to the same item[Start]
IF ASCAN(loFormSet.laEvntTrig , PADR('UPDMASTDT',10)) <> 0
  =loFormSet.mDoTrigger(PADR('UPDMASTDT',10))
ENDIF
*! C201577,1 MMT 05/13/2013 Add trigger to allow user to add multiple Vendor reference to the same item[End]

=gfSEEK(lcVenCode, 'VENDMATL')

SELECT (loFormSet.lcTmpFile)
SET ORDER TO MatCol

SET DELETED OFF

SELECT VENDMATH
IF DELETED()
  gfDelete()
  
  *--Delete from lines
  SELECT (loFormSet.lcTmpFile)
  =SEEK(lcVenCode)
  DELETE REST WHILE VENDOR+FABRIC+COLOR+CVENFAB+CVENCOLR = lcVenCode
ENDIF

SELECT (loFormSet.lcTmpFile)
SCAN
  lcKey = Vendor +;
          IIF(!EMPTY(OldFbCl),;
              PADR(SUBSTR(OldFbCl, 1, loFormSet.lnMajLen), loFormSet.lnFabLenInMATL) +;
                   SUBSTR(OldFbCl, loFormSet.lnMajLen + 2,6),;
              Fabric + Color)

  *! C201577,1 MMT 05/13/2013 Add trigger to allow user to add multiple Vendor reference to the same item[Start]
  IF ASCAN(loFormSet.laEvntTrig , PADR('CHNGKEYV',10)) <> 0
    lcKey =loFormSet.mDoTrigger(PADR('CHNGKEYV',10))
  ENDIF
  *! C201577,1 MMT 05/13/2013 Add trigger to allow user to add multiple Vendor reference to the same item[End]
              
  DO CASE
    *--If this record was deleted
    CASE DELETED()
      *--Note that if this record was deleted and wasn't previously added to the master
      *--file that means it's a new one. So no action will be taken.
      IF SEEK(vendor+fabric+color+cvenfab+cvencolr, 'VENDMATL')  && No need to gfSeek() again...
        SELECT VENDMATL
        *-If it's not deleted, Delete it.
        IF !DELETED()
          gfDelete()
        ELSE
          *--Sreach any non-deleted record as it might be added and deleted and readded before.
          LOCATE REST WHILE vendor+fabric+color+cvenfab+cvencolr = lcKey FOR !DELETED()
          IF FOUND()
            gfDelete()
          ENDIF
        ENDIF
      ENDIF
    
    CASE !DELETED()
      SELECT VENDMATL
      *--It it already exists, reflict the changes only.
      IF !SEEK(lcKey)   && No need to gfSeek() again...
        gfAppend()
      ELSE
        IF DELETED()
          LOCATE REST WHILE Vendor+Fabric+Color+cVenFab+cVenColr = lcKey FOR !DELETED()
          IF !FOUND()
            =SEEK(lcKey)   && No need to gfSeek() again...
            RECALL
          ENDIF
        ENDIF   
      ENDIF
      
      SELECT (loFormSet.lcTmpFile)
      SCATTER MEMVAR MEMO
      
      *-- If Key value has been changed then we have delete the Old Record and Add it again. (RDAC - SQL)
      
      SELECT VENDMATL

      *: B608396,1 MMT 12/31/2007 Convert VENDMATH,VENDMATL tables to SQL[Start]
      *!*        IF !EOF() AND Vendor+Fabric+Color+cVenFab+cVenColr # m.Vendor+m.Fabric+m.Color+m.cVenFab+m.cVenColr
      *!*          gfDelete()
      *!*          gfAppend()
      *!*        ENDIF
      *: B608396,1 MMT 12/31/2007 Convert VENDMATH,VENDMATL tables to SQL[End]
      
      gfReplace("VENDOR   WITH m.VENDOR ," +;
                "FABRIC   WITH m.FABRIC ," +;
                "COLOR    WITH m.COLOR  ," +;
                "CVENFAB  WITH m.CVENFAB," +;
                "CVENCOLR WITH m.CVENCOLR,"+;
                "ITEM     WITH m.ITEM")
      GATHER MEMVAR MEMO
      
      =gfAdd_info('VENDMATL', loFormSet)
      
      SELECT VENDMATL
      gfReplace()
  ENDCASE
ENDSCAN

SET DELETED &lcSetDel

SELECT VENDMATH
*loFormSet.RecordLock(.F.)

llUpdate = gfTableUpdate(.T.)
=gfSEEK(lcVenCode, 'VENDMATH')
loFormSet.RecordLock(.F.)

SELECT VENDMATL
llUpdate = llUpdate AND gfTableUpdate(.T.)

IF !llUpdate
  loFormSet.RecordLock(.T.)
ENDIF

SELECT (lnAlias)
RETURN llUpdate

*!*************************************************************
*! Name      : lfChkPos
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/04/2006
*! Purpose   : Check if an Item has POs
*!*************************************************************
*! Example   : =lfChkPos()
*!*************************************************************
FUNCTION lfChkPos
LPARAMETERS loFormSet, lcKey

LOCAL lnAlias, llPos, lcVenCode
lnAlias   = SELECT()
lcVenCode = loFormSet.AriaForm1.kbVendor.KeyTextBox.Value
llPos     = .F.
lcKey     = '0002' + lcKey

IF gfSEEK(lcKey+'P','POSLN')
  SELECT POSLN
  LOCATE REST WHILE CINVTYPE+STYLE+CBUSDOCU+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = lcKey FOR Vendor = lcVenCode
  llPos = FOUND()
ENDIF

SELECT (lnAlias)
RETURN llPos

*!*************************************************************
*! Name      : lfCrtTempLine
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/03/2006
*! Purpose   : Create Temp Lines Cursor
*!*************************************************************
*! Example   : =lfCrtTempLine()
*!*************************************************************
FUNCTION lfCrtTempLine
LPARAMETERS loFormSet

LOCAL lnAlias, lnFileStru
lnAlias = SELECT(0)

*--Create the temp file
SELECT VENDMATL
=AFIELDS(laFileStru)

lnFileStru = ALEN(laFileStru,1)

DIMENSION laFileStru[lnFileStru+3,18]
laFileStru[lnFileStru+1,1] = 'OldFbCl'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 19
laFileStru[lnFileStru+1,4] = 0

laFileStru[lnFileStru+2,1] = 'cItemDesc'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = 30
laFileStru[lnFileStru+2,4] = 0

laFileStru[lnFileStru+3,1] = 'cColorDesc'
laFileStru[lnFileStru+3,2] = 'C'
laFileStru[lnFileStru+3,3] = 30
laFileStru[lnFileStru+3,4] = 0

STORE 0 TO laFilestru[lnFileStru+1,17], laFilestru[lnFileStru+1,18],;
           laFilestru[lnFileStru+2,17], laFilestru[lnFileStru+2,18],;
           laFilestru[lnFileStru+3,17], laFilestru[lnFileStru+3,18]

FOR lnI = 7 TO 16
  STORE "" TO lafilestru[lnFileStru+1,lnI],;
              lafilestru[lnFileStru+2,lnI],;
              lafilestru[lnFileStru+3,lnI]
ENDFOR

DECLARE laIndex[2,2]
laIndex[1,1] = 'Vendor+Item+cVenFab+cVenColr'
laIndex[1,2] = 'MatCol'
laIndex[2,1] = 'Vendor+cVenFab+cVenColr+Item'
laIndex[2,2] = 'VenMat'
=gfCrtTmp(loFormSet.lcTmpFile, @laFileStru, @laIndex)

SELECT (loFormSet.lcTmpFile)
=CURSORSETPROP("Buffering", 5)
SET ORDER TO MATCOL

SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfCrtTempLine
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 04/03/2006
*! Purpose   : Adjust Grid Columns
*!*************************************************************
*! Example   : =lfAdjustGrid()
*!*************************************************************
FUNCTION lfAdjustGrid
LPARAMETERS loFormSet

WITH loFormSet.AriaForm1.grdLines
  .RecordSource = ''
  .RecordSource = loFormSet.lcTmpFile
  
  .Column1.Header1.Caption = loFormSet.lcItemTitle
  .Column1.ControlSource   = loFormSet.lcTmpFile + '.Item'
  .Column2.ControlSource   = loFormSet.lcTmpFile + '.cVenFab'
  .Column3.ControlSource   = loFormSet.lcTmpFile + '.cVenColr'
  .Column4.ControlSource   = loFormSet.lcTmpFile + '.nFabCost'
  .Column5.ControlSource   = loFormSet.lcTmpFile + '.nFabTotQty'
  .Column6.ControlSource   = loFormSet.lcTmpFile + '.LeadTime'
ENDWITH

*!*************************************************************
*! Name      : lfAddPro
*! Developer : AHMED MAHER (AMH)
*! Date      : 12/25/2003
*! Purpose   : function to Add properties to the FormSet.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfAddPro
LPARAMETERS loFormSet

LOCAL lnI
FOR lnI = 1 TO ALEN(laAllVar,1)
  IF LEFT(laAllVar[lnI,1],2) = 'la'
    LOCAL lnRow,lnCol,lcACopy
    lnRow = ALEN(laAllVar[lnI,1],1)
    lnCol = ALEN(laAllVar[lnI,1],2)
    loFormSet.AddProperty(laAllVar[lnI,1]+'['+ALLTRIM(STR(lnRow))+;
                                          ','+ALLTRIM(STR(lnCol))+']')
    lcACopy = '=ACOPY(' + laAllVar[lnI,1] + ',loFormSet.' + laAllVar[lnI,1] + ')'
    &lcACopy.
  ELSE
    loFormSet.AddProperty(laAllVar[lnI,1],EVALUATE(laAllVar[lnI,1]))
  ENDIF
ENDFOR
*--end of lfAddPro.
