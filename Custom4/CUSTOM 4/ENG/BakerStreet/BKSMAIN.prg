*!**************************************************************************
*! Name      : BKSMAIN.PRG
*! Developer : MMT (Mariam Mazhar)
*! Date      : 06/08/2008
*! Purpose   : Baker Street Custom Process Program.
*! C201009 for Aria4  attachments
*! C201010 for Aria27 attachments
*! Ticket id [T20080417.0002]
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       
*!**************************************************************************
*!Modifications:
*:*C201017,1 MMT 06/23/2008 Custom to validated User definable field against Material file [T20080331.0001] 
*:*C201017,2 MMT 06/30/2008 Custom to validated User definable field against Material file [T20080331.0001]
*:*C201024,1 MMT 07/06/2008 Custom to Add Setup For Default Sort of Style Cost Sheet Screen[T20080331.0002] 
*!**************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue


*:**************************************************************************
*:* Name        : lfADDFLDDTL
*:* Developer   : MMT (Mariam Mazhar)
*:* Date        : 06/08/2008
*:* Purpose     : Add Price detail fields to Style Cost Sheet Screen
*:***************************************************************************
FUNCTION lfADDFLDDTL

lcCurrAlias = SELECT()
lcCurCode = ''
lnPrice = 0
lcUomBuy = ''
lnMatMajlen = LEN(gfItemMask('PM','', "0002"))
IF TYPE("loFormSet.ariaFORM1.ariapageframe1.pglineDetail.txtPrcDtl") <> 'O'
  loFormSet.ariaFORM1.ariapageframe1.pglineDetail.Addobject('txtPrcDtl','AriaTextBox')
  loFormSet.ariaFORM1.ariapageframe1.pglineDetail.txtPrcDtl.Width = 145
  loFormSet.ariaFORM1.ariapageframe1.pglineDetail.txtPrcDtl.Top   = loFormSet.ariaFORM1.ariapageframe1.pglineDetail.cboStatus.top
  *C201009 ,2 MMT 06/19/2008 Fix bug of wrong position of txtBOX [Start]
  *loFormSet.ariaFORM1.ariapageframe1.pglineDetail.txtPrcDtl.Left  = 504
  *C201009 ,2 MMT 06/19/2008 Fix bug of wrong position of txtBOX [End]
ENDIF 

*C201009 ,2 MMT 06/19/2008 Fix bug of wrong position of txtBOX [Start]
loFormSet.ariaFORM1.ariapageframe1.pglineDetail.txtPrcDtl.Left  = loFormSet.ariaform1.ariapageframe1.pglineDetail.txtMarker.left
*C201009 ,2 MMT 06/19/2008 Fix bug of wrong position of txtBOX [End]

IF !(EVALUATE(loFormSet.lcTmpBom+'.cCatgTyp') $ 'TF') AND  TYPE("loFormSet.ariaFORM1.ariapageframe1.pglineDetail.txtPrcDtl") = 'O'
  loFormSet.ariaFORM1.ariapageframe1.pglineDetail.txtPrcDtl.VISIble = .F.
  loFormSet.ariaFORM1.ariapageframe1.pglineDetail.lblMarker.caption = "Marker"
  RETURN 
ENDIF 

loFormSet.ariaFORM1.ariapageframe1.pglineDetail.txtPrcDtl.VISIble = .T.
loFormSet.ariaFORM1.ariapageframe1.pglineDetail.lblMarker.VISIble = .T. 
loFormSet.ariaFORM1.ariapageframe1.pglineDetail.lblMarker.caption = "Pricing Details"
 
IF !USED("ITEM_INFO")
  gfOpenTable('ITEM','style','SH','ITEM_INFO')
ENDIF 
IF "******" $ EVALUATE(loFormSet.lcTmpBom+'.ITEM') 
  IF gfSeek("0002"+SUBSTR(EVALUATE(loFormSet.lcTmpBom+'.ITEM'),1,lnMatMajlen),'ITEM_INFO','cstyle')
    lcCurCode = ITEM_INFO.cpricecur
    lnPrice = ITEM_INFO.nicost1
    IF !EMPTY(ITEM_INFO.cconvbuy)
      lnConf = 1
      lcUomCode = ITEM_INFO.cconvbuy
      =gfGetUOMData(lcUomCode , @lcUomBuy , '', lnConf)
    ENDIF 
  ENDIF   
ELSE
  IF gfSeek("0002"+EVALUATE(loFormSet.lcTmpBom+'.ITEM'),'ITEM_INFO','style')
    lcCurCode = ITEM_INFO.cpricecur
    lnPrice = ITEM_INFO.nicost1
    IF !EMPTY(ITEM_INFO.cconvbuy)
      lnConf = 1
      lcUomCode = ITEM_INFO.cconvbuy
      =gfGetUOMData(lcUomCode , @lcUomBuy , '', lnConf)
    ENDIF 
  ENDIF 
ENDIF 

loFormSet.ariaFORM1.ariapageframe1.pglineDetail.txtPrcDtl.Value = lcCurCode +" "+ALLTRIM(STR(lnPrice,11,3))+" "+ALLTRIM(lcUomBuy) 
loFormSet.ariaFORM1.ariapageframe1.pglineDetail.txtPrcDtl.enabled = .F.

SELECT(lcCurrAlias)

*:**************************************************************************
*:* Name        : lfADDCOLDTL
*:* Developer   : MMT (Mariam Mazhar)
*:* Date        : 06/08/2008
*:* Purpose     : Add Price detail Column to Style Cost Sheet Costing Screen
*:***************************************************************************
FUNCTION lfADDCOLDTL

lnArrLen = ALEN( laBOMStru,1)
DIMENSION laBOMStru[lnArrLen +1,18]

laBOMStru[lnArrLen +1,1] = 'cMatInfo'
laBOMStru[lnArrLen +1,2] = 'C'
laBOMStru[lnArrLen +1,3] = 20
laBOMStru[lnArrLen +1,4] = 0

STORE '' TO laBOMStru[lnArrLen +1,7],laBOMStru[lnArrLen +1,8],laBOMStru[lnArrLen +1,9],;
          laBOMStru[lnArrLen +1,10],laBOMStru[lnArrLen +1,11],laBOMStru[lnArrLen +1,12],;
          laBOMStru[lnArrLen +1,13],laBOMStru[lnArrLen +1,14],laBOMStru[lnArrLen +1,15],;
          laBOMStru[lnArrLen +1,16]
STORE 0 TO  laBOMStru[lnArrLen +1,17],laBOMStru[lnArrLen +1,18]

*:**************************************************************************
*:* Name        : lfADDVALCST 
*:* Developer   : MMT (Mariam Mazhar)
*:* Date        : 06/08/2008
*:* Purpose     : Fill Price detail Field in Costing Screen
*:***************************************************************************
FUNCTION lfADDVALCST 
lcCurrAlias = SELECT()
lcCurCode = ''
lnPrice = 0
lcUomBuy = ''
lnMatMajlen = LEN(gfItemMask('PM','', "0002"))


IF !USED("ITEM_INFO")
  gfOpenTable('ITEM','style','SH','ITEM_INFO')
ENDIF 
IF "******" $ ITEM 
  IF gfSeek("0002"+SUBSTR(ITEM,1,lnMatMajlen),'ITEM_INFO','cstyle')
    lcCurCode = ITEM_INFO.cpricecur
    lnPrice = ITEM_INFO.nicost1
    IF !EMPTY(ITEM_INFO.cconvbuy)
      lnConf = 1
      lcUomCode = ITEM_INFO.cconvbuy
      =gfGetUOMData(lcUomCode , @lcUomBuy , '', lnConf)
    ENDIF 
  ENDIF   
ELSE
  IF gfSeek("0002"+ITEM,'ITEM_INFO','style')
    lcCurCode = ITEM_INFO.cpricecur
    lnPrice = ITEM_INFO.nicost1
    IF !EMPTY(ITEM_INFO.cconvbuy)
      lnConf = 1
      lcUomCode = ITEM_INFO.cconvbuy
      =gfGetUOMData(lcUomCode , @lcUomBuy , '', lnConf)
    ENDIF 
  ELSE
    IF gfSeek("0002"+SUBSTR(ITEM,1,lnMatMajlen),'ITEM_INFO','cstyle')
      lcCurCode = ITEM_INFO.cpricecur
      lnPrice = ITEM_INFO.nicost1
      IF !EMPTY(ITEM_INFO.cconvbuy)
        lnConf = 1
        lcUomCode = ITEM_INFO.cconvbuy
        =gfGetUOMData(lcUomCode , @lcUomBuy , '', lnConf)
      ENDIF 
    ENDIF 
  ENDIF 
ENDIF 

REPLACE cMatInfo WITH  lcCurCode +" "+ALLTRIM(STR(lnPrice,11,3))+" "+ALLTRIM(lcUomBuy) 


SELECT(lcCurrAlias)

*:**************************************************************************
*:* Name        : lfADDGRDCOL
*:* Developer   : MMT (Mariam Mazhar)
*:* Date        : 06/08/2008
*:* Purpose     : Add Price Column to Costing grid
*:***************************************************************************
FUNCTION lfADDGRDCOL

loGrdObject.Columncount = loGrdObject.Columncount + 1
lcColNo = ALLTRIM(STR(loGrdObject.Columncount))
lcFileName = loGrdObject.recordSource
loGrdObject.Column&lcColNo..ControlSource = '&lcFileName..cMatInfo'
loGrdObject.Column&lcColNo..readonly = .T.
loGrdObject.Column&lcColNo..width = 100
loGrdObject.Column&lcColNo..header1.caption = "Pricing Details"


*:**************************************************************************
*:* Name        : lfADDTXTCST
*:* Developer   : MMT (Mariam Mazhar)
*:* Date        : 06/08/2008
*:* Purpose     : Add Text Price Field to New Item Screen
*:**************************************************************************
FUNCTION lfADDTXTCST

IF TYPE('loBranchForm') = 'O' 
  IF TYPE('loBranchForm.AriaForm1.cntUnitCost.txtPrcDtl') <> 'O'
    loBranchForm.AriaForm1.cntUnitCost.Addobject('txtPrcDtl','AriaTextBox')
    loBranchForm.AriaForm1.cntUnitCost.txtPrcDtl.Width = 125
    loBranchForm.AriaForm1.cntUnitCost.txtPrcDtl.Top   = loBranchForm.AriaForm1.cntUnitCost.txtcstperc.Top
    loBranchForm.AriaForm1.cntUnitCost.txtPrcDtl.Left  = 400
  ENDIF 
  loBranchForm.AriaForm1.cntUnitCost.txtPrcDtl.Visible = .T.
  loBranchForm.AriaForm1.cntUnitCost.txtPrcDtl.Enabled = .F.
  loBranchForm.AriaForm1.cntUnitCost.lblMarker.Visible =.T.
  loBranchForm.AriaForm1.cntUnitCost.lblMarker.caption = "Pricing Details :"
ENDIF 


lcCurrAlias = SELECT()
lcCurCode = ''
lnPrice = 0
lcUomBuy = ''
lnMatMajlen = LEN(gfItemMask('PM','', "0002"))

IF !USED("ITEM_INFO")
  gfOpenTable('ITEM','style','SH','ITEM_INFO')
ENDIF 
IF gfSeek("0002"+m.CstyMajor,'ITEM_INFO','cstyle')
  lcCurCode = ITEM_INFO.cpricecur
  lnPrice = ITEM_INFO.nicost1
  IF !EMPTY(ITEM_INFO.cconvbuy)
    lnConf = 1
    lcUomCode = ITEM_INFO.cconvbuy
    =gfGetUOMData(lcUomCode , @lcUomBuy , '', lnConf)
  ENDIF 
ENDIF   
loBranchForm.AriaForm1.cntUnitCost.txtPrcDtl.value =lcCurCode +" "+ALLTRIM(STR(lnPrice,11,3))+" "+ALLTRIM(lcUomBuy) 
SELECT(lcCurrAlias)

*:**************************************************************************
*:* Name        : lfRSTNEWCS
*:* Developer   : MMT (Mariam Mazhar)
*:* Date        : 06/08/2008
*:* Purpose     : Reset Values for new item screen
*:**************************************************************************
FUNCTION lfRSTNEWCS
IF TYPE('loBranchForm') = 'O' 
  IF TYPE('loBranchForm.AriaForm1.cntUnitCost.txtPrcDtl') = 'O'
    loBranchForm.AriaForm1.cntUnitCost.txtPrcDtl.Visible = .F.
    loBranchForm.AriaForm1.cntUnitCost.lblMarker.caption = "Marker"
    loBranchForm.AriaForm1.cntUnitCost.lblMarker.Visible = .F.
  ENDIF
ENDIF 

*:**************************************************************************
*:* Name        : lfCSTFABCLR 
*:* Developer   : MMT (Mariam Mazhar)
*:* Date        : 06/08/2008
*:* Purpose     : get color price when user select certain color
*:**************************************************************************
FUNCTION lfCSTFABCLR 

lcCurrAlias = SELECT()
lnMatMajlen = LEN(gfItemMask('PM','', "0002"))
lcCurCode = ''
lnPrice = 0
lcUomBuy = ''
IF !USED("ITEM_INFO")
  gfOpenTable('ITEM','style','SH','ITEM_INFO')
ENDIF 

IF TYPE('loBranchForm') = 'O' 
  IF TYPE('loBranchForm.AriaForm1.cntUnitCost.txtPrcDtl') <> 'O' AND !EMPTY(loBranchForm.ariaForm1.kbItemColor.keytextbox.Value)
    IF gfSeek("0002"+ALLTRIM(m.CstyMajor),'ITEM_INFO','style')
      SELECT 'ITEM_INFO'
      LOCATE FOR SUBSTR(Style,lnFbrClrStr,lnFbrClrLen) = loBranchForm.ariaForm1.kbItemColor.keytextbox.Value
      IF FOUND()
        lcCurCode = ITEM_INFO.cpricecur
        lnPrice = ITEM_INFO.nicost1
        IF !EMPTY(ITEM_INFO.cconvbuy)
          lnConf = 1
          lcUomCode = ITEM_INFO.cconvbuy
          =gfGetUOMData(lcUomCode , @lcUomBuy , '', lnConf)
        ENDIF 
      ELSE
        LOCATE 
        lcCurCode = ITEM_INFO.cpricecur
        lnPrice = ITEM_INFO.nicost1
        IF !EMPTY(ITEM_INFO.cconvbuy)
          lnConf = 1
          lcUomCode = ITEM_INFO.cconvbuy
          =gfGetUOMData(lcUomCode , @lcUomBuy , '', lnConf)
        ENDIF 
      ENDIF   
    ENDIF 
    loBranchForm.AriaForm1.cntUnitCost.txtPrcDtl.value =lcCurCode +" "+ALLTRIM(STR(lnPrice,11,3))+" "+ALLTRIM(lcUomBuy) 
  ENDIF
ENDIF 
SELECT(lcCurrAlias)

*:*C201017,1 MMT 06/23/2008 Custom to validated User definable field against Material file [Start] 
*:**************************************************************************
*:* Name        : lfvMat
*:* Developer   : MMT (Mariam Mazhar)
*:* Date        : 06/23/2008
*:* Purpose     : Validate user entered material value
*:**************************************************************************
FUNCTION lfvMat

lnAlias    = SELECT(0)
lcControl  = loOGScroll.FocusControl
lcCurVar   = loOGScroll.&lcControl.
lcCurVal   = PADR(lcCurVar.Value, 19)
lcMajLen   = LEN(gfItemMask('PM','', '0002'))
IF EMPTY(lcCurVal)
  RETURN 
ELSE
  IF !USED('Item_File')
    =gfOpenTable('Item','CSTYLE','SH','Item_File')
  ENDIF 
  IF !gfSeek("0002"+lcCurVal,'Item_File')
    =gfSeek("0002",'Item_File')
    =SEEK("0002"+lcCurVal,'Item_File')
    
    *:*C201017,2 MMT 06/30/2008 Custom to validated User definable field against Material file[Start]
    *lcItemCd = gfItemBrow("0002", "", "", "", "M", "", .F.)
    lcItemCd = gfItemBrow("0002", lcCurVal, "", "", "M", "", .F.)
    *:*C201017,2 MMT 06/30/2008 Custom to validated User definable field against Material file[End]
    
    IF !EMPTY(lcItemCd )
      lcCurVar.Value = lcItemCd 
      loOgScroll.laOGFxFlt[lcCurVar.Parent.nRowIndex,6] = PADR(lcItemCd , lcMajLen)
      = lfOGShowGet(lcCurVar.Parent.nRowIndex)
    ELSE
      lcCurVar.Value = ''
      loOgScroll.laOGFxFlt[lcCurVar.Parent.nRowIndex,6] = ''
      = lfOGShowGet(lcCurVar.Parent.nRowIndex)
    ENDIF 
  ENDIF 
ENDIF 
SELECT(lnAlias )
*:**************************************************************************
*:* Name        : LFSETPROC
*:* Developer   : MMT (Mariam Mazhar)
*:* Date        : 06/23/2008
*:* Purpose     : Call this function in ICItem Screen init to set procedure to this program
*:**************************************************************************
FUNCTION LFSETPROC
RETURN 

*:*C201017,2 MMT 06/30/2008 Custom to validated User definable field against Material file[Start]
*:**************************************************************************
*:* Name        : lfSAVUDFM
*:* Developer   : MMT (Mariam Mazhar)
*:* Date        : 06/23/2008
*:* Purpose     : Call this function to save UDF fields
*:**************************************************************************
FUNCTION lfSAVUDFM
lcStdFlds = lcStdFlds + ",CALTFEC,CALTFEC2,CALTFEC3,CALTFEC4"
*:*C201017,2 MMT 06/30/2008 Custom to validated User definable field against Material file[End]
*:*C201017,1 MMT 06/23/2008 Custom to validated User definable field against Material file [End] 

*:*C201024,1 MMT 07/06/2008 Custom to Add Setup For Default Sort of Style Cost Sheet Screen[Start] 
*:**************************************************************************
*:* Name        : lfCSETORDER
*:* Developer   : MMT (Mariam Mazhar)
*:* Date        : 07/06/2008
*:* Purpose     : Call this function to Change the sort by
*:**************************************************************************
FUNCTION lfCSETORDER
IF ALLTRIM(gfGetMemVar('M_POCSDOR', oAriaApplication.ActiveCompanyID)) = 'L'
  loFormSet.llSortByln = .T.
  loFormSet.Activate()
ENDIF 
*:**************************************************************************
*:* Name        : lfCCALLCOST 
*:* Developer   : MMT (Mariam Mazhar)
*:* Date        : 07/06/2008
*:* Purpose     : Call the costing screen from style screen
*:**************************************************************************
FUNCTION lfCCALLCOST 
IF ALLTRIM(gfGetMemVar('M_POCSDOR', oAriaApplication.ActiveCompanyID)) = 'L'
  DO FORM (oAriaApplication.ScreenHome+'Costing.scx') WITH ;
          lcCurrStyle,loFormSet.lcBom,loFormSet.lcCostSheetType,,,.T.

ELSE
  DO FORM (oAriaApplication.ScreenHome+'Costing.scx') WITH ;
          lcCurrStyle,loFormSet.lcBom,loFormSet.lcCostSheetType

ENDIF 
gfSeek(loFormSet.lcStyleKey)
SELECT(lnCurrAlias)
*:*C201024,1 MMT 07/06/2008 Custom to Add Setup For Default Sort of Style Cost Sheet Screen[End] 