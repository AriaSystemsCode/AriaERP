*:***********************************************************************
*:  Program file : ALAUPLJ.PRG
*:  Program desc.: Automatic Packing List Template (Custom for JET10)
*:         System: Aria 4XP
*:      Developer: Mariam Mazhar
*:           Date: 03/01/2009
*:      Reference: C201109[T20081208.0016]
*:************************************************************************
*: Modifications:
*: B609154,1 MMT 02/24/2010 Fix bug of error when run program on SAAS[T20100223.0003]
*: B609229,1 MMT 04/29/2010 Fix bug of error when run program on SAAS[T20100419.0022]
*:************************************************************************
#INCLUDE D:\ARIA4XP\PRGS\ALPLIST.h
PARAMETERS lcPassOrd
*: B609154,1 MMT 02/24/2010 Fix bug of error when run program on SAAS[Start]
IF oAriaApplication.MULTIINST 
  *: B609229,1 MMT 04/29/2010 Fix bug of error when run program on SAAS[Start]
  * DO FORM ("X:\aria4xp\Screens\"+'AL\ALAUPLJ.SCX')
  =gfCallForm('ALAUPLJ','AL',lcPassOrd)
  *: B609229,1 MMT 04/29/2010 Fix bug of error when run program on SAAS[End]
ELSE
*: B609154,1 MMT 02/24/2010 Fix bug of error when run program on SAAS[End]
  DO FORM (oAriaApplication.ScreenHome+'AL\ALAUPLJ.SCX') WITH lcPassOrd
*: B609154,1 MMT 02/24/2010 Fix bug of error when run program on SAAS[Start]
ENDIF 
*: B609154,1 MMT 02/24/2010 Fix bug of error when run program on SAAS[End]
*!*************************************************************
*! Name      : lfInit
*: Developer : Mariam Mazhar
*: Date      : 03/01/2009
*! Purpose   : Init Method of the Form
*!*************************************************************
FUNCTION lfInit
PARAMETERS loFormSet
loFormSet.ariaBrFields.edtBrowseFields.Value = "Order:H='Order',Account:H= 'Account',"+;
                                               "Status:H='Status',TOT_wght:H='Tot. Weight',"+;
                                               "tot_cart:H='Tot. Carton',Tot_pcs:H='Tot. Pcs',cwarecode:H='Warehouse'"
loFormSet.llShoPckSu = .T.
loFormSet.lnLastNo = 0
=gfOPenTable('TMPL_LIN','TMPL_LIN')
=gfOPenTable('TMPL_HDR','TMPL_hDr')
=gfOPenTable('OrdHdr','OrdHdr')
=gfOPenTable('PIKTKT','PIKTKT')
=gfOPenTable('Customer','Customer')
=gfOPenTable('ORDLINE','ORDLINE')
=gfOPenTable('SPck_lin','SPCK_LINVR')
=gfOPenTable('PIKLINE','PIKLINEO')
=gfOPenTable('SCALE','SCALE')
=gfOPenTable('StyLE','StyLE')
=gfOPenTable('SPck_Hdr','SPCK_HDRVR')
loFormSet.lnCrtnSeq = 0 
loFormset.lnCurrCtn = 0
loFormSet.lcPckLin = gfTempName()
loFormSet.lcPakIndxSt =gfTempName()
loFormSet.lcCtnHdr= gfTempName()
loFormSet.lcCtnDtl= gfTempName()
loFormSet.lcSumPck= gfTempName()
loFormSet.lcTmpPck= gfTempName()
loFormSet.lcTempCur= gfTempName()
loFormSet.lnMaxCtn = 0
loFormset.lnCtnPal = 0
STORE 0 TO loFormSet.lnMaxPal,loFormSet.lnMinPal
loFormSet.lcStyTtl = loFormSet.ariaForm1.pgfPacking.CartonInfo.kbStyle.lcItemHeader
loFormSet.lnMajLen = LEN(gfItemMask('PM','','0001'))
=lfCrTmpFiles(loFormSet)
WITH loFormSet
  .cbrowsetabledbengine   = "SQL"
  .nWorkArea        = 'TMPL_HDR'
  .DataEnvironment.InitialSelectedAlias = 'TMPL_HDR'
  .cBrowseFileName        = "TMPL_HDR"
  .cBrowseIndexExpression = "ORDER"
  .cBrowseIndexFields     = "ORDER"
  .cBrowseIndexName       = "TMPL_HDR"
  .cBrowseAliasName       = "TMPL_HDR"
  .cBrowseTableName       = "TMPL_HDR"
  .cBrowseFilter          = ""
  .BrowseTitle 		  	  ="Templates"
ENDWITH 

loFormSet.llEdiSys   = ('AS' $ oAriaApplication.CompanyInstalledModules)

IF loFormSet.llEdiSys
  =gfOPenTable('EDIAcPrt','ACCFACT')
  =gfOPenTable('EDIPH','PARTNER')
  =gfOPenTable('EDICRTSQ','EDICRTSQ')
  =gfOPenTable('BOL_HDR','BOL_HDR')
  =gfOPenTable('BOL_LIN','BOL_LIN')
ENDIF

loFormSet.llAnyRec = .F.
loFormSet.llNew = .F.


*!*************************************************************
*! Name      : lfvOrderNo
*: Developer : Mariam Mazhar
*: Date      : 03/01/2009
*! Purpose   : Validate Order Number
*!*************************************************************
FUNCTION lfvOrderNo
LPARAMETERS loFormSet, lcOrderNo,lcAccount, llBrowse
PRIVATE lnCurAlias,lcOrdHdrTg,lcOrder
lnCurAlias = SELECT(0)

lcOrdHdrTg = ORDER('OrdHdr')
SELECT Ordhdr
=gfSetOrder('OrdHdr')


IF !( (EMPTY(lcOrderNo) OR LASTKEY() <> 13) AND !llBrowse)
  IF (!EMPTY(lcOrderNo) .AND. !gfSEEK('O'+lcOrderNo,'OrdHdr') ) OR llBrowse
    lcOrder = lcOrderNo
    =lfOrdBrow(@lcOrder,lcAccount)
    lcOrderNo = lcOrder 
    loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE = lcOrderNo
    llBrowse = .F.
  ENDIF
ENDIF


IF !EMPTY(lcOrderNo) AND gfSEEK('O'+lcOrderNo,'OrdHdr')
  lcAlias = SELECT(0)
  SELECT TMPL_HDR
  lcTmpKey = Order
  llPacked = gfSEEK(lcOrderNo,'TMPL_HDR')
  =gfSEEK(lcTmpKey)

  *relocate pointer on the Ordhdr file due to the relation between tmpl_hdr and ordHdr
  =gfSEEK('O'+lcOrderNo ,'OrdHdr')
  SELECT (lcAlias)
  
  DO CASE

    *--- and the orde packed.[Begin]
     CASE OrdHdr.Status = 'C' AND !llPacked
       =gfModalGen("INM000000B00000","DIALOG",'','',;
       'This order is completed, cannot create a template for it.')  
      loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE  = SPACE(6)
      llPacked = .F.
      
    CASE OrdHdr.Status = 'X'
      *-- This order is canceled, cannot pack.
      *-- OK
      = gfModalGen("INM44050B00000","Dialog","canceled")  
      loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE = SPACE(6)

    
    CASE OrdHdr.bulk = 'Y'
      *-- This order is bulk, cannot pack.
      *-- OK
      = gfModalGen("INM44050B00000","Dialog","bulk")  
      loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE = SPACE(6)

    
    OTHERWISE
      llComp = (OrdHdr.Status = 'C' AND !llPacked)
      =lfGetData(loFormSet, lcOrderNo)
          
      SELECT PIKTKT
      =gfSetOrder('ORDPIK')
      
      IF !llPacked
        lnRspn = gfModalGen('INM00000B32000',.F.,.F.,.F.,'No tamplate has been created for order '+;
                                              lcOrderNo+', Do you want to create new?')
        IF lnRspn=1
          loFormSet.ChangeMode('A')
          =lfvNewPack(loFormSet)
        ELSE
          loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE  = '      '
          loFormSet.AriaForm1.kbAccount.keytextbox.VALUE = '     '
          loFormSet.ChangeMode('S')
        ENDIF
      ELSE
        Dime laSq[1]
        laSq[1] = 0
        =gfSeek(lcOrderNo,'tmpl_lin')
        select max(ncarton) from tmpl_lin where order = lcOrderNo into array laSq
        loFormSet.lnCrtnSeq = laSq[1]
        loFormSet.lnCurrCtn = loFormSet.lnCrtnSeq
        =gfSEEK(ALLTRIM(lcOrderNo),'TMPL_HDR')
        loFormSet.ChangeMode('V')
        =lfvNewPack(loFormSet)
      ENDIF
  ENDCASE
ENDIF
SELECT Ordhdr
gfSetOrder(lcOrdHdrTg)
SELECT (lnCurAlias)
  *!*************************************************************
  *! Name      : lfActFolder
  *! Developer : Mariam Mazhar
  *! Date      : 03/01/2009
  *! Purpose   : Activate folder when Change.
  *!*************************************************************
  *! Calls     :
  *!             Procedures : ....
  *!             Functions  : ....
  *!*************************************************************
  *! Passed Parameters  : None
  *!*************************************************************
  *! Returns            : None
  *!*************************************************************
  *! Example   : = lfvCtDtRem()
  *!*************************************************************

FUNCTION lfActFolder
  LPARAMETERS loFormSet,lnActFolder

  IF loFormSet.ActiveMode $ 'SV'
    RETURN 
  ENDIF 

  STORE IIF((loFormSet.ActiveMode $ 'AE' OR loFormSet.llNew) AND lnActFolder = 1,.T.,.F.) TO llInstStat,llShipStat

  =lfWinHdrSt(loFormSet)

  DO CASE

    CASE lnActFolder = 1
      =lfSelStatus(loFormSet)
      =lfDtlBrow(loFormSet)
      =lfwDtlBrow(loFormSet)

    CASE lnActFolder = 2
      = lfWinCtnSt(loFormSet,lnActFolder)
      = lfwCtnHdrBr(loFormSet)
      = lfwCtnDtlBr(loFormSet)

  ENDCASE


  *!*************************************************************
  *! Name      : lfSelStatus
  *! Developer : Mariam MAzhar
  *! Date      : 03/01/2009
  *! Purpose   : adjust the status of selection buttons
  *!*************************************************************
  *! Calls     :
  *!             Procedures : ...
  *!             Functions  : lfSelStatus,lfDtlBrow
  *!*************************************************************
  *! Passed Parameters  : None
  *!*************************************************************
  *! Returns            : None
  *!*************************************************************
  *! Example   : = lfSelStatus()
  *!*************************************************************

FUNCTION lfSelStatus
  LPARAMETERS loFormSet

  PRIVATE lnCurAlias,lnCurRec

  lnCurAlias = SELECT(0)

  SELECT (loFormSet.lcPckLin)
  lnCurRec = RECNO()

  IF !loFormSet.llAnyRec
    llDtl2Stat = .F.
  ELSE
    llDtl2Stat = IIF((loFormSet.ActiveMode $ 'AE' OR loFormSet.llNew) AND loFormSet.ariaForm1.pgfPacking.ACTIVEPAGE = 1,.T.,.F.)
  ENDIF

  WITH loFormSet.ariaForm1.pgfPacking.DETAIL
    STORE llDtl2Stat TO   .cmdStyle.ENABLED , .cmdSelect.ENABLED ,;
      .cmdInvert.ENABLED , .cmdSelAll.ENABLED , .cmdSelNone.ENABLED , .cmdApply.ENABLED,.cmdPack.ENABLED

  ENDWITH
  SELECT (lnCurAlias)

  *!*************************************************************
  *! Name      : lfOrdBrow
  *! Developer : Mariam MAzhar
  *! Date      : 03/01/2009
  *! Purpose   : To browse all packing list for a specific order
  *!*************************************************************
  *! Parameters: loFormSet : ThisFormSet
  *!*************************************************************
  *! Called    : From ChangeMode in the formset
  *!*************************************************************
FUNCTION lfOrdBrow
  LPARAMETERS lcOrder,lcAccount

  *-- lcFields   variable that hold the name of browsed fields
  *-- laBrow     array that hold the returned values from AriaBrow function
  *-- lnCurAlias variable that hold the current alias
  *-- lcCurTag   variable that hold the currend tag name
  *-- llReturn   variable which is returned by this function
  *-- lcTag      variable that hold the name of the tag which is desired to switch
  *              file order to it

  PRIVATE lcFields,laBrowArr,lnCurAlias,lcCurTag,llReturn,lcTag,lcBrFields
  DIMENSION laBrowArr[1]
  STORE SPACE(0) TO lcFields,laBrowArr,lcBrFields

  lnCurAlias = SELECT(0)
  SELECT ORDHDR
  SET RELATION TO 'M'+Ordhdr.Account INTO customer ADDITIVE 
  =gfSeek('O')
  LOCATE
  lcTag = ORDER('OrdHdr')
  lcBrFields = [Order:H=LANG_ManulPL_lblOrderNo,status:3:H=LANG_ManulPL_lblStatus,lcSesDesc=gfCodDes(Season,'SEASON'):H=LANG_ManulPL_lblSeason,lcDivDesc=gfCodDes(cDivision,'CDIVISION'):H=LANG_ManulPL_lblDivision,]+;
    [CustPo=IIF(multipo,LANG_ManulPL_ValMultiPO,custpo):H=LANG_ManulPL_lblCustPO,]+;
    [ACCOUNT:H=LANG_ManulPL_lblAcc,store=IIF(MULTI='Y',LANG_ManulPL_ValMulti,STORE):H=LANG_ManulPL_lblStore,Customer.stname]+;
    [:15:H=LANG_ManulPL_lblName,Open:H=LANG_ManulPL_lblOpenQty,OpenAmt:H=LANG_ManulPL_lblOpenAmt,Ship:H=LANG_ManulPL_lblShipQty,Shipamt:H=LANG_ManulPL_lblShipAmt,]+;
    [start:H=LANG_ManulPL_lblStart,Complete:H=LANG_ManulPL_lblComplete,]+;
    [Note1:6:H=LANG_ManulPL_lblNotes]

  DO CASE
    CASE !EMPTY(lcAccount)
      =gfSETORDER('OrdAcct')
      lcAcc = lcAccount
      lcOrder = IIF(ARIABROW("lcAcc+'O'",;
        LANG_ManulPL_hdrOrders,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),;
        OrdHdr.ORDER,SPACE(6))

    CASE EMPTY(lcAccount)
      gfSETORDER('OrdHdr')
      lcOrder = IIF(ARIABROW("'O'",LANG_ManulPL_hdrOrders,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),;
        OrdHdr.ORDER,SPACE(6))
  ENDCASE
  SELECT Ordhdr
  SET RELATION TO 
  gfSETORDER(lcTag)
  SELECT (lnCurAlias)

  *!*************************************************************
  *! Name      : lfGetData
  *! Developer : Mariam MAzhar
  *! Date      : 03/01/2009
  *! Purpose   : To restore the order or piktkt informtion form
  *!             OrdHdr file or PikTkt file
  *!*************************************************************
  *! Parameters: loFormSet : ThisFormSet
  *!*************************************************************
  *! Called    : From ChangeMode in the formset
  *!*************************************************************
FUNCTION lfGetData
  LPARAMETERS loFormSet, lcOrderNo
  PRIVATE lcPikTkt, lcStore

  gfSEEK('O'+lcOrderNo,'OrdHdr')

  WITH loFormSet.ariaForm1
    STORE IIF(!EMPTY(lcOrderNo),OrdHdr.Account,SPACE(6)) TO .kbAccount.keytextbox.VALUE, lcAccount
    *-- Account Name
    gfSEEK('M'+OrdHdr.Account,'Customer')
    .txtCustName.VALUE = Customer.stName
    loFormSet.llEdiAcc = loFormSet.llEdiSys .AND. gfSEEK('A'+lcAccount,'EDIACPRT') .AND.;
      gfSEEK(EDIACPRT.cPartCode,'EDIPH')

    STORE loFormSet.llEdiSys AND loFormSet.llEdiAcc .AND. EDIACPRT.lPkChrDes TO loFormSet.llPCDStat
    STORE loFormSet.llEdiSys AND loFormSet.llEdiAcc .AND. EDIPH.lPltShp  TO loFormSet.llPalStat

    loFormSet.lcWareCode = IIF(EMPTY(lcOrderNo),'',OrdHdr.cWareCode)
    .txtCustPo.VALUE = OrdHDR.CustPo

    IF !loFormSet.llEdiAcc
      *-- means carton type is standard
      loFormSet.lnCtnTyp = 2
      *-- means pack is directed to store
      loFormSet.lnDrctTo = 1
    ENDIF
  ENDWITH

 *!*************************************************************
  *! Name      : lfvNewPack
  *! Developer : Mariam MAzhar
  *! Date      : 03/01/2009
  *! Purpose   : Create new pack list
  *!*************************************************************
  *! Parameters: loFormSet : ThisFormSet
  *!*************************************************************
FUNCTION lfvNewPack
  LPARAMETERS loFormSet

  PRIVATE lcTag,lnCurAlias,lcAccount, lcStore,lcOrderNo
  lcOrderNo = loFormSet.AriaForm1.kbOrderNo.keytextbox.VALUE
  lcAccount = loFormSet.AriaForm1.kbAccount.keytextbox.VALUE

  STORE .T. TO loFormSet.llCupdate,loFormSet.llAnyUpd,loFormSet.llNew

  STORE 0 TO loFormSet.lnPackWgh, loFormSet.lnPackCtn, loFormSet.lnPackQty


  *-- check if any pack is being created now on the same order in any
  *-- other session
  IF loFormSet.ActiveMode $ 'AE'
    llGoOn = lfChkOrdLok(loFormSet)
  ELSE
    llGoOn  = .T.
  ENDIF   
  gfSEEK('O'+lcOrderNo,'OrdHdr')

  IF llGoOn
    =lfvSelOrdL(loFormSet)
    *-- Set the object status
    =lfWinHdrSt(loFormSet)
  ELSE
    loFormSet.ChangeMode("S")
    RETURN .F.
  ENDIF
 *!*************************************************************
  *! Name      : lfChkOrdLok
  *! Developer : Mariam MAzhar
  *! Date      : 03/01/2009
  *! Purpose   : Check using this order in creating pack and
  *!           : Lock ordhdr record if it is not
  *!*************************************************************
  *! Parameters: loFormSet : ThisFormSet
  *!*************************************************************
FUNCTION lfChkOrdLok
  LPARAMETERS loFormSet
  PRIVATE llGoOn,lnCurAlias,lnCurRecNo, lcOrderNo,lcStore, lcOldOrd

  llGoOn     = .T.
  lcOrderNo  = loFormSet.AriaForm1.kbOrderNo.keytextbox.VALUE
  lnCurAlias = SELECT(0)

  lcOldHOrd = ORDER('ORDHDR')
  SELECT Ordhdr
  gfSetOrder('ORDHDR')
  IF gfSEEK('O'+lcOrderNo)
    SELECT ORDHDR
    llGoOn = gfObj_Lock(.T.)
  ENDIF
  gfSetOrder(lcOldHOrd)
  SELECT (lnCurAlias)
  RETURN llGoOn
  
  *!*************************************************************
  *! Name      : lfvSelOrdL
  *! Developer : Mariam MAzhar
  *! Date      : 03/01/2009
  *! Purpose   : To collect order lines
  *!*************************************************************
  *! Parameters: loFormSet : ThisFormSet
  *!*************************************************************
FUNCTION lfvSelOrdL
  LPARAMETERS loFormSet
  *-- lnAlias    variable that hold the current alias
  *-- lnI        counter to be used for the 8 quantities or piked fields
  *-- lcSize     variable that hold the size desc. from scale file
  *-- lnQty      variable that hold the value in quantities fields
  *-- lcStyle    variable that hold style
  *-- lcStyDesc  variable that hold style desc.
  *-- lnRemPQty  variable to be used in case style multi ordline is found
  *--            that hold the remained quantity from style qty as long as
  *--            style order line is packed
  PRIVATE lnAlias,lnI,lnJ,lcSize,lnQty,lcStyle,lcStyDesc,lnContinue,;
    lnCtnQty,lnCtnWgh,lnStyOrdLin,llStyFound,lnRemPQty,;
    lcPckNo, lcOrderNo, lcStore, lcPkTktNo , llLinFound

  LOCAL lnAvlQty,lnOQty,lnTotPck, lcSizeNum

  *-- This variable indecates if the style is found more than once in order
  llStyFound = .F.
  STORE 0 TO lnContinue,lnCtnQty,lnCtnWgh,lnI,lnJ,lnStyOrdLin,lnRemPQty,;
    lnTotPck

  loFormSet.llAnyUpd = .F.

  lnAlias = ALIAS()
  loFormSet.AriaForm1.pgfPacking.DETAIL.grdDetail.RECORDSOURCE = ''
  loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonH.RECORDSOURCE = ''
  loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonD.RECORDSOURCE = ''

  *-- Creating the temp. files
  =lfCrTmpFiles(loFormSet)
  lcOrderNo = loFormSet.AriaForm1.kbOrderNo.keytextbox.VALUE
  STORE SPACE(0) TO lcStyle,lcStyDesc

  SELECT Ordline
  gfSetOrder('ORDLINST')
  =gfSEEK('O'+lcOrderNo)
  SCAN REST WHILE CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+lcOrderNo
    llPack = !EMPTY(PACK_ID)
    IF llPack
      *SET ORDER TO 'CSTPKINFP' IN CSTPKINF
      lcSeekKey = 'P'+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION
    ELSE
      *SET ORDER TO 'CSTPKINF' IN CSTPKINF
      lcSeekKey = 'S'+ACCOUNT+STYLE
    ENDIF  
    *=SEEK(lcSeekKey,'CSTPKINF')
    llDefault = .T. &&!SEEK(lcSeekKey,'CSTPKINF')
    IF llPack
      llSeek =gfSEEK('P'+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE,'SPCK_LIN')
      IF !llSEEK
        llSeek =gfSEEK('P*****'+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE,'SPCK_LIN')
      ENDIF
    ENDIF
  && TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE+DYELOT
  =gfSeek(Ordline.Style,'Style')
  =gfSeek('S'+Style.Scale,'Scale')
  *--PikLine file contains the piked line if the order is invoiced
  llFrmPikLn = gfSEEK(ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'PIKLINE') AND PIKLINE.PICKED
  lcSeek = PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE
  llAdd = !SEEK(lcSeek,loFormSet.lcPckLin)
  SELECT (loFormSet.lcPckLin)
  FOR lnIScl = 1 TO SCALE.CNT    
    lcSz = STR(lnIScl,1)
    IF llAdd
      APPEND BLANK   
      REPLACE Style      WITH IIF(llFrmPikLn,PikLine.Style,OrdLine.Style),;             
              Scale      WITH Scale.Scale,;
              OrgStyWgh  WITH Style.nStyWeight,;
              PkStyQty   WITH SPCK_LIN.QTY&lcSz,;
              CtnQty     WITH IIF(llPack , Style.ninpackqty * SPCK_LIN.QTY&lcSz , Style.ninpackqty),; &&mmq
              StyWgh     WITH IIF(llPack , Style.nStyWeight , IIF(!EMPTY(Style.nmspackwgt),Style.nmspackwgt,Style.nStyWeight)),; 
              LPicked    WITH IIF(llFrmPikLn,PikLine.Picked,OrdLine.Picked),;
              cSizeNo    WITH lcSz,;
              cSize      WITH Scale.Sz&lcSz  ,;
              PACK_ID    WITH IIF(llFrmPikLn , PikLine.PACK_ID    , OrdLine.PACK_ID   ),;
              cPkColor   WITH IIF(llFrmPikLn , PikLine.cPkColor   , OrdLine.cPkColor  ),;
              cPckSize   WITH IIF(llFrmPikLn , PikLine.cPckSize   , OrdLine.cPckSize  ),;
              cPkVersion WITH IIF(llFrmPikLn , PikLine.cPkVersion , OrdLine.cPkVersion)

      REPLACE lRange     WITH IIF(llFrmPikLn , PikLine.lRange     , OrdLine.lRange)
      REPLACE DISCTNQTY WITH IIF(lRange,CtnQty,0)	,;
              CtnQty 	WITH IIF(llDefault,STYLE.Qty_Ctn,CtnQty)
      
      llIsPck = !EMPTY(PACK_ID)
    ELSE
      =SEEK(lcSeek+lcSz,loFormSet.lcPckLin)
    ENDIF
    
    REPLACE AvlQty  WITH AvlQty + IIF(llFrmPikLn,PikLine.Pik&lcSz,IIF(!EMPTY(ORDLINE.PIKTKT),;
                                                 OrdLine.Pik&lcSz,OrdLine.Qty&lcSz)),;
            OrdQty  WITH OrdQty + IIF(llFrmPikLn,PikLine.Qty&lcSz,OrdLine.Qty&lcSz),;
            OrgOrd  WITH OrgOrd + IIF(llFrmPikLn,PikLine.Qty&lcSz,OrdLine.Qty&lcSz),;
            OQty    WITH AvlQty  ,;
            OrgPQty WITH OrdLine.nPck&lcSz
  ENDFOR
ENDSCAN 

=lfColSum(loFormSet)
lcPckLin = loFormSet.lcPckLin
lcTmpPck = loFormSet.lcTmpPck
SELECT (lcPckLin)
GO TOP
=RLOCK(lcPckLin) 
UNLOCk IN (lcPckLin)
STORE 1 TO loFormSet.lnFrom,loFormSet.lnTo
IF !USED(lcTmpPck)
  USE (oAriaApplication.WorkDir+lcPckLin)  IN 0 AGAIN ALIAS (lcTmpPck) ORDER (loFormSet.lcPakIndxSt)
ENDIF


*IF loFormSet.ariaForm1.pgfPacking.ACTIVEPAGE = 1
 =lfDtlBrow(loFormSet)
*ENDIF

SELECT(loFormSet.lcCtnHdr)
=RLOCK(loFormSet.lcCtnHdr) 
UNLOCk IN (loFormSet.lcCtnHdr)
GO TOP

SELECT(loFormSet.lcCtnDtl)
UNLOCk IN (loFormSet.lcCtnDtl)

*IF  loFormSet.ariaForm1.pgfPacking.ACTIVEPAGE = 2
  = lfwCtnHdrBr(loFormSet)    
*ENDIF

*IF  loFormSet.ariaForm1.pgfPacking.ACTIVEPAGE = 2
  = lfwCtnDtlBr(loFormSet)
*ENDIF

  *-- Browse for cartons header in the caron information folder
  =lfCtnHdrBr(loFormSet)
  *-- Browse for cartons details in the caron information folder
  =lfCtnDtlBr(loFormSet)

  WAIT CLEAR

  IF loFormSet.ActiveMode $ 'V'
    lfGetTmpData(loFormSet)
    IF  loFormSet.AriaForm1.pgfPacking.ActivePage = 2
       loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonH.refresh 
       loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonH.AfterRowColChange 
    ENDIF   

  ENDIF 

  SELECT (lnAlias)
  *!*************************************************************
  *! Name      : lfCrTmpFiles
  *! Developer : Mariam MAzhar
  *! Date      : 03/01/2009
  *! Purpose   : To create temporary files
  *!*************************************************************
  *! Parameters: loFormSet : ThisFormSet
  *!*************************************************************
FUNCTION lfCrTmpFiles
  LPARAMETERS loFormSet

  =lfCrSelFiles(loFormSet)
  =lfCrCtnFiles(loFormSet)
  =lfCrSelSum(loFormSet)

  *!*************************************************************
  *! Name      : lfCrSelFiles
  *! Developer : Mariam MAzhar
  *! Date      : 03/01/2009
  *! Purpose   : Create the files that are used in Function lfvSelOrd
  *!             (lcPckLin)
  *!*************************************************************
  *! Parameters: loFormSet : ThisFormSet
  *!*************************************************************
FUNCTION lfCrSelFiles
LPARAMETERS loFormSet

PRIVATE lnCurAlias,lnI
lnCurAlias = SELECT(0)

lnI = 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lSelect'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Style'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 19
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Scale'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 3
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgPQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgPWgh'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

*-- The carton No the style applied into
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nCarton'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSizeNo'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PkStyQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'StyWgh'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgStyWgh'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PWgh'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'LPicked'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nStep'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 2
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Selected'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PACK_ID'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 16
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPkVersion'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 4
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPkColor'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPckSize'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 3
laFileStru[lnI,4] = 0

*C122070,1 HBG 04/04/2004 Add new filed to temp detail file to know if this pack is range or not [Begin]
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lRange'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'DisCtnQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nRngCart'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lIgnor'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0
*C122070,1 [End]

*--Indexes
DIMENSION laIndx[5,2]

laIndx[1,1] = "Style"
laIndx[1,2] = loFormSet.lcPckLin


laIndx[2,1] = "IIF(Selected>0,'Y','N')"
laIndx[2,2] = 'Selected'

laIndx[3,1] = "IIF(OQty>0,'Y','N')"
laIndx[3,2] = 'Opened'

laIndx[4,1] = "IIF(PQty=0,'Y','N')"
laIndx[4,2] = 'NoPacked'

laIndx[5,1] = "PACK_ID+cPkColor+cPckSize+cPkVersion+Style+cSizeNo"   
laIndx[5,2] = loFormSet.lcPakIndxSt

=gfCrtTmp(loFormSet.lcPckLin,@laFileStru,@laIndx)
SET ORDER TO (loFormSet.lcPakIndxSt) IN (loFormSet.lcPckLin)
SELECT (lnCurAlias)


  *!*************************************************************
  *! Name      : lfCrSelFiles
  *! Developer : Mariam MAzhar
  *! Date      : 03/01/2009
  *! Purpose   : Create the files that are used in Function lfvSelOrd
  *!             (lcPckLin)
  *!*************************************************************
  *! Parameters: loFormSet : ThisFormSet
  *!*************************************************************
FUNCTION lfCrCtnFiles
LPARAMETERS loFormSet
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)

DIMENSION laFileStru[5,4]

laFileStru[01,1] = 'Cart_No'
laFileStru[01,2] = 'N'
laFileStru[01,3] = 4
laFileStru[01,4] = 0

laFileStru[02,1] = 'Pal_No'
laFileStru[02,2] = 'N'
laFileStru[02,3] = 4
laFileStru[02,4] = 0

laFileStru[03,1] = 'TotPcs'
laFileStru[03,2] = 'N'
laFileStru[03,3] = 7
laFileStru[03,4] = 0

laFileStru[04,1] = 'TotWgh'
laFileStru[04,2] = 'N'
laFileStru[04,3] = 9
laFileStru[04,4] = 2

laFileStru[05,1] = 'Empty'
laFileStru[05,2] = 'C'
laFileStru[05,3] = 1
laFileStru[05,4] = 0

DIMENSION laIndx[2,2]
laIndx[1,1] = "STR(Cart_No,4)+STR(Pal_No,4)"
laIndx[1,2] = loFormSet.lcCtnHdr

laIndx[2,1] = "Empty+STR(Cart_No,4)+STR(Pal_No,4)"
laIndx[2,2] = "EMPTY"

=gfCrtTmp(loFormSet.lcCtnHdr,@laFileStru,@laIndx)           
SET ORDER TO (loFormSet.lcCtnHdr) IN  (loFormSet.lcCtnHdr)

*:***************************************************************************
*:*******************  lcCtnDtl   *******************************************

lnI = 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Cart_No'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 4
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Style'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 19
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size1'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size2'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size3'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size4'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size5'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size6'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size7'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size8'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

*--
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0
*--

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

*-- TotWight
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotWeight'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 2
*-

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nStep'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PackLineNo'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgWgh'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SzCnt'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cStatus'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br1'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br2'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br3'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br4'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br5'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br6'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br7'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br8'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PACK_ID'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 16
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPkColor'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPckSize'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 3
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPkVersion'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 4
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'NPACKNO'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lFlag'
laFileStru[lnI,2] = 'l'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cNoSize'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0


DIMENSION laIndx[2,2]
laIndx[1,1] = "STR(Cart_No,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style" 
laIndx[1,2] = loFormSet.lcCtnDtl
laIndx[2,1] = "cStatus"
laIndx[2,2] = "Status"

=gfCrtTmp(loFormSet.lcCtnDtl,@laFileStru,@laIndx)           
SET ORDER TO (loFormSet.lcCtnDtl) IN  (loFormSet.lcCtnDtl)

SELECT (lnCurAlias)

 *!*************************************************************
  *! Name      : lfCrSelSum
  *! Developer : Mariam MAzhar
  *! Date      : 03/01/2009
  *! Purpose   : Create Temp files
  *!             
  *!*************************************************************
  *! Parameters: loFormSet : ThisFormSet
  *!*************************************************************
FUNCTION lfCrSelSum
LPARAMETERS loFormSet
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)
lnI = 1

DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lSelect'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0


lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 2

*-- Add field for available qty used in browse
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PkCtnQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PTotWgh'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PACK_ID'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 19
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPkVersion'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 4
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPkColor'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPckSize'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 3
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'llPack'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lRange'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nCarton'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cRange'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 3
laFileStru[lnI,4] = 0



DIMENSION laIndx[1,2]
laIndx[1,1] = "PACK_ID+cPkColor+cPckSize+cPkVersion"
laIndx[1,2] = loFormSet.lcSumPck

=gfCrtTmp(loFormSet.lcSumPck,@laFileStru,"PACK_ID+cPkColor+cPckSize+cPkVersion",loFormSet.lcSumPck,.F.)
SET ORDER TO (loFormSet.lcSumPck) IN (loFormSet.lcSumPck)

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfColSum
*! Developer : Mariam Mazhar
*! Date      : 03/01/2009
*! Purpose   : Function To Size discreption
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        : 
*!*************************************************************
*! Example            : =lfColSum()
*!*************************************************************
*
FUNCTION lfColSum
PARAMETERS loFormSet

PRIVATE lnCurAlias
lnCurAlias = SELECT(0)
lcAccount = ALLTRIM(loFormSet.AriaForm1.kbAccount.keytextbox.VALUE)
lcPckLin = loFormSet.lcPckLin
lcSumPck = loFormSet.lcSumPck 
SELECT (loFormSet.lcPckLin)
LOCATE
lcPack_Id  = ''
SCAN
  IF lcPack_Id = IIF(EMPTY(&lcPckLin..Pack_id),STYLE,LEFT(Pack_Id,16)) + cpkcolor + cpcksize + cPkVersion 
    SELECT(lcSumPck) 
    IF !&lcPckLin..LRANGE .OR. (&lcPckLin..LRANGE .AND. &lcSumPck..CtnTotQty = 0)
      REPLACE OTotQty      WITH OTotQty   + &lcPckLin..OQty,;
              CtnTotQty    WITH CtnTotQty + IIF(&lcPckLin..OQty>0,&lcPckLin..CtnQty,0),;
              PkCtnQty     WITH IIF(!EMPTY(&lcPckLin..Pack_id),CtnTotQty,0) 
    ENDIF
    *-- Update AvlQty field in Summary file
    REPLACE AvlQty WITH AvlQty + &lcPckLin..AvlQty

    IF !EMPTY(&lcPckLin..Pack_id)
      REPLACE PTotWgh WITH PTotWgh + &lcPckLin..StyWgh
    ENDIF
  ELSE
    SELECT(lcSumPck)  
    APPEND BLANK
    REPLACE OTotQty      WITH &lcPckLin..OQty,;
            CtnTotQty    WITH IIF(&lcPckLin..OQty>0,&lcPckLin..CtnQty,0),;
            PkCtnQty     WITH IIF(!EMPTY(&lcPckLin..Pack_id),CtnTotQty,0)    ,;
            PTotWgh      WITH &lcPckLin..StyWgh,;
            cPkVersion   WITH &lcPckLin..cPkVersion,; 
            cPkColor     WITH &lcPckLin..cPkColor,;
            cPckSize     WITH &lcPckLin..cPckSize
           
    *--  Update AvlQty field in Summary file
    REPLACE AvlQty WITH &lcPckLin..AvlQty
    
    IF EMPTY(&lcPckLin..Pack_id)
      REPLACE PACK_ID  WITH &lcPckLin..Style
    ELSE
      llGetPack  = gfSEEK('P'+lcAccount +PADR(&lcPckLin..Pack_Id,16)+&lcPckLin..cPkColor+;
                        &lcPckLin..cPckSize+&lcPckLin..cPkVersion,'SPCK_HDR')
      IF !llGetPack 
        = gfSEEK('P*****'+PADR(&lcPckLin..Pack_Id,16)+&lcPckLin..cPkColor+;
                        &lcPckLin..cPckSize+&lcPckLin..cPkVersion,'SPCK_HDR')
      ENDIF 
      REPLACE PACK_ID      WITH &lcPckLin..PACK_ID,;
              llPack       WITH .T.,;
              lRange       WITH &lcPckLin..lRange,;
              cRange       WITH IIF(lRange,'Yes','No')
    ENDIF       
    lcPack_Id = LEFT(Pack_Id,IIF(EMPTY(&lcPckLin..Pack_id),19,16)) + cpkcolor + cpcksize + cPkVersion             
  ENDIF 
ENDSCAN
llApply = .F.
=lfUpdSumFl(loFormSet)
SELECT (lnCurAlias)
*!*************************************************************
*! Name      : lfUpdSumFl
*! Developer : Mariam Mazhar
*! Date      : 03/01/2009
*! Purpose   : Function To validate Key
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        : 
*!*************************************************************
*! Example            : =lfUpdSumFl()
*!*************************************************************

FUNCTION lfUpdSumFl
PARAMETERS loFormSet
PRIVATE lnPkWght
lcSumPck = loFormSet.lcSumPck
lcAccount = ALLTRIM(loFormSet.AriaForm1.kbAccount.keytextbox.VALUE)
lcCurAlis = ALIAS()
SELECT(lcSumPck) 
lcExpr = IIF(llApply,"lSelect = .T.",".T.")
SCAN FOR &lcExpr 
  IF &lcSumPck..llPack AND !(&lcSumPck..lRange)
    llGetPack  =gfSEEK('P'+lcAccount +PADR(Pack_Id,16)+cPkColor+cPckSize+cPkVersion,'SPCK_HDR')
    IF !llGetPack 
      = gfSEEK('P*****'+PADR(Pack_Id,16)+cPkColor+cPckSize+cPkVersion,'SPCK_HDR')
    ENDIF 
    IF !EOF('SPCK_HDR')
      =lfPackQty()                && Update nPckQty field if it 0
      lnOqty = &lcSumPck..OTotQty / SPCK_HDR.npckqty
      lnPqty = &lcSumPck..PTotQty / SPCK_HDR.npckqty
      lnPkWght = lfPackWght()
      REPLACE OTotQty WITH lnOqty,;
              PTotQty WITH lnPqty,;
              PkCtnQty WITH PkCtnQty / SPCK_HDR.npckqty
    ENDIF

  ENDIF          
ENDSCAN
GO TOP  
SELECT (lcCurAlis)

*:**************************************************************************
*:* Name        : lfPackWght                                      
*! Developer : Mariam Mazhar
*! Date      : 03/01/2009
*:* Purpose     : Get pack weight from SPCK_LIN file
*:***************************************************************************
FUNCTION lfPackWght
PRIVATE lnSlct,lnWght
lnSlct = SELECT()
lnWght = 0

SELECT SPCK_HDR
IF gfSEEK('P'+ACCOUNT+Pack_Id+cPkColor+cPckSize+cPkVersion,'SPCK_LIN')
  SELECT SPCK_LIN
  SCAN REST WHILE TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE = ;
                  'P'+SPCK_HDR.ACCOUNT+SPCK_HDR.Pack_Id+SPCK_HDR.cPkColor+SPCK_HDR.cPckSize+SPCK_HDR.cPkVersion
    =gfSEEK(SPCK_LIN.STYLE,'STYLE')
    lnWght = lnWght + SPCK_LIN.TOTQTY * STYLE.NSTYWEIGHT
  ENDSCAN
ENDIF
SELECT (lnSlct)
RETURN lnWght
*-- end of lfPackWght.

  *!*************************************************************
  *! Name      : lfDtlBrow
*! Developer : Mariam Mazhar
*! Date      : 03/01/2009
  *! Purpose   : Browse lcPckLin file
  *!*************************************************************
  *! Parameters: loFormSet : FormSet
  *!*************************************************************
FUNCTION lfDtlBrow
LPARAMETERS loFormSet
PRIVATE lnCurAlias
lnCurAlias = SELECT(0)

IF loFormSet.llShoPckSu
  SELECT (loFormSet.lcPckLin)
  IF RECCOUNT() = 0
    RETURN
  ENDIF
  SET ORDER TO (loFormSet.lcPakIndxSt)
ENDIF


lcTemFile = IIF(!loFormSet.llShoPckSu,loFormSet.lcSumPck,loFormSet.lcPckLin)
lcTmpPck = loFormSet.lcTmpPck



WITH loFormSet.AriaForm1.pgfPacking.DETAIL.grdDetail
  .RECORDSOURCE = ''
  IF loFormSet.llShoPckSu
    SELECT (loFormSet.lcTmpPck)
	  SET FILTER TO
	  IF loFormSet.llShwOpn
	    SET FILTER TO AvlQty > 0
	  ELSE
	    SET FILTER TO
	  ENDIF
	  LOCATE
	  IF EOF()
	    RETURN
	  ENDIF
    
    .RECORDSOURCE = loFormSet.lcTmpPck
     *-- Select
    .Column1.Header1.CAPTION = ""
    .Column1.CONTROLSOURCE  = loFormSet.lcTmpPck +'.lSelect'
      *-- Pack-Id
    .Column2.CONTROLSOURCE  = 'ThisFormset.lfGetDetPack()'
    .Column2.Header1.caption = 'Pack_Id-Color-Size-Version'
    .Column2.VISIBLE = .T.
     *-- Style
  
    .Column3.CONTROLSOURCE  = loFormSet.lcTmpPck + '.Style'
    .Column3.Header1.CAPTION = loFormSet.lcStyTtl
    .Column3.VISIBLE = .T.
        
    *-- Size
    .Column4.visible  = .F.
    .Column5.CONTROLSOURCE  = loFormSet.lcTmpPck+'.cSize'
    .Column5.Header1.caption = 'Size'
    
    *-- O.Qty
    .Column6.CONTROLSOURCE = loFormSet.lcTmpPck+'.OQty'
    .Column6.Header1.ALIGNMENT = 1     && Right

    *-- Qty.\Ctn
    IF  EVAL(lcTmpPck+'.LRange')
      .Column7.CONTROLSOURCE = loFormSet.lcTmpPck+'.DisCtnQty'
      .Column7.Header1.caption = 'Qty.\Ctn'
      .Column7.Header1.ALIGNMENT = 1     && Right
      
    ELSE
      .Column7.CONTROLSOURCE = loFormSet.lcTmpPck+'.CtnQty'
      .Column7.Header1.caption = 'Qty.\Ctn(Pcs)'
      .Column7.Header1.ALIGNMENT = 1     && Right
     
    ENDIF   
    BINDEVENT(.Column7.Text1,"LostFocus",loFormSet,'lfvQTYCRN')

    *-- Wgh.\Unt
    .Column8.CONTROLSOURCE = loFormSet.lcTmpPck+'.StyWgh'
    .Column8.Header1.ALIGNMENT = 1     && Right
    .Column8.Header1.CAPTION = 'Wgh.\Unt'
    BINDEVENT(.Column8.Text1,"LostFocus",loFormSet,'lfvWeight')
    
    *-- PackQty
    .Column9.CONTROLSOURCE = loFormSet.lcTmpPck+'.nCarton'
    .Column9.Header1.CAPTION ='Carton#'
    .Column9.Header1.ALIGNMENT = 1     && Right

    *-- PackWght
    *.Column10.CONTROLSOURCE = loFormSet.lcPckLin +'.PWgh'
    .Column10.Header1.ALIGNMENT = 1     && Right
    .Column10.Visible = .F.
  ELSE
    SELECT (loFormSet.lcSumPck)
    IF loFormSet.llShwOpn
      SET FILTER TO AvlQty > 0
    ELSE
      SET FILTER TO
    ENDIF

    LOCATE
    IF EOF()
      RETURN
    ENDIF

    LOCATE
    .RECORDSOURCE = loFormSet.lcSumPck
    *-- Select
    .Column1.Header1.CAPTION = ""
    .Column1.CONTROLSOURCE  = loFormSet.lcSumPck +'.lSelect'
    *-- Pack-Id
    .Column2.CONTROLSOURCE  = 'ThisFormset.lfGetPack()'
    
    *-- Style
    .Column3.VISIBLE = .F.

    *-- Dyelot
    .Column4.VISIBLE = .F.

    *-- Size
    .Column5.CONTROLSOURCE  = loFormSet.lcSumPck +'.cRange'
    .Column5.Header1.caption ='Range'
    *-- O.Qty
    .Column6.CONTROLSOURCE = loFormSet.lcSumPck +'.OTotQty'
    .Column6.Header1.ALIGNMENT = 1     && Right

    *-- Qty.\Ctn
    .Column7.CONTROLSOURCE = loFormSet.lcSumPck +'.PkCtnQty'
    .Column7.Header1.caption = 'Qty.\Ctn(Pks)'
    .Column7.Header1.ALIGNMENT = 1     && Right

    *-- Wgh.\Unt
    .Column8.CONTROLSOURCE = loFormSet.lcSumPck +'.CtnTotQty'
    .Column8.Header1.caption =  'Qty.\Ctn(Pcs)'
    .Column8.Header1.ALIGNMENT = 1     && Right

    *-- PackQty
    .Column9.CONTROLSOURCE = loFormSet.lcSumPck +'.PTotWgh'
    .Column9.Header1.caption =  'Wgh.\Unt'
    .Column9.Header1.ALIGNMENT = 1     && Right

    *-- PackWght
    .Column10.CONTROLSOURCE = loFormSet.lcSumPck +'.nCarton'
    .Column10.Header1.caption =  'Carton#' 
    .Column10.Header1.ALIGNMENT = 1    && Right

  ENDIF
  .SETALL('ReadOnly',.T.,'COLUMN')
  
  IF loFormSet.llShoPckSu AND loFormSet.ActiveMode $ 'AE'
    .Column1.READONLY = .F.
    .Column7.readonly = .F.
    .Column8.readonly = .F.
  ENDIF   
  .REFRESH()
  ENDWITH
  =lfwDtlBrow(loFormSet)

  *!*************************************************************
  *! Name      : lfShowDtSum
  *! Developer : Mariam Mazhar
  *! Date      : 03/01/2009
  *! Purpose   : Show pack detail/summary from option menu
  *!*************************************************************
  *! Parameters: loFormSet : FormSet
  *!*************************************************************
FUNCTION lfShowDtSum
LPARAMETERS loFormSet,lnSumPack
LOCAL lcAlias,lcDataSes,lcExpr
IF lnSumPack =1 
  loFormSet.llShoPckSu = .T.
ELSE
  loFormSet.llShoPckSu = .F.
ENDIF 
=lfDtlBrow(loFormSet)
=lfwDtlBrow(loFormSet)

  *!*************************************************************
  *! Name      : lfShwOpn
  *! Developer : Mariam Mazhar
  *! Date      : 03/01/2009
  *! Purpose   : Show Open qty
  *!*************************************************************
  *! Parameters: loFormSet : FormSet
  *!*************************************************************
FUNCTION lfShwOpn
PARAMETERS  loFormSet
PRIVATE lnCurAlias,lcTag, lcDataSes
lcDataSes = SET("Datasession")
SET DATASESSION TO loFormSet.DATASESSIONID
lnCurAlias = SELECT(0)

*-- this to show only opened quantity or all quantity according to
*-- variable llShwOpn

SELECT (loFormSet.lcPckLin)
IF !loFormSet.llShwOpn
  loFormSet.llShwOpn = .T.
  SET MARK OF BAR 1 OF _OPTPOP TO .T.
ELSE
  loFormSet.llShwOpn = .F.
  SET MARK OF BAR 1 OF _OPTPOP TO .F.
ENDIF

SELECT (loFormSet.lcPckLin)
lcTag = ORDER(loFormSet.lcPckLin)
SET ORDER TO OPENED IN (loFormSet.lcPckLin)

IF SEEK('Y',loFormSet.lcPckLin)
  loFormSet.llAnyRec = .T.
ELSE
  loFormSet.llAnyRec = .F.
ENDIF
SET ORDER TO lcTag IN (loFormSet.lcPckLin)

=lfDtlBrow(loFormSet)

SELECT(lnCurAlias)
SET DATASESSION TO &lcDataSes


 *!*************************************************************
  *! Name      : lfwDtlBrow
  *! Developer : Mariam Mazhar
  *! Date      : 03/01/2009
  *! Purpose   : Adjust the label of pbsel button
  *!*************************************************************
  *! Parameters: loFormSet : FormSet
  *!*************************************************************
FUNCTION lfwDtlBrow
  LPARAMETERS loFormSet
  PRIVATE lnAlias,lnRecNo,lnSlected

  IF loFormSet.ActiveMode = 'S'
    RETURN 
  eNdif 


  lnAlias   = SELECT(0)


  lcTemFile = IIF(!loFormSet.llShoPckSu,loFormSet.lcSumPck,loFormSet.lcTmpPck)
  SELECT (lcTemFile)
  lnRecNo = RECNO()
  lnSlected = 0
  COUNT FOR lSelect TO lnSlected
  COUNT FOR !lSelect TO lnNtSlected
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GOTO lnRecNo
  ENDIF

  IF loFormSet.llShoPckSu
    loFormSet.lnBrCtnQty = EVALUATE(loFormSet.lcTmpPck+'.CtnQty')
    loFormSet.lnBrUntWgh = EVALUATE(loFormSet.lcTmpPck+'.StyWgh')
    
    loFormSet.ariaForm1.pgfPacking.DETAIL.cmdSelect.CAPTION = ;
      IIF(EVALUATE(loFormSet.lcTmpPck+'.lSelect'),LANG_ManulPL_CptUnSel,LANG_ManulPL_CptThis)
      
    loFormSet.ariaForm1.pgfPacking.DETAIL.cmdPack.CAPTION = ;
      IIF(!EMPTY(EVALUATE(loFormSet.lcTmpPck+'.Pack_id')),IIF(EVALUATE(loFormSet.lcTmpPck+'.Selected')>0,LANG_ManulPL_CptUnSelP,LANG_ManulPL_CptPack),LANG_ManulPL_CptPack)
      
    loFormSet.ariaForm1.pgfPacking.DETAIL.cmdStyle.CAPTION = ;
      IIF(EMPTY(EVALUATE(loFormSet.lcTmpPck+'.Pack_id')),IIF(EVALUATE(loFormSet.lcTmpPck+'.Selected')>0,LANG_ManulPL_CptUnSelS,LANG_ManulPL_CptStyle),LANG_ManulPL_CptStyle)
  ELSE
    loFormSet.lnBrCtnQty = EVALUATE(loFormSet.lcSumPck+'.CtnTotQty')
    *loFormSet.lnBrUntWgh = EVALUATE(loFormSet.lcSumPck+'.StyWgh')
    loFormSet.ariaForm1.pgfPacking.DETAIL.cmdSelect.CAPTION = ;
      IIF(EVALUATE(loFormSet.lcSumPck +'.lSelect' ),LANG_ManulPL_CptUnSel,LANG_ManulPL_CptThis)
    loFormSet.ariaForm1.pgfPacking.DETAIL.cmdPack.CAPTION = ;
      IIF(!EMPTY(EVALUATE(loFormSet.lcSumPck +'.Pack_id')),IIF(EVALUATE(loFormSet.lcSumPck +'.lSelect'),LANG_ManulPL_CptUnSelP,LANG_ManulPL_CptPack),LANG_ManulPL_CptPack)
    loFormSet.ariaForm1.pgfPacking.DETAIL.cmdStyle.CAPTION = ;
      IIF(!EMPTY(EVALUATE(loFormSet.lcSumPck +'.Pack_id')),IIF(EVALUATE(loFormSet.lcSumPck +'.lSelect'),LANG_ManulPL_CptUnSelS,LANG_ManulPL_CptStyle),LANG_ManulPL_CptStyle)
  ENDIF


  =lfSelStatus(loFormSet)
*!*    IF loFormSet.llQtyArr OR loFormSet.llWghArr
*!*      CLEAR TYPEAHEAD
*!*      KEYBOARD "{RIGHTARROW}{LEFTARROW}" CLEAR PLAIN
*!*      DO CASE
*!*        CASE loFormSet.llQtyArr
*!*          loFormSet.ariaForm1.pgfPacking.DETAIL.txtQtyPerCart.SETFOCUS
*!*        CASE loFormSet.llWghArr
*!*          loFormSet.ariaForm1.pgfPacking.DETAIL.txtUnitWeight.SETFOCUS
*!*      ENDCASE
*!*      STORE .F. TO loFormSet.llQtyArr,loFormSet.llWghArr
*!*    ENDIF

  *-- to control showing of Select by pack
  lcOrdNo = loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE
  WITH loFormSet.ariaForm1.pgfPacking.DETAIL
    IF loFormSet.llShoPckSu
      .cmdStyle.ENABLED      = IIF(loFormSet.ActiveMode = 'V',.F.,IIF(EMPTY(lcOrdNo),.F.,IIF(EMPTY(EVALUATE(loFormSet.lcTmpPck+'.Pack_ID')),.T.,.F.)))
      .cmdPack.ENABLED       = IIF(loFormSet.ActiveMode = 'V',.F.,IIF(EMPTY(lcOrdNo),.F.,IIF(EMPTY(EVALUATE(loFormSet.lcTmpPck+'.Pack_ID')),.F.,.T.)))
    ELSE
      .cmdPack.ENABLED  = .F.
      .cmdStyle.ENABLED = IIF(loFormSet.ActiveMode = 'V',.F.,;
        IIF(EMPTY(lcOrdNo ),.F.,IIF(EVALUATE(loFormSet.lcSumPck+'.llPack'),.F.,.T.)))
    ENDIF
    STORE IIF(loFormSet.ActiveMode = 'V',.F.,(lnSlected <> 0)) TO .cmdApply.ENABLED,.cmdSelNone.ENABLED
    STORE IIF(loFormSet.ActiveMode = 'V',.F.,(lnNtSlected <> 0)) TO .cmdSelAll.ENABLED
    STORE IIF(loFormSet.ActiveMode $ 'SV',.F.,.T.) TO  .cmdSelect.ENABLED,.cmdInvert.ENABLED
    
  ENDWITH
  loFormSet.lnCtnPal  = EVALUATE(loFormSet.lcCtnHdr+'.Pal_No')

  SELECT(lnAlias)
  *!*************************************************************
  *! Name      : lfSelStatus
  *! Developer : Mariam Mazhar
  *! Date      : 03/01/2009
  *! Purpose   : adjust the status of selection buttons
  *!*************************************************************
  *! Calls     :
  *!             Procedures : ...
  *!             Functions  : lfSelStatus,lfDtlBrow
  *!*************************************************************
  *! Passed Parameters  : None
  *!*************************************************************
  *! Returns            : None
  *!*************************************************************
  *! Example   : = lfSelStatus()
  *!*************************************************************

FUNCTION lfSelStatus
  LPARAMETERS loFormSet

  PRIVATE lnCurAlias,lnCurRec

  lnCurAlias = SELECT(0)

  IF !loFormSet.llAnyRec
    llDtl2Stat = .F.
  ELSE
    llDtl2Stat = IIF((loFormSet.ActiveMode $ 'AE' OR loFormSet.llNew) AND loFormSet.ariaForm1.pgfPacking.ACTIVEPAGE = 1,.T.,.F.)
  ENDIF

  WITH loFormSet.ariaForm1.pgfPacking.DETAIL
    STORE llDtl2Stat TO  .cmdStyle.ENABLED , .cmdSelect.ENABLED ,;
      .cmdInvert.ENABLED , .cmdSelAll.ENABLED , .cmdSelNone.ENABLED , .cmdApply.ENABLED,.cmdPack.ENABLED
      .cmdStyle.ENABLED = llDtl2Stat
  ENDWITH
  SELECT (lnCurAlias)
  
    *!*************************************************************
  *! Name      : lfwCtnDtlBr
  *! Developer : Mariam MAzhar
  *! Date      : 03/01/2009
  *! Purpose   : To refresh the edit rigion for carton details according to
  *!             lcCtnDtl file
  *!*************************************************************
  *! Parameters: loFormSet : FormSet
  *!*************************************************************
FUNCTION lfwCtnDtlBr
  LPARAMETERS loFormSet
  LOCAL lnAlias
  lnAlias = SELECT(0)
  lcCtnDtl =loFormSet.lcCtnDtl
  SELECT (loFormSet.lcCtnDtl)
  WITH loFormSet.AriaForm1.pgfPacking.CartonInfo
    .kbStyle.REFRESH
    .kbSize.REFRESH
    .kbStyle.value       = &lcCtnDtl..Style
    
    lcSzNum = &lcCtnDtl..cNoSize
    IF !EMPTY(&lcCtnDtl..cNoSize)
      .kbSize.value       = &lcCtnDtl..Size&lcSzNum.
      
      .txtCartQtyD.VALUE  = &lcCtnDtl..Qty&lcSzNum.
      
      .txtCartWghD.VALUE  = &lcCtnDtl..Weight&lcSzNum.
      
    ENDIF 
    
    IF loFormSet.ActiveMode $ 'AE' &&OR loFormSet.llNew
      IF EVAL(loFormSet.lcCtnDtl+'.Cart_No') = 0 AND EMPTY(EVALUATE(loFormSet.lcCtnDtl+'.Style'))
        STORE .F. TO .cmdRemoveCartD.ENABLED,.kbStyle.ENABLED,.kbSize.ENABLED,.txtCartQtyD.ENABLED,.txtCartWghD.ENABLED
      ELSE
        STORE .T. TO .cmdRemoveCartD.ENABLED
        IF EMPTY(EVALUATE(loFormSet.lcCtnDtl+'.Style')) &&OR EMPTY(EVALUATE(loFormSet.lcCtnDtl+'.Size'))
          STORE .T. TO .kbStyle.ENABLED
          STORE EMPTY(Pack_id) TO .txtCartQtyD.ENABLED,.txtCartWghD.ENABLED
          STORE .F. TO .kbSize.ENABLED
        ELSE
          STORE .F. TO .kbStyle.ENABLED,.kbSize.ENABLED
          STORE .F. TO .txtCartQtyD.ENABLED,.txtCartWghD.ENABLED
        ENDIF
      ENDIF
    ELSE
      STORE .F. TO .cmdRemoveCartD.ENABLED,.kbStyle.ENABLED,.kbSize.ENABLED,.txtCartQtyD.ENABLED,.txtCartWghD.ENABLED
    ENDIF
    .txtCartQtyD.Enabled = .F.  
    .txtCartWghD.Enabled = .F.
    .kbSize.Enabled = .F.
    .kbStyle.Enabled = .F.
  ENDWITH
  SELECT(lnAlias)

  *!*************************************************************
  *! Name      : lfwCtnHdrBr
  *! Developer : Mariam MAzhar
  *! Date      : 03/01/2009
  *! Purpose   : refresh the edit rigion for carton header according to
  *!             lcCtnHdr file
  *!*************************************************************
  *! Parameters: loFormSet : FormSet
  *!*************************************************************
FUNCTION lfwCtnHdrBr
  LPARAMETERS loFormSet
  LOCAL lnAlias
  lnAlias = SELECT(0)
  lcCtnHdr =loFormSet.lcCtnHdr
  
  SELECT (loFormSet.lcCtnDtl)
  SET ORDER to (loFormSet.lcCtnDtl)
  SET KEY TO STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4)
  =SEEK(STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4))
  loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonD.REFRESH
  SELECT (loFormSet.lcCtnHdr)
  WITH loFormSet.AriaForm1.pgfPacking.CartonInfo
    .txtCartonNo.REFRESH
    .txtTotWghH.REFRESH
    .txtCartonNo.Value = &lcCtnHdr..Cart_No
    .txtTotWghH.Value = &lcCtnHdr..TotWgh
    IF loFormSet.ActiveMode $ 'AE' &&OR loFormSet.llNew
      .cmdNewCartH.ENABLED = .T.
      IF EOF(loFormSet.lcCtnHdr)
        STORE 0   TO .txtCartonNo.VALUE , .txtTotWghH.VALUE 
        STORE .F. TO .txtCartonNo.ENABLED, .txtTotWghH.ENABLED,;
           .cmdNewCartD.ENABLED,.cmdRemoveCartH.ENABLED
      ELSE
        .cmdRemoveCartH.ENABLED = .T.
        .txtCartonNo.ENABLED = IIF(!EMPTY(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No')),.F.,.T.)
        .txtTotWghH.ENABLED = .F.
        .cmdNewCartD.EnableD = .t.
      ENDIF
    ELSE
      STORE .F. TO .txtCartonNo.ENABLED, .txtTotWghH.ENABLED, ;
        .cmdNewCartH.ENABLED,.cmdRemoveCartH.ENABLED,;
        .cmdNewCartD.ENABLED
    ENDIF
  ENDWITH
*  loFormset.lnCtnPal  = &lcCtnHdr..Pal_No
  =lfwCtnDtlBr(loFormSet)

  SELECT(lnAlias)
*!*************************************************************
*! Name      : lfvSel
*! Developer : Mariam MAzhar
*! Date      : 03/01/2009
*! Purpose   : Valid function for select button
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!         lcOption  : To indicate the selected button
*!*************************************************************
FUNCTION lfvSel
  LPARAMETERS loFormSet, lcOption
  
*!*    PRIVATE lnCurRec,lcUnSelSty,lcSelFld,lcCtnQtyFld,lnSizeRec
*!*    LOCAL lcPkTktNo, lcExpr,lcExpr2, lnCrtPck, lnPackNo, lnTmpRec, lnCurRec

*!*    lnCurAlias = SELECT(0)
*!*    SELECT (loFormSet.lcPckLin)
*!*    lnTmpRec = RECNO(loFormSet.lcPckLin)
*!*    lnCurRec = RECNO(loFormSet.lcSumPck)
*!*    IF loFormSet.llShoPckSu
*!*      SELECT (loFormSet.lcPckLin)
*!*    ELSE
*!*      SELECT (loFormSet.lcSumPck)
*!*    ENDIF
*!*    SET RELATION TO
*!*    DO CASE
*!*        *-- Select Button
*!*      CASE lcOption = 'S'
*!*        IF loFormSet.llShoPckSu
*!*          IF !EMPTY(Pack_Id + cpkcolor + cpcksize + cPkVersion)
*!*            =lfSelPckLn(loFormSet,Pack_Id+cpkcolor+cpcksize+cPkVersion,.T., .T.,;
*!*              Pack_Id +'   '+ cpkColor + cPckSize + cPkVersion,lcOption)
*!*          ELSE
*!*            lcExpr  = STYLE
*!*            =lfSelPckLn(loFormSet,lcExpr,.F., .T.,lcExpr,lcOption)
*!*          ENDIF
*!*        ELSE
*!*          SELECT (loFormSet.lcSumPck)
*!*          lcExpr   = IIF(llPack,Pack_ID+cPkColor+cPckSize+cPkVersion,;
*!*            Pack_ID+STR(nOrdLineNo,6)+cSze)
*!*          lcExpr2  = IIF(llPAck,LEFT(Pack_ID,16)+cPkColor+cPckSize+cPkVersion, Pack_ID)
*!*          =lfSelPckLn(loFormSet,lcExpr,llPack, .F.,.F.,lcOption)
*!*          SELECT(loFormSet.lcPckLin)
*!*        ENDIF

*!*        *-- Select None
*!*      CASE lcOption = 'N'
*!*        lcCurrAls = ALIAS()
*!*        SELECT(loFormSet.lcPckLin)
*!*        REPLACE ALL lSelect   WITH .F.,;
*!*          SELECTED  WITH 0,;
*!*          CtnQty    WITH 0 ,;
*!*          CtnTotQty WITH 0,;
*!*          StyWgh    WITH OrgStyWgh

*!*        SELECT(loFormSet.lcSumPck)
*!*        REPLACE ALL lSelect   WITH .F.,;
*!*          lSelected WITH .F.,;
*!*          CtnTotQty WITH 0
*!*        SELECT(lcCurrAls)

*!*        *-- Invert or Select All
*!*      CASE lcOption $ 'IA'
*!*        SELECT (loFormSet.lcSumPck)
*!*        USE (oAriaApplication.WorkDir+loFormSet.lcSumPck) IN 0 AGAIN ;
*!*          ALIAS (loFormSet.lcTempCur)
*!*        SELECT (loFormSet.lcTempCur)
*!*        LOCATE
*!*        SCAN
*!*          lcExpr   = IIF(llPack,Pack_ID+cPkColor+cPckSize+cPkVersion,;
*!*            Pack_ID)
*!*          lcExpr2  = IIF(llPAck,LEFT(Pack_ID,16)+cPkColor+cPckSize+cPkVersion, Pack_ID)
*!*          =lfSelPckLn(loFormSet,lcExpr,llPack, .F.,.F.,lcOption)
*!*        ENDSCAN
*!*        USE IN (loFormSet.lcTempCur)
*!*        ERASE oAriaApplication.WorkDir+loFormSet.lcTempCur
*!*        IF loFormSet.llShoPckSu
*!*          SELECT (loFormSet.lcPckLin)
*!*        ELSE
*!*          SELECT (loFormSet.lcSumPck)
*!*        ENDIF
*!*        LOCATE

*!*        *-- Style
*!*      CASE lcOption = 'T'
*!*        IF !loFormSet.llShoPckSu
*!*          SELECT (loFormSet.lcSumPck)
*!*          lcExpr   = IIF(llPack,Pack_ID+cPkColor+cPckSize+cPkVersion,;
*!*            Pack_ID)
*!*          lcExpr2  = IIF(llPAck,LEFT(Pack_ID,16)+cPkColor+cPckSize+cPkVersion, Pack_ID)
*!*          =lfSelPckLn(loFormSet,lcExpr,llPack, .F.,lcExpr2,lcOption)
*!*        ELSE
*!*          IF !EMPTY(Pack_Id + cpkcolor + cpcksize + cPkVersion)
*!*            =lfSelPckLn(loFormSet,Pack_Id+cpkcolor+cpcksize+cPkVersion,.T., .T.,;
*!*              Pack_Id +'   '+ cpkColor + cPckSize + cPkVersion,lcOption)
*!*          ELSE
*!*            SELECT (loFormSet.lcPckLin)
*!*            lcExpr  = STYLE
*!*            =lfSelPckLn(loFormSet,lcExpr,.F., .T.,lcExpr,lcOption)
*!*          ENDIF
*!*        ENDIF

*!*        *-- Pack
*!*      CASE lcOption = 'P'
*!*        =lfSelPckLn(loFormSet,Pack_Id+cpkcolor+cpcksize+cPkVersion,.T., .T.,;
*!*          Pack_Id +'   '+ cpkColor + cPckSize + cPkVersion,lcOption)
*!*    ENDCASE

*!*    SELECT (loFormSet.lcPckLin)
*!*    =RLOCK(loFormSet.lcPckLin)
*!*    UNLOCK IN (loFormSet.lcPckLin)

*!*    IF BETWEEN(lnTmpRec,1,RECCOUNT())
*!*      GOTO lnTmpRec
*!*    ENDIF

*!*    SELECT (loFormSet.lcSumPck)
*!*    IF BETWEEN(lnCurRec,1,RECCOUNT())
*!*      GOTO lnCurRec
*!*    ENDIF

*!*    SELECT(lnCurAlias)

*!*    =lfwDtlBrow(loFormSet)

PRIVATE lnRecno,lnRecno2,lcUnSelSty,lcSelFld,lcCtnQtyFld,lnSizeRec,lcStyMjr,lcSeek,;
        lcTmPckOrd
lcTmpPck = loFormSet.lcTmpPck        
lcPakIndxSt = loFormSet.lcPakIndxSt
lcSumPck = loFormSet.lcSumPck
PRIVATE lcSvOrd
lcSvOrd = ORDER('ORDLINE')
lcTmPckOrd = ORDER(lcTmpPck)
SELECT Ordline
=gfSetOrder('ORDLINE')
SET ORDER TO &lcPakIndxSt IN (lcTmpPck)    && "PACK_ID+cPkColor+cPckSize+cPkVersion+Style+cSizeNo"   

lnCurAlias = SELECT(0)
IF ALIAS() = lcSumPck
  IF llPack
    =SEEK(PADR(PACK_ID,16)+cPkColor+cPckSize+cPkVersion,lcTmpPck)
  ELSE
    =SEEK(PADR('',29)+PACK_ID,lcTmpPck)
  ENDIF
ENDIF


SELECT (lcTmpPck)
lnRecno  = RECNO(lcTmpPck)
lnRecno2 = RECNO(lcSumPck)

*lnSizeRec = VAL(&lcTmpPck..cSizeNo)
lcSelFld    = &lcTmpPck..lSelect
lcCtnQtyFld = &lcTmpPck..CtnQty
lcOQtyFld   = &lcTmpPck..OQty

llcUpate = .T.
SET RELATION TO
DO CASE
  
  CASE lcOption = 'S'
    IF !loFormSet.llShoPckSu
      SELECT (lcSumPck)
      lcPack_Id = IIF(&lcSumPck..llPack , LEFT(&lcSumPck..Pack_Id,16),SPACE(16)) + ;
                  &lcSumPck..cpkcolor + &lcSumPck..cpcksize + &lcSumPck..cPkVersion +;
                  IIF(&lcSumPck..llPack,'',&lcSumPck..Pack_Id) 
      SET ORDER TO TAG (lcPakIndxSt) IN (lcTmpPck)
      SELECT (lcTmpPck)
      LOCATE
      =SEEK(lcPack_Id)
      SCAN REST WHILE PACK_ID+cPkColor+cPckSize+cPkVersion+Style+cSizeNo = lcPack_Id ;
                FOR nCarton = 0
        REPLACE lSelect  WITH IIF(lSelect ,.F.,IIF(AvlQty>0 , .T. , .F.)),;
                Selected WITH IIF(lSelect , 1 , 0 )

        SELECT (lcSumPck)
        *-- Update as lcTmpPck
        REPLACE &lcSumPck..lSelect   WITH IIF(&lcTmpPck..lSelect,.T.,.F.)
      ENDSCAN  
    ELSE
      IF !EMPTY(PACK_ID) 
         =gfModalGen('INM00000B00000',.F.,.F.,.F.,'This style is included in a pack , can not be selected individually.')
         REPLACE lSelect  WITH .F. IN (lcTmpPck)
      ELSE
        SELECT (lcTmpPck)
        REPLACE lSelect  WITH IIF(lSelect ,.F.,IIF(AvlQty>0 , .T. , .F.)),;
                Selected WITH IIF(lSelect, 1,0)
          IF SEEK(STYLE + cpkcolor + cpcksize + cPkVersion ,lcSumPck) 
            SELECT &lcSumPck
            REPLACE &lcSumPck..lSelect WITH &lcTmpPck..lSelect
          ENDIF
     ENDIF      

*!*        SELECT (lcSumPck)
*!*        lcPack_Id = IIF(&lcSumPck..llPack , LEFT(&lcSumPck..Pack_Id,16),SPACE(16)) + ;
*!*                    &lcSumPck..cpkcolor + &lcSumPck..cpcksize + &lcSumPck..cPkVersion +;
*!*                    IIF(&lcSumPck..llPack,'',&lcSumPck..Pack_Id) 
*!*        SET ORDER TO TAG (lcPakIndxSt) IN (lcTmpPck)                  
*!*        SELECT (lcTmpPck)
*!*        =SEEK(lcPack_Id)
*!*        REPLACE lSelect  WITH IIF(AvlQty>0 , .T. , .F.) ,;
*!*                Selected WITH IIF(lSelect , 1 , 0 )
*!*        SELECT (lcSumPck)
*!*        *-- Update as lcTmpPck
*!*        REPLACE &lcSumPck..lSelect   WITH IIF(&lcTmpPck..lSelect,.T.,.F.)
    ENDIF 
  CASE lcOption = 'I'
    SELECT (lcSumPck)
    LOCATE
    SCAN  
      lcPack_Id = IIF(&lcSumPck..llPack , LEFT(&lcSumPck..Pack_Id,16),SPACE(16)) + ;
                  &lcSumPck..cpkcolor + &lcSumPck..cpcksize + &lcSumPck..cPkVersion +;
                  IIF(&lcSumPck..llPack,'',&lcSumPck..Pack_Id) 
      SET ORDER TO TAG (lcPakIndxSt) IN (lcTmpPck)
 
      SELECT (lcTmpPck)
      LOCATE
      =SEEK(lcPack_Id)
      SCAN REST WHILE PACK_ID+cPkColor+cPckSize+cPkVersion+Style+cSizeNo = lcPack_Id ;
                FOR nCarton = 0
        REPLACE lSelect  WITH IIF(lSelect ,.F.,IIF(AvlQty>0 , .T. , .F.)),;
                Selected WITH IIF(lSelect , 1 , 0 )

        SELECT (lcSumPck)
        *-- Update as lcTmpPck
        REPLACE &lcSumPck..lSelect   WITH IIF(&lcTmpPck..lSelect,.T.,.F.)
      ENDSCAN  
    ENDSCAN
  

  CASE lcOption = 'A'
    SELECT (lcSumPck)
    LOCATE
    SCAN  
      lcPack_Id = IIF(&lcSumPck..llPack , LEFT(&lcSumPck..Pack_Id,16),SPACE(16)) + ;
                  &lcSumPck..cpkcolor + &lcSumPck..cpcksize + &lcSumPck..cPkVersion +;
                  IIF(&lcSumPck..llPack,'',&lcSumPck..Pack_Id) 
      SET ORDER TO TAG (lcPakIndxSt) IN (lcTmpPck)
 
      SELECT (lcTmpPck)
      LOCATE
      =SEEK(lcPack_Id)
      SCAN REST WHILE PACK_ID+cPkColor+cPckSize+cPkVersion+Style+cSizeNo = lcPack_Id ;
                FOR nCarton = 0
        REPLACE lSelect  WITH IIF(AvlQty>0 , .T. , .F.) ,;
                Selected WITH IIF(lSelect , 1 , 0 )

        SELECT (lcSumPck)
        *-- Update as lcTmpPck
        REPLACE &lcSumPck..lSelect   WITH IIF(&lcTmpPck..lSelect,.T.,.F.)
      ENDSCAN  
    ENDSCAN

    
  CASE lcOption =  'N'
    SELECT (lcTmpPck)
    REPLACE ALL lSelect WITH .F.,;
                Selected WITH 0

    SELECT(lcSumPck)
    REPLACE ALL  &lcSumPck..lSelect    WITH .F.

  CASE  lcOption =  'T' 
    IF !EMPTY(PACK_ID) 
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'This style is included in a pack , can not be selected individually.')
    ELSE    
      lcStyMjr = SUBSTR(&lcTmpPck..STYLE,1,loFormSet.lnMajLen)
      SET ORDER TO TAG (lcPakIndxSt) IN (lcTmpPck)
      GO TOP      
      lcKey = PADR('',16+6+3+4)+ lcStyMjr
      =SEEK(lcKey)
      SCAN REST WHILE Pack_Id + cpkcolor + cpcksize + cPkVersion + Style  + cSizeNo = lcKey ;
                FOR nCarton = 0
        REPLACE lSelect  WITH IIF(AvlQty>0,!lSelect,.F.),;
                Selected WITH IIF(lSelect, 1,0)
        IF SEEK(STYLE + cpkcolor + cpcksize + cPkVersion ,lcSumPck) 
          SELECT &lcSumPck
          REPLACE &lcSumPck..lSelect WITH &lcTmpPck..lSelect
        ENDIF
      ENDSCAN
    ENDIF

  CASE lcOption =  'P' 

    lcPack_Id  = PADR(Pack_Id,16) + cpkcolor + cpcksize + cPkVersion 
    lcPack_id2 = PADR(Pack_Id,19) + cpkcolor + cpcksize + cPkVersion 
    IF !EMPTY(lcPack_Id)
    
      LOCATE 
      =SEEK(ALLTRIM(lcPack_Id))
      SCAN REST WHILE  Pack_Id + cpkcolor + cpcksize + cPkVersion =ALLTRIM(lcPack_Id);
                FOR   nCarton = 0 AND !lIgnor
        REPLACE lSelect  WITH IIF(AvlQty>0,!lSelect,.F.),;
                Selected WITH IIF(lSelect,1,0)
      ENDSCAN
      IF SEEK(ALLTRIM(lcPack_id2),lcSumPck)
        =SEEK(ALLTRIM(lcPack_id),lcTmpPck)
        SELECT (lcSumPck)
        REPLACE lSelect  WITH &lcTmpPck..lSelect
      ENDIF
    ELSE
      IF !EMPTY(&lcTmpPck..NCARTON)
        *--Message to tell user it is not belonge to pack
        =gfModalGen("INM00000B42001","Dialog",.F.,.F.,;
                    "This style did not belong to pack.")
      ENDIF                  
    
    ENDIF  
ENDCASE

SELECT(lcTmpPck)
= RLOCK(lcTmpPck)
UNLOCk IN (lcTmpPck)


IF BETWEEN(lnRecno,1,RECCOUNT(lcTmpPck))
  GOTO (lnRecno) IN &lcTmpPck
ENDIF
IF BETWEEN(lnRecno2,1,RECCOUNT(lcSumPck))
  GOTO (lnRecno2) IN &lcSumPck
ENDIF

SELECT ORDLINE
=gfSetOrder(lcSvOrd)
SET ORDER TO &lcTmPckOrd IN (lcTmpPck)
=lfwDtlBrow(loFormSet)

SELECT(lnCurAlias)


  *!*********************************************************************
  *! Name      : lfSelPckLn
  *! Developer : Mariam MAzhar
  *! Date      : 03/01/2009
  *! Purpose   : To select/unselect line pack in detail/summary table
  *!*********************************************************************
  *! Parameters: loFormSet : ThisFormSet
  *!           lcSeekExpr: Seek expression
  *!         llHasPack : To update qty/crt and stywgh if not a pack
  *!             llDetail  : To use either lcSumPck or lcPckLin
  *!         lcDtExpr  : To select/unselect from lcSumPck in case of
  *!               detail.
  *!         lcOption  : To select/unselect based on the pressed button
  *!*********************************************************************
FUNCTION lfSelPckLn
  LPARAMETERS loFormSet, lcSeekExpr, llHasPack, llDetail, lcDtExpr, lcOption
  LOCAL lcCurAlis, lcOldOrd, lcWhileExpr, lcKey, lcPkLWhExpr, lcOldOrd1, lcOldOrd2

  lcCurAlis = ALIAS()

  lcOldOrd1 = ORDER(loFormSet.lcSumPck)
  lcOldOrd2 = ORDER(loFormSet.lcPckLin)

  IF llDetail
    SELECT (loFormSet.lcPckLin)
    LOCAL lnDtRecNo
    lnDtRecNo = RECNO()
    IF llHasPack
      SET ORDER TO (loFormSet.lcSumPck) IN (loFormSet.lcSumPck)
      lcWhileExpr = "REST FOR PACK_ID+cPkColor+cPckSize+cPkVersion"
      SET ORDER TO (loFormSet.lcPakIndxSt) IN (loFormSet.lcPckLin)
      lcPkLWhExpr = "REST FOR PACK_ID+cPkColor+cPckSize+cPkVersion+Style+cSizeNo"
*
    ELSE
      SET ORDER TO lcSumPkLn IN (loFormSet.lcSumPck)
      lcWhileExpr = "REST FOR PACK_ID+cPkColor+cPckSize+cPkVersion"
      SET ORDER TO (loFormSet.lcPckLin) IN (loFormSet.lcPckLin)
      lcPkLWhExpr = "REST FOR Style"
    ENDIF
    IF lcOption $ 'T'
      llSelect = lSelect
    ENDIF

    IF SEEK(lcSeekExpr)
      SCAN &lcPkLWhExpr = lcSeekExpr
        REPLACE lSelect   WITH IIF(lcOption $ 'T',!llSelect,IIF(IIF(loFormSet.llShwOpn,AvlQty > 0 AND OQty>0,AvlQty > 0),;
          IIF(lSelect,IIF(lcOption $ 'A',lSelect,.F.),.T.),lSelect)),;
          SELECTED  WITH IIF(lSelect,1,0),;
          CtnQty    WITH IIF(lSelect,IIF((loFormSet.lnTo-loFormSet.lnFrom+1)>0,;
          INT(OQty/(loFormSet.lnTo-loFormSet.lnFrom+1)),0),0),;
          CtnTotQty WITH CtnQty
        loFormSet.lnBrUntWgh = IIF(!lSelect,OrgStyWgh,StyWgh)
        REPLACE StyWgh    WITH loFormSet.lnBrUntWgh
      ENDSCAN
    ENDIF
    IF BETWEEN(lnDtRecNo,1,RECCOUNT())
      GOTO lnDtRecNo
    ENDIF

    IF SEEK(lcDtExpr,loFormSet.lcSumPck)
      SELECT(loFormSet.lcSumPck)
      SCAN &lcWhileExpr = lcDtExpr
        IF !llHasPack
          =SEEK(PADR(Pack_ID,19),loFormSet.lcPckLin)
        ENDIF
        REPLACE lSelect    WITH EVALUATE(loFormSet.lcPckLin+'.lSelect'),;
          lSelected  WITH lSelect
        IF llHasPack
          REPLACE CtnTotQty WITH IIF(EVALUATE(loFormSet.lcPckLin+'.lSelect'),;
            IIF(EVALUATE(loFormSet.lcPckLin+'.nPkPack') = 0,;
            EVALUATE(loFormSet.lcPckLin+'.nPackNo'),;
            EVALUATE(loFormSet.lcPckLin+'.nPkPack')),0)
        ELSE
          REPLACE CtnTotQty  WITH IIF(lSelect,EVALUATE(loFormSet.lcPckLin+'.CtnQty'),0)
        ENDIF
      ENDSCAN
    ENDIF

  ELSE
    SELECT (loFormSet.lcSumPck)
    lnDtRecNo = RECNO()
    IF llHasPack
      SET ORDER TO loFormSet.lcSumPck
      lcWhileExpr = "REST FOR PACK_ID+cPkColor+cPckSize+cPkVersion "
      SET ORDER TO (loFormSet.lcPakIndxSt) IN (loFormSet.lcPckLin)
      lcPkLWhExpr = "REST FOR PACK_ID+cPkColor+cPckSize+cPkVersion+Style"
    ELSE
      SET ORDER TO (loFormSet.lcSumPck)
      lcWhileExpr = "REST FOR PACK_ID"
      SET ORDER TO (loFormSet.lcPckLin) IN (loFormSet.lcPckLin)
      lcPkLWhExpr = "REST FOR Style"
    ENDIF
    IF SEEK(lcSeekExpr)
      SCAN &lcWhileExpr = lcSeekExpr
        lcKey = IIF(llPAck,LEFT(Pack_ID,16)+cPkColor+cPckSize+cPkVersion,;
          PADR(Pack_ID,19))
        REPLACE lSelect    WITH IIF(EVALUATE(loFormSet.lcSumPck+'.lSelected'),IIF(lcOption $ 'A',lSelect,.F.),.T.),;
          CtnTotQty  WITH IIF(lSelect,IIF((loFormSet.lnTo-loFormSet.lnFrom+1)>0,;
          INT(OTotQty/(loFormSet.lnTo-loFormSet.lnFrom+1)),0),0),;
          lSelected  WITH lSelect
        lnOrdPck  = nOrgQty
        lnNumPack = CTnTotQty

        IF SEEK(lcKey,loFormSet.lcPckLin)
          SELECT (loFormSet.lcPckLin)
          SCAN &lcPkLWhExpr = lcKey
            REPLACE lSelect   WITH EVALUATE(loFormSet.lcSumPck+'.lSelect'),;
              SELECTED  WITH IIF(lSelect,1,0)
            loFormSet.lnBrUntWgh = IIF(!lSelect,OrgStyWgh,StyWgh)
            REPLACE StyWgh    WITH loFormSet.lnBrUntWgh
            IF OQty <> 0
              IF !EVALUATE(loFormSet.lcSumPck+'.llPAck')
                REPLACE CtnQty    WITH IIF(lSelect,IIF((loFormSet.lnTo-loFormSet.lnFrom+1)>0,;
                  INT(OQty/(loFormSet.lnTo-loFormSet.lnFrom+1)),0),0),;
                  CtnTotQty WITH CtnQty
              ELSE
                REPLACE CtnQty    WITH IIF( (loFormSet.lnTo-loFormSet.lnFrom+1)> 0,;
                  INT(OQty/(loFormSet.lnTo-loFormSet.lnFrom+1)),0) ,;
                  CtnTotQty WITH CtnQty


              ENDIF
            ENDIF
          ENDSCAN
        ENDIF
      ENDSCAN
    ENDIF
    IF BETWEEN(lnDtRecNo,1,RECCOUNT())
      GOTO lnDtRecNo
    ENDIF
  ENDIF
  SET ORDER TO lcOldOrd1 IN (loFormSet.lcSumPck)
  SET ORDER TO lcOldOrd2 IN (loFormSet.lcPckLin)
  SELECT (lcCurAlis)
    *!*************************************************************
  *! Name      : lfvApply
  *! Developer : Mariam MAzhar
  *! Date      : 03/01/2009
  *! Purpose   : To validate the apply button
  *!*************************************************************
  *! Parameters: loFormSet : ThisFormSet
  *!*************************************************************
FUNCTION lfvApply
LPARAMETERS loFormSet

LOCAL lnCurAlias,lnCurRec,lnJ,laCart,lnContinue,lcTag,lnRecNom, lnRecNo, lcAccount,lnPkRecNo
DIMENSION laCart[1]
lnOldFrm = loFormSet.lnFrom
lnOldTo  = loFormSet.lnTo
STORE 0 TO laCart,lnContinue
lnCurAlias = SELECT(0)
lnRecNom   = RECNO(loFormSet.lcPckLin)
lnRecNo    = RECNO(loFormSet.lcSumPck)
lcAccount  = loFormSet.ariaForm1.kbAccount.keytextbox.VALUE
lcOrderNo  = loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE
lcPckLin = loFormSet.lcPckLin 
lcTmpPck = loFormSet.lcTmpPck
lcCtnDtl = loFormSet.lcCtnDtl
lcSumPck = loFormSet.lcSumPck
DIMENSION laCart[1]
STORE 0 To laCart
lnCurAlias = SELECT(0)
SELECT (lcTmpPck)
lnRecNom = RECNO()

SELECT (lcSumPck)
lnRecNo = RECNO()

SELECT (lcPckLin)
SET RELATION TO
lnCurRec = RECNO()


lcTag = ORDER(lcPckLin)
SET ORDER TO Selected IN (lcPckLin)
  
SELECT &lcPckLin
IF SEEK('Y',lcPckLin)
  SCAN FOR Selected>0 AND lSelect 
    lcCheckStr =IIF(lRange,'DisCtnQty = 0 OR STYWGH = 0','CTNQTY = 0 OR STYWGH = 0')
    IF &lcCheckStr
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Style '+Style+' has '+IIF(CTNQTY=0,'Qty/Ctn','Wgh/Unt')+' equals zero.')
      GOTO RECNO(lcPckLin) IN &lcTmpPck
      RETURN
    ENDIF
    IF !lRange
      IF MOD(OQTY,CTNQTY) # 0
        *-- Button : B44002
        *-- <Yes>;<Yes to All>;<No>
        lnRspn = gfModalGen('INM00000B44002',.F.,.F.,.F.,'The O.Qty. for '+STYLE+' is not divisible by Qty./Ctn. Continue?')
        IF lnRspn = 2
          EXIT
        ENDIF
        IF lnRspn = 3
          GOTO RECNO(lcPckLin) IN &lcTmpPck
          RETURN
        ENDIF
      ENDIF    
    ENDIF
  ENDSCAN
ENDIF

SELECT &lcPckLin
SET ORDER To Selected

*--Increment Carton#
SELECT &lcPckLin
LOCATE FOR lSelect 
llFound = FOUND()
lcCtnHdr  = loFormSet.lcCtnHdr 
IF llFound
  =lfNewCrt(loFormSet)
ELSE
  RETURN  
ENDIF

DIMENSION laPckId[1]
laPckId[1] = ' '
lnPckLn = 0
lcSumPck = loFormSet.lcSumPck

IF llFound
  SCAN FOR lSelect  
    
    =gfSEEK(&lcPckLin..STYLE,'STYLE')
    =gfSEEK('S'+STYLE.SCALE,'SCALE')
    SELECT (lcSumPck)
    IF !EMPTY(&lcPckLin..PACK_ID+&lcPckLin..cPkColor+&lcPckLin..cPckSize+&lcPckLin..cPkVersion)
      =SEEK(PADR(&lcPckLin..PACK_ID,19)+&lcPckLin..cPkColor+&lcPckLin..cPckSize+&lcPckLin..cPkVersion)
    ELSE
      =SEEK(PADR(&lcPckLin..Style,19)+&lcPckLin..cPkColor+&lcPckLin..cPckSize+&lcPckLin..cPkVersion)            
    ENDIF 
    IF &lcSumPck..lSelect 
      REPLACE &lcSumPck..PTotQty    WITH &lcSumPck..PTotQty + &lcPckLin..PQty 
      REPLACE lSelect WITH .F.
      REPLACE NCARTON WITH  loFormSet.lnCurrCtn 
    ENDIF          
    lnQuantities = &lcPckLin..CtnQty
    lnWeights    = &lcPckLin..StyWgh*&lcPckLin..CtnQty
    lcPckId = &lcPckLin..PACK_ID+&lcPckLin..cPkColor+&lcPckLin..cPckSize+&lcPckLin..cPkVersion+STR(loFormset.lnCurrCtn)
    IF ASCAN(laPckId , lcPckId ) = 0
      lnPckLn = lnPckLn + 1
      DIMENSION laPckId[lnPckLn]
      laPckId[lnPckLn] = lcPckId
    ELSE
      IF &lcPckLin..LRANGE
        lnQuantities = 0
        lnWeights    = 0
      ENDIF
    ENDIF

    IF lnQuantities > 0
      IF SEEK(STR(loFormset.lnCurrCtn,4),lcCtnHdr)
        SELECT (lcCtnHdr)
        REPLACE TotPcs WITH TotPcs + lnQuantities,;
                TotWgh WITH TotWgh + lnWeights
        loFormSet.lnPackQty = loFormSet.lnPackQty+ lnQuantities
        loFormSet.lnPackWgh = loFormSet.lnPackWgh+ lnWeights
        SELECT(lcPckLin)
      ELSE
        INSERT INTO (lcCtnHdr)(Cart_No   ,Pal_No  ,TotPcs      ,TotWgh   ,Empty);
                       VALUES (loFormset.lnCurrCtn,loFormset.lnCtnPal,lnQuantities,lnWeights,'N'  )
        loFormSet.lnPackWgh= loFormSet.lnPackWgh+ &lcCtnHdr..TotWgh
        loFormSet.lnPackCtn= loFormSet.lnPackCtn+ 1
        loFormSet.lnPackQty = loFormSet.lnPackQty+ &lcCtnHdr..TotPcs
      ENDIF
      loFormSet.lnMaxPal  = MAX(loFormSet.lnMaxPal,&lcCtnHdr..Pal_No)    
      loFormSet.lnMinPal  = Min(IIF(loFormSet.lnMinPal=0,&lcCtnHdr..Pal_No,loFormSet.lnMinPal),&lcCtnHdr..Pal_No)
    ENDIF
    
    SELECT (lcPckLin)    
    IF CTNQTY <> 0
      IF !SEEK(STR(loFormset.lnCurrCtn,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style,lcCtnDtl)  
        =gfSEEK(&lcPckLin..Style,'STYLE')
        =gfSEEK('S'+STYLE.SCALE,'SCALE')
        SELECT &lcCtnDtl
        APPEND BLANK
        REPLACE PACK_ID     WITH &lcPckLin..PACK_ID     ,;
                CPKCOLOR    WITH &lcPckLin..CPKCOLOR    ,;
                CPCKSIZE    WITH &lcPckLin..CPCKSIZE    ,;
                CPKVERSION  WITH &lcPckLin..CPKVERSION  ,;
                Style       WITH &lcPckLin..Style       ,;
                OrgWgh      WITH &lcPckLin..OrgStyWgh   ,;
                SzCnt       WITH SCALE.CNT,;
                cStatus     WITH "A",;
                Cart_No     WITH loFormset.lnCurrCtn

        REPLACE lFlag WITH .T.
        IF &lcPckLin..lRange
          lcSz = &lcPckLin..cSizeNo
          REPLACE Weight&lcSz WITH &lcPckLin..StyWgh*&lcSumPck..CtnTotQty,;
                  TotWeight   WITH TotWeight + Weight&lcSz

        ENDIF 
      ELSE
         llFoundDtl = .F.
         SELECT &lcCtnDtl
         SCAN REST WHILE STR(Cart_No,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style =;
                         STR(loFormset.lnCurrCtn,4)+&lcPckLin..PACK_ID+&lcPckLin..CPKCOLOR+&lcPckLin..CPCKSIZE+&lcPckLin..CPKVERSION+&lcPckLin..Style for;
                         cNoSize = &lcPckLin..cSizeNo     
           llFoundDtl = .T.               
         ENDSCAN 
         IF !llFoundDtl   
           =gfSEEK(&lcPckLin..Style,'STYLE')
           =gfSEEK('S'+STYLE.SCALE,'SCALE')
           SELECT &lcCtnDtl
           APPEND BLANK
           REPLACE PACK_ID     WITH &lcPckLin..PACK_ID     ,;
                   CPKCOLOR    WITH &lcPckLin..CPKCOLOR    ,;
                   CPCKSIZE    WITH &lcPckLin..CPCKSIZE    ,;
                   CPKVERSION  WITH &lcPckLin..CPKVERSION  ,;
                   Style       WITH &lcPckLin..Style       ,;
                   OrgWgh      WITH &lcPckLin..OrgStyWgh   ,;
                   SzCnt       WITH SCALE.CNT,;
                   cStatus     WITH "A",;
                   Cart_No     WITH loFormset.lnCurrCtn

           REPLACE lFlag WITH .T.
           IF &lcPckLin..lRange
             lcSz = &lcPckLin..cSizeNo
             REPLACE Weight&lcSz WITH &lcPckLin..StyWgh*&lcSumPck..CtnTotQty,;
                     TotWeight   WITH TotWeight + Weight&lcSz

           ENDIF 
        ENDIF 
      ENDIF
      SELECT &lcCtnDtl
      lcSz = &lcPckLin..cSizeNo
      IF &lcPckLin..lRange
       REPLACE Size&lcSz   WITH IIF(&lcPckLin..lSelect AND &lcPckLin..CtnQty>0,&lcPckLin..cSize,Size&lcSz) ,;
                Br&lcSz     WITH !EMPTY(Size&lcSz),;
                Qty&lcSz    WITH &lcPckLin..CtnQty,;
                cStatus     WITH IIF(&lcPckLin..Selected>0,'M',cStatus)      
      ELSE
        REPLACE Size&lcSz   WITH IIF(&lcPckLin..lSelect AND &lcPckLin..CtnQty>0,&lcPckLin..cSize,Size&lcSz) ,;
                Br&lcSz     WITH !EMPTY(Size&lcSz),;
                Qty&lcSz    WITH &lcPckLin..CtnQty,;
                Weight&lcSz WITH &lcPckLin..StyWgh*&lcPckLin..CtnQty,;
                TotWeight   WITH TotWeight + Weight&lcSz ,;
                cStatus     WITH IIF(&lcPckLin..Selected>0,'M',cStatus)
      ENDIF   
      REPLACE cNoSize WITH lcSz
    *-- End if Packed qty = 0 
    ELSE          
      IF &lcPckLin..lRange
        IF !SEEK(STR(loFormset.lnCurrCtn,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style,lcCtnDtl)  
          =gfSEEK(&lcPckLin..Style,'STYLE')
          =gfSEEK('S'+STYLE.SCALE,'SCALE')
          SELECT &lcCtnDtl
          lcSz = &lcPckLin..cSizeNo
          APPEND BLANK
          REPLACE PACK_ID     WITH &lcPckLin..PACK_ID     ,;
                 CPKCOLOR    WITH &lcPckLin..CPKCOLOR    ,;
                 CPCKSIZE    WITH &lcPckLin..CPCKSIZE    ,;
                 CPKVERSION  WITH &lcPckLin..CPKVERSION  ,;
                 Style       WITH &lcPckLin..Style       ,;
                 OrgWgh      WITH &lcPckLin..OrgStyWgh   ,;
                 SzCnt       WITH SCALE.CNT,;
                 cStatus     WITH "A",;
                 Cart_No     WITH loFormset.lnCurrCtn,;
                 Weight&lcSz WITH &lcPckLin..StyWgh*&lcSumPck..CtnTotQty,;
                 TotWeight   WITH TotWeight + Weight&lcSz 
        ENDIF
        SELECT &lcCtnDtl
        lcSz = &lcPckLin..cSizeNo
        REPLACE Size&lcSz   WITH IIF(!EMPTY(&lcPckLin..lSelect) AND &lcPckLin..CtnQty>0,&lcPckLin..cSize,Size&lcSz) ,;
                Br&lcSz     WITH !EMPTY(Size&lcSz),;
                cStatus     WITH IIF(&lcPckLin..Selected>0,'M',cStatus),;
                lFlag       WITH IIF(lFlag,.T.,.F.)
                
        REPLACE cNoSize WITH lcSz         
      ENDIF    
    ENDIF

    SELECT(lcPckLin)
    REPLACE lSelect WITH .F.
    REPLACE NCARTON WITH loFormset.lnCurrCtn   

    IF lRange
      REPLACE NRNGCART WITH loFormset.lnCurrCtn   
    ENDIF  

  ENDSCAN
ENDIF
SET ORDER TO lcTag IN (lcPckLin)

SELECT(lcPckLin)

=RLOCK(lcPckLin)
UNLOCK IN (lcPckLin)

llApply = .T.

SELECT (lcSumPck)
IF BETWEEN(lnRecNo,1,RECCOUNT(lcSumPck))
  GOTO lnRecNo
ENDIF

SELECT(lcPckLin)
IF lnCurRec <= RECCOUNT()
  GOTO lnCurRec
ENDIF

SELECT (lcCtnHdr)
GOTO TOP
= RLOCK(lcCtnHdr) 
UNLOCk IN (lcCtnHdr)

SELECT (lcCtnDtl)
GOTO TOP
= RLOCK(lcCtnDtl) 
UNLOCk IN (lcCtnDtl)

SELECT (lcTmpPck)
IF BETWEEN(lnRecNom ,1,RECCOUNT(lcTmpPck))
  GOTO lnRecNom 
ENDIF

SELECT (lnCurAlias)
*-- END OF lfvApply

*:**************************************************************************
*:* Name        : lfNewCrt
*:* Developer : Mariam MAzhar
*:* Date        : 03/01/2009
*:* Purpose     : Check if there is gaps between cartons and get current carton #
*:***************************************************************************
FUNCTION lfNewCrt
PARAMETERS loFormset
PRIVATE lnI,laCtns
DIME laCtns[1]
laCtns = 0

SELECT DIST Cart_No FROM &lcCtnHdr WHERE !DELETED() INTO ARRAY  laCtns
FOR lnI = 1 TO loFormSet.lnCrtnSeq
  IF ASCAN(laCtns,lnI)=0
    loFormset.lnCurrCtn = lnI
    RETURN 
  ENDIF
ENDFOR

loFormset.lnCrtnSeq = loFormset.lnCrtnSeq + 1
loFormset.lnCurrCtn = loFormset.lnCrtnSeq
  *!*************************************************************
  *! Name      : lfWinHdrSt
  *! Developer : Mariam Mazhar
  *! Date      : 03/01/2009
  *! Purpose   : adjust enable status for header folder fields
  *!*************************************************************
  *! Called    : From ChangeMode in the formset
  *!*************************************************************
FUNCTION lfWinHdrSt
  LPARAMETERS loFormSet

  IF loFormSet.ActiveMode $ 'AE' &&OR loFormSet.llNew
    WITH loFormSet.AriaForm1
      STORE .F. TO .kbOrderNo.ENABLED, .kbAccount.ENABLED,;
        .txtCustName.ENABLED,  .txtCustPo.ENABLED
    ENDWITH
  ENDIF


  *!*************************************************************
  *! Name      : lfWinCtnSt
  *! Developer : MAriam Mazhar
  *! Date      : 03/01/2009
  *! Purpose   : To adjust add buttons for carton header and
  *!             carton detail staus
  *!*************************************************************
  *! Calls     :
  *!             Procedures : ....
  *!             Functions  : ....
  *!*************************************************************
  *! Passed Parameters  : None
  *!*************************************************************
  *! Returns            : None
  *!*************************************************************
  *! Example   : = lfWinCtnSt()
  *!*************************************************************

FUNCTION lfWinCtnSt
  LPARAMETERS loFormSet,lnActFolder


  WITH loFormSet.AriaForm1.pgfPacking.CartonInfo
    IF lnActFolder = 2 AND (loFormSet.ActiveMode $ 'AE' )&&OR loFormSet.llNew
      STORE .T. TO .cmdNewCartH.ENABLED 

      IF EMPTY(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'))
        STORE .F. TO .cmdNewCartD.ENABLED
      ELSE
        STORE .T. TO .cmdNewCartD.ENABLED
      ENDIF
    ELSE
      STORE .F. TO .cmdNewCartH.ENABLED,.cmdNewCartD.ENABLED
    ENDIF
  ENDWITH
*!*************************************************************
*! Name      : lfGetPackId
*! Developer : Mariam MAzhar
*! Date      : 03/01/2009
*! Purpose   : Function To get pack_id
*!*************************************************************
FUNCTION lfGetPackId
PARAMETERS loFormSet
RETURN EVALUATE(loFormSet.lcSumPck + '.Pack_Id')+'-'+EVALUATE(loFormSet.lcSumPck + '.cPkColor')+;
       '-'+lfGetGmSz(EVALUATE(loFormSet.lcSumPck + '.cPckSize'))+'-'+EVALUATE(loFormSet.lcSumPck + '.cPkVersion')
       
*!*************************************************************
*! Name      : lfGetGmSz
*! Developer : Mariam MAzhar
*! Date      : 03/01/2009
*! Purpose   : Function To Size discreption
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        : 
*!*************************************************************
*! Example            : =lfGetGmSz()
*!*************************************************************

FUNCTION lfGetGmSz
PARAMETER lcPackSize
PRIVATE lcNombr

IF !EMPTY(lcPackSize)
  =gfSEEK('S'+LEFT(lcPackSize,1),'SCALE')
  lcNombr = RIGHT(lcPackSize,1)
  lcLocSize=EVAL('SCALE.SZ'+lcNombr)
ELSE
  lcLocSize ='*****'
ENDIF  
RETURN lcLocSize 

*!*************************************************************
*! Name      : lfGetDetPackId       
*! Developer : Mariam MAzhar
*! Date      : 03/01/2009
*! Purpose   : Function To get Detail tab pack_id
*!*************************************************************
FUNCTION lfGetDetPackId       
PARAMETERS loFormSet
RETURN EVALUATE(loFormSet.lcTmpPck+ '.Pack_Id')+'-'+EVALUATE(loFormSet.lcTmpPck+ '.cPkColor')+;
       '-'+lfGetGmSz(EVALUATE(loFormSet.lcTmpPck+ '.cPckSize'))+'-'+EVALUATE(loFormSet.lcTmpPck+ '.cPkVersion')

*!*************************************************************
*! Name      : lfvQTYCRN
*! Developer : Mariam MAzhar
*! Date      : 03/01/2009
*! Purpose   : Function To Validate carton qty
*!*************************************************************
FUNCTION lfvQTYCRN
PARAMETERS loFormSet

IF loFormSet.ActiveMode $ 'SV'
  RETURN  
ENDIF 

lcTmpPck = loFormSet.lcTmpPck
lcSumPck = loFormSet.lcSumPck
PRIVATE lcOrd
lcOrd = ORDER(lcTmpPck)
lcTmpAls = ALIAS()
DO CASE
  CASE ALIAS() = lcTmpPck
    IF EVAL(lcTmpPck+'.LRANGE')=.T.
      REPLACE CtnQty WITH DisCtnQty
      DIMENSION laCtnSum[1]
      laCtnSum[1] = 0
      IF !EMPTY(PACK_ID)
        lcPck = PADR(PACK_ID,16)+CPKCOLOR+CPCKSIZE+CPKVERSION
        SELECT SUM(CtnQty) FROM &lcTmpPck WHERE ;
        PADR(PACK_ID,16)+CPKCOLOR+CPCKSIZE+CPKVERSION = lcPck ;
        .AND. AvlQty>0 INTO ARRAY laCtnSum
        IF SEEK(PADR(PACK_ID,19)+cPkColor+cPckSize+cPkVersion,lcSumPck)
          REPLACE &lcSumPck..CtnTotQty WITH laCtnSum[1]
        ENDIF
      ELSE
        lcPck = STYLE+CPKCOLOR+CPCKSIZE+CPKVERSION
        SELECT SUM(CtnQty) ;
          FROM &lcTmpPck ;
          WHERE STYLE+CPKCOLOR+CPCKSIZE+CPKVERSION = lcPck ;
          .AND. EMPTY(PACK_ID) .AND. AvlQty>0;
          INTO ARRAY laCtnSum
        IF SEEK(lcPck,lcSumPck)
          REPLACE &lcSumPck..CtnTotQty WITH laCtnSum[1]
        ENDIF
      ENDIF


    ELSE
      IF !EMPTY(PACK_ID)
        lcPck = PADR(PACK_ID,16)+CPKCOLOR+CPCKSIZE+CPKVERSION
        SELECT SUM(CtnQty) FROM &lcTmpPck WHERE ;
        PADR(PACK_ID,16)+CPKCOLOR+CPCKSIZE+CPKVERSION = lcPck ;
        .AND. AvlQty>0 INTO ARRAY laCtnSum
        IF SEEK(PADR(PACK_ID,19)+cPkColor+cPckSize+cPkVersion,lcSumPck)
          REPLACE &lcSumPck..CtnTotQty WITH laCtnSum[1]
        ENDIF
      ELSE
        lcPck = STYLE+CPKCOLOR+CPCKSIZE+CPKVERSION
        SELECT SUM(CtnQty) ;
          FROM &lcTmpPck ;
          WHERE STYLE+CPKCOLOR+CPCKSIZE+CPKVERSION = lcPck ;
          .AND. EMPTY(PACK_ID) .AND. AvlQty>0;
          INTO ARRAY laCtnSum
        IF SEEK(lcPck,lcSumPck)
          REPLACE &lcSumPck..CtnTotQty WITH laCtnSum[1]
        ENDIF
      ENDIF
   ENDIF

    IF &lcTmpPck..CTNQTY = 0
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Qty./Ctn. can not be zero.')
    ELSE  
      IF MOD(&lcTmpPck..OQTY,&lcTmpPck..CTNQTY) # 0
        =gfModalGen('INM00000B00000',.F.,.F.,.F.,'The O.Qty. is not divisible by Qty./Ctn.')
      ENDIF
    ENDIF
 
  CASE ALIAS() = lcSumPck
    IF llPack 
      lcKey = PADR(PACK_ID,16)+cPkColor+cPckSize+cPkVersion
      =SEEK(lcKey,lcTmpPck)
      SELECT &lcTmpPck  
      lnQtySm = 0
      SCAN REST WHILE PACK_ID+cPkColor+cPckSize+cPkVersion+Style+cSizeNo = lcKey
        REPLACE CtnQty WITH PkStyQty * &lcSumPck..PkCtnQty
        REPLACE DisCtnQty WITH PkStyQty * &lcSumPck..PkCtnQty
        lnQtySm = lnQtySm + CtnQty
      ENDSCAN
      SELECT &lcSumPck
      REPLACE &lcSumPck..CtnTotQty WITH lnQtySm
    ENDIF
    SELECT &lcSumPck
    IF &lcSumPck..CTNTotQTY = 0
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Qty./Ctn. can not be zero.')
    ELSE  
      IF MOD(&lcSumPck..OTotQty,IIF(llPack,&lcSumPck..PkCtnQty,&lcSumPck..CtnTotQty)) # 0
        =gfModalGen('INM00000B00000',.F.,.F.,.F.,'The O.Qty. is not divisible by Qty./Ctn.')
      ENDIF
    ENDIF
ENDCASE
*!*************************************************************
*! Name      : lfvWeight
*! Developer : Mariam MAzhar
*! Date      : 03/01/2009
*! Purpose   : Function To Validate Weight
*!*************************************************************
FUNCTION lfvWeight
PARAMETERS loFormSet

IF loFormSet.ActiveMode $ 'SV'
  RETURN  
ENDIF 


lcTmpPck = loFormSet.lcTmpPck
lcSumPck = loFormSet.lcSumPck
PRIVATE lnAlias,lcSeek
lnAlias = SELECT()
lcSeek = ''

DO CASE
  CASE ALIAS() = lcTmpPck
    IF !EMPTY(PACK_ID)
      DIMENSION laWghtSum[1]
      laWghtSum[1] = 0
      lcPck = PADR(PACK_ID,16)+CPKCOLOR+CPCKSIZE+CPKVERSION
      SELECT SUM(StyWgh) FROM &lcTmpPck WHERE ;
      PADR(PACK_ID,16)+CPKCOLOR+CPCKSIZE+CPKVERSION = lcPck INTO ARRAY laWghtSum
      IF SEEK(PADR(PACK_ID,19)+cPkColor+cPckSize+cPkVersion,lcSumPck)
        REPLACE &lcSumPck..PTotWgh WITH laWghtSum[1]
      ENDIF
    ELSE
      IF SEEK(STYLE+cPkColor+cPckSize+cPkVersion,lcSumPck)
        REPLACE &lcSumPck..PTotWgh WITH &lcTmpPck..StyWgh
      ENDIF
      lcSeek = PACK_ID+cPkColor+cPckSize+cPkVersion+STYLE
      lcSz = CSIZENO
      =SEEK(lcSeek,lcTmpPck)
      SCAN REST WHILE PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE+CSIZENO = lcSeek
        REPLACE StyWgh WITH &lcSumPck..PTotWgh
      ENDSCAN
      =SEEK(lcSeek+lcSz,lcTmpPck)
    ENDIF
  
  CASE ALIAS() = lcSumPck
    IF llPack 
      REPLACE &lcSumPck..PTotWgh WITH lcOldVal
    ELSE
      lcLoop = PADR('',16)+CPKCOLOR+CPCKSIZE+CPKVERSION+PACK_ID
      SELECT (lcTmpPck)      
      lcSv = PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE+CSIZENO
      =SEEK(lcLoop,lcTmpPck)
      SCAN REST WHILE PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE+CSIZENO = lcLoop
        REPLACE &lcTmpPck..StyWgh WITH &lcSumPck..PTotWgh
      ENDSCAN
      =SEEK(lcSv,lcTmpPck)
    ENDIF
ENDCASE

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfCtnHdrBr 
*! Developer : Mariam MAzhar
*! Date      : 03/01/2009
*! Purpose   : Function To Add Control source to cartons header grid
*!*************************************************************
FUNCTION lfCtnHdrBr 
PARAMETERS loFormSet
LOCAL lnCurAlias

lnCurAlias = SELECT(0)

WITH loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonH
  .RECORDSOURCE = ''
  SELECT (loFormSet.lcCtnHdr)
  LOCATE
  .RECORDSOURCE = loFormSet.lcCtnHdr
    *-- Cart. #
  .Column1.CONTROLSOURCE  = loFormSet.lcCtnHdr +'.Cart_No'
  .Column1.Header1.caption = 'Cart.#'       
  .Column1.Header1.ALIGNMENT = 1
  .COLUMNS(1).ALIGNMENT = 1
  .COLUMNS(1).WIDTH = 40

  *-- Palette (If llEDISys)
  *.Column2.CONTROLSOURCE  = loFormSet.lcCtnHdr + '.Pal_No'
  .Column2.VISIBLE =  .F.
  *.Column2.Header1.ALIGNMENT = 1
*!*    .COLUMNS(2).ALIGNMENT = 1
*!*    .COLUMNS(2).WIDTH = 40

  *-- Tot. Pcs.
  .Column3.CONTROLSOURCE  = loFormSet.lcCtnHdr + '.TotPcs'
  .Column3.Header1.caption= 'Tot.Pcs'      
  .Column3.Header1.ALIGNMENT = 1
  .COLUMNS(3).ALIGNMENT = 1
  .COLUMNS(3).WIDTH = 70

  *-- Tot. Wgh.
  .Column4.CONTROLSOURCE  = loFormSet.lcCtnHdr + '.TotWgh'
  .Column4.Header1.ALIGNMENT = 1
  .Column4.Header1.caption= 'Tot.Wgh'      
  .COLUMNS(4).ALIGNMENT = 1
  .COLUMNS(4).WIDTH = 80

  *-- Carrier Carton ID
  *.Column5.CONTROLSOURCE  = loFormSet.lcCtnHdr +'.cCarrCtnID'
  .Column5.Visible =.F.
  .SETALL('ReadOnly',.T.,'COLUMN')
ENDWITH
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfCtnDtlBr 
*! Developer : Mariam MAzhar
*! Date      : 03/01/2009
*! Purpose   : Function To Add Control source to cartons detail grid
*!*************************************************************
FUNCTION lfCtnDtlBr 
PARAMETERS loFormSet
  LOCAL lnCurAlias

  lnCurAlias = SELECT(0)

  WITH loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonD
    .RECORDSOURCE = ''
    SELECT (loFormSet.lcCtnDtl)
    LOCATE
    .RECORDSOURCE = loFormSet.lcCtnDtl
    *-- PAck_ID
    *.Column6.CONTROLSOURCE  = loFormSet.lcCtnDtl + '.cPackId'
    .Column6.VISIBLE =.F.&& IIF(loFormSet.llUsePack, .T., .F.)
    *.COLUMNS(6).WIDTH = 160
    *.Column1.COLUMNORDER = 1

    *-- Style
    .Column1.CONTROLSOURCE  = loFormSet.lcCtnDtl +'.Style'
    .Column1.Header1.CAPTION = loFormSet.lcStyTtl
    .COLUMNS(1).WIDTH = 120
    .Column1.COLUMNORDER = 2

    *-- Configuration/Dyelot
    *.Column2.Header1.CAPTION = IIF(loFormSet.llUseConfg,LANG_ManulPL_CptConfig,LANG_ManulPL_CptDyelot)
    *.Column2.CONTROLSOURCE   = loFormSet.lcCtnDtl + '.Dyelot'
    .Column2.VISIBLE        = .F. &&IIF(loFormSet.llUseConfg OR loFormSet.llDyelot , .T., .F.)
    *.COLUMNS(2).WIDTH = 80
    *.Column2.COLUMNORDER = 3

    *-- Size Code.
    .Column3.CONTROLSOURCE  = "THISFormSet.lfGetSz()"
    .COLUMNS(3).WIDTH = 40
    .Column3.COLUMNORDER = 4

    *-- Qty
    .Column4.CONTROLSOURCE  = "THISFormSet.lfGetQty()" 
    .COLUMNS(4).WIDTH = 40
    .Column4.Header1.ALIGNMENT = 1
    .COLUMNS(4).ALIGNMENT = 1
    .Column4.COLUMNORDER = 5

    *-- Weight
    .Column5.CONTROLSOURCE  = "THISFormSet.lfGetWgh()"
    .COLUMNS(5).WIDTH = 90
    .Column5.Header1.ALIGNMENT = 1
    .COLUMNS(5).ALIGNMENT = 1
    .Column5.COLUMNORDER = 6

    .SETALL('ReadOnly',.T.,'COLUMN')
  ENDWITH

  SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfGetSz
*! Developer : Mariam MAzhar
*! Date      : 03/01/2009
*! Purpose   : Function To get Size description
*!*************************************************************
FUNCTION lfGetSz
PARAMETERS loFormSet
 
lcSizeNo = eval(loFormSet.lcCtnDtl +'.cNoSize')
IF !EMPTY(lcSizeNo)
  RETURN Eval(loFormSet.lcCtnDtl +'.Size'+lcSizeNo)
ELSE
  RETURN ''
ENDIF 

*!*************************************************************
*! Name      : lfGetQty
*! Developer : Mariam MAzhar
*! Date      : 03/01/2009
*! Purpose   : Function To get Qty of a given size
*!*************************************************************
FUNCTION lfGetQty
PARAMETERS loFormSet
 
lcSizeNo = eval(loFormSet.lcCtnDtl +'.cNoSize')
IF !EMPTY(lcSizeNo)
  RETURN Eval(loFormSet.lcCtnDtl +'.Qty'+lcSizeNo)
ELSE
  RETURN ''
ENDIF 

*!*************************************************************
*! Name      : lfGetWgh
*! Developer : Mariam MAzhar
*! Date      : 03/01/2009
*! Purpose   : Function To get Weight of a given size
*!*************************************************************
FUNCTION lfGetWgh
PARAMETERS loFormSet
 
lcSizeNo = eval(loFormSet.lcCtnDtl +'.cNoSize')
IF !EMPTY(lcSizeNo)
  RETURN Eval(loFormSet.lcCtnDtl +'.Weight'+lcSizeNo)
ELSE
  RETURN ''
ENDIF   
*!*************************************************************
*! Name      : lfvCtHdWgh
*! Developer : Mariam MAzhar
*! Date      : 03/01/2009
*! Purpose   : validate Carton total weight 
*!*************************************************************
FUNCTION lfvCtHdWgh
PARAMETERS loFormSet
PRIVATE lnCurAlias,lnCart,lcStyle,lnOrdLin,lnUnitWgh,lnChoice
STORE 0 TO lnCart,lnOrdLin,lnUnitWgh,lnChoice
STORE SPACE(0) TO lcStyle
lcCtnDtl = loFormSet.lcCtnDtl
lcPckLin = loFormSet.lcPckLin
lcCtnHdr = loFormSet.lcCtnHdr
lnCurAlias = SELECT(0)
lcOldVal = loFormSet.AriaForm1.pgfPacking.CartonInfo.txtTotWghH.OldValue 
IF LASTKEY() = 13 AND !(loFormSet.AriaForm1.pgfPacking.CartonInfo.txtTotWghH.Value == loFormSet.AriaForm1.pgfPacking.CartonInfo.txtTotWghH.OldValue)
  *-- This is to enforce equal contribution if the total weight was zero
  IF loFormSet.AriaForm1.pgfPacking.CartonInfo.txtTotWghH.OldValue = 0
    lnChoice = 2
  ELSE
    SELECT(lcCtnDtl)
    lnStyRec = RECNO(lcCtnDtl)
    SCAN
      IF (Br1 AND Weight1=0) OR (Br2 AND Weight2=0) OR ;
         (Br3 AND Weight3=0) OR (Br4 AND Weight4=0) OR ;
         (Br5 AND Weight5=0) OR (Br6 AND Weight6=0) OR ;
         (Br7 AND Weight7=0) OR (Br8 AND Weight8=0)
        *-- One or more sizes has zeros weight.
        *-- Contributing the total carton weight may
        *-- either ignore the zero weight sizes,
        *-- or contribute equal weight to all sizes
        *-- <Ignore zero sizes>,<Equal contibution>,<Cancel>
        lnChoice = gfModalGen("INM44049B44007","Dialog")
        EXIT
      ENDIF
    ENDSCAN
    IF lnStyRec <= RECCOUNT(lcCtnDtl)
      GOTO lnStyRec
    ENDIF
  ENDIF
  IF !(lnChoice=3)
    SELECT(lcPckLin)
    SET RELATION TO
    SET RELATION TO STR(&lcCtnHdr..Cart_No,4)+Style INTO (lcCtnDtl)
    lnStyRec = RECNO(lcPckLin)
*    REPLACE PWgh WITH 
    *REPLACE ALL FOR Style =&lcCtnDtl..Style AND STR(Cart_No,4)+Style =STR(&lcCtnHdr..Cart_No,4)+Style;
                PWgh1 WITH IIF(&lcCtnDtl..Br1,lfPckLinUp('1'),PWgh1),;
                PWgh2 WITH IIF(&lcCtnDtl..Br2,lfPckLinUp('2'),PWgh2),;
                PWgh3 WITH IIF(&lcCtnDtl..Br3,lfPckLinUp('3'),PWgh3),;
                PWgh4 WITH IIF(&lcCtnDtl..Br4,lfPckLinUp('4'),PWgh4),;
                PWgh5 WITH IIF(&lcCtnDtl..Br5,lfPckLinUp('5'),PWgh5),;
                PWgh6 WITH IIF(&lcCtnDtl..Br6,lfPckLinUp('6'),PWgh6),;
                PWgh7 WITH IIF(&lcCtnDtl..Br7,lfPckLinUp('7'),PWgh7),;
                PWgh8 WITH IIF(&lcCtnDtl..Br8,lfPckLinUp('8'),PWgh8),;
                PTotWgh WITH (PWgh1+PWgh2+PWgh3+PWgh4+PWgh5+PWgh6+PWgh7+PWgh8)
    SET RELATION TO
    *SET RELATION TO '' INTO (lcScaFile)
    *SET SKIP TO (lcScaFile)
    IF lnStyRec <= RECCOUNT(lcPckLin) 
      GOTO lnStyRec
    ENDIF
    = RLOCK(lcPckLin) 
    UNLOCk IN (lcPckLin)

    SELECT(lcCtnDtl)
    lnStyRec = RECNO(lcCtnDtl)
    SET RELATION TO 
    = SEEK(STR(&lcCtnHdr..Cart_No,4),lcCtnDtl)

    REPLACE REST FOR STR(Cart_No,4)+Style+STR(nOrdLineNo,6) = STR(&lcCtnHdr..Cart_No,4) ;
                 Weight1 WITH IIF(Br1,lfCtnDtlUp('1'),Weight1),;
                 Weight2 WITH IIF(Br2,lfCtnDtlUp('2'),Weight2),;
                 Weight3 WITH IIF(Br3,lfCtnDtlUp('3'),Weight3),;
                 Weight4 WITH IIF(Br4,lfCtnDtlUp('4'),Weight4),;
                 Weight5 WITH IIF(Br5,lfCtnDtlUp('5'),Weight5),;
                 Weight6 WITH IIF(Br6,lfCtnDtlUp('6'),Weight6),;
                 Weight7 WITH IIF(Br7,lfCtnDtlUp('7'),Weight7),;
                 Weight8 WITH IIF(Br8,lfCtnDtlUp('8'),Weight8),;
                 cStatus WITH 'M',;
                 TotWeight WITH (Weight1+Weight2+Weight3+Weight4+Weight5+Weight6+Weight7+Weight8)

    IF lnStyRec <= RECCOUNT(lcCtnDtl) 
      GOTO lnStyRec
    ENDIF
    = RLOCK(lcCtnDtl) 
    UNLOCk IN (lcCtnDtl)
  ENDIF

  loFormSet.lnPackWgh = loFormSet.lnPackWgh - &lcCtnHdr..TotWgh + lnTotWgh

  SELECT (lcCtnHdr)
  REPLACE TotWgh WITH lnTotWgh

  = lfwCtnHdrBr(loFormSet)

ELSE
  lnTotWgh = loFormSet.AriaForm1.pgfPacking.CartonInfo.txtTotWghH.OldValue
ENDIF    

SELECT(lnCurAlias)
*!*************************************************************
*! Name      : lfPckLinUp
*! Developer : Mariam MAzhar
*! Date      : 03/01/2009
*! Purpose   : Update the weight in file lcPckLin according to
*!             update the total carton weight field
*!*************************************************************
FUNCTION lfPckLinUp
PARAMETERS lcSize,lnReturn
PRIVATE lcSize

lcPWghFld = 'PWgh'+lcSize
lcWghFld  = lcCtnDtl+'.Weight'+lcSize
lcQtyFld  = lcCtnDtl+'.Qty'+lcSize

lnReturn = (&lcPWghFld-&lcWghFld+IIF(lnChoice=2,;
                                     (&lcQtyFld/&lcCtnHdr..TotPcs)*lnTotWgh,;
                                     (lnTotWgh/lcOldVal)*&lcWghFld))

RETURN lnReturn
*!*************************************************************
*! Name      : lfCtnDtlUp
*! Developer : Mariam Mazhar
*! Date      : 03/01/2009
*! Purpose   : Update the weight in lcCtnDtl file according to
*!             update the total carton weight field
*!*************************************************************

FUNCTION lfCtnDtlUp
PARAMETERS lcSize
PRIVATE lcSize,lnReturn

lcWghFld  = 'Weight'+lcSize
lcQtyFld  = 'Qty'+lcSize

lnReturn = (IIF(lnChoice=2,(&lcQtyFld/&lcCtnHdr..TotPcs)*lnTotWgh,(lnTotWgh/lcOldVal)*&lcWghFld))

RETURN lnReturn

*!*************************************************************
*! Name      : lfvCtHdNew
*! Developer : Mariam Mazhar
*! Date      : 03/01/2009
*! Purpose   : validate New Carton button
*!*************************************************************

FUNCTION lfvCtHdNew
PARAMETERS loFormSet
PRIVATE lnCurAlias

STORE .T. TO loFormSet.llCupdate,loFormSet.llAnyUpd

lnCurAlias = SELECT(0)
lcCtnHdr = loFormSet.lcCtnHdr
SELECT (loFormSet.lcCtnHdr)
APPEND BLANK

loFormSet.lnPackCtn = loFormSet.lnPackCtn + 1   
loFormSet.lnMaxCtn  = loFormSet.lnMaxCtn  + 1

loFormSet.lnFrom = loFormSet.lnMaxCtn + 1
loFormSet.lnTo   = loFormSet.lnMaxCtn + 1

=lfNewCrt(loFormSet)
REPLACE Cart_No WITH loFormSet.lnCurrCtn,;
        Empty   WITH 'Y'

= RLOCK(lcCtnHdr) 
UNLOCk IN (lcCtnHdr)

loFormSet.AriaForm1.pgfPacking.CartonInfo.cmdNewCartD.ENABLED = .T.

= lfwCtnHdrBr(loFormSet)

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfvCtDtNew
*! Developer : Mariam Mazhar
*! Date      : 03/01/2009
*! Purpose   : validate New Style button
*!*************************************************************
FUNCTION lfvCtDtNew
PARAMETERS loFormSet
PRIVATE lnCurAlias,lcBrFld,lcNewFld,lnK

STORE .T. TO loFormSet.llCupdate,loFormSet.llAnyUpd
lnCurAlias = SELECT(0)
lcCtnDtl = loFormSet.lcCtnDtl
lcCtnHdr = loFormSet.lcCtnHdr
SELECT(lcCtnDtl)

IF !SEEK(STR(&lcCtnHdr..Cart_NO,4)+SPACE(48),lcCtnDtl)
  APPEND BLANK
  REPLACE Cart_No WITH &lcCtnHdr..Cart_NO,;
          SzCnt   WITH 1,;
          Br1     WITH .T.
     
  =RLOCK(lcCtnDtl) 
  UNLOCk IN (lcCtnDtl)
ELSE
  REPLACE Br1     WITH .T.
ENDIF
=lfwCtnDtlBr(loFormSet)
loFormSet.AriaForm1.pgfPacking.CartonInfo.cmdRemoveCartD.ENABLED = .T.
loFormSet.AriaForm1.pgfPacking.CartonInfo.kbStyle.Enabled = .T.
SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfvCtnSty
*! Developer : Mariam Mazhar
*! Date      : 06/09/2003
*! Purpose   : Validat Style field .
*!*************************************************************

FUNCTION lfvCtnSty
LPARAMETERS loFormSet,llBrowse,lcCtnSty
PRIVATE lnCurAlias,llNoThing,lnOrdLine,lnWgh,lcSzCode,lnK,lcFld,;
        lcPQtyFld,lcPWghFld,lcOQtyFld,llAlowAdd
STORE SPACE(0) TO lcSzCode
STORE 0 TO lnOrdLine,lnWgh,lnK
llNoThing = .F.
llAlowAdd = .F.
lcCtnDtl = loFormSet.lcCtnDtl 
lnCurAlias = SELECT(0)
lcCtnHdr = loFormSet.lcCtnHdr
*-- using lcpcklin file with another alias refers to there is a browse
*-- from this file and browsing from the same file on other window terminates
*-- the first one
lcPckLin = loFormSet.lcPckLin
USE (oAriaApplication.WorkDir+loFormSet.lcPckLin)  IN 0 AGAIN ALIAS PackLines ORDER (loFormSet.lcPckLin)
SELECT PackLines
IF llBrowse OR (LASTKEY() = 13 AND !EMPTY(lcCtnSty) AND !SEEK(lcCtnSty,'PackLines'))
  llBrowse = .T.
ELSE
  IF LASTKEY() = 13 AND !EMPTY(lcCtnSty)
    lcCtnSty  = PackLines.Style
  ENDIF
ENDIF   

IF llBrowse
  llBrowse  = .F.
  llNoThing = lfStyBrow(loFormSet)
  lcCtnSty  = IIF(llNoThing,PackLines.Style,SPACE(19))
  lcStySz   = IIF(llNoThing,PackLines.cSize,SPACE(5))
  lnStyQty  = IIF(llNoThing,PackLines.OQty,0)
  lnWgh     = IIF(llNoThing,PackLines.OQty*PackLines.StyWgh,0)
  lcStyno   =IIF(llNoThing, PackLines.cSizeno,'')
ENDIF

IF !EMPTY(lcCtnSty)
  SELECT (lcCtnDtl)
  IF EMPTY(lcStySz) 
    REPLACE Style WITH lcCtnSty
  ELSE
    *-- if the style with order record No. is not found in the carton
    IF !SEEK(STR(&lcCtnHdr..Cart_No,4)+lcCtnSty,lcCtnDtl)
      llAlowAdd = .T.
      IF SEEK(lcCtnSty+STR(lnOrdLine,6),'PackLines') AND;
         PackLines.LPicked 
         *-- This order line has been picked, can not be selected.
         *-- OK
         = gfModalGen("INM44041B00000","Dialog")
         lcCtnSty = SPACE(19)
         lcStySz  = SPACE(5)
         loFormSet.AriaForm1.pgfPacking.CartonInfo.kbStyle.value = ''
         loFormSet.AriaForm1.pgfPacking.CartonInfo.kbSize.value = ''
      ELSE
        IF SEEK(STR(&lcCtnHdr..Cart_No,4)+SPACE(19),lcCtnDtl)
          REPLACE Style      WITH lcCtnSty ,;
                  Br1        WITH .F.      ,;
                  Qty&lcStyno. WITH lnStyQty,;
                  Weight&lcStyno.  WITH lnWgh,;
                  Br&lcStyno.   WITH .T.,;
                  SzCnt      WITH VAL(PackLines.cSizeNo),;
                  OrgWgh     WITH PackLines.StyWgh,;
                  cStatus    WITH "A",;
                  cNoSize WITH lcStyno,;
                  Size&lcStyno. WITH   lcStySz  
            


         loFormSet.lnPackQty = loFormSet.lnPackQty +&lcCtnDtl..Weight&lcStyno.
         loFormSet.lnPackWgh = loFormSet.lnPackWgh + &lcCtnDtl..Qty&lcStyno.
         SELECT(lcCtnHdr)
         REPLACE TotPcs WITH TotPcs +  &lcCtnDtl..Qty&lcStyno.,;
                  TotWgh WITH TotWgh + &lcCtnDtl..Weight&lcStyno.,;
                  Empty  WITH IIF(TotPcs>0,'N','Y')
                  
          = RLOCK(lcCtnHdr) 
          UNLOCk IN (lcCtnHdr)
          SELECT(lcCtnDtl)
        ENDIF
      ENDIF
    ELSE
      
      *-- if the style with order record No. is found in the carton
      *-- we should determine if the user has select or entered
      *-- an existing size or size hasnot added to the carton yet.
      
      *-- Determining if size has been added (Start)
      *-- this by looping for the 8 size fields in carton detail file
      *-- and comparing the content of each size field with the selected
      *-- or entered size which is represented by the variable "lcStySz"
      lnK = 0
      FOR lnK = 1 TO 8
        *-- this means that the size has been added before
        IF lcStySz = EVAL('Size'+STR(lnK,1))
          EXIT
        ENDIF
      ENDFOR
      *-- Determining if size has been added (End)
      
      
      *-- IF lnK>8 means that the size hasnot been added yet 
      *-- and we can added now
      IF lnK>8
        lcFld = 'Br'+STR(lnSzOrd,1)
          REPLACE Style      WITH lcCtnSty ,;
                  Br1        WITH .F.      ,;
                  Qty&lcStyno. WITH lnStyQty,;
                  Weight&lcStyno.  WITH lnWgh,;
                  Br&lcStyno.   WITH .T.,;
                  SzCnt      WITH PackLines.SzCnt,;
                  OrgWgh     WITH PackLines.StyWgh,;
                  cStatus    WITH "A",;
                  cNoSize WITH lcStyno,;
                  Size&lcStyno.  WITH lcStySz   
                  
             loFormSet.lnPackQty = loFormSet.lnPackQty +&lcCtnDtl..Weight&lcStyno.
             loFormSet.lnPackWgh = loFormSet.lnPackWgh + &lcCtnDtl..Qty&lcStyno.

          SELECT(lcCtnHdr)
           REPLACE TotPcs WITH TotPcs +  &lcCtnDtl..Qty&lcStyno.,;
                  TotWgh WITH TotWgh + &lcCtnDtl..Weight&lcStyno.,;
                  Empty  WITH IIF(TotPcs>0,'N','Y')
                  
          = RLOCK(lcCtnHdr) 
          UNLOCk IN (lcCtnHdr)
          SELECT(lcCtnDtl)
      ENDIF
      lnCtnRec = RECNO(lcCtnDtl)
      lnSzRec  = RECNO(lcCartonSz)
      IF SEEK(STR(&lcCtnHdr..Cart_No,4),lcCtnDtl) AND Br1 AND EMPTY(Size1)
         REPLACE Style WITH SPACE(19),;
                 Br1   WITH .F.
         GOTO lnCtnRec IN (lcCtnDtl)
      ENDIF
    ENDIF

*!*      lcExpGma = "SEEK(&lcTmpPck..Pack_ID+&lcTmpPck..cPkColor+&lcTmpPck..cPckSize+&lcTmpPck..cPkVersion+lcCtnSty+STR(lnOrdLine,6),lcPckLin)"
*!*      IF &lcExpGma

*!*        SKIP lnSzOrd - 1 IN (lcPckLin)

*!*        *-- IF lnK>8 means that the size hasnot been added yet 
*!*        *-- and we added to carton detail file, so we can update 
*!*        *-- pack lines file
*!*        IF lnK>8 OR llAlowAdd
*!*          SELECT(lcPckLin)
*!*          lcPQtyFld = 'PQty'+STR(lnSzOrd,1)
*!*          lcPWghFld = 'PWgh'+STR(lnSzOrd,1)
*!*          lcOQtyFld = 'OQty'+STR(lnSzOrd,1)
*!*          REPLACE &lcPQtyFld WITH &lcPQtyFld+lnStyQty,;
*!*                  &lcPWghFld WITH &lcPWghFld+lnWgh   ,;
*!*                  &lcOQtyFld WITH EVAL('OrdQty'+STR(lnSzOrd,1)) - EVAL('PQty'+STR(lnSzOrd,1))
*!*         = RLOCK(lcPckLin)
*!*         UNLOCk IN (lcPckLin)
*!*        ENDIF
*!*      ENDIF
  ENDIF
ENDIF  

= RLOCK(lcCtnDtl)
UNLOCk IN (lcCtnDtl)

=lfwCtnDtlBr(loFormSet)
USE IN PackLines
SELECT(lnCurAlias)
  *!*************************************************************
  *! Name      : lfStyBrow
  *! Developer : Mariam Mazhar
  *! Date      : 03/01/2009
  *! Purpose   : Browse order styles/sizes.
  *!*************************************************************
  *! Calls     :
  *!             Procedures : ....
  *!             Functions  : AriaBrow
  *!*************************************************************
  *! Passed Parameters  : None
  *!*************************************************************
  *! Returns            : None
  *!*************************************************************
  *! Example   : = lfStyBrow()
  *!*************************************************************
FUNCTION lfStyBrow
  LPARAMETERS loFormSet

  PRIVATE lcFields,lnCurAlias,llReturn,lcBrFields,lcFile_Ttl

  STORE SPACE(0) TO lcFields
  llReturn = .F.

  lnCurAlias = SELECT(0)
  lcFields    = "Style,cSize,nOrdLineNo,OQty,StyWgh"
  lcBrFields  = [StyCode=Style:H='Style',]+;
    [cSize:H='Size',]+;
    [StyWgh     :H='Wgh\Unit',]+;
    [OQty:H='Open Quantity']
  lcFile_Ttl  = 'Style Open Quantities'
  lcForExp = "FOR " + IIF(loFormSet.llShwOpn,"EVAL('OQty')>0 AND EVAL('AvlQty')>0","EVAL('AvlQty')>0")

  SELECT PackLines

  DECLARE laTemp[1]

  llReturn  = gfBrows(lcForExp,'Style','laTemp')

  SELECT(lnCurAlias)

  RETURN llReturn


*!*************************************************************
*! Name      : lfvCtDtRem 
*! Developer : Mariam Mazhar
*! Date      : 03/01/2009
*! Purpose   : validate remove button
*!*************************************************************

FUNCTION lfvCtDtRem
 LPARAMETERS loFormSet
PRIVATE lnAlias,lcFltrExp,lcSzFld,lcSvOrd,lnResp,lcSeekPack,lnWgh,lnQty 
lcPckLin = loFormSet.lcPckLin
lcPakIndxSt = loFormSet.lcPakIndxSt 
STORE .T. TO loFormSet.llCupdate,loFormSet.llAnyUpd
lnAlias = SELECT(0)
lcCtnDtl = loFormSet.lcCtnDtl
lcCtnHdr = loFormSet.lcCtnHdr
lcSvOrd = ORDER(lcPckLin)
SET ORDER TO lcPakIndxSt IN (lcPckLin) 

IF gfModalGen("INM44040B00006","Dialog") = 1
  lcSz = &lcCtnDtl..cNoSize
  loFormSet.lnPackWgh = MAX(loFormSet.lnPackWgh - &lcCtnDtl..Weight&lcSz.,0)
  loFormSet.lnPackQty = MAX(loFormSet.lnPackQty - &lcCtnDtl..Qty&lcSz.,0)
  
  SELECT (lcCtnDtl)
  IF SEEK(PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE+lcSz,lcPckLin)
    SELECT(lcPckLin)
    BLANK FIELDS NCARTON,Selected
  ENDIF  
  
  SELECT (lcCtnHdr)
  REPLACE TotPcs WITH MAX(TotPcs - &lcCtnDtl..Qty&lcSz.,0),;
          TotWgh WITH MAX(TotWgh - &lcCtnDtl..Weight&lcSz.,0),;
          Empty  WITH IIF(TotPcs>0,'N','Y')

  =RLOCK(lcCtnHdr) 
  UNLOCk IN (lcCtnHdr)

  lcFld    = 'Br'  + lcSz
  lcSzFld  = 'Size'+ lcSz
  lcQtyFld = 'Qty' + lcSz
  SELECT (lcCtnDtl)
  REPLACE &lcFld     WITH .F. ,;
          &lcSzFld   WITH ''  ,;
          &lcQtyFld  WITH 0
  IF Br1 OR Br2 OR Br3 OR Br4 OR Br5 OR Br6 OR Br7 OR Br8
    REPLACE cStatus WITH 'M'
  ELSE
    REPLACE cStatus WITH 'D'
    DELETE
  ENDIF
  lnMn = 0
  FOR lnM = 1 TO 8
    lnMn = lnMn +1
    IF EVAL('Br'+STR(lnM,1))
      EXIT
    ENDIF
  ENDFOR
  IF lnMn <= 8
    SKIP lnM-1 IN (lcCtnDtl)
  ELSE
    SELECT (lcCtnDtl)
    REPLACE Cart_No WITH 0 ,;
            Style   WITH ''
  ENDIF
  =RLOCK(lcCtnDtl) 
  UNLOCk IN (lcCtnDtl)
  llNoThing = lfwCtnDtlBr(loFormSet)
ENDIF

SET ORDER TO lcSvOrd IN (lcPckLin)
SELECT (lnAlias)


*!*************************************************************
*! Name      : lfvCtHdRem
*! Developer : Mariam MAzhar
*! Date      : 06/09/2003
*! Purpose   : validate Remove Carton button
*!*************************************************************

FUNCTION lfvCtHdRem
 LPARAMETERS loFormSet
PRIVATE lnAlias,lcFltrExp,lnStyRec,lnSzeRec,lcPrvOrd,lcSeek

STORE 0 TO lnStyRec,lnSzeRec

STORE .T. TO loFormSet.llCupdate,loFormSet.llAnyUpd

lnAlias = SELECT(0)
lcSumPck = loFormSet.lcSumPck
lcPckLin = loFormSet.lcPckLin
lcPakIndxSt = loFormSet.lcPakIndxSt 
lcCtnDtl = loFormSet.lcCtnDtl
lcCtnHdr = loFormSet.lcCtnHdr
lcPrvOrd = ORDER(lcPckLin)
SET ORDER TO &lcPakIndxSt IN (lcPckLin)   

*-- "Are You Sure To Delete This Record"
*-- <YES>, <NO>
IF gfModalGen("INM44040B00006","Dialog") = 1

  SELECT(lcPckLin)
  lcFltrExp = SET("FILTER")
  SET FILTER TO Style = ''
  SET RELATION TO

  SELECT(lcCtnDtl)
  SET RELATION TO
  SET KEY TO 
  IF SEEK(STR(&lcCtnHdr..Cart_No,4),lcCtnDtl)
    lnStyRec = RECNO(lcPckLin)
    SCAN REST FOR STR(Cart_No,4)+Style = STR(&lcCtnHdr..Cart_No,4)
      lcSeek = PACK_ID+cPkColor+cPckSize+cPkVersion+Style
      
      SELECT(lcPckLin)   
      IF !EMPTY(&lcCtnDtl..Style) 
        FOR lnI = 1 TO 8
          SELECT(lcPckLin)   
          lcI = STR(lnI,1)
          IF &lcCtnDtl..Br&lcI
            IF SEEK(lcSeek+lcI,lcPckLin)     
              BLANK FIELDS NCARTON
              IF !EMPTY(&lcPckLin..PACK_ID)
                IF SEEK(PADR(&lcPckLin..PACK_ID,19)+&lcPckLin..cPkColor+&lcPckLin..cPckSize+&lcPckLin..cPkVersion,lcSumPck)
                  SELECT &lcSumPck
                  BLANK FIELDS NCARTON
                ENDIF              
              ENDIF
            ENDIF
          ENDIF
        ENDFOR    
      ENDIF

      SELECT (lcCtnDtl)
      REPLACE cStatus WITH 'D'
      DELETE
      
    ENDSCAN
    

  ENDIF
  SELECT (lcPckLin)  
  SET FILTER TO &lcFltrExp
  
  = RLOCK(lcCtnDtl) 
  UNLOCk IN (lcCtnDtl)

  SELECT (lcCtnDtl)
  

  SELECT(lcCtnHdr)
  *-- This means that the carton has the max carton No.
  *-- which means it is the last record in the file
  IF &lcCtnHdr..Cart_No = loFormSet.lnMaxCtn
    SKIP -1
    loFormSet.lnMaxCtn = &lcCtnHdr..Cart_No
    IF !BOF()
      SKIP 1
    ENDIF
  ENDIF

  loFormSet.lnPackCtn = loFormSet.lnPackCtn - 1
 
 
  
  *-- Ahm (START)
  *-- This is instead of subtracting each deleteing line in carton detail
  *-- from Pack weight and quantity 
  loFormSet.lnPackWgh = MAX(loFormSet.lnPackWgh - &lcCtnHdr..TotWgh,0)
  loFormSet.lnPackQty = MAX(loFormSet.lnPackQty - &lcCtnHdr..TotPcs,0)
  *-- Ahm (END)
  
  DELETE
  GO TOP  
  =RLOCK(lcCtnHdr) 
  UNLOCk IN (lcCtnHdr)

  loFormSet.lnMaxCtn = MAX(loFormSet.lnMaxCtn - 1,0)
  loFormSet.lnFrom   = loFormSet.lnMaxCtn + 1
  loFormSet.lnTo     = loFormSet.lnMaxCtn + 1

  llNoThing = lfwCtnHdrBr(loFormSet)
  
  
ENDIF

SET ORDER TO &lcPrvOrd IN (lcPckLin)

SELECT (lnAlias)


*!*************************************************************
*! Name      : lfSavscreen
*! Developer : Mariam Mazhar
*! Date      : 03/01/2009
*! Purpose   : To make local save.
*!*************************************************************

PROCEDURE lfSavscreen
PARAMETERS loFormSet


STORE 2 TO lnCtnTyp
STORE 1 TO lnDrctTo
*-- lnBookQty,lnBookAmt these variables are to update book,bookamt fields in 
*-- ordhdr file
PRIVATE llReturn,lnCurAlias,lcPckHdTag,lnCount,lnBookQty,lnBookAmt,;
        lnOpenQty,lnOpenAmt,llStyDyOpn,lnCurRecNo

lcPckLin = loFormSet.lcPckLin
lcCtnHdr = loFormSet.lcCtnHdr 
lcCtnDtl = loFormSet.lcCtnDtl
lcSumPck = loFormSet.lcSumPck
llStyDyOpn = .F.

STORE 0 To lnBookQty,lnBookAmt,lnRelCho,lnOpenQty,lnOpenAmt
lnCount = 0
lnCurAlias = SELECT(0)

llNoThing = lfUpdVars()  
lcDelStat = SET("DELETED")

PRIVATE lnBookDiff , lnQtyDiff , lnBkAmtDif , lnQyAmtDif , lnOrgBook , lnOrgQty
STORE 0 TO lnBookDiff , lnQtyDiff , lnBkAmtDif , lnQyAmtDif, lnOrgBook , lnOrgQty

SELECT (lcPcklin)
SET FILTER TO
IF !lfGtNtAply()
  RETURN .F.
ENDIF
lcOrder = loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE
lcAcc = loFormSet.AriaForm1.kbAccount.keytextbox.VALUE 
SELECT (lcCtnDtl)
lcCtDtRel = SET("RELATION")
SET RELATION TO
SET FILTER TO
SET KEY TO 
GOTO TOP
IF EOF() OR BOF() OR DELETED()
  *--No Packing list lines were applied. Cannot proceed!
  *-- <OK>
  llNoThing = gfModalGen("INM00000B00000","Dialog",.F.,.F.,'No lines applied, cannot proceed.')  
  llCSave = .F.
  RETURN .F.
ELSE
  *===============this part for updating TMPL_HDR file=========
  lnLastCtn = loFormSet.lnCurrCtn
  SELECT TMPL_HDR

  IF !gfSEEK(lcOrder ,'TMPL_HDR')
    INSERT INTO TMPL_HDR (ORDER) VALUES (lcOrder)
    *gfReplace("")
  ENDIF  
  lnLastNo = TMPL_HDR.nLastLNo    

  *-- Get cartons information
  DIMENSION laCtnHdrSm[1,3]
  laCtnHdrSm = 0
  SELECT COUNT(CART_NO),SUM(TOTPCS),SUM(TOTWGH) ;
  FROM &lcCtnHdr ;
  INTO ARRAY laCtnHdrSm  
  m.Pack_No = ''
  m.Order = lcOrder 
  m.Piktkt = ''
  m.Account = lcAcc 
  m.Store = OrdHdr.Store
  m.Note = ''
  m.ShipVia = Ordhdr.Shipvia 
  m.Tot_Wght = laCtnHdrSm[1,3]
  m.Tot_Cart = laCtnHdrSm[1,1]
  m.Tot_Pcs  = laCtnHdrSm[1,2]
  m.Sp_Inst2 = ''
  m.Sp_Inst1 = ''
  m.LStandCtn =  IIF(lnCtnTyp = 1,.T.,.F.)
  m.CToStorCn =  IIF(lnDrctTo = 1,'S','C')
  m.CPkChCode = IIF(loFormSet.llPCDStat and loFormSet.llEdiAcc,EDIACPRT.cPckChCode ,'')
  m.CPkDsCode =IIF(loFormSet.llPCDStat and loFormSet.llEdiAcc,EDIACPRT.cPckDsCode,'')
  m.LLCKONERR = loFormSet.ariaform1.pgfPacking.Detail.chkError.value
  m.cWareCode = Ordhdr.cWareCode
  
  SELECT TMPL_HDR
  GATHER MEMO  MEMVAR 
  
  =gfAdd_Info('TMPL_HDR')
  =gfReplace("")
ENDIF
  *=================end of updating TMPL_HDR File================
 
 
  *=================This part for saving TMPL_LIN file================
  
  *--Remove all lines relating to order laData[2] in TMPL_LIN file
  *- Note  : after a long time these deleted records may cause the file to be large , it is suggested to pack this file periodically
  IF gfSEEK(lcOrder,'TMPL_LIN') 
    SELECT TMPL_LIN
    SCAN REST WHILE ORDER+STR(NCARTON,4)+PACK_ID+CPKCOLOR+CPKSIZE+CPKVERSION+STYLE = lcOrder      
      gfDELETE()
    ENDSCAN 
    =gfTableUpdate()  
  ENDIF

  SELECT(lcCtnDtl)
  SET FILTER TO 
  SET ORDER TO 0
  SET  RELATION  TO  
  loFormset.lnLastNo = 0  
  = gfSEEK('O'+lcOrder,'OrdHdr')
  SELECT ORDLINE 
  =gfSetOrDer('Ordblkst')
  SELECT(lcCtnDtl)
  SCAN FOR cStatus <> 'S' AND !EMPTY(cNoSize)
    IF &lcCtnDtl..nStep = 0   && for uncomplete session
      SELECT &lcCtnDtl
      IF !SEEK(lcOrder+STR(Cart_No,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style,'TMPL_LIN') 
      *AND    !gfSEEK(lcOrder+STR(Cart_No,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style,'TMPL_LIN')              
      
        SELECT TMPL_LIN
        APPEND BLANK 
        REPLACE ORDER      WITH lcOrder ,;
                NCARTON    WITH &lcCtnDtl..Cart_No,;
                PACK_ID    WITH &lcCtnDtl..PACK_ID    ,;
                CPKCOLOR   WITH &lcCtnDtl..CPKCOLOR   ,;
                CPKSIZE   WITH &lcCtnDtl..CPCKSIZE   ,;
                CPKVERSION WITH &lcCtnDtl..CPKVERSION ,;
                Style      WITH &lcCtnDtl..Style,;
                line_no    WITH  loFormset.lnLastNo + 1
                
                
        loFormset.lnLastNo =  loFormset.lnLastNo + 1       
        =gfAdd_Info('TMPL_LIN')
        =gfReplace('')
      ENDIF    
      SELECT TMPL_LIN      
      lcSzNo = &lcCtnDtl..cNoSize
      *REPLACE Qty1       WITH &lcCtnDtl..Qty1,;
              Qty2       WITH &lcCtnDtl..Qty2,;
              Qty3       WITH &lcCtnDtl..Qty3,;
              Qty4       WITH &lcCtnDtl..Qty4,;
              Qty5       WITH &lcCtnDtl..Qty5,;
              Qty6       WITH &lcCtnDtl..Qty6,;
              Qty7       WITH &lcCtnDtl..Qty7,;
              Qty8       WITH &lcCtnDtl..Qty8,;
              TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8   
                 
      REPLACE Qty&lcSzNo.  WITH &lcCtnDtl..Qty&lcSzNo.,;        
              WEIGHT&lcSzNo.     WITH  &lcCtnDtl..WEIGHT&lcSzNo.,;
              TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8      
              
      IF gfSEEK('O'+lcOrder+&lcCtnDtl..Style+&lcCtnDtl..PACK_ID+&lcCtnDtl..CPKCOLOR+;
              &lcCtnDtl..CPCKSIZE+&lcCtnDtl..CPKVERSION,'ORDLINE') AND ORDLINE.lRange 
              
          =SEEK(PADR(&lcCtnDtl..PACK_ID,19)+&lcCtnDtl..cPkColor+&lcCtnDtl..cPckSize+&lcCtnDtl..cPkVersion,lcSumPck)
          REPLACE NPWGHT WITH &lcCtnDtl..Weight1+&lcCtnDtl..Weight2+&lcCtnDtl..Weight3+&lcCtnDtl..Weight4+ ;
                              &lcCtnDtl..Weight5+&lcCtnDtl..Weight6+&lcCtnDtl..Weight7+&lcCtnDtl..Weight8 ,;
                  WEIGHT WITH NPWGHT/&lcSumPck..CtnTotQty      

        ELSE        

          SELECT TMPL_LIN
          IF TOTQTY>0
            REPLACE NPWGHT WITH Weight1+Weight2+Weight3+Weight4+ ;
                                Weight5+Weight6+Weight7+Weight8 ,;
                    WEIGHT WITH NPWGHT/TOTQTY
          ENDIF

      ENDIF
      
       =gfReplace('')
      SELECT (lcCtnDtl)
      REPLACE &lcCtnDtl..nStep WITH 1
    ENDIF
  ENDSCAN

  SELECT TMPL_LIN
  lcSetDel = SET('DELETE')
  SET DELETE ON
  =SEEK(lcOrder)
  lnI = 0
  llFirst  = .T.
  lnLastCrt = TMPL_LIN.No_Cart
  llSameCrt = .F.
  SCAN REST WHILE ORDER+STR(No_Cart,4)+Style = lcOrder
    IF !llFirst
      llSameCrt = (TMPL_LIN.No_Cart = lnLastCrt)
      IF !llSameCrt 
        lnLastCrt = TMPL_LIN.No_Cart
      ENDIF
    ENDIF
    IF llFirst OR !llSameCrt 
      lnI = lnI + 1
    ENDIF
    REPLACE TMPL_LIN.No_Cart WITH lnI
    =gfReplace("")
    llFirst  = .F.
  ENDSCAN

  lnLastCtn = lnI

  SELECT (lcCtnDtl)
  GO TOP 

  lnI = 0
  llFirst  = .T.
  lnLastCrt = &lcCtnDtl..Cart_No
  llSameCrt = .F.
  SCAN
    IF !llFirst
      llSameCrt = (&lcCtnDtl..Cart_No = lnLastCrt)
      IF !llSameCrt 
        lnLastCrt = &lcCtnDtl..Cart_No
      ENDIF
    ENDIF
    IF llFirst OR !llSameCrt 
      lnI = lnI + 1
    ENDIF
    REPLACE &lcCtnDtl..Cart_No WITH lnI
    llFirst  = .F.
  ENDSCAN
  SET DELETE &lcSetDel
    
  SELECT TMPL_HDR
  REPLACE nLastLno  WITH MAX(nLastLno,loFormset.lnLastNo) ,;
          nLastCart WITH lnLastCtn
          
  =gfReplace('')        
  *=================end saving TMPL_LIN file======================  
  
  
  *------------- Update field STYLE.NSTYWEIGHT if it is zero --------------*
  SELECT &lcPckLin
  LOCATE 
  SCAN
    IF EMPTY(&lcPckLin..PACK_ID) .AND. gfSEEK(&lcPckLin..STYLE,'STYLE') .AND. STYLE.NSTYWEIGHT = 0
      SELECT STYLE
      gfREPLACE('NSTYWEIGHT WITH &lcPckLin..StyWgh')
    ENDIF    
  ENDSCAN  
  
SET DELETED &lcDelStat

IF lnRelCho <> 2
  llReturn = .T.
ELSE
  llReturn = .F.
ENDIF

SELECT(lcCtnDtl)
SET RELATION TO &lcCtDtRel.

*-- ORDHDR file locking
=lfOrdHdLck(.F.)

SELECT OrdLine
lcTag = ORDER()
lnCurRecNo = 0
=gfSetOrder('OrdLinSt')


*!*  IF gfSEEK('O'+lcOrder,'OrdLine') 
*!*    SELECT OrdLine
*!*    lnCurRecNo = RECNO()
*!*    SCAN REST WHILE cOrdType+Order+Store+Style+STR(lineno,6) = 'O'+lcOrder
*!*      = gfObj_Lock(.F.)
*!*    ENDSCAN
*!*  ENDIF
SELECT OrdLine
=gfSetOrder(lcTag)
IF BETWEEN(lnCurRecNo,1,RECCOUNT('ORDLINE'))
  GOTO lnCurRecNo 
ENDIF  
SELECT(lnCurAlias)
SELECT Tmpl_lin
=gfTableUpdate()
SELECT Tmpl_HDr
=gfTableUpdate()
SELECT Style 
=gfTableUpdate()
IF loFormSet.ActiveMode = 'E'
  loFormSet.RECOrdlock(.F.)
ENDIF  
return 

*-- end of lpSavscr.

*!*************************************************************
*! Name      : lfUpdVars
*! Developer : Mariam Mazhar
*! Date      : 03/01/2009
*! Purpose   : Update the variable string 
*!*************************************************************

FUNCTION lfUpdVars

lcFiles = "lcPckLin," + lcPckLin + "," + ORDER(lcPckLin) + ";" + ;
          "lcCtnHdr," + lcCtnHdr + "," + ORDER(lcCtnHdr) + ";" + ;
          "lcCtnDtl," + lcCtnDtl + "," + ORDER(lcCtnDtl) + ";"  
*:**************************************************************************
*:* Name        : lfGtNtAply
*! Developer : Mariam Mazhar
*! Date      : 03/01/2009
*:* Purpose     : Show if some styles are not applied in cartons
*:***************************************************************************
FUNCTION lfGtNtAply
PRIVATE lnAlias,lnRecno,lnCnt
lnAlias = SELECT()
lnRecno = RECNO()
lnCnt = 1
SELECT &lcPckLin

&& 1. If the order contains styles only, when we save the template, a msg always shows up: "Some styles are not applied.." even though we selected all the styles in cartons. (check on order# 999998). 
* The cause of this is that there is record of the order with avlqty = 0 and these are not displayed and when we execute the 
* command Locate for nCarton = 0 it returns that there is lines with nCarton not applied and it show the message that some 
* styles are not applied , 
LOCATE FOR nCarton=0 AND AvlQty>0
IF FOUND()
  lnCnt = gfModalGen('INM00000B00023',.F.,.F.,.F.,'Some styles are not applied to cartons.')
ENDIF
SELECT (lnAlias)
IF BETWEEN(lnRecno,1,RECCOUNT())
  GOTO lnRecno
ENDIF
RETURN (lnCnt = 1)  
*-- end of function.
*:**************************************************************************
*:* Name        : lfOrdHdLck
*! Developer : Mariam Mazhar
*! Date      : 03/01/2009
*:* Purpose     : Lock / un-lock ordhdr file
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfOrdHdLck()
*:***************************************************************************
FUNCTION lfOrdHdLck
PARAMETERS llLock
PRIVATE lnAlias,lcSvOrd,llGoOn
llGoOn = .T.
lnAlias = SELECT()
lcSvOrd = ORDER('ORDHDR')
SELECT ORDHDR
=gfSetOrder('ORDHDR')
=gfSEEK('O'+lcOrder,'ORDHDR')
=gfObj_Lock( llLock )
=gfSetOrder(lcSvOrd)
SELECT (lnAlias)
RETURN llGoOn
*-- end of lfOrdHdLck.
*:**************************************************************************
*:* Name        : lfGetTmpData
*! Developer : Mariam Mazhar
*! Date      : 03/01/2009
*:* Purpose     : Get Template data
*:***************************************************************************
FUNCTION lfGetTmpData
PARAMETERS loFormSet




*=lfCrTmpFiles(loFormSet)
PRIVATE lnAlias,lnUpdtWgh,lcSeek,lcSz,lcSvOrd
lnAlias = SELECT(0)
lnUpdtWgh = 0
lcSvOrd = ORDER('SPCK_HDR')
SELECT Spck_hdr
=gfSetOrder('SPCK_HDRVR')

lcOrderNo = loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE  
=gfSeek('O'+lcOrderNo ,'Ordhdr','Ordhdr')
lcAccount = ordhdr.account
loFormSet.AriaForm1.kbAccount.keytextbox.VALUE = lcAccount 
SELECT TMPL_HDR
IF gfSEEK(lcOrderNo)
  loFormSet.ariaform1.pgfPacking.Detail.chkError.value = TMPL_HDR.LLCKONERR
  loFormSet.lnLastNo = TMPL_HDR.nlastlno
ENDIF  
lcPckLin = loFormSet.lcPckLin
lcSumPck = loFormSet.lcSumPck
lcCtnHdr = loFormSet.lcCtnHdr
lcCtnDtl = loFormSet.lcCtnDtl
DIMENSION laPckId[1]
laPckId[1] = ' '
lnPckLn = 0
llRng = .F.
SELECT TMPL_LIN
=gfSEEK(lcOrderNo)
SCAN REST WHILE ORDER+STR(NCARTON,4)+PACK_ID+CPKCOLOR+CPKSIZE+CPKVERSION+STYLE = lcOrderNo
  =gfSEEK(TMPL_LIN.Style,'STYLE')
  =gfSEEK('S'+STYLE.SCALE,'SCALE')
  =gfSEEK('P'+lcAccount+PACK_ID+CPKCOLOR+CPKSIZE+CPKVERSION,'SPCK_HDR') OR ;
   gfSEEK('P*****'+PACK_ID+CPKCOLOR+CPKSIZE+CPKVERSION,'SPCK_HDR')
  =lfPackQty()                && Update nPckQty field if it 0
  DIMENSION laCtn[TMPL_HDR.TOT_CART]
  FOR lnI = 1 TO 8
    lcSz = STR(lnI,1)
    SELECT TMPL_LIN
    IF !EMPTY(QTY&lcSz) AND SEEK(PACK_ID+CPKCOLOR+CPKSIZE+CPKVERSION+STYLE+lcSz,lcPckLin)

      SELECT &lcPckLin
      REPLACE &lcPckLin..NCARTON WITH TMPL_LIN.NCARTON,;
              &lcPckLin..CtnQty  WITH TMPL_LIN.Qty&lcSz,;
              &lcPckLin..StyWgh  WITH TMPL_LIN.WEIGHT

      REPLACE &lcPckLin..DisCtnQty WITH TMPL_LIN.Qty&lcSz


      =SEEK(IIF(EMPTY(TMPL_LIN.PACK_ID),TMPL_LIN.STYLE,PADR(TMPL_LIN.PACK_ID,19))+TMPL_LIN.cPkColor+TMPL_LIN.cPkSize+TMPL_LIN.cPkVersion,lcSumPck)
      SELECT &lcSumPck
      IF EMPTY(&lcSumPck..NCARTON)     && if there is data befor , empty it to collect from tmpl_lin file
        REPLACE &lcSumPck..CtnTotQty WITH 0,;
                &lcSumPck..PTotWgh   WITH IIF(!llPack,0,&lcSumPck..PTotWgh)
      ENDIF
       
      IF !&lcSumPck..LRange .OR. (&lcSumPck..LRange .AND. &lcSumPck..CtnTotQty = 0 )
        REPLACE &lcSumPck..NCARTON   WITH TMPL_LIN.NCARTON,;  
                &lcSumPck..CtnTotQty WITH &lcSumPck..CtnTotQty + TMPL_LIN.Qty&lcSz,; 
                &lcSumPck..PTotWgh   WITH IIF(!llPack,TMPL_LIN.WEIGHT,&lcSumPck..PTotWgh),;
                &lcSumPck..PkCtnQty  WITH IIF(!llPack,0,&lcSumPck..CtnTotQty/SPCK_HDR.NPCKQTY)                
      ENDIF
      IF &lcSumPck..LRange
        REPLACE &lcSumPck..PkCtnQty WITH 0 ;
                &lcSumPck..PTotWgh  WITH TMPL_LIN.WEIGHT
      ENDIF      
      
      *--Ask for the carton header
      IF !SEEK(STR(TMPL_LIN.NCARTON,4),lcCtnHdr)
        INSERT INTO (lcCtnHdr)(Cart_No         ,TotPcs ,TotWgh   ,Empty);
                       VALUES (TMPL_LIN.NCARTON,0      ,0        ,'N')
      ENDIF
      
      *--Ask for the carton Details
      SELECT TMPL_LIN
      IF !SEEK(STR(nCarton,4)+PACK_ID+CPKCOLOR+CPKSIZE+CPKVERSION+Style,lcCtnDtl)
        SELECT &lcCtnDtl
        APPEND BLANK
        REPLACE PACK_ID     WITH &lcPckLin..PACK_ID     ,;
                CPKCOLOR    WITH &lcPckLin..CPKCOLOR    ,;
                CPCKSIZE    WITH &lcPckLin..CPCKSIZE    ,;
                CPKVERSION  WITH &lcPckLin..CPKVERSION  ,;
                Style       WITH &lcPckLin..Style       ,;
                OrgWgh      WITH &lcPckLin..OrgStyWgh   ,;
                SzCnt       WITH SCALE.CNT,;
                cStatus     WITH "A",;
                Cart_No     WITH &lcPckLin..NCARTON

        IF !laCtn[&lcPckLin..NCARTON]
          REPLACE lFlag WITH .T.
          IF &lcPckLin..lRange
            laCtn[&lcPckLin..NCARTON] = .T.  && Allow to show only the first line for range packs
          ENDIF
        ENDIF
	  ELSE
	    SELECT &lcCtnDtl
	    llFound = .F.
	    SCAN REST WHILE STR(Cart_No,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style = ;
	    				STR(TMPL_LIN.nCarton,4)+TMPL_LIN.PACK_ID+TMPL_LIN.CPKCOLOR+TMPL_LIN.CPKSIZE+TMPL_LIN.CPKVERSION+TMPL_LIN.Style ;
	    				FOR cNoSize = lcSz
	      
  	      llFound = .T. 				
	    ENDSCAN  		
	    IF !llFound 
        SELECT &lcCtnDtl
	      APPEND BLANK
        REPLACE PACK_ID     WITH &lcPckLin..PACK_ID     ,;
                CPKCOLOR    WITH &lcPckLin..CPKCOLOR    ,;
                CPCKSIZE    WITH &lcPckLin..CPCKSIZE    ,;
                CPKVERSION  WITH &lcPckLin..CPKVERSION  ,;
                Style       WITH &lcPckLin..Style       ,;
                OrgWgh      WITH &lcPckLin..OrgStyWgh   ,;
                SzCnt       WITH SCALE.CNT,;
                cStatus     WITH "A",;
                Cart_No     WITH &lcPckLin..NCARTON,;
                cNoSize     WITH  lcSz
                

	    ENDIF
    ENDIF
      
      SELECT &lcCtnDtl
      REPLACE Size&lcSz   WITH &lcPckLin..cSize,;
              Br&lcSz     WITH !EMPTY(Size&lcSz) ,;
              Qty&lcSz    WITH TMPL_LIN.Qty&lcSz,;
              TOTQTY      WITH TOTQTY + Qty&lcSz,;
              Weight&lcSz WITH TMPL_LIN.WEIGHT*TMPL_LIN.Qty&lcSz,;
              TotWeight   WITH TotWeight + Weight&lcSz ,;
              cStatus     WITH IIF(&lcPckLin..Selected>0,'M',cStatus),;
              cNoSize     WITH lcSz
      
      lnPkQty = TMPL_LIN.Qty&lcSz
      lnWght  = &lcCtnDtl..Weight&lcSz
      lcPckId = &lcPckLin..PACK_ID+&lcPckLin..cPkColor+&lcPckLin..cPckSize+&lcPckLin..cPkVersion+STR(TMPL_LIN.NCARTON)
      IF ASCAN(laPckId , lcPckId ) = 0
        lnPckLn = lnPckLn + 1
        DIMENSION laPckId[lnPckLn]
        laPckId[lnPckLn] = lcPckId
      ELSE
        IF &lcPckLin..LRANGE
          lnPkQty = 0
          lnWght  = 0
        ENDIF
      ENDIF

      REPLACE &lcCtnHdr..TotPcs WITH &lcCtnHdr..TotPcs + lnPkQty ,;
              &lcCtnHdr..TotWgh WITH &lcCtnHdr..TotWgh + lnWght
    ENDIF
  ENDFOR
ENDSCAN
SELECT  SPCK_HDR
gfSETORDER(lcSvOrd)
SELECT (lcCtnDtl)
*:**************************************************************************
*:* Name        : lfPackQty
*:* Developer : Mariam Mazhar
*:* Date        : 03/01/2009
*:* Purpose     : Get pack QTY from SPCK_LIN file and update SPCK_HDR.NPCKQTY if it is 0
*:***************************************************************************
FUNCTION lfPackQty
PRIVATE lnSlct,lnQty,lcSvOrd
lnSlct = SELECT()
lnQty = 0

SELECT SPCK_HDR
lcSvOrd = ORDER('SPCK_LIN')
SELECT  SPCK_LIN
=gfSetOrder("SPCK_LINVR")
SELECT SPCK_HDR
iF gfSEEK('P'+ACCOUNT+Pack_Id+cPkColor+cPckSize+cPkVersion,'SPCK_LIN')
  SELECT SPCK_LIN
  SCAN REST WHILE TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE = ;
                  'P'+SPCK_HDR.ACCOUNT+SPCK_HDR.Pack_Id+SPCK_HDR.cPkColor+SPCK_HDR.cPckSize+SPCK_HDR.cPkVersion
    lnQty = lnQty + SPCK_LIN.TOTQTY
  ENDSCAN
ENDIF
SELECT SPCK_HDR
gfREPLACE("NPCKQTY WITH lnQty")
=gfTableUpdate()
SELECT SPCK_LIN    
=gfSetOrder(lcSvOrd)

SELECT (lnSlct)

*-- end of lfPackQty.
  *!*************************************************************
  *! Name      : lfvAccount
  *! Developer : Mariam Mazhar
  *! Date      : 03/01/2009
  *! Purpose   : To validate the account code
  *!*************************************************************
  *! Parameters: loFormSet : ThisFormSet
  *!*************************************************************
FUNCTION lfvAccount
  LPARAMETERS loFormSet, lcAccount, llBrowse

  IF llBrowse OR (!EMPTY(lcAccount) AND !gfSEEK('M'+lcAccount,'Customer'))
    DO CUSBROWM WITH lcAccount
  ENDIF
  loFormSet.AriaForm1.kbAccount.keytextbox.VALUE = lcAccount
  loFormSet.AriaForm1.txtCustName.VALUE = IIF(!EMPTY(lcAccount),Customer.stName,'')
  RETURN


*!*************************************************************
*! Name      : lpDelScr
*! Developer : Mariam Mazhar
*! Date      : 03/01/2009
*! Purpose   : To make local delete.
*!*************************************************************

PROCEDURE lfDelScr
PARAMETERS loFormSet
lcPckLin = loFormSet.lcPckLin
lcCtnHdr = loFormSet.lcCtnHdr 
lcCtnDtl = loFormSet.lcCtnDtl
lcSumPck = loFormSet.lcSumPck
lcOrder = loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE
lcAcc = loFormSet.AriaForm1.kbAccount.keytextbox.VALUE 

PRIVATE lnCurAlias,lcHdrTag,lcLinTag,llShiped 

IF gfSEEK(lcOrder,'TMPL_HDR')
  IF TMPL_HDR.Status = 'C'
    PRIVATE lcMessage
    *-- This packing list is shipped. Can't be Deleted
    *-- OK
    lcMessage = "Can't be Deleted."
    = gfModalGen("INM44102B00000","Dialog",lcMessage)  
    *-- Return to "SELECT" mode
    RETURN .F.
  ENDIF
ENDIF

lnCurAlias = SELECT(0)

lcHdrTag = ORDER('TMPL_HDR')
SELECT TMPL_HDR
=gfSetOrder('TMPL_HDR')
IF gfSEEK(lcOrder,'TMPL_HDR')
  SELECT TMPL_HDR
  gfDELETE() 
ENDIF  
=gfSetOrder(lcHdrTag)

lcLinTag = ORDER('TMPL_LIN')
IF gfSEEK(lcOrder,'TMPL_LIN')
  SELECT TMPL_LIN
  SCAN REST WHILE ORDER+STR(NCARTON,4)+PACK_ID+CPKCOLOR+CPKSIZE+CPKVERSION+STYLE+DYELOT= lcOrder
    gfDELETE() 
  ENDSCAN
ENDIF  
=gfSetOrder(lcLinTag)
SELECT TMPL_LIN
=gfTableUpdate()
SELECT TMPL_HDR
=gfTableUpdate()
loFormSet.ChangeMode('S')
SELECT(lnCurAlias)

