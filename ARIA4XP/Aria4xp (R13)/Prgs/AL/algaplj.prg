*:***********************************************************************
*:  Program file : ALGAPLJ.PRG
*:  Program desc.: Automatic Packing List (Custom for JET10)
*:         System: Aria 4XP
*:      Developer: Mariam Mazhar
*:           Date: 03/01/2009
*:      Reference: C201109[T20081208.0016]
*:************************************************************************
*: Modifications:
*: B609154,1 MMT 02/24/2010 Fix bug of error when run program on SAAS[T20100223.0003]
*: B609154,2 MMT 03/01/2010 Fix bug of error when run program on SAAS[T20100223.0003]
*: B609229,1 MMT 04/29/2010 Fix bug of error when run program on SAAS[T20100419.0022]
*! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[T20110314.0033]
*!B611093,1 AEG 2015-12-14 Two Diffrent packing list have the same UCC9 [T20151111.0001]
*:************************************************************************
#INCLUDE R:\ARIA4XP\PRGS\ALPLIST.h
*: B609154,1 MMT 02/24/2010 Fix bug of error when run program on SAAS[Start]
If oAriaApplication.MULTIINST
    *: B609229,1 MMT 04/29/2010 Fix bug of error when run program on SAAS[Start]
    *DO FORM ("X:\aria4xp\Screens\"+'AL\ALGAPLJ.SCX')
    =gfCallForm('ALGAPLJ','AL')
    *: B609229,1 MMT 04/29/2010 Fix bug of error when run program on SAAS[End]
Else
    *: B609154,1 MMT 02/24/2010 Fix bug of error when run program on SAAS[End]
    Do Form (oAriaApplication.ScreenHome+'AL\ALGAPLJ.SCX')
    *: B609154,1 MMT 02/24/2010 Fix bug of error when run program on SAAS[Start]
Endif
*: B609154,1 MMT 02/24/2010 Fix bug of error when run program on SAAS[End]

*!*************************************************************
*! Name      : lfFormInit
*: Developer : Mariam Mazhar
*: Date      : 03/01/2009
*! Purpose   : Init Method of the Form
*!*************************************************************
Function lfFormInit
Parameters loFormSet
Store 0 To loFormset.lnFrom,loFormset.lnTo,loFormset.lnCtnPal,loFormSet.lnPackWgh
loFormset.llUpdtPkTk = .F.
If File (oAriaApplication.DataDir+'UpPkTk.MEM')
    Restore From (oAriaApplication.DataDir+'UpPkTk.MEM') Additive
    loFormset.llUpdtPkTk = llUpdtPkTk
Endif

Store '' To loFormset.cPckDsCode,loFormset.cPckChCode,loFormset.lcWareCode
loFormset.llComp = .F.
loFormset.llDyelot = .F.
Store 0 To  loFormset.lnDrctTo
Store 0 To loFormset.lnCtnTyp  ,loFormset.lnMaxCtn
loFormset.lcBol  = ''
loFormSet.llAlwForce = lfSuppForc(loFormSet)
loFormSet.ariaBrFields.edtBrowseFields.Value = "Order:H='Order',Account:H= 'Account',"+;
    "Status:H='Status',TOT_wght:H='Tot. Weight',"+;
    "tot_cart:H='Tot. Carton',Tot_pcs:H='Tot. Pcs',cwarecode:H='Warehouse'"

=gfOPenTable('TMPL_LIN','TMPL_LIN')
=gfOPenTable('TMPL_HDR','TMPL_hDr')
=gfOPenTable('OrdHdr','OrdHdr')
=gfOPenTable('PIKTKT','PIKTKT')
=gfOPenTable('Pack_Hdr','Pack_Hdr')
=gfOPenTable('Customer','Customer')
=gfOPenTable('SCALE','SCALE')
=gfOPenTable('StyLE','StyLE')
=gfOPenTable('Pack_Lin','Pack_Lin')
=gfOPenTable('Pikline','Pikline')
=gfOPenTable('Ordline','Ordline')
=gfOPenTable('SPck_Hdr','SPCK_HDRVR')
=gfOPenTable('SPck_lin','SPCK_LINVR')
loFormSet.lcPckLin   = gfTempName()
loFormSet.lcPack_Lin = gfTempName()
loFormSet.lcCtnHdr   = gfTempName()
loFormSet.lcStores   = gfTempName()
loFormSet.lcCtnDtl   = gfTempName()
loFormSet.lcTmpPck  = gfTempName()
loFormSet.lcTmpIdx  = gfTempName()
loFormSet.lcTmpUpLn = gfTempName()
loFormSet.lcSumPck  = gfTempName()
loFormSet.lcPakIndxSt = gfTempName()
loFormSet.lcPakIndxLn = gfTempName()
loFormSet.llCanPrnLb = gfUserPriv('AL','ALPLIST','PRNPACKING')
loFormSet.lcErorFil= gfTempName()
loFormSet.llAsnSys   = ('AS' $ oAriaApplication.CompanyInstalledModules)
loFormSet.llAloModul = ('AL' $ oAriaApplication.CompanyInstalledModules)
loFormset.llDyelot = Alltrim(gfGetMemVar('M_DYELOT',oAriaApplication.ActiveCompanyID))='Y'


loFormSet.lcTmpUpLn =  gfTempName()
loFormSet.llExtSizSc = gfGetMemVar('M_USEEXSSC',oAriaApplication.ActiveCompanyID)
loFormSet.lcSizeSep  = ''
Store 0 To lnSizePos,lnSizeLen
If loFormSet.llExtSizSc
    Declare laStySeg[1,1]
    Store "" To laStySeg
    =gfItemMask(@laStySeg)
    For lnCnt = 1 To Alen(laStySeg,1)
        If laStySeg[lnCnt , 1] = "S"
            loFormSet.lcSizeSep  = Alltrim(laStySeg[lnCnt-1 , 6])
            loFormSet.lnSizePos  = laStySeg[lnCnt , 4] - Iif(!Empty(loFormSet.lcSizeSep) , 1 , 0)
            loFormSet.lnSizeLen  = Len(laStySeg[lnCnt , 3]) + Iif(!Empty(loFormSet.lcSizeSep) , 1 , 0)
        Endif
    Endfor
Endif

loFormSet.lcStyPic   = gfItemMask("PI")
loFormSet.lcStyTtl   = gfItemMask("HI")
loFormSet.lnStyleWid = Len(loFormSet.lcStyPic)
loFormSet.lcTmAsnShp = gfTempName()
If loFormSet.llCanPrnLb

    =gfOPenTable('Asn_Ship','Asn_Ship')
    If !Used(loFormSet.lcTmAsnShp)
        Select 'Asn_Ship'
        Copy Structure To (oAriaApplication.WorkDir+loFormSet.lcTmAsnShp)
        =gfOpenFile(oAriaApplication.WorkDir+loFormSet.lcTmAsnShp,'','EX')
        Select (loFormSet.lcTmAsnShp)
        Index On Str(CART_NO,6) Tag (loFormSet.lcTmAsnShp)
    Endif
    =gfOpenTable(oAriaApplication.SysPath+'SYCASNLB','ASNlbl')
    llDetLabel = gfSEEK("XX1" + "H" , "SYCASNLB") .And. gfSEEK("XX1" + "L" , "SYCASNLB")

    If llDetLabel
        *-- Flag to know if UPC module is installed or not
        loFormSet.llUPCInst  = ('UP' $ oAriaApplication.CompanyInstalledModules)
        *-- Flag to know if the user want to print detailed shipping label for all cartons or not
        loFormSet.lcDetLbAll = ""

        If loFormSet.llUPCInst
            =gfOpenTable( 'STYLEUPC' ,  'STYLEUPC' )
        Endif
    Endif

Endif

loFormSet.llEdiSys   = ('AS' $ oAriaApplication.CompanyInstalledModules)
If loFormSet.llEdiSys
    =gfOPenTable('EDIAcPrt','ACCFACT')
    =gfOPenTable('EDIPH','PARTNER')
    =gfOPenTable('EDICRTSQ','EDICRTSQ')
    =gfOPenTable('BOL_HDR','BOL_HDR')
    =gfOPenTable('BOL_LIN','BOL_LIN')
Endif


With loFormSet
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
Endwith


*!*************************************************************
*! Name      : lfvOrderNo
*: Developer : Mariam Mazhar
*: Date      : 03/01/2009
*! Purpose   : Order number validation
*!*************************************************************
Function lfvOrderNo
Lparameters loFormSet, lcOrderNo,lcAccount, llBrowse
Private lnCurAlias,lcOrdHdrTg,lcOrder
lnCurAlias = Select(0)

lcOrdHdrTg = Order('OrdHdr')
Select Ordhdr
=gfSetOrder('OrdHdr')

loFormSet.ariaform1.pgfPacking.Header.grdHeadr.RecordSource = ''
loFormSet.AriaForm1.pgfPacking.Detail.grdDetail.RecordSource = ''
loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonH.RecordSource = ''
loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonD.RecordSource = ''




If !( (Empty(lcOrderNo) Or Lastkey() <> 13) And !llBrowse)
    If (!Empty(lcOrderNo) .And. !gfSEEK('O'+lcOrderNo,'OrdHdr') ) Or llBrowse
        lcOrder = lcOrderNo
        =lfOrdBrow(@lcOrder,lcAccount)
        lcOrderNo = lcOrder
        loFormSet.ariaForm1.kbOrderNo.keytextbox.Value = lcOrderNo
        llBrowse = .F.
    Endif
Endif


If !Empty(lcOrderNo) And gfSEEK('O'+lcOrderNo,'OrdHdr')
    lcAlias = Select(0)
    Select Pack_Hdr
    lcTmpKey = pack_no
    lcOrdr = Order()
    =gfSetOrder('OrderPck')
    llPacked = gfSEEK(OrdHdr.Order+OrdHdr.Store)
    =gfSetOrder(lcOrdr )
    =gfSEEK(lcTmpKey)
    =gfSEEK('O'+lcOrderNo ,'OrdHdr')
    Select (lcAlias)

    Do Case

        *--- and the orde packed.[Begin]
    Case OrdHdr.Status = 'C' And llPacked
        =gfModalGen("INM000000B00000","DIALOG",'','',;
            'This order is completed and packed, cannot pack.')
        loFormSet.ariaForm1.kbOrderNo.keytextbox.Value  = Space(6)
        llPacked = .F.

    Case OrdHdr.Status = 'X'
        *-- This order is canceled, cannot pack.
        *-- OK
        = gfModalGen("INM44050B00000","Dialog","canceled")
        loFormSet.ariaForm1.kbOrderNo.keytextbox.Value = Space(6)


    Case OrdHdr.bulk = 'Y'
        *-- This order is bulk, cannot pack.
        *-- OK
        = gfModalGen("INM44050B00000","Dialog","bulk")
        loFormSet.ariaForm1.kbOrderNo.keytextbox.Value = Space(6)


    Otherwise

        If !gfSEEK(lcOrderNo,'TMPL_LIN')
            =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No tamplate saved for this order, can not proceed.')
            Select Ordhdr
            =gfSetOrder(lcOrdHdrTg)
            loFormSet.ariaForm1.kbOrderNo.keytextbox.Value = Space(6)
            Select (lnCurAlias)
            Return
        Endif
        =gfSEEK(lcOrderNo,'TMPL_HDR')
        llNothing = lfCrtUnCmp(loFormSet)
        loFormset.llComp= (OrdHdr.Status = 'C' And !llPacked)
        =lfGetData(loFormSet, lcOrderNo)
        Select PIKTKT
        =gfSetOrder('ORDPIK')

        *-- llHaSPik variable that indicates if this order has pikTkt or not
        If gfSEEK(lcOrderNo,'PikTkt')
            lnAlias = Select(0)
            Select PikTkt
            Locate Rest While Order = lcOrderNo For Status $ 'PO'
            If Found()
                llHasPik = .T.
            Else
                llHasPik = .F.
            Endif
            Select (lnAlias)
        Else
            llHasPik = .F.
        Endif
        =lfvNewPack(loFormSet)
        lcPckHdrOrd = Order('pack_hdr')
        Select pack_hdr
        =gfSetOrder('ORDERPCK')
        If !gfSEEK(lcOrderNo ,'PACK_HDR')
            =gfSetOrder(lcPckHdrOrd)
            loFormSet.ChangeMode('A')
        Else
            =gfSetOrder(lcPckHdrOrd)
            loFormSet.ChangeMode('V')
        Endif
    Endcase
Endif
Select Ordhdr
gfSetOrder(lcOrdHdrTg)
Select (lnCurAlias)

*!*************************************************************
*! Name      : lfGetData
*: Developer : Mariam Mazhar
*: Date      : 03/01/2009
*! Purpose   : Get Order data
*!*************************************************************
Function lfGetData
Lparameters loFormSet, lcOrderNo
Private lcPikTkt, lcStore
llNoThing = gfSEEK('O'+lcOrderNo,'OrdHdr')
With loFormSet.ariaForm1
    Store Iif(!Empty(lcOrderNo),OrdHdr.Account,Space(6)) To .kbAccount.keytextbox.Value, lcAccount

    loFormSet.llEdiAcc   = loFormSet.llEdiSys .And. gfSEEK('A' + lcAccount, 'EDIACPRT') .And.;
        gfSEEK(EDIACPRT.cPartCode , 'EDIPH')

    Store loFormSet.llEdiSys .And. loFormSet.llEdiAcc .And. EDIACPRT.lPkChrDes  To llPCDStat
    Store loFormSet.llEdiSys .And. loFormSet.llEdiAcc .And. EDIPH.lPltShp  To llPalStat

    lcStore  = OrdHdr.Store
    lcCusPo    = OrdHdr.CustPo
    lcDept     = OrdHdr.Dept
    .txtCustPo.Value = OrdHDR.CustPo
    loFormSet.lcWareCode = OrdHdr.cWareCode
    If  loFormSet.llEdiAcc
        loFormSet.cPckChCode =  EDIACPRT.cPckChCode
        loFormSet.cPckDsCode  =  EDIACPRT.cPckDsCode
    Else
        loFormSet.lnCtnTyp   = 2
        loFormSet.lnDrctTo   = 1
    Endif
Endwith
*!*************************************************************
*! Name      : lfvNewPack
*: Developer : Mariam Mazhar
*: Date      : 03/01/2009
*! Purpose   : Get new or existing packing list data
*!*************************************************************
Function lfvNewPack
Lparameters loFormSet
Private lcTag,lnCurAlias
Store .T. To loFormSet.llCupdate,loFormSet.llAnyUpd,loFormSet.llNew
lcAccount = Ordhdr.account
lcStore = Ordhdr.Store
If Alltrim(OrdHdr.ShipVia)='*' And gfSEEK('S'+lcAccount+lcStore ,'Customer')
    lcShipVia  = Customer.ShipVia
Endif
lcPckHdrOrd = Order('PACK_HDR')
Select PACK_HDR
=gfSetOrder('ORDERPCK')
*-- Get Order stores
=lfGtOrdSto(loFormSet)
lcStores = loFormSet.lcStores
Private lcTmplOrd
lcTmplOrd = Order('TMPL_LIN')
Select  TMPL_LIN
=gfSetOrder('TMPL_LINS')
Select &lcStores
Go Top
Scan
    Wait Window Nowait 'Collecting data for store '+&lcStores..Store + ' D.C '+ &lcStores..DIST_CTR
    =lfvSelOrdL(loFormSet)
Endscan
Select &lcStores
Go Top
Wait Clear
Select PACK_HDR
=gfSetOrder(lcPckHdrOrd)
Select TMPL_LIN
=gfSetOrder(lcTmplOrd)
lcPckLin = loFormSet.lcPckLin
Select (lcPckLin)
Go Top
=Rlock(lcPckLin)
Unlock In (lcPckLin)
lcTmpPck = loFormSet.lcTmpPck
If !Used(lcTmpPck)
    If gfGetMemVar('M_ORDSTUTS',oAriaApplication.ActiveCompanyID) = 'L'
        Use (oAriaApplication.WorkDir+lcPckLin)  In 0 Again Alias (lcTmpPck) Order (loFormSet.lcPakIndxLn)
    Else
        Use (oAriaApplication.WorkDir+lcPckLin)  In 0 Again Alias (lcTmpPck) Order (loFormSet.lcPakIndxSt)
    Endif
Endif

lfDtlBrow(loFormSet)
*:**************************************************************************
*:* Name        : lfGtOrdSto
*:* Developer   : MMT -Mariam Mazhar
*:* Date        : 03/01/2009
*:* Purpose     : Get Order stores
*:***************************************************************************
Function lfGtOrdSto
Parameters loFormSet
Private lcSvOrd,lcGtStore,lnStoOrdrd,lcSvRel,lcSvPkOrd
lnStoOrdrd = 0
lcSvOrd = Order('ORDLINE')
lcSvPkOrd = Order('PIKLINE')
Select  ORDLINE
=gfSetOrder("ORDLINST")
Select PIKLINE
=gfSetOrder("PIKLINEO")
Select PIKLINE
lcSvRel = Set('RELATION')
Set Relation To
Go Top
lcGtStore = ' '
*! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
lcGtPikTkt = '  '
*! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
lcOrderNo = loFormSet.ariaForm1.kbOrderNo.keytextbox.Value
=gfSEEK('O'+lcOrderNo ,'ORDLINE')
Select ORDLINE
lcStores = loFormSet.lcStores
*! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
*!*	SCAN REST WHILE CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+lcOrderNo
*!*	  IF lcGtStore # ORDLINE.STORE
*!*	    lcGtStore = ORDLINE.STORE
*!*	    =gfSEEK('S'+ORDLINE.ACCOUNT+ORDLINE.STORE,'CUSTOMER')
*!*	    =gfSEEK(lcOrderNo  +ORDLINE.STORE,'PACK_HDR')
*!*	    INSERT INTO (loFormSet.lcStores) (DIST_CTR, STORE, PIKTKT, TOTORD, TOTPIK, PACK_NO, TOTPQTY, WEIGHT, CARTONS, BOL_NO ) ;
*!*	                    VALUES (CUSTOMER.DIST_CTR, ORDLINE.STORE, ORDLINE.PIKTKT, ORDLINE.TOTBOOK, ;
*!*	                            ORDLINE.TOTPIK, PACK_HDR.PACK_NO, PACK_HDR.TOT_PCS , PACK_HDR.TOT_WGHT ,PACK_HDR.TOT_CART,;
*!*	                            PACK_HDR.BILL_LADG )
*!*	    IF gfSEEK(ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'PIKLINE')
*!*	      SELECT PikLine
*!*	      SCAN REST WHILE ORDER+STR(LINENO,6) = ORDLINE.ORDER+STR(ORDLINE.LINENO,6) FOR gfSeek(PIKLINE.PIKTKT,'PIKTKT','PIKTKT') AND PIKTKT.Status <> 'X'
*!*	        SELECT (loFormSet.lcStores)
*!*	        REPLACE PIKTKT WITH PIKLINE.PIKTKT,;
*!*	                TOTPIK WITH PIKLINE.TOTPIK
*!*	        EXIT
*!*	      ENDSCAN
*!*	    ENDIF
*!*	  ELSE
*!*	    REPLACE &lcStores..TOTORD WITH &lcStores..TOTORD + ORDLINE.TOTBOOK,;
*!*	            &lcStores..TOTPIK WITH &lcStores..TOTPIK + ;
*!*	            IIF(gfSEEK(ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'PIKLINE'),PIKLINE.TOTPIK,ORDLINE.TOTPIK)
*!*	  ENDIF
*!*	ENDSCAN
Select * From Ordline Where CORDTYPE+Order+Store+Style+Str(Lineno,6) = 'O'+lcOrderNo And !Empty(PIKTKT) Order By Store,PIKTKT,Style,Lineno Into Cursor 'TmpLines'
Select 'TmpLines'
Locate
Scan
    If lcGtStore # TmpLines.Store Or lcGtPikTkt  # TmpLines.PIKTKT
        lcGtPikTkt  = TmpLines.PIKTKT
        lcGtStore = TmpLines.Store
        =gfSEEK('S'+TmpLines.ACCOUNT+TmpLines.Store,'CUSTOMER')
        =gfSEEK(lcOrderNo  +TmpLines.Store+lcGtPikTkt ,'PACK_HDR')
        Insert Into (loFormSet.lcStores) (DIST_CTR, Store, PIKTKT, TOTORD, TOTPIK, PACK_NO, TOTPQTY, WEIGHT, CARTONS, BOL_NO ) ;
            VALUES (CUSTOMER.DIST_CTR, TmpLines.Store, TmpLines.PIKTKT, TmpLines.TOTBOOK, ;
            TmpLines.TOTPIK, PACK_HDR.PACK_NO, PACK_HDR.TOT_PCS , PACK_HDR.TOT_WGHT ,PACK_HDR.TOT_CART,;
            PACK_HDR.BILL_LADG )
        If gfSEEK(TmpLines.Order+Str(TmpLines.Lineno,6),'PIKLINE')
            Select PikLine
            Scan Rest While Order+Str(Lineno,6) = TmpLines.Order+Str(TmpLines.Lineno,6) For gfSeek(PIKLINE.PIKTKT,'PIKTKT','PIKTKT') And PIKTKT.Status <> 'X'
                Select (loFormSet.lcStores)
                Replace PIKTKT With PIKLINE.PIKTKT,;
                    TOTPIK With PIKLINE.TOTPIK
                Exit
            Endscan
        Endif
    Else
        Replace &lcStores..TOTORD With &lcStores..TOTORD + TmpLines.TOTBOOK,;
            &lcStores..TOTPIK With &lcStores..TOTPIK + ;
            IIF(gfSEEK(TmpLines.Order+Str(TmpLines.Lineno,6),'PIKLINE'),PIKLINE.TOTPIK,TmpLines.TOTPIK)
    Endif
Endscan
*! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]

Go Top In &lcStores
= lfwStrsBr(loFormSet)

Select PIKLINE
=gfsetOrder(lcSvPkOrd )
Select ORDLINE
=gfsetOrder(lcSvOrd )
*-- end of lfGtOrdSto.
*:**************************************************************************
*:* Name        : lfCrtUnCmp
*:* Developer   : MMT -Mariam Mazhar
*:* Date        : 03/01/2009
*:* Purpose     : Create Temp Files
*:**************************************************************************
Function lfCrtUnCmp
Parameters loFormSet
= lfCrSelFiles(loFormSet)
= lfCrCtnFiles(loFormSet)
=lfCrSelSum(loFormSet)
lfwStrsBr(loFormSet)

*:**************************************************************************
*:* Name        : lfCrSelFiles
*:* Developer   : MMT -Mariam Mazhar
*:* Date        : 03/01/2009
*:* Purpose     : Create Cartons Temp Files
*:**************************************************************************
Function lfCrSelFiles
Parameters loFormSet

Private lnCurAlias,lnI
lnCurAlias = Select(0)

lnI = 1

Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect1'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect2'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

*! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PIKTKT'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0
*! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect3'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect4'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect5'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect6'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect7'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect8'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Style'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 19
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Scale'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 3
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'SzCnt'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize1'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize2'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize3'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize4'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize5'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize6'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize7'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize8'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgPWgh'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdQty1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdQty2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdQty3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdQty4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdQty5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdQty6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdQty7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdQty8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

*-- Total order Qty
lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

*--total Avalable Qty
lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0
*--

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

*-- total open qty
lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'OTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0
*--

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

*-- total carton Qty
lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0
*--

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

*-- TotWight
lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotWeight'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 2
*-

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

*--
lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'StyWgh'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgStyWgh'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PWgh1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PWgh2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PWgh3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PWgh4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PWgh5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PWgh6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PWgh7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PWgh8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

*-- total wight
lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PTotWgh'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2
*--

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'LPicked'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'nStep'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 2
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'nOrdLineNo'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Selected'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0


*-- total original Qty
lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgTotOrd'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0

*-- total original Qty

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0


lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

*-- total diffrance Qty
lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'nTotDiff'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PACK_ID'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 16
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPkVersion'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 4
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPkColor'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPckSize'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 3
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'STORE'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0


lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'llPack'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'lRange'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'UpOqty1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'UpOqty2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'UpOqty3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'UpOqty4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'UpOqty5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'UpOqty6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'UpOqty7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'UpOqty8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

*-- total open qty
lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'UOTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0


lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cNoSize'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'llFrst'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

*--
Dimension laIndx[7,2]

laIndx[1,1] = "Style+STR(nOrdLineNo,6)"
laIndx[1,2] = loFormSet.lcPckLin

*! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
*laIndx[2,1] = "STORE"
laIndx[2,1] = 'STORE+PIKTKT'
*! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
laIndx[2,2] = 'Store'

laIndx[3,1] = "IIF(OQty1+OQty2+OQty3+OQty4+OQty5+OQty6+OQty7+OQty8>0,'Y','N')"
laIndx[3,2] = 'Opened'

laIndx[4,1] = "IIF(PQty1+PQty2+PQty3+PQty4+PQty5+PQty6+PQty7+PQty8=0,'Y','N')"
laIndx[4,2] = 'NoPacked'

laIndx[5,1] = "STR(nOrdLineNo,6)+Style"
laIndx[5,2] = loFormSet.lcTmpIdx
laIndx[6,1] = "PACK_ID+cPkColor+cPckSize+cPkVersion+STR(nOrdLineNo,6)+Style"
laIndx[6,2] = loFormSet.lcPakIndxLn
laIndx[7,1] = "PACK_ID+cPkColor+cPckSize+cPkVersion+Style+STR(nOrdLineNo,6)"
laIndx[7,2] = loFormSet.lcPakIndxSt


=gfCrtTmp(loFormSet.lcPckLin,@laFileStru,@laIndx)
Set Order To (loFormSet.lcPakIndxLn) In (loFormSet.lcPckLin)

Select PACK_LIN
=Afields(laFileStru)

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,18]
laFileStru[lnI,1] = 'STORE'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

Store ' ' To  laFileStru[lnI,7],laFileStru[lnI,8],;
    laFileStru[lnI,9],laFileStru[lnI,10],;
    laFileStru[lnI,11],laFileStru[lnI,12],;
    laFileStru[lnI,13],laFileStru[lnI,14],;
    laFileStru[lnI,15],laFileStru[lnI,16]
Store 0 To    laFileStru[lnI,17] ,laFileStru[lnI,18]


Dimension laIndx[1,2]
laIndx[1,1] = "PACK_ID+cPkColor+cPckSize+cPkVersion+Style+STR(nOrdLineNo,6)"
laIndx[1,2] = loFormSet.lcPack_Lin

=gfCrtTmp(loFormSet.lcPack_Lin,@laFileStru,@laIndx)
Set Order To (loFormSet.lcPakIndxLn) In (loFormSet.lcPckLin)




*:************* Create Stores temp file  ************************************
lnI = 1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'lSelect'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'STORE'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'DIST_CTR'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PIKTKT'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'TOTORD'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'TOTPIK'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Cartons'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'TOTPQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PACK_NO'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'BOL_NO'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

Dimension laIndx[1,2]
laIndx[1,1] = "DIST_CTR+STORE"
laIndx[1,2] = loFormSet.lcStores

=gfCrtTmp(loFormSet.lcStores,@laFileStru,@laIndx)
Set Order To (loFormSet.lcStores) In (loFormSet.lcStores)

Select (lnCurAlias)

*!*************************************************************
*! Name      : lfCrCtnFiles
*! Developer : Mariam Mazhar
*! Date      : 03/01/2009
*! Purpose   : Create the files that are used in Function lfvSelOrd
*!             (lcOrdLin,lcPckLin)
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfCrCtnFiles()
*!*************************************************************

Function lfCrCtnFiles
Parameters loFormSet
Private lnCurAlias

lnCurAlias = Select(0)
Dimension laFileStru[5,4]

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

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'STORE'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PIKTKT'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]


Dimension laIndx[3,2]
laIndx[1,1] = "STR(Cart_No,4)+STR(Pal_No,4)"
laIndx[1,2] = loFormSet.lcCtnHdr

laIndx[2,1] = "Empty+STR(Cart_No,4)+STR(Pal_No,4)"
laIndx[2,2] = "EMPTY"
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
*laIndx[3,1] = "STORE+STR(Cart_No,4)+STR(Pal_No,4)"
laIndx[3,1] = "STORE+PIKTKT+STR(Cart_No,4)+STR(Pal_No,4)"
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
laIndx[3,2] = "STORE"

=gfCrtTmp(loFormSet.lcCtnHdr,@laFileStru,@laIndx)
Set Order To (loFormSet.lcCtnHdr) In (loFormSet.lcCtnHdr)


lnI = 1

Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Cart_No'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 4
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Style'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 19
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'nOrdLineNo'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size1'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size2'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size3'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size4'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size5'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size6'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size7'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Size8'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

*--
lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0
*--

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

*-- TotWight
lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotWeight'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 2
*-

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'nStep'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PackLineNo'
laFileStru[lnI,2] = 'N'

*---(Start) Increased the width of the line no field used in
*---the packing list lines to be 6 instead of 3 digits
*---not to have an error when creating big packing lists
laFileStru[lnI,3] = 6

laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgWgh'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'SzCnt'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cStatus'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br1'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br2'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br3'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br4'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br5'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br6'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br7'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'Br8'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'STORE'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0


lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PACK_ID'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 16
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPkColor'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPckSize'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 3
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPkVersion'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 4
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'NPACKNO'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cNoSize'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PIKTKT'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]

Dimension laIndx[2,2]
laIndx[1,1] = "STR(Cart_No,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style+STR(nOrdLineNo,6)"
laIndx[1,2] = loFormSet.lcCtnDtl

laIndx[2,1] = "cStatus"
laIndx[2,2] = "Status"

=gfCrtTmp(loFormSet.lcCtnDtl,@laFileStru,@laIndx)
Set Order To (loFormSet.lcCtnDtl) In (loFormSet.lcCtnDtl)

Select (lnCurAlias)

*!*************************************************************
*! Name        : lfCrSelFiles
*! Developer : Mariam Mazhar
*! Date         : 03/01/2009
*! Purpose   : Create the files that are used in Function lfvSelOrd
*!                  to get pack summery (lcSumPck)
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfCrSelSum()
*!*************************************************************

Function lfCrSelSum
Parameters loFormSet
Private lnCurAlias,lnI
lnCurAlias = Select(0)

*--We modify way of selection to just select By Style-Color
lnI = 1

Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'lSelect'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0


lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'OTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PTotWgh'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'PACK_ID'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 19
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPkVersion'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 4
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPkColor'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPckSize'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 3
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'llPack'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = Alen(laFileStru,1)+1
Dimension laFileStru[lnI,4]
laFileStru[lnI,1] = 'lRange'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

Dimension laIndx[1,2]

laIndx[1,1] = "PACK_ID+cPkColor+cPckSize+cPkVersion"
laIndx[1,2] = loFormSet.lcSumPck

=gfCrtTmp(loFormSet.lcSumPck,@laFileStru,@laIndx)
Set Order To (loFormSet.lcSumPck) In (loFormSet.lcSumPck)

Select (lnCurAlias)

*!*************************************************************
*! Name      : lfwStrsBr
*! Developer : Mariam Mazhar
*! Date      : 03/01/2009
*! Purpose   : Store Grid Control source assigment
*!*************************************************************
Function  lfwStrsBr
Parameters loFormSet
loFormSet.ariaform1.pgfPacking.Header.grdHeadr.RecordSource = ''
loFormSet.ariaform1.pgfPacking.Header.grdHeadr.RecordSource = loFormSet.lcStores
loFormSet.ariaform1.pgfPacking.Header.grdHeadr.Column11.ControlSource = loFormSet.lcStores+'.lSelect'
If Type('loFormSet.ariaform1.pgfPacking.header.grdHeadr.Column11.Ariacheckbox1') <> 'O'
    loFormSet.ariaform1.pgfPacking.Header.grdHeadr.Column11.AddObject('Ariacheckbox1','Ariacheckbox')
Endif
loFormSet.ariaform1.pgfPacking.Header.grdHeadr.Column11.CurrentControl = 'Ariacheckbox1'
loFormSet.ariaform1.pgfPacking.Header.grdHeadr.Column1.ControlSource= loFormSet.lcStores+'.Store'
loFormSet.ariaform1.pgfPacking.Header.grdHeadr.Column2.ControlSource= loFormSet.lcStores+'.PikTkt'
loFormSet.ariaform1.pgfPacking.Header.grdHeadr.Column3.ControlSource = loFormSet.lcStores+'.TOTORD'
loFormSet.ariaform1.pgfPacking.Header.grdHeadr.Column4.ControlSource = loFormSet.lcStores+'.TOTPIK'
loFormSet.ariaform1.pgfPacking.Header.grdHeadr.Column5.ControlSource= loFormSet.lcStores+'.Cartons'
loFormSet.ariaform1.pgfPacking.Header.grdHeadr.Column6.ControlSource= loFormSet.lcStores+'.Weight'
loFormSet.ariaform1.pgfPacking.Header.grdHeadr.Column7.ControlSource= loFormSet.lcStores+'.TOTPQty'
loFormSet.ariaform1.pgfPacking.Header.grdHeadr.Column8.ControlSource= loFormSet.lcStores+'.PACK_NO'
loFormSet.ariaform1.pgfPacking.Header.grdHeadr.Column9.ControlSource= loFormSet.lcStores+'.BOL_NO'
loFormSet.ariaform1.pgfPacking.Header.grdHeadr.Column10.ControlSource = loFormSet.lcStores+'.DIST_CTR'
loFormSet.ariaform1.pgfPacking.Header.grdHeadr.ReadOnly = .T.
loFormSet.ariaform1.pgfPacking.Header.grdHeadr.Column11.ReadOnly =.F.
loFormSet.ariaform1.pgfPacking.Header.grdHeadr.Refresh
lfwStrsBr1(loFormSet)

*!*************************************************************
*! Name      : lfvSelOrdL
*! Developer : Mariam Mazhar
*! Date      : 03/01/2009
*! Purpose   : Select order data
*!*************************************************************
Function lfvSelOrdL
Parameters loFormSet
Private lnAlias,lnI,lnJ,lcSize,lnQty,lcStyle,lcStyDesc,lnContinue,;
    lnCtnQty,lnCtnWgh,lnStyOrdLin,llStyFound,lnRemPQty


lcCtnHdr = loFormSet.lcCtnHdr
lcStores = loFormSet.lcStores
lcCtnDtl = loFormSet.lcCtnDtl
*-- This variable indecates if the style is found more than once in order
llStyFound = .F.
Store 0 To lnContinue,lnCtnQty,lnCtnWgh,lnI,lnJ,lnStyOrdLin,lnRemPQty
Select &lcCtnHdr
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
*SET FILTER TO STORE = &lcStores..STORE
Set Filter To Store = &lcStores..Store And PIKTKT =&lcStores..PIKTKT
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
Go Top

Select &lcCtnDtl
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
*SET FILTER TO STORE = &lcStores..STORE
Set Filter To Store = &lcStores..Store And PIKTKT =&lcStores..PIKTKT
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
Go Top

loFormSet.llAnyUpd = .F.
lnAlias = Alias()
lcorder =  loFormSet.ariaForm1.kbOrderNo.keytextbox.Value
Select OrdLine
=gfSetOrder('Ordlinst')
llNoThing = gfSEEK('O'+lcorder +&lcStores..Store,'OrdLine')
Store Space(0) To lcStyle,lcStyDesc
lcExp = Iif(!Empty(&lcStores..PIKTKT),"WHILE cOrdType+Order+Store='O'+lcorder+&lcStores..STORE FOR PikTkt=&lcStores..PIKTKT AND Picked",;
    "WHILE cOrdType+Order+Store='O'+lcorder+&lcStores..STORE")
*--- llFrmPikLn --> .T. ----> colect data from pikline , .F. --> from ordline

llFrmPikLn = .F.
If !Empty(&lcStores..PIKTKT)
    Select PIKTKT
    =gfSetOrder('PIKTKT')
    If gfSEEK(&lcStores..PIKTKT) And Status = 'C'
        llFrmPikLn = .T.
        lcExp = "WHILE PikTkt+Order+Store=&lcStores..PIKTKT+lcorder+&lcStores..STORE FOR Picked "
    Endif
Endif
*-- move the pointer to frist record
If llFrmPikLn
    Select "PikLine"
    =gfSEEK(&lcStores..PIKTKT+lcorder)
Else
    Select "OrdLine"
    =gfSEEK('O'+lcorder+&lcStores..Store)
Endif
lcPckLin = loFormSet.lcPckLin
Scan Rest &lcExp
    =gfSeek(Style,'Style')
    =gfSeek('S'+Style.Scale,'Scale')
    llFrstRecord = .T.
    For lnIScl = 1 To Scale.Cnt
        lcSz = Str(lnIScl,1)

        If Iif(llFrmPikLn,PikLine.Qty&lcSz.,OrdLine.Qty&lcSz.) = 0
            Loop
        Endif

        Select (lcPckLin)
        Append Blank
        Replace llFrst     With llFrstRecord
        llFrstRecord = .F.
        *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
        Replace PIKTKT With Iif(llFrmPikLn,PikLine.PIKTKT ,OrdLine.PIKTKT )
        *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
        Replace Style      With Iif(llFrmPikLn,PikLine.Style,OrdLine.Style),;
            Scale      With Scale.Scale,;
            SzCnt      With Scale.Cnt,;
            StyWgh     With Style.nStyWeight,;
            OrgStyWgh  With Style.nStyWeight,;
            nOrdLineNo With Iif(llFrmPikLn,PikLine.Lineno,OrdLine.Lineno),;
            LPicked    With Iif(llFrmPikLn,PikLine.Picked,OrdLine.Picked),;
            cSize1     With Scale.Sz1,;
            cSize2     With Scale.Sz2,;
            cSize3     With Scale.Sz3,;
            cSize4     With Scale.Sz4,;
            cSize5     With Scale.Sz5,;
            cSize6     With Scale.Sz6,;
            cSize7     With Scale.Sz7,;
            cSize8     With Scale.Sz8,;
            AvlQty1    With Iif(Empty(&lcStores..PIKTKT),OrdLine.Qty1,;
            IIF(llFrmPikLn,PikLine.Pik1,OrdLine.Pik1)),;
            AvlQty2    With Iif(Empty(&lcStores..PIKTKT),OrdLine.Qty2,;
            IIF(llFrmPikLn,PikLine.Pik2,OrdLine.Pik2)),;
            AvlQty3    With Iif(Empty(&lcStores..PIKTKT),OrdLine.Qty3,;
            IIF(llFrmPikLn,PikLine.Pik3,OrdLine.Pik3)),;
            AvlQty4    With Iif(Empty(&lcStores..PIKTKT),OrdLine.Qty4,;
            IIF(llFrmPikLn,PikLine.Pik4,OrdLine.Pik4)),;
            AvlQty5    With Iif(Empty(&lcStores..PIKTKT),OrdLine.Qty5,;
            IIF(llFrmPikLn,PikLine.Pik5,OrdLine.Pik5)),;
            AvlQty6    With Iif(Empty(&lcStores..PIKTKT),OrdLine.Qty6,;
            IIF(llFrmPikLn,PikLine.Pik6,OrdLine.Pik6)),;
            AvlQty7    With Iif(Empty(&lcStores..PIKTKT),OrdLine.Qty7,;
            IIF(llFrmPikLn,PikLine.Pik7,OrdLine.Pik7)),;
            AvlQty8    With Iif(Empty(&lcStores..PIKTKT),OrdLine.Qty8,;
            IIF(llFrmPikLn,PikLine.Pik8,OrdLine.Pik8)),;
            OrdQty1    With Iif(llFrmPikLn,PikLine.Qty1,OrdLine.Qty1),;
            OrdQty2    With Iif(llFrmPikLn,PikLine.Qty2,OrdLine.Qty2),;
            OrdQty3    With Iif(llFrmPikLn,PikLine.Qty3,OrdLine.Qty3),;
            OrdQty4    With Iif(llFrmPikLn,PikLine.Qty4,OrdLine.Qty4),;
            OrdQty5    With Iif(llFrmPikLn,PikLine.Qty5,OrdLine.Qty5),;
            OrdQty6    With Iif(llFrmPikLn,PikLine.Qty6,OrdLine.Qty6),;
            OrdQty7    With Iif(llFrmPikLn,PikLine.Qty7,OrdLine.Qty7),;
            OrdQty8    With Iif(llFrmPikLn,PikLine.Qty8,OrdLine.Qty8),;
            STORE      With &lcStores..Store

        Replace AvlTotQty  With (AvlQty1+AvlQty2+AvlQty3+AvlQty4+AvlQty5+AvlQty6+AvlQty7+AvlQty8),;
            OrdTotQty  With (OrdQty1+OrdQty2+OrdQty3+OrdQty4+OrdQty5+OrdQty6+OrdQty7+OrdQty8)

        Replace OQty1  With Max(0,AvlQty1-Ordline.nPck1),;
            OQty2  With Max(0,AvlQty2-Ordline.nPck2),;
            OQty3  With Max(0,AvlQty3-Ordline.nPck3),;
            OQty4  With Max(0,AvlQty4-Ordline.nPck4),;
            OQty5  With Max(0,AvlQty5-Ordline.nPck5),;
            OQty6  With Max(0,AvlQty6-Ordline.nPck6),;
            OQty7  With Max(0,AvlQty7-Ordline.nPck7),;
            OQty8  With Max(0,AvlQty8-Ordline.nPck8),;
            OTotQty With Max(0,OQty1+OQty2+OQty3+OQty4+OQty5+OQty6+OQty7+OQty8),;
            PQty1  With OrdLine.nPck1,;
            PQty2  With OrdLine.nPck2,;
            PQty3  With OrdLine.nPck3,;
            PQty4  With OrdLine.nPck4,;
            PQty5  With OrdLine.nPck5,;
            PQty6  With OrdLine.nPck6,;
            PQty7  With OrdLine.nPck7,;
            PQty8  With OrdLine.nPck8
        Replace PTotQty With (PQty1+PQty2+PQty3+PQty4+PQty5+PQty6+PQty7+PQty8),;
            OTotQty With (OQty1+OQty2+OQty3+OQty4+OQty5+OQty6+OQty7+oQty8)


        Replace UpOqty1   With Max(0,AvlQty1-Ordline.nPck1),;
            UpOqty2   With Max(0,AvlQty2-Ordline.nPck2),;
            UpOqty3   With Max(0,AvlQty3-Ordline.nPck3),;
            UpOqty4   With Max(0,AvlQty4-Ordline.nPck4),;
            UpOqty5   With Max(0,AvlQty5-Ordline.nPck5),;
            UpOqty6   With Max(0,AvlQty6-Ordline.nPck6),;
            UpOqty7   With Max(0,AvlQty7-Ordline.nPck7),;
            UpOqty8   With Max(0,AvlQty8-Ordline.nPck8),;
            UOTotQty With Max(0,UpOqty1+UpOqty2+UpOqty3+UpOqty4+UpOqty5+UpOqty6+UpOqty7+UpOqty8)


        If (OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8) <> 0
            Replace PWgh1      With (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty1,;
                PWgh2      With (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty2,;
                PWgh3      With (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty3,;
                PWgh4      With (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty4,;
                PWgh5      With (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty5,;
                PWgh6      With (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty6,;
                PWgh7      With (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty7,;
                PWgh8      With (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty8
        Else
            Replace PWgh1      With 0,;
                PWgh2      With 0,;
                PWgh3      With 0,;
                PWgh4      With 0,;
                PWgh5      With 0,;
                PWgh6      With 0,;
                PWgh7      With 0,;
                PWgh8      With 0

        Endif

        Replace cNoSize    With lcSz
        Replace PTotWgh    With (PWgh1+PWgh2+PWgh3+PWgh4+PWgh5+PWgh6+PWgh7+PWgh8)

        Replace PACK_ID    With Iif(llFrmPikLn , PikLine.PACK_ID    , OrdLine.PACK_ID),;
            cPkColor   With Iif(llFrmPikLn , PikLine.cPkColor   , OrdLine.cPkColor),;
            cPckSize   With Iif(llFrmPikLn , PikLine.cPckSize   , OrdLine.cPckSize),;
            cPkVersion With Iif(llFrmPikLn , PikLine.cPkVersion    , OrdLine.cPkVersion)

        Replace lRange     With Iif(llFrmPikLn,PikLine.lRange, OrdLine.lRange),;
            llPack     With !Empty(PACK_ID)

        If gfSEEK(lcorder+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style,'TMPL_LIN')
            Replace CtnQty1   With TMPL_LIN.QTY1,;
                CtnQty2   With TMPL_LIN.QTY2,;
                CtnQty3   With TMPL_LIN.QTY3,;
                CtnQty4   With TMPL_LIN.QTY4,;
                CtnQty5   With TMPL_LIN.QTY5,;
                CtnQty6   With TMPL_LIN.QTY6,;
                CtnQty7   With TMPL_LIN.QTY7,;
                CtnQty8   With TMPL_LIN.QTY8,;
                CTNTOTQTY With CtnQty1+CtnQty2+CtnQty3+CtnQty4+ ;
                CtnQty5+CtnQty6+CtnQty7+CtnQty8
            *-- Show in the detail folder the unt/wgt not total unit for each size
            Replace Weight1   With TMPL_LIN.Weight,;
                Weight2   With TMPL_LIN.Weight,;
                Weight3   With TMPL_LIN.Weight,;
                Weight4   With TMPL_LIN.Weight,;
                Weight5   With TMPL_LIN.Weight,;
                Weight6   With TMPL_LIN.Weight,;
                Weight7   With TMPL_LIN.Weight,;
                Weight8   With TMPL_LIN.Weight
        Endif

        If lRange
            =gfSEEK(lcorder+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION,'TMPL_LIN')
            Replace Weight1   With TMPL_LIN.Weight,;
                Weight2   With TMPL_LIN.Weight,;
                Weight3   With TMPL_LIN.Weight,;
                Weight4   With TMPL_LIN.Weight,;
                Weight5   With TMPL_LIN.Weight,;
                Weight6   With TMPL_LIN.Weight,;
                Weight7   With TMPL_LIN.Weight,;
                Weight8   With TMPL_LIN.Weight
        Endif
        loFormSet.llAnyRec = .T.
    Endfor
Endscan


Select (lcPckLin)
lcPack_Id = ""
Scan For lRange
    lnPckedQty = 0

    If lcPack_Id <> PACK_ID
        lcPack_Id = PACK_ID
        If gfSEEK(lcorder+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION,'TMPL_LIN')
            lnPckedQty = TMPL_LIN.TotQty
            lnWeight   = TMPL_LIN.Weight
        Endif
    Endif
    If lnPckedQty > 0

        =gfSEEK(Style,'STYLE') And gfSEEK('S'+Style.Scale,'SCALE')
        If lnPckedQty < OTOTQTY
            lnPkQty = lnPckedQty/Scale.Cnt
            lnMod   = Mod(lnPkQty,1)
            llEven = .F.
            For lnI = 1 To Scale.Cnt
                lcI = Str(lnI,1)
                Replace CtnQty&lcI With Iif(!llEven,lnPkQty-lnMod,lnPkQty+lnMod)
                llEven = !llEven
            Endfor
        Else
            For lnI = 1 To Scale.Cnt
                lcI = Str(lnI,1)
                Replace CtnQty&lcI With OQty&lcI
            Endfor
        Endif
        Replace CTNTOTQTY With CtnQty1+CtnQty2+CtnQty3+CtnQty4+CtnQty5+CtnQty6+CtnQty7+CtnQty8
    Endif
Endscan

Select Pack_Lin
lcTag = Order()
=gfSetOrder('PackStyle')

If !Empty(&lcStores..PACK_NO)
    If gfSEEK(&lcStores..PACK_NO,'Pack_Lin')
        Scan Rest While pack_no = &lcStores..PACK_NO
            llStyFound = .F.
            lnRemPQty1 = Pack_Lin.Qty1
            lnRemPQty2 = Pack_Lin.Qty2
            lnRemPQty3 = Pack_Lin.Qty3
            lnRemPQty4 = Pack_Lin.Qty4
            lnRemPQty5 = Pack_Lin.Qty5
            lnRemPQty6 = Pack_Lin.Qty6
            lnRemPQty7 = Pack_Lin.Qty7
            lnRemPQty8 = Pack_Lin.Qty8

            *-- This is to avoid checking that the style has more than 1 line
            *-- in the order in case we are restoring pack data (in view mode)
            *-- which means that this check will be only we are copying from
            *-- another pack
            lcSearExp = "'O'+lcorder+&lcStores..STORE+Pack_Lin.Style+STR(Pack_Lin.nOrdLineNo,6)"
            If gfSEEK(&lcSearExp.,'Ordline')
                Select OrdLine
                lnPackLin = 0
                Scan Rest While cOrdType+Order+Store+Style+Str(Lineno,6) = ;
                        &lcSearExp.
                    lnStyOrdLin = OrdLine.Lineno
                    llNoThing = Seek('O'+lcorder+&lcStores..Store+Pack_Lin.Style+Str(lnStyOrdLin,6),'Ordline')
                    Set Order To (loFormset.lcPakIndxSt) In (loFormset.lcPckLin)
                    llNoThing = Seek(Pack_ID+cPkColor+cPckSize +cPkVersion+Pack_Lin.Style+Str(lnStyOrdLin,6),lcPckLin)

                    If (Empty(&lcStores..PIKTKT) And !OrdLine.Picked) Or (!Empty(&lcStores..PIKTKT) And Iif(llFrmPikLn,!OrdLine.Picked,OrdLine.Picked))
                        Select (lcCtnDtl)
                        lnPackLin = lnPackLin+1
                        =gfSeek(Pack_Lin.Style,'Style')
                        =gfSeek("S"+Style.Scale,'Scale')
                        llFirstRec = .T.
                        For lnI = 1 To Scale.Cnt
                            lcI =Str(lnI,1)
                            If Pack_Lin.Qty&lcI. = 0
                                Loop
                            Endif
                            Append Blank
                            *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
                            Replace PIKTKT With &lcStores..PIKTKT
                            *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
                            Replace Style      With Pack_Lin.Style,;
                                STORE      With &lcStores..Store,;
                                SzCnt      With &lcPckLin..SzCnt,;
                                cStatus    With "A",;
                                nOrdLineNo With lnStyOrdLin,;
                                PackLineNo With lnPackLin,;
                                Cart_No    With Pack_Lin.No_Cart,;
                                Qty1       With Pack_Lin.Qty1,;
                                Qty2       With Pack_Lin.Qty2,;
                                Qty3       With Pack_Lin.Qty3,;
                                Qty4       With Pack_Lin.Qty4,;
                                Qty5       With Pack_Lin.Qty5,;
                                Qty6       With Pack_Lin.Qty6,;
                                Qty7       With Pack_Lin.Qty7,;
                                Qty8       With Pack_Lin.Qty8,;
                                TotQty     With Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
                            Replace Size1      With Iif(Qty1>0,&lcPckLin..cSize1,Size1),;
                                Size2      With Iif(Qty2>0,&lcPckLin..cSize2,Size2),;
                                Size3      With Iif(Qty3>0,&lcPckLin..cSize3,Size3),;
                                Size4      With Iif(Qty4>0,&lcPckLin..cSize4,Size4),;
                                Size5      With Iif(Qty5>0,&lcPckLin..cSize5,Size5),;
                                Size6      With Iif(Qty6>0,&lcPckLin..cSize6,Size6),;
                                Size7      With Iif(Qty7>0,&lcPckLin..cSize7,Size7),;
                                Size8      With Iif(Qty8>0,&lcPckLin..cSize8,Size8)
                            Replace Br1        With !Empty(Qty1),;
                                Br2        With !Empty(Qty2),;
                                Br3        With !Empty(Qty3),;
                                Br4        With !Empty(Qty4),;
                                Br5        With !Empty(Qty5),;
                                Br6        With !Empty(Qty6),;
                                Br7        With !Empty(Qty7),;
                                Br8        With !Empty(Qty8),;
                                Weight1    With Qty1*(Pack_Lin.Weight/Pack_Lin.TotQty),;
                                Weight2    With Qty2*(Pack_Lin.Weight/Pack_Lin.TotQty),;
                                Weight3    With Qty3*(Pack_Lin.Weight/Pack_Lin.TotQty),;
                                Weight4    With Qty4*(Pack_Lin.Weight/Pack_Lin.TotQty),;
                                Weight5    With Qty5*(Pack_Lin.Weight/Pack_Lin.TotQty),;
                                Weight6    With Qty6*(Pack_Lin.Weight/Pack_Lin.TotQty),;
                                Weight7    With Qty7*(Pack_Lin.Weight/Pack_Lin.TotQty),;
                                Weight8    With Qty8*(Pack_Lin.Weight/Pack_Lin.TotQty),;
                                OrgWgh     With &lcPckLin..OrgStyWgh
                            Replace TotWeight  With (Weight1+Weight2+Weight3+Weight4+Weight5+Weight6+Weight7+Weight8)
                            Replace cNoSize    With lcI
                            * REPLACE llFirst WITH llFirstRec
                            * llFirstRec = .F.
                        Endfor
                        If Seek(Str(Pack_Lin.No_Cart,4),lcCtnHdr)
                            Select (lcCtnHdr)
                            Replace TotPcs With TotPcs + ;
                                &lcCtnDtl..Qty1+&lcCtnDtl..Qty2+;
                                &lcCtnDtl..Qty3+&lcCtnDtl..Qty4+;
                                &lcCtnDtl..Qty5+&lcCtnDtl..Qty6+;
                                &lcCtnDtl..Qty7+&lcCtnDtl..Qty8,;
                                TotWgh With TotWgh + ;
                                &lcCtnDtl..Weight1+&lcCtnDtl..Weight2+;
                                &lcCtnDtl..Weight3+&lcCtnDtl..Weight4+;
                                &lcCtnDtl..Weight5+&lcCtnDtl..Weight6+;
                                &lcCtnDtl..Weight7+&lcCtnDtl..Weight8
                        Else
                            If (&lcCtnDtl..Qty1+&lcCtnDtl..Qty2+;
                                    &lcCtnDtl..Qty3+&lcCtnDtl..Qty4+;
                                    &lcCtnDtl..Qty5+&lcCtnDtl..Qty6+;
                                    &lcCtnDtl..Qty7+&lcCtnDtl..Qty8) > 0

                                loFormset.lnMaxCtn = Max(loFormset.lnMaxCtn,Pack_Lin.No_Cart)
                                *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
                                *!*	                INSERT INTO (lcCtnHdr) (Cart_No,Pal_No,TotPcs,TotWgh,Empty, STORE);
                                *!*	                                VALUES (Pack_Lin.No_Cart,Pack_Lin.NPltNo,;
                                *!*	                                        &lcCtnDtl..Qty1+&lcCtnDtl..Qty2+;
                                *!*	                                        &lcCtnDtl..Qty3+&lcCtnDtl..Qty4+;
                                *!*	                                        &lcCtnDtl..Qty5+&lcCtnDtl..Qty6+;
                                *!*	                                        &lcCtnDtl..Qty7+&lcCtnDtl..Qty8,;
                                *!*	                                        &lcCtnDtl..Weight1+&lcCtnDtl..Weight2+;
                                *!*	                                        &lcCtnDtl..Weight3+&lcCtnDtl..Weight4+;
                                *!*	                                        &lcCtnDtl..Weight5+&lcCtnDtl..Weight6+;
                                *!*	                                        &lcCtnDtl..Weight7+&lcCtnDtl..Weight8,'N', &lcStores..STORE)
                                Insert Into (lcCtnHdr) (Cart_No,Pal_No,TotPcs,TotWgh,Empty, Store,PIKTKT);
                                    VALUES (Pack_Lin.No_Cart,Pack_Lin.NPltNo,;
                                    &lcCtnDtl..Qty1+&lcCtnDtl..Qty2+;
                                    &lcCtnDtl..Qty3+&lcCtnDtl..Qty4+;
                                    &lcCtnDtl..Qty5+&lcCtnDtl..Qty6+;
                                    &lcCtnDtl..Qty7+&lcCtnDtl..Qty8,;
                                    &lcCtnDtl..Weight1+&lcCtnDtl..Weight2+;
                                    &lcCtnDtl..Weight3+&lcCtnDtl..Weight4+;
                                    &lcCtnDtl..Weight5+&lcCtnDtl..Weight6+;
                                    &lcCtnDtl..Weight7+&lcCtnDtl..Weight8,'N', &lcStores..Store, &lcStores..PIKTKT)
                                *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]

                            Endif
                        Endif
                    Endif
                Endscan
            Endif
        Endscan
    Endif
Else
    Store 1 To loFormset.lnFrom,loFormset.lnTo
Endif
Select Pack_Lin
=gfSetOrder(lcTag )


*:**************************************************************************
*:* Name        : lfwStrsBr1
*:* Developer : MMT -Mariam Mazhar
*:* Date        : 03/01/2009
*:* Purpose     : When functions for stores browse
*:***************************************************************************
Function lfwStrsBr1
Parameters loFormset
Private lnAlias,laSelct,lcOpnTmpl
lcStores = loFormset.lcStores
lcOrder =loFormSet.ariaForm1.kbOrderNo.keytextbox.Value
lnAlias = Select(0)
Select (lcStores)
lnStBrRec = Recno()
Dimension laSelct[1]
laSelct = .F.
Select Count(lSelect) From (lcStores) Where !Empty(lcStores) Into Array laSelct
loFormset.ARiaform1.PgfPacking.Header.CMDGen.Enabled = !Empty(laSelct[1]) And  loFormset.ActiveMode $ 'EA'
loFormset.ARiaform1.PgfPacking.Header.cmdTemp.Enabled = !Empty(lcOrder)
Select (lcStores)
Select(lnAlias)

*:**************************************************************************
*:* Name        : lfDtlBrow
*:* Developer : MMT -Mariam Mazhar
*:* Date        : 03/01/2009
*:* Purpose     : Detail browse control source
*:***************************************************************************
Function lfDtlBrow
Parameters loFormSet
Private lnCurAlias
lnCurAlias = Select(0)
lcPckLin = loFormSet.lcPckLin
lcTmpPck = loFormSet.lcTmpPck
If !Used(lcTmpPck)
    If gfGetMemVar('M_ORDSTUTS',oAriaApplication.ActiveCompanyID) = 'L'
        Use (oAriaApplication.WorkDir+lcPckLin)  In 0 Again Alias (lcTmpPck) Order (loFormSet.lcPakIndxLn)
    Else
        Use (oAriaApplication.WorkDir+lcPckLin)  In 0 Again Alias (lcTmpPck) Order (loFormSet.lcPakIndxSt)
    Endif
Endif

lcTemFile = lcTmpPck
Select (lcTemFile)
lnDtBrRec = Recno()
With loFormSet.AriaForm1.pgfPacking.Detail.grdDetail
    .RecordSource = ''
    Select (loFormSet.lcTmpPck)
    *SET FILTER TO
    Locate
    If Eof()
        Return
    Endif

    .RecordSource = loFormSet.lcTmpPck

    *-- Pack-Id
    .Column2.ControlSource  = 'ThisFormset.lfGetDetPack()'
    .Column2.Header1.Caption = 'Pack_Id-Color-Size-Version'
    .Column2.Visible = .T.
    *-- Style

    .Column3.ControlSource  = 'ThisFormSet.lfIsRange()'
    .Column3.Header1.Caption = 'Range'
    .Column3.Width = 60
    .Column3.Visible = .T.

    .Column4.ControlSource  = loFormSet.lcTmpPck+'.Style'
    .Column4.Header1.Caption = loFormSet.lcStyTtl
    .Column4.Width = 180
    .Column4.Visible = .T.

    .Column5.ControlSource  = "THISFormSet.lfGetSz()"
    .Column5.Header1.Caption = 'Size'
    .Column5.Visible = .T.

    .Column6.ControlSource  = "THISFormSet.lfGetOQty()"
    .Column6.Header1.Caption = "O.Qty."
    .Column6.Visible = .T.

    .Column7.ControlSource  = "THISFormSet.lfGetCtnQty()"
    .Column7.Header1.Caption = "Qty.\Ctn"
    .Column7.Visible = .T.

    .Column8.ControlSource  = "THISFormSet.lfGetWghUnt()"
    .Column8.Header1.Caption = 'Wgh.\Unt'
    .Column8.Visible = .T.

    .Column9.ControlSource  = "THISFormSet.lfGetPQty()"
    .Column9.Header1.Caption ='P.Qty.'
    .Column9.Visible = .T.

    .Column10.ControlSource  = "THISFormSet.lfGetPWgh()"
    .Column10.Header1.Caption ='P.Wgh.'
    .Column10.Visible = .T.

    .ReadOnly = .T.
    *TTT
Endwith

*:**************************************************************************
*:* Name        : lfGetPackId
*:* Developer : MMT -Mariam Mazhar
*:* Date        : 03/01/2009
*:* Purpose     : get Pack_id
*:***************************************************************************
Function lfGetPackId
Parameters loFormSet
Return Evaluate(loFormSet.lcTmpPck+ '.Pack_Id')+'-'+Evaluate(loFormSet.lcTmpPck+ '.cPkColor')+;
    '-'+lfGetGmSz(Evaluate(loFormSet.lcTmpPck+ '.cPckSize'))+'-'+Evaluate(loFormSet.lcTmpPck+ '.cPkVersion')

*!*************************************************************
*! Name      : lfGetGmSz
*! Developer : MMT -Mariam Mazhar
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

Function lfGetGmSz
Parameter lcPackSize
Private lcNombr

If !Empty(lcPackSize)
    =gfSEEK('S'+Left(lcPackSize,1),'SCALE')
    lcNombr = Right(lcPackSize,1)
    lcLocSize=Eval('SCALE.SZ'+lcNombr)
Else
    lcLocSize ='*****'
Endif
Return lcLocSize
*:**************************************************************************
*:* Name        : lfIsRange
*:* Developer : MMT -Mariam Mazhar
*:* Date        : 03/01/2009
*:* Purpose     : check if is range pack or not
*:***************************************************************************
Function lfIsRange
Parameters loFormSet
Return Iif(Evaluate(loFormSet.lcTmpPck+'.llPack'),Iif(Evaluate(loFormSet.lcTmpPck+'.lRange'),'YES','NO'),'')
*!*************************************************************
*! Name      : lfOrdBrow
*! Developer : Mariam MazHar
*! Date      : 03/01/2009
*! Purpose   : To browse all packing list for a specific order
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
*! Called    : From ChangeMode in the formset
*!*************************************************************
Function lfOrdBrow
Lparameters lcOrder,lcAccount

*-- lcFields   variable that hold the name of browsed fields
*-- laBrow     array that hold the returned values from AriaBrow function
*-- lnCurAlias variable that hold the current alias
*-- lcCurTag   variable that hold the currend tag name
*-- llReturn   variable which is returned by this function
*-- lcTag      variable that hold the name of the tag which is desired to switch
*              file order to it

Private lcFields,laBrowArr,lnCurAlias,lcCurTag,llReturn,lcTag,lcBrFields
Dimension laBrowArr[1]
Store Space(0) To lcFields,laBrowArr,lcBrFields

lnCurAlias = Select(0)
Select ORDHDR
Set Relation To 'M'+Ordhdr.Account Into customer Additive
=gfSeek('O')
Locate
lcTag = Order('OrdHdr')
lcBrFields = [Order:H=LANG_ManulPL_lblOrderNo,status:3:H=LANG_ManulPL_lblStatus,lcSesDesc=gfCodDes(Season,'SEASON'):H=LANG_ManulPL_lblSeason,lcDivDesc=gfCodDes(cDivision,'CDIVISION'):H=LANG_ManulPL_lblDivision,]+;
    [CustPo=IIF(multipo,LANG_ManulPL_ValMultiPO,custpo):H=LANG_ManulPL_lblCustPO,]+;
    [ACCOUNT:H=LANG_ManulPL_lblAcc,store=IIF(MULTI='Y',LANG_ManulPL_ValMulti,STORE):H=LANG_ManulPL_lblStore,Customer.stname]+;
    [:15:H=LANG_ManulPL_lblName,Open:H=LANG_ManulPL_lblOpenQty,OpenAmt:H=LANG_ManulPL_lblOpenAmt,Ship:H=LANG_ManulPL_lblShipQty,Shipamt:H=LANG_ManulPL_lblShipAmt,]+;
    [start:H=LANG_ManulPL_lblStart,Complete:H=LANG_ManulPL_lblComplete,]+;
    [Note1:6:H=LANG_ManulPL_lblNotes]

Do Case
Case !Empty(lcAccount)
    =gfSETORDER('OrdAcct')
    lcAcc = lcAccount
    lcOrder = Iif(ARIABROW("lcAcc+'O'",;
        LANG_ManulPL_hdrOrders,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),;
        OrdHdr.Order,Space(6))

Case Empty(lcAccount)
    gfSETORDER('OrdHdr')
    lcOrder = Iif(ARIABROW("'O'",LANG_ManulPL_hdrOrders,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),;
        OrdHdr.Order,Space(6))
Endcase
Select Ordhdr
Set Relation To
gfSETORDER(lcTag)
Select (lnCurAlias)

*!*************************************************************
*! Name      : lfvAccount
*! Developer : Mariam MazHar
*! Date      : 03/01/2009
*! Purpose   : To validate the account code
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
Function lfvAccount
Lparameters loFormSet, lcAccount, llBrowse

If llBrowse Or (!Empty(lcAccount) And !gfSEEK('M'+lcAccount,'Customer'))
    Do CUSBROWM With lcAccount
Endif
loFormSet.AriaForm1.kbAccount.keytextbox.Value = lcAccount
loFormSet.AriaForm1.txtCustName.Value = Iif(!Empty(lcAccount),Customer.stName,'')
Return

*!*************************************************************
*! Name      : lfActFold
*! Developer : Mariam MazHar
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

Function lfActFold
Lparameters loFormSet,lnActFolder
lcStores = loFormSet.lcStores
lcTmpPck = loFormSet.lcTmpPck
lcCtnHdr = loFormSet.lcCtnHdr
If loFormSet.ActiveMode $ 'S'
    Return
Endif

Store Iif((loFormSet.ActiveMode $ 'AE' Or loFormSet.llNew) And lnActFolder = 1,.T.,.F.) To llInstStat,llShipStat

=lfWinHdrSt(loFormSet)

Do Case

Case lnActFolder = 2


    If Used(lcTmpPck)
        Select &lcTmpPck
        *! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
        *SET FILTER TO STORE = &lcStores..STORE
        Set Filter To Store = &lcStores..Store  And PIKTKT = &lcStores..PIKTKT
        *! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
        Go Top
    Endif

    =lfDtlBrow(loFormSet)

Case lnActFolder = 3

    If Used(lcCtnHdr)
        Select (lcCtnHdr)
        *! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
        *SET FILTER TO STORE = &lcStores..STORE
        Set Filter To Store = &lcStores..Store  And PIKTKT = &lcStores..PIKTKT
        *! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
        Go Top
    Endif
    With loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonH
        .RecordSource = ''
        Select (loFormSet.lcCtnHdr)
        .RecordSource = loFormSet.lcCtnHdr
        *-- Cart. #
        .Column1.ControlSource  = loFormSet.lcCtnHdr +'.Cart_No'
        .Column1.Header1.Caption = 'Cart.#'
        .Column1.Header1.Alignment = 1
        .Columns(1).Alignment = 1
        .Columns(1).Width = 40
        .Column2.Visible =  .F.

        *-- Tot. Pcs.
        .Column3.ControlSource  = loFormSet.lcCtnHdr + '.TotPcs'
        .Column3.Header1.Caption= 'Tot.Pcs'
        .Column3.Header1.Alignment = 1
        .Columns(3).Alignment = 1
        .Columns(3).Width = 70

        *-- Tot. Wgh.
        .Column4.ControlSource  = loFormSet.lcCtnHdr + '.TotWgh'
        .Column4.Header1.Alignment = 1
        .Column4.Header1.Caption= 'Tot.Wgh'
        .Columns(4).Alignment = 1
        .Columns(4).Width = 80

        *-- Carrier Carton ID
        .Column5.Visible =.F.
        .SetAll('ReadOnly',.T.,'COLUMN')
    Endwith
    With loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonD
        .RecordSource = ''
        Select (loFormSet.lcCtnDtl)
        .RecordSource = loFormSet.lcCtnDtl
        *-- PAck_ID
        .Column6.Visible =.F.&& IIF(loFormSet.llUsePack, .T., .F.)

        *-- Style
        .Column1.ControlSource  = loFormSet.lcCtnDtl +'.Style'
        .Column1.Header1.Caption = loFormSet.lcStyTtl
        .Columns(1).Width = 120
        .Column1.ColumnOrder = 2
        .Column2.Visible        = .F. &&IIF(loFormSet.llUseConfg OR loFormSet.llDyelot , .T., .F.)

        *-- Size Code.
        .Column3.ControlSource  = "THISFormSet.lfGetSzdet()"
        .Columns(3).Width = 40
        .Column3.ColumnOrder = 4

        *-- Qty
        .Column4.ControlSource  = "THISFormSet.lfGetQtyDet()"
        .Columns(4).Width = 40
        .Column4.Header1.Alignment = 1
        .Columns(4).Alignment = 1
        .Column4.ColumnOrder = 5

        *-- Weight
        .Column5.ControlSource  = "THISFormSet.lfGetWghDET()"
        .Columns(5).Width = 90
        .Column5.Header1.Alignment = 1
        .Columns(5).Alignment = 1
        .Column5.ColumnOrder = 6

        .SetAll('ReadOnly',.T.,'COLUMN')
    Endwith
    = lfwCtnHdrBrP(loFormSet)

Endcase


*!*************************************************************
*! Name      : lfWinHdrSt
*! Developer : Mariam MazHar
*! Date      : 03/01/2009
*! Purpose   : adjust enable status for header folder fields
*!*************************************************************
*! Called    : From ChangeMode in the formset
*!*************************************************************
Function lfWinHdrSt
Lparameters loFormSet

If loFormSet.ActiveMode $ 'AE' &&OR loFormSet.llNew
    With loFormSet.AriaForm1
        Store .F. To .kbOrderNo.Enabled, .kbAccount.Enabled,;
            .txtCustName.Enabled,  .txtCustPo.Enabled
    Endwith
Endif
*!*************************************************************
*! Name      : lfwCtnHdrBrP
*! Developer : Mariam MazHar
*! Date      : 03/01/2009
*! Purpose   : refresh the edit rigion for carton header according to
*!             lcCtnHdr file
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
Function lfwCtnHdrBrP
Lparameters loFormSet
Local lnAlias
lnAlias = Select(0)
lcCtnHdr =loFormSet.lcCtnHdr
Select (loFormSet.lcCtnDtl)
Set Key To Str(Evaluate(loFormSet.lcCtnHdr+'.Cart_No'),4)
=Seek(Str(Evaluate(loFormSet.lcCtnHdr+'.Cart_No'),4))
loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonD.Refresh
Select (loFormSet.lcCtnHdr)


Select(lnAlias)

*!*************************************************************
*! Name      : lfGetSz
*! Developer : Mariam MazHar
*! Date      : 03/01/2009
*! Purpose   : Get Size
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
Function lfGetSz
Parameters loFormSet

lcSizeNo = Eval(loFormSet.lcTmpPck +'.cNoSize')
If !Empty(lcSizeNo)
    Return Eval(loFormSet.lcTmpPck+'.cSize'+lcSizeNo)
Else
    Return ''
Endif
*!*************************************************************
*! Name      : lfGetOQty
*! Developer : Mariam MazHar
*! Date      : 03/01/2009
*! Purpose   : Get Qty of a given size
*!*************************************************************
Function lfGetOQty
Parameters loFormSet

lcSizeNo = Eval(loFormSet.lcTmpPck +'.cNoSize')
If !Empty(lcSizeNo)
    Return Eval(loFormSet.lcTmpPck+'.OQty'+lcSizeNo)
Else
    Return 0
Endif

*!*************************************************************
*! Name      : lfGetCtnQty
*! Developer : Mariam MazHar
*! Date      : 03/01/2009
*! Purpose   : Get carton Qty of a given size
*!*************************************************************
Function lfGetCtnQty
Parameters loFormSet

lcSizeNo = Eval(loFormSet.lcTmpPck +'.cNoSize')
If !Empty(lcSizeNo)
    Return Eval(loFormSet.lcTmpPck+'.CtnQty'+lcSizeNo)
Else
    Return 0
Endif

*!*************************************************************
*! Name      : lfGetWghUnt
*! Developer : Mariam MazHar
*! Date      : 03/01/2009
*! Purpose   : Get unit weight of a given size
*!*************************************************************
Function lfGetWghUnt
Parameters loFormSet

lcSizeNo = Eval(loFormSet.lcTmpPck +'.cNoSize')
If !Empty(lcSizeNo)
    Return Eval(loFormSet.lcTmpPck+'.Weight'+lcSizeNo)
Else
    Return 0
Endif

*!*************************************************************
*! Name      : lfGetPQty
*! Developer : Mariam MazHar
*! Date      : 03/01/2009
*! Purpose   : Get packed qty of a given size
*!*************************************************************
Function lfGetPQty
Parameters loFormSet

lcSizeNo = Eval(loFormSet.lcTmpPck +'.cNoSize')
If !Empty(lcSizeNo)
    Return Eval(loFormSet.lcTmpPck+'.PQty'+lcSizeNo)
Else
    Return 0
Endif

*!*************************************************************
*! Name      : lfGetPWgh
*! Developer : Mariam MazHar
*! Date      : 03/01/2009
*! Purpose   : Get packed weight of a given size
*!*************************************************************
Function lfGetPWgh
Parameters loFormSet

lcSizeNo = Eval(loFormSet.lcTmpPck +'.cNoSize')
If !Empty(lcSizeNo)
    Return Eval(loFormSet.lcTmpPck+'.PWgh'+lcSizeNo)
Else
    Return 0
Endif

*!*************************************************************
*! Name      : lfGetQtyDet
*! Developer : Mariam MazHar
*! Date      : 03/01/2009
*! Purpose   : Get qty of a given size in detail tab
*!*************************************************************
Function lfGetQtyDet
Parameters loFormSet
lcSizeNo = Eval(loFormSet.lcCtnDtl+'.cNoSize')
If !Empty(lcSizeNo)
    Return Eval(loFormSet.lcCtnDtl+'.Qty'+lcSizeNo)
Else
    Return 0
Endif

*!*************************************************************
*! Name      : lfGetWghDET
*! Developer : Mariam MazHar
*! Date      : 03/01/2009
*! Purpose   : Get WeighT of a given size in detail tab
*!*************************************************************
Function lfGetWghDET
Parameters loFormSet
lcSizeNo = Eval(loFormSet.lcCtnDtl+'.cNoSize')
If !Empty(lcSizeNo)
    Return Eval(loFormSet.lcCtnDtl+'.Weight'+lcSizeNo)
Else
    Return 0
Endif

*!*************************************************************
*! Name      : lfGetSzdet
*! Developer : Mariam MazHar
*! Date      : 03/01/2009
*! Purpose   : Get size desc. of a given size in detail tab
*!*************************************************************
Function lfGetSzdet
Parameters loFormSet
lcSizeNo = Eval(loFormSet.lcCtnDtl+'.cNoSize')
If !Empty(lcSizeNo)
    Return Eval(loFormSet.lcCtnDtl+'.Size'+lcSizeNo)
Else
    Return ''
Endif

*:**************************************************************************
*:* Name        : lfvSelSto
*:* Developer   : MMTT-Mariam MAzhar
*:* Date        : 03/01/2009
*:* Purpose     : Select stores
*:***************************************************************************
Function lfvSelSto
Parameters loFormSEt,lcSelTyp
lcStores = loFormSEt.lcStores
Private lnAlias,lnRecno
lnAlias = Select()
Select &lcStores
lnRecno = Recno(lcStores)

Do Case
Case  lcSelTyp = 'S'
    If Empty(PACK_NO)
        Replace lSelect With .T.
    Else
        Replace lSelect With .F.
    Endif

Case  lcSelTyp = 'A'
    Go Top
    Replace lSelect With .T. For Empty(PACK_NO)

Case  lcSelTyp = 'N'
    Go Top
    Replace lSelect  With .F. All

Endcase
If Between(lnRecno,1,Reccount(lcStores))
    Goto (lnRecno) In (lcStores)
Endif
=lfwStrsBr(loFormSEt)
Select (lnAlias)
*-- end of lfvSelSto.

*:**************************************************************************
*:* Name        : lfOpnTmpl
*:* Developer   : MMT -Mariam Mazhar
*:* Date        : 03/01/2009
*:* Purpose     : Open tamplate screen
*:***************************************************************************
*:* Called from :
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfOpnTmpl()
*:***************************************************************************
Function lfOpnTmpl
Parameters loFormset
Private lcOrder
lcOrder = "'"+Alltrim(loFormSet.ariaForm1.kbOrderNo.keytextbox.Value) +"'"
oAriaApplication.DoProgram("AWRALAUPLJ", lcOrder,.F.,'AL')

*:**************************************************************************
*:* Name        : lfvGenPL
*:* Developer   : MMT -Mariam Mazhar
*:* Date        : 03/01/2009
*:* Purpose     : Generate Packing list
*:***************************************************************************
Function lfvGenPL
Parameters loFormset
Private lcCurs,lcI,lnSvRec,lnI,lnCartons,lnJ,lnCrtSum,lcCurrSty
lcStores = loFormset.lcStores
lcErorFil = loFormset.lcErorFil
lcAccount = loFormSet.AriaForm1.kbAccount.keytextbox.Value
If loFormset.ActiveMode $ 'SV'
    Return
Endif
lcOrder = loFormSet.ariaForm1.kbOrderNo.keytextbox.Value
=gfSEEK('O'+lcOrder ,'ORDHDR')
If ORDHDR.Status = 'C'
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Order is invoiced, can not generate!')
    Return
Endif


Select &lcStores
lnSvRec = Recno(lcStores)

Locate For lSelect
If !Found()
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No Store selected.')
    Return
Endif

lcCurs = gfTempName()
lcPckLin  = loFormset.lcPckLin
lcCtnHdr = loFormset.lcCtnHdr
lcCtnDtl = loFormset.lcCtnDtl
Select TMPL_HDR
=gfSEEK(lcOrder ,'TMPL_HDR')
Select &lcPckLin
lcRelation = Set('RELATION')
lcSkip = Set('SKIP')
lnOrder = Order(lcPckLin)
Set Relation To
Set Order To (loFormset.lcPakIndxSt)
lnCrtHOrd = Order(lcCtnHdr)
Set Order To Store In (lcCtnHdr)
lnCrtDOrd  = Order(lcCtnDtl)
Set Order To (lcCtnDtl) In (lcCtnDtl)

Dimension laErrArr[1,4]
laErrArr[1,1] = 'cError'
laErrArr[1,2] = 'C'
laErrArr[1,3] = 80
laErrArr[1,4] = 0
=gfCrtTmp(loFormset.lcErorFil,@laErrArr)
llGenrted = .F.    && Set this to .T. if at least one pack is generated
*-- Check if there are errors
=lfChkErrs(loFormset)
llNoErr = ( Reccount(lcErorFil) = 0 )
llContinue = .T.
If Reccount(lcErorFil) > 0
    =lfErrDsply(loFormset)
    If llContinue .And. TMPL_HDR.LLCKONERR
        =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Order is locked on errors, can not create P/L for all selected stores')
    Endif
Endif

If llContinue .And. ;
        (!TMPL_HDR.LLCKONERR  .Or. (TMPL_HDR.LLCKONERR .And. llNoErr))
    Select &lcStores
    Go Top
    Scan For lSelect
        lnCrtSum = 0         && Variable holds # of cartons generated for the current store
        Select &lcPckLin
        Set Filter To
        Go Top
        Dimension laCarStru[3,4]
        laCarStru[1,1] = 'NCARTON'
        laCarStru[1,2] = 'N'
        laCarStru[1,3] = 4
        laCarStru[1,4] = 0

        laCarStru[2,1] = 'NCOUNT'
        laCarStru[2,2] = 'N'
        laCarStru[2,3] = 4
        laCarStru[2,4] = 0

        laCarStru[3,1] = 'NFROM'
        laCarStru[3,2] = 'N'
        laCarStru[3,3] = 4
        laCarStru[3,4] = 0
        =gfCrtTmp(lcCurs,@laCarStru,'NCARTON',lcCurs)

        =lfCtnOrg(loFormset)
        Wait Window Nowait 'Generating Cartons for store '+&lcStores..Store + ' '+Iif(!Empty(&lcStores..DIST_CTR),'D.C. ' +&lcStores..DIST_CTR , '')
        Select &lcPckLin
        Set Filter To
        *! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
        *SET FILTER TO STORE = &lcStores..STORE AND NOT lRange
        Set Filter To Store = &lcStores..Store And Not lRange And PIKTKT = &lcStores..PIKTKT
        *! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
        Go Top


        =gfSEEK(lcOrder ,'TMPL_LIN')
        Select TMPL_LIN
        Scan Rest While Order+Str(NO_CART,4)+PACK_ID+CPKCOLOR+CPKSIZE+CPKVERSION+Style = lcOrder  ;
                FOR TMPL_LIN.TOTQTY > 0
            lnLinpk = 1
            lcCurrSty = TMPL_LIN.PACK_ID+TMPL_LIN.CPKCOLOR+TMPL_LIN.CPKSIZE+TMPL_LIN.CPKVERSION+TMPL_LIN.Style
            If Seek(lcCurrSty,lcPckLin)
                Select &lcPckLin

                Scan Rest While PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style+Str(nOrdLineNo,6) = lcCurrSty
                    *-- laQty : This array holds the availabel qty for the current line in lcPckLin file
                    *--         from the equation CEILING(&lcPckLin..AvlQty1/TMPL_LIN.QTY1) we get the # of cartons
                    *--         needed for the qty laQty[1,1] , we get the max of this equation for sizes 1,...,8
                    *-          save this value in the lnCartons variable
                    Dimension laQty[8,3]
                    laQty = 0
                    lnCartons = 0
                    For lnI = 1 To 8
                        lcI = Str(lnI,1)
                        If &lcPckLin..cNoSize <> lcI
                            Loop
                        Endif
                        laQty[lnI,1] = &lcPckLin..AvlQty&lcI
                        laQty[lnI,2] = TMPL_LIN.QTY&lcI
                        laQty[lnI,3] = Iif( TMPL_LIN.QTY&lcI>0 , &lcPckLin..AvlQty&lcI/TMPL_LIN.QTY&lcI , 0 )
                        If laQty[lnI,3] > lnCartons
                            lnCartons = Ceiling(laQty[lnI,3])    && Get max number of cartons to hold all pieces
                        Endif
                    Endfor
                    Select &lcPckLin
                    lcSz = &lcPckLin..cNoSize


                    *    REPLACE PQTY1   WITH OQTY1,;
                    PQTY2   WITH OQTY2,;
                    PQTY3   WITH OQTY3,;
                    PQTY4   WITH OQTY4,;
                    PQTY5   WITH OQTY5,;
                    PQTY6   WITH OQTY6,;
                    PQTY7   WITH OQTY7,;
                    PQTY8   WITH OQTY8,;
                    PTOTQTY WITH PQTY1+PQTY2+PQTY3+PQTY4+PQTY5+PQTY6+PQTY7+PQTY8,;
                    PWGH1   WITH PQTY1*WEIGHT1,;
                    PWGH2   WITH PQTY2*WEIGHT2,;
                    PWGH3   WITH PQTY3*WEIGHT3,;
                    PWGH4   WITH PQTY4*WEIGHT4,;
                    PWGH5   WITH PQTY5*WEIGHT5,;
                    PWGH6   WITH PQTY6*WEIGHT6,;
                    PWGH7   WITH PQTY7*WEIGHT7,;
                    PWGH8   WITH PQTY8*WEIGHT8,;
                    PTOTWGH WITH PWGH1+PWGH2+PWGH3+PWGH4+PWGH5+PWGH6+PWGH7+PWGH8

                    Replace PQTY&lcSz.    With OQTY&lcSz.,;
                        PTOTQTY With PQTY1+PQTY2+PQTY3+PQTY4+PQTY5+PQTY6+PQTY7+PQTY8,;
                        PWGH&lcSz.   With PQTY&lcSz.* WEIGHT&lcSz.,;
                        PTOTWGH With PWGH1+PWGH2+PWGH3+PWGH4+PWGH5+PWGH6+PWGH7+PWGH8

                    =gfSEEK(TMPL_LIN.Style,'STYLE')
                    =gfSEEK('S'+Style.Scale,'SCALE')
                    =gfSEEK(TMPL_LIN.NCARTON,lcCurs)
                    lnPackNo = 0
                    If !Empty(&lcPckLin..PACK_ID)
                        lcSvOrd = Order('SPCK_LIN')
                        Select SPCK_LIN
                        =gfSetOrder('SPCK_LINVR')
                        Select &lcPckLin
                        If gfSEEK('P'+lcAccount+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style,'SPCK_LIN') Or ;
                                gfSEEK('P*****'+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style,'SPCK_LIN')
                            lnPackNo = PTOTQTY/SPCK_LIN.TOTQTY
                        Endif
                        Select SPCK_LIN
                        =gfSetOrder(lcSvOrd)
                        Select &lcPckLin
                    Endif

                    For lnCrtn = 1 To lnCartons

                        If Min(laQty[VAL(lcSz),1],TMPL_LIN.QTY&lcSz.) = 0
                            Loop
                        Endif

                        Select (lcCtnDtl)
                        Append Blank
                        Replace cNOSize With &lcPckLin..cNoSize
                        *REPLACE CART_NO WITH lnCrtn+&lcCurs..nFrom,;
                        STORE      WITH &lcStores..STORE,;
                        nOrdLineNo WITH &lcPckLin..nOrdLineNo,;
                        STYLE      WITH TMPL_LIN.STYLE,;
                        SZCNT      WITH SCALE.CNT,;
                        SIZE1      WITH SCALE.SZ1,;
                        SIZE2      WITH SCALE.SZ2,;
                        SIZE3      WITH SCALE.SZ3,;
                        SIZE4      WITH SCALE.SZ4,;
                        SIZE5      WITH SCALE.SZ5,;
                        SIZE6      WITH SCALE.SZ6,;
                        SIZE7      WITH SCALE.SZ7,;
                        SIZE8      WITH SCALE.SZ8

                        *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
                        Replace PIKTKT With &lcStores..PIKTKT
                        *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]

                        Replace CART_NO With lnCrtn+&lcCurs..nFrom,;
                            STORE      With &lcStores..Store,;
                            nOrdLineNo With &lcPckLin..nOrdLineNo,;
                            STYLE      With TMPL_LIN.Style,;
                            SZCNT      With Scale.Cnt,;
                            SIZE&lcSz.     With Scale.SZ&lcSz.
                        *,;
                        PackLineNo  WITH lnLinpk

                        *  REPLACE QTY1       WITH IIF(laQty[1,1] > 0 , MIN(laQty[1,1],TMPL_LIN.QTY1) , 0 ),;
                        QTY2       WITH IIF(laQty[2,1] > 0 , MIN(laQty[2,1],TMPL_LIN.QTY2) , 0 ),;
                        QTY3       WITH IIF(laQty[3,1] > 0 , MIN(laQty[3,1],TMPL_LIN.QTY3) , 0 ),;
                        QTY4       WITH IIF(laQty[4,1] > 0 , MIN(laQty[4,1],TMPL_LIN.QTY4) , 0 ),;
                        QTY5       WITH IIF(laQty[5,1] > 0 , MIN(laQty[5,1],TMPL_LIN.QTY5) , 0 ),;
                        QTY6       WITH IIF(laQty[6,1] > 0 , MIN(laQty[6,1],TMPL_LIN.QTY6) , 0 ),;
                        QTY7       WITH IIF(laQty[7,1] > 0 , MIN(laQty[7,1],TMPL_LIN.QTY7) , 0 ),;
                        QTY8       WITH IIF(laQty[8,1] > 0 , MIN(laQty[8,1],TMPL_LIN.QTY8) , 0 ),;
                        TOTQTY     WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8

                        Replace QTY&lcSz.       With Iif(laQty[VAL(lcSz),1] > 0 , Min(laQty[VAL(lcSz),1],TMPL_LIN.QTY&lcSz.) , 0 ),;
                            TOTQTY     With QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8

                        *REPLACE WEIGHT1    WITH IIF(Qty1 > 0 , QTY1*TMPL_LIN.WEIGHT , 0 ),;
                        WEIGHT2    WITH IIF(Qty2 > 0 , QTY2*TMPL_LIN.WEIGHT , 0 ),;
                        WEIGHT3    WITH IIF(Qty3 > 0 , QTY3*TMPL_LIN.WEIGHT , 0 ),;
                        WEIGHT4    WITH IIF(Qty4 > 0 , QTY4*TMPL_LIN.WEIGHT , 0 ),;
                        WEIGHT5    WITH IIF(Qty5 > 0 , QTY5*TMPL_LIN.WEIGHT , 0 ),;
                        WEIGHT6    WITH IIF(Qty6 > 0 , QTY6*TMPL_LIN.WEIGHT , 0 ),;
                        WEIGHT7    WITH IIF(Qty7 > 0 , QTY7*TMPL_LIN.WEIGHT , 0 ),;
                        WEIGHT8    WITH IIF(Qty8 > 0 , QTY8*TMPL_LIN.WEIGHT , 0 ),;
                        TOTWEIGHT  WITH WEIGHT1+WEIGHT2+WEIGHT3+WEIGHT4+;
                        WEIGHT5+WEIGHT6+WEIGHT7+WEIGHT8
                        Replace WEIGHT&lcSz.    With Iif(Qty&lcSz. > 0 , QTY&lcSz. *TMPL_LIN.WEIGHT , 0),;
                            TOTWEIGHT  With WEIGHT1+WEIGHT2+WEIGHT3+WEIGHT4+;
                            WEIGHT5+WEIGHT6+WEIGHT7+WEIGHT8

                        Replace BR1        With !Empty(QTY1),;
                            BR2        With !Empty(QTY2),;
                            BR3        With !Empty(QTY3),;
                            BR4        With !Empty(QTY4),;
                            BR5        With !Empty(QTY5),;
                            BR6        With !Empty(QTY6),;
                            BR7        With !Empty(QTY7),;
                            BR8        With !Empty(QTY8)

                        *-- Update here Pack_id,pksize,packcolor,pkversion ,nPackNo
                        If !Empty(&lcPckLin..PACK_ID)
                            Replace PACK_ID    With SPCK_LIN.PACK_ID,;
                                cPkColor   With SPCK_LIN.cPkColor,;
                                cPCkSize   With SPCK_LIN.cPckSize,;
                                cPKVersion With SPCK_LIN.cPkVersion,;
                                nPackNO    With lnPackNo
                        Endif

                        *- Decrease availabel qty to update next cartons ( specially last one )
                        For lnJ = 1 To 8
                            lcJ = Str(lnJ,1)
                            laQty[lnJ,1] = Max( laQty[lnJ,1] - QTY&lcJ , 0 )
                        Endfor
                        *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
                        *!*	            IF !SEEK(&lcStores..STORE+STR(lnCrtn+&lcCurs..nFrom,4),lcCtnHdr)
                        *!*	              INSERT INTO (lcCtnHdr) (Cart_No,Pal_No,Empty,STORE);
                        *!*	                                  VALUES (lnCrtn+&lcCurs..nFrom,0,;
                        *!*	                                          'N',&lcStores..STORE)
                        If !Seek(&lcStores..Store+&lcStores..PIKTKT+Str(lnCrtn+&lcCurs..nFrom,4),lcCtnHdr)
                            Insert Into (lcCtnHdr) (Cart_No,Pal_No,Empty,Store,PIKTKT);
                                VALUES (lnCrtn+&lcCurs..nFrom,0,;
                                'N',&lcStores..Store,&lcStores..PIKTKT)
                            *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
                            lnCrtSum = lnCrtSum + 1
                        Endif
                        Replace &lcCtnHdr..TotPcs With &lcCtnHdr..TotPcs + &lcCtnDtl..TotQty,;
                            &lcCtnHdr..TotWgh With &lcCtnHdr..TotWgh + &lcCtnDtl..TOTWEIGHT

                        loFormset.lnPackWgh = loFormset.lnPackWgh + &lcCtnDtl..TOTWEIGHT

                        Select &lcStores
                        Replace TOTPQty With TOTPQty + &lcCtnDtl..TotQty ,;
                            WEIGHT  With WEIGHT  + &lcCtnDtl..TOTWEIGHT
                    Endfor
                Endscan
                lnLinpk = lnLinpk +  1
            Endif
        Endscan
        =lfUpdRngPk(loFormset)
        Select &lcStores
        Replace CARTONS With lnCrtSum ,;
            PACK_NO With "######" ,;
            lSelect With .F.
        llGenrted = .T.
    Endscan
Endif
If llGenrted
    =lfwStrsBr(loFormSet)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Packing Lists are generated.')
    Locate For TOTORD # TOTPQTY And !Empty(PACK_NO)
    If Found()
        =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Total Packed Qty not equal to Total OrdQty for some stores.')
    Endif
Endif
Select &lcPckLin
Set Relation To &lcRelation
Set Skip To &lcSkip
Set Order To (lnOrder)

Set Order To &lnCrtHOrd In (lcCtnHdr)

If Used(lcCurs)
    Use In &lcCurs
Endif

If Between(lnSvRec,1,Reccount(lcStores))
    Goto (lnSvRec) In (lcStores)
Endif
*:**************************************************************************
*:* Name        : lfChkErrs
*:* Developer : MMT -Mariam MAzhar
*:* Date        : 03/01/2009
*:* Purpose     : Check errors for tempate
*:***************************************************************************
*:* Called from :
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfChkErrs()
*:***************************************************************************
Function lfChkErrs
Parameters loFormset
Private lnCnt,lnCartons,lnAlias,lcSvOrder,lcSvOrder2,lnI,lcI,lcCurrSty
lnAlias = Select()
lcOrder = loFormSet.ariaForm1.kbOrderNo.keytextbox.Value
lcPckLin = loFormset.lcPckLin
lcStores = loFormset.lcStores
lcSvOrder  = Order(lcPckLin)
lcSvOrder2 = Order('TMPL_LIN')
Set Order To Store In (lcPckLin)
Select TMPL_LIN
=gfSetOrder('TMPL_LINS')
lcErorFil = loFormset.lcErorFil
Select &lcPckLin
Set Filter To
Go Top

Select &lcStores
Go Top

Scan For lSelect
    lcMissed = ' '
    llAdd = .F.
    Select (lcPckLin)
    If Seek(&lcStores..Store,lcPckLin)
        Scan Rest While Store = &lcStores..Store
            If lRange
                Loop
            Endif
            *-- Check if style is missing in TMPL_LIN file
            lcCurrSty = lcOrder +PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style
            If !gfSEEK(lcOrder +PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style,'TMPL_LIN') .And. !(Style $ lcMissed)

                Insert Into (lcErorFil) Values (&lcStores..Store+'      | '+&lcPckLin..Style+'  | MISSED')
                lcMissed = lcMissed + &lcPckLin..Style + '|'
                llAdd = .T.
                Loop
            Endif

            *-- If style is in TMPL_LIN check that open qty is divisible by qty Ctn.
            *-- validate each size qty/ctn separately.
            *-- Apply non division only if AvlQty is greater than template qty , reverse case means
            *-- that all qty will be placed in one carton
            Select TMPL_LIN
            Scan Rest While Order+PACK_ID+CPKCOLOR+CPKSIZE+CPKVERSION+Style = lcCurrSty
                For lnI = 1 To 8
                    lcI = Str(lnI,1)
                    If TMPL_LIN.QTY&lcI > 0 .And. ;
                            &lcPckLin..AvlQty&lcI > TMPL_LIN.QTY&lcI .And. ;
                            MOD( &lcPckLin..AvlQty&lcI , TMPL_LIN.QTY&lcI ) > 0
                        Insert Into (lcErorFil) Values (&lcStores..Store+'    | '   +;
                            &lcPckLin..Style+'|'    +;
                            &lcPckLin..CSIZE&lcI+'|  '+;
                            ALLT(Str(TMPL_LIN.NCARTON))+;
                            '  | Division '+;
                            'O.Qty='+Ltrim(Str(&lcPckLin..AvlQty&lcI))+;
                            ', Qty/Ctn='+Ltrim(Str(TMPL_LIN.QTY&lcI)))
                        llAdd = .T.
                    Endif
                Endfor
            Endscan

        Endscan
    Endif

    *-- Add a separator after each store
    If llAdd
        Insert Into (lcErorFil) Values (Repl('=',80))
    Endif
Endscan

Set Order To &lcSvOrder  In (lcPckLin)
Select TMPL_LIN
=gfSetOrder(lcSvOrder2)
Select (lnAlias )
*-- end of lfChkErrs.
*:**************************************************************************
*:* Name        : lfErrDsply
*:* Developer   : MMT -Mariam Mazhar
*:* Date        : 03/01/2009
*:* Purpose     : Display Error Screen
*:***************************************************************************
Function lfErrDsply
Parameters loFormset
lcErorFil =loFormset.lcErorFil
Private lcErroSrc,lcNote,lcProcStat
lcErroSrc = (oAriaApplication.WorkDir+'Error.TXT')
lcNote     = Iif(TMPL_HDR.LLCKONERR,'Order is locked on errors','')
llProcStat = !TMPL_HDR.LLCKONERR
*: B609154,2 MMT 03/01/2010 Fix bug of error when run program on SAAS[Start]
If oAriaApplication.MULTIINST
    *: B609229,1 MMT 04/29/2010 Fix bug of error when run program on SAAS[Start]
    *DO FORM ("X:\aria4xp\Screens\"+'AL\ALERRORS.SCX') WITH loFormset,lcNote,llProcStat ,lcErroSrc
    lcParmLst = "loFormset,lcNote,llProcStat ,lcErroSrc"
    =gfCallForm('ALERRORS','AL',lcParmLst)
    *: B609229,1 MMT 04/29/2010 Fix bug of error when run program on SAAS[End]
Else
    *: B609154,2 MMT 03/01/2010 Fix bug of error when run program on SAAS[End]
    Do Form (oAriaApplication.ScreenHome+'AL\ALERRORS.scx') With loFormset,lcNote,llProcStat ,lcErroSrc
    *: B609154,2 MMT 03/01/2010 Fix bug of error when run program on SAAS[Start]
Endif
*: B609154,2 MMT 03/01/2010 Fix bug of error when run program on SAAS[End]
Copy To (lcErroSrc) Sdf
Use In (lcErorFil)
*:**************************************************************************
*:* Name        : lfvErrSrc
*:* Developer : MMT -MAriam MAzhar
*:* Date        : 03/01/2009
*:* Purpose     : Change the error log text file name
*:***************************************************************************
Function lfvErrSrc
Parameters loBranchForm
Private lcRet,lcSavDef,lcFullPath
lcFullPath = Set('FULLPATH')
lcSavDef = Fullpath('')
Set Default  To (oAriaApplication.WorkDir)
lcRet = Getfile('TXT','Select File To Save.')
If !Empty(lcRet)
    lcErroSrc = lcRet
Endif
loBranchForm.ariaForm1.txtFlPth.Value = lcErroSrc
Set Default  To &lcSavDef
Set Fullpath &lcFullPath

*:**************************************************************************
*:* Name        : lfCtnOrg
*:* Developer : MMT -MAriam MAzhar
*:* Date        : 03/01/2009
*:* Purpose     : get Carton info. from Template lines
*:***************************************************************************
Function lfCtnOrg
Parameters loFormset

lcPckLin = loFormset.lcPckLin
lcStores = loFormset.lcStores
Private lnCnt,lnCartons,lnAlias,lcCurrSty,lcOldSty,laAvlQty,lcSto
lnAlias = Select()
lcOrder = loFormSet.ariaForm1.kbOrderNo.keytextbox.Value
Store ' ' To lcCurrSty,lcOldSty
Dimension laAvlQty[9]


Select TMPL_LIN
=gfSEEK(lcOrder ,'TMPL_LIN')
Scan Rest While Order+Str(NO_CART,4)+PACK_ID+CPKCOLOR+CPKSIZE+CPKVERSION+Style = lcOrder ;
        FOR TMPL_LIN.TOTQTY > 0

    lcCurrSty = PACK_ID+CPKCOLOR+CPKSIZE+CPKVERSION+Style
    lcCurrPck = PACK_ID+CPKCOLOR+CPKSIZE+CPKVERSION
    If lcOldSty # lcCurrSty
        laAvlQty = 0
        lcOldSty = lcCurrSty
        lcSto = &lcStores..Store
        Select &lcPckLin
        =Seek(lcCurrSty,lcPckLin)
        lcPckOrSty = Iif(&lcPckLin..LRANGE,lcCurrPck,lcCurrSty)
        lnCntSz =&lcPckLin..SzCnt
        For lnA = 1 To lnCntSz
            =Seek(lcCurrSty,lcPckLin)
            Dimension laSzSum[1]
            Store 0 To laSzSum
            lcA = Str(lnA,1)
            Select Sum(AVLQTY&lcA.) From &lcPckLin Where PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style = lcPckOrSty .And. Store = lcSto ;
                AND cNoSize = lcA Into Array laSzSum
            If !Isnull(laSzSum[1])
                laAvlQty[lnA] = laSzSum[1]
                laAvlQty[9] = laAvlQty[9] +  laSzSum[1]
            Endif
        Endfor
        * SELECT SUM(AVLQTY1),SUM(AVLQTY2),SUM(AVLQTY3),SUM(AVLQTY4),;
        SUM(AVLQTY5),SUM(AVLQTY6),SUM(AVLQTY7),SUM(AVLQTY8),SUM(AvlTotQty) ;
        FROM &lcPckLin ;
        WHERE PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE = lcPckOrSty .AND. STORE = lcSto ;
        INTO ARRAY laAvlQty
    Endif
    If laAvlQty[9] > 0
        lnPSum = Iif(TMPL_LIN.QTY1>0,laAvlQty[1],0) + Iif(TMPL_LIN.QTY2>0,laAvlQty[2],0) + ;
            IIF(TMPL_LIN.QTY3>0,laAvlQty[3],0) + Iif(TMPL_LIN.QTY4>0,laAvlQty[4],0) + ;
            IIF(TMPL_LIN.QTY5>0,laAvlQty[5],0) + Iif(TMPL_LIN.QTY6>0,laAvlQty[6],0) + ;
            IIF(TMPL_LIN.QTY7>0,laAvlQty[7],0) + Iif(TMPL_LIN.QTY8>0,laAvlQty[8],0)
        lnCartons = Ceiling(lnPSum/TMPL_LIN.TOTQTY)
        If &lcPckLin..LRANGE
            lnCartons = Ceiling(laAvlQty[9]/TMPL_LIN.TOTQTY)
        Endif
        If !Seek(TMPL_LIN.NCARTON,lcCurs)
            Insert Into (lcCurs) (NCARTON) Values (TMPL_LIN.NCARTON)
        Endif
        Select &lcCurs
        Replace NCOUNT With Max( NCOUNT , lnCartons )  && Always put the maximum needed cartons
    Endif
Endscan
Select (lcCurs)
Set Order To
Go Top
lnCnt = 0
*- NFROM : this field is used to calculate starting # for each carton in tamplate to be filled
*-         in actual cartons
Scan
    Replace NFROM With lnCnt
    lnCnt = lnCnt + NCOUNT
Endscan
Set Order To Tag (lcCurs) In (lcCurs)
Locate
Select (lnAlias )

*:**************************************************************************
*:* Name        : lfUpdRngPk
*:* Developer   : MMT MAriam MAzhar
*:* Date        : 03/01/2009
*:* Purpose     : Update Range packs in carton detai file
*:***************************************************************************
Function lfUpdRngPk
Parameters loFormset
lcTmpUpLn = loFormSet.lcTmpUpLn
lcCtnDtl = loFormSet.lcCtnDtl
lcStores = loFormSet.lcStores
If Used(lcTmpUpLn)
    Use In (lcTmpUpLn)
Endif
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
*DIMENSION laUplnStr[18,4]
Dimension laUplnStr[19,4]
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
laUplnStr[1,1] = 'Style'
laUplnStr[1,2] = 'C'
laUplnStr[1,3] = 19
laUplnStr[1,4] = 0

laUplnStr[2,1] = 'Qty1'
laUplnStr[2,2] = 'N'
laUplnStr[2,3] = 6
laUplnStr[2,4] = 0

laUplnStr[3,1] = 'Qty2'
laUplnStr[3,2] = 'N'
laUplnStr[3,3] = 6
laUplnStr[3,4] = 0

laUplnStr[4,1] = 'Qty3'
laUplnStr[4,2] = 'N'
laUplnStr[4,3] = 6
laUplnStr[4,4] = 0

laUplnStr[5,1] = 'Qty4'
laUplnStr[5,2] = 'N'
laUplnStr[5,3] = 6
laUplnStr[5,4] = 0

laUplnStr[6,1] = 'Qty5'
laUplnStr[6,2] = 'N'
laUplnStr[6,3] = 6
laUplnStr[6,4] = 0

laUplnStr[7,1] = 'Qty6'
laUplnStr[7,2] = 'N'
laUplnStr[7,3] = 6
laUplnStr[7,4] = 0

laUplnStr[8,1] = 'Qty7'
laUplnStr[8,2] = 'N'
laUplnStr[8,3] = 6
laUplnStr[8,4] = 0

laUplnStr[9,1] = 'Qty8'
laUplnStr[9,2] = 'N'
laUplnStr[9,3] = 6
laUplnStr[9,4] = 0

laUplnStr[10,1] = 'TotQty'
laUplnStr[10,2] = 'N'
laUplnStr[10,3] = 7
laUplnStr[10,4] = 0

laUplnStr[11,1] = 'CartNo'
laUplnStr[11,2] = 'N'
laUplnStr[11,3] = 6
laUplnStr[11,4] = 0


laUplnStr[12,1] = 'nOrdLineNo'
laUplnStr[12,2] = 'N'
laUplnStr[12,3] = 6
laUplnStr[12,4] = 0

laUplnStr[13,1] = 'Store'
laUplnStr[13,2] = 'C'
laUplnStr[13,3] = 8
laUplnStr[13,4] = 0

laUplnStr[14,1] = 'PACK_ID'
laUplnStr[14,2] = 'C'
laUplnStr[14,3] = 16
laUplnStr[14,4] = 0

laUplnStr[15,1] = 'CPKCOLOR'
laUplnStr[15,2] = 'C'
laUplnStr[15,3] = 6
laUplnStr[15,4] = 0

laUplnStr[16,1] = 'CPCKSIZE'
laUplnStr[16,2] = 'C'
laUplnStr[16,3] = 3
laUplnStr[16,4] = 0

laUplnStr[17,1] = 'CPKVERSION'
laUplnStr[17,2] = 'C'
laUplnStr[17,3] = 4
laUplnStr[17,4] = 0

laUplnStr[18,1] = 'cNoSize'
laUplnStr[18,2] = 'C'
laUplnStr[18,3] = 1
laUplnStr[18,4] = 0

*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
laUplnStr[19,1] = 'PIKTKT'
laUplnStr[19,2] = 'C'
laUplnStr[19,3] = 6
laUplnStr[19,4] = 0
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]

=gfCrtTmp(loFormSet.lcTmpUpLn,@laUplnStr)
Select (lcCtnDtl)
Go Bottom
lnLastCrt = &lcCtnDtl..CART_NO
Select TMPL_LIN
=gfSetOrder('TMPL_LINS')
lnCartNO  = 0
lcOrder = loFormSet.ariaForm1.kbOrderNo.keytextbox.Value
Select &lcPckLin
Set Filter To
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
*SET FILTER TO STORE = &lcStores..STORE AND lRange
Set Filter To Store = &lcStores..Store And lRange And PIKTKT  = &lcStores..PIKTKT
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
Go Top
lnRemQty = 0
lcPack_Id = ""
lnPckedQty = 0  && initialize "lnPckedQty" variable
Scan For UOTOTQTY > 0
    If lcPack_Id <> PACK_ID
        If !Empty(lcPack_Id)
            lnCartNO = 0
        Endif
        lcPack_Id = PACK_ID
        If gfSEEK(lcOrder+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION,'TMPL_LIN')
            Private lnSz
            For lnSz = 1 To 8
                If Eval('TMPL_LIN.QTY'+Str(lnSz,1)) > 0
                    lnPckedQty = Eval('TMPL_LIN.QTY'+Str(lnSz,1))
                    Exit
                Endif
            Endfor
            lnRemQty   = 0
        Endif
    Endif
    lnI = 1
    Dime laQty[8]
    Store 0 To laQty
    =gfSEEK(Style,'STYLE') And gfSEEK('S'+Style.Scale,'SCALE')
    lnReqQty = Iif(lnRemQty   =0,lnPckedQty ,lnRemQty)
    lnStillReq = lnReqQty
    Do While lnI <= Scale.Cnt .And. UOTOTQTY > 0
        lcI = Str(lnI,1)
        laQty[lnI] = Min(UpOqty&lcI,lnReqQty)
        lnReqQty   = Max(lnReqQty - laQty[lnI],0)
        lnStillReq = lnReqQty
        Replace UOTOTQTY   With UOTOTQTY   - laQty[lnI],;
            UpOQTY&lcI With UpOQTY&lcI - laQty[lnI]
        If lnReqQty = 0
            lnCartNO   = lnCartNO + 1
            *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
            *!*	      INSERT INTO (lcTmpUpLn) (Style,nOrdLineNo,Store,PACK_ID,CPKCOLOR,CPCKSIZE,CPKVERSION,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,CartNo,TotQty);
            *!*	                   VALUES  (&lcPckLin..Style,&lcPckLin..nOrdLineNo,&lcPckLin..Store,&lcPckLin..PACK_ID,&lcPckLin..CPKCOLOR,;
            *!*	                           &lcPckLin..CPCKSIZE,&lcPckLin..CPKVERSION,laQty[1],laQty[2],laQty[3],;
            *!*	                            laQty[4],laQty[5],laQty[6],laQty[7],laQty[8],lnCartNO,laQty[1]+laQty[2]+laQty[3]+;
            *!*	                            laQty[4]+laQty[5]+laQty[6]+laQty[7]+laQty[8],lcI)
            Insert Into (lcTmpUpLn) (Style,nOrdLineNo,Store,PACK_ID,CPKCOLOR,CPCKSIZE,CPKVERSION,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,CartNo,TotQty,cNoSize,PIKTKT);
                VALUES  (&lcPckLin..Style,&lcPckLin..nOrdLineNo,&lcPckLin..Store,&lcPckLin..PACK_ID,&lcPckLin..CPKCOLOR,;
                &lcPckLin..CPCKSIZE,&lcPckLin..CPKVERSION,laQty[1],laQty[2],laQty[3],;
                laQty[4],laQty[5],laQty[6],laQty[7],laQty[8],lnCartNO,laQty[1]+laQty[2]+laQty[3]+;
                laQty[4]+laQty[5]+laQty[6]+laQty[7]+laQty[8],lcI,&lcPckLin..PIKTKT)
            *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
            lnReqQty = lnPckedQty
            lnRemQty = 0
            Store 0 To laQty
            If UpOqty&lcI = 0
                lnI = lnI + 1
            Endif
        Else
            lnI = lnI + 1
        Endif
    Enddo
    If lnStillReq <> 0
        lnCartNO   = lnCartNO + 1
        *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
        *!*	    INSERT INTO (lcTmpUpLn) (Style,nOrdLineNo,Store,PACK_ID,CPKCOLOR,CPCKSIZE,CPKVERSION,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,CartNo,TotQty);
        *!*	                VALUES  (&lcPckLin..Style,&lcPckLin..nOrdLineNo,&lcPckLin..Store,&lcPckLin..PACK_ID,&lcPckLin..CPKCOLOR,;
        *!*	                           &lcPckLin..CPCKSIZE,&lcPckLin..CPKVERSION,laQty[1],laQty[2],laQty[3],;
        *!*	                            laQty[4],laQty[5],laQty[6],laQty[7],laQty[8],lnCartNO,laQty[1]+laQty[2]+laQty[3]+;
        *!*	                            laQty[4]+laQty[5]+laQty[6]+laQty[7]+laQty[8])
        Insert Into (lcTmpUpLn) (Style,nOrdLineNo,Store,PACK_ID,CPKCOLOR,CPCKSIZE,CPKVERSION,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,CartNo,TotQty,PIKTKT);
            VALUES  (&lcPckLin..Style,&lcPckLin..nOrdLineNo,&lcPckLin..Store,&lcPckLin..PACK_ID,&lcPckLin..CPKCOLOR,;
            &lcPckLin..CPCKSIZE,&lcPckLin..CPKVERSION,laQty[1],laQty[2],laQty[3],;
            laQty[4],laQty[5],laQty[6],laQty[7],laQty[8],lnCartNO,laQty[1]+laQty[2]+laQty[3]+;
            laQty[4]+laQty[5]+laQty[6]+laQty[7]+laQty[8],&lcPckLin..PIKTKT)
        *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
        lnCartNO   = lnCartNO - 1
        lnRemQty   = lnStillReq
    Endif
Endscan

=lfUpdCtn(loFormset)
Select TMPL_LIN
=gfSetOrder('TMPL_LIN')

*:**************************************************************************
*:* Name        : lfUpdCtn
*:* Developer   : Mariam Mazhar
*:* Date        : 03/01/2009
*:* Purpose     : Update Range packs in carton detai file
*:***************************************************************************
Function lfUpdCtn
Parameters loFormset

lcTmpUpLn = loFormset.lcTmpUpLn
lcPckLin = loFormset.lcPckLin
lcCtnDtl = loFormset.lcCtnDtl
lcStores = loFormset.lcStores
lcCtnHdr = loFormset.lcCtnHdr
lcOrder = loFormSet.ariaForm1.kbOrderNo.keytextbox.Value
Select (lcTmpUpLn)
Scan
    lcCurrSty = PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style
    =Seek(lcCurrSty,lcPckLin)
    Select &lcPckLin
    Replace PQTY1   With OQTY1,;
        PQTY2   With OQTY2,;
        PQTY3   With OQTY3,;
        PQTY4   With OQTY4,;
        PQTY5   With OQTY5,;
        PQTY6   With OQTY6,;
        PQTY7   With OQTY7,;
        PQTY8   With OQTY8,;
        PTOTQTY With PQTY1+PQTY2+PQTY3+PQTY4+PQTY5+PQTY6+PQTY7+PQTY8,;
        PWGH1   With PQTY1*WEIGHT1,;
        PWGH2   With PQTY2*WEIGHT2,;
        PWGH3   With PQTY3*WEIGHT3,;
        PWGH4   With PQTY4*WEIGHT4,;
        PWGH5   With PQTY5*WEIGHT5,;
        PWGH6   With PQTY6*WEIGHT6,;
        PWGH7   With PQTY7*WEIGHT7,;
        PWGH8   With PQTY8*WEIGHT8,;
        PTOTWGH With PWGH1+PWGH2+PWGH3+PWGH4+PWGH5+PWGH6+PWGH7+PWGH8

    =gfSEEK(&lcTmpUpLn..Style,'STYLE') And gfSEEK('S'+Style.Scale,'SCALE')
    =gfSEEK(lcOrder +PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION,'TMPL_LIN')
    =Seek(TMPL_LIN.NCARTON,lcCurs)
    lnLastCrt = &lcCurs..nFrom

    Select (lcCtnDtl)
    Append Blank
    *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
    Replace PIKTKT With &lcTmpUpLn..PIKTKT
    *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[end]
    Replace cNoSize With   &lcTmpUpLn..cNoSize
    Replace CART_NO    With &lcTmpUpLn..CartNO+lnLastCrt ,;
        STORE      With &lcTmpUpLn..Store,;
        nOrdLineNo With &lcTmpUpLn..nOrdLineNo,;
        STYLE      With &lcTmpUpLn..Style,;
        SZCNT      With Scale.Cnt,;
        SIZE1      With Scale.SZ1,;
        SIZE2      With Scale.SZ2,;
        SIZE3      With Scale.SZ3,;
        SIZE4      With Scale.SZ4,;
        SIZE5      With Scale.SZ5,;
        SIZE6      With Scale.SZ6,;
        SIZE7      With Scale.SZ7,;
        SIZE8      With Scale.SZ8
    Replace QTY1       With &lcTmpUpLn..Qty1,;
        QTY2       With &lcTmpUpLn..Qty2,;
        QTY3       With &lcTmpUpLn..Qty3,;
        QTY4       With &lcTmpUpLn..Qty4,;
        QTY5       With &lcTmpUpLn..Qty5,;
        QTY6       With &lcTmpUpLn..Qty6,;
        QTY7       With &lcTmpUpLn..Qty7,;
        QTY8       With &lcTmpUpLn..Qty8,;
        TOTQTY     With QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8

    Replace WEIGHT1    With Iif(Qty1 > 0 , QTY1*&lcPckLin..WEIGHT1 , 0 ),;
        WEIGHT2    With Iif(Qty2 > 0 , QTY2*&lcPckLin..WEIGHT2 , 0 ),;
        WEIGHT3    With Iif(Qty3 > 0 , QTY3*&lcPckLin..WEIGHT3 , 0 ),;
        WEIGHT4    With Iif(Qty4 > 0 , QTY4*&lcPckLin..WEIGHT4 , 0 ),;
        WEIGHT5    With Iif(Qty5 > 0 , QTY5*&lcPckLin..WEIGHT5 , 0 ),;
        WEIGHT6    With Iif(Qty6 > 0 , QTY6*&lcPckLin..WEIGHT6 , 0 ),;
        WEIGHT7    With Iif(Qty7 > 0 , QTY7*&lcPckLin..WEIGHT7 , 0 ),;
        WEIGHT8    With Iif(Qty8 > 0 , QTY8*&lcPckLin..WEIGHT8 , 0 ),;
        TOTWEIGHT  With WEIGHT1+WEIGHT2+WEIGHT3+WEIGHT4+WEIGHT5+WEIGHT6+WEIGHT7+WEIGHT8


    Replace BR1        With !Empty(QTY1),;
        BR2        With !Empty(QTY2),;
        BR3        With !Empty(QTY3),;
        BR4        With !Empty(QTY4),;
        BR5        With !Empty(QTY5),;
        BR6        With !Empty(QTY6),;
        BR7        With !Empty(QTY7),;
        BR8        With !Empty(QTY8)

    *-- Update here Pack_id,pksize,packcolor,pkversion ,nPackNo
    Replace PACK_ID    With &lcTmpUpLn..PACK_ID,;
        cPkColor   With &lcTmpUpLn..cPkColor,;
        cPCkSize   With &lcTmpUpLn..cPckSize,;
        cPKVersion With &lcTmpUpLn..cPkVersion,;
        nPackNO    With 0
    *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
    *!*	   IF !SEEK(&lcStores..STORE+STR(&lcTmpUpLn..CartNO+lnLastCrt,4),lcCtnHdr)
    *!*	     INSERT INTO (lcCtnHdr) (Cart_No,Pal_No,Empty,STORE);
    *!*	                     VALUES (&lcTmpUpLn..CartNO+lnLastCrt,0,'N',&lcStores..STORE)
    If !Seek(&lcStores..Store+&lcStores..PIKTKT+Str(&lcTmpUpLn..CartNO+lnLastCrt,4),lcCtnHdr)
        Insert Into (lcCtnHdr) (Cart_No,Pal_No,Empty,Store,PIKTKT);
            VALUES (&lcTmpUpLn..CartNO+lnLastCrt,0,'N',&lcStores..Store,&lcStores..PIKTKT)
        *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
        lnCrtSum = lnCrtSum + 1
    Endif
    Replace &lcCtnHdr..TotPcs With &lcCtnHdr..TotPcs + &lcCtnDtl..TotQty,;
        &lcCtnHdr..TotWgh With &lcCtnHdr..TotWgh + &lcCtnDtl..TOTWEIGHT

    loFormset.lnPackWgh = loFormset.lnPackWgh + &lcCtnDtl..TOTWEIGHT

    Select &lcStores
    Replace TOTPQty With TOTPQty + &lcCtnDtl..TotQty ,;
        WEIGHT  With WEIGHT  + &lcCtnDtl..TOTWEIGHT
Endscan



*!*************************************************************
*! Name      : lpSavscr
*! Developer : MMT - MAriaM Mazhar
*! Date      : 03/01/2009
*! Purpose   : To make local save.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : DO lpSavscr
*!*************************************************************

Procedure lfSavScrPack
Parameters loFormSet


lcOrderNo = loFormSet.ariaForm1.kbOrderNo.keytextbox.Value
lcStores = loFormSet.lcStores
=gfSeek('O'+lcOrderNo ,'Ordhdr','Ordhdr')
Private laPcks,lnI
If ORDHDR.Status = 'C'
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Order is invoiced, can not save!')
    Return .F.
Endif
Select &lcStores
Go Top
lnI = 0
Scan For PACK_NO = '######'
    Store 0 To m.Tot_Wght, m.Tot_Cart, m.Tot_Pcs
    Store .F. To m.LStandCtn
    Store '' To m.CToStorCn
    Wait Window Nowait 'Generating P/L for store '+&lcStores..Store + ' D.C '+ &lcStores..DIST_CTR
    *!*    laData[1] = &lcStores..PACK_NO
    *!*    laData[3] = &lcStores..PIKTKT
    *!*    laData[5] = &lcStores..STORE
    lcPackNum = &lcStores..PACK_NO
    Do lfSavePack With loFormSet
    If !Empty(lcPackNum)
        lnI = lnI + 1
        Dimension laPcks[lnI]
        laPcks[lnI] = lcPackNum
    Endif
Endscan
Wait Window Nowait ''
If lnI>0
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Packing lists from '+laPcks[1]+' to '+laPcks[lnI]+' are generated.')
    Return .T.
Else
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No packing lists generated, cannot save.')
    Return .F.
Endif

*:**************************************************************************
*:* Name        : lfSavePack
*:* Developer : MMT -MAriaM Mazhar
*:* Date        : 03/01/2009
*:* Purpose     : Save pack for the current store
*:***************************************************************************
Function lfSavePack
Parameters loFormSet
lcOrderNo = loFormSet.ariaForm1.kbOrderNo.keytextbox.Value
lcAccount = loFormSet.AriaForm1.kbAccount.keytextbox.Value
lcStores = loFormSet.lcStores
lcCtnHdr= loFormSet.lcCtnHdr
*!*  lcStores = loFormSet.lcStores
lcCtnDtl= loFormSet.lcCtnDtl
Private llReturn,lnCurAlias,lcPckHdTag,lnCount,lnBookQty,lnBookAmt,;
    lnOpenQty,lnOpenAmt,lnCount,lnI,llStyDyOpn,lnCurRecNo,lcSvOrd
lcPcklin = loFormSet.lcPcklin
llStyDyOpn = .F.
Store 0 To lnBookQty,lnBookAmt,lnRelCho,lnOpenQty,lnOpenAmt
lnCount = 0
lnCurAlias = Select(0)
lcDelStat = Set("DELETED")
Private lnBookDiff , lnQtyDiff , lnBkAmtDif , lnQyAmtDif , lnOrgBook , lnOrgQty
Store 0 To lnBookDiff , lnQtyDiff , lnBkAmtDif , lnQyAmtDif, lnOrgBook , lnOrgQty

Select Pack_lin
=gfSetOrder('PackStyle')
Select (lcPcklin)
Set Relation To
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
*SET FILTER TO STORE =&lcStores..STORE
Set Filter To Store =&lcStores..Store And PIKTKT =&lcStores..PIKTKT
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
Go Top

Select (lcCtnDtl)
lcCtDtRel = Set("RELATION")
Set Relation To
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
*SET FILTER TO STORE = &lcStores..STORE
Set Filter To Store = &lcStores..Store And PIKTKT =&lcStores..PIKTKT
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
Go Top

Select (lcCtnDtl)
Set Filter To Store = &lcStores..Store
Go Top
If Eof() Or Bof() Or Deleted()
    *--No Packing list lines were applied. Cannot proceed!
    *-- <OK>
    llNoThing = gfModalGen("INM44035B00000","Dialog")
    Return .F.
    llReturn = .F.
Else

    *---------------------------------------------------
    *-------- Check for availability of picking --------
    *-- IF user want to update pick quantity and does not have access to force allocation

    If  loFormset.llUpdtPkTk
        *-- Open StyDye file before checking in it.
        =gfOpenTable(oAriaApplication.DataDir+"StyDye",oAriaApplication.DataDir+'StyDye')
        *-- if no Sufficient quantity.
        If lfNoSuffic(loFormSet)
            Return
        Endif
    Endif

    *============================================================
    *This part for updating allocated quantities in style,stydye files
    *and picked quantities in ordline file if the created pack list is
    *made by piktkt
    If !Empty(&lcStores..PIKTKT)
        Select(lcPckLin)
        lcTag = Order(lcPckLin)
        Set Order To NoPacked
        If Seek('Y',lcPckLin)
            *-- "All unselected lines from the picking ticket will be released."
            *-- <Release> <Resume packing> <Save as is>
            If lnRelCho = 0      && uncompelete session
                lnRelCho = gfModalGen("QRM44039B44006","Dialog")
            Endif
        Endif
        Set Order To lcTag In (lcPckLin)
    Endif
    If lnRelCho <> 2
        Select(lcPckLin)
        Scan
            llNoThing  = gfSEEK('O'+lcOrderNo+&lcStores..Store+&lcPckLin..Style+Str(nOrdLineNo,6),'OrdLine')
            If llNoThing And &lcPckLin..nStep = 0      && uncompelete session
                If !loFormset.llComp
                    Select OrdLine
                    *          REPLACE nPck1  WITH &lcPckLin..PQty1,;
                    nPck2  WITH &lcPckLin..PQty2,;
                    nPck3  WITH &lcPckLin..PQty3,;
                    nPck4  WITH &lcPckLin..PQty4,;
                    nPck5  WITH &lcPckLin..PQty5,;
                    nPck6  WITH &lcPckLin..PQty6,;
                    nPck7  WITH &lcPckLin..PQty7,;
                    nPck8  WITH &lcPckLin..PQty8,;
                    nPWght WITH &lcPckLin..PWgh1 + &lcPckLin..PWgh2 +;
                    &lcPckLin..PWgh3 + &lcPckLin..PWgh4 +;
                    &lcPckLin..PWgh5 + &lcPckLin..PWgh6 +;
                    &lcPckLin..PWgh7 + &lcPckLin..PWgh8

                    lcSz =&lcPckLin..cnoSize
                    Replace nPck&lcSz.   With &lcPckLin..PQty&lcSz.,;
                        nPWght       With nPWght + &lcPckLin..PWgh&lcSz.

                    If !Empty(&lcPckLin..PACK_ID)
                        lcSvOrd = Order('SPCK_LIN')
                        Select SPCK_LIN
                        =gfSetOrder('SPCK_LIN')
                        Select OrdLine
                        If gfSEEK('P'+ORDLINE.ACCOUNT+ORDLINE.PACK_ID+ORDLINE.CPKCOLOR+ORDLINE.CPCKSIZE+ORDLINE.CPKVERSION+ORDLINE.Style,'SPCK_LIN')
                            Replace ORDLINE.NPKPACK With (nPck1+nPck2+nPck3+nPck4+nPck5+nPck6+nPck7+nPck8)/SPCK_LIN.TOTQTY
                        Endif
                        Select SPCK_LIN
                        =gfSetOrder(lcSvOrd)
                        Select OrdLine
                        =gfReplace("")
                    Endif
                Endif

                If loFormset.llUpdtPkTk
                    lcSz =&lcPckLin..cnoSize
                    Select (lcPckLin)
                    Replace nDiff&lcSz. With PQty&lcSz.- OrdLine.Pik&lcSz.
                    *REPLACE nDiff1 WITH PQty1- OrdLine.Pik1 ,;
                    nDiff2 WITH PQty2- OrdLine.Pik2 ,;
                    nDiff3 WITH PQty3- OrdLine.Pik3 ,;
                    nDiff4 WITH PQty4- OrdLine.Pik4 ,;
                    nDiff5 WITH PQty5- OrdLine.Pik5 ,;
                    nDiff6 WITH PQty6- OrdLine.Pik6 ,;
                    nDiff7 WITH PQty7- OrdLine.Pik7 ,;
                    nDiff8 WITH PQty8- OrdLine.Pik8

                    If !loFormset.llComp
                        Select OrdLine
                        Replace Pik1  With nPck1,;
                            Pik2  With nPck2,;
                            Pik3  With nPck3,;
                            Pik4  With nPck4,;
                            Pik5  With nPck5,;
                            Pik6  With nPck6,;
                            Pik7  With nPck7,;
                            Pik8  With nPck8,;
                            TotPik  With Pik1 + Pik2 + Pik3 + Pik4 +Pik5 + Pik6 + Pik7 + Pik8
                    Endif
                Endif

                *-- If we are packing from piktkt
                If !Empty(&lcStores..PIKTKT)
                    *-- update pik fields in ordline file only
                    *-- If user option release or (save as is and the line is selected)
                    *-- which means if it is unselected line and "save as is" left the
                    *-- pik quantities as it is.
                    If lnRelCho = 1 Or ;
                            (lnRelCho = 3  And ;
                            !( Empty(&lcPckLin..cSelect1) Or Empty(&lcPckLin..cSelect2) Or ;
                            EMPTY(&lcPckLin..cSelect3) Or Empty(&lcPckLin..cSelect4) Or ;
                            EMPTY(&lcPckLin..cSelect5) Or Empty(&lcPckLin..cSelect6) Or ;
                            EMPTY(&lcPckLin..cSelect7) Or Empty(&lcPckLin..cSelect8) ) )

                        If !loFormset.llComp
                            lcSz =&lcPckLin..cnoSize
                            *              REPLACE Pik1   WITH &lcPckLin..PQty1,;
                            Pik2   WITH &lcPckLin..PQty2,;
                            Pik3   WITH &lcPckLin..PQty3,;
                            Pik4   WITH &lcPckLin..PQty4,;
                            Pik5   WITH &lcPckLin..PQty5,;
                            Pik6   WITH &lcPckLin..PQty6,;
                            Pik7   WITH &lcPckLin..PQty7,;
                            Pik8   WITH &lcPckLin..PQty8,;
                            TotPik WITH &lcPckLin..PQty1+&lcPckLin..PQty2+;
                            &lcPckLin..PQty3+&lcPckLin..PQty4+;
                            &lcPckLin..PQty5+&lcPckLin..PQty6+;
                            &lcPckLin..PQty7+&lcPckLin..PQty8 ;
                            PikTkt  WITH IIF(TotPik=0,'',PikTkt),;
                            PikDate WITH IIF(TotPik=0,{},PikDate),;
                            Picked  WITH IIF(TotPik=0,.F.,Picked)

                            Replace Pik&lcSz.   With &lcPckLin..PQty&lcSz.,;
                                TotPik With TotPik+ &lcPckLin..PQty&lcSz.,;
                                PikTkt  With Iif(TotPik=0,'',PikTkt),;
                                PikDate With Iif(TotPik=0,{},PikDate),;
                                Picked  With Iif(TotPik=0,.F.,Picked)

                        Endif
                    Endif
                Endif
                Select (lcPckLin)
                Replace &lcPckLin..nStep With 1
            Endif

            *--  updating the style file.
            If &lcPckLin..nStep = 1      && uncompelete session
                If !loFormset.llComp
                    Select Style
                    lcSz =&lcPckLin..cnoSize
                    If loFormset.llUpdtPkTk And !Empty(&lcStores..PIKTKT) And gfSEEK(&lcPckLin..Style,'Style')
                        *              REPLACE Alo1   WITH Alo1 + &lcPckLin..nDiff1,;
                        Alo2   WITH Alo2 + &lcPckLin..nDiff2,;
                        Alo3   WITH Alo3 + &lcPckLin..nDiff3,;
                        Alo4   WITH Alo4 + &lcPckLin..nDiff4,;
                        Alo5   WITH Alo5 + &lcPckLin..nDiff5,;
                        Alo6   WITH Alo6 + &lcPckLin..nDiff6,;
                        Alo7   WITH Alo7 + &lcPckLin..nDiff7,;
                        Alo8   WITH Alo8 + &lcPckLin..nDiff8,;
                        TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
                        Replace Alo&lcSz.   With Alo&lcSz. + &lcPckLin..nDiff&lcSz.,;
                            TotAlo With Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8

                    Endif
                    *-- updaing ord fields in style file in all cases not only piktkt
                    *          REPLACE Ord1   WITH IIF(&lcPckLin..PQty1>OrdLine.Qty1,Ord1+(&lcPckLin..PQty1-OrdLine.Qty1),Ord1),;
                    Ord2   WITH IIF(&lcPckLin..PQty2>OrdLine.Qty2,Ord2+(&lcPckLin..PQty2-OrdLine.Qty2),Ord2),;
                    Ord3   WITH IIF(&lcPckLin..PQty3>OrdLine.Qty3,Ord3+(&lcPckLin..PQty3-OrdLine.Qty3),Ord3),;
                    Ord4   WITH IIF(&lcPckLin..PQty4>OrdLine.Qty4,Ord4+(&lcPckLin..PQty4-OrdLine.Qty4),Ord4),;
                    Ord5   WITH IIF(&lcPckLin..PQty5>OrdLine.Qty5,Ord5+(&lcPckLin..PQty5-OrdLine.Qty5),Ord5),;
                    Ord6   WITH IIF(&lcPckLin..PQty6>OrdLine.Qty6,Ord6+(&lcPckLin..PQty6-OrdLine.Qty6),Ord6),;
                    Ord7   WITH IIF(&lcPckLin..PQty7>OrdLine.Qty7,Ord7+(&lcPckLin..PQty7-OrdLine.Qty7),Ord7),;
                    Ord8   WITH IIF(&lcPckLin..PQty8>OrdLine.Qty8,Ord8+(&lcPckLin..PQty8-OrdLine.Qty8),Ord8),;
                    TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8

                    Replace Ord&lcSz.   With Iif(&lcPckLin..PQty&lcSz.>OrdLine.Qty&lcSz.,Ord&lcSz.+(&lcPckLin..PQty&lcSz.-OrdLine.Qty&lcSz.),Ord&lcSz.),;
                        TotOrd With Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8



                Endif
                =gfReplace("")
                Select (lcPckLin)
                Replace &lcPckLin..nStep With 2
            Endif

            If !loFormset.llComp
                llStyDyOpn = gfOpenTable(oAriaApplication.DataDir+"StyDye",oAriaApplication.DataDir+'StyDye')
                *--  updating the stydye file.
                lcSz =&lcPckLin..cnoSize
                If &lcPckLin..nStep = 2      && uncompelete session
                    If !Empty(&lcStores..PIKTKT)
                        If gfSEEK (OrdLine.Style+loFormset.lcWareCode,'StyDye')
                            Select StyDye

                            If loFormset.llUpdtPkTk

                                * REPLACE Alo1 WITH Alo1 + &lcPckLin..nDiff1,;
                                Alo2 WITH Alo2 + &lcPckLin..nDiff2,;
                                Alo3 WITH Alo3 + &lcPckLin..nDiff3,;
                                Alo4 WITH Alo4 + &lcPckLin..nDiff4,;
                                Alo5 WITH Alo5 + &lcPckLin..nDiff5,;
                                Alo6 WITH Alo6 + &lcPckLin..nDiff6,;
                                Alo7 WITH Alo7 + &lcPckLin..nDiff7,;
                                Alo8 WITH Alo8 + &lcPckLin..nDiff8
                                Replace Alo&lcSz. With Alo&lcSz. + &lcPckLin..nDiff&lcSz.,;

                                Replace StyDye.TotAlo With StyDye.Alo1+StyDye.Alo2+StyDye.Alo3+;
                                    StyDye.Alo4+StyDye.Alo5+StyDye.Alo6+;
                                    StyDye.Alo7+StyDye.Alo8

                            Endif

                            *-- Update ord quantities in stydye file.

                            *REPLACE Ord1 WITH Ord1 + MAX(&lcPckLin..PQty1-OrdLine.Qty1,0),;
                            Ord2 WITH Ord2 + MAX(&lcPckLin..PQty2-OrdLine.Qty2,0),;
                            Ord3 WITH Ord3 + MAX(&lcPckLin..PQty3-OrdLine.Qty3,0),;
                            Ord4 WITH Ord4 + MAX(&lcPckLin..PQty4-OrdLine.Qty4,0),;
                            Ord5 WITH Ord5 + MAX(&lcPckLin..PQty5-OrdLine.Qty5,0),;
                            Ord6 WITH Ord6 + MAX(&lcPckLin..PQty6-OrdLine.Qty6,0),;
                            Ord7 WITH Ord7 + MAX(&lcPckLin..PQty7-OrdLine.Qty7,0),;
                            Ord8 WITH Ord8 + MAX(&lcPckLin..PQty8-OrdLine.Qty8,0),;
                            TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
                            Replace Ord&lcSz. With Ord&lcSz. + Max(&lcPckLin..PQty&lcSz.-OrdLine.Qty&lcSz.,0),;
                                TotOrd With Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8

                            =gfReplace("")
                        Endif
                    Endif
                    Select (lcPckLin)
                    Replace &lcPckLin..nStep With 3
                Endif
            Endif

            If !Empty(&lcStores..PIKTKT)
                lcSz =&lcPckLin..cnoSize
                If &lcPckLin..nStep = 3      && uncompelete session
                    If loFormset.llDyelot And  Style.cDye_Flg = 'Y' And ;
                            SEEK(OrdLine.Style+loFormset.lcWareCode+OrdLine.Dyelot,'StyDye')
                        If !loFormset.llComp
                            Select StyDye
                            If loFormset.llUpdtPkTk
                                *REPLACE Alo1 WITH Alo1 + &lcPckLin..nDiff1,;
                                Alo2 WITH Alo2 + &lcPckLin..nDiff2,;
                                Alo3 WITH Alo3 + &lcPckLin..nDiff3,;
                                Alo4 WITH Alo4 + &lcPckLin..nDiff4,;
                                Alo5 WITH Alo5 + &lcPckLin..nDiff5,;
                                Alo6 WITH Alo6 + &lcPckLin..nDiff6,;
                                Alo7 WITH Alo7 + &lcPckLin..nDiff7,;
                                Alo8 WITH Alo8 + &lcPckLin..nDiff8
                                Replace Alo&lcSz. With Alo&lcSz. + &lcPckLin..nDiff&lcSz.

                                Replace StyDye.TotAlo With StyDye.Alo1+StyDye.Alo2+StyDye.Alo3+;
                                    StyDye.Alo4+StyDye.Alo5+StyDye.Alo6+;
                                    StyDye.Alo7+StyDye.Alo8

                            Endif

                            *!*                REPLACE Ord1 WITH Ord1 + MAX(&lcPckLin..PQty1-OrdLine.Qty1,0),;
                            *!*                        Ord2 WITH Ord2 + MAX(&lcPckLin..PQty2-OrdLine.Qty2,0),;
                            *!*                        Ord3 WITH Ord3 + MAX(&lcPckLin..PQty3-OrdLine.Qty3,0),;
                            *!*                        Ord4 WITH Ord4 + MAX(&lcPckLin..PQty4-OrdLine.Qty4,0),;
                            *!*                        Ord5 WITH Ord5 + MAX(&lcPckLin..PQty5-OrdLine.Qty5,0),;
                            *!*                        Ord6 WITH Ord6 + MAX(&lcPckLin..PQty6-OrdLine.Qty6,0),;
                            *!*                        Ord7 WITH Ord7 + MAX(&lcPckLin..PQty7-OrdLine.Qty7,0),;
                            *!*                        Ord8 WITH Ord8 + MAX(&lcPckLin..PQty8-OrdLine.Qty8,0),;
                            *!*                        TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
                            Replace Ord&lcSz. With Ord&lcSz. + Max(&lcPckLin..PQty&lcSz.-OrdLine.Qty&lcSz.,0),;
                                TotOrd With Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
                            =gfReplace("")
                        Endif
                    Endif
                    Select (lcPckLin)
                    Replace &lcPckLin..nStep With 4
                Endif
            Endif
        Endscan
    Endif
    If Used("StyDye")
        Select "StyDye"
        =gfTableUpdate()
    Endif
    If llStyDyOpn
        Select "StyDye"
        =gfCloseTable("StyDye")
    Endif
    *===============end updating style,stydye files===================

    *===============this part for updating pack_hdr,bol files=========
    If lnRelCho <> 2

        *-- This is to get the count of only cartons that have contents
        *-- and the last carton no in the pack also that has contents
        *-- Start
        Select (lcCtnHdr)
        lcCtHdInd = Order(lcCtnHdr)
        Set Order To Empty In (lcCtnHdr)
        If Seek ('Y',lcCtnHdr)
            lnRecNo = Recno()
            Skip -1
            lnLastCtn = &lcCtnHdr..Cart_No
            Go lnRecNo
            Scan Rest For Empty = 'Y'
                lnPackCtn = lnPackCtn - 1
            Endscan
        Else
            Go Bottom
            lnLastCtn = &lcCtnHdr..Cart_No
        Endif
        Set Order To lcCtHdInd In (lcCtnHdr)
        *-- End getting last carton and carton count

        Select Pack_Hdr
        If !gfSEEK(lcPackNum ,'Pack_Hdr')
            *-- this part to Handle pack_no
            If Empty(&lcStores..PIKTKT)
                =gfSEEK('O'+lcOrderNo,'ORDHDR')
                lcPackNum  = gfSequence('PIKTKT', '', '', ORDHDR.cDivision)

                Do While gfSEEK( lcPackNum ,'PACK_HDR')
                    lcPackNum  = gfSequence('PIKTKT', '', '', ORDHDR.cDivision)
                Enddo
                Insert Into PACK_HDR (PACK_NO) Values (lcPackNum)
                Select Pack_Hdr
                =gfReplace("")
                Replace &lcStores..PACK_NO With lcPackNum

            Else
                lcPackNum = &lcStores..PIKTKT
                Insert Into PACK_HDR (PACK_NO) Values (lcPackNum)
                Select Pack_Hdr
                Replace Account With lcAccount,;
                    Order With lcOrderNo,;
                    Store With &lcStores..Store,;
                    shipvia With ORDHDR.Shipvia
                =gfReplace("")
            Endif
            *=============This part for BOL=================
            *-- This part will be executed only if the system has ASN Module
            *===============end of BOL=========================
            Select Pack_Hdr
        Endif
        lnLastNo = Pack_Hdr.nLastLNo

        *-- Get totals for this packing
        Select &lcCtnHdr
        Scan
            m.Tot_Wght = m.Tot_Wght+ TotWgh
            m.Tot_Cart = m.Tot_Cart + 1
            m.Tot_Pcs = m.Tot_Pcs  + TotPcs
        Endscan


        m.Account = lcAccount
        m.Order = lcOrderNo
        m.Store = &lcStores..Store
        m.shipvia = ORDHDR.Shipvia
        m.LStandCtn = Iif(loFormset.lnCtnTyp = 1,.T.,.F.)
        m.CToStorCn = Iif(loFormset.lnDrctTo = 1,'S','C')
        Select Pack_Hdr
        Gather Memo Memvar

        Replace cWareCode With loFormset.lcWareCode,;
            Bill_Ladg With ""

        =gfAdd_Info('Pack_Hdr')
        =gfReplace("")

    Endif
    *=================end of updating pack_hdr,BOL Files================

    *=================This part for saving pack_lin file================
    If lnRelCho <> 2

        lcSetDele = Set('DELETE')
        Set Delete On
        Select EDICRTSQ
        =gfSetOrder('PCKCRTSQ')
        If gfSEEK(lcPackNum,'EDICRTSQ')
            Select EDICRTSQ
            Scan Rest While pack_no+Str(cart_no,6) = lcPackNum
                gfDELETE()
            Endscan
        Endif
        Set Delete &lcSetDele

        Set Deleted Off
        Select(lcCtnDtl)
        *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
        *SET FILTER TO STORE = &lcStores..Store
        Set Filter To Store = &lcStores..Store And PIKTKT =&lcStores..PIKTKT
        *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
        Set Order To 0
        = gfSEEK('O'+lcOrderNo ,'OrdHdr')
        Scan For cStatus <> 'S'
            If &lcCtnDtl..nStep = 0   && for uncomplete session
                Select Pack_Lin
                If gfSEEK(lcPackNum+Str(&lcCtnDtl..Cart_No,4)+&lcCtnDtl..Style,'Pack_Lin')
                    If Deleted('Pack_Lin')
                        Locate Rest For pack_no+Str(no_cart,4)+Style = ;
                            lcPackNum+Str(&lcCtnDtl..Cart_No,4)+;
                            &lcCtnDtl..Style And !Deleted()
                    Endif
                    *LOCATE REST FOR Pack_No+STR(Line_No,6)+Style+cPackColor = ;
                    lcPackNum+STR(&lcCtnDtl..PackLineNo,6)+&lcCtnDtl..Style
                    *!*            IF loFormSet.ActiveMode = 'E'
                    *!*              LOCATE REST WHILE Pack_No+STR(Line_No,6)+STYLE+Dyelot = lcPackNum+STR(&lcCtnDtl..PackLineNo,6)+&lcCtnDtl..STYLE
                    *!*            ENDIF
                Endif

                If !Found('Pack_Lin') And !Deleted(lcCtnDtl)
                    lnLastNo = lnLastNo + 1
                    Append Blank
                    Replace Line_No With lnLastNo
                    =gfReplace("")
                    Select (lcCtnDtl)
                    Replace PackLineNo With lnLastNo
                Endif
                *-- This seek for getting Pal_No
                lcSz =&lcCtnDtl..cnoSize
                llNothing = Seek(Str(&lcCtnDtl..Cart_No,4),lcCtnHdr)
                Select Pack_Lin

                *!*          REPLACE Pack_No    WITH lcPackNum,;
                *!*                  No_Cart    WITH &lcCtnDtl..Cart_No,;
                *!*                  NPltNo     WITH &lcCtnHdr..Pal_No,;
                *!*                  Style      WITH &lcCtnDtl..Style,;
                *!*                  nOrdLineNo WITH &lcCtnDtl..nOrdLineNo,;
                *!*                  Qty1       WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty1,0),;
                *!*                  Qty2       WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty2,0),;
                *!*                  Qty3       WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty3,0),;
                *!*                  Qty4       WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty4,0),;
                *!*                  Qty5       WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty5,0),;
                *!*                  Qty6       WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty6,0),;
                *!*                  Qty7       WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty7,0),;
                *!*                  Qty8       WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty8,0),;
                *!*                  TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
                Replace Pack_No    With lcPackNum,;
                    No_Cart    With &lcCtnDtl..Cart_No,;
                    NPltNo     With &lcCtnHdr..Pal_No,;
                    Style      With &lcCtnDtl..Style,;
                    nOrdLineNo With &lcCtnDtl..nOrdLineNo,;
                    Qty&lcSz.  With Iif(!Deleted(lcCtnDtl),&lcCtnDtl..Qty&lcSz.,0),;
                    TotQty     With Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8



                *-- Save information of the pack in PACK_LIN file
                Replace PACK_ID    With &lcCtnDtl..PACK_ID,;
                    cPkColor   With &lcCtnDtl..cPkColor,;
                    cPCkSize   With &lcCtnDtl..cPckSize,;
                    cPKVersion With &lcCtnDtl..cPkVersion,;
                    nPackNO    With &lcCtnDtl..nPackNO

                *!*          REPLACE Weight     WITH IIF(!DELETED(lcCtnDtl),;
                *!*                                      &lcCtnDtl..Weight1+&lcCtnDtl..Weight2+;
                *!*                                      &lcCtnDtl..Weight3+&lcCtnDtl..Weight4+;
                *!*                                      &lcCtnDtl..Weight5+&lcCtnDtl..Weight6+;
                *!*                                      &lcCtnDtl..Weight7+&lcCtnDtl..Weight8 ;
                *!*                                      ,MAX(Weight-&lcCtnDtl..Weight1;
                *!*                                                 -&lcCtnDtl..Weight2;
                *!*                                                 -&lcCtnDtl..Weight3;
                *!*                                                 -&lcCtnDtl..Weight4;
                *!*                                                 -&lcCtnDtl..Weight5;
                *!*                                                 -&lcCtnDtl..Weight6;
                *!*                                                 -&lcCtnDtl..Weight7;
                *!*                                                 -&lcCtnDtl..Weight8;
                *!*                                                 +(&lcCtnDtl..Qty1*&lcCtnDtl..OrgWgh);
                *!*                                                 +(&lcCtnDtl..Qty2*&lcCtnDtl..OrgWgh);
                *!*                                                 +(&lcCtnDtl..Qty3*&lcCtnDtl..OrgWgh);
                *!*                                                 +(&lcCtnDtl..Qty4*&lcCtnDtl..OrgWgh);
                *!*                                                 +(&lcCtnDtl..Qty5*&lcCtnDtl..OrgWgh);
                *!*                                                 +(&lcCtnDtl..Qty6*&lcCtnDtl..OrgWgh);
                *!*                                                 +(&lcCtnDtl..Qty7*&lcCtnDtl..OrgWgh);
                *!*                                                 +(&lcCtnDtl..Qty8*&lcCtnDtl..OrgWgh),0))

                Replace Weight     With Weight + Iif(!Deleted(lcCtnDtl),;
                    &lcCtnDtl..Weight&lcSz.,Max(Weight-&lcCtnDtl..Weight&lcSz.+(&lcCtnDtl..Qty&lcSz.*&lcCtnDtl..OrgWgh),0))


                =gfAdd_Info('Pack_Lin')
                =gfReplace("")
                If Pack_Lin.TotQty = 0
                    gfDELETE()
                Endif
                Select (lcCtnDtl)
                Replace &lcCtnDtl..nStep With 1
            Endif


            *=================update ordline file ==========================
            If lnRelCho <> 2
                If !loFormset.llComp
                    Select OrdLine
                    *-- These 2 seek are to adjust the pointer in both files (lcpcklin,'OrdLine')

                    Set Order To lcPckLin In (lcPckLin)
                    =Seek(&lcCtnDtl..Style+Str(&lcCtnDtl..nOrdLineNo,6),lcPckLin)

                    =gfSEEK('O'+lcOrderNo +&lcStores..Store+&lcCtnDtl..Style+Str(&lcCtnDtl..nOrdLineNo,6),'OrdLine')
                    lnOrgBook = ToTBook
                    lnOrgQty  = TotQty
                    lcSz =&lcCtnDtl..cnoSize
                    *!*            REPLACE Book1   WITH Book1+MAX(&lcPckLin..OrdQty1-OrdLine.Qty1,0),;
                    *!*                    Book2   WITH Book2+MAX(&lcPckLin..OrdQty2-OrdLine.Qty2,0),;
                    *!*                    Book3   WITH Book3+MAX(&lcPckLin..OrdQty3-OrdLine.Qty3,0),;
                    *!*                    Book4   WITH Book4+MAX(&lcPckLin..OrdQty4-OrdLine.Qty4,0),;
                    *!*                    Book5   WITH Book5+MAX(&lcPckLin..OrdQty5-OrdLine.Qty5,0),;
                    *!*                    Book6   WITH Book6+MAX(&lcPckLin..OrdQty6-OrdLine.Qty6,0),;
                    *!*                    Book7   WITH Book7+MAX(&lcPckLin..OrdQty7-OrdLine.Qty7,0),;
                    *!*                    Book8   WITH Book8+MAX(&lcPckLin..OrdQty8-OrdLine.Qty8,0),;
                    *!*                    ToTBook WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8

                    Replace Book&lcSz.    With Book&lcSz. +Max(&lcPckLin..OrdQty&lcSz. -OrdLine.Qty&lcSz. ,0),;
                        ToTBook With Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8

                    lnBookQty = lnBookQty + ToTBook
                    lnBookAmt = lnBookAmt + (TotBook * Price)

                    *!*            REPLACE Qty1    WITH &lcPckLin..OrdQty1,;
                    *!*                    Qty2    WITH &lcPckLin..OrdQty2,;
                    *!*                    Qty3    WITH &lcPckLin..OrdQty3,;
                    *!*                    Qty4    WITH &lcPckLin..OrdQty4,;
                    *!*                    Qty5    WITH &lcPckLin..OrdQty5,;
                    *!*                    Qty6    WITH &lcPckLin..OrdQty6,;
                    *!*                    Qty7    WITH &lcPckLin..OrdQty7,;
                    *!*                    Qty8    WITH &lcPckLin..OrdQty8,;
                    *!*                    TotQty  WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8

                    Replace Qty&lcSz.    With &lcPckLin..OrdQty&lcSz.,;
                        TotQty  With Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
                    lnBookDiff = lnBookDiff + (TotBook - lnOrgBook)
                    lnBkAmtDif = lnBkAmtDif + ((TotBook - lnOrgBook) * Price)

                    lnQtyDiff  = lnQtyDiff  + (TotQty - lnOrgQty)
                    lnQyAmtDif = lnQyAmtDif + ((TotQty - lnOrgQty) * Price)
                Endif
            Endif
            *=================end updating ordline file======================

        Endscan

        Select Pack_Lin
        lcSetDel = Set('DELETE')
        Set Delete On
        =gfSEEK(lcPackNum)
        lnI = 0
        llFirst  = .T.
        lnLastCrt = Pack_Lin.No_Cart
        llSameCrt = .F.
        Scan Rest While Pack_No+Str(No_Cart,4)+Style = lcPackNum
            If !llFirst
                llSameCrt = (Pack_Lin.No_Cart = lnLastCrt)
                If !llSameCrt
                    lnLastCrt = Pack_Lin.No_Cart
                Endif
            Endif
            If llFirst Or !llSameCrt
                lnI = lnI + 1
            Endif
            Replace Pack_Lin.No_Cart With lnI
            =gfReplace("")
            llFirst  = .F.
        Endscan

        Select Pack_Lin
        =gfSEEK(lcPackNum)
        Scan Rest While Pack_No+Str(No_Cart,4)+Style = lcPackNum
            Select EDICRTSQ
            =gfSetOrder('PCKCRTSQ')
            Select Pack_Lin
            If !gfSEEK(lcPackNum+Str(Pack_Lin.No_Cart,6),'EDICRTSQ')
                *-- If this customer is a partner , get the last Ucc # from EDIACPRT file,
                If gfSEEK('A'+lcAccount ,'EDIACPRT')

                    *B611093,1 AEG 2015-12-14 Two diffrent packing lst have the same UCC9 [Begin]

                    Do While !Rlock('EDIACPRT')
                    Enddo
                    *B611093,1 AEG 2015-12-14 Two diffrent packing lst have the same UCC9 [End]


                    lnUcc9  = Iif(Empty(EDIACPRT.Ucc9),0,Eval(EDIACPRT.Ucc9)) + 1
                    Select EDICRTSQ
                    =gfSetOrder("EDICRTSQ")
                    llFound = gfSEEK(lcAccount +Padl(lnUcc9,9,'0'),'EDICRTSQ')
                    Do While llFound
                        lnUcc9  = lnUcc9  + 1
                        llFound = gfSEEK(lcAccount +Padl(lnUcc9,9,'0'),'EDICRTSQ')
                    Enddo
                    Insert Into EDICRTSQ (Pack_No,Account,cart_no,Ucc9);
                        VALUE (lcPackNum,lcAccount ,Pack_Lin.No_Cart,Padl(lnUcc9,9,'0'))
                    Replace EDIACPRT.Ucc9 With Padl(lnUcc9,9,'0')
                    =gfReplace("")

                    *B611093,1 AEG 2015-12-14 Two diffrent packing lst have the same UCC9 [Begin]
                    Unlock In('EDIACPRT')
                    *B611093,1 AEG 2015-12-14 Two diffrent packing lst have the same UCC9 [End]


                Else    && Get the last Ucc # from EDICRTSQ file.
                    Select EDICRTSQ
                    IF gfSeek(lcAccount)
                    *B611093,1 AEG 2015-12-14 Two diffrent packing lst have the same UCC9 [Begin]
                        Do While!Rlock('EDICRTSQ')
                        Enddo
                    Endif
                    *B611093,1 AEG 2015-12-14 Two diffrent packing lst have the same UCC9 [End]
                    Select Max(EDICRTSQ .ucc9) From EDICRTSQ Where EDICRTSQ.account = lcAccount Into Cursor lcMaxUcc


                    Select lcMaxUcc
                    Locate
                    If Eof()
                        lcUcc9 = '000000001'
                    Else


                        lcUcc9 = Padl(Eval(lcMaxUcc.MAX_UCC9)+1,9,'0')

                    Endif
                    Insert Into EDICRTSQ (Pack_No,Account,cart_no,Ucc9);
                        VALUE (lcPackNum,lcAccount ,Pack_Lin.No_Cart,lcUcc9)
                    =gfReplace("")

                    *B611093,1 AEG 2015-12-14 Two diffrent packing lst have the same UCC9 [Begin]
                    Unlock In('EDICRTSQ')
                    *B611093,1 AEG 2015-12-14 Two diffrent packing lst have the same UCC9 [End]

                Endif
            Endif
        Endscan

        lnLastCtn = lnI

        Select (lcCtnDtl)
        Go Top

        lnI = 0
        llFirst  = .T.
        lnLastCrt = &lcCtnDtl..Cart_No
        llSameCrt = .F.
        Scan
            If !llFirst
                llSameCrt = (&lcCtnDtl..Cart_No = lnLastCrt)
                If !llSameCrt
                    lnLastCrt = &lcCtnDtl..Cart_No
                Endif
            Endif
            If llFirst Or !llSameCrt
                lnI = lnI + 1
            Endif
            Replace &lcCtnDtl..Cart_No With lnI
            llFirst  = .F.
        Endscan
        Set Delete &lcSetDel

        Select Pack_Hdr
        Replace nLastLno  With Max(nLastLno,lnLastNo) ,;
            nLastCart With lnLastCtn

        =gfReplace('')
    Endif
    *=================end saving pack_lin file======================

    *=================update book,open fields in ordhdr file=========
    If lnRelCho <> 2
        If !(lnBookDiff=0 And lnQtyDiff=0 And lnBkAmtDif=0 And lnQyAmtDif=0) And ;
                gfSEEK('O'+Pack_Hdr.Order,'OrdHdr')

            If !loFormset.llComp
                Select OrdHdr
                =Rlock()
                Replace BOOK    With BOOK    + lnBookDiff ,;
                    BOOKAMT With BOOKAMT + lnBkAmtDif ,;
                    OPEN    With Open    + lnQtyDiff  ,;
                    OPENAMT With OPENAMT + lnQyAmtDif
                Unlock
                =gfAdd_Info("OrdHdr")
                =gfReplace("")
            Endif
        Endif
    Endif
Endif
Set Deleted &lcDelStat

If lnRelCho <> 2
    llReturn = .T.
Else
    llReturn = .F.
Endif

Select(lcCtnDtl)
Set Relation To &lcCtDtRel.

*!*  SELECT OrdLine
*!*  lcTag = ORDER()
*!*  =gfSetOrder('OrdLinSt')
*!*  lnCurRecNo = 0
*!*  IF gfSEEK('O'+lcOrderNo+&lcStores..Store,'OrdLine')
*!*    SELECT OrdLine
*!*    lnCurRecNo = RECNO()
*!*    SCAN REST WHILE cOrdType+Order+Store+Style+STR(lineno,6) = 'O'+lcOrderNo+&lcStores..Store
*!*      = gfObj_Lock(.F.)
*!*    ENDSCAN
*!*  ENDIF
*!*  SELECT OrdLine
*!*  =gfSetOrder(lcTag)
*!*  IF BETWEEN(lnCurRecNo,1,RECCOUNT('ORDLINE'))
*!*    GOTO (lnCurRecNo)
*!*  ENDIF
llCSave = llReturn
Select Pack_lin
=gfTableUpdate()
Select Pack_Hdr
=gfTableUpdate()
Select OrdHdr
=gfTableUpdate()
Select OrdLINE
=gfTableUpdate()
Select Style
=gfTableUpdate()
If Used('EDICRTSQ')
    Select EDICRTSQ
    =gfTableUpdate()
Endif
=lfOrdHdLck(.F.,loFormSet)
Select(lnCurAlias)
Return llReturn
*-- end of lpSavscr.


*!*************************************************************
*! Name      : lfNoSuffic
*! Developer : MMT -MARIAMM  MAZHAR
*! Date      : 03/01/2009
*! Purpose   : Check Stydye Availability
*!*************************************************************
*!
Function lfNoSuffic
Parameters loFormSet
lcPckLin =loFormSet.lcPckLin
lcStores = loFormSet.lcStores
lcOrderNo = loFormSet.ariaForm1.kbOrderNo.keytextbox.Value
Private lcMessage , lnI , lcI , lnChoice , llExitLoop, llRet2Main
Store .F. To llExitLoop, llRet2Main
Select(lcPckLin)
Scan
    llNoThing = gfSEEK('O'+lcOrderNo +&lcStores..Store+&lcPckLin..Style+Str(nOrdLineNo,6),'OrdLine')
    If llNoThing
        lnI = 0
        For lnI = 1 To 8
            lcI = Str(lnI,1)
            If (PQty&lcI > OrdLine.Pik&lcI) And ;
                    gfSEEK(OrdLine.Style+OrdLine.cWareCode+OrdLine.Dyelot,'StyDye') And ;
                    PQty&lcI > (StyDye.Stk&lcI - StyDye.Alo&lcI)
                lcMessage = "Order : " + lcOrderNo  + ", Style : " + ordline.Style +;
                    ", does not have available quantity."
                If loFormSet.llAlwForce
                    lcMessage = lcMessage + "Do you want to force allocation?"
                    *GFMODALGEN
                    * <Yes> <Yes to All> <No>
                    *lnChoice = 1 or 2 or 3
                    lnChoice  =gfModalGen("INM00000B44002","Dialog","","",lcMessage)
                    Do Case
                    Case lnChoice = 1
                        *DO NOTHING

                    Case lnChoice = 2
                        llExitLoop = .T.

                    Case lnChoice = 3
                        Store .T. To llExitLoop , llRet2Main
                    Endcase
                Else
                    lcMessage = lcMessage + "Save without update pick Quantity?"
                    *GFMODALGEN
                    * <Yes> <No>
                    *lnChoice=1 or 2
                    lnChoice =gfModalGen("INM00000B44009","Dialog","","",lcMessage)
                    If lnChoice=1
                        loFormset.llUpdtPkTk = .F.
                        llExitLoop = .T.

                    Else
                        Store .T. To llExitLoop , llRet2Main
                    Endif
                Endif
                *-- Exit for loop
                If llExitLoop
                    Exit
                Endif
            Endif
        Endfor

        *-- exit scan loop
        If llExitLoop
            Exit
        Endif

    Endif
Endscan
Return llRet2Main
*-- end of lfNoSuffic.
*!*************************************************************
*! Name      : lfSuppForc
*! Developer : MMT -MARIAM MAzhar
*! Date      : 03/01/2009
*! Purpose   : User can Force allocation. (Y/N)
*!*************************************************************
*!
Function lfSuppForc
Parameters loFormSet
Private llAlwForce
llAlwForce = .T.

If gfGetMemVar('M_FORCEALO',oAriaApplication.ActiveCompanyID)  <> "Y"
    *-- No Force allocation done.
    If gfGetMemVar('M_FORCEALO',oAriaApplication.ActiveCompanyID)   = "N"
        llAlwForce = .F.  && User can not
    Else  && User Prev.
        *-- Call user defined process.
        llAlwForce = gfUserPriv('AL','ALAUTAL','FORCING')
    Endif
Endif
Return llAlwForce
*-- end of lfSuppForc.

*!*************************************************************
*! Name      : lfChkOrdLok
*! Developer : Mariam MAzhar
*! Date      : 03/01/2009
*! Purpose   : Check using this order in creating pack and
*!           : Lock ordhdr record if it is not
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
Function lfChkOrdLok
Lparameters loFormSet
Private llGoOn,lnCurAlias,lnCurRecNo, lcOrderNo,lcStore, lcOldOrd

llGoOn     = .T.
lcOrderNo  = loFormSet.AriaForm1.kbOrderNo.keytextbox.Value
lnCurAlias = Select(0)
lcOldHOrd = Order('ORDHDR')
Select Ordhdr
gfSetOrder('ORDHDR')
If gfSEEK('O'+lcOrderNo)
    Select ORDHDR
    llGoOn = gfObj_Lock(.T.)
Endif
gfSetOrder(lcOldHOrd)
Select (lnCurAlias)
Return llGoOn

*:**************************************************************************
*:* Name        : lfOrdHdLck
*:* Developer   : Mariam Mazhar
*:* Date        : 03/01/2009
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
Function lfOrdHdLck
Parameters llLock,loFormSet
lcOrder = loFormSet.AriaForm1.kbOrderNo.keytextbox.Value
Private lnAlias,lcSvOrd,llGoOn
llGoOn = .T.
lnAlias = Select()
lcSvOrd = Order('ORDHDR')
Select ORDHDR
=gfSetOrder('ORDHDR')
=gfSEEK('O'+lcOrder,'ORDHDR')
=gfObj_Lock(llLock)
=gfSetOrder(lcSvOrd)
Select (lnAlias)
Return llGoOn
*-- end of lfOrdHdLck.

