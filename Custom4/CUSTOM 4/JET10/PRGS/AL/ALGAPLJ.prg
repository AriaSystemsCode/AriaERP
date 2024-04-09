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
*:************************************************************************
#INCLUDE D:\ARIA4XP\PRGS\ALPLIST.h
*: B609154,1 MMT 02/24/2010 Fix bug of error when run program on SAAS[Start]
IF oAriaApplication.MULTIINST 
 *: B609229,1 MMT 04/29/2010 Fix bug of error when run program on SAAS[Start] 
 *DO FORM ("X:\aria4xp\Screens\"+'AL\ALGAPLJ.SCX')
 =gfCallForm('ALGAPLJ','AL')
 *: B609229,1 MMT 04/29/2010 Fix bug of error when run program on SAAS[End] 
ELSE
*: B609154,1 MMT 02/24/2010 Fix bug of error when run program on SAAS[End]
DO FORM (oAriaApplication.ScreenHome+'AL\ALGAPLJ.SCX')
*: B609154,1 MMT 02/24/2010 Fix bug of error when run program on SAAS[Start]
ENDIF 
*: B609154,1 MMT 02/24/2010 Fix bug of error when run program on SAAS[End]

*!*************************************************************
*! Name      : lfFormInit
*: Developer : Mariam Mazhar
*: Date      : 03/01/2009
*! Purpose   : Init Method of the Form
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet
STORE 0 TO loFormset.lnFrom,loFormset.lnTo,loFormset.lnCtnPal,loFormSet.lnPackWgh
loFormset.llUpdtPkTk = .F.
IF FILE (oAriaApplication.DataDir+'UpPkTk.MEM')
  RESTORE FROM (oAriaApplication.DataDir+'UpPkTk.MEM') ADDITIVE
  loFormset.llUpdtPkTk = llUpdtPkTk 
ENDIF

STORE '' TO loFormset.cPckDsCode,loFormset.cPckChCode,loFormset.lcWareCode
loFormset.llComp = .F.
loFormset.llDyelot = .F.
STORE 0 TO  loFormset.lnDrctTo
STORE 0 TO loFormset.lnCtnTyp  ,loFormset.lnMaxCtn
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
loFormset.llDyelot = ALLTRIM(gfGetMemVar('M_DYELOT',oAriaApplication.ActiveCompanyID))='Y'


loFormSet.lcTmpUpLn =  gfTempName()
loFormSet.llExtSizSc = gfGetMemVar('M_USEEXSSC',oAriaApplication.ActiveCompanyID)
loFormSet.lcSizeSep  = ''
STORE 0 TO lnSizePos,lnSizeLen
IF loFormSet.llExtSizSc 
  DECLARE laStySeg[1,1]
  STORE "" TO laStySeg
  =gfItemMask(@laStySeg)
  FOR lnCnt = 1 TO ALEN(laStySeg,1)
    IF laStySeg[lnCnt , 1] = "S"
      loFormSet.lcSizeSep  = ALLTRIM(laStySeg[lnCnt-1 , 6])
      loFormSet.lnSizePos  = laStySeg[lnCnt , 4] - IIF(!EMPTY(loFormSet.lcSizeSep) , 1 , 0)
      loFormSet.lnSizeLen  = LEN(laStySeg[lnCnt , 3]) + IIF(!EMPTY(loFormSet.lcSizeSep) , 1 , 0)
    ENDIF
  ENDFOR
ENDIF

loFormSet.lcStyPic   = gfItemMask("PI")
loFormSet.lcStyTtl   = gfItemMask("HI")
loFormSet.lnStyleWid = LEN(loFormSet.lcStyPic)  
loFormSet.lcTmAsnShp = gfTempName()
IF loFormSet.llCanPrnLb 

 =gfOPenTable('Asn_Ship','Asn_Ship')
  IF !USED(loFormSet.lcTmAsnShp)
    SELECT 'Asn_Ship'
    COPY STRUCTURE TO (oAriaApplication.WorkDir+loFormSet.lcTmAsnShp)
    =gfOpenFile(oAriaApplication.WorkDir+loFormSet.lcTmAsnShp,'','EX')
    SELECT (loFormSet.lcTmAsnShp)
    INDEX ON STR(CART_NO,6) TAG (loFormSet.lcTmAsnShp)
  ENDIF 
  =gfOpenTable(oAriaApplication.SysPath+'SYCASNLB','ASNlbl')    
  llDetLabel = gfSEEK("XX1" + "H" , "SYCASNLB") .AND. gfSEEK("XX1" + "L" , "SYCASNLB")
  
  IF llDetLabel
    *-- Flag to know if UPC module is installed or not
    loFormSet.llUPCInst  = ('UP' $ oAriaApplication.CompanyInstalledModules)
    *-- Flag to know if the user want to print detailed shipping label for all cartons or not
    loFormSet.lcDetLbAll = ""
    
    IF loFormSet.llUPCInst
      =gfOpenTable( 'STYLEUPC' ,  'STYLEUPC' )
    ENDIF
  ENDIF
  
ENDIF

loFormSet.llEdiSys   = ('AS' $ oAriaApplication.CompanyInstalledModules)
IF loFormSet.llEdiSys
  =gfOPenTable('EDIAcPrt','ACCFACT')
  =gfOPenTable('EDIPH','PARTNER')
  =gfOPenTable('EDICRTSQ','EDICRTSQ')
  =gfOPenTable('BOL_HDR','BOL_HDR')
  =gfOPenTable('BOL_LIN','BOL_LIN')
ENDIF


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


*!*************************************************************
*! Name      : lfvOrderNo
*: Developer : Mariam Mazhar
*: Date      : 03/01/2009
*! Purpose   : Order number validation
*!*************************************************************
FUNCTION lfvOrderNo
LPARAMETERS loFormSet, lcOrderNo,lcAccount, llBrowse
PRIVATE lnCurAlias,lcOrdHdrTg,lcOrder
lnCurAlias = SELECT(0)

lcOrdHdrTg = ORDER('OrdHdr')
SELECT Ordhdr
=gfSetOrder('OrdHdr')

loFormSet.ariaform1.pgfPacking.header.grdHeadr.RecordSource = ''
loFormSet.AriaForm1.pgfPacking.DETAIL.grdDetail.RECORDSOURCE = ''
loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonH.RECORDSOURCE = ''
loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonD.RECORDSOURCE = ''




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
  SELECT Pack_Hdr
  lcTmpKey = pack_no
  lcOrdr = ORDER()
  =gfSetOrder('OrderPck')
  llPacked = gfSEEK(OrdHdr.Order+OrdHdr.Store)
  =gfSetOrder(lcOrdr )
  =gfSEEK(lcTmpKey)
  =gfSEEK('O'+lcOrderNo ,'OrdHdr')
  SELECT (lcAlias)
  
  DO CASE

    *--- and the orde packed.[Begin]
     CASE OrdHdr.Status = 'C' AND llPacked
      =gfModalGen("INM000000B00000","DIALOG",'','',;
      'This order is completed and packed, cannot pack.')  
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
    
      IF !gfSEEK(lcOrderNo,'TMPL_LIN')
        =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No tamplate saved for this order, can not proceed.')
        SELECT Ordhdr
        =gfSetOrder(lcOrdHdrTg)
        loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE = SPACE(6)
        SELECT (lnCurAlias)
        RETURN        
      ENDIF
      =gfSEEK(lcOrderNo,'TMPL_HDR')
      llNothing = lfCrtUnCmp(loFormSet)
      loFormset.llComp= (OrdHdr.Status = 'C' AND !llPacked)
      =lfGetData(loFormSet, lcOrderNo)
      SELECT PIKTKT
      =gfSetOrder('ORDPIK')
      
      *-- llHaSPik variable that indicates if this order has pikTkt or not
      IF gfSEEK(lcOrderNo,'PikTkt')
        lnAlias = SELECT(0)
        SELECT PikTkt
        LOCATE REST WHILE Order = lcOrderNo FOR Status $ 'PO'
        IF FOUND()
          llHasPik = .T.
        ELSE
          llHasPik = .F.
        ENDIF
        SELECT (lnAlias)
      ELSE
        llHasPik = .F.
      ENDIF
      =lfvNewPack(loFormSet)
      lcPckHdrOrd = order('pack_hdr')
      SELECT pack_hdr
      =gfSetOrder('ORDERPCK')
      IF !gfSEEK(lcOrderNo ,'PACK_HDR')
        =gfSetOrder(lcPckHdrOrd)
        loFormSet.ChangeMode('A')
      ELSE
        =gfSetOrder(lcPckHdrOrd)
        loFormSet.ChangeMode('V')
      ENDIF
  ENDCASE
ENDIF
SELECT Ordhdr
gfSetOrder(lcOrdHdrTg)
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfGetData
*: Developer : Mariam Mazhar
*: Date      : 03/01/2009
*! Purpose   : Get Order data
*!*************************************************************
FUNCTION lfGetData
LPARAMETERS loFormSet, lcOrderNo
PRIVATE lcPikTkt, lcStore
llNoThing = gfSEEK('O'+lcOrderNo,'OrdHdr')
WITH loFormSet.ariaForm1
  STORE IIF(!EMPTY(lcOrderNo),OrdHdr.Account,SPACE(6)) TO .kbAccount.keytextbox.VALUE, lcAccount
    
  loFormSet.llEdiAcc   = loFormSet.llEdiSys .AND. gfSEEK('A' + lcAccount, 'EDIACPRT') .AND.;
             gfSEEK(EDIACPRT.cPartCode , 'EDIPH')

  STORE loFormSet.llEdiSys .AND. loFormSet.llEdiAcc .AND. EDIACPRT.lPkChrDes  TO llPCDStat
  STORE loFormSet.llEdiSys .AND. loFormSet.llEdiAcc .AND. EDIPH.lPltShp  TO llPalStat

  lcStore  = OrdHdr.Store
  lcCusPo    = OrdHdr.CustPo
  lcDept     = OrdHdr.Dept
  .txtCustPo.VALUE = OrdHDR.CustPo
  loFormSet.lcWareCode = OrdHdr.cWareCode
  IF  loFormSet.llEdiAcc
     loFormSet.cPckChCode =  EDIACPRT.cPckChCode 
     loFormSet.cPckDsCode  =  EDIACPRT.cPckDsCode 
  ELSE
    loFormSet.lnCtnTyp   = 2
    loFormSet.lnDrctTo   = 1
  ENDIF
ENDWITH 
*!*************************************************************
*! Name      : lfvNewPack
*: Developer : Mariam Mazhar
*: Date      : 03/01/2009
*! Purpose   : Get new or existing packing list data
*!*************************************************************
FUNCTION lfvNewPack
LPARAMETERS loFormSet
PRIVATE lcTag,lnCurAlias
STORE .T. TO loFormSet.llCupdate,loFormSet.llAnyUpd,loFormSet.llNew
lcAccount = Ordhdr.account
lcStore = Ordhdr.Store 
IF ALLTRIM(OrdHdr.ShipVia)='*' AND gfSEEK('S'+lcAccount+lcStore ,'Customer')
  lcShipVia  = Customer.ShipVia
ENDIF
lcPckHdrOrd = ORDER('PACK_HDR')
SELECT PACK_HDR
=gfSetOrder('ORDERPCK')
*-- Get Order stores
=lfGtOrdSto(loFormSet)   
lcStores = loFormSet.lcStores
PRIVATE lcTmplOrd
lcTmplOrd = ORDER('TMPL_LIN')
SELECT  TMPL_LIN
=gfSetOrder('TMPL_LINS')
SELECT &lcStores
GO TOP
SCAN    
  WAIT WINDOW NOWAIT 'Collecting data for store '+&lcStores..STORE + ' D.C '+ &lcStores..DIST_CTR
  =lfvSelOrdL(loFormSet)
ENDSCAN
SELECT &lcStores
GO TOP
WAIT CLEAR  
SELECT PACK_HDR
=gfSetOrder(lcPckHdrOrd)
SELECT TMPL_LIN
=gfSetOrder(lcTmplOrd)
lcPckLin = loFormSet.lcPckLin
SELECT (lcPckLin)
GO TOP
=RLOCK(lcPckLin) 
UNLOCk IN (lcPckLin)
lcTmpPck = loFormSet.lcTmpPck
IF !USED(lcTmpPck)
  IF gfGetMemVar('M_ORDSTUTS',oAriaApplication.ActiveCompanyID) = 'L'
    USE (oAriaApplication.WorkDir+lcPckLin)  IN 0 AGAIN ALIAS (lcTmpPck) ORDER (loFormSet.lcPakIndxLn)
  ELSE
    USE (oAriaApplication.WorkDir+lcPckLin)  IN 0 AGAIN ALIAS (lcTmpPck) ORDER (loFormSet.lcPakIndxSt)
  ENDIF
ENDIF

lfDtlBrow(loFormSet)
*:**************************************************************************
*:* Name        : lfGtOrdSto
*:* Developer   : MMT -Mariam Mazhar
*:* Date        : 03/01/2009
*:* Purpose     : Get Order stores
*:***************************************************************************
FUNCTION lfGtOrdSto
PARAMETERS loFormSet
PRIVATE lcSvOrd,lcGtStore,lnStoOrdrd,lcSvRel,lcSvPkOrd
lnStoOrdrd = 0
lcSvOrd = ORDER('ORDLINE')
lcSvPkOrd = ORDER('PIKLINE')
SELECT  ORDLINE
=gfSetOrder("ORDLINST")
SELECT PIKLINE
=gfSetOrder("PIKLINEO")
SELECT PIKLINE
lcSvRel = SET('RELATION')
SET RELATION TO
GO TOP
lcGtStore = ' '
*! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
lcGtPikTkt = '  '
*! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
lcOrderNo = loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE  
=gfSEEK('O'+lcOrderNo ,'ORDLINE')
SELECT ORDLINE
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
SELECT * FROM Ordline WHERE CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+lcOrderNo AND !EMPTY(PIKTKT) ORDER BY STORE,PIKTKT,STYLE,LINENO INTO CURSOR 'TmpLines'
SELECT 'TmpLines'
LOCATE 
SCAN
  IF lcGtStore # TmpLines.STORE OR lcGtPikTkt  # TmpLines.PIKTKT
    lcGtPikTkt  = TmpLines.PIKTKT
    lcGtStore = TmpLines.STORE    
    =gfSEEK('S'+TmpLines.ACCOUNT+TmpLines.STORE,'CUSTOMER')
    =gfSEEK(lcOrderNo  +TmpLines.STORE+lcGtPikTkt ,'PACK_HDR')
    INSERT INTO (loFormSet.lcStores) (DIST_CTR, STORE, PIKTKT, TOTORD, TOTPIK, PACK_NO, TOTPQTY, WEIGHT, CARTONS, BOL_NO ) ; 
                    VALUES (CUSTOMER.DIST_CTR, TmpLines.STORE, TmpLines.PIKTKT, TmpLines.TOTBOOK, ;
                            TmpLines.TOTPIK, PACK_HDR.PACK_NO, PACK_HDR.TOT_PCS , PACK_HDR.TOT_WGHT ,PACK_HDR.TOT_CART,;
                            PACK_HDR.BILL_LADG )
    IF gfSEEK(TmpLines.ORDER+STR(TmpLines.LINENO,6),'PIKLINE')
      SELECT PikLine
      SCAN REST WHILE ORDER+STR(LINENO,6) = TmpLines.ORDER+STR(TmpLines.LINENO,6) FOR gfSeek(PIKLINE.PIKTKT,'PIKTKT','PIKTKT') AND PIKTKT.Status <> 'X'
        SELECT (loFormSet.lcStores)
        REPLACE PIKTKT WITH PIKLINE.PIKTKT,;
                TOTPIK WITH PIKLINE.TOTPIK
        EXIT         
      ENDSCAN           
    ENDIF                        
  ELSE
    REPLACE &lcStores..TOTORD WITH &lcStores..TOTORD + TmpLines.TOTBOOK,;
            &lcStores..TOTPIK WITH &lcStores..TOTPIK + ;
            IIF(gfSEEK(TmpLines.ORDER+STR(TmpLines.LINENO,6),'PIKLINE'),PIKLINE.TOTPIK,TmpLines.TOTPIK)
  ENDIF
ENDSCAN
*! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]

GO TOP IN &lcStores
= lfwStrsBr(loFormSet)

SELECT PIKLINE
=gfsetOrder(lcSvPkOrd )
SELECT ORDLINE
=gfsetOrder(lcSvOrd )
*-- end of lfGtOrdSto.
*:**************************************************************************
*:* Name        : lfCrtUnCmp
*:* Developer   : MMT -Mariam Mazhar
*:* Date        : 03/01/2009
*:* Purpose     : Create Temp Files
*:**************************************************************************
FUNCTION lfCrtUnCmp
PARAMETERS loFormSet
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
FUNCTION lfCrSelFiles
PARAMETERS loFormSet

PRIVATE lnCurAlias,lnI
lnCurAlias = SELECT(0)

lnI = 1

DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect1'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect2'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

*! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PIKTKT'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0
*! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect3'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect4'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect5'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect6'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect7'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSelect8'
laFileStru[lnI,2] = 'C'
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
laFileStru[lnI,1] = 'SzCnt'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize1'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize2'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize3'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize4'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize5'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize6'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize7'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize8'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgPWgh'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdQty1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdQty2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdQty3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdQty4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdQty5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdQty6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdQty7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdQty8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

*-- Total order Qty
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

*--total Avalable Qty
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0
*--

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OQty8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

*-- total open qty
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0
*--

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

*-- total carton Qty
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
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
laFileStru[lnI,1] = 'PQty1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

*--
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PTotQty'
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
laFileStru[lnI,1] = 'PWgh1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PWgh2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PWgh3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PWgh4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PWgh5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PWgh6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PWgh7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PWgh8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

*-- total wight
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PTotWgh'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2
*--

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
laFileStru[lnI,1] = 'nOrdLineNo'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Selected'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0


*-- total original Qty
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgTotOrd'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0

*-- total original Qty

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0


lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

*-- total diffrance Qty
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nTotDiff'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
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

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'STORE'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 8
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
laFileStru[lnI,1] = 'UpOqty1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'UpOqty2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'UpOqty3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'UpOqty4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'UpOqty5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'UpOqty6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'UpOqty7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'UpOqty8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

*-- total open qty
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'UOTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0


lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cNoSize'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'llFrst'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

*--
DIMENSION laIndx[7,2]

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
SET ORDER TO (loFormSet.lcPakIndxLn) IN (loFormSet.lcPckLin)

SELECT PACK_LIN
=AFIELDS(laFileStru)

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,18]
laFileStru[lnI,1] = 'STORE'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

STORE ' ' TO  laFileStru[lnI,7],laFileStru[lnI,8],;
                laFileStru[lnI,9],laFileStru[lnI,10],;
                laFileStru[lnI,11],laFileStru[lnI,12],;
                laFileStru[lnI,13],laFileStru[lnI,14],;
                laFileStru[lnI,15],laFileStru[lnI,16]
STORE 0 TO    laFileStru[lnI,17] ,laFileStru[lnI,18]


DIMENSION laIndx[1,2]
laIndx[1,1] = "PACK_ID+cPkColor+cPckSize+cPkVersion+Style+STR(nOrdLineNo,6)"
laIndx[1,2] = loFormSet.lcPack_Lin

=gfCrtTmp(loFormSet.lcPack_Lin,@laFileStru,@laIndx)
SET ORDER TO (loFormSet.lcPakIndxLn) IN (loFormSet.lcPckLin)




*:************* Create Stores temp file  ************************************
lnI = 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lSelect'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'STORE'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'DIST_CTR'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PIKTKT'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TOTORD'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TOTPIK'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Cartons'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 9
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TOTPQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PACK_NO'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'BOL_NO'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

DIMENSION laIndx[1,2]
laIndx[1,1] = "DIST_CTR+STORE"
laIndx[1,2] = loFormSet.lcStores

=gfCrtTmp(loFormSet.lcStores,@laFileStru,@laIndx)
SET ORDER TO (loFormSet.lcStores) IN (loFormSet.lcStores)

SELECT (lnCurAlias)

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

FUNCTION lfCrCtnFiles
PARAMETERS loFormSet
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

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'STORE'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 8
laFileStru[lnI,4] = 0

*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PIKTKT'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]


DIMENSION laIndx[3,2]
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
SET ORDER TO (loFormSet.lcCtnHdr) In (loFormSet.lcCtnHdr)


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
laFileStru[lnI,1] = 'nOrdLineNo'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
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

*---(Start) Increased the width of the line no field used in 
*---the packing list lines to be 6 instead of 3 digits
*---not to have an error when creating big packing lists
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
laFileStru[lnI,1] = 'STORE'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 8
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
laFileStru[lnI,1] = 'cNoSize'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PIKTKT'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]

DIMENSION laIndx[2,2]
laIndx[1,1] = "STR(Cart_No,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style+STR(nOrdLineNo,6)" 
laIndx[1,2] = loFormSet.lcCtnDtl

laIndx[2,1] = "cStatus"
laIndx[2,2] = "Status"

=gfCrtTmp(loFormSet.lcCtnDtl,@laFileStru,@laIndx)           
SET ORDER TO (loFormSet.lcCtnDtl) In (loFormSet.lcCtnDtl)

SELECT (lnCurAlias)

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

FUNCTION lfCrSelSum
PARAMETERS loFormSet
PRIVATE lnCurAlias,lnI
lnCurAlias = SELECT(0)

*--We modify way of selection to just select By Style-Color
lnI = 1

DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lSelect'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0


lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 2

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

DIMENSION laIndx[1,2]

laIndx[1,1] = "PACK_ID+cPkColor+cPckSize+cPkVersion"
laIndx[1,2] = loFormSet.lcSumPck

=gfCrtTmp(loFormSet.lcSumPck,@laFileStru,@laIndx)
SET ORDER TO (loFormSet.lcSumPck) IN (loFormSet.lcSumPck)

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfwStrsBr
*! Developer : Mariam Mazhar
*! Date      : 03/01/2009
*! Purpose   : Store Grid Control source assigment
*!*************************************************************
FUNCTION  lfwStrsBr
PARAMETERS loFormSet
loFormSet.ariaform1.pgfPacking.header.grdHeadr.RecordSource = ''
loFormSet.ariaform1.pgfPacking.header.grdHeadr.RecordSource = loFormSet.lcStores   
loFormSet.ariaform1.pgfPacking.header.grdHeadr.Column11.ControlSource = loFormSet.lcStores+'.lSelect'
IF TYPE('loFormSet.ariaform1.pgfPacking.header.grdHeadr.Column11.Ariacheckbox1') <> 'O'
  loFormSet.ariaform1.pgfPacking.header.grdHeadr.Column11.Addobject('Ariacheckbox1','Ariacheckbox')
ENDIF 
loFormSet.ariaform1.pgfPacking.header.grdHeadr.Column11.CurrentControl = 'Ariacheckbox1'
loFormSet.ariaform1.pgfPacking.header.grdHeadr.Column1.ControlSource= loFormSet.lcStores+'.Store'
loFormSet.ariaform1.pgfPacking.header.grdHeadr.Column2.ControlSource= loFormSet.lcStores+'.PikTkt'
loFormSet.ariaform1.pgfPacking.header.grdHeadr.Column3.ControlSource = loFormSet.lcStores+'.TOTORD'
loFormSet.ariaform1.pgfPacking.header.grdHeadr.Column4.ControlSource = loFormSet.lcStores+'.TOTPIK'
loFormSet.ariaform1.pgfPacking.header.grdHeadr.Column5.ControlSource= loFormSet.lcStores+'.Cartons'
loFormSet.ariaform1.pgfPacking.header.grdHeadr.Column6.ControlSource= loFormSet.lcStores+'.Weight'
loFormSet.ariaform1.pgfPacking.header.grdHeadr.Column7.ControlSource= loFormSet.lcStores+'.TOTPQty'
loFormSet.ariaform1.pgfPacking.header.grdHeadr.Column8.ControlSource= loFormSet.lcStores+'.PACK_NO'
loFormSet.ariaform1.pgfPacking.header.grdHeadr.Column9.ControlSource= loFormSet.lcStores+'.BOL_NO'
loFormSet.ariaform1.pgfPacking.header.grdHeadr.Column10.ControlSource = loFormSet.lcStores+'.DIST_CTR'
loFormSet.ariaform1.pgfPacking.header.grdHeadr.readonly = .T.
loFormSet.ariaform1.pgfPacking.header.grdHeadr.Column11.readonly =.F.
loFormSet.ariaform1.pgfPacking.header.grdHeadr.refresh
lfwStrsBr1(loFormSet)

*!*************************************************************
*! Name      : lfvSelOrdL
*! Developer : Mariam Mazhar
*! Date      : 03/01/2009
*! Purpose   : Select order data
*!*************************************************************
FUNCTION lfvSelOrdL
PARAMETERS loFormSet
PRIVATE lnAlias,lnI,lnJ,lcSize,lnQty,lcStyle,lcStyDesc,lnContinue,;
        lnCtnQty,lnCtnWgh,lnStyOrdLin,llStyFound,lnRemPQty


lcCtnHdr = loFormSet.lcCtnHdr
lcStores = loFormSet.lcStores
lcCtnDtl = loFormSet.lcCtnDtl
*-- This variable indecates if the style is found more than once in order
llStyFound = .F.
STORE 0 TO lnContinue,lnCtnQty,lnCtnWgh,lnI,lnJ,lnStyOrdLin,lnRemPQty
SELECT &lcCtnHdr
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
*SET FILTER TO STORE = &lcStores..STORE
SET FILTER TO STORE = &lcStores..STORE AND PIKTKT =&lcStores..PIKTKT
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
GO TOP

SELECT &lcCtnDtl
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
*SET FILTER TO STORE = &lcStores..STORE
SET FILTER TO STORE = &lcStores..STORE AND PIKTKT =&lcStores..PIKTKT
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
GO TOP

loFormSet.llAnyUpd = .F.
lnAlias = ALIAS()
lcorder =  loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE
SELECT OrdLine
=gfSetOrder('Ordlinst')
llNoThing = gfSEEK('O'+lcorder +&lcStores..STORE,'OrdLine')
STORE SPACE(0) TO lcStyle,lcStyDesc
lcExp = IIF(!EMPTY(&lcStores..PIKTKT),"WHILE cOrdType+Order+Store='O'+lcorder+&lcStores..STORE FOR PikTkt=&lcStores..PIKTKT AND Picked",;
                              "WHILE cOrdType+Order+Store='O'+lcorder+&lcStores..STORE")
*--- llFrmPikLn --> .T. ----> colect data from pikline , .F. --> from ordline

llFrmPikLn = .F.
IF !EMPTY(&lcStores..PIKTKT)
  SELECT PIKTKT
  =gfSetOrder('PIKTKT')
  IF gfSEEK(&lcStores..PIKTKT) AND Status = 'C'
     llFrmPikLn = .T.
     lcExp = "WHILE PikTkt+Order+Store=&lcStores..PIKTKT+lcorder+&lcStores..STORE FOR Picked "
  ENDIF
ENDIF
*-- move the pointer to frist record 
IF llFrmPikLn
  SELECT "PikLine"
  =gfSEEK(&lcStores..PIKTKT+lcorder)
ELSE
  SELECT "OrdLine"
  =gfSEEK('O'+lcorder+&lcStores..STORE)
ENDIF
lcPckLin = loFormSet.lcPckLin
SCAN REST &lcExp
  =gfSeek(Style,'Style')
  =gfSeek('S'+Style.scale,'Scale')
  llFrstRecord = .T.
  FOR lnIScl = 1 TO SCALE.CNT    
    lcSz = STR(lnIScl,1)
    
    IF IIF(llFrmPikLn,PikLine.Qty&lcSz.,OrdLine.Qty&lcSz.) = 0
      LOOP 
    ENDIF 
    
    SELECT (lcPckLin)
    APPEND BLANK
    REPLACE llFrst     WITH llFrstRecord 
    llFrstRecord = .F.
    *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
    REPLACE PIKTKT WITH IIF(llFrmPikLn,PikLine.PIKTKT ,OrdLine.PIKTKT )
    *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
    REPLACE Style      WITH IIF(llFrmPikLn,PikLine.Style,OrdLine.Style),;
            Scale      WITH Scale.Scale,;
            SzCnt      WITH Scale.Cnt,;
            StyWgh     WITH Style.nStyWeight,;
            OrgStyWgh  WITH Style.nStyWeight,;
            nOrdLineNo WITH IIF(llFrmPikLn,PikLine.LineNo,OrdLine.LineNo),;
            LPicked    WITH IIF(llFrmPikLn,PikLine.Picked,OrdLine.Picked),;
            cSize1     WITH Scale.Sz1,;
            cSize2     WITH Scale.Sz2,;
            cSize3     WITH Scale.Sz3,;
            cSize4     WITH Scale.Sz4,;
            cSize5     WITH Scale.Sz5,;
            cSize6     WITH Scale.Sz6,;
            cSize7     WITH Scale.Sz7,;
            cSize8     WITH Scale.Sz8,;
            AvlQty1    WITH IIF(EMPTY(&lcStores..PIKTKT),OrdLine.Qty1,;
                            IIF(llFrmPikLn,PikLine.Pik1,OrdLine.Pik1)),;
            AvlQty2    WITH IIF(EMPTY(&lcStores..PIKTKT),OrdLine.Qty2,;
                            IIF(llFrmPikLn,PikLine.Pik2,OrdLine.Pik2)),;
            AvlQty3    WITH IIF(EMPTY(&lcStores..PIKTKT),OrdLine.Qty3,;
                            IIF(llFrmPikLn,PikLine.Pik3,OrdLine.Pik3)),;
            AvlQty4    WITH IIF(EMPTY(&lcStores..PIKTKT),OrdLine.Qty4,;
                            IIF(llFrmPikLn,PikLine.Pik4,OrdLine.Pik4)),;
            AvlQty5    WITH IIF(EMPTY(&lcStores..PIKTKT),OrdLine.Qty5,;
                            IIF(llFrmPikLn,PikLine.Pik5,OrdLine.Pik5)),;
            AvlQty6    WITH IIF(EMPTY(&lcStores..PIKTKT),OrdLine.Qty6,;
                            IIF(llFrmPikLn,PikLine.Pik6,OrdLine.Pik6)),;
            AvlQty7    WITH IIF(EMPTY(&lcStores..PIKTKT),OrdLine.Qty7,;
                            IIF(llFrmPikLn,PikLine.Pik7,OrdLine.Pik7)),;
            AvlQty8    WITH IIF(EMPTY(&lcStores..PIKTKT),OrdLine.Qty8,;
                            IIF(llFrmPikLn,PikLine.Pik8,OrdLine.Pik8)),;
            OrdQty1    WITH IIF(llFrmPikLn,PikLine.Qty1,OrdLine.Qty1),;
            OrdQty2    WITH IIF(llFrmPikLn,PikLine.Qty2,OrdLine.Qty2),;
            OrdQty3    WITH IIF(llFrmPikLn,PikLine.Qty3,OrdLine.Qty3),;
            OrdQty4    WITH IIF(llFrmPikLn,PikLine.Qty4,OrdLine.Qty4),;
            OrdQty5    WITH IIF(llFrmPikLn,PikLine.Qty5,OrdLine.Qty5),;
            OrdQty6    WITH IIF(llFrmPikLn,PikLine.Qty6,OrdLine.Qty6),;
            OrdQty7    WITH IIF(llFrmPikLn,PikLine.Qty7,OrdLine.Qty7),;
            OrdQty8    WITH IIF(llFrmPikLn,PikLine.Qty8,OrdLine.Qty8),;
            STORE      WITH &lcStores..STORE          

    REPLACE AvlTotQty  WITH (AvlQty1+AvlQty2+AvlQty3+AvlQty4+AvlQty5+AvlQty6+AvlQty7+AvlQty8),;
            OrdTotQty  WITH (OrdQty1+OrdQty2+OrdQty3+OrdQty4+OrdQty5+OrdQty6+OrdQty7+OrdQty8)

    REPLACE OQty1  WITH MAX(0,AvlQty1-Ordline.nPck1),;
            OQty2  WITH MAX(0,AvlQty2-Ordline.nPck2),;
            OQty3  WITH MAX(0,AvlQty3-Ordline.nPck3),;
            OQty4  WITH MAX(0,AvlQty4-Ordline.nPck4),;
            OQty5  WITH MAX(0,AvlQty5-Ordline.nPck5),;
            OQty6  WITH MAX(0,AvlQty6-Ordline.nPck6),;
            OQty7  WITH MAX(0,AvlQty7-Ordline.nPck7),;
            OQty8  WITH MAX(0,AvlQty8-Ordline.nPck8),;
            OTotQty WITH MAX(0,OQty1+OQty2+OQty3+OQty4+OQty5+OQty6+OQty7+OQty8),;
            PQty1  WITH OrdLine.nPck1,;
            PQty2  WITH OrdLine.nPck2,;
            PQty3  WITH OrdLine.nPck3,;
            PQty4  WITH OrdLine.nPck4,;
            PQty5  WITH OrdLine.nPck5,;
            PQty6  WITH OrdLine.nPck6,;
            PQty7  WITH OrdLine.nPck7,;
            PQty8  WITH OrdLine.nPck8
    REPLACE PTotQty WITH (PQty1+PQty2+PQty3+PQty4+PQty5+PQty6+PQty7+PQty8),;
            OTotQty WITH (OQty1+OQty2+OQty3+OQty4+OQty5+OQty6+OQty7+oQty8)
            

    REPLACE UpOqty1   WITH MAX(0,AvlQty1-Ordline.nPck1),;
            UpOqty2   WITH MAX(0,AvlQty2-Ordline.nPck2),;
            UpOqty3   WITH MAX(0,AvlQty3-Ordline.nPck3),;
            UpOqty4   WITH MAX(0,AvlQty4-Ordline.nPck4),;
            UpOqty5   WITH MAX(0,AvlQty5-Ordline.nPck5),;
            UpOqty6   WITH MAX(0,AvlQty6-Ordline.nPck6),;
            UpOqty7   WITH MAX(0,AvlQty7-Ordline.nPck7),;
            UpOqty8   WITH MAX(0,AvlQty8-Ordline.nPck8),;
            UOTotQty WITH MAX(0,UpOqty1+UpOqty2+UpOqty3+UpOqty4+UpOqty5+UpOqty6+UpOqty7+UpOqty8)


    IF (OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8) <> 0
      REPLACE PWgh1      WITH (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty1,;
              PWgh2      WITH (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty2,;
              PWgh3      WITH (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty3,;
              PWgh4      WITH (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty4,;
              PWgh5      WITH (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty5,;
              PWgh6      WITH (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty6,;
              PWgh7      WITH (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty7,;
              PWgh8      WITH (OrdLine.nPWght/(OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8))*PQty8
    ELSE
      REPLACE PWgh1      WITH 0,;
              PWgh2      WITH 0,;
              PWgh3      WITH 0,;
              PWgh4      WITH 0,;
              PWgh5      WITH 0,;
              PWgh6      WITH 0,;
              PWgh7      WITH 0,;
              PWgh8      WITH 0
      
    ENDIF           

    REPLACE cNoSize    WITH lcSz  
    REPLACE PTotWgh    WITH (PWgh1+PWgh2+PWgh3+PWgh4+PWgh5+PWgh6+PWgh7+PWgh8)
            
    REPLACE PACK_ID    WITH IIF(llFrmPikLn , PikLine.PACK_ID    , OrdLine.PACK_ID),;
            cPkColor   WITH IIF(llFrmPikLn , PikLine.cPkColor   , OrdLine.cPkColor),;
            cPckSize   WITH IIF(llFrmPikLn , PikLine.cPckSize   , OrdLine.cPckSize),;
            cPkVersion WITH IIF(llFrmPikLn , PikLine.cPkVersion    , OrdLine.cPkVersion)

    REPLACE lRange     WITH IIF(llFrmPikLn,PikLine.lRange, OrdLine.lRange),;
            llPack     WITH !EMPTY(PACK_ID)
    
    IF gfSEEK(lcorder+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE,'TMPL_LIN')
      REPLACE CtnQty1   WITH TMPL_LIN.QTY1,;
              CtnQty2   WITH TMPL_LIN.QTY2,;
              CtnQty3   WITH TMPL_LIN.QTY3,;
              CtnQty4   WITH TMPL_LIN.QTY4,;
              CtnQty5   WITH TMPL_LIN.QTY5,;
              CtnQty6   WITH TMPL_LIN.QTY6,;
              CtnQty7   WITH TMPL_LIN.QTY7,;
              CtnQty8   WITH TMPL_LIN.QTY8,;
              CTNTOTQTY WITH CtnQty1+CtnQty2+CtnQty3+CtnQty4+ ;
                             CtnQty5+CtnQty6+CtnQty7+CtnQty8
      *-- Show in the detail folder the unt/wgt not total unit for each size
      REPLACE Weight1   WITH TMPL_LIN.Weight,;
              Weight2   WITH TMPL_LIN.Weight,;
              Weight3   WITH TMPL_LIN.Weight,;
              Weight4   WITH TMPL_LIN.Weight,;
              Weight5   WITH TMPL_LIN.Weight,;
              Weight6   WITH TMPL_LIN.Weight,;
              Weight7   WITH TMPL_LIN.Weight,;
              Weight8   WITH TMPL_LIN.Weight
    ENDIF

    IF lRange
      =gfSEEK(lcorder+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION,'TMPL_LIN')
      REPLACE Weight1   WITH TMPL_LIN.Weight,;
              Weight2   WITH TMPL_LIN.Weight,;
              Weight3   WITH TMPL_LIN.Weight,;
              Weight4   WITH TMPL_LIN.Weight,;
              Weight5   WITH TMPL_LIN.Weight,;
              Weight6   WITH TMPL_LIN.Weight,;
              Weight7   WITH TMPL_LIN.Weight,;
              Weight8   WITH TMPL_LIN.Weight
    ENDIF
    loFormSet.llAnyRec = .T.  
  ENDFOR           
ENDSCAN


SELECT (lcPckLin)
lcPack_Id = ""
SCAN FOR lRange
  lnPckedQty = 0

  IF lcPack_Id <> PACK_ID
    lcPack_Id = PACK_ID
    IF gfSEEK(lcorder+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION,'TMPL_LIN')
      lnPckedQty = TMPL_LIN.TotQty
      lnWeight   = TMPL_LIN.Weight
    ENDIF
  ENDIF
  IF lnPckedQty > 0

    =gfSEEK(Style,'STYLE') AND gfSEEK('S'+Style.Scale,'SCALE')
    IF lnPckedQty < OTOTQTY
      lnPkQty = lnPckedQty/SCALE.Cnt
      lnMod   = MOD(lnPkQty,1)
      llEven = .F.
      FOR lnI = 1 TO SCALE.Cnt
        lcI = STR(lnI,1)
        REPLACE CtnQty&lcI WITH IIF(!llEven,lnPkQty-lnMod,lnPkQty+lnMod)
        llEven = !llEven
      ENDFOR  
    ELSE
      FOR lnI = 1 TO SCALE.Cnt
        lcI = STR(lnI,1)
        REPLACE CtnQty&lcI WITH OQty&lcI
      ENDFOR  
    ENDIF
    REPLACE CTNTOTQTY WITH CtnQty1+CtnQty2+CtnQty3+CtnQty4+CtnQty5+CtnQty6+CtnQty7+CtnQty8
  ENDIF
ENDSCAN

SELECT Pack_Lin
lcTag = ORDER()
=gfSetOrder('PackStyle')

IF !EMPTY(&lcStores..PACK_NO)  
  IF gfSEEK(&lcStores..PACK_NO,'Pack_Lin')
    SCAN REST WHILE pack_no = &lcStores..PACK_NO
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
      IF gfSEEK(&lcSearExp.,'Ordline')
        SELECT OrdLine
        lnPackLin = 0
        SCAN REST WHILE cOrdType+Order+Store+Style+STR(LineNo,6) = ;
                          &lcSearExp. 
          lnStyOrdLin = OrdLine.LineNo
          llNoThing = SEEK('O'+lcorder+&lcStores..STORE+Pack_Lin.Style+STR(lnStyOrdLin,6),'Ordline')
          SET ORDER TO (loFormset.lcPakIndxSt) IN (loFormset.lcPckLin)
          llNoThing = SEEK(Pack_ID+cPkColor+cPckSize +cPkVersion+Pack_Lin.Style+STR(lnStyOrdLin,6),lcPckLin)
  
          IF (EMPTY(&lcStores..PIKTKT) AND !OrdLine.Picked) OR (!EMPTY(&lcStores..PIKTKT) AND IIF(llFrmPikLn,!OrdLine.Picked,OrdLine.Picked))
            SELECT (lcCtnDtl)
            lnPackLin = lnPackLin+1
            =gfSeek(Pack_Lin.Style,'Style')
            =gfSeek("S"+Style.Scale,'Scale')
            llFirstRec = .T.
            FOR lnI = 1 TO Scale.Cnt 
              lcI =STR(lnI,1)
              IF Pack_Lin.Qty&lcI. = 0
                LOOP 
              ENDIF 
              APPEND BLANK
              *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
              REPLACE PIKTKT WITH &lcStores..PIKTKT               
              *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
              REPLACE Style      WITH Pack_Lin.Style,;
                      STORE      WITH &lcStores..STORE,;
                      SzCnt      WITH &lcPckLin..SzCnt,;
                      cStatus    WITH "A",;
                      nOrdLineNo WITH lnStyOrdLin,;
                      PackLineNo WITH lnPackLin,;
                      Cart_No    WITH Pack_Lin.No_Cart,;
                      Qty1       WITH Pack_Lin.Qty1,;
                      Qty2       WITH Pack_Lin.Qty2,;
                      Qty3       WITH Pack_Lin.Qty3,;
                      Qty4       WITH Pack_Lin.Qty4,;
                      Qty5       WITH Pack_Lin.Qty5,;
                      Qty6       WITH Pack_Lin.Qty6,;
                      Qty7       WITH Pack_Lin.Qty7,;
                      Qty8       WITH Pack_Lin.Qty8,;
                      TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
              REPLACE Size1      WITH IIF(Qty1>0,&lcPckLin..cSize1,Size1),;
                      Size2      WITH IIF(Qty2>0,&lcPckLin..cSize2,Size2),;
                      Size3      WITH IIF(Qty3>0,&lcPckLin..cSize3,Size3),;
                      Size4      WITH IIF(Qty4>0,&lcPckLin..cSize4,Size4),;
                      Size5      WITH IIF(Qty5>0,&lcPckLin..cSize5,Size5),;
                      Size6      WITH IIF(Qty6>0,&lcPckLin..cSize6,Size6),;
                      Size7      WITH IIF(Qty7>0,&lcPckLin..cSize7,Size7),;
                      Size8      WITH IIF(Qty8>0,&lcPckLin..cSize8,Size8)
              REPLACE Br1        WITH !EMPTY(Qty1),;
                      Br2        WITH !EMPTY(Qty2),;
                      Br3        WITH !EMPTY(Qty3),;
                      Br4        WITH !EMPTY(Qty4),;
                      Br5        WITH !EMPTY(Qty5),;
                      Br6        WITH !EMPTY(Qty6),;
                      Br7        WITH !EMPTY(Qty7),;
                      Br8        WITH !EMPTY(Qty8),;
                      Weight1    WITH Qty1*(Pack_Lin.Weight/Pack_Lin.TotQty),;
                      Weight2    WITH Qty2*(Pack_Lin.Weight/Pack_Lin.TotQty),;
                      Weight3    WITH Qty3*(Pack_Lin.Weight/Pack_Lin.TotQty),;
                      Weight4    WITH Qty4*(Pack_Lin.Weight/Pack_Lin.TotQty),;
                      Weight5    WITH Qty5*(Pack_Lin.Weight/Pack_Lin.TotQty),;
                      Weight6    WITH Qty6*(Pack_Lin.Weight/Pack_Lin.TotQty),;
                      Weight7    WITH Qty7*(Pack_Lin.Weight/Pack_Lin.TotQty),;
                      Weight8    WITH Qty8*(Pack_Lin.Weight/Pack_Lin.TotQty),;
                      OrgWgh     WITH &lcPckLin..OrgStyWgh
              REPLACE TotWeight  WITH (Weight1+Weight2+Weight3+Weight4+Weight5+Weight6+Weight7+Weight8)      
              REPLACE cNoSize    WITH lcI 
             * REPLACE llFirst WITH llFirstRec 
             * llFirstRec = .F.
            ENDFOR   
            IF SEEK(STR(Pack_Lin.No_Cart,4),lcCtnHdr)
              SELECT (lcCtnHdr)
              REPLACE TotPcs WITH TotPcs + ;
                                  &lcCtnDtl..Qty1+&lcCtnDtl..Qty2+;
                                  &lcCtnDtl..Qty3+&lcCtnDtl..Qty4+;
                                  &lcCtnDtl..Qty5+&lcCtnDtl..Qty6+;
                                  &lcCtnDtl..Qty7+&lcCtnDtl..Qty8,;
                      TotWgh WITH TotWgh + ;
                                  &lcCtnDtl..Weight1+&lcCtnDtl..Weight2+;
                                  &lcCtnDtl..Weight3+&lcCtnDtl..Weight4+;
                                  &lcCtnDtl..Weight5+&lcCtnDtl..Weight6+;
                                  &lcCtnDtl..Weight7+&lcCtnDtl..Weight8
            ELSE
              IF (&lcCtnDtl..Qty1+&lcCtnDtl..Qty2+;
                  &lcCtnDtl..Qty3+&lcCtnDtl..Qty4+;
                  &lcCtnDtl..Qty5+&lcCtnDtl..Qty6+;
                  &lcCtnDtl..Qty7+&lcCtnDtl..Qty8) > 0

                loFormset.lnMaxCtn = MAX(loFormset.lnMaxCtn,Pack_Lin.No_Cart)
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
                INSERT INTO (lcCtnHdr) (Cart_No,Pal_No,TotPcs,TotWgh,Empty, STORE,PIKTKT);
                                VALUES (Pack_Lin.No_Cart,Pack_Lin.NPltNo,;
                                        &lcCtnDtl..Qty1+&lcCtnDtl..Qty2+;
                                        &lcCtnDtl..Qty3+&lcCtnDtl..Qty4+;
                                        &lcCtnDtl..Qty5+&lcCtnDtl..Qty6+;
                                        &lcCtnDtl..Qty7+&lcCtnDtl..Qty8,;
                                        &lcCtnDtl..Weight1+&lcCtnDtl..Weight2+;
                                        &lcCtnDtl..Weight3+&lcCtnDtl..Weight4+;
                                        &lcCtnDtl..Weight5+&lcCtnDtl..Weight6+;
                                        &lcCtnDtl..Weight7+&lcCtnDtl..Weight8,'N', &lcStores..STORE, &lcStores..PIKTKT)
				*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
 
              ENDIF
            ENDIF
          ENDIF
        ENDSCAN
      ENDIF
    ENDSCAN
  ENDIF
ELSE
  STORE 1 TO loFormset.lnFrom,loFormset.lnTo
ENDIF
SELECT Pack_Lin
=gfSetOrder(lcTag )


*:**************************************************************************
*:* Name        : lfwStrsBr1
*:* Developer : MMT -Mariam Mazhar
*:* Date        : 03/01/2009
*:* Purpose     : When functions for stores browse
*:***************************************************************************
FUNCTION lfwStrsBr1
PARAMETERS loFormset
PRIVATE lnAlias,laSelct,lcOpnTmpl
lcStores = loFormset.lcStores
lcOrder =loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE 
lnAlias = SELECT(0)
SELECT (lcStores)
lnStBrRec = RECNO()
DIMENSION laSelct[1]
laSelct = .F.
SELECT COUNT(lSelect) FROM (lcStores) WHERE !EMPTY(lcStores) INTO ARRAY laSelct
loFormset.ARiaform1.PgfPacking.Header.CMDGen.Enabled = !EMPTY(laSelct[1]) and  loFormset.ActiveMode $ 'EA'
loFormset.ARiaform1.PgfPacking.Header.cmdTemp.Enabled = !EMPTY(lcOrder)
SELECT (lcStores)
SELECT(lnAlias)

*:**************************************************************************
*:* Name        : lfDtlBrow
*:* Developer : MMT -Mariam Mazhar
*:* Date        : 03/01/2009
*:* Purpose     : Detail browse control source
*:***************************************************************************
FUNCTION lfDtlBrow
PARAMETERS loFormSet
PRIVATE lnCurAlias
lnCurAlias = SELECT(0)
lcPckLin = loFormSet.lcPckLin
lcTmpPck = loFormSet.lcTmpPck
IF !USED(lcTmpPck)
  IF gfGetMemVar('M_ORDSTUTS',oAriaApplication.ActiveCompanyID) = 'L'
    USE (oAriaApplication.WorkDir+lcPckLin)  IN 0 AGAIN ALIAS (lcTmpPck) ORDER (loFormSet.lcPakIndxLn)
  ELSE
    USE (oAriaApplication.WorkDir+lcPckLin)  IN 0 AGAIN ALIAS (lcTmpPck) ORDER (loFormSet.lcPakIndxSt)
  ENDIF
ENDIF

lcTemFile = lcTmpPck
SELECT (lcTemFile)
lnDtBrRec = RECNO()
WITH loFormSet.AriaForm1.pgfPacking.DETAIL.grdDetail
  .RECORDSOURCE = ''
  SELECT (loFormSet.lcTmpPck)
  *SET FILTER TO
  LOCATE
	IF EOF()
	  RETURN
	ENDIF 
    
    .RECORDSOURCE = loFormSet.lcTmpPck
    
     *-- Pack-Id
    .Column2.CONTROLSOURCE  = 'ThisFormset.lfGetDetPack()'
    .Column2.Header1.caption = 'Pack_Id-Color-Size-Version'
    .Column2.VISIBLE = .T.
     *-- Style
  
    .Column3.CONTROLSOURCE  = 'ThisFormSet.lfIsRange()'
    .Column3.Header1.CAPTION = 'Range'
    .Column3.width = 60
    .Column3.VISIBLE = .T.
  
    .Column4.CONTROLSOURCE  = loFormSet.lcTmpPck+'.Style'
    .Column4.Header1.CAPTION = loFormSet.lcStyTtl   
    .Column4.Width = 180
    .Column4.VISIBLE = .T.
    
    .Column5.CONTROLSOURCE  = "THISFormSet.lfGetSz()"
    .Column5.Header1.CAPTION = 'Size'  
    .Column5.VISIBLE = .T.
    
    .Column6.CONTROLSOURCE  = "THISFormSet.lfGetOQty()"
    .Column6.Header1.CAPTION = "O.Qty."
    .Column6.VISIBLE = .T.

    .Column7.CONTROLSOURCE  = "THISFormSet.lfGetCtnQty()"
    .Column7.Header1.CAPTION = "Qty.\Ctn"
    .Column7.VISIBLE = .T.

    .Column8.CONTROLSOURCE  = "THISFormSet.lfGetWghUnt()"
    .Column8.Header1.CAPTION = 'Wgh.\Unt'
    .Column8.VISIBLE = .T.
    
    .Column9.CONTROLSOURCE  = "THISFormSet.lfGetPQty()"
    .Column9.Header1.CAPTION ='P.Qty.'
    .Column9.VISIBLE = .T.
    
    .Column10.CONTROLSOURCE  = "THISFormSet.lfGetPWgh()"
    .Column10.Header1.CAPTION ='P.Wgh.'
    .Column10.VISIBLE = .T.
    
    .readOnly = .t.
  *TTT
ENDWITH 

*:**************************************************************************
*:* Name        : lfGetPackId
*:* Developer : MMT -Mariam Mazhar
*:* Date        : 03/01/2009
*:* Purpose     : get Pack_id
*:***************************************************************************
FUNCTION lfGetPackId
PARAMETERS loFormSet
RETURN EVALUATE(loFormSet.lcTmpPck+ '.Pack_Id')+'-'+EVALUATE(loFormSet.lcTmpPck+ '.cPkColor')+;
       '-'+lfGetGmSz(EVALUATE(loFormSet.lcTmpPck+ '.cPckSize'))+'-'+EVALUATE(loFormSet.lcTmpPck+ '.cPkVersion')
       
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
*:**************************************************************************
*:* Name        : lfIsRange
*:* Developer : MMT -Mariam Mazhar
*:* Date        : 03/01/2009
*:* Purpose     : check if is range pack or not
*:***************************************************************************
FUNCTION lfIsRange
PARAMETERS loFormSet
RETURN IIF(EVALUATE(loFormSet.lcTmpPck+'.llPack'),IIF(EVALUATE(loFormSet.lcTmpPck+'.lRange'),'YES','NO'),'')
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
  *! Name      : lfvAccount
  *! Developer : Mariam MazHar
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

FUNCTION lfActFold
  LPARAMETERS loFormSet,lnActFolder
  lcStores = loFormSet.lcStores 
  lcTmpPck = loFormSet.lcTmpPck
  lcCtnHdr = loFormSet.lcCtnHdr
  IF loFormSet.ActiveMode $ 'S'
    RETURN 
  ENDIF 

  STORE IIF((loFormSet.ActiveMode $ 'AE' OR loFormSet.llNew) AND lnActFolder = 1,.T.,.F.) TO llInstStat,llShipStat

  =lfWinHdrSt(loFormSet)

  DO CASE
    
    CASE lnActFolder = 2
    
    
      IF USED(lcTmpPck)
        SELECT &lcTmpPck
        *! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
        *SET FILTER TO STORE = &lcStores..STORE
        SET FILTER TO STORE = &lcStores..STORE  AND PIKTKT = &lcStores..PIKTKT
        *! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
        GO TOP
      ENDIF

      =lfDtlBrow(loFormSet)

    CASE lnActFolder = 3
    
      IF USED(lcCtnHdr)
        SELECT (lcCtnHdr)
        *! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
        *SET FILTER TO STORE = &lcStores..STORE
        SET FILTER TO STORE = &lcStores..STORE  AND PIKTKT = &lcStores..PIKTKT
        *! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]        
        GO TOP      
      ENDIF
      WITH loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonH
        .RECORDSOURCE = ''
        SELECT (loFormSet.lcCtnHdr)
        .RECORDSOURCE = loFormSet.lcCtnHdr
        *-- Cart. #
       .Column1.CONTROLSOURCE  = loFormSet.lcCtnHdr +'.Cart_No'
       .Column1.Header1.caption = 'Cart.#'       
       .Column1.Header1.ALIGNMENT = 1
       .COLUMNS(1).ALIGNMENT = 1
       .COLUMNS(1).WIDTH = 40
       .Column2.VISIBLE =  .F.

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
       .Column5.Visible =.F.
       .SETALL('ReadOnly',.T.,'COLUMN')
     ENDWITH
     WITH loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonD
       .RECORDSOURCE = ''
       SELECT (loFormSet.lcCtnDtl)
       .RECORDSOURCE = loFormSet.lcCtnDtl
       *-- PAck_ID
       .Column6.VISIBLE =.F.&& IIF(loFormSet.llUsePack, .T., .F.)
 
       *-- Style
       .Column1.CONTROLSOURCE  = loFormSet.lcCtnDtl +'.Style'
       .Column1.Header1.CAPTION = loFormSet.lcStyTtl
       .COLUMNS(1).WIDTH = 120
       .Column1.COLUMNORDER = 2
       .Column2.VISIBLE        = .F. &&IIF(loFormSet.llUseConfg OR loFormSet.llDyelot , .T., .F.)
 
       *-- Size Code.
       .Column3.CONTROLSOURCE  = "THISFormSet.lfGetSzdet()"
       .COLUMNS(3).WIDTH = 40
       .Column3.COLUMNORDER = 4

       *-- Qty
       .Column4.CONTROLSOURCE  = "THISFormSet.lfGetQtyDet()" 
       .COLUMNS(4).WIDTH = 40
       .Column4.Header1.ALIGNMENT = 1
       .COLUMNS(4).ALIGNMENT = 1
       .Column4.COLUMNORDER = 5

       *-- Weight
       .Column5.CONTROLSOURCE  = "THISFormSet.lfGetWghDET()"
       .COLUMNS(5).WIDTH = 90
       .Column5.Header1.ALIGNMENT = 1
       .COLUMNS(5).ALIGNMENT = 1
       .Column5.COLUMNORDER = 6

       .SETALL('ReadOnly',.T.,'COLUMN')
    ENDWITH
    = lfwCtnHdrBrP(loFormSet)

  ENDCASE


  *!*************************************************************
  *! Name      : lfWinHdrSt
  *! Developer : Mariam MazHar
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
  *! Name      : lfwCtnHdrBrP
  *! Developer : Mariam MazHar
  *! Date      : 03/01/2009
  *! Purpose   : refresh the edit rigion for carton header according to
  *!             lcCtnHdr file
  *!*************************************************************
  *! Parameters: loFormSet : FormSet
  *!*************************************************************
FUNCTION lfwCtnHdrBrP
  LPARAMETERS loFormSet
  LOCAL lnAlias
  lnAlias = SELECT(0)
  lcCtnHdr =loFormSet.lcCtnHdr
  SELECT (loFormSet.lcCtnDtl)
  SET KEY TO STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4)
  =SEEK(STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4))
  loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonD.REFRESH
  SELECT (loFormSet.lcCtnHdr)
  
  
  SELECT(lnAlias)

*!*************************************************************
*! Name      : lfGetSz
*! Developer : Mariam MazHar
*! Date      : 03/01/2009
*! Purpose   : Get Size
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfGetSz
PARAMETERS loFormSet
 
lcSizeNo = eval(loFormSet.lcTmpPck +'.cNoSize')
IF !EMPTY(lcSizeNo)
  RETURN Eval(loFormSet.lcTmpPck+'.cSize'+lcSizeNo)
ELSE
  RETURN ''
ENDIF 
*!*************************************************************
*! Name      : lfGetOQty
*! Developer : Mariam MazHar
*! Date      : 03/01/2009
*! Purpose   : Get Qty of a given size
*!*************************************************************
FUNCTION lfGetOQty
PARAMETERS loFormSet
 
lcSizeNo = eval(loFormSet.lcTmpPck +'.cNoSize')
IF !EMPTY(lcSizeNo)
  RETURN Eval(loFormSet.lcTmpPck+'.OQty'+lcSizeNo)
ELSE
  RETURN 0
ENDIF 

*!*************************************************************
*! Name      : lfGetCtnQty
*! Developer : Mariam MazHar
*! Date      : 03/01/2009
*! Purpose   : Get carton Qty of a given size
*!*************************************************************
FUNCTION lfGetCtnQty
PARAMETERS loFormSet
 
lcSizeNo = eval(loFormSet.lcTmpPck +'.cNoSize')
IF !EMPTY(lcSizeNo)
  RETURN Eval(loFormSet.lcTmpPck+'.CtnQty'+lcSizeNo)
ELSE
  RETURN 0
ENDIF 

*!*************************************************************
*! Name      : lfGetWghUnt
*! Developer : Mariam MazHar
*! Date      : 03/01/2009
*! Purpose   : Get unit weight of a given size
*!*************************************************************
FUNCTION lfGetWghUnt
PARAMETERS loFormSet
 
lcSizeNo = eval(loFormSet.lcTmpPck +'.cNoSize')
IF !EMPTY(lcSizeNo)
  RETURN Eval(loFormSet.lcTmpPck+'.Weight'+lcSizeNo)
ELSE
  RETURN 0
ENDIF 

*!*************************************************************
*! Name      : lfGetPQty
*! Developer : Mariam MazHar
*! Date      : 03/01/2009
*! Purpose   : Get packed qty of a given size
*!*************************************************************
FUNCTION lfGetPQty
PARAMETERS loFormSet
 
lcSizeNo = eval(loFormSet.lcTmpPck +'.cNoSize')
IF !EMPTY(lcSizeNo)
  RETURN Eval(loFormSet.lcTmpPck+'.PQty'+lcSizeNo)
ELSE
  RETURN 0
ENDIF 

*!*************************************************************
*! Name      : lfGetPWgh
*! Developer : Mariam MazHar
*! Date      : 03/01/2009
*! Purpose   : Get packed weight of a given size
*!*************************************************************
FUNCTION lfGetPWgh
PARAMETERS loFormSet
 
lcSizeNo = eval(loFormSet.lcTmpPck +'.cNoSize')
IF !EMPTY(lcSizeNo)
  RETURN Eval(loFormSet.lcTmpPck+'.PWgh'+lcSizeNo)
ELSE
  RETURN 0
ENDIF 

*!*************************************************************
*! Name      : lfGetQtyDet
*! Developer : Mariam MazHar
*! Date      : 03/01/2009
*! Purpose   : Get qty of a given size in detail tab
*!*************************************************************
FUNCTION lfGetQtyDet
PARAMETERS loFormSet
lcSizeNo = eval(loFormSet.lcCtnDtl+'.cNoSize')
IF !EMPTY(lcSizeNo)
  RETURN Eval(loFormSet.lcCtnDtl+'.Qty'+lcSizeNo)
ELSE
  RETURN 0
ENDIF 

*!*************************************************************
*! Name      : lfGetWghDET
*! Developer : Mariam MazHar
*! Date      : 03/01/2009
*! Purpose   : Get WeighT of a given size in detail tab
*!*************************************************************
FUNCTION lfGetWghDET
PARAMETERS loFormSet
lcSizeNo = eval(loFormSet.lcCtnDtl+'.cNoSize')
IF !EMPTY(lcSizeNo)
  RETURN Eval(loFormSet.lcCtnDtl+'.Weight'+lcSizeNo)
ELSE
  RETURN 0
ENDIF 

*!*************************************************************
*! Name      : lfGetSzdet 
*! Developer : Mariam MazHar
*! Date      : 03/01/2009
*! Purpose   : Get size desc. of a given size in detail tab
*!*************************************************************
FUNCTION lfGetSzdet 
PARAMETERS loFormSet
lcSizeNo = eval(loFormSet.lcCtnDtl+'.cNoSize')
IF !EMPTY(lcSizeNo)
  RETURN Eval(loFormSet.lcCtnDtl+'.Size'+lcSizeNo)
ELSE
  RETURN ''
ENDIF 

*:**************************************************************************
*:* Name        : lfvSelSto
*:* Developer   : MMTT-Mariam MAzhar
*:* Date        : 03/01/2009
*:* Purpose     : Select stores
*:***************************************************************************
FUNCTION lfvSelSto
PARAMETERS loFormSEt,lcSelTyp
lcStores = loFormSEt.lcStores
PRIVATE lnAlias,lnRecno
lnAlias = SELECT()
SELECT &lcStores
lnRecno = RECNO(lcStores)

DO CASE
  CASE  lcSelTyp = 'S'
    IF EMPTY(PACK_NO)
      REPLACE lSelect WITH .T.  
    ELSE
      REPLACE lSelect WITH .F.  
    ENDIF   

  CASE  lcSelTyp = 'A'
    GO TOP
    REPLACE lSelect WITH .T. FOR EMPTY(PACK_NO)
  
  CASE  lcSelTyp = 'N'
    GO TOP
    REPLACE lSelect  WITH .F. ALL

ENDCASE
IF BETWEEN(lnRecno,1,RECCOUNT(lcStores))
  GOTO (lnRecno) IN (lcStores)
ENDIF  
=lfwStrsBr(loFormSEt)
SELECT (lnAlias)
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
FUNCTION lfOpnTmpl
PARAMETERS loFormset
PRIVATE lcOrder
lcOrder = "'"+ALLTRIM(loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE) +"'"
oAriaApplication.DoProgram("AWRALAUPLJ", lcOrder,.F.,'AL')

*:**************************************************************************
*:* Name        : lfvGenPL
*:* Developer   : MMT -Mariam Mazhar
*:* Date        : 03/01/2009
*:* Purpose     : Generate Packing list
*:***************************************************************************
FUNCTION lfvGenPL
PARAMETERS loFormset
PRIVATE lcCurs,lcI,lnSvRec,lnI,lnCartons,lnJ,lnCrtSum,lcCurrSty
lcStores = loFormset.lcStores
lcErorFil = loFormset.lcErorFil
lcAccount = loFormSet.AriaForm1.kbAccount.keytextbox.VALUE  
IF loFormset.ActiveMode $ 'SV'
  RETURN 
ENDIF  
lcOrder = loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE
=gfSEEK('O'+lcOrder ,'ORDHDR')
IF ORDHDR.STATUS = 'C'
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Order is invoiced, can not generate!')
  RETURN
ENDIF


SELECT &lcStores
lnSvRec = RECNO(lcStores)

LOCATE FOR lSelect 
IF !FOUND()
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No Store selected.')
  RETURN
ENDIF 

lcCurs = gfTempName()
lcPckLin  = loFormset.lcPckLin 
lcCtnHdr = loFormset.lcCtnHdr
lcCtnDtl = loFormset.lcCtnDtl
SELECT TMPL_HDR
=gfSEEK(lcOrder ,'TMPL_HDR')
SELECT &lcPckLin 
lcRelation = SET('RELATION')
lcSkip = SET('SKIP')
lnOrder = ORDER(lcPckLin)
SET RELATION TO
SET ORDER TO (loFormset.lcPakIndxSt)
lnCrtHOrd = ORDER(lcCtnHdr)
SET ORDER TO STORE IN (lcCtnHdr)
lnCrtDOrd  = ORDER(lcCtnDtl)
SET ORDER TO (lcCtnDtl) IN (lcCtnDtl)

DIMENSION laErrArr[1,4]
laErrArr[1,1] = 'cError'
laErrArr[1,2] = 'C'
laErrArr[1,3] = 80
laErrArr[1,4] = 0
=gfCrtTmp(loFormset.lcErorFil,@laErrArr)
llGenrted = .F.    && Set this to .T. if at least one pack is generated
*-- Check if there are errors
=lfChkErrs(loFormset)      
llNoErr = ( RECCOUNT(lcErorFil) = 0 )
llContinue = .T.
IF RECCOUNT(lcErorFil) > 0
  =lfErrDsply(loFormset)
  IF llContinue .AND. TMPL_HDR.LLCKONERR
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Order is locked on errors, can not create P/L for all selected stores')
  ENDIF
ENDIF

IF llContinue .AND. ;
   (!TMPL_HDR.LLCKONERR  .OR. (TMPL_HDR.LLCKONERR .AND. llNoErr))
  SELECT &lcStores
  GO TOP
  SCAN FOR lSelect 
    lnCrtSum = 0         && Variable holds # of cartons generated for the current store
    SELECT &lcPckLin    
    SET FILTER TO 
    GO TOP
    DIMENSION laCarStru[3,4]
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
    WAIT WINDOW NOWAIT 'Generating Cartons for store '+&lcStores..STORE + ' '+IIF(!EMPTY(&lcStores..DIST_CTR),'D.C. ' +&lcStores..DIST_CTR , '')
    SELECT &lcPckLin
    SET FILTER TO 
    *! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
    *SET FILTER TO STORE = &lcStores..STORE AND NOT lRange
    SET FILTER TO STORE = &lcStores..STORE AND NOT lRange AND PIKTKT = &lcStores..PIKTKT
    *! B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
    GO TOP
    
     
    =gfSEEK(lcOrder ,'TMPL_LIN')
    SELECT TMPL_LIN    
    SCAN REST WHILE ORDER+STR(NO_CART,4)+PACK_ID+CPKCOLOR+CPKSIZE+CPKVERSION+STYLE = lcOrder  ;
              FOR TMPL_LIN.TOTQTY > 0
      lnLinpk = 1      
      lcCurrSty = TMPL_LIN.PACK_ID+TMPL_LIN.CPKCOLOR+TMPL_LIN.CPKSIZE+TMPL_LIN.CPKVERSION+TMPL_LIN.STYLE                
      IF SEEK(lcCurrSty,lcPckLin)      
        SELECT &lcPckLin
        
        SCAN REST WHILE PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE+STR(nOrdLineNo,6) = lcCurrSty 
          *-- laQty : This array holds the availabel qty for the current line in lcPckLin file
          *--         from the equation CEILING(&lcPckLin..AvlQty1/TMPL_LIN.QTY1) we get the # of cartons
          *--         needed for the qty laQty[1,1] , we get the max of this equation for sizes 1,...,8 
          *-          save this value in the lnCartons variable
          DIMENSION laQty[8,3]
          laQty = 0
          lnCartons = 0    
          FOR lnI = 1 TO 8        
            lcI = STR(lnI,1)
            IF &lcPckLin..cNoSize <> lcI
               LOOP 
            ENDIF 
            laQty[lnI,1] = &lcPckLin..AvlQty&lcI
            laQty[lnI,2] = TMPL_LIN.QTY&lcI
            laQty[lnI,3] = IIF( TMPL_LIN.QTY&lcI>0 , &lcPckLin..AvlQty&lcI/TMPL_LIN.QTY&lcI , 0 )
            IF laQty[lnI,3] > lnCartons
              lnCartons = CEILING(laQty[lnI,3])    && Get max number of cartons to hold all pieces
            ENDIF
          ENDFOR
          SELECT &lcPckLin
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
                  
          REPLACE PQTY&lcSz.    WITH OQTY&lcSz.,;
                  PTOTQTY WITH PQTY1+PQTY2+PQTY3+PQTY4+PQTY5+PQTY6+PQTY7+PQTY8,;
                  PWGH&lcSz.   WITH PQTY&lcSz.* WEIGHT&lcSz.,;
                  PTOTWGH WITH PWGH1+PWGH2+PWGH3+PWGH4+PWGH5+PWGH6+PWGH7+PWGH8           
                  
          =gfSEEK(TMPL_LIN.STYLE,'STYLE')
          =gfSEEK('S'+STYLE.SCALE,'SCALE')
          =gfSEEK(TMPL_LIN.NCARTON,lcCurs)
          lnPackNo = 0
          IF !EMPTY(&lcPckLin..PACK_ID)            
            lcSvOrd = ORDER('SPCK_LIN')
            SELECT SPCK_LIN   
            =gfSetOrder('SPCK_LINVR')
            SELECT &lcPckLin
            IF gfSEEK('P'+lcAccount+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE,'SPCK_LIN') OR ;
               gfSEEK('P*****'+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE,'SPCK_LIN')
              lnPackNo = PTOTQTY/SPCK_LIN.TOTQTY
            ENDIF
            SELECT SPCK_LIN
            =gfSetOrder(lcSvOrd)
            SELECT &lcPckLin
          ENDIF
          
          FOR lnCrtn = 1 TO lnCartons
              
            IF MIN(laQty[VAL(lcSz),1],TMPL_LIN.QTY&lcSz.) = 0
              LOOP 
            ENDIF     
            
            SELECT (lcCtnDtl)
            APPEND BLANK
            REPLACE cNOSize WITH &lcPckLin..cNoSize
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
		  REPLACE PIKTKT WITH &lcStores..PIKTKT
  		  *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
                    
          REPLACE CART_NO WITH lnCrtn+&lcCurs..nFrom,;
                    STORE      WITH &lcStores..STORE,;
                    nOrdLineNo WITH &lcPckLin..nOrdLineNo,;
                    STYLE      WITH TMPL_LIN.STYLE,;
                    SZCNT      WITH SCALE.CNT,;
                    SIZE&lcSz.     WITH SCALE.SZ&lcSz.
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
                    
             REPLACE QTY&lcSz.       WITH IIF(laQty[VAL(lcSz),1] > 0 , MIN(laQty[VAL(lcSz),1],TMPL_LIN.QTY&lcSz.) , 0 ),;
                    TOTQTY     WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8                    
                    
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
             REPLACE WEIGHT&lcSz.    WITH IIF(Qty&lcSz. > 0 , QTY&lcSz. *TMPL_LIN.WEIGHT , 0),;
                     TOTWEIGHT  WITH WEIGHT1+WEIGHT2+WEIGHT3+WEIGHT4+;
                              WEIGHT5+WEIGHT6+WEIGHT7+WEIGHT8                              
                              
            REPLACE BR1        WITH !EMPTY(QTY1),;
                    BR2        WITH !EMPTY(QTY2),;
                    BR3        WITH !EMPTY(QTY3),;
                    BR4        WITH !EMPTY(QTY4),;
                    BR5        WITH !EMPTY(QTY5),;
                    BR6        WITH !EMPTY(QTY6),;
                    BR7        WITH !EMPTY(QTY7),;
                    BR8        WITH !EMPTY(QTY8)                
                    
            *-- Update here Pack_id,pksize,packcolor,pkversion ,nPackNo       
            IF !EMPTY(&lcPckLin..PACK_ID)
              REPLACE PACK_ID    WITH SPCK_LIN.PACK_ID,;
                      cPkColor   WITH SPCK_LIN.cPkColor,;
                      cPCkSize   WITH SPCK_LIN.cPckSize,;
                      cPKVersion WITH SPCK_LIN.cPkVersion,;
                      nPackNO    WITH lnPackNo  
            ENDIF
    
            *- Decrease availabel qty to update next cartons ( specially last one )
            FOR lnJ = 1 TO 8                  
              lcJ = STR(lnJ,1)
              laQty[lnJ,1] = MAX( laQty[lnJ,1] - QTY&lcJ , 0 )
            ENDFOR                
            *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
*!*	            IF !SEEK(&lcStores..STORE+STR(lnCrtn+&lcCurs..nFrom,4),lcCtnHdr)
*!*	              INSERT INTO (lcCtnHdr) (Cart_No,Pal_No,Empty,STORE);
*!*	                                  VALUES (lnCrtn+&lcCurs..nFrom,0,;
*!*	                                          'N',&lcStores..STORE)
            IF !SEEK(&lcStores..STORE+&lcStores..PIKTKT+STR(lnCrtn+&lcCurs..nFrom,4),lcCtnHdr)
              INSERT INTO (lcCtnHdr) (Cart_No,Pal_No,Empty,STORE,PIKTKT);
                                  VALUES (lnCrtn+&lcCurs..nFrom,0,;
                                          'N',&lcStores..STORE,&lcStores..PIKTKT)
			*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]                                          
              lnCrtSum = lnCrtSum + 1                              
            ENDIF
            REPLACE &lcCtnHdr..TotPcs WITH &lcCtnHdr..TotPcs + &lcCtnDtl..TotQty,;
                    &lcCtnHdr..TotWgh WITH &lcCtnHdr..TotWgh + &lcCtnDtl..TOTWEIGHT
    
            loFormset.lnPackWgh = loFormset.lnPackWgh + &lcCtnDtl..TOTWEIGHT
                    
            SELECT &lcStores
            REPLACE TOTPQty WITH TOTPQty + &lcCtnDtl..TotQty ,;
                    WEIGHT  WITH WEIGHT  + &lcCtnDtl..TOTWEIGHT
          ENDFOR
        ENDSCAN
        lnLinpk = lnLinpk +  1      
      ENDIF    
    ENDSCAN
    =lfUpdRngPk(loFormset)
    SELECT &lcStores
    REPLACE CARTONS WITH lnCrtSum ,;
            PACK_NO WITH "######" ,;
            lSelect WITH .F.
    llGenrted = .T.
   ENDSCAN 
ENDIF 
IF llGenrted
  =lfwStrsBr(loFormSet)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Packing Lists are generated.')
  LOCATE FOR TOTORD # TOTPQTY AND !EMPTY(PACK_NO)
  IF FOUND()
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Total Packed Qty not equal to Total OrdQty for some stores.')  
  ENDIF
ENDIF
SELECT &lcPckLin
SET RELATION TO &lcRelation
SET SKIP TO &lcSkip
SET ORDER TO (lnOrder)

SET ORDER TO &lnCrtHOrd IN (lcCtnHdr)

IF USED(lcCurs)
  USE IN &lcCurs 
ENDIF

IF BETWEEN(lnSvRec,1,RECCOUNT(lcStores))
  GOTO (lnSvRec) IN (lcStores)
ENDIF
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
FUNCTION lfChkErrs
PARAMETERS loFormset
PRIVATE lnCnt,lnCartons,lnAlias,lcSvOrder,lcSvOrder2,lnI,lcI,lcCurrSty
lnAlias = SELECT()
lcOrder = loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE
lcPckLin = loFormset.lcPckLin
lcStores = loFormset.lcStores
lcSvOrder  = ORDER(lcPckLin)
lcSvOrder2 = ORDER('TMPL_LIN')
SET ORDER TO STORE IN (lcPckLin)
SELECT TMPL_LIN
=gfSetOrder('TMPL_LINS')
lcErorFil = loFormset.lcErorFil
SELECT &lcPckLin
SET FILTER TO 
GO TOP

SELECT &lcStores
GO TOP

SCAN FOR lSelect
  lcMissed = ' '
  llAdd = .F.
  SELECT (lcPckLin)
  IF SEEK(&lcStores..STORE,lcPckLin)
    SCAN REST WHILE STORE = &lcStores..STORE
      IF lRange
        LOOP
      ENDIF 
      *-- Check if style is missing in TMPL_LIN file
      lcCurrSty = lcOrder +PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE
      IF !gfSEEK(lcOrder +PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE,'TMPL_LIN') .AND. !(STYLE $ lcMissed)
      
        INSERT INTO (lcErorFil) VALUES (&lcStores..STORE+'      | '+&lcPckLin..STYLE+'  | MISSED')
        lcMissed = lcMissed + &lcPckLin..STYLE + '|'
        llAdd = .T.
        LOOP
      ENDIF
      
      *-- If style is in TMPL_LIN check that open qty is divisible by qty Ctn.
      *-- validate each size qty/ctn separately.
      *-- Apply non division only if AvlQty is greater than template qty , reverse case means 
      *-- that all qty will be placed in one carton      
      SELECT TMPL_LIN
      SCAN REST WHILE ORDER+PACK_ID+CPKCOLOR+CPKSIZE+CPKVERSION+STYLE = lcCurrSty
        FOR lnI = 1 TO 8
          lcI = STR(lnI,1)
          IF TMPL_LIN.QTY&lcI > 0 .AND. ;
            &lcPckLin..AvlQty&lcI > TMPL_LIN.QTY&lcI .AND. ;
            MOD( &lcPckLin..AvlQty&lcI , TMPL_LIN.QTY&lcI ) > 0
            INSERT INTO (lcErorFil) VALUES (&lcStores..STORE+'    | '   +;
                                                &lcPckLin..STYLE+'|'    +;
                                                &lcPckLin..CSIZE&lcI+'|  '+;
                                                ALLT(STR(TMPL_LIN.NCARTON))+;
                                                '  | Division '+;
                                                'O.Qty='+LTRIM(STR(&lcPckLin..AvlQty&lcI))+;
                                                ', Qty/Ctn='+LTRIM(STR(TMPL_LIN.QTY&lcI)))
            llAdd = .T.  
          ENDIF
        ENDFOR
      ENDSCAN
      
    ENDSCAN  
  ENDIF
  
  *-- Add a separator after each store
  IF llAdd
    INSERT INTO (lcErorFil) VALUES (REPL('=',80))
  ENDIF
ENDSCAN

SET ORDER TO &lcSvOrder  IN (lcPckLin)
SELECT TMPL_LIN
=gfSetOrder(lcSvOrder2)
SELECT (lnAlias )
*-- end of lfChkErrs.
*:**************************************************************************
*:* Name        : lfErrDsply
*:* Developer   : MMT -Mariam Mazhar
*:* Date        : 03/01/2009
*:* Purpose     : Display Error Screen
*:***************************************************************************
FUNCTION lfErrDsply
PARAMETERS loFormset
lcErorFil =loFormset.lcErorFil
PRIVATE lcErroSrc,lcNote,lcProcStat
lcErroSrc = (oAriaApplication.WorkDir+'Error.TXT')
lcNote     = IIF(TMPL_HDR.LLCKONERR,'Order is locked on errors','')
llProcStat = !TMPL_HDR.LLCKONERR
*: B609154,2 MMT 03/01/2010 Fix bug of error when run program on SAAS[Start]
IF oAriaApplication.MULTIINST 
  *: B609229,1 MMT 04/29/2010 Fix bug of error when run program on SAAS[Start]  
 *DO FORM ("X:\aria4xp\Screens\"+'AL\ALERRORS.SCX') WITH loFormset,lcNote,llProcStat ,lcErroSrc 
 lcParmLst = "loFormset,lcNote,llProcStat ,lcErroSrc"
 =gfCallForm('ALERRORS','AL',lcParmLst)
 *: B609229,1 MMT 04/29/2010 Fix bug of error when run program on SAAS[End] 
ELSE
*: B609154,2 MMT 03/01/2010 Fix bug of error when run program on SAAS[End]
DO FORM (oAriaApplication.ScreenHome+'AL\ALERRORS.scx') WITH loFormset,lcNote,llProcStat ,lcErroSrc 
*: B609154,2 MMT 03/01/2010 Fix bug of error when run program on SAAS[Start]
ENDIF 
*: B609154,2 MMT 03/01/2010 Fix bug of error when run program on SAAS[End]
COPY TO (lcErroSrc) SDF   
USE IN (lcErorFil)
*:**************************************************************************
*:* Name        : lfvErrSrc
*:* Developer : MMT -MAriam MAzhar
*:* Date        : 03/01/2009
*:* Purpose     : Change the error log text file name
*:***************************************************************************
FUNCTION lfvErrSrc
PARAMETERS loBranchForm
PRIVATE lcRet,lcSavDef,lcFullPath
lcFullPath = SET('FULLPATH')
lcSavDef = FULLPATH('')
SET DEFAULT  TO (oAriaApplication.WorkDir)
lcRet = GETFILE('TXT','Select File To Save.')
IF !EMPTY(lcRet)
  lcErroSrc = lcRet
ENDIF
loBranchForm.ariaForm1.txtFlPth.Value = lcErroSrc
SET DEFAULT  TO &lcSavDef
SET FULLPATH &lcFullPath

*:**************************************************************************
*:* Name        : lfCtnOrg
*:* Developer : MMT -MAriam MAzhar
*:* Date        : 03/01/2009
*:* Purpose     : get Carton info. from Template lines 
*:***************************************************************************
FUNCTION lfCtnOrg
PARAMETERS loFormset

lcPckLin = loFormset.lcPckLin 
lcStores = loFormset.lcStores
PRIVATE lnCnt,lnCartons,lnAlias,lcCurrSty,lcOldSty,laAvlQty,lcSto
lnAlias = SELECT()
lcOrder = loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE
STORE ' ' TO lcCurrSty,lcOldSty
DIMENSION laAvlQty[9]


SELECT TMPL_LIN
=gfSEEK(lcOrder ,'TMPL_LIN')
SCAN REST WHILE ORDER+STR(NO_CART,4)+PACK_ID+CPKCOLOR+CPKSIZE+CPKVERSION+STYLE = lcOrder ;
          FOR TMPL_LIN.TOTQTY > 0           
          
  lcCurrSty = PACK_ID+CPKCOLOR+CPKSIZE+CPKVERSION+STYLE
  lcCurrPck = PACK_ID+CPKCOLOR+CPKSIZE+CPKVERSION
  IF lcOldSty # lcCurrSty
    laAvlQty = 0
    lcOldSty = lcCurrSty  
    lcSto = &lcStores..STORE
    SELECT &lcPckLin
    =SEEK(lcCurrSty,lcPckLin)
    lcPckOrSty = IIF(&lcPckLin..LRANGE,lcCurrPck,lcCurrSty)
    lnCntSz =&lcPckLin..SzCnt
    FOR lnA = 1 TO lnCntSz 
      =SEEK(lcCurrSty,lcPckLin)
      DIMENSION laSzSum[1]
      STORE 0 TO laSzSum
      lcA = STR(lnA,1)
      SELECT SUM(AVLQTY&lcA.) FROM &lcPckLin WHERE PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE = lcPckOrSty .AND. STORE = lcSto ;        
      AND cNoSize = lcA INTO ARRAY laSzSum
      IF !ISNULL(laSzSum[1])
        laAvlQty[lnA] = laSzSum[1]
        laAvlQty[9] = laAvlQty[9] +  laSzSum[1]
      ENDIF   
    ENDFOR  
   * SELECT SUM(AVLQTY1),SUM(AVLQTY2),SUM(AVLQTY3),SUM(AVLQTY4),;
           SUM(AVLQTY5),SUM(AVLQTY6),SUM(AVLQTY7),SUM(AVLQTY8),SUM(AvlTotQty) ;
    FROM &lcPckLin ;
    WHERE PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE = lcPckOrSty .AND. STORE = lcSto ;        
    INTO ARRAY laAvlQty
  ENDIF
  IF laAvlQty[9] > 0
    lnPSum = IIF(TMPL_LIN.QTY1>0,laAvlQty[1],0) + IIF(TMPL_LIN.QTY2>0,laAvlQty[2],0) + ;
             IIF(TMPL_LIN.QTY3>0,laAvlQty[3],0) + IIF(TMPL_LIN.QTY4>0,laAvlQty[4],0) + ;
             IIF(TMPL_LIN.QTY5>0,laAvlQty[5],0) + IIF(TMPL_LIN.QTY6>0,laAvlQty[6],0) + ;
             IIF(TMPL_LIN.QTY7>0,laAvlQty[7],0) + IIF(TMPL_LIN.QTY8>0,laAvlQty[8],0)
    lnCartons = CEILING(lnPSum/TMPL_LIN.TOTQTY)
    IF &lcPckLin..LRANGE
      lnCartons = CEILING(laAvlQty[9]/TMPL_LIN.TOTQTY)
    ENDIF
    IF !SEEK(TMPL_LIN.NCARTON,lcCurs)
      INSERT INTO (lcCurs) (NCARTON) VALUES (TMPL_LIN.NCARTON)
    ENDIF
    SELECT &lcCurs
    REPLACE NCOUNT WITH MAX( NCOUNT , lnCartons )  && Always put the maximum needed cartons 
  ENDIF
ENDSCAN 
SELECT (lcCurs)
SET ORDER TO 
GO TOP
lnCnt = 0
*- NFROM : this field is used to calculate starting # for each carton in tamplate to be filled 
*-         in actual cartons
SCAN
  REPLACE NFROM WITH lnCnt
  lnCnt = lnCnt + NCOUNT
ENDSCAN
SET ORDER TO TAG (lcCurs) IN (lcCurs)
LOCATE
SELECT (lnAlias )

*:**************************************************************************
*:* Name        : lfUpdRngPk
*:* Developer   : MMT MAriam MAzhar
*:* Date        : 03/01/2009
*:* Purpose     : Update Range packs in carton detai file
*:***************************************************************************
FUNCTION lfUpdRngPk
PARAMETERS loFormset
lcTmpUpLn = loFormSet.lcTmpUpLn 
lcCtnDtl = loFormSet.lcCtnDtl
lcStores = loFormSet.lcStores
IF USED(lcTmpUpLn)
  USE IN (lcTmpUpLn)
ENDIF
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
*DIMENSION laUplnStr[18,4]
DIMENSION laUplnStr[19,4]
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
SELECT (lcCtnDtl)
GO BOTTOM
lnLastCrt = &lcCtnDtl..CART_NO
SELECT TMPL_LIN
=gfSetOrder('TMPL_LINS')
lnCartNO  = 0
lcOrder = loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE
SELECT &lcPckLin
SET FILTER TO
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
*SET FILTER TO STORE = &lcStores..STORE AND lRange
SET FILTER TO STORE = &lcStores..STORE AND lRange AND PIKTKT  = &lcStores..PIKTKT 
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
GO TOP
lnRemQty = 0  
lcPack_Id = ""
lnPckedQty = 0  && initialize "lnPckedQty" variable
SCAN FOR UOTOTQTY > 0
  IF lcPack_Id <> PACK_ID
    IF !EMPTY(lcPack_Id)
      lnCartNO = 0
    ENDIF
    lcPack_Id = PACK_ID
    IF gfSEEK(lcOrder+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION,'TMPL_LIN')
      PRIVATE lnSz
      FOR lnSz = 1 TO 8
        IF EVAL('TMPL_LIN.QTY'+STR(lnSz,1)) > 0
          lnPckedQty = EVAL('TMPL_LIN.QTY'+STR(lnSz,1))
          EXIT
        ENDIF
      ENDFOR
      lnRemQty   = 0
    ENDIF
  ENDIF
  lnI = 1
  DIME laQty[8]
  STORE 0 TO laQty
  =gfSEEK(Style,'STYLE') AND gfSEEK('S'+Style.Scale,'SCALE')
  lnReqQty = IIF(lnRemQty   =0,lnPckedQty ,lnRemQty)
  lnStillReq = lnReqQty 
  DO WHILE lnI <= Scale.cnt .AND. UOTOTQTY > 0
    lcI = STR(lnI,1)
    laQty[lnI] = MIN(UpOqty&lcI,lnReqQty)   
    lnReqQty   = MAX(lnReqQty - laQty[lnI],0)
    lnStillReq = lnReqQty
    REPLACE UOTOTQTY   WITH UOTOTQTY   - laQty[lnI],;
            UpOQTY&lcI WITH UpOQTY&lcI - laQty[lnI]
    IF lnReqQty = 0
      lnCartNO   = lnCartNO + 1
      *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
*!*	      INSERT INTO (lcTmpUpLn) (Style,nOrdLineNo,Store,PACK_ID,CPKCOLOR,CPCKSIZE,CPKVERSION,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,CartNo,TotQty);
*!*	                   VALUES  (&lcPckLin..Style,&lcPckLin..nOrdLineNo,&lcPckLin..Store,&lcPckLin..PACK_ID,&lcPckLin..CPKCOLOR,;
*!*	                           &lcPckLin..CPCKSIZE,&lcPckLin..CPKVERSION,laQty[1],laQty[2],laQty[3],;
*!*	                            laQty[4],laQty[5],laQty[6],laQty[7],laQty[8],lnCartNO,laQty[1]+laQty[2]+laQty[3]+;
*!*	                            laQty[4]+laQty[5]+laQty[6]+laQty[7]+laQty[8],lcI)
      INSERT INTO (lcTmpUpLn) (Style,nOrdLineNo,Store,PACK_ID,CPKCOLOR,CPCKSIZE,CPKVERSION,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,CartNo,TotQty,cNoSize,PIKTKT);
                   VALUES  (&lcPckLin..Style,&lcPckLin..nOrdLineNo,&lcPckLin..Store,&lcPckLin..PACK_ID,&lcPckLin..CPKCOLOR,;
                           &lcPckLin..CPCKSIZE,&lcPckLin..CPKVERSION,laQty[1],laQty[2],laQty[3],;
                            laQty[4],laQty[5],laQty[6],laQty[7],laQty[8],lnCartNO,laQty[1]+laQty[2]+laQty[3]+;
                            laQty[4]+laQty[5]+laQty[6]+laQty[7]+laQty[8],lcI,&lcPckLin..PIKTKT)
      *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
      lnReqQty = lnPckedQty 
      lnRemQty = 0
      STORE 0 TO laQty
      IF UpOqty&lcI = 0
        lnI = lnI + 1
      ENDIF
    ELSE
      lnI = lnI + 1
    ENDIF
  ENDDO
  IF lnStillReq <> 0
    lnCartNO   = lnCartNO + 1
    *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
*!*	    INSERT INTO (lcTmpUpLn) (Style,nOrdLineNo,Store,PACK_ID,CPKCOLOR,CPCKSIZE,CPKVERSION,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,CartNo,TotQty);
*!*	                VALUES  (&lcPckLin..Style,&lcPckLin..nOrdLineNo,&lcPckLin..Store,&lcPckLin..PACK_ID,&lcPckLin..CPKCOLOR,;
*!*	                           &lcPckLin..CPCKSIZE,&lcPckLin..CPKVERSION,laQty[1],laQty[2],laQty[3],;
*!*	                            laQty[4],laQty[5],laQty[6],laQty[7],laQty[8],lnCartNO,laQty[1]+laQty[2]+laQty[3]+;
*!*	                            laQty[4]+laQty[5]+laQty[6]+laQty[7]+laQty[8])
    INSERT INTO (lcTmpUpLn) (Style,nOrdLineNo,Store,PACK_ID,CPKCOLOR,CPCKSIZE,CPKVERSION,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,CartNo,TotQty,PIKTKT);
                VALUES  (&lcPckLin..Style,&lcPckLin..nOrdLineNo,&lcPckLin..Store,&lcPckLin..PACK_ID,&lcPckLin..CPKCOLOR,;
                           &lcPckLin..CPCKSIZE,&lcPckLin..CPKVERSION,laQty[1],laQty[2],laQty[3],;
                            laQty[4],laQty[5],laQty[6],laQty[7],laQty[8],lnCartNO,laQty[1]+laQty[2]+laQty[3]+;
                            laQty[4]+laQty[5]+laQty[6]+laQty[7]+laQty[8],&lcPckLin..PIKTKT)
    *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
    lnCartNO   = lnCartNO - 1
    lnRemQty   = lnStillReq
  ENDIF
ENDSCAN

=lfUpdCtn(loFormset)
SELECT TMPL_LIN
=gfSetOrder('TMPL_LIN')

*:**************************************************************************
*:* Name        : lfUpdCtn
*:* Developer   : Mariam Mazhar
*:* Date        : 03/01/2009
*:* Purpose     : Update Range packs in carton detai file
*:***************************************************************************
FUNCTION lfUpdCtn
PARAMETERS loFormset

lcTmpUpLn = loFormset.lcTmpUpLn
lcPckLin = loFormset.lcPckLin
lcCtnDtl = loFormset.lcCtnDtl
lcStores = loFormset.lcStores
lcCtnHdr = loFormset.lcCtnHdr
lcOrder = loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE
SELECT (lcTmpUpLn)
SCAN
  lcCurrSty = PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE      
  =SEEK(lcCurrSty,lcPckLin)      
  SELECT &lcPckLin
  REPLACE PQTY1   WITH OQTY1,;
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

   =gfSEEK(&lcTmpUpLn..STYLE,'STYLE') AND gfSEEK('S'+STYLE.SCALE,'SCALE')
   =gfSEEK(lcOrder +PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION,'TMPL_LIN')
   =SEEK(TMPL_LIN.NCARTON,lcCurs) 
   lnLastCrt = &lcCurs..nFrom
  
   SELECT (lcCtnDtl)
   APPEND BLANK
   *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]   
   REPLACE PIKTKT WITH &lcTmpUpLn..PIKTKT    
   *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[end]
   REPLACE cNoSize WITH   &lcTmpUpLn..cNoSize
   REPLACE CART_NO    WITH &lcTmpUpLn..CartNO+lnLastCrt ,;
           STORE      WITH &lcTmpUpLn..STORE,;
           nOrdLineNo WITH &lcTmpUpLn..nOrdLineNo,;
           STYLE      WITH &lcTmpUpLn..STYLE,;
           SZCNT      WITH SCALE.CNT,;
           SIZE1      WITH SCALE.SZ1,;
           SIZE2      WITH SCALE.SZ2,;
           SIZE3      WITH SCALE.SZ3,;
           SIZE4      WITH SCALE.SZ4,;
           SIZE5      WITH SCALE.SZ5,;
           SIZE6      WITH SCALE.SZ6,;
           SIZE7      WITH SCALE.SZ7,;
           SIZE8      WITH SCALE.SZ8
   REPLACE QTY1       WITH &lcTmpUpLn..Qty1,;
           QTY2       WITH &lcTmpUpLn..Qty2,;
           QTY3       WITH &lcTmpUpLn..Qty3,;
           QTY4       WITH &lcTmpUpLn..Qty4,;
           QTY5       WITH &lcTmpUpLn..Qty5,;
           QTY6       WITH &lcTmpUpLn..Qty6,;
           QTY7       WITH &lcTmpUpLn..Qty7,;
           QTY8       WITH &lcTmpUpLn..Qty8,;
           TOTQTY     WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
  
   REPLACE WEIGHT1    WITH IIF(Qty1 > 0 , QTY1*&lcPckLin..WEIGHT1 , 0 ),;
           WEIGHT2    WITH IIF(Qty2 > 0 , QTY2*&lcPckLin..WEIGHT2 , 0 ),;
           WEIGHT3    WITH IIF(Qty3 > 0 , QTY3*&lcPckLin..WEIGHT3 , 0 ),;
           WEIGHT4    WITH IIF(Qty4 > 0 , QTY4*&lcPckLin..WEIGHT4 , 0 ),;
           WEIGHT5    WITH IIF(Qty5 > 0 , QTY5*&lcPckLin..WEIGHT5 , 0 ),;
           WEIGHT6    WITH IIF(Qty6 > 0 , QTY6*&lcPckLin..WEIGHT6 , 0 ),;
           WEIGHT7    WITH IIF(Qty7 > 0 , QTY7*&lcPckLin..WEIGHT7 , 0 ),;
           WEIGHT8    WITH IIF(Qty8 > 0 , QTY8*&lcPckLin..WEIGHT8 , 0 ),;
           TOTWEIGHT  WITH WEIGHT1+WEIGHT2+WEIGHT3+WEIGHT4+WEIGHT5+WEIGHT6+WEIGHT7+WEIGHT8

            
   REPLACE BR1        WITH !EMPTY(QTY1),;
           BR2        WITH !EMPTY(QTY2),;
           BR3        WITH !EMPTY(QTY3),;
           BR4        WITH !EMPTY(QTY4),;
           BR5        WITH !EMPTY(QTY5),;
           BR6        WITH !EMPTY(QTY6),;
           BR7        WITH !EMPTY(QTY7),;
           BR8        WITH !EMPTY(QTY8)                
                   
   *-- Update here Pack_id,pksize,packcolor,pkversion ,nPackNo       
   REPLACE PACK_ID    WITH &lcTmpUpLn..PACK_ID,;
           cPkColor   WITH &lcTmpUpLn..cPkColor,;
           cPCkSize   WITH &lcTmpUpLn..cPckSize,;
           cPKVersion WITH &lcTmpUpLn..cPkVersion,;
           nPackNO    WITH 0  
   *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]   
*!*	   IF !SEEK(&lcStores..STORE+STR(&lcTmpUpLn..CartNO+lnLastCrt,4),lcCtnHdr)
*!*	     INSERT INTO (lcCtnHdr) (Cart_No,Pal_No,Empty,STORE);
*!*	                     VALUES (&lcTmpUpLn..CartNO+lnLastCrt,0,'N',&lcStores..STORE)          
   IF !SEEK(&lcStores..STORE+&lcStores..PIKTKT+STR(&lcTmpUpLn..CartNO+lnLastCrt,4),lcCtnHdr)
     INSERT INTO (lcCtnHdr) (Cart_No,Pal_No,Empty,STORE,PIKTKT);
                     VALUES (&lcTmpUpLn..CartNO+lnLastCrt,0,'N',&lcStores..STORE,&lcStores..PIKTKT)          
   *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]                     
     lnCrtSum = lnCrtSum + 1                              
   ENDIF
   REPLACE &lcCtnHdr..TotPcs WITH &lcCtnHdr..TotPcs + &lcCtnDtl..TotQty,;
           &lcCtnHdr..TotWgh WITH &lcCtnHdr..TotWgh + &lcCtnDtl..TOTWEIGHT
    
   loFormset.lnPackWgh = loFormset.lnPackWgh + &lcCtnDtl..TOTWEIGHT
                    
   SELECT &lcStores
   REPLACE TOTPQty WITH TOTPQty + &lcCtnDtl..TotQty ,;
           WEIGHT  WITH WEIGHT  + &lcCtnDtl..TOTWEIGHT
ENDSCAN



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

PROCEDURE lfSavScrPack
PARAMETERS loFormSet


lcOrderNo = loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE  
lcStores = loFormSet.lcStores
=gfSeek('O'+lcOrderNo ,'Ordhdr','Ordhdr')
PRIVATE laPcks,lnI
IF ORDHDR.STATUS = 'C'
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Order is invoiced, can not save!')
  RETURN .F.
ENDIF
SELECT &lcStores
GO TOP
lnI = 0
SCAN FOR PACK_NO = '######'
  STORE 0 TO m.Tot_Wght, m.Tot_Cart, m.Tot_Pcs 
  STORE .F. TO m.LStandCtn
  STORE '' TO m.CToStorCn
  WAIT WINDOW NOWAIT 'Generating P/L for store '+&lcStores..STORE + ' D.C '+ &lcStores..DIST_CTR
*!*    laData[1] = &lcStores..PACK_NO
*!*    laData[3] = &lcStores..PIKTKT
*!*    laData[5] = &lcStores..STORE
  lcPackNum = &lcStores..PACK_NO
  DO lfSavePack WITH loFormSet
  IF !EMPTY(lcPackNum)
    lnI = lnI + 1
    DIMENSION laPcks[lnI]
    laPcks[lnI] = lcPackNum 
  ENDIF
ENDSCAN
WAIT WINDOW NOWAIT ''
IF lnI>0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Packing lists from '+laPcks[1]+' to '+laPcks[lnI]+' are generated.')
  RETURN .T.  
ELSE
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No packing lists generated, cannot save.')
  RETURN .F.
ENDIF

*:**************************************************************************
*:* Name        : lfSavePack
*:* Developer : MMT -MAriaM Mazhar
*:* Date        : 03/01/2009
*:* Purpose     : Save pack for the current store
*:***************************************************************************
FUNCTION lfSavePack
PARAMETERS loFormSet
lcOrderNo = loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE  
lcAccount = loFormSet.AriaForm1.kbAccount.keytextbox.VALUE 
lcStores = loFormSet.lcStores
lcCtnHdr= loFormSet.lcCtnHdr
*!*  lcStores = loFormSet.lcStores
lcCtnDtl= loFormSet.lcCtnDtl
PRIVATE llReturn,lnCurAlias,lcPckHdTag,lnCount,lnBookQty,lnBookAmt,;
        lnOpenQty,lnOpenAmt,lnCount,lnI,llStyDyOpn,lnCurRecNo,lcSvOrd
lcPcklin = loFormSet.lcPcklin
llStyDyOpn = .F.
STORE 0 To lnBookQty,lnBookAmt,lnRelCho,lnOpenQty,lnOpenAmt
lnCount = 0
lnCurAlias = SELECT(0)
lcDelStat = SET("DELETED")
PRIVATE lnBookDiff , lnQtyDiff , lnBkAmtDif , lnQyAmtDif , lnOrgBook , lnOrgQty
STORE 0 TO lnBookDiff , lnQtyDiff , lnBkAmtDif , lnQyAmtDif, lnOrgBook , lnOrgQty

SELECT Pack_lin
=gfSetOrder('PackStyle')
SELECT (lcPcklin)
SET RELATION TO 
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]   
*SET FILTER TO STORE =&lcStores..STORE
SET FILTER TO STORE =&lcStores..STORE AND PIKTKT =&lcStores..PIKTKT 
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]   
GO TOP

SELECT (lcCtnDtl)
lcCtDtRel = SET("RELATION")
SET RELATION TO
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]   
*SET FILTER TO STORE = &lcStores..STORE
SET FILTER TO STORE = &lcStores..STORE AND PIKTKT =&lcStores..PIKTKT 
*B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]   
GO TOP

SELECT (lcCtnDtl)
SET FILTER TO STORE = &lcStores..STORE
GO TOP
IF EOF() OR BOF() OR DELETED()
  *--No Packing list lines were applied. Cannot proceed!
  *-- <OK>
  llNoThing = gfModalGen("INM44035B00000","Dialog")  
  RETURN .F.
  llReturn = .F.
ELSE

  *---------------------------------------------------
  *-------- Check for availability of picking --------
  *-- IF user want to update pick quantity and does not have access to force allocation
  
  IF  loFormset.llUpdtPkTk
    *-- Open StyDye file before checking in it.
    =gfOpenTable(oAriaApplication.DataDir+"StyDye",oAriaApplication.DataDir+'StyDye')
    *-- if no Sufficient quantity.
    IF lfNoSuffic(loFormSet)
      RETURN
    ENDIF
  ENDIF

  *============================================================
  *This part for updating allocated quantities in style,stydye files
  *and picked quantities in ordline file if the created pack list is
  *made by piktkt
  IF !EMPTY(&lcStores..PIKTKT)
    SELECT(lcPckLin)
    lcTag = ORDER(lcPckLin)
    SET ORDER TO NoPacked
    IF SEEK('Y',lcPckLin)
      *-- "All unselected lines from the picking ticket will be released."
      *-- <Release> <Resume packing> <Save as is>
      IF lnRelCho = 0      && uncompelete session
        lnRelCho = gfModalGen("QRM44039B44006","Dialog")
      ENDIF
    ENDIF
    SET ORDER TO lcTag IN (lcPckLin)
  ENDIF
  IF lnRelCho <> 2 
    SELECT(lcPckLin)
    SCAN
      llNoThing  = gfSEEK('O'+lcOrderNo+&lcStores..STORE+&lcPckLin..Style+STR(nOrdLineNo,6),'OrdLine')
      IF llNoThing AND &lcPckLin..nStep = 0      && uncompelete session
        IF !loFormset.llComp
          SELECT OrdLine 
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
          REPLACE nPck&lcSz.   WITH &lcPckLin..PQty&lcSz.,;
                  nPWght       WITH nPWght + &lcPckLin..PWgh&lcSz.                             
                              
          IF !EMPTY(&lcPckLin..PACK_ID)            
            lcSvOrd = ORDER('SPCK_LIN')
            SELECT SPCK_LIN   
            =gfSetOrder('SPCK_LIN')
            SELECT OrdLine 
            IF gfSEEK('P'+ORDLINE.ACCOUNT+ORDLINE.PACK_ID+ORDLINE.CPKCOLOR+ORDLINE.CPCKSIZE+ORDLINE.CPKVERSION+ORDLINE.STYLE,'SPCK_LIN')
              REPLACE ORDLINE.NPKPACK WITH (nPck1+nPck2+nPck3+nPck4+nPck5+nPck6+nPck7+nPck8)/SPCK_LIN.TOTQTY
            ENDIF  
            SELECT SPCK_LIN             
            =gfSetOrder(lcSvOrd)
            SELECT OrdLine 
            =gfReplace("")
          ENDIF
        ENDIF

        IF loFormset.llUpdtPkTk
          lcSz =&lcPckLin..cnoSize   
          SELECT (lcPckLin)
          REPLACE nDiff&lcSz. WITH PQty&lcSz.- OrdLine.Pik&lcSz. 
          *REPLACE nDiff1 WITH PQty1- OrdLine.Pik1 ,;
                  nDiff2 WITH PQty2- OrdLine.Pik2 ,;
                  nDiff3 WITH PQty3- OrdLine.Pik3 ,;
                  nDiff4 WITH PQty4- OrdLine.Pik4 ,;
                  nDiff5 WITH PQty5- OrdLine.Pik5 ,;
                  nDiff6 WITH PQty6- OrdLine.Pik6 ,;
                  nDiff7 WITH PQty7- OrdLine.Pik7 ,;
                  nDiff8 WITH PQty8- OrdLine.Pik8

          IF !loFormset.llComp
            SELECT OrdLine 
            REPLACE Pik1  WITH nPck1,;
                  Pik2  WITH nPck2,;
                  Pik3  WITH nPck3,;
                  Pik4  WITH nPck4,;
                  Pik5  WITH nPck5,;
                  Pik6  WITH nPck6,;
                  Pik7  WITH nPck7,;
                  Pik8  WITH nPck8,;
                    TotPik  WITH Pik1 + Pik2 + Pik3 + Pik4 +Pik5 + Pik6 + Pik7 + Pik8
          ENDIF
        ENDIF

        *-- If we are packing from piktkt
        IF !EMPTY(&lcStores..PIKTKT)
          *-- update pik fields in ordline file only
          *-- If user option release or (save as is and the line is selected)
          *-- which means if it is unselected line and "save as is" left the 
          *-- pik quantities as it is.
          IF lnRelCho = 1 OR ; 
             (lnRelCho = 3  AND ;
               !( EMPTY(&lcPckLin..cSelect1) OR EMPTY(&lcPckLin..cSelect2) OR ;
                  EMPTY(&lcPckLin..cSelect3) OR EMPTY(&lcPckLin..cSelect4) OR ;
                  EMPTY(&lcPckLin..cSelect5) OR EMPTY(&lcPckLin..cSelect6) OR ;
                  EMPTY(&lcPckLin..cSelect7) OR EMPTY(&lcPckLin..cSelect8) ) )

            IF !loFormset.llComp
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
                      
         REPLACE Pik&lcSz.   WITH &lcPckLin..PQty&lcSz.,;             
                 TotPik WITH TotPik+ &lcPckLin..PQty&lcSz.,;
                      PikTkt  WITH IIF(TotPik=0,'',PikTkt),;             
                      PikDate WITH IIF(TotPik=0,{},PikDate),;
                      Picked  WITH IIF(TotPik=0,.F.,Picked)              
                      
            ENDIF
          ENDIF                  
        ENDIF                    
        SELECT (lcPckLin)
        REPLACE &lcPckLin..nStep WITH 1
      ENDIF

      *--  updating the style file.
      IF &lcPckLin..nStep = 1      && uncompelete session
        IF !loFormset.llComp
          SELECT STYLE
          lcSz =&lcPckLin..cnoSize   
          IF loFormset.llUpdtPkTk AND !EMPTY(&lcStores..PIKTKT) AND gfSEEK(&lcPckLin..Style,'Style')
*              REPLACE Alo1   WITH Alo1 + &lcPckLin..nDiff1,;
                    Alo2   WITH Alo2 + &lcPckLin..nDiff2,;
                    Alo3   WITH Alo3 + &lcPckLin..nDiff3,;
                    Alo4   WITH Alo4 + &lcPckLin..nDiff4,;
                    Alo5   WITH Alo5 + &lcPckLin..nDiff5,;
                    Alo6   WITH Alo6 + &lcPckLin..nDiff6,;
                    Alo7   WITH Alo7 + &lcPckLin..nDiff7,;
                    Alo8   WITH Alo8 + &lcPckLin..nDiff8,;
                    TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
              REPLACE Alo&lcSz.   WITH Alo&lcSz. + &lcPckLin..nDiff&lcSz.,;
                      TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
                    
          ENDIF
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
                  
          REPLACE Ord&lcSz.   WITH IIF(&lcPckLin..PQty&lcSz.>OrdLine.Qty&lcSz.,Ord&lcSz.+(&lcPckLin..PQty&lcSz.-OrdLine.Qty&lcSz.),Ord&lcSz.),;
                  TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
                  
                  
                  
        ENDIF         
        =gfReplace("")
        SELECT (lcPckLin)
        REPLACE &lcPckLin..nStep WITH 2
      ENDIF
      
      IF !loFormset.llComp
        llStyDyOpn = gfOpenTable(oAriaApplication.DataDir+"StyDye",oAriaApplication.DataDir+'StyDye')
        *--  updating the stydye file.
        lcSz =&lcPckLin..cnoSize  
        IF &lcPckLin..nStep = 2      && uncompelete session
          IF !EMPTY(&lcStores..PIKTKT)
            IF gfSEEK (OrdLine.Style+loFormset.lcWareCode,'StyDye')
              SELECT StyDye
              
              IF loFormset.llUpdtPkTk
             
              * REPLACE Alo1 WITH Alo1 + &lcPckLin..nDiff1,;
                       Alo2 WITH Alo2 + &lcPckLin..nDiff2,;
                        Alo3 WITH Alo3 + &lcPckLin..nDiff3,;
                        Alo4 WITH Alo4 + &lcPckLin..nDiff4,;
                        Alo5 WITH Alo5 + &lcPckLin..nDiff5,;
                        Alo6 WITH Alo6 + &lcPckLin..nDiff6,;
                        Alo7 WITH Alo7 + &lcPckLin..nDiff7,;
                        Alo8 WITH Alo8 + &lcPckLin..nDiff8
             REPLACE Alo&lcSz. WITH Alo&lcSz. + &lcPckLin..nDiff&lcSz.,;

             REPLACE StyDye.TotAlo WITH StyDye.Alo1+StyDye.Alo2+StyDye.Alo3+;
                                          StyDye.Alo4+StyDye.Alo5+StyDye.Alo6+;
                                          StyDye.Alo7+StyDye.Alo8
            
             ENDIF

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
          REPLACE Ord&lcSz. WITH Ord&lcSz. + MAX(&lcPckLin..PQty&lcSz.-OrdLine.Qty&lcSz.,0),;
                     TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8                     
            
            =gfReplace("")
            ENDIF
          ENDIF
          SELECT (lcPckLin)
          REPLACE &lcPckLin..nStep WITH 3
        ENDIF
      ENDIF

      IF !EMPTY(&lcStores..PIKTKT)
        lcSz =&lcPckLin..cnoSize  
        IF &lcPckLin..nStep = 3      && uncompelete session
          IF loFormset.llDyelot AND  Style.cDye_Flg = 'Y' AND ;
             SEEK(OrdLine.Style+loFormset.lcWareCode+OrdLine.Dyelot,'StyDye')
            IF !loFormset.llComp
              SELECT StyDye
              IF loFormset.llUpdtPkTk
              *REPLACE Alo1 WITH Alo1 + &lcPckLin..nDiff1,;
                      Alo2 WITH Alo2 + &lcPckLin..nDiff2,;
                      Alo3 WITH Alo3 + &lcPckLin..nDiff3,;
                      Alo4 WITH Alo4 + &lcPckLin..nDiff4,;
                      Alo5 WITH Alo5 + &lcPckLin..nDiff5,;
                      Alo6 WITH Alo6 + &lcPckLin..nDiff6,;
                      Alo7 WITH Alo7 + &lcPckLin..nDiff7,;
                      Alo8 WITH Alo8 + &lcPckLin..nDiff8
          REPLACE Alo&lcSz. WITH Alo&lcSz. + &lcPckLin..nDiff&lcSz.
          
          REPLACE StyDye.TotAlo WITH StyDye.Alo1+StyDye.Alo2+StyDye.Alo3+;
                        StyDye.Alo4+StyDye.Alo5+StyDye.Alo6+;
                        StyDye.Alo7+StyDye.Alo8
                                        
              ENDIF

*!*                REPLACE Ord1 WITH Ord1 + MAX(&lcPckLin..PQty1-OrdLine.Qty1,0),;
*!*                        Ord2 WITH Ord2 + MAX(&lcPckLin..PQty2-OrdLine.Qty2,0),;
*!*                        Ord3 WITH Ord3 + MAX(&lcPckLin..PQty3-OrdLine.Qty3,0),;
*!*                        Ord4 WITH Ord4 + MAX(&lcPckLin..PQty4-OrdLine.Qty4,0),;
*!*                        Ord5 WITH Ord5 + MAX(&lcPckLin..PQty5-OrdLine.Qty5,0),;
*!*                        Ord6 WITH Ord6 + MAX(&lcPckLin..PQty6-OrdLine.Qty6,0),;
*!*                        Ord7 WITH Ord7 + MAX(&lcPckLin..PQty7-OrdLine.Qty7,0),;
*!*                        Ord8 WITH Ord8 + MAX(&lcPckLin..PQty8-OrdLine.Qty8,0),;
*!*                        TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
           REPLACE Ord&lcSz. WITH Ord&lcSz. + MAX(&lcPckLin..PQty&lcSz.-OrdLine.Qty&lcSz.,0),;
                   TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
            =gfReplace("")
            ENDIF
          ENDIF
          SELECT (lcPckLin)
          REPLACE &lcPckLin..nStep WITH 4
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
  IF USED("StyDye")
    SELECT "StyDye"
     =gfTableUpdate()
  ENDIF 
  IF llStyDyOpn
    SELECT "StyDye"
    =gfCloseTable("StyDye")
  ENDIF  
  *===============end updating style,stydye files===================

  *===============this part for updating pack_hdr,bol files=========
  IF lnRelCho <> 2

    *-- This is to get the count of only cartons that have contents
    *-- and the last carton no in the pack also that has contents
    *-- Start
    SELECT (lcCtnHdr)
    lcCtHdInd = ORDER(lcCtnHdr)
    SET ORDER TO EMPTY IN (lcCtnHdr)
    IF SEEK ('Y',lcCtnHdr)
      lnRecNo = RECNO()
      SKIP -1
      lnLastCtn = &lcCtnHdr..Cart_No
      GO lnRecNo
      SCAN REST FOR Empty = 'Y'
        lnPackCtn = lnPackCtn - 1 
      ENDSCAN
    ELSE
      GO BOTTOM
      lnLastCtn = &lcCtnHdr..Cart_No
    ENDIF
    SET ORDER TO lcCtHdInd IN (lcCtnHdr)
    *-- End getting last carton and carton count

    SELECT Pack_Hdr
    IF !gfSEEK(lcPackNum ,'Pack_Hdr')
      *-- this part to Handle pack_no
      IF EMPTY(&lcStores..PIKTKT)
        =gfSEEK('O'+lcOrderNo,'ORDHDR')
         lcPackNum  = gfSequence('PIKTKT', '', '', ORDHDR.cDivision)        

        DO WHILE gfSEEK( lcPackNum ,'PACK_HDR')
          lcPackNum  = gfSequence('PIKTKT', '', '', ORDHDR.cDivision)
        ENDDO
        INSERT INTO PACK_HDR (PACK_NO) VALUES (lcPackNum)
        SELECT Pack_Hdr
        =gfReplace("")
        REPLACE &lcStores..PACK_NO WITH lcPackNum
        
      ELSE
        lcPackNum = &lcStores..PIKTKT
        INSERT INTO PACK_HDR (PACK_NO) VALUES (lcPackNum)
        SELECT Pack_Hdr
        REPLACE Account WITH lcAccount,;
                Order WITH lcOrderNo,;
                Store WITH &lcStores..Store,;
                shipvia WITH ORDHDR.Shipvia
        =gfReplace("")        
      ENDIF
      *=============This part for BOL=================
      *-- This part will be executed only if the system has ASN Module
      *===============end of BOL=========================
      SELECT Pack_Hdr
    ENDIF  
    lnLastNo = Pack_Hdr.nLastLNo    
    
    *-- Get totals for this packing
    SELECT &lcCtnHdr
    SCAN
      m.Tot_Wght = m.Tot_Wght+ TotWgh
      m.Tot_Cart = m.Tot_Cart + 1
      m.Tot_Pcs = m.Tot_Pcs  + TotPcs
    ENDSCAN


    m.Account = lcAccount
    m.Order = lcOrderNo
    m.Store = &lcStores..Store
    m.shipvia = ORDHDR.Shipvia
    m.LStandCtn = IIF(loFormset.lnCtnTyp = 1,.T.,.F.)
    m.CToStorCn = IIF(loFormset.lnDrctTo = 1,'S','C')
    SELECT Pack_Hdr
    GATHER MEMO MEMVAR  
    
    REPLACE cWareCode WITH loFormset.lcWareCode,;
            Bill_Ladg WITH ""
        
     =gfAdd_Info('Pack_Hdr')
     =gfReplace("")       

  ENDIF
  *=================end of updating pack_hdr,BOL Files================

  *=================This part for saving pack_lin file================
  IF lnRelCho <> 2

    lcSetDele = SET('DELETE')
    SET DELETE ON
    SELECT EDICRTSQ
    =gfSetOrder('PCKCRTSQ')
    IF gfSEEK(lcPackNum,'EDICRTSQ')
      SELECT EDICRTSQ
      SCAN REST WHILE pack_no+STR(cart_no,6) = lcPackNum
        gfDELETE()
      ENDSCAN 
    ENDIF  
    SET DELETE &lcSetDele 

    SET DELETED OFF
    SELECT(lcCtnDtl)
    *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[Start]
    *SET FILTER TO STORE = &lcStores..Store
    SET FILTER TO STORE = &lcStores..Store AND PIKTKT =&lcStores..PIKTKT
    *B609552,1 MMT 03/21/2011 Custom automatic Packing list program gives wrong PL#[End]
    SET ORDER TO 0
    = gfSEEK('O'+lcOrderNo ,'OrdHdr')
    SCAN FOR cStatus <> 'S'
      IF &lcCtnDtl..nStep = 0   && for uncomplete session
        SELECT Pack_Lin
        IF gfSEEK(lcPackNum+STR(&lcCtnDtl..Cart_No,4)+&lcCtnDtl..Style,'Pack_Lin')
          IF DELETED('Pack_Lin')
            LOCATE REST FOR pack_no+STR(no_cart,4)+style = ;
                            lcPackNum+STR(&lcCtnDtl..Cart_No,4)+;
                            &lcCtnDtl..Style AND !DELETED()
          ENDIF
          *LOCATE REST FOR Pack_No+STR(Line_No,6)+Style+cPackColor = ;
                          lcPackNum+STR(&lcCtnDtl..PackLineNo,6)+&lcCtnDtl..Style
*!*            IF loFormSet.ActiveMode = 'E'     
*!*              LOCATE REST WHILE Pack_No+STR(Line_No,6)+STYLE+Dyelot = lcPackNum+STR(&lcCtnDtl..PackLineNo,6)+&lcCtnDtl..STYLE                
*!*            ENDIF   
        ENDIF
        
        IF !FOUND('Pack_Lin') AND !DELETED(lcCtnDtl)
          lnLastNo = lnLastNo + 1
          APPEND BLANK
          REPLACE Line_No WITH lnLastNo
          =gfReplace("")
          SELECT (lcCtnDtl)
          REPLACE PackLineNo WITH lnLastNo
        ENDIF
        *-- This seek for getting Pal_No
        lcSz =&lcCtnDtl..cnoSize 
        llNothing = SEEK(STR(&lcCtnDtl..Cart_No,4),lcCtnHdr)
        SELECT Pack_Lin

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
        REPLACE Pack_No    WITH lcPackNum,;
                No_Cart    WITH &lcCtnDtl..Cart_No,;
                NPltNo     WITH &lcCtnHdr..Pal_No,;                
                Style      WITH &lcCtnDtl..Style,;
                nOrdLineNo WITH &lcCtnDtl..nOrdLineNo,;                  
                Qty&lcSz.  WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty&lcSz.,0),;
                TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
                


        *-- Save information of the pack in PACK_LIN file 
        REPLACE PACK_ID    WITH &lcCtnDtl..PACK_ID,;
                cPkColor   WITH &lcCtnDtl..cPkColor,;
                cPCkSize   WITH &lcCtnDtl..cPckSize,;
                cPKVersion WITH &lcCtnDtl..cPkVersion,;
                nPackNO    WITH &lcCtnDtl..nPackNO    

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

        REPLACE Weight     WITH Weight + IIF(!DELETED(lcCtnDtl),;
                                    &lcCtnDtl..Weight&lcSz.,MAX(Weight-&lcCtnDtl..Weight&lcSz.+(&lcCtnDtl..Qty&lcSz.*&lcCtnDtl..OrgWgh),0))


        =gfAdd_Info('Pack_Lin')
        =gfReplace("")       
        IF Pack_Lin.TotQty = 0
          gfDELETE()
        ENDIF 
        SELECT (lcCtnDtl)
        REPLACE &lcCtnDtl..nStep WITH 1
      ENDIF

     
      *=================update ordline file ==========================
      IF lnRelCho <> 2
        IF !loFormset.llComp
          SELECT OrdLine
          *-- These 2 seek are to adjust the pointer in both files (lcpcklin,'OrdLine')

          SET ORDER TO lcPckLin IN (lcPckLin)
          =SEEK(&lcCtnDtl..Style+STR(&lcCtnDtl..nOrdLineNo,6),lcPckLin)
         
          =gfSEEK('O'+lcOrderNo +&lcStores..STORE+&lcCtnDtl..Style+STR(&lcCtnDtl..nOrdLineNo,6),'OrdLine')
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

          REPLACE Book&lcSz.    WITH Book&lcSz. +MAX(&lcPckLin..OrdQty&lcSz. -OrdLine.Qty&lcSz. ,0),;
                  ToTBook WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8

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

           REPLACE Qty&lcSz.    WITH &lcPckLin..OrdQty&lcSz.,;
                  TotQty  WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
          lnBookDiff = lnBookDiff + (TotBook - lnOrgBook)
          lnBkAmtDif = lnBkAmtDif + ((TotBook - lnOrgBook) * Price)

          lnQtyDiff  = lnQtyDiff  + (TotQty - lnOrgQty)
          lnQyAmtDif = lnQyAmtDif + ((TotQty - lnOrgQty) * Price)      
        ENDIF
      ENDIF
      *=================end updating ordline file======================    

    ENDSCAN

    SELECT Pack_Lin
    lcSetDel = SET('DELETE')
    SET DELETE ON
    =gfSEEK(lcPackNum)
    lnI = 0
    llFirst  = .T.
    lnLastCrt = Pack_Lin.No_Cart
    llSameCrt = .F.
    SCAN REST WHILE Pack_No+STR(No_Cart,4)+Style = lcPackNum
      IF !llFirst
        llSameCrt = (Pack_Lin.No_Cart = lnLastCrt)
        IF !llSameCrt 
          lnLastCrt = Pack_Lin.No_Cart
        ENDIF
      ENDIF
      IF llFirst OR !llSameCrt 
        lnI = lnI + 1
      ENDIF
      REPLACE Pack_Lin.No_Cart WITH lnI
      =gfReplace("")
      llFirst  = .F.
    ENDSCAN

    SELECT Pack_Lin
    =gfSEEK(lcPackNum)
    SCAN REST WHILE Pack_No+STR(No_Cart,4)+Style = lcPackNum
      SELECT EDICRTSQ
      =gfSetOrder('PCKCRTSQ')
      SELECT Pack_Lin
      IF !gfSEEK(lcPackNum+STR(Pack_Lin.No_Cart,6),'EDICRTSQ')
        *-- If this customer is a partner , get the last Ucc # from EDIACPRT file,
        IF gfSEEK('A'+lcAccount ,'EDIACPRT')
          lnUcc9  = IIF(EMPTY(EDIACPRT.Ucc9),0,EVAL(EDIACPRT.Ucc9)) + 1
          SELECT EDICRTSQ
          =gfSetOrder("EDICRTSQ")
          llFound = gfSEEK(lcAccount +PADL(lnUcc9,9,'0'),'EDICRTSQ')
          DO WHILE llFound 
            lnUcc9  = lnUcc9  + 1
            llFound = gfSEEK(lcAccount +PADL(lnUcc9,9,'0'),'EDICRTSQ')
          ENDDO
          INSERT INTO EDICRTSQ (Pack_No,Account,cart_no,Ucc9);
                         VALUE (lcPackNum,lcAccount ,Pack_Lin.No_Cart,PADL(lnUcc9,9,'0'))
          REPLACE EDIACPRT.Ucc9 WITH PADL(lnUcc9,9,'0')
          =gfReplace("")
        ELSE    && Get the last Ucc # from EDICRTSQ file.
          SELECT EDICRTSQ 
          =gfSeek(lcAccount)
          SELECT MAX(EDICRTSQ .ucc9) FROM EDICRTSQ WHERE EDICRTSQ.account = lcAccount INTO CURSOR lcMaxUcc
          SELECT lcMaxUcc
          LOCATE
          IF EOF()
            lcUcc9 = '000000001'
          ELSE
            lcUcc9 = PADL(EVAL(lcMaxUcc.MAX_UCC9)+1,9,'0')
          ENDIF  
          INSERT INTO EDICRTSQ (Pack_No,Account,cart_no,Ucc9);
                         VALUE (lcPackNum,lcAccount ,Pack_Lin.No_Cart,lcUcc9)
          =gfReplace("")              
        ENDIF  
      ENDIF  
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
    
    SELECT Pack_Hdr
    REPLACE nLastLno  WITH MAX(nLastLno,lnLastNo) ,;
            nLastCart WITH lnLastCtn
    
     =gfReplace('')            
  ENDIF
  *=================end saving pack_lin file======================  
  
  *=================update book,open fields in ordhdr file=========
  IF lnRelCho <> 2 
    IF !(lnBookDiff=0 AND lnQtyDiff=0 AND lnBkAmtDif=0 AND lnQyAmtDif=0) AND ;
       gfSEEK('O'+Pack_Hdr.Order,'OrdHdr')

      IF !loFormset.llComp
        SELECT OrdHdr
        =RLOCK()
        REPLACE BOOK    WITH BOOK    + lnBookDiff ,;
                BOOKAMT WITH BOOKAMT + lnBkAmtDif ,;
                OPEN    WITH OPEN    + lnQtyDiff  ,;
                OPENAMT WITH OPENAMT + lnQyAmtDif
        UNLOCK        
        =gfAdd_Info("OrdHdr")
        =gfReplace("")
      ENDIF
    ENDIF
  ENDIF
ENDIF
SET DELETED &lcDelStat

IF lnRelCho <> 2
  llReturn = .T.
ELSE
  llReturn = .F.
ENDIF

SELECT(lcCtnDtl)
SET RELATION TO &lcCtDtRel.

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
SELECT Pack_lin
=gfTableUpdate()
SELECT Pack_Hdr
=gfTableUpdate()
SELECT OrdHdr
=gfTableUpdate()
SELECT OrdLINE
=gfTableUpdate()
SELECT Style
=gfTableUpdate()
IF USED('EDICRTSQ')
  SELECT EDICRTSQ
  =gfTableUpdate()
ENDIF 
=lfOrdHdLck(.F.,loFormSet)
SELECT(lnCurAlias)
RETURN llReturn
*-- end of lpSavscr.     


*!*************************************************************
*! Name      : lfNoSuffic
*! Developer : MMT -MARIAMM  MAZHAR
*! Date      : 03/01/2009
*! Purpose   : Check Stydye Availability
*!*************************************************************
*!
FUNCTION lfNoSuffic
PARAMETERS loFormSet
lcPckLin =loFormSet.lcPckLin
lcStores = loFormSet.lcStores
lcOrderNo = loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE  
PRIVATE lcMessage , lnI , lcI , lnChoice , llExitLoop, llRet2Main
STORE .F. TO llExitLoop, llRet2Main
SELECT(lcPckLin)
SCAN
  llNoThing = gfSEEK('O'+lcOrderNo +&lcStores..STORE+&lcPckLin..Style+STR(nOrdLineNo,6),'OrdLine')
  IF llNoThing
    lnI = 0
    FOR lnI = 1 TO 8
      lcI = STR(lnI,1)
      IF (PQty&lcI > OrdLine.Pik&lcI) AND ;
          gfSEEK(OrdLine.Style+OrdLine.cWareCode+OrdLine.Dyelot,'StyDye') AND ;
          PQty&lcI > (StyDye.Stk&lcI - StyDye.Alo&lcI)
        lcMessage = "Order : " + lcOrderNo  + ", Style : " + ordline.style +;
                     ", does not have available quantity."
        IF loFormSet.llAlwForce
          lcMessage = lcMessage + "Do you want to force allocation?"
          *GFMODALGEN
          * <Yes> <Yes to All> <No>
          *lnChoice = 1 or 2 or 3
          lnChoice  =gfModalGen("INM00000B44002","Dialog","","",lcMessage)  
          DO CASE
            CASE lnChoice = 1
              *DO NOTHING

            CASE lnChoice = 2
              llExitLoop = .T.

            CASE lnChoice = 3
              STORE .T. TO llExitLoop , llRet2Main                   
          ENDCASE
        ELSE
          lcMessage = lcMessage + "Save without update pick Quantity?"
          *GFMODALGEN
          * <Yes> <No>
          *lnChoice=1 or 2
          lnChoice =gfModalGen("INM00000B44009","Dialog","","",lcMessage)  
          IF lnChoice=1
            loFormset.llUpdtPkTk = .F.
            llExitLoop = .T.
                   
           ELSE
             STORE .T. TO llExitLoop , llRet2Main                   
           ENDIF
         ENDIF
         *-- Exit for loop
         IF llExitLoop
           EXIT
         ENDIF
      ENDIF
    ENDFOR
          
    *-- exit scan loop
    IF llExitLoop
      EXIT
    ENDIF          

  ENDIF
ENDSCAN
RETURN llRet2Main
*-- end of lfNoSuffic.
*!*************************************************************
*! Name      : lfSuppForc
*! Developer : MMT -MARIAM MAzhar
*! Date      : 03/01/2009
*! Purpose   : User can Force allocation. (Y/N)
*!*************************************************************
*!
FUNCTION lfSuppForc
PARAMETERS loFormSet
PRIVATE llAlwForce
llAlwForce = .T.

IF gfGetMemVar('M_FORCEALO',oAriaApplication.ActiveCompanyID)  <> "Y"
  *-- No Force allocation done.
  IF gfGetMemVar('M_FORCEALO',oAriaApplication.ActiveCompanyID)   = "N"
    llAlwForce = .F.  && User can not
  ELSE  && User Prev.
    *-- Call user defined process.  
    llAlwForce = gfUserPriv('AL','ALAUTAL','FORCING')
  ENDIF
ENDIF
RETURN llAlwForce
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
FUNCTION lfOrdHdLck
PARAMETERS llLock,loFormSet
lcOrder = loFormSet.AriaForm1.kbOrderNo.keytextbox.VALUE
PRIVATE lnAlias,lcSvOrd,llGoOn
llGoOn = .T.
lnAlias = SELECT()
lcSvOrd = ORDER('ORDHDR')
SELECT ORDHDR
=gfSetOrder('ORDHDR')
=gfSEEK('O'+lcOrder,'ORDHDR')
=gfObj_Lock(llLock)
=gfSetOrder(lcSvOrd)
SELECT (lnAlias)
RETURN llGoOn
*-- end of lfOrdHdLck.

