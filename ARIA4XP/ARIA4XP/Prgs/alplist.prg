**:***********************************************************************
*:  Program file : ALPLIST.PRG
*:  Program desc.: Manual Packing List (Custom for GMA)
*:         System: Aria 4XP
*:      Developer: Khalid Mohi El-Din Mohamed
*:           Date: 11/07/2004
*:      Reference: *E037427,1
*:************************************************************************
*: Modifications:
*: B132257,1 HBG 05/21/2006 Fix bug of not updating Qty,Crt.Wieght in BOL_HDR file
*: E302315,1 WLD 10/16/2006 Using visual label report - include E040123,1 HBG
*                           Get the UCC # structure from the setup
*: B607804,1 WAM 10/19/2006 Clear order lock after save packing list
*: B608181,1 WLD 07/29/2007 Select the temporary file as (loFormSet.lcTmAsnShp) instead of (lcTmAsnShp)
*: B608242,1 MMT 08/27/2007 fix bug of wrong style weight while creating new packing list					[T20070801.0021]
*: B608244,1 MMT 08/27/2007 fix of Locking ordhdr in case of more than one piktkt 							  [T20070801.0022]
*: B608254,1 WAM 09/05/2007 Fix the bug of not updating packed quantity in ORDLINE when delete a packing list
*: B608242,1 MMT 08/27/2007 fix bug of wrong style weight while creating new packing list					[T20070801.0021]
*: B608244,1 MMT 08/27/2007 fix of Locking ordhdr in case of more than one piktkt 							  [T20070801.0022]
*! B608296,1 MMT 09/30/2007 fix bug or error if EDICRTSQ  is empty											          [T20070904.0323]
*! B608336,1 MMT 10/29/2007 fix bug of displaying wrong styles not included in PIKTKT 						[T20071022.0155]
*! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list                            [T20071009.0026]
*! B608364,1 WAM 11/28/2007 Check BOL assigned to packing list when the shipvia changed
*! B608381,1 WAM 12/13/2007 Display the right cust. PO#
*! B608416,1 WAM 01/27/2008 Don't zero order quantity for unselected sizes
*! B608456,1 WAM 02/27/2008 Fix error variable oFormset not found when copy from another packing list [T20080208.0015]
*! C200945,1 WLD 02/21/2008 Custom Scan and Pack Program,Fix  bug of pressing enter when Scan using the Upc (PRG,Screen)
*! E302507,1 WLD 03/17/2008 Call Packing List screen by parameter T20070625.0003
*! B608518,1 MMT 04/17/2008 Fix bug of Wrong Line Qty when Editing Packing List [T20071129.0005]
*! C200876,1 TMI 06/01/2008 Addting the bin locations custom triggers
*! B608622,1 WAM 07/14/2008 Fix bug while edit Ship to field [T20080714.0033]
*! B608647,1 MMT 08/11/2008 Delete last carton in Packing list damage next PL cartons[T20080710.0003]
*! B608664,1 MMT 08/26/2008 Fix bug of packing deleted sizes [T20080821.0023]
*! E302567,1 MMT 01/06/2009 Change file paths for SAAS[Task:T20081225.0022]
*! B608874,1 TMI 05/27/2009 Define a scan mode
*! B608882,1 TMI 05/31/2009 fix the problem that the weight in the scan mode is calculated wrong, also show the correct customer name in the piktkt browse
*! B608972,1[T20080821.0007]MMT 08/13/2009 Change order to avoid blank lines
*! B609138,1 MMT 02/04/2010 Fix bug of repeated lines in pack_lin after editing PL[T20100122.0004]
*! B609167,1 MMT 03/11/2010 the orders are not changing the ship to address's should either be stores or to DC[T20100223.0023]
*! B609245,1 HES 05/12/2010 style module - Incorrect weights in allocation screens [T20100310.0021]
*! E302698,1 MMT 05/25/2010 Display SKU# in Style Detail and Carton Detail grids[T20100322.0039]
*! B609485,1 MMT 01/05/2011 Fix bug of wrong style allocated qty  and SO Booked Qty[T20100816.0023]
*! E302857,1 MMT 04/27/2011 Use New Property to point to the EDI Folder classes[MEDIA]
*! B609658,1 WAM 08/09/2011 Allow scanning UPC for packs [T20110718.0020]
*! B609720,1 TMI 10/31/2011 set the correct order to allow update the BOL_HDR file correctly [T20110720.0047]
*! E302998,1 HES 27/11/2011 Change the way of getting the UCC9 while saving the P\L [T20110620.0017]
*! B609768,1 MMT 12/12/2011 Cannot add new code for carton type in manual PL Screen[T20110720.0050]
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[T20100922.0014]
*! E302998,2 HES 27/11/2011 Handle this issue in LFGETUCC9 function in EDIGLOBL program [T20110620.0017]
*! E302998,3 HES 01/16/2012 Update to work if 'AS' module is included no more [BEGIN]
*! B609837,1 MMT 02/20/2012 Error while editing Packing list has 2 styles with diff. scales[T20120131.0033]
*! E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004]
*! B609859,1 HES 03/08/2012 Don't call SEND SendUccLabels object if the BOL is empty [T20120306.0019]
*! B609997,1 MMT 07/11/2012 Update Pick Ticket option updates unpacked lines in Ordline[T20120206.0037]
*! B610008,1 HIA 07/16/2012 Error, type mismatch, when saving new P\L [T20120621.0065]
*: B610016,1 SAB 07/16/2012 Enable Editing Notes and Special Inst. for Completed Packs [T20120227.0014]
*! E303260,1 HES 09/20/2012 Add records in EDICRTSQ while no BOL assigned [T20120703.0017]
*! B610281,1 HIA 03/24/2013 Aria XP - AR - Scan pick ticket invoice not updating style and stydye files, T20130117.0010
*! B610287,1 HIA 04/02/2013 Aria XP - Manual Packing List - Assign default, T20130226.0030
*! B610615,1 TMI 12/09/2013 be sure that the asn_ship tabB610733,1 low Error when creating a packing list [T20131224.0037 ] 
*! B610733,1 HES 05/26/2014 Fix a bug happened while releasing shortages in P/L [T20140512.0016]
*:************************************************************************
#INCLUDE R:\ARIA4XP\PRGS\ALPLIST.h

*E302507,1 WLD 03/17/2008 Call Packing List screen by parameter [Begin]
PARAMETERS lcPackLsNo
*E302507,1 WLD 03/17/2008 Call Packing List screen by parameter [End]

*-- Initialize the variables in order to define it as properties in the formset.
PRIVATE lcAllVar
lcAllVar = "llPacked,llComp,laStySrc,laStyTrgt,laOldSty,laClrSrc,laClrTrgt,laOldClr,"+;
  "lcCtnHdr,lcCtnDtl,lcTmCtnDtl,lcStyTtl,lcDistCtr,lcBol,lcStyPic,lcMask,llntfound,"+;
  "lcOldVal,lcCtnSty,lcfoldprnt,lcPackNo,lcPckLin,lcTmpStores,lcLsVPck,lnPalNo,lcPackLines,"+;
  "lnDumCtnR,lnNonMajSt,lcFree_Clr,lcTmpDetFl,lcStySz,lnCtDtBrRe,lcTmAsnShp,"+;
  "lnStyleWid,lnRelCho,lnTotWgh,lnMajLen,lcBollsttl,lnColorLen,lnCtHdBrRe,"+;
  "lnStyWgh,lnCartNo,lnHdBrRec,lnDtBrRec,lnMinPal,lnCtnPal,lnStyQty,lnMaxPal,"+;
  "lnLastNo,lnMaxCtn,lnPackWgh,lnPackQty,lnTo,lcSndPort,lnLastCtn,lnFrom,"+;
  "lnPackCtn,lnCtnTyp,lnDrctTo,llShoPckSu,llScrPrnLb,llPCDStat,lnBrUntWgh,"+;
  "lnOrdMdPr,llPalStat,llEdiSys,llAsnSys,llEdiAcc,lnBrCtnQty,llClrSel,llMulWare,"+;
  "llNew,llShwOpn,llEdiopn,llB_H_Opn,llB_D_Opn,llDyelot,llAnyRec,llPckCopy,"+;
  "llPrint,llNoShow,llSelOrdLi,llUnComp,llCupdate,llAnyUpd,lcManufId,llPakPrSiz,"+;
  "llCanPrnLb,llQtyArr,llWghArr,llUpdtPkTk,llAlwForce,llBolnever,llUseConfg,"+;
  "lcSizeSep,llExtSizSc,llBolAutom,lcWareCode,lcTmpIdx,lcSumPck,lcAsnLabel,"+;
  "lnSizePos,lnSizeLen,lcForceAlo,lcPLSortBy,lcOrdNo,lcStoreNo,lcPakIndxLn,"+;
  "lcPakIndxSt,EDIPH,PikLine,Scale,Style,StyDye,Customer,EDICRTSQ,EDIACPRT,PikTkt,"+;
  "Pack_Hdr,OrdHdr,SPck_Hdr,Pack_Lin,BOL_HDR,BOL_LIN,SPCK_LIN,CUSTDEPT,WareHous,"+;
  "OrdLine,lcTempCur,lcTmpScpSty,lcTmpScpPck,lcScpStyFlt,llStyFltr,llClrFltr,llFilter,Asn_Ship,SYCASNLB,"+;
  "STYLEUPC,llDetLabel,lcDetLbAll,lcDetailVr,llUPCInst,llUsePack,llPckFltr,lcScpPckFlt,lcTmPckLin,lcPrvPack"

*E302315,1 WLD 10/16/2006 Using visual label report -Check for UCC # structure setup [Begin]
lcAllVar = lcAllVar + ",SYCASNHD,SYCASNDT,laLblInfo,lcPrnAsnShp,llIncPLNum,lnNumOfDig,SETUPS,SYCCONFG"
*E302315,1 WLD 10/16/2006 Using visual label report [End]
*C200945,1 WLD Fix  bug of pressing enter when Scan using the Upc  (PRG,Screen) 02/21/2008 [Begin]
lcAllVar = lcAllVar + ",llConfirm,lcSetConf"
*lcSetConf  = SET('CONFIRM')	 && Save the confirmation setting.
*C200945,1 WLD [End]
*-- Arrange the string into an array in order to create these variables as properties
DIMENSION laAllVar[1,1]
STORE '' TO laAllVar
=gfSubStr(lcAllVar,@laAllVar,',')
LOCAL lnI
FOR lnI = 1 TO ALEN(laAllVar,1)
  PRIVATE &laAllVar[lnI,1].
ENDFOR

*-- Initialize the variables
DIMENSION laStySrc[1,1],laStyTrgt[1,1],laOldSty[1,1],;
  laClrSrc[1,1],laClrTrgt[1,1],laOldClr[1,1]

STORE .F. TO llPacked, llComp, llntfound, llScrPrnLb,llEdiSys,llAsnSys,llEdiAcc,llPCDStat,;
  llPalStat,llClrSel,llShwOpn,llEdiopn,llB_H_Opn,llB_D_Opn,llDyelot,llMulWare,;
  llNew,llPckCopy,llNoShow,llSelOrdLi,llUnComp,llCupdate,llAnyUpd,llAnyRec,;
  llPrint,llPakPrSiz,llQtyArr,llWghArr,llUpdtPkTk,llStyFltr,llPckFltr,llClrFltr,llFilter,llDetLabel,llUPCInst

lcSndPort = "COM2"
STORE '' TO laStySrc,laStyTrgt,laOldSty,laClrSrc, laClrTrgt,laOldClr,lcPackNo,lcPckLin,lcTmpStores,lcTmPckLin,lcPackLines,;
  lcCtnHdr,lcStyPic,lcStyTtl,lcDistCtr,lcBol,lcCtnDtl,lcTmCtnDtl,lcfoldprnt,lcFree_Clr,;
  lcOldVal,lcCtnSty,lcStySz,lcLsVPck,lcForceAlo,lcSizeSep,;
  lcTmpDetFl,lcBollsttl,lcTmAsnShp,lcWareCode,STYLE,STYDYE,Customer,EDICRTSQ,;
  Pack_Hdr,OrdHdr,PikTkt,EDIACPRT,EDIPH,PikLine,SCALE,BOL_HDR,BOL_LIN,lcPLSortBy,;
  OrdLine,WareHous,CUSTDEPT,SPCK_LIN,SPck_Hdr,Pack_Lin,lcOrdNo,lcStoreNo,lcTempCur,;
  Asn_Ship,SYCASNLB,STYLEUPC,lcDetLbAll,lcDetailVr,lcPrvPack

*E302315,1 WLD 10/16/2006 Using visual label report - Check for UCC # structure setup [Begin]
STORE '' TO SYCASNHD,SYCASNDT,laLblInfo,lcPrnAsnShp,SETUPS,SYCCONFG
STORE .F. TO llIncPLNum
STORE 5  TO lnNumOfDig
DIMENSION laLblInfo[1, 6]
*E302315,1 WLD 10/16/2006 Using visual label report [End]
*C200945,1 WLD Fix  bug of pressing enter when Scan using the Upc  (PRG,Screen) 02/21/2008 [Begin]
STORE .F. TO llConfirm          && If set confirmation is on or off.
STORE "" TO lcSetConf,lcCnfrm   && Variable hold the confirmation setting.
*C200945,1 WLD [End]

STORE 0 TO lnStyleWid,lnRelCho,lnTotWgh,lnPalNo,lnCtnPal,lnStyQty,lnStyWgh,lnCartNo,;
  lnTo,lnMaxPal,lnMinPal,lnHdBrRec,lnDtBrRec,lnCtHdBrRe,lnCtDtBrRe,lnFrom,;
  lnPackWgh,lnPackQty,lnPackCtn,lnBrCtnQty,lnLastNo,lnMaxCtn,lnLastCtn,;
  lnBrUntWgh,lnOrdMdPr,lnDumCtnR, lnNonMajSt, lnMajLen , lnColorLen

STORE 2 TO lnCtnTyp
STORE 1 TO lnDrctTo
STORE .T. TO llShoPckSu
IF FILE (oAriaApplication.DataDir+'UpdtPkTk.MEM')
  RESTORE FROM (oAriaApplication.DataDir+'UpdtPkTk.MEM') ADDITIVE
ENDIF

*E302315,1 WLD 10/16/2006 Add new setup of including P\L number in UCC [Begin]
*DIMENSION laSetup[11,2]
*C200945,1 WLD Fix  bug of pressing enter when Scan using the Upc  (PRG,Screen) 02/21/2008 [Begin]
*DIMENSION laSetup[12,2]
DIMENSION laSetup[13,2]
*C200945,1 WLD Fix  bug of pressing enter when Scan using the Upc  (PRG,Screen) 02/21/2008 [End]

*E302315,1 WLD 10/16/2006 Add new setup of including P\L number in UCC [End]

laSetUp[1,1]  = 'M_DYELOT'
laSetUp[2,1]  = 'M_CANAFTER'
laSetUp[3,1]  = 'M_WareHouse'
laSetUp[4,1]  = 'XMANUFID'
laSetUp[5,1]  = 'M_FORCEALO'
laSetUp[6,1]  = 'M_BOLSTUTS'
laSetUp[7,1]  = 'M_PAKPRSIZ'
laSetUp[8,1]  = 'M_STYCNFG'
laSetUp[9,1]  = 'M_USEEXSSC'
laSetUp[10,1] = 'M_ORDSTUTS'
laSetUp[11,1] = 'M_PACK'
*E302315,1 WLD 10/16/2006 Add new setup of including P\L number in UCC [Begin]
laSetUp[12,1] = 'M_UCCBSDON'
*E302315,1 WLD 10/16/2006 Add new setup of including P\L number in UCC [End]
*C200945,1 WLD Fix  bug of pressing enter when Scan using the Upc  (PRG,Screen) 02/21/2008 [Begin]
laSetUp[13,1] = 'M_CONFIRM'
*C200945,1 WLD Fix  bug of pressing enter when Scan using the Upc  (PRG,Screen) 02/21/2008 [End]

=gfGetMemVar(@laSetUp,oAriaApplication.ActiveCompanyID)

llDyelot   = ALLTRIM(laSetUp[1,2]) = 'Y'
lnOrdMdPr  = laSetUp[2,2]
*-- Get Status For The M_PRNLABEL And llCanPrnLb [Begin]
llMulWare  = ALLTRIM(laSetUp[3,2]) = 'Y'
lcManufId  = ALLTRIM(laSetUp[4,2])
llCanPrnLb = gfUserPriv('AL','ALPLIST','PRNPACKING')
lcForceAlo = ALLTRIM(laSetUp[5,2])
*-- Add flag to hold the valu of the setup (M_BOLSTUTS) [Begin]
llBolnever = (ALLTRIM(laSetup[6,2]) = "N")
*-- Get the setting of generate B.O.L Automatic [begin]
llBolAutom = (ALLTRIM(laSetup[6,2]) = "A")
*-- Add flag to hold the value of the setup (M_PAKPRSIZ) [Begin]
llPakPrSiz = laSetup[7,2]
*-- Get the configuration setup [Begin]
llUseConfg = (ALLTRIM(laSetup[8,2]) = "Y")
*-- Get the extended size scale setup
llExtSizSc = laSetup[9,2]
*-- Packing list sort by
lcPLSortBy = ALLTRIM(laSetup[10,2])
*-- Use style packs\Sku
llUsePack = (ALLTRIM(laSetup[11,2]) = "Y")
*E302315,1 WLD 10/16/2006 Using visual label report [Begin]
lnNumOfDig = EVAL(laSetup[12,2])
*E302315,1 WLD 10/16/2006 Using visual label report [End]
*C200945,1 WLD Fix  bug of pressing enter when Scan using the Upc  (PRG,Screen) 02/21/2008 [Begin]
llConfirm  = (ALLTRIM(laSetup[13,2]) = "Y")
*C200945,1 WLD [End]

*-- Check the existance of the EDI module
llAsnSys   = ('AS' $ oAriaApplication.CompanyInstalledModules)
llEdiSys   = ('AS' $ oAriaApplication.CompanyInstalledModules)
lcMask     = gfItemMask("PM", '', '0001')
*-- Check if the user has an access to force allocation
llAlwForce = lfSuppForc()

STORE 0 TO lnSizePos,lnSizeLen
IF llExtSizSc
  DECLARE laStySeg[1,1]
  STORE "" TO laStySeg
  =gfItemMask(@laStySeg)
  FOR lnCnt = 1 TO ALEN(laStySeg,1)
    IF laStySeg[lnCnt , 1] = "S"
      lcSizeSep  = ALLTRIM(laStySeg[lnCnt-1 , 6])
      lnSizePos  = laStySeg[lnCnt , 4] - IIF(!EMPTY(lcSizeSep) , 1 , 0)
      lnSizeLen  = LEN(laStySeg[lnCnt , 3]) + IIF(!EMPTY(lcSizeSep) , 1 , 0)
    ENDIF
  ENDFOR
ENDIF

lcAsnLabel  = gfTempName()
lcTmAsnShp  = gfTempName()
lcTmpDetFl  = gftempName()
lcPckLin    = gfTempName()
lcTmpStores = gfTempName()
lcTmPckLin  = gfTempName()
lcPackLines = gfTempName()
lcCtnHdr    = gfTempName()
lcCtnDtl    = gfTempName()
lcTmCtnDtl  = gfTempName()
lcTmpIdx    = gfTempName()
lcSumPck    = gfTempName()
lcPakIndxSt = gfTempName()
lcPakIndxLn = gfTempName()
lcTempCur   = gfTempName()
lcTmpScpSty = gfTempName()
lcTmpScpPck = gfTempName()
lcScpStyFlt = gfTempName()
lcScpPckFlt = gfTempName()
*E302315,1 WLD 10/16/2006 Using visual label report [Begin]
lcPrnAsnShp  = gfTempName()
*E302315,1 WLD 10/16/2006 Using visual label report [End]

=lfEvalSegs()

*E302507,1 WLD 03/17/2008 Call Packing List screen by parameter [Begin]
*DO FORM (oAriaApplication.ScreenHome+'ALPLIST.SCX')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'ALPLIST.SCX') WITH lcPackLsNo
=gfCallForm('ALPLIST',.F.,"lcPackLsNo")
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
*E302507,1 WLD 03/17/2008 Call Packing List screen by parameter [End]


*!*************************************************************
*! Name      : lfInit
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : To do all the necessary actions to start using the form
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
*! Called    : From Init in the formset
*!*************************************************************
FUNCTION lfInit
LPARAMETERS loFormSet
LOCAL laIndex

*-- To initialize the variables as properties in the formset.
=lfAddPro(loFormSet)

llModInst = ('AL' $ oAriaApplication.CompanyInstalledModules .OR. ;
  'AS' $ oAriaApplication.CompanyInstalledModules)

IF !llModInst
  *-- XX module is not installed. Cannot proceed.
  =gfModalGen('TRM42083B00000','DIALOG',LANG_ManulPL_MsgModInst)
  RETURN .F.
ENDIF

RETURN .T.

*!*************************************************************
*! Name      : lfAddPro
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 11/07/2004
*! Purpose   : function to Add properties to the FormSet.
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
FUNCTION lfAddPro
LPARAMETERS loFormSet

LOCAL lnI
FOR lnI = 1 TO ALEN(laAllVar,1)
  IF LEFT(laAllVar[lnI,1],2) = 'la'
    LOCAL lnRow,lnCol,lcACopy
    lnRow = ALEN(laAllVar[lnI,1],1)
    lnCol = ALEN(laAllVar[lnI,1],2)
    loFormSet.ADDPROPERTY(laAllVar[lnI,1]+'['+ALLTRIM(STR(lnRow))+;
      ','+ALLTRIM(STR(lnCol))+']')
    lcACopy = '=ACOPY(' + laAllVar[lnI,1] + ',loFormSet.' + laAllVar[lnI,1] + ')'
    &lcACopy.
  ELSE
    loFormSet.ADDPROPERTY(laAllVar[lnI,1],EVALUATE(laAllVar[lnI,1]))
  ENDIF
ENDFOR
*--end of lfAddPro.

*!*************************************************************
*! Name      : lfAfterInit
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 09/11/2004
*! Purpose   : To open the necessary files and set the
*!			   necessary relations
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
*! Called    : From Init in the formset
*!*************************************************************
FUNCTION lfAfterInit
LPARAMETERS loFormSet
PRIVATE lcSqlStatement, laIndex

DIMENSION loFormSet.laStySrc[1],loFormSet.laStyTrgt[1],loFormSet.laOldSty[1],;
  loFormSet.laClrSrc[1],loFormSet.laClrTrgt[1],loFormSet.laOldClr[1]
*-- Open the necessary files
SET MULTILOCKS ON
=lfOpenFiles(loFormSet)
loFormSet.nWorkArea = "Pack_Hdr"
loFormSet.DATAENVIRONMENT.INITIALSELECTEDALIAS = "Pack_Hdr"

*! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[Start]
IF !loFormSet.llUPCInst
  loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.VISIBLE = .F.
  loFormSet.ariaForm1.pgfPacking.CartonInfo.cmdScan.VISIBLE = .F.
ENDIF
*! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[End]

loFormSet.ariaForm1.pgfPacking.DETAIL.ENABLED = .F.
loFormSet.ariaForm1.pgfPacking.CartonInfo.ENABLED = .F.
loFormSet.ariaForm1.pgfPacking.HEADER.cboCntType.ROWSOURCE = LANG_ManulPL_CtnType
loFormSet.ariaForm1.pgfPacking.HEADER.cboDirectTo.ROWSOURCE = LANG_ManulPL_DirectTo
loFormSet.lcStyTtl = loFormSet.ariaForm1.pgfPacking.CartonInfo.kbStyle.lcItemHeader
=lfCrTmpFiles(loFormSet)

SELECT STYLE
=AFIELDS(laFileStru)
DECLARE laIndeces[1,2]
laIndeces[1,1] = 'cStyMajor'
laIndeces[1,2] = loFormSet.lcTmpScpSty
=gfCrtTmp(loFormSet.lcTmpScpSty,@laFileStru, @laIndeces)

SELECT SPCK_HDR
=AFIELDS(laFileStru)
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,18]
laFileStru[lnI,1] = 'cPSIZE'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,18]
laFileStru[lnI,1] = 'cPDesc'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 20
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,18]
laFileStru[lnI,1] = 'CUPCNUM1'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,18]
laFileStru[lnI,1] = 'CUPCNUM2'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,18]
laFileStru[lnI,1] = 'CUPCNUM3'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 2
laFileStru[lnI,4] = 0

FOR lnCount = 1 TO ALEN(laFileStru,1)
  STORE '' TO laFileStru[lnCount,7],laFileStru[lnCount,8],laFileStru[lnCount,9],;
    laFileStru[lnCount,10],laFileStru[lnCount,11],laFileStru[lnCount,12],;
    laFileStru[lnCount,13],laFileStru[lnCount,14],laFileStru[lnCount,15],;
    laFileStru[lnCount,16]
  STORE 0 TO  laFileStru[lnCount,17],laFileStru[lnCount,18]
ENDFOR

DECLARE laIndeces[1,2]
laIndeces[1,1] = 'Pack_id+cPkColor+cPckSize+cPkVersion'
laIndeces[1,2] = loFormSet.lcTmpScpPck
=gfCrtTmp(loFormSet.lcTmpScpPck,@laFileStru, @laIndeces)

*-- Set relations
SELECT PikTkt
loFormSet.PikTkt.SetOrder('OrdPik')

SELECT Pack_Hdr
loFormSet.Pack_Hdr.SetOrder('Pack_Hdr')
SET RELATION TO 'O'+ORDER INTO OrdHdr

loFormSet.STYLE.SetOrder('Style')

SELECT OrdLine
loFormSet.OrdLine.SetOrder('OrdLinSt')
SET FILTER TO cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = "O"
IF EMPTY(SET("RELATION"))
  SET RELATION TO 'S'+SCALE INTO SCALE ADDITIVE
  SET RELATION TO STYLE INTO STYLE ADDITIVE
ENDIF
loFormSet.Pack_Lin.SetOrder('PackStyle')
SELECT Pack_Lin

*B608456,1 WAM 02/27/2008 Fix error variable oFormset not found when copy from another packing list
*B608456,1 WAM 02/27/2008 Relation between PACK_LIN and ORDLINE is not needed
*!*	IF EMPTY(SET("RELATION"))
*!*	  SET RELATION TO 'O'+;
*!*	    loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE+;
*!*	    loFormSet.ariaForm1.kbStore.keytextbox.VALUE+STYLE INTO OrdLine ADDITIVE
*!*	ENDIF
*B608456,1 WAM 02/27/2008 (End)

SELECT Pikline
SET RELATION TO 'O'+Pikline.ORDER+Pikline.STORE+Pikline.STYLE+STR(Pikline.LINENO,6) INTO ORDLINE ADDITIVE
SELECT PikTkt
SET RELATION TO Piktkt.piktkt INTO Pikline ADDITIVE

IF loFormSet.llCanPrnLb
  *! B610733,1 HES 05/26/2014 Fix a bug happened while releasing shortages in P/L [START]
*!*	  loFormSet.Asn_Ship = CREATEOBJECT("RemoteTable",'Asn_Ship','Asn_Ship','Asn_Ship',loFormSet.DATASESSIONID)
*!*	  SELECT Asn_Ship
*!*	  IF !USED(loFormSet.lcTmAsnShp)
*!*	    COPY STRUCTURE TO (oAriaApplication.WorkDir+loFormSet.lcTmAsnShp)
*!*	    =gfOpenFile(oAriaApplication.WorkDir+loFormSet.lcTmAsnShp,'','EX')
*!*	    SELECT (loFormSet.lcTmAsnShp)
*!*	    INDEX ON STR(CART_NO,6) TAG (loFormSet.lcTmAsnShp)
*!*	  ENDIF
  *! B610733,1 HES 05/26/2014 Fix a bug happened while releasing shortages in P/L [END  ]
  *E302315,1 WLD 10/16/2006 Using visual label report [Begin]
  loFormSet.SYCASNHD = CREATEOBJECT("RemoteTable",'SYCASNHD','VerPrt','SYCASNHD',loFormSet.DATASESSIONID)
  loFormSet.SYCASNDT = CREATEOBJECT("RemoteTable",'SYCASNDT','CVer','SYCASNDT',loFormSet.DATASESSIONID)
  *E302315,1 WLD 10/16/2006 Using visual label report [End  ]

  loFormSet.SYCASNLB = CREATEOBJECT("RemoteTable",'SYCASNLB','ASNlbl','SYCASNLB',loFormSet.DATASESSIONID)
  *E302315,1 WLD 10/16/2006 Using visual label report [Begin]
  *loFormSet.llDetLabel = loFormSet.SYCASNLB.SEEK("XX1" + "H") .AND. loFormSet.SYCASNLB.SEEK("XX1" + "L")

  SELECT SYCASNHD
  LOCATE FOR SYCASNHD.lDetLabel = .T.
  IF FOUND()
    loFormSet.llDetLabel = .T.
  ENDIF
  *E302315,1 WLD 10/16/2006 Using visual label report [End]
  IF loFormSet.llDetLabel
    *-- Flag to know if the user want to print detailed shipping label for all cartons or not
    loFormSet.lcDetLbAll = ""
  ENDIF
ENDIF

*! B610733,1 HES 05/26/2014 Fix a bug happened while releasing shortages in P/L [START]
loFormSet.Asn_Ship = CREATEOBJECT("RemoteTable",'Asn_Ship','Asn_Ship','Asn_Ship',loFormSet.DATASESSIONID)
SELECT Asn_Ship
IF !USED(loFormSet.lcTmAsnShp)
  COPY STRUCTURE TO (oAriaApplication.WorkDir+loFormSet.lcTmAsnShp)
  =gfOpenFile(oAriaApplication.WorkDir+loFormSet.lcTmAsnShp,'','EX')
  SELECT (loFormSet.lcTmAsnShp)
  INDEX ON STR(CART_NO,6) TAG (loFormSet.lcTmAsnShp)
ENDIF
*! B610733,1 HES 05/26/2014 Fix a bug happened while releasing shortages in P/L [END  ]

*E302315,1 WLD 10/16/2006 Add new setup of including P\L number in UCC [Begin]
llDispMsg  = .F.
loFormSet.SETUPS   = CREATEOBJECT("RemoteTable",'SETUPS','MODVAR','SETUPS',loFormSet.DATASESSIONID)
loFormSet.SYCCONFG = CREATEOBJECT("RemoteTable",'SYCCONFG','MODVAR','SYCCONFG',loFormSet.DATASESSIONID)

IF  loFormSet.SYCCONFG.SEEK('ALM_UCCBSDON') AND EVAL(ALLTRIM(SYCCONFG.mData_def)) = 0
  lcTitle  = 'Select UCC # Structure'
  *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
  *DO FORM (oAriaApplication.ScreenHome+'ALUCCSTR.SCX') TO loFormSet.lnNumOfDig
  loFormCall = loFormSet
  =gfCallForm('ALUCCSTR',.F.,.F.,"loFormCall.lnNumOfDig")
  *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
  m.mData_def = ALLTRIM(STR(loFormSet.lnNumOfDig))
  loFormSet.SYCCONFG.REPLACE('mVentries WITH "5 Digits of PL # + 4 Digits for Carton #|6 Digits of PL # + 3 Digits for Carton #|9 Digits for Carton #~5|6|9",mData_def WITH m.mData_def')
  IF loFormSet.SETUPS.SEEK('ALM_UCCBSDON')
    loFormSet.SETUPS.REPLACE('mVentries WITH SYCCONFG.mVentries ,mData_def WITH m.mData_def')
  ELSE
    SELECT SYCCONFG
    SCATTER MEMVAR MEMO
    SELECT SETUPS
    m.mData_def = ALLTRIM(STR(loFormSet.lnNumOfDig))
    INSERT INTO 'SETUPS' FROM MEMVAR
  ENDIF
  DIMENSION laTableUpdate[2]
  STORE "" TO laTableUpdate
  laTableUpdate[1] = loFormSet.SYCCONFG
  laTableUpdate[2] = loFormSet.SETUPS
  IF !lfTableUpdate()
    RETURN .F.
  ENDIF
ELSE
  loFormSet.lnNumOfDig = EVAL(laSetup[12,2])
ENDIF
loFormSet.llIncPLNum = (loFormSet.lnNumOfDig = 5 OR loFormSet.lnNumOfDig = 6)
*E302315,1 WLD 10/16/2006 Add new setup of including P\L number in UCC [End]
*C200945,1 WLD Fix  bug of pressing enter when Scan using the Upc  (PRG,Screen) 02/21/2008 [Begin]
loFormSet.lcSetConf  = SET('CONFIRM')	 && Save the confirmation setting.
*C200945,1 WLD [End]
*!*************************************************************
*! Name      : lfShow
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : Show Screen Mode Changes (Select and Add) only.
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet, lcScrMode-> Screen Mode
*!*************************************************************
*! Called    : From ChangeMode in the formset
*!*************************************************************
FUNCTION lfShow
LPARAMETERS loFormSet, lcScrMode
DO CASE
  *-- S E L E C T  M O D E.
CASE lcScrMode = 'S'
  SELECT Pack_HDR
  *E037427,1 WAM 05/05/2005 Enhance performance
  *DELETE FOR EMPTY(Pack_no)
  *E037427,1 WAM 05/05/2005 (End)
  DIMENSION laTableUpdate[1]
  laTableUpdate[1] = loFormSet.Pack_HDR
  *E037427,1 WAM 05/05/2005 Enhance performance
  *=lfTableUpdate()
  *E037427,1 WAM 05/05/2005 (End)

  loFormSet.AriaForm1.pgfPacking.DETAIL.grdDetail.RECORDSOURCE = ''
  loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonH.RECORDSOURCE = ''
  loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonD.RECORDSOURCE = ''
  loFormSet.llNew = .F.
  *-- Creating the temp. files
  =lfCrTmpFiles(loFormSet)
  *-- Unlock the selected order lines
  *E037427,1 WAM 05/05/2005 The undo method clears order lock
  *=lfUnLock(loFormSet)
  *E037427,1 WAM 05/05/2005 (End)

  STORE SPACE(0) TO loFormSet.lcDistCtr,loFormSet.lcBol,loFormSet.lcPackNo,;
    loFormSet.lcCtnSty,loFormSet.lcStySz,loFormSet.lcLsVPck,;
    loFormSet.lcWareCode,loFormSet.lcOrdNo,loFormSet.lcStoreNo,loFormSet.lcPrvPack

  STORE 0 TO loFormSet.lnRelCho,loFormSet.lnTotWgh,loFormSet.lnPalNo,;
    loFormSet.lnCtnPal,loFormSet.lnStyQty,loFormSet.lnStyWgh,loFormSet.lnCartNo,;
    loFormSet.lnHdBrRec,loFormSet.lnDtBrRec,loFormSet.lnCtHdBrRe,loFormSet.lnCtDtBrRe,;
    loFormSet.lnFrom,loFormSet.lnTo,loFormSet.lnMaxPal,loFormSet.lnMinPal,;
    loFormSet.lnLastNo,loFormSet.lnMaxCtn,loFormSet.lnLastCtn,loFormSet.lnPackWgh,;
    loFormSet.lnPackQty,loFormSet.lnPackCtn,loFormSet.lnBrCtnQty,;
    loFormSet.lnBrUntWgh,loFormSet.lnCtnTyp, loFormSet.lnDrctTo
  STORE .F. TO loFormSet.llPCDStat,loFormSet.llPalStat,loFormSet.llClrSel,;
    loFormSet.llShwOpn,loFormSet.llNew,loFormSet.llPckCopy,;
    loFormSet.llSelOrdLi,loFormSet.llAnyUpd,loFormSet.llAnyRec,;
    loFormSet.llQtyArr,loFormSet.llWghArr,loFormSet.llStyFltr , loFormSet.llPckFltr , loFormSet.llClrFltr , loFormSet.llFilter
  loFormSet.llShoPckSu = .T.
  IF TYPE('_OPTPOP') = 'O'
    SET MARK OF BAR 1 OF _OPTPOP TO .F.
    SET MARK OF BAR 2 OF _OPTPOP TO .F.
    SET MARK OF BAR IIF(loFormSet.llCanPrnLb,6,4) OF _OPTPOP TO IIF(loFormSet.llUpdtPkTk,.T.,.F.)
  ENDIF
  DIMENSION loFormSet.laStyTrgt[1],loFormSet.laClrTrgt[1]
  STORE SPACE(0) TO loFormSet.laStyTrgt,loFormSet.laClrTrgt
  IF USED(loFormSet.lcPckLin)
    =lfDtlBrow(loFormSet)
  ENDIF

  *-- V I E W  M O D E.
CASE lcScrMode = "V"
  SELECT Pack_Hdr
  STORE nLastCart TO loFormSet.lnMaxCtn
  STORE nLastCart+1 TO loFormSet.lnFrom,loFormSet.lnTo
  lcAccount = PADR(loFormSet.ariaForm1.kbAccount.keytextbox.VALUE ,5)
  loFormSet.llEdiAcc = loFormSet.llEdiSys .AND. loFormSet.EDIACPRT.SEEK('A'+lcAccount) .AND.;
    loFormSet.EDIPH.SEEK(EDIACPRT.cPartCode)
  STORE loFormSet.llEdiSys AND loFormSet.llEdiAcc .AND. EDIACPRT.lPkChrDes TO loFormSet.llPCDStat
  STORE loFormSet.llEdiSys AND loFormSet.llEdiAcc .AND. EDIPH.lPltShp  TO loFormSet.llPalStat
  loFormSet.AriaForm1.kbPackNo.keytextbox.VALUE =  Pack_Hdr.Pack_no
  *-- Pack information. Keys and Header folder
  =lfPckInfo(loFormSet)
  *E037427,1 WAM 05/05/2005 Refresh screen after undo
  =lfvSelOrdL(loFormSet)
  *E037427,1 WAM 05/05/2005 (End)

  *! B609245,1 HES 05/12/2010 style module - Incorrect weights in allocation screens [Start]
  && Refreshing the Weight field
  *B609768,1 MMT 12/12/2011 Cannot add new code for carton type in manual PL Screen[Start]
  *=lfwCtnDtlBr(loFormSet)
  =lfwCtnHdrBr(loFormSet)
  *B609768,1 MMT 12/12/2011 Cannot add new code for carton type in manual PL Screen[ENd]
  *! B609245,1 HES 05/12/2010 style module - Incorrect weights in allocation screens [End  ]

  *E302315,1 WLD 10/16/2006 Using visual label report [Begin]
  IF loFormSet.llEdiSys
    loFormSet.BOL_HDR.SEEK(Pack_Hdr.Bill_Ladg)
  ENDIF
  lcAccount = PADR(loFormSet.ariaForm1.kbAccount.keytextbox.VALUE ,5)
  loFormSet.llEdiAcc = loFormSet.llEdiSys .AND. loFormSet.EDIACPRT.SEEK('A'+lcAccount) .AND.;
    loFormSet.EDIPH.SEEK(EDIACPRT.cPartCode)
  STORE loFormSet.llEdiSys AND loFormSet.llEdiAcc .AND. EDIACPRT.lPkChrDes TO loFormSet.llPCDStat
  STORE loFormSet.llEdiSys AND loFormSet.llEdiAcc .AND. EDIPH.lPltShp  TO loFormSet.llPalStat

  *E302315,1 WLD 10/16/2006 Using visual label report [End]
  *-- E D I T  M O D E.
CASE lcScrMode = "E"
  IF Pack_Hdr.STATUS = 'C'
    *: B610016,1 SAB 07/16/2012 Enable Editing Notes and Special Inst. for Completed Packs [Start]
    *PRIVATE lcMessage
    **-- This packing list is shipped. Can't be Deleted
    **-- OK
    *lcMessage = LANG_ManulPL_MsgCntCancel
    *=gfModalGen("INM44102B00000","Dialog",lcMessage)
    *lcScrMode = 'V'
    *SELECT Pack_Hdr
    **=lfObj_Lock(.F.,loformset.Pack_Hdr,loformset,'Pack_Hdr')
    *=gfObj_Lock(.F.)

    **E037427,1 WAM 05/05/2005 Do not lock order if pack lock failed
    **llGoOn = lfChkOrdLok(loFormSet)
    **E037427,1 WAM 05/05/2005 (End)
    *loFormSet.AlterMode(lcScrMode)
    loFormset.AriaForm1.SETALL('ENABLED', .F., 'AriaTextBox')
    loFormset.AriaForm1.SETALL('ENABLED', .F., 'AriaKeyField')
    loFormset.AriaForm1.pgfPacking.HEADER.SETALL('ENABLED', .F., 'AriaTextBox')
    loFormset.AriaForm1.pgfPacking.HEADER.SETALL('ENABLED', .F., 'AriaCodes')
    loFormset.AriaForm1.pgfPacking.HEADER.SETALL('ENABLED', .F., 'AriaValidEntriesComboBox')
    loFormset.AriaForm1.pgfPacking.HEADER.SETALL('ENABLED', .F., 'AriaCheckBox')
    loFormset.AriaForm1.pgfPacking.DETAIL.SETALL('ENABLED', .F., 'AriaTextBox')
    loFormset.AriaForm1.pgfPacking.DETAIL.SETALL('ENABLED', .F., 'AriaCommandButton')
    loFormset.AriaForm1.pgfPacking.CartonInfo.SETALL('ENABLED', .F., 'AriaTextBox')
    loFormset.AriaForm1.pgfPacking.CartonInfo.SETALL('ENABLED', .F., 'AriaCommandButton')
    WITH loFormset.AriaForm1
      STORE .T. TO .pgfPacking.HEADER.txtNoteH.ENABLED,.pgfPacking.HEADER.txtSpInst1.ENABLED,;
        .pgfPacking.HEADER.txtSpInst2.ENABLED
    ENDWITH
    *: B610016,1 SAB 07/16/2012 Enable Editing Notes and Special Inst. for Completed Packs [End]

    RETURN
  ENDIF
  lcScrMode = "E"
  *-- check if any pack is being created now on the same order in any
  *-- other session
  llGoOn = lfChkOrdLok(loFormSet)
  IF !llGoOn
    loFormSet.AlterMode('V')
  ENDIF

  *-- A D D  M O D E.
CASE lcScrMode = "A"
  STORE .F. TO loFormSet.llStyFltr , loFormSet.llPckFltr , loFormSet.llClrFltr , loFormSet.llFilter
  IF !loFormSet.llNew
    *-- means carton type is standard
    loFormSet.lnCtnTyp = 2
    *-- means pack is directed to store
    loFormSet.lnDrctTo = 1
  ENDIF
ENDCASE
*-- Set the object status
=lfObjStatus(loFormSet,lcScrMode)

RETURN

*!*************************************************************
*! Name      : lfvPackNo
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : To validate the pack no.
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
FUNCTION lfvPackNo
LPARAMETERS loFormSet,lcPackNo,llBrowse
PRIVATE lnCurAlias,lcPCkHdrTag

SELECT Pack_Hdr
lcPCkHdrTag = ORDER('Pack_Hdr')
loFormSet.Pack_Hdr.SetOrder('Pack_Hdr')

IF llBrowse OR (!EMPTY(lcPackNo) AND !loFormSet.Pack_Hdr.SEEK(lcPackNo))
  lcPackNo = IIF(lfPakNoBrw(loFormSet),Pack_Hdr.Pack_No,SPACE(6))
ENDIF

loFormSet.Pack_Hdr.SetOrder(lcPCkHdrTag)
RETURN lcPackNo

*!*************************************************************
*! Name      : lfPakNoBrw
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : To browse the packs.
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
FUNCTION lfPakNoBrw
LPARAMETERS loFormSet

PRIVATE lcFields,laBrow,lnCurAlias,lcCurTag,llReturn,lcTag,lcBrFields,lcFile_Ttl,;
  lcOrder, lcAccount, lcSeekExpr
DIMENSION laBrow[1]
STORE SPACE(0) TO lcFields,laBrow
llReturn   = .F.

lcOrder    = loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE
lcAccount  = loFormSet.ariaForm1.kbAccount.keytextbox.VALUE

lnCurAlias = SELECT(0)

lcFields    = "Pack_No,Order,Account,Store,Note,Tot_Wght"
lcBrFields  = [Pack_No  :H=LANG_ManulPL_lblPackNo,]+;
  [Order    :H=LANG_ManulPL_lblOrderNo,]+;
  [Account  :H=LANG_ManulPL_lblAccount,]+;
  [Store    :H=LANG_ManulPL_lblStoreNo,]+;
  [Bill_Ladg:H=LANG_ManulPL_lblBOL,]+;
  [Tot_Wght :H=LANG_ManulPL_lblTotWght]
lcFile_Ttl  = LANG_ManulPL_hdrPckSlip
SELECT Pack_Hdr
lcCurTag = ORDER('Pack_Hdr')
*lcTag = IIF((EMPTY(lcOrder) AND !EMPTY(lcAccount) OR loFormSet.llPckCopy),"AccPack","OrderPck")
DO CASE
CASE (EMPTY(lcOrder) AND !EMPTY(lcAccount)) OR loFormSet.llPckCopy
  lcTag = "AccPack"
CASE !EMPTY(lcOrder)
  lcTag = "OrderPck"
OTHERWISE
  lcTag = 'Pack_Hdr'
ENDCASE
loFormSet.Pack_Hdr.SetOrder(lcTag)
lcSeekExpr = IIF(UPPER(ORDER('Pack_Hdr'))="ACCPACK",lcAccount,ALLTRIM(lcOrder))
IF !loFormSet.Pack_Hdr.SEEK(lcSeekExpr)
  *-- There are no records to browse.
  *-- OK
  =gfModalGen("INM44032B00000","Dialog")
ELSE
  llReturn = AriaBrow(IIF(ORDER('Pack_Hdr')="ACCPACK","lcAccount","ALLTRIM(lcOrder)"),;
    lcFile_Ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,;
    lcFields,"laBrow",.F.,'Pack_Hdr',.F.)
ENDIF
loFormSet.Pack_Hdr.SetOrder('Pack_Hdr')
SELECT(lnCurAlias)
RETURN llReturn

*!*************************************************************************
*! Name      : lfPckInfo
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : To get the pack information
*!*************************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************************
FUNCTION lfPckInfo
LPARAMETERS loFormSet

*E037427,1 WAM 05/05/2005 Fix bug of delete carton lines
loFormSet.lcOrdNo = Pack_Hdr.ORDER
*E037427,1 WAM 05/05/2005 (End)
WITH loFormSet.AriaForm1
  *-- Order No.
  .kbOrderNo.keytextbox.VALUE = Pack_Hdr.ORDER
  *-- Pick Ticket No.
  .kbPkTktNo.keytextbox.VALUE = Pack_Hdr.PikTkt
  *-- Account Code
  .kbAccount.keytextbox.VALUE = Pack_Hdr.Account
  *-- Store Code
  .kbStore.keytextbox.VALUE = Pack_Hdr.STORE
  *-- Account Name
  loFormSet.Customer.SEEK('M'+Pack_Hdr.Account)
  .txtCustName.VALUE = Customer.stName
  *-- Store Name
  loFormSet.Customer.SEEK('S'+Pack_Hdr.Account+Pack_Hdr.STORE)
  .txtStoreName.VALUE = Customer.stName
  *-- Get Cust P.O #
  loFormSet.OrdHdr.SEEK('O'+Pack_Hdr.ORDER)
  IF OrdHdr.MultiPo
    LOCAL lcTag
    lcTag = ORDER('OrdLine')
    loFormSet.OrdLine.SetOrder('ORDLINST')
    loFormSet.OrdLine.SEEK('O'+Pack_Hdr.ORDER+Pack_Hdr.STORE)
    .txtCustPo.VALUE = OrdLine.CustPo
    loFormSet.OrdLine.SetOrder(lcTag)
  ELSE
    .txtCustPo.VALUE = OrdHdr.CustPo
  ENDIF

  *-- Header Information --*
  *-- Notes
  .pgfPacking.HEADER.txtNoteH.CONTROLSOURCE = "Pack_Hdr.Note"
  *-- ShipVia
  .pgfPacking.HEADER.cboShipViaH.CONTROLSOURCE = "Pack_Hdr.ShipVia"
  *-- Bill of Lading
  *B132257,1 HBG 05/21/2006 Fix bug of not updating Qty,Crt.Wieght in BOL_HDR file [Begin]
  *.pgfPacking.header.txtBOL.ControlSource = "Pack_Hdr.Bill_Ladg"
  .pgfPacking.HEADER.txtBOL.VALUE = Pack_Hdr.Bill_Ladg
  *B132257,1 HBG 05/21/2006 [End]
  *-- Department
  .pgfPacking.HEADER.txtDept.VALUE = OrdHdr.Dept
  *-- Total Weight
  .pgfPacking.HEADER.txtWeight.VALUE =  Pack_Hdr.Tot_Wght
  *-- Total Cartons
  .pgfPacking.HEADER.txtCartons.VALUE = Pack_Hdr.Tot_Cart
  *-- Total Pieces
  .pgfPacking.HEADER.txtPieces.VALUE =  Pack_Hdr.Tot_Pcs

  *-- Special Instructions 1
  .pgfPacking.HEADER.txtSpInst1.CONTROLSOURCE = "Pack_Hdr.Sp_Inst1"
  *-- Special Instructions 2
  .pgfPacking.HEADER.txtSpInst2.CONTROLSOURCE = "Pack_Hdr.Sp_Inst2"
  *-- Cnt Type
  loFormSet.lnCtnTyp = IIF(Pack_Hdr.LStandCtn,1,2)
  *-- Direct To
  loFormSet.lnDrctTo = IIF(Pack_Hdr.CToStorCn='C',2,1)
  *-- Pack Char Code
  .pgfPacking.HEADER.txtPackCharCode.CONTROLSOURCE = "Pack_Hdr.cPkChCode"
  *-- Pack Char Desc
  .pgfPacking.HEADER.txtPackDescCode.CONTROLSOURCE = "Pack_Hdr.cPkDsCode"

ENDWITH
*E037427,1 WAM 05/05/2005 Initialize pack total cartons, weight, and pieces
loFormSet.lnPackWgh = Pack_Hdr.Tot_Wght
loFormSet.lnPackCtn = Pack_Hdr.Tot_Cart
loFormSet.lnPackQty = Pack_Hdr.Tot_Pcs
*E037427,1 WAM 05/05/2005 (End)
*B608364,1 WAM 11/28/2007 Check BOL assigned to packing list when the shipvia changed
*B608364,1 WAM 11/28/2007 Load formset property
loFormSet.lcWareCode = Pack_Hdr.cWareCode
*B608364,1 WAM 11/28/2007 (End)

*! B608518,1 MMT 04/17/2008 Fix bug of Wrong Line Qty when Editing Packing List [Start]
loFormSet.lnLastNo = pack_hdr.nlastlno
*! B608518,1 MMT 04/17/2008 Fix bug of Wrong Line Qty when Editing Packing List [End]

*!*************************************************************
*! Name      : lfvOrderNo
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : To validate the order #
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
*! Called    : From ChangeMode in the formset
*!*************************************************************
FUNCTION lfvOrderNo
LPARAMETERS loFormSet, lcOrderNo,lcAccount, llBrowse
PRIVATE lnCurAlias,lcTag,lcOrder,llPacked,llHasPik,llGoOn
llGoOn = .T.
lnCurAlias = SELECT(0)
lcTag = ORDER('OrdHdr')
SELECT OrdHdr
loFormSet.OrdHdr.SetOrder('OrdHdr')

IF llBrowse OR (!EMPTY(lcOrderNo) .AND. !loFormSet.OrdHdr.SEEK('O'+lcOrderNo))
  lcOrder = lcOrderNo
  =lfOrdBrow(@lcOrder,lcAccount,loFormSet)
  lcOrderNo = lcOrder
ENDIF

IF !EMPTY(lcOrderNo)
  loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE = lcOrderNo
  SELECT Pack_Hdr
  lcTmpKey = Pack_No
  lcOrdr = ORDER('Pack_Hdr')
  loFormSet.Pack_Hdr.SetOrder('OrderPck')
  loFormSet.llPacked = loFormSet.Pack_Hdr.SEEK(OrdHdr.ORDER+OrdHdr.STORE)
  loFormSet.Pack_Hdr.SETORDER(lcOrdr)
  loFormSet.Pack_Hdr.SEEK(lcTmpKey)
  SELECT OrdHdr
  loFormSet.OrdHdr.SEEK('O'+lcOrderNo)
  loFormSet.AriaForm1.kbAccount.keytextbox.VALUE  = OrdHdr.Account
  loFormSet.AriaForm1.kbAccount.ENABLED = .F.
  loFormSet.AriaForm1.kbAccount.keytextbox.ENABLED = .F.
  loFormSet.Customer.SEEK('M'+OrdHdr.Account)
  loFormSet.AriaForm1.txtCustName.VALUE = Customer.stName

  DO CASE
    *--- This order is completed and the orde packed.[Begin]
  CASE OrdHdr.STATUS = 'C' AND loFormSet.llPacked
    =gfModalGen("INM000000B00000","DIALOG",'','',;
      LANG_ManulPL_MsgcomPack)
    loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE = SPACE(6)
    loFormSet.llPacked = .F.
    llGoOn = .F.
  CASE OrdHdr.STATUS = 'X'
    *-- This order is canceled, cannot pack.
    *-- OK
    =gfModalGen("INM44050B00000","Dialog",LANG_ManulPL_MsgCanceled)
    loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE = SPACE(6)
    llGoOn = .F.
  CASE OrdHdr.bulk = 'Y'
    *-- This order is Bulk, cannot pack.
    *-- OK
    =gfModalGen("INM44050B00000","Dialog",LANG_ManulPL_Msgbulk)
    loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE = SPACE(6)
    llGoOn = .F.
  OTHERWISE
    loFormSet.llComp = (OrdHdr.STATUS = 'C' AND !loFormSet.llPacked)
    =lfGetData(loFormSet,lcOrderNo)
    loFormSet.PikTkt.SETORDER('ORDPIK')

    *-- llHaSPik variable that indicates if this order has pikTkt or not
    IF loFormSet.PikTkt.SEEK(lcOrderNo)
      LOCAL lnAlias
      lnAlias = SELECT(0)
      SELECT PikTkt
      LOCATE REST WHILE ORDER = lcOrderNo FOR STATUS $ 'PO'
      llHasPik = FOUND()
      SELECT (lnAlias)
    ELSE
      llHasPik = .F.
    ENDIF
    IF llGoOn
      DO CASE
        *-- this order has piktkt and it is multi store
        *-- piktkt could be selected to create Pack list for piktkt or
        *-- store could be selected to create Pack list for order
      CASE llHasPik AND loFormSet.ActiveMode = 'A' AND OrdHdr.MULTI = 'Y'
        loFormSet.ariaForm1.kbPkTktNo.ENABLED = .T.
        loFormSet.ariaForm1.kbStore.ENABLED = .T.

        *-- this order has piktkt and it is single store
        *-- piktkt could be selected to create Pack list for piktkt or
        *-- if piktkt is left empty that creates pack list for order
      CASE llHasPik AND loFormSet.ActiveMode = 'A' AND OrdHdr.MULTI = 'N'
        loFormSet.ariaForm1.kbPkTktNo.ENABLED = .T.
        loFormSet.ariaForm1.kbStore.ENABLED = .F.

        *-- this order has no piktkt and it is multi store
        *-- this pack list is for order and a store should be selected
      CASE !llHasPik AND loFormSet.ActiveMode = 'A' AND OrdHdr.MULTI = 'Y'
        loFormSet.ariaForm1.kbPkTktNo.ENABLED  = .F.
        loFormSet.ariaForm1.kbStore.ENABLED = .T.

        *-- this order has no piktkt and it is single store
        *-- so the processing goes in creating pack list for order with
        *-- selecting order or piktkt
      CASE !llHasPik AND loFormSet.ActiveMode = 'A' AND OrdHdr.MULTI = 'N'
        loFormSet.ariaForm1.kbPkTktNo.ENABLED  = .F.
        loFormSet.ariaForm1.kbStore.ENABLED = .F.
        llGoOn =lfvNewPack(loFormSet)
      ENDCASE
    ENDIF
  ENDCASE
ELSE
  loFormSet.AriaForm1.kbAccount.ENABLED = .T.
  loFormSet.AriaForm1.kbAccount.keytextbox.ENABLED = .T.
ENDIF
IF llGoOn
  loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE = lcOrderNo
  loFormSet.lcOrdNo = lcOrderNo
  SELECT OrdHdr
  loFormSet.OrdHdr.SETORDER('OrdHdr')
ELSE
  loFormSet.AriaForm1.kbAccount.keytextbox.VALUE = ""
  loFormSet.AriaForm1.txtCustName.VALUE = ""
  lcOrderNo = ""
ENDIF
SELECT (lnCurAlias)
RETURN !EMPTY(lcOrderNo)

*!*************************************************************
*! Name      : lfOrdBrow
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : To browse all packing list for a specific order
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
*! Called    : From ChangeMode in the formset
*!*************************************************************
FUNCTION lfOrdBrow
LPARAMETERS lcOrder,lcAccount, loFormSet

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
  loFormSet.OrdHdr.SETORDER('OrdAcct')
  lcAcc = lcAccount
  lcOrder = IIF(ARIABROW("lcAcc+'O'",;
    LANG_ManulPL_hdrOrders,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),;
    OrdHdr.ORDER,SPACE(6))

CASE EMPTY(lcAccount)
  loFormSet.OrdHdr.SETORDER('OrdHdr')
  lcOrder = IIF(ARIABROW("'O'",LANG_ManulPL_hdrOrders,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),;
    OrdHdr.ORDER,SPACE(6))
ENDCASE
loFormSet.OrdHdr.SETORDER(lcTag)

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvPktTktNo
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : To validate the Pick Ticket #
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!			   lcPkTkt   : Pick Ticket #
*!*************************************************************
FUNCTION lfvPktTktNo
LPARAMETERS loFormSet, lcPkTkt, lcOrderNo, lcAccount, llBrowse
*-- lcPikTag variable that hold piktkt file tag name
*-- lnCurAlias variable that hold the current alias
PRIVATE lcPikTag,lnCurAlias,lcPkTkt,llReturn
llReturn = .T.

IF LASTKEY() = 13 OR LASTKEY() = 9 OR LASTKEY() = 24 OR llBrowse
  lcPikTag = ORDER("PikTkt")
  loFormSet.PikTkt.SetOrder(IIF(EMPTY(lcOrderNo),'PikTkt','OrdPik'))

  IF llBrowse OR !loFormSet.PikTkt.SEEK(ALLTRIM(lcOrderNo)+lcPkTkt) OR;
      (loFormSet.PikTkt.SEEK(ALLTRIM(lcOrderNo)+lcPkTkt) AND PikTkt.STATUS = 'C') OR EMPTY(lcPkTkt)

    DO CASE
      *-- Case empty order and not empty account
    CASE EMPTY(lcOrderNo) AND !EMPTY(lcAccount)
      lnCurAlias = SELECT(0)
      SELECT PikTkt
      LOCATE FOR Account = lcAccount AND PikTkt.STATUS $ "PO"
      IF FOUND()
        lcPkTkt = IIF(lfPikTktBrow(loFormSet,lcPkTkt,lcOrderNo,lcAccount),;
          PikTkt.PikTkt,SPACE(6))
      ELSE
        *-- 'No piktkts found for account lcAccount
        *-- <OK>
        llNoThing = gfModalGen("INM44043B00000","Dialog",lcAccount)
        STORE SPACE(6) TO lcPkTkt
      ENDIF
      SELECT (lnCurAlias)
    OTHERWISE
      IF EMPTY(lcOrderNo)
        lcPkTkt = IIF(lfPikTktBrow(loFormSet,lcPkTkt,lcOrderNo,lcAccount),PikTkt.PikTkt,SPACE(6))
      ELSE
        IF loFormSet.PikTkt.SEEK(lcOrderNo)
          SELECT PikTkt
          LOCATE REST WHILE ORDER = lcOrderNo FOR STATUS $ "PO"
          IF FOUND()
            lcPkTkt = IIF(lfPikTktBrow(loFormSet,lcPkTkt,lcOrderNo,lcAccount),PikTkt.PikTkt,SPACE(6))
          ENDIF
        ELSE
          *-- 'No piktkts found for order lcOrder
          *-- <OK>
          llNoThing = gfModalGen("INM44028B00000","Dialog",lcOrderNo)
          STORE SPACE(6) TO lcPkTkt
        ENDIF
      ENDIF
    ENDCASE
  ENDIF
  *E037427,1 WAM 05/05/2005 Show selected piktkt
  loFormSet.ariaForm1.kbPkTktNo.keytextbox.VALUE = lcPkTkt
  *E037427,1 WAM 05/05/2005 (End)

  IF !EMPTY(lcPkTkt)
    loFormSet.ariaForm1.kbPkTktNo.keytextbox.VALUE = lcPkTkt
    *B608254,1 WAM 09/05/2007 Initialize variables that are used by the delete function
    lcAccount = PikTkt.Account
    lcOrderNo = PikTkt.ORDER
    *B608254,1 WAM 09/05/2007 (End)
    IF loFormSet.Pack_Hdr.SEEK(lcPkTkt)
      loFormSet.ariaForm1.kbPackNo.keytextbox.VALUE = lcPkTkt
      =loFormSet.SeekRecord(lcPkTkt)
    ELSE
      lcAccount = PikTkt.Account
      lcOrderNo = PikTkt.ORDER

      IF PikTkt.STATUS = 'H'
        *-- This picking ticket is on hold.
        *-- OK
        = gfModalGen("INM44045B00000","Dialog")
      ENDIF
      =lfGetData(loFormSet,lcOrderNo)
    ENDIF
  ENDIF

  loFormSet.AriaForm1.kbOrderNo.keytextbox.VALUE = lcOrderNo
  loFormSet.AriaForm1.kbAccount.keytextbox.VALUE = lcAccount
  *-- Pick Tket # could be empty while Order # is empty, but while Pick Ticket
  *-- is validated and not empty Order could not be empty
  IF (!EMPTY(lcOrderNo) OR !EMPTY(lcPkTkt)) AND loFormSet.ActiveMode = 'A'
    loFormSet.OrdHdr.SEEK('O'+lcOrderNo)
    DO CASE
    CASE OrdHdr.MULTI = 'Y' AND !EMPTY(lcPkTkt)
      loFormSet.AriaForm1.kbStore.keytextbox.VALUE = PikTkt.STORE
      loFormSet.Customer.SEEK('S'+OrdHdr.Account+OrdHdr.STORE)
      loFormSet.AriaForm1.txtStoreName.VALUE = Customer.stName
      loFormSet.AriaForm1.kbStore.ENABLED = .F.
      llReturn = lfvNewPack(loFormSet)

    CASE OrdHdr.MULTI = 'Y' AND EMPTY(lcPkTkt) AND !EMPTY(lcOrderNo)
      lcOrderNo = loFormSet.AriaForm1.kbOrderNo.keytextbox.VALUE
      lcStore   = loFormSet.AriaForm1.kbStore.keytextbox.VALUE
      loFormSet.OrdLine.SEEK('O'+lcOrderNo)
      SELECT ORDLINE
      LOCATE REST FOR cOrdType+ORDER = 'O'+lcOrderNo AND EMPTY(Piktkt)
      IF !FOUND()
        *-- 44125 : 'There are no unpicked or allocated lines in this order. You have to select a Pick Ticket.'
        =gfModalGen('TRM44125B00000','DIALOG')
        llReturn = .F.
      ELSE
        *-- 44131 : 'Only unpicked or allocated lines in this order will be displayed.'
        *-- 00012 : <Procced> <Cancel>
        IF gfModalGen('TRM44131B00012','DIALOG') = 1
          loFormSet.AriaForm1.kbStore.ENABLED = .T.
          llReturn = .T.
        ELSE
          llReturn = .F.
        ENDIF
      ENDIF

    CASE OrdHdr.MULTI = 'N'
      IF EMPTY(lcPkTkt)
        lcOrderNo = loFormSet.AriaForm1.kbOrderNo.keytextbox.VALUE
        loFormSet.OrdLine.SEEK('O'+lcOrderNo)
        SELECT ORDLINE
        LOCATE REST FOR cOrdType+ORDER= 'O'+lcOrderNo AND EMPTY(Piktkt)
        IF !FOUND()
          *-- 44125 : 'There are no unpicked or allocated lines in this order. You have to select a Pick Ticket.'
          =gfModalGen('TRM44125B00000','DIALOG')
          llReturn = .F.
        ELSE
          *-- 44131 : 'Only unpicked or allocated lines in this order will be displayed.'
          *-- 00012 : <Procced> <Cancel>
          IF gfModalGen('TRM44131B00012','DIALOG',.F.,.F.,'Only unpicked or allocated lines in this order will be displayed.') = 1
            llReturn = lfvNewPack(loFormSet)
          ELSE
            llReturn = .F.
          ENDIF
        ENDIF
      ELSE
        llReturn = lfvNewPack(loFormSet)
      ENDIF
    ENDCASE
  ENDIF
  loFormSet.PikTkt.SetOrder(lcPikTag)
ENDIF
RETURN llReturn

*!*************************************************************
*! Name      : lfPikTktBrow
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : To browse the pick tickets
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!			   lcPkTkt   : Pick Ticket #
*!*************************************************************
FUNCTION lfPikTktBrow
LPARAMETERS loFormSet, lcPkTkt, lcOrderNo, lcAccount
*-- lcFields   variable that hold the name of browsed fields
*-- laBrow     array that hold the returned values from AriaBrow function
*-- lnCurAlias variable that hold the current alias
*-- lcCurTag   variable that hold the currend tag name
*-- llReturn   value that is returned by this function
*-- lcTag      variable that hold the name of the tag which is desired to switch
*              file order to it

PRIVATE lcBrFields,lcFile_Ttl,laBrow,lcFields,lnCurAlias,lcCurTag,llReturn,;
  lcBrFields
DIMENSION laBrow[7]
STORE SPACE(0) TO laBrow

lnCurAlias = SELECT(0)

lcFields    = "PikTkt,Order,Store,OrdHdr.Complete,Date,Account,Customer.StName"
lcBrFields  = [PikTkt:H=LANG_ManulPL_lblPikTkt,]+;
  [Order :H=LANG_ManulPL_lblOrderNo,]+;
  [Store :H=LANG_ManulPL_lblStore,]+;
  [OrdHdr.Complete:H=LANG_ManulPL_lblComplete,]+;
  [Date:H=LANG_ManulPL_lblPikDate,]+;
  [Account:H=LANG_ManulPL_lblAccount,]+;
  [Customer.StName:H=LANG_ManulPL_lblAccName]

lcFile_Ttl = LANG_ManulPL_hdrPikTicket

SELECT OrdHdr
lcCurTag = ORDER('OrdHdr')
loFormSet.OrdHdr.SetOrder('OrdAcct')

lcKey = IIF(EMPTY(lcAccount),'',lcAccount)
SET KEY TO lcKey

SELECT PikTkt
LOCATE
SET RELATION TO Account+'O'+ORDER INTO OrdHdr ADDITIVE
*B608882,1 TMI 05/31/2009 09:46:47 AM [Start] set a relation to the customer file to show the correct customer name
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER ADDITIVE
*B608882,1 TMI 05/31/2009 09:46:48 AM [End  ]
IF EMPTY(lcOrderNo) AND !EMPTY(lcAccount)
  lcAcc = lcAccount
  llReturn = AriaBrow("'' FOR PikTkt.Account = lcAcc AND !EOF('OrdHdr') AND PikTkt.Status$'PO'",;
    lcFile_Ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2,;
    gnBrFSCol2,.F.,.F.,lcFields,"laBrow",.F.,'PikTkt',.F.)
ELSE
  lcOrd = lcOrderNo
  llReturn = AriaBrow("FOR Order = ALLTRIM(lcOrd) AND Status $ 'PO'",;
    lcFile_Ttl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2,;
    gnBrFSCol2,.F.,.F.,lcFields,"laBrow",.F.,'PikTkt',.F.)
ENDIF

SELECT PikTkt
SET RELATION TO
loFormSet.OrdHdr.SetOrder(lcCurTag)
SELECT OrdHdr
SET KEY TO ''
SELECT(lnCurAlias)

RETURN llReturn


*!*************************************************************
*! Name      : lfvAccount
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : To validate the account code
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
FUNCTION lfvAccount
LPARAMETERS loFormSet, lcAccount, llBrowse

IF llBrowse OR (!EMPTY(lcAccount) AND !loFormSet.Customer.SEEK('M'+lcAccount))
  DO CUSBROWM WITH lcAccount
ENDIF
loFormSet.AriaForm1.kbAccount.keytextbox.VALUE = lcAccount
loFormSet.AriaForm1.txtCustName.VALUE = IIF(!EMPTY(lcAccount),Customer.stName,'')
*RETURN !EMPTY(lcAccount)
RETURN

*!*************************************************************
*! Name      : lfvStore
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : To validate the store code
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!			   lcAccount : Account Code
*!			   lcStore   : Store code
*!             lcOrderNo : Order #
*!		       llBrowse  : To browse the stores
*!*************************************************************
FUNCTION lfvStore
LPARAMETERS loFormSet, lcAccount,lcStore, lcOrderNo,llBrowse

PRIVATE xStore,llGoOn
IF EMPTY(lcOrderNo)
  RETURN .F.
ENDIF
llGoOn = .T.
xStore = lcStore

*E037427,1 WAM 05/05/2005 Enhance performance
*!*	loFormSet.ORDLINE.SetOrder('ORDLINE')
*!*	IF loFormSet.ORDLINE.SEEK('O'+PADR(lcOrderNo,6))
*!*	  SELECT ORDLINE
*!*	  SCAN REST FOR cordtype+order+STR(lineno,6) = 'O'+PADR(lcOrderNo,6)
*!*	    IF loFormSet.Customer.SEEK('S'+PADR(ORDLINE.Account,5)+PADR(ORDLINE.Store,8))
*!*	      SELECT CUSTOMER
*!*	      SCATTER MEMVAR MEMO
*!*	      IF !SEEK(m.Store,loFormSet.lctmpstores)
*!*	        INSERT INTO (loFormSet.lctmpstores) FROM MEMVAR
*!*	      ENDIF
*!*	    ENDIF
*!*	  ENDSCAN
*!*	ENDIF
loFormSet.ORDLINE.SetOrder('ORDLINST')
IF loFormSet.ORDLINE.SEEK('O'+PADR(lcOrderNo,6))
  SELECT ORDLINE
  DO WHILE cordtype+ORDER+STORE+STYLE+STR(LINENO,6) =  'O'+PADR(lcOrderNo,6)
    lcOrdStore = STORE
    IF loFormSet.Customer.SEEK('S'+ORDLINE.Account+lcOrdStore)
      SELECT CUSTOMER
      SCATTER MEMVAR MEMO
      INSERT INTO (loFormSet.lctmpstores) FROM MEMVAR
    ENDIF
    SELECT ORDLINE
    SCAN REST WHILE cordtype+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+PADR(lcOrderNo,6)+lcOrdStore
    ENDSCAN
  ENDDO
ENDIF
*E037427,1 WAM 05/05/2005 (End)

IF llBrowse .OR. (!EMPTY(lcStore) AND !SEEK(lcStore,loFormSet.lctmpstores))
  DIMENSION laBrow[1]
  STORE SPACE(0) TO laBrow
  SELECT (loFormSet.lctmpstores)
  LOCATE
  lcBrFields = "STORE :h='"+LANG_ManulPL_LabelStore+"',stName:23:h='"+LANG_ManulPL_LabelName+"',"+;
    "cAddress1 :H='"+LANG_ManulPL_LabelStAdd+"',"+;
    "cAddress2 :H='"+LANG_ManulPL_LabelSt+"',"+;
    "cAddress3 :H='"+LANG_ManulPL_LabelStCity+"',"+;
    "cAddress4 :H='"+LANG_ManulPL_LabelStState+"',"+;
    "cAddress5 :H='"+LANG_ManulPL_LabelStZip+"',"+;
    "cAddress12 :H='"+LANG_ManulPL_LabelBTAdd+"',"+;
    "cAddress22 :H='"+LANG_ManulPL_LabelBT+"',"+;
    "cAddress32 :H='"+LANG_ManulPL_LabelBTCity+"',"+;
    "cAddress42 :H='"+LANG_ManulPL_LabelBTState+"',"+;
    "cAddress52 :H='"+LANG_ManulPL_LabelBTZip+"',"+;
    "Phone1 :P= GFPHONETEM() :H='"+LANG_ManulPL_LabelPhone+"',Buyer :H='"+LANG_ManulPL_LabelBuyer+"'"
  lcWinTitl = LANG_ManulPL_hdrStorebrow
  *E037427,1 WAM 05/05/2005 Fix file index
  *llReturn = AriaBrow(["S"],lcWinTitl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2,gnBrFSCol2,.F.,.F.,"","laBrow",.F.,'Store',.F.)
  llReturn = AriaBrow(.F.,lcWinTitl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2,gnBrFSCol2,.F.,.F.,"","laBrow",.F.,'Store',.F.)
  *E037427,1 WAM 05/05/2005 (End)
  lcStore = IIF(!llReturn,SPACE(8),EVALUATE(loFormSet.lctmpstores+'.store'))
ENDIF

IF !EMPTY(lcStore)
  loFormSet.AriaForm1.kbStore.keytextbox.VALUE = lcStore
  loFormSet.Customer.SEEK('S'+lcAccount+lcStore)
  IF CUSTOMER.STATUS <> 'A'
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_ManulPL_lblStore+' '+ALLTRIM(CUSTOMER.STORE)+LANG_ManulPL_MsgnonActv)
    lcStore = SPACE(8)
    RETURN .F.
  ENDIF
ENDIF
SELECT OrdLine
loFormSet.OrdLine.SetOrder('OrdLinSt')
llStorFund = loFormSet.OrdLine.SEEK('O'+lcOrderNo+lcStore)
IF llStorFund
  lcPkTktNo = PADR(loFormSet.AriaForm1.kbPkTktNo.keytextbox.VALUE ,6)
  SELECT ORDLINE
  *E037427,1 WAM 05/05/2005 Enhance performance
  *LOCATE REST FOR cOrdType+order+Store = 'O'+lcOrderNo+lcStore AND Piktkt = lcPkTktNo
  LOCATE REST WHILE cOrdType+ORDER+STORE = 'O'+lcOrderNo+lcStore AND Piktkt = lcPkTktNo
  *E037427,1 WAM 05/05/2005 (End)
  IF !FOUND()
    *-- 44126 : 'There are no unpicked lines in this order for store ' + lcStore
    =gfModalGen('TRM44126B00000','DIALOG',lcStore)
    lcStore = SPACE(8)
    RETURN .F.
  ENDI
ENDIF
IF !EMPTY(lcStore) AND !llStorFund
  *-- No ordered quantity for Store lcStore
  *-- <OK>
  llNoThing = gfModalGen("INM44031B00000","Dialog",lcStore)
  lcStore = SPACE(8)
ELSE
  IF EMPTY(lcOrderNo)
    IF OrdHdr.MultiPo AND loFormSet.OrdLine.SEEK('O'+lcOrderNo+lcStore)
      loFormSet.AriaForm1.txtCustPo.VALUE = OrdLine.CustPo
    ELSE
      loFormSet.AriaForm1.txtCustPo.VALUE = OrdHdr.CustPo
    ENDIF
  ENDIF
  IF !EMPTY(lcStore)
    llGoOn =lfvNewPack(loFormSet)
  ENDIF
ENDIF
IF llGoOn
  loFormSet.AriaForm1.txtStoreName.VALUE = IIF(!EMPTY(lcStore) AND ;
    loFormSet.Customer.SEEK('S'+lcAccount+lcStore),;
    Customer.stName,"")
  loFormSet.lcStoreNo = lcStore
ELSE
  lcStore = ""
ENDIF
RETURN !EMPTY(lcStore)

*!*************************************************************
*! Name      : lfGetData
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
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

=loFormSet.OrdHdr.SEEK('O'+lcOrderNo)

WITH loFormSet.ariaForm1
  STORE IIF(!EMPTY(lcOrderNo),OrdHdr.Account,SPACE(6)) TO .kbAccount.keytextbox.VALUE, lcAccount
  *-- Account Name
  loFormSet.Customer.SEEK('M'+OrdHdr.Account)
  .txtCustName.VALUE = Customer.stName
  loFormSet.llEdiAcc = loFormSet.llEdiSys .AND. loFormSet.EDIACPRT.SEEK('A'+lcAccount) .AND.;
    loFormSet.EDIPH.SEEK(EDIACPRT.cPartCode)

  STORE loFormSet.llEdiSys AND loFormSet.llEdiAcc .AND. EDIACPRT.lPkChrDes TO loFormSet.llPCDStat
  STORE loFormSet.llEdiSys AND loFormSet.llEdiAcc .AND. EDIPH.lPltShp  TO loFormSet.llPalStat
  *-- PikTkt
  lcPikTkt = .kbPkTktNo.keytextbox.VALUE

  *-- Store
  .kbStore.keytextbox.VALUE  = ;
    IIF(EMPTY(lcOrderNo),SPACE(8),IIF(!EMPTY(lcPikTkt),PikTkt.STORE,;
    IIF(OrdHdr.MULTI='Y',SPACE(8),OrdHdr.STORE)))
  lcStore    = .kbStore.keytextbox.VALUE

  *-- Ware House
  loFormSet.lcWareCode = IIF(EMPTY(lcOrderNo),'',IIF(!EMPTY(lcPikTkt),PikTkt.cWareCode,OrdHdr.cWareCode))
  *-- Cust PO
  *B608381,1 WAM 12/13/2007 get the cust. PO# from piktkt file
  *.txtCustPo.VALUE = IIF(OrdHdr.MultiPo,IIF(loFormSet.OrdLine.SEEK('O'+lcOrderNo+lcStore),OrdLine.CustPo,SPACE(10)),OrdHdr.CustPo)
  .txtCustPo.VALUE = IIF(!EMPTY(lcPikTkt),PikTkt.CustPo,IIF(OrdHdr.MultiPo,IIF(loFormSet.OrdLine.SEEK('O'+lcOrderNo+lcStore),OrdLine.CustPo,SPACE(10)),OrdHdr.CustPo))
  *B608381,1 WAM 12/13/2007 (End)

  *-- Department
  .pgfPacking.HEADER.txtdept.VALUE = IIF(EMPTY(lcOrderNo),'',OrdHdr.Dept)
  *-- ShipVia
  .pgfPacking.HEADER.cboShipViaH.VALUE =  IIF(EMPTY(lcOrderNo),SPACE(6),OrdHdr.ShipVia)

  IF loFormSet.llEdiAcc
    *: B610016,1 SAB 07/16/2012 Enable Editing Notes and Special Inst. for Completed Packs [Start]
    *STORE (loFormSet.ActiveMode = 'S' OR loFormSet.ActiveMode = 'V') TO ;
    .pgfPacking.HEADER.cboCntType.ENABLED, .pgfPacking.HEADER.cboDirectTo.ENABLED
    STORE !(loFormSet.ActiveMode = 'S' OR loFormSet.ActiveMode = 'V') TO ;
      .pgfPacking.HEADER.cboCntType.ENABLED, .pgfPacking.HEADER.cboDirectTo.ENABLED
    *: B610016,1 SAB 07/16/2012 Enable Editing Notes and Special Inst. for Completed Packs [End]
    *-- Pack Char. Code
    .pgfPacking.HEADER.txtPackCharCode.VALUE = ;
      IIF(EMPTY(lcOrderNo),SPACE(5),IIF(loFormSet.llPCDStat , EDIACPRT.cPckChCode , SPACE(5)))
    *-- Pack Char. Desc
    .pgfPacking.HEADER.txtPackDescCode.VALUE = ;
      IIF(EMPTY(lcOrderNo),SPACE(7) ,IIF(loFormSet.llPCDStat , EDIACPRT.cPckDsCode , SPACE(7)))

    STORE (loFormSet.llPCDStat AND (loFormSet.ActiveMode = 'E' OR loFormSet.llNew) AND ;
      loFormSet.ariAFORM1.pgfPacking.ACTIVEPAGE = 1) TO ;
      .pgfPacking.HEADER.txtPackCharCode.ENABLED,;
      .pgfPacking.HEADER.txtPackDescCode.ENABLED

    STORE (loFormSet.llPalStat AND (loFormSet.ActiveMode = 'E' OR loFormSet.llNew) AND ;
      loFormSet.ariAFORM1.pgfPacking.ACTIVEPAGE = 3) TO ;
      loFormSet.ariAFORM1.pgfPacking.CartonInfo.txtPaletteNoH.ENABLED
  ELSE
    *-- means carton type is standard
    loFormSet.lnCtnTyp = 2
    *-- means pack is directed to store
    loFormSet.lnDrctTo = 1

    *-- Replace CToStorCn WITH 'S'
    STORE .F. TO .pgfPacking.HEADER.cboCntType.ENABLED,.pgfPacking.HEADER.cboDirectTo.ENABLED
  ENDIF
ENDWITH

*!*************************************************************
*! Name      : lfvNewPack
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : Create new pack list
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
FUNCTION lfvNewPack
LPARAMETERS loFormSet

PRIVATE lcTag,lnCurAlias,lcAccount, lcStore,lcOrderNo
lcOrderNo = loFormSet.AriaForm1.kbOrderNo.keytextbox.VALUE
lcAccount = loFormSet.AriaForm1.kbAccount.keytextbox.VALUE
lcStore   = loFormSet.AriaForm1.kbStore.keytextbox.VALUE

STORE .T. TO loFormSet.llCupdate,loFormSet.llAnyUpd,loFormSet.llNew

*E037427,1 WAM 05/05/2005 Initialize pack total cartons, weight, and pieces
STORE 0 TO loFormSet.lnPackWgh, loFormSet.lnPackCtn, loFormSet.lnPackQty
*E037427,1 WAM 05/05/2005 (End)

*-- check if any pack is being created now on the same order in any
*-- other session
llGoOn = lfChkOrdLok(loFormSet)
loFormSet.OrdHdr.SEEK('O'+lcOrderNo)

IF llGoOn
  IF ALLTRIM(OrdHdr.ShipVia)='*' AND loFormSet.Customer.SEEK('S'+lcAccount+lcStore)
    loFormSet.ariaForm1.pgfPacking.HEADER.cboShipViaH.VALUE  = Customer.ShipVia
  ENDIF

  =lfNewBOL(loFormSet)

  lcTag = ORDER('Pack_Hdr')
  loFormSet.Pack_Hdr.SetOrder('AccPack')

  IF loFormSet.Pack_Hdr.SEEK(lcAccount)
    *-- "Do you wish to copy from another pack list"
    *-- <YES>, <NO>
    IF gfModalGen("INM44044B00006","Dialog") = 1
      = lfPackCopy(loFormSet,lcOrderNo,lcAccount)
    ENDIF
  ENDIF
  loFormSet.Pack_Hdr.SetOrder(lcTag)
  = lfvSelOrdL(loFormSet)
  *-- Set the object status
  =lfWinHdrSt(loFormSet)

ELSE
  loFormSet.ChangeMode("S")
  RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfPackCopy
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : Copy data from existing pack
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
FUNCTION lfPackCopy
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*LPARAMETERS loFormSet,lcOrder, lcAccount
PARAMETERS loFormSet,lcOrder, lcAccount
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
lcCopyFromPack = ''
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+ 'ALPLSTCP') WITH loFormSet,lcOrder,lcAccount TO lcCopyFromPack
=gfCallForm('ALPLSTCP',.F.,"loFormSet,lcOrder,lcAccount","lcCopyFromPack")
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
loFormSet.lcPackNo = lcCopyFromPack

*!*************************************************************
*! Name      : lfvPackFrom
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : To validate pack id in copy from another pack screen
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!			   lcPackId  : Pack Id
*!			   llBrowse  : .T. if click browse button
*!*************************************************************
FUNCTION lfvPackFrom
LPARAMETERS loFormSet,lcPackId,llBrowse
PRIVATE lnCurAlias,lcPCkHdrTag

SELECT Pack_Hdr
lcPCkHdrTag = ORDER('Pack_Hdr')
loFormSet.Pack_Hdr.SetOrder('Pack_Hdr')

loFormSet.lcPackNo = lcPackId

IF llBrowse OR (!EMPTY(lcPackId) AND !loFormSet.Pack_Hdr.SEEK(lcPackId))
  loFormSet.llPckCopy = .T.
  loFormSet.lcPackNo = IIF(lfPakNoBrw(loFormSet),Pack_Hdr.Pack_No,SPACE(6))
  loFormSet.llPckCopy = .F.
ENDIF
IF !EMPTY(loFormSet.lcPackNo)
  loFormSet.lnCtnTyp = IIF(Pack_Hdr.LStandCtn,1,2)
  loFormSet.lnDrctTo = IIF(Pack_Hdr.CToStorCn='C',2,1)
  loFormSet.lnFrom = 1
  loFormSet.lnTo   = 1
ENDIF
loFormSet.Pack_Hdr.SetOrder(lcPCkHdrTag)
RETURN loFormSet.lcPackNo

*!*************************************************************
*! Name      : lfvSelOrdL
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
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
lcStore   = loFormSet.AriaForm1.kbStore.keytextbox.VALUE
lcPkTktNo = PADR(loFormSet.AriaForm1.kbPkTktNo.keytextbox.VALUE ,6)
lcPckNo   = loFormSet.AriaForm1.kbPackNo.keytextbox.VALUE
loFormSet.lcPrvPack = lcPckNo
loFormSet.OrdLine.SetOrder('Ordlinst')
loFormSet.OrdLine.SEEK('O'+lcOrderNo+lcStore)

STORE SPACE(0) TO lcStyle,lcStyDesc

*! B608336,1 MMT 10/29/2007 fix bug of displaying wrong styles not included in PIKTKT[Start]
*lcExp = IIF(loFormSet.ActiveMode = 'V',"WHILE cOrdType+Order+Store='O'+lcOrderNo+lcStore",;
IIF(!EMPTY(lcPkTktNo),"WHILE cOrdType+Order+Store='O'+lcOrderNo+lcStore FOR PikTkt=lcPkTktNo AND Picked",;
"WHILE cOrdType+Order+Store='O'+lcOrderNo+lcStore FOR PikTkt=lcPkTktNo OR PikTkt = '******'"))
lcExp = IIF(loFormSet.ActiveMode = 'V',"WHILE cOrdType+Order+Store='O'+lcOrderNo+lcStore FOR IIF(!EMPTY(lcPkTktNo),piktkt =lcPkTktNo,.T.)",;
  IIF(!EMPTY(lcPkTktNo),"WHILE cOrdType+Order+Store='O'+lcOrderNo+lcStore FOR PikTkt=lcPkTktNo AND Picked",;
  "WHILE cOrdType+Order+Store='O'+lcOrderNo+lcStore FOR PikTkt=lcPkTktNo OR PikTkt = '******'"))
*! B608336,1 MMT 10/29/2007 fix bug of displaying wrong styles not included in PIKTKT[End]


*--- llFrmPikLn --> .T. ----> colect data from pikline , .F. --> from ordline
llFrmPikLn = .F.
IF !EMPTY(lcPkTktNo)
  SELECT PIKTKT
  loFormSet.PikTkt.SetOrder('PikTkt')
  IF loFormSet.PikTkt.SEEK(lcPkTktNo) AND STATUS = 'C'
    llFrmPikLn = .T.
    lcExp = "WHILE PikTkt+Order+Store=lcPkTktNo+lcOrderNo+lcStore FOR Picked "
  ENDIF
ENDIF

SELECT PACK_LIN
SET RELATION TO
SET KEY TO
SET FILTER TO
*E037427,1 WAM Get all order lines
*IF EMPTY(lcPckNo)
*  =lfNewPckLn(loFormSet,llFrmPikLn )
*ELSE
*  =lfGetPckLn(loFormSet,llFrmPikLn )
*ENDIF
=lfNewPckLn(loFormSet,llFrmPikLn )
IF !EMPTY(lcPckNo)
  =lfGetPckLn(loFormSet,llFrmPikLn )
ENDIF
*E037427,1 WAM (End)

*B607804,1 WAM 10/19/2006 Clear order lock
loFormSet.lcOrdNo = lcOrderNo
*B607804,1 WAM 10/19/2006 (End)

llCopyPck  = .F.
IF !EMPTY(loFormSet.lcPackNo)
  llNtFound = .F.
  llLinFound = lfCkPckMch(loFormSet)
  IF !llLinFound
    *-- 44132 : Lines of this order have no matching lines in packing list 999999. Can not Copy.
    = gfModalGen("QRM44132B00000","Dialog",loFormSet.lcPackNo)
    loFormSet.lcPackNo = ""
  ELSE
    IF llNtFound
      *-- 44133 : Some lines can not be copied. It has no matching lines in packing list 999999
      = gfModalGen("QRM44133B00000","Dialog",loFormSet.lcPackNo)
    ENDIF
  ENDIF
ENDIF
SELECT Pack_Lin
lcTag = ORDER()
loFormSet.Pack_Lin.SetOrder('PackStyle')
IF !EMPTY(lcPckNo) OR !EMPTY(loFormSet.lcPackNo)
  IF !EMPTY(lcPckNo)
    =lfGetPkInf(loFormSet)
  ENDIF
  IF !EMPTY(loFormSet.lcPackNo)
    =lfCpyPkInf(loFormSet)
  ENDIF
ELSE
  STORE 1 TO loFormSet.lnFrom,loFormSet.lnTo
ENDIF

IF !EMPTY(loFormSet.lcPackNo)
  WITH loFormSet.AriaForm1
    .pgfPacking.HEADER.txtNoteH.VALUE   = Pack_Hdr.NOTE
    .pgfPacking.HEADER.txtWeight.VALUE  = loFormSet.lnPackWgh
    *-- Total Cartons
    .pgfPacking.HEADER.txtCartons.VALUE = loFormSet.lnPackCtn
    *-- Total Pieces
    .pgfPacking.HEADER.txtPieces.VALUE  = loFormSet.lnPackQty

    *-- Special Instructions 1
    .pgfPacking.HEADER.txtSpInst1.VALUE = Pack_Hdr.Sp_Inst1
    *-- Special Instructions 2
    .pgfPacking.HEADER.txtSpInst2.VALUE = Pack_Hdr.Sp_Inst2
    *-- Cnt Type
    loFormSet.lnCtnTyp = IIF(Pack_Hdr.LStandCtn,1,2)
    *-- Direct To
    loFormSet.lnDrctTo = IIF(Pack_Hdr.CToStorCn='C',2,1)
    *-- Pack Char Code
    .pgfPacking.HEADER.txtPackCharCode.VALUE = Pack_Hdr.cPkChCode
    *-- Pack Char Desc
    .pgfPacking.HEADER.txtPackDescCode.VALUE = Pack_Hdr.cPkDsCode
  ENDWITH
ENDIF
loFormSet.Pack_Lin.SetOrder(lcTag)
SELECT (loFormSet.lcPckLin)
SCAN FOR !EMPTY(Pack_ID)
  IF llFrmPikLn
    loFormSet.PikLine.SEEK(lcPkTktNo+lcOrderNo)
    LOCAL lcPrvOrd
    lcPrvOrd = ORDER('ORDLINE')
    loFormSet.OrdLine.SetOrder('ORDLINE')
    loFormSet.OrdLine.SEEK('O'+PikLine.ORDER+STR(nOrdLineNo,6))
    loFormSet.OrdLine.SetOrder(lcPrvOrd)
  ELSE
    loFormSet.OrdLine.SEEK('O'+lcOrderNo+lcStore)
  ENDIF
  loFormSet.OrdLine.SEEK('O'+lcOrderNo+lcStore+EVALUATE(loFormSet.lcPckLin+'.STYLE')+;
    STR(EVALUATE(loFormSet.lcPckLin+'.nOrdLineNo'),6))
  =IIF(llFrmPikLn,;
    loFormSet.PikLine.SEEK('O'+lcPkTktNo+lcOrderNo+STR(EVALUATE(loFormSet.lcPckLin+'.nOrdLineNo'),6)),.T.)

  IF !llCopyPck
    REPLACE nPackNo WITH Ordline.nPackNo,;
      nPkPack WITH Ordline.nPkPack,;
      lRange  WITH IIF(llFrmPikLn , PikLine.lRange, OrdLine.lRange)
  ELSE
    REPLACE nPackNo WITH Ordline.nPackNo,;
      nPkPack WITH OrdLine.nPkPack,;
      lRange  WITH IIF(llFrmPikLn , PikLine.lRange, OrdLine.lRange)
  ENDIF
ENDSCAN
*--collect data for temp file in case of pack summery
=lfColSum(loFormSet)
SELECT (loFormSet.lcPckLin)
SCAN
  SCATTER MEMVAR MEMO
  *IF EMPTY(m.Pack_Id) && Wael
  INSERT INTO (loFormSet.lcPackLines) FROM MEMVAR
  *ENDIF
ENDSCAN
SELECT (loFormSet.lcPckLin)
GO TOP
=RLOCK(loFormSet.lcPckLin)
UNLOCK IN (loFormSet.lcPckLin)

= lfDtlBrow(loFormSet)
= lfwDtlBrow(loFormSet)

SELECT(loFormSet.lcCtnHdr)
= RLOCK(loFormSet.lcCtnHdr)
UNLOCK IN (loFormSet.lcCtnHdr)
GO TOP

SELECT(loFormSet.lcCtnDtl)
UNLOCK IN (loFormSet.lcCtnDtl)

*-- Browse for cartons header in the caron information folder
=lfCtnHdrBr(loFormSet)
*-- Browse for cartons details in the caron information folder
=lfCtnDtlBr(loFormSet)

WAIT CLEAR

*-- If the sales order is completed and not packed Or no lines to pack
IF loFormSet.llComp OR !loFormSet.llAnyRec
  WITH loFormSet.ariaForm1.pgfPacking.DETAIL
    STORE .F. TO .cmdSelect.ENABLED,.cmdSelAll.ENABLED,.cmdInvert.ENABLED,;
      .cmdSelNone.ENABLED,.cmdPack.ENABLED,.cmdStyle.ENABLED,.cmdApply.ENABLED,;
      .txtCartFrom.ENABLED,.txtCartTo.ENABLED,.txtPaletteNo.ENABLED,;
      .txtQtyPerCart.ENABLED,.txtUnitWeight.ENABLED
    STORE "" TO .txtCartFrom.CONTROLSOURCE,.txtCartTo.CONTROLSOURCE,.txtPaletteNo.CONTROLSOURCE,;
      .txtQtyPerCart.CONTROLSOURCE,.txtUnitWeight.CONTROLSOURCE
  ENDWITH
ELSE
  IF loFormSet.ActiveMode = "E" OR loFormSet.llNew
    WITH loFormSet.ariaForm1.pgfPacking.DETAIL
      *: B610016,1 SAB 07/16/2012 Enable Editing Notes and Special Inst. for Completed Packs [Start]
      *STORE .T. TO .cmdSelect.ENABLED,.cmdSelAll.ENABLED,.cmdInvert.ENABLED,;
      .cmdSelNone.ENABLED,.cmdStyle.ENABLED,.cmdApply.ENABLED,;
      .txtCartFrom.ENABLED,.txtCartTo.ENABLED,.txtPaletteNo.ENABLED,;
      .txtQtyPerCart.ENABLED,.txtUnitWeight.ENABLED,.cmdPack.ENABLED
      STORE IIF(loformset.ActiveMode='E' .AND. Pack_Hdr.STATUS = 'C', .F., .T.) TO .cmdSelect.ENABLED,.cmdSelAll.ENABLED,.cmdInvert.ENABLED,;
        .cmdSelNone.ENABLED,.cmdStyle.ENABLED,.cmdApply.ENABLED,;
        .txtCartFrom.ENABLED,.txtCartTo.ENABLED,.txtPaletteNo.ENABLED,;
        .txtQtyPerCart.ENABLED,.txtUnitWeight.ENABLED,.cmdPack.ENABLED
      *: B610016,1 SAB 07/16/2012 Enable Editing Notes and Special Inst. for Completed Packs [End]
    ENDWITH
  ENDIF
ENDIF

SELECT (lnAlias)

*!********************************************************************************
*! Name      : lfnewPckLn
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   :
*!********************************************************************************
*! Parameters: loFormSet : FormSet
*!********************************************************************************
FUNCTION lfnewPckLn
LPARAMETERS loFormSet,llFrmPikLn


lcOrderNo = loFormSet.AriaForm1.kbOrderNo.keytextbox.VALUE
lcStore   = loFormSet.AriaForm1.kbStore.keytextbox.VALUE
lcPkTktNo = PADR(loFormSet.AriaForm1.kbPkTktNo.keytextbox.VALUE ,6)
lcPckNo   = loFormSet.AriaForm1.kbPackNo.keytextbox.VALUE

*: B608242,1 MMT 08/27/2007 fix bug of wrong style weight while creating new packing list[Start]
SELECT Pack_Hdr
IF EMPTY(SET("Relation"))
  SET RELATION TO 'O'+ORDER INTO OrdHdr ADDITIVE
ENDIF
SELECT Pack_Lin

*B608456,1 WAM 02/27/2008 Fix error variable oFormset not found when copy from another packing list
*B608456,1 WAM 02/27/2008 Relation between PACK_LIN and ORDLINE is not needed
*!*	IF EMPTY(SET("RELATION"))
*!*	  SET RELATION TO 'O'+;
*!*	  loFormSet.ariaForm1.kbOrderNo.keytextbox.Value+;
*!*	  loFormSet.ariaForm1.kbStore.keytextbox.Value+Style INTO OrdLine ADDITIVE
*!*	ENDIF
*B608456,1 WAM 02/27/2008 (End)

*: B608242,1 MMT 08/27/2007 fix bug of wrong style weight while creating new packing list[End]

IF llFrmPikLn
  SELECT PikLine
  loFormSet.PikLine.SEEK(lcPkTktNo)
ELSE
  SELECT "OrdLine"
  loFormSet.OrdLine.SEEK('O'+lcOrderNo+lcStore)

  *: B608242,1 MMT 08/27/2007 fix bug of wrong style weight while creating new packing list[Start]
  IF EMPTY(SET("Filter"))
    SET FILTER TO cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = "O"
  ENDIF
  IF EMPTY(SET("Relation"))
    SET RELATION TO 'S'+SCALE INTO SCALE ADDITIVE
    SET RELATION TO STYLE INTO STYLE ADDITIVE
  ENDIF
  *: B608242,1 MMT 08/27/2007 fix bug of wrong style weight while creating new packing list[End]
ENDIF

IF loFormSet.ActiveMode = 'A' AND loFormSet.llNew
  WAIT WINDOW LANG_ManulPL_WtWndLines NOWAIT
ENDIF
loFormSet.Pack_Lin.SetOrder('PackStyle')
SCAN REST &lcExp
  STORE 0 TO lnAvlQty,lnOQty
  lnTotPck = OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8
  loFormSet.SCALE.SEEK('S'+IIF(llFrmPikLn,PikLine.SCALE,ORDLINE.SCALE))
  SELECT (loFormSet.lcPckLin)
  FOR lnCntr = 1 TO SCALE.CNT
    lcCntr = STR(lnCntr,1)

    lnAvlQty = IIF(EMPTY(lcPkTktNo),OrdLine.Qty&lcCntr,;
      IIF(llFrmPikLn,PikLine.Pik&lcCntr,OrdLine.Pik&lcCntr))
    lnOQty   = MAX(0,lnAvlQty - Ordline.nPck&lcCntr)
    IF lnOQty > 0 OR lnAvlQty > 0
      SELECT (loFormSet.lcPckLin)
      APPEND BLANK
      REPLACE STYLE      WITH IIF(llFrmPikLn,PikLine.STYLE,OrdLine.STYLE),;
        SCALE      WITH SCALE.SCALE,;
        Piktkt     WITH IIF(llFrmPikLn,PikLine.Piktkt,OrdLine.Piktkt),;
        SzCnt      WITH SCALE.CNT,;
        StyWgh     WITH STYLE.nStyWeight,;
        OrgStyWgh  WITH STYLE.nStyWeight,;
        nOrdLineNo WITH IIF(llFrmPikLn,PikLine.LINENO,OrdLine.LINENO),;
        lPicked    WITH IIF(llFrmPikLn,PikLine.Picked,OrdLine.Picked),;
        AvlQty     WITH lnAvlQty,;
        OrdQty     WITH IIF(llFrmPikLn,PikLine.Qty&lcCntr,OrdLine.Qty&lcCntr)
      REPLACE PrePak     WITH IIF(llFrmPikLn,PikLine.Prepak,OrdLine.Prepak),;
        PpQty      WITH IIF(llFrmPikLn,PikLine.PpQty,OrdLine.PpQty)
      REPLACE AvlTotQty WITH lnAvlQty,;
        OrdTotQty WITH OrdQty,;
        OQty      WITH lnOQty,;
        OTotQty   WITH MAX(0,OQty),;
        PQty      WITH OrdLine.nPck&lcCntr,;
        PTotQty   WITH PQty,;
        OTotQty   WITH OQty,;
        PWgh      WITH IIF(lnTotPck > 0,(OrdLine.nPWght/lnTotPck)* PQty,0),;
        PTotWgh   WITH PWgh,;
        OrgPQty   WITH OrdLine.nPck&lcCntr,;
        OrgPwgh   WITH OrdLine.nPwght
      REPLACE OrgOrd     WITH IIF(llFrmPikLn,PikLine.Qty&lcCntr,OrdLine.Qty&lcCntr),;
        OrgTotOrd  WITH OrgOrd,;
        PACK_ID    WITH IIF(llFrmPikLn,PikLine.PACK_ID , OrdLine.PACK_ID),;
        cPkColor   WITH IIF(llFrmPikLn,PikLine.cPkColor, OrdLine.cPkColor),;
        cPckSize   WITH IIF(llFrmPikLn,PikLine.cPckSize, OrdLine.cPckSize),;
        cPkVersion WITH IIF(llFrmPikLn,PikLine.cPkVersion, OrdLine.cPkVersion),;
        Dyelot     WITH IIF(llFrmPikLn , PikLine.Dyelot , OrdLine.Dyelot)

      REPLACE cSizeCod   WITH SCALE.Sz&lcCntr,;
        cOpen      WITH IIF(OQty > 0 AND AvlQty > 0,'Y','N'),;
        cSizeNo    WITH lcCntr ,;
        cPackId    WITH IIF(!EMPTY(Pack_Id),;
        ALLTRIM(Pack_Id)+'-'+ ALLTRIM(cPkColor)+'-'+;
        ALLTRIM(lfGetSize(cPckSize,loFormSet))+'-'+;
        ALLTRIM(cPkVersion),Pack_Id+'-'+cPkColor+'-'+;
        lfGetSize(cPckSize,loFormSet)+'-'+cPkVersion)

      *! E302698,1 MMT 05/25/2010 Display SKU# in Style Detail and Carton Detail grids[Start]
      REPLACE SKUNUM WITH lfGetSKU(loFormSet)
      *! E302698,1 MMT 05/25/2010 Display SKU# in Style Detail and Carton Detail grids[End]

      loFormSet.SCALE.SEEK('S'+IIF(llFrmPikLn,PikLine.SCALE,ORDLINE.SCALE))
      SELECT (loFormSet.lcPckLin)
    ENDIF
  ENDFOR
  loFormSet.llAnyRec = .T.
ENDSCAN


*!********************************************************************************
*! Name      : lfGetPckLn
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   :
*!********************************************************************************
*! Parameters: loFormSet : FormSet
*!********************************************************************************
FUNCTION lfGetPckLn
LPARAMETERS loFormSet,llFrmPikLn

lcOrderNo = loFormSet.AriaForm1.kbOrderNo.keytextbox.VALUE
lcStore   = loFormSet.AriaForm1.kbStore.keytextbox.VALUE
lcPkTktNo = PADR(loFormSet.AriaForm1.kbPkTktNo.keytextbox.VALUE ,6)
lcPckNo   = loFormSet.AriaForm1.kbPackNo.keytextbox.VALUE
SET ORDER TO (loFormSet.lcPakIndxSt) IN (loFormSet.lcPckLin)
loFormSet.Pack_Lin.SetOrder('PackStyle')
SELECT Pack_Lin
lOFormSet.Pack_Lin.SEEK(lcPckNo)
SCAN REST WHILE Pack_No+STR(No_Cart,4)+STYLE+Dyelot = lcPckNo
  IF llFrmPikLn
    SELECT PikLine
    loFormSet.PikLine.SEEK(lcPkTktNo+lcOrderNo+STR(Pack_Lin.nOrdLineNo,6))
  ELSE
    SELECT "OrdLine"
    loFormSet.OrdLine.SEEK('O'+lcOrderNo+lcStore+PACK_LIN.STYLE+STR(Pack_Lin.nOrdLineNo,6))
  ENDIF
  lcMastFile = IIF(llFrmPikLn,'PikLine','OrdLine')
  STORE 0 TO lnAvlQty,lnOQty
  lnTotPck = OrdLine.nPck1+OrdLine.nPck2+OrdLine.nPck3+OrdLine.nPck4+OrdLine.nPck5+OrdLine.nPck6+OrdLine.nPck7+OrdLine.nPck8
  loFormSet.SCALE.SEEK('S'+IIF(llFrmPikLn,PikLine.SCALE,ORDLINE.SCALE))
  SELECT (loFormSet.lcPckLin)
  FOR lnCntr = 1 TO SCALE.CNT
    lcCntr = STR(lnCntr,1)
    IF Pack_lin.Qty&lcCntr = 0
      LOOP
    ENDIF
    lnAvlQty = IIF(EMPTY(lcPkTktNo),OrdLine.Qty&lcCntr,;
      IIF(llFrmPikLn,PikLine.Pik&lcCntr,OrdLine.Pik&lcCntr))
    lnOQty   = MAX(0,lnAvlQty - Ordline.nPck&lcCntr)
    *: B610016,1 SAB 07/16/2012 Enable Editing Notes and Special Inst. for Completed Packs [Start]
    lnOQty = IIF(llFrmPikLn, 0, lnOQty)
    *: B610016,1 SAB 07/16/2012 Enable Editing Notes and Special Inst. for Completed Packs [End]
    IF lnOQty > 0 OR lnAvlQty > 0
      SELECT (loFormSet.lcPckLin)
      IF !SEEK(&lcMastFile..PACK_ID+&lcMastFile..cPkColor+&lcMastFile..cPckSize+&lcMastFile..cPkVersion+&lcMastFile..STYLE+;
          &lcMastFile..Dyelot+STR(&lcMastFile..LINENO,6)+lcCntr)
        APPEND BLANK
      ENDIF
      REPLACE STYLE      WITH IIF(llFrmPikLn,PikLine.STYLE,OrdLine.STYLE),;
        SCALE      WITH SCALE.SCALE,;
        Piktkt     WITH IIF(llFrmPikLn,PikLine.Piktkt,OrdLine.Piktkt),;
        SzCnt      WITH SCALE.CNT,;
        StyWgh     WITH STYLE.nStyWeight,;
        OrgStyWgh  WITH STYLE.nStyWeight,;
        nOrdLineNo WITH IIF(llFrmPikLn,PikLine.LINENO,OrdLine.LINENO),;
        lPicked    WITH IIF(llFrmPikLn,PikLine.Picked,OrdLine.Picked),;
        AvlQty     WITH lnAvlQty,;
        OrdQty     WITH IIF(llFrmPikLn,PikLine.Qty&lcCntr,OrdLine.Qty&lcCntr)
      REPLACE PrePak     WITH IIF(llFrmPikLn,PikLine.Prepak,OrdLine.Prepak),;
        PpQty      WITH IIF(llFrmPikLn,PikLine.PpQty,OrdLine.PpQty)
      *: B610016,1 SAB 07/16/2012 Enable Editing Notes and Special Inst. for Completed Packs [Start]
      *REPLACE AvlTotQty WITH lnAvlQty,;
      OrdTotQty WITH OrdQty,;
      OQty      WITH lnOQty,;
      OTotQty   WITH MAX(0,OQty),;
      PQty      WITH OrdLine.nPck&lcCntr,;
      PTotQty   WITH PQty,;
      OTotQty   WITH OQty,;
      PWgh      WITH IIF(lnTotPck > 0,(OrdLine.nPWght/lnTotPck)* PQty,0),;
      PTotWgh   WITH PWgh,;
      OrgPQty   WITH OrdLine.nPck&lcCntr,;
      OrgPwgh   WITH OrdLine.nPwght
      REPLACE AvlTotQty WITH lnAvlQty,;
        OrdTotQty WITH OrdQty,;
        OQty      WITH lnOQty,;
        OTotQty   WITH MAX(0,OQty),;
        PQty      WITH IIF(llFrmPikLn, Pack_lin.Qty&lcCntr, OrdLine.nPck&lcCntr),;
        PTotQty   WITH PQty,;
        OTotQty   WITH OQty,;
        PWgh      WITH IIF(lnTotPck > 0,(OrdLine.nPWght/lnTotPck)* PQty,0),;
        PTotWgh   WITH PWgh,;
        OrgPQty   WITH OrdLine.nPck&lcCntr,;
        OrgPwgh   WITH OrdLine.nPwght
      *: B610016,1 SAB 07/16/2012 Enable Editing Notes and Special Inst. for Completed Packs [End]
      REPLACE OrgOrd     WITH IIF(llFrmPikLn,PikLine.Qty&lcCntr,OrdLine.Qty&lcCntr),;
        OrgTotOrd  WITH OrgOrd,;
        PACK_ID    WITH IIF(llFrmPikLn,PikLine.PACK_ID , OrdLine.PACK_ID),;
        cPkColor   WITH IIF(llFrmPikLn,PikLine.cPkColor, OrdLine.cPkColor),;
        cPckSize   WITH IIF(llFrmPikLn,PikLine.cPckSize, OrdLine.cPckSize),;
        cPkVersion WITH IIF(llFrmPikLn,PikLine.cPkVersion, OrdLine.cPkVersion),;
        Dyelot     WITH IIF(llFrmPikLn , PikLine.Dyelot , OrdLine.Dyelot)

      REPLACE cSizeCod   WITH SCALE.Sz&lcCntr,;
        cOpen      WITH IIF(OQty > 0 AND AvlQty > 0,'Y','N'),;
        cSizeNo    WITH lcCntr ,;
        cPackId    WITH IIF(!EMPTY(Pack_Id),;
        ALLTRIM(Pack_Id)+'-'+ ALLTRIM(cPkColor)+'-'+;
        ALLTRIM(lfGetSize(cPckSize,loFormSet))+'-'+;
        ALLTRIM(cPkVersion),Pack_Id+'-'+cPkColor+'-'+;
        lfGetSize(cPckSize,loFormSet)+'-'+cPkVersion)

      *! E302698,1 MMT 05/25/2010 Display SKU# in Style Detail and Carton Detail grids[Start]
      REPLACE SKUNUM WITH lfGetSKU(loFormSet)
      *! E302698,1 MMT 05/25/2010 Display SKU# in Style Detail and Carton Detail grids[End]

      loFormSet.SCALE.SEEK('S'+IIF(llFrmPikLn,PikLine.SCALE,ORDLINE.SCALE))
      SELECT (loFormSet.lcPckLin)
    ENDIF
  ENDFOR
  loFormSet.llAnyRec = .T.
ENDSCAN

*!********************************************************************************
*! Name      : lfCkPckMch
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : Check if the packing list which copy from is match the order lines
*!********************************************************************************
*! Parameters: loFormSet : FormSet
*!********************************************************************************
FUNCTION lfCkPckMch
LPARAMETERS loFormSet
PRIVATE lcCurAlias , lcOrderNo , lcPreOrder , llLineFund

lcCurAlias = ALIAS()

*B608456,1 WAM 02/27/2008 The following lines are commented out and rewritten because they caused an infinite loop
*!*	lcPreOrder = ORDER('ORDLINE')
*!*	STORE .F. TO llLineFund
*!*	lcOrderNo = PADR(loFormSet.AriaForm1.kbOrderNo.keytextbox.VALUE,6)
*!*	loFormSet.PACK_LIN.SetOrder('Packstyle')
*!*	loFormSet.OrdLine.SetOrder('ordline')
*!*	SELECT ORDLINE
*!*	loFormSet.ORDLINE.SEEK('O'+lcOrderNo)
*!*	SCAN REST WHILE cordtype+ORDER+STR(LINENO,6) = 'O'+lcOrderNo
*!*	  IF loFormSet.PACK_LIN.SEEK(loFormSet.lcPackNo)
*!*	    SELECT PACK_LIN
*!*	    LOCATE REST WHILE pack_no+STR(no_cart,4)+STYLE+dyelot = loFormSet.lcPackNo FOR STYLE = ORDLINE.STYLE AND Dyelot = ORDLINE.Dyelot AND ;
*!*	      ORDLINE.PAck_id = Pack_Id AND ORDLINE.cPkColor = cPkColor AND ORDLINE.cPckSize = cPckSize AND ;
*!*	      ORDLINE.cpkVersion = cPkVersion
*!*	    IF !FOUND()
*!*	      llNtFound = .T.
*!*	    ELSE
*!*	      llLineFund = .T.
*!*	    ENDIF
*!*	  ENDIF
*!*	ENDSCAN
*!*	loFormSet.OrdLine.SetOrder(lcPreOrder)

*B608456,1 WAM 02/27/2008 The following lines are commented out and rewritten because they caused an infinite loop
STORE .F. TO llLineFund
lcOrderNo = PADR(loFormSet.AriaForm1.kbOrderNo.keytextbox.VALUE,6)
lcStore   = loFormSet.AriaForm1.kbStore.keytextbox.VALUE
lcPkTktNo = PADR(loFormSet.AriaForm1.kbPkTktNo.keytextbox.VALUE ,6)

loFormSet.PACK_LIN.SetOrder('Packstyle')

SELECT ORDLINE
loFormSet.ORDLINE.SEEK('O'+lcOrderNo+lcStore)
SCAN REST WHILE cordtype+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+lcOrderNo+lcStore FOR pikTkt = lcPkTktNo
  IF loFormSet.PACK_LIN.SEEK(loFormSet.lcPackNo)
    SELECT PACK_LIN
    LOCATE REST WHILE pack_no+STR(no_cart,4)+STYLE+dyelot = loFormSet.lcPackNo FOR STYLE = ORDLINE.STYLE AND Dyelot = ORDLINE.Dyelot AND ;
      ORDLINE.Pack_Id = Pack_Id AND ORDLINE.cPkColor = cPkColor AND ORDLINE.cPckSize = cPckSize AND ;
      ORDLINE.cpkVersion = cPkVersion
    IF !FOUND()
      llNtFound = .T.
    ELSE
      llLineFund = .T.
    ENDIF
  ENDIF
ENDSCAN
*B608456,1 WAM 02/27/2008 (End)

SELECT (lcCurAlias)
RETURN llLineFund

*!********************************************************************************
*! Name      : lfGetPkInf
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : Get packing list lines
*!********************************************************************************
*! Parameters: loFormSet : FormSet
*!********************************************************************************
FUNCTION lfGetPkInf
LPARAMETERS loFormSet

lcOrderNo = loFormSet.AriaForm1.kbOrderNo.keytextbox.VALUE
lcStore   = loFormSet.AriaForm1.kbStore.keytextbox.VALUE
lcPkTktNo = PADR(loFormSet.AriaForm1.kbPkTktNo.keytextbox.VALUE ,6)
lcPckNo   = loFormSet.AriaForm1.kbPackNo.keytextbox.VALUE

SELECT Pack_Lin
loFormSet.Pack_Lin.SetOrder('PackStyle')
SET ORDER TO (loFormSet.lcPakIndxLn) IN (loFormSet.lcPckLin)
IF loFormSet.Pack_Lin.SEEK(lcPckNo)
  LOCAL lcTmpPack
  lcTmpPack  = lcPckNo
  SET ORDER TO (loFormSet.lcPakIndxSt) IN (loFormSet.lcPckLin)
  SET ORDER TO (loFormSet.lcCtnDtl) IN (loFormSet.lcCtnDtl)
  SCAN REST WHILE Pack_No+STR(No_Cart,4)+STYLE+Dyelot = lcTmpPack

    IF llFrmPikLn
      SELECT PikLine
      loFormSet.PikLine.SEEK(lcPkTktNo+lcOrderNo+STR(Pack_Lin.nOrdLineNo,6))
    ELSE
      SELECT "OrdLine"
      loFormSet.OrdLine.SEEK('O'+lcOrderNo+lcStore+PACK_LIN.STYLE+STR(Pack_Lin.nOrdLineNo,6))
    ENDIF

    loFormSet.SCALE.SEEK('S'+IIF(llFrmPikLn,PikLine.SCALE,ORDLINE.SCALE))
    FOR lnI = 1 TO SCALE.CNT
      lcI = STR(lnI,1)
      IF Pack_Lin.Qty&lcI = 0
        LOOP
      ENDIF
      *E037427,1 WAM commented out
      *!*	      IF SEEK(Pack_Lin.Pack_ID+Pack_Lin.cPkColor+Pack_Lin.cPckSize+Pack_Lin.cPkVersion+Pack_Lin.Style+Pack_Lin.Dyelot+;
      *!*	   		   STR(Pack_Lin.nOrdLineNo,6)+lcI,loFormSet.lcPckLin)
      *!*	        SELECT (loFormSet.lcPckLin)
      *!*	        REPLACE PQty       WITH PQty    + Pack_Lin.Qty&lcI,;
      *!*	                OQty       WITH MAX(OrdQty -PQty,0),;
      *!*		            PWgh       WITH (Pack_lin.Weight/Pack_Lin.TotQty)* PQty,;
      *!*	      	      OrdQty     WITH Pack_Lin.Qty&lcI,;
      *!*		            cCrtnVlTyp WITH Pack_Lin.cCrtnVlTyp
      *!*	      ENDIF

      =SEEK(Pack_Lin.Pack_ID+Pack_Lin.cPkColor+Pack_Lin.cPckSize+Pack_Lin.cPkVersion+Pack_Lin.STYLE+Pack_Lin.Dyelot+;
        STR(Pack_Lin.nOrdLineNo,6)+lcI,loFormSet.lcPckLin)
      *E037427,1 WAM (End)
      SELECT (loFormSet.lcCtnDtl)
      APPEND BLANK
      REPLACE STYLE       WITH Pack_Lin.STYLE,;
        SzCnt       WITH EVALUATE(loFormSet.lcPckLin+'.SzCnt'),;
        cStatus     WITH "A",;
        nOrdLineNo  WITH Pack_Lin.nOrdLineNo,;
        PackLineNo  WITH Pack_Lin.Line_No,;
        Cart_No     WITH Pack_Lin.No_Cart,;
        Qty         WITH Pack_Lin.Qty&lcI,;
        TotQty      WITH Qty,;
        Dyelot      WITH Pack_Lin.Dyelot,;
        PrePak      WITH EVALUATE(loFormSet.lcPckLin+'.PrePak'),;
        PpQty       WITH EVALUATE(loFormSet.lcPckLin+'.PpQty')
      REPLACE SIZE       WITH IIF(EVALUATE(loFormSet.lcCtnDtl+'.Qty') > 0,EVALUATE(loFormSet.lcPckLin+'.cSizeCod'),SIZE),;
        Br	     WITH EVALUATE(loFormSet.lcCtnDtl+'.Qty')> 0,;
        Weight     WITH EVALUATE(loFormSet.lcCtnDtl+'.Qty')*(Pack_Lin.Weight/Pack_Lin.TotQty),;
        OrgWgh     WITH EVALUATE(loFormSet.lcPckLin+'.OrgStyWgh'),;
        TotWeight  WITH Weight,;
        cSizeNo	WITH lcI
      REPLACE PACK_ID    WITH Pack_Lin.PACK_ID,;
        cPkColor   WITH Pack_Lin.cPkColor,;
        cPCkSize   WITH Pack_Lin.cPckSize,;
        cPKVersion WITH Pack_Lin.cPkVersion,;
        nPackNO    WITH PACK_LIN.nPackNO,;
        cPackId    WITH IIF(!EMPTY(Pack_Id),;
        ALLTRIM(Pack_Id)+'-'+ ALLTRIM(cPkColor)+'-'+;
        ALLTRIM(lfGetSize(cPckSize,loFormSet))+'-'+;
        ALLTRIM(cPkVersion),Pack_Id+'-'+cPkColor+'-'+;
        lfGetSize(cPckSize,loFormSet)+'-'+cPkVersion)

      *! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[Start]
      IF loFormSet.llUPCInst AND loFormSet.STYLEUPC.SEEK(Pack_Lin.STYLE +cSizeNo,'StyleUpc')
        REPLACE cUpc WITH StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3
      ENDIF
      *! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[End]

      *! E302698,1 MMT 05/25/2010 Display SKU# in Style Detail and Carton Detail grids[Start]
      REPLACE SKUNUM WITH lfGetSKU(loFormSet)
      *! E302698,1 MMT 05/25/2010 Display SKU# in Style Detail and Carton Detail grids[End]

      IF SEEK(STR(Pack_Lin.No_Cart,4),loFormSet.lcCtnHdr)
        SELECT (loFormSet.lcCtnHdr)
        REPLACE TotPcs WITH TotPcs + EVALUATE(loFormSet.lcCtnDtl+'.Qty'),;
          TotWgh WITH TotWgh + EVALUATE(loFormSet.lcCtnDtl+'.Weight')
      ELSE
        IF EVALUATE(loFormSet.lcCtnDtl+'.Qty') > 0
          loFormSet.lnMaxCtn = MAX(loFormSet.lnMaxCtn,Pack_Lin.No_Cart)
          INSERT INTO (loFormSet.lcCtnHdr) (Cart_No,Pal_No,TotPcs,TotWgh,cCarrCtnID,;
            cCrtnVlTyp,EMPTY);
            VALUES (Pack_Lin.No_Cart,Pack_Lin.NPltNo,;
            EVALUATE(loFormSet.lcCtnDtl+'.Qty'),;
            EVALUATE(loFormSet.lcCtnDtl+'.Weight'),;
            PACK_LIN.cCarrCtnID,Pack_Lin.cCrtnVlTyp,'N')
        ENDIF
      ENDIF
    ENDFOR
  ENDSCAN
ENDIF

*!********************************************************************************
*! Name      : lfCpyPkInf
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : Get packing list lines
*!********************************************************************************
*! Parameters: loFormSet : FormSet
*!********************************************************************************
FUNCTION lfCpyPkInf
LPARAMETERS loFormSet

SELECT (loFormSet.lcPckLin)
SCAN
  IF loFormSet.PACK_LIN.SEEK(loFormSet.lcPackNo)
    SELECT PACK_LIN

    *E037427,1 WAM 05/05/2005 Fix bugs in copy from pack
    *LOCATE REST WHILE pack_no+STR(no_cart,4)+style+dyelot = loFormSet.lcPackNo FOR STYLE = EVALUATE(loFormSet.lcPckLin+'.Style') AND;
    Dyelot = EVALUATE(loFormSet.lcPckLin+'.Dyelot') AND Pack_Id = EVALUATE(loFormSet.lcPckLin+'.PAck_id')AND;
    cPkColor = EVALUATE(loFormSet.lcPckLin+'.cPkColor') AND cPckSize = EVALUATE(loFormSet.lcPckLin+'.cPckSize') AND ;
    cpkVersion = EVALUATE(loFormSet.lcPckLin+'.cPkVersion')
    *IF FOUND()
    *loFormSet.Scale.SEEK('S'+EVALUATE(loFormSet.lcPckLin+'.Scale'))
    *FOR lnI = 1 TO Scale.Cnt
    *  lcI = STR(lnI,1)
    *  IF Pack_Lin.Qty&lcI = 0
    *    LOOP
    *  ENDIF
    SCAN REST WHILE pack_no+STR(no_cart,4)+STYLE+dyelot = loFormSet.lcPackNo FOR STYLE = EVALUATE(loFormSet.lcPckLin+'.Style') AND;
        Dyelot = EVALUATE(loFormSet.lcPckLin+'.Dyelot') AND Pack_Id = EVALUATE(loFormSet.lcPckLin+'.PAck_id')AND;
        cPkColor = EVALUATE(loFormSet.lcPckLin+'.cPkColor') AND cPckSize = EVALUATE(loFormSet.lcPckLin+'.cPckSize') AND ;
        cpkVersion = EVALUATE(loFormSet.lcPckLin+'.cPkVersion')

      lcI = ALLTRIM(EVALUATE(loFormSet.lcPckLin+'.cSizeNo'))
      *E037427,1 WAM 05/05/2005 (End)
      lnQty        = Pack_Lin.Qty&LcI
      lnOrdQty     = EVALUATE(loFormSet.lcPckLin + '.OrdQty')
      llClearPrPck = .F.
      IF Pack_Lin.Qty&lcI > EVALUATE(loFormSet.lcPckLin+'.OQty')
        *-- Packed quantity for Style/Size x/x exceeds ordered quantity.;
        Do you want to modify the orderd quantity?
        *-- <Yes>,<No>
        lnChoice = gfModalGen("QRM44034B00006","Dialog",Pack_Lin.STYLE + '/' + EVAL(loFormSet.lcPckLin + '.cSizeCod'))
        *-- 44134 : This line has a prepack. Increasing the Quantity will release the prepack.
        *-- 00012 : <Procced> <Cancel>
        IF lnChoice = 1
          IF !EMPTY(EVALUATE(loFormSet.lcPckLin + '.PrePak'))
            llClearPrPck = (gfModalGen("QRM44134B00012","Dialog") = 1)
            IF !llClearPrPck
              lnChoice = 2
            ENDIF
          ENDIF
        ENDIF
        lnQty    = IIF(lnChoice=1,Pack_Lin.Qty&LcI,EVALUATE(loFormSet.lcPckLin + '.OQty'))
        lnOrdQty = IIF(lnChoice=1,Pack_Lin.Qty&LcI,EVALUATE(loFormSet.lcPckLin + '.OrdQty'))
      ENDIF
      SELECT (loFormSet.lcPckLin)
      REPLACE PQty       WITH PQty    + lnQty,;
        OQty       WITH MAX(OrdQty -PQty,0),;
        PWgh       WITH (Pack_lin.Weight/Pack_Lin.TotQty)* PQty,;
        cCrtnVlTyp WITH Pack_Lin.cCrtnVlTyp,;
        OrdQty     WITH lnOrdQty
      IF llClearPrPck
        REPLACE PrePak WITH "",;
          PpQty  WITH 0
      ENDIF
      IF lnQty <> 0
        SELECT (loFormSet.lcCtnDtl)
        APPEND BLANK
        REPLACE STYLE       WITH Pack_Lin.STYLE,;
          SzCnt       WITH EVALUATE(loFormSet.lcPckLin+'.SzCnt'),;
          cStatus     WITH "A",;
          nOrdLineNo  WITH EVALUATE(loFormSet.lcPckLin+'.nOrdLineNo'),;
          PackLineNo  WITH Pack_Lin.Line_No,;
          Cart_No     WITH Pack_Lin.No_Cart,;
          Qty         WITH lnQty,;
          TotQty      WITH Qty,;
          Dyelot      WITH Pack_Lin.Dyelot,;
          PrePak      WITH EVALUATE(loFormSet.lcPckLin+'.PrePak'),;
          PpQty       WITH EVALUATE(loFormSet.lcPckLin+'.PpQty')
        REPLACE SIZE       WITH IIF(Qty > 0,EVALUATE(loFormSet.lcPckLin+'.cSizeCod'),;
          SIZE),;
          Br	     WITH Qty > 0,;
          Weight     WITH Qty*(Pack_Lin.Weight/Pack_Lin.TotQty),;
          OrgWgh     WITH EVALUATE(loFormSet.lcPckLin+'.OrgStyWgh'),;
          TotWeight  WITH Weight,;
          cSizeNo	WITH lcI
        REPLACE PACK_ID    WITH Pack_Lin.PACK_ID,;
          cPkColor   WITH Pack_Lin.cPkColor,;
          cPCkSize   WITH Pack_Lin.cPckSize,;
          cPKVersion WITH Pack_Lin.cPkVersion,;
          nPackNO    WITH PACK_LIN.nPackNO,;
          cPackId    WITH IIF(!EMPTY(Pack_Id),;
          ALLTRIM(Pack_Id)+'-'+ ALLTRIM(cPkColor)+'-'+;
          ALLTRIM(lfGetSize(cPckSize,loFormSet))+'-'+;
          ALLTRIM(cPkVersion),Pack_Id+'-'+cPkColor+'-'+;
          lfGetSize(cPckSize,loFormSet)+'-'+cPkVersion)

        *! E302698,1 MMT 05/25/2010 Display SKU# in Style Detail and Carton Detail grids[Start]
        REPLACE SKUNUM WITH lfGetSKU(loFormSet)
        *! E302698,1 MMT 05/25/2010 Display SKU# in Style Detail and Carton Detail grids[End]

        IF SEEK(STR(Pack_Lin.No_Cart,4),loFormSet.lcCtnHdr)
          SELECT (loFormSet.lcCtnHdr)
          REPLACE TotPcs WITH TotPcs + EVALUATE(loFormSet.lcCtnDtl+'.Qty'),;
            TotWgh WITH TotWgh + EVALUATE(loFormSet.lcCtnDtl+'.Weight')
          loFormSet.lnPackQty = loFormSet.lnPackQty + EVALUATE(loFormSet.lcCtnDtl+'.Qty')
          loFormSet.lnPackWgh = loFormSet.lnPackWgh + EVALUATE(loFormSet.lcCtnDtl+'.Weight')

        ELSE
          IF EVALUATE(loFormSet.lcCtnDtl+'.Qty') > 0
            loFormSet.lnMaxCtn = MAX(loFormSet.lnMaxCtn,Pack_Lin.No_Cart)
            INSERT INTO (loFormSet.lcCtnHdr) (Cart_No,Pal_No,TotPcs,TotWgh,cCarrCtnID,;
              cCrtnVlTyp,EMPTY);
              VALUES (Pack_Lin.No_Cart,Pack_Lin.NPltNo,;
              EVALUATE(loFormSet.lcCtnDtl+'.Qty'),;
              EVALUATE(loFormSet.lcCtnDtl+'.Weight'),;
              PACK_LIN.cCarrCtnID,Pack_Lin.cCrtnVlTyp,'N')
            loFormSet.lnPackCtn = loFormSet.lnPackCtn + 1
            STORE loFormSet.lnPackCtn + 1 TO loFormSet.lnFrom,loFormSet.lnTo
            loFormSet.lnPackQty = loFormSet.lnPackQty + EVALUATE(loFormSet.lcCtnDtl+'.Qty')
            loFormSet.lnPackWgh = loFormSet.lnPackWgh + EVALUATE(loFormSet.lcCtnDtl+'.Weight')
          ENDIF
        ENDIF
      ENDIF
      *E037427,1 WAM 05/05/2005 Fix bugs in copy from pack
      *ENDFOR
      *ENDIF
    ENDSCAN
    *E037427,1 WAM 05/05/2005 (End)
  ENDIF
ENDSCAN

*!*************************************************************
*! Name      : lfDtlBrow
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
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
  IF loFormSet.lcPLSortBy = 'L'
    SET ORDER TO (loFormSet.lcPakIndxLn)
  ELSE
    SET ORDER TO (loFormSet.lcPakIndxSt)
  ENDIF
ENDIF

lcTemFile = IIF(!loFormSet.llShoPckSu,loFormSet.lcSumPck,loFormSet.lcPckLin)

WITH loFormSet.AriaForm1.pgfPacking.DETAIL.grdDetail
  .RECORDSOURCE = ''
  IF loFormSet.llShoPckSu
    SELECT (loFormSet.lcPckLin)
    SET FILTER TO
    IF loFormSet.llFilter
      IF loFormSet.llShwOpn
        SET FILTER TO cOpen = 'Y' AND llFilter
      ELSE
        SET FILTER TO llFilter
      ENDIF
    ELSE
      IF loFormSet.llShwOpn
        SET FILTER TO cOpen = 'Y'
      ELSE
        SET FILTER TO
      ENDIF
    ENDIF
    LOCATE
    IF EOF()
      RETURN
    ENDIF

    .RECORDSOURCE = loFormSet.lcPckLin
    *-- Select
    .Column1.Header1.CAPTION = ""
    .Column1.CONTROLSOURCE  = loFormSet.lcPckLin +'.Selected'
    *-- Pack-Id
    .Column2.CONTROLSOURCE  = loFormSet.lcPckLin + '.cPackId'
    .Column2.VISIBLE = IIF(loFormSet.llUsePack, .T., .F.)
    *-- Style
    .Column3.CONTROLSOURCE  = loFormSet.lcPckLin + '.Style'
    .Column3.Header1.CAPTION = loFormSet.lcStyTtl
    .Column3.VISIBLE = .T.
    *-- Configuration
    .Column4.Header1.CAPTION = IIF(loFormSet.llUseConfg,LANG_ManulPL_CptConfig,LANG_ManulPL_CptDyelot)
    .Column4.CONTROLSOURCE   = loFormSet.lcPckLin + '.Dyelot'
    .Column4.VISIBLE 		 = IIF(loFormSet.llUseConfg OR loFormSet.llDyelot , .T., .F.)
    *-- Size
    .Column5.CONTROLSOURCE  = loFormSet.lcPckLin +'.cSizeCod'
    *-- O.Qty
    .Column6.CONTROLSOURCE = loFormSet.lcPckLin +'.OQty'
    .Column6.Header1.ALIGNMENT = 1     && Right

    *-- Qty.\Ctn
    .Column7.CONTROLSOURCE = loFormSet.lcPckLin +'.CtnQty'
    .Column7.Header1.ALIGNMENT = 1     && Right

    *-- Wgh.\Unt
    .Column8.CONTROLSOURCE = loFormSet.lcPckLin +'.StyWgh'
    .Column8.Header1.ALIGNMENT = 1     && Right

    *-- PackQty
    .Column9.CONTROLSOURCE = loFormSet.lcPckLin +'.PQty'
    .Column9.Header1.ALIGNMENT = 1     && Right

    *-- PackWght
    .Column10.CONTROLSOURCE = loFormSet.lcPckLin +'.PWgh'
    .Column10.Header1.ALIGNMENT = 1     && Right

    *! E302698,1 MMT 05/25/2010 Display SKU# in Style Detail and Carton Detail grids[Start]
    .Column11.VISIBLE = .T.
    .Column11.CONTROLSOURCE  = loFormSet.lcPckLin +'.SKUNUM'
    .Column11.COLUMNORDER = 11
    *! E302698,1 MMT 05/25/2010 Display SKU# in Style Detail and Carton Detail grids[End]

  ELSE
    SELECT (loFormSet.lcSumPck)
    IF loFormSet.llFilter
      IF loFormSet.llShwOpn
        SET FILTER TO cOpen = 'Y' AND llFilter
      ELSE
        SET FILTER TO llFilter
      ENDIF
    ELSE
      IF loFormSet.llShwOpn
        SET FILTER TO cOpen = 'Y'
      ELSE
        SET FILTER TO
      ENDIF
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
    .Column2.CONTROLSOURCE  = loFormSet.lcSumPck + '.cPack_Id'
    *-- Style
    .Column3.VISIBLE = .F.

    *E037427,1 WAM 05/05/2005 Hide dyelot column
    *-- Dyelot
    .Column4.VISIBLE = .F.
    *E037427,1 WAM 05/05/2005 (End)

    *-- Size
    .Column5.CONTROLSOURCE  = loFormSet.lcSumPck +'.cSize'
    *-- O.Qty
    .Column6.CONTROLSOURCE = loFormSet.lcSumPck +'.OTotQty'
    .Column6.Header1.ALIGNMENT = 1     && Right

    *-- Qty.\Ctn
    .Column7.CONTROLSOURCE = loFormSet.lcSumPck +'.CtnTotQty'
    .Column7.Header1.ALIGNMENT = 1     && Right

    *-- Wgh.\Unt
    .Column8.CONTROLSOURCE = loFormSet.lcSumPck +'.StyWgh'
    .Column8.Header1.ALIGNMENT = 1     && Right

    *-- PackQty
    .Column9.CONTROLSOURCE = loFormSet.lcSumPck +'.PTotQty'
    .Column9.Header1.ALIGNMENT = 1     && Right

    *-- PackWght
    .Column10.CONTROLSOURCE = loFormSet.lcSumPck +'.PTotWgh'
    .Column10.Header1.ALIGNMENT = 1    && Right

    *! E302698,1 MMT 05/25/2010 Display SKU# in Style Detail and Carton Detail grids[Start]
    .Column11.VISIBLE = .F.
    *! E302698,1 MMT 05/25/2010 Display SKU# in Style Detail and Carton Detail grids[End]
  ENDIF
  .SETALL('ReadOnly',.T.,'COLUMN')
  .Column1.READONLY = .F.
  .REFRESH()
ENDWITH
=lfwDtlBrow(loFormSet)

*!*************************************************************
*! Name      : lfwDtlBrow
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : Adjust the label of pbsel button
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfwDtlBrow
LPARAMETERS loFormSet
PRIVATE lnAlias,lnRecNo,lnSlected

lnAlias   = SELECT(0)

IF INLIST(loFormSet.ActiveMode,'V','E')
  loFormSet.ariaForm1.kbPkTktNo.keytextbox.VALUE = EVALUATE(loFormSet.lcPckLin+'.Piktkt')
ENDIF

lcTemFile = IIF(!loFormSet.llShoPckSu,loFormSet.lcSumPck,loFormSet.lcPckLin)
SELECT (lcTemFile)
lnRecNo = RECNO()
lnSlected = 0
COUNT FOR lSelect TO lnSlected
COUNT FOR !lSelect TO lnNtSlected
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF

IF loFormSet.llShoPckSu
  loFormSet.lnBrCtnQty = EVALUATE(loFormSet.lcPckLin+'.CtnQty')
  loFormSet.lnBrUntWgh = EVALUATE(loFormSet.lcPckLin+'.StyWgh')
  loFormSet.ariaForm1.pgfPacking.DETAIL.cmdSelect.CAPTION = ;
    IIF(EVALUATE(loFormSet.lcPckLin +'.Selected'),LANG_ManulPL_CptUnSel,LANG_ManulPL_CptThis)
  loFormSet.ariaForm1.pgfPacking.DETAIL.cmdPack.CAPTION = ;
    IIF(!EMPTY(EVALUATE(loFormSet.lcPckLin +'.Pack_id')),IIF(EVALUATE(loFormSet.lcPckLin +'.Selected'),LANG_ManulPL_CptUnSelP,LANG_ManulPL_CptPack),LANG_ManulPL_CptPack)
  loFormSet.ariaForm1.pgfPacking.DETAIL.cmdStyle.CAPTION = ;
    IIF(EMPTY(EVALUATE(loFormSet.lcPckLin +'.Pack_id')),IIF(EVALUATE(loFormSet.lcPckLin +'.Selected'),LANG_ManulPL_CptUnSelS,LANG_ManulPL_CptStyle),LANG_ManulPL_CptStyle)
ELSE
  loFormSet.lnBrCtnQty = EVALUATE(loFormSet.lcSumPck+'.CtnTotQty')
  loFormSet.lnBrUntWgh = EVALUATE(loFormSet.lcSumPck+'.StyWgh')
  loFormSet.ariaForm1.pgfPacking.DETAIL.cmdSelect.CAPTION = ;
    IIF(EVALUATE(loFormSet.lcSumPck +'.lSelect' ),LANG_ManulPL_CptUnSel,LANG_ManulPL_CptThis)
  loFormSet.ariaForm1.pgfPacking.DETAIL.cmdPack.CAPTION = ;
    IIF(!EMPTY(EVALUATE(loFormSet.lcSumPck +'.Pack_id')),IIF(EVALUATE(loFormSet.lcSumPck +'.lSelect'),LANG_ManulPL_CptUnSelP,LANG_ManulPL_CptPack),LANG_ManulPL_CptPack)
  loFormSet.ariaForm1.pgfPacking.DETAIL.cmdStyle.CAPTION = ;
    IIF(!EMPTY(EVALUATE(loFormSet.lcSumPck +'.Pack_id')),IIF(EVALUATE(loFormSet.lcSumPck +'.lSelect'),LANG_ManulPL_CptUnSelS,LANG_ManulPL_CptStyle),LANG_ManulPL_CptStyle)
ENDIF

*-- Refresh the objects that have control source as properties
WITH loFormSet.AriaForm1.pgfPacking.DETAIL
  .txtCartFrom.REFRESH
  .txtCartTo.REFRESH
  .txtPaletteNo.REFRESH
  .txtQtyPerCart.REFRESH
  .txtUnitWeight.REFRESH
ENDWITH

=lfSelStatus(loFormSet)
IF loFormSet.llQtyArr OR loFormSet.llWghArr
  CLEAR TYPEAHEAD
  KEYBOARD "{RIGHTARROW}{LEFTARROW}" CLEAR PLAIN
  DO CASE
  CASE loFormSet.llQtyArr
    loFormSet.ariaForm1.pgfPacking.DETAIL.txtQtyPerCart.SETFOCUS
  CASE loFormSet.llWghArr
    loFormSet.ariaForm1.pgfPacking.DETAIL.txtUnitWeight.SETFOCUS
  ENDCASE
  STORE .F. TO loFormSet.llQtyArr,loFormSet.llWghArr
ENDIF

*-- to control showing of Select by pack
lcOrdNo = loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE
WITH loFormSet.ariaForm1.pgfPacking.DETAIL
  IF loFormSet.llShoPckSu
    .cmdStyle.ENABLED      = IIF(loFormSet.ActiveMode = 'V',.F.,IIF(EMPTY(lcOrdNo ),.F.,IIF(EMPTY(Pack_ID),.T.,.F.)))
    .cmdPack.ENABLED       = IIF(loFormSet.ActiveMode = 'V',.F.,IIF(EMPTY(lcOrdNo ),.F.,IIF(EMPTY(Pack_ID),.F.,.T.)))
    .txtQtyPerCart.ENABLED = IIF(loFormSet.ActiveMode = 'V',.F.,IIF(EMPTY(lcOrdNo ),.F.,IIF(EMPTY(Pack_ID),.T.,.F.)))
    .txtUnitWeight.ENABLED = IIF(loFormSet.ActiveMode = 'V',.F.,IIF(EMPTY(lcOrdNo ),.F.,IIF(EMPTY(Pack_ID),.T.,.F.)))
    .txtCartFrom.ENABLED   = IIF(loFormSet.ActiveMode = 'V',.F.,IIF(EMPTY(lcOrdNo ),.F.,IIF(EMPTY(Pack_ID),.T.,.F.)))
    .txtCartTo.ENABLED     = IIF(loFormSet.ActiveMode = 'V',.F.,IIF(EMPTY(lcOrdNo ),.F.,IIF(EMPTY(Pack_ID),.T.,.F.)))
  ELSE
    .cmdPack.ENABLED  = .F.
    .cmdStyle.ENABLED = IIF(loFormSet.ActiveMode = 'V',.F.,;
      IIF(EMPTY(lcOrdNo ),.F.,IIF(EVALUATE(loFormSet.lcSumPck+'.llPack'),.F.,.T.)))
    .txtQtyPerCart.ENABLED = IIF(loFormSet.ActiveMode = 'V',.F.,!EMPTY(lcOrdNo))
    .txtUnitWeight.ENABLED = IIF(loFormSet.ActiveMode = 'V',.F.,!EMPTY(lcOrdNo))
    .txtCartFrom.ENABLED   = IIF(loFormSet.ActiveMode = 'V',.F.,!EMPTY(lcOrdNo))
    .txtCartTo.ENABLED     = IIF(loFormSet.ActiveMode = 'V',.F.,!EMPTY(lcOrdNo))
  ENDIF
  STORE IIF(loFormSet.ActiveMode = 'V',.F.,(lnSlected <> 0)) TO .cmdApply.ENABLED,.cmdSelNone.ENABLED
  STORE IIF(loFormSet.ActiveMode = 'V',.F.,(lnNtSlected <> 0)) TO .cmdSelAll.ENABLED
  *: B610016,1 SAB 07/16/2012 Enable Editing Notes and Special Inst. for Completed Packs [Start]
  IF loformset.activemode='E' .AND. pack_hdr.STATUS='C'
    STORE .F. TO .cmdstyle.ENABLED, .cmdpack.ENABLED, .cmdapply.ENABLED, .cmdselnone.ENABLED, .cmdselall.ENABLED, .txtqtypercart.ENABLED, .txtunitweight.ENABLED, .txtcartfrom.ENABLED, .txtcartto.ENABLED
  ENDIF
  *: B610016,1 SAB 07/16/2012 Enable Editing Notes and Special Inst. for Completed Packs [End]
ENDWITH
loFormSet.lnCtnPal  = EVALUATE(loFormSet.lcCtnHdr+'.Pal_No')

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfCtnHdrBr
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : Browse for cartons information (Carton Header)
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfCtnHdrBr
LPARAMETERS loFormSet
LOCAL lnCurAlias

lnCurAlias = SELECT(0)

WITH loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonH
  .RECORDSOURCE = ''
  SELECT (loFormSet.lcCtnHdr)
  LOCATE
  .RECORDSOURCE = loFormSet.lcCtnHdr
  *-- Cart. #
  .Column1.CONTROLSOURCE  = loFormSet.lcCtnHdr +'.Cart_No'
  .Column1.Header1.ALIGNMENT = 1
  .COLUMNS(1).ALIGNMENT = 1
  .COLUMNS(1).WIDTH = 40

  *-- Palette (If llEDISys)
  .Column2.CONTROLSOURCE  = loFormSet.lcCtnHdr + '.Pal_No'
  .Column2.VISIBLE = IIF(loFormSet.llEdiSys , .T., .F.)
  .Column2.Header1.ALIGNMENT = 1
  .COLUMNS(2).ALIGNMENT = 1
  .COLUMNS(2).WIDTH = 40

  *-- Tot. Pcs.
  .Column3.CONTROLSOURCE  = loFormSet.lcCtnHdr + '.TotPcs'
  .Column3.Header1.ALIGNMENT = 1
  .COLUMNS(3).ALIGNMENT = 1
  .COLUMNS(3).WIDTH = 70

  *-- Tot. Wgh.
  .Column4.CONTROLSOURCE  = loFormSet.lcCtnHdr + '.TotWgh'
  .Column4.Header1.ALIGNMENT = 1
  .COLUMNS(4).ALIGNMENT = 1
  .COLUMNS(4).WIDTH = 80

  *-- Carrier Carton ID
  .Column5.CONTROLSOURCE  = loFormSet.lcCtnHdr +'.cCarrCtnID'
  .SETALL('ReadOnly',.T.,'COLUMN')
ENDWITH
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfwCtnHdrBr
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : refresh the edit rigion for carton header according to
*!             lcCtnHdr file
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfwCtnHdrBr
LPARAMETERS loFormSet
LOCAL lnAlias
lnAlias = SELECT(0)

SELECT (loFormSet.lcCtnDtl)
SET KEY TO STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4)

*B608456,1 WAM 02/27/2008 Go to the first record for the selected carton
=SEEK(STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4))
*B608456,1 WAM 02/27/2008 (End)

*! B608342,1 MMT 11/01/2007 Add Scan option to the manual packing list[Start]
*LOCATE
*! B608342,1 MMT 11/01/2007 Add Scan option to the manual packing list[End]
loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonD.REFRESH
SELECT (loFormSet.lcCtnHdr)
WITH loFormSet.AriaForm1.pgfPacking.CartonInfo
  .txtCartonNo.REFRESH
  .txtTotWghH.REFRESH
  .txtPaletteNoH.REFRESH
  .txtCarrierCartId.REFRESH
  .cboCrtType.VALUE   = EVALUATE(loFormSet.lcCtnHdr + '.CCRTNVLTYP')
  IF loFormSet.ActiveMode = 'E' OR loFormSet.llNew
    .cmdNewCartH.ENABLED = .T.
    IF EOF(loFormSet.lcCtnHdr)
      STORE 0   TO .txtCartonNo.VALUE , .txtTotWghH.VALUE , .txtPaletteNoH.VALUE
      STORE ""  TO .txtCarrierCartId.VALUE , .cboCrtType.VALUE
      STORE .F. TO .txtCartonNo.ENABLED, .txtTotWghH.ENABLED, .txtPaletteNoH.ENABLED,;
        .txtCarrierCartId.ENABLED, .cmdNewCartD.ENABLED,.cmdRemoveCartH.ENABLED,.cboCrtType.ENABLED
      *C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [Begin]
      STORE .F. TO .cmdScan.ENABLED
      IF ASCAN(loFormSet.laEvntTrig,PADR('NEWSCAN',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
        =loFormSet.mDoTrigger(PADR('NEWSCAN',10))
      ENDIF
      *C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [End]
    ELSE
      .cmdRemoveCartH.ENABLED = .T.
      .txtCartonNo.ENABLED = IIF(!EMPTY(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No')),.F.,.T.)
      .txtTotWghH.ENABLED = .T.
      .txtPaletteNoH.ENABLED = IIF(loFormSet.llPalStat,.T.,.F.)
      .cboCrtType.ENABLED = .T.
      *! B610287,1 HIA 04/02/2013 Aria XP - Manual Packing List - Assign default, T20130226.0030 [Start]
      IF ALLTRIM(loFormSet.AriaForm1.pgfPacking.CartonInfo.cboCrtType.VALUE) == "" AND loFormSet.llNew
        loFormSet.AriaForm1.pgfPacking.CartonInfo.cboCrtType.VALUE = loFormSet.AriaForm1.pgfPacking.CartonInfo.cboCrtType.codedefaultvalue
      ENDIF 
      *! B610287,1 HIA 04/02/2013 Aria XP - Manual Packing List - Assign default, T20130226.0030 [End]      
      .txtCarrierCartId.ENABLED = loFormSet.AriaForm1.pgfpacking.HEADER.chkAssCarrID.VALUE
    ENDIF
    *: B610016,1 SAB 07/16/2012 Enable Editing Notes and Special Inst. for Completed Packs [Start]
    IF loformset.activemode='E' .AND. pack_hdr.STATUS='C'
      STORE .F. TO .cmdnewcarth.ENABLED, .cmdremovecarth.ENABLED, .txtcartonno.ENABLED, .txttotwghh.ENABLED, .txtpalettenoh.ENABLED, .cbocrttype.ENABLED, .txtcarriercartid.ENABLED
    ENDIF
    *: B610016,1 SAB 07/16/2012 Enable Editing Notes and Special Inst. for Completed Packs [End]
  ELSE
    STORE .F. TO .txtCartonNo.ENABLED, .txtTotWghH.ENABLED, .txtPaletteNoH.ENABLED,;
      .txtCarrierCartId.ENABLED, .cmdNewCartH.ENABLED,.cmdRemoveCartH.ENABLED,;
      .cmdNewCartD.ENABLED,.cboCrtType.ENABLED
    *C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [Begin]
    STORE .F. TO .cmdScan.ENABLED
    *C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [End]
  ENDIF
ENDWITH

=lfwCtnDtlBr(loFormSet)

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfCtnDtlBr
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : Browse for cartons information (Carton Detail)
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfCtnDtlBr
LPARAMETERS loFormSet
LOCAL lnCurAlias

lnCurAlias = SELECT(0)

WITH loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonD
  .RECORDSOURCE = ''
  SELECT (loFormSet.lcCtnDtl)
  LOCATE
  .RECORDSOURCE = loFormSet.lcCtnDtl
  *-- PAck_ID
  .Column6.CONTROLSOURCE  = loFormSet.lcCtnDtl + '.cPackId'
  .Column6.VISIBLE = IIF(loFormSet.llUsePack, .T., .F.)
  .COLUMNS(6).WIDTH = 160
  .Column1.COLUMNORDER = 1

  *-- Style
  .Column1.CONTROLSOURCE  = loFormSet.lcCtnDtl +'.Style'
  .Column1.Header1.CAPTION = loFormSet.lcStyTtl
  .COLUMNS(1).WIDTH = 120
  .Column1.COLUMNORDER = 2

  *-- Configuration/Dyelot
  .Column2.Header1.CAPTION = IIF(loFormSet.llUseConfg,LANG_ManulPL_CptConfig,LANG_ManulPL_CptDyelot)
  .Column2.CONTROLSOURCE   = loFormSet.lcCtnDtl + '.Dyelot'
  .Column2.VISIBLE 		   = IIF(loFormSet.llUseConfg OR loFormSet.llDyelot , .T., .F.)
  .COLUMNS(2).WIDTH = 80
  .Column2.COLUMNORDER = 3

  *-- Size Code.
  .Column3.CONTROLSOURCE  = loFormSet.lcCtnDtl + '.Size'
  .COLUMNS(3).WIDTH = 40
  .Column3.COLUMNORDER = 4

  *-- Qty
  .Column4.CONTROLSOURCE  = loFormSet.lcCtnDtl + '.Qty'
  .COLUMNS(4).WIDTH = 40
  .Column4.Header1.ALIGNMENT = 1
  .COLUMNS(4).ALIGNMENT = 1
  .Column4.COLUMNORDER = 5

  *-- Weight
  .Column5.CONTROLSOURCE  = loFormSet.lcCtnDtl +'.Weight'
  .COLUMNS(5).WIDTH = 90
  .Column5.Header1.ALIGNMENT = 1
  .COLUMNS(5).ALIGNMENT = 1
  .Column5.COLUMNORDER = 6

  *! E302698,1 MMT 05/25/2010 Display SKU# in Style Detail and Carton Detail grids[Start]
  .Column7.CONTROLSOURCE  = loFormSet.lcCtnDtl +'.SKUNUM'
  .Column7.COLUMNORDER = 7
  *! E302698,1 MMT 05/25/2010 Display SKU# in Style Detail and Carton Detail grids[End]

  .SETALL('ReadOnly',.T.,'COLUMN')
ENDWITH

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfwCtnDtlBr
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : To refresh the edit rigion for carton details according to
*!             lcCtnDtl file
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfwCtnDtlBr
LPARAMETERS loFormSet

LOCAL lnAlias

lnAlias = SELECT(0)

SELECT (loFormSet.lcCtnDtl)
WITH loFormSet.AriaForm1.pgfPacking.CartonInfo

  *! B609138,1 MMT 02/04/2010 Fix bug of repeated lines in pack_lin after editing PL[Start]
  .grdCartonD.READONLY = .T.
  *! B609138,1 MMT 02/04/2010 Fix bug of repeated lines in pack_lin after editing PL[End]

  .kbStyle.REFRESH
  .kbConfiguration.REFRESH
  .kbSize.REFRESH
  .txtCartQtyD.VALUE  = Qty
  .txtCartWghD.VALUE  = Weight
  *! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[Start]
  IF loFormSet.llUPCInst AND !EMPTY(cupc)
    *C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [Begin]
    *   loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.VALUE = cupc
    *   loFormSet.ariaForm1.pgfPacking.CartonInfo.kbStyle.Enabled = .F.
    *   loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.Enabled = .F.
    *B608874,1  TMI 05/27/2009 [Start] when not in scan mode show the UPC
    *loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.VALUE = ''
    loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.VALUE = cupc
    *B608874,1  TMI 05/27/2009 [End  ]
    loFormSet.ariaForm1.pgfPacking.CartonInfo.kbStyle.ENABLED = .T.
    *B608874,1  TMI 05/31/2009 08:17:05 AM [Start] disable txtUPC field if not in scan mode
    *loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.ENABLED = .T.
    *B608874,1  TMI 05/31/2009 08:17:08 AM [End  ]
    *C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [End]
  ELSE
    loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.VALUE = ""
    loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.ENABLED = .F.
    *C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [Begin]
    IF ASCAN(loFormSet.laEvntTrig,PADR('UPCFRMT',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
      =loFormSet.mDoTrigger(PADR('UPCFRMT',10))
    ENDIF
    *C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [End]
  ENDIF
  *! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[End]


  IF loFormSet.ActiveMode = 'E' OR loFormSet.llNew
    IF EVAL(loFormSet.lcCtnDtl+'.Cart_No') = 0 AND EMPTY(EVALUATE(loFormSet.lcCtnDtl+'.Style')+EVALUATE(loFormSet.lcCtnDtl+'.Size'))
      STORE .F. TO .cmdRemoveCartD.ENABLED,.kbStyle.ENABLED,.kbConfiguration.ENABLED,.kbSize.ENABLED,.txtCartQtyD.ENABLED,.txtCartWghD.ENABLED
    ELSE
      STORE .T. TO .cmdRemoveCartD.ENABLED
      IF EMPTY(EVALUATE(loFormSet.lcCtnDtl+'.Style')) OR EMPTY(EVALUATE(loFormSet.lcCtnDtl+'.Size'))
        STORE .T. TO .kbStyle.ENABLED
        STORE EMPTY(Pack_id) TO .txtCartQtyD.ENABLED,.txtCartWghD.ENABLED
        STORE .F. TO .kbConfiguration.ENABLED,.kbSize.ENABLED
      ELSE
        STORE .F. TO .kbStyle.ENABLED,.kbConfiguration.ENABLED,.kbSize.ENABLED
        *! B609138,1 MMT 02/04/2010 Fix bug of repeated lines in pack_lin after editing PL[Start]
        *STORE EMPTY(Pack_id) TO .txtCartQtyD.ENABLED,.txtCartWghD.ENABLED
        STORE .T. TO .txtCartQtyD.ENABLED,.txtCartWghD.ENABLED
        *! B609138,1 MMT 02/04/2010 Fix bug of repeated lines in pack_lin after editing PL[End]
      ENDIF
    ENDIF
  ELSE
    STORE .F. TO .cmdRemoveCartD.ENABLED,.kbStyle.ENABLED,.kbConfiguration.ENABLED,.kbSize.ENABLED,.txtCartQtyD.ENABLED,.txtCartWghD.ENABLED
  ENDIF
  *: B610016,1 SAB 07/16/2012 Enable Editing Notes and Special Inst. for Completed Packs [Start]
  IF loformset.activemode='E' .AND. pack_hdr.STATUS='C'
    STORE .F. TO .cmdremovecartd.ENABLED, .kbstyle.ENABLED, .kbconfiguration.ENABLED, .kbsize.ENABLED, .txtcartqtyd.ENABLED, .txtcartwghd.ENABLED
  ENDIF
  *: B610016,1 SAB 07/16/2012 Enable Editing Notes and Special Inst. for Completed Packs [End]

ENDWITH
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfGetSize
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : To get size code
*!*************************************************************
*! Parameters:  lcSizeCode : Size code
*!				loFormSet  : FormSet
*!*************************************************************
FUNCTION lfGetSize
PARAMETER lcPackSize,loFormSet
PRIVATE lcNombr

IF !EMPTY(lcPackSize)
  loFormSet.SCALE.SEEK('S'+LEFT(lcPackSize,1))
  lcNombr = RIGHT(lcPackSize,1)
  lcLocSize=EVALUATE('SCALE.SZ'+lcNombr)
ELSE
  lcLocSize ='*****'
ENDIF

RETURN lcLocSize


*!*************************************************************
*! Name      : lfColSum
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : To collect data for temp file in case of pack summery
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
FUNCTION lfColSum
LPARAMETERS loFormSet
PRIVATE lnCurAlias
lnCurAlias = SELECT(0)
SELECT (loFormSet.lcPckLin)
LOCATE
lcPack_Id  = ''
SCAN FOR !EMPTY(EVALUATE(loFormSet.lcPckLin+'.Pack_id'))
  IF lcPack_Id = LEFT(Pack_Id,16) + cpkcolor + cpcksize + cPkVersion
    SELECT(loFormSet.lcSumPck)
    REPLACE PTotWgh WITH PTotWgh + EVALUATE(loFormSet.lcPckLin+'.PTotWgh')
  ELSE
    SELECT(loFormSet.lcSumPck)
    APPEND BLANK
    REPLACE OTotQty      WITH EVALUATE(loFormSet.lcPckLin+'.nPackNo') - EVALUATE(loFormSet.lcPckLin+'.nPkPack'),;
      cOpen        WITH IIF(OTotQty > 0 ,'Y','N'),;
      nOrgQty      WITH EVALUATE(loFormSet.lcPckLin+'.nPackNo') - EVALUATE(loFormSet.lcPckLin+'.nPkPack'),;
      PTotQty      WITH IIF(EVALUATE(loFormSet.lcPckLin+'.nPkPack') = 0,0,EVALUATE(loFormSet.lcPckLin+'.nPkPack')),;
      PTotWgh      WITH EVALUATE(loFormSet.lcPckLin+'.PTotWgh'),;
      StyWgh       WITH EVALUATE(loFormSet.lcPckLin+'.StyWgh'),;
      PACK_ID      WITH EVALUATE(loFormSet.lcPckLin+'.PACK_ID'),;
      cPkVersion   WITH EVALUATE(loFormSet.lcPckLin+'.cPkVersion'),;
      cPkColor     WITH EVALUATE(loFormSet.lcPckLin+'.cPkColor'),;
      cPckSize     WITH EVALUATE(loFormSet.lcPckLin+'.cPckSize'),;
      llPack       WITH .T.,;
      lRange       WITH EVALUATE(loFormSet.lcPckLin+'.lRange'),;
      cPack_ID     WITH ALLTRIM(EVALUATE(loFormSet.lcPckLin+'.PACK_ID'))  + '-' + ALLTRIM(EVALUATE(loFormSet.lcPckLin+'.cPkColor')) + '-' +;
      ALLTRIM(lfGetSize(EVALUATE(loFormSet.lcPckLin+'.cPckSize'),loFormSet)) + '-' +ALLTRIM(EVALUATE(loFormSet.lcPckLin+'.cPkVersion'))

    REPLACE Dyelot WITH EVALUATE(loFormSet.lcPckLin+'.Dyelot')

    lcPack_Id = LEFT(Pack_Id,16) + cpkcolor + cpcksize + cPkVersion
  ENDIF
ENDSCAN

SELECT (loFormSet.lcPckLin)
LOCATE
SCAN FOR EMPTY(EVALUATE(loFormSet.lcPckLin+'.Pack_id'))
  SELECT(loFormSet.lcSumPck)
  APPEND BLANK
  REPLACE PACK_ID    WITH EVALUATE(loFormSet.lcPckLin+'.Style'),;
    OTotQty    WITH EVALUATE(loFormSet.lcPckLin+'.OQty'),;
    cOpen      WITH IIF(OTotQty > 0 ,'Y','N'),;
    nOrgQty    WITH EVALUATE(loFormSet.lcPckLin+'.OQty'),;
    CtnTotQty  WITH EVALUATE(loFormSet.lcPckLin+'.CtnQty'),;
    PTotQty    WITH EVALUATE(loFormSet.lcPckLin+'.PQty'),;
    PTotWgh    WITH EVALUATE(loFormSet.lcPckLin+'.PWgh'),;
    StyWgh     WITH EVALUATE(loFormSet.lcPckLin+'.StyWgh'),;
    cSze       WITH EVALUATE(loFormSet.lcPckLin+'.cSizeNo'),;
    cSize      WITH EVALUATE(loFormSet.lcPckLin+'.cSizeCod'),;
    nOrdLineNO WITH EVALUATE(loFormSet.lcPckLin+'.nOrdLineNO'),;
    Dyelot     WITH EVALUATE(loFormSet.lcPckLin+'.Dyelot'),;
    cPack_ID   WITH EVALUATE(loFormSet.lcPckLin+'.Style')

ENDSCAN

llApply = .F.
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfCrTmpFiles
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
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
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
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

*-- Total order Qty
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrdTotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'AvlQty'
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
laFileStru[lnI,1] = 'OQty'
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

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'CtnQty'
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

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

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
laFileStru[lnI,1] = 'PWgh'
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
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lSelect'
laFileStru[lnI,2] = 'L'
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
laFileStru[lnI,1] = 'OrgTotOrd'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nDiff'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

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
laFileStru[lnI,1] = 'nPackNo'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nPkPack'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lRange'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Dyelot'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSizeCod'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cOpen'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSizeNo'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPackId'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 35
laFileStru[lnI,4] = 0

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'cCrtnVlTyp'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 6
laFileStru[lnFileStru+1,4] = 0

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'llFilter'
laFileStru[lnFileStru+1,2] = 'L'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0


lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'Piktkt'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 6
laFileStru[lnFileStru+1,4] = 0

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'PrePak'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'PpQty'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 4
laFileStru[lnFileStru+1,4] = 0

*! E302698,1 MMT 05/25/2010 Display SKU# in Style Detail and Carton Detail grids[Start]
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'SKUNUM'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 16
laFileStru[lnFileStru+1,4] = 0
*! E302698,1 MMT 05/25/2010 Display SKU# in Style Detail and Carton Detail grids[End]


DIMENSION laIndx[8,2]
laIndx[1,1] = "Style+Dyelot+STR(nOrdLineNo,6)+cSizeNo"
laIndx[1,2] = loFormSet.lcPckLin
laIndx[2,1] = "IIF(Selected,'Y','N')"
laIndx[2,2] = 'Selected'
laIndx[3,1] = "IIF(OQty>0,'Y','N')"
laIndx[3,2] = 'Opened'
laIndx[4,1] = "IIF(PQty=0,'Y','N')"
laIndx[4,2] = 'NoPacked'
laIndx[5,1] = "STR(nOrdLineNo,6)+Style+Dyelot"
laIndx[5,2] = loFormSet.lcTmpIdx
laIndx[6,1] = "PACK_ID+cPkColor+cPckSize+cPkVersion+STR(nOrdLineNo,6)+Style+Dyelot+cSizeNo"
laIndx[6,2] = loFormSet.lcPakIndxLn
laIndx[7,1] = "PACK_ID+cPkColor+cPckSize+cPkVersion+Style+Dyelot+STR(nOrdLineNo,6)+cSizeNo"
laIndx[7,2] = loFormSet.lcPakIndxSt
laIndx[8,1] = "Style+Dyelot+STR(nOrdLineNo,6)+cSizeCod"
laIndx[8,2] = 'StySze'


=gfCrtTmp(loFormSet.lcPckLin,@laFileStru,@laIndx)
SET ORDER TO loFormSet.lcPakIndxLn IN (loFormSet.lcPckLin)


DIMENSION laIndx[4,2]
laIndx[1,1] = "Style+Dyelot+STR(nOrdLineNo,6)+cSizeNo"
laIndx[1,2] = loFormSet.lcPackLines
laIndx[2,1] = "Style"
laIndx[2,2] = 'Style'
laIndx[3,1] = "Dyelot"
laIndx[3,2] = 'Config'
laIndx[4,1] = "cSizeCod"
laIndx[4,2] = 'Size'
=gfCrtTmp(loFormSet.lcPackLines,@laFileStru,@laIndx)

DIMENSION laFileStru[19, 4]
laFileStru[1,1] = 'Store'
laFileStru[1,2] = 'C'
laFileStru[1,3] = 8
laFileStru[1,4] = 0
laFileStru[2,1] = 'StName'
laFileStru[2,2] = 'C'
laFileStru[2,3] = 30
laFileStru[2,4] = 0
laFileStru[3,1] = 'cAddress1'
laFileStru[3,2] = 'C'
laFileStru[3,3] = 30
laFileStru[3,4] = 0
laFileStru[4,1] = 'cAddress2'
laFileStru[4,2] = 'C'
laFileStru[4,3] = 30
laFileStru[4,4] = 0
laFileStru[5,1] = 'cAddress3'
laFileStru[5,2] = 'C'
laFileStru[5,3] = 30
laFileStru[5,4] = 0
laFileStru[6,1] = 'cAddress4'
laFileStru[6,2] = 'C'
laFileStru[6,3] = 30
laFileStru[6,4] = 0
laFileStru[7,1] = 'cAddress5'
laFileStru[7,2] = 'C'
laFileStru[7,3] = 30
laFileStru[7,4] = 0
laFileStru[8,1] = 'cAddress6'
laFileStru[8,2] = 'C'
laFileStru[8,3] = 30
laFileStru[8,4] = 0
laFileStru[9,1] = 'BtName'
laFileStru[9,2] = 'C'
laFileStru[9,3] = 30
laFileStru[9,4] = 0
laFileStru[10,1] = 'cAddress12'
laFileStru[10,2] = 'C'
laFileStru[10,3] = 30
laFileStru[10,4] = 0
laFileStru[11,1] = 'cAddress22'
laFileStru[11,2] = 'C'
laFileStru[11,3] = 30
laFileStru[11,4] = 0
laFileStru[12,1] = 'cAddress32'
laFileStru[12,2] = 'C'
laFileStru[12,3] = 30
laFileStru[12,4] = 0
laFileStru[13,1] = 'cAddress42'
laFileStru[13,2] = 'C'
laFileStru[13,3] = 30
laFileStru[13,4] = 0
laFileStru[14,1] = 'cAddress52'
laFileStru[14,2] = 'C'
laFileStru[14,3] = 30
laFileStru[14,4] = 0
laFileStru[15,1] = 'cAddress62'
laFileStru[15,2] = 'C'
laFileStru[15,3] = 30
laFileStru[15,4] = 0
laFileStru[16,1] = 'Phone1'
laFileStru[16,2] = 'C'
laFileStru[16,3] = 16
laFileStru[16,4] = 0
laFileStru[17,1] = 'Buyer'
laFileStru[17,2] = 'C'
laFileStru[17,3] = 30
laFileStru[17,4] = 0
laFileStru[18,1] = 'SalesRep'
laFileStru[18,2] = 'C'
laFileStru[18,3] = 3
laFileStru[18,4] = 0
laFileStru[19,1] = 'Type'
laFileStru[19,2] = 'C'
laFileStru[19,3] = 1
laFileStru[19,4] = 0
DIMENSION laIndeces[1,2]
*E037427,1 WAM 05/05/2005 Fix file index
*laIndeces[1,1] = 'Type+Store'
laIndeces[1,1] = 'Store'
*E037427,1 WAM 05/05/2005 (End)
laIndeces[1,2] = 'STORE'
=gfCrtTmp(loFormSet.lcTmpStores, @laFileStru, @laIndeces)


*E037427,1 WAM 05/05/2005 Reset filter
IF USED(loFormSet.lcTmpScpPck)
  ZAP IN (loFormSet.lcTmpScpPck)
ENDIF
IF USED(loFormSet.lcScpStyFlt)
  ZAP IN (loFormSet.lcScpStyFlt)
ENDIF
IF USED(loFormSet.lcScpPckFlt)
  ZAP IN (loFormSet.lcScpPckFlt)
ENDIF
*E037427,1 WAM 05/05/2005 (End)

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfCrCtnFiles
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : Create the files that are used in Function lfvSelOrd
*!             (lcOrdLin,lcPckLin)
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
FUNCTION lfCrCtnFiles
LPARAMETERS loFormSet
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)

DIMENSION laFileStru[7,4]

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

laFileStru[05,1] = 'CCARRCTNID'
laFileStru[05,2] = 'C'
laFileStru[05,3] = 25
laFileStru[05,4] = 0

laFileStru[06,1] = 'cCrtnVlTyp'
laFileStru[06,2] = 'C'
laFileStru[06,3] = 6
laFileStru[06,4] = 0

laFileStru[07,1] = 'Empty'
laFileStru[07,2] = 'C'
laFileStru[07,3] = 1
laFileStru[07,4] = 0

DIMENSION laIndx[2,2]
laIndx[1,1] = "STR(Cart_No,4)+STR(Pal_No,4)"
laIndx[1,2] = loFormSet.lcCtnHdr
laIndx[2,1] = "Empty+STR(Cart_No,4)+STR(Pal_No,4)"
laIndx[2,2] = "EMPTY"
=gfCrtTmp(loFormSet.lcCtnHdr,@laFileStru,@laIndx)
SET ORDER TO loFormSet.lcCtnHdr IN (loFormSet.lcCtnHdr)

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
laFileStru[lnI,1] = 'Size'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Qty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'TotQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Weight'
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
laFileStru[lnI,1] = 'Br'
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
laFileStru[lnI,1] = 'cPackId'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 35
laFileStru[lnI,4] = 0


lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nPackNO'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Dyelot'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSizeNo'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'PrePak'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'PpQty'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 4
laFileStru[lnFileStru+1,4] = 0

*! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[Start]
*DIMENSION laIndx[3,2]

DIMENSION laIndx[4,2]
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'CUPC'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 13
laFileStru[lnFileStru+1,4] = 0
*! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[End]

*! E302698,1 MMT 05/25/2010 Display SKU# in Style Detail and Carton Detail grids[Start]
DIMENSION laFileStru[lnFileStru+2,4]
laFileStru[lnFileStru+2,1] = 'SKUNUM'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = 16
laFileStru[lnFileStru+2,4] = 0
*! E302698,1 MMT 05/25/2010 Display SKU# in Style Detail and Carton Detail grids[End]

laIndx[1,1] = "STR(Cart_No,4)+Style+Dyelot+STR(nOrdLineNo,6)+cSizeNo"
laIndx[1,2] = loFormSet.lcCtnDtl
laIndx[2,1] = "cStatus"
laIndx[2,2] = "Status"
laIndx[3,1] = "STR(Cart_No,4)+Style+Dyelot+STR(nOrdLineNo,6)+Size"
laIndx[3,2] = 'StySze'

*! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[Start]
laIndx[4,1] = "STR(Cart_No,4)+CUPC+STR(nOrdLineNo,6)+Size"
laIndx[4,2] = 'StyUPC'
*! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[End]


=gfCrtTmp(loFormSet.lcCtnDtl,@laFileStru,@laIndx)
SET ORDER TO loFormSet.lcCtnDtl IN (loFormSet.lcCtnDtl)

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfCrSelSum
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : Create the files that are used in Function lfvSelOrd
*!                  to get pack summery (lcSumPck)
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
FUNCTION lfCrSelSum
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
laFileStru[lnI,1] = 'lSelected'
laFileStru[lnI,2] = 'L'
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

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nOrdLineNo'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSze'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 3
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cSize'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nOrgQty'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 7
laFileStru[lnI,4] = 2

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'Dyelot'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 10
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cPack_ID'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 30
laFileStru[lnI,4] = 0

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'llFilter'
laFileStru[lnFileStru+1,2] = 'L'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'cOpen'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0


lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'StyWgh'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 2

DIMENSION laIndx[3,2]
laIndx[1,1] = "PACK_ID+cPkColor+cPckSize+cPkVersion"
laIndx[1,2] = loFormSet.lcSumPck
laIndx[2,1] = "PACK_ID+Dyelot+STR(nOrdLineNo,6)+cSze"
laIndx[2,2] = "lcSumPkLn"
laIndx[3,1] = "IIF(lSelected,'Y','N')"
laIndx[3,2] = 'Selected'

=gfCrtTmp(loFormSet.lcSumPck,@laFileStru,@laIndx)
SET ORDER TO loFormSet.lcSumPck IN (loFormSet.lcSumPck)

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfChkOrdLok
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
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
lcStore    = loFormSet.AriaForm1.kbStore.keytextbox.VALUE
lnCurAlias = SELECT(0)

lcOldHOrd = ORDER('ORDHDR')
loFormSet.ORDHDR.SetOrder('ORDHDR')
IF loFormSet.Ordhdr.SEEK('O'+lcOrderNo)
  *llGoOn = lfObj_Lock(.T.,loFormSet.Ordhdr,loformset,'Ordhdr')
  *: B608244,1 MMT 08/27/2007 fix of Locking ordhdr in case of more than one piktkt[Start]
  *!*	  SELECT ordhdr
  *!*	  llGoOn = gfObj_Lock(.T.)
  lcPkTk = loFormSet.ariaForm1.kbPkTktNo.keytextbox.VALUE
  IF loFormSet.PikTkt.SEEK(lcOrderNo+lcPkTk,'ORDPIK')
    SELECT PIKTKT
    llGoOn = gfObj_Lock(.T.)
  ENDIF
  *: B608244,1 MMT 08/27/2007 fix of Locking ordhdr in case of more than one piktkt[End]
ENDIF
loFormSet.ORDHDR.SetOrder(lcOldHOrd)

SELECT (lnCurAlias)

RETURN llGoOn

*!*************************************************************
*! Name      : lfUnLock
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : Unlock the selected order lines
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
FUNCTION lfUnLock
LPARAMETERS loFormSet

PRIVATE lnCurAlias, lcOldHOrd
lnCurAlias = SELECT(0)
IF !EMPTY(loFormSet.lcOrdNo)
  lcOldHOrd = ORDER('ORDHDR')
  loFormSet.ORDHDR.SetOrder('ORDHDR')
  IF loFormSet.ORDHDR.SEEK('O'+loFormSet.lcOrdNo)
    *=lfObj_Lock(.F.,loFormSet.ORDHDR,loformset,'ORDHDR')
    *: B608244,1 MMT 08/27/2007 fix of Locking ordhdr in case of more than one piktkt[Start]
    *!*	    SELECT ordhdr
    *!*	    =gfObj_Lock(.F.)
    lcPkTk = loFormSet.ariaForm1.kbPkTktNo.keytextbox.VALUE
    IF loFormSet.PikTkt.SEEK(loFormSet.lcOrdNo+lcPkTk,'ORDPIK')
      SELECT PIKTKT
      =gfObj_Lock(.F.)
    ENDIF
    *: B608244,1 MMT 08/27/2007 fix of Locking ordhdr in case of more than one piktkt[End]
  ENDIF
  loFormSet.ORDHDR.SetOrder(lcOldHOrd)
  *B608254,1 WAM 09/05/2007 Fix the bug of adding empty records in PACK_HDR file
  SELECT PACK_HDR
  =TABLEREVERT(.T.)
  *B608254,1 WAM 09/05/2007 (End)

ENDIF
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvShipVia
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : Validate the ship via
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
FUNCTION lfvShipVia
LPARAMETERS loFormSet

WITH loFormSet.ariaForm1.pgfPacking.HEADER
  IF .cboShipViaH.VALUE = .cboShipViaH.OldValue
    RETURN
  ENDIF
ENDWITH
=lfNewBOL(loFormSet)

*!*************************************************************
*! Name      : lfvDirectTo
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : Validate the direct to popup
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
FUNCTION lfvDirectTo
LPARAMETERS loFormSet
LOCAL lcBOL,lnPrvAlis
lcBOL = loFormSet.ariaForm1.pgfPacking.HEADER.txtBOL.VALUE

*B608622,1 WAM 07/14/2008 Fix bug while edit Ship to field
lnPrvAlis = SELECT(0)
*B608622,1 WAM 07/14/2008 (End)

*--Update field of To Store or consalidatore in BOL_HDR
IF loFormSet.llEdiSys AND loFormSet.BOL_HDR.SEEK(lcBOL)
  *B608622,1 WAM 07/14/2008 Fix bug while edit Ship to field
  *lnPrvAlis = SELECT(0)
  *B608622,1 WAM 07/14/2008 (End)
  SELECT BOL_HDR
  REPLACE BOL_HDR.cToStorCn WITH IIF(loFormSet.lnDrctTo = 2,"C","S")
ENDIF
SELECT (lnPrvAlis)


*!*************************************************************
*! Name      : lfNewBOL
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : Check for the BOL in add mode and if not found
*!             generat a rundoum one
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
FUNCTION lfNewBOL
LPARAMETERS loFormSet

IF !loFormSet.llEdiSys
  RETURN
ENDIF

PRIVATE laFields,lcAccount,lcStore,lcShipVia
*-- array to hold defulted fields for BOL
DIMENSION laFields[3,2]
laFields[1,1] = "cgronhang"
laFields[1,2] = "N"
laFields[2,1] = "ctranmthd"
laFields[2,2] = "M"
laFields[3,1] = "packtype"
laFields[3,2] = "CTN25"
lcAccount = loFormSet.AriaForm1.kbAccount.keytextbox.VALUE
lcStore   = loFormSet.AriaForm1.kbStore.keytextbox.VALUE
lcShipVia = loFormSet.ariaForm1.pgfPacking.HEADER.cboShipViaH.VALUE
*! B609167,1 MMT 03/11/2010 the orders are not changing the ship to address's should either be stores or to DC[Start]
*!*	  loFormSet.ariaForm1.pgfPacking.HEADER.txtBOL.VALUE = ;
*!*	    loFormSet.AriaForm1.alclass.lfGetBOL("",lcAccount,lcStore,loFormSet.lcWareCode,;
*!*	    lcShipVia,IIF(loFormSet.lnCtnTyp=1,'Y','N'),"laFields",.F.,.F.,;
*!*	    IIF(loFormSet.lnDrctTo=2,"C","S"))
loFormSet.ariaForm1.pgfPacking.HEADER.txtBOL.VALUE = ;
  loFormSet.AriaForm1.alclass.lfGetBOL("",lcAccount,lcStore,loFormSet.lcWareCode,;
  lcShipVia,IIF(loFormSet.lnCtnTyp=1,'Y','N'),"laFields",.F.,.F.,;
  IIF(loFormSet.lnDrctTo=2,"C","S"),ORDHDR.LSTRDIRCT)
*! B609167,1 MMT 03/11/2010 the orders are not changing the ship to address's should either be stores or to DC[End]
*-- End Of Function lfNewBOL

*!*************************************************************
*! Name      : lfvBol
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : Validate the BOL #
*!             carton detail staus
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!			   lcBolNo   : Bill of Lading #
*!			   lcOldVal  : Old value
*!*************************************************************
FUNCTION lfvBol
LPARAMETERS loFormSet, lcBolNo, lcOldVal
PRIVATE lcAccount,lcStore,lcShipVia
WITH loFormSet.AriaForm1
  lcAccount = .kbAccount.keytextbox.VALUE
  lcStore   = .kbStore.keytextbox.VALUE
  lcShipVia = .pgfPacking.HEADER.cboShipViaH.VALUE
ENDWITH
*B608364,1 WAM 11/28/2007 Check BOL assigned to packing list when the shipvia changed
*IF !MDOWN() AND !EMPTY(lcBolNo) AND (PADR(lcBolNo,6) <> PADR(lcOldVal,6))
IF !MDOWN() AND !EMPTY(lcBolNo) AND (PADR(lcBolNo,6) <> PADR(lcOldVal,6)) AND INLIST(loFormSet.ActiveMode,'A','E')
  *B608364,1 WAM 11/28/2007 (End)

  IF loFormSet.llEdiSys AND !loFormSet.llBolnever
    *B609167,1 MMT 03/11/2010 the orders are not changing the ship to address's should either be stores or to DC[Start]
    *!*        lcBolNo = loFormSet.AriaForm1.alclass.lfGetBOL("",lcAccount,lcStore,loFormSet.lcWareCode,;
    *!*          lcShipVia,IIF(loFormSet.lnCtnTyp=1,'Y','N'),"laFields",.F.,.F.,;
    *!*          IIF(loFormSet.lnDrctTo=2,"C","S"))
    lcBolNo = loFormSet.AriaForm1.alclass.lfGetBOL("",lcAccount,lcStore,loFormSet.lcWareCode,;
      lcShipVia,IIF(loFormSet.lnCtnTyp=1,'Y','N'),"laFields",.F.,.F.,;
      IIF(loFormSet.lnDrctTo=2,"C","S"),ORDHDR.LSTRDIRCT)
    *B609167,1 MMT 03/11/2010 the orders are not changing the ship to address's should either be stores or to DC[End]
  ENDIF
  lcBolNo = IIF(EMPTY(lcBolNo),lcOldVal,lcBolNo)
  loFormSet.AriaForm1.pgfPacking.HEADER.txtBOL.VALUE = lcBolNo
ENDIF

*!*************************************************************
*! Name      : lfvSel
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : Valid function for select button
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!			   lcOption  : To indicate the selected button
*!*************************************************************
FUNCTION lfvSel
LPARAMETERS loFormSet, lcOption
PRIVATE lnCurRec,lcUnSelSty,lcSelFld,lcCtnQtyFld,lnSizeRec
LOCAL lcPkTktNo, lcExpr,lcExpr2, lnCrtPck, lnPackNo, lnTmpRec, lnCurRec

lnCurAlias = SELECT(0)
lcPkTktNo = loFormSet.AriaForm1.kbPkTktNo.keytextbox.VALUE
SELECT (loFormSet.lcPckLin)
lnTmpRec = RECNO(loFormSet.lcPckLin)
lnCurRec = RECNO(loFormSet.lcSumPck)
IF loFormSet.llShoPckSu
  SELECT (loFormSet.lcPckLin)
ELSE
  SELECT (loFormSet.lcSumPck)
ENDIF
SET RELATION TO
DO CASE
  *-- Select Button
CASE lcOption = 'S'
  IF loFormSet.llShoPckSu
    *Select the whole Pack if trying to select style belongs to a pack
    IF !EMPTY(Pack_Id + cpkcolor + cpcksize + cPkVersion)
      =lfSelPckLn(loFormSet,Pack_Id+cpkcolor+cpcksize+cPkVersion,.T., .T.,;
        Pack_Id +'   '+ cpkColor + cPckSize + cPkVersion,lcOption)
    ELSE
      lcExpr  = STYLE+Dyelot+STR(nOrdLineNo,6)+cSizeNo
      =lfSelPckLn(loFormSet,lcExpr,.F., .T.,lcExpr,lcOption)
    ENDIF
  ELSE
    SELECT (loFormSet.lcSumPck)
    lcExpr   = IIF(llPack,Pack_ID+cPkColor+cPckSize+cPkVersion,;
      Pack_ID+Dyelot+STR(nOrdLineNo,6)+cSze)
    lcExpr2  = IIF(llPAck,LEFT(Pack_ID,16)+cPkColor+cPckSize+cPkVersion, Pack_ID)
    =lfSelPckLn(loFormSet,lcExpr,llPack, .F.,.F.,lcOption)
    SELECT(loFormSet.lcPckLin)
  ENDIF

  *-- Select None
CASE lcOption = 'N'
  lcCurrAls = ALIAS()
  SELECT(loFormSet.lcPckLin)
  REPLACE ALL lSelect   WITH .F.,;
    SELECTED  WITH .F.,;
    CtnQty    WITH 0 ,;
    CtnTotQty WITH 0,;
    StyWgh    WITH OrgStyWgh

  SELECT(loFormSet.lcSumPck)
  *E037427,1 WAM 05/05/2005 Do not reset packed quantity
  *REPLACE ALL lSelect   WITH .F.,;
  lSelected WITH .F.,;
  CtnTotQty WITH 0,;
  PTotQty   WITH 0
  REPLACE ALL lSelect   WITH .F.,;
    lSelected WITH .F.,;
    CtnTotQty WITH 0
  *E037427,1 WAM 05/05/2005 (End)

  SELECT(lcCurrAls)

  *-- Invert or Select All
CASE lcOption $ 'IA'
  SELECT (loFormSet.lcSumPck)
  USE (oAriaApplication.WorkDir+loFormSet.lcSumPck) IN 0 AGAIN ;
    ALIAS (loFormSet.lcTempCur)
  SELECT (loFormSet.lcTempCur)
  LOCATE
  SCAN
    lcExpr   = IIF(llPack,Pack_ID+cPkColor+cPckSize+cPkVersion,;
      Pack_ID+Dyelot+STR(nOrdLineNo,6)+cSze)
    lcExpr2  = IIF(llPAck,LEFT(Pack_ID,16)+cPkColor+cPckSize+cPkVersion, Pack_ID)
    =lfSelPckLn(loFormSet,lcExpr,llPack, .F.,.F.,lcOption)
  ENDSCAN
  USE IN (loFormSet.lcTempCur)
  ERASE oAriaApplication.WorkDir+loFormSet.lcTempCur
  IF loFormSet.llShoPckSu
    SELECT (loFormSet.lcPckLin)
  ELSE
    SELECT (loFormSet.lcSumPck)
  ENDIF
  LOCATE

  *-- Style
CASE lcOption = 'T'
  IF !loFormSet.llShoPckSu
    SELECT (loFormSet.lcSumPck)
    lcExpr   = IIF(llPack,Pack_ID+cPkColor+cPckSize+cPkVersion,;
      Pack_ID)
    lcExpr2  = IIF(llPAck,LEFT(Pack_ID,16)+cPkColor+cPckSize+cPkVersion, Pack_ID+Dyelot)
    =lfSelPckLn(loFormSet,lcExpr,llPack, .F.,lcExpr2,lcOption)
  ELSE
    IF !EMPTY(Pack_Id + cpkcolor + cpcksize + cPkVersion)
      =lfSelPckLn(loFormSet,Pack_Id+cpkcolor+cpcksize+cPkVersion,.T., .T.,;
        Pack_Id +'   '+ cpkColor + cPckSize + cPkVersion,lcOption)
    ELSE
      SELECT (loFormSet.lcPckLin)
      lcExpr  = STYLE+Dyelot
      =lfSelPckLn(loFormSet,lcExpr,.F., .T.,lcExpr,lcOption)
    ENDIF
  ENDIF

  *-- Pack
CASE lcOption = 'P'
  =lfSelPckLn(loFormSet,Pack_Id+cpkcolor+cpcksize+cPkVersion,.T., .T.,;
    Pack_Id +'   '+ cpkColor + cPckSize + cPkVersion,lcOption)
ENDCASE

SELECT (loFormSet.lcPckLin)
=RLOCK(loFormSet.lcPckLin)
UNLOCK IN (loFormSet.lcPckLin)

IF BETWEEN(lnTmpRec,1,RECCOUNT())
  GOTO lnTmpRec
ENDIF

SELECT (loFormSet.lcSumPck)
IF BETWEEN(lnCurRec,1,RECCOUNT())
  GOTO lnCurRec
ENDIF

SELECT(lnCurAlias)

=lfwDtlBrow(loFormSet)

*!*********************************************************************
*! Name      : lfSelPckLn
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : To select/unselect line pack in detail/summary table
*!*********************************************************************
*! Parameters: loFormSet : ThisFormSet
*!		       lcSeekExpr: Seek expression
*!			   llHasPack : To update qty/crt and stywgh if not a pack
*!             llDetail  : To use either lcSumPck or lcPckLin
*!			   lcDtExpr  : To select/unselect from lcSumPck in case of
*!						   detail.
*!			   lcOption  : To select/unselect based on the pressed button
*!*********************************************************************
FUNCTION lfSelPckLn
LPARAMETERS loFormSet, lcSeekExpr, llHasPack, llDetail, lcDtExpr, lcOption
LOCAL lcCurAlis, lcOldOrd, lcWhileExpr, lcKey, lcPkLWhExpr, lcOldOrd1, lcOldOrd2,;
  lcPkTktNo
lcPkTktNo = loFormSet.AriaForm1.kbPkTktNo.keytextbox.VALUE
lcCurAlis = ALIAS()

lcOldOrd1 = ORDER(loFormSet.lcSumPck)
lcOldOrd2 = ORDER(loFormSet.lcPckLin)

IF llDetail
  SELECT (loFormSet.lcPckLin)
  LOCAL lnDtRecNo
  lnDtRecNo = RECNO()
  IF llHasPack
    SET ORDER TO (loFormSet.lcSumPck) IN (loFormSet.lcSumPck)
    lcWhileExpr = "REST FOR PACK_ID+cPkColor+cPckSize+cPkVersion "
    SET ORDER TO (loFormSet.lcPakIndxSt) IN (loFormSet.lcPckLin)
    lcPkLWhExpr = "REST FOR PACK_ID+cPkColor+cPckSize+cPkVersion+Style+Dyelot+STR(nOrdLineNo,6) "

  ELSE
    SET ORDER TO lcSumPkLn IN (loFormSet.lcSumPck)
    lcWhileExpr = "REST FOR PACK_ID+Dyelot+STR(nOrdLineNo,6)+cSze "
    SET ORDER TO (loFormSet.lcPckLin) IN (loFormSet.lcPckLin)
    lcPkLWhExpr = "REST FOR Style+Dyelot+STR(nOrdLineNo,6)+cSizeNo "
  ENDIF
  IF lcOption $ 'T'
    llSelect = lSelect
  ENDIF

  IF SEEK(lcSeekExpr)
    SCAN &lcPkLWhExpr = lcSeekExpr
      REPLACE lSelect   WITH IIF(lcOption $ 'T',!llSelect,IIF(IIF(loFormSet.llShwOpn,AvlQty > 0 AND OQty>0,AvlQty > 0),;
        IIF(lSelect,IIF(lcOption $ 'A',lSelect,.F.),.T.),lSelect)),;
        SELECTED  WITH lSelect,;
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
        =SEEK(PADR(Pack_ID,19)+Dyelot+STR(nOrdLineNo,6)+ALLTRIM(cSze),loFormSet.lcPckLin)
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
    lcPkLWhExpr = "REST FOR PACK_ID+cPkColor+cPckSize+cPkVersion+Style+Dyelot+STR(nOrdLineNo,6)"
  ELSE
    SET ORDER TO lcSumPkLn
    lcWhileExpr = "REST FOR PACK_ID+Dyelot+STR(nOrdLineNo,6)+cSze "
    SET ORDER TO (loFormSet.lcPckLin) IN (loFormSet.lcPckLin)
    lcPkLWhExpr = "REST FOR Style+Dyelot+STR(nOrdLineNo,6)+cSizeNo "
  ENDIF
  IF SEEK(lcSeekExpr)
    SCAN &lcWhileExpr = lcSeekExpr
      lcKey = IIF(llPAck,LEFT(Pack_ID,16)+cPkColor+cPckSize+cPkVersion,;
        PADR(Pack_ID,19)+Dyelot+STR(nOrdLineNo,6)+ALLTRIM(cSze))
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
            SELECTED  WITH lSelect
          loFormSet.lnBrUntWgh = IIF(!lSelect,OrgStyWgh,StyWgh)
          REPLACE StyWgh    WITH loFormSet.lnBrUntWgh
          IF OQty <> 0
            IF !EVALUATE(loFormSet.lcSumPck+'.llPAck')
              REPLACE CtnQty    WITH IIF(lSelect,IIF((loFormSet.lnTo-loFormSet.lnFrom+1)>0,;
                INT(OQty/(loFormSet.lnTo-loFormSet.lnFrom+1)),0),0),;
                CtnTotQty WITH CtnQty
            ELSE

              *E037427,1 WAM 05/05/2005 Correct update of packed quantity
              *REPLACE CtnQty    WITH IIF(EVALUATE(loFormSet.lcPckLin+'.OQty') <> 0,;
              (EVALUATE(loFormSet.lcPckLin+'.OQty')/lnOrdPck)*lnNumPack,0),;
              CtnTotQty WITH CtnQty
              *lnPack   = OTotQty   / lnOrdPck
              *lnPkPack = CtnTotQty / lnPack
              *REPLACE nPkPack WITH lnPkPack

              REPLACE CtnQty    WITH IIF( (loFormSet.lnTo-loFormSet.lnFrom+1)> 0,;
                INT(OQty/(loFormSet.lnTo-loFormSet.lnFrom+1)),0) ,;
                CtnTotQty WITH CtnQty
              *E037427,1 WAM 05/05/2005 (End)

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
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
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
*E302315 WLD Print Visual lables from Packing List 10/10/2006 [Begin]
lcBOL = loFormSet.ariaForm1.pgfPacking.HEADER.txtBOL.VALUE
*E302315 WLD Print Visual lables from Packing List 10/10/2006 [End]

SELECT (loFormSet.lcPckLin)
SET RELATION TO
IF loFormSet.lnFrom <= 0 OR loFormSet.lnTo <= 0 OR loFormSet.lnFrom > loFormSet.lnTo
  *-- You have to enter correct carton range.
  *-- <OK>
  = gfModalGen("INM44033B00000","Dialog")
  loFormSet.ariaForm1.pgfPacking.DETAIL.txtCartFrom.SETFOCUS
ELSE
  IF loFormSet.llPalStat
    SELECT COUNT(Cart_No) FROM (loFormSet.lcCtnHdr) INTO ARRAY laCart;
      WHERE Pal_No <> loFormSet.lnCtnPal;
      AND BETWEEN (Cart_No,loFormSet.lnFrom,loFormSet.lnTo)
  ENDIF
  IF laCart[1] > 0
    *-- some cartons in carton range related to another palette.
    *-- OK
    = gfModalGen("INM44047B00000","Dialog")
  ELSE
    *-- This part is to avoid entering a new palette number less
    *-- than the first palette number.
    *-- It also will avoid having gaps between palette numbers.
    *-- In order to check if there is a gap between two palette numbers
    *-- we subtract the last pallet number (lnMaxPal) from the currently
    *-- entered pallet number, the difference should not be greater than 1.
    *-- this rule is valid, except for the first pallet number to be
    *-- entered, because this rule will prevent the user from start
    *-- counting the pallets using any number greater than 1, that's why
    *-- we should not use that rule when this is the first pallet number
    *-- to be entered (in other words when lnMaxPal = 0).

    IF loFormSet.llPalStat AND loFormSet.lnCtnPal < loFormSet.lnMinPal OR ;
        (loFormSet.lnCtnPal - IIF(loFormSet.lnMaxPal=0,loFormSet.lnCtnPal,loFormSet.lnMaxPal)) > 1
      *-- palette No. has to be sequential.
      *-- OK
      = gfModalGen("INM44048B00000","Dialog")
      loFormSet.lnCtnPal = loFormSet.lnMaxPal + 1
      loFormSet.AriaForm1.pgfPacking.DETAIL.txtPaletteNo.REFRESH
    ELSE
      lcTag = ORDER(loFormSet.lcPckLin)
      SET ORDER TO SELECTED IN (loFormSet.lcPckLin)
      IF SEEK('Y',loFormSet.lcPckLin)
        SCAN FOR SELECTED
          *-- Here, is comparison between packed quantity and ordered quantity
          *-- in ordline file and give choice to modify the ordered qauntity if
          *-- packed exceeded the ordered
          lcSelFld    = loFormSet.lcPckLin + '.lSelect'
          lcCtnQtyFld = loFormSet.lcPckLin + '.CtnQty'
          lcOrdQtyFld = loFormSet.lcPckLin + '.OrdQty'
          lcPQtyFld   = loFormSet.lcPckLin + '.PQty'
          lcSizeFld   = loFormSet.lcPckLin + '.cSizeCod'
          llClearPrPck = .F.
          IF !EMPTY(&lcSelFld) AND !EMPTY(&lcCtnQtyFld) AND ;
              &lcOrdQtyFld < &lcPQtyFld + (&lcCtnQtyFld*(loFormSet.lnTo-loFormSet.lnFrom+1))
            *-- Packed quantity for Style/Size x/x exceeds ordered quantity.;
            Do you want to modify the orderd quantity?
            *-- <Yes>,<No>
            lnContinue = gfModalGen("QRM44034B00006","Dialog",;
              EVALUATE(loFormSet.lcPckLin+'.Style') + '/' + EVAL(lcSizeFld))
            *-- 44134 : This line has a prepack. Increasing the Quantity will release the prepack.
            *-- 00012 : <Procced> <Cancel>
            IF lnContinue = 1
              IF !EMPTY(EVALUATE(loFormSet.lcPckLin+'.PrePak'))
                llClearPrPck = (gfModalGen("QRM44134B00012","Dialog") = 1)
                IF !llClearPrPck
                  lnContinue = 2
                ENDIF
              ENDIF
            ENDIF
            EXIT
          ENDIF
        ENDSCAN
      ENDIF
      IF lnContinue <> 2
        IF SEEK('Y',loFormSet.lcPckLin)
          SELECT(loFormSet.lcPckLin)
          SCAN FOR SELECTED
            lcSizeNum = EVALUATE(loFormSet.lcPckLin+'.cSizeNo')
            *-- This for updating order qty with the new order qty if the user
            *-- select to update order qty with exceeds qty.

            IF lnContinue = 1
              SELECT (loformSet.lcPckLin)
              *E037427,1 WAM 05/05/2005 Fix update ordered quantity
              *REPLACE OrdQty    WITH IIF(!lSelect OR (!lSelect AND EMPTY(CtnQty)),OrdQty,;
              MAX(OrdQty,OrgPQty+;
              (CtnQty*(loFormSet.lnTo-loFormSet.lnFrom+1)))),;
              OrdTotQty WITH OrdQty
              REPLACE OrdQty    WITH IIF(!lSelect OR (!lSelect AND EMPTY(CtnQty)),OrdQty,;
                MAX(OrdQty,PQty+;
                (CtnQty*(loFormSet.lnTo-loFormSet.lnFrom+1)))),;
                OrdTotQty WITH OrdQty
              *E037427,1 WAM 05/05/2005 (End)
            ENDIF

            REPLACE OQty        WITH IIF(!lSelect AND EMPTY(CtnQty),;
              OQty,MAX(0,OQty-(CtnQty*(loFormSet.lnTo-loFormSet.lnFrom+1)))),;
              OTotQty     WITH OQty,;
              PQty        WITH IIF(!lSelect AND EMPTY(CtnQty),PQty,;
              PQty+(CtnQty*(loFormSet.lnTo-loFormSet.lnFrom+1))),;
              PTotQty     WITH PQty,;
              PWgh        WITH IIF(!lSelect AND EMPTY(CtnQty),PWgh,;
              PWgh+ StyWgh*(CtnQty*(loFormSet.lnTo-loFormSet.lnFrom+1))),;
              PTotWgh     WITH PWgh,;
              cOpen       WITH IIF(OQty > 0 AND AvlQty > 0,'Y','N')
            IF llClearPrPck
              REPLACE PrePak WITH "",;
                PpQty  WITH 0
            ENDIF
            IF !EMPTY(PACK_ID)
              SET ORDER TO (loFormSet.lcSumPck) IN (loFormSet.lcSumPck)
              lcSeekExpr = PADR(PACK_ID,19)+cPkColor+cPckSize+cPkVersion
            ELSE
              SET ORDER TO lcSumPkLn IN (loFormSet.lcSumPck)
              lcSeekExpr = STYLE+Dyelot+STR(nOrdLineNo,6)+cSizeNo
            ENDIF
            IF SEEK(lcSeekExpr,loFormSet.lcSumPck)
              SELECT (loFormSet.lcSumPck)
              *E037427,1 WAM 05/05/2005 Correct update of weight
              *REPLACE PTotWgh WITH IIF(!lSelect AND EMPTY(CtntotQty),PtotWgh,;
              PtotWgh+ StyWgh*(CtntotQty*(loFormSet.lnTo-loFormSet.lnFrom+1)))
              REPLACE PTotWgh WITH IIF(!lSelect AND EMPTY(EVALUATE(loformSet.lcPckLin+'.CtnQty')),PtotWgh,;
                PtotWgh+ StyWgh*(EVALUATE(loformSet.lcPckLin+'.CtnQty')*(loFormSet.lnTo-loFormSet.lnFrom+1)))
              *E037427,1 WAM 05/05/2005 (End)
              SELECT (loFormset.lcPckLin)
            ENDIF

            STORE 0 TO lnQuantities,lnWeights

            FOR lnJ = 0 TO (loFormSet.lnTo-loFormSet.lnFrom)
              lnQuantities = CtnQty
              lnWeights    = StyWgh*CtnQty

              IF lnQuantities > 0
                IF SEEK(STR(loFormSet.lnFrom+lnJ,4),loFormSet.lcCtnHdr)
                  SELECT (loFormSet.lcCtnHdr)
                  REPLACE TotPcs WITH TotPcs + lnQuantities,;
                    TotWgh WITH TotWgh + lnWeights

                  loFormSet.lnPackQty = loFormSet.lnPackQty + lnQuantities
                  loFormSet.lnPackWgh = loFormSet.lnPackWgh + lnWeights

                  SELECT(loFormSet.lcPckLin)
                ELSE

                  *! B610287,1 HIA 04/02/2013 Aria XP - Manual Packing List - Assign default, T20130226.0030 [Start]

                  *INSERT INTO (loFormSet.lcCtnHdr)(Cart_No, Pal_No, TotPcs, TotWgh, EMPTY,CCARRCTNID,cCrtnVlTyp);
                  *  VALUES (loFormSet.lnFrom+lnJ,loFormSet.lnCtnPal,lnQuantities,;
                  *  lnWeights,'N',"",EVALUATE(loFormSet.lcPckLin+'.cCrtnVlTyp'))


                  INSERT INTO (loFormSet.lcCtnHdr)(Cart_No, Pal_No, TotPcs, TotWgh, EMPTY,CCARRCTNID,cCrtnVlTyp);
                    VALUES (loFormSet.lnFrom+lnJ,loFormSet.lnCtnPal,lnQuantities,;
                    lnWeights,'N',"",loFormSet.AriaForm1.pgfPacking.CartonInfo.cboCrtType.VALUE)
                  *! B610287,1 HIA 04/02/2013 Aria XP - Manual Packing List - Assign default, T20130226.0030 [End]                    
                    
                    
                  loFormSet.lnMaxCtn  = MAX(loFormSet.lnMaxCtn,loFormSet.lnFrom+lnJ)
                  loFormSet.lnPackWgh = loFormSet.lnPackWgh + EVALUATE(loFormSet.lcCtnHdr+'.TotWgh')
                  loFormSet.lnPackCtn = loFormSet.lnPackCtn + 1
                  loFormSet.lnPackQty = loFormSet.lnPackQty + EVALUATE(loFormSet.lcCtnHdr+'.TotPcs')
                ENDIF
                loFormSet.lnMaxPal  = MAX(loFormSet.lnMaxPal,EVALUATE(loFormSet.lcCtnHdr+'.Pal_No'))
                loFormSet.lnMinPal  = MIN(IIF(loFormSet.lnMinPal=0,;
                  EVALUATE(loFormSet.lcCtnHdr+'.Pal_No'),;
                  loFormSet.lnMinPal),EVALUATE(loFormSet.lcCtnHdr+'.Pal_No'))
              ENDIF
              IF lnQuantities > 0
                SELECT (loFormSet.lcCtnDtl)
                IF !SEEK(STR(loFormSet.lnFrom+lnJ,4)+EVALUATE(loFormSet.lcPckLin+'.Style')+EVALUATE(loFormSet.lcPckLin+'.Dyelot')+;
                    STR(EVALUATE(loFormSet.lcPckLin+'.nOrdLineNo'),6)+lcSizeNum,;
                    loFormSet.lcCtnDtl)
                  APPEND BLANK
                ENDIF

                REPLACE STYLE      WITH EVALUATE(loFormSet.lcPckLin+'.Style'),;
                  OrgWgh     WITH EVALUATE(loFormSet.lcPckLin+'.OrgStyWgh'),;
                  nOrdLineNo WITH EVALUATE(loFormSet.lcPckLin+'.nOrdLineNo'),;
                  SzCnt      WITH EVALUATE(loFormSet.lcPckLin+'.SzCnt'),;
                  cStatus    WITH "A",;
                  Cart_No    WITH loFormSet.lnFrom+lnJ,;
                  SIZE       WITH IIF(EVALUATE(loFormset.lcPckLin+'.lSelect') AND ;
                  EVALUATE(loFormSet.lcPckLin+'.CtnQty')>0,;
                  EVALUATE(loFormSet.lcPckLin+'.cSizeCod'),;
                  SIZE),;
                  Br         WITH !EMPTY(SIZE),;
                  Qty        WITH IIF(!EVALUATE(loFormSet.lcPckLin+'.lSelect') AND;
                  EMPTY(loFormSet.lcPckLin+'.CtnQty'),;
                  Qty,Qty+EVALUATE(loFormSet.lcPckLin+'.CtnQty')),;
                  TotQty     WITH Qty,;
                  Dyelot     WITH EVALUATE(loFormSet.lcPckLin+'.Dyelot'),;
                  cSizeNo    WITH lcSizeNum,;
                  PrePak     WITH EVALUATE(loFormSet.lcPckLin+'.PrePak'),;
                  PpQty      WITH EVALUATE(loFormSet.lcPckLin+'.PpQty')
                *! E302698,1 MMT 05/25/2010 Display SKU# in Style Detail and Carton Detail grids[Start]
                REPLACE SKUNUM WITH lfGetSKU(loFormSet)
                *! E302698,1 MMT 05/25/2010 Display SKU# in Style Detail and Carton Detail grids[End]

                IF !EMPTY(EVALUATE(loFormSet.lcPckLin+'.PACK_ID')+;
                    EVALUATE(loFormSet.lcPckLin+'.cPkColor')+;
                    EVALUATE(loFormSet.lcPckLin+'.cPckSize')+;
                    EVALUATE(loFormSet.lcPckLin+'.cPkVersion'))
                  IF !loFormSet.llShoPckSu
                    SET ORDER TO (loFormSet.lcSumPck) IN (loFormSet.lcSumPck)
                    =SEEK(PADR(EVALUATE(loFormSet.lcPckLin+'.PACK_ID'),19)+;
                      EVALUATE(loFormSet.lcPckLin+'.cPkColor')+;
                      EVALUATE(loFormSet.lcPckLin+'.cPckSize')+;
                      EVALUATE(loFormSet.lcPckLin+'.cPkVersion'),loFormSet.lcSumPck)
                    REPLACE PACK_ID    WITH EVALUATE(loFormSet.lcPckLin+'.PACK_ID'),;
                      cPkColor   WITH EVALUATE(loFormset.lcPckLin+'.cPkColor'),;
                      cPCkSize   WITH EVALUATE(loFormSet.lcPckLin+'.cPckSize'),;
                      cPKVersion WITH EVALUATE(loFormSet.lcPckLin+'.cPkVersion'),;
                      nPackNO    WITH EVALUATE(loFormSet.lcSumPck+'.CtnTotQty'),;
                      cPackId    WITH IIF(!EMPTY(Pack_Id),;
                      ALLTRIM(Pack_Id)+'-'+ ALLTRIM(cPkColor)+'-'+;
                      ALLTRIM(lfGetSize(cPckSize,loFormSet))+'-'+;
                      ALLTRIM(cPkVersion),Pack_Id+'-'+cPkColor+'-'+;
                      lfGetSize(cPckSize,loFormSet)+'-'+cPkVersion)
                  ELSE
                    loFormSet.SPCK_LIN.SetOrder('SPCK_LINVR')
                    =loFormSet.SPCK_LIN.SEEK('P'+lcAccount+PADR(EVALUATE(loFormSet.lcPckLin+'.PACK_ID'),16)+;
                      EVALUATE(loFormSet.lcPckLin+'.cPkColor')+;
                      EVALUATE(loFormSet.lcPckLin+'.cPckSize')+;
                      EVALUATE(loFormSet.lcPckLin+'.cPkVersion')+;
                      PADR(EVALUATE(loFormSet.lcPckLin+'.STYLE'),19)+EVALUATE(loFormSet.lcPckLin+'.Dyelot')) OR ;
                      loFormSet.SPCK_LIN.SEEK('P*****'+PADR(EVALUATE(loFormSet.lcPckLin+'.PACK_ID'),16)+;
                      EVALUATE(loFormSet.lcPckLin+'.cPkColor')+;
                      EVALUATE(loFormSet.lcPckLin+'.cPckSize')+;
                      EVALUATE(loFormSet.lcPckLin+'.cPkVersion')+;
                      PADR(EVALUATE(loFormSet.lcPckLin+'.STYLE'),19)+EVALUATE(loFormSet.lcPckLin+'.Dyelot'))
                    *E037427,1 WAM 05/05/2005 Correct update of packed packs
                    *lnpackNo = EVALUATE(loFormSet.lcPckLin+'.CtnTotQty') / SPCK_LIN.ToTQty
                    lnpackNo = EVALUATE(loFormSet.lcSumPck+'.CtnTotQty')
                    *E037427,1 WAM 05/05/2005 (End)

                    REPLACE PACK_ID    WITH EVALUATE(loFormSet.lcPckLin+'.PACK_ID'),;
                      cPkColor   WITH EVALUATE(loFormSet.lcPckLin+'.cPkColor'),;
                      cPCkSize   WITH EVALUATE(loFormSet.lcPckLin+'.cPckSize'),;
                      cPKVersion WITH EVALUATE(loFormSet.lcPckLin+'.cPkVersion'),;
                      nPackNO    WITH lnPackNo,;
                      cPackId    WITH IIF(!EMPTY(Pack_Id),;
                      ALLTRIM(Pack_Id)+'-'+ ALLTRIM(cPkColor)+'-'+;
                      ALLTRIM(lfGetSize(cPckSize,loFormSet))+'-'+;
                      ALLTRIM(cPkVersion),Pack_Id+'-'+cPkColor+'-'+;
                      lfGetSize(cPckSize,loFormSet)+'-'+cPkVersion)
                  ENDIF
                ENDIF
                REPLACE Weight    WITH IIF(!EVALUATE(loFormSet.lcPckLin+'.lSelect') AND;
                  EMPTY(EVALUATE(loFormSet.lcPckLin+'.CtnQty')),;
                  Weight,Weight+EVALUATE(loFormSet.lcPckLin+'.StyWgh')*;
                  (EVALUATE(loFormSet.lcPckLin+'.CtnQty'))),;
                  TotWeight WITH Weight,;
                  cStatus   WITH IIF(EVALUATE(loFormSet.lcPckLin+'.Selected'),'M',cStatus)
              ENDIF
              SELECT(loFormSet.lcPckLin)
            ENDFOR

            REPLACE CtnQty  	WITH IIF(!loFormSet.llClrSel AND lSelect,EVALUATE(loFormSet.lcPckLin+'.OQty'),0),;
              CtnTotQty WITH CtnQty,;
              lSelect   WITH IIF(!loFormSet.llClrSel AND lSelect,.T.,.F.),;
              SELECTED 	WITH IIF(!loFormSet.llClrSel,SELECTED,.F.)

            lnPkRecNo = RECNO()
            IF SEEK(EVALUATE(loFormSet.lcPckLin+'.Pack_ID')+;
                EVALUATE(loFormSet.lcPckLin+'.cPkColor')+;
                EVALUATE(loFormSet.lcPckLin+'.cPckSize')+;
                EVALUATE(loFormSet.lcPckLin+'.cPkVersion')+;
                STR(EVALUATE(loFormSet.lcPckLin+'.nOrdLineNo'),6)+;
                EVALUATE(loFormSet.lcPckLin+'.Style')+EVALUATE(loFormSet.lcPckLin+'.Dyelot') ,loFormSet.lcPckLin)
              SELECT (loFormSet.lcPckLin)

              REPLACE CtnQty    WITH IIF(!loFormSet.llClrSel AND lSelect,;
                EVALUATE(loFormSet.lcPckLin+'.OQty'),0),;
                CtnTotQty WITH CtnQty,;
                lSelect   WITH IIF(!loFormSet.llClrSel AND lSelect,.T.,.F.),;
                SELECTED  WITH IIF(!loFormSet.llClrSel,SELECTED,.F.)
              SELECT(loFormSet.lcPckLin)
            ENDIF
            GOTO lnPkRecNo
          ENDSCAN
        ENDIF
        SELECT (loFormSet.lcSumPck)
        SCAN FOR lSelect
          *E037427,1 WAM 05/05/2005 Correct update of packed quantity
          *!*	          REPLACE PTotQty    WITH PTotQty + CtnTotQty,;
          *!*	                  OTotQty    WITH MAX(0,OTotQty-CtnTotQty),;
          *!*	                  CtnTotQty  WITH OTotQty,;
          *!*	                  cOpen      WITH IIF(OTotQty > 0 ,'Y','N')
          REPLACE PTotQty   WITH PTotQty + CtnTotQty*(loFormSet.lnTo-loFormSet.lnFrom+1),;
            OTotQty   WITH MAX(0,OTotQty-CtnTotQty*(loFormSet.lnTo-loFormSet.lnFrom+1)),;
            CtnTotQty WITH OTotQty,;
            cOpen     WITH IIF(OTotQty > 0 ,'Y','N')
          *E037427,1 WAM 05/05/2005 (End)
        ENDSCAN

        lnOldFrm = loFormSet.lnFrom
        lnOldTo  = loFormSet.lnTo
        STORE loFormSet.lnPackCtn + 1 TO loFormSet.lnFrom,loFormSet.lnTo
        loFormSet.AriaForm1.pgfPacking.DETAIL.txtCartFrom.REFRESH
        loFormSet.AriaForm1.pgfPacking.DETAIL.txtCartTo.REFRESH

        IF loFormSet.llPalStat
          STORE loFormSet.lnMaxPal + 1 TO loFormSet.lnCtnPal
        ENDIF
      ENDIF
      SET ORDER TO lcTag IN (loFormSet.lcPckLin)
    ENDIF
  ENDIF
ENDIF
SELECT (loFormSet.lcPackLines)
ZAP
SELECT(loFormSet.lcPckLin)
SCAN
  SCATTER MEMVAR MEMO
  *IF EMPTY(m.Pack_Id) & wael
  INSERT INTO (loFormSet.lcPackLines) FROM MEMVAR
  *ENDIF
ENDSCAN
SELECT (loFormSet.lcPckLin)
GO TOP
=RLOCK(loFormSet.lcPckLin)
UNLOCK IN (loFormSet.lcPckLin)

llApply = .T.

SELECT (loFormSet.lcPckLin)
IF BETWEEN(lnRecNom ,1,RECCOUNT(loFormSet.lcPckLin))
  GOTO lnRecNom
ENDIF
SELECT (loFormSet.lcSumPck)
IF BETWEEN(lnRecNo,1,RECCOUNT(loFormSet.lcSumPck))
  GOTO lnRecNo
ENDIF

SELECT (loFormSet.lcCtnHdr)
LOCATE
= RLOCK(loFormSet.lcCtnHdr)
UNLOCK IN (loFormSet.lcCtnHdr)

SELECT (loFormSet.lcCtnDtl)
LOCATE
= RLOCK(loFormSet.lcCtnDtl)
UNLOCK IN (loFormSet.lcCtnDtl)

loFormSet.AriaForm1.pgfPacking.HEADER.txtWeight.VALUE  = loFormSet.lnPackWgh
loFormSet.AriaForm1.pgfPacking.HEADER.txtCartons.VALUE = loFormSet.lnPackCtn
loFormSet.AriaForm1.pgfPacking.HEADER.txtPieces.VALUE  = loFormSet.lnPackQty

*-- Browse for cartons header in the caron information folder
=lfCtnHdrBr(loFormSet)
*-- Browse for cartons details in the caron information folder
=lfCtnDtlBr(loFormSet)

*-- Print labels for all selected cartons
IF loFormSet.llScrPrnLb
  PRIVATE lnDetRec , lnCrtRec , lnCarRef , lcCurrAlis

  *-- Save current setting
  lcCurrAlis = SELECT (0)
  lnDetRec   = RECNO(loFormSet.lcCtnDtl)
  lnCrtRec   = RECNO(loFormSet.lcCtnDtl)
  lnCarRef = 0
  FOR lnCarRef = lnOldFrm TO lnOldTo
    =SEEK(STR(lnCarRef,4),loFormSet.lcCtnDtl) AND lfvLblInfo(loFormSet,lnCarRef,loFormSet.llScrPrnLb)
  ENDFOR

  *-- Restore Setting.
  IF BETWEEN(lnDetRec,1,RECCOUNT(loFormSet.lcCtnDtl))
    GO lnDetRec IN (loFormSet.lcCtnDtl)
  ENDIF
  SELECT (lcCurrAlis)
ENDIF
=lfwDtlBrow(loFormSet)
SELECT (lnCurAlias)
*-- END OF lfvApply

*!*************************************************************
*! Name      : lfObjStatus
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : To set the object status
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!			   llStatus  : .T. or .F.
*!			   lcScrMode : FormSet Activemode.
*!*************************************************************
FUNCTION lfObjStatus
LPARAMETERS loFormSet,lcScrMode,llStatus

*E037427,1 WAM 05/05/2005 to prevent calling changemode method with empty screen mode
IF !INLIST(lcScrMode,'S','V','A','E')
  RETURN
ENDIF
*E037427,1 WAM 05/05/2005 (End)

WITH loFormSet.AriaForm1
  STORE .F.   TO .kbPackNo.ENABLED,  .kbOrderNo.ENABLED, .kbPkTktNo.ENABLED, .kbAccount.ENABLED,;
    .kbStore.ENABLED,   .txtCustName.ENABLED, .txtStoreName.ENABLED,;
    .txtCustPo.ENABLED, .pgfPacking.HEADER.txtDept.ENABLED,;
    .pgfPacking.HEADER.txtWeight.ENABLED, .pgfPacking.HEADER.txtCartons.ENABLED,;
    .pgfPacking.HEADER.txtPieces.ENABLED

  DO CASE
  CASE lcScrMode = 'S'
    STORE .T. TO .kbPackNo.ENABLED, .kbOrderNo.ENABLED, .kbAccount.ENABLED
    STORE ""  TO .kbPackNo.keytextbox.VALUE, .kbOrderNo.keytextbox.VALUE, .kbPkTktNo.keytextbox.VALUE, .kbAccount.keytextbox.VALUE,;
      .kbStore.keytextbox.VALUE,  .txtCustName.VALUE, .txtStoreName.VALUE,;
      .txtCustPo.VALUE, .pgfPacking.HEADER.txtDept.VALUE,;
      .pgfPacking.HEADER.txtWeight.VALUE, .pgfPacking.HEADER.txtCartons.VALUE,;
      .pgfPacking.HEADER.txtPieces.VALUE

    WITH .pgfPacking.HEADER
      IF !loFormSet.llEdiSys
        STORE .F. TO .lblPackCharCode.VISIBLE, .lblSimH10.VISIBLE, .txtPackCharCode.VISIBLE,;
          .lblPackDescCode.VISIBLE, .lblSimH12.VISIBLE, .txtpackDescCode.VISIBLE
        STORE .F. TO .lblBOL.VISIBLE, .lblSimH3.VISIBLE, .txtBol.VISIBLE
      ELSE
        STORE !loFormSet.llBolnever TO .lblBOL.VISIBLE, .lblSimH3.VISIBLE, .txtBol.VISIBLE
      ENDIF
    ENDWITH
    .SETALL('Value','','AriaTextBox')
    *.kbPackNo.keytextbox.SetFocus
    *-- Detail Information
    WITH .pgfPacking.DETAIL
      IF !loFormSet.llEdiSys
        STORE .F. TO .txtpaletteNo.VISIBLE, .lblDtSimCol3.VISIBLE, .lblpalette.VISIBLE
      ENDIF
      .cmdPack.VISIBLE = loFormSet.llUsePack
      .txtCartFrom.CONTROLSOURCE   = "THISFORMSET.lnFrom"
      .txtCartTo.CONTROLSOURCE     = "THISFORMSET.lnTo"
      .txtPaletteNo.CONTROLSOURCE  = "THISFORMSET.lnCtnPal"
      .txtQtyPerCart.CONTROLSOURCE = "THISFORMSET.lnBrCtnQty"
      .txtUnitWeight.CONTROLSOURCE = "THISFORMSET.lnBrUntWgh"
    ENDWITH
    *-- Carton Information
    WITH .pgfPacking.CartonInfo
      *-- Carton header information.
      IF !loFormSet.llEdiSys
        STORE .F. TO .lblPaletteNoH.VISIBLE, .lblSimColH3.VISIBLE, .txtPaletteNoH.VISIBLE
      ENDIF
      .txtCartonNo.CONTROLSOURCE      = loFormSet.lcCtnHdr + '.Cart_No'
      .txtTotWghH.CONTROLSOURCE       = loFormSet.lcCtnHdr + '.TotWgh'
      .txtPaletteNoH.CONTROLSOURCE    = loFormSet.lcCtnHdr + '.Pal_No'
      .cboCrtType.VALUE   		    = EVALUATE(loFormSet.lcCtnHdr + '.CCRTNVLTYP')
      .txtCarrierCartId.CONTROLSOURCE = loFormSet.lcCtnHdr + '.cCarrCtnID'

      *-- Carton Detail information.
      .kbStyle.CONTROLSOURCE   		= loFormSet.lcCtnDtl + '.Style'
      .lblConfgDyeD.VISIBLE 		   = loFormSet.llUseConfg
      .kbConfiguration.VISIBLE 		= loFormSet.llUseConfg
      .kbConfiguration.CONTROLSOURCE   = loFormSet.lcCtnDtl + '.Dyelot'
      .kbSize.CONTROLSOURCE		    = loFormSet.lcCtnDtl + '.Size'

      *! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[Start]
      *!*        .lblSizeCartD.LEFT 			  = IIF(loFormSet.llUseConfg,500,351)
      *!*        .kbSize.LEFT 					= IIF(loFormSet.llUseConfg,498,350)
      *! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[End]
    ENDWITH

  CASE lcScrMode = 'V'

    .pgfPacking.DETAIL.grdDetail.SETALL('READONLY',.T.,'COLUMN')
    .pgfPacking.DETAIL.grdDetail.REFRESH
    .pgfPacking.DETAIL.grdDetail.COLUMNS(1).ENABLED = .F.
    .pgfPacking.DETAIL.grdDetail.COLUMNS(1).READONLY = .F.
    WITH .pgfPacking.DETAIL
      STORE .F. TO .cmdSelect.ENABLED,.cmdSelAll.ENABLED,.cmdInvert.ENABLED,;
        .cmdSelNone.ENABLED,.cmdPack.ENABLED,.cmdStyle.ENABLED,.cmdApply.ENABLED,;
        .txtCartFrom.ENABLED,.txtCartTo.ENABLED,.txtPaletteNo.ENABLED,;
        .txtQtyPerCart.ENABLED,.txtUnitWeight.ENABLED
    ENDWITH
  CASE lcScrMode = 'E'
    .pgfpacking.cartonInfo.txtCarrierCartId.ENABLED = .pgfpacking.HEADER.chkAssCarrID.VALUE

  CASE lcScrMode = 'A'
    *=lfClrCntrS(loFormSet)
    STORE .F. TO .kbPackNo.ENABLED
    STORE .T. TO .kbOrderNo.ENABLED, .kbPkTktNo.ENABLED, .kbAccount.ENABLED
    STORE .F. TO .pgfPacking.HEADER.txtNoteH.ENABLED,.pgfPacking.HEADER.cboShipViaH.ENABLED,;
      .pgfPacking.HEADER.txtBOL.ENABLED,.pgfPacking.HEADER.txtSpInst1.ENABLED,;
      .pgfPacking.HEADER.txtSpInst2.ENABLED,.pgfPacking.HEADER.cboCntType.ENABLED,;
      .pgfPacking.HEADER.cboDirectTo.ENABLED,;
      .pgfPacking.HEADER.txtPackCharCode.ENABLED,.pgfPacking.HEADER.txtPackDescCode.ENABLED
    .SETALL('Value','','AriaTextBox')
    .kbOrderNo.keytextbox.SETFOCUS
    .pgfPacking.DETAIL.grdDetail.SETALL('READONLY',.T.,'COLUMN')
    .pgfPacking.DETAIL.grdDetail.REFRESH
    .pgfPacking.DETAIL.grdDetail.COLUMNS(1).ENABLED = .T.
    .pgfPacking.DETAIL.grdDetail.COLUMNS(1).READONLY = .F.

  ENDCASE
ENDWITH

*!*************************************************************
*! Name      : lfWinHdrSt
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : adjust enable status for header folder fields
*!*************************************************************
*! Called    : From ChangeMode in the formset
*!*************************************************************
FUNCTION lfWinHdrSt
LPARAMETERS loFormSet

IF loFormSet.ActiveMode = 'E' OR loFormSet.llNew
  WITH loFormSet.AriaForm1
    STORE .F. TO .kbPackNo.ENABLED,  .kbOrderNo.ENABLED, .kbPkTktNo.ENABLED, .kbAccount.ENABLED,;
      .kbStore.ENABLED,   .txtCustName.ENABLED, .txtStoreName.ENABLED,;
      .txtCustPo.ENABLED, .pgfPacking.HEADER.txtDept.ENABLED,;
      .pgfPacking.HEADER.txtWeight.ENABLED, .pgfPacking.HEADER.txtBOL.ENABLED,;
      .pgfPacking.HEADER.txtCartons.ENABLED,.pgfPacking.HEADER.txtPieces.ENABLED
    STORE .T. TO .pgfPacking.HEADER.txtNoteH.ENABLED,.pgfPacking.HEADER.cboShipViaH.ENABLED,;
      .pgfPacking.HEADER.txtSpInst1.ENABLED,.pgfPacking.HEADER.txtSpInst2.ENABLED
    .pgfPacking.HEADER.txtBOL.ENABLED = IIF(loFormSet.llBolAutom,.F.,.T.)
    *: B610016,1 SAB 07/16/2012 Enable Editing Notes and Special Inst. for Completed Packs [Start]
    .pgfpacking.HEADER.cboshipviah.ENABLED = IIF(loformset.activemode='E' .AND. pack_hdr.STATUS='C', .F., .T.)
    .pgfpacking.HEADER.txtbol.ENABLED = IIF(loformset.activemode='E' .AND. pack_hdr.STATUS='C', .F., IIF(loformset.llbolautom, .F., .T.))
    *: B610016,1 SAB 07/16/2012 Enable Editing Notes and Special Inst. for Completed Packs [End]
  ENDWITH
ENDIF


*!*************************************************************
*! Name      : lfActPad
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : To create the "Option" menu pad.
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
*! Called    : From ChangeMode in the formset
*!*************************************************************
FUNCTION lfActPad
LPARAMETERS loFormSet

loFormSet.llCanPrnLb = gfUserPriv('AL','ALPLIST','PRNPACKING')
*-- Define the "Options" menu pad.
DEFINE PAD _OPTIONS OF (loFormSet.chostformname) PROMPT LANG_ManulPL_OptOptnMnu KEY ALT+P , SPACE(1)
lcHostFormName = '[' + loFormSet.cHostFormName + ']'

*-- Define the "Options" and the "Transactions" popups.
DEFINE POPUP _OPTPOP MARGIN SHADOW

*-- Define the "Options" bars in the "Options" menu pad.
*-- Define the "Transactions" bars in the "Transactions" menu pad.
DEFINE BAR 1 OF _OPTPOP PROMPT LANG_ManulPL_OptClerApp SKIP FOR gfFormIsActive(&lcHostFormName) .AND.;
  !(!(_SCREEN.ACTIVEFORM.PARENT.ActiveMode='S') AND _SCREEN.ACTIVEFORM.pgfPacking.ACTIVEPAGE=2) KEY ALT+C

DEFINE BAR 2 OF _OPTPOP PROMPT LANG_ManulPL_OptShowOpen SKIP FOR gfFormIsActive(&lcHostFormName) .AND.;
  !(!(_SCREEN.ACTIVEFORM.PARENT.ActiveMode='S') AND _SCREEN.ACTIVEFORM.pgfPacking.ACTIVEPAGE=2) KEY ALT+O
IF loFormSet.llCanPrnLb
  DEFINE BAR 3 OF _OPTPOP PROMPT "\-"

  DEFINE BAR 4 OF _OPTPOP PROMPT LANG_ManulPL_OptPrintLbl SKIP FOR gfFormIsActive(&lcHostFormName) .AND.;
    (INLIST(_SCREEN.ACTIVEFORM.PARENT.ActiveMode,'S','V') OR ;
    (_SCREEN.ACTIVEFORM.pgfPacking.ACTIVEPAGE !=2))

  DEFINE BAR 5 OF _OPTPOP PROMPT LANG_ManulPL_OptStyFiltr SKIP FOR gfFormIsActive(&lcHostFormName) .AND.;
    (INLIST(_SCREEN.ACTIVEFORM.PARENT.ActiveMode,'S','V') OR (_SCREEN.ACTIVEFORM.pgfPacking.ACTIVEPAGE!=2) OR ;
    lfChkfile(_SCREEN.ACTIVEFORM.PARENT,_SCREEN.ACTIVEFORM.PARENT.lcPckLin))
  DEFINE BAR 6 OF _OPTPOP PROMPT LANG_ManulPL_OptUpdPiktkt SKIP FOR gfFormIsActive(&lcHostFormName) .AND.;
    (INLIST(_SCREEN.ACTIVEFORM.PARENT.ActiveMode,'S','V') OR ;
    (_SCREEN.ACTIVEFORM.pgfPacking.ACTIVEPAGE=1))

  SET MARK OF BAR 6 OF _OPTPOP TO loFormSet.llUpdtPkTk
ELSE
  DEFINE BAR 3 OF _OPTPOP PROMPT "\-"
  DEFINE BAR 4 OF _OPTPOP PROMPT LANG_ManulPL_OptStyFiltr SKIP FOR gfFormIsActive(&lcHostFormName) .AND.;
    (INLIST(_SCREEN.ACTIVEFORM.PARENT.ActiveMode,'S','V') OR ;
    (_SCREEN.ACTIVEFORM.pgfPacking.ACTIVEPAGE!=2) OR ;
    lfChkfile(_SCREEN.ACTIVEFORM.PARENT,_SCREEN.ACTIVEFORM.PARENT.lcPckLin))
  DEFINE BAR 5 OF _OPTPOP PROMPT LANG_ManulPL_OptUpdPiktkt SKIP FOR gfFormIsActive(&lcHostFormName) .AND.;
    (INLIST(_SCREEN.ACTIVEFORM.PARENT.ActiveMode,'S','V') OR ;
    (_SCREEN.ACTIVEFORM.pgfPacking.ACTIVEPAGE=1))

  SET MARK OF BAR 5 OF _OPTPOP TO loFormSet.llUpdtPkTk
ENDIF

*-- Define the action to be done when activating the pad and the popup.
ON PAD _OPTIONS OF (loFormSet.chostformname) ACTIVATE POPUP _OPTPOP
*-- Clear selection upon apply
ON SELECTION BAR 1 OF _OPTPOP lfClrSel(_SCREEN.ACTIVEFORM.PARENT)

*-- Show open Qty only
ON SELECTION BAR 2 OF _OPTPOP lfShwOpn(_SCREEN.ACTIVEFORM.PARENT)

*-- Print Label
IF loFormSet.llCanPrnLb
  ON SELECTION BAR 4 OF _OPTPOP lfPortScr(_SCREEN.ACTIVEFORM.PARENT)
ENDIF

*-- Style Filter
ON SELECTION BAR IIF(loFormSet.llCanPrnLb,5,4) OF _OPTPOP lfFltrScr(_SCREEN.ACTIVEFORM.PARENT)

*-- Update Pick ticket Qty. upon saving
ON SELECTION BAR IIF(loFormSet.llCanPrnLb,6,5) OF _OPTPOP lfUpdtPkTk(_SCREEN.ACTIVEFORM.PARENT)

*-- Pack Summary / Detail
IF loFormSet.llUsePack
  IF loFormSet.llShoPckSu
    DEFINE BAR IIF(loFormSet.llCanPrnLb,7,6) OF _OPTPOP PROMPT LANG_ManulPL_OptPckSummry  SKIP FOR gfFormIsActive(&lcHostFormName) .AND.;
      (_SCREEN.ACTIVEFORM.PARENT.ActiveMode='S' OR ;
      _SCREEN.ACTIVEFORM.pgfPacking.ACTIVEPAGE=1)

    ON SELECTION BAR IIF(loFormSet.llCanPrnLb,7,6) OF _OPTPOP lfShowDtSum(_SCREEN.ACTIVEFORM.PARENT)
  ELSE
    DEFINE BAR IIF(loFormSet.llCanPrnLb,7,6) OF _OPTPOP PROMPT LANG_ManulPL_OptPckDetail  SKIP FOR gfFormIsActive(&lcHostFormName) .AND.;
      (_SCREEN.ACTIVEFORM.PARENT.ActiveMode='S' OR  _SCREEN.ACTIVEFORM.pgfPacking.ACTIVEPAGE=1)

    ON SELECTION BAR IIF(loFormSet.llCanPrnLb,7,6) OF _OPTPOP lfShowDtSum(_SCREEN.ACTIVEFORM.PARENT)
  ENDIF
ENDIF

*-- end of lfActPad.

*!*************************************************************
*! Name      : lfShwOpn
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : Show open quantity only
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfShwOpn
LPARAMETERS loFormSet
PRIVATE lnCurAlias,lcTag, lcDataSes
lcDataSes = SET("Datasession")
SET DATASESSION TO loFormSet.DATASESSIONID
lnCurAlias = SELECT(0)

*-- this to show only opened quantity or all quantity according to
*-- variable llShwOpn

SELECT (loFormSet.lcPckLin)
IF !loFormSet.llShwOpn
  loFormSet.llShwOpn = .T.
  SET MARK OF BAR 2 OF _OPTPOP TO .T.
ELSE
  loFormSet.llShwOpn = .F.
  SET MARK OF BAR 2 OF _OPTPOP TO .F.
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
*! Name      : lfShowDtSum
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : Show pack detail/summary from option menu
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfShowDtSum
LPARAMETERS loFormSet
LOCAL lcAlias,lcDataSes,lcExpr
lcDataSes = SET("Datasession")

SET DATASESSION TO loFormSet.DATASESSIONID

loFormSet.llShoPckSu = IIF(loFormSet.llShoPckSu,.F.,.T.)
lcHostFormName = '[' + loFormSet.cHostFormName + ']'

=lfDtlBrow(loFormSet)
=lfwDtlBrow(loFormSet)
IF !loFormSet.llShoPckSu
  DEFINE BAR IIF(loFormSet.llCanPrnLb,7,6) OF _OPTPOP PROMPT LANG_ManulPL_OptPckDetail  SKIP FOR gfFormIsActive(&lcHostFormName) .AND.;
    (_SCREEN.ACTIVEFORM.PARENT.ActiveMode='S' OR _SCREEN.ACTIVEFORM.pgfPacking.ACTIVEPAGE = 1)
ELSE
  DEFINE BAR IIF(loFormSet.llCanPrnLb,7,6) OF _OPTPOP PROMPT LANG_ManulPL_OptPckSummry  SKIP FOR gfFormIsActive(&lcHostFormName) .AND.;
    (_SCREEN.ACTIVEFORM.PARENT.ActiveMode='S' OR  _SCREEN.ACTIVEFORM.pgfPacking.ACTIVEPAGE = 1)
ENDIF
SET DATASESSION TO &lcDataSes

*!*************************************************************
*! Name      : lfSuppForc
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : User can Force allocation. (Y/N)
*!*************************************************************
FUNCTION lfSuppForc
PRIVATE llAlwForce
llAlwForce = .T.
IF lcForceAlo <> "Y"
  *-- No Force allocation done.
  IF lcForceAlo = "N"
    llAlwForce = .F.  && User can not

  ELSE  && User Prev.
    *-- Call user defined process.
    llAlwForce = gfUserPriv('AL','ALAUTAL','FORCING')

  ENDIF
ENDIF
RETURN llAlwForce
*-- end of lfSuppForc.

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : Show Screen Mode Changes (Select and Add) only.
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet, lcScrMode-> Screen Mode
*!*************************************************************
FUNCTION lfEvalSegs
LPARAMETERS loFormSet
LOCAL laMajSegs
lcNonMajPi = ""

lnMajSeg = gfItemMask('SM','','0001')
lnMajLen = LEN(gfItemMask('PM','','0001'))
DIMENSION laMajSegs[1,1]

*-- Compute Free/Color Items in Style code Structure. [Begin]
= gfItemMask(@laMajSegs,'','0001')

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
      laMajSegs[lnI,3],;
      lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
  ENDIF
  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

lnColorLen = LEN(lcNonMajPi)

RETURN ''
*-- end of lfEvalSegs.

*!*************************************************************
*! Name      : lfOpenFiles
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : To open all the necessary files
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
FUNCTION lfOpenFiles
LPARAMETERS loFormSet

loFormSet.Pack_Hdr = CREATEOBJECT("RemoteTable",'Pack_Hdr','Pack_Hdr','Pack_Hdr',loFormSet.DATASESSIONID)
loFormSet.Pack_Lin = CREATEOBJECT("RemoteTable",'Pack_Lin','Pack_Lin','Pack_Lin',loFormSet.DATASESSIONID)
loFormSet.OrdHdr   = CREATEOBJECT("RemoteTable",'OrdHdr','OrdHdr','OrdHdr',loFormSet.DATASESSIONID)
loFormSet.OrdLine  = CREATEOBJECT("RemoteTable",'OrdLine','OrdLine','OrdLine',loFormSet.DATASESSIONID)
loFormSet.PikTkt   = CREATEOBJECT("RemoteTable",'PikTkt','PikTkt','PikTkt',loFormSet.DATASESSIONID)
loFormSet.Customer = CREATEOBJECT("RemoteTable",'Customer','Customer','Customer',loFormSet.DATASESSIONID)
loFormSet.WareHous = CREATEOBJECT("RemoteTable",'WareHous','WareHous','WareHous',loFormSet.DATASESSIONID)
loFormSet.CUSTDEPT = CREATEOBJECT("RemoteTable",'CUSTDEPT','CUSTDEPT','CUSTDEPT',loFormSet.DATASESSIONID)
loFormSet.SPck_Hdr = CREATEOBJECT("RemoteTable",'SPck_Hdr','SPCK_HDRVR','SPck_Hdr',loFormSet.DATASESSIONID)
loFormSet.SPCK_LIN = CREATEOBJECT("RemoteTable",'SPCK_LIN','Spcklins','SPCK_LIN',loFormSet.DATASESSIONID)
loFormSet.PikLine  = CREATEOBJECT("RemoteTable",'PikLine','PikLine','PikLine',loFormSet.DATASESSIONID)
loFormSet.SCALE    = CREATEOBJECT("RemoteTable",'Scale','Scale','Scale',loFormSet.DATASESSIONID)
loFormSet.STYLE    = CREATEOBJECT("RemoteTable",'Style','Style','Style',loFormSet.DATASESSIONID)
loFormSet.StyDye   = CREATEOBJECT("RemoteTable",'StyDye','StyDye','StyDye',loFormSet.DATASESSIONID)

*-- Flag to know if UPC module is installed or not
loFormSet.llUPCInst  = ('UP' $ oAriaApplication.CompanyInstalledModules)
*-- Flag to know if the user want to print detailed shipping label for all cartons or not
IF loFormSet.llUPCInst
  loFormSet.STYLEUPC = CREATEOBJECT("RemoteTable",'STYLEUPC','STYLEUPC','STYLEUPC',loFormSet.DATASESSIONID)
ENDIF

IF loFormSet.llEdiSys
  loFormSet.EDIAcPrt = CREATEOBJECT("RemoteTable",'EDIAcPrt','ACCFACT','EDIAcPrt',loFormSet.DATASESSIONID)
  loFormSet.EDIPH    = CREATEOBJECT("RemoteTable",'EDIPH','PARTNER','EDIPH',loFormSet.DATASESSIONID)
  loFormSet.EDICRTSQ = CREATEOBJECT("RemoteTable",'EDICRTSQ','EDICRTSQ','EDICRTSQ',loFormSet.DATASESSIONID)
  loFormSet.BOL_HDR  = CREATEOBJECT("RemoteTable",'BOL_HDR','BOL_HDR','BOL_HDR',loFormSet.DATASESSIONID)
  loFormSet.BOL_LIN  = CREATEOBJECT("RemoteTable",'BOL_LIN','BOL_LIN','BOL_LIN',loFormSet.DATASESSIONID)
ENDIF

*!*************************************************************
*! Name      : lfChkfile
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 11/07/2004
*! Purpose   : To check the EOF
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfChkfile
LPARAMETERS loFormSet,lcChkfile
LOCAL lnAlias, lnRecNo,lcDataSes
lcDataSes = SET("Datasession")
SET DATASESSION TO loFormSet.DATASESSIONID

lnAlias = SELECT()
SELECT (lcChkfile)
lnRecNo = RECNO()
IF EOF()
  GO TOP
  lnRet = EOF()
ELSE
  lnRet = .F.
ENDIF
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF
SELECT(lnAlias)

SET DATASESSION TO &lcDataSes
RETURN lnRet

*!*************************************************************
*! Name      : lfActFolder
*! Developer : Hend Ghanem
*! Date      : 21/03/2005
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

STORE IIF((loFormSet.ActiveMode = 'E' OR loFormSet.llNew) AND lnActFolder = 1,.T.,.F.) TO llInstStat,llShipStat

=lfWinHdrSt(loFormSet)

DO CASE
CASE lnActFolder = 1
  loFormSet.AriaForm1.pgfpacking.HEADER.chkAssCarrID.ENABLED = IIF(loFormSet.ActiveMode = 'E' .OR. loFormSet.ActiveMode = 'A',.T.,.F.)
  *: B610016,1 SAB 07/16/2012 Enable Editing Notes and Special Inst. for Completed Packs [Start]
  IF loformset.activemode='E' .AND. pack_hdr.STATUS='C'
    loformset.ariaform1.pgfpacking.HEADER.chkasscarrid.ENABLED = .F.
  ENDIF
  *: B610016,1 SAB 07/16/2012 Enable Editing Notes and Special Inst. for Completed Packs [End]
CASE lnActFolder = 2
  =lfSelStatus(loFormSet)
  =lfDtlBrow(loFormSet)
  =lfwDtlBrow(loFormSet)

CASE lnActFolder = 3
  = lfWinCtnSt(loFormSet,lnActFolder)
  = lfwCtnHdrBr(loFormSet)
  = lfwCtnDtlBr(loFormSet)

ENDCASE

*!*************************************************************
*! Name      : lfWinCtnSt
*! Developer : Hend Ghanem
*! Date      : 21/03/2005
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
  IF lnActFolder = 3 AND (loFormSet.ActiveMode = 'E' OR loFormSet.llNew)
    STORE .T. TO .cmdNewCartH.ENABLED ,.cmdPrintCart.ENABLED

    IF EMPTY(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'))
      STORE .F. TO .cmdNewCartD.ENABLED,.cmdPrintCart.ENABLED
    ELSE
      STORE .T. TO .cmdNewCartD.ENABLED,.cmdPrintCart.ENABLED
    ENDIF
  ELSE
    STORE .F. TO .cmdNewCartH.ENABLED,.cmdNewCartD.ENABLED,.cmdPrintCart.ENABLED
  ENDIF
  *: B610016,1 SAB 07/16/2012 Enable Editing Notes and Special Inst. for Completed Packs [Start]
  IF loformset.activemode='E' .AND. pack_hdr.STATUS='C'
    STORE .F. TO .cmdnewcartd.ENABLED, .cmdprintcart.ENABLED, .cmdnewcarth.ENABLED
  ENDIF
  *: B610016,1 SAB 07/16/2012 Enable Editing Notes and Special Inst. for Completed Packs [End]
ENDWITH


*!*************************************************************
*! Name      : lfClrCntrS
*! Developer : Hend Ghanem
*! Date      : 21/03/2005
*! Purpose   : Clear control source for objects in the screen
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
FUNCTION lfClrCntrS
LPARAMETERS loFormSet

WITH loFormSet.AriaForm1.pgfPacking.HEADER
  *-- Header Information --*
  STORE "" TO .txtNoteH.CONTROLSOURCE,.cboShipViaH.CONTROLSOURCE,.txtBOL.CONTROLSOURCE,;
    .txtSpInst1.CONTROLSOURCE,.txtSpInst2.CONTROLSOURCE,.txtPackCharCode.CONTROLSOURCE,;
    .txtPackDescCode.CONTROLSOURCE
  STORE 1 TO loFormSet.lnCtnTyp , loFormSet.lnDrctTo
ENDWITH

SELECT (loFormSet.lcPckLin)
ZAP
SELECT (loFormSet.lcSumPck)
ZAP
= lfwDtlBrow(loFormSet)
SELECT (loFormSet.lcCtnHdr)
ZAP
= lfwCtnHdrBr(loFormSet)
SELECT (loFormSet.lcCtnDtl)
ZAP
=lfwCtnDtlBr(loFormSet)

*:**************************************************************************
*:* Name        : lfvCrCtnId
*:* Developer   : Hend Ghanem
*:* Date        : 11/07/2004
*:* Date        : 10/14/2003
*:* Purpose     : Valid function to Carrier Carton Id field
*:***************************************************************************
*:* Called from :
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvCrCtnId()
*:***************************************************************************
FUNCTION lfvCrCtnId
LPARAMETERS loFormSet

WITH loFormSet.AriaForm1.pgfpacking
  .cartonInfo.txtCarrierCartId.ENABLED = .HEADER.chkAssCarrID.VALUE
ENDWITH

*!*************************************************************
*! Name      : lfSelStatus
*! Developer : Hend Ghanem
*! Date      : 21/03/2005
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
  *: B610016,1 SAB 07/16/2012 Enable Editing Notes and Special Inst. for Completed Packs [Start]
  *llDtl2Stat = IIF((loFormSet.ActiveMode = 'E' OR loFormSet.llNew) AND loFormSet.ariaForm1.pgfPacking.ACTIVEPAGE = 2,.T.,.F.)
  lldtl2stat = IIF(((loformset.activemode='E' .AND. pack_hdr.STATUS<>'C') .OR. loformset.llnew) .AND. loformset.ariaform1.pgfpacking.ACTIVEPAGE=2, .T., .F.)
  *: B610016,1 SAB 07/16/2012 Enable Editing Notes and Special Inst. for Completed Packs [End]
ENDIF

WITH loFormSet.ariaForm1.pgfPacking.DETAIL
  STORE llDtl2Stat TO .txtCartFrom.ENABLED, .txtCartTo.ENABLED , .txtPaletteNo.ENABLED , .txtQtyPerCart.ENABLED ,;
    .txtUnitWeight.ENABLED  , .cmdStyle.ENABLED , .cmdSelect.ENABLED ,;
    .cmdInvert.ENABLED , .cmdSelAll.ENABLED , .cmdSelNone.ENABLED , .cmdApply.ENABLED,.cmdPack.ENABLED

  IF loFormSet.llPakPrSiz
    .cmdStyle.ENABLED = .F.
  ELSE
    .cmdStyle.ENABLED = llDtl2Stat
  ENDIF
ENDWITH
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvCtnRng
*! Developer : Hend Ghanem
*! Date      : 21/03/2005
*! Purpose   : validate lnFrom and lnTo fields which validate carton range
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfwDtlBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCtnRng()
*!*************************************************************

FUNCTION lfvCtnRng
LPARAMETERS loFormSet,lcObj
PRIVATE lnCurAlias,lcOldVal,lcCurTag,lnCurRec,lcOQtyFld,lnSizeRec,lnQty

lnCurAlias = SELECT(0)
llReturn = .T.
lcDetailFile = IIF(loFormSet.llShoPckSu,loFormSet.lcPckLin,loFormSet.lcSumPck)
SELECT(lcDetailFile)
lnCurRec = RECNO()

DO CASE
CASE lcObj = 'F'
  lcOldVal  = loFormSet.ariaForm1.pgfPacking.DETAIL.txtCartFrom.oldvalue
  lcValue   = loFormSet.lnFrom
CASE lcObj = 'T'
  lcOldVal  = loFormSet.ariaForm1.pgfPacking.DETAIL.txtCartTo.oldvalue
  lcValue   = loFormSet.lnTo
ENDCASE
IF lcValue < 0
  *-- Message : 42000
  *-- Negative values are not allowed.
  *-- Button  : 40011
  *--Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  DO CASE
  CASE lcObj = 'F'
    loFormSet.lnFrom = loFormSet.ariaForm1.pgfPacking.DETAIL.txtCartFrom.oldvalue
    loFormSet.ariaForm1.pgfPacking.DETAIL.txtCartFrom.REFRESH()
    SELECT(lcDetailFile)
    IF BETWEEN(lnCurRec,1,RECCOUNT())
      GOTO lnCurRec
    ENDIF
    llReturn = .F.
  CASE lcObj = 'T'
    loFormSet.lnTo = loFormSet.ariaForm1.pgfPacking.DETAIL.txtCartTo.oldvalue
    loFormSet.ariaForm1.pgfPacking.DETAIL.txtCartTo.REFRESH()
    SELECT(lcDetailFile)
    IF BETWEEN(lnCurRec,1,RECCOUNT())
      GOTO lnCurRec
    ENDIF
    llReturn = .F.
  ENDCASE
  RETURN .F.
ENDIF
lnQty = IIF(loFormSet.llShoPckSu,INT(EVALUATE(lcDetailFile+'.OQty')/(loFormSet.lnTo-loFormSet.lnFrom+1)),;
  INT(EVALUATE(lcDetailFile+'.OTotQty')/(loFormSet.lnTo-loFormSet.lnFrom+1)))
*E037427,1 WAM 05/05/2005 commented out
*!*	IF EVALUATE(lcDetailFile+'.lSelect') AND (lcOldVal <> lcValue) AND lnQty = 0
*!*	  *-- Message : 44130
*!*	  *-- Quantity is not divisible by number of cartons.
*!*	  *-- Button  : 00000
*!*	  *--Ok
*!*	  = gfModalGen('TRM44130B40011','DIALOG')
*!*	  DO CASE
*!*	    CASE lcObj = 'F'
*!*	      loFormSet.lnFrom = loFormSet.ariaForm1.pgfPacking.detail.txtCartFrom.oldvalue
*!*	      loFormSet.ariaForm1.pgfPacking.detail.txtCartFrom.refresh()
*!*	      SELECT(lcDetailFile)
*!*	      IF BETWEEN(lnCurRec,1,RECCOUNT())
*!*	        GOTO lnCurRec
*!*	      ENDIF
*!*	      llReturn = .F.
*!*	    CASE lcObj = 'T'
*!*	      loFormSet.lnTo = loFormSet.ariaForm1.pgfPacking.detail.txtCartTo.oldvalue
*!*	      loFormSet.ariaForm1.pgfPacking.detail.txtCartTo.refresh()
*!*	      SELECT(lcDetailFile)
*!*	      IF BETWEEN(lnCurRec,1,RECCOUNT())
*!*	        GOTO lnCurRec
*!*	      ENDIF
*!*	      llReturn = .F.
*!*	  ENDCASE
*!*	  RETURN .F.
*!*	ENDIF
*E037427,1 WAM 05/05/2005 (End)

IF loFormSet.lnFrom<=0 OR loFormSet.lnTo<=0 OR loFormSet.lnFrom > loFormSet.lnTo
  *-- You have to enter correct carton range.
  *-- <OK>
  = gfModalGen("INM44033B00000","Dialog")
  DO CASE
  CASE lcObj = 'F'
    loFormSet.lnFrom = loFormSet.ariaForm1.pgfPacking.DETAIL.txtCartFrom.oldvalue
    loFormSet.ariaForm1.pgfPacking.DETAIL.txtCartFrom.REFRESH()
    SELECT(lcDetailFile)
    IF BETWEEN(lnCurRec,1,RECCOUNT())
      GOTO lnCurRec
    ENDIF
    llReturn = .F.
  CASE lcObj = 'T'
    loFormSet.lnTo = loFormSet.ariaForm1.pgfPacking.DETAIL.txtCartTo.oldvalue
    loFormSet.ariaForm1.pgfPacking.DETAIL.txtCartTo.REFRESH()
    SELECT(lcDetailFile)
    IF BETWEEN(lnCurRec,1,RECCOUNT())
      GOTO lnCurRec
    ENDIF
    llReturn = .F.
  ENDCASE
ELSE
  IF (lcOldVal # lcValue) AND (loFormSet.lnTo - loFormSet.lnFrom + 1 >=0)
    SET RELATION TO
    lcCurTag = ORDER(lcDetailFile)
    SET ORDER TO SELECTED IN (lcDetailFile)
    lnCurRec = RECNO(lcDetailFile)
    IF SEEK ('Y',lcDetailFile)
      SELECT (lcDetailFile)
      *E037427,1 WAM 05/05/2005 Correct update of packed quantity
      *!*	      IF loFormSet.llShoPckSu
      *!*	        REPLACE REST CtnQty WITH IIF(lSelect,INT(OQty/(loFormSet.lnTo-loFormSet.lnFrom+1)),0) FOR Selected
      *!*	      ELSE
      *!*	        REPLACE REST CtnTotQty WITH IIF(lSelect,INT(OTotQty/(loFormSet.lnTo-loFormSet.lnFrom+1)),0) FOR lSelected
      *!*	      ENDIF
      IF loFormSet.llShoPckSu
        SCAN REST FOR SELECTED
          loFormSet.lnBrCtnQty =  IIF(lSelect,INT(OQty/(loFormSet.lnTo-loFormSet.lnFrom+1)),0)
          =lfvCtnQty(loFormSet)
        ENDSCAN
      ELSE
        SCAN REST FOR lSelected
          loFormSet.lnBrCtnQty =  IIF(lSelect,INT(OTotQty/(loFormSet.lnTo-loFormSet.lnFrom+1)),0)
          =lfvCtnQty(loFormSet)
        ENDSCAN
      ENDIF
      *E037427,1 WAM 05/05/2005 (End)
    ENDIF
    SET ORDER TO lcCurTag IN (lcDetailFile)
    IF lnCurRec<=RECCOUNT(lcDetailFile)
      GOTO lnCurRec
    ENDIF
  ENDIF
ENDIF

= RLOCK(lcDetailFile)
UNLOCK IN (lcDetailFile)
= lfwDtlBrow(loFormSet)

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfvCtnQty
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : Validate quantity per carton
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCtnQty()
*!*************************************************************
FUNCTION lfvCtnQty
LPARAMETERS loFormSet
PRIVATE lnCurAlias

*C200876,1 TMI [Start] do not change the qtys added when bin location is installed
IF ASCAN(loFormSet.laEvntTrig,PADR('CHNGQTY',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0 .AND. ;
    !loFormSet.mDoTrigger(PADR('CHNGQTY',10))
  RETURN
ENDIF
*C200876,1 TMI [End  ]

lnCurAlias = SELECT(0)
lcSumPck = loFormSet.lcSumPck
lcPckLin = loFormSet.lcPckLin
lcOldVal  = loFormSet.ariaForm1.pgfPacking.DETAIL.txtQtyPerCart.oldValue
SELECT (loFormSet.lcPckLin)
lcKeytoSeek = EVALUATE(KEY())
IF loFormSet.lnBrCtnQty < 0
  *-- Message : 42000
  *-- Negative values are not allowed.
  *-- Button  : 40011
  *--Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  loFormSet.lnBrCtnQty = lcOldVal
  loFormSet.ariaForm1.pgfPacking.DETAIL.txtQtyPerCart.REFRESH()
  =SEEK(lcKeytoSeek,loFormSet.lcPckLin)
  RETURN .F.
ENDIF

IF !loFormSet.llShoPckSu
  IF &lcSumPck..lSelect  AND loFormSet.lnBrCtnQty > 0
    IF &lcSumPck..llPack
      lnPackNo = EVAL(lcSumPck+'.OTotQty')
      IF loFormSet.lnBrCtnQty > lnPackNo
        *-- Message : 44127
        *-- 'Carton Qty can not be grater than ordered number of packs.'
        *-- Button  : 00000
        *--Ok
        =gfModalGen('TRM44127B00000','DIALOG')
        loFormSet.lnBrCtnQty = lcOldVal
        =SEEK(lcKeytoSeek,loFormSet.lcPckLin)
        RETURN .F.
      ENDIF
      IF MOD(loFormSet.lnBrCtnQty,1) <> 0
        *-- Message : 44128
        *-- 'Carton Qty not divisable by ordered number of packs.'
        *-- Button  : 00000
        *--Ok
        =gfModalGen('TRM44128B00000','DIALOG')
        loFormSet.lnBrCtnQty = lcOldVal
        =SEEK(lcKeytoSeek,loFormSet.lcPckLin)
        RETURN .F.
      ENDIF
      SELECT (lcSumPck)
      REPLACE CtnTotQty WITH loFormSet.lnBrCtnQty
      SELECT (lcPckLin)
      =SEEK(LEFT(EVAL(lcSumPck+'.Pack_ID'),16) +EVAL(lcSumPck+'.cPkColor') +EVAL(lcSumPck+'.cPckSize') +EVAL(lcSumPck+'.cPkVersion'))
      lcPackID = IIF(EVAL(lcSumPck+'.llPAck'),LEFT(EVAL(lcSumPck+'.Pack_ID'),16)+EVAL(lcSumPck+'.cPkColor') +EVAL(lcSumPck+'.cPckSize') +EVAL(lcSumPck+'.cPkVersion'),;
        EVAL(lcSumPck+'.Pack_ID'))
      lnCrtPck = loFormSet.lnBrCtnQty
      =lfApplyAll(loFormSet,lnCrtPck,lnPackNo,lcPackID)
    ELSE
      SELECT (lcSumPck)
      REPLACE CtnTotQty WITH loFormSet.lnBrCtnQty
      SELECT (lcPckLin)
      lcOldOrd = ORDER()
      SET ORDER TO &lcPckLin
      lcSz = EVAL(lcSumPck+'.cSze')
      =SEEK(LEFT(EVAL(lcSumPck+'.Pack_ID'),19)+EVAL(lcSumPck+'.Dyelot')+STR(EVAL(lcSumPck+'.nOrdLineNo'),6)+ALLTRIM(lcSz),lcPckLin)
      REPLACE CtnQty      WITH loFormSet.lnBrCtnQty,;
        CtnTotQty   WITH CtnQty,;
        lSelect     WITH .T.,;
        SELECTED    WITH .T.
      SET ORDER TO &lcOldOrd
    ENDIF
  ELSE
    loFormSet.lnBrCtnQty = lcOldVal
  ENDIF
ELSE
  IF EVAL(lcPckLin+'.lSelect') AND loFormSet.lnBrCtnQty >= 0
    SELECT (loFormSet.lcPckLin)
    REPLACE CtnQty WITH loFormSet.lnBrCtnQty
    = RLOCK(loFormSet.lcPckLin)
    UNLOCK IN (loFormSet.lcPckLin)
    *B610675,1 TMI 02/07/2014 20:59 [Start] update field CtnTotQty in cursor loFormSet.lcSumPck to be the same value of field CtnQty
    SELECT (lcSumPck)
    lcOldOrd = ORDER()
    SET ORDER TO lcSumPkLN
    lcSz = &lcPckLin..csizeno
    =SEEK(&lcPckLin..STYLE+&lcPckLin..Dyelot+STR(&lcPckLin..nOrdLineNo,6)+lcSz,lcSumPck)    
    REPLACE &lcSumPck..CtnTotQty WITH &lcPckLin..CtnQty
    SET ORDER TO &lcOldOrd
    *B610675,1 TMI 02/07/2014 20:59 [End  ] 
  ELSE
    loFormSet.lnBrCtnQty = lcOldVal
  ENDIF
ENDIF

loFormSet.ariaForm1.pgfPacking.DETAIL.txtQtyPerCart.REFRESH()

lcSkip = IIF(LASTKEY() = 5,"{UPARROW}",IIF(LASTKEY() = 24,"{DNARROW}",""))
lcKeyb = ""
SELECT(lnCurAlias)
CLEAR TYPEAHEAD
loFormSet.llQtyArr = .F.
IF !EMPTY(lcSkip)
  loFormSet.llQtyArr = .T.
  lcKeyb = ["{ALT+B}+]+lcSkip+'"'
  KEYBOARD &lcKeyB PLAIN CLEAR
ENDIF

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfvStyWgh
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : Validate unit weight per carton
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvStyWgh()
*!*************************************************************

FUNCTION lfvStyWgh
LPARAMETERS loFormSet
PRIVATE lnCurAlias,lnCurWgh,lcSelSty
lcOldVal   = loFormSet.ariaForm1.pgfPacking.DETAIL.txtUnitWeight.oldValue
SELECT (loFormSet.lcPckLin)
lcKeytoSeek = EVALUATE(KEY())
IF loFormSet.lnBrUntWgh  < 0
  *-- Message : 42000
  *-- Negative values are not allowed.
  *-- Button  : 40011
  *--Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  loFormSet.lnBrUntWgh  = lcOldVal
  loFormSet.ariaForm1.pgfPacking.DETAIL.txtUnitWeight.REFRESH()
  =SEEK(lcKeytoSeek,loFormSet.lcPckLin)
  RETURN .F.
ENDIF
lnCurAlias = SELECT(0)
lcSumPck = loFormSet.lcSumPck
lcPckLin = loFormSet.lcPckLin
IF loFormSet.llShoPckSu
  SELECT (lcPckLin)
ELSE
  SELECT (lcSumPck)
ENDIF
IF lSelect AND loFormSet.lnBrCtnQty > 0
  IF loFormSet.llShoPckSu
    SELECT (lcPckLin)
    REPLACE StyWgh WITH loFormSet.lnBrUntWgh
    SELECT (lcSumPck)
    SET ORDER TO lcSumPkLn
    =SEEK(EVAL(lcPckLin+'.Style') +EVAL(lcPckLin+'.Dyelot') +STR(EVAL(lcPckLin+'.nOrdLineNo'),6) +EVAL(lcPckLin+'.cSizeNo'))
    REPLACE StyWgh WITH loFormSet.lnBrUntWgh
    SELECT (lcPckLin)
  ELSE
    SELECT (lcSumPck)
    REPLACE StyWgh WITH loFormSet.lnBrUntWgh
    lcSeekExpr = IIF(!llPack,LEFT(EVAL(lcSumPck+'.Pack_ID'),16) +EVAL(lcSumPck+'.cPkColor') +EVAL(lcSumPck+'.cPckSize') +EVAL(lcSumPck+'.cPkVersion')+;
      STR(EVAL(lcSumPck+'.nOrdLineNo'),6)+EVAL(lcSumPck+'.Pack_ID')+EVAL(lcSumPck+'.Dyelot')+EVAL(lcSumPck+'.cSze'),;
      LEFT(EVAL(lcSumPck+'.Pack_ID'),16) +EVAL(lcSumPck+'.cPkColor') +EVAL(lcSumPck+'.cPckSize') +EVAL(lcSumPck+'.cPkVersion'))
    SELECT (lcPckLin)
    SET ORDER TO (loFormSet.lcPakIndxLn)
    =SEEK(lcSeekExpr)
    SCAN REST WHILE (PACK_ID+cPkColor+cPckSize+cPkVersion+STR(nOrdLineNo,6)+STYLE+Dyelot+cSizeNo = lcSeekExpr)
      REPLACE StyWgh WITH loFormSet.lnBrUntWgh
    ENDSCAN
    SELECT (lcSumPck)
  ENDIF
  = RLOCK(lcPckLin)
  UNLOCK IN (lcPckLin)
ELSE
  loFormSet.lnBrUntWgh = lcOldVal
ENDIF

loFormSet.ariaForm1.pgfPacking.DETAIL.txtUnitWeight.REFRESH()

lcSkip = IIF(LASTKEY() = 5,"{UPARROW}",IIF(LASTKEY() = 24,"{DNARROW}",""))
lcKeyb = ""
SELECT(lnCurAlias)
CLEAR TYPEAHEAD
loFormSet.llWghArr = .F.
IF !EMPTY(lcSkip)
  loFormSet.llWghArr = .T.
  lcKeyb = ["{ALT+B}+]+lcSkip+'"'
  KEYBOARD &lcKeyB PLAIN CLEAR
ENDIF

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfApplyAll
*! Developer : Hend Ghanem (HBG)
*! Date      : 23/03/2003
*! Purpose   :
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Called Form        :
*!*************************************************************
*! Example            : =lfApplyAll()
*!*************************************************************
FUNCTION lfApplyAll
LPARAMETERS loFormSet , lnNumPack , lnOrdPck , lcPackID

lcPckLin = loFormSet.lcPckLin
SCAN FOR Pack_ID + cPkColor + cPckSize + cPkVersion = lcPackID
  REPLACE CtnQty     WITH IIF(&lcPckLin..OQty <> 0,(&lcPckLin..OQty/lnOrdPck)*lnNumPack,0)
  REPLACE CtnTotQty  WITH CtnQty
  IF CtnQty <> 0

    *E037427,1 WAM 05/05/2005 The undo method clears order lock
    *lnPack   = OTotQty   / lnOrdPck
    *lnPkPack = CtnTotQty / lnPack
    lnPack   = IIF(lnOrdPck=0,0,OTotQty / lnOrdPck)
    lnPkPack = IIF(lnPack=0,0,CtnTotQty / lnPack)
    *E037427,1 WAM 05/05/2005 (End)
    REPLACE nPkPack WITH lnPkPack
  ENDIF
ENDSCAN

*!*************************************************************
*! Name      : lfShowDtSum
*! Developer : Hend Ghanem
*! Date      : 21/03/2005
*! Purpose   : Clear selection upon apply from option menu
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfClrSel
LPARAMETERS loFormSet

*-- this means if it is not clear selection upon apply invers this to be
*-- cleared upon apply and vice vresa is coorect
IF !loFormSet.llClrSel
  loFormSet.llClrSel = .T.
  SET MARK OF BAR 1 OF _OPTPOP TO .T.
ELSE
  loFormSet.llClrSel = .F.
  SET MARK OF BAR 1 OF _OPTPOP TO .F.
ENDIF

*!*************************************************************
*! Name      : lfPortScr
*! Developer : Hend Ghanem
*! Date      : 21/03/2005
*! Purpose   : Handle the printing  labels screen setup
*!*************************************************************
*! Example   : = lfPortScr
*!*************************************************************
FUNCTION lfPortScr
LPARAMETERS loFormSet

PRIVATE lcOldPort , llOldPrn , llCancel
lcOldPort = loFormSet.lcSndPort
llOldPrn  = loFormSet.llScrPrnLb
llCancel = .F.
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+ 'ALOUTPRT.SCX') WITH loFormSet TO llCancel
loCallingForm = loFormSet
=gfCallForm('ALOUTPRT',.F.,"loCallingForm","llCancel")
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
IF llCancel
  loFormSet.lcSndPort  = lcOldPort
  loFormSet.llScrPrnLb = llOldPrn
ENDIF
*-- end of lfPortScr.

*!*************************************************************
*! Name      : lfFltrScr
*! Developer : Hend Ghanem
*! Date      : 21/03/2005
*! Purpose   : to load the filter screen
*! Called by : the menu bar Style Filter
*!*************************************************************
*! Example   : = lfFltrScr ()
*!*************************************************************
FUNCTION lfFltrScr
LPARAMETERS loFormSet

SET DATASESSION TO loFormSet.DATASESSIONID
loFormSet.AriaForm1.pgfPacking.DETAIL.grdDetail.REFRESH()
loFormSet.STYLE.SetOrder('cStyle')
lcAccount = PADR(loFormSet.ariaForm1.kbAccount.keytextbox.VALUE ,5)
lcDetailFile = loFormSet.lcPckLin
SELECT (lcDetailFile)
lnRecNo = RECNO()
SCAN
  IF EMPTY(Pack_id)
    IF loFormSet.STYLE.SEEK(PADR(&lcDetailFile..STYLE,loFormSet.lnMajLen))
      SELECT STYLE
      SCATTER MEMVAR MEMO
      IF !SEEK(m.cStyMajor,loFormSet.lcTmpScpSty)
        INSERT INTO (loFormSet.lcTmpScpSty) FROM MEMVAR
      ENDIF
    ENDIF
  ELSE
    IF loFormSet.SPCK_HDR.SEEK('P'+lcAccount+&lcDetailFile..pack_id+&lcDetailFile..cpkcolor+&lcDetailFile..cpcksize+&lcDetailFile..cpkversion) OR ;
        loFormSet.SPCK_HDR.SEEK('P*****'+&lcDetailFile..pack_id+&lcDetailFile..cpkcolor+&lcDetailFile..cpcksize+&lcDetailFile..cpkversion)
      SELECT SPCK_HDR
      SCATTER MEMVAR MEMO
      m.cPSIZE = lfGetSize(m.cPckSize,loFormSet)
      m.cPDesc = m.Desc
      IF loFormSet.llUPCInst
        loFormSet.STYLEUPC.SetOrder('Stypkupc')
        = loFormSet.STYLEUPC.SEEK(lcAccount+&lcDetailFile..pack_id+"   "+&lcDetailFile..cpkcolor+&lcDetailFile..cpcksize+&lcDetailFile..cpkversion) OR;
          loFormSet.STYLEUPC.SEEK('*****'+&lcDetailFile..pack_id+"   "+&lcDetailFile..cpkcolor+&lcDetailFile..cpcksize+&lcDetailFile..cpkversion)
        m.CUPCNUM1 = STYLEUPC.Cupcnum1
        m.CUPCNUM2 = STYLEUPC.Cupcnum2
        m.CUPCNUM3 = STYLEUPC.Cupcnum3
      ELSE
        STORE "" TO m.CUPCNUM1,m.CUPCNUM2,m.CUPCNUM3
      ENDIF
      IF !SEEK(m.Pack_id+m.cPkColor+m.cPckSize+m.cPKVersion,loFormSet.lcTmpScpPck)
        INSERT INTO (loFormSet.lcTmpScpPck) FROM MEMVAR
      ENDIF
    ENDIF
  ENDIF
ENDSCAN
SELECT (lcDetailFile)
IF BETWEEN(lnRecNo ,1,RECCOUNT())
  GOTO lnRecNo
ENDIF
SELECT DISTINCT SUBSTR(STYLE,loFormSet.lnMajLen+2,loFormSet.lnColorLen) FROM (loFormSet.lcPckLin) WHERE EMPTY(PACK_ID) INTO ARRAY loFormSet.laClrSrc
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+ 'ALSTYFLTR.SCX') WITH loFormSet
loCallingForm = loFormSet
=gfCallForm('ALSTYFLTR',.F.,"loCallingForm")
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
=lfSetFlt(loFormSet)
*--END PROCEUDRE lfFltrScr

*!****************************************************************************
*! Name      : lfSetFlt
*! Developer : Hend Ghanem
*! Date      : 21/03/2005
*! Purpose   : Function to set the filter selected in filter screen
*!*****************************************************************************
*! Example   : = lfSetFlt()
*!*****************************************************************************
FUNCTION lfSetFlt
LPARAMETERS loFormSet

*E037427,1 WAM 05/05/2005 Set data session
SET DATASESSION TO loFormSet.DATASESSIONID
*E037427,1 WAM 05/05/2005 (End)

lcOAlias=ALIAS()
lcDetailFile = loFormSet.lcPckLin
loFormSet.llFilter = .F.
SELECT (loFormSet.lcSumPck)
SET FILTER TO
SELECT (lcDetailFile)
SET FILTER TO
SCAN
  IF EMPTY(&lcDetailFile..Pack_Id)
    DO CASE
    CASE loFormSet.llStyFltr AND loFormSet.llClrFltr
      IF SEEK(PADR(&lcDetailFile..STYLE,loFormSet.lnMajLen),loFormSet.lcScpStyFlt) AND;
          ASCAN(loFormSet.laClrTrgt,SUBSTR(&lcDetailFile..STYLE,loFormSet.lnMajLen+2,loFormSet.lnColorLen)) <> 0
        loFormSet.llFilter  = .T.
        REPLACE llFilter WITH .T.
      ELSE
        REPLACE llFilter WITH .F.
      ENDIF
    CASE loFormSet.llStyFltr AND !loFormSet.llClrFltr
      IF SEEK(PADR(&lcDetailFile..STYLE,loFormSet.lnMajLen),loFormSet.lcScpStyFlt)
        loFormSet.llFilter  = .T.
        REPLACE llFilter WITH .T.
      ELSE
        REPLACE llFilter WITH .F.
      ENDIF
    CASE !loFormSet.llStyFltr AND loFormSet.llClrFltr
      IF ASCAN(loFormSet.laClrTrgt,SUBSTR(&lcDetailFile..STYLE,loFormSet.lnMajLen+2,loFormSet.lnColorLen)) <> 0
        loFormSet.llFilter  = .T.
        REPLACE llFilter WITH .T.
      ELSE
        REPLACE llFilter WITH .F.
      ENDIF
    OTHERWISE
      REPLACE llFilter WITH .F.
    ENDCASE
    SET ORDER TO lcSumPkLn IN (loFormSet.lcSumPck)
    IF SEEK(&lcDetailFile..STYLE+&lcDetailFile..Dyelot+STR(&lcDetailFile..nOrdLineNo,6)+&lcDetailFile..cSizeNo,loFormSet.lcSumPck)
      SELECT (loFormSet.lcSumPck)
      REPLACE llFilter  WITH &lcDetailFile..llFilter
    ENDIF
  ELSE
    IF loFormSet.llPckFltr AND SEEK(&lcDetailFile..Pack_id+&lcDetailFile..cPkColor+&lcDetailFile..cPckSize+&lcDetailFile..cPkVersion,loFormSet.lcScpPckFlt)
      loFormSet.llFilter  = .T.
      REPLACE llFilter WITH .T.
    ELSE
      REPLACE llFilter WITH .F.
    ENDIF
    SET ORDER TO (loFormSet.lcSumPck) IN (loFormSet.lcSumPck)
    IF SEEK(PADR(&lcDetailFile..PACK_ID,19)+&lcDetailFile..cPkColor+&lcDetailFile..cPckSize+&lcDetailFile..cPkVersion,loFormSet.lcSumPck)
      SELECT (loFormSet.lcSumPck)
      REPLACE llFilter  WITH &lcDetailFile..llFilter
    ENDIF
  ENDIF

ENDSCAN

SELECT (lcOAlias)
=lfDtlBrow(loFormSet)

*!******************************************************************************
*! Name      : lfUpdtPkTk
*! Developer : Hend Ghanem
*! Date      : 21/03/2005
*! Purpose   : Valid function for the update piktkt menu bar.
*!*******************************************************************************
*! Example   : = lfUpdtPkTk()
*!*******************************************************************************
FUNCTION lfUpdtPkTk
LPARAMETERS loFormSet

IF !loFormSet.llUpdtPkTk
  loFormSet.llUpdtPkTk= .T.
  SET MARK OF BAR IIF(loFormSet.llCanPrnLb,6,4) OF _OPTPOP TO .T.
ELSE
  loFormSet.llUpdtPkTk= .F.
  SET MARK OF BAR IIF(loFormSet.llCanPrnLb,6,4) OF _OPTPOP TO .F.
ENDIF
*-- End of lfUpdtPkTk.

*!*************************************************************
*! Name      : lfvLblInfo
*! Developer : Hend Ghanem
*! Date      : 21/03/2005
*! Purpose   : Print carton labels
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Parameter : lnCarton : May be one of the following
*!           :         1- Carton No.
*!           :         2- Array or string of cartons
*!           :         3- Null for all cartons
*!*************************************************************
*! Example   : =lfvLblInfo(lnCarton)
*!*************************************************************
*!C101735,1
*!
FUNCTION lfvLblInfo
LPARAMETERS loFormSet , lnCarton, llPrint

PRIVATE lnCarton , lcUccVer , laAcShipTo , laDcShipTo , laWareAddr ,;
  lcStyle,lcColor,lcSize, llPrint , lcBol

DIMENSION laAcShipTo[5] , laDcShipTo[5] , laWareAddr[5]
STORE "" TO laAcShipTo , laDcShipTo , laWareAddr , lcStyle , lcColor , lcSize
*E302315,1 WLD 10/16/2006 Using visual label report [Begin]
lcCrtnSku = ""
lcCrtnUpc = ""
lcPkTkt = PADR(loFormSet.ariaForm1.kbPkTktNo.keytextbox.VALUE,6)
*E302315,1 WLD 10/16/2006 Using visual label report [End]
=lfGetStySz(loFormSet,lnCarton,@lcSize)

lcBOL   = loFormSet.ariaForm1.pgfPacking.HEADER.txtBOL.VALUE
lcOrder = PADR(loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE,6)
lcPack_id = PADR(loFormSet.ariaForm1.kbPackNo.keytextbox.VALUE,6)
lcAccount = PADR(loFormSet.ariaForm1.kbAccount.keytextbox.VALUE,5)
lcStore   = PADR(loFormSet.ariaForm1.kbstore.keytextbox.VALUE,8)

*E302315,1 WLD 10/16/2006 Using visual label report [Begin]
lcPack_id = IIF(EMPTY(lcPack_id),lcPkTkt,lcPack_id)
*E302315,1 WLD 10/16/2006 Using visual label report [End]

SELECT ORDHDR
loFormSet.ORDHDR.SetOrder('ORDHDR')
loFormSet.ORDHDR.SEEK('O'+lcOrder)
loFormSet.lcWareCode = ORDHDR.cWareCode

m.bol_no   = lcBol
m.Cart_No  = lnCarton

loFormSet.Warehous.SEEK(loFormSet.lcWareCode)
m.VND_NAME  = Warehous.cDesc
m.VND_ADDR1 = Warehous.cAddress1
m.VND_ADDR2 = Warehous.cAddress2
m.VND_CITY  = Warehous.cAddress3
m.VND_STATE = Warehous.cAddress4
m.VND_ZIP   = Warehous.cAddress5

*-- Get Customer information [Begin]
SELECT CUSTOMER
loFormSet.CUSTOMER.SEEK(IIF(EMPTY(lcStore),'M'+lcAccount,'S'+lcAccount+lcStore))
lcDistCtr = Customer.Dist_Ctr

STORE lcStore            TO m.cStStore,m.STORE
STORE Customer.StName    TO m.cStName ,m.SHP_NAME
STORE Customer.cAddress1 TO m.cStAddr1,m.SHP_ADDR1
STORE Customer.cAddress2 TO m.cStAddr2,m.SHP_ADDR2
STORE Customer.cAddress3 TO m.cStCity ,m.SHP_CITY
STORE Customer.cAddress4 TO m.cStState,m.SHP_STATE
STORE Customer.cAddress5 TO m.cStZip  ,m.SHP_ZIP
*-- Get Customer information [End  ]

*-- Get Distribution Center information [Begin]
loFormSet.CUSTOMER.SEEK('M'+lcAccount)
m.Int_Vend = Customer.cCusVend
IF loFormSet.CUSTOMER.SEEK('S'+lcAccount+lcDistCtr)
  m.STORE     = lcDistCtr
  m.SHP_NAME  = Customer.STName
  m.SHP_ADDR1 = Customer.cAddress1
  m.SHP_ADDR2 = Customer.cAddress2
  m.SHP_CITY  = Customer.cAddress3
  m.SHP_STATE = Customer.cAddress4
  m.SHP_ZIP   = Customer.cAddress5
ENDIF
*-- Get Distribution Center information [End]

m.ShipVia  = loFormSet.ariaForm1.pgfPacking.HEADER.cboShipViaH.VALUE
lcWrName   = loFormSet.lcWareCode

*-- Get Carton information [Begin]
lcOrStore   = OrdHdr.STORE

lcUccVer = ''
IF loFormSet.llEdiSys AND loFormSet.llEdiAcc
  lcUccVer = IIF(loFormSet.lnDrctTo=1,EdiPH.cASNLbl1,EdiPH.cASNLbl2)
ENDIF

lcUccVer    = IIF(EMPTY(lcUccVer),'XXX',lcUccVer)
m.CUSTPO    = OrdHdr.CustPo
m.DEPT      = OrdHdr.Dept

m.MANUF_ID  = PADL(ALLTRIM(loFormSet.lcManufId),7,'0')
*E302315,1 WLD 10/16/2006 Using visual label report -Add new setup of including P\L number in UCC [Begin]
*m.UCC9      = RIGHT(PADL(ALLTRIM(lcPack_id),6,'0'),5)+PADL(lnCarton,4,'0')
*m.UCC_CHECK = lfCheckNo('000'+m.MANUF_ID+m.Ucc9)
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
m.Ucc9 = ''
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
IF loFormSet.llEdiSys

  *! E302998,1 HES 27/11/2011 Change the way of getting the UCC9 while saving the P\L [BEGIN]
  *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
  IF loFormSet.EDICRTSQ.SEEK(lcPack_Id+STR(lnCarton,6),'PCKCRTSQ')
    *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
    m.Ucc9 = PADL(EDICRTSQ.Ucc9,9,'0')
  ELSE
    *Media,1 MMT 02/13/2012 Fix bugs reported by Media testing[Start]
    *= MESSAGEBOX('Packing list # '+ lcPack_Id + ", Carton # (" + ALLTRIM(STR(lnCarton,6)) + ") hasn't a record in EDICRTSQ file, please refer to Aria.",16,_SCREEN.CAPTION)
    =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Packing list # '+ lcPack_Id + ", Carton # (" + ALLTRIM(STR(lnCarton,6)) + ") hasn't a record in EDICRTSQ file, please refer to Aria.")
    *Media,1 MMT 02/13/2012 Fix bugs reported by Media testing[END]
    *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
    RETURN
    *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
  ENDIF
  *!*	    IF loFormSet.llIncPLNum
  *!*	      m.Ucc9  = RIGHT(PADL(ALLTRIM(lcPack_Id),6,'0'),loFormSet.lnNumOfDig)+PADL(lnCarton,IIF(loFormSet.lnNumOfDig = 5,4,3),'0')
  *!*	    ELSE
  *!*	      loFormSet.EDICRTSQ.SetOrder('PCKCRTSQ')
  *!*	      &&IF loFormSet.EDICRTSQ.SEEK(lcPack_id+STR(&lcCtnDtl..PackLineNo ,6))
  *!*	      IF loFormSet.EDICRTSQ.SEEK(lcPack_Id+STR(lnCarton,6))
  *!*	        m.Ucc9 = PADL(EDICRTSQ.Ucc9,9,'0')
  *!*	      ELSE
  *!*	        m.Ucc9 = '000000001'
  *!*	      ENDIF
  *!*	    ENDIF
  *! E302998,1 HES 27/11/2011 Change the way of getting the UCC9 while saving the P\L [END]

  m.UCC_CHECK = lfCheckNo('000'+m.MANUF_ID+m.Ucc9)
ENDIF
*E302315,1 WLD 10/16/2006 Using visual label report - Add new setup of including P\L number in UCC [End]
*-- These fields has to be filled
m.Note1     = OrdHdr.Note1
m.Note2     = OrdHdr.Note2
m.Int_Vend  = IIF(EMPTY(ORDHDR.INT_VEND),m.Int_Vend,ORDHDR.INT_VEND)
m.Cancelled = OrdHdr.COMPLETE
m.ASN_VER   = lcUccVer
m.EVENT_COD = OrdHdr.Event_Cod

IF (loFormSet.lcFree_Clr!="C") OR EMPTY(lcColor) OR ("MIXED" $ lcColor)
  DIMENSION laCodDesc[1,3]
  laCodDesc = ""
  laCodDesc[1,1] = m.ShipVia
  laCodDesc[1,2] = "SHIPVIA"
  m.cClrDesc = lcColor
ELSE
  DIMENSION laCodDesc[2,3]
  laCodDesc = ""
  laCodDesc[1,1] = m.ShipVia
  laCodDesc[2,1] = SUBSTR(EVALUATE(loFormSet.lcCtnDtl+'.Style'),loFormSet.lnNonMajSt,loFormSet.lnColorLen)
  laCodDesc[1,2] = "SHIPVIA"
  laCodDesc[2,2] = "COLOR"
  =gfCodDes(@laCodDesc)
  m.cClrDesc = laCodDesc[2,1]
ENDIF

=gfCodDes(@laCodDesc)
m.PACK_NO   = lcPack_id
m.Carrier   = laCodDesc[1,3]
m.Bol_No    = lcBol
m.Style     = lcStyle

=SEEK(STR(m.Cart_No,4),loFormSet.lcCtnHdr)
m.TotQty    = EVALUATE(loFormSet.lcCtnHdr+'.TotPcs')
m.cSizeDesc = lcSize

*-- Get No. of Cartons.
lcDetOrder = ORDER(loFormSet.lcCtnHdr)
USE (oAriaApplication.WorkDir+loFormSet.lcCtnHdr) ORDER (lcDetOrder) IN 0 AGAIN ALIAS (loFormSet.lcTmpDetFl)
GO BOTTOM IN (loFormSet.lcTmpDetFl)
m.Cartons = EVALUATE(loFormSet.lcTmpDetFl+'.Cart_No')
USE IN (loFormSet.lcTmpDetFl)
m.cpkversion = EVALUATE(loFormSet.lcCtnDtl+'.cpkversion')
m.cpkcolor   = EVALUATE(loFormSet.lcCtnDtl+'.cpkcolor')
m.cpcksize   = EVALUATE(loFormSet.lcCtnDtl+'.cpcksize')
m.pack_id    = EVALUATE(loFormSet.lcCtnDtl+'.pack_id ')
IF SEEK(STR(m.CART_NO,6),loFormSet.lcTmAsnShp)
  SELECT (loFormSet.lcTmAsnShp)
  GATHER MEMVAR MEMO
ELSE
  INSERT INTO (loFormSet.lcTmAsnShp) FROM MEMVAR
ENDIF
*E302315,1 WLD 10/16/2006 Using visual label report [Begin]
SELECT (loFormSet.lcTmAsnShp)
IF TYPE("loFormSet.laLblInfo[1,4]") = "C" .AND. !EMPTY(loFormSet.laLblInfo[1,4])
  =ACOPY(loFormSet.laLblInfo,laLblInfo)
  SAVE TO MEMO MLBLINFO ALL LIKE laLblInfo
ENDIF
*E302315,1 WLD 10/16/2006 Using visual label report [End]
IF llPrint
  *E302315,1 WLD 10/16/2006 Using visual label report [Begin]
  * =lfPrintLbl(loFormSet,lcUccVer)
  SELECT SycAsnHd
  GO TOP
  LOCATE FOR cver = lcUccVer AND cType = 'Y'
  IF FOUND()
    SELECT (loFormSet.lcTmAsnShp)
    =lfVsulLbl(loFormSet,lcUccVer,ALLTRIM(STR(m.Cart_No)))
  ELSE
    LOCATE FOR cver = lcUccVer AND cType = 'N'
    IF FOUND()
      *B608181,1 WLD 07/29/2007 Select the temporary file as (loFormSet.lcTmAsnShp) instead of (lcTmAsnShp) [Begin]
      *SELECT (lcTmAsnShp)
      SELECT (loFormSet.lcTmAsnShp)
      *B608181,1 WLD 07/29/2007 [End]
      =lfPrintLbl(loFormSet,lcUccVer)
    ENDIF
  ENDIF
  *E302315,1 WLD 10/16/2006 Using visual label report [End]
  IF loFormSet.llntFound
    RETURN
  ENDIF
ENDIF
SELECT (loFormSet.lcPckLin)
*-- End Of lfvLblInfo

*!*************************************************************
*! Name      : lfGetStySz
*! Developer : Hend Ghanem
*! Date      : 21/03/2005
*! Purpose   : Clculatre the style size.
*!*************************************************************
*! Example     : = lfGetStySz()
*!*************************************************************
FUNCTION lfGetStySz
LPARAMETERS loFormSet,lnCarton,lcSize

PRIVATE lcCurrAlis , lnRecPoint , lnDetPoint

*-- Save current setting
lcCurrAlis = SELECT (0)
*E302315,1 WLD 10/16/2006 Using visual label report [Begin]
lnLblInfo = 0
DIMENSION loFormSet.laLblInfo[1, 6]
lcAccount = PADR(loFormSet.ariaForm1.kbAccount.keytextbox.VALUE,5)
lcCrtnSku = ""
lcCrtnUpc = ""
*E302315,1 WLD 10/16/2006 Using visual label report [End]
SELECT (loFormSet.lcCtnDtl)

lnRecPoint = RECNO()
*E302315,1 WLD 10/16/2006 Using visual label report [Begin]
lcSetSkip = SET('SKIP')
SET SKIP TO
*E302315,1 WLD 10/16/2006 Using visual label report [End]

SEEK STR(lnCarton,4)

lcStyle  = SUBSTR(STYLE,1,loFormSet.lnMajLen)

IF loFormSet.lcFree_Clr="C"
  lcColor  = SUBSTR(STYLE,loFormSet.lnNonMajSt,loFormSet.lnColorLen)
ELSE
  lcColor  = SPACE(6)
ENDIF

lcUpdSty = STYLE
lcSize   = SIZE
SCAN REST WHILE STR(cart_no,4) = STR(lncarton,4)
  lcStyle = IIF(lcStyle<>SUBSTR(STYLE,1,loFormSet.lnMajLen),SPACE(loFormSet.lnMajLen),lcStyle)
  IF loFormSet.lcFree_Clr="C"
    lcColor = IIF(EMPTY(lcStyle),SPACE(loFormSet.lnColorLen),;
      IIF(lcColor<>SUBSTR(STYLE,loFormSet.lnNonMajSt,loFormSet.lnColorLen),'MIXED',lcColor))
  ENDIF
  *E302315,1 WLD 10/16/2006 Using visual label report [Begin]
  &&lcSize1 = ''
  &&IF !EMPTY(lcStyle)
  &&  lcSize1 = Size
  &&  lcSize  = IIF(lcSize <>lcSize1,'MIXED',lcSize)
  &&  IF lcSize = 'MIXED'
  &&    EXIT
  &&  ENDIF
  &&ELSE
  &&  lcSize = SPACE(5)
  &&ENDIF
  lcSizeNo = EVALUATE(loFormSet.lcCtnDtl+'.cSizeNo')
  FOR lnSzNo1 = 1 TO 8
    lcSzNo = STR(lnSzNo1,1)
    IF lcSizeNo = lcSzNo AND !EMPTY(QTY)
      lcSKU = SPACE(16)
      IF loFormSet.Spck_Lin.SEEK('S'+lcAccount + EVALUATE(loFormSet.lcCtnDtl+'.Style'))
        SELECT SPCK_LIN
        LOCATE REST ;
          WHILE TYPE + Account + STYLE = 'S' + lcAccount + EVALUATE(loFormSet.lcCtnDtl+'.Style') FOR Qty&lcSzNo = 1
        IF FOUND()
          lcSKU = PADR(Pack_Id, 16)
        ELSE
          loFormSet.Spck_Lin.SEEK('S'+lcAccount + EVALUATE(loFormSet.lcCtnDtl+'.Style'))
          LOCATE REST;
            WHILE TYPE + Account + STYLE = 'S' + lcAccount + EVALUATE(loFormSet.lcCtnDtl+'.Style') FOR TotQty = 0
          IF FOUND()
            lcSKU = PADR(Pack_Id, 16)
          ENDIF
        ENDIF
      ENDIF
      lcUpc = SPACE(13)
      IF loFormSet.llUPCInst AND  loFormSet.STYLEUPC.SEEK(EVALUATE(loFormSet.lcCtnDtl+'.Style')+lcSzNo)
        lcUpc = StyleUpc.cUpcNum1+StyleUpc.cUpcNum2+StyleUpc.cUpcNum3
      ENDIF
      SELECT (loFormSet.lcCtnDtl)
      lnLblInfo = lnLblInfo + 1
      DIMENSION loFormSet.laLblInfo[lnLblInfo, 6]
      loFormSet.laLblInfo[lnLblInfo,1] = lcSKU
      loFormSet.laLblInfo[lnLblInfo,2] = Qty
      loFormSet.laLblInfo[lnLblInfo,3] = SIZE
      loFormSet.laLblInfo[lnLblInfo,4] = STYLE
      loFormSet.laLblInfo[lnLblInfo,5] = lcUpc
      loFormSet.laLblInfo[lnLblInfo,6] = lnSzNo1

      lcSize    = IIF(SIZE <> lcSize   ,'MIXED',SIZE)
      lcCrtnSku = IIF(lcSKU <> lcCrtnSku ,SPACE(16),lcSKU)
      lcCrtnUpc = IIF(lcUpc <> lcCrtnUpc,SPACE(13),lcUpc)
    ENDIF
  ENDFOR
  *E302315,1 WLD 10/16/2006 Using visual label report [End]

ENDSCAN
*E302315,1 WLD 10/16/2006 Using visual label report [Begin]
lcSize = IIF(EMPTY(lcStyle),SPACE(5), lcSize)
IF !EMPTY(lcSetSkip)
  SET SKIP TO (lcSetSkip)
ENDIF
*E302315,1 WLD 10/16/2006 Using visual label report [End]
*-- Restore Setting.
IF BETWEEN(lnRecPoint,1,RECCOUNT())
  GO lnRecPoint
ENDIF

SELECT (lcCurrAlis)
*-- end of lfGetStySz.


*!*************************************************************
*! Name      : lfCheckNo
*! Developer : Hend Ghanem
*! Date      : 21/03/2005
*! Purpose   : Algorithm to Compute the modulus-10 Check digit for the
*!             UCC 128 code
*!*************************************************************
*! Parameters: lcUccNo : UCC number without check digit
*!*************************************************************
*! Returns   : UCC Check digit
*!*************************************************************
*! Example   :  =lfCheckNo('1234567890123456789')
*!*************************************************************
FUNCTION lfCheckNo
*E302315,1 WLD 10/16/2006 Using visual label report [Begin]
&&PARAMETER lcUccNo
&&PRIVATE lnChkDigit,lnSumOdd,lnSumEven,lnCount

&&STORE 0 TO lnSumOdd,lnSumEven,lnChkDigit
&&FOR lnCount = 1 TO 9
&&  lnSumOdd  = lnSumOdd + VAL(SUBSTR(lcUccNo,lnCount*2-1,1))
&&  lnSumEven = lnSumEven+ VAL(SUBSTR(lcUccNo,lnCount*2,1))
&&ENDFOR
&&lnSumOdd   = lnSumOdd + VAL(SUBSTR(lcUccNo,19,1))
&&lnChkDigit = MOD(lnSumOdd*3+lnSumEven,10)
&&RETURN(IIF(lnChkDigit=0,'0',STR(INT(10-lnChkDigit),1)))

LPARAMETER lcUccNo, lcType
lcType = IIF(TYPE('lcType')='C',lcType,'O')

LOCAL      lnChkDigit ,lnSumOdd  ,lnSumEven ,lnCount
STORE 0 TO lnChkDigit ,lnSumOdd  ,lnSumEven ,lnTop

IF TYPE('lcUccNo') = 'C'
  lnTop = LEN(lcUccNo)
ENDIF

FOR lnCount = 1 TO lnTop STEP 2
  lnSumOdd  = lnSumOdd  + VAL(SUBSTR(lcUccNo,lnCount     , 1))
  lnSumEven = lnSumEven + VAL(SUBSTR(lcUccNo,lnCount + 1 , 1))
ENDFOR
IF lcType = 'O'
  lnChkDigit = MOD(lnSumOdd*3 + lnSumEven , 10)
ELSE
  lnChkDigit = MOD(lnSumOdd + lnSumEven*3 , 10)
ENDIF
RETURN(IIF(lnChkDigit=0,'0',STR(INT(10-lnChkDigit),1)))
*E302315,1 WLD 10/16/2006 Using visual label report [End]

*-- end of lfCheckNo.

*!*************************************************************
*! Name      : lfPrintLbl
*! Developer : Hend Ghanem
*! Date      : 21/03/2005
*! Purpose   : Print carton labels
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :
*!*************************************************************
*!C101735,1
*!
FUNCTION lfPrintLbl
LPARAMETERS loFormSet , lcVersion
PRIVATE lnCurAlias, lnHandle, lcOutFile, lcString, lcData
lnCurAlias = SELECT(0)
WAIT LANG_ManulPL_WghtGenLbl WINDOW NOWAIT
lcString  = SPACE(40)
SELECT SYCASNLB
IF !loFormSet.SYCASNLB.SEEK(lcVersion+'H') .AND. gfModalGen('INM44068B00000','DIALOG' )=1
  loFormSet.llntfound = .T.
  RETURN
ENDIF
lcOutFile = oAriaApplication.WorkDir+loFormSet.lcAsnLabel+".TXT"
lnHandle  = FCREATE(lcOutFile,0)
=FSEEK(lnHandle,0,2)

SCAN WHILE 	cVer+cEdiType = lcVersion+'H'
  STORE DATA TO lcData
  lcString = &lcData
  =FPUTS(lnHandle,lcString)
ENDSCAN

lcString = SPACE(3)
=FPUTS(lnHandle,lcString)


SELECT (loFormSet.lcTmAsnShp)
SCATTER MEMVAR

MUCB = PADL(ALLTRIM(lfManufID(loFormSet,m.Bol_No)) , 7 , '0') + PADL(ALLTRIM(m.Bol_No) , 9 , '0')
MUCB = MUCB + lfCheckDgt(MUCB,'E')

DECLARE laDRltFld[1,2]
laDRltFld[1,1] = 'DIVLNAME  '
laDRltFld[1,2] = 'lcDivLName'
lcDivLName = ''
=gfRltFld(ORDHDR.cDivision,@laDRltFld,'CDIVISION ')

m.DivLName = lcDivLName
loFormSet.CUSTDEPT.SEEK(OrdHdr.Account+OrdHdr.Dept)
m.DeptDesc = CustDept.cDeptDesc
loFormSet.STYLE.SEEK(ALLTRIM(STYLE))
m.StyDesc = STYLE.DESC

=SEEK(STR(EVALUATE(loFormSet.lcTmAsnShp+'.cart_no'),4),loFormSet.lcCtnHdr)
m.Weight = EVALUATE(loFormSet.lcCtnHdr+'.TotWgh')


SELECT (loFormSet.lcTmAsnShp)
m.Date     = oAriaApplication.SystemDate

m.Order    = PADR(loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE,6)
m.Account  = PADR(loFormSet.ariaForm1.kbAccount.keytextbox.VALUE,5)
m.Pattern  = STYLE.PATTERN


STORE '' TO m.SizeDesc1,m.SizeDesc2,m.SizeDesc3,m.SizeDesc4,m.SizeDesc5,m.SizeDesc6,m.SizeDesc7,m.SizeDesc8
STORE '' TO m.SizeSku1,m.Sizesku2,m.Sizesku3,m.Sizesku4,m.Sizesku5,m.Sizesku6,m.Sizesku7,m.Sizesku8
STORE 0  TO m.SizeQty1,m.SizeQty2,m.SizeQty3,m.SizeQty4,m.SizeQty5,m.SizeQty6,m.SizeQty7,m.SizeQty8


SELECT(loFormSet.lcCtnDtl)
lnStyRec = RECNO(loFormSet.lcCtnDtl)
SET RELATION TO
IF loFormSet.llExtSizSc
  SELECT (loFormSet.lcCtnDtl)
  IF SEEK(STR(EVALUATE(loFormSet.lcTmAsnShp+'.cart_no'),4)+ALLTRIM(EVALUATE(loFormSet.lcTmAsnShp+'.Style')))
    lnSizeCount = 0
    lcCurrSty = SUBSTR(STYLE,1,loFormSet.lnSizePos-IIF(EMPTY(loFormSet.lcSizeSep),1,2))+Dyelot
    SCAN REST WHILE STR(Cart_No,4)+STYLE+Dyelot+STR(nOrdLineNo,6) = STR(EVALUATE(loFormSet.lcTmAsnShp+'.cart_no'),4)+lcCurrSty ;
        AND lnSizeCount < 8
      loFormSet.SCALE.SEEK('S'+SUBSTR(STYLE,loFormSet.lnSizePos,loFormSet.lnSizeLen))
      lcSizeNo = EVALUATE(loFormSet.lcCtnDtl+'.cSizeNo')
      IF EVALUATE(loFormSet.lcCtnDtl+'.Qty') <> 0
        lnSizeCount = lnSizeCount + 1
        lcSizeCount = STR(lnSizeCount,1)
        m.SizeDesc&lcSizeCount = ALLTRIM(SCALE.Sz&lcCount)
        m.SizeQty&lcSizeCount  = EVALUATE(loFormSet.lcCtnDtl+'.Qty')
        SELECT SPCK_LIN
        loFormSet.SPCK_LIN.SetOrder('Spklnstcn')
        loFormSet.SPCK_LIN.SEEK('S'+m.Account+EVALUATE(loFormSet.lcCtnDtl+'.style')+EVALUATE(loFormSet.lcCtnDtl+'.Dyelot'))
        LOCATE REST WHILE TYPE+account+STYLE+Dyelot+pack_id = 'S'+m.Account+EVALUATE(loFormSet.lcCtnDtl+'.style')+EVALUATE(loFormSet.lcCtnDtl+'.Dyelot') FOR QTY&lcSizeNo= 1
        IF FOUND()
          m.SizeSku&lcSizeCount = ALLTRIM(Spck_Lin.Pack_Id)
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
ELSE
  SELECT (loFormSet.lcTmAsnShp)
  loFormSet.SCALE.SEEK('S'+STYLE.SCALE)
  IF SEEK(STR(Cart_no,4)+ALLTRIM(STYLE),loFormSet.lcCtnDtl)
    lcCurrSty = ALLTRIM(EVALUATE(loFormSet.lcCtnDtl+'.Style'))+EVALUATE(loFormSet.lcCtnDtl+'.Dyelot')
    SELECT (loFormSet.lcCtnDtl)
    lcSizeNo = EVALUATE(loFormSet.lcCtnDtl+'.cSizeNo')
    lnSizeCount = VAL(lcSizeNo)
    SCAN REST WHILE STR(Cart_No,4)+STYLE+Dyelot+STR(nOrdLineNo,6) = STR(EVALUATE(loFormSet.lcTmAsnShp+'.cart_no'),4)+lcCurrSty ;
        AND lnSizeCount < 8
      lcSizeNo = EVALUATE(loFormSet.lcCtnDtl+'.cSizeNo')
      lnSizeCount = VAL(lcSizeNo)
      IF EVALUATE(loFormSet.lcCtnDtl+'.Qty') <> 0
        m.SizeDesc&lcSizeNo= ALLTRIM(SCALE.Sz&lcSizeNo)
        m.SizeQty&lcSizeNo= EVALUATE(loFormSet.lcCtnDtl+'.Qty')
        SELECT SPCK_LIN
        loFormSet.SPCK_LIN.SetOrder('Spklnstcn')
        loFormSet.SPCK_LIN.SEEK('S'+m.Account+EVALUATE(loFormSet.lcCtnDtl+'.style')+EVALUATE(loFormSet.lcCtnDtl+'.Dyelot'))
        LOCATE REST WHILE TYPE+account+STYLE+Dyelot+pack_id = 'S'+m.Account+EVALUATE(loFormSet.lcCtnDtl+'.style')+EVALUATE(loFormSet.lcCtnDtl+'.Dyelot') FOR QTY&lcSizeNo= 1
        IF FOUND()
          m.SizeSku&lcSizeNo = ALLTRIM(Spck_Lin.Pack_Id)
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
ENDIF
SELECT (loFormSet.lcTmAsnShp)

TMP_ADDR = ALLTRIM(VND_CITY) + ",  " +ALLTRIM(VND_STATE) + "   " + ALLTRIM(VND_ZIP)
SELECT SYCASNLB
loFormSet.SYCASNLB.SEEK(lcVersion+'L')
SCAN WHILE cVer+cEdiType= lcVersion+'L'
  STORE DATA TO lcData
  lcString = &lcData
  =FPUTS(lnHandle,lcString)
ENDSCAN
lcString = SPACE(3)
=FPUTS(lnHandle,lcString)


IF loFormSet.llEdiAcc
  *-- Check for the detail Version for current account.
  IF loFormSet.EDIACPRT.SEEK('A' + m.Account) .AND.;
      loFormSet.EDIPH.SEEK(EDIACPRT.cPartCode) .AND. ;
      Ediph.lDtlbl
    loFormSet.llDetLabel = .T.
    loFormSet.lcDetailVr = Ediph.cDtlbl
  ELSE
    loFormSet.llDetLabel = .F.
  ENDIF
ELSE
  *- Function to get the number of XX? into array.
  STORE '' TO loFormSet.lcDetailVr , loFormSet.lcDetLbAll
  DIMENSION laVersn[1]
  lnVerCount = lfGetVerXX(loFormSet)
  loFormSet.llDetLabel = (lnVerCount >= 1)
ENDIF
IF loFormSet.llDetLabel
  PRIVATE lnChoice , llPrintLbl , lnMajorLen , lnClrLen , lnClrPos , llUseColor ,;
    lcUPCStyle , lcGenColor , lcGenSty

  IF EMPTY(loFormSet.lcDetLbAll)

    lnChoice = gfModalGen('TRM44105B40016' , 'DIALOG' , ALLTRIM(STR(EVALUATE(loFormSet.lcTmAsnShp+'.Cart_No') , 4)))
    DO CASE
    CASE lnChoice = 1
      llPrintLbl = .T.
    CASE lnChoice = 2
      llPrintLbl = .T.
      loFormSet.lcDetLbAll = "Y"
    CASE lnChoice = 3
      llPrintLbl = .F.
    CASE lnChoice = 4
      llPrintLbl = .F.
      loFormSet.lcDetLbAll = "N"
    ENDCASE
  ELSE
    llPrintLbl = (loFormSet.lcDetLbAll = "Y")
  ENDIF

  IF llPrintLbl

    IF !loFormSet.llEdiAcc .AND. lnVerCount >= 1
      *-- the customer should print one from the current version.
      *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
      *= lfSeleVer(loFormSet,laVersn)
      = lfSeleVer(loFormSet,@laVersn)
      *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
    ENDIF

    STORE '' TO MDSTYLE1 , MDSTYLE2 , MDSTYLE3 , MDSTYLE4 , MDSTYLE5 ,;
      MDSTYLE6 , MDSTYLE7 , MDSTYLE8 , MDSTYLE9 , MDSTYLE10,;
      MDSTYLE11, MDSTYLE12, MDSTYLE13, MDSTYLE14, MDSTYLE15,;
      MDSTYLE16, MDSTYLE17, MDSTYLE18, MDSTYLE19, MDSTYLE20

    STORE '' TO MDSTYMAJ1 , MDSTYMAJ2 , MDSTYMAJ3 , MDSTYMAJ4 , MDSTYMAJ5 ,;
      MDSTYMAJ6 , MDSTYMAJ7 , MDSTYMAJ8 , MDSTYMAJ9 , MDSTYMAJ10,;
      MDSTYMAJ11, MDSTYMAJ12, MDSTYMAJ13, MDSTYMAJ14, MDSTYMAJ15,;
      MDSTYMAJ16, MDSTYMAJ17, MDSTYMAJ18, MDSTYMAJ19, MDSTYMAJ20

    STORE '' TO MDCOLOR1 , MDCOLOR2 , MDCOLOR3 , MDCOLOR4 , MDCOLOR5 ,;
      MDCOLOR6 , MDCOLOR7 , MDCOLOR8 , MDCOLOR9 , MDCOLOR10,;
      MDCOLOR11, MDCOLOR12, MDCOLOR13, MDCOLOR14, MDCOLOR15,;
      MDCOLOR16, MDCOLOR17, MDCOLOR18, MDCOLOR19, MDCOLOR20

    STORE '' TO MDSKU1 , MDSKU2 , MDSKU3 , MDSKU4 , MDSKU5 ,;
      MDSKU6 , MDSKU7 , MDSKU8 , MDSKU9 , MDSKU10,;
      MDSKU11, MDSKU12, MDSKU13, MDSKU14, MDSKU15,;
      MDSKU16, MDSKU17, MDSKU18, MDSKU19, MDSKU20

    STORE '' TO MDSTYUPC1 , MDSTYUPC2 , MDSTYUPC3 , MDSTYUPC4 , MDSTYUPC5 ,;
      MDSTYUPC6 , MDSTYUPC7 , MDSTYUPC8 , MDSTYUPC9 , MDSTYUPC10,;
      MDSTYUPC11, MDSTYUPC12, MDSTYUPC13, MDSTYUPC14, MDSTYUPC15,;
      MDSTYUPC16, MDSTYUPC17, MDSTYUPC18, MDSTYUPC19, MDSTYUPC20

    STORE '' TO MDSIZDES1 , MDSIZDES2 , MDSIZDES3 , MDSIZDES4 , MDSIZDES5 ,;
      MDSIZDES6 , MDSIZDES7 , MDSIZDES8 , MDSIZDES9 , MDSIZDES10,;
      MDSIZDES11, MDSIZDES12, MDSIZDES13, MDSIZDES14, MDSIZDES15,;
      MDSIZDES16, MDSIZDES17, MDSIZDES18, MDSIZDES19, MDSIZDES20

    STORE 0  TO MDQTY1 , MDQTY2 , MDQTY3 , MDQTY4 , MDQTY5 ,;
      MDQTY6 , MDQTY7 , MDQTY8 , MDQTY9 , MDQTY10,;
      MDQTY11, MDQTY12, MDQTY13, MDQTY14, MDQTY15,;
      MDQTY16, MDQTY17, MDQTY18, MDQTY19, MDQTY20

    *-- Get major length
    lnMajorLen = LEN(loFormSet.lcMask)

    *-- Get color start position and length
    STORE 0   TO lnClrLen , lnClrPos
    STORE .F. TO llUseColor
    DECLARE laItemSeg[1]
    =gfItemMask(@laItemSeg)
    FOR lnCount = 1 TO ALEN(laItemSeg , 1)
      IF laItemSeg[lnCount,1] = 'C'
        llUseColor = .T.
        lnClrLen   = LEN(laItemSeg[lnCount,3])
        lnClrPos   = laItemSeg[lnCount,4]
        EXIT
      ENDIF
    ENDFOR

    lcGenColor = PADR(gfGetMemVar("MCLRASSCOD",oAriaApplication.ActiveCompanyID) , 6)

    SELECT (loFormSet.lcCtnDtl)
    IF SEEK(STR(EVALUATE(loFormSet.lcTmAsnShp+'.Cart_No') , 4))
      lnSizeCount = 0
      SCAN REST WHILE STR(Cart_No , 4) = STR(EVALUATE(loFormSet.lcTmAsnShp+'.Cart_No') , 4) .AND. lnSizeCount < 20
        loFormSet.STYLE.SEEK(EVALUATE(loFormSet.lcCtnDtl+'.Style'))
        loFormSet.SCALE.SEEK('S' + STYLE.SCALE)

        IF loFormSet.llUPCInst
          lcUPCStyle = EVALUATE(loFormSet.lcCtnDtl+'.Style')
          *C102275,1 HS  Call a trigger to get the UPC style
          IF ASCAN(loFormSet.laEvntTrig,PADR('GETUPCST',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
            =loFormSet.mDoTrigger(PADR('GETUPCST',10))
          ENDIF
          IF llUseColor
            lcGenSty = SUBSTR(lcUPCStyle , 1 , lnClrPos - 1) + lcGenColor +;
              SUBSTR(lcUPCStyle , lnClrPos + lnCLrLen)
          ENDIF
        ENDIF

        lcCount = EVALUATE(loFormSet.lcCtnDtl+'.cSizeNo')
        IF lnSizeCount < 20 .AND. EVALUATE(loFormSet.lcCtnDtl+'.Qty') <> 0
          lnSizeCount = lnSizeCount + 1
          lcSizeCount = ALLTRIM(STR(lnSizeCount,2))

          MDStyle&lcSizeCount  = EVALUATE(loFormSet.lcCtnDtl+'.Style')
          MDStyMaj&lcSizeCount = LEFT(EVALUATE(loFormSet.lcCtnDtl+'.Style'), lnMajorLen)
          MDSizDes&lcSizeCount = ALLTRIM(SCALE.Sz&lcCount)
          MDQty&lcSizeCount    = EVALUATE(loFormSet.lcCtnDtl+'.Qty')

          IF llUseColor
            MDColor&lcSizeCount = SUBSTR( EVALUATE(loFormSet.lcCtnDtl+'.Style') , lnClrPos , lnClrLen)
          ENDIF

          IF loFormSet.llUPCInst
            loFormSet.STYLEUPC.SetOrder('STYLEUPC')
            IF loFormSet.StyleUPC.SEEK(lcUPCStyle + lcCount )
              MDStyUPC&lcSizeCount = StyleUPC.cUPCNum1 + StyleUPC.cUPCNum2 + StyleUPC.cUPCNum3
            ELSE
              IF llUseColor .AND. loFormSet.StyleUPC.SEEK(lcGenSty + lcCount)
                MDStyUPC&lcSizeCount = StyleUPC.cUPCNum1 + StyleUPC.cUPCNum2 +;
                  StyleUPC.cUPCNum3
              ENDIF
            ENDIF
          ENDIF

          SELECT SPCK_LIN
          loFormSet.SPCK_LIN.SetOrder('Spklnstcn')
          loFormSet.SPCK_LIN.SEEK('S' + m.Account + EVALUATE(loFormSet.lcCtnDtl+'.Style')+EVALUATE(loFormSet.lcCtnDtl+'.Dyelot'))
          LOCATE REST;
            WHILE TYPE + Account + STYLE + Pack_ID = 'S' + m.Account + EVALUATE(loFormSet.lcCtnDtl+'.Style')+EVALUATE(loFormSet.lcCtnDtl+'.Dyelot');
            FOR QTY&lcCount = 1

          IF FOUND()
            MDSKU&lcSizeCount = ALLTRIM(Spck_Lin.Pack_Id)
          ENDIF
        ENDIF
      ENDSCAN
    ENDIF

    SELECT SYCASNLB
    *--- collect data for the selected Version.
    loFormSet.SYCASNLB.SEEK(loFormSet.lcDetailVr + 'H')
    SCAN WHILE cVer + cEdiType = loFormSet.lcDetailVr + 'H'
      STORE DATA TO lcData
      lcString = &lcData
      =FPUTS(lnHandle,lcString)
    ENDSCAN
    lcString = SPACE(3)
    =FPUTS(lnHandle,lcString)

    loFormSet.SYCASNLB.SEEK(loFormSet.lcDetailVr  + 'L')
    SCAN WHILE cVer + cEdiType = loFormSet.lcDetailVr + 'L'
      STORE DATA TO lcData
      lcString = &lcData
      =FPUTS(lnHandle,lcString)
    ENDSCAN
  ENDIF
ENDIF

=FCLOSE(lnHandle)
WAIT LANG_ManulPL_ShpGenLbl WINDOW NOWAIT
lcCommand = "TYPE " + lcOutFile + " > " + loFormSet.lcSndPort
*E302315 WLD Print Visual lables from Packing List 10/10/2006 [Begin]
&& *!/N0 &lcCommand
*B608181,1 WLD 07/29/2007 Print at DOS [Begin]
*!/N0 &lcCommand
! &lcCommand
*B608181,1 WLD 07/29/2007 [End]

*E302315 WLD Print Visual lables from Packing List 10/10/2006 [End]

WAIT CLEAR

SELECT (loFormSet.lcCtnDtl)
IF lnStyRec <= RECCOUNT(loFormSet.lcCtnDtl)
  GOTO lnStyRec
ENDIF
= RLOCK(loFormSet.lcCtnDtl)
UNLOCK IN (loFormSet.lcCtnDtl)

SELECT(lnCurAlias)
RETURN
*-- End Of Function lfPrintLbl


*!*************************************************************
*! Name      : lfGetVerXX
*! Developer : Hend Ghanem
*! Date      : 21/03/2005
*! Purpose   : Function to get the number of XX? into array.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  =lfGetVerXX()
*!*************************************************************
FUNCTION lfGetVerXX
LPARAMETERS loFormSet

PRIVATE lnPRvAlias , lnReturnVr
lnReturnVr = 0
lnPRvAlias = SELECT(0)

SELECT SYCASNLB
SELECT DIST cVer FROM sycasnlb ;
  WHERE LEFT(Cver,2) = 'XX' .AND. Cver # 'XXX';
  INTO ARRAY laVersn

lnReturnVr = ALEN(laVersn,1)
*-- if the customer have one version it should print it direct.
IF lnReturnVr >= 1
  loFormSet.lcDetailVr = laVersn[1]
ENDIF

SELECT(lnPRvAlias)

RETURN lnReturnVr

*-- End Of lfGetVerXX

*!*************************************************************
*! Name      : lfSeleVer
*! Developer : Hend Ghanem
*! Date      : 21/03/2005
*! Purpose   : Function to get the Version types
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*! Example   :  =lfSeleVer()
*!*************************************************************
FUNCTION lfSeleVer
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*LPARAMETERS loFormSet,laVersn
PARAMETERS loFormSet,laVersn
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
PRIVATE lnPRvAlias

lnPRvAlias = SELECT(0)
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'ALDtVer.SCX') WITH loFormSet,laVersn
=gfCallForm('ALDtVer',.F.,"loFormSet,laVersn")
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
SELECT(lnPRvAlias)
*-- End OF lfSeleVer

*!*************************************************************
*! Name      : lfManufID
*! Developer : Hend Ghanem
*! Date      : 21/03/2005
*! Purpose   : validate pbDtlNew button
*!*************************************************************
*! Example   : = lfManufID(Bill of lading no)
*!*************************************************************
FUNCTION lfManufID
LPARAMETERS loFormSet,lcBolNo
PRIVATE lcManuf_Id,laRltdFld, MUCCLEVEL

lcManuf_Id = loFormSet.lcManufId
MUCCLEVEL  = gfGetMemVar('M_UCCDIV',oAriaApplication.ActiveCompanyID)

*-- Maintain UCC manufaturer ID at division level
IF MUCCLEVEL = 'N'
  DECLARE laRltdFld[1,2]
  STORE '' TO laRltdFld,LCUPCMAN
  laRltdFld[1,1] = "CUPCMAN"
  laRltdFld[1,2] = 'LCUPCMAN'
  =gfRltFld(ORDHDR.cDivision,@laRltdFld,'CDIVISION')
  lcManuf_Id = IIF(EMPTY(LCUPCMAN),lcManuf_Id,LCUPCMAN)
ENDIF
RETURN ALLTRIM(lcManuf_Id)
*-- End of lfManufID.

*:**************************************************************************
*:* Name        : lfCheckDgt
*! Developer    : Hend Ghanem
*! Date         : 21/03/2005
*:* Purpose     :
*:***************************************************************************
*:* Called from :
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCheckDgt()
*:***************************************************************************
FUNCTION lfCheckDgt
PARAMETER lcUccNo, lcType
PRIVATE lnChkDigit ,lnSumOdd  ,lnSumEven ,lnCount
STORE 0 TO lnChkDigit ,lnSumOdd  ,lnSumEven ,lnTop

lnTop = LEN(lcUccNo)
FOR lnCount = 1 TO lnTop STEP 2
  lnSumOdd  = lnSumOdd  + VAL(SUBSTR(lcUccNo,lnCount     , 1))
  lnSumEven = lnSumEven + VAL(SUBSTR(lcUccNo,lnCount + 1 , 1))
ENDFOR
IF lcType = 'O'
  lnChkDigit = MOD(lnSumOdd*3 + lnSumEven , 10)
ELSE
  lnChkDigit = MOD(lnSumOdd + lnSumEven*3 , 10)
ENDIF
RETURN(IIF(lnChkDigit=0,'0',STR(INT(10-lnChkDigit),1)))


*!*************************************************************
*! Name      : lfvCtHdWgh
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : validate Carton total weight
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCtHdWgh()
*!*************************************************************
FUNCTION lfvCtHdWgh
LPARAMETERS loFormSet

PRIVATE lnCurAlias,lnChoice,lcOldVal
STORE 0 TO lnChoice


lnCurAlias = SELECT(0)
lnTotWgh = loFormSet.AriaForm1.pgfPacking.CartonInfo.txtTotWghH.VALUE
lcOldVal = loFormSet.AriaForm1.pgfPacking.CartonInfo.txtTotWghH.OldValue

SELECT (loFormSet.lcCtnHdr)
lcKeytoSeek = EVALUATE(KEY())

IF lnTotWgh < 0
  *-- Message : 42000
  *-- Negative values are not allowed.
  *-- Button  : 40011
  *--Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  loFormSet.AriaForm1.pgfPacking.CartonInfo.txtTotWghH.VALUE = lcOldVal
  =SEEK(lcKeytoSeek,loFormSet.lcCtnHdr)
  llReturn = .F.
ENDIF

IF !(lnTotWgh == lcOldVal)
  *-- This is to enforce equal contribution if the total weight was zero
  IF lcOldVal = 0
    lnChoice = 2
  ELSE
    SELECT(loFormSet.lcCtnDtl)
    lnStyRec = RECNO(loFormSet.lcCtnDtl)
    SCAN
      IF (Br AND Weight=0)
        *-- One or more sizes has zeros weight.
        *-- Contributing the total carton weight may
        *-- either ignore the zero weight sizes,
        *-- or contribute equal weight to all sizes
        *-- <Ignore zero sizes>,<Equal contibution>,<Cancel>
        lnChoice = gfModalGen("INM44049B44007","Dialog")
        EXIT
      ENDIF
    ENDSCAN
    IF lnStyRec <= RECCOUNT(loFormSet.lcCtnDtl)
      GOTO lnStyRec
    ENDIF
  ENDIF
  IF !(lnChoice=3)
    SELECT(loFormSet.lcPckLin)
    SET FILTER TO
    SET RELATION TO
    SET RELATION TO STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4)+STYLE+STR(nOrdLineNo,6) INTO (loFormSet.lcCtnDtl)
    lnStyRec = RECNO(loFormSet.lcPckLin)
    REPLACE ALL FOR STYLE+Dyelot+STR(nOrdLineNo,6)=EVALUATE(loFormSet.lcCtnDtl+'.Style')+EVALUATE(loFormSet.lcCtnDtl+'.Dyelot')+STR(nOrdLineNo,6);
      PWgh WITH IIF(EVALUATE(loFormSet.lcCtnDtl+'.Br'),lfPckLinUp(loFormSet,lnChoice),PWgh)
    SET RELATION TO
    IF lnStyRec <= RECCOUNT(loFormSet.lcPckLin)
      GOTO lnStyRec
    ENDIF
    = RLOCK(loFormSet.lcPckLin)
    UNLOCK IN (loFormSet.lcPckLin)

    SELECT(loFormSet.lcSumPck)
    SET FILTER TO
    lnStyRec = RECNO(loFormSet.lcSumPck)
    REPLACE ALL FOR PACK_ID+Dyelot+STR(nOrdLineNo,6)=EVALUATE(loFormSet.lcCtnDtl+'.Style')+EVALUATE(loFormSet.lcCtnDtl+'.Dyelot')+STR(nOrdLineNo,6);
      PTotWgh WITH IIF(EVALUATE(loFormSet.lcCtnDtl+'.Br'),lfPckLinUp(loFormSet,lnChoice),PWgh)
    SET RELATION TO
    IF lnStyRec <= RECCOUNT(loFormSet.lcSumPck)
      GOTO lnStyRec
    ENDIF

    SELECT(loFormSet.lcCtnDtl)
    lnStyRec = RECNO(loFormSet.lcCtnDtl)
    SET RELATION TO
    = SEEK(STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4),loFormSet.lcCtnDtl)
    REPLACE REST FOR STR(Cart_No,4)+STYLE+Dyelot+STR(nOrdLineNo,6) = STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4) ;
      Weight WITH IIF(Br,lfCtnDtlUp(loFormSet,lnChoice),Weight),;
      cStatus WITH 'M'

    IF lnStyRec <= RECCOUNT(loFormSet.lcCtnDtl)
      GOTO lnStyRec
    ENDIF
    = RLOCK(loFormSet.lcCtnDtl)
    UNLOCK IN (loFormSet.lcCtnDtl)
  ENDIF
  loFormSet.lnPackWgh = (loFormSet.lnPackWgh - lcOldVal) + lnTotWgh
  loFormSet.AriaForm1.pgfPacking.HEADER.txtWeight.VALUE  = loFormSet.lnPackWgh

  SELECT (loFormSet.lcCtnHdr)
  REPLACE TotWgh WITH lnTotWgh

  = lfwCtnHdrBr(loFormSet)
ELSE
  loFormSet.AriaForm1.pgfPacking.CartonInfo.txtTotWghH.VALUE = lcOldVal
ENDIF

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfPckLinUp
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : Update the weight in file lcPckLin according to
*!             update the total carton weight field
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfPckLinUp()
*!*************************************************************

FUNCTION lfPckLinUp
PARAMETERS loFormSet,lnChoice
PRIVATE lcSize,lnReturn,lnTotWgh ,lcOldVal


lcCtnHdr  = loFormSet.lcCtnHdr
lcCtnDtl  = loFormSet.lcCtnDtl
lcPWghFld = 'PWgh'
lcWghFld  = lcCtnDtl+'.Weight'
lcQtyFld  = lcCtnDtl+'.Qty'
lnTotWgh = loFormSet.AriaForm1.pgfPacking.CartonInfo.txtTotWghH.VALUE
lcOldVal = loFormSet.AriaForm1.pgfPacking.CartonInfo.txtTotWghH.OldValue

lnReturn = (&lcPWghFld-&lcWghFld+IIF(lnChoice=2,;
  (&lcQtyFld/&lcCtnHdr..TotPcs)*lnTotWgh,;
  (lnTotWgh/lcOldVal)*&lcWghFld))

RETURN lnReturn

*!*************************************************************
*! Name      : lfCtnDtlUp
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : Update the weight in lcCtnDtl file according to
*!             update the total carton weight field
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCtnDtlUp()
*!*************************************************************

FUNCTION lfCtnDtlUp
PARAMETERS loFormSet,lnChoice
PRIVATE lcSize,lnReturn,lnTotWgh ,lcOldVal

lcCtnHdr  = loFormSet.lcCtnHdr
lcWghFld  = 'Weight'
lcQtyFld  = 'Qty'
lnTotWgh = loFormSet.AriaForm1.pgfPacking.CartonInfo.txtTotWghH.VALUE
lcOldVal = loFormSet.AriaForm1.pgfPacking.CartonInfo.txtTotWghH.OldValue


lnReturn = (IIF(lnChoice=2,(&lcQtyFld/&lcCtnHdr..TotPcs)*lnTotWgh,(lnTotWgh/lcOldVal)*&lcWghFld))

RETURN lnReturn


*!*************************************************************
*! Name      : lfvPalNo
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : Validate palette No for carton header edit rigion.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvPalNo()
*!*************************************************************
FUNCTION lfvPalNo
LPARAMETERS loFormSet

PRIVATE lnCurAlias , lcOldVal
lnPalNo  = loFormSet.ariAFORM1.pgfPacking.CartonInfo.txtPaletteNoH.VALUE
lcOldVal = loFormSet.ariAFORM1.pgfPacking.CartonInfo.txtPaletteNoH.OldValue
lnCurAlias = SELECT(0)

IF lnPalNo < loFormSet.lnMinPal OR (lnPalNo - IIF(loFormSet.lnMaxPal=0,lnPalNo,loFormSet.lnMaxPal)) > 1
  *-- palette no has to be sequential.
  *-- OK
  = gfModalGen("INM44048B00000","Dialog")
  loFormSet.ariAFORM1.pgfPacking.CartonInfo.txtPaletteNoH.VALUE = lcOldVal
ELSE
  SELECT(loFormSet.lcCtnHdr)
  REPLACE Pal_No WITH lnPalNo
  = RLOCK(loFormSet.lcCtnHdr)
  UNLOCK IN (loFormSet.lcCtnHdr)
  loFormSet.lnMaxPal = MAX(loFormSet.lnMaxPal,EVALUATE(loFormSet.lcCtnHdr+'.Pal_No'))
  loFormSet.lnMinPal = MIN(IIF(loFormSet.lnMinPal=0,EVALUATE(loFormSet.lcCtnHdr+'.Pal_No'),loFormSet.lnMinPal),EVALUATE(loFormSet.lcCtnHdr+'.Pal_No'))
ENDIF

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfvCrtType
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : Validate Carton type for carton header edit rigion.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCrtType()
*!*************************************************************
FUNCTION lfvCrtType
LPARAMETERS loFormSet

PRIVATE lcOldVal , lcCrtType
lcOldVal = loFormSet.AriaForm1.pgfPacking.CartonInfo.cboCrtType.OldValue
lcCrtType = loFormSet.AriaForm1.pgfPacking.CartonInfo.cboCrtType.VALUE

IF lcOldVal <> lcCrtType
  *! B610287,1 HIA 04/02/2013 Aria XP - Manual Packing List - Assign default, T20130226.0030[Start]
  *SELECT(loFormSet.lcCtnHdr)
  *REPLACE CCRTNVLTYP WITH lcCrtType
  SELECT(loFormSet.lcCtnHdr)
  
  IF gfModalGen('TRM44150B44009','DIALOG',LANG_ManulPL_MsgModInst)  = 1
    lnrecnoCrt = RECNO()
    REPLACE CCRTNVLTYP WITH lcCrtType ALL 
    GO (lnrecnoCrt)
  ELSE 
    REPLACE CCRTNVLTYP WITH lcCrtType 
  ENDIF 
  *! B610287,1 HIA 04/02/2013 Aria XP - Manual Packing List - Assign default, T20130226.0030[End]
ENDIF


*!*************************************************************
*! Name      : lfvCtHdNew
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : validate pbHdrNew button
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCtHdNew()
*!*************************************************************

FUNCTION lfvCtHdNew
LPARAMETERS loFormSet

PRIVATE lnCurAlias,lcCtnHdr,lcCtnDtl


lcCtnHdr  = loFormSet.lcCtnHdr
lcCtnDtl  = loFormSet.lcCtnDtl

STORE .T. TO loFormSet.llCupdate,loFormSet.llAnyUpd

lnCurAlias = SELECT(0)



*! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[Start]
IF loFormSet.llUPCInst AND loFormSet.llPrntLblAftrPack AND loFormSet.lnPackCtn > 0
  =lfvLblInfo(loFormSet,loFormSet.lnMaxCtn,.T.)
ENDIF
*! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[End]


SELECT (lcCtnHdr)
APPEND BLANK
loFormSet.lnPackCtn = loFormSet.lnPackCtn + 1
loFormSet.lnMaxCtn  = loFormSet.lnMaxCtn  + 1

loFormSet.lnFrom = loFormSet.lnMaxCtn + 1
loFormSet.lnTo   = loFormSet.lnMaxCtn + 1

REPLACE Cart_No WITH loFormSet.lnMaxCtn,;
  EMPTY   WITH 'Y'
= RLOCK(lcCtnHdr)
UNLOCK IN (lcCtnHdr)
loFormSet.AriaForm1.pgfPacking.HEADER.txtCartons.VALUE = loFormSet.lnPackCtn
loFormSet.AriaForm1.pgfPacking.CartonInfo.cmdNewCartD.ENABLED = .T.
*C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [Begin]
loFormSet.AriaForm1.pgfPacking.CartonInfo.cmdScan.ENABLED = .T.
*C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [End]

= lfwCtnHdrBr(loFormSet)
loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonH.REFRESH

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfvCtHdRem
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : validate pbHdrRem button
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCtHdRem()
*!*************************************************************

FUNCTION lfvCtHdRem
LPARAMETERS loFormSet
PRIVATE lnAlias,lcFltrExp,lnStyRec,lnSzeRec,lcCtnHdr,lcCtnDtl

lcCtnHdr = loFormSet.lcCtnHdr
lcCtnDtl = loFormSet.lcCtnDtl
lcPckLin = loFormSet.lcPckLin

STORE 0 TO lnStyRec,lnSzeRec

STORE .T. TO loFormSet.llCupdate,loFormSet.llAnyUpd

lnAlias = SELECT(0)
*-- "Are You Sure To Delete This Record"
*-- <YES>, <NO>
IF gfModalGen("INM44040B00006","Dialog") = 1
  *E037427,1 WAM 05/05/2005 Correct update of packed quantity
  DECLARE laSumKey[1]
  lnSumKey = 0
  *E037427,1 WAM 05/05/2005 (End)
  SELECT(lcPckLin)
  lcFltrExp = SET("FILTER")
  SET FILTER TO STYLE = ''
  SET RELATION TO
  SELECT (loFormSet.lcSumPck)
  SET FILTER TO
  SELECT(lcCtnDtl)
  SET RELATION TO
  IF SEEK(STR(&lcCtnHdr..Cart_No,4),lcCtnDtl)
    lnStyRec = RECNO(lcPckLin)
    SCAN REST FOR STR(Cart_No,4)+STYLE+Dyelot+STR(nOrdLineNo,6) = STR(&lcCtnHdr..Cart_No,4)
      SELECT(lcPckLin)
      IF EMPTY(Pack_iD)
        IF !EMPTY(&lcCtnDtl..STYLE) AND SEEK(&lcCtnDtl..STYLE+&lcCtnDtl..Dyelot+STR(&lcCtnDtl..nOrdLineNo,6)+&lcCtnDtl..SIZE,lcPckLin,'StySze')
          REPLACE PQty    WITH IIF(&lcCtnDtl..Br,MAX(PQty - &lcCtnDtl..Qty,0),PQty),;
            OrdQty  WITH IIF(&lcCtnDtl..Br,OrgOrd,OrdQty),;
            PWgh    WITH IIF(&lcCtnDtl..Br,MAX(PWgh - &lcCtnDtl..Weight,0),PWgh),;
            OQty    WITH IIF(&lcCtnDtl..Br,OQty + &lcCtnDtl..Qty,OQty),;
            OTotQty WITH OQty,;
            CtnQty  WITH IIF(&lcCtnDtl..Br,IIF(lSelect,OQty,0),CtnQty),;
            cOpen   WITH IIF(OQty > 0 AND AvlQty > 0,'Y','N')
        ENDIF
        SET ORDER TO lcSumPkLn IN (loFormSet.lcSumPck)
        IF SEEK(EVALUATE(lcCtnDtl+'.Style')+EVALUATE(lcCtnDtl+'.Dyelot')+STR(EVALUATE(lcCtnDtl+'.nOrdLineNo'),6)+;
            PADR(EVALUATE(lcCtnDtl+'.cSizeNo'),3),loFormSet.lcSumPck)
          SELECT (loFormSet.lcSumPck)
          REPLACE PTotQty    WITH EVALUATE(lcPckLin+'.PQty'),;
            OTotQty    WITH EVALUATE(lcPckLin+'.OQty'),;
            CtnTotQty  WITH EVALUATE(lcPckLin+'.CtnQty'),;
            PTotWgh    WITH EVALUATE(lcPckLin+'.PWgh'),;
            cOpen      WITH IIF(OTotQty > 0 ,'Y','N')
        ENDIF
      ELSE
        IF SEEK(&lcCtnDtl..pack_id+&lcCtnDtl..cpkColor+&lcCtnDtl..cPckSize+&lcCtnDtl..cpkVersion+;
            STR(&lcCtnDtl..nOrdLineNo,6)+&lcCtnDtl..STYLE+&lcCtnDtl..Dyelot+&lcCtnDtl..cSizeNo,lcPckLin,loFormSet.lcPakIndxLn)
          REPLACE PQty    WITH IIF(&lcCtnDtl..Br,MAX(PQty - &lcCtnDtl..Qty,0),PQty),;
            OrdQty  WITH IIF(&lcCtnDtl..Br,OrgOrd,OrdQty),;
            PWgh    WITH IIF(&lcCtnDtl..Br,MAX(PWgh - &lcCtnDtl..Weight,0),PWgh),;
            OQty    WITH IIF(&lcCtnDtl..Br,OQty + &lcCtnDtl..Qty,OQty),;
            OTotQty WITH OQty,;
            CtnQty  WITH IIF(&lcCtnDtl..Br,IIF(lSelect,OQty,0),CtnQty),;
            cOpen   WITH IIF(OQty > 0 AND AvlQty > 0,'Y','N')
        ENDIF
        SET ORDER TO (loFormSet.lcSumPck) IN (loFormSet.lcSumPck)
        IF SEEK(PADR(EVALUATE(lcCtnDtl+'.PACK_ID'),19)+EVALUATE(lcCtnDtl+'.cPkColor')+EVALUATE(lcCtnDtl+'.cPckSize')+;
            EVALUATE(lcCtnDtl+'.cPkVersion'),loFormSet.lcSumPck)
          SELECT (loFormSet.lcSumPck)
          *E037427,1 WAM 05/05/2005 Correct update of packed quantity
          lcPackId = PADR(EVALUATE(lcCtnDtl+'.PACK_ID'),19)+EVALUATE(lcCtnDtl+'.cPkColor')+EVALUATE(lcCtnDtl+'.cPckSize')+;
            EVALUATE(lcCtnDtl+'.cPkVersion')
          IF ASCAN(laSumKey,lcPackId)=0
            lnSumKey = lnSumKey + 1
            DIMENSION laSumKey[lnSumKey]
            laSumKey[lnSumKey] = lcPackId
            REPLACE PTotQty   WITH PTotQty - EVALUATE(loFormSet.lcCtnDtl+'.nPackNo '),;
              OTotQty   WITH OTotQty + EVALUATE(loFormSet.lcCtnDtl+'.nPackNo ') ,;
              CtnTotQty WITH OTotQty,;
              cOpen     WITH IIF(OTotQty > 0 ,'Y','N')
          ENDIF
          REPLACE PTotWgh WITH PTotWgh - EVALUATE(loFormSet.lcCtnDtl+'.Weight')
          *!*	          REPLACE OTotQty   WITH OTotQty + PTotQty,;
          *!*	                  PTotQty   WITH 0,;
          *!*	                  CtnTotQty WITH OTotQty,;
          *!*	                  PTotWgh   WITH PTotWgh - EVALUATE(lcPckLin+'.PWgh'),;
          *!*	                  cOpen     WITH IIF(OTotQty > 0 ,'Y','N')
          *E037427,1 WAM 05/05/2005 (End)
        ENDIF
      ENDIF
      SELECT (lcCtnDtl)
      REPLACE cStatus WITH 'D'
      DELETE
    ENDSCAN
  ENDIF
  SELECT (lcPckLin)
  SET FILTER TO &lcFltrExp
  = RLOCK(lcCtnDtl)
  UNLOCK IN (lcCtnDtl)
  IF lnStyRec <> 0 AND lnStyRec <= RECCOUNT(lcPckLin)
    GOTO lnStyRec
  ENDIF
  SELECT(lcCtnHdr)
  *-- This means that the carton has the max carton No.
  *-- which means it is the last record in the file
  IF &lcCtnHdr..Cart_No = loFormSet.lnMaxCtn
    *loFormSet.lnMaxCtn = &lcCtnHdr..Cart_No
    loFormSet.lnMaxCtn = MAX(loFormSet.lnMaxCtn - 1,0)
  ENDIF

  loFormSet.lnPackCtn = loFormSet.lnPackCtn - 1
  loFormSet.lnPackWgh = MAX(loFormSet.lnPackWgh - &lcCtnHdr..TotWgh,0)
  loFormSet.lnPackQty = MAX(loFormSet.lnPackQty - &lcCtnHdr..TotPcs,0)
  loFormSet.AriaForm1.pgfPacking.HEADER.txtWeight.VALUE  = loFormSet.lnPackWgh
  loFormSet.AriaForm1.pgfPacking.HEADER.txtCartons.VALUE = loFormSet.lnPackCtn
  loFormSet.AriaForm1.pgfPacking.HEADER.txtPieces.VALUE  = loFormSet.lnPackQty

  DELETE
  GO TOP
  = RLOCK(lcCtnHdr)
  UNLOCK IN (lcCtnHdr)

  *  loFormSet.lnMaxCtn = MAX(loFormSet.lnMaxCtn - 1,0)
  loFormSet.lnFrom   = loFormSet.lnMaxCtn + 1
  loFormSet.lnTo     = loFormSet.lnMaxCtn + 1

  =lfwCtnHdrBr(loFormSet)
  loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonH.REFRESH
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvCtDtNew
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : validate pbDtlNew button
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCtDtNew()
*!*************************************************************
FUNCTION lfvCtDtNew

*! B608342,1 MMT 11/01/2007 Add Scan Option to the manual pascking list[Start]
*LPARAMETERS loFormSet
LPARAMETERS loFormSet,llUPC
*! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[End]

PRIVATE lnCurAlias
STORE .T. TO loFormSet.llCupdate,loFormSet.llAnyUpd
lnCurAlias = SELECT(0)

SELECT (loFormSet.lcPackLines)
ZAP
SELECT(loFormSet.lcPckLin)
SCAN
  SCATTER MEMVAR MEMO
  *IF EMPTY(m.Pack_Id) && Wael
  INSERT INTO (loFormSet.lcPackLines) FROM MEMVAR
  *ENDIF
ENDSCAN
SELECT (loFormSet.lcPckLin)
GO TOP
=RLOCK(loFormSet.lcPckLin)
UNLOCK IN (loFormSet.lcPckLin)


SELECT(loFormSet.lcCtnDtl)
*! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[Start]
*!*  IF !SEEK(STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_NO'),4)+SPACE(19),loFormSet.lcCtnDtl)
*!*    APPEND BLANK
*!*    REPLACE Cart_No WITH EVALUATE(loFormSet.lcCtnHdr+'.Cart_NO'),;
*!*      SzCnt   WITH 1,;
*!*      Br      WITH .T.

*!*    = RLOCK(loFormSet.lcCtnDtl)
*!*    UNLOCK IN (loFormSet.lcCtnDtl)
*!*  ELSE
*!*    REPLACE Br      WITH .T.
*!*  ENDIF
*!*  =lfwCtnDtlBr(loFormSet)
lfReSetControlSource(loFormSet)
IF !llUPC
  loFormSet.ariaForm1.pgfPacking.CartonInfo.kbStyle.ENABLED = .T.
  loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.ENABLED = .F.
  loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.VALUE = ''
  loFormSet.AriaForm1.pgfPacking.CartonInfo.kbStyle.VALUE = ''
  loFormSet.AriaForm1.pgfPacking.CartonInfo.kbSize.VALUE =''
  loFormSet.AriaForm1.pgfPacking.CartonInfo.txtCartQtyD.VALUE = 0
  loFormSet.AriaForm1.pgfPacking.CartonInfo.txtCartWghD.VALUE = 0
  loFormSet.AriaForm1.pgfPacking.CartonInfo.kbStyle.SETFOCUS()
ENDIF
*! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[End]

loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonD.REFRESH
loFormSet.AriaForm1.pgfPacking.CartonInfo.cmdRemoveCartD.ENABLED = .T.

*! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[Start]
loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.ENABLED = .F.
*! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[End]


SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfvCtDtRem
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : validate pbDtlRem button
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
FUNCTION lfvCtDtRem
LPARAMETERS loFormSet

PRIVATE lnAlias
lnAlias = SELECT(0)
*-- "Are You Sure To Delete This Record"
*-- <YES>, <NO>
IF gfModalGen("INM44040B00006","Dialog") = 1
  SELECT (loFormSet.lcPckLin)
  SET FILTER TO
  SELECT (loFormSet.lcSumPck)
  SET FILTER TO
  IF !EMPTY(EVALUATE(loFormSet.lcCtnDtl+'.PAck_id'))
    *-- Message < This line is included in a pack, do you want to remove the entire pack ? >
    *-- Buttons <                                Yes     No                                >
    IF gfModalGen('TRM44129B32000','DIALOG') = 2
      RETURN .F.
    ENDIF
    SET ORDER TO (loFormSet.lcSumPck) IN (loFormSet.lcSumPck)
    SELECT (loFormSet.lcCtnDtl)
    lcPack_Id = PAck_id+cpkColor+cpckSize+cpKversion
    *E037427,1 WAM 05/05/2005 Correct update of packed quantity
    IF SEEK(PADR(PAck_id,19)+cpkColor+cpckSize+cpKversion,loFormSet.lcSumPck)
      SELECT (loFormSet.lcSumPck)
      REPLACE PTotQty    WITH PTotQty - EVALUATE(loFormSet.lcCtnDtl+'.nPackNo '),;
        OTotQty    WITH OTotQty + EVALUATE(loFormSet.lcCtnDtl+'.nPackNo ') ,;
        CtnTotQty  WITH OTotQty,;
        cOpen      WITH IIF(OTotQty > 0 ,'Y','N')
    ENDIF
    SELECT (loFormSet.lcCtnDtl)
    *E037427,1 WAM 05/05/2005 (End)
    SCAN FOR PAck_id+cpkColor+cpckSize+cpKversion = lcPack_Id
      =lfvCtlnRem(loFormSet)
      IF SEEK(PADR(EVALUATE(loFormSet.lcPckLin+'.PACK_ID'),19)+EVALUATE(loFormSet.lcPckLin+'.cPkColor')+EVALUATE(loFormSet.lcPckLin+'.cPckSize')+EVALUATE(loFormSet.lcPckLin+'.cPkVersion'),loFormSet.lcSumPck)
        SELECT (loFormSet.lcSumPck)
        *E037427,1 WAM 05/05/2005 Correct update of packed quantity
        REPLACE PTotWgh WITH PTotWgh - EVALUATE(loFormSet.lcCtnDtl+'.Weight')
        *!*	        REPLACE PTotQty    WITH PTotQty - EVALUATE(loFormSet.lcPckLin+'.PQty'),;
        *!*	                OTotQty    WITH OTotQty + EVALUATE(loFormSet.lcPckLin+'.OQty'),;
        *!*	                CtnTotQty  WITH OTotQty,;
        *!*	                PTotWgh    WITH PTotWgh - EVALUATE(loFormSet.lcPckLin+'.PWgh'),;
        *!*	                cOpen      WITH IIF(OTotQty > 0 ,'Y','N')
        *E037427,1 WAM 05/05/2005 (End)
      ENDIF
    ENDSCAN
    SELECT (loFormSet.lcCtnDtl)
    REPLACE Cart_No WITH 0 ,;
      STYLE   WITH '',;
      Br      WITH .F. ,;
      SIZE    WITH ''  ,;
      Qty     WITH 0   ,;
      cStatus WITH 'D' FOR PAck_id+cpkColor+cpckSize+cpKversion = lcPack_Id
    *E037427,1 WAM 05/05/2005 Fix bug od delete carton lines
    *DELETE FOR PAck_id+cpkColor+cpckSize+cpKversion = lcPack_Id
    SET KEY TO
    DELETE FOR Cart_No= 0 AND PAck_id+cpkColor+cpckSize+cpKversion = lcPack_Id
    SET KEY TO STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4)
    LOCATE
    *E037427,1 WAM 05/05/2005 (End)
  ELSE
    =lfvCtlnRem(loFormSet)
    SET ORDER TO lcSumPkLn IN (loFormSet.lcSumPck)
    IF SEEK(EVALUATE(loFormSet.lcPckLin+'.Style')+EVALUATE(loFormSet.lcPckLin+'.Dyelot')+STR(EVALUATE(loFormSet.lcPckLin+'.nOrdLineNo'),6)+PADR(EVALUATE(loFormSet.lcPckLin+'.cSizeNo'),3),loFormSet.lcSumPck)
      SELECT (loFormSet.lcSumPck)
      REPLACE PTotQty    WITH EVALUATE(loFormSet.lcPckLin+'.PQty'),;
        OTotQty    WITH EVALUATE(loFormSet.lcPckLin+'.OQty'),;
        CtnTotQty  WITH OTotQty,;
        PTotWgh    WITH EVALUATE(loFormSet.lcPckLin+'.PWgh'),;
        cOpen      WITH IIF(OTotQty > 0 ,'Y','N')
    ENDIF
    SELECT (loFormSet.lcCtnDtl)
    *E037427,1 WAM 05/05/2005 Fix bug of delete carton lines
    *REPLACE Cart_No WITH 0 ,;
    Style   WITH '',;
    Br      WITH .F. ,;
    Size    WITH ''  ,;
    Qty     WITH 0   ,;
    cStatus WITH 'D'
    SET KEY TO
    REPLACE Qty     WITH 0  ,;
      cStatus WITH 'D'
    *E037427,1 WAM 05/05/2005 (End)

    DELETE
    *E037427,1 WAM 05/05/2005 Fix bug of delete carton lines
    SET KEY TO STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4)
    LOCATE
    *E037427,1 WAM 05/05/2005 (End)
  ENDIF
ENDIF

=RLOCK(loFormSet.lcCtnDtl)
UNLOCK IN (loFormSet.lcCtnDtl)
=lfwCtnDtlBr(loFormSet)
loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonD.REFRESH
=lfwCtnHdrBr(loFormSet)
loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonH.REFRESH

SELECT (lnAlias )

*!*************************************************************
*! Name      : lfvCtlnRem
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : validate pbDtlRem button
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCtlnRem()
*!*************************************************************
FUNCTION lfvCtlnRem
LPARAMETERS loFormSet

PRIVATE lnAlias,lcFltrExp,lcSzFld

STORE .T. TO loFormSet.llCupdate,loFormSet.llAnyUpd

lnAlias = SELECT(0)
SELECT(loFormSet.lcPckLin)
lcFltrExp = SET("FILTER")
SET FILTER TO STYLE = ''

IF !EMPTY(EVALUATE(loFormSet.lcCtnDtl+'.Style')) AND SEEK(EVALUATE(loFormSet.lcCtnDtl+'.Style')+EVALUATE(loFormSet.lcCtnDtl+'.Dyelot')+;
    STR(EVALUATE(loFormSet.lcCtnDtl+'.nOrdLineNo'),6)+EVALUATE(loFormSet.lcCtnDtl+'.Size'),loFormSet.lcPckLin,'StySze')
  SELECT(loFormSet.lcPckLin)
  lcPQtyFld   = 'PQty'
  lcPWghFld   = 'PWgh'
  lcOQtyFld   = 'OQty'
  lcCtnQtyFld = 'CtnQty'
  llSelectFld = 'lSelect'
  lcOrdQtyFld = 'OrdQty'
  lcOrgOrdFld = 'OrgOrd'
  REPLACE &lcPQtyFld   WITH MAX(EVAL('PQty') - EVAL(loFormSet.lcCtnDtl+'.Qty'),0) ,;
    &lcPWghFld   WITH MAX(EVAL('PWgh') - EVAL(loFormSet.lcCtnDtl+'.Weight'),0),;
    &lcOQtyFld   WITH EVAL('OQty')     + EVAL(loFormSet.lcCtnDtl+'.Qty'),;
    &lcCtnQtyFld WITH IIF(EVAL('lSelect'),EVAL('OQty'),0),;
    &lcOrdQtyFld WITH &lcOrgOrdFld,;
    cOpen        WITH IIF(OQty > 0 AND AvlQty > 0,'Y','N')

  = RLOCK(loFormSet.lcPckLin)
  UNLOCK IN (loFormSet.lcPckLin)
ENDIF
SET FILTER TO &lcFltrExp

loFormSet.lnPackWgh = MAX(loFormSet.lnPackWgh - EVAL(loFormSet.lcCtnDtl+'.Weight'),0)
loFormSet.lnPackQty = MAX(loFormSet.lnPackQty - EVAL(loFormSet.lcCtnDtl+'.Qty'),0)

SELECT (loFormSet.lcCtnHdr)
REPLACE TotPcs WITH MAX(TotPcs - EVAL(loFormSet.lcCtnDtl+'.Qty'),0),;
  TotWgh WITH MAX(TotWgh - EVAL(loFormSet.lcCtnDtl+'.Weight'),0),;
  EMPTY  WITH IIF(TotPcs>0,'N','Y')

loFormSet.AriaForm1.pgfPacking.HEADER.txtWeight.VALUE  = loFormSet.lnPackWgh
loFormSet.AriaForm1.pgfPacking.HEADER.txtPieces.VALUE  = loFormSet.lnPackQty

IF TotPcs = 0
  DELETE
  LOCATE
  loFormSet.lnPackCtn = MAX(loFormSet.lnPackCtn - 1,0)
  loFormSet.AriaForm1.pgfPacking.HEADER.txtCartons.VALUE = loFormSet.lnPackCtn
  loFormSet.lnMaxCtn = MAX(loFormSet.lnMaxCtn - 1,0)
ENDIF
loFormSet.lnFrom   = loFormSet.lnMaxCtn + 1
loFormSet.lnTo     = loFormSet.lnMaxCtn + 1

= RLOCK(loFormSet.lcCtnHdr)
UNLOCK IN (loFormSet.lcCtnHdr)

SELECT (lnAlias)


*!*************************************************************
*! Name      : lfvStyCtnQty
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : validate Style quantity in carton
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvStyCtnQty()
*!*************************************************************
FUNCTION lfvStyCtnQty
LPARAMETERS loFormSet

*C200876,1 TMI [Start] do not change the qtys added when bin location is installed
IF ASCAN(loFormSet.laEvntTrig,PADR('CHNGQTY',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0 .AND. ;
    !loFormSet.mDoTrigger(PADR('CHNGQTY',10))
  RETURN
ENDIF
*C200876,1 TMI [End  ]

PRIVATE lnCurAlias,lnUnitWgh,lnContinue,lcFld

STORE 0 TO lnUnitWgh,lnContinue
SELECT (loFormSet.lcSumPck)
SET FILTER TO
SELECT (loFormSet.lcPckLin)
SET FILTER TO
lcOrderNo = loFormSet.AriaForm1.kbOrderNo.keytextbox.VALUE
lnStyQty  = loFormSet.AriaFORM1.pgfPacking.cartonInfo.txtCartQtyD.VALUE
lcOldVal  = loFormSet.AriaFORM1.pgfPacking.cartonInfo.txtCartQtyD.OldValue

SELECT (loFormSet.lcCtnDtl)
lcKeytoSeek = EVALUATE(KEY())

IF lnStyQty < 0
  *-- Message : 42000
  *-- Negative values are not allowed.
  *-- Button  : 40011
  *--Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  loFormSet.AriaFORM1.pgfPacking.cartonInfo.txtCartQtyD.VALUE = lcOldVal
  =SEEK(lcKeytoSeek,loFormSet.lcCtnDtl)
  llReturn = .F.
ENDIF
llClearPrPck = .F.
lnStyOQty = 0
IF SEEK(EVALUATE(loFormSet.lcCtnDtl+'.Style')+EVALUATE(loFormSet.lcCtnDtl+'.Dyelot')+;
    STR(EVALUATE(loFormSet.lcCtnDtl+'.nOrdLineNo'),6)+EVALUATE(loFormSet.lcCtnDtl+'.Size'),loFormSet.lcPckLin,'StySze')
  lnStyOQty =  EVAL(loFormSet.lcPckLin+'.OQty')
ENDIF
IF !(lnStyQty == lcOldVal)
  lnCurAlias = SELECT(0)

  lnUnitWgh  = IIF(EVAL(loFormSet.lcCtnDtl+'.Qty')>0,EVAL(loFormSet.lcCtnDtl+'.Weight')/EVAL(loFormSet.lcCtnDtl+'.Qty'),0)

  IF SEEK(EVALUATE(loFormSet.lcCtnDtl+'.Style')+EVALUATE(loFormSet.lcCtnDtl+'.Dyelot')+;
      STR(EVALUATE(loFormSet.lcCtnDtl+'.nOrdLineNo'),6)+EVALUATE(loFormSet.lcCtnDtl+'.Size'),loFormSet.lcPckLin,'StySze')
    IF EVAL(loFormSet.lcPcklin+'.OrdQty') < EVAL(loFormSet.lcPckLin+'.PQty') - ;
        EVAL(loFormSet.lcCtnDtl+'.Qty') + lnStyQty
      *-- Packed quantity for Style/Size x/x exceeds ordered quantity.;
      Do you want to modify the orderd quantity?
      *-- <Yes>,<No>
      lnContinue = gfModalGen("QRM44034B00006","Dialog",EVALUATE(loFormSet.lcPckLin+'.Style') + '/' +;
        EVAL(loFormSet.lcPckLin+'.cSizeCod'))
      *-- 44134 : This line has a prepack. Increasing the Quantity will release the prepack.
      *-- 00012 : <Procced> <Cancel>
      IF lnContinue = 1
        IF!EMPTY(EVAL(loFormSet.lcCtnDtl+'.PrePak'))
          llClearPrPck = (gfModalGen("QRM44134B00012","Dialog") = 1)
          IF !llClearPrPck
            lnContinue = 2
          ENDIF
        ENDIF
      ENDIF
    ENDIF

    IF lnContinue <> 2
      SELECT(loFormSet.lcPckLin)
      PRIVATE lcOrgOrd
      lcOrgOrd = 'OrgOrd'

      REPLACE OrdQty WITH MAX(EVAL(loFormSet.lcPckLin+'.PQty') - ;
        EVAL(loFormSet.lcCtnDtl+'.Qty') + lnStyQty ,&lcOrgOrd)
      REPLACE OQty  WITH MAX(EVAL('OQty') + EVAL(loFormSet.lcCtnDtl+'.Qty') - lnStyQty,0),;
        PQty  WITH EVAL('PQty') - EVAL(loFormSet.lcCtnDtl+'.Qty') + lnStyQty,;
        PWgh  WITH EVAL('PWgh') - EVAL(loFormSet.lcCtnDtl+'.Weight')+ lnStyQty*lnUnitWgh,;
        cOpen WITH IIF(OQty > 0 ,'Y','N')

      = RLOCK(loFormSet.lcPckLin)
      UNLOCK IN (loFormSet.lcPckLin)
      SET ORDER TO lcSumPkLn IN (loFormSet.lcSumPck)
      IF SEEK(EVALUATE(loFormSet.lcPckLin+'.Style')+EVALUATE(loFormSet.lcPckLin+'.Dyelot')+;
          STR(EVALUATE(loFormSet.lcPckLin+'.nOrdLineNo'),6)+PADR(EVALUATE(loFormSet.lcPckLin+'.cSizeNo'),3),loFormSet.lcSumPck)
        SELECT (loFormSet.lcSumPck)
        REPLACE PTotQty    WITH EVALUATE(loFormSet.lcPckLin+'.PQty'),;
          OTotQty    WITH EVALUATE(loFormSet.lcPckLin+'.OQty'),;
          CtnTotQty  WITH OTotQty,;
          PTotWgh    WITH EVALUATE(loFormSet.lcPckLin+'.PWgh'),;
          cOpen      WITH IIF(OTotQty > 0 ,'Y','N')
      ENDIF

    ENDIF
  ENDIF

  IF SEEK(STR(EVALUATE(loFormSet.lcCtnDtl+'.Cart_No'),4),loFormSet.lcCtnHdr) AND lnContinue <> 2
    loFormSet.lnPackWgh = loFormSet.lnPackWgh - EVAL(loFormSet.lcCtnDtl+'.Weight') + lnStyQty*lnUnitWgh
    loFormSet.lnPackQty = loFormSet.lnPackQty - EVAL(loFormSet.lcCtnDtl+'.Qty') + lnStyQty

    SELECT(loFormSet.lcCtnHdr)
    REPLACE TotPcs WITH TotPcs - EVAL(loFormSet.lcCtnDtl+'.Qty') + lnStyQty,;
      EMPTY  WITH IIF(TotPcs > 0 , 'N' , 'Y'),;
      TotWgh WITH TotWgh - EVAL(loFormSet.lcCtnDtl+'.Weight') + lnStyQty*lnUnitWgh


    = RLOCK(loFormSet.lcCtnHdr)
    UNLOCK IN (loFormSet.lcCtnHdr)
  ENDIF

  IF lnContinue <> 2
    SELECT(loFormSet.lcCtnDtl)
    lcQtyFld = 'Qty'
    lcWghFld = 'Weight'

    REPLACE &lcQtyFld   WITH lnStyQty,;
      &lcWghFld   WITH EVAL('Qty')*lnUnitWgh,;
      cStatus     WITH 'M'
    IF llClearPrPck
      REPLACE PrePak  WITH "",;
        PpQty   WITH 0
    ENDIF
    = RLOCK(loFormSet.lcCtnDtl)
    UNLOCK IN (loFormSet.lcCtnDtl)
  ENDIF

  loFormSet.AriaForm1.pgfPacking.HEADER.txtWeight.VALUE  = loFormSet.lnPackWgh
  loFormSet.AriaForm1.pgfPacking.HEADER.txtPieces.VALUE  = loFormSet.lnPackQty

  = lfwCtnDtlBr(loFormSet)
  loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonD.REFRESH
  SELECT(lnCurAlias)
ENDIF

*!*************************************************************
*! Name      : lfvCtDtWgh
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : validate Style weight in carton
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCtDtWgh()
*!*************************************************************
FUNCTION lfvCtDtWgh
LPARAMETERS loFormSet

PRIVATE lnCurAlias,lnCart,lcStyle,lnOrdLin,lnUnitWgh
STORE 0 TO lnCart,lnOrdLin,lnUnitWgh
STORE SPACE(0) TO lcStyle

lnStyWgh = loFormSet.AriaFORM1.pgfPacking.cartonInfo.txtCartWghD.VALUE
lcOldVal = loFormSet.AriaFORM1.pgfPacking.cartonInfo.txtCartWghD.OldValue

SELECT (loFormSet.lcCtnDtl)
lcKeytoSeek = EVALUATE(KEY())

IF lnStyWgh < 0
  *-- Message : 42000
  *-- Negative values are not allowed.
  *-- Button  : 40011
  *--Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  loFormSet.AriaFORM1.pgfPacking.cartonInfo.txtCartWghD.VALUE  = lcOldVal
  =SEEK(lcKeytoSeek,loFormSet.lcCtnDtl)
  llReturn = .F.
ENDIF

SELECT (loFormSet.lcSumPck)
SET FILTER TO
SELECT (loFormSet.lcPckLin)
SET FILTER TO

IF !(lnStyWgh == lcOldVal)
  lnCurAlias = SELECT(0)
  lnUnitWgh = lnStyWgh/EVAL(loFormSet.lcCtnDtl+'.Qty')

  lnCrtWeight = EVALUATE(loFormSet.lcCtnDtl+'.Weight')
  lnCart    = EVALUATE(loFormSet.lcCtnDtl+'.Cart_No')
  lcStyle   = EVALUATE(loFormSet.lcCtnDtl+'.Style')
  lcDyelot  = EVALUATE(loFormSet.lcCtnDtl+'.Dyelot')
  lnOrdLin  = EVALUATE(loFormSet.lcCtnDtl+'.nOrdLineNo')
  lcSizeCod = EVALUATE(loFormSet.lcCtnDtl+'.Size')
  lcSizeNo  = EVALUATE(loFormSet.lcCtnDtl+'.cSizENo')
  lnStyRec  = RECNO(loFormSet.lcPckLin)
  SET ORDER TO lcSumPkLn IN (loFormSet.lcSumPck)
  SELECT (loFormSet.lcPckLin)
  SET ORDER TO (loFormSet.lcPckLin)
  SET FILTER TO
  IF SEEK(lcStyle+lcDyelot+STR(lnOrdLin,6)+lcSizeNo,loFormSet.lcPckLin)
    SELECT(loFormSet.lcPckLin)
    SCAN REST FOR STYLE+dyelot+STR(nOrdLineNo,6)+cSizeCod = lcStyle+lcDyelot+STR(lnOrdLin,6)+lcSizeCod
      =SEEK(STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4)+STYLE+Dyelot+STR(nOrdLineNo,6)+lcSizeNo,loFormSet.lcCtnDtl)
      REPLACE PWgh WITH PWgh - EVALUATE(loFormSet.lcCtnDtl+'.Weight')+ EVAL(loFormSet.lcCtnDtl+'.Qty')*lnUnitWgh
      IF SEEK(lcStyle+lcDyelot+STR(lnOrdLin,6)+PADR(lcSizeNo,3),loFormSet.lcSumPck)
        SELECT (loFormSet.lcSumPck)
        REPLACE PTotWgh    WITH EVALUATE(loFormSet.lcPckLin+'.PWgh')
      ENDIF
    ENDSCAN
    SET RELATION TO
    IF lnStyRec <= RECCOUNT(loFormSet.lcPckLin)
      GOTO lnStyRec
    ENDIF

    = RLOCK(loFormSet.lcPckLin)
    UNLOCK IN (loFormSet.lcPckLin)

  ENDIF

  IF SEEK(STR(lnCart,4)+lcStyle+lcDyelot,loFormSet.lcCtnDtl)
    lnStyRec = RECNO(loFormSet.lcCtnDtl)
    lcWghFld = 'Weight'
    SELECT(loFormSet.lcCtnDtl)
    REPLACE REST FOR STR(Cart_No,4)+STYLE = STR(lnCart,4)+lcStyle ;
      Weight WITH Qty*lnUnitWgh,;
      cStatus WITH 'M'
    IF lnStyRec <= RECCOUNT(loFormSet.lcCtnDtl)
      GOTO lnStyRec
    ENDIF
    = RLOCK(loFormSet.lcCtnDtl)
    UNLOCK IN (loFormSet.lcCtnDtl)
    IF SEEK(STR(EVALUATE(loFormSet.lcCtnDtl+'.Cart_No'),4),loFormSet.lcCtnHdr)
      SELECT SUM(Weight);
        FROM (loFormSet.lcCtnDtl) INTO ARRAY laCtnWgh;
        WHERE Cart_No = EVAL(loFormSet.lcCtnHdr+'.Cart_No')

      loFormSet.lnPackWgh = loFormSet.lnPackWgh - EVALUATE(loFormSet.lcCtnHdr+'.TotWgh') + laCtnWgh
      loFormSet.AriaForm1.pgfPacking.HEADER.txtWeight.VALUE  = loFormSet.lnPackWgh

      SELECT(loFormSet.lcCtnHdr)
      REPLACE TotWgh WITH laCtnWgh
      = RLOCK(loFormSet.lcCtnHdr)
      UNLOCK IN (loFormSet.lcCtnHdr)
    ENDIF
  ENDIF
  = lfwCtnHdrBr(loFormSet)
  loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonD.REFRESH
  SELECT(lnCurAlias)

ENDIF


*!*************************************************************
*! Name      : lfvCtnSty
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : Validat lcCtnSty field .
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfStyBrow,lfChkSty
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCtnSty()
*!*************************************************************
FUNCTION lfvCtnSty

*! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[Start]
*LPARAMETERS loFormSet,llBrowse,lcCtnSty

*B609658,1 WAM 08/09/2011 Add new parameter set to true if the UPC for a pack
*LPARAMETERS loFormSet,llBrowse,lcCtnSty,llUpcAdd
LPARAMETERS loFormSet,llBrowse,lcCtnSty,llUpcAdd, lUpcPack
*B609658,1 WAM 08/09/2011 (End)
*! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[End]

lnCurAlias = SELECT(0)

SET ORDER TO (loFormSet.lcPackLines) IN (loFormSet.lcPackLines)
SELECT (loFormSet.lcSumPck)
SET FILTER TO
SELECT (loFormSet.lcPckLin)
SET FILTER TO


*! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[Start]
lnOldRec = RECNO(loFormSet.lcPackLines)
*! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[End]
*! B609837,1 MMT 02/20/2012 Error while editing Packing list has 2 styles with diff. scales[T20120131.0033][Start]
lcCtnSizeCode = SPACE(1)
*! B609837,1 MMT 02/20/2012 Error while editing Packing list has 2 styles with diff. scales[T20120131.0033][End]

*B609658,1 WAM 08/09/2011 Check if the scanned pack belong to the order
*IF llBrowse OR (!EMPTY(lcCtnSty) AND !SEEK(lcCtnSty,loFormSet.lcPackLines))
SELECT (loFormSet.lcPackLines)
IF lUpcPack
  LOCATE FOR PACK_ID = LEFT(lcCtnSty,16)
  llItemExist = FOUND()
ELSE
  llItemExist = !EMPTY(lcCtnSty) AND SEEK(lcCtnSty,loFormSet.lcPackLines)
ENDIF
IF llBrowse OR !llItemExist
  *B609658,1 WAM 08/09/2011 (End)

  llBrowse = .T.
ELSE

  *! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[Start]
  *IF !EMPTY(lcCtnSty)
  IF !EMPTY(lcCtnSty) AND !llUpcAdd
    *! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[End]
    lcCtnSty  = EVALUATE(loFormSet.lcPackLines+'.STYLE')
    *! B609837,1 MMT 02/20/2012 Error while editing Packing list has 2 styles with diff. scales[T20120131.0033][Start]
    lcCtnSizeCode = EVALUATE(loFormSet.lcPackLines+'.cSizeno')
    *! B609837,1 MMT 02/20/2012 Error while editing Packing list has 2 styles with diff. scales[T20120131.0033][End]
  ENDIF
ENDIF

IF llBrowse
  llBrowse  = .F.
  llNoThing = lfStyBrow(loFormSet)
  lcCtnSty  = IIF(llNoThing,EVALUATE(loFormSet.lcPackLines+'.Style'),SPACE(19))
  *! B609837,1 MMT 02/20/2012 Error while editing Packing list has 2 styles with diff. scales[T20120131.0033][Start]
  lcCtnSizeCode = IIF(llNoThing,EVALUATE(loFormSet.lcPackLines+'.cSizeno'),SPACE(1))
  *! B609837,1 MMT 02/20/2012 Error while editing Packing list has 2 styles with diff. scales[T20120131.0033][End]
  *! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[Start]
ELSE
  *C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [Begin]
  IF ASCAN(loFormSet.laEvntTrig,PADR('AUTOSCAN',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
  ELSE
    *C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [End]

    IF BETWEEN(lnOldRec,1,RECCOUNT(loFormSet.lcPackLines))
      GO RECORD lnOldRec IN (loFormSet.lcPackLines)
    ENDIF
    *C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [Begin]
  ENDIF
  *C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [End]

  *! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[End]
ENDIF

*! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[Start]
IF llUpcAdd
  =SEEK(lcCtnSty+ORDLINE.Dyelot+STR(ORDLINE.LINENO,6)+ALLTRIM(STYLEUPC.SIZE),loFormSet.lcPackLines,loFormSet.lcPackLines)
ENDIF
*! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[End]

IF EMPTY(lcCtnSty)
  lfSetControlSource(loFormSet)
  RETURN
ENDIF
*! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[End]

*B609658,1 WAM 08/09/2011 Process all lines for the scanned pack
SELECT (loFormSet.lcPackLines)
lcPack_ID = LEFT(lcCtnSty,16)
IF lUpcPack AND llUpcAdd  AND SEEK(lcPack_ID,loFormSet.lcSumPck,loFormSet.lcSumPck)
  REPLACE PTotQty WITH PTotQty + 1 ,;
    OTotQty WITH OTotQty - 1,;
    cOPen   WITH IIF(OTotQty > 0 ,'T','N') IN (loFormSet.lcSumPck)
ENDIF
*! B609837,1 MMT 02/20/2012 Error while editing Packing list has 2 styles with diff. scales[T20120131.0033][Start]
*SCAN FOR IIF(lUpcPack, PACK_ID = lcPack_ID,Style+cSizeNo=lcCtnSty+ALLTRIM(STYLEUPC.SIZE))
SCAN FOR IIF(lUpcPack, PACK_ID = lcPack_ID,STYLE+cSizeNo=lcCtnSty+IIF(llUpcAdd,ALLTRIM(STYLEUPC.SIZE),ALLTRIM(lcCtnSizeCode)))
  *! B609837,1 MMT 02/20/2012 Error while editing Packing list has 2 styles with diff. scales[T20120131.0033][End]
  lcCtnSty = IIF(lUpcPack, STYLE, lcCtnSty)
  *B609658,1 WAM 08/09/2011 (End)

  lcStySz   = EVALUATE(loFormSet.lcPackLines+'.cSizeCod')
  lcStyno   = EVALUATE(loFormSet.lcPackLines+'.cSizeno')

  *! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[Start]
  *lnStyQty  = EVALUATE(loFormSet.lcPackLines+'.OQty')
  lnStyQty  = IIF(llUpcAdd,1,EVALUATE(loFormSet.lcPackLines+'.OQty'))
  *! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[End]

  *B609658,1 WAM 08/09/2011 Get style\color\size quantity
  IF llUpcAdd AND lUpcPack
    IF loFormSet.Spck_Lin.SEEK('P'+lcAccount + lcPack_ID +lcCtnSty ) OR loFormSet.Spck_Lin.SEEK('P'+'*****' + lcPack_ID +lcCtnSty ,'spck_lin')
      lnStyQty = EVALUATE('Spck_Lin.Qty'+CSIZENO)
    ENDIF
  ENDIF
  *B609658,1 WAM 08/09/2011 (End)

  lnOrdLine = EVALUATE(loFormSet.lcPackLines+'.nOrdLineNo')
  *B608882,1 TMI 05/31/2009 09:01:28 AM [Start] if in scan mode , qty should be 1
  *lnWgh     = EVALUATE(loFormSet.lcPackLines+'.OQty')*EVALUATE(loFormSet.lcPackLines+'.StyWgh')

  *B609658,1 WAM 08/09/2011 Get Style\color\size weight
  *lnWgh     = IIF(llUpcAdd,1,EVALUATE(loFormSet.lcPackLines+'.OQty'))*EVALUATE(loFormSet.lcPackLines+'.StyWgh')
  lnWgh     = lnStyQty*EVALUATE(loFormSet.lcPackLines+'.StyWgh')
  *B609658,1 WAM 08/09/2011 (End)

  *B608882,1 TMI 05/31/2009 09:01:57 AM [End  ]
  lcDyelot  = EVALUATE(loFormSet.lcPackLines+'.Dyelot')
  *C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [Begin]
  IF ASCAN(loFormSet.laEvntTrig,PADR('UPCQTY',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
    llUpcAdd_INF = llUpcAdd
    =loFormSet.mDoTrigger(PADR('UPCQTY',10))
  ENDIF
  *C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [End]

  *-- if the style with order record No. is not found in the carton
  SELECT (loFormSet.lcCtnDtl)
  *! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[Start]
  *IF !SEEK(STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4)+lcCtnSty+lcDyelot+STR(lnOrdLine,6)+lcStySz,loFormSet.lcCtnDtl,'StySze') OR ;
  (SEEK(STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4)+lcCtnSty+lcDyelot+STR(lnOrdLine,6)+lcStySz,loFormSet.lcCtnDtl,'StySze') AND;
  lnStyQty <> 0)
  *C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [Begin]
  *IF !SEEK(STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4)+lcCtnSty+lcDyelot+STR(lnOrdLine,6)+lcStySz,loFormSet.lcCtnDtl,'StySze')
  IF (!SEEK(STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4)+lcCtnSty+lcDyelot+STR(lnOrdLine,6)+lcStySz,loFormSet.lcCtnDtl,'StySze') ;
      AND lnStyQty <> 0)
    *C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [End]
    APPEND BLANK
    REPLACE Cart_No WITH EVALUATE(loFormSet.lcCtnHdr+'.Cart_NO'),;
      SzCnt   WITH 1,;
      Br      WITH .T.

    = RLOCK(loFormSet.lcCtnDtl)
    UNLOCK IN (loFormSet.lcCtnDtl)

    *! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[End]
    llAlowAdd = .T.
    IF SEEK(STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4)+SPACE(19),loFormSet.lcCtnDtl)
      REPLACE STYLE      WITH lcCtnSty ,;
        Dyelot     WITH lcDyelot ,;
        SIZE       WITH lcStySz  ,;
        cSizeno    WITH lcStyno  ,;
        Qty        WITH lnStyQty ,;
        Weight     WITH lnWgh    ,;
        nOrdLineNo WITH lnOrdLine,;
        Br         WITH .T.,;
        SzCnt      WITH EVALUATE(loFormSet.lcPackLines+'.SzCnt'),;
        OrgWgh     WITH EVALUATE(loFormSet.lcPackLines+'.StyWgh'),;
        cStatus    WITH "A",;
        TotQty     WITH lnStyQty,;
        TotWeight  WITH lnWgh

      *! B608342,1 MMT 11/01/2007 Add Scan option to the manual packing list[Start]
      IF loFormSet.ActiveMode = 'E' AND EMPTY(PackLineNo)

        *! B608518,1 MMT 04/17/2008 Fix bug of Wrong Line Qty when Editing Packing List [Start]
        loFormSet.lnLastNo = loFormSet.lnLastNo + 1
        *! B608518,1 MMT 04/17/2008 Fix bug of Wrong Line Qty when Editing Packing List [End]

        REPLACE PackLineNo  WITH loFormSet.lnLastNo
      ENDIF

      IF llUpcAdd
        REPLACE Cupc WITH lcUpc
      ENDIF
      *! B608342,1 MMT 11/01/2007 Add Scan option to the manual packing list[End]


      loFormSet.lnPackWgh = loFormSet.lnPackWgh + EVAL(loFormSet.lcCtnDtl+'.Weight')
      loFormSet.lnPackQty = loFormSet.lnPackQty + EVAL(loFormSet.lcCtnDtl+'.Qty')
      SELECT(loFormSet.lcCtnHdr)
      REPLACE TotPcs WITH TotPcs + EVAL(loFormSet.lcCtnDtl+'.Qty'),;
        TotWgh WITH TotWgh + EVAL(loFormSet.lcCtnDtl+'.Weight'),;
        EMPTY  WITH IIF(TotPcs>0,'N','Y')
      = RLOCK(loFormSet.lcCtnHdr)
      UNLOCK IN (loFormSet.lcCtnHdr)
      SELECT(loFormSet.lcCtnDtl)
    ENDIF
    IF SEEK(lcCtnSty+lcDyelot+STR(lnOrdLine,6)+lcStySz,loFormSet.lcPckLin,'StySze')
      IF llAlowAdd
        SELECT(loFormSet.lcPckLin)
        REPLACE PQty WITH PQty   + lnStyQty,;
          PWgh WITH PWgh   + lnWgh   ,;
          OQty WITH OrdQty - PQty,;
          cOPen WITH IIF(OQty > 0 ,'T','N')
        REPLACE CtnQty     WITH IIF(!loFormSet.llClrSel AND lSelect,EVALUATE(loFormSet.lcPckLin+'.OQty'),0),;
          CtnTotQty  WITH CtnQty,;
          lSelect    WITH IIF(!loFormSet.llClrSel AND lSelect,.T.,.F.),;
          SELECTED   WITH IIF(!loFormSet.llClrSel,SELECTED,.F.)
        = RLOCK(loFormSet.lcPckLin)
        UNLOCK IN (loFormSet.lcPckLin)
      ENDIF
    ENDIF

    *B609658,1 WAM 08/09/2011 Update pack weight
    IF lUpcPack
      IF llUpcAdd  AND SEEK(lcPack_ID,loFormSet.lcSumPck,loFormSet.lcSumPck)
        REPLACE PTotWgh WITH PtotWgh+ lnWgh IN (loFormSet.lcSumPck)
      ENDIF
    ELSE
      *B609658,1 WAM 08/09/2011 (End)
      SET ORDER TO lcSumPkLn IN (loFormSet.lcSumPck)
      IF SEEK(lcCtnSty+lcDyelot+STR(lnOrdLine,6)+PADR(lcStyNo,3),loFormSet.lcSumPck)
        IF llAlowAdd
          SELECT(loFormSet.lcSumPck)
          REPLACE PTotQty WITH EVALUATE(loFormSet.lcPckLin+'.PQty'),;
            PTotWgh WITH EVALUATE(loFormSet.lcPckLin+'.PWgh')  ,;
            OTotQty WITH EVALUATE(loFormSet.lcPckLin+'.OQty'),;
            cOPen   WITH IIF(OTotQty > 0 ,'T','N')
          = RLOCK(loFormSet.lcPckLin)
          UNLOCK IN (loFormSet.lcPckLin)
        ENDIF
      ENDIF
      *B609658,1 WAM 08/09/2011 Update pack weight
    ENDIF
    *B609658,1 WAM 08/09/2011 (End)

    *! B608342,1 MMT 11/01/2007 Add Scan option to the manual packing list[Start]
  ELSE
    *C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [Begin]
    *IF (SEEK(STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4)+lcCtnSty+lcDyelot+STR(lnOrdLine,6)+lcStySz,loFormSet.lcCtnDtl,'StySze'))
    &&AND lnStyQty <> 0)
    IF (SEEK(STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4)+lcCtnSty+lcDyelot+STR(lnOrdLine,6)+lcStySz,loFormSet.lcCtnDtl,'StySze') ;
        AND lnStyQty <> 0)
      *C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [eND]
      llAlowAdd = .T.
      SELECT(loFormSet.lcCtnDtl)


      *! B608518,1 MMT 04/17/2008 Fix bug of Wrong Line Qty when Editing Packing List [Start]
      *REPLACE STYLE      WITH lcCtnSty ,;
      Dyelot     WITH lcDyelot ,;
      SIZE       WITH lcStySz  ,;
      cSizeno    WITH lcStyno  ,;
      Qty        WITH Qty+lnStyQty ,;
      Weight     WITH lnWgh    ,;
      nOrdLineNo WITH lnOrdLine,;
      Br         WITH .T.,;
      SzCnt      WITH EVALUATE(loFormSet.lcPackLines+'.SzCnt'),;
      OrgWgh     WITH EVALUATE(loFormSet.lcPackLines+'.StyWgh'),;
      cStatus    WITH "M",;
      TotQty     WITH TotQty+lnStyQty,;
      TotWeight  WITH lnWgh

      REPLACE STYLE      WITH lcCtnSty ,;
        Dyelot     WITH lcDyelot ,;
        SIZE       WITH lcStySz  ,;
        cSizeno    WITH lcStyno  ,;
        Qty        WITH Qty + lnStyQty ,;
        Weight     WITH Weight + lnWgh    ,;
        nOrdLineNo WITH lnOrdLine,;
        Br         WITH .T.,;
        SzCnt      WITH EVALUATE(loFormSet.lcPackLines+'.SzCnt'),;
        OrgWgh     WITH EVALUATE(loFormSet.lcPackLines+'.StyWgh'),;
        cStatus    WITH "M",;
        TotQty     WITH TotQty+lnStyQty,;
        TotWeight  WITH TotWeight + lnWgh
      *! B608518,1 MMT 04/17/2008 Fix bug of Wrong Line Qty when Editing Packing List [End]

      IF loFormSet.ActiveMode = 'E' AND EMPTY(PackLineNo)

        *! B608518,1 MMT 04/17/2008 Fix bug of Wrong Line Qty when Editing Packing List [Start]
        loFormSet.lnLastNo = loFormSet.lnLastNo + 1
        *! B608518,1 MMT 04/17/2008 Fix bug of Wrong Line Qty when Editing Packing List [End]

        REPLACE PackLineNo  WITH loFormSet.lnLastNo
      ENDIF

      IF llUpcAdd
        REPLACE Cupc WITH lcUpc
      ENDIF

      *! B608518,1 MMT 04/17/2008 Fix bug of Wrong Line Qty when Editing Packing List [Start]
      *!*        loFormSet.lnPackWgh = loFormSet.lnPackWgh + EVAL(loFormSet.lcCtnDtl+'.Weight')
      *!*        loFormSet.lnPackQty = loFormSet.lnPackQty + EVAL(loFormSet.lcCtnDtl+'.Qty')
      loFormSet.lnPackWgh = loFormSet.lnPackWgh + lnWgh
      loFormSet.lnPackQty = loFormSet.lnPackQty + lnStyQty
      *! B608518,1 MMT 04/17/2008 Fix bug of Wrong Line Qty when Editing Packing List [End]

      SELECT(loFormSet.lcCtnHdr)

      *! B608518,1 MMT 04/17/2008 Fix bug of Wrong Line Qty when Editing Packing List [Start]
      *REPLACE TotPcs WITH TotPcs + EVAL(loFormSet.lcCtnDtl+'.Qty'),;
      TotWgh WITH TotWgh + EVAL(loFormSet.lcCtnDtl+'.Weight'),;
      EMPTY  WITH IIF(TotPcs>0,'N','Y')
      REPLACE TotPcs WITH TotPcs + lnStyQty ,;
        TotWgh WITH TotWgh + lnWgh    ,;
        EMPTY  WITH IIF(TotPcs>0,'N','Y')
      *! B608518,1 MMT 04/17/2008 Fix bug of Wrong Line Qty when Editing Packing List [End]

      = RLOCK(loFormSet.lcCtnHdr)
      UNLOCK IN (loFormSet.lcCtnHdr)
      SELECT(loFormSet.lcCtnDtl)

      IF SEEK(lcCtnSty+lcDyelot+STR(lnOrdLine,6)+lcStySz,loFormSet.lcPckLin,'StySze')
        IF llAlowAdd
          SELECT(loFormSet.lcPckLin)
          REPLACE PQty  WITH PQty   + lnStyQty,;
            PWgh  WITH PWgh   + lnWgh   ,;
            OQty  WITH OrdQty - PQty,;
            cOPen WITH IIF(OQty > 0 ,'T','N')
          REPLACE CtnQty    WITH IIF(!loFormSet.llClrSel AND lSelect,EVALUATE(loFormSet.lcPckLin+'.OQty'),0),;
            CtnTotQty WITH CtnQty,;
            lSelect   WITH IIF(!loFormSet.llClrSel AND lSelect,.T.,.F.),;
            SELECTED  WITH IIF(!loFormSet.llClrSel,SELECTED,.F.)
          = RLOCK(loFormSet.lcPckLin)
          UNLOCK IN (loFormSet.lcPckLin)
        ENDIF
      ENDIF
      *B609658,1 WAM 08/09/2011 Update pack weight
      IF lUpcPack
        IF llUpcAdd  AND SEEK(lcPack_ID,loFormSet.lcSumPck,loFormSet.lcSumPck)
          REPLACE PTotWgh WITH PtotWgh+ lnWgh IN (loFormSet.lcSumPck)
        ENDIF
      ELSE
        *B609658,1 WAM 08/09/2011 (End)

        SET ORDER TO lcSumPkLn IN (loFormSet.lcSumPck)
        IF SEEK(lcCtnSty+lcDyelot+STR(lnOrdLine,6)+PADR(lcStyNo,3),loFormSet.lcSumPck)
          IF llAlowAdd
            SELECT(loFormSet.lcSumPck)

            *! B608518,1 MMT 04/17/2008 Fix bug of Wrong Line Qty when Editing Packing List [Start]
            *REPLACE PTotQty WITH EVALUATE(loFormSet.lcPckLin+'.PQty'),;
            PTotWgh WITH EVALUATE(loFormSet.lcPckLin+'.PWgh')  ,;
            OTotQty WITH EVALUATE(loFormSet.lcPckLin+'.OQty'),;
            cOPen   WITH IIF(OTotQty > 0 ,'T','N')
            REPLACE PTotQty WITH PTotQty + lnStyQty,;
              PTotWgh WITH PTotWgh + lnWgh    ,;
              OTotQty WITH EVALUATE(loFormSet.lcPckLin+'.OQty'),;
              cOPen   WITH IIF(OTotQty > 0 ,'T','N')
            *! B608518,1 MMT 04/17/2008 Fix bug of Wrong Line Qty when Editing Packing List [End]

            = RLOCK(loFormSet.lcPckLin)
            UNLOCK IN (loFormSet.lcPckLin)
          ENDIF
        ENDIF
        *B609658,1 WAM 08/09/2011 Update pack weight
      ENDIF
      *B609658,1 WAM 08/09/2011 (End)

      *=SEEK(STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4)+lcCtnSty+lcDyelot+STR(lnOrdLine,6)+lcStySz,loFormSet.lcCtnDtl,'StySze')
    ENDIF
    *! B608342,1 MMT 11/01/2007 Add Scan option to the manual packing list[End]
  ENDIF

  *B608882,1 TMI 05/31/2009 09:08:39 AM [Start] update the total wight of the carton being updated
  IF llUpcAdd
    LOCAL lcStyKey,lcCrtnNo,lnTotWeight
    SELECT (loFormSet.lcCtnDtl)
    lcStyKey = STR(CART_NO,4)+STYLE+DYELOT+STR(NORDLINENO,6)+CSIZENO
    lcCrtnNo = STR(CART_NO,4)
    =SEEK(lcCrtnNo)
    lnTotWeight = 0
    SUM TotWeight TO lnTotWeight REST WHILE STR(CART_NO,4)+STYLE+DYELOT+STR(NORDLINENO,6)+CSIZENO = lcCrtnNo
    =SEEK(lcStyKey)
    loFormSet.AriaForm1.pgfPacking.CartonInfo.txtTotWghH.VALUE = lnTotWeight
  ENDIF
  *B608882,1 TMI 05/31/2009 09:08:40 AM [End  ]

  *B609658,1 WAM 08/09/2011 Process all lines in the scanned pack
ENDSCAN
*B609658,1 WAM 08/09/2011 (End)


*! B608342,1 MMT 11/01/2007 Add Scan option to the manual packing list[Start]
lfSetControlSource(loFormSet)
*! B608342,1 MMT 11/01/2007 Add Scan option to the manual packing list[End]

loFormSet.AriaForm1.pgfPacking.HEADER.txtWeight.VALUE  = loFormSet.lnPackWgh
loFormSet.AriaForm1.pgfPacking.HEADER.txtPieces.VALUE  = loFormSet.lnPackQty
loFormSet.AriaForm1.pgfPacking.CartonInfo.kbStyle.VALUE = lcCtnSty
loFormSet.AriaForm1.pgfPacking.CartonInfo.kbConfiguration.VALUE = lcDyelot
loFormSet.AriaForm1.pgfPacking.CartonInfo.kbSize.VALUE =lcStySz

=lfwCtnDtlBr(loFormSet)
loFormSet.AriaForm1.pgfPacking.CartonInfo.grdCartonD.REFRESH

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfStyBrow
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
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
  [SzCode =EVAL('cSizeCod'):H='Size',]+;
  [SzWgh  =StyWgh     :H='Wgh\Unit',]+;
  [Open   =EVAL('OQty'):H='Open Quantity']
lcFile_Ttl  = 'Style Open Quantities'
lcForExp = "FOR " + IIF(loFormSet.llShwOpn,"EVAL('OQty')>0 AND EVAL('AvlQty')>0","EVAL('AvlQty')>0")

SELECT (loFormSet.lcPackLines)

DECLARE laTemp[1]

llReturn  = gfBrows(lcForExp,'Style','laTemp')

SELECT(lnCurAlias)

RETURN llReturn

*!*************************************************************
*! Name      : lfCrSavFiles
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : Create the files that are used in Function lfMSavScr
*!             (lcTmPckLin) (lcTmCtnDtl)
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfCrSavFiles()
*!*************************************************************
FUNCTION lfCrSavFiles
LPARAMETERS loFormSet

PRIVATE lnCurAlias,lnI
lnCurAlias = SELECT(0)

lnI = 1

DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lSelect1'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lSelect2'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lSelect3'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lSelect4'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lSelect5'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lSelect6'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lSelect7'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lSelect8'
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
laFileStru[lnI,1] = 'Dyelot'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 10
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
laFileStru[lnI,1] = 'OrgPQty1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgPQty2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgPQty3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgPQty4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgPQty5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgPQty6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgPQty7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgPQty8'
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

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd1'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd2'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd3'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd4'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd5'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd6'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0
lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd7'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'OrgOrd8'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0


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


lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'cCrtnVlTyp'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 6
laFileStru[lnFileStru+1,4] = 0


lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'Piktkt'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 6
laFileStru[lnFileStru+1,4] = 0


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
laFileStru[lnI,1] = 'nPackNo'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'nPkPack'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'lRange'
laFileStru[lnI,2] = 'L'
laFileStru[lnI,3] = 1
laFileStru[lnI,4] = 0

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'PrePak'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'PpQty'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 4
laFileStru[lnFileStru+1,4] = 0

DIMENSION laIndx[7,2]
laIndx[1,1] = "Style+Dyelot+STR(nOrdLineNo,6)"
laIndx[1,2] = loFormSet.lcTmPckLin

laIndx[2,1] = "IIF(Selected>0,'Y','N')"
laIndx[2,2] = 'Selected'

laIndx[3,1] = "IIF(OQty1+OQty2+OQty3+OQty4+OQty5+OQty6+OQty7+OQty8>0,'Y','N')"
laIndx[3,2] = 'Opened'

laIndx[4,1] = "IIF(PQty1+PQty2+PQty3+PQty4+PQty5+PQty6+PQty7+PQty8=0,'Y','N')"
laIndx[4,2] = 'NoPacked'

laIndx[5,1] = "STR(nOrdLineNo,6)+Style+Dyelot"
laIndx[5,2] = 'LineNOSty'

laIndx[6,1] = "PACK_ID+cPkColor+cPckSize+cPkVersion+STR(nOrdLineNo,6)+Style+Dyelot"
laIndx[6,2] = 'PakIndxLn'

laIndx[7,1] = "PACK_ID+cPkColor+cPckSize+cPkVersion+Style+Dyelot+STR(nOrdLineNo,6)"
laIndx[7,2] = 'PakIndxSt'

=gfCrtTmp(loFormSet.lcTmPckLin,@laFileStru,@laIndx)
SET ORDER TO loFormSet.lcTmPckLin IN (loFormSet.lcTmPckLin)

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
laFileStru[lnI,1] = 'Dyelot'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 10
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
laFileStru[lnI,1] = 'nPackNO'
laFileStru[lnI,2] = 'N'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'PrePak'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'PpQty'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 4
laFileStru[lnFileStru+1,4] = 0

DIMENSION laIndx[2,2]

laIndx[1,1] = "STR(Cart_No,4)+Style+Dyelot+STR(nOrdLineNo,6)"
laIndx[1,2] = loFormSet.lcTmCtnDtl
laIndx[2,1] = "cStatus"
laIndx[2,2] = "Status"

=gfCrtTmp(loFormSet.lcTmCtnDtl,@laFileStru,@laIndx)
SET ORDER TO loFormSet.lcTmCtnDtl IN (loFormSet.lcTmCtnDtl)


*!*************************************************************
*! Name      : lfUpdSavFle
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : Create the files that are used in Function lfMSavScr
*!             (lcTmPckLin) (lcTmCtnDtl)
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfUpdSavFle()
*!*************************************************************
FUNCTION lfUpdSavFle
LPARAMETERS loFormSet
*-- Update Pack detail file
SELECT (loFormSet.lcPckLin)
SCAN
  SCATTER MEMVAR MEMO
  SELECT (loFormSet.lcTmPckLin)
  =SEEK(EVALUATE(loFormSet.lcPckLin+'.Style')+EVALUATE(loFormSet.lcPckLin+'.Dyelot')+STR(EVALUATE(loFormSet.lcPckLin+'.nOrdLineNo'),6),loFormSet.lcTmPckLin)
  LOCATE REST WHILE STYLE+Dyelot+STR(nOrdLineNo,6)=EVALUATE(loFormSet.lcPckLin+'.Style')+EVALUATE(loFormSet.lcPckLin+'.Dyelot')+STR(EVALUATE(loFormSet.lcPckLin+'.nOrdLineNo'),6) FOR !DELETED()
  IF !FOUND()
    APPEND BLANK
    REPLACE STYLE      WITH m.Style,;
      Dyelot     WITH m.Dyelot,;
      nOrdLineNo WITH m.nOrdLineNo,;
      SCALE      WITH m.Scale,;
      SzCnt      WITH m.SzCnt,;
      StyWgh     WITH m.StyWgh,;
      OrgStyWgh  WITH m.OrgStyWgh,;
      LPicked    WITH m.LPicked,;
      nStep      WITH m.nStep,;
      SELECTED   WITH IIF(m.Selected,1,0),;
      cCrtnVlTyp WITH m.cCrtnVlTyp,;
      Piktkt     WITH m.Piktkt,;
      PACK_ID    WITH m.PACK_ID,;
      cPkVersion WITH m.cPkVersion,;
      cPkColor   WITH m.cPkColor,;
      cPckSize   WITH m.cPckSize,;
      nPackNo    WITH m.nPackNo,;
      nPkPack    WITH m.nPkPack,;
      lRange     WITH m.lRange,;
      Prepak     WITH m.Prepak,;
      PpQty      WITH m.PpQty
  ENDIF
  lcSze = m.cSizeNo
  REPLACE lSelect&lcSze WITH m.lSelect,;
    cSize&lcSze   WITH m.cSizeCod,;
    OrgPQty&lcSze WITH m.OrgPQty,;
    OrgPWgh       WITH m.OrgPWgh,;
    OrdQty&lcSze  WITH m.OrdQty,;
    AvlQty&lcSze  WITH m.AvlQty,;
    OQty&lcSze    WITH m.OQty,;
    CtnQty&lcSze  WITH m.CtnQty,;
    PQty&lcSze    WITH m.PQty,;
    PWgh&lcSze    WITH m.PWgh,;
    OrgOrd&lcSze  WITH m.OrgOrd,;
    nDiff&lcSze   WITH m.nDiff
ENDSCAN


*-- Update Carton Detail File
SELECT (loFormSet.lcCtnDtl)
*E037427,1 WAM 05/05/2005 Fix bug of delete carton lines
SET DELETED OFF
*E037427,1 WAM 05/05/2005 (End)
SET RELATION TO
SET FILTER TO
SET KEY TO
GOTO TOP
SCAN
  SCATTER MEMVAR MEMO
  SELECT (loFormSet.lcTmCtnDtl)
  *E037427,1 WAM 05/05/2005 Fix bug of delete carton lines
  *IF !SEEK(STR(EVALUATE(loFormSet.lcCtnDtl+'.Cart_No'),4)+EVALUATE(loFormSet.lcCtnDtl+'.Style')+;
  EVALUATE(loFormSet.lcCtnDtl+'.Dyelot')+STR(EVALUATE(loFormSet.lcCtnDtl+'.nOrdLineNo'),6),loFormSet.lcTmCtnDtl)
  =SEEK(STR(EVALUATE(loFormSet.lcCtnDtl+'.Cart_No'),4)+EVALUATE(loFormSet.lcCtnDtl+'.Style')+;
    EVALUATE(loFormSet.lcCtnDtl+'.Dyelot')+STR(EVALUATE(loFormSet.lcCtnDtl+'.nOrdLineNo'),6),loFormSet.lcTmCtnDtl)


  LOCATE REST WHILE STR(Cart_No,4)+STYLE+Dyelot+STR(nOrdLineNo,6) = ;
    STR(EVALUATE(loFormSet.lcCtnDtl+'.Cart_No'),4)+EVALUATE(loFormSet.lcCtnDtl+'.Style')+;
    EVALUATE(loFormSet.lcCtnDtl+'.Dyelot')+STR(EVALUATE(loFormSet.lcCtnDtl+'.nOrdLineNo'),6) FOR !DELETED()

  IF !FOUND()
    *E037427,1 WAM 05/05/2005 (End)

    APPEND BLANK
    REPLACE Cart_No    WITH m.Cart_No,;
      STYLE      WITH m.Style,;
      Dyelot     WITH m.Dyelot,;
      nOrdLineNo WITH m.nOrdLineNo,;
      SzCnt      WITH m.SzCnt,;
      OrgWgh     WITH m.OrgWgh,;
      nStep      WITH m.nStep,;
      cStatus    WITH m.cStatus,;
      PackLineNo WITH m.PackLineNo,;
      PACK_ID    WITH m.PACK_ID,;
      cPkVersion WITH m.cPkVersion,;
      cPkColor   WITH m.cPkColor,;
      cPckSize   WITH m.cPckSize,;
      nPackNo    WITH m.nPackNo,;
      Prepak     WITH m.Prepak,;
      PpQty      WITH m.PpQty

    *! B608518,1 MMT 04/17/2008 Fix bug of Wrong Line Qty when Editing Packing List [Start]
  ELSE
    IF (PackLineNo = 0 AND m.PackLineNo <> 0) OR ((PackLineNo > 0) AND PackLineNo > m.PackLineNo)
      REPLACE PackLineNo WITH m.PackLineNo
    ENDIF
    *! B608518,1 MMT 04/17/2008 Fix bug of Wrong Line Qty when Editing Packing List [End]

  ENDIF
  lcSze = m.cSizeNO

  *! B608664,1 MMT 08/26/2008 Fix bug of packing deleted sizes[Start]
  *REPLACE SIZE&lcSze   WITH m.Size,;
  Qty&lcSze    WITH m.Qty;
  Weight&lcSze WITH m.Weight,;
  Br&lcSze     WITH m.Br

  REPLACE SIZE&lcSze   WITH IIF(m.cStatus = 'D',"",m.Size),;
    Qty&lcSze    WITH IIF(m.cStatus = 'D',0,m.Qty);
    Weight&lcSze WITH IIF(m.cStatus = 'D',0,m.Weight),;
    Br&lcSze     WITH IIF(m.cStatus = 'D',.F.,m.Br)
  *! B608664,1 MMT 08/26/2008 Fix bug of packing deleted sizes[End]

  IF cStatus = 'D'
    DELETE
  ENDIF
ENDSCAN
*E037427,1 WAM 05/05/2005 Fix bug of delete carton lines
SET DELETED ON
*E037427,1 WAM 05/05/2005 (End)

*!*************************************************************
*! Name      : lfBefSavscr
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
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
*! Example   : DO lfBefSavscr
*!*************************************************************
FUNCTION lfBefSavscr
LPARAMETERS loFormSet

lcPckLin  = loFormSet.lcPckLin
lcCtnDtl  = loFormSet.lcCtnDtl
lcCtnHdr  = loFormSet.lcCtnHdr
lcPkTktNo = PADR(loFormSet.AriaForm1.kbPkTktNo.keytextbox.VALUE ,6)
llReturn  = .T.

SELECT (lcCtnDtl)
lcCtDtRel = SET("RELATION")
SET RELATION TO
SET FILTER TO
SET KEY TO
GOTO TOP
IF EOF() OR BOF() OR DELETED()
  *--No Packing list lines were applied. Cannot proceed!
  *-- <OK>
  llNoThing = gfModalGen("INM44035B00000","Dialog")
  llReturn = .F.
ELSE
  *---------------------------------------------------
  *-------- Check for availability of picking --------
  *E037427,1 WAM 05/05/2005 UPdate pikced quantity if piktkt selected
  *-- IF user want to update pick quantity and does not have access to force allocation
  *IF loFormSet.llUpdtPkTk
  *  *-- if no Sufficient quantity.
  *  IF lfNoSuffic(loFormSet)
  *    RETURN .F.
  *  ENDIF
  *ENDIF
  *E037427,1 WAM 05/05/2005 (End)


  *============================================================
  *This part for updating allocated quantities in style,stydye files
  *and picked quantities in ordline file if the created pack list is
  *made by piktkt
  IF !EMPTY(lcPkTktNo)

    *E037427,1 WAM 05/05/2005 UPdate pikced quantity if piktkt selected
    *-- IF user want to update pick quantity and does not have access to force allocation
    IF loFormSet.llUpdtPkTk
      *-- if no Sufficient quantity.
      IF lfNoSuffic(loFormSet)
        RETURN .F.
      ENDIF
    ENDIF
    *E037427,1 WAM 05/05/2005 (End)

    SELECT(lcPckLin)
    lcTag = ORDER(lcPckLin)
    SET ORDER TO NoPacked
    IF SEEK('Y',lcPckLin)
      *-- "All unselected lines from the picking ticket will be released."
      *-- <Release> <Resume packing> <Save as is>

      *E037427,1 WAM 05/05/2005 Resume if user select
      *IF loFormSet.lnRelCho = 0      && uncompelete session
      *  loFormSet.lnRelCho = gfModalGen("QRM44039B44006","Dialog")
      *ENDIF
      loFormSet.lnRelCho = gfModalGen("QRM44039B44006","Dialog")
      llReturn = loFormSet.lnRelCho <> 2
      *E037427,1 WAM 05/05/2005 (End)
    ENDIF
    SET ORDER TO lcTag IN (lcPckLin)
    *--C102239,1 (Begin) Custom : Update ALPAKINF file for Bel05.
    IF ASCAN(loFormSet.laEvntTrig,PADR('UPDPACK',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
      =loFormSet.mDoTrigger(PADR('UPDPACK',10))
    ENDIF
    *--C102239,1 (End)
  ENDIF
ENDIF
RETURN llReturn

*!*************************************************************
*! Name      : lfMSavScr
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
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
*! Example   : DO lfMSavScr
*!*************************************************************
FUNCTION lfmSavScr
LPARAMETERS loFormSet

*-- lnBookQty,lnBookAmt these variables are to update book,bookamt fields in
*-- ordhdr file
PRIVATE llReturn,lnCurAlias,lnBookQty,lnBookAmt,;
  lnOpenQty,lnOpenAmt,lcPkTktNo ,lcOrder , lcAccount , lcStore , lcPackNo

PRIVATE llStyDyOpn
SELECT (loFormSet.lcPckLin)
SET FILTER TO

=lfCrSavFiles(loFormSet)
=lfUpdSavFle(loFormSet)
SELECT PACK_LIN
SET RELATION TO
SET KEY TO
SET FILTER TO

*B608972,1[T20080821.0007]MMT 08/13/2009 Change order to avoid blank lines[Start]
loFormSet.PACK_HDR.SetOrder('PACK_HDR')
*B608972,1[T20080821.0007]MMT 08/13/2009 Change order to avoid blank lines[End]

*-- Save names of new files which created to be used in saving
lcPcklin   = loFormSet.lcTmPckLin
lcCtnDtl   = loFormSet.lcTmCtnDtl
lcCtnHdr   = loFormSet.lcCtnHdr
lcSumPck   = loFormSet.lcSumPck
lcPackNo   = PADR(loFormSet.AriaForm1.kbPackNo.keytextbox.VALUE,6)
lcOrder    = PADR(loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE,6)
lcAccount  = PADR(loFormSet.ariaForm1.kbAccount.keytextbox.VALUE,5)
lcStore    = PADR(loFormSet.AriaForm1.kbStore.keytextbox.VALUE,8)
lcPkTktNo  = PADR(loFormSet.AriaForm1.kbPkTktNo.keytextbox.VALUE,6)
lcBol      = PADR(loformset.ariaform1.pgfPacking.HEADER.txtbOL.VALUE,6)
lcWareCode = loFormSet.lcWareCode
lcUser_Id  = oAriaApplication.User_ID
ldSysDate  = oAriaApplication.SystemDate
lcTime     = gfGettime()
llStyDyOpn = .F.
*E037427,1 WAM 05/05/2005 Do not reset user selection
*STORE 0 To loFormSet.lnRelCho,lnBookQty,lnBookAmt,lnOpenQty,lnOpenAmt
STORE 0 TO lnBookQty,lnBookAmt,lnOpenQty,lnOpenAmt
*E037427,1 WAM (End)

lnCurAlias = SELECT(0)

lcDelStat = SET("DELETED")
PRIVATE lnBookDiff , lnQtyDiff , lnBkAmtDif , lnQyAmtDif , lnOrgBook , lnOrgQty
STORE 0 TO lnBookDiff , lnQtyDiff , lnBkAmtDif , lnQyAmtDif, lnOrgBook , lnOrgQty
SELECT (lcPcklin)
SET FILTER TO

SELECT (lcCtnDtl)
lcCtDtRel = SET("RELATION")
SET RELATION TO
SET FILTER TO
SET KEY TO
GOTO TOP


*!B610281,1 HIA 03/24/2013 Aria XP - AR - Scan pick ticket invoice not updating style and stydye files, T20130117.0010 [Start]
*IF loFormSet.lnRelCho <> 2
IF loFormSet.lnRelCho <> 2 AND !(loformset.activemode ='E' AND Pack_hdr.STATUS ='C')
  *!B610281,1 HIA 03/24/2013 Aria XP - AR - Scan pick ticket invoice not updating style and stydye files, T20130117.0010 [End]

  SELECT(lcPckLin)
  SCAN
    llNoThing  = loFormSet.OrdLine.SEEK('O'+lcOrder+lcStore+&lcPckLin..STYLE+STR(nOrdLineNo,6))
    IF llNoThing AND &lcPckLin..nStep = 0      && uncompelete session
      SELECT OrdLine
      FOR lnI = 1 TO &lcPckLin..SzCnt
        lcI = STR(lnI,1)
        loFormSet.OrdLine.REPLACE("nPck&lcI  WITH &lcPckLin..PQty&lcI")
      ENDFOR
      *E037427,1 WAM 05/05/2005 Update packed packs in ordline
      IF SEEK(PADR(PACK_ID,19)+cPkColor+cPckSize+cPkVersion,loFormSet.lcSumPck,loFormSet.lcSumPck)
        loFormSet.OrdLine.REPLACE("nPkPack  WITH &lcSumPck..PTotQty")
      ENDIF
      *E037427,1 WAM 05/05/2005 (End)

      loFormSet.OrdLine.REPLACE("nPWght WITH &lcPckLin..PWgh1 + &lcPckLin..PWgh2 +;
                    					     &lcPckLin..PWgh3 + &lcPckLin..PWgh4 +;
				                             &lcPckLin..PWgh5 + &lcPckLin..PWgh6 +;
                    					       &lcPckLin..PWgh7 + &lcPckLin..PWgh8")
      *B609997,1 MMT 07/11/2012 Update Pick Ticket option updates unpacked lines in Ordline[Start]
      *IF loFormSet.llUpdtPkTk
      IF loFormSet.llUpdtPkTk AND (&lcPckLin..PQty1 <> 0 OR &lcPckLin..PQty2 <> 0 OR &lcPckLin..PQty3 <> 0 OR &lcPckLin..PQty4 <> 0 OR;
          &lcPckLin..PQty5 <> 0 OR &lcPckLin..PQty6 <> 0 OR &lcPckLin..PQty7 <> 0 OR &lcPckLin..PQty8 <> 0)
        *B609997,1 MMT 07/11/2012 Update Pick Ticket option updates unpacked lines in Ordline[End]
        SELECT (lcPckLin)
        FOR lnI = 1 TO &lcPckLin..SzCnt
          lcI = STR(lnI,1)
          REPLACE nDiff&lcI WITH PQty&lcI- OrdLine.Pik&lcI
        ENDFOR
        *E037427,1 WAM 05/05/2005 Fix update order picked quantity
        *!*	        SELECT OrdLine
        *!*	        IF !EMPTY(Piktkt)
        *!*	          FOR lnI = 1 TO &lcPckLin..SzCnt
        *!*	            lcI = STR(lnI,1)
        *!*	            loFormSet.OrdLine.REPLACE("Pik&lcI  WITH nPck&lcI")
        *!*	          ENDFOR
        *!*	          loFormSet.OrdLine.REPLACE("TotPik  WITH Pik1 + Pik2 + Pik3 + Pik4 +Pik5 + Pik6 + Pik7 + Pik8 ")
        *!*	        ENDIF
        *E037427,1 WAM 05/05/2005 (End)
      ENDIF


      *-- If we are packing from piktkt
      IF !EMPTY(lcPkTktNo)
        *-- update pik fields in ordline file only
        *-- If user option release or (save as is and the line is selected)
        *-- which means if it is unselected line and "save as is" left the
        *-- pik quantities as it is.
        *E037427,1 WAM 05/05/2005 Fix update order picked quantity
        *!*	        IF loFormSet.lnRelCho = 1 OR ;
        *!*	           (loFormSet.lnRelCho = 3  AND ;
        *!*	               !( EMPTY(&lcPckLin..cSelect1) OR EMPTY(&lcPckLin..cSelect2) OR ;
        *!*	                  EMPTY(&lcPckLin..cSelect3) OR EMPTY(&lcPckLin..cSelect4) OR ;
        *!*	                  EMPTY(&lcPckLin..cSelect5) OR EMPTY(&lcPckLin..cSelect6) OR ;
        *!*	                  EMPTY(&lcPckLin..cSelect7) OR EMPTY(&lcPckLin..cSelect8) ) )
        *!*	          FOR lnI = 1 TO &lcPckLin..SzCnt
        *!*	            lcI = STR(lnI,1)
        *!*	            loFormSet.OrdLine.REPLACE("Pik&lcI   WITH &lcPckLin..PQty&lcI")
        *!*	          ENDFOR
        *!*	          loFormSet.OrdLine.REPLACE("TotPik WITH &lcPckLin..PQty1+&lcPckLin..PQty2+;
        *!*					                                  &lcPckLin..PQty3+&lcPckLin..PQty4+;
        *!*					                                  &lcPckLin..PQty5+&lcPckLin..PQty6+;
        *!*					                                  &lcPckLin..PQty7+&lcPckLin..PQty8")
        *!*	          loFormSet.OrdLine.REPLACE("PikTkt  WITH IIF(TotPik=0,'',PikTkt),;
        *!*	               	                  PikDate WITH IIF(TotPik=0,{},PikDate),;
        *!*	               	                  Picked  WITH IIF(TotPik=0,.F.,Picked)")
        *!*	        ENDIF

        *! B609485,1 MMT 01/05/2011 Fix bug of wrong style allocated qty  and SO Booked Qty[Start]
        loFormSet.STYLE.SEEK(ORDLINE.STYLE,'STYLE')
        loFormSet.StyDye.SEEK(&lcPckLin..STYLE+lcWareCode,'STYDYE')
        *! B609485,1 MMT 01/05/2011 Fix bug of wrong style allocated qty  and SO Booked Qty[END]

        FOR lnI = 1 TO &lcPckLin..SzCnt
          lcI = STR(lnI,1)
          *! B609485,1 MMT 01/05/2011 Fix bug of wrong style allocated qty  and SO Booked Qty[Start]
          lnOldLPikQty = ORDLINE.Pik&lcI
          *! B609485,1 MMT 01/05/2011 Fix bug of wrong style allocated qty  and SO Booked Qty[End]
          DO CASE
            *B609997,1 MMT 07/11/2012 Update Pick Ticket option updates unpacked lines in Ordline[Start]
            *CASE &lcPckLin..lSelect&lcI AND loFormSet.llUpdtPkTk
          CASE loFormSet.llUpdtPkTk AND (&lcPckLin..PQty1 <> 0 OR &lcPckLin..PQty2 <> 0 OR &lcPckLin..PQty3 <> 0 OR;
              &lcPckLin..PQty4 <> 0 OR &lcPckLin..PQty5 <> 0 OR &lcPckLin..PQty6 <> 0 OR &lcPckLin..PQty7 <> 0 OR &lcPckLin..PQty8 <> 0)
            *B609997,1 MMT 07/11/2012 Update Pick Ticket option updates unpacked lines in Ordline[End]
            loFormSet.OrdLine.REPLACE("Pik&lcI WITH &lcPckLin..PQty&lcI")
          CASE !&lcPckLin..lSelect&lcI AND loFormSet.lnRelCho = 1
            loFormSet.OrdLine.REPLACE("Pik&lcI WITH &lcPckLin..PQty&lcI")
          ENDCASE
          *B609997,1 MMT 07/11/2012 Update Pick Ticket option updates unpacked lines in Ordline[Start]
          IF !loFormSet.llUpdtPkTk OR EMPTY(lcPkTktNo) OR !(&lcPckLin..PQty1 <> 0 OR &lcPckLin..PQty2 <> 0 OR &lcPckLin..PQty3 <> 0 OR;
              &lcPckLin..PQty4 <> 0 OR &lcPckLin..PQty5 <> 0 OR &lcPckLin..PQty6 <> 0 OR &lcPckLin..PQty7 <> 0 OR &lcPckLin..PQty8 <> 0)
            *B609997,1 MMT 07/11/2012 Update Pick Ticket option updates unpacked lines in Ordline[END]
            *! B609485,1 MMT 01/05/2011 Fix bug of wrong style allocated qty  and SO Booked Qty[Start]
            loFormSet.STYLE.REPLACE("ALO&lcI WITH ALO&lcI -lnOldLPikQty +ORDLINE.Pik&lcI")
            loFormSet.StyDye.REPLACE("ALO&lcI WITH ALO&lcI -lnOldLPikQty +ORDLINE.Pik&lcI")
            *! B609485,1 MMT 01/05/2011 Fix bug of wrong style allocated qty  and SO Booked Qty[End]
            *B609997,1 MMT 07/11/2012 Update Pick Ticket option updates unpacked lines in Ordline[Start]
          ENDIF
          *B609997,1 MMT 07/11/2012 Update Pick Ticket option updates unpacked lines in Ordline[END]
        ENDFOR
        loFormSet.OrdLine.REPLACE("TotPik WITH Pik1+ Pik2+ Pik3+ Pik4+ Pik5+ Pik6+ Pik7+ Pik8")
        loFormSet.OrdLine.REPLACE("PikTkt  WITH IIF(TotPik=0,'',PikTkt),;
             	                  PikDate WITH IIF(TotPik=0,{},PikDate),;
               	                  Picked  WITH IIF(TotPik=0,.F.,Picked)")
        *E037427,1 WAM 05/05/2005 (End)
        *! B609485,1 MMT 01/05/2011 Fix bug of wrong style allocated qty  and SO Booked Qty[Start]
        loFormSet.STYLE.REPLACE("TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8")
        loFormSet.StyDye.REPLACE("TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8")
        *! B609485,1 MMT 01/05/2011 Fix bug of wrong style allocated qty  and SO Booked Qty[End]
      ENDIF
      *C102239,1 (Begin) Custom : Update ALPAKINF file for Bel05.
      IF ASCAN(loFormSet.laEvntTrig,PADR('UPDTOT',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
        =loFormSet.mDoTrigger(PADR('UPDTOT',10))
      ENDIF
      *C102239,1 (End)
      SELECT (lcPckLin)
      REPLACE &lcPckLin..nStep WITH 1
    ENDIF
    *--  updating the style file.
    IF &lcPckLin..nStep = 1      && uncompelete session
      SELECT STYLE
      *B609997,1 MMT 07/11/2012 Update Pick Ticket option updates unpacked lines in Ordline[Start]
      *IF loFormSet.llUpdtPkTk AND !EMPTY(lcPkTktNo) AND loFormSet.STYLE.SEEK(&lcPckLin..STYLE)
      IF loFormSet.llUpdtPkTk AND !EMPTY(lcPkTktNo) AND loFormSet.STYLE.SEEK(&lcPckLin..STYLE) AND;
          (&lcPckLin..PQty1 <> 0 OR &lcPckLin..PQty2 <> 0 OR &lcPckLin..PQty3 <> 0 OR;
          &lcPckLin..PQty4 <> 0 OR &lcPckLin..PQty5 <> 0 OR &lcPckLin..PQty6 <> 0 OR &lcPckLin..PQty7 <> 0 OR &lcPckLin..PQty8 <> 0)
        *B609997,1 MMT 07/11/2012 Update Pick Ticket option updates unpacked lines in Ordline[END]
        FOR lnI = 1 TO &lcPckLin..SzCnt
          lcI = STR(lnI,1)
          loFormSet.STYLE.REPLACE("Alo&lcI   WITH Alo&lcI + &lcPckLin..nDiff&lcI")
        ENDFOR
        loFormSet.STYLE.REPLACE("TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8")
      ENDIF
      *-- updaing ord fields in style file in all cases not only piktkt
      FOR lnI = 1 TO &lcPckLin..SzCnt
        lcI = STR(lnI,1)
        loFormSet.STYLE.REPLACE("Ord&lcI   WITH IIF(&lcPckLin..PQty&lcI>OrdLine.Qty&lcI,Ord&lcI+(&lcPckLin..PQty&lcI-OrdLine.Qty&lcI),Ord&lcI)")
      ENDFOR
      loFormSet.STYLE.REPLACE("TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8")
      SELECT (lcPckLin)
      REPLACE &lcPckLin..nStep WITH 2
    ENDIF


    *--  updating the stydye file.
    IF &lcPckLin..nStep = 2      && uncompelete session
      IF loFormSet.StyDye.SEEK(&lcPckLin..STYLE+lcWareCode)
        *B609997,1 MMT 07/11/2012 Update Pick Ticket option updates unpacked lines in Ordline[Start]
        *IF loFormSet.llUpdtPkTk AND !EMPTY(lcPkTktNo)
        IF loFormSet.llUpdtPkTk AND !EMPTY(lcPkTktNo)AND (&lcPckLin..PQty1 <> 0 OR &lcPckLin..PQty2 <> 0 OR &lcPckLin..PQty3 <> 0 OR;
            &lcPckLin..PQty4 <> 0 OR &lcPckLin..PQty5 <> 0 OR &lcPckLin..PQty6 <> 0 OR &lcPckLin..PQty7 <> 0 OR &lcPckLin..PQty8 <> 0)
          *B609997,1 MMT 07/11/2012 Update Pick Ticket option updates unpacked lines in Ordline[End]
          FOR lnI = 1 TO &lcPckLin..SzCnt
            lcI = STR(lnI,1)
            loFormSet.STYDYE.REPLACE("Alo&lcI WITH Alo&lcI + &lcPckLin..nDiff&lcI")
          ENDFOR
          loFormSet.STYDYE.REPLACE("TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8")
        ENDIF

        *-- Update ord quantities in stydye file.
        FOR lnI = 1 TO &lcPckLin..SzCnt
          lcI = STR(lnI,1)
          loFormSet.STYDYE.REPLACE("Ord&lcI WITH Ord&lcI + MAX(&lcPckLin..PQty&lcI-OrdLine.Qty&lcI,0)")
        ENDFOR
        loFormSet.STYDYE.REPLACE("TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8")
      ENDIF
      SELECT (lcPckLin)
      REPLACE &lcPckLin..nStep WITH 3
    ENDIF

    IF &lcPckLin..nStep = 3      && uncompelete session
      IF (loFormSet.llUseConfg OR loFormSet.llDyelot) AND  STYLE.cDye_Flg = 'Y' AND ;
          loFormSet.StyDye.SEEK(&lcPckLin..STYLE+lcWareCode+&lcPckLin..Dyelot)
        SELECT StyDye
        *B609997,1 MMT 07/11/2012 Update Pick Ticket option updates unpacked lines in Ordline[Start]
        *IF loFormSet.llUpdtPkTk AND !EMPTY(lcPkTktNo)
        IF loFormSet.llUpdtPkTk AND !EMPTY(lcPkTktNo) AND (&lcPckLin..PQty1 <> 0 OR &lcPckLin..PQty2 <> 0 OR &lcPckLin..PQty3 <> 0 OR;
            &lcPckLin..PQty4 <> 0 OR &lcPckLin..PQty5 <> 0 OR &lcPckLin..PQty6 <> 0 OR &lcPckLin..PQty7 <> 0 OR &lcPckLin..PQty8 <> 0)
          *B609997,1 MMT 07/11/2012 Update Pick Ticket option updates unpacked lines in Ordline[End]
          FOR lnI = 1 TO &lcPckLin..SzCnt
            lcI = STR(lnI,1)
            loFormSet.STYDYE.REPLACE("Alo&lcI WITH Alo&lcI + &lcPckLin..nDiff&lcI")
          ENDFOR
          loFormSet.STYDYE.REPLACE("TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8")
        ENDIF
        FOR lnI = 1 TO &lcPckLin..SzCnt
          lcI = STR(lnI,1)
          loFormSet.STYDYE.REPLACE("Ord&lcI WITH Ord&lcI + MAX(&lcPckLin..PQty&lcI-OrdLine.Qty&lcI,0)")
        ENDFOR
        loFormSet.STYDYE.REPLACE("TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8")
        SELECT (lcPckLin)
        REPLACE &lcPckLin..nStep WITH 4
      ENDIF
    ENDIF
  ENDSCAN
ENDIF

*===============end updating style,stydye files===================
*===============this part for updating pack_hdr,bol files=========
IF loFormSet.lnRelCho <> 2
  *-- This is to get the count of only cartons that have contents
  *-- and the last carton no in the pack also that has contents
  *-- Start
  SELECT (lcCtnHdr)
  lcCtHdInd = ORDER(lcCtnHdr)
  SET ORDER TO EMPTY IN (lcCtnHdr)
  IF SEEK ('Y',lcCtnHdr)
    lnRecNo = RECNO()
    SKIP -1
    loFormSet.lnLastCtn = &lcCtnHdr..Cart_No
    GO lnRecNo
    SCAN REST FOR EMPTY = 'Y'
      loFormSet.lnPackCtn = loFormSet.lnPackCtn - 1
    ENDSCAN
  ELSE
    GO BOTTOM
    loFormSet.lnLastCtn = &lcCtnHdr..Cart_No
  ENDIF
  SET ORDER TO lcCtHdInd IN (lcCtnHdr)
  SELECT Pack_Hdr
  IF EMPTY(lcPackNo) OR !loFormSet.Pack_Hdr.SEEK(lcPackNo)
    *-- this part to Handle pack_no
    IF EMPTY(lcPkTktNo)
      =loFormSet.ORDHDR.SEEK('O'+lcOrder)
      lcPackNo = gfSequence('PIKTKT', '', '', ORDHDR.cDivision)
      DO WHILE loFormSet.PACK_HDR.SEEK(lcPackNo)
        lcPackNo = gfSequence('PIKTKT', '', '', ORDHDR.cDivision)
      ENDDO
      IF loFormSet.Pack_Hdr.SEEK(SPACE(6))
        loFormSet.Pack_HDR.REPLACE("PACK_NO WITH lcPackNo")
      ELSE
        loFormSet.PACK_HDR.INSERT('(PACK_NO) VALUES (lcPackNo)')
      ENDIF
      *-- Packing slip saved with # lcPackNo
      *-- <OK>
      llNoThing = gfModalGen("INM44036B00000","Dialog",lcPackNo)
    ELSE
      lcPackNo = lcPkTktNo
      *B607804,1 WAM 10/19/2006 Don't add blank record
      *loFormSet.PACK_HDR.INSERT('(PACK_NO) VALUES (lcPackNo)')
      IF loFormSet.Pack_Hdr.SEEK(SPACE(6))
        loFormSet.Pack_HDR.REPLACE("PACK_NO WITH lcPackNo")
      ELSE
        loFormSet.PACK_HDR.INSERT('(PACK_NO) VALUES (lcPackNo)')
      ENDIF
      *B607804,1 WAM 10/19/2006 (End)

    ENDIF
    SELECT Pack_Hdr
  ENDIF
  loFormSet.lnLastNo = Pack_Hdr.nLastLNo
  loFormSet.Pack_Hdr.SEEK(lcPackNo)
  m.Tot_Wght  = loFormSet.lnPackWgh
  m.Tot_Cart  = loFormSet.lnPackCtn
  m.Tot_Pcs   = loFormSet.lnPackQty
  m.LStandCtn = IIF(loFormSet.lnCtnTyp = 1,.T.,.F.)
  m.CToStorCn = IIF(loFormSet.lnDrctTo = 1,'S','C')
  IF loFormSet.llEdiSys
    IF !EMPTY(lcBol) AND !loFormSet.BOL_LIN.SEEK(lcBol+lcOrder+lcPackNo)
      loFormSet.BOL_LIN.INSERT('(BOL_NO,ORDER,PACK_NO  )  VALUES (lcBol ,lcOrder,lcPackNo)')
    ENDIF
    *-- IF user change BOL then apply delete BOL code
    PRIVATE llTranBol
    *B132257,1 HBG 05/21/2006 Fix bug of not updating Qty,Crt.Wieght in BOL_HDR file [Begin]
    loFormSet.BOL_HDR.SETORDER('BOL_HDR')
    *B132257,1 HBG 05/21/2006 [End]
    llTranBol = .F.
    llTranBol =  (lcBol <> Pack_Hdr.Bill_Ladg) AND lfDelBol(loFormSet)

    IF loFormSet.BOL_HDR.SEEK(lcBol)
      SELECT BOL_HDR
      IF llTranBol
        loFormSet.BOL_HDR.REPLACE("TOT_WGHT WITH TOT_WGHT + m.Tot_Wght ,;
                TOT_CART WITH TOT_CART + m.Tot_Cart ,;
                TOT_PCS  WITH TOT_PCS  +  m.Tot_Pcs")
      ELSE   && Update the quantites of the BOL
        loFormSet.BOL_HDR.REPLACE("TOT_WGHT WITH TOT_WGHT- PACK_HDR.Tot_Wght + m.Tot_Wght ,;
                TOT_CART WITH TOT_CART- PACK_HDR.Tot_Cart + m.Tot_Cart ,;
                TOT_PCS  WITH TOT_PCS- PACK_HDR.Tot_Pcs   + m.Tot_Pcs")
      ENDIF
    ENDIF
    SELECT Pack_Hdr
  ENDIF

  m.Pack_No   = lcPackNo
  m.Order     = lcOrder
  m.Pikttk    = lcPkTktNo
  m.Account   = lcAccount
  m.Store     = lcStore
  m.Note      = loFormSet.Ariaform1.pgfPacking.HEADER.txtNoteH.VALUE
  m.ShipVia   = loFormSet.Ariaform1.pgfPacking.HEADER.cboShipViaH.VALUE
  m.Sp_Inst1  = loFormSet.Ariaform1.pgfPacking.HEADER.txtSpInst1.VALUE
  m.Sp_Inst2  = loFormSet.Ariaform1.pgfPacking.HEADER.txtSpInst2.VALUE
  m.cPkChCode = loFormSet.Ariaform1.pgfPacking.HEADER.txtPackCharCode.VALUE
  m.cPkDsCode = loFormSet.Ariaform1.pgfPacking.HEADER.txtPackDescCode.VALUE

  loFormSet.Pack_Hdr.REPLACE("Pack_No WITH m.Pack_No , Order WITH m.Order , Account WITH m.Account ,;
  						    Store WITH m.Store , Note WITH m.Note , ShipVia WITH m.ShipVia , Tot_Wght WITH m.Tot_Wght")
  loFormSet.Pack_Hdr.REPLACE("Tot_Cart WITH m.Tot_Cart,tot_Pcs WITH m.Tot_Pcs , Sp_inst1 WITH m.Sp_inst1 , SP_inst2 WITH m.Sp_inst2,;
  						    LStandCtn WITh m.LStandCtn , CToStorCn WITh m.CToStorCn , CPkChCode WITH m.CPkChCode")
  loFormSet.Pack_Hdr.REPLACE("CPkDsCode  WITh m.CPkDsCode ,cWareCode WITH lcWareCode,Bill_Ladg WITH lcBol,;
  						    cAdd_user WITH lcUser_Id,dAdd_date WITH ldSysDate, cAdd_time WITH lcTime")

  *! B608336,1 MMT 10/29/2007 fix bug of displaying wrong styles not included in PIKTKT[Start]
  loFormSet.Pack_Hdr.REPLACE("PIKTKT with m.Pikttk")
  *! B608336,1 MMT 10/29/2007 fix bug of displaying wrong styles not included in PIKTKT[End]

  *C102478,1 (Begin) Generate P/K.
  IF ASCAN(loFormSet.laEvntTrig,PADR('UPDPICK',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
    =loFormSet.mDoTrigger(PADR('UPDPICK',10))
  ENDIF
  *C102478,1 (End)

ENDIF
*=================end of updating pack_hdr,BOL Files================
*=================This part for saving pack_lin file================
*!B610281,1 HIA 03/24/2013 Aria XP - AR - Scan pick ticket invoice not updating style and stydye files, T20130117.0010 [Start]
*IF loFormSet.lnRelCho <> 2
IF loFormSet.lnRelCho <> 2 AND !(loformset.activemode ='E' AND Pack_hdr.STATUS ='C')
  *!B610281,1 HIA 03/24/2013 Aria XP - AR - Scan pick ticket invoice not updating style and stydye files, T20130117.0010 [End]

  *E302315,1 WLD 10/16/2006 Using visual label report [Begin]
  *Check if EDI installed or not
  *Update the new file EDICRTSQ , abd thenew field Ucc9 in EDIACPRT
  *Add new setup of including P\L number in UCC
  IF loFormSet.llEdiSys
    IF !loFormSet.llIncPLNum
      lcSetDele = SET('DELETE')
      SET DELETE ON
      loFormSet.EDICRTSQ.SetOrder('PCKCRTSQ')
      IF loFormSet.EDICRTSQ.SEEK(lcPackNo)
        SELECT EDICRTSQ
      ENDIF
      SET DELETE &lcSetDele
    ENDIF
  ENDIF
  *E302315,1 WLD 10/16/2006 Using visual label report [End]
  SET DELETED OFF
  SELECT(lcCtnDtl)
  SET FILTER TO
  SET ORDER TO 0
  =loFormSet.OrdHdr.SEEK('O'+lcOrder)

  SCAN FOR cStatus <> 'S'
    IF &lcCtnDtl..nStep = 0   && for uncomplete session
      SELECT Pack_Lin
      IF loFormSet.Pack_Lin.SEEK(lcPackNo+STR(&lcCtnDtl..Cart_No,4)+&lcCtnDtl..STYLE)
        IF DELETED('Pack_Lin')
          *E037427,1 WAM 05/05/2005 Enhance performance
          *LOCATE REST FOR pack_no+STR(no_cart,4)+style = ;
          lcPackNo+STR(&lcCtnDtl..Cart_No,4)+;
          &lcCtnDtl..Style AND !DELETED()
          LOCATE REST WHILE pack_no+STR(no_cart,4)+STYLE = lcPackNo+STR(&lcCtnDtl..Cart_No,4)+&lcCtnDtl..STYLE FOR !DELETED()
          *E037427,1 WAM 05/05/2005 (End)

        ENDIF
        *E037427,1 WAM 05/05/2005 Enhance performance
        *LOCATE REST FOR Pack_No+STR(Line_No,6)+Style+cPackColor = ;
        lcPackNo+STR(&lcCtnDtl..PackLineNo,6)+&lcCtnDtl..Style
        *! B609138,1 MMT 02/04/2010 Fix bug of repeated lines in pack_lin after editing PL[Start]
        *LOCATE REST WHILE Pack_No+STR(Line_No,6)+STYLE+cPackColor = lcPackNo+STR(&lcCtnDtl..PackLineNo,6)+&lcCtnDtl..STYLE
        LOCATE REST WHILE PACK_NO+STR(NO_CART,4)+STYLE+DYELOT =lcPackNo+STR(&lcCtnDtl..Cart_No,4)+&lcCtnDtl..STYLE;
          FOR STR(Line_No,6) =STR(&lcCtnDtl..PackLineNo,6)
        *! B609138,1 MMT 02/04/2010 Fix bug of repeated lines in pack_lin after editing PL[End]
        *E037427,1 WAM 05/05/2005 (End)
      ENDIF
      IF !FOUND('Pack_Lin') AND !DELETED(lcCtnDtl)
        loFormSet.lnLastNo = loFormSet.lnLastNo + 1
        loFormSet.Pack_Lin.APPEND()
        lnLastNo  = loFormSet.lnLastNo
        loFormSet.Pack_Lin.REPLACE("Line_No WITH lnLastNo")
        SELECT (lcCtnDtl)
        REPLACE PackLineNo WITH loFormSet.lnLastNo
        *! B608647,1 MMT 08/12/2008 Delete last carton in Packing list damage next PL cartons[Start]
      ELSE
        IF !FOUND('Pack_Lin') AND DELETED(lcCtnDtl)
          LOOP
        ENDIF
        *! B608647,1 MMT 08/12/2008 Delete last carton in Packing list damage next PL cartons[End]
      ENDIF
      *-- This seek for getting Pal_No
      llNothing = SEEK(STR(&lcCtnDtl..Cart_No,4),lcCtnHdr)
      SELECT Pack_Lin
      loFormSet.Pack_Lin.REPLACE("Pack_No    WITH lcPackNo,;
              No_Cart    WITH &lcCtnDtl..Cart_No,;
              NPltNo     WITH &lcCtnHdr..Pal_No,;
              Style      WITH &lcCtnDtl..Style,;
              nOrdLineNo WITH &lcCtnDtl..nOrdLineNo")
      loFormSet.Pack_Lin.REPLACE("Dyelot  WITH &lcCtnDtl..Dyelot,;
              Pack_id    WITH &lcCtnDtl..pack_id,;
              cPkColor    WITH &lcCtnDtl..cPkColor,;
              cPckSize    WITH &lcCtnDtl..cPckSize,;
              cPkVersion  WITH &lcCtnDtl..cPkVersion")
      *E037427,1 WAM 05/05/2005 update packed packs
      loFormSet.Pack_Lin.REPLACE("nPackNo    WITH &lcCtnDtl..nPackNo,;
                                  cCarrCtnID WITH &lcCtnHdr..cCarrCtnID ,;
                                  cCrtnVlTyp WITH IIF(EMPTY(&lcCtnHdr..cCrtnVlTyp),PACK_HDR.cCrtnVlTyp,&lcCtnHdr..cCrtnVlTyp)")
      *E037427,1 WAM 05/05/2005 (End)

      FOR lnI = 1 TO &lcCtnDtl..SzCnt
        lcI = STR(lnI,1)
        loFormSet.Pack_Lin.REPLACE("Qty&lcI WITH IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Qty&lcI,0)")
      ENDFOR
      loFormSet.Pack_Lin.REPLACE("TotQty    WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8")

      lnWeight = IIF(!DELETED(lcCtnDtl),&lcCtnDtl..Weight1+&lcCtnDtl..Weight2+&lcCtnDtl..Weight3+&lcCtnDtl..Weight4+;
        &lcCtnDtl..Weight5+&lcCtnDtl..Weight6+&lcCtnDtl..Weight7+&lcCtnDtl..Weight8,;
        MAX(Weight-&lcCtnDtl..Weight1-&lcCtnDtl..Weight2-&lcCtnDtl..Weight3-&lcCtnDtl..Weight4;
        -&lcCtnDtl..Weight5-&lcCtnDtl..Weight6-&lcCtnDtl..Weight7-&lcCtnDtl..Weight8;
        +(&lcCtnDtl..Qty1*&lcCtnDtl..OrgWgh);
        +(&lcCtnDtl..Qty2*&lcCtnDtl..OrgWgh);
        +(&lcCtnDtl..Qty3*&lcCtnDtl..OrgWgh);
        +(&lcCtnDtl..Qty4*&lcCtnDtl..OrgWgh);
        +(&lcCtnDtl..Qty5*&lcCtnDtl..OrgWgh);
        +(&lcCtnDtl..Qty6*&lcCtnDtl..OrgWgh);
        +(&lcCtnDtl..Qty7*&lcCtnDtl..OrgWgh);
        +(&lcCtnDtl..Qty8*&lcCtnDtl..OrgWgh),0))
      loFormSet.Pack_Lin.REPLACE("Weight     WITH lnWeight")

      loFormSet.Pack_Lin.REPLACE("cAdd_user WITH lcUser_Id,;
              dAdd_date WITH ldSysDate,;
              cAdd_time WITH lcTime")
      IF Pack_Lin.TotQty = 0
        loFormSet.Pack_Lin.DELETE()
      ENDIF
      SELECT (lcCtnDtl)
      REPLACE &lcCtnDtl..nStep WITH 1
    ENDIF
    *=================update ordline file ==========================
    *!B610281,1 HIA 03/24/2013 Aria XP - AR - Scan pick ticket invoice not updating style and stydye files, T20130117.0010 [Start]
    *IF loFormSet.lnRelCho <> 2
    IF loFormSet.lnRelCho <> 2 AND !(loformset.activemode ='E' AND Pack_hdr.STATUS ='C')
      *!B610281,1 HIA 03/24/2013 Aria XP - AR - Scan pick ticket invoice not updating style and stydye files, T20130117.0010 [End]

      SELECT OrdLine
      *-- These 2 seek are to adjust the pointer in both files (lcpcklin,'OrdLine')
      =SEEK(&lcCtnDtl..STYLE+&lcCtnDtl..Dyelot+STR(&lcCtnDtl..nOrdLineNo,6),lcPckLin,lcPckLin)
      =loFormSet.OrdLine.SEEK('O'+lcOrder+lcStore+&lcCtnDtl..STYLE+STR(&lcCtnDtl..nOrdLineNo,6))
      lnOrgBook = ToTBook
      lnOrgQty  = TotQty
      FOR lnI = 1 TO &lcPckLin..SzCnt
        lcI = STR(lnI,1)
        loFormSet.OrdLine.REPLACE("Book&lcI   WITH Book&lcI+MAX(&lcPckLin..OrdQty&lcI-OrdLine.Qty&lcI,0)")
      ENDFOR
      loFormSet.OrdLine.REPLACE("ToTBook WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8")
      lnBookQty = lnBookQty + ToTBook
      lnBookAmt = lnBookAmt + (TotBook * Price)

      FOR lnI = 1 TO &lcPckLin..SzCnt
        lcI = STR(lnI,1)
        *B608416,1 WAM 01/27/2008 Don't zero order quantity for unselected sizes
        IF &lcPckLin..lSelect&lcI
          *B608416,1 WAM 01/27/2008 (End)
          loFormSet.OrdLine.REPLACE("Qty&lcI WITH &lcPckLin..OrdQty&lcI")
          *B608416,1 WAM 01/27/2008 Don't zero order quantity for unselected sizes
        ENDIF
        *B608416,1 WAM 01/27/2008 (End)
      ENDFOR
      loFormSet.OrdLine.REPLACE("TotQty  WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8")
      loFormSet.OrdLine.REPLACE("PrePak  WITH &lcCtnDtl..PrePak,PpQty WITh &lcCtnDtl..PpQty")

      lnBookDiff = lnBookDiff + (TotBook - lnOrgBook)
      lnBkAmtDif = lnBkAmtDif + ((TotBook - lnOrgBook) * Price)
      lnQtyDiff  = lnQtyDiff  + (TotQty - lnOrgQty)
      lnQyAmtDif = lnQyAmtDif + ((TotQty - lnOrgQty) * Price)
    ENDIF
    *=================end updating ordline file======================
  ENDSCAN
  IF loFormSet.ActivemOde = 'A'
    SELECT Pack_Lin
    lcSetDel = SET('DELETE')
    SET DELETE ON
    =loFormSet.Pack_Lin.SEEK(lcPackNo)
    lnI = 0
    llFirst  = .T.
    lnLastCrt = Pack_Lin.No_Cart
    llSameCrt = .F.
    SCAN REST WHILE Pack_No+STR(No_Cart,4)+STYLE = lcPackNo
      IF !llFirst
        llSameCrt = (Pack_Lin.No_Cart = lnLastCrt)
        IF !llSameCrt
          lnLastCrt = Pack_Lin.No_Cart
        ENDIF
      ENDIF
      IF llFirst OR !llSameCrt
        lnI = lnI + 1
      ENDIF
      loFormSet.Pack_Lin.REPLACE("Pack_Lin.No_Cart WITH lnI")
      llFirst  = .F.
    ENDSCAN

    *! E302998,1 HES 27/11/2011 Change the way of getting the UCC9 while saving the P\L [BEGIN]
    *E302315,1 WLD 10/16/2006 Using visual label report [Begin]
    *!*	      IF loFormSet.llEdiSys
    *!*	        IF !loFormSet.llIncPLNum
    *!*	          IF loFormSet.EDICRTSQ.SEEK(lcPackNo+STR(lnI+1,6))
    *!*	            SELECT EDICRTSQ
    *!*	            DELETE REST WHILE pack_no+STR(cart_no,6) = lcPackNo
    *!*	          ENDIF
    *!*	          SELECT Pack_Lin
    *!*	          =loFormSet.Pack_Lin.SEEK(lcPackNo)
    *!*	          SCAN REST WHILE Pack_No+STR(No_Cart,4)+STYLE = lcPackNo
    *!*	            loFormSet.EDICRTSQ.SetOrder('PCKCRTSQ')
    *!*	            IF !loFormSet.EDICRTSQ.SEEK(lcPackNo+STR(Pack_Lin.No_Cart,6))
    *!*	              *-- If this customer is a partner , get the last Ucc # from EDIACPRT file,
    *!*	              IF loFormSet.EDIACPRT.SEEK('A'+lcAccount)
    *!*	                lnUcc9  = IIF(EMPTY(EDIACPRT.Ucc9),0,EVAL(EDIACPRT.Ucc9)) + 1
    *!*	                loFormSet.EDICRTSQ.SetOrder('EDICRTSQ')
    *!*	                llFound = loFormSet.EDICRTSQ.SEEK(lcAccount+PADL(lnUcc9,9,'0'))
    *!*	                DO WHILE llFound
    *!*	                  lnUcc9  = lnUcc9  + 1
    *!*	                  llFound = loFormSet.EDICRTSQ.SEEK(lcAccount+PADL(lnUcc9,9,'0'))
    *!*	                ENDDO
    *!*	                INSERT INTO 'EDICRTSQ' (Pack_No,Account,cart_no,Ucc9);
    *!*	                  VALUES (lcPackNo,lcAccount,Pack_Lin.No_Cart,PADL(lnUcc9,9,'0'))
    *!*	                loFormSet.EDIACPRT.REPLACE('Ucc9 WITH PADL(lnUcc9,9,"0")')
    *!*	              ELSE    && Get the last Ucc # from EDICRTSQ file.
    *!*
    *!*	                * HES NEW
    *!*	*!*	                SELECT MAX(EDICRTSQ.ucc9) FROM EDICRTSQ WHERE EDICRTSQ.account = lcAccount INTO CURSOR lcMaxUcc
    *!*	*!*	                SELECT lcMaxUcc
    *!*	*!*	                LOCATE
    *!*	                SELECT EDICRTSQ
    *!*	                lcOrd = ORDER()
    *!*	                SET ORDER TO EDICRTSQ DESCENDING
    *!*	                LOCATE FOR EDICRTSQ.account = lcAccount
    *!*	                * HES NEW
    *!*
    *!*
    *!*	                *B608296,1 MMT 09/30/2007 fix bug or error if EDICRTSQ  is empty[Start]
    *!*	                *IF EOF()
    *!*	                * HES NEW
    *!*	*!*	                IF EOF() OR ISNULL(lcMaxUcc.MAX_UCC9)
    *!*	                IF EOF()
    *!*	                * HES NEW
    *!*	                  *B608296,1 MMT 09/30/2007 fix bug or error if EDICRTSQ  is empty[End]
    *!*	                  lcUcc9 = '000000001'
    *!*	                ELSE
    *!*	                  *B608296,1 MMT 09/30/2007 fix bug or error if EDICRTSQ  is empty[Start]
    *!*	                  *lcUcc9 = PADL(EVAL(lcMaxUcc.MAX_UCC9)+1,9,'0')
    *!*	                  *! E302998,1 HES 27/11/2011 Change the way of getting the UCC9 while saving the P\L [BEGIN]
    *!*	*!*	                  lcUcc9 = PADL(VAL(lcMaxUcc.MAX_UCC9)+1,9,'0')
    *!*	                  lcUcc9 = PADL(ALLTRIM(STR(VAL(EDICRTSQ.Ucc9)+1,9,0)),9,'0')
    *!*	                  SELECT EDICRTSQ
    *!*	                  SET ORDER TO &lcOrd
    *!*	                  *! E302998,1 HES 27/11/2011 Change the way of getting the UCC9 while saving the P\L [END  ]
    *!*	                  *B608296,1 MMT 09/30/2007 fix bug or error if EDICRTSQ  is empty[ENd]
    *!*	                ENDIF
    *!*	                INSERT INTO 'EDICRTSQ' (Pack_No,Account,cart_no,Ucc9);
    *!*	                  VALUES (lcPackNo,lcAccount,Pack_Lin.No_Cart,lcUcc9)
    *!*	              ENDIF
    *!*	            ENDIF
    *!*	          ENDSCAN
    *!*	        ENDIF
    *!*	      ENDIF
    *E302315,1 WLD 10/16/2006 Using visual label report [End]
    *! E302998,1 HES 27/11/2011 Change the way of getting the UCC9 while saving the P\L [END  ]

    loFormSet.lnLastCtn = lnI
    SELECT (lcCtnDtl)
    *E302315,1 WLD 10/16/2006 Update the new file EDICRTSQ , and the new field Ucc9 in EDIACPRT [Begin]
    SET ORDER TO (lcCtnDtl) IN (lcCtnDtl)
    *E302315,1 WLD 10/16/2006 Update the new file EDICRTSQ , and the new field Ucc9 in EDIACPRT [End]
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
      *E302315,1 WLD 10/16/2006 Using visual label report [Begin]
      IF !loFormSet.llIncPLNum
        IF SEEK(STR(&lcCtnDtl..Cart_No,4),lcCtnHdr)
          SELECT &lcCtnHdr
          REPLACE Cart_No WITH lnI
          SELECT &lcCtnDtl
        ENDIF
      ENDIF
      *E302315,1 WLD 10/16/2006 Using visual label report [End]
      loFormSet.Pack_Lin.REPLACE("&lcCtnDtl..Cart_No WITH lnI")
      llFirst  = .F.
    ENDSCAN
    SET DELETE &lcSetDel
  ENDIF

  *E302315,1 WLD 10/16/2006 Using visual label report [Begin]
  IF !loFormSet.llIncPLNum
    IF loFormSet.ActivemOde = 'E'
      lcSetDel = SET('DELETE')
      SET DELETE ON
      lnLastCtn = lnI
      SELECT (lcCtnDtl)
      SET ORDER TO (lcCtnDtl) IN (lcCtnDtl)
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
        IF SEEK(STR(&lcCtnDtl..Cart_No,4) , lcCtnHdr )
          SELECT &lcCtnHdr
          REPLACE Cart_No WITH lnI
          SELECT &lcCtnDtl
        ENDIF
        REPLACE &lcCtnDtl..Cart_No WITH lnI
        llFirst  = .F.
      ENDSCAN

      *! E302998,1 HES 27/11/2011 Change the way of getting the UCC9 while saving the P\L [BEGIN]
      *!*	        IF loFormSet.EDICRTSQ.SEEK(lcPackNo+STR(lnI,6))
      *!*	          SELECT EDICRTSQ
      *!*	          *DELETE REST WHILE pack_no+STR(cart_no,6) = laData[1]
      *!*	          DELETE FOR pack_no+STR(cart_no,6) = lcPackNo
      *!*	        ENDIF

      *!*	        SELECT Pack_Lin
      *!*	        =loFormSet.Pack_Lin.SEEK(lcPackNo)
      *!*	        SCAN REST WHILE Pack_No+STR(No_Cart,4)+STYLE = lcPackNo
      *!*	          loFormSet.EDICRTSQ.SetOrder('PCKCRTSQ')
      *!*	          IF !loFormSet.EDICRTSQ.SEEK(lcPackNo+STR(Pack_Lin.No_Cart,6))
      *!*	            *-- If this customer is a partner , get the last Ucc # from EDIACPRT file,
      *!*	            IF loFormSet.EDIACPRT.SEEK('A'+lcAccount)
      *!*	              lnUcc9  = IIF(EMPTY(EDIACPRT.Ucc9),0,EVAL(EDIACPRT.Ucc9)) + 1
      *!*	              loFormSet.EDICRTSQ.SetOrder('EDICRTSQ')
      *!*	              llFound = loFormSet.EDICRTSQ.SEEK(lcAccount+PADL(lnUcc9,9,'0'))
      *!*	              DO WHILE llFound
      *!*	                lnUcc9  = lnUcc9  + 1
      *!*	                llFound = loFormSet.EDICRTSQ.SEEK(lcAccount+PADL(lnUcc9,9,'0'))
      *!*	              ENDDO
      *!*	              INSERT INTO 'EDICRTSQ' (Pack_No,Account,cart_no,Ucc9);
      *!*	                VALUE (lcPackNo,lcAccount,Pack_Lin.No_Cart,PADL(lnUcc9,9,'0'))
      *!*	              loFormSet.EDIACPRT.REPLACE('Ucc9 WITH PADL(lnUcc9,9,"0")')
      *!*	            ELSE    && Get the last Ucc # from EDICRTSQ file.
      *!*
      *!*	              * HES NEW
      *!*	*!*	              SELECT MAX(EDICRTSQ.ucc9) FROM EDICRTSQ WHERE EDICRTSQ.account = lcAccount INTO CURSOR lcMaxUcc
      *!*	*!*	              SELECT lcMaxUcc
      *!*	*!*	              LOCATE
      *!*	              SELECT EDICRTSQ
      *!*	              lcOrd = ORDER()
      *!*	              SET ORDER TO EDICRTSQ DESCENDING
      *!*	              LOCATE FOR EDICRTSQ.account = lcAccount
      *!*	              * HES NEW

      *!*
      *!*	              *B608296,1 MMT 09/30/2007 fix bug or error if EDICRTSQ  is empty[Start]
      *!*	              *IF EOF()
      *!*	              * HES NEW
      *!*	*!*	              IF EOF() OR ISNULL(lcMaxUcc.MAX_UCC9)
      *!*	              IF EOF()
      *!*	              * HES NEW
      *!*	                *B608296,1 MMT 09/30/2007 fix bug or error if EDICRTSQ  is empty[End]
      *!*	                lcUcc9 = '000000001'
      *!*	              ELSE
      *!*	                *B608296,1 MMT 09/30/2007 fix bug or error if EDICRTSQ  is empty[Start]
      *!*	                *lcUcc9 = PADL(EVAL(lcMaxUcc.MAX_UCC9)+1,9,'0')
      *!*	                *! E302998,1 HES 27/11/2011 Change the way of getting the UCC9 while saving the P\L [BEGIN]
      *!*	*!*	                lcUcc9 = PADL(VAL(lcMaxUcc.MAX_UCC9)+1,9,'0')
      *!*	                lcUcc9 = PADL(ALLTRIM(STR(VAL(EDICRTSQ.UCC9)+1,9,0)),9,'0')
      *!*	                SELECT EDICRTSQ
      *!*	                SET ORDER TO &lcOrd
      *!*	                *! E302998,1 HES 27/11/2011 Change the way of getting the UCC9 while saving the P\L [END  ]
      *!*	                *B608296,1 MMT 09/30/2007 fix bug or error if EDICRTSQ  is empty[End]
      *!*	              ENDIF
      *!*	              INSERT INTO 'EDICRTSQ' (Pack_No,Account,cart_no,Ucc9);
      *!*	                VALUE (lcPackNo,lcAccount,Pack_Lin.No_Cart,lcUcc9)
      *!*	            ENDIF
      *!*	          ENDIF
      *!*	        ENDSCAN
      *! E302998,1 HES 27/11/2011 Change the way of getting the UCC9 while saving the P\L [END  ]

      SET DELETE &lcSetDel
    ENDIF
  ENDIF
  *E302315,1 WLD 10/16/2006 Using visual label report [End]
  SELECT Pack_Hdr
  lnLastNo  = loFormSet.lnLastNo
  lnLastCtn = loFormSet.lnLastCtn
  loFormSet.Pack_Hdr.REPLACE("nLastLno  WITH MAX(nLastLno,lnLastNo) ,;
			            nLastCart WITH lnLastCtn")
ENDIF

*E037427,1 WAM 05/05/2005 Enhance performance
*!*	PRIVATE lcDelSt
*!*	lcDelSt = SET('DELETED')
*!*	SET DELETED ON
*!*	SELECT (lcCtnHdr)
*!*	GO TOP
*!*	SCAN
*!*	  IF loFormSet.Pack_Lin.SEEK(lcPackNo+STR(&lcCtnHdr..CART_NO,4))
*!*	    SELECT PACK_LIN
*!*	    loFormSet.PACK_LIN.REPLACE("cCarrCtnID WITH &lcCtnHdr..cCarrCtnID REST ;
*!*	      					       WHILE PACK_NO+STR(NO_CART,4)+STYLE = lcPackNo+STR(&lcCtnHdr..CART_NO,4)")
*!*	  ENDIF
*!*	ENDSCAN
*!*	SET DELETED &lcDelStat
*E037427,1 WAM 05/05/2005 (End)

*C200218,1 BWA 30/08/2001 Modify the custom invoice for sademara.[START]
IF ASCAN(loFormSet.laEvntTrig,PADR('RECALC',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
  lcParam = "MANUAL"
  =loFormSet.mDoTrigger(PADR('RECALC',10))
ENDIF
*C200218,1 BWA 30/08/2001.[END]
*=================end saving pack_lin file======================

*=================update book,open fields in ordhdr file=========
*!B610281,1 HIA 03/24/2013 Aria XP - AR - Scan pick ticket invoice not updating style and stydye files, T20130117.0010 [Start]
*IF loFormSet.lnRelCho <> 2
IF loFormSet.lnRelCho <> 2 AND !(loformset.activemode ='E' AND Pack_hdr.STATUS ='C')
  *!B610281,1 HIA 03/24/2013 Aria XP - AR - Scan pick ticket invoice not updating style and stydye files, T20130117.0010 [End]

  IF !(lnBookDiff=0 AND lnQtyDiff=0 AND lnBkAmtDif=0 AND lnQyAmtDif=0) AND ;
      loFormSet.ORDHDR.SEEK('O'+Pack_Hdr.ORDER)
    SELECT OrdHdr
    =RLOCK()
    loFormSet.OrdHdr.REPLACE("BOOK    WITH BOOK    + lnBookDiff ,;
            BOOKAMT WITH BOOKAMT + lnBkAmtDif ,;
            OPEN    WITH OPEN    + lnQtyDiff  ,;
            OPENAMT WITH OPENAMT + lnQyAmtDif")
    UNLOCK
  ENDIF
ENDIF
*=================end update book,open fields in ordhdr file=====

*================= Update status fields in Piktkt file to be 'K' As 'Pack' =========*
*!B610281,1 HIA 03/24/2013 Aria XP - AR - Scan pick ticket invoice not updating style and stydye files, T20130117.0010 [Start]
IF loFormSet.lnRelCho <> 2 AND !(loformset.activemode ='E' AND Pack_hdr.STATUS ='C')
  *!B610281,1 HIA 03/24/2013 Aria XP - AR - Scan pick ticket invoice not updating style and stydye files, T20130117.0010 [End]

  IF ASCAN(loFormSet.laEvntTrig,PADR('LLPAKST',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
    IF loFormSet.lnRelCho <> 2 .AND. PikTkt.STATUS # 'K'
      lnOldAlias = SELECT(0)
      SELECT PikTkt
      loFormSet.PikTkt.REPLACE("Status With 'K'")
      SELECT (lnOldAlias)
    ENDIF
  ENDIF
  *E302315,1 WLD 10/16/2006 Add new setup of including P\L number in UCC [Begin]
  *!B610281,1 HIA 03/24/2013 Aria XP - AR - Scan pick ticket invoice not updating style and stydye files, T20130117.0010 [Start]
ENDIF
*!B610281,1 HIA 03/24/2013 Aria XP - AR - Scan pick ticket invoice not updating style and stydye files, T20130117.0010 [End]


*! E302998,1 HES 27/11/2011 Change the way of getting the UCC9 while saving the P\L -Include all UCC structure- [BEGIN]
*!*	  IF loFormSet.llEdiSys AND !loFormSet.llIncPLNum
IF loFormSet.llEdiSys
  *! E302998,1 HES 27/11/2011 Change the way of getting the UCC9 while saving the P\L -Include all UCC structure- [END  ]

  DIMENSION laTableUpdate[2]
  STORE "" TO laTableUpdate
  laTableUpdate[1] = loFormSet.EDICRTSQ
  laTableUpdate[2] = loFormSet.EDIACPRT
  IF !lfTableUpdate()
    RETURN .F.
  ENDIF
ENDIF
*E302315,1 WLD 10/16/2006 Add new setup of including P\L number in UCC [End]

*! E302998,3 HES 01/16/2012 Update to work if 'AS' module is included no more [BEGIN]
*!*	  =loFormSet.llCanPrnLb AND lfSavCartn(loFormSet,lcPackNo)
=lfSavCartn(loFormSet,lcPackNo)
*! E302998,3 HES 01/16/2012 Update to work if 'AS' module is included no more [END  ]

SET DELETED &lcDelStat

*E037427,1 WAM 05/05/2005 Enhance performance
*!*	SELECT (lcCtnHdr)
*!*	SCAN
*!*	  IF loFormSet.Pack_Lin.SEEK(lcPackNo+STR(Cart_No,4))
*!*	    SELECT Pack_Lin
*!*	    SCAN WHILE pack_no+STR(no_cart,4)+style = lcPackNo+STR(&lcCtnHdr..Cart_No,4)
*!*	      loFormSet.Pack_Lin.REPLACE("cCrtnVlTyp WITH IIF(EMPTY(&lcCtnHdr..cCrtnVlTyp),PACK_HDR.cCrtnVlTyp,&lcCtnHdr..cCrtnVlTyp)")
*!*	    ENDSCAN
*!*	  ENDIF
*!*	ENDSCAN
*E037427,1 WAM 05/05/2005 (End)

IF loFormSet.lnRelCho <> 2
  llReturn = .T.
ELSE
  llReturn = .F.
ENDIF
SELECT(lcCtnDtl)
SET RELATION TO &lcCtDtRel.

*E037427,1 WAM 05/05/2005 Screen unlock method will cleaer order lock
*=lfUnLock(loFormset)
*E037427,1 WAM 05/05/2005 (End)

DIMENSION laTableUpdate[6]
STORE "" TO laTableUpdate
laTableUpdate[1] = loFormSet.ORDHDR
laTableUpdate[2] = loFormSet.OrdLine
laTableUpdate[3] = loFormSet.STYLE
laTableUpdate[4] = loFormSet.Stydye
laTableUpdate[5] = loFormSet.Pack_hdr
laTableUpdate[6] = loFormSet.Pack_lin
IF loFormSet.llEdiSys
  lnDim = ALEN(laTableUpdate,1)
  DIMENSION laTableUpdate[lnDim+2]
  laTableUpdate[lnDim+1] = loFormSet.BOL_LIN
  laTableUpdate[lnDim+2] = loFormSet.BOL_HDR
ENDIF
IF loFormSet.llCanPrnLb
  lnDim = ALEN(laTableUpdate,1)
  DIMENSION laTableUpdate[lnDim+1]
  laTableUpdate[lnDim+1] = loFormSet.Asn_ship
ENDIF
*E037427,1 WAM 05/05/2005 Enhance performance
*SELECT Pack_HDR
*DELETE FOR EMPTY(Pack_no)
*E037427,1 WAM 05/05/2005 (End)

IF !lfTableUpdate()
  RETURN .F.
ENDIF

SELECT(lnCurAlias)
llCSave = llReturn
*-- end of lpSavscr.


*!*************************************************************
*! Name      : lfNoSuffic
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : Check Stydye Availability
*!*************************************************************
*!
FUNCTION lfNoSuffic
LPARAMETERS loFormSet

PRIVATE lcMessage , lnI , lcI , lnChoice , llExitLoop, llRet2Main
STORE .F. TO llExitLoop, llRet2Main
lcPckLin = loFormSet.lcPckLin
lcOrder   = PADR(loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE,6)
lcStore   = loFormSet.AriaForm1.kbStore.keytextbox.VALUE


SELECT(lcPckLin)
SCAN
  llNoThing = loFormSet.OrdLine.SEEK('O'+lcOrder+lcStore+&lcPckLin..STYLE+STR(nOrdLineNo,6))
  IF llNoThing
    lcI = cSizeNO
    IF (PQty > OrdLine.Pik&lcI) AND ;
        loFormSet.StyDye.SEEK(OrdLine.STYLE+OrdLine.cWareCode+OrdLine.Dyelot) AND ;
        PQty > (StyDye.Stk&lcI - StyDye.Alo&lcI)
      lcMessage = "Order : " + lcOrder + ", Style : " + ordline.STYLE + LANG_ManulPL_MsgAvlQty
      IF loFormSet.llAlwForce
        lcMessage = lcMessage + LANG_ManulPL_MsgFoecAllo
        * <Yes> <Yes to All> <No>
        *lnChoice = 1 or 2 or 3
        lnChoice  =gfModalGen("INM00000B44002","Dialog","","",lcMessage)
        DO CASE
        CASE lnChoice = 2
          llExitLoop = .T.

        CASE lnChoice = 3
          STORE .T. TO llExitLoop , llRet2Main
        ENDCASE

      ELSE
        lcMessage = lcMessage + LANG_ManulPL_MsgSavWUpdPik
        * <Yes> <No>
        *lnChoice=1 or 2
        lnChoice =gfModalGen("INM00000B44009","Dialog","","",lcMessage)
        IF lnChoice=1
          loFormSet.llUpdtPkTk = .F.
          llExitLoop = .T.
        ELSE
          STORE .T. TO llExitLoop , llRet2Main
        ENDIF
      ENDIF
      *-- Exit for loop
      IF llExitLoop
        LOOP
      ENDIF
    ENDIF

    *-- exit scan loop
    IF llExitLoop
      LOOP
    ENDIF
  ENDIF
ENDSCAN
RETURN llRet2Main
*-- end of lfNoSuffic.


*!*************************************************************
*! Name      : lfDelBol
*! Developer : Hend Ghanem (HBG)
*! Date      : 07/20/2000
*! Purpose   :
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfDelBol
LPARAMETERS loFormSet

*B609720,1 TMI 10/31/2011 [Start] set the correct order
LOCAL lcBOLOrd
lcBOLOrd = ORDER('BOL_HDR')
loFormSet.BOL_HDR.SETORDER('BOL_HDR')
*B609720,1 TMI 10/31/2011 [End  ]

IF loFormSet.BOL_HDR.SEEK(PACK_HDR.Bill_Ladg) AND ;
    loFormSet.BOL_LIN.SEEK(PACK_HDR.Bill_Ladg+PACK_HDR.ORDER+PACK_HDR.Pack_No)
  loFormSet.BOL_LIN.DELETE()
  loFormSet.BOL_HDR.REPLACE("TOT_WGHT WITH TOT_WGHT - PACK_HDR.Tot_Wght ,;
          TOT_CART WITH TOT_CART - PACK_HDR.Tot_Cart ,TOT_PCS  WITH TOT_PCS  - PACK_HDR.Tot_Pcs")

ENDIF

*B609720,1 TMI 10/31/2011 [Start] restore the old order
IIF(!EMPTY(lcBOLOrd),loFormSet.BOL_HDR.SETORDER(lcBOLOrd),'')
*B609720,1 TMI 10/31/2011 [End  ]



*!*************************************************************
*! Name      : lfSavCartn
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : Saving Cartons  in ASN_SHIP
*!*************************************************************
*! Example   : = lfSavCartn()
*!*************************************************************
FUNCTION lfSavCartn
LPARAMETERS loFormSet,lcPack_no
*E302315 WLD Print Visual lables from Packing List 10/10/2006 [Begin]

lcSetDele = SET('DELETE')
IF !loformset.llIncPLNum
  SET DELETE ON
  SELECT (loformset.lcTmAsnShp)
  ZAP
  SELECT Asn_Ship
  DELETE FOR bol_no+pack_no+STR(cart_no,6)+asn_ver = lcBol+lcPackNo
ENDIF
lcBOL = PADR(loformset.ariaform1.pgfPacking.HEADER.txtbOL.VALUE,6)

*! E302998,3 HES 01/16/2012 Update to work if 'AS' module is included no more [BEGIN]
*!*	  IF !EMPTY(lcBOL)
*!*	    IF loFormSet.llCanPrnLb

*! B609859,1 HES 03/08/2012 Don't call SEND SendUccLabels object if the BOL is empty [BEGIN]
*!*	  IF loFormSet.llEdiSys
*! E303260,1 HES 09/20/2012 Add records in EDICRTSQ while no BOL assigned [BEGIN]
*!*	  IF loFormSet.llEdiSys AND !EMPTY(lcBOL)
SET STEP ON
IF loFormSet.llEdiSys
  lcManufId = ALLTRIM(gfGetMemVar('XMANUFID',oAriaApplication.ActiveCompanyID))
  PRIVATE oGetMemVar
  oGetMemVar     = CREATEOBJECT("GetMemVar")
  lnCRTSEQ_SETUP = '0'
  lnCRTSEQ_SETUP = ALLTRIM(oGetMemVar.DO('M_UCCBSDON',oAriaApplication.ActiveCompanyId))
  IF (oAriaApplication.ClassDir+'EDI.VCX' $ SET('classlib'))
    RELEASE CLASSLIB (oAriaApplication.ClassDir+'EDI.VCX')
  ENDIF
  lcoldAppHome = oAriaApplication.ApplicationHome
  lcoldAppRep  = oAriaApplication.ReportHome
  lcoldAppBitMapHome = oAriaApplication.BitMapHome
  lcoldAppCls = oAriaApplication.ClassDir
  lcoldAppScx = oAriaApplication.ScreenHome
  lcEdiPath = oAriaApplication.ediinstallationpath
  oAriaApplication.ApplicationHome = lcEdiPath +  'PRGS\'
  oAriaApplication.ReportHome = lcEdiPath +  'REPORTS\'
  oAriaApplication.BitMapHome = lcEdiPath +  'BMPs\'
  oAriaApplication.ClassDir   = lcEdiPath +  'CLASSES\'
  oAriaApplication.ScreenHome = lcEdiPath +  'SCREENS\'
  SET CLASSLIB TO (oAriaApplication.ClassDir +'EDI.VCX') ADDIT
  SET PROCEDURE TO (oAriaApplication.ApplicationHome +'EDIGLOBL.FXP') ADDITIV
  loFormSet.EDICRTSQ.SetOrder('PCKCRTSQ')
  IF EMPTY(lcBOL)
    SELECT (lcCtnHdr)
    SCAN
      lfGetUccNO(lnCRTSEQ_SETUP, lcPack_no, Cart_No,.F.,LEN(ALLTRIM(lcManufId)))
    ENDSCAN
  ELSE
    *! E303260,1 HES 09/20/2012 Add records in EDICRTSQ while no BOL assigned [END  ]

    *! B609859,1 HES 03/08/2012 Don't call SEND SendUccLabels object if the BOL is empty [END  ]

    *! E302998,3 HES 01/16/2012 Update to work if 'AS' module is included no more [END  ]
    *! E303260,1 HES 09/20/2012 Add records in EDICRTSQ while no BOL assigned [BEGIN]
    *!*	      IF (oAriaApplication.ClassDir+'EDI.VCX' $ SET('classlib'))
    *!*	        RELEASE CLASSLIB (oAriaApplication.ClassDir+'EDI.VCX')
    *!*	      ENDIF
    *!*	      lcoldAppHome = oAriaApplication.ApplicationHome
    *!*	      lcoldAppRep  = oAriaApplication.ReportHome
    *!*	      lcoldAppBitMapHome = oAriaApplication.BitMapHome
    *!*	      lcoldAppCls = oAriaApplication.ClassDir
    *!*	      lcoldAppScx = oAriaApplication.ScreenHome
    *! E303260,1 HES 09/20/2012 Add records in EDICRTSQ while no BOL assigned [END  ]
    *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[Start]
    *! E302857,1 MMT 04/27/2011 Use New Property to point to the EDI Folder classes[MEDIA][Start]
    *!*	      IF oAriaApplication.multiinst
    *!*	        lcEdiPath = ALLTRIM(UPPER(SUBSTR(oariaapplication.InstallPath,1,AT('\',oariaapplication.InstallPath,3))))
    *!*	      ELSE
    *!*	      *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[End]
    *!*
    *!*	        lcEdiPath = UPPER(SUBSTR(oariaapplication.syspath,1,AT('\',oariaapplication.syspath,2)))
    *!*
    *!*	      *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[Start]
    *!*	      ENDIF
    *! E303260,1 HES 09/20/2012 Add records in EDICRTSQ while no BOL assigned [BEGIN]
    *!*		  lcEdiPath = oAriaApplication.ediinstallationpath
    *! E303260,1 HES 09/20/2012 Add records in EDICRTSQ while no BOL assigned [END  ]
    *! E302857,1 MMT 04/27/2011 Use New Property to point to the EDI Folder classes[MEDIA][End]
    *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[End]

    *! E303260,1 HES 09/20/2012 Add records in EDICRTSQ while no BOL assigned [BEGIN]
    *!*	      oAriaApplication.ApplicationHome = lcEdiPath +  'PRGS\'
    *!*	      oAriaApplication.ReportHome = lcEdiPath +  'REPORTS\'
    *!*	      oAriaApplication.BitMapHome = lcEdiPath +  'BMPs\'
    *!*	      oAriaApplication.ClassDir   = lcEdiPath +  'CLASSES\'
    *!*	      oAriaApplication.ScreenHome = lcEdiPath +  'SCREENS\'
    *!*	      SET CLASSLIB TO (oAriaApplication.ClassDir +'EDI.VCX') ADDIT
    *!*	      SET PROCEDURE TO (oAriaApplication.ApplicationHome +'EDIGLOBL.FXP') ADDITIV
    *!*	      loFormSet.EDICRTSQ.SetOrder('PCKCRTSQ')
    *! E303260,1 HES 09/20/2012 Add records in EDICRTSQ while no BOL assigned [END  ]
    oUccLabels = CREATEOBJECT("SendUccLabels",.T.)


    *! E302998,2 HES 27/11/2011 Handle this issue in LFGETUCC9 function in EDIGLOBL program [BEGIN]
    *! E302998,1 HES 27/11/2011 Change the way of getting the UCC9 while saving the P\L -Add the parameter to indicate if we need to add or not- [BEGIN]
    *!*	      =oUccLabels.DO(lcBOL,.T.)
    *!*	      =oUccLabels.DO(lcBol,.T.,.T.)
    =oUccLabels.DO(lcBOL,.T.)
    *! E302998,1 HES 27/11/2011 Change the way of getting the UCC9 while saving the P\L -Add the parameter to indicate if we need to add or not- [END  ]
    *! E302998,2 HES 27/11/2011 Handle this issue in LFGETUCC9 function in EDIGLOBL program [END  ]


    RELEASE oUccLabels
    *! E303260,1 HES 09/20/2012 Add records in EDICRTSQ while no BOL assigned [BEGIN]
    *!*	      oAriaApplication.ApplicationHome = lcoldAppHome
    *!*	      oAriaApplication.ReportHome      = lcoldAppRep
    *!*	      oAriaApplication.BitMapHome      = lcoldAppBitMapHome
    *!*	      oAriaApplication.ClassDir        = lcoldAppCls
    *!*	      oAriaApplication.ScreenHome      = lcoldAppScx
    *! E303260,1 HES 09/20/2012 Add records in EDICRTSQ while no BOL assigned [END  ]

    *!*	  *! E302998,3 HES 01/16/2012 Update to work if 'AS' module is included no more [BEGIN]
    *!*	    ENDIF
    *!*	  ENDIF
  ENDIF

  *! E303260,1 HES 09/20/2012 Add records in EDICRTSQ while no BOL assigned [BEGIN]
  oAriaApplication.ApplicationHome = lcoldAppHome
  oAriaApplication.ReportHome      = lcoldAppRep
  oAriaApplication.BitMapHome      = lcoldAppBitMapHome
  oAriaApplication.ClassDir        = lcoldAppCls
  oAriaApplication.ScreenHome      = lcoldAppScx
ENDIF
*! E303260,1 HES 09/20/2012 Add records in EDICRTSQ while no BOL assigned [END  ]

*!*	  *! E302998,3 HES 01/16/2012 Update to work if 'AS' module is included no more [END  ]

DIMENSION laTableUpdate[1]
laTableUpdate[1] = loFormSet.Asn_ship
*! B610008,1 HIA 07/16/2012 Error, type mismatch, when saving new P\L [T20120621.0065][Begin]
*IF !lfTableUpdate()
IF !(TYPE('laTableUpdate[1]')= 'O' AND lfTableUpdate())
  *! B610008,1 HIA 07/16/2012 Error, type mismatch, when saving new P\L [T20120621.0065][End]
  RETURN .F.
ENDIF
*E302315 WLD Print Visual lables from Packing List 10/10/2006 [End]

PRIVATE llchoice, lnCartons, lnCarRef , lnActAlias , lcDetOrder , llPrinSel
lnActAlias = SELECT(0)

lcCtnDtl   = loFormSet.lcCtnDtl
lcTmAsnShp = loFormSet.lcTmAsnShp
STORE SPACE (0) TO laSource,laTarget
lnCartons =0
GO BOTTOM IN (lcCtnDtl)
lnCartons = &lcCtnDtl..Cart_No
IF lnCartons <> 0
  DIMENSION laSource[lnCartons] , laTarget[1]
ENDIF
DIMENSION laTarget[1]
lnCarRef = 0
lcDetOrder = ORDER(lcCtnDtl)
SET ORDER TO (lcCtnDtl) IN (lcCtnDtl)
*-- Saving code.
FOR lnCarRef = 1 TO lnCartons
  *E302315,1 WLD 10/16/2006 Using visual label report [Begin]
  *!*	  =SEEK(STR(lnCarRef,4),lcCtnDtl) AND lfvLblInfo(loFormSet,lnCarRef)
  *!*	  laSource[lnCarRef] =  "Carton # " + PADL(ALLTRIM(STR(&lcCtnDtl..CART_NO)),4)
  *!*	  IF SEEK(STR(lnCarRef,6),lcTmAsnShp)
  *!*	    SELECT (lcTmAsnShp)
  *!*	    SCATTER MEMVAR MEMO
  *!*	    m.Pack_No = lcPack_no
  *!*	    IF loFormSet.Asn_Ship.SEEK(&lcTmAsnShp..BOL_NO+&lcTmAsnShp..PACK_NO+STR(&lcTmAsnShp..CART_NO,6))
  *!*	      loFormSet.Asn_Ship.replace()
  *!*	      SELECT (loFormSet.Asn_Ship.lcCursorUpdate)
  *!*	      GATHER MEMVAR MEMO
  *!*	      SELECT Asn_Ship
  *!*	    ELSE
  *!*	      loFormSet.Asn_Ship.INSERT("FROM MEMVAR")
  *!*	    ENDIF
  *!*	  ENDIF
  =SEEK(STR(lnCarRef,4),lcCtnDtl)
  laSource[lnCarRef] =  "Carton # " + PADL(ALLTRIM(STR(&lcCtnDtl..CART_NO)),4)
  m.Pack_No = lcPack_no
  m.Bol_No = lcBOL
  IF loFormSet.Asn_Ship.SEEK(lcBOL+lcPack_no+STR(&lcCtnDtl..CART_NO,6))
    *! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[Start]
    SELECT Asn_Ship
    *! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[End]
    SCATTER MEMVAR MEMO
    IF SEEK(STR(m.CART_NO,6),loFormSet.lcTmAsnShp)
      SELECT (loFormSet.lcTmAsnShp)
      GATHER MEMVAR MEMO
    ELSE
      INSERT INTO (loFormSet.lcTmAsnShp) FROM MEMVAR
    ENDIF
  ENDIF
  *E302315,1 WLD 10/16/2006 Using visual label report [End]
ENDFOR
*E302315,1 WLD 10/16/2006 Fix bug of of not deleting old cartons from ASN_SHIP file [Begin]
SET DELETE &lcSetDele
*E302315,1 WLD 10/16/2006 Fix bug of of not deleting old cartons from ASN_SHIP file[End]

llchoice = .T.
*E302315,1 WLD 10/16/2006 Using visual label report [Begin]
*IF !loFormSet.llScrPrnLb
IF loFormSet.llScrPrnLb
  *E302315,1 WLD 10/16/2006 Using visual label report [End]
  DO WHILE llchoice
    lnCarRef = 0
    lnChoice = gfModalGen("QRM44097B44011","Dialog")
    DO CASE

      *-- Case of YES
    CASE lnChoice=1
      *E302315,1 WLD 10/16/2006 Using visual label report [Begin]
      IF loFormSet.SycAsnHd.SEEK(&lcTmAsnShp..ASN_VER) AND SycAsnHd.cType = 'Y'
        =lfVsulLbl(loFormSet,&lcTmAsnShp..ASN_VER,'')
        *E302315,1 WLD 10/16/2006 Using visual label report [End]
      ELSE

        *B608696,1 HIA 09/23/2008 default version with 'XXX' if not found in the SYCASNHD [Begin]
        lcASN_VER = &lcTmAsnShp..ASN_VER
        IF EMPTY(ALLTRIM(&lcTmAsnShp..ASN_VER)) OR !loFormSet.SycAsnHd.SEEK(&lcTmAsnShp..ASN_VER)
          lcASN_VER = 'XXX'
        ENDIF
        *B608696,1 HIA 09/23/2008 default version with 'XXX' if not found in the SYCASNHD [End]

        FOR lnCarRef = 1 TO lnCartons
          IF SEEK(STR(lnCarRef,6),lcTmAsnShp)
            SELECT (lcTmAsnShp)
            *B608696,1 HIA 09/23/2008 default version with 'XXX' if not found in the SYCASNHD [Begin]
            *=lfPrintLbl(loFormSet,ASN_VER)
            =lfPrintLbl(loFormSet,lcASN_VER )
            *B608696,1 HIA 09/23/2008 default version with 'XXX' if not found in the SYCASNHD [End]
          ENDIF
        ENDFOR
        *E302315,1 WLD 10/16/2006 Using visual label report [Begin]
      ENDIF
      llchoice = .F.
      *E302315,1 WLD 10/16/2006 Using visual label report [End]
      *-- Case of Selective
    CASE lnChoice=2

      DIMENSION laTarBef[ALEN(laTarget,1)]
      =ACOPY(laTarget,laTarBef)
      = gfMover(@laSource,@laTarget,'Select Carton(s).',.T.,'')

      IF ALEN(laTarBef,1) = ALEN(laTarget,1)
        llPrinSel = .F.
        IF !EMPTY(laTarget[1])
          FOR lnJ = 1 TO ALEN(laTarget,1)
            IF ASCAN(laTarBef,laTarget[lnJ]) = 0
              llPrinSel = .T.
              EXIT
            ENDIF
          ENDFOR
        ENDIF
      ELSE
        llPrinSel = !EMPTY(laTarget[1])
      ENDIF

      IF llPrinSel
        *E302315,1 WLD 10/16/2006 Using visual label report [Begin]
        IF loFormSet.SycAsnHd.SEEK(&lcTmAsnShp..ASN_VER) AND SycAsnHd.cType = 'Y'
          lcCrtnRng = ''
          FOR lnCarRef = 1 TO ALEN(laTarget,1)
            lcCrtnRng = lcCrtnRng + ALLTRIM(RIGHT(laTarget[lnCarRef],4)) + ','
          ENDFOR
          =lfVsulLbl(loFormSet,&lcTmAsnShp..ASN_VER,LEFT(lcCrtnRng,LEN(lcCrtnRng)-1))
        ELSE
          *E302315,1 WLD 10/16/2006 Using visual label report [End]
          FOR lnCarRef = 1 TO ALEN(laTarget,1)
            lnToPrnCrt = VAL(RIGHT(laTarget[lnCarRef],4))
            IF SEEK(STR(lnToPrnCrt,6),lcTmAsnShp)
              SELECT (lcTmAsnShp)
              =lfPrintLbl(loFormSet,ASN_VER)
            ENDIF
          ENDFOR
          *E302315,1 WLD 10/16/2006 Using visual label report [Begin]
        ENDIF
        *E302315,1 WLD 10/16/2006 Using visual label report [End]
      ENDIF

      *-- Case of proceed
    CASE lnChoice=3
      llchoice = .F.
    ENDCASE
  ENDDO

ENDIF
SELECT (lnActAlias)
*-- end of lfSavCartn.

*!*************************************************************
*! Name      : lfBefDelScr
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : To make local delete.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : DO lfBefDelScr
*!*************************************************************
FUNCTION lfBefDelScr
LPARAMETERS loFormSet

PRIVATE lnCurAlias,lcHdrTag,lcLinTag,llShiped ,lcPackNo

lcPackNo  = PADR(loFormSet.AriaForm1.kbPackNo.keytextbox.VALUE,6)

*-- Are you sure you want to Delete this record?
*-- <Yes> <No>
IF gfModalGen("INM00002B00006","Dialog",'delete') = 2
  RETURN .F.
ENDIF

IF loFOrmSet.Pack_Hdr.SEEK(lcPackNo)
  IF Pack_Hdr.STATUS = 'C'
    PRIVATE lcMessage
    *-- This packing list is shipped. Can't be Deleted
    *-- OK
    lcMessage = LANG_ManulPL_MsgCantDelet
    = gfModalGen("INM44102B00000","Dialog",lcMessage)
    loFormSet.ChangeMode('S')
    RETURN .F.
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lpDelScr
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : To make local delete.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : DO lpDelScr
*!*************************************************************
FUNCTION lfmDelScr
LPARAMETERS loFormSet

PRIVATE lnCurAlias,lcHdrTag,lcLinTag,llShiped

lcCtnDtl  = loFormSet.lcCtnDtl
lcPackNo  = PADR(loFormSet.AriaForm1.kbPackNo.keytextbox.VALUE,6)
lcOrder   = PADR(loFormSet.ariaForm1.kbOrderNo.keytextbox.VALUE,6)
lcStore   = loFormSet.AriaForm1.kbStore.keytextbox.VALUE
*! E302998,1 HES 27/11/2011 Change the way of getting the UCC9 while saving the P\L [BEGIN]
lcBOL = PADR(ALLTRIM(loFormSet.ariaForm1.pgfPacking.HEADER.txtBOL.VALUE),6)
*! E302998,1 HES 27/11/2011 Change the way of getting the UCC9 while saving the P\L [END  ]

lnCurAlias = SELECT(0)

IF !(lcPackNo==loFormSet.lcPrvPack)
  =lfvSelOrdL(loFormSet)
ENDIF
loFormSet.OrdLine.SetOrder('Ordlinst')
SELECT (lcCtnDtl)
SET FILTER TO
SET RELATION TO
SET KEY TO
SCAN
  IF loFormSet.OrdLine.SEEK('O'+lcOrder+lcStore+&lcCtnDtl..STYLE+STR(&lcCtnDtl..nOrdLineNo,6))
    lcSze = cSizeNO
    SELECT OrdLine
    loFormSet.OrdLine.REPLACE("nPck&lcSze  WITH IIF((&lcCtnDtl..Qty) <> 0,MAX(nPck&lcSze  - (&lcCtnDtl..Qty),0),0),;
            nPwght WITH IIF((&lcCtnDtl..Weight) <> 0, MAX(nPwght - (&lcCtnDtl..Weight),0),0)")

  ENDIF
ENDSCAN

IF loFormSet.llEdiSys AND loFormSet.Pack_Hdr.SEEK(lcPackNo)
  =lfDelBol(loFormSet)
ENDIF

lcHdrTag = ORDER('Pack_Hdr')
IF loFormSet.Pack_Hdr.SEEK(lcPackNo)
  loFormSet.Pack_Hdr.DELETE()
ENDIF
loFormSet.Pack_hdr.SetOrder(lcHdrTag)

lcLinTag = ORDER('Pack_Lin')
SELECT Pack_Lin
SET RELATION TO
SET FILTER TO
SET KEY TO
IF loFormSet.Pack_Lin.SEEK(lcPackNo)
  SELECT Pack_Lin
  *E037427,1 WAM 05/05/2005 Enhance performance
  *SCAN FOR Pack_No+STR(No_Cart,4)+Style = lcPackNo
  SCAN REST WHILE Pack_No+STR(No_Cart,4)+STYLE = lcPackNo
    *E037427,1 WAM 05/05/2005 (End)
    *E037427,1 WAM 05/05/2005 Update packed packs in ordline
    IF loFormSet.OrdLine.SEEK('O'+lcOrder+lcStore+Pack_Lin.STYLE+STR(Pack_Lin.nOrdLineNo,6))
      SELECT OrdLine
      loFormSet.OrdLine.REPLACE("nPkPack WITH MAX(nPkPack  - Pack_Lin.nPackNo,0)")
      SELECT Pack_Lin
    ENDIF
    *E037427,1 WAM 05/05/2005 (End)
    loFormSet.Pack_Lin.DELETE()
  ENDSCAN
ENDIF
*E302315,1 WLD 10/16/2006 Using visual label report [Begin]




*! E302998,1 HES 27/11/2011 Change the way of getting the UCC9 while saving the P\L -Delete from EDICRTSQ for all kind of UCC structure- [BEGIN]
*!*	  IF !loFormSet.llIncPLNum
*! E302998,1 HES 27/11/2011 Change the way of getting the UCC9 while saving the P\L -Delete from EDICRTSQ for all kind of UCC structure- [END  ]
*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][Start]
IF loFormSet.llEdiSys
  *E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][END]
  loFormSet.EDICRTSQ.SetOrder('PCKCRTSQ')
  lcSetDele = SET('DELETE')
  SET DELETE ON
  IF loFormSet.EDICRTSQ.SEEK(lcPackNo)
    SELECT EDICRTSQ
    DELETE FOR pack_no+STR(cart_no,6) = lcPackNo
  ENDIF
  SET DELETE &lcSetDele
  
  *B610615,1 TMI 12/09/2013 12:40 [Start] open the asn_ship if it is not
  IF !USED('ASN_SHIP')
    loFormSet.Asn_Ship = CREATEOBJECT("RemoteTable",'Asn_Ship','Asn_Ship','Asn_Ship',loFormSet.DATASESSIONID)
  ENDIF 
  *B610615,1 TMI 12/09/2013 12:40 [End  ] 

  *! E302998,1 HES 27/11/2011 Change the way of getting the UCC9 while saving the P\L -Delete from EDICRTSQ for all kind of UCC structure- [BEGIN]
  SELECT Asn_Ship
  DELETE FOR bol_no+pack_no+STR(cart_no,6)+asn_ver = lcBol+lcPackNo
  *!*	  ENDIF
  *! E302998,1 HES 27/11/2011 Change the way of getting the UCC9 while saving the P\L -Delete from EDICRTSQ for all kind of UCC structure- [END  ]
  *E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][Start]
ENDIF
*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][END]

*E302315,1 WLD 10/16/2006 Using visual label report [End]
=loFormSet.Pack_Lin.SetOrder(lcLinTag)

DIMENSION laTableUpdate[3]
STORE "" TO laTableUpdate
laTableUpdate[1] = loFormSet.OrdLine
laTableUpdate[2] = loFormSet.Pack_hdr
laTableUpdate[3] = loFormSet.Pack_lin
IF loFormSet.llEdiSys
  lnDim = ALEN(laTableUpdate,1)

  *! E302998,1 HES 27/11/2011 Change the way of getting the UCC9 while saving the P\L -Include the EDICRTSQ to be updated- [BEGIN]
  *!*	    DIMENSION laTableUpdate[lnDim+2]
  DIMENSION laTableUpdate[lnDim+4]
  laTableUpdate[lnDim+3] = loFormSet.EDICRTSQ
  laTableUpdate[lnDim+4] = loFormSet.Asn_Ship
  *! E302998,1 HES 27/11/2011 Change the way of getting the UCC9 while saving the P\L -Include the EDICRTSQ to be updated- [END  ]

  laTableUpdate[lnDim+1] = loFormSet.BOL_LIN
  laTableUpdate[lnDim+2] = loFormSet.BOL_HDR
ENDIF

IF !lfTableUpdate()
  RETURN .F.
ENDIF

loFormSet.ChangeMode('S')


SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfTableUpdate
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : function to Update Sql Tables.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfTableUpdate

*--Open Dictionary files.
LOCAL lnAlias,lnConnectionHandlar,lcTranCode,lnI,llUpdate
lnAlias = SELECT(0)

lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  SELECT (lnAlias)
  RETURN .F.
ENDIF
FOR lnI = 1 TO ALEN(laTableUpdate,1)
  SELECT (laTableUpdate[lnI].lcCursorView)
  SET RELATION TO
  SET KEY TO
  SET FILTER TO
  llUpdate = laTableUpdate[lnI].TABLEUPDATE(lcTranCode)
  IF !llUpdate
    =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
ENDFOR

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
IF lnConnectionHandlar # 1
  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  SELECT(lnAlias)
  RETURN .F.
ENDIF

SELECT(lnAlias)
*--end of lfTableUpdate.

*!*************************************************************
*! Name      : lfObj_Lock
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : Function to logicaly lock a record
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Passed Parameters  :  Lock or unlock
*!
*!*************************************************************
*! Returns            :  .T. --> succeded
*!                       .F. --> unsuccess
*!*************************************************************
*! Example            :  gfObj_Lock(.T.)
*!*************************************************************
FUNCTION lfObj_Lock
PARAMETERS lLok_Set,lcFile,loformset,lcFileName

PRIVATE lnRecNo,lRet_Flag
IF !EMPTY(loformset.Dataentityobject)
  lcDataEntityCom = loformset.Dataentityobject+".RecordLock(lLok_Set)"
  lRet_Flag = &lcDataEntityCom
  RETURN lRet_Flag
ENDIF

PRIVATE lnOldrpSt
LOCAL lnDataSession
lnDataSession = loformset.DATASESSIONID  &&SET("DATASESSION")
*SET DATASESSION TO (lnDataSession)
lnAlias = SELECT()
SELECT (lcFileName)


lRet_Flag = .F.
lLok_It   = .F.
llLocked  = .F.
*** Go to the same record to get a fresh copy in the buffer
lnRecNo = RECNO()

DO WHILE .T.
  SELECT (lcFileName)
  IF lnRecNo <= RECCOUNT()
    GO lnRecNo
    llLocked = RLOCK()

    *-- UNLOCK RECORD lnRecNo
    IF lnRecNo > 0
      UNLOCK RECORD lnRecNo
    ENDIF

    IF DELETED()
      =gfModalGen('INM00095B00000','ALERT')
      SELECT (lnAlias)
      loformset.AlterMode("S")
      RETURN .F.
    ENDIF
  ENDIF

  *** Chek if the record is in use by another user
  IF lLok_Set
    *** Chek if the field cLok_User in the structur
    IF !lLok_Stat .AND. llLocked
      *** Record is not locked you may lock it
      lLok_It   = .T.
    ELSE
      lcLok_User = cLok_User
      lcLok_Name = oAriaApplication.getUserName(cEdit_User)
      IF !EMPTY(lcLok_User)
        IF ALLTRIM(lcLok_User) = ALLTRIM(oAriaApplication.User_ID)
          * Messaging the user that he cannot edit the same record
          * from more than one session and permit him from editing
          * the same record
          IF gfModalGen("INM00240B00006","ALERT")=2
            lLok_It    = .F.
            lRet_Flag  = .F.
          ELSE
            lLok_It    = .T.
          ENDIF
        ELSE
          lnOldrpSt = SET('REPROCESS')
          llLoop = .F.
          **-- If the order licked by Invoice sales order screen
          IF lcLok_User = '*AL*' OR lcLok_User = '*SOORD*'
            *SET DATASESSION TO (lnDataSession)
            IF  gfModalGen("INM00028B00015","ALERT",lcLok_Name) = 1
              llLoop = .T.
            ENDIF
            lLok_It    = .F.
            lRet_Flag  = .F.
          ELSE
            IF !USED('syuStatc')
              loformset.syuStatc = CREATEOBJECT("RemoteTable",'syuStatc','Cuser_id','syuStatc',loformset.DATASESSIONID)
            ENDIF
            SELECT syuStatc
            IF loformset.syuStatc.SEEK('INI'+'OLDVARS'+lcLok_User)
              LOCAL lnStatcRec
              SCAN REST WHILE cobj_typ+ALLTRIM(cobj_name)+cuser_id = 'INI'+'OLDVARS'+lcLok_User
                lnStatcRec = RECNO()
                IF RLOCK('syuStatc')
                  UNLOCK RECORD lnStatcRec IN  syuStatc
                  lLok_It    = .T.
                ELSE
                  UNLOCK RECORD lnStatcRec
                  lcLok_User = oAriaApplication.getUserName(lcLok_User)
                  *** Record is in use by user ????
                  *SET DATASESSION TO (lnDataSession)
                  IF  gfModalGen("INM00028B00015","ALERT",lcLok_User) = 1
                    llLoop = .T.
                  ENDIF
                  lLok_It    = .F.
                  lRet_Flag  = .F.
                  EXIT
                ENDIF
              ENDSCAN
              DIMENSION laTableUpdate[1]
              laTableUpdate[1] =  loformset.syuStatc
              =lfTableUpdate()
            ELSE
              lLok_It    = .T.
            ENDIF
          ENDIF
          * Return the old value of reprocess.
          SET REPROCESS TO  lnOldrpSt
          *SET DATASESSION TO (lnDataSession)
          IF llLoop
            LOOP
          ENDIF

        ENDIF
      ELSE
        *** Display the message "Record is in use by another"
        *SET DATASESSION TO (lnDataSession)
        IF gfModalGen("INM00029B00015","ALERT") = 1
          LOOP
        ENDIF
        lLok_It    = .F.
        lRet_Flag  = .F.
      ENDIF
    ENDIF

  ELSE
    *** Chek if these three field in the file structur
    IF TYPE ('cLok_User') <> "U" .AND. ;
        TYPE ('dLok_Date') <> "U" .AND. ;
        TYPE ('cLok_Time') <> "U"
      SELECT (lcFileName)
      *** Unlock the record
      lcFile.REPLACE("lLok_Stat WITH .F. , cLok_User WITH ''  , dLok_Date WITH {}  , cLok_Time WITH ''")
      lRet_Flag = .T.
    ENDIF
  ENDIF
  EXIT
ENDDO
*** Chek if you have to lock the record or not
*SET DATASESSION TO (lnDataSession)
IF lLok_It
  *** Chek if these three field in the file structur
  IF TYPE ('cLok_User') <> "U" .AND. ;
      TYPE ('dLok_Date') <> "U" .AND. ;
      TYPE ('cLok_Time') <> "U"
    *** Lock the record for loformset user with date and time
    lcFile.REPLACE("lLok_Stat  WITH .T.       , ;
		    cLok_User  WITH '*SOORD*' ,;
            cEdit_User WITH oAriaApplication.User_ID , ;
            dLok_Date  WITH DATE()    , ;
            cLok_Time  WITH gfGetTime()")
    lRet_Flag  = .T.
  ENDIF
ENDIF
SELECT (lcFileName)
UNLOCK

DIMENSION laTableUpdate[1]
laTableUpdate[1] = lcFile
IF !lfTableUpdate()
  RETURN .F.
ENDIF

SELECT (lnAlias)

RETURN lRet_Flag

*:**************************************************************************
*:* Name        : lfVsulLbl
*:* Developer   : WLD - Waleed Hamed
*:* Date        : 10/16/2006
*:* Purpose     : Calling  Visual UCC128 Label Report E302315
*:***************************************************************************
*:* Called from : lfSavCartn  and lfvLblInfo
*:***************************************************************************
*:* Parameters : loFormSet,lcVersion && Label Version
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfVsulLbl()
*:***************************************************************************
FUNCTION lfVsulLbl
PARAMETERS loFormSet,lcVersion, lcCartons

STORE 0 TO lnChoiceDtl
lcAccount  = PADR(loFormSet.ariaForm1.kbAccount.keytextbox.VALUE,5)
IF loFormSet.llEdiAcc
  *-- Check for the detail Version for current account.
  IF loFormSet.EDIACPRT.SEEK('A' + lcAccount) .AND.;
      loFormSet.EDIPH.SEEK(EDIACPRT.cPartCode) .AND. ;
      Ediph.lDtlbl
    loFormSet.llDetLabel = .T.
    loFormSet.lcDetailVr = Ediph.cDtlbl
  ELSE
    loFormSet.llDetLabel = .F.
  ENDIF
ELSE
  *- Function to get the number of XX? into array.
  STORE '' TO loFormSet.lcDetailVr , loFormSet.lcDetLbAll
  DIMENSION laVersn[1]
  lnVerCount = lfGetVerXX(loFormSet)
  loFormSet.llDetLabel = (lnVerCount >= 1)
ENDIF
IF !loFormSet.llEdiAcc .AND. lnVerCount >= 1
  *-- the customer should print one from the current version.
  *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
  *= lfSeleVer(loFormSet,laVersn)
  = lfSeleVer(loFormSet,@laVersn)
  *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
ENDIF
*Print the detailed label, if needed [Begin]

STORE .F. TO llPrintLbl
IF loFormSet.llDetLabel
  IF EMPTY(loFormSet.lcDetLbAll)
    lnChoiceDtl = gfModalGen('TRM44105B40016' , 'DIALOG' , lcCartons)

    DO CASE
    CASE lnChoiceDtl = 1
      llPrintLbl = .T.
    CASE lnChoiceDtl = 2
      llPrintLbl = .T.
      loFormSet.lcDetLbAll = "Y"
    CASE lnChoiceDtl = 3
      llPrintLbl = .F.
    CASE lnChoiceDtl = 4
      llPrintLbl = .F.
      loFormSet.lcDetLbAll = "N"
    ENDCASE
  ELSE
    llPrintLbl = (loFormSet.lcDetLbAll = "Y")
  ENDIF
ENDIF

SELECT (loFormSet.lcTmAsnShp)
lnEdiRecNo = RECNO()
COPY TO (oAriaApplication.WorkDir+loFormSet.lcPrnAsnShp)
SELECT 0
USE (oAriaApplication.WorkDir+loFormSet.lcPrnAsnShp) EXCLUSIVE
INDEX ON bol_no+pack_no+STR(cart_no,6)+asn_ver TAG (loFormSet.lcPrnAsnShp)
USE IN (loFormSet.lcPrnAsnShp)

tcEDIAct   = lcAccount
tcEDIShp   = loFormSet.lcPrnAsnShp
tcEDICmp   = oAriaApplication.ActiveCompanyID
tcEDIPrtNm = loFormSet.lcSndPort
tcEDIBolNo = Bol_No
tcEDIPckNo = Pack_No
tnEDICrtNo = lcCartons
tcEDIVer   = lcVersion
tlEDIDetLb = llPrintLbl
tcEDIDetVr = IIF(llPrintLbl,loFormSet.lcDetailVr,'')

*! E302567,1 MMT 01/06/2009 Change file paths for SAAS[Start]
*! E302857,1 MMT 04/27/2011 Use New Property to point to the EDI Folder classes[MEDIA][Start]
*!*	  IF oAriaApplication.multiinst
*!*	    lcEdiPath = ALLTRIM(UPPER(SUBSTR(oariaapplication.InstallPath,1,AT('\',oariaapplication.InstallPath,3))))
*!*	  ELSE
*!*	  *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[End]
*!*
*!*	    lcEdiPath = ALLTRIM(UPPER(SUBSTR(oariaapplication.syspath,1,AT('\',oariaapplication.syspath,2))))
*!*
*!*	  *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[Start]
*!*	  ENDIF
lcEdiPath = oAriaApplication.ediinstallationpath
*! E302857,1 MMT 04/27/2011 Use New Property to point to the EDI Folder classes[MEDIA][End]
*! E302567,1 MMT 01/06/2009 Change file paths for SAAS[End]

lcEdiPath  = SUBSTR(lcEdiPath,1,LEN(lcEdiPath)-1)
lcAria4XPPath = ALLTRIM(UPPER(SUBSTR(oAriaApplication.ApplicationHome,1,AT('\',oAriaApplication.ApplicationHome,2))))
lcAria4XPPath = SUBSTR(lcAria4XPPath,1,LEN(lcAria4XPPath)-1)

IF (oAriaApplication.ClassDir+'EDI.VCX' $ SET('classlib'))
  RELEASE CLASSLIB (oAriaApplication.ClassDir+'EDI.VCX')
ENDIF
lcoldAppHome = oAriaApplication.ApplicationHome
lcoldAppRep  = oAriaApplication.ReportHome
lcoldAppBitMapHome = oAriaApplication.BitMapHome
lcoldAppCls = oAriaApplication.ClassDir
lcoldAppScx = oAriaApplication.ScreenHome

*! E302567,1 MMT 01/06/2009 Change file paths for SAAS[Start]
*! E302857,1 MMT 04/27/2011 Use New Property to point to the EDI Folder classes[MEDIA][Start]
*!*	  IF oAriaApplication.multiinst
*!*	    lcEdiPath = ALLTRIM(UPPER(SUBSTR(oariaapplication.syspath,1,AT('\',oariaapplication.syspath,3))))
*!*	  ELSE
*!*	  *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[End]
*!*	     lcEdiPath = UPPER(SUBSTR(oariaapplication.syspath,1,AT('\',oariaapplication.syspath,2)))
*!*	  *! E302567,1 MMT 01/06/2009 Change file paths for SAAS[Start]
*!*	  ENDIF
lcEdiPath  = oAriaApplication.ediinstallationpath
*! E302857,1 MMT 04/27/2011 Use New Property to point to the EDI Folder classes[MEDIA][End]
*! E302567,1 MMT 01/06/2009 Change file paths for SAAS[End]
oAriaApplication.ApplicationHome = lcEdiPath +  'PRGS\'
oAriaApplication.ReportHome = lcEdiPath +  'REPORTS\'
oAriaApplication.BitMapHome = lcEdiPath +  'BMPs\'
oAriaApplication.ClassDir   = lcEdiPath +  'CLASSES\'
oAriaApplication.ScreenHome = lcEdiPath +  'SCREENS\'

SET CLASSLIB TO (oAriaApplication.ClassDir +'EDI.VCX') ADDIT
SET PROCEDURE TO (oAriaApplication.ApplicationHome +'EDIGLOBL.FXP') ADDITIV


oPrnLabel=CREATEOBJECT('PrnLabel',.T.,ALLTRIM(lcAria4XPPath),ALLTRIM(tcEDIAct),ALLTRIM(tcEDIShp) ,;
  ALLTRIM(tcEDIPrtNm), ALLTRIM(tcEDIBolNo), ALLTRIM(tcEDIPckNo),tnEDICrtNo)

oPrnLabel.DO(.T.,,ALLTRIM(lcAria4XPPath),ALLTRIM(tcEDIAct),ALLTRIM(tcEDIShp) ,;
  ALLTRIM(tcEDIPrtNm), ALLTRIM(tcEDIBolNo), ALLTRIM(tcEDIPckNo),tnEDICrtNo,ALLTRIM(tcEDIVer), tlEDIDetLb, ;
  IIF(tlEDIDetLb,ALLTRIM(tcEDIDetVr),''))
RELEASE oPrnLabel

oAriaApplication.ApplicationHome = lcoldAppHome
oAriaApplication.ReportHome      = lcoldAppRep
oAriaApplication.BitMapHome      = lcoldAppBitMapHome
oAriaApplication.ClassDir        = lcoldAppCls
oAriaApplication.ScreenHome      = lcoldAppScx

IF USED(loFormSet.lcPrnAsnShp)
  USE IN (loFormSet.lcPrnAsnShp)
  ERASE (oAriaApplication.WorkDir+loFormSet.lcPrnAsnShp+'.*')
ENDIF

SELECT (loFormSet.lcTmAsnShp)
GOTO lnEdiRecNo

WAIT CLEAR
*-- end of  lfVsulLbl.
*:**************************************************************************
*:* Name        : lfvUccStrc
*:* Developer   : Waleed Hamed (WLD)
*:* Date        : 10/16/2006
*:* Purpose     : Valid function to Ok bnuton in UCC # structure screen E302315
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvUccStrc()
*:***************************************************************************
*E040123
FUNCTION lfvUccStrc
LPARAMETERS loformUCCStr
DO CASE

CASE loformUCCStr.AriaForm1.optgrpUCCStr.VALUE = 1
  loformUCCStr.lnUCCStr = 5
  RETURN 5
CASE loformUCCStr.AriaForm1.optgrpUCCStr.VALUE = 2
  loformUCCStr.lnUCCStr = 6
CASE loformUCCStr.AriaForm1.optgrpUCCStr.VALUE = 3
  loformUCCStr.lnUCCStr = 9
ENDCASE
*-- end of  lfvUccStrc.

*! B608342,1 MMT 11/01/2007 Add Scan option to the manual packing list[Start]
*:**************************************************************************
*:* Name        : lfVScan
*:* Developer   : Mariam Mazhar(MMT)
*:* Date        : 11/01/2007
*:* Purpose     : Valid function to Scan Button
*:***************************************************************************
FUNCTION lfVScan
PARAMETERS loFormset
lnAlias = SELECT(0)
lfvCtDtNew(loFormset,.T.)

loFormSet.ariaForm1.pgfPacking.CartonInfo.kbStyle.ENABLED = .F.
loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.ENABLED = .T.
loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.VALUE = ''
loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.SETFOCUS
loFormSet.AriaForm1.pgfPacking.CartonInfo.kbStyle.VALUE = ''
loFormSet.AriaForm1.pgfPacking.CartonInfo.kbSize.VALUE =''
loFormSet.AriaForm1.pgfPacking.CartonInfo.txtCartQtyD.VALUE = 0
loFormSet.AriaForm1.pgfPacking.CartonInfo.txtCartWghD.VALUE = 0
*C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [Begin]
IF ASCAN(loFormSet.laEvntTrig,PADR('UPCFRMT',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
  =loFormSet.mDoTrigger(PADR('UPCFRMT',10))
ENDIF
*C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [End]

SELECT(lnAlias)
*:**************************************************************************
*:* Name        : lfvUPC
*:* Developer   : Mariam Mazhar(MMT)
*:* Date        : 11/01/2007
*:* Purpose     : Valid function to Scanned Number entered
*:***************************************************************************
FUNCTION lfvUPC
PARAMETERS loFormset,lcUpc

PRIVATE lnRecNo,lnPicked,lnOrdered ,lnAlias ,lcOldOrder

lcOldOrder = ''
lnAlias = SELECT(0)
lcUpc = PADR(ALLTRIM(lcUpc),13)
*-- Clear keyboard buffer as the scanner somtimes issue CHR(13) causing any

CLEAR TYPEAHEAD


lcStyUpcOrd = ORDER('STYLEUPC')
lcOrdlineOrd = ORDER('ORDLINE')
loFormSet.STYLEUPC.SetOrder('STYUPCN')
loFormSet.OrdLine.SetOrder('OrdLinSt')

lcOrderNo = loFormSet.AriaForm1.kbOrderNo.keytextbox.VALUE
lcStore   = loFormSet.AriaForm1.kbStore.keytextbox.VALUE
lcPackNo   = PADR(loFormSet.AriaForm1.kbPkTktNo.keytextbox.VALUE ,6)

*B609658,1 WAM 08/09/2011 Get packing list account
lcAccount = PADR(loFormSet.ariaForm1.kbAccount.keytextbox.VALUE ,5)
*B609658,1 WAM 08/09/2011 (End)

IF !EMPTY(lcUpc)
  *C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [Begin]
  IF ASCAN(loFormSet.laEvntTrig,PADR('AUTOSCAN',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
    lcType_INF = ''
    llRtn_INF = .F.
    =loFormSet.mDoTrigger(PADR('AUTOSCAN',10))
    IF llRtn_INF
      RETURN
    ENDIF
  ENDIF
  *C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [End]

  IF !loFormSet.STYLEUPC.SEEK(lcUpc)
    *- Message Text   :- UPC XXXX is not found.
    *- Message No.    :- 44065.
    *- Buttom Message :- Ok
    *- Buttom Number  :- 00000
    =gfModalGen('INM44065B00000','DIALOG',lcUpc)
    loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.VALUE = SPACE(12)

    loFormSet.STYLEUPC.SetOrder(lcStyUpcOrd)
    loFormSet.OrdLine.SetOrder(lcOrdlineOrd)
    lfSetControlSource(loFormSet)
    SELECT(lnAlias)
    RETURN
  ENDIF

  *B609658,1 WAM 08/09/2011 Validate scanned upc for pack and style\color\size
  *!*	    SELECT OrdLine
  *!*	    =loFormSet.OrdLine.SEEK('O'+lcOrderNo+lcStore+StyleUpc.STYLE)
  *!*	    LOCATE REST WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = ;
  *!*	      'O'+lcOrderNo+lcStore+StyleUpc.STYLE;
  *!*	      FOR PikTkt = lcPackNo
  *!*	    IF !FOUND()

  llValidUpc = .T.
  IF StyleUpc.lUpcPack
    loFormSet.Spck_Lin.SetOrder('spck_lin')
    IF loFormSet.Spck_Lin.SEEK('P'+lcAccount + LEFT(StyleUpc.STYLE,16)) OR loFormSet.Spck_Lin.SEEK('P'+'*****' + LEFT(StyleUpc.STYLE,16))
      SELECT SPCK_LIN
      lcAccount = Account
      lnPackQty = 0
      SCAN REST WHILE TYPE+account+pack_id+STYLE+dyelot = 'P'+lcAccount + LEFT(StyleUpc.STYLE,16)
        lnPackQty = lnPackQty + TOTQTY
        SELECT OrdLine
        =loFormSet.OrdLine.SEEK('O'+lcOrderNo+lcStore+SPCK_LIN.STYLE)
        LOCATE REST WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+lcOrderNo+lcStore+SPCK_LIN.STYLE;
          FOR PACK_ID = LEFT(StyleUpc.STYLE,16) AND PikTkt = lcPackNo
        IF !FOUND()
          llValidUpc = .F.
          EXIT
        ENDIF
      ENDSCAN
    ENDIF
  ELSE
    SELECT OrdLine
    =loFormSet.OrdLine.SEEK('O'+lcOrderNo+lcStore+StyleUpc.STYLE)
    LOCATE REST WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = ;
      'O'+lcOrderNo+lcStore+StyleUpc.STYLE;
      FOR PikTkt = lcPackNo
    llValidUpc =FOUND()
  ENDIF
  IF !llValidUpc
    *B609658,1 WAM 08/09/2011 (End)

    *- Message Text   :- UPC XXX is not found in piktkt# XXX.
    *- Message No.    :- 44066.
    *- Buttom Message :- Ok
    *- Buttom Number  :- 00000
    =gfModalGen('INM44066B00000','DIALOG',lcUpc+'|'+lcPackNo)

    loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.VALUE = SPACE(12)
    loFormSet.STYLEUPC.SetOrder(lcStyUpcOrd)
    loFormSet.OrdLine.SetOrder(lcOrdlineOrd)
    lfSetControlSource(loFormSet)
    SELECT(lnAlias)
    RETURN
  ENDIF

  llCUpdate  = .T.
  SELECT (loFormSet.lcCtnDtl)
  lnRecNo = RECNO()
  SET KEY TO

  *B609658,1 WAM 08/09/2011 get packed quantity for scanned pack
  IF StyleUpc.lUpcPack
    SUM TotQty TO lnPacked FOR PACK_ID = LEFT(StyleUpc.STYLE,16)
    lnPacked=lnPacked/lnPackQty
  ELSE
    *B609658,1 WAM 08/09/2011 (End)
    SUM TotQty TO lnPacked FOR cUPc = lcUpc

    *B609658,1 WAM 08/09/2011 get packed quantity for scanned pack
  ENDIF
  *B609658,1 WAM 08/09/2011 (End)
  SET KEY TO STR(EVALUATE(loFormSet.lcCtnHdr+'.Cart_No'),4)
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GO lnRecNo
  ENDIF
  *C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [Begin]
  IF ASCAN(loFormSet.laEvntTrig,PADR('SNDALLQTY',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
    =loFormSet.mDoTrigger(PADR('SNDALLQTY',10))
  ENDIF
  *C200945,1 WLD Custom Scan and Pack Program  INF10 02/24/2008 [End]

  SELECT OrdLine
  STORE 0 TO lnPicked,lnOrdered

  *B609658,1 WAM 08/09/2011 get picked and ordered quantity for scanned pack
  IF StyleUpc.lUpcPack
    =loFormSet.OrdLine.SEEK('O'+lcOrderNo+lcStore)
    SCAN REST WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = ;
        'O'+lcOrderNo+lcStore FOR PACK_ID = LEFT(StyleUpc.STYLE,16) AND PikTkt = lcPackNo
      lnPicked  = lnPicked  + TOTPIK
      lnOrdered = lnOrdered + TOTQTY
    ENDSCAN
    lnPicked  = lnPicked /lnPackQty
    lnOrdered = lnOrdered/lnPackQty
  ELSE
    *B609658,1 WAM 08/09/2011 (End)

    =loFormSet.OrdLine.SEEK('O'+lcOrderNo+lcStore+StyleUpc.STYLE)
    SCAN REST WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = ;
        'O'+lcOrderNo+lcStore+StyleUpc.STYLE FOR PikTkt = lcPackNo
      lnPicked  = lnPicked  + EVAL('PIK'+ALLTRIM(StyleUpc.SIZE))
      lnOrdered = lnOrdered + EVAL('QTY'+ALLTRIM(StyleUpc.SIZE))
      IF lnPacked+1 <= lnPicked
        EXIT
      ENDIF
    ENDSCAN
    *B609658,1 WAM 08/09/2011 get picked and ordered quantity for scanned pack
  ENDIF
  *B609658,1 WAM 08/09/2011 (End)

  =loFormSet.STYLE.SEEK(StyleUpc.STYLE,'Style')
  =loFormSet.SCALE.SEEK('S'+STYLE.SCALE)
  *- Message Text   :- Packed Quantity for Style/Size XXXXX exceeds
  *- Message Text   :- Ordered Quantity. Can not modify the Ordered Quantity?
  *- Message No.    :- 44112.
  *- Buttom Message :- Ok
  *- Buttom Number  :-00000

  IF lnPacked+1 > lnOrdered
    =loFormSet.OrdLine.SEEK('O'+lcOrderNo+lcStore+StyleUpc.STYLE)
    = gfModalGen('INM44112B00000','DIALOG',EVALUATE(loFormSet.lcCtnDtl+'.Style')+'/'+EVAL('SCALE.SZ'+ALLTRIM(StyleUpc.SIZE)))
    loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.VALUE = SPACE(12)
    loFormSet.STYLEUPC.SetOrder(lcStyUpcOrd)
    loFormSet.OrdLine.SetOrder(lcOrdlineOrd)
    lfSetControlSource(loFormSet)
    SELECT(lnAlias)
    RETURN
  ENDIF

  IF lnPacked > lnPicked
    *- Message Text   :- Packed quantity for ð exceeds picked quantity.
    *- Message Text   :- Do you want to add in the packing list?
    *- Message No.    :- 44073.
    *- Buttom Message :- Ok
    *- Buttom Number  :-00000
    IF gfModalGen('QRM44073B44009','DIALOG',StyleUpc.STYLE+'\'+EVAL('SCALE.SZ'+ALLTRIM(StyleUpc.SIZE)) ) = 2
      loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.VALUE = SPACE(12)
      loFormSet.STYLEUPC.SetOrder(lcStyUpcOrd)
      loFormSet.OrdLine.SetOrder(lcOrdlineOrd)
      lfSetControlSource(loFormSet)
      SELECT(lnAlias)
      RETURN
    ENDIF

    SELECT OrdLine
    =loFormSet.OrdLine.SEEK('O'+lcOrderNo+lcStore+StyleUpc.STYLE)
    LOCATE REST WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = ;
      'O'+lcOrderNo+lcStore+StyleUpc.STYLE;
      FOR PikTkt = lcPackNo
  ENDIF
  *! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[Start]
  SELECT OrdLine
  =loFormSet.OrdLine.SEEK('O'+lcOrderNo+lcStore+StyleUpc.STYLE)
  LOCATE REST WHILE cOrdType+ORDER+STORE+STYLE+STR(LINENO,6) = ;
    'O'+lcOrderNo+lcStore+StyleUpc.STYLE;
    FOR PikTkt = lcPackNo

  *! B608342,1 MMT 11/01/2007 Add Scan Option to the manual packing list[End]
  SELECT (loFormSet.lcCtnDtl)

  *B609658,1 WAM 08/09/2011 Pass new parameter to indicate if the scanned upc for pack
  *lfvCtnSty(loFormSet,.F.,StyleUpc.STYLE,.T.)
  lfvCtnSty(loFormSet,.F.,StyleUpc.STYLE,.T.,StyleUpc.lUpcPack)
  *B609658,1 WAM 08/09/2011 (End)

  loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.ENABLED  = .T.
  loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.VALUE = SPACE(12)
  loFormSet.STYLEUPC.SetOrder(lcStyUpcOrd)
  loFormSet.OrdLine.SetOrder(lcOrdlineOrd)
  SELECT(lnAlias)
ELSE
  loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.ENABLED  = .T.
  loFormSet.ariaForm1.pgfPacking.CartonInfo.txtUPC.VALUE = SPACE(12)
  loFormSet.STYLEUPC.SetOrder(lcStyUpcOrd)
  loFormSet.OrdLine.SetOrder(lcOrdlineOrd)
  lfSetControlSource(loFormSet)
  SELECT(lnAlias)
  RETURN
ENDIF
loFormSet.STYLEUPC.SetOrder('STYLEUPC')
SELECT (lnAlias)
*C200945,1 WLD Fix  bug of pressing enter when Scan using the Upc  (PRG,Screen) 02/21/2008 [Begin]
*--Restore confirmation setting.
lcFrmCnfrm = loFormSet.lcSetConf
SET CONFIRM &lcFrmCnfrm
*C200945,1 WLD [End]


*:**************************************************************************
*:* Name        : lfSetControlSource
*:* Developer   : Mariam Mazhar(MMT)
*:* Date        : 11/01/2007
*:* Purpose     : function to Set Control source
*:***************************************************************************
FUNCTION  lfSetControlSource
PARAMETERS loFormSet
WITH loFormSet.AriaForm1.pgfPacking.CartonInfo
  .kbStyle.CONTROLSOURCE       = loFormSet.lcCtnDtl + '.Style'
  .kbConfiguration.CONTROLSOURCE   = loFormSet.lcCtnDtl + '.Dyelot'
  .kbSize.CONTROLSOURCE        = loFormSet.lcCtnDtl + '.Size'
ENDWITH



*:**************************************************************************
*:* Name        : lfReSetControlSource
*:* Developer   : Mariam Mazhar(MMT)
*:* Date        : 11/01/2007
*:* Purpose     : function to Reset conrol source
*:***************************************************************************
FUNCTION  lfReSetControlSource
PARAMETERS loFormSet
WITH loFormSet.AriaForm1.pgfPacking.CartonInfo
  .kbStyle.CONTROLSOURCE       = ""
  .kbConfiguration.CONTROLSOURCE = ""
  .kbSize.CONTROLSOURCE        = ""
ENDWITH
*! B608342,1 MMT 11/01/2007 Add Scan option to the manual packing list[End]

*:*************************************************************
*: Name      : lfWUpc
*: Developer : Walid Hamed - (WLD)
*: Date      : 07/12/2005
*: Purpose   : Function to make the Upc Validated without Enter
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfWUpc ()
*:*************************************************************
*: C200945,1 WLD 02/21/2008 Fix  bug of pressing enter when Scan using the Upc  (PRG,Screen)
FUNCTION lfWUpc
PARAMETERS loFormSet

lcCnfrm = IIF(loFormSet.llConfirm,'ON','OFF')
SET CONFIRM &lcCnfrm
*-- End Of lfWUpc.
*: C200945,1 WLD [End]
*! E302698,1 MMT 05/25/2010 Display SKU# in Style Detail and Carton Detail grids[Start]
*:*************************************************************
*: Name      : lfGetSKU
*: Developer : Mariam Mazhar(MMT)
*: Date      : 05/25/2010
*: Purpose   : Function to get SKU#
*! E302698,1
*:*************************************************************
FUNCTION lfGetSKU
PARAMETERS  loFormSet
lcOldSelect = SELECT(0)
lcAccount = PADR(loFormSet.ariaForm1.kbAccount.keytextbox.VALUE ,5)
lcSzCnt = ALLTRIM(EVALUATE(loFormSet.lcPckLin+'.CSIZENO'))
lcSku = ''
SELECT Spck_lin
lcOldOrder = ORDER()
loFormSet.SPCK_LIN.SetOrder('SPKLNSTCN')
IF !loFormSet.SPCK_LIN.SEEK('S'+lcAccount+EVALUATE(loFormSet.lcPckLin+'.style')+EVALUATE(loFormSet.lcPckLin+'.Dyelot'),'SPKLNSTCN')
  loFormSet.SPCK_LIN.SetOrder(lcOldOrder)
  SELECT(lcOldSelect)
  RETURN ''
ELSE

  lcSku = Spck_lin.Pack_id
  LOCATE REST WHILE TYPE+ACCOUNT+STYLE+DYELOT+PACK_ID=;
    'S'+lcAccount+EVALUATE(loFormSet.lcPckLin+'.style')+EVALUATE(loFormSet.lcPckLin+'.Dyelot') FOR ;
    QTy&lcSzCnt. = 1
  IF FOUND()
    lcSku = Spck_lin.Pack_id
  ENDIF
  loFormSet.SPCK_LIN.SetOrder(lcOldOrder)
  SELECT(lcOldSelect)
  RETURN lcSku
ENDIF
*! E302698,1 MMT 05/25/2010 Display SKU# in Style Detail and Carton Detail grids[End]
