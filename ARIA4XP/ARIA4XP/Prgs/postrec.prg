***********************************************************************
*:  Program file : POSTREC.PRG
*:  Program desc.: Receving work orders
*:         System: Aria 4XP
*:      Developer: Khalid Mohi El-Din Mohamed
*:           Date: 09/11/2004
*:      Reference: *N037578,1
*:************************************************************************
*: Passed Parameters  :lcPType => 'I' Receive P/O
*:                                'S' Receive by Shipment
*:                                'B' Receive P/O Batch
*:                                'R' Issue Return P/o
*:                                'M' Receive C/T
*:                                'T' Receive C/T Batch
*:                                'N' Issue Inter-Location P/O
*:                                'U' Issue Inter Location P/O Shipment
*:                                'H' Issue Inter Location P/O Batch
*:                                'O' Receive Inter-Location P/O
*:                                'C' Receive Inter Location P/O Shipment
*:                                'L' Receive Inter Location P/O Batch
*:                                'D' Receive Dye Order
*:                                'A' Issue Adornment order
*:                                'E' Receive Adornment order
*:                                'P' Receive Material PO
*:                                'F' Receive Material PO Shipment
*:				  			  'G' Issue Return Material PO
*:  						      'W' Receive MMO
*:*************************************************************************
*: Modifications      :
*: B125331,1 KHM 11/18/2004 Add feature of edit landed cost
*: B128055,1 AMH 05/18/2005 Fix bug of Connection Error while getting vendor
*:                          information remotely.
*: B128070,1 KHM 05/19/2005 Fix bug of changing the status of shipment to
*: B128070,1                complete when receiving partially.
*: B607585,1 AMH 05/25/2005 Add cwarecode to POSLN index and nLineNo to BOMLINE file.
*: E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File.
*: B607880,1 SSH 12/17/2006 Replace TotQtyHdr WIth ZERO in case recieve completely
*: B607881,1 WAM 12/17/2006 Fix Error "File is in use" when two users open the screen
*: B607927,1 TMI 01/11/2007 Fix a bug that the pointer is not located on the correct record, the reason for that was not passing
*:                          the while condition in the ARIABROW global function   ( ticket # T20061011.0009 )
*: B607935,1 WAM 01/17/2007 Fix error "variable lcTmpItm not found' while saving style PO
*: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   T20070117.0001
*: N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP T20070206.0007
*: B608042,1 MMT 04/15/2007 fix bug of wrong dates while calling reports from receiving screen T20070404.0012
*: B608366,1 MMT 11/29/2007 fix bug of not adding record for last MFG in operation dt File[T20070926.0183]
*: E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[T20061128.0001]
*: N000587,1 WAM 12/01/2007 Get equivalent cost from BOM table for each cost element based on saved currency code, exchange
*: N000587,1 WAM 12/01/2007 rate and unit.
*: C200919,1 MMT 01/27/2008 Add trig. to PO price ex. Rate while Receiving[T20070919.0020]
*: B608491,1 WAM 03/25/2008 Get the currency and exchange rates form the BOMLINE to calculate the landed cost in
*: B608491,1 WAM 03/25/2008 base currency when receive by shipment      [T20080317.0001]
*: C200876,1 TMI 05/01/2008 Adding the BIN location triggers ( T20060818.0001)
*: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[T20061128.0001]
*: B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[T20080513.0017]
*! B608617,1 MMT 07/14/2008 Add trigger to update bomline with new ex rate            [T20080624.0001]
*! B608643,1 WAM 08/05/2008 Get cost from estimated records if some of the POs in the shipment don't have adjusted cost
*! B608643,1 WAM 08/05/2008 for receiving records [T20080801.0001]
*! B608645,1 WAM 08/07/2008 Get the receiving cost from the style master table when the system is setup to use Standard
*! B608645,1 WAM 08/07/2008 cost as a costing methods [T20080408.0016]
*! C200876,1 TMI 09/11/2008 T20071102.0018,10/C200876
*! B608709,1 MMT 10/07/2008 Fix error while receiving PO manually[t20081001.0002]
*! B608718,1 WAM 10/09/2008 take into consideration cost per size when calculating the equivalent
*! B608718,1 WAM 10/09/2008 landed cost [t20081001.0002]
*! B608760,1 WAM 12/04/2008 While receive by PO or by shipment, Get foreign cost from the adjusted
*! B608760,1 WAM 12/04/2008 record for receiving [T20080929.0004]
*! B608762,1 WAM 12/15/2008 Consider receiving cut ticket line into more than one warehouse [T20081215.0003]
*! B608762,1 WAM 12/15/2008 Add new field CWARECODE in the BOMLINE file. increament the NLINENO field in BOMLINE [T20081215.0003]
*! B608773,1 WAM 12/24/2008 Update cost of good variance GL account when landed cost is difference that style standard
*! B608773,1 WAM 12/24/2008 cost when the system is setup to use Standard cost [T20081128.0001]
*! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [T20090122.0014]
*! B608824,1 MMT 03/30/2009 Fix bug of Wrong Values while adjustemnt Material[T20090303.0017]
*! B608834,1 HES 04/01/2009 Add Standared Fields for POMLINE File[T20081001.0001]
*! B608845,1 WAM 04/09/2009 Fix Error while updating error while saving [T20090317.0071]
*! C201141,1 HES 04/26/2009,please quote Processing Enbroidery orders for DCC [T20080808.0019]
*! B609048,1 WAM 10/19/2009 PO receipt update GLDIST incorrectly using Standard Cost [T20090818.0006]
*! B609051,1 HES 10/20/2009 error receiving PO receipts [T20090914.0001]
*! B609068,1 WAM 10/29/2009 Update CWARECODE in BOMLINE when there are adjust records for receiving  [T20091023.0004]
*! C201211,1 HES 01/20/2010 Handle Scanned Barcodes from Recieve PO Screen [T20100107.0022]
*! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [T20100107.0022]
*! C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [T20091130.0004]
*! B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[T20090430.0001]
*! B609295,1 TMI 06/14/2010 use "loFormSet.mDoTrigger" instead the old function gfDotriger to call the POTRAN for Blum & Fink [T20100608.0012]
*! B609297,1 TMI 06/14/2010 Add the SYS(1104)
*! B609377,1 MMT 08/09/2010 Material PO Receiving-rolls not matching total[T20100706.0020]
*! B609517,1 TMI 02/05/2011 Error when receiving into two locations in the same session [T20110117.0034]
*! B609634,1 MMT 06/28/2011 Fix bug of Zero Cost in Styinvjl in Case of receiving Interlocation PO[T20110617.0001]
*! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [T20101208.0008]
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[T20110521.0001]
*! B609696,1 MMT 10/13/2011 Fix bug of wrong updates of the Field apvendor.nVenOpnPo [T20110622.0022]
*! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [T20110621.0044]
*! B609728,1 MMT 11/14/2011 Fix bug of Freezing in Scan barcode screen after using Scanned batch in PO Screen[T20110521.0001]
*! E303006,1 SAB 12/03/2011 Enable Issue From BINs in Inter Location PO [T20110621.0044]
*! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[T20100922.0014]
*! C201407,1 SAB 03/20/2012 Fix problem of Tran_type in GLDIST should be PO in receiveing case [T20110621.0044]
*! E303103,2 MMT 04/05/2012 Add trigger in PO receiving screen for [T20120314.0001]
*! E303079,1 MMT 06/28/2012 Fixing Media issues[T20120304.0004]
*! B610051,1 MMT 08/16/2012 Receiving Inter-location PO to different location other than target location on PO gives error if Bin location is used[T20120816.0012]
*! E303177,1 HIA 07/28/2012 Get the batch creation date from the screen as per specs [T20120618.0015]
*! N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019]
*! B610107,1 MMT 10/04/2012 Add Foreign currency costing to Material MFG order summary[T20110914.0019]
*! B610163,1 HIA 11/27/2012 Error updateing, while receving material PO [T20121112.0027]
*! B610260,1 HES 02/27/2013 Fix bug of not fund table in the current area when the Dyelot not found for the used style location [T20130214.0006]
*! B610469,1 TMI 08/19/2013 when the user receives a line for more than location in the same session don't add lines with empty session more than once in the lcTmpBomLn [T20130612.0013] 
*! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017]
*! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017]
*! B610965,1 MMT 03/12/2015 Error while editing line Qty[T20150215.0015]
*:********************************************************************************
*! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
*PARAMETERS lcInvType, lcPType
PARAMETERS lcInvType, lcPType, lcObjName
*! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

#INCLUDE R:\aria4xp\prgs\postrec.h

*-- Initialize the variables in order to define it as properties in the formset.
PRIVATE lcAllVar

*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [START]
*lcAllVar = "lcInvType,lcPType,lcInvType,llMFCall,llLinkToGl,llWareHous,llWareLoc,llDyelot,"+;
"llFabDye,llImpCost,lcCostImp,lcGLFYear,lcGLPeriod,lnTotStk,lnTotDam,lnTotCan,"+;
"llIgnorAll,llConfig,llMulCurr,llEditExRt,lcCur1,lcCur2,lnRate1,lnRate2,"+;
"lnCurrUnt1,lnCurrUnt2,lcCostMth,llCostPrv,lcTmpLine,lcTemLoc,lcPosHdr,lcPosLn,"+;
"lnWare,laWare,llCMInstld,llPOSale,lnMjrWid,lcDropLoc,llUseMCurr,laECost,"+;
"lcBusDoc,lcWorkOrd,lcCTktBom,lcBomCost,lnPolstln,llByCarton,lcCostMthM,"+;
"lcIType1,lcIType2,lcIType3,lcIType4,lcIType5,lcIType6,lcIType7,llFirst,llIssue,"+;
"lcAuto,llFirstTmp,lcBomHdr,lcTempFile, lcOldPoNo, lcTmpItem,lcItemLoc,lcMastShp,"+;
"cbrowsefields,lcPoNo,lcStyle,lcDyelot,laLotArry,llSpecLot,llNewItem,lcLotNo,"+;
"lcClrLstOp,lcWareHFil,lcTmpCurs,llEditLCst"

*N000587,1 WAM 12/01/2007 Get equivalent cost from BOM table for each cost element based on saved currency code
*lcAllVar = "lcInvType,lcPType,lcInvType,llMFCall,llLinkToGl,llWareHous,llWareLoc,llDyelot,"+;
"llFabDye,llImpCost,lcCostImp,lcGLFYear,lcGLPeriod,lnTotStk,lnTotDam,lnTotCan,"+;
"llIgnorAll,llConfig,llMulCurr,llEditExRt,lcCur1,lcCur2,lnRate1,lnRate2,"+;
"lnCurrUnt1,lnCurrUnt2,lcCostMth,llCostPrv,lcTmpLine,lcTemLoc,lcPosHdr,lcPosLn,"+;
"lnWare,laWare,llCMInstld,llPOSale,lnMjrWid,lcDropLoc,llUseMCurr,laECost,"+;
"lcBusDoc,lcWorkOrd,lcCTktBom,lcBomCost,lnPolstln,llByCarton,lcCostMthM,"+;
"lcIType1,lcIType2,lcIType3,lcIType4,lcIType5,lcIType6,lcIType7,llFirst,llIssue,"+;
"lcAuto,llFirstTmp,lcBomHdr,lcTempFile, lcOldPoNo, lcTmpItem,lcItemLoc,lcMastShp,"+;
"cbrowsefields,lcPoNo,lcStyle,lcDyelot,laLotArry,llSpecLot,llNewItem,lcLotNo,"+;
"lcClrLstOp,lcWareHFil,lcTmpCurs,llEditLCst,llTrkRolls"

lcAllVar = "lcInvType,lcPType,lcInvType,llMFCall,llLinkToGl,llWareHous,llWareLoc,llDyelot,"+;
  "llFabDye,llImpCost,lcCostImp,lcGLFYear,lcGLPeriod,lnTotStk,lnTotDam,lnTotCan,"+;
  "llIgnorAll,llConfig,llMulCurr,llEditExRt,lcCur1,lcCur2,lnRate1,lnRate2,"+;
  "lnCurrUnt1,lnCurrUnt2,lcCostMth,llCostPrv,lcTmpLine,lcTemLoc,lcPosHdr,lcPosLn,"+;
  "lnWare,laWare,llCMInstld,llPOSale,lnMjrWid,lcDropLoc,llUseMCurr,laECost,"+;
  "lcBusDoc,lcWorkOrd,lcCTktBom,lcBomCost,lnPolstln,llByCarton,lcCostMthM,"+;
  "lcIType1,lcIType2,lcIType3,lcIType4,lcIType5,lcIType6,lcIType7,llFirst,llIssue,"+;
  "lcAuto,llFirstTmp,lcBomHdr,lcTempFile, lcOldPoNo, lcTmpItem,lcItemLoc,lcMastShp,"+;
  "cbrowsefields,lcPoNo,lcStyle,lcDyelot,laLotArry,llSpecLot,llNewItem,lcLotNo,"+;
  "lcClrLstOp,lcWareHFil,lcTmpCurs,llEditLCst,llTrkRolls,lcMastBomLn"

*N000587,1 WAM 12/01/2007 (End)

*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [END]

*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
lcAllVar = lcAllVar +  ',llApproveBatch,lcbatchdet,laUsedBatchPO,laStatusArr,Saveorpost'
DIMENSION laStatusArr[1,2],laUsedBatchPO[1,2]
STORE '' TO laStatusArr,laUsedBatchPO
STORE .F. TO llApproveBatch
STORE '' TO lcbatchdet,Saveorpost
lcBatchDet = gfTempName()
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]

*! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
lcAllVar = lcAllVar + ',llSilentMod'
*! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

DIMENSION laAllVar[1,1]
STORE '' TO laAllVar
=gfSubStr(lcAllVar,@laAllVar,',')

LOCAL lnI
FOR lnI = 1 TO ALEN(laAllVar,1)
  PRIVATE &laAllVar[lnI,1].
ENDFOR

*-- This paramter is to indicate the receiving type. If calling the program directly from
*-- menu without passing parameters then this means you are receiving style P/O
lcPType   = IIF(TYPE('lcPType') $ 'UL','I',lcPType)
lcInvType = IIF(TYPE('lcInvType') $ 'UL','0001',lcInvType)

*--Global program variable flag indicate if it P/O or C/T receive.
llMFCall = ( lcPType $ 'MT' )

*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
llApproveBatch = IIF(llMFCall,gfUserPriv('MF','MFRCVCT   ','APPBTCH'),gfUserPriv('PO','POSTREC','APPBTCH'))
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
*-- Assign the business document and work order type
STORE '' TO lcBusDoc,lcWorkOrd,lcPoNo,lcStyle,lcDyelot
DIMENSION laSetups[23,2]
laSetups[1,1]  = 'M_WareHouse' &&-- Use multi location (IC)
laSetups[2,1]  = 'M_WareLoc'   &&-- Keep Track of bins (IC)
laSetups[3,1]  = 'M_Dyelot'    &&-- Use Dyelot (IC)
laSetups[4,1]  = 'M_MATDYE'    &&-- Use Dyelot (MA)
laSetups[5,1]  = 'M_Cost_Met'  &&-- Costing Method (IC)
laSetups[6,1]  = 'M_Link_GL'   &&-- Link to GL (SM)
laSetups[7,1]  = 'M_DROPWARE'  &&-- Drop down location (IC)
laSetups[8,1]  = 'M_SYSTYPE'   &&-- System Type 'P' for Point of sale (SM)
laSetups[9,1]  = 'M_LImpCost'  &&-- Use Detail Costing (PO)
laSetups[10,1] = 'llMulCurr'   &&-- Multi currency (SM)
laSetups[11,1] = 'llEditExRa'  &&-- Change exch. rates (SM)
laSetups[12,1] = 'M_cCostImp'  &&-- Receive Style PO by PO/Shipment (PO)
laSetups[13,1] = 'M_cIType1'   &&-- Style Imported Cost Element Type 1 (PO)
laSetups[14,1] = 'M_cIType2'   &&-- Style Imported Cost Element Type 2 (PO)
laSetups[15,1] = 'M_cIType3'   &&-- Style Imported Cost Element Type 3 (PO)
laSetups[16,1] = 'M_cIType4'   &&-- Style Imported Cost Element Type 4 (PO)
laSetups[17,1] = 'M_cIType5'   &&-- Style Imported Cost Element Type 5 (PO)
laSetups[18,1] = 'M_cIType6'   &&-- Style Imported Cost Element Type 6 (PO)
laSetups[19,1] = 'M_cIType7'   &&-- Style Imported Cost Element Type 7 (PO)
laSetups[20,1] = 'M_GenStOrN'  &&-- Generate PO manually/Automatic (PO)
laSetups[21,1] = 'M_STYCNFG'   &&-- Style Configuration
laSetups[22,1] = 'M_MATCSTMT'  &&-- Costing Method (MA)
laSetups[23,1] = 'M_TrkRolls'  &&-- Keep track of rolls (MA)

llGenOrNum = .F.
=gfGetMemVar(@laSetups,oAriaApplication.ActiveCompanyID)

*-- Use multi location
llWareHous = (laSetups[1,2]='Y')
*-- Keep Track of bins
llWareLoc  = (laSetups[2,2]='Y')
*-- Use Dyelot (IC)
llDyelot   = (laSetups[3,2]='Y')
*-- Use Dyelot (MA)
llFabDye   = (laSetups[4,2]='Y')
*-- Costing Method (IC)
lcCostMth  = laSetups[5,2]
*-- Link to GL
llLinkToGl = (laSetups[6,2] = 'Y')
*-- Drop down location
lcDropLoc  = laSetups[7,2]
*-- System Type 'P' for Point of sale.
llPOSale   = (laSetups[8,2] = 'P')
*-- Use Detail Costing (PO)
llImpCost  = laSetups[9,2]
llImpCost  = IIF(lcInvType='0002' OR EMPTY(llImpCost),.F.,llImpCost)
*-- Multi currency
llUseMCurr = laSetups[10,2]
llMulCurr  = IIF(lcPType $ 'DAE',.F.,laSetups[10,2])
*-- Change exch. rates
llEditExRt = laSetups[11,2]
*-- Receive Style PO by PO/Shipment
lcCostImp  = laSetups[12,2]
*-- Cost element type 1
lcIType1   = IIF(lcInvType = '0002','P',laSetups[13,2])
*-- Cost element type 2
lcIType2   = IIF(lcInvType = '0002','D',laSetups[14,2])
*-- Cost element type 3
lcIType3   = IIF(lcInvType = '0002','D',laSetups[15,2])
*-- Cost element type 4
lcIType4   = IIF(lcInvType = '0002','D',laSetups[16,2])
*-- Cost element type 5
lcIType5   = laSetups[17,2]
*-- Cost element type 6
lcIType6   = laSetups[18,2]
*-- Cost element type 7
lcIType7   = laSetups[19,2]
*-- Style Configuration
llConfig   = (laSetups[21,2]='Y')
*-- Costing Method (MA)
lcCostMthM = laSetups[22,2]
*-- Keep track of rolls (MA)
llTrkRolls = ALLTRIM(laSetups[23,2]) = 'Y'
*-- User previlage
llCostPrv  = gfUserPriv('IC','ICSTYLE','COSTING')

*-- Ware house popup, Estimated Cost, Lot quantity in case of PO multi lot
DIMENSION laWare[1,1],laECost[7,1], laLotArry[8,1]

STORE '' TO lcGLFYear,lcGLPeriod,lcCur1,lcCur2,laWare,lcOldPoNo,cbrowsefields,lcLotNo,;
  lcClrLstOp
STORE 1 TO lnCurrUnt1,lnCurrUnt2
*-- Initialize the necessary variables
STORE 0 TO lnTotStk,lnTotDam,lnTotCan,lnRate1,lnRate2,lnWare,laECost,lnPolstln
*-- 'R' Return Style PO, 'N' Issue inter location PO, 'A' Issue adornment order
*-- 'H' Issue inter location PO batch, 'U' Issue Inter location PO Shipment
llIssue = (lcPType $ 'RNAHUG')

llDyelot = IIF(lcInvType = "0002",llFabDye,llDyelot)

STORE .F. TO llIgnorAll,llCMInstld,llByCarton,llFirstTmp,llSpecLot,llNewItem,llEditLCst
STORE .T. TO llFirst
*-- major segment width
lnMjrWid   = LEN(gfItemMask('PM',"",lcInvType))
*-- Temporary file to hold the warehouses
lcWareHFil = gfTempName()
*-- Temporary file to be used for the receiving program and saving
lcTmpLine = gfTempName()
*-- To indicate receiving PO lines manually
lcAuto = 'M'
*-- If the system keeps track of bins.
lcTemLoc = gfTempName()
*-- Temporary PosHdr
lcPosHdr = gfTempName()
*-- Temporary PosLn
lcPosLn  = gfTempName()
*-- Temporary cTktBom file
lcCTktBom = gfTempName()
*-- Temporary BomCost file
lcBomCost = gfTempName()
*-- Temporary BomHeadr file
lcBomHdr  = gfTempName()
*-- Temporary file to hold the receiving lines
lcTempFile= gfTempName()
*-- Temporary file to hold the style or material master file
lcTmpItem = gfTempName()
*-- Temporary file to hold the stydye or ItemLoc master file
lcItemLoc  = gfTempName()
*-- Temporary file to hold the shipment master file
lcMastShp  = gfTempName()
*-- Temporary cursor to be used when checking the existance of style/fabric
*-- in the stydye or itemloc
lcTmpCurs  = gfTempName()

*N000587,1 WAM 12/01/2007 Get equivalent cost from BOM table for each cost element based on saved currency code
lcMastBomLn = gfTempName()
*N000587,1 WAM 12/01/2007 (End)


*! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
*DO FORM (oAriaApplication.ScreenHome+'POSREC.SCX')
llSilentMod = !EMPTY(lcObjName)
IF !llSilentMod
  *! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[Start]
  *DO FORM (oAriaApplication.ScreenHome+'POSREC.SCX')
  =gfCallForm('POSREC')
  *! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[End]
ELSE
  *! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[Start]
  IF FILE(oAriaApplication.clientscreenhome+'POSREC.SCX')
    DO FORM (oAriaApplication.clientscreenhome+'POSREC.SCX') NAME lcObjName NOSHOW
  ELSE
    *! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[End]
    DO FORM (oAriaApplication.ScreenHome+'POSREC.SCX') NAME lcObjName NOSHOW
    *! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[Start]
  ENDIF
  *! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[End]
ENDIF
*! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

*!*************************************************************
*! Name      : lfInit
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 09/11/2004
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

*! E303006,1 SAB 12/03/2011 Enable Issue From BINs in Inter Location PO [Start]
IF loFormSet.llSilentMod
  gcBaseWind = 'AWRPOSTREC'
ENDIF
*! E303006,1 SAB 12/03/2011 Enable Issue From BINs in Inter Location PO [End]

*-- 'N' Issue Inter-Location P/o, 'A' Issue Adornment order,;
*-- 'H' Issue Inter Location P/O Batch, 'U' Issue Inter-Location PO Shipment
IF loFormSet.lcPType $ 'NAHU' AND !loFormSet.llWareHous
  *--The system has not been setup to use multiple locations. Cannot proceed.
  =gfModalGen('TRM42054B42001','DIALOG')
  RETURN .F.
ENDIF
*B126833,1 WAM 04/03/2005 Add new button to edit/view landed cost
loFormSet.ariaform1.cmdLandCost.VISIBLE = !loFormSet.llMfCall
*B126833,1 WAM 04/03/2005 (End)

PRIVATE lcSqlStatement
*-- Open the PosHdr file
lcSqlStatement  = "SELECT TOP 0 * FROM POSHDR [INDEX=POSHDR]"
=lfOpenSql(lcSqlStatement,'POSHDR',loFormSet.lcPosHdr, "","",.F.)
loFormSet.DATAENVIRONMENT.INITIALSELECTEDALIAS = loFormSet.lcPosHdr
loFormSet.cBrowseAliasName       = loFormSet.lcPosHdr

*-- Open the PosLn file
lcSqlStatement  = "SELECT TOP 0 *, SPACE(3) AS cPriceCur, SPACE(3) AS cDutyCur, "+;
  " SPACE(1) AS Status FROM POSLN [INDEX=POSLN]"
=lfOpenSql(lcSqlStatement,'POSLN',loFormSet.lcPosLn, "","",.F.)

*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
DIMENSION loFormSet.laStatusArr[4,2]
loFormSet.laStatusArr[1,1]  = LANG_POSTREC_STATUS_OPEN
loFormSet.laStatusArr[1,2]  = LANG_POSTREC_STATUS_O
loFormSet.laStatusArr[2,1]  = LANG_POSTREC_STATUS_APPROVED
loFormSet.laStatusArr[2,2]  = LANG_POSTREC_STATUS_A
loFormSet.laStatusArr[3,1]  = LANG_POSTREC_STATUS_CANCEL
loFormSet.laStatusArr[3,2]  = LANG_POSTREC_STATUS_X
loFormSet.laStatusArr[4,1]  = LANG_POSTREC_STATUS_POSTED
loFormSet.laStatusArr[4,2]  = LANG_POSTREC_STATUS_P
*! E303079,1 MMT 06/28/2012 Fixing Media issues[T20120304.0004][Start]
*! N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][start]
*IF !(loFormSet.lcPType $ 'PGF')
IF !(loFormSet.lcPType $ 'PGFW')
  *! N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][End]
  *! E303079,1 MMT 06/28/2012 Fixing Media issues[T20120304.0004][END]
  DIMENSION loFormSet.laPanelObj[1,6]
  loFormSet.laPanelObj[1,1]="cmdDelBatch"
  loFormSet.laPanelObj[1,2]=oAriaApplication.BitmapHome+"TRASH.BMP"
  loFormSet.laPanelObj[1,3]="mCanBatch"
  loFormSet.laPanelObj[1,4]=LANG_POSTREC_CANCBATCH
  loFormSet.laPanelObj[1,5]=LANG_POSTREC_CANCBATCH
  loFormSet.laPanelObj[1,6]="A"
  *! E303079,1 MMT 06/28/2012 Fixing Media issues[T20120304.0004][Start]
ENDIF
*! E303079,1 MMT 06/28/2012 Fixing Media issues[T20120304.0004][END]
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]

*-- If the system uses keep track of bins.
IF loFormSet.llWareLoc
  *T20071102.0018(C200876) TMI [Start] Enhance opening WHSLOC file when Bin Location is installed
  IF ASCAN(loFormSet.laEvntTrig,PADR('CRTBINLN',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0 ;
      .AND. gfDoTriger('POSTREC',PADR('ISUSEBIN',10))
    =gfOpenTable(oAriaApplication.DataDir+'WHSLOC','WHSLOCST','SH')
  ELSE
    *T20071102.0018(C200876) TMI [End  ]
    lcSqlStatement  = "SELECT * FROM WHSLOC"
    =lfOpenFox(lcSqlStatement,'WHSLOC','WHSLOC',"")
    SELECT WhsLoc
    DIMENSION laIndex[2,2]
    laIndex = ''
    laIndex[1,1] = 'cWareCode+cLocation+Style+Color'
    laIndex[1,2] = 'WHSLOC'
    laIndex[2,1] = 'Style+Color+cWareCode+cLocation'
    laIndex[2,2] = 'WHSLOCST'
    =lfSetIndex('WhsLoc',@laIndex)
    SET ORDER TO TAG WHSLOCST IN WhsLoc
    *T20071102.0018(C200876) TMI [Start] close the above IF condition
  ENDIF
  *T20071102.0018(C200876) TMI [End  ]
ENDIF
RETURN

*!*************************************************************
*! Name      : lfAfterInit
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 09/11/2004
*! Purpose   : To bound the objects
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
*! Called    : From Init in the formset
*!*************************************************************
FUNCTION lfAfterInit
LPARAMETERS loFormSet
PRIVATE lcSqlStatement, laIndex

*-- Open warehouse file.
lcSqlStatement  = "SELECT * FROM WAREHOUS"
=lfOpenFox(lcSqlStatement,'WAREHOUS',lcWareHFil,"")
SELECT (lcWareHFil)
DIMENSION laIndex[1,2]
laIndex = ''
laIndex[1,1] = 'cWareCode'
laIndex[1,2] = lcWareHFil
=lfSetIndex(lcWareHFil,@laIndex)

*-- Get all ware houses
loFormSet.llCMInstld = (OCCURS('NC',oAriaApplication.CompanyInstalledModules) <> 0)

*-- Fill the warehouse popup
DIMENSION loFormSet.laWare[1]
SELECT (lcWareHFil)
IF loFormSet.llWareHous
  IF loFormSet.llCMInstld AND loFormSet.llPOSale
    LOCATE FOR cSiteId = oAriaApplication.gcCurSite
    loFormSet.laWare[1] = EVALUATE(lcWareHFil+'.cWareCode')+'-'+EVALUATE(lcWareHFil+'.cDesc')
    loFormSet.lnWare = 1
  ELSE
    SELECT cWareCode+'-'+cDesc FROM (lcWareHFil) ;
      WHERE IIF(loFormSet.lcInvType="0001",lStyInv,lMatInv) INTO ARRAY loFormSet.laWare

    DECLARE loFormSet.laWare [ALEN(loFormSet.laWare,1)+1]
    =AINS(loFormSet.laWare,1)
    lcArray1 = ""
    loFormSet.laWare[1] = PADR(lcArray1,40)+"N/A"+PADR(lcArray1,40)
    loFormSet.lnWare = 0
    loFormSet.AriaForm1.cboLocations.REQUERY
  ENDIF
ELSE
  *GO TOP IN WAREHOUS
  GO TOP IN (lcWareHFil)
  loFormSet.laWare[1] = EVALUATE(lcWareHFil+'.cWareCode')+'-'+EVALUATE(lcWareHFil+'.cDesc')
ENDIF

*-- Open the style or material master file
IF loFormSet.lcInvType = "0001"
  *-- To get the structure of the style file
  lcSqlStatement  = "SELECT * FROM STYLE WHERE 1 = 2"
  =lfOpenFox(lcSqlStatement,'STYLE',loFormSet.lcTmpItem,"")
  DIMENSION laIndex[1,2]
  laIndex = ''
  laIndex[1,1] = 'Style'
  laIndex[1,2] = loFormSet.lcTmpItem
  =lfSetIndex(loFormSet.lcTmpItem,@laIndex)

  *-- To get the structure of the stydye file
  lcSqlStatement  = "SELECT * FROM STYDYE WHERE 1 = 2"
  =lfOpenFox(lcSqlStatement,'STYDYE',loFormSet.lcItemLoc,"")
  DIMENSION laIndex[1,2]
  laIndex = ''
  laIndex[1,1] = 'Style+cWareCode+Dyelot'
  laIndex[1,2] = loFormSet.lcItemLoc
  =lfSetIndex(loFormSet.lcItemLoc,@laIndex)
ELSE

  *-- To get the structure of the style file
  lcSqlStatement  = "SELECT TOP 0 * FROM ITEM [INDEX=STYLE]"
  =lfOpenSql(lcSqlStatement,'ITEM',loFormSet.lcTmpItem, "","",.F.)
  DIMENSION laIndex[1,2]
  laIndex = ''
  laIndex[1,1] = 'cInvType+Style'
  laIndex[1,2] = loFormSet.lcTmpItem
  =lfSetIndex(loFormSet.lcTmpItem,@laIndex)

  lcSqlStatement  = "SELECT TOP 0 * FROM ITEMLOC [INDEX=STYDYE]"
  =lfOpenSql(lcSqlStatement,'ITEMLOC',loFormSet.lcItemLoc, "","",.F.)
  DIMENSION laIndex[1,2]
  laIndex = ''
  laIndex[1,1] = 'cInvType+Style+cWareCode+Dyelot'
  laIndex[1,2] = loFormSet.lcItemLoc
  =lfSetIndex(loFormSet.lcItemLoc,@laIndex)
ENDIF
*-- Set the form Caption
DO CASE
  *-- 'N' Issue Inter-Location P/O, 'U' Issue Inter-Location PO Shipment
CASE loFormSet.lcPType $ 'NU'
  loFormSet.AriaForm1.CAPTION = "Issue Inter-Location PO"
  *-- 'H' Issue Inter Location P/O Batch
CASE loFormSet.lcPType = 'H'

  *-- 'P' Receive Material PO 'F' Receive Material PO Shipment
CASE loFormSet.lcPType $ 'PF'
  loFormSet.AriaForm1.CAPTION = "Receive Material PO"

  *!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [START]
  loFormSet.AriaForm1.cmdRolls.VISIBLE = (loFormSet.lcCostMthM = 'L' AND loFormSet.llTrkRolls)
  *!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]

  *N039416,1 KHM 06/21/2005 Changing the screen caption for return style PO [Begin]
  *-- 'R' Issue Return Style PO
CASE loFormSet.lcPType = 'R'
  loFormSet.AriaForm1.CAPTION = "Issue Return Style PO"
  *N039416,1 KHM 06/21/2005 [End]

  *N039417,1 KHM 06/21/2005 Changing the screen caption for return material PO [Begin]
  *-- 'R' Issue Return Style PO
CASE loFormSet.lcPType = 'G'
  loFormSet.AriaForm1.CAPTION = "Issue Return Material PO"
  *N039417,1 KHM 06/21/2005 [End]

  *!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [START]
  loFormSet.AriaForm1.cmdRolls.VISIBLE = (loFormSet.lcCostMthM = 'L')
  loFormSet.AriaForm1.cmdRolls.CAPTION = IIF((loFormSet.lcCostMthM = 'L' AND !loFormSet.llTrkRolls),'\<Lots','\<Rolls')
  *!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]

  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
CASE loFormSet.lcPType = 'W'
  loFormSet.AriaForm1.CAPTION = LANG_POSTREC_MMO_REC_MMO
  loFormSet.AriaForm1.cmdRolls.VISIBLE = (loFormSet.lcCostMthM = 'L')
  loFormSet.AriaForm1.cmdRolls.CAPTION = IIF((loFormSet.lcCostMthM = 'L' AND !loFormSet.llTrkRolls),'\<Lots','\<Rolls')
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][End]
ENDCASE

*-- Hide the currencies object in case of not multi currency
IF !loFormSet.llMulCurr
  WITH loFormSet.AriaForm1
    STORE .F. TO .lblCurrency.VISIBLE,  .lblPrice.VISIBLE, .txtPrice.VISIBLE,;
      .txtpriceRate.VISIBLE, .lblRate.VISIBLE, .txtDutyRate.VISIBLE,;
      .lblDuty.VISIBLE, .txtDuty.VISIBLE,;
      .shpRegion4.VISIBLE
    .shpRegion5.WIDTH = 661

  ENDWITH
ENDIF

*N000587,1 WAM 12/01/2007 Don't read duty currency code and exchange rate
WITH loFormSet.AriaForm1
  IF loFormSet.lcBusDoc+loFormSet.lcWorkOrd $ 'PP|PD|NN'
    STORE .F. TO  .txtDutyRate.VISIBLE,.lblDuty.VISIBLE, .txtDuty.VISIBLE
  ENDIF
ENDWITH
*N000587,1 WAM 12/01/2007 (End)

*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [START]
*N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
*IF loFormSet.lcCostMthM $ 'LIF' AND loFormSet.lcPType $ 'PFG'
IF loFormSet.lcCostMthM $ 'LIF' AND loFormSet.lcPType $ 'PFGW'
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
  loFormSet.ADDPROPERTY('lcTmpJour')
  loFormSet.ADDPROPERTY('lcTmpRoll')
  loFormSet.ADDPROPERTY('lcFullRoll')
  loFormSet.lcTmpJour = gfTempName()
  loFormSet.lcFullRoll = gfTempName()
  loFormSet.lcTmpRoll = gfTempName()
ENDIF
*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]

*!*************************************************************
*! Name      : lfShow
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 09/11/2004
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

  *!*	    *C200488,1 WAB (Start)
  *!*	    IF ASCAN(laEvntTrig,PADR("ZAPTEMP",10)) <> 0
  *!*	      =gfDoTriger("POSTREC",PADR("ZAPTEMP",10))
  *!*	    ENDIF
  *!*	    *C200488,1 WAB (END)
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  STORE '' TO loFormSet.laUsedBatchPO
  IF USED(loFormSet.lcbatchdet)
    USE IN (loFormSet.lcbatchdet)
  ENDIF
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]

  IF loFormSet.llEditLCst
    SET MARK OF BAR 1 OF _OPTIONPOP TO .F.
  ENDIF
  loFormSet.llEditLCst = .F.
  loFormSet.lnWare = 0

  *-- Create the temporary files
  =lfCreatTmp(loFormSet)
  *-- Assing the grid record source after create the temporary file.
  loFormSet.AriaForm1.grdReceivingLines.RECORDSOURCE = ""
  loFormSet.AriaForm1.grdReceivingLines.RECORDSOURCE = loFormSet.lcTmpLine
  *-- To disable the edit region
  =lfObjStatus(loFormSet,.F.)

  *-- If this is the first time running the screen
  IF loFormSet.llFirst
    WITH loFormSet.AriaForm1
      STORE .T. TO .dtPickerPostingDate.ENABLED, .dtpickerReceivingDate.ENABLED
      STORE oAriaApplication.SystemDate TO .dtPickerPostingDate.VALUE,;
        .dtpickerReceivingDate.VALUE
      IF loFormSet.llIssue AND loFormSet.lcPType $ 'NU'
        .lblPO.CAPTION = 'Inter-Loc. P/O#'
      ENDIF
      *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
      IF loFormSet.lcPType $ 'MT'
        .lblPO.CAPTION = 'C/T #'
      ENDIF
      *N038893,1 WAM 06/02/2005 (End)
      *-- IF not link to GL and Issue Inter-Location PO
      IF loFormSet.llLinkToGl
        *-- 'N' Issue Inter-Location PO, 'U' Issue Inter-location PO Shipment
        IF loFormSet.lcPType $ 'NU'
          .lblRecevingDate.CAPTION = "Issuing Date"
        ENDIF

      ELSE
        STORE .F. TO .lblRecevingDate.VISIBLE, .dtpickerReceivingDate.VISIBLE,;
          .lblSmiCol2.VISIBLE
        *-- 'N' Issue Inter-Location PO, 'U' Issue Inter-location PO Shipment
        IF loFormSet.lcPType $ 'NU'
          .lblPostingDate.CAPTION = "Issuing Date"
        ELSE
          .lblPostingDate.CAPTION = "Receiving Date"
        ENDIF
      ENDIF

      *-- Display the receiving types
      DO CASE
        *-- Issue Return Style PO
      CASE loFormSet.lcPType = 'R'
        .cboReceivingTypes.ROWSOURCE = LANG_POSTREC_IssueReturn+","+LANG_POSTREC_IssueR
        loFormSet.lcBusDoc   = 'R'
        loFormSet.lcWorkOrd  = 'P'

        *-- Issue Inter Location PO, Issue Inter Location PO Shipment
      CASE loFormSet.lcPType = 'N'
        .cboReceivingTypes.ROWSOURCE = LANG_POSTREC_IssueIntrPO+","+LANG_POSTREC_IssueN+;
          ","+LANG_POSTREC_IssueIntrPOS+","+;
          LANG_POSTREC_IssueIntrPOSH

        loFormSet.lcBusDoc   = 'N'
        loFormSet.lcWorkOrd  = 'N'

        *-- Issue Inter Location Batch
      CASE loFormSet.lcPType = 'H'
        .cboReceivingTypes.ROWSOURCE = LANG_POSTREC_IssueIntrPOB+","+LANG_POSTREC_IssueH
        loFormSet.lcBusDoc   = 'N'
        loFormSet.lcWorkOrd  = 'N'

        *-- Issue Adornment PO
      CASE loFormSet.lcPType = 'A'
        .cboReceivingTypes.ROWSOURCE = LANG_POSTREC_IssueAdronPO+","+LANG_POSTREC_IssueA
        loFormSet.lcBusDoc   = 'P'
        loFormSet.lcWorkOrd  = 'A'

        *-- Receive Style PO and Style Shipment PO
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
        *CASE loFormSet.lcPType $ 'IS'
      CASE loFormSet.lcPType = 'S'
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
        loFormSet.lcBusDoc   = 'P'
        loFormSet.lcWorkOrd  = 'P'

        *-- Receive Material PO and Material Shipment PO
      CASE loFormSet.lcPType $ 'PF'
        loFormSet.lcBusDoc   = 'P'
        loFormSet.lcWorkOrd  = 'M'
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
        .cboReceivingTypes.ROWSOURCE =LANG_POSTREC_ReceiveMatPO+","+LANG_POSTREC_ReceiveMatPOP+","+;
          LANG_POSTREC_ReceiveMatPOSHP+","+LANG_POSTREC_ReceiveMatPOF
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
        *-- Issue Return Material PO
      CASE loFormSet.lcPType = 'G'
        .cboReceivingTypes.ROWSOURCE = LANG_POSTREC_IssueReturn+","+LANG_POSTREC_IssueRMat
        loFormSet.lcBusDoc   = 'R'
        loFormSet.lcWorkOrd  = 'M'

        *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
        *-- 'M' Receive C/T, 'T' Receive C/T Batch
      CASE loFormSet.lcPType $ 'MT'
        loFormSet.lcBusDoc   = 'P'
        loFormSet.lcWorkOrd  = 'U'
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
        .cboReceivingTypes.ROWSOURCE = LANG_POSTREC_ReceiveCT +","+LANG_POSTREC_ReceiveM+","+;
          LANG_POSTREC_ReceiveDyeOrd+","+LANG_POSTREC_ReceiveD
      CASE loFormSet.lcPType $ 'OI'
        .cboReceivingTypes.ROWSOURCE =  LANG_POSTREC_ReceivePO+","+LANG_POSTREC_ReceiveSPOI+","+;
          LANG_POSTREC_ReceiveSPOShip+","+LANG_POSTREC_ReceiveSS
        loFormSet.lcBusDoc   = 'P'
        loFormSet.lcWorkOrd  = 'P'
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
        *N038893,1 WAM 06/02/2005 (End)
        *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
      CASE loFormSet.lcPType = 'W'
        .cboReceivingTypes.ROWSOURCE = LANG_POSTREC_REC_MMO+","+LANG_POSTREC_MMOW
        loFormSet.lcBusDoc   = 'P'
        loFormSet.lcWorkOrd  = 'F'
        *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][End]


      ENDCASE

      *-- To assign the aria work order key properties
      =lfSetKeyPro(loFormSet,loFormSet.lcPType)

      =lfActBrow(loFormSet)
      .cboReceivingTypes.VALUE = loFormSet.lcPType
      .cboReceivingTypes.OldValue = "*"
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
      =lfvRecvTyp(loFormSet)
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
      .cboReceivingTypes.ENABLED = .F.
      *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
      *.dtPickerPostingDate.SetFocus
      IF !loFormSet.llSilentMod
        .dtPickerPostingDate.SETFOCUS
      ENDIF
      *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

    ENDWITH
  ELSE
    WITH loFormSet.AriaForm1
      .dtPickerPostingDate.ENABLED    = .T.
      .dtPickerPostingDate.VALUE = oAriaApplication.SystemDate
      .dtpickerReceivingDate.ENABLED  = .T.
      .dtpickerReceivingDate.VALUE = oAriaApplication.SystemDate
      STORE loFormSet.lcPType TO .cboReceivingTypes.VALUE, .cboReceivingTypes.OldValue
      .cboReceivingTypes.ENABLED = .T.
      =lfActBrow(loFormSet)
      =lfvRecvTyp(loFormSet)
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
      *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
      *.dtPickerPostingDate.SetFocus
      IF !loFormSet.llSilentMod
        .dtPickerPostingDate.SETFOCUS
      ENDIF
      *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
    ENDWITH
  ENDIF

  *-- 'H' Issue Inter Location P/O Batch, 'U' Issue Inter Location P/O Shipment
  IF loFormSet.lcPType = 'H'
    =lfvRecvTyp(loFormSet)
  ENDIF
  STORE 0 TO loFormSet.lnTotStk,loFormSet.lnTotDam,loFormSet.lnTotCan
  loFormSet.llIgnorAll = .F.

  *-- 'R' Issue Return Style PO , 'A' Issue Adornment PO
  *-- 'H' Issue Inter Location Batch', 'G' Issue Return Material PO
  IF loFormSet.lcPType $ 'RAHG'
    loFormSet.AriaForm1.cboReceivingTypes.ENABLED = .F.
  ELSE
    *loFormSet.AriaForm1.cboReceivingTypes.Enabled = .T.
  ENDIF
  *-- If not use multi-currency
  IF !loFormSet.llMulCurr
    STORE oAriaApplication.BaseCurrency TO loFormSet.lcCur1, loFormSet.lcCur2
  ENDIF
  STORE 1 TO loFormSet.lnRate1,loFormSet.lnRate2,loFormSet.lnCurrUnt1,loFormSet.lnCurrUnt2
  *-- To assign object's control source.
  =lfCntrSour(loFormSet,.F.)
  *-- Define array laECost to hold the equivelant costs
  DIMENSION loFormSet.laECost[7]
  loFormSet.laECost = 0

  *-- A D D  M O D E.
CASE lcScrMode = "A"

  llCUpDate  = .T.
  llGoAndChk = .T.
  WITH loFormSet.AriaForm1
    STORE .F. TO .dtPickerPostingDate.ENABLED,;
      .dtpickerReceivingDate.ENABLED,;
      .txtstock.ENABLED, .txtothers.ENABLED,;
      .txtcancel.ENABLED
  ENDWITH
  =lfShowScr(loFormSet)
  =lfActBrow(loFormSet)
  =lfwBrow(loFormSet)

ENDCASE

RETURN

*:*************************************************************
*! Name     : lfCntrSour
*! Developer: Khalid Mohi El-Din Mohamed
*! Date     : 09/11/2004
*! Purpose  : To assign object's control source.
*:*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!             llAssign : .T. To assign control source
*!*************************************************************
FUNCTION lfCntrSour
LPARAMETERS loFormSet, llAssign
*-- Asign the control source
WITH loFormSet.AriaForm1
  .txtStock.CONTROLSOURCE					= IIF(llAssign,'ThisFormSet.lnTotStk',"")
  .txtothers.CONTROLSOURCE					= IIF(llAssign,'ThisFormSet.lnTotDam',"")
  .txtcancel.CONTROLSOURCE					= IIF(llAssign,'ThisFormSet.lnTotCan',"")
  .kbPoNo.keytextbox.CONTROLSOURCE          = IIF(llAssign,loFormSet.lcTmpLine + '.PO',"")
  .kbItem.CONTROLSOURCE                     = IIF(llAssign,loFormSet.lcTmpLine + '.Style',"")
  .kbconfiguration.keytextbox.CONTROLSOURCE = IIF(llAssign,loFormSet.lcTmpLine + '.Dyelot',"")
  .txtitemDesc.CONTROLSOURCE                = IIF(llAssign,loFormSet.lcTmpLine + '.cStyDesc',"")
  .cboLocations.CONTROLSOURCE               = IIF(llAssign,'ThisFormSet.lnWare',"")
  .txtreference.CONTROLSOURCE               = IIF(llAssign,loFormSet.lcTmpLine + '.Reference',"")
  .txtprice.CONTROLSOURCE                   = IIF(llAssign,loFormSet.lcTmpLine + '.cPriceCur',"")
  .txtduty.CONTROLSOURCE                    = IIF(llAssign,loFormSet.lcTmpLine + '.cDutyCur',"")
  .txtpriceRate.CONTROLSOURCE               = IIF(llAssign,loFormSet.lcTmpLine + '.nLanPrRat',"")
  .txtDutyRate.CONTROLSOURCE                = IIF(llAssign,loFormSet.lcTmpLine + '.nLanDuRat',"")
  .cboLocations.REQUERY

  *-- 'P' Material PO, 'F' Material PO Shipment
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
  *IF loFormSet.lcPType $ 'PF'
  IF loFormSet.lcPType $ 'PFW'
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][End]
    .txtPattern.CONTROLSOURCE                = IIF(llAssign,loFormSet.lcTmpLine + '.Pattern',"")
  ENDIF

  IF !llAssign
    STORE ""  TO .kbItem.VALUE       , .kbconfiguration.keytextbox.VALUE,;
      .txtreference.VALUE , .txtpattern.VALUE,;
      .txtPrice.VALUE     , .txtDuty.VALUE   ,;
      .txtpriceRate.VALUE , .txtDutyRate.VALUE
    STORE 0   TO .cboLocations.VALUE
  ENDIF

ENDWITH


*:*************************************************************
*! Name     : lfActBrow
*! Developer: Khalid Mohi El-Din Mohamed
*! Date     : 09/11/2004
*! Purpose  : Valid the Posting date.
*:*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
*! Called    : To set the titles and fields in the grid
*!*************************************************************
FUNCTION lfActBrow
LPARAMETERS loFormSet
LOCAL lnColumnNo, lcWorkOrdTit, lcTmpFile, lcFieldMask
lcTmpFile   = loFormSet.lcTmpLine
lcFieldMask = IIF(loFormSet.lcInvType="0001","9999999","9999999.999")
SELECT(lcTmpFile)


WITH loFormSet.AriaForm1.grdReceivingLines
  .COLUMNCOUNT = 1
  lnColumnNo   = 1

  *-- Cartons Column
  *-- 'B' Receive P/O Batch, 'T' Receive C/T Batch,
  *-- 'L' Receive Inter Location P/O Batch', 'H' Issue Inter Location P/O Batch'
  IF loFormSet.lcPType $ 'BTLH'
    .COLUMNS(lnColumnNo).Header1.CAPTION = "Cartons"
    .COLUMNS(lnColumnNo).Header1.ALIGNMENT = 0     && Left
    .COLUMNS(lnColumnNo).WIDTH = 50
    .COLUMNS(lnColumnNo).CONTROLSOURCE = lcTmpFile +'.Style'
    .COLUMNS(lnColumnNo).ALIGNMENT = 0     && Left
    .COLUMNCOUNT = .COLUMNCOUNT + 1
    lnColumnNo   = lnColumnNo + 1
  ENDIF

  *-- Work Order Column
  lcWorkOrdTit = IIF(loFormSet.lcPtype $ 'MT','C/T #',;
    IIF(loFormSet.lcPtype='R','Ret #','P/O #'))
  .COLUMNS(lnColumnNo).Header1.CAPTION = lcWorkOrdTit
  .COLUMNS(lnColumnNo).Header1.ALIGNMENT = 0     && Left
  .COLUMNS(lnColumnNo).WIDTH = 75
  .COLUMNS(lnColumnNo).CONTROLSOURCE = lcTmpFile +'.PO'
  .COLUMNS(lnColumnNo).ALIGNMENT = 0     && Left

  *-- Style Column
  .COLUMNCOUNT = .COLUMNCOUNT + 1
  lnColumnNo   = lnColumnNo + 1
  .COLUMNS(lnColumnNo).Header1.CAPTION = loFormSet.AriaForm1.kbItem.lcItemHeader
  .COLUMNS(lnColumnNo).Header1.ALIGNMENT = 0     && Left
  .COLUMNS(lnColumnNo).WIDTH = 125
  .COLUMNS(lnColumnNo).CONTROLSOURCE = lcTmpFile +'.Style'
  .COLUMNS(lnColumnNo).ALIGNMENT = 0     && Left

  *-- IF system uses configuration and receiving type is not material
  IF loFormSet.llConfig
    .COLUMNCOUNT = .COLUMNCOUNT + 1
    lnColumnNo   = lnColumnNo + 1
    .COLUMNS(lnColumnNo).Header1.CAPTION = 'Configuration'
    .COLUMNS(lnColumnNo).Header1.ALIGNMENT = 0     && Left
    .COLUMNS(lnColumnNo).WIDTH = 75
    .COLUMNS(lnColumnNo).CONTROLSOURCE = lcTmpFile +'.Dyelot'
    .COLUMNS(lnColumnNo).ALIGNMENT = 0     && Left
  ELSE
    *-- If style or material uses dyelot
    IF loFormSet.llDyelot OR loFormSet.llFabDye
      .COLUMNCOUNT = .COLUMNCOUNT + 1
      lnColumnNo   = lnColumnNo + 1
      .COLUMNS(lnColumnNo).Header1.CAPTION = 'Dyelot'
      .COLUMNS(lnColumnNo).Header1.ALIGNMENT = 0     && Left
      .COLUMNS(lnColumnNo).WIDTH = 75
      .COLUMNS(lnColumnNo).CONTROLSOURCE = lcTmpFile +'.Dyelot'
      .COLUMNS(lnColumnNo).ALIGNMENT = 0     && Left
    ENDIF
  ENDIF

  *-- IF system uses multi-location
  IF loFormSet.llWareHous
    *-- IF 'N' Issue Inter-Location P/o, 'H' Issue Inter Location P/O Batch'
    *-- 'L' Receive Inter Location P/O Batch,  'A' Issue Adornment order,
    *-- 'U' Issue Inter Location PO Shipment, 'C' Receive Inter-Location P/O Shipment
    IF loFormSet.lcPtype $ 'NHLAUC'
      .COLUMNCOUNT = .COLUMNCOUNT + 1
      lnColumnNo   = lnColumnNo + 1
      .COLUMNS(lnColumnNo).Header1.CAPTION = 'Source'
      .COLUMNS(lnColumnNo).Header1.ALIGNMENT = 0     && Left
      .COLUMNS(lnColumnNo).WIDTH = 75
      .COLUMNS(lnColumnNo).CONTROLSOURCE = lcTmpFile +'.Vendor'
      .COLUMNS(lnColumnNo).ALIGNMENT = 0     && Left

      .COLUMNCOUNT = .COLUMNCOUNT + 1
      lnColumnNo   = lnColumnNo + 1
      .COLUMNS(lnColumnNo).Header1.CAPTION = 'Target'
      .COLUMNS(lnColumnNo).Header1.ALIGNMENT = 0     && Left
      .COLUMNS(lnColumnNo).WIDTH = 75
      .COLUMNS(lnColumnNo).CONTROLSOURCE = lcTmpFile +'.cWareCode'
      .COLUMNS(lnColumnNo).ALIGNMENT = 0     && Left
    ELSE
      .COLUMNCOUNT = .COLUMNCOUNT + 1
      lnColumnNo   = lnColumnNo + 1
      .COLUMNS(lnColumnNo).Header1.CAPTION = 'Location'
      .COLUMNS(lnColumnNo).Header1.ALIGNMENT = 0     && Left
      .COLUMNS(lnColumnNo).WIDTH = 75
      .COLUMNS(lnColumnNo).CONTROLSOURCE = lcTmpFile +'.cWareCode'
      .COLUMNS(lnColumnNo).ALIGNMENT = 0     && Left
    ENDIF
  ENDIF

  *-- 'P' Material PO, 'F' Material PO Shipment
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
  *IF loFormSet.lcPtype $ 'PF'
  IF loFormSet.lcPtype $ 'PFW'
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
    .COLUMNCOUNT = .COLUMNCOUNT + 1
    lnColumnNo   = lnColumnNo + 1
    .COLUMNS(lnColumnNo).Header1.CAPTION = 'Pattern'
    .COLUMNS(lnColumnNo).Header1.ALIGNMENT = 0     && Left
    .COLUMNS(lnColumnNo).WIDTH = 75
    .COLUMNS(lnColumnNo).CONTROLSOURCE = lcTmpFile +'.Pattern'
    .COLUMNS(lnColumnNo).ALIGNMENT = 0     && Left
  ENDIF

  *-- Original Qty
  .COLUMNCOUNT = .COLUMNCOUNT + 1
  lnColumnNo   = lnColumnNo + 1
  .COLUMNS(lnColumnNo).Header1.CAPTION = 'Original'
  .COLUMNS(lnColumnNo).Header1.ALIGNMENT = 1     && Right
  .COLUMNS(lnColumnNo).WIDTH = 60
  .COLUMNS(lnColumnNo).CONTROLSOURCE = lcTmpFile +'.TotQty'
  .COLUMNS(lnColumnNo).ALIGNMENT = 1     && Right
  .COLUMNS(lnColumnNo).INPUTMASK = lcFieldMask
  *-- Stock Qty
  .COLUMNCOUNT = .COLUMNCOUNT + 1
  lnColumnNo   = lnColumnNo + 1
  .COLUMNS(lnColumnNo).Header1.CAPTION = 'Stock'
  .COLUMNS(lnColumnNo).Header1.ALIGNMENT = 1     && Right
  .COLUMNS(lnColumnNo).WIDTH = 60
  .COLUMNS(lnColumnNo).CONTROLSOURCE = lcTmpFile +'.TotStk'
  .COLUMNS(lnColumnNo).ALIGNMENT = 1     && Right
  .COLUMNS(lnColumnNo).INPUTMASK = lcFieldMask

  *-- Other Qty
  .COLUMNCOUNT = .COLUMNCOUNT + 1
  lnColumnNo   = lnColumnNo + 1
  .COLUMNS(lnColumnNo).Header1.CAPTION = 'Other'
  .COLUMNS(lnColumnNo).Header1.ALIGNMENT = 1     && Right
  .COLUMNS(lnColumnNo).WIDTH = 60
  .COLUMNS(lnColumnNo).CONTROLSOURCE = lcTmpFile +'.TotDam'
  .COLUMNS(lnColumnNo).ALIGNMENT = 1     && Right
  .COLUMNS(lnColumnNo).INPUTMASK = lcFieldMask

  *-- Cancel Qty
  .COLUMNCOUNT = .COLUMNCOUNT + 1
  lnColumnNo   = lnColumnNo + 1
  .COLUMNS(lnColumnNo).Header1.CAPTION = 'Cancel'
  .COLUMNS(lnColumnNo).Header1.ALIGNMENT = 1     && Right
  .COLUMNS(lnColumnNo).WIDTH = 60
  .COLUMNS(lnColumnNo).CONTROLSOURCE = lcTmpFile +'.TotCan'
  .COLUMNS(lnColumnNo).ALIGNMENT = 1     && Right
  .COLUMNS(lnColumnNo).INPUTMASK = lcFieldMask

  *-- Balance Qty
  .COLUMNCOUNT = .COLUMNCOUNT + 1
  lnColumnNo   = lnColumnNo + 1
  .COLUMNS(lnColumnNo).Header1.CAPTION = 'Balance'
  .COLUMNS(lnColumnNo).Header1.ALIGNMENT = 1     && Right
  .COLUMNS(lnColumnNo).WIDTH = 60
  .COLUMNS(lnColumnNo).CONTROLSOURCE = lcTmpFile +'.TotBal'
  .COLUMNS(lnColumnNo).ALIGNMENT = 1     && Right
  .COLUMNS(lnColumnNo).INPUTMASK = lcFieldMask

  *-- Reference
  .COLUMNCOUNT = .COLUMNCOUNT + 1
  lnColumnNo   = lnColumnNo + 1
  .COLUMNS(lnColumnNo).Header1.CAPTION = 'Reference'
  .COLUMNS(lnColumnNo).Header1.ALIGNMENT = 1     && Right
  .COLUMNS(lnColumnNo).WIDTH = 160
  .COLUMNS(lnColumnNo).CONTROLSOURCE = lcTmpFile +'.Reference'
  .COLUMNS(lnColumnNo).ALIGNMENT = 0     && Left

  .SETALL('ReadOnly',.T.,'COLUMN')
ENDWITH

*:*************************************************************
*! Name     : lfvPostDat
*! Developer: Khalid Mohi El-Din Mohamed
*! Date     : 09/11/2004
*! Purpose  : Valid the Posting date.
*:*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
*! Called    : From valid of dtPickerPostingDate in the formset
*!*************************************************************
FUNCTION lfvPostDat
LPARAMETERS loFormSet
PRIVATE lcGLFYear,lcGLPeriod
*-- Fiscal yeare and period
STORE '' TO lcGLFYear,lcGLPeriod

WITH loFormSet.AriaForm1
  IF loFormSet.llLinkToGl
    *B126833,1 WAM 04/03/2005 Do not hide the Options pad
    *loFormSet.Deactivate()
    *B126833,1 WAM 04/03/2005 (End)

    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
    *IF !CHECKPRD(.dtPickerPostingDate.Value,'lcGLFYear','lcGLPeriod','PO')
    *  .dtPickerPostingDate.Value = .dtPickerPostingDate.OldValue
    *  RETURN .F.
    *ENDIF
    IF !loFormSet.llSilentMod
      IF !CHECKPRD(.dtPickerPostingDate.VALUE,'lcGLFYear','lcGLPeriod','PO')
        .dtPickerPostingDate.VALUE = .dtPickerPostingDate.OldValue
        RETURN .F.
      ENDIF
    ENDIF
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

    loFormSet.lcGLFYear = lcGLFYear
    loFormSet.lcGLPeriod = lcGLPeriod
  ELSE
    .dtpickerReceivingDate.VALUE = .dtPickerPostingDate.VALUE
    =lfvRecvDat(loFormSet)
  ENDIF
ENDWITH
RETURN

*:*************************************************************
*! Name     : lfvRecvDat
*! Developer: Khalid Mohi El-Din Mohamed
*! Date     : 09/11/2004
*! Purpose  : Valid the Receiving date.
*:*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
*! Called    : From valid of dtpickerReceivingDate in the formset
*!*************************************************************
FUNCTION lfvRecvDat
LPARAMETERS loFormSet
PRIVATE lcRowSource

*-- If the system is linked to GL and the receiving date is > than the posting date
WITH loFormSet.AriaForm1
  IF loFormSet.llLinkToGl AND ;
      .dtpickerReceivingDate.VALUE > .dtPickerPostingDate.VALUE

    *B126833,1 WAM 04/03/2005 Do not hide the option pad
    *loFormSet.Deactivate()
    *B126833,1 WAM 04/03/2005 (End)
    *--The xxxx date cannot date cannot follow the posting date.
    = gfModalGen('TRM42106B42000','DIALOG',IIF(loFormSet.lcPType $ 'RNAH','issuing','receiving'))
    RETURN .F.
  ELSE
    PRIVATE lcGLFYear,lcGLPeriod
    *-- Fiscal yeare and period
    STORE '' TO lcGLFYear,lcGLPeriod
    *B126833,1 WAM 04/03/2005 Do not hide the option pad
    *loFormSet.Deactivate()
    *B126833,1 WAM 04/03/2005 (End)
    IF !lfCHECKPRD(.dtpickerReceivingDate.VALUE,'lcGLFYear','lcGLPeriod',loFormSet)
      RETURN .F.
    ENDIF
    loFormSet.lcGLFYear = lcGLFYear
    loFormSet.lcGLPeriod = lcGLPeriod

  ENDIF

  IF !loFormSet.llFirst
    RETURN
  ENDIF
  loFormSet.llFirst = .F.

  *-- If receiving styles
  IF loFormSet.lcInvType = "0001"
    *-- 'R' Return Style PO, 'N' Issue Inter Location PO, 'A' Issue Adornment PO
    *-- 'H' Issue Inter Location PO Batch, 'U' Issue Inter Location PO Shipment
    IF !(loFormSet.lcPType $ 'RNAHU')
      IF loFormSet.llMfCall .OR. loFormSet.lcPType = 'E'
        *-- 'M' Receive Cutting Ticket, 'T' Receive Batch, 'D' Receive Dye Order
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
        *!*          lcRowSource = LANG_POSTREC_ReceiveCT +","+LANG_POSTREC_ReceiveM+","+;
        *!*                        LANG_POSTREC_ReceiveCTB+","+LANG_POSTREC_ReceiveT+","+;
        *!*                        LANG_POSTREC_ReceiveDyeOrd+","+LANG_POSTREC_ReceiveD
        lcRowSource = LANG_POSTREC_ReceiveCT +","+LANG_POSTREC_ReceiveM+","+;
          LANG_POSTREC_ReceiveDyeOrd+","+LANG_POSTREC_ReceiveD
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
        IF loFormSet.llWareHous .AND. gfGetMemvar('M_BOMVAR')
          *-- 'E' Receive Adornment P/O
          lcRowSource = lcRowSource + ","+LANG_POSTREC_ReceiveAdronPO+","+LANG_POSTREC_ReceiveAdronPOE
        ENDIF
      ELSE
        *-- If use detail costing and receive by shipment
        IF loFormSet.llImpCost AND loFormSet.lcCostImp='S'
          *-- 'S' Receive by Shipment, 'B' Receive P/O Batch
          *-- 'L' Receive Inter Location P/O Batch, 'C' Receive Inter Location P/O Shipment
          *! B609634,1 MMT 06/28/2011 Fix bug of Zero Cost in Styinvjl in Case of receiving Interlocation PO[Start]
          *!*	          lcRowSource = LANG_POSTREC_ReceiveSPOShip+","+LANG_POSTREC_ReceiveSS+","+;
          *!*		      	            LANG_POSTREC_ReceivePOB+","+LANG_POSTREC_ReceiveSPOB+","+;
          *!*						    LANG_POSTREC_ReceiveIntrPOIS+","+LANG_POSTREC_ReceiveIntrPOSH+","+;
          *!*						  LANG_POSTREC_ReceiveIntrPOBT+","+LANG_POSTREC_ReceiveIntrPOB
          *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
          *!*            lcRowSource = LANG_POSTREC_ReceiveSPOShip+","+LANG_POSTREC_ReceiveSS+","+;
          *!*  	      	            LANG_POSTREC_ReceivePOB+","+LANG_POSTREC_ReceiveSPOB+","+;
          *!*  	     	            LANG_POSTREC_ReceiveIntrPO+","+LANG_POSTREC_ReceiveIntrPOO+","+;
          *!*  					    LANG_POSTREC_ReceiveIntrPOIS+","+LANG_POSTREC_ReceiveIntrPOSH+","+;
          *!*  					  LANG_POSTREC_ReceiveIntrPOBT+","+LANG_POSTREC_ReceiveIntrPOB
          lcRowSource = LANG_POSTREC_ReceiveSPOShip+","+LANG_POSTREC_ReceiveSS+","+;
            LANG_POSTREC_ReceiveIntrPO+","+LANG_POSTREC_ReceiveIntrPOO+","+;
            LANG_POSTREC_ReceiveIntrPOIS+","+LANG_POSTREC_ReceiveIntrPOSH
          *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
          *! B609634,1 MMT 06/28/2011 Fix bug of Zero Cost in Styinvjl in Case of receiving Interlocation PO[END]
          loFormSet.lcPType = IIF(loFormSet.lcPType='I','S',loFormSet.lcPType)
        ELSE
          *-- 'I' Receive by P/O, 'S' Receive by Shipment, 'B' Receive P/O Batch
          *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
          *!*            lcRowSource = LANG_POSTREC_ReceivePO+","+LANG_POSTREC_ReceiveSPOI+","+;
          *!*  		   			    LANG_POSTREC_ReceiveSPOShip+","+LANG_POSTREC_ReceiveSS+","+;
          *!*  					    LANG_POSTREC_ReceivePOB+","+LANG_POSTREC_ReceiveSPOB
          lcRowSource = LANG_POSTREC_ReceivePO+","+LANG_POSTREC_ReceiveSPOI+","+;
            LANG_POSTREC_ReceiveSPOShip+","+LANG_POSTREC_ReceiveSS
          *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
          *-- If multi-location and not from MF
          IF loFormSet.llWareHous AND !loFormSet.llMfCall
            *-- 'O' Receive Inter Location P/O, 'C' Receive Inter Location P/O Shipment
            *-- 'L' Receive Inter Location P/O Batch
            *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
            *!*              lcRowSource = lcRowSource + ","+LANG_POSTREC_ReceiveIntrPO+","+;
            *!*          						            LANG_POSTREC_ReceiveIntrPOO+","+;
            *!*  										    LANG_POSTREC_ReceiveIntrPOIS+","+;
            *!*  										    LANG_POSTREC_ReceiveIntrPOSH+","+;
            *!*  							                LANG_POSTREC_ReceiveIntrPOBT+","+;
            *!*  							                LANG_POSTREC_ReceiveIntrPOB
            lcRowSource = lcRowSource + ","+LANG_POSTREC_ReceiveIntrPO+","+;
              LANG_POSTREC_ReceiveIntrPOO+","+;
              LANG_POSTREC_ReceiveIntrPOIS+","+;
              LANG_POSTREC_ReceiveIntrPOSH
            *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
          ENDIF
        ENDIF
      ENDIF
      .cboReceivingTypes.ROWSOURCE = lcRowSource
      .cboReceivingTypes.ENABLED = .T.
      .cntShipment.ENABLED = .F.
    ELSE
      *-- 'R' Return Style PO, 'A' Issue Adornment PO, 'H' Issue Inter Location PO Batch
      IF loFormSet.lcPType $ 'RAH'
        .cboReceivingTypes.ENABLED = .F.
      ELSE
        .cboReceivingTypes.ENABLED = .T.
      ENDIF
    ENDIF

    *-- Case material PO, Shipment, issue return PO
  ELSE
    *-- 'G' Return Material PO
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
    *!*	    IF loFormSet.lcPType = 'G'
    *!*	      lcRowSource = LANG_POSTREC_IssueReturn+","+LANG_POSTREC_IssueRMat
    IF loFormSet.lcPType $ 'WG'
      lcRowSource = IIF(loFormSet.lcPType = 'G',LANG_POSTREC_IssueReturn+","+LANG_POSTREC_IssueRMat,LANG_POSTREC_REC_MMO+","+LANG_POSTREC_MMOW)
      *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][End]
      .cboReceivingTypes.ENABLED = .F.
    ELSE
      lcRowSource = LANG_POSTREC_ReceiveMatPO+","+LANG_POSTREC_ReceiveMatPOP+","+;
        LANG_POSTREC_ReceiveMatPOSHP+","+LANG_POSTREC_ReceiveMatPOF
      .cboReceivingTypes.ENABLED = .T.
    ENDIF
    .cboReceivingTypes.ROWSOURCE = lcRowSource

  ENDIF

  .cboReceivingTypes.VALUE = loFormSet.lcPType
  =lfObjStatus(loFormSet,.F.)

  *-- Case styles
  IF loFormSet.lcInvType = "0001"
    *-- If not Issue Inter location PO Batch
    IF !(loFormSet.lcPType = 'H')
      IF loFormSet.llImpCost AND loFormSet.lcCostImp='S' AND loFormSet.lcPType ='S'
        .grdReceivingLines.HEIGHT = .grdReceivingLines.HEIGHT - .cntShipment.HEIGHT
        .grdReceivingLines.TOP = .grdReceivingLines.TOP + .cntShipment.HEIGHT + 2
        .cntBatch.VISIBLE = .F.
        .cntBatch.ENABLED = .F.
        .cntShipment.VISIBLE = .T.
        .cntShipment.ENABLED = .T.
        .cntShipment.kbShipNo.ENABLED = .T.
        =lfActBrow(loFormSet)
      ELSE
        IF loFormSet.lcPType = 'U'
          *.cntShipment.Enabled = .T.
        ELSE
          .cntShipment.VISIBLE = .F.
          .cntShipment.ENABLED = .F.
          *-- Enable the work order key
          .kbPONo.ENABLED = .T.
        ENDIF

        *-- If receiving cutting ticket enable the style field
        IF loFormSet.llMFCall AND loFormSet.lcPType='M'
          .kbItem.ENABLED = .T.
        ENDIF
      ENDIF
    ENDIF
    *-- Case Material
  ELSE
    .cntShipment.VISIBLE = .F.
    .cntShipment.ENABLED = .F.
    *-- Enable the work order key
    .kbPONo.ENABLED = .T.

  ENDIF

  *-- 'R' Issue retrun PO, 'A' Issue Adornment order
  *-- 'H' Issue inter location batch
  IF !(loFormSet.lcPType $'RAHG')
    KEYBOARD '{SPACEBAR}'
  ENDIF
ENDWITH
=lfSetKeyPro(loFormSet,loFormSet.lcPType)
RETURN

*!**************************************************************************
*! Name      : lfCheckprd
*! Developer : Khalid Mohi El-Din
*! Date      : 09/11/2004
*! Purpose   : Function To validate the receiving date in case of GL not installed
*!**************************************************************************
*! Calls     : gfModalGen,gfOpenFile
*!**************************************************************************
*! Parameters: ldDate   : Transaction date to be check
*!			 : lcPeriod : Transaction Period
*!           : lcFYear  : Transaction Fiscal Year
*!**************************************************************************
*! Example   :  =lfCheckprd()
*!**************************************************************************
FUNCTION lfCheckprd
PARAMETERS ldDate,lcFYear,lcPeriod, loFormSet

PRIVATE llContinue,lcErrorM1,lnAlias, lcSqlStatement
lnAlias = SELECT()
llContinue = .T.

lcSqlStatement = "SELECT Cfisfyear, Cfspprdid, Dfsppbgdt, Dfsppendt "+;
  " FROM FSPRD "+;
  " WHERE BETWEEN ('"+DTOS(ldDate)+"' ,DTOS(Dfsppbgdt),DTOS(Dfsppendt))"

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatement,"FSPRD","FSPRD",;
  oAriaApplication.cAriaNativeDataFilesConStr,3,'SAVE',loFormSet.DATASESSIONID)

IF lnConnectionHandlar <> 1
  llContinue = .F.
ENDIF

LOCATE
IF FOUND()
  LOCATE REST FOR BETWEEN(ldDate,Dfsppbgdt,Dfsppendt) WHILE (ldDate >= Dfsppbgdt)
ENDIF
IF !FOUND()                  && No period match checked date
  llContinue = .F.
ELSE
  &lcFYear  = Cfisfyear      && Transaction date year
  &lcPeriod = Cfspprdid      && Transaction date period
ENDIF
IF !llContinue             && There is an error.
  lcErrorM1  = 'This receiving date '
  lcErrorM1 =  lcErrorM1 + DTOC(ldDate) + ' does not fall within any period. '
  lcErrorM1 = lcErrorM1 + 'Would you like to continue any way ?'
  IF gfModalGen('INM00274B34001','ALERT',lcErrorM1)=1
    SELECT (lnAlias)
    RETURN(.T.)
  ELSE
    SELECT (lnAlias)
    RETURN(.F.)
  ENDIF
ENDIF
SELECT (lnAlias)
RETURN(.T.)

*!*************************************************************
*! Name      : lfvRecvTyp
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To validate the receiving types
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
FUNCTION lfvRecvTyp
LPARAMETERS loFormSet

WITH loFormSet.AriaForm1
  IF .cboReceivingTypes.VALUE = .cboReceivingTypes.OldValue
    DO CASE
      *-- 'I' Receive P/O, 'R' Issue Return P/o, 'M' Receive C/T, 'N' Issue Inter-Location P/o
      *-- 'O' Receive Inter-Location P/o, 'D' Receive Dye Order, 'A' Issue Adornment order
      *-- 'E' Receive Adornment order, 'P' Receive Material PO, 'G' Return Material PO
      *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
      *CASE loFormSet.lcPType $ 'IRMNODAEPG'
    CASE loFormSet.lcPType $ 'IRMNODAEPGW'
      *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
      .kbPONo.keytextbox.CONTROLSOURCE = ""
      .kbPONo.ENABLED = .T.
      *.kbPONo.SetFocus

      *-- 'S' Receive by Style Shipment, 'U' Issue Inter Location PO
      *-- 'C' Receive Inter Location PO Shipment, 'F' Receive Material Shipment
    CASE loFormSet.lcPType $ 'SUCF'
      .cntShipment.ENABLED = .T.
      .cntShipment.kbShipNo.ENABLED = .T.
      IF loFormSet.lcPType $ 'SFUC'
        *.cntShipment.kbShipNo.SetFocus
      ENDIF

      *-- 'B' Receive P/O Batch, 'T' Receive C/T Batch, 'L' Receive Inter Location P/O Batch'
      *-- 'H' Issue Inter Location P/O Batch'
    CASE loFormSet.lcPType $ 'BTLH'
      *.cntBatch.kbBatchNo.SetFocus
    ENDCASE
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
    IF loFormSet.lcPType $  'MIO' && AND (.cboReceivingTypes.Value <> .cboReceivingTypes.OldValue)
      IF !(.cboReceivingTypes.OldValue $ 'SCBTLHIOM')
        .grdReceivingLines.HEIGHT = .grdReceivingLines.HEIGHT - .cntBatch.HEIGHT
        .grdReceivingLines.TOP = .grdReceivingLines.TOP + .cntBatch.HEIGHT + 2
      ENDIF
      .cntBatch.VISIBLE = .T.
      .cntBatch.ENABLED = .T.
      .cntShipment.VISIBLE = .F.
      .cntShipment.ENABLED = .F.
      .cntBatch.kbBatchNo.ENABLED = .T.
      .cntBatch.kbBatchNo.keytextbox.VALUE = ""
    ENDIF
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
    RETURN
  ENDIF

  *-- Create the temporary files
  *=lfCreatTmp(loFormSet)

  loFormSet.lcPType = .cboReceivingTypes.VALUE
  =lfObjStatus(loFormSet,.F.)
  loFormSet.llMulCurr  = IIF(loFormSet.lcPType $ 'DAE',.F.,loFormSet.llUseMCurr)
  *!*	  *-- 'H' Issue Inter Location P/O Batch', 'B' Receive P/O Batch, 'R' Issue Return P/o
  *!*	  *-- 'M' Receive C/T, 'T' Receive C/T Batch, 'D' Receive Dye Order
  *!*	  *-- 'A' Issue Adornment order, 'E' Receive Adornment order
  *!*	  IF loFormSet.lcPType $ 'HBRMDAELT'
  *!*	    WAIT WINDOW "Program still under development"
  *!*	  ENDIF

  DO CASE
    *-- 'I' Receive P/O, 'R' Issue Return P/o, 'M' Receive C/T, 'N' Issue Inter-Location P/o
    *-- 'O' Receive Inter-Location P/o, 'D' Receive Dye Order, 'A' Issue Adornment order
    *-- 'E' Receive Adornment order, 'P' Receive Material PO
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
    *CASE loFormSet.lcPType $ 'IRMNODAEPG'
  CASE loFormSet.lcPType $ 'IRMNODAEPGW'
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
    *IF .cboReceivingTypes.OldValue $ "SBTLHCFU"
    IF IIF(loFormSet.lcPType $ 'MIO',.F.,.cboReceivingTypes.OldValue $ "SBTLHCFUIOM" )
      .CNTBATCH.VISIBLE = .F.
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
      .grdReceivingLines.HEIGHT = .grdReceivingLines.HEIGHT + .cntShipment.HEIGHT
      .grdReceivingLines.TOP = .cntShipment.TOP
      .cntShipment.VISIBLE = .F.
    ENDIF
    .kbPONo.ENABLED = .T.
    IF loFormSet.llMFCall AND loFormSet.lcPType='M'
      .kbItem.ENABLED = .T.
    ENDIF
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
    *.kbPONo.KeyTextBox.SetFocus
    IF !loFormSet.llSilentMod
      .kbPONo.KeyTextBox.SETFOCUS
    ENDIF
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]


    *-- 'B' Receive P/O Batch, 'T' Receive C/T Batch, 'L' Receive Inter Location P/O Batch'
    *-- 'H' Issue Inter Location P/O Batch'
  CASE loFormSet.lcPType $ 'BTLH'
    IF !(.cboReceivingTypes.OldValue $ 'SCBTLH')
      .grdReceivingLines.HEIGHT = .grdReceivingLines.HEIGHT - .cntBatch.HEIGHT
      .grdReceivingLines.TOP = .grdReceivingLines.TOP + .cntBatch.HEIGHT + 2
    ENDIF
    .cntBatch.VISIBLE = .T.
    .cntBatch.ENABLED = .T.
    .cntShipment.VISIBLE = .F.
    .cntShipment.ENABLED = .F.
    .cntBatch.kbBatchNo.ENABLED = .T.
    .cntBatch.kbBatchNo.keytextbox.VALUE = ""
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
    *.cntBatch.kbBatchNo.SetFocus
    IF !loFormSet.llSilentMod
      .cntBatch.kbBatchNo.SETFOCUS
    ENDIF
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]


    *-- 'S' Receive by Shipment, 'U' Issue Inter-Location PO Shipment
    *-- 'C' Receive Inter Location PO Shipment, 'F' Receive Material Shipment
  CASE loFormSet.lcPType $ 'SUCF'
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
    *IF !(.cboReceivingTypes.OldValue $ 'BTLHCSU')
    IF !(.cboReceivingTypes.OldValue $ 'BTLHCSUIOM')
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
      .grdReceivingLines.HEIGHT = ;
        .grdReceivingLines.HEIGHT - .cntShipment.HEIGHT
      .grdReceivingLines.TOP = .grdReceivingLines.TOP + .cntShipment.HEIGHT + 2
    ENDIF
    .cntShipment.VISIBLE = .T.
    .cntShipment.ENABLED = .T.
    .cntBatch.VISIBLE = .F.
    .cntBatch.ENABLED = .F.
    .cntShipment.kbShipNo.ENABLED = .T.
    .cntShipment.kbShipNo.keytextbox.VALUE = ""
    IF loFormSet.lcPType $ 'SCFU'
      *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
      *.cntShipment.kbShipNo.SetFocus
      IF !loFormSet.llSilentMod
        .cntShipment.kbShipNo.SETFOCUS
      ENDIF
      *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
    ENDIF
  ENDCASE
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
  IF loFormSet.lcPType $  'IOM' && AND (.cboReceivingTypes.Value <> .cboReceivingTypes.OldValue)
    IF !(.cboReceivingTypes.OldValue $ 'SCBTLHIOM')
      .grdReceivingLines.HEIGHT = .grdReceivingLines.HEIGHT - .cntBatch.HEIGHT
      .grdReceivingLines.TOP = .grdReceivingLines.TOP + .cntBatch.HEIGHT + 2
    ENDIF
    .cntBatch.VISIBLE = .T.
    .cntBatch.ENABLED = .T.
    .cntShipment.VISIBLE = .F.
    .cntShipment.ENABLED = .F.
    .cntBatch.kbBatchNo.ENABLED = .T.
    .cntBatch.kbBatchNo.keytextbox.VALUE = ""
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
    *.cntBatch.kbBatchNo.SetFocus
    IF !loFormSet.llSilentMod
      .cntBatch.kbBatchNo.SETFOCUS
    ENDIF
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
  ENDIF
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
ENDWITH
*-- Assign the business and work order
DO CASE
  *-- 'I' Receive P/O, 'S' Receive by Shipment,'B' Receive P/O Batch
CASE loFormSet.lcPType $ 'ISB'
  loFormSet.lcBusDoc   = 'P'
  loFormSet.lcWorkOrd  = 'P'

  *-- 'N' Issue Inter-Location P/o, 'O' Receive Inter-Location P/o
  *-- 'L' Receive Inter Location P/O Batch, 'H' Issue Inter Location P/O Batch
  *-- 'U' Issue Inter-Location PO Shipment, 'C' Receive Inter Location PO Shipment
CASE loFormSet.lcPType $ 'NOLHUC'
  loFormSet.lcBusDoc   = 'N'
  loFormSet.lcWorkOrd  = 'N'

  *-- 'M' Receive C/T, 'T' Receive C/T Batch
CASE loFormSet.lcPType $ 'MT'
  loFormSet.lcBusDoc   = 'P'
  loFormSet.lcWorkOrd  = 'U'

  *-- 'R' Issue Return P/o
CASE loFormSet.lcPType = 'R'
  loFormSet.lcBusDoc   = 'R'
  loFormSet.lcWorkOrd  = 'P'

  *-- 'A' Issue Adornment order
CASE loFormSet.lcPType = 'A'
  loFormSet.lcBusDoc   = 'P'
  loFormSet.lcWorkOrd  = 'A'

  *-- 'E' Receive Adornment order
CASE loFormSet.lcPType = 'D'
  loFormSet.lcBusDoc   = 'P'
  loFormSet.lcWorkOrd  = 'D'

  *-- Receive Material PO and Material Shipment PO
CASE loFormSet.lcPType $ 'PF'
  loFormSet.lcBusDoc   = 'P'
  loFormSet.lcWorkOrd  = 'M'

  *-- Issue Material PO
CASE loFormSet.lcPType = 'G'
  loFormSet.lcBusDoc   = 'R'
  loFormSet.lcWorkOrd  = 'M'
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
CASE loFormSet.lcPType = 'W'
  loFormSet.lcBusDoc   = 'P'
  loFormSet.lcWorkOrd  = 'F'
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
ENDCASE
*N038893,1 WAM 06/02/2005 Receive Cutting Ticket
*!*	IF loFormSet.lcPType = 'OUCN'
*!*	  loFormSet.AriaForm1.lblPO.Caption = 'Inter-Loc. P/O#'
*!*	ELSE
*!*	  loFormSet.AriaForm1.lblPO.Caption = 'P/O#'
*!*	ENDIF
DO CASE
CASE loFormSet.lcPType = 'OUCN'
  loFormSet.AriaForm1.lblPO.CAPTION = 'Inter-Loc. P/O#'
CASE loFormSet.lcPType $ 'MT'
  loFormSet.AriaForm1.lblPO.CAPTION = 'C/T #'
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
CASE loFormSet.lcPType = 'W'
  loFormSet.AriaForm1.lblPO.CAPTION = 'Order #'
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
OTHERWISE
  loFormSet.AriaForm1.lblPO.CAPTION = 'P/O#'
ENDCASE
*N038893,1 WAM 06/02/2005 (End)

*-- To assign the aria work order key properties
=lfSetKeyPro(loFormSet,loFormSet.lcPType)

=lfActBrow(loFormSet)

*!*	IF loFormSet.lcPType $ 'RNAH'
*!*	lcnTpMode = IIF(lcPType $ 'RNAH','DISABLE','ENABLE')
*!*	ENDIF

RETURN

*!*************************************************************
*! Name      : lfAddPro
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
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
*! Name      : lfObjStatus
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To disable/enable the edit region
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfObjStatus
LPARAMETERS loFormSet, llStatus

WITH loFormSet.AriaForm1
  STORE .F. TO .txtstock.ENABLED, .txtothers.ENABLED, .txtcancel.ENABLED,;
    .txtitemDesc.ENABLED, .txtprice.ENABLED, .txtduty.ENABLED

  STORE llStatus TO .kbPONo.ENABLED , .kbPONo.keyCmd.ENABLED , .kbItem.ENABLED ,;
    .kbconfiguration.ENABLED  , .cboLocations.ENABLED ,;
    .txtreference.ENABLED     , .txtpattern.ENABLED,;
    .txtpriceRate.ENABLED     , .txtDutyRate.ENABLED,;
    .cmdLineQty.ENABLED       , .cmdNew.ENABLED,;
    .cmdRemove.ENABLED        , .cntShipment.ENABLED

  *-- To change the configuration lable and disable it in case dyelot not used in both
  *-- IC and MA
  IF loFormSet.lcInvType = "0001"
    IF loFormSet.llConfig
      .lblConfiguration.CAPTION = "Configuration"
    ELSE
      IF loFormSet.llDyelot
        .lblConfiguration.CAPTION = "Dyelot"
        .kbconfiguration.llVlDyelot = .T.
      ELSE
        .lblConfiguration.VISIBLE = .F.
        .kbconfiguration.VISIBLE = .F.
      ENDIF
    ENDIF
  ELSE
    IF loFormSet.llFabDye
      .lblConfiguration.CAPTION = "Dyelot"
      .kbconfiguration.llVlDyelot = .T.
    ELSE
      .lblConfiguration.VISIBLE = .F.
      .kbconfiguration.VISIBLE = .F.
    ENDIF
  ENDIF

  *-- To be shown only in the receiving of materials in this phase because
  *-- the pattern does not exist in the po screen
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
  *IF !(loFormSet.lcPType $ 'PF')
  IF !(loFormSet.lcPType $ 'PFW')
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
    .lblPattern.VISIBLE = .F.
    .lblSimClo6.VISIBLE = .F.
    .txtpattern.VISIBLE = .F.
  ENDIF

  *-- If not keep track of bins
  IF !loFormSet.llWareLoc
    .chkBins.VISIBLE = .F.
  ELSE
    .chkBins.ENABLED = llStatus
  ENDIF

  *-- IF not multi warehouse or 'N' Issue Inter-location, 'H' Issue Inter-Location Shipment
  IF  loFormSet.llWareHous OR loFormSet.lcPType $ "NH"
    .cboLocations.ENABLED = .F.
  ENDIF

ENDWITH

*!*************************************************************
*! Name      : lfShowScr
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To control the disabling/enabling of the screen
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfShowScr
LPARAMETERS loFormSet

=lfObjStatus(loFormSet,.F.)

WITH loFormSet.AriaForm1
  DO CASE
    *-- 'B' Receive P/O Batch, 'T' Receive C/T Batch, 'L' Receive Inter Location P/O Batch'
    *-- 'H' Issue Inter Location P/O Batch'
  CASE loFormSet.lcPType $ 'BTLH'
    .cntBatch.ENABLED = .F.

    *-- 'I' Receive P/O, 'R' Issue Return P/o, 'M' Receive C/T, 'N' Issue Inter-Location P/o
    *-- 'O' Receive Inter-Location P/o, 'D' Receive Dye Order, 'A' Issue Adornment order
    *-- 'E' Receive Adornment order, 'P' Receive Material PO
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
    *CASE loFormSet.lcPType $ 'IRMNODAEPG'
  CASE loFormSet.lcPType $ 'IRMNODAEPGW'
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
    IF !(loFormSet.lcPType $ 'IOM')
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
      .cntBatch.ENABLED = .F.
      .cntBatch.VISIBLE = .F.
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
    ENDIF
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
    .cntShipment.ENABLED = .F.
    .cntShipment.VISIBLE = .F.
    IF (!loFormSet.llLinkToGl OR !EMPTY(loFormSet.lcGLFYear+loFormSet.lcGLPeriod))
      .cmdNew.ENABLED = .T.
      IF !EOF(loFormSet.lcTmpLine)
        *IF loFormSet.lcInvType = "0002"
        *-- 'R' Issue Return P/O, 'N' Issue Inter-Location P/O
        *-- 'U' Issue Inter Location P/O Shipment, 'O' Receive Inter-Location P/O
        *-- 'C' Receive Inter Location P/O Shipment, 'L' Receive Inter Location P/O Batch
        *-- 'A' Issue Adornment order, 'E' Receive Adornment order
        IF !(loFormSet.lcPType $ 'RNOLCAEU')

          *B000211,1 WAM 03/03/2005 Disable configuration button
          *.kbconfiguration.Enabled = (EVALUATE(loFormSet.lcTmpLine+'.cDye_Flg') = "Y")
          .kbconfiguration.ENABLED = .F.
          *B000211,1 WAM (End)

        ENDIF
        .cmdRemove.ENABLED = .T.
        IF loFormSet.lcPType $ 'NA'
          .cboLocations.ENABLED = .F.
          .chkBins.ENABLED = .F.
        ELSE
          .cboLocations.ENABLED = .T.
          .chkBins.ENABLED = .T.
        ENDIF
        .txtReference.ENABLED = .T.
        IF loFormSet.llWareLoc AND USED(loFormSet.lcTemLoc)
          .chkBins.VALUE = IIF(SEEK(EVALUATE(loFormSet.lcTmpLine+'.Style')+;
            EVALUATE(loFormSet.lcTmpLine+'.cWareCode'),;
            loFormSet.lcTemLoc) ,.T.,.F.)
        ENDIF
        *! N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
        *IF loFormSet.lcPType = 'P'
        IF loFormSet.lcPType $ 'WP'
          *! N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][End]
          .txtPattern.ENABLED = .T.
        ENDIF
      ENDIF
    ENDIF
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
    IF (loFormSet.lcPType $ 'IOM')
      IF !EMPTY(loFormSet.AriaForm1.CntBatch.kbBatchNo.KeyTextBox.VALUE)
        IF loFormSet.llApproveBatch AND loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE <> 'A'
          loFormSet.AriaForm1.CntBatch.cboBatchStatus.ENABLED = .T.
        ELSE
          loFormSet.AriaForm1.CntBatch.cboBatchStatus.ENABLED = .F.
        ENDIF
        loFormSet.AriaForm1.CntBatch.kbBatchNo.ENABLED =.F.
        IF (loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE $ 'XP') OR (loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE ='A' AND !loFormSet.llApproveBatch)
          lfObjStatus(loFormSet,.F.)
          loFormSet.AriaForm1.CntBatch.DtpickerBatchDate.ENABLED =!(loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE $ 'XP')
          loFormSet.AriaForm1.CntBatch.txtBatchDesc.ENABLED = !(loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE $ 'XP')
          IF (loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE <> 'A' AND loFormSet.llApproveBatch) AND !(loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE $ 'XP')
            loFormSet.AriaForm1.CntBatch.cboBatchStatus.ENABLED =.T.
          ELSE
            loFormSet.AriaForm1.CntBatch.cboBatchStatus.ENABLED =.F.
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]

    *-- 'S' Receive by Style PO Shipment, 'U' Issue Inter-Location PO Shipment
    *-- 'C' Receive Inter-Location PO Shipment, 'F' Receive Material Shipment
  CASE loFormSet.lcPType $ 'SUCF'
    .cntShipment.ENABLED = .T.
    .cntShipment.kbShipNo.ENABLED = .F.
    .cmdNew.ENABLED = .F.
    IF !EOF(loFormSet.lcTmpLine)
      STORE .T. TO .cntShipment.dtpickerShpEntered.ENABLED,;
        .cntShipment.dtpickerShpETA.ENABLED,;
        .cntShipment.txtShpCartons.ENABLED ,;
        .cntShipment.txtShpAirWay.ENABLED  ,;
        .cntShipment.txtShpReference.ENABLED

      .cmdRemove.ENABLED = .T.

      *B128741,1 KHM 06/30/2005 Enable the location in case of shipment [Begin]
      .cboLocations.ENABLED = .T.
      *B128741,1 KHM 06/30/2005 [End]

      IF loFormSet.llWareLoc
        .chkBins.ENABLED = .T.
        IF USED(loFormSet.lcTemLoc)
          .chkBins.VALUE = IIF(SEEK(EVALUATE(loFormSet.lcTmpLine+'.Style')+;
            EVALUATE(loFormSet.lcTmpLine+'.cWareCode'),;
            loFormSet.lcTemLoc) ,.T.,.F.)
        ENDIF

      ENDIF
      .txtReference.ENABLED = .T.

      IF loFormSet.lcPType = 'F'
        .txtPattern.ENABLED = .T.
      ENDIF


    ENDIF
  ENDCASE


  *!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [START]
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
  *!*	  IF loFormSet.lcInvType="0002" AND loFormSet.lcPType $ 'FPG'
  *!*	    IF loFormSet.lcPType $ 'GFP' AND loFormSet.lcCostMthM = 'L' AND loFormSet.llTrkRolls AND USED(loFormSet.lcTmpItem) AND ;
  *!*	        SEEK(loFormSet.lcInvType+EVALUATE(loFormSet.lcTmpLine+'.Style'),loFormSet.lcTmpItem);
  *!*	        AND EVALUATE(loFormSet.lcTmpItem+'.LTRKROLLS')
  IF loFormSet.lcInvType="0002" AND loFormSet.lcPType $ 'FPGW'
    IF loFormSet.lcPType $ 'WGFP' AND loFormSet.lcCostMthM = 'L' AND loFormSet.llTrkRolls AND USED(loFormSet.lcTmpItem) AND ;
        SEEK(loFormSet.lcInvType+EVALUATE(loFormSet.lcTmpLine+'.Style'),loFormSet.lcTmpItem);
        AND EVALUATE(loFormSet.lcTmpItem+'.LTRKROLLS')
      *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][End]
      loFormSet.AriaForm1.cmdRolls.ENABLED = .T.
      loFormSet.AriaForm1.cmdRolls.CAPTION = 'Ro\<lls'
    ELSE
      IF (loFormSet.lcPType = 'G' AND loFormSet.lcCostMthM = 'L' AND (!loFormSet.llTrkRolls OR (loFormSet.llTrkRolls AND USED(loFormSet.lcTmpItem) AND ;
          SEEK(loFormSet.lcInvType+EVALUATE(loFormSet.lcTmpLine+'.Style'),loFormSet.lcTmpItem);
          AND !EVALUATE(loFormSet.lcTmpItem+'.LTRKROLLS'))))
        loFormSet.AriaForm1.cmdRolls.ENABLED = .T.
        loFormSet.AriaForm1.cmdRolls.CAPTION = '\<Lots'

      ELSE

        loFormSet.AriaForm1.cmdRolls.ENABLED = .F.
      ENDIF
    ENDIF
  ELSE
    loFormSet.AriaForm1.cmdRolls.ENABLED = .F.
    loFormSet.AriaForm1.cmdRolls.VISIBLE = .F.
  ENDIF
  *!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]


  *-- If return style PO or return material PO
  IF loFormSet.lcPType $ 'RG' AND !EMPTY(EVALUATE(loFormSet.lcPosHdr+'.CPONO'))
    .cboLocations.ENABLED = .F.
    .chkBins.ENABLED = .F.
  ENDIF
  *-- If style PO and multi-currency and change exchange rate and price currency
  *-- or duty currency is not equal to the base currency and !eof.
  IF (!loFormSet.llMFCall AND loFormSet.llMulCurr AND loFormSet.llEditExRt) AND ;
      (loFormSet.lcCur1 <> oAriaApplication.BaseCurrency OR;
      loFormSet.lcCur2 <> oAriaApplication.BaseCurrency) AND !EOF(loFormSet.lcTmpLine)
    IF loFormSet.lcCur1 <> oAriaApplication.BaseCurrency
      .txtpriceRate.ENABLED = .T.
    ENDIF
    IF loFormSet.lcCur2 <> oAriaApplication.BaseCurrency
      .txtDutyRate.ENABLED = .T.
    ENDIF
  ENDIF

  IF !EOF(loFormSet.lcTmpLine)
    *-- Edit Quantity button.
    .cmdLineQty.ENABLED = .T.
    IF loFormSet.llConfig AND EVALUATE(loFormSet.lcTmpLine+'.cDye_Flg') = "Y"
      .kbconfiguration.lcstylecode = EVALUATE(loFormSet.lcTmpLine+'.Style')
      .kbconfiguration.lcwarecode  = EVALUATE(loFormSet.lcTmpLine+'.cWareCode')
    ENDIF

    IF TYPE('loFormSet.lcPosLn') = 'C' AND USED(loFormSet.lcPosLn)
      =SEEK(loFormSet.lcBusDoc+loFormSet.lcWorkOrd+EVALUATE(loFormSet.lcTmpLine+'.PO')+;
        loFormSet.lcInvType+EVALUATE(loFormSet.lcTmpLine+'.Style')+;
        STR(EVALUATE(loFormSet.lcTmpLine+'.LineNo'),6), loFormSet.lcPosLn)
    ENDIF
    loFormSet.lcPONo   = EVALUATE(loFormSet.lcTmpLine+'.PO')
    loFormSet.lcStyle  = EVALUATE(loFormSet.lcTmpLine+'.Style')
    loFormSet.lcDyelot = PADR(EVALUATE(loFormSet.lcTmpLine+'.Dyelot'),10)
  ENDIF
ENDWITH
RETURN

*!*************************************************************
*! Name      : lfwBrow
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : When valid function for browse.
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfwBrow
LPARAMETERS loFormSet
=lfReadLine(loFormSet,EOF())
RETURN

*!*************************************************************
*! Name      : lfReadLine
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To get the line information
*!*************************************************************
*! Parameters: loFormSet : FormSet, llClearLn
*!*************************************************************
FUNCTION lfReadLine
LPARAMETERS loFormSet,llClearLn
LOCAL lnAlias

lnAlias = SELECT()

IF !llClearLn
  SELECT (loFormSet.lcTmpLine)
  loFormSet.lnWare  = IIF(loFormSet.llCMInstld AND loFormSet.llPOSale,1,;
    ASCAN(loFormSet.laWare,cWareCode,1))

  IF loFormSet.llMulCurr AND !loFormSet.llMfCall
    *-Get price currency and rate.
    loFormSet.lcCur1  = cPriceCur
    loFormSet.lnRate1 = nLanPrRat
    *-Get duty currency and rate.
    loFormSet.lcCur2  = cDutyCur
    loFormSet.lnRate2 = nLanDuRat
  ENDIF
  *-- If receive to damage or cancel disable the warehouse
  IF loFormSet.lnTotDam <> 0 OR loFormSet.lnTotCan <> 0
    loFormSet.AriaForm1.cboLocations.ENABLED = .F.
  ENDIF

ELSE
  STORE ' ' TO loFormSet.lcCur1,loFormSet.lcCur2
  STORE 0   TO loFormSet.lnRate1,loFormSet.lnRate2
  STORE 1   TO loFormSet.lnCurrUnt1,loFormSet.lnCurrUnt2
ENDIF
=lfShowScr(loFormSet)
loFormSet.REFRESH
SELECT(lnAlias)
RETURN

*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To create the temporary files
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfCreatTmp
LPARAMETERS loFormSet

SELECT (loFormSet.lcPosLn)
=AFIELDS(laFStru)
lnFStru = ALEN(laFStru,1)
*B126833,1 WAM 04/03/2005 add new button to edit landed cost for styles has no detail costing
*DIMENSION laFStru[lnFStru+16,18]
DIMENSION laFStru[lnFStru+17,18]
*B126833,1 WAM 04/03/2005 (End)

laFStru[lnFStru+1,1] = 'TOTSTK'
laFStru[lnFStru+2,1] = 'TOTDAM'
laFStru[lnFStru+3,1] = 'TOTCAN'
laFStru[lnFStru+4,1] = 'TOTBAL'
laFStru[lnFStru+5,1] = 'CCARTON'
laFStru[lnFStru+6,1] = 'NLINENO'
laFStru[lnFStru+7,1] = 'LNEWLN'
laFStru[lnFStru+8,1] = 'NSTEPS'
laFStru[lnFStru+9,1] = 'LALOCHG'
laFStru[lnFStru+10,1]= 'CLASTOPR'
laFStru[lnFStru+11,1]= 'LNEWLUPD'
laFStru[lnFStru+12,1]= 'LCOSTMADE'
laFStru[lnFStru+13,1]= 'LAUTOMODE'
laFStru[lnFStru+14,1]= 'cStyDesc'
laFStru[lnFStru+15,1] = 'cRcvBy'
laFStru[lnFStru+16,1] = 'cDye_Flg'
*B126833,1 WAM 04/03/2005 add new button to edit landed cost for styles has no detail costing
laFStru[lnFStru+17,1] = 'lDetCost'
*B126833,1 WAM 04/03/2005 (End)

STORE 'N' TO laFStru[lnFStru+1,2],laFStru[lnFStru+2,2],;
  laFStru[lnFStru+3,2],laFStru[lnFStru+4,2],;
  laFStru[lnFStru+6,2],laFStru[lnFStru+8,2]

STORE 'C' TO laFStru[lnFStru+5,2],laFStru[lnFStru+10,2],laFStru[lnFStru+14,2],;
  laFStru[lnFStru+15,2],laFStru[lnFStru+16,2]

*B126833,1 WAM 04/03/2005 add new button to edit landed cost for styles has no detail costing
*STORE 'L' TO laFStru[lnFStru+7,2],laFStru[lnFStru+9,2],;
laFStru[lnFStru+11,2],laFStru[lnFStru+13,2],;
laFStru[lnFStru+12,2]
STORE 'L' TO laFStru[lnFStru+7,2],laFStru[lnFStru+9,2],;
  laFStru[lnFStru+11,2],laFStru[lnFStru+13,2],;
  laFStru[lnFStru+12,2],laFStru[lnFStru+17,2]
*B126833,1 WAM 04/03/2005 (End)

*! B608824,1 MMT 03/30/2009 Fix bug of Wrong Values while adjustemnt Material[Start]
*IF loFormSet.lcPType $ 'PF'
*N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
*IF loFormSet.lcPType $ 'PFG'
IF loFormSet.lcPType $ 'PFGW'
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
  *! B608824,1 MMT 03/30/2009 Fix bug of Wrong Values while adjustemnt Material[End]
  STORE  12 TO laFStru[lnFStru+1,3],laFStru[lnFStru+2,3],;
    laFStru[lnFStru+3,3],laFStru[lnFStru+4,3]
ELSE
  STORE  6  TO laFStru[lnFStru+1,3],laFStru[lnFStru+2,3],;
    laFStru[lnFStru+3,3],laFStru[lnFStru+4,3]
ENDIF
STORE  6  TO laFStru[lnFStru+6,3],laFStru[lnFStru+10,3]

STORE  3  TO laFStru[lnFStru+5,3]

STORE  2  TO laFStru[lnFStru+8,3]

*B126833,1 WAM 04/03/2005 add new button to edit landed cost for styles has no detail costing
*STORE  1  TO laFStru[lnFStru+7,3],laFStru[lnFStru+9,3],;
laFStru[lnFStru+11,3],laFStru[lnFStru+13,3],;
laFStru[lnFStru+12,3],laFStru[lnFStru+16,3]
STORE  1  TO laFStru[lnFStru+7,3],laFStru[lnFStru+9,3],;
  laFStru[lnFStru+11,3],laFStru[lnFStru+13,3],;
  laFStru[lnFStru+12,3],laFStru[lnFStru+16,3],laFStru[lnFStru+17,3]
*B126833,1 WAM 04/03/2005 (End)

STORE 60  TO laFStru[lnFStru+14,3]

STORE 19  TO laFStru[lnFStru+15,3]

*! B608824,1 MMT 03/30/2009 Fix bug of Wrong Values while adjustemnt Material[Start]
*IF loFormSet.lcPType $ 'PF'
*N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
*IF loFormSet.lcPType $ 'PFG'
IF loFormSet.lcPType $ 'PFGW'
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
  *! B608824,1 MMT 03/30/2009 Fix bug of Wrong Values while adjustemnt Material[End]
  STORE  3  TO laFStru[lnFStru+1,4],laFStru[lnFStru+2,4],;
    laFStru[lnFStru+3,4],laFStru[lnFStru+4,4]
ELSE
  STORE  0  TO laFStru[lnFStru+1,4],laFStru[lnFStru+2,4],;
    laFStru[lnFStru+3,4],laFStru[lnFStru+4,4]
ENDIF
*B126833,1 WAM 04/03/2005 add new button to edit landed cost for styles has no detail costing
*!*	STORE  0  TO laFStru[lnFStru+5,4],laFStru[lnFStru+6,4],;
*!*	             laFStru[lnFStru+7,4],laFStru[lnFStru+8,4],;
*!*	             laFStru[lnFStru+9,4],laFStru[lnFStru+10,4],;
*!*	             laFStru[lnFStru+11,4],laFStru[lnFStru+13,4],;
*!*	             laFStru[lnFStru+12,4],laFStru[lnFStru+14,4],;
*!*	             laFStru[lnFStru+15,4],laFStru[lnFStru+16,4]
*!*	FOR lnI = 7 TO 16
*!*	  FOR lnJ = 1 TO 16
*!*	    STORE '' TO laFStru[lnFStru+lnJ,lnI]
*!*	  ENDFOR
*!*	ENDFOR
*!*	FOR lnJ = 1 TO 16
*!*	  STORE 0 TO laFStru[lnFStru+lnJ,17],laFStru[lnFStru+lnJ,18]
*!*	ENDFOR
STORE  0  TO laFStru[lnFStru+5,4],laFStru[lnFStru+6,4],;
  laFStru[lnFStru+7,4],laFStru[lnFStru+8,4],;
  laFStru[lnFStru+9,4],laFStru[lnFStru+10,4],;
  laFStru[lnFStru+11,4],laFStru[lnFStru+13,4],;
  laFStru[lnFStru+12,4],laFStru[lnFStru+14,4],;
  laFStru[lnFStru+15,4],laFStru[lnFStru+16,4],laFStru[lnFStru+17,4]
FOR lnI = 7 TO 16
  FOR lnJ = 1 TO 17
    STORE '' TO laFStru[lnFStru+lnJ,lnI]
  ENDFOR
ENDFOR
FOR lnJ = 1 TO 17
  STORE 0 TO laFStru[lnFStru+lnJ,17],laFStru[lnFStru+lnJ,18]
ENDFOR
*B126833,1 WAM 04/03/2005 (End)

DIMENSION laTags[3,2]
DIME laTags[5,2]
laTags[1,1]='TranCd+cCarton+Po+Style+Dyelot+cWareCode+STR(LineNo,6)'
laTags[2,1]='cCarton+PO+Style+Dyelot+cWareCode+STR(LineNo,6)+TranCd'
laTags[3,1]='PO+Style+Dyelot+cWareCode+STR(LineNo,6)+cCarton+TranCd'
laTags[4,1]='cstytype+po+style+STR(lineno,6)+TranCd'
laTags[5,1]='shipno+cstytype+po+style+STR(lineno,6)+trancd'
laTags[1,2]='TmpLine1'
laTags[2,2]='TmpLine2'
laTags[3,2]='TmpLine3'
laTags[4,2]='POSLN'
laTags[5,2]='POSLNSH'

*T20071102.0018(C200876) TMI [Start] Add new fields to be updated when using bin location custom
IF ASCAN(loFormSet.laEvntTrig,PADR('ADDFILDS',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
  =gfDoTriger("POSTREC",PADR("ADDFILDS",10))
ENDIF
*T20071102.0018(C200876) TMI [End  ]

=gfCrtTmp(loFormSet.lcTmpLine,@laFStru,@laTags)
SELECT (loFormSet.lcTmpLine)
SET ORDER TO TAG TmpLine1

*--Warehouse location tmp file.
IF loFormSet.llWareLoc
  SELECT WhsLoc
  =AFIELDS(laFStru)
  =gfCrtTmp(loFormSet.lcTemLoc,@laFStru,'Style+cWareCode+cLocation',loFormSet.lcTemLoc)
  =CURSORSETPROP("Buffering",5,loFormSet.lcTemLoc)
ENDIF

*T20071102.0018(C200876) TMI [Start] create the lcBinLine temp file
IF ASCAN(loFormSet.laEvntTrig,PADR('CRTBINLN',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0 ;
    .AND. gfDoTriger('POSTREC',PADR('ISUSEBIN',10))
  PRIVATE lnbinOldAls
  lnbinOldAls=SELECT(0)
  SELECT (loFormSet.lcTmpLine)
  =gfDoTriger("POSTREC",PADR("CRTBINLN",10))
  SELECT(lnbinOldAls)
ENDIF
*T20071102.0018(C200876) TMI [End  ]

RETURN

*!*************************************************************
*! Name      : lfvTCode
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To validate the transaction code by shipment or batch
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfvTCode
LPARAMETERS loFormSet, llBrowse
LOCAL lnAlias

WITH loFormSet.AriaForm1
  lnAlias = SELECT()
  DO CASE
    *-- 'S' Style PO Shipment, 'U' Issue Inter-Location PO Shipment
    *-- 'F' Receive Material Shipment, 'C' Receive Inter-Location PO Shipment
  CASE loFormSet.lcPType $ 'SUFC' AND !lfvShipmnt(loFormSet)
    SELECT(lnAlias)
    RETURN .F.

    *-- 'B' Receive P/O Batch, 'T' Receive C/T Batch,
    *-- 'L' Receive Inter Location P/O Batch', 'H' Issue Inter Location P/O Batch'
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
    *CASE loFormSet.lcPType $ 'BTLH' AND !lfvBatch(loFormSet)
  CASE loFormSet.lcPType $ 'MIOBTLH'  AND !lfvBatch(loFormSet,llBrowse)
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
    SELECT(lnAlias)
    RETURN .F.
  ENDCASE

  STORE .F. TO .dtPickerPostingDate.ENABLED, .dtpickerReceivingDate.ENABLED
  .cboReceivingTypes.ENABLED = .F.

  SELECT(lnAlias)
ENDWITH
RETURN .T.

*!*************************************************************
*! Name      : lfvPO
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To validate the work order #
*!*************************************************************
*! Parameters: loFormSet : FormSet, llBrowse
*!*************************************************************
FUNCTION lfvPO
LPARAMETERS loFormSet, llBrowse
LOCAL lnAlias, lcPrvAlis, llAbort, lcTxtMsg1, lcTxtMsg2, lcTxtMsg2, lcSqlStatement
PRIVATE lcPoTtle, lcBrFields
loFormSet.lcPONo = loFormSet.AriaForm1.kbPONo.KeyTextBox.VALUE
IF EMPTY(loFormSet.lcPONo)
  loFormSet.AriaForm1.kbPONo.KeyTextBox.VALUE = ""
  IF !EOF(loFormSet.lcTmpLine)
    *-- To assign object's control source.
    =lfCntrSour(loFormSet,.T.)
    =lfwBrow(loFormSet)
  ENDIF
  RETURN
ENDIF

lnAlias = SELECT()

llAbort  = .F.
*N038893,1 WAM 06/02/2005 Receive Cutting Ticket
*lcPoTtle = IIF(loFormSet.lcPType$'RG','Return P/O',IIF(loFormSet.lcPType='D','Dye Order',;
IIF(loFormSet.lcPType='A','Adornment Order',IIF(loFormSet.lcPType$'NOCLU','Inter-Location P/O','P/O'))))
*N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
*!*	lcPoTtle = IIF(loFormSet.lcPType$'RG','Return P/O',IIF(loFormSet.lcPType='D','Dye Order',;
*!*	  IIF(loFormSet.lcPType='A','Adornment Order',;
*!*	  IIF(loFormSet.lcPType$'NOCLU','Inter-Location P/O',;
*!*	  IIF(loFormSet.lcPType$'MT','Cutting ticket','P/O')))))
lcPoTtle = IIF(loFormSet.lcPType$'RG','Return P/O',IIF(loFormSet.lcPType='D','Dye Order',;
  IIF(loFormSet.lcPType='A','Adornment Order',;
  IIF(loFormSet.lcPType$'NOCLU','Inter-Location P/O',;
  IIF(loFormSet.lcPType$'MT','Cutting ticket',IIF(loFormSet.lcPType='W',LANG_POSTREC_MMO_MMO,'P/O'))))))
*N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
*N038893,1 WAM 06/02/2005 (End)

lcTxtMsg1 = IIF(loFormSet.llIssue,'issuing','receiving')
lcTxtMsg2 = IIF(loFormSet.llIssue,'issue','receive')
DO WHILE .T.
  DO CASE
  CASE EVALUATE(loFormSet.lcPosHdr+'.Status') = 'S'
    *--XXX status is XXX. Therefore,no receivings can be done.
    = gfModalGen('INM34055B42000','DIALOG',lcPoTtle+'|'+'Closed'+'|'+lcTxtMsg1)
    llAbort=.T.
    EXIT
  CASE EVALUATE(loFormSet.lcPosHdr+'.Status') = 'B'
    *--XXX status is XXX. Therefore,no receivings can be done.
    = gfModalGen('INM34055B42000','DIALOG',lcPoTtle+'|'+'Bid'+'|'+lcTxtMsg1)
    llAbort=.T.
    EXIT
  CASE EVALUATE(loFormSet.lcPosHdr+'.Status') = 'H'
    *--XXX status is Hold since a cost sheet has not been created yet. Therefore,no receivings can be done.
    = gfModalGen('INM34056B42000','DIALOG',lcPoTtle)
    llAbort=.T.
    EXIT
  CASE EVALUATE(loFormSet.lcPosHdr+'.Status') = 'X'
    *--XXX has been canceled. Not allowed to receive.
    = gfModalGen('INM34057B42000','DIALOG',lcPoTtle+'|'+lcTxtMsg2)
    llAbort=.T.
    EXIT
  CASE EVALUATE(loFormSet.lcPosHdr+'.Status') = 'C'
    IF loFormSet.lcPType $ 'NA'
      *--XXX status is complete. Therefore,no issuings can be done.
      = gfModalGen('INM34055B42000','DIALOG',lcPoTtle+'|'+'complete'+'|'+lcTxtMsg1)
      llAbort=.T.
      EXIT
    ELSE
      *--XXX is completely received. Do you wish to continue ?,\<Yes;\<No
      *-- 'O' Receive inter-location PO
      IF loFormSet.lcPtype = 'O'
        = gfModalGen('INM00000B00000','','','',lcPoTtle +' is completely received. Cannot proceed.')
        llAbort=.T.
        EXIT
      ELSE
        IF gfModalGen('INM34058B42002','DIALOG',lcPoTtle) = 2
          llAbort=.T.
          EXIT
        ENDIF
      ENDIF
    ENDIF
  ENDCASE
  *-- Get the lines from POSLN
  lcSqlStatement  =  "SELECT  POSLN.*, POSHDR.cPriceCur, POSHDR.cDutyCur, POSHDR.Status, POSHDR.CPONO "+;
    "FROM POSLN posln (INDEX=POSLN) inner join POSHDR (INDEX=POSHDR) "+;
    "ON POSHDR.cBusDocu = POSLN.cBusDocu AND POSHDR.cStyType = POSLN.cStyType "+;
    "AND  POSHDR.PO = POSLN.PO "+;
    "WHERE POSLN.cBusDocu = '" + loFormSet.lcBusDoc +;
    "' AND POSLN.cStyType = '" + loFormSet.lcWorkOrd +;
    "' AND POSLN.PO = '" + loFormSet.lcPONo +;
    "' AND POSLN.cInvType='"+loFormSet.lcInvType + "'"

  =lfOpenSql(lcSqlStatement,'POSLN',loFormSet.lcPosLn, "","",.F.)
  DIMENSION laIndex[1,2]
  laIndex = ''
  laIndex[1,1] = 'cBusDocu+cStyType+PO+cInvType+Style+STR(LineNo,6)+TranCd'
  laIndex[1,2] = 'POSLN'
  =lfSetIndex(loFormSet.lcPosLn,@laIndex)

  *-- Set the item quality
  SELECT (loFormSet.lcPosLn)
  LOCATE
  loFormSet.AriaForm1.kbItem.cQuality = EVALUATE(loFormSet.lcPosLn+'.cStyGrade')
  lcPrvAlis = ALIAS()
  SELECT (loFormSet.lcPosLn)
  *-- IF the Po is included in a shippment , cann't receive by PO [Begin]
  *-- 'I' Receive Style PO, 'O' Receive Inter location PO
  *-- 'P' Receive Material PO, 'N' Issue Inter location PO
  IF loFormSet.lcPtype $ 'IOPN'
    LOCATE FOR TranCd = '3'
    IF FOUND()
      *-- This XX is included in shipment XX. You can either XX by shipment,
      *-- or remove the XX from this shipment.
      *=gfModalGen('INM34181B00000','DIALOG',EVALUATE(loFormSet.lcPosLn+'.ShipNo'))
      lcTxtMsg1 = IIF(loFormSet.lcPType $ 'NO','Inter-Location P/O','P/O')
      lcTxtMsg2 = ALLTRIM(EVALUATE(loFormSet.lcPosLn+'.ShipNo'))
      lcTxtMsg3 = IIF(loFormSet.lcPType = 'N','issue','receive')
      =gfModalGen('INM34204B00000','DIALOG',lcTxtMsg1+'|'+lcTxtMsg2+'|'+lcTxtMsg3+'|'+lcTxtMsg1)
      *B000121,1 WAM 03/03/2005 Do not continue
      *llAbort=.T.
      *B000121,1 WAM 03/03/2005 (End)
      EXIT
    ENDIF
    LOCATE
    SELECT (lcPrvAlis)
  ENDIF
  *-- Check if this PO has BOM materials.
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
  *IF !(loFormSet.lcPType $ 'RDAEPFNOG') AND loFormSet.llImpCost
  IF !(loFormSet.lcPType $ 'RDAEPFNOGW') AND loFormSet.llImpCost
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][End]
    llMatExist = .F.
    DO CASE
      *-- 'I' Receive P/O
    CASE loFormSet.lcPType $ 'ISB'
      lcCstShtType = 'I'

      *-- 'M' Receive C/T
    CASE loFormSet.lcPType $ 'MT'
      lcCstShtType = 'M'

    ENDCASE

    *-- Check if there a work order cost sheet and there is a material,
    *-- style component or trim and inventory maintained.
    lcSqlStatement = "SELECT cImTyp, CutTkt FROM CTKTBOM [INDEX=CTKTBOM] "+;
      " WHERE cImTyp = '" + lcCstShtType + "' AND "+;
      "CutTkt ='" + loFormSet.lcPONo +;
      "' AND (cCatgTyp = 'F' OR cCatgTyp = 'S' OR "+;
      "(cCatgTyp = 'T' AND Trim_Invt=1))"

    =lfWOrdBOM(lcSqlStatement,.T.,.F.,.F.,.F.,loFormSet)
    IF USED(loFormSet.lcCTktBom)
      SELECT (loFormSet.lcCTktBom)
      LOCATE
      llMatExist = !EOF()
    ENDIF

    SELECT (loFormSet.lcPosLn)
    IF llMatExist
      *-- Check if there an issued item.
      lcSqlStatement = "SELECT cImTyp, cTktNo FROM BOMCOST [INDEX=POBOMCLS] "+;
        " WHERE cImTyp = '" + lcCstShtType + "' AND "+;
        "cTktNo ='" + loFormSet.lcPONo + "'"
      =lfWOrdBOM(lcSqlStatement,.F.,.T.,.F.,.F.,loFormSet)

      IF USED(loFormSet.lcBomCost)
        SELECT(loFormSet.lcBomCost)
        LOCATE
        *--No cost items have been applied against this P/o. Are you sure you want to
        *-- receive ?
        *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
        *IF EOF() AND gfModalGen('QRM34071B42002','DIALOG') = 2
        IF EOF() AND gfModalGen(IIF(loFormSet.lcPType = 'M','QRM38240B42002','QRM34071B42002'),'DIALOG') = 2
          *N038893,1 WAM 06/02/2005 (End)

          llAbort=.T.
          EXIT
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  *B000109,1 WAM 03/05/2005 Lock PO header
  IF .F.
    SELECT (loFormSet.lcTmpLine)
    lclckKey = PO+STYLE+Dyelot+cWareCode+STR(LINENO,6)+cCarton+TranCd
    IF !SEEK(EVALUATE(loFormSet.lcPosHdr+'.po'),loFormSet.lcTmpLine,'TmpLine3')
      IF EVALUATE(loFormSet.lcPosHdr+'.lLok_Stat')
        IF ALLTRIM(EVALUATE(loFormSet.lcPosHdr+'.cLok_User')) = ALLTRIM(oAriaApplication.User_ID)
          IF gfModalGen("INM00240B00006","ALERT")=2
            llAbort=.T.
            =SEEK(lclckKey,loFormSet.lcTmpLine,'TmpLine3')
            EXIT
          ENDIF
        ELSE
          =gfModalGen("INM00028B00000","ALERT",oAriaApplication.getUserName(EVALUATE(loFormSet.lcPosHdr+'.cLok_User')))
          llAbort=.T.
          =SEEK(lclckKey,loFormSet.lcTmpLine,'TmpLine3')
          EXIT
        ENDIF
      ENDIF
      lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'',.T.)
      IF TYPE('lcTranCode') = 'N'
        =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
        llAbort=.T.
        =SEEK(lclckKey,loFormSet.lcTmpLine,'TmpLine3')
        EXIT
      ELSE
        lcSelString = "UPDATE POSHDR SET lLok_stat =1,cLok_User= '"+oAriaApplication.User_ID+"', dLok_Date='"+DTOS(oAriaApplication.SystemDate)+"',cLok_Time='"+TIME()+"' WHERE cBusDocu+cStyType+PO='"+EVALUATE(loFormSet.lcPosHdr+'.cBusDocu')+EVALUATE(loFormSet.lcPosHdr+'.cStyType')+EVALUATE(loFormSet.lcPosHdr+'.Po')+"'"
        lnResult = oAriaApplication.RemoteCompanyData.Execute(lcSelString,'',"SAVEFILE","",lcTranCode ,4,'',SET("DataSession"))
        IF lnResult <=0
          =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLUPDATE",lnResult,.T.)
          =oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
          llAbort=.T.
          =SEEK(lclckKey,loFormSet.lcTmpLine,'TmpLine3')
          EXIT
        ELSE
          =oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
        ENDIF
      ENDIF
    ENDIF
    =SEEK(lclckKey,loFormSet.lcTmpLine,'TmpLine3')
  ENDIF
  *B000109,1 WAM 03/05/2005 (End)
  EXIT
ENDDO

*!*	IF loFormSet.lcPType = 'A' .AND. ASCAN(laEvntTrig,PADR("VADORORD",10)) <> 0
*!*	  =gfDoTriger("PORCVAP",PADR("VADORORD",10))
*!*	ENDIF
IF llAbort
  loFormSet.AriaForm1.kbPONo.KeyTextBox.VALUE = SPACE(6)
  SELECT(lnAlias)
  RETURN .F.
ENDIF

SELECT (loFormSet.lcPosLn)
IF EOF()
  *-The lines for this 'P/O' are missing ! cannot proceed.
  =gfModalGen('TRM34017B42000','DIALOG','P/O')
  loFormSet.AriaForm1.kbPONo.KeyTextBox.VALUE = SPACE(6)
  SELECT(lnAlias)
  RETURN .F.
ENDIF

*--Check InTransit record.
*-- 'O' Receive inter location PO, 'E' Receive adornment order
IF loFormSet.lcPType $ 'OE'
  LOCATE FOR Trancd='6'
  IF !FOUND()
    IF loFormSet.lcPType = 'O'
      *--Inter-location PO not yet issued from the target location. Cannot proceed.
      =gfModalGen('TRM34109B42000','DIALOG')
    ELSE
      *--The Adornment purchase order hasn't been issued yet from the targer location, cannot proceed!.
      =gfModalGen('TRM38176B00000','DIALOG')
    ENDIF
    loFormSet.AriaForm1.kbPONo.KeyTextBox.VALUE = SPACE(6)
    SELECT(lnAlias)
    RETURN .F.
  ENDIF
ENDIF
*-- Do you wish to select xxxx lines, Manually (by line) or Automatic (all) ?
*--  <Manually>   <Automatic>.
SELECT (loFormSet.lcTmpLine)
LOCATE FOR PO = loFormSet.AriaForm1.kbPONo.KeyTextBox.VALUE
IF !FOUND()
  IF loFormSet.lcPType $ 'RG' AND !EMPTY(EVALUATE(loFormSet.lcPosLn+'.CPONO'))
    loFormSet.lcAuto = 'A'
  ELSE

    *! C201211,1 HES 01/20/2010 Handle Scanned Barcodes from Recieve PO Screen -By PO- [Start]
    IF ASCAN(loformset.laEvntTrig,PADR('SCANBARCD',10),1,ALEN(loformset.laEvntTrig,1),1) > 0
      IF TYPE('loFormSet.lnCAlias') = 'U'
        loFormSet.ADDPROPERTY('lnCAlias',lnAlias)
      ENDIF
      loFormSet.mDoTrigger(PADR('SCANBARCD' ,10))
      IF loFormSet.lcAuto = 'S'
        RETURN
      ENDIF
    ELSE
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
      IF !(loFormSet.lcPType $ 'IO')
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
        *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
        *loFormSet.lcAuto = IIF(gfModalGen('QRM34126B34008','DIALOG',lcPoTtle) = 1,'M','A')
        IF !loFormSet.llSilentMod
          loFormSet.lcAuto = IIF(gfModalGen('QRM34126B34008','DIALOG',lcPoTtle) = 1,'M','A')
        ELSE
          loFormSet.lcAuto = 'A'
          loFormSet.VISIBLE = .F.
        ENDIF
        *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
      ELSE
        *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
        *lnResp = gfModalGen('QRM34126B34218','DIALOG',lcPoTtle)
        IF !loFormSet.llSilentMod
          lnResp = gfModalGen('QRM34126B34218','DIALOG',lcPoTtle)
        ELSE
          lnResp = 2
          loFormSet.VISIBLE = .F.
        ENDIF
        *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

        DO CASE
        CASE lnResp  = 1
          loFormSet.lcAuto = 'M'
        CASE lnResp  = 2
          loFormSet.lcAuto = 'A'
        OTHERWISE
          loFormSet.lcAuto = 'I'
        ENDCASE
      ENDIF
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
    ENDIF
    *! C201211,1 HES 01/20/2010 Handle Scanned Barcodes from Recieve PO Screen -By PO- [End]

  ENDIF
ELSE

  IF ASCAN(loformset.laEvntTrig,PADR('VLDEXIST',10),1,ALEN(loformset.laEvntTrig,1),1) > 0
    IF loFormSet.mDoTrigger(PADR('VLDEXIST' ,10))
      SELECT(lnAlias)
      RETURN .F.
    ENDIF
  ENDIF
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
  IF !(loFormSet.lcPType $ 'IO')
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
    loFormSet.lcAuto = IIF(lAutoMode,'A','M')
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
  ELSE
    IF loFormSet.lcAuto  <> 'I'
      loFormSet.lcAuto = IIF(lAutoMode,'A','M')
    ENDIF
  ENDIF
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  IF lAutoMode
    *--This XXXX already selected.
    = gfModalGen('INM34128B42000','DIALOG',lcPoTtle)
    loFormSet.AriaForm1.kbPONo.KeyTextBox.VALUE = SPACE(6)
    SELECT(lnAlias)
    RETURN .F.
  ENDIF
ENDIF

*N000587,1 WAM 12/01/2007 Get style cost sheet
lcPosHdr = loFormSet.lcPosHdr
=lfGetBomLn(loFormSet,&lcPosHdr..cStyType, &lcPosHdr..PO, PADR(loFormSet.AriaForm1.cntShipment.kbShipNo.keytextbox.VALUE,6))
*N000587,1 WAM 12/01/2007 (End)

lcCarton = ''
SELECT (loFormSet.lcPosLn)

*! C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
IF ASCAN(loformset.laevnttrig, PADR('PRCSPUPREF', 10), 1, ALEN(loformset.laevnttrig, 1), 1)>0
  DO lfpreisshdat IN DIRMAIN.FXP WITH loformset
ENDIF
*! C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]

IF loFormSet.lcAuto = 'A'
  lcFTrnCd = IIF(loFormSet.lcPType$'OE','6','1')
  SCAN FOR Trancd = lcFTrnCd
    loFormSet.lcStyle   = STYLE
    *-- Get the style information
    IF loFormSet.lcInvType = "0001"
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
      *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = '" + loFormSet.lcStyle + "'"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
      *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = [" + loFormSet.lcStyle + "]"      
      lcItemValue = loFormSet.lcStyle
      lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = ?m.lcItemValue"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][END]      
    ELSE
      *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
      *lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loFormSet.lcInvType +;
      "' AND Style = '" + loFormSet.lcStyle + "'"
      lcItemValue = loFormSet.lcStyle
      lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loFormSet.lcInvType +;
        "' AND Style = ?m.lcItemValue"
      *B607658,1 KHM 07/07/2005 [End]

    ENDIF

    =lfGetItmInf(loFormSet.lcInvType,;
      IIF(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+loFormSet.lcStyle,;
      loFormSet.lcTmpItem,;
      IIF(loFormSet.lcInvType = "0001",'STYLE','ITEM'),lcSqlStatement,;
      loFormSet.lcInvType = "0002")
    =SEEK(IIF(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+loFormSet.lcStyle,;
      loFormSet.lcTmpItem)
    SELECT (loFormSet.lcPosLn)
    lnRecNo   = RECNO()
    IF SEEK('1'+SPACE(3)+PO+STYLE+Dyelot+cWareCode+STR(LINENO,6),loFormSet.lcTmpLine)
      LOOP
    ENDIF
    =lfvItem(loFormSet,LINENO)
    SELECT (loFormSet.lcPosLn)
    GOTO lnRecNo
    IF loFormSet.lcAuto = 'X'
      *--The style xxxxx will be ignored.
      = gfModalGen('INM34203B42000','DIALOG',;
        IIF(loFormSet.lcInvType='0001','Style','Fabric')+ALLTRIM(loFormSet.lcStyle))
      loFormSet.lcAuto = 'A'
    ENDIF
  ENDSCAN
  SELECT (loFormSet.lcTmpLine)
  LOCATE
  =lfActBrow(loFormSet)
  =lfwBrow(loFormSet)
  STORE .F. TO loFormSet.AriaForm1.dtPickerPostingDate.ENABLED,;
    loFormSet.AriaForm1.dtpickerReceivingDate.ENABLED,;
    loFormSet.AriaForm1.cboReceivingTypes.ENABLED

  SELECT(lnAlias)
  RETURN
ENDIF
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
IF  loFormSet.lcAuto = 'I'
  lnBatchSel = oAriaApplication.RemoteCompanyData.execute("Select * from SCAN_BATCH_HEADER_T WHERE [STATUS]='O'",'',;
    "BATCH_HEADER_T","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))
  llErrorLog = .F.
  IF lnBatchSel > 0
    SELECT BATCH_HEADER_T
    CURSORSETPROP("Buffering", 3, 'BATCH_HEADER_T')
    INDEX ON BATCH TAG BATCH
    lcBrFields = [BATCH :R :H= LANG_POSTREC_BATCH_NO,DESCRIPTION :R :H= LANG_POSTREC_BATCH_DESC, ] +;
      [Vendor :R :H= LANG_POSTREC_BATCH_VENDOR, DATE :R :H= LANG_POSTREC_BATCH_DATE]
    DIMENSION laBrowArr[2]
    laBrowArr = ''
    lcBatch = ''
    IF !EMPTY(loFormSet.laUsedBatchPO[1,1])
      ACOPY(loFormSet.laUsedBatchPO,laBatch)
      SET FILTER TO ASCAN(laBatch,ALLTRIM(BATCH_HEADER_T.SCAN_BATCH_HEADER_KEY)) = 0
    ENDIF
    lcBatch = IIF(ARIABROW('',LANG_POSTREC_BATCH_TITLE ,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, ;
      gnBrFSCol2,'','','BATCH,SCAN_BATCH_HEADER_KEY','laBrowArr'),laBrowArr[1],SPACE(6))

    IF !EMPTY(lcBatch)
      IF EMPTY(loFormSet.laUsedBatchPO[1,1])
        loFormSet.laUsedBatchPO[1,1] = laBrowArr[2]
        loFormSet.laUsedBatchPO[1,2] = loFormSet.AriaForm1.kbPONo.KeyTextBox.VALUE
      ELSE
        DIMENSION loFormSet.laUsedBatchPO[ALEN(loFormSet.laUsedBatchPO,1)+1,2]
        loFormSet.laUsedBatchPO[ALEN(loFormSet.laUsedBatchPO,1),1] = laBrowArr[2]
        loFormSet.laUsedBatchPO[ALEN(loFormSet.laUsedBatchPO,1),2] = loFormSet.AriaForm1.kbPONo.KeyTextBox.VALUE
      ENDIF
      IF USED('TMPSTR')
        USE IN TMPSTR
      ENDIF

      CREATE CURSOR TMPSTR (mStrRep M(10))
      SELECT TMPSTR
      APPEND BLANK
      REPLACE mStrRep WITH REPLICATE('*',68) + CHR(13) +;
        LANG_POSTREC_BATCH_ERROR+ CHR(13) +;
        REPLICATE('*',68) + CHR(13) + ' ' + CHR(13)

      loFormSet.AriaForm1.kbItem.ENABLED = .T.
      lcbatchid =   laBrowArr[2]
      lnBatchDSel = oAriaApplication.RemoteCompanyData.execute("Select * from SCAN_BATCH_DETAILS_T WHERE SCAN_BATCH_HEADER_KEY= '"+;
        ALLTRIM(lcbatchid) +"'" ,'',;
        "SCAN_BATCH_DETAILS_T","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))



      IF lnBatchDSel > 0
        SELECT SCAN_BATCH_DETAILS_T
        SELECT DISTINCT STYLE,SPACE(60) AS REASON FROM SCAN_BATCH_DETAILS_T ORDER BY STYLE INTO CURSOR 'BATCHSTYLE'
        SELECT 'BATCHSTYLE'
        llFrst = .T.
        SCAN
          loFormSet.lcStyle   = BATCHSTYLE.STYLE
          *-- Get the style information
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
          *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = '" + loFormSet.lcStyle + "'"
          *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
          *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = [" + loFormSet.lcStyle + "]"          
          lcStyleSelValue = loFormSet.lcStyle 
          lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = ?m.lcStyleSelValue"
          *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][END]
          =lfGetItmInf(loFormSet.lcInvType,;
            IIF(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+loFormSet.lcStyle,;
            loFormSet.lcTmpItem,;
            IIF(loFormSet.lcInvType = "0001",'STYLE','ITEM'),lcSqlStatement,;
            loFormSet.lcInvType = "0002")
          =SEEK(IIF(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+loFormSet.lcStyle,;
            loFormSet.lcTmpItem)
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
          *lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE='S"+EVALUATE(loFormSet.lcTmpItem+'.Scale')+"'"
          *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
          *lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=[S"+EVALUATE(loFormSet.lcTmpItem+'.Scale')+"]"          
          lcSelScaleValue = "S"+EVALUATE(loFormSet.lcTmpItem+'.Scale')
          lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=?m.lcSelScaleValue"
          *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][END]
          =lfOpenFox(lcSqlStatement,'SCALE','SCALE',"")
          lcMessageParam = ''
          IF loFormSet.lcPType  ='O'
            lcExpSeek =loFormSet.lcBusDoc+loFormSet.lcWorkOrd+loFormSet.AriaForm1.kbPoNo.keytextbox.VALUE+loFormSet.lcInvType+loFormSet.lcStyle
            IF SEEK(lcExpSeek,loFormSet.lcPosln,'POSLN')
              SELECT (loFormSet.lcPosln)
              LOCATE REST WHILE cBusDocu+cStyType+PO+cInvType+STYLE+STR(LINENO,6)+TRANCD = lcExpSeek FOR TRANCD = '6'
              IF !FOUND()
                lcMessageParam = ['TRM34109B42000','DIALOG']
              ELSE
                =lfvItem(loFormSet,.T.)
              ENDIF
            ELSE
              lcMessageParam = LANG_POSTREC_STYLENOTINPO
            ENDIF
          ELSE
            =lfvItem(loFormSet,.T.)
          ENDIF

          IF !EMPTY(lcMessageParam)
            IF !USED(loFormSet.lcbatchdet)
              SELECT SCAN_BATCH_DETAILS_T
              AFIELDS(laDetStru)
              CREATE CURSOR (loFormSet.lcbatchdet) FROM ARRAY laDetStru
            ENDIF
            lcMessage = ''
            IF !EMPTY(lcMessageParam)
              llErrorLog = .T.
              IF lcMessageParam <> LANG_POSTREC_STYLENOTINPO
                oMessageBox = NEWOBJECT("AriaMessageBox",ADDBS(oAriaApplication.ClassDir)+"Utility.vcx")
                lcMsgtxt=  lcMessageParam
                oMessageBox.getmessage (&lcMsgtxt.)
                lcMessage = oMessageBox.Cmessage
                oMessageBox =NULL
              ELSE
                lcMessage =  lcMessageParam
              ENDIF
              REPLACE mStrRep WITH mStrRep +LANG_POSTREC_BATCH_STYLE+loFormSet.lcStyle+SPACE(5)+lcMessage  + CHR(13) +CHR(10) IN  TMPSTR
              SELECT SCAN_BATCH_DETAILS_T
              SCAN FOR STYLE = loFormSet.lcStyle
                SCATTER MEMO MEMVAR
                m.REJECTION_REASON = lcMessage
                m.STATUS           =  'R'
                INSERT INTO (loFormSet.lcbatchdet) FROM MEMVAR
              ENDSCAN
            ENDIF
          ENDIF
          IF EMPTY(lcMessageParam)
            SELECT (loFormSet.lcTmpLine)
            SET FILTER TO
            lnCurRec = RECNO()
            lcKeyV = cCarton+Po+STYLE+Dyelot+PADR(cWareCode,6)+STR(LINENO,6)
            lcKeyExpr = cBusDocu+cStyType+PO+cInvType+STYLE+STR(LINENO,6)+'6'
            IF loFormSet.lcPType  ='O'
              =SEEK(lcKeyExpr,loFormSet.lcPosln,'POSLN')
            ENDIF
            IF !SEEK('2'+lcKeyV)
              =SEEK('1'+lcKeyV)
              SCATTER MEMO MEMVAR
              m.trancd = '2'
              APPEND BLANK
              GATHER MEMO MEMVAR
            ENDIF
            STORE 0 TO lnQtyRec1,lnQtyRec2,lnQtyRec3,lnQtyRec4,lnQtyRec5,lnQtyRec6,lnQtyRec7,lnQtyRec8
            FOR lnS = 1 TO 8
              lcS = ALLTRIM(STR(lnS))
              REPLACE QTY&lcS. WITH  0 ,;
                TotQty  WITH 0
            ENDFOR
            SELECT  SCAN_BATCH_DETAILS_T
            SCAN FOR STYLE = BATCHSTYLE.STYLE
              FOR lnS = 1 TO SCALE.CNT
                lcS = ALLTRIM(STR(lnS))
                lcSize = SCALE.SZ&lcS.

                IF ALLTRIM(lcSize) = ALLTRIM(SCAN_BATCH_DETAILS_T.SIZE)
                  IF  loFormSet.lcPType  <> 'O'
                    REPLACE QTY&lcS. WITH QTY&lcS. +  SCAN_BATCH_DETAILS_T.Quantity ,;
                      TotQty   WITH TotQty+  SCAN_BATCH_DETAILS_T.Quantity IN (loFormSet.lcTmpLine)

                  ELSE
                    REPLACE QTY&lcS. WITH QTY&lcS. +  MIN(SCAN_BATCH_DETAILS_T.Quantity,EVALUATE(loFormSet.lcPosln+'.QTY'+lcS)) ,;
                      TotQty   WITH TotQty +   MIN(SCAN_BATCH_DETAILS_T.Quantity,EVALUATE(loFormSet.lcPosln+'.QTY'+lcS)) IN (loFormSet.lcTmpLine)

                  ENDIF
                  lnQtyRec&lcS. = lnQtyRec&lcS. +    SCAN_BATCH_DETAILS_T.Quantity
                  EXIT
                ENDIF
              ENDFOR
            ENDSCAN
            REPLACE TotStk WITH TotQty,;
              TotBal WITH MAX(TotQty-TotStk,0) IN (loFormSet.lcTmpLine)
            lnTotalQty = EVALUATE(loFormSet.lcTmpLine+'.TotStk')
            =SEEK('1'+lcKeyV,loFormSet.lcTmpLine)
            REPLACE TotStk WITH lnTotalQty,;
              TotBal WITH MAX(Qty1-lnQtyRec1,0)+MAX(Qty2-lnQtyRec2,0)+MAX(Qty3-lnQtyRec3,0)+MAX(Qty4-lnQtyRec4,0)+;
              MAX(Qty5-lnQtyRec5,0)+MAX(Qty6-lnQtyRec6,0)+MAX(Qty7-lnQtyRec7,0)+MAX(Qty8-lnQtyRec8,0) IN (loFormSet.lcTmpLine)

            SELECT (loFormSet.lcTmpLine)
            SET FILTER TO tranCd ='1'
            IF BETWEEN(lnCurRec ,1,RECCOUNT())
              GO RECORD lnCurRec
            ENDIF

          ENDIF
          IF loFormSet.lcAuto = 'X'
            *--The style xxxxx will be ignored.
            = gfModalGen('INM34203B42000','DIALOG',;
              IIF(loFormSet.lcInvType='0001','Style','Fabric')+ALLTRIM(loFormSet.lcStyle))
            loFormSet.lcAuto = 'A'
          ENDIF
        ENDSCAN
      ENDIF
      IF llErrorLog
        lfShowErrorLog()
      ENDIF
      RETURN
    ENDIF
  ENDIF
ENDIF
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
*-- Call procedure to calculate open in tmp file Set relation between the master file
*-- and the tmp add &lcTempFile..TOTQTY to the lcBrFields
=lfGetOpen(loFormSet)
SELECT (loFormSet.lcPosLn)

*! B608709,1 MMT 10/07/2008 Fix error while receiving PO manually[Start]
*PRIVATE lcTmpFile, lcTmpStyle
*lcTmpFile  = loFormSet.lcTempFile
PRIVATE lcTmprFile, lcTmpStyle
lcTmprFile = loFormSet.lcTempFile
*! B608709,1 MMT 10/07/2008 Fix error while receiving PO manually[End]


lcTmpStyle = loFormSet.lcTmpItem
SET RELATION TO STYLE INTO (lcTmpStyle) ADDITIVE

*! B608709,1 MMT 10/07/2008 Fix error while receiving PO manually[Start]
*SET RELATION TO cBusDocu+cStyType+PO+cInvType+Style+STR(LineNo,6)+Trancd INTO (lcTmpFile) ADDITIVE
SET RELATION TO cBusDocu+cStyType+PO+cInvType+STYLE+STR(LINENO,6)+Trancd INTO (lcTmprFile) ADDITIVE
*! B608709,1 MMT 10/07/2008 Fix error while receiving PO manually[End]

lcStyHdr   = loFormSet.AriaForm1.kbItem.lblItemHeader.CAPTION
lcBrFields = "Style :R :H=lcStyHdr :25,"+;
  "&lcTmpStyle..Desc1 :R :H='Desc':45,"+;
  "lcShpTTl=IIF(!EMPTY(Account),Account,cWareCode) :H='ShipTo',"+;
  "TotQty :R :H='Quantity':P='9999999',"

IF !(loFormSet.lcPType $ 'NOAE')

  *! B608709,1 MMT 10/07/2008 Fix error while receiving PO manually[Start]
  *lcBrFields = lcBrFields +"lnOpnQt=IIF(EVALUATE(lcTmpFile+'.TOTQTY')<0,0,EVALUATE(lcTmpFile+'.TOTQTY')) :R :H='Open':10:P='999999',"
  lcBrFields = lcBrFields +"lnOpnQt=IIF(EVALUATE(lcTmprFile+'.TOTQTY')<0,0,EVALUATE(lcTmprFile+'.TOTQTY')) :R :H='Open':10:P='999999',"
  *! B608709,1 MMT 10/07/2008 Fix error while receiving PO manually[End]

ENDIF
IF loFormSet.llCostPrv
  *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
  IF loFormSet.lcPType = 'M'
    lcBrFields = lcBrFields +"nTotCost =nICost1+nICost2+nICost3+nICost4+nICost5+nICost6+nICost7 :R :H='Total Cost':P='9999999.999',"
  ELSE
    *N038893,1 WAM 06/02/2005 (End)
    lcBrFields = lcBrFields +"nFCost1 :R :H='Price':P='9999999.999',"
  ENDIF
ENDIF
lcBrFields = lcBrFields + "Reference :R"

DIMENSION laTemp[1]
laTemp = ''
=lfActBrow(loFormSet)
SELECT (loFormSet.lcPosLn)
LOCATE

lcFTrnCd = IIF(loFormSet.lcPType$'OE','6','1')
*N038893,1 WAM 06/02/2005 Receive Cutting Ticket
*lcBrowTitl = IIF(loFormSet.lcPType$'RG','return purchase order',;
IIF(loFormSet.lcPType='N','inter location purchase order',;
IIF(loFormSet.lcPType$'AE','adornment order',;
IIF(loFormSet.lcPType='D','Dye order','purchase order'))))
*N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
*!*	lcBrowTitl = IIF(loFormSet.lcPType$'RG','return purchase order',;
*!*	  IIF(loFormSet.lcPType='N','inter location purchase order',;
*!*	  IIF(loFormSet.lcPType$'AE','adornment order',;
*!*	  IIF(loFormSet.lcPType='D','Dye order',IIF(loFormSet.lcPType='M','Cutting Ticket','purchase order')))))
lcBrowTitl = IIF(loFormSet.lcPType$'RG','return purchase order',;
  IIF(loFormSet.lcPType='N','inter location purchase order',;
  IIF(loFormSet.lcPType$'AE','adornment order',;
  IIF(loFormSet.lcPType='D','Dye order',IIF(loFormSet.lcPType='M','Cutting Ticket',IIF(loFormSet.lcPType ='W',LANG_POSTREC_MMO_MMO,'purchase order'))))))
*N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
*N038893,1 WAM 06/02/2005 (End)

*B607927,1 TMI [Start] Fix a bug that the pointer is not located on the correct record, the reason for that was not passing the while condition in the ARIABROW global function
*=ARIABROW([FOR Trancd = lcFTrnCd],'Select ' + lcBrowTitl + ' Line',gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','Style','laTemp')
=ARIABROW([CBUSDOCU+CSTYTYPE+PO+CINVTYPE FOR Trancd = lcFTrnCd],'Select ' + lcBrowTitl + ' Line',gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','Style','laTemp')
*B607927,1 TMI [End  ]

loFormSet.lcStyle = laTemp[1]
=lfActBrow(loFormSet)

SELECT (loFormSet.lcPosLn)
IF EMPTY(loFormSet.lcStyle )
  IF loFormSet.lcPType $ 'NOAE'
    loFormSet.AriaForm1.kbPONo.KeyTextBox.VALUE = SPACE(6)
    SELECT(lnAlias)
    RETURN .F.
  ELSE
    loFormSet.AriaForm1.kbItem.ENABLED = .T.
  ENDIF
ELSE
  IF !SEEK(IIF(loFormSet.lcInvType="0001","","0002")+loFormSet.lcStyle ,loFormSet.lcTmpItem)
    IF loFormSet.lcInvType = "0001"
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
      *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = '" + loFormSet.lcStyle  + "'"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
      *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = [" + loFormSet.lcStyle  + "]"      
      lcStyleSleVal = loFormSet.lcStyle  
      lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = ?m.lcStyleSleVal"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][END]
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
    ELSE
      *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
      *lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loFormSet.lcInvType +;
      "' AND Style = '" + loFormSet.lcStyle  + "'"
      PRIVATE lcItemValue
      lcItemValue = loFormSet.lcStyle
      lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loFormSet.lcInvType +;
        "' AND Style = ?m.lcItemValue"
      *B607658,1 KHM 07/07/2005 [End]
    ENDIF
    =lfGetItmInf(loFormSet.lcInvType,;
      IIF(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+loFormSet.lcStyle ,;
      loFormSet.lcTmpItem,;
      IIF(loFormSet.lcInvType="0001",'STYLE','ITEM'),lcSqlStatement,;
      loFormSet.lcInvType = "0002")
    =SEEK(IIF(loFormSet.lcInvType="0001","","0002")+loFormSet.lcStyle ,loFormSet.lcTmpItem)
  ENDIF
  SELECT (loFormSet.lcPosLn)
  =lfvItem(loFormSet,LINENO)
ENDIF
SELECT(lnAlias)
RETURN

*!*************************************************************
*! Name      : lfvItem
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To validate the style
*!*************************************************************
*! Parameters: loFormSet  : FormSet
*!			   lnPOLLnNo  : LineNo
*!			   llFromItem : .T. when called from item validation
*!*************************************************************
FUNCTION lfvItem
LPARAMETERS loFormSet,lnPOLLnNo,llFromItem
LOCAL lcItmHdr, lcMTxt
*-------
*-- To be done later
*------
*-- If calling from manufactring module to receive cut ticket By Style
*!*	IF loFormSet.llMFCall AND loFormSet.lcPType = 'M'
*!*	  IF llBrowse OR (!EMPTY(PADR(lcStyle,loFormSet.lnMjrWid)) AND EMPTY(lcTCode))
*!*	    = lfvStyCut()
*!*	  ENDIF
*!*	  RETURN
*!*	ENDIF
*-- Get the style information if the user enters the style manualy
IF llFromItem AND ;
    !SEEK(IIF(loFormSet.lcInvType="0001","","0002")+loFormSet.lcStyle ,loFormSet.lcTmpItem)
  IF loFormSet.lcInvType = "0001"
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
    *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = '" + loFormSet.lcStyle  + "'"    
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
    *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = [" + loFormSet.lcStyle  + "]"    
    lcSelStyleVal = loFormSet.lcStyle  
    lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = ?m.lcSelStyleVal "
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
  ELSE
    *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
    *lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loFormSet.lcInvType +;
    "' AND Style = '" + loFormSet.lcStyle  + "'"
    PRIVATE lcItemValue
    lcItemValue = loFormSet.lcStyle
    lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loFormSet.lcInvType +;
      "' AND Style = ?m.lcItemValue "
    *B607658,1 KHM 07/07/2005 [End]
  ENDIF
  =lfGetItmInf(loFormSet.lcInvType,;
    IIF(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+loFormSet.lcStyle ,;
    loFormSet.lcTmpItem,;
    IIF(loFormSet.lcInvType="0001",'STYLE','ITEM'),lcSqlStatement,;
    loFormSet.lcInvType = "0002")
  =SEEK(IIF(loFormSet.lcInvType="0001","","0002")+loFormSet.lcStyle ,loFormSet.lcTmpItem)
  lcCarton = ''
ENDIF

lnAlias = SELECT()

*--Check if it was entered.
IF loFormSet.lcAuto = 'M' AND ((loFormSet.llCMInstld AND loFormSet.llPOSale) OR;
    !(loFormSet.llWareHous AND !(loFormSet.lcPType $ 'NA'))) AND;
    (!loFormSet.llDyelot OR EVALUATE(loFormSet.lcTmpItem+'.cDye_Flg')<>'Y') AND ;
    TYPE('lnPOLLnNo')='N' AND SEEK('1'+SPACE(3)+loFormSet.lcPONo+loFormSet.lcStyle,;
    loFormSet.lcTmpLine)

  SELECT (loFormSet.lcTmpLine)
  LOCATE REST WHILE TranCd+cCarton+Po+STYLE+Dyelot+cWareCode+STR(LINENO,6) = ;
    '1'+SPACE(3)+loFormSet.lcPONo+loFormSet.lcStyle FOR LINENO = lnPOLLnNo
  IF FOUND()
    lcMTxt = IIF(loFormSet.lcPType$'NAU','issue ','receiving ')+;
      IIF(loFormSet.lcPType$'SCUF','shipment',;
      IIF(loFormSet.lcPType$'RG','return P/O',;
      IIF(loFormSet.lcPType='N','inter location P/O' ,;
      IIF(loFormSet.lcPType$'AE','adornment P/O','P/O'))))
    lcItmHdr = IIF(loFormSet.lcInvType = "0002", "Fabric","Style")
    *-This XXXX has been entered on this XXXX.
    =gfModalGen('TRM42107B42000','DIALOG',lcItmHdr+'|'+lcMTxt)
    SELECT(lnAlias)
    RETURN
  ENDIF
  SELECT (loFormSet.lcTmpItem)
ENDIF

llAbort   = .F.
loFormSet.llNewItem = .F.

*-- To check the type of style cost sheet
DO CASE
  *-- 'I' Receive P/O, 'S' Receive by Shipment, 'B' Receive P/O Batch
CASE loFormSet.lcPType $ 'ISBNO'
  lcCstShtType = 'I'

  *-- 'M' Receive C/T, 'T' Receive C/T Batch
CASE loFormSet.lcPType $ 'MT'
  lcCstShtType = 'M'

ENDCASE

=SEEK(IIF(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+loFormSet.lcStyle ,;
  loFormSet.lcTmpItem)
*-- Validation checks loop for Style.....
*- P/O styles check.
DO WHILE .T.
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
  *IF loFormSet.llImpCost AND !(loFormSet.lcPType $ 'RAEPF')
  IF loFormSet.llImpCost AND !(loFormSet.lcPType $ 'RAEPFW')
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][End]
    IF lfGetBOM(loFormSet,lcCstShtType,SUBSTR(loFormSet.lcStyle,1,loFormSet.lnMjrWid),.F.)
      SELECT (loFormSet.lcBomHdr)
      LOCATE FOR cItmMajor = SUBSTR(loFormSet.lcStyle,1,loFormSet.lnMjrWid) AND;
        cCstShtTyp = lcCstShtType AND lDefCstSht AND !EMPTY(cCstSht_Id)
      IF !FOUND()
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
        IF !(loFormSet.lcAuto = 'I' AND loFormSet.lcPType $ 'IO')
          *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
          *-No cost lines found in the cost sheet, Cannot proceed!
          =gfModalGen('TRM34037B42000','DIALOG')
          *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
        ELSE
          lcMessageParam = ['TRM34037B42000','DIALOG']
        ENDIF
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
        llAbort=.T.
        EXIT
      ENDIF
    ELSE
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
      IF !(loFormSet.lcAuto = 'I' AND loFormSet.lcPType $ 'IO')
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
        *-No cost lines found in the cost sheet, Cannot proceed!
        =gfModalGen('TRM34037B42000','DIALOG')
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
      ELSE
        lcMessageParam = ['TRM34037B42000','DIALOG']
      ENDIF
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
      llAbort=.T.
      EXIT
    ENDIF
  ENDIF

  IF loFormSet.lcInvType = '0001' AND EVALUATE(loFormSet.lcTmpItem+'.cDivision') <> EVALUATE(loFormSet.lcPosHdr+'.cDivision')
    *-Conflict ! styles restricted to division XXXX, Cannot proceed!
    *=gfModalGen('TRM34041B42000','DIALOG',ALLTRIM(EVALUATE(loFormSet.lcPosHdr+'.cDivision')))
    lcMTxt = IIF(loFormSet.lcInvType="0001","Style", "Fabric")
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
    IF !(loFormSet.lcAuto = 'I' AND loFormSet.lcPType $ 'IO')
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
      =gfModalGen('TRM36229B34000','DIALOG',lcMTxt+"|"+lcMTxt+"s"+"|"+ALLTRIM(EVALUATE(loFormSet.lcPosHdr+'.cDivision')))
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
    ELSE
      lcMessageParam = ['TRM36229B34000','DIALOG',']+lcMTxt+[|]+lcMTxt+[s|]+ALLTRIM(EVALUATE(loFormSet.lcPosHdr+'.cDivision'))+[']
    ENDIF
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
    llAbort=.T.
    EXIT
  ENDIF

  IF EVALUATE(loFormSet.lcTmpItem+'.Status') = 'X'
    *-This is a canceled style. Not allowed to enter here, Cannot proceed!
    *=gfModalGen('TRM34040B42000','DIALOG')
    lcMTxt = IIF(loFormSet.lcInvType="0001","Style", "Fabric")
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
    IF !(loFormSet.lcAuto = 'I' AND loFormSet.lcPType $ 'IO')
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
      =gfModalGen('TRM36228B34000','DIALOG', lcMTxt)
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
    ELSE
      lcMessageParam = ['TRM36228B34000','DIALOG',']+ lcMTxt+[']
    ENDIF
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
    llAbort=.T.
    EXIT
  ENDIF

  *--Item not on P/O , Modify P/O?
  loFormSet.llNewItem = .F.
  lcSeekTyp           = loFormSet.lcWorkOrd

  IF !SEEK(loFormSet.lcBusDoc+loFormSet.lcWorkOrd+loFormSet.lcPONo+;
      loFormSet.lcInvType+loFormSet.lcStyle,loFormSet.lcPosLn)
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
    IF loFormSet.lcPType = 'O'
      IF (loFormSet.lcAuto = 'I')
        lcMessageParam = LANG_POSTREC_STYLENOTINPO
      ENDIF
      llAbort = .T.
      EXIT
    ENDIF
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
    *--Item not on P/O! Modify P/O.<Yes\<No
    IF gfModalGen('QRM34072B42002','DIALOG') = 2
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
      IF (loFormSet.lcAuto = 'I' AND loFormSet.lcPType $ 'IO')
        lcMessageParam = LANG_POSTREC_STYLENOTINPO
      ENDIF
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
      llAbort = .T.
      EXIT
    ELSE
      loFormSet.llNewItem = .T.
    ENDIF
  ELSE
    IF TYPE('lnPOLLnNo')='N'
      =SEEK(loFormSet.lcBusDoc+loFormSet.lcWorkOrd+loFormSet.lcPONo+;
        loFormSet.lcInvType+loFormSet.lcStyle+STR(lnPOLLnNo,6),loFormSet.lcPosLn)
    ENDIF
  ENDIF

  *!*	  IF loFormSet.lcPType = 'E' .AND. POSLN.nCost2 = 0 .AND. ASCAN(laEvntTrig,PADR("RCVADORD",10)) = 0
  *!*	    =gfModalGen('TRM38182B00000','DIALOG')
  *!*	    llAbort=.T.
  *!*	    EXIT
  *!*	  ENDIF

  =SEEK(IIF(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+loFormSet.lcStyle,;
    loFormSet.lcTmpItem)

  lcWareCode = IIF(EVALUATE(loFormSet.lcPosHdr+'.lMultiWare') AND !loFormSet.llNewItem,;
    EVALUATE(loFormSet.lcPosLn+'.cWareCode'),;
    EVALUATE(loFormSet.lcPosHdr+'.cWareCode'))
  lcWareCode = IIF(loFormSet.lcPType $ 'NA',PADR(EVALUATE(loFormSet.lcPosHdr+'.Vendor'),6),lcWareCode)

  IF loFormSet.llWareHous AND !(loFormSet.lcPType $ 'NOAE')
    *-- Case of style
    IF loFormSet.lcInvType = "0001"
      *-- Get item information from stydye file
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
*!*	      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
*!*	        PADR(loFormSet.lcStyle,19)+lcWareCode+SPACE(10) + "'"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
*!*	      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
*!*	        PADR(loFormSet.lcStyle,19)+lcWareCode+SPACE(10) + "]"
      lcSelValue = PADR(loFormSet.lcStyle,19)+lcWareCode+SPACE(10) 
      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = ?m.lcSelValue "
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]  
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]  
      =lfOpenFox(lcSqlStatement,'STYDYE',loFormSet.lcTmpCurs,"")
      SELECT(loFormSet.lcTmpCurs)
      LOCATE
      IF EOF()
        IF !(EVALUATE(loFormSet.lcPosHdr+'.lMultiWare'))
          *-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
          *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
          *IF loFormSet.lcAuto = 'M'
          IF loFormSet.lcAuto $ 'IM'
            *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
            IF gfModalGen('QRM34048B42006','DIALOG',ALLTRIM(loFormSet.lcStyle)+'|'+lcWareCode) = 1
              DO gpAdStyWar WITH loFormSet.lcStyle,SPACE(10),lcWareCode
            ELSE
              *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
              IF (loFormSet.lcAuto = 'I' AND loFormSet.lcPType $ 'IO')
                lcMessageParam = ['QRM34048B42006','DIALOG',']+ALLTRIM(loFormSet.lcStyle)+[|]+lcWareCode+[']
              ENDIF
              *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
              llAbort=.T.
              EXIT
            ENDIF
          ELSE
            DO gpAdStyWar WITH loFormSet.lcStyle,SPACE(10),lcWareCode
          ENDIF
        ELSE
          lcWareCode =  EVALUATE(loFormSet.lcTmpItem+'.cDefWare')
        ENDIF
      ENDIF
    ELSE
      *-- Get item information from ItemLoc file
      *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
      *lcSqlStatement  = "SELECT * FROM ITEMLOC [INDEX=STYDYE] " + ;
      "WHERE cInvType ='" + loFormSet.lcInvType + "' AND "+;
      "Style ='" + PADR(loFormSet.lcStyle,19) +"' AND " + ;
      "cWareCode ='" + lcWareCode + "' AND " + ;
      "Dyelot = '         '"
      PRIVATE lcItemValue, lcWareCodVal
      lcItemValue  = PADR(loFormSet.lcStyle,19)
      lcWareCodVal = lcWareCode
      lcSqlStatement  = "SELECT * FROM ITEMLOC [INDEX=STYDYE] " + ;
        "WHERE cInvType ='" + loFormSet.lcInvType + "' AND "+;
        "Style = ?m.lcItemValue " + " AND " + ;
        "cWareCode = ?m.lcWareCodVal " + " AND " + ;
        "Dyelot = '         '"
      *B607658,1 KHM 07/07/2005 [End]

      =lfOpenSql(lcSqlStatement,'ITEMLOC',loFormSet.lcTmpCurs, "","",.F.)
      SELECT(loFormSet.lcTmpCurs)
      LOCATE
      IF EOF()
        IF !(EVALUATE(loFormSet.lcPosHdr+'.lMultiWare'))
          *-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
          IF loFormSet.lcAuto = 'M'
            lcMsg = "Fabric: " + ALLTRIM(loFormSet.lcStyle)
            IF gfModalGen('QRM36226B34004','DIALOG',lcMsg +'|'+lcWareCode) = 1
              =gfAdItemWar(loFormSet.lcInvType,loFormSet.lcStyle,SPACE(10),lcWareCode)
            ELSE
              llAbort=.T.
              EXIT
            ENDIF
          ELSE
            =gfAdItemWar(loFormSet.lcInvType,loFormSet.lcStyle,SPACE(10),lcWareCode)
          ENDIF
        ELSE
          lcWareCode =  EVALUATE(loFormSet.lcTmpItem+'.cDefWare')
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  loFormSet.lcDyelot = IIF(!loFormSet.llNewItem AND loFormSet.llDyelot AND ;
    EVALUATE(loFormSet.lcTmpItem+'.cDye_Flg') = 'Y',;
    EVALUATE(loFormSet.lcPosLn+'.Dyelot'),'')
  EXIT
ENDDO

IF llAbort
  SELECT(lnAlias)
  loFormSet.lcAuto = IIF(loFormSet.lcAuto = 'A','X',loFormSet.lcAuto)
  RETURN
ENDIF
loFormSet.AriaForm1.kbPONo.ENABLED = .F.

*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [START]
*N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
*IF loFormSet.lcInvType="0002" AND loFormSet.lcPType $ 'FPG'
IF loFormSet.lcInvType="0002" AND loFormSet.lcPType $ 'FPGW'
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
  IF  loFormSet.lcCostMthM = 'L' AND loFormSet.llTrkRolls AND USED(loFormSet.lcTmpItem) AND ;
      SEEK(loFormSet.lcInvType+EVALUATE(loFormSet.lcTmpLine+'.Style'),loFormSet.lcTmpItem);
      AND EVALUATE(loFormSet.lcTmpItem+'.LTRKROLLS')
    loFormSet.AriaForm1.cmdRolls.ENABLED = .T.
  ELSE
    IF loFormSet.lcPType = 'G' AND loFormSet.lcCostMthM = 'L' AND !loFormSet.llTrkRolls
      loFormSet.AriaForm1.cmdRolls.ENABLED = .T.
    ENDIF
  ENDIF
ELSE
  loFormSet.AriaForm1.cmdRolls.ENABLED = .F.
  loFormSet.AriaForm1.cmdRolls.VISIBLE = .F.
ENDIF
*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]

*--Get a lot no and last operation.
loFormSet.llSpecLot  = .F.
loFormSet.lcLotNo    = SPACE(2)
loFormSet.lcClrLstOp = SPACE(6)

*N038893,1 WAM 06/02/2005 Receive Cutting Ticket
*IF loFormSet.llImpCost AND loFormSet.lcPType $ 'ID' AND !loFormSet.llNewItem
IF (loFormSet.llMfCall OR (loFormSet.llImpCost AND loFormSet.lcPType $ 'ID')) AND !loFormSet.llNewItem
  *N038893,1 WAM 06/02/2005 (End)

  loFormSet.lcLotNo = lfSelLots(STR(EVALUATE(loFormSet.lcPosLn+'.LineNo'),6),;
    EVALUATE(loFormSet.lcPosHdr+'.cLastOpr'),loFormSet)
ENDIF

llShpPO = (loFormSet.lcPType $ 'SUC')

= SEEK(IIF(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+loFormSet.lcStyle,;
  loFormSet.lcTmpItem)
IF loFormSet.lcAuto = 'M' AND ((!(loFormSet.llCMInstld AND loFormSet.llPOSale) AND ;
    loFormSet.llWareHous) OR loFormSet.llDyelot) AND ;
    EVALUATE(loFormSet.lcTmpItem+'.cDye_Flg') = 'Y' AND !(loFormSet.lcPType $ 'ONAE')
  *-- Enable the dyelot field and set focus to it.
  loFormSet.AriaForm1.kbItem.VALUE = loFormSet.lcStyle
  loFormSet.AriaForm1.kbconfiguration.keyTextBox.VALUE =  PADR(loFormSet.lcDyelot,10)
  loFormSet.AriaForm1.kbconfiguration.ENABLED = .T.
  loFormSet.AriaForm1.kbconfiguration.lcstylecode = loFormSet.lcStyle
  loFormSet.lnWare  = ASCAN(loFormSet.laWare,lcWareCode,1)
  loFormSet.AriaForm1.kbconfiguration.lcwarecode  = SUBSTR(loFormSet.laWare[loFormSet.lnWare],1,6)

ELSE
  *  IF (loFormSet.lcPType $ 'ONAE') AND !EMPTY(loFormSet.lcDyelot)
  *    =lfvDyelot(loFormSet)
  *  ELSE
  *SHOW GET lcDyelot DISABLE
  = lfChkLine(loFormSet)    && Check for existance of default location.
  *  ENDIF
ENDIF

RETURN

*!*************************************************************
*! Name      : lfChkLine
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : Check existance of entered location line,
*!             then if found check existance of N/A line.
*!*************************************************************
*! Called from : lfvItem,lfvDyelot
*!*************************************************************
*! Calls       : lfGetInfo
*!*************************************************************
*! Passed Parameters : loFormSet : FormSet
*!*************************************************************
*! Return      : .T. if line is exist before.
*!*************************************************************
FUNCTION lfChkLine
LPARAMETERS loFormSet
SELECT (loFormSet.lcTmpLine)
llEmpWare = .F.
lcCarton   = cCarton
lcCurrRec  = PADR(cCarton,3)+PADR(PO,6)+PADR(STYLE,19)+PADR(Dyelot,10)+PADR(cWareCode,6)+STR(LINENO,6)
lcLineUnq  = STR(EVALUATE(loFormSet.lcPosLn+'.LineNo'),6)
lcWareCode = IIF(TYPE('lcWareCode') $ 'UL',EVALUATE(loFormSet.lcPosLn+'.cWareCode'),lcWareCode)
*-- if this record is found in temp. file before
IF SEEK('1'+PADR(lcCarton,3)+PADR(loFormSet.lcPONo,6)+PADR(loFormSet.lcStyle,19)+;
    PADR(loFormSet.lcDyelot,10)+PADR(lcWareCode,6)+lcLineUnq)
  llEmpWare = .T.
  IF SEEK('1'+PADR(lcCarton,3)+PADR(loFormSet.lcPONo,6)+PADR(loFormSet.lcStyle,19)+;
      PADR(loFormSet.lcDyelot,10)+SPACE(6)+lcLineUnq)
    lnBrRecNo = RECNO()
    lnWare = 1
    =lfwBrow(loFormSet)
    RETURN
  ELSE
    SEEK '1' + lcCurrRec
  ENDIF
ENDIF
=lfGetInfo(loFormSet,llEmpWare)
*-- end of lfChkLine.

*!*************************************************************
*! Name      : lfCalOpen
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : Get original open quantity on P/o and return
*!             the original warehouse.
*!*************************************************************
*! Parameters: loFormSet : FormSet, lcKey: Primary key, llSubtNo
*!*************************************************************
FUNCTION lfCalOpen
LPARAMETERS lcRecvType, lcMastPoLn,lcParmKey,llSubtNo, loFormSet
LOCAL lnAlias, lnMsLnRNo

IF TYPE('loFormSet') = 'O' AND EMPTY(loFormSet.lcDyelot)
  lcParmKey = ALLTRIM(lcParmKey)
ENDIF

lnAlias = SELECT()
SELECT (lcMastPoLn)
lnMsLnRNo = RECNO()
SEEK lcParmKey

lcOrjWareH = cWareCode
laOpnQty   = 0
lcWhlCndn  = "cBusDocu+cStyType+PO+cInvType+Style+STR(LineNo,6)" + " = lcParmKey"

IF !(lcRecvType $ 'OELC')
  lcForCndn = IIF(lcRecvType $ 'MT',".T.","TranCd <> '3'")
  lcBaseTrCd = '1'
ELSE

  *B999999,1 AMH Fix bug of incorrent total stock when receive inter-location po again [Start]
  *lcForCndn = "TranCd <> '1'"
  lcForCndn = "TranCd = '6'"
  *B999999,1 AMH [End]

  lcBaseTrCd = '6'
ENDIF

SCAN REST WHILE &lcWhlCndn FOR &lcForCndn
  FOR I=1 TO 8
    lcCnt=STR(I,1)

    *B999999,1 AMH Fix bug of incorrect open qty [Start]
    *laOpnQty[I]= IIF(TranCd = lcBaseTrCd,laOpnQty[I]+Qty&lcCnt,;
    IIF(lcRecvType $ 'IBSDPF' AND !EMPTY(DYELOT),laOpnQty[I]-Qty&lcCnt,;
    IIF(lcRecvType $ 'OCL',laOpnQty[I]-Qty&lcCnt,MAX(laOpnQty[I]-Qty&lcCnt,0))))
    laOpnQty[I]= MAX(laOpnQty[I]+(EVALUATE('Qty'+lcCnt)*IIF(TranCd = lcBaseTrCd,1,-1)),0)
    *B999999,1 AMH [End]

    IF llSubtNo
      laPrevRecQ[I] = laPrevRecQ[I]+ IIF(TranCd <> lcBaseTrCd,Qty&lcCnt,0)
    ENDIF

  ENDFOR
ENDSCAN

IF lcRecvType $ 'OCL'
  FOR lnCntr = 1 TO 8
    laOpnQty[lnCntr] = MAX(laOpnQty[lnCntr],0)
  ENDFOR
ENDIF
*N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
*IF !llSubtNo AND (lcRecvType $ 'IBSMDPF')
IF !llSubtNo AND (lcRecvType $ 'IBSMDPFW')
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
  SEEK lcParmKey
  SELECT (loFormSet.lcTmpLine)
  lcCurDyelot = IIF(loFormSet.lcPType $ 'BM',Dyelot,PADR(loFormSet.lcDyelot,10))
  lcCurWare   = IIF(loFormSet.lcPType $ 'BM',cWareCode,lcWareCode)
  lcGoAgain   = Trancd+cCarton+Po+STYLE+Dyelot+PADR(cWareCode,6)+STR(LINENO,6)
  lcScanExpr  = IIF(loFormSet.lcPType = 'B',cCarton+Po+STYLE+STR(LINENO,6),;
    '   '+EVALUATE(loFormSet.lcPosLn+'.PO')+loFormSet.lcStyle+;
    STR(EVALUATE(loFormSet.lcPosLn+'.LineNo'),6))
  lcForExpr = IIF(loFormSet.lcPType $ 'BM',"(Dyelot+cWareCode # lcCurDyelot+lcCurWare) AND ;
                  (Trancd # '1')", "Trancd # '1'")
  lcScanCond  = "cCarton"+"Po"+"Style"+"STR(LineNo,6)"
  GO TOP
  IF !EOF()
    SCAN REST WHILE  lcScanCond = lcScanExpr;
        FOR &lcForExpr
      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        laOpnQty[lnI] = laOpnQty[lnI] - Qty&lcI
      ENDFOR
    ENDSCAN
    = SEEK(lcGoAgain)
  ENDIF
ENDIF

SELECT (lcMastPoLn)
IF BETWEEN(lnMsLnRNo,1,RECCOUNT())
  GO lnMsLnRNo
ENDIF

SELECT(lnAlias)
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
RELEASE lnAlias, lnMsLnRNo,lcWhlCndn  ,lcForCndn ,lcBaseTrCd ,I,lcCnt,lnCntr ,lcCurDyelot ,;
  lcCurWare,lcGoAgain   ,lcScanExpr  ,lcForExpr ,lcScanCond  ,lnI,lcI
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]
RETURN (lcOrjWareH)

*!*************************************************************
*! Name      : lfGetOpen
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : Calaulate prv. receive if any per each line
*!           : and brows it in the screen from which we select the
*!           : style/colors in a cutting ticket and PO
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
*! Called from : lfvPo
*!*************************************************************
PROCEDURE lfGetOpen
LPARAMETERS loFormSet

PRIVATE lnRecNo , lnFilePos , laSumAll , lcMastFile , lcScanCond , lcTrancd

*-- To hold the received qty (Stock, cancel, and damage)
PRIVATE laQty, lcSeekTyp, lcSeekCond, lcScanKey, laOpenQty

SELECT (loFormSet.lcPosLn)
lnFilePos = RECNO()
DIMENSION laSumAll[3], laOpenQty[8]

*-- To hold the received qty (Stock, cancel, and damage)
DIMENSION laQty[4,8]
STORE 0 TO laQty, laSumAll

*------------------ CREATE TEMP. FILE -> [BEGIN]
IF !loFormSet.llFirstTmp &&-- do it only one time

  DIMENSION laFileStru[1,4]

  =AFIELDS(laFileStru)
  DIMENSION laFileStru[ALEN(laFileStru,1),18]

  DIME laTags[1,2]
  laTags[1,1]='cBusdocu+cStyType+PO+cInvType+Style+STR(LineNo,6)+TranCd'
  laTags[1,2]= loFormSet.lcTempFile
  =gfCrtTmp(loFormSet.lcTempFile,@laFileStru,@laTags)
  llFirstTmp=.T.
ENDIF

*-- If we did the same calculations befor don't do it again .
IF loFormSet.AriaForm1.kbPONo.keytextbox.VALUE == loFormSet.AriaForm1.kbPONo.keytextbox.OldValue
  RETURN
ENDIF

SELECT (loFormSet.lcTempFile)
SET ORDER TO TAG loFormSet.lcTempFile

lcSeekTyp  = loFormSet.lcWorkOrd
lcSeekCond = loFormSet.lcBusdoc+loFormSet.lcWorkOrd+loFormSet.lcPONo+loFormSet.lcInvType

IF !SEEK(lcSeekCond)
  SELECT (loFormSet.lcPosLn)
  IF SEEK(loFormSet.lcBusdoc+loFormSet.lcWorkOrd+loFormSet.lcPONo+loFormSet.lcInvType)
    DO WHILE cBusDocu+cStyType+PO+cInvType+STYLE+STR(LINENO,6)+TranCd = lcSeekCond
      SCATTER MEMVAR
      lcScanKey = cBusDocu+cStyType+PO+cInvType+STYLE+STR(LINENO,6)
      laOpenQty = 0
      SCAN REST WHILE cBusDocu+cStyType+PO+cInvType+STYLE+STR(LINENO,6)+TranCd = lcScanKey
        IF TranCd = '1'
          FOR lnCntr = 1 TO 8
            lcCntr = STR(lnCntr,1)
            laOpenQty[lnCntr] = laOpenQty[lnCntr] + Qty&lcCntr
          ENDFOR
          *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
          loFormSet.lcStyle   = STYLE
          *-- Get the style information
          IF loFormSet.lcInvType = "0001"
            *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
            *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = '" + loFormSet.lcStyle + "'"            
            *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
            *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = [" + loFormSet.lcStyle + "]"
            lcSelValueSty= loFormSet.lcStyle 
            lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = ?m.lcSelValueSty"
            *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
            *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
          ELSE
            *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
            *lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loFormSet.lcInvType +;
            "' AND Style = '" + loFormSet.lcStyle + "'"
            lcItemValue = loFormSet.lcStyle
            lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loFormSet.lcInvType +;
              "' AND Style = ?m.lcItemValue "
            *B607658,1 KHM 07/07/2005 [End]
          ENDIF
          =lfGetItmInf(loFormSet.lcInvType,IIF(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+loFormSet.lcStyle,;
            loFormSet.lcTmpItem,IIF(loFormSet.lcInvType = "0001",'STYLE','ITEM'),lcSqlStatement,loFormSet.lcInvType = "0002")
          *N038893,1 WAM 06/02/2005 (End)
        ELSE
          IF TranCd $ "245"
            FOR lnCntr = 1 TO 8
              lcCntr = STR(lnCntr,1)
              laOpenQty[lnCntr] = MAX(laOpenQty[lnCntr] - Qty&lcCntr,0)
            ENDFOR
          ENDIF
        ENDIF
      ENDSCAN
      m.Totqty = 0
      FOR lnCntr = 1 TO 8
        m.Totqty = m.Totqty + laOpenQty[lnCntr]
      ENDFOR
      INSERT INTO (loFormSet.lcTempFile) FROM MEMVAR
    ENDDO
  ENDIF
ENDIF
SELECT (loFormSet.lcPosLn)
GO lnFilePos

*!*************************************************************
*! Name      : lfvShipmnt
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To validate the shipment code
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfvShipmnt
LPARAMETERS loFormSet

LOCAL llRet
PRIVATE lcShipNo, lcTranCd
llRet = .T.
lcShipNo = loFormSet.AriaForm1.cntShipment.kbShipNo.keytextbox.VALUE
IF EMPTY(lcShipNo)
  RETURN .F.
ENDIF

*--Shipment validation.
IF EVALUATE(loFormSet.lcMastShp+'.Status') = 'C'
  *--This shipment has been received complete! unable to proceed.
  = gfModalGen('TRM34078B42000','DIALOG','completely received')
  loFormSet.AriaForm1.cntShipment.kbShipNo.keytextbox.VALUE = ""
  RETURN .F.
ENDIF

IF EVALUATE(loFormSet.lcMastShp+'.Status') = 'X'
  *--This shipment has been canceled! unable to proceed.
  = gfModalGen('TRM34078B42000','DIALOG','canceled')
  loFormSet.AriaForm1.cntShipment.kbShipNo.keytextbox.VALUE = ""
  RETURN .F.
ENDIF

*: E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
IF loFormSet.lcPType $ 'S' AND  EVALUATE(loFormSet.lcMastShp+'.Status') <> 'O'
  *-- The shipment is not Open, can not proceed.
  loFormSet.AriaForm1.cntShipment.kbShipNo.keytextbox.VALUE = ""
  = gfModalGen('TRM34134B00000','DIALOG',"Hold")
  RETURN .F.
ENDIF
*: E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]

lcTranCd = IIF(loFormSet.lcPType $ 'SUF','3','6')
*-- Get the shipment lines from POSLN

*B608491,1 WAM 03/25/2008 Get the currency and exchange rates form the BOMLINE to calculate the landed cost in base currenct when receive by shipment
*lcSqlStatement  =  "SELECT  posln.*, POSHDR.cPriceCur, POSHDR.cDutyCur, POSHDR.Status "+;
"FROM POSLN posln (INDEX=POSLN) inner join POSHDR (INDEX=POSHDR) "+;
"ON POSHDR.cBusDocu = POSLN.cBusDocu AND POSHDR.cStyType = POSLN.cStyType"+;
" AND POSHDR.PO = POSLN.PO "+;
"WHERE POSLN.cBusDocu = '" + loFormSet.lcBusDoc +;
"' AND POSLN.cStyType = '" + loFormSet.lcWorkOrd +;
"' AND POSLN.ShipNo = '" + lcShipNo + "' AND TranCd ='" + lcTranCd + "'"

lcSqlStatement  =  "SELECT  posln.*, POSHDR.cPriceCur, POSHDR.cDutyCur, PosHdr.nPriceRat, PosHdr.nCurrUnit, PosHdr.nDutyRat, PosHdr.nDCurUnit, POSHDR.Status "+;
  "FROM POSLN posln (INDEX=POSLN) inner join POSHDR (INDEX=POSHDR) "+;
  "ON POSHDR.cBusDocu = POSLN.cBusDocu AND POSHDR.cStyType = POSLN.cStyType"+;
  " AND POSHDR.PO = POSLN.PO "+;
  "WHERE POSLN.cBusDocu = '" + loFormSet.lcBusDoc +;
  "' AND POSLN.cStyType = '" + loFormSet.lcWorkOrd +;
  "' AND POSLN.ShipNo = '" + lcShipNo + "' AND TranCd ='" + lcTranCd + "'"
*B608491,1 WAM 03/25/2008 (End)

=lfOpenSql(lcSqlStatement,'POSLN',loFormSet.lcPosLn, "","",.F.)
DIMENSION laIndex[1,2]
laIndex = ''
laIndex[1,1] = 'cBusDocu+cStyType+PO+cInvType+Style+STR(LineNo,6)+TranCd'
laIndex[1,2] = 'POSLN'
=lfSetIndex(loFormSet.lcPosLn,@laIndex)

*! C201211,1 HES 01/20/2010 Handle Scanned Barcodes from Recieve PO Screen -By Shipment- [Start]
IF ASCAN(loformset.laEvntTrig,PADR('SCANBARCD',10),1,ALEN(loformset.laEvntTrig,1),1) > 0
  IF loFormSet.mDoTrigger(PADR('SCANBARCD' ,10))
    RETURN
  ENDIF
ENDIF
*! C201211,1 HES 01/20/2010 Handle Scanned Barcodes from Recieve PO Screen -By Shipment- [End]

SELECT (loFormSet.lcPosLn)
LOCATE
IF EOF()
  IF loFormSet.lcPType = 'C'
    = gfModalGen('TRM34205B42000','DIALOG')
  ELSE
    *--The shipment lines have not been found! unable to proceed.
    = gfModalGen('TRM34077B42000','DIALOG')
  ENDIF
  loFormSet.AriaForm1.cntShipment.kbShipNo.keytextbox.VALUE = ""
  RETURN .F.
ENDIF
*B608491,1 WAM 03/25/2008 Get the currency and exchange rates form the BOMLINE to calculate the landed cost in base currenct when receive by shipment
=lfGetBomLn(loFormSet,'P', '', lcShipNo )
*B608491,1 WAM 03/25/2008 (End)

llShpPO = .F.
llRet = lfGetInfo(loFormSet,.F.)

RETURN llRet


*!*************************************************************
*! Name    : lfvBatch
*! Developer: Timour A. K.
*! Date     : 10/10/97
*! Purpose : Validate Temp. Batch.
*!*************************************************************
FUNCTION lfvBatch
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
*!*  LPARAMETERS loFormSet
*!*  WAIT WINDOW "Still under development"
*!*  RETURN .F.
*!*  *E301077,11 MAB Open Required Batch files [Begin]
*!*  =lfOpn_Rest(gcDatadir,'CTKTRCVH','CTKTRCVH')
*!*  =lfOpn_Rest(gcDatadir,'CTKTRCVL','CTKTRCVL')
LPARAMETERS loFormSet,llBrowse
lcBatch = loFormSet.AriaForm1.CntBatch.KBBatchNo.KeyTextBox.VALUE
lcPType = loFormSet.lcPType
IF !USED('CTKTRCVH')
  =gfOpenTable('CTKTRCVH','CTKTRCVH')
ENDIF
IF !USED('CTKTRCVL')
  =gfOpenTable('CTKTRCVL','CTKTRCVL')
ENDIF
IF !USED('POSHDR')
  =gfOpenTable('POSHDR','POSHDR')
ENDIF
IF !USED('POSLN')
  =gfOpenTable('POSLN','POSLN')
ENDIF
*E301077,11 MAB Open Required Batch files [End  ]
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
SELECT CTKTRCVH

*E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
*IF !SEEK(IIF(llMfCall,'M','I')+lcBatch)

*C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
*IF !SEEK(IIF(llMfCall,'M',IIF(lcPType='L','N','I'))+lcBatch)
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
*!*  IF !SEEK(IIF(llMfCall,'M',IIF(lcPType$'LH','N','I'))+lcBatch)
*!*    *SET FILTER TO cType=IIF(llMfCall,'M','I')
*!*    *SET FILTER TO cType=IIF(llMfCall,'M',IIF(lcPType='L','N','I'))
*!*    SET FILTER TO cType=IIF(llMfCall,'M',IIF(lcPType$'LH','N','I'))
IF !gfSEEK(IIF(loFormSet.llMfCall,'M',IIF(loFormSet.lcPType $ 'LHO','N','I'))+lcBatch) OR llBrowse
  =gfSeek(IIF(loFormSet.llMfCall,'M',IIF(loFormSet.lcPType = 'O','N','I')))
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  *C200170,1 AMH [End]

  *E301480,1 NAD (End)
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
  *!*    lcBrFields = [TmpRcvNum :8:H='Recv. #',]+;
  *!*                 [cStatus:1:H="S",]+;
  *!*                 [cDesc:24:H="Description",]+;
  *!*                 [dDate:10:H="Date",]+;
  *!*                 [nTotStk:8:H="Tot.Stk",]+;
  *!*                 [nTotDam:8:H="Tot.Oth",]+;
  *!*                 [nTotCan:8:H="Tot.Can"]
  lcBrFields = [TmpRcvNum :8:H=LANG_POSTREC_BATCHBROWSE_REC,]+;
    [cStatus:10:H=LANG_POSTREC_BATCHBROWSE_REC_STATUS,]+;
    [cDesc:24:H=LANG_POSTREC_BATCHBROWSE_REC_DESC,]+;
    [dDate:10:H=LANG_POSTREC_BATCHBROWSE_REC_DATE    ,]+;
    [nTotStk:8:H=LANG_POSTREC_BATCHBROWSE_TOTSTK     ,]+;
    [nTotDam:8:H=LANG_POSTREC_BATCHBROWSE_REC_TOTOTH  ,]+;
    [nTotCan:8:H=LANG_POSTREC_BATCHBROWSE_REC_TOTCAN  ]
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  DIME laTempData[1]
  STORE '' TO laTempData
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
  *=gfBrows(.F.,'TMPRCVNUM','laTempData','Temp. Receive Batchs')
  =gfBrows(.F.,'TMPRCVNUM','laTempData',LANG_POSTREC_BATCHBROWSE_TITLE)
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  lcBatch=laTempData[1]
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
  *SET FILTER TO
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
ENDIF
IF EMPTY(lcBatch) OR EOF('CTKTRCVH')
  RETURN .F.
ENDIF
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
*SHOW GET lcBatch
loFormSet.AriaForm1.CntBatch.KBBatchNo.KeyTextBox.VALUE =lcBatch
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]

*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
*--Batch Validation.
*!*  IF CTKTRCVH.cStatus = 'P'
*!*    *--This temporary receiving batch is posted. Cannot proceed.
*!*    = gfModalGen('TRM34070B42000','DIALOG','is posted')
*!*    RETURN .F.
*!*  ENDIF
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
*C200170,1 AMH Add case of issued batch for issue inter-location P/O Batch [Start]
IF lcPType = 'H' .AND. CTKTRCVH.cStatus = 'I'
  *--This temporary receiving batch is posted. Cannot proceed.
  = gfModalGen('TRM34070B42000','DIALOG','is issued')
  RETURN .F.
ENDIF
*C200170,1 AMH Add case of issue inter-location P/O Batch [Start]

*C200170,1 AMH remove case of receive inter-location P/O Batch [Start]
*IF CTKTRCVH.cStatus <> 'A'
IF CTKTRCVH.cStatus <> 'A' .AND. lcPType <> 'L'
  *C200170,1 AMH [End]

  *--This temporary receiving batch is not approved. Cannot proceed.
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
  *!*    = gfModalGen('TRM34070B42000','DIALOG','is not approved')
  *!*    RETURN .F.
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
ENDIF

*C200170,1 AMH Check if batch is issued in case of receive inter-location P/O Batch [Start]
IF lcPType = 'L' .AND. CTKTRCVH.cStatus <> 'I'

  *--This temporary receiving batch is not issued. Cannot proceed.
  = gfModalGen('TRM34070B42000','DIALOG','is not issued')
  RETURN .F.
ENDIF
*C200170,1 AMH [End]

SELECT CtKtRcvL

*E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
*IF !SEEK(IIF(llMfCall,'M','I')+lcBatch)
*C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
*IF !SEEK(IIF(llMfCall,'M',IIF(lcPtype='L','N','I')+lcBatch))
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
*IF !SEEK(IIF(llMfCall,'M',IIF(lcPtype$'LH','N','I')+lcBatch))
IF !gfSeek(IIF(loFormSet.llMfCall,'M',IIF(lcPType = 'O','N','I'))+lcBatch)
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  *C200170,1 AMH [End]

  *E301480,1 NAD (End)

  *--This temporary receiving batch has no lines. Cannot proceed.
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
  *= gfModalGen('TRM34070B42000','DIALOG','has no lines')
  = gfModalGen('TRM34070B42000','DIALOG',LANG_POSTREC_BATCHHASNOLINES)
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  RETURN .F.
ENDIF
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
*=lfGetInfo(loFormSet,.F.)
=lfGetInfo(loFormSet,.F.,IIF(loFormSet.lcPType $ 'OIM',.T.,.F.))
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
*C200170,4 AMH Add case of issue inter-location P/O Batch [Start]
IF lcPType = 'H'
  lcSource = CTKTRCVH.VENDOR
ENDIF
*C200170,4 AMH [End]

RETURN .T.

*!*************************************************************
*! Name      : lfGetInfo
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To get the lines of the selcted transaction
*!*************************************************************
*! Parameters: loFormSet : FormSet, llEmpWare:
*!*************************************************************
FUNCTION lfGetInfo
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
*PARAMETERS loFormSet,llEmpWare
PARAMETERS loFormSet,llEmpWare,llBatch
lcBatch = loFormSet.AriaForm1.CntBatch.KBBatchNo.KeyTextBox.VALUE
lcOthrTrCd = '4'
lcCanlTrCd = '5'
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
LOCAL lnAlias, llMatExist,lcSqlStatement, lcMsg1,lcMsg2,lcMsg3, lcMsg4
PRIVATE lnCrRt1, lnCrRt2
STORE 1 TO lnCrRt1, lnCrRt2
lnAlias = SELECT()
DO CASE

  ****************************************************************
  *-- 'S' Style PO Shipment, 'U' Issue Inter-Location PO Shipment
  *-- 'C' Receive Inter-Location PO Shipment
  *-- 'F' Receive Material PO Shipment
  ****************************************************************
CASE loFormSet.lcPType $ 'SUCF' AND !llShpPO

  *: E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
  IF loFormSet.lcPType $ 'S'
    =gfOpenTable('SHPRLFLD','SHPRLFLD','SH')
  ENDIF
  *: E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]

  *-Header info.
  WITH loFormSet.AriaForm1.cntShipment
    .dtpickerShpEntered.VALUE = EVALUATE(loFormSet.lcMastShp+'.Entered')
    .dtpickerShpETA.VALUE     = EVALUATE(loFormSet.lcMastShp+'.Eta')
    .txtShpCartons.VALUE      = EVALUATE(loFormSet.lcMastShp+'.Cartons')
    .txtShpAirWay.VALUE       = EVALUATE(loFormSet.lcMastShp+'.AirWayB')
    .txtShpReference.VALUE    = EVALUATE(loFormSet.lcMastShp+'.Reference')
  ENDWITH

  DIME laZero[9]
  laZero = 0

  *-Line info.
  lcTPO = SPACE(6)

  SELECT (loFormSet.lcPosLn)
  SCAN
    IF EMPTY(STATUS)
      LOOP
    ENDIF
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
    *WAIT WINDOW 'Shipping '+ IIF(loFormSet.lcPType = 'F', 'material: ','style : ')+ Style NOWAIT
    IF !loFormSet.llSilentMod
      WAIT WINDOW 'Shipping '+ IIF(loFormSet.lcPType = 'F', 'material: ','style : ')+ STYLE NOWAIT
    ENDIF
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

    IF !INLIST(STATUS,'O','A')
      IF PO = lcTPO
        LOOP
      ENDIF
      IF STATUS = 'H'
        *--The P/O &lcTPO status is Hold since a P/O cost sheet has not
        *--been created yet. Therefore,no receivings can be done for
        *--this P/O. It will be skipped for the shipment.
        *=gfModalGen('TRM34073B42000','DIALOG',PO)

        lcMsg1  = IIF(loFormSet.lcPType $ 'UC', 'Inter-Location P/O ','P/O ')+PO
        lcMsg2  = IIF(loFormSet.lcPType $ 'UC', 'an Inter-Location P/O','a P/O')
        lcMsg3  = IIF(loFormSet.lcPType $ 'UC', 'Inter-Location P/O','P/O')
        lcMsg4  = IIF(loFormSet.lcPType = 'U','issued','received')
        *-- ð status is On Hold since ð cost sheet has not yet been created.
        *-- This ð cannot be received and will be excluded from the shipment.
        =gfModalGen('TRM34206B42000','DIALOG',lcMsg1+'|'+lcMsg2+'|'+lcMsg3+'|'+lcMsg4)
      ENDIF

      IF STATUS = 'S'
        *--The P/O &lcTPO status is Closed, the P/O will be skipped for the shipment.
        *=gfModalGen('TRM34074B42000','DIALOG',PO)
        lcMsg1  = IIF(loFormSet.lcPType $ 'UC', 'Inter-Location P/O ','P/O ')+PO
        =gfModalGen('TRM34207B42000','DIALOG',lcMsg1)
      ENDIF
      lcTPO = PO
      LOOP
    ENDIF

    IF loFormSet.llImpCost AND loFormSet.lcWorkOrd <> 'M' && Material PO
      IF  PO <> lcTpo
        llMatExist = .F.
        lcCstShtType = IIF(loFormSet.lcWorkOrd $ 'UC','N','I')
        *-- Check if there a work order cost sheet and there is a material,
        *-- style component or trim and inventory maintained.
        lcSqlStatement = "SELECT cImTyp, CutTkt FROM CTKTBOM [INDEX=CTKTBOM] "+;
          " WHERE cImTyp = '" + lcCstShtType + "' AND "+;
          "CutTkt ='" + PO +;
          "' AND (cCatgTyp = 'F' OR cCatgTyp = 'S' OR "+;
          "(cCatgTyp = 'T' AND Trim_Invt=1))"

        =lfWOrdBOM(lcSqlStatement,.T.,.F.,.F.,.F.,loFormSet)
        IF USED(loFormSet.lcCTktBom)
          SELECT (loFormSet.lcCTktBom)
          LOCATE
          llMatExist = !EOF()
        ENDIF

        SELECT (loFormSet.lcPosLn)
        IF llMatExist
          *-- Check if there an issued item.
          lcSqlStatement = "SELECT cImTyp, cTktNo FROM BOMCOST [INDEX=POBOMCLS] "+;
            " WHERE cImTyp = '" + lcCstShtType + "' AND "+;
            "cTktNo ='" + PO + "'"
          =lfWOrdBOM(lcSqlStatement,.F.,.T.,.F.,.F.,loFormSet)

          IF USED(loFormSet.lcBomCost)
            SELECT(loFormSet.lcBomCost)
            LOCATE
            *--No cost items have been applied against this P/o. Are you sure you want to
            *-- receive ?
            IF EOF() AND gfModalGen('QRM34071B42002','DIALOG') = 2
              LOOP
            ENDIF
          ENDIF
        ENDIF
        SELECT (loFormSet.lcPosLn)
        lcTPO = PO
      ENDIF
    ENDIF  && End if detail importing.
    STORE 1 TO lnCrRt1, lnCrRt2
    SELECT (loFormSet.lcPosLn)
    IF loFormSet.llMulCurr
      loFormSet.lcCur1 = cPriceCur
      lnCrRt1 = IIF(cPriceCur = oAriaApplication.BaseCurrency, 1,;
        gfChkRate('loFormSet.lnCurrUnt1',cPriceCur,;
        loFormSet.AriaForm1.dtpickerReceivingDate.VALUE,loFormSet.llEditExRt,;
        oAriaApplication.ActiveCompanyId,.F.))

      loFormSet.lcCur2 = cDutyCur
      lnCrRt2 = IIF(cDutyCur = oAriaApplication.BaseCurrency, 1,;
        gfChkRate('loFormSet.lnCurrUnt2',cDutyCur,;
        loFormSet.AriaForm1.dtpickerReceivingDate.VALUE,loFormSet.llEditExRt,;
        oAriaApplication.ActiveCompanyId,.F.))
    ENDIF

    IF lnCrRt1 = 0 OR lnCrRt2 = 0
      IF !loFormSet.llEditExRt
        *--This line has currency with zero rate, it will be ignored.'
        = gfModalGen('TRM34079B42000','DIALOG')
        LOOP
      ELSE
        STORE 1 TO lnCrRt1,lnCrRt2
      ENDIF
    ENDIF

    *-- Get the style information
    IF loFormSet.lcInvType = "0001"
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
*!*	      lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = '" +;
*!*	        EVALUATE(loFormSet.lcPosLn+'.Style') + "'"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
*!*	      lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = [" +;
*!*	        EVALUATE(loFormSet.lcPosLn+'.Style') + "]"
      lcStyleSleV =EVALUATE(loFormSet.lcPosLn+'.Style') 
      lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = ?m.lcStyleSleV"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]   
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]  
    ELSE
      *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
      *lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loFormSet.lcInvType +;
      "' AND Style = '" + EVALUATE(loFormSet.lcPosLn+'.Style') + "'"
      PRIVATE lcItemValue
      lcItemValue = EVALUATE(loFormSet.lcPosLn+'.Style')
      lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loFormSet.lcInvType +;
        "' AND Style = ?m.lcItemValue "
      *B607658,1 KHM 07/07/2005 [End]
    ENDIF
    =lfGetItmInf(loFormSet.lcInvType,;
      IIF(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+;
      EVALUATE(loFormSet.lcPosLn+'.Style'),;
      loFormSet.lcTmpItem,;
      IIF(loFormSet.lcInvType = "0001",'STYLE','ITEM'),lcSqlStatement,;
      loFormSet.lcInvType = "0002")

    IF SEEK(IIF(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+;
        EVALUATE(loFormSet.lcPosLn+'.Style'),loFormSet.lcTmpItem)
      m.cStyDesc = EVALUATE(loFormSet.lcTmpItem+'.Desc1')
      m.cDye_Flg = EVALUATE(loFormSet.lcTmpItem+'.cDye_Flg')
    ENDIF

    SELECT (loFormSet.lcPosLn)
    SCATTER MEMVAR
    SCATTER FIELDS nFCost1,nFCost2,nFCost3,nFCost4,nFCost5,nFCost6,nFCost7 TO laEstiCost

    *B608718,1 WAM 10/09/2008 Store budget quantity
    SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,Totqty TO laEstQty
    *B608718,1 WAM 10/09/2008 (End)

    *B126833,1 WAM 04/03/2005 add new button to edit landed cost for styles has no detail costing
    m.lDetCost = EVALUATE(loFormSet.lcTmpItem+'.lDetCost')
    *B126833,1 WAM 04/03/2005 (End)

    SELECT (loFormSet.lcTmpLine)

    *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [Start]
    IF ASCAN(loformset.laEvntTrig,PADR('MNGFNDL1',10),1,ALEN(loformset.laEvntTrig,1),1) > 0
      loFormSet.mDoTrigger(PADR('MNGFNDL1' ,10))
    ELSE
      *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [End]

      APPEND BLANK

      *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [Start]
    ENDIF
    *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [End]

    GATHER MEMVAR
    REPLACE TranCd    WITH '1',;
      ShipNo    WITH loFormSet.AriaForm1.cntShipment.kbShipNo.keytextbox.VALUE ,;
      nLanPrRat WITH lnCrRt1,;
      nLanDuRat WITH lnCrRt2,;
      TotStk    WITH TotQty,;
      TotDam    WITH 0,;
      TotCan    WITH 0,;
      TotBal    WITH 0

    GATHER FROM laEstiCost FIELDS nFLanCost1,nFLanCost2,nFLanCost3,nFLanCost4,;
      nFLanCost5,nFLanCost6,nFLanCost7

    *: E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
    IF loFormSet.lcPType $ 'S' AND USED('SHPRLFLD') .AND. gfGetMemVar('M_APRVSHIP')
      IF gfSEEK(EVALUATE(loFormSet.lcPosLn+'.SHIPNO')+EVALUATE(loFormSet.lcPosLn+'.PO')+STR(EVALUATE(loFormSet.lcPosLn+'.LINENO'),6),'SHPRLFLD')

        REPLACE nLanPrRat WITH SHPRLFLD.NPRICERAT
        REPLACE NLANDURAT WITH SHPRLFLD.NDUTYRAT

        *[Start] At GPS the duty rate is ALWAYS by the base currency.
        IF ASCAN(loFormSet.laEvntTrig,PADR('SETDTYRT',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
          loFormSet.mDoTrigger(PADR('SETDTYRT',10))
        ENDIF
        *[End]

      ENDIF
    ENDIF
    *: E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]
    *B608491,1 WAM 03/25/2008 Get the currency and exchange rates form the BOMLINE to calculate the landed cost in base currenct when receive by shipment
    GATHER FROM laEstiCost FIELDS nFCost1,nFCost2,nFCost3,nFCost4,nFCost5,nFCost6,nFCost7
    *! B610107,1 MMT 10/04/2012 Add Foreign currency costing to Material MFG order summary[Start]
    *IF loFormSet.lcBusDoc+loFormSet.lcWorkOrd $ 'PP'
    IF loFormSet.lcBusDoc+loFormSet.lcWorkOrd $ 'PP|PF'
      *! B610107,1 MMT 10/04/2012 Add Foreign currency costing to Material MFG order summary[End]
      lcPosLn = loFormSet.lcPosLn

      *B608718,1 WAM 10/09/2008 Store budget quantity
      *=lfBaseCost(loFormSet, &lcPosLn..cStyType, &lcPosLn..PO, &lcPosLn..cPriceCur, &lcPosLn..nPriceRat, &lcPosLn..nCurrUnit, &lcPosLn..cDutyCur, &lcPosLn..nDutyRat, &lcPosLn..nDCurUnit)
      =lfBaseCost(loFormSet, &lcPosLn..cStyType, &lcPosLn..PO, &lcPosLn..cPriceCur, &lcPosLn..nPriceRat, &lcPosLn..nCurrUnit, &lcPosLn..cDutyCur, &lcPosLn..nDutyRat, &lcPosLn..nDCurUnit,@laEstQty)
      *B608718,1 WAM 10/09/2008 (End)

    ELSE
      *B608491,1 WAM 03/25/2008 (End)

      IF loFormSet.llMulCurr
        =lfGetEqv('1234567',nLanPrRat,nLanDuRat,loFormSet.lnCurrUnt1,loFormSet.lnCurrUnt2,;
          nFLanCost1,nFLanCost2,nFLanCost3,nFLanCost4,nFLanCost5,nFLanCost6,;
          nFLanCost7,loFormSet)
      ELSE
        =ACOPY(laEstiCost,loFormSet.laECost)
      ENDIF
      *B608491,1 WAM 03/25/2008 Get the currency and exchange rates form the BOMLINE to calculate the landed cost in base currenct when receive by shipment
    ENDIF
    *: C200919,1 MMT 01/27/2008 Add trig. to PO price ex. Rate while Receiving[Start]
    IF cPriceCur <> oAriaApplication.BaseCurrency AND ASCAN(loFormSet.laEvntTrig ,PADR('GTPOEXRAT',10))<>0
      loFormSet.mDoTrigger(PADR('GTPOEXRAT' ,10))
    ENDIF
    SELECT (loFormSet.lcTmpLine)
    *: C200919,1 MMT 01/27/2008 Add trig. to PO price ex. Rate while Receiving[End]

    GATHER FROM loFormSet.laECost FIELDS nICost1,nICost2,nICost3,nICost4,nICost5,nICost6,nICost7
    REPLACE nLanPrRat WITH lnCrRt1
    *B608491,1 WAM 03/25/2008 (End)

    GATHER FROM loFormSet.laECost FIELDS nLan_Cost1,nLan_Cost2,nLan_Cost3,nLan_Cost4,nLan_Cost5,;
      nLan_Cost6,nLan_Cost7

    *B608760,1 WAM 12/04/2008 Get foreign cost from the adjusted record for receiving
    GATHER FROM laEstiCost FIELDS nFLanCost1,nFLanCost2,nFLanCost3,nFLanCost4,nFLanCost5,nFLanCost6,nFLanCost7
    *B608760,1 WAM 12/04/2008 (End)

    *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [Start]
    IF ASCAN(loformset.laEvntTrig,PADR('MNGFNDL2',10),1,ALEN(loformset.laEvntTrig,1),1) > 0
      IF !loFormSet.mDoTrigger(PADR('MNGFNDL2' ,10))
        RETURN
      ENDIF
    ELSE
      *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [End]

      loFormSet.lnTotStk = loFormSet.lnTotStk + TotStk
      SCATTER MEMVAR
      APPEND BLANK
      GATHER MEMVAR

      REPLACE TranCd WITH IIF(loFormSet.lcWorkOrd = 'U','6','2')
      GATHER FROM laZero FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd

      *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [Start]
    ENDIF
    *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [End]

  ENDSCAN
  SELECT(loFormSet.lcMastShp)
  IF loFormSet.lcPType <> 'U'
    *B128070,1 KHM 05/19/2005 Commented out. There is no need to update the table here [Begin]
    *REPLACE Recv_Stk  WITH Recv_Stk + loFormSet.lnTotStk
    *B128070,1 KHM 05/19/2005 [End]
  ENDIF

  WAIT CLEAR
  SELECT (loFormSet.lcTmpLine)
  LOCATE

  ************************************
  *-- F R O M  P O  B A T C H. --*
  ************************************
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  *CASE loFormSet.lcPType $ 'BTLH'
CASE loFormSet.lcPType $ 'BTLHOIM' AND IIF(loFormSet.lcPType $ 'MOI',!EMPTY(lcBatch) AND llBatch,.F.)
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  *!*      llByCarton = CTKTRCVH.Carton
  *!*      lcBDesc    = CTKTRCVH.cDesc
  *!*      lcBStatus  = IIF(lcPType='L','ISSUED','APPROVED')
  *!*      ldBDate    = CTKTRCVH.dDate
  *!*      lnTotStk   = CTKTRCVH.nTotStk
  *!*      lnTotDam   = CTKTRCVH.nTotDam
  *!*      lnTotCan   = CTKTRCVH.nTotCan
  loFormSet.AriaForm1.CntBatch.ENABLED =.T.
  loFormSet.AriaForm1.txtStock.VALUE = CTKTRCVH.nTotStk
  loFormSet.AriaForm1.txtOthers.VALUE = CTKTRCVH.nTotDam
  loFormSet.AriaForm1.txtCancel.VALUE =  CTKTRCVH.nTotCan
  loFormSet.AriaForm1.CntBatch.DtpickerBatchDate.VALUE = CTKTRCVH.dDate
  loFormSet.AriaForm1.CntBatch.DtpickerBatchDate.ENABLED =!(CTKTRCVH.cStatus $ 'XP')
  loFormSet.AriaForm1.CntBatch.txtBatchDesc.VALUE = CTKTRCVH.cDesc
  loFormSet.AriaForm1.CntBatch.txtBatchDesc.ENABLED = !(CTKTRCVH.cStatus $ 'XP')
  loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE = CTKTRCVH.cStatus
  IF (CTKTRCVH.cStatus <> 'A' AND loFormSet.llApproveBatch) AND !(CTKTRCVH.cStatus $ 'XP')
    loFormSet.AriaForm1.CntBatch.cboBatchStatus.ENABLED =.T.
  ELSE
    loFormSet.AriaForm1.CntBatch.cboBatchStatus.ENABLED =.F.
  ENDIF
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  *--Line info.
  lcKey=' '

  SELECT CTKTRCVL

  *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
  *= SEEK(IIF(llMfCall,'M','I')+lcBatch)
  *IF lcPType = 'B'
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  *= SEEK(IIF(llMfCall,'M',IIF(lcPType='B','I','N'))+lcBatch)
  = gfSEEK(IIF(loFormSet.llMfCall,'M',IIF(loFormSet.lcPType$'BI','I','N'))+lcBatch)
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]

  *C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
  *IF lcPType $ 'BL'
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  *IF lcPType $ 'BLH'
  IF lcPType $ 'BLHIOM'
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
    *C200170,1 AMH Add case of issue inter-location P/O Batch [Start]

    PRIVATE lcTypScan,lcTypSeek
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
    *!*      lcTypScan=IIF(lcPType='B','I','N')
    *!*      lcTypSeek=IIF(lcPType='B','P','N')
    IF loFormSet.llMfCall
      lcTypScan='M'
      lcTypSeek='PU'
    ELSE
      lcTypScan=IIF(lcPType$'BI','I','N')
      lcTypSeek=IIF(lcPType$'BI','PP','NN')
    ENDIF
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
    *E301480,1 NAD (End)

    *E300935,4 Change Scan While to scan rest while because it's faster.
    *SCAN WHILE cType+TmpRcvNum = 'I'+lcBatch

    *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
    *SCAN REST WHILE cType+TmpRcvNum = 'I'+lcBatch
    *=SEEK('P'+Cuttkt,'POSHDR')

    *C200170,1 AMH intialize lnTotStk in case of issue inter-location P/O Batch [Start]
    IF lcPType='H'
      lnTotStk   = 0
    ENDIF
    *C200170,1 AMH [End]
    SCAN REST WHILE cType+TmpRcvNum = lcTypScan+lcBatch
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
      *=SEEK(lcTypSeek+Cuttkt,'POSHDR')
      =gfSEEK(lcTypSeek+Cuttkt,'POSHDR')
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
      *E301480,1 NAD (End)

      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
      *!*          IF llMulCurr
      *!*            lcCur1  = POSHDR.cPriceCur
      *!*            lnCrRt1 = IIF(lcCur1=gcBaseCurr,1,gfChkRate('lnCurrUnt1',lcCur1,ldRcvDate,llEditExRt,gcAct_comp,.F.))
      *!*            lcCur2  = POSHDR.cDutyCur
      *!*            lnCrRt2 = IIF(lcCur2=gcBaseCurr,1,gfChkRate('lnCurrUnt2',lcCur2,ldRcvDate,llEditExRt,gcAct_comp,.F.))
      *!*          ENDIF
      *!*          *--Case of batch do not accept saving the batch if there is 0 rates.
      *!*          IF lnCrRt1=0 OR lnCrRt2=0
      *!*            IF !llEditExRt
      *!*              *--One or more lines has currency with zero rate, Unable to proceed.'
      *!*              = gfModalGen('TRM34080B42000','DIALOG')
      *!*              STORE ' ' TO lcPo,lcStyle,lcCur1,lcCur2
      *!*              SHOW GETS
      *!*              RETURN
      *!*            ELSE
      *!*              STORE 1 TO lnCrRt1,lnCrRt2
      *!*            ENDIF
      *!*          ENDIF
      *!*
      *!*          SELECT POSLN
      *!*
      *!*          *E301480,1 NAD (Start) Add the Inter location Po batch to the Expression.
      *!*          *= SEEK('P'+CTKTRCVL.Cuttkt+CTKTRCVL.Style)
      *!*          = SEEK(lcTypSeek+CTKTRCVL.Cuttkt+CTKTRCVL.Style)
      *!*          *E301480,1 NAD (End)
      *!*
      *!*          SCATTER FIELDS nCost1,nCost2,nCost3,nCost4,nCost5 TO laEstiCost
      *!*          SCATTER FIELDS nECost1,nECost2,nECost3,nECost4,nECost5 TO laEstECost
      *!*          SCATTER FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd TO laAlo
      *!*          =lfGetEqv('12345',lnCrRt1,lnCrRt2,lnCurrUnt1,lnCurrUnt2,;
      *!*                    laEstiCost[1],laEstiCost[2],laEstiCost[3],laEstiCost[4],laEstiCost[5])

      *!*          SELECT CTKTRCVL
      *!*          =SEEK(Style,'STYLE')

      *!*          *E300935,4 Add dyelot field in P/O by batch case.  [begin]
      *!*          *SCATTER FIELDS cCarton,Lineno,Style,cWareCode,Reference,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,cStyGrade,nLineNo TO laFields
      *!*          *B603100,1 SSH 22/05/2000 Fix the bug of incoret recieved cost in case of
      *!*          *B603100,1 SSH 22/05/2000 recieve by batch.
      *!*          *SCATTER FIELDS cCarton,Lineno,Style,Dyelot,cWareCode,Reference,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,cStyGrade,nLineNo TO laFields
      *!*          SCATTER FIELDS cCarton,Lineno,Style,Dyelot,cWareCode,Reference,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,Style.cStyGrade,nLineNo TO laFields
      *!*          *B603100,1 SSH 22/05/2000 [End]
      *!*          *E300935,4 Add dyelot field in P/O by batch case.  [end]

      *!*          *E300935,4
      *!*          *E300935,4 Add nLineNo To expression Expression and use expression arrays.
      *!*          *IF cType+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+IIF(llByCarton,STR(LineNo,6),'') <> lcKey
      *!*          IF cuttkt+style+dyelot+ccarton+STR(nLineNo,6)+STR(lineno,6) <> lcKey
      *!*            SELECT (lcTmpLine)
      *!*            APPEND BLANK
      *!*
      *!*            *E301480,1 NAD (Start) cosider the case of  Inter location Po batch
      *!*            *REPLACE cStyType  WITH 'P',;
      *!*                    PO        WITH CTKTRCVL.Cuttkt,;
      *!*                    Vendor    WITH POSHDR.Vendor,;
      *!*                    Scale     WITH STYLE.Scale,;
      *!*                    nLanPrRat WITH lnCrRt1,;
      *!*                    nLanDuRat WITH lnCrRt2,;
      *!*                    TranCd    WITH '1'
      *!*
      *!*             REPLACE cStyType  WITH lcTypSeek,;
      *!*                     PO        WITH CTKTRCVL.Cuttkt,;
      *!*                     Vendor    WITH POSHDR.Vendor,;
      *!*                     Scale     WITH STYLE.Scale,;
      *!*                     nLanPrRat WITH lnCrRt1,;
      *!*                     nLanDuRat WITH lnCrRt2,;
      *!*                     TranCd    WITH '1'

      *!*            IF lcPType='L'
      *!*              REPLACE  TotStk WITH CTKTRCVL.TotQty
      *!*            ENDIF
      *!*            *E301480,1 NAD (End)
      *!*
      *!*            *E300935,4 Add dyelot field in P/O by batch case.  [begin]
      *!*            *GATHER FROM laFields FIELDS cCarton,Lineno,Style,cWareCode,Reference,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,cStyGrade,nLineNo
      *!*            GATHER FROM laFields FIELDS cCarton,Lineno,Style,Dyelot,cWareCode,Reference,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,cStyGrade,nLineNo
      *!*            *E300935,4 Add dyelot field in P/O by batch case.  [end]

      *!*            *C200170,1 AMH Add Get TotStk & Tot Bal in case of issue inter-location P/O Batch [Start]
      *!*            IF lcPType='H'
      *!*              REPLACE  TotStk WITH CTKTRCVL.TotQty;
      *!*                       TotDam WITH 0;
      *!*                       TotCan WITH 0;
      *!*                       TotBal WITH TotQty-TotStk;
      *!*                       Lineno WITH nLineNo
      *!*              lnTotStk   = lnTotStk + TotStk
      *!*            ENDIF
      *!*            *C200170,1 AMH [End]

      *!*            GATHER FROM laAlo      FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
      *!*            GATHER FROM laEstiCost FIELDS nCost1,nCost2,nCost3,nCost4,nCost5
      *!*            GATHER FROM laEstiCost FIELDS nLan_CST1,nLan_CST2,nLan_CST3,nLan_CST4,nLan_CST5
      *!*            GATHER FROM laEstECost FIELDS nECost1,nECost2,nECost3,nECost4,nECost5
      *!*            GATHER FROM laECost    FIELDS nELanCost1,nELanCost2,nELanCost3,nELanCost4,nELanCost5
      *!*            SELECT CTKTRCVL
      *!*
      *!*            *E300935,4 Change lcKey expression.
      *!*            *lcKey = 'I'+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+IIF(llByCarton,STR(LineNo,6),'')
      *!*            lcKey = cuttkt+style+dyelot+ccarton+STR(nLineNo,6)+STR(lineno,6)

      *!*          ENDIF
      *!*
      *!*          SELECT (lcTmpLine)
      *!*          APPEND BLANK
      *!*
      *!*          *E301480,1 NAD (Start) cosider the case of Inter location Po batch
      *!*          *REPLACE cStyType  WITH 'P',;
      *!*                  PO        WITH CTKTRCVL.Cuttkt,;
      *!*                  Vendor    WITH POSHDR.Vendor,;
      *!*                  Scale     WITH STYLE.Scale,;
      *!*                  nLanPrRat WITH lnCrRt1,;
      *!*                  nLanDuRat WITH lnCrRt2,;
      *!*                  TranCd    WITH CTKTRCVL.TranCd
      *!*
      *!*          REPLACE cStyType  WITH lcTypSeek,;
      *!*                  PO        WITH CTKTRCVL.Cuttkt,;
      *!*                  Vendor    WITH POSHDR.Vendor,;
      *!*                  Scale     WITH STYLE.Scale,;
      *!*                  nLanPrRat WITH lnCrRt1,;
      *!*                  nLanDuRat WITH lnCrRt2,;
      *!*                  TranCd    WITH CTKTRCVL.TranCd
      *!*
      *!*          *E301480,1 NAD (End)
      *!*
      *!*          *E300935,4 Add dyelot field in P/O by batch case.  [begin]
      *!*          *GATHER FROM laFields FIELDS cCarton,Lineno,Style,cWareCode,Reference,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,cStyGrade,nLineNo
      *!*          GATHER FROM laFields FIELDS cCarton,Lineno,Style,Dyelot,cWareCode,Reference,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,cStyGrade,nLineNo
      *!*          *E300935,4 Add dyelot field in P/O by batch case.  [end]

      *!*          *C200170,1 AMH Add Get TotStk & Tot Bal in case of issue inter-location P/O Batch [Start]
      *!*          IF lcPType='H'
      *!*            REPLACE  TotStk WITH CTKTRCVL.TotQty;
      *!*                     TotDam WITH 0;
      *!*                     TotCan WITH 0;
      *!*                     TotBal WITH TotQty-TotStk;
      *!*                     Lineno WITH nLineNo
      *!*          ENDIF
      *!*          *C200170,1 AMH [End]

      *!*          GATHER FROM laEstiCost FIELDS nCost1,nCost2,nCost3,nCost4,nCost5
      *!*          GATHER FROM laEstiCost FIELDS nLan_CST1,nLan_CST2,nLan_CST3,nLan_CST4,nLan_CST5
      *!*          GATHER FROM laEstECost FIELDS nECost1,nECost2,nECost3,nECost4,nECost5
      *!*          GATHER FROM laECost    FIELDS nELanCost1,nELanCost2,nELanCost3,nELanCost4,nELanCost5
      *!*        ENDSCAN
      STORE 1 TO lnCrRt1, lnCrRt2
      lcCstShtTyp = IIF(loFormSet.lcWorkOrd  = "U","M",;
        IIF(loFormSet.lcWorkOrd  = "P","I",loFormSet.lcWorkOrd))
      lcSqlStatement  =  "SELECT  POSLN.*, POSHDR.cPriceCur, POSHDR.cDutyCur, PosHdr.nPriceRat, PosHdr.nCurrUnit, PosHdr.nDutyRat, PosHdr.nDCurUnit, POSHDR.Status "+;
        "FROM POSLN posln (INDEX=POSLN) inner join POSHDR (INDEX=POSHDR) "+;
        "ON POSHDR.cBusDocu = POSLN.cBusDocu AND POSHDR.cStyType = POSLN.cStyType "+;
        "AND  POSHDR.PO = POSLN.PO "+;
        "WHERE POSLN.cBusDocu = '" + loFormSet.lcBusDoc +;
        "' AND POSLN.cStyType = '" + loFormSet.lcWorkOrd +;
        "' AND POSLN.PO = '" + CTKTRCVL.Cuttkt +;
        "' AND POSLN.cInvType='"+loFormSet.lcInvType + "'"

      =lfOpenSql(lcSqlStatement,'POSLN',loFormSet.lcPosLn, "","",.F.)
      DIMENSION laIndex[1,2]
      laIndex = ''
      laIndex[1,1] = 'cBusDocu+cStyType+PO+cInvType+Style+STR(LineNo,6)+TranCd'
      laIndex[1,2] = 'POSLN'
      =lfSetIndex(loFormSet.lcPosLn,@laIndex)

      lcSqlStatement = "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+;
        "WHERE cImTyp = '" + lcCstShtTyp +;
        "' AND cTktNo ='" + CTKTRCVL.Cuttkt + "'"
      =lfOpenSql(lcSqlStatement,'BOMLINE',loFormSet.lcMastBomLn, "","",.F.)
      DIMENSION laIndex[1,2]
      laIndex = ''
      laIndex[1,1] = 'cImTyp+cType+ShipNo+cTktNo+STR(LineNo,6)'
      laIndex[1,2] = loFormSet.lcMastBomLn
      =lfSetIndex(loFormSet.lcMastBomLn,@laIndex)

      SELECT(loFormSet.lcPosLn)
      =SEEK(lcTypSeek+CTKTRCVL.Cuttkt+loformset.lcInvtype+CTKTRCVL.STYLE)
      IF loFormSet.llMulCurr
        loFormSet.lcCur1 = cPriceCur
        lnCrRt1 = IIF(cPriceCur = oAriaApplication.BaseCurrency, 1,;
          gfChkRate('loFormSet.lnCurrUnt1',cPriceCur,;
          loFormSet.AriaForm1.dtpickerReceivingDate.VALUE,loFormSet.llEditExRt,;
          oAriaApplication.ActiveCompanyId,.F.))

        loFormSet.lcCur2 = cDutyCur
        lnCrRt2 = IIF(cDutyCur = oAriaApplication.BaseCurrency, 1,;
          gfChkRate('loFormSet.lnCurrUnt2',cDutyCur,;
          loFormSet.AriaForm1.dtpickerReceivingDate.VALUE,loFormSet.llEditExRt,;
          oAriaApplication.ActiveCompanyId,.F.))
      ENDIF
      IF lnCrRt1 = 0 OR lnCrRt2 = 0
        IF !loFormSet.llEditExRt
          *--This line has currency with zero rate, it will be ignored.'
          = gfModalGen('TRM34079B42000','DIALOG')
          LOOP
        ELSE
          STORE 1 TO lnCrRt1,lnCrRt2
        ENDIF
      ENDIF
      IF loFormSet.lcInvType = "0001"
        *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
*!*	        lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = '" +;
*!*	          EVALUATE(loFormSet.lcPosLn+'.Style') + "'"
        *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
*!*	        lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = [" +;
*!*	          EVALUATE(loFormSet.lcPosLn+'.Style') + "]"
        lcSelValSty = EVALUATE(loFormSet.lcPosLn+'.Style')
        lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = ?m.lcSelValSty"
        *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]  
        *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]  
      ELSE
        PRIVATE lcItemValue
        lcItemValue = POSLN.STYLE
        lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loFormSet.lcInvType +;
          "' AND Style = ?m.lcItemValue "
      ENDIF
      =lfGetItmInf(loFormSet.lcInvType,;
        IIF(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+;
        EVALUATE(loFormSet.lcPosLn+'.Style'),;
        loFormSet.lcTmpItem,;
        IIF(loFormSet.lcInvType = "0001",'STYLE','ITEM'),lcSqlStatement,;
        loFormSet.lcInvType = "0002")

      IF SEEK(IIF(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+;
          EVALUATE(loFormSet.lcPosLn+'.Style'),loFormSet.lcTmpItem)
        m.cStyDesc = EVALUATE(loFormSet.lcTmpItem+'.Desc1')
        m.cDye_Flg = EVALUATE(loFormSet.lcTmpItem+'.cDye_Flg')
      ENDIF

      SELECT(loFormSet.lcPosLn)
      SCATTER MEMVAR
      SCATTER FIELDS nFCost1,nFCost2,nFCost3,nFCost4,nFCost5,nFCost6,nFCost7 TO laEstiCost
      SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,Totqty TO laEstQty
      SCATTER FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd TO laAlo
      m.lDetCost = EVALUATE(loFormSet.lcTmpItem+'.lDetCost')
      lcTmpLine = loFormSet.lcTmpLine
      SELECT CTKTRCVL
      IF cuttkt+STYLE+dyelot+ccarton+STR(nLineNo,6)+STR(LINENO,6) <> lcKey
        SELECT (loFormSet.lcTmpLine)
        APPEND BLANK
        GATHER MEMVAR
        REPLACE TranCd    WITH '1',;
          nLanPrRat WITH lnCrRt1,;
          nLanDuRat WITH lnCrRt2,;
          TotStk    WITH CTKTRCVL.TotQty,;
          TotDam    WITH 0,;
          TotCan    WITH 0,;
          TotBal    WITH 0,;
          Qty1      WITH  CTKTRCVL.Qty1,;
          Qty2      WITH  CTKTRCVL.Qty2,;
          Qty3      WITH  CTKTRCVL.Qty3,;
          Qty4      WITH  CTKTRCVL.Qty4,;
          Qty5      WITH  CTKTRCVL.Qty5,;
          Qty6      WITH  CTKTRCVL.Qty6,;
          Qty7      WITH  CTKTRCVL.Qty7,;
          Qty8      WITH  CTKTRCVL.Qty8,;
          TotQty    WITH  CTKTRCVL.TotQty,;
          nLineNo   WITH CTKTRCVL.nLineNo ,;
          LINENO    WITH CTKTRCVL.LINENO ,;
          cWareCode WITH CTKTRCVL.cWareCode,;
          cRetSty   WITH CTKTRCVL.cRetSty ,;
          cStyGrade WITH CTKTRCVL.cStyGrade

        GATHER FROM laEstiCost FIELDS nFLanCost1,nFLanCost2,nFLanCost3,nFLanCost4,;
          nFLanCost5,nFLanCost6,nFLanCost7
        GATHER FROM laEstiCost FIELDS nFCost1,nFCost2,nFCost3,nFCost4,nFCost5,nFCost6,nFCost7
        IF loFormSet.lcBusDoc+loFormSet.lcWorkOrd $ 'PP'
          lcPosLn = loFormSet.lcPosLn
          llZeroQty = .F.
          IF laEstQty[9] = 0
            llZeroQty = .T.
            laEstQty[9] = 1
          ENDIF
          =lfBaseCost(loFormSet, &lcPosLn..cStyType, &lcPosLn..PO, &lcPosLn..cPriceCur, &lcPosLn..nPriceRat, &lcPosLn..nCurrUnit, &lcPosLn..cDutyCur, &lcPosLn..nDutyRat, &lcPosLn..nDCurUnit,@laEstQty)
          IF llZeroQty
            laEstQty[9] = 0
          ENDIF
        ELSE
          IF loFormSet.llMulCurr
            =lfGetEqv('1234567',nLanPrRat,nLanDuRat,loFormSet.lnCurrUnt1,loFormSet.lnCurrUnt2,;
              nFLanCost1,nFLanCost2,nFLanCost3,nFLanCost4,nFLanCost5,nFLanCost6,;
              nFLanCost7,loFormSet)
          ELSE
            =ACOPY(laEstiCost,loFormSet.laECost)
          ENDIF
        ENDIF
        SELECT (loFormSet.lcTmpLine)
        GATHER FROM loFormSet.laECost FIELDS nICost1,nICost2,nICost3,nICost4,nICost5,nICost6,nICost7
        REPLACE nLanPrRat WITH lnCrRt1
        GATHER FROM loFormSet.laECost FIELDS nLan_Cost1,nLan_Cost2,nLan_Cost3,nLan_Cost4,nLan_Cost5,;
          nLan_Cost6,nLan_Cost7

        GATHER FROM laEstiCost FIELDS nFLanCost1,nFLanCost2,nFLanCost3,nFLanCost4,nFLanCost5,nFLanCost6,nFLanCost7

        IF lcPType='L'
          REPLACE  TotStk WITH CTKTRCVL.TotQty
        ENDIF
        IF lcPType='H'
          REPLACE  TotStk WITH CTKTRCVL.TotQty;
            TotDam WITH 0;
            TotCan WITH 0;
            TotBal WITH TotQty-TotStk;
            LINENO WITH nLineNo
          lnTotStk   = lnTotStk + TotStk
        ENDIF

        loFormSet.lnTotStk = loFormSet.lnTotStk + TotStk

        SELECT CTKTRCVL
        lcKey = cuttkt+STYLE+dyelot+ccarton+STR(nLineNo,6)+STR(LINENO,6)



      ENDIF


      SELECT (lcTmpLine)
      SCATTER MEMO MEMVAR
      APPEND BLANK
      GATHER MEMO MEMVAR
      REPLACE TranCd    WITH CTKTRCVL.TranCd,;
        QTY1      WITH CTKTRCVL.Qty1,;
        QTY2      WITH CTKTRCVL.Qty2,;
        QTY3      WITH CTKTRCVL.Qty3,;
        QTY4      WITH CTKTRCVL.Qty4,;
        QTY5      WITH CTKTRCVL.Qty5,;
        QTY6      WITH CTKTRCVL.Qty6,;
        QTY7      WITH CTKTRCVL.Qty7,;
        QTY8      WITH CTKTRCVL.Qty8,;
        totQTY      WITH  CTKTRCVL.totQty,;
        cStyGrade WITH CTKTRCVL.cStyGrade,;
        cRetSty   WITH CTKTRCVL.cRetSty


      IF lcPType='H'
        REPLACE  TotStk WITH CTKTRCVL.TotQty;
          TotDam WITH 0;
          TotCan WITH 0;
          TotBal WITH TotQty-TotStk;
          LINENO WITH nLineNo
      ENDIF
    ENDSCAN
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  ELSE  && C/t Batch.
    SCAN WHILE cType+TmpRcvNum = 'M'+lcBatch
      SELECT CUTTKTL
      =SEEK(CTKTRCVL.Cuttkt+CTKTRCVL.STYLE)
      SCATTER FIELDS nCost1,nCost2,nCost3,nCost4,nCost5 TO laEstiCost
      SCATTER FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd TO laAlo
      SELECT CTKTRCVL
      =SEEK(STYLE,'STYLE')
      *B603100,1 SSH 22/05/2000 Fix the bug of incoret recieved cost in case of
      *B603100,1 SSH 22/05/2000 recieve by batch.
      *SCATTER FIELDS Cuttkt,TranCd,cCarton,Lineno,Style,Dyelot,cWareCode,Reference,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,cStyGrade,nLineNo TO laFields
      SCATTER FIELDS Cuttkt,TranCd,cCarton,LINENO,STYLE,Dyelot,cWareCode,REFERENCE,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,STYLE.cStyGrade,nLineNo TO laFields
      *B603100,1 SSH 22/05/2000 [End]
      IF cType+TmpRcvNum+Cuttkt+STYLE+Dyelot+cCarton+IIF(llByCarton,STR(LINENO,6),'') <> lcKey
        SELECT (lcTmpLine)
        APPEND BLANK
        GATHER FROM laFields   FIELDS Cuttkt,TranCd,cCarton,LINENO,STYLE,Dyelot,cWareCode,REFERENCE,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,cStyGrade,nLineNo
        GATHER FROM laEstiCost FIELDS nCost1,nCost2,nCost3,nCost4,nCost5
        GATHER FROM laEstiCost FIELDS nLan_CST1,nLan_CST2,nLan_CST3,nLan_CST4,nLan_CST5
        REPLACE TranCd WITH '1'
        GATHER FROM laAlo    FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
        SELECT CTKTRCVL
        lcKey='M'+TmpRcvNum+Cuttkt+STYLE+Dyelot+cCarton+IIF(llByCarton,STR(LINENO,6),'')
      ENDIF
      SELECT (lcTmpLine)
      APPEND BLANK
      GATHER FROM laFields   FIELDS Cuttkt,TranCd,cCarton,LINENO,STYLE,Dyelot,cWareCode,REFERENCE,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,cStyGrade,nLineNo
      GATHER FROM laEstiCost FIELDS nCost1,nCost2,nCost3,nCost4,nCost5
      GATHER FROM laEstiCost FIELDS nLan_CST1,nLan_CST2,nLan_CST3,nLan_CST4,nLan_CST5
    ENDSCAN

  ENDIF

  *C200170,1 AMH In case of issue inter-location P/O Batch read the lines and return [Start]
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  *!*      IF lcPType = 'H'
  *!*        IF !EOF()
  *!*          laScrMode=.F.
  *!*          laScrMode[4]=.T.
  *!*        ELSE
  *!*          GO TOP
  *!*          IF !EOF()
  *!*              laScrMode=.F.
  *!*              laScrMode[4]=.T.
  *!*          ENDIF
  *!*        ENDIF
  *!*        SHOW GETS
  *!*
  *!*        *--Read line info.
  *!*        SELECT (lcTmpLine)
  *!*        =lfReadLine(EOF())

  *!*        SELECT(lnAlias)
  *!*        RETURN
  *!*      ENDIF
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  *C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
  STORE 0  TO lnTStk,lnTDam,lnTCan
  lcVar1=''
  SELECT CTKTRCVL
  *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
  *=SEEK(IIF(llMfCall,'M','I')+lcBatch)
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  *=SEEK(IIF(llMfCall,'M',IIF(lcPType='L','N','I'))+lcBatch)
  =gfSEEK(IIF(loFormSet.llMfCall,'M',IIF(loFormSet.lcPType$'LO','N','I'))+lcBatch)
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  *E301480,1 NAD (End)

  *E300935,4 Change lcKey to match imported styles with dyelots.
  *lcKey = cType+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+IIF(llByCarton,STR(LineNo,6),'')
  lcKey =  Cuttkt+STYLE+Dyelot+cCarton+STR(nLineNo,6)+STR(LINENO,6)

  *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
  *SCAN WHILE cType+TmpRcvNum = IIF(llMfCall,'M','I')+lcBatch
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  REPLACE TotStk WITH 0,;
    TotDam WITH 0,;
    TotCan WITH 0,;
    TOTQTY WITH 0 ,;
    TotBal WITH 0 FOR TRANCD = '1' IN (lcTmpLine)
  SELECT CTKTRCVL
  lcKeyFld = ''
  STORE 0 TO lnQtyRc1,lnQtyRc2,lnQtyRc3,lnQtyRc4,lnQtyRc5,lnQtyRc6,lnQtyRc7,lnQtyRc8
  SCAN WHILE cType+TmpRcvNum = IIF(loFormSet.llMfCall,'M',IIF(loFormSet.lcPType$'LO','N','I'))+lcBatch
    IF lcKeyFld  <>  Cuttkt+STYLE+Dyelot+cCarton+STR(nLineNo,6)+STR(LINENO,6)
      IF !EMPTY(lcVar1)
        =SEEK('1'+lcVar1,lcTmpLine)
        REPLACE  TotBal WITH MAX(Qty1 - lnQtyRc1,0)+ MAX(Qty2 - lnQtyRc2,0)+;
          MAX(Qty3 - lnQtyRc3,0)+ MAX(Qty4 - lnQtyRc4,0)+;
          MAX(Qty5 - lnQtyRc5,0)+ MAX(Qty6 - lnQtyRc6,0)+;
          MAX(Qty7 - lnQtyRc7,0)+ MAX(Qty8 - lnQtyRc8,0) IN (lcTmpLine)
      ENDIF
      STORE 0 TO lnQtyRc1,lnQtyRc2,lnQtyRc3,lnQtyRc4,lnQtyRc5,lnQtyRc6,lnQtyRc7,lnQtyRc8
      lcSqlStatement  =  "SELECT  POSLN.*, POSHDR.cPriceCur, POSHDR.cDutyCur, PosHdr.nPriceRat, PosHdr.nCurrUnit, PosHdr.nDutyRat, PosHdr.nDCurUnit, POSHDR.Status "+;
        "FROM POSLN posln (INDEX=POSLN) inner join POSHDR (INDEX=POSHDR) "+;
        "ON POSHDR.cBusDocu = POSLN.cBusDocu AND POSHDR.cStyType = POSLN.cStyType "+;
        "AND  POSHDR.PO = POSLN.PO "+;
        "WHERE POSLN.cBusDocu = '" + loFormSet.lcBusDoc +;
        "' AND POSLN.cStyType = '" + loFormSet.lcWorkOrd +;
        "' AND POSLN.PO = '" + CTKTRCVL.cuttkt +;
        "' AND POSLN.cInvType='"+loFormSet.lcInvType + "' AND TRANCD ='1' AND POSLN.STYLE = '"+CTKTRCVL.STYLE+"' AND [LINENO] = "+STR(CTKTRCVL.LINENO,6)

      =lfOpenSql(lcSqlStatement,'POSLN',loFormSet.lcPosLn, "","",.F.)
      DIMENSION laIndex[1,2]
      laIndex = ''
      laIndex[1,1] = 'cBusDocu+cStyType+PO+cInvType+Style+STR(LineNo,6)+TranCd'
      laIndex[1,2] = 'POSLN'
      =lfSetIndex(loFormSet.lcPosLn,@laIndex)
      lcVar1 = CTKTRCVL.cCarton+CTKTRCVL.Cuttkt+CTKTRCVL.STYLE+CTKTRCVL.Dyelot+CTKTRCVL.cwarecode+STR(CTKTRCVL.LINENO,6)
      =SEEK('1'+lcVar1,lcTmpLine)
      FOR I=1 TO 8
        Z=STR(I,1)
        REPLACE Qty&Z WITH EVALUATE(loFormSet.lcPosLn+'.QTY'+Z) IN (lcTmpLine)
      ENDFOR
      REPLACE TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 IN (lcTmpLine)
    ENDIF

    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
    *E301480,1 NAD (End)

    *E300935,4 There is no need to cType+TmpRcvNum because it's in scan command.
    *IF cType+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+IIF(llByCarton,STR(LineNo,6),'') <> lcKey
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
    *!*	      IF Cuttkt+Style+Dyelot+cCarton+STR(nLineNo,6)+STR(LineNo,6) <> lcKey
    *!*	        SELECT (lcTmpLine)
    *!*	        SEEK '1'+lcVar1
    *!*	        REPLACE TotStk WITH lnTStk,;
    *!*	                TotDam WITH lnTDam,;
    *!*	                TotCan WITH lnTCan
    *!*	        STORE 0 TO lnTStk,lnTDam,lnTCan
    *!*	        SELECT CTKTRCVL
    *!*	        *lcKey=cType+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+IIF(llByCarton,STR(LineNo,6),'')
    *!*	        lcKey =  Cuttkt+Style+Dyelot+cCarton+STR(nLineNo,6)+STR(LineNo,6)
    *!*	      ENDIF
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
    *lcVar1=cCarton+Cuttkt+Style+IIF(llMfCall,Dyelot,'')+IIF(llByCarton,STR(LineNo,6),'')
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
    *lcVar1 =cCarton+Cuttkt+Style+Dyelot+IIF(llMfCall,'',STR(LineNo,6))
    *!*	     DO CASE
    *!*	        CASE TranCd = '2'
    *!*	          lnTStk=lnTStk+TotQty
    *!*	        CASE TranCd = lcOthrTrCd
    *!*	          lnTDam=lnTDam+TotQty
    *!*	        CASE TranCd = lcCanlTrCd
    *!*	          lnTCan=lnTCan+TotQty
    *!*	      ENDCASE
    SELECT CTKTRCVL
    lcVar1 =cCarton+Cuttkt+STYLE+Dyelot+cwarecode+STR(LINENO,6)
    =SEEK('1'+lcVar1,lcTmpLine)
    DO CASE
    CASE TranCd = '2'
      REPLACE TotStk WITH TotStk + CTKTRCVL.TotQty IN (lcTmpLine)
    CASE TranCd = lcOthrTrCd
      REPLACE TotDam WITH TotDam + CTKTRCVL.TotQty  IN (lcTmpLine)
    CASE TranCd = lcCanlTrCd
      REPLACE TotCan WITH TotCan + CTKTRCVL.TotQty  IN (lcTmpLine)
    ENDCASE
    FOR lnU = 1 TO 8
      lcU = STR(lnU,1)
      lnQtyRc&lcU. = lnQtyRc&lcU. + CTKTRCVL.Qty&lcU.
    ENDFOR

    *REPLACE TotBal WITH TotBal + MAX(Qty1 - CTKTRCVL.Qty1,0)+ MAX(Qty2 - CTKTRCVL.Qty2,0)+;
    MAX(Qty3 - CTKTRCVL.Qty3,0)+ MAX(Qty4 - CTKTRCVL.Qty4,0)+;
    MAX(Qty5 - CTKTRCVL.Qty5,0)+ MAX(Qty6 - CTKTRCVL.Qty6,0)+;
    MAX(Qty7 - CTKTRCVL.Qty7,0)+ MAX(Qty8 - CTKTRCVL.Qty8,0) IN (lcTmpLine)
    lcKeyFld  =  Cuttkt+STYLE+Dyelot+cCarton+STR(nLineNo,6)+STR(LINENO,6)
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  ENDSCAN
  *--Update last record.
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  *!*	    SELECT (lcTmpLine)
  *!*	    SEEK '1'+lcVar1
  *!*	    REPLACE TotStk WITH lnTStk,;
  *!*	            TotDam WITH lnTDam,;
  *!*	            TotCan WITH lnTCan
  IF !EMPTY(lcVar1)
    =SEEK('1'+lcVar1,lcTmpLine)
    REPLACE  TotBal WITH MAX(Qty1 - lnQtyRc1,0)+ MAX(Qty2 - lnQtyRc2,0)+;
      MAX(Qty3 - lnQtyRc3,0)+ MAX(Qty4 - lnQtyRc4,0)+;
      MAX(Qty5 - lnQtyRc5,0)+ MAX(Qty6 - lnQtyRc6,0)+;
      MAX(Qty7 - lnQtyRc7,0)+ MAX(Qty8 - lnQtyRc8,0) IN (lcTmpLine)
  ENDIF
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]

  *--Get originals.

  SELECT (lcTmpLine)
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  *!*      IF !llByCarton
  *!*      *--In case of not by carton batch update lineNo with original lineNo on P/o or C/t.
  *!*      *--In case of by carton batch it will be updated while saving.
  *!*        REPLACE ALL LineNo WITH nLineNo
  *!*      ENDIF
  *!*	    DIME laOpnQty[8]
  *!*	    laOpnQty  =0
  **    DIME laOpenQty[8]
  **    laOpenQty=0
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  *--Compute previous open balance on style P/o.----------------
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  **    SELECT (lcTmpLine)
  **    SEEK '1'
  **    SCAN WHILE TranCd='1'
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
  *lcMainKy = IIF(llMfCall,Cuttkt+Style+Dyelot,'P'+PO+Style+STR(nLineNo,6))
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  *lcMainKy = IIF(llMfCall,Cuttkt+Style+Dyelot,IIF(lcPType='L','N','P')+PO+Style+STR(nLineNo,6))
  *!*        =lfGetOpen( lcMainKy )
  *!*        SELECT (lcTmpLine)
  *!*        FOR I=1 TO 8
  *!*          Z=STR(I,1)
  *!*          REPLACE Qty&Z WITH laOpnQty[I]
  *!*        ENDFOR

  *E301480,1 NAD (End)
  *!*
  *!*        REPLACE TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
  *!*                TotBal WITH IIF(TotQty-(TotStk+TotDam+TotCan)<0,0,TotQty-(TotStk+TotDam+TotCan))
  *!*        laOpnQty=0
  *!*      ENDSCAN
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  *-- Check Batch Line Quantity if the Batch was modifed.
  *-- ( laCan[lnCnxt] > laOrg[lnCnxt] - laAlo[lnCnxt] ).
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  *!*      WAIT WINDOW 'Validate the batch lines quantity...' NOWAIT
  *!*      IF llSOInstld
  *!*        SCAN FOR TranCd = '1'
  *!*          lnRecNo = RECNO()

  *!*          *E300935,4 Change key to have dyelots in P/O and cWarecode (Add Location)
  *!*          *lcLKey=cCarton+IIF(llMFCall,Cuttkt,Po)+Style+IIF(llMFCall,IIF(llDyelot,Dyelot,''),STR(LineNo,6))
  *!*          lcLKey=cCarton+IIF(llMFCall,Cuttkt,Po)+Style+PADR(Dyelot,10)+PADR(cWareCode,6)+STR(LineNo,6)

  *!*          DIME laOrg[8],laCan[8],laAlo[8],laBud[8]
  *!*          STORE 0 TO laOrg,laCan,laAlo,laBud
  *!*          *--Get Cancel quantity.
  *!*          IF SEEK(lcCanlTrCd+lcLKey)
  *!*            SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laCan
  *!*          ENDIF
  *!*          =SEEK('1'+lcLKey)
  *!*          SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laOrg
  *!*          SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laBud
  *!*          SCATTER FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8 TO laAlo
  *!*          =SEEK(&lcTmpLine..Style,'STYLE')
  *!*          =SEEK('S'+STYLE.Scale,'SCALE')

  *!*          FOR lnCnxt = 1 TO 8
  *!*            *B603216,1 ASH 10/18/1999 (Begin) Fix the bug of getting wrong message in case of no cancel qty found.
  *!*            *IF ( laCan[lnCnxt] > laOrg[lnCnxt] - laAlo[lnCnxt] )
  *!*            IF laCan[lnCnxt]<>0 AND ( laCan[lnCnxt] > laOrg[lnCnxt] - laAlo[lnCnxt] )
  *!*            *B603216,1 ASH 10/18/1999 (End)
  *!*              lcCnxt = STR(lnCnxt,1)
  *!*              lcSz&lcCnxt = SCALE.Sz&lcCnxt
  *!*              lcCtPkKey = IIF(llMfCall,'1'+&lcTmpLine..Cuttkt,'2'+&lcTmpLine..PO)+&lcTmpLine..Style
  *!*              IF SEEK(lcCtPkKey,'CutPick')
  *!*                =lfChkOrdQt(lcCnxt,.T.)
  *!*              ENDIF
  *!*            ENDIF
  *!*          ENDFOR
  *!*          SELECT (lcTmpLine)
  *!*
  *!*          *E300935,4 Check of phisical record before go to it.
  *!*          IF (lnRecNo # 0) AND (lnRecNo <= RECCOUNT())
  *!*            GOTO lnRecNo
  *!*          ENDIF
  *!*        ENDSCAN
  *!*      ENDIF
  *!*      WAIT CLEAR
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  GO TOP

  ************************
  *-- F R O M   C / T. --*
  ************************
  *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
  *!*	  CASE loFormSet.lcPType = 'M'
  *!*	    *--Get originals.
  *!*	    DIME laOpnQty[8]
  *!*	    laOpnQty  =0
  *!*	    *B603275,1 AMM Declare variable that hold last cuttkt line number
  *!*	    lnLastLNo = 0
  *!*	    *B603275,1 AMM end
  *!*	    SELECT CUTTKTL
  *!*	    lnSvRc=RECNO()
  *!*	    SCATTER MEMVAR
  *!*	    SCATTER FIELDS nCost1,nCost2,nCost3,nCost4,nCost5 TO laEstiCost
  *!*	    lcLKey = Cuttkt+Style+IIF(llDyelot,lcDyelot,SPACE(10))
  *!*	    *--Compute previous open balance on C/t style/dyelot.----------------
  *!*	    =lfGetOpen( lcLKey )
  *!*	    llNewLn=.F.
  *!*	    IF llDyelot AND !EMPTY(lcDyelot) AND !SEEK(lcLKey,'CUTTKTL')
  *!*	      llNewLn =.T.
  *!*	      *B603275,1 AMM Get last line number
  *!*	      =SEEK(LEFT(lcLKey,6))
  *!*	      SCAN REST WHILE CUTTKT = LEFT(lcLKey,6)
  *!*	        lnLastLNo = MAX(lnLastLNo,LINENO)
  *!*	      ENDSCAN
  *!*	      *B603275,1 AMM end
  *!*	    ENDIF
  *!*
  *!*	    *E300935,4 Check of phisical record before go to it.
  *!*	    IF BETWEEN(lnSvRc,1,RECCOUNT())
  *!*	      GOTO lnSvRc
  *!*	    ENDIF
  *!*
  *!*	    SELECT (lcTmpLine)
  *!*	    *B603275,1 AMM Get last line number
  *!*	    IF llNewLn
  *!*	      lcOldLOrd = ORDER()
  *!*	      SET ORDER TO TmpLine3
  *!*	      =SEEK(LEFT(lcLKey,6))
  *!*	      SCAN REST WHILE CUTTKT = LEFT(lcLKey,6)
  *!*	        lnLastLNo = MAX(lnLastLNo,LINENO)
  *!*	      ENDSCAN
  *!*	      m.LineNo = lnLastLNo + 1
  *!*	      SET ORDER TO (lcOldLOrd)
  *!*	    ENDIF
  *!*	    *B603275,1 AMM end
  *!*	    APPEND BLANK

  *!*	    *E300935,4 If this line is found before (make N/A location)
  *!*	    m.cWareCode = IIF(llEmpWare,'',m.cWareCode)
  *!*
  *!*	    GATHER MEMVAR
  *!*	    *--If specific operation lot selected.
  *!*	    IF loFormSet.llSpecLot
  *!*	      GATHER FROM loFormSet.laLotArry FIELDS Qty1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8
  *!*	    ELSE
  *!*	      GATHER FROM laOpnQty  FIELDS Qty1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8
  *!*	    ENDIF
  *!*	    *FOR I=1 TO 8
  *!*	    *  Z=STR(I,1)
  *!*	    *  REPLACE Qty&Z WITH laOpnQty[I]
  *!*	    *ENDFOR
  *!*
  *!*	    *C101424,1 Replace cRcvBy according to current receive (C/T or Style) [Begin]
  *!*	    *REPLACE Dyelot   WITH IIF(llDyelot,lcDyelot,''),;
  *!*	    *        lNewLn   WITH llNewLn,;
  *!*	    *        cLotNo   WITH lcLotNo,;
  *!*	    *        clastopr WITH lcClrLstOp,;
  *!*	    *        TotQty   WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
  *!*	    *        TotStk   WITH 0,;
  *!*	    *        TotDam   WITH 0,;
  *!*	    *        TotCan   WITH 0,;
  *!*	    *        TotBal   WITH TotQty
  *!*
  *!*	    REPLACE Dyelot   WITH IIF(llDyelot,lcDyelot,''),;
  *!*	            lNewLn   WITH llNewLn,;
  *!*	            cLotNo   WITH lcLotNo,;
  *!*	            clastopr WITH lcClrLstOp,;
  *!*	            TotQty   WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
  *!*	            TotStk   WITH 0,;
  *!*	            TotDam   WITH 0,;
  *!*	            TotCan   WITH 0,;
  *!*	            TotBal   WITH TotQty,;
  *!*	            cRcvBy   WITH lcCurrSty
  *!*	    *C101424,1 Replace cRcvBy according to current receive (C/T or Style) [End  ]

  *!*	    GATHER FROM laEstiCost FIELDS nLan_Cst1,nLan_Cst2,nLan_Cst3,nLan_Cst4,nLan_Cst5


  ************************************
  *-- B Y   S T Y L E   P / O. --*
  ************************************
  *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
  *CASE loFormSet.lcPType $ 'IRNODAEPG'
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
  *CASE loFormSet.lcPType $ 'IRNODAEPGM'
CASE loFormSet.lcPType $ 'IRNODAEPGMW'
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
  *N038893,1 WAM 06/02/2005 (End)
  STORE 1 TO lnCrRt1,lnCrRt2
  IF loFormSet.llMulCurr
    SELECT (loFormSet.lcPosLn)
    *B608718,1 WAM 10/09/2008 Store budget quantity
    SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,Totqty TO laEstQty
    *B608718,1 WAM 10/09/2008 (End)

    IF loFormSet.llMulCurr
      *B000122,1 WAM 03/20/2005 for new line, get price and duty currencies from purchase order
      *loFormSet.lcCur1 = cPriceCur
      *lnCrRt1 = IIF(cPriceCur = oAriaApplication.BaseCurrency, 1,;
      gfChkRate('loFormSet.lnCurrUnt1',cPriceCur,;
      loFormSet.AriaForm1.dtpickerReceivingDate.Value,loFormSet.llEditExRt,;
      oAriaApplication.ActiveCompanyId,.F.))
      *loFormSet.lcCur2 = cDutyCur
      *lnCrRt2 = IIF(cDutyCur = oAriaApplication.BaseCurrency, 1,;
      gfChkRate('loFormSet.lnCurrUnt2',cDutyCur,;
      loFormSet.AriaForm1.dtpickerReceivingDate.Value,loFormSet.llEditExRt,;
      oAriaApplication.ActiveCompanyId,.F.))
      loFormSet.lcCur1 = IIF(loFormSet.llNewItem,EVALUATE(loFormSet.lcPosHdr+'.cPriceCur'),cPriceCur)
      lnCrRt1 = IIF(loFormSet.lcCur1 = oAriaApplication.BaseCurrency, 1,;
        gfChkRate('loFormSet.lnCurrUnt1',loFormSet.lcCur1,;
        loFormSet.AriaForm1.dtpickerReceivingDate.VALUE,loFormSet.llEditExRt,;
        oAriaApplication.ActiveCompanyId,.F.))

      loFormSet.lcCur2 = IIF(loFormSet.llNewItem,EVALUATE(loFormSet.lcPosHdr+'.cDutyCur'),cDutyCur)
      lnCrRt2 = IIF(loFormSet.lcCur2 = oAriaApplication.BaseCurrency, 1,;
        gfChkRate('loFormSet.lnCurrUnt2',loFormSet.lcCur2,;
        loFormSet.AriaForm1.dtpickerReceivingDate.VALUE,loFormSet.llEditExRt,;
        oAriaApplication.ActiveCompanyId,.F.))
      *B000122,1 WAM 03/20/2005 (End)

    ENDIF

    *--Cannot accept zero or -ve rates.
    IF lnCrRt1 = 0 OR lnCrRt2 = 0
      IF !loFormSet.llEditExRt
        *--This line has currency with zero rate, it will be ignored.'
        = gfModalGen('TRM34079B42000','DIALOG')
        STORE ' ' TO loFormSet.lcStyle,lcCur1,lcCur2
        RETURN
      ELSE
        STORE 1 TO lnCrRt1,lnCrRt2
      ENDIF
    ENDIF
  ENDIF
  IF loFormSet.llNewItem
    IF SEEK(IIF(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+loFormSet.lcStyle,;
        loFormSet.lcTmpItem)
      m.cStyDesc = EVALUATE(loFormSet.lcTmpItem+'.Desc1')
      m.cDye_Flg = EVALUATE(loFormSet.lcTmpItem+'.cDye_Flg')
    ENDIF
    *B608718,1 WAM 10/09/2008 get cost from style file
    SELECT (loFormSet.lcTmpItem)
    *B608718,1 WAM 10/09/2008 (End)
    *! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [Start]
    *SCATTER FIELDS nICost1,nICost2,nICost3,nICost4,nICost5,nICost6,nICost7 TO laEstiCost
    lcBomAlias      = gfTempName()
    =gfOpenTable('Bom','multibom',"SH",lcBomAlias)
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
    *lcCstShType = IIF(loFormSet.lcPType $ 'ISB','I',IIF(loFormSet.lcPType $ 'MT','M',""))
    lcCstShType = IIF(loFormSet.lcPType $ 'ISB','I',IIF(loFormSet.lcPType $ 'MT','M',IIF(loFormSet.lcPType = 'W',"T","")))
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
    =gfsqlrun("SELECT  BOM.* FROM BOM INNER JOIN BOMHEADR ON  BOMHEADR.cinvtype = BOM.cinvtype  AND  BOMHEADR.cCstShtTyp = BOM.cCstShtTyp AND "+;
      " BOMHEADR.cItmMajor = BOM.cItmMajor  and  BOMHEADR.ccstsht_id =  BOM.ccstsht_id  WHERE BOM.cinvtype = '"+loFormSet.lcInvType+"' AND BOM.cItmMajor = '" + ;
      SUBSTR(loFormSet.lcStyle,1,loFormSet.lnMjrWid)+ "'" +;
      " AND BOM.cCstShtTyp ='" + lcCstShtType + "' AND BOMHEADR.ldefcstsht = 1",lcBomAlias)
    SELECT(lcBomAlias)
    LOCATE REST WHILE CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6) =loFormSet.lcInvType+PADR(SUBSTR(loFormSet.lcStyle,1,loFormSet.lnMjrWid),19)+lcCstShType FOR CCATGTYP = 'P' AND CCURRCODE = EVALUATE(loFormSet.lcPosHdr+'.CPRICECUR')
    IF !FOUND()
      =gfsqlrun("SELECT  BOM.* FROM BOM INNER JOIN BOMHEADR ON  BOMHEADR.cinvtype = BOM.cinvtype  AND  BOMHEADR.cCstShtTyp = BOM.cCstShtTyp AND "+;
        " BOMHEADR.cItmMajor = BOM.cItmMajor and  BOMHEADR.ccstsht_id =  BOM.ccstsht_id  WHERE BOM.cinvtype = '"+loFormSet.lcInvType+"' AND BOM.cItmMajor = '" + ;
        SUBSTR(loFormSet.lcStyle,1,loFormSet.lnMjrWid)+ "'" +;
        " AND BOM.cCstShtTyp ='" + lcCstShType + "' AND BOMHEADR.ccstsht_id IN (Select Top 1 ccstsht_id from BOM WHERE BOM.cinvtype = '"+loFormSet.lcInvType+"' AND BOM.cItmMajor = '" + ;
        SUBSTR(loFormSet.lcStyle,1,loFormSet.lnMjrWid)+ "'" +;
        " AND BOM.cCstShtTyp ='" + lcCstShType + "' AND ccatgtyp ='P' AND CCURRCODE ='"+EVALUATE(loFormSet.lcPosHdr+'.CPRICECUR') +"')",lcBomAlias)
    ENDIF
    SELECT(lcBomAlias)
    DIMENSION laEstiCost[7]
    DIMENSION loFormSet.laECost[7]
    laEstiCost = 0
    loFormSet.laECost = 0
    SCAN
      IF !LIKE(STRTRAN(cItmMask,'*','?'),loFormSet.lcStyle)
        LOOP
      ENDIF
      IF Typ <> '8'
        lnI = EVALUATE(Typ)
        lcI = Typ
        lcUntSin   = '/'
        lcExRSin   = gfGetExSin(@lcUntSin, IIF(EMPTY(cCurrCode),oAriaApplication.BaseCurrency,cCurrCode))
        lnExRate   = IIF(nExRate=0,1,nExRate)
        lnCurrUnit = IIF(nCurrUnit=0,1,nCurrUnit)
        loFormSet.laECost[lnI] = loFormSet.laECost[lnI] + TotCost &lcExRSin lnExRate &lcUntSin lnCurrUnit
        laEstiCost[lnI] = laEstiCost[lnI]+ TotCost
      ENDIF
    ENDSCAN
    *! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [END]

    SELECT (loFormSet.lcTmpLine)
    loFormSet.lnPolstln = MAX(loFormSet.lnPolstln + 1, EVALUATE(loFormSet.lcPosHdr+'.LastLine') + 1)

    APPEND BLANK

    lcWareCode = IIF(llEmpWare,'',lcWareCode)
    REPLACE cBusDocu  WITH loFormSet.lcBusDoc ,;
      cStyType  WITH loFormSet.lcWorkOrd,;
      Po        WITH loFormSet.lcPONo,;
      TranCd    WITH '1',;
      Vendor    WITH EVALUATE(loFormSet.lcPosHdr+'.Vendor'),;
      STYLE     WITH loFormSet.lcStyle,;
      SCALE     WITH EVALUATE(loFormSet.lcTmpItem+'.Scale'),;
      LINENO    WITH loFormSet.lnPolstln,;
      cWareCode WITH lcWareCode,;
      nLanPrRat WITH lnCrRt1,;
      nLanDuRat WITH lnCrRt2,;
      TotQty    WITH 0,;
      TotStk    WITH 0,;
      TotDam    WITH 0,;
      TotCan    WITH 0,;
      TotBal    WITH 0,;
      lNewLn    WITH .T.,;
      Dyelot    WITH PADR(loFormSet.lcDyelot,10)

    REPLACE cStyDesc  WITH m.cStyDesc,;
      cDye_Flg  WITH m.cDye_Flg
    *B000122,1 WAM 03/20/2005 for new line, get price and duty currencies from purchase order
    REPLACE cPriceCur WITH loFormSet.lcCur1,;
      cDutyCur  WITH loFormSet.lcCur2
    *B000122,1 WAM 03/20/2005 (End)
    *B126833,1 WAM 04/03/2005 add new button to edit landed cost for styles has no detail costing
    REPLACE lDetCost WITH  EVALUATE(loFormSet.lcTmpItem+'.lDetCost')
    *B126833,1 WAM 04/03/2005 (End)

    *B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[Start]
    REPLACE CINVTYPE WITH loFormSet.lcInvType
    *B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[End]


    *N000587,1 WAM 12/01/2007 Get equivalent cost from BOM table for each cost element based on saved currency code, exchange rate and unit.
    *! B609634,1 MMT 06/28/2011 Fix bug of Zero Cost in Styinvjl in Case of receiving Interlocation PO[Start]
    *IF loFormSet.lcBusDoc+loFormSet.lcWorkOrd $ 'PP|PD|NN'
    *! B610107,1 MMT 10/04/2012 Add Foreign currency costing to Material MFG order summary[Start]
    *IF loFormSet.lcBusDoc+loFormSet.lcWorkOrd $ 'PP|PD'
    IF loFormSet.lcBusDoc+loFormSet.lcWorkOrd $ 'PP|PD|PF'
      *! B610107,1 MMT 10/04/2012 Add Foreign currency costing to Material MFG order summary[End]
      *! B609634,1 MMT 06/28/2011 Fix bug of Zero Cost in Styinvjl in Case of receiving Interlocation PO[END]
      REPLACE nFCost1    WITH laEstiCost[1],;
        nFCost2    WITH laEstiCost[2],;
        nFCost3    WITH laEstiCost[3],;
        nFCost4    WITH laEstiCost[4],;
        nFCost5    WITH laEstiCost[5],;
        nFCost6    WITH laEstiCost[6],;
        nFCost7    WITH laEstiCost[7],;
        nFLanCost1 WITH laEstiCost[1],;
        nFLanCost2 WITH laEstiCost[2],;
        nFLanCost3 WITH laEstiCost[3],;
        nFLanCost4 WITH laEstiCost[4],;
        nFLanCost5 WITH laEstiCost[5],;
        nFLanCost6 WITH laEstiCost[6],;
        nFLanCost7 WITH laEstiCost[7]
      lcPosHdr = loFormSet.lcPosHdr
      *B608718,1 WAM 10/09/2008 Store budget quantity
      *=lfBaseCost(loFormSet, &lcPosHdr..cStyType, &lcPosHdr..PO, &lcPosHdr..cPriceCur, &lcPosHdr..nPriceRat, &lcPosHdr..nCurrUnit, &lcPosHdr..cDutyCur, &lcPosHdr..nDutyRat, &lcPosHdr..nDCurUnit)
      *! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [Start]
      *=lfBaseCost(loFormSet, &lcPosHdr..cStyType, &lcPosHdr..PO, &lcPosHdr..cPriceCur, &lcPosHdr..nPriceRat, &lcPosHdr..nCurrUnit, &lcPosHdr..cDutyCur, &lcPosHdr..nDutyRat, &lcPosHdr..nDCurUnit,@laEstQty)
      REPLACE Gros_Price WITH  nfcost1
      *! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [End]
      *B608718,1 WAM 10/09/2008 (End)

      *: C200919,1 MMT 01/27/2008 Add trig. to PO price ex. Rate while Receiving[Start]
      IF loFormSet.lcCur1 <> oAriaApplication.BaseCurrency AND ASCAN(loFormSet.laEvntTrig ,PADR('GTPOEXRAT',10))<>0
        loFormSet.mDoTrigger(PADR('GTPOEXRAT' ,10))
      ENDIF
      SELECT (loFormSet.lcTmpLine)
      REPLACE nLanPrRat WITH lnCrRt1
      *: C200919,1 MMT 01/27/2008 Add trig. to PO price ex. Rate while Receiving[END]

      GATHER FROM loFormSet.laECost FIELDS nICost1,nICost2,nICost3,nICost4,nICost5,nICost6,nICost7
      GATHER FROM loFormSet.laECost FIELDS nLan_Cost1,nLan_Cost2,nLan_Cost3,nLan_Cost4,nLan_Cost5,nLan_Cost6,nLan_Cost7
    ELSE
      *N000587,1 WAM 12/01/2007 (End)

      IF loFormSet.lcCur1 = EVALUATE(loFormSet.lcTmpItem+'.cPriceCur')
        REPLACE nFCost1    WITH laEstiCost[1],;
          nFLanCost1 WITH laEstiCost[1]

        =lfGetEqv('1',nLanPrRat,0,loFormSet.lnCurrUnt1,0,nFLanCost1,0,0,0,0,0,0,loFormSet)
        REPLACE nICost1    WITH loFormSet.laECost[1],;
          nLan_Cost1 WITH loFormSet.laECost[1]
      ENDIF
      IF loFormSet.lcCur2 = EVALUATE(loFormSet.lcTmpItem+'.cDutyCur')
        REPLACE nFCost2    WITH laEstiCost[2],;
          nFCost3    WITH laEstiCost[3],;
          nFCost4    WITH laEstiCost[4],;
          nFCost5    WITH laEstiCost[5],;
          nFCost6    WITH laEstiCost[6],;
          nFCost7    WITH laEstiCost[7],;
          nFLanCost2 WITH laEstiCost[2],;
          nFLanCost3 WITH laEstiCost[3],;
          nFLanCost4 WITH laEstiCost[4],;
          nFLanCost5 WITH laEstiCost[5],;
          nFLanCost6 WITH laEstiCost[6],;
          nFLanCost7 WITH laEstiCost[7]

        =lfGetEqv('234567',0,nLanDuRat,0,loFormSet.lnCurrUnt2,0,nFLanCost2,nFLanCost3,;
          nFLanCost4,nFLanCost5,nFLanCost6,nFLanCost7,loFormSet)

        GATHER FROM loFormSet.laECost FIELDS nICost2,nICost3,nICost4,nICost5,nICost6,nICost7
        GATHER FROM loFormSet.laECost FIELDS nLan_Cost2,nLan_Cost3,nLan_Cost4,nLan_Cost5,;
          nLan_Cost6,nLan_Cost7
      ENDIF
      *N000587,1 WAM 12/01/2007 Get equivalent cost from BOM table for each cost element based on saved currency code, exchange rate and unit.
    ENDIF
    *N000587,1 WAM 12/01/2007 (End)

  ELSE

    *--Get originals.
    DIME laOpnQty[8]
    laOpnQty  =0
    SELECT (loFormSet.lcPosLn)
    lnSvRc = RECNO()
    SCATTER MEMVAR
    *B126833,1 WAM 04/03/2005 add new button to edit landed cost for styles has no detail costing
    m.lDetCost = EVALUATE(loFormSet.lcTmpItem+'.lDetCost')
    *B126833,1 WAM 04/03/2005 (End)

    IF SEEK(IIF(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+m.Style,;
        loFormSet.lcTmpItem)
      m.cStyDesc = EVALUATE(loFormSet.lcTmpItem+'.Desc1')
      m.cDye_Flg = EVALUATE(loFormSet.lcTmpItem+'.cDye_Flg')
    ENDIF
    IF loFormSet.lcPType = 'A'
      SCATTER FIELDS nCost1 TO laEstiCost
    ELSE
      *SCATTER FIELDS nICost1,nICost2,nICost3,nICost4,nICost5,nICost6,nICost7 TO laEstiCost
      SCATTER FIELDS nFCost1,nFCost2,nFCost3,nFCost4,nFCost5,nFCost6,nFCost7 TO laEstiCost
    ENDIF

    *--Compute previous open balance on style P/o.----------------
    =lfCalOpen(loFormSet.lcPType,loFormSet.lcPosLn,;
      loFormSet.lcBusDoc+loFormSet.lcWorkOrd+PO+loFormSet.lcInvType+STYLE+STR(LINENO,6),.F.,loFormSet)
    IF BETWEEN(lnSvRc,1,RECCOUNT())
      GOTO lnSvRc
    ENDIF
    SELECT (loFormSet.lcTmpLine)

    *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [Start]
    IF ASCAN(loformset.laEvntTrig,PADR('MNGFNDL1',10),1,ALEN(loformset.laEvntTrig,1),1) > 0
      loFormSet.mDoTrigger(PADR('MNGFNDL1' ,10))
    ELSE
      *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [End]

      APPEND BLANK
      m.cWareCode = IIF(llEmpWare,'',m.cWareCode)

      *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [Start]
    ENDIF
    *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [End]

    GATHER MEMVAR
    REPLACE TranCd WITH '1'

    *--If specific operation lot selected.
    IF loFormSet.llSpecLot
      GATHER FROM loFormSet.laLotArry FIELDS Qty1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8
    ELSE
      GATHER FROM laOpnQty  FIELDS Qty1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8
    ENDIF
    REPLACE nLanPrRat WITH lnCrRt1,;
      nLanDuRat WITH lnCrRt2,;
      cLotNo    WITH loFormSet.lcLotNo,;
      clastopr  WITH loFormSet.lcClrLstOp,;
      TotQty    WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
      TotStk    WITH IIF(loFormSet.lcPType $ 'OE' OR loFormSet.lcAuto = 'A',TotQty,0),;
      TotDam    WITH 0,;
      TotCan    WITH 0,;
      TotBal    WITH IIF(loFormSet.lcPType $ 'OE' OR loFormSet.lcAuto = 'A',0,TotQty),;
      Dyelot    WITH PADR(loFormSet.lcDyelot,10),;
      LAUTOMODE WITH (loFormSet.lcAuto='A')
    IF loFormSet.lcPType = 'A'
      GATHER FROM laEstiCost FIELDS nFLanCOST1
    ELSE
      GATHER FROM laEstiCost FIELDS nFLanCOST1,nFLanCOST2,nFLanCOST3,nFLanCOST4,nFLanCOST5,;
        nFLanCOST6,nFLanCOST7
    ENDIF

    IF loFormSet.llMulCurr .AND. loFormSet.lcPType # 'A'
      *N000587,1 WAM 12/01/2007 Get equivalent cost from BOM table for each cost element based on saved currency code, exchange rate and unit.
      *! B609634,1 MMT 06/28/2011 Fix bug of Zero Cost in Styinvjl in Case of receiving Interlocation PO[Start]
      *IF loFormSet.lcBusDoc+loFormSet.lcWorkOrd $ 'PP|PD|NN'
      *! B610107,1 MMT 10/04/2012 Add Foreign currency costing to Material MFG order summary[Start]
      *IF loFormSet.lcBusDoc+loFormSet.lcWorkOrd $ 'PP|PD'
      IF loFormSet.lcBusDoc+loFormSet.lcWorkOrd $ 'PP|PD|PF'
        *! B610107,1 MMT 10/04/2012 Add Foreign currency costing to Material MFG order summary[End]
        *! B609634,1 MMT 06/28/2011 Fix bug of Zero Cost in Styinvjl in Case of receiving Interlocation PO[END]
        lcPosHdr = loFormSet.lcPosHdr
        *B608718,1 WAM 10/09/2008 Store budget quantity
        *=lfBaseCost(loFormSet, &lcPosHdr..cStyType, &lcPosHdr..PO, &lcPosHdr..cPriceCur, &lcPosHdr..nPriceRat, &lcPosHdr..nCurrUnit, &lcPosHdr..cDutyCur, &lcPosHdr..nDutyRat, &lcPosHdr..nDCurUnit)
        =lfBaseCost(loFormSet, &lcPosHdr..cStyType, &lcPosHdr..PO, &lcPosHdr..cPriceCur, &lcPosHdr..nPriceRat, &lcPosHdr..nCurrUnit, &lcPosHdr..cDutyCur, &lcPosHdr..nDutyRat, &lcPosHdr..nDCurUnit,@laEstQty)
        *B608718,1 WAM 10/09/2008 (End)
        REPLACE nLanPrRat WITH lnCrRt1
      ELSE
        *N000587,1 WAM 12/01/2007 (End)

        =lfGetEqv('1234567',nLanPrRat,nLanDuRat,loFormSet.lnCurrUnt1,loFormSet.lnCurrUnt2,;
          nFLanCost1,nFLanCost2,nFLanCost3,nFLanCost4,nFLanCost5,nFLanCost6,nFLanCost7,loFormSet)
        *N000587,1 WAM 12/01/2007 Get equivalent cost from BOM table for each cost element based on saved currency code, exchange rate and unit.
      ENDIF
      *N000587,1 WAM 12/01/2007 (End)

    ELSE
      =ACOPY(laEstiCost,loFormSet.laECost)
    ENDIF
    *: C200919,1 MMT 01/27/2008 Add trig. to PO price ex. Rate while Receiving[Start]
    IF loFormSet.lcCur1 <> oAriaApplication.BaseCurrency AND ASCAN(loFormSet.laEvntTrig ,PADR('GTPOEXRAT',10))<>0
      loFormSet.mDoTrigger(PADR('GTPOEXRAT' ,10))
    ENDIF
    SELECT (loFormSet.lcTmpLine)
    REPLACE nLanPrRat WITH lnCrRt1
    *: C200919,1 MMT 01/27/2008 Add trig. to PO price ex. Rate while Receiving[END]

    IF loFormSet.lcPType = 'A'
      GATHER FROM loFormSet.laECost FIELDS nLan_Cost1
    ELSE
      GATHER FROM loFormSet.laECost FIELDS nLan_Cost1,nLan_Cost2,nLan_Cost3,nLan_Cost4,nLan_Cost5,;
        nLan_Cost6,nLan_Cost7
      *B608760,1 WAM 12/04/2008 Get foreign cost from the adjusted record for receiving
      GATHER FROM laEstiCost FIELDS nFLanCOST1,nFLanCOST2,nFLanCOST3,nFLanCOST4,nFLanCOST5,nFLanCOST6,nFLanCOST7
      *B608760,1 WAM 12/04/2008 (End)

    ENDIF

    *--If recieve inter Location P/o defult recieved qty by intransit qty.
    *--Or If specific operation lot selected.
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
    *IF loFormSet.lcPType $ 'OE' OR loFormSet.lcAuto = 'A'
    IF loFormSet.lcPType $ 'OE' OR loFormSet.lcAuto $ 'AI'
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
      lnTSvRc = RECNO()

      *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [Start]
      IF ASCAN(loformset.laEvntTrig,PADR('MNGFNDL2',10),1,ALEN(loformset.laEvntTrig,1),1) > 0
        IF !loFormSet.mDoTrigger(PADR('MNGFNDL2' ,10))
          RETURN
        ENDIF
      ELSE
        *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [End]

        SCATTER MEMVAR
        APPEND BLANK
        GATHER MEMVAR
        REPLACE TranCd WITH '2'

        *! B610051,1 MMT 08/16/2012 Receiving Inter-location PO to different location other than target location on PO gives error if Bin location is used[Start]
        IF  loFormSet.lcPType = 'O' AND ASCAN(loformset.laEvntTrig,PADR('ADDBNRCD',10),1,ALEN(loformset.laEvntTrig,1),1) > 0
          =loFormSet.mDoTrigger(PADR('ADDBNRCD' ,10))
        ENDIF
        *! B610051,1 MMT 08/16/2012 Receiving Inter-location PO to different location other than target location on PO gives error if Bin location is used[END]

        IF BETWEEN(lnTSvRc,1,RECCOUNT())
          GOTO lnTSvRc
        ENDIF

        loFormSet.lnTotStk = loFormSet.lnTotStk + TotStk

        *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [Start]
      ENDIF
      *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [End]
    ENDIF

  ENDIF

ENDCASE

*--Swich the mode to Add if there is lines selected.
SELECT (loFormSet.lcTmpLine)
IF !EOF()
  loFormSet.AlterMode("A")
ELSE
  GO TOP
  IF !EOF()
    loFormSet.AlterMode("A")
  ENDIF
ENDIF

*-- In case of receive by shipment and the screen mode is still in the select mode
*-- then return to the select mode. This is done in case you select a shipment that has only
*-- a PO with status hold.
IF loFormSet.lcPType $ 'USC' AND loFormSet.ActiveMode = 'S'
  =lfClearInfo(loFormSet)
  loFormSet.AriaForm1.cntShipment.kbShipNo.ENABLED = .T.
  STORE .T. TO loFormSet.AriaForm1.dtPickerPostingDate.ENABLED,;
    loFormSet.AriaForm1.dtpickerReceivingDate.ENABLED
  loFormSet.AriaForm1.cmdNew.ENABLED = .F.
  RETURN .F.
ENDIF

SELECT (loFormSet.lcTmpLine)
LOCAL lnRecNo
lnRecNo = RECNO()
SET FILTER TO TranCd = "1"
LOCATE

IF !EOF()
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GOTO lnRecNo
  ENDIF
  lfCntrSour(loFormSet,.T.)
ENDIF
loFormSet.AriaForm1.grdReceivingLines.ENABLED = .T.
loFormSet.AriaForm1.grdReceivingLines.REFRESH
=lfActBrow(loFormSet)
=lfReadLine(loFormSet,EOF())
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
IF loFormSet.lcPType $ 'IOM' AND llBatch
  IF loFormSet.AriaForm1.CntBatch.cboBatchStatus.ENABLED
    DIMENSION loFormSet.laStatusArr[2,2]
    lcValStat = loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE
    loFormSet.laStatusArr[1,1]  = LANG_POSTREC_STATUS_OPEN
    loFormSet.laStatusArr[1,2]  = LANG_POSTREC_STATUS_O
    loFormSet.laStatusArr[2,1]  = LANG_POSTREC_STATUS_APPROVED
    loFormSet.laStatusArr[2,2]  = LANG_POSTREC_STATUS_A
    loFormSet.AriaForm1.CntBatch.cboBatchStatus.REQUERY()
    loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE = lcValStat
  ELSE
    lcValStat = loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE
    DIMENSION loFormSet.laStatusArr[4,2]
    loFormSet.laStatusArr[1,1]  = LANG_POSTREC_STATUS_OPEN
    loFormSet.laStatusArr[1,2]  = LANG_POSTREC_STATUS_O
    loFormSet.laStatusArr[2,1]  = LANG_POSTREC_STATUS_APPROVED
    loFormSet.laStatusArr[2,2]  = LANG_POSTREC_STATUS_A
    loFormSet.laStatusArr[3,1]  = LANG_POSTREC_STATUS_CANCEL
    loFormSet.laStatusArr[3,2]  = LANG_POSTREC_STATUS_X
    loFormSet.laStatusArr[4,1]  = LANG_POSTREC_STATUS_POSTED
    loFormSet.laStatusArr[4,2]  = LANG_POSTREC_STATUS_P
    loFormSet.AriaForm1.CntBatch.cboBatchStatus.REQUERY()
    loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE = lcValStat
  ENDIF
  IF loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE $ 'PX'
    loFormSet.oToolBar.cmdadd.ENABLED =.F.
  ENDIF
ENDIF
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
*-- 'S' Style PO Shipment, 'C' Receive Inter-Location PO Shipment
*-- 'F' Receive Material PO Shipment
IF loFormSet.lcPType $ 'SCFU' AND !llShpPO
  WITH loFormSet.AriaForm1.cntShipment
    .dtpickerShpEntered.CONTROLSOURCE = loFormSet.lcMastShp+'.Entered'
    .dtpickerShpETA.CONTROLSOURCE     = loFormSet.lcMastShp+'.Eta'
    .txtShpCartons.CONTROLSOURCE      = loFormSet.lcMastShp+'.Cartons'
    .txtShpAirWay.CONTROLSOURCE       = loFormSet.lcMastShp+'.AirWayB'
    .txtShpReference.CONTROLSOURCE    = loFormSet.lcMastShp+'.Reference'
  ENDWITH
ENDIF

SELECT(lnAlias)
RETURN
*-- end of lfGetInfo.

*!*************************************************************
*! Name      : lfvDyelot
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To validate the dyelot
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfvDyelot
LPARAMETERS loFormSet

IF PADR(EVALUATE(loFormSet.lcTmpLine+'.Dyelot'),10) = PADR(loFormSet.lcDyelot,10)
  RETURN
ENDIF

LOCAL lcMessage, lcKey, lnLineNo, lnRecNo, lcDyeCode
*lcCarton  = ""
*lnLineNo  = EVALUATE(loFormSet.lcPosLn+'.LineNo')
*lnRecNo   = RECNO(loFormSet.lcTmpLine)
lcDyeCode = PADR(loFormSet.AriaForm1.kbconfiguration.keytextbox.VALUE,10)

*!*	IF ((loFormSet.llCMInstld AND loFormSet.llPOSale) OR !(loFormSet.lcPType $ 'NA'))
*!*	  lcSeekDye = '1'+SPACE(3)+loFormSet.lcPONo+loFormSet.lcStyle+lcDyeCode
*!*	  IF SEEK(lcSeekDye,loFormSet.lcTmpLine)
*!*	    SELECT (loFormSet.lcTmpLine)
*!*	    LOCATE REST WHILE TranCd+cCarton+PO+Style+Dyelot+cWareCode+STR(LineNo,6) = ;
*!*	                     '1'+SPACE(3)+loFormSet.lcPONo+loFormSet.lcStyle+lcDyeCode;
*!*	                FOR IIF(loFormSet.llMfCall,.T.,LineNo = lnLineNo)
*!*	    IF FOUND()
*!*	      lcMessage = IIF(loFormSet.llMfCall,'receiving cutting ticket',;
*!*	                  IIF(loFormSet.lcPType $ 'NA','issue ','receiving ') +;
*!*	                  IIF(loFormSet.lcPType='N','inter location ',;
*!*	                  IIF(loFormSet.lcPType='A','adornment',;
*!*	                  IIF(loFormSet.lcPType='D','dye',''))) + ' purchase order')
*!*	      *-This XXXX has been entered on this XXXX.
*!*	      =gfModalGen('TRM42107B42000','DIALOG','Style/Dyelot'+'|'+lcMessage)
*!*	      RETURN .F.
*!*	    ELSE
*!*	    =SEEK(lcSeekDye,loFormSet.lcTmpLine)
*!*	      loFormSet.llNewItem = .T.
*!*	    ENDIF
*!*	  ELSE
*!*	    IF BETWEEN(lnRecNo,1,RECCOUNT(loFormSet.lcTmpLine))
*!*	      GOTO lnRecNo IN (loFormSet.lcTmpLine)
*!*	    ENDIF
*!*	*    loFormSet.llNewItem = .T.
*!*	  ENDIF
*!*	*ENDIF


*!*	lcKey = loFormSet.lcBusDoc+loFormSet.lcWorkOrd+loFormSet.lcPONo+loFormSet.lcInvType+;
*!*			loFormSet.lcStyle+STR(EVALUATE(loFormSet.lcPosLn+'.LineNo'),6)
*!*	IF SEEK(loFormSet.lcBusDoc+loFormSet.lcWorkOrd+loFormSet.lcPONo+loFormSet.lcInvType+;
*!*													loFormSet.lcStyle, loFormSet.lcPosLn)
*!*	  SELECT (loFormSet.lcPosLn)
*!*	  LOCATE REST WHILE cBusDocu+cStyType+PO+cInvType+Style+STR(LineNo,6)+TranCd = ;
*!*	                    loFormSet.lcBusDoc+loFormSet.lcWorkOrd+loFormSet.lcPONo+;
*!*	                    loFormSet.lcInvType+loFormSet.lcStyle;
*!*	              FOR   Dyelot = PADR(loFormSet.lcDyelot,10) AND LineNo = lnLineNo
*!*	  IF FOUND()
*!*	    loFormSet.llNewItem = .F.
*!*	  ENDIF

*!*	ENDIF
*!*	=SEEK(lcKey,loFormSet.lcPosLn)
*!*	= lfChkLine(loFormSet)    && Check for existance of default location.
*SHOW GET lcDyelot DISABLE

SELECT (loFormSet.lcTmpLine)
lnRecNo   = RECNO()
lcLKey    = cCarton+Po+STYLE+loFormSet.lcDyelot+PADR(cWareCode,6)+STR(LINENO,6)
SET FILTER TO
SET ORDER TO TAG TmpLine2
SEEK lcLKey+'2'
REPLACE REST Dyelot WITH lcDyeCode ;
  WHILE cCarton+Po+STYLE+Dyelot+cWareCode+STR(LINENO,6)+TranCd = lcLKey+'2'

*-- To update the receiving lines
SEEK lcLKey+'4'
REPLACE REST Dyelot WITH lcDyeCode ;
  WHILE cCarton+Po+STYLE+Dyelot+cWareCode+STR(LINENO,6)+TranCd = lcLKey+'4'

*-- To update the receiving lines
SEEK lcLKey+'5'
REPLACE REST Dyelot WITH lcDyeCode ;
  WHILE cCarton+Po+STYLE+Dyelot+cWareCode+STR(LINENO,6)+TranCd = lcLKey+'5'

SET ORDER TO TAG TmpLine1
SET FILTER TO TranCd = '1'

IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF


RETURN


*!*************************************************************
*! Name      : lfClearInfo
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To clear the information in case of nothing selected
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfClearInfo
LPARAMETERS loFormSet
*--Clear shipment and batch header.
WITH loFormSet.AriaForm1
  STORE {}  TO  .cntShipment.dtpickerShpEntered.VALUE,;
    .cntShipment.dtpickerShpETA.VALUE
  STORE ""  TO  .cntShipment.txtShpCartons.VALUE,;
    .cntShipment.txtShpAirWay.VALUE,;
    .cntShipment.txtShpReference.VALUE
  STORE .F. TO .cntShipment.dtpickerShpEntered.ENABLED,;
    .cntShipment.dtpickerShpETA.ENABLED,;
    .cntShipment.txtShpCartons.ENABLED,;
    .cntShipment.txtShpAirWay.ENABLED,;
    .cntShipment.txtShpReference.ENABLED
  *-- Do it in case of batch
  *!*	STORE ' ' TO lcBDesc,lcBStatus
  *!*	STORE {}  TO ldBDate

ENDWITH
STORE 0 TO loFormSet.lnTotStk, loFormSet.lnTotDam, loFormSet.lnTotCan, loFormSet.lnPolstln
loFormSet.llByCarton = .F.

IF loFormSet.llMFCall AND loFormSet.lcPtype='M'
  STORE '' TO loFormSet.AriaForm1.kbPoNo.KeyTextBox.VALUE,;
    loFormSet.AriaForm1.kbItem.KeyTextBox.VALUE,;
    loFormSet.AriaForm1.txtitemDesc.VALUE
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
  *loFormSet.AriaForm1.kbItem.KeyTextBox.SetFocus
  IF !loFormSet.llSilentMod
    loFormSet.AriaForm1.kbItem.KeyTextBox.SETFOCUS
  ENDIF
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

ENDIF
=lfReadLine(loFormSet,.T.)

RETURN

*!*************************************************************
*! Name      : lfvWare
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To validate the warehouse field
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfvWare
LPARAMETERS loFormSet
LOCAL  lnRecNo, lcLKey
PRIVATE lcWareCode, lcItemCode, lcDyelotCod

IF SUBSTR(loFormSet.laWare[loFormSet.lnWare],1,6) = EVALUATE(loFormSet.lcTmpLine+'.cWareCode');
    OR loFormSet.lnWare = 0 OR (loFormSet.llCMInstld AND loFormSet.llPOSale)
  RETURN
ENDIF

IF (loFormSet.lnWare = 1) AND ;
    SUBSTR(loFormSet.laWare[loFormSet.lnWare],1,6) <> EVALUATE(loFormSet.lcTmpLine+'.cWareCode')
  *loFormSet.lnWare = loFormSet.AriaForm1.cboLocations.OldValue
  loFormSet.lnWare = ASCAN(loFormSet.laWare,EVALUATE(loFormSet.lcTmpLine+'.cWareCode'),1)
  RETURN 0
ENDIF

lcItemCode  = EVALUATE(loFormSet.lcTmpLine+'.Style')
lcWareCode  = SUBSTR(loFormSet.laWare[loFormSet.lnWare],1,6)
lcDyelotCod = PADR(EVALUATE(loFormSet.lcTmpLine+'.Dyelot'),10)

SELECT (loFormSet.lcTmpLine)
IF lfExistWar()
  lcMessage1 = IIF(loFormSet.lcInvType="0001",'Style','Fabric')+;
    IIF(EMPTY(loFormSet.lcDyelot),'','/Dyelot')+'/Location'
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
  *!*	  lcMessage2 = IIF(loFormSet.llMfCall,'receiving cutting ticket',;
  *!*	    IIF(loFormSet.lcPType='D','receiving dye order',IIF(loFormSet.lcPType $ 'ON',;
  *!*	    'receiving inter location P/O','receiving purchase order')))
  lcMessage2 = IIF(loFormSet.llMfCall,'receiving cutting ticket',;
    IIF(loFormSet.lcPType='D','receiving dye order',IIF(loFormSet.lcPType $ 'ON',;
    'receiving inter location P/O',IIF(loFormSet.lcPType='W',LANG_POSTREC_MMO_RECING_MMO,'receiving purchase order'))))
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
  *-This XXXX has been entered on this XXXX.
  = gfModalGen('TRM42107B42000','DIALOG',lcMessage1+'|'+lcMessage2)
  *loFormSet.lnWare = loFormSet.AriaForm1.cboLocations.OldValue
  loFormSet.lnWare = ASCAN(loFormSet.laWare,EVALUATE(loFormSet.lcTmpLine+'.cWareCode'),1)
  RETURN 0
ENDIF

*-- Case of style
IF loFormSet.lcInvType = "0001"
  *-- Get item information from stydye file for the target location
  *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
*!*	  lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
*!*	    PADR(STYLE,19)+lcWareCode+SPACE(10) + "'"
  *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
*!*	  lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
*!*	    PADR(STYLE,19)+lcWareCode+SPACE(10) + "]"
  lcSelectVal =  PADR(STYLE,19)+lcWareCode+SPACE(10)
  lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot =?m.lcSelectVal "
  *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]  
  *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]  
  =lfOpenFox(lcSqlStatement,'STYDYE',loFormSet.lcTmpCurs,"")
  SELECT(loFormSet.lcTmpCurs)
  LOCATE
  IF EOF()
    *-Style: xxx is not assigned to Location: xxx. "\<Add;\<Reenter"
    IF gfModalGen('QRM34048B42006','DIALOG',ALLTRIM(lcItemCode)+'|'+lcWareCode) = 1
      DO gpAdStyWar WITH lcItemCode,SPACE(10),lcWareCode
      IF !EMPTY(lcDyelotCod)
        DO gpAdStyWar WITH lcItemCode,PADR(lcDyelotCod,10),lcWareCode
      ENDIF
    ELSE
      *loFormSet.lnWare = loFormSet.AriaForm1.cboLocations.OldValue
      loFormSet.lnWare = ASCAN(loFormSet.laWare,EVALUATE(loFormSet.lcTmpLine+'.cWareCode'),1)
      RETURN 0
    ENDIF
  ELSE
    IF !EMPTY(lcDyelotCod)
      *-- Get item information from stydye file for the target location
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
*!*	      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
*!*	        PADR(STYLE,19)+lcWareCode+lcDyelotCod + "'"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
*!*	      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
*!*	        PADR(STYLE,19)+lcWareCode+lcDyelotCod + "]"
      lcStySelVal = PADR(STYLE,19)+lcWareCode+lcDyelotCod 
      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = ?m.lcStySelVal"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]  
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][END]  
      =lfOpenFox(lcSqlStatement,'STYDYE',loFormSet.lcTmpCurs,"")
      SELECT(loFormSet.lcTmpCurs)
      LOCATE
      IF EOF()
        DO gpAdStyWar WITH lcItemCode,lcDyelotCod,lcWareCode
      ENDIF
    ENDIF
  ENDIF
ELSE
  *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
  *lcSqlStatement  = "SELECT * FROM ITEMLOC [INDEX=STYDYE] " + ;
  "WHERE cInvType ='" + loFormSet.lcInvType + "' AND "+;
  "Style ='" + lcItemCode +"' AND " + ;
  "cWareCode ='" + lcWareCode + "' AND " + ;
  "Dyelot = '         '"
  lcSqlStatement  = "SELECT * FROM ITEMLOC [INDEX=STYDYE] " + ;
    "WHERE cInvType ='" + loFormSet.lcInvType + "' AND "+;
    "Style = ?m.lcItemCode " + " AND " + ;
    "cWareCode = ?m.lcWareCode " + " AND " + ;
    "Dyelot = '         '"
  *B607658,1 KHM 07/07/2005 [End]
  =lfOpenSql(lcSqlStatement,'ITEMLOC',loFormSet.lcTmpCurs, "","",.F.)
  SELECT(loFormSet.lcTmpCurs)
  LOCATE
  IF EOF()
    lcMsg = "Fabric: " + ALLTRIM(lcItemCode)
    IF gfModalGen('QRM36226B34004','DIALOG',lcMsg +'|'+lcWareCode) = 1
      =gfAdItemWar(loFormSet.lcInvType,lcItemCode,SPACE(10),lcWareCode)
      IF !EMPTY(lcDyelotCod)
        =gfAdItemWar(loFormSet.lcInvType,lcItemCode,lcDyelotCod,lcWareCode)
      ENDIF
    ELSE
      *loFormSet.lnWare = loFormSet.AriaForm1.cboLocations.OldValue
      loFormSet.lnWare = ASCAN(loFormSet.laWare,EVALUATE(loFormSet.lcTmpLine+'.cWareCode'),1)
      RETURN 0
    ENDIF
  ELSE
    IF !EMPTY(lcDyelotCod)
      *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
      *lcSqlStatement  = "SELECT * FROM ITEMLOC [INDEX=STYDYE] " + ;
      "WHERE cInvType ='" + loFormSet.lcInvType + "' AND "+;
      "Style ='" + lcItemCode +"' AND " + ;
      "cWareCode ='" + lcWareCode + "' AND " + ;
      "Dyelot = '" + lcDyelotCod + "'"
      lcSqlStatement  = "SELECT * FROM ITEMLOC [INDEX=STYDYE] " + ;
        "WHERE cInvType ='" + loFormSet.lcInvType + "' AND "+;
        "Style = ?m.lcItemCode " + " AND " + ;
        "cWareCode = ?m.lcWareCode " + " AND " + ;
        "Dyelot = '" + lcDyelotCod + "'"
      *B607658,1 KHM 07/07/2005
      =lfOpenSql(lcSqlStatement,'ITEMLOC',loFormSet.lcTmpCurs, "","",.F.)
      SELECT(loFormSet.lcTmpCurs)
      LOCATE
      IF EOF()
        =gfAdItemWar(loFormSet.lcInvType,lcItemCode,lcDyelotCod,lcWareCode)
      ENDIF
    ENDIF
  ENDIF
ENDIF

SELECT (loFormSet.lcTmpLine)
lnRecNo   = RECNO()
lcLKey    = cCarton+Po+STYLE+Dyelot+PADR(cWareCode,6)+STR(LINENO,6)
SET FILTER TO
SET ORDER TO TAG TmpLine2
SEEK lcLKey
REPLACE REST cWareCode WITH lcWareCode ;
  WHILE cCarton+Po+STYLE+Dyelot+cWareCode+STR(LINENO,6)+TranCd = lcLKey

*-- To update the receiving lines
SEEK lcLKey
REPLACE REST cWareCode WITH lcWareCode ;
  WHILE cCarton+Po+STYLE+Dyelot+cWareCode+STR(LINENO,6)+TranCd = lcLKey

SET ORDER TO TAG TmpLine1
SET FILTER TO TranCd = '1'

IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF

RETURN

*!*************************************************************
*! Name      : lfExistWar
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To check existance of selected location line.
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
*! Return      : .T. if line is exist before.
*!*************************************************************
FUNCTION lfExistWar
lcCurrRec  = PADR(cCarton,3)+PO+PADR(STYLE,19)+PADR(Dyelot,10)+PADR(cWareCode,6)+STR(LINENO,6)
*-- if this record is found in temp. file before
llExists = SEEK('1'+PADR(cCarton,3)+PADR(PO,6)+PADR(STYLE,19)+PADR(Dyelot,10)+;
  PADR(lcWareCode,6)+STR(LINENO,6))
= SEEK('1'+lcCurrRec)
RETURN llExists
*-- end of lfExistWar.

*!*************************************************************
*! Name      : lfvLocWare
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To validate the bins
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfvLocWare
LPARAMETERS loFormSet
loFormSet.AriaForm1.chkBins.VALUE = .F.
LOCAL lnAlias

SELECT (loFormSet.lcTmpLine)

IF EMPTY(EVALUATE(loFormSet.lcTmpLine+'.cWareCode'))
  *-- Message : 'First, you must select location.'
  =gfModalGen('TRM42150B42001','DIALOG')
  loFormSet.AriaForm1.cboLocations.ENABLED = .T.
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
  *loFormSet.AriaForm1.cboLocations.SetFocus
  IF !loFormSet.llSilentMod
    loFormSet.AriaForm1.cboLocations.SETFOCUS
  ENDIF
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
  RETURN
ENDIF

lnAlias = SELECT()
SELECT (loFormSet.lcTemLoc)
DELETE ALL FOR cWareCode <> EVALUATE(loFormSet.lcTmpLine+'.cWareCode') AND;
  STYLE = EVALUATE(loFormSet.lcTmpLine+'.Style')
SELECT WHSLOC
SET ORDER TO TAG WhsLoc
llFound = SEEK(EVALUATE(loFormSet.lcTmpLine+'.cWareCode'))
SET ORDER TO TAG WhsLocSt
IF llFound
  =lfvLocat(loFormSet)
ELSE
  *--No bins have been assigned to location XXX .
  = gfModalGen('TRM42058B42000','DIALOG',ALLTRIM(EVALUATE(loFormSet.lcTmpLine+'.cWareCode')))
ENDIF
SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfvLocat
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To select the bins from the mover
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfvLocat
LPARAMETERS loFormSet
SELECT WHsLoc
SEEK EVALUATE(loFormSet.lcTmpLine+'.Style')+SPACE(6)+;
  EVALUATE(loFormSet.lcTmpLine+'.cWareCode')

IF !SEEK(EVALUATE(loFormSet.lcTmpLine+'.Style')+;
    EVALUATE(loFormSet.lcTmpLine+'.cWareCode'),loFormSet.lcTemLoc)
  SCAN WHILE STYLE+COLOR+CWARECODE+CLOCATION = EVALUATE(loFormSet.lcTmpLine+'.Style')+;
      SPACE(6)+EVALUATE(loFormSet.lcTmpLine+'.cWareCode')
    SCATTER MEMVAR
    SELECT (loFormSet.lcTemLoc)
    IF !SEEK(EVALUATE(loFormSet.lcTmpLine+'.Style')+;
        EVALUATE(loFormSet.lcTmpLine+'.cWareCode')+WHsLoc.CLOCATION)
      APPEND BLANK
      GATHER MEMVAR
    ENDIF

  ENDSCAN
ENDIF

DECLARE laSource[1],laTarget[1]
STORE ' ' TO laSource,laTarget
lsSource = 1
SELECT cLocation FROM WHSLOC ;
  WHERE STYLE+COLOR+cWareCode == SPACE(19)+SPACE(6)+;
  EVALUATE(loFormSet.lcTmpLine+'.cWareCode');
  INTO ARRAY laSource

SELECT cLocation FROM (loFormSet.lcTemLoc) ;
  WHERE STYLE+cWareCode+cLocation = EVALUATE(loFormSet.lcTmpLine+'.Style')+;
  EVALUATE(loFormSet.lcTmpLine+'.cWareCode');
  INTO ARRAY laTarget

=gfMover(@laSource,@laTarget,LANG_POSTREC_AssignBins,.T.,'mvLoc',.F.,.F.,loFormSet)
SELECT (loFormSet.lcTemLoc)

DELETE FOR STYLE+cWareCode+cLocation = EVALUATE(loFormSet.lcTmpLine+'.Style')+;
  EVALUATE(loFormSet.lcTmpLine+'.cWareCode')

FOR I = 1 TO ALEN(laTarget)
  APPEND BLANK
  IF SEEK (EVALUATE(loFormSet.lcTmpLine+'.Style')+SPACE(6)+;
      EVALUATE(loFormSet.lcTmpLine+'.cWareCode')+laTarget[i],'WHsLoc')
    TABLEUPDATE(0,.T.)
  ENDIF
  REPLACE STYLE     WITH EVALUATE(loFormSet.lcTmpLine+'.Style');
    CWARECODE WITH EVALUATE(loFormSet.lcTmpLine+'.cWareCode');
    CLOCATION WITH laTarget[i]
ENDFOR
loFormSet.AriaForm1.chkBins.VALUE = IIF(!EMPTY(laTarget) ,.T.,.F.)

RETURN

*!*************************************************************
*! Name      : lfvLoc
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To validate the bins in the mover when selected.
*!*************************************************************
*! Parameters: lnOption : Selected option, lnSource: Source #,loFormSet : FormSet
*!*************************************************************
FUNCTION lfvLoc
LPARAMETERS lnOption, lnSource, loFormSet
DO CASE
CASE lnOption=1
  lcLocatin = laSource[lsSource]
  IF !SEEK(EVALUATE(loFormSet.lcTmpLine+'.Style')+SPACE(6)+;
      EVALUATE(loFormSet.lcTmpLine+'.cWareCode')+lcLocatin,"WhsLoc")
    *--Bin XXX is not assigned to '+'style XXX in location XXX,'\!\<Assign;\<Cancel
    IF gfModalGen('TRM42064B42007','DIALOG',ALLTRIM(lcLocatin)+'|'+;
        ALLTRIM(EVALUATE(loFormSet.lcTmpLine+'.Style'))+'|'+;
        ALLTRIM(EVALUATE(loFormSet.lcTmpLine+'.cWareCode'))) = 1
      RETURN .T.
    ELSE
      RETURN .F.
    ENDIF
  ELSE
    RETURN .T.
  ENDIF

CASE lnOption=2
  FOR I=1 TO ALEN(laSource,1)
    lcLocatin = laSource[I]
    IF !SEEK(EVALUATE(loFormSet.lcTmpLine+'.Style')+SPACE(6)+;
        EVALUATE(loFormSet.lcTmpLine+'.cWareCode')+lcLocatin,"WhsLoc")
      *--One or more Bin(s) are not assigned to '+'style XXX in location XXX,'\!\<Assign;\<Cancel
      IF gfModalGen('TRM42092B42007','DIALOG',ALLTRIM(EVALUATE(loFormSet.lcTmpLine+'.Style'))+;
          '|'+ALLTRIM(EVALUATE(loFormSet.lcTmpLine+'.cWareCode'))) = 1
        RETURN .T.
      ELSE
        RETURN .F.
      ENDIF
    ENDIF
  ENDFOR
CASE (lnOption=3 OR lnOption=4)
ENDCASE
RETURN

*!*************************************************************
*! Name      : lfvNewLn
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To validate the new button.
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfvNewLn
LPARAMETERS loFormSet

*-- Remove the control source of the edit region objects
=lfCntrSour(loFormSet,.F.)
=lfReadLine(loFormSet,.T.)
*-- To disable the edit region
=lfObjStatus(loFormSet,.F.)
loFormSet.lnWare = 0
loFormSet.AriaForm1.cboLocations.REFRESH
IF loFormSet.llMFCall AND loFormSet.lcPType = 'M'

  lnSelAls = SELECT(0)
  *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
  *SELECT (lcTmpLine)
  SELECT (loFormSet.lcTmpLine)
  *N038893,1 WAM 06/02/2005 (End)
  IF EMPTY(PADR(cRcvBy,loFormSet.lnMjrWid))
    *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
    *loFormSet.AriaForm1.kbItem.keyTextBox.Value = ""
    loFormSet.AriaForm1.kbItem.VALUE = ""
    *N038893,1 WAM 06/02/2005 (End)
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
    *loFormSet.AriaForm1.kbPONo.SetFocus
    IF !loFormSet.llSilentMod
      loFormSet.AriaForm1.kbPONo.SETFOCUS
    ENDIF
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

  ELSE
    *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
    *loFormSet.AriaForm1.kbItem.keyTextBox.Value = cRcvBy
    loFormSet.AriaForm1.kbItem.VALUE = cRcvBy
    *N038893,1 WAM 06/02/2005 (End)
    loFormSet.AriaForm1.kbPONo.KeyTextBox.VALUE = ""
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
    *loFormSet.AriaForm1.kbItem.SetFocus
    IF !loFormSet.llSilentMod
      loFormSet.AriaForm1.kbItem.SETFOCUS
    ENDIF
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

  ENDIF
  loFormSet.AriaForm1.kbItem.ENABLED = .T.
  SELECT (lnSelAls)
ENDIF

loFormSet.AriaForm1.kbPONo.ENABLED = .T.
*! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
*loFormSet.AriaForm1.kbPONo.SetFocus
IF !loFormSet.llSilentMod
  loFormSet.AriaForm1.kbPONo.SETFOCUS
ENDIF
*! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
loFormSet.AriaForm1.kbPONo.keytextbox.oldvalue = ''
RETURN

*!*************************************************************
*! Name      : lfvRemLn
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To validate the remove button
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfvRemLn
LPARAMETERS loFormSet
LOCAL lcLKey, lcDyelot, lcWareCode, lcScanVar, lcScanVal,;
  lnCurLine, lcCarton, lcTCode, lcStyle

*-Are you sure you want to delete this line?
IF gfModalGen('QRM34036B42002','DIALOG') = 1

  IF loFormSet.lcPType $ 'ISMD'
    PRIVATE laRemoved
    DECLARE laRemoved[8]
    laRemoved = 0
  ENDIF

  SELECT (loFormSet.lcTmpLine)
  SET FILTER TO
  *B000109,1 WAM 03/05/2005 Remove Lock PO header
  lcLckPo  = Po
  lcLckKey = cBusDocu+cStyType+PO
  *B000109,1 WAM 03/05/2005 (End)
  lcLKey = SPACE(3)+Po+STYLE+Dyelot+PADR(cWareCode,6)+IIF(loFormSet.llMFCall,'',STR(LINENO,6))

  *-- Check if the there is cancelled quantities
  IF SEEK('5'+lcLKey)
    IF loFormSet.lcPType $ 'ISMD'
      =lfEvalRemQ()
    ENDIF
    DELETE

  ENDIF
  *-- Check if the there is damaged or 2nd quality quantities
  IF SEEK('4'+lcLKey)
    SCAN REST WHILE Trancd+cCarton+Po+STYLE+Dyelot+;
        PADR(cWareCode,6)+IIF(loFormSet.llMFCall,'',STR(LINENO,6))='4'+lcLKey
      IF loFormSet.lcPType $ 'ISMD'
        =lfEvalRemQ()
      ENDIF
      DELETE

    ENDSCAN

  ENDIF

  *-- Check if there is receive to stock quanities
  IF SEEK('2'+lcLKey)
    IF loFormSet.lcPType $ 'ISMD'
      =lfEvalRemQ()
    ENDIF
    DELETE
  ENDIF

  IF SEEK('1'+lcLKey)
    loFormSet.lnTotStk = loFormSet.lnTotStk - TotStk
    loFormSet.lnTotDam = loFormSet.lnTotDam - TotDam
    loFormSet.lnTotCan = loFormSet.lnTotCan - TotCan

    IF loFormSet.lcPType $ 'ISMD'
      lnCurLine  = LINENO
      lcCarton   = cCarton
      lcTCode    = PO
      lcStyle    = STYLE
      lcDyelot   = Dyelot
      lcWareCode = cWareCode
      lcScanVar  = [TranCd+cCarton+Po+Style+Dyelot+cWareCode+STR(LineNo,6)]
      lcScanVal  = ['1' + lcCarton + lcTCode + lcStyle]

      SCAN FOR EVALUATE(lcScanVar) = EVALUATE(lcScanVal) AND ;
          STR(LINENO,6) = STR(lnCurLine,6)  AND ;
          Dyelot+cWareCode # lcDyelot+lcWareCode

        FOR lnI = 1 TO 8
          lcZ = STR(lnI,1)
          REPLACE QTY&lcZ WITH QTY&lcZ + laRemoved[lnI] ,;
            TOTQTY  WITH TOTQTY  + laRemoved[lnI]
        ENDFOR
        REPLACE TOTBAL WITH TOTQTY - TOTSTK - TOTDAM - TOTCAN
      ENDSCAN
      = SEEK('1'+lcLKey)

    ENDIF
    DELETE
  ENDIF

  GO TOP
  IF EOF()
    =lfReadLine(loFormSet,.T.)
    *-- To disable the edit region
    =lfObjStatus(loFormSet,.F.)
  ELSE
    =lfReadLine(loFormSet,.F.)
  ENDIF
  IF !(loFormSet.lcPType $ 'SCUF')
    loFormSet.AriaForm1.cmdNew.ENABLED  = .T.
  ENDIF

  *B000109,1 WAM 03/05/2005 Remove Lock PO header
  IF .F.
    IF !SEEK(lcLckPo,loFormSet.lcTmpLine,'TmpLine3')
      lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'',.T.)
      IF TYPE('lcTranCode') = 'N'
        =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
        llAbort=.T.
        EXIT
      ELSE
        lcSelString = "UPDATE POSHDR SET lLok_stat =0,cLok_User= '', dLok_Date='',cLok_Time='' WHERE cBusDocu+cStyType+PO='"+lcLckKey+"'"
        lnResult = oAriaApplication.RemoteCompanyData.Execute(lcSelString,'',"SAVEFILE","",lcTranCode ,4,'',SET("DataSession"))
        IF lnResult <=0
          =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLUPDATE",lnResult,.T.)
          =oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
        ELSE
          =oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
        ENDIF
      ENDIF
    ENDIF
    SELECT (loFormSet.lcTmpLine)
    GO TOP
  ENDIF
  *B000109,1 WAM 03/05/2005 (End)

ENDIF
SELECT (loFormSet.lcTmpLine)
SET FILTER TO TranCd = '1'

*B999999,1 AMH Fix bug of PO lines duplicated [Start]
loFormSet.AriaForm1.grdReceivingLines.REFRESH
*B999999,1 AMH [End]

RETURN

*!*************************************************************
*! Name      : lfEvalRemQ
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To evaluate remove quantity to add it to another
*!             equavilent lines if found.
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfEvalRemQ
LOCAL lnI, lcI
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  laRemoved[lnI] = laRemoved[lnI] + Qty&lcI
ENDFOR
*-- end of lfEvalRemQ.

*!*************************************************************
*! Name      : lfvRefer
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To validate the reference filed
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfvRefer
LPARAMETERS loFormSet
LOCAL lcKey, lnRecNo

IF loFormSet.AriaForm1.txtreference.VALUE = loFormSet.AriaForm1.txtreference.OldValue
  RETURN
ENDIF
loFormSet.AriaForm1.txtreference.CONTROLSOURCE = ""
SELECT (loFormSet.lcTmpLine)
lcKey = cCarton+Po+STYLE+Dyelot+PADR(cWareCode,6)+STR(LINENO,6)

SET FILTER TO
lnRecNo = RECNO()
SET ORDER TO TAG TmpLine2
SEEK lcKey

REPLACE REST REFERENCE WITH loFormSet.AriaForm1.txtreference.VALUE ;
  WHILE cCarton+Po+STYLE+Dyelot+PADR(cWareCode,6)+STR(LINENO,6)= lcKey

SET ORDER TO TAG TmpLine1
SET FILTER TO TranCd = '1'
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF
loFormSet.AriaForm1.txtreference.CONTROLSOURCE = loFormSet.lcTmpLine + '.Reference'

RETURN

*!*************************************************************
*! Name      : lfvRate
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To validate the rate
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfvRate
LPARAMETERS loFormSet, lnRateNo
LOCAL lcKey, lnRecNo

IF (lnRateNo = 1 AND loFormSet.AriaForm1.txtPriceRate.VALUE <=0 ) OR ;
    (lnRateNo = 2 AND loFormSet.AriaForm1.txtDutyRate.VALUE <=0 )
  *--Cannot accept zero or -ve rates.
  = gfModalGen('TRM34081B42000','DIALOG')
  IF lnRateNo = 1
    loFormSet.AriaForm1.txtPriceRate.VALUE = loFormSet.AriaForm1.txtPriceRate.OldValue
  ELSE
    loFormSet.AriaForm1.txtDutyRate.VALUE  = loFormSet.AriaForm1.txtDutyRate.OldValue
  ENDIF
  RETURN 0
ENDIF

SELECT (loFormSet.lcTmpLine)
lcKey   = cCarton+Po+STYLE+Dyelot+PADR(cWareCode,6)+STR(LINENO,6)
lnRecNo = RECNO()
SET FILTER TO

IF lnRateNo  = 1 AND loFormSet.AriaForm1.txtPriceRate.VALUE <> loFormSet.AriaForm1.txtPriceRate.OldValue
  SET ORDER TO TAG TmpLine2
  SEEK lcKey

  SCAN REST WHILE cCarton+Po+STYLE+Dyelot+PADR(cWareCode,6)+STR(LINENO,6) = lcKey
    =lfGetEqv('1',loFormSet.AriaForm1.txtPriceRate.VALUE,0,loFormSet.lnCurrUnt1,0,;
      nFLanCost1,0,0,0,0,0,0,loFormSet)
    REPLACE nLanPrRat  WITH loFormSet.AriaForm1.txtPriceRate.VALUE,;
      nLan_Cost1 WITH loFormSet.laECost[1]
  ENDSCAN
  SET ORDER TO TAG TmpLine1
ENDIF

IF lnRateNo  = 2 AND loFormSet.AriaForm1.txtDutyRate.VALUE <> loFormSet.AriaForm1.txtDutyRate.OldValue
  SET ORDER TO TAG TmpLine2
  SEEK lcKey
  SCAN REST WHILE cCarton+Po+STYLE+Dyelot+PADR(cWareCode,6)+STR(LINENO,6)=lcKey
    =lfGetEqv('234567',0,loFormSet.AriaForm1.txtDutyRate.VALUE,0,loFormSet.lnCurrUnt2,0,;
      nFLanCost2,nFLanCost3,nFLanCost4,nFLanCost5,nFLanCost6,nFLanCost7,loFormSet)

    REPLACE nLanDuRat WITH loFormSet.AriaForm1.txtDutyRate.VALUE
    GATHER FROM loFormSet.laECost FIELDS nLan_Cost2,nLan_Cost3,nLan_Cost4,nLan_Cost5,;
      nLan_Cost6,nLan_Cost7
  ENDSCAN
  SET ORDER TO TAG TmpLine1
ENDIF
SELECT (loFormSet.lcTmpLine)
SET FILTER TO TranCd = '1'

IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF

RETURN

*!*************************************************************
*! Name      : lfGetEqv
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To get the equivelant costs
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
FUNCTION lfGetEqv
LPARAMETERS lcUpdCsts,lnPRate1,lnDRate2,lnCurUnt1,lnCurUnt2,;
  lnFCost1,lnFCost2,lnFCost3,lnFCost4,lnFCost5,lnFCost6,lnFCost7,loFormSet
IF TYPE('loFormSet') = 'O'
  DIMENSION loFormSet.laECost[LEN(lcUpdCsts)]
ELSE
  DIMENSION laECost[LEN(lcUpdCsts)]
ENDIF

lnPt = 1
IF '1' $ lcUpdCsts
  IF TYPE('loFormSet') = 'O'
    loFormSet.laECost[lnPt] = lfvEquCost('1',lnFCost1,lnPRate1,lnCurUnt1,loFormSet)
  ELSE
    laECost[lnPt] = lfvEquCost('1',lnFCost1,lnPRate1,lnCurUnt1,.F.,lcPriceCur,'')
  ENDIF
  lnPt = lnPt + 1
ENDIF
IF '2' $ lcUpdCsts
  IF TYPE('loFormSet') = 'O'
    loFormSet.laECost[lnPt] = lfvEquCost('2',lnFCost2,lnDRate2,lnCurUnt2,loFormSet)
  ELSE
    laECost[lnPt] = lfvEquCost('2',lnFCost2,lnDRate2,lnCurUnt2,.F.,'',lcDutyCur)
  ENDIF
  lnPt = lnPt + 1
ENDIF
IF '3' $ lcUpdCsts
  IF TYPE('loFormSet') = 'O'
    loFormSet.laECost[lnPt] = lfvEquCost('3',lnFCost3,lnDRate2,lnCurUnt2,loFormSet)
  ELSE
    laECost[lnPt] = lfvEquCost('3',lnFCost3,lnDRate2,lnCurUnt2,.F.,'',lcDutyCur)
  ENDIF

  lnPt = lnPt + 1
ENDIF
IF '4' $ lcUpdCsts
  IF TYPE('loFormSet') = 'O'
    loFormSet.laECost[lnPt] = lfvEquCost('4',lnFCost4,lnDRate2,lnCurUnt2,loFormSet)
  ELSE
    laECost[lnPt] = lfvEquCost('4',lnFCost4,lnDRate2,lnCurUnt2,.F.,'',lcDutyCur)
  ENDIF
  lnPt = lnPt + 1
ENDIF
IF '5' $ lcUpdCsts
  IF TYPE('loFormSet') = 'O'
    loFormSet.laECost[lnPt] = lfvEquCost('5',lnFCost5,lnDRate2,lnCurUnt2,loFormSet)
  ELSE
    laECost[lnPt] = lfvEquCost('5',lnFCost5,lnDRate2,lnCurUnt2,.F.,'',lcDutyCur)
  ENDIF
  lnPt = lnPt + 1
ENDIF
IF '6' $ lcUpdCsts
  IF TYPE('loFormSet') = 'O'
    loFormSet.laECost[lnPt] = lfvEquCost('6',lnFCost6,lnDRate2,lnCurUnt2,loFormSet)
  ELSE
    laECost[lnPt] = lfvEquCost('6',lnFCost6,lnDRate2,lnCurUnt2,.F.,'',lcDutyCur)
  ENDIF
  lnPt = lnPt + 1
ENDIF
IF '7' $ lcUpdCsts
  IF TYPE('loFormSet') = 'O'
    loFormSet.laECost[lnPt] = lfvEquCost('7',lnFCost7,lnDRate2,lnCurUnt2,loFormSet)
  ELSE
    laECost[lnPt] = lfvEquCost('7',lnFCost7,lnDRate2,lnCurUnt2,.F.,'',lcDutyCur)
  ENDIF
  lnPt = lnPt + 1
ENDIF

RETURN


*!*************************************************************
*! Name      : lfvEquCost
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To get the equivelant costs by passing teh foreign
*!             costs
*!*************************************************************
*! Parameters: lcCstNo   : Cost elemement type
*!			   lnFrnCost : Foreign cost
*!			   lnCurRate : Currency rate
*!			   lnCurUnt  : Currency unit
*!			   loFormSet : FormSet
*!*************************************************************
FUNCTION lfvEquCost
LPARAMETERS lcCstNo,lnFrnCost,lnCurRate,lnCurUnt,loFormSet,;
  lcPriceCur,lcDutyCur
LOCAL lcCstType
lcCstType = IIF(TYPE('loFormSet') = 'O',loFormSet.lcIType&lcCstNo,lcIType&lcCstNo)
IF lcCstType $ 'PMD'
  STORE '' TO lcPMethod,lcPUnMeth,lcDMethod,lcDUnMeth
  IF lcCstType='P'
    lcPMethod = gfGetExSin(@lcPUnMeth,IIF(TYPE('loFormSet')='O',loFormSet.lcCur1,lcPriceCur))
    lcPMethod = IIF(EMPTY(lcPMethod),'*',lcPMethod)
    lcPUnMeth = IIF(EMPTY(lcPUnMeth),'/',lcPUnMeth)
    lnEquCost = lnFrnCost &lcPMethod lnCurRate &lcPUnMeth lnCurUnt
  ELSE
    lcDMethod = gfGetExSin(@lcDUnMeth,IIF(TYPE('loFormSet')='O',loFormSet.lcCur2,lcDutyCur))
    lcDMethod = IIF(EMPTY(lcDMethod),'*',lcDMethod)
    lcDUnMeth = IIF(EMPTY(lcDUnMeth),'/',lcDUnMeth)
    lnEquCost = lnFrnCost &lcDMethod lnCurRate &lcDUnMeth lnCurUnt
  ENDIF
ELSE
  lnEquCost = lnFrnCost
ENDIF
*: E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
*lnEquCost = ROUND(lnEquCost,2)
lnEquCost = ROUND(lnEquCost,3)
*: E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
RELEASE lcCstType,lcPMethod,lcPUnMeth,lcDMethod,lcDUnMeth
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]

RETURN (lnEquCost)

*!*************************************************************
*! Name      : lfWOrdBOM
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To get the work order bill of materials
*!*************************************************************
*! Parameters: lcSqlStat   : Passing SQL statement.
*!             lcTranNo    : Work Order No.
*!             llcTktBom   : .T. to get records from cTktBom
*!             llBomLine   : .T. to get records from BomLine
*!             llBomCost   : .T. to get records from BomCost
*!             llMfgOprH   : .T. to get records from MfgOprHd
*!             loFormSet   : FormSet
*!*************************************************************
FUNCTION lfWOrdBOM
LPARAMETERS  lcSqlStat, llcTktBom, llBomLine, llBomCost, llMfgOprHd,loFormSet

*-- Getting the cTktBom for the selected PO
IF llcTktBom
  =lfOpenSql(lcSqlStat,'CTKTBOM',loFormSet.lcCTktBom, "","",.F.)
ENDIF

*-- Getting the BomLine for the selected PO
IF llBomLine
  =lfOpenSql(lcSqlStat,'BOMCOST',loFormSet.lcBomCost, "","",.F.)
ENDIF

*-- Getting the MfgOprHd for the selected PO
IF llMfgOprHd
ENDIF

RETURN

*!*************************************************************
*! Name      : lfGetBOM
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To check if the selected style has a style cost sheet
*!*************************************************************
*! Parameters: loFormSet   : FormSet
*!             lcCstShtType: "I" - Style PO styles
*!					       : "M" - Manufactured Styles
*!					       : "F" - Material Cost sheet
*!					       : "N" - Inter Location PO Cost sheet
*!             lcStyMajor  : Style or Fabric
*!             lGetBom     : Wether to get the BOM or not

*!*************************************************************
FUNCTION lfGetBOM
LPARAMETERS loFormSet, lcCstShtType, lcStyMajor, lGetBom
LOCAL lcSqlStatement

lcSqlStatement  = "SELECT * FROM BOMHEADR (INDEX=BOMHEADR) " + ;
  " WHERE cInvType ='" + loFormSet.lcInvType + ;
  "' AND cItmMajor = '" + lcStyMajor + ;
  "' AND cCstShtTyp='"+ lcCstShtType + "'"

=lfOpenSql(lcSqlStatement,'BOMHEADR',loFormSet.lcBomHdr, "","",.F.)
SELECT (loFormSet.lcBomHdr)
LOCATE
RETURN !EOF()

*!*	IF lGetBom
*!*	  This.mopenwrkordsql('This.oBomCon','BOM',This.cbom,;
*!*	  "cItmMajor = '" + lcStyMajor + "'" + " AND cCstShtTyp ='" + lcCstShtType + "'")

*!*	ENDIF

*!********************************************************************************
*! Name      : lfOpenSql
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To open the SQL files
*!********************************************************************************
*! Parameters: lcSqlStatment : Pass the SQL statement
*!			   lcTable		 : Table to select from
*!			   lcCursor		 : Cursor to select into
*!			   lcSqlIndex    : Table index
*!			   lcWhereCond   : Where condition
*!			   llIsInitial   : .T. the table will be the initial selected alias.
*!********************************************************************************
FUNCTION lfOpenSql
LPARAMETERS lcSqlStatment,lcTable,lcCursor,lcSqlIndex,lcWhereCond,llIsInitial

LOCAL lnConnectionHandlar, lnBuffering

IF TYPE('lcSqlIndex') <> 'C'
  lcSqlIndex=""
ENDIF

IF TYPE("lcSqlStatment") <> 'C'
  lcSqlStatment = "SELECT * FROM " + lcTable + lcSqlIndex +" WHERE "+lcWhereCond
ENDIF

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,lcCursor,lcTable,;
  oAriaApplication.ActiveCompanyConStr,3,'SAVE',SET("DataSession"))

IF lnConnectionHandlar = 1
  IF llIsInitial
    loFormSet.DATAENVIRONMENT.INITIALSELECTEDALIAS = lcCursor
  ENDIF
ELSE
  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
RELEASE lnConnectionHandlar, lnBuffering
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]
*!*************************************************************
*! Name      : lfOpenFox
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To open the Fox files remotely
*!*************************************************************
*! Parameters: lcSqlStatment : Pass the SQL statement
*!			   lcTable		 : Table to select from
*!			   lcCursor		 : Cursor to select into
*!			   lcWhereCond   : Where condition
*!*************************************************************
FUNCTION lfOpenFox
LPARAMETERS lcSqlStatment,lcTable,lcCursor,lcWhereCond
LOCAL lnConnectionHandlar


IF TYPE("lcSqlStatment") <> 'C'
  lcSqlStatment = "SELECT * FROM " + lcTable +" WHERE "+lcWhereCond
ENDIF

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,lcCursor,lcTable,;
  oAriaApplication.cAriaNativeDataFilesConStr,3,'SAVE',SET("DataSession"))

IF lnConnectionHandlar <> 1
  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
RELEASE lnConnectionHandlar
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]
*!*************************************************************
*! Name      : lfSetIndex
*! Developer : Khalid Mohi El-Din
*! Date      : 09/11/2004
*! Purpose   : function to Create index in table.
*!*************************************************************
FUNCTION lfSetIndex
LPARAMETERS lcTable,laIndex

LOCAL lnBuffering
lnBuffering = CURSORGETPROP("Buffering",lcTable)
=TABLEUPDATE(.T.,.T.,lcTable)
=CURSORSETPROP("Buffering",3,lcTable)
SELECT (lcTable)
IF !EMPTY(laIndex)
  FOR lnI = 1 TO ALEN(laIndex,1)
    lcIndex = laIndex[lnI,1]
    lcTag   = laIndex[lnI,2]
    *B607881,1 WAM 12/17/2006 Fix Error "File is in use" when two users open the screen
    *INDEX ON &lcIndex. TAG (lcTag) OF (lcTable)
    INDEX ON &lcIndex. TAG (lcTag)
    *B607881,1 WAM 12/17/2006 (End)
  ENDFOR
  lcTag = laIndex[1,2]
  SET ORDER TO TAG (lcTag)
ENDIF
=CURSORSETPROP("Buffering",lnBuffering,lcTable)
*--end of lfSetIndex.
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
RELEASE lnBuffering,lnI ,lcIndex ,lcTag
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]
*!***************************************************************************
*! Name      : lfBefSave
*! Developer : Khalid Mohi El-Din
*! Date      : 09/11/2004
*! Purpose   : function to perform validation before saving.
*!***************************************************************************
*! Parameters: lcRecvType : 'I' Receive P/O
*!                          'S' Receive by Shipment
*!                          'B' Receive P/O Batch
*!                          'R' Issue Return P/o
*!                          'M' Receive C/T
*!                          'T' Receive C/T Batch
*!                          'N' Issue Inter-Location P/o
*!                          'U' Issue Inter Location P/O Shipment
*!                          'H' Issue Inter Location P/O Batch'
*!                          'O' Receive Inter-Location P/o
*!                          'C' Receive Inter Location P/O Shipment
*!                          'L' Receive Inter Location P/O Batch'
*!                          'D' Receive Dye Order
*!                          'A' Issue Adornment order
*!                          'E' Receive Adornment order
*!                          'P' Receive Material PO
*!                          'F' Receive Material PO Shipment
*!             lcTmpLine  : Receiving File
*!             llFromEdi  : If called from EDI
*!********************* ******************************************************
FUNCTION lfBefSave

*! B125565,1 WSH 04/11/2005, Add Parameter to get FormSet Reference. [Start]
*LPARAMETERS lcRecvType, lcTmpLine, llFromEdi
LPARAMETERS loFormSet, lcRecvType, lcTmpLine, llFromEdi
*! B125565,1 WSH 04/11/2005, [End]

*T20060818.0001,10/C200876 TMI 05/01/2008 [Start] check PO before save
IF ASCAN(loFormSet.laEvntTrig,PADR('DLCHKSAV',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0 .AND. ;
    !loFormSet.mDoTrigger(PADR('DLCHKSAV',10))
  RETURN .F.
ENDIF
*T20060818.0001,7 TMI [End  ]


*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [START]
*N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
*IF loFormset.lcInvType = '0002' AND lcRecvType $ 'PF' AND loFormSet.llTrkRolls
IF loFormset.lcInvType = '0002' AND lcRecvType $ 'WPF' AND loFormSet.llTrkRolls
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
  lenClrLen  = LEN(gfitemmask("PN", "", loFormset.lcInvType))
  SELECT(lcTmpLine)
  SCAN FOR (TotStk > 0 AND TRANCD <> '4')
    *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
    *lcSeekKey = SUBSTR(Style,1,7)+RIGHT(Style,lenClrLen)
    lcSeekKey = STYLE
    *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
    =SEEK(loFormSet.lcInvType+STYLE,loFormSet.lcTmpItem)
    IF EVALUATE(loFormSet.lcTmpItem+'.LTRKROLLS')
      llRoll = .T.
      IF !EMPTY(loFormSet.lcTmpRoll) AND USED(loFormset.lcTmpRoll)
        SELECT(loFormSet.lcTmpRoll)
        lcTmpRTag = ORDER()
        SET ORDER TO lcTmpRoll2
        IF !SEEK(lcSeekKey+&lcTmpLine..cWarecode+&lcTmpLine..Dyelot,loFormSet.lcTmpRoll) OR;
            (SEEK(lcSeekKey+&lcTmpLine..cWarecode+&lcTmpLine..Dyelot,loFormSet.lcTmpRoll) AND NAPPLY = 0)
          SET ORDER TO &lcTmpRTag
          =gfModalGen('TRM36096B36000','ALERT','rolls')
          SELECT(lcTmpLine)
          RETURN .F.

        ELSE
          lnTotApp = 0
          SELECT (loFormSet.lcTmpRoll)
          *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
          *SUM nApply REST WHILE ;
          cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6) = lcSeekKey+&lcTmpLine..cWarecode+&lcTmpLine..Dyelot;
          TO lnTotApp
          SUM nApply REST WHILE ;
            STYLE+cWareCode+cDyelot+STR(LINENO,6) = lcSeekKey+&lcTmpLine..cWarecode+&lcTmpLine..Dyelot;
            TO lnTotApp
          *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
          IF lnTotApp <> &lcTmpLine..TotStk
            =gfModalGen('TRM36096B36000','ALERT','rolls')
            SELECT(lcTmpLine)
            RETURN .F.
          ENDIF

        ENDIF
      ELSE
        =gfModalGen('TRM36096B36000','ALERT','rolls')
        SELECT(lcTmpLine)
        RETURN .F.
      ENDIF
    ENDIF
  ENDSCAN
ENDIF

llRoll  = .F.
IF loFormset.lcInvType = '0002' AND (loFormSet.llTrkRolls OR loFormSet.lcCostMthM $ 'L') AND lcRecvType $ 'G'
  SELECT(lcTmpLine)
  lenClrLen  = LEN(gfitemmask("PN", "", loFormset.lcInvType))
  SCAN FOR (TotStk > 0 AND TRANCD <> '4')

    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
    *lcSeekKey = SUBSTR(Style,1,7)+RIGHT(Style,lenClrLen)
    lcSeekKey = STYLE
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

    =SEEK(loFormSet.lcInvType+STYLE,loFormSet.lcTmpItem)
    IF EVALUATE(loFormSet.lcTmpItem+'.LTRKROLLS')
      llRoll = .T.
      IF !EMPTY(loFormset.lcTmpRoll) AND USED(loFormset.lcTmpRoll)
        SELECT(loFormset.lcTmpRoll)
        lcTmpRTag = ORDER()
        SET ORDER TO lcTmpRoll2
        *-- IF  We can't find the roll record
        *-- OR  We found it
        *-- AND The user didn't apply any quantity
        *--         Stop the Saving process
        IF !SEEK(lcSeekKey+&lcTmpLine..cWarecode+&lcTmpLine..Dyelot,loFormSet.lcTmpRoll)
          SET ORDER TO &lcTmpRTag
          =gfModalGen('TRM36096B36000','ALERT','rolls')
          SELECT(lcTmpLine)
          RETURN .F.
        ELSE
          = SEEK(lcSeekKey+&lcTmpLine..cWarecode+&lcTmpLine..Dyelot,loFormSet.lcTmpRoll)

          *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
          *!*	          LOCATE REST WHILE cfabric+ccolor+cwarecode+cdyelot+STR(lineno,6) ;
          *!*	                          = lcSeekKey + &lcTmpLine..cWarecode +  &lcTmpLine..Dyelot;
          *!*	                          FOR NAPPLY > 0
          LOCATE REST WHILE STYLE+cwarecode+cdyelot+STR(LINENO,6) ;
            = lcSeekKey + &lcTmpLine..cWarecode +  &lcTmpLine..Dyelot;
            FOR NAPPLY > 0
          *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
          IF !FOUND()
            SET ORDER TO &lcTmpRTag
            =gfModalGen('TRM36096B36000','ALERT','rolls')
            SELECT(lcTmpLine)
            RETURN .F.

          ELSE
            lnTotApp = 0
            SELECT (loFormSet.lcTmpRoll)
            *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
            *!*	            SUM nApply REST WHILE ;
            *!*	            cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6) = lcSeekKey+&lcTmpLine..cWarecode+&lcTmpLine..Dyelot;
            *!*	            TO lnTotApp
            SUM nApply REST WHILE ;
              STYLE+cWareCode+cDyelot+STR(LINENO,6) = lcSeekKey+&lcTmpLine..cWarecode+&lcTmpLine..Dyelot;
              TO lnTotApp
            *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
            IF lnTotApp <> &lcTmpLine..TotStk
              =gfModalGen('TRM36096B36000','ALERT','rolls')
              SELECT(lcTmpLine)
              RETURN .F.
            ELSE
              llRoll  = .T.
            ENDIF


          ENDIF
        ENDIF
      ELSE
        =gfModalGen('TRM36096B36000','ALERT','rolls')
        SELECT(lcTmpLine)
        RETURN .F.
      ENDIF
    ELSE
      IF loFormSet.lcCostMthM $ 'L'
        IF !EMPTY(loFormSet.lcTmpJour) AND USED(loFormSet.lcTmpJour)
          IF !SEEK(loFormset.lcInvType+&lcTmpLine..STYLE+&lcTmpLine..cWarecode+&lcTmpLine..Dyelot,loFormSet.lcTmpJour)
            =gfModalGen('TRM36096B36000','ALERT','lots')
            SELECT(lcTmpLine)
            RETURN .F.
          ENDIF
          SELECT(loFormSet.lcTmpJour)
          LOCATE REST WHILE cInvType+STYLE+cWareCode+cDyelot+cRSession+cISession = ;
            loFormset.lcInvType+&lcTmpLine..STYLE+&lcTmpLine..cWarecode+&lcTmpLine..Dyelot FOR nApply > 0
          IF !FOUND()
            =gfModalGen('TRM36096B36000','ALERT','rolls')
            SELECT(lcTmpLine)
            RETURN .F.
          ELSE
            lnTotApp = 0
            SELECT(loFormSet.lcTmpJour)
            SUM nApply REST WHILE  ;
              cInvType+STYLE+cWareCode+cDyelot+cRSession+cISession = ;
              loFormset.lcInvType+&lcTmpLine..STYLE+&lcTmpLine..cWarecode+&lcTmpLine..Dyelot ;
              TO lnTotApp

            IF lnTotApp <> &lcTmpLine..TotStk
              =gfModalGen('TRM36096B36000','ALERT','lots')
              SELECT(lcTmpLine)
              RETURN .F.
            ELSE
              llRoll  = .T.
            ENDIF
          ENDIF
        ELSE
          =gfModalGen('TRM36096B36000','ALERT','lots')
          SELECT(lcTmpLine)
          RETURN .F.
        ENDIF
      ELSE
        llRoll = .T.
      ENDIF
    ENDIF
  ENDSCAN
  IF !llRoll
    =gfModalGen('TRM36096B36000','ALERT',IIF(EVALUATE(loFormSet.lcTmpItem+'.LTRKROLLS'),'rolls','lots'))
    SELECT(lcTmpLine)
    RETURN .F.
  ENDIF
ENDIF
*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]


DO CASE
  *-- 'R' Issue Return P/o, 'M' Receive C/T, 'T' Receive C/T Batch
  *-- 'D' Receive Dye Order, 'A' Issue Adornment order, 'E' Receive Adornment order
  *-- 'G' Issue Material PO
CASE lcRecvType $ 'RMTDAEG'
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  *lnSel = gfModalGen('QRM34076B42010','DIALOG')
  lnSel = IIF(lcRecvType ='M' ,gfModalGen('QRM34076B38037','DIALOG'),gfModalGen('QRM34076B42010','DIALOG'))
  IF lcRecvType ='M'
    IF lnSel < 3
      loFormSet.Saveorpost = IIF(lnSel =1,'P','S')
    ELSE
      loFormSet.SaveOrPost = ''
    ENDIF
    lnSel = IIF(lnSel = 3,2,1)
  ELSE
    loFormSet.SaveOrPost = ''
  ENDIF
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  IF lnSel = 2
    RETURN .F.
  ELSE
    lnSel=3
  ENDIF

  *-- 'I' Receive P/O, 'S' Receive by Shipment, 'B' Receive P/O Batch
  *-- 'N' Issue Inter-Location P/O, 'O' Receive Inter-Location P/o
  *-- 'L' Receive Inter Location P/O Batch, 'H' Issue Inter Location P/O Batch
  *-- 'U' Issue Inter-Location P/O Shipment
  *-- 'P' Receive Material PO, 'F' Receive Material PO Shipment.
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
  *CASE lcRecvType $ 'ISBNOLHUPF'
CASE lcRecvType $ 'ISBNOLHUPFW'
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][End]
  *--Print reciepts log.
  *--Do you wish to print/view [reciepts/issues] log before posting all transactions?,
  *-- 'Print Log,Post,Cancel'
  *-- lnSel=1 View log
  *-- lnSel=2 print log
  *-- lnSel=3 post
  *-- lnSel=4 cancel
  lnSel = IIF(llFromEdi,3,0)
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  *DO WHILE lnSel < 3 &&-- after view/print do the same screen for print/view
  &&-- or post or cancel .
  *lnSel = gfModalGen('QRM34075B42009','DIALOG',IIF(lcRecvType $ 'NAHU','issues','receipts'))
  *IF lnSel=1 OR lnSel=2 &&-- if preview/print only
  DO WHILE IIF(lcRecvType $ 'IO',lnSel < 2,lnSel < 3) &&-- after view/print do the same screen for print/view
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
    *lnSel = gfModalGen(IIF(lcRecvType $ 'IO','QRM34075B34219','QRM34075B42009'),'DIALOG',IIF(lcRecvType $ 'NAHU','issues','receipts'))
    IF !loFormSet.llSilentMod
      lnSel = gfModalGen(IIF(lcRecvType $ 'IO','QRM34075B34219','QRM34075B42009'),'DIALOG',IIF(lcRecvType $ 'NAHU','issues','receipts'))
    ELSE
      lnSel = 3
      loFormSet.VISIBLE = .F.
    ENDIF
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

    IF lcRecvType $ 'IO'
      loFormSet.SaveOrPost = IIF(lnSel= 1 OR lnSel= 4,'',IIF(lnSel = 2,'P','S'))
    ELSE
      loFormSet.SaveOrPost = ''
    ENDIF
    IF lnSel=1 OR IIF(lcRecvType $ 'IO',.F.,lnSel=2) &&-- if preview/print only
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]



      *-- Start all modifications needed to call POSTYREC report
      *-- 1- Save the current alias
      *-- 2- Save the record pointer of STYLE  file
      *-- 3- Save the record pointer of POSHDR file
      *-- 4- Save the record pointer of POSLN  file
      *--    And save it's order then close it .
      *-- 5- Save the record pointer of TMPLINE file
      *--    And save it's order
      *-- 6- Creat the requierd index the POSTYREC report need for
      *--    this file [cstytype+po+style+STR(lineno,6)+trancd]
      *--    then use it again with alias POSLN
      *-- 7- define all variabels needed for POSTYREC report .
      *-- 8- Run the POSTYREC report .
      *-- 9- Restore all .

      *! B125565,1 WSH 04/11/2005, Adjust needed variables and files and call Receipt Log report. [Start]
      loFormSet.AriaForm1.LOCKSCREEN = .T.
      loFormSet.AriaForm1.grdReceivingLines.RECORDSOURCE = ""

      LOCAL lnCurAlias, lnNo, laFlds[1], lcCurrGrp, lcSqlStatement

      lnCurAlias  = SELECT(0)
      lcRPTitle   = ''
      llMultiWh   = loFormSet.llWareHous && not found
      llRPCostDt  = .F.
      llRPFrnCur  = .F.
      lcRpCurr    = 'F'
      lcRpSortBy  = IIF(loFormSet.lcPType = 'S', 'T', 'P')

      *-- If this program is called by parameter 'N' Issue Inter-Location P/O
      *-- or 'O' Receive Inter-Location P/o
      *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
      *!*	      lcInvType   = IIF(loFormSet.lcPType $ 'PF', '0002', '0001')
      *!*	      lcRPPoType  = IIF(loFormSet.lcPType $ "ON", "N", "A")
      *!*	      lcBusDocu   = IIF(loFormSet.lcPType $ "ON", "N", IIF(loFormSet.lcPType $ 'RG', 'R', 'P'))
      lcInvType   = IIF(loFormSet.lcPType $ 'PFW', '0002', '0001')
      lcRPPoType  = IIF(loFormSet.lcPType $ "ON", "N", IIF(loFormSet.lcPType = 'W',"F","A"))
      *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][End]
      lcBusDocu   = IIF(loFormSet.lcPType $ "ON", "N", IIF(loFormSet.lcPType $ 'RG', 'R', 'P'))
      lcRPPrice   = 'C'
      lcRpExp     = '.T.'
      lcMajTtl    = gfItemMask("HM", '', loFormSet.lcInvType)

      DO CASE
      CASE loFormSet.lcPType $ 'ISNUHOCL'
        lcOGWinTitl = LANG_POSTREC_POSTYREC
      CASE loFormSet.lcPType $ 'D'
        lcOGWinTitl = LANG_POSTREC_DYEORD
      OTHERWISE
        lcOGWinTitl = LANG_POSTREC_MAPOREC
      ENDCASE

      llMultCurr  = loFormSet.llMulCurr

      PRIVATE lcBomLine, loBomLine, loPosHdr, lcRepTemp, lcMastPoHd
      lcBomLine  = gfTempName()
      lcRepCurs  = gfTempName()
      lcMastPoHd = gfTempName()

      loBomLine = CREATEOBJECT('RemoteTable', 'BOMLINE', 'BOMLINE', lcBomLine, SET("Datasession"))
      loPosHdr  = CREATEOBJECT('RemoteTable', 'POSHDR', 'POSHDR', lcMastPoHd, SET("Datasession"))

      lnTmpPos = RECNO(loFormSet.lcTmpLine)
      lcTmpOrd = ORDER(loFormSet.lcTmpLine)

      = AFIELDS(laFlds, loFormSet.lcTmpLine)

      =gfCrtTmp(lcRepCurs, @laFlds, 'cstytype+po+style+STR(lineno,6)+TranCd', 'POSLN')
      SELECT (lcRepCurs)
      APPEND FROM (oAriaApplication.WorkDir + loFormSet.lcTmpLine)
      LOCATE

      lcBomPTyp = IIF(loFormSet.lcPType = 'S', 'S', 'I')

      IF (loFormSet.lcPType = 'I' OR loFormSet.lcPType = 'S' OR loFormSet.lcPType = 'B' OR loFormSet.lcPType = 'O' OR loFormSet.lcPType = 'L')
        SELECT (lcRepCurs)
        lcCurrGrp = cBusDocu + cStyType + cStyType
        =loPosHdr.SEEK(cBusDocu+cStyType+PO)
        SCAN FOR TranCd $ '42' AND TotQty <> 0;
            .AND. lfChekAdj(lcBomPTyp, Po + STR(LINENO,6), STYLE, IIF(loFormSet.lcPType = 'S', ShipNo, ''), '', cStyGrade)

          *B126833,1 WAM 04/03/2005 add new button to edit landed cost for styles has no detail costing
          *IF loFormSet.llImpCost OR !loFormSet.llEditLCst
          IF lDetCost OR !loFormSet.llEditLCst
            *B126833,1 WAM 04/03/2005 (End)
            IF EMPTY(lcCurrGrp) OR lcCurrGrp <> cBusDocu + cStyType + cStyType
              =loPosHdr.SEEK(cBusDocu+cStyType+PO)
              lcCurrGrp = cBusDocu + cStyType + cStyType
            ENDIF
            =lfPrnLanded(loFormSet)
          ENDIF

        ENDSCAN
      ENDIF

      loBomLine = .NULL.
      loPOSHdr  = .NULL.
      IF USED(lcMastPoHd)
        USE IN (lcMastPoHd)
      ENDIF
      USE IN (lcRepCurs)

      *-- Run Report
      DECLARE laWareCodes[1]
      STORE "" TO laWareCodes

      FOR lnNo = 1 TO ALEN(laWareCodes,1)
        IF loFormSet.lcPType = "O" AND lnSel = 2
          lcRpExp   = "cWareCode = '"+ laWareCodes[lnNo] + "'"
          lcRPTitle = "Issue Inter Location To " + laWareCodes[lnNo]
        ENDIF
        *MEDIA
        gcContCode = oAriaApplication.DefaultCountry
        *MEDIA
        *B608042,1 MMT 04/15/2007 fix bug of wrong dates while calling reports from receiving screen[Start]
        *DO (oAriaApplication.ReportHome + 'ItemRecv') WITH lcRepCurs, loFormSet.AriaForm1.cntShipment.kbShipNo.keytextbox.Value, (lnSel = 2)
        *! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[Start]
        IF FILE(oAriaApplication.clientreporthome+ 'ItemRecv.fxp')
          DO (oAriaApplication.clientreporthome+ 'ItemRecv') WITH lcRepCurs, loFormSet.AriaForm1.cntShipment.kbShipNo.keytextbox.VALUE, (lnSel = 2),loFormSet.ariaForm1.dtpickerReceivingDate.VALUE
        ELSE
          *! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[End]
          DO (oAriaApplication.ReportHome + 'ItemRecv') WITH lcRepCurs, loFormSet.AriaForm1.cntShipment.kbShipNo.keytextbox.VALUE, (lnSel = 2),loFormSet.ariaForm1.dtpickerReceivingDate.VALUE
          *! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[Start]
        ENDIF
        *! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[End]
        *B608042,1 MMT 04/15/2007 fix bug of wrong dates while calling reports from receiving screen[End]
      ENDFOR

      SELECT (loFormSet.lcTmpLine)
      IF lnTmpPos <> 0 AND lnTmpPos <= RECCOUNT()
        GOTO lnTmpPos
      ENDIF
      SET ORDER TO &lcTmpOrd

      SELECT (lnCurAlias)
      loFormSet.AriaForm1.grdReceivingLines.RECORDSOURCE = loFormSet.lcTmpLine
      loFormSet.AriaForm1.grdReceivingLines.ACTIVATECELL(1,1)
      =lfActBrow(loFormSet)
      loFormSet.AriaForm1.LOCKSCREEN = .F.
      *! B125565,1 WSH 04/11/2005, [End]

    ENDIF  &&-- Endif lnSel = 1 OR lnSel = 2
  ENDDO

  IF lnSel = 4  &&-- <Canel>
    RETURN .F.
  ENDIF
ENDCASE
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
IF loFormset.lcPType $ 'IOM' AND !EMPTY(loFormset.ariaFORM1.cntBATCH.kbBATCHNO.keYTEXTBOX.VALUE) AND loFormSet.Saveorpost = 'P'
  IF !lfCheckBatch(loFormset)
    RETURN .F.
  ENDIF
ENDIF
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
*!***************************************************************************
*! Name      : lfSaveRec
*! Developer : Khalid Mohi El-Din
*! Date      : 09/11/2004
*! Purpose   : function to receive the work orders
*!***************************************************************************
*! Parameters: lcInvType  : '0001' for styles, '0002' for materials
*!             lcRecvType : 'I' Receive P/O
*!                          'S' Receive by Shipment
*!                          'B' Receive P/O Batch
*!                          'R' Issue Return P/o
*!                          'M' Receive C/T
*!                          'T' Receive C/T Batch
*!                          'N' Issue Inter-Location P/o
*!                          'U' Issue Inter Location P/O Shipment
*!                          'H' Issue Inter Location P/O Batch'
*!                          'O' Receive Inter-Location P/o
*!                          'C' Receive Inter Location P/O Shipment
*!                          'L' Receive Inter Location P/O Batch'
*!                          'D' Receive Dye Order
*!                          'A' Issue Adornment order
*!                          'E' Receive Adornment order
*!                          'P' Receive Material PO
*!                          'F' Receive Material PO Shipment
*!             lcTmpLine  : Receiving File
*!             ldTrDate   : Posting date
*!             ldRecDate  : Receiving date
*!			   lcTempLoc  : Temporary bins file in case of keep track of bins
*!			   lcMastShp  : Temporary shipment file in case of receive by shipment
*!             llBarCode  : If called from temporary receiving by bar code
*!             llFromEdi  : If called from EDI
*!             llEditLCst : To get the costs from the file instead of getlanded
*!                          FUNCTION in case of edit cost per line was marked
*!             lnShipStk  : Receive to Stock in case of shipment
*!             lnShipCan  : Receive to Cancel in case of shipment
*!             lnShipDam  : Receive to Damage in case of shipment
*!***************************************************************************
FUNCTION lfSaveRec

*B128070,1 KHM 05/19/2005 Add 3 new paramters to hold stock, damage and cancelled quantity
*B128070,1 KHM 05/19/2005 in case of receive by shipment [Begin]
*PARAMETERS lcInvType, lcRecvType, lcTmpLine, ldTrDate, ldRecDate, lcTempLoc,;
lcMastShp, llBarCode, llFromEdi, llEditLCst

*: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
*PARAMETERS lcInvType, lcRecvType, lcTmpLine, ldTrDate, ldRecDate, lcTempLoc,;
lcMastShp, llBarCode, llFromEdi, llEditLCst, lnShipStk, lnShipCan, lnShipDam

PARAMETERS lcInvType, lcRecvType, lcTmpLine, ldTrDate, ldRecDate, lcTempLoc,;
  lcMastShp, llBarCode, llFromEdi, llEditLCst, lnShipStk, lnShipCan, lnShipDam,llAddBomLine
*: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]
*B128070,1 KHM 05/19/2005 [End]

*-- Check if no receiving or issuing has been done.
SELECT (lcTmpLine)
lnCrRec = IIF(EOF(),0,RECNO())
LOCATE FOR TranCd <> '1'
IF !FOUND()
  *--No [Receiving Lines/Issuing lines] was done,Cannot update.
  = gfModalGen('INM34061B42000','DIALOG',IIF(lcRecvType $ 'RNAHUG', 'issuing Line','receiving Line'))
  IF BETWEEN(lnCrRec,1,RECCOUNT())
    GOTO lnCrRec
  ENDIF
  RETURN .F.
ENDIF

*-- 'B' Receive P/O Batch 'L' Receive Inter Location P/O Batch'
IF lcRecvType $ 'BL'
  LOCATE FOR nLanPrRat = 0 OR nLanDuRat = 0
  IF FOUND()
    *--You cannot receive batch with zero rates.
    = gfModalGen('TRM34082B42000','DIALOG')
    IF BETWEEN(lnCrRec,1,RECCOUNT())
      GOTO lnCrRec
    ENDIF
    RETURN .F.
  ENDIF
ENDIF


*-- Get the necessary settings
PRIVATE laSetups  , llWareHous, llWareLoc, llDyelot, llFabDye, lcCostMthM, lcCostMth,;
  llLinkToGl, lcDropLoc , llPOSale  , llImpCost, llMulCurr, llUseMCurr,;
  llEditExRt, llConfig  , llMFCall, llUnblProc, llShpRec

DIMENSION laSetups[21,2]
laSetups[1,1]  = 'M_WareHouse'  &&-- Use multi location
laSetups[2,1]  = 'M_WareLoc'    &&-- Keep Track of bins
laSetups[3,1]  = 'M_Dyelot'     &&-- Use Dyelot (IC)
laSetups[4,1]  = 'M_MATDYE'     &&-- Use Dyelot (MA)
laSetups[5,1]  = 'M_COST_MET'   &&-- Costing Method (IC)
laSetups[6,1]  = 'M_MATCSTMT'   &&-- Costing Method (MA)
laSetups[7,1]  = 'M_Link_GL'    &&-- Link to GL
laSetups[8,1]  = 'M_DROPWARE'   &&-- Drop down location
laSetups[9,1]  = 'M_SYSTYPE'    &&-- System Type 'P' for Point of sale.
laSetups[10,1]  = 'M_LImpCost'  &&-- Use Detail Costing (PO)
laSetups[11,1] = 'llMulCurr'    &&-- Multi currency
laSetups[12,1] = 'llEditExRa'   &&-- Change exch. rates
laSetups[13,1] = 'M_cCostImp'   &&-- Receive Style PO by PO/Shipment
laSetups[14,1] = 'M_STYCNFG'    &&-- Use configuration
laSetups[15,1] = 'M_cIType1'    &&-- Imported Style - Cost element type 1
laSetups[16,1] = 'M_cIType2'	&&				    - Cost element type 2
laSetups[17,1] = 'M_cIType3'    && 				    - Cost element type 3
laSetups[18,1] = 'M_cIType4'	&&			        - Cost element type 4
laSetups[19,1] = 'M_cIType5'    &&					- Cost element type 5
laSetups[20,1] = 'M_cIType6'	&&					- Cost element type 6
laSetups[21,1] = 'M_cIType7'    &&					- Cost element type 7

=gfGetMemVar(@laSetups,oAriaApplication.ActiveCompanyID)

*-- Use multi location
llWareHous = (laSetups[1,2]='Y')
*-- Keep Track of bins
llWareLoc  = (laSetups[2,2]='Y')
*-- Use Dyelot (IC)
llDyelot   = (laSetups[3,2]='Y')
*-- Use Dyelot (MA)
llFabDye   = (laSetups[4,2]='Y')
*-- Costing Method (IC)
lcCostMth  = laSetups[5,2]
*-- Costing Method (MA)
lcCostMthM = laSetups[6,2]
*-- Link to GL
llLinkToGl = (laSetups[7,2] = 'Y')
*-- Drop down location
lcDropLoc  = laSetups[8,2]
*-- System Type 'P' for Point of sale.
llPOSale   = (laSetups[9,2] = 'P')
*-- Use Detail Costing (PO)
llImpCost  = laSetups[10,2]
*-- Multi currency
llUseMCurr = laSetups[11,2]
llMulCurr  = IIF(lcRecvType $ 'DAE',.F.,laSetups[10,2])
*-- Change exch. rates
llEditExRt = laSetups[12,2]
*-- Receive Style PO by PO/Shipment
lcCostImp  = laSetups[13,2]
*-- Style Configuration
llConfig   = (laSetups[14,2]='Y')
*-- Cost element type 1
lcIType1   = laSetups[15,2]
*-- Cost element type 2
lcIType2   = laSetups[16,2]
*-- Cost element type 3
lcIType3   = laSetups[17,2]
*-- Cost element type 4
lcIType4   = laSetups[18,2]
*-- Cost element type 5
lcIType5   = laSetups[19,2]
*-- Cost element type 6
lcIType6   = laSetups[20,2]
*-- Cost element type 7
lcIType7   = laSetups[21,2]

llMFCall = (lcRecvType $ 'MT' )
*-- To check if there is a record in the bomline with shipment No.
llShpRec = .F.

IF INLIST(lcRecvType,'O','L') AND !llFromEDI AND 'NC' $ oAriaApplication.CompanyInstalledModules
  =gfOpenFile(oAriaApplication.DataDir+'EDIACPRT',oAriaApplication.DataDir+'ACCFACT','SH')
  =gfOpenFile(oAriaApplication.DataDir+'EDIPD',oAriaApplication.DataDir+'PARTTRANS','SH')
  =gfOpenFile(oAriaApplication.DataDir+'EDITRANS',oAriaApplication.DataDir+'TYPEKEY','SH')
ENDIF

*--Get the fiscal year and fiscal period.
PRIVATE lcGLFYear, lcGlPeriod
STORE '' TO lcGLFYear, lcGlPeriod
IF llLinkToGl
  =CHECKPRD(ldTrDate,'lcGLFYear','lcGLPeriod','PO',.T.)
  *B608773,1 WAM 12/24/2008 Open GL_LINK account to validate cost of goods variance account
  =gfOpenTable(oAriaApplication.DataDir+'GL_LINK','GL_LINK','SH')
  *B608773,1 WAM 12/24/2008 (End)
ENDIF
*B608845,1 WAM 04/09/2009 Use the scale file
IF USED('SCALE')
  USE IN SCALE
ENDIF
=gfOpenFile(oAriaApplication.DataDir+'SCALE',oAriaApplication.DataDir+'SCALE','SH')
*B608845,1 WAM 04/09/2009 (End)

*B607935,1 WAM 01/17/2007 Initialize variable lcTmpItm
*PRIVATE lcVendFile, lcMastPoHd, lcMastPoLn, lcPosLn, lcMastBomLn, lcTmpBomLn, lcTempItem, lcItemLoc,;
lcTmpCur, lcGlDist, lcGlSession,  lcMastOprDt, lcTmpCode, lcTmpItmJrl, lcTmpItmDye
PRIVATE lcVendFile, lcMastPoHd, lcMastPoLn, lcPosLn, lcMastBomLn, lcTmpBomLn, lcTempItem, lcItemLoc,;
  lcTmpCur, lcGlDist, lcGlSession,  lcMastOprDt, lcTmpCode, lcTmpItmJrl, lcTmpItmDye, lcTmpItm
*B607935,1 WAM 01/17/2007 (End)


*-- lcVendFile to hold the vendor file (FOX)
lcVendFile = gfTempName()
*-- lcMastPoHd to hold the POSHDR file (SQL)
lcMastPoHd  = gfTempName()
*-- lcMastPoLn to hold the POSLN file (SQL)
lcMastPoLn  = gfTempName()
*-- lcPosLn to hold the recieving lins then passed to be updated (SQL)
lcPosLN     = gfTempName()
*-- lcMastBomLn to hold the BomLine file (SQL)
lcMastBomLn = gfTempName()
*-- lcTmpBomLn to hold a temporary BomLine then passed to be updated (SQL)
lcTmpBomLn  = gfTempName()
*-- lcMastOprDt to hold the MfgOprDt file (SQL)
lcMastOprDt = gfTempName()
*-- lcTmpCtPk to hold a temporary CutPick then passed to be updated (SQL)
lcTmpCtPk   = gfTempName()

*-- lcTempItem to hold the style file (FOX) or Item (SQL)
lcTempItem = gfTempName()
*-- lcItemLoc  to hold the stydye file (FOX) or ItemLoc file (SQL)
lcItemLoc  = gfTempName()
*-- lcGlDist   to hold the GLDIST file (FOX)
lcGlDist   = gfTempName()
*-- lcTmpCode to hold a CODES file (FOX)
lcTmpCode   = gfTempName()
*-- lcTmpItmJrl to hold the item journal file (SQL)
lcTmpItmJrl = gfTempName()

*B607935,1 WAM 01/17/2007 Initialize variable lcTmpItm to hold the item file (SQL) in order to update usage fields
lcTmpItm = gfTempName()
*B607935,1 WAM 01/17/2007 (End)

*-- lcTmpItmDye to hold the item location file (SQL) in order to update usage fields
lcTmpItmDye = gfTempName()

*-- Free lcTmpCur
lcTmpCur   = gfTempName()

*-- To get the structure of the style,stydye or itemloc to be able to add the
*-- styles or materials and update the WIP then pass it to SqlUpdate.
=lfGetStruc(lcInvType)

*----------------- To preview, print, post or cancel -----------------
*----------------- To preview, print, post or cancel [End] -----------------

*-- Add New C/t line only if using dyelots and dyelot changed.
IF llDyelot AND llMFCall
  *--Check new lines dyelots case of batch.
  SELECT (lcTmpLine)
  SCAN FOR !EMPTY(Dyelot)
    IF TranCd = '1'
      REPLACE lNewLn WITH .T.
    ELSE
      IF lcRecvType = 'T' AND !SEEK(Cuttkt+STYLE+Dyelot,'CUTTKTL')
        REPLACE lNewLUpd WITH .T.
      ENDIF
    ENDIF
  ENDSCAN

  llUnblProc = .F.

  = lfDyeOvrRcv()
  *--Unable to proceed due to
  *--The over received quantity is not allocated completely.
  IF llUnblProc
    SELECT (lcTmpLine)
    SET ORDER TO TAG TmpLine1
    GO TOP
    RETURN .F.
  ENDIF
ENDIF


*! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
*WAIT WINDOW 'Posting all transactions ...' NOWAIT
IF !loFormSet.llSilentMod
  WAIT WINDOW 'Posting all transactions ...' NOWAIT
ENDIF
*! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
lcGlSession = gfsequence('GLSESSION')

*-- Update lineNo with original lineNo on p/o in case of batch.
*-- To join the cartons lines when receive batch.
*-- 'B' Receive P/O Batch, 'T' Receive C/T Batch, 'L' Receive Inter Location P/O Batch
IF (lcRecvType $ 'BTL') AND llByCarton
  SELECT (lcTmpLine)
  REPLACE ALL LINENO WITH nLineNo
ENDIF

*-- Check it later
*-- If new line added to the P/O and update bom. Or
*-- 'I' Receive P/O, 'S' Receive by Shipment, 'R' Issue Return P/o 'D' Receive Dye Order
*!*	IF (lcRecvType $ 'ISRD')
*!*	  =lfAddNewLn()
*!*	ENDIF

*B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[Start]
IF (lcRecvType $ 'ISRD')
  =lfAddNewLn()
ENDIF
*B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[End]


*--Issue Inter Location P/O Updates.
*-- 'N' Issue Inter-Location P/O, 'A' Issue Adornment order
*-- 'H' Issue Inter Location P/O Batch, 'U' Issue Inter Location P/O Shipment
IF lcRecvType $ 'NAHU'
  *--Issue stock for Inter Location P/o Styles and create intransit records.
  =lfUpdIntCmp(lcMastShp)
  *-- Update Inter Location P/O Status [to be done later]
  IF lcRecvType = 'H'
    IF SEEK('N'+lcBatch,'CTKTRCVH')
      SELECT CTKTRCVH
      REPLACE CSTATUS WITH 'I'
    ENDIF
  ENDIF
  WAIT CLEAR
  SELECT (lcTmpLine)
  SET ORDER TO TAG TmpLine1
  DELETE ALL
  RETURN .T.
ENDIF

*-- Initialize the necessary variables to srart updating lines except
PRIVATE laOpnQty,lnLstPoRAm,lnOpnPoAmt,lcLstTrn,lcPoVend,lcOrjWareH,;
  lcLotNo, lcChkPO, lcBomPTyp, lcBomPKey,;
  laReceive, lcCstShtTyp, lcPriceCur,lcDutyCur,lnConvFact,laPrevRecQ
*B128318,1 KHM 06/01/2005 Add the initialization of laOldOpnQty to hold open qty before
*B128318,1                multiplying it by the conversion factor [Begin]
*DIMENSION laOpnQty[8], laReceive[8],laPrevRecQ[8]
DIMENSION laOpnQty[8], laReceive[8],laPrevRecQ[8], laOldOpnQty[8]
*B128318,1 KHM 06/01/2005 [End]

*B128318,1 KHM 06/01/2005 Add the initialization of laOldOpnQty to hold open qty before
*B128318,1                multiplying it by the conversion factor [Begin]
*STORE 0   TO lnLstPoRAm,lnOpnPoAmt,laOpnQty,laPrevRecQ
STORE 0   TO lnLstPoRAm,lnOpnPoAmt,laOpnQty,laPrevRecQ, laOldOpnQty
*B128318,1 KHM 06/01/2005 [End]

STORE ' ' TO lcLstTrn,lcPoVend,lcOrjWareH
STORE ' ' TO lcChkPO, lcCstShtTyp, lcShpcode
*-- To hold the lot No.
lcLotNo    = SPACE(2)

*B608773,1 WAM 12/24/2008 Use the MFCSTSC to get style standard cost in base currency for all costing elements
IF llLinkToGl AND lcCostMth = 'S' AND loFormSet.llMulCurr AND lcInvType = "0001"
  llMscale   = gfGetMemVar('M_USEEXSSC')
  lcTmpBom  = gfTempName()
  oCostSheet = NEWOBJECT('MFCSTSC',oAriaApplication.ClassDir+"WORKORDERS.VCX")
ENDIF
*B608773,1 WAM 12/24/2008 (End)

*-- Conversion Factor
lnConvFact = 1
*--Update lines for Cutting Ticket or Purchase Order.
SELECT (lcTmpLine)
SET ORDER TO TAG TmpLine3
LOCATE
SCAN FOR TranCd <> '1' AND TotQty <> 0
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
  *WAIT WINDOW 'Posting all transactions ... '+IIF(lcRecvType $ 'PFG','Material:','Style :')+Style NOWAIT
  IF !loFormSet.llSilentMod
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
    *WAIT WINDOW 'Posting all transactions ... '+IIF(lcRecvType $ 'PFG','Material:','Style :')+STYLE NOWAIT
    WAIT WINDOW 'Posting all transactions ... '+IIF(lcRecvType $ 'WPFG','Material:','Style :')+STYLE NOWAIT
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
  ENDIF
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

  *-- Get the conversion factor
  lnConvFact = 1
  IF lcInvType = "0002"
    =gfGetUOMData(&lcTmpLine..cUOMCode, '', '', @lnConvFact, .F.)
  ENDIF
  IF PO <> lcChkPO
    *-- Get the POSHDR record for the current receiving lines
    lcSqlStatement = "SELECT * FROM POSHDR [INDEX=POSHDR] "+;
      "WHERE cBusDocu = '" + &lcTmpLine..cBusDocu +;
      "' AND cStyType = '" + &lcTmpLine..cStyType +;
      "' AND PO = '" + &lcTmpLine..PO + "'"
    =lfGetItmInf(lcInvType,&lcTmpLine..cBusDocu+&lcTmpLine..cStyType+&lcTmpLine..PO,;
      lcMastPoHd,'POSHDR',lcSqlStatement,.T.)
    =SEEK(&lcTmpLine..cBusDocu+&lcTmpLine..cStyType+&lcTmpLine..PO,lcMastPoHd)
    lcPriceCur = EVALUATE(lcMastPoHd+'.cPriceCur')
    lcDutyCur  = EVALUATE(lcMastPoHd+'.cDutyCur')

    *-- Get the master records from POSLN file
    *B128070,1 KHM 05/19/2005 In case of shipment get the shipment lines only once [Begin]
    IF lcRecvType $ 'SFC' AND &lcTmpLine..ShipNo <> lcShpcode
      lcShpcode = &lcTmpLine..ShipNo
      *B128318,1 KHM 06/01/2005 [Begin]
      *lcSqlStatement  = "SELECT * FROM POSLN [INDEX=POSLNSH] "+;
      "WHERE ShipNo = '" + lcShpcode + ;
      "' AND cBusDocu = '" + &lcTmpLine..cBusDocu + ;
      "' AND cStyType ='" + &lcTmpLine..cStyType + "'"

      lcSqlStatement  = "SELECT POSLN2.* "+;
        "  FROM POSLN AS POSLN1 (INDEX=POSLNSH) inner JOIN  "+;
        "       POSLN AS POSLN2 (INDEX=POSLNS) "+;
        "    ON POSLN1.cBusDocu = POSLN2.cBusDocu " +;
        "   AND POSLN1.cStyType = POSLN2.cStyType "+;
        "   AND POSLN1.PO = POSLN2.PO " +;
        "   AND POSLN1.cInvType = POSLN2.cInvType "+;
        "   AND POSLN1.Style = POSLN2.Style "+;
        "   AND POSLN1.[LineNo] = POSLN2.[LineNo] "+;
        " WHERE POSLN1.ShipNo = '" + lcShpcode + ;
        "'  AND POSLN1.cBusDocu ='" + &lcTmpLine..cBusDocu + ;
        "'  AND POSLN1.cStyType = '" + &lcTmpLine..cStyType + "'"
      *B128318,1 KHM 06/01/2005 [End]
      =lfOpenSql(lcSqlStatement,'POSLN',lcMastPoLn)
    ENDIF
    IF !(lcRecvType $ 'SFC')
      *B128070,1 KHM 05/19/2005 [End]
      lcSqlStatement  = "SELECT * FROM POSLN [INDEX=POSLN] "+;
        "WHERE cBusDocu = '" + &lcTmpLine..cBusDocu + ;
        "' AND cStyType ='" + &lcTmpLine..cStyType  + ;
        "' AND PO ='" + &lcTmpLine..PO + "' AND cInvType ='" + lcInvType + "'"
      =lfOpenSql(lcSqlStatement,'POSLN',lcMastPoLn)
    ENDIF
    
    *! B610260,1 HES 02/27/2013 Fix bug of not fund table in the current area when the Dyelot not found for the used style location [Start]
    LOCAL laIndex
    *! B610260,1 HES 02/27/2013 Fix bug of not fund table in the current area when the Dyelot not found for the used style location [End  ]
    
    DIMENSION laIndex[2,2]
    laIndex = ''
    laIndex[1,1] = 'CBUSDOCU+CSTYTYPE+PO+cInvType+STYLE+STR(LINENO,6)+TRANCD'
    laIndex[1,2] = lcMastPoLn
    laIndex[2,1] = 'SHIPNO+CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD'
    laIndex[2,2] = 'Poslnsh'
    =lfSetIndex(lcMastPoLn,@laIndex)
    SET ORDER TO TAG lcMastPoLn IN (lcMastPoLn)

    *-- If not receive by Material PO or Material Shipment
    IF !(lcRecvType $ 'PFG')
      *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
      *!*	      lcCstShtTyp = IIF(&lcTmpLine..cStyType = "U","M",;
      *!*	        IIF(&lcTmpLine..cStyType = "P","I",&lcTmpLine..cStyType))
      lcCstShtTyp = IIF(&lcTmpLine..cStyType = "U","M",;
        IIF(&lcTmpLine..cStyType = "P","I",IIF(&lcTmpLine..cStyType = 'F','T',&lcTmpLine..cStyType)))
      *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
      *-- Get the information from the BomLine
      IF lcRecvType = 'S' AND !EMPTY(lcShpcode)

        *: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
        *lcSqlStatement = "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+;
        "WHERE cImTyp = '" + lcCstShtTyp +;
        "' AND cTktNo ='" + &lcTmpLine..PO + "' AND ShipNo='" + lcShpcode + "'"
        lcSqlStatement = "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+;
          "WHERE cImTyp = '" + lcCstShtTyp +;
          "' AND cTktNo ='" + &lcTmpLine..PO + "' AND ShipNo='" + lcShpcode + "' AND CRSESSION = ''"
        *: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]
      ELSE
        lcSqlStatement = "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+;
          "WHERE cImTyp = '" + lcCstShtTyp +;
          "' AND cTktNo ='" + &lcTmpLine..PO + "'"
      ENDIF
      =lfOpenSql(lcSqlStatement,'BOMLINE',lcMastBomLn, "","",.F.)
      IF lcRecvType = 'S' AND !EMPTY(lcShpcode)
        SELECT (lcMastBomLn)
        LOCATE
        IF EOF()
          llShpRec = .F.
          lcSqlStatement = "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+;
            "WHERE cImTyp = '" + lcCstShtTyp +;
            "' AND cTktNo ='" + &lcTmpLine..PO + "'"
          =lfOpenSql(lcSqlStatement,'BOMLINE',lcMastBomLn, "","",.F.)
        ELSE
          llShpRec = .T.
        ENDIF
      ENDIF
      DIMENSION laIndex[1,2]
      laIndex = ''
      laIndex[1,1] = 'cImTyp+cType+ShipNo+cTktNo+STR(LineNo,6)'
      laIndex[1,2] = lcMastBomLn
      =lfSetIndex(lcMastBomLn,@laIndex)
    ENDIF

    *-- Get the information of the vendor

    *B128055,1 AMH 05/18/2005 Use m.Vendor to work with RDAC correctly. [Start]
    *lcSqlStatement = "SELECT * FROM APVENDOR "+;
    "WHERE cVendCode = '" + &lcMastPoHd..Vendor + "'"
    m.vendor = EVALUATE(lcMastPoHd+'.Vendor')
    lcSqlStatement = "SELECT * FROM APVENDOR "+;
      "WHERE cVendCode =?m.Vendor"
    *B128055,1 AMH 05/18/2005 [End]

    =lfGetItmInf(lcInvType,&lcMastPoHd..Vendor,;
      lcVendFile,'APVENDOR',lcSqlStatement,.F.)
    =SEEK(&lcMastPoHd..Vendor,lcVendFile)
    lcChkPO = &lcTmpLine..PO
  ENDIF

  SELECT (lcTmpLine)

  *--Check if receive to stock or receive to damage/2nd qualtity.
  llStkLine = ( TranCd $'24')
  *--Compute previous open balance on style P/o or C/t.----------------
  lcMainKy   = cBusDocu+cStyType+PO+cInvType+STYLE+STR(LINENO,6)
  lcOrjWareH = lfCalOpen(lcRecvType, lcMastPoLn, lcMainKy , .T.)
  *--If this is a new line therefor no open qty.
  IF &lctmpline..lnewln
    laOpnQty = 0
    *B128318,1 KHM 06/01/2005 [Begin]
    laOldOpnQty = 0
    *B128318,1 KHM 06/01/2005 [End]
  ENDIF

  *-- Multiply the open quantity by the conversion factor
  FOR lnCntI = 1 TO 8
    *B128318,1 KHM 06/01/2005 Save the open quantity before multiplying it by the conversion
    *B128318,1                factor in order to use it in udpdating the POHDR file [Begin]
    laOldOpnQty[lnCntI] = laOpnQty[lnCntI]
    *B128318,1 KHM 06/01/2005 [End]

    laOpnQty[lnCntI] = laOpnQty[lnCntI] * lnConvFact
  ENDFOR

  *--Update BomLine (Step 1)--------------------------------------------
  SELECT (lcTmpLine)
  IF llStkLine AND (llMFCall OR !(lcRecvType $'REPFG') )
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
    *!*	    lcBomPTyp = IIF(&lcTmpLine..cStyType = "U","M",;
    *!*	      IIF(&lcTmpLine..cStyType = "P","I",&lcTmpLine..cStyType))
    lcBomPTyp = IIF(&lcTmpLine..cStyType = "U","M",;
      IIF(&lcTmpLine..cStyType = "P","I",IIF(&lcTmpLine..cStyType = "F","T",&lcTmpLine..cStyType)))
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
    *lcBomPKey = Po+STR(LineNo,6)
    lcBomPKey = Po
    *-- Create new records in BOMLINE file with type '2' if it is not found.
    DO gpCrtBom WITH lcBomPTyp,lcBomPKey,STYLE,;
      IIF(lcRecvType $ 'SC',ShipNo,SPACE(6)),;
      lcGlSession,&lcTmpLine..cStyGrade,lcMastBomLn,lcTmpBomLn,llShpRec

    SELECT (lcTmpLine)

    *--Calculate landed costs case of detail costing.
    *-- If you are not in the manufacturing module and not detail costing
    *-- and you are editing the costs then take it from the entered one.

    *B126833,1 WAM 04/03/2005 add new button to edit landed cost for styles has no detail costing
    *IF !llMfCall AND !llImpCost AND llEditLCst
    IF !llMfCall AND !lDetCost AND llEditLCst
      *B126833,1 WAM 04/03/2005 (End)

      *--Take from entered landed cost.
    ELSE
      *! B609634,1 MMT 06/28/2011 Fix bug of Zero Cost in Styinvjl in Case of receiving Interlocation PO[Start]
      IF !(&lcTmpLine..cStyType+&lcTmpLine..cBusDocu = 'NN')
        *! B609634,1 MMT 06/28/2011 Fix bug of Zero Cost in Styinvjl in Case of receiving Interlocation PO[ENd]
        =lfGetLanded()
        *! B609634,1 MMT 06/28/2011 Fix bug of Zero Cost in Styinvjl in Case of receiving Interlocation PO[Start]
      ENDIF
      *! B609634,1 MMT 06/28/2011 Fix bug of Zero Cost in Styinvjl in Case of receiving Interlocation PO[End]
    ENDIF
  ENDIF

  *--New Receiving Cost.
  SELECT (lcTmpLine)
  IF llMFCall
    lnNewCost = nFLanCost1 + nFLanCost2 + nFLanCost3 + nFLanCost4 + nFLanCost5 +;
      nFLanCost6 + nFLanCost7
  ELSE
    lnELanded = nLan_Cost1
    IF lnELanded = ROUND(Gros_Price*(1-Disc_Pcnt/100),3)
      lnELanded = Gros_Price*(1-Disc_Pcnt/100)
    ENDIF
    lnNewCost = lnELanded+nLan_Cost2+nLan_Cost3+nLan_Cost4+nLan_Cost5+nLan_Cost6+nLan_Cost7
  ENDIF
  *B608773,1 WAM 12/24/2008 Store landed cost in base currency
  lnOrgCost = lnNewCost
  *B608773,1 WAM 12/24/2008 (End)

  lcStyle    = STYLE
  lcWareHous = cWareCode
  lcDyelot   = IIF(SEEK(lcMainKy,lcMastPoLn),EVALUATE(lcMastPoLn+'.Dyelot'),'')

  *-- Get item information from style file
  IF lcInvType = "0001"
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
    *lcSqlStatement = "SELECT * FROM STYLE WHERE Style = '" + lcStyle + "'"    
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
    *lcSqlStatement = "SELECT * FROM STYLE WHERE Style = [" + lcStyle + "]"
    lcSelValueStyle = lcStyle 
    lcSqlStatement = "SELECT * FROM STYLE WHERE Style = ?m.lcSelValueStyle"
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][END]
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][ENd]
  ELSE
    *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
    *lcSqlStatement = "SELECT * FROM ITEM WHERE cInvType ='" + lcInvType +;
    "' AND Style = '" + lcStyle + "'"
    lcSqlStatement = "SELECT * FROM ITEM WHERE cInvType ='" + lcInvType +;
      "' AND Style = ?m.lcStyle "
    *B607658,1 KHM 07/07/2005 [End]
  ENDIF
  =lfGetItmInf(lcInvType,IIF(lcInvType="0001","",lcInvType)+lcStyle,lcTempItem,;
    IIF(lcInvType="0001",'STYLE','ITEM'),lcSqlStatement,lcInvType = "0002")
  =SEEK(IIF(lcInvType="0001","",lcInvType)+lcStyle,lcTempItem)

  *-- Get item information from stydye file for the target location
  IF lcInvType = "0001"
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
*!*	    lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
*!*	      lcStyle+lcOrjWareH+SPACE(10) + "'"
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
*!*	    lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
*!*	      lcStyle+lcOrjWareH+SPACE(10) + "]"
    lcSelValueSty = lcStyle+lcOrjWareH+SPACE(10) 
    lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = ?m.lcSelValueSty "
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]  
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]  
  ELSE
    *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
    *lcSqlStatement = "SELECT * FROM ITEMLOC WHERE cInvType = '" + lcInvType +;
    "' AND Style ='" + lcStyle + "' AND cWareCode='" + lcOrjWareH +;
    "' AND Dyelot = '" + SPACE(10) + "'"
    lcSqlStatement = "SELECT * FROM ITEMLOC WHERE cInvType = '" + lcInvType +;
      "' AND Style = ?m.lcStyle " + " AND cWareCode = ?m.lcOrjWareH " +;
      "  AND Dyelot = '" + SPACE(10) + "'"
    *B607658,1 KHM 07/07/2005
  ENDIF
  =lfGetItmInf(lcInvType,IIF(lcInvType="0001","",lcInvType)+lcStyle+lcOrjWareH+SPACE(10),lcItemLoc,;
    IIF(lcInvType="0001",'STYDYE','ITEMLOC'),lcSqlStatement, lcInvType = "0002")
  =SEEK(IIF(lcInvType="0001","",lcInvType)+lcStyle+lcOrjWareH+SPACE(10),lcItemLoc)

  *--Read the G/L link code that will be used to create GL entres.
  IF llStkLine
    IF &lcTmpLine..Trancd = '4'
      *-- Get item information from style file
      IF lcInvType = "0001"
        *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
        *lcSqlStatement = "SELECT * FROM STYLE WHERE Style = '" + &lcTmpLine..cRetSty + "'"        
        *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
        *lcSqlStatement = "SELECT * FROM STYLE WHERE Style = [" + &lcTmpLine..cRetSty + "]"        
        lcSelStyVal = &lcTmpLine..cRetSty 
        lcSqlStatement = "SELECT * FROM STYLE WHERE Style = ?m.lcSelStyVal"
        *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
        *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][END]
      ELSE
        *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
        *lcSqlStatement = "SELECT * FROM ITEM WHERE cInvType ='" + lcInvType +;
        "' AND Style = '" + lcStyle + "'"
        lcSqlStatement = "SELECT * FROM ITEM WHERE cInvType ='" + lcInvType +;
          "' AND Style = ?m.lcStyle "
        *B607658,1 KHM 07/07/2005 [End]
      ENDIF
      =lfGetItmInf(lcInvType,IIF(lcInvType="0001","",lcInvType)+&lcTmpLine..cRetSty,lcTempItem,;
        IIF(lcInvType="0001",'STYLE','ITEM'),lcSqlStatement,lcInvType = "0002")
      =SEEK(IIF(lcInvType="0001","",lcInvType)+&lcTmpLine..cRetSty,lcTempItem)
    ENDIF
    *B608645,1 WAM 08/07/2008 Get the receiving cost from the style table when the costing methos is Standard
    IF lcInvType = "0001" AND lcCostMth = 'S'
      lnNewCost = &lcTempItem..TotCost
    ENDIF
    *B608645,1 WAM 08/07/2008 (End)

    lclinkCode = IIF(!EMPTY(&lcTempItem..link_code),&lcTempItem..link_code,'DEFDEF')

    IF &lcTmpLine..Trancd = '2'
      lcStyle = &lcTmpLine..STYLE
    ELSE
      lcStyle = &lcTmpLine..cRetSty
    ENDIF
    *-- Get item information from stydye file
    IF lcInvType = "0001"
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
*!*	      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
*!*	        lcStyle+lcOrjWareH+SPACE(10) + "'"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
*!*	      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
*!*	        lcStyle+lcOrjWareH+SPACE(10) + "]"
      lcValSelSty = lcStyle+lcOrjWareH+SPACE(10) 
      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = ?m.lcValSelSty "
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]  
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]  
    ELSE
      *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
      *lcSqlStatement = "SELECT * FROM ITEMLOC WHERE cInvType = '" + lcInvType +;
      "' AND Style ='" + lcStyle + "' AND cWareCode='" + lcOrjWareH +;
      "' AND Dyelot = '" + SPACE(10) + "'"
      lcSqlStatement = "SELECT * FROM ITEMLOC WHERE cInvType = '" + lcInvType +;
        "' AND Style = ?m.lcStyle " + " AND cWareCode = ?m.lcOrjWareH " +;
        "  AND Dyelot = '" + SPACE(10) + "'"
      *B607658,1 KHM 07/07/2005 [End]
    ENDIF
    =lfGetItmInf(lcInvType,IIF(lcInvType="0001","",lcInvType)+lcStyle+lcOrjWareH+SPACE(10),lcItemLoc,;
      IIF(lcInvType="0001",'STYDYE','ITEMLOC'),lcSqlStatement,lcInvType = "0002")
    =SEEK(IIF(lcInvType="0001","",lcInvType)+lcStyle+lcOrjWareH+SPACE(10),lcItemLoc)

    lclinkCode=IIF(!EMPTY(&lcItemLoc..Gl_link),&lcItemLoc..Gl_link,lclinkcode)
  ENDIF
  *-- Get item information from stydye file for dyelot
  IF !EMPTY(lcDyelot) AND &lcTmpLine..cDye_Flg = "Y"
    IF lcInvType = "0001"
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
*!*	      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
*!*	        lcStyle+lcOrjWareH+lcDyelot + "'"
	  *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
*!*	      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
*!*	        lcStyle+lcOrjWareH+lcDyelot + "]"
      lcValStySel = lcStyle+lcOrjWareH+lcDyelot 
      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = ?m.lcValStySel"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]  
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][END]  
    ELSE
      *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
      *lcSqlStatement = "SELECT * FROM ITEMLOC WHERE cInvType = '" + lcInvType +;
      "' AND Style ='" + lcStyle + "' AND cWareCode='" + lcOrjWareH +;
      "' AND Dyelot = '" + lcDyelot + "'"
      lcSqlStatement = "SELECT * FROM ITEMLOC WHERE cInvType = '" + lcInvType +;
        "' AND Style = ?m.lcStyle " + " AND cWareCode = ?m.lcOrjWareH " +;
        "  AND Dyelot = '" + lcDyelot + "'"
      *B607658,1 KHM 07/07/2005
    ENDIF
    =lfGetItmInf(lcInvType,IIF(lcInvType="0001","",lcInvType)+lcStyle+lcOrjWareH+lcDyelot,lcItemLoc,;
      IIF(lcInvType="0001",'STYDYE','ITEMLOC'),lcSqlStatement,lcInvType = "0002")
    =SEEK(IIF(lcInvType="0001","",lcInvType)+lcStyle+lcOrjWareH+lcDyelot,lcItemLoc)
  ENDIF

  lcGLPLkC = IIF(SEEK(&lcTmpLine..cBusDocu+&lcTmpLine..cStyType+&lcTmpLine..PO,lcMastPoHd),;
    &lcMastPoHd..Link_Code,'DEFDEF')

  lcCurSty = &lcTempItem..STYLE
  = SEEK(&lcTmpLine..STYLE,lcTempItem)

  IF !(lcRecvType $ 'RNOLAEG') AND llStkLine AND llDyelot AND &lcTempItem..cDye_Flg = 'Y'
  
  
    IF &lcTmpLine..Trancd = '2'
      *! B610163,1 HIA 11/27/2012 Error updateing, while receving material PO [T20121112.0027][Begin]
      **llFound=SEEK(&lcTmpLine..STYLE  +&lcTmpLine..cWareCode+&lcTmpLine..Dyelot,lcItemLoc)
	  *llFound=SEEK(IIF(lcInvType="0001","",lcInvType)+&lcTmpLine..STYLE  +&lcTmpLine..cWareCode+&lcTmpLine..Dyelot,lcItemLoc)
      lcOldAliasT = ALIAS()
      lcTempItm   = gfTempName()
      IF !USED(lcTempItm)
        = gfOpenTable('ITEMLOC','STYDYE', 'SH',lcTempItm)
      ENDIF
      SELECT (lcTempItm)
      llFound = gfSEEK(IIF(lcInvType="0001","",lcInvType)+&lcTmpLine..STYLE  +&lcTmpLine..cWareCode+&lcTmpLine..Dyelot)
      =gfcloseTable(lcTempItm)
      SELECT (lcOldAliasT)
      *! B610163,1 HIA 11/27/2012 Error updateing, while receving material PO [T20121112.0027][End]
    ELSE    && Trancd = lcOthrTrCd
      *! B610163,1 HIA 11/27/2012 Error updateing, while receving material PO [T20121112.0027][BEgin]
      **llFound=SEEK(&lcTmpLine..cRetSty+&lcTmpLine..cWareCode+&lcTmpLine..Dyelot,lcItemLoc)
      *llFound=SEEK(IIF(lcInvType="0001","",lcInvType)+&lcTmpLine..cRetSty+&lcTmpLine..cWareCode+&lcTmpLine..Dyelot,lcItemLoc)
      lcOldAliasT = ALIAS()
      lcTempItm  = gfTempName()
      IF !USED(lcTempItm)
        =gfOpenTable('ITEMLOC','STYDYE', 'SH',lcTempItm)
      ENDIF
      SELECT (lcTempItm)
      llFound = gfSEEK(IIF(lcInvType="0001","",lcInvType)+&lcTmpLine..cRetSty+&lcTmpLine..cWareCode+&lcTmpLine..Dyelot)
      =gfcloseTable(lcTempItm)
      SELECT (lcOldAliasT)      
      
      *! B610163,1 HIA 11/27/2012 Error updateing, while receving material PO [T20121112.0027][End]
    ENDIF

    IF ! llFound
      IF lcInvType = "0001"
        DO gpAdStyWar WITH IIF(&lcTmpLine..Trancd='2',&lcTmpLine..STYLE,&lcTmpLine..cRetSty),;
          &lcTmpLine..Dyelot,&lcTmpLine..cWareCode
      ELSE
        =gfAdItemWar(lcInvType,IIF(&lcTmpLine..Trancd='2',&lcTmpLine..STYLE,&lcTmpLine..cRetSty),;
          &lcTmpLine..Dyelot,&lcTmpLine..cWareCode)
      ENDIF
      *      SELECT (lcItemLoc)
      *      APPEND BLANK
      *      REPLACE Style     WITH IIF(&lcTmpLine..Trancd='2',&lcTmpLine..Style,&lcTmpLine..cRetSty),;
      cWareCode WITH &lcTmpLine..cWareCode,;
      Dyelot    WITH &lcTmpLine..Dyelot
    ENDIF
  ENDIF

  SELECT (lcTmpLine)

  = SEEK(lcCurSty,lcTempItem)

  lcWipSgn = IIF(lcRecvType $ 'R','+','-')
  *-- Update WIP in the style file
  FOR lnCnI = 1 TO 8
    lcCntI = STR(lnCnI,1)
    laReceive[lnCnI] = EVALUATE(lcTmpLine+'.Qty'+lcCntI) * lnConvFact
  ENDFOR

  *--Update Style -----------------------------------------------
  *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
  *IF lcInvType = "0001" AND SEEK(&lcTmpLine..Style,lcTempItem) AND !&lcTmpLine..lNewLUpd
  *  =lfUpdItmFl(lcTempItem,'WIP',lcWipSgn,@laReceive,@laOpnQty)
  *  =lfUpdWip()
  *ENDIF
  IF SEEK(&lcTmpLine..STYLE,lcTempItem) AND !&lcTmpLine..lNewLUpd
    =lfUpdItmFl(lcTempItem, IIF(lcRecvType = 'G', 'NONRET', 'WIP'), lcWipSgn, @laReceive, @laOpnQty, lcInvType = "0002")
    IF lcInvType = "0001"
      =lfUpdWip()
    ENDIF
  ENDIF
  *E039550,1 WSH 08/07/2005 [End]

  *--Update StyDye Warehouse record -------------------------------
  *IF SEEK(IIF(lcInvType="0001","",lcInvType)+&lcTmpLine..Style+lcOrjWareH+SPACE(10),lcItemLoc) AND !&lcTmpLine..lNewLUpd
  IF lcRecvType <> 'G'
    IF SEEK(IIF(lcInvType="0001","",lcInvType)+&lcTmpLine..STYLE+lcOrjWareH+SPACE(10),lcItemLoc);
        AND !&lcTmpLine..lNewLUpd
      =lfUpdItmFl(lcItemLoc,'WIP',lcWipSgn,@laReceive,@laOpnQty)
    ENDIF
  ELSE
    IF SEEK(lcInvType+&lcTmpLine..STYLE+lcOrjWareH+SPACE(10),lcItemLoc);
        AND !&lcTmpLine..lNewLUpd
      =lfUpdItmFl(lcItemLoc,'NONRET',lcWipSgn,@laReceive,@laOpnQty)
    ENDIF
  ENDIF

  *--Update StyDye Warehouse/dyelot record -----------------------
  *IF (llDyelot OR llFabDye) AND !EMPTY(&lcTmpLine..Dyelot) AND &lcTempItem..cDye_Flg='Y' AND;
  SEEK(IIF(lcInvType="0001","",lcInvType)+&lcTmpLine..Style+lcOrjWareH+lcDyelot,lcItemLoc)
  IF lcRecvType <> 'G'
    IF (llDyelot OR llFabDye) AND !EMPTY(&lcTmpLine..Dyelot) AND;
        &lcTempItem..cDye_Flg='Y' AND;
        SEEK(IIF(lcInvType="0001","",lcInvType)+&lcTmpLine..STYLE+lcOrjWareH+lcDyelot,lcItemLoc)
      =lfUpdItmFl(lcItemLoc,'WIP',lcWipSgn,@laReceive,@laOpnQty)
    ENDIF
  ELSE
    IF (llDyelot OR llFabDye) AND !EMPTY(&lcTmpLine..Dyelot) AND;
        &lcTempItem..cDye_Flg='Y' AND;
        SEEK(lcInvType+&lcTmpLine..STYLE+lcOrjWareH+lcDyelot,lcItemLoc)
      =lfUpdItmFl(lcItemLoc,'NONRET',lcWipSgn,@laReceive,@laOpnQty)
    ENDIF
  ENDIF

  *--Update APVendor -------------------------------
  IF !llMFCall AND !(lcRecvType $ 'OELC') AND SEEK(&lcTmpLine..Vendor,lcVendFile)
    SELECT (lcTmpLine)
    IF lcLstTrn <> Po AND lcPoVend <> Vendor
      STORE 0 TO lnLstPoRAm,lnOpnPoAmt
      lcLstTrn  = Po
      lcPoVend = Vendor
    ENDIF
    lnLstPoRAm = lnLstPoRAm + (TotQty * nLan_Cost1)
    =SEEK(cBusDocu+cStyType+Po+cInvType+STYLE+STR(LINENO,6)+'1',lcMastPoLn)

    *B609696,1 MMT 10/13/2011 Fix bug of wrong updates of the Field apvendor.nVenOpnPo [Start]
    *lnOpnPoAmt = ( MIN((TotQty * nLan_Cost1),(&lcMastPoLn..TotQty * &lcMastPoLn..nICost1)) )
    lnTotQtyVend = 0
    FOR lnCntQty = 1 TO 8
      lnTotQtyVend = lnTotQtyVend +  MIN(laReceive[lnCntQty],laOpnQty[lnCntQty])
    ENDFOR
    lnOpnPoAmt = (lnTotQtyVend * NFLANCOST1)
    *B609696,1 MMT 10/13/2011 Fix bug of wrong updates of the Field apvendor.nVenOpnPo [END]

    SELECT (lcVendFile)
    IF lcRecvType $ 'RG'
      REPLACE nVenOpnPo WITH nVenOpnPo - lnOpnPoAmt
    ELSE
      REPLACE dVenLPoRD WITH ldRecDate  ,;
        nVenLPoRA WITH lnLstPoRAm ,;
        nVenOpnPo WITH nVenOpnPo - lnOpnPoAmt
    ENDIF
    SELECT (lcTmpLine)
  ENDIF

  *--Update POSHDR Header file  --------------------------
  lnOpnSub=0
  FOR lnI = 1 TO 8
    lcCnt = STR(lnI,1)
    *B128318,1 KHM 06/01/2005 Use laOldOpnQty instead of laOpnQty [Begin]
    *lnOpnSub = lnOpnSub + MIN(laOpnQty[lnI] , &lcTmpLine..Qty&lcCnt )
    lnOpnSub = lnOpnSub + MIN(laOldOpnQty[lnI] , &lcTmpLine..Qty&lcCnt )
    *B128318,1 KHM 06/01/2005 [End]

  ENDFOR

  lcFrstSess = IIF(lcRecvType $ 'RG' AND !EMPTY(&lcMastPoHd..cPONo),&lcTmpLine..CRSESSION,'')
  SELECT (lcMastPoHd)
  DO CASE
  CASE llStkLine
    *-- If receive to stock
    IF &lcTmpLine..Trancd = '2'
      REPLACE Receive WITH Receive + &lcTmpLine..TotQty,;
        OPEN    WITH MAX(OPEN - lnOpnSub,0)
    ELSE
      *-- If receive to 2nd quality or damage
      REPLACE Damage  WITH Damage  + &lcTmpLine..TotQty,;
        OPEN    WITH MAX(OPEN - lnOpnSub,0)
    ENDIF
    REPLACE nFlanCost2 WITH nFlanCost2 +(ROUND(&lcTmpLine..nFlanCost2 * &lcTmpLine..TotQty,3)),;
      nFlanCost3 WITH nFlanCost3 +(ROUND(&lcTmpLine..nFlanCost3 * &lcTmpLine..TotQty,3)),;
      nFlanCost4 WITH nFlanCost4 +(ROUND(&lcTmpLine..nFlanCost4 * &lcTmpLine..TotQty,3)),;
      nFlanCost5 WITH nFlanCost5 +(ROUND(&lcTmpLine..nFlanCost5 * &lcTmpLine..TotQty,3)),;
      nFlanCost6 WITH nFlanCost6 +(ROUND(&lcTmpLine..nFlanCost6 * &lcTmpLine..TotQty,3)),;
      nFlanCost7 WITH nFlanCost7 +(ROUND(&lcTmpLine..nFlanCost7 * &lcTmpLine..TotQty,3))

    REPLACE nlan_cost2 WITH nlan_cost2 +(ROUND(&lcTmpLine..nlan_cost2 * &lcTmpLine..TotQty,3)),;
      nlan_cost3 WITH nlan_cost3 +(ROUND(&lcTmpLine..nlan_cost3 * &lcTmpLine..TotQty,3)),;
      nlan_cost4 WITH nlan_cost4 +(ROUND(&lcTmpLine..nlan_cost4 * &lcTmpLine..TotQty,3)),;
      nlan_cost5 WITH nlan_cost5 +(ROUND(&lcTmpLine..nlan_cost5 * &lcTmpLine..TotQty,3)),;
      nlan_cost6 WITH nlan_cost6 +(ROUND(&lcTmpLine..nlan_cost6 * &lcTmpLine..TotQty,3)),;
      nlan_cost7 WITH nlan_cost7 +(ROUND(&lcTmpLine..nlan_cost7 * &lcTmpLine..TotQty,3))

    IF &lcTmpLine..nlan_Cost1 = ROUND(&lcTmpLine..Gros_Price*(1-&lcTmpLine..Disc_Pcnt/100),3)
      REPLACE nFlanCost1 WITH nFlanCost1 +(ROUND(&lcTmpLine..nFlancost1 * &lcTmpLine..TotQty,3)),;
        nlan_cost1 WITH nlan_cost1 +(ROUND((&lcTmpLine..Gros_Price*(1-&lcTmpLine..Disc_Pcnt/100))* &lcTmpLine..TotQty,3))
    ELSE
      REPLACE nFlanCost1 WITH nFlanCost1 +(ROUND(&lcTmpLine..nFlanCost1 * &lcTmpLine..TotQty,3)),;
        nlan_cost1 WITH nlan_cost1 +(ROUND(&lcTmpLine..nlan_cost1 * &lcTmpLine..TotQty,3))
    ENDIF
    REPLACE nTot_Cost  WITH nlan_cost1+nlan_cost2+nlan_cost3+nlan_cost4+nlan_cost5+nlan_cost6+nlan_cost7

  CASE &lcTmpLine..Trancd = '5'
    REPLACE CANCEL  WITH CANCEL  + &lcTmpLine..TotQty,;
      OPEN    WITH MAX((OPEN    - lnOpnSub),0)
  ENDCASE

  IF OPEN = 0
    REPLACE &lcMastPoHd..STATUS WITH 'C'
  ENDIF

  *!*	    *C037345,1 ABD - Trigger for customer PUFFA to update the style critical path file. [BEgin]
  *!*	    IF lcRecvType = 'I' .AND. ASCAN(laEvntTrig , PADR('STYCRUPD',10)) <> 0
  *!*	      =gfDoTriger('POSTREC',PADR('STYCRUPD',10))
  *!*	    ENDIF
  *!*	    *C037345,1 ABD - [End]

  SELECT (lcTmpLine)
  SCATTER MEMVAR
  SELECT (lcPosLn)
  APPEND BLANK
  GATHER MEMVAR
  REPLACE DATE      WITH ldRecDate,;
    dPostDate WITH ldTrDate ,;
    cOwner    WITH ' '
  *--Changing receiving from customer to location.
  IF !llMFCall AND cWareCode <> lcDropLoc AND !EMPTY(Account)
    REPLACE Account WITH SPACE(5),;
      STORE   WITH SPACE(8)
  ENDIF
  *B128070,1 KHM 05/19/2005 Updating the receiving session in thePosLn in call cases
  *B128070,1 KHM 05/19/2005 which means when receive to cancel [Begin]
  *IF llStkLine
  *B128070,1 KHM 05/19/2005 [End]
  REPLACE cRSession WITH lcGlSession
  *ENDIF

  SELECT (lcTmpLine)
  IF llStkLine AND (llMFCall OR !(lcRecvType $ 'ROELG'))
    IF llMFCall
      *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
      *lcBomLKey = 'M2'+Cuttkt
      lcBomLKey = 'M2'+PO
      *N038893,1 WAM 06/02/2005 (End)

      lcWhileCn = "cIMTyp+cType+cTktNo=lcBomLKey"
    ELSE
      *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
      *!*	      lcBomLKey = IIF(lcRecvType = 'D','D',;
      *!*	        IIF(&lcTmpLine..cStyType="N","N",'I'))+'2'+;
      *!*	        IIF(lcRecvType = 'S',Shipno,'')+Po+STR(LINENO,6)
      lcBomLKey = IIF(lcRecvType = 'D','D',;
        IIF(&lcTmpLine..cStyType="N","N",IIF(&lcTmpLine..cStyType="F",'T','I')))+'2'+;
        IIF(lcRecvType = 'S',Shipno,'')+Po+STR(LINENO,6)
      *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
      lcWhileCn = "cIMTyp+cType+IIF(lcRecvType = 'S',Shipno,'')+cTktNo+STR(LineNo,6)=lcBomLKey"
    ENDIF
    SELECT (lcTmpBomLn)
    IF lcRecvType ='S'
      SET ORDER TO TAG BomLnShp
    ELSE
      SET ORDER TO TAG BomLine
    ENDIF
    *B610469,1 TMI 08/18/2013 [Start] Get the key
    lcTmpBomLnKey = KEY()
    *B610469,1 TMI 08/18/2013 [End  ] Get the key

    IF SEEK(lcBomLKey)
      *B608762,1 WAM 12/15/2008  Consider receiving cut ticket line into more than one warehouse
      *SCAN REST WHILE &lcWhileCn ;
      FOR Style = &lcTmpLine..Style AND EMPTY(cRSession) AND cStyGrade = &lcTmpLine..cStyGrade

      *B608845,1 WAM 04/09/2009 Take line# into consideration
      *SCAN REST WHILE &lcWhileCn ;
      FOR Style = &lcTmpLine..Style AND cWarecode = &lcTmpLine..cWarecode AND EMPTY(cRSession) AND cStyGrade = &lcTmpLine..cStyGrade
      SCAN REST WHILE &lcWhileCn ;
          FOR STYLE = &lcTmpLine..STYLE AND LINENO = &lcTmpLine..LINENO AND cWarecode = &lcTmpLine..cWarecode AND EMPTY(cRSession) AND cStyGrade = &lcTmpLine..cStyGrade

        *B608845,1 WAM 04/09/2009 (End)

        SCATTER MEMVAR MEMO
        DELETE
        APPEND BLANK
        GATHER MEMVAR MEMO
        REPLACE cRSession WITH lcGlSession
        SCATTER MEMVAR
        STORE 0 TO m.StyQty,m.ItemQty,m.ItemAmt
        m.cRSession = ''
        *B610469,1 TMI 08/18/2013 [Start] get the key of the current alias
        lcCurrKeyNLINENO = EVALUATE(KEY())
        lcCurrKey = LEFT(lcCurrKeyNLINENO,LEN(lcCurrKeyNLINENO)-6)
        *B610469,1 TMI 08/18/2013 [End  ] 
        SELECT (lcTmpBomLn)
        *: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
        IF llAddBomLine
          *: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]
          *B610469,1 TMI 08/18/2013 [Start] locate to check if there is empty receiving session more than once, if so then don't add the line
          =SEEK(lcCurrKey)
          LOCATE REST WHILE &lcTmpBomLnKey. = lcCurrKey FOR EMPTY(cRSession)
          IF !FOUND()
            *B610469,1 TMI 08/18/2013 [End  ] 
          APPEND BLANK
          GATHER MEMVAR
          ELSE
            =SEEK(lcCurrKeyNLINENO)
            *B610469,1 TMI 08/18/2013 [Start] 
          ENDIF 
          *B610469,1 TMI 08/18/2013 [End  ] 
          *: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
        ENDIF
        *: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]
      ENDSCAN
      SELECT (lcTmpLine)
    ENDIF
  ENDIF

  *--Update MfgOprDt file --------------------------------------
  SELECT (lcTmpLine)
  lcLastOpr = clastopr
  IF !EMPTY(lcLastOpr) AND lcRecvType $ 'IMDO'
    SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laRecvQty
    IF laRecvQty[9] > 0
      =lfUpdLot(&lcTmpLine..PO,lcLastOpr,&lcTmpLine..cLotNo,'laRecvQty',&lcTmpLine..STYLE,&lcTmpLine..Dyelot)
    ENDIF
  ENDIF

  *--Update Shipment In-Transit P/o line qty. ------------
  SELECT (lcTmpLine)
  IF lcRecvType $ 'SF' OR (lcRecvType = 'B' AND !EMPTY(CTKTRCVH.ShipNo) )
    IF (lcRecvType = 'B' AND !EMPTY(CTKTRCVH.ShipNo) )
      lcShpCode = CTKTRCVH.ShipNo
    ENDIF
    lcShpLine = ShipNo+cBusDocu+cStyType+PO+cInvType+STYLE+STR(LINENO,6)+'3'
    SELECT (lcMastPoLn)
    SET ORDER TO TAG Poslnsh
    =SEEK(lcShpLine)
    SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laSHPQty
    SCATTER MEMVAR MEMO
    SELECT(lcPosLn)
    SET ORDER TO TAG Poslnsh
    IF !SEEK(lcShpLine)
      SELECT(lcPosLn)
      APPEND BLANK
      TABLEUPDATE(0,.T.)
      GATHER MEMVAR MEMO
    ENDIF
    REPLACE Qty1 WITH MAX(Qty1 -&lcTmpLine..Qty1,0),;
      Qty2 WITH MAX(Qty2 -&lcTmpLine..Qty2,0),;
      Qty3 WITH MAX(Qty3 -&lcTmpLine..Qty3,0),;
      Qty4 WITH MAX(Qty4 -&lcTmpLine..Qty4,0),;
      Qty5 WITH MAX(Qty5 -&lcTmpLine..Qty5,0),;
      Qty6 WITH MAX(Qty6 -&lcTmpLine..Qty6,0),;
      Qty7 WITH MAX(Qty7 -&lcTmpLine..Qty7,0),;
      Qty8 WITH MAX(Qty8 -&lcTmpLine..Qty8,0),;
      TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
    IF TotQty = 0
      DELETE
    ENDIF

    SELECT (lcMastPoLn)
    SET ORDER TO TAG lcMastPoLn
    SELECT(lcPosLn)
    SET ORDER TO TAG (lcPosLn)

    SELECT (lcTmpLine)
  ENDIF

  SELECT (lcTmpLine)
  *! B610051,1 MMT 08/16/2012 Receiving Inter-location PO to different location other than target location on PO gives error if Bin location is used[Start]
  lcRecWareH = m.cWareCode
  *! B610051,1 MMT 08/16/2012 Receiving Inter-location PO to different location other than target location on PO gives error if Bin location is used[End]
  IF lcRecvType $ 'OELC'
    SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laSHPQty

    *--Get the received line qty.
    SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laAcLRQy
    lcShpLine = cBusDocu+cStyType+Po+cInvType+STYLE+STR(LINENO,6)+'6'
    SELECT (lcMastPoLn)
    =SEEK(lcShpLine)
    SCAN REST WHILE cBusDocu+cStyType+Po+cInvType+STYLE+STR(LINENO,6)+Trancd = lcShpLine;
        FOR TotQty <> 0
      SCATTER MEMVAR MEMO
      *! B610051,1 MMT 08/16/2012 Receiving Inter-location PO to different location other than target location on PO gives error if Bin location is used[Start]
      m.cWareCode  =lcRecWareH
      *! B610051,1 MMT 08/16/2012 Receiving Inter-location PO to different location other than target location on PO gives error if Bin location is used[End]
      SELECT(lcPosLn)
      APPEND BLANK
      TABLEUPDATE(0,.T.)
      GATHER MEMVAR MEMO
      SELECT(lcPosLn)
      FOR I=1 TO 8
        IF laAcLRQy[I]<=0
          LOOP
        ENDIF
        Z=STR(I,1)
        lnQty&z = Qty&Z
        REPLACE Qty&Z WITH MAX(Qty&Z - laAcLRQy[I],0)
        laAcLRQy[I] = laAcLRQy[I] - lnQty&z
      ENDFOR
      REPLACE TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
      IF TotQty = 0
        DELETE
      ENDIF
    ENDSCAN
    SELECT (lcTmpLine)
  ENDIF

  *--Update Intransit line for Receive by Shipment or Receive inter Location P/o.
  *-- 'S' Receive by Shipment, 'O' Receive Inter-Location P/o
  *-- 'E' Receive Adornment order, 'L' Receive Inter Location P/O Batch
  *-- 'F' Receive Material PO Shipment, 'C' Receive Inter Location P/O Shipment
  IF lcRecvType $ 'SOELFC'
    FOR lnCnI = 1 TO 8
      lcCntI = STR(lnCnI,1)
      laReceive[lnCnI] = EVALUATE(lcTmpLine+'.Qty'+lcCntI) * lnConvFact
      laSHPQty[lnCnI]  = laSHPQty[lnCnI]  * lnConvFact
    ENDFOR

    *--Update Style record -----------------------
    *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
    *IF lcInvType="0001" AND SEEK(&lcTmpLine..Style,lcTempItem)
    *  =lfUpdItmFl(lcTempItem,'InTrans','-',@laSHPQty,@laReceive)
    IF SEEK(&lcTmpLine..STYLE,lcTempItem)
      =lfUpdItmFl(lcTempItem, 'InTrans', '-', @laSHPQty, @laReceive, lcInvType = "0002")
      *E039550,1 WSH 08/07/2005 [End]
      SELECT (lcTmpLine)
    ENDIF

    *--Update ItemLoc/StyDye Warehouse record -----------------------
    *B128741,1 KHM 06/30/2005 Use the original warehouse [Begin]
    *IF SEEK(IIF(lcInvType="0001","",lcInvType)+&lcTmpLine..Style+&lcTmpLine..cWareCode+SPACE(10),lcItemLoc)
    IF SEEK(IIF(lcInvType="0001","",lcInvType)+&lcTmpLine..STYLE+lcOrjWareH+SPACE(10),lcItemLoc)
      *B128741,1 KHM 06/30/2005 [End]
      =lfUpdItmFl(lcItemLoc,'InTrans','-',@laSHPQty,@laReceive)
      SELECT (lcTmpLine)
    ENDIF

    *--Update ItemLoc/StyDye Warehouse/dyelot record -----------------------
    IF (llDyelot OR llFabDye) AND !EMPTY(&lcTmpLine..Dyelot) AND ;
        &lcTempItem..cDye_Flg='Y' AND;
        SEEK(IIF(lcInvType="0001","",lcInvType)+&lcTmpLine..STYLE+lcOrjWareH+lcDyelot,lcItemLoc)
      =lfUpdItmFl(lcItemLoc,'InTrans','-',@laSHPQty,@laReceive)
      SELECT (lcTmpLine)
    ENDIF

  ENDIF

  IF llStkLine
    *--G/L Array difinition and initialization.
    *-- Update general ledger entreis in gfStyCrl()
    IF llLinkToGl
      DECLARE laGLDistAr[2,13]
      laGLDistAr[1,1] = lcLinkCode
      laGLDistAr[2,1] = lcGLPLkC
      laGLDistAr[1,2] = IIF(lcInvType = "0001",'006','015')
      laGLDistAr[2,2] = '013'
      laGLDistAr[1,3] =  1
      laGLDistAr[2,3] = -1
      *N039541,1 KHM 12/12/2005 [Start]
      *STORE 'PO' TO laGLDistAr[1,4],laGLDistAr[2,4]
      DO CASE
        *-- Case Style PO
      CASE lcRecvType $ 'ISBROCL'
        STORE 'PO' TO laGLDistAr[1,4],laGLDistAr[2,4]
        *-- Case Material PO
      CASE lcRecvType $ 'PGF'
        STORE 'MO' TO laGLDistAr[1,4],laGLDistAr[2,4]
        *-- Case Cutting Ticket
      CASE lcRecvType $ 'MT'
        STORE 'CT' TO laGLDistAr[1,4],laGLDistAr[2,4]
        *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
      CASE lcRecvType ='W'
        STORE 'MM' TO laGLDistAr[1,4],laGLDistAr[2,4]
        *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
      ENDCASE
      *N039541,1 KHM 12/12/2005 [End]
      STORE &lcTmpLine..PO TO laGLDistAr[1,5],laGLDistAr[2,5]
      STORE ldTrDate   TO laGLDistAr[1,6],laGLDistAr[2,6]
      STORE lcGLFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
      STORE lcGlPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
      STORE lcGlDist   TO laGLDistAr[1,9],laGLDistAr[2,9]
    ELSE
      DIME laGLDistAr[1,1]
      laGLDistAr = ''
    ENDIF

    SELECT (lcTmpLine)
    lcJTType = IIF(lcInvType = "0001",IIF(llMFCall,'5','6'),IIF(lcRecvType $ 'G','6','5'))
    lcJrlSty = IIF(TranCd = '4',cRetSty,STYLE)
    lcJDyelt = Dyelot
    lnJSgn   = IIF(lcRecvType $ 'RG',-1,1)
    DECLARE laAdjust[9]
    FOR I = 1 TO 8
      Z=STR(I,1)
      laAdjust[I] = (lnJSgn*Qty&Z) * lnConvFact
    ENDFOR
    laAdjust[9] = (lnJSgn*TotQty) * lnConvFact

    *--Call the global function for update style inventory control.
    PRIVATE lcRefer
    IF llMFCall
      SELECT (lcMastOprDt)
      =SEEK('M'+&lcTmpLine..PO+&lcTmpLine..clastopr+&lcTmpLine..cLotNo+'2')
      LOCATE REST WHILE cIMTyp+cTktNo+cOprCode+TranCd = ;
        'M'+&lcTmpLine..PO+&lcTmpLine..clastopr+&lcTmpLine..cLotNo+'2' ;
        FOR ITEM = &lcTmpLine..STYLE
      lcRefer = "VEN. " + &lcMastOprDt..CCONTCODE+ &lcMastOprDt..CCONTNAME
    ELSE
      IF !(lcRecvType $ 'E')
        lcRefer = "VEN. " + &lcTmpLine..Vendor + ' ' + &lcVendFile..cVenComp
      ELSE
        lcRefer = SPACE(0)
      ENDIF
    ENDIF
    lnNxtStp = 0
    IF lcInvType = "0001"
      *B608773,1 WAM 12/24/2008 Update the WIP account with the received landed cost
      IF llLinkToGl AND lcCostMth = 'S' AND INLIST(laGLDistAr[1,4],'CT','PO')

        *B609048,1 WAM 10/19/209 PO receipt update GLDIST incorrectly using Standard Cost [Start]
        lcCstShtType  = IIF(llMFCall,'M','I')
        IF loFormSet.llMulCurr
          lcSqlStatement  = "SELECT * FROM BOM WHERE cInvType = '"+lcInvType+"' AND cItmMajor = '"+SUBSTR(&lcTmpLine..STYLE,1,loFormSet.lnMjrWid)+ "'"+;
            " AND cCstShtTyp ='" + lcCstShtType + "' AND ccstsht_id IN (SELECT cCstSht_Id FROM BOMHEADR WHERE cInvType = '"+;
            lcInvType+"' AND cItmMajor = '"+SUBSTR(&lcTmpLine..STYLE,1,loFormSet.lnMjrWid)+ "' AND lDefCstSht=1)"
          =lfOpenSql(lcSqlStatement,'',lcTmpBom, "","",.F.)
          DECLARE laCosts(7),laECosts(7), laCurrency[7,3]
          STORE '' TO laCurrency
          STORE 0 TO laCosts(7),laECosts(7)
          =oCostSheet.mGetStyClrCst(&lcTmpLine..STYLE, &lcTmpLine..SCALE, llMscale, lcTmpBom, "", @laCosts, @laECosts, @laCurrency)
        ELSE
          SELECT (lcTempItem)
          SCATTER FIELDS ('N'+lcCstShtType+'COST1'),('N'+lcCstShtType+'COST2'),('N'+lcCstShtType+'COST3'),;
            ('N'+lcCstShtType+'COST4'),('N'+lcCstShtType+'COST5'),('N'+lcCstShtType+'COST6'),('N'+lcCstShtType+'COST7') TO laECosts
        ENDIF
        lnNewCost = laECosts[1]+laECosts[2]+laECosts[3]+laECosts[4]+laECosts[5]+laECosts[6]+laECosts[7]
        *B609048,1 WAM 10/19/209 PO receipt update GLDIST incorrectly using Standard Cost [End]

        laGLDistAr[2,3] = -1 * lnOrgCost /lnNewCost
      ENDIF
      *B608773,1 WAM 12/24/2008 (End)

      lnNxtStp = gfStyCrl(lcJTType,lcJrlSty,&lcTmpLine..cWareCode,lcJDyelt,ldRecDate,;
        &lcTmpLine..PO,@laAdjust,lnNewCost,lcRefer,lcGlSession,'',;
        lnNxtStp,lcTmpLine,'nSteps',@laGLDistAr,;
        &lcTmpLine..LINENO,lcFrstSess)
    ELSE
      PRIVATE laOtherPar
      DIMENSION laOtherPar[2,2]
      laOtherPar[1,1] = 'lnLineNo'
      laOtherPar[1,2] = &lcTmpLine..LINENO
      laOtherPar[2,1] = 'lcRelCode'
      laOtherPar[2,2] = &lcTempItem..cConvBuy

      =gfItemCrl(lcJTType,lcInvType,lcJrlSty,&lcTmpLine..cWareCode,&lcTmpLine..Dyelot,ldRecDate,;
        ldTrDate,EVALUATE(lcTmpLine+'.PO'),@laAdjust,lnNewCost/lnConvFact,lcRefer,'','',;
        @laGLDistAr,lcGlSession,EVALUATE(lcTmpLine+'.cBusDocu'),;
        EVALUATE(lcTmpLine+'.cStyType'),lcGlSession,'','','',.F.,;
        '',.F.,@laOtherPar)
    ENDIF

    *T20071102.0018(C200876) TMI Validate Bin Location [Start]
    *tmi 03/27/2008 SP9 fixes [START]
    *IF ASCAN(loFormSet.laEvntTrig,PADR('DLSBNPOR',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
    IF TYPE('loFormSet')='O' AND ASCAN(loFormSet.laEvntTrig,PADR('DLSBNPOR',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
      *tmi 03/27/2008 SP9 fixes [END]
      SELECT (lcTmpLine)
      =gfDoTriger("POSTREC",PADR("DLSBNPOR",10))
    ENDIF
    *T20071102.0018(C200876) TMI [End  ]

    *B608773,1 WAM 12/24/2008 Get equivalent standard cost for each costing element from BOM file
    IF llLinkToGl AND lcCostMth = 'S' AND INLIST(laGLDistAr[1,4],'CT','PO')
      lcCstShtType  = IIF(llMFCall,'M','I')
      IF loFormSet.llMulCurr
        lcSqlStatement  = "SELECT * FROM BOM WHERE cInvType = '"+lcInvType+"' AND cItmMajor = '"+SUBSTR(&lcTmpLine..STYLE,1,loFormSet.lnMjrWid)+ "'"+;
          " AND cCstShtTyp ='" + lcCstShtType + "' AND ccstsht_id IN (SELECT cCstSht_Id FROM BOMHEADR WHERE cInvType = '"+;
          lcInvType+"' AND cItmMajor = '"+SUBSTR(&lcTmpLine..STYLE,1,loFormSet.lnMjrWid)+ "' AND lDefCstSht=1)"
        =lfOpenSql(lcSqlStatement,'',lcTmpBom, "","",.F.)
        DECLARE laCosts(7),laECosts(7), laCurrency[7,3]
        STORE '' TO laCurrency
        STORE 0 TO laCosts(7),laECosts(7)
        =oCostSheet.mGetStyClrCst(&lcTmpLine..STYLE, &lcTmpLine..SCALE, llMscale, lcTmpBom, "", @laCosts, @laECosts, @laCurrency)
      ELSE
        SELECT (lcTempItem)
        SCATTER FIELDS ('N'+lcCstShtType+'COST1'),('N'+lcCstShtType+'COST2'),('N'+lcCstShtType+'COST3'),;
          ('N'+lcCstShtType+'COST4'),('N'+lcCstShtType+'COST5'),('N'+lcCstShtType+'COST6'),('N'+lcCstShtType+'COST7') TO laECosts
      ENDIF
      *B608773,1 WAM 12/24/2008 If receive cost is different than the standard cost , update the cost of goods variance
      FOR lnCount = 1 TO 7
        *-- Item Estimated landed cost
        lnIELanCost = EVALUATE(lcTmpLine+'.nLan_Cost'+STR(lnCount,1))
        lnIEStnCost = laECosts[lnCount]
        IF lnIELanCost <> lnIEStnCost
          lcGLTrnTyp = IIF(llMFCall,'CT','PO')
          *-- Cost of goods variance GL account
          lcLinkCode = &lcMastPoHd..Link_Code
          lcGlCatg = PADL(ALLTRIM(STR(21+lnCount,3)),3,'0')
          =GFSEEK(lcLinkCode +lcGlCatg,'GL_LINK')

          DO GlDist WITH IIF(!EMPTY(lcLinkCode),lcLinkCode ,'DEFDEF'),IIF(EMPTY(GL_LINK.GlAcnt),'019',lcGlCatg ),;
            &lcTmpLine..TotQty* lnConvFact*(lnIELanCost - lnIEStnCost),lcGLTrnTyp,&lcTmpLine..PO,ldRecDate,;
            lcGlFYear,lcGlPeriod,lcGlDist
        ENDIF
      ENDFOR
    ENDIF
    SELECT (lcTmpLine)
    *B608773,1 WAM 12/24/2008 (End)

    *!*	     *C200444,1 ALB Add GL entries when recieving PO [Begin]
    *!*	     IF ASCAN(laEvntTrig , PADR('UPDTRCGL',10)) <> 0
    *!*	       =gfDoTriger('POSTREC',PADR('UPDTRCGL',10))
    *!*	     ENDIF
    *!*	     *C200444,1 ALB Add GL entries when recieving PO [end]


    *!*	    IF lcRecvType = 'E'  AND llLinkToGl
    *!*	      lcBomType = '3'
    *!*	      FOR lnI = 1 TO 5
    *!*	        lcI = STR(lnI,1)
    *!*	        IF lcIType&lcI = 'M'
    *!*	          lcBomType = lcI
    *!*	          EXIT
    *!*	        ENDIF
    *!*	      ENDFOR


    *!*	      SELECT BomCost
    *!*	      APPEND BLANK
    *!*	      REPLACE cTktNo       WITH lcSvCode                                 ,;
    *!*	              MfgCode      WITH '**'                                     ,;
    *!*	              NTotQty      WITH &lcTmpLine..TotQty                       ,;
    *!*	              nUNitCst     WITH &lcTmpLine..nCost2                       ,;
    *!*	              nTotCst      WITH &lcTmpLine..nCost2*&lcTmpLine..TotQty    ,;
    *!*	              nUnitACst    WITH &lcTmpLine..nAct_Cst2                    ,;
    *!*	              nTotACst     WITH &lcTmpLine..nAct_Cst2*&lcTmpLine..TotQty ,;
    *!*	              dTranDate    WITH ldRcvDate                                ,;
    *!*	              cISession    WITH lcGlSession                              ,;
    *!*	              cCostType    WITH 'M'                                      ,;
    *!*	              cBOMType     WITH lcBomType                                ,;
    *!*	              cIMTyp       WITH 'M'                                      ,;
    *!*	              Actualize    WITH 'Y'

    *!*	      DO GLDIST WITH lcGLPLkC,'018',-BOMCost.nTotCst,'NL',lcSvCode,ldTrDate,;
    *!*	                     lcGLFYear,lcGlPeriod,lcGlDist,''
    *!*	      SELECT BOMCost
    *!*	      REPLACE cLbltyAcnt WITH &lcGlDist..GLAccount

    *!*	      DO GLDIST WITH lcGLPLkC,'013',BOMCost.nTotCst,'NL',lcSvCode,ldTrDate,;
    *!*	                     lcGLFYear,lcGlPeriod,lcGlDist,''
    *!*	      SELECT BOMCost
    *!*	      REPLACE cWIPAcnt WITH &lcGlDist..GLAccount
  ENDIF

  *!*	  *E301484,1 WAM Send Inter Location PO or Purchase orders to sites
  *!*	  IF lcPType = 'L' AND !llFromEDI AND 'NC' $ gcCmpModules
  *!*	    =SEEK(PADR(&lcTmpLine..Vendor,6),'WAREHOUS')
  *!*	    IF WAREHOUS.cSiteId = gcCurSite
  *!*	      =SEEK(&lcTmpLine..cwarecode,'WAREHOUS')
  *!*	      SELECT EDiAcPrt
  *!*	      LOCATE FOR cSiteId = WAREHOUS.cSiteId
  *!*	      IF FOUND() AND EdiAcPrt.lInterComp AND SEEK(EdiAcPrt.cPartCode+'856','EdiPd')
  *!*	        SELECT EdiTrans
  *!*	        *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char
  *!*	        *IF !SEEK('856'+PADR(&lcTmpLine..Po,20)+EdiAcPrt.Type+EdiAcPrt.cPartner)
  *!*	        IF !SEEK('856'+PADR(&lcTmpLine..Po,40)+EdiAcPrt.Type+EdiAcPrt.cPartner)
  *!*	        *E037853,1 [End]
  *!*	          INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER,lInterComp) VALUES ;
  *!*	                                   ('856',&lcTmpLine..Po,EdiAcPrt.Type,EdiAcPrt.cPartner,EdiAcPrt.lInterComp)
  *!*	        ENDIF
  *!*	        REPLACE cStatus WITH 'N'
  *!*	        =gfAdd_Info('EDITRANS')
  *!*	      ENDIF
  *!*	    ENDIF
  *!*	  ENDIF
  *!*	  *E301484,1 WAM (End)

ENDSCAN

*B608773,1 WAM 12/24/2008 Get equivalent standard cost for each costing element from BOM file
IF TYPE('oCostSheet') = 'O'
  RELEASE oCostSheet
ENDIF
*B608773,1 WAM 12/24/2008 (End)

*--Update Audit Trail.
*!*	IF !llFromEdi AND ASCAN(laEvntTrig,PADR("RECV_ADO",10)) <> 0
*!*	  SELECT (lcTmpLine)
*!*	  SELECT DIST PO,SHIPNO,PADR(REFERENCE,6) AS ORDER FROM (lcTmpLine) INTO CURSOR 'POAudtTl'
*!*	  SELECT POAudtTl
*!*	  SCAN
*!*	    =gfDoTriger('MFRCVAR',PADR("RECV_ADO",10))
*!*	  ENDSCAN
*!*	  USE
*!*	ENDIF

*-------------------------- Begin updating Master files ---------------------------------

*-- Begin transaction - Updating Fox tables Remotely

*B609297 TMI 06/14/2010  call the Garbag collection [start] tmi 6/3/2010
WAIT WINDOW NOWAIT ''
&& This is a testing code used to deal with an elusive problem appeared with Tony that the PO Receiving screen crashes with
&& TOO MANY VARIABLES error
&& this system function acts as a garbage collector for VFP, it is used to clear the buffer of a cursor
&& is also supposed to release variables no more defined from the memory to free more memory, same comments
&& applies to all occurences of this code in this program

&& I added the condition that this is ENG as this function may cause some delay , so we may try it first in a small area
IF UPPER(ALLTRIM(oAriaapplication.DefaultCountry)) == 'ENG'
  =SYS(1104)
ENDIF
**-- call the Garbag collection [end  ] tmi 6/3/2010
*B609297 TMI 06/14/2010 [End  ]

LOCAL lnConnectionHandlar, lcTranCode
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.cAriaNativeDataFilesConStr,3,'',.T.)
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
  RETURN .F.
ENDIF
*-- 1) Updating style file (FOX) in case of receiving by styles
IF lcInvType = '0001'
  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcTempItem,lcTranCode,;
    SET("DataSession"),'STYLE','STYLE')

  IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
    RETURN .F.
  ELSE
    =TABLEUPDATE(.T.,.T.)
  ENDIF

  *-- 2) Updating stydye file (FOX)
  SELECT (lcItemLoc)
  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcItemLoc,lcTranCode,;
    SET("DataSession"),'Style+cWareCode+dyelot','STYDYE')

  IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
    RETURN .F.
  ELSE
    =TABLEUPDATE(.T.,.T.)
  ENDIF
ENDIF

*-- 3) Updating vendor file (FOX)
*-- Case not 'M' Receive C/T, 'T' Receive C/T Batch , 'O' Receive Inter-Location P/o
*--          'C' Receive Inter Location P/O Shipment, 'L' Receive Inter Location P/O Batch
*--          'E' Receive Adornment order
IF !llMFCall AND !(lcRecvType $ 'OELC')
  SELECT (lcVendFile)
  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcVendFile,lcTranCode,;
    SET("DataSession"),'cVendCode','APVENDOR')

  IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
    RETURN .F.
  ELSE
    =TABLEUPDATE(.T.,.T.)
  ENDIF
ENDIF

*-- 4) Updating WhsLoc file (FOX)
IF llWareLoc AND TYPE('lcTempLoc') = 'C' AND USED(lcTempLoc)
  SELECT (lcTempLoc)
  LOCATE
  IF !EOF()
    lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcTempLoc,lcTranCode,;
      SET("DataSession"),'Style+Color+cWareCode+cLocation','WHSLOC')

    IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
      =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
      RETURN .F.
    ELSE
      =TABLEUPDATE(.T.,.T.)
    ENDIF
  ENDIF
ENDIF

*-- 5) Updating the GLDIST file (FOX)
IF llLinkToGl
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
  *WAIT WINDOW 'Updating General Ledger Distribution File ' NOWAIT
  IF !loFormSet.llSilentMod
    WAIT WINDOW 'Updating General Ledger Distribution File ' NOWAIT
  ENDIF
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

  SELECT (lcGlDist)
  DO CASE
  CASE lcInvType = '0002' AND lcRecvType $ 'PF'
    REPLACE ALL GlSession WITH lcGlSession,;
      Tran_Desc WITH 'MAT. P/O RECEIVING'
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
  CASE lcInvType = '0002' AND lcRecvType = 'W'
    REPLACE ALL GlSession WITH lcGlSession,;
      Tran_Desc WITH 'RECEIVE M.F.G. ORDER'
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]

  CASE lcInvType = '0002' AND lcRecvType = 'G'
    REPLACE ALL GlSession WITH lcGlSession,;
      Tran_Desc WITH 'MAT. P/O ISSUING'

  CASE lcRecvType $ 'OCL'
    REPLACE ALL GlSession WITH lcGlSession,;
      Tran_Desc WITH 'INTER LOC. RECEIVING'
  OTHERWISE
    REPLACE ALL GlSession WITH lcGlSession
  ENDCASE

  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcGlDist,lcTranCode,;
    SET("DataSession"),'Tran_No+Tran_Type+GlSession+Catg_Key','GLDIST')

  IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
    RETURN .F.
  ELSE
    =TABLEUPDATE(.T.,.T.)
  ENDIF
ENDIF

*tmi 05/13/2008 no need for this part
*!*	*T20060818.0001(C200876) TMI [Start] Save Locations in WHBINLOC File from PO
*!*	*tmi 03/27/2008 SP9 fixes [start]
*!*	*IF ASCAN(loFormSet.laEvntTrig,PADR('SAVBINS',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
*!*	IF TYPE('loFormSet')='O' AND ASCAN(loFormSet.laEvntTrig,PADR('SAVBINS',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
*!*	  *tmi 03/27/2008 SP9 fixes [end]
*!*	  SELECT (lcTmpLine)
*!*	  =gfDoTriger("POSTREC",PADR("SAVBINS",10))
*!*	ENDIF
*!*	*T20060818.0001(C200876) TMI [End  ]
*tmi 05/13/2008 no need for this part

*-- 6) Updating EDI files
*-- Send Product Activity to Back Office
IF INLIST(lcRecvType,'O','L') AND 'NC' $ oAriaApplication.CompanyInstalledModules
  =gfOpenFile(oAriaApplication.DataDir+'CODES',oAriaApplication.DataDir+'CODES','SH')
  SELECT CODES
  SET ORDER TO TAG Idrltfname
  =SEEK('NYCSITEID')
  LOCATE REST WHILE cdefcode+crltfield+cfld_name = 'NYCSITEID' ;
    FOR   cRltd_Nam = 'CCMSITETYP' AND cRltd_Vlu= 'B'
  IF FOUND()
    lcSiteId = Codes.cCode_No
    SELECT EDiAcPrt
    LOCATE FOR cSiteId = lcSiteId
    IF FOUND() AND SEEK(EdiAcPrt.cPartCode+'852','EdiPd')
      lcDateKey = DTOS(ldRecDate)+'-'+DTOS(ldRecDate)
      SELECT EdiTrans
      IF !SEEK('852'+PADR(lcDateKey,40)+EdiAcPrt.TYPE+EdiAcPrt.cPartner)
        INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER,LINTERCOMP) ;
          VALUES ('852',lcDateKey,EdiAcPrt.TYPE,EdiAcPrt.cPartner,;
          EdiAcPrt.lInterComp)
      ENDIF
      REPLACE cStatus WITH 'N'
    ENDIF
  ENDIF
  SET ORDER TO TAG CODES IN CODES
ENDIF

*-- Commit updating FOX tables
lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode,.T.)
IF lnConnectionHandlar # 1
  =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF
*------------------------------- End Updating Fox Tables -------------------------

*-- Begin transaction - Updating SQL tables
*B609297 TMI 06/14/2010 [Start]
**-- call the Garbag collection [start] tmi 6/3/2010
WAIT WINDOW NOWAIT ''
IF UPPER(ALLTRIM(oAriaapplication.DefaultCountry)) == 'ENG'
  =SYS(1104)
ENDIF
**-- call the Garbag collection [end  ] tmi 6/3/2010
*B609297 TMI 06/14/2010 [End  ]

lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode)
  RETURN .F.
ENDIF

*-- 1) Updating Itemloc (SQL)
IF lcInvType = '0002'
  SELECT(lcItemLoc)

  *B609297 TMI 06/14/2010 [Start]
  **-- call the Garbag collection [start] tmi 6/3/2010
  WAIT WINDOW NOWAIT ''
  IF UPPER(ALLTRIM(oAriaapplication.DefaultCountry)) == 'ENG'
    =SYS(1104)
  ENDIF
  **-- call the Garbag collection [end  ] tmi 6/3/2010
  *B609297 TMI 06/14/2010 [End  ]

  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcItemLoc,lcTranCode,;
    SET("DataSession"),'cInvType,Style,cWareCode,Dyelot','ITEMLOC','STYDYE')
  IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
    lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
    RETURN .F.
  ELSE
    =TABLEUPDATE(.T.,.T.)
  ENDIF

  *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
  SELECT(lcTempItem)

  *B609297 TMI 06/14/2010 [Start]
  **-- call the Garbag collection [start] tmi 6/3/2010
  WAIT WINDOW NOWAIT ''
  IF UPPER(ALLTRIM(oAriaapplication.DefaultCountry)) == 'ENG'
    =SYS(1104)
  ENDIF
  **-- call the Garbag collection [end  ] tmi 6/3/2010
  *B609297 TMI 06/14/2010 [End  ]

  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcTempItem,lcTranCode,;
    SET("DataSession"),'cInvType,Style','ITEM','STYLE')
  IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
    lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
    RETURN .F.
  ELSE
    =TABLEUPDATE(.T.,.T.)
  ENDIF
  *E039550,1 WSH 08/07/2005 [End]

  *!*	  *-- 2) Updating the itemjrnl file (SQL) in case of receiving materials
  *!*	  SELECT (lcTmpItmJrl)
  *!*	  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcTmpItmJrl,lcTranCode,;
  *!*	   	 			        SET("Datasession"),;
  *!*	                        'CINVTYPE,STYLE,CWARECODE,DTRDATE,CSESSION,CIRTYPE,CTRCODE,LINENO',;
  *!*	                        'ITEMJRNL','STYDATE')
  *!*	  IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
  *!*	    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
  *!*	    RETURN .F.
  *!*	  ELSE
  *!*	    =TABLEUPDATE(.T.,.T.)
  *!*	  ENDIF
ENDIF

*-- 3) Update master POSHDR (SQL)
SELECT(lcMastPoHd)

*B609297 TMI 06/14/2010 [Start]
**-- call the Garbag collection [start] tmi 6/3/2010
WAIT WINDOW NOWAIT ''
IF UPPER(ALLTRIM(oAriaapplication.DefaultCountry)) == 'ENG'
  =SYS(1104)
ENDIF
**-- call the Garbag collection [end  ] tmi 6/3/2010
*B609297 TMI 06/14/2010 [End  ]

lnRecNo = RECNO()
REPLACE lLok_Stat WITH .F.
*B000109,1 WAM 03/05/2005 Remove Lock PO header
*REPLACE ALL lLok_stat WITH .F. ,;
cLok_User WITH ''  ,;
dLok_Date WITH {}  ,;
cLok_Time WITH ''
*B000109,1 WAM 03/05/2005 (End)

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcMastPoHd,lcTranCode,;
  SET("DataSession"),'cBusDocu,cStyType,PO','POSHDR','POSHDR')
IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
  lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
  RETURN .F.
ELSE
  =TABLEUPDATE(.T.,.T.)
ENDIF

*-- 4) Update master ShpmtHdr (SQL)
*-- 'S' Receive by Shipment, 'C' Receive Inter Location P/O Shipment
*-- 'F' Receive Material PO Shipment
IF lcRecvType $ 'SFC' AND TYPE("lcMastShp") = "C" AND USED(lcMastShp)
  *B128070,1 KHM 05/19/2005 Initialize llChgStatus to indicate whether to change the
  *B128070,1                shipment status or not [Begin]
  LOCAL llChgStatus
  llChgStatus = .F.
  *B128070,1 KHM 05/19/2005 [End]

  SELECT (lcPosLn)
  IF lcRecvType $ 'SF'
    *B128070,1 KHM 05/19/2005 Comment the following line and check if there are still
    *B128070,1                         lines that have not been received yet [Begin]
    *LOCATE FOR ShipNo = lcShpCode AND TranCd = '3' AND TotQty > 0
    LOCAL laShipQty
    DIMENSION laShipQty[8]
    STORE 0 TO laShipQty
    SET ORDER TO TAG Poslnsh

    SELECT(lcMastPoLn)
    SET ORDER TO TAG Poslnsh
    SEEK lcShpCode
    SCAN REST WHILE ShipNo+cBusDocu+cStyType+PO+cInvType+STYLE+STR(LINENO,6)+TranCd = ;
        lcShpCode FOR TranCd = '3'
      lcKeyVal = ShipNo+cBusDocu+cStyType+PO+cInvType+STYLE+STR(LINENO,6)
      laShipQty = 0
      IF SEEK(lcKeyVal+'2', lcPosLn) OR SEEK(lcKeyVal+'4', lcPosLn) OR +;
          SEEK(lcKeyVal+'5', lcPosLn)
        SELECT (lcPosLn)
        SUM REST Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 ;
          WHILE SHIPNO+CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD =;
          lcKeyVal FOR TranCd $ '245' TO ARRAY laShipQty
        FOR lnVal = 1 TO 8
          lcVal = STR(lnVal,1)
          laShipQty[lnVal] = MAX(EVALUATE(lcMastPoLn+'.Qty'+lcVal) - laShipQty[lnVal],0)
        ENDFOR
        IF laShipQty[1]+laShipQty[2]+laShipQty[3]+laShipQty[4]+laShipQty[5]+laShipQty[6]+;
            laShipQty[7]+laShipQty[8] > 0
          llChgStatus = .T.
          EXIT
        ENDIF
      ELSE
        llChgStatus = .T.
        EXIT
      ENDIF
    ENDSCAN
    *B128070,1 KHM 05/19/2005 [End]
  ELSE
    SELECT(lcMastPoLn)
    LOCATE FOR ShipNo = lcShpCode AND TranCd = '3' AND TotQty > 0
    *B128070,1 KHM 05/19/2005 Use llChgStatus instead of FOUND() [Begin]
    llChgStatus = FOUND()

  ENDIF
  *B128070,1 KHM 05/19/2005 Use llChgStatus instead of FOUND() [Begin]
  *lcShpStat = IIF(FOUND(),'O','C')
  lcShpStat = IIF(llChgStatus,'O','C')
  *B128070,1 KHM 05/19/2005 [End]
  SELECT (lcMastShp)
  LOCATE
  *B128070,1 KHM 05/19/2005 Add the replacement of stock, damage and cancel quantity [Begin]
  *REPLACE Status     WITH lcShpStat
  REPLACE STATUS   WITH lcShpStat,;
    Recv_Stk WITH Recv_Stk + lnShipStk,;
    Recv_Dam WITH Recv_Dam + lnShipDam ,;
    Recv_Can WITH Recv_Can + lnShipCan
  *B607880,1 SSH 12/17/2006 [Begin] Replace TotQtyHdr WIth ZERO in case recieve completely
  IF STATUS="C"
    REPLACE TotQtyHdr WITH 0
  ENDIF
  *B607880,1 SSH 12/17/2006 Replace TotQtyHdr WIth ZERO in case recieve completely
  *B128070,1 KHM 05/19/2005 [End]

  *B609297 TMI 06/14/2010 [Start]
  **-- call the Garbag collection [start] tmi 6/3/2010
  WAIT WINDOW NOWAIT ''
  IF UPPER(ALLTRIM(oAriaapplication.DefaultCountry)) == 'ENG'
    =SYS(1104)
  ENDIF
  **-- call the Garbag collection [end  ] tmi 6/3/2010
  *B609297 TMI 06/14/2010 [End  ]

  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcMastShp,lcTranCode,;
    SET("DataSession"),;
    'CBUSDOCU,CSHPTYPE,SHIPNO',;
    'SHPMTHDR','SHPMTHDR')
  IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
    lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
    RETURN .F.
  ELSE
    =TABLEUPDATE(.T.,.T.)
  ENDIF
ENDIF

*-- 5) Update master POSLN (SQL)
SELECT(lcPosLn)

*B609297 TMI 06/14/2010 [Start]
**-- call the Garbag collection [start] tmi 6/3/2010
WAIT WINDOW NOWAIT ''
IF UPPER(ALLTRIM(oAriaapplication.DefaultCountry)) == 'ENG'
  =SYS(1104)
ENDIF
**-- call the Garbag collection [end  ] tmi 6/3/2010
*B609297 TMI 06/14/2010 [End  ]

*B607585,1 AMH Add cwarecode to POSLN index and nLineNo to BOMLINE file [Start]
*lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcPosLn,lcTranCode,;
SET("DataSession"),;
'cBusDocu,cStyType,PO,cRSession,ShipNo,cInvType,Style,LineNo,TranCd,cStyGrade',;
'POSLN','POSREC')
lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcPosLn,lcTranCode,;
  SET("DataSession"),;
  'cBusDocu,cStyType,PO,cRSession,ShipNo,cInvType,Style,LineNo,TranCd,cStyGrade,cWareCode',;
  'POSLN','POSREC')
*B607585,1 AMH [End]

IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
  lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
  RETURN .F.
ELSE
  =TABLEUPDATE(.T.,.T.)
ENDIF

*-- 6) Update master BomLine
*-- 'P' Material PO, 'F' Material PO Shipment
IF !(lcRecvType $ 'PFG')
  SELECT(lcTmpBomLn)

  *B609297 TMI 06/14/2010 [Start]
  **-- call the Garbag collection [start] tmi 6/3/2010
  WAIT WINDOW NOWAIT ''
  IF UPPER(ALLTRIM(oAriaapplication.DefaultCountry)) == 'ENG'
    =SYS(1104)
  ENDIF
  **-- call the Garbag collection [end  ] tmi 6/3/2010
  *B609297 TMI 06/14/2010 [End  ]

  *B607585,1 AMH Add cwarecode to POSLN index and nLineNo to BOMLINE file [Start]
  *lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcTmpBomLn,lcTranCode,;
  SET("DataSession"),;
  'CIMTYP,CTYPE,CTKTNO,SHIPNO,LINENO,CBOMTYP,CINVTYPE,STYLE,CINVTYPC,'+;
  'ITEM,MFGCODE,CRSESSION,CSTYGRADE',;
  'BOMLINE','BOMLINEU')
  *B609517,1 TMI 02/08/2011 [Start] I updated the program as the review stated but still there are duplicates, I devised this general function to remove a duplicates from a cursor
  ** this is the unique key fields of the table BOMLINE
  lcKeyFld = "CIMTYP+CTYPE+CTKTNO+SHIPNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE+CRSESSION+CSTYGRADE+STR(NLINENO,4)"
  lfRmvDupl(lcTmpBomLn,lcKeyFld,"nLineNo")
  *B609517,1 TMI 02/08/2011 [End  ]

  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcTmpBomLn,lcTranCode,;
    SET("DataSession"),;
    'CIMTYP,CTYPE,CTKTNO,SHIPNO,LINENO,CBOMTYP,CINVTYPE,STYLE,CINVTYPC,'+;
    'ITEM,MFGCODE,CRSESSION,CSTYGRADE,NLINENO',;
    'BOMLINE','BOMLINEU')
  *B607585,1 AMH [End]

  IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
    lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
    RETURN .F.
  ELSE
    =TABLEUPDATE(.T.,.T.)
  ENDIF
ENDIF

*-- 7) Update master MfgOprDt
*-- 'P' Material PO, 'F' Material PO Shipment
IF !(lcRecvType $ 'PFG') AND USED(lcMastOprDt)
  SELECT(lcMastOprDt)
  LOCATE
  IF !EOF()

    *B609297 TMI 06/14/2010 [Start]
    **-- call the Garbag collection [start] tmi 6/3/2010
    WAIT WINDOW NOWAIT ''
    IF UPPER(ALLTRIM(oAriaapplication.DefaultCountry)) == 'ENG'
      =SYS(1104)
    ENDIF
    **-- call the Garbag collection [end  ] tmi 6/3/2010
    *B609297 TMI 06/14/2010 [End  ]

    lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcMastOprDt,lcTranCode,;
      SET("DataSession"),;
      'CIMTYP,CTKTNO,COPRCODE,CLOTNO,TRANCD,CINVTYPE,ITEM,CDYELOT',;
      'MFGOPRDT','MFGOPRITME')
    IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
      =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
      lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
      RETURN .F.
    ELSE
      =TABLEUPDATE(.T.,.T.)
    ENDIF
  ENDIF
ENDIF

*- this file to update the usage in the itemloc
IF lcInvType = "0001" AND TYPE('lcTmpItmDye') = 'C' AND USED(lcTmpItmDye)
  SELECT(lcTmpItmDye)
  LOCATE
  IF !EOF()

    *B609297 TMI 06/14/2010 [Start]
    **-- call the Garbag collection [start] tmi 6/3/2010
    WAIT WINDOW NOWAIT ''
    IF UPPER(ALLTRIM(oAriaapplication.DefaultCountry)) == 'ENG'
      =SYS(1104)
    ENDIF
    **-- call the Garbag collection [end  ] tmi 6/3/2010
    *B609297 TMI 06/14/2010 [End  ]

    lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcTmpItmDye,lcTranCode,;
      SET("DataSession"),'cInvType,Style,cWareCode,Dyelot','ITEMLOC','STYDYE')
    IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
      =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
      lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
      RETURN .F.
    ELSE
      =TABLEUPDATE(.T.,.T.)
    ENDIF
  ENDIF
ENDIF

*E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
IF lcInvType = "0001" AND TYPE('lcTmpItm') = 'C' AND USED(lcTmpItm)
  SELECT(lcTmpItm)
  LOCATE
  IF !EOF()

    *B609297 TMI 06/14/2010 [Start]
    **-- call the Garbag collection [start] tmi 6/3/2010
    WAIT WINDOW NOWAIT ''
    IF UPPER(ALLTRIM(oAriaapplication.DefaultCountry)) == 'ENG'
      =SYS(1104)
    ENDIF
    **-- call the Garbag collection [end  ] tmi 6/3/2010
    *B609297 TMI 06/14/2010 [End  ]

    lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcTmpItm,lcTranCode,;
      SET("DataSession"),'cInvType,Style','ITEM','STYLE')
    IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
      =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
      lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
      RETURN .F.
    ELSE
      =TABLEUPDATE(.T.,.T.)
    ENDIF
  ENDIF
ENDIF
*E039550,1 WSH 08/07/2005 [End]

*tmi 05/13/2008 no need for this trigger , this code must be removed [begin]
*!*	*T20060818.0001(C200876) TMI [Start] Updating the BININVJL,WHBINLOC sql files
*!*	*tmi 03/27/2008 SP9 fixes [start]
*!*	*IF ASCAN(loFormSet.laEvntTrig,PADR('UPDBNFLS',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
*!*	IF TYPE('loFormSet')='O' AND ASCAN(loFormSet.laEvntTrig,PADR('UPDBNFLS',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
*!*	*tmi 03/27/2008 SP9 fixes [end]
*!*	  PRIVATE lcTrnCod
*!*	  lcTrnCod = lcTranCode
*!*	  IF !gfDoTriger("POSTREC",PADR("UPDBNFLS",10))
*!*	    RETURN .F.
*!*	  ENDIF
*!*	ENDIF
*!*	*T20060818.0001(C200876) TMI [End  ]
*tmi 05/13/2008 no need for this trigger , this code must be removed [end]

*!*	*-- To be done later
*!*	*-- 11) Update CtktRcvH
*!*	IF lcRecvType $ 'BTL'
*!*	  SELECT CTKTRCVH
*!*	  REPLACE cStatus    WITH 'P'
*!*	ENDIF

WAIT CLEAR

*!*	*-- 13) Updating CutPick file (SQL)
*!*	IF "SO" $ oAriaApplication.CompanyInstalledModules AND !(lcRecvType$'R') AND USED(lcTmpCtPk)
*!*	  SELECT (lcTmpLine)
*!*	  SCAN FOR TranCd='1' AND lAloChg
*!*	    SCATTER FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd TO laNewOrd
*!*	    =SEEK(cStyType+Po+Style+STR(LineNo,6)+TranCd,lcPosLn)

*!*	    SELECT (lcPosLn)
*!*	    GATHER FROM laNewOrd FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
*!*	    SELECT (lcTmpLine)
*!*	    REPLACE lAloChg WITH .F.
*!*	  ENDSCAN
*!*
*!*	  SELECT (lcTmpCtPk)
*!*	  SCAN
*!*	    lcCpkky = Trancd+cTktNo+Style
*!*	    SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laNewOrd
*!*	    SCATTER FIELDS nCurPck1,nCurPck2,nCurPck3,nCurPck4,nCurPck5,nCurPck6,nCurPck7,nCurPck8,nCurPck9 TO laOldOrd
*!*	    SELECT CUTPICK
*!*	    SEEK lcCpkky
*!*	    LOCATE REST WHILE Trancd+cTktNo+Style=lcCpkky FOR cOrdLine=&lcTmpCtPk..cOrdLine
*!*	    IF FOUND()
*!*	      GATHER FROM laNewOrd FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty
*!*	      =SEEK('O'+CUTPICK.Order,'ORDHDR')
*!*	      =SEEK('O'+CUTPICK.Order+STR(INT(VAL(CUTPICK.cOrdLine)),6),'ORDLINE')
*!*	      FOR lnSizeNo = 1 TO 8
*!*	        lcSz = STR(lnSizeNo,1)
*!*	        REPLACE ORDHDR.TotCut    WITH ORDHDR.TotCut    - laOldOrd[lnSizeNo] + laNewOrd[lnSizeNo]
*!*	        REPLACE ORDLINE.TotCut   WITH ORDLINE.TotCut   - laOldOrd[lnSizeNo] + laNewOrd[lnSizeNo],;
*!*	                ORDLINE.Cut&lcSz WITH ORDLINE.Cut&lcSz - laOldOrd[lnSizeNo] + laNewOrd[lnSizeNo]
*!*	      ENDFOR
*!*	      SELECT CUTPICK
*!*	      IF CUTPICK.TotQty = 0
*!*	        SELECT CUTPICK
*!*	        DELETE
*!*	      ENDIF
*!*	    ENDIF
*!*	  ENDSCAN
*!*	ENDIF

*-- Checking if the material module is installed and system is setup to use dyelot
*-- and the material uses dyelot.
*!*	IF 'MA' $ oAriaApplication.CompanyInstalledModules AND llDyelot
*!*	  = gfArDyRl('' , '' , lcTmDyeRel,.T.)
*!*	ENDIF

*-- Commit updating of SQL files

*B609297 TMI 06/14/2010 [Start]
**-- call the Garbag collection [start] tmi 6/3/2010
WAIT WINDOW NOWAIT ''
IF UPPER(ALLTRIM(oAriaapplication.DefaultCountry)) == 'ENG'
  =SYS(1104)
ENDIF
**-- call the Garbag collection [end  ] tmi 6/3/2010
*B609297 TMI 06/14/2010 [End  ]

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)

*B609297 TMI 06/14/2010 [Start]
**-- call the Garbag collection [start] tmi 6/3/2010
WAIT WINDOW NOWAIT ''
IF UPPER(ALLTRIM(oAriaapplication.DefaultCountry)) == 'ENG'
  =SYS(1104)
ENDIF
**-- call the Garbag collection [end  ] tmi 6/3/2010
*B609297 TMI 06/14/2010 [End  ]

IF lnConnectionHandlar # 1
  =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran",lnConnectionHandlar)
  RETURN .F.
ENDIF
*--------------------------- End updating Master files ---------------------------------
WAIT CLEAR
*C131527,1 KHM

*B609295,1 TMI 06/14/2010 [Start] use "loFormSet.mDoTrigger" instead the old function gfDotriger to call the POTRAN for Blum & Fink
*!*	=gfDoTriger('POSTREC',PADR('POTRAN',10))
IF ASCAN(loFormSet.laEvntTrig,PADR('POTRAN',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
  =loFormSet.mDoTrigger(PADR('POTRAN',10))
ENDIF
*B609295,1 TMI 06/14/2010 [End  ]

*! C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
IF ASCAN(loformset.laEvntTrig,PADR('PRCSPUPREF',10),1,ALEN(loformset.laEvntTrig,1),1) > 0
  PUBLIC oPoRecRef
  oPoRecRef = loFormSet
ENDIF
*! C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]

*!C201141, 1 Hesham Elmasry(HES), 04/26/2009, Calling Cost Sheet Sheet Screen to complete the Issuing Process [Start]
IF ASCAN(loformset.laEvntTrig,PADR('CALCSSHSCR',10),1,ALEN(loformset.laEvntTrig,1),1) > 0
  loFormSet.mDoTrigger(PADR('CALCSSHSCR' ,10))
ENDIF
*!C201141, 1 Hesham Elmasry(HES), 04/26/2009, Calling Cost Sheet Sheet Screen to complete the Issuing Process [End]

*!* E303103,2 MMT 04/05/2012 Add trigger in PO receiving screen for [Start]
IF ASCAN(loformset.laEvntTrig,PADR('AUTALLOC',10),1,ALEN(loformset.laEvntTrig,1),1) > 0
  =loformset.mDoTrigger(PADR('AUTALLOC',10))
ENDIF
*!* E303103,2 MMT 04/05/2012 Add trigger in PO receiving screen for [END]



*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
RELEASE laSetups  , llWareHous, llWareLoc, llDyelot, llFabDye, lcCostMthM, lcCostMth,;
  llLinkToGl, lcDropLoc , llPOSale  , llImpCost, llMulCurr, llUseMCurr,;
  llEditExRt, llConfig  , llMFCall, llUnblProc, llShpRec
RELEASE  lcGLFYear, lcGlPeriod
RELEASE lcVendFile, lcMastPoHd, lcMastPoLn, lcPosLn, lcMastBomLn, lcTmpBomLn, lcTempItem, lcItemLoc,;
  lcTmpCur, lcGlDist, lcGlSession,  lcMastOprDt, lcTmpCode, lcTmpItmJrl, lcTmpItmDye, lcTmpItm
RELEASE laOpnQty,lnLstPoRAm,lnOpnPoAmt,lcLstTrn,lcPoVend,lcOrjWareH,;
  lcLotNo, lcChkPO, lcBomPTyp, lcBomPKey,;
  laReceive, lcCstShtTyp, lcPriceCur,lcDutyCur,lnConvFact,laPrevRecQ
RELEASE llMscale ,lcTmpBom
RELEASE lcSqlStatement,laIndex, m.vendor
RELEASE llStkLine,lcMainKy,lcOrjWareH
RELEASE lnNewCost,lnELanded,lnOrgCost,lcStyle,lcWareHous,lcDyelot   ,lclinkCode
RELEASE lcGLPLkC,lcCurSty,llFound
RELEASE lcWipSgn ,lcCntI ,lnCnI
RELEASE lnOpnSub,lnI,lcCnt
RELEASE lcFrstSess,lcBomLKey
RELEASE lcWhileCn , m.StyQty,m.ItemQty,m.ItemAmt, m.cRSession,lcLastOpr
RELEASE laSHPQty,laRecvQty,lcShpLine ,laAcLRQy,I,Z,lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8
RELEASE laGLDistAr,lcJTType,lcJrlSty,lcJDyelt,lnJSgn,laAdjust,lcRefer
RELEASE lcCstShtType, lnNxtStp
RELEASE laCosts,laECosts,laCurrency
RELEASE laOtherPar,lnCount,lnIELanCost,lnIEStnCost
RELEASE lcGlCatg,lnConnectionHandlar, lcTranCode
RELEASE lnRecNo, llChgStatus ,laShipQty,lcKeyVal ,lcShpStat ,lnResult
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]

*!***************************************************************************
*! Name      : lfGetStruc
*! Developer : Khalid Mohi El-Din
*! Date      : 09/11/2004
*! Purpose   : To get the structure of the style,stydye or itemloc to be able to add the
*!             styles or materials and update the WIP then pass it to SqlUpdate.
*!******************************************************************************
*! Parameters: lcInvType   : "0001" for Style, "0002" for Material
*!******************************************************************************
FUNCTION lfGetStruc
LPARAMETERS lcInvType

PRIVATE lcSqlStatement, laIndex

*! B609051,1 HES 10/20/2009 error receiving PO receipts because there is no index for the STYDYE file [Start]
IF !USED('STYDYE')
  =gfOpenTABLE(oAriaApplication.DATADIR+'STYDYE',oAriaApplication.DATADIR+'STYDYE','SH')
ELSE
  SELECT STYDYE
  gfSetOrder('STYDYE')
ENDIF
*! B609051,1 HES 10/20/2009 error receiving PO receipts because there is no index for the STYDYE file [End]

IF lcInvType = "0001"
  *-- To get the structure of the style file
  lcSqlStatement  = "SELECT * FROM STYLE WHERE 1 = 2"
  =lfOpenFox(lcSqlStatement,'STYLE',lcTempItem,"")
  DIMENSION laIndex[1,2]
  laIndex = ''
  laIndex[1,1] = 'Style'
  laIndex[1,2] = lcTempItem
  =lfSetIndex(lcTempItem,@laIndex)

  *-- To get the structure of the stydye file
  lcSqlStatement  = "SELECT * FROM STYDYE WHERE 1 = 2"
  =lfOpenFox(lcSqlStatement,'STYDYE',lcItemLoc,"")
  DIMENSION laIndex[1,2]
  laIndex = ''
  laIndex[1,1] = 'Style+cWareCode+Dyelot'
  laIndex[1,2] = lcItemLoc
  =lfSetIndex(lcItemLoc,@laIndex)
ELSE
  *-- To get the structure of the style file
  lcSqlStatement  = "SELECT TOP 0 * FROM ITEM [INDEX=STYLE]"
  =lfOpenSql(lcSqlStatement,'ITEM',lcTempItem,"","",.F.)
  DIMENSION laIndex[1,2]
  laIndex = ''
  laIndex[1,1] = 'Style'
  laIndex[1,2] = lcTempItem
  =lfSetIndex(lcTempItem,@laIndex)

  *-- To get the structure of the itemloc file
  lcSqlStatement  = "SELECT TOP 0 * FROM ITEMLOC [INDEX=STYDYE]"
  =lfOpenSql(lcSqlStatement,'ITEMLOC',lcItemLoc, "","",.F.)
  DIMENSION laIndex[1,2]
  laIndex = ''
  laIndex[1,1] = 'cInvType+Style+cWareCode+Dyelot'
  laIndex[1,2] = lcItemLoc
  =lfSetIndex(lcItemLoc,@laIndex)

  *-- To get the structure of the item journal file
  lcSqlStatement  = "SELECT TOP 0 * FROM ITEMJRNL [INDEX=STYINVJL]"
  =lfOpenSql(lcSqlStatement,'ITEMJRNL',lcTmpItmJrl, "","",.F.)
  DIMENSION laIndex[1,2]
  laIndex = ''
  laIndex[1,1] = 'cInvType+Style+cWareCode+DTOS(dTrDate)+cSession+cIrType+cTrCode+STR(LineNo,6)'
  laIndex[1,2] = lcTmpItmJrl
  =lfSetIndex(lcTmpItmJrl,@laIndex)

ENDIF
*-- To get the structure of the POSHDR file
lcSqlStatement  = "SELECT TOP 0 * FROM POSHDR [INDEX=POSHDR]"
=lfOpenSql(lcSqlStatement,'POSHDR',lcMastPoHd, "","",.F.)
DIMENSION laIndex[1,2]
laIndex = ''
laIndex[1,1] = 'cBusDocu+cStyType+PO'
laIndex[1,2] = lcMastPoHd
=lfSetIndex(lcMastPoHd,@laIndex)

*-- To get the structure of the POSLN file
lcSqlStatement  = "SELECT TOP 0 * FROM POSLN [INDEX=POSLN]"
=lfOpenSql(lcSqlStatement,'POSLN',lcPosLN, "","",.F.)
DIMENSION laIndex[2,2]
laIndex = ''
laIndex[1,1] = 'CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD'
laIndex[1,2] = lcPosLN
laIndex[2,1] = 'SHIPNO+CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD'
laIndex[2,2] = 'Poslnsh'
=lfSetIndex(lcPosLN,@laIndex)

*-- To get the structure of the vendor file
lcSqlStatement  = "SELECT * FROM APVENDOR WHERE 1 = 2"
=lfOpenFox(lcSqlStatement,'APVENDOR',lcVendFile,"")
DIMENSION laIndex[1,2]
laIndex = ''
laIndex[1,1] = 'cVendCode'
laIndex[1,2] = lcVendFile
=lfSetIndex(lcVendFile,@laIndex)

*-- To Get the structure of the GLDIST file
IF llLinkToGl
  lcSqlStatement  = "SELECT * FROM GLDIST WHERE 1 = 2"
  =lfOpenFox(lcSqlStatement,'GLDist',lcGlDist,"")
ENDIF

*-- To get the structure of the BOMLINE file
lcSqlStatement  = "SELECT TOP 0 * FROM BOMLINE [INDEX=BOMLINE]"
=lfOpenSql(lcSqlStatement,'BOMLINE',lcTmpBomLn, "","",.F.)
DIMENSION laIndex[2,2]
laIndex = ''
laIndex[1,1] = 'CIMTYP+CTYPE+CTKTNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE'
laIndex[1,2] = 'BomLine'
laIndex[2,1] = 'CIMTYP+CTYPE+SHIPNO+CTKTNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE'
laIndex[2,2] = 'BOMLNSHP'
=lfSetIndex(lcTmpBomLn,@laIndex)

*-- To get the structure of the BOMLINE file
lcSqlStatement  = "SELECT TOP 0 * FROM BOMLINE [INDEX=BOMLINE]"
=lfOpenSql(lcSqlStatement,'BOMLINE',lcTmpBomLn, "","",.F.)
DIMENSION laIndex[2,2]
laIndex = ''
*B608845,1 WAM 04/09/2009 Fix Error while updating error while saving [T20090317.0071]
*B608845,1 WAM 04/09/2009 Add the NLINENO to the index expression to cover the case where the same cost item exist more than once in the style cost sheet
*laIndex[1,1] = 'CIMTYP+CTYPE+CTKTNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE'
laIndex[1,1] = 'CIMTYP+CTYPE+CTKTNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE+STR(NLINENO,6)'
*B608845,1 WAM 04/09/2009 (End)
laIndex[1,2] = 'BomLine'
*B608845,1 WAM 04/09/2009 Fix Error while updating error while saving [T20090317.0071]
*B608845,1 WAM 04/09/2009 Add the NLINENO to the index expression to cover the case where the same cost item exist more than once in the style cost sheet
*laIndex[2,1] = 'CIMTYP+CTYPE+SHIPNO+CTKTNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE'
laIndex[2,1] = 'CIMTYP+CTYPE+SHIPNO+CTKTNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE+STR(NLINENO,6)'
*B608845,1 WAM 04/09/2009 (End)
laIndex[2,2] = 'BOMLNSHP'
=lfSetIndex(lcTmpBomLn,@laIndex)

*-- To get the structure of the MFGOPRDT file
lcSqlStatement = "SELECT TOP 0 * FROM MFGOPRDT [INDEX=MFGOPRDT]"
=lfOpenSql(lcSqlStatement,'MFGOPRDT',lcMastOprDt, "","",.F.)
DIMENSION laIndex[1,2]
laIndex = ''
laIndex[1,1] = 'CIMTYP+CTKTNO+COPRCODE+CLOTNO+TRANCD'
laIndex[1,2] = lcMastOprDt
=lfSetIndex(lcMastOprDt,@laIndex)

*B607935,1 WAM 01/17/2007 Create temp item file
*-- To be used in case of need to update the usage.
lcSqlStatement  = "SELECT TOP 0 * FROM ITEM [INDEX=STYLE]"
=lfOpenSql(lcSqlStatement,'ITEM',lcTmpItm,"","",.F.)
DIMENSION laIndex[1,2]
laIndex = ''
laIndex[1,1] = 'cInvType+Style'
laIndex[1,2] = lcTmpItm
=lfSetIndex(lcTmpItm,@laIndex)
*B607935,1 WAM 01/17/2007 (End)

*-- To be used in case of need to update the usage.
lcSqlStatement  = "SELECT TOP 0 * FROM ITEMLOC [INDEX=STYDYE]"
=lfOpenSql(lcSqlStatement,'ITEMLOC',lcTmpItmDye, "","",.F.)
DIMENSION laIndex[1,2]
laIndex = ''
laIndex[1,1] = 'cInvType+Style+cWareCode+Dyelot'
laIndex[1,2] = lcTmpItmDye
=lfSetIndex(lcTmpItmDye,@laIndex)
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
RELEASE lcSqlStatement, laIndex
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]
*!***************************************************************************
*! Name      : lfUpdItmFl
*! Developer : Khalid Mohi El-Din
*! Date      : 09/11/2004
*! Purpose   : To update the WIP,WO and In-Transit in the style/stydye and ItemLoc
*!             in case of (invtype=0001) and Itemloc
*!             in case of (invtype=0002)
*!******************************************************************************
*! Parameters: lcFile      : File to be replaced
*!             lcField     : WIP or WO
*!             lcOperator  : '+' or '-'
*!             laRecQty    : Received quantity
*!             laOpnQty    : open quantity
*!******************************************************************************
FUNCTION lfUpdItmFl
*E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
*LPARAMETERS lcFile,  lcField, lcOperator, laRecQty, laOpnQty
LPARAMETERS lcFile,  lcField, lcOperator, laRecQty, laOpnQty, llTotalOnly
*E039550,1 WSH 08/07/2005 [End]

PRIVATE lnAlias, lcTotFld
lnAlias  = SELECT(0)
SELECT(lcFile)

*E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
IF !llTotalOnly
  *E039550,1 WSH 08/07/2005 [End]

  REPLACE &lcField.1  WITH &lcField.1 &lcOperator MIN(laRecQty[1] , laOpnQty[1]) ,;
    &lcField.2  WITH &lcField.2 &lcOperator MIN(laRecQty[2] , laOpnQty[2]) ,;
    &lcField.3  WITH &lcField.3 &lcOperator MIN(laRecQty[3] , laOpnQty[3]) ,;
    &lcField.4  WITH &lcField.4 &lcOperator MIN(laRecQty[4] , laOpnQty[4]) ,;
    &lcField.5  WITH &lcField.5 &lcOperator MIN(laRecQty[5] , laOpnQty[5]) ,;
    &lcField.6  WITH &lcField.6 &lcOperator MIN(laRecQty[6] , laOpnQty[6]) ,;
    &lcField.7  WITH &lcField.7 &lcOperator MIN(laRecQty[7] , laOpnQty[7]) ,;
    &lcField.8  WITH &lcField.8 &lcOperator MIN(laRecQty[8] , laOpnQty[8])

  *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
ENDIF
*E039550,1 WSH 08/07/2005 [End]

DO CASE
CASE UPPER(lcField) = "INTRANS"
  lcTotFld = "TotIntrn"
CASE UPPER(lcField) = "WIP"
  lcTotFld = "TotWip"
CASE UPPER(lcField) = "NWO"
  lcTotFld = "nTotWO"
CASE UPPER(lcField) = "STK"
  lcTotFld = "TOTSTK"
CASE UPPER(lcField) = "NONRET"
  lcTotFld = "NTOTONRET"

ENDCASE

*E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
*REPLACE &lcTotFld WITH &lcField.1+&lcField.2+&lcField.3+&lcField.4+&lcField.5+&lcField.6+;
&lcField.7+&lcField.8
LOCAL lnTotal, lnI
lnTotal = 0

FOR lnI = 1 TO 8
  lnTotal = lnTotal + MIN(laRecQty[lnI] , laOpnQty[lnI])
ENDFOR

REPLACE (lcTotFld) WITH EVALUATE(lcTotFld) &lcOperator lnTotal
*E039550,1 WSH 08/07/2005 [End]

SELECT(lnAlias)

*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
RELEASE lnAlias, lcTotFld,lnTotal, lnI
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]

*!***************************************************************************
*! Name      : lfAddNewLn
*! Developer : Khalid Mohi El-Din
*! Date      : 09/11/2004
*! Purpose   : To prepare the lines for updating. If a new line added to the PO
*!             update the Bill of Materials
*!******************************************************************************
FUNCTION lfAddNewLn

*B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[Start]
IF USED('Scale')
  DIMENSION laIndex[1,2]
  laIndex[1,1] = 'TYPE+SCALE'
  laIndex[1,2] = 'Scale'
  *T20071102.0018,10/C200876 TMI 09/11/2008 [Start]
  IF UPPER(oAriaApplication.DataDir)$DBF('SCALE')
    SELECT SCALE
    =gfSetOrder('SCALE')
  ELSE
    *T20071102.0018,10/C200876 TMI 09/11/2008 [Start]

    lfSetIndex('Scale',@laIndex)

    *T20071102.0018,10/C200876 TMI 09/11/2008 [Start]
  ENDIF
  *T20071102.0018,10/C200876 TMI 09/11/2008 [End  ]
ENDIF


lcTmpFileNam    = gfTempName()
lcPoshdrAlias   = gfTempName()
lcPoslnAlias    = gfTempName()
lcBomAlias      = gfTempName()
lcBomLineAlias  = gfTempName()
lcCtktBomAlias  = gfTempName()
lcMFGOprHdAlias = gfTempName()
*! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [Start]
lcTmpPoshdr = gfTempName()
*! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [End]

IF !USED(lcMFGOprHdAlias)
  =gfOpenTable('MFGOprHd','mfgoprhd',"SH",lcMFGOprHdAlias)
ENDIF


IF !USED(lcBomAlias)
  =gfOpenTable('Bom','multibom',"SH",lcBomAlias)
ENDIF

IF !USED(lcBomLineAlias)
  =gfOpenTable('BomLINE','BomLine',"SH",lcBomLineAlias)
ENDIF


IF !USED(lcPoshdrAlias)
  =gfOpenTable('POSHDR','POSHDR',"SH",lcPoshdrAlias)
ENDIF
*! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [Start]
IF !USED(lcTmpPoshdr)
  =gfOpenTable('POSHDR','POSHDR',"SH",lcTmpPoshdr)
ENDIF
*! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [End]
IF !USED(lcPoslnAlias)
  =gfOpenTable('POSLN','POSLN',"SH",lcPoslnAlias)
ENDIF

IF !USED(lcCtktBomAlias)
  =gfOpenTable('CtktBom','CtktBom',"SH",lcCtktBomAlias)
ENDIF
lcCstShtType = IIF(loFormSet.lcPType $ 'ISB','I',IIF(loFormSet.lcPType $ 'MT','M',""))
*B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[End]


*--Update For new added P/o Lines.....
SELECT (lcTmpLine)
SCAN FOR TranCd = '1'
  IF !lNewLn
    LOOP
  ENDIF

  *B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[Start]
  *! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [Start]
  *!*	  =gfsqlrun("SELECT  BOM.* FROM BOM INNER JOIN BOMHEADR ON  BOMHEADR.cinvtype = BOM.cinvtype  AND  BOMHEADR.cCstShtTyp = BOM.cCstShtTyp AND "+;
  *!*	            " BOMHEADR.cItmMajor = BOM.cItmMajor AND BOMHEADR.ldefcstsht = 1 WHERE BOM.cinvtype = '"+lcInvType+"' AND BOM.cItmMajor = '" + ;
  *!*	            SUBSTR(Style,1,loFormSet.lnMjrWid)+ "'" +;
  *!*	            " AND BOM.cCstShtTyp ='" + lcCstShtType + "'",lcBomAlias)
  IF !SEEK(IIF(lcRecvType $'RD',lcPType,'P')+'P'+&lcTmpLine..Po,lcPoshdrAlias)
    =gfSEEK(IIF(lcRecvType $'RD',lcPType,'P')+'P'+&lcTmpLine..Po,lcPoshdrAlias)
  ENDIF
  IF !SEEK(IIF(lcRecvType $'RD',lcPType,'P')+'P'+&lcTmpLine..Po,lcTmpPoshdr)
    =gfSEEK(IIF(lcRecvType $'RD',lcPType,'P')+'P'+&lcTmpLine..Po,lcTmpPoshdr)
  ENDIF



  SELECT (lcTmpLine)
  =gfsqlrun("SELECT  BOM.* FROM BOM INNER JOIN BOMHEADR ON  BOMHEADR.cinvtype = BOM.cinvtype  AND  BOMHEADR.cCstShtTyp = BOM.cCstShtTyp AND "+;
    " BOMHEADR.cItmMajor = BOM.cItmMajor  and  BOMHEADR.ccstsht_id =  BOM.ccstsht_id  WHERE BOM.cinvtype = '"+lcInvType+"' AND BOM.cItmMajor = '" + ;
    SUBSTR(STYLE,1,loFormSet.lnMjrWid)+ "'" +;
    " AND BOM.cCstShtTyp ='" + lcCstShtType + "' AND BOMHEADR.ldefcstsht = 1",lcBomAlias)
  SELECT(lcBomAlias)
  LOCATE REST WHILE CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6) =lcInvType+PADR(SUBSTR(&lcTmpLine..STYLE,1,loFormSet.lnMjrWid),19)+lcCstShtType FOR CCATGTYP = 'P' AND CCURRCODE = &lcPoshdrAlias..CPRICECUR
  IF !FOUND()
    SELECT (lcTmpLine)
    =gfsqlrun("SELECT  BOM.* FROM BOM INNER JOIN BOMHEADR ON  BOMHEADR.cinvtype = BOM.cinvtype  AND  BOMHEADR.cCstShtTyp = BOM.cCstShtTyp AND "+;
      " BOMHEADR.cItmMajor = BOM.cItmMajor and  BOMHEADR.ccstsht_id =  BOM.ccstsht_id  WHERE BOM.cinvtype = '"+lcInvType+"' AND BOM.cItmMajor = '" + ;
      SUBSTR(STYLE,1,loFormSet.lnMjrWid)+ "'" +;
      " AND BOM.cCstShtTyp ='" + lcCstShtType + "' AND BOMHEADR.ccstsht_id IN (Select Top 1 ccstsht_id from BOM WHERE BOM.cinvtype = '"+lcInvType+"' AND BOM.cItmMajor = '" + ;
      SUBSTR(STYLE,1,loFormSet.lnMjrWid)+ "'" +;
      " AND BOM.cCstShtTyp ='" + lcCstShtType + "' AND ccatgtyp ='P' AND CCURRCODE ='"+&lcPoshdrAlias..CPRICECUR +"')",lcBomAlias)
  ENDIF
  *! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [End]
  SELECT (lcTmpLine)
  *B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[End]


  SCATTER MEMVAR

  *B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[Start]
  *=SEEK(IIF(lcRecvType $'RD',lcPType,'P')+&lcTmpLine..Po,'POSHDR')
  m.ccstsht_id = &lcBomAlias..ccstsht_id
  IF !SEEK(IIF(llMFCall,'M','I')+&lcTmpLine..Po,lcMFGOprHdAlias)
    gfSEEK(IIF(llMFCall,'M','I')+&lcTmpLine..Po,lcMFGOprHdAlias)
  ENDIF
  IF !SEEK(IIF(lcRecvType $'RD',lcPType,'P')+'P'+&lcTmpLine..Po,lcPoshdrAlias)
    =gfSEEK(IIF(lcRecvType $'RD',lcPType,'P')+'P'+&lcTmpLine..Po,lcPoshdrAlias)
  ENDIF
  *! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [Start]
  IF !SEEK(IIF(lcRecvType $'RD',lcPType,'P')+'P'+&lcTmpLine..Po,lcTmpPoshdr)
    =gfSEEK(IIF(lcRecvType $'RD',lcPType,'P')+'P'+&lcTmpLine..Po,lcTmpPoshdr)
  ENDIF
  *! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [End]
  *B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[End]

  IF (llImpCost AND lcRecvType <>'R') AND !lfUpdBom('A')
    SELECT (lcTmpLine)
    LOOP
  ENDIF

  *B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[Start]
  *SELECT POSLN
  *APPEND BLANK
  *GATHER MEMVAR
  *REPLACE cOwner WITH ' '
  SELECT (lcTmpLine)
  =gfAdd_Info(lcTmpLine)
  SCATTER MEMVAR
  SELECT(lcPoslnAlias)
  gfAppend('',.T.)
  gfReplace("cOwner WITH ' '")
  *B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[End]


  *B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[Start]
  *!*    SELECT POSHDR
  *!*    =RLOCK()
  *!*    REPLACE LastLine   WITH POSLN.LineNo
  *!*    UNLOCK
  SELECT(lcPoshdrAlias)
  gfREPLACE ("LastLine   WITH "+IIF(LastLine >&lcPoslnAlias..LINENO,STR(LastLine,6), STR(&lcPoslnAlias..LINENO,6))+"")
  *B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[End]

  SELECT (lcTmpLine)
  REPLACE lNewLn WITH .F.
ENDSCAN

*B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[Start]
SELECT(lcPoshdrAlias)
gfTableUpdate()
SELECT(lcPoslnAlias)
gfTableUpdate()
SELECT(lcBomLineAlias)
SCAN
  gfReplace('')
ENDSCAN
gfTableUpdate()
SELECT(lcCtktBomAlias)
SCAN
  gfReplace('')
ENDSCAN
gfTableUpdate()
SELECT(lcMFGOprHdAlias)
SCAN
  gfReplace('')
ENDSCAN
gfTableUpdate()
*B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[End]

*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
RELEASE laIndex,lcTmpFileNam,lcPoshdrAlias   ,lcPoslnAlias    ,lcBomAlias      ,lcBomLineAlias  ,lcCtktBomAlias  ,;
  lcMFGOprHdAlias ,lcCstShtType
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]

RETURN

*!***************************************************************************
*! Name      : lfUpdIntCmp
*! Developer : Khalid Mohi El-Din
*! Date      : 09/11/2004
*! Purpose   : To update Issue Inter Location Purchase Order.
*!******************************************************************************
*! Parameters: lcMastShp : Shipment file in case of issue shipment
*!******************************************************************************
FUNCTION lfUpdIntCmp
LPARAMETERS lcMastShp
PRIVATE lcSqlStatement, lcStyle, lcWareHous, lcDyelot, lcVendor,;
  lcBusDocu, lcStyType, lcPoNo, laReceive, laOpen, laStkQty
LOCAL lcChkPO, lcSysType
DIMENSION laReceive[8], laOpen[8], laStkQty[8]
STORE 0 TO laReceive, laOpen, laStkQty
lcChkPO = SPACE(6)
*-- Send Inter Location PO or Purchase orders to sites
IF !llFromEDI AND 'NC' $ oAriaApplication.CompanyInstalledModules
  =gfOpenFile(oAriaApplication.DataDir+'EDIACPRT',oAriaApplication.DataDir+'ACCFACT','SH')
  =gfOpenFile(oAriaApplication.DataDir+'EDIPD',oAriaApplication.DataDir+'PARTTRANS','SH')
  =gfOpenFile(oAriaApplication.DataDir+'EDITRANS',oAriaApplication.DataDir+'TYPEKEY','SH')
  lcSysType = gfGetMemVar('M_SYSTYPE')
  IF lcSysType = 'P'
    =gfOpenFile(oAriaApplication.DataDir+'CODES',gcDataDir+'Idrltfname','SH')
    SELECT CODES
    =SEEK('N'+'Y'+PADR('CSITEID',10))
    LOCATE REST WHILE cDefCode+cRltField+cFld_Name = 'N'+'Y'+PADR('CSITEID',10) ;
      FOR   cRltd_Nam = 'CCMSITETYP' AND cRltd_Vlu = 'B'
    SELECT EDiAcPrt
    LOCATE FOR cSiteId = Codes.cCode_No
    lcBackSite = cSiteId
    lcBackAcc  = cPartner
  ENDIF
ENDIF

SELECT (lcTmpLine)
*--PO+Style+Dyelot+cWareCode+STR(LineNo,6)+cCarton+TranCd
SET ORDER TO TAG TmpLine3

*!*	*C102358,5 AMH Add trigger to custom issue adornment order for JL [Start]
*!*	IF !llFromEdi AND lcRecvType = 'A' AND ASCAN(laEvntTrig , PADR('UPDISSAD',10)) <> 0
*!*	  =gfDoTriger('PORCVAP',PADR('UPDISSAD',10))
*!*	ENDIF
*!*	*C102358,5 AMH [End]

SELECT (lcTmpLine)
GO TOP
SCAN FOR TranCd <> '1' AND TotQty <> 0
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
  *WAIT WINDOW 'Posting all transactions ... Style :'+Style NOWAIT
  IF !loFormSet.llSilentMod
    WAIT WINDOW 'Posting all transactions ... Style :'+STYLE NOWAIT
  ENDIF
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

  lcBusDocu  = cBusDocu
  lcStyType  = cStyType
  lcPoNo     = PO
  lcStyle    = STYLE
  lcWareHous = cWareCode
  lcDyelot   = Dyelot
  lcVendor   = PADR(Vendor,6)

  *-- Get item information from style file
  *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
  *lcSqlStatement = "SELECT * FROM STYLE WHERE Style = '" + lcStyle + "'"
  *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
  *lcSqlStatement = "SELECT * FROM STYLE WHERE Style = [" + lcStyle + "]"  
  lcStySelVal = lcStyle
  lcSqlStatement = "SELECT * FROM STYLE WHERE Style = ?m.lcStySelVal"
  *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
  *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][END]
  =lfGetItmInf(lcInvType,lcStyle,lcTempItem,'STYLE',lcSqlStatement)
  =SEEK(lcStyle,lcTempItem)

  *-- Get item information from stydye file for the target location
  *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
*!*	  lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
*!*	    lcStyle+lcWareHous+SPACE(10) + "'"
  *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
*!*	  lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
*!*	    lcStyle+lcWareHous+SPACE(10) + "]"
  lcSelValSty = lcStyle+lcWareHous+SPACE(10) 
  lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = ?m.lcSelValSty"
  *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]  
  *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]  
  =lfGetItmInf(lcInvType,lcStyle+lcWareHous+SPACE(10),lcItemLoc,'STYDYE',lcSqlStatement)
  =SEEK(lcStyle+lcWareHous+SPACE(10),lcItemLoc)

  *-- IF not empty dyelot get the dyelot record for the target location.
  IF !EMPTY(lcDyelot)
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
*!*	    lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
*!*	      lcStyle+lcWareHous+lcDyelot + "'"
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
*!*	    lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
*!*	      lcStyle+lcWareHous+lcDyelot + "]"
    m.lcSelStyv= lcStyle+lcWareHous+lcDyelot
    lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = ?m.m.lcSelStyv"
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]  
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][ENd]  
    =lfGetItmInf(lcInvType,lcStyle+lcWareHous+lcDyelot,lcItemLoc,'STYDYE',lcSqlStatement)
    =SEEK(lcStyle+lcWareHous+lcDyelot,lcItemLoc)
  ENDIF

  *-- Get item information from stydye file for the sourc location
  *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
*!*	  lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
*!*	    lcStyle+lcVendor+SPACE(10) + "'"
  *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
*!*	  lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
*!*	    lcStyle+lcVendor+SPACE(10) + "]"
  lcSelStyV = lcStyle+lcVendor+SPACE(10) 
  lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot =?m.lcSelStyV"
  *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]  
  *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]  
  =lfGetItmInf(lcInvType,lcStyle+lcVendor+SPACE(10),lcItemLoc,'STYDYE',lcSqlStatement)
  =SEEK(lcStyle+lcVendor+SPACE(10),lcItemLoc)

  *-- IF not empty dyelot get the dyelot record for the source location.
  IF llDyelot AND !EMPTY(lcDyelot) AND &lcTempItem..cDye_Flg='Y'
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
*!*	    lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
*!*	      lcStyle+lcVendor+lcDyelot + "'"
	*! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
*!*	    lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
*!*	      lcStyle+lcVendor+lcDyelot + "]"
    lcStyValuSel = lcStyle+lcVendor+lcDyelot 
    lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = ?m.lcStyValuSel "
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]  
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]  
    =lfGetItmInf(lcInvType,lcStyle+lcVendor+lcDyelot,lcItemLoc,'STYDYE',lcSqlStatement)
    =SEEK(lcStyle+lcVendor+lcDyelot,lcItemLoc)
  ENDIF

  *-- Get the information of the POSHDR
  IF lcPoNo <> lcChkPO
    lcSqlStatement = "SELECT * FROM POSHDR [INDEX=POSHDR] "+;
      "WHERE cBusDocu = '" + lcBusDocu + "' AND cStyType = '" + lcStyType +;
      "' AND PO = '" + lcPONo + "'"
    =lfGetItmInf(lcInvType,lcBusDocu+lcStyType+lcPONo,lcMastPoHd,'POSHDR',lcSqlStatement,.T.)
    =SEEK(lcBusDocu+lcStyType+lcPONo,lcMastPoHd)
    *-- Issue inter-location PO shipment
    IF lcRecvType  = 'U'
      *-- Get the master records from POSLN file
      lcSqlStatement  = "SELECT * FROM POSLN [INDEX=POSLN] "+;
        "WHERE cBusDocu = '" + &lcTmpLine..cBusDocu + ;
        "' AND cStyType ='" + &lcTmpLine..cStyType  + ;
        "' AND PO ='" + &lcTmpLine..PO + "' AND cInvType ='" + lcInvType + "'"
      =lfOpenSql(lcSqlStatement,'POSLN',lcMastPoLn)
      DIMENSION laIndex[2,2]
      laIndex = ''
      laIndex[1,1] = 'CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD'
      laIndex[1,2] = lcMastPoLn
      laIndex[2,1] = 'SHIPNO+CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD'
      laIndex[2,2] = 'Poslnsh'
      =lfSetIndex(lcMastPoLn,@laIndex)
      SET ORDER TO TAG lcMastPoLn IN (lcMastPoLn)
    ENDIF
  ENDIF

  SELECT (lcTmpLine)
  *-- Get the average cost from the source location.
  =SEEK(lcStyle,lcTempItem)
  =SEEK(lcStyle+lcVendor+SPACE(10),lcItemLoc)
  lnNewCost  = IIF(IIF(lcInvType="0001",lcCostMth = 'A',lcCostMthM = 'A'),;
    &lcItemLoc..Ave_Cost , &lcTempItem..TotCost )

  *--Update Style Inventory
  *--G/L Array difinition and initialization.
  *-- Update general ledger entreis in gfStyCrl()
  IF llLinkToGl
    *--Read the G/L link code that will be used to create GL entres.
    DECLARE laGLDistAr[2,13]
    laGLDistAr[1,1] = IIF(SEEK(lcStyle+lcVendor+SPACE(10),lcItemLoc) AND ;
      !EMPTY(&lcItemLoc..Gl_link),&lcItemLoc..Gl_link,'DEFDEF')

    laGLDistAr[2,1] = IIF(SEEK(lcBusDocu+lcStyType+lcPONo,lcMastPoHd),&lcMastPoHd..Link_Code,'DEFDEF')
    laGLDistAr[1,2] = '006'
    laGLDistAr[2,2] = '013'
    laGLDistAr[1,3] =  1
    laGLDistAr[2,3] = -1
    *! C201407,1 SAB 03/20/2012 Fix problem of Tran_type in GLDIST should be PO in receiveing case [Start]
    **N039541,1 KHM 12/12/2005 [Start]
    **STORE 'PO'       TO laGLDistAr[1,4],laGLDistAr[2,4]
    *STORE 'IA'       TO laGLDistAr[1,4],laGLDistAr[2,4]
    **N039541,1 KHM 12/12/2005 [End]
    STORE 'PO'       TO laGLDistAr[1,4],laGLDistAr[2,4]
    *! C201407,1 SAB 03/20/2012 Fix problem of Tran_type in GLDIST should be PO in receiveing case [End]
    STORE lcPONo     TO laGLDistAr[1,5],laGLDistAr[2,5]
    STORE ldTrDate   TO laGLDistAr[1,6],laGLDistAr[2,6]
    STORE lcGLFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
    STORE lcGlPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
    STORE lcGlDist   TO laGLDistAr[1,9],laGLDistAr[2,9]
  ELSE
    DIME laGLDistAr[1,1]
    laGLDistAr = ''
  ENDIF
  PRIVATE laAdjust
  DECLARE laAdjust[9]
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    laAdjust[lnI] = -(Qty&lcI)
  ENDFOR
  laAdjust[9] = -(TotQty)

  *--Call the global function for update style inventory control.
  PRIVATE lcRefer
  lcRefer  = "Source :" + lcVendor + " Target: "+ lcWareHous
  lnNxtStp = gfStyCrl('6',lcStyle,lcVendor,lcDyelot,ldRecDate,;
    lcPONo,@laAdjust,lnNewCost,lcRefer,lcGlSession,'',;
    1,lcTmpLine,'nSteps',@laGLDistAr,,,,,&lcMastPoHd..cPONO)


  *--Create Intransit P/O line record
  SELECT (lcTmpLine)
  SCATTER MEMVAR

  IF lcRecvType  = 'A'
    STORE 0 TO m.nCost2,m.nLan_Cst2,m.nECost2,m.nELanCost2
  ENDIF

  =SEEK(lcStyle+lcVendor+SPACE(10),lcItemLoc)
  m.nFCost1  = IIF(IIF(lcInvType="0001",lcCostMth='A',lcCostMthM ='A'),;
    &lcItemLoc..Ave_Cost , &lcTempItem..TotCost)
  m.nICost1 = m.nFCost1

  SELECT (lcPosLn)
  APPEND BLANK
  GATHER MEMVAR
  REPLACE DATE      WITH ldRecDate,;
    TranCd    WITH '6',;
    cOwner    WITH ' ',;
    cRSession WITH lcGlSession
  SELECT (lcTmpLine)

  *! E303006,1 SAB 12/03/2011 Enable Issue From BINs in Inter Location PO [Start]
  *- Call Bin Location Trigger
  IF TYPE('loFormSet')='O' AND ASCAN(loFormSet.laEvntTrig,PADR('DLSBNPOR',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
    SELECT (lcTmpLine)
    =gfDoTriger("POSTREC",PADR("DLSBNPOR",10))
  ENDIF
  *! E303006,1 SAB 12/03/2011 Enable Issue From BINs in Inter Location PO [End]

  *--Update in-transit
  IF (lcRecvType $ 'NHU' OR;
      (!llFromEdi AND lcRecvType = 'A' AND ASCAN(laEvntTrig,PADR("RCVADORD",10)) <> 0))
    FOR lnCnt = 1 TO 8
      lcCnt = STR(lnCnt,1)
      laReceive[lnCnt] = EVALUATE(lcTmpLine+'.Qty'+lcCnt)
      laOpen[lnCnt]    = EVALUATE(lcTmpLine+'.Qty'+lcCnt)
    ENDFOR

    *-- Update In-Transit in the style file
    IF SEEK(lcStyle,lcTempItem)
      =lfUpdItmFl(lcTempItem,'InTrans','+',@laReceive,@laOpen)
      *-- Update WIP in the style file
      IF lcRecvType $ 'NHU'
        =lfUpdItmFl(lcTempItem,'WIP','+',@laReceive,@laOpen)
        *-- Update NWO in the style file
        *B124297,1 WAM 03/05/2005 Do not update Work Order
        *=lfUpdItmFl(lcTempItem,'NWO','+',@laReceive,@laOpen)
        *B124297,1 WAM 03/05/2005 (End)
      ENDIF
    ENDIF

    *-- Update In-Transit in the stydye file
    IF SEEK(lcStyle+IIF(lcRecvType='U',lcVendor,lcWareHous)+SPACE(10),lcItemLoc)
      =lfUpdItmFl(lcItemLoc,'InTrans','+',@laReceive,@laOpen)
    ENDIF
    *-- Update the record on the dyelot level
    IF llDyelot AND !EMPTY(lcDyelot) AND &lcTempItem..cDye_Flg='Y' AND;
        SEEK(lcStyle+IIF(lcRecvType='U',lcVendor,lcWareHous)+lcDyelot,lcItemLoc)
      =lfUpdItmFl(lcItemLoc,'InTrans','+',@laReceive,@laOpen)
    ENDIF

    *-- Update WIP & WO in the stydye file
    IF lcRecvType $ 'NHU'
      IF SEEK(lcStyle+lcVendor+SPACE(10),lcItemLoc)
        =lfUpdItmFl(lcItemLoc,'WIP','+',@laReceive,@laOpen)
        *B124297,1 WAM 03/05/2005 Do not update Work Order
        *=lfUpdItmFl(lcItemLoc,'NWO','+',@laReceive,@laOpen)
        *B124297,1 WAM 03/05/2005 (End)
      ENDIF
      *-- Update the record on the dyelot level
      IF llDyelot AND !EMPTY(lcDyelot) AND &lcTempItem..cDye_Flg='Y' AND ;
          SEEK(lcStyle+lcVendor+lcDyelot,lcItemLoc)
        =lfUpdItmFl(lcItemLoc,'WIP','+',@laReceive,@laOpen)
        *B124297,1 WAM 03/05/2005 Do not update Work Order
        *=lfUpdItmFl(lcItemLoc,'NWO','+',@laReceive,@laOpen)
        *B124297,1 WAM 03/05/2005 (End)
      ENDIF
    ENDIF
  ENDIF

  *-- IF issue inter-location PO shipment
  IF lcRecvType = 'U'
    SELECT (lcTmpLine)
    lcShpCode = ShipNo
    lcShpLine = ShipNo+cBusDocu+cStyType+PO+cInvType+STYLE+STR(LINENO,6)+'3'
    SELECT (lcMastPoLn)
    SET ORDER TO TAG Poslnsh
    =SEEK(lcShpLine)
    SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laSHPQty
    SCATTER MEMVAR MEMO
    SELECT(lcPosLn)
    APPEND BLANK
    TABLEUPDATE(0,.T.)
    GATHER MEMVAR MEMO
    REPLACE Qty1 WITH MAX(laSHPQty[1]-&lcTmpLine..Qty1,0),;
      Qty2 WITH MAX(laSHPQty[2]-&lcTmpLine..Qty2,0),;
      Qty3 WITH MAX(laSHPQty[3]-&lcTmpLine..Qty3,0),;
      Qty4 WITH MAX(laSHPQty[4]-&lcTmpLine..Qty4,0),;
      Qty5 WITH MAX(laSHPQty[5]-&lcTmpLine..Qty5,0),;
      Qty6 WITH MAX(laSHPQty[6]-&lcTmpLine..Qty6,0),;
      Qty7 WITH MAX(laSHPQty[7]-&lcTmpLine..Qty7,0),;
      Qty8 WITH MAX(laSHPQty[8]-&lcTmpLine..Qty8,0),;
      TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
    IF TotQty = 0
      DELETE
    ENDIF
  ENDIF

  *-- To be done later
  *-- Send Inter Location PO or Purchase orders to sites
  IF !llFromEDI AND 'NC' $ oAriaApplication.CompanyInstalledModules AND lcRecvType = 'N'
    LOCAL lcWareCode
    lcWareCode = &lcTmpLine..cwarecode
    =gfOpenFile(oAriaApplication.DataDir+'WAREHOUS',oAriaApplication.DataDir+'WAREHOUS','SH')
    =SEEK(lcWareCode,'WAREHOUS')
    lcSiteId = WAREHOUS.cSiteId
    SELECT EDiAcPrt
    LOCATE FOR cSiteId = lcSiteId
    IF FOUND() AND EdiAcPrt.lInterComp AND SEEK(EdiAcPrt.cPartCode+'856','EdiPd')
      SELECT EdiTrans
      IF !SEEK('856'+PADR(&lcTmpLine..Po,40)+EdiAcPrt.TYPE+EdiAcPrt.cPartner)
        INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER,lInterComp) ;
          VALUES ('856',&lcTmpLine..Po,EdiAcPrt.TYPE,EdiAcPrt.cPartner,;
          EdiAcPrt.lInterComp)
      ENDIF
      REPLACE cStatus WITH 'N'

      IF lcSysType = 'P' AND lcSiteId <> lcBackSite AND !EMPTY(lcBackAcc)
        IF !SEEK('856'+PADR(&lcTmpLine..Po,40)+'A'+lcBackAcc)
          INSERT INTO ('EDITRANS') (CEDITRNTYP,KEY,TYPE,CPARTNER,lInterComp) VALUES ;
            ('856',&lcTmpLine..Po,'A',lcBackAcc,.T.)
        ENDIF
        REPLACE cStatus WITH 'N'
      ENDIF
    ENDIF
  ENDIF

ENDSCAN

*--------------------------- Begin updating Master files ---------------------------------
*-- Updating Fox tables Remotely
LOCAL lnConnectionHandlar, lcTranCode
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.cAriaNativeDataFilesConStr,3,'',.T.)
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
  RETURN .F.
ENDIF

*-- 1) Updating style file (FOX) in case of receiving by styles

*!B999999,1 WSH 03/01/2005, Select Table that will be updated... [Start]
SELECT (lcTempItem)
*!B999999,1 WSH 03/01/2005, [End]

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcTempItem,lcTranCode,;
  SET("DataSession"),'STYLE','STYLE')
IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
  RETURN .F.
ELSE
  =TABLEUPDATE(.T.,.T.)
ENDIF

*-- 2) Updating stydye file (FOX)
SELECT (lcItemLoc)
lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcItemLoc,lcTranCode,;
  SET("DataSession"),'Style+cWareCode+dyelot','STYDYE')
IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
  RETURN .F.
ELSE
  =TABLEUPDATE(.T.,.T.)
ENDIF

*-- 3) Updating the GLDIST file (FOX)
IF llLinkToGl
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
  *WAIT WINDOW 'Updating General Ledger Distribution File ' NOWAIT
  IF !loFormSet.llSilentMod
    WAIT WINDOW 'Updating General Ledger Distribution File ' NOWAIT
  ENDIF
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

  SELECT (lcGlDist)
  REPLACE ALL GlSession WITH lcGlSession ,;
    Tran_Desc WITH 'ISSUE INTER-LOC. P/O'

  *!B999999,1 WSH 03/01/2005, Fix bug of Alias not found... [Start]
  *ENDIF
  *!B999999,1 WSH 03/01/2005, [End]

  SELECT (lcGlDist)
  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcGlDist,lcTranCode,;
    SET("DataSession"),'Tran_No+Tran_Type+GlSession+Catg_Key','GLDIST')

  IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
    RETURN .F.
  ELSE
    =TABLEUPDATE(.T.,.T.)
  ENDIF

  *!B999999,1 WSH 03/01/2005, Fix bug of Alias not found... [Start]
ENDIF
*!B999999,1 WSH 03/01/2005, [End]

*-- Commit updating of FOX tables
lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode,.T.)
IF lnConnectionHandlar # 1
  =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF

*-- Begin transaction - Updating SQL tables
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
  RETURN .F.
ENDIF

*-- 1) Update master POSLN (SQL)
SELECT(lcPosLn)

*B607585,1 AMH Add cwarecode to POSLN index and nLineNo to BOMLINE file [Start]
*lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcPosLn,lcTranCode,;
SET("DataSession"),;
'cBusDocu,cStyType,PO,cRSession,ShipNo,cInvType,Style,LineNo,TranCd,cStyGrade',;
'POSLN','POSREC')
lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcPosLn,lcTranCode,;
  SET("DataSession"),;
  'cBusDocu,cStyType,PO,cRSession,ShipNo,cInvType,Style,LineNo,TranCd,cStyGrade,cWareCode',;
  'POSLN','POSREC')
*B607585,1 AMH [End]

IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
  lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
  RETURN .F.
ELSE
  =TABLEUPDATE(.T.,.T.)
ENDIF

*-- 2) Update master Shipment file (SQL)
*-- IF issue inter-location PO shipment
IF lcRecvType = 'U' AND TYPE("lcMastShp") = "C" AND USED(lcMastShp)
  *--Delete the shipment lines for coplete receive lines.
  SELECT (lcMastShp)
  LOCATE
  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcMastShp,lcTranCode,;
    SET("DataSession"),;
    'CBUSDOCU,CSHPTYPE,SHIPNO',;
    'SHPMTHDR','SHPMTHDR')
  IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
    lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
    RETURN .F.
  ELSE
    =TABLEUPDATE(.T.,.T.)
  ENDIF
ENDIF
*B000109,1 WAM 03/05/2005 Remove Lock PO header
IF .F.
  SELECT (lcTmpLine)
  SET ORDER TO TAG TmpLine3
  GO TOP
  DO WHILE !EOF()
    lcPo = Po
    lcSelString = "UPDATE POSHDR SET lLok_stat =0,cLok_User= '', dLok_Date='',cLok_Time='' WHERE cBusDocu+cStyType+PO='"+cBusDocu+cStyType+Po+"'"
    lnResult = oAriaApplication.RemoteCompanyData.Execute(lcSelString,'',"SAVEFILE","",lcTranCode ,4,'',SET("DataSession"))
    IF lnResult # 1 .AND. lnResult # 2
      =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLUPDATE",lnResult,.T.)
      =oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
      RETURN .F.
    ENDIF
    SELECT (lcTmpLine)
    SCAN REST WHILE PO = lcPo
    ENDSCAN
  ENDDO
ENDIF
*B000109,1 WAM 03/05/2005 (End)

*-- Commit updating of SQL tables
lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
IF lnConnectionHandlar # 1
  =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF

WAIT CLEAR

*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
RELEASE lcSqlStatement, lcStyle, lcWareHous, lcDyelot, lcVendor,lcBusDocu, lcStyType, lcPoNo, laReceive, laOpen, laStkQty
RELEASE lcChkPO, lcSysType,laReceive, laOpen, laStkQty
RELEASE lcBackSite,lcBackAcc ,lnNewCost,laGLDistAr,laAdjust,lnI ,lcI ,lcRefer  ,lnNxtStp
RELEASE lnCnt ,lcCnt ,lcShpCode ,lcShpLine ,laSHPQty,lcWareCode,lcSiteId
RELEASE lnConnectionHandlar, lcTranCode
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]


RETURN
*--------------------------- End updating Master files ---------------------------------

*!***************************************************************************
*! Name      : lfGetItmInf
*! Developer : Khalid Mohi El-Din
*! Date      : 09/11/2004
*! Purpose   : To get the item information from style,stydye or itemloc
*!******************************************************************************
*! Parameters: lcInvType   : "0001" for Style, "0002" for Material
*!             lcKey       : To check if the record exist or not
*!             lcFilToUpd  : File to be updated
*!             lcMastFile  : Master file to get the information from
*!             lcSqlState  : SQL statement
*!             llSql       : To indicate whether to open Fox or SQL tables
*!******************************************************************************
FUNCTION lfGetItmInf
LPARAMETERS lcInvType, lcKey, lcFilToUpd, lcMastFile, lcSqlState, llSql
PRIVATE lnAlias

lnAlias  = SELECT(0)

*-- this condition has been done in order not to change the whole code and it might be
*-- removed later on
IF TYPE('lcTmpCur') <> 'C'
  lcTmpCur = gfTempName()
ENDIF

IF !SEEK(lcKey, lcFilToUpd)
  IF llSql
    =lfOpenSql(lcSqlState,lcMastFile,lcTmpCur)
  ELSE
    =lfOpenFox(lcSqlState,lcMastFile,lcTmpCur)
  ENDIF

  SELECT(lcTmpCur)
  LOCATE
  SCATTER NAME loData
  INSERT INTO(lcFilToUpd) FROM NAME loData
  RELEASE loData
  SELECT(lcFilToUpd)
  =TABLEUPDATE(0,.T.)
ENDIF

*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
RELEASE lnAlias,loData
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]
*!***************************************************************************
*! Name      : lfGetLanded
*! Developer : Khalid Mohi El-Din
*! Date      : 09/11/2004
*! Purpose   : Calculate landed costs case of detail costing.
*!******************************************************************************
FUNCTION lfGetLanded

SELECT (lcTmpBomLn)
IF lcRecvType $ 'SFC'
  SET ORDER TO TAG BomLnShp
ELSE
  SET ORDER TO TAG BomLine
ENDIF
PRIVATE laECost,lnCurrUnt1,lnCurrUnt2
DIMENSION laECost[1]
laECost = 0
STORE 1 TO lnCurrUnt1,lnCurrUnt2

SELECT (lcTmpLine)
SCATTER FIELDS Qty1,qty2,qty3,qty4,qty5,qty6,qty7,qty8 TO laLnQty

FOR lnCnt = 1 TO 7
  lcCnt=STR(lnCnt,1)
  IF llMFCall
    lcBomLKey = 'M2'+&lcTmpLine..PO
    lcWCondtn = "cImTyp+cType+cTktNo = lcBomLKey"
    *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
    lcFCondtn = "cBomTyp=lcCnt AND Style=&lcTmpLine..Style AND (EMPTY(cRSession) OR cRSession=lcGlSession)"
    *N038893,1 WAM 06/02/2005 (End)
    lcFCondtn = lcFCondtn + " .AND. cStyGrade = &lcTmpLine..cStyGrade "
  ELSE
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
    *!*	    lcBomLKey = IIF(lcRecvType='D','D',IIF(lcRecvType$"NOC","N",'I'))+'2'+;
    *!*	      IIF(lcRecvType$'SFC',&lcTmpLine..Shipno,'')+&lcTmpLine..Po+;
    *!*	      STR(&lcTmpLine..LINENO,6)+lcCnt+lcInvType+&lcTmpLine..STYLE
    lcBomLKey = IIF(lcRecvType='D','D',IIF(lcRecvType$"NOC","N",IIF(&lcTmpLine..cStyType="F",'T','I')))+'2'+;
      IIF(lcRecvType$'SFC',&lcTmpLine..Shipno,'')+&lcTmpLine..Po+;
      STR(&lcTmpLine..LINENO,6)+lcCnt+lcInvType+&lcTmpLine..STYLE
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
    lcWCondtn = "cImTyp+cType+IIF(lcRecvType$'SFC',ShipNo,'')+cTktNo+STR(LineNo,6)+cBomTyp+cInvType+Style = lcBomLKey"
    lcFCondtn = "(EMPTY(cRSession) OR cRSession=lcGlSession) AND cStyGrade = &lcTmpLine..cStyGrade"
  ENDIF
  *B608762,1 WAM 12/15/2008 Consider receiving cut ticket line into more than one warehouse
  lcFCondtn = lcFCondtn + " .AND. cWareCode = &lcTmpLine..cWareCode"
  *B608762,1 WAM (End)

  lnNLCs&lcCnt = 0
  lnCurSQt = 0

  SELECT (lcTmpBomLn)
  IF SEEK(lcBomLKey)
    REPLACE REST StyQty  WITH lfBomSzQt(),;
      ItemQty WITH (StyQty*UnitQty),;
      ItemAmt WITH (ItemQty*UnitCost);
      WHILE &lcWCondtn FOR &lcFCondtn
    =SEEK(lcBomLKey)
    SUM REST (UnitCost*UnitQty)*StyQty WHILE &lcWCondtn FOR &lcFCondtn TO lnNLCs&lcCnt
    lnNLCs&lcCnt = IIF(&lcTmpLine..TotQty<>0,(lnNLCs&lcCnt/&lcTmpLine..TotQty),0)
    lnNLCs&lcCnt = IIF(TYPE("lnNLCs"+lcCnt) <> "N",0,lnNLCs&lcCnt)
  ENDIF

  SELECT (lcTmpLine)
  REPLACE nFLanCost&lcCnt WITH lnNLCs&lcCnt
  IF !llMFCall
    *! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [Start]
    *IF llMulCurr
    IF loFormSet.llMulCurr
      *! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [END]
      *N000587,1 WAM 12/01/2007 Get equivalent cost from BOM table for each cost element based on saved currency code, exchange rate and unit.
      *! B609634,1 MMT 06/28/2011 Fix bug of Zero Cost in Styinvjl in Case of receiving Interlocation PO[Start]
      *IF !(cBusDocu+cStyType $ 'PP|PD|NN' )
      *! B610107,1 MMT 10/04/2012 Add Foreign currency costing to Material MFG order summary[Start]
      *IF !(cBusDocu+cStyType $ 'PP|PD')
      IF !(cBusDocu+cStyType $ 'PP|PD|PF')
        *! B610107,1 MMT 10/04/2012 Add Foreign currency costing to Material MFG order summary[End]
        *! B609634,1 MMT 06/28/2011 Fix bug of Zero Cost in Styinvjl in Case of receiving Interlocation PO[END]
        *N000587,1 WAM 12/01/2007 (End)


        =lfGetEqv(lcCnt,nLanPrRat,nLanDuRat,lnCurrUnt1,lnCurrUnt2,nFLanCost1,nFLanCost2,;
          nFLanCost3,nFLanCost4,nFLanCost5,nFLanCost6,nFLanCost7)

        laECost[1] = IIF(TYPE("laECost[1]") <> "N", 0, laECost[1])
        laECost[1] = IIF(OCCURS('*',STR(laECost[1])) > 0,0,laECost[1])
        REPLACE nLan_Cost&lcCnt WITH laECost[1]
        *N000587,1 WAM 12/01/2007 Get equivalent cost from BOM table for each cost element based on saved currency code, exchange rate and unit.
      ENDIF
      *N000587,1 WAM 12/01/2007 (End)

    ELSE
      REPLACE nLan_Cost&lcCnt WITH lnNLCs&lcCnt
    ENDIF
  ENDIF
ENDFOR
SELECT (lcTmpLine)
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
RELEASE laECost,lnCurrUnt1,lnCurrUnt2,laLnQty,lnCnt ,lcCnt,lcBomLKey ,lcWCondtn ,lcFCondtn ,lnCurSQt
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]
RETURN

*!***************************************************************************
*! Name      : lfBomSzQt
*! Developer : Khalid Mohi El-Din
*! Date      : 09/11/2004
*! Purpose   : Calculate the style quantity per size.
*!******************************************************************************
FUNCTION lfBomSzQt
lnCurSQt = 0
FOR lnI=1 TO 8
  IF STR(lnI,1) $ EVALUATE(lcTmpBomLn+'.CSIZES')
    lnCurSQt = lnCurSQt + laLnQty[lnI]
  ENDIF
ENDFOR
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
RELEASE lnI
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]

RETURN lnCurSQt


*:*******************************************************************************
*! Name : GpCrtBom
*! Auth : Timour Abdalla Khalil.
*:*******************************************************************************
*! Synopsis : Create new records in BOMLINE file with type '2'
*!            if it is not found.
*:*******************************************************************************
*: Parameter: lcTrType   : --> 'M' C/T
*:  					 	   'I' P/O
*:  					       'S' Shipment
*:  					  	   'T' Material Mfg.
*:            lcTket     : --> Work Order #
*:            lcItem     : --> Item (Style/Fabric)
*:            lcShipNo   : --> Shipment #
*:            lcSess     : --> Receiving session
*:            lcStyQlt   : --> Item quality
*:            lcMastFile : --> Master records from bomline
*:            lcTmpFile  : --> Temporary file to hold records with type '2'
*:            llShpRec   : --> .T. there are records for shipment with type = '2'
*:*******************************************************************************
PROCEDURE GpCrtBom
PARAMETERS lcTrType,lcTket,lcItem,lcShipNo,lcSess,lcStyQlt, lcMastFile,;
  lcTmpFile, llShpRec



SELECT (lcMastFile)
IF lcTrType = 'F'
  lcItem = PADR(lcItem,19)
ENDIF

*B132695,1 KHM 07/03/2006 Add the line # [Begin]
*lcSeekKey = lcTrType+'2'+IIF(llShpRec,lcShipNo,SPACE(6))+lcTket
lcSeekKey = lcTrType+'2'+IIF(llShpRec,lcShipNo,SPACE(6))+lcTket+STR(EVALUATE(lcTmpLine+'.LineNo'),6)
*B132695,1 KHM 07/03/2006 [End]

lcWhleCnd = "cImTyp+cType+ShipNo+cTktNo+STR(LineNo,6) = lcSeekKey"
lcForCond = "Style = lcItem AND ( EMPTY(cRSession) OR cRSession=lcSess )"
lcQltFltr = IIF(TYPE('lcStyQlt') $ 'UL' ,".T.","cStyGrade = lcStyQlt")
SEEK lcSeekKey

LOCATE REST WHILE &lcWhleCnd FOR &lcForCond AND &lcQltFltr
IF !FOUND()
  *B132695,1 KHM 07/03/2006 [Begin]
  *SEEK lcTrType+'1'+IIF(llShpRec,lcShipNo,SPACE(6))+lcTket
  *SCAN REST WHILE cImTyp+cType+ShipNo+cTktNo+STR(LineNo,6) = ;
  lcTrType+'1'+IIF(llShpRec,lcShipNo,SPACE(6))+lcTket;
  FOR Style = lcItem AND !lVoid
  SEEK lcTrType+'1'+IIF(llShpRec,lcShipNo,SPACE(6))+lcTket+STR(EVALUATE(lcTmpLine+'.LineNo'),6)
  SCAN REST WHILE cImTyp+cType+ShipNo+cTktNo+STR(LINENO,6) = ;
      lcTrType+'1'+IIF(llShpRec,lcShipNo,SPACE(6))+lcTket+STR(EVALUATE(lcTmpLine+'.LineNo'),6);
      FOR STYLE = lcItem AND !lVoid
    SCATTER MEMVAR MEMO
    *B132695,1 KHM 07/03/2006 [End]

    *B608762,1 WAM 12/15/2008 Increament NLINENO whenthe same key repeated
    SELECT (lcTmpFile)
    lnNewLineNo = 0
    *B608845,1 WAM 04/09/2009 Fix Error while updating error while saving [T20090317.0071]
    *B608845,1 WAM 04/09/2009 Cover the case where the same cost item exist more than once in the style cost sheet
    *!*	    SCAN FOR ;
    *!*	      CIMTYP+CTYPE+CTKTNO+SHIPNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE+CRSESSION+CSTYGRADE+STR(NLINENO,4)=;
    *!*	      lcTrType+'2'+lcTket+IIF(llShpRec,lcShipNo,SPACE(6))+STR(&lcTmpLine..LineNo,6)+;
    *!*	      &lcMastFile..CBOMTYP+&lcMastFile..CINVTYPE+PADR(lcItem ,19)+;
    *!*	      &lcMastFile..cInvTypC+&lcMastFile..Item+&lcMastFile..MFGCODE+lcSess+&lcMastFile..CSTYGRADE
    *B609517,1 TMI 02/14/2011 [Start] remove the check for the cRSession be empty
    *SCAN FOR ;
    *  CIMTYP+CTYPE+CTKTNO+SHIPNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE+CRSESSION+CSTYGRADE+STR(NLINENO,4)=;
    *  lcTrType+'2'+lcTket+IIF(llShpRec,lcShipNo,SPACE(6))+STR(&lcTmpLine..LineNo,6)+;
    *  &lcMastFile..CBOMTYP+&lcMastFile..CINVTYPE+PADR(lcItem ,19)+;
    *  &lcMastFile..cInvTypC+&lcMastFile..Item+&lcMastFile..MFGCODE+SPACE(6)+&lcMastFile..CSTYGRADE
    SCAN FOR ;
        CIMTYP+CTYPE+CTKTNO+SHIPNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE+CRSESSION+CSTYGRADE+STR(NLINENO,4)=;
        lcTrType+'2'+lcTket+IIF(llShpRec,lcShipNo,SPACE(6))+STR(&lcTmpLine..LINENO,6)+;
        &lcMastFile..CBOMTYP+&lcMastFile..CINVTYPE+PADR(lcItem ,19)+;
        &lcMastFile..cInvTypC+&lcMastFile..ITEM+&lcMastFile..MFGCODE ;
        AND CSTYGRADE = &lcMastFile..CSTYGRADE
      *B609517,1 TMI 02/14/2011 [End  ]
      *B608845,1 WAM 04/09/2009 (End)

      lnNewLineNo = MAX(IIF(ISNULL(nLineNo),0,nLineNo),lnNewLineNo)
    ENDSCAN
    lnNewLineNo = lnNewLineNo + 1
    *B608762,1 WAM 12/15/2008 (End)

    SELECT(lcTmpFile)
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE cType      WITH '2',;
      ShipNo    WITH lcShipNo,;
      cStyGrade WITH IIF(lcQltFltr<>".T.",lcStyQlt,''),;
      StyQty    WITH  0,;
      ItemQty   WITH  0,;
      ItemAmt   WITH  0

    * B608834,1 HES 04/01/2009 Add Standared Fields for POMLINE File[T20081001.0001]
    =gfAdd_Info(lcTmpFile)
    * B608834,1 HES 04/01/2009 Add Standared Fields for POMLINE File[T20081001.0001]

    *B608762,1 WAM 12/15/2008 Update NLINENO and CWARECODE fields
    REPLACE nLineNo   WITH lnNewLineNo ,;
      CWARECODE WITH &lcTmpLine..cWarecode
    *B608762,1 WAM 12/15/2008 (End)

    *! B608617,1 MMT 07/14/2008 Add trigger to update bomline with new ex rate [Start]
    IF CCATGTYP = 'P' AND ASCAN(loFormSet.laEvntTrig ,PADR('UPDEXRATE',10))<>0
      loFormSet.mDoTrigger(PADR('UPDEXRATE' ,10))
    ENDIF
    *! B608617,1 MMT 07/14/2008 Add trigger to update bomline with new ex rate [End]

  ENDSCAN
ELSE
  SCAN REST WHILE &lcWhleCnd FOR &lcForCond AND &lcQltFltr
    SCATTER MEMVAR MEMO
    SELECT(lcTmpFile)

    *B609517,1 TMI 02/05/2011 [Start] make the NlineNo field updated as in IF !FOUND() case
    *B609517,1                        the SCAN FOR should not check crsession = space(6) because it will has value
    lnNewLineNo = 0
    SCAN FOR ;
        CIMTYP+CTYPE+CTKTNO+SHIPNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE+CRSESSION+CSTYGRADE+STR(NLINENO,4)=;
        lcTrType+'2'+lcTket+IIF(llShpRec,lcShipNo,SPACE(6))+STR(&lcTmpLine..LINENO,6)+;
        &lcMastFile..CBOMTYP+&lcMastFile..CINVTYPE+PADR(lcItem ,19)+;
        &lcMastFile..cInvTypC+&lcMastFile..ITEM+&lcMastFile..MFGCODE ;
        AND CSTYGRADE = &lcMastFile..CSTYGRADE

      lnNewLineNo = MAX(IIF(ISNULL(nLineNo),0,nLineNo),lnNewLineNo)
    ENDSCAN
    lnNewLineNo = lnNewLineNo + 1
    *B609517,1 TMI 02/05/2011 [End  ]
    APPEND BLANK
    GATHER MEMVAR MEMO
    TABLEUPDATE(0,.T.)
    REPLACE cType      WITH '2',;
      ShipNo    WITH lcShipNo,;
      cStyGrade WITH IIF(lcQltFltr<>".T.",lcStyQlt,''),;
      StyQty    WITH  0,;
      ItemQty   WITH  0,;
      ItemAmt   WITH  0

    *B609517,1 TMI 02/05/2011 [Start] Update the nLineno field
    REPLACE nLineNo   WITH lnNewLineNo
    *B609517,1 TMI 02/05/2011 [End  ]

    *B609068,1 WAM 10/29/2009 Update CWARECODE in BOMLINE when there are adjust records for receiving
    REPLACE CWARECODE WITH &lcTmpLine..cWarecode
    *B609068,1 WAM 10/29/2009 (End)

    * B608834,1 HES 04/01/2009 Add Standared Fields for POMLINE File[T20081001.0001]
    =gfAdd_Info(lcTmpFile)
    * B608834,1 HES 04/01/2009 Add Standared Fields for POMLINE File[T20081001.0001]

  ENDSCAN
ENDIF

*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
RELEASE lcSeekKey ,lcWhleCnd ,lcForCond,lcQltFltr ,lnNewLineNo
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]

RETURN



*!*************************************************************
*! Name      : lfUpdLot
*! Developer : Khalid Mohi El-Din
*! Date      : 09/11/2004
*! Purpose   : Function to update reciving quantity.
*!*************************************************************
*! Parameters: lcCuTick   ---> P/O or C/T number.
*!             lcOpration ---> Operation code.
*!             lcRecvLot  ---> Selected lot no.
*!             laQuantity ---> Array holding Received quantity
*!             lcRcvItem  ---> Received Style.
*!             lcRcvDyelot---> Received Dyelot.
*!*************************************************************
FUNCTION lfUpdLot
LPARAMETERS lcCTCode, lcOpration, lcRecvLot, laQuantity, lcRcvItem,;
  lcRcvDyelot

PRIVATE laAddQty, laOpenQty, lcSaveRecKey, lnCount, lcCount, lcLotNo,;
  lcSqlStatement, lcIMType, llFound

DECLARE laAddQty[9], laOpenQty[9]

lcIMType = IIF(llMFCall,'M',IIF(&lcTmpLine..cStyType="N","N",'I'))
STORE .F. TO llFound, llLotFound

*-- Get the information from the MFGOPRDT
*-- cimtyp+ctktno+coprcode+clotno+trancd
lcSqlStatement = "SELECT * FROM MFGOPRDT [INDEX=MFGOPRDT] "+;
  "WHERE cImTyp = '" + lcIMType +;
  "' AND cTktNo ='" + lcCTCode + "' AND cOprCode = '" + lcOpration + ;
  "' AND Item = '" + lcRcvItem + "' AND cDyelot ='" + lcRcvDyelot + "'"
=lfOpenSql(lcSqlStatement,'MFGOPRDT',lcTmpCur, "","",.F.)
DIMENSION laIndex[1,2]
laIndex = ''
laIndex[1,1] = 'CIMTYP+CTKTNO+COPRCODE+CLOTNO+TRANCD'
laIndex[1,2] = lcTmpCur
=lfSetIndex(lcTmpCur,@laIndex)

*N038893,1 WAM 06/02/2005 Fix receive last operation
*!*	SELECT(lcTmpCur)
*!*	=SEEK(lcIMType+lcCTCode+lcOpration+lcRecvLot+'1')
*!*	LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd=;
*!*	                  lcIMType+lcCTCode+lcOpration+lcRecvLot+'1' ;
*!*	            FOR Item = lcRcvItem AND cDyelot = lcRcvDyelot
*!*	llFound = FOUND()
*!*	*-- Check again to handle the case of receiving to 2nd quality or damage.
*!*	IF !llFound
*!*	  SELECT (lcMastOprDt)
*!*	  =SEEK(lcIMType+lcCTCode+lcOpration+lcRecvLot+'1')
*!*	  LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd=;
*!*	                    lcIMType+lcCTCode+lcOpration+lcRecvLot+'1' ;
*!*	              FOR Item = lcRcvItem AND cDyelot = lcRcvDyelot
*!*	ENDIF
SELECT(lcTmpCur)
SCAN
  SCATTER MEMVAR
  SELECT (lcMastOprDt)
  =SEEK(m.cimtyp+m.ctktno+m.coprcode+m.clotno+m.trancd)
  LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd= m.cimtyp+m.ctktno+m.coprcode+m.clotno+m.trancd ;
    FOR ITEM = m.Item AND cDyelot = m.cDyelot
  IF !FOUND()
    INSERT INTO (lcMastOprDt) FROM MEMVAR
    TABLEUPDATE(0,.T.,lcMastOprDt)
  ENDIF
ENDSCAN

SELECT (lcMastOprDt)
=SEEK(lcIMType+lcCTCode+lcOpration+lcRecvLot+'1')
LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd=;
  lcIMType+lcCTCode+lcOpration+lcRecvLot+'1' ;
  FOR ITEM = lcRcvItem AND cDyelot = lcRcvDyelot
*N038893,1 WAM 06/02/2005 (End)

IF !FOUND()
  IF &lcTmpLine..TranCd <> '5'
    =SEEK(lcIMType+lcCTCode+lcOpration)
    SCATTER MEMVAR MEMO

    lcSqlStatement  = "SELECT * FROM CODES " + ;
      "WHERE cDefCode+ccode_no+crltfield+cfld_name = 'N'"+;
      "+'"+PADR(lcOpration,6)+"'+'Y'+'MFGCODE   ' AND CRLTD_NAM = 'CCONTCODE'"

    =lfOpenFox(lcSqlStatement,'CODES',lcTmpCode,"")
    SELECT(lcTmpCode)
    LOCATE

    SELECT (lcMastOprDt)
    m.cDyelot = &lcTmpLine..Dyelot
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE cIMtyp    WITH lcIMType,;
      cTktNo    WITH lcCTCode,;
      ITEM      WITH &lcTmpLine..STYLE,;
      cLotNo    WITH lcRecvLot ,;
      cOprCode  WITH lcOpration,;
      cContcode WITH IIF(EMPTY(cContcode),ALLT(&lcTmpCode..cRltd_vlu),cContcode),;
      dTranDate WITH oAriaApplication.SystemDate ,;
      DueDate   WITH oAriaApplication.SystemDate ,;
      TranCd    WITH '1'       ,;
      cTrgOpr   WITH SPACE(2)  ,;
      cTrgLot   WITH SPACE(2)  ,;
      cInvType  WITH lcInvType

    REPLACE nLotQty1   WITH 0,;
      nLotQty2   WITH 0,;
      nLotQty3   WITH 0,;
      nLotQty4   WITH 0,;
      nLotQty5   WITH 0,;
      nLotQty6   WITH 0,;
      nLotQty7   WITH 0,;
      nLotQty8   WITH 0,;
      nLotTotQty WITH 0,;
      cInvType   WITH lcInvType
  ENDIF
ENDIF
*N038893,1 WAM 06/02/2005 Fix receive last operation
*!*	IF llFound
*!*	  SELECT(lcTmpCur)
*!*	ELSE
*!*	  SELECT (lcMastOprDt)
*!*	ENDIF
SELECT (lcMastOprDt)
*N038893,1 WAM 06/02/2005 (End)

=SEEK(lcIMType+lcCTCode+lcOpration)

DO WHILE cIMTyp+cTktNo+cOprCode = lcIMType+lcCTCode+lcOpration AND !EOF()
  IF ITEM+cDyelot <> lcRcvItem+lcRcvDyelot
    SKIP
    LOOP
  ENDIF
  IF !EMPTY(lcRecvLot) AND cLotNo<>lcRecvLot
    SKIP
    LOOP
  ENDIF

  lcLotNo    = cLotNo
  llLotFound = .T.

  *--Check if there is any open quantity for this Opr/Lor/Clr
  STORE 0 TO laOpenQty
  SCAN REST WHILE cIMTyp+cTktNo+cOprCode+cLotNo = lcIMType+lcCTCode+lcOpration+lcLotNo ;
      FOR ITEM =lcRcvItem AND cDyelot = lcRcvDyelot
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      laOpenQty[lnCount] = MAX(laOpenQty[lnCount] + ;
        IIF(TranCd='1',nLotQty&lcCount,-nLotQty&lcCount),0)
      laOpenQty[9] = laOpenQty[9] + laOpenQty[lnCount]
    ENDFOR
  ENDSCAN

  *--Comupte quantity to be added for this Opr/Lot/Clr
  STORE 0 TO laAddQty
  IF laOpenQty[9] > 0
    FOR lnCount = 1 TO 8
      laAddQty[lnCount]    = MIN(&laQuantity[lnCount],laOpenQty[lnCount])
      &laQuantity[lnCount] = &laQuantity[lnCount] - laAddQty[lnCount]
      laAddQty[9]          = laAddQty[9]    + laAddQty[lnCount]
      &laQuantity[9]       = &laQuantity[9] - laAddQty[lnCount]
    ENDFOR
    IF laAddQty[9]>0
      lnSavRec=RECNO()
      =lfAppDetRec(lcLotNo, &lcTmpLine..TranCd,'laAddQty',lcCTCode,lcRcvItem,lcRcvDyelot)

      IF BETWEEN(lnSavRec,1,RECCOUNT())
        GOTO lnSavRec
      ENDIF

    ENDIF
  ENDIF
ENDDO

*--If there is over received quantity, Update the last lot with the remaind quantity.
IF llLotFound AND &laQuantity[9] > 0 .AND.  &lcTmpLine..TranCd <> '5'
  =lfAppDetRec(lcLotNo, &lcTmpLine..TranCd,'&laQuantity',lcCTCode,lcRcvItem,lcRcvDyelot)
ENDIF
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
RELEASE laAddQty, laOpenQty, lcSaveRecKey, lnCount, lcCount, lcLotNo, lcSqlStatement, lcIMType, llFound,;
  llLotFound,laIndex,lnSavRec
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]

RETURN

*!***************************************************************************
*! Name      : lfAppDetRec
*! Developer : Khalid Mohi El-Din
*! Date      : 09/11/2004
*! Purpose   : Function to append transaction record in the
*!             MFG operation detailed file.
*!*************************************************************
*! Parameters: lcLotNum   ---> Lot number
*!             lcTranCode ---> Transaction code
*!             laNewQty   ---> Array holding Quantity to be added
*!             lcPoNo     ---> Work Order No.
*!             lcRcvItem  ---> Received Style.
*!             lcRcvDyelot---> Received Dyelot.
*!*************************************************************
FUNCTION lfAppDetRec
PARAMETERS lcLotNum, lcTranCode, laNewQty, lcPoNo,lcRcvItem,lcRcvDyelot)
LOCAL llExists
llExists = .F.
IF !llMFCall
  lcTranCode = IIF(lcTranCode='4','3',IIF(lcTranCode='5','4',lcTranCode))
ENDIF

*N038893,1 WAM 06/02/2005 Fix receive last operation
*!*	IF llFound
*!*	  SELECT(lcTmpCur)
*!*	ELSE
*!*	  SELECT (lcMastOprDt)
*!*	ENDIF
*!*	*-- Get the record of the current receiving type (lcTranCode)
*!*	=SEEK(lcIMType+lcPoNo+lcLastOpr+lcLotNum+lcTranCode)
*!*	LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd=;
*!*	                  lcIMType + lcPoNo+lcLastOpr+lcLotNum+lcTranCode ;
*!*	              FOR Item = lcRcvItem AND cDyelot = lcRcvDyelot
*!*	*-- If there is no records this means that's its a new record therefore, we will seek
*!*	*-- for the budget record
*!*	IF !FOUND()
*!*	  =SEEK(lcIMType+lcPoNo+lcLastOpr+lcLotNum+'1')
*!*	  LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd=;
*!*	                    lcIMType + lcPoNo+lcLastOpr+lcLotNum+'1' ;
*!*	                FOR Item = lcRcvItem AND cDyelot = lcRcvDyelot
*!*	ENDIF

*!*	SCATTER MEMVAR MEMO
*!*	SELECT (lcMastOprDt)
*!*	=SEEK(lcIMType+lcPoNo+lcLastOpr+lcLotNum+lcTranCode)
*!*	LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd=;
*!*	                  lcIMType + lcPoNo+lcLastOpr+lcLotNum+lcTranCode ;
*!*	              FOR Item = lcRcvItem AND cDyelot = lcRcvDyelot
*!*	IF !FOUND()
*!*	  APPEND BLANK
*!*	  IF SEEK(lcIMType+lcPoNo+lcLastOpr+lcLotNum+lcTranCode,lcTmpCur)
*!*	    SELECT(lcTmpCur)
*!*	    LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd=;
*!*	                     lcIMType + lcPoNo+lcLastOpr+lcLotNum+lcTranCode ;
*!*	                FOR Item = lcRcvItem AND cDyelot = lcRcvDyelot
*!*
*!*	    IF FOUND()
*!*	      SELECT (lcMastOprDt)
*!*	      TABLEUPDATE(0,.T.)
*!*	    ENDIF
*!*	  ENDIF
*!*	  GATHER  MEMVAR MEMO
*!*	ENDIF
*!*	SELECT (lcMastOprDt)
*!*	REPLACE nLotQty1   WITH nLotQty1 + &laNewQty[1] ,;
*!*	        nLotQty2   WITH nLotQty2 + &laNewQty[2] ,;
*!*	        nLotQty3   WITH nLotQty3 + &laNewQty[3] ,;
*!*	        nLotQty4   WITH nLotQty4 + &laNewQty[4] ,;
*!*	        nLotQty5   WITH nLotQty5 + &laNewQty[5] ,;
*!*	        nLotQty6   WITH nLotQty6 + &laNewQty[6] ,;
*!*	        nLotQty7   WITH nLotQty7 + &laNewQty[7] ,;
*!*	        nLotQty8   WITH nLotQty8 + &laNewQty[8] ,;
*!*	        nLotTotQty WITH nLotTotQty + &laNewQty[9] ,;
*!*	        dTranDate  WITH oAriaApplication.SystemDate    ,;
*!*	        DueDate    WITH {}           ,;
*!*	        TranCd     WITH lcTranCode   ,;
*!*	        cTrgOpr    WITH SPACE(2)     ,;
*!*	        cTrgLot    WITH SPACE(2)     ,;
*!*	        cOwner     WITH ' '			 ,;
*!*	        cInvType   WITH lcInvType

SELECT (lcMastOprDt)
*-- Get the record of the current receiving type (lcTranCode)
=SEEK(lcIMType+lcPoNo+lcLastOpr+lcLotNum+lcTranCode)
LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd=;
  lcIMType + lcPoNo+lcLastOpr+lcLotNum+lcTranCode ;
  FOR ITEM = lcRcvItem AND cDyelot = lcRcvDyelot
*-- If there is no records this means that's its a new record therefore, we will seek
*-- for the budget record
IF !FOUND()
  =SEEK(lcIMType+lcPoNo+lcLastOpr+lcLotNum+'1')
  LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd=;
    lcIMType + lcPoNo+lcLastOpr+lcLotNum+'1' ;
    FOR ITEM = lcRcvItem AND cDyelot = lcRcvDyelot
  SCATTER MEMVAR MEMO
  STORE 0 TO m.nLotQty1, m.nLotQty2,m.nLotQty3,m.nLotQty4,m.nLotQty5,m.nLotQty6,m.nLotQty7,m.nLotQty8,m.nlottotqty
  APPEND BLANK
  GATHER  MEMVAR MEMO
  *:B608366,1 MMT 11/29/2007 fix bug of not adding record for last MFG in operation dt File[Start]
  *!*	ELSE
  *!*	  TABLEUPDATE(0,.T.)
  *:B608366,1 MMT 11/29/2007 fix bug of not adding record for last MFG in operation dt File[End]
ENDIF
SELECT (lcMastOprDt)
REPLACE nLotQty1   WITH nLotQty1 + &laNewQty[1] ,;
  nLotQty2   WITH nLotQty2 + &laNewQty[2] ,;
  nLotQty3   WITH nLotQty3 + &laNewQty[3] ,;
  nLotQty4   WITH nLotQty4 + &laNewQty[4] ,;
  nLotQty5   WITH nLotQty5 + &laNewQty[5] ,;
  nLotQty6   WITH nLotQty6 + &laNewQty[6] ,;
  nLotQty7   WITH nLotQty7 + &laNewQty[7] ,;
  nLotQty8   WITH nLotQty8 + &laNewQty[8] ,;
  nLotTotQty WITH nLotTotQty + &laNewQty[9] ,;
  dTranDate  WITH oAriaApplication.SystemDate  ,;
  DueDate    WITH {}           ,;
  TranCd     WITH lcTranCode   ,;
  cTrgOpr    WITH SPACE(2)     ,;
  cTrgLot    WITH SPACE(2)     ,;
  cOwner     WITH ' '			 ,;
  cInvType   WITH lcInvType
*N038893,1 WAM 06/02/2005 (End)

*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
RELEASE llExists
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]

RETURN(.T.)


*!***************************************************************************
*! Name      : lfUpdWip
*! Developer : Khalid Mohi El-Din
*! Date      : 09/11/2004
*! Purpose   : Function to update the usage fields in ItemLoc
*!******************************************************************************
*! Parameters: loFormSet   : FormSet
*!******************************************************************************
FUNCTION lfUpdWip

PRIVATE lcTmpWip,lcTmpWip1
lcTmpWip  = gfTempName()
lcTmpWip1 = gfTempName()
LOCAL ARRAY laFileStru[12,18]
laFileStru[01,1] = 'CWARECODE'
laFileStru[01,2] = 'C'
laFileStru[01,3] = 6
laFileStru[01,4] = 0

laFileStru[02,1] = 'NCUSAGE1'
laFileStru[02,2] = 'N'
laFileStru[02,3] = 12
laFileStru[02,4] = 3

laFileStru[03,1] = 'NCUSAGE2'
laFileStru[03,2] = 'N'
laFileStru[03,3] = 12
laFileStru[03,4] = 3

laFileStru[04,1] = 'NCUSAGE3'
laFileStru[04,2] = 'N'
laFileStru[04,3] = 12
laFileStru[04,4] = 3

laFileStru[05,1] = 'NCUSAGE4'
laFileStru[05,2] = 'N'
laFileStru[05,3] = 12
laFileStru[05,4] = 3

laFileStru[06,1] = 'NCUSAGE5'
laFileStru[06,2] = 'N'
laFileStru[06,3] = 12
laFileStru[06,4] = 3

laFileStru[07,1] = 'NCUSAGE6'
laFileStru[07,2] = 'N'
laFileStru[07,3] = 12
laFileStru[07,4] = 3

laFileStru[08,1] = 'NCUSAGE7'
laFileStru[08,2] = 'N'
laFileStru[08,3] = 12
laFileStru[08,4] = 3

laFileStru[09,1] = 'NCUSAGE8'
laFileStru[09,2] = 'N'
laFileStru[09,3] = 12
laFileStru[09,4] = 3

laFileStru[10,1] = 'NTOTCUSA'
laFileStru[10,2] = 'N'
laFileStru[10,3] = 13
laFileStru[10,4] = 3

laFileStru[11,1] = 'DTRDATE'
laFileStru[11,2] = 'D'
laFileStru[11,3] = 8
laFileStru[11,4] = 0

laFileStru[12,1] = 'CISESSION'
laFileStru[12,2] = 'C'
laFileStru[12,3] = 6
laFileStru[12,4] = 0

LOCAL lnI,lnJ
FOR lnI = 1 TO 12
  FOR lnJ = 7 TO 16
    laFileStru[lnI,lnJ] = ''
  ENDFOR
  STORE 0 TO laFileStru[lnI,17],laFileStru[lnI,18]
ENDFOR

LOCAL ARRAY laIndex[2,2]
laIndex[1,1] = 'DTOS(DTRDATE)+CWARECODE'
laIndex[1,2] = lcTmpWip
laIndex[2,1] = 'CWARECODE+CISESSION'
laIndex[2,2] = lcTmpWip1
=gfCrtTmp(lcTmpWip,@laFileStru,@laIndex)
SET ORDER TO TAG (lcTmpWip) IN (lcTmpWip)

LOCAL lcBomLine,lcMatInvJl,laCurrRecQ
lcBomLine  = gfTempName()
lcMatInvJl = gfTempName()
m.cImTyp   = IIF(llMFCall,'M',IIF(&lcTmpLine..cStyType="N","N",'I'))
m.cType    = '2'
m.CtktNo   = EVALUATE(lcMastPoHd+'.PO')
DIMENSION laCurrRecQ[8]
laCurrRecQ = 0

SELECT (lcTmpLine)

PRIVATE lcSeekExpr,lcWhileExp,lnOpnQty,lnOpnQtySt,lnI,lcI,lcOrder,lnBomLnRec,lcFabric,lcDyelot

IF llMFCall
  lcSeekExpr = 'M1'+SPACE(6)+PO+STR(LINENO,6)
  **: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [Start]
  *lcWhileExp = "cIMTyp+cType+cTktNo+STR(lineno,6)=lcSeekExpr"
  lcWhileExp = "cIMTyp+cType+Shipno+cTktNo+STR(LineNo,6)=lcSeekExpr"
  *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [End]
ELSE
  lcSeekExpr = IIF(lcRecvType = 'D','D','I')+'1'+IIF(lcRecvType = 'S',Shipno,SPACE(6))+Po+STR(LINENO,6)
  lcWhileExp = "cIMTyp+cType+Shipno+cTktNo+STR(LineNo,6)=lcSeekExpr"
ENDIF

IF SEEK(lcSeekExpr,lcMastBomLn)
  SELECT (lcMastBomLn)
  SCAN REST WHILE &lcWhileExp. FOR CCATGTYP $ 'FT'
    FOR lnI = 1 TO LEN(ALLTRIM(CSIZES))
      lcI = SUBSTR(ALLTRIM(CSIZES),lnI,1)
      laCurrRecQ[lnI] = laCurrRecQ[lnI]+ MIN(EVALUATE(lcTmpLine+'.Qty'+lcI), laOpnQty[lnI])
    ENDFOR
    lnBomLnRec = RECNO()
    lcFabric   = ITEM
    lcDyelot   = DYELOT
    *: B607982,1 MMT 02/20/2007 Get item used quantity for previous receivings  [Start]
    lnOpnQtySt = 0
    IF llMFCall
      IF SEEK('M2'+SPACE(6)+EVALUATE(lcTmpLine+'.PO'))
        SUM REST WHILE cIMTyp+cType+Shipno+cTktNo+STR(LINENO,6)='M2'+SPACE(6)+EVALUATE(lcTmpLine+'.PO');
          FOR ITEM = lcFabric .AND. DYELOT = lcDyelot .AND. !EMPTY(cRSession) ITEMQTY TO lnOpnQtySt
      ENDIF
    ELSE
      IF SEEK(IIF(lcRecvType ='D','D','I')+'2'+IIF(lcRecvType ='S',EVALUATE(lcTmpLine+'.Shipno'),SPACE(6))+EVALUATE(lcTmpLine+'.Po'))
        SUM REST WHILE cIMTyp+cType+Shipno+cTktNo+STR(LINENO,6)=;
          IIF(lcRecvType ='D','D','I')+'2'+IIF(lcRecvType ='S',EVALUATE(lcTmpLine+'.Shipno'),SPACE(6))+EVALUATE(lcTmpLine+'.Po');
          FOR ITEM = lcFabric .AND. DYELOT = lcDyelot .AND. !EMPTY(cRSession) ITEMQTY TO lnOpnQtySt
      ENDIF
    ENDIF
    IF BETWEEN(lnBomLnRec,1,RECCOUNT())
      GO lnBomLnRec
    ENDIF
    *: B607982,1 MMT 02/20/2007 Get item used quantity for previous receivings  [End]

    SELECT (lcTmpWip)
    ZAP
    *-- CINVTYPE+STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)

    *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
    *lcSqlStatement  = "SELECT * FROM ITEMJRNL [INDEX=STYINVJL] "+;
    "WHERE cInvType='" + '0002' + "' AND Style ='"+ lcFabric + "'"
    lcSqlStatement  = "SELECT * FROM ITEMJRNL [INDEX=STYINVJL] "+;
      "WHERE cInvType='" + '0002' + "' AND Style = ?m.lcFabric "
    *B607658,1 KHM 07/07/2005 [End]

    =lfOpenSql(lcSqlStatement,'ITEMJRNL',lcMatInvJl, "","",.F.)
    DIMENSION laIndex[1,2]
    laIndex = ''
    laIndex[1,1] = 'CINVTYPE+STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)'
    laIndex[1,2] = lcMatInvJl
    =lfSetIndex(lcMatInvJl,@laIndex)

    SELECT (lcMastBomLn)

    IF SEEK('0002'+lcFabric,lcMatInvJl)
      SELECT (lcMatInvJl)
      **: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [Start]
      *SCAN REST WHILE CINVTYPE+STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)=;
      '0002'+lcFabric;
      FOR CDYELOT = lcDYELOT .AND. CTRTYPE = '9' .AND. CIMTYP = m.cImTyp .AND.;
      CTKTNO = EVALUATE(lcTmpLine+'.PO')
      SCAN REST WHILE CINVTYPE+STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)=;
          '0002'+lcFabric;
          FOR CDYELOT = lcDYELOT .AND. CTRTYPE = '9' .AND. IIF(m.cImTyp='M',CBusDocu+cStytype = 'PU',CBusDocu+cStytype = 'PP') .AND.;
          CTRCODE = EVALUATE(lcTmpLine+'.PO')

        SELECT (lcTmpWip)
        SET ORDER TO TAG (lcTmpWip1)
        IF !SEEK(EVALUATE(lcMATINVJL+'.CWARECODE')+EVALUATE(lcMATINVJL+'.CISESSION'))
          INSERT INTO (lcTmpWip) (CWARECODE,DTRDATE,CISESSION);
            VALUES (EVALUATE(lcMATINVJL+'.CWARECODE'),EVALUATE(lcMATINVJL+'.DTRDATE'),EVALUATE(lcMATINVJL+'.CISESSION'))
        ENDIF
        SET ORDER TO TAG (lcTmpWip)
        SELECT (lcMatInvJl)

        *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage  [End]
        IF CIRTYPE = 'I'
          **: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [Start]
          * INSERT INTO (lcTmpWip) (CWARECODE,NCUSAGE1,NCUSAGE2,NCUSAGE3,NCUSAGE4,NCUSAGE5,NCUSAGE6,NCUSAGE7,NCUSAGE8,;
          NTOTCUSA,DTRDATE,CISESSION);
          VALUES (EVALUATE(lcMATINVJL+'.CWARECODE'),EVALUATE(lcMATINVJL+'.NSTK1')*-1,;
          EVALUATE(lcMATINVJL+'.NSTK2')*-1,EVALUATE(lcMATINVJL+'.NSTK3')*-1,;
          EVALUATE(lcMATINVJL+'.NSTK4')*-1,EVALUATE(lcMATINVJL+'.NSTK5')*-1,;
          EVALUATE(lcMATINVJL+'.NSTK6')*-1,EVALUATE(lcMATINVJL+'.NSTK7')*-1,;
          EVALUATE(lcMATINVJL+'.NSTK8')*-1,EVALUATE(lcMATINVJL+'.NTOTSTK')*-1,;
          EVALUATE(lcMATINVJL+'.DTRDATE'),EVALUATE(lcMATINVJL+'.CISESSION'))
          SELECT (lcTmpWip)
          =SEEK(EVALUATE(lcMATINVJL+'.CWARECODE')+EVALUATE(lcMATINVJL+'.CISESSION'),lcTmpWip,lcTmpWip1)
          REPLACE  NCUSAGE1 WITH NCUSAGE1 + EVALUATE(lcMATINVJL+'.NSTK1')*-1 ,;
            NCUSAGE2 WITH NCUSAGE2 + EVALUATE(lcMATINVJL+'.NSTK2')*-1 ,;
            NCUSAGE3 WITH NCUSAGE3 + EVALUATE(lcMATINVJL+'.NSTK3')*-1 ,;
            NCUSAGE4 WITH NCUSAGE4 + EVALUATE(lcMATINVJL+'.NSTK4')*-1 ,;
            NCUSAGE5 WITH NCUSAGE5 + EVALUATE(lcMATINVJL+'.NSTK5')*-1 ,;
            NCUSAGE6 WITH NCUSAGE6 + EVALUATE(lcMATINVJL+'.NSTK6')*-1 ,;
            NCUSAGE7 WITH NCUSAGE7 + EVALUATE(lcMATINVJL+'.NSTK7')*-1 ,;
            NCUSAGE8 WITH NCUSAGE8 + EVALUATE(lcMATINVJL+'.NSTK8')*-1 ,;
            NTOTCUSA WITH NTOTCUSA + EVALUATE(lcMATINVJL+'.NTOTSTK')*-1

          *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [End]
        ELSE
          SELECT (lcTmpWip)
          SET ORDER TO TAG (lcTmpWip1)
          *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [Start]
          *IF SEEK(EVALUATE(lcMATINVJL+'.CWARECODE')+EVALUATE(lcMATINVJL+'.CISESSION'))
          IF SEEK(EVALUATE(lcMATINVJL+'.CWARECODE')+EVALUATE(lcMATINVJL+'.CISESSION'),lcTmpWip,lcTmpWip1)
            *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [End]
            REPLACE NCUSAGE1 WITH NCUSAGE1 - EVALUATE(lcMATINVJL+'.NSTK1'),;
              NCUSAGE2 WITH NCUSAGE2 - EVALUATE(lcMATINVJL+'.NSTK2'),;
              NCUSAGE3 WITH NCUSAGE3 - EVALUATE(lcMATINVJL+'.NSTK3'),;
              NCUSAGE4 WITH NCUSAGE4 - EVALUATE(lcMATINVJL+'.NSTK4'),;
              NCUSAGE5 WITH NCUSAGE5 - EVALUATE(lcMATINVJL+'.NSTK5'),;
              NCUSAGE6 WITH NCUSAGE6 - EVALUATE(lcMATINVJL+'.NSTK6'),;
              NCUSAGE7 WITH NCUSAGE7 - EVALUATE(lcMATINVJL+'.NSTK7'),;
              NCUSAGE8 WITH NCUSAGE8 - EVALUATE(lcMATINVJL+'.NSTK8'),;
              NTOTCUSA WITH NTOTCUSA - EVALUATE(lcMATINVJL+'.NTOTSTK')
          ENDIF
          SET ORDER TO TAG (lcTmpWip)
        ENDIF
      ENDSCAN
    ENDIF
    IF RECCOUNT(lcTmpWip) > 0
      STORE 0 TO lnRemQty, lnRemQtySt
      FOR lnCount = 1 TO 8
        laCurrRecQ[lnCount] = laCurrRecQ[lnCount]*EVALUATE(lcMastBomLn+'.UNITQTY')
        lnRemQty   = lnRemQty + laCurrRecQ[lnCount]
        lnRemQtySt = lnRemQtySt + laPrevRecQ[lnCount]
      ENDFOR
      *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage  [Start]
      lnRemQtySt = lnOpnQtySt
      *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [End]

      SELECT (lcTmpWip)
      LOCATE
      SCAN

        *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
        lcKey = '0002'+EVALUATE(lcMastBomLn+'.Item')
        *B607935,1 WAM 01/17/2007 Fix error "variable lcTmpItm not found' while saving style PO
        *IF !SEEK(lcKey,lcTmpItmDye)
        IF !SEEK(lcKey,lcTmpItm)
          *B607935,1 WAM 01/17/2007 (End)
          lcItemValue    = EVALUATE(lcMastBomLn+'.Item')
          lcSqlStatement = "SELECT * FROM ITEM (INDEX=STYLE) WHERE cInvType = '0002'"+;
            " AND Style = ?m.lcItemValue "
          =lfGetItmInf('0002',lcKey,lcTmpItm,'ITEM',;
            lcSqlStatement,.T.)
          *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [Start]
          *!*	          =SEEK(lcKey,lcTmpItm)
          *!*	          SELECT (lcTmpItm)
          *!*	          REPLACE NTOTCUSA WITH NTOTCUSA &lcWipSgn MAX(MIN(EVALUATE(lcTmpWip+'.NCUSAGE1')-lnRemQtySt,lnRemQty),0)
          *!*	          *B607935,1 WAM 01/17/2007 Fix error "variable lcTmpItm not found' while saving style PO
          *!*	          SELECT (lcTmpWip)
          *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [End]
          *B607935,1 WAM 01/17/2007 (End)
        ENDIF
        *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [Start]
        =SEEK(lcKey,lcTmpItm)
        SELECT (lcTmpItm)
        REPLACE NTOTCUSA WITH NTOTCUSA &lcWipSgn MAX(MIN(EVALUATE(lcTmpWip+'.NCUSAGE1')-lnRemQtySt,lnRemQty),0)
        SELECT (lcTmpWip)
        *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage [End]

        lcKey = '0002'+EVALUATE(lcMastBomLn+'.Item')+cWareCode
        IF !SEEK(lcKey,lcTmpItmDye)
          *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
          *lcSqlStatement = "SELECT * FROM ITEMLOC WHERE cInvType = '0002'"+;
          " AND Style ='" + EVALUATE(lcMastBomLn+'.Item') +;
          "' AND cWareCode='" + cWareCode +;
          "' AND Dyelot = '" + SPACE(10) + "'"

          lcItemValue   = EVALUATE(lcMastBomLn+'.Item')
          lcWareCodeVal = cWareCode
          lcSqlStatement = "SELECT * FROM ITEMLOC WHERE cInvType = '0002'"+;
            " AND Style = ?m.lcItemValue " +;
            " AND cWareCode = ?m.lcWareCodeVal " +;
            " AND Dyelot = '" + SPACE(10) + "'"
          *B607658,1 KHM 07/07/2005 [End]
          =lfGetItmInf('0002',lcKey+SPACE(10),lcTmpItmDye,'ITEMLOC',;
            lcSqlStatement,.T.)
          *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage [Start]
          *!*	          =SEEK(lcKey+SPACE(10),lcTmpItmDye)
          *!*	          SELECT (lcTmpItmDye)
          *!*	          REPLACE NCUSAGE1 WITH NCUSAGE1 &lcWipSgn MAX(MIN(EVALUATE(lcTmpWip+'.NCUSAGE1')-lnRemQtySt,lnRemQty),0),;
          *!*	                  NTOTCUSA WITH NCUSAGE1
          *!*	          *B607935,1 WAM 01/17/2007 Fix error "variable lcTmpItm not found' while saving style PO
          *!*	          SELECT (lcTmpWip)
          *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage [End]
          *B607935,1 WAM 01/17/2007 (End)
        ENDIF
        *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage [Start]
        =SEEK(lcKey+SPACE(10),lcTmpItmDye)
        SELECT (lcTmpItmDye)
        REPLACE NCUSAGE1 WITH NCUSAGE1 &lcWipSgn MAX(MIN(EVALUATE(lcTmpWip+'.NCUSAGE1')-lnRemQtySt,lnRemQty),0),;
          NTOTCUSA WITH NCUSAGE1
        SELECT (lcTmpWip)
        *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage [End]
        IF !EMPTY(lcDYELOT)
          *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
          *lcSqlStatement = "SELECT * FROM ITEMLOC WHERE cInvType = '0002'"+;
          " AND Style ='" + EVALUATE(lcMastBomLn+'.Item') +;
          "' AND cWareCode='" + cWareCode +;
          "' AND Dyelot = '" + lcDyelot + "'"

          lcItemValue   = EVALUATE(lcMastBomLn+'.Item')
          lcWareCodeVal = cWareCode
          lcSqlStatement = "SELECT * FROM ITEMLOC WHERE cInvType = '0002'"+;
            " AND Style = ?m.lcItemValue " + ;
            " AND cWareCode = ?m.lcWareCodeVal " +;
            " AND Dyelot = '" + lcDyelot + "'"
          *B607658,1 KHM 07/07/2005

          =lfGetItmInf('0002',lcKey+lcDyelot,lcTmpItmDye,'ITEMLOC',lcSqlStatement,.T.)
          IF SEEK(lcKey+lcDyelot,lcTmpItmDye)
            SELECT (lcTmpItmDye)
            REPLACE NCUSAGE1 WITH NCUSAGE1 &lcWipSgn MAX(MIN(EVALUATE(lcTmpWip+'.NCUSAGE1')-lnRemQtySt,lnRemQty),0),;
              NTOTCUSA WITH NCUSAGE1
          ENDIF
        ENDIF
        **: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [Start]
        lnRemQty   = MIN(MAX(lnRemQty-EVALUATE(lcTmpWip+'.NCUSAGE1')+lnRemQtySt,0),lnRemQty)
        lnRemQtySt = MAX(lnRemQtySt-EVALUATE(lcTmpWip+'.NCUSAGE1'),0)
        **: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [End]
      ENDSCAN
    ENDIF
  ENDSCAN
ENDIF
USE IN (lcTmpWip)
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
RELEASE lcTmpWip,lcTmpWip1,laFileStru,lnI,lnJ,laIndex,lcBomLine,lcMatInvJl,laCurrRecQ,;
  lcSeekExpr,lcWhileExp,lnOpnQty,lnOpnQtySt,lnI,lcI,lcOrder,lnBomLnRec,lcFabric,lcDyelot,;
  lnRemQty, lnRemQtySt ,lnCount ,lcKey ,lcItemValue    ,lcSqlStatement ,lcWareCodeVal
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]

*!***************************************************************************
*! Name      : lfSetKeyPro
*! Developer : Khalid Mohi El-Din
*! Date      : 09/11/2004
*! Purpose   : To assign the aria work order key properties
*!******************************************************************************
*! Parameters: loFormSet   : FormSet, lcRecvType: Receiving type
*!******************************************************************************
FUNCTION lfSetKeyPro
LPARAMETERS loFormSet, lcRecvType
PRIVATE lcVendor

DO CASE
  *-- 'S' Receeve Style PO Shipment, 'F' Receiving Material PO Shipment
  *-- 'C' Receive Inter-Location PO Shipment
CASE lcRecvType $ "SFCU"
  LOCAL llNoRecordFound, llServerError

  WITH loFormSet
    .cBrowseFileName        = "SHPMTHDR"
    .cBrowseIndexExpression = "CBUSDOCU+CSHPTYPE+SHIPNO"
    .cBrowseIndexFields     = "CBUSDOCU,CSHPTYPE,SHIPNO"
    .cBrowseIndexName       = "SHPMTHDR"
    .cBrowseKey             = loFormSet.lcBusDoc + loFormSet.lcWorkOrd
    .cBrowseAliasName       = loFormSet.lcMastShp
    .cBrowseTableName       = "SHPMTHDR"
    .oRemoteCursor.mGetCursor(loFormSet.lcMastShp,loFormSet.lcMastShp,.DATASESSIONID,;
      'STRUCTURE',.F.,.cBrowseTableName,'*','',.cBrowseIndexName,;
      .cBrowseIndexExpression,.F.,.cBrowseIndexFields,0,;
      .cBrowseFilter,.cBrowseKey,@llNoRecordFound,@llServerError)
    .cbrowsefields = "ShipNo:H='Shipment #', Status, Entered, Cartons,"+;
      "AirWayB:H='Air-way Bill#', ETA:H='E.T.A.',"+;
      "TotQtyHdr:H='In-Transit', Recv_Stk:H='Received',"+;
      "Recv_Dam:H='Damaged', Recv_Can:H='Canceled',"+;
      "cVessel:H='Airline / Vessel' ,Reference"

  ENDWITH


  *-- 'L' Receive Inter Location P/O Batch, 'T' Receive C/T Batch, 'B' Receive P/O Batch
CASE lcRecvType $ "LTB"

OTHERWISE
  loFormSet.cBrowseAliasName       = loFormSet.lcPosHdr
  loFormSet.cBrowseKey             = loFormSet.lcBusDoc + loFormSet.lcWorkOrd
  loFormSet.cBrowseIndexExpression = "CBUSDOCU+CSTYTYPE+PO"
  loFormSet.cBrowseIndexFields     = "CBUSDOCU,CSTYTYPE,PO"
  loFormSet.cBrowseIndexName       = "POSHDR"
  loFormSet.cBrowseTableName       = "POSHDR"
  loFormSet.cBrowseFileName        = "POSHDR"

  WITH loFormSet.ariaform1.kbPONo
    .cBusinessDocumentType = loFormSet.lcBusDoc
    .cWorkOrderType        = loFormSet.lcWorkOrd
    .oBrowseCursor         = loFormSet.lcPosHdr
    *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
    IF loFormSet.lcWorkOrd <> 'U'
      *N038893,1 WAM 06/02/2005 (End)
      .cbrowsetitle          = IIF(loFormSet.lcWorkOrd = 'N',;
        LANG_POSTREC_IntrLocBrowseTitle,LANG_POSTREC_BrowseTitle)
      lcVendor = IIF(loFormSet.lcWorkOrd = 'N',LANG_POSTREC_SourceLBrowTitl,;
        LANG_POSTREC_VendorBrowTitl)
      .cbrowsefields = "PO        :H='"+LANG_POSTREC_POBrowTitl +"',"+;
        "Status    :H='"+LANG_POSTREC_StatusBrowTitl +"',"+;
        "Vendor    :H='"+lcVendor +"' ,"+;
        "Entered   :H='"+LANG_POSTREC_EnteredBrowTitl+"' ,"+;
        "Complete  :H='"+LANG_POSTREC_CompleteBrowTitl+"' ,"+;
        "nStyOrder :H='"+LANG_POSTREC_TotQtyBrowTitl+"' ,"+;
        "POTotal   :H='"+LANG_POSTREC_AmountBrowTitl+"' ,"+;
        "Receive   :H='"+LANG_POSTREC_ReceiveBrowTitl+"' ,"+;
        "Open      :H='"+LANG_POSTREC_OpenBrowTitl+"' "
      *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
    ELSE
      .cbrowsetitle  = 'Cutting Tickets'
      .cbrowsefields = "PO          :H='"+"Cutkt#"+"' ,"+;
        "Status    :H='"+"Status"+"' ,"+;
        "Style     :H='"+"Style"+"' ,"+;
        "cDivision :H='"+"Division"+"' ,"+;
        "Entered   :H='"+"Entered"+"' ,"+;
        "Complete  :H='"+"Complete"+"' ,"+;
        "nStyOrder :H='"+"Total Qty."+"' ,"+;
        "POTotal   :H='"+"Amount"+"' ,"+;
        "Receive   :H='"+"Received"+"' ,"+;
        "Open      :H='"+LANG_POSTREC_OpenBrowTitl+"' "
    ENDIF
    *N038893,1 WAM 06/02/2005 (End)
    SELECT(.oBrowseCursor)
  ENDWITH
ENDCASE

*!*************************************************************
*! Name      : lfSelLots
*! Developer : Khalid Mohi El-Din
*! Date      : 09/11/2004
*! Purpose   : To select lot number to receive to.
*!*************************************************************
*! Parameters: lcTktLn  : Style line no.
*!		       lcHdrLOpr: Last Operation
*!		       loFormSet: FormSet
*!*************************************************************
*! Returns   : Selected lot number
*!*************************************************************
FUNCTION lfSelLots
LPARAMETERS lcTktLn,lcHdrLOpr,loFormSet
LOCAL lnAlias, lcSqlStatement, lcCstShtType
PRIVATE lcTmpCur,lcTmpCode
lnAlias  = SELECT()
lcTmpCur = gfTempName()
lcTmpCode = gfTempName()
*-- To check the type of style cost sheet
DO CASE
  *-- 'I' Receive P/O
CASE loFormSet.lcPType $ 'ISB'
  lcCstShtType = 'I'

  *-- 'M' Receive C/T
CASE loFormSet.lcPType $ 'MT'
  lcCstShtType = 'M'

  *-- 'N' Issue Inter-Location P/O, 'O' Receive Inter-Location P/O
CASE loFormSet.lcPType $ 'NO'
  lcCstShtType = 'N'
ENDCASE

*-- Get BOMLINE records
lcSqlStatement = "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+;
  "WHERE cImTyp = '" + lcCstShtType +;
  "' AND cTktNo ='" + loFormSet.lcPoNo +;
  "' AND cCatgTyp = 'M'" + " AND MfgCode <> SPACE(6)" + ;
  "  AND [LineNo] = '" + lcTktLn + "'"
=lfOpenSql(lcSqlStatement,'BOMLINE',lcTmpCur, "","",.F.)

SELECT (lcTmpCur)
IF EMPTY(lcHdrLOpr)

  *--Get the Last operation for this Style/Color LINE NO.
  STORE ' ' TO lcLastMfgOr
  lnMaxSeq = 0
  SCAN
    *--Check if this MFG operation code works as operation.
    llWrkAsOpr = .F.
    lnCurSeqn  = 0

    lcSqlStatement = "SELECT * FROM CODES " + ;
      "WHERE cDefCode+ccode_no+crltfield+cfld_name = 'N'"+;
      "+'"+PADR(MfgCode,6)+"'+'Y'+'MFGCODE   ' AND CRLTD_NAM = 'LMFGOPR'"
    =lfOpenFox(lcSqlStatement,'CODES',lcTmpCode,"")

    SELECT(lcTmpCode)
    LOCATE

    IF FOUND()
      llWrkAsOpr = ALLT(EVALUATE(lcTmpCode+'.cRltd_vlu')) = "T"
      lcSqlStatement = "SELECT * FROM CODES " + ;
        "WHERE cDefCode+ccode_no+crltfield+cfld_name = 'N'"+;
        "+'"+PADR(EVALUATE(lcTmpCur+'.MfgCode'),6)+"'+'Y'+'MFGCODE   ' AND CRLTD_NAM = 'COPERSEQ'"
      =lfOpenFox(lcSqlStatement,'CODES',lcTmpCode,"")
      SELECT(lcTmpCode)
      LOCATE

      lnCurSeqn = IIF(FOUND(),INT(VAL(ALLT(EVALUATE(lcTmpCode+'.cRltd_vlu')))),0)
    ENDIF

    SELECT (lcTmpCur)
    IF !llWrkAsOpr
      LOOP
    ENDIF

    *-- Read last operation for sequence is last.
    IF lnMaxSeq <= lnCurSeqn
      *--Read last operation.
      lcLastMfgOr = MfgCode
      lnMaxSeq = lnCurSeqn
    ENDIF

  ENDSCAN

ELSE
  lcLastMfgOr = lcHdrLOpr
ENDIF

*--If not empty last operation , read receive lot.
lcRecvLot = '  '
IF !EMPTY(lcLastMfgOr)

  *-- cimtyp+ctktno+coprcode+trancd
  lcSqlStatement = "SELECT * FROM MFGOPRDT [INDEX=TKTOPTRN] "+;
    "WHERE cImTyp = '" + lcCstShtType +;
    "' AND cTktNo = '" + loFormSet.lcPoNo +;
    "' AND cOprCode = '" + lcLastMfgOr + "'"
  =lfOpenSql(lcSqlStatement,'MFGOPRDT',lcTmpCur, "","",.F.)

  SELECT (lcTmpCur)
  LOCATE
  lnNumLots = 0
  COUNT TO lnNumLots ;
    FOR cIMTyp+cTktNo+cOprCode+TranCd = ;
    lcCstShtType+loFormSet.lcPoNo+lcLastMfgOr+'1' AND ITEM = loFormSet.lcStyle ;
    AND nLotTotQty >0
  IF lnNumLots <> 0
    LOCATE FOR cIMTyp+cTktNo+cOprCode+TranCd = ;
      lcCstShtType+loFormSet.lcPoNo+lcLastMfgOr+'1' AND ITEM = loFormSet.lcStyle ;
      AND cDyelot = PADR(loFormSet.lcDyelot,10)
    DO CASE
    CASE lnNumLots = 1
      lcRecvLot = cLotNo
    CASE lnNumLots > 1
      *--Do you want to select a lot to receive to or distribute received quantity to all lots?,\<Select;\<Distribute
      IF gfModalGen('QRM42108B42011','DIALOG') = 1
        lcBrFields = [cLotNo    :H='Lot No.',]+;
          [cOprCode  :H='Operation',]+;
          [cContCode :H='Cont./Dept.',]+;
          [cContName :H='Name',]+;
          [dTranDate :H='Trans.Date',]+;
          [dueDate   :H='Due Date',]+;
          [nLotTotQty :H='Total']
        DIMENSION laTemp[1]
        laTemp = ''
        =ARIABROW([FOR Item=loFormSet.lcStyle;
                  .AND. cDyelot = PADR(loFormSet.lcDyelot,10)],'Lots',gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','cLotNo','laTemp')
        lcRecvLot=laTemp[1]
        IF !EMPTY(lcRecvLot)
          loFormSet.llSpecLot = .T.
          DIMENSION loFormSet.laLotArry[8]
          STORE 0 TO loFormSet.laLotArry
          SELECT (lcTmpCur)
          LOCATE FOR cIMTyp+cTktNo+cOprCode+TranCd = ;
            lcCstShtType+loFormSet.lcPoNo+lcLastMfgOr+'1' AND ;
            ITEM = loFormSet.lcStyle AND cDyelot = PADR(loFormSet.lcDyelot,10)

          SCAN FOR cIMTyp+cTktNo+cOprCode = lcCstShtType+loFormSet.lcPoNo+lcLastMfgOr+'1';
              AND ITEM = loFormSet.lcStyle AND cLotNo = lcRecvLot

            FOR I=1 TO 8
              Z=STR(I,1)
              loFormSet.laLotArry[I]=loFormSet.laLotArry[I]+(NLOTQTY&Z * IIF(TranCd='1',1,-1))
            ENDFOR
          ENDSCAN
        ENDIF
      ENDIF
    ENDCASE
  ELSE
    lcRecvLot = '01'
  ENDIF
ENDIF

SELECT (lnAlias)
*--Read last operation for color.
loFormSet.lcClrLstOp = lcLastMfgOr
lcRecvLot  = IIF(EMPTY(lcRecvLot),SPACE(2),lcRecvLot)
USE IN (lcTmpCur)
RETURN(lcRecvLot)

*!*************************************************************
*! Name      : lfvEditQty
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/4/2004
*! Purpose   : Edit line quantity.
*!*************************************************************
FUNCTION lfvEditQty
LPARAMETERS loFormSet

*C102172,4 AMH Check if coming for reciving by lot [Start]
*!*  IF ASCAN(laEvntTrig,PADR("ADOPTCAT",10)) <> 0 .AND. WEXIST('MFRECLOT')
*!*    lcTmpLine = lcTktSheet
*!*    ON KEY LABEL ESC     &lcHldEsc
*!*  ENDIF
*C102172,4 AMH [End]

PRIVATE lcRelCode, lcUOM_B, lcUOM_V, lnConf, lnRetVal
lcRelCode = EVALUATE(loFormSet.lcTmpLine+'.cUomCode')
lcUOM_B   = ''
lcUOM_V   = ''
lnConf    = 1

=gfGetUOMData(lcRelCode, lcUOM_B, @lcUOM_V, @lnConf, .F.)

PRIVATE lcWareCode,lcDyelot
lcWareCode = EVALUATE(loFormSet.lcTmpLine+'.cWareCode')
IF loFormSet.llWareHous AND EMPTY(lcWareCode)
  *-- Message : 'First, you must select location.'
  =gfModalGen('TRM42150B42001','DIALOG')
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
  *loFormSet.AriaForm1.cboLocations.SetFocus
  IF !loFormSet.llSilentMod
    loFormSet.AriaForm1.cboLocations.SETFOCUS
  ENDIF
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
  RETURN
ENDIF

SELECT (loFormSet.lcTmpLine)
SET FILTER TO
lcDyelot = Dyelot
SCATTER MEMVAR
lcLKey = cCarton+Po+STYLE+Dyelot+PADR(cWareCode,6)+STR(LINENO,6)

DIME laOrg[8],laSok[8],laDam1[8],laDam2[8],laCan[8],laBal[8],laAlo[8],laBud[8],laZero[9]
STORE 0 TO laOrg,laSok,laDam1,laDam2,laCan,laBal,laAlo,laZero,laBud

DIMENSION laOldOut[8]
STORE 0 TO laOldOut
DIMENSION laConst[8]    && Original qty.
STORE 0 TO laConst
DIMENSION laRemain[9]   &&-- which carry the last receive qty
STORE 0 TO laRemain

*--New Origenal and order quantity.
=SEEK('1'+lcLKey)
SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laOrg
SCATTER FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8 TO laAlo

IF loFormSet.lcInvType = "0001"
  FOR lnCntr = 1 TO 8
    laOrg[lnCntr] = INT(laOrg[lnCntr])
  ENDFOR
ENDIF
*-- If edit cost per line (Option menu)
STORE .F. TO llSpecLot

IF loFormSet.lcPType = 'H'
  *!*    lcBdgLine = 'N'+lcBatch+po+style+dyelot
  *!*    *--Budjet total ordered quantity.
  *!*    SELECT CTKTRCVL
ELSE

  lcBdgLine = cBusDocu+cStyType+PO+cInvType+STYLE+STR(LINENO,6)+TranCd
  *-- Case Shipment get the budget line
  *-- 'S' Receive by Shipment, 'U' Issue Inter Location P/O Shipment
  *-- 'C' Receive Inter Location P/O Shipment, 'F' Receive Material PO Shipment
  IF loFormSet.lcPType $ 'SUCF' AND !SEEK(lcBdgLine,loFormSet.lcPOSLN)
    lcSqlStatement  =  "SELECT  POSLN.*, POSHDR.cPriceCur, POSHDR.cDutyCur, POSHDR.Status "+;
      "FROM POSLN posln (INDEX=POSLN) inner join POSHDR (INDEX=POSHDR) "+;
      "ON POSHDR.cBusDocu = POSLN.cBusDocu AND POSHDR.cStyType = POSLN.cStyType "+;
      "AND  POSHDR.PO = POSLN.PO "+;
      "WHERE POSLN.cBusDocu = '" + cBusDocu +;
      "' AND POSLN.cStyType = '" + cStyType +;
      "' AND POSLN.PO = '" + PO +;
      "' AND POSLN.cInvType ='" + cInvType +;
      "' AND POSLN.Style ='" + STYLE +;
      "' AND [LineNo] = " + ALLTRIM(STR(LINENO)) +;
      " AND (TranCd = '" + TranCd + ;
      "' OR TranCd = '2' OR TranCd = '4' OR TranCd = '5')"

    =lfOpenSql(lcSqlStatement,'POSLN',loFormSet.lcTmpCurs, "","",.F.)
    SELECT (loFormSet.lcTmpCurs)
    LOCATE
    IF !EOF()
      SCAN
        SCATTER MEMVAR MEMO
        SELECT (loFormSet.lcPOSLN)
        INSERT INTO (loFormSet.lcPOSLN) FROM MEMVAR
      ENDSCAN
    ENDIF
  ENDIF
  *--Budjet total ordered quantity.
  SELECT (loFormSet.lcPOSLN)
ENDIF
=SEEK(lcBdgLine)

SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laBud
=ACOPY(laBud,laConst)

*--Get last receiving for inter location Issue and receive.
DO CASE

  *-- [O] Receive Inter-Location P/o, [N] Issue Inter-Location P/o
  *-- [A] Issue Adornment order, [E] Receive Adornment order
  *-- [S] Receive by Shipment, [L] Receive Inter Location P/O Batch
  *-- [C] Receive Inter Location P/O Shipment
  *-- [U] Issue Inter Location P/O Shipment
  *B130601,1 KHM 01/25/2006 Add the receive by material shipment [Stasrt]
  *CASE loFormSet.lcPType $ 'ONAESLCU'
CASE loFormSet.lcPType $ 'ONAESLCUF'
  *B130601,1 KHM 01/25/2006 [End]
  lcToseekLn = cBusDocu+cStyType+PO+cInvType+STYLE+STR(LINENO,6)
  SCAN REST WHILE cBusDocu+cStyType+PO+cInvType+STYLE+STR(LINENO,6)=lcToseekLn FOR Trancd $ '2456'
    FOR lnI = 1 TO 8
      lcI=STR(lnI,1)
      laRemain[lnI] = laRemain[lnI] + IIF(loFormSet.lcPType $ 'ONCU' AND TranCd = '6',0,EVALUATE('Qty'+lcI))
    ENDFOR
  ENDSCAN
  laRemain[9] = laRemain[1]+laRemain[2]+laRemain[3]+laRemain[4]+laRemain[5]+laRemain[6]+laRemain[7]+laRemain[8]
  =SEEK(lcBdgLine)

  *-- [I] Receive P/O, [M] Receive C/T
CASE loFormSet.lcPType = 'I'
  IF llSpecLot
    *!*        FOR lnI=1 to 8
    *!*          laRemain[lnI] = laLotArry[lnI] - laOrg[lnI]
    *!*          laRemain[9] = laRemain[9] + laRemain[lnI]
    *!*        ENDFOR
  ELSE
    lcToseekLn = cBusDocu+cStyType+PO+cInvType+STYLE+STR(LINENO,6)
    SCAN WHILE cBusDocu+cStyType+PO+cInvType+STYLE+STR(LINENO,6)=lcToseekLn FOR Trancd $ '245'
      FOR lnI=1 TO 8
        lcI=STR(lnI,1)
        laRemain[lnI] = laRemain[lnI] + IIF(loFormSet.lcPType $ 'ON' AND TranCd = '6',0,EVALUATE('Qty'+lcI))
      ENDFOR
    ENDSCAN
    laRemain[9] = laRemain[1]+laRemain[2]+laRemain[3]+laRemain[4]+laRemain[5]+laRemain[6]+laRemain[7]+laRemain[8]
    =SEEK(lcBdgLine)
  ENDIF

OTHERWISE
  FOR lnI=1 TO 8
    IF llSpecLot
      *!*          laRemain[lnI] = laLotArry[lnI] - laOrg[lnI]
    ELSE
      laRemain[lnI] = laConst[lnI] - laOrg[lnI]
    ENDIF
    laRemain[9] = laRemain[9] + laRemain[lnI]
  ENDFOR
ENDCASE

SELECT (loFormSet.lcTmpLine)
*! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
*lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE='S"+SCALE+"'"
*! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
*lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=[S"+SCALE+"]"
lcScaleValSel = "S"+SCALE
lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=?m.lcScaleValSel"
*! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
*! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][END]
=lfOpenFox(lcSqlStatement,'SCALE','SCALE',"")
SELECT (loFormSet.lcTmpLine)

STORE '' TO lcRetSty1,lcRetSty2,lcRetSHd,lcRetSHd1,lcRetSHd2,lcRetSHd3,lcRetSHd4
lcMStyQlty = cStyGrade
lcSndGrd   = "2"
lcTrdGrd   = "3"
DO CASE
CASE lcMStyQlty='1'
  lcRetSHd = LANG_POSTREC_1STQUALITY
  *!*      IF lcPType<>'A' .OR. (ASCAN(laEvntTrig,PADR("RCVADORD",10)) <> 0)
  IF loFormSet.lcPType<>'A'
    lcRetSHd1 = LANG_POSTREC_2NDQUALITY
    lcRetSHd2 = LANG_POSTREC_DAMAGED
    lcRetSHd3 = LANG_POSTREC_SECONDQUALITY
    lcRetSHd4 = LANG_POSTREC_DAMAGED
  ENDIF
CASE lcMStyQlty='2'
  lcRetSHd = LANG_POSTREC_2NDQUALITY
  IF loFormSet.lcPType<>'A'
    lcRetSHd1 = LANG_POSTREC_1STQUALITY
    lcRetSHd2 = LANG_POSTREC_DAMAGED
    lcRetSHd3 = LANG_POSTREC_FIRSTQUALITY
    lcRetSHd4 = LANG_POSTREC_DAMAGED
  ENDIF
  lcSndGrd = "1"
  lcTrdGrd = "3"
CASE lcMStyQlty='3'
  lcRetSHd = LANG_POSTREC_DAMAGED
  IF loFormSet.lcPType<>'A'
    lcRetSHd1 = LANG_POSTREC_1STQUALITY
    lcRetSHd2 = LANG_POSTREC_2NDQUALITY
    lcRetSHd3 = LANG_POSTREC_FIRSTQUALITY
    lcRetSHd4 = LANG_POSTREC_SECONDQUALITY
  ENDIF
  lcSndGrd = "1"
  lcTrdGrd = "2"
ENDCASE

lnOldStk = TotStk
lnOldDam = TotDam
lnOldCan = TotCan

*--1) Get Stock quantity.
IF SEEK('2'+lcLKey)
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laSok
  *--Initialize the landed cost.
ENDIF

*--2) Get Damage quantity.
STORE .F. TO llRSt1Stat,llRSt2Stat
IF SEEK('4'+lcLKey)
  SCAN REST WHILE TranCd+cCarton+Po+STYLE+Dyelot+PADR(cWareCode,6)+STR(LINENO,6) = '4'+lcLKey
    IF cStyGrade = lcSndGrd
      SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laDam1
      lcRetSty1  = cRetSty
      llRSt1Stat = .T.
    ELSE
      SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laDam2
      lcRetSty2  = cRetSty
      llRSt2Stat = .T.
    ENDIF
  ENDSCAN
ENDIF

*--3) Get Cancel quantity.
IF SEEK('5'+lcLKey)
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 TO laCan
ENDIF

FOR I = 1 TO 8
  laBal[I] = MAX(laOrg[I]-(laSok[I]+laDam1[I]+laDam2[I]+laCan[I]),0)
ENDFOR

llSkMode = .T.
llOtMode = .T.

*--Cannot update quantity case of batch (view only).
IF loFormSet.lcPType $ 'BTLH' OR (loFormSet.llPOSale AND loFormSet.lcPType $ 'OE') OR ;
    (loFormSet.lcPType = 'R' AND !EMPTY(EVALUATE(loFormSet.lcPOSHDR+'.CPONO')))
  llSkMode   = .F.
  llOtMode   = .F.
  llRSt1Stat = .F.
  llRSt2Stat = .F.
ENDIF

*--Cannot edit the other and cancel quantity case of issue inter Location P/o.
IF loFormSet.lcPType $ 'NAU'
  llOtMode   = .F.
  llRSt1Stat = .F.
  llRSt2Stat = .F.
ENDIF

=SEEK('1'+lcLKey)
LOCAL lnCostVal1,lnCostVal2,lnCostVal3,lnCostVal4,lnCostVal5,lnCostVal6,lnCostVal7,;
  lnEqCost1,lnEqCost2,lnEqCost3,lnEqCost4,lnEqCost5,lnEqCost6,lnEqCost7
STORE 0 TO lnCostVal1,lnCostVal2,lnCostVal3,lnCostVal4,lnCostVal5,lnCostVal6,lnCostVal7,;
  lnEqCost1,lnEqCost2,lnEqCost3,lnEqCost4,lnEqCost5,lnEqCost6,lnEqCost7
IF loFormSet.llEditLCst
  lnCostVal1 = nFLanCost1
  lnCostVal2 = nFLanCost2
  lnCostVal3 = nFLanCost3
  lnCostVal4 = nFLanCost4
  lnCostVal5 = nFLanCost5
  lnCostVal6 = nFLanCost6
  lnCostVal7 = nFLanCost7
  lnEqCost1  = nLan_Cost1
  lnEqCost2  = nLan_Cost2
  lnEqCost3  = nLan_Cost3
  lnEqCost4  = nLan_Cost4
  lnEqCost5  = nLan_Cost5
  lnEqCost6  = nLan_Cost6
  lnEqCost7  = nLan_Cost7
ENDIF

*calculate old values of Out quantities from this line.
FOR lnI = 1 TO 8
  laOldOut[lnI] = laSok[lnI]+laDam1[lnI]+laDam2[lnI]+laCan[lnI]
ENDFOR

*--Call line quantity screen.
PRIVATE loParentForm
loParentForm = loFormSet
*T20071102.0018(C200876) TMI [Start] if bin location isntalled run for the line receive another screen
IF ASCAN(loFormSet.laEvntTrig,PADR('DLRCVQTY',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0 ;
    .AND. loFormSet.mDoTrigger(PADR('ISUSEBIN',10))
  loFormSet.mDoTrigger(PADR('DLRCVQTY',10))
ELSE
  *T20071102.0018(C200876) TMI [End  ]
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  *DO FORM (oAriaApplication.ScreenHome+'MFRCVQ.SCX')
  *! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[Start]
  *DO FORM (oAriaApplication.ScreenHome+'MFRCVQ.SCX') WITH loFormSet.lcPType $ 'IOM' AND !EMPTY(loFormSet.AriaForm1.CntBatch.kbBatchNo.KeyTextBox.Value) AND (loFormSet.AriaForm1.CntBatch.cboBatchStatus.Value $ 'XP' OR (loFormSet.AriaForm1.CntBatch.cboBatchStatus.Value ='A' AND !loFormSet.llApproveBatch))
  loCallingForm  = loFormSet
  =gfCallForm('MFRCVQ',.F.,;
    "loCallingForm.lcPType $ 'IOM' AND !EMPTY(loCallingForm.AriaForm1.CntBatch.kbBatchNo.KeyTextBox.Value) AND "+;
    " (loCallingForm.AriaForm1.CntBatch.cboBatchStatus.Value $ 'XP' OR (loCallingForm.AriaForm1.CntBatch.cboBatchStatus.Value ='A' AND !loCallingForm.llApproveBatch))")
  *! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[End]
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  *T20071102.0018(C200876) TMI [Start]
ENDIF
*T20071102.0018(C200876) TMI [End  ]


SELECT (loFormSet.lcTmpLine)
IF loFormSet.lcPType $ 'BT'
  =SEEK('1'+lcLKey)
  RETURN
ENDIF
SCATTER MEMVAR

*--Get Totals.
lnTStk  = laSok[1] +laSok[2] +laSok[3] +laSok[4] +laSok[5] +laSok[6] +laSok[7] +laSok[8]
lnTCan  = laCan[1] +laCan[2] +laCan[3] +laCan[4] +laCan[5] +laCan[6] +laCan[7] +laCan[8]
lnTDam1 = laDam1[1]+laDam1[2]+laDam1[3]+laDam1[4]+laDam1[5]+laDam1[6]+laDam1[7]+laDam1[8]
lnTDam2 = laDam2[1]+laDam2[2]+laDam2[3]+laDam2[4]+laDam2[5]+laDam2[6]+laDam2[7]+laDam2[8]
lnTDam  = lnTDam1  +lnTDam2

*B126833,1 WAM 04/03/2005 Add new button to view/Edit landed cost
*!*	*--Edit Landed cost.
*!*	IF loFormSet.llEditLCst AND ( lnTStk+lnTDam+lnTCan <> 0 )
*!*	  =lfvLCost(loFormSet)
*!*	  SELECT (loFormSet.lcTmpLine)
*!*	  lnCostVal1 = nFLanCost1
*!*	  lnCostVal2 = nFLanCost2
*!*	  lnCostVal3 = nFLanCost3
*!*	  lnCostVal4 = nFLanCost4
*!*	  lnCostVal5 = nFLanCost5
*!*	  lnCostVal6 = nFLanCost6
*!*	  lnCostVal7 = nFLanCost7
*!*	  lnEqCost1  = nLan_Cost1
*!*	  lnEqCost2  = nLan_Cost2
*!*	  lnEqCost3  = nLan_Cost3
*!*	  lnEqCost4  = nLan_Cost4
*!*	  lnEqCost5  = nLan_Cost5
*!*	  lnEqCost6  = nLan_Cost6
*!*	  lnEqCost7  = nLan_Cost7
*!*	ENDIF
*B126833,1 WAM 04/03/2005 (End)

*--1) Update stock quantity.
IF SEEK('2'+lcLKey)
  IF lnTStk<>0
    GATHER FROM laSok FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    REPLACE TotQty WITH lnTStk
    IF loFormSet.llEditLCst
      REPLACE nLan_Cost1 WITH lnEqCost1, nLan_Cost2 WITH lnEqCost2, nLan_Cost3 WITH lnEqCost3,;
        nLan_Cost4 WITH lnEqCost4, nLan_Cost5 WITH lnEqCost5, nLan_Cost6 WITH lnEqCost6,;
        nLan_Cost7 WITH lnEqCost7,;
        nFLanCost1 WITH lnCostVal1, nFLanCost2 WITH lnCostVal2, nFLanCost3 WITH lnCostVal3,;
        nFLanCost4 WITH lnCostVal4, nFLanCost5 WITH lnCostVal5,;
        nFLanCost6 WITH lnCostVal6, nFLanCost7 WITH lnCostVal7
    ENDIF
  ELSE
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
    IF IIF(loFormSet.lcPType $ 'IMO',EMPTY(loFormSet.AriaForm1.CntBatch.KBBatchNo.KeyTextBox.VALUE),.T.)
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
      BLANK
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
    ENDIF
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
    DELETE
  ENDIF
ELSE
  IF lnTStk<>0
    APPEND BLANK
    GATHER MEMVAR
    REPLACE Trancd    WITH '2',;
      cStyGrade WITH lcMStyQlty


    *B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[Start]
    REPLACE CINVTYPE WITH loFormSet.lcInvType
    *B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[End]


    GATHER FROM laSok  FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    GATHER FROM laZero FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
    REPLACE TotQty WITH lnTStk
    IF loFormSet.llEditLCst
      REPLACE nLan_Cost1 WITH lnEqCost1, nLan_Cost2 WITH lnEqCost2, nLan_Cost3 WITH lnEqCost3,;
        nLan_Cost4 WITH lnEqCost4, nLan_Cost5 WITH lnEqCost5, nLan_Cost6 WITH lnEqCost6,;
        nLan_Cost7 WITH lnEqCost7,;
        nFLanCost1 WITH lnCostVal1, nFLanCost2 WITH lnCostVal2, nFLanCost3 WITH lnCostVal3,;
        nFLanCost4 WITH lnCostVal4, nFLanCost5 WITH lnCostVal5,;
        nFLanCost6 WITH lnCostVal6, nFLanCost7 WITH lnCostVal7
    ENDIF
  ENDIF
ENDIF

*--2) Update Damaged quantity.
=SEEK('4'+lcLKey)
LOCATE REST WHILE Trancd+cCarton+Po+STYLE+Dyelot+PADR(cWareCode,6)+STR(LINENO,6)='4'+lcLKey ;
  FOR cStyGrade=lcSndGrd
IF FOUND()
  IF lnTDam1=0
    BLANK
    DELETE
  ELSE
    GATHER FROM laDam1 FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    REPLACE TotQty  WITH lnTDam1,;
      cRetSty WITH lcRetSty1
    IF loFormSet.llEditLCst
      REPLACE nLan_Cost1 WITH lnEqCost1, nLan_Cost2 WITH lnEqCost2, nLan_Cost3 WITH lnEqCost3,;
        nLan_Cost4 WITH lnEqCost4, nLan_Cost5 WITH lnEqCost5, nLan_Cost6 WITH lnEqCost6,;
        nLan_Cost7 WITH lnEqCost7,;
        nFLanCost1 WITH lnCostVal1, nFLanCost2 WITH lnCostVal2, nFLanCost3 WITH lnCostVal3,;
        nFLanCost4 WITH lnCostVal4, nFLanCost5 WITH lnCostVal5,;
        nFLanCost6 WITH lnCostVal6, nFLanCost7 WITH lnCostVal7
    ENDIF
  ENDIF
ELSE
  IF lnTDam1<>0
    APPEND BLANK
    GATHER MEMVAR
    REPLACE Trancd    WITH '4',;
      cStyGrade WITH lcSndGrd,;
      cRetSty   WITH lcRetSty1,;
      TotQty    WITH lnTDam1
    GATHER FROM laDam1 FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    GATHER FROM laZero FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
    IF loFormSet.llEditLCst
      REPLACE nLan_Cost1 WITH lnEqCost1, nLan_Cost2 WITH lnEqCost2, nLan_Cost3 WITH lnEqCost3,;
        nLan_Cost4 WITH lnEqCost4, nLan_Cost5 WITH lnEqCost5, nLan_Cost6 WITH lnEqCost6,;
        nLan_Cost7 WITH lnEqCost7,;
        nFLanCost1 WITH lnCostVal1, nFLanCost2 WITH lnCostVal2, nFLanCost3 WITH lnCostVal3,;
        nFLanCost4 WITH lnCostVal4, nFLanCost5 WITH lnCostVal5,;
        nFLanCost6 WITH lnCostVal6, nFLanCost7 WITH lnCostVal7
    ENDIF
  ENDIF
ENDIF

=SEEK('4'+lcLKey)
LOCATE REST WHILE Trancd+cCarton+Po+STYLE+Dyelot+PADR(cWareCode,6)+STR(LINENO,6)='4'+lcLKey ;
  FOR cStyGrade=lcTrdGrd

IF FOUND()
  IF lnTDam2=0
    BLANK
    DELETE
  ELSE
    GATHER FROM laDam2 FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    REPLACE TotQty  WITH lnTDam2,;
      cRetSty WITH lcRetSty2
    IF loFormSet.llEditLCst
      REPLACE nLan_Cost1 WITH lnEqCost1, nLan_Cost2 WITH lnEqCost2, nLan_Cost3 WITH lnEqCost3,;
        nLan_Cost4 WITH lnEqCost4, nLan_Cost5 WITH lnEqCost5, nLan_Cost6 WITH lnEqCost6,;
        nLan_Cost7 WITH lnEqCost7,;
        nFLanCost1 WITH lnCostVal1, nFLanCost2 WITH lnCostVal2, nFLanCost3 WITH lnCostVal3,;
        nFLanCost4 WITH lnCostVal4, nFLanCost5 WITH lnCostVal5,;
        nFLanCost6 WITH lnCostVal6, nFLanCost7 WITH lnCostVal7
    ENDIF
  ENDIF
ELSE
  IF lnTDam2<>0
    APPEND BLANK
    GATHER MEMVAR
    REPLACE Trancd    WITH '4',;
      cStyGrade WITH lcTrdGrd,;
      cRetSty   WITH lcRetSty2,;
      TotQty    WITH lnTDam2
    GATHER FROM laDam2 FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    GATHER FROM laZero FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
    IF loFormSet.llEditLCst
      REPLACE nLan_Cost1 WITH lnEqCost1, nLan_Cost2 WITH lnEqCost2, nLan_Cost3 WITH lnEqCost3,;
        nLan_Cost4 WITH lnEqCost4, nLan_Cost5 WITH lnEqCost5, nLan_Cost6 WITH lnEqCost6,;
        nLan_Cost7 WITH lnEqCost7,;
        nFLanCost1 WITH lnCostVal1, nFLanCost2 WITH lnCostVal2, nFLanCost3 WITH lnCostVal3,;
        nFLanCost4 WITH lnCostVal4, nFLanCost5 WITH lnCostVal5,;
        nFLanCost6 WITH lnCostVal6, nFLanCost7 WITH lnCostVal7
    ENDIF
  ENDIF
ENDIF

*--3) Update cancel quantity.
IF SEEK('5'+lcLKey)
  IF lnTCan<>0
    GATHER FROM laCan FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    REPLACE TotQty WITH lnTCan
    IF loFormSet.llEditLCst
      REPLACE nLan_Cost1 WITH lnEqCost1, nLan_Cost2 WITH lnEqCost2, nLan_Cost3 WITH lnEqCost3,;
        nLan_Cost4 WITH lnEqCost4, nLan_Cost5 WITH lnEqCost5, nLan_Cost6 WITH lnEqCost6,;
        nLan_Cost7 WITH lnEqCost7,;
        nFLanCost1 WITH lnCostVal1, nFLanCost2 WITH lnCostVal2, nFLanCost3 WITH lnCostVal3,;
        nFLanCost4 WITH lnCostVal4, nFLanCost5 WITH lnCostVal5,;
        nFLanCost6 WITH lnCostVal6, nFLanCost7 WITH lnCostVal7
    ENDIF
  ELSE
    BLANK
    DELETE
  ENDIF
ELSE
  IF lnTCan<>0
    APPEND BLANK
    GATHER MEMVAR
    REPLACE Trancd WITH '5',;
      TotQty WITH lnTCan
    GATHER FROM laCan  FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    GATHER FROM laZero FIELDS Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
    IF loFormSet.llEditLCst
      REPLACE nLan_Cost1 WITH lnEqCost1, nLan_Cost2 WITH lnEqCost2, nLan_Cost3 WITH lnEqCost3,;
        nLan_Cost4 WITH lnEqCost4, nLan_Cost5 WITH lnEqCost5, nLan_Cost6 WITH lnEqCost6,;
        nLan_Cost7 WITH lnEqCost7,;
        nFLanCost1 WITH lnCostVal1, nFLanCost2 WITH lnCostVal2, nFLanCost3 WITH lnCostVal3,;
        nFLanCost4 WITH lnCostVal4, nFLanCost5 WITH lnCostVal5,;
        nFLanCost6 WITH lnCostVal6, nFLanCost7 WITH lnCostVal7
    ENDIF
  ENDIF
ENDIF

SELECT (loFormSet.lcTmpLine)
IF SEEK('1'+lcLKey)
  REPLACE TotStk WITH lnTStk,;
    TotDam WITH lnTDam,;
    TotCan WITH lnTCan,;
    TotBal WITH laBal[1]+laBal[2]+laBal[3]+laBal[4]+laBal[5]+laBal[6]+laBal[7]+laBal[8]
ENDIF

loFormSet.lnTotStk = loFormSet.lnTotStk+(TotStk-lnOldStk)
loFormSet.lnTotDam = loFormSet.lnTotDam+(TotDam-lnOldDam)
loFormSet.lnTotCan = loFormSet.lnTotCan+(TotCan-lnOldCan)

IF (loFormSet.lcPType $ 'ISMD')
  DIMENSION laOut[8]
  STORE 0 TO lnTotOut,laOut
  FOR lnI = 1 TO 8
    Z = STR(lnI,1)
    laOut[lnI] = laSok[lnI]+laDam1[lnI]+laDam2[lnI]+laCan[lnI]
    lnTotOut   = lnTotOut + laOut[lnI]
  ENDFOR
  *-- Calculate Out Quantity in this Line.
  lcCurLine =  cCarton+Po+STYLE+STR(LINENO,6)
  *-- Subtract out quantity from the same lines with another dyelots.
  lcScanVar = [Trancd+cCarton+Po+Style+STR(LineNo,6)]
  SCAN FOR EVALUATE(lcScanVar) = '1' + lcCurLine AND ;
      DYELOT+cWareCode # lcDyelot+lcWareCode
    FOR lnI = 1 TO 8
      lcZ = STR(lnI,1)
      REPLACE ('QTY'+lcZ) WITH EVALUATE('QTY'+lcZ) + laOldOut[lnI] - laOut[lnI] ,;
        TOTQTY      WITH TOTQTY  + laOldOut[lnI] - laOut[lnI]
    ENDFOR
    REPLACE TOTBAL WITH TOTQTY-TOTSTK-TOTDAM-TOTCAN
  ENDSCAN
  =SEEK('1'+lcLKey)
ENDIF

*C102172,4 AMH Check if coming for reciving by lot [Start]
*!*  IF ASCAN(laEvntTrig,PADR("ADOPTCAT",10)) <> 0 .AND. WEXIST('MFRECLOT')
*!*    lcTmpLine = lcTktShee1
*!*    ON KEY LABEL ESC DO lfvReturn
*!*  ENDIF
*C102172,4 AMH [End]

*-- Update shipment header file
*B128070,1 KHM 05/19/2005 Commented out. There is no need to update the table here [Begin]
*!*	IF loFormSet.lcPType $ 'SCF' AND USED(loFormSet.lcMastShp)
*!*	  SELECT (loFormSet.lcMastShp)
*!*	  REPLACE Recv_Stk WITH loFormSet.lnTotStk ,;
*!*	  		  Recv_Dam WITH loFormSet.lnTotDam ,;
*!*	  		  Recv_Can WITH loFormSet.lnTotCan
*!*	ENDIF
*B128070,1 KHM 05/19/2005 [End]

*T20071102.0018(C200876) TMI [Start] Update bin location fields for 2nd/ damaged quality fields
*MMT
*IF ASCAN(laEvntTrig,'UPBNLOC') <> 0
IF ASCAN(loFormSet.laEvntTrig,'UPBNLOC') <> 0
  *MMT
  lcTmpLn = loFormSet.lcTmpLine
  =gfDoTriger('POSTREC','UPBNLOC')
ENDIF
*T20071102.0018(C200876) TMI [End  ]

SELECT (loFormSet.lcTmpLine)
SET FILTER TO TranCd = '1'
loFormSet.REFRESH
RETURN

*!*************************************************************
*! Name      : lfMfEdAloInit
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/04/2004
*! Purpose   : function called from the init event of form MFRCVQ.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfMfRcvQInit
LPARAMETERS loFormSet

loFormSet.ariaForm1.CAPTION = LANG_POSTREC_MFRCVQ

=gfOpenFile(oAriaApplication.DataDir+'STYLE','STYLE','SH')
LOCAL lcLineQty,lnFldWidth,lnFldPer
lcLineQty  = gfTempName()
loFormSet.lcLineQty = lcLineQty
lnFldWidth = IIF(loParentForm.lcInvType='0001',6,11)
lnFldPer   = IIF(loParentForm.lcInvType='0001',0,3)

LOCAL lnSetDec
lnSetDec = SET("Decimals")
SET DECIMALS TO lnfldper
IF lnFldPer = 0
  loFormSet.lnFldHigh = VAL(REPLICATE('9',lnFldWidth))
ELSE
  loFormSet.lnFldHigh = VAL(STUFF(REPLICATE('9',lnFldWidth),lnFldWidth-lnFldPer,1,'.'))
ENDIF
SET DECIMALS TO lnSetDec

DIMENSION laFileStru[12,18]
laFileStru[01,1] = 'cType'
laFileStru[01,2] = 'C'
laFileStru[01,3] = 1
laFileStru[01,4] = 0

laFileStru[02,1] = 'cDesc'
laFileStru[02,2] = 'C'
laFileStru[02,3] = 11
laFileStru[02,4] = 0

laFileStru[03,1] = 'nQty1'
laFileStru[03,2] = 'N'
laFileStru[03,3] = lnFldWidth
laFileStru[03,4] = lnFldPer

laFileStru[04,1] = 'nQty2'
laFileStru[04,2] = 'N'
laFileStru[04,3] = lnFldWidth
laFileStru[04,4] = lnFldPer

laFileStru[05,1] = 'nQty3'
laFileStru[05,2] = 'N'
laFileStru[05,3] = lnFldWidth
laFileStru[05,4] = lnFldPer

laFileStru[06,1] = 'nQty4'
laFileStru[06,2] = 'N'
laFileStru[06,3] = lnFldWidth
laFileStru[06,4] = lnFldPer

laFileStru[07,1] = 'nQty5'
laFileStru[07,2] = 'N'
laFileStru[07,3] = lnFldWidth
laFileStru[07,4] = lnFldPer

laFileStru[08,1] = 'nQty6'
laFileStru[08,2] = 'N'
laFileStru[08,3] = lnFldWidth
laFileStru[08,4] = lnFldPer

laFileStru[09,1] = 'nQty7'
laFileStru[09,2] = 'N'
laFileStru[09,3] = lnFldWidth
laFileStru[09,4] = lnFldPer

laFileStru[10,1] = 'nQty8'
laFileStru[10,2] = 'N'
laFileStru[10,3] = lnFldWidth
laFileStru[10,4] = lnFldPer

laFileStru[11,1] = 'nTotQty'
laFileStru[11,2] = 'N'
laFileStru[11,3] = lnFldWidth + 1
laFileStru[11,4] = lnFldPer

laFileStru[12,1] = 'nUseQty'
laFileStru[12,2] = 'N'
laFileStru[12,3] = 12
laFileStru[12,4] = 3

FOR lnI = 7 TO 16
  FOR lnJ = 1 TO 12
    laFileStru[lnJ,lnI] = ''
  ENDFOR
ENDFOR
FOR lnJ = 1 TO 12
  STORE 0 TO laFileStru[lnJ,17],laFileStru[lnJ,18]
ENDFOR

=gfCrtTmp(lcLineQty,@laFileStru,'cType',lcLineQty)

SELECT (lcLineQty)
APPEND BLANK
REPLACE cType   WITH '1',;
  cDesc   WITH LANG_POSTREC_BUDGET,;
  nQty1   WITH MAX(laConst[1],0),;
  nQty2   WITH MAX(laConst[2],0),;
  nQty3   WITH MAX(laConst[3],0),;
  nQty4   WITH MAX(laConst[4],0),;
  nQty5   WITH MAX(laConst[5],0),;
  nQty6   WITH MAX(laConst[6],0),;
  nQty7   WITH MAX(laConst[7],0),;
  nQty8   WITH MAX(laConst[8],0),;
  nTotQty WITH nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8,;
  nUseQty WITH nTotQty * lnConf

APPEND BLANK
REPLACE cType   WITH '2',;
  cDesc   WITH LANG_POSTREC_PRVRCPT,;
  nQty1   WITH MAX(laRemain[1],0),;
  nQty2   WITH MAX(laRemain[2],0),;
  nQty3   WITH MAX(laRemain[3],0),;
  nQty4   WITH MAX(laRemain[4],0),;
  nQty5   WITH MAX(laRemain[5],0),;
  nQty6   WITH MAX(laRemain[6],0),;
  nQty7   WITH MAX(laRemain[7],0),;
  nQty8   WITH MAX(laRemain[8],0),;
  nTotQty WITH MAX(laRemain[9],0),;
  nUseQty WITH nTotQty * lnConf

APPEND BLANK
REPLACE cType   WITH '3',;
  cDesc   WITH LANG_POSTREC_OpenBrowTitl,;
  nQty1   WITH MAX(laOrg[1],0),;
  nQty2   WITH MAX(laOrg[2],0),;
  nQty3   WITH MAX(laOrg[3],0),;
  nQty4   WITH MAX(laOrg[4],0),;
  nQty5   WITH MAX(laOrg[5],0),;
  nQty6   WITH MAX(laOrg[6],0),;
  nQty7   WITH MAX(laOrg[7],0),;
  nQty8   WITH MAX(laOrg[8],0),;
  nTotQty WITH nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8,;
  nUseQty WITH nTotQty * lnConf

APPEND BLANK
REPLACE cType   WITH '4',;
  cDesc   WITH lcRetSHd,;
  nQty1   WITH laSok[1],;
  nQty2   WITH laSok[2],;
  nQty3   WITH laSok[3],;
  nQty4   WITH laSok[4],;
  nQty5   WITH laSok[5],;
  nQty6   WITH laSok[6],;
  nQty7   WITH laSok[7],;
  nQty8   WITH laSok[8],;
  nTotQty WITH nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8,;
  nUseQty WITH nTotQty * lnConf

APPEND BLANK
REPLACE cType   WITH '5',;
  cDesc   WITH lcRetSHd1,;
  nQty1   WITH laDam1[1],;
  nQty2   WITH laDam1[2],;
  nQty3   WITH laDam1[3],;
  nQty4   WITH laDam1[4],;
  nQty5   WITH laDam1[5],;
  nQty6   WITH laDam1[6],;
  nQty7   WITH laDam1[7],;
  nQty8   WITH laDam1[8],;
  nTotQty WITH nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8,;
  nUseQty WITH nTotQty * lnConf

APPEND BLANK
REPLACE cType   WITH '6',;
  cDesc   WITH lcRetSHd2,;
  nQty1   WITH laDam2[1],;
  nQty2   WITH laDam2[2],;
  nQty3   WITH laDam2[3],;
  nQty4   WITH laDam2[4],;
  nQty5   WITH laDam2[5],;
  nQty6   WITH laDam2[6],;
  nQty7   WITH laDam2[7],;
  nQty8   WITH laDam2[8],;
  nTotQty WITH nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8,;
  nUseQty WITH nTotQty * lnConf

APPEND BLANK
REPLACE cType   WITH '7',;
  cDesc   WITH LANG_POSTREC_CANCEL,;
  nQty1   WITH laCan[1],;
  nQty2   WITH laCan[2],;
  nQty3   WITH laCan[3],;
  nQty4   WITH laCan[4],;
  nQty5   WITH laCan[5],;
  nQty6   WITH laCan[6],;
  nQty7   WITH laCan[7],;
  nQty8   WITH laCan[8],;
  nTotQty WITH nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8,;
  nUseQty WITH nTotQty * lnConf

APPEND BLANK
REPLACE cType   WITH '8',;
  cDesc   WITH LANG_POSTREC_BALANCE,;
  nQty1   WITH MAX(laBal[1],0),;
  nQty2   WITH MAX(laBal[2],0),;
  nQty3   WITH MAX(laBal[3],0),;
  nQty4   WITH MAX(laBal[4],0),;
  nQty5   WITH MAX(laBal[5],0),;
  nQty6   WITH MAX(laBal[6],0),;
  nQty7   WITH MAX(laBal[7],0),;
  nQty8   WITH MAX(laBal[8],0),;
  nTotQty WITH nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8,;
  nUseQty WITH nTotQty * lnConf
LOCATE

WITH loFormSet.AriaForm1
  WITH .grdLineQty
    .RECORDSOURCE = lcLineQty
    .Column1.CONTROLSOURCE   = lcLineQty+".cDesc"

    .Column2.CONTROLSOURCE   = lcLineQty+".nQty1"
    .Column2.Header1.CAPTION = SCALE.SZ1
    .Column2.WIDTH           = lnFldWidth * 10
    .column2.VISIBLE         = (SCALE.CNT>0)

    .Column3.CONTROLSOURCE   = lcLineQty+".nQty2"
    .Column3.Header1.CAPTION = SCALE.SZ2
    .Column3.WIDTH           = lnFldWidth * 10
    .column3.VISIBLE         = (SCALE.CNT>1)

    .Column4.CONTROLSOURCE   = lcLineQty+".nQty3"
    .Column4.Header1.CAPTION = SCALE.SZ3
    .Column4.WIDTH           = lnFldWidth * 10
    .column4.VISIBLE         = (SCALE.CNT>2)

    .Column5.CONTROLSOURCE   = lcLineQty+".nQty4"
    .Column5.Header1.CAPTION = SCALE.SZ4
    .Column5.WIDTH           = lnFldWidth * 10
    .column5.VISIBLE         = (SCALE.CNT>3)

    .Column6.CONTROLSOURCE   = lcLineQty+".nQty5"
    .Column6.Header1.CAPTION = SCALE.SZ5
    .Column6.WIDTH           = lnFldWidth * 10
    .column6.VISIBLE         = (SCALE.CNT>4)

    .Column7.CONTROLSOURCE   = lcLineQty+".nQty6"
    .Column7.Header1.CAPTION = SCALE.SZ6
    .Column7.WIDTH           = lnFldWidth * 10
    .column7.VISIBLE         = (SCALE.CNT>5)

    .Column8.CONTROLSOURCE   = lcLineQty+".nQty7"
    .Column8.Header1.CAPTION = SCALE.SZ7
    .Column8.WIDTH           = lnFldWidth * 10
    .column8.VISIBLE         = (SCALE.CNT>6)

    .Column9.CONTROLSOURCE   = lcLineQty+".nQty8"
    .Column9.Header1.CAPTION = SCALE.SZ8
    .Column9.WIDTH           = lnFldWidth * 10
    .column9.VISIBLE         = (SCALE.CNT>7)

    .Column10.CONTROLSOURCE  = lcLineQty+".nTotQty"
    .Column10.WIDTH          = (lnFldWidth + 1) * 10

    .Column11.CONTROLSOURCE  = lcLineQty+".nUseQty"
    .Column11.Header1.CAPTION= .Column11.Header1.CAPTION+lcUOM_V
    .Column11.WIDTH          = 120
    .column11.VISIBLE        = (loParentForm.lcInvType='0002')

    *--Set dyenamic colors.
    .SETALL("Dynamicbackcolor", "", "Column")
    *.SetAll("Dynamicbackcolor","IIF("+lcLineQty+".cType='1',12320767,IIF("+lcLineQty+".cType='2',16769996,"+;
    "IIF("+lcLineQty+".cType='3',16763806,16777215)))", "Column")
    .SETALL("Dynamicbackcolor","IIF("+lcLineQty+".cType='1',12320767,IIF("+lcLineQty+".cType='2',16769996,"+;
      "IIF("+lcLineQty+".cType='3',16763806,IIF("+lcLineQty+".cType='8',16763806,16777215))))", "Column")
  ENDWITH
  .lblItemQuality1.CAPTION = lcRetSHd3
  .lblItemQuality2.CAPTION = lcRetSHd4
  .kbItemQuality2.ENABLED  = llRSt1Stat
  .kbItemQuality3.ENABLED  = llRSt2Stat
  .kbItemQuality2.cquality = lcSndGrd
  .kbItemQuality3.cquality = lcTrdGrd
  .kbItemQuality2.VALUE    = lcRetSty1
  .kbItemQuality3.VALUE    = lcRetSty2
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  IF !EMPTY(lcRetSty1) AND !USED('SCALE1')
    m.lcRetS = lcRetSty1
    IF !SEEK(IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty1,loParentForm.lcTmpItem)
      IF loParentForm.lcInvType = "0001"
        *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
        *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = '" + lcRetSty1+ "'"        
        *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
        *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = [" + lcRetSty1+ "]"
        lcStySelValue = lcRetSty1
        lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = ?m.lcStySelValue"
        *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
        *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
      ELSE
        lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loParentForm.lcInvType +;
          "' AND Style = ?m.lcRetS "
      ENDIF
      =lfGetItmInf(loParentForm.lcInvType,;
        IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty1,;
        loParentForm.lcTmpItem,;
        IIF(loParentForm.lcInvType = "0001",'STYLE','ITEM'),lcSqlStatement,;
        loParentForm.lcInvType = "0002")
      =SEEK(IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty1,loParentForm.lcTmpItem)
    ENDIF
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
    *lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE='S"+EVALUATE(loParentForm.lcTmpItem+'.SCALE')+"'"
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
    *lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=[S"+EVALUATE(loParentForm.lcTmpItem+'.SCALE')+"]"
    lcScaleSelV = "S"+EVALUATE(loParentForm.lcTmpItem+'.SCALE')
    lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=?.lcScaleSelV "
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][END]
    =lfOpenFox(lcSqlStatement,'SCALE','SCALE1',"")
  ENDIF
  IF !EMPTY(lcRetSty2) AND !USED('SCALE2')
    m.lcRetS = lcRetSty2
    IF !SEEK(IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty2,loParentForm.lcTmpItem)
      IF loParentForm.lcInvType = "0001"
        *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
        *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = '" + lcRetSty2+ "'"        
        *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
        *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = [" + lcRetSty2+ "]"
        lcSelStyV = lcRetSty2
        lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = ?m.lcSelStyV"
        *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
        *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][ENd]
      ELSE
        lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loParentForm.lcInvType +;
          "' AND Style = ?m.lcRetS "
      ENDIF
      =lfGetItmInf(loParentForm.lcInvType,;
        IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty2,;
        loParentForm.lcTmpItem,;
        IIF(loParentForm.lcInvType = "0001",'STYLE','ITEM'),lcSqlStatement,;
        loParentForm.lcInvType = "0002")
      =SEEK(IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty2,loParentForm.lcTmpItem)
    ENDIF
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
    *lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE='S"+EVALUATE(loParentForm.lcTmpItem+'.SCALE')+"'"
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
    *lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=[S"+EVALUATE(loParentForm.lcTmpItem+'.SCALE')+"]"    
    lcSclValSel = "S"+EVALUATE(loParentForm.lcTmpItem+'.SCALE')
    lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=?m.lcSclValSel "
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
    =lfOpenFox(lcSqlStatement,'SCALE','SCALE2',"")
  ENDIF
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
ENDWITH
*--end of lfMfRcvQInit.

*!*************************************************************
*! Name      : lfvQty
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/07/2004
*! Purpose   : Validate the quantity fields.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfvQty
LPARAMETERS lnCnxt,loFormSet


LOCAL lcCnxt
lcCnxt = STR(lnCnxt,1)

SELECT (loFormSet.lcLineQty)
REPLACE nTotQty WITH nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8,;
  nUseQty WITH nTotQty * lnConf
lcType = cType
DO CASE
CASE cType = '4'
  laSok[lnCnxt]  = EVALUATE('nQty'+lcCnxt)
CASE cType = '5'
  laDam1[lnCnxt] = EVALUATE('nQty'+lcCnxt)
CASE cType = '6'
  laDam2[lnCnxt] = EVALUATE('nQty'+lcCnxt)
CASE cType = '7'
  laCan[lnCnxt]  = EVALUATE('nQty'+lcCnxt)
ENDCASE

IF loParentForm.lcPType $ 'O' AND (laSok[lnCnxt]+laDam1[lnCnxt]+laDam2[lnCnxt]+laCan[lnCnxt]) > laOrg[lnCnxt]
  =gfModalGen('INM00000B00000','','','',LANG_POSTREC_MESSAG1)
  REPLACE ('nQty'+lcCnxt) WITH IIF(cType='4',EVALUATE(loParentForm.lcTmpLine+'.Qty'+lcCnxt),0)
  REPLACE nTotQty WITH nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8
  DO CASE
  CASE cType = '4'
    laSok[lnCnxt]  = EVALUATE('nQty'+lcCnxt)
  CASE cType = '5'
    laDam1[lnCnxt] = EVALUATE('nQty'+lcCnxt)
  CASE cType = '6'
    laDam2[lnCnxt] = EVALUATE('nQty'+lcCnxt)
  CASE cType = '7'
    laCan[lnCnxt]  = EVALUATE('nQty'+lcCnxt)
  ENDCASE
  RETURN 0
ELSE
  IF loParentForm.lcPType $ 'AE' AND laSok[lnCnxt] > laOrg[lnCnxt]
    *-You cannot increase quantity more that original picked quantity!
    =gfModalGen('INM38189B00000','DIALOG')
    REPLACE ('nQty'+lcCnxt) WITH EVALUATE(loParentForm.lcTmpLine+'.Qty'+lcCnxt)
    REPLACE nTotQty WITH nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8
    laSok[lnCnxt] = EVALUATE('nQty'+lcCnxt)
    RETURN 0
  ENDIF
ENDIF

lnNewBal = laOrg[lnCnxt]-(laSok[lnCnxt]+laDam1[lnCnxt]+laDam2[lnCnxt]+laCan[lnCnxt])
IF cType<>'7' AND laCan[lnCnxt] > 0 AND lnNewBal<0
  *--Cancel quantity more that new balance, Therefore it will be redused.
  =gfModalGen('INM34068B42000','DIALOG')
  laBal[lnCnxt]=0
  laCan[lnCnxt]=laOrg[lnCnxt]-(laSok[lnCnxt]+laDam1[lnCnxt]+laDam2[lnCnxt])
  laCan[lnCnxt]=IIF(laCan[lnCnxt]<0,0,laCan[lnCnxt])
  =SEEK('7')
  REPLACE ('nQty'+lcCnxt) WITH laCan[lnCnxt]
  REPLACE nTotQty WITH nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8,;
    nUseQty WITH nTotQty * lnConf
ELSE
  laBal[lnCnxt]=laOrg[lnCnxt]-(laSok[lnCnxt]+laDam1[lnCnxt]+laDam2[lnCnxt]+laCan[lnCnxt])
  laBal[lnCnxt]=IIF(laBal[lnCnxt]<0,0,laBal[lnCnxt])
ENDIF
=SEEK('8')
REPLACE ('nQty'+lcCnxt) WITH laBal[lnCnxt]
REPLACE nTotQty WITH nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8,;
  nUseQty WITH nTotQty * lnConf
=SEEK(lcType)

IF laDam1[1]+laDam1[2]+laDam1[3]+laDam1[4]+laDam1[5]+laDam1[6]+laDam1[7]+laDam1[8]<>0
  IF EMPTY(lcRetSty1) AND SEEK(IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+EVALUATE(loParentForm.lcTmpLine+'.Style'),loParentForm.lcTmpItem)
    lcRetSty1 = EVALUATE(loParentForm.lcTmpItem+'.cRetSty')
    IF !EMPTY(lcRetSty1)
      *-- Get the style information
      IF !SEEK(IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty1,loParentForm.lcTmpItem)
        IF loParentForm.lcInvType = "0001"
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
          *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = '" + lcRetSty1 + "'"
          *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
          *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = [" + lcRetSty1 + "]"          
          lcSelStyValue =lcRetSty1 
          lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = ?m.lcSelStyValue"
          *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
        ELSE
          *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
          *lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loParentForm.lcInvType +;
          "' AND Style = '" + lcRetSty1 + "'"
          lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loParentForm.lcInvType +;
            "' AND Style = ?m.lcRetSty1 "
          *B607658,1 KHM 07/07/2005
        ENDIF

        =lfGetItmInf(loParentForm.lcInvType,;
          IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty1,;
          loParentForm.lcTmpItem,;
          IIF(loParentForm.lcInvType = "0001",'STYLE','ITEM'),lcSqlStatement,;
          loParentForm.lcInvType = "0002")
        =SEEK(IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty1,loParentForm.lcTmpItem)
      ENDIF
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
      *lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE='S"+EVALUATE(loParentForm.lcTmpItem+'.SCALE')+"'"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
      *lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=[S"+EVALUATE(loParentForm.lcTmpItem+'.SCALE')+"]"      
      lcSelValScl ="S"+EVALUATE(loParentForm.lcTmpItem+'.SCALE')
      lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=?m.lcSelValScl "
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
      =lfOpenFox(lcSqlStatement,'SCALE','SCALE1',"")
      IF SCALE1.CNT < lnCnxt
        lcRetSty1=SPACE(19)
      ENDIF
    ENDIF
    SELECT (loFormSet.lcLineQty)
  ENDIF
  loFormSet.AriaForm1.kbItemQuality2.ENABLED = .T.
ELSE
  lcRetSty1=SPACE(19)
  loFormSet.AriaForm1.kbItemQuality2.ENABLED = .F.
ENDIF
loFormSet.AriaForm1.kbItemQuality2.VALUE = lcRetSty1

IF laDam2[1]+laDam2[2]+laDam2[3]+laDam2[4]+laDam2[5]+laDam2[6]+laDam2[7]+laDam2[8]<>0
  IF EMPTY(lcRetSty2) AND SEEK(IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+EVALUATE(loParentForm.lcTmpLine+'.Style'),loParentForm.lcTmpItem)
    lcRetSty2 = EVALUATE(loParentForm.lcTmpItem+'.cRetSty2')
    IF !EMPTY(lcRetSty2)
      *-- Get the style information
      IF !SEEK(IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty2,loParentForm.lcTmpItem)
        IF loParentForm.lcInvType = "0001"
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
          *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = '" + lcRetSty2 + "'"
          *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
          *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = [" + lcRetSty2 + "]"          
          lcSelValSty = lcRetSty2 
          lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = ?m.lcSelValSty "
          *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
        ELSE
          *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
          *lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loParentForm.lcInvType +;
          "' AND Style = '" + lcRetSty2 + "'"
          lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loParentForm.lcInvType +;
            "' AND Style = ?m.lcRetSty2 "
          *B607658,1 KHM 07/07/2005
        ENDIF

        =lfGetItmInf(loParentForm.lcInvType,;
          IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty2,;
          loParentForm.lcTmpItem,;
          IIF(loParentForm.lcInvType = "0001",'STYLE','ITEM'),lcSqlStatement,;
          loParentForm.lcInvType = "0002")
        =SEEK(IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty2,loParentForm.lcTmpItem)
      ENDIF
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
      *lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE='S"+EVALUATE(loParentForm.lcTmpItem+'.SCALE')+"'"      
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
      *lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=[S"+EVALUATE(loParentForm.lcTmpItem+'.SCALE')+"]"      
      lcSclSelValue = "S"+EVALUATE(loParentForm.lcTmpItem+'.SCALE')
      lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=?m.lcSclSelValue "
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
      =lfOpenFox(lcSqlStatement,'SCALE','SCALE2',"")
      IF SCALE2.CNT < lnCnxt
        lcRetSty2=SPACE(19)
      ENDIF
    ENDIF
    SELECT (loFormSet.lcLineQty)
  ENDIF
  loFormSet.AriaForm1.kbItemQuality3.ENABLED = .T.
ELSE
  lcRetSty2=SPACE(19)
  loFormSet.AriaForm1.kbItemQuality3.ENABLED = .F.
ENDIF
loFormSet.AriaForm1.kbItemQuality3.VALUE = lcRetSty2

*!*  IF llSOInstld AND !(lcPType $ 'RN') AND _CUROBJ = OBJNUM(laCan[lnCnxt]) AND ;
*!*     ( laCan[lnCnxt] > laBud[lnCnxt] - laAlo[lnCnxt] )
*!*    lcCtPkKey = IIF(llMfCall,'1'+&lcTmpLine..Cuttkt,'2'+&lcTmpLine..PO)+&lcTmpLine..Style
*!*    IF SEEK(lcCtPkKey,'CutPick')
*!*      IF !lfChkOrdQt(STR(lnCnxt,1),.F.)
*!*        laCan[lnCnxt] = lcOldValue
*!*        SHOW GET laCan[lnCnxt]
*!*        _CUROBJ=_CUROBJ
*!*        RETURN
*!*      ENDIF
*!*    ENDIF
*!*  ENDIF

loFormSet.REFRESH
RETURN
*--end of lfvQty.

*:*************************************************************
*! Name     : lfvRetSty
*! Developer: Ahmed Maher (AMH)
*! Date     : 10/11/2004
*! Purpose  : Return style valid.
*:*************************************************************
FUNCTION lfvRetSty
LPARAMETERS lcPQualty,lcBrStyle

LOCAL lcOldValue
lcOldValue = lcRetSty&lcPQualty
lcRetSty&lcPQualty = lcBrStyle

*E039550,1 KHM
m.lcRetS = lcBrStyle
*E039550,1 KHM

IF !EMPTY(lcBrStyle)
  *-- Get the style information
  IF !SEEK(IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcBrStyle,loParentForm.lcTmpItem)
    IF loParentForm.lcInvType = "0001"
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
      *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = '" + lcBrStyle + "'"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
      *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = [" + lcBrStyle + "]"
      lcSelStyleValue = lcBrStyle 
      lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = ?m.lcSelStyleValue "
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][END]
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
    ELSE
      *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
      *lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loParentForm.lcInvType +;
      "' AND Style = '" + lcBrStyle + "'"

      *E039550,1 KHM
      *lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loParentForm.lcInvType +;
      "' AND Style = ?m.lcBrStyle "
      lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loParentForm.lcInvType +;
        "' AND Style = ?m.lcRetS "
      *E039550,1 KHM

      *B607658,1 KHM 07/07/2005
    ENDIF

    =lfGetItmInf(loParentForm.lcInvType,;
      IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcBrStyle,;
      loParentForm.lcTmpItem,;
      IIF(loParentForm.lcInvType = "0001",'STYLE','ITEM'),lcSqlStatement,;
      loParentForm.lcInvType = "0002")
    =SEEK(IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcBrStyle,loParentForm.lcTmpItem)
  ENDIF
  *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
  *lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE='S"+EVALUATE(loParentForm.lcTmpItem+'.SCALE')+"'"
  *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
  *lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=[S"+EVALUATE(loParentForm.lcTmpItem+'.SCALE')+"]"  
  lcSelScaleVal = "S"+EVALUATE(loParentForm.lcTmpItem+'.SCALE')
  lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=?m.lcSelScaleVal"
  *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
  *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
  =lfOpenFox(lcSqlStatement,'SCALE','SCALE'+lcPQualty,"")
ENDIF

IF !EMPTY(LCBrStyle) AND EVALUATE('SCALE'+lcPQualty+'.SCALE')<>SCALE.SCALE
  LOCAL llDefScale,lnI,lcI,lnDamCnt
  llDefScale = .F.
  lnDamCnt = 1
  FOR lnI = 1 TO SCALE.CNT
    lcI = STR(lnI,1)
    lnDamCnt = MAX(lnDamCnt,IIF(EVALUATE('laDam'+lcPQualty+'['+lcI+']')>0,lnI,0))
  ENDFOR
  llDefScale = (EVALUATE('SCALE'+lcPQualty+'.Cnt')<lnDamCnt)
  *--The selected style has a different size scale.
  IF llDefScale
    lcBrStyle = ''
    lcRetSty&lcPQualty = ''
    =gfModalGen('TRM42089B42000','DIALOG',"","","Selected style has a different size scale. Can not proceed")
    RETURN .F.
  ELSE
    =gfModalGen('INM42089B42000','DIALOG')
  ENDIF
ENDIF

IF !EMPTY(LCBrStyle) AND loParentForm.llWareHous
  *-- Get the stydye information
  IF !SEEK(IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+;
      PADR(LCBrStyle,19)+lcWareCode+SPACE(10),loParentForm.lcItemLoc)
    IF loParentForm.lcInvType = "0001"
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
*!*	      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
*!*	        PADR(LCBrStyle,19)+lcWareCode+SPACE(10) + "'"
   	  *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
*!*	      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
*!*	        PADR(LCBrStyle,19)+lcWareCode+SPACE(10) + "]"
      lcValSelectSty =PADR(LCBrStyle,19)+lcWareCode+SPACE(10) 
      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = ?m.lcValSelectSty"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]  
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]  
    ELSE
      *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
      *lcSqlStatement  = "SELECT * FROM ITEMLOC [INDEX=STYDYE] " + ;
      "WHERE cInvType ='" + loParentForm.lcInvType + "' AND "+;
      "Style ='" + LCBrStyle +"' AND " + ;
      "cWareCode ='" + lcWareCode + "' AND " + ;
      "Dyelot = '         '"

      *E039550,1 KHM
      *lcSqlStatement  = "SELECT * FROM ITEMLOC [INDEX=STYDYE] " + ;
      "WHERE cInvType ='" + loParentForm.lcInvType + "' AND "+;
      "Style = ?m.lcStyle " + " AND " + ;
      "cWareCode = ?m.lcWareCode" + " AND " + ;
      "Dyelot = '" + PADR(lcDyelot,10) + "'"
      lcSqlStatement  = "SELECT * FROM ITEMLOC [INDEX=STYDYE] " + ;
        "WHERE cInvType ='" + loParentForm.lcInvType + "' AND "+;
        "Style = ?m.lcRetS " + " AND " + ;
        "cWareCode = ?m.lcWareCode" + " AND " + ;
        "Dyelot = '" + PADR(lcDyelot,10) + "'"
      *E039550,1 KHM

      *B607658,1 KHM 07/07/2005
    ENDIF

    =lfGetItmInf(loParentForm.lcInvType,;
      IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+PADR(LCBrStyle,19)+lcWareCode+SPACE(10),;
      loParentForm.lcItemLoc,;
      IIF(loParentForm.lcInvType = "0001",'STYDYE','ITEMLOC'),lcSqlStatement,;
      loParentForm.lcInvType = "0002")
    IF !SEEK(IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+;
        PADR(LCBrStyle,19)+lcWareCode+SPACE(10),loParentForm.lcItemLoc)

      IF loParentForm.lcInvType="0001"
        *-Style: xxx is not assigned to Location: xxx. "\<Add;\<Reenter"
        IF gfModalGen('QRM34048B42006','DIALOG',ALLTRIM(LCBrStyle)+'|'+lcWareCode) = 1
          DO gpAdStyWar WITH LCBrStyle,SPACE(10),lcWareCode
        ELSE
          STORE lcOldValue TO lcRetSty&lcPQualty
          RETURN .F.
        ENDIF
      ELSE
        lcMsg = "Fabric: " + ALLTRIM(LCBrStyle)
        IF gfModalGen('QRM36226B34004','DIALOG',lcMsg +'|'+lcWareCode) = 1
          =gfAdItemWar(loParentForm.lcInvType,LCBrStyle,SPACE(10),lcWareCode)
        ELSE
          STORE lcOldValue TO lcRetSty&lcPQualty
          RETURN .F.
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDIF

RETURN

*:*************************************************************
*! Name      : lfvQtyOk
*! Developer : Ahmed Maher (AMH)
*! Date      : 10/12/2004
*! Purpose   : Quantity Ok pb. valid.
*:*************************************************************
FUNCTION lfvQtyOk
LPARAMETERS loFormSet

*! C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
IF ASCAN(loparentform.laevnttrig, PADR('CALCSSHSCR', 10), 1, ALEN(loparentform.laevnttrig, 1), 1)>0
  DO lfprrecdata IN DIRMAIN.FXP WITH loformset
ENDIF
*! C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]

LOCAL lcMsg
lcMsg = IIF(loParentForm.lcInvType = "0001", ' Style',' Fabric')
IF loParentForm.ActiveMode#'V'
  LOCAL lnAlias,lcStyle
  lnAlias = SELECT(0)
  lcStyle = EVALUATE(loParentForm.lcTmpLine+'.Style')

  *-You cannot leave the XXXX style empty since the XXXX quantity was entered.
  IF laDam1[1]+laDam1[2]+laDam1[3]+laDam1[4]+laDam1[5]+laDam1[6]+laDam1[7]+laDam1[8]<>0 AND ;
      EMPTY(lcRetSty1)
    =gfModalGen('TRM34201B42000','DIALOG',lcRetSHd3+lcMsg+'|'+lcRetSHd3)
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
    *loFormSet.AriaForm1.kbItemQuality2.SetFocus
    *! B610965,1 MMT 03/12/2015 Error while editing line Qty[T20150215.0015][Start]
    *IF !loFormSet.llSilentMod
    IF !loParentForm.llSilentMod
    *! B610965,1 MMT 03/12/2015 Error while editing line Qty[T20150215.0015][End]
      loFormSet.AriaForm1.kbItemQuality2.SETFOCUS
    ENDIF
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
  IF laDam2[1]+laDam2[2]+laDam2[3]+laDam2[4]+laDam2[5]+laDam2[6]+laDam2[7]+laDam2[8]<>0 AND ;
      EMPTY(lcRetSty2)
    =gfModalGen('TRM34201B42000','DIALOG',lcRetSHd4+lcMsg+'|'+lcRetSHd4)
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
    *loFormSet.AriaForm1.kbItemQuality3.SetFocus
    *! B610965,1 MMT 03/12/2015 Error while editing line Qty[T20150215.0015][Start]
    *IF !loFormSet.llSilentMod    
    IF !loParentForm.llSilentMod
    *! B610965,1 MMT 03/12/2015 Error while editing line Qty[T20150215.0015][End]
      loFormSet.AriaForm1.kbItemQuality3.SETFOCUS
    ENDIF
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
    SELECT (lnAlias)
    RETURN .F.
  ENDIF

  IF loParentForm.llDyelot AND EMPTY(lcDyelot)
    IF !EMPTY(lcRetSty1) AND;
        SEEK(IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty1,loParentForm.lcTmpItem) AND;
        EVALUATE(loParentForm.lcTmpItem+'.CDYE_FLG')='Y'
      *--The style xxx comes in dyelot but the original style xxxx did not use dyelots,
      *--Please make sure the the other quality style has same dyelot usage.
      =gfModalGen('TRM34202B42000','DIALOG',ALLTRIM(lcMsg)+' '+lcRetSty1+'|'+;
        EVALUATE(loParentForm.lcTmpLine+'.Style')+'|'+ALLTRIM(lcMsg))
      *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
      *loFormSet.AriaForm1.kbItemQuality2.SetFocus
      *! B610965,1 MMT 03/12/2015 Error while editing line Qty[T20150215.0015][Start]
      *IF !loFormSet.llSilentMod      
      IF !loParentForm.llSilentMod
      *! B610965,1 MMT 03/12/2015 Error while editing line Qty[T20150215.0015][End]
        loFormSet.AriaForm1.kbItemQuality2.SETFOCUS
      ENDIF
      *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
      SELECT (lnAlias)
      RETURN .F.
    ENDIF
    IF !EMPTY(lcRetSty2) AND;
        SEEK(IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty2,loParentForm.lcTmpItem) AND;
        EVALUATE(loParentForm.lcTmpItem+'.CDYE_FLG')='Y'
      *--The style xxx comes in dyelot but the original style xxxx did not use dyelots,
      *--Please make sure the the other quality style has same dyelot usage.
      =gfModalGen('TRM34202B42000','DIALOG',ALLTRIM(lcMsg)+' '+lcRetSty2+'|'+;
        EVALUATE(loParentForm.lcTmpLine+'.Style')+'|'+ALLTRIM(lcMsg))
      *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
      *loFormSet.AriaForm1.kbItemQuality3.SetFocus
      *! B610965,1 MMT 03/12/2015 Error while editing line Qty[T20150215.0015][Start]
      *IF !loFormSet.llSilentMod      
      IF !loParentForm.llSilentMod
      *! B610965,1 MMT 03/12/2015 Error while editing line Qty[T20150215.0015][End]
        loFormSet.AriaForm1.kbItemQuality3.SETFOCUS
      ENDIF
      *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
      SELECT (lnAlias)
      RETURN .F.
    ENDIF
  ENDIF

  *--If Receive Return P/O and costs method FIFO or LIFO.
  *--Not Accept to Issue more than Stock.
  IF loParentForm.llIssue AND;
      IIF(loParentForm.lcInvType="0001",loParentForm.lcCostMth,loParentForm.lcCostMthM) $ 'FLI'
    IF laSok[1]+laSok[2]+laSok[3]+laSok[4]+laSok[5]+laSok[6]+laSok[7]+laSok[8]<>0
      *--Get Current Stock.
      IF !SEEK(IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+;
          PADR(lcStyle,19)+lcWareCode+PADR(lcDyelot,10),loParentForm.lcItemLoc)
        IF loParentForm.lcInvType = "0001"
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
*!*	          lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
*!*	            PADR(lcStyle,19)+lcWareCode+PADR(lcDyelot,10) + "'"
	   	  *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
*!*	          lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
*!*	            PADR(lcStyle,19)+lcWareCode+PADR(lcDyelot,10) + "]"
          lcStySelectV =  PADR(lcStyle,19)+lcWareCode+PADR(lcDyelot,10) 
          lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot =?m.lcStySelectV"
          *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]   
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]  
        ELSE
          *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
          *lcSqlStatement  = "SELECT * FROM ITEMLOC [INDEX=STYDYE] " + ;
          "WHERE cInvType ='" + loParentForm.lcInvType + "' AND "+;
          "Style ='" + PADR(lcStyle,19) +"' AND " + ;
          "cWareCode ='" + lcWareCode + "' AND " + ;
          "Dyelot = '" + PADR(lcDyelot,10) + "'"
          lcStyVal = lcStyle
          lcSqlStatement  = "SELECT * FROM ITEMLOC [INDEX=STYDYE] " + ;
            "WHERE cInvType ='" + loParentForm.lcInvType + "' AND "+;
            "Style = ?m.lcStyVal " + " AND " + ;
            "cWareCode = ?m.lcWareCode" + " AND " + ;
            "Dyelot = '" + PADR(lcDyelot,10) + "'"
          *B607658,1 KHM 07/07/2005
        ENDIF

        =lfGetItmInf(loParentForm.lcInvType,;
          IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+PADR(lcStyle,19)+lcWareCode+PADR(lcDyelot,10),;
          loParentForm.lcItemLoc,;
          IIF(loParentForm.lcInvType = "0001",'STYDYE','ITEMLOC'),lcSqlStatement,;
          loParentForm.lcInvType = "0002")
        =SEEK(IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+;
          PADR(lcStyle,19)+lcWareCode+PADR(lcDyelot,10),loParentForm.lcItemLoc)
      ENDIF
      SELECT (loParentForm.lcItemLoc)
      SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 TO laSStk
      FOR I=1 TO 8
        IF laSStk[I] < laSok[I]
          *--Insufficient stock to issue P/O line!
          = gfModalGen('TRM34107B34000','DIALOG')
          SELECT (loFormSet.lcLineQty)
          =SEEK('4')
          *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
          *loFormSet.AriaForm1.grdLineQty.SetFocus
          *! B610965,1 MMT 03/12/2015 Error while editing line Qty[T20150215.0015][Start]
          *IF !loFormSet.llSilentMod          
          IF !loParentForm.llSilentMod
          *! B610965,1 MMT 03/12/2015 Error while editing line Qty[T20150215.0015][End]
            loFormSet.AriaForm1.grdLineQty.SETFOCUS
          ENDIF
          *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
          SELECT(lnAlias)
          RETURN .F.
        ENDIF
      ENDFOR
    ENDIF
    IF laDam1[1]+laDam1[2]+laDam1[3]+laDam1[4]+laDam1[5]+laDam1[6]+laDam1[7]+laDam1[8]<>0
      IF !SEEK(IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+;
          PADR(lcRetSty1,19)+lcWareCode+SPACE(10),loParentForm.lcItemLoc)
        IF loParentForm.lcInvType = "0001"
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
*!*	          lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
*!*	            PADR(lcRetSty1,19)+lcWareCode+SPACE(10) + "'"
		  *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
*!*	          lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
*!*	            PADR(lcRetSty1,19)+lcWareCode+SPACE(10) + "]"
          lcSelValSty =PADR(lcRetSty1,19)+lcWareCode+SPACE(10) 
          lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = ?m.lcSelValSty "
          *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]  
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]  
        ELSE
          *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
          *lcSqlStatement  = "SELECT * FROM ITEMLOC [INDEX=STYDYE] " + ;
          "WHERE cInvType ='" + loParentForm.lcInvType + "' AND "+;
          "Style ='" + PADR(lcRetSty1,19) +"' AND " + ;
          "cWareCode ='" + lcWareCode + "' AND " + ;
          "Dyelot = '" + SPACE(10) + "'"
          lcSqlStatement  = "SELECT * FROM ITEMLOC [INDEX=STYDYE] " + ;
            "WHERE cInvType ='" + loParentForm.lcInvType + "' AND "+;
            "Style = ?m.lcRetSty1 "  + " AND " + ;
            "cWareCode =?m.lcWareCode " + " AND " + ;
            "Dyelot = '" + SPACE(10) + "'"
          *B607658,1 KHM 07/07/2005 [End]
        ENDIF

        =lfGetItmInf(loParentForm.lcInvType,;
          IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+PADR(lcRetSty1,19)+lcWareCode+SPACE(10),;
          loParentForm.lcItemLoc,;
          IIF(loParentForm.lcInvType = "0001",'STYDYE','ITEMLOC'),lcSqlStatement,;
          loParentForm.lcInvType = "0002")
        =SEEK(IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+;
          PADR(lcRetSty1,19)+lcWareCode+SPACE(10),loParentForm.lcItemLoc)
      ENDIF
      SELECT (loParentForm.lcItemLoc)
      SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 TO laSStk
      FOR I=1 TO 8
        IF laSStk[I] < laDam1[I]
          *--Insufficient stock to issue P/O line!
          = gfModalGen('TRM34107B34000','DIALOG')
          SELECT (loFormSet.lcLineQty)
          =SEEK('5')
          *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
          *loFormSet.AriaForm1.grdLineQty.SetFocus
          *! B610965,1 MMT 03/12/2015 Error while editing line Qty[T20150215.0015][Start]
          *IF !loFormSet.llSilentMod          
          IF !loParentForm.llSilentMod
          *! B610965,1 MMT 03/12/2015 Error while editing line Qty[T20150215.0015][End]
            loFormSet.AriaForm1.grdLineQty.SETFOCUS
          ENDIF
          *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

          SELECT(lnAlias)
          RETURN .F.
        ENDIF
      ENDFOR
    ENDIF
    IF laDam2[1]+laDam2[2]+laDam2[3]+laDam2[4]+laDam2[5]+laDam2[6]+laDam2[7]+laDam2[8]<>0
      IF !SEEK(IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+;
          PADR(lcRetSty2,19)+lcWareCode+SPACE(10),loParentForm.lcItemLoc)
        IF loParentForm.lcInvType = "0001"
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
*!*	          lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
*!*	            PADR(lcRetSty2,19)+lcWareCode+SPACE(10) + "'"
		  *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
*!*	          lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
*!*	            PADR(lcRetSty2,19)+lcWareCode+SPACE(10) + "]"
          lcStySelValue =PADR(lcRetSty2,19)+lcWareCode+SPACE(10)
          lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot =?m.lcStySelValue "
          *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]  
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]  
        ELSE
          *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
          *lcSqlStatement  = "SELECT * FROM ITEMLOC [INDEX=STYDYE] " + ;
          "WHERE cInvType ='" + loParentForm.lcInvType + "' AND "+;
          "Style ='" + PADR(lcRetSty2,19) +"' AND " + ;
          "cWareCode ='" + lcWareCode + "' AND " + ;
          "Dyelot = '" + SPACE(10) + "'"
          lcSqlStatement  = "SELECT * FROM ITEMLOC [INDEX=STYDYE] " + ;
            "WHERE cInvType ='" + loParentForm.lcInvType + "' AND "+;
            "Style = ?m.lcRetSty2 " + " AND " + ;
            "cWareCode = ?m.lcWareCode "  + " AND " + ;
            "Dyelot = '" + SPACE(10) + "'"
          *B607658,1 KHM 07/07/2005
        ENDIF

        =lfGetItmInf(loParentForm.lcInvType,;
          IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+PADR(lcRetSty2,19)+lcWareCode+SPACE(10),;
          loParentForm.lcItemLoc,;
          IIF(loParentForm.lcInvType = "0001",'STYDYE','ITEMLOC'),lcSqlStatement,;
          loParentForm.lcInvType = "0002")
        =SEEK(IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+;
          PADR(lcRetSty2,19)+lcWareCode+SPACE(10),loParentForm.lcItemLoc)
      ENDIF
      SELECT (loParentForm.lcItemLoc)
      SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 TO laSStk
      FOR I=1 TO 8
        IF laSStk[I] < laDam1[I]
          *--Insufficient stock to issue P/O line!
          = gfModalGen('TRM34107B34000','DIALOG')
          SELECT (loFormSet.lcLineQty)
          =SEEK('6')
          *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
          *loFormSet.AriaForm1.grdLineQty.SetFocus
          *! B610965,1 MMT 03/12/2015 Error while editing line Qty[T20150215.0015][Start]
          *IF !loFormSet.llSilentMod          
          IF !loParentForm.llSilentMod
          *! B610965,1 MMT 03/12/2015 Error while editing line Qty[T20150215.0015][end]
            loFormSet.AriaForm1.grdLineQty.SETFOCUS
          ENDIF
          *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
          SELECT(lnAlias)
          RETURN .F.
        ENDIF
      ENDFOR
    ENDIF
  ENDIF
  SELECT (lnAlias)
ENDIF

RETURN .T.

*!*************************************************************
*! Name      : lfEditLnCst
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 09/11/2004
*! Purpose   : To allow the user to edit the costs per line
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
*! Called    : From Init in the formset
*!*************************************************************
*! B125331,1 KHM 11/18/2004 Add feature of edit landed cost
*!*************************************************************
FUNCTION lfEditLnCst
LPARAMETERS loFormSet, lnBarNo

DO CASE
CASE lnBarNo = 1
  loFormSet.llEditLCst = !loFormSet.llEditLCst
  SET MARK OF BAR 1 OF _OPTIONPOP TO _SCREEN.ACTIVEFORM.PARENT.llEditLCst
ENDCASE

*!*************************************************************
*! Name      : lfvLCost
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 09/11/2004
*! Purpose   : To edit the costs per line
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
*! B125331,1 KHM 11/18/2004 Add feature of edit landed cost
*!*************************************************************
FUNCTION lfvLCost
LPARAMETERS loFormSet

*B126833,1 WAM 04/03/2005 Add new button to view/Edit landed cost
SELECT (loFormSet.lcTmpLine)

SET FILTER TO
lcLKey = cCarton+Po+STYLE+Dyelot+PADR(cWareCode,6)+STR(LINENO,6)
lcSndGrd = "2"
lcTrdGrd = "3"
DO CASE
CASE cStyGrade = '2'
  lcSndGrd = "1"
  lcTrdGrd = "3"
CASE cStyGrade = '3'
  lcSndGrd = "1"
  lcTrdGrd = "2"
ENDCASE
*B126833,1 WAM 04/03/2005 (End)

*N000587,1 WAM 12/01/2007 Get PO cost sheet lines
=lfGetBomLn(loFormSet,cStyType, PO, ShipNo)
*N000587,1 WAM 12/01/2007 (End)

*B126833,1 WAM 04/03/2005 Add new button to view/Edit landed cost
*DO FORM (oAriaApplication.ScreenHome+'EDTRCVLN.SCX') WITH loFormSet, loFormSet.lcPType
*! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+'EDTRCVLN.SCX') WITH loFormSet, loFormSet.lcPType, !EVALUATE(loFormSet.lcTmpLine+'.lDetCost') AND loFormSet.llEditLCst
loCallingForm  = loFormSet
=gfCallForm('EDTRCVLN',.F.,"loCallingForm, loCallingForm.lcPType, !EVALUATE(loCallingForm.lcTmpLine+'.lDetCost') AND loCallingForm.llEditLCst")
*! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[End]
*B126833,1 WAM 04/03/2005 (End)

*B126833,1 WAM 04/03/2005 Add new button to view/Edit landed cost
SELECT (loFormSet.lcTmpLine)
IF !lDetCost AND loFormSet.llEditLCst
  lnCostVal1 = nFLanCost1
  lnCostVal2 = nFLanCost2
  lnCostVal3 = nFLanCost3
  lnCostVal4 = nFLanCost4
  lnCostVal5 = nFLanCost5
  lnCostVal6 = nFLanCost6
  lnCostVal7 = nFLanCost7
  lnEqCost1  = nLan_Cost1
  lnEqCost2  = nLan_Cost2
  lnEqCost3  = nLan_Cost3
  lnEqCost4  = nLan_Cost4
  lnEqCost5  = nLan_Cost5
  lnEqCost6  = nLan_Cost6
  lnEqCost7  = nLan_Cost7

  *--1) Update stock quantity.
  IF SEEK('2'+lcLKey)
    REPLACE nLan_Cost1 WITH lnEqCost1, nLan_Cost2 WITH lnEqCost2, nLan_Cost3 WITH lnEqCost3,;
      nLan_Cost4 WITH lnEqCost4, nLan_Cost5 WITH lnEqCost5, nLan_Cost6 WITH lnEqCost6,;
      nLan_Cost7 WITH lnEqCost7,;
      nFLanCost1 WITH lnCostVal1, nFLanCost2 WITH lnCostVal2, nFLanCost3 WITH lnCostVal3,;
      nFLanCost4 WITH lnCostVal4, nFLanCost5 WITH lnCostVal5,;
      nFLanCost6 WITH lnCostVal6, nFLanCost7 WITH lnCostVal7
  ENDIF

  *--2) Update Damaged quantity.
  =SEEK('4'+lcLKey)
  LOCATE REST WHILE Trancd+cCarton+Po+STYLE+Dyelot+PADR(cWareCode,6)+STR(LINENO,6)='4'+lcLKey ;
    FOR cStyGrade=lcSndGrd
  IF FOUND()
    REPLACE nLan_Cost1 WITH lnEqCost1, nLan_Cost2 WITH lnEqCost2, nLan_Cost3 WITH lnEqCost3,;
      nLan_Cost4 WITH lnEqCost4, nLan_Cost5 WITH lnEqCost5, nLan_Cost6 WITH lnEqCost6,;
      nLan_Cost7 WITH lnEqCost7,;
      nFLanCost1 WITH lnCostVal1, nFLanCost2 WITH lnCostVal2, nFLanCost3 WITH lnCostVal3,;
      nFLanCost4 WITH lnCostVal4, nFLanCost5 WITH lnCostVal5,;
      nFLanCost6 WITH lnCostVal6, nFLanCost7 WITH lnCostVal7
  ENDIF
  =SEEK('4'+lcLKey)
  LOCATE REST WHILE Trancd+cCarton+Po+STYLE+Dyelot+PADR(cWareCode,6)+STR(LINENO,6)='4'+lcLKey ;
    FOR cStyGrade=lcTrdGrd
  IF FOUND()
    REPLACE nLan_Cost1 WITH lnEqCost1, nLan_Cost2 WITH lnEqCost2, nLan_Cost3 WITH lnEqCost3,;
      nLan_Cost4 WITH lnEqCost4, nLan_Cost5 WITH lnEqCost5, nLan_Cost6 WITH lnEqCost6,;
      nLan_Cost7 WITH lnEqCost7,;
      nFLanCost1 WITH lnCostVal1, nFLanCost2 WITH lnCostVal2, nFLanCost3 WITH lnCostVal3,;
      nFLanCost4 WITH lnCostVal4, nFLanCost5 WITH lnCostVal5,;
      nFLanCost6 WITH lnCostVal6, nFLanCost7 WITH lnCostVal7
  ENDIF
  *--3) Update cancel quantity.
  IF SEEK('5'+lcLKey)
    REPLACE nLan_Cost1 WITH lnEqCost1, nLan_Cost2 WITH lnEqCost2, nLan_Cost3 WITH lnEqCost3,;
      nLan_Cost4 WITH lnEqCost4, nLan_Cost5 WITH lnEqCost5, nLan_Cost6 WITH lnEqCost6,;
      nLan_Cost7 WITH lnEqCost7,;
      nFLanCost1 WITH lnCostVal1, nFLanCost2 WITH lnCostVal2, nFLanCost3 WITH lnCostVal3,;
      nFLanCost4 WITH lnCostVal4, nFLanCost5 WITH lnCostVal5,;
      nFLanCost6 WITH lnCostVal6, nFLanCost7 WITH lnCostVal7
  ENDIF
ENDIF
SET FILTER TO TranCd = '1'
=SEEK('1'+lcLKey)
loFormSet.REFRESH
*B126833,1 WAM 04/03/2005 (End)

*!*************************************************************
*! Name      : lfUnDo()   B000109,1
*! Developer : Wael Ali Mohamed
*! Date      : 03/05/2005
*! Purpose   : Remove PO lock upon undo session
*!*************************************************************
*! Call      : None
*!*************************************************************
*! Example   : =lfUnDo(loFormSet)
*!*************************************************************
FUNCTION lfUnDo
LPARAMETERS loFormSet
IF .F.
  lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'',.T.)
  IF TYPE('lcTranCode') = 'N'
    =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
  ELSE
    SELECT (loFormSet.lcTmpLine)
    SET ORDER TO TAG TmpLine3
    GO TOP
    DO WHILE !EOF()
      lcPo = Po
      lcSelString = "UPDATE POSHDR SET lLok_stat =0,cLok_User= '', dLok_Date='',cLok_Time='' WHERE cBusDocu+cStyType+PO='"+EVALUATE(loFormSet.lcTmpLine+'.cBusDocu')+EVALUATE(loFormSet.lcTmpLine+'.cStyType')+EVALUATE(loFormSet.lcTmpLine+'.Po')+"'"
      lnResult = oAriaApplication.RemoteCompanyData.Execute(lcSelString,'',"SAVEFILE","",lcTranCode ,4,'',SET("DataSession"))
      IF lnResult <=0
        =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLUPDATE",lnResult,.T.)
        =oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
        RETURN
      ENDIF
      SELECT (loFormSet.lcTmpLine)
      SCAN REST WHILE PO = lcPo
      ENDSCAN
    ENDDO
    =oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
  ENDIF
ENDIF
*:*************************************************************
*! Name     : lfPrnLanded
*! Developer: Wael M. Abo-Shawareb (WSH)
*! Date     : 04/11/2005
*! Purpose  : Calculate landed costs case of detail costing.
*:*************************************************************
*! B125565,1
FUNCTION lfPrnLanded
LPARAMETERS loFormSet

LOCAL lnCnt

PRIVATE lcPriceCur, lcDutyCur, lcIType1, lcIType2, lcIType3, lcIType4, lcIType5, lcIType6, lcIType7
DIMENSION laECost[1]

SELECT (lcMastPoHd)
LOCATE
lcPriceCur = EVALUATE(lcMastPoHd+'.cPriceCur')
lcDutyCur  = EVALUATE(lcMastPoHd+'.cDutyCur')

IF loFormSet.lcPType = 'S'
  loBomLine.SETORDER('BomLnShp')
ELSE
  loBomLine.SETORDER('BomLine')
ENDIF

SELECT (lcRepCurs)
SCATTER FIELDS Qty1, qty2, qty3, qty4, qty5, qty6, qty7, qty8 TO laLnQty

FOR lnCnt = 1 TO 7
  lcCnt = STR(lnCnt,1)
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
  lcBomLKey = IIF(loFormSet.lcPType = 'D', 'D',IIF(loFormSet.lcPType = 'W' ,'T', 'I')) + '2' + IIF(loFormSet.lcPType = 'S', EVALUATE(lcRepCurs + '.Shipno'), '') + EVALUATE(lcRepCurs + '.Po') + STR(EVALUATE(lcRepCurs + '.LineNo'), 6) + lcCnt + loFormSet.lcInvType + EVALUATE(lcRepCurs + '.Style')
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
  lcWCondtn = "cImTyp+cType+IIF(loFormSet.lcPType='S',ShipNo,'')+cTktNo+STR(LineNo,6)+cBomTyp+cInvType+Style = lcBomLKey"
  lcFCondtn = "EMPTY(cRSession) AND cStyGrade = " + lcRepCurs + ".cStyGrade"

  lcIType&lcCnt = loFormSet.lcIType&lcCnt
  lnNLCs&lcCnt  = 0
  lnCurSQt      = 0

  IF loBomLine.SEEK(lcBomLKey)
    SELECT (lcBomLine)
    SUM REST (UnitCost*UnitQty) * lfBomSzQty() WHILE &lcWCondtn FOR &lcFCondtn TO lnNLCs&lcCnt
    lnNLCs&lcCnt = IIF(EVALUATE(lcRepCurs + '.TotQty') <> 0, (lnNLCs&lcCnt / EVALUATE(lcRepCurs + '.TotQty')), 0)
  ENDIF

  SELECT (lcRepCurs)
  REPLACE nflancost&lcCnt WITH lnNLCs&lcCnt

  IF !loFormSet.llMFCall
    IF loFormSet.llMulCurr
      =lfGetEqv(lcCnt, loFormSet.lnRate1, loFormSet.lnRate2, loFormSet.lnCurrUnt1, loFormSet.lnCurrUnt2, nflancost1, nflancost2, nflancost3, nflancost4, nflancost5, nflancost6, nflancost7)
      REPLACE nlan_cost&lcCnt WITH laECost[1]
    ELSE
      REPLACE nlan_cost&lcCnt WITH lnNLCs&lcCnt
    ENDIF
  ENDIF
ENDFOR

loBomLine.SETORDER('BOMLINE')

SELECT (lcRepCurs)
RETURN

*:*************************************************************
*! Name     : lfChekAdj
*! Developer: Wael M. Abo-Shawareb (WSH)
*! Date     : 04/11/2005
*! Purpose  : Check if adjust cost for recieve..
*:*************************************************************
*! B125565,1
FUNCTION lfChekAdj
LPARAMETERS lcTrType, lcTket, lcItem, lcColor, lcSess, lcStyQlt

LOCAL lnOldAls, llToREt
lnOldAls = SELECT(0)
lcItem   = PADR(lcItem, 19)
llToREt  = .F.

lcSeekKey = IIF(lcTrType = 'S', 'I', lcTrType) + '2' + lcTket
lcWhleCnd = "cIMTyp+cType+cTktNo+STR(LineNo,6)=lcSeekKey"

lcForCond = "Style=lcItem AND EMPTY(cRSession)"
lcQltFltr = IIF(TYPE('lcStyQlt') $ 'UL' ,".T.","cStyGrade = lcStyQlt")

loBomLine.SETORDER('BOMLINE')
IF loBomLine.SEEK(lcSeekKey)
  SELECT (lcBomLine)
  LOCATE REST WHILE &lcWhleCnd FOR &lcForCond AND &lcQltFltr
  llToREt = FOUND()
ENDIF

SELECT(lnOldAls)
RETURN llToREt

*!***************************************************************************
*! Name     : lfBomSzQty
*! Developer: Wael M. Abo-Shawareb (WSH)
*! Date     : 04/11/2005
*! Purpose  : Calculate the style quantity per size.
*!******************************************************************************
*! B125565,1
FUNCTION lfBomSzQty

lnCurSQt = 0
FOR lnI=1 TO 8
  IF STR(lnI,1) $ EVALUATE(lcBomLine + '.CSIZES')
    lnCurSQt = lnCurSQt + laLnQty[lnI]
  ENDIF
ENDFOR
RETURN lnCurSQt

*N038893,1 WAM 06/02/2005 Receive Cutting Ticket
FUNCTION lfDyeOvrRcv


*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [START]
*!***************************************************************************
*! Name     : lfvRollAss
*! Developer: Mariam Mazhar[MMT]
*! Date     : 03/21/2007
*! Purpose  : Assign rolls
*!******************************************************************************
FUNCTION lfvRollAss
PARAMETERS loFormset

lnJSgn   = IIF(loFormSet.lcPType $ 'G',-1,1)
DECLARE aAdjStk[9]
aAdjStk = 0
aAdjStk[1]  = lnJSgn*EVALUATE(loFormSet.lcTmpLine+'.TotStk')
aAdjStk[9]  = aAdjStk[1]
FOR LoopNo=2 TO 8
  aAdjStk[LoopNo]= 0
  aAdjStk[9] = aAdjStk[9] + aAdjStk[LoopNo]
NEXT

ldTranDate =loFormset.ariaForm1.dtPickerPostingDate.VALUE
ldPostDate =loFormset.ariaForm1.dtpickerReceivingDate.VALUE
*! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[Start]
*!*	DO (oAriaApplication.ApplicationHome+'MAJrRoData.PRG') WITH IIF(loFormSet.lcPType $ 'G','6','5'),;
*!*				loFormset.lcInvType,EVALUATE(loFormSet.lcTmpLine+'.Style'),;
*!*				EVALUATE(loFormSet.lcTmpLine+'.Dyelot'),EVALUATE(loFormSet.lcTmpLine+'.cWareCode'),;
*!*				"",aAdjStk,loFormSet.lcTmpJour,loFormSet.lcFullRoll,;
*!*				loFormSet.lcTmpRoll,ldTranDate ,ldPostDate
IF FILE(oAriaApplication.clientapplicationhome+'MAJrRoData.FXP')
  DO (oAriaApplication.clientapplicationhome+'MAJrRoData.FXP') WITH IIF(loFormSet.lcPType $ 'G','6','5'),;
    loFormset.lcInvType,EVALUATE(loFormSet.lcTmpLine+'.Style'),;
    EVALUATE(loFormSet.lcTmpLine+'.Dyelot'),EVALUATE(loFormSet.lcTmpLine+'.cWareCode'),;
    "",aAdjStk,loFormSet.lcTmpJour,loFormSet.lcFullRoll,;
    loFormSet.lcTmpRoll,ldTranDate ,ldPostDate


ELSE
  DO (oAriaApplication.ApplicationHome+'MAJrRoData.FXP') WITH IIF(loFormSet.lcPType $ 'G','6','5'),;
    loFormset.lcInvType,EVALUATE(loFormSet.lcTmpLine+'.Style'),;
    EVALUATE(loFormSet.lcTmpLine+'.Dyelot'),EVALUATE(loFormSet.lcTmpLine+'.cWareCode'),;
    "",aAdjStk,loFormSet.lcTmpJour,loFormSet.lcFullRoll,;
    loFormSet.lcTmpRoll,ldTranDate ,ldPostDate
ENDIF
*! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[End]
REPLACE TotStk WITH lnJSgn*aAdjStk[1] IN (loFormSet.lcTmpLine)
*! B609377,1 MMT 08/09/2010 Material PO Receiving-rolls not matching total[START]
lnOldAlis = SELECT(0)
SELECT(loFormSet.lcTmpLine)
lcLineKey = cCarton+Po+STYLE+Dyelot+PADR(cWareCode,6)+STR(LINENO,6)
lcCurFilt = SET("Filter")
SET FILTER TO
IF SEEK('2'+lcLineKey)
  REPLACE Qty1 WITH  lnJSgn*aAdjStk[1] ,;
    TotQty WITH  lnJSgn*aAdjStk[1]
ENDIF
SET FILTER TO &lcCurFilt.
=SEEK('1'+lcLineKey)
SELECT(lnOldAlis)
*! B609377,1 MMT 08/09/2010 Material PO Receiving-rolls not matching total[End]
*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]


*!*************************************************************
*! Name      : lfGetBomLn()   N000587,1
*! Developer : Wael Ali Mohamed
*! Date      : 12/01/2007
*! Purpose   : Get PO cost sheet lines
*!*************************************************************
*! Call      : None
*!*************************************************************
*! Example   : =lfGetBomLn(loFormSet)
*!*************************************************************
FUNCTION lfGetBomLn
LPARAMETERS loFormSet, lcStyType, lcPo, lcShipNo
lnAlias = SELECT()
*N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
*lcCstShtTyp = IIF(lcStyType = "U","M",IIF(lcStyType = "P","I",lcStyType))
lcCstShtTyp = IIF(lcStyType = "U","M",IIF(lcStyType = "P","I",IIF(lcStyType = "F","T",lcStyType)))
*N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
*B608643,1 WAM 08/05/2008 Get cost from estimated records if some of the POs in the shipment don't have adjusted cost for receiving records
*B608643,1 WAM 08/05/2008 Following lines are commented out
*!*	IF loFormSet.lcPType = 'S' .AND. !EMPTY(lcShipNo)
*!*	  lcTranCd = IIF(loFormSet.lcPType $ 'SUF','3','6')
*!*
*!*	  *: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
*!*	  *lcSqlStatement = "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+ "WHERE cImTyp = '" + lcCstShtTyp +;
*!*	                   "' AND CTYPE ='1' AND CTKTNO IN ("
*!*	  lcSqlStatement = "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+ "WHERE cImTyp = '" + lcCstShtTyp +;
*!*	                   "' AND CTYPE ='2' AND CRSESSION = '' AND CTKTNO IN ("
*!*	  *: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]

*!*	  lcSqlStatement  =  lcSqlStatement+"SELECT PO FROM POSLN WHERE POSLN.cBusDocu = '" + loFormSet.lcBusDoc +;
*!*	                   "' AND POSLN.cStyType = '" + loFormSet.lcWorkOrd +;
*!*	                   "' AND POSLN.ShipNo = '" + lcShipNo + "' AND TranCd ='" + lcTranCd + "')"
*!*	ELSE

*!*	  *: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
*!*	  *lcSqlStatement = "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+"WHERE cImTyp = '" + lcCstShtTyp +;
*!*	                   "' AND cTktNo ='" + lcPO + "' AND CTYPE ='1'"
*!*	  lcSqlStatement = "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+"WHERE cImTyp = '" + lcCstShtTyp +;
*!*	                   "' AND cTktNo ='" + lcPO + "' AND CTYPE ='2' AND CRSESSION = ''"
*!*	  *: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]
*!*	ENDIF
*!*	=lfOpenSql(lcSqlStatement,'BOMLINE',loFormSet.lcMastBomLn, "","",.F.)

*!*	*: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
*!*	SELECT (loFormSet.lcMastBomLn)
*!*	LOCATE
*!*	IF EOF()
*!*	  IF loFormSet.lcPType = 'S' .AND. !EMPTY(lcShipNo)
*!*	    lcTranCd = IIF(loFormSet.lcPType $ 'SUF','3','6')
*!*	    lcSqlStatement = "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+ "WHERE cImTyp = '" + lcCstShtTyp +;
*!*	                   "' AND CTYPE ='1' AND CTKTNO IN ("
*!*	    lcSqlStatement  =  lcSqlStatement+"SELECT PO FROM POSLN WHERE POSLN.cBusDocu = '" + loFormSet.lcBusDoc +;
*!*	                   "' AND POSLN.cStyType = '" + loFormSet.lcWorkOrd +;
*!*	                   "' AND POSLN.ShipNo = '" + lcShipNo + "' AND TranCd ='" + lcTranCd + "')"
*!*	  ELSE
*!*	    lcSqlStatement = "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+"WHERE cImTyp = '" + lcCstShtTyp +;
*!*	                   "' AND cTktNo ='" + lcPO + "' AND CTYPE ='1'"
*!*	  ENDIF
*!*	  =lfOpenSql(lcSqlStatement,'BOMLINE',loFormSet.lcMastBomLn, "","",.F.)
*!*	ELSE
*!*	  loFormSet.lladdbomline = .T.
*!*	ENDIF
*!*	*: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]

*B608643,1 WAM 08/05/2008 Get cost from estimated records if some of the POs in the shipment don't have adjusted cost for receiving records
IF loFormSet.lcPType = 'S' .AND. !EMPTY(lcShipNo)
  lcTranCd = IIF(loFormSet.lcPType $ 'SUF','3','6')
  lcSqlStatement = "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+ "WHERE cImTyp = '" + lcCstShtTyp +;
    "' AND CTYPE ='2' AND ShipNo = '" + lcShipNo + "' AND CRSESSION = '' AND CTKTNO IN ("
  lcSqlStatement  =  lcSqlStatement+"SELECT PO FROM POSLN WHERE POSLN.cBusDocu = '" + loFormSet.lcBusDoc +;
    "' AND POSLN.cStyType = '" + loFormSet.lcWorkOrd +;
    "' AND POSLN.ShipNo = '" + lcShipNo + "' AND TranCd ='" + lcTranCd + "') UNION "

  lcSqlStatement = lcSqlStatement  + "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+ "WHERE cImTyp = '" + lcCstShtTyp +;
    "' AND CTYPE ='1' AND CTKTNO IN ("
  lcSqlStatement  =  lcSqlStatement+"SELECT PO FROM POSLN WHERE POSLN.cBusDocu = '" + loFormSet.lcBusDoc +;
    "' AND POSLN.cStyType = '" + loFormSet.lcWorkOrd +;
    "' AND POSLN.ShipNo = '" + lcShipNo + "' AND TranCd ='" + lcTranCd + "')"

ELSE
  lcSqlStatement = "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+"WHERE cImTyp = '" + lcCstShtTyp +;
    "' AND cTktNo ='" + lcPO + "' AND CTYPE ='2' AND CRSESSION = '' UNION "

  lcSqlStatement = lcSqlStatement  + "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+"WHERE cImTyp = '" + lcCstShtTyp +;
    "' AND cTktNo ='" + lcPO + "' AND CTYPE ='1'"
ENDIF
=lfOpenSql(lcSqlStatement,'BOMLINE',loFormSet.lcMastBomLn, "","",.F.)
SELECT (loFormSet.lcMastBomLn)
*B609517,1 TMI 02/05/2011 [Start]   make the property lladdbomline = .T. only if there are records have ctype ='2' and empty crsession in lcMastBomLn file
*LOCATE
LOCATE FOR cType ='2' AND EMPTY(cRSession)
*B609517,1 TMI 02/05/2011 [End  ]
IF EOF()
ELSE
  loFormSet.lladdbomline = .T.
ENDIF
*B608643,1 WAM 08/05/2008 (End)

IF loFormSet.lcPType = 'S' .AND. !EMPTY(lcShipNo)
  SELECT (loFormSet.lcMastBomLn)
  REPLACE ALL ShipNo WITH lcShipNo
ENDIF
DIMENSION laIndex[1,2]
laIndex = ''
laIndex[1,1] = 'cImTyp+cType+ShipNo+cTktNo+STR(LineNo,6)'
laIndex[1,2] = loFormSet.lcMastBomLn
=lfSetIndex(loFormSet.lcMastBomLn,@laIndex)
SELECT (lnAlias)


*!*************************************************************
*! Name      : lfBaseCost()   N000587,1
*! Developer : Wael Ali Mohamed
*! Date      : 12/01/2007
*! Purpose   : Get equivalent cost from BOM table for each cost element based on saved currency code, exchange
*!             rate and unit
*!*************************************************************
*! Call      : None
*!*************************************************************
*! Example   : =lfBaseCost(loFormSet)
*!*************************************************************
FUNCTION lfBaseCost

*B608718,1 WAM 10/09/2008 Store budget quantity
*LPARAMETERS loFormSet, lcStyType, lcPO, lcPriceCur, lnPriceRat, lnCurrUnit, lcDutyCur, lnDutyRat, lnDCurUnit
LPARAMETERS loFormSet, lcStyType, lcPO, lcPriceCur, lnPriceRat, lnCurrUnit, lcDutyCur, lnDutyRat, lnDCurUnit, laEstQty
*B608718,1 WAM 10/09/2008 (End)

lnAlias = SELECT()
lnLineNo = EVALUATE(loFormSet.lcTmpLine+'.LineNo')
*! B610107,1 MMT 10/04/2012 Add Foreign currency costing to Material MFG order summary[Start]
*lcCstShtTyp = IIF(lcStyType = "U","M",IIF(lcStyType = "P","I",lcStyType))
lcCstShtTyp = IIF(lcStyType = "U","M",IIF(lcStyType = "P","I",IIF(lcStyType = "F","T",lcStyType)))
*! B610107,1 MMT 10/04/2012 Add Foreign currency costing to Material MFG order summary[End]
*-- Get the information from the BomLine
lcShipNo = PADR(loFormSet.AriaForm1.cntShipment.kbShipNo.keytextbox.VALUE,6)
DIMENSION loFormSet.laECost[7]
STORE 0 TO loFormSet.laECost
*B608760,1 WAM 12/04/2008 Get foreign cost from the adjusted record for receiving
DIMENSION laEstiCost[7]
STORE 0 TO laEstiCost
*B608760,1 WAM 12/04/2008 (End)

SELECT (loFormSet.lcMastBomLn)

*: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
IF SEEK(lcCstShtTyp+'2'+lcShipNo +lcPO+STR(lnLineNo,6))
  SCAN REST WHILE cImTyp+cType+ShipNo+cTktNo+STR(LINENO,6) = lcCstShtTyp+'2'+lcShipNo +lcPO+STR(lnLineNo,6)
    lnBomType = EVALUATE(cBomTyp)
    IF ISNULL(cCurrCode) OR EMPTY(cCurrCode)
      DO CASE
      CASE cCatgTyp = 'P'
        IF loFormSet.llMulCurr AND !EMPTY(lcPriceCur)
          lcCurrCode = lcPriceCur
          lnExRate   = IIF(lnPriceRat=0,1, lnPriceRat)
          lnCurrUnit = IIF(lnCurrUnit=0,1, lnCurrUnit)
        ELSE
          lcCurrCode = oAriaApplication.BaseCurrency
          lnExRate   = 1
          lnCurrUnit = 1
        ENDIF
      CASE INLIST(cCatgTyp,'D','M')
        IF loFormSet.llMulCurr AND !EMPTY(lcDutyCur)
          lcCurrCode = lcDutyCur
          lnExRate   = IIF(lnDutyRat=0,1, lnDutyRat)
          lnCurrUnit = IIF(lnDCurUnit=0,1, lnDCurUnit)
        ELSE
          lcCurrCode = oAriaApplication.BaseCurrency
          lnExRate   = 1
          lnCurrUnit = 1
        ENDIF
      OTHERWISE
        lcCurrCode = oAriaApplication.BaseCurrency
        lnExRate   = 1
        lnCurrUnit = 1
      ENDCASE
    ELSE
      lcCurrCode = cCurrCode
      lnExRate   = IIF(ISNULL(nExRate) OR nExRate=0,1,nExRate)
      lnCurrUnit = IIF(ISNULL(nCurrUnit) OR nCurrUnit=0,1,nCurrUnit)
    ENDIF
    STORE '/' TO lcExSign, lcUntSin
    lcExSign = gfGetExSin(@lcUntSin, lcCurrCode)

    *B608718,1 WAM 10/09/2008 Get cost per selected sizes
    *lnEquCost = UnitCost*UnitQty &lcExSign lnExRate &lcUntSin lnCurrUnit
    lcBomSizes = IIF(EMPTY(cSizes),'12345678',cSizes)
    lnTransQty  = 0
    FOR lnCntr = 1 TO 8
      IF STR(lnCntr,1) $ lcBomSizes
        lnTransQty = lnTransQty + laEstQty[lnCntr]
      ENDIF
    ENDFOR
    lnEquCost = UnitCost*UnitQty* (lnTransQty/laEstQty[9]) &lcExSign lnExRate &lcUntSin lnCurrUnit
    *B608718,1 WAM 10/09/2008 (End)

    *B608760,1 WAM 12/04/2008 Get foreign cost from the adjusted record for receiving
    laEstiCost[lnBomType]  = laEstiCost[lnBomType] + UnitCost*UnitQty* (lnTransQty/laEstQty[9])
    *B608760,1 WAM 12/04/2008 (End)

    loFormSet.laECost[lnBomType] = loFormSet.laECost[lnBomType] + lnEquCost
    lnCrRt1 = IIF(cCatgTyp = 'P',lnExRate,lnCrRt1)
  ENDSCAN
ELSE
  *: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]
  =SEEK(lcCstShtTyp+'1'+lcShipNo +lcPO+STR(lnLineNo,6))
  SCAN REST WHILE cImTyp+cType+ShipNo+cTktNo+STR(LINENO,6) = lcCstShtTyp+'1'+lcShipNo +lcPO+STR(lnLineNo,6)
    lnBomType = EVALUATE(cBomTyp)
    IF ISNULL(cCurrCode) OR EMPTY(cCurrCode)
      DO CASE
      CASE cCatgTyp = 'P'
        IF loFormSet.llMulCurr AND !EMPTY(lcPriceCur)
          lcCurrCode = lcPriceCur
          lnExRate   = IIF(lnPriceRat=0,1, lnPriceRat)
          lnCurrUnit = IIF(lnCurrUnit=0,1, lnCurrUnit)
        ELSE
          lcCurrCode = oAriaApplication.BaseCurrency
          lnExRate   = 1
          lnCurrUnit = 1
        ENDIF
      CASE INLIST(cCatgTyp,'D','M')
        IF loFormSet.llMulCurr AND !EMPTY(lcDutyCur)
          lcCurrCode = lcDutyCur
          lnExRate   = IIF(lnDutyRat=0,1, lnDutyRat)
          lnCurrUnit = IIF(lnDCurUnit=0,1, lnDCurUnit)
        ELSE
          lcCurrCode = oAriaApplication.BaseCurrency
          lnExRate   = 1
          lnCurrUnit = 1
        ENDIF
      OTHERWISE
        lcCurrCode = oAriaApplication.BaseCurrency
        lnExRate   = 1
        lnCurrUnit = 1
      ENDCASE
    ELSE
      lcCurrCode = cCurrCode
      lnExRate   = IIF(ISNULL(nExRate) OR nExRate=0,1,nExRate)
      lnCurrUnit = IIF(ISNULL(nCurrUnit) OR nCurrUnit=0,1,nCurrUnit)
    ENDIF
    STORE '/' TO lcExSign, lcUntSin
    lcExSign = gfGetExSin(@lcUntSin, lcCurrCode)

    *B608718,1 WAM 10/09/2008 Get cost per selected sizes
    *lnEquCost = UnitCost*UnitQty &lcExSign lnExRate &lcUntSin lnCurrUnit
    lcBomSizes = IIF(EMPTY(cSizes),'12345678',cSizes)
    lnTransQty  = 0
    FOR lnCntr = 1 TO 8
      IF STR(lnCntr,1) $ lcBomSizes
        lnTransQty = lnTransQty + laEstQty[lnCntr]
      ENDIF
    ENDFOR
    lnEquCost = UnitCost*UnitQty* (lnTransQty/laEstQty[9]) &lcExSign lnExRate &lcUntSin lnCurrUnit
    *B608718,1 WAM 10/09/2008 (End)

    *B608760,1 WAM 12/04/2008 Get foreign cost from the adjusted record for receiving
    laEstiCost[lnBomType]  = laEstiCost[lnBomType] + UnitCost*UnitQty* (lnTransQty/laEstQty[9])
    *B608760,1 WAM 12/04/2008 (End)

    loFormSet.laECost[lnBomType] = loFormSet.laECost[lnBomType] + lnEquCost
    lnCrRt1 = IIF(cCatgTyp = 'P',lnExRate,lnCrRt1)
  ENDSCAN

  *: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
ENDIF
*: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]
SELECT (lnAlias)


*B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[Start]
*!*************************************************************
*! Name     : lfUpdBom
*! Developer: Mariam Mazhar
*! Date     : 07/08/2008
*! Purpose  : Function to update po bill of material.
*!*************************************************************
FUNCTION lfUpdBom
PARA lcAction
PRIVATE lnAlias

lnAlias  = SELECT(0)
llRetVal = .T.
lcShtTyp = IIF(llMFCall,'M','I')
lcShtKey = &lcPoshdrAlias..Po
IF !SEEK(lcShtTyp +lcShtKey ,lcCtktBomAlias)
  gfSeek(lcShtTyp +lcShtKey ,lcCtktBomAlias)
ENDIF

lcLinkCode= EVALUATE(lcCtktBomAlias+'.Link_Code')
lcStyle   = &lcTmpLine..STYLE
lnPoLNo   = IIF(llMFCall ,0,&lcTmpLine..LINENO)

lcShtDye  = &lcTmpLine..Dyelot

lnPrice   = &lcTmpLine..nfCost1

lcLastOpr = IIF(!EMPTY(&lcPoshdrAlias..cLastOpr),&lcPoshdrAlias..cLastOpr,&lcTmpLine..cLastOpr)

lcItmWare = &lcPoshdrAlias..cItemWare
lcMatWare = &lcPoshdrAlias..cMatWare

SELECT (lcTmpLine)
SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laBomQty
laBomQty[9] = 0

FOR lnSize = 1 TO 8
  laBomQty[9] = laBomQty[9] + laBomQty[lnSize]
ENDFOR

*! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [Start]
*!*	llRetVal = gfSheetItem (lcShtTyp ,lcShtKey ,lcLinkCode,lcStyle ,;
*!*	                        lcInvType    , lnPoLNo   , lcShtDye , lcItmWare   ,;
*!*	                        lcMatWare, @laBomQty,  lcBomAlias , lcCtktBomAlias      ,;
*!*	                        lcBomLineAlias, lcMFGOprHdAlias, @lcLastOpr , lnPrice   ,;
*!*	                        0, 0, 0, 0, 0,0,0,lcPoshdrAlias ,lcPoslnAlias  ,;
*!*	                          lcTmpFileNam,.T.)
llRetVal = gfSheetItem (lcShtTyp ,lcShtKey ,lcLinkCode,lcStyle ,;
  lcInvType    , lnPoLNo   , lcShtDye , lcItmWare   ,;
  lcMatWare, @laBomQty,  lcBomAlias , lcCtktBomAlias      ,;
  lcBomLineAlias, lcMFGOprHdAlias, @lcLastOpr , lnPrice   ,;
  0, 0, 0, 0, 0,0,0,lcTmpPoshdr,lcPoslnAlias  ,;
  lcTmpFileNam,.T.)
REPLACE CLASTOPR WITH &lcTmpPoshdr..CLASTOPR IN (lcPoshdrAlias)
*! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [END]


SELECT (lcTmpLine)
lnTmpRecNo = RECNO(lcTmpLine)
REPLACE lCostMade WITH llRetVal FOR LINENO = lnPoLNo
IF (lnTmpRecNo # 0) AND (lnTmpRecNo <= RECCOUNT())
  GOTO lnTmpRecNo
ENDIF
SELECT (lnAlias)

*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
RELEASE lnAlias,lcShtTyp ,lcShtKey ,lcLinkCode,lcStyle   ,lnPoLNo   ,lcShtDye  ,lnPrice   ,lcLastOpr ,lcItmWare ,lcMatWare ,laBomQty,lnSize ,lnTmpRecNo
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]


RETURN (llRetVal)
*B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[End]

*:*******************************************************************************
*! Name : lfRmvDupl
*! Auth : Tarek Mohammed Ibrahim
*! *B609497,1 TMI 02/07/2011
*! To remove duplicated lines from the BOMLINE temp file based on the unique key index
** Parameters Passed
* lcAlias    : the temp alias name to remove duplicates from
* lcKeyFld   : the key of the uniuqe index
* lcFldToUpd : the numeric field to increment to overcome the duplication
*:*******************************************************************************
*B609517,1 TMI 02/08/2011
FUNCTION lfRmvDupl
LPARAMETERS lcAlias,lcKeyFld,lcFldToUpd
LOCAL lnSlct,lcOldOrd,laDist,lnI,lnCnt,lcTmpAls

lnSlct = SELECT(0)

lcTmpAls = gfTempName()
SELECT (lcAlias)
lcOldOrd = ORDER()
SET ORDER TO
LOCATE
COPY TO (oAriaApplication.WorkDir+lcTmpAls)
DIME laDist[1]
laDist = ''
SELECT DISTINCT &lcKeyFld FROM (oAriaApplication.WorkDir+lcTmpAls+'.dbf') INTO ARRAY laDist
IF !EMPTY(laDist[1])
  FOR lnI = 1 TO ALEN(laDist,1)
    lnCnt = 0
    LOCATE
    SCAN FOR &lcKeyFld = laDist[lnI]
      lnCnt = lnCnt + 1
      IF lnCnt = 1
        SCATTER MEMVAR MEMO
      ELSE
        REPLACE &lcFldToUpd WITH m.&lcFldToUpd.+lnCnt-1
      ENDIF
    ENDSCAN
  ENDFOR
ENDIF
IF USED(lcTmpAls)
  USE IN &lcTmpAls
ENDIF
ERASE (oAriaApplication.WorkDir+lcTmpAls+'.*')

SELECT &lcAlias
SET ORDER TO &lcOldOrd
SELECT (lnSlct)

*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
*!*************************************************************
*! Name      : lfSaveBatch
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : Save Batch
*!*************************************************************
FUNCTION lfSaveBatch
LPARAMETERS loFormSet
lcInvType = loFormSet.lcInvType
lcPType = loFormSet.lcPType
lcTmpLine = loFormSet.lcTmpLine
ldDate = loFormSet.ariaForm1.dtPickerPostingDate.VALUE
llMFCall = loFormSet.llMFCall
llImpCost = loFormSet.llImpCost
lcRecvType = loFormSet.lcPType
IF lcPType = 'I'
  SELECT (lcTmpLine)
  SET FILTER TO
  LOCATE FOR LNEWLN
  IF FOUND()
    lfAddNewLn()
  ENDIF
ENDIF
lcDelSt = SET("Deleted")
SET DELETED OFF
SELECT  (lcTmpLine)
lcOldOrd = ORDER()
SET ORDER TO 'TmpLine2'
*Add Mode
IF EMPTY(loFormSet.ariaForm1.cntBatch.kbBatchNo.keytextbox.VALUE)
  IF !USED('CTKTRCVL')
    =gfOpenTable('CTKTRCVL','CTKTRCVL')
  ENDIF
  IF !USED('CTKTRCVH')
    =gfOpenTable('CTKTRCVH','CTKTRCVH')
  ENDIF
  lcNewRec = gfsequence('TMPRCVNUM')
  SELECT CTKTRCVH
  APPEND BLANK
  *! E303177,1 HIA 07/28/2012 Get the batch creation date from the screen as per specs [T20120618.0015]  [Begin]
  *REPLACE ctype WITH IIF(loFormSet.llMfCall,'M',IIF(lcPType  = 'O','N','I')) ,;
  *        carton WIth .F.,;
  *        cdesc WITH '',;
  *        cstatus WITH 'O',;
  *        cwarecode WIth '',;
  *        ddate  with oAriaApplication.systemdate,;
  *        ntotcan with 0,;
  *        ntotdam WiTh 0,;
  *        ntotstk WITH 0,;
  *        shipno  WITH '',;
  *        tmprcvnum with lcNewRec,;
  *        vendor  With  ''

  REPLACE ctype WITH IIF(loFormSet.llMfCall,'M',IIF(lcPType  = 'O','N','I')) ,;
    carton WITH .F.,;
    cdesc WITH '',;
    cstatus WITH 'O',;
    cwarecode WITH '',;
    ddate  WITH IIF(TYPE('ldDate')='D',ldDate,oAriaApplication.systemdate),;
    ntotcan WITH 0,;
    ntotdam WITH 0,;
    ntotstk WITH 0,;
    shipno  WITH '',;
    tmprcvnum WITH lcNewRec,;
    vendor  WITH  ''
  *! E303177,1 HIA 07/28/2012 Get the batch creation date from the screen as per specs [T20120618.0015]  [End]
  SELECT (lcTmpLine)
  SET FILTER TO
  lnBatchLine = 0
  lnLineNo = 0
  lcPO = ''
  SCAN FOR TRANCD $ '245' AND TOTQTY >0
    IF (lnLineNo <> &lcTmpLine..LINENO) OR (lcPO <> &lcTmpLine..PO)
      lnBatchLine = lnBatchLine + 1
    ENDIF
    SELECT CTKTRCVL
    APPEND BLANK
    REPLACE ccarton WITH '' ,;
      cretsty WITH &lcTmpLine..cretsty,;
      cstygrade WITH &lcTmpLine..cstygrade,;
      ctype WITH IIF(loFormSet.llMfCall,'M',IIF(lcPType  = 'O','N','I'))  ,;
      cuttkt WITH &lcTmpLine..PO,;
      cwarecode WITH &lcTmpLine..CWARECODE,;
      dyelot WITH &lcTmpLine..DYELOT,;
      FLAG WITH 'A',;
      LINENO  WITH &lcTmpLine..LINENO,;
      nlineno WITH lnBatchLine,;
      qty1 WITH &lcTmpLine..qty1 ,;
      qty2 WITH &lcTmpLine..qty2 ,;
      qty3 WITH &lcTmpLine..qty3 ,;
      qty4 WITH &lcTmpLine..qty4 ,;
      qty5 WITH &lcTmpLine..qty5 ,;
      qty6 WITH &lcTmpLine..qty6 ,;
      qty7 WITH &lcTmpLine..qty7 ,;
      qty8 WITH &lcTmpLine..qty8 ,;
      REFERENCE WITH &lcTmpLine..REFERENCE ,;
      STYLE WITH &lcTmpLine..STYLE ,;
      tmprcvnum WITH lcNewRec,;
      totqty WITH &lcTmpLine..totqty ,;
      trancd WITH &lcTmpLine..TRANCD
    =gfAdd_Info('CTKTRCVL')
    =gfReplace("")
    DO CASE
    CASE TRANCD  ='2'
      REPLACE  ntotstk WITH ntotstk+&lcTmpLine..totqty  IN 'CTKTRCVH'
    CASE TRANCD  ='4'
      REPLACE ntotdam WITH ntotdam+&lcTmpLine..totqty IN 'CTKTRCVH'
    CASE TRANCD  ='5'
      REPLACE  ntotcan WITH ntotcan +&lcTmpLine..totqty IN 'CTKTRCVH'
    ENDCASE
    lnLineNo = &lcTmpLine..LINENO
    lcPO = &lcTmpLine..PO
  ENDSCAN

  IF loFormSet.llApproveBatch AND gfModalGen('QRM34062B34001','DIALOG')=1
    SELECT CTKTRCVH
    REPLACE cStatus WITH 'A'
  ENDIF
  SELECT CTKTRCVH
  =gfAdd_Info('CTKTRCVH')
  =gfReplace("")
  =gfTableUpdate()
  SELECT CTKTRCVL
  =gfTableUpdate()
  =gfModalGen('INM42085B42000','DIALOG',LANG_POSTREC_TMPRECBATCH+'|'+lcNewRec)
ELSE
  *Edit Mode
  lcBatchNo = loFormSet.ariaForm1.cntBatch.kbBatchNo.keytextbox.VALUE
  SELECT CTKTRCVH
  =gfSeek(IIF(loFormSet.llMfCall,'M',IIF(loFormSet.lcPType $ 'LHO','N','I'))+lcBatchNo)
  SELECT CTKTRCVL
  =gfSeek(IIF(loFormSet.llMfCall,'M',IIF(loFormSet.lcPType $ 'LHO','N','I'))+lcBatchNo)
  SELECT MAX(nlinenO) FROM CTKTRCVL INTO ARRAY laLastLine
  IF _TALLY > 0
    lnBatchLine =  laLastLine[1]
  ENDIF
  SELECT (lcTmpLine)
  SET FILTER TO
  lnLineNum = 0
  lnLineNo = 0
  lcPO = ''
  SCAN FOR TRANCD $ '245'
    IF &lcTmpLine..TOTQTY > 0 AND !SEEK(IIF(loFormSet.llMfCall,'M',IIF(lcPType  = 'O','N','I')) +lcBatchNo +&lcTmpLine..PO+&lcTmpLine..STYLE+&lcTmpLine..DYELOT+&lcTmpLine..CCARTON+STR(&lcTmpLine..NLINENO,6)+STR(&lcTmpLine..LINENO,6)+&lcTmpLine..TRANCD++&lcTmpLine..CSTYGRADE,'CTKTRCVL')
      IF !SEEK(IIF(loFormSet.llMfCall,'M',IIF(lcPType  = 'O','N','I')) +lcBatchNo +&lcTmpLine..PO+&lcTmpLine..STYLE+&lcTmpLine..DYELOT+&lcTmpLine..CCARTON+STR(&lcTmpLine..NLINENO,6)+STR(&lcTmpLine..LINENO,6),'CTKTRCVL')
        IF lnLineNo <> &lcTmpLine..LINENO OR (lcPO <> &lcTmpLine..PO)
          lnBatchLine = lnBatchLine + 1
          lnLineNum = lnBatchLine
        ENDIF
      ELSE
        lnLineNum = &lcTmpLine..NLINENO
      ENDIF
      SELECT CTKTRCVL
      APPEND BLANK
      REPLACE ccarton WITH '' ,;
        cretsty WITH &lcTmpLine..cretsty,;
        cstygrade WITH &lcTmpLine..cstygrade,;
        ctype WITH IIF(loFormSet.llMfCall,'M',IIF(loFormSet.lcPType $ 'LHO','N','I')) ,;
        cuttkt WITH &lcTmpLine..PO,;
        cwarecode WITH &lcTmpLine..CWARECODE,;
        dyelot WITH &lcTmpLine..DYELOT,;
        FLAG WITH 'A',;
        LINENO  WITH &lcTmpLine..LINENO,;
        nlineno WITH lnLineNum ,;
        qty1 WITH &lcTmpLine..qty1 ,;
        qty2 WITH &lcTmpLine..qty2 ,;
        qty3 WITH &lcTmpLine..qty3 ,;
        qty4 WITH &lcTmpLine..qty4 ,;
        qty5 WITH &lcTmpLine..qty5 ,;
        qty6 WITH &lcTmpLine..qty6 ,;
        qty7 WITH &lcTmpLine..qty7 ,;
        qty8 WITH &lcTmpLine..qty8 ,;
        REFERENCE WITH &lcTmpLine..REFERENCE ,;
        STYLE WITH &lcTmpLine..STYLE ,;
        tmprcvnum WITH lcBatchNo ,;
        totqty WITH &lcTmpLine..totqty ,;
        trancd WITH &lcTmpLine..TRANCD
      =gfAdd_Info('CTKTRCVL')
      =gfReplace("")
      DO CASE
      CASE TRANCD  ='2'
        REPLACE  ntotstk WITH ntotstk+&lcTmpLine..totqty  IN 'CTKTRCVH'
      CASE TRANCD  ='4'
        REPLACE ntotdam WITH ntotdam+&lcTmpLine..totqty IN 'CTKTRCVH'
      CASE TRANCD  ='5'
        REPLACE  ntotcan WITH ntotcan +&lcTmpLine..totqty IN 'CTKTRCVH'
      ENDCASE
      lnLineNo = &lcTmpLine..LINENO
      lcPO =&lcTmpLine..PO
    ELSE
      SELECT CTKTRCVL
      lnOldQty = CtktRcvl.TotQTy
      IF !DELETED(lcTmpLine) AND  &lcTmpLine..TOTQTY > 0
        REPLACE qty1 WITH &lcTmpLine..qty1 ,;
          qty2 WITH &lcTmpLine..qty2 ,;
          qty3 WITH &lcTmpLine..qty3 ,;
          qty4 WITH &lcTmpLine..qty4 ,;
          qty5 WITH &lcTmpLine..qty5 ,;
          qty6 WITH &lcTmpLine..qty6 ,;
          qty7 WITH &lcTmpLine..qty7 ,;
          qty8 WITH &lcTmpLine..qty8 ,;
          totqty WITH &lcTmpLine..totqty ,;
          cwarecode WITH &lcTmpLine..CWARECODE,;
          cretsty WITH &lcTmpLine..cretsty
        =gfAdd_Info('CTKTRCVL')
        =gfReplace("")
        DO CASE
        CASE TRANCD  ='2'
          REPLACE  ntotstk WITH ntotstk+&lcTmpLine..totqty-lnOldQty   IN 'CTKTRCVH'
        CASE TRANCD  ='4'
          REPLACE ntotdam WITH ntotdam+&lcTmpLine..totqty-lnOldQty  IN 'CTKTRCVH'
        CASE TRANCD  ='5'
          REPLACE  ntotcan WITH ntotcan +&lcTmpLine..totqty-lnOldQty  IN 'CTKTRCVH'
        ENDCASE
      ELSE

        DO CASE
        CASE TRANCD  ='2'
          REPLACE  ntotstk WITH ntotstk-lnOldQty   IN 'CTKTRCVH'
        CASE TRANCD  ='4'
          REPLACE ntotdam WITH ntotdam-lnOldQty  IN 'CTKTRCVH'
        CASE TRANCD  ='5'
          REPLACE  ntotcan WITH ntotcan-lnOldQty  IN 'CTKTRCVH'
        ENDCASE
        =gfDelete()
      ENDIF
    ENDIF
  ENDSCAN
  SELECT CTKTRCVH
  REPLACE cdesc   WITH loFormSet.ariaForm1.cntBatch.txtBatchDesc.VALUE,;
    cstatus WITH loFormSet.ariaForm1.cntBatch.cboBatchStatus.VALUE,;
    ddate   WITH ldDate
  =gfAdd_Info('CTKTRCVH')
  =gfReplace("")
  =gfTableUpdate()
  SELECT CTKTRCVL
  =gfTableUpdate()
ENDIF
SELECT  (lcTmpLine)
SET ORDER TO (lcOldOrd)

SET DELETED &lcDelSt.
*!*************************************************************
*! Name      : lfCancelBatch
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : Cancel Batch
*!*************************************************************
FUNCTION lfCancelBatch
LPARAMETERS loFormSet
*=lfSaveBatch(loFormSet)
IF gfModalGen('INM00002B34001','ALERT',IIF(loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE <> 'X',LANG_POSTREC_BATCH_CANCELREC,LANG_POSTREC_BATCH_UNCANCELREC))<>1
  RETURN
ENDIF
lcBatchNo = loFormSet.ariaForm1.cntBatch.kbBatchNo.keytextbox.VALUE
SELECT CTKTRCVH
=gfSeek(IIF(loFormSet.llMfCall,'M',IIF(loFormSet.lcPType $ 'LHO','N','I'))+lcBatchNo)
IF loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE = 'O' OR (loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE = 'A' AND loFormSet.llApproveBatch) && Cancel
  REPLACE cStatus WITH 'X'
  loFormSet.oToolBar.cmdadd.ENABLED =.F.
  DIMENSION loFormSet.laStatusArr[4,2]
  loFormSet.laStatusArr[1,1]  = LANG_POSTREC_STATUS_OPEN
  loFormSet.laStatusArr[1,2]  = LANG_POSTREC_STATUS_O
  loFormSet.laStatusArr[2,1]  = LANG_POSTREC_STATUS_APPROVED
  loFormSet.laStatusArr[2,2]  = LANG_POSTREC_STATUS_A
  loFormSet.laStatusArr[3,1]  = LANG_POSTREC_STATUS_CANCEL
  loFormSet.laStatusArr[3,2]  = LANG_POSTREC_STATUS_X
  loFormSet.laStatusArr[4,1]  = LANG_POSTREC_STATUS_POSTED
  loFormSet.laStatusArr[4,2]  = LANG_POSTREC_STATUS_P
  loFormSet.AriaForm1.CntBatch.cboBatchStatus.REQUERY()
  loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE = 'X'

ELSE
  IF loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE = 'X' && UnCancel
    REPLACE cStatus WITH 'O'
    loFormSet.oToolBar.cmdadd.ENABLED =.T.
    DIMENSION loFormSet.laStatusArr[2,2]
    loFormSet.laStatusArr[1,1]  = LANG_POSTREC_STATUS_OPEN
    loFormSet.laStatusArr[1,2]  = LANG_POSTREC_STATUS_O
    loFormSet.laStatusArr[2,1]  = LANG_POSTREC_STATUS_APPROVED
    loFormSet.laStatusArr[2,2]  = LANG_POSTREC_STATUS_A
    loFormSet.AriaForm1.CntBatch.cboBatchStatus.REQUERY()
    loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE = 'O'
  ENDIF
ENDIF



=gfReplace('')
=gfTableUpdate()
loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE = CTKTRCVH.cStatus

=lfShowScr(loFormSet)
loFormSet.Refreshall()
*!*************************************************************
*! Name      : lfCheckBatch
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : Check Batch Status
*!*************************************************************
FUNCTION lfCheckBatch
LPARAMETERS loFormSet
IF loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE =  'X'
  = gfModalGen('TRM34070B42000','DIALOG',LANG_POSTREC_ISCANCELLED)
  RETURN .F.
ENDIF
IF loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE = 'P'
  *--This temporary receiving batch is posted. Cannot proceed.
  = gfModalGen('TRM34070B42000','DIALOG',LANG_POSTREC_ISPOSTED)
  RETURN .F.
ENDIF
IF loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE <>  'A'
  = gfModalGen('TRM34070B42000','DIALOG',LANG_POSTREC_ISNOTAPPROVED)
  RETURN .F.
ENDIF
*!*************************************************************
*! Name      : lfUpdBatchSt
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : Update Batch Status
*!*************************************************************
FUNCTION lfUpdBatchSt
LPARAMETERS loFormSet
SELECT CTKTRCVH
lcBatchNo = loFormSet.ariaForm1.cntBatch.kbBatchNo.keytextbox.VALUE
=gfSeek(IIF(loFormSet.llMfCall,'M',IIF(loFormSet.lcPType $ 'LHO','N','I'))+lcBatchNo)
REPLACE cStatus WITH 'P'
=gfReplace('')
=gfTableUpdate()
*!*************************************************************
*! Name      : lfShowErrorLog
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : Show Error Log
*!*************************************************************
FUNCTION lfShowErrorLog
SELECT TMPSTR
*! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome + 'POBTLOG.SCX')
=gfCallForm('POBTLOG')
*! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[End]
USE IN TMPSTR
*!*************************************************************
*! Name      : lfUpdScanBatch
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : To Update Imported Scan Batch
*!*************************************************************
FUNCTION lfUpdScanBatch
LPARAMETERS loFormSet
IF !USED('POSHDR')
  =gfOpenTable('POSHDR','POSHDR')
ENDIF
*! B609728,1 MMT 11/14/2011 Fix bug of Freezing in Scan barcode screen after using Scanned batch in PO Screen[Start]
LOCAL lnConnHandler,lnUpSt
lnConnHandler = 0
lnUpSt = 0
*! B609728,1 MMT 11/14/2011 Fix bug of Freezing in Scan barcode screen after using Scanned batch in PO Screen[End]
FOR lnT = 1 TO ALEN(loFormSet.laUsedBatchPO,1)
  =gfSeek(loFormSet.lcBusDoc+loFormSet.lcWorkOrd+loFormSet.laUsedBatchPO[lnT ,2],'POSHDR')
  lcUpDStat = "Update SCAN_BATCH_HEADER_T Set TRANSACTION_TYPE ='"+;
    loFormSet.lcBusDoc+loFormSet.lcWorkOrd +"',TRANSACTION_NO = '"+loFormSet.laUsedBatchPO[lnT ,2]+;
    "',VENDOR = '"+poshdr.VENDOR+"',Status ='C' Where SCAN_BATCH_HEADER_KEY = '"+loFormSet.laUsedBatchPO[lnT ,1]+"'"
  *! B609728,1 MMT 11/14/2011 Fix bug of Freezing in Scan barcode screen after using Scanned batch in PO Screen[Start]
  *!*	  lnUpSt = oAriaApplication.RemoteCompanyData.execute(lcUpDStat ,'',;
  *!*	                                                      "SCAN_BATCH_HEADER_T ","",oAriaApplication.activecompanyconstr ,3,"",SET("Datasession"))
  lnUpSt = oAriaApplication.RemoteCompanyData.execute(lcUpDStat ,'',;
    "SCAN_BATCH_HEADER_T ","",oAriaApplication.activecompanyconstr ,3,"",SET("Datasession"),.T., @lnConnHandler)
  IF lnUpSt <= 0
    EXIT
  ENDIF
  *! B609728,1 MMT 11/14/2011 Fix bug of Freezing in Scan barcode screen after using Scanned batch in PO Screen[End]
ENDFOR
*! B609728,1 MMT 11/14/2011 Fix bug of Freezing in Scan barcode screen after using Scanned batch in PO Screen[Start]
IF lnUpSt > 0
  SQLCOMMIT(lnConnHandler)
ELSE
  =oAriaApplication.RemoteCompanyData.CheckRetResult("Execute",lnResult,.T.)
  SQLROLLBACK(lnConnHandler)
ENDIF
*! B609728,1 MMT 11/14/2011 Fix bug of Freezing in Scan barcode screen after using Scanned batch in PO Screen[End]
lcBatDet = loFormSet.lcbatchdet
IF USED(lcBatDet)
  *!*  B609728,1 MMT 11/14/2011 Fix bug of Freezing in Scan barcode screen after using Scanned batch in PO Screen[Start]
  lnConnHandler = 0
  lnUpSt = 0
  SELECT (lcBatDet)
  LOCATE FOR !DELETED()
  IF EOF()
    RETURN
  ENDIF
  LOCATE
  *!*  B609728,1 MMT 11/14/2011 Fix bug of Freezing in Scan barcode screen after using Scanned batch in PO Screen[End]
  SELECT (lcBatDet)
  SCAN FOR !EMPTY(ALLTRIM(Rejection_reason)) AND !DELETED()
    lcUpDDet =  "Update SCAN_BATCH_DETAILS_T Set Status ='R',Rejection_reason = '"+;
      &lcBatDet..Rejection_reason +"' Where SCAN_BATCH_HEADER_KEY = '"+;
      &lcBatDet..SCAN_BATCH_HEADER_KEY+"' AND SCAN_BATCH_DETAILS_KEY = '"+&lcBatDet..SCAN_BATCH_DETAILS_KEY+"'"
    *! B609728,1 MMT 11/14/2011 Fix bug of Freezing in Scan barcode screen after using Scanned batch in PO Screen[Start]
    *!*	    lnUpSt = oAriaApplication.RemoteCompanyData.execute(lcUpDDet ,'',;
    *!*	                                                          "SCAN_BATCH_DETAILS_TMP","",oAriaApplication.activecompanyconstr ,3,"",SET("Datasession"))
    lnUpSt = oAriaApplication.RemoteCompanyData.execute(lcUpDDet ,'',;
      "SCAN_BATCH_DETAILS_TMP","",oAriaApplication.activecompanyconstr ,3,"",SET("Datasession"),.T., @lnConnHandler)
    IF lnUpSt <= 0
      EXIT
    ENDIF
    *! B609728,1 MMT 11/14/2011 Fix bug of Freezing in Scan barcode screen after using Scanned batch in PO Screen[End]
  ENDSCAN
  *!*  B609728,1 MMT 11/14/2011 Fix bug of Freezing in Scan barcode screen after using Scanned batch in PO Screen[Start]
  IF lnUpSt > 0
    SQLCOMMIT(lnConnHandler)
  ELSE
    =oAriaApplication.RemoteCompanyData.CheckRetResult("Execute",lnResult,.T.)
    SQLROLLBACK(lnConnHandler)
  ENDIF
  *!*  B609728,1 MMT 11/14/2011 Fix bug of Freezing in Scan barcode screen after using Scanned batch in PO Screen[END]
ENDIF
*!*************************************************************
*! Name      : lfRefshAll
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : Refresh screen
*!*************************************************************
FUNCTION lfRefshAll
LPARAMETERS loFormSet
IF loFormSet.lcPType $'IOM'
  lnButtonNo = 0
  FOR lnCount = 1 TO loFormSet.oToolBar.CONTROLCOUNT
    IF UPPER(loFormSet.oToolBar.CONTROLS(lnCount).CLASS) = 'TOOLBARCUSTOMBUTTON' AND ;
        UPPER(loFormSet.oToolBar.CONTROLS(lnCount).CustomName) = UPPER('cmdDelBatch')
      lnButtonNo  = lnCount
      EXIT
    ENDIF
  ENDFOR


  IF !EMPTY(loFormSet.ariaFORM1.cnTBATCH.kbBATCHNO.keYTEXTBOX.VALUE) AND !(loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE $ 'P') AND;
      (loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE <> 'P' AND  IIF(loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE = 'A',loFormSet.llApproveBatch,.T.))
    loFormSet.oToolBar.ChangeButtonStatus('cmdDelBatch','ENABLED')
    IF loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE = 'X'
      loFormSet.oToolBar.CONTROLS(lnButtonNo).PICTURE = oAriaApplication.BitmapHome+"UNTRASH.BMP"
      loFormSet.oToolBar.CONTROLS(lnButtonNo).STATUSBARTEXT = LANG_POSTREC_BATCH_UNCANCELBATCH
      loFormSet.oToolBar.CONTROLS(lnButtonNo).TOOLTIPTEXT = LANG_POSTREC_BATCH_UNCANCELBATCH
    ELSE
      loFormSet.oToolBar.CONTROLS(lnButtonNo).PICTURE = oAriaApplication.BitmapHome+"TRASH.BMP"
      loFormSet.oToolBar.CONTROLS(lnButtonNo).STATUSBARTEXT = LANG_POSTREC_BATCH_CANCBATCH
      loFormSet.oToolBar.CONTROLS(lnButtonNo).TOOLTIPTEXT = LANG_POSTREC_BATCH_CANCBATCH
    ENDIF
  ELSE
    loFormSet.oToolBar.ChangeButtonStatus('cmdDelBatch','DISABLED')
  ENDIF
ENDIF
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
