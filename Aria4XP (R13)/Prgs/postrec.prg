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
*! B610575,1 MMT 11/04/2013 add option to Print Cut tkt rec. log while receiving Cut.tkt[T20131030.0018]
*! B610897,1 MMT 10/26/2014 Cutting ticket receiving allow user to enter style before entering CT#[T20141022.0011]
*! B610904,1 MMT 11/06/2014 Error in PO receiving screen while using scanned batch option[T20141029.0023]
*! E303627,1 MMT 11/29/2015 Modify the Inter-Location PO receiving to work in silent mode[T20151014.0017]
*! B611276,1 MMT 03/26/2017 Po receving error while receiving by shipment[T20161212.0005]
*! E303848,1 SARA.O 07/09/2017 Saving a ‘Receive by Shipment’ session will allow the user to save the receiving as a batch (The message appear at the save time will have:'Preview Log', 'Post', 'Save' and 'Cancel')
*! B611375,  MHM 8/2/2017 po recieving screen grid size shrunk [T20170720.0009]
*! B611405   MHM error while recive cut ticket
*! B611450,1 HMS ,error still getting after "receive style po" and click post T20171019.0013
*! B611536,1 SAH 02/25/2018 Modify function lfBefSave as the user receive un-Approved batch while receiving by Shipment in PO receiving screen [T20180222.0020]
*! E304047,1 MMT 07/25/2018 Modify Receiving program to update SQL GLDIST table
*! E304047,2 MMT 08/02/2018 Modify Receiving program to update SQL GLDIST table
*! E304063,1 ES 01/22/2019 Client wants to update the Shipment Warehouse date with the shipment actual receiving date. [T20180718.0017].
*! B611657,1 ES 01/23/2019 Receiving program does not inform user that Shipment included POs are completed[T20180723.0178]
*! E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [T20180508.0022]
*! B611745,1 Heba HMS, 18/03/2018 - Aria 5 - Error with purchase order [T20190313.0002 ]
*! B611784,1 MMT 06/12/2019 Fix error while receiving by shipment using temp. Receiving batch[P20171120.0011]
*! B611880,1 Es 08/22/2019  DOUBLE RECEIVING PO [T20190806.0005]
*! B611953,1 ES 10/29/2019 Shipment is not marked as locked once user selected it in PO receiving screen [T20191009.0001]
*! E611838,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement]
*! B612583,1 MMT 06/12/2022 Issue inter-location PO screen does accept PO lines that does not have default Imported style cost sheet even it is not used by the program [T20220607.0003]
*:********************************************************************************
*! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
*PARAMETERS lcInvType, lcPType
Parameters lcInvType, lcPType, lcObjName
*! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
*B611405 MHM error while recive cut ticket 
PRIVATE llRcv 
llRcv = .f.

IF TYPE('lcInvType') = 'L'  AND lcInvType == .f.
llRcv = .t.
ENDIF 
*B611405 MHM error while recive cut ticket
#INCLUDE r:\aria4xp\prgs\postrec.h

*-- Initialize the variables in order to define it as properties in the formset.
Private lcAllVar

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
  "lcClrLstOp,lcWareHFil,lcTmpCurs,llEditLCst,llTrkRolls,lcMastBomLn,llRcv"

*N000587,1 WAM 12/01/2007 (End)

*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [END]

*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
lcAllVar = lcAllVar +  ',llApproveBatch,lcbatchdet,laUsedBatchPO,laStatusArr,Saveorpost'
Dimension laStatusArr[1,2],laUsedBatchPO[1,2]
Store '' To laStatusArr,laUsedBatchPO
Store .F. To llApproveBatch
Store '' To lcbatchdet,Saveorpost
lcbatchdet = gfTempName()
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]

*! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
lcAllVar = lcAllVar + ',llSilentMod'
*! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

Dimension laAllVar[1,1]
Store '' To laAllVar
=gfSubStr(lcAllVar,@laAllVar,',')

Local lnI
For lnI = 1 To Alen(laAllVar,1)
  Private &laAllVar[lnI,1].
Endfor

*-- This paramter is to indicate the receiving type. If calling the program directly from
*-- menu without passing parameters then this means you are receiving style P/O
lcPType   = Iif(Type('lcPType') $ 'UL','I',lcPType)
lcInvType = Iif(Type('lcInvType') $ 'UL','0001',lcInvType)

*--Global program variable flag indicate if it P/O or C/T receive.
llMFCall = ( lcPType $ 'MT' )

*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
llApproveBatch = Iif(llMFCall,gfUserPriv('MF','MFRCVCT   ','APPBTCH'),gfUserPriv('PO','POSTREC','APPBTCH'))
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
*-- Assign the business document and work order type
Store '' To lcBusDoc,lcWorkOrd,lcPoNo,lcStyle,lcDyelot
Dimension laSetups[23,2]
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
llImpCost  = Iif(lcInvType='0002' Or Empty(llImpCost),.F.,llImpCost)
*-- Multi currency
llUseMCurr = laSetups[10,2]
llMulCurr  = Iif(lcPType $ 'DAE',.F.,laSetups[10,2])
*-- Change exch. rates
llEditExRt = laSetups[11,2]
*-- Receive Style PO by PO/Shipment
lcCostImp  = laSetups[12,2]
*-- Cost element type 1
lcIType1   = Iif(lcInvType = '0002','P',laSetups[13,2])
*-- Cost element type 2
lcIType2   = Iif(lcInvType = '0002','D',laSetups[14,2])
*-- Cost element type 3
lcIType3   = Iif(lcInvType = '0002','D',laSetups[15,2])
*-- Cost element type 4
lcIType4   = Iif(lcInvType = '0002','D',laSetups[16,2])
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
llTrkRolls = Alltrim(laSetups[23,2]) = 'Y'
*-- User previlage
llCostPrv  = gfUserPriv('IC','ICSTYLE','COSTING')

*-- Ware house popup, Estimated Cost, Lot quantity in case of PO multi lot
Dimension laWare[1,1],laECost[7,1], laLotArry[8,1]

Store '' To lcGLFYear,lcGLPeriod,lcCur1,lcCur2,laWare,lcOldPoNo,cbrowsefields,lcLotNo,;
  lcClrLstOp
Store 1 To lnCurrUnt1,lnCurrUnt2
*-- Initialize the necessary variables
Store 0 To lnTotStk,lnTotDam,lnTotCan,lnRate1,lnRate2,lnWare,laECost,lnPolstln
*-- 'R' Return Style PO, 'N' Issue inter location PO, 'A' Issue adornment order
*-- 'H' Issue inter location PO batch, 'U' Issue Inter location PO Shipment
llIssue = (lcPType $ 'RNAHUG')

llDyelot = Iif(lcInvType = "0002",llFabDye,llDyelot)

Store .F. To llIgnorAll,llCMInstld,llByCarton,llFirstTmp,llSpecLot,llNewItem,llEditLCst
Store .T. To llFirst
*-- major segment width
lnMjrWid   = Len(gfItemMask('PM',"",lcInvType))
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
llSilentMod = !Empty(lcObjName)
If !llSilentMod
  *! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[Start]
  *DO FORM (oAriaApplication.ScreenHome+'POSREC.SCX')
  =gfCallForm('POSREC')
  *! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[End]
Else
  *! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[Start]
  If File(oAriaApplication.clientscreenhome+'POSREC.SCX')
    Do Form (oAriaApplication.clientscreenhome+'POSREC.SCX') Name lcObjName Noshow
  Else
    *! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[End]
    Do Form (oAriaApplication.ScreenHome+'POSREC.SCX') Name lcObjName Noshow
    *! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[Start]
  Endif
  *! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[End]
Endif
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
Function lfInit
Lparameters loFormSet
Local laIndex
*-- To initialize the variables as properties in the formset.
=lfAddPro(loFormSet)

*! E303006,1 SAB 12/03/2011 Enable Issue From BINs in Inter Location PO [Start]
If loFormSet.llSilentMod
  gcBaseWind = 'AWRPOSTREC'
Endif
*! E303006,1 SAB 12/03/2011 Enable Issue From BINs in Inter Location PO [End]

*-- 'N' Issue Inter-Location P/o, 'A' Issue Adornment order,;
*-- 'H' Issue Inter Location P/O Batch, 'U' Issue Inter-Location PO Shipment
If loFormSet.lcPType $ 'NAHU' And !loFormSet.llWareHous
  *--The system has not been setup to use multiple locations. Cannot proceed.
  =gfModalGen('TRM42054B42001','DIALOG')
  Return .F.
Endif
*B126833,1 WAM 04/03/2005 Add new button to edit/view landed cost
loFormSet.ariaform1.cmdLandCost.Visible = !loFormSet.llMFCall
*B126833,1 WAM 04/03/2005 (End)

Private lcSqlStatement
*-- Open the PosHdr file
lcSqlStatement  = "SELECT TOP 0 * FROM POSHDR [INDEX=POSHDR]"
=lfOpenSql(lcSqlStatement,'POSHDR',loFormSet.lcPosHdr, "","",.F.)
loFormSet.DataEnvironment.InitialSelectedAlias = loFormSet.lcPosHdr
loFormSet.cBrowseAliasName       = loFormSet.lcPosHdr

*-- Open the PosLn file
lcSqlStatement  = "SELECT TOP 0 *, SPACE(3) AS cPriceCur, SPACE(3) AS cDutyCur, "+;
  " SPACE(1) AS Status FROM POSLN [INDEX=POSLN]"
=lfOpenSql(lcSqlStatement,'POSLN',loFormSet.lcPosLn, "","",.F.)

*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
Dimension loFormSet.laStatusArr[4,2]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laStatusArr[1,1]  = LANG_POSTREC_STATUS_OPEN
loFormSet.laStatusArr[1,1]  = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_STATUS_OPEN,loFormSet.GetHeaderText("LANG_POSTREC_STATUS_OPEN",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]
loFormSet.laStatusArr[1,2]  = LANG_POSTREC_STATUS_O

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laStatusArr[2,1]  = LANG_POSTREC_STATUS_APPROVED
loFormSet.laStatusArr[2,1]  = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_STATUS_APPROVED,loFormSet.GetHeaderText("LANG_POSTREC_STATUS_APPROVED",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]
loFormSet.laStatusArr[2,2]  = LANG_POSTREC_STATUS_A

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laStatusArr[3,1]  = LANG_POSTREC_STATUS_CANCEL
loFormSet.laStatusArr[3,1]  = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_STATUS_CANCEL,loFormSet.GetHeaderText("LANG_POSTREC_STATUS_CANCEL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

loFormSet.laStatusArr[3,2]  = LANG_POSTREC_STATUS_X

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laStatusArr[4,1]  = LANG_POSTREC_STATUS_POSTED
loFormSet.laStatusArr[4,1]  = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_STATUS_POSTED,loFormSet.GetHeaderText("LANG_POSTREC_STATUS_POSTED",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

loFormSet.laStatusArr[4,2]  = LANG_POSTREC_STATUS_P

*! E303079,1 MMT 06/28/2012 Fixing Media issues[T20120304.0004][Start]
*! N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][start]
*IF !(loFormSet.lcPType $ 'PGF')
If !(loFormSet.lcPType $ 'PGFW')
  *! N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][End]
  *! E303079,1 MMT 06/28/2012 Fixing Media issues[T20120304.0004][END]
  Dimension loFormSet.laPanelObj[1,6]
  loFormSet.laPanelObj[1,1]="cmdDelBatch"
  loFormSet.laPanelObj[1,2]=oAriaApplication.BitmapHome+"TRASH.BMP"
  loFormSet.laPanelObj[1,3]="mCanBatch"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loFormSet.laPanelObj[1,4]=LANG_POSTREC_CANCBATCH
  loFormSet.laPanelObj[1,4]=Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_CANCBATCH,loFormSet.GetHeaderText("LANG_POSTREC_CANCBATCH",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loFormSet.laPanelObj[1,5]=LANG_POSTREC_CANCBATCH
  loFormSet.laPanelObj[1,5]=Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_CANCBATCH,loFormSet.GetHeaderText("LANG_POSTREC_CANCBATCH",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.laPanelObj[1,6]="A"
  *! E303079,1 MMT 06/28/2012 Fixing Media issues[T20120304.0004][Start]
Endif
*! E303079,1 MMT 06/28/2012 Fixing Media issues[T20120304.0004][END]
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]

*-- If the system uses keep track of bins.
If loFormSet.llWareLoc
  *T20071102.0018(C200876) TMI [Start] Enhance opening WHSLOC file when Bin Location is installed
  If Ascan(loFormSet.laEvntTrig,Padr('CRTBINLN',10),1,Alen(loFormSet.laEvntTrig,1),1) > 0 ;
      .And. gfDoTriger('POSTREC',Padr('ISUSEBIN',10))
    =gfOpenTable(oAriaApplication.DataDir+'WHSLOC','WHSLOCST','SH')
  Else
    *T20071102.0018(C200876) TMI [End  ]
    lcSqlStatement  = "SELECT * FROM WHSLOC"
    =lfOpenFox(lcSqlStatement,'WHSLOC','WHSLOC',"")
    Select WhsLoc
    Dimension laIndex[2,2]
    laIndex = ''
    laIndex[1,1] = 'cWareCode+cLocation+Style+Color'
    laIndex[1,2] = 'WHSLOC'
    laIndex[2,1] = 'Style+Color+cWareCode+cLocation'
    laIndex[2,2] = 'WHSLOCST'
    =lfSetIndex('WhsLoc',@laIndex)
    Set Order To Tag WHSLOCST In WhsLoc
    *T20071102.0018(C200876) TMI [Start] close the above IF condition
  Endif
  *T20071102.0018(C200876) TMI [End  ]
Endif
Return

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
Function lfAfterInit
Lparameters loFormSet
Private lcSqlStatement, laIndex

*-- Open warehouse file.
lcSqlStatement  = "SELECT * FROM WAREHOUS"
=lfOpenFox(lcSqlStatement,'WAREHOUS',lcWareHFil,"")
Select (lcWareHFil)
Dimension laIndex[1,2]
laIndex = ''
laIndex[1,1] = 'cWareCode'
laIndex[1,2] = lcWareHFil
=lfSetIndex(lcWareHFil,@laIndex)

*-- Get all ware houses
loFormSet.llCMInstld = (Occurs('NC',oAriaApplication.CompanyInstalledModules) <> 0)

*-- Fill the warehouse popup
Dimension loFormSet.laWare[1]
Select (lcWareHFil)
If loFormSet.llWareHous
  If loFormSet.llCMInstld And loFormSet.llPOSale
    Locate For cSiteId = oAriaApplication.gcCurSite
    loFormSet.laWare[1] = Evaluate(lcWareHFil+'.cWareCode')+'-'+Evaluate(lcWareHFil+'.cDesc')
    loFormSet.lnWare = 1
  Else
    Select cWareCode+'-'+cDesc From (lcWareHFil) ;
      WHERE Iif(loFormSet.lcInvType="0001",lStyInv,lMatInv) Into Array loFormSet.laWare

    Declare loFormSet.laWare [ALEN(loFormSet.laWare,1)+1]
    =Ains(loFormSet.laWare,1)
    lcArray1 = ""
    loFormSet.laWare[1] = Padr(lcArray1,40)+"N/A"+Padr(lcArray1,40)
    loFormSet.lnWare = 0
    loFormSet.ariaform1.cboLocations.Requery
  Endif
Else
  *GO TOP IN WAREHOUS
  Go Top In (lcWareHFil)
  loFormSet.laWare[1] = Evaluate(lcWareHFil+'.cWareCode')+'-'+Evaluate(lcWareHFil+'.cDesc')
Endif

*-- Open the style or material master file
If loFormSet.lcInvType = "0001"
  *-- To get the structure of the style file
  lcSqlStatement  = "SELECT * FROM STYLE WHERE 1 = 2"
  =lfOpenFox(lcSqlStatement,'STYLE',loFormSet.lcTmpItem,"")
  Dimension laIndex[1,2]
  laIndex = ''
  laIndex[1,1] = 'Style'
  laIndex[1,2] = loFormSet.lcTmpItem
  =lfSetIndex(loFormSet.lcTmpItem,@laIndex)

  *-- To get the structure of the stydye file
  lcSqlStatement  = "SELECT * FROM STYDYE WHERE 1 = 2"
  =lfOpenFox(lcSqlStatement,'STYDYE',loFormSet.lcItemLoc,"")
  Dimension laIndex[1,2]
  laIndex = ''
  laIndex[1,1] = 'Style+cWareCode+Dyelot'
  laIndex[1,2] = loFormSet.lcItemLoc
  =lfSetIndex(loFormSet.lcItemLoc,@laIndex)
Else

  *-- To get the structure of the style file
  lcSqlStatement  = "SELECT TOP 0 * FROM ITEM [INDEX=STYLE]"
  =lfOpenSql(lcSqlStatement,'ITEM',loFormSet.lcTmpItem, "","",.F.)
  Dimension laIndex[1,2]
  laIndex = ''
  laIndex[1,1] = 'cInvType+Style'
  laIndex[1,2] = loFormSet.lcTmpItem
  =lfSetIndex(loFormSet.lcTmpItem,@laIndex)

  lcSqlStatement  = "SELECT TOP 0 * FROM ITEMLOC [INDEX=STYDYE]"
  =lfOpenSql(lcSqlStatement,'ITEMLOC',loFormSet.lcItemLoc, "","",.F.)
  Dimension laIndex[1,2]
  laIndex = ''
  laIndex[1,1] = 'cInvType+Style+cWareCode+Dyelot'
  laIndex[1,2] = loFormSet.lcItemLoc
  =lfSetIndex(loFormSet.lcItemLoc,@laIndex)
Endif
*-- Set the form Caption
Do Case
  *-- 'N' Issue Inter-Location P/O, 'U' Issue Inter-Location PO Shipment
Case loFormSet.lcPType $ 'NU'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loFormSet.AriaForm1.CAPTION = "Issue Inter-Location PO"
  loFormSet.ariaform1.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ISSUE_INTERLOCATION_PO,loFormSet.GetHeaderText("LANG_POSTREC_ISSUE_INTERLOCATION_PO",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *-- 'H' Issue Inter Location P/O Batch
Case loFormSet.lcPType = 'H'

  *-- 'P' Receive Material PO 'F' Receive Material PO Shipment
Case loFormSet.lcPType $ 'PF'
  *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
  *loFormSet.AriaForm1.CAPTION = "Receive Material PO"
  loFormSet.ariaform1.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_RECEIVE_MATERIAL_PO,loFormSet.GetHeaderText("LANG_POSTREC_RECEIVE_MATERIAL_PO",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]


  *!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [START]
  loFormSet.ariaform1.cmdRolls.Visible = (loFormSet.lcCostMthM = 'L' And loFormSet.llTrkRolls)
  *!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]

  *N039416,1 KHM 06/21/2005 Changing the screen caption for return style PO [Begin]
  *-- 'R' Issue Return Style PO
Case loFormSet.lcPType = 'R'
  *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
  *loFormSet.AriaForm1.CAPTION = "Issue Return Style PO"
  loFormSet.ariaform1.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ISSUE_RETURN_STYLE_PO,loFormSet.GetHeaderText("LANG_POSTREC_ISSUE_RETURN_STYLE_PO",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N039416,1 KHM 06/21/2005 [End]

  *N039417,1 KHM 06/21/2005 Changing the screen caption for return material PO [Begin]
  *-- 'R' Issue Return Style PO
Case loFormSet.lcPType = 'G'
  *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
  *loFormSet.AriaForm1.CAPTION = "Issue Return Material PO"
  loFormSet.ariaform1.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ISSUE_RETURN_MATERIAL_PO,loFormSet.GetHeaderText("LANG_POSTREC_ISSUE_RETURN_MATERIAL_PO",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N039417,1 KHM 06/21/2005 [End]

  *!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [START]
  loFormSet.ariaform1.cmdRolls.Visible = (loFormSet.lcCostMthM = 'L')
  *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
  *loFormSet.AriaForm1.cmdRolls.CAPTION = IIF((loFormSet.lcCostMthM = 'L' AND !loFormSet.llTrkRolls),'\<Lots','\<Rolls')
  loFormSet.ariaform1.cmdRolls.Caption = Iif((loFormSet.lcCostMthM = 'L' And !loFormSet.llTrkRolls),Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_LOTS,loFormSet.GetHeaderText("LANG_POSTREC_LOTS",loFormSet.HeaderAlias)),Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ROLLS_2,loFormSet.GetHeaderText("LANG_POSTREC_ROLLS_2",loFormSet.HeaderAlias)))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]

  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
Case loFormSet.lcPType = 'W'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loFormSet.AriaForm1.Caption = LANG_POSTREC_MMO_REC_MMO
  loFormSet.ariaform1.Caption = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MMO_REC_MMO,loFormSet.GetHeaderText("LANG_POSTREC_MMO_REC_MMO",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.ariaform1.cmdRolls.Visible = (loFormSet.lcCostMthM = 'L')
  *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
  *loFormSet.AriaForm1.cmdRolls.CAPTION = IIF((loFormSet.lcCostMthM = 'L' AND !loFormSet.llTrkRolls),'\<Lots','\<Rolls')
  loFormSet.ariaform1.cmdRolls.Caption = Iif((loFormSet.lcCostMthM = 'L' And !loFormSet.llTrkRolls),Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_LOTS,loFormSet.GetHeaderText("LANG_POSTREC_LOTS",loFormSet.HeaderAlias)),Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ROLLS_2,loFormSet.GetHeaderText("LANG_POSTREC_ROLLS_2",loFormSet.HeaderAlias)))
  *N000682,1 11/20/2012 MMT Globlization changes[End]


  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][End]
Endcase

*-- Hide the currencies object in case of not multi currency
If !loFormSet.llMulCurr
  With loFormSet.ariaform1
    Store .F. To .lblCurrency.Visible,  .lblPrice.Visible, .txtPrice.Visible,;
      .txtpriceRate.Visible, .lblRate.Visible, .txtDutyRate.Visible,;
      .lblDuty.Visible, .txtDuty.Visible,;
      .shpRegion4.Visible
    .shpRegion5.Width = 661

  Endwith
Endif

*N000587,1 WAM 12/01/2007 Don't read duty currency code and exchange rate
With loFormSet.ariaform1
  If loFormSet.lcBusDoc+loFormSet.lcWorkOrd $ 'PP|PD|NN'
    Store .F. To  .txtDutyRate.Visible,.lblDuty.Visible, .txtDuty.Visible
  Endif
Endwith
*N000587,1 WAM 12/01/2007 (End)

*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [START]
*N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
*IF loFormSet.lcCostMthM $ 'LIF' AND loFormSet.lcPType $ 'PFG'
If loFormSet.lcCostMthM $ 'LIF' And loFormSet.lcPType $ 'PFGW'
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
  loFormSet.AddProperty('lcTmpJour')
  loFormSet.AddProperty('lcTmpRoll')
  loFormSet.AddProperty('lcFullRoll')
  loFormSet.lcTmpJour = gfTempName()
  loFormSet.lcFullRoll = gfTempName()
  loFormSet.lcTmpRoll = gfTempName()
Endif
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
Function lfShow
Lparameters loFormSet, lcScrMode


Do Case

  *-- S E L E C T  M O D E.
Case lcScrMode = 'S'

  *!*	    *C200488,1 WAB (Start)
  *!*	    IF ASCAN(laEvntTrig,PADR("ZAPTEMP",10)) <> 0
  *!*	      =gfDoTriger("POSTREC",PADR("ZAPTEMP",10))
  *!*	    ENDIF
  *!*	    *C200488,1 WAB (END)
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  Store '' To loFormSet.laUsedBatchPO
  If Used(loFormSet.lcbatchdet)
    Use In (loFormSet.lcbatchdet)
  Endif
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]

  If loFormSet.llEditLCst
    Set Mark Of Bar 1 Of _OPTIONPOP To .F.
  Endif
  loFormSet.llEditLCst = .F.
  loFormSet.lnWare = 0

  *-- Create the temporary files
  =lfCreatTmp(loFormSet)
  *-- Assing the grid record source after create the temporary file.
  loFormSet.ariaform1.grdReceivingLines.RecordSource = ""
  loFormSet.ariaform1.grdReceivingLines.RecordSource = loFormSet.lcTmpLine
  *-- To disable the edit region
  =lfObjStatus(loFormSet,.F.)

  *-- If this is the first time running the screen
  If loFormSet.llFirst
    With loFormSet.ariaform1
      Store .T. To .dtPickerPostingDate.Enabled, .dtpickerReceivingDate.Enabled
      Store oAriaApplication.SystemDate To .dtPickerPostingDate.Value,;
        .dtpickerReceivingDate.Value
      If loFormSet.llIssue And loFormSet.lcPType $ 'NU'
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *.lblPO.CAPTION = 'Inter-Loc. P/O#'
        .lblPO.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_INTERLOC_PO,loFormSet.GetHeaderText("LANG_POSTREC_INTERLOC_PO",loFormSet.HeaderAlias))
        *N000682,1 MMT 11/20/2012 Globalization Changes[End]
      Endif
      *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
      If loFormSet.lcPType $ 'MT'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *.lblPO.CAPTION = 'C/T #'
        .lblPO.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_CT,loFormSet.GetHeaderText("LANG_POSTREC_CT",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]

      Endif
      *N038893,1 WAM 06/02/2005 (End)
      *-- IF not link to GL and Issue Inter-Location PO
      If loFormSet.llLinkToGl
        *-- 'N' Issue Inter-Location PO, 'U' Issue Inter-location PO Shipment
        If loFormSet.lcPType $ 'NU'
          *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
          *          .lblRecevingDate.CAPTION = "Issuing Date"
          .lblRecevingDate.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ISSUING_DATE,loFormSet.GetHeaderText("LANG_POSTREC_ISSUING_DATE",loFormSet.HeaderAlias))
          *N000682,1 MMT 11/20/2012 Globalization Changes[End]
        Endif

      Else
        Store .F. To .lblRecevingDate.Visible, .dtpickerReceivingDate.Visible,;
          .lblSmiCol2.Visible
        *-- 'N' Issue Inter-Location PO, 'U' Issue Inter-location PO Shipment
        If loFormSet.lcPType $ 'NU'
          *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
          *          .lblPostingDate.CAPTION = "Issuing Date"
          .lblPostingDate.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ISSUING_DATE,loFormSet.GetHeaderText("LANG_POSTREC_ISSUING_DATE",loFormSet.HeaderAlias))
          *N000682,1 11/20/2012 MMT Globlization changes[End]

        Else
          *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
          *          .lblPostingDate.CAPTION = "Receiving Date"
          .lblPostingDate.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_RECEIVING_DATE,loFormSet.GetHeaderText("LANG_POSTREC_RECEIVING_DATE",loFormSet.HeaderAlias))
          *N000682,1 11/20/2012 MMT Globlization changes[End]

        Endif
      Endif

      *-- Display the receiving types
      Do Case
        *-- Issue Return Style PO
      Case loFormSet.lcPType = 'R'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *.cboReceivingTypes.ROWSOURCE = LANG_POSTREC_IssueReturn+","+LANG_POSTREC_IssueR
        .cboReceivingTypes.RowSource = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_IssueReturn,loFormSet.GetHeaderText("LANG_POSTREC_IssueReturn",loFormSet.HeaderAlias))+","+;
          LANG_POSTREC_IssueR
        *N000682,1 11/20/2012 MMT Globlization changes[End]

        loFormSet.lcBusDoc   = 'R'
        loFormSet.lcWorkOrd  = 'P'

        *-- Issue Inter Location PO, Issue Inter Location PO Shipment
      Case loFormSet.lcPType = 'N'
        .cboReceivingTypes.RowSource = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_IssueIntrPO,loFormSet.GetHeaderText("LANG_POSTREC_IssueIntrPO",loFormSet.HeaderAlias))+;
          ","+LANG_POSTREC_IssueN+;
          ","+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_IssueIntrPOS,loFormSet.GetHeaderText("LANG_POSTREC_IssueIntrPOS",loFormSet.HeaderAlias))+","+;
          LANG_POSTREC_IssueIntrPOSH

        loFormSet.lcBusDoc   = 'N'
        loFormSet.lcWorkOrd  = 'N'

        *-- Issue Inter Location Batch
      Case loFormSet.lcPType = 'H'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *.cboReceivingTypes.ROWSOURCE = LANG_POSTREC_IssueIntrPOB+","+LANG_POSTREC_IssueH
        .cboReceivingTypes.RowSource = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_IssueIntrPOB,loFormSet.GetHeaderText("LANG_POSTREC_IssueIntrPOB",loFormSet.HeaderAlias))+","+;
          LANG_POSTREC_IssueH
        *N000682,1 11/20/2012 MMT Globlization changes[End]

        loFormSet.lcBusDoc   = 'N'
        loFormSet.lcWorkOrd  = 'N'

        *-- Issue Adornment PO
      Case loFormSet.lcPType = 'A'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *.cboReceivingTypes.ROWSOURCE = LANG_POSTREC_IssueAdronPO+","+LANG_POSTREC_IssueA
        .cboReceivingTypes.RowSource = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_IssueAdronPO,loFormSet.GetHeaderText("LANG_POSTREC_IssueAdronPO",loFormSet.HeaderAlias))+","+;
          LANG_POSTREC_IssueA
        *N000682,1 11/20/2012 MMT Globlization changes[End]

        loFormSet.lcBusDoc   = 'P'
        loFormSet.lcWorkOrd  = 'A'

        *-- Receive Style PO and Style Shipment PO
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
        *CASE loFormSet.lcPType $ 'IS'
      Case loFormSet.lcPType = 'S'
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
        loFormSet.lcBusDoc   = 'P'
        loFormSet.lcWorkOrd  = 'P'

        *-- Receive Material PO and Material Shipment PO
      Case loFormSet.lcPType $ 'PF'
        loFormSet.lcBusDoc   = 'P'
        loFormSet.lcWorkOrd  = 'M'
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
        .cboReceivingTypes.RowSource =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ReceiveMatPO,loFormSet.GetHeaderText("LANG_POSTREC_ReceiveMatPO",loFormSet.HeaderAlias))+","+;
          LANG_POSTREC_ReceiveMatPOP+","+;
          IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ReceiveMatPOSHP,loFormSet.GetHeaderText("LANG_POSTREC_ReceiveMatPOSHP",loFormSet.HeaderAlias))+","+;
          LANG_POSTREC_ReceiveMatPOF

        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
        *-- Issue Return Material PO
      Case loFormSet.lcPType = 'G'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *.cboReceivingTypes.ROWSOURCE = LANG_POSTREC_IssueReturn+","+LANG_POSTREC_IssueRMat
        .cboReceivingTypes.RowSource = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_IssueReturn,loFormSet.GetHeaderText("LANG_POSTREC_IssueReturn",loFormSet.HeaderAlias))+","+;
          LANG_POSTREC_IssueRMat
        *N000682,1 11/20/2012 MMT Globlization changes[End]

        loFormSet.lcBusDoc   = 'R'
        loFormSet.lcWorkOrd  = 'M'

        *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
        *-- 'M' Receive C/T, 'T' Receive C/T Batch
      Case loFormSet.lcPType $ 'MT'
        loFormSet.lcBusDoc   = 'P'
        loFormSet.lcWorkOrd  = 'U'
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
        .cboReceivingTypes.RowSource = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ReceiveCT,loFormSet.GetHeaderText("LANG_POSTREC_ReceiveCT",loFormSet.HeaderAlias)) +","+;
          LANG_POSTREC_ReceiveM+","+;
          IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ReceiveDyeOrd,loFormSet.GetHeaderText("LANG_POSTREC_ReceiveDyeOrd",loFormSet.HeaderAlias))+","+;
          LANG_POSTREC_ReceiveD

      Case loFormSet.lcPType $ 'OI'
        .cboReceivingTypes.RowSource =  Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ReceivePO,loFormSet.GetHeaderText("LANG_POSTREC_ReceivePO",loFormSet.HeaderAlias))+","+;
          LANG_POSTREC_ReceiveSPOI+","+;
          IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ReceiveSPOShip,loFormSet.GetHeaderText("LANG_POSTREC_ReceiveSPOShip",loFormSet.HeaderAlias))+","+;
          LANG_POSTREC_ReceiveSS

        loFormSet.lcBusDoc   = 'P'
        loFormSet.lcWorkOrd  = 'P'
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
        *N038893,1 WAM 06/02/2005 (End)
        *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
      Case loFormSet.lcPType = 'W'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *.cboReceivingTypes.RowSource = LANG_POSTREC_REC_MMO+","+LANG_POSTREC_MMOW
        .cboReceivingTypes.RowSource = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_REC_MMO,loFormSet.GetHeaderText("LANG_POSTREC_REC_MMO",loFormSet.HeaderAlias))+","+;
          LANG_POSTREC_MMOW
        *N000682,1 11/20/2012 MMT Globlization changes[End]

        loFormSet.lcBusDoc   = 'P'
        loFormSet.lcWorkOrd  = 'F'
        *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][End]


      Endcase

      *-- To assign the aria work order key properties
      =lfSetKeyPro(loFormSet,loFormSet.lcPType)

      =lfActBrow(loFormSet)
      .cboReceivingTypes.Value = loFormSet.lcPType
      .cboReceivingTypes.OldValue = "*"
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]

      =lfvRecvTyp(loFormSet)
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
      .cboReceivingTypes.Enabled = .F.
      *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
      *.dtPickerPostingDate.SetFocus
      If !loFormSet.llSilentMod
        .dtPickerPostingDate.SetFocus
      Endif
      *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

    Endwith
  Else
    With loFormSet.ariaform1
      .dtPickerPostingDate.Enabled    = .T.
      .dtPickerPostingDate.Value = oAriaApplication.SystemDate
      .dtpickerReceivingDate.Enabled  = .T.
      .dtpickerReceivingDate.Value = oAriaApplication.SystemDate
      Store loFormSet.lcPType To .cboReceivingTypes.Value, .cboReceivingTypes.OldValue
      .cboReceivingTypes.Enabled = .T.
      =lfActBrow(loFormSet)
      =lfvRecvTyp(loFormSet)
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
      *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
      *.dtPickerPostingDate.SetFocus
      If !loFormSet.llSilentMod
        .dtPickerPostingDate.SetFocus
      Endif
      *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
    Endwith
  Endif

  *-- 'H' Issue Inter Location P/O Batch, 'U' Issue Inter Location P/O Shipment
  If loFormSet.lcPType = 'H'
    =lfvRecvTyp(loFormSet)
  Endif
  Store 0 To loFormSet.lnTotStk,loFormSet.lnTotDam,loFormSet.lnTotCan
  loFormSet.llIgnorAll = .F.

  *-- 'R' Issue Return Style PO , 'A' Issue Adornment PO
  *-- 'H' Issue Inter Location Batch', 'G' Issue Return Material PO
  If loFormSet.lcPType $ 'RAHG'
    loFormSet.ariaform1.cboReceivingTypes.Enabled = .F.
  Else
    *loFormSet.AriaForm1.cboReceivingTypes.Enabled = .T.
  Endif
  *-- If not use multi-currency
  If !loFormSet.llMulCurr
    Store oAriaApplication.BaseCurrency To loFormSet.lcCur1, loFormSet.lcCur2
  Endif
  Store 1 To loFormSet.lnRate1,loFormSet.lnRate2,loFormSet.lnCurrUnt1,loFormSet.lnCurrUnt2
  *-- To assign object's control source.
  =lfCntrSour(loFormSet,.F.)
  *-- Define array laECost to hold the equivelant costs
  Dimension loFormSet.laECost[7]
  loFormSet.laECost = 0

  *-- A D D  M O D E.
Case lcScrMode = "A"

  llCUpDate  = .T.
  llGoAndChk = .T.
  With loFormSet.ariaform1
    Store .F. To .dtPickerPostingDate.Enabled,;
      .dtpickerReceivingDate.Enabled,;
      .txtstock.Enabled, .txtothers.Enabled,;
      .txtcancel.Enabled
  Endwith
  =lfShowScr(loFormSet)
  =lfActBrow(loFormSet)
  =lfwBrow(loFormSet)

Endcase

Return

*:*************************************************************
*! Name     : lfCntrSour
*! Developer: Khalid Mohi El-Din Mohamed
*! Date     : 09/11/2004
*! Purpose  : To assign object's control source.
*:*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!             llAssign : .T. To assign control source
*!*************************************************************
Function lfCntrSour
Lparameters loFormSet, llAssign
*-- Asign the control source
With loFormSet.ariaform1
  .txtstock.ControlSource					= Iif(llAssign,'ThisFormSet.lnTotStk',"")
  .txtothers.ControlSource					= Iif(llAssign,'ThisFormSet.lnTotDam',"")
  .txtcancel.ControlSource					= Iif(llAssign,'ThisFormSet.lnTotCan',"")
  .kbPoNo.keytextbox.ControlSource          = Iif(llAssign,loFormSet.lcTmpLine + '.PO',"")
  .kbItem.ControlSource                     = Iif(llAssign,loFormSet.lcTmpLine + '.Style',"")
  .kbconfiguration.keytextbox.ControlSource = Iif(llAssign,loFormSet.lcTmpLine + '.Dyelot',"")
  .txtitemDesc.ControlSource                = Iif(llAssign,loFormSet.lcTmpLine + '.cStyDesc',"")
  .cboLocations.ControlSource               = Iif(llAssign,'ThisFormSet.lnWare',"")
  .txtreference.ControlSource               = Iif(llAssign,loFormSet.lcTmpLine + '.Reference',"")
  .txtPrice.ControlSource                   = Iif(llAssign,loFormSet.lcTmpLine + '.cPriceCur',"")
  .txtDuty.ControlSource                    = Iif(llAssign,loFormSet.lcTmpLine + '.cDutyCur',"")
  .txtpriceRate.ControlSource               = Iif(llAssign,loFormSet.lcTmpLine + '.nLanPrRat',"")
  .txtDutyRate.ControlSource                = Iif(llAssign,loFormSet.lcTmpLine + '.nLanDuRat',"")
  .cboLocations.Requery

  *-- 'P' Material PO, 'F' Material PO Shipment
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
  *IF loFormSet.lcPType $ 'PF'
  If loFormSet.lcPType $ 'PFW'
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][End]
    .txtPattern.ControlSource                = Iif(llAssign,loFormSet.lcTmpLine + '.Pattern',"")
  Endif

  If !llAssign
    Store ""  To .kbItem.Value       , .kbconfiguration.keytextbox.Value,;
      .txtreference.Value , .txtPattern.Value,;
      .txtPrice.Value     , .txtDuty.Value   ,;
      .txtpriceRate.Value , .txtDutyRate.Value
    Store 0   To .cboLocations.Value
  Endif

Endwith


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
Function lfActBrow
Lparameters loFormSet
Local lnColumnNo, lcWorkOrdTit, lcTmpFile, lcFieldMask
lcTmpFile   = loFormSet.lcTmpLine
lcFieldMask = Iif(loFormSet.lcInvType="0001","9999999","9999999.999")
Select(lcTmpFile)


With loFormSet.ariaform1.grdReceivingLines
  .ColumnCount = 1
  lnColumnNo   = 1

  *-- Cartons Column
  *-- 'B' Receive P/O Batch, 'T' Receive C/T Batch,
  *-- 'L' Receive Inter Location P/O Batch', 'H' Issue Inter Location P/O Batch'
  If loFormSet.lcPType $ 'BTLH'
    *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
    *.COLUMNS(lnColumnNo).Header1.CAPTION = "Cartons"
    .Columns(lnColumnNo).Header1.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_CARTONS,loFormSet.GetHeaderText("LANG_POSTREC_CARTONS",loFormSet.HeaderAlias))
    *N000682,1 MMT 11/20/2012 Globalization Changes[End]
    .Columns(lnColumnNo).Header1.Alignment = 0     && Left
    .Columns(lnColumnNo).Width = 50
    .Columns(lnColumnNo).ControlSource = lcTmpFile +'.Style'
    .Columns(lnColumnNo).Alignment = 0     && Left
    .ColumnCount = .ColumnCount + 1
    lnColumnNo   = lnColumnNo + 1
  Endif

  *-- Work Order Column
  *N000682,1 MMT 03/15/2012 Globalization Changes[Start]
  *lcWorkOrdTit = IIF(loFormSet.lcPtype $ 'MT','C/T #',;
  IIF(loFormSet.lcPtype='R','Ret #','P/O #'))
  lcWorkOrdTit = Iif(loFormSet.lcPType $ 'MT',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_CT,loFormSet.GetHeaderText("LANG_POSTREC_CT",loFormSet.HeaderAlias)),;
    IIF(loFormSet.lcPType='R',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_RETPO,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_RETPO",loFormSet.HeaderAlias)),;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_PO,loFormSet.GetHeaderText("LANG_POSTREC_PO",loFormSet.HeaderAlias))))
  *N000682,1 MMT 03/15/2012 Globalization Changes[End]

  .Columns(lnColumnNo).Header1.Caption = lcWorkOrdTit
  .Columns(lnColumnNo).Header1.Alignment = 0     && Left
  .Columns(lnColumnNo).Width = 75
  .Columns(lnColumnNo).ControlSource = lcTmpFile +'.PO'
  .Columns(lnColumnNo).Alignment = 0     && Left

  *-- Style Column
  .ColumnCount = .ColumnCount + 1
  lnColumnNo   = lnColumnNo + 1
  .Columns(lnColumnNo).Header1.Caption = loFormSet.ariaform1.kbItem.lcItemHeader
  .Columns(lnColumnNo).Header1.Alignment = 0     && Left
  .Columns(lnColumnNo).Width = 125
  .Columns(lnColumnNo).ControlSource = lcTmpFile +'.Style'
  .Columns(lnColumnNo).Alignment = 0     && Left

  *-- IF system uses configuration and receiving type is not material
  If loFormSet.llConfig
    .ColumnCount = .ColumnCount + 1
    lnColumnNo   = lnColumnNo + 1
    *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
    *.COLUMNS(lnColumnNo).Header1.CAPTION = 'Configuration'
    .Columns(lnColumnNo).Header1.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_CONFIGURATION,loFormSet.GetHeaderText("LANG_POSTREC_CONFIGURATION",loFormSet.HeaderAlias))
    *N000682,1 MMT 11/20/2012 Globalization Changes[End]

    .Columns(lnColumnNo).Header1.Alignment = 0     && Left
    .Columns(lnColumnNo).Width = 75
    .Columns(lnColumnNo).ControlSource = lcTmpFile +'.Dyelot'
    .Columns(lnColumnNo).Alignment = 0     && Left
  Else
    *-- If style or material uses dyelot
    If loFormSet.llDyelot Or loFormSet.llFabDye
      .ColumnCount = .ColumnCount + 1
      lnColumnNo   = lnColumnNo + 1
      *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
      *.COLUMNS(lnColumnNo).Header1.CAPTION = 'Dyelot'
      .Columns(lnColumnNo).Header1.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_DYELOT,loFormSet.GetHeaderText("LANG_POSTREC_DYELOT",loFormSet.HeaderAlias))
      *N000682,1 MMT 11/20/2012 Globalization Changes[End]

      .Columns(lnColumnNo).Header1.Alignment = 0     && Left
      .Columns(lnColumnNo).Width = 75
      .Columns(lnColumnNo).ControlSource = lcTmpFile +'.Dyelot'
      .Columns(lnColumnNo).Alignment = 0     && Left
    Endif
  Endif

  *-- IF system uses multi-location
  If loFormSet.llWareHous
    *-- IF 'N' Issue Inter-Location P/o, 'H' Issue Inter Location P/O Batch'
    *-- 'L' Receive Inter Location P/O Batch,  'A' Issue Adornment order,
    *-- 'U' Issue Inter Location PO Shipment, 'C' Receive Inter-Location P/O Shipment
    If loFormSet.lcPType $ 'NHLAUC'
      .ColumnCount = .ColumnCount + 1
      lnColumnNo   = lnColumnNo + 1
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *.COLUMNS(lnColumnNo).Header1.CAPTION = 'Source'
      .Columns(lnColumnNo).Header1.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_SOURCE_1,loFormSet.GetHeaderText("LANG_POSTREC_SOURCE_1",loFormSet.HeaderAlias))
      *N000682,1 11/20/2012 MMT Globlization changes[End]

      .Columns(lnColumnNo).Header1.Alignment = 0     && Left
      .Columns(lnColumnNo).Width = 75
      .Columns(lnColumnNo).ControlSource = lcTmpFile +'.Vendor'
      .Columns(lnColumnNo).Alignment = 0     && Left

      .ColumnCount = .ColumnCount + 1
      lnColumnNo   = lnColumnNo + 1
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *.COLUMNS(lnColumnNo).Header1.CAPTION = 'Target'
      .Columns(lnColumnNo).Header1.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TARGET,loFormSet.GetHeaderText("LANG_POSTREC_TARGET",loFormSet.HeaderAlias))
      *N000682,1 MMT 11/20/2012 Globalization Changes[End]
      .Columns(lnColumnNo).Header1.Alignment = 0     && Left
      .Columns(lnColumnNo).Width = 75
      .Columns(lnColumnNo).ControlSource = lcTmpFile +'.cWareCode'
      .Columns(lnColumnNo).Alignment = 0     && Left
    Else
      .ColumnCount = .ColumnCount + 1
      lnColumnNo   = lnColumnNo + 1
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *.COLUMNS(lnColumnNo).Header1.CAPTION = 'Location'
      .Columns(lnColumnNo).Header1.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_LOCATION,loFormSet.GetHeaderText("LANG_POSTREC_LOCATION",loFormSet.HeaderAlias))
      *N000682,1 11/20/2012 MMT Globlization changes[End]

      .Columns(lnColumnNo).Header1.Alignment = 0     && Left
      .Columns(lnColumnNo).Width = 75
      .Columns(lnColumnNo).ControlSource = lcTmpFile +'.cWareCode'
      .Columns(lnColumnNo).Alignment = 0     && Left
    Endif
  Endif

  *-- 'P' Material PO, 'F' Material PO Shipment
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
  *IF loFormSet.lcPtype $ 'PF'
  If loFormSet.lcPType $ 'PFW'
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
    .ColumnCount = .ColumnCount + 1
    lnColumnNo   = lnColumnNo + 1
    *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
    *.COLUMNS(lnColumnNo).Header1.CAPTION = 'Pattern'
    .Columns(lnColumnNo).Header1.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_PATTERN,loFormSet.GetHeaderText("LANG_POSTREC_PATTERN",loFormSet.HeaderAlias))
    *N000682,1 MMT 11/20/2012 Globalization Changes[End]

    .Columns(lnColumnNo).Header1.Alignment = 0     && Left
    .Columns(lnColumnNo).Width = 75
    .Columns(lnColumnNo).ControlSource = lcTmpFile +'.Pattern'
    .Columns(lnColumnNo).Alignment = 0     && Left
  Endif

  *-- Original Qty
  .ColumnCount = .ColumnCount + 1
  lnColumnNo   = lnColumnNo + 1
  *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
  *.COLUMNS(lnColumnNo).Header1.CAPTION = 'Original'
  .Columns(lnColumnNo).Header1.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ORIGINAL,loFormSet.GetHeaderText("LANG_POSTREC_ORIGINAL",loFormSet.HeaderAlias))
  *N000682,1 MMT 11/20/2012 Globalization Changes[End]

  .Columns(lnColumnNo).Header1.Alignment = 1     && Right
  .Columns(lnColumnNo).Width = 60
  .Columns(lnColumnNo).ControlSource = lcTmpFile +'.TotQty'
  .Columns(lnColumnNo).Alignment = 1     && Right
  .Columns(lnColumnNo).InputMask = lcFieldMask
  *-- Stock Qty
  .ColumnCount = .ColumnCount + 1
  lnColumnNo   = lnColumnNo + 1
  *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
  *.COLUMNS(lnColumnNo).Header1.CAPTION = 'Stock'
  .Columns(lnColumnNo).Header1.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_STOCK,loFormSet.GetHeaderText("LANG_POSTREC_STOCK",loFormSet.HeaderAlias))
  *N000682,1 MMT 11/20/2012 Globalization Changes[End]

  .Columns(lnColumnNo).Header1.Alignment = 1     && Right
  .Columns(lnColumnNo).Width = 60
  .Columns(lnColumnNo).ControlSource = lcTmpFile +'.TotStk'
  .Columns(lnColumnNo).Alignment = 1     && Right
  .Columns(lnColumnNo).InputMask = lcFieldMask

  *-- Other Qty
  .ColumnCount = .ColumnCount + 1
  lnColumnNo   = lnColumnNo + 1
  *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
  *.COLUMNS(lnColumnNo).Header1.CAPTION = 'Other'
  .Columns(lnColumnNo).Header1.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_OTHER,loFormSet.GetHeaderText("LANG_POSTREC_OTHER",loFormSet.HeaderAlias))
  *N000682,1 MMT 11/20/2012 Globalization Changes[End]

  .Columns(lnColumnNo).Header1.Alignment = 1     && Right
  .Columns(lnColumnNo).Width = 60
  .Columns(lnColumnNo).ControlSource = lcTmpFile +'.TotDam'
  .Columns(lnColumnNo).Alignment = 1     && Right
  .Columns(lnColumnNo).InputMask = lcFieldMask

  *-- Cancel Qty
  .ColumnCount = .ColumnCount + 1
  lnColumnNo   = lnColumnNo + 1
  *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
  *.COLUMNS(lnColumnNo).Header1.CAPTION = 'Cancel'
  .Columns(lnColumnNo).Header1.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_CANCEL,loFormSet.GetHeaderText("LANG_POSTREC_CANCEL",loFormSet.HeaderAlias))
  *N000682,1 MMT 11/20/2012 Globalization Changes[End]

  .Columns(lnColumnNo).Header1.Alignment = 1     && Right
  .Columns(lnColumnNo).Width = 60
  .Columns(lnColumnNo).ControlSource = lcTmpFile +'.TotCan'
  .Columns(lnColumnNo).Alignment = 1     && Right
  .Columns(lnColumnNo).InputMask = lcFieldMask

  *-- Balance Qty
  .ColumnCount = .ColumnCount + 1
  lnColumnNo   = lnColumnNo + 1
  *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
  *  .COLUMNS(lnColumnNo).Header1.CAPTION = 'Balance'
  .Columns(lnColumnNo).Header1.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BALANCE,loFormSet.GetHeaderText("LANG_POSTREC_BALANCE",loFormSet.HeaderAlias))
  *N000682,1 MMT 11/20/2012 Globalization Changes[End]

  .Columns(lnColumnNo).Header1.Alignment = 1     && Right
  .Columns(lnColumnNo).Width = 60
  .Columns(lnColumnNo).ControlSource = lcTmpFile +'.TotBal'
  .Columns(lnColumnNo).Alignment = 1     && Right
  .Columns(lnColumnNo).InputMask = lcFieldMask

  *-- Reference
  .ColumnCount = .ColumnCount + 1
  lnColumnNo   = lnColumnNo + 1
  *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
  *.COLUMNS(lnColumnNo).Header1.CAPTION = 'Reference'
  .Columns(lnColumnNo).Header1.Caption = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_REFERENCE,loFormSet.GetHeaderText("LANG_POSTREC_REFERENCE",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .Columns(lnColumnNo).Header1.Alignment = 1     && Right
  .Columns(lnColumnNo).Width = 160
  .Columns(lnColumnNo).ControlSource = lcTmpFile +'.Reference'
  .Columns(lnColumnNo).Alignment = 0     && Left

  .SetAll('ReadOnly',.T.,'COLUMN')
Endwith

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
Function lfvPostDat
Lparameters loFormSet
Private lcGLFYear,lcGLPeriod
*-- Fiscal yeare and period
Store '' To lcGLFYear,lcGLPeriod

With loFormSet.ariaform1
  If loFormSet.llLinkToGl
    *B126833,1 WAM 04/03/2005 Do not hide the Options pad
    *loFormSet.Deactivate()
    *B126833,1 WAM 04/03/2005 (End)

    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
    *IF !CHECKPRD(.dtPickerPostingDate.Value,'lcGLFYear','lcGLPeriod','PO')
    *  .dtPickerPostingDate.Value = .dtPickerPostingDate.OldValue
    *  RETURN .F.
    *ENDIF
    If !loFormSet.llSilentMod
      If !CHECKPRD(.dtPickerPostingDate.Value,'lcGLFYear','lcGLPeriod','PO')
        .dtPickerPostingDate.Value = .dtPickerPostingDate.OldValue
        Return .F.
      Endif
    Endif
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

    loFormSet.lcGLFYear = lcGLFYear
    loFormSet.lcGLPeriod = lcGLPeriod
  Else
    .dtpickerReceivingDate.Value = .dtPickerPostingDate.Value
    =lfvRecvDat(loFormSet)
  Endif
Endwith
Return

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
Function lfvRecvDat
Lparameters loFormSet
Private lcRowSource

*-- If the system is linked to GL and the receiving date is > than the posting date
With loFormSet.ariaform1
  If loFormSet.llLinkToGl And ;
      .dtpickerReceivingDate.Value > .dtPickerPostingDate.Value

    *B126833,1 WAM 04/03/2005 Do not hide the option pad
    *loFormSet.Deactivate()
    *B126833,1 WAM 04/03/2005 (End)
    *--The xxxx date cannot date cannot follow the posting date.
    *N000682,1 MMT 11/21/2012 globalization Changes [Start]
    *= gfModalGen('TRM42106B42000','DIALOG',IIF(loFormSet.lcPType $ 'RNAH','issuing','receiving'))
    = gfModalGen('TRM42106B42000','DIALOG',Iif(loFormSet.lcPType $ 'RNAH',;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_ISSUING,loFormSet.GetHeaderText("LANG_POSTREC_MSG_ISSUING",loFormSet.HeaderAlias)),;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_RECEVING,loFormSet.GetHeaderText("LANG_POSTREC_MSG_RECEVING",loFormSet.HeaderAlias))))
    *N000682,1 MMT 11/21/2012 globalization Changes [End]
    Return .F.
  Else
    Private lcGLFYear,lcGLPeriod
    *-- Fiscal yeare and period
    Store '' To lcGLFYear,lcGLPeriod
    *B126833,1 WAM 04/03/2005 Do not hide the option pad
    *loFormSet.Deactivate()
    *B126833,1 WAM 04/03/2005 (End)
    If !lfCHECKPRD(.dtpickerReceivingDate.Value,'lcGLFYear','lcGLPeriod',loFormSet)
      Return .F.
    Endif
    loFormSet.lcGLFYear = lcGLFYear
    loFormSet.lcGLPeriod = lcGLPeriod

  Endif

  If !loFormSet.llFirst
    Return
  Endif
  loFormSet.llFirst = .F.

  *-- If receiving styles
  If loFormSet.lcInvType = "0001"
    *-- 'R' Return Style PO, 'N' Issue Inter Location PO, 'A' Issue Adornment PO
    *-- 'H' Issue Inter Location PO Batch, 'U' Issue Inter Location PO Shipment
    If !(loFormSet.lcPType $ 'RNAHU')
      If loFormSet.llMFCall .Or. loFormSet.lcPType = 'E'
        *-- 'M' Receive Cutting Ticket, 'T' Receive Batch, 'D' Receive Dye Order
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
        *!*          lcRowSource = LANG_POSTREC_ReceiveCT +","+LANG_POSTREC_ReceiveM+","+;
        *!*                        LANG_POSTREC_ReceiveCTB+","+LANG_POSTREC_ReceiveT+","+;
        *!*                        LANG_POSTREC_ReceiveDyeOrd+","+LANG_POSTREC_ReceiveD
        lcRowSource = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ReceiveCT,loFormSet.GetHeaderText("LANG_POSTREC_ReceiveCT",loFormSet.HeaderAlias)) +","+;
          LANG_POSTREC_ReceiveM+","+;
          IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ReceiveDyeOrd,loFormSet.GetHeaderText("LANG_POSTREC_ReceiveDyeOrd",loFormSet.HeaderAlias))+","+;
          LANG_POSTREC_ReceiveD

        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
        If loFormSet.llWareHous .And. gfGetMemVar('M_BOMVAR')
          *-- 'E' Receive Adornment P/O
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
          *lcRowSource = lcRowSource + ","+LANG_POSTREC_ReceiveAdronPO+","+LANG_POSTREC_ReceiveAdronPOE
          lcRowSource = lcRowSource + ","+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ReceiveAdronPO,loFormSet.GetHeaderText("LANG_POSTREC_ReceiveAdronPO",loFormSet.HeaderAlias))+","+;
            LANG_POSTREC_ReceiveAdronPOE
          *N000682,1 11/20/2012 MMT Globlization changes[End]

        Endif
      Else
        *-- If use detail costing and receive by shipment
        If loFormSet.llImpCost And loFormSet.lcCostImp='S'
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
          lcRowSource = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ReceiveSPOShip,loFormSet.GetHeaderText("LANG_POSTREC_ReceiveSPOShip",loFormSet.HeaderAlias))+","+;
            LANG_POSTREC_ReceiveSS+","+;
            IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ReceiveIntrPO,loFormSet.GetHeaderText("LANG_POSTREC_ReceiveIntrPO",loFormSet.HeaderAlias))+","+;
            LANG_POSTREC_ReceiveIntrPOO+","+;
            IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ReceiveIntrPOIS,loFormSet.GetHeaderText("LANG_POSTREC_ReceiveIntrPOIS",loFormSet.HeaderAlias))+","+;
            LANG_POSTREC_ReceiveIntrPOSH

          *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
          *! B609634,1 MMT 06/28/2011 Fix bug of Zero Cost in Styinvjl in Case of receiving Interlocation PO[END]
          loFormSet.lcPType = Iif(loFormSet.lcPType='I','S',loFormSet.lcPType)
        Else
          *-- 'I' Receive by P/O, 'S' Receive by Shipment, 'B' Receive P/O Batch
          *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
          *!*            lcRowSource = LANG_POSTREC_ReceivePO+","+LANG_POSTREC_ReceiveSPOI+","+;
          *!*  		   			    LANG_POSTREC_ReceiveSPOShip+","+LANG_POSTREC_ReceiveSS+","+;
          *!*  					    LANG_POSTREC_ReceivePOB+","+LANG_POSTREC_ReceiveSPOB
          lcRowSource = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ReceivePO,loFormSet.GetHeaderText("LANG_POSTREC_ReceivePO",loFormSet.HeaderAlias))+","+;
            LANG_POSTREC_ReceiveSPOI+","+;
            IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ReceiveSPOShip,loFormSet.GetHeaderText("LANG_POSTREC_ReceiveSPOShip",loFormSet.HeaderAlias))+;
            ","+LANG_POSTREC_ReceiveSS

          *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
          *-- If multi-location and not from MF
          If loFormSet.llWareHous And !loFormSet.llMFCall
            *-- 'O' Receive Inter Location P/O, 'C' Receive Inter Location P/O Shipment
            *-- 'L' Receive Inter Location P/O Batch
            *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
            *!*              lcRowSource = lcRowSource + ","+LANG_POSTREC_ReceiveIntrPO+","+;
            *!*          						            LANG_POSTREC_ReceiveIntrPOO+","+;
            *!*  										    LANG_POSTREC_ReceiveIntrPOIS+","+;
            *!*  										    LANG_POSTREC_ReceiveIntrPOSH+","+;
            *!*  							                LANG_POSTREC_ReceiveIntrPOBT+","+;
            *!*  							                LANG_POSTREC_ReceiveIntrPOB
            lcRowSource = lcRowSource + ","+;
              IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ReceiveIntrPO,loFormSet.GetHeaderText("LANG_POSTREC_ReceiveIntrPO",loFormSet.HeaderAlias))+","+;
              LANG_POSTREC_ReceiveIntrPOO+","+;
              IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ReceiveIntrPOIS,loFormSet.GetHeaderText("LANG_POSTREC_ReceiveIntrPOIS",loFormSet.HeaderAlias))+","+;
              LANG_POSTREC_ReceiveIntrPOSH
            *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
          Endif
        Endif
      Endif
      .cboReceivingTypes.RowSource = lcRowSource
      .cboReceivingTypes.Enabled = .T.
      .cntShipment.Enabled = .F.
    Else
      *-- 'R' Return Style PO, 'A' Issue Adornment PO, 'H' Issue Inter Location PO Batch
      If loFormSet.lcPType $ 'RAH'
        .cboReceivingTypes.Enabled = .F.
      Else
        .cboReceivingTypes.Enabled = .T.
      Endif
    Endif

    *-- Case material PO, Shipment, issue return PO
  Else
    *-- 'G' Return Material PO
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
    *!*	    IF loFormSet.lcPType = 'G'
    *!*	      lcRowSource = LANG_POSTREC_IssueReturn+","+LANG_POSTREC_IssueRMat
    If loFormSet.lcPType $ 'WG'
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *lcRowSource = IIF(loFormSet.lcPType = 'G',LANG_POSTREC_IssueReturn+","+LANG_POSTREC_IssueRMat,LANG_POSTREC_REC_MMO+","+LANG_POSTREC_MMOW)
      lcRowSource = Iif(loFormSet.lcPType = 'G',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_IssueReturn,loFormSet.GetHeaderText("LANG_POSTREC_IssueReturn",loFormSet.HeaderAlias))+","+;
        LANG_POSTREC_IssueRMat,;
        IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_REC_MMO,loFormSet.GetHeaderText("LANG_POSTREC_REC_MMO",loFormSet.HeaderAlias))+","+;
        LANG_POSTREC_MMOW)
      *N000682,1 11/20/2012 MMT Globlization changes[End]

      *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][End]
      .cboReceivingTypes.Enabled = .F.
    Else
      lcRowSource = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ReceiveMatPO,loFormSet.GetHeaderText("LANG_POSTREC_ReceiveMatPO",loFormSet.HeaderAlias))+","+;
        LANG_POSTREC_ReceiveMatPOP+","+;
        IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ReceiveMatPOSHP,loFormSet.GetHeaderText("LANG_POSTREC_ReceiveMatPOSHP",loFormSet.HeaderAlias))+","+;
        LANG_POSTREC_ReceiveMatPOF

      .cboReceivingTypes.Enabled = .T.
    Endif
    .cboReceivingTypes.RowSource = lcRowSource

  Endif

  .cboReceivingTypes.Value = loFormSet.lcPType
  =lfObjStatus(loFormSet,.F.)

  *-- Case styles
  If loFormSet.lcInvType = "0001"
    *-- If not Issue Inter location PO Batch
    If !(loFormSet.lcPType = 'H')
      If loFormSet.llImpCost And loFormSet.lcCostImp='S' And loFormSet.lcPType ='S'
        .grdReceivingLines.Height = .grdReceivingLines.Height - .cntShipment.Height
        .grdReceivingLines.Top = .grdReceivingLines.Top + .cntShipment.Height + 2

        .cntBatch.Visible = .F.
        .cntBatch.Enabled = .F.
        .cntShipment.Visible = .T.
        .cntShipment.Enabled = .T.
        .cntShipment.kbShipNo.Enabled = .T.
        =lfActBrow(loFormSet)
      Else
        If loFormSet.lcPType = 'U'
          *.cntShipment.Enabled = .T.
        Else
          .cntShipment.Visible = .F.
          .cntShipment.Enabled = .F.
          *-- Enable the work order key
          .kbPoNo.Enabled = .T.
        Endif

        *-- If receiving cutting ticket enable the style field
        If loFormSet.llMFCall And loFormSet.lcPType='M'
          *! B610897,1 MMT 10/26/2014 Cutting ticket receiving allow user to enter style before entering CT#[T20141022.0011][Start]
          *.kbItem.ENABLED = .T.
          *! B610897,1 MMT 10/26/2014 Cutting ticket receiving allow user to enter style before entering CT#[T20141022.0011][End]
        Endif
      Endif
    Endif
    *-- Case Material
  Else
    .cntShipment.Visible = .F.
    .cntShipment.Enabled = .F.
    *-- Enable the work order key
    .kbPoNo.Enabled = .T.

  Endif

  *-- 'R' Issue retrun PO, 'A' Issue Adornment order
  *-- 'H' Issue inter location batch
  If !(loFormSet.lcPType $'RAHG')
    Keyboard '{SPACEBAR}'
  Endif
Endwith
=lfSetKeyPro(loFormSet,loFormSet.lcPType)
Return

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
Function lfCHECKPRD
Parameters ldDate,lcFYear,lcPeriod, loFormSet

Private llContinue,lcErrorM1,lnAlias, lcSqlStatement
lnAlias = Select()
llContinue = .T.

lcSqlStatement = "SELECT Cfisfyear, Cfspprdid, Dfsppbgdt, Dfsppendt "+;
  " FROM FSPRD "+;
  " WHERE BETWEEN ('"+Dtos(ldDate)+"' ,DTOS(Dfsppbgdt),DTOS(Dfsppendt))"

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatement,"FSPRD","FSPRD",;
  oAriaApplication.cAriaNativeDataFilesConStr,3,'SAVE',loFormSet.DataSessionId)

If lnConnectionHandlar <> 1
  llContinue = .F.
Endif

Locate
If Found()
  Locate Rest For Between(ldDate,Dfsppbgdt,Dfsppendt) While (ldDate >= Dfsppbgdt)
Endif
If !Found()                  && No period match checked date
  llContinue = .F.
Else
  &lcFYear  = Cfisfyear      && Transaction date year
  &lcPeriod = Cfspprdid      && Transaction date period
Endif
If !llContinue             && There is an error.
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *!*    lcErrorM1  = 'This receiving date '
  *!*    lcErrorM1 =  lcErrorM1 + DTOC(ldDate) + ' does not fall within any period. '
  *!*    lcErrorM1 = lcErrorM1 + 'Would you like to continue any way ?'
  lcErrorM1  = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_RECDATE,loFormSet.GetHeaderText("LANG_POSTREC_RECDATE",loFormSet.HeaderAlias))+' '
  lcErrorM1 =  lcErrorM1 + Dtoc(ldDate) + ' '+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_FALLANYPRD,loFormSet.GetHeaderText("LANG_POSTREC_FALLANYPRD",loFormSet.HeaderAlias))+' '
  lcErrorM1 = lcErrorM1 + Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_LIKECONTINUE,loFormSet.GetHeaderText("LANG_POSTREC_LIKECONTINUE",loFormSet.HeaderAlias))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  If gfModalGen('INM00274B34001','ALERT',lcErrorM1)=1
    Select (lnAlias)
    Return(.T.)
  Else
    Select (lnAlias)
    Return(.F.)
  Endif
Endif
Select (lnAlias)
Return(.T.)

*!*************************************************************
*! Name      : lfvRecvTyp
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To validate the receiving types
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
Function lfvRecvTyp
Lparameters loFormSet


*!*	*B611xx [Begin]
*!*	IF loFormSet.nCntBatchTop = 0
*!*	  loFormSet.nCntBatchTop = loFormSet.ariaform1.cntBatch.Top
*!*	  loFormSet.nGrigLinesHeight = loFormSet.ariaform1.grdReceivingLines.Height
*!*	  loFormSet.nGrigLinesTop = loFormSet.ariaform1.grdReceivingLines.Top
*!*	ENDIF
*B611405 MHM error while recive cut ticket
IF loFormSet.llRcv == .T.  
   If loFormSet.nCntBatchTop != 0 
  lnCntBatchTop     = loFormSet.nCntBatchTop
  lnGrigLinesHeight = loFormSet.nGrigLinesHeight
  lnGrigLinesTop    = loFormSet.nGrigLinesTop 
  loFormSet.ariaform1.cntBatch.Top=loFormSet.ariaform1.shpHeader.top +loFormSet.ariaform1.shpHeader.height 
  loFormSet.ariaform1.grdReceivingLines.Top = loFormSet.ariaform1.cntBatch.Top + loFormSet.ariaform1.cntBatch.Height + 2
  loFormSet.ariaform1.grdReceivingLines.Height = loFormSet.ariaform1.shpRegion1.Top - loFormSet.ariaform1.grdReceivingLines.Top - 2
   Endif 
ENDIF 
*!*	If loFormSet.nCntBatchTop != 0
*!*	  lnCntBatchTop     = loFormSet.nCntBatchTop
*!*	  lnGrigLinesHeight = loFormSet.nGrigLinesHeight
*!*	  lnGrigLinesTop    = loFormSet.nGrigLinesTop
*!*	  loFormSet.ariaform1.cntBatch.Top = lnCntBatchTop
*!*	  loFormSet.ariaform1.grdReceivingLines.Top = lnGrigLinesTop
*!*	  loFormSet.ariaform1.grdReceivingLines.Height = lnGrigLinesHeight
*!*	Endif
*B611xx [End]

*B611405 MHM error while recive cut ticket

With loFormSet.ariaform1
  If .cboReceivingTypes.Value = .cboReceivingTypes.OldValue
    Do Case
      *-- 'I' Receive P/O, 'R' Issue Return P/o, 'M' Receive C/T, 'N' Issue Inter-Location P/o
      *-- 'O' Receive Inter-Location P/o, 'D' Receive Dye Order, 'A' Issue Adornment order
      *-- 'E' Receive Adornment order, 'P' Receive Material PO, 'G' Return Material PO
      *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
      *CASE loFormSet.lcPType $ 'IRMNODAEPG'
    Case loFormSet.lcPType $ 'IRMNODAEPGW'
      *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
      .kbPoNo.keytextbox.ControlSource = ""
      .kbPoNo.Enabled = .T.
      *.kbPONo.SetFocus

      *-- 'S' Receive by Style Shipment, 'U' Issue Inter Location PO
      *-- 'C' Receive Inter Location PO Shipment, 'F' Receive Material Shipment
    Case loFormSet.lcPType $ 'SUCF'
      .cntShipment.Enabled = .T.
      .cntShipment.kbShipNo.Enabled = .T.
      If loFormSet.lcPType $ 'SFUC'
        *.cntShipment.kbShipNo.SetFocus
      Endif
      *! E303848,1 SARA.O 07/09/2017 Saving a ‘Receive by Shipment’ session will allow the user to save the receiving as a batch [Begin]
      *
      If loFormSet.lcPType ='S'
        .cntBatch.Visible = .T.
        .cntBatch.Enabled = .T.
        .cntBatch.kbBatchNo.Enabled = .T.
        .cntBatch.kbBatchNo.keytextbox.Value = ""

        *! E303848,1 SARA.O 07/09/2017 Saving a ‘Receive by Shipment’ session will allow the user to save the receiving as a batch [Begin]
        *.grdReceivingLines.HEIGHT = loFormSet.nGrigLinesHeight - .cntBatch.HEIGHT
        *.grdReceivingLines.TOP = loFormSet.nGrigLinesTop + .cntBatch.HEIGHT + 2
        *.cntBatch.TOP = loFormSet.nCntBatchTop + .cntShipment.HEIGHT + 2
        **B611375,MHM 8/2/2017 po recieving screen grid size shrunk 
        .cntBatch.Top = .cntShipment.Top+ .cntShipment.Height + 2
        .grdReceivingLines.Top = .cntBatch.Top + .cntBatch.Height + 2
        
        *B611450 , 1 HMS ,error still getting after "receive style po" and click post T20171019.0013 [begin]
        
        *.grdReceivingLines.Height = Thisformset.ariaform1.shpRegion1.Top - .grdReceivingLines.Top - 2
        .grdReceivingLines.Height = loFormSet.ariaform1.shpRegion1.Top - .grdReceivingLines.Top - 2
        
        *B611450 , 1 HMS ,error still getting after "receive style po" and click post T20171019.0013 [end]
        
        **B611375,MHM 8/2/2017 po recieving screen grid size shrunk 
        *lnCntBatchTop, lnGrigLinesHeight, lnGrigLinesTop
      Endif
      *! E303848,1 SARA.O 07/09/2017 Saving a ‘Receive by Shipment’ session will allow the user to save the receiving as a batch [End]


      *-- 'B' Receive P/O Batch, 'T' Receive C/T Batch, 'L' Receive Inter Location P/O Batch'
      *-- 'H' Issue Inter Location P/O Batch'
    Case loFormSet.lcPType $ 'BTLH'
      *.cntBatch.kbBatchNo.SetFocus
    Endcase
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
    If loFormSet.lcPType $  'MIO' && AND (.cboReceivingTypes.Value <> .cboReceivingTypes.OldValue)
      If !(.cboReceivingTypes.OldValue $ 'SCBTLHIOM')

        .grdReceivingLines.Height = .grdReceivingLines.Height - .cntBatch.Height

        .grdReceivingLines.Top = .grdReceivingLines.Top + .cntBatch.Height + 2

      Endif
      .cntBatch.Visible = .T.
      .cntBatch.Enabled = .T.
      .cntShipment.Visible = .F.
      .cntShipment.Enabled = .F.
      .cntBatch.kbBatchNo.Enabled = .T.
      .cntBatch.kbBatchNo.keytextbox.Value = ""
    Endif
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
    Return
  Endif

  *-- Create the temporary files
  *=lfCreatTmp(loFormSet)

  loFormSet.lcPType = .cboReceivingTypes.Value
  =lfObjStatus(loFormSet,.F.)
  loFormSet.llMulCurr  = Iif(loFormSet.lcPType $ 'DAE',.F.,loFormSet.llUseMCurr)
  *!*	  *-- 'H' Issue Inter Location P/O Batch', 'B' Receive P/O Batch, 'R' Issue Return P/o
  *!*	  *-- 'M' Receive C/T, 'T' Receive C/T Batch, 'D' Receive Dye Order
  *!*	  *-- 'A' Issue Adornment order, 'E' Receive Adornment order
  *!*	  IF loFormSet.lcPType $ 'HBRMDAELT'
  *!*	    WAIT WINDOW "Program still under development"
  *!*	  ENDIF

  Do Case
    *-- 'I' Receive P/O, 'R' Issue Return P/o, 'M' Receive C/T, 'N' Issue Inter-Location P/o
    *-- 'O' Receive Inter-Location P/o, 'D' Receive Dye Order, 'A' Issue Adornment order
    *-- 'E' Receive Adornment order, 'P' Receive Material PO
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
    *CASE loFormSet.lcPType $ 'IRMNODAEPG'
  Case loFormSet.lcPType $ 'IRMNODAEPGW'
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
    *IF .cboReceivingTypes.OldValue $ "SBTLHCFU"
    If Iif(loFormSet.lcPType $ 'MIO',.F.,.cboReceivingTypes.OldValue $ "SBTLHCFUIOM" )
      .cntBatch.Visible = .F.
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
      *xxx
      .grdReceivingLines.Height = .grdReceivingLines.Height + .cntShipment.Height
      .grdReceivingLines.Top = .cntShipment.Top
      .cntShipment.Visible = .F.
    Endif
    .kbPoNo.Enabled = .T.
    If loFormSet.llMFCall And loFormSet.lcPType='M'
      *! B610897,1 MMT 10/26/2014 Cutting ticket receiving allow user to enter style before entering CT#[T20141022.0011][Start]
      *.kbItem.ENABLED = .T.
      *! B610897,1 MMT 10/26/2014 Cutting ticket receiving allow user to enter style before entering CT#[T20141022.0011][End]
    Endif
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
    *.kbPONo.KeyTextBox.SetFocus
    If !loFormSet.llSilentMod
      .kbPoNo.keytextbox.SetFocus
    Endif
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]


    *-- 'B' Receive P/O Batch, 'T' Receive C/T Batch, 'L' Receive Inter Location P/O Batch'
    *-- 'H' Issue Inter Location P/O Batch'
  Case loFormSet.lcPType $ 'BTLH'
    If !(.cboReceivingTypes.OldValue $ 'SCBTLH')
      *xxx
      .grdReceivingLines.Height = .grdReceivingLines.Height - .cntBatch.Height

      .grdReceivingLines.Top = .grdReceivingLines.Top + .cntBatch.Height + 2
    Endif
    .cntBatch.Visible = .T.
    .cntBatch.Enabled = .T.
    .cntShipment.Visible = .F.
    .cntShipment.Enabled = .F.
    .cntBatch.kbBatchNo.Enabled = .T.
    .cntBatch.kbBatchNo.keytextbox.Value = ""
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
    *.cntBatch.kbBatchNo.SetFocus
    If !loFormSet.llSilentMod
      .cntBatch.kbBatchNo.SetFocus
    Endif
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]


    *-- 'S' Receive by Shipment, 'U' Issue Inter-Location PO Shipment
    *-- 'C' Receive Inter Location PO Shipment, 'F' Receive Material Shipment
  Case loFormSet.lcPType $ 'SUCF'
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
    *IF !(.cboReceivingTypes.OldValue $ 'BTLHCSU')
    If !(.cboReceivingTypes.OldValue $ 'BTLHCSUIOM')
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
      *xxx
      .grdReceivingLines.Height = ;
        .grdReceivingLines.Height - .cntShipment.Height

      .grdReceivingLines.Top = .grdReceivingLines.Top + .cntShipment.Height + 2

    Endif
    .cntShipment.Visible = .T.
    .cntShipment.Enabled = .T.
    .cntBatch.Visible = .F.
    .cntBatch.Enabled = .F.
    .cntShipment.kbShipNo.Enabled = .T.
    .cntShipment.kbShipNo.keytextbox.Value = ""

    *! E303848,1 SARA.O 07/09/2017 Saving a ‘Receive by Shipment’ session will allow the user to save the receiving as a batch [Begin]
    *
    If loFormSet.lcPType ='S'
      .cntBatch.Visible = .T.
      .cntBatch.Enabled = .T.
      .cntBatch.kbBatchNo.Enabled = .T.
      .cntBatch.kbBatchNo.keytextbox.Value = ""

      *! E303848,1 SARA.O 07/09/2017 Saving a ‘Receive by Shipment’ session will allow the user to save the receiving as a batch [Begin]
      *!*	      .grdReceivingLines.HEIGHT = .grdReceivingLines.HEIGHT - .cntBatch.HEIGHT
      *!*	      .grdReceivingLines.TOP = .grdReceivingLines.TOP + .cntBatch.HEIGHT + 2
      *!*	      .cntBatch.TOP = .cntBatch.TOP + .cntShipment.HEIGHT + 2
      **B611375,MHM 8/2/2017 po recieving screen grid size shrunk 
      .cntBatch.Top = .cntShipment.Top+ .cntShipment.Height + 2
      .grdReceivingLines.Top = .cntBatch.Top + .cntBatch.Height + 2
      .grdReceivingLines.Height = .shpRegion1.Top - .grdReceivingLines.Top - 2
      **B611375,MHM 8/2/2017 po recieving screen grid size shrunk 
      *lnCntBatchTop, lnGrigLinesHeight, lnGrigLinesTop
    Endif
    *! E303848,1 SARA.O 07/09/2017 Saving a ‘Receive by Shipment’ session will allow the user to save the receiving as a batch [End]

    If loFormSet.lcPType $ 'SCFU'
      *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
      *.cntShipment.kbShipNo.SetFocus
      If !loFormSet.llSilentMod
        .cntShipment.kbShipNo.SetFocus
      Endif
      *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
    Endif
  Endcase
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
  If loFormSet.lcPType $  'IOM' && AND (.cboReceivingTypes.Value <> .cboReceivingTypes.OldValue)
    If !(.cboReceivingTypes.OldValue $ 'SCBTLHIOM')
      .grdReceivingLines.Height = .grdReceivingLines.Height - .cntBatch.Height
      .grdReceivingLines.Top = .grdReceivingLines.Top + .cntBatch.Height + 2
    Endif
    **B611375,MHM 8/2/2017 po recieving screen grid size shrunk 
    .grdReceivingLines.Top = .cntBatch.Top + .cntBatch.Height + 4
    **B611375,MHM 8/2/2017 po recieving screen grid size shrunk 
    .cntBatch.Visible = .T.
    .cntBatch.Enabled = .T.
    .cntShipment.Visible = .F.
    .cntShipment.Enabled = .F.
    .cntBatch.kbBatchNo.Enabled = .T.
    .cntBatch.kbBatchNo.keytextbox.Value = ""
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
    *.cntBatch.kbBatchNo.SetFocus
    If !loFormSet.llSilentMod
      .cntBatch.kbBatchNo.SetFocus
    Endif
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
  Endif
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
Endwith
*-- Assign the business and work order
Do Case
  *-- 'I' Receive P/O, 'S' Receive by Shipment,'B' Receive P/O Batch
Case loFormSet.lcPType $ 'ISB'
  loFormSet.lcBusDoc   = 'P'
  loFormSet.lcWorkOrd  = 'P'

  *-- 'N' Issue Inter-Location P/o, 'O' Receive Inter-Location P/o
  *-- 'L' Receive Inter Location P/O Batch, 'H' Issue Inter Location P/O Batch
  *-- 'U' Issue Inter-Location PO Shipment, 'C' Receive Inter Location PO Shipment
Case loFormSet.lcPType $ 'NOLHUC'
  loFormSet.lcBusDoc   = 'N'
  loFormSet.lcWorkOrd  = 'N'

  *-- 'M' Receive C/T, 'T' Receive C/T Batch
Case loFormSet.lcPType $ 'MT'
  loFormSet.lcBusDoc   = 'P'
  loFormSet.lcWorkOrd  = 'U'

  *-- 'R' Issue Return P/o
Case loFormSet.lcPType = 'R'
  loFormSet.lcBusDoc   = 'R'
  loFormSet.lcWorkOrd  = 'P'

  *-- 'A' Issue Adornment order
Case loFormSet.lcPType = 'A'
  loFormSet.lcBusDoc   = 'P'
  loFormSet.lcWorkOrd  = 'A'

  *-- 'E' Receive Adornment order
Case loFormSet.lcPType = 'D'
  loFormSet.lcBusDoc   = 'P'
  loFormSet.lcWorkOrd  = 'D'

  *-- Receive Material PO and Material Shipment PO
Case loFormSet.lcPType $ 'PF'
  loFormSet.lcBusDoc   = 'P'
  loFormSet.lcWorkOrd  = 'M'

  *-- Issue Material PO
Case loFormSet.lcPType = 'G'
  loFormSet.lcBusDoc   = 'R'
  loFormSet.lcWorkOrd  = 'M'
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
Case loFormSet.lcPType = 'W'
  loFormSet.lcBusDoc   = 'P'
  loFormSet.lcWorkOrd  = 'F'
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
Endcase
*N038893,1 WAM 06/02/2005 Receive Cutting Ticket
*!*	IF loFormSet.lcPType = 'OUCN'
*!*	  loFormSet.AriaForm1.lblPO.Caption = 'Inter-Loc. P/O#'
*!*	ELSE
*!*	  loFormSet.AriaForm1.lblPO.Caption = 'P/O#'
*!*	ENDIF
Do Case
Case loFormSet.lcPType = 'OUCN'
  *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
  *loFormSet.AriaForm1.lblPO.CAPTION = 'Inter-Loc. P/O#'
  loFormSet.ariaform1.lblPO.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_INTERLOC_PO,loFormSet.GetHeaderText("LANG_POSTREC_INTERLOC_PO",loFormSet.HeaderAlias))
  *N000682,1 MMT 11/20/2012 Globalization Changes[End]

Case loFormSet.lcPType $ 'MT'
  *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
  *loFormSet.AriaForm1.lblPO.CAPTION = 'C/T #'
  loFormSet.ariaform1.lblPO.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_CT,loFormSet.GetHeaderText("LANG_POSTREC_CT",loFormSet.HeaderAlias))
  *N000682,1 MMT 11/20/2012 Globalization Changes[End]



  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
Case loFormSet.lcPType = 'W'
  *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
  *  loFormSet.AriaForm1.lblPO.Caption = 'Order #'
  loFormSet.ariaform1.lblPO.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ORDER,loFormSet.GetHeaderText("LANG_POSTREC_ORDER",loFormSet.HeaderAlias))
  *N000682,1 MMT 11/20/2012 Globalization Changes[End]



  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
Otherwise
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loFormSet.AriaForm1.lblPO.CAPTION = 'P/O#'
  loFormSet.ariaform1.lblPO.Caption = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_PO,loFormSet.GetHeaderText("LANG_POSTREC_PO",loFormSet.HeaderAlias))
  *N000682,1 MMT 11/20/2012 Globalization Changes[End]
Endcase
*N038893,1 WAM 06/02/2005 (End)

*-- To assign the aria work order key properties
=lfSetKeyPro(loFormSet,loFormSet.lcPType)

=lfActBrow(loFormSet)

*!*	IF loFormSet.lcPType $ 'RNAH'
*!*	lcnTpMode = IIF(lcPType $ 'RNAH','DISABLE','ENABLE')
*!*	ENDIF

*B611xx [Begin]
If loFormSet.nCntBatchTop = 0
  loFormSet.nCntBatchTop = loFormSet.ariaform1.cntBatch.Top
  loFormSet.nGrigLinesHeight = loFormSet.ariaform1.grdReceivingLines.Height
  loFormSet.nGrigLinesTop = loFormSet.ariaform1.grdReceivingLines.Top
Endif

*B611xx [eND]

Return

*!*************************************************************
*! Name      : lfAddPro
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : function to Add properties to the FormSet.
*!*************************************************************
*! Parameters: loFormSet : Reference to the formset
*!*************************************************************
Function lfAddPro
Lparameters loFormSet

Local lnI
For lnI = 1 To Alen(laAllVar,1)
  If Left(laAllVar[lnI,1],2) = 'la'
    Local lnRow,lnCol,lcACopy
    lnRow = Alen(laAllVar[lnI,1],1)
    lnCol = Alen(laAllVar[lnI,1],2)
    loFormSet.AddProperty(laAllVar[lnI,1]+'['+Alltrim(Str(lnRow))+;
      ','+Alltrim(Str(lnCol))+']')
    lcACopy = '=ACOPY(' + laAllVar[lnI,1] + ',loFormSet.' + laAllVar[lnI,1] + ')'
    &lcACopy.
  Else
    loFormSet.AddProperty(laAllVar[lnI,1],Evaluate(laAllVar[lnI,1]))
  Endif
Endfor
*--end of lfAddPro.

*!*************************************************************
*! Name      : lfObjStatus
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To disable/enable the edit region
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
Function lfObjStatus
Lparameters loFormSet, llStatus

With loFormSet.ariaform1
  Store .F. To .txtstock.Enabled, .txtothers.Enabled, .txtcancel.Enabled,;
    .txtitemDesc.Enabled, .txtPrice.Enabled, .txtDuty.Enabled

  Store llStatus To .kbPoNo.Enabled , .kbPoNo.keyCmd.Enabled , .kbItem.Enabled ,;
    .kbconfiguration.Enabled  , .cboLocations.Enabled ,;
    .txtreference.Enabled     , .txtPattern.Enabled,;
    .txtpriceRate.Enabled     , .txtDutyRate.Enabled,;
    .cmdLineQty.Enabled       , .cmdNew.Enabled,;
    .cmdRemove.Enabled        , .cntShipment.Enabled

  *-- To change the configuration lable and disable it in case dyelot not used in both
  *-- IC and MA
  If loFormSet.lcInvType = "0001"
    If loFormSet.llConfig
      *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
      *.lblConfiguration.CAPTION = "Configuration"
      .lblConfiguration.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_CONFIGURATION,loFormSet.GetHeaderText("LANG_POSTREC_CONFIGURATION",loFormSet.HeaderAlias))
      *N000682,1 11/20/2012 MMT Globlization changes[End]

    Else
      If loFormSet.llDyelot
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *.lblConfiguration.CAPTION = "Dyelot"
        .lblConfiguration.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_DYELOT,loFormSet.GetHeaderText("LANG_POSTREC_DYELOT",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]

        .kbconfiguration.llVlDyelot = .T.
      Else
        .lblConfiguration.Visible = .F.
        .kbconfiguration.Visible = .F.
      Endif
    Endif
  Else
    If loFormSet.llFabDye
      *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
      *.lblConfiguration.CAPTION = "Dyelot"
      .lblConfiguration.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_DYELOT,loFormSet.GetHeaderText("LANG_POSTREC_DYELOT",loFormSet.HeaderAlias))
      *N000682,1 11/20/2012 MMT Globlization changes[End]
      .kbconfiguration.llVlDyelot = .T.
    Else
      .lblConfiguration.Visible = .F.
      .kbconfiguration.Visible = .F.
    Endif
  Endif

  *-- To be shown only in the receiving of materials in this phase because
  *-- the pattern does not exist in the po screen
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
  *IF !(loFormSet.lcPType $ 'PF')
  If !(loFormSet.lcPType $ 'PFW')
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
    .lblPattern.Visible = .F.
    .lblSimClo6.Visible = .F.
    .txtPattern.Visible = .F.
  Endif

  *-- If not keep track of bins
  If !loFormSet.llWareLoc
    .chkBins.Visible = .F.
  Else
    .chkBins.Enabled = llStatus
  Endif

  *-- IF not multi warehouse or 'N' Issue Inter-location, 'H' Issue Inter-Location Shipment
  If  loFormSet.llWareHous Or loFormSet.lcPType $ "NH"
    .cboLocations.Enabled = .F.
  Endif

Endwith

*!*************************************************************
*! Name      : lfShowScr
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To control the disabling/enabling of the screen
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
Function lfShowScr
Lparameters loFormSet

=lfObjStatus(loFormSet,.F.)

With loFormSet.ariaform1
  Do Case
    *-- 'B' Receive P/O Batch, 'T' Receive C/T Batch, 'L' Receive Inter Location P/O Batch'
    *-- 'H' Issue Inter Location P/O Batch'
  Case loFormSet.lcPType $ 'BTLH'
    .cntBatch.Enabled = .F.

    *-- 'I' Receive P/O, 'R' Issue Return P/o, 'M' Receive C/T, 'N' Issue Inter-Location P/o
    *-- 'O' Receive Inter-Location P/o, 'D' Receive Dye Order, 'A' Issue Adornment order
    *-- 'E' Receive Adornment order, 'P' Receive Material PO
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
    *CASE loFormSet.lcPType $ 'IRMNODAEPG'
  Case loFormSet.lcPType $ 'IRMNODAEPGW'
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
    If !(loFormSet.lcPType $ 'IOM')
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]

      .cntBatch.Enabled = .F.
      .cntBatch.Visible = .F.
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
    Endif
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
    .cntShipment.Enabled = .F.
    .cntShipment.Visible = .F.
    If (!loFormSet.llLinkToGl Or !Empty(loFormSet.lcGLFYear+loFormSet.lcGLPeriod))
      .cmdNew.Enabled = .T.
      If !Eof(loFormSet.lcTmpLine)
        *IF loFormSet.lcInvType = "0002"
        *-- 'R' Issue Return P/O, 'N' Issue Inter-Location P/O
        *-- 'U' Issue Inter Location P/O Shipment, 'O' Receive Inter-Location P/O
        *-- 'C' Receive Inter Location P/O Shipment, 'L' Receive Inter Location P/O Batch
        *-- 'A' Issue Adornment order, 'E' Receive Adornment order
        If !(loFormSet.lcPType $ 'RNOLCAEU')

          *B000211,1 WAM 03/03/2005 Disable configuration button
          *.kbconfiguration.Enabled = (EVALUATE(loFormSet.lcTmpLine+'.cDye_Flg') = "Y")
          .kbconfiguration.Enabled = .F.
          *B000211,1 WAM (End)

        Endif
        .cmdRemove.Enabled = .T.
        If loFormSet.lcPType $ 'NA'
          .cboLocations.Enabled = .F.
          .chkBins.Enabled = .F.
        Else
          .cboLocations.Enabled = .T.
          .chkBins.Enabled = .T.
        Endif
        .txtreference.Enabled = .T.
        If loFormSet.llWareLoc And Used(loFormSet.lcTemLoc)
          .chkBins.Value = Iif(Seek(Evaluate(loFormSet.lcTmpLine+'.Style')+;
            EVALUATE(loFormSet.lcTmpLine+'.cWareCode'),;
            loFormSet.lcTemLoc) ,.T.,.F.)
        Endif
        *! N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
        *IF loFormSet.lcPType = 'P'
        If loFormSet.lcPType $ 'WP'
          *! N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][End]
          .txtPattern.Enabled = .T.
        Endif
      Endif
    Endif
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
    If (loFormSet.lcPType $ 'IOM')
      If !Empty(loFormSet.ariaform1.cntBatch.kbBatchNo.keytextbox.Value)
        If loFormSet.llApproveBatch And loFormSet.ariaform1.cntBatch.cboBatchStatus.Value <> 'A'
          loFormSet.ariaform1.cntBatch.cboBatchStatus.Enabled = .T.
        Else
          loFormSet.ariaform1.cntBatch.cboBatchStatus.Enabled = .F.
        Endif
        loFormSet.ariaform1.cntBatch.kbBatchNo.Enabled =.F.
        If (loFormSet.ariaform1.cntBatch.cboBatchStatus.Value $ 'XP') Or (loFormSet.ariaform1.cntBatch.cboBatchStatus.Value ='A' And !loFormSet.llApproveBatch)
          lfObjStatus(loFormSet,.F.)
          loFormSet.ariaform1.cntBatch.DtpickerBatchDate.Enabled =!(loFormSet.ariaform1.cntBatch.cboBatchStatus.Value $ 'XP')
          loFormSet.ariaform1.cntBatch.txtBatchDesc.Enabled = !(loFormSet.ariaform1.cntBatch.cboBatchStatus.Value $ 'XP')
          If (loFormSet.ariaform1.cntBatch.cboBatchStatus.Value <> 'A' And loFormSet.llApproveBatch) And !(loFormSet.ariaform1.cntBatch.cboBatchStatus.Value $ 'XP')
            loFormSet.ariaform1.cntBatch.cboBatchStatus.Enabled =.T.
          Else
            loFormSet.ariaform1.cntBatch.cboBatchStatus.Enabled =.F.
          Endif
        Endif
      Endif
    Endif
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]

    *-- 'S' Receive by Style PO Shipment, 'U' Issue Inter-Location PO Shipment
    *-- 'C' Receive Inter-Location PO Shipment, 'F' Receive Material Shipment
  Case loFormSet.lcPType $ 'SUCF'
    .cntShipment.Enabled = .T.
    .cntShipment.kbShipNo.Enabled = .F.
    .cmdNew.Enabled = .F.
    If !Eof(loFormSet.lcTmpLine)
      Store .T. To .cntShipment.dtpickerShpEntered.Enabled,;
        .cntShipment.dtpickerShpETA.Enabled,;
        .cntShipment.txtShpCartons.Enabled ,;
        .cntShipment.txtShpAirWay.Enabled  ,;
        .cntShipment.txtShpReference.Enabled

      .cmdRemove.Enabled = .T.

      *B128741,1 KHM 06/30/2005 Enable the location in case of shipment [Begin]
      .cboLocations.Enabled = .T.
      *B128741,1 KHM 06/30/2005 [End]

      If loFormSet.llWareLoc
        .chkBins.Enabled = .T.
        If Used(loFormSet.lcTemLoc)
          .chkBins.Value = Iif(Seek(Evaluate(loFormSet.lcTmpLine+'.Style')+;
            EVALUATE(loFormSet.lcTmpLine+'.cWareCode'),;
            loFormSet.lcTemLoc) ,.T.,.F.)
        Endif

      Endif
      .txtreference.Enabled = .T.

      If loFormSet.lcPType = 'F'
        .txtPattern.Enabled = .T.
      Endif


    Endif
  Endcase


  *!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [START]
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
  *!*	  IF loFormSet.lcInvType="0002" AND loFormSet.lcPType $ 'FPG'
  *!*	    IF loFormSet.lcPType $ 'GFP' AND loFormSet.lcCostMthM = 'L' AND loFormSet.llTrkRolls AND USED(loFormSet.lcTmpItem) AND ;
  *!*	        SEEK(loFormSet.lcInvType+EVALUATE(loFormSet.lcTmpLine+'.Style'),loFormSet.lcTmpItem);
  *!*	        AND EVALUATE(loFormSet.lcTmpItem+'.LTRKROLLS')
  If loFormSet.lcInvType="0002" And loFormSet.lcPType $ 'FPGW'
    If loFormSet.lcPType $ 'WGFP' And loFormSet.lcCostMthM = 'L' And loFormSet.llTrkRolls And Used(loFormSet.lcTmpItem) And ;
        SEEK(loFormSet.lcInvType+Evaluate(loFormSet.lcTmpLine+'.Style'),loFormSet.lcTmpItem);
        AND Evaluate(loFormSet.lcTmpItem+'.LTRKROLLS')
      *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][End]
      loFormSet.ariaform1.cmdRolls.Enabled = .T.
      *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
      *loFormSet.AriaForm1.cmdRolls.CAPTION = 'Ro\<lls'
      loFormSet.ariaform1.cmdRolls.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ROLLS_1,loFormSet.GetHeaderText("LANG_POSTREC_ROLLS_1",loFormSet.HeaderAlias))
      *N000682,1 MMT 11/20/2012 Globalization Changes[End]
    Else
      If (loFormSet.lcPType = 'G' And loFormSet.lcCostMthM = 'L' And (!loFormSet.llTrkRolls Or (loFormSet.llTrkRolls And Used(loFormSet.lcTmpItem) And ;
          SEEK(loFormSet.lcInvType+Evaluate(loFormSet.lcTmpLine+'.Style'),loFormSet.lcTmpItem);
          AND !Evaluate(loFormSet.lcTmpItem+'.LTRKROLLS'))))
        loFormSet.ariaform1.cmdRolls.Enabled = .T.
        *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
        *loFormSet.AriaForm1.cmdRolls.CAPTION = '\<Lots'
        loFormSet.ariaform1.cmdRolls.Caption =Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_LOTS,loFormSet.GetHeaderText("LANG_POSTREC_LOTS",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]


      Else

        loFormSet.ariaform1.cmdRolls.Enabled = .F.
      Endif
    Endif
  Else
    loFormSet.ariaform1.cmdRolls.Enabled = .F.
    loFormSet.ariaform1.cmdRolls.Visible = .F.
  Endif
  *!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]


  *-- If return style PO or return material PO
  If loFormSet.lcPType $ 'RG' And !Empty(Evaluate(loFormSet.lcPosHdr+'.CPONO'))
    .cboLocations.Enabled = .F.
    .chkBins.Enabled = .F.
  Endif
  *-- If style PO and multi-currency and change exchange rate and price currency
  *-- or duty currency is not equal to the base currency and !eof.
  If (!loFormSet.llMFCall And loFormSet.llMulCurr And loFormSet.llEditExRt) And ;
      (loFormSet.lcCur1 <> oAriaApplication.BaseCurrency Or;
      loFormSet.lcCur2 <> oAriaApplication.BaseCurrency) And !Eof(loFormSet.lcTmpLine)
    If loFormSet.lcCur1 <> oAriaApplication.BaseCurrency
      .txtpriceRate.Enabled = .T.
    Endif
    If loFormSet.lcCur2 <> oAriaApplication.BaseCurrency
      .txtDutyRate.Enabled = .T.
    Endif
  Endif

  If !Eof(loFormSet.lcTmpLine)
    *-- Edit Quantity button.
    .cmdLineQty.Enabled = .T.
    If loFormSet.llConfig And Evaluate(loFormSet.lcTmpLine+'.cDye_Flg') = "Y"
      .kbconfiguration.lcstylecode = Evaluate(loFormSet.lcTmpLine+'.Style')
      .kbconfiguration.lcwarecode  = Evaluate(loFormSet.lcTmpLine+'.cWareCode')
    Endif

    If Type('loFormSet.lcPosLn') = 'C' And Used(loFormSet.lcPosLn)
      =Seek(loFormSet.lcBusDoc+loFormSet.lcWorkOrd+Evaluate(loFormSet.lcTmpLine+'.PO')+;
        loFormSet.lcInvType+Evaluate(loFormSet.lcTmpLine+'.Style')+;
        STR(Evaluate(loFormSet.lcTmpLine+'.LineNo'),6), loFormSet.lcPosLn)
    Endif
    loFormSet.lcPoNo   = Evaluate(loFormSet.lcTmpLine+'.PO')
    loFormSet.lcStyle  = Evaluate(loFormSet.lcTmpLine+'.Style')
    loFormSet.lcDyelot = Padr(Evaluate(loFormSet.lcTmpLine+'.Dyelot'),10)
  Endif
Endwith
Return

*!*************************************************************
*! Name      : lfwBrow
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : When valid function for browse.
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
Function lfwBrow
Lparameters loFormSet
=lfReadLine(loFormSet,Eof())
Return

*!*************************************************************
*! Name      : lfReadLine
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To get the line information
*!*************************************************************
*! Parameters: loFormSet : FormSet, llClearLn
*!*************************************************************
Function lfReadLine
Lparameters loFormSet,llClearLn
Local lnAlias

lnAlias = Select()

If !llClearLn
  Select (loFormSet.lcTmpLine)
  loFormSet.lnWare  = Iif(loFormSet.llCMInstld And loFormSet.llPOSale,1,;
    ASCAN(loFormSet.laWare,cWareCode,1))

  If loFormSet.llMulCurr And !loFormSet.llMFCall
    *-Get price currency and rate.
    loFormSet.lcCur1  = cPriceCur
    loFormSet.lnRate1 = nLanPrRat
    *-Get duty currency and rate.
    loFormSet.lcCur2  = cDutyCur
    loFormSet.lnRate2 = nLanDuRat
  Endif
  *-- If receive to damage or cancel disable the warehouse
  If loFormSet.lnTotDam <> 0 Or loFormSet.lnTotCan <> 0
    loFormSet.ariaform1.cboLocations.Enabled = .F.
  Endif

Else
  Store ' ' To loFormSet.lcCur1,loFormSet.lcCur2
  Store 0   To loFormSet.lnRate1,loFormSet.lnRate2
  Store 1   To loFormSet.lnCurrUnt1,loFormSet.lnCurrUnt2
Endif
=lfShowScr(loFormSet)
loFormSet.Refresh
Select(lnAlias)
Return

*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To create the temporary files
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
Function lfCreatTmp
Lparameters loFormSet

Select (loFormSet.lcPosLn)
=Afields(laFStru)
lnFStru = Alen(laFStru,1)
*B126833,1 WAM 04/03/2005 add new button to edit landed cost for styles has no detail costing
*DIMENSION laFStru[lnFStru+16,18]
Dimension laFStru[lnFStru+17,18]
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

Store 'N' To laFStru[lnFStru+1,2],laFStru[lnFStru+2,2],;
  laFStru[lnFStru+3,2],laFStru[lnFStru+4,2],;
  laFStru[lnFStru+6,2],laFStru[lnFStru+8,2]

Store 'C' To laFStru[lnFStru+5,2],laFStru[lnFStru+10,2],laFStru[lnFStru+14,2],;
  laFStru[lnFStru+15,2],laFStru[lnFStru+16,2]

*B126833,1 WAM 04/03/2005 add new button to edit landed cost for styles has no detail costing
*STORE 'L' TO laFStru[lnFStru+7,2],laFStru[lnFStru+9,2],;
laFStru[lnFStru+11,2],laFStru[lnFStru+13,2],;
laFStru[lnFStru+12,2]
Store 'L' To laFStru[lnFStru+7,2],laFStru[lnFStru+9,2],;
  laFStru[lnFStru+11,2],laFStru[lnFStru+13,2],;
  laFStru[lnFStru+12,2],laFStru[lnFStru+17,2]
*B126833,1 WAM 04/03/2005 (End)

*! B608824,1 MMT 03/30/2009 Fix bug of Wrong Values while adjustemnt Material[Start]
*IF loFormSet.lcPType $ 'PF'
*N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
*IF loFormSet.lcPType $ 'PFG'
If loFormSet.lcPType $ 'PFGW'
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
  *! B608824,1 MMT 03/30/2009 Fix bug of Wrong Values while adjustemnt Material[End]
  Store  12 To laFStru[lnFStru+1,3],laFStru[lnFStru+2,3],;
    laFStru[lnFStru+3,3],laFStru[lnFStru+4,3]
Else
  Store  6  To laFStru[lnFStru+1,3],laFStru[lnFStru+2,3],;
    laFStru[lnFStru+3,3],laFStru[lnFStru+4,3]
Endif
Store  6  To laFStru[lnFStru+6,3],laFStru[lnFStru+10,3]

Store  3  To laFStru[lnFStru+5,3]

Store  2  To laFStru[lnFStru+8,3]

*B126833,1 WAM 04/03/2005 add new button to edit landed cost for styles has no detail costing
*STORE  1  TO laFStru[lnFStru+7,3],laFStru[lnFStru+9,3],;
laFStru[lnFStru+11,3],laFStru[lnFStru+13,3],;
laFStru[lnFStru+12,3],laFStru[lnFStru+16,3]
Store  1  To laFStru[lnFStru+7,3],laFStru[lnFStru+9,3],;
  laFStru[lnFStru+11,3],laFStru[lnFStru+13,3],;
  laFStru[lnFStru+12,3],laFStru[lnFStru+16,3],laFStru[lnFStru+17,3]
*B126833,1 WAM 04/03/2005 (End)

Store 60  To laFStru[lnFStru+14,3]

Store 19  To laFStru[lnFStru+15,3]

*! B608824,1 MMT 03/30/2009 Fix bug of Wrong Values while adjustemnt Material[Start]
*IF loFormSet.lcPType $ 'PF'
*N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
*IF loFormSet.lcPType $ 'PFG'
If loFormSet.lcPType $ 'PFGW'
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
  *! B608824,1 MMT 03/30/2009 Fix bug of Wrong Values while adjustemnt Material[End]
  Store  3  To laFStru[lnFStru+1,4],laFStru[lnFStru+2,4],;
    laFStru[lnFStru+3,4],laFStru[lnFStru+4,4]
Else
  Store  0  To laFStru[lnFStru+1,4],laFStru[lnFStru+2,4],;
    laFStru[lnFStru+3,4],laFStru[lnFStru+4,4]
Endif
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
Store  0  To laFStru[lnFStru+5,4],laFStru[lnFStru+6,4],;
  laFStru[lnFStru+7,4],laFStru[lnFStru+8,4],;
  laFStru[lnFStru+9,4],laFStru[lnFStru+10,4],;
  laFStru[lnFStru+11,4],laFStru[lnFStru+13,4],;
  laFStru[lnFStru+12,4],laFStru[lnFStru+14,4],;
  laFStru[lnFStru+15,4],laFStru[lnFStru+16,4],laFStru[lnFStru+17,4]
For lnI = 7 To 16
  For lnJ = 1 To 17
    Store '' To laFStru[lnFStru+lnJ,lnI]
  Endfor
Endfor
For lnJ = 1 To 17
  Store 0 To laFStru[lnFStru+lnJ,17],laFStru[lnFStru+lnJ,18]
Endfor
*B126833,1 WAM 04/03/2005 (End)

Dimension laTags[3,2]
Dime laTags[5,2]
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
If Ascan(loFormSet.laEvntTrig,Padr('ADDFILDS',10),1,Alen(loFormSet.laEvntTrig,1),1) > 0
  =gfDoTriger("POSTREC",Padr("ADDFILDS",10))
Endif
*T20071102.0018(C200876) TMI [End  ]

=gfCrtTmp(loFormSet.lcTmpLine,@laFStru,@laTags)
Select (loFormSet.lcTmpLine)
Set Order To Tag TmpLine1

*--Warehouse location tmp file.
If loFormSet.llWareLoc
  Select WhsLoc
  =Afields(laFStru)
  =gfCrtTmp(loFormSet.lcTemLoc,@laFStru,'Style+cWareCode+cLocation',loFormSet.lcTemLoc)
  =CursorSetProp("Buffering",5,loFormSet.lcTemLoc)
Endif

*T20071102.0018(C200876) TMI [Start] create the lcBinLine temp file
If Ascan(loFormSet.laEvntTrig,Padr('CRTBINLN',10),1,Alen(loFormSet.laEvntTrig,1),1) > 0 ;
    .And. gfDoTriger('POSTREC',Padr('ISUSEBIN',10))
  Private lnbinOldAls
  lnbinOldAls=Select(0)
  Select (loFormSet.lcTmpLine)
  =gfDoTriger("POSTREC",Padr("CRTBINLN",10))
  Select(lnbinOldAls)
Endif
*T20071102.0018(C200876) TMI [End  ]

Return

*!*************************************************************
*! Name      : lfvTCode
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To validate the transaction code by shipment or batch
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
Function lfvTCode
*! E303848,1 SARA.O 07/09/2017 Saving a ‘Receive by Shipment’ session will allow the user to save the receiving as a batch [Begin]
*LPARAMETERS loFormSet, llBrowse
Lparameters loFormSet, llBrowse, llfromBatch
*! E303848,1 SARA.O 07/09/2017 Saving a ‘Receive by Shipment’ session will allow the user to save the receiving as a batch [End]
Local lnAlias


With loFormSet.ariaform1
  lnAlias = Select()
  Do Case
    *-- 'S' Style PO Shipment, 'U' Issue Inter-Location PO Shipment
    *-- 'F' Receive Material Shipment, 'C' Receive Inter-Location PO Shipment
    *CASE loFormSet.lcPType $ 'SUFC' AND !lfvShipmnt(loFormSet)
    *! E303848,1 SARA.O 07/09/2017 Saving a ‘Receive by Shipment’ session will allow the user to save the receiving as a batch [Begin]
    *CASE loFormSet.lcPType $ 'SUFC' AND !lfvShipmnt(loFormSet)
  Case loFormSet.lcPType $ 'SUFC' And !llfromBatch And !lfvShipmnt(loFormSet)
    *! E303848,1 SARA.O 07/09/2017 Saving a ‘Receive by Shipment’ session will allow the user to save the receiving as a batch [End]
    Select(lnAlias)
    Return .F.

    *-- 'B' Receive P/O Batch, 'T' Receive C/T Batch,
    *-- 'L' Receive Inter Location P/O Batch', 'H' Issue Inter Location P/O Batch'
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
    *CASE loFormSet.lcPType $ 'BTLH' AND !lfvBatch(loFormSet)
    *CASE loFormSet.lcPType $ 'MIOBTLH'  AND !lfvBatch(loFormSet,llBrowse)
    *! E303848,1 SARA.O 07/09/2017 Saving a ‘Receive by Shipment’ session will allow the user to save the receiving as a batch [Begin]
    * CASE loFormSet.lcPType $ 'MIOBTLH' AND !lfvBatch(loFormSet,llBrowse)
  Case ((loFormSet.lcPType $ 'MIOBTLH') Or (llfromBatch And loFormSet.lcPType='S'))  And !lfvBatch(loFormSet,llBrowse)
    *! E303848,1 SARA.O 07/09/2017 Saving a ‘Receive by Shipment’ session will allow the user to save the receiving as a batch [End]
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
    Select(lnAlias)
    Return .F.
  Endcase

  Store .F. To .dtPickerPostingDate.Enabled, .dtpickerReceivingDate.Enabled
  .cboReceivingTypes.Enabled = .F.

  Select(lnAlias)
Endwith
Return .T.

*!*************************************************************
*! Name      : lfvPO
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To validate the work order #
*!*************************************************************
*! Parameters: loFormSet : FormSet, llBrowse
*!*************************************************************
Function lfvPO
Lparameters loFormSet, llBrowse
Local lnAlias, lcPrvAlis, llAbort, lcTxtMsg1, lcTxtMsg2, lcTxtMsg2, lcSqlStatement
Private lcPoTtle, lcBrFields
loFormSet.lcPoNo = loFormSet.ariaform1.kbPoNo.keytextbox.Value
If Empty(loFormSet.lcPoNo)
  loFormSet.ariaform1.kbPoNo.keytextbox.Value = ""
  If !Eof(loFormSet.lcTmpLine)
    *-- To assign object's control source.
    =lfCntrSour(loFormSet,.T.)
    =lfwBrow(loFormSet)
  Endif
  Return
Endif

lnAlias = Select()

llAbort  = .F.
*N038893,1 WAM 06/02/2005 Receive Cutting Ticket
*lcPoTtle = IIF(loFormSet.lcPType$'RG','Return P/O',IIF(loFormSet.lcPType='D','Dye Order',;
IIF(loFormSet.lcPType='A','Adornment Order',IIF(loFormSet.lcPType$'NOCLU','Inter-Location P/O','P/O'))))
*N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
*!*	lcPoTtle = IIF(loFormSet.lcPType$'RG','Return P/O',IIF(loFormSet.lcPType='D','Dye Order',;
*!*	  IIF(loFormSet.lcPType='A','Adornment Order',;
*!*	  IIF(loFormSet.lcPType$'NOCLU','Inter-Location P/O',;
*!*	  IIF(loFormSet.lcPType$'MT','Cutting ticket','P/O')))))
*N000682,1 11/20/2012 MMT Globlization changes[Start]
lcPoTtle = Iif(loFormSet.lcPType$'RG',;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_RETPO,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_RETPO",loFormSet.HeaderAlias)),Iif(loFormSet.lcPType='D',;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_DYEORDER,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_DYEORDER",loFormSet.HeaderAlias)),;
  IIF(loFormSet.lcPType='A',;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_ADOR,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_ADOR",loFormSet.HeaderAlias)),;
  IIF(loFormSet.lcPType$'NOCLU',;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_INTERPO,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_INTERPO",loFormSet.HeaderAlias)),;
  IIF(loFormSet.lcPType$'MT',;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_CT,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_CT",loFormSet.HeaderAlias)),;
  IIF(loFormSet.lcPType='W',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MMO_MMO,loFormSet.GetHeaderText("LANG_POSTREC_MMO_MMO",loFormSet.HeaderAlias)),;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_PO,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_PO",loFormSet.HeaderAlias))))))))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
*N038893,1 WAM 06/02/2005 (End)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*!*	lcTxtMsg1 = IIF(loFormSet.llIssue,'issuing','receiving')
*!*	lcTxtMsg2 = IIF(loFormSet.llIssue,'issue','receive')
lcTxtMsg1 = Iif(loFormSet.llIssue,;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_ISSUING,loFormSet.GetHeaderText("LANG_POSTREC_MSG_ISSUING",loFormSet.HeaderAlias)),;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_RECEVING,loFormSet.GetHeaderText("LANG_POSTREC_MSG_RECEVING",loFormSet.HeaderAlias)))
lcTxtMsg2 = Iif(loFormSet.llIssue,;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_ISS,loFormSet.GetHeaderText("LANG_POSTREC_MSG_ISS",loFormSet.HeaderAlias)),;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_REC,loFormSet.GetHeaderText("LANG_POSTREC_MSG_REC",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[END]
Do While .T.
  Do Case
  Case Evaluate(loFormSet.lcPosHdr+'.Status') = 'S'
    *--XXX status is XXX. Therefore,no receivings can be done.
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *= gfModalGen('INM34055B42000','DIALOG',lcPoTtle+'|'+'Closed'+'|'+lcTxtMsg1)
    = gfModalGen('INM34055B42000','DIALOG',lcPoTtle+'|'+;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_CLOSED,loFormSet.GetHeaderText("LANG_POSTREC_MSG_CLOSED",loFormSet.HeaderAlias))+'|'+lcTxtMsg1)
    *N000682,1 11/20/2012 MMT Globlization changes[END]
    llAbort=.T.
    Exit
  Case Evaluate(loFormSet.lcPosHdr+'.Status') = 'B'
    *--XXX status is XXX. Therefore,no receivings can be done.
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *= gfModalGen('INM34055B42000','DIALOG',lcPoTtle+'|'+'Bid'+'|'+lcTxtMsg1)
    = gfModalGen('INM34055B42000','DIALOG',lcPoTtle+'|'+;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_BID,loFormSet.GetHeaderText("LANG_POSTREC_MSG_BID",loFormSet.HeaderAlias))+'|'+lcTxtMsg1)
    *N000682,1 11/20/2012 MMT Globlization changes[END]
    llAbort=.T.
    Exit
  Case Evaluate(loFormSet.lcPosHdr+'.Status') = 'H'
    *--XXX status is Hold since a cost sheet has not been created yet. Therefore,no receivings can be done.
    = gfModalGen('INM34056B42000','DIALOG',lcPoTtle)
    llAbort=.T.
    Exit
  Case Evaluate(loFormSet.lcPosHdr+'.Status') = 'X'
    *--XXX has been canceled. Not allowed to receive.
    = gfModalGen('INM34057B42000','DIALOG',lcPoTtle+'|'+lcTxtMsg2)
    llAbort=.T.
    Exit
  Case Evaluate(loFormSet.lcPosHdr+'.Status') = 'C'
    If loFormSet.lcPType $ 'NA'
      *--XXX status is complete. Therefore,no issuings can be done.
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *= gfModalGen('INM34055B42000','DIALOG',lcPoTtle+'|'+'complete'+'|'+lcTxtMsg1)
      = gfModalGen('INM34055B42000','DIALOG',lcPoTtle+'|'+;
        IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_COMPLETE,loFormSet.GetHeaderText("LANG_POSTREC_MSG_COMPLETE",loFormSet.HeaderAlias))+'|'+lcTxtMsg1)
      *N000682,1 11/20/2012 MMT Globlization changes[END]
      llAbort=.T.
      Exit
    Else
      *--XXX is completely received. Do you wish to continue ?,\<Yes;\<No
      *-- 'O' Receive inter-location PO
      If loFormSet.lcPType = 'O'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *= gfModalGen('INM00000B00000','','','',lcPoTtle +' is completely received. Cannot proceed.')
        = gfModalGen('INM00000B00000','','','',lcPoTtle +' '+;
          IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_ISCOMPLTELYRECEVIED,loFormSet.GetHeaderText("LANG_POSTREC_MSG_ISCOMPLTELYRECEVIED",loFormSet.HeaderAlias)))
        *N000682,1 11/20/2012 MMT Globlization changes[END]
        llAbort=.T.
        Exit
      Else
        If gfModalGen('INM34058B42002','DIALOG',lcPoTtle) = 2
          llAbort=.T.
          Exit
        Endif
      Endif
    Endif
  Endcase
  *-- Get the lines from POSLN
  lcSqlStatement  =  "SELECT  POSLN.*, POSHDR.cPriceCur, POSHDR.cDutyCur, POSHDR.Status, POSHDR.CPONO "+;
    "FROM POSLN posln (INDEX=POSLN) inner join POSHDR (INDEX=POSHDR) "+;
    "ON POSHDR.cBusDocu = POSLN.cBusDocu AND POSHDR.cStyType = POSLN.cStyType "+;
    "AND  POSHDR.PO = POSLN.PO "+;
    "WHERE POSLN.cBusDocu = '" + loFormSet.lcBusDoc +;
    "' AND POSLN.cStyType = '" + loFormSet.lcWorkOrd +;
    "' AND POSLN.PO = '" + loFormSet.lcPoNo +;
    "' AND POSLN.cInvType='"+loFormSet.lcInvType + "'"

  =lfOpenSql(lcSqlStatement,'POSLN',loFormSet.lcPosLn, "","",.F.)
  Dimension laIndex[1,2]
  laIndex = ''
  laIndex[1,1] = 'cBusDocu+cStyType+PO+cInvType+Style+STR(LineNo,6)+TranCd'
  laIndex[1,2] = 'POSLN'
  =lfSetIndex(loFormSet.lcPosLn,@laIndex)

  *-- Set the item quality
  Select (loFormSet.lcPosLn)
  Locate
  loFormSet.ariaform1.kbItem.cQuality = Evaluate(loFormSet.lcPosLn+'.cStyGrade')
  lcPrvAlis = Alias()
  Select (loFormSet.lcPosLn)
  *-- IF the Po is included in a shippment , cann't receive by PO [Begin]
  *-- 'I' Receive Style PO, 'O' Receive Inter location PO
  *-- 'P' Receive Material PO, 'N' Issue Inter location PO
  If loFormSet.lcPType $ 'IOPN'
    Locate For TranCd = '3'
    If Found()
      *-- This XX is included in shipment XX. You can either XX by shipment,
      *-- or remove the XX from this shipment.
      *=gfModalGen('INM34181B00000','DIALOG',EVALUATE(loFormSet.lcPosLn+'.ShipNo'))
      *N000682,1 MMT 11/21/2012 globalization Changes [Start]
      *!*	      lcTxtMsg1 = IIF(loFormSet.lcPType $ 'NO','Inter-Location P/O','P/O')
      *!*	      lcTxtMsg2 = ALLTRIM(EVALUATE(loFormSet.lcPosLn+'.ShipNo'))
      *!*	      lcTxtMsg3 = IIF(loFormSet.lcPType = 'N','issue','receive')
      lcTxtMsg1 = Iif(loFormSet.lcPType $ 'NO',;
        IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_INTERPO,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_INTERPO",loFormSet.HeaderAlias)),;
        IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_PO,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_PO",loFormSet.HeaderAlias)))
      lcTxtMsg2 = Alltrim(Evaluate(loFormSet.lcPosLn+'.ShipNo'))
      lcTxtMsg3 = Iif(loFormSet.lcPType = 'N',;
        IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_ISS,loFormSet.GetHeaderText("LANG_POSTREC_MSG_ISS",loFormSet.HeaderAlias)),;
        IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_REC,loFormSet.GetHeaderText("LANG_POSTREC_MSG_REC",loFormSet.HeaderAlias)))
      *N000682,1 MMT 11/21/2012 globalization Changes [END]
      =gfModalGen('INM34204B00000','DIALOG',lcTxtMsg1+'|'+lcTxtMsg2+'|'+lcTxtMsg3+'|'+lcTxtMsg1)
      *B000121,1 WAM 03/03/2005 Do not continue
      *llAbort=.T.
      *B000121,1 WAM 03/03/2005 (End)
      Exit
    Endif
    Locate
    Select (lcPrvAlis)
  Endif
  *-- Check if this PO has BOM materials.
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
  *IF !(loFormSet.lcPType $ 'RDAEPFNOG') AND loFormSet.llImpCost
  If !(loFormSet.lcPType $ 'RDAEPFNOGW') And loFormSet.llImpCost
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][End]
    llMatExist = .F.
    Do Case
      *-- 'I' Receive P/O
    Case loFormSet.lcPType $ 'ISB'
      lcCstShtType = 'I'

      *-- 'M' Receive C/T
    Case loFormSet.lcPType $ 'MT'
      lcCstShtType = 'M'

    Endcase

    *-- Check if there a work order cost sheet and there is a material,
    *-- style component or trim and inventory maintained.
    lcSqlStatement = "SELECT cImTyp, CutTkt FROM CTKTBOM [INDEX=CTKTBOM] "+;
      " WHERE cImTyp = '" + lcCstShtType + "' AND "+;
      "CutTkt ='" + loFormSet.lcPoNo +;
      "' AND (cCatgTyp = 'F' OR cCatgTyp = 'S' OR "+;
      "(cCatgTyp = 'T' AND Trim_Invt=1))"

    =lfWOrdBOM(lcSqlStatement,.T.,.F.,.F.,.F.,loFormSet)
    If Used(loFormSet.lcCTktBom)
      Select (loFormSet.lcCTktBom)
      Locate
      llMatExist = !Eof()
    Endif

    Select (loFormSet.lcPosLn)
    If llMatExist
      *-- Check if there an issued item.
      lcSqlStatement = "SELECT cImTyp, cTktNo FROM BOMCOST [INDEX=POBOMCLS] "+;
        " WHERE cImTyp = '" + lcCstShtType + "' AND "+;
        "cTktNo ='" + loFormSet.lcPoNo + "'"
      =lfWOrdBOM(lcSqlStatement,.F.,.T.,.F.,.F.,loFormSet)

      If Used(loFormSet.lcBomCost)
        Select(loFormSet.lcBomCost)
        Locate
        *--No cost items have been applied against this P/o. Are you sure you want to
        *-- receive ?
        *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
        *IF EOF() AND gfModalGen('QRM34071B42002','DIALOG') = 2
        If Eof() And gfModalGen(Iif(loFormSet.lcPType = 'M','QRM38240B42002','QRM34071B42002'),'DIALOG') = 2
          *N038893,1 WAM 06/02/2005 (End)

          llAbort=.T.
          Exit
        Endif
      Endif
    Endif
  Endif
  *B000109,1 WAM 03/05/2005 Lock PO header
  If .F.
    Select (loFormSet.lcTmpLine)
    lclckKey = PO+Style+Dyelot+cWareCode+Str(Lineno,6)+cCarton+TranCd
    If !Seek(Evaluate(loFormSet.lcPosHdr+'.po'),loFormSet.lcTmpLine,'TmpLine3')
      If Evaluate(loFormSet.lcPosHdr+'.lLok_Stat')
        If Alltrim(Evaluate(loFormSet.lcPosHdr+'.cLok_User')) = Alltrim(oAriaApplication.User_ID)
          If gfModalGen("INM00240B00006","ALERT")=2
            llAbort=.T.
            =Seek(lclckKey,loFormSet.lcTmpLine,'TmpLine3')
            Exit
          Endif
        Else
          =gfModalGen("INM00028B00000","ALERT",oAriaApplication.getUserName(Evaluate(loFormSet.lcPosHdr+'.cLok_User')))
          llAbort=.T.
          =Seek(lclckKey,loFormSet.lcTmpLine,'TmpLine3')
          Exit
        Endif
      Endif
      lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'',.T.)
      If Type('lcTranCode') = 'N'
        =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
        llAbort=.T.
        =Seek(lclckKey,loFormSet.lcTmpLine,'TmpLine3')
        Exit
      Else
        lcSelString = "UPDATE POSHDR SET lLok_stat =1,cLok_User= '"+oAriaApplication.User_ID+"', dLok_Date='"+Dtos(oAriaApplication.SystemDate)+"',cLok_Time='"+Time()+"' WHERE cBusDocu+cStyType+PO='"+Evaluate(loFormSet.lcPosHdr+'.cBusDocu')+Evaluate(loFormSet.lcPosHdr+'.cStyType')+Evaluate(loFormSet.lcPosHdr+'.Po')+"'"
        lnResult = oAriaApplication.RemoteCompanyData.Execute(lcSelString,'',"SAVEFILE","",lcTranCode ,4,'',Set("DataSession"))
        If lnResult <=0
          =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLUPDATE",lnResult,.T.)
          =oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
          llAbort=.T.
          =Seek(lclckKey,loFormSet.lcTmpLine,'TmpLine3')
          Exit
        Else
          =oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
        Endif
      Endif
    Endif
    =Seek(lclckKey,loFormSet.lcTmpLine,'TmpLine3')
  Endif
  *B000109,1 WAM 03/05/2005 (End)
  Exit
Enddo

*!*	IF loFormSet.lcPType = 'A' .AND. ASCAN(laEvntTrig,PADR("VADORORD",10)) <> 0
*!*	  =gfDoTriger("PORCVAP",PADR("VADORORD",10))
*!*	ENDIF
If llAbort
  loFormSet.ariaform1.kbPoNo.keytextbox.Value = Space(6)
  Select(lnAlias)
  Return .F.
Endif

Select (loFormSet.lcPosLn)
If Eof()
  *-The lines for this 'P/O' are missing ! cannot proceed.
  *N000682,1 MMT 11/21/2012 globalization Changes [Start]
  *=gfModalGen('TRM34017B42000','DIALOG','P/O')
  =gfModalGen('TRM34017B42000','DIALOG',;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_PO,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_PO",loFormSet.HeaderAlias)))
  *N000682,1 MMT 11/21/2012 globalization Changes [END]
  loFormSet.ariaform1.kbPoNo.keytextbox.Value = Space(6)
  Select(lnAlias)
  Return .F.
Endif

*--Check InTransit record.
*-- 'O' Receive inter location PO, 'E' Receive adornment order
If loFormSet.lcPType $ 'OE'
  Locate For TranCd='6'
  If !Found()
    If loFormSet.lcPType = 'O'
      *--Inter-location PO not yet issued from the target location. Cannot proceed.
      =gfModalGen('TRM34109B42000','DIALOG')
    Else
      *--The Adornment purchase order hasn't been issued yet from the targer location, cannot proceed!.
      =gfModalGen('TRM38176B00000','DIALOG')
    Endif
    loFormSet.ariaform1.kbPoNo.keytextbox.Value = Space(6)
    Select(lnAlias)
    Return .F.
  Endif
Endif
*-- Do you wish to select xxxx lines, Manually (by line) or Automatic (all) ?
*--  <Manually>   <Automatic>.
Select (loFormSet.lcTmpLine)
Locate For PO = loFormSet.ariaform1.kbPoNo.keytextbox.Value
If !Found()
  If loFormSet.lcPType $ 'RG' And !Empty(Evaluate(loFormSet.lcPosLn+'.CPONO'))
    loFormSet.lcAuto = 'A'
  Else

    *! C201211,1 HES 01/20/2010 Handle Scanned Barcodes from Recieve PO Screen -By PO- [Start]
    If Ascan(loFormSet.laEvntTrig,Padr('SCANBARCD',10),1,Alen(loFormSet.laEvntTrig,1),1) > 0
      If Type('loFormSet.lnCAlias') = 'U'
        loFormSet.AddProperty('lnCAlias',lnAlias)
      Endif
      loFormSet.mDoTrigger(Padr('SCANBARCD' ,10))
      If loFormSet.lcAuto = 'S'
        Return
      Endif
    Else
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
      If !(loFormSet.lcPType $ 'IO')
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
        *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
        *loFormSet.lcAuto = IIF(gfModalGen('QRM34126B34008','DIALOG',lcPoTtle) = 1,'M','A')
        If !loFormSet.llSilentMod
          loFormSet.lcAuto = Iif(gfModalGen('QRM34126B34008','DIALOG',lcPoTtle) = 1,'M','A')
        Else
          loFormSet.lcAuto = 'A'
          loFormSet.Visible = .F.
        Endif
        *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
      Else
        *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
        *lnResp = gfModalGen('QRM34126B34218','DIALOG',lcPoTtle)
        If !loFormSet.llSilentMod
          lnResp = gfModalGen('QRM34126B34218','DIALOG',lcPoTtle)
        Else
          lnResp = 2
          loFormSet.Visible = .F.
        Endif
        *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

        Do Case
        Case lnResp  = 1
          loFormSet.lcAuto = 'M'
        Case lnResp  = 2
          loFormSet.lcAuto = 'A'
        Otherwise
          loFormSet.lcAuto = 'I'
        Endcase
      Endif
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
    Endif
    *! C201211,1 HES 01/20/2010 Handle Scanned Barcodes from Recieve PO Screen -By PO- [End]

  Endif
Else

  If Ascan(loFormSet.laEvntTrig,Padr('VLDEXIST',10),1,Alen(loFormSet.laEvntTrig,1),1) > 0
    If loFormSet.mDoTrigger(Padr('VLDEXIST' ,10))
      Select(lnAlias)
      Return .F.
    Endif
  Endif
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
  If !(loFormSet.lcPType $ 'IO')
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
    loFormSet.lcAuto = Iif(lAutoMode,'A','M')
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
  Else
    If loFormSet.lcAuto  <> 'I'
      loFormSet.lcAuto = Iif(lAutoMode,'A','M')
    Endif
  Endif
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  If lAutoMode
    *--This XXXX already selected.
    = gfModalGen('INM34128B42000','DIALOG',lcPoTtle)
    loFormSet.ariaform1.kbPoNo.keytextbox.Value = Space(6)
    Select(lnAlias)
    Return .F.
  Endif
Endif

*N000587,1 WAM 12/01/2007 Get style cost sheet
lcPosHdr = loFormSet.lcPosHdr
=lfGetBomLn(loFormSet,&lcPosHdr..cStyType, &lcPosHdr..PO, Padr(loFormSet.ariaform1.cntShipment.kbShipNo.keytextbox.Value,6))
*N000587,1 WAM 12/01/2007 (End)

lcCarton = ''
Select (loFormSet.lcPosLn)

*! C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
If Ascan(loFormSet.laEvntTrig, Padr('PRCSPUPREF', 10), 1, Alen(loFormSet.laEvntTrig, 1), 1)>0
  Do lfpreisshdat In DIRMAIN.FXP With loFormSet
Endif
*! C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]

If loFormSet.lcAuto = 'A'
  lcFTrnCd = Iif(loFormSet.lcPType$'OE','6','1')
  Scan For TranCd = lcFTrnCd
    loFormSet.lcStyle   = Style
    *-- Get the style information
    If loFormSet.lcInvType = "0001"
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
      *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = '" + loFormSet.lcStyle + "'"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
      *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = [" + loFormSet.lcStyle + "]"
      lcStySelVal = loFormSet.lcStyle
      lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = ?m.lcStySelVal "
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
    Else
      *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
      *lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loFormSet.lcInvType +;
      "' AND Style = '" + loFormSet.lcStyle + "'"
      lcItemValue = loFormSet.lcStyle
      lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loFormSet.lcInvType +;
        "' AND Style = ?m.lcItemValue"
      *B607658,1 KHM 07/07/2005 [End]

    Endif

    =lfGetItmInf(loFormSet.lcInvType,;
      IIF(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+loFormSet.lcStyle,;
      loFormSet.lcTmpItem,;
      IIF(loFormSet.lcInvType = "0001",'STYLE','ITEM'),lcSqlStatement,;
      loFormSet.lcInvType = "0002")
    =Seek(Iif(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+loFormSet.lcStyle,;
      loFormSet.lcTmpItem)
    Select (loFormSet.lcPosLn)
    lnRecNo   = Recno()
    If Seek('1'+Space(3)+PO+Style+Dyelot+cWareCode+Str(Lineno,6),loFormSet.lcTmpLine)
      Loop
    Endif
    =lfvItem(loFormSet,Lineno)
    Select (loFormSet.lcPosLn)
    Goto lnRecNo
    If loFormSet.lcAuto = 'X'
      *--The style xxxxx will be ignored.
      *N000682,1 MMT 11/21/2012 globalization Changes [Start]
      *!*	      = gfModalGen('INM34203B42000','DIALOG',;
      *!*	        IIF(loFormSet.lcInvType='0001','Style','Fabric')+ALLTRIM(loFormSet.lcStyle))
      = gfModalGen('INM34203B42000','DIALOG',;
        IIF(loFormSet.lcInvType='0001',;
        IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_STYLE,loFormSet.GetHeaderText("LANG_POSTREC_MSG_STYLE",loFormSet.HeaderAlias)),;
        IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_FABRIC,loFormSet.GetHeaderText("LANG_POSTREC_MSG_FABRIC",loFormSet.HeaderAlias)))+Alltrim(loFormSet.lcStyle))
      *N000682,1 MMT 11/21/2012 globalization Changes [END]
      loFormSet.lcAuto = 'A'
    Endif
  Endscan
  Select (loFormSet.lcTmpLine)
  Locate
  =lfActBrow(loFormSet)
  =lfwBrow(loFormSet)
  Store .F. To loFormSet.ariaform1.dtPickerPostingDate.Enabled,;
    loFormSet.ariaform1.dtpickerReceivingDate.Enabled,;
    loFormSet.ariaform1.cboReceivingTypes.Enabled

  Select(lnAlias)
  Return
Endif
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
If  loFormSet.lcAuto = 'I'
  lnBatchSel = oAriaApplication.RemoteCompanyData.Execute("Select * from SCAN_BATCH_HEADER_T WHERE [STATUS]='O'",'',;
    "BATCH_HEADER_T","",oAriaApplication.ActiveCompanyConStr,3,"",Set("Datasession"))
  llErrorLog = .F.
  If lnBatchSel > 0
    Select BATCH_HEADER_T
    CursorSetProp("Buffering", 3, 'BATCH_HEADER_T')
    Index On Batch Tag Batch
    lcBrFields = [BATCH :R :H=']+;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BATCH_NO,loFormSet.GetHeaderText("LANG_POSTREC_BATCH_NO",loFormSet.HeaderAlias))+;
      [',DESCRIPTION :R :H=']+;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BATCH_DESC,loFormSet.GetHeaderText("LANG_POSTREC_BATCH_DESC",loFormSet.HeaderAlias))+;
      [', ] +[Vendor :R :H=']+;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BATCH_VENDOR,loFormSet.GetHeaderText("LANG_POSTREC_BATCH_VENDOR",loFormSet.HeaderAlias))+;
      [', DATE :R :H=']+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BATCH_DATE,loFormSet.GetHeaderText("LANG_POSTREC_BATCH_DATE",loFormSet.HeaderAlias)) +[']
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    Dimension laBrowArr[2]
    laBrowArr = ''
    lcBatch = ''
    If !Empty(loFormSet.laUsedBatchPO[1,1])
      Acopy(loFormSet.laUsedBatchPO,laBatch)
      Set Filter To Ascan(laBatch,Alltrim(BATCH_HEADER_T.SCAN_BATCH_HEADER_KEY)) = 0
    Endif
    lcBatch = Iif(ARIABROW('',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BATCH_TITLE,loFormSet.GetHeaderText("LANG_POSTREC_BATCH_TITLE",loFormSet.HeaderAlias)) ,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, ;
      gnBrFSCol2,'','','BATCH,SCAN_BATCH_HEADER_KEY','laBrowArr'),laBrowArr[1],Space(6))

    If !Empty(lcBatch)
      If Empty(loFormSet.laUsedBatchPO[1,1])
        loFormSet.laUsedBatchPO[1,1] = laBrowArr[2]
        loFormSet.laUsedBatchPO[1,2] = loFormSet.ariaform1.kbPoNo.keytextbox.Value
      Else
        Dimension loFormSet.laUsedBatchPO[ALEN(loFormSet.laUsedBatchPO,1)+1,2]
        loFormSet.laUsedBatchPO[ALEN(loFormSet.laUsedBatchPO,1),1] = laBrowArr[2]
        loFormSet.laUsedBatchPO[ALEN(loFormSet.laUsedBatchPO,1),2] = loFormSet.ariaform1.kbPoNo.keytextbox.Value
      Endif
      If Used('TMPSTR')
        Use In TMPSTR
      Endif

      Create Cursor TMPSTR (mStrRep M(10))
      Select TMPSTR
      Append Blank
      Replace mStrRep With Replicate('*',68) + Chr(13) +;
        IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BATCH_ERROR,loFormSet.GetHeaderText("LANG_POSTREC_BATCH_ERROR",loFormSet.HeaderAlias))+ Chr(13) +;
        REPLICATE('*',68) + Chr(13) + ' ' + Chr(13)

      loFormSet.ariaform1.kbItem.Enabled = .T.
      lcbatchid =   laBrowArr[2]
      *B610904,1 MMT 11/06/2014 Error in PO receiving screen while using scanned batch option[T20141029.0023][Start]
      *!*	      lnBatchDSel = oAriaApplication.RemoteCompanyData.execute("Select * from SCAN_BATCH_DETAILS_T WHERE SCAN_BATCH_HEADER_KEY= '"+;
      *!*	        ALLTRIM(lcbatchid) +"'" ,'',;
      *!*	        "SCAN_BATCH_DETAILS_T","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))
      lnBatchDSel = oAriaApplication.RemoteCompanyData.Execute("Select * from SCAN_BATCH_DETAILS_T WHERE SCAN_BATCH_HEADER_KEY= ?m.lcbatchid",'',;
        "SCAN_BATCH_DETAILS_T","",oAriaApplication.ActiveCompanyConStr,3,"",Set("Datasession"))
      *B610904,1 MMT 11/06/2014 Error in PO receiving screen while using scanned batch option[T20141029.0023][End]
      If lnBatchDSel > 0
        Select SCAN_BATCH_DETAILS_T
        Select Distinct Style,Space(60) As REASON From SCAN_BATCH_DETAILS_T Order By Style Into Cursor 'BATCHSTYLE'
        Select 'BATCHSTYLE'
        llFrst = .T.
        Scan
          loFormSet.lcStyle   = BATCHSTYLE.Style
          *-- Get the style information
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
          *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = '" + loFormSet.lcStyle + "'"
          *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
          *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = [" + loFormSet.lcStyle + "]"
          lcSelStyValue = loFormSet.lcStyle
          lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = ?m.lcSelStyValue"
          *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][END]
          =lfGetItmInf(loFormSet.lcInvType,;
            IIF(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+loFormSet.lcStyle,;
            loFormSet.lcTmpItem,;
            IIF(loFormSet.lcInvType = "0001",'STYLE','ITEM'),lcSqlStatement,;
            loFormSet.lcInvType = "0002")
          =Seek(Iif(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+loFormSet.lcStyle,;
            loFormSet.lcTmpItem)
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
          *lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE='S"+EVALUATE(loFormSet.lcTmpItem+'.Scale')+"'"
          *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
          *lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=[S"+EVALUATE(loFormSet.lcTmpItem+'.Scale')+"]"
          lcSelSclV = "S"+Evaluate(loFormSet.lcTmpItem+'.Scale')
          lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=?m.lcSelSclV"
          *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
          =lfOpenFox(lcSqlStatement,'SCALE','SCALE',"")
          lcMessageParam = ''
          If loFormSet.lcPType  ='O'
            lcExpSeek =loFormSet.lcBusDoc+loFormSet.lcWorkOrd+loFormSet.ariaform1.kbPoNo.keytextbox.Value+loFormSet.lcInvType+loFormSet.lcStyle
            If Seek(lcExpSeek,loFormSet.lcPosLn,'POSLN')
              Select (loFormSet.lcPosLn)
              Locate Rest While cBusDocu+cStyType+PO+cInvType+Style+Str(Lineno,6)+TranCd = lcExpSeek For TranCd = '6'
              If !Found()
                lcMessageParam = ['TRM34109B42000','DIALOG']
              Else
                =lfvItem(loFormSet,.T.)
              Endif
            Else
              *N000682,1 11/20/2012 MMT Globlization changes[Start]
              *lcMessageParam = LANG_POSTREC_STYLENOTINPO
              lcMessageParam = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_STYLENOTINPO,loFormSet.GetHeaderText("LANG_POSTREC_STYLENOTINPO",loFormSet.HeaderAlias))
              *N000682,1 11/20/2012 MMT Globlization changes[End]

            Endif
          Else
            =lfvItem(loFormSet,.T.)
          Endif

          If !Empty(lcMessageParam)
            If !Used(loFormSet.lcbatchdet)
              Select SCAN_BATCH_DETAILS_T
              Afields(laDetStru)
              Create Cursor (loFormSet.lcbatchdet) From Array laDetStru
            Endif
            lcMessage = ''
            If !Empty(lcMessageParam)
              llErrorLog = .T.
              *N000682,1 11/20/2012 MMT Globlization changes[Start]
              *IF lcMessageParam <> LANG_POSTREC_STYLENOTINPO
              If lcMessageParam <> Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_STYLENOTINPO,loFormSet.GetHeaderText("LANG_POSTREC_STYLENOTINPO",loFormSet.HeaderAlias))
                *N000682,1 11/20/2012 MMT Globlization changes[End]

                oMessageBox = Newobject("AriaMessageBox",Addbs(oAriaApplication.ClassDir)+"Utility.vcx")
                lcMsgtxt=  lcMessageParam
                oMessageBox.getmessage (&lcMsgtxt.)
                lcMessage = oMessageBox.Cmessage
                oMessageBox =Null
              Else
                lcMessage =  lcMessageParam
              Endif
              *N000682,1 11/20/2012 MMT Globlization changes[Start]
              *REPLACE mStrRep WITH mStrRep +LANG_POSTREC_BATCH_STYLE+loFormSet.lcStyle+SPACE(5)+lcMessage  + CHR(13) +CHR(10) IN  TMPSTR
              Replace mStrRep With mStrRep +Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BATCH_STYLE,loFormSet.GetHeaderText("LANG_POSTREC_BATCH_STYLE",loFormSet.HeaderAlias))+loFormSet.lcStyle+Space(5)+lcMessage  + Chr(13) +Chr(10) In  TMPSTR
              *N000682,1 11/20/2012 MMT Globlization changes[End]

              Select SCAN_BATCH_DETAILS_T
              Scan For Style = loFormSet.lcStyle
                Scatter Memo Memvar
                m.REJECTION_REASON = lcMessage
                m.STATUS           =  'R'
                Insert Into (loFormSet.lcbatchdet) From Memvar
              Endscan
            Endif
          Endif
          If Empty(lcMessageParam)
            Select (loFormSet.lcTmpLine)
            Set Filter To
            lnCurRec = Recno()
            lcKeyV = cCarton+PO+Style+Dyelot+Padr(cWareCode,6)+Str(Lineno,6)
            lcKeyExpr = cBusDocu+cStyType+PO+cInvType+Style+Str(Lineno,6)+'6'
            If loFormSet.lcPType  ='O'
              =Seek(lcKeyExpr,loFormSet.lcPosLn,'POSLN')
            Endif
            If !Seek('2'+lcKeyV)
              =Seek('1'+lcKeyV)
              Scatter Memo Memvar
              m.TranCd = '2'
              Append Blank
              Gather Memo Memvar
            Endif
            Store 0 To lnQtyRec1,lnQtyRec2,lnQtyRec3,lnQtyRec4,lnQtyRec5,lnQtyRec6,lnQtyRec7,lnQtyRec8
            For lnS = 1 To 8
              lcS = Alltrim(Str(lnS))
              Replace QTY&lcS. With  0 ,;
                TotQty  With 0
            Endfor
            Select  SCAN_BATCH_DETAILS_T
            Scan For Style = BATCHSTYLE.Style
              For lnS = 1 To Scale.Cnt
                lcS = Alltrim(Str(lnS))
                lcSize = Scale.SZ&lcS.

                If Alltrim(lcSize) = Alltrim(SCAN_BATCH_DETAILS_T.Size)
                  If  loFormSet.lcPType  <> 'O'
                    Replace QTY&lcS. With QTY&lcS. +  SCAN_BATCH_DETAILS_T.Quantity ,;
                      TotQty   With TotQty+  SCAN_BATCH_DETAILS_T.Quantity In (loFormSet.lcTmpLine)

                  Else
                    Replace QTY&lcS. With QTY&lcS. +  Min(SCAN_BATCH_DETAILS_T.Quantity,Evaluate(loFormSet.lcPosLn+'.QTY'+lcS)) ,;
                      TotQty   With TotQty +   Min(SCAN_BATCH_DETAILS_T.Quantity,Evaluate(loFormSet.lcPosLn+'.QTY'+lcS)) In (loFormSet.lcTmpLine)

                  Endif
                  lnQtyRec&lcS. = lnQtyRec&lcS. +    SCAN_BATCH_DETAILS_T.Quantity
                  Exit
                Endif
              Endfor
            Endscan
            Replace TotStk With TotQty,;
              TotBal With Max(TotQty-TotStk,0) In (loFormSet.lcTmpLine)
            lnTotalQty = Evaluate(loFormSet.lcTmpLine+'.TotStk')
            =Seek('1'+lcKeyV,loFormSet.lcTmpLine)
            Replace TotStk With lnTotalQty,;
              TotBal With Max(Qty1-lnQtyRec1,0)+Max(Qty2-lnQtyRec2,0)+Max(Qty3-lnQtyRec3,0)+Max(Qty4-lnQtyRec4,0)+;
              MAX(Qty5-lnQtyRec5,0)+Max(Qty6-lnQtyRec6,0)+Max(Qty7-lnQtyRec7,0)+Max(Qty8-lnQtyRec8,0) In (loFormSet.lcTmpLine)

            Select (loFormSet.lcTmpLine)
            Set Filter To TranCd ='1'
            If Between(lnCurRec ,1,Reccount())
              Go Record lnCurRec
            Endif

          Endif
          If loFormSet.lcAuto = 'X'
            *--The style xxxxx will be ignored.
            *N000682,1 MMT 11/21/2012 globalization Changes [Start]
            *!*              = gfModalGen('INM34203B42000','DIALOG',;
            *!*                IIF(loFormSet.lcInvType='0001','Style','Fabric')+ALLTRIM(loFormSet.lcStyle))
            = gfModalGen('INM34203B42000','DIALOG',;
              IIF(loFormSet.lcInvType='0001',;
              IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_STYLE,loFormSet.GetHeaderText("LANG_POSTREC_MSG_STYLE",loFormSet.HeaderAlias)),;
              IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_FABRIC,loFormSet.GetHeaderText("LANG_POSTREC_MSG_FABRIC",loFormSet.HeaderAlias)))+Alltrim(loFormSet.lcStyle))
            *N000682,1 MMT 11/21/2012 globalization Changes [End]
            loFormSet.lcAuto = 'A'
          Endif
        Endscan
      Endif
      If llErrorLog
        lfShowErrorLog()
      Endif
      Return
    Endif
  Endif
Endif
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
*-- Call procedure to calculate open in tmp file Set relation between the master file
*-- and the tmp add &lcTempFile..TOTQTY to the lcBrFields
=lfGetOpen(loFormSet)
Select (loFormSet.lcPosLn)

*! B608709,1 MMT 10/07/2008 Fix error while receiving PO manually[Start]
*PRIVATE lcTmpFile, lcTmpStyle
*lcTmpFile  = loFormSet.lcTempFile
Private lcTmprFile, lcTmpStyle
lcTmprFile = loFormSet.lcTempFile
*! B608709,1 MMT 10/07/2008 Fix error while receiving PO manually[End]


lcTmpStyle = loFormSet.lcTmpItem
Set Relation To Style Into (lcTmpStyle) Additive

*! B608709,1 MMT 10/07/2008 Fix error while receiving PO manually[Start]
*SET RELATION TO cBusDocu+cStyType+PO+cInvType+Style+STR(LineNo,6)+Trancd INTO (lcTmpFile) ADDITIVE
Set Relation To cBusDocu+cStyType+PO+cInvType+Style+Str(Lineno,6)+TranCd Into (lcTmprFile) Additive
*! B608709,1 MMT 10/07/2008 Fix error while receiving PO manually[End]

lcStyHdr   = loFormSet.ariaform1.kbItem.lblItemHeader.Caption
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*!*  lcBrFields = "Style :R :H=lcStyHdr :25,"+;
*!*    "&lcTmpStyle..Desc1 :R :H='Desc':45,"+;
*!*    "lcShpTTl=IIF(!EMPTY(Account),Account,cWareCode) :H='ShipTo',"+;
*!*    "TotQty :R :H='Quantity':P='9999999',"
lcBrFields = "Style :R :H=lcStyHdr :25,"+;
  "&lcTmpStyle..Desc1 :R :H='"+;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_DESCSTY,loFormSet.GetHeaderText("LANG_POSTREC_DESCSTY",loFormSet.HeaderAlias))+"':45,"+;
  "lcShpTTl=IIF(!EMPTY(Account),Account,cWareCode) :H='"+;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_SHIPTO,loFormSet.GetHeaderText("LANG_POSTREC_SHIPTO",loFormSet.HeaderAlias))+"',"+;
  "TotQty :R :H='"+;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_QTY,loFormSet.GetHeaderText("LANG_POSTREC_QTY",loFormSet.HeaderAlias))+"':P='9999999',"
*N000682,1 MMT 12/09/2012 Globalization changes[END]
If !(loFormSet.lcPType $ 'NOAE')

  *! B608709,1 MMT 10/07/2008 Fix error while receiving PO manually[Start]
  *lcBrFields = lcBrFields +"lnOpnQt=IIF(EVALUATE(lcTmpFile+'.TOTQTY')<0,0,EVALUATE(lcTmpFile+'.TOTQTY')) :R :H='Open':10:P='999999',"
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *lcBrFields = lcBrFields +"lnOpnQt=IIF(EVALUATE(lcTmprFile+'.TOTQTY')<0,0,EVALUATE(lcTmprFile+'.TOTQTY')) :R :H='Open':10:P='999999',"
  lcBrFields = lcBrFields +"lnOpnQt=IIF(EVALUATE(lcTmprFile+'.TOTQTY')<0,0,EVALUATE(lcTmprFile+'.TOTQTY')) :R :H='"+;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_OpenBrowTitl,loFormSet.GetHeaderText("LANG_POSTREC_OpenBrowTitl",loFormSet.HeaderAlias))+"':10:P='999999',"
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  *! B608709,1 MMT 10/07/2008 Fix error while receiving PO manually[End]

Endif
If loFormSet.llCostPrv
  *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
  If loFormSet.lcPType = 'M'
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *lcBrFields = lcBrFields +"nTotCost =nICost1+nICost2+nICost3+nICost4+nICost5+nICost6+nICost7 :R :H='Total Cost':P='9999999.999',"
    lcBrFields = lcBrFields +"nTotCost =nICost1+nICost2+nICost3+nICost4+nICost5+nICost6+nICost7 :R :H='"+;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TOTALCOST,loFormSet.GetHeaderText("LANG_POSTREC_TOTALCOST",loFormSet.HeaderAlias))+"':P='9999999.999',"
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
  Else
    *N038893,1 WAM 06/02/2005 (End)
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *lcBrFields = lcBrFields +"nFCost1 :R :H='Price':P='9999999.999',"
    lcBrFields = lcBrFields +"nFCost1 :R :H='"+;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_PRICE,loFormSet.GetHeaderText("LANG_POSTREC_PRICE",loFormSet.HeaderAlias))+"':P='9999999.999',"
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  Endif
Endif
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*lcBrFields = lcBrFields + "Reference :R"
lcBrFields = lcBrFields + "Reference :R :H='"+;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_REFERENCE,loFormSet.GetHeaderText("LANG_POSTREC_REFERENCE",loFormSet.HeaderAlias))+"'"
*N000682,1 MMT 12/09/2012 Globalization changes[END]
Dimension laTemp[1]
laTemp = ''
=lfActBrow(loFormSet)
Select (loFormSet.lcPosLn)
Locate

lcFTrnCd = Iif(loFormSet.lcPType$'OE','6','1')
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
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*!*  lcBrowTitl = IIF(loFormSet.lcPType$'RG','return purchase order',;
*!*    IIF(loFormSet.lcPType='N','inter location purchase order',;
*!*    IIF(loFormSet.lcPType$'AE','adornment order',;
*!*    IIF(loFormSet.lcPType='D','Dye order',IIF(loFormSet.lcPType='M','Cutting Ticket',IIF(loFormSet.lcPType ='W',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MMO_MMO,loFormSet.GetHeaderText("LANG_POSTREC_MMO_MMO",loFormSet.HeaderAlias)),'purchase order'))))))
lcBrowTitl = Iif(loFormSet.lcPType$'RG',;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_RETPO,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_RETPO",loFormSet.HeaderAlias)),;
  IIF(loFormSet.lcPType='N',;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_INTERPO,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_INTERPO",loFormSet.HeaderAlias)),;
  IIF(loFormSet.lcPType$'AE',;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_ADOR,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_ADOR",loFormSet.HeaderAlias)),;
  IIF(loFormSet.lcPType='D',;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_DYEORDER,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_DYEORDER",loFormSet.HeaderAlias)),;
  IIF(loFormSet.lcPType='M',;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_CT,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_CT",loFormSet.HeaderAlias)),;
  IIF(loFormSet.lcPType ='W',;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MMO_MMO,loFormSet.GetHeaderText("LANG_POSTREC_MMO_MMO",loFormSet.HeaderAlias)),;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_PO,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_PO",loFormSet.HeaderAlias))))))))
*N000682,1 MMT 12/09/2012 Globalization changes[END]

*N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
*N038893,1 WAM 06/02/2005 (End)

*B607927,1 TMI [Start] Fix a bug that the pointer is not located on the correct record, the reason for that was not passing the while condition in the ARIABROW global function
*=ARIABROW([FOR Trancd = lcFTrnCd],'Select ' + lcBrowTitl + ' Line',gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','Style','laTemp')
*N000682,1 MMT 12/09/2012 Globalization changes[Start]
*=ARIABROW([CBUSDOCU+CSTYTYPE+PO+CINVTYPE FOR Trancd = lcFTrnCd],'Select ' + lcBrowTitl + ' Line',gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','Style','laTemp')
=ARIABROW([CBUSDOCU+CSTYTYPE+PO+CINVTYPE FOR Trancd = lcFTrnCd],;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BROWSELECT,loFormSet.GetHeaderText("LANG_POSTREC_BROWSELECT",loFormSet.HeaderAlias))+' ' + lcBrowTitl + ' '+;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BROWLINE,loFormSet.GetHeaderText("LANG_POSTREC_BROWLINE",loFormSet.HeaderAlias)),gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','Style','laTemp')
*N000682,1 MMT 12/09/2012 Globalization changes[END]
*B607927,1 TMI [End  ]

loFormSet.lcStyle = laTemp[1]
=lfActBrow(loFormSet)

Select (loFormSet.lcPosLn)
If Empty(loFormSet.lcStyle )
  If loFormSet.lcPType $ 'NOAE'
    loFormSet.ariaform1.kbPoNo.keytextbox.Value = Space(6)
    Select(lnAlias)
    Return .F.
  Else
    loFormSet.ariaform1.kbItem.Enabled = .T.
  Endif
Else
  If !Seek(Iif(loFormSet.lcInvType="0001","","0002")+loFormSet.lcStyle ,loFormSet.lcTmpItem)
    If loFormSet.lcInvType = "0001"
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
      *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = '" + loFormSet.lcStyle  + "'"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
      *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = [" + loFormSet.lcStyle  + "]"
      lcSeleStyVa = loFormSet.lcStyle
      lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = ?m.lcSeleStyVa "
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][END]
    Else
      *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
      *lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loFormSet.lcInvType +;
      "' AND Style = '" + loFormSet.lcStyle  + "'"
      Private lcItemValue
      lcItemValue = loFormSet.lcStyle
      lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loFormSet.lcInvType +;
        "' AND Style = ?m.lcItemValue"
      *B607658,1 KHM 07/07/2005 [End]
    Endif
    =lfGetItmInf(loFormSet.lcInvType,;
      IIF(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+loFormSet.lcStyle ,;
      loFormSet.lcTmpItem,;
      IIF(loFormSet.lcInvType="0001",'STYLE','ITEM'),lcSqlStatement,;
      loFormSet.lcInvType = "0002")
    =Seek(Iif(loFormSet.lcInvType="0001","","0002")+loFormSet.lcStyle ,loFormSet.lcTmpItem)
  Endif
  Select (loFormSet.lcPosLn)
  =lfvItem(loFormSet,Lineno)
Endif
Select(lnAlias)
Return

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
Function lfvItem
Lparameters loFormSet,lnPOLLnNo,llFromItem
Local lcItmHdr, lcMTxt
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
If llFromItem And ;
    !Seek(Iif(loFormSet.lcInvType="0001","","0002")+loFormSet.lcStyle ,loFormSet.lcTmpItem)
  If loFormSet.lcInvType = "0001"
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
    *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = '" + loFormSet.lcStyle  + "'"
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
    *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = [" + loFormSet.lcStyle  + "]"
    lcStySelV = loFormSet.lcStyle
    lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = ?m.lcStySelV "
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][END]
  Else
    *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
    *lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loFormSet.lcInvType +;
    "' AND Style = '" + loFormSet.lcStyle  + "'"
    Private lcItemValue
    lcItemValue = loFormSet.lcStyle
    lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loFormSet.lcInvType +;
      "' AND Style = ?m.lcItemValue "
    *B607658,1 KHM 07/07/2005 [End]
  Endif
  =lfGetItmInf(loFormSet.lcInvType,;
    IIF(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+loFormSet.lcStyle ,;
    loFormSet.lcTmpItem,;
    IIF(loFormSet.lcInvType="0001",'STYLE','ITEM'),lcSqlStatement,;
    loFormSet.lcInvType = "0002")
  =Seek(Iif(loFormSet.lcInvType="0001","","0002")+loFormSet.lcStyle ,loFormSet.lcTmpItem)
  lcCarton = ''
Endif

lnAlias = Select()

*--Check if it was entered.
If loFormSet.lcAuto = 'M' And ((loFormSet.llCMInstld And loFormSet.llPOSale) Or;
    !(loFormSet.llWareHous And !(loFormSet.lcPType $ 'NA'))) And;
    (!loFormSet.llDyelot Or Evaluate(loFormSet.lcTmpItem+'.cDye_Flg')<>'Y') And ;
    TYPE('lnPOLLnNo')='N' And Seek('1'+Space(3)+loFormSet.lcPoNo+loFormSet.lcStyle,;
    loFormSet.lcTmpLine)

  Select (loFormSet.lcTmpLine)
  Locate Rest While TranCd+cCarton+PO+Style+Dyelot+cWareCode+Str(Lineno,6) = ;
    '1'+Space(3)+loFormSet.lcPoNo+loFormSet.lcStyle For Lineno = lnPOLLnNo
  If Found()
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *!*      lcMTxt = IIF(loFormSet.lcPType$'NAU','issue ','receiving ')+;
    *!*        IIF(loFormSet.lcPType$'SCUF','shipment',;
    *!*        IIF(loFormSet.lcPType$'RG','return P/O',;
    *!*        IIF(loFormSet.lcPType='N','inter location P/O' ,;
    *!*        IIF(loFormSet.lcPType$'AE','adornment P/O','P/O'))))
    *!*      lcItmHdr = IIF(loFormSet.lcInvType = "0002", "Fabric","Style")
    lcMTxt = Iif(loFormSet.lcPType$'NAU',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_ISS,loFormSet.GetHeaderText("LANG_POSTREC_MSG_ISS",loFormSet.HeaderAlias))+' ',;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_RECEVING,loFormSet.GetHeaderText("LANG_POSTREC_MSG_RECEVING",loFormSet.HeaderAlias))+' ')+;
      IIF(loFormSet.lcPType$'SCUF',;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_S_SHIPMENT,loFormSet.GetHeaderText("LANG_POSTREC_S_SHIPMENT",loFormSet.HeaderAlias)),;
      IIF(loFormSet.lcPType$'RG',;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_RETPO,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_RETPO",loFormSet.HeaderAlias)),;
      IIF(loFormSet.lcPType='N',;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_INTERPO,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_INTERPO",loFormSet.HeaderAlias)),;
      IIF(loFormSet.lcPType$'AE',;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_ADOR ,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_ADOR",loFormSet.HeaderAlias)),;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_PO,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_PO",loFormSet.HeaderAlias))))))
    lcItmHdr = Iif(loFormSet.lcInvType = "0002", ;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_FABRIC,loFormSet.GetHeaderText("LANG_POSTREC_MSG_FABRIC",loFormSet.HeaderAlias)),;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_STYLE,loFormSet.GetHeaderText("LANG_POSTREC_MSG_STYLE",loFormSet.HeaderAlias)))
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
    *-This XXXX has been entered on this XXXX.
    =gfModalGen('TRM42107B42000','DIALOG',lcItmHdr+'|'+lcMTxt)
    Select(lnAlias)
    Return
  Endif
  Select (loFormSet.lcTmpItem)
Endif

llAbort   = .F.
loFormSet.llNewItem = .F.

*-- To check the type of style cost sheet
Do Case
  *-- 'I' Receive P/O, 'S' Receive by Shipment, 'B' Receive P/O Batch
Case loFormSet.lcPType $ 'ISBNO'
  lcCstShtType = 'I'

  *-- 'M' Receive C/T, 'T' Receive C/T Batch
Case loFormSet.lcPType $ 'MT'
  lcCstShtType = 'M'

Endcase

=Seek(Iif(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+loFormSet.lcStyle ,;
  loFormSet.lcTmpItem)
*-- Validation checks loop for Style.....
*- P/O styles check.
Do While .T.
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
  *IF loFormSet.llImpCost AND !(loFormSet.lcPType $ 'RAEPF')
  *! B612583,1 MMT 06/12/2022 Issue inter-location PO screen does accept PO lines that does not have default Imported style cost sheet even it is not used by the program [T20220607.0003][Start]
  *If loFormSet.llImpCost And !(loFormSet.lcPType $ 'RAEPFW')
  If loFormSet.llImpCost And !(loFormSet.lcPType $ 'RAEPFWN')
  *! B612583,1 MMT 06/12/2022 Issue inter-location PO screen does accept PO lines that does not have default Imported style cost sheet even it is not used by the program [T20220607.0003][End]  
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][End]
    If lfGetBOM(loFormSet,lcCstShtType,Substr(loFormSet.lcStyle,1,loFormSet.lnMjrWid),.F.)
      Select (loFormSet.lcBomHdr)
      Locate For cItmMajor = Substr(loFormSet.lcStyle,1,loFormSet.lnMjrWid) And;
        cCstShtTyp = lcCstShtType And lDefCstSht And !Empty(cCstSht_Id)
      If !Found()
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
        If !(loFormSet.lcAuto = 'I' And loFormSet.lcPType $ 'IO')
          *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
          *-No cost lines found in the cost sheet, Cannot proceed!
          =gfModalGen('TRM34037B42000','DIALOG')
          *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
        Else
          lcMessageParam = ['TRM34037B42000','DIALOG']
        Endif
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
        llAbort=.T.
        Exit
      Endif
    Else
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
      If !(loFormSet.lcAuto = 'I' And loFormSet.lcPType $ 'IO')
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
        *-No cost lines found in the cost sheet, Cannot proceed!
        =gfModalGen('TRM34037B42000','DIALOG')
        *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
      Else
        lcMessageParam = ['TRM34037B42000','DIALOG']
      Endif
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
      llAbort=.T.
      Exit
    Endif
  Endif

  If loFormSet.lcInvType = '0001' And Evaluate(loFormSet.lcTmpItem+'.cDivision') <> Evaluate(loFormSet.lcPosHdr+'.cDivision')
    *-Conflict ! styles restricted to division XXXX, Cannot proceed!
    *=gfModalGen('TRM34041B42000','DIALOG',ALLTRIM(EVALUATE(loFormSet.lcPosHdr+'.cDivision')))
    *N000682,1 MMT 11/21/2012 globalization Changes [Start]
    *lcMTxt = IIF(loFormSet.lcInvType="0001","Style", "Fabric")
    lcMTxt = Iif(loFormSet.lcInvType="0001",;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_STYLE,loFormSet.GetHeaderText("LANG_POSTREC_MSG_STYLE",loFormSet.HeaderAlias)),;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_FABRIC,loFormSet.GetHeaderText("LANG_POSTREC_MSG_FABRIC",loFormSet.HeaderAlias)) )
    *N000682,1 MMT 11/21/2012 globalization Changes [END]
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
    If !(loFormSet.lcAuto = 'I' And loFormSet.lcPType $ 'IO')
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
      *N000682,1 MMT 11/21/2012 globalization Changes [Start]
      *=gfModalGen('TRM36229B34000','DIALOG',lcMTxt+"|"+lcMTxt+"s"+"|"+ALLTRIM(EVALUATE(loFormSet.lcPosHdr+'.cDivision')))
      =gfModalGen('TRM36229B34000','DIALOG',lcMTxt+"|"+lcMTxt+Iif(oAriaApplication.oActivelang.cLang_ID = "EN","s",'')+"|"+Alltrim(Evaluate(loFormSet.lcPosHdr+'.cDivision')))
      *N000682,1 MMT 11/21/2012 globalization Changes [END]
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
    Else
      *N000682,1 MMT 11/21/2012 globalization Changes [Start]
      *lcMessageParam = ['TRM36229B34000','DIALOG',']+lcMTxt+[|]+lcMTxt+[s|]+ALLTRIM(EVALUATE(loFormSet.lcPosHdr+'.cDivision'))+[']
      lcMessageParam = ['TRM36229B34000','DIALOG',']+lcMTxt+[|]+lcMTxt+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",[s|],[|])+Alltrim(Evaluate(loFormSet.lcPosHdr+'.cDivision'))+[']
      *N000682,1 MMT 11/21/2012 globalization Changes [END]
    Endif
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
    llAbort=.T.
    Exit
  Endif

  If Evaluate(loFormSet.lcTmpItem+'.Status') = 'X'
    *-This is a canceled style. Not allowed to enter here, Cannot proceed!
    *=gfModalGen('TRM34040B42000','DIALOG')
    *N000682,1 MMT 11/21/2012 globalization Changes [Start]
    *lcMTxt = IIF(loFormSet.lcInvType="0001","Style", "Fabric")
    lcMTxt = Iif(loFormSet.lcInvType="0001",Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_STYLE,loFormSet.GetHeaderText("LANG_POSTREC_MSG_STYLE",loFormSet.HeaderAlias)),;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_FABRIC,loFormSet.GetHeaderText("LANG_POSTREC_MSG_FABRIC",loFormSet.HeaderAlias)))
    *N000682,1 MMT 11/21/2012 globalization Changes [END]
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
    If !(loFormSet.lcAuto = 'I' And loFormSet.lcPType $ 'IO')
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
      =gfModalGen('TRM36228B34000','DIALOG', lcMTxt)
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
    Else
      lcMessageParam = ['TRM36228B34000','DIALOG',']+ lcMTxt+[']
    Endif
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
    llAbort=.T.
    Exit
  Endif

  *--Item not on P/O , Modify P/O?
  loFormSet.llNewItem = .F.
  lcSeekTyp           = loFormSet.lcWorkOrd

  If !Seek(loFormSet.lcBusDoc+loFormSet.lcWorkOrd+loFormSet.lcPoNo+;
      loFormSet.lcInvType+loFormSet.lcStyle,loFormSet.lcPosLn)
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
    If loFormSet.lcPType = 'O'
      If (loFormSet.lcAuto = 'I')
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *lcMessageParam = LANG_POSTREC_STYLENOTINPO
        lcMessageParam = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_STYLENOTINPO,loFormSet.GetHeaderText("LANG_POSTREC_STYLENOTINPO",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]

      Endif
      llAbort = .T.
      Exit
    Endif
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
    *--Item not on P/O! Modify P/O.<Yes\<No
    If gfModalGen('QRM34072B42002','DIALOG') = 2
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
      If (loFormSet.lcAuto = 'I' And loFormSet.lcPType $ 'IO')
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *lcMessageParam = LANG_POSTREC_STYLENOTINPO
        lcMessageParam = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_STYLENOTINPO,loFormSet.GetHeaderText("LANG_POSTREC_STYLENOTINPO",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]

      Endif
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
      llAbort = .T.
      Exit
    Else
      loFormSet.llNewItem = .T.
    Endif
  Else
    If Type('lnPOLLnNo')='N'
      =Seek(loFormSet.lcBusDoc+loFormSet.lcWorkOrd+loFormSet.lcPoNo+;
        loFormSet.lcInvType+loFormSet.lcStyle+Str(lnPOLLnNo,6),loFormSet.lcPosLn)
    Endif
  Endif

  *!*	  IF loFormSet.lcPType = 'E' .AND. POSLN.nCost2 = 0 .AND. ASCAN(laEvntTrig,PADR("RCVADORD",10)) = 0
  *!*	    =gfModalGen('TRM38182B00000','DIALOG')
  *!*	    llAbort=.T.
  *!*	    EXIT
  *!*	  ENDIF

  =Seek(Iif(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+loFormSet.lcStyle,;
    loFormSet.lcTmpItem)

  lcwarecode = Iif(Evaluate(loFormSet.lcPosHdr+'.lMultiWare') And !loFormSet.llNewItem,;
    EVALUATE(loFormSet.lcPosLn+'.cWareCode'),;
    EVALUATE(loFormSet.lcPosHdr+'.cWareCode'))
  lcwarecode = Iif(loFormSet.lcPType $ 'NA',Padr(Evaluate(loFormSet.lcPosHdr+'.Vendor'),6),lcwarecode)

  If loFormSet.llWareHous And !(loFormSet.lcPType $ 'NOAE')
    *-- Case of style
    If loFormSet.lcInvType = "0001"
      *-- Get item information from stydye file
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
      *!*	      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
      *!*	        PADR(loFormSet.lcStyle,19)+lcWareCode+SPACE(10) + "'"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
      *!*	      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
      *!*	        PADR(loFormSet.lcStyle,19)+lcWareCode+SPACE(10) + "]"
      lcStySelValue = Padr(loFormSet.lcStyle,19)+lcwarecode+Space(10)
      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = ?m.lcStySelValue"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
      =lfOpenFox(lcSqlStatement,'STYDYE',loFormSet.lcTmpCurs,"")
      Select(loFormSet.lcTmpCurs)
      Locate
      If Eof()
        If !(Evaluate(loFormSet.lcPosHdr+'.lMultiWare'))
          *-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
          *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
          *IF loFormSet.lcAuto = 'M'
          If loFormSet.lcAuto $ 'IM'
            *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
            If gfModalGen('QRM34048B42006','DIALOG',Alltrim(loFormSet.lcStyle)+'|'+lcwarecode) = 1
              Do gpAdStyWar With loFormSet.lcStyle,Space(10),lcwarecode
            Else
              *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
              If (loFormSet.lcAuto = 'I' And loFormSet.lcPType $ 'IO')
                lcMessageParam = ['QRM34048B42006','DIALOG',']+Alltrim(loFormSet.lcStyle)+[|]+lcwarecode+[']
              Endif
              *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
              llAbort=.T.
              Exit
            Endif
          Else
            Do gpAdStyWar With loFormSet.lcStyle,Space(10),lcwarecode
          Endif
        Else
          lcwarecode =  Evaluate(loFormSet.lcTmpItem+'.cDefWare')
        Endif
      Endif
    Else
      *-- Get item information from ItemLoc file
      *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
      *lcSqlStatement  = "SELECT * FROM ITEMLOC [INDEX=STYDYE] " + ;
      "WHERE cInvType ='" + loFormSet.lcInvType + "' AND "+;
      "Style ='" + PADR(loFormSet.lcStyle,19) +"' AND " + ;
      "cWareCode ='" + lcWareCode + "' AND " + ;
      "Dyelot = '         '"
      Private lcItemValue, lcWareCodVal
      lcItemValue  = Padr(loFormSet.lcStyle,19)
      lcWareCodVal = lcwarecode
      lcSqlStatement  = "SELECT * FROM ITEMLOC [INDEX=STYDYE] " + ;
        "WHERE cInvType ='" + loFormSet.lcInvType + "' AND "+;
        "Style = ?m.lcItemValue " + " AND " + ;
        "cWareCode = ?m.lcWareCodVal " + " AND " + ;
        "Dyelot = '         '"
      *B607658,1 KHM 07/07/2005 [End]

      =lfOpenSql(lcSqlStatement,'ITEMLOC',loFormSet.lcTmpCurs, "","",.F.)
      Select(loFormSet.lcTmpCurs)
      Locate
      If Eof()
        If !(Evaluate(loFormSet.lcPosHdr+'.lMultiWare'))
          *-Style: xxx is not assigned to location: xxx. "\<Add;\<Reenter"
          If loFormSet.lcAuto = 'M'
            *N000682,1 MMT 12/09/2012 Globalization changes[Start]
            *lcMsg = "Fabric: " + ALLTRIM(loFormSet.lcStyle)
            lcMsg = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_FABRIC,loFormSet.GetHeaderText("LANG_POSTREC_MSG_FABRIC",loFormSet.HeaderAlias))+": " + Alltrim(loFormSet.lcStyle)
            *N000682,1 MMT 12/09/2012 Globalization changes[END]
            If gfModalGen('QRM36226B34004','DIALOG',lcMsg +'|'+lcwarecode) = 1
              =gfAdItemWar(loFormSet.lcInvType,loFormSet.lcStyle,Space(10),lcwarecode)
            Else
              llAbort=.T.
              Exit
            Endif
          Else
            =gfAdItemWar(loFormSet.lcInvType,loFormSet.lcStyle,Space(10),lcwarecode)
          Endif
        Else
          lcwarecode =  Evaluate(loFormSet.lcTmpItem+'.cDefWare')
        Endif
      Endif
    Endif
  Endif

  loFormSet.lcDyelot = Iif(!loFormSet.llNewItem And loFormSet.llDyelot And ;
    EVALUATE(loFormSet.lcTmpItem+'.cDye_Flg') = 'Y',;
    EVALUATE(loFormSet.lcPosLn+'.Dyelot'),'')
  Exit
Enddo

If llAbort
  Select(lnAlias)
  loFormSet.lcAuto = Iif(loFormSet.lcAuto = 'A','X',loFormSet.lcAuto)
  Return
Endif
loFormSet.ariaform1.kbPoNo.Enabled = .F.

*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [START]
*N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
*IF loFormSet.lcInvType="0002" AND loFormSet.lcPType $ 'FPG'
If loFormSet.lcInvType="0002" And loFormSet.lcPType $ 'FPGW'
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
  If  loFormSet.lcCostMthM = 'L' And loFormSet.llTrkRolls And Used(loFormSet.lcTmpItem) And ;
      SEEK(loFormSet.lcInvType+Evaluate(loFormSet.lcTmpLine+'.Style'),loFormSet.lcTmpItem);
      AND Evaluate(loFormSet.lcTmpItem+'.LTRKROLLS')
    loFormSet.ariaform1.cmdRolls.Enabled = .T.
  Else
    If loFormSet.lcPType = 'G' And loFormSet.lcCostMthM = 'L' And !loFormSet.llTrkRolls
      loFormSet.ariaform1.cmdRolls.Enabled = .T.
    Endif
  Endif
Else
  loFormSet.ariaform1.cmdRolls.Enabled = .F.
  loFormSet.ariaform1.cmdRolls.Visible = .F.
Endif
*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]

*--Get a lot no and last operation.
loFormSet.llSpecLot  = .F.
loFormSet.lcLotNo    = Space(2)
loFormSet.lcClrLstOp = Space(6)

*N038893,1 WAM 06/02/2005 Receive Cutting Ticket
*IF loFormSet.llImpCost AND loFormSet.lcPType $ 'ID' AND !loFormSet.llNewItem
If (loFormSet.llMFCall Or (loFormSet.llImpCost And loFormSet.lcPType $ 'ID')) And !loFormSet.llNewItem
  *N038893,1 WAM 06/02/2005 (End)

  loFormSet.lcLotNo = lfSelLots(Str(Evaluate(loFormSet.lcPosLn+'.LineNo'),6),;
    EVALUATE(loFormSet.lcPosHdr+'.cLastOpr'),loFormSet)
Endif

llShpPO = (loFormSet.lcPType $ 'SUC')

= Seek(Iif(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+loFormSet.lcStyle,;
  loFormSet.lcTmpItem)
If loFormSet.lcAuto = 'M' And ((!(loFormSet.llCMInstld And loFormSet.llPOSale) And ;
    loFormSet.llWareHous) Or loFormSet.llDyelot) And ;
    EVALUATE(loFormSet.lcTmpItem+'.cDye_Flg') = 'Y' And !(loFormSet.lcPType $ 'ONAE')
  *-- Enable the dyelot field and set focus to it.
  loFormSet.ariaform1.kbItem.Value = loFormSet.lcStyle
  loFormSet.ariaform1.kbconfiguration.keytextbox.Value =  Padr(loFormSet.lcDyelot,10)
  loFormSet.ariaform1.kbconfiguration.Enabled = .T.
  loFormSet.ariaform1.kbconfiguration.lcstylecode = loFormSet.lcStyle
  loFormSet.lnWare  = Ascan(loFormSet.laWare,lcwarecode,1)
  loFormSet.ariaform1.kbconfiguration.lcwarecode  = Substr(loFormSet.laWare[loFormSet.lnWare],1,6)

Else
  *  IF (loFormSet.lcPType $ 'ONAE') AND !EMPTY(loFormSet.lcDyelot)
  *    =lfvDyelot(loFormSet)
  *  ELSE
  *SHOW GET lcDyelot DISABLE
  = lfChkLine(loFormSet)    && Check for existance of default location.
  *  ENDIF
Endif

Return

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
Function lfChkLine
Lparameters loFormSet
Select (loFormSet.lcTmpLine)
llEmpWare = .F.
lcCarton   = cCarton
lcCurrRec  = Padr(cCarton,3)+Padr(PO,6)+Padr(Style,19)+Padr(Dyelot,10)+Padr(cWareCode,6)+Str(Lineno,6)
lcLineUnq  = Str(Evaluate(loFormSet.lcPosLn+'.LineNo'),6)
lcwarecode = Iif(Type('lcWareCode') $ 'UL',Evaluate(loFormSet.lcPosLn+'.cWareCode'),lcwarecode)
*-- if this record is found in temp. file before
If Seek('1'+Padr(lcCarton,3)+Padr(loFormSet.lcPoNo,6)+Padr(loFormSet.lcStyle,19)+;
    PADR(loFormSet.lcDyelot,10)+Padr(lcwarecode,6)+lcLineUnq)
  llEmpWare = .T.
  If Seek('1'+Padr(lcCarton,3)+Padr(loFormSet.lcPoNo,6)+Padr(loFormSet.lcStyle,19)+;
      PADR(loFormSet.lcDyelot,10)+Space(6)+lcLineUnq)
    lnBrRecNo = Recno()
    lnWare = 1
    =lfwBrow(loFormSet)
    Return
  Else
    Seek '1' + lcCurrRec
  Endif
Endif
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
Function lfCalOpen
Lparameters lcRecvType, lcMastPoLn,lcParmKey,llSubtNo, loFormSet
Local lnAlias, lnMsLnRNo

If Type('loFormSet') = 'O' And Empty(loFormSet.lcDyelot)
  lcParmKey = Alltrim(lcParmKey)
Endif

lnAlias = Select()
Select (lcMastPoLn)
lnMsLnRNo = Recno()
Seek lcParmKey

lcOrjWareH = cWareCode
laOpnQty   = 0
lcWhlCndn  = "cBusDocu+cStyType+PO+cInvType+Style+STR(LineNo,6)" + " = lcParmKey"

If !(lcRecvType $ 'OELC')
  lcForCndn = Iif(lcRecvType $ 'MT',".T.","TranCd <> '3'")
  lcBaseTrCd = '1'
Else

  *B999999,1 AMH Fix bug of incorrent total stock when receive inter-location po again [Start]
  *lcForCndn = "TranCd <> '1'"
  lcForCndn = "TranCd = '6'"
  *B999999,1 AMH [End]

  lcBaseTrCd = '6'
Endif

Scan Rest While &lcWhlCndn For &lcForCndn
  For I=1 To 8
    lcCnt=Str(I,1)

    *B999999,1 AMH Fix bug of incorrect open qty [Start]
    *laOpnQty[I]= IIF(TranCd = lcBaseTrCd,laOpnQty[I]+Qty&lcCnt,;
    IIF(lcRecvType $ 'IBSDPF' AND !EMPTY(DYELOT),laOpnQty[I]-Qty&lcCnt,;
    IIF(lcRecvType $ 'OCL',laOpnQty[I]-Qty&lcCnt,MAX(laOpnQty[I]-Qty&lcCnt,0))))
    laOpnQty[I]= Max(laOpnQty[I]+(Evaluate('Qty'+lcCnt)*Iif(TranCd = lcBaseTrCd,1,-1)),0)
    *B999999,1 AMH [End]

    If llSubtNo
      laPrevRecQ[I] = laPrevRecQ[I]+ Iif(TranCd <> lcBaseTrCd,QTY&lcCnt,0)
    Endif

  Endfor
Endscan

If lcRecvType $ 'OCL'
  For lnCntr = 1 To 8
    laOpnQty[lnCntr] = Max(laOpnQty[lnCntr],0)
  Endfor
Endif
*N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
*IF !llSubtNo AND (lcRecvType $ 'IBSMDPF')
If !llSubtNo And (lcRecvType $ 'IBSMDPFW')
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
  Seek lcParmKey
  Select (loFormSet.lcTmpLine)
  lcCurDyelot = Iif(loFormSet.lcPType $ 'BM',Dyelot,Padr(loFormSet.lcDyelot,10))
  lcCurWare   = Iif(loFormSet.lcPType $ 'BM',cWareCode,lcwarecode)
  lcGoAgain   = TranCd+cCarton+PO+Style+Dyelot+Padr(cWareCode,6)+Str(Lineno,6)
  lcScanExpr  = Iif(loFormSet.lcPType = 'B',cCarton+PO+Style+Str(Lineno,6),;
    '   '+Evaluate(loFormSet.lcPosLn+'.PO')+loFormSet.lcStyle+;
    STR(Evaluate(loFormSet.lcPosLn+'.LineNo'),6))
  lcForExpr = Iif(loFormSet.lcPType $ 'BM',"(Dyelot+cWareCode # lcCurDyelot+lcCurWare) AND ;
                  (Trancd # '1')", "Trancd # '1'")
  lcScanCond  = "cCarton"+"Po"+"Style"+"STR(LineNo,6)"
  Go Top
  If !Eof()
    Scan Rest While  lcScanCond = lcScanExpr;
        FOR &lcForExpr
      For lnI = 1 To 8
        lcI = Str(lnI,1)
        laOpnQty[lnI] = laOpnQty[lnI] - QTY&lcI
      Endfor
    Endscan
    = Seek(lcGoAgain)
  Endif
Endif

Select (lcMastPoLn)
If Between(lnMsLnRNo,1,Reccount())
  Go lnMsLnRNo
Endif

Select(lnAlias)
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
Release lnAlias, lnMsLnRNo,lcWhlCndn  ,lcForCndn ,lcBaseTrCd ,I,lcCnt,lnCntr ,lcCurDyelot ,;
  lcCurWare,lcGoAgain   ,lcScanExpr  ,lcForExpr ,lcScanCond  ,lnI,lcI
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]
Return (lcOrjWareH)

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
Procedure lfGetOpen
Lparameters loFormSet

Private lnRecNo , lnFilePos , laSumAll , lcMastFile , lcScanCond , lcTrancd

*-- To hold the received qty (Stock, cancel, and damage)
Private laQty, lcSeekTyp, lcSeekCond, lcScanKey, laOpenQty

Select (loFormSet.lcPosLn)
lnFilePos = Recno()
Dimension laSumAll[3], laOpenQty[8]

*-- To hold the received qty (Stock, cancel, and damage)
Dimension laQty[4,8]
Store 0 To laQty, laSumAll

*------------------ CREATE TEMP. FILE -> [BEGIN]
If !loFormSet.llFirstTmp &&-- do it only one time

  Dimension laFileStru[1,4]

  =Afields(laFileStru)
  Dimension laFileStru[ALEN(laFileStru,1),18]

  Dime laTags[1,2]
  laTags[1,1]='cBusdocu+cStyType+PO+cInvType+Style+STR(LineNo,6)+TranCd'
  laTags[1,2]= loFormSet.lcTempFile
  =gfCrtTmp(loFormSet.lcTempFile,@laFileStru,@laTags)
  llFirstTmp=.T.
Endif

*-- If we did the same calculations befor don't do it again .
If loFormSet.ariaform1.kbPoNo.keytextbox.Value == loFormSet.ariaform1.kbPoNo.keytextbox.OldValue
  Return
Endif

Select (loFormSet.lcTempFile)
Set Order To Tag loFormSet.lcTempFile

lcSeekTyp  = loFormSet.lcWorkOrd
lcSeekCond = loFormSet.lcBusDoc+loFormSet.lcWorkOrd+loFormSet.lcPoNo+loFormSet.lcInvType

If !Seek(lcSeekCond)
  Select (loFormSet.lcPosLn)
  If Seek(loFormSet.lcBusDoc+loFormSet.lcWorkOrd+loFormSet.lcPoNo+loFormSet.lcInvType)
    Do While cBusDocu+cStyType+PO+cInvType+Style+Str(Lineno,6)+TranCd = lcSeekCond
      Scatter Memvar
      lcScanKey = cBusDocu+cStyType+PO+cInvType+Style+Str(Lineno,6)
      laOpenQty = 0
      Scan Rest While cBusDocu+cStyType+PO+cInvType+Style+Str(Lineno,6)+TranCd = lcScanKey
        If TranCd = '1'
          For lnCntr = 1 To 8
            lcCntr = Str(lnCntr,1)
            laOpenQty[lnCntr] = laOpenQty[lnCntr] + QTY&lcCntr
          Endfor
          *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
          loFormSet.lcStyle   = Style
          *-- Get the style information
          If loFormSet.lcInvType = "0001"
            *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
            *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = '" + loFormSet.lcStyle + "'"
            *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
            *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = [" + loFormSet.lcStyle + "]"
            lcStySelVal = loFormSet.lcStyle
            lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = ?m.lcStySelVal"
            *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
            *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
          Else
            *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
            *lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loFormSet.lcInvType +;
            "' AND Style = '" + loFormSet.lcStyle + "'"
            lcItemValue = loFormSet.lcStyle
            lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loFormSet.lcInvType +;
              "' AND Style = ?m.lcItemValue "
            *B607658,1 KHM 07/07/2005 [End]
          Endif
          =lfGetItmInf(loFormSet.lcInvType,Iif(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+loFormSet.lcStyle,;
            loFormSet.lcTmpItem,Iif(loFormSet.lcInvType = "0001",'STYLE','ITEM'),lcSqlStatement,loFormSet.lcInvType = "0002")
          *N038893,1 WAM 06/02/2005 (End)
        Else
          If TranCd $ "245"
            For lnCntr = 1 To 8
              lcCntr = Str(lnCntr,1)
              laOpenQty[lnCntr] = Max(laOpenQty[lnCntr] - QTY&lcCntr,0)
            Endfor
          Endif
        Endif
      Endscan
      m.TotQty = 0
      For lnCntr = 1 To 8
        m.TotQty = m.TotQty + laOpenQty[lnCntr]
      Endfor
      Insert Into (loFormSet.lcTempFile) From Memvar
    Enddo
  Endif
Endif
Select (loFormSet.lcPosLn)
Go lnFilePos

*!*************************************************************
*! Name      : lfvShipmnt
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To validate the shipment code
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
Function lfvShipmnt
Lparameters loFormSet

Local llRet
Private lcShipNo, lcTrancd
llRet = .T.
lcShipNo = loFormSet.ariaform1.cntShipment.kbShipNo.keytextbox.Value
If Empty(lcShipNo)
  Return .F.
Endif

*--Shipment validation.
*B611953,1 ES 10/29/2019 Shipment is not marked as locked once user selected it in PO receiving screen [Start]
IF !loFormSet.recordLock(.T.)
  RETURN .F.
ENDIF
*B611953,1 ES 10/29/2019 Shipment is not marked as locked once user selected it in PO receiving screen [End]
If Evaluate(loFormSet.lcMastShp+'.Status') = 'C'
  *--This shipment has been received complete! unable to proceed.
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *= gfModalGen('TRM34078B42000','DIALOG','completely received')
  = gfModalGen('TRM34078B42000','DIALOG',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_COMPREC,loFormSet.GetHeaderText("LANG_POSTREC_COMPREC",loFormSet.HeaderAlias)))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  loFormSet.ariaform1.cntShipment.kbShipNo.keytextbox.Value = ""
  Return .F.
Endif

If Evaluate(loFormSet.lcMastShp+'.Status') = 'X'
  *--This shipment has been canceled! unable to proceed.
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *= gfModalGen('TRM34078B42000','DIALOG','canceled')
  = gfModalGen('TRM34078B42000','DIALOG',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_CANCELLED,loFormSet.GetHeaderText("LANG_POSTREC_CANCELLED",loFormSet.HeaderAlias)))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  loFormSet.ariaform1.cntShipment.kbShipNo.keytextbox.Value = ""
  Return .F.
Endif

*: E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
If loFormSet.lcPType $ 'S' And  Evaluate(loFormSet.lcMastShp+'.Status') <> 'O'
  *-- The shipment is not Open, can not proceed.
  loFormSet.ariaform1.cntShipment.kbShipNo.keytextbox.Value = ""
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *= gfModalGen('TRM34134B00000','DIALOG',"Hold")
  = gfModalGen('TRM34134B00000','DIALOG',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_HOLD,loFormSet.GetHeaderText("LANG_POSTREC_HOLD",loFormSet.HeaderAlias)))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  Return .F.
Endif
*: E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]

lcTrancd = Iif(loFormSet.lcPType $ 'SUF','3','6')
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
  "' AND POSLN.ShipNo = '" + lcShipNo + "' AND TranCd ='" + lcTrancd + "'"
*B608491,1 WAM 03/25/2008 (End)

=lfOpenSql(lcSqlStatement,'POSLN',loFormSet.lcPosLn, "","",.F.)
Dimension laIndex[1,2]
laIndex = ''
laIndex[1,1] = 'cBusDocu+cStyType+PO+cInvType+Style+STR(LineNo,6)+TranCd'
laIndex[1,2] = 'POSLN'
=lfSetIndex(loFormSet.lcPosLn,@laIndex)

*! C201211,1 HES 01/20/2010 Handle Scanned Barcodes from Recieve PO Screen -By Shipment- [Start]
If Ascan(loFormSet.laEvntTrig,Padr('SCANBARCD',10),1,Alen(loFormSet.laEvntTrig,1),1) > 0
  If loFormSet.mDoTrigger(Padr('SCANBARCD' ,10))
    Return
  Endif
Endif
*! C201211,1 HES 01/20/2010 Handle Scanned Barcodes from Recieve PO Screen -By Shipment- [End]

Select (loFormSet.lcPosLn)
Locate
If Eof()
  If loFormSet.lcPType = 'C'
    = gfModalGen('TRM34205B42000','DIALOG')
  Else
    *--The shipment lines have not been found! unable to proceed.
    = gfModalGen('TRM34077B42000','DIALOG')
  Endif
  loFormSet.ariaform1.cntShipment.kbShipNo.keytextbox.Value = ""
  Return .F.
Endif
*B608491,1 WAM 03/25/2008 Get the currency and exchange rates form the BOMLINE to calculate the landed cost in base currenct when receive by shipment
=lfGetBomLn(loFormSet,'P', '', lcShipNo )
*B608491,1 WAM 03/25/2008 (End)

llShpPO = .F.
llRet = lfGetInfo(loFormSet,.F.)

Return llRet


*!*************************************************************
*! Name    : lfvBatch
*! Developer: Timour A. K.
*! Date     : 10/10/97
*! Purpose : Validate Temp. Batch.
*!*************************************************************
Function lfvBatch
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
*!*  LPARAMETERS loFormSet
*!*  WAIT WINDOW "Still under development"
*!*  RETURN .F.
*!*  *E301077,11 MAB Open Required Batch files [Begin]
*!*  =lfOpn_Rest(gcDatadir,'CTKTRCVH','CTKTRCVH')
*!*  =lfOpn_Rest(gcDatadir,'CTKTRCVL','CTKTRCVL')
*! E303848,1 SARA.O 07/09/2017 Saving a ‘Receive by Shipment’ session will allow the user to save the receiving as a batch [Begin]
Lparameters loFormSet,llBrowse
*PARAMETERS loFormSet,llEmpWare


*! E303848,1 SARA.O 07/09/2017 Saving a ‘Receive by Shipment’ session will allow the user to save the receiving as a batch [End]
lcBatch = loFormSet.ariaform1.cntBatch.kbBatchNo.keytextbox.Value
lcPType = loFormSet.lcPType
*B611xxx [Begin]
lcaddedFields = ''
If lcPType = 'S'
  llShpPO = .F.

  lcaddedFields = "ShipNo:H='Shipment #',"
Endif
*B611xxx [End]

If !Used('CTKTRCVH')
  =gfOpenTable('CTKTRCVH','CTKTRCVH')
Endif
If !Used('CTKTRCVL')
  =gfOpenTable('CTKTRCVL','CTKTRCVL')
Endif
If !Used('POSHDR')
  =gfOpenTable('POSHDR','POSHDR')
Endif
If !Used('POSLN')
  =gfOpenTable('POSLN','POSLN')
Endif
*E301077,11 MAB Open Required Batch files [End  ]
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
Select CTKTRCVH

*E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
*IF !SEEK(IIF(llMfCall,'M','I')+lcBatch)

*C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
*IF !SEEK(IIF(llMfCall,'M',IIF(lcPType='L','N','I'))+lcBatch)
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
*!*  IF !SEEK(IIF(llMfCall,'M',IIF(lcPType$'LH','N','I'))+lcBatch)
*!*    *SET FILTER TO cType=IIF(llMfCall,'M','I')
*!*    *SET FILTER TO cType=IIF(llMfCall,'M',IIF(lcPType='L','N','I'))
*!*    SET FILTER TO cType=IIF(llMfCall,'M',IIF(lcPType$'LH','N','I'))
If !gfSEEK(Iif(loFormSet.llMFCall,'M',Iif(loFormSet.lcPType $ 'LHO','N','I'))+lcBatch) Or llBrowse
  =gfSEEK(Iif(loFormSet.llMFCall,'M',Iif(loFormSet.lcPType = 'O','N','I')))
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
  lcBrFields = [TmpRcvNum :8:H=']+;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BATCHBROWSE_REC,loFormSet.GetHeaderText("LANG_POSTREC_BATCHBROWSE_REC",loFormSet.HeaderAlias))+[',]+;
    [cStatus:10:H=']+;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BATCHBROWSE_REC_STATUS,loFormSet.GetHeaderText("LANG_POSTREC_BATCHBROWSE_REC_STATUS",loFormSet.HeaderAlias))+[',]+;
    [cDesc:24:H=']+;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BATCHBROWSE_REC_DESC,loFormSet.GetHeaderText("LANG_POSTREC_BATCHBROWSE_REC_DESC",loFormSet.HeaderAlias))+[',]+;
    [dDate:10:H=']+;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BATCHBROWSE_REC_DATE,loFormSet.GetHeaderText("LANG_POSTREC_BATCHBROWSE_REC_DATE",loFormSet.HeaderAlias))+['    ,]+;
    [nTotStk:8:H=']+;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BATCHBROWSE_TOTSTK,loFormSet.GetHeaderText("LANG_POSTREC_BATCHBROWSE_TOTSTK",loFormSet.HeaderAlias))+['     ,]+;
    [nTotDam:8:H=']+;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BATCHBROWSE_REC_TOTOTH,loFormSet.GetHeaderText("LANG_POSTREC_BATCHBROWSE_REC_TOTOTH",loFormSet.HeaderAlias))+['  ,]+;
    [nTotCan:8:H=']+;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BATCHBROWSE_REC_TOTCAN,loFormSet.GetHeaderText("LANG_POSTREC_BATCHBROWSE_REC_TOTCAN",loFormSet.HeaderAlias))+['  ]

  *B611xxx [Begin]
  *WAIT WINDOW lcaddedFields
  lcBrFields = lcaddedFields+lcBrFields
  *B611xxx [End]
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  Dime laTempData[1]
  Store '' To laTempData
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
  *=gfBrows(.F.,'TMPRCVNUM','laTempData','Temp. Receive Batchs')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *=gfBrows(.F.,'TMPRCVNUM','laTempData',LANG_POSTREC_BATCHBROWSE_TITLE)

  *! E303848,1 SARA.O 07/09/2017 Saving a ‘Receive by Shipment’ session will allow the user to save the receiving as a batch [Begin]
  * =gfBrows(.F.,'TMPRCVNUM','laTempData',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BATCHBROWSE_TITLE,loFormSet.GetHeaderText("LANG_POSTREC_BATCHBROWSE_TITLE",loFormSet.HeaderAlias)))
  *WAIT WINDOW lcPType
  lcFilter = 'FOR EMPTY(shipno)'

  If lcPType = 'S'
    lcFilter = 'FOR !EMPTY(shipno)  and cstatus!="P"'
  Endif
  If lcPType = 'S' And !Empty(Alltrim(loFormSet.ariaform1.cntShipment.kbShipNo.keytextbox.Value))
    * lcFilter = 'FOR !EMPTY(shipno) and shipno = "'+loFormSet.ariaform1.cntShipment.kbShipNo.keytextbox.VALUE+"'"
  Endif
  *WAIT WINDOW lcfilter
  =gfBrows(lcFilter ,'TMPRCVNUM','laTempData',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BATCHBROWSE_TITLE,loFormSet.GetHeaderText("LANG_POSTREC_BATCHBROWSE_TITLE",loFormSet.HeaderAlias)))
  *! E303848,1 SARA.O 07/09/2017 Saving a ‘Receive by Shipment’ session will allow the user to save the receiving as a batch [End]

  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  lcBatch=laTempData[1]
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
  *SET FILTER TO
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
Endif
If Empty(lcBatch) Or Eof('CTKTRCVH')
  Return .F.
Endif
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
*SHOW GET lcBatch
loFormSet.ariaform1.cntBatch.kbBatchNo.keytextbox.Value =lcBatch
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
If lcPType = 'H' .And. CTKTRCVH.cStatus = 'I'
  *--This temporary receiving batch is posted. Cannot proceed.
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *= gfModalGen('TRM34070B42000','DIALOG','is issued')
  = gfModalGen('TRM34070B42000','DIALOG',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ISISSUED,loFormSet.GetHeaderText("LANG_POSTREC_ISISSUED",loFormSet.HeaderAlias)))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  Return .F.
Endif
*C200170,1 AMH Add case of issue inter-location P/O Batch [Start]

*C200170,1 AMH remove case of receive inter-location P/O Batch [Start]
*IF CTKTRCVH.cStatus <> 'A'
If CTKTRCVH.cStatus <> 'A' .And. lcPType <> 'L'
  *C200170,1 AMH [End]

  *--This temporary receiving batch is not approved. Cannot proceed.
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
  *!*    = gfModalGen('TRM34070B42000','DIALOG','is not approved')
  *!*    RETURN .F.
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
Endif

*C200170,1 AMH Check if batch is issued in case of receive inter-location P/O Batch [Start]
If lcPType = 'L' .And. CTKTRCVH.cStatus <> 'I'

  *--This temporary receiving batch is not issued. Cannot proceed.
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *= gfModalGen('TRM34070B42000','DIALOG','is not issued')
  = gfModalGen('TRM34070B42000','DIALOG',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ISNOTISSUED,loFormSet.GetHeaderText("LANG_POSTREC_ISNOTISSUED",loFormSet.HeaderAlias)))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  Return .F.
Endif
*C200170,1 AMH [End]

Select CtKtRcvL

*E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
*IF !SEEK(IIF(llMfCall,'M','I')+lcBatch)
*C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
*IF !SEEK(IIF(llMfCall,'M',IIF(lcPtype='L','N','I')+lcBatch))
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
*IF !SEEK(IIF(llMfCall,'M',IIF(lcPtype$'LH','N','I')+lcBatch))
If !gfSEEK(Iif(loFormSet.llMFCall,'M',Iif(lcPType = 'O','N','I'))+lcBatch)
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  *C200170,1 AMH [End]

  *E301480,1 NAD (End)

  *--This temporary receiving batch has no lines. Cannot proceed.
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
  *= gfModalGen('TRM34070B42000','DIALOG','has no lines')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *= gfModalGen('TRM34070B42000','DIALOG',LANG_POSTREC_BATCHHASNOLINES)
  = gfModalGen('TRM34070B42000','DIALOG',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BATCHHASNOLINES,loFormSet.GetHeaderText("LANG_POSTREC_BATCHHASNOLINES",loFormSet.HeaderAlias)))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  Return .F.
Endif
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
*=lfGetInfo(loFormSet,.F.)
=lfGetInfo(loFormSet, .F.,Iif(loFormSet.lcPType $ 'OIM',.T.,.F.),.T.)
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
*C200170,4 AMH Add case of issue inter-location P/O Batch [Start]
If lcPType = 'H'
  lcSource = CTKTRCVH.VENDOR
Endif
*C200170,4 AMH [End]

Return .T.

*!*************************************************************
*! Name      : lfGetInfo
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To get the lines of the selcted transaction
*!*************************************************************
*! Parameters: loFormSet : FormSet, llEmpWare:
*!*************************************************************
Function lfGetInfo
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[START]
*PARAMETERS loFormSet,llEmpWare
Parameters loFormSet,llEmpWare,llBatch, llfromBatch



lcBatch = loFormSet.ariaform1.cntBatch.kbBatchNo.keytextbox.Value
lcOthrTrCd = '4'
lcCanlTrCd = '5'
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
Local lnAlias, llMatExist,lcSqlStatement, lcMsg1,lcMsg2,lcMsg3, lcMsg4
Private lnCrRt1, lnCrRt2
Store 1 To lnCrRt1, lnCrRt2
lnAlias = Select()

Do Case

  ****************************************************************
  *-- 'S' Style PO Shipment, 'U' Issue Inter-Location PO Shipment
  *-- 'C' Receive Inter-Location PO Shipment
  *-- 'F' Receive Material PO Shipment
  ****************************************************************
Case loFormSet.lcPType $ 'SUCF' And !llShpPO And !llfromBatch

  *: E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
  If loFormSet.lcPType $ 'S'
    =gfOpenTable('SHPRLFLD','SHPRLFLD','SH')
  Endif
  *: E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]

  *-Header info.
  With loFormSet.ariaform1.cntShipment
    .dtpickerShpEntered.Value = Evaluate(loFormSet.lcMastShp+'.Entered')
    .dtpickerShpETA.Value     = Evaluate(loFormSet.lcMastShp+'.Eta')
    .txtShpCartons.Value      = Evaluate(loFormSet.lcMastShp+'.Cartons')
    .txtShpAirWay.Value       = Evaluate(loFormSet.lcMastShp+'.AirWayB')
    .txtShpReference.Value    = Evaluate(loFormSet.lcMastShp+'.Reference')
  Endwith

  Dime laZero[9]
  laZero = 0

  *-Line info.
  lcTPO = Space(6)

  Select (loFormSet.lcPosLn)
  Scan
    If Empty(Status)
      Loop
    Endif
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
    *WAIT WINDOW 'Shipping '+ IIF(loFormSet.lcPType = 'F', 'material: ','style : ')+ Style NOWAIT
    If !loFormSet.llSilentMod
      *N000682,1 MMT 12/09/2012 Globalization changes[Start]
      *WAIT WINDOW 'Shipping '+ IIF(loFormSet.lcPType = 'F', 'material: ','style : ')+ STYLE NOWAIT
      Wait Window Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_SHIPPING,loFormSet.GetHeaderText("LANG_POSTREC_SHIPPING",loFormSet.HeaderAlias))+' '+ Iif(loFormSet.lcPType = 'F', ;
        IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_FABRIC,loFormSet.GetHeaderText("LANG_POSTREC_MSG_FABRIC",loFormSet.HeaderAlias))+': ',;
        IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_STYLE,loFormSet.GetHeaderText("LANG_POSTREC_MSG_STYLE",loFormSet.HeaderAlias))+' : ')+ Style Nowait
      *N000682,1 MMT 12/09/2012 Globalization changes[END]
    Endif
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

    If !Inlist(Status,'O','A')
      If PO = lcTPO
        Loop
      Endif
      If Status = 'H'
        *--The P/O &lcTPO status is Hold since a P/O cost sheet has not
        *--been created yet. Therefore,no receivings can be done for
        *--this P/O. It will be skipped for the shipment.
        *=gfModalGen('TRM34073B42000','DIALOG',PO)
        *N000682,1 MMT 12/09/2012 Globalization changes[Start]
        *!*          lcMsg1  = IIF(loFormSet.lcPType $ 'UC', 'Inter-Location P/O ','P/O ')+PO
        *!*          lcMsg2  = IIF(loFormSet.lcPType $ 'UC', 'an Inter-Location P/O','a P/O')
        *!*          lcMsg3  = IIF(loFormSet.lcPType $ 'UC', 'Inter-Location P/O','P/O')
        *!*          lcMsg4  = IIF(loFormSet.lcPType = 'U','issued','received')
        lcMsg1  = Iif(loFormSet.lcPType $ 'UC',;
          IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_INTERPO,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_INTERPO",loFormSet.HeaderAlias))+' ',;
          IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_PO,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_PO",loFormSet.HeaderAlias))+' ')+PO
        lcMsg2  = Iif(loFormSet.lcPType $ 'UC', ;
          IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ANINTERLOC,loFormSet.GetHeaderText("LANG_POSTREC_ANINTERLOC",loFormSet.HeaderAlias)),;
          IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_APO,loFormSet.GetHeaderText("LANG_POSTREC_APO",loFormSet.HeaderAlias)))
        lcMsg3  = Iif(loFormSet.lcPType $ 'UC', ;
          IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_INTERPO,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_INTERPO",loFormSet.HeaderAlias)),;
          IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_PO,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_PO",loFormSet.HeaderAlias)))
        lcMsg4  = Iif(loFormSet.lcPType = 'U',;
          IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ISSUED,loFormSet.GetHeaderText("LANG_POSTREC_ISSUED",loFormSet.HeaderAlias)),;
          IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_REVD,loFormSet.GetHeaderText("LANG_POSTREC_REVD",loFormSet.HeaderAlias)))
        *N000682,1 MMT 12/09/2012 Globalization changes[END]
        *-- ð status is On Hold since ð cost sheet has not yet been created.
        *-- This ð cannot be received and will be excluded from the shipment.
        =gfModalGen('TRM34206B42000','DIALOG',lcMsg1+'|'+lcMsg2+'|'+lcMsg3+'|'+lcMsg4)
      Endif

      If Status = 'S'
        *--The P/O &lcTPO status is Closed, the P/O will be skipped for the shipment.
        *=gfModalGen('TRM34074B42000','DIALOG',PO)
        *N000682,1 MMT 12/09/2012 Globalization changes[Start]
        *lcMsg1  = IIF(loFormSet.lcPType $ 'UC', 'Inter-Location P/O ','P/O ')+PO
        lcMsg1  = Iif(loFormSet.lcPType $ 'UC', Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_INTERPO,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_INTERPO",loFormSet.HeaderAlias))+' ',;
          IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_PO,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_PO",loFormSet.HeaderAlias))+' ')+PO
        *N000682,1 MMT 12/09/2012 Globalization changes[end]
        =gfModalGen('TRM34207B42000','DIALOG',lcMsg1)
      ENDIF
      
      *! B611657,1 ES 01/23/2019 Receiving program does not inform user that Shipment included POs are completed[T20180723.0178][Start]
      IF Status = 'C'
        lcMsg1  = Iif(loFormSet.lcPType $ 'UC', Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_INTERPO,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_INTERPO",loFormSet.HeaderAlias))+' ',;
          IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLE_PO,loFormSet.GetHeaderText("LANG_POSTREC_TITLE_PO",loFormSet.HeaderAlias))+' ')+PO
        =gfModalGen('TRM34223B42000','DIALOG',lcMsg1)        
      ENDIF
      *! B611657,1 ES 01/23/2019 Receiving program does not inform user that Shipment included POs are completed[T20180723.0178][End]      
      
      lcTPO = PO
      Loop
    Endif

    If loFormSet.llImpCost And loFormSet.lcWorkOrd <> 'M' && Material PO
      If  PO <> lcTPO
        llMatExist = .F.
        lcCstShtType = Iif(loFormSet.lcWorkOrd $ 'UC','N','I')
        *-- Check if there a work order cost sheet and there is a material,
        *-- style component or trim and inventory maintained.
        lcSqlStatement = "SELECT cImTyp, CutTkt FROM CTKTBOM [INDEX=CTKTBOM] "+;
          " WHERE cImTyp = '" + lcCstShtType + "' AND "+;
          "CutTkt ='" + PO +;
          "' AND (cCatgTyp = 'F' OR cCatgTyp = 'S' OR "+;
          "(cCatgTyp = 'T' AND Trim_Invt=1))"

        =lfWOrdBOM(lcSqlStatement,.T.,.F.,.F.,.F.,loFormSet)
        If Used(loFormSet.lcCTktBom)
          Select (loFormSet.lcCTktBom)
          Locate
          llMatExist = !Eof()
        Endif

        Select (loFormSet.lcPosLn)
        If llMatExist
          *-- Check if there an issued item.
          lcSqlStatement = "SELECT cImTyp, cTktNo FROM BOMCOST [INDEX=POBOMCLS] "+;
            " WHERE cImTyp = '" + lcCstShtType + "' AND "+;
            "cTktNo ='" + PO + "'"
          =lfWOrdBOM(lcSqlStatement,.F.,.T.,.F.,.F.,loFormSet)

          If Used(loFormSet.lcBomCost)
            Select(loFormSet.lcBomCost)
            Locate
            *--No cost items have been applied against this P/o. Are you sure you want to
            *-- receive ?
            If Eof() And gfModalGen('QRM34071B42002','DIALOG') = 2
              Loop
            Endif
          Endif
        Endif
        Select (loFormSet.lcPosLn)
        lcTPO = PO
      Endif
    Endif  && End if detail importing.
    Store 1 To lnCrRt1, lnCrRt2
    Select (loFormSet.lcPosLn)
    If loFormSet.llMulCurr
      loFormSet.lcCur1 = cPriceCur
      lnCrRt1 = Iif(cPriceCur = oAriaApplication.BaseCurrency, 1,;
        gfChkRate('loFormSet.lnCurrUnt1',cPriceCur,;
        loFormSet.ariaform1.dtpickerReceivingDate.Value,loFormSet.llEditExRt,;
        oAriaApplication.ActiveCompanyID,.F.))

      loFormSet.lcCur2 = cDutyCur
      lnCrRt2 = Iif(cDutyCur = oAriaApplication.BaseCurrency, 1,;
        gfChkRate('loFormSet.lnCurrUnt2',cDutyCur,;
        loFormSet.ariaform1.dtpickerReceivingDate.Value,loFormSet.llEditExRt,;
        oAriaApplication.ActiveCompanyID,.F.))
    Endif

    If lnCrRt1 = 0 Or lnCrRt2 = 0
      If !loFormSet.llEditExRt
        *--This line has currency with zero rate, it will be ignored.'
        = gfModalGen('TRM34079B42000','DIALOG')
        Loop
      Else
        Store 1 To lnCrRt1,lnCrRt2
      Endif
    Endif

    *-- Get the style information
    If loFormSet.lcInvType = "0001"
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
      *!*	      lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = '" +;
      *!*	        EVALUATE(loFormSet.lcPosLn+'.Style') + "'"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
      *!*	      lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = [" +;
      *!*	        EVALUATE(loFormSet.lcPosLn+'.Style') + "]"
      lcSelStyValue = Evaluate(loFormSet.lcPosLn+'.Style')
      lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = ?m.lcSelStyValue"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
    Else
      *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
      *lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loFormSet.lcInvType +;
      "' AND Style = '" + EVALUATE(loFormSet.lcPosLn+'.Style') + "'"
      Private lcItemValue
      lcItemValue = Evaluate(loFormSet.lcPosLn+'.Style')
      lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loFormSet.lcInvType +;
        "' AND Style = ?m.lcItemValue "
      *B607658,1 KHM 07/07/2005 [End]
    Endif
    =lfGetItmInf(loFormSet.lcInvType,;
      IIF(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+;
      EVALUATE(loFormSet.lcPosLn+'.Style'),;
      loFormSet.lcTmpItem,;
      IIF(loFormSet.lcInvType = "0001",'STYLE','ITEM'),lcSqlStatement,;
      loFormSet.lcInvType = "0002")

    If Seek(Iif(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+;
        EVALUATE(loFormSet.lcPosLn+'.Style'),loFormSet.lcTmpItem)
      m.cStyDesc = Evaluate(loFormSet.lcTmpItem+'.Desc1')
      m.cDye_Flg = Evaluate(loFormSet.lcTmpItem+'.cDye_Flg')
    Endif

    Select (loFormSet.lcPosLn)
    Scatter Memvar
    Scatter Fields nFCost1,nFCost2,nFCost3,nFCost4,nFCost5,nFCost6,nFCost7 To laEstiCost

    *B608718,1 WAM 10/09/2008 Store budget quantity
    Scatter Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty To laEstQty
    *B608718,1 WAM 10/09/2008 (End)

    *B126833,1 WAM 04/03/2005 add new button to edit landed cost for styles has no detail costing
    m.lDetCost = Evaluate(loFormSet.lcTmpItem+'.lDetCost')
    *B126833,1 WAM 04/03/2005 (End)

    Select (loFormSet.lcTmpLine)

    *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [Start]
    If Ascan(loFormSet.laEvntTrig,Padr('MNGFNDL1',10),1,Alen(loFormSet.laEvntTrig,1),1) > 0
      loFormSet.mDoTrigger(Padr('MNGFNDL1' ,10))
    Else
      *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [End]

      Append Blank

      *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [Start]
    Endif
    *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [End]

    Gather Memvar
    Replace TranCd    With '1',;
      ShipNo    With loFormSet.ariaform1.cntShipment.kbShipNo.keytextbox.Value ,;
      nLanPrRat With lnCrRt1,;
      nLanDuRat With lnCrRt2,;
      TotStk    With TotQty,;
      TotDam    With 0,;
      TotCan    With 0,;
      TotBal    With 0

    Gather From laEstiCost Fields nFLanCost1,nFLanCost2,nFLanCost3,nFLanCost4,;
      nFLanCost5,nFLanCost6,nFLanCost7

    *: E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
    If loFormSet.lcPType $ 'S' And Used('SHPRLFLD') .And. gfGetMemVar('M_APRVSHIP')
      If gfSEEK(Evaluate(loFormSet.lcPosLn+'.SHIPNO')+Evaluate(loFormSet.lcPosLn+'.PO')+Str(Evaluate(loFormSet.lcPosLn+'.LINENO'),6),'SHPRLFLD')

        Replace nLanPrRat With SHPRLFLD.NPRICERAT
        Replace nLanDuRat With SHPRLFLD.NDUTYRAT

        *[Start] At GPS the duty rate is ALWAYS by the base currency.
        If Ascan(loFormSet.laEvntTrig,Padr('SETDTYRT',10),1,Alen(loFormSet.laEvntTrig,1),1) > 0
          loFormSet.mDoTrigger(Padr('SETDTYRT',10))
        Endif
        *[End]

      Endif
    Endif
    *: E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]
    *B608491,1 WAM 03/25/2008 Get the currency and exchange rates form the BOMLINE to calculate the landed cost in base currenct when receive by shipment
    Gather From laEstiCost Fields nFCost1,nFCost2,nFCost3,nFCost4,nFCost5,nFCost6,nFCost7
    *! B610107,1 MMT 10/04/2012 Add Foreign currency costing to Material MFG order summary[Start]
    *IF loFormSet.lcBusDoc+loFormSet.lcWorkOrd $ 'PP'
    If loFormSet.lcBusDoc+loFormSet.lcWorkOrd $ 'PP|PF'
      *! B610107,1 MMT 10/04/2012 Add Foreign currency costing to Material MFG order summary[End]
      lcPosLn = loFormSet.lcPosLn

      *B608718,1 WAM 10/09/2008 Store budget quantity
      *=lfBaseCost(loFormSet, &lcPosLn..cStyType, &lcPosLn..PO, &lcPosLn..cPriceCur, &lcPosLn..nPriceRat, &lcPosLn..nCurrUnit, &lcPosLn..cDutyCur, &lcPosLn..nDutyRat, &lcPosLn..nDCurUnit)
      =lfBaseCost(loFormSet, &lcPosLn..cStyType, &lcPosLn..PO, &lcPosLn..cPriceCur, &lcPosLn..NPRICERAT, &lcPosLn..nCurrUnit, &lcPosLn..cDutyCur, &lcPosLn..NDUTYRAT, &lcPosLn..nDCurUnit,@laEstQty)
      *B608718,1 WAM 10/09/2008 (End)

    Else
      *B608491,1 WAM 03/25/2008 (End)

      If loFormSet.llMulCurr
        =lfGetEqv('1234567',nLanPrRat,nLanDuRat,loFormSet.lnCurrUnt1,loFormSet.lnCurrUnt2,;
          nFLanCost1,nFLanCost2,nFLanCost3,nFLanCost4,nFLanCost5,nFLanCost6,;
          nFLanCost7,loFormSet)
      Else
        =Acopy(laEstiCost,loFormSet.laECost)
      Endif
      *B608491,1 WAM 03/25/2008 Get the currency and exchange rates form the BOMLINE to calculate the landed cost in base currenct when receive by shipment
    Endif
    *: C200919,1 MMT 01/27/2008 Add trig. to PO price ex. Rate while Receiving[Start]
    If cPriceCur <> oAriaApplication.BaseCurrency And Ascan(loFormSet.laEvntTrig ,Padr('GTPOEXRAT',10))<>0
      loFormSet.mDoTrigger(Padr('GTPOEXRAT' ,10))
    Endif
    Select (loFormSet.lcTmpLine)
    *: C200919,1 MMT 01/27/2008 Add trig. to PO price ex. Rate while Receiving[End]

    Gather From loFormSet.laECost Fields nICost1,nICost2,nICost3,nICost4,nICost5,nICost6,nICost7
    Replace nLanPrRat With lnCrRt1
    *B608491,1 WAM 03/25/2008 (End)

    Gather From loFormSet.laECost Fields nLan_Cost1,nLan_Cost2,nLan_Cost3,nLan_Cost4,nLan_Cost5,;
      nLan_Cost6,nLan_Cost7

    *B608760,1 WAM 12/04/2008 Get foreign cost from the adjusted record for receiving
    Gather From laEstiCost Fields nFLanCost1,nFLanCost2,nFLanCost3,nFLanCost4,nFLanCost5,nFLanCost6,nFLanCost7
    *B608760,1 WAM 12/04/2008 (End)

    *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [Start]
    If Ascan(loFormSet.laEvntTrig,Padr('MNGFNDL2',10),1,Alen(loFormSet.laEvntTrig,1),1) > 0
      If !loFormSet.mDoTrigger(Padr('MNGFNDL2' ,10))
        Return
      Endif
    Else
      *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [End]

      loFormSet.lnTotStk = loFormSet.lnTotStk + TotStk
      Scatter Memvar
      Append Blank
      Gather Memvar

      Replace TranCd With Iif(loFormSet.lcWorkOrd = 'U','6','2')
      Gather From laZero Fields Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd

      *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [Start]
    Endif
    *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [End]

  Endscan
  Select(loFormSet.lcMastShp)
  If loFormSet.lcPType <> 'U'
    *B128070,1 KHM 05/19/2005 Commented out. There is no need to update the table here [Begin]
    *REPLACE Recv_Stk  WITH Recv_Stk + loFormSet.lnTotStk
    *B128070,1 KHM 05/19/2005 [End]
  Endif

  Wait Clear
  Select (loFormSet.lcTmpLine)
  Locate

  ************************************
  *-- F R O M  P O  B A T C H. --*
  ************************************
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  *CASE loFormSet.lcPType $ 'BTLH'
Case (loFormSet.lcPType $ 'BTLHOIM' And Iif(loFormSet.lcPType $ 'MOI',!Empty(lcBatch) And llBatch,.F.)) Or llfromBatch
  lcPType1 = loFormSet.lcPType
  loFormSet.lcPType = 'I'
  lcPType = 'I'

  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  *!*      llByCarton = CTKTRCVH.Carton
  *!*      lcBDesc    = CTKTRCVH.cDesc
  *!*      lcBStatus  = IIF(lcPType='L','ISSUED','APPROVED')
  *!*      ldBDate    = CTKTRCVH.dDate
  *!*      lnTotStk   = CTKTRCVH.nTotStk
  *!*      lnTotDam   = CTKTRCVH.nTotDam
  *!*      lnTotCan   = CTKTRCVH.nTotCan

  *B611xxx [Begin]

  If llfromBatch
    lcShipNo = CTKTRCVH.ShipNo
    lcAliasSel = Alias()
    If !Used('SHPMTHDR')
      =gfOpenTable('SHPMTHDR','SHPMTHDR')
    Endif
    Select SHPMTHDR
    loFormSet.ariaform1.cntShipment.kbShipNo.keytextbox.Value = lcShipNo
    If gfSEEK('PP'+CTKTRCVH.ShipNo)
      Scatter Memvar Memo
      Select (loFormSet.lcMastShp)
      Locate
      If Eof()
        Append Blank
      Endif
      Gather Memvar Memo
      *! B611784,1 MMT 06/12/2019 Fix error while receiving by shipment using temp. Receiving batch[P20171120.0011][Start]
      =TABLEUPDATE(.T.)
      *! B611784,1 MMT 06/12/2019 Fix error while receiving by shipment using temp. Receiving batch[P20171120.0011][End]
      *WITH loFormSet.ariaform1.cntShipment
      *  .dtpickerShpEntered.VALUE = SHPMTHDR.Entered
      *  .dtpickerShpETA.VALUE     = SHPMTHDR.Eta
      *  .txtShpCartons.VALUE      = SHPMTHDR.Cartons
      *  .txtShpAirWay.VALUE       = SHPMTHDR.AirWayB
      *  .txtShpReference.VALUE    = SHPMTHDR.REFERENCE
      *ENDWITH
    Endif
    If !Empty(Alltrim(lcAliasSel))
      Select &lcAliasSel.
    Endif
  Endif

  *B611xxx [End]


  loFormSet.ariaform1.cntBatch.Enabled =.T.
  loFormSet.ariaform1.txtstock.Value = CTKTRCVH.nTotStk
  loFormSet.ariaform1.txtothers.Value = CTKTRCVH.nTotDam
  loFormSet.ariaform1.txtcancel.Value =  CTKTRCVH.nTotCan
  loFormSet.ariaform1.cntBatch.DtpickerBatchDate.Value = CTKTRCVH.dDate
  loFormSet.ariaform1.cntBatch.DtpickerBatchDate.Enabled =!(CTKTRCVH.cStatus $ 'XP')
  loFormSet.ariaform1.cntBatch.txtBatchDesc.Value = CTKTRCVH.cDesc
  loFormSet.ariaform1.cntBatch.txtBatchDesc.Enabled = !(CTKTRCVH.cStatus $ 'XP')
  loFormSet.ariaform1.cntBatch.cboBatchStatus.Value = CTKTRCVH.cStatus
  If (CTKTRCVH.cStatus <> 'A' And loFormSet.llApproveBatch) And !(CTKTRCVH.cStatus $ 'XP')
    loFormSet.ariaform1.cntBatch.cboBatchStatus.Enabled =.T.
  Else
    loFormSet.ariaform1.cntBatch.cboBatchStatus.Enabled =.F.
  Endif
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  *--Line info.
  lcKey=' '

  Select CtKtRcvL

  *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
  *= SEEK(IIF(llMfCall,'M','I')+lcBatch)
  *IF lcPType = 'B'
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  *= SEEK(IIF(llMfCall,'M',IIF(lcPType='B','I','N'))+lcBatch)
  = gfSEEK(Iif(loFormSet.llMFCall,'M',Iif(loFormSet.lcPType$'BI','I','N'))+lcBatch)
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]

  *C200170,1 AMH Add case of issue inter-location P/O Batch [Start]
  *IF lcPType $ 'BL'
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  *IF lcPType $ 'BLH'
  If lcPType $ 'BLHIOM'
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
    *C200170,1 AMH Add case of issue inter-location P/O Batch [Start]

    Private lcTypScan,lcTypSeek
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
    *!*      lcTypScan=IIF(lcPType='B','I','N')
    *!*      lcTypSeek=IIF(lcPType='B','P','N')
    If loFormSet.llMFCall
      lcTypScan='M'
      lcTypSeek='PU'
    Else
      lcTypScan=Iif(lcPType$'BI','I','N')
      lcTypSeek=Iif(lcPType$'BI','PP','NN')
    Endif
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
    *E301480,1 NAD (End)

    *E300935,4 Change Scan While to scan rest while because it's faster.
    *SCAN WHILE cType+TmpRcvNum = 'I'+lcBatch

    *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
    *SCAN REST WHILE cType+TmpRcvNum = 'I'+lcBatch
    *=SEEK('P'+Cuttkt,'POSHDR')

    *C200170,1 AMH intialize lnTotStk in case of issue inter-location P/O Batch [Start]
    If lcPType='H'
      lnTotStk   = 0
    Endif
    *C200170,1 AMH [End]
    Scan Rest While cType+TmpRcvNum = lcTypScan+lcBatch
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
      Store 1 To lnCrRt1, lnCrRt2
      lcCstShtTyp = Iif(loFormSet.lcWorkOrd  = "U","M",;
        IIF(loFormSet.lcWorkOrd  = "P","I",loFormSet.lcWorkOrd))
      lcSqlStatement  =  "SELECT  POSLN.*, POSHDR.cPriceCur, POSHDR.cDutyCur, PosHdr.nPriceRat, PosHdr.nCurrUnit, PosHdr.nDutyRat, PosHdr.nDCurUnit, POSHDR.Status "+;
        "FROM POSLN posln (INDEX=POSLN) inner join POSHDR (INDEX=POSHDR) "+;
        "ON POSHDR.cBusDocu = POSLN.cBusDocu AND POSHDR.cStyType = POSLN.cStyType "+;
        "AND  POSHDR.PO = POSLN.PO "+;
        "WHERE POSLN.cBusDocu = '" + loFormSet.lcBusDoc +;
        "' AND POSLN.cStyType = '" + loFormSet.lcWorkOrd +;
        "' AND POSLN.PO = '" + CtKtRcvL.Cuttkt +;
        "' AND POSLN.cInvType='"+loFormSet.lcInvType + "'"

      =lfOpenSql(lcSqlStatement,'POSLN',loFormSet.lcPosLn, "","",.F.)
      Dimension laIndex[1,2]
      laIndex = ''
      laIndex[1,1] = 'cBusDocu+cStyType+PO+cInvType+Style+STR(LineNo,6)+TranCd'
      laIndex[1,2] = 'POSLN'
      =lfSetIndex(loFormSet.lcPosLn,@laIndex)

      lcSqlStatement = "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+;
        "WHERE cImTyp = '" + lcCstShtTyp +;
        "' AND cTktNo ='" + CtKtRcvL.Cuttkt + "'"
      =lfOpenSql(lcSqlStatement,'BOMLINE',loFormSet.lcMastBomLn, "","",.F.)
      Dimension laIndex[1,2]
      laIndex = ''
      laIndex[1,1] = 'cImTyp+cType+ShipNo+cTktNo+STR(LineNo,6)'
      laIndex[1,2] = loFormSet.lcMastBomLn
      =lfSetIndex(loFormSet.lcMastBomLn,@laIndex)

      Select(loFormSet.lcPosLn)
      =Seek(lcTypSeek+CtKtRcvL.Cuttkt+loFormSet.lcInvType+CtKtRcvL.Style)
      If loFormSet.llMulCurr
        loFormSet.lcCur1 = cPriceCur
        lnCrRt1 = Iif(cPriceCur = oAriaApplication.BaseCurrency, 1,;
          gfChkRate('loFormSet.lnCurrUnt1',cPriceCur,;
          loFormSet.ariaform1.dtpickerReceivingDate.Value,loFormSet.llEditExRt,;
          oAriaApplication.ActiveCompanyID,.F.))

        loFormSet.lcCur2 = cDutyCur
        lnCrRt2 = Iif(cDutyCur = oAriaApplication.BaseCurrency, 1,;
          gfChkRate('loFormSet.lnCurrUnt2',cDutyCur,;
          loFormSet.ariaform1.dtpickerReceivingDate.Value,loFormSet.llEditExRt,;
          oAriaApplication.ActiveCompanyID,.F.))
      Endif
      If lnCrRt1 = 0 Or lnCrRt2 = 0
        If !loFormSet.llEditExRt
          *--This line has currency with zero rate, it will be ignored.'
          = gfModalGen('TRM34079B42000','DIALOG')
          Loop
        Else
          Store 1 To lnCrRt1,lnCrRt2
        Endif
      Endif
      If loFormSet.lcInvType = "0001"
        *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
        *!*	        lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = '" +;
        *!*	          EVALUATE(loFormSet.lcPosLn+'.Style') + "'"
        *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
        *!*	        lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = [" +;
        *!*	          EVALUATE(loFormSet.lcPosLn+'.Style') + "]"
        lcStyleValueSelect = Evaluate(loFormSet.lcPosLn+'.Style')
        lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = ?m.lcStyleValueSelect "
        *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
        *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
      Else
        Private lcItemValue
        lcItemValue = POSLN.Style
        lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loFormSet.lcInvType +;
          "' AND Style = ?m.lcItemValue "
      Endif
      =lfGetItmInf(loFormSet.lcInvType,;
        IIF(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+;
        EVALUATE(loFormSet.lcPosLn+'.Style'),;
        loFormSet.lcTmpItem,;
        IIF(loFormSet.lcInvType = "0001",'STYLE','ITEM'),lcSqlStatement,;
        loFormSet.lcInvType = "0002")

      If Seek(Iif(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+;
          EVALUATE(loFormSet.lcPosLn+'.Style'),loFormSet.lcTmpItem)
        m.cStyDesc = Evaluate(loFormSet.lcTmpItem+'.Desc1')
        m.cDye_Flg = Evaluate(loFormSet.lcTmpItem+'.cDye_Flg')
      Endif

      Select(loFormSet.lcPosLn)
      Scatter Memvar
      Scatter Fields nFCost1,nFCost2,nFCost3,nFCost4,nFCost5,nFCost6,nFCost7 To laEstiCost
      Scatter Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty To laEstQty
      Scatter Fields Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd To laAlo
      m.lDetCost = Evaluate(loFormSet.lcTmpItem+'.lDetCost')
      lcTmpLine = loFormSet.lcTmpLine
      Select CtKtRcvL
      If Cuttkt+Style+Dyelot+cCarton+Str(nLineNo,6)+Str(Lineno,6) <> lcKey
        Select (loFormSet.lcTmpLine)
        Append Blank
        Gather Memvar
        Replace TranCd    With '1',;
          nLanPrRat With lnCrRt1,;
          nLanDuRat With lnCrRt2,;
          TotStk    With CtKtRcvL.TotQty,;
          TotDam    With 0,;
          TotCan    With 0,;
          TotBal    With 0,;
          Qty1      With  CtKtRcvL.Qty1,;
          Qty2      With  CtKtRcvL.Qty2,;
          Qty3      With  CtKtRcvL.Qty3,;
          Qty4      With  CtKtRcvL.Qty4,;
          Qty5      With  CtKtRcvL.Qty5,;
          Qty6      With  CtKtRcvL.Qty6,;
          Qty7      With  CtKtRcvL.Qty7,;
          Qty8      With  CtKtRcvL.Qty8,;
          TotQty    With  CtKtRcvL.TotQty,;
          nLineNo   With CtKtRcvL.nLineNo ,;
          LINENO    With CtKtRcvL.Lineno ,;
          cWareCode With CtKtRcvL.cWareCode,;
          cRetSty   With CtKtRcvL.cRetSty ,;
          cStyGrade With CtKtRcvL.cStyGrade

        Gather From laEstiCost Fields nFLanCost1,nFLanCost2,nFLanCost3,nFLanCost4,;
          nFLanCost5,nFLanCost6,nFLanCost7
        Gather From laEstiCost Fields nFCost1,nFCost2,nFCost3,nFCost4,nFCost5,nFCost6,nFCost7
        If loFormSet.lcBusDoc+loFormSet.lcWorkOrd $ 'PP'
          lcPosLn = loFormSet.lcPosLn
          llZeroQty = .F.
          If laEstQty[9] = 0
            llZeroQty = .T.
            laEstQty[9] = 1
          Endif
          =lfBaseCost(loFormSet, &lcPosLn..cStyType, &lcPosLn..PO, &lcPosLn..cPriceCur, &lcPosLn..NPRICERAT, &lcPosLn..nCurrUnit, &lcPosLn..cDutyCur, &lcPosLn..NDUTYRAT, &lcPosLn..nDCurUnit,@laEstQty)
          If llZeroQty
            laEstQty[9] = 0
          Endif
        Else
          If loFormSet.llMulCurr
            =lfGetEqv('1234567',nLanPrRat,nLanDuRat,loFormSet.lnCurrUnt1,loFormSet.lnCurrUnt2,;
              nFLanCost1,nFLanCost2,nFLanCost3,nFLanCost4,nFLanCost5,nFLanCost6,;
              nFLanCost7,loFormSet)
          Else
            =Acopy(laEstiCost,loFormSet.laECost)
          Endif
        Endif
        Select (loFormSet.lcTmpLine)
        Gather From loFormSet.laECost Fields nICost1,nICost2,nICost3,nICost4,nICost5,nICost6,nICost7
        Replace nLanPrRat With lnCrRt1
        Gather From loFormSet.laECost Fields nLan_Cost1,nLan_Cost2,nLan_Cost3,nLan_Cost4,nLan_Cost5,;
          nLan_Cost6,nLan_Cost7

        Gather From laEstiCost Fields nFLanCost1,nFLanCost2,nFLanCost3,nFLanCost4,nFLanCost5,nFLanCost6,nFLanCost7

        If lcPType='L'
          Replace  TotStk With CtKtRcvL.TotQty
        Endif
        If lcPType='H'
          Replace  TotStk With CtKtRcvL.TotQty;
            TotDam With 0;
            TotCan With 0;
            TotBal With TotQty-TotStk;
            LINENO With nLineNo
          lnTotStk   = lnTotStk + TotStk
        Endif

        loFormSet.lnTotStk = loFormSet.lnTotStk + TotStk

        Select CtKtRcvL
        lcKey = Cuttkt+Style+Dyelot+cCarton+Str(nLineNo,6)+Str(Lineno,6)



      Endif


      Select (lcTmpLine)
      Scatter Memo Memvar
      Append Blank
      Gather Memo Memvar
      Replace TranCd    With CtKtRcvL.TranCd,;
        Qty1      With CtKtRcvL.Qty1,;
        Qty2      With CtKtRcvL.Qty2,;
        Qty3      With CtKtRcvL.Qty3,;
        Qty4      With CtKtRcvL.Qty4,;
        Qty5      With CtKtRcvL.Qty5,;
        Qty6      With CtKtRcvL.Qty6,;
        Qty7      With CtKtRcvL.Qty7,;
        Qty8      With CtKtRcvL.Qty8,;
        TotQty      With  CtKtRcvL.TotQty,;
        cStyGrade With CtKtRcvL.cStyGrade,;
        cRetSty   With CtKtRcvL.cRetSty
    
     *! B611784,1 MMT 06/12/2019 Fix error while receiving by shipment using temp. Receiving batch[P20171120.0011][Start]
     REPLACE SHIPNO WITH CtKtRcvH.ShipNO
     *! B611784,1 MMT 06/12/2019 Fix error while receiving by shipment using temp. Receiving batch[P20171120.0011][End]

      If lcPType='H'
        Replace  TotStk With CtKtRcvL.TotQty;
          TotDam With 0;
          TotCan With 0;
          TotBal With TotQty-TotStk;
          LINENO With nLineNo
      Endif
    Endscan
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  Else  && C/t Batch.
    Scan While cType+TmpRcvNum = 'M'+lcBatch
      Select CUTTKTL
      =Seek(CtKtRcvL.Cuttkt+CtKtRcvL.Style)
      Scatter Fields nCost1,nCost2,nCost3,nCost4,nCost5 To laEstiCost
      Scatter Fields Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd To laAlo
      Select CtKtRcvL
      =Seek(Style,'STYLE')
      *B603100,1 SSH 22/05/2000 Fix the bug of incoret recieved cost in case of
      *B603100,1 SSH 22/05/2000 recieve by batch.
      *SCATTER FIELDS Cuttkt,TranCd,cCarton,Lineno,Style,Dyelot,cWareCode,Reference,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,cStyGrade,nLineNo TO laFields
      Scatter Fields Cuttkt,TranCd,cCarton,Lineno,Style,Dyelot,cWareCode,Reference,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,Style.cStyGrade,nLineNo To laFields
      *B603100,1 SSH 22/05/2000 [End]
      If cType+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+Iif(llByCarton,Str(Lineno,6),'') <> lcKey
        Select (lcTmpLine)
        Append Blank
        Gather From laFields   Fields Cuttkt,TranCd,cCarton,Lineno,Style,Dyelot,cWareCode,Reference,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,cStyGrade,nLineNo
        Gather From laEstiCost Fields nCost1,nCost2,nCost3,nCost4,nCost5
        Gather From laEstiCost Fields nLan_CST1,nLan_CST2,nLan_CST3,nLan_CST4,nLan_CST5
        Replace TranCd With '1'
        Gather From laAlo    Fields Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
        Select CtKtRcvL
        lcKey='M'+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+Iif(llByCarton,Str(Lineno,6),'')
      Endif
      Select (lcTmpLine)
      Append Blank
      Gather From laFields   Fields Cuttkt,TranCd,cCarton,Lineno,Style,Dyelot,cWareCode,Reference,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,cRetSty,cStyGrade,nLineNo
      Gather From laEstiCost Fields nCost1,nCost2,nCost3,nCost4,nCost5
      Gather From laEstiCost Fields nLan_CST1,nLan_CST2,nLan_CST3,nLan_CST4,nLan_CST5
    Endscan

  Endif

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
  Store 0  To lnTStk,lnTDam,lnTCan
  lcVar1=''
  Select CtKtRcvL
  *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
  *=SEEK(IIF(llMfCall,'M','I')+lcBatch)
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  *=SEEK(IIF(llMfCall,'M',IIF(lcPType='L','N','I'))+lcBatch)
  =gfSEEK(Iif(loFormSet.llMFCall,'M',Iif(loFormSet.lcPType$'LO','N','I'))+lcBatch)
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  *E301480,1 NAD (End)

  *E300935,4 Change lcKey to match imported styles with dyelots.
  *lcKey = cType+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+IIF(llByCarton,STR(LineNo,6),'')
  lcKey =  Cuttkt+Style+Dyelot+cCarton+Str(nLineNo,6)+Str(Lineno,6)

  *E301480,1 NAD (Start) Add the Inter location Po batch to the condition.
  *SCAN WHILE cType+TmpRcvNum = IIF(llMfCall,'M','I')+lcBatch
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  Replace TotStk With 0,;
    TotDam With 0,;
    TotCan With 0,;
    TotQty With 0 ,;
    TotBal With 0 For TranCd = '1' In (lcTmpLine)
  Select CtKtRcvL
  lcKeyFld = ''
  Store 0 To lnQtyRc1,lnQtyRc2,lnQtyRc3,lnQtyRc4,lnQtyRc5,lnQtyRc6,lnQtyRc7,lnQtyRc8
  Scan While cType+TmpRcvNum = Iif(loFormSet.llMFCall,'M',Iif(loFormSet.lcPType$'LO','N','I'))+lcBatch
    If lcKeyFld  <>  Cuttkt+Style+Dyelot+cCarton+Str(nLineNo,6)+Str(Lineno,6)
      If !Empty(lcVar1)
        =Seek('1'+lcVar1,lcTmpLine)
        Replace  TotBal With Max(Qty1 - lnQtyRc1,0)+ Max(Qty2 - lnQtyRc2,0)+;
          MAX(Qty3 - lnQtyRc3,0)+ Max(Qty4 - lnQtyRc4,0)+;
          MAX(Qty5 - lnQtyRc5,0)+ Max(Qty6 - lnQtyRc6,0)+;
          MAX(Qty7 - lnQtyRc7,0)+ Max(Qty8 - lnQtyRc8,0) In (lcTmpLine)
      Endif
      Store 0 To lnQtyRc1,lnQtyRc2,lnQtyRc3,lnQtyRc4,lnQtyRc5,lnQtyRc6,lnQtyRc7,lnQtyRc8
      lcSqlStatement  =  "SELECT  POSLN.*, POSHDR.cPriceCur, POSHDR.cDutyCur, PosHdr.nPriceRat, PosHdr.nCurrUnit, PosHdr.nDutyRat, PosHdr.nDCurUnit, POSHDR.Status "+;
        "FROM POSLN posln (INDEX=POSLN) inner join POSHDR (INDEX=POSHDR) "+;
        "ON POSHDR.cBusDocu = POSLN.cBusDocu AND POSHDR.cStyType = POSLN.cStyType "+;
        "AND  POSHDR.PO = POSLN.PO "+;
        "WHERE POSLN.cBusDocu = '" + loFormSet.lcBusDoc +;
        "' AND POSLN.cStyType = '" + loFormSet.lcWorkOrd +;
        "' AND POSLN.PO = '" + CtKtRcvL.Cuttkt +;
        "' AND POSLN.cInvType='"+loFormSet.lcInvType + "' AND TRANCD ='1' AND POSLN.STYLE = '"+CtKtRcvL.Style+"' AND [LINENO] = "+Str(CtKtRcvL.Lineno,6)

      =lfOpenSql(lcSqlStatement,'POSLN',loFormSet.lcPosLn, "","",.F.)
      Dimension laIndex[1,2]
      laIndex = ''
      laIndex[1,1] = 'cBusDocu+cStyType+PO+cInvType+Style+STR(LineNo,6)+TranCd'
      laIndex[1,2] = 'POSLN'
      =lfSetIndex(loFormSet.lcPosLn,@laIndex)
      lcVar1 = CtKtRcvL.cCarton+CtKtRcvL.Cuttkt+CtKtRcvL.Style+CtKtRcvL.Dyelot+CtKtRcvL.cWareCode+Str(CtKtRcvL.Lineno,6)
      =Seek('1'+lcVar1,lcTmpLine)
      For I=1 To 8
        Z=Str(I,1)
        Replace QTY&Z With Evaluate(loFormSet.lcPosLn+'.QTY'+Z) In (lcTmpLine)
      Endfor
      Replace TotQty With Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 In (lcTmpLine)
    Endif

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
    Select CtKtRcvL
    lcVar1 =cCarton+Cuttkt+Style+Dyelot+cWareCode+Str(Lineno,6)
    =Seek('1'+lcVar1,lcTmpLine)
    Do Case
    Case TranCd = '2'
      Replace TotStk With TotStk + CtKtRcvL.TotQty In (lcTmpLine)
    Case TranCd = lcOthrTrCd
      Replace TotDam With TotDam + CtKtRcvL.TotQty  In (lcTmpLine)
    Case TranCd = lcCanlTrCd
      Replace TotCan With TotCan + CtKtRcvL.TotQty  In (lcTmpLine)
    Endcase
    For lnU = 1 To 8
      lcU = Str(lnU,1)
      lnQtyRc&lcU. = lnQtyRc&lcU. + CtKtRcvL.QTY&lcU.
    Endfor

    *REPLACE TotBal WITH TotBal + MAX(Qty1 - CTKTRCVL.Qty1,0)+ MAX(Qty2 - CTKTRCVL.Qty2,0)+;
    MAX(Qty3 - CTKTRCVL.Qty3,0)+ MAX(Qty4 - CTKTRCVL.Qty4,0)+;
    MAX(Qty5 - CTKTRCVL.Qty5,0)+ MAX(Qty6 - CTKTRCVL.Qty6,0)+;
    MAX(Qty7 - CTKTRCVL.Qty7,0)+ MAX(Qty8 - CTKTRCVL.Qty8,0) IN (lcTmpLine)
    lcKeyFld  =  Cuttkt+Style+Dyelot+cCarton+Str(nLineNo,6)+Str(Lineno,6)
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  Endscan
  *--Update last record.
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  *!*	    SELECT (lcTmpLine)
  *!*	    SEEK '1'+lcVar1
  *!*	    REPLACE TotStk WITH lnTStk,;
  *!*	            TotDam WITH lnTDam,;
  *!*	            TotCan WITH lnTCan
  If !Empty(lcVar1)
    =Seek('1'+lcVar1,lcTmpLine)
    Replace  TotBal With Max(Qty1 - lnQtyRc1,0)+ Max(Qty2 - lnQtyRc2,0)+;
      MAX(Qty3 - lnQtyRc3,0)+ Max(Qty4 - lnQtyRc4,0)+;
      MAX(Qty5 - lnQtyRc5,0)+ Max(Qty6 - lnQtyRc6,0)+;
      MAX(Qty7 - lnQtyRc7,0)+ Max(Qty8 - lnQtyRc8,0) In (lcTmpLine)
  Endif
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]

  *--Get originals.

  Select (lcTmpLine)
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
  Go Top

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

  loFormSet.lcPType = lcPType1
  lcPType = lcPType1

  *CASE loFormSet.lcPType $ 'IRNODAEPGM'

Case loFormSet.lcPType $ 'IRNODAEPGMW'
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
  *N038893,1 WAM 06/02/2005 (End)
  Store 1 To lnCrRt1,lnCrRt2
  If loFormSet.llMulCurr
    Select (loFormSet.lcPosLn)
    *B608718,1 WAM 10/09/2008 Store budget quantity
    Scatter Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty To laEstQty
    *B608718,1 WAM 10/09/2008 (End)

    If loFormSet.llMulCurr
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
      loFormSet.lcCur1 = Iif(loFormSet.llNewItem,Evaluate(loFormSet.lcPosHdr+'.cPriceCur'),cPriceCur)
      lnCrRt1 = Iif(loFormSet.lcCur1 = oAriaApplication.BaseCurrency, 1,;
        gfChkRate('loFormSet.lnCurrUnt1',loFormSet.lcCur1,;
        loFormSet.ariaform1.dtpickerReceivingDate.Value,loFormSet.llEditExRt,;
        oAriaApplication.ActiveCompanyID,.F.))

      loFormSet.lcCur2 = Iif(loFormSet.llNewItem,Evaluate(loFormSet.lcPosHdr+'.cDutyCur'),cDutyCur)
      lnCrRt2 = Iif(loFormSet.lcCur2 = oAriaApplication.BaseCurrency, 1,;
        gfChkRate('loFormSet.lnCurrUnt2',loFormSet.lcCur2,;
        loFormSet.ariaform1.dtpickerReceivingDate.Value,loFormSet.llEditExRt,;
        oAriaApplication.ActiveCompanyID,.F.))
      *B000122,1 WAM 03/20/2005 (End)

    Endif

    *--Cannot accept zero or -ve rates.
    If lnCrRt1 = 0 Or lnCrRt2 = 0
      If !loFormSet.llEditExRt
        *--This line has currency with zero rate, it will be ignored.'
        = gfModalGen('TRM34079B42000','DIALOG')
        Store ' ' To loFormSet.lcStyle,lcCur1,lcCur2
        Return
      Else
        Store 1 To lnCrRt1,lnCrRt2
      Endif
    Endif
  Endif
  If loFormSet.llNewItem
    If Seek(Iif(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+loFormSet.lcStyle,;
        loFormSet.lcTmpItem)
      m.cStyDesc = Evaluate(loFormSet.lcTmpItem+'.Desc1')
      m.cDye_Flg = Evaluate(loFormSet.lcTmpItem+'.cDye_Flg')
    Endif
    *B608718,1 WAM 10/09/2008 get cost from style file
    Select (loFormSet.lcTmpItem)
    *B608718,1 WAM 10/09/2008 (End)
    *! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [Start]
    *SCATTER FIELDS nICost1,nICost2,nICost3,nICost4,nICost5,nICost6,nICost7 TO laEstiCost
    lcBomAlias      = gfTempName()
    =gfOpenTable('Bom','multibom',"SH",lcBomAlias)
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
    *lcCstShType = IIF(loFormSet.lcPType $ 'ISB','I',IIF(loFormSet.lcPType $ 'MT','M',""))
    lcCstShType = Iif(loFormSet.lcPType $ 'ISB','I',Iif(loFormSet.lcPType $ 'MT','M',Iif(loFormSet.lcPType = 'W',"T","")))
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
    =gfsqlrun("SELECT  BOM.* FROM BOM INNER JOIN BOMHEADR ON  BOMHEADR.cinvtype = BOM.cinvtype  AND  BOMHEADR.cCstShtTyp = BOM.cCstShtTyp AND "+;
      " BOMHEADR.cItmMajor = BOM.cItmMajor  and  BOMHEADR.ccstsht_id =  BOM.ccstsht_id  WHERE BOM.cinvtype = '"+loFormSet.lcInvType+"' AND BOM.cItmMajor = '" + ;
      SUBSTR(loFormSet.lcStyle,1,loFormSet.lnMjrWid)+ "'" +;
      " AND BOM.cCstShtTyp ='" + lcCstShtType + "' AND BOMHEADR.ldefcstsht = 1",lcBomAlias)
    Select(lcBomAlias)
    *B610904,1 MMT 11/06/2014 Error in PO receiving screen while using scanned batch option[T20141029.0023][Start]
    *LOCATE REST WHILE CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6) =loFormSet.lcInvType+PADR(SUBSTR(loFormSet.lcStyle,1,loFormSet.lnMjrWid),19)+lcCstShTyp
    Locate Rest While cInvType+cItmMajor+cCstShtTyp+cCstSht_Id+TYP+CITMMASK+MFGCODE+CINVTYPC+Item+Str(nLineNo,6) =loFormSet.lcInvType+Padr(Substr(loFormSet.lcStyle,1,loFormSet.lnMjrWid),19)+lcCstShType For CCATGTYP = 'P' And CCURRCODE = Evaluate(loFormSet.lcPosHdr+'.CPRICECUR')
    *B610904,1 MMT 11/06/2014 Error in PO receiving screen while using scanned batch option[T20141029.0023][End]
    If !Found()
      =gfsqlrun("SELECT  BOM.* FROM BOM INNER JOIN BOMHEADR ON  BOMHEADR.cinvtype = BOM.cinvtype  AND  BOMHEADR.cCstShtTyp = BOM.cCstShtTyp AND "+;
        " BOMHEADR.cItmMajor = BOM.cItmMajor and  BOMHEADR.ccstsht_id =  BOM.ccstsht_id  WHERE BOM.cinvtype = '"+loFormSet.lcInvType+"' AND BOM.cItmMajor = '" + ;
        SUBSTR(loFormSet.lcStyle,1,loFormSet.lnMjrWid)+ "'" +;
        " AND BOM.cCstShtTyp ='" + lcCstShType + "' AND BOMHEADR.ccstsht_id IN (Select Top 1 ccstsht_id from BOM WHERE BOM.cinvtype = '"+loFormSet.lcInvType+"' AND BOM.cItmMajor = '" + ;
        SUBSTR(loFormSet.lcStyle,1,loFormSet.lnMjrWid)+ "'" +;
        " AND BOM.cCstShtTyp ='" + lcCstShType + "' AND ccatgtyp ='P' AND CCURRCODE ='"+Evaluate(loFormSet.lcPosHdr+'.CPRICECUR') +"')",lcBomAlias)
    Endif
    Select(lcBomAlias)
    Dimension laEstiCost[7]
    Dimension loFormSet.laECost[7]
    laEstiCost = 0
    loFormSet.laECost = 0
    Scan
      If !Like(Strtran(CITMMASK,'*','?'),loFormSet.lcStyle)
        Loop
      Endif
      If TYP <> '8'
        lnI = Evaluate(TYP)
        lcI = TYP
        lcUntSin   = '/'
        lcExRSin   = gfGetExSin(@lcUntSin, Iif(Empty(CCURRCODE),oAriaApplication.BaseCurrency,CCURRCODE))
        lnExRate   = Iif(nExRate=0,1,nExRate)
        lnCurrUnit = Iif(nCurrUnit=0,1,nCurrUnit)
        loFormSet.laECost[lnI] = loFormSet.laECost[lnI] + TotCost &lcExRSin lnExRate &lcUntSin lnCurrUnit
        laEstiCost[lnI] = laEstiCost[lnI]+ TotCost
      Endif
    Endscan
    *! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [END]

    Select (loFormSet.lcTmpLine)
    loFormSet.lnPolstln = Max(loFormSet.lnPolstln + 1, Evaluate(loFormSet.lcPosHdr+'.LastLine') + 1)

    Append Blank

    lcwarecode = Iif(llEmpWare,'',lcwarecode)
    Replace cBusDocu  With loFormSet.lcBusDoc ,;
      cStyType  With loFormSet.lcWorkOrd,;
      PO        With loFormSet.lcPoNo,;
      TranCd    With '1',;
      VENDOR    With Evaluate(loFormSet.lcPosHdr+'.Vendor'),;
      STYLE     With loFormSet.lcStyle,;
      SCALE     With Evaluate(loFormSet.lcTmpItem+'.Scale'),;
      LINENO    With loFormSet.lnPolstln,;
      cWareCode With lcwarecode,;
      nLanPrRat With lnCrRt1,;
      nLanDuRat With lnCrRt2,;
      TotQty    With 0,;
      TotStk    With 0,;
      TotDam    With 0,;
      TotCan    With 0,;
      TotBal    With 0,;
      lNewLn    With .T.,;
      Dyelot    With Padr(loFormSet.lcDyelot,10)

    Replace cStyDesc  With m.cStyDesc,;
      cDye_Flg  With m.cDye_Flg
    *B000122,1 WAM 03/20/2005 for new line, get price and duty currencies from purchase order
    Replace cPriceCur With loFormSet.lcCur1,;
      cDutyCur  With loFormSet.lcCur2
    *B000122,1 WAM 03/20/2005 (End)
    *B126833,1 WAM 04/03/2005 add new button to edit landed cost for styles has no detail costing
    Replace lDetCost With  Evaluate(loFormSet.lcTmpItem+'.lDetCost')
    *B126833,1 WAM 04/03/2005 (End)

    *B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[Start]
    Replace cInvType With loFormSet.lcInvType
    *B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[End]


    *N000587,1 WAM 12/01/2007 Get equivalent cost from BOM table for each cost element based on saved currency code, exchange rate and unit.
    *! B609634,1 MMT 06/28/2011 Fix bug of Zero Cost in Styinvjl in Case of receiving Interlocation PO[Start]
    *IF loFormSet.lcBusDoc+loFormSet.lcWorkOrd $ 'PP|PD|NN'
    *! B610107,1 MMT 10/04/2012 Add Foreign currency costing to Material MFG order summary[Start]
    *IF loFormSet.lcBusDoc+loFormSet.lcWorkOrd $ 'PP|PD'
    If loFormSet.lcBusDoc+loFormSet.lcWorkOrd $ 'PP|PD|PF'
      *! B610107,1 MMT 10/04/2012 Add Foreign currency costing to Material MFG order summary[End]
      *! B609634,1 MMT 06/28/2011 Fix bug of Zero Cost in Styinvjl in Case of receiving Interlocation PO[END]
      Replace nFCost1    With laEstiCost[1],;
        nFCost2    With laEstiCost[2],;
        nFCost3    With laEstiCost[3],;
        nFCost4    With laEstiCost[4],;
        nFCost5    With laEstiCost[5],;
        nFCost6    With laEstiCost[6],;
        nFCost7    With laEstiCost[7],;
        nFLanCost1 With laEstiCost[1],;
        nFLanCost2 With laEstiCost[2],;
        nFLanCost3 With laEstiCost[3],;
        nFLanCost4 With laEstiCost[4],;
        nFLanCost5 With laEstiCost[5],;
        nFLanCost6 With laEstiCost[6],;
        nFLanCost7 With laEstiCost[7]
      lcPosHdr = loFormSet.lcPosHdr
      *B608718,1 WAM 10/09/2008 Store budget quantity
      *=lfBaseCost(loFormSet, &lcPosHdr..cStyType, &lcPosHdr..PO, &lcPosHdr..cPriceCur, &lcPosHdr..nPriceRat, &lcPosHdr..nCurrUnit, &lcPosHdr..cDutyCur, &lcPosHdr..nDutyRat, &lcPosHdr..nDCurUnit)
      *! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [Start]
      *=lfBaseCost(loFormSet, &lcPosHdr..cStyType, &lcPosHdr..PO, &lcPosHdr..cPriceCur, &lcPosHdr..nPriceRat, &lcPosHdr..nCurrUnit, &lcPosHdr..cDutyCur, &lcPosHdr..nDutyRat, &lcPosHdr..nDCurUnit,@laEs
      Replace Gros_Price With  nFCost1
      *! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [End]
      *B608718,1 WAM 10/09/2008 (End)

      *: C200919,1 MMT 01/27/2008 Add trig. to PO price ex. Rate while Receiving[Start]
      If loFormSet.lcCur1 <> oAriaApplication.BaseCurrency And Ascan(loFormSet.laEvntTrig ,Padr('GTPOEXRAT',10))<>0
        loFormSet.mDoTrigger(Padr('GTPOEXRAT' ,10))
      Endif
      Select (loFormSet.lcTmpLine)
      Replace nLanPrRat With lnCrRt1
      *: C200919,1 MMT 01/27/2008 Add trig. to PO price ex. Rate while Receiving[END]

      Gather From loFormSet.laECost Fields nICost1,nICost2,nICost3,nICost4,nICost5,nICost6,nICost7
      Gather From loFormSet.laECost Fields nLan_Cost1,nLan_Cost2,nLan_Cost3,nLan_Cost4,nLan_Cost5,nLan_Cost6,nLan_Cost7
    Else
      *N000587,1 WAM 12/01/2007 (End)

      If loFormSet.lcCur1 = Evaluate(loFormSet.lcTmpItem+'.cPriceCur')
        Replace nFCost1    With laEstiCost[1],;
          nFLanCost1 With laEstiCost[1]

        =lfGetEqv('1',nLanPrRat,0,loFormSet.lnCurrUnt1,0,nFLanCost1,0,0,0,0,0,0,loFormSet)
        Replace nICost1    With loFormSet.laECost[1],;
          nLan_Cost1 With loFormSet.laECost[1]
      Endif
      If loFormSet.lcCur2 = Evaluate(loFormSet.lcTmpItem+'.cDutyCur')
        Replace nFCost2    With laEstiCost[2],;
          nFCost3    With laEstiCost[3],;
          nFCost4    With laEstiCost[4],;
          nFCost5    With laEstiCost[5],;
          nFCost6    With laEstiCost[6],;
          nFCost7    With laEstiCost[7],;
          nFLanCost2 With laEstiCost[2],;
          nFLanCost3 With laEstiCost[3],;
          nFLanCost4 With laEstiCost[4],;
          nFLanCost5 With laEstiCost[5],;
          nFLanCost6 With laEstiCost[6],;
          nFLanCost7 With laEstiCost[7]

        =lfGetEqv('234567',0,nLanDuRat,0,loFormSet.lnCurrUnt2,0,nFLanCost2,nFLanCost3,;
          nFLanCost4,nFLanCost5,nFLanCost6,nFLanCost7,loFormSet)

        Gather From loFormSet.laECost Fields nICost2,nICost3,nICost4,nICost5,nICost6,nICost7
        Gather From loFormSet.laECost Fields nLan_Cost2,nLan_Cost3,nLan_Cost4,nLan_Cost5,;
          nLan_Cost6,nLan_Cost7
      Endif
      *N000587,1 WAM 12/01/2007 Get equivalent cost from BOM table for each cost element based on saved currency code, exchange rate and unit.
    Endif
    *N000587,1 WAM 12/01/2007 (End)

  Else

    *--Get originals.
    Dime laOpnQty[8]
    laOpnQty  =0
    Select (loFormSet.lcPosLn)
    lnSvRc = Recno()
    Scatter Memvar
    *B126833,1 WAM 04/03/2005 add new button to edit landed cost for styles has no detail costing
    m.lDetCost = Evaluate(loFormSet.lcTmpItem+'.lDetCost')
    *B126833,1 WAM 04/03/2005 (End)

    If Seek(Iif(loFormSet.lcInvType="0001","",loFormSet.lcInvType)+m.Style,;
        loFormSet.lcTmpItem)
      m.cStyDesc = Evaluate(loFormSet.lcTmpItem+'.Desc1')
      m.cDye_Flg = Evaluate(loFormSet.lcTmpItem+'.cDye_Flg')
    Endif
    If loFormSet.lcPType = 'A'
      Scatter Fields nCost1 To laEstiCost
    Else
      *SCATTER FIELDS nICost1,nICost2,nICost3,nICost4,nICost5,nICost6,nICost7 TO laEstiCost
      Scatter Fields nFCost1,nFCost2,nFCost3,nFCost4,nFCost5,nFCost6,nFCost7 To laEstiCost
    Endif

    *--Compute previous open balance on style P/o.----------------
    =lfCalOpen(loFormSet.lcPType,loFormSet.lcPosLn,;
      loFormSet.lcBusDoc+loFormSet.lcWorkOrd+PO+loFormSet.lcInvType+Style+Str(Lineno,6),.F.,loFormSet)
    If Between(lnSvRc,1,Reccount())
      Goto lnSvRc
    Endif
    Select (loFormSet.lcTmpLine)

    *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [Start]
    If Ascan(loFormSet.laEvntTrig,Padr('MNGFNDL1',10),1,Alen(loFormSet.laEvntTrig,1),1) > 0
      loFormSet.mDoTrigger(Padr('MNGFNDL1' ,10))
    Else
      *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [End]

      Append Blank
      m.cWareCode = Iif(llEmpWare,'',m.cWareCode)

      *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [Start]
    Endif
    *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [End]

    Gather Memvar
    Replace TranCd With '1'

    *--If specific operation lot selected.
    If loFormSet.llSpecLot
      Gather From loFormSet.laLotArry Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    Else
      Gather From laOpnQty  Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    Endif
    Replace nLanPrRat With lnCrRt1,;
      nLanDuRat With lnCrRt2,;
      cLotNo    With loFormSet.lcLotNo,;
      clastopr  With loFormSet.lcClrLstOp,;
      TotQty    With Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
      TotStk    With Iif(loFormSet.lcPType $ 'OE' Or loFormSet.lcAuto = 'A',TotQty,0),;
      TotDam    With 0,;
      TotCan    With 0,;
      TotBal    With Iif(loFormSet.lcPType $ 'OE' Or loFormSet.lcAuto = 'A',0,TotQty),;
      Dyelot    With Padr(loFormSet.lcDyelot,10),;
      lAutoMode With (loFormSet.lcAuto='A')
    If loFormSet.lcPType = 'A'
      Gather From laEstiCost Fields nFLanCost1
    Else
      Gather From laEstiCost Fields nFLanCost1,nFLanCost2,nFLanCost3,nFLanCost4,nFLanCost5,;
        nFLanCost6,nFLanCost7
    Endif

    If loFormSet.llMulCurr .And. loFormSet.lcPType # 'A'
      *N000587,1 WAM 12/01/2007 Get equivalent cost from BOM table for each cost element based on saved currency code, exchange rate and unit.
      *! B609634,1 MMT 06/28/2011 Fix bug of Zero Cost in Styinvjl in Case of receiving Interlocation PO[Start]
      *IF loFormSet.lcBusDoc+loFormSet.lcWorkOrd $ 'PP|PD|NN'
      *! B610107,1 MMT 10/04/2012 Add Foreign currency costing to Material MFG order summary[Start]
      *IF loFormSet.lcBusDoc+loFormSet.lcWorkOrd $ 'PP|PD'
      If loFormSet.lcBusDoc+loFormSet.lcWorkOrd $ 'PP|PD|PF'
        *! B610107,1 MMT 10/04/2012 Add Foreign currency costing to Material MFG order summary[End]
        *! B609634,1 MMT 06/28/2011 Fix bug of Zero Cost in Styinvjl in Case of receiving Interlocation PO[END]
        lcPosHdr = loFormSet.lcPosHdr
        *B608718,1 WAM 10/09/2008 Store budget quantity
        *=lfBaseCost(loFormSet, &lcPosHdr..cStyType, &lcPosHdr..PO, &lcPosHdr..cPriceCur, &lcPosHdr..nPriceRat, &lcPosHdr..nCurrUnit, &lcPosHdr..cDutyCur, &lcPosHdr..nDutyRat, &lcPosHdr..nDCurUnit)
        =lfBaseCost(loFormSet, &lcPosHdr..cStyType, &lcPosHdr..PO, &lcPosHdr..cPriceCur, &lcPosHdr..NPRICERAT, &lcPosHdr..nCurrUnit, &lcPosHdr..cDutyCur, &lcPosHdr..NDUTYRAT, &lcPosHdr..nDCurUnit,@laEstQty)
        *B608718,1 WAM 10/09/2008 (End)
        Replace nLanPrRat With lnCrRt1
      Else
        *N000587,1 WAM 12/01/2007 (End)

        =lfGetEqv('1234567',nLanPrRat,nLanDuRat,loFormSet.lnCurrUnt1,loFormSet.lnCurrUnt2,;
          nFLanCost1,nFLanCost2,nFLanCost3,nFLanCost4,nFLanCost5,nFLanCost6,nFLanCost7,loFormSet)
        *N000587,1 WAM 12/01/2007 Get equivalent cost from BOM table for each cost element based on saved currency code, exchange rate and unit.
      Endif
      *N000587,1 WAM 12/01/2007 (End)

    Else
      =Acopy(laEstiCost,loFormSet.laECost)
    Endif
    *: C200919,1 MMT 01/27/2008 Add trig. to PO price ex. Rate while Receiving[Start]
    If loFormSet.lcCur1 <> oAriaApplication.BaseCurrency And Ascan(loFormSet.laEvntTrig ,Padr('GTPOEXRAT',10))<>0
      loFormSet.mDoTrigger(Padr('GTPOEXRAT' ,10))
    Endif
    Select (loFormSet.lcTmpLine)
    Replace nLanPrRat With lnCrRt1
    *: C200919,1 MMT 01/27/2008 Add trig. to PO price ex. Rate while Receiving[END]

    If loFormSet.lcPType = 'A'
      Gather From loFormSet.laECost Fields nLan_Cost1
    Else
      Gather From loFormSet.laECost Fields nLan_Cost1,nLan_Cost2,nLan_Cost3,nLan_Cost4,nLan_Cost5,;
        nLan_Cost6,nLan_Cost7
      *B608760,1 WAM 12/04/2008 Get foreign cost from the adjusted record for receiving
      Gather From laEstiCost Fields nFLanCost1,nFLanCost2,nFLanCost3,nFLanCost4,nFLanCost5,nFLanCost6,nFLanCost7
      *B608760,1 WAM 12/04/2008 (End)

    Endif

    *--If recieve inter Location P/o defult recieved qty by intransit qty.
    *--Or If specific operation lot selected.
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
    *IF loFormSet.lcPType $ 'OE' OR loFormSet.lcAuto = 'A'
    If loFormSet.lcPType $ 'OE' Or loFormSet.lcAuto $ 'AI'
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
      lnTSvRc = Recno()

      *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [Start]
      If Ascan(loFormSet.laEvntTrig,Padr('MNGFNDL2',10),1,Alen(loFormSet.laEvntTrig,1),1) > 0
        If !loFormSet.mDoTrigger(Padr('MNGFNDL2' ,10))
          Return
        Endif
      Else
        *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [End]

        Scatter Memvar
        Append Blank
        Gather Memvar
        Replace TranCd With '2'

        *! B610051,1 MMT 08/16/2012 Receiving Inter-location PO to different location other than target location on PO gives error if Bin location is used[Start]
        If  loFormSet.lcPType = 'O' And Ascan(loFormSet.laEvntTrig,Padr('ADDBNRCD',10),1,Alen(loFormSet.laEvntTrig,1),1) > 0
          =loFormSet.mDoTrigger(Padr('ADDBNRCD' ,10))
        Endif
        *! B610051,1 MMT 08/16/2012 Receiving Inter-location PO to different location other than target location on PO gives error if Bin location is used[END]

        If Between(lnTSvRc,1,Reccount())
          Goto lnTSvRc
        Endif

        loFormSet.lnTotStk = loFormSet.lnTotStk + TotStk

        *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [Start]
      Endif
      *! C201211,2 HES 01/20/2010 Handle the scanning to be per piece [End]
    Endif

  Endif

Endcase

*--Swich the mode to Add if there is lines selected.
*B611xxx [Begin]
lnCntBatchTop = loFormSet.ariaform1.cntBatch.Top

*B611xxx [End]

*B611xxx [End]SELECT (loFormSet.lcTmpLine)
If !Eof()
  loFormSet.AlterMode("A")
Else
  Go Top
  If !Eof()
    loFormSet.AlterMode("A")
  Endif
Endif

*B611xxx [Begin]
If loFormSet.lcPType ='S' And loFormSet.ariaform1.cntBatch.Visible = .T.
  loFormSet.ariaform1.cntBatch.Top = lnCntBatchTop
Endif
*B611xxx [End]


*-- In case of receive by shipment and the screen mode is still in the select mode
*-- then return to the select mode. This is done in case you select a shipment that has only
*-- a PO with status hold.
*B611xxx [Begin]
*IF loFormSet.lcPType $ 'USC' AND loFormSet.ActiveMode = 'S'
If loFormSet.lcPType $ 'USC' And loFormSet.ActiveMode = 'S'
  *AND !(loFormSet.ariaform1.cntBatch.VISIBLE = .T. AND !EMPTY(loFormSet.ariaform1.cntBatch.KbBatchno.value))
  *B611xxx [End]
  =lfClearInfo(loFormSet)
  loFormSet.ariaform1.cntShipment.kbShipNo.Enabled = .T.
  Store .T. To loFormSet.ariaform1.dtPickerPostingDate.Enabled,;
    loFormSet.ariaform1.dtpickerReceivingDate.Enabled
  loFormSet.ariaform1.cmdNew.Enabled = .F.
  Return .F.
Endif

Select (loFormSet.lcTmpLine)
Local lnRecNo
lnRecNo = Recno()
Set Filter To TranCd = "1"
Locate

If !Eof()
  If Between(lnRecNo,1,Reccount())
    Goto lnRecNo
  Endif
  lfCntrSour(loFormSet,.T.)
Endif
loFormSet.ariaform1.grdReceivingLines.Enabled = .T.
loFormSet.ariaform1.grdReceivingLines.Refresh
=lfActBrow(loFormSet)
=lfReadLine(loFormSet,Eof())
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
If loFormSet.lcPType $ 'IOM' And llBatch
  If loFormSet.ariaform1.cntBatch.cboBatchStatus.Enabled
    Dimension loFormSet.laStatusArr[2,2]
    lcValStat = loFormSet.ariaform1.cntBatch.cboBatchStatus.Value
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *loFormSet.laStatusArr[1,1]  = LANG_POSTREC_STATUS_OPEN
    loFormSet.laStatusArr[1,1]  = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_STATUS_OPEN,loFormSet.GetHeaderText("LANG_POSTREC_STATUS_OPEN",loFormSet.HeaderAlias))
    *N000682,1 11/20/2012 MMT Globlization changes[End]
    loFormSet.laStatusArr[1,2]  = LANG_POSTREC_STATUS_O

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *loFormSet.laStatusArr[2,1]  = LANG_POSTREC_STATUS_APPROVED
    loFormSet.laStatusArr[2,1]  = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_STATUS_APPROVED,loFormSet.GetHeaderText("LANG_POSTREC_STATUS_APPROVED",loFormSet.HeaderAlias))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    loFormSet.laStatusArr[2,2]  = LANG_POSTREC_STATUS_A

    loFormSet.ariaform1.cntBatch.cboBatchStatus.Requery()
    loFormSet.ariaform1.cntBatch.cboBatchStatus.Value = lcValStat
  Else
    lcValStat = loFormSet.ariaform1.cntBatch.cboBatchStatus.Value
    Dimension loFormSet.laStatusArr[4,2]
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *loFormSet.laStatusArr[1,1]  = LANG_POSTREC_STATUS_OPEN
    loFormSet.laStatusArr[1,1]  = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_STATUS_OPEN,loFormSet.GetHeaderText("LANG_POSTREC_STATUS_OPEN",loFormSet.HeaderAlias))
    *N000682,1 11/20/2012 MMT Globlization changes[End]
    loFormSet.laStatusArr[1,2]  = LANG_POSTREC_STATUS_O

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *loFormSet.laStatusArr[2,1]  = LANG_POSTREC_STATUS_APPROVED
    loFormSet.laStatusArr[2,1]  = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_STATUS_APPROVED,loFormSet.GetHeaderText("LANG_POSTREC_STATUS_APPROVED",loFormSet.HeaderAlias))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    loFormSet.laStatusArr[2,2]  = LANG_POSTREC_STATUS_A

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *loFormSet.laStatusArr[3,1]  = LANG_POSTREC_STATUS_CANCEL
    loFormSet.laStatusArr[3,1]  = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_STATUS_CANCEL,loFormSet.GetHeaderText("LANG_POSTREC_STATUS_CANCEL",loFormSet.HeaderAlias))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    loFormSet.laStatusArr[3,2]  = LANG_POSTREC_STATUS_X

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *loFormSet.laStatusArr[4,1]  = LANG_POSTREC_STATUS_POSTED
    loFormSet.laStatusArr[4,1]  = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_STATUS_POSTED,loFormSet.GetHeaderText("LANG_POSTREC_STATUS_POSTED",loFormSet.HeaderAlias))
    *N000682,1 11/20/2012 MMT Globlization changes[End]


    loFormSet.laStatusArr[4,2]  = LANG_POSTREC_STATUS_P

    loFormSet.ariaform1.cntBatch.cboBatchStatus.Requery()
    loFormSet.ariaform1.cntBatch.cboBatchStatus.Value = lcValStat
  Endif
  If loFormSet.ariaform1.cntBatch.cboBatchStatus.Value $ 'PX'
    loFormSet.oToolBar.cmdadd.Enabled =.F.
  Endif
Endif
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
*-- 'S' Style PO Shipment, 'C' Receive Inter-Location PO Shipment
*-- 'F' Receive Material PO Shipment

If loFormSet.lcPType $ 'SCFU' And !llShpPO
  With loFormSet.ariaform1.cntShipment
    .dtpickerShpEntered.ControlSource = loFormSet.lcMastShp+'.Entered'
    .dtpickerShpETA.ControlSource     = loFormSet.lcMastShp+'.Eta'
    .txtShpCartons.ControlSource      = loFormSet.lcMastShp+'.Cartons'
    .txtShpAirWay.ControlSource       = loFormSet.lcMastShp+'.AirWayB'
    .txtShpReference.ControlSource    = loFormSet.lcMastShp+'.Reference'
  Endwith
Endif

Select(lnAlias)
Return
*-- end of lfGetInfo.

*!*************************************************************
*! Name      : lfvDyelot
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To validate the dyelot
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
Function lfvDyelot
Lparameters loFormSet

If Padr(Evaluate(loFormSet.lcTmpLine+'.Dyelot'),10) = Padr(loFormSet.lcDyelot,10)
  Return
Endif

Local lcMessage, lcKey, lnLineNo, lnRecNo, lcDyeCode
*lcCarton  = ""
*lnLineNo  = EVALUATE(loFormSet.lcPosLn+'.LineNo')
*lnRecNo   = RECNO(loFormSet.lcTmpLine)
lcDyeCode = Padr(loFormSet.ariaform1.kbconfiguration.keytextbox.Value,10)

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

Select (loFormSet.lcTmpLine)
lnRecNo   = Recno()
lcLKey    = cCarton+PO+Style+loFormSet.lcDyelot+Padr(cWareCode,6)+Str(Lineno,6)
Set Filter To
Set Order To Tag TmpLine2
Seek lcLKey+'2'
Replace Rest Dyelot With lcDyeCode ;
  WHILE cCarton+PO+Style+Dyelot+cWareCode+Str(Lineno,6)+TranCd = lcLKey+'2'

*-- To update the receiving lines
Seek lcLKey+'4'
Replace Rest Dyelot With lcDyeCode ;
  WHILE cCarton+PO+Style+Dyelot+cWareCode+Str(Lineno,6)+TranCd = lcLKey+'4'

*-- To update the receiving lines
Seek lcLKey+'5'
Replace Rest Dyelot With lcDyeCode ;
  WHILE cCarton+PO+Style+Dyelot+cWareCode+Str(Lineno,6)+TranCd = lcLKey+'5'

Set Order To Tag TmpLine1
Set Filter To TranCd = '1'

If Between(lnRecNo,1,Reccount())
  Goto lnRecNo
Endif


Return


*!*************************************************************
*! Name      : lfClearInfo
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To clear the information in case of nothing selected
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
Function lfClearInfo
Lparameters loFormSet
*--Clear shipment and batch header.
With loFormSet.ariaform1
  Store {}  To  .cntShipment.dtpickerShpEntered.Value,;
    .cntShipment.dtpickerShpETA.Value
  Store ""  To  .cntShipment.txtShpCartons.Value,;
    .cntShipment.txtShpAirWay.Value,;
    .cntShipment.txtShpReference.Value
  Store .F. To .cntShipment.dtpickerShpEntered.Enabled,;
    .cntShipment.dtpickerShpETA.Enabled,;
    .cntShipment.txtShpCartons.Enabled,;
    .cntShipment.txtShpAirWay.Enabled,;
    .cntShipment.txtShpReference.Enabled
  *-- Do it in case of batch
  *!*	STORE ' ' TO lcBDesc,lcBStatus
  *!*	STORE {}  TO ldBDate

Endwith
Store 0 To loFormSet.lnTotStk, loFormSet.lnTotDam, loFormSet.lnTotCan, loFormSet.lnPolstln
loFormSet.llByCarton = .F.

If loFormSet.llMFCall And loFormSet.lcPType='M'
  Store '' To loFormSet.ariaform1.kbPoNo.keytextbox.Value,;
    loFormSet.ariaform1.kbItem.keytextbox.Value,;
    loFormSet.ariaform1.txtitemDesc.Value
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
  *loFormSet.AriaForm1.kbItem.KeyTextBox.SetFocus
  If !loFormSet.llSilentMod
    loFormSet.ariaform1.kbItem.keytextbox.SetFocus
  Endif
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

Endif
=lfReadLine(loFormSet,.T.)

Return

*!*************************************************************
*! Name      : lfvWare
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To validate the warehouse field
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
Function lfvWare
Lparameters loFormSet
Local  lnRecNo, lcLKey
Private lcwarecode, lcItemCode, lcDyelotCod

If Substr(loFormSet.laWare[loFormSet.lnWare],1,6) = Evaluate(loFormSet.lcTmpLine+'.cWareCode');
    OR loFormSet.lnWare = 0 Or (loFormSet.llCMInstld And loFormSet.llPOSale)
  Return
Endif

If (loFormSet.lnWare = 1) And ;
    SUBSTR(loFormSet.laWare[loFormSet.lnWare],1,6) <> Evaluate(loFormSet.lcTmpLine+'.cWareCode')
  *loFormSet.lnWare = loFormSet.AriaForm1.cboLocations.OldValue
  loFormSet.lnWare = Ascan(loFormSet.laWare,Evaluate(loFormSet.lcTmpLine+'.cWareCode'),1)
  Return 0
Endif

lcItemCode  = Evaluate(loFormSet.lcTmpLine+'.Style')
lcwarecode  = Substr(loFormSet.laWare[loFormSet.lnWare],1,6)
lcDyelotCod = Padr(Evaluate(loFormSet.lcTmpLine+'.Dyelot'),10)

Select (loFormSet.lcTmpLine)
If lfExistWar()
  lcMessage1 = Iif(loFormSet.lcInvType="0001",'Style','Fabric')+;
    IIF(Empty(loFormSet.lcDyelot),'','/Dyelot')+'/Location'
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
  *!*	  lcMessage2 = IIF(loFormSet.llMfCall,'receiving cutting ticket',;
  *!*	    IIF(loFormSet.lcPType='D','receiving dye order',IIF(loFormSet.lcPType $ 'ON',;
  *!*	    'receiving inter location P/O','receiving purchase order')))
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  lcMessage2 = Iif(loFormSet.llMFCall,;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_RECCT,loFormSet.GetHeaderText("LANG_POSTREC_MSG_RECCT",loFormSet.HeaderAlias)),;
    IIF(loFormSet.lcPType='D',;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_RECDYE,loFormSet.GetHeaderText("LANG_POSTREC_MSG_RECDYE",loFormSet.HeaderAlias)),Iif(loFormSet.lcPType $ 'ON',;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_RECINTER,loFormSet.GetHeaderText("LANG_POSTREC_MSG_RECINTER",loFormSet.HeaderAlias)),Iif(loFormSet.lcPType='W',;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MMO_RECING_MMO,loFormSet.GetHeaderText("LANG_POSTREC_MMO_RECING_MMO",loFormSet.HeaderAlias)),;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_RECPO,loFormSet.GetHeaderText("LANG_POSTREC_MSG_RECPO",loFormSet.HeaderAlias))))))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
  *-This XXXX has been entered on this XXXX.
  = gfModalGen('TRM42107B42000','DIALOG',lcMessage1+'|'+lcMessage2)
  *loFormSet.lnWare = loFormSet.AriaForm1.cboLocations.OldValue
  loFormSet.lnWare = Ascan(loFormSet.laWare,Evaluate(loFormSet.lcTmpLine+'.cWareCode'),1)
  Return 0
Endif

*-- Case of style
If loFormSet.lcInvType = "0001"
  *-- Get item information from stydye file for the target location
  *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
  *!*	  lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
  *!*	    PADR(STYLE,19)+lcWareCode+SPACE(10) + "'"
  *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
  *!*	  lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
  *!*	    PADR(STYLE,19)+lcWareCode+SPACE(10) + "]"
  lcStyValuSele = Padr(Style,19)+lcwarecode+Space(10)
  lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = ?m.lcStyValuSele "
  *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
  *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
  =lfOpenFox(lcSqlStatement,'STYDYE',loFormSet.lcTmpCurs,"")
  Select(loFormSet.lcTmpCurs)
  Locate
  If Eof()
    *-Style: xxx is not assigned to Location: xxx. "\<Add;\<Reenter"
    If gfModalGen('QRM34048B42006','DIALOG',Alltrim(lcItemCode)+'|'+lcwarecode) = 1
      Do gpAdStyWar With lcItemCode,Space(10),lcwarecode
      If !Empty(lcDyelotCod)
        Do gpAdStyWar With lcItemCode,Padr(lcDyelotCod,10),lcwarecode
      Endif
    Else
      *loFormSet.lnWare = loFormSet.AriaForm1.cboLocations.OldValue
      loFormSet.lnWare = Ascan(loFormSet.laWare,Evaluate(loFormSet.lcTmpLine+'.cWareCode'),1)
      Return 0
    Endif
  Else
    If !Empty(lcDyelotCod)
      *-- Get item information from stydye file for the target location
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
      *!*	      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
      *!*	        PADR(STYLE,19)+lcWareCode+lcDyelotCod + "'"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
      *!*	      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
      *!*	        PADR(STYLE,19)+lcWareCode+lcDyelotCod + "]"
      lcValuSeleSty = Padr(Style,19)+lcwarecode+lcDyelotCod
      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = ?m.lcValuSeleSty "
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][END]
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
      =lfOpenFox(lcSqlStatement,'STYDYE',loFormSet.lcTmpCurs,"")
      Select(loFormSet.lcTmpCurs)
      Locate
      If Eof()
        Do gpAdStyWar With lcItemCode,lcDyelotCod,lcwarecode
      Endif
    Endif
  Endif
Else
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
  Select(loFormSet.lcTmpCurs)
  Locate
  If Eof()
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *lcMsg = "Fabric: " + ALLTRIM(lcItemCode)
    lcMsg =  Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_FABRIC,loFormSet.GetHeaderText("LANG_POSTREC_MSG_FABRIC",loFormSet.HeaderAlias))+": " + Alltrim(lcItemCode)
    *N000682,1 MMT 12/09/2012 Globalization changes[end]
    If gfModalGen('QRM36226B34004','DIALOG',lcMsg +'|'+lcwarecode) = 1
      =gfAdItemWar(loFormSet.lcInvType,lcItemCode,Space(10),lcwarecode)
      If !Empty(lcDyelotCod)
        =gfAdItemWar(loFormSet.lcInvType,lcItemCode,lcDyelotCod,lcwarecode)
      Endif
    Else
      *loFormSet.lnWare = loFormSet.AriaForm1.cboLocations.OldValue
      loFormSet.lnWare = Ascan(loFormSet.laWare,Evaluate(loFormSet.lcTmpLine+'.cWareCode'),1)
      Return 0
    Endif
  Else
    If !Empty(lcDyelotCod)
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
      Select(loFormSet.lcTmpCurs)
      Locate
      If Eof()
        =gfAdItemWar(loFormSet.lcInvType,lcItemCode,lcDyelotCod,lcwarecode)
      Endif
    Endif
  Endif
Endif

Select (loFormSet.lcTmpLine)
lnRecNo   = Recno()
lcLKey    = cCarton+PO+Style+Dyelot+Padr(cWareCode,6)+Str(Lineno,6)
Set Filter To
Set Order To Tag TmpLine2
Seek lcLKey
Replace Rest cWareCode With lcwarecode ;
  WHILE cCarton+PO+Style+Dyelot+cWareCode+Str(Lineno,6)+TranCd = lcLKey

*-- To update the receiving lines
Seek lcLKey
Replace Rest cWareCode With lcwarecode ;
  WHILE cCarton+PO+Style+Dyelot+cWareCode+Str(Lineno,6)+TranCd = lcLKey

Set Order To Tag TmpLine1
Set Filter To TranCd = '1'

If Between(lnRecNo,1,Reccount())
  Goto lnRecNo
Endif

Return

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
Function lfExistWar
lcCurrRec  = Padr(cCarton,3)+PO+Padr(Style,19)+Padr(Dyelot,10)+Padr(cWareCode,6)+Str(Lineno,6)
*-- if this record is found in temp. file before
llExists = Seek('1'+Padr(cCarton,3)+Padr(PO,6)+Padr(Style,19)+Padr(Dyelot,10)+;
  PADR(lcwarecode,6)+Str(Lineno,6))
= Seek('1'+lcCurrRec)
Return llExists
*-- end of lfExistWar.

*!*************************************************************
*! Name      : lfvLocWare
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To validate the bins
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
Function lfvLocWare
Lparameters loFormSet
loFormSet.ariaform1.chkBins.Value = .F.
Local lnAlias

Select (loFormSet.lcTmpLine)

If Empty(Evaluate(loFormSet.lcTmpLine+'.cWareCode'))
  *-- Message : 'First, you must select location.'
  =gfModalGen('TRM42150B42001','DIALOG')
  loFormSet.ariaform1.cboLocations.Enabled = .T.
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
  *loFormSet.AriaForm1.cboLocations.SetFocus
  If !loFormSet.llSilentMod
    loFormSet.ariaform1.cboLocations.SetFocus
  Endif
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
  Return
Endif

lnAlias = Select()
Select (loFormSet.lcTemLoc)
Delete All For cWareCode <> Evaluate(loFormSet.lcTmpLine+'.cWareCode') And;
  STYLE = Evaluate(loFormSet.lcTmpLine+'.Style')
Select WhsLoc
Set Order To Tag WhsLoc
llFound = Seek(Evaluate(loFormSet.lcTmpLine+'.cWareCode'))
Set Order To Tag WHSLOCST
If llFound
  =lfvLocat(loFormSet)
Else
  *--No bins have been assigned to location XXX .
  = gfModalGen('TRM42058B42000','DIALOG',Alltrim(Evaluate(loFormSet.lcTmpLine+'.cWareCode')))
Endif
Select (lnAlias)
Return

*!*************************************************************
*! Name      : lfvLocat
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To select the bins from the mover
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
Function lfvLocat
Lparameters loFormSet
Select WhsLoc
Seek Evaluate(loFormSet.lcTmpLine+'.Style')+Space(6)+;
  EVALUATE(loFormSet.lcTmpLine+'.cWareCode')

If !Seek(Evaluate(loFormSet.lcTmpLine+'.Style')+;
    EVALUATE(loFormSet.lcTmpLine+'.cWareCode'),loFormSet.lcTemLoc)
  Scan While Style+Color+cWareCode+CLOCATION = Evaluate(loFormSet.lcTmpLine+'.Style')+;
      SPACE(6)+Evaluate(loFormSet.lcTmpLine+'.cWareCode')
    Scatter Memvar
    Select (loFormSet.lcTemLoc)
    If !Seek(Evaluate(loFormSet.lcTmpLine+'.Style')+;
        EVALUATE(loFormSet.lcTmpLine+'.cWareCode')+WhsLoc.CLOCATION)
      Append Blank
      Gather Memvar
    Endif

  Endscan
Endif

Declare laSource[1],laTarget[1]
Store ' ' To laSource,laTarget
lsSource = 1
Select CLOCATION From WhsLoc ;
  WHERE Style+Color+cWareCode == Space(19)+Space(6)+;
  EVALUATE(loFormSet.lcTmpLine+'.cWareCode');
  INTO Array laSource

Select CLOCATION From (loFormSet.lcTemLoc) ;
  WHERE Style+cWareCode+CLOCATION = Evaluate(loFormSet.lcTmpLine+'.Style')+;
  EVALUATE(loFormSet.lcTmpLine+'.cWareCode');
  INTO Array laTarget

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfMover(@laSource,@laTarget,LANG_POSTREC_AssignBins,.T.,'mvLoc',.F.,.F.,loFormSet)
=gfMover(@laSource,@laTarget,Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_AssignBins,loFormSet.GetHeaderText("LANG_POSTREC_AssignBins",loFormSet.HeaderAlias)),.T.,'mvLoc',.F.,.F.,loFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

Select (loFormSet.lcTemLoc)

Delete For Style+cWareCode+CLOCATION = Evaluate(loFormSet.lcTmpLine+'.Style')+;
  EVALUATE(loFormSet.lcTmpLine+'.cWareCode')

For I = 1 To Alen(laTarget)
  Append Blank
  If Seek (Evaluate(loFormSet.lcTmpLine+'.Style')+Space(6)+;
      EVALUATE(loFormSet.lcTmpLine+'.cWareCode')+laTarget[i],'WHsLoc')
    Tableupdate(0,.T.)
  Endif
  Replace Style     With Evaluate(loFormSet.lcTmpLine+'.Style');
    cWareCode With Evaluate(loFormSet.lcTmpLine+'.cWareCode');
    CLOCATION With laTarget[i]
Endfor
loFormSet.ariaform1.chkBins.Value = Iif(!Empty(laTarget) ,.T.,.F.)

Return

*!*************************************************************
*! Name      : lfvLoc
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To validate the bins in the mover when selected.
*!*************************************************************
*! Parameters: lnOption : Selected option, lnSource: Source #,loFormSet : FormSet
*!*************************************************************
Function lfvLoc
Lparameters lnOption, lnSource, loFormSet
Do Case
Case lnOption=1
  lcLocatin = laSource[lsSource]
  If !Seek(Evaluate(loFormSet.lcTmpLine+'.Style')+Space(6)+;
      EVALUATE(loFormSet.lcTmpLine+'.cWareCode')+lcLocatin,"WhsLoc")
    *--Bin XXX is not assigned to '+'style XXX in location XXX,'\!\<Assign;\<Cancel
    If gfModalGen('TRM42064B42007','DIALOG',Alltrim(lcLocatin)+'|'+;
        ALLTRIM(Evaluate(loFormSet.lcTmpLine+'.Style'))+'|'+;
        ALLTRIM(Evaluate(loFormSet.lcTmpLine+'.cWareCode'))) = 1
      Return .T.
    Else
      Return .F.
    Endif
  Else
    Return .T.
  Endif

Case lnOption=2
  For I=1 To Alen(laSource,1)
    lcLocatin = laSource[I]
    If !Seek(Evaluate(loFormSet.lcTmpLine+'.Style')+Space(6)+;
        EVALUATE(loFormSet.lcTmpLine+'.cWareCode')+lcLocatin,"WhsLoc")
      *--One or more Bin(s) are not assigned to '+'style XXX in location XXX,'\!\<Assign;\<Cancel
      If gfModalGen('TRM42092B42007','DIALOG',Alltrim(Evaluate(loFormSet.lcTmpLine+'.Style'))+;
          '|'+Alltrim(Evaluate(loFormSet.lcTmpLine+'.cWareCode'))) = 1
        Return .T.
      Else
        Return .F.
      Endif
    Endif
  Endfor
Case (lnOption=3 Or lnOption=4)
Endcase
Return

*!*************************************************************
*! Name      : lfvNewLn
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To validate the new button.
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
Function lfvNewLn
Lparameters loFormSet

*-- Remove the control source of the edit region objects
=lfCntrSour(loFormSet,.F.)
=lfReadLine(loFormSet,.T.)
*-- To disable the edit region
=lfObjStatus(loFormSet,.F.)
loFormSet.lnWare = 0
loFormSet.ariaform1.cboLocations.Refresh
If loFormSet.llMFCall And loFormSet.lcPType = 'M'

  lnSelAls = Select(0)
  *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
  *SELECT (lcTmpLine)
  Select (loFormSet.lcTmpLine)
  *N038893,1 WAM 06/02/2005 (End)
  If Empty(Padr(cRcvBy,loFormSet.lnMjrWid))
    *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
    *loFormSet.AriaForm1.kbItem.keyTextBox.Value = ""
    loFormSet.ariaform1.kbItem.Value = ""
    *N038893,1 WAM 06/02/2005 (End)
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
    *loFormSet.AriaForm1.kbPONo.SetFocus
    If !loFormSet.llSilentMod
      loFormSet.ariaform1.kbPoNo.SetFocus
    Endif
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

  Else
    *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
    *loFormSet.AriaForm1.kbItem.keyTextBox.Value = cRcvBy
    loFormSet.ariaform1.kbItem.Value = cRcvBy
    *N038893,1 WAM 06/02/2005 (End)
    loFormSet.ariaform1.kbPoNo.keytextbox.Value = ""
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
    *loFormSet.AriaForm1.kbItem.SetFocus
    If !loFormSet.llSilentMod
      loFormSet.ariaform1.kbItem.SetFocus
    Endif
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

  Endif
  loFormSet.ariaform1.kbItem.Enabled = .T.
  Select (lnSelAls)
Endif

loFormSet.ariaform1.kbPoNo.Enabled = .T.
*! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
*loFormSet.AriaForm1.kbPONo.SetFocus
If !loFormSet.llSilentMod
  loFormSet.ariaform1.kbPoNo.SetFocus
Endif
*! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
loFormSet.ariaform1.kbPoNo.keytextbox.OldValue = ''
Return

*!*************************************************************
*! Name      : lfvRemLn
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To validate the remove button
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
Function lfvRemLn
Lparameters loFormSet
Local lcLKey, lcDyelot, lcwarecode, lcScanVar, lcScanVal,;
  lnCurLine, lcCarton, lcTCode, lcStyle

*-Are you sure you want to delete this line?
If gfModalGen('QRM34036B42002','DIALOG') = 1

  If loFormSet.lcPType $ 'ISMD'
    Private laRemoved
    Declare laRemoved[8]
    laRemoved = 0
  Endif

  Select (loFormSet.lcTmpLine)
  Set Filter To
  *B000109,1 WAM 03/05/2005 Remove Lock PO header
  lcLckPo  = PO
  lclckKey = cBusDocu+cStyType+PO
  *B000109,1 WAM 03/05/2005 (End)
  lcLKey = Space(3)+PO+Style+Dyelot+Padr(cWareCode,6)+Iif(loFormSet.llMFCall,'',Str(Lineno,6))

  *-- Check if the there is cancelled quantities
  If Seek('5'+lcLKey)
    If loFormSet.lcPType $ 'ISMD'
      =lfEvalRemQ()
    Endif
    Delete

  Endif
  *-- Check if the there is damaged or 2nd quality quantities
  If Seek('4'+lcLKey)
    Scan Rest While TranCd+cCarton+PO+Style+Dyelot+;
        PADR(cWareCode,6)+Iif(loFormSet.llMFCall,'',Str(Lineno,6))='4'+lcLKey
      If loFormSet.lcPType $ 'ISMD'
        =lfEvalRemQ()
      Endif
      Delete

    Endscan

  Endif

  *-- Check if there is receive to stock quanities
  If Seek('2'+lcLKey)
    If loFormSet.lcPType $ 'ISMD'
      =lfEvalRemQ()
    Endif
    Delete
  Endif

  If Seek('1'+lcLKey)
    loFormSet.lnTotStk = loFormSet.lnTotStk - TotStk
    loFormSet.lnTotDam = loFormSet.lnTotDam - TotDam
    loFormSet.lnTotCan = loFormSet.lnTotCan - TotCan

    If loFormSet.lcPType $ 'ISMD'
      lnCurLine  = Lineno
      lcCarton   = cCarton
      lcTCode    = PO
      lcStyle    = Style
      lcDyelot   = Dyelot
      lcwarecode = cWareCode
      lcScanVar  = [TranCd+cCarton+Po+Style+Dyelot+cWareCode+STR(LineNo,6)]
      lcScanVal  = ['1' + lcCarton + lcTCode + lcStyle]

      Scan For Evaluate(lcScanVar) = Evaluate(lcScanVal) And ;
          STR(Lineno,6) = Str(lnCurLine,6)  And ;
          Dyelot+cWareCode # lcDyelot+lcwarecode

        For lnI = 1 To 8
          lcZ = Str(lnI,1)
          Replace QTY&lcZ With QTY&lcZ + laRemoved[lnI] ,;
            TotQty  With TotQty  + laRemoved[lnI]
        Endfor
        Replace TotBal With TotQty - TotStk - TotDam - TotCan
      Endscan
      = Seek('1'+lcLKey)

    Endif
    Delete
  Endif

  Go Top
  If Eof()
    =lfReadLine(loFormSet,.T.)
    *-- To disable the edit region
    =lfObjStatus(loFormSet,.F.)
  Else
    =lfReadLine(loFormSet,.F.)
  Endif
  If !(loFormSet.lcPType $ 'SCUF')
    loFormSet.ariaform1.cmdNew.Enabled  = .T.
  Endif

  *B000109,1 WAM 03/05/2005 Remove Lock PO header
  If .F.
    If !Seek(lcLckPo,loFormSet.lcTmpLine,'TmpLine3')
      lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'',.T.)
      If Type('lcTranCode') = 'N'
        =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
        llAbort=.T.
        Exit
      Else
        lcSelString = "UPDATE POSHDR SET lLok_stat =0,cLok_User= '', dLok_Date='',cLok_Time='' WHERE cBusDocu+cStyType+PO='"+lclckKey+"'"
        lnResult = oAriaApplication.RemoteCompanyData.Execute(lcSelString,'',"SAVEFILE","",lcTranCode ,4,'',Set("DataSession"))
        If lnResult <=0
          =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLUPDATE",lnResult,.T.)
          =oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
        Else
          =oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
        Endif
      Endif
    Endif
    Select (loFormSet.lcTmpLine)
    Go Top
  Endif
  *B000109,1 WAM 03/05/2005 (End)

Endif
Select (loFormSet.lcTmpLine)
Set Filter To TranCd = '1'

*B999999,1 AMH Fix bug of PO lines duplicated [Start]
loFormSet.ariaform1.grdReceivingLines.Refresh
*B999999,1 AMH [End]

Return

*!*************************************************************
*! Name      : lfEvalRemQ
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To evaluate remove quantity to add it to another
*!             equavilent lines if found.
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
Function lfEvalRemQ
Local lnI, lcI
For lnI = 1 To 8
  lcI = Str(lnI,1)
  laRemoved[lnI] = laRemoved[lnI] + QTY&lcI
Endfor
*-- end of lfEvalRemQ.

*!*************************************************************
*! Name      : lfvRefer
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To validate the reference filed
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
Function lfvRefer
Lparameters loFormSet
Local lcKey, lnRecNo

If loFormSet.ariaform1.txtreference.Value = loFormSet.ariaform1.txtreference.OldValue
  Return
Endif
loFormSet.ariaform1.txtreference.ControlSource = ""
Select (loFormSet.lcTmpLine)
lcKey = cCarton+PO+Style+Dyelot+Padr(cWareCode,6)+Str(Lineno,6)

Set Filter To
lnRecNo = Recno()
Set Order To Tag TmpLine2
Seek lcKey

Replace Rest Reference With loFormSet.ariaform1.txtreference.Value ;
  WHILE cCarton+PO+Style+Dyelot+Padr(cWareCode,6)+Str(Lineno,6)= lcKey

Set Order To Tag TmpLine1
Set Filter To TranCd = '1'
If Between(lnRecNo,1,Reccount())
  Goto lnRecNo
Endif
loFormSet.ariaform1.txtreference.ControlSource = loFormSet.lcTmpLine + '.Reference'

Return

*!*************************************************************
*! Name      : lfvRate
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To validate the rate
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
Function lfvRate
Lparameters loFormSet, lnRateNo
Local lcKey, lnRecNo

If (lnRateNo = 1 And loFormSet.ariaform1.txtpriceRate.Value <=0 ) Or ;
    (lnRateNo = 2 And loFormSet.ariaform1.txtDutyRate.Value <=0 )
  *--Cannot accept zero or -ve rates.
  = gfModalGen('TRM34081B42000','DIALOG')
  If lnRateNo = 1
    loFormSet.ariaform1.txtpriceRate.Value = loFormSet.ariaform1.txtpriceRate.OldValue
  Else
    loFormSet.ariaform1.txtDutyRate.Value  = loFormSet.ariaform1.txtDutyRate.OldValue
  Endif
  Return 0
Endif

Select (loFormSet.lcTmpLine)
lcKey   = cCarton+PO+Style+Dyelot+Padr(cWareCode,6)+Str(Lineno,6)
lnRecNo = Recno()
Set Filter To

If lnRateNo  = 1 And loFormSet.ariaform1.txtpriceRate.Value <> loFormSet.ariaform1.txtpriceRate.OldValue
  Set Order To Tag TmpLine2
  Seek lcKey

  Scan Rest While cCarton+PO+Style+Dyelot+Padr(cWareCode,6)+Str(Lineno,6) = lcKey
    =lfGetEqv('1',loFormSet.ariaform1.txtpriceRate.Value,0,loFormSet.lnCurrUnt1,0,;
      nFLanCost1,0,0,0,0,0,0,loFormSet)
    Replace nLanPrRat  With loFormSet.ariaform1.txtpriceRate.Value,;
      nLan_Cost1 With loFormSet.laECost[1]
  Endscan
  Set Order To Tag TmpLine1
Endif

If lnRateNo  = 2 And loFormSet.ariaform1.txtDutyRate.Value <> loFormSet.ariaform1.txtDutyRate.OldValue
  Set Order To Tag TmpLine2
  Seek lcKey
  Scan Rest While cCarton+PO+Style+Dyelot+Padr(cWareCode,6)+Str(Lineno,6)=lcKey
    =lfGetEqv('234567',0,loFormSet.ariaform1.txtDutyRate.Value,0,loFormSet.lnCurrUnt2,0,;
      nFLanCost2,nFLanCost3,nFLanCost4,nFLanCost5,nFLanCost6,nFLanCost7,loFormSet)

    Replace nLanDuRat With loFormSet.ariaform1.txtDutyRate.Value
    Gather From loFormSet.laECost Fields nLan_Cost2,nLan_Cost3,nLan_Cost4,nLan_Cost5,;
      nLan_Cost6,nLan_Cost7
  Endscan
  Set Order To Tag TmpLine1
Endif
Select (loFormSet.lcTmpLine)
Set Filter To TranCd = '1'

If Between(lnRecNo,1,Reccount())
  Goto lnRecNo
Endif

Return

*!*************************************************************
*! Name      : lfGetEqv
*! Developer : Khalid Mohi El-Dine Mohamed
*! Date      : 09/11/2004
*! Purpose   : To get the equivelant costs
*!*************************************************************
*! Parameters: loFormSet : FormSet
*!*************************************************************
Function lfGetEqv
Lparameters lcUpdCsts,lnPRate1,lnDRate2,lnCurUnt1,lnCurUnt2,;
  lnFCost1,lnFCost2,lnFCost3,lnFCost4,lnFCost5,lnFCost6,lnFCost7,loFormSet
If Type('loFormSet') = 'O'
  Dimension loFormSet.laECost[LEN(lcUpdCsts)]
Else
  Dimension laECost[LEN(lcUpdCsts)]
Endif

lnPt = 1
If '1' $ lcUpdCsts
  If Type('loFormSet') = 'O'
    loFormSet.laECost[lnPt] = lfvEquCost('1',lnFCost1,lnPRate1,lnCurUnt1,loFormSet)
  Else
    laECost[lnPt] = lfvEquCost('1',lnFCost1,lnPRate1,lnCurUnt1,.F.,lcPriceCur,'')
  Endif
  lnPt = lnPt + 1
Endif
If '2' $ lcUpdCsts
  If Type('loFormSet') = 'O'
    loFormSet.laECost[lnPt] = lfvEquCost('2',lnFCost2,lnDRate2,lnCurUnt2,loFormSet)
  Else
    laECost[lnPt] = lfvEquCost('2',lnFCost2,lnDRate2,lnCurUnt2,.F.,'',lcDutyCur)
  Endif
  lnPt = lnPt + 1
Endif
If '3' $ lcUpdCsts
  If Type('loFormSet') = 'O'
    loFormSet.laECost[lnPt] = lfvEquCost('3',lnFCost3,lnDRate2,lnCurUnt2,loFormSet)
  Else
    laECost[lnPt] = lfvEquCost('3',lnFCost3,lnDRate2,lnCurUnt2,.F.,'',lcDutyCur)
  Endif

  lnPt = lnPt + 1
Endif
If '4' $ lcUpdCsts
  If Type('loFormSet') = 'O'
    loFormSet.laECost[lnPt] = lfvEquCost('4',lnFCost4,lnDRate2,lnCurUnt2,loFormSet)
  Else
    laECost[lnPt] = lfvEquCost('4',lnFCost4,lnDRate2,lnCurUnt2,.F.,'',lcDutyCur)
  Endif
  lnPt = lnPt + 1
Endif
If '5' $ lcUpdCsts
  If Type('loFormSet') = 'O'
    loFormSet.laECost[lnPt] = lfvEquCost('5',lnFCost5,lnDRate2,lnCurUnt2,loFormSet)
  Else
    laECost[lnPt] = lfvEquCost('5',lnFCost5,lnDRate2,lnCurUnt2,.F.,'',lcDutyCur)
  Endif
  lnPt = lnPt + 1
Endif
If '6' $ lcUpdCsts
  If Type('loFormSet') = 'O'
    loFormSet.laECost[lnPt] = lfvEquCost('6',lnFCost6,lnDRate2,lnCurUnt2,loFormSet)
  Else
    laECost[lnPt] = lfvEquCost('6',lnFCost6,lnDRate2,lnCurUnt2,.F.,'',lcDutyCur)
  Endif
  lnPt = lnPt + 1
Endif
If '7' $ lcUpdCsts
  If Type('loFormSet') = 'O'
    loFormSet.laECost[lnPt] = lfvEquCost('7',lnFCost7,lnDRate2,lnCurUnt2,loFormSet)
  Else
    laECost[lnPt] = lfvEquCost('7',lnFCost7,lnDRate2,lnCurUnt2,.F.,'',lcDutyCur)
  Endif
  lnPt = lnPt + 1
Endif

Return


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
Function lfvEquCost
Lparameters lcCstNo,lnFrnCost,lnCurRate,lnCurUnt,loFormSet,;
  lcPriceCur,lcDutyCur
Local lcCstType
lcCstType = Iif(Type('loFormSet') = 'O',loFormSet.lcIType&lcCstNo,lcIType&lcCstNo)
If lcCstType $ 'PMD'
  Store '' To lcPMethod,lcPUnMeth,lcDMethod,lcDUnMeth
  If lcCstType='P'
    lcPMethod = gfGetExSin(@lcPUnMeth,Iif(Type('loFormSet')='O',loFormSet.lcCur1,lcPriceCur))
    lcPMethod = Iif(Empty(lcPMethod),'*',lcPMethod)
    lcPUnMeth = Iif(Empty(lcPUnMeth),'/',lcPUnMeth)
    lnEquCost = lnFrnCost &lcPMethod lnCurRate &lcPUnMeth lnCurUnt
  Else
    lcDMethod = gfGetExSin(@lcDUnMeth,Iif(Type('loFormSet')='O',loFormSet.lcCur2,lcDutyCur))
    lcDMethod = Iif(Empty(lcDMethod),'*',lcDMethod)
    lcDUnMeth = Iif(Empty(lcDUnMeth),'/',lcDUnMeth)
    lnEquCost = lnFrnCost &lcDMethod lnCurRate &lcDUnMeth lnCurUnt
  Endif
Else
  lnEquCost = lnFrnCost
Endif
*: E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[Start]
*lnEquCost = ROUND(lnEquCost,2)
lnEquCost = Round(lnEquCost,3)
*: E302483,1 MMT 01/03/2008 Convert Shipment Cost sheet program to ARIA4[End]
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
Release lcCstType,lcPMethod,lcPUnMeth,lcDMethod,lcDUnMeth
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]

Return (lnEquCost)

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
Function lfWOrdBOM
Lparameters  lcSqlStat, llcTktBom, llBomLine, llBomCost, llMfgOprHd,loFormSet

*-- Getting the cTktBom for the selected PO
If llcTktBom
  =lfOpenSql(lcSqlStat,'CTKTBOM',loFormSet.lcCTktBom, "","",.F.)
Endif

*-- Getting the BomLine for the selected PO
If llBomLine
  =lfOpenSql(lcSqlStat,'BOMCOST',loFormSet.lcBomCost, "","",.F.)
Endif

*-- Getting the MfgOprHd for the selected PO
If llMfgOprHd
Endif

Return

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
Function lfGetBOM
Lparameters loFormSet, lcCstShtType, lcStyMajor, lGetBom
Local lcSqlStatement

lcSqlStatement  = "SELECT * FROM BOMHEADR (INDEX=BOMHEADR) " + ;
  " WHERE cInvType ='" + loFormSet.lcInvType + ;
  "' AND cItmMajor = '" + lcStyMajor + ;
  "' AND cCstShtTyp='"+ lcCstShtType + "'"

=lfOpenSql(lcSqlStatement,'BOMHEADR',loFormSet.lcBomHdr, "","",.F.)
Select (loFormSet.lcBomHdr)
Locate
Return !Eof()

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
Function lfOpenSql
Lparameters lcSqlStatment,lcTable,lcCursor,lcSqlIndex,lcWhereCond,llIsInitial

Local lnConnectionHandlar, lnBuffering

If Type('lcSqlIndex') <> 'C'
  lcSqlIndex=""
Endif

If Type("lcSqlStatment") <> 'C'
  lcSqlStatment = "SELECT * FROM " + lcTable + lcSqlIndex +" WHERE "+lcWhereCond
Endif

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,lcCursor,lcTable,;
  oAriaApplication.ActiveCompanyConStr,3,'SAVE',Set("DataSession"))

If lnConnectionHandlar = 1
  If llIsInitial
    loFormSet.DataEnvironment.InitialSelectedAlias = lcCursor
  Endif
Else
  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  Return .F.
Endif
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
Release lnConnectionHandlar, lnBuffering
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
Function lfOpenFox
Lparameters lcSqlStatment,lcTable,lcCursor,lcWhereCond
Local lnConnectionHandlar


If Type("lcSqlStatment") <> 'C'
  lcSqlStatment = "SELECT * FROM " + lcTable +" WHERE "+lcWhereCond
Endif

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,lcCursor,lcTable,;
  oAriaApplication.cAriaNativeDataFilesConStr,3,'SAVE',Set("DataSession"))

If lnConnectionHandlar <> 1
  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  Return .F.
Endif
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
Release lnConnectionHandlar
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]
*!*************************************************************
*! Name      : lfSetIndex
*! Developer : Khalid Mohi El-Din
*! Date      : 09/11/2004
*! Purpose   : function to Create index in table.
*!*************************************************************
Function lfSetIndex
Lparameters lcTable,laIndex

Local lnBuffering
lnBuffering = CursorGetProp("Buffering",lcTable)
=Tableupdate(.T.,.T.,lcTable)
=CursorSetProp("Buffering",3,lcTable)
Select (lcTable)
If !Empty(laIndex)
  For lnI = 1 To Alen(laIndex,1)
    lcIndex = laIndex[lnI,1]
    lcTag   = laIndex[lnI,2]
    *B607881,1 WAM 12/17/2006 Fix Error "File is in use" when two users open the screen
    *INDEX ON &lcIndex. TAG (lcTag) OF (lcTable)
    Index On &lcIndex. Tag (lcTag)
    *B607881,1 WAM 12/17/2006 (End)
  Endfor
  lcTag = laIndex[1,2]
  Set Order To Tag (lcTag)
Endif
=CursorSetProp("Buffering",lnBuffering,lcTable)
*--end of lfSetIndex.
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
Release lnBuffering,lnI ,lcIndex ,lcTag
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
Function lfBefSave


*! B125565,1 WSH 04/11/2005, Add Parameter to get FormSet Reference. [Start]
*LPARAMETERS lcRecvType, lcTmpLine, llFromEdi
Lparameters loFormSet, lcRecvType, lcTmpLine, llFromEdi
*! B125565,1 WSH 04/11/2005, [End]



*T20060818.0001,10/C200876 TMI 05/01/2008 [Start] check PO before save
If Ascan(loFormSet.laEvntTrig,Padr('DLCHKSAV',10),1,Alen(loFormSet.laEvntTrig,1),1) > 0 .And. ;
    !loFormSet.mDoTrigger(Padr('DLCHKSAV',10))
  Return .F.
Endif
*T20060818.0001,7 TMI [End  ]


*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [START]
*N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
*IF loFormset.lcInvType = '0002' AND lcRecvType $ 'PF' AND loFormSet.llTrkRolls
If loFormSet.lcInvType = '0002' And lcRecvType $ 'WPF' And loFormSet.llTrkRolls
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
  lenClrLen  = Len(gfItemMask("PN", "", loFormSet.lcInvType))
  Select(lcTmpLine)
  Scan For (TotStk > 0 And TranCd <> '4')
    *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
    *lcSeekKey = SUBSTR(Style,1,7)+RIGHT(Style,lenClrLen)
    lcSeekKey = Style
    *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
    =Seek(loFormSet.lcInvType+Style,loFormSet.lcTmpItem)
    If Evaluate(loFormSet.lcTmpItem+'.LTRKROLLS')
      llRoll = .T.
      If !Empty(loFormSet.lcTmpRoll) And Used(loFormSet.lcTmpRoll)
        Select(loFormSet.lcTmpRoll)
        lcTmpRTag = Order()
        Set Order To lcTmpRoll2
        If !Seek(lcSeekKey+&lcTmpLine..cWareCode+&lcTmpLine..Dyelot,loFormSet.lcTmpRoll) Or;
            (Seek(lcSeekKey+&lcTmpLine..cWareCode+&lcTmpLine..Dyelot,loFormSet.lcTmpRoll) And NAPPLY = 0)
          Set Order To &lcTmpRTag
          *N000682,1 MMT 12/09/2012 Globalization changes[Start]
          *=gfModalGen('TRM36096B36000','ALERT','rolls')
          =gfModalGen('TRM36096B36000','ALERT',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSGROLLS,loFormSet.GetHeaderText("LANG_POSTREC_MSGROLLS",loFormSet.HeaderAlias)))
          *N000682,1 MMT 12/09/2012 Globalization changes[end]
          Select(lcTmpLine)
          Return .F.

        Else
          lnTotApp = 0
          Select (loFormSet.lcTmpRoll)
          *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
          *SUM nApply REST WHILE ;
          cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6) = lcSeekKey+&lcTmpLine..cWarecode+&lcTmpLine..Dyelot;
          TO lnTotApp
          Sum NAPPLY Rest While ;
            STYLE+cWareCode+cDyelot+Str(Lineno,6) = lcSeekKey+&lcTmpLine..cWareCode+&lcTmpLine..Dyelot;
            TO lnTotApp
          *!* B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
          If lnTotApp <> &lcTmpLine..TotStk
            *N000682,1 MMT 12/09/2012 Globalization changes[Start]
            *=gfModalGen('TRM36096B36000','ALERT','rolls')
            =gfModalGen('TRM36096B36000','ALERT',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSGROLLS,loFormSet.GetHeaderText("LANG_POSTREC_MSGROLLS",loFormSet.HeaderAlias)))
            *N000682,1 MMT 12/09/2012 Globalization changes[end]
            Select(lcTmpLine)
            Return .F.
          Endif

        Endif
      Else
        *N000682,1 MMT 12/09/2012 Globalization changes[Start]
        *=gfModalGen('TRM36096B36000','ALERT','rolls')
        =gfModalGen('TRM36096B36000','ALERT',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSGROLLS,loFormSet.GetHeaderText("LANG_POSTREC_MSGROLLS",loFormSet.HeaderAlias)))
        *N000682,1 MMT 12/09/2012 Globalization changes[end]
        Select(lcTmpLine)
        Return .F.
      Endif
    Endif
  Endscan
Endif

llRoll  = .F.
If loFormSet.lcInvType = '0002' And (loFormSet.llTrkRolls Or loFormSet.lcCostMthM $ 'L') And lcRecvType $ 'G'
  Select(lcTmpLine)
  lenClrLen  = Len(gfItemMask("PN", "", loFormSet.lcInvType))
  Scan For (TotStk > 0 And TranCd <> '4')

    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
    *lcSeekKey = SUBSTR(Style,1,7)+RIGHT(Style,lenClrLen)
    lcSeekKey = Style
    *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]

    =Seek(loFormSet.lcInvType+Style,loFormSet.lcTmpItem)
    If Evaluate(loFormSet.lcTmpItem+'.LTRKROLLS')
      llRoll = .T.
      If !Empty(loFormSet.lcTmpRoll) And Used(loFormSet.lcTmpRoll)
        Select(loFormSet.lcTmpRoll)
        lcTmpRTag = Order()
        Set Order To lcTmpRoll2
        *-- IF  We can't find the roll record
        *-- OR  We found it
        *-- AND The user didn't apply any quantity
        *--         Stop the Saving process
        If !Seek(lcSeekKey+&lcTmpLine..cWareCode+&lcTmpLine..Dyelot,loFormSet.lcTmpRoll)
          Set Order To &lcTmpRTag
          *N000682,1 MMT 12/09/2012 Globalization changes[Start]
          *=gfModalGen('TRM36096B36000','ALERT','rolls')
          =gfModalGen('TRM36096B36000','ALERT',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSGROLLS,loFormSet.GetHeaderText("LANG_POSTREC_MSGROLLS",loFormSet.HeaderAlias)))
          *N000682,1 MMT 12/09/2012 Globalization changes[end]
          Select(lcTmpLine)
          Return .F.
        Else
          = Seek(lcSeekKey+&lcTmpLine..cWareCode+&lcTmpLine..Dyelot,loFormSet.lcTmpRoll)

          *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
          *!*	          LOCATE REST WHILE cfabric+ccolor+cwarecode+cdyelot+STR(lineno,6) ;
          *!*	                          = lcSeekKey + &lcTmpLine..cWarecode +  &lcTmpLine..Dyelot;
          *!*	                          FOR NAPPLY > 0
          Locate Rest While Style+cWareCode+cDyelot+Str(Lineno,6) ;
            = lcSeekKey + &lcTmpLine..cWareCode +  &lcTmpLine..Dyelot;
            FOR NAPPLY > 0
          *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
          If !Found()
            Set Order To &lcTmpRTag
            *N000682,1 MMT 12/09/2012 Globalization changes[Start]
            *=gfModalGen('TRM36096B36000','ALERT','rolls')
            =gfModalGen('TRM36096B36000','ALERT',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSGROLLS,loFormSet.GetHeaderText("LANG_POSTREC_MSGROLLS",loFormSet.HeaderAlias)))
            *N000682,1 MMT 12/09/2012 Globalization changes[eND]
            Select(lcTmpLine)
            Return .F.

          Else
            lnTotApp = 0
            Select (loFormSet.lcTmpRoll)
            *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [Start]
            *!*	            SUM nApply REST WHILE ;
            *!*	            cFabric+cColor+cWareCode+cDyelot+STR(LineNo,6) = lcSeekKey+&lcTmpLine..cWarecode+&lcTmpLine..Dyelot;
            *!*	            TO lnTotApp
            Sum NAPPLY Rest While ;
              STYLE+cWareCode+cDyelot+Str(Lineno,6) = lcSeekKey+&lcTmpLine..cWareCode+&lcTmpLine..Dyelot;
              TO lnTotApp
            *! B608792,1 MMT 01/27/2009 Fix bug of wrong Fabric code saved in rolls table [End]
            If lnTotApp <> &lcTmpLine..TotStk
              *N000682,1 MMT 12/09/2012 Globalization changes[Start]
              *=gfModalGen('TRM36096B36000','ALERT','rolls')
              =gfModalGen('TRM36096B36000','ALERT',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSGROLLS,loFormSet.GetHeaderText("LANG_POSTREC_MSGROLLS",loFormSet.HeaderAlias)))
              *N000682,1 MMT 12/09/2012 Globalization changes[end]
              Select(lcTmpLine)
              Return .F.
            Else
              llRoll  = .T.
            Endif


          Endif
        Endif
      Else
        *N000682,1 MMT 12/09/2012 Globalization changes[Start]
        *=gfModalGen('TRM36096B36000','ALERT','rolls')
        =gfModalGen('TRM36096B36000','ALERT',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSGROLLS,loFormSet.GetHeaderText("LANG_POSTREC_MSGROLLS",loFormSet.HeaderAlias)))
        *N000682,1 MMT 12/09/2012 Globalization changes[end]
        Select(lcTmpLine)
        Return .F.
      Endif
    Else
      If loFormSet.lcCostMthM $ 'L'
        If !Empty(loFormSet.lcTmpJour) And Used(loFormSet.lcTmpJour)
          If !Seek(loFormSet.lcInvType+&lcTmpLine..Style+&lcTmpLine..cWareCode+&lcTmpLine..Dyelot,loFormSet.lcTmpJour)
            *N000682,1 MMT 12/09/2012 Globalization changes[Start]
            *=gfModalGen('TRM36096B36000','ALERT','lots')
            =gfModalGen('TRM36096B36000','ALERT',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSGLOTS,loFormSet.GetHeaderText("LANG_POSTREC_MSGLOTS",loFormSet.HeaderAlias)))
            *N000682,1 MMT 12/09/2012 Globalization changes[end]
            Select(lcTmpLine)
            Return .F.
          Endif
          Select(loFormSet.lcTmpJour)
          Locate Rest While cInvType+Style+cWareCode+cDyelot+cRSession+cISession = ;
            loFormSet.lcInvType+&lcTmpLine..Style+&lcTmpLine..cWareCode+&lcTmpLine..Dyelot For NAPPLY > 0
          If !Found()
            *N000682,1 MMT 12/09/2012 Globalization changes[Start]
            *=gfModalGen('TRM36096B36000','ALERT','rolls')
            =gfModalGen('TRM36096B36000','ALERT',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSGLOTS,loFormSet.GetHeaderText("LANG_POSTREC_MSGLOTS",loFormSet.HeaderAlias)))
            *N000682,1 MMT 12/09/2012 Globalization changes[end]
            Select(lcTmpLine)
            Return .F.
          Else
            lnTotApp = 0
            Select(loFormSet.lcTmpJour)
            Sum NAPPLY Rest While  ;
              cInvType+Style+cWareCode+cDyelot+cRSession+cISession = ;
              loFormSet.lcInvType+&lcTmpLine..Style+&lcTmpLine..cWareCode+&lcTmpLine..Dyelot ;
              TO lnTotApp

            If lnTotApp <> &lcTmpLine..TotStk
              *N000682,1 MMT 12/09/2012 Globalization changes[Start]
              *=gfModalGen('TRM36096B36000','ALERT','lots')
              =gfModalGen('TRM36096B36000','ALERT',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSGLOTS,loFormSet.GetHeaderText("LANG_POSTREC_MSGLOTS",loFormSet.HeaderAlias)))
              *N000682,1 MMT 12/09/2012 Globalization changes[end]
              Select(lcTmpLine)
              Return .F.
            Else
              llRoll  = .T.
            Endif
          Endif
        Else
          *N000682,1 MMT 12/09/2012 Globalization changes[Start]
          *=gfModalGen('TRM36096B36000','ALERT','lots')
          =gfModalGen('TRM36096B36000','ALERT',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSGLOTS,loFormSet.GetHeaderText("LANG_POSTREC_MSGLOTS",loFormSet.HeaderAlias)))
          *N000682,1 MMT 12/09/2012 Globalization changes[end]
          Select(lcTmpLine)
          Return .F.
        Endif
      Else
        llRoll = .T.
      Endif
    Endif
  Endscan
  If !llRoll
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *=gfModalGen('TRM36096B36000','ALERT',IIF(EVALUATE(loFormSet.lcTmpItem+'.LTRKROLLS'),'rolls','lots'))
    =gfModalGen('TRM36096B36000','ALERT',Iif(Evaluate(loFormSet.lcTmpItem+'.LTRKROLLS'),;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSGROLLS,loFormSet.GetHeaderText("LANG_POSTREC_MSGROLLS",loFormSet.HeaderAlias)),;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSGLOTS,loFormSet.GetHeaderText("LANG_POSTREC_MSGLOTS",loFormSet.HeaderAlias))))
    *N000682,1 MMT 12/09/2012 Globalization changes[end]
    Select(lcTmpLine)
    Return .F.
  Endif
Endif
*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [End]


Do Case
  *-- 'R' Issue Return P/o, 'M' Receive C/T, 'T' Receive C/T Batch
  *-- 'D' Receive Dye Order, 'A' Issue Adornment order, 'E' Receive Adornment order
  *-- 'G' Issue Material PO
Case lcRecvType $ 'RMTDAEG'
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  *lnSel = gfModalGen('QRM34076B42010','DIALOG')
  *! B610575,1 MMT 11/04/2013 add option to Print Cut tkt rec. log while receiving Cut.tkt[T20131030.0018][Start]
  If lcRecvType ='M'
    lnSel = 0
    Do While lnSel < 2
      lnSel = gfModalGen('QRM34076B38037','DIALOG')
      If lnSel = 1
        loFormSet.ariaform1.LockScreen = .T.
        loFormSet.ariaform1.grdReceivingLines.RecordSource = ""
        Local lnCurAlias, lnNo, laFlds[1], lcCurrGrp, lcSqlStatement
        lnCurAlias  = Select(0)
        lcRPTitle   = ''
        llMultiWh   = loFormSet.llWareHous && not found
        llRPCostDt  = .F.
        llRPFrnCur  = .F.
        lcRpCurr    = 'F'
        lcRpSortBy  = 'P'
        lcInvType   = '0001'
        lcRPPoType  = 'U'
        lcBusDocu   = 'P'
        lcRPPrice   = 'C'
        lcRpExp     = '.T.'
        lcMajTtl    = gfItemMask("HM", '', loFormSet.lcInvType)
        lcOGWinTitl = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_CTRECLOG,loFormSet.GetHeaderText("LANG_POSTREC_CTRECLOG",loFormSet.HeaderAlias))
        llMultCurr  = loFormSet.llMulCurr
        Private lcBomLine, loBomLine, loPosHdr, lcRepTemp, lcMastPoHd
        lcBomLine  = gfTempName()
        lcRepCurs  = gfTempName()
        lcMastPoHd = gfTempName()
        lcSQLConts = gfTempName()
        loBomLine = Createobject('RemoteTable', 'BOMLINE', 'BOMLINE', lcBomLine, Set("Datasession"))
        loPosHdr  = Createobject('RemoteTable', 'POSHDR', 'POSHDR', lcMastPoHd, Set("Datasession"))
        lnTmpPos = Recno(loFormSet.lcTmpLine)
        lcTmpOrd = Order(loFormSet.lcTmpLine)
        = Afields(laFlds, loFormSet.lcTmpLine)
        =gfCrtTmp(lcRepCurs, @laFlds, 'cstytype+po+style+STR(lineno,6)+TranCd', 'POSLN')
        Select (lcRepCurs)
        Append From (oAriaApplication.WorkDir + loFormSet.lcTmpLine)
        Locate
        loBomLine = .Null.
        loPosHdr  = .Null.
        If Used(lcMastPoHd)
          Use In (lcMastPoHd)
        Endif
        Use In (lcRepCurs)
        *-- Run Report
        Declare laWareCodes[1]
        Store "" To laWareCodes
        For lnNo = 1 To Alen(laWareCodes,1)
          gcContCode = oAriaApplication.DefaultCountry
          If oAriaApplication.oActivelang.cLang_ID <> "EN"
            AHEADERFILE = oAriaApplication.GetClassHeaderFile(Upper(Alltrim(oAriaApplication.LangPath))+'\REPORTS\ITEMRECV_H.XML')
          Endif
          If File(oAriaApplication.clientreporthome+ 'ItemRecv.fxp')
            Do (oAriaApplication.clientreporthome+ 'ItemRecv') With lcRepCurs, loFormSet.ariaform1.cntShipment.kbShipNo.keytextbox.Value, (lnSel = 2),loFormSet.ariaform1.dtpickerReceivingDate.Value
          Else
            Do (oAriaApplication.ReportHome + 'ItemRecv') With lcRepCurs, loFormSet.ariaform1.cntShipment.kbShipNo.keytextbox.Value, (lnSel = 2),loFormSet.ariaform1.dtpickerReceivingDate.Value
          Endif
        Endfor
        Select (loFormSet.lcTmpLine)
        If lnTmpPos <> 0 And lnTmpPos <= Reccount()
          Goto lnTmpPos
        Endif
        Set Order To &lcTmpOrd
        Select (lnCurAlias)
        loFormSet.ariaform1.grdReceivingLines.RecordSource = loFormSet.lcTmpLine
        loFormSet.ariaform1.grdReceivingLines.ActivateCell(1,1)
        =lfActBrow(loFormSet)
        loFormSet.ariaform1.LockScreen = .F.
      Endif
    Enddo
    If lnSel = 3 Or lnSel = 2
      loFormSet.Saveorpost = Iif(lnSel =2,'P','S')
    Else
      loFormSet.Saveorpost = ''
    Endif
    lnSel = Iif(lnSel = 4,2,1)
  Else
    *! B610575,1 MMT 11/04/2013 add option to Print Cut tkt rec. log while receiving Cut.tkt[T20131030.0018][END]
    lnSel = Iif(lcRecvType ='M' ,gfModalGen('QRM34076B38037','DIALOG'),gfModalGen('QRM34076B42010','DIALOG'))
    If lcRecvType ='M'
      If lnSel < 3
        loFormSet.Saveorpost = Iif(lnSel =1,'P','S')
      Else
        loFormSet.Saveorpost = ''
      Endif
      lnSel = Iif(lnSel = 3,2,1)
    Else
      loFormSet.Saveorpost = ''
    Endif
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
    *! B610575,1 MMT 11/04/2013 add option to Print Cut tkt rec. log while receiving Cut.tkt[T20131030.0018][Start]
  Endif
  *! B610575,1 MMT 11/04/2013 add option to Print Cut tkt rec. log while receiving Cut.tkt[T20131030.0018][End]
  If lnSel = 2
    Return .F.
  Else
    lnSel=3
  Endif
  *-- 'I' Receive P/O, 'S' Receive by Shipment, 'B' Receive P/O Batch
  *-- 'N' Issue Inter-Location P/O, 'O' Receive Inter-Location P/o
  *-- 'L' Receive Inter Location P/O Batch, 'H' Issue Inter Location P/O Batch
  *-- 'U' Issue Inter-Location P/O Shipment
  *-- 'P' Receive Material PO, 'F' Receive Material PO Shipment.
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
  *CASE lcRecvType $ 'ISBNOLHUPF'
Case lcRecvType $ 'ISBNOLHUPFW'
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][End]
  *--Print reciepts log.
  *--Do you wish to print/view [reciepts/issues] log before posting all transactions?,
  *-- 'Print Log,Post,Cancel'
  *-- lnSel=1 View log
  *-- lnSel=2 print log
  *-- lnSel=3 post
  *-- lnSel=4 cancel
  lnSel = Iif(llFromEdi,3,0)
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  *DO WHILE lnSel < 3 &&-- after view/print do the same screen for print/view
  &&-- or post or cancel .
  *lnSel = gfModalGen('QRM34075B42009','DIALOG',IIF(lcRecvType $ 'NAHU','issues','receipts'))
  *IF lnSel=1 OR lnSel=2 &&-- if preview/print only
  Do While Iif(lcRecvType $ 'IO',lnSel < 2,lnSel < 3) &&-- after view/print do the same screen for print/view
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
    *lnSel = gfModalGen(IIF(lcRecvType $ 'IO','QRM34075B34219','QRM34075B42009'),'DIALOG',IIF(lcRecvType $ 'NAHU','issues','receipts'))
    If !loFormSet.llSilentMod
      *N000682,1 MMT 12/09/2012 Globalization changes[Start]
      *lnSel = gfModalGen(IIF(lcRecvType $ 'IO','QRM34075B34219','QRM34075B42009'),'DIALOG',IIF(lcRecvType $ 'NAHU','issues','receipts'))
      *! E303848,1 SARA.O 07/09/2017 Saving a ‘Receive by Shipment’ session will allow the user to save the receiving as a batch [Begin]

      *lnSel = gfModalGen(IIF(lcRecvType $ 'IO','QRM34075B34219','QRM34075B42009'),'DIALOG',IIF(lcRecvType $ 'NAHU',;
      *IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSGISSUES,loFormSet.GetHeaderText("LANG_POSTREC_MSGISSUES",loFormSet.HeaderAlias)),;
      *IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSGRECEIPTS,loFormSet.GetHeaderText("LANG_POSTREC_MSGRECEIPTS",loFormSet.HeaderAlias))))


      lnSel = gfModalGen(Iif(lcRecvType $ 'IOS','QRM34075B34219','QRM34075B42009'),'DIALOG',Iif(lcRecvType $ 'NAHU',;
        IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSGISSUES,loFormSet.GetHeaderText("LANG_POSTREC_MSGISSUES",loFormSet.HeaderAlias)),;
        IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSGRECEIPTS,loFormSet.GetHeaderText("LANG_POSTREC_MSGRECEIPTS",loFormSet.HeaderAlias))))


      *! E303848,1 SARA.O 07/09/2017 Saving a ‘Receive by Shipment’ session will allow the user to save the receiving as a batch [End]

      *N000682,1 MMT 12/09/2012 Globalization changes[end]
    Else
      lnSel = 3
      loFormSet.Visible = .F.
    Endif
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

    If lcRecvType $ 'IO'

      loFormSet.Saveorpost = Iif(lnSel= 1 Or lnSel= 4,'',Iif(lnSel = 2,'P','S'))
      *E303627,1 MMT 11/29/2015 Modify the Inter-Location PO receiving to work in silent mode[T20151014.0017][Start]
      If loFormSet.llSilentMod And lcRecvType ='O'
        loFormSet.Saveorpost = 'P'
      Endif
      *E303627,1 MMT 11/29/2015 Modify the Inter-Location PO receiving to work in silent mode[T20151014.0017][End]
    Else
      loFormSet.Saveorpost = ''
      *! E303848,1 SARA.O 07/09/2017 Saving a ‘Receive by Shipment’ session will allow the user to save the receiving as a batch [Begin]
      If lcRecvType $ 'S' And lnSel = 3
        loFormSet.Saveorpost = 'S'
      Endif
      If lcRecvType $ 'S' And lnSel = 2
        lnSel = 3
        loFormSet.SaveorpostBatch = 'P'
      Endif

      *! E303848,1 SARA.O 07/09/2017 Saving a ‘Receive by Shipment’ session will allow the user to save the receiving as a batch [End]
    Endif
    If lnSel=1 Or Iif(lcRecvType $ 'IO',.F.,lnSel=2) &&-- if preview/print only
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
      loFormSet.ariaform1.LockScreen = .T.
      loFormSet.ariaform1.grdReceivingLines.RecordSource = ""

      Local lnCurAlias, lnNo, laFlds[1], lcCurrGrp, lcSqlStatement

      lnCurAlias  = Select(0)
      lcRPTitle   = ''
      llMultiWh   = loFormSet.llWareHous && not found
      llRPCostDt  = .F.
      llRPFrnCur  = .F.
      lcRpCurr    = 'F'
      lcRpSortBy  = Iif(loFormSet.lcPType = 'S', 'T', 'P')

      *-- If this program is called by parameter 'N' Issue Inter-Location P/O
      *-- or 'O' Receive Inter-Location P/o
      *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
      *!*	      lcInvType   = IIF(loFormSet.lcPType $ 'PF', '0002', '0001')
      *!*	      lcRPPoType  = IIF(loFormSet.lcPType $ "ON", "N", "A")
      *!*	      lcBusDocu   = IIF(loFormSet.lcPType $ "ON", "N", IIF(loFormSet.lcPType $ 'RG', 'R', 'P'))
      lcInvType   = Iif(loFormSet.lcPType $ 'PFW', '0002', '0001')
      lcRPPoType  = Iif(loFormSet.lcPType $ "ON", "N", Iif(loFormSet.lcPType = 'W',"F","A"))
      *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][End]
      lcBusDocu   = Iif(loFormSet.lcPType $ "ON", "N", Iif(loFormSet.lcPType $ 'RG', 'R', 'P'))
      lcRPPrice   = 'C'
      lcRpExp     = '.T.'
      lcMajTtl    = gfItemMask("HM", '', loFormSet.lcInvType)

      Do Case
      Case loFormSet.lcPType $ 'ISNUHOCL'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *lcOGWinTitl = LANG_POSTREC_POSTYREC
        lcOGWinTitl = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_POSTYREC,loFormSet.GetHeaderText("LANG_POSTREC_POSTYREC",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]

      Case loFormSet.lcPType $ 'D'
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *lcOGWinTitl = LANG_POSTREC_DYEORD
        lcOGWinTitl = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_DYEORD,loFormSet.GetHeaderText("LANG_POSTREC_DYEORD",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]

      Otherwise
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *lcOGWinTitl = LANG_POSTREC_MAPOREC
        lcOGWinTitl = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MAPOREC,loFormSet.GetHeaderText("LANG_POSTREC_MAPOREC",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]

      Endcase

      llMultCurr  = loFormSet.llMulCurr

      Private lcBomLine, loBomLine, loPosHdr, lcRepTemp, lcMastPoHd
      lcBomLine  = gfTempName()
      lcRepCurs  = gfTempName()
      lcMastPoHd = gfTempName()

      loBomLine = Createobject('RemoteTable', 'BOMLINE', 'BOMLINE', lcBomLine, Set("Datasession"))
      loPosHdr  = Createobject('RemoteTable', 'POSHDR', 'POSHDR', lcMastPoHd, Set("Datasession"))

      lnTmpPos = Recno(loFormSet.lcTmpLine)
      lcTmpOrd = Order(loFormSet.lcTmpLine)

      = Afields(laFlds, loFormSet.lcTmpLine)

      =gfCrtTmp(lcRepCurs, @laFlds, 'cstytype+po+style+STR(lineno,6)+TranCd', 'POSLN')
      Select (lcRepCurs)
      Append From (oAriaApplication.WorkDir + loFormSet.lcTmpLine)
      Locate

      lcBomPTyp = Iif(loFormSet.lcPType = 'S', 'S', 'I')

      If (loFormSet.lcPType = 'I' Or loFormSet.lcPType = 'S' Or loFormSet.lcPType = 'B' Or loFormSet.lcPType = 'O' Or loFormSet.lcPType = 'L')
        Select (lcRepCurs)
        lcCurrGrp = cBusDocu + cStyType + cStyType
        =loPosHdr.Seek(cBusDocu+cStyType+PO)
        Scan For TranCd $ '42' And TotQty <> 0;
            .And. lfChekAdj(lcBomPTyp, PO + Str(Lineno,6), Style, Iif(loFormSet.lcPType = 'S', ShipNo, ''), '', cStyGrade)

          *B126833,1 WAM 04/03/2005 add new button to edit landed cost for styles has no detail costing
          *IF loFormSet.llImpCost OR !loFormSet.llEditLCst
          If lDetCost Or !loFormSet.llEditLCst
            *B126833,1 WAM 04/03/2005 (End)
            If Empty(lcCurrGrp) Or lcCurrGrp <> cBusDocu + cStyType + cStyType
              =loPosHdr.Seek(cBusDocu+cStyType+PO)
              lcCurrGrp = cBusDocu + cStyType + cStyType
            Endif
            =lfPrnLanded(loFormSet)
          Endif

        Endscan
      Endif

      loBomLine = .Null.
      loPosHdr  = .Null.
      If Used(lcMastPoHd)
        Use In (lcMastPoHd)
      Endif
      Use In (lcRepCurs)

      *-- Run Report
      Declare laWareCodes[1]
      Store "" To laWareCodes

      For lnNo = 1 To Alen(laWareCodes,1)
        If loFormSet.lcPType = "O" And lnSel = 2
          lcRpExp   = "cWareCode = '"+ laWareCodes[lnNo] + "'"
          lcRPTitle = "Issue Inter Location To " + laWareCodes[lnNo]
        Endif
        *MEDIA
        gcContCode = oAriaApplication.DefaultCountry
        *MEDIA
        *N000682,1 MMT 04/19/2013 Fix Media issues[Start]
        If oAriaApplication.oActivelang.cLang_ID <> "EN"
          AHEADERFILE = oAriaApplication.GetClassHeaderFile(Upper(Alltrim(oAriaApplication.LangPath))+'\REPORTS\ITEMRECV_H.XML')
        Endif
        *N000682,1 MMT 04/19/2013 Fix Media issues[End]
        *B608042,1 MMT 04/15/2007 fix bug of wrong dates while calling reports from receiving screen[Start]
        *DO (oAriaApplication.ReportHome + 'ItemRecv') WITH lcRepCurs, loFormSet.AriaForm1.cntShipment.kbShipNo.keytextbox.Value, (lnSel = 2)
        *! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[Start]
        If File(oAriaApplication.clientreporthome+ 'ItemRecv.fxp')
          Do (oAriaApplication.clientreporthome+ 'ItemRecv') With lcRepCurs, loFormSet.ariaform1.cntShipment.kbShipNo.keytextbox.Value, (lnSel = 2),loFormSet.ariaform1.dtpickerReceivingDate.Value
        Else
          *! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[End]
          Do (oAriaApplication.ReportHome + 'ItemRecv') With lcRepCurs, loFormSet.ariaform1.cntShipment.kbShipNo.keytextbox.Value, (lnSel = 2),loFormSet.ariaform1.dtpickerReceivingDate.Value
          *! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[Start]
        Endif
        *! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[End]
        *B608042,1 MMT 04/15/2007 fix bug of wrong dates while calling reports from receiving screen[End]
      Endfor

      Select (loFormSet.lcTmpLine)
      If lnTmpPos <> 0 And lnTmpPos <= Reccount()
        Goto lnTmpPos
      Endif
      Set Order To &lcTmpOrd

      Select (lnCurAlias)
      loFormSet.ariaform1.grdReceivingLines.RecordSource = loFormSet.lcTmpLine
      loFormSet.ariaform1.grdReceivingLines.ActivateCell(1,1)
      =lfActBrow(loFormSet)
      loFormSet.ariaform1.LockScreen = .F.
      *! B125565,1 WSH 04/11/2005, [End]

    Endif  &&-- Endif lnSel = 1 OR lnSel = 2
  Enddo

  If lnSel = 4  &&-- <Canel>
    Return .F.
  Endif
Endcase
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
*If loFormSet.lcPType $ 'IOM' And !Empty(loFormSet.ariaform1.cntBatch.kbBatchNo.keytextbox.Value) And loFormSet.Saveorpost = 'P'
*! B611536,1 SAH 02/25/2018 Modify function lfBefSave as the user receive un-Approved batch while receiving by Shipment in PO receiving screen [start]
If (loFormSet.lcPType $ 'IOM' And !Empty(loFormSet.ariaform1.cntBatch.kbBatchNo.keytextbox.Value) And loFormSet.Saveorpost = 'P')OR(loFormSet.lcPType = 'S' And !Empty(loFormSet.ariaform1.cntBatch.kbBatchNo.keytextbox.Value) And loFormSet.SaveorpostBatch = 'P')
*! B611536,1 SAH 02/25/2018 Modify function lfBefSave as the user receive un-Approved batch while receiving by Shipment in PO receiving screen [end]
  If !lfCheckBatch(loFormSet)
    Return .F.
  Endif
Endif
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
Function lfSaveRec

*B128070,1 KHM 05/19/2005 Add 3 new paramters to hold stock, damage and cancelled quantity
*B128070,1 KHM 05/19/2005 in case of receive by shipment [Begin]
*PARAMETERS lcInvType, lcRecvType, lcTmpLine, ldTrDate, ldRecDate, lcTempLoc,;
lcMastShp, llBarCode, llFromEdi, llEditLCst

*: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
*PARAMETERS lcInvType, lcRecvType, lcTmpLine, ldTrDate, ldRecDate, lcTempLoc,;
lcMastShp, llBarCode, llFromEdi, llEditLCst, lnShipStk, lnShipCan, lnShipDam

Parameters lcInvType, lcRecvType, lcTmpLine, ldTrDate, ldRecDate, lcTempLoc,;
  lcMastShp, llBarCode, llFromEdi, llEditLCst, lnShipStk, lnShipCan, lnShipDam,llAddBomLine
*: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]
*B128070,1 KHM 05/19/2005 [End]

*B611953,1 ES 10/29/2019 Shipment is not marked as locked once user selected it in PO receiving screen [Start]
IF loFormSet.lcPType $ 'SUFC'
  IF !USED('SHPMTHDR_CH')
    =gfOpenTabLe('SHPMTHDR','SHPMTHDR','SH','SHPMTHDR_CH')
  ENDIF &&CBUSDOCU+CSHPTYPE+SHIPNO                                                                                                
  =gfSeek(loFormSet.lcBusDoc + loFormSet.lcWorkOrd+loFormSet.ariaform1.cntShipment.kbShipNo.keytextbox.Value,'SHPMTHDR_CH')
  IF (!EMPTY(&lcMastShp..dedit_date) OR !EMPTY(SHPMTHDR_CH.dedit_date)) OR ;
     (!EMPTY(&lcMastShp..cedit_time) OR !EMPTY(SHPMTHDR_CH.cedit_time)) 
    IF (SHPMTHDR_CH.dedit_date > &lcMastShp..dedit_date) OR (CTOT(SHPMTHDR_CH.cedit_time) > CTOT(&lcMastShp..cedit_time))
      =gfCloseTable('SHPMTHDR_CH')
      =gfModalGen('TRM54056B00000','DIALOG')
      RETURN .f. 
    ENDIF  
  ENDIF         
  =gfCloseTable('SHPMTHDR_CH')
ENDIF
*B611953,1 ES 10/29/2019 Shipment is not marked as locked once user selected it in PO receiving screen [End]



*-- Check if no receiving or issuing has been done.
Select (lcTmpLine)
lnCrRec = Iif(Eof(),0,Recno())
Locate For TranCd <> '1'
If !Found()
  *--No [Receiving Lines/Issuing lines] was done,Cannot update.
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *= gfModalGen('INM34061B42000','DIALOG',IIF(lcRecvType $ 'RNAHUG', 'issuing Line','receiving Line'))
  = gfModalGen('INM34061B42000','DIALOG',Iif(lcRecvType $ 'RNAHUG',   Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_ISSUING,loFormSet.GetHeaderText("LANG_POSTREC_MSG_ISSUING",loFormSet.HeaderAlias))+' '+;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BROWLINE,loFormSet.GetHeaderText("LANG_POSTREC_BROWLINE",loFormSet.HeaderAlias)),;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_RECEVING,loFormSet.GetHeaderText("LANG_POSTREC_MSG_RECEVING",loFormSet.HeaderAlias))+' '+;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BROWLINE,loFormSet.GetHeaderText("LANG_POSTREC_BROWLINE",loFormSet.HeaderAlias))))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  If Between(lnCrRec,1,Reccount())
    Goto lnCrRec
  Endif
  Return .F.
Endif

*-- 'B' Receive P/O Batch 'L' Receive Inter Location P/O Batch'
If lcRecvType $ 'BL'
  Locate For nLanPrRat = 0 Or nLanDuRat = 0
  If Found()
    *--You cannot receive batch with zero rates.
    = gfModalGen('TRM34082B42000','DIALOG')
    If Between(lnCrRec,1,Reccount())
      Goto lnCrRec
    Endif
    Return .F.
  Endif
Endif


*-- Get the necessary settings
Private laSetups  , llWareHous, llWareLoc, llDyelot, llFabDye, lcCostMthM, lcCostMth,;
  llLinkToGl, lcDropLoc , llPOSale  , llImpCost, llMulCurr, llUseMCurr,;
  llEditExRt, llConfig  , llMFCall, llUnblProc, llShpRec

Dimension laSetups[21,2]
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
llMulCurr  = Iif(lcRecvType $ 'DAE',.F.,laSetups[10,2])
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

If Inlist(lcRecvType,'O','L') And !llFromEdi And 'NC' $ oAriaApplication.CompanyInstalledModules
  =gfOpenFile(oAriaApplication.DataDir+'EDIACPRT',oAriaApplication.DataDir+'ACCFACT','SH')
  =gfOpenFile(oAriaApplication.DataDir+'EDIPD',oAriaApplication.DataDir+'PARTTRANS','SH')
  =gfOpenFile(oAriaApplication.DataDir+'EDITRANS',oAriaApplication.DataDir+'TYPEKEY','SH')
Endif

*--Get the fiscal year and fiscal period.
Private lcGLFYear, lcGLPeriod
Store '' To lcGLFYear, lcGLPeriod
If llLinkToGl
  =CHECKPRD(ldTrDate,'lcGLFYear','lcGLPeriod','PO',.T.)
  *B608773,1 WAM 12/24/2008 Open GL_LINK account to validate cost of goods variance account
  =gfOpenTable(oAriaApplication.DataDir+'GL_LINK','GL_LINK','SH')
  *B608773,1 WAM 12/24/2008 (End)
Endif
*B608845,1 WAM 04/09/2009 Use the scale file
If Used('SCALE')
  Use In Scale
Endif
=gfOpenFile(oAriaApplication.DataDir+'SCALE',oAriaApplication.DataDir+'SCALE','SH')
*B608845,1 WAM 04/09/2009 (End)

*B607935,1 WAM 01/17/2007 Initialize variable lcTmpItm
*PRIVATE lcVendFile, lcMastPoHd, lcMastPoLn, lcPosLn, lcMastBomLn, lcTmpBomLn, lcTempItem, lcItemLoc,;
lcTmpCur, lcGlDist, lcGlSession,  lcMastOprDt, lcTmpCode, lcTmpItmJrl, lcTmpItmDye
Private lcVendFile, lcMastPoHd, lcMastPoLn, lcPosLn, lcMastBomLn, lcTmpBomLn, lcTempItem, lcItemLoc,;
  lcTmpCur, lcGlDist, lcGlSession,  lcMastOprDt, lcTmpCode, lcTmpItmJrl, lcTmpItmDye, lcTmpItm
*B607935,1 WAM 01/17/2007 (End)


*-- lcVendFile to hold the vendor file (FOX)
lcVendFile = gfTempName()
*-- lcMastPoHd to hold the POSHDR file (SQL)
lcMastPoHd  = gfTempName()
*-- lcMastPoLn to hold the POSLN file (SQL)
lcMastPoLn  = gfTempName()
*-- lcPosLn to hold the recieving lins then passed to be updated (SQL)
lcPosLn     = gfTempName()
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
If llDyelot And llMFCall
  *--Check new lines dyelots case of batch.
  Select (lcTmpLine)
  Scan For !Empty(Dyelot)
    If TranCd = '1'
      Replace lNewLn With .T.
    Else
      If lcRecvType = 'T' And !Seek(Cuttkt+Style+Dyelot,'CUTTKTL')
        Replace lNewLUpd With .T.
      Endif
    Endif
  Endscan

  llUnblProc = .F.

  = lfDyeOvrRcv()
  *--Unable to proceed due to
  *--The over received quantity is not allocated completely.
  If llUnblProc
    Select (lcTmpLine)
    Set Order To Tag TmpLine1
    Go Top
    Return .F.
  Endif
Endif


*! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
*WAIT WINDOW 'Posting all transactions ...' NOWAIT
If !loFormSet.llSilentMod
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *WAIT WINDOW 'Posting all transactions ...' NOWAIT
  Wait Window Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_POSTING,loFormSet.GetHeaderText("LANG_POSTREC_POSTING",loFormSet.HeaderAlias)) Nowait
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
Endif
*! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
lcGlSession = gfsequence('GLSESSION')

*-- Update lineNo with original lineNo on p/o in case of batch.
*-- To join the cartons lines when receive batch.
*-- 'B' Receive P/O Batch, 'T' Receive C/T Batch, 'L' Receive Inter Location P/O Batch
If (lcRecvType $ 'BTL') And llByCarton
  Select (lcTmpLine)
  Replace All Lineno With nLineNo
Endif

*-- Check it later
*-- If new line added to the P/O and update bom. Or
*-- 'I' Receive P/O, 'S' Receive by Shipment, 'R' Issue Return P/o 'D' Receive Dye Order
*!*	IF (lcRecvType $ 'ISRD')
*!*	  =lfAddNewLn()
*!*	ENDIF

*B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[Start]
If (lcRecvType $ 'ISRD')
  =lfAddNewLn()
Endif
*B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[End]


*--Issue Inter Location P/O Updates.
*-- 'N' Issue Inter-Location P/O, 'A' Issue Adornment order
*-- 'H' Issue Inter Location P/O Batch, 'U' Issue Inter Location P/O Shipment
If lcRecvType $ 'NAHU'
  *--Issue stock for Inter Location P/o Styles and create intransit records.
  =lfUpdIntCmp(lcMastShp)
  *-- Update Inter Location P/O Status [to be done later]
  If lcRecvType = 'H'
    If Seek('N'+lcBatch,'CTKTRCVH')
      Select CTKTRCVH
      Replace cStatus With 'I'
    Endif
  Endif
  Wait Clear
  Select (lcTmpLine)
  Set Order To Tag TmpLine1
  Delete All
  Return .T.
Endif

*-- Initialize the necessary variables to srart updating lines except
Private laOpnQty,lnLstPoRAm,lnOpnPoAmt,lcLstTrn,lcPoVend,lcOrjWareH,;
  lcLotNo, lcChkPO, lcBomPTyp, lcBomPKey,;
  laReceive, lcCstShtTyp, lcPriceCur,lcDutyCur,lnConvFact,laPrevRecQ
*B128318,1 KHM 06/01/2005 Add the initialization of laOldOpnQty to hold open qty before
*B128318,1                multiplying it by the conversion factor [Begin]
*DIMENSION laOpnQty[8], laReceive[8],laPrevRecQ[8]
Dimension laOpnQty[8], laReceive[8],laPrevRecQ[8], laOldOpnQty[8]
*B128318,1 KHM 06/01/2005 [End]

*B128318,1 KHM 06/01/2005 Add the initialization of laOldOpnQty to hold open qty before
*B128318,1                multiplying it by the conversion factor [Begin]
*STORE 0   TO lnLstPoRAm,lnOpnPoAmt,laOpnQty,laPrevRecQ
Store 0   To lnLstPoRAm,lnOpnPoAmt,laOpnQty,laPrevRecQ, laOldOpnQty
*B128318,1 KHM 06/01/2005 [End]

Store ' ' To lcLstTrn,lcPoVend,lcOrjWareH
Store ' ' To lcChkPO, lcCstShtTyp, lcShpcode
*-- To hold the lot No.
lcLotNo    = Space(2)

*B608773,1 WAM 12/24/2008 Use the MFCSTSC to get style standard cost in base currency for all costing elements
If llLinkToGl And lcCostMth = 'S' And loFormSet.llMulCurr And lcInvType = "0001"
  llMscale   = gfGetMemVar('M_USEEXSSC')
  lcTmpBom  = gfTempName()
  oCostSheet = Newobject('MFCSTSC',oAriaApplication.ClassDir+"WORKORDERS.VCX")
Endif
*B608773,1 WAM 12/24/2008 (End)

*-- Conversion Factor
lnConvFact = 1
*--Update lines for Cutting Ticket or Purchase Order.
Select (lcTmpLine)
Set Order To Tag TmpLine3
Locate
Scan For TranCd <> '1' And TotQty <> 0
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
  *WAIT WINDOW 'Posting all transactions ... '+IIF(lcRecvType $ 'PFG','Material:','Style :')+Style NOWAIT
  If !loFormSet.llSilentMod
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
    *WAIT WINDOW 'Posting all transactions ... '+IIF(lcRecvType $ 'PFG','Material:','Style :')+STYLE NOWAIT
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *WAIT WINDOW 'Posting all transactions ... '+IIF(lcRecvType $ 'WPFG','Material:','Style :')+STYLE NOWAIT
    Wait Window Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_POSTING,loFormSet.GetHeaderText("LANG_POSTREC_POSTING",loFormSet.HeaderAlias))+Iif(lcRecvType $ 'WPFG',;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_FABRIC,loFormSet.GetHeaderText("LANG_POSTREC_MSG_FABRIC",loFormSet.HeaderAlias))+' :',;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_STYLE,loFormSet.GetHeaderText("LANG_POSTREC_MSG_STYLE",loFormSet.HeaderAlias))+' :')+Style Nowait
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
  Endif
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

  *-- Get the conversion factor
  lnConvFact = 1
  If lcInvType = "0002"
    =gfGetUOMData(&lcTmpLine..cUOMCode, '', '', @lnConvFact, .F.)
  Endif
  If PO <> lcChkPO
    *-- Get the POSHDR record for the current receiving lines
    lcSqlStatement = "SELECT * FROM POSHDR [INDEX=POSHDR] "+;
      "WHERE cBusDocu = '" + &lcTmpLine..cBusDocu +;
      "' AND cStyType = '" + &lcTmpLine..cStyType +;
      "' AND PO = '" + &lcTmpLine..PO + "'"
    =lfGetItmInf(lcInvType,&lcTmpLine..cBusDocu+&lcTmpLine..cStyType+&lcTmpLine..PO,;
      lcMastPoHd,'POSHDR',lcSqlStatement,.T.)
    =Seek(&lcTmpLine..cBusDocu+&lcTmpLine..cStyType+&lcTmpLine..PO,lcMastPoHd)
    lcPriceCur = Evaluate(lcMastPoHd+'.cPriceCur')
    lcDutyCur  = Evaluate(lcMastPoHd+'.cDutyCur')

    *-- Get the master records from POSLN file
    *B128070,1 KHM 05/19/2005 In case of shipment get the shipment lines only once [Begin]
    *! B611276,1 MMT 03/26/2017 Po receving error while receiving by shipment[T20161212.0005][Start]
    *IF lcRecvType $ 'SFC' AND &lcTmpLine..ShipNo <> lcShpcode
    If lcRecvType $ 'SFC' And !(&lcTmpLine..ShipNo == lcShpcode)
      *! B611276,1 MMT 03/26/2017 Po receving error while receiving by shipment[T20161212.0005][End]
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
    Endif
    If !(lcRecvType $ 'SFC')
      *B128070,1 KHM 05/19/2005 [End]
      lcSqlStatement  = "SELECT * FROM POSLN [INDEX=POSLN] "+;
        "WHERE cBusDocu = '" + &lcTmpLine..cBusDocu + ;
        "' AND cStyType ='" + &lcTmpLine..cStyType  + ;
        "' AND PO ='" + &lcTmpLine..PO + "' AND cInvType ='" + lcInvType + "'"
      =lfOpenSql(lcSqlStatement,'POSLN',lcMastPoLn)
    Endif

    *! B610260,1 HES 02/27/2013 Fix bug of not fund table in the current area when the Dyelot not found for the used style location [Start]
    Local laIndex
    *! B610260,1 HES 02/27/2013 Fix bug of not fund table in the current area when the Dyelot not found for the used style location [End  ]

    Dimension laIndex[2,2]
    laIndex = ''
    laIndex[1,1] = 'CBUSDOCU+CSTYTYPE+PO+cInvType+STYLE+STR(LINENO,6)+TRANCD'
    laIndex[1,2] = lcMastPoLn
    laIndex[2,1] = 'SHIPNO+CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD'
    laIndex[2,2] = 'Poslnsh'
    =lfSetIndex(lcMastPoLn,@laIndex)
    Set Order To Tag lcMastPoLn In (lcMastPoLn)

    *-- If not receive by Material PO or Material Shipment
    If !(lcRecvType $ 'PFG')
      *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
      *!*	      lcCstShtTyp = IIF(&lcTmpLine..cStyType = "U","M",;
      *!*	        IIF(&lcTmpLine..cStyType = "P","I",&lcTmpLine..cStyType))
      lcCstShtTyp = Iif(&lcTmpLine..cStyType = "U","M",;
        IIF(&lcTmpLine..cStyType = "P","I",Iif(&lcTmpLine..cStyType = 'F','T',&lcTmpLine..cStyType)))
      *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
      *-- Get the information from the BomLine
      If lcRecvType = 'S' And !Empty(lcShpcode)

        *: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
        *lcSqlStatement = "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+;
        "WHERE cImTyp = '" + lcCstShtTyp +;
        "' AND cTktNo ='" + &lcTmpLine..PO + "' AND ShipNo='" + lcShpcode + "'"
        lcSqlStatement = "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+;
          "WHERE cImTyp = '" + lcCstShtTyp +;
          "' AND cTktNo ='" + &lcTmpLine..PO + "' AND ShipNo='" + lcShpcode + "' AND CRSESSION = ''"
        *: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]
      Else
        lcSqlStatement = "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+;
          "WHERE cImTyp = '" + lcCstShtTyp +;
          "' AND cTktNo ='" + &lcTmpLine..PO + "'"
      Endif
      =lfOpenSql(lcSqlStatement,'BOMLINE',lcMastBomLn, "","",.F.)
      If lcRecvType = 'S' And !Empty(lcShpcode)
        Select (lcMastBomLn)
        Locate
        If Eof()
          llShpRec = .F.
          lcSqlStatement = "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+;
            "WHERE cImTyp = '" + lcCstShtTyp +;
            "' AND cTktNo ='" + &lcTmpLine..PO + "'"
          =lfOpenSql(lcSqlStatement,'BOMLINE',lcMastBomLn, "","",.F.)
        Else
          llShpRec = .T.
        Endif
      Endif
      Dimension laIndex[1,2]
      laIndex = ''
      laIndex[1,1] = 'cImTyp+cType+ShipNo+cTktNo+STR(LineNo,6)'
      laIndex[1,2] = lcMastBomLn
      =lfSetIndex(lcMastBomLn,@laIndex)
    Endif

    *-- Get the information of the vendor

    *B128055,1 AMH 05/18/2005 Use m.Vendor to work with RDAC correctly. [Start]
    *lcSqlStatement = "SELECT * FROM APVENDOR "+;
    "WHERE cVendCode = '" + &lcMastPoHd..Vendor + "'"
    m.VENDOR = Evaluate(lcMastPoHd+'.Vendor')
    lcSqlStatement = "SELECT * FROM APVENDOR "+;
      "WHERE cVendCode =?m.Vendor"
    *B128055,1 AMH 05/18/2005 [End]

    =lfGetItmInf(lcInvType,&lcMastPoHd..VENDOR,;
      lcVendFile,'APVENDOR',lcSqlStatement,.F.)
    =Seek(&lcMastPoHd..VENDOR,lcVendFile)
    lcChkPO = &lcTmpLine..PO
  Endif

  Select (lcTmpLine)

  *--Check if receive to stock or receive to damage/2nd qualtity.
  llStkLine = ( TranCd $'24')
  *--Compute previous open balance on style P/o or C/t.----------------
  lcMainKy   = cBusDocu+cStyType+PO+cInvType+Style+Str(Lineno,6)
  lcOrjWareH = lfCalOpen(lcRecvType, lcMastPoLn, lcMainKy , .T.)
  *--If this is a new line therefor no open qty.
  If &lcTmpLine..lNewLn
    laOpnQty = 0
    *B128318,1 KHM 06/01/2005 [Begin]
    laOldOpnQty = 0
    *B128318,1 KHM 06/01/2005 [End]
  Endif

  *-- Multiply the open quantity by the conversion factor
  For lnCntI = 1 To 8
    *B128318,1 KHM 06/01/2005 Save the open quantity before multiplying it by the conversion
    *B128318,1                factor in order to use it in udpdating the POHDR file [Begin]
    laOldOpnQty[lnCntI] = laOpnQty[lnCntI]
    *B128318,1 KHM 06/01/2005 [End]

    laOpnQty[lnCntI] = laOpnQty[lnCntI] * lnConvFact
  Endfor

  *--Update BomLine (Step 1)--------------------------------------------
  Select (lcTmpLine)
  If llStkLine And (llMFCall Or !(lcRecvType $'REPFG') )
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
    *!*	    lcBomPTyp = IIF(&lcTmpLine..cStyType = "U","M",;
    *!*	      IIF(&lcTmpLine..cStyType = "P","I",&lcTmpLine..cStyType))
    lcBomPTyp = Iif(&lcTmpLine..cStyType = "U","M",;
      IIF(&lcTmpLine..cStyType = "P","I",Iif(&lcTmpLine..cStyType = "F","T",&lcTmpLine..cStyType)))
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
    *lcBomPKey = Po+STR(LineNo,6)
    lcBomPKey = PO
    *-- Create new records in BOMLINE file with type '2' if it is not found.
    Do gpCrtBom With lcBomPTyp,lcBomPKey,Style,;
      IIF(lcRecvType $ 'SC',ShipNo,Space(6)),;
      lcGlSession,&lcTmpLine..cStyGrade,lcMastBomLn,lcTmpBomLn,llShpRec

    Select (lcTmpLine)

    *--Calculate landed costs case of detail costing.
    *-- If you are not in the manufacturing module and not detail costing
    *-- and you are editing the costs then take it from the entered one.

    *B126833,1 WAM 04/03/2005 add new button to edit landed cost for styles has no detail costing
    *IF !llMfCall AND !llImpCost AND llEditLCst
    If !llMFCall And !lDetCost And llEditLCst
      *B126833,1 WAM 04/03/2005 (End)

      *--Take from entered landed cost.
    Else
      *! B609634,1 MMT 06/28/2011 Fix bug of Zero Cost in Styinvjl in Case of receiving Interlocation PO[Start]
      If !(&lcTmpLine..cStyType+&lcTmpLine..cBusDocu = 'NN')
        *! B609634,1 MMT 06/28/2011 Fix bug of Zero Cost in Styinvjl in Case of receiving Interlocation PO[ENd]
        =lfGetLanded()
        *! B609634,1 MMT 06/28/2011 Fix bug of Zero Cost in Styinvjl in Case of receiving Interlocation PO[Start]
      Endif
      *! B609634,1 MMT 06/28/2011 Fix bug of Zero Cost in Styinvjl in Case of receiving Interlocation PO[End]
    Endif
  Endif

  *--New Receiving Cost.
  Select (lcTmpLine)
  If llMFCall
    lnNewCost = nFLanCost1 + nFLanCost2 + nFLanCost3 + nFLanCost4 + nFLanCost5 +;
      nFLanCost6 + nFLanCost7
  Else
    lnELanded = nLan_Cost1
    If lnELanded = Round(Gros_Price*(1-Disc_Pcnt/100),3)
      lnELanded = Gros_Price*(1-Disc_Pcnt/100)
    Endif
    lnNewCost = lnELanded+nLan_Cost2+nLan_Cost3+nLan_Cost4+nLan_Cost5+nLan_Cost6+nLan_Cost7
  Endif
  *B608773,1 WAM 12/24/2008 Store landed cost in base currency
  lnOrgCost = lnNewCost
  *B608773,1 WAM 12/24/2008 (End)

  lcStyle    = Style
  lcWareHous = cWareCode
  lcDyelot   = Iif(Seek(lcMainKy,lcMastPoLn),Evaluate(lcMastPoLn+'.Dyelot'),'')

  *-- Get item information from style file
  If lcInvType = "0001"
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
    *lcSqlStatement = "SELECT * FROM STYLE WHERE Style = '" + lcStyle + "'"
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
    *lcSqlStatement = "SELECT * FROM STYLE WHERE Style = [" + lcStyle + "]"
    lcSelStyV = lcStyle
    lcSqlStatement = "SELECT * FROM STYLE WHERE Style = ?m.lcSelStyV "
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
  Else
    *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
    *lcSqlStatement = "SELECT * FROM ITEM WHERE cInvType ='" + lcInvType +;
    "' AND Style = '" + lcStyle + "'"
    lcSqlStatement = "SELECT * FROM ITEM WHERE cInvType ='" + lcInvType +;
      "' AND Style = ?m.lcStyle "
    *B607658,1 KHM 07/07/2005 [End]
  Endif
  =lfGetItmInf(lcInvType,Iif(lcInvType="0001","",lcInvType)+lcStyle,lcTempItem,;
    IIF(lcInvType="0001",'STYLE','ITEM'),lcSqlStatement,lcInvType = "0002")
  =Seek(Iif(lcInvType="0001","",lcInvType)+lcStyle,lcTempItem)

  *-- Get item information from stydye file for the target location
  If lcInvType = "0001"
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
    *!*	    lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
    *!*	      lcStyle+lcOrjWareH+SPACE(10) + "'"
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
    *!*	    lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
    *!*	      lcStyle+lcOrjWareH+SPACE(10) + "]"
    lcSelStyDyeV = lcStyle+lcOrjWareH+Space(10)
    lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = ?m.lcSelStyDyeV"
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][EnD]
  Else
    *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
    *lcSqlStatement = "SELECT * FROM ITEMLOC WHERE cInvType = '" + lcInvType +;
    "' AND Style ='" + lcStyle + "' AND cWareCode='" + lcOrjWareH +;
    "' AND Dyelot = '" + SPACE(10) + "'"
    lcSqlStatement = "SELECT * FROM ITEMLOC WHERE cInvType = '" + lcInvType +;
      "' AND Style = ?m.lcStyle " + " AND cWareCode = ?m.lcOrjWareH " +;
      "  AND Dyelot = '" + Space(10) + "'"
    *B607658,1 KHM 07/07/2005
  Endif
  =lfGetItmInf(lcInvType,Iif(lcInvType="0001","",lcInvType)+lcStyle+lcOrjWareH+Space(10),lcItemLoc,;
    IIF(lcInvType="0001",'STYDYE','ITEMLOC'),lcSqlStatement, lcInvType = "0002")
  =Seek(Iif(lcInvType="0001","",lcInvType)+lcStyle+lcOrjWareH+Space(10),lcItemLoc)

  *--Read the G/L link code that will be used to create GL entres.
  If llStkLine
    If &lcTmpLine..TranCd = '4'
      *-- Get item information from style file
      If lcInvType = "0001"
        *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
        *lcSqlStatement = "SELECT * FROM STYLE WHERE Style = '" + &lcTmpLine..cRetSty + "'"
        *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
        *lcSqlStatement = "SELECT * FROM STYLE WHERE Style = [" + &lcTmpLine..cRetSty + "]"
        lcRetStySelV =   &lcTmpLine..cRetSty
        lcSqlStatement = "SELECT * FROM STYLE WHERE Style = ?m.lcRetStySelV"
        *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
        *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][END]
      Else
        *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
        *lcSqlStatement = "SELECT * FROM ITEM WHERE cInvType ='" + lcInvType +;
        "' AND Style = '" + lcStyle + "'"
        lcSqlStatement = "SELECT * FROM ITEM WHERE cInvType ='" + lcInvType +;
          "' AND Style = ?m.lcStyle "
        *B607658,1 KHM 07/07/2005 [End]
      Endif
      =lfGetItmInf(lcInvType,Iif(lcInvType="0001","",lcInvType)+&lcTmpLine..cRetSty,lcTempItem,;
        IIF(lcInvType="0001",'STYLE','ITEM'),lcSqlStatement,lcInvType = "0002")
      =Seek(Iif(lcInvType="0001","",lcInvType)+&lcTmpLine..cRetSty,lcTempItem)
    Endif
    *B608645,1 WAM 08/07/2008 Get the receiving cost from the style table when the costing methos is Standard
    If lcInvType = "0001" And lcCostMth = 'S'
      lnNewCost = &lcTempItem..TotCost
    Endif
    *B608645,1 WAM 08/07/2008 (End)

    lclinkCode = Iif(!Empty(&lcTempItem..link_code),&lcTempItem..link_code,'DEFDEF')

    If &lcTmpLine..TranCd = '2'
      lcStyle = &lcTmpLine..Style
    Else
      lcStyle = &lcTmpLine..cRetSty
    Endif
    *-- Get item information from stydye file
    If lcInvType = "0001"
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
      *!*	      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
      *!*	        lcStyle+lcOrjWareH+SPACE(10) + "'"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
      *!*	      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
      *!*	        lcStyle+lcOrjWareH+SPACE(10) + "]"
      lcStyVSelect = lcStyle+lcOrjWareH+Space(10)
      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = ?m.lcStyVSelect "
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
    Else
      *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
      *lcSqlStatement = "SELECT * FROM ITEMLOC WHERE cInvType = '" + lcInvType +;
      "' AND Style ='" + lcStyle + "' AND cWareCode='" + lcOrjWareH +;
      "' AND Dyelot = '" + SPACE(10) + "'"
      lcSqlStatement = "SELECT * FROM ITEMLOC WHERE cInvType = '" + lcInvType +;
        "' AND Style = ?m.lcStyle " + " AND cWareCode = ?m.lcOrjWareH " +;
        "  AND Dyelot = '" + Space(10) + "'"
      *B607658,1 KHM 07/07/2005 [End]
    Endif
    =lfGetItmInf(lcInvType,Iif(lcInvType="0001","",lcInvType)+lcStyle+lcOrjWareH+Space(10),lcItemLoc,;
      IIF(lcInvType="0001",'STYDYE','ITEMLOC'),lcSqlStatement,lcInvType = "0002")
    =Seek(Iif(lcInvType="0001","",lcInvType)+lcStyle+lcOrjWareH+Space(10),lcItemLoc)

    lclinkCode=Iif(!Empty(&lcItemLoc..Gl_link),&lcItemLoc..Gl_link,lclinkCode)
  Endif
  *-- Get item information from stydye file for dyelot
  If !Empty(lcDyelot) And &lcTmpLine..cDye_Flg = "Y"
    If lcInvType = "0001"
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
      *!*	      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
      *!*	        lcStyle+lcOrjWareH+lcDyelot + "'"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
      *!*	      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
      *!*	        lcStyle+lcOrjWareH+lcDyelot + "]"
      lcStyValueSelect = lcStyle+lcOrjWareH+lcDyelot
      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = ?m.lcStyValueSelect"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
    Else
      *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
      *lcSqlStatement = "SELECT * FROM ITEMLOC WHERE cInvType = '" + lcInvType +;
      "' AND Style ='" + lcStyle + "' AND cWareCode='" + lcOrjWareH +;
      "' AND Dyelot = '" + lcDyelot + "'"
      lcSqlStatement = "SELECT * FROM ITEMLOC WHERE cInvType = '" + lcInvType +;
        "' AND Style = ?m.lcStyle " + " AND cWareCode = ?m.lcOrjWareH " +;
        "  AND Dyelot = '" + lcDyelot + "'"
      *B607658,1 KHM 07/07/2005
    Endif
    =lfGetItmInf(lcInvType,Iif(lcInvType="0001","",lcInvType)+lcStyle+lcOrjWareH+lcDyelot,lcItemLoc,;
      IIF(lcInvType="0001",'STYDYE','ITEMLOC'),lcSqlStatement,lcInvType = "0002")
    =Seek(Iif(lcInvType="0001","",lcInvType)+lcStyle+lcOrjWareH+lcDyelot,lcItemLoc)
  Endif

  lcGLPLkC = Iif(Seek(&lcTmpLine..cBusDocu+&lcTmpLine..cStyType+&lcTmpLine..PO,lcMastPoHd),;
    &lcMastPoHd..link_code,'DEFDEF')

  lcCurSty = &lcTempItem..Style
  = Seek(&lcTmpLine..Style,lcTempItem)

  If !(lcRecvType $ 'RNOLAEG') And llStkLine And llDyelot And &lcTempItem..cDye_Flg = 'Y'
    If &lcTmpLine..TranCd = '2'
      *! B610163,1 HIA 11/27/2012 Error updateing, while receving material PO [T20121112.0027][Begin]
      *llFound=SEEK(&lcTmpLine..STYLE  +&lcTmpLine..cWareCode+&lcTmpLine..Dyelot,lcItemLoc)
      lcOldAliasT = Alias()
      lcTempItm   = gfTempName()
      If !Used(lcTempItm)
        = gfOpenTable('ITEMLOC','STYDYE', 'SH',lcTempItm)
      Endif
      Select (lcTempItm)
      llFound = gfSEEK(Iif(lcInvType="0001","",lcInvType)+&lcTmpLine..Style  +&lcTmpLine..cWareCode+&lcTmpLine..Dyelot)
      =gfcloseTable(lcTempItm)
      Select (lcOldAliasT)
      *! B610163,1 HIA 11/27/2012 Error updateing, while receving material PO [T20121112.0027][END]
    Else    && Trancd = lcOthrTrCd
      *! B610163,1 HIA 11/27/2012 Error updateing, while receving material PO [T20121112.0027][Begin]
      *llFound=SEEK(&lcTmpLine..cRetSty+&lcTmpLine..cWareCode+&lcTmpLine..Dyelot,lcItemLoc)
      lcOldAliasT = Alias()
      lcTempItm  = gfTempName()
      If !Used(lcTempItm)
        =gfOpenTable('ITEMLOC','STYDYE', 'SH',lcTempItm)
      Endif
      Select (lcTempItm)
      llFound = gfSEEK(Iif(lcInvType="0001","",lcInvType)+&lcTmpLine..cRetSty+&lcTmpLine..cWareCode+&lcTmpLine..Dyelot)
      =gfcloseTable(lcTempItm)
      Select (lcOldAliasT)
      *! B610163,1 HIA 11/27/2012 Error updateing, while receving material PO [T20121112.0027][End]
    Endif

    If ! llFound
      If lcInvType = "0001"
        Do gpAdStyWar With Iif(&lcTmpLine..TranCd='2',&lcTmpLine..Style,&lcTmpLine..cRetSty),;
          &lcTmpLine..Dyelot,&lcTmpLine..cWareCode
      Else
        =gfAdItemWar(lcInvType,Iif(&lcTmpLine..TranCd='2',&lcTmpLine..Style,&lcTmpLine..cRetSty),;
          &lcTmpLine..Dyelot,&lcTmpLine..cWareCode)
      Endif
      *      SELECT (lcItemLoc)
      *      APPEND BLANK
      *      REPLACE Style     WITH IIF(&lcTmpLine..Trancd='2',&lcTmpLine..Style,&lcTmpLine..cRetSty),;
      cWareCode WITH &lcTmpLine..cWareCode,;
      Dyelot    WITH &lcTmpLine..Dyelot
    Endif
  Endif

  Select (lcTmpLine)

  = Seek(lcCurSty,lcTempItem)

  lcWipSgn = Iif(lcRecvType $ 'R','+','-')
  *-- Update WIP in the style file
  For lnCnI = 1 To 8
    lcCntI = Str(lnCnI,1)
    laReceive[lnCnI] = Evaluate(lcTmpLine+'.Qty'+lcCntI) * lnConvFact
  Endfor

  *--Update Style -----------------------------------------------
  *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
  *IF lcInvType = "0001" AND SEEK(&lcTmpLine..Style,lcTempItem) AND !&lcTmpLine..lNewLUpd
  *  =lfUpdItmFl(lcTempItem,'WIP',lcWipSgn,@laReceive,@laOpnQty)
  *  =lfUpdWip()
  *ENDIF
  If Seek(&lcTmpLine..Style,lcTempItem) And !&lcTmpLine..lNewLUpd
    =lfUpdItmFl(lcTempItem, Iif(lcRecvType = 'G', 'NONRET', 'WIP'), lcWipSgn, @laReceive, @laOpnQty, lcInvType = "0002")
    If lcInvType = "0001"
      =lfUpdWip()
    Endif
  Endif
  *E039550,1 WSH 08/07/2005 [End]

  *--Update StyDye Warehouse record -------------------------------
  *IF SEEK(IIF(lcInvType="0001","",lcInvType)+&lcTmpLine..Style+lcOrjWareH+SPACE(10),lcItemLoc) AND !&lcTmpLine..lNewLUpd
  If lcRecvType <> 'G'
    If Seek(Iif(lcInvType="0001","",lcInvType)+&lcTmpLine..Style+lcOrjWareH+Space(10),lcItemLoc);
        AND !&lcTmpLine..lNewLUpd
      =lfUpdItmFl(lcItemLoc,'WIP',lcWipSgn,@laReceive,@laOpnQty)
    Endif
  Else
    If Seek(lcInvType+&lcTmpLine..Style+lcOrjWareH+Space(10),lcItemLoc);
        AND !&lcTmpLine..lNewLUpd
      =lfUpdItmFl(lcItemLoc,'NONRET',lcWipSgn,@laReceive,@laOpnQty)
    Endif
  Endif

  *--Update StyDye Warehouse/dyelot record -----------------------
  *IF (llDyelot OR llFabDye) AND !EMPTY(&lcTmpLine..Dyelot) AND &lcTempItem..cDye_Flg='Y' AND;
  SEEK(IIF(lcInvType="0001","",lcInvType)+&lcTmpLine..Style+lcOrjWareH+lcDyelot,lcItemLoc)
  If lcRecvType <> 'G'
    If (llDyelot Or llFabDye) And !Empty(&lcTmpLine..Dyelot) And;
        &lcTempItem..cDye_Flg='Y' And;
        SEEK(Iif(lcInvType="0001","",lcInvType)+&lcTmpLine..Style+lcOrjWareH+lcDyelot,lcItemLoc)
      =lfUpdItmFl(lcItemLoc,'WIP',lcWipSgn,@laReceive,@laOpnQty)
    Endif
  Else
    If (llDyelot Or llFabDye) And !Empty(&lcTmpLine..Dyelot) And;
        &lcTempItem..cDye_Flg='Y' And;
        SEEK(lcInvType+&lcTmpLine..Style+lcOrjWareH+lcDyelot,lcItemLoc)
      =lfUpdItmFl(lcItemLoc,'NONRET',lcWipSgn,@laReceive,@laOpnQty)
    Endif
  Endif

  *--Update APVendor -------------------------------
  If !llMFCall And !(lcRecvType $ 'OELC') And Seek(&lcTmpLine..VENDOR,lcVendFile)
    Select (lcTmpLine)
    If lcLstTrn <> PO And lcPoVend <> VENDOR
      Store 0 To lnLstPoRAm,lnOpnPoAmt
      lcLstTrn  = PO
      lcPoVend = VENDOR
    Endif
    lnLstPoRAm = lnLstPoRAm + (TotQty * nLan_Cost1)
    =Seek(cBusDocu+cStyType+PO+cInvType+Style+Str(Lineno,6)+'1',lcMastPoLn)

    *B609696,1 MMT 10/13/2011 Fix bug of wrong updates of the Field apvendor.nVenOpnPo [Start]
    *lnOpnPoAmt = ( MIN((TotQty * nLan_Cost1),(&lcMastPoLn..TotQty * &lcMastPoLn..nICost1)) )
    lnTotQtyVend = 0
    For lnCntQty = 1 To 8
      lnTotQtyVend = lnTotQtyVend +  Min(laReceive[lnCntQty],laOpnQty[lnCntQty])
    Endfor
    lnOpnPoAmt = (lnTotQtyVend * nFLanCost1)
    *B609696,1 MMT 10/13/2011 Fix bug of wrong updates of the Field apvendor.nVenOpnPo [END]

    Select (lcVendFile)
    If lcRecvType $ 'RG'
      Replace nVenOpnPo With nVenOpnPo - lnOpnPoAmt
    Else
      Replace dVenLPoRD With ldRecDate  ,;
        nVenLPoRA With lnLstPoRAm ,;
        nVenOpnPo With nVenOpnPo - lnOpnPoAmt
    Endif
    Select (lcTmpLine)
  Endif

  *--Update POSHDR Header file  --------------------------
  lnOpnSub=0
  For lnI = 1 To 8
    lcCnt = Str(lnI,1)
    *B128318,1 KHM 06/01/2005 Use laOldOpnQty instead of laOpnQty [Begin]
    *lnOpnSub = lnOpnSub + MIN(laOpnQty[lnI] , &lcTmpLine..Qty&lcCnt )
    lnOpnSub = lnOpnSub + Min(laOldOpnQty[lnI] , &lcTmpLine..QTY&lcCnt )
    *B128318,1 KHM 06/01/2005 [End]

  Endfor

  lcFrstSess = Iif(lcRecvType $ 'RG' And !Empty(&lcMastPoHd..cPONo),&lcTmpLine..cRSession,'')
  Select (lcMastPoHd)
  Do Case
  Case llStkLine
    *-- If receive to stock
    If &lcTmpLine..TranCd = '2'
      Replace Receive With Receive + &lcTmpLine..TotQty,;
        OPEN    With Max(Open - lnOpnSub,0)
    Else
      *-- If receive to 2nd quality or damage
      Replace Damage  With Damage  + &lcTmpLine..TotQty,;
        OPEN    With Max(Open - lnOpnSub,0)
    Endif
    Replace nFLanCost2 With nFLanCost2 +(Round(&lcTmpLine..nFLanCost2 * &lcTmpLine..TotQty,3)),;
      nFLanCost3 With nFLanCost3 +(Round(&lcTmpLine..nFLanCost3 * &lcTmpLine..TotQty,3)),;
      nFLanCost4 With nFLanCost4 +(Round(&lcTmpLine..nFLanCost4 * &lcTmpLine..TotQty,3)),;
      nFLanCost5 With nFLanCost5 +(Round(&lcTmpLine..nFLanCost5 * &lcTmpLine..TotQty,3)),;
      nFLanCost6 With nFLanCost6 +(Round(&lcTmpLine..nFLanCost6 * &lcTmpLine..TotQty,3)),;
      nFLanCost7 With nFLanCost7 +(Round(&lcTmpLine..nFLanCost7 * &lcTmpLine..TotQty,3))

    Replace nLan_Cost2 With nLan_Cost2 +(Round(&lcTmpLine..nLan_Cost2 * &lcTmpLine..TotQty,3)),;
      nLan_Cost3 With nLan_Cost3 +(Round(&lcTmpLine..nLan_Cost3 * &lcTmpLine..TotQty,3)),;
      nLan_Cost4 With nLan_Cost4 +(Round(&lcTmpLine..nLan_Cost4 * &lcTmpLine..TotQty,3)),;
      nLan_Cost5 With nLan_Cost5 +(Round(&lcTmpLine..nLan_Cost5 * &lcTmpLine..TotQty,3)),;
      nLan_Cost6 With nLan_Cost6 +(Round(&lcTmpLine..nLan_Cost6 * &lcTmpLine..TotQty,3)),;
      nLan_Cost7 With nLan_Cost7 +(Round(&lcTmpLine..nLan_Cost7 * &lcTmpLine..TotQty,3))

    If &lcTmpLine..nLan_Cost1 = Round(&lcTmpLine..Gros_Price*(1-&lcTmpLine..Disc_Pcnt/100),3)
      Replace nFLanCost1 With nFLanCost1 +(Round(&lcTmpLine..nFLanCost1 * &lcTmpLine..TotQty,3)),;
        nLan_Cost1 With nLan_Cost1 +(Round((&lcTmpLine..Gros_Price*(1-&lcTmpLine..Disc_Pcnt/100))* &lcTmpLine..TotQty,3))
    Else
      Replace nFLanCost1 With nFLanCost1 +(Round(&lcTmpLine..nFLanCost1 * &lcTmpLine..TotQty,3)),;
        nLan_Cost1 With nLan_Cost1 +(Round(&lcTmpLine..nLan_Cost1 * &lcTmpLine..TotQty,3))
    Endif
    Replace nTot_Cost  With nLan_Cost1+nLan_Cost2+nLan_Cost3+nLan_Cost4+nLan_Cost5+nLan_Cost6+nLan_Cost7

  Case &lcTmpLine..TranCd = '5'
    Replace Cancel  With Cancel  + &lcTmpLine..TotQty,;
      OPEN    With Max((Open    - lnOpnSub),0)
  Endcase

  If Open = 0
    Replace &lcMastPoHd..Status With 'C'
    *E303627,1 MMT 11/29/2015 Call trigger to update the Inter-Location PO related Picking ticket[T20151014.0017][Start]
    If Ascan(loFormSet.laEvntTrig,Padr('COMPPIKTKT',10),1,Alen(loFormSet.laEvntTrig,1),1) > 0
      =loFormSet.mDoTrigger(Padr('COMPPIKTKT' ,10))
    Endif
    *E303627,1 MMT 11/29/2015 Call trigger to update the Inter-Location PO related Picking ticket[T20151014.0017][End]
  Endif

  *!*	    *C037345,1 ABD - Trigger for customer PUFFA to update the style critical path file. [BEgin]
  *!*	    IF lcRecvType = 'I' .AND. ASCAN(laEvntTrig , PADR('STYCRUPD',10)) <> 0
  *!*	      =gfDoTriger('POSTREC',PADR('STYCRUPD',10))
  *!*	    ENDIF
  *!*	    *C037345,1 ABD - [End]

  Select (lcTmpLine)
  Scatter Memvar
  Select (lcPosLn)
  Append Blank
  Gather Memvar
  Replace Date      With ldRecDate,;
    dPostDate With ldTrDate ,;
    cOwner    With ' '
  *--Changing receiving from customer to location.
  If !llMFCall And cWareCode <> lcDropLoc And !Empty(Account)
    Replace Account With Space(5),;
      STORE   With Space(8)
  Endif
  *B128070,1 KHM 05/19/2005 Updating the receiving session in thePosLn in call cases
  *B128070,1 KHM 05/19/2005 which means when receive to cancel [Begin]
  *IF llStkLine
  *B128070,1 KHM 05/19/2005 [End]
  Replace cRSession With lcGlSession
  *ENDIF

  Select (lcTmpLine)
  If llStkLine And (llMFCall Or !(lcRecvType $ 'ROELG'))
    If llMFCall
      *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
      *lcBomLKey = 'M2'+Cuttkt
      lcBomLKey = 'M2'+PO
      *N038893,1 WAM 06/02/2005 (End)

      lcWhileCn = "cIMTyp+cType+cTktNo=lcBomLKey"
    Else
      *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
      *!*	      lcBomLKey = IIF(lcRecvType = 'D','D',;
      *!*	        IIF(&lcTmpLine..cStyType="N","N",'I'))+'2'+;
      *!*	        IIF(lcRecvType = 'S',Shipno,'')+Po+STR(LINENO,6)
      lcBomLKey = Iif(lcRecvType = 'D','D',;
        IIF(&lcTmpLine..cStyType="N","N",Iif(&lcTmpLine..cStyType="F",'T','I')))+'2'+;
        IIF(lcRecvType = 'S',ShipNo,'')+PO+Str(Lineno,6)
      *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
      lcWhileCn = "cIMTyp+cType+IIF(lcRecvType = 'S',Shipno,'')+cTktNo+STR(LineNo,6)=lcBomLKey"
    Endif
    Select (lcTmpBomLn)
    If lcRecvType ='S'
      Set Order To Tag BomLnShp
    Else
      Set Order To Tag BomLine
    Endif
    *B610469,1 TMI 08/18/2013 [Start] Get the key
    lcTmpBomLnKey = Key()
    *B610469,1 TMI 08/18/2013 [End  ] Get the key

    If Seek(lcBomLKey)
      *B608762,1 WAM 12/15/2008  Consider receiving cut ticket line into more than one warehouse
      *SCAN REST WHILE &lcWhileCn ;
      FOR Style = &lcTmpLine..Style AND EMPTY(cRSession) AND cStyGrade = &lcTmpLine..cStyGrade

      *B608845,1 WAM 04/09/2009 Take line# into consideration
      *SCAN REST WHILE &lcWhileCn ;
      FOR Style = &lcTmpLine..Style AND cWarecode = &lcTmpLine..cWarecode AND EMPTY(cRSession) AND cStyGrade = &lcTmpLine..cStyGrade
      Scan Rest While &lcWhileCn ;
          FOR Style = &lcTmpLine..Style And Lineno = &lcTmpLine..Lineno And cWareCode = &lcTmpLine..cWareCode And Empty(cRSession) And cStyGrade = &lcTmpLine..cStyGrade

        *B608845,1 WAM 04/09/2009 (End)

        Scatter Memvar Memo
        Delete
        Append Blank
        Gather Memvar Memo
        Replace cRSession With lcGlSession
        Scatter Memvar
        Store 0 To m.StyQty,m.ItemQty,m.ItemAmt
        m.cRSession = ''
        *B610469,1 TMI 08/18/2013 [Start] get the key of the current alias
        lcCurrKeyNLINENO = Evaluate(Key())
        lcCurrKey = Left(lcCurrKeyNLINENO,Len(lcCurrKeyNLINENO)-6)
        *B610469,1 TMI 08/18/2013 [End  ]
        Select (lcTmpBomLn)
        *: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
        If llAddBomLine
          *: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]
          *B610469,1 TMI 08/18/2013 [Start] locate to check if there is empty receiving session more than once, if so then don't add the line
          =Seek(lcCurrKey)
          Locate Rest While &lcTmpBomLnKey. = lcCurrKey For Empty(cRSession)
          If !Found()
            *B610469,1 TMI 08/18/2013 [End  ]
            Append Blank
            Gather Memvar
          Else
            =Seek(lcCurrKeyNLINENO)
            *B610469,1 TMI 08/18/2013 [Start]
          Endif
          *B610469,1 TMI 08/18/2013 [End  ]
          *: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
        Endif
        *: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]
      Endscan
      Select (lcTmpLine)
    Endif
  Endif

  *--Update MfgOprDt file --------------------------------------
  Select (lcTmpLine)
  lcLastOpr = clastopr
  If !Empty(lcLastOpr) And lcRecvType $ 'IMDO'
    Scatter Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty To laRecvQty
    If laRecvQty[9] > 0
      =lfUpdLot(&lcTmpLine..PO,lcLastOpr,&lcTmpLine..cLotNo,'laRecvQty',&lcTmpLine..Style,&lcTmpLine..Dyelot)
    Endif
  Endif

  *--Update Shipment In-Transit P/o line qty. ------------
  Select (lcTmpLine)
  If lcRecvType $ 'SF' Or (lcRecvType = 'B' And !Empty(CTKTRCVH.ShipNo) )
    If (lcRecvType = 'B' And !Empty(CTKTRCVH.ShipNo) )
      lcShpcode = CTKTRCVH.ShipNo
    Endif
    lcShpLine = ShipNo+cBusDocu+cStyType+PO+cInvType+Style+Str(Lineno,6)+'3'
    Select (lcMastPoLn)
    Set Order To Tag Poslnsh
    =Seek(lcShpLine)
    Scatter Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 To laSHPQty
    Scatter Memvar Memo
    Select(lcPosLn)
    Set Order To Tag Poslnsh
    If !Seek(lcShpLine)
      Select(lcPosLn)
      Append Blank
      Tableupdate(0,.T.)
      Gather Memvar Memo
    Endif
    Replace Qty1 With Max(Qty1 -&lcTmpLine..Qty1,0),;
      Qty2 With Max(Qty2 -&lcTmpLine..Qty2,0),;
      Qty3 With Max(Qty3 -&lcTmpLine..Qty3,0),;
      Qty4 With Max(Qty4 -&lcTmpLine..Qty4,0),;
      Qty5 With Max(Qty5 -&lcTmpLine..Qty5,0),;
      Qty6 With Max(Qty6 -&lcTmpLine..Qty6,0),;
      Qty7 With Max(Qty7 -&lcTmpLine..Qty7,0),;
      Qty8 With Max(Qty8 -&lcTmpLine..Qty8,0),;
      TotQty With Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
    If TotQty = 0
      Delete
    Endif

    Select (lcMastPoLn)
    Set Order To Tag lcMastPoLn
    Select(lcPosLn)
    Set Order To Tag (lcPosLn)

    Select (lcTmpLine)
  Endif

  Select (lcTmpLine)
  *! B610051,1 MMT 08/16/2012 Receiving Inter-location PO to different location other than target location on PO gives error if Bin location is used[Start]
  lcRecWareH = m.cWareCode
  *! B610051,1 MMT 08/16/2012 Receiving Inter-location PO to different location other than target location on PO gives error if Bin location is used[End]
  If lcRecvType $ 'OELC'
    Scatter Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 To laSHPQty

    *--Get the received line qty.
    Scatter Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 To laAcLRQy
    lcShpLine = cBusDocu+cStyType+PO+cInvType+Style+Str(Lineno,6)+'6'
    Select (lcMastPoLn)
    =Seek(lcShpLine)
    Scan Rest While cBusDocu+cStyType+PO+cInvType+Style+Str(Lineno,6)+TranCd = lcShpLine;
        FOR TotQty <> 0
      Scatter Memvar Memo
      *! B610051,1 MMT 08/16/2012 Receiving Inter-location PO to different location other than target location on PO gives error if Bin location is used[Start]
      m.cWareCode  =lcRecWareH
      *! B610051,1 MMT 08/16/2012 Receiving Inter-location PO to different location other than target location on PO gives error if Bin location is used[End]
      Select(lcPosLn)
      Append Blank
      Tableupdate(0,.T.)
      Gather Memvar Memo
      Select(lcPosLn)
      For I=1 To 8
        If laAcLRQy[I]<=0
          Loop
        Endif
        Z=Str(I,1)
        lnQty&Z = QTY&Z
        Replace QTY&Z With Max(QTY&Z - laAcLRQy[I],0)
        laAcLRQy[I] = laAcLRQy[I] - lnQty&Z
      Endfor
      Replace TotQty With Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
      If TotQty = 0
        Delete
      Endif
    Endscan
    Select (lcTmpLine)
  Endif

  *--Update Intransit line for Receive by Shipment or Receive inter Location P/o.
  *-- 'S' Receive by Shipment, 'O' Receive Inter-Location P/o
  *-- 'E' Receive Adornment order, 'L' Receive Inter Location P/O Batch
  *-- 'F' Receive Material PO Shipment, 'C' Receive Inter Location P/O Shipment
  If lcRecvType $ 'SOELFC'
    For lnCnI = 1 To 8
      lcCntI = Str(lnCnI,1)
      laReceive[lnCnI] = Evaluate(lcTmpLine+'.Qty'+lcCntI) * lnConvFact
      laSHPQty[lnCnI]  = laSHPQty[lnCnI]  * lnConvFact
    Endfor

    *--Update Style record -----------------------
    *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
    *IF lcInvType="0001" AND SEEK(&lcTmpLine..Style,lcTempItem)
    *  =lfUpdItmFl(lcTempItem,'InTrans','-',@laSHPQty,@laReceive)
    If Seek(&lcTmpLine..Style,lcTempItem)
      =lfUpdItmFl(lcTempItem, 'InTrans', '-', @laSHPQty, @laReceive, lcInvType = "0002")
      *E039550,1 WSH 08/07/2005 [End]
      Select (lcTmpLine)
    Endif

    *--Update ItemLoc/StyDye Warehouse record -----------------------
    *B128741,1 KHM 06/30/2005 Use the original warehouse [Begin]
    *IF SEEK(IIF(lcInvType="0001","",lcInvType)+&lcTmpLine..Style+&lcTmpLine..cWareCode+SPACE(10),lcItemLoc)
    If Seek(Iif(lcInvType="0001","",lcInvType)+&lcTmpLine..Style+lcOrjWareH+Space(10),lcItemLoc)
      *B128741,1 KHM 06/30/2005 [End]
      =lfUpdItmFl(lcItemLoc,'InTrans','-',@laSHPQty,@laReceive)
      Select (lcTmpLine)
    Endif

    *--Update ItemLoc/StyDye Warehouse/dyelot record -----------------------
    If (llDyelot Or llFabDye) And !Empty(&lcTmpLine..Dyelot) And ;
        &lcTempItem..cDye_Flg='Y' And;
        SEEK(Iif(lcInvType="0001","",lcInvType)+&lcTmpLine..Style+lcOrjWareH+lcDyelot,lcItemLoc)
      =lfUpdItmFl(lcItemLoc,'InTrans','-',@laSHPQty,@laReceive)
      Select (lcTmpLine)
    Endif

  Endif

  If llStkLine
    *--G/L Array difinition and initialization.
    *-- Update general ledger entreis in gfStyCrl()
    If llLinkToGl
      Declare laGLDistAr[2,13]
      laGLDistAr[1,1] = lclinkCode
      laGLDistAr[2,1] = lcGLPLkC
      laGLDistAr[1,2] = Iif(lcInvType = "0001",'006','015')
      laGLDistAr[2,2] = '013'
      laGLDistAr[1,3] =  1
      laGLDistAr[2,3] = -1
      *N039541,1 KHM 12/12/2005 [Start]
      *STORE 'PO' TO laGLDistAr[1,4],laGLDistAr[2,4]
      Do Case
        *-- Case Style PO
      Case lcRecvType $ 'ISBROCL'
        Store 'PO' To laGLDistAr[1,4],laGLDistAr[2,4]
        *-- Case Material PO
      Case lcRecvType $ 'PGF'
        Store 'MO' To laGLDistAr[1,4],laGLDistAr[2,4]
        *-- Case Cutting Ticket
      Case lcRecvType $ 'MT'
        Store 'CT' To laGLDistAr[1,4],laGLDistAr[2,4]
        *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
      Case lcRecvType ='W'
        Store 'MM' To laGLDistAr[1,4],laGLDistAr[2,4]
        *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
      Endcase
      *N039541,1 KHM 12/12/2005 [End]
      Store &lcTmpLine..PO To laGLDistAr[1,5],laGLDistAr[2,5]
      Store ldTrDate   To laGLDistAr[1,6],laGLDistAr[2,6]
      Store lcGLFYear  To laGLDistAr[1,7],laGLDistAr[2,7]
      Store lcGLPeriod To laGLDistAr[1,8],laGLDistAr[2,8]
      Store lcGlDist   To laGLDistAr[1,9],laGLDistAr[2,9]
    Else
      Dime laGLDistAr[1,1]
      laGLDistAr = ''
    Endif

    Select (lcTmpLine)
    lcJTType = Iif(lcInvType = "0001",Iif(llMFCall,'5','6'),Iif(lcRecvType $ 'G','6','5'))
    lcJrlSty = Iif(TranCd = '4',cRetSty,Style)
    lcJDyelt = Dyelot
    lnJSgn   = Iif(lcRecvType $ 'RG',-1,1)
    Declare laAdjust[9]
    For I = 1 To 8
      Z=Str(I,1)
      laAdjust[I] = (lnJSgn*QTY&Z) * lnConvFact
    Endfor
    laAdjust[9] = (lnJSgn*TotQty) * lnConvFact

    *--Call the global function for update style inventory control.
    Private lcRefer
    If llMFCall
      Select (lcMastOprDt)
      =Seek('M'+&lcTmpLine..PO+&lcTmpLine..clastopr+&lcTmpLine..cLotNo+'2')
      Locate Rest While cIMTyp+cTktNo+cOprCode+TranCd = ;
        'M'+&lcTmpLine..PO+&lcTmpLine..clastopr+&lcTmpLine..cLotNo+'2' ;
        FOR Item = &lcTmpLine..Style
      lcRefer = "VEN. " + &lcMastOprDt..CCONTCODE+ &lcMastOprDt..CCONTNAME
    Else
      If !(lcRecvType $ 'E')
        lcRefer = "VEN. " + &lcTmpLine..VENDOR + ' ' + &lcVendFile..cVenComp
      Else
        lcRefer = Space(0)
      Endif
    Endif
    lnNxtStp = 0
    If lcInvType = "0001"
      *B608773,1 WAM 12/24/2008 Update the WIP account with the received landed cost
      If llLinkToGl And lcCostMth = 'S' And Inlist(laGLDistAr[1,4],'CT','PO')

        *B609048,1 WAM 10/19/209 PO receipt update GLDIST incorrectly using Standard Cost [Start]
        lcCstShtType  = Iif(llMFCall,'M','I')
        If loFormSet.llMulCurr
          lcSqlStatement  = "SELECT * FROM BOM WHERE cInvType = '"+lcInvType+"' AND cItmMajor = '"+Substr(&lcTmpLine..Style,1,loFormSet.lnMjrWid)+ "'"+;
            " AND cCstShtTyp ='" + lcCstShtType + "' AND ccstsht_id IN (SELECT cCstSht_Id FROM BOMHEADR WHERE cInvType = '"+;
            lcInvType+"' AND cItmMajor = '"+Substr(&lcTmpLine..Style,1,loFormSet.lnMjrWid)+ "' AND lDefCstSht=1)"
          =lfOpenSql(lcSqlStatement,'',lcTmpBom, "","",.F.)
          Declare laCosts(7),laECosts(7), laCurrency[7,3]
          Store '' To laCurrency
          Store 0 To laCosts(7),laECosts(7)
          =oCostSheet.mGetStyClrCst(&lcTmpLine..Style, &lcTmpLine..Scale, llMscale, lcTmpBom, "", @laCosts, @laECosts, @laCurrency)
        Else
          Select (lcTempItem)
          Scatter Fields ('N'+lcCstShtType+'COST1'),('N'+lcCstShtType+'COST2'),('N'+lcCstShtType+'COST3'),;
            ('N'+lcCstShtType+'COST4'),('N'+lcCstShtType+'COST5'),('N'+lcCstShtType+'COST6'),('N'+lcCstShtType+'COST7') To laECosts
        Endif
        lnNewCost = laECosts[1]+laECosts[2]+laECosts[3]+laECosts[4]+laECosts[5]+laECosts[6]+laECosts[7]
        *B609048,1 WAM 10/19/209 PO receipt update GLDIST incorrectly using Standard Cost [End]

*B611880,1 Es 08/22/2019  DOUBLE RECEIVING PO [Start]
IF lnNewCost <> 0
*B611880,1 Es 08/22/2019  DOUBLE RECEIVING PO [End]

        laGLDistAr[2,3] = -1 * lnOrgCost /lnNewCost
        
*B611880,1 Es 08/22/2019  DOUBLE RECEIVING PO [Start]
ENDIF
*B611880,1 Es 08/22/2019  DOUBLE RECEIVING PO [End]

      Endif
      *B608773,1 WAM 12/24/2008 (End)

*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]

*!*	      lnNxtStp = gfStyCrl(lcJTType,lcJrlSty,&lcTmpLine..cWareCode,lcJDyelt,ldRecDate,;
*!*	        &lcTmpLine..PO,@laAdjust,lnNewCost,lcRefer,lcGlSession,'',;
*!*	        lnNxtStp,lcTmpLine,'nSteps',@laGLDistAr,;
*!*	        &lcTmpLine..Lineno,lcFrstSess)
lnNxtStp = gfStyCrl(lcJTType,lcJrlSty,&lcTmpLine..cWareCode,lcJDyelt,ldRecDate,;
        &lcTmpLine..PO,@laAdjust,lnNewCost,lcRefer,lcGlSession,'',;
        lnNxtStp,lcTmpLine,'nSteps',@laGLDistAr,;
        &lcTmpLine..Lineno,lcFrstSess,.f.,.f.,.f.,.T.)
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]


        
    Else
      Private laOtherPar
      Dimension laOtherPar[2,2]
      laOtherPar[1,1] = 'lnLineNo'
      laOtherPar[1,2] = &lcTmpLine..Lineno
      laOtherPar[2,1] = 'lcRelCode'
      laOtherPar[2,2] = &lcTempItem..cConvBuy

      =gfItemCrl(lcJTType,lcInvType,lcJrlSty,&lcTmpLine..cWareCode,&lcTmpLine..Dyelot,ldRecDate,;
        ldTrDate,Evaluate(lcTmpLine+'.PO'),@laAdjust,lnNewCost/lnConvFact,lcRefer,'','',;
        @laGLDistAr,lcGlSession,Evaluate(lcTmpLine+'.cBusDocu'),;
        EVALUATE(lcTmpLine+'.cStyType'),lcGlSession,'','','',.F.,;
        '',.F.,@laOtherPar)
    Endif

    *T20071102.0018(C200876) TMI Validate Bin Location [Start]
    *tmi 03/27/2008 SP9 fixes [START]
    *IF ASCAN(loFormSet.laEvntTrig,PADR('DLSBNPOR',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
    If Type('loFormSet')='O' And Ascan(loFormSet.laEvntTrig,Padr('DLSBNPOR',10),1,Alen(loFormSet.laEvntTrig,1),1) > 0
      *tmi 03/27/2008 SP9 fixes [END]
      Select (lcTmpLine)
      =gfDoTriger("POSTREC",Padr("DLSBNPOR",10))
    Endif
    *T20071102.0018(C200876) TMI [End  ]

    *B608773,1 WAM 12/24/2008 Get equivalent standard cost for each costing element from BOM file
    If llLinkToGl And lcCostMth = 'S' And Inlist(laGLDistAr[1,4],'CT','PO')
      lcCstShtType  = Iif(llMFCall,'M','I')
      If loFormSet.llMulCurr
        lcSqlStatement  = "SELECT * FROM BOM WHERE cInvType = '"+lcInvType+"' AND cItmMajor = '"+Substr(&lcTmpLine..Style,1,loFormSet.lnMjrWid)+ "'"+;
          " AND cCstShtTyp ='" + lcCstShtType + "' AND ccstsht_id IN (SELECT cCstSht_Id FROM BOMHEADR WHERE cInvType = '"+;
          lcInvType+"' AND cItmMajor = '"+Substr(&lcTmpLine..Style,1,loFormSet.lnMjrWid)+ "' AND lDefCstSht=1)"
        =lfOpenSql(lcSqlStatement,'',lcTmpBom, "","",.F.)
        Declare laCosts(7),laECosts(7), laCurrency[7,3]
        Store '' To laCurrency
        Store 0 To laCosts(7),laECosts(7)
        =oCostSheet.mGetStyClrCst(&lcTmpLine..Style, &lcTmpLine..Scale, llMscale, lcTmpBom, "", @laCosts, @laECosts, @laCurrency)
      Else
        Select (lcTempItem)
        Scatter Fields ('N'+lcCstShtType+'COST1'),('N'+lcCstShtType+'COST2'),('N'+lcCstShtType+'COST3'),;
          ('N'+lcCstShtType+'COST4'),('N'+lcCstShtType+'COST5'),('N'+lcCstShtType+'COST6'),('N'+lcCstShtType+'COST7') To laECosts
      Endif
      *B608773,1 WAM 12/24/2008 If receive cost is different than the standard cost , update the cost of goods variance
      For lnCount = 1 To 7
        *-- Item Estimated landed cost
        lnIELanCost = Evaluate(lcTmpLine+'.nLan_Cost'+Str(lnCount,1))
        lnIEStnCost = laECosts[lnCount]
        If lnIELanCost <> lnIEStnCost
          lcGLTrnTyp = Iif(llMFCall,'CT','PO')
          *-- Cost of goods variance GL account
          lclinkCode = &lcMastPoHd..link_code
          lcGlCatg = Padl(Alltrim(Str(21+lnCount,3)),3,'0')
          =gfSEEK(lclinkCode +lcGlCatg,'GL_LINK')

          Do GlDist With Iif(!Empty(lclinkCode),lclinkCode ,'DEFDEF'),Iif(Empty(Gl_link.GlAcnt),'019',lcGlCatg ),;
            &lcTmpLine..TotQty* lnConvFact*(lnIELanCost - lnIEStnCost),lcGLTrnTyp,&lcTmpLine..PO,ldRecDate,;
            lcGLFYear,lcGLPeriod,lcGlDist
        Endif
      Endfor
    Endif
    Select (lcTmpLine)
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
  Endif

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

Endscan

*B608773,1 WAM 12/24/2008 Get equivalent standard cost for each costing element from BOM file
If Type('oCostSheet') = 'O'
  Release oCostSheet
Endif
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
Wait Window Nowait ''
&& This is a testing code used to deal with an elusive problem appeared with Tony that the PO Receiving screen crashes with
&& TOO MANY VARIABLES error
&& this system function acts as a garbage collector for VFP, it is used to clear the buffer of a cursor
&& is also supposed to release variables no more defined from the memory to free more memory, same comments
&& applies to all occurences of this code in this program

&& I added the condition that this is ENG as this function may cause some delay , so we may try it first in a small area
If Upper(Alltrim(oAriaApplication.DefaultCountry)) == 'ENG'
  =Sys(1104)
Endif
**-- call the Garbag collection [end  ] tmi 6/3/2010
*B609297 TMI 06/14/2010 [End  ]

Local lnConnectionHandlar, lcTranCode
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.cAriaNativeDataFilesConStr,3,'',.T.)
If Type('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
  Return .F.
Endif
*-- 1) Updating style file (FOX) in case of receiving by styles
If lcInvType = '0001'
  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcTempItem,lcTranCode,;
    SET("DataSession"),'STYLE','STYLE')

  If lnConnectionHandlar # 1 .And. lnConnectionHandlar # 2
    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
    Return .F.
  Else
    =Tableupdate(.T.,.T.)
  Endif

  *-- 2) Updating stydye file (FOX)
  Select (lcItemLoc)
  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcItemLoc,lcTranCode,;
    SET("DataSession"),'Style+cWareCode+dyelot','STYDYE')

  If lnConnectionHandlar # 1 .And. lnConnectionHandlar # 2
    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
    Return .F.
  Else
    =Tableupdate(.T.,.T.)
  Endif
Endif

*-- 3) Updating vendor file (FOX)
*-- Case not 'M' Receive C/T, 'T' Receive C/T Batch , 'O' Receive Inter-Location P/o
*--          'C' Receive Inter Location P/O Shipment, 'L' Receive Inter Location P/O Batch
*--          'E' Receive Adornment order
If !llMFCall And !(lcRecvType $ 'OELC')
  Select (lcVendFile)
  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcVendFile,lcTranCode,;
    SET("DataSession"),'cVendCode','APVENDOR')

  If lnConnectionHandlar # 1 .And. lnConnectionHandlar # 2
    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
    Return .F.
  Else
    =Tableupdate(.T.,.T.)
  Endif
Endif

*-- 4) Updating WhsLoc file (FOX)
If llWareLoc And Type('lcTempLoc') = 'C' And Used(lcTempLoc)
  Select (lcTempLoc)
  Locate
  If !Eof()
    lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcTempLoc,lcTranCode,;
      SET("DataSession"),'Style+Color+cWareCode+cLocation','WHSLOC')

    If lnConnectionHandlar # 1 .And. lnConnectionHandlar # 2
      =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
      Return .F.
    Else
      =Tableupdate(.T.,.T.)
    Endif
  Endif
Endif
*! E304047,1 MMT 07/25/2018 Modify Receiving program to update SQL GLDIST table[Start]
*-- 5) Updating the GLDIST file (FOX)
*!*	If llLinkToGl
*!*	  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
*!*	  *WAIT WINDOW 'Updating General Ledger Distribution File ' NOWAIT
*!*	  If !loFormSet.llSilentMod
*!*	    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
*!*	    *WAIT WINDOW 'Updating General Ledger Distribution File ' NOWAIT
*!*	    Wait Window Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_UPDATEGL,loFormSet.GetHeaderText("LANG_POSTREC_UPDATEGL",loFormSet.HeaderAlias)) Nowait
*!*	    *N000682,1 MMT 12/09/2012 Globalization changes[END]
*!*	  Endif
*!*	  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

*!*	  Select (lcGlDist)
*!*	  Do Case
*!*	  Case lcInvType = '0002' And lcRecvType $ 'PF'
*!*	    Replace All GlSession With lcGlSession,;
*!*	      Tran_Desc With 'MAT. P/O RECEIVING'
*!*	    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
*!*	  Case lcInvType = '0002' And lcRecvType = 'W'
*!*	    Replace All GlSession With lcGlSession,;
*!*	      Tran_Desc With 'RECEIVE M.F.G. ORDER'
*!*	    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]

*!*	  Case lcInvType = '0002' And lcRecvType = 'G'
*!*	    Replace All GlSession With lcGlSession,;
*!*	      Tran_Desc With 'MAT. P/O ISSUING'

*!*	  Case lcRecvType $ 'OCL'
*!*	    Replace All GlSession With lcGlSession,;
*!*	      Tran_Desc With 'INTER LOC. RECEIVING'
*!*	  Otherwise
*!*	    Replace All GlSession With lcGlSession
*!*	  Endcase

*!*	  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcGlDist,lcTranCode,;
*!*	    SET("DataSession"),'Tran_No+Tran_Type+GlSession+Catg_Key','GLDIST')

*!*	  If lnConnectionHandlar # 1 .And. lnConnectionHandlar # 2
*!*	    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
*!*	    Return .F.
*!*	  Else
*!*	    =Tableupdate(.T.,.T.)
*!*	  Endif
*!*	Endif
*! E304047,1 MMT 07/25/2018 Modify Receiving program to update SQL GLDIST table[End]
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
If Inlist(lcRecvType,'O','L') And 'NC' $ oAriaApplication.CompanyInstalledModules
  =gfOpenFile(oAriaApplication.DataDir+'CODES',oAriaApplication.DataDir+'CODES','SH')
  Select CODES
  Set Order To Tag Idrltfname
  =Seek('NYCSITEID')
  Locate Rest While cdefcode+crltfield+cfld_name = 'NYCSITEID' ;
    FOR   cRltd_Nam = 'CCMSITETYP' And cRltd_Vlu= 'B'
  If Found()
    lcSiteId = CODES.cCode_No
    Select EDiAcPrt
    Locate For cSiteId = lcSiteId
    If Found() And Seek(EDiAcPrt.cPartCode+'852','EdiPd')
      lcDateKey = Dtos(ldRecDate)+'-'+Dtos(ldRecDate)
      Select EdiTrans
      If !Seek('852'+Padr(lcDateKey,40)+EDiAcPrt.Type+EDiAcPrt.cPartner)
        Insert Into ('EDITRANS') (CEDITRNTYP,Key,Type,cPartner,LINTERCOMP) ;
          VALUES ('852',lcDateKey,EDiAcPrt.Type,EDiAcPrt.cPartner,;
          EDiAcPrt.LINTERCOMP)
      Endif
      Replace cStatus With 'N'
    Endif
  Endif
  Set Order To Tag CODES In CODES
ENDIF

*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
*-- Commit updating FOX tables
*!*	lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode,.T.)
*!*	If lnConnectionHandlar # 1
*!*	  =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran",lnConnectionHandlar,.T.)
*!*	  Return .F.
*!*	Endif
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]

*------------------------------- End Updating Fox Tables -------------------------

*-- Begin transaction - Updating SQL tables
*B609297 TMI 06/14/2010 [Start]
**-- call the Garbag collection [start] tmi 6/3/2010
Wait Window Nowait ''
If Upper(Alltrim(oAriaApplication.DefaultCountry)) == 'ENG'
  =Sys(1104)
Endif
**-- call the Garbag collection [end  ] tmi 6/3/2010
*B609297 TMI 06/14/2010 [End  ]

*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
*lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
lcTranCodSQL = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]

*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
*If Type('lcTranCode') = 'N' 
*=oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode)
IF Type('lcTranCodSQL') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCodSQL)
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]
  Return .F.
Endif

*-- 1) Updating Itemloc (SQL)
If lcInvType = '0002'
  Select(lcItemLoc)

  *B609297 TMI 06/14/2010 [Start]
  **-- call the Garbag collection [start] tmi 6/3/2010
  Wait Window Nowait ''
  If Upper(Alltrim(oAriaApplication.DefaultCountry)) == 'ENG'
    =Sys(1104)
  Endif
  **-- call the Garbag collection [end  ] tmi 6/3/2010
  *B609297 TMI 06/14/2010 [End  ]

*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
 * lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcItemLoc,lcTranCode,
  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcItemLoc,lcTranCodSQL,;
    SET("DataSession"),'cInvType,Style,cWareCode,Dyelot','ITEMLOC','STYDYE')
  If lnConnectionHandlar # 1 .And. lnConnectionHandlar # 2
    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
   *E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]

*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
  * lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
    lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCodSQL)
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]

    Return .F.
  Else
    =Tableupdate(.T.,.T.)
  Endif

  *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
  Select(lcTempItem)

  *B609297 TMI 06/14/2010 [Start]
  **-- call the Garbag collection [start] tmi 6/3/2010
  Wait Window Nowait ''
  If Upper(Alltrim(oAriaApplication.DefaultCountry)) == 'ENG'
    =Sys(1104)
  Endif
  **-- call the Garbag collection [end  ] tmi 6/3/2010
  *B609297 TMI 06/14/2010 [End  ]

*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
  *lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcTempItem,lcTranCode,
   lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcTempItem,lcTranCodSQL,;
    SET("DataSession"),'cInvType,Style','ITEM','STYLE')
  If lnConnectionHandlar # 1 .And. lnConnectionHandlar # 2
    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
    *E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]

*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
  *lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
   lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCodSQL)
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]

    Return .F.
  Else
    =Tableupdate(.T.,.T.)
  Endif
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
Endif

*-- 3) Update master POSHDR (SQL)
Select(lcMastPoHd)

*B609297 TMI 06/14/2010 [Start]
**-- call the Garbag collection [start] tmi 6/3/2010
Wait Window Nowait ''
If Upper(Alltrim(oAriaApplication.DefaultCountry)) == 'ENG'
  =Sys(1104)
Endif
**-- call the Garbag collection [end  ] tmi 6/3/2010
*B609297 TMI 06/14/2010 [End  ]

lnRecNo = Recno()
Replace lLok_Stat With .F.
*B611745,1 Heba HMS, 18/03/2018 - Aria 5 - Error with purchase order [T20190313.0002 ][Begin]
gfAdd_Info(lcMastPoHd)
*B611745,1 Heba HMS, 18/03/2018 - Aria 5 - Error with purchase order [T20190313.0002 ][END]
*B000109,1 WAM 03/05/2005 Remove Lock PO header
*REPLACE ALL lLok_stat WITH .F. ,;
cLok_User WITH ''  ,;
dLok_Date WITH {}  ,;
cLok_Time WITH ''
*B000109,1 WAM 03/05/2005 (End)

*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
*lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcMastPoHd,lcTranCode,
lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcMastPoHd,lcTranCodSQL,;
  SET("DataSession"),'cBusDocu,cStyType,PO','POSHDR','POSHDR')
If lnConnectionHandlar # 1 .And. lnConnectionHandlar # 2
  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
 *E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]

*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
 *lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
  lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCodSQL)
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]
  Return .F.
Else
  =Tableupdate(.T.,.T.)
Endif



*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
LNTABLESTYINV = GFGETREMOTETABLE(SET("Datasession"),'STYINVJL')
IF LNTABLESTYINV <>  0 
  SELECT(OARIAAPPLICATION.LAREMOTETABLE[LNTABLESTYINV].LCCURSORUPDATE)
  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(OARIAAPPLICATION.LAREMOTETABLE[LNTABLESTYINV].LCCURSORUPDATE,lcTranCodSQL,;
  SET("DataSession"),'Oid','STYINVJL','USTYINVJL')
  If lnConnectionHandlar # 1 .And. lnConnectionHandlar # 2
    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
    Return .F.
  ELSE
    =Tableupdate(.T.,.T.) 
  ENDIF
ENDIF  
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]



*-- 4) Update master ShpmtHdr (SQL)
*-- 'S' Receive by Shipment, 'C' Receive Inter Location P/O Shipment
*-- 'F' Receive Material PO Shipment
If lcRecvType $ 'SFC' And Type("lcMastShp") = "C" And Used(lcMastShp)
  *B128070,1 KHM 05/19/2005 Initialize llChgStatus to indicate whether to change the
  *B128070,1                shipment status or not [Begin]
  Local llChgStatus
  llChgStatus = .F.
  *B128070,1 KHM 05/19/2005 [End]

  Select (lcPosLn)
  If lcRecvType $ 'SF'
    *B128070,1 KHM 05/19/2005 Comment the following line and check if there are still
    *B128070,1                         lines that have not been received yet [Begin]
    *LOCATE FOR ShipNo = lcShpCode AND TranCd = '3' AND TotQty > 0
    Local laShipQty
    Dimension laShipQty[8]
    Store 0 To laShipQty
    Set Order To Tag Poslnsh

    Select(lcMastPoLn)
    Set Order To Tag Poslnsh
    Seek lcShpcode
    Scan Rest While ShipNo+cBusDocu+cStyType+PO+cInvType+Style+Str(Lineno,6)+TranCd = ;
        lcShpcode For TranCd = '3'
      lcKeyVal = ShipNo+cBusDocu+cStyType+PO+cInvType+Style+Str(Lineno,6)
      laShipQty = 0
      If Seek(lcKeyVal+'2', lcPosLn) Or Seek(lcKeyVal+'4', lcPosLn) Or +;
          SEEK(lcKeyVal+'5', lcPosLn)
        Select (lcPosLn)
        Sum Rest Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 ;
          WHILE ShipNo+cBusDocu+cStyType+PO+cInvType+Style+Str(Lineno,6)+TranCd =;
          lcKeyVal For TranCd $ '245' To Array laShipQty
        For lnVal = 1 To 8
          lcVal = Str(lnVal,1)
          laShipQty[lnVal] = Max(Evaluate(lcMastPoLn+'.Qty'+lcVal) - laShipQty[lnVal],0)
        Endfor
        If laShipQty[1]+laShipQty[2]+laShipQty[3]+laShipQty[4]+laShipQty[5]+laShipQty[6]+;
            laShipQty[7]+laShipQty[8] > 0
          llChgStatus = .T.
          Exit
        Endif
      Else
        llChgStatus = .T.
        Exit
      Endif
    Endscan
    *B128070,1 KHM 05/19/2005 [End]
  Else
    Select(lcMastPoLn)
    Locate For ShipNo = lcShpcode And TranCd = '3' And TotQty > 0
    *B128070,1 KHM 05/19/2005 Use llChgStatus instead of FOUND() [Begin]
    llChgStatus = Found()

  Endif
  *B128070,1 KHM 05/19/2005 Use llChgStatus instead of FOUND() [Begin]
  *lcShpStat = IIF(FOUND(),'O','C')
  lcShpStat = Iif(llChgStatus,'O','C')
  *B128070,1 KHM 05/19/2005 [End]
  Select (lcMastShp)
  Locate
  *B128070,1 KHM 05/19/2005 Add the replacement of stock, damage and cancel quantity [Begin]
  *REPLACE Status     WITH lcShpStat
  Replace Status   With lcShpStat,;
    Recv_Stk With Recv_Stk + lnShipStk,;
    Recv_Dam With Recv_Dam + lnShipDam ,;
    Recv_Can With Recv_Can + lnShipCan
  *! E304063,1 ES 01/22/2019 Client wants to update the Shipment Warehouse date with the shipment actual receiving date. [Start].
  REPLACE dWareHous WITH IIF(Status="C",ldRecDate,dWareHous)
  *! E304063,1 ES 01/22/2019 Client wants to update the Shipment Warehouse date with the shipment actual receiving date. [End].

  *B607880,1 SSH 12/17/2006 [Begin] Replace TotQtyHdr WIth ZERO in case recieve completely
  If Status="C"
    Replace TotQtyHdr With 0
  ENDIF
  *B611953,1 ES 10/29/2019 Shipment is not marked as locked once user selected it in PO receiving screen [Start]
    loFormSet.recordLock(.F.)
  =gfAdd_Info(lcMastShp)
  *B611953,1 ES 10/29/2019 Shipment is not marked as locked once user selected it in PO receiving screen [End]
  
  
  *B607880,1 SSH 12/17/2006 Replace TotQtyHdr WIth ZERO in case recieve completely
  *B128070,1 KHM 05/19/2005 [End]

  *B609297 TMI 06/14/2010 [Start]
  **-- call the Garbag collection [start] tmi 6/3/2010
  Wait Window Nowait ''
  If Upper(Alltrim(oAriaApplication.DefaultCountry)) == 'ENG'
    =Sys(1104)
  Endif
  **-- call the Garbag collection [end  ] tmi 6/3/2010
  *B609297 TMI 06/14/2010 [End  ]

*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
 *lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcMastShp,lcTranCode,
  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcMastShp,lcTranCodSQL,; 
    SET("DataSession"),;
    'CBUSDOCU,CSHPTYPE,SHIPNO',;
    'SHPMTHDR','SHPMTHDR')
  If lnConnectionHandlar # 1 .And. lnConnectionHandlar # 2
    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
   *E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]

*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
   *lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
    lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCodSQL)
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]

    Return .F.
  Else
    =Tableupdate(.T.,.T.)
  Endif
Endif

*-- 5) Update master POSLN (SQL)
Select(lcPosLn)

*B609297 TMI 06/14/2010 [Start]
**-- call the Garbag collection [start] tmi 6/3/2010
Wait Window Nowait ''
If Upper(Alltrim(oAriaApplication.DefaultCountry)) == 'ENG'
  =Sys(1104)
Endif
**-- call the Garbag collection [end  ] tmi 6/3/2010
*B609297 TMI 06/14/2010 [End  ]

*B607585,1 AMH Add cwarecode to POSLN index and nLineNo to BOMLINE file [Start]
*lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcPosLn,lcTranCode,;
SET("DataSession"),;
'cBusDocu,cStyType,PO,cRSession,ShipNo,cInvType,Style,LineNo,TranCd,cStyGrade',;
'POSLN','POSREC')

*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
*lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcPosLn,lcTranCode,
lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcPosLn,lcTranCodSQL,;
  SET("DataSession"),;
  'cBusDocu,cStyType,PO,cRSession,ShipNo,cInvType,Style,LineNo,TranCd,cStyGrade,cWareCode',;
  'POSLN','POSREC')
*B607585,1 AMH [End]

If lnConnectionHandlar # 1 .And. lnConnectionHandlar # 2
  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
 *E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]

*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
  *lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
  lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCodSQL)
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]

  Return .F.
Else
  =Tableupdate(.T.,.T.)
Endif

*-- 6) Update master BomLine
*-- 'P' Material PO, 'F' Material PO Shipment
If !(lcRecvType $ 'PFG')
  Select(lcTmpBomLn)

  *B609297 TMI 06/14/2010 [Start]
  **-- call the Garbag collection [start] tmi 6/3/2010
  Wait Window Nowait ''
  If Upper(Alltrim(oAriaApplication.DefaultCountry)) == 'ENG'
    =Sys(1104)
  Endif
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

*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
  *lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcTmpBomLn,lcTranCode,
   lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcTmpBomLn,lcTranCodSQL,;
    SET("DataSession"),;
    'CIMTYP,CTYPE,CTKTNO,SHIPNO,LINENO,CBOMTYP,CINVTYPE,STYLE,CINVTYPC,'+;
    'ITEM,MFGCODE,CRSESSION,CSTYGRADE,NLINENO',;
    'BOMLINE','BOMLINEU')
  *B607585,1 AMH [End]

  If lnConnectionHandlar # 1 .And. lnConnectionHandlar # 2
    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
  *E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]

*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
    *lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
    lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCodSQL)
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]
   
    Return .F.
  Else
    =Tableupdate(.T.,.T.)
  Endif
Endif

*-- 7) Update master MfgOprDt
*-- 'P' Material PO, 'F' Material PO Shipment
If !(lcRecvType $ 'PFG') And Used(lcMastOprDt)
  Select(lcMastOprDt)
  Locate
  If !Eof()

    *B609297 TMI 06/14/2010 [Start]
    **-- call the Garbag collection [start] tmi 6/3/2010
    Wait Window Nowait ''
    If Upper(Alltrim(oAriaApplication.DefaultCountry)) == 'ENG'
      =Sys(1104)
    Endif
    **-- call the Garbag collection [end  ] tmi 6/3/2010
    *B609297 TMI 06/14/2010 [End  ]
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]

    *lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcMastOprDt,lcTranCode,
    lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcMastOprDt,lcTranCodSQL,;
      SET("DataSession"),;
      'CIMTYP,CTKTNO,COPRCODE,CLOTNO,TRANCD,CINVTYPE,ITEM,CDYELOT',;
      'MFGOPRDT','MFGOPRITME')
    If lnConnectionHandlar # 1 .And. lnConnectionHandlar # 2
      =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
     *E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]

*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
     *lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
      lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCodSQL)
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]

      Return .F.
    Else
      =Tableupdate(.T.,.T.)
    Endif
  Endif
Endif

*- this file to update the usage in the itemloc
If lcInvType = "0001" And Type('lcTmpItmDye') = 'C' And Used(lcTmpItmDye)
  Select(lcTmpItmDye)
  Locate
  If !Eof()

    *B609297 TMI 06/14/2010 [Start]
    **-- call the Garbag collection [start] tmi 6/3/2010
    Wait Window Nowait ''
    If Upper(Alltrim(oAriaApplication.DefaultCountry)) == 'ENG'
      =Sys(1104)
    Endif
    **-- call the Garbag collection [end  ] tmi 6/3/2010
    *B609297 TMI 06/14/2010 [End  ]
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]

    *lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcTmpItmDye,lcTranCode,
    lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcTmpItmDye,lcTranCodSQL,;
      SET("DataSession"),'cInvType,Style,cWareCode,Dyelot','ITEMLOC','STYDYE')
    If lnConnectionHandlar # 1 .And. lnConnectionHandlar # 2
      =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
    *E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]

*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
     *lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
      lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCodSQL)
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]
 
      Return .F.
    Else
      =Tableupdate(.T.,.T.)
    Endif
  Endif
Endif

*E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
If lcInvType = "0001" And Type('lcTmpItm') = 'C' And Used(lcTmpItm)
  Select(lcTmpItm)
  Locate
  If !Eof()

    *B609297 TMI 06/14/2010 [Start]
    **-- call the Garbag collection [start] tmi 6/3/2010
    Wait Window Nowait ''
    If Upper(Alltrim(oAriaApplication.DefaultCountry)) == 'ENG'
      =Sys(1104)
    Endif
    **-- call the Garbag collection [end  ] tmi 6/3/2010
    *B609297 TMI 06/14/2010 [End  ]
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
    *lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcTmpItm,lcTranCode,
     lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcTmpItm,lcTranCodSQL,;
      SET("DataSession"),'cInvType,Style','ITEM','STYLE')
    If lnConnectionHandlar # 1 .And. lnConnectionHandlar # 2
      =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
      *E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]

*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
      *lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
      lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCodSQL)
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]

      Return .F.
    Else
      =Tableupdate(.T.,.T.)
    Endif
  Endif
Endif
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

*! E304047,1 MMT 07/25/2018 Modify Receiving program to update SQL GLDIST table[Start]
*-- 8) Updating the GLDIST file (SQL)
If llLinkToGl
  If !loFormSet.llSilentMod
    Wait Window Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_UPDATEGL,loFormSet.GetHeaderText("LANG_POSTREC_UPDATEGL",loFormSet.HeaderAlias)) Nowait
  Endif

  Select (lcGlDist)
  Do Case
  Case lcInvType = '0002' And lcRecvType $ 'PF'
    Replace All GlSession With lcGlSession,;
      Tran_Desc With 'MAT. P/O RECEIVING'
  Case lcInvType = '0002' And lcRecvType = 'W'
    Replace All GlSession With lcGlSession,;
      Tran_Desc With 'RECEIVE M.F.G. ORDER'
  Case lcInvType = '0002' And lcRecvType = 'G'
    Replace All GlSession With lcGlSession,;
      Tran_Desc With 'MAT. P/O ISSUING'
  Case lcRecvType $ 'OCL'
    Replace All GlSession With lcGlSession,;
      Tran_Desc With 'INTER LOC. RECEIVING'
  Otherwise
    Replace All GlSession With lcGlSession
  Endcase

*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
 *lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcGlDist,lcTranCode,
  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcGlDist,lcTranCodSQL,;
    SET("DataSession"),'Tran_No+Tran_Type+GlSession+Catg_Key','GLDIST')
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]

  If lnConnectionHandlar # 1 .And. lnConnectionHandlar # 2
    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
    Return .F.
  Else
    =Tableupdate(.T.,.T.)
  Endif
Endif
*! E304047,1 MMT 07/25/2018 Modify Receiving program to update SQL GLDIST table[End]

Wait Clear

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
Wait Window Nowait ''
If Upper(Alltrim(oAriaApplication.DefaultCountry)) == 'ENG'
  =Sys(1104)
Endif
**-- call the Garbag collection [end  ] tmi 6/3/2010
*B609297 TMI 06/14/2010 [End  ]


*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
*lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
lnConnectionHandlarSQL = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCodSQL)
If lnConnectionHandlarSQL # 1
  =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran",lnConnectionHandlarSQL,.T.)
  Return .F.
Endif
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode,.T.)
If lnConnectionHandlar # 1
  =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran",lnConnectionHandlar,.T.)
  Return .F.
Endif



*B609297 TMI 06/14/2010 [Start]
**-- call the Garbag collection [start] tmi 6/3/2010
Wait Window Nowait ''
If Upper(Alltrim(oAriaApplication.DefaultCountry)) == 'ENG'
  =Sys(1104)
Endif
**-- call the Garbag collection [end  ] tmi 6/3/2010
*B609297 TMI 06/14/2010 [End  ]

If lnConnectionHandlar # 1
  =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran",lnConnectionHandlar)
  Return .F.
Endif
*--------------------------- End updating Master files ---------------------------------
*! E611838,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
IF llLinkToGl
  =gfCreateGLEntries(lcGlDist,'')
ENDIF
*! E611838,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]
Wait Clear
*C131527,1 KHM

*B609295,1 TMI 06/14/2010 [Start] use "loFormSet.mDoTrigger" instead the old function gfDotriger to call the POTRAN for Blum & Fink
*!*	=gfDoTriger('POSTREC',PADR('POTRAN',10))
If Ascan(loFormSet.laEvntTrig,Padr('POTRAN',10),1,Alen(loFormSet.laEvntTrig,1),1) > 0
  =loFormSet.mDoTrigger(Padr('POTRAN',10))
Endif
*B609295,1 TMI 06/14/2010 [End  ]

*! C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
If Ascan(loFormSet.laEvntTrig,Padr('PRCSPUPREF',10),1,Alen(loFormSet.laEvntTrig,1),1) > 0
  Public oPoRecRef
  oPoRecRef = loFormSet
Endif
*! C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]

*!C201141, 1 Hesham Elmasry(HES), 04/26/2009, Calling Cost Sheet Sheet Screen to complete the Issuing Process [Start]
If Ascan(loFormSet.laEvntTrig,Padr('CALCSSHSCR',10),1,Alen(loFormSet.laEvntTrig,1),1) > 0
  loFormSet.mDoTrigger(Padr('CALCSSHSCR' ,10))
Endif
*!C201141, 1 Hesham Elmasry(HES), 04/26/2009, Calling Cost Sheet Sheet Screen to complete the Issuing Process [End]

*!* E303103,2 MMT 04/05/2012 Add trigger in PO receiving screen for [Start]
If Ascan(loFormSet.laEvntTrig,Padr('AUTALLOC',10),1,Alen(loFormSet.laEvntTrig,1),1) > 0
  =loFormSet.mDoTrigger(Padr('AUTALLOC',10))
Endif
*!* E303103,2 MMT 04/05/2012 Add trigger in PO receiving screen for [END]



*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
Release laSetups  , llWareHous, llWareLoc, llDyelot, llFabDye, lcCostMthM, lcCostMth,;
  llLinkToGl, lcDropLoc , llPOSale  , llImpCost, llMulCurr, llUseMCurr,;
  llEditExRt, llConfig  , llMFCall, llUnblProc, llShpRec
Release  lcGLFYear, lcGLPeriod
Release lcVendFile, lcMastPoHd, lcMastPoLn, lcPosLn, lcMastBomLn, lcTmpBomLn, lcTempItem, lcItemLoc,;
  lcTmpCur, lcGlDist, lcGlSession,  lcMastOprDt, lcTmpCode, lcTmpItmJrl, lcTmpItmDye, lcTmpItm
Release laOpnQty,lnLstPoRAm,lnOpnPoAmt,lcLstTrn,lcPoVend,lcOrjWareH,;
  lcLotNo, lcChkPO, lcBomPTyp, lcBomPKey,;
  laReceive, lcCstShtTyp, lcPriceCur,lcDutyCur,lnConvFact,laPrevRecQ
Release llMscale ,lcTmpBom
Release lcSqlStatement,laIndex, m.VENDOR
Release llStkLine,lcMainKy,lcOrjWareH
Release lnNewCost,lnELanded,lnOrgCost,lcStyle,lcWareHous,lcDyelot   ,lclinkCode
Release lcGLPLkC,lcCurSty,llFound
Release lcWipSgn ,lcCntI ,lnCnI
Release lnOpnSub,lnI,lcCnt
Release lcFrstSess,lcBomLKey
Release lcWhileCn , m.StyQty,m.ItemQty,m.ItemAmt, m.cRSession,lcLastOpr
Release laSHPQty,laRecvQty,lcShpLine ,laAcLRQy,I,Z,lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8
Release laGLDistAr,lcJTType,lcJrlSty,lcJDyelt,lnJSgn,laAdjust,lcRefer
Release lcCstShtType, lnNxtStp
Release laCosts,laECosts,laCurrency
Release laOtherPar,lnCount,lnIELanCost,lnIEStnCost
Release lcGlCatg,lnConnectionHandlar, lcTranCode
Release lnRecNo, llChgStatus ,laShipQty,lcKeyVal ,lcShpStat ,lnResult
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
Function lfGetStruc
Lparameters lcInvType

Private lcSqlStatement, laIndex

*! B609051,1 HES 10/20/2009 error receiving PO receipts because there is no index for the STYDYE file [Start]
If !Used('STYDYE')
  =gfOpenTable(oAriaApplication.DataDir+'STYDYE',oAriaApplication.DataDir+'STYDYE','SH')
Else
  Select STYDYE
  gfSetOrder('STYDYE')
Endif
*! B609051,1 HES 10/20/2009 error receiving PO receipts because there is no index for the STYDYE file [End]

If lcInvType = "0001"
  *-- To get the structure of the style file
  lcSqlStatement  = "SELECT * FROM STYLE WHERE 1 = 2"
  =lfOpenFox(lcSqlStatement,'STYLE',lcTempItem,"")
  Dimension laIndex[1,2]
  laIndex = ''
  laIndex[1,1] = 'Style'
  laIndex[1,2] = lcTempItem
  =lfSetIndex(lcTempItem,@laIndex)

  *-- To get the structure of the stydye file
  lcSqlStatement  = "SELECT * FROM STYDYE WHERE 1 = 2"
  =lfOpenFox(lcSqlStatement,'STYDYE',lcItemLoc,"")
  Dimension laIndex[1,2]
  laIndex = ''
  laIndex[1,1] = 'Style+cWareCode+Dyelot'
  laIndex[1,2] = lcItemLoc
  =lfSetIndex(lcItemLoc,@laIndex)
Else
  *-- To get the structure of the style file
  lcSqlStatement  = "SELECT TOP 0 * FROM ITEM [INDEX=STYLE]"
  =lfOpenSql(lcSqlStatement,'ITEM',lcTempItem,"","",.F.)
  Dimension laIndex[1,2]
  laIndex = ''
  laIndex[1,1] = 'Style'
  laIndex[1,2] = lcTempItem
  =lfSetIndex(lcTempItem,@laIndex)

  *-- To get the structure of the itemloc file
  lcSqlStatement  = "SELECT TOP 0 * FROM ITEMLOC [INDEX=STYDYE]"
  =lfOpenSql(lcSqlStatement,'ITEMLOC',lcItemLoc, "","",.F.)
  Dimension laIndex[1,2]
  laIndex = ''
  laIndex[1,1] = 'cInvType+Style+cWareCode+Dyelot'
  laIndex[1,2] = lcItemLoc
  =lfSetIndex(lcItemLoc,@laIndex)

  *-- To get the structure of the item journal file
  lcSqlStatement  = "SELECT TOP 0 * FROM ITEMJRNL [INDEX=STYINVJL]"
  =lfOpenSql(lcSqlStatement,'ITEMJRNL',lcTmpItmJrl, "","",.F.)
  Dimension laIndex[1,2]
  laIndex = ''
  laIndex[1,1] = 'cInvType+Style+cWareCode+DTOS(dTrDate)+cSession+cIrType+cTrCode+STR(LineNo,6)'
  laIndex[1,2] = lcTmpItmJrl
  =lfSetIndex(lcTmpItmJrl,@laIndex)

Endif
*-- To get the structure of the POSHDR file
lcSqlStatement  = "SELECT TOP 0 * FROM POSHDR [INDEX=POSHDR]"
=lfOpenSql(lcSqlStatement,'POSHDR',lcMastPoHd, "","",.F.)
Dimension laIndex[1,2]
laIndex = ''
laIndex[1,1] = 'cBusDocu+cStyType+PO'
laIndex[1,2] = lcMastPoHd
=lfSetIndex(lcMastPoHd,@laIndex)

*-- To get the structure of the POSLN file
lcSqlStatement  = "SELECT TOP 0 * FROM POSLN [INDEX=POSLN]"
=lfOpenSql(lcSqlStatement,'POSLN',lcPosLn, "","",.F.)
Dimension laIndex[2,2]
laIndex = ''
laIndex[1,1] = 'CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD'
laIndex[1,2] = lcPosLn
laIndex[2,1] = 'SHIPNO+CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD'
laIndex[2,2] = 'Poslnsh'
=lfSetIndex(lcPosLn,@laIndex)

*-- To get the structure of the vendor file
lcSqlStatement  = "SELECT * FROM APVENDOR WHERE 1 = 2"
=lfOpenFox(lcSqlStatement,'APVENDOR',lcVendFile,"")
Dimension laIndex[1,2]
laIndex = ''
laIndex[1,1] = 'cVendCode'
laIndex[1,2] = lcVendFile
=lfSetIndex(lcVendFile,@laIndex)

*-- To Get the structure of the GLDIST file
If llLinkToGl
  lcSqlStatement  = "SELECT * FROM GLDIST WHERE 1 = 2"
  *! E304047,1 MMT 07/25/2018 Modify Receiving program to update SQL GLDIST table[Start]
  *=lfOpenFox(lcSqlStatement,'GLDist',lcGlDist,"")  
  =lfOpenSql(lcSqlStatement,'GLDist',lcGlDist,"")
  *! E304047,1 MMT 07/25/2018 Modify Receiving program to update SQL GLDIST table[End]
Endif

*-- To get the structure of the BOMLINE file
lcSqlStatement  = "SELECT TOP 0 * FROM BOMLINE [INDEX=BOMLINE]"
=lfOpenSql(lcSqlStatement,'BOMLINE',lcTmpBomLn, "","",.F.)
Dimension laIndex[2,2]
laIndex = ''
laIndex[1,1] = 'CIMTYP+CTYPE+CTKTNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE'
laIndex[1,2] = 'BomLine'
laIndex[2,1] = 'CIMTYP+CTYPE+SHIPNO+CTKTNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE'
laIndex[2,2] = 'BOMLNSHP'
=lfSetIndex(lcTmpBomLn,@laIndex)

*-- To get the structure of the BOMLINE file
lcSqlStatement  = "SELECT TOP 0 * FROM BOMLINE [INDEX=BOMLINE]"
=lfOpenSql(lcSqlStatement,'BOMLINE',lcTmpBomLn, "","",.F.)
Dimension laIndex[2,2]
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
Dimension laIndex[1,2]
laIndex = ''
laIndex[1,1] = 'CIMTYP+CTKTNO+COPRCODE+CLOTNO+TRANCD'
laIndex[1,2] = lcMastOprDt
=lfSetIndex(lcMastOprDt,@laIndex)

*B607935,1 WAM 01/17/2007 Create temp item file
*-- To be used in case of need to update the usage.
lcSqlStatement  = "SELECT TOP 0 * FROM ITEM [INDEX=STYLE]"
=lfOpenSql(lcSqlStatement,'ITEM',lcTmpItm,"","",.F.)
Dimension laIndex[1,2]
laIndex = ''
laIndex[1,1] = 'cInvType+Style'
laIndex[1,2] = lcTmpItm
=lfSetIndex(lcTmpItm,@laIndex)
*B607935,1 WAM 01/17/2007 (End)

*-- To be used in case of need to update the usage.
lcSqlStatement  = "SELECT TOP 0 * FROM ITEMLOC [INDEX=STYDYE]"
=lfOpenSql(lcSqlStatement,'ITEMLOC',lcTmpItmDye, "","",.F.)
Dimension laIndex[1,2]
laIndex = ''
laIndex[1,1] = 'cInvType+Style+cWareCode+Dyelot'
laIndex[1,2] = lcTmpItmDye
=lfSetIndex(lcTmpItmDye,@laIndex)
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
Release lcSqlStatement, laIndex
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
Function lfUpdItmFl
*E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
*LPARAMETERS lcFile,  lcField, lcOperator, laRecQty, laOpnQty
Lparameters lcFile,  lcField, lcOperator, laRecQty, laOpnQty, llTotalOnly
*E039550,1 WSH 08/07/2005 [End]

Private lnAlias, lcTotFld
lnAlias  = Select(0)
Select(lcFile)

*E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
If !llTotalOnly
  *E039550,1 WSH 08/07/2005 [End]

  Replace &lcField.1  With &lcField.1 &lcOperator Min(laRecQty[1] , laOpnQty[1]) ,;
    &lcField.2  With &lcField.2 &lcOperator Min(laRecQty[2] , laOpnQty[2]) ,;
    &lcField.3  With &lcField.3 &lcOperator Min(laRecQty[3] , laOpnQty[3]) ,;
    &lcField.4  With &lcField.4 &lcOperator Min(laRecQty[4] , laOpnQty[4]) ,;
    &lcField.5  With &lcField.5 &lcOperator Min(laRecQty[5] , laOpnQty[5]) ,;
    &lcField.6  With &lcField.6 &lcOperator Min(laRecQty[6] , laOpnQty[6]) ,;
    &lcField.7  With &lcField.7 &lcOperator Min(laRecQty[7] , laOpnQty[7]) ,;
    &lcField.8  With &lcField.8 &lcOperator Min(laRecQty[8] , laOpnQty[8])

  *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
Endif
*E039550,1 WSH 08/07/2005 [End]

Do Case
Case Upper(lcField) = "INTRANS"
  lcTotFld = "TotIntrn"
Case Upper(lcField) = "WIP"
  lcTotFld = "TotWip"
Case Upper(lcField) = "NWO"
  lcTotFld = "nTotWO"
Case Upper(lcField) = "STK"
  lcTotFld = "TOTSTK"
Case Upper(lcField) = "NONRET"
  lcTotFld = "NTOTONRET"

Endcase

*E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
*REPLACE &lcTotFld WITH &lcField.1+&lcField.2+&lcField.3+&lcField.4+&lcField.5+&lcField.6+;
&lcField.7+&lcField.8
Local lnTotal, lnI
lnTotal = 0

For lnI = 1 To 8
  lnTotal = lnTotal + Min(laRecQty[lnI] , laOpnQty[lnI])
Endfor

Replace (lcTotFld) With Evaluate(lcTotFld) &lcOperator lnTotal
*E039550,1 WSH 08/07/2005 [End]

Select(lnAlias)

*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
Release lnAlias, lcTotFld,lnTotal, lnI
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]

*!***************************************************************************
*! Name      : lfAddNewLn
*! Developer : Khalid Mohi El-Din
*! Date      : 09/11/2004
*! Purpose   : To prepare the lines for updating. If a new line added to the PO
*!             update the Bill of Materials
*!******************************************************************************
Function lfAddNewLn

*B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[Start]
If Used('Scale')
  Dimension laIndex[1,2]
  laIndex[1,1] = 'TYPE+SCALE'
  laIndex[1,2] = 'Scale'
  *T20071102.0018,10/C200876 TMI 09/11/2008 [Start]
  If Upper(oAriaApplication.DataDir)$Dbf('SCALE')
    Select Scale
    =gfSetOrder('SCALE')
  Else
    *T20071102.0018,10/C200876 TMI 09/11/2008 [Start]

    lfSetIndex('Scale',@laIndex)

    *T20071102.0018,10/C200876 TMI 09/11/2008 [Start]
  Endif
  *T20071102.0018,10/C200876 TMI 09/11/2008 [End  ]
Endif


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

If !Used(lcMFGOprHdAlias)
  =gfOpenTable('MFGOprHd','mfgoprhd',"SH",lcMFGOprHdAlias)
Endif


If !Used(lcBomAlias)
  =gfOpenTable('Bom','multibom',"SH",lcBomAlias)
Endif

If !Used(lcBomLineAlias)
  =gfOpenTable('BomLINE','BomLine',"SH",lcBomLineAlias)
Endif


If !Used(lcPoshdrAlias)
  =gfOpenTable('POSHDR','POSHDR',"SH",lcPoshdrAlias)
Endif
*! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [Start]
If !Used(lcTmpPoshdr)
  =gfOpenTable('POSHDR','POSHDR',"SH",lcTmpPoshdr)
Endif
*! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [End]
If !Used(lcPoslnAlias)
  =gfOpenTable('POSLN','POSLN',"SH",lcPoslnAlias)
Endif

If !Used(lcCtktBomAlias)
  =gfOpenTable('CtktBom','CtktBom',"SH",lcCtktBomAlias)
Endif
lcCstShtType = Iif(loFormSet.lcPType $ 'ISB','I',Iif(loFormSet.lcPType $ 'MT','M',""))
*B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[End]


*--Update For new added P/o Lines.....
Select (lcTmpLine)
Scan For TranCd = '1'
  If !lNewLn
    Loop
  Endif

  *B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[Start]
  *! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [Start]
  *!*	  =gfsqlrun("SELECT  BOM.* FROM BOM INNER JOIN BOMHEADR ON  BOMHEADR.cinvtype = BOM.cinvtype  AND  BOMHEADR.cCstShtTyp = BOM.cCstShtTyp AND "+;
  *!*	            " BOMHEADR.cItmMajor = BOM.cItmMajor AND BOMHEADR.ldefcstsht = 1 WHERE BOM.cinvtype = '"+lcInvType+"' AND BOM.cItmMajor = '" + ;
  *!*	            SUBSTR(Style,1,loFormSet.lnMjrWid)+ "'" +;
  *!*	            " AND BOM.cCstShtTyp ='" + lcCstShtType + "'",lcBomAlias)
  If !Seek(Iif(lcRecvType $'RD',lcPType,'P')+'P'+&lcTmpLine..PO,lcPoshdrAlias)
    =gfSEEK(Iif(lcRecvType $'RD',lcPType,'P')+'P'+&lcTmpLine..PO,lcPoshdrAlias)
  Endif
  If !Seek(Iif(lcRecvType $'RD',lcPType,'P')+'P'+&lcTmpLine..PO,lcTmpPoshdr)
    =gfSEEK(Iif(lcRecvType $'RD',lcPType,'P')+'P'+&lcTmpLine..PO,lcTmpPoshdr)
  Endif



  Select (lcTmpLine)
  =gfsqlrun("SELECT  BOM.* FROM BOM INNER JOIN BOMHEADR ON  BOMHEADR.cinvtype = BOM.cinvtype  AND  BOMHEADR.cCstShtTyp = BOM.cCstShtTyp AND "+;
    " BOMHEADR.cItmMajor = BOM.cItmMajor  and  BOMHEADR.ccstsht_id =  BOM.ccstsht_id  WHERE BOM.cinvtype = '"+lcInvType+"' AND BOM.cItmMajor = '" + ;
    SUBSTR(Style,1,loFormSet.lnMjrWid)+ "'" +;
    " AND BOM.cCstShtTyp ='" + lcCstShtType + "' AND BOMHEADR.ldefcstsht = 1",lcBomAlias)
  Select(lcBomAlias)
  *B610904,1 MMT 11/06/2014 Error in PO receiving screen while using scanned batch option[T20141029.0023][Start]
  *LOCATE REST WHILE CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6) =lcInvType+PADR(SUBSTR(&lcTmpLine..STYLE,1,loFormSet.lnMjrWid),19)+lcCstShtType FOR CCATG
  Locate Rest While cInvType+cItmMajor+cCstShtTyp+cCstSht_Id+TYP+CITMMASK+MFGCODE+CINVTYPC+Item+Str(nLineNo,6) =lcInvType+Padr(Substr(&lcTmpLine..Style,1,loFormSet.lnMjrWid),19)+lcCstShtType For CCATGTYP = 'P' And CCURRCODE = &lcPoshdrAlias..cPriceCur
  *B610904,1 MMT 11/06/2014 Error in PO receiving screen while using scanned batch option[T20141029.0023][END]
  If !Found()
    Select (lcTmpLine)
    =gfsqlrun("SELECT  BOM.* FROM BOM INNER JOIN BOMHEADR ON  BOMHEADR.cinvtype = BOM.cinvtype  AND  BOMHEADR.cCstShtTyp = BOM.cCstShtTyp AND "+;
      " BOMHEADR.cItmMajor = BOM.cItmMajor and  BOMHEADR.ccstsht_id =  BOM.ccstsht_id  WHERE BOM.cinvtype = '"+lcInvType+"' AND BOM.cItmMajor = '" + ;
      SUBSTR(Style,1,loFormSet.lnMjrWid)+ "'" +;
      " AND BOM.cCstShtTyp ='" + lcCstShtType + "' AND BOMHEADR.ccstsht_id IN (Select Top 1 ccstsht_id from BOM WHERE BOM.cinvtype = '"+lcInvType+"' AND BOM.cItmMajor = '" + ;
      SUBSTR(Style,1,loFormSet.lnMjrWid)+ "'" +;
      " AND BOM.cCstShtTyp ='" + lcCstShtType + "' AND ccatgtyp ='P' AND CCURRCODE ='"+&lcPoshdrAlias..cPriceCur +"')",lcBomAlias)
  Endif
  *! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [End]
  Select (lcTmpLine)
  *B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[End]


  Scatter Memvar

  *B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[Start]
  *=SEEK(IIF(lcRecvType $'RD',lcPType,'P')+&lcTmpLine..Po,'POSHDR')
  m.cCstSht_Id = &lcBomAlias..cCstSht_Id
  If !Seek(Iif(llMFCall,'M','I')+&lcTmpLine..PO,lcMFGOprHdAlias)
    gfSEEK(Iif(llMFCall,'M','I')+&lcTmpLine..PO,lcMFGOprHdAlias)
  Endif
  If !Seek(Iif(lcRecvType $'RD',lcPType,'P')+'P'+&lcTmpLine..PO,lcPoshdrAlias)
    =gfSEEK(Iif(lcRecvType $'RD',lcPType,'P')+'P'+&lcTmpLine..PO,lcPoshdrAlias)
  Endif
  *! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [Start]
  If !Seek(Iif(lcRecvType $'RD',lcPType,'P')+'P'+&lcTmpLine..PO,lcTmpPoshdr)
    =gfSEEK(Iif(lcRecvType $'RD',lcPType,'P')+'P'+&lcTmpLine..PO,lcTmpPoshdr)
  Endif
  *! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [End]
  *B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[End]

  If (llImpCost And lcRecvType <>'R') And !lfUpdBom('A')
    Select (lcTmpLine)
    Loop
  Endif

  *B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[Start]
  *SELECT POSLN
  *APPEND BLANK
  *GATHER MEMVAR
  *REPLACE cOwner WITH ' '
  Select (lcTmpLine)
  =gfAdd_Info(lcTmpLine)
  Scatter Memvar
  Select(lcPoslnAlias)
  gfAppend('',.T.)
  gfReplace("cOwner WITH ' '")
  *B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[End]


  *B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[Start]
  *!*    SELECT POSHDR
  *!*    =RLOCK()
  *!*    REPLACE LastLine   WITH POSLN.LineNo
  *!*    UNLOCK
  Select(lcPoshdrAlias)
  gfReplace ("LastLine   WITH "+Iif(LastLine >&lcPoslnAlias..Lineno,Str(LastLine,6), Str(&lcPoslnAlias..Lineno,6))+"")
  *B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[End]

  Select (lcTmpLine)
  Replace lNewLn With .F.
Endscan

*B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[Start]
Select(lcPoshdrAlias)
gfTableUpdate()
Select(lcPoslnAlias)
gfTableUpdate()
Select(lcBomLineAlias)
Scan
  gfReplace('')
Endscan
gfTableUpdate()
Select(lcCtktBomAlias)
Scan
  gfReplace('')
Endscan
gfTableUpdate()
Select(lcMFGOprHdAlias)
Scan
  gfReplace('')
Endscan
gfTableUpdate()
*B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[End]

*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
Release laIndex,lcTmpFileNam,lcPoshdrAlias   ,lcPoslnAlias    ,lcBomAlias      ,lcBomLineAlias  ,lcCtktBomAlias  ,;
  lcMFGOprHdAlias ,lcCstShtType
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]

Return

*!***************************************************************************
*! Name      : lfUpdIntCmp
*! Developer : Khalid Mohi El-Din
*! Date      : 09/11/2004
*! Purpose   : To update Issue Inter Location Purchase Order.
*!******************************************************************************
*! Parameters: lcMastShp : Shipment file in case of issue shipment
*!******************************************************************************
Function lfUpdIntCmp
Lparameters lcMastShp
Private lcSqlStatement, lcStyle, lcWareHous, lcDyelot, lcVendor,;
  lcBusDocu, lcStyType, lcPoNo, laReceive, laOpen, laStkQty
Local lcChkPO, lcSysType
Dimension laReceive[8], laOpen[8], laStkQty[8]
Store 0 To laReceive, laOpen, laStkQty
lcChkPO = Space(6)
*-- Send Inter Location PO or Purchase orders to sites
If !llFromEdi And 'NC' $ oAriaApplication.CompanyInstalledModules
  =gfOpenFile(oAriaApplication.DataDir+'EDIACPRT',oAriaApplication.DataDir+'ACCFACT','SH')
  =gfOpenFile(oAriaApplication.DataDir+'EDIPD',oAriaApplication.DataDir+'PARTTRANS','SH')
  =gfOpenFile(oAriaApplication.DataDir+'EDITRANS',oAriaApplication.DataDir+'TYPEKEY','SH')
  lcSysType = gfGetMemVar('M_SYSTYPE')
  If lcSysType = 'P'
    =gfOpenFile(oAriaApplication.DataDir+'CODES',gcDataDir+'Idrltfname','SH')
    Select CODES
    =Seek('N'+'Y'+Padr('CSITEID',10))
    Locate Rest While cdefcode+crltfield+cfld_name = 'N'+'Y'+Padr('CSITEID',10) ;
      FOR   cRltd_Nam = 'CCMSITETYP' And cRltd_Vlu = 'B'
    Select EDiAcPrt
    Locate For cSiteId = CODES.cCode_No
    lcBackSite = cSiteId
    lcBackAcc  = cPartner
  Endif
Endif

Select (lcTmpLine)
*--PO+Style+Dyelot+cWareCode+STR(LineNo,6)+cCarton+TranCd
Set Order To Tag TmpLine3

*!*	*C102358,5 AMH Add trigger to custom issue adornment order for JL [Start]
*!*	IF !llFromEdi AND lcRecvType = 'A' AND ASCAN(laEvntTrig , PADR('UPDISSAD',10)) <> 0
*!*	  =gfDoTriger('PORCVAP',PADR('UPDISSAD',10))
*!*	ENDIF
*!*	*C102358,5 AMH [End]

Select (lcTmpLine)
Go Top
Scan For TranCd <> '1' And TotQty <> 0
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
  *WAIT WINDOW 'Posting all transactions ... Style :'+Style NOWAIT
  If !loFormSet.llSilentMod
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *WAIT WINDOW 'Posting all transactions ... Style :'+STYLE NOWAIT
    Wait Window Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_POSTING,loFormSet.GetHeaderText("LANG_POSTREC_POSTING",loFormSet.HeaderAlias))+' '+;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_STYLE,loFormSet.GetHeaderText("LANG_POSTREC_MSG_STYLE",loFormSet.HeaderAlias))+' :'+Style Nowait
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
  Endif
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

  lcBusDocu  = cBusDocu
  lcStyType  = cStyType
  lcPoNo     = PO
  lcStyle    = Style
  lcWareHous = cWareCode
  lcDyelot   = Dyelot
  lcVendor   = Padr(VENDOR,6)

  *-- Get item information from style file
  *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
  *lcSqlStatement = "SELECT * FROM STYLE WHERE Style = '" + lcStyle + "'"
  *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
  *lcSqlStatement = "SELECT * FROM STYLE WHERE Style = [" + lcStyle + "]"
  lcSelStyVal =  lcStyle
  lcSqlStatement = "SELECT * FROM STYLE WHERE Style = ?m.lcSelStyVal "
  *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
  *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
  =lfGetItmInf(lcInvType,lcStyle,lcTempItem,'STYLE',lcSqlStatement)
  =Seek(lcStyle,lcTempItem)

  *-- Get item information from stydye file for the target location
  *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
  *!*	  lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
  *!*	    lcStyle+lcWareHous+SPACE(10) + "'"
  *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
  *!*	  lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
  *!*	    lcStyle+lcWareHous+SPACE(10) + "]"
  lcStyValueSelect = lcStyle+lcWareHous+Space(10)
  lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = ?m.lcStyValueSelect "
  *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
  *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
  =lfGetItmInf(lcInvType,lcStyle+lcWareHous+Space(10),lcItemLoc,'STYDYE',lcSqlStatement)
  =Seek(lcStyle+lcWareHous+Space(10),lcItemLoc)

  *-- IF not empty dyelot get the dyelot record for the target location.
  If !Empty(lcDyelot)
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
    *!*	    lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
    *!*	      lcStyle+lcWareHous+lcDyelot + "'"
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
    *!*	    lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
    *!*	      lcStyle+lcWareHous+lcDyelot + "]"
    lcStySelValue = lcStyle+lcWareHous+lcDyelot
    lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = ?m.lcStySelValue "
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
    =lfGetItmInf(lcInvType,lcStyle+lcWareHous+lcDyelot,lcItemLoc,'STYDYE',lcSqlStatement)
    =Seek(lcStyle+lcWareHous+lcDyelot,lcItemLoc)
  Endif

  *-- Get item information from stydye file for the sourc location
  *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
  *!*	  lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
  *!*	    lcStyle+lcVendor+SPACE(10) + "'"
  *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
  *!*	  lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
  *!*	    lcStyle+lcVendor+SPACE(10) + "]"
  lcSelStyValue = lcStyle+lcVendor+Space(10)
  lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = ?m.lcSelStyValue"
  *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
  *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
  =lfGetItmInf(lcInvType,lcStyle+lcVendor+Space(10),lcItemLoc,'STYDYE',lcSqlStatement)
  =Seek(lcStyle+lcVendor+Space(10),lcItemLoc)

  *-- IF not empty dyelot get the dyelot record for the source location.
  If llDyelot And !Empty(lcDyelot) And &lcTempItem..cDye_Flg='Y'
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
    *!*	    lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
    *!*	      lcStyle+lcVendor+lcDyelot + "'"
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
    *!*	    lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
    *!*	      lcStyle+lcVendor+lcDyelot + "]"
    lcStySelV = lcStyle+lcVendor+lcDyelot
    lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = ?m.lcStySelV "
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
    =lfGetItmInf(lcInvType,lcStyle+lcVendor+lcDyelot,lcItemLoc,'STYDYE',lcSqlStatement)
    =Seek(lcStyle+lcVendor+lcDyelot,lcItemLoc)
  Endif

  *-- Get the information of the POSHDR
  If lcPoNo <> lcChkPO
    lcSqlStatement = "SELECT * FROM POSHDR [INDEX=POSHDR] "+;
      "WHERE cBusDocu = '" + lcBusDocu + "' AND cStyType = '" + lcStyType +;
      "' AND PO = '" + lcPoNo + "'"
    =lfGetItmInf(lcInvType,lcBusDocu+lcStyType+lcPoNo,lcMastPoHd,'POSHDR',lcSqlStatement,.T.)
    =Seek(lcBusDocu+lcStyType+lcPoNo,lcMastPoHd)
    *-- Issue inter-location PO shipment
    If lcRecvType  = 'U'
      *-- Get the master records from POSLN file
      lcSqlStatement  = "SELECT * FROM POSLN [INDEX=POSLN] "+;
        "WHERE cBusDocu = '" + &lcTmpLine..cBusDocu + ;
        "' AND cStyType ='" + &lcTmpLine..cStyType  + ;
        "' AND PO ='" + &lcTmpLine..PO + "' AND cInvType ='" + lcInvType + "'"
      =lfOpenSql(lcSqlStatement,'POSLN',lcMastPoLn)
      Dimension laIndex[2,2]
      laIndex = ''
      laIndex[1,1] = 'CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD'
      laIndex[1,2] = lcMastPoLn
      laIndex[2,1] = 'SHIPNO+CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD'
      laIndex[2,2] = 'Poslnsh'
      =lfSetIndex(lcMastPoLn,@laIndex)
      Set Order To Tag lcMastPoLn In (lcMastPoLn)
    Endif
  Endif

  Select (lcTmpLine)
  *-- Get the average cost from the source location.
  =Seek(lcStyle,lcTempItem)
  =Seek(lcStyle+lcVendor+Space(10),lcItemLoc)
  lnNewCost  = Iif(Iif(lcInvType="0001",lcCostMth = 'A',lcCostMthM = 'A'),;
    &lcItemLoc..Ave_Cost , &lcTempItem..TotCost )

  *--Update Style Inventory
  *--G/L Array difinition and initialization.
  *-- Update general ledger entreis in gfStyCrl()
  If llLinkToGl
    *--Read the G/L link code that will be used to create GL entres.
    Declare laGLDistAr[2,13]
    laGLDistAr[1,1] = Iif(Seek(lcStyle+lcVendor+Space(10),lcItemLoc) And ;
      !Empty(&lcItemLoc..Gl_link),&lcItemLoc..Gl_link,'DEFDEF')

    laGLDistAr[2,1] = Iif(Seek(lcBusDocu+lcStyType+lcPoNo,lcMastPoHd),&lcMastPoHd..link_code,'DEFDEF')
    laGLDistAr[1,2] = '006'
    laGLDistAr[2,2] = '013'
    laGLDistAr[1,3] =  1
    laGLDistAr[2,3] = -1
    *! C201407,1 SAB 03/20/2012 Fix problem of Tran_type in GLDIST should be PO in receiveing case [Start]
    **N039541,1 KHM 12/12/2005 [Start]
    **STORE 'PO'       TO laGLDistAr[1,4],laGLDistAr[2,4]
    *STORE 'IA'       TO laGLDistAr[1,4],laGLDistAr[2,4]
    **N039541,1 KHM 12/12/2005 [End]
    Store 'PO'       To laGLDistAr[1,4],laGLDistAr[2,4]
    *! C201407,1 SAB 03/20/2012 Fix problem of Tran_type in GLDIST should be PO in receiveing case [End]
    Store lcPoNo     To laGLDistAr[1,5],laGLDistAr[2,5]
    Store ldTrDate   To laGLDistAr[1,6],laGLDistAr[2,6]
    Store lcGLFYear  To laGLDistAr[1,7],laGLDistAr[2,7]
    Store lcGLPeriod To laGLDistAr[1,8],laGLDistAr[2,8]
    Store lcGlDist   To laGLDistAr[1,9],laGLDistAr[2,9]
  Else
    Dime laGLDistAr[1,1]
    laGLDistAr = ''
  Endif
  Private laAdjust
  Declare laAdjust[9]
  For lnI = 1 To 8
    lcI = Str(lnI,1)
    laAdjust[lnI] = -(QTY&lcI)
  Endfor
  laAdjust[9] = -(TotQty)

  *--Call the global function for update style inventory control.
  Private lcRefer
  lcRefer  = "Source :" + lcVendor + " Target: "+ lcWareHous
  
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
*!*	  lnNxtStp = gfStyCrl('6',lcStyle,lcVendor,lcDyelot,ldRecDate,;
*!*	    lcPoNo,@laAdjust,lnNewCost,lcRefer,lcGlSession,'',;
*!*	    1,lcTmpLine,'nSteps',@laGLDistAr,,,,,&lcMastPoHd..cPONo)
  lnNxtStp = gfStyCrl('6',lcStyle,lcVendor,lcDyelot,ldRecDate,;
    lcPoNo,@laAdjust,lnNewCost,lcRefer,lcGlSession,'',;
    1,lcTmpLine,'nSteps',@laGLDistAr,,,,,&lcMastPoHd..cPONo,.T.)
    
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]

  *--Create Intransit P/O line record
  Select (lcTmpLine)
  Scatter Memvar

  If lcRecvType  = 'A'
    Store 0 To m.nCost2,m.nLan_CST2,m.nECost2,m.nELanCost2
  Endif

  =Seek(lcStyle+lcVendor+Space(10),lcItemLoc)
  m.nFCost1  = Iif(Iif(lcInvType="0001",lcCostMth='A',lcCostMthM ='A'),;
    &lcItemLoc..Ave_Cost , &lcTempItem..TotCost)
  m.nICost1 = m.nFCost1

  Select (lcPosLn)
  Append Blank
  Gather Memvar
  Replace Date      With ldRecDate,;
    TranCd    With '6',;
    cOwner    With ' ',;
    cRSession With lcGlSession
  Select (lcTmpLine)

  *! E303006,1 SAB 12/03/2011 Enable Issue From BINs in Inter Location PO [Start]
  *- Call Bin Location Trigger
  If Type('loFormSet')='O' And Ascan(loFormSet.laEvntTrig,Padr('DLSBNPOR',10),1,Alen(loFormSet.laEvntTrig,1),1) > 0
    Select (lcTmpLine)
    =gfDoTriger("POSTREC",Padr("DLSBNPOR",10))
  Endif
  *! E303006,1 SAB 12/03/2011 Enable Issue From BINs in Inter Location PO [End]

  *--Update in-transit
  If (lcRecvType $ 'NHU' Or;
      (!llFromEdi And lcRecvType = 'A' And Ascan(laEvntTrig,Padr("RCVADORD",10)) <> 0))
    For lnCnt = 1 To 8
      lcCnt = Str(lnCnt,1)
      laReceive[lnCnt] = Evaluate(lcTmpLine+'.Qty'+lcCnt)
      laOpen[lnCnt]    = Evaluate(lcTmpLine+'.Qty'+lcCnt)
    Endfor

    *-- Update In-Transit in the style file
    If Seek(lcStyle,lcTempItem)
      =lfUpdItmFl(lcTempItem,'InTrans','+',@laReceive,@laOpen)
      *-- Update WIP in the style file
      If lcRecvType $ 'NHU'
        =lfUpdItmFl(lcTempItem,'WIP','+',@laReceive,@laOpen)
        *-- Update NWO in the style file
        *B124297,1 WAM 03/05/2005 Do not update Work Order
        *=lfUpdItmFl(lcTempItem,'NWO','+',@laReceive,@laOpen)
        *B124297,1 WAM 03/05/2005 (End)
      Endif
    Endif

    *-- Update In-Transit in the stydye file
    If Seek(lcStyle+Iif(lcRecvType='U',lcVendor,lcWareHous)+Space(10),lcItemLoc)
      =lfUpdItmFl(lcItemLoc,'InTrans','+',@laReceive,@laOpen)
    Endif
    *-- Update the record on the dyelot level
    If llDyelot And !Empty(lcDyelot) And &lcTempItem..cDye_Flg='Y' And;
        SEEK(lcStyle+Iif(lcRecvType='U',lcVendor,lcWareHous)+lcDyelot,lcItemLoc)
      =lfUpdItmFl(lcItemLoc,'InTrans','+',@laReceive,@laOpen)
    Endif

    *-- Update WIP & WO in the stydye file
    If lcRecvType $ 'NHU'
      If Seek(lcStyle+lcVendor+Space(10),lcItemLoc)
        =lfUpdItmFl(lcItemLoc,'WIP','+',@laReceive,@laOpen)
        *B124297,1 WAM 03/05/2005 Do not update Work Order
        *=lfUpdItmFl(lcItemLoc,'NWO','+',@laReceive,@laOpen)
        *B124297,1 WAM 03/05/2005 (End)
      Endif
      *-- Update the record on the dyelot level
      If llDyelot And !Empty(lcDyelot) And &lcTempItem..cDye_Flg='Y' And ;
          SEEK(lcStyle+lcVendor+lcDyelot,lcItemLoc)
        =lfUpdItmFl(lcItemLoc,'WIP','+',@laReceive,@laOpen)
        *B124297,1 WAM 03/05/2005 Do not update Work Order
        *=lfUpdItmFl(lcItemLoc,'NWO','+',@laReceive,@laOpen)
        *B124297,1 WAM 03/05/2005 (End)
      Endif
    Endif
  Endif

  *-- IF issue inter-location PO shipment
  If lcRecvType = 'U'
    Select (lcTmpLine)
    lcShpcode = ShipNo
    lcShpLine = ShipNo+cBusDocu+cStyType+PO+cInvType+Style+Str(Lineno,6)+'3'
    Select (lcMastPoLn)
    Set Order To Tag Poslnsh
    =Seek(lcShpLine)
    Scatter Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 To laSHPQty
    Scatter Memvar Memo
    Select(lcPosLn)
    Append Blank
    Tableupdate(0,.T.)
    Gather Memvar Memo
    Replace Qty1 With Max(laSHPQty[1]-&lcTmpLine..Qty1,0),;
      Qty2 With Max(laSHPQty[2]-&lcTmpLine..Qty2,0),;
      Qty3 With Max(laSHPQty[3]-&lcTmpLine..Qty3,0),;
      Qty4 With Max(laSHPQty[4]-&lcTmpLine..Qty4,0),;
      Qty5 With Max(laSHPQty[5]-&lcTmpLine..Qty5,0),;
      Qty6 With Max(laSHPQty[6]-&lcTmpLine..Qty6,0),;
      Qty7 With Max(laSHPQty[7]-&lcTmpLine..Qty7,0),;
      Qty8 With Max(laSHPQty[8]-&lcTmpLine..Qty8,0),;
      TotQty With Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
    If TotQty = 0
      Delete
    Endif
  Endif

  *-- To be done later
  *-- Send Inter Location PO or Purchase orders to sites
  If !llFromEdi And 'NC' $ oAriaApplication.CompanyInstalledModules And lcRecvType = 'N'
    Local lcwarecode
    lcwarecode = &lcTmpLine..cWareCode
    =gfOpenFile(oAriaApplication.DataDir+'WAREHOUS',oAriaApplication.DataDir+'WAREHOUS','SH')
    =Seek(lcwarecode,'WAREHOUS')
    lcSiteId = WAREHOUS.cSiteId
    Select EDiAcPrt
    Locate For cSiteId = lcSiteId
    If Found() And EDiAcPrt.LINTERCOMP And Seek(EDiAcPrt.cPartCode+'856','EdiPd')
      Select EdiTrans
      If !Seek('856'+Padr(&lcTmpLine..PO,40)+EDiAcPrt.Type+EDiAcPrt.cPartner)
        Insert Into ('EDITRANS') (CEDITRNTYP,Key,Type,cPartner,LINTERCOMP) ;
          VALUES ('856',&lcTmpLine..PO,EDiAcPrt.Type,EDiAcPrt.cPartner,;
          EDiAcPrt.LINTERCOMP)
      Endif
      Replace cStatus With 'N'

      If lcSysType = 'P' And lcSiteId <> lcBackSite And !Empty(lcBackAcc)
        If !Seek('856'+Padr(&lcTmpLine..PO,40)+'A'+lcBackAcc)
          Insert Into ('EDITRANS') (CEDITRNTYP,Key,Type,cPartner,LINTERCOMP) Values ;
            ('856',&lcTmpLine..PO,'A',lcBackAcc,.T.)
        Endif
        Replace cStatus With 'N'
      Endif
    Endif
  Endif

Endscan

*--------------------------- Begin updating Master files ---------------------------------
*-- Updating Fox tables Remotely
Local lnConnectionHandlar, lcTranCode
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.cAriaNativeDataFilesConStr,3,'',.T.)
If Type('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
  Return .F.
Endif

*-- 1) Updating style file (FOX) in case of receiving by styles

*!B999999,1 WSH 03/01/2005, Select Table that will be updated... [Start]
Select (lcTempItem)
*!B999999,1 WSH 03/01/2005, [End]

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcTempItem,lcTranCode,;
  SET("DataSession"),'STYLE','STYLE')
If lnConnectionHandlar # 1 .And. lnConnectionHandlar # 2
  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
  Return .F.
Else
  =Tableupdate(.T.,.T.)
Endif

*-- 2) Updating stydye file (FOX)
Select (lcItemLoc)
lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcItemLoc,lcTranCode,;
  SET("DataSession"),'Style+cWareCode+dyelot','STYDYE')
If lnConnectionHandlar # 1 .And. lnConnectionHandlar # 2
  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
  Return .F.
Else
  =Tableupdate(.T.,.T.)
Endif
*! E304047,2 MMT 08/02/2018 Modify Receiving program to update SQL GLDIST table[Start]
*-- 3) Updating the GLDIST file (FOX)
*!*	If llLinkToGl
*!*	  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
*!*	  *WAIT WINDOW 'Updating General Ledger Distribution File ' NOWAIT
*!*	  If !loFormSet.llSilentMod
*!*	    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
*!*	    *WAIT WINDOW 'Updating General Ledger Distribution File ' NOWAIT
*!*	    Wait Window Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_UPDATEGL,loFormSet.GetHeaderText("LANG_POSTREC_UPDATEGL",loFormSet.HeaderAlias)) Nowait
*!*	    *N000682,1 MMT 12/09/2012 Globalization changes[END]
*!*	  Endif
*!*	  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

*!*	  Select (lcGlDist)
*!*	  Replace All GlSession With lcGlSession ,;
*!*	    Tran_Desc With 'ISSUE INTER-LOC. P/O'

*!*	  *!B999999,1 WSH 03/01/2005, Fix bug of Alias not found... [Start]
*!*	  *ENDIF
*!*	  *!B999999,1 WSH 03/01/2005, [End]

*!*	  Select (lcGlDist)
*!*	  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcGlDist,lcTranCode,;
*!*	    SET("DataSession"),'Tran_No+Tran_Type+GlSession+Catg_Key','GLDIST')

*!*	  If lnConnectionHandlar # 1 .And. lnConnectionHandlar # 2
*!*	    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
*!*	    Return .F.
*!*	  Else
*!*	    =Tableupdate(.T.,.T.)
*!*	  Endif

*!*	  *!B999999,1 WSH 03/01/2005, Fix bug of Alias not found... [Start]
*!*	Endif
*!B999999,1 WSH 03/01/2005, [End]
*! E304047,2 MMT 08/02/2018 Modify Receiving program to update SQL GLDIST table[End]

*-- Commit updating of FOX tables
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
*!*	lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode,.T.)
*!*	If lnConnectionHandlar # 1
*!*	  =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran",lnConnectionHandlar,.T.)
*!*	  Return .F.
*!*	ENDIF
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [ENd]

*-- Begin transaction - Updating SQL tables
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
*!*	lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
*!*	If Type('lcTranCode') = 'N'
*!*	  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
lcSQLTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
If Type('lcSQLTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcSQLTranCode,.T.)
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]  
  RETURN .F.
Endif

*-- 1) Update master POSLN (SQL)
Select(lcPosLn)

*B607585,1 AMH Add cwarecode to POSLN index and nLineNo to BOMLINE file [Start]
*lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcPosLn,lcTranCode,;
SET("DataSession"),;
'cBusDocu,cStyType,PO,cRSession,ShipNo,cInvType,Style,LineNo,TranCd,cStyGrade',;
'POSLN','POSREC')
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
*!*	lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcPosLn,lcTranCode,;
*!*	  SET("DataSession"),;
*!*	  'cBusDocu,cStyType,PO,cRSession,ShipNo,cInvType,Style,LineNo,TranCd,cStyGrade,cWareCode',;
*!*	  'POSLN','POSREC')
lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcPosLn,lcSQLTranCode,;
  SET("DataSession"),;
  'cBusDocu,cStyType,PO,cRSession,ShipNo,cInvType,Style,LineNo,TranCd,cStyGrade,cWareCode',;
  'POSLN','POSREC')
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]  
*B607585,1 AMH [End]

If lnConnectionHandlar # 1 .And. lnConnectionHandlar # 2
  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
  *E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]  
  *lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
  lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcSQLTranCode)
  *E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]  
  Return .F.
Else
  =Tableupdate(.T.,.T.)
Endif

*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
IF USED('STYINVJL')
  LNTABLESTY = GFGETREMOTETABLE(SET("Datasession"),'STYINVJL')
  IF LNTABLESTY <>0
    SELECT(OARIAAPPLICATION.LAREMOTETABLE[LNTABLESTY].LCCURSORUPDATE)
    lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(OARIAAPPLICATION.LAREMOTETABLE[LNTABLESTY].LCCURSORUPDATE,lcSQLTranCode,;
    SET("DataSession"),'Oid','STYINVJL','USTYINVJL')
    If lnConnectionHandlar # 1 .And. lnConnectionHandlar # 2
      =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
      lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcSQLTranCode)
      RETURN .F.
    Else
      =Tableupdate(.T.,.T.)
    ENDIF    
  ENDIF  
ENDIF
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]

*-- 2) Update master Shipment file (SQL)
*-- IF issue inter-location PO shipment
If lcRecvType = 'U' And Type("lcMastShp") = "C" And Used(lcMastShp)
  *--Delete the shipment lines for coplete receive lines.
  Select (lcMastShp)
  LOCATE
  *E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
*!*	  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcMastShp,lcTranCode,;
*!*	    SET("DataSession"),;
*!*	    'CBUSDOCU,CSHPTYPE,SHIPNO',;
*!*	    'SHPMTHDR','SHPMTHDR')
  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcMastShp,lcSQLTranCode,;
    SET("DataSession"),;
    'CBUSDOCU,CSHPTYPE,SHIPNO',;
    'SHPMTHDR','SHPMTHDR')
  *E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]    
  If lnConnectionHandlar # 1 .And. lnConnectionHandlar # 2
    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
    *E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
    *lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
    lnResult = oAriaApplication.RemoteCompanyData.RollBackTran (lcSQLTranCode)
    *E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]    
    Return .F.
  Else
    =Tableupdate(.T.,.T.)
  Endif
Endif
*B000109,1 WAM 03/05/2005 Remove Lock PO header
If .F.
  Select (lcTmpLine)
  Set Order To Tag TmpLine3
  Go Top
  Do While !Eof()
    lcPo = PO
    lcSelString = "UPDATE POSHDR SET lLok_stat =0,cLok_User= '', dLok_Date='',cLok_Time='' WHERE cBusDocu+cStyType+PO='"+cBusDocu+cStyType+PO+"'"
    *E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
    *lnResult = oAriaApplication.RemoteCompanyData.Execute(lcSelString,'',"SAVEFILE","",lcTranCode ,4,'',Set("DataSession"))    
    lnResult = oAriaApplication.RemoteCompanyData.Execute(lcSelString,'',"SAVEFILE","",lcSQLTranCode,4,'',Set("DataSession"))
    *E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]
    If lnResult # 1 .And. lnResult # 2
      =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLUPDATE",lnResult,.T.)
      *E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
      *=oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
      =oAriaApplication.RemoteCompanyData.RollBackTran (lcSQLTranCode)      
      *E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]
      Return .F.
    Endif
    Select (lcTmpLine)
    Scan Rest While PO = lcPo
    Endscan
  Enddo
Endif
*B000109,1 WAM 03/05/2005 (End)
*! E304047,2 MMT 08/02/2018 Modify Receiving program to update SQL GLDIST table[Start]
*-- 3) Updating the GLDIST file (FOX)
If llLinkToGl
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
  *WAIT WINDOW 'Updating General Ledger Distribution File ' NOWAIT
  If !loFormSet.llSilentMod
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *WAIT WINDOW 'Updating General Ledger Distribution File ' NOWAIT
    Wait Window Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_UPDATEGL,loFormSet.GetHeaderText("LANG_POSTREC_UPDATEGL",loFormSet.HeaderAlias)) Nowait
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
  Endif
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

  Select (lcGlDist)
  Replace All GlSession With lcGlSession ,;
    Tran_Desc With 'ISSUE INTER-LOC. P/O'

  *!B999999,1 WSH 03/01/2005, Fix bug of Alias not found... [Start]
  *ENDIF
  *!B999999,1 WSH 03/01/2005, [End]

  Select (lcGlDist)
  *E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
  *lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcGlDist,lcTranCode,;
    SET("DataSession"),'Tran_No+Tran_Type+GlSession+Catg_Key','GLDIST')
  lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcGlDist,lcSQLTranCode,;
    SET("DataSession"),'Tran_No+Tran_Type+GlSession+Catg_Key','GLDIST')
  *E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
  If lnConnectionHandlar # 1 .And. lnConnectionHandlar # 2
    =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
    Return .F.
  Else
    =Tableupdate(.T.,.T.)
  Endif

  *!B999999,1 WSH 03/01/2005, Fix bug of Alias not found... [Start]
Endif
*!B999999,1 WSH 03/01/2005, [End]
*! E304047,2 MMT 08/02/2018 Modify Receiving program to update SQL GLDIST table[End]
*-- Commit updating of SQL tables
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
*lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcSQLTranCode)
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [End]
If lnConnectionHandlar # 1
  =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran",lnConnectionHandlar,.T.)
  Return .F.
Endif
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [Start]
* Fox Update
lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode,.T.)
If lnConnectionHandlar # 1
  =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran",lnConnectionHandlar,.T.)
  Return .F.
ENDIF
*E303991,1 ES 01/22/2019 "The STYINVJL is updated in gfStyCrl function, and fox table are updated in SQL Transaction and SQL tables are updated in another SQL transaction" [ENd]
*E611838,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][Start]
IF llLinkToGl
  =gfCreateGLEntries(lcGlDist,'')
EnDIF
*E611838,1 MMT 11/18/2019 Add new Global function to update GLTRNHD,GLTRNDT[GL Enhancement][End]
Wait Clear

*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
Release lcSqlStatement, lcStyle, lcWareHous, lcDyelot, lcVendor,lcBusDocu, lcStyType, lcPoNo, laReceive, laOpen, laStkQty
Release lcChkPO, lcSysType,laReceive, laOpen, laStkQty
Release lcBackSite,lcBackAcc ,lnNewCost,laGLDistAr,laAdjust,lnI ,lcI ,lcRefer  ,lnNxtStp
Release lnCnt ,lcCnt ,lcShpcode ,lcShpLine ,laSHPQty,lcwarecode,lcSiteId
Release lnConnectionHandlar, lcTranCode
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]


Return
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
Function lfGetItmInf
Lparameters lcInvType, lcKey, lcFilToUpd, lcMastFile, lcSqlState, llSql
Private lnAlias

lnAlias  = Select(0)

*-- this condition has been done in order not to change the whole code and it might be
*-- removed later on
If Type('lcTmpCur') <> 'C'
  lcTmpCur = gfTempName()
Endif

If !Seek(lcKey, lcFilToUpd)
  If llSql
    =lfOpenSql(lcSqlState,lcMastFile,lcTmpCur)
  Else
    =lfOpenFox(lcSqlState,lcMastFile,lcTmpCur)
  Endif

  Select(lcTmpCur)
  Locate
  Scatter Name loData
  Insert Into(lcFilToUpd) From Name loData
  Release loData
  Select(lcFilToUpd)
  =Tableupdate(0,.T.)
Endif

*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
Release lnAlias,loData
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]
*!***************************************************************************
*! Name      : lfGetLanded
*! Developer : Khalid Mohi El-Din
*! Date      : 09/11/2004
*! Purpose   : Calculate landed costs case of detail costing.
*!******************************************************************************
Function lfGetLanded

Select (lcTmpBomLn)
If lcRecvType $ 'SFC'
  Set Order To Tag BomLnShp
Else
  Set Order To Tag BomLine
Endif
Private laECost,lnCurrUnt1,lnCurrUnt2
Dimension laECost[1]
laECost = 0
Store 1 To lnCurrUnt1,lnCurrUnt2

Select (lcTmpLine)
Scatter Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 To laLnQty

For lnCnt = 1 To 7
  lcCnt=Str(lnCnt,1)
  If llMFCall
    lcBomLKey = 'M2'+&lcTmpLine..PO
    lcWCondtn = "cImTyp+cType+cTktNo = lcBomLKey"
    *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
    lcFCondtn = "cBomTyp=lcCnt AND Style=&lcTmpLine..Style AND (EMPTY(cRSession) OR cRSession=lcGlSession)"
    *N038893,1 WAM 06/02/2005 (End)
    lcFCondtn = lcFCondtn + " .AND. cStyGrade = &lcTmpLine..cStyGrade "
  Else
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
    *!*	    lcBomLKey = IIF(lcRecvType='D','D',IIF(lcRecvType$"NOC","N",'I'))+'2'+;
    *!*	      IIF(lcRecvType$'SFC',&lcTmpLine..Shipno,'')+&lcTmpLine..Po+;
    *!*	      STR(&lcTmpLine..LINENO,6)+lcCnt+lcInvType+&lcTmpLine..STYLE
    lcBomLKey = Iif(lcRecvType='D','D',Iif(lcRecvType$"NOC","N",Iif(&lcTmpLine..cStyType="F",'T','I')))+'2'+;
      IIF(lcRecvType$'SFC',&lcTmpLine..ShipNo,'')+&lcTmpLine..PO+;
      STR(&lcTmpLine..Lineno,6)+lcCnt+lcInvType+&lcTmpLine..Style
    *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
    lcWCondtn = "cImTyp+cType+IIF(lcRecvType$'SFC',ShipNo,'')+cTktNo+STR(LineNo,6)+cBomTyp+cInvType+Style = lcBomLKey"
    lcFCondtn = "(EMPTY(cRSession) OR cRSession=lcGlSession) AND cStyGrade = &lcTmpLine..cStyGrade"
  Endif
  *B608762,1 WAM 12/15/2008 Consider receiving cut ticket line into more than one warehouse
  lcFCondtn = lcFCondtn + " .AND. cWareCode = &lcTmpLine..cWareCode"
  *B608762,1 WAM (End)

  lnNLCs&lcCnt = 0
  lnCurSQt = 0

  Select (lcTmpBomLn)
  If Seek(lcBomLKey)
    Replace Rest StyQty  With lfBomSzQt(),;
      ItemQty With (StyQty*UnitQty),;
      ItemAmt With (ItemQty*UnitCost);
      WHILE &lcWCondtn For &lcFCondtn
    =Seek(lcBomLKey)
    Sum Rest (UnitCost*UnitQty)*StyQty While &lcWCondtn For &lcFCondtn To lnNLCs&lcCnt
    lnNLCs&lcCnt = Iif(&lcTmpLine..TotQty<>0,(lnNLCs&lcCnt/&lcTmpLine..TotQty),0)
    lnNLCs&lcCnt = Iif(Type("lnNLCs"+lcCnt) <> "N",0,lnNLCs&lcCnt)
  Endif

  Select (lcTmpLine)
  Replace nFLanCost&lcCnt With lnNLCs&lcCnt
  If !llMFCall
    *! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [Start]
    *IF llMulCurr
    If loFormSet.llMulCurr
      *! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [END]
      *N000587,1 WAM 12/01/2007 Get equivalent cost from BOM table for each cost element based on saved currency code, exchange rate and unit.
      *! B609634,1 MMT 06/28/2011 Fix bug of Zero Cost in Styinvjl in Case of receiving Interlocation PO[Start]
      *IF !(cBusDocu+cStyType $ 'PP|PD|NN' )
      *! B610107,1 MMT 10/04/2012 Add Foreign currency costing to Material MFG order summary[Start]
      *IF !(cBusDocu+cStyType $ 'PP|PD')
      If !(cBusDocu+cStyType $ 'PP|PD|PF')
        *! B610107,1 MMT 10/04/2012 Add Foreign currency costing to Material MFG order summary[End]
        *! B609634,1 MMT 06/28/2011 Fix bug of Zero Cost in Styinvjl in Case of receiving Interlocation PO[END]
        *N000587,1 WAM 12/01/2007 (End)


        =lfGetEqv(lcCnt,nLanPrRat,nLanDuRat,lnCurrUnt1,lnCurrUnt2,nFLanCost1,nFLanCost2,;
          nFLanCost3,nFLanCost4,nFLanCost5,nFLanCost6,nFLanCost7)

        laECost[1] = Iif(Type("laECost[1]") <> "N", 0, laECost[1])
        laECost[1] = Iif(Occurs('*',Str(laECost[1])) > 0,0,laECost[1])
        Replace nLan_Cost&lcCnt With laECost[1]
        *N000587,1 WAM 12/01/2007 Get equivalent cost from BOM table for each cost element based on saved currency code, exchange rate and unit.
      Endif
      *N000587,1 WAM 12/01/2007 (End)

    Else
      Replace nLan_Cost&lcCnt With lnNLCs&lcCnt
    Endif
  Endif
Endfor
Select (lcTmpLine)
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
Release laECost,lnCurrUnt1,lnCurrUnt2,laLnQty,lnCnt ,lcCnt,lcBomLKey ,lcWCondtn ,lcFCondtn ,lnCurSQt
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]
Return

*!***************************************************************************
*! Name      : lfBomSzQt
*! Developer : Khalid Mohi El-Din
*! Date      : 09/11/2004
*! Purpose   : Calculate the style quantity per size.
*!******************************************************************************
Function lfBomSzQt
lnCurSQt = 0
For lnI=1 To 8
  If Str(lnI,1) $ Evaluate(lcTmpBomLn+'.CSIZES')
    lnCurSQt = lnCurSQt + laLnQty[lnI]
  Endif
Endfor
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
Release lnI
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]

Return lnCurSQt


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
Procedure gpCrtBom
Parameters lcTrType,lcTket,lcItem,lcShipNo,lcSess,lcStyQlt, lcMastFile,;
  lcTmpFile, llShpRec



Select (lcMastFile)
If lcTrType = 'F'
  lcItem = Padr(lcItem,19)
Endif

*B132695,1 KHM 07/03/2006 Add the line # [Begin]
*lcSeekKey = lcTrType+'2'+IIF(llShpRec,lcShipNo,SPACE(6))+lcTket
lcSeekKey = lcTrType+'2'+Iif(llShpRec,lcShipNo,Space(6))+lcTket+Str(Evaluate(lcTmpLine+'.LineNo'),6)
*B132695,1 KHM 07/03/2006 [End]

lcWhleCnd = "cImTyp+cType+ShipNo+cTktNo+STR(LineNo,6) = lcSeekKey"
lcForCond = "Style = lcItem AND ( EMPTY(cRSession) OR cRSession=lcSess )"
lcQltFltr = Iif(Type('lcStyQlt') $ 'UL' ,".T.","cStyGrade = lcStyQlt")
Seek lcSeekKey

Locate Rest While &lcWhleCnd For &lcForCond And &lcQltFltr
If !Found()
  *B132695,1 KHM 07/03/2006 [Begin]
  *SEEK lcTrType+'1'+IIF(llShpRec,lcShipNo,SPACE(6))+lcTket
  *SCAN REST WHILE cImTyp+cType+ShipNo+cTktNo+STR(LineNo,6) = ;
  lcTrType+'1'+IIF(llShpRec,lcShipNo,SPACE(6))+lcTket;
  FOR Style = lcItem AND !lVoid
  Seek lcTrType+'1'+Iif(llShpRec,lcShipNo,Space(6))+lcTket+Str(Evaluate(lcTmpLine+'.LineNo'),6)
  Scan Rest While cIMTyp+cType+ShipNo+cTktNo+Str(Lineno,6) = ;
      lcTrType+'1'+Iif(llShpRec,lcShipNo,Space(6))+lcTket+Str(Evaluate(lcTmpLine+'.LineNo'),6);
      FOR Style = lcItem And !lVoid
    Scatter Memvar Memo
    *B132695,1 KHM 07/03/2006 [End]

    *B608762,1 WAM 12/15/2008 Increament NLINENO whenthe same key repeated
    Select (lcTmpFile)
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
    Scan For ;
        cIMTyp+cType+cTktNo+ShipNo+Str(Lineno,6)+CBOMTYP+cInvType+Style+CINVTYPC+Item+MFGCODE+cRSession+cStyGrade+Str(nLineNo,4)=;
        lcTrType+'2'+lcTket+Iif(llShpRec,lcShipNo,Space(6))+Str(&lcTmpLine..Lineno,6)+;
        &lcMastFile..CBOMTYP+&lcMastFile..cInvType+Padr(lcItem ,19)+;
        &lcMastFile..CINVTYPC+&lcMastFile..Item+&lcMastFile..MFGCODE ;
        AND cStyGrade = &lcMastFile..cStyGrade
      *B609517,1 TMI 02/14/2011 [End  ]
      *B608845,1 WAM 04/09/2009 (End)

      lnNewLineNo = Max(Iif(Isnull(nLineNo),0,nLineNo),lnNewLineNo)
    Endscan
    lnNewLineNo = lnNewLineNo + 1
    *B608762,1 WAM 12/15/2008 (End)

    Select(lcTmpFile)
    Append Blank
    Gather Memvar Memo
    Replace cType      With '2',;
      ShipNo    With lcShipNo,;
      cStyGrade With Iif(lcQltFltr<>".T.",lcStyQlt,''),;
      StyQty    With  0,;
      ItemQty   With  0,;
      ItemAmt   With  0

    * B608834,1 HES 04/01/2009 Add Standared Fields for POMLINE File[T20081001.0001]
    =gfAdd_Info(lcTmpFile)
    * B608834,1 HES 04/01/2009 Add Standared Fields for POMLINE File[T20081001.0001]

    *B608762,1 WAM 12/15/2008 Update NLINENO and CWARECODE fields
    Replace nLineNo   With lnNewLineNo ,;
      cWareCode With &lcTmpLine..cWareCode
    *B608762,1 WAM 12/15/2008 (End)

    *! B608617,1 MMT 07/14/2008 Add trigger to update bomline with new ex rate [Start]
    If CCATGTYP = 'P' And Ascan(loFormSet.laEvntTrig ,Padr('UPDEXRATE',10))<>0
      loFormSet.mDoTrigger(Padr('UPDEXRATE' ,10))
    Endif
    *! B608617,1 MMT 07/14/2008 Add trigger to update bomline with new ex rate [End]

  Endscan
Else
  Scan Rest While &lcWhleCnd For &lcForCond And &lcQltFltr
    Scatter Memvar Memo
    Select(lcTmpFile)

    *B609517,1 TMI 02/05/2011 [Start] make the NlineNo field updated as in IF !FOUND() case
    *B609517,1                        the SCAN FOR should not check crsession = space(6) because it will has value
    lnNewLineNo = 0
    Scan For ;
        cIMTyp+cType+cTktNo+ShipNo+Str(Lineno,6)+CBOMTYP+cInvType+Style+CINVTYPC+Item+MFGCODE+cRSession+cStyGrade+Str(nLineNo,4)=;
        lcTrType+'2'+lcTket+Iif(llShpRec,lcShipNo,Space(6))+Str(&lcTmpLine..Lineno,6)+;
        &lcMastFile..CBOMTYP+&lcMastFile..cInvType+Padr(lcItem ,19)+;
        &lcMastFile..CINVTYPC+&lcMastFile..Item+&lcMastFile..MFGCODE ;
        AND cStyGrade = &lcMastFile..cStyGrade

      lnNewLineNo = Max(Iif(Isnull(nLineNo),0,nLineNo),lnNewLineNo)
    Endscan
    lnNewLineNo = lnNewLineNo + 1
    *B609517,1 TMI 02/05/2011 [End  ]
    Append Blank
    Gather Memvar Memo
    Tableupdate(0,.T.)
    Replace cType      With '2',;
      ShipNo    With lcShipNo,;
      cStyGrade With Iif(lcQltFltr<>".T.",lcStyQlt,''),;
      StyQty    With  0,;
      ItemQty   With  0,;
      ItemAmt   With  0

    *B609517,1 TMI 02/05/2011 [Start] Update the nLineno field
    Replace nLineNo   With lnNewLineNo
    *B609517,1 TMI 02/05/2011 [End  ]

    *B609068,1 WAM 10/29/2009 Update CWARECODE in BOMLINE when there are adjust records for receiving
    Replace cWareCode With &lcTmpLine..cWareCode
    *B609068,1 WAM 10/29/2009 (End)

    * B608834,1 HES 04/01/2009 Add Standared Fields for POMLINE File[T20081001.0001]
    =gfAdd_Info(lcTmpFile)
    * B608834,1 HES 04/01/2009 Add Standared Fields for POMLINE File[T20081001.0001]

  Endscan
Endif

*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
Release lcSeekKey ,lcWhleCnd ,lcForCond,lcQltFltr ,lnNewLineNo
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]

Return



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
Function lfUpdLot
Lparameters lcCTCode, lcOpration, lcRecvLot, laQuantity, lcRcvItem,;
  lcRcvDyelot

Private laAddQty, laOpenQty, lcSaveRecKey, lnCount, lcCount, lcLotNo,;
  lcSqlStatement, lcIMType, llFound

Declare laAddQty[9], laOpenQty[9]

lcIMType = Iif(llMFCall,'M',Iif(&lcTmpLine..cStyType="N","N",'I'))
Store .F. To llFound, llLotFound

*-- Get the information from the MFGOPRDT
*-- cimtyp+ctktno+coprcode+clotno+trancd
lcSqlStatement = "SELECT * FROM MFGOPRDT [INDEX=MFGOPRDT] "+;
  "WHERE cImTyp = '" + lcIMType +;
  "' AND cTktNo ='" + lcCTCode + "' AND cOprCode = '" + lcOpration + ;
  "' AND Item = '" + lcRcvItem + "' AND cDyelot ='" + lcRcvDyelot + "'"
=lfOpenSql(lcSqlStatement,'MFGOPRDT',lcTmpCur, "","",.F.)
Dimension laIndex[1,2]
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
Select(lcTmpCur)
Scan
  Scatter Memvar
  Select (lcMastOprDt)
  =Seek(m.cIMTyp+m.cTktNo+m.cOprCode+m.cLotNo+m.TranCd)
  Locate Rest While cIMTyp+cTktNo+cOprCode+cLotNo+TranCd= m.cIMTyp+m.cTktNo+m.cOprCode+m.cLotNo+m.TranCd ;
    FOR Item = m.Item And cDyelot = m.cDyelot
  If !Found()
    Insert Into (lcMastOprDt) From Memvar
    Tableupdate(0,.T.,lcMastOprDt)
  Endif
Endscan

Select (lcMastOprDt)
=Seek(lcIMType+lcCTCode+lcOpration+lcRecvLot+'1')
Locate Rest While cIMTyp+cTktNo+cOprCode+cLotNo+TranCd=;
  lcIMType+lcCTCode+lcOpration+lcRecvLot+'1' ;
  FOR Item = lcRcvItem And cDyelot = lcRcvDyelot
*N038893,1 WAM 06/02/2005 (End)

If !Found()
  If &lcTmpLine..TranCd <> '5'
    =Seek(lcIMType+lcCTCode+lcOpration)
    Scatter Memvar Memo

    lcSqlStatement  = "SELECT * FROM CODES " + ;
      "WHERE cDefCode+ccode_no+crltfield+cfld_name = 'N'"+;
      "+'"+Padr(lcOpration,6)+"'+'Y'+'MFGCODE   ' AND CRLTD_NAM = 'CCONTCODE'"

    =lfOpenFox(lcSqlStatement,'CODES',lcTmpCode,"")
    Select(lcTmpCode)
    Locate

    Select (lcMastOprDt)
    m.cDyelot = &lcTmpLine..Dyelot
    Append Blank
    Gather Memvar Memo
    Replace cIMTyp    With lcIMType,;
      cTktNo    With lcCTCode,;
      ITEM      With &lcTmpLine..Style,;
      cLotNo    With lcRecvLot ,;
      cOprCode  With lcOpration,;
      CCONTCODE With Iif(Empty(CCONTCODE),Allt(&lcTmpCode..cRltd_Vlu),CCONTCODE),;
      dTranDate With oAriaApplication.SystemDate ,;
      DueDate   With oAriaApplication.SystemDate ,;
      TranCd    With '1'       ,;
      cTrgOpr   With Space(2)  ,;
      cTrgLot   With Space(2)  ,;
      cInvType  With lcInvType

    Replace nLotQty1   With 0,;
      nLotQty2   With 0,;
      nLotQty3   With 0,;
      nLotQty4   With 0,;
      nLotQty5   With 0,;
      nLotQty6   With 0,;
      nLotQty7   With 0,;
      nLotQty8   With 0,;
      nLotTotQty With 0,;
      cInvType   With lcInvType
  Endif
Endif
*N038893,1 WAM 06/02/2005 Fix receive last operation
*!*	IF llFound
*!*	  SELECT(lcTmpCur)
*!*	ELSE
*!*	  SELECT (lcMastOprDt)
*!*	ENDIF
Select (lcMastOprDt)
*N038893,1 WAM 06/02/2005 (End)

=Seek(lcIMType+lcCTCode+lcOpration)

Do While cIMTyp+cTktNo+cOprCode = lcIMType+lcCTCode+lcOpration And !Eof()
  If Item+cDyelot <> lcRcvItem+lcRcvDyelot
    Skip
    Loop
  Endif
  If !Empty(lcRecvLot) And cLotNo<>lcRecvLot
    Skip
    Loop
  Endif

  lcLotNo    = cLotNo
  llLotFound = .T.

  *--Check if there is any open quantity for this Opr/Lor/Clr
  Store 0 To laOpenQty
  Scan Rest While cIMTyp+cTktNo+cOprCode+cLotNo = lcIMType+lcCTCode+lcOpration+lcLotNo ;
      FOR Item =lcRcvItem And cDyelot = lcRcvDyelot
    For lnCount = 1 To 8
      lcCount = Str(lnCount,1)
      laOpenQty[lnCount] = Max(laOpenQty[lnCount] + ;
        IIF(TranCd='1',nLotQty&lcCount,-nLotQty&lcCount),0)
      laOpenQty[9] = laOpenQty[9] + laOpenQty[lnCount]
    Endfor
  Endscan

  *--Comupte quantity to be added for this Opr/Lot/Clr
  Store 0 To laAddQty
  If laOpenQty[9] > 0
    For lnCount = 1 To 8
      laAddQty[lnCount]    = Min(&laQuantity[lnCount],laOpenQty[lnCount])
      &laQuantity[lnCount] = &laQuantity[lnCount] - laAddQty[lnCount]
      laAddQty[9]          = laAddQty[9]    + laAddQty[lnCount]
      &laQuantity[9]       = &laQuantity[9] - laAddQty[lnCount]
    Endfor
    If laAddQty[9]>0
      lnSavRec=Recno()
      =lfAppDetRec(lcLotNo, &lcTmpLine..TranCd,'laAddQty',lcCTCode,lcRcvItem,lcRcvDyelot)

      If Between(lnSavRec,1,Reccount())
        Goto lnSavRec
      Endif

    Endif
  Endif
Enddo

*--If there is over received quantity, Update the last lot with the remaind quantity.
If llLotFound And &laQuantity[9] > 0 .And.  &lcTmpLine..TranCd <> '5'
  =lfAppDetRec(lcLotNo, &lcTmpLine..TranCd,'&laQuantity',lcCTCode,lcRcvItem,lcRcvDyelot)
Endif
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
Release laAddQty, laOpenQty, lcSaveRecKey, lnCount, lcCount, lcLotNo, lcSqlStatement, lcIMType, llFound,;
  llLotFound,laIndex,lnSavRec
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]

Return

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
Function lfAppDetRec
Parameters lcLotNum, lcTranCode, laNewQty, lcPoNo,lcRcvItem,lcRcvDyelot)
Local llExists
llExists = .F.
If !llMFCall
  lcTranCode = Iif(lcTranCode='4','3',Iif(lcTranCode='5','4',lcTranCode))
Endif

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

Select (lcMastOprDt)
*-- Get the record of the current receiving type (lcTranCode)
=Seek(lcIMType+lcPoNo+lcLastOpr+lcLotNum+lcTranCode)
Locate Rest While cIMTyp+cTktNo+cOprCode+cLotNo+TranCd=;
  lcIMType + lcPoNo+lcLastOpr+lcLotNum+lcTranCode ;
  FOR Item = lcRcvItem And cDyelot = lcRcvDyelot
*-- If there is no records this means that's its a new record therefore, we will seek
*-- for the budget record
If !Found()
  =Seek(lcIMType+lcPoNo+lcLastOpr+lcLotNum+'1')
  Locate Rest While cIMTyp+cTktNo+cOprCode+cLotNo+TranCd=;
    lcIMType + lcPoNo+lcLastOpr+lcLotNum+'1' ;
    FOR Item = lcRcvItem And cDyelot = lcRcvDyelot
  Scatter Memvar Memo
  Store 0 To m.nLotQty1, m.nLotQty2,m.nLotQty3,m.nLotQty4,m.nLotQty5,m.nLotQty6,m.nLotQty7,m.nLotQty8,m.nLotTotQty
  Append Blank
  Gather  Memvar Memo
  *:B608366,1 MMT 11/29/2007 fix bug of not adding record for last MFG in operation dt File[Start]
  *!*	ELSE
  *!*	  TABLEUPDATE(0,.T.)
  *:B608366,1 MMT 11/29/2007 fix bug of not adding record for last MFG in operation dt File[End]
Endif
Select (lcMastOprDt)
Replace nLotQty1   With nLotQty1 + &laNewQty[1] ,;
  nLotQty2   With nLotQty2 + &laNewQty[2] ,;
  nLotQty3   With nLotQty3 + &laNewQty[3] ,;
  nLotQty4   With nLotQty4 + &laNewQty[4] ,;
  nLotQty5   With nLotQty5 + &laNewQty[5] ,;
  nLotQty6   With nLotQty6 + &laNewQty[6] ,;
  nLotQty7   With nLotQty7 + &laNewQty[7] ,;
  nLotQty8   With nLotQty8 + &laNewQty[8] ,;
  nLotTotQty With nLotTotQty + &laNewQty[9] ,;
  dTranDate  With oAriaApplication.SystemDate  ,;
  DueDate    With {}           ,;
  TranCd     With lcTranCode   ,;
  cTrgOpr    With Space(2)     ,;
  cTrgLot    With Space(2)     ,;
  cOwner     With ' '			 ,;
  cInvType   With lcInvType
*N038893,1 WAM 06/02/2005 (End)

*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
Release llExists
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]

Return(.T.)


*!***************************************************************************
*! Name      : lfUpdWip
*! Developer : Khalid Mohi El-Din
*! Date      : 09/11/2004
*! Purpose   : Function to update the usage fields in ItemLoc
*!******************************************************************************
*! Parameters: loFormSet   : FormSet
*!******************************************************************************
Function lfUpdWip

Private lcTmpWip,lcTmpWip1
lcTmpWip  = gfTempName()
lcTmpWip1 = gfTempName()
Local Array laFileStru[12,18]
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

Local lnI,lnJ
For lnI = 1 To 12
  For lnJ = 7 To 16
    laFileStru[lnI,lnJ] = ''
  Endfor
  Store 0 To laFileStru[lnI,17],laFileStru[lnI,18]
Endfor

Local Array laIndex[2,2]
laIndex[1,1] = 'DTOS(DTRDATE)+CWARECODE'
laIndex[1,2] = lcTmpWip
laIndex[2,1] = 'CWARECODE+CISESSION'
laIndex[2,2] = lcTmpWip1
=gfCrtTmp(lcTmpWip,@laFileStru,@laIndex)
Set Order To Tag (lcTmpWip) In (lcTmpWip)

Local lcBomLine,lcMatInvJl,laCurrRecQ
lcBomLine  = gfTempName()
lcMatInvJl = gfTempName()
m.cIMTyp   = Iif(llMFCall,'M',Iif(&lcTmpLine..cStyType="N","N",'I'))
m.cType    = '2'
m.cTktNo   = Evaluate(lcMastPoHd+'.PO')
Dimension laCurrRecQ[8]
laCurrRecQ = 0

Select (lcTmpLine)

Private lcSeekExpr,lcWhileExp,lnOpnQty,lnOpnQtySt,lnI,lcI,lcOrder,lnBomLnRec,lcFabric,lcDyelot

If llMFCall
  lcSeekExpr = 'M1'+Space(6)+PO+Str(Lineno,6)
  **: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [Start]
  *lcWhileExp = "cIMTyp+cType+cTktNo+STR(lineno,6)=lcSeekExpr"
  lcWhileExp = "cIMTyp+cType+Shipno+cTktNo+STR(LineNo,6)=lcSeekExpr"
  *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [End]
Else
  lcSeekExpr = Iif(lcRecvType = 'D','D','I')+'1'+Iif(lcRecvType = 'S',ShipNo,Space(6))+PO+Str(Lineno,6)
  lcWhileExp = "cIMTyp+cType+Shipno+cTktNo+STR(LineNo,6)=lcSeekExpr"
Endif

If Seek(lcSeekExpr,lcMastBomLn)
  Select (lcMastBomLn)
  Scan Rest While &lcWhileExp. For CCATGTYP $ 'FT'
    For lnI = 1 To Len(Alltrim(CSIZES))
      lcI = Substr(Alltrim(CSIZES),lnI,1)
      laCurrRecQ[lnI] = laCurrRecQ[lnI]+ Min(Evaluate(lcTmpLine+'.Qty'+lcI), laOpnQty[lnI])
    Endfor
    lnBomLnRec = Recno()
    lcFabric   = Item
    lcDyelot   = Dyelot
    *: B607982,1 MMT 02/20/2007 Get item used quantity for previous receivings  [Start]
    lnOpnQtySt = 0
    If llMFCall
      If Seek('M2'+Space(6)+Evaluate(lcTmpLine+'.PO'))
        Sum Rest While cIMTyp+cType+ShipNo+cTktNo+Str(Lineno,6)='M2'+Space(6)+Evaluate(lcTmpLine+'.PO');
          FOR Item = lcFabric .And. Dyelot = lcDyelot .And. !Empty(cRSession) ItemQty To lnOpnQtySt
      Endif
    Else
      If Seek(Iif(lcRecvType ='D','D','I')+'2'+Iif(lcRecvType ='S',Evaluate(lcTmpLine+'.Shipno'),Space(6))+Evaluate(lcTmpLine+'.Po'))
        Sum Rest While cIMTyp+cType+ShipNo+cTktNo+Str(Lineno,6)=;
          IIF(lcRecvType ='D','D','I')+'2'+Iif(lcRecvType ='S',Evaluate(lcTmpLine+'.Shipno'),Space(6))+Evaluate(lcTmpLine+'.Po');
          FOR Item = lcFabric .And. Dyelot = lcDyelot .And. !Empty(cRSession) ItemQty To lnOpnQtySt
      Endif
    Endif
    If Between(lnBomLnRec,1,Reccount())
      Go lnBomLnRec
    Endif
    *: B607982,1 MMT 02/20/2007 Get item used quantity for previous receivings  [End]

    Select (lcTmpWip)
    Zap
    *-- CINVTYPE+STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)

    *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
    *lcSqlStatement  = "SELECT * FROM ITEMJRNL [INDEX=STYINVJL] "+;
    "WHERE cInvType='" + '0002' + "' AND Style ='"+ lcFabric + "'"
    lcSqlStatement  = "SELECT * FROM ITEMJRNL [INDEX=STYINVJL] "+;
      "WHERE cInvType='" + '0002' + "' AND Style = ?m.lcFabric "
    *B607658,1 KHM 07/07/2005 [End]

    =lfOpenSql(lcSqlStatement,'ITEMJRNL',lcMatInvJl, "","",.F.)
    Dimension laIndex[1,2]
    laIndex = ''
    laIndex[1,1] = 'CINVTYPE+STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)'
    laIndex[1,2] = lcMatInvJl
    =lfSetIndex(lcMatInvJl,@laIndex)

    Select (lcMastBomLn)

    If Seek('0002'+lcFabric,lcMatInvJl)
      Select (lcMatInvJl)
      **: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [Start]
      *SCAN REST WHILE CINVTYPE+STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)=;
      '0002'+lcFabric;
      FOR CDYELOT = lcDYELOT .AND. CTRTYPE = '9' .AND. CIMTYP = m.cImTyp .AND.;
      CTKTNO = EVALUATE(lcTmpLine+'.PO')
      Scan Rest While cInvType+Style+cWareCode+CSESSION+Dtos(DTRDATE)+CTRCODE+Str(Lineno,6)=;
          '0002'+lcFabric;
          FOR cDyelot = lcDyelot .And. CTRTYPE = '9' .And. Iif(m.cIMTyp='M',cBusDocu+cStyType = 'PU',cBusDocu+cStyType = 'PP') .And.;
          CTRCODE = Evaluate(lcTmpLine+'.PO')

        Select (lcTmpWip)
        Set Order To Tag (lcTmpWip1)
        If !Seek(Evaluate(lcMatInvJl+'.CWARECODE')+Evaluate(lcMatInvJl+'.CISESSION'))
          Insert Into (lcTmpWip) (cWareCode,DTRDATE,cISession);
            VALUES (Evaluate(lcMatInvJl+'.CWARECODE'),Evaluate(lcMatInvJl+'.DTRDATE'),Evaluate(lcMatInvJl+'.CISESSION'))
        Endif
        Set Order To Tag (lcTmpWip)
        Select (lcMatInvJl)

        *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage  [End]
        If CIRTYPE = 'I'
          **: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [Start]
          * INSERT INTO (lcTmpWip) (CWARECODE,NCUSAGE1,NCUSAGE2,NCUSAGE3,NCUSAGE4,NCUSAGE5,NCUSAGE6,NCUSAGE7,NCUSAGE8,;
          NTOTCUSA,DTRDATE,CISESSION);
          VALUES (EVALUATE(lcMATINVJL+'.CWARECODE'),EVALUATE(lcMATINVJL+'.NSTK1')*-1,;
          EVALUATE(lcMATINVJL+'.NSTK2')*-1,EVALUATE(lcMATINVJL+'.NSTK3')*-1,;
          EVALUATE(lcMATINVJL+'.NSTK4')*-1,EVALUATE(lcMATINVJL+'.NSTK5')*-1,;
          EVALUATE(lcMATINVJL+'.NSTK6')*-1,EVALUATE(lcMATINVJL+'.NSTK7')*-1,;
          EVALUATE(lcMATINVJL+'.NSTK8')*-1,EVALUATE(lcMATINVJL+'.NTOTSTK')*-1,;
          EVALUATE(lcMATINVJL+'.DTRDATE'),EVALUATE(lcMATINVJL+'.CISESSION'))
          Select (lcTmpWip)
          =Seek(Evaluate(lcMatInvJl+'.CWARECODE')+Evaluate(lcMatInvJl+'.CISESSION'),lcTmpWip,lcTmpWip1)
          Replace  NCUSAGE1 With NCUSAGE1 + Evaluate(lcMatInvJl+'.NSTK1')*-1 ,;
            NCUSAGE2 With NCUSAGE2 + Evaluate(lcMatInvJl+'.NSTK2')*-1 ,;
            NCUSAGE3 With NCUSAGE3 + Evaluate(lcMatInvJl+'.NSTK3')*-1 ,;
            NCUSAGE4 With NCUSAGE4 + Evaluate(lcMatInvJl+'.NSTK4')*-1 ,;
            NCUSAGE5 With NCUSAGE5 + Evaluate(lcMatInvJl+'.NSTK5')*-1 ,;
            NCUSAGE6 With NCUSAGE6 + Evaluate(lcMatInvJl+'.NSTK6')*-1 ,;
            NCUSAGE7 With NCUSAGE7 + Evaluate(lcMatInvJl+'.NSTK7')*-1 ,;
            NCUSAGE8 With NCUSAGE8 + Evaluate(lcMatInvJl+'.NSTK8')*-1 ,;
            NTOTCUSA With NTOTCUSA + Evaluate(lcMatInvJl+'.NTOTSTK')*-1

          *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [End]
        Else
          Select (lcTmpWip)
          Set Order To Tag (lcTmpWip1)
          *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [Start]
          *IF SEEK(EVALUATE(lcMATINVJL+'.CWARECODE')+EVALUATE(lcMATINVJL+'.CISESSION'))
          If Seek(Evaluate(lcMatInvJl+'.CWARECODE')+Evaluate(lcMatInvJl+'.CISESSION'),lcTmpWip,lcTmpWip1)
            *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [End]
            Replace NCUSAGE1 With NCUSAGE1 - Evaluate(lcMatInvJl+'.NSTK1'),;
              NCUSAGE2 With NCUSAGE2 - Evaluate(lcMatInvJl+'.NSTK2'),;
              NCUSAGE3 With NCUSAGE3 - Evaluate(lcMatInvJl+'.NSTK3'),;
              NCUSAGE4 With NCUSAGE4 - Evaluate(lcMatInvJl+'.NSTK4'),;
              NCUSAGE5 With NCUSAGE5 - Evaluate(lcMatInvJl+'.NSTK5'),;
              NCUSAGE6 With NCUSAGE6 - Evaluate(lcMatInvJl+'.NSTK6'),;
              NCUSAGE7 With NCUSAGE7 - Evaluate(lcMatInvJl+'.NSTK7'),;
              NCUSAGE8 With NCUSAGE8 - Evaluate(lcMatInvJl+'.NSTK8'),;
              NTOTCUSA With NTOTCUSA - Evaluate(lcMatInvJl+'.NTOTSTK')
          Endif
          Set Order To Tag (lcTmpWip)
        Endif
      Endscan
    Endif
    If Reccount(lcTmpWip) > 0
      Store 0 To lnRemQty, lnRemQtySt
      For lnCount = 1 To 8
        laCurrRecQ[lnCount] = laCurrRecQ[lnCount]*Evaluate(lcMastBomLn+'.UNITQTY')
        lnRemQty   = lnRemQty + laCurrRecQ[lnCount]
        lnRemQtySt = lnRemQtySt + laPrevRecQ[lnCount]
      Endfor
      *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage  [Start]
      lnRemQtySt = lnOpnQtySt
      *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [End]

      Select (lcTmpWip)
      Locate
      Scan

        *E039550,1 WSH 08/07/2005 Add Totals for Quantity Fields in the Item File. [Start]
        lcKey = '0002'+Evaluate(lcMastBomLn+'.Item')
        *B607935,1 WAM 01/17/2007 Fix error "variable lcTmpItm not found' while saving style PO
        *IF !SEEK(lcKey,lcTmpItmDye)
        If !Seek(lcKey,lcTmpItm)
          *B607935,1 WAM 01/17/2007 (End)
          lcItemValue    = Evaluate(lcMastBomLn+'.Item')
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
        Endif
        *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [Start]
        =Seek(lcKey,lcTmpItm)
        Select (lcTmpItm)
        Replace NTOTCUSA With NTOTCUSA &lcWipSgn Max(Min(Evaluate(lcTmpWip+'.NCUSAGE1')-lnRemQtySt,lnRemQty),0)
        Select (lcTmpWip)
        *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage [End]

        lcKey = '0002'+Evaluate(lcMastBomLn+'.Item')+cWareCode
        If !Seek(lcKey,lcTmpItmDye)
          *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
          *lcSqlStatement = "SELECT * FROM ITEMLOC WHERE cInvType = '0002'"+;
          " AND Style ='" + EVALUATE(lcMastBomLn+'.Item') +;
          "' AND cWareCode='" + cWareCode +;
          "' AND Dyelot = '" + SPACE(10) + "'"

          lcItemValue   = Evaluate(lcMastBomLn+'.Item')
          lcWareCodeVal = cWareCode
          lcSqlStatement = "SELECT * FROM ITEMLOC WHERE cInvType = '0002'"+;
            " AND Style = ?m.lcItemValue " +;
            " AND cWareCode = ?m.lcWareCodeVal " +;
            " AND Dyelot = '" + Space(10) + "'"
          *B607658,1 KHM 07/07/2005 [End]
          =lfGetItmInf('0002',lcKey+Space(10),lcTmpItmDye,'ITEMLOC',;
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
        Endif
        *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage [Start]
        =Seek(lcKey+Space(10),lcTmpItmDye)
        Select (lcTmpItmDye)
        Replace NCUSAGE1 With NCUSAGE1 &lcWipSgn Max(Min(Evaluate(lcTmpWip+'.NCUSAGE1')-lnRemQtySt,lnRemQty),0),;
          NTOTCUSA With NCUSAGE1
        Select (lcTmpWip)
        *: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage [End]
        If !Empty(lcDyelot)
          *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
          *lcSqlStatement = "SELECT * FROM ITEMLOC WHERE cInvType = '0002'"+;
          " AND Style ='" + EVALUATE(lcMastBomLn+'.Item') +;
          "' AND cWareCode='" + cWareCode +;
          "' AND Dyelot = '" + lcDyelot + "'"

          lcItemValue   = Evaluate(lcMastBomLn+'.Item')
          lcWareCodeVal = cWareCode
          lcSqlStatement = "SELECT * FROM ITEMLOC WHERE cInvType = '0002'"+;
            " AND Style = ?m.lcItemValue " + ;
            " AND cWareCode = ?m.lcWareCodeVal " +;
            " AND Dyelot = '" + lcDyelot + "'"
          *B607658,1 KHM 07/07/2005

          =lfGetItmInf('0002',lcKey+lcDyelot,lcTmpItmDye,'ITEMLOC',lcSqlStatement,.T.)
          If Seek(lcKey+lcDyelot,lcTmpItmDye)
            Select (lcTmpItmDye)
            Replace NCUSAGE1 With NCUSAGE1 &lcWipSgn Max(Min(Evaluate(lcTmpWip+'.NCUSAGE1')-lnRemQtySt,lnRemQty),0),;
              NTOTCUSA With NCUSAGE1
          Endif
        Endif
        **: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [Start]
        lnRemQty   = Min(Max(lnRemQty-Evaluate(lcTmpWip+'.NCUSAGE1')+lnRemQtySt,0),lnRemQty)
        lnRemQtySt = Max(lnRemQtySt-Evaluate(lcTmpWip+'.NCUSAGE1'),0)
        **: B607982,1 MMT 02/20/2007 Fix bug of Wrong update of material usage   [End]
      Endscan
    Endif
  Endscan
Endif
Use In (lcTmpWip)
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
Release lcTmpWip,lcTmpWip1,laFileStru,lnI,lnJ,laIndex,lcBomLine,lcMatInvJl,laCurrRecQ,;
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
Function lfSetKeyPro
Lparameters loFormSet, lcRecvType
Private lcVendor

Do Case
  *-- 'S' Receeve Style PO Shipment, 'F' Receiving Material PO Shipment
  *-- 'C' Receive Inter-Location PO Shipment
Case lcRecvType $ "SFCU"
  Local llNoRecordFound, llServerError

  With loFormSet
    .cBrowseFileName        = "SHPMTHDR"
    .cBrowseIndexExpression = "CBUSDOCU+CSHPTYPE+SHIPNO"
    .cBrowseIndexFields     = "CBUSDOCU,CSHPTYPE,SHIPNO"
    .cBrowseIndexName       = "SHPMTHDR"
    .cBrowseKey             = loFormSet.lcBusDoc + loFormSet.lcWorkOrd
    .cBrowseAliasName       = loFormSet.lcMastShp
    .cBrowseTableName       = "SHPMTHDR"
    .oRemoteCursor.mGetCursor(loFormSet.lcMastShp,loFormSet.lcMastShp,.DataSessionId,;
      'STRUCTURE',.F.,.cBrowseTableName,'*','',.cBrowseIndexName,;
      .cBrowseIndexExpression,.F.,.cBrowseIndexFields,0,;
      .cBrowseFilter,.cBrowseKey,@llNoRecordFound,@llServerError)
    .cbrowsefields = "ShipNo:H='Shipment #', Status, Entered, Cartons,"+;
      "AirWayB:H='Air-way Bill#', ETA:H='E.T.A.',"+;
      "TotQtyHdr:H='In-Transit', Recv_Stk:H='Received',"+;
      "Recv_Dam:H='Damaged', Recv_Can:H='Canceled',"+;
      "cVessel:H='Airline / Vessel' ,Reference"

  Endwith


  *-- 'L' Receive Inter Location P/O Batch, 'T' Receive C/T Batch, 'B' Receive P/O Batch
Case lcRecvType $ "LTB"

Otherwise
  loFormSet.cBrowseAliasName       = loFormSet.lcPosHdr
  loFormSet.cBrowseKey             = loFormSet.lcBusDoc + loFormSet.lcWorkOrd
  loFormSet.cBrowseIndexExpression = "CBUSDOCU+CSTYTYPE+PO"
  loFormSet.cBrowseIndexFields     = "CBUSDOCU,CSTYTYPE,PO"
  loFormSet.cBrowseIndexName       = "POSHDR"
  loFormSet.cBrowseTableName       = "POSHDR"
  loFormSet.cBrowseFileName        = "POSHDR"

  With loFormSet.ariaform1.kbPoNo
    .cBusinessDocumentType = loFormSet.lcBusDoc
    .cWorkOrderType        = loFormSet.lcWorkOrd
    .oBrowseCursor         = loFormSet.lcPosHdr
    *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
    If loFormSet.lcWorkOrd <> 'U'
      *N038893,1 WAM 06/02/2005 (End)
      .cbrowsetitle          = Iif(loFormSet.lcWorkOrd = 'N',;
        IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_IntrLocBrowseTitle,loFormSet.GetHeaderText("LANG_POSTREC_IntrLocBrowseTitle",loFormSet.HeaderAlias)),Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BrowseTitle,loFormSet.GetHeaderText("LANG_POSTREC_BrowseTitle",loFormSet.HeaderAlias)))
      lcVendor = Iif(loFormSet.lcWorkOrd = 'N',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_SourceLBrowTitl,loFormSet.GetHeaderText("LANG_POSTREC_SourceLBrowTitl",loFormSet.HeaderAlias)),;
        IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_VendorBrowTitl,loFormSet.GetHeaderText("LANG_POSTREC_VendorBrowTitl",loFormSet.HeaderAlias)))
      .cbrowsefields = "PO        :H='"+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_POBrowTitl,loFormSet.GetHeaderText("LANG_POSTREC_POBrowTitl",loFormSet.HeaderAlias)) +"',"+;
        "Status    :H='"+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_StatusBrowTitl,loFormSet.GetHeaderText("LANG_POSTREC_StatusBrowTitl",loFormSet.HeaderAlias)) +"',"+;
        "Vendor    :H='"+lcVendor +"' ,"+;
        "Entered   :H='"+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_EnteredBrowTitl,loFormSet.GetHeaderText("LANG_POSTREC_EnteredBrowTitl",loFormSet.HeaderAlias))+"' ,"+;
        "Complete  :H='"+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_CompleteBrowTitl,loFormSet.GetHeaderText("LANG_POSTREC_CompleteBrowTitl",loFormSet.HeaderAlias))+"' ,"+;
        "nStyOrder :H='"+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TotQtyBrowTitl,loFormSet.GetHeaderText("LANG_POSTREC_TotQtyBrowTitl",loFormSet.HeaderAlias))+"' ,"+;
        "POTotal   :H='"+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_AmountBrowTitl,loFormSet.GetHeaderText("LANG_POSTREC_AmountBrowTitl",loFormSet.HeaderAlias))+"' ,"+;
        "Receive   :H='"+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ReceiveBrowTitl,loFormSet.GetHeaderText("LANG_POSTREC_ReceiveBrowTitl",loFormSet.HeaderAlias))+"' ,"+;
        "Open      :H='"+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_OpenBrowTitl,loFormSet.GetHeaderText("LANG_POSTREC_OpenBrowTitl",loFormSet.HeaderAlias))+"' "

      *N038893,1 WAM 06/02/2005 Receive Cutting Ticket
    Else
      *N000682,1 MMT 12/09/2012 Globalization changes[Start]
      *!*        .cbrowsetitle  = 'Cutting Tickets'
      *!*        .cbrowsefields = "PO          :H='"+"Cutkt#"+"' ,"+;
      *!*          "Status    :H='"+"Status"+"' ,"+;
      *!*          "Style     :H='"+"Style"+"' ,"+;
      *!*          "cDivision :H='"+"Division"+"' ,"+;
      *!*          "Entered   :H='"+"Entered"+"' ,"+;
      *!*          "Complete  :H='"+"Complete"+"' ,"+;
      *!*          "nStyOrder :H='"+"Total Qty."+"' ,"+;
      *!*          "POTotal   :H='"+"Amount"+"' ,"+;
      *!*          "Receive   :H='"+"Received"+"' ,"+;
      *!*          "Open      :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_OpenBrowTitl,loFormSet.GetHeaderText("LANG_POSTREC_OpenBrowTitl",loFormSet.HeaderAlias))+"' "
      .cbrowsetitle  = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TITLECT,loFormSet.GetHeaderText("LANG_POSTREC_TITLECT",loFormSet.HeaderAlias))
      .cbrowsefields = "PO          :H='"+;
        IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BROWCT,loFormSet.GetHeaderText("LANG_POSTREC_BROWCT",loFormSet.HeaderAlias))+"' ,"+;
        "Status    :H='"+;
        IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_StatusBrowTitl,loFormSet.GetHeaderText("LANG_POSTREC_StatusBrowTitl",loFormSet.HeaderAlias))+"' ,"+;
        "Style     :H='"+;
        IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MSG_STYLE,loFormSet.GetHeaderText("LANG_POSTREC_MSG_STYLE",loFormSet.HeaderAlias))+"' ,"+;
        "cDivision :H='"+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BROWDIVISION,loFormSet.GetHeaderText("LANG_POSTREC_BROWDIVISION",loFormSet.HeaderAlias))+"' ,"+;
        "Entered   :H='"+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_EnteredBrowTitl,loFormSet.GetHeaderText("LANG_POSTREC_EnteredBrowTitl",loFormSet.HeaderAlias))+"' ,"+;
        "Complete  :H='"+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_CompleteBrowTitl,loFormSet.GetHeaderText("LANG_POSTREC_CompleteBrowTitl",loFormSet.HeaderAlias))+"' ,"+;
        "nStyOrder :H='"+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TotQtyBrowTitl,loFormSet.GetHeaderText("LANG_POSTREC_TotQtyBrowTitl",loFormSet.HeaderAlias))+"' ,"+;
        "POTotal   :H='"+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_AmountBrowTitl,loFormSet.GetHeaderText("LANG_POSTREC_AmountBrowTitl",loFormSet.HeaderAlias))+"' ,"+;
        "Receive   :H='"+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ReceiveBrowTitl,loFormSet.GetHeaderText("LANG_POSTREC_ReceiveBrowTitl",loFormSet.HeaderAlias))+"' ,"+;
        "Open      :H='"+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_OpenBrowTitl,loFormSet.GetHeaderText("LANG_POSTREC_OpenBrowTitl",loFormSet.HeaderAlias))+"' "
      *N000682,1 MMT 12/09/2012 Globalization changes[end]
    Endif
    *N038893,1 WAM 06/02/2005 (End)
    Select(.oBrowseCursor)
  Endwith
Endcase

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
Function lfSelLots
Lparameters lcTktLn,lcHdrLOpr,loFormSet
Local lnAlias, lcSqlStatement, lcCstShtType
Private lcTmpCur,lcTmpCode
lnAlias  = Select()
lcTmpCur = gfTempName()
lcTmpCode = gfTempName()
*-- To check the type of style cost sheet
Do Case
  *-- 'I' Receive P/O
Case loFormSet.lcPType $ 'ISB'
  lcCstShtType = 'I'

  *-- 'M' Receive C/T
Case loFormSet.lcPType $ 'MT'
  lcCstShtType = 'M'

  *-- 'N' Issue Inter-Location P/O, 'O' Receive Inter-Location P/O
Case loFormSet.lcPType $ 'NO'
  lcCstShtType = 'N'
Endcase

*-- Get BOMLINE records
lcSqlStatement = "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+;
  "WHERE cImTyp = '" + lcCstShtType +;
  "' AND cTktNo ='" + loFormSet.lcPoNo +;
  "' AND cCatgTyp = 'M'" + " AND MfgCode <> SPACE(6)" + ;
  "  AND [LineNo] = '" + lcTktLn + "'"
=lfOpenSql(lcSqlStatement,'BOMLINE',lcTmpCur, "","",.F.)

Select (lcTmpCur)
If Empty(lcHdrLOpr)

  *--Get the Last operation for this Style/Color LINE NO.
  Store ' ' To lcLastMfgOr
  lnMaxSeq = 0
  Scan
    *--Check if this MFG operation code works as operation.
    llWrkAsOpr = .F.
    lnCurSeqn  = 0

    lcSqlStatement = "SELECT * FROM CODES " + ;
      "WHERE cDefCode+ccode_no+crltfield+cfld_name = 'N'"+;
      "+'"+Padr(MFGCODE,6)+"'+'Y'+'MFGCODE   ' AND CRLTD_NAM = 'LMFGOPR'"
    =lfOpenFox(lcSqlStatement,'CODES',lcTmpCode,"")

    Select(lcTmpCode)
    Locate

    If Found()
      llWrkAsOpr = Allt(Evaluate(lcTmpCode+'.cRltd_vlu')) = "T"
      lcSqlStatement = "SELECT * FROM CODES " + ;
        "WHERE cDefCode+ccode_no+crltfield+cfld_name = 'N'"+;
        "+'"+Padr(Evaluate(lcTmpCur+'.MfgCode'),6)+"'+'Y'+'MFGCODE   ' AND CRLTD_NAM = 'COPERSEQ'"
      =lfOpenFox(lcSqlStatement,'CODES',lcTmpCode,"")
      Select(lcTmpCode)
      Locate

      lnCurSeqn = Iif(Found(),Int(Val(Allt(Evaluate(lcTmpCode+'.cRltd_vlu')))),0)
    Endif

    Select (lcTmpCur)
    If !llWrkAsOpr
      Loop
    Endif

    *-- Read last operation for sequence is last.
    If lnMaxSeq <= lnCurSeqn
      *--Read last operation.
      lcLastMfgOr = MFGCODE
      lnMaxSeq = lnCurSeqn
    Endif

  Endscan

Else
  lcLastMfgOr = lcHdrLOpr
Endif

*--If not empty last operation , read receive lot.
lcRecvLot = '  '
If !Empty(lcLastMfgOr)

  *-- cimtyp+ctktno+coprcode+trancd
  lcSqlStatement = "SELECT * FROM MFGOPRDT [INDEX=TKTOPTRN] "+;
    "WHERE cImTyp = '" + lcCstShtType +;
    "' AND cTktNo = '" + loFormSet.lcPoNo +;
    "' AND cOprCode = '" + lcLastMfgOr + "'"
  =lfOpenSql(lcSqlStatement,'MFGOPRDT',lcTmpCur, "","",.F.)

  Select (lcTmpCur)
  Locate
  lnNumLots = 0
  Count To lnNumLots ;
    FOR cIMTyp+cTktNo+cOprCode+TranCd = ;
    lcCstShtType+loFormSet.lcPoNo+lcLastMfgOr+'1' And Item = loFormSet.lcStyle ;
    AND nLotTotQty >0
  If lnNumLots <> 0
    Locate For cIMTyp+cTktNo+cOprCode+TranCd = ;
      lcCstShtType+loFormSet.lcPoNo+lcLastMfgOr+'1' And Item = loFormSet.lcStyle ;
      AND cDyelot = Padr(loFormSet.lcDyelot,10)
    Do Case
    Case lnNumLots = 1
      lcRecvLot = cLotNo
    Case lnNumLots > 1
      *--Do you want to select a lot to receive to or distribute received quantity to all lots?,\<Select;\<Distribute
      If gfModalGen('QRM42108B42011','DIALOG') = 1
        *N000682,1 MMT 12/09/2012 Globalization changes[Start]
        *!*          lcBrFields = [cLotNo    :H='Lot No.',]+;
        *!*            [cOprCode  :H='Operation',]+;
        *!*            [cContCode :H='Cont./Dept.',]+;
        *!*            [cContName :H='Name',]+;
        *!*            [dTranDate :H='Trans.Date',]+;
        *!*            [dueDate   :H='Due Date',]+;
        *!*            [nLotTotQty :H='Total']
        lcBrFields = [cLotNo    :H=']+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_LOTBROW_LOTNO,loFormSet.GetHeaderText("LANG_POSTREC_LOTBROW_LOTNO",loFormSet.HeaderAlias))+[',]+;
          [cOprCode  :H=']+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_LOTBROW_OPERATION,loFormSet.GetHeaderText("LANG_POSTREC_LOTBROW_OPERATION",loFormSet.HeaderAlias))+[',]+;
          [cContCode :H=']+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_LOTBROW_CONTDEPT,loFormSet.GetHeaderText("LANG_POSTREC_LOTBROW_CONTDEPT",loFormSet.HeaderAlias))+[',]+;
          [cContName :H=']+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_LOTBROW_NAME,loFormSet.GetHeaderText("LANG_POSTREC_LOTBROW_NAME",loFormSet.HeaderAlias))+[',]+;
          [dTranDate :H=']+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_LOTBROW_TRANSDATE,loFormSet.GetHeaderText("LANG_POSTREC_LOTBROW_TRANSDATE",loFormSet.HeaderAlias))+[',]+;
          [dueDate   :H=']+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_LOTBROW_DUEDATE,loFormSet.GetHeaderText("LANG_POSTREC_LOTBROW_DUEDATE",loFormSet.HeaderAlias))+[',]+;
          [nLotTotQty :H=']+Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_LOTBROW_TOTAL,loFormSet.GetHeaderText("LANG_POSTREC_LOTBROW_TOTAL",loFormSet.HeaderAlias))+[']
        *N000682,1 MMT 12/09/2012 Globalization changes[eND]
        Dimension laTemp[1]
        laTemp = ''
        *N000682,1 MMT 12/09/2012 Globalization changes[Start]
        *!*          =ARIABROW([FOR Item=loFormSet.lcStyle;
        *!*                    .AND. cDyelot = PADR(loFormSet.lcDyelot,10)],'Lots',gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','cLotNo','laTemp')
        =ARIABROW([FOR Item=loFormSet.lcStyle;
                  .AND. cDyelot = PADR(loFormSet.lcDyelot,10)],Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_LOTBROW_LOTS,loFormSet.GetHeaderText("LANG_POSTREC_LOTBROW_LOTS",loFormSet.HeaderAlias)),gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','cLotNo','laTemp')
        *N000682,1 MMT 12/09/2012 Globalization changes[end]
        lcRecvLot=laTemp[1]
        If !Empty(lcRecvLot)
          loFormSet.llSpecLot = .T.
          Dimension loFormSet.laLotArry[8]
          Store 0 To loFormSet.laLotArry
          Select (lcTmpCur)
          Locate For cIMTyp+cTktNo+cOprCode+TranCd = ;
            lcCstShtType+loFormSet.lcPoNo+lcLastMfgOr+'1' And ;
            ITEM = loFormSet.lcStyle And cDyelot = Padr(loFormSet.lcDyelot,10)

          Scan For cIMTyp+cTktNo+cOprCode = lcCstShtType+loFormSet.lcPoNo+lcLastMfgOr+'1';
              AND Item = loFormSet.lcStyle And cLotNo = lcRecvLot

            For I=1 To 8
              Z=Str(I,1)
              loFormSet.laLotArry[I]=loFormSet.laLotArry[I]+(nLotQty&Z * Iif(TranCd='1',1,-1))
            Endfor
          Endscan
        Endif
      Endif
    Endcase
  Else
    lcRecvLot = '01'
  Endif
Endif

Select (lnAlias)
*--Read last operation for color.
loFormSet.lcClrLstOp = lcLastMfgOr
lcRecvLot  = Iif(Empty(lcRecvLot),Space(2),lcRecvLot)
Use In (lcTmpCur)
Return(lcRecvLot)

*!*************************************************************
*! Name      : lfvEditQty
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/4/2004
*! Purpose   : Edit line quantity.
*!*************************************************************
Function lfvEditQty
Lparameters loFormSet

*C102172,4 AMH Check if coming for reciving by lot [Start]
*!*  IF ASCAN(laEvntTrig,PADR("ADOPTCAT",10)) <> 0 .AND. WEXIST('MFRECLOT')
*!*    lcTmpLine = lcTktSheet
*!*    ON KEY LABEL ESC     &lcHldEsc
*!*  ENDIF
*C102172,4 AMH [End]

Private lcRelCode, lcUOM_B, lcUOM_V, lnConf, lnRetVal
lcRelCode = Evaluate(loFormSet.lcTmpLine+'.cUomCode')
lcUOM_B   = ''
lcUOM_V   = ''
lnConf    = 1

=gfGetUOMData(lcRelCode, lcUOM_B, @lcUOM_V, @lnConf, .F.)

Private lcwarecode,lcDyelot
lcwarecode = Evaluate(loFormSet.lcTmpLine+'.cWareCode')
If loFormSet.llWareHous And Empty(lcwarecode)
  *-- Message : 'First, you must select location.'
  =gfModalGen('TRM42150B42001','DIALOG')
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
  *loFormSet.AriaForm1.cboLocations.SetFocus
  If !loFormSet.llSilentMod
    loFormSet.ariaform1.cboLocations.SetFocus
  Endif
  *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
  Return
Endif

Select (loFormSet.lcTmpLine)
Set Filter To
lcDyelot = Dyelot
Scatter Memvar
lcLKey = cCarton+PO+Style+Dyelot+Padr(cWareCode,6)+Str(Lineno,6)

Dime laOrg[8],laSok[8],laDam1[8],laDam2[8],laCan[8],laBal[8],laAlo[8],laBud[8],laZero[9]
Store 0 To laOrg,laSok,laDam1,laDam2,laCan,laBal,laAlo,laZero,laBud

Dimension laOldOut[8]
Store 0 To laOldOut
Dimension laConst[8]    && Original qty.
Store 0 To laConst
Dimension laRemain[9]   &&-- which carry the last receive qty
Store 0 To laRemain

*--New Origenal and order quantity.
=Seek('1'+lcLKey)
Scatter Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 To laOrg
Scatter Fields Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8 To laAlo

If loFormSet.lcInvType = "0001"
  For lnCntr = 1 To 8
    laOrg[lnCntr] = Int(laOrg[lnCntr])
  Endfor
Endif
*-- If edit cost per line (Option menu)
Store .F. To llSpecLot

If loFormSet.lcPType = 'H'
  *!*    lcBdgLine = 'N'+lcBatch+po+style+dyelot
  *!*    *--Budjet total ordered quantity.
  *!*    SELECT CTKTRCVL
Else

  lcBdgLine = cBusDocu+cStyType+PO+cInvType+Style+Str(Lineno,6)+TranCd
  *-- Case Shipment get the budget line
  *-- 'S' Receive by Shipment, 'U' Issue Inter Location P/O Shipment
  *-- 'C' Receive Inter Location P/O Shipment, 'F' Receive Material PO Shipment
  If loFormSet.lcPType $ 'SUCF' And !Seek(lcBdgLine,loFormSet.lcPosLn)
    lcSqlStatement  =  "SELECT  POSLN.*, POSHDR.cPriceCur, POSHDR.cDutyCur, POSHDR.Status "+;
      "FROM POSLN posln (INDEX=POSLN) inner join POSHDR (INDEX=POSHDR) "+;
      "ON POSHDR.cBusDocu = POSLN.cBusDocu AND POSHDR.cStyType = POSLN.cStyType "+;
      "AND  POSHDR.PO = POSLN.PO "+;
      "WHERE POSLN.cBusDocu = '" + cBusDocu +;
      "' AND POSLN.cStyType = '" + cStyType +;
      "' AND POSLN.PO = '" + PO +;
      "' AND POSLN.cInvType ='" + cInvType +;
      "' AND POSLN.Style ='" + Style +;
      "' AND [LineNo] = " + Alltrim(Str(Lineno)) +;
      " AND (TranCd = '" + TranCd + ;
      "' OR TranCd = '2' OR TranCd = '4' OR TranCd = '5')"

    =lfOpenSql(lcSqlStatement,'POSLN',loFormSet.lcTmpCurs, "","",.F.)
    Select (loFormSet.lcTmpCurs)
    Locate
    If !Eof()
      Scan
        Scatter Memvar Memo
        Select (loFormSet.lcPosLn)
        Insert Into (loFormSet.lcPosLn) From Memvar
      Endscan
    Endif
  Endif
  *--Budjet total ordered quantity.
  Select (loFormSet.lcPosLn)
Endif
=Seek(lcBdgLine)

Scatter Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 To laBud
=Acopy(laBud,laConst)

*--Get last receiving for inter location Issue and receive.
Do Case

  *-- [O] Receive Inter-Location P/o, [N] Issue Inter-Location P/o
  *-- [A] Issue Adornment order, [E] Receive Adornment order
  *-- [S] Receive by Shipment, [L] Receive Inter Location P/O Batch
  *-- [C] Receive Inter Location P/O Shipment
  *-- [U] Issue Inter Location P/O Shipment
  *B130601,1 KHM 01/25/2006 Add the receive by material shipment [Stasrt]
  *CASE loFormSet.lcPType $ 'ONAESLCU'
Case loFormSet.lcPType $ 'ONAESLCUF'
  *B130601,1 KHM 01/25/2006 [End]
  lcToseekLn = cBusDocu+cStyType+PO+cInvType+Style+Str(Lineno,6)
  Scan Rest While cBusDocu+cStyType+PO+cInvType+Style+Str(Lineno,6)=lcToseekLn For TranCd $ '2456'
    For lnI = 1 To 8
      lcI=Str(lnI,1)
      laRemain[lnI] = laRemain[lnI] + Iif(loFormSet.lcPType $ 'ONCU' And TranCd = '6',0,Evaluate('Qty'+lcI))
    Endfor
  Endscan
  laRemain[9] = laRemain[1]+laRemain[2]+laRemain[3]+laRemain[4]+laRemain[5]+laRemain[6]+laRemain[7]+laRemain[8]
  =Seek(lcBdgLine)

  *-- [I] Receive P/O, [M] Receive C/T
Case loFormSet.lcPType = 'I'
  If llSpecLot
    *!*        FOR lnI=1 to 8
    *!*          laRemain[lnI] = laLotArry[lnI] - laOrg[lnI]
    *!*          laRemain[9] = laRemain[9] + laRemain[lnI]
    *!*        ENDFOR
  Else
    lcToseekLn = cBusDocu+cStyType+PO+cInvType+Style+Str(Lineno,6)
    Scan While cBusDocu+cStyType+PO+cInvType+Style+Str(Lineno,6)=lcToseekLn For TranCd $ '245'
      For lnI=1 To 8
        lcI=Str(lnI,1)
        laRemain[lnI] = laRemain[lnI] + Iif(loFormSet.lcPType $ 'ON' And TranCd = '6',0,Evaluate('Qty'+lcI))
      Endfor
    Endscan
    laRemain[9] = laRemain[1]+laRemain[2]+laRemain[3]+laRemain[4]+laRemain[5]+laRemain[6]+laRemain[7]+laRemain[8]
    =Seek(lcBdgLine)
  Endif

Otherwise
  For lnI=1 To 8
    If llSpecLot
      *!*          laRemain[lnI] = laLotArry[lnI] - laOrg[lnI]
    Else
      laRemain[lnI] = laConst[lnI] - laOrg[lnI]
    Endif
    laRemain[9] = laRemain[9] + laRemain[lnI]
  Endfor
Endcase

Select (loFormSet.lcTmpLine)
*! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
*lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE='S"+SCALE+"'"
*! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
*lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=[S"+SCALE+"]"
lcScaleValue="S"+Scale
lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=?m.lcScaleValue"
*! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
*! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
=lfOpenFox(lcSqlStatement,'SCALE','SCALE',"")
Select (loFormSet.lcTmpLine)

Store '' To lcRetSty1,lcRetSty2,lcRetSHd,lcRetSHd1,lcRetSHd2,lcRetSHd3,lcRetSHd4
lcMStyQlty = cStyGrade
lcSndGrd   = "2"
lcTrdGrd   = "3"
Do Case
Case lcMStyQlty='1'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *lcRetSHd = LANG_POSTREC_1STQUALITY
  lcRetSHd = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_1STQUALITY,loFormSet.GetHeaderText("LANG_POSTREC_1STQUALITY",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *!*      IF lcPType<>'A' .OR. (ASCAN(laEvntTrig,PADR("RCVADORD",10)) <> 0)
  If loFormSet.lcPType<>'A'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcRetSHd1 = LANG_POSTREC_2NDQUALITY
    lcRetSHd1 = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_2NDQUALITY,loFormSet.GetHeaderText("LANG_POSTREC_2NDQUALITY",loFormSet.HeaderAlias))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcRetSHd2 = LANG_POSTREC_DAMAGED
    lcRetSHd2 = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_DAMAGED,loFormSet.GetHeaderText("LANG_POSTREC_DAMAGED",loFormSet.HeaderAlias))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcRetSHd3 = LANG_POSTREC_SECONDQUALITY
    lcRetSHd3 = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_SECONDQUALITY,loFormSet.GetHeaderText("LANG_POSTREC_SECONDQUALITY",loFormSet.HeaderAlias))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcRetSHd4 = LANG_POSTREC_DAMAGED
    lcRetSHd4 = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_DAMAGED,loFormSet.GetHeaderText("LANG_POSTREC_DAMAGED",loFormSet.HeaderAlias))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

  Endif
Case lcMStyQlty='2'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *lcRetSHd = LANG_POSTREC_2NDQUALITY
  lcRetSHd = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_2NDQUALITY,loFormSet.GetHeaderText("LANG_POSTREC_2NDQUALITY",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  If loFormSet.lcPType<>'A'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcRetSHd1 = LANG_POSTREC_1STQUALITY
    lcRetSHd1 = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_1STQUALITY,loFormSet.GetHeaderText("LANG_POSTREC_1STQUALITY",loFormSet.HeaderAlias))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcRetSHd2 = LANG_POSTREC_DAMAGED
    lcRetSHd2 = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_DAMAGED,loFormSet.GetHeaderText("LANG_POSTREC_DAMAGED",loFormSet.HeaderAlias))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcRetSHd3 = LANG_POSTREC_FIRSTQUALITY
    lcRetSHd3 = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_FIRSTQUALITY,loFormSet.GetHeaderText("LANG_POSTREC_FIRSTQUALITY",loFormSet.HeaderAlias))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcRetSHd4 = LANG_POSTREC_DAMAGED
    lcRetSHd4 = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_DAMAGED,loFormSet.GetHeaderText("LANG_POSTREC_DAMAGED",loFormSet.HeaderAlias))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

  Endif
  lcSndGrd = "1"
  lcTrdGrd = "3"
Case lcMStyQlty='3'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *lcRetSHd = LANG_POSTREC_DAMAGED
  lcRetSHd = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_DAMAGED,loFormSet.GetHeaderText("LANG_POSTREC_DAMAGED",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  If loFormSet.lcPType<>'A'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcRetSHd1 = LANG_POSTREC_1STQUALITY
    lcRetSHd1 = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_1STQUALITY,loFormSet.GetHeaderText("LANG_POSTREC_1STQUALITY",loFormSet.HeaderAlias))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcRetSHd2 = LANG_POSTREC_2NDQUALITY
    lcRetSHd2 = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_2NDQUALITY,loFormSet.GetHeaderText("LANG_POSTREC_2NDQUALITY",loFormSet.HeaderAlias))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcRetSHd3 = LANG_POSTREC_FIRSTQUALITY
    lcRetSHd3 = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_FIRSTQUALITY,loFormSet.GetHeaderText("LANG_POSTREC_FIRSTQUALITY",loFormSet.HeaderAlias))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcRetSHd4 = LANG_POSTREC_SECONDQUALITY
    lcRetSHd4 = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_SECONDQUALITY,loFormSet.GetHeaderText("LANG_POSTREC_SECONDQUALITY",loFormSet.HeaderAlias))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

  Endif
  lcSndGrd = "1"
  lcTrdGrd = "2"
Endcase

lnOldStk = TotStk
lnOldDam = TotDam
lnOldCan = TotCan

*--1) Get Stock quantity.
If Seek('2'+lcLKey)
  Scatter Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 To laSok
  *--Initialize the landed cost.
Endif

*--2) Get Damage quantity.
Store .F. To llRSt1Stat,llRSt2Stat
If Seek('4'+lcLKey)
  Scan Rest While TranCd+cCarton+PO+Style+Dyelot+Padr(cWareCode,6)+Str(Lineno,6) = '4'+lcLKey
    If cStyGrade = lcSndGrd
      Scatter Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 To laDam1
      lcRetSty1  = cRetSty
      llRSt1Stat = .T.
    Else
      Scatter Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 To laDam2
      lcRetSty2  = cRetSty
      llRSt2Stat = .T.
    Endif
  Endscan
Endif

*--3) Get Cancel quantity.
If Seek('5'+lcLKey)
  Scatter Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 To laCan
Endif

For I = 1 To 8
  laBal[I] = Max(laOrg[I]-(laSok[I]+laDam1[I]+laDam2[I]+laCan[I]),0)
Endfor

llSkMode = .T.
llOtMode = .T.

*--Cannot update quantity case of batch (view only).
If loFormSet.lcPType $ 'BTLH' Or (loFormSet.llPOSale And loFormSet.lcPType $ 'OE') Or ;
    (loFormSet.lcPType = 'R' And !Empty(Evaluate(loFormSet.lcPosHdr+'.CPONO')))
  llSkMode   = .F.
  llOtMode   = .F.
  llRSt1Stat = .F.
  llRSt2Stat = .F.
Endif

*--Cannot edit the other and cancel quantity case of issue inter Location P/o.
If loFormSet.lcPType $ 'NAU'
  llOtMode   = .F.
  llRSt1Stat = .F.
  llRSt2Stat = .F.
Endif

=Seek('1'+lcLKey)
Local lnCostVal1,lnCostVal2,lnCostVal3,lnCostVal4,lnCostVal5,lnCostVal6,lnCostVal7,;
  lnEqCost1,lnEqCost2,lnEqCost3,lnEqCost4,lnEqCost5,lnEqCost6,lnEqCost7
Store 0 To lnCostVal1,lnCostVal2,lnCostVal3,lnCostVal4,lnCostVal5,lnCostVal6,lnCostVal7,;
  lnEqCost1,lnEqCost2,lnEqCost3,lnEqCost4,lnEqCost5,lnEqCost6,lnEqCost7
If loFormSet.llEditLCst
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
Endif

*calculate old values of Out quantities from this line.
For lnI = 1 To 8
  laOldOut[lnI] = laSok[lnI]+laDam1[lnI]+laDam2[lnI]+laCan[lnI]
Endfor

*--Call line quantity screen.
Private loParentForm
loParentForm = loFormSet
*T20071102.0018(C200876) TMI [Start] if bin location isntalled run for the line receive another screen
If Ascan(loFormSet.laEvntTrig,Padr('DLRCVQTY',10),1,Alen(loFormSet.laEvntTrig,1),1) > 0 ;
    .And. loFormSet.mDoTrigger(Padr('ISUSEBIN',10))
  loFormSet.mDoTrigger(Padr('DLRCVQTY',10))
Else
  *T20071102.0018(C200876) TMI [End  ]
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  *DO FORM (oAriaApplication.ScreenHome+'MFRCVQ.SCX')
  *! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[Start]
  *DO FORM (oAriaApplication.ScreenHome+'MFRCVQ.SCX') WITH loFormSet.lcPType $ 'IOM' AND !EMPTY(loFormSet.AriaForm1.CntBatch.kbBatchNo.KeyTextBox.Value) AND (loFormSet.AriaForm1.CntBatch.cboBatchStatu
  loCallingForm  = loFormSet
  =gfCallForm('MFRCVQ',.F.,;
    "loCallingForm.lcPType $ 'IOM' AND !EMPTY(loCallingForm.AriaForm1.CntBatch.kbBatchNo.KeyTextBox.Value) AND "+;
    " (loCallingForm.AriaForm1.CntBatch.cboBatchStatus.Value $ 'XP' OR (loCallingForm.AriaForm1.CntBatch.cboBatchStatus.Value ='A' AND !loCallingForm.llApproveBatch))")
  *! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[End]
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
  *T20071102.0018(C200876) TMI [Start]
Endif
*T20071102.0018(C200876) TMI [End  ]


Select (loFormSet.lcTmpLine)
If loFormSet.lcPType $ 'BT'
  =Seek('1'+lcLKey)
  Return
Endif
Scatter Memvar

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
If Seek('2'+lcLKey)
  If lnTStk<>0
    Gather From laSok Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    Replace TotQty With lnTStk
    If loFormSet.llEditLCst
      Replace nLan_Cost1 With lnEqCost1, nLan_Cost2 With lnEqCost2, nLan_Cost3 With lnEqCost3,;
        nLan_Cost4 With lnEqCost4, nLan_Cost5 With lnEqCost5, nLan_Cost6 With lnEqCost6,;
        nLan_Cost7 With lnEqCost7,;
        nFLanCost1 With lnCostVal1, nFLanCost2 With lnCostVal2, nFLanCost3 With lnCostVal3,;
        nFLanCost4 With lnCostVal4, nFLanCost5 With lnCostVal5,;
        nFLanCost6 With lnCostVal6, nFLanCost7 With lnCostVal7
    Endif
  Else
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
    If Iif(loFormSet.lcPType $ 'IMO',Empty(loFormSet.ariaform1.cntBatch.kbBatchNo.keytextbox.Value),.T.)
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
      Blank
      *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
    Endif
    *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
    Delete
  Endif
Else
  If lnTStk<>0
    Append Blank
    Gather Memvar
    Replace TranCd    With '2',;
      cStyGrade With lcMStyQlty


    *B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[Start]
    Replace cInvType With loFormSet.lcInvType
    *B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[End]


    Gather From laSok  Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    Gather From laZero Fields Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
    Replace TotQty With lnTStk
    If loFormSet.llEditLCst
      Replace nLan_Cost1 With lnEqCost1, nLan_Cost2 With lnEqCost2, nLan_Cost3 With lnEqCost3,;
        nLan_Cost4 With lnEqCost4, nLan_Cost5 With lnEqCost5, nLan_Cost6 With lnEqCost6,;
        nLan_Cost7 With lnEqCost7,;
        nFLanCost1 With lnCostVal1, nFLanCost2 With lnCostVal2, nFLanCost3 With lnCostVal3,;
        nFLanCost4 With lnCostVal4, nFLanCost5 With lnCostVal5,;
        nFLanCost6 With lnCostVal6, nFLanCost7 With lnCostVal7
    Endif
  Endif
Endif

*--2) Update Damaged quantity.
=Seek('4'+lcLKey)
Locate Rest While TranCd+cCarton+PO+Style+Dyelot+Padr(cWareCode,6)+Str(Lineno,6)='4'+lcLKey ;
  FOR cStyGrade=lcSndGrd
If Found()
  If lnTDam1=0
    Blank
    Delete
  Else
    Gather From laDam1 Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    Replace TotQty  With lnTDam1,;
      cRetSty With lcRetSty1
    If loFormSet.llEditLCst
      Replace nLan_Cost1 With lnEqCost1, nLan_Cost2 With lnEqCost2, nLan_Cost3 With lnEqCost3,;
        nLan_Cost4 With lnEqCost4, nLan_Cost5 With lnEqCost5, nLan_Cost6 With lnEqCost6,;
        nLan_Cost7 With lnEqCost7,;
        nFLanCost1 With lnCostVal1, nFLanCost2 With lnCostVal2, nFLanCost3 With lnCostVal3,;
        nFLanCost4 With lnCostVal4, nFLanCost5 With lnCostVal5,;
        nFLanCost6 With lnCostVal6, nFLanCost7 With lnCostVal7
    Endif
  Endif
Else
  If lnTDam1<>0
    Append Blank
    Gather Memvar
    Replace TranCd    With '4',;
      cStyGrade With lcSndGrd,;
      cRetSty   With lcRetSty1,;
      TotQty    With lnTDam1
    Gather From laDam1 Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    Gather From laZero Fields Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
    If loFormSet.llEditLCst
      Replace nLan_Cost1 With lnEqCost1, nLan_Cost2 With lnEqCost2, nLan_Cost3 With lnEqCost3,;
        nLan_Cost4 With lnEqCost4, nLan_Cost5 With lnEqCost5, nLan_Cost6 With lnEqCost6,;
        nLan_Cost7 With lnEqCost7,;
        nFLanCost1 With lnCostVal1, nFLanCost2 With lnCostVal2, nFLanCost3 With lnCostVal3,;
        nFLanCost4 With lnCostVal4, nFLanCost5 With lnCostVal5,;
        nFLanCost6 With lnCostVal6, nFLanCost7 With lnCostVal7
    Endif
  Endif
Endif

=Seek('4'+lcLKey)
Locate Rest While TranCd+cCarton+PO+Style+Dyelot+Padr(cWareCode,6)+Str(Lineno,6)='4'+lcLKey ;
  FOR cStyGrade=lcTrdGrd

If Found()
  If lnTDam2=0
    Blank
    Delete
  Else
    Gather From laDam2 Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    Replace TotQty  With lnTDam2,;
      cRetSty With lcRetSty2
    If loFormSet.llEditLCst
      Replace nLan_Cost1 With lnEqCost1, nLan_Cost2 With lnEqCost2, nLan_Cost3 With lnEqCost3,;
        nLan_Cost4 With lnEqCost4, nLan_Cost5 With lnEqCost5, nLan_Cost6 With lnEqCost6,;
        nLan_Cost7 With lnEqCost7,;
        nFLanCost1 With lnCostVal1, nFLanCost2 With lnCostVal2, nFLanCost3 With lnCostVal3,;
        nFLanCost4 With lnCostVal4, nFLanCost5 With lnCostVal5,;
        nFLanCost6 With lnCostVal6, nFLanCost7 With lnCostVal7
    Endif
  Endif
Else
  If lnTDam2<>0
    Append Blank
    Gather Memvar
    Replace TranCd    With '4',;
      cStyGrade With lcTrdGrd,;
      cRetSty   With lcRetSty2,;
      TotQty    With lnTDam2
    Gather From laDam2 Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    Gather From laZero Fields Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
    If loFormSet.llEditLCst
      Replace nLan_Cost1 With lnEqCost1, nLan_Cost2 With lnEqCost2, nLan_Cost3 With lnEqCost3,;
        nLan_Cost4 With lnEqCost4, nLan_Cost5 With lnEqCost5, nLan_Cost6 With lnEqCost6,;
        nLan_Cost7 With lnEqCost7,;
        nFLanCost1 With lnCostVal1, nFLanCost2 With lnCostVal2, nFLanCost3 With lnCostVal3,;
        nFLanCost4 With lnCostVal4, nFLanCost5 With lnCostVal5,;
        nFLanCost6 With lnCostVal6, nFLanCost7 With lnCostVal7
    Endif
  Endif
Endif

*--3) Update cancel quantity.
If Seek('5'+lcLKey)
  If lnTCan<>0
    Gather From laCan Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    Replace TotQty With lnTCan
    If loFormSet.llEditLCst
      Replace nLan_Cost1 With lnEqCost1, nLan_Cost2 With lnEqCost2, nLan_Cost3 With lnEqCost3,;
        nLan_Cost4 With lnEqCost4, nLan_Cost5 With lnEqCost5, nLan_Cost6 With lnEqCost6,;
        nLan_Cost7 With lnEqCost7,;
        nFLanCost1 With lnCostVal1, nFLanCost2 With lnCostVal2, nFLanCost3 With lnCostVal3,;
        nFLanCost4 With lnCostVal4, nFLanCost5 With lnCostVal5,;
        nFLanCost6 With lnCostVal6, nFLanCost7 With lnCostVal7
    Endif
  Else
    Blank
    Delete
  Endif
Else
  If lnTCan<>0
    Append Blank
    Gather Memvar
    Replace TranCd With '5',;
      TotQty With lnTCan
    Gather From laCan  Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8
    Gather From laZero Fields Ord1,Ord2,Ord3,Ord4,Ord5,Ord6,Ord7,Ord8,TotOrd
    If loFormSet.llEditLCst
      Replace nLan_Cost1 With lnEqCost1, nLan_Cost2 With lnEqCost2, nLan_Cost3 With lnEqCost3,;
        nLan_Cost4 With lnEqCost4, nLan_Cost5 With lnEqCost5, nLan_Cost6 With lnEqCost6,;
        nLan_Cost7 With lnEqCost7,;
        nFLanCost1 With lnCostVal1, nFLanCost2 With lnCostVal2, nFLanCost3 With lnCostVal3,;
        nFLanCost4 With lnCostVal4, nFLanCost5 With lnCostVal5,;
        nFLanCost6 With lnCostVal6, nFLanCost7 With lnCostVal7
    Endif
  Endif
Endif

Select (loFormSet.lcTmpLine)
If Seek('1'+lcLKey)
  Replace TotStk With lnTStk,;
    TotDam With lnTDam,;
    TotCan With lnTCan,;
    TotBal With laBal[1]+laBal[2]+laBal[3]+laBal[4]+laBal[5]+laBal[6]+laBal[7]+laBal[8]
Endif

loFormSet.lnTotStk = loFormSet.lnTotStk+(TotStk-lnOldStk)
loFormSet.lnTotDam = loFormSet.lnTotDam+(TotDam-lnOldDam)
loFormSet.lnTotCan = loFormSet.lnTotCan+(TotCan-lnOldCan)

If (loFormSet.lcPType $ 'ISMD')
  Dimension laOut[8]
  Store 0 To lnTotOut,laOut
  For lnI = 1 To 8
    Z = Str(lnI,1)
    laOut[lnI] = laSok[lnI]+laDam1[lnI]+laDam2[lnI]+laCan[lnI]
    lnTotOut   = lnTotOut + laOut[lnI]
  Endfor
  *-- Calculate Out Quantity in this Line.
  lcCurLine =  cCarton+PO+Style+Str(Lineno,6)
  *-- Subtract out quantity from the same lines with another dyelots.
  lcScanVar = [Trancd+cCarton+Po+Style+STR(LineNo,6)]
  Scan For Evaluate(lcScanVar) = '1' + lcCurLine And ;
      Dyelot+cWareCode # lcDyelot+lcwarecode
    For lnI = 1 To 8
      lcZ = Str(lnI,1)
      Replace ('QTY'+lcZ) With Evaluate('QTY'+lcZ) + laOldOut[lnI] - laOut[lnI] ,;
        TotQty      With TotQty  + laOldOut[lnI] - laOut[lnI]
    Endfor
    Replace TotBal With TotQty-TotStk-TotDam-TotCan
  Endscan
  =Seek('1'+lcLKey)
Endif

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
If Ascan(loFormSet.laEvntTrig,'UPBNLOC') <> 0
  *MMT
  lcTmpLn = loFormSet.lcTmpLine
  =gfDoTriger('POSTREC','UPBNLOC')
Endif
*T20071102.0018(C200876) TMI [End  ]

Select (loFormSet.lcTmpLine)
Set Filter To TranCd = '1'
loFormSet.Refresh
Return

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
Function lfMfRcvQInit
Lparameters loFormSet

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.ariaForm1.CAPTION = LANG_POSTREC_MFRCVQ
loFormSet.ariaform1.Caption = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MFRCVQ,loFormSet.GetHeaderText("LANG_POSTREC_MFRCVQ",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]


=gfOpenFile(oAriaApplication.DataDir+'STYLE','STYLE','SH')
Local lcLineQty,lnFldWidth,lnFldPer
lcLineQty  = gfTempName()
loFormSet.lcLineQty = lcLineQty
lnFldWidth = Iif(loParentForm.lcInvType='0001',6,11)
lnFldPer   = Iif(loParentForm.lcInvType='0001',0,3)

Local lnSetDec
lnSetDec = Set("Decimals")
Set Decimals To lnFldPer
If lnFldPer = 0
  loFormSet.lnFldHigh = Val(Replicate('9',lnFldWidth))
Else
  loFormSet.lnFldHigh = Val(Stuff(Replicate('9',lnFldWidth),lnFldWidth-lnFldPer,1,'.'))
Endif
Set Decimals To lnSetDec

Dimension laFileStru[12,18]
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

For lnI = 7 To 16
  For lnJ = 1 To 12
    laFileStru[lnJ,lnI] = ''
  Endfor
Endfor
For lnJ = 1 To 12
  Store 0 To laFileStru[lnJ,17],laFileStru[lnJ,18]
Endfor

=gfCrtTmp(lcLineQty,@laFileStru,'cType',lcLineQty)

Select (lcLineQty)
Append Blank
Replace cType   With '1',;
  cDesc   With Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BUDGET,loFormSet.GetHeaderText("LANG_POSTREC_BUDGET",loFormSet.HeaderAlias)),;
  nQty1   With Max(laConst[1],0),;
  nQty2   With Max(laConst[2],0),;
  nQty3   With Max(laConst[3],0),;
  nQty4   With Max(laConst[4],0),;
  nQty5   With Max(laConst[5],0),;
  nQty6   With Max(laConst[6],0),;
  nQty7   With Max(laConst[7],0),;
  nQty8   With Max(laConst[8],0),;
  nTotQty With nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8,;
  nUseQty With nTotQty * lnConf

Append Blank
Replace cType   With '2',;
  cDesc   With Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_PRVRCPT,loFormSet.GetHeaderText("LANG_POSTREC_PRVRCPT",loFormSet.HeaderAlias)),;
  nQty1   With Max(laRemain[1],0),;
  nQty2   With Max(laRemain[2],0),;
  nQty3   With Max(laRemain[3],0),;
  nQty4   With Max(laRemain[4],0),;
  nQty5   With Max(laRemain[5],0),;
  nQty6   With Max(laRemain[6],0),;
  nQty7   With Max(laRemain[7],0),;
  nQty8   With Max(laRemain[8],0),;
  nTotQty With Max(laRemain[9],0),;
  nUseQty With nTotQty * lnConf

Append Blank
Replace cType   With '3',;
  cDesc   With Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_OpenBrowTitl,loFormSet.GetHeaderText("LANG_POSTREC_OpenBrowTitl",loFormSet.HeaderAlias)),;
  nQty1   With Max(laOrg[1],0),;
  nQty2   With Max(laOrg[2],0),;
  nQty3   With Max(laOrg[3],0),;
  nQty4   With Max(laOrg[4],0),;
  nQty5   With Max(laOrg[5],0),;
  nQty6   With Max(laOrg[6],0),;
  nQty7   With Max(laOrg[7],0),;
  nQty8   With Max(laOrg[8],0),;
  nTotQty With nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8,;
  nUseQty With nTotQty * lnConf

Append Blank
Replace cType   With '4',;
  cDesc   With lcRetSHd,;
  nQty1   With laSok[1],;
  nQty2   With laSok[2],;
  nQty3   With laSok[3],;
  nQty4   With laSok[4],;
  nQty5   With laSok[5],;
  nQty6   With laSok[6],;
  nQty7   With laSok[7],;
  nQty8   With laSok[8],;
  nTotQty With nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8,;
  nUseQty With nTotQty * lnConf

Append Blank
Replace cType   With '5',;
  cDesc   With lcRetSHd1,;
  nQty1   With laDam1[1],;
  nQty2   With laDam1[2],;
  nQty3   With laDam1[3],;
  nQty4   With laDam1[4],;
  nQty5   With laDam1[5],;
  nQty6   With laDam1[6],;
  nQty7   With laDam1[7],;
  nQty8   With laDam1[8],;
  nTotQty With nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8,;
  nUseQty With nTotQty * lnConf

Append Blank
Replace cType   With '6',;
  cDesc   With lcRetSHd2,;
  nQty1   With laDam2[1],;
  nQty2   With laDam2[2],;
  nQty3   With laDam2[3],;
  nQty4   With laDam2[4],;
  nQty5   With laDam2[5],;
  nQty6   With laDam2[6],;
  nQty7   With laDam2[7],;
  nQty8   With laDam2[8],;
  nTotQty With nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8,;
  nUseQty With nTotQty * lnConf

Append Blank
Replace cType   With '7',;
  cDesc   With Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_CANCEL,loFormSet.GetHeaderText("LANG_POSTREC_CANCEL",loFormSet.HeaderAlias)),;
  nQty1   With laCan[1],;
  nQty2   With laCan[2],;
  nQty3   With laCan[3],;
  nQty4   With laCan[4],;
  nQty5   With laCan[5],;
  nQty6   With laCan[6],;
  nQty7   With laCan[7],;
  nQty8   With laCan[8],;
  nTotQty With nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8,;
  nUseQty With nTotQty * lnConf

Append Blank
Replace cType   With '8',;
  cDesc   With Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BALANCE,loFormSet.GetHeaderText("LANG_POSTREC_BALANCE",loFormSet.HeaderAlias)),;
  nQty1   With Max(laBal[1],0),;
  nQty2   With Max(laBal[2],0),;
  nQty3   With Max(laBal[3],0),;
  nQty4   With Max(laBal[4],0),;
  nQty5   With Max(laBal[5],0),;
  nQty6   With Max(laBal[6],0),;
  nQty7   With Max(laBal[7],0),;
  nQty8   With Max(laBal[8],0),;
  nTotQty With nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8,;
  nUseQty With nTotQty * lnConf
Locate

With loFormSet.ariaform1
  With .grdLineQty
    .RecordSource = lcLineQty
    .Column1.ControlSource   = lcLineQty+".cDesc"

    .Column2.ControlSource   = lcLineQty+".nQty1"
    .Column2.Header1.Caption = Scale.SZ1
    .Column2.Width           = lnFldWidth * 10
    .Column2.Visible         = (Scale.Cnt>0)

    .Column3.ControlSource   = lcLineQty+".nQty2"
    .Column3.Header1.Caption = Scale.SZ2
    .Column3.Width           = lnFldWidth * 10
    .Column3.Visible         = (Scale.Cnt>1)

    .Column4.ControlSource   = lcLineQty+".nQty3"
    .Column4.Header1.Caption = Scale.SZ3
    .Column4.Width           = lnFldWidth * 10
    .Column4.Visible         = (Scale.Cnt>2)

    .Column5.ControlSource   = lcLineQty+".nQty4"
    .Column5.Header1.Caption = Scale.SZ4
    .Column5.Width           = lnFldWidth * 10
    .Column5.Visible         = (Scale.Cnt>3)

    .Column6.ControlSource   = lcLineQty+".nQty5"
    .Column6.Header1.Caption = Scale.SZ5
    .Column6.Width           = lnFldWidth * 10
    .Column6.Visible         = (Scale.Cnt>4)

    .Column7.ControlSource   = lcLineQty+".nQty6"
    .Column7.Header1.Caption = Scale.SZ6
    .Column7.Width           = lnFldWidth * 10
    .Column7.Visible         = (Scale.Cnt>5)

    .Column8.ControlSource   = lcLineQty+".nQty7"
    .Column8.Header1.Caption = Scale.SZ7
    .Column8.Width           = lnFldWidth * 10
    .Column8.Visible         = (Scale.Cnt>6)

    .Column9.ControlSource   = lcLineQty+".nQty8"
    .Column9.Header1.Caption = Scale.SZ8
    .Column9.Width           = lnFldWidth * 10
    .Column9.Visible         = (Scale.Cnt>7)

    .Column10.ControlSource  = lcLineQty+".nTotQty"
    .Column10.Width          = (lnFldWidth + 1) * 10

    .Column11.ControlSource  = lcLineQty+".nUseQty"
    .Column11.Header1.Caption= .Column11.Header1.Caption+lcUOM_V
    .Column11.Width          = 120
    .Column11.Visible        = (loParentForm.lcInvType='0002')

    *--Set dyenamic colors.
    .SetAll("Dynamicbackcolor", "", "Column")
    *.SetAll("Dynamicbackcolor","IIF("+lcLineQty+".cType='1',12320767,IIF("+lcLineQty+".cType='2',16769996,"+;
    "IIF("+lcLineQty+".cType='3',16763806,16777215)))", "Column")
    .SetAll("Dynamicbackcolor","IIF("+lcLineQty+".cType='1',12320767,IIF("+lcLineQty+".cType='2',16769996,"+;
      "IIF("+lcLineQty+".cType='3',16763806,IIF("+lcLineQty+".cType='8',16763806,16777215))))", "Column")
  Endwith
  .lblItemQuality1.Caption = lcRetSHd3
  .lblItemQuality2.Caption = lcRetSHd4
  .kbItemQuality2.Enabled  = llRSt1Stat
  .kbItemQuality3.Enabled  = llRSt2Stat
  .kbItemQuality2.cQuality = lcSndGrd
  .kbItemQuality3.cQuality = lcTrdGrd
  .kbItemQuality2.Value    = lcRetSty1
  .kbItemQuality3.Value    = lcRetSty2
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
  If !Empty(lcRetSty1) And !Used('SCALE1')
    m.lcRetS = lcRetSty1
    If !Seek(Iif(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty1,loParentForm.lcTmpItem)
      If loParentForm.lcInvType = "0001"
        *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
        *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = '" + lcRetSty1+ "'"
        *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
        *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = [" + lcRetSty1+ "]"
        lcRetStyVal = lcRetSty1
        lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = ?m.lcRetStyVal "
        *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
        *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
      Else
        lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loParentForm.lcInvType +;
          "' AND Style = ?m.lcRetS "
      Endif
      =lfGetItmInf(loParentForm.lcInvType,;
        IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty1,;
        loParentForm.lcTmpItem,;
        IIF(loParentForm.lcInvType = "0001",'STYLE','ITEM'),lcSqlStatement,;
        loParentForm.lcInvType = "0002")
      =Seek(Iif(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty1,loParentForm.lcTmpItem)
    Endif
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
    *lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE='S"+EVALUATE(loParentForm.lcTmpItem+'.SCALE')+"'"
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
    *lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=[S"+EVALUATE(loParentForm.lcTmpItem+'.SCALE')+"]"
    lcScaleV = "S"+Evaluate(loParentForm.lcTmpItem+'.SCALE')
    lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=?m.lcScaleV "
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
    =lfOpenFox(lcSqlStatement,'SCALE','SCALE1',"")
  Endif
  If !Empty(lcRetSty2) And !Used('SCALE2')
    m.lcRetS = lcRetSty2
    If !Seek(Iif(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty2,loParentForm.lcTmpItem)
      If loParentForm.lcInvType = "0001"
        *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
        *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = '" + lcRetSty2+ "'"
        *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
        *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = [" + lcRetSty2+ "]"
        lcStyRetValue =lcRetSty2
        lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = ?m.lcStyRetValue"
        *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
        *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
      Else
        lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loParentForm.lcInvType +;
          "' AND Style = ?m.lcRetS "
      Endif
      =lfGetItmInf(loParentForm.lcInvType,;
        IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty2,;
        loParentForm.lcTmpItem,;
        IIF(loParentForm.lcInvType = "0001",'STYLE','ITEM'),lcSqlStatement,;
        loParentForm.lcInvType = "0002")
      =Seek(Iif(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty2,loParentForm.lcTmpItem)
    Endif
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
    *lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE='S"+EVALUATE(loParentForm.lcTmpItem+'.SCALE')+"'"
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
    *lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=[S"+EVALUATE(loParentForm.lcTmpItem+'.SCALE')+"]"
    lcSclValueSel = "S"+Evaluate(loParentForm.lcTmpItem+'.SCALE')
    lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=?m.lcSclValueSel "
    *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
    *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
    =lfOpenFox(lcSqlStatement,'SCALE','SCALE2',"")
  Endif
  *! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
Endwith
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
Function lfvQty
Lparameters lnCnxt,loFormSet


Local lcCnxt
lcCnxt = Str(lnCnxt,1)

Select (loFormSet.lcLineQty)
Replace nTotQty With nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8,;
  nUseQty With nTotQty * lnConf
lcType = cType
Do Case
Case cType = '4'
  laSok[lnCnxt]  = Evaluate('nQty'+lcCnxt)
Case cType = '5'
  laDam1[lnCnxt] = Evaluate('nQty'+lcCnxt)
Case cType = '6'
  laDam2[lnCnxt] = Evaluate('nQty'+lcCnxt)
Case cType = '7'
  laCan[lnCnxt]  = Evaluate('nQty'+lcCnxt)
Endcase

If loParentForm.lcPType $ 'O' And (laSok[lnCnxt]+laDam1[lnCnxt]+laDam2[lnCnxt]+laCan[lnCnxt]) > laOrg[lnCnxt]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *=gfModalGen('INM00000B00000','','','',LANG_POSTREC_MESSAG1)
  =gfModalGen('INM00000B00000','','','',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_MESSAG1,loFormSet.GetHeaderText("LANG_POSTREC_MESSAG1",loFormSet.HeaderAlias)))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  Replace ('nQty'+lcCnxt) With Iif(cType='4',Evaluate(loParentForm.lcTmpLine+'.Qty'+lcCnxt),0)
  Replace nTotQty With nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8
  Do Case
  Case cType = '4'
    laSok[lnCnxt]  = Evaluate('nQty'+lcCnxt)
  Case cType = '5'
    laDam1[lnCnxt] = Evaluate('nQty'+lcCnxt)
  Case cType = '6'
    laDam2[lnCnxt] = Evaluate('nQty'+lcCnxt)
  Case cType = '7'
    laCan[lnCnxt]  = Evaluate('nQty'+lcCnxt)
  Endcase
  Return 0
Else
  If loParentForm.lcPType $ 'AE' And laSok[lnCnxt] > laOrg[lnCnxt]
    *-You cannot increase quantity more that original picked quantity!
    =gfModalGen('INM38189B00000','DIALOG')
    Replace ('nQty'+lcCnxt) With Evaluate(loParentForm.lcTmpLine+'.Qty'+lcCnxt)
    Replace nTotQty With nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8
    laSok[lnCnxt] = Evaluate('nQty'+lcCnxt)
    Return 0
  Endif
Endif

lnNewBal = laOrg[lnCnxt]-(laSok[lnCnxt]+laDam1[lnCnxt]+laDam2[lnCnxt]+laCan[lnCnxt])
If cType<>'7' And laCan[lnCnxt] > 0 And lnNewBal<0
  *--Cancel quantity more that new balance, Therefore it will be redused.
  =gfModalGen('INM34068B42000','DIALOG')
  laBal[lnCnxt]=0
  laCan[lnCnxt]=laOrg[lnCnxt]-(laSok[lnCnxt]+laDam1[lnCnxt]+laDam2[lnCnxt])
  laCan[lnCnxt]=Iif(laCan[lnCnxt]<0,0,laCan[lnCnxt])
  =Seek('7')
  Replace ('nQty'+lcCnxt) With laCan[lnCnxt]
  Replace nTotQty With nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8,;
    nUseQty With nTotQty * lnConf
Else
  laBal[lnCnxt]=laOrg[lnCnxt]-(laSok[lnCnxt]+laDam1[lnCnxt]+laDam2[lnCnxt]+laCan[lnCnxt])
  laBal[lnCnxt]=Iif(laBal[lnCnxt]<0,0,laBal[lnCnxt])
Endif
=Seek('8')
Replace ('nQty'+lcCnxt) With laBal[lnCnxt]
Replace nTotQty With nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8,;
  nUseQty With nTotQty * lnConf
=Seek(lcType)

If laDam1[1]+laDam1[2]+laDam1[3]+laDam1[4]+laDam1[5]+laDam1[6]+laDam1[7]+laDam1[8]<>0
  If Empty(lcRetSty1) And Seek(Iif(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+Evaluate(loParentForm.lcTmpLine+'.Style'),loParentForm.lcTmpItem)
    lcRetSty1 = Evaluate(loParentForm.lcTmpItem+'.cRetSty')
    If !Empty(lcRetSty1)
      *-- Get the style information
      If !Seek(Iif(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty1,loParentForm.lcTmpItem)
        If loParentForm.lcInvType = "0001"
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
          *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = '" + lcRetSty1 + "'"
          *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
          *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = [" + lcRetSty1 + "]"
          lcSelStyRetV = lcRetSty1
          lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = ?m.lcSelStyRetV"
          *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
        Else
          *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
          *lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loParentForm.lcInvType +;
          "' AND Style = '" + lcRetSty1 + "'"
          lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loParentForm.lcInvType +;
            "' AND Style = ?m.lcRetSty1 "
          *B607658,1 KHM 07/07/2005
        Endif

        =lfGetItmInf(loParentForm.lcInvType,;
          IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty1,;
          loParentForm.lcTmpItem,;
          IIF(loParentForm.lcInvType = "0001",'STYLE','ITEM'),lcSqlStatement,;
          loParentForm.lcInvType = "0002")
        =Seek(Iif(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty1,loParentForm.lcTmpItem)
      Endif
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
      *lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE='S"+EVALUATE(loParentForm.lcTmpItem+'.SCALE')+"'"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
      *lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=[S"+EVALUATE(loParentForm.lcTmpItem+'.SCALE')+"]"
      lcSclSelectV = "S"+Evaluate(loParentForm.lcTmpItem+'.SCALE')
      lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=?m.lcSclSelectV"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
      =lfOpenFox(lcSqlStatement,'SCALE','SCALE1',"")
      If SCALE1.Cnt < lnCnxt
        lcRetSty1=Space(19)
      Endif
    Endif
    Select (loFormSet.lcLineQty)
  Endif
  loFormSet.ariaform1.kbItemQuality2.Enabled = .T.
Else
  lcRetSty1=Space(19)
  loFormSet.ariaform1.kbItemQuality2.Enabled = .F.
Endif
loFormSet.ariaform1.kbItemQuality2.Value = lcRetSty1

If laDam2[1]+laDam2[2]+laDam2[3]+laDam2[4]+laDam2[5]+laDam2[6]+laDam2[7]+laDam2[8]<>0
  If Empty(lcRetSty2) And Seek(Iif(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+Evaluate(loParentForm.lcTmpLine+'.Style'),loParentForm.lcTmpItem)
    lcRetSty2 = Evaluate(loParentForm.lcTmpItem+'.cRetSty2')
    If !Empty(lcRetSty2)
      *-- Get the style information
      If !Seek(Iif(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty2,loParentForm.lcTmpItem)
        If loParentForm.lcInvType = "0001"
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
          *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = '" + lcRetSty2 + "'"
          *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
          *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = [" + lcRetSty2 + "]"
          lcSelRetStyV = lcRetSty2
          lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = ?m.lcSelRetStyV"
          *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
        Else
          *B607658,1 KHM 07/07/2005 Use m. to cover the case of having special char [Begin]
          *lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loParentForm.lcInvType +;
          "' AND Style = '" + lcRetSty2 + "'"
          lcSqlStatement  = "SELECT * FROM ITEM WHERE cInvType ='" + loParentForm.lcInvType +;
            "' AND Style = ?m.lcRetSty2 "
          *B607658,1 KHM 07/07/2005
        Endif

        =lfGetItmInf(loParentForm.lcInvType,;
          IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty2,;
          loParentForm.lcTmpItem,;
          IIF(loParentForm.lcInvType = "0001",'STYLE','ITEM'),lcSqlStatement,;
          loParentForm.lcInvType = "0002")
        =Seek(Iif(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty2,loParentForm.lcTmpItem)
      Endif
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
      *lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE='S"+EVALUATE(loParentForm.lcTmpItem+'.SCALE')+"'"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
      *lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=[S"+EVALUATE(loParentForm.lcTmpItem+'.SCALE')+"]"
      lcSelScalV = "S"+Evaluate(loParentForm.lcTmpItem+'.SCALE')
      lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=?m.lcSelScalV"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
      =lfOpenFox(lcSqlStatement,'SCALE','SCALE2',"")
      If SCALE2.Cnt < lnCnxt
        lcRetSty2=Space(19)
      Endif
    Endif
    Select (loFormSet.lcLineQty)
  Endif
  loFormSet.ariaform1.kbItemQuality3.Enabled = .T.
Else
  lcRetSty2=Space(19)
  loFormSet.ariaform1.kbItemQuality3.Enabled = .F.
Endif
loFormSet.ariaform1.kbItemQuality3.Value = lcRetSty2

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

loFormSet.Refresh
Return
*--end of lfvQty.

*:*************************************************************
*! Name     : lfvRetSty
*! Developer: Ahmed Maher (AMH)
*! Date     : 10/11/2004
*! Purpose  : Return style valid.
*:*************************************************************
Function lfvRetSty
Lparameters lcPQualty,lcBrStyle

Local lcOldValue
lcOldValue = lcRetSty&lcPQualty
lcRetSty&lcPQualty = lcBrStyle

*E039550,1 KHM
m.lcRetS = lcBrStyle
*E039550,1 KHM

If !Empty(lcBrStyle)
  *-- Get the style information
  If !Seek(Iif(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcBrStyle,loParentForm.lcTmpItem)
    If loParentForm.lcInvType = "0001"
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
      *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = '" + lcBrStyle + "'"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
      *lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = [" + lcBrStyle + "]"
      lcBrSelValuSty = lcBrStyle
      lcSqlStatement  = "SELECT * FROM STYLE WHERE Style = ?m.lcBrSelValuSty "
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
    Else
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
    Endif

    =lfGetItmInf(loParentForm.lcInvType,;
      IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcBrStyle,;
      loParentForm.lcTmpItem,;
      IIF(loParentForm.lcInvType = "0001",'STYLE','ITEM'),lcSqlStatement,;
      loParentForm.lcInvType = "0002")
    =Seek(Iif(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcBrStyle,loParentForm.lcTmpItem)
  Endif
  *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
  *lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE='S"+EVALUATE(loParentForm.lcTmpItem+'.SCALE')+"'"
  *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
  *lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=[S"+EVALUATE(loParentForm.lcTmpItem+'.SCALE')+"]"
  lcScalSelV = "S"+Evaluate(loParentForm.lcTmpItem+'.SCALE')
  lcSqlStatement  = "SELECT * FROM SCALE WHERE TYPE+SCALE=?m.lcScalSelV"
  *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
  *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
  =lfOpenFox(lcSqlStatement,'SCALE','SCALE'+lcPQualty,"")
Endif

If !Empty(lcBrStyle) And Evaluate('SCALE'+lcPQualty+'.SCALE')<>Scale.Scale
  Local llDefScale,lnI,lcI,lnDamCnt
  llDefScale = .F.
  lnDamCnt = 1
  For lnI = 1 To Scale.Cnt
    lcI = Str(lnI,1)
    lnDamCnt = Max(lnDamCnt,Iif(Evaluate('laDam'+lcPQualty+'['+lcI+']')>0,lnI,0))
  Endfor
  llDefScale = (Evaluate('SCALE'+lcPQualty+'.Cnt')<lnDamCnt)
  *--The selected style has a different size scale.
  If llDefScale
    lcBrStyle = ''
    lcRetSty&lcPQualty = ''
    =gfModalGen('TRM42089B42000','DIALOG',"","","Selected style has a different size scale. Can not proceed")
    Return .F.
  Else
    =gfModalGen('INM42089B42000','DIALOG')
  Endif
Endif

If !Empty(lcBrStyle) And loParentForm.llWareHous
  *-- Get the stydye information
  If !Seek(Iif(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+;
      PADR(lcBrStyle,19)+lcwarecode+Space(10),loParentForm.lcItemLoc)
    If loParentForm.lcInvType = "0001"
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
      *!*	      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
      *!*	        PADR(LCBrStyle,19)+lcWareCode+SPACE(10) + "'"
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
      *!*	      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
      *!*	        PADR(LCBrStyle,19)+lcWareCode+SPACE(10) + "]"
      lcStyDySelV =Padr(lcBrStyle,19)+lcwarecode+Space(10)
      lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = ?m.lcStyDySelV "
      *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
      *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
    Else
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
        "Dyelot = '" + Padr(lcDyelot,10) + "'"
      *E039550,1 KHM

      *B607658,1 KHM 07/07/2005
    Endif

    =lfGetItmInf(loParentForm.lcInvType,;
      IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+Padr(lcBrStyle,19)+lcwarecode+Space(10),;
      loParentForm.lcItemLoc,;
      IIF(loParentForm.lcInvType = "0001",'STYDYE','ITEMLOC'),lcSqlStatement,;
      loParentForm.lcInvType = "0002")
    If !Seek(Iif(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+;
        PADR(lcBrStyle,19)+lcwarecode+Space(10),loParentForm.lcItemLoc)

      If loParentForm.lcInvType="0001"
        *-Style: xxx is not assigned to Location: xxx. "\<Add;\<Reenter"
        If gfModalGen('QRM34048B42006','DIALOG',Alltrim(lcBrStyle)+'|'+lcwarecode) = 1
          Do gpAdStyWar With lcBrStyle,Space(10),lcwarecode
        Else
          Store lcOldValue To lcRetSty&lcPQualty
          Return .F.
        Endif
      Else
        lcMsg = "Fabric: " + Alltrim(lcBrStyle)
        If gfModalGen('QRM36226B34004','DIALOG',lcMsg +'|'+lcwarecode) = 1
          =gfAdItemWar(loParentForm.lcInvType,lcBrStyle,Space(10),lcwarecode)
        Else
          Store lcOldValue To lcRetSty&lcPQualty
          Return .F.
        Endif
      Endif
    Endif
  Endif
Endif

Return

*:*************************************************************
*! Name      : lfvQtyOk
*! Developer : Ahmed Maher (AMH)
*! Date      : 10/12/2004
*! Purpose   : Quantity Ok pb. valid.
*:*************************************************************
Function lfvQtyOk
Lparameters loFormSet

*! C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
If Ascan(loParentForm.laEvntTrig, Padr('CALCSSHSCR', 10), 1, Alen(loParentForm.laEvntTrig, 1), 1)>0
  Do lfprrecdata In DIRMAIN.FXP With loFormSet
Endif
*! C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]

Local lcMsg
lcMsg = Iif(loParentForm.lcInvType = "0001", ' Style',' Fabric')
If loParentForm.ActiveMode#'V'
  Local lnAlias,lcStyle
  lnAlias = Select(0)
  lcStyle = Evaluate(loParentForm.lcTmpLine+'.Style')

  *-You cannot leave the XXXX style empty since the XXXX quantity was entered.
  If laDam1[1]+laDam1[2]+laDam1[3]+laDam1[4]+laDam1[5]+laDam1[6]+laDam1[7]+laDam1[8]<>0 And ;
      EMPTY(lcRetSty1)
    =gfModalGen('TRM34201B42000','DIALOG',lcRetSHd3+lcMsg+'|'+lcRetSHd3)
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
    *loFormSet.AriaForm1.kbItemQuality2.SetFocus
    *N000682,1 MMT 04/19/2013 Fix Media issues[Start]
    *IF !loFormSet.llSilentMod
    If !loParentForm.llSilentMod
      *N000682,1 MMT 04/19/2013 Fix Media issues[End]
      loFormSet.ariaform1.kbItemQuality2.SetFocus
    Endif
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
    Select (lnAlias)
    Return .F.
  Endif
  If laDam2[1]+laDam2[2]+laDam2[3]+laDam2[4]+laDam2[5]+laDam2[6]+laDam2[7]+laDam2[8]<>0 And ;
      EMPTY(lcRetSty2)
    =gfModalGen('TRM34201B42000','DIALOG',lcRetSHd4+lcMsg+'|'+lcRetSHd4)
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
    *loFormSet.AriaForm1.kbItemQuality3.SetFocus
    *N000682,1 MMT 04/19/2013 Fix Media issues[Start]
    *IF !loFormSet.llSilentMod
    If !loParentForm.llSilentMod
      *N000682,1 MMT 04/19/2013 Fix Media issues[End]
      loFormSet.ariaform1.kbItemQuality3.SetFocus
    Endif
    *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
    Select (lnAlias)
    Return .F.
  Endif

  If loParentForm.llDyelot And Empty(lcDyelot)
    If !Empty(lcRetSty1) And;
        SEEK(Iif(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty1,loParentForm.lcTmpItem) And;
        EVALUATE(loParentForm.lcTmpItem+'.CDYE_FLG')='Y'
      *--The style xxx comes in dyelot but the original style xxxx did not use dyelots,
      *--Please make sure the the other quality style has same dyelot usage.
      =gfModalGen('TRM34202B42000','DIALOG',Alltrim(lcMsg)+' '+lcRetSty1+'|'+;
        EVALUATE(loParentForm.lcTmpLine+'.Style')+'|'+Alltrim(lcMsg))
      *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
      *loFormSet.AriaForm1.kbItemQuality2.SetFocus
      *N000682,1 MMT 04/19/2013 Fix Media issues[Start]
      *IF !loFormSet.llSilentMod
      If !loParentForm.llSilentMod
        *N000682,1 MMT 04/19/2013 Fix Media issues[End]
        loFormSet.ariaform1.kbItemQuality2.SetFocus
      Endif
      *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
      Select (lnAlias)
      Return .F.
    Endif
    If !Empty(lcRetSty2) And;
        SEEK(Iif(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+lcRetSty2,loParentForm.lcTmpItem) And;
        EVALUATE(loParentForm.lcTmpItem+'.CDYE_FLG')='Y'
      *--The style xxx comes in dyelot but the original style xxxx did not use dyelots,
      *--Please make sure the the other quality style has same dyelot usage.
      =gfModalGen('TRM34202B42000','DIALOG',Alltrim(lcMsg)+' '+lcRetSty2+'|'+;
        EVALUATE(loParentForm.lcTmpLine+'.Style')+'|'+Alltrim(lcMsg))
      *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
      *loFormSet.AriaForm1.kbItemQuality3.SetFocus
      *N000682,1 MMT 04/19/2013 Fix Media issues[Start]
      *IF !loFormSet.llSilentMod
      If !loParentForm.llSilentMod
        *N000682,1 MMT 04/19/2013 Fix Media issues[End]
        loFormSet.ariaform1.kbItemQuality3.SetFocus
      Endif
      *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
      Select (lnAlias)
      Return .F.
    Endif
  Endif

  *--If Receive Return P/O and costs method FIFO or LIFO.
  *--Not Accept to Issue more than Stock.
  If loParentForm.llIssue And;
      IIF(loParentForm.lcInvType="0001",loParentForm.lcCostMth,loParentForm.lcCostMthM) $ 'FLI'
    If laSok[1]+laSok[2]+laSok[3]+laSok[4]+laSok[5]+laSok[6]+laSok[7]+laSok[8]<>0
      *--Get Current Stock.
      If !Seek(Iif(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+;
          PADR(lcStyle,19)+lcwarecode+Padr(lcDyelot,10),loParentForm.lcItemLoc)
        If loParentForm.lcInvType = "0001"
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
          *!*	          lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
          *!*	            PADR(lcStyle,19)+lcWareCode+PADR(lcDyelot,10) + "'"
          *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
          *!*	          lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
          *!*	            PADR(lcStyle,19)+lcWareCode+PADR(lcDyelot,10) + "]"
          lcStyDyeVal = Padr(lcStyle,19)+lcwarecode+Padr(lcDyelot,10)
          lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = ?m.lcStyDyeVal "
          *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
        Else
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
            "Dyelot = '" + Padr(lcDyelot,10) + "'"
          *B607658,1 KHM 07/07/2005
        Endif

        =lfGetItmInf(loParentForm.lcInvType,;
          IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+Padr(lcStyle,19)+lcwarecode+Padr(lcDyelot,10),;
          loParentForm.lcItemLoc,;
          IIF(loParentForm.lcInvType = "0001",'STYDYE','ITEMLOC'),lcSqlStatement,;
          loParentForm.lcInvType = "0002")
        =Seek(Iif(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+;
          PADR(lcStyle,19)+lcwarecode+Padr(lcDyelot,10),loParentForm.lcItemLoc)
      Endif
      Select (loParentForm.lcItemLoc)
      Scatter Fields Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 To laSStk
      For I=1 To 8
        If laSStk[I] < laSok[I]
          *--Insufficient stock to issue P/O line!
          = gfModalGen('TRM34107B34000','DIALOG')
          Select (loFormSet.lcLineQty)
          =Seek('4')
          *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
          *loFormSet.AriaForm1.grdLineQty.SetFocus
          If !loFormSet.llSilentMod
            loFormSet.ariaform1.grdLineQty.SetFocus
          Endif
          *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
          Select(lnAlias)
          Return .F.
        Endif
      Endfor
    Endif
    If laDam1[1]+laDam1[2]+laDam1[3]+laDam1[4]+laDam1[5]+laDam1[6]+laDam1[7]+laDam1[8]<>0
      If !Seek(Iif(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+;
          PADR(lcRetSty1,19)+lcwarecode+Space(10),loParentForm.lcItemLoc)
        If loParentForm.lcInvType = "0001"
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
          *!*	          lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
          *!*	            PADR(lcRetSty1,19)+lcWareCode+SPACE(10) + "'"
          *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
          *!*	          lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
          *!*	            PADR(lcRetSty1,19)+lcWareCode+SPACE(10) + "]"
          lcSeleStyDyeV =Padr(lcRetSty1,19)+lcwarecode+Space(10)
          lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot =?m.lcSeleStyDyeV "
          *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
        Else
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
            "Dyelot = '" + Space(10) + "'"
          *B607658,1 KHM 07/07/2005 [End]
        Endif

        =lfGetItmInf(loParentForm.lcInvType,;
          IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+Padr(lcRetSty1,19)+lcwarecode+Space(10),;
          loParentForm.lcItemLoc,;
          IIF(loParentForm.lcInvType = "0001",'STYDYE','ITEMLOC'),lcSqlStatement,;
          loParentForm.lcInvType = "0002")
        =Seek(Iif(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+;
          PADR(lcRetSty1,19)+lcwarecode+Space(10),loParentForm.lcItemLoc)
      Endif
      Select (loParentForm.lcItemLoc)
      Scatter Fields Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 To laSStk
      For I=1 To 8
        If laSStk[I] < laDam1[I]
          *--Insufficient stock to issue P/O line!
          = gfModalGen('TRM34107B34000','DIALOG')
          Select (loFormSet.lcLineQty)
          =Seek('5')
          *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
          *loFormSet.AriaForm1.grdLineQty.SetFocus
          If !loFormSet.llSilentMod
            loFormSet.ariaform1.grdLineQty.SetFocus
          Endif
          *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]

          Select(lnAlias)
          Return .F.
        Endif
      Endfor
    Endif
    If laDam2[1]+laDam2[2]+laDam2[3]+laDam2[4]+laDam2[5]+laDam2[6]+laDam2[7]+laDam2[8]<>0
      If !Seek(Iif(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+;
          PADR(lcRetSty2,19)+lcwarecode+Space(10),loParentForm.lcItemLoc)
        If loParentForm.lcInvType = "0001"
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][Start]
          *!*	          lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = '" + ;
          *!*	            PADR(lcRetSty2,19)+lcWareCode+SPACE(10) + "'"
          *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][Start]
          *!*	          lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = [" + ;
          *!*	            PADR(lcRetSty2,19)+lcWareCode+SPACE(10) + "]"
          lcStyDyeSelVal =Padr(lcRetSty2,19)+lcwarecode+Space(10)
          lcSqlStatement = "SELECT * FROM STYDYE WHERE Style+cWareCode+Dyelot = ?m.lcStyDyeSelVal "
          *! B610539,2 MMT 10/13/2013 PO receiving program crashes if styles has ' or '[' or any special character[T20130926.0017][End]
          *! B610539,1 MMT 10/02/2013 PO receiving program crashes if styles has '[T20130926.0017][End]
        Else
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
            "Dyelot = '" + Space(10) + "'"
          *B607658,1 KHM 07/07/2005
        Endif

        =lfGetItmInf(loParentForm.lcInvType,;
          IIF(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+Padr(lcRetSty2,19)+lcwarecode+Space(10),;
          loParentForm.lcItemLoc,;
          IIF(loParentForm.lcInvType = "0001",'STYDYE','ITEMLOC'),lcSqlStatement,;
          loParentForm.lcInvType = "0002")
        =Seek(Iif(loParentForm.lcInvType="0001","",loParentForm.lcInvType)+;
          PADR(lcRetSty2,19)+lcwarecode+Space(10),loParentForm.lcItemLoc)
      Endif
      Select (loParentForm.lcItemLoc)
      Scatter Fields Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 To laSStk
      For I=1 To 8
        If laSStk[I] < laDam1[I]
          *--Insufficient stock to issue P/O line!
          = gfModalGen('TRM34107B34000','DIALOG')
          Select (loFormSet.lcLineQty)
          =Seek('6')
          *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [Start]
          *loFormSet.AriaForm1.grdLineQty.SetFocus
          If !loFormSet.llSilentMod
            loFormSet.ariaform1.grdLineQty.SetFocus
          Endif
          *! E302980,1 SAB 10/12/2011 Run Inter-Location PO and Issue Inter-Location PO Screens in Silent Mode [End]
          Select(lnAlias)
          Return .F.
        Endif
      Endfor
    Endif
  Endif
  Select (lnAlias)
Endif

Return .T.

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
Function lfEditLnCst
Lparameters loFormSet, lnBarNo

Do Case
Case lnBarNo = 1
  loFormSet.llEditLCst = !loFormSet.llEditLCst
  Set Mark Of Bar 1 Of _OPTIONPOP To _Screen.ActiveForm.Parent.llEditLCst
Endcase

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
Function lfvLCost
Lparameters loFormSet

*B126833,1 WAM 04/03/2005 Add new button to view/Edit landed cost
Select (loFormSet.lcTmpLine)

Set Filter To
lcLKey = cCarton+PO+Style+Dyelot+Padr(cWareCode,6)+Str(Lineno,6)
lcSndGrd = "2"
lcTrdGrd = "3"
Do Case
Case cStyGrade = '2'
  lcSndGrd = "1"
  lcTrdGrd = "3"
Case cStyGrade = '3'
  lcSndGrd = "1"
  lcTrdGrd = "2"
Endcase
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
Select (loFormSet.lcTmpLine)
If !lDetCost And loFormSet.llEditLCst
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
  If Seek('2'+lcLKey)
    Replace nLan_Cost1 With lnEqCost1, nLan_Cost2 With lnEqCost2, nLan_Cost3 With lnEqCost3,;
      nLan_Cost4 With lnEqCost4, nLan_Cost5 With lnEqCost5, nLan_Cost6 With lnEqCost6,;
      nLan_Cost7 With lnEqCost7,;
      nFLanCost1 With lnCostVal1, nFLanCost2 With lnCostVal2, nFLanCost3 With lnCostVal3,;
      nFLanCost4 With lnCostVal4, nFLanCost5 With lnCostVal5,;
      nFLanCost6 With lnCostVal6, nFLanCost7 With lnCostVal7
  Endif

  *--2) Update Damaged quantity.
  =Seek('4'+lcLKey)
  Locate Rest While TranCd+cCarton+PO+Style+Dyelot+Padr(cWareCode,6)+Str(Lineno,6)='4'+lcLKey ;
    FOR cStyGrade=lcSndGrd
  If Found()
    Replace nLan_Cost1 With lnEqCost1, nLan_Cost2 With lnEqCost2, nLan_Cost3 With lnEqCost3,;
      nLan_Cost4 With lnEqCost4, nLan_Cost5 With lnEqCost5, nLan_Cost6 With lnEqCost6,;
      nLan_Cost7 With lnEqCost7,;
      nFLanCost1 With lnCostVal1, nFLanCost2 With lnCostVal2, nFLanCost3 With lnCostVal3,;
      nFLanCost4 With lnCostVal4, nFLanCost5 With lnCostVal5,;
      nFLanCost6 With lnCostVal6, nFLanCost7 With lnCostVal7
  Endif
  =Seek('4'+lcLKey)
  Locate Rest While TranCd+cCarton+PO+Style+Dyelot+Padr(cWareCode,6)+Str(Lineno,6)='4'+lcLKey ;
    FOR cStyGrade=lcTrdGrd
  If Found()
    Replace nLan_Cost1 With lnEqCost1, nLan_Cost2 With lnEqCost2, nLan_Cost3 With lnEqCost3,;
      nLan_Cost4 With lnEqCost4, nLan_Cost5 With lnEqCost5, nLan_Cost6 With lnEqCost6,;
      nLan_Cost7 With lnEqCost7,;
      nFLanCost1 With lnCostVal1, nFLanCost2 With lnCostVal2, nFLanCost3 With lnCostVal3,;
      nFLanCost4 With lnCostVal4, nFLanCost5 With lnCostVal5,;
      nFLanCost6 With lnCostVal6, nFLanCost7 With lnCostVal7
  Endif
  *--3) Update cancel quantity.
  If Seek('5'+lcLKey)
    Replace nLan_Cost1 With lnEqCost1, nLan_Cost2 With lnEqCost2, nLan_Cost3 With lnEqCost3,;
      nLan_Cost4 With lnEqCost4, nLan_Cost5 With lnEqCost5, nLan_Cost6 With lnEqCost6,;
      nLan_Cost7 With lnEqCost7,;
      nFLanCost1 With lnCostVal1, nFLanCost2 With lnCostVal2, nFLanCost3 With lnCostVal3,;
      nFLanCost4 With lnCostVal4, nFLanCost5 With lnCostVal5,;
      nFLanCost6 With lnCostVal6, nFLanCost7 With lnCostVal7
  Endif
Endif
Set Filter To TranCd = '1'
=Seek('1'+lcLKey)
loFormSet.Refresh
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
Function lfUnDo
Lparameters loFormSet
If .F.
  lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'',.T.)
  If Type('lcTranCode') = 'N'
    =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
  Else
    Select (loFormSet.lcTmpLine)
    Set Order To Tag TmpLine3
    Go Top
    Do While !Eof()
      lcPo = PO
      lcSelString = "UPDATE POSHDR SET lLok_stat =0,cLok_User= '', dLok_Date='',cLok_Time='' WHERE cBusDocu+cStyType+PO='"+Evaluate(loFormSet.lcTmpLine+'.cBusDocu')+Evaluate(loFormSet.lcTmpLine+'.cStyType')+Evaluate(loFormSet.lcTmpLine+'.Po')+"'"
      If lnResult <=0
        =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLUPDATE",lnResult,.T.)
        =oAriaApplication.RemoteCompanyData.RollBackTran (lcTranCode)
        Return
      Endif
      Select (loFormSet.lcTmpLine)
      Scan Rest While PO = lcPo
      Endscan
    Enddo
    =oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
  Endif
Endif
*:*************************************************************
*! Name     : lfPrnLanded
*! Developer: Wael M. Abo-Shawareb (WSH)
*! Date     : 04/11/2005
*! Purpose  : Calculate landed costs case of detail costing.
*:*************************************************************
*! B125565,1
Function lfPrnLanded
Lparameters loFormSet

Local lnCnt

Private lcPriceCur, lcDutyCur, lcIType1, lcIType2, lcIType3, lcIType4, lcIType5, lcIType6, lcIType7
Dimension laECost[1]

Select (lcMastPoHd)
Locate
lcPriceCur = Evaluate(lcMastPoHd+'.cPriceCur')
lcDutyCur  = Evaluate(lcMastPoHd+'.cDutyCur')

If loFormSet.lcPType = 'S'
  loBomLine.SETORDER('BomLnShp')
Else
  loBomLine.SETORDER('BomLine')
Endif

Select (lcRepCurs)
Scatter Fields Qty1, Qty2, Qty3, Qty4, Qty5, Qty6, Qty7, Qty8 To laLnQty

For lnCnt = 1 To 7
  lcCnt = Str(lnCnt,1)
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
  lcBomLKey = Iif(loFormSet.lcPType = 'D', 'D',Iif(loFormSet.lcPType = 'W' ,'T', 'I')) + '2' + Iif(loFormSet.lcPType = 'S', Evaluate(lcRepCurs + '.Shipno'), '') + Evaluate(lcRepCurs + '.Po') + Str(Evaluate(lcRepCurs + '.LineNo'), 6) + lcCnt + loFormSet.lcInvType + Evaluate(lcRepCurs + '.Style')
  *N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][END]
  lcWCondtn = "cImTyp+cType+IIF(loFormSet.lcPType='S',ShipNo,'')+cTktNo+STR(LineNo,6)+cBomTyp+cInvType+Style = lcBomLKey"
  lcFCondtn = "EMPTY(cRSession) AND cStyGrade = " + lcRepCurs + ".cStyGrade"

  lcIType&lcCnt = loFormSet.lcIType&lcCnt
  lnNLCs&lcCnt  = 0
  lnCurSQt      = 0

  If loBomLine.Seek(lcBomLKey)
    Select (lcBomLine)
    Sum Rest (UnitCost*UnitQty) * lfBomSzQty() While &lcWCondtn For &lcFCondtn To lnNLCs&lcCnt
    lnNLCs&lcCnt = Iif(Evaluate(lcRepCurs + '.TotQty') <> 0, (lnNLCs&lcCnt / Evaluate(lcRepCurs + '.TotQty')), 0)
  Endif

  Select (lcRepCurs)
  Replace nFLanCost&lcCnt With lnNLCs&lcCnt

  If !loFormSet.llMFCall
    If loFormSet.llMulCurr
      =lfGetEqv(lcCnt, loFormSet.lnRate1, loFormSet.lnRate2, loFormSet.lnCurrUnt1, loFormSet.lnCurrUnt2, nFLanCost1, nFLanCost2, nFLanCost3, nFLanCost4, nFLanCost5, nFLanCost6, nFLanCost7)
      Replace nLan_Cost&lcCnt With laECost[1]
    Else
      Replace nLan_Cost&lcCnt With lnNLCs&lcCnt
    Endif
  Endif
Endfor

loBomLine.SETORDER('BOMLINE')

Select (lcRepCurs)
Return

*:*************************************************************
*! Name     : lfChekAdj
*! Developer: Wael M. Abo-Shawareb (WSH)
*! Date     : 04/11/2005
*! Purpose  : Check if adjust cost for recieve..
*:*************************************************************
*! B125565,1
Function lfChekAdj
Lparameters lcTrType, lcTket, lcItem, lcColor, lcSess, lcStyQlt

Local lnOldAls, llToREt
lnOldAls = Select(0)
lcItem   = Padr(lcItem, 19)
llToREt  = .F.

lcSeekKey = Iif(lcTrType = 'S', 'I', lcTrType) + '2' + lcTket
lcWhleCnd = "cIMTyp+cType+cTktNo+STR(LineNo,6)=lcSeekKey"

lcForCond = "Style=lcItem AND EMPTY(cRSession)"
lcQltFltr = Iif(Type('lcStyQlt') $ 'UL' ,".T.","cStyGrade = lcStyQlt")

loBomLine.SETORDER('BOMLINE')
If loBomLine.Seek(lcSeekKey)
  Select (lcBomLine)
  Locate Rest While &lcWhleCnd For &lcForCond And &lcQltFltr
  llToREt = Found()
Endif

Select(lnOldAls)
Return llToREt

*!***************************************************************************
*! Name     : lfBomSzQty
*! Developer: Wael M. Abo-Shawareb (WSH)
*! Date     : 04/11/2005
*! Purpose  : Calculate the style quantity per size.
*!******************************************************************************
*! B125565,1
Function lfBomSzQty

lnCurSQt = 0
For lnI=1 To 8
  If Str(lnI,1) $ Evaluate(lcBomLine + '.CSIZES')
    lnCurSQt = lnCurSQt + laLnQty[lnI]
  Endif
Endfor
Return lnCurSQt

*N038893,1 WAM 06/02/2005 Receive Cutting Ticket
Function lfDyeOvrRcv


*!* N000601,1 MMT 03/20/2007 Convert Rolls Screen to Aria4XP [START]
*!***************************************************************************
*! Name     : lfvRollAss
*! Developer: Mariam Mazhar[MMT]
*! Date     : 03/21/2007
*! Purpose  : Assign rolls
*!******************************************************************************
Function lfvRollAss
Parameters loFormSet

lnJSgn   = Iif(loFormSet.lcPType $ 'G',-1,1)
Declare aAdjStk[9]
aAdjStk = 0
aAdjStk[1]  = lnJSgn*Evaluate(loFormSet.lcTmpLine+'.TotStk')
aAdjStk[9]  = aAdjStk[1]
For LoopNo=2 To 8
  aAdjStk[LoopNo]= 0
  aAdjStk[9] = aAdjStk[9] + aAdjStk[LoopNo]
Next

ldTranDate =loFormSet.ariaform1.dtPickerPostingDate.Value
ldPostDate =loFormSet.ariaform1.dtpickerReceivingDate.Value
*! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[Start]
*!*	DO (oAriaApplication.ApplicationHome+'MAJrRoData.PRG') WITH IIF(loFormSet.lcPType $ 'G','6','5'),;
*!*				loFormset.lcInvType,EVALUATE(loFormSet.lcTmpLine+'.Style'),;
*!*				EVALUATE(loFormSet.lcTmpLine+'.Dyelot'),EVALUATE(loFormSet.lcTmpLine+'.cWareCode'),;
*!*				"",aAdjStk,loFormSet.lcTmpJour,loFormSet.lcFullRoll,;
*!*				loFormSet.lcTmpRoll,ldTranDate ,ldPostDate
If File(oAriaApplication.clientapplicationhome+'MAJrRoData.FXP')
  Do (oAriaApplication.clientapplicationhome+'MAJrRoData.FXP') With Iif(loFormSet.lcPType $ 'G','6','5'),;
    loFormSet.lcInvType,Evaluate(loFormSet.lcTmpLine+'.Style'),;
    EVALUATE(loFormSet.lcTmpLine+'.Dyelot'),Evaluate(loFormSet.lcTmpLine+'.cWareCode'),;
    "",aAdjStk,loFormSet.lcTmpJour,loFormSet.lcFullRoll,;
    loFormSet.lcTmpRoll,ldTranDate ,ldPostDate


Else
  Do (oAriaApplication.ApplicationHome+'MAJrRoData.FXP') With Iif(loFormSet.lcPType $ 'G','6','5'),;
    loFormSet.lcInvType,Evaluate(loFormSet.lcTmpLine+'.Style'),;
    EVALUATE(loFormSet.lcTmpLine+'.Dyelot'),Evaluate(loFormSet.lcTmpLine+'.cWareCode'),;
    "",aAdjStk,loFormSet.lcTmpJour,loFormSet.lcFullRoll,;
    loFormSet.lcTmpRoll,ldTranDate ,ldPostDate
Endif
*! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[End]
Replace TotStk With lnJSgn*aAdjStk[1] In (loFormSet.lcTmpLine)
*! B609377,1 MMT 08/09/2010 Material PO Receiving-rolls not matching total[START]
lnOldAlis = Select(0)
Select(loFormSet.lcTmpLine)
lcLineKey = cCarton+PO+Style+Dyelot+Padr(cWareCode,6)+Str(Lineno,6)
lcCurFilt = Set("Filter")
Set Filter To
If Seek('2'+lcLineKey)
  Replace Qty1 With  lnJSgn*aAdjStk[1] ,;
    TotQty With  lnJSgn*aAdjStk[1]
Endif
Set Filter To &lcCurFilt.
=Seek('1'+lcLineKey)
Select(lnOldAlis)
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
Function lfGetBomLn
Lparameters loFormSet, lcStyType, lcPo, lcShipNo
lnAlias = Select()
*N037687,1 MMT 09/30/2012 Convert Receive MFG Order to Aria4xp[T20110914.0019][Start]
*lcCstShtTyp = IIF(lcStyType = "U","M",IIF(lcStyType = "P","I",lcStyType))
lcCstShtTyp = Iif(lcStyType = "U","M",Iif(lcStyType = "P","I",Iif(lcStyType = "F","T",lcStyType)))
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
If loFormSet.lcPType = 'S' .And. !Empty(lcShipNo)
  lcTrancd = Iif(loFormSet.lcPType $ 'SUF','3','6')
  lcSqlStatement = "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+ "WHERE cImTyp = '" + lcCstShtTyp +;
    "' AND CTYPE ='2' AND ShipNo = '" + lcShipNo + "' AND CRSESSION = '' AND CTKTNO IN ("
  lcSqlStatement  =  lcSqlStatement+"SELECT PO FROM POSLN WHERE POSLN.cBusDocu = '" + loFormSet.lcBusDoc +;
    "' AND POSLN.cStyType = '" + loFormSet.lcWorkOrd +;
    "' AND POSLN.ShipNo = '" + lcShipNo + "' AND TranCd ='" + lcTrancd + "') UNION "

  lcSqlStatement = lcSqlStatement  + "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+ "WHERE cImTyp = '" + lcCstShtTyp +;
    "' AND CTYPE ='1' AND CTKTNO IN ("
  lcSqlStatement  =  lcSqlStatement+"SELECT PO FROM POSLN WHERE POSLN.cBusDocu = '" + loFormSet.lcBusDoc +;
    "' AND POSLN.cStyType = '" + loFormSet.lcWorkOrd +;
    "' AND POSLN.ShipNo = '" + lcShipNo + "' AND TranCd ='" + lcTrancd + "')"

Else
  lcSqlStatement = "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+"WHERE cImTyp = '" + lcCstShtTyp +;
    "' AND cTktNo ='" + lcPo + "' AND CTYPE ='2' AND CRSESSION = '' UNION "

  lcSqlStatement = lcSqlStatement  + "SELECT * FROM BOMLINE [INDEX=BOMLINE] "+"WHERE cImTyp = '" + lcCstShtTyp +;
    "' AND cTktNo ='" + lcPo + "' AND CTYPE ='1'"
Endif
=lfOpenSql(lcSqlStatement,'BOMLINE',loFormSet.lcMastBomLn, "","",.F.)
Select (loFormSet.lcMastBomLn)
*B609517,1 TMI 02/05/2011 [Start]   make the property lladdbomline = .T. only if there are records have ctype ='2' and empty crsession in lcMastBomLn file
*LOCATE
Locate For cType ='2' And Empty(cRSession)
*B609517,1 TMI 02/05/2011 [End  ]
If Eof()
Else
  loFormSet.llAddBomLine = .T.
Endif
*B608643,1 WAM 08/05/2008 (End)

If loFormSet.lcPType = 'S' .And. !Empty(lcShipNo)
  Select (loFormSet.lcMastBomLn)
  Replace All ShipNo With lcShipNo
Endif
Dimension laIndex[1,2]
laIndex = ''
laIndex[1,1] = 'cImTyp+cType+ShipNo+cTktNo+STR(LineNo,6)'
laIndex[1,2] = loFormSet.lcMastBomLn
=lfSetIndex(loFormSet.lcMastBomLn,@laIndex)
Select (lnAlias)


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
Function lfBaseCost

*B608718,1 WAM 10/09/2008 Store budget quantity
*LPARAMETERS loFormSet, lcStyType, lcPO, lcPriceCur, lnPriceRat, lnCurrUnit, lcDutyCur, lnDutyRat, lnDCurUnit
Lparameters loFormSet, lcStyType, lcPo, lcPriceCur, lnPriceRat, lnCurrUnit, lcDutyCur, lnDutyRat, lnDCurUnit, laEstQty
*B608718,1 WAM 10/09/2008 (End)

lnAlias = Select()
lnLineNo = Evaluate(loFormSet.lcTmpLine+'.LineNo')
*! B610107,1 MMT 10/04/2012 Add Foreign currency costing to Material MFG order summary[Start]
*lcCstShtTyp = IIF(lcStyType = "U","M",IIF(lcStyType = "P","I",lcStyType))
lcCstShtTyp = Iif(lcStyType = "U","M",Iif(lcStyType = "P","I",Iif(lcStyType = "F","T",lcStyType)))
*! B610107,1 MMT 10/04/2012 Add Foreign currency costing to Material MFG order summary[End]
*-- Get the information from the BomLine
lcShipNo = Padr(loFormSet.ariaform1.cntShipment.kbShipNo.keytextbox.Value,6)
Dimension loFormSet.laECost[7]
Store 0 To loFormSet.laECost
*B608760,1 WAM 12/04/2008 Get foreign cost from the adjusted record for receiving
Dimension laEstiCost[7]
Store 0 To laEstiCost
*B608760,1 WAM 12/04/2008 (End)

Select (loFormSet.lcMastBomLn)

*: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
If Seek(lcCstShtTyp+'2'+lcShipNo +lcPo+Str(lnLineNo,6))
  Scan Rest While cIMTyp+cType+ShipNo+cTktNo+Str(Lineno,6) = lcCstShtTyp+'2'+lcShipNo +lcPo+Str(lnLineNo,6)
    lnBomType = Evaluate(CBOMTYP)
    If Isnull(CCURRCODE) Or Empty(CCURRCODE)
      Do Case
      Case CCATGTYP = 'P'
        If loFormSet.llMulCurr And !Empty(lcPriceCur)
          lcCurrCode = lcPriceCur
          lnExRate   = Iif(lnPriceRat=0,1, lnPriceRat)
          lnCurrUnit = Iif(lnCurrUnit=0,1, lnCurrUnit)
        Else
          lcCurrCode = oAriaApplication.BaseCurrency
          lnExRate   = 1
          lnCurrUnit = 1
        Endif
      Case Inlist(CCATGTYP,'D','M')
        If loFormSet.llMulCurr And !Empty(lcDutyCur)
          lcCurrCode = lcDutyCur
          lnExRate   = Iif(lnDutyRat=0,1, lnDutyRat)
          lnCurrUnit = Iif(lnDCurUnit=0,1, lnDCurUnit)
        Else
          lcCurrCode = oAriaApplication.BaseCurrency
          lnExRate   = 1
          lnCurrUnit = 1
        Endif
      Otherwise
        lcCurrCode = oAriaApplication.BaseCurrency
        lnExRate   = 1
        lnCurrUnit = 1
      Endcase
    Else
      lcCurrCode = CCURRCODE
      lnExRate   = Iif(Isnull(nExRate) Or nExRate=0,1,nExRate)
      lnCurrUnit = Iif(Isnull(nCurrUnit) Or nCurrUnit=0,1,nCurrUnit)
    Endif
    Store '/' To lcExSign, lcUntSin
    lcExSign = gfGetExSin(@lcUntSin, lcCurrCode)

    *B608718,1 WAM 10/09/2008 Get cost per selected sizes
    *lnEquCost = UnitCost*UnitQty &lcExSign lnExRate &lcUntSin lnCurrUnit
    lcBomSizes = Iif(Empty(CSIZES),'12345678',CSIZES)
    lnTransQty  = 0
    For lnCntr = 1 To 8
      If Str(lnCntr,1) $ lcBomSizes
        lnTransQty = lnTransQty + laEstQty[lnCntr]
      Endif
    Endfor
    lnEquCost = UnitCost*UnitQty* (lnTransQty/laEstQty[9]) &lcExSign lnExRate &lcUntSin lnCurrUnit
    *B608718,1 WAM 10/09/2008 (End)

    *B608760,1 WAM 12/04/2008 Get foreign cost from the adjusted record for receiving
    laEstiCost[lnBomType]  = laEstiCost[lnBomType] + UnitCost*UnitQty* (lnTransQty/laEstQty[9])
    *B608760,1 WAM 12/04/2008 (End)

    loFormSet.laECost[lnBomType] = loFormSet.laECost[lnBomType] + lnEquCost
    lnCrRt1 = Iif(CCATGTYP = 'P',lnExRate,lnCrRt1)
  Endscan
Else
  *: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]
  =Seek(lcCstShtTyp+'1'+lcShipNo +lcPo+Str(lnLineNo,6))
  Scan Rest While cIMTyp+cType+ShipNo+cTktNo+Str(Lineno,6) = lcCstShtTyp+'1'+lcShipNo +lcPo+Str(lnLineNo,6)
    lnBomType = Evaluate(CBOMTYP)
    If Isnull(CCURRCODE) Or Empty(CCURRCODE)
      Do Case
      Case CCATGTYP = 'P'
        If loFormSet.llMulCurr And !Empty(lcPriceCur)
          lcCurrCode = lcPriceCur
          lnExRate   = Iif(lnPriceRat=0,1, lnPriceRat)
          lnCurrUnit = Iif(lnCurrUnit=0,1, lnCurrUnit)
        Else
          lcCurrCode = oAriaApplication.BaseCurrency
          lnExRate   = 1
          lnCurrUnit = 1
        Endif
      Case Inlist(CCATGTYP,'D','M')
        If loFormSet.llMulCurr And !Empty(lcDutyCur)
          lcCurrCode = lcDutyCur
          lnExRate   = Iif(lnDutyRat=0,1, lnDutyRat)
          lnCurrUnit = Iif(lnDCurUnit=0,1, lnDCurUnit)
        Else
          lcCurrCode = oAriaApplication.BaseCurrency
          lnExRate   = 1
          lnCurrUnit = 1
        Endif
      Otherwise
        lcCurrCode = oAriaApplication.BaseCurrency
        lnExRate   = 1
        lnCurrUnit = 1
      Endcase
    Else
      lcCurrCode = CCURRCODE
      lnExRate   = Iif(Isnull(nExRate) Or nExRate=0,1,nExRate)
      lnCurrUnit = Iif(Isnull(nCurrUnit) Or nCurrUnit=0,1,nCurrUnit)
    Endif
    Store '/' To lcExSign, lcUntSin
    lcExSign = gfGetExSin(@lcUntSin, lcCurrCode)

    *B608718,1 WAM 10/09/2008 Get cost per selected sizes
    *lnEquCost = UnitCost*UnitQty &lcExSign lnExRate &lcUntSin lnCurrUnit
    lcBomSizes = Iif(Empty(CSIZES),'12345678',CSIZES)
    lnTransQty  = 0
    For lnCntr = 1 To 8
      If Str(lnCntr,1) $ lcBomSizes
        lnTransQty = lnTransQty + laEstQty[lnCntr]
      Endif
    Endfor
    lnEquCost = UnitCost*UnitQty* (lnTransQty/laEstQty[9]) &lcExSign lnExRate &lcUntSin lnCurrUnit
    *B608718,1 WAM 10/09/2008 (End)

    *B608760,1 WAM 12/04/2008 Get foreign cost from the adjusted record for receiving
    laEstiCost[lnBomType]  = laEstiCost[lnBomType] + UnitCost*UnitQty* (lnTransQty/laEstQty[9])
    *B608760,1 WAM 12/04/2008 (End)

    loFormSet.laECost[lnBomType] = loFormSet.laECost[lnBomType] + lnEquCost
    lnCrRt1 = Iif(CCATGTYP = 'P',lnExRate,lnCrRt1)
  Endscan

  *: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[Start]
Endif
*: B608510,1 MMT 05/20/2008 Convert Shipment Cost sheet program to ARIA4[End]
Select (lnAlias)


*B608606,1 MMT 07/08/2008 Add budget record in POSLN in case of rec. style not in PO[Start]
*!*************************************************************
*! Name     : lfUpdBom
*! Developer: Mariam Mazhar
*! Date     : 07/08/2008
*! Purpose  : Function to update po bill of material.
*!*************************************************************
Function lfUpdBom
Para lcAction
Private lnAlias

lnAlias  = Select(0)
llRetVal = .T.
lcShtTyp = Iif(llMFCall,'M','I')
lcShtKey = &lcPoshdrAlias..PO
If !Seek(lcShtTyp +lcShtKey ,lcCtktBomAlias)
  gfSEEK(lcShtTyp +lcShtKey ,lcCtktBomAlias)
Endif

lclinkCode= Evaluate(lcCtktBomAlias+'.Link_Code')
lcStyle   = &lcTmpLine..Style
lnPoLNo   = Iif(llMFCall ,0,&lcTmpLine..Lineno)

lcShtDye  = &lcTmpLine..Dyelot

lnPrice   = &lcTmpLine..nFCost1

lcLastOpr = Iif(!Empty(&lcPoshdrAlias..clastopr),&lcPoshdrAlias..clastopr,&lcTmpLine..clastopr)

lcItmWare = &lcPoshdrAlias..cItemWare
lcMatWare = &lcPoshdrAlias..cMatWare

Select (lcTmpLine)
Scatter Fields Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty To laBomQty
laBomQty[9] = 0

For lnSize = 1 To 8
  laBomQty[9] = laBomQty[9] + laBomQty[lnSize]
Endfor

*! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [Start]
*!*	llRetVal = gfSheetItem (lcShtTyp ,lcShtKey ,lcLinkCode,lcStyle ,;
*!*	                        lcInvType    , lnPoLNo   , lcShtDye , lcItmWare   ,;
*!*	                        lcMatWare, @laBomQty,  lcBomAlias , lcCtktBomAlias      ,;
*!*	                        lcBomLineAlias, lcMFGOprHdAlias, @lcLastOpr , lnPrice   ,;
*!*	                        0, 0, 0, 0, 0,0,0,lcPoshdrAlias ,lcPoslnAlias  ,;
*!*	                          lcTmpFileNam,.T.)
llRetVal = gfSheetItem (lcShtTyp ,lcShtKey ,lclinkCode,lcStyle ,;
  lcInvType    , lnPoLNo   , lcShtDye , lcItmWare   ,;
  lcMatWare, @laBomQty,  lcBomAlias , lcCtktBomAlias      ,;
  lcBomLineAlias, lcMFGOprHdAlias, @lcLastOpr , lnPrice   ,;
  0, 0, 0, 0, 0,0,0,lcTmpPoshdr,lcPoslnAlias  ,;
  lcTmpFileNam,.T.)
Replace clastopr With &lcTmpPoshdr..clastopr In (lcPoshdrAlias)
*! B609653,1 MMT 07/28/2011 Error “variable LLMULCURR not found” while posting style purchase order receiving [END]


Select (lcTmpLine)
lnTmpRecNo = Recno(lcTmpLine)
Replace lCostMade With llRetVal For Lineno = lnPoLNo
If (lnTmpRecNo # 0) And (lnTmpRecNo <= Reccount())
  Goto lnTmpRecNo
Endif
Select (lnAlias)

*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[Start]
Release lnAlias,lcShtTyp ,lcShtKey ,lclinkCode,lcStyle   ,lnPoLNo   ,lcShtDye  ,lnPrice   ,lcLastOpr ,lcItmWare ,lcMatWare ,laBomQty,lnSize ,lnTmpRecNo
*B609232,1 MMT 04/29/2010 Fix bug of error 'Too Many VAriables' in receiving program[End]


Return (llRetVal)
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
Function lfRmvDupl
Lparameters lcAlias,lcKeyFld,lcFldToUpd
Local lnSlct,lcOldOrd,laDist,lnI,lnCnt,lcTmpAls

lnSlct = Select(0)

lcTmpAls = gfTempName()
Select (lcAlias)
lcOldOrd = Order()
Set Order To
Locate
Copy To (oAriaApplication.WorkDir+lcTmpAls)
Dime laDist[1]
laDist = ''
Select Distinct &lcKeyFld From (oAriaApplication.WorkDir+lcTmpAls+'.dbf') Into Array laDist
If !Empty(laDist[1])
  For lnI = 1 To Alen(laDist,1)
    lnCnt = 0
    Locate
    Scan For &lcKeyFld = laDist[lnI]
      lnCnt = lnCnt + 1
      If lnCnt = 1
        Scatter Memvar Memo
      Else
        Replace &lcFldToUpd With m.&lcFldToUpd.+lnCnt-1
      Endif
    Endscan
  Endfor
Endif
If Used(lcTmpAls)
  Use In &lcTmpAls
Endif
Erase (oAriaApplication.WorkDir+lcTmpAls+'.*')

Select &lcAlias
Set Order To &lcOldOrd
Select (lnSlct)

*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[Start]
*!*************************************************************
*! Name      : lfSaveBatch
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : Save Batch
*!*************************************************************
Function lfSaveBatch
Lparameters loFormSet
lcInvType = loFormSet.lcInvType
lcPType = loFormSet.lcPType
lcTmpLine = loFormSet.lcTmpLine
ldDate = loFormSet.ariaform1.dtPickerPostingDate.Value
llMFCall = loFormSet.llMFCall
llImpCost = loFormSet.llImpCost
lcRecvType = loFormSet.lcPType
If lcPType = 'I'
  Select (lcTmpLine)
  Set Filter To
  Locate For lNewLn
  If Found()
    lfAddNewLn()
  Endif
Endif
lcDelSt = Set("Deleted")
Set Deleted Off
Select  (lcTmpLine)
lcOldOrd = Order()
Set Order To 'TmpLine2'
*Add Mode
If Empty(loFormSet.ariaform1.cntBatch.kbBatchNo.keytextbox.Value)
  If !Used('CTKTRCVL')
    =gfOpenTable('CTKTRCVL','CTKTRCVL')
  Endif
  If !Used('CTKTRCVH')
    =gfOpenTable('CTKTRCVH','CTKTRCVH')
  Endif
  lcNewRec = gfsequence('TMPRCVNUM')
  Select CTKTRCVH
  Append Blank
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

  Replace cType With Iif(loFormSet.llMFCall,'M',Iif(lcPType  = 'O','N','I')) ,;
    carton With .F.,;
    cDesc With '',;
    cStatus With 'O',;
    cWareCode With '',;
    dDate  With Iif(Type('ldDate')='D',ldDate,oAriaApplication.SystemDate),;
    nTotCan With 0,;
    nTotDam With 0,;
    nTotStk With 0,;
    ShipNo  With '',;
    TmpRcvNum With lcNewRec,;
    VENDOR  With  ''
  *! E303848,1 SARA.O 07/09/2017 Saving a ‘Receive by Shipment’ session will allow the user to save the receiving as a batch [Begin]
  If lcRecvType = 'S'
    Replace ShipNo With loFormSet.ariaform1.cntShipment.kbShipNo.keytextbox.Value
  Endif
  *! E303848,1 SARA.O 07/09/2017 Saving a ‘Receive by Shipment’ session will allow the user to save the receiving as a batch [Begin]

  *! E303177,1 HIA 07/28/2012 Get the batch creation date from the screen as per specs [T20120618.0015]  [End]
  Select (lcTmpLine)
  Set Filter To
  lnBatchLine = 0
  lnLineNo = 0
  lcPo = ''
  Scan For TranCd $ '245' And TotQty >0
    If (lnLineNo <> &lcTmpLine..Lineno) Or (lcPo <> &lcTmpLine..PO)
      lnBatchLine = lnBatchLine + 1
    Endif
    Select CtKtRcvL
    Append Blank
    Replace cCarton With '' ,;
      cRetSty With &lcTmpLine..cRetSty,;
      cStyGrade With &lcTmpLine..cStyGrade,;
      cType With Iif(loFormSet.llMFCall,'M',Iif(lcPType  = 'O','N','I'))  ,;
      Cuttkt With &lcTmpLine..PO,;
      cWareCode With &lcTmpLine..cWareCode,;
      Dyelot With &lcTmpLine..Dyelot,;
      FLAG With 'A',;
      LINENO  With &lcTmpLine..Lineno,;
      nLineNo With lnBatchLine,;
      Qty1 With &lcTmpLine..Qty1 ,;
      Qty2 With &lcTmpLine..Qty2 ,;
      Qty3 With &lcTmpLine..Qty3 ,;
      Qty4 With &lcTmpLine..Qty4 ,;
      Qty5 With &lcTmpLine..Qty5 ,;
      Qty6 With &lcTmpLine..Qty6 ,;
      Qty7 With &lcTmpLine..Qty7 ,;
      Qty8 With &lcTmpLine..Qty8 ,;
      REFERENCE With &lcTmpLine..Reference ,;
      STYLE With &lcTmpLine..Style ,;
      TmpRcvNum With lcNewRec,;
      TotQty With &lcTmpLine..TotQty ,;
      TranCd With &lcTmpLine..TranCd
    =gfAdd_Info('CTKTRCVL')
    =gfReplace("")
    Do Case
    Case TranCd  ='2'
      Replace  nTotStk With nTotStk+&lcTmpLine..TotQty  In 'CTKTRCVH'
    Case TranCd  ='4'
      Replace nTotDam With nTotDam+&lcTmpLine..TotQty In 'CTKTRCVH'
    Case TranCd  ='5'
      Replace  nTotCan With nTotCan +&lcTmpLine..TotQty In 'CTKTRCVH'
    Endcase
    lnLineNo = &lcTmpLine..Lineno
    lcPo = &lcTmpLine..PO
  Endscan

  If loFormSet.llApproveBatch And gfModalGen('QRM34062B34001','DIALOG')=1
    Select CTKTRCVH
    Replace cStatus With 'A'
  Endif
  Select CTKTRCVH
  =gfAdd_Info('CTKTRCVH')
  =gfReplace("")
  =gfTableUpdate()
  Select CtKtRcvL
  =gfTableUpdate()
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *=gfModalGen('INM42085B42000','DIALOG',LANG_POSTREC_TMPRECBATCH+'|'+lcNewRec)
  =gfModalGen('INM42085B42000','DIALOG',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_TMPRECBATCH,loFormSet.GetHeaderText("LANG_POSTREC_TMPRECBATCH",loFormSet.HeaderAlias))+'|'+lcNewRec)
  *N000682,1 11/20/2012 MMT Globlization changes[End]

Else
  *Edit Mode
  lcBatchNo = loFormSet.ariaform1.cntBatch.kbBatchNo.keytextbox.Value
  Select CTKTRCVH
  =gfSEEK(Iif(loFormSet.llMFCall,'M',Iif(loFormSet.lcPType $ 'LHO','N','I'))+lcBatchNo)
  Select CtKtRcvL
  =gfSEEK(Iif(loFormSet.llMFCall,'M',Iif(loFormSet.lcPType $ 'LHO','N','I'))+lcBatchNo)
  Select Max(nLineNo) From CtKtRcvL Into Array laLastLine
  If _Tally > 0
    lnBatchLine =  laLastLine[1]
  Endif
  Select (lcTmpLine)
  Set Filter To
  lnLineNum = 0
  lnLineNo = 0
  lcPo = ''
  Scan For TranCd $ '245'
    If &lcTmpLine..TotQty > 0 And !Seek(Iif(loFormSet.llMFCall,'M',Iif(lcPType  = 'O','N','I')) +lcBatchNo +&lcTmpLine..PO+&lcTmpLine..Style+&lcTmpLine..Dyelot+&lcTmpLine..cCarton+Str(&lcTmpLine..nLineNo,6)+Str(&lcTmpLine..Lineno,6)+&lcTmpLine..TranCd++&lcTmpLine..cStyGrade,'CTKTRCVL')
      If !Seek(Iif(loFormSet.llMFCall,'M',Iif(lcPType  = 'O','N','I')) +lcBatchNo +&lcTmpLine..PO+&lcTmpLine..Style+&lcTmpLine..Dyelot+&lcTmpLine..cCarton+Str(&lcTmpLine..nLineNo,6)+Str(&lcTmpLine..Lineno,6),'CTKTRCVL')
        If lnLineNo <> &lcTmpLine..Lineno Or (lcPo <> &lcTmpLine..PO)
          lnBatchLine = lnBatchLine + 1
          lnLineNum = lnBatchLine
        Endif
      Else
        lnLineNum = &lcTmpLine..nLineNo
      Endif
      Select CtKtRcvL
      Append Blank
      Replace cCarton With '' ,;
        cRetSty With &lcTmpLine..cRetSty,;
        cStyGrade With &lcTmpLine..cStyGrade,;
        cType With Iif(loFormSet.llMFCall,'M',Iif(loFormSet.lcPType $ 'LHO','N','I')) ,;
        Cuttkt With &lcTmpLine..PO,;
        cWareCode With &lcTmpLine..cWareCode,;
        Dyelot With &lcTmpLine..Dyelot,;
        FLAG With 'A',;
        LINENO  With &lcTmpLine..Lineno,;
        nLineNo With lnLineNum ,;
        Qty1 With &lcTmpLine..Qty1 ,;
        Qty2 With &lcTmpLine..Qty2 ,;
        Qty3 With &lcTmpLine..Qty3 ,;
        Qty4 With &lcTmpLine..Qty4 ,;
        Qty5 With &lcTmpLine..Qty5 ,;
        Qty6 With &lcTmpLine..Qty6 ,;
        Qty7 With &lcTmpLine..Qty7 ,;
        Qty8 With &lcTmpLine..Qty8 ,;
        REFERENCE With &lcTmpLine..Reference ,;
        STYLE With &lcTmpLine..Style ,;
        TmpRcvNum With lcBatchNo ,;
        TotQty With &lcTmpLine..TotQty ,;
        TranCd With &lcTmpLine..TranCd
      =gfAdd_Info('CTKTRCVL')
      =gfReplace("")
      Do Case
      Case TranCd  ='2'
        Replace  nTotStk With nTotStk+&lcTmpLine..TotQty  In 'CTKTRCVH'
      Case TranCd  ='4'
        Replace nTotDam With nTotDam+&lcTmpLine..TotQty In 'CTKTRCVH'
      Case TranCd  ='5'
        Replace  nTotCan With nTotCan +&lcTmpLine..TotQty In 'CTKTRCVH'
      Endcase
      lnLineNo = &lcTmpLine..Lineno
      lcPo =&lcTmpLine..PO
    Else
      Select CtKtRcvL
      lnOldQty = CtKtRcvL.TotQty
      If !Deleted(lcTmpLine) And  &lcTmpLine..TotQty > 0
        Replace Qty1 With &lcTmpLine..Qty1 ,;
          Qty2 With &lcTmpLine..Qty2 ,;
          Qty3 With &lcTmpLine..Qty3 ,;
          Qty4 With &lcTmpLine..Qty4 ,;
          Qty5 With &lcTmpLine..Qty5 ,;
          Qty6 With &lcTmpLine..Qty6 ,;
          Qty7 With &lcTmpLine..Qty7 ,;
          Qty8 With &lcTmpLine..Qty8 ,;
          TotQty With &lcTmpLine..TotQty ,;
          cWareCode With &lcTmpLine..cWareCode,;
          cRetSty With &lcTmpLine..cRetSty
        =gfAdd_Info('CTKTRCVL')
        =gfReplace("")
        Do Case
        Case TranCd  ='2'
          Replace  nTotStk With nTotStk+&lcTmpLine..TotQty-lnOldQty   In 'CTKTRCVH'
        Case TranCd  ='4'
          Replace nTotDam With nTotDam+&lcTmpLine..TotQty-lnOldQty  In 'CTKTRCVH'
        Case TranCd  ='5'
          Replace  nTotCan With nTotCan +&lcTmpLine..TotQty-lnOldQty  In 'CTKTRCVH'
        Endcase
      Else

        Do Case
        Case TranCd  ='2'
          Replace  nTotStk With nTotStk-lnOldQty   In 'CTKTRCVH'
        Case TranCd  ='4'
          Replace nTotDam With nTotDam-lnOldQty  In 'CTKTRCVH'
        Case TranCd  ='5'
          Replace  nTotCan With nTotCan-lnOldQty  In 'CTKTRCVH'
        Endcase
        =gfDelete()
      Endif
    Endif
  Endscan
  Select CTKTRCVH
  Replace cDesc   With loFormSet.ariaform1.cntBatch.txtBatchDesc.Value,;
    cStatus With loFormSet.ariaform1.cntBatch.cboBatchStatus.Value,;
    dDate   With ldDate
  =gfAdd_Info('CTKTRCVH')
  =gfReplace("")
  =gfTableUpdate()
  Select CtKtRcvL
  =gfTableUpdate()
Endif
Select  (lcTmpLine)
Set Order To (lcOldOrd)

Set Deleted &lcDelSt.
*!*************************************************************
*! Name      : lfCancelBatch
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : Cancel Batch
*!*************************************************************
Function lfCancelBatch
Lparameters loFormSet
*=lfSaveBatch(loFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF gfModalGen('INM00002B34001','ALERT',IIF(loFormSet.AriaForm1.CntBatch.cboBatchStatus.VALUE <> 'X',LANG_POSTREC_BATCH_CANCELREC,LANG_POSTREC_BATCH_UNCANCELREC))<>1
If gfModalGen('INM00002B34001','ALERT',Iif(loFormSet.ariaform1.cntBatch.cboBatchStatus.Value <> 'X',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BATCH_CANCELREC,loFormSet.GetHeaderText("LANG_POSTREC_BATCH_CANCELREC",loFormSet.HeaderAlias)),Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BATCH_UNCANCELREC,loFormSet.GetHeaderText("LANG_POSTREC_BATCH_UNCANCELREC",loFormSet.HeaderAlias))))<>1
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  Return
Endif
lcBatchNo = loFormSet.ariaform1.cntBatch.kbBatchNo.keytextbox.Value
Select CTKTRCVH
=gfSEEK(Iif(loFormSet.llMFCall,'M',Iif(loFormSet.lcPType $ 'LHO','N','I'))+lcBatchNo)
If loFormSet.ariaform1.cntBatch.cboBatchStatus.Value = 'O' Or (loFormSet.ariaform1.cntBatch.cboBatchStatus.Value = 'A' And loFormSet.llApproveBatch) && Cancel
  Replace cStatus With 'X'
  loFormSet.oToolBar.cmdadd.Enabled =.F.
  Dimension loFormSet.laStatusArr[4,2]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loFormSet.laStatusArr[1,1]  = LANG_POSTREC_STATUS_OPEN
  loFormSet.laStatusArr[1,1]  = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_STATUS_OPEN,loFormSet.GetHeaderText("LANG_POSTREC_STATUS_OPEN",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.laStatusArr[1,2]  = LANG_POSTREC_STATUS_O

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loFormSet.laStatusArr[2,1]  = LANG_POSTREC_STATUS_APPROVED
  loFormSet.laStatusArr[2,1]  = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_STATUS_APPROVED,loFormSet.GetHeaderText("LANG_POSTREC_STATUS_APPROVED",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.laStatusArr[2,2]  = LANG_POSTREC_STATUS_A

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loFormSet.laStatusArr[3,1]  = LANG_POSTREC_STATUS_CANCEL
  loFormSet.laStatusArr[3,1]  = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_STATUS_CANCEL,loFormSet.GetHeaderText("LANG_POSTREC_STATUS_CANCEL",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.laStatusArr[3,2]  = LANG_POSTREC_STATUS_X

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loFormSet.laStatusArr[4,1]  = LANG_POSTREC_STATUS_POSTED
  loFormSet.laStatusArr[4,1]  = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_STATUS_POSTED,loFormSet.GetHeaderText("LANG_POSTREC_STATUS_POSTED",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]


  loFormSet.laStatusArr[4,2]  = LANG_POSTREC_STATUS_P

  loFormSet.ariaform1.cntBatch.cboBatchStatus.Requery()
  loFormSet.ariaform1.cntBatch.cboBatchStatus.Value = 'X'

Else
  If loFormSet.ariaform1.cntBatch.cboBatchStatus.Value = 'X' && UnCancel
    Replace cStatus With 'O'
    loFormSet.oToolBar.cmdadd.Enabled =.T.
    Dimension loFormSet.laStatusArr[2,2]
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *loFormSet.laStatusArr[1,1]  = LANG_POSTREC_STATUS_OPEN
    loFormSet.laStatusArr[1,1]  = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_STATUS_OPEN,loFormSet.GetHeaderText("LANG_POSTREC_STATUS_OPEN",loFormSet.HeaderAlias))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    loFormSet.laStatusArr[1,2]  = LANG_POSTREC_STATUS_O

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *loFormSet.laStatusArr[2,1]  = LANG_POSTREC_STATUS_APPROVED
    loFormSet.laStatusArr[2,1]  = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_STATUS_APPROVED,loFormSet.GetHeaderText("LANG_POSTREC_STATUS_APPROVED",loFormSet.HeaderAlias))
    *N000682,1 11/20/2012 MMT Globlization changes[End]


    loFormSet.laStatusArr[2,2]  = LANG_POSTREC_STATUS_A

    loFormSet.ariaform1.cntBatch.cboBatchStatus.Requery()
    loFormSet.ariaform1.cntBatch.cboBatchStatus.Value = 'O'
  Endif
Endif



=gfReplace('')
=gfTableUpdate()
loFormSet.ariaform1.cntBatch.cboBatchStatus.Value = CTKTRCVH.cStatus

=lfShowScr(loFormSet)
loFormSet.Refreshall()
*!*************************************************************
*! Name      : lfCheckBatch
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : Check Batch Status
*!*************************************************************
Function lfCheckBatch
Lparameters loFormSet
If loFormSet.ariaform1.cntBatch.cboBatchStatus.Value =  'X'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *= gfModalGen('TRM34070B42000','DIALOG',LANG_POSTREC_ISCANCELLED)
  = gfModalGen('TRM34070B42000','DIALOG',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ISCANCELLED,loFormSet.GetHeaderText("LANG_POSTREC_ISCANCELLED",loFormSet.HeaderAlias)))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  Return .F.
Endif
If loFormSet.ariaform1.cntBatch.cboBatchStatus.Value = 'P'
  *--This temporary receiving batch is posted. Cannot proceed.
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *= gfModalGen('TRM34070B42000','DIALOG',LANG_POSTREC_ISPOSTED)
  = gfModalGen('TRM34070B42000','DIALOG',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ISPOSTED,loFormSet.GetHeaderText("LANG_POSTREC_ISPOSTED",loFormSet.HeaderAlias)))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  Return .F.
Endif
If loFormSet.ariaform1.cntBatch.cboBatchStatus.Value <>  'A'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *= gfModalGen('TRM34070B42000','DIALOG',LANG_POSTREC_ISNOTAPPROVED)
  = gfModalGen('TRM34070B42000','DIALOG',Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_ISNOTAPPROVED,loFormSet.GetHeaderText("LANG_POSTREC_ISNOTAPPROVED",loFormSet.HeaderAlias)))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  Return .F.
Endif
*!*************************************************************
*! Name      : lfUpdBatchSt
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : Update Batch Status
*!*************************************************************
Function lfUpdBatchSt
Lparameters loFormSet
Select CTKTRCVH
lcBatchNo = loFormSet.ariaform1.cntBatch.kbBatchNo.keytextbox.Value
=gfSEEK(Iif(loFormSet.llMFCall,'M',Iif(loFormSet.lcPType $ 'LHO','N','I'))+lcBatchNo)
Replace cStatus With 'P'
=gfReplace('')
=gfTableUpdate()
*!*************************************************************
*! Name      : lfShowErrorLog
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : Show Error Log
*!*************************************************************
Function lfShowErrorLog
Select TMPSTR
*! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome + 'POBTLOG.SCX')
=gfCallForm('POBTLOG')
*! E303029,1 MMT 01/02/2012 correct the calling of non-major programs within the SaaS environemnt[End]
Use In TMPSTR
*!*************************************************************
*! Name      : lfUpdScanBatch
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : To Update Imported Scan Batch
*!*************************************************************
Function lfUpdScanBatch
Lparameters loFormSet

If !Used('POSHDR')
  =gfOpenTable('POSHDR','POSHDR')
Endif
*! B609728,1 MMT 11/14/2011 Fix bug of Freezing in Scan barcode screen after using Scanned batch in PO Screen[Start]
Local lnConnHandler,lnUpSt
lnConnHandler = 0
lnUpSt = 0
*! B609728,1 MMT 11/14/2011 Fix bug of Freezing in Scan barcode screen after using Scanned batch in PO Screen[End]
For lnT = 1 To Alen(loFormSet.laUsedBatchPO,1)
  *! B610904,1 MMT 11/06/2014 Error in PO receiving screen while using scanned batch option[T20141029.0023][Start]
  lnHeaderKey= loFormSet.laUsedBatchPO[lnT ,1]
  *! B610904,1 MMT 11/06/2014 Error in PO receiving screen while using scanned batch option[T20141029.0023][End]
  =gfSEEK(loFormSet.lcBusDoc+loFormSet.lcWorkOrd+loFormSet.laUsedBatchPO[lnT ,2],'POSHDR')
  *! B610904,1 MMT 11/06/2014 Error in PO receiving screen while using scanned batch option[T20141029.0023][Start]
  *!*	  lcUpDStat = "Update SCAN_BATCH_HEADER_T Set TRANSACTION_TYPE ='"+;
  *!*	    loFormSet.lcBusDoc+loFormSet.lcWorkOrd +"',TRANSACTION_NO = '"+loFormSet.laUsedBatchPO[lnT ,2]+;
  *!*	    "',VENDOR = '"+poshdr.VENDOR+"',Status ='C' Where SCAN_BATCH_HEADER_KEY = '"+loFormSet.laUsedBatchPO[lnT ,1]+"'"
  lcUpDStat = "Update SCAN_BATCH_HEADER_T Set TRANSACTION_TYPE ='"+;
    loFormSet.lcBusDoc+loFormSet.lcWorkOrd +"',TRANSACTION_NO = '"+loFormSet.laUsedBatchPO[lnT ,2]+;
    "',VENDOR = '"+poshdr.VENDOR+"',Status ='C' Where SCAN_BATCH_HEADER_KEY = ?m.lnHeaderKey"
  *! B610904,1 MMT 11/06/2014 Error in PO receiving screen while using scanned batch option[T20141029.0023][End]
  *! B609728,1 MMT 11/14/2011 Fix bug of Freezing in Scan barcode screen after using Scanned batch in PO Screen[Start]
  *!*	  lnUpSt = oAriaApplication.RemoteCompanyData.execute(lcUpDStat ,'',;
  *!*	                                                      "SCAN_BATCH_HEADER_T ","",oAriaApplication.activecompanyconstr ,3,"",SET("Datasession"))
  lnUpSt = oAriaApplication.RemoteCompanyData.Execute(lcUpDStat ,'',;
    "SCAN_BATCH_HEADER_T ","",oAriaApplication.ActiveCompanyConStr ,3,"",Set("Datasession"),.T., @lnConnHandler)
  If lnUpSt <= 0
    Exit
  Endif
  *! B609728,1 MMT 11/14/2011 Fix bug of Freezing in Scan barcode screen after using Scanned batch in PO Screen[End]
Endfor
*! B609728,1 MMT 11/14/2011 Fix bug of Freezing in Scan barcode screen after using Scanned batch in PO Screen[Start]
If lnUpSt > 0
  Sqlcommit(lnConnHandler)
Else
  =oAriaApplication.RemoteCompanyData.CheckRetResult("Execute",lnResult,.T.)
  Sqlrollback(lnConnHandler)
Endif
*! B609728,1 MMT 11/14/2011 Fix bug of Freezing in Scan barcode screen after using Scanned batch in PO Screen[End]
lcBatDet = loFormSet.lcbatchdet
If Used(lcBatDet)
  *!*  B609728,1 MMT 11/14/2011 Fix bug of Freezing in Scan barcode screen after using Scanned batch in PO Screen[Start]
  lnConnHandler = 0
  lnUpSt = 0
  Select (lcBatDet)
  Locate For !Deleted()
  If Eof()
    Return
  Endif
  Locate
  *!*  B609728,1 MMT 11/14/2011 Fix bug of Freezing in Scan barcode screen after using Scanned batch in PO Screen[End]
  Select (lcBatDet)
  Scan For !Empty(Alltrim(REJECTION_REASON)) And !Deleted()
    *! B610904,1 MMT 11/06/2014 Error in PO receiving screen while using scanned batch option[T20141029.0023][Start]
    *lcUpDDet =  "Update SCAN_BATCH_DETAILS_T Set Status ='R',Rejection_reason = '"+;
    &lcBatDet..Rejection_reason +"' Where SCAN_BATCH_HEADER_KEY = '"+;
    &lcBatDet..SCAN_BATCH_HEADER_KEY+"' AND SCAN_BATCH_DETAILS_KEY = '"+&lcBatDet..SCAN_BATCH_DETAILS_KEY+"'"
    lcDetailItemNum= &lcBatDet..ITEM_NUMBER
    lcDetailPAckNum= &lcBatDet..PACK_NUMBER
    lnHeaderKey = &lcBatDet..SCAN_BATCH_HEADER_KEY
    lcUpDDet =  "Update SCAN_BATCH_DETAILS_T Set Status ='R',Rejection_reason = '"+;
      &lcBatDet..REJECTION_REASON +"' Where SCAN_BATCH_HEADER_KEY = ?m.lnHeaderKey "+;
      " AND ITEM_NUMBER = ?m.lcDetailItemNum AND PACK_NUMBER=?m.lcDetailPAckNum"
    *! B610904,1 MMT 11/06/2014 Error in PO receiving screen while using scanned batch option[T20141029.0023][End]
    *! B609728,1 MMT 11/14/2011 Fix bug of Freezing in Scan barcode screen after using Scanned batch in PO Screen[Start]
    *!*	    lnUpSt = oAriaApplication.RemoteCompanyData.execute(lcUpDDet ,'',;
    *!*	                                                          "SCAN_BATCH_DETAILS_TMP","",oAriaApplication.activecompanyconstr ,3,"",SET("Datasession"))
    lnUpSt = oAriaApplication.RemoteCompanyData.Execute(lcUpDDet ,'',;
      "SCAN_BATCH_DETAILS_TMP","",oAriaApplication.ActiveCompanyConStr ,3,"",Set("Datasession"),.T., @lnConnHandler)
    If lnUpSt <= 0
      Exit
    Endif
    *! B609728,1 MMT 11/14/2011 Fix bug of Freezing in Scan barcode screen after using Scanned batch in PO Screen[End]
  Endscan
  *!*  B609728,1 MMT 11/14/2011 Fix bug of Freezing in Scan barcode screen after using Scanned batch in PO Screen[Start]
  If lnUpSt > 0
    Sqlcommit(lnConnHandler)
  Else
    =oAriaApplication.RemoteCompanyData.CheckRetResult("Execute",lnResult,.T.)
    Sqlrollback(lnConnHandler)
  Endif
  *!*  B609728,1 MMT 11/14/2011 Fix bug of Freezing in Scan barcode screen after using Scanned batch in PO Screen[END]
Endif
*!*************************************************************
*! Name      : lfRefshAll
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : Refresh screen
*!*************************************************************
Function lfRefshAll
Lparameters loFormSet
If loFormSet.lcPType $'IOM'
  lnButtonNo = 0
  For lnCount = 1 To loFormSet.oToolBar.ControlCount
    If Upper(loFormSet.oToolBar.Controls(lnCount).Class) = 'TOOLBARCUSTOMBUTTON' And ;
        UPPER(loFormSet.oToolBar.Controls(lnCount).CustomName) = Upper('cmdDelBatch')
      lnButtonNo  = lnCount
      Exit
    Endif
  Endfor


  If !Empty(loFormSet.ariaform1.cntBatch.kbBatchNo.keytextbox.Value) And !(loFormSet.ariaform1.cntBatch.cboBatchStatus.Value $ 'P') And;
      (loFormSet.ariaform1.cntBatch.cboBatchStatus.Value <> 'P' And  Iif(loFormSet.ariaform1.cntBatch.cboBatchStatus.Value = 'A',loFormSet.llApproveBatch,.T.))
    loFormSet.oToolBar.ChangeButtonStatus('cmdDelBatch','ENABLED')
    If loFormSet.ariaform1.cntBatch.cboBatchStatus.Value = 'X'
      loFormSet.oToolBar.Controls(lnButtonNo).Picture = oAriaApplication.BitmapHome+"UNTRASH.BMP"
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *loFormSet.oToolBar.CONTROLS(lnButtonNo).STATUSBARTEXT = LANG_POSTREC_BATCH_UNCANCELBATCH
      loFormSet.oToolBar.Controls(lnButtonNo).StatusBarText = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BATCH_UNCANCELBATCH,loFormSet.GetHeaderText("LANG_POSTREC_BATCH_UNCANCELBATCH",loFormSet.HeaderAlias))
      *N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *loFormSet.oToolBar.CONTROLS(lnButtonNo).TOOLTIPTEXT = LANG_POSTREC_BATCH_UNCANCELBATCH
      loFormSet.oToolBar.Controls(lnButtonNo).ToolTipText = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BATCH_UNCANCELBATCH,loFormSet.GetHeaderText("LANG_POSTREC_BATCH_UNCANCELBATCH",loFormSet.HeaderAlias))
      *N000682,1 11/20/2012 MMT Globlization changes[End]

    Else
      loFormSet.oToolBar.Controls(lnButtonNo).Picture = oAriaApplication.BitmapHome+"TRASH.BMP"
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *loFormSet.oToolBar.CONTROLS(lnButtonNo).STATUSBARTEXT = LANG_POSTREC_BATCH_CANCBATCH
      loFormSet.oToolBar.Controls(lnButtonNo).StatusBarText = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BATCH_CANCBATCH,loFormSet.GetHeaderText("LANG_POSTREC_BATCH_CANCBATCH",loFormSet.HeaderAlias))
      *N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *loFormSet.oToolBar.CONTROLS(lnButtonNo).TOOLTIPTEXT = LANG_POSTREC_BATCH_CANCBATCH
      loFormSet.oToolBar.Controls(lnButtonNo).ToolTipText = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_POSTREC_BATCH_CANCBATCH,loFormSet.GetHeaderText("LANG_POSTREC_BATCH_CANCBATCH",loFormSet.HeaderAlias))
      *N000682,1 11/20/2012 MMT Globlization changes[End]

    Endif
  Else
    loFormSet.oToolBar.ChangeButtonStatus('cmdDelBatch','DISABLED')
  Endif
Endif
*! E302963,1 MMT 09/07/2011 Modify the PO Receiving to Import Scanned Batch and create Temp. rec. Batch[END]
*!*************************************************************
*! Name      : lfUpdbatch
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 09/11/2004
*! Purpose   : To do all the necessary actions to start using the form
*!*************************************************************
*! Parameters: loFormSet : ThisFormSet
*!*************************************************************
*! Called    : From Init in the formset
*!*************************************************************
Function lfUpdbatch
Lparameters lpcShipmentNo

Set Step On
lcAliasSel = Alias()

If !Used('SYCCOMP')
  Use (oAriaApplication.SysPath+'SYCCOMP') In 0 Order Tag ccomp_id
Endif
=Seek(oAriaApplication.ActiveCompanyID,'SYCCOMP')
lcServer  = Alltrim(SYCCOMP.cConserver)
lcdb  = Alltrim(SYCCOMP.ccondbname)
lcUsrNam  =  Alltrim(SYCCOMP.cConUserID)
lcPasword =  Alltrim(SYCCOMP.cConPasWrd)

lnConnHandler = Sqlstringconnect( "Driver={SQL Server};server="+Alltrim(lcServer)+";DATABASE="+Alltrim(lcdb)+;
  ";uid="+Alltrim(lcUsrNam  )+";pwd="+Alltrim(lcPasword ))

lnRes =SQLExec(lnConnHandler,"USE ["+lcdb+"]" )

*! ShippingOrderHeader_T ***********************************************************************************
lcInsertString = ""
lcInsertString = "update  [ctktrcvh] set "+;
  "          [cstatus] ='"+'P'+"' where Shipno = '"+lpcShipmentNo+"'"


lnRes = SQLExec(lnConnHandler,lcInsertString )



If !Empty(Alltrim(lcAliasSel))
  Select &lcAliasSel.
Endif

Endfunc
*!*************************************************************
*B611953,1 ES 10/29/2019 Shipment is not marked as locked once user selected it in PO receiving screen [Start]
FUNCTION lfUndo
LPARAMETERS loFormSet
IF loFormSet.lcPType $ 'SUFC' 
  loFormSet.recordLock(.F.)
ENDIF
*B611953,1 ES 10/29/2019 Shipment is not marked as locked once user selected it in PO receiving screen [End]



