*!***************************************************************************************************************************
*: Program file  : ALAUTAL.PRG
*: Program desc. : Automatic Allocation  (N120718)
*: Module        : Allocation
*: System        : Aria Apparel System
*: Developer     : Hend Ghanem (HBG)
*!***************************************************************************************************************************
*: Passed Parameters  : None
*!***************************************************************************************************************************
*: Modifications      :
*:   B607674,1 HBG 07/26/2005 Fix bug of too many urgument in case of InList
*:   B607789,1 WAM 09/28/2006 Fix bu when filter to some seasons or divisions
*:   B607815,1 WAM 10/31/2006 Cannot amend allocation min % if Foce allocation setup is set to No
*:   B608118,1 WAM 06/10/2007 Increase he width of the available fields
*:   B608316,1 MMT 10/11/2007 convert 3PL Provider Enhancemnt from 27[T20071010.0009]
*:   C200876,1 TMI 05/01/2008 Adding the BIN location triggers [T20060818.0001]
*:   B608600,1 MMT 07/01/2008 Move Ordered Qty to the new warehouse when user change it[T20080305.0010]
*:   B608997,1 AHS 09/17/2009 Adding new variable to hold the title of POs and cuttkts when Exclude = YES on OG [T20081115.0001]
*:   B609356,1 SMA 07/25/2010 Fix bug of creating empty *.cdx files [T20091027.0093]
*:   B609534,1 MMT 02/22/2011 When tryin to use the automatic allocation order screen it freezes[T20110121.0002]
*:   C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003]
*:   B609735,1 MMT 11/20/2011 Allocation screen has fields for Open Amnt and Pik Amnt but no data appears[T20110926.0030]
*:   B609785,1 MMT 12/26/2011 Creating PIKTKT does not fire an event in request builder[T20111207.0031]
*:   B610039,1 MMT 08/08/2012 Automatic Allocation screen gives error when user select more than 25 codes from OG[T20120718.0001]
*:   B610272,1 HIA 03/18/2013 Aria XP - DCC R12 slowing the system [T20130111.0006]
*:   B610291,1 HIA 04/04/2013 Aria XP - Aria4xp - Master Data - Budgets, T20130111.0006
*:   B610517,1 MMT 09/16/2013 Fix bug of empty scale field in ordline after allocation using automatic allocation screen[T20130828.0013]
*:   B610516,1 HIA 09/15/13 T20130910.0026 - Automatic Allocation - Available column is not shown.
*:   B610534,1 MMT 09/29/2013 Modify Automatic allocation screen to include WIP in Avail. based on setup{T20130910.0026}
*:   B610573,1 MMT 11/04/2013 Automatic Allocation calculated total picked Qty incorrectly[T20131031.0007]
*:   B610579,1 MMT 11/10/2013 Automtaic allocation hanges when first sort by Start Date[T20131024.0020]
*:   B610670,1 HIA 02/04/2014 T20140123.0021 - DCC - duplicate Pick Tickets with wrong or no qtys.
*:B610697,1 TMI 03/12/2014 fix a problem of auto allocation over allocating orders [T20140227.0014 ] 
*:B610697,3 TMI 03/16/2014  modify the changes done in Auto. allocation program for this ticket to update the update the Qty fields in (loFormSet.lcTmpOrdLn) from Ordline_Picked to use the minimum between the current qty in (loFormSet.lcTmpOrdLn) and qty in Ordline_Picked [T20140227.0014 ] 
*B610705,1 TMI 03/25/2014 1. update totqty same as qty fields 
*B610705,1 TMI            2. add the code of B610697 & B610670 to lfGenScr [T20140227.0014]
*B610884,1 MMT 10/16/2014 Add "Add user" field to order browser in Auto. Allocation screen[T20141006.0003]
*B610886,1 MMT 10/19/2014 Automatic allocation screen order browser has no index[T20141016.0005]
*E303530,1 MMT 11/30/2014 Add Triggers for [T20141118.0011]
*B610945,1 MMT 02/08/2015 Automatic allocation doubles the picked Qty[T20150205.0025]
*B610997,1 MMT 04/28/2015 Error in Automatic allocation screen at DCC[T20150424.0001]
*E303612,1 MMT 10/25/2015 Add trigger to Automatic allocation to check pickpack table[T20150908.0008]
*B611569,1 MMT 05/07/2018 Automatic Allocation - Allocation logic grid does not save filter correctly[T20180430.0035]
*B611762,1 DM 10/04/2019 T20181017.0004 Modify lfEditLine Function - Take Max Qty1 in order not to be greater than the Ordered qty   -
*B611762,1 ES 05/29/2019 - Style wrong ordered quantity in the Rebalance program - [T20181017.0004]
*E612337,1 MMT 02/10/2021 Automatic Allocation does not update Entity table when picking ticket is created/released [T20201008.0003]
*B612690,1 MMT 09/11/2023 Automatic Allocation corrupts the Order Qty of the Styles when the user changes the Pick From warehouse[T-ERP-20230907.0001]
*!***************************************************************************************************************************

#INCLUDE R:\Aria4XP\Screens\AL\ALAUTAL.h

*-- Declear Variavles
PRIVATE lnPanArrLn,lcOldScMod

STORE ' ' TO lcOldScMod

** lnPanArrLn   Pannel Array length.
lnPanArrLn = 4

DO FORM oAriaApplication.ScreenHome+oAriaApplication.ActiveModuleID+ "\ALAUTAL.SCX"

*!*************************************************************
*! Name      : lfInit
*! Developer : Hend Ghanem
*! Date      : 03/03/2004
*! Purpose   : Init Function of the screen
*!*************************************************************
*!
FUNCTION lfInit
PARAMETERS loFormSet

PRIVATE llRetVal
loFormSet.llReject = .F.

loFormSet.oAlObj = CREATEOBJECT("AL")

*--Get needed setups
DECLARE laSetups[8,2]
laSetups[1,1] = 'M_DYELOT'
laSetups[2,1] = 'M_MATDYE'
laSetups[3,1] = 'M_WareHouse'
laSetups[4,1] = 'M_FORCEALO'
laSetups[5,1] = 'M_TOTAVLBL'
laSetups[6,1] = 'M_CMPDOLN'
laSetups[7,1] = 'M_CHKAPROV'
laSetups[8,1] = 'M_STYCNFG'
=gfGetMemVar(@laSetups,oAriaApplication.ActiveCompanyID)

** laSizes      Array to hold the Titles for the sizes columns
DIMENSION loFormSet.laSizes[8]
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  loFormSet.laSizes[lnI] = 'Size'+lcI
ENDFOR

** llUseDyes    Flag to know if the system use Dyelots
** llFabDye     .T. if material use dyelots.
** llMultWare   Flag to know if the system is multi warehouse
** llChkAprov   Approve Amount Setup
** llUseConfg   Flag to know if the style use configuration
loFormSet.llUseDyes  = (UPPER(ALLTRIM(laSetups[1,2])) = 'Y')
loFormSet.llFabDye   = ('MA' $ oAriaApplication.CompanyInstalledModules) AND (UPPER(ALLTRIM(laSetups[2,2])) = 'Y')
loFormSet.llMultWare = (UPPER(ALLTRIM(laSetups[3,2])) = 'Y')
loFormSet.llTotAvlbl = laSetups[5,2]
loFormSet.llLinCmplt = laSetups[6,2]
loFormSet.llChkAprov = laSetups[7,2]
loFormSet.llAlwForce = lfSuppForc(ALLTRIM(laSetups[4,2]))
loFormSet.llUseConfg = (UPPER(ALLTRIM(laSetups[8,2])) = 'Y')

** lnSelRec     Variable to hold the number of selected records
** lnSelAlo     Variable to hold the number of selected and allocated records
** lnAloRec     Variable to hold the number of allocated records
** lnChangAlo   Variable to hold the number of the changed allocated quantity in the current record
** lnDellRec    Variable to hold the number of deleted records in the Temp. Order Lines file
** lnBrRecNo    Variable to hold the Browse Record number
** lnRecNumbr   Variable to hold the number of the undeleted records in the Temp. Order Lines file
STORE 0 TO loFormSet.lnSelRec,loFormSet.lnSelAlo,loFormSet.lnAloRec,loFormSet.lnchangalo,;
  loFormSet.lnDellRec,loFormSet.lnBrRecNo,loFormSet.lnRecNumbr

IF loFormSet.llChkAprov
  loFormSet.lcTmpOrdAp = gfTempName()
ENDIF

** llOpnPikLn   .T. if we open Pick Line file in this program.
** llOpnPack    .T. if we open pack header file in this program.
** llCh3Stat    Variable to hold the status of the 3rd. child screen in the screen set
** llDyelotSt   Variable to hold the status of the Dyelot get field
** llIncOrd     Flag to know if the User has increased any of the Ordered quantities
** llSelAllSt   Variable to hold the Select All button status
** llSelNonSt   Variable to hold the Select None button status
** llStartSlc   Only .T. if user start from selection grid.
** llPartAlo    .T. if any of selected records have partial allocation.
** llStylRel    Flag to know if we need to establish a relationship between the ORDLINE file and the STYLE file
** llOrdrRel    Flag to know if we need to establish a relationship between the ORDLINE file and the ORDHDR file
** llMustLoop   To loop avoiding nesting error.
STORE .F. TO loFormSet.llforceall,loFormSet.llCh3Stat ,loFormSet.llDyelotSt,loFormSet.llIncOrd  ,loFormSet.llSelAllSt,;
  loFormSet.llSelNonSt,loFormSet.llSelNonSt,loFormSet.llPartAlo ,loFormSet.llStartSlc,loFormSet.llStylRel ,;
  loFormSet.llOrdrRel ,loFormSet.llMustLoop,loFormSet.llOpnPack ,loFormSet.llOpnPikLn,loFormSet.llCalWip

** lcIndexExp   Variable to hold the Index expression to sort the Temp. Order Lines file with
** lcOldIndex   Variable hold old index value.
** laShpExp     Array to hold selected shipments.
DIMENSION loFormSet.laShpExp[1]
STORE " " TO loFormSet.laFiltExp,loFormSet.lcOptmFile,loFormSet.lcIndexExp,loFormSet.lcOldIndex,loFormSet.laShpExp,loFormSet.lcShpCond,;
  loFormSet.laString1,loFormSet.laString2

** llCtrStat1	Flag to handle enabling and disabling of Top field
** llCtrStat2	Flag to handle enabling and disabling of End field
** llCtrStat3	Flag to handle enabling and disabling of Next field
** llCtrStat4	Flag to handle enabling and disabling of Previuos field
** llCtrStat6	Flag to handle enabling and disabling of print field
** llCtrStat7	Flag to handle enabling and disabling of Edit field
** llCtrStat8	Flag to handle enabling and disabling of Delete field
** llCtrStat9	Flag to handle enabling and disabling of Select field
** llCtrStat1	Flag to handle enabling and disabling of browse field
STORE .F. TO loFormSet.llCtrStat1,loFormSet.llCtrStat2,loFormSet.llCtrStat3,loFormSet.llCtrStat4,;
  loFormSet.llCtrStat6,loFormSet.llCtrStat7,loFormSet.llCtrStat8,loFormSet.llCtrStat9,;
  loFormSet.llCtrStat10

loFormSet.lcTmpPkTk  = gfTempName()  && New Session pick ticket file.
loFormSet.lcRelLine  = gfTempName()
loFormSet.lcTmpRelPk = gfTempName()  && Release File and Index.

** lcStyleTtl   Variable to hold the Style field Title
** lcStylePct   Variable to hold the Style field Mask
** lnStyleWid   Variable to hold the width of the Style field
** lcStyMajor   Variable to hold Style major title   ...
** lcMajorPic   Variable to hold style major picture ...
** lnMajorLen   Variable to Hold Major Length.
** lcSeason     Variable Hold Style season title.
** lcDivision   Variable Hold Style division title.
** lcStyGroup   Variable Hold Style group title.
** lcFabTlt     Variable Hold Style Fabric title.
** lcPatTlt     Variable Hold Style pattern title.
** lnMajSeg     Number of major segments.
** lcColorTlt   Variable to hold Color segment title.
** lcNonMajTl   Variable to hold Non Major title .
** lcNonMajPi   Variable to hold Non major picture.
** lcFree_Clr   Variable to hold 'C' for Color seg. and 'F' for Free seg. ..
** lnNonMajSt   Non major (Color/Free) start filter position.
** lnSupMajSt

*-- Get Item Information
oGetItemMask = CREATEOBJECT('GetItemMask')
loFormSet.lcStyleTtl = oGetItemMask.DO("HI")
loFormSet.lcStylePct = oGetItemMask.DO("PI")
loFormSet.lnStyleWid = LEN(loFormSet.lcStylePct)
loFormSet.lcStyMajor = oGetItemMask.DO('HM')
loFormSet.lcMajorPic = oGetItemMask.DO('PM')
loFormSet.lnMajorLen = LEN(loFormSet.lcMajorPic)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.lcSeason   = loFormSet.lcStyMajor + LANG_AutoAlloc_Season
loFormSet.lcSeason   = loFormSet.lcStyMajor + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_Season,loFormSet.GetHeaderText("LANG_AutoAlloc_Season",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.lcDivision = loFormSet.lcStyMajor + LANG_AutoAlloc_Division
loFormSet.lcDivision = loFormSet.lcStyMajor + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_Division,loFormSet.GetHeaderText("LANG_AutoAlloc_Division",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.lcStyGroup = loFormSet.lcStyMajor + LANG_AutoAlloc_Group
loFormSet.lcStyGroup = loFormSet.lcStyMajor + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_Group,loFormSet.GetHeaderText("LANG_AutoAlloc_Group",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.lcFabTlt   = loFormSet.lcStyMajor + LANG_AutoAlloc_FabricCode
loFormSet.lcFabTlt   = loFormSet.lcStyMajor + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_FabricCode,loFormSet.GetHeaderText("LANG_AutoAlloc_FabricCode",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.lcPatTlt   = loFormSet.lcStyMajor + LANG_AutoAlloc_Pattern
loFormSet.lcPatTlt   = loFormSet.lcStyMajor + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_Pattern,loFormSet.GetHeaderText("LANG_AutoAlloc_Pattern",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

lnMajSeg   = oGetItemMask.DO('SM')  && No. of major segments.
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
oGetItemMask.DO(@laMajSegs)
STORE 0  TO loFormSet.lnNonMajSt
STORE "" TO loFormSet.lcNonMajPi,loFormSet.lcNonMajTl
STORE .T. TO loFormSet.llCallScop , loFormSet.llFirstRun
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    loFormSet.lcFree_Clr = laMajSegs[lnI,1]
    loFormSet.lnNonMajSt = IIF(loFormSet.lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],loFormSet.lnNonMajSt)      && This item hold seg. start position.
    loFormSet.lnSupMajSt = loFormSet.lnNonMajSt
    loFormSet.lcNonMajPi = IIF(EMPTY(loFormSet.lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
      laMajSegs[lnI,3],;
      loFormSet.lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    loFormSet.lcNonMajTl = IIF(EMPTY(loFormSet.lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
      PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
      loFormSet.lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(loFormSet.lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(loFormSet.lcNonMajPi) TO loFormSet.lnFreeLen , loFormSet.lnColorLen
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.lcColorTlt = LANG_AUTOALLOC_COLROTITL1 + ALLTRIM(loFormSet.lcNonMajTl) + LANG_AUTOALLOC_COLROTITL2
loFormSet.lcColorTlt = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_COLROTITL1,loFormSet.GetHeaderText("LANG_AUTOALLOC_COLROTITL1",loFormSet.HeaderAlias)) + ALLTRIM(loFormSet.lcNonMajTl) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_COLROTITL2,loFormSet.GetHeaderText("LANG_AUTOALLOC_COLROTITL2",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]


*-- Fabric Major length
lcFabMajor  = oGetItemMask.DO('PM','','0002')
loFormSet.lnFabMajor   = LEN(lcFabMajor)

DIMENSION laFabSegs[1,1]
oGetItemMask.DO(@laFabSegs,'','0002')
lnFabMajSeg  = oGetItemMask.DO('SM','','0002')  && No. of major segments.
*-- Fabric Non major length
loFormSet.lnFabNonMaj  = LEN(laFabSegs[lnFabMajSeg+1,3]  )
*-- Fabric seperator
loFormSet.lcFabSep     = laFabSegs[lnFabMajSeg,6]


** llPikNow     This flage to detrimined if this program run into this customer.
loFormSet.llPikNow = .T.

** llForceAlc   Flag to know if we are going to Force the allocation

IF !(loFormSet.llPikNow)
  lnPanArrLn = lnPanArrLn -1
ENDIF


STORE '' TO loFormSet.laIncExprs , loFormSet.laExcExprs

** laWip        Array save wip values and used if user want to include Work Process.
** laIndexExp   Array to hold the Valid fields to Sort the Temp. Order Lines file with
DIMENSION loFormSet.laScopExpr[1,2] , loFormSet.laNormExpr[1,2] , loFormSet.laWIP[9] , loFormSet.laIndexExp[6]

STORE 0 TO loFormSet.laWIP[9]

** laSortAry    Variable to hold user select sorted.
DIMENSION loFormSet.laSortAry[4,2]
loFormSet.laSortAry = ''
loFormSet.laSortAry[1,1] = 1
loFormSet.laSortAry[1,2] = [DTOS(COMPLETE)]
loFormSet.laSortAry[2,1] = 2
loFormSet.laSortAry[2,2] = [PRIORITY]
loFormSet.laSortAry[3,1] = 3
loFormSet.laSortAry[3,2] = [DTOS(START)]
loFormSet.laSortAry[4,1] = 4
loFormSet.laSortAry[4,2] = [ORDER]

loFormSet.laIndexExp[1] = 'DTOS(COMPLETE)'
loFormSet.laIndexExp[2] = 'PRIORITY'
loFormSet.laIndexExp[3] = 'DTOS(START)'
loFormSet.laIndexExp[4] = 'ORDER'
loFormSet.laIndexExp[5] = 'ACCOUNT'
loFormSet.laIndexExp[6] = ''


** laSlctDesc   Variable to hold Select By array data.
** laSlctVals   Variable to hold Select By array Values.
lnSubtract = 0
lnSubtract = IIF('MF' $ oAriaApplication.CompanyInstalledModules,lnSubtract, lnSubtract + 1)
lnSubtract = IIF('PO' $ oAriaApplication.CompanyInstalledModules,lnSubtract, lnSubtract + 1)

DIMENSION loFormSet.laSlctDesc[6 - lnSubtract,1], loFormSet.laSlctVals[6 - lnSubtract,1]
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*loFormSet.laSlctDesc[1,1] = 'All'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laSlctDesc[1,1] = LANG_AutoAlloc_ALL
loFormSet.laSlctDesc[1,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_ALL,loFormSet.GetHeaderText("LANG_AutoAlloc_ALL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/22/2012 Globalization changes[eND]
loFormSet.laSlctDesc[2,1] = loFormSet.lcStyMajor
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	loFormSet.laSlctDesc[3,1] = 'Order'
*!*	loFormSet.laSlctDesc[4,1] = 'Account'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laSlctDesc[3,1] = LANG_AutoAlloc_ORDER
loFormSet.laSlctDesc[3,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_ORDER,loFormSet.GetHeaderText("LANG_AutoAlloc_ORDER",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laSlctDesc[4,1] = LANG_AutoAlloc_ACCOUNT
loFormSet.laSlctDesc[4,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_ACCOUNT,loFormSet.GetHeaderText("LANG_AutoAlloc_ACCOUNT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/22/2012 Globalization changes[END]
loFormSet.laSlctVals[1,1] = " "
loFormSet.laSlctVals[2,1] = "S"
loFormSet.laSlctVals[3,1] = "O"
loFormSet.laSlctVals[4,1] = "A"

DO CASE
CASE lnSubtract = 0
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  *!*	    loFormSet.laSlctDesc[5,1] = 'Cutting ticket'
  *!*	    loFormSet.laSlctDesc[6,1] = 'Purchase order'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loFormSet.laSlctDesc[5,1] = LANG_AutoAlloc_CUTTINGTICKET
  loFormSet.laSlctDesc[5,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_CUTTINGTICKET,loFormSet.GetHeaderText("LANG_AutoAlloc_CUTTINGTICKET",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loFormSet.laSlctDesc[6,1] = LANG_AutoAlloc_PURCHASEORDER
  loFormSet.laSlctDesc[6,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_PURCHASEORDER,loFormSet.GetHeaderText("LANG_AutoAlloc_PURCHASEORDER",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[END]
  loFormSet.laSlctVals[5,1] = 'K'
  loFormSet.laSlctVals[6,1] = 'P'
CASE lnSubtract = 1
  IF 'MF' $ oAriaApplication.CompanyInstalledModules
    *N000682,1 MMT 11/22/2012 Globalization changes[Start]
    *loFormSet.laSlctDesc[5,1] = 'Cutting ticket'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *loFormSet.laSlctDesc[5,1] = LANG_AutoAlloc_CUTTINGTICKET
    loFormSet.laSlctDesc[5,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_CUTTINGTICKET,loFormSet.GetHeaderText("LANG_AutoAlloc_CUTTINGTICKET",loFormSet.HeaderAlias))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/22/2012 Globalization changes[Start]
    loFormSet.laSlctVals[5,1] = 'K'
  ELSE
    *N000682,1 MMT 11/22/2012 Globalization changes[Start]
    *loFormSet.laSlctDesc[5,1] = 'Purchase order'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *loFormSet.laSlctDesc[5,1] = LANG_AutoAlloc_PURCHASEORDER
    loFormSet.laSlctDesc[5,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_PURCHASEORDER,loFormSet.GetHeaderText("LANG_AutoAlloc_PURCHASEORDER",loFormSet.HeaderAlias))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 MMT 11/22/2012 Globalization changes[END]
    loFormSet.laSlctVals[5,1] = 'P'
  ENDIF
ENDCASE


DECLARE loFormSet.laFileStru[1]
DIMENSION loFormSet.lapikst[8]
STORE .F. TO loFormSet.lapikst

** llRpExlDye   Flag to know if the User want to Exclude Styles that is Dyelot yes
** llRpExlBlk   Flag to know if the User want to Exclude the Bulk orders
** llRpPikSep   Flag to know if the User want to allocate the Order lines records that dose not have a group
** llRpPikCor   Flag to know if the User want to allocate the Order lines records that have a group
** llRpGdExcl  : Flag is .T. when user press < Exclude > button is selection grid.
STORE .T. TO loFormSet.llRpExlDye,loFormSet.llRpExlBlk,loFormSet.llRpPikSep,loFormSet.llRpPikCor,loFormSet.llRpGdExcl

** llRpIncHOr   Flag to know if we are going to include the orders in hold
** llRpAlocat   Flag to know if the User want allocated lines [From the Option grid]
** llRpGenPik   Flag to know if the User want to Generate Pick tickets for the allocated lines [From the Option grid]
** llExclude    Flag is .T. when user make Exclude option.
STORE .F. TO loFormSet.llRpIncHOr,loFormSet.llRpAlocat,loFormSet.llRpGenPik,loFormSet.llExclude

** lcRpExSlct   Exclude option Select By.
** lcRpSepCor   Variable to hold Separates or coordinate group.
** lcRpAloNot   Variable hold Allocated / Not Allocated status.
** lcRpScpMod   Variable to hold the Select By [From the Option grid]
STORE " " TO loFormSet.lcRpExSlct,loFormSet.lcRpSepCor,loFormSet.lcRpAloNot,loFormSet.lcRpScpMod,loFormSet.lcSOrdStat

** lnDummyPos   Position of dummy variable in filter array.
STORE 0 TO loFormSet.lnDummyPos,loFormSet.lnRngAlias

*-- Variable to hold the title of the option of configuration in O.G.
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.lcConfgTlt = IIF(loFormSet.llUseConfg ,LANG_AUTOALLOC_OPTNCONFG,LANG_AUTOALLOC_OPTNDYELOT)
loFormSet.lcConfgTlt = IIF(loFormSet.llUseConfg ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_OPTNCONFG,loFormSet.GetHeaderText("LANG_AUTOALLOC_OPTNCONFG",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_OPTNDYELOT,loFormSet.GetHeaderText("LANG_AUTOALLOC_OPTNDYELOT",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]


** lnRpPikSep   Variable to hold the Pick separates Min. %
** lnRpPikCor   Variable to hold the Pick coordinate Min. %
STORE 100 TO loFormSet.lnRpPikSep,loFormSet.lnRpPikCor

** lnRpCutUnt   Variable to hold the Cut of units [From the Option grid]
STORE 0 TO loFormSet.lnRpCutUnt ,loFormSet.lnRpSort1 ,loFormSet.lnRpSort2 ,loFormSet.lnRpSort3,loFormSet.lnRpSort4
** lcRpPkFWrh   Variable to hold the Pick from warehouse
** lcRpIncWip   new OG Flag is 'A' if include work process , 'S' Open shipment
STORE " " TO loFormSet.lcRpPkFWrh,loFormSet.lcRpIncWip

** llRpForAlo   Flag to know if we are going to Force the allocation [From the Option grid]
*B607815,1 WAM 10/31/2006 Initialize the variable
*loFormSet.llRpForAlo  = .F.
loFormSet.llRpForAlo  = loFormSet.llAlwForce
*B607815,1 WAM 10/31/2006 (End)

** llRpCond     Flag is .T. if we allocate conditionally.
STORE .F. TO loFormSet.llRpCond

*-- Varables needs for (Pick O.G.)
** lnRpGenNew   Generate New pick ticket variable.
loFormSet.lnRpGenNew = 2
** llRpPkHPck   Add to P/T which have P/L
loFormSet.llRpPkHPck = .F.

loFormSet.lcStyScale = " "

*-- lcTmpOrdLn     Variable to hold a Temp. name for the Temp. Order Lines file
*-- lcTmpIndex     Variable to hold a Temp. name to create an Index in the Temp. Order Lines file with it
*-- lcTmStyTag     Name of Temp. index used in collectiong data In Exclude Style case .
loFormSet.lcTmpOrdLn = gfTempName()
loFormSet.lcTmStyTag = gfTempName()
loFormSet.lcTmpIndex = gfTempName()

=lfBldTolBt(loFormSet)

*-- Open needed files in the program
llRetVal = lfOpnFiles(loFormSet)
=lfCrtFile(loFormSet)
*-- Get the WareCode
=lfGetWareH(loFormSet)

*-- Control the displaying of object on the screen
=lfCntrlObj(loFormSet)


=lfBundBrow(loFormSet)


SELECT (loFormSet.lcTmpOrdLn)
LOCATE

RETURN llRetVal


*!*************************************************************
*! Name      : lfSuppForc
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Suppress Force allocation. (From logic grid...)
*!*************************************************************
*!
FUNCTION lfSuppForc
LPARAMETERS lcAlwForce

llAlwForce = .T.
IF lcAlwForce <> "Y"
  *-- No Force allocation done.
  IF lcAlwForce = "N"
    loFormSet.llRpForAlo = .F.
    llAlwForce = .F.  && Suppress line.
  ELSE  && User Prev.
    *-- Call user defined process.
    llAlwForce = gfUserPriv('AL','ALAUTAL','FORCING')
  ENDIF
ENDIF
RETURN llAlwForce
*-- end of lfSuppForc.

*!*************************************************************
*! Name      : lfvActBar
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : POPUP _OPTIONPOP SELECTION
*!*************************************************************
*! Called from : POPUP _OPTIONPOP
*!*************************************************************
*! Calls       : lfAloScr() , lfRelScr() , lfGenScr , lfvScope()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvActBar
PARAMETERS loFormSet

*E039488,1 MMT 09/12/2005 Convert screen to SQL[Start]
SET DATASESSION TO loFormSet.DATASESSIONID
*E039488,1 MMT 09/12/2005 Convert screen to SQL[End]

=lfAprov(loFormSet)

DO CASE

CASE BAR() = 1      && Scope
  =lfvScope(loFormSet)

CASE BAR() = 3      && Allocation logic and Allocate selected records
  =lfAlocGrid(loFormSet) AND loFormSet.llRpAlocat AND lfAloScr(loFormSet) AND loFormSet.llRpGenPik AND lfGenScr(loFormSet)

CASE BAR() = 4      && Release selected records
  =lfRelScr(loFormSet)

CASE BAR() = 6      && Generate picking tickets options and Generate.
  =lfPickGrid(loFormSet) AND loFormSet.llRpGenPik AND lfGenScr(loFormSet)

ENDCASE    && End of DO CASE Statment
*-- Handle status of tool bar and option menu
=lfHandlObj(loFormSet)
*E039488,1 MMT 09/12/2005 Convert screen to SQL[Start]
SET DATASESSION TO 1
*E039488,1 MMT 09/12/2005 Convert screen to SQL[End]


*:***********************************************************************************
*: Name      : lfAprov
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*: Purpose   : Zap The temp Aprove file to allow Checking Order Approve Amount agian.
*:***********************************************************************************
*: Passed Parameters : None
*:***********************************************************************************
*: Return      : ....
*:***********************************************************************************
*: Example     : = lfChkAprov()
*:***********************************************************************************
FUNCTION lfAprov
PARAMETERS loFormSet

IF loFormSet.llChkAprov
  lcCurrAlis = ALIAS()
  SELECT (loFormSet.lcTmpOrdAp)
  DELETE ALL
  SELECT (lcCurrAlis)
ENDIF


*!*************************************************************
*! Name      : lfvScope
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid function of push button Scope
*!*************************************************************
*! Called from : Control Panel [Push button Scope] , lpShow , Option popup
*!*************************************************************
*! Calls       : gfOpGrid() , gfModalGen() , lfAllocate() ,
*!               lfGenScr() , lfShowGets() , lfRefresh() , lfvpbSel() ,
*!               lfSelData()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvScope
PARAMETERS loFormSet


lcStyle = loFormSet.AriaForm1.cntDetail.keyStyle.VALUE

loFormSet.llRpCond = .F.

DIMENSION loFormSet.laScopExpr[1,2] , loFormSet.laNormExpr[1,2]
loFormSet.laScopExpr = ''
loFormSet.laNormExpr = ''
lcOldScMod = ' '

STORE .F. TO lcExpr1,lcExpr2
*-- Call the selection grid
lcSeason    = loFormSet.lcSeason
lcDivision  = loFormSet.lcDivision
lcStyGroup  = loFormSet.lcStyGroup
lcFabTlt    = loFormSet.lcFabTlt
lcPatTlt    = loFormSet.lcPatTlt
lcStyMajor  = loFormSet.lcStyMajor
lcMajorPic  = loFormSet.lcMajorPic
lcFree_Clr  = loFormSet.lcFree_Clr
lcNonMajPi  = loFormSet.lcNonMajPi
lcNonMajTl  = loFormSet.lcNonMajTl
lcColorTlt  = loFormSet.lcColorTlt
lnColorLen  = loFormSet.lnColorLen
lnNonMajSt  = loFormSet.lnNonMajSt
lnSupMajSt  = loFormSet.lnSupMajSt
llRpGdExcl  = loFormSet.llRpGdExcl
llRpExlDye  = loFormSet.llRpExlDye
llRpExlBlk  = loFormSet.llRpExlBlk
llRpIncHOr  = loFormSet.llRpIncHOr
lcRpExSlct  = loFormSet.lcRpExSlct
lcRpSepCor  = loFormSet.lcRpSepCor
lcRpAloNot  = loFormSet.lcRpAloNot
lcRpScpMod  = loFormSet.lcRpScpMod
lnDummyPos	= loFormSet.lnDummyPos
llUseDyes   = loFormSet.llUseDyes
llUseConfg  = loFormSet.llUseConfg
llFabDye	= loFormSet.llFabDye
llAlwForce  = loFormSet.llAlwForce
llRpPikSep  = loFormSet.llRpPikSep
llRpPikCor  = loFormSet.llRpPikCor
llRpGenPik  = loFormSet.llRpGenPik
lnRngAlias  = loFormSet.lnRngAlias
lcSOrdStat  = loFormSet.lcSOrdStat
lcConfgTlt  = loFormSet.lcConfgTlt
llExclude   = loFormSet.llExclude
gcCmpModules = oAriaApplication.CompanyInstalledModules
=ACOPY(loFormSet.laSlctDesc,laSlctDesc)
=ACOPY(loFormSet.laSlctVals,laSlctVals)
lcOldVal   = " "
llSelData  = .F.  && No data was selected yet.
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*lcPOTlt  = "Purchase order number    "
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcPOTlt  = LANG_AUTOALLOC_PONUMBER
lcPOTlt  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_PONUMBER,loFormSet.GetHeaderText("LANG_AUTOALLOC_PONUMBER",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/22/2012 Globalization changes[END]
*B608997,1 AHS 09/17/2009 Adding new variable to hold the title of POs and cuttkts when Exclude = YES on OG [Start]
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*lcPOExcl = "Purchase order number    "
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcPOExcl = LANG_AUTOALLOC_PONUMBER
lcPOExcl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_PONUMBER,loFormSet.GetHeaderText("LANG_AUTOALLOC_PONUMBER",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 11/22/2012 Globalization changes[END]
*B608997,1 AHS 09/17/2009 Adding new variable to hold the title of POs and cuttkts when Exclude = YES on OG [End]
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	lcBrwFld = "PO :R :H= 'PO#    ', STATUS :R :H= 'Status' , Vendor :R :H= 'Vendor' ,"+;
*!*	           "APVENDOR.cVenComp :R :H= 'Name' , Entered  :R :H= 'Entered' , Complete :R :H= 'Complete' ,"+;
*!*	           "Open :R :H= 'Open' :P='999999' , POTOTAL  :R :H= 'PoTotal' :P='9999999999.99'"
lcBrwFld = "PO :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_PONO,loFormSet.GetHeaderText("LANG_AUTOALLOC_PONO",loFormSet.HeaderAlias)) +"', STATUS :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_STATUS,loFormSet.GetHeaderText("LANG_AUTOALLOC_STATUS",loFormSet.HeaderAlias)) +"' , Vendor :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_VEDNOR,loFormSet.GetHeaderText("LANG_AUTOALLOC_VEDNOR",loFormSet.HeaderAlias)) +"' ,"+;
  "APVENDOR.cVenComp :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_NAME,loFormSet.GetHeaderText("LANG_AUTOALLOC_NAME",loFormSet.HeaderAlias))+"' , Entered  :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_ENTRED,loFormSet.GetHeaderText("LANG_AUTOALLOC_ENTRED",loFormSet.HeaderAlias))+"' , Complete :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_COMPLETE,loFormSet.GetHeaderText("LANG_AUTOALLOC_COMPLETE",loFormSet.HeaderAlias)) +"' ,"+;
  "Open :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_OPEN,loFormSet.GetHeaderText("LANG_AUTOALLOC_OPEN",loFormSet.HeaderAlias)) +"' :P='999999' , POTOTAL  :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_POTOTAL,loFormSet.GetHeaderText("LANG_AUTOALLOC_POTOTAL",loFormSet.HeaderAlias))+"' :P='9999999999.99'"

*N000682,1 MMT 11/22/2012 Globalization changes[END]
*B608997,1 AHS 09/17/2009 Chaning the headers in the browsing of POs and cuttkts [Start]
*N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	lcBrwFldExcl = "PO :R :H= 'PO#    ', STATUS :R :H= 'Status' , Vendor :R :H= 'Vendor' ,"+;
*!*	               "APVENDOR.cVenComp :R :H= 'Name' , Entered  :R :H= 'Entered' , Complete :R :H= 'Complete' ,"+;
*!*	               "Open :R :H= 'Open' :P='999999' , POTOTAL  :R :H= 'PoTotal' :P='9999999999.99'"
lcBrwFldExcl = "PO :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_PONO,loFormSet.GetHeaderText("LANG_AUTOALLOC_PONO",loFormSet.HeaderAlias)) +"', STATUS :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_STATUS,loFormSet.GetHeaderText("LANG_AUTOALLOC_STATUS",loFormSet.HeaderAlias)) +"' , Vendor :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_VEDNOR,loFormSet.GetHeaderText("LANG_AUTOALLOC_VEDNOR",loFormSet.HeaderAlias)) +"' ,"+;
  "APVENDOR.cVenComp :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_NAME,loFormSet.GetHeaderText("LANG_AUTOALLOC_NAME",loFormSet.HeaderAlias))+"' , Entered  :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_ENTRED,loFormSet.GetHeaderText("LANG_AUTOALLOC_ENTRED",loFormSet.HeaderAlias))+"' , Complete :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_COMPLETE,loFormSet.GetHeaderText("LANG_AUTOALLOC_COMPLETE",loFormSet.HeaderAlias)) +"' ,"+;
  "Open :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_OPEN,loFormSet.GetHeaderText("LANG_AUTOALLOC_OPEN",loFormSet.HeaderAlias)) +"' :P='999999' , POTOTAL  :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_POTOTAL,loFormSet.GetHeaderText("LANG_AUTOALLOC_POTOTAL",loFormSet.HeaderAlias))+"' :P='9999999999.99'"


*N000682,1 MMT 11/22/2012 Globalization changes[end]
*B608997,1 AHS 09/17/2009 Chaning the headers in the browsing of POs and cuttkts [End]
STORE .F. TO llClrPO1 , llClrPO2 , llClrOrd2 , llClrSty2 , llClrOrd1 , llClrSty1 , llClrAcc2 , llClrAcc1
llDfCnfgVl = !loFormSet.llUseConfg
lnO_T_S = 0
=lfRelOpMnu(loFormSet)
lcDataSess = loFormSet.DATASESSIONID
lcExpr = gfOpGrid('ALAUTSLC',.T.,.F.,.F.,.T.,.T.)
SET DATASESSION TO lcDataSess
=lfFillVar(1,loFormSet)
*HBG 1/24/2005 Modify code to apply the new interface [Begin]
*=lfBldOpMnu()
=lfBldOpMnu(loFormSet)
*HBG [End]
loFormSet.lcRpScpMod =IIF(EMPTY(loFormSet.lcRpScpMod)," ",loFormSet.lcRpScpMod)
*-- IF lcExpr have a value. which means that the user selecte certain criteria.

IF lcExpr <> ".F."
  IF loFormSet.llFirstRun
    loFormSet.llFirstRun = .F.
    =lfOpnSqlFl(loFormSet)
  ENDIF
  loFormSet.llStartSlc = .T.  && User start from selection grid
  llSelData  = .F.  && No data was selected yet.

  DIMENSION loFormSet.laFiltExp[ALEN(loFormSet.laIncExprs,1),ALEN(loFormSet.laIncExprs,2)]
  =ACOPY(loFormSet.laIncExprs,loFormSet.laFiltExp)
  =lfChngDate(loFormSet)

  loFormSet.llExclude = .F.
  lcOldSlct  = loFormSet.lcRpScpMod
  = lfCreatExp(1,loFormSet) && Create normal expression.

  loFormSet.llPartAlo = .F.
  *-- Collect Data for the selection criteria
  llSelData = lfSelData(loFormSet)  && .T. if we have data.

  loFormSet.lcRpScpMod  = lcOldSlct  && Restore select by.

  loFormSet.llExclude = !loFormSet.llRpGdExcl  && .T. if user want to exclude records.

  *-- if there are records in Temp. file and its exclude option.
  IF llSelData AND loFormSet.llExclude

    *-- Create array have exclude filter expression
    DIMENSION loFormSet.laFiltExp[ALEN(loFormSet.laExcExprs,1),ALEN(loFormSet.laExcExprs,2)]
    =ACOPY(loFormSet.laExcExprs,loFormSet.laFiltExp)

    =lfChngDate(loFormSet)
    *-- Clear the excluded expression from any "DTOS" and "ALLTRIM"
    FOR lnI = 1 TO ALEN(loFormSet.laFiltExp,1)
      IF ('DTOS' $ loFormSet.laFiltExp[lnI,1]) OR ('ALLTRIM' $ loFormSet.laFiltExp[lnI,1])
        lnStartCut = ATC('(',loFormSet.laFiltExp[lnI,1])+1
        lnEndCut   = ATC(')',loFormSet.laFiltExp[lnI,1])
        loFormSet.laFiltExp[lnI,1] = SUBSTR(loFormSet.laFiltExp[lnI,1],lnStartCut,lnEndCut-lnStartCut)
      ENDIF
    ENDFOR

    *-- initialize optimize arrays again.
    DIMENSION loFormSet.laScopExpr[1,2] , loFormSet.laNormExpr[1,2]
    loFormSet.laScopExpr = ''
    loFormSet.laNormExpr = ''

    *-- Select by value will be the excluded values
    lcOldSlct  = loFormSet.lcRpScpMod
    loFormSet.lcRpScpMod = loFormSet.lcRpExSlct

    = lfCreatExp(2,loFormSet)  && Create exclude expression.
    llSelData  = lfSelData(loFormSet)  && .T. if there if temp. file still have records.
    loFormSet.lcRpScpMod = lcOldSlct    && Restore select by value.

  ENDIF  && end if there are records in Temp. file and its exclude option.
ENDIF



*IF if the User has selected Cancel from the Option grid or there is no records for the selection criteria
*HBG 1/24/2005 Modify code to apply the new interface [Begin]
*!*	OAriaApplication.oToolBar.ChangeButtonStatus('pbScop','ENABLED')
loFormSet.oToolBar.ChangeButtonStatus('pbScop','ENABLED')
*HBG [End]

IF lcExpr = '.F.' .OR. !llSelData
  IF !EOF(loFormSet.lcTmpOrdLn) AND lcExpr = '.F.'
    loFormSet.activemode = 'V'        && The Array that hold the Screen mode
    =loFormSet.changeMode('V')
  ELSE
    loFormSet.activemode = 'S'        && The Array that hold the Screen mode
    =loFormSet.changeMode('S')
  ENDIF
  *IF There is no records for the selection criteria
  IF lcExpr <> '.F.'
    IF !EMPTY(lcStyle)
      = lfRefScr(loFormSet)
    ENDIF
    *** Message : "There are no records to display...!"
    ***           "              < Ok >               "
    =gfModalGen("TRM00052B00000","DIALOG")
    *B607789,1 WAM 09/28/2006 Hide progress bar
    loFormSet.oPross.HIDE()
    *B607789,1 WAM 09/28/2006 (End)
  ENDIF    && End of IF
  IF EOF(loFormSet.lcTmpOrdLn)
    loFormSet.AriaForm1.grdOrders.cmdSelect.ENABLED     = .F.
    loFormSet.AriaForm1.grdOrders.cmdSelectAll.ENABLED  = .F.
    loFormSet.AriaForm1.grdOrders.cmdSelectNone.ENABLED = .F.
    loFormSet.AriaForm1.grdOrders.cmdInvert.ENABLED = .F.
    *HBG 1/24/2005 Modify code to apply the new interface [Begin]
    *!*	    OAriaApplication.oToolBar.ChangeButtonStatus('pbAlo','DISABLED')
    *!*	    OAriaApplication.oToolBar.ChangeButtonStatus('pbRel','DISABLED')
    *!*	    OAriaApplication.oToolBar.ChangeButtonStatus('pbGen','DISABLED')
    loFormSet.oToolBar.ChangeButtonStatus('pbAlo','DISABLED')
    loFormSet.oToolBar.ChangeButtonStatus('pbRel','DISABLED')
    loFormSet.oToolBar.ChangeButtonStatus('pbGen','DISABLED')
    *HBG [End]
  ENDIF
ELSE    && Else
  loFormSet.llStartSlc = .F.
  = lfAlocGrid(loFormSet)  && Next two option grids if user want.
  loFormSet.llStartSlc = .F.
  IF loFormSet.llRpAlocat
    loFormSet.lnAloRec = 0
    = lfAllocate(1,.F.,loFormSet)
  ELSE
    *-- Get the # of selected and allocated records if no records selected and allocated
    *-- and under generating pick ticket flag condition
    IF loFormSet.llRpGenPik AND loFormSet.lnSelAlo = 0
      loFormSet.lnSelAlo = RECCOUNT(loFormSet.lcTmpOrdLn)
    ENDIF
  ENDIF
  *IF Generate Pick tickets [From the Option grid] is Yes and there is some
  *allocated records
  IF loFormSet.llRpGenPik .AND. loFormSet.lnSelAlo > 0
    =lfGenScr(loFormSet)
    *IF There is no records to be Browsed [IF we have Generated Pick tickets
    *for all the records in the temp. Order lines file]
    IF RECCOUNT(loFormSet.lcTmpOrdLn) = loFormSet.lnDellRec
      RETURN
    ENDIF    && End of IF
  ENDIF    && End of IF


  loFormSet.activemode = 'V'        && The Array that hold the Screen mode
  =loFormSet.changeMode('V')
  loFormSet.llCtrStat6 = .T.                && Print Button

  GO TOP IN (loFormSet.lcTmpOrdLn)

  SET ORDER TO TAG STYLE IN STYLE
  SELECT (loFormSet.lcTmpOrdLn)
  SET ORDER TO TAG (loFormSet.lcTmpOrdLn)

  *IF The system use Dyelots
  IF loFormSet.llUseDyes
    SET RELATION TO STYLE + cWareCode + DyeLot INTO STYDYE
  ELSE    && Else
    SET RELATION TO STYLE + cWareCode + SPACE(10) INTO STYDYE
  ENDIF    && End of IF
  GO TOP
  loFormSet.lnBrRecNo  = RECNO()
  =SEEK(STYLE , 'STYLE')
  =SEEK('S' + STYLE.SCALE , 'SCALE')

  =lfBundFlds(loFormSet)

  *IF the current record is selected
  IF lnSel = 1
    =lfShowGets(.T.,loFormSet)
  ELSE
    loFormSet.llCh3Stat = .F.
    loFormSet.lapikst   = .F.
    =lfDisblGts(loFormSet)
  ENDIF    && End of IF

  *-- Handle status of tool bar and option menu
  =lfHandlObj(loFormSet)

  SELECT POSHDR1
  IF lfUpdatSQL(loFormSet,'POSHDR1','cBusDocu,cStyType,PO')
    =TABLEUPDATE(.T.,.T.)
  ELSE
    =TABLEREVERT(.T.)
  ENDIF

ENDIF    && End of IF
*-- end of lfvScope.


*!*************************************************************
*! Name      : lfAlocGrid
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Call allocation grid and prepair indecies
*!*************************************************************
*! Called from : Menu,lfvScope
*!*************************************************************
*! Calls       : gfModalGen,gfOpGrid,lfPickGrid.
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfAlocGrid()
*!*************************************************************
*
FUNCTION lfAlocGrid
PARAMETERS loFormSet

*B610670,1 HIA 02/04/2014 T20140123.0021 - DCC - duplicate Pick Tickets with wrong or no qtys [Begin]
IF !USED('Ordline_Picked')
  =gfOpenTable("Ordline",'Ordline','SH','Ordline_Picked')
ENDIF

lnSaveRecNum = RECNO(loFormSet.lcTmpOrdLn)

SELECT (loFormSet.lcTmpOrdLn)
SCAN FOR lnSel=1
  IF gfSeek(EVALUATE(loFormSet.lcTmpOrdLn+'.cOrdtype')+EVALUATE(loFormSet.lcTmpOrdLn+'.Order')+STR(EVALUATE(loFormSet.lcTmpOrdLn+'.LineNo'),6),'Ordline_Picked') &&AND !EMPTY(Ordline_Picked.PIKTKT)
    REPLACE PIKTKT WITH Ordline_Picked.PIKTKT,;
      pik1 WITH  Ordline_Picked.pik1,;
      pik2 WITH  Ordline_Picked.pik2,;
      pik3 WITH  Ordline_Picked.pik3,;
      pik4 WITH  Ordline_Picked.pik4,;
      pik5 WITH  Ordline_Picked.pik5,;
      pik6 WITH  Ordline_Picked.pik6,;
      pik7 WITH  Ordline_Picked.pik7,;
      pik8 WITH  Ordline_Picked.pik8,;
      totpik  WITH  Ordline_Picked.totpik,;
      picked  WITH  Ordline_Picked.picked,;
      pikdate WITH  Ordline_Picked.pikdate IN (loFormSet.lcTmpOrdLn)
    *B610697,1 TMI 03/12/2014 18:40 [Start] update qty fields same as done for pik fields in loFormSet.lcTmpOrdLn from Ordline_Picked
    *B610697,3 TMI 03/16/2014 10:59 [Start] update using the min qty of qty field in loFormSet.lcTmpOrdLn and Ordline_Picked
    *REPLACE Qty1 WITH  Ordline_Picked.Qty1,;
            Qty2 WITH  Ordline_Picked.Qty2,;
            Qty3 WITH  Ordline_Picked.Qty3,;
            Qty4 WITH  Ordline_Picked.Qty4,;
            Qty5 WITH  Ordline_Picked.Qty5,;
            Qty6 WITH  Ordline_Picked.Qty6,;
            Qty7 WITH  Ordline_Picked.Qty7,;
            Qty8 WITH  Ordline_Picked.Qty8,;
            TotQty  WITH  Ordline_Picked.TotQty IN (loFormSet.lcTmpOrdLn)
    *B610705,1 TMI 03/25/2014 15:08 [Start] update totqty same as qty fields
    *REPLACE Qty1 WITH  MIN(qty1,Ordline_Picked.Qty1),;
            Qty2 WITH  MIN(qty2,Ordline_Picked.Qty2),;
            Qty3 WITH  MIN(qty3,Ordline_Picked.Qty3),;
            Qty4 WITH  MIN(qty4,Ordline_Picked.Qty4),;
            Qty5 WITH  MIN(qty5,Ordline_Picked.Qty5),;
            Qty6 WITH  MIN(qty6,Ordline_Picked.Qty6),;
            Qty7 WITH  MIN(qty7,Ordline_Picked.Qty7),;
            Qty8 WITH  MIN(qty8,Ordline_Picked.Qty8),;
            TotQty  WITH  Ordline_Picked.TotQty IN (loFormSet.lcTmpOrdLn)
    REPLACE Qty1 WITH  MIN(qty1,Ordline_Picked.Qty1),;
            Qty2 WITH  MIN(qty2,Ordline_Picked.Qty2),;
            Qty3 WITH  MIN(qty3,Ordline_Picked.Qty3),;
            Qty4 WITH  MIN(qty4,Ordline_Picked.Qty4),;
            Qty5 WITH  MIN(qty5,Ordline_Picked.Qty5),;
            Qty6 WITH  MIN(qty6,Ordline_Picked.Qty6),;
            Qty7 WITH  MIN(qty7,Ordline_Picked.Qty7),;
            Qty8 WITH  MIN(qty8,Ordline_Picked.Qty8),;
            TotQty  WITH  qty1+qty2+qty3+qty4+qty5+qty6+qty7+qty8 IN (loFormSet.lcTmpOrdLn)
    *B610705,1 TMI 03/25/2014 15:11 [End  ] 
    *B610697,3 TMI 03/16/2014 10:59 [End  ] 
    *B610697,1 TMI 03/12/2014 18:40 [End  ] 
  ENDIF
ENDSCAN

IF BETWEEN(lnSaveRecNum,1,RECCOUNT(loFormSet.lcTmpOrdLn))
  GO lnSaveRecNum IN (loFormSet.lcTmpOrdLn)
ENDIF
*B610670,1 HIA 02/04/2014 T20140123.0021 - DCC - duplicate Pick Tickets with wrong or no qtys [End]

loFormSet.lcOldIndex = IIF(EMPTY(loFormSet.lcOldIndex),lfEvalIndx(),loFormSet.lcOldIndex)
loFormSet.lcIndexExp = IIF(EMPTY(loFormSet.lcIndexExp),loFormSet.lcOldIndex,loFormSet.lcIndexExp)

loFormSet.lcRpIncWip = IIF(loFormSet.llTotAvlbl AND (loFormSet.lcRpScpMod $ 'KP' OR EMPTY(loFormSet.lcRpScpMod)),'A','N')

IF loFormSet.llPartAlo
  *-- Do you wish to Allocate. <Yes>  <No>
  IF gfModalGen('QRM44052B42002','Dialog') = 1
    *-- call allocation logic grid.
    lnNonMajSt  = loFormSet.lnNonMajSt
    lnColorLen  = loFormSet.lnColorLen
    lnRpPikSep  = loFormSet.lnRpPikSep
    lnRpPikCor  = loFormSet.lnRpPikCor
    lnRpCutUnt  = loFormSet.lnRpCutUnt
    lnRpSort1   = loFormSet.lnRpSort1
    lnRpSort2   = loFormSet.lnRpSort2
    lnRpSort3   = loFormSet.lnRpSort3
    lnRpSort4   = loFormSet.lnRpSort4
    lcRpPkFWrh  = loFormSet.lcRpPkFWrh
    lcRpIncWip  = loFormSet.lcRpIncWip
    llRpForAlo  = loFormSet.llRpForAlo
    llRpCond    = loFormSet.llRpCond
    lcFree_Clr  = loFormSet.lcFree_Clr
    lcSeason    = loFormSet.lcSeason
    lcDivision  = loFormSet.lcDivision
    lcStyGroup  = loFormSet.lcStyGroup
    lcFabTlt    = loFormSet.lcFabTlt
    lcPatTlt    = loFormSet.lcPatTlt
    lcColorTlt  = loFormSet.lcColorTlt
    lcRpSepCor  = loFormSet.lcRpSepCor
    llUseDyes   = loFormSet.llUseDyes
    llFabDye	= loFormSet.llFabDye
    llAlwForce  = loFormSet.llAlwForce
    llRpGenPik  = loFormSet.llRpGenPik
    lnRngAlias  = loFormSet.lnRngAlias
    lcSOrdStat  = loFormSet.lcSOrdStat
    lcRpAloNot  = loFormSet.lcRpAloNot
    llRpPikCor  = loFormSet.llRpPikCor
    llRpPikSep  = loFormSet.llRpPikSep
    llUseConfg  = loFormSet.llUseConfg
    lcPkWOldVl  = ""
    DIMENSION laSortAry[4,2]
    =ACOPY(loFormSet.laSortAry,laSortAry)
    gcCmpModules = oAriaApplication.CompanyInstalledModules
    lcOldVal   = ""
    =lfRelOpMnu(loFormSet)
    lcDataSess = loFormSet.DATASESSIONID
    lcExpr1 = gfOpGrid('ALAUTALC' , .T.,.F.,.F.,.T.,.T.)  && Allocation logic grid.
    SET DATASESSION TO lcDataSess
    *HBG 1/24/2005 Modify code to apply the new interface [Begin]
    *=lfBldOpMnu()
    =lfBldOpMnu(loFormSet)
    *HBG [End]
    =lfFillVar(2,loFormSet)
    FOR lnCntr = 1 TO 4
      lcArrayCnt = STR(lnCntr,1)
      loFormSet.laSortAry[lnCntr,1] = lnRpSort&lcArrayCnt
      loFormSet.laSortAry[lnCntr,2] = loFormSet.laIndexExp[lnRpSort&lcArrayCnt]
    ENDFOR

    *-- if user press run and call allocation grid from menu itself and
    *-- change Sort1/Sort2/Sort3 .
    IF lcExpr1 <> ".F."

      SELECT (loFormSet.lcTmpOrdLn)
      REPLACE ALL cReason WITH ""

      *-- IF Include open Shipments , get the selected shipments.
      IF loFormSet.lcRpIncWip = 'S'
        IF lfopensql(loFormSet,'SHPMTHDR','SHPMTHDR1',loFormSet.lcShpCond)
          SELECT SHPMTHDR1
          lnI = 0
          SCAN
            lnI = lnI + 1
            DIMENSION loFormSet.laShpExp[lnI]
            loFormSet.laShpExp[lnI] = SHPMTHDR1.ShipNo
          ENDSCAN
        ENDIF
      ENDIF

      loFormSet.lcIndexExp = lfEvalIndx()
      lnSaveRec = RECNO(loFormSet.lcTmpOrdLn)
      GO TOP IN (loFormSet.lcTmpOrdLn)
      IF !(loFormSet.llStartSlc) AND !EOF(loFormSet.lcTmpOrdLn) AND !(loFormSet.lcIndexExp == loFormSet.lcOldIndex)
        SELECT (loFormSet.lcTmpOrdLn)
        REPLACE ALL cSortField WITH EVALUATE(loFormSet.lcIndexExp)
        GO TOP
      ENDIF

      IF BETWEEN(lnSaveRec,1,RECCOUNT(loFormSet.lcTmpOrdLn))
        GO lnSaveRec IN (loFormSet.lcTmpOrdLn)
      ENDIF

      loFormSet.lcOldIndex = loFormSet.lcIndexExp
      loFormSet.llRpAlocat = .T.  && user want to allocate.
    ELSE
      loFormSet.llRpAlocat = .F.  && Disable Allocate Flag.
    ENDIF  && end if user press OG <Run> .
  ELSE  && User response message with <No>
    loFormSet.llRpAlocat = .F.
  ENDIF
ELSE
  loFormSet.llRpAlocat = .T.
ENDIF


loFormSet.llPartAlo = .T.

IF TYPE("loFormSet.laPanelObj[4,1]") = "C"
  IF loFormSet.llRpAlocat OR (loFormSet.lcRpAloNot # 'N')
    = lfPickGrid(loFormSet) && Call picket ticket quetion.
  ENDIF
ELSE  && else standard program
  loFormSet.llRpGenPik = .F.
ENDIF

*-- end of lfAlocGrid.

*!*************************************************************
*! Name      : lfRelScr
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid function of push button Release
*!*************************************************************
*! Called from : Control Panel [Push button Release] , Option popup
*!*************************************************************
*! Calls       : gfThermo() , lfRelQty() , lfShowGets() , lfRefresh()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfRelScr
PARAMETERS loFormSet

PRIVATE lcRelStyle , lnTotRec , lnCurent ,;
  lnPrepRec

*B610670,1 HIA 02/04/2014 T20140123.0021 - DCC - duplicate Pick Tickets with wrong or no qtys [Begin]
IF !USED('Ordline_Picked')
  =gfOpenTable("Ordline",'Ordline','SH','Ordline_Picked')
ENDIF

lnSaveRecNum = RECNO(loFormSet.lcTmpOrdLn)

SELECT (loFormSet.lcTmpOrdLn)
SCAN FOR lnSel=1
  IF gfSeek(EVALUATE(loFormSet.lcTmpOrdLn+'.cOrdtype')+EVALUATE(loFormSet.lcTmpOrdLn+'.Order')+STR(EVALUATE(loFormSet.lcTmpOrdLn+'.LineNo'),6),'Ordline_Picked') &&AND !EMPTY(Ordline_Picked.PIKTKT)
    REPLACE PIKTKT WITH Ordline_Picked.PIKTKT,;
      pik1 WITH  Ordline_Picked.pik1,;
      pik2 WITH  Ordline_Picked.pik2,;
      pik3 WITH  Ordline_Picked.pik3,;
      pik4 WITH  Ordline_Picked.pik4,;
      pik5 WITH  Ordline_Picked.pik5,;
      pik6 WITH  Ordline_Picked.pik6,;
      pik7 WITH  Ordline_Picked.pik7,;
      pik8 WITH  Ordline_Picked.pik8,;
      totpik  WITH  Ordline_Picked.totpik,;
      picked  WITH  Ordline_Picked.picked,;
      pikdate WITH  Ordline_Picked.pikdate IN (loFormSet.lcTmpOrdLn)
    *B610697,1 TMI 03/12/2014 18:40 [Start] update qty fields same as done for pik fields in loFormSet.lcTmpOrdLn from Ordline_Picked
    *B610697,3 TMI 03/16/2014 10:59 [Start] update using the min qty of qty field in loFormSet.lcTmpOrdLn and Ordline_Picked     
    *REPLACE Qty1 WITH  Ordline_Picked.Qty1,;
            Qty2 WITH  Ordline_Picked.Qty2,;
            Qty3 WITH  Ordline_Picked.Qty3,;
            Qty4 WITH  Ordline_Picked.Qty4,;
            Qty5 WITH  Ordline_Picked.Qty5,;
            Qty6 WITH  Ordline_Picked.Qty6,;
            Qty7 WITH  Ordline_Picked.Qty7,;
            Qty8 WITH  Ordline_Picked.Qty8,;
            TotQty  WITH  Ordline_Picked.TotQty IN (loFormSet.lcTmpOrdLn)
    *B610705,1 TMI 03/25/2014 15:08 [Start] update totqty same as qty fields
    *REPLACE Qty1 WITH  MIN(qty1,Ordline_Picked.Qty1),;
            Qty2 WITH  MIN(qty2,Ordline_Picked.Qty2),;
            Qty3 WITH  MIN(qty3,Ordline_Picked.Qty3),;
            Qty4 WITH  MIN(qty4,Ordline_Picked.Qty4),;
            Qty5 WITH  MIN(qty5,Ordline_Picked.Qty5),;
            Qty6 WITH  MIN(qty6,Ordline_Picked.Qty6),;
            Qty7 WITH  MIN(qty7,Ordline_Picked.Qty7),;
            Qty8 WITH  MIN(qty8,Ordline_Picked.Qty8),;
            TotQty  WITH  Ordline_Picked.TotQty IN (loFormSet.lcTmpOrdLn)
    REPLACE Qty1 WITH  MIN(qty1,Ordline_Picked.Qty1),;
            Qty2 WITH  MIN(qty2,Ordline_Picked.Qty2),;
            Qty3 WITH  MIN(qty3,Ordline_Picked.Qty3),;
            Qty4 WITH  MIN(qty4,Ordline_Picked.Qty4),;
            Qty5 WITH  MIN(qty5,Ordline_Picked.Qty5),;
            Qty6 WITH  MIN(qty6,Ordline_Picked.Qty6),;
            Qty7 WITH  MIN(qty7,Ordline_Picked.Qty7),;
            Qty8 WITH  MIN(qty8,Ordline_Picked.Qty8),;
            TotQty  WITH  qty1+qty2+qty3+qty4+qty5+qty6+qty7+qty8 IN (loFormSet.lcTmpOrdLn)
    *B610705,1 TMI 03/25/2014 15:11 [End  ] 
    *B610697,3 TMI 03/16/2014 10:59 [End  ]         
    *B610697,1 TMI 03/12/2014 18:41 [End  ] 
  ENDIF
ENDSCAN

IF BETWEEN(lnSaveRecNum,1,RECCOUNT(loFormSet.lcTmpOrdLn))
  GO lnSaveRecNum IN (loFormSet.lcTmpOrdLn)
ENDIF
*B610670,1 HIA 02/04/2014 T20140123.0021 - DCC - duplicate Pick Tickets with wrong or no qtys [End]


*IF Statment to make sure that the user want to release the selected records
*** Message : "Are you sure you want to release the allocation from the selected order lines "
***           "                   < Yes >           < No >                    "
IF gfModalGen("TRM44007B00006","DIALOG") = 1

  SET ORDER TO TAG STYDYE IN STYDYE
  SET ORDER TO TAG STYLE IN STYLE
  SET ORDER TO TAG ORDLINE IN ORDLINE
  SET ORDER TO TAG ORDHDR IN ORDHDR
  SELECT (loFormSet.lcTmpOrdLn)
  SET ORDER TO TAG (loFormSet.lcTmpOrdLn)
  SET RELATION TO

  loFormSet.lnBrRecNo = RECNO(loFormSet.lcTmpOrdLn)
  COUNT FOR lnSel = 1 AND totpik <> 0 TO lnTotRec  && Varible to hold the Total count to be done for the thermometer
  lnCurent = 0                   && Varible to hold the current count to be done for the thermometer

  *IF This is not an incompleted session
  lnRecCount = RECCOUNT() - loFormSet.lnDellRec      && Varible to hold the Total count to be done for the thermometer
  lnPrepRec = 0                            && Varible to hold the current count to be done for the thermometer
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loFormSet.oPross.lblFirstLabel.Caption = LANG_AutoAlloc_LableProgress
  loFormSet.oPross.lblFirstLabel.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LableProgress,loFormSet.GetHeaderText("LANG_AutoAlloc_LableProgress",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.oPross.TotalProgress = lnRecCount
  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  loFormSet.oPross.AUTOCENTER = .T.
  *HBG [End]
  loFormSet.oPross.SHOW()
  *SCAN Loop to scan the temp. Order lines file
  SCAN
    lnPrepRec = lnPrepRec + 1
    REPLACE nProcNo WITH 0
    loFormSet.oPross.CurrentProgress(lnPrepRec)
  ENDSCAN    && End of SCAN Loop


  SELECT (loFormSet.lcTmpOrdLn)
  *SCAN Loop to scan the temp. Order lines file FOR the selected
  *and allocated records and for nProcNo < 12
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loFormSet.oPross.lblFirstLabel.Caption = LANG_AutoAlloc_LableReleas
  loFormSet.oPross.lblFirstLabel.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LableReleas,loFormSet.GetHeaderText("LANG_AutoAlloc_LableReleas",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.oPross.TotalProgress = lnTotRec
  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  loFormSet.oPross.AUTOCENTER = .T.
  *HBG [End]

  loFormSet.oPross.SHOW()
  SCAN FOR lnSel = 1 .AND. totpik > 0 .AND. nProcNo < 14
    lcRelStyle = STYLE          && Variable to hold the Style
    =lfRelQty(loFormSet)
    loFormSet.lnSelAlo = loFormSet.lnSelAlo - 1
    loFormSet.lnAloRec = loFormSet.lnAloRec - 1
    lnCurent = lnCurent + 1
    loFormSet.oPross.lblSecondLabel.CAPTION = ORDER + '/' + lcRelStyle
    loFormSet.oPross.CurrentProgress(lnCurent)
  ENDSCAN    && End of SCAN Loop

  SELECT (loFormSet.lcTmpOrdLn)

  *IF The system use Dyelots
  IF loFormSet.llUseDyes
    SET RELATION TO STYLE + cWareCode + DyeLot INTO STYDYE
  ELSE    && Else
    SET RELATION TO STYLE + cWareCode + SPACE(10) INTO STYDYE
  ENDIF    && End of IF
  GO loFormSet.lnBrRecNo

  *-- Handle status of tool bar and option menu
  =lfHandlObj(loFormSet)

  =SEEK(STYLE , 'STYLE')
  =SEEK('S' + STYLE.SCALE , 'SCALE')
  =lfBundFlds(loFormSet)

  *IF the current record is selected
  IF lnSel = 1
    =lfShowGets(.T.,loFormSet)
  ELSE
    loFormSet.llCh3Stat = .F.
    loFormSet.lapikst   = .F.
    =lfDisblGts(loFormSet)
  ENDIF    && End of IF

ENDIF    && End of IF



*!*************************************************************
*! Name      : lfPickGrid
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Call allocation grid and prepair indecies
*!*************************************************************
*! Called from : Menu,lfAlocGrid
*!*************************************************************
*! Calls       : gfModalGen,gfOpGrid.
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfPickGrid()
*!*************************************************************
FUNCTION lfPickGrid
PARAMETERS loFormSet


*B610670,1 HIA 02/04/2014 T20140123.0021 - DCC - duplicate Pick Tickets with wrong or no qtys [Begin]
IF !USED('Ordline_Picked')
  =gfOpenTable("Ordline",'Ordline','SH','Ordline_Picked')
ENDIF

lnSaveRecNum = RECNO(loFormSet.lcTmpOrdLn)

SELECT (loFormSet.lcTmpOrdLn)
SCAN FOR lnSel=1
  IF gfSeek(EVALUATE(loFormSet.lcTmpOrdLn+'.cOrdtype')+EVALUATE(loFormSet.lcTmpOrdLn+'.Order')+STR(EVALUATE(loFormSet.lcTmpOrdLn+'.LineNo'),6),'Ordline_Picked') &&AND !EMPTY(Ordline_Picked.PIKTKT)
    REPLACE PIKTKT WITH Ordline_Picked.PIKTKT,;
      pik1 WITH  Ordline_Picked.pik1,;
      pik2 WITH  Ordline_Picked.pik2,;
      pik3 WITH  Ordline_Picked.pik3,;
      pik4 WITH  Ordline_Picked.pik4,;
      pik5 WITH  Ordline_Picked.pik5,;
      pik6 WITH  Ordline_Picked.pik6,;
      pik7 WITH  Ordline_Picked.pik7,;
      pik8 WITH  Ordline_Picked.pik8,;
      totpik  WITH  Ordline_Picked.totpik,;
      picked  WITH  Ordline_Picked.picked,;
      pikdate WITH  Ordline_Picked.pikdate IN (loFormSet.lcTmpOrdLn)
    *B610697,1 TMI 03/12/2014 18:40 [Start] update qty fields same as done for pik fields in loFormSet.lcTmpOrdLn from Ordline_Picked
    *B610697,3 TMI 03/16/2014 10:59 [Start]  update using the min qty of qty field in loFormSet.lcTmpOrdLn and Ordline_Picked
    *REPLACE Qty1 WITH  Ordline_Picked.Qty1,;
            Qty2 WITH  Ordline_Picked.Qty2,;
            Qty3 WITH  Ordline_Picked.Qty3,;
            Qty4 WITH  Ordline_Picked.Qty4,;
            Qty5 WITH  Ordline_Picked.Qty5,;
            Qty6 WITH  Ordline_Picked.Qty6,;
            Qty7 WITH  Ordline_Picked.Qty7,;
            Qty8 WITH  Ordline_Picked.Qty8,;
            TotQty  WITH  Ordline_Picked.TotQty IN (loFormSet.lcTmpOrdLn)
    *B610705,1 TMI 03/25/2014 15:08 [Start] update totqty same as qty fields
    *REPLACE Qty1 WITH  MIN(qty1,Ordline_Picked.Qty1),;
            Qty2 WITH  MIN(qty2,Ordline_Picked.Qty2),;
            Qty3 WITH  MIN(qty3,Ordline_Picked.Qty3),;
            Qty4 WITH  MIN(qty4,Ordline_Picked.Qty4),;
            Qty5 WITH  MIN(qty5,Ordline_Picked.Qty5),;
            Qty6 WITH  MIN(qty6,Ordline_Picked.Qty6),;
            Qty7 WITH  MIN(qty7,Ordline_Picked.Qty7),;
            Qty8 WITH  MIN(qty8,Ordline_Picked.Qty8),;
            TotQty  WITH  Ordline_Picked.TotQty IN (loFormSet.lcTmpOrdLn)
    REPLACE Qty1 WITH  MIN(qty1,Ordline_Picked.Qty1),;
            Qty2 WITH  MIN(qty2,Ordline_Picked.Qty2),;
            Qty3 WITH  MIN(qty3,Ordline_Picked.Qty3),;
            Qty4 WITH  MIN(qty4,Ordline_Picked.Qty4),;
            Qty5 WITH  MIN(qty5,Ordline_Picked.Qty5),;
            Qty6 WITH  MIN(qty6,Ordline_Picked.Qty6),;
            Qty7 WITH  MIN(qty7,Ordline_Picked.Qty7),;
            Qty8 WITH  MIN(qty8,Ordline_Picked.Qty8),;
            TotQty  WITH  qty1+qty2+qty3+qty4+qty5+qty6+qty7+qty8 IN (loFormSet.lcTmpOrdLn)
    *B610705,1 TMI 03/25/2014 15:12 [End  ] 
    *B610697,3 TMI 03/16/2014 10:59 [End  ] 
    *B610697,1 TMI 03/12/2014 18:41 [End  ] 
  ENDIF
ENDSCAN

IF BETWEEN(lnSaveRecNum,1,RECCOUNT(loFormSet.lcTmpOrdLn))
  GO lnSaveRecNum IN (loFormSet.lcTmpOrdLn)
ENDIF
*B610670,1 HIA 02/04/2014 T20140123.0021 - DCC - duplicate Pick Tickets with wrong or no qtys [End]


*-- Do you wish to Generate pick ticket. <Yes>  <No>
IF loFormSet.llPikNow .AND. gfModalGen('QRM44053B42002','Dialog') = 1
  lnRpGenNew = loFormSet.lnRpGenNew
  llRpPkHPck = loFormSet.llRpPkHPck
  llRpGenPik = loFormSet.llRpGenPik
  lcRpAloNot = loFormSet.lcRpAloNot
  gcCmpModules = oAriaApplication.CompanyInstalledModules
  lcOldVal   = ""
  =lfRelOpMnu(loFormSet)
  lcDataSess = loFormSet.DATASESSIONID
  *E303530,1 MMT 11/30/2014 Add Triggers for [T20141118.0011][Start]
  IF ASCAN(loFormSet.laEvntTrig,PADR('DEFINEVAR',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
    loFormSet.mDoTrigger(PADR('DEFINEVAR',10))
  ENDIF
  *E303530,1 MMT 11/30/2014 Add Triggers for [T20141118.0011][End]
  lcExpr2 = gfOpGrid('ALAUTPIK' , .T.,.F.,.F.,.T.,.T.)   && Pick Ticket grid.
  SET DATASESSION TO lcDataSess
  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *=lfBldOpMnu()
  =lfBldOpMnu(loFormSet)
  *HBG [End]
  =lfFillVar(3,loFormSet)
  IF lcExpr2 <> ".F."
    loFormSet.llRpGenPik = .T.
  ELSE
    loFormSet.llRpGenPik = .F.
  ENDIF
ELSE  && User response message with <No>
  loFormSet.llRpGenPik = .F.
ENDIF
*-- end of lfPickGrid.


*!*************************************************************
*! Name      : lfBldTolBt
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Add new buttons to the tool bar
*!*************************************************************
*! Called from : Menu,lfAlocGrid
*!*************************************************************
*! Calls       : gfModalGen,gfOpGrid.
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfBldTolBt()
*!*************************************************************
FUNCTION lfBldTolBt
PARAMETERS loFormSet

DECLARE loFormSet.laPanelObj[lnPanArrLn,6]
STORE '' TO loFormSet.laPanelObj
loFormSet.laPanelObj[1,1] = 'pbScop'
loFormSet.laPanelObj[1,2] = oAriaApplication.BitMapHome+"SCOPE.bmp"
loFormSet.laPanelObj[1,3] = 'mTolScope'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laPanelObj[1,4] = LANG_AutoAlloc_Scope
loFormSet.laPanelObj[1,4] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_Scope,loFormSet.GetHeaderText("LANG_AutoAlloc_Scope",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laPanelObj[1,5] = LANG_AutoAlloc_Scope
loFormSet.laPanelObj[1,5] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_Scope,loFormSet.GetHeaderText("LANG_AutoAlloc_Scope",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

loFormSet.laPanelObj[1,6] = 'V'

loFormSet.laPanelObj[2,1] = 'pbAlo'
loFormSet.laPanelObj[2,2] = oAriaApplication.BitMapHome+"PICK.bmp"
loFormSet.laPanelObj[2,3] = 'mTolAlocGrid'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laPanelObj[2,4] = LANG_AutoAlloc_Allocate
loFormSet.laPanelObj[2,4] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_Allocate,loFormSet.GetHeaderText("LANG_AutoAlloc_Allocate",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laPanelObj[2,5] = LANG_AutoAlloc_Allocate
loFormSet.laPanelObj[2,5] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_Allocate,loFormSet.GetHeaderText("LANG_AutoAlloc_Allocate",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

loFormSet.laPanelObj[2,6] = 'V'

loFormSet.laPanelObj[3,1] = 'pbRel'
loFormSet.laPanelObj[3,2] = oAriaApplication.BitMapHome+"RELEASE2.bmp"
loFormSet.laPanelObj[3,3] = 'mTolRelScr'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laPanelObj[3,4] = LANG_AutoAlloc_release
loFormSet.laPanelObj[3,4] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_release,loFormSet.GetHeaderText("LANG_AutoAlloc_release",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laPanelObj[3,5] = LANG_AutoAlloc_release
loFormSet.laPanelObj[3,5] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_release,loFormSet.GetHeaderText("LANG_AutoAlloc_release",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

loFormSet.laPanelObj[3,6] = 'V'

IF TYPE("loFormSet.laPanelObj[4,1]") = "C"
  loFormSet.laPanelObj[4,1] = 'pbGen'
  loFormSet.laPanelObj[4,2] = oAriaApplication.BitMapHome+"GENERAT.BMP"
  loFormSet.laPanelObj[4,3] = 'mTolPickGrid'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loFormSet.laPanelObj[4,4] = LANG_AutoAlloc_GenPick
  loFormSet.laPanelObj[4,4] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_GenPick,loFormSet.GetHeaderText("LANG_AutoAlloc_GenPick",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loFormSet.laPanelObj[4,5] = LANG_AutoAlloc_GenPick
  loFormSet.laPanelObj[4,5] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_GenPick,loFormSet.GetHeaderText("LANG_AutoAlloc_GenPick",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.laPanelObj[4,6] = 'V'
ENDIF

*!*************************************************************
*! Name      : lfActPad
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Called from : ALAUTAL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfActPad
PARAMETERS loFormSet

IF TYPE("_SCREEN.ActiveForm.parent") = 'O' AND !ISNULL(_SCREEN.ACTIVEFORM.PARENT) AND;
    TYPE("_SCREEN.ActiveForm.parent.lnSelRec") <> 'U' AND !(TYPE("glOptionGridIsRuning") <> 'U' AND glOptionGridIsRuning)
  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *=lfBldOpMnu()
  =lfBldOpMnu(loFormSet)
  *HBG [End]
  *-- IF it is first time run current session , so it is not incompleted sesion.
  *-- We are going to call the Option grid
  IF _SCREEN.ACTIVEFORM.PARENT.llCallScop
    loFormSet.oPross = CREATEOBJECT('ariaprogressbar')
    loFormSet.REFRESH()
    _SCREEN.ACTIVEFORM.PARENT.llCallScop = .F.
    =lfvScope(_SCREEN.ACTIVEFORM.PARENT)
  ENDIF    && End of IF
ENDIF

*!*************************************************************
*! Name      : lfBldOpMnu
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Build the menu pad [Options]
*!*************************************************************
*! Called from : ALAUTAL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfBldOpMnu
*HBG 1/24/2005 Modify code to apply the new interface [Begin]
PARAMETERS loFormSet
IF TYPE('_SCREEN.ActiveForm.parent') = 'O'
  *DEFINE PAD _Option OF _MSYSMENU PROMPT LANG_AutoAlloc_Option KEY ALT+P , ' ' SKIP FOR (TYPE("glOptionGridIsRuning") <> 'U' AND glOptionGridIsRuning)
  *ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *DEFINE PAD _Option OF (_SCREEN.ActiveForm.parent.chostformname) PROMPT LANG_AutoAlloc_Option KEY ALT+P , ' ' SKIP FOR (TYPE("glOptionGridIsRuning") <> 'U' AND glOptionGridIsRuning)
  DEFINE PAD _Option OF (_SCREEN.ACTIVEFORM.PARENT.chostformname) PROMPT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_Option,loFormSet.GetHeaderText("LANG_AutoAlloc_Option",loFormSet.HeaderAlias)) KEY ALT+P , ' ' SKIP FOR (TYPE("glOptionGridIsRuning") <> 'U' AND glOptionGridIsRuning)
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  ON PAD _Option OF (_SCREEN.ACTIVEFORM.PARENT.chostformname) ACTIVATE POPUP _OPTIONPOP
  lcHostFormName = '[' + loFormSet.chostformname + ']'
  *HBG [End]
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *DEFINE BAR 1 OF _OPTIONPOP PROMPT LANG_AutoAlloc_ScopeOpt
  DEFINE BAR 1 OF _OPTIONPOP PROMPT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_ScopeOpt	,loFormSet.GetHeaderText("LANG_AutoAlloc_ScopeOpt	",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *DEFINE BAR 2 OF _OPTIONPOP PROMPT LANG_AutoAlloc_SepertOpt      SKIP FOR .T.
  DEFINE BAR 2 OF _OPTIONPOP PROMPT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_SepertOpt,loFormSet.GetHeaderText("LANG_AutoAlloc_SepertOpt",loFormSet.HeaderAlias))      SKIP FOR .T.
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *!*	  DEFINE BAR 3 OF _OPTIONPOP PROMPT LANG_AutoAlloc_AllocateOpt    SKIP FOR (_SCREEN.ActiveForm.parent.lnSelRec = 0)
  *!*	  DEFINE BAR 4 OF _OPTIONPOP PROMPT LANG_AutoAlloc_releaseOpt     SKIP FOR (_SCREEN.ActiveForm.parent.lnSelAlo = 0)
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *DEFINE BAR 3 OF _OPTIONPOP PROMPT LANG_AutoAlloc_AllocateOpt    SKIP FOR gfFormIsActive(&lcHostFormName) .AND. (_SCREEN.ActiveForm.parent.lnSelRec = 0)
  DEFINE BAR 3 OF _OPTIONPOP PROMPT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_AllocateOpt,loFormSet.GetHeaderText("LANG_AutoAlloc_AllocateOpt",loFormSet.HeaderAlias))    SKIP FOR gfFormIsActive(&lcHostFormName) .AND. (_SCREEN.ACTIVEFORM.PARENT.lnSelRec = 0)
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *DEFINE BAR 4 OF _OPTIONPOP PROMPT LANG_AutoAlloc_releaseOpt     SKIP FOR gfFormIsActive(&lcHostFormName) .AND. (_SCREEN.ActiveForm.parent.lnSelAlo = 0)
  DEFINE BAR 4 OF _OPTIONPOP PROMPT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_releaseOpt,loFormSet.GetHeaderText("LANG_AutoAlloc_releaseOpt",loFormSet.HeaderAlias))     SKIP FOR gfFormIsActive(&lcHostFormName) .AND. (_SCREEN.ACTIVEFORM.PARENT.lnSelAlo = 0)
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *HBG [End]

  IF TYPE("_SCREEN.ActiveForm.parent.laPanelObj[4,1]") = "C"
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *DEFINE BAR 5 OF _OPTIONPOP PROMPT LANG_AutoAlloc_SepertOpt   SKIP FOR .T.
    DEFINE BAR 5 OF _OPTIONPOP PROMPT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_SepertOpt,loFormSet.GetHeaderText("LANG_AutoAlloc_SepertOpt",loFormSet.HeaderAlias))   SKIP FOR .T.
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *HBG 1/24/2005 Modify code to apply the new interface [Begin]
    *DEFINE BAR 6 OF _OPTIONPOP PROMPT LANG_AutoAlloc_GenerateOp  SKIP FOR (_SCREEN.ActiveForm.parent.lnSelAlo = 0)
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *DEFINE BAR 6 OF _OPTIONPOP PROMPT LANG_AutoAlloc_GenerateOp  SKIP FOR gfFormIsActive(&lcHostFormName) .AND. (_SCREEN.ActiveForm.parent.lnSelAlo = 0)
    DEFINE BAR 6 OF _OPTIONPOP PROMPT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_GenerateOp,loFormSet.GetHeaderText("LANG_AutoAlloc_GenerateOp",loFormSet.HeaderAlias))  SKIP FOR gfFormIsActive(&lcHostFormName) .AND. (_SCREEN.ACTIVEFORM.PARENT.lnSelAlo = 0)
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *HBG [End]
  ENDIF

  ON SELECTION POPUP _OPTIONPOP DO lfvActBar WITH _SCREEN.ACTIVEFORM.PARENT
  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
ENDIF
*HBG [End]

*!*************************************************************
*! Name      : lfRelOpMnu
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Release the menu pad [Options]
*!*************************************************************
*! Called from : ALAUTAL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfRelOpMnu
PARAMETERS loFormSet

*HBG 1/24/2005 Modify code to apply the new interface [Begin]
*IF TYPE('loFormSet') = 'O'
*RELEASE PAD _Option OF _MSYSMENU
IF TYPE('loFormSet') = 'O'
  RELEASE PAD _Option OF (loFormSet.chostformname)
  *HBG [End]
ENDIF

*!*************************************************************
*! Name      : lfCrtFile
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Create temp file
*!*************************************************************
*! Called from : ALAUTAL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCrtFile
PARAMETERS loFormSet

SELECT ORDLINE
=AFIELDS(loFormSet.laFileStru)
lnFileStru = ALEN(loFormSet.laFileStru,1)        && Varible to hold the number of fields in the ORDLINE file
lnAddToArr = IIF('MF' $ oAriaApplication.CompanyInstalledModules  .OR. 'PO' $ oAriaApplication.CompanyInstalledModules ,12,10)
lnAddToArr = IIF(loFormSet.llUseDyes,lnAddToArr+1,lnAddToArr)
DIMENSION loFormSet.laFileStru[lnFileStru+lnAddToArr,ALEN(loFormSet.laFileStru,2)]

loFormSet.laFileStru[lnFileStru+1,1] = 'PRIORITY'
loFormSet.laFileStru[lnFileStru+1,2] = 'C'
loFormSet.laFileStru[lnFileStru+1,3] = 3
loFormSet.laFileStru[lnFileStru+1,4] = 0

loFormSet.laFileStru[lnFileStru+2,1] = 'LnSEL'
loFormSet.laFileStru[lnFileStru+2,2] = 'N'
loFormSet.laFileStru[lnFileStru+2,3] = 1
loFormSet.laFileStru[lnFileStru+2,4] = 0

loFormSet.laFileStru[lnFileStru+3,1] = 'NPROCNO'
loFormSet.laFileStru[lnFileStru+3,2] = 'N'
loFormSet.laFileStru[lnFileStru+3,3] = 2
loFormSet.laFileStru[lnFileStru+3,4] = 0

loFormSet.laFileStru[lnFileStru+4,1] = 'ENTERED'
loFormSet.laFileStru[lnFileStru+4,2] = 'D'
loFormSet.laFileStru[lnFileStru+4,3] = 8
loFormSet.laFileStru[lnFileStru+4,4] = 0

loFormSet.laFileStru[lnFileStru+5,1] = 'CDIVISION'
loFormSet.laFileStru[lnFileStru+5,2] = 'C'
loFormSet.laFileStru[lnFileStru+5,3] = 6
loFormSet.laFileStru[lnFileStru+5,4] = 0

loFormSet.laFileStru[lnFileStru+6,1] = 'CSTYGROUP'
loFormSet.laFileStru[lnFileStru+6,2] = 'C'
loFormSet.laFileStru[lnFileStru+6,3] = 6
loFormSet.laFileStru[lnFileStru+6,4] = 0

loFormSet.laFileStru[lnFileStru+7,1] = 'FABRIC'
loFormSet.laFileStru[lnFileStru+7,2] = 'C'
loFormSet.laFileStru[lnFileStru+7,3] = 7
loFormSet.laFileStru[lnFileStru+7,4] = 0

loFormSet.laFileStru[lnFileStru+8,1] = 'PATTERN'
loFormSet.laFileStru[lnFileStru+8,2] = 'C'
loFormSet.laFileStru[lnFileStru+8,3] = 10
loFormSet.laFileStru[lnFileStru+8,4] = 0

loFormSet.laFileStru[lnFileStru+9,1] = 'CSTYMAJOR'
loFormSet.laFileStru[lnFileStru+9,2] = 'C'
loFormSet.laFileStru[lnFileStru+9,3] = 19
loFormSet.laFileStru[lnFileStru+9,4] = 0

loFormSet.laFileStru[lnFileStru+10,1] = 'CSORTFIELD'
loFormSet.laFileStru[lnFileStru+10,2] = 'C'
loFormSet.laFileStru[lnFileStru+10,3] = 80
loFormSet.laFileStru[lnFileStru+10,4] = 0

*-- if MF or PO modules is installed
IF 'MF' $ oAriaApplication.CompanyInstalledModules .OR. 'PO' $ oAriaApplication.CompanyInstalledModules
  loFormSet.laFileStru[lnFileStru+11,1] = 'TRANCD'
  loFormSet.laFileStru[lnFileStru+11,2] = 'C'
  loFormSet.laFileStru[lnFileStru+11,3] = 1
  loFormSet.laFileStru[lnFileStru+11,4] = 0

  loFormSet.laFileStru[lnFileStru+12,1] = 'CTKTNO'
  loFormSet.laFileStru[lnFileStru+12,2] = 'C'
  loFormSet.laFileStru[lnFileStru+12,3] = 6
  loFormSet.laFileStru[lnFileStru+12,4] = 0
ENDIF

*-- if system use dyelots add fabric color field.
IF loFormSet.llUseDyes
  loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),1] = 'CFABCOLOR'
  loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),2] = 'C'
  loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),3] = 6
  loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),4] = 0
ENDIF

lnElmnt = lnAddToArr

FOR lnCount = 1 TO lnElmnt
  STORE '' TO loFormSet.laFileStru[lnFileStru+lnCount,7],loFormSet.laFileStru[lnFileStru+lnCount,8],loFormSet.laFileStru[lnFileStru+lnCount,9],;
    loFormSet.laFileStru[lnFileStru+lnCount,10],loFormSet.laFileStru[lnFileStru+lnCount,11],loFormSet.laFileStru[lnFileStru+lnCount,12],;
    loFormSet.laFileStru[lnFileStru+lnCount,13],loFormSet.laFileStru[lnFileStru+lnCount,14],loFormSet.laFileStru[lnFileStru+lnCount,15],;
    loFormSet.laFileStru[lnFileStru+lnCount,16]
  STORE 0  TO loFormSet.laFileStru[lnFileStru+lnCount,17],  loFormSet.laFileStru[lnFileStru+lnCount,18]
ENDFOR

*-- Add 8 ExcCut fields to end of this file structure and TotExcCut field.
DIMENSION loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1)+9,ALEN(loFormSet.laFileStru,2)]
loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),1] = 'TOTEXCCUT'
loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),2] = 'N'
loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),3] = 7
loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),4] = 0
STORE '' TO loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),7],loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),8],loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),9],;
  loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),10],loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),11],loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),12],;
  loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),13],loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),14],loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),15],;
  loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),16]
STORE 0  TO loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),17],  loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),18]


lnThArrPos = ALEN(loFormSet.laFileStru,1) - 9
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  loFormSet.laFileStru[lnThArrPos+lnI,1] = 'EXCCUT'+lcI
  loFormSet.laFileStru[lnThArrPos+lnI,2] = 'N'
  loFormSet.laFileStru[lnThArrPos+lnI,3] = 6
  loFormSet.laFileStru[lnThArrPos+lnI,4] = 0
  STORE '' TO loFormSet.laFileStru[lnThArrPos+lnI,7],loFormSet.laFileStru[lnThArrPos+lnI,8],loFormSet.laFileStru[lnThArrPos+lnI,9],;
    loFormSet.laFileStru[lnThArrPos+lnI,10],loFormSet.laFileStru[lnThArrPos+lnI,11],loFormSet.laFileStru[lnThArrPos+lnI,12],;
    loFormSet.laFileStru[lnThArrPos+lnI,13],loFormSet.laFileStru[lnThArrPos+lnI,14],loFormSet.laFileStru[lnThArrPos+lnI,15],;
    loFormSet.laFileStru[lnThArrPos+lnI,16]
  STORE 0  TO loFormSet.laFileStru[lnThArrPos+lnI,17],  loFormSet.laFileStru[lnThArrPos+lnI,18]
ENDFOR

*-- Add Totwip field to end of this file structure.
DIMENSION loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1)+1,ALEN(loFormSet.laFileStru,2)]
loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),1] = 'TOTWIP'
loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),2] = 'N'
loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),3] = 7
loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),4] = 0
STORE '' TO loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),7],loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),8],loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),9],;
  loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),10],loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),11],loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),12],;
  loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),13],loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),14],loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),15],;
  loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),16]
STORE 0  TO loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),17],  loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1),18]

lnFileStru = ALEN(loFormSet.laFileStru,1)
DIMENSION loFormSet.laFileStru[ALEN(loFormSet.laFileStru,1)+1,ALEN(loFormSet.laFileStru,2)]
loFormSet.laFileStru[lnFileStru +1,1] = 'cPeggedDye'
loFormSet.laFileStru[lnFileStru +1,2] = 'C'
loFormSet.laFileStru[lnFileStru +1,3] = 10
loFormSet.laFileStru[lnFileStru +1,4] = 0
STORE '' TO loFormSet.laFileStru[lnFileStru +1,7],loFormSet.laFileStru[lnFileStru +1,8],loFormSet.laFileStru[lnFileStru +1,9],;
  loFormSet.laFileStru[lnFileStru +1,10],loFormSet.laFileStru[lnFileStru +1,11],loFormSet.laFileStru[lnFileStru +1,12],;
  loFormSet.laFileStru[lnFileStru +1,13],loFormSet.laFileStru[lnFileStru +1,14],loFormSet.laFileStru[lnFileStru +1,15],;
  loFormSet.laFileStru[lnFileStru +1,16]
STORE 0  TO loFormSet.laFileStru[lnFileStru +1,17],  loFormSet.laFileStru[lnFileStru +1,18]

lnFileStru = ALEN(loFormSet.laFileStru,1)
DIMENSION loFormSet.laFileStru[lnFileStru + 9,ALEN(loFormSet.laFileStru,2)]
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  loFormSet.laFileStru[lnFileStru +lnI,1] = 'Alo'+lcI
  loFormSet.laFileStru[lnFileStru +lnI,2] = 'N'
  loFormSet.laFileStru[lnFileStru +lnI,3] = 5
  loFormSet.laFileStru[lnFileStru +lnI,4] = 0
  STORE '' TO loFormSet.laFileStru[lnFileStru +lnI,7],loFormSet.laFileStru[lnFileStru +lnI,8],loFormSet.laFileStru[lnFileStru +lnI,9],;
    loFormSet.laFileStru[lnFileStru +lnI,10],loFormSet.laFileStru[lnFileStru +lnI,11],loFormSet.laFileStru[lnFileStru +lnI,12],;
    loFormSet.laFileStru[lnFileStru +lnI,13],loFormSet.laFileStru[lnFileStru +lnI,14],loFormSet.laFileStru[lnFileStru +lnI,15],;
    loFormSet.laFileStru[lnFileStru +lnI,16]
  STORE 0  TO loFormSet.laFileStru[lnFileStru +lnI,17],  loFormSet.laFileStru[lnFileStru +lnI,18]
ENDFOR
loFormSet.laFileStru[lnFileStru +9,1] = 'TotAlo'
loFormSet.laFileStru[lnFileStru +9,2] = 'N'
loFormSet.laFileStru[lnFileStru +9,3] = 6
loFormSet.laFileStru[lnFileStru +9,4] = 0
STORE '' TO loFormSet.laFileStru[lnFileStru +9,7],loFormSet.laFileStru[lnFileStru  +9,8],loFormSet.laFileStru[lnFileStru  +9,9],;
  loFormSet.laFileStru[lnFileStru +9,10],loFormSet.laFileStru[lnFileStru +9,11],loFormSet.laFileStru[lnFileStru +9,12],;
  loFormSet.laFileStru[lnFileStru +9,13],loFormSet.laFileStru[lnFileStru +9,14],loFormSet.laFileStru[lnFileStru +9,15],;
  loFormSet.laFileStru[lnFileStru +9,16]
STORE 0  TO loFormSet.laFileStru[lnFileStru +9,17],  loFormSet.laFileStru[lnFileStru +9,18]

lnFileStru = ALEN(loFormSet.laFileStru,1)
DIMENSION loFormSet.laFileStru[lnFileStru + 9,ALEN(loFormSet.laFileStru,2)]
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  loFormSet.laFileStru[lnFileStru +lnI,1] = 'Avl'+lcI
  loFormSet.laFileStru[lnFileStru +lnI,2] = 'N'
  *B608118,1 WAM 06/10/2007 Increase he width of the available fields
  *loFormSet.laFileStru[lnFileStru +lnI,3] = 5
  loFormSet.laFileStru[lnFileStru +lnI,3] = 6
  *B608118,1 WAM 06/10/2007 (End)
  loFormSet.laFileStru[lnFileStru +lnI,4] = 0
  STORE '' TO loFormSet.laFileStru[lnFileStru +lnI,7],loFormSet.laFileStru[lnFileStru +lnI,8],loFormSet.laFileStru[lnFileStru +lnI,9],;
    loFormSet.laFileStru[lnFileStru +lnI,10],loFormSet.laFileStru[lnFileStru +lnI,11],loFormSet.laFileStru[lnFileStru +lnI,12],;
    loFormSet.laFileStru[lnFileStru +lnI,13],loFormSet.laFileStru[lnFileStru +lnI,14],loFormSet.laFileStru[lnFileStru +lnI,15],;
    loFormSet.laFileStru[lnFileStru +lnI,16]
  STORE 0  TO loFormSet.laFileStru[lnFileStru +lnI,17],  loFormSet.laFileStru[lnFileStru +lnI,18]
ENDFOR
loFormSet.laFileStru[lnFileStru +9,1] = 'TotAvl'
loFormSet.laFileStru[lnFileStru +9,2] = 'N'
*B608118,1 WAM 06/10/2007 Increase he width of the available fields
*loFormSet.laFileStru[lnFileStru +9,3] = 6
loFormSet.laFileStru[lnFileStru +9,3] = 7
*B608118,1 WAM 06/10/2007 (End)
loFormSet.laFileStru[lnFileStru +9,4] = 0
STORE '' TO loFormSet.laFileStru[lnFileStru +9,7],loFormSet.laFileStru[lnFileStru  +9,8],loFormSet.laFileStru[lnFileStru  +9,9],;
  loFormSet.laFileStru[lnFileStru +9,10],loFormSet.laFileStru[lnFileStru +9,11],loFormSet.laFileStru[lnFileStru +9,12],;
  loFormSet.laFileStru[lnFileStru +9,13],loFormSet.laFileStru[lnFileStru +9,14],loFormSet.laFileStru[lnFileStru +9,15],;
  loFormSet.laFileStru[lnFileStru +9,16]
STORE 0  TO loFormSet.laFileStru[lnFileStru +9,17],  loFormSet.laFileStru[lnFileStru +9,18]

lnFileStru = ALEN(loFormSet.laFileStru,1) + 1
DIMENSION loFormSet.laFileStru[lnFileStru,ALEN(loFormSet.laFileStru,2)]
loFormSet.laFileStru[lnFileStru ,1] = 'cReason'
loFormSet.laFileStru[lnFileStru ,2] = 'C'
loFormSet.laFileStru[lnFileStru ,3] = 100
loFormSet.laFileStru[lnFileStru ,4] = 0
STORE '' TO loFormSet.laFileStru[lnFileStru ,7],loFormSet.laFileStru[lnFileStru ,8],loFormSet.laFileStru[lnFileStru ,9],;
  loFormSet.laFileStru[lnFileStru ,10],loFormSet.laFileStru[lnFileStru ,11],loFormSet.laFileStru[lnFileStru ,12],;
  loFormSet.laFileStru[lnFileStru ,13],loFormSet.laFileStru[lnFileStru ,14],loFormSet.laFileStru[lnFileStru ,15],;
  loFormSet.laFileStru[lnFileStru ,16]
STORE 0  TO loFormSet.laFileStru[lnFileStru ,17],  loFormSet.laFileStru[lnFileStru ,18]


lnFileStru = ALEN(loFormSet.laFileStru,1) + 1
DIMENSION loFormSet.laFileStru[lnFileStru,ALEN(loFormSet.laFileStru,2)]
loFormSet.laFileStru[lnFileStru ,1] = 'cPosition'
loFormSet.laFileStru[lnFileStru ,2] = 'C'
loFormSet.laFileStru[lnFileStru ,3] = 1
loFormSet.laFileStru[lnFileStru ,4] = 0
STORE '' TO loFormSet.laFileStru[lnFileStru ,7],loFormSet.laFileStru[lnFileStru ,8],loFormSet.laFileStru[lnFileStru ,9],;
  loFormSet.laFileStru[lnFileStru ,10],loFormSet.laFileStru[lnFileStru ,11],loFormSet.laFileStru[lnFileStru ,12],;
  loFormSet.laFileStru[lnFileStru ,13],loFormSet.laFileStru[lnFileStru ,14],loFormSet.laFileStru[lnFileStru ,15],;
  loFormSet.laFileStru[lnFileStru ,16]
STORE 0  TO loFormSet.laFileStru[lnFileStru ,17],  loFormSet.laFileStru[lnFileStru ,18]

DIMENSION laTmpIndex[3,2]
laTmpIndex[1,1] = 'cordtype+order+STR(lineno,6)'
laTmpIndex[1,2] = loFormSet.lcTmpIndex
laTmpIndex[2,1] = 'Style+DTOS(complete)+cordtype+order+store+STR(lineno,6)'
laTmpIndex[2,2] = loFormSet.lcTmStyTag
laTmpIndex[3,1] = 'cSortField'
laTmpIndex[3,2] = loFormSet.lcTmpOrdLn

=ACOPY(loFormSet.laFileStru,laFileStru)

=gfCrtTmp(loFormSet.lcTmpOrdLn,@laFileStru,@laTmpIndex)
*!*	CREATE CURSOR (loFormSet.lcTmpOrdLn) FROM ARRAY laFileStru
*!*	FOR lnI = 1 TO ALEN(laTmpIndex,1)
*!*	  INDEX ON &laTmpIndex[lnI,1] TAG &laTmpIndex[lnI,2] ADDITIVE
*!*	ENDFOR

=ACOPY(laFileStru,loFormSet.laFileStru)


IF loFormSet.llChkAprov
  CREATE CURSOR (loFormSet.lcTmpOrdAp) (TYPE C(1),ORDER C(6),AprAmnt N(13,2),TotQty N(7),lExceed L(1))
  INDEX ON TYPE + ORDER TAG (loFormSet.lcTmpOrdAp) ADDITIVE
  SET ORDER TO (loFormSet.lcTmpOrdAp)
ENDIF

*!*************************************************************
*! Name      : lfOpnFiles
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : open Files needed in the program
*!*************************************************************
*! Called from : ALAUTAL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfOpnFiles
PARAMETERS loFormSet

=gfOpenFile(oAriaApplication.DataDir+"ORDHDR",oAriaApplication.DataDir+"ORDHDR", "SH")
=gfOpenFile(oAriaApplication.DataDir+"ORDLINE",oAriaApplication.DataDir+"ORDLINE", "SH")
=gfOpenFile(oAriaApplication.DataDir+"STYLE",oAriaApplication.DataDir+"STYLE", "SH")
=gfOpenFile(oAriaApplication.DataDir+"SCALE",oAriaApplication.DataDir+"SCALE", "SH")
=gfOpenFile(oAriaApplication.DataDir+"STYDYE",oAriaApplication.DataDir+"STYDYE", "SH")
=gfOpenFile(oAriaApplication.DataDir+"CUSTOMER",oAriaApplication.DataDir+"CUSTOMER", "SH")
=gfOpenFile(oAriaApplication.DataDir+"PIKTKT",oAriaApplication.DataDir+"PIKTKT", "SH")
=gfOpenFile(oAriaApplication.DataDir+"WAREHOUS","", "SH")
IF 'PO' $ oAriaApplication.CompanyInstalledModules
  =gfOpenFile(oAriaApplication.DataDir+"APVENDOR","","SH")
ENDIF

*B608316,1 MMT 10/11/2007 convert 3PL Provider Enhamancemnt from 27[Start]
IF 'AS' $ oAriaApplication.CompanyInstalledModules
  =gfOpenFile(oAriaApplication.DataDir+'EDIACPRT',oAriaApplication.DataDir+'ACCFACT','SH')
  =gfOpenFile(oAriaApplication.DataDir+'EDIPD',oAriaApplication.DataDir+'PARTTRANS','SH')
  =gfOpenFile(oAriaApplication.DataDir+'EDITRANS',oAriaApplication.DataDir+'TYPEKEY','SH')
ENDIF
*B608316,1 MMT 10/11/2007 convert 3PL Provider Enhamancemnt from 27[End]

RETURN .T.
*!*************************************************************
*! Name      : lfOpnSqlFl
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : open SQL Files needed in the program
*!*************************************************************
*! Called from : ALAUTAL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfOpnSqlFl
PARAMETERS loFormSet

IF 'MA' $ oAriaApplication.CompanyInstalledModules
  =lfopensql(loFormSet,'DYE_REL','DYE_REL1')
ENDIF
IF 'MF' $ oAriaApplication.CompanyInstalledModules OR 'PO' $ oAriaApplication.CompanyInstalledModules
  llOpenSQLPo = lfopensql(loFormSet,'POSHDR','POSHDR1')
  RETURN llOpenSQLPo
ENDIF

RETURN .T.

*!*************************************************************
*! Name      : lfClsFiles
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Close Files opened in the program
*!*************************************************************
*! Called from : ALAUTAL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfClsFiles

IF USED("ORDHDR")
  USE IN ORDHDR
ENDIF
IF USED("ORDLINE")
  USE IN ORDLINE
ENDIF
IF USED("STYLE")
  USE IN STYLE
ENDIF
IF USED("SCALE")
  USE IN SCALE
ENDIF
IF USED("STYDYE")
  USE IN STYDYE
ENDIF
IF USED("CUSTOMER")
  USE IN CUSTOMER
ENDIF
IF USED("PIKTKT")
  USE IN PIKTKT
ENDIF
IF USED("WAREHOUS")
  USE IN WAREHOUS
ENDIF

IF 'MA' $ oAriaApplication.CompanyInstalledModules
  IF USED("ITEM1")
    USE IN ITEM1
  ENDIF
  IF USED("DYE_REL1")
    USE IN DYE_REL1
  ENDIF
  IF USED("BOM1")
    USE IN BOM1
  ENDIF
ENDIF
IF 'PO' $ oAriaApplication.CompanyInstalledModules
  IF USED("APVENDOR")
    USE IN APVENDOR
  ENDIF
ENDIF
IF 'MF' $ oAriaApplication.CompanyInstalledModules OR 'PO' $ oAriaApplication.CompanyInstalledModules
  IF USED("CUTPICK1")
    USE IN CUTPICK1
  ENDIF
  IF USED("POSHDR1")
    USE IN POSHDR1
  ENDIF
  IF USED("POSLN1")
    USE IN POSLN1
  ENDIF
ENDIF


*!*************************************************************
*! Name      : lfOpenSql
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : function to open SQL tables
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfopensql
LPARAMETERS loFormSet,lcTable,lcCursor,lcWhereCond,llIsInitial

LOCAL lnConnectionHandlar, lnBuffering, lcSqlStatment , loSqlConnection
PRIVATE laIndex
DIMENSION laIndex[1,2]

lcSqlStatment   = "SELECT * FROM " + lcTable + IIF(TYPE('lcWhereCond') = 'C' AND !EMPTY(lcWhereCond)," WHERE " + lcWhereCond ,"")
loSqlConnection = CREATEOBJECT('remotedataaccess')

lnConnectionHandlar = loSqlConnection.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
  'SAVE',loFormSet.DATASESSIONID)
IF lnConnectionHandlar = 1
  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
  =CURSORSETPROP("Buffering",3,lcCursor)
  *-- To initialize the indecis that will be created for each file
  =lfCrtindex(loFormSet,lcTable)
  SELECT (lcCursor)
  FOR lnI = 1 TO ALEN(laIndex,1)
    lcIndex = laIndex[lnI,1]
    lcTag   = laIndex[lnI,2]
    *B609356,1 SMA 07/25/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
    *INDEX ON &lcIndex. TAG (lcTag) OF (lcCursor)
    INDEX ON &lcIndex. TAG (lcTag)
    *B609356,1 SMA 07/25/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
  ENDFOR
  lcTag = laIndex[1,2]
  SET ORDER TO TAG (lcTag)
  =CURSORSETPROP("Buffering",lnBuffering,lcCursor)
  IF llIsInitial
    loFormSet.DATAENVIRONMENT.INITIALSELECTEDALIAS = lcCursor
  ENDIF
  loSqlConnection = NULL
ELSE
  =loSqlConnection.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  loSqlConnection = NULL
  RETURN .F.
ENDIF
*-- end of lfOpenSql.

*!*************************************************************
*! Name      : lfCrtindex
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : function to Set the index for the SQL files
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfCrtindex
LPARAMETERS loFormSet,lcTable

DO CASE
CASE UPPER(lcTable) = "POSHDR"
  DIMENSION laIndex[1,2]
  laIndex[1,1] = 'CBUSDOCU+CSTYTYPE+PO'
  laIndex[1,2] = 'POSHDR'

CASE UPPER(lcTable) = "POSLN"
  DIMENSION laIndex[2,2]
  laIndex[1,1] = 'CBUSDOCU+CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD'
  laIndex[1,2] = 'POSLN'
  laIndex[2,1] = 'SHIPNO+CSTYTYPE+CBUSDOCU+PO+STYLE+STR(LINENO,6)+TRANCD'
  laIndex[2,2] = 'POSLNSH'

CASE UPPER(lcTable) = "CUTPICK"
  DIMENSION laIndex[2,2]
  laIndex[1,1] = 'TRANCD+CTKTNO+STYLE'
  laIndex[1,2] = 'Cutpick'
  laIndex[2,1] = 'TRANCD+ORDER+CORDLINE'
  laIndex[2,2] = 'Cutord'

CASE UPPER(lcTable) = "SHPMTHDR"
  DIMENSION laIndex[1,2]
  laIndex[1,1] = 'CBUSDOCU+CSHPTYPE+SHIPNO'
  laIndex[1,2] = 'SHPMTHDR'

CASE UPPER(lcTable) = "ITEM"
  DIMENSION laIndex[5,2]
  laIndex[1,1] = 'CINVTYPE+STYLE'
  laIndex[1,2] = 'STYLE'
  laIndex[2,1] = 'PATTERN+CINVTYPE+STYLE FOR .NOT. EMPTY(PATTERN)'
  laIndex[2,2] = 'STYLEPAT'
  laIndex[3,1] = 'CINVTYPE+CSTYMAJOR'
  laIndex[3,2] = 'CSTYLE'
  laIndex[4,1] = 'CSTYGRADE+CINVTYPE+STYLE'
  laIndex[4,2] = 'STYQLTY'
  laIndex[5,1] = 'CINVTYPE+STATUS+CSTYGROUP'
  laIndex[5,2] = 'CRM'

CASE UPPER(lcTable) = "DYE_REL"
  DIMENSION laIndex[3,2]
  laIndex[1,1] = 'CINVTYPE+CITEM+DYELOT'
  laIndex[1,2] = 'DYE_REL'
  laIndex[2,1] = 'CINVTYPE+CITEM+CDYE_SEQ'
  laIndex[2,2] = 'SEQUENCE'
  laIndex[3,1] = 'CTMPSCOPE'
  laIndex[3,2] = 'SCOPE'

CASE UPPER(lcTable) = "BOM"
  DIMENSION laIndex[2,2]
  laIndex[1,1] = 'CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6)'
  laIndex[1,2] = 'MULTIBOM'
  laIndex[2,1] = 'TYP+CINVTYPC+ITEM+CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+CITMMASK'
  laIndex[2,2] = 'MBOMITEM'

ENDCASE

*!*************************************************************
*! Name      : lfUpdatSQL
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : open SQL Files needed in the program
*!*************************************************************
*! Called from : ALAUTAL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfUpdatSQL
LPARAMETERS loFormSet,lcTable,LcPrimaryKeyList,lcSQLTable

LOCAL lnConnectionHandlar, lcTranCode,llReturn

llReturn   = .T.

loSqlConnection = CREATEOBJECT('remotedataaccess')

lcTranCode = loSqlConnection.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')

IF TYPE('lcTranCode') = 'N'
  =loSqlConnection.CheckRetResult("BeginTran",lcTranCode,.T.)
  *B610997,1 MMT 04/28/2015 Error in Automatic allocation screen at DCC[T20150424.0001][Start]
*!*	  IF BETWEEN(lnRecNo,1,RECCOUNT(lcTable))
*!*	    GOTO lnRecNo IN (lcTable)
*!*	  ENDIF
  *B610997,1 MMT 04/28/2015 Error in Automatic allocation screen at DCC[T20150424.0001][end]
  loSqlConnection = NULL
  RETURN .F.
ENDIF

lnConnectionHandlar = loSqlConnection.sqlupdate(lcTable,lcTranCode,loFormSet.DATASESSIONID,LcPrimaryKeyList,lcSQLTable)
IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
  =loSqlConnection.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
  llReturn = .F.
ENDIF

lnConnectionHandlar = loSqlConnection.CommitTran(lcTranCode)
IF lnConnectionHandlar # 1
  =loSqlConnection.CheckRetResult("CommitTran",lnConnectionHandlar,.T.)
  llReturn = .F.
ENDIF

loSqlConnection = NULL
RETURN llReturn

*!*************************************************************
*! Name      : lfBundBrow
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Bound columns of the Orders grid
*!*************************************************************
*! Called from : ALAUTAL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfBundBrow
PARAMETERS loFormSet

WITH loFormSet.AriaForm1.grdOrders.grdMultiSelectionGrid
  .RECORDSOURCE = ''
  .RECORDSOURCE = loFormSet.lcTmpOrdLn

  *-- Build browse columns
  .COLUMNS(1).Header1.CAPTION  = ""
  .COLUMNS(1).CURRENTCONTROL   = "AriaCheckBox1"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.Columns(2).Header1.Caption  = LANG_AutoAlloc_LabelOrder
  .COLUMNS(2).Header1.CAPTION  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LabelOrder,loFormSet.GetHeaderText("LANG_AutoAlloc_LabelOrder",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .COLUMNS(2).CURRENTCONTROL = "Text1"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.Columns(3).Header1.Caption  = LANG_AutoAlloc_LabelAccount
  .COLUMNS(3).Header1.CAPTION  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LabelAccount,loFormSet.GetHeaderText("LANG_AutoAlloc_LabelAccount",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .COLUMNS(3).CURRENTCONTROL = "Text1"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.Columns(4).Header1.Caption  = LANG_AutoAlloc_LabelStore
  .COLUMNS(4).Header1.CAPTION  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LabelStore,loFormSet.GetHeaderText("LANG_AutoAlloc_LabelStore",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .COLUMNS(4).CURRENTCONTROL = "Text1"
  .COLUMNS(5).Header1.CAPTION  = loFormSet.lcStyleTtl
  .COLUMNS(5).CURRENTCONTROL = "Text1"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.Columns(6).Header1.Caption  = IIF(loFormSet.lluseConfg,LANG_AutoAlloc_LabelConfg,LANG_AutoAlloc_LabelDyelot)
  .COLUMNS(6).Header1.CAPTION  = IIF(loFormSet.llUseConfg,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LabelConfg,loFormSet.GetHeaderText("LANG_AutoAlloc_LabelConfg",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LabelDyelot,loFormSet.GetHeaderText("LANG_AutoAlloc_LabelDyelot",loFormSet.HeaderAlias)))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .COLUMNS(6).CURRENTCONTROL = "Text1"
  .COLUMNS(6).VISIBLE		   = loFormSet.llUseDyes
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.Columns(7).Header1.Caption  = LANG_AutoAlloc_LabelGroup
  .COLUMNS(7).Header1.CAPTION  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LabelGroup,loFormSet.GetHeaderText("LANG_AutoAlloc_LabelGroup",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .COLUMNS(7).CURRENTCONTROL = "Text1"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.Columns(8).Header1.Caption  = LANG_AutoAlloc_LabelAvialable
  .COLUMNS(8).Header1.CAPTION  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LabelAvialable,loFormSet.GetHeaderText("LANG_AutoAlloc_LabelAvialable",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .COLUMNS(8).CURRENTCONTROL = "Text1"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.Columns(9).Header1.Caption  = LANG_AutoAlloc_LabelOpen
  .COLUMNS(9).Header1.CAPTION  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LabelOpen,loFormSet.GetHeaderText("LANG_AutoAlloc_LabelOpen",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .COLUMNS(9).CURRENTCONTROL = "Text1"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.Columns(10).Header1.Caption  = LANG_AutoAlloc_LabelOpnAmnt
  .COLUMNS(10).Header1.CAPTION  = LANG_AutoAlloc_LabelOpnAmnt
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .COLUMNS(10).CURRENTCONTROL = "Text1"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.Columns(11).Header1.Caption = LANG_AutoAlloc_LabelPiktkt
  .COLUMNS(11).Header1.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LabelPiktkt,loFormSet.GetHeaderText("LANG_AutoAlloc_LabelPiktkt",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .COLUMNS(11).CURRENTCONTROL = "Text1"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.Columns(12).Header1.Caption = LANG_AutoAlloc_LabelPicked
  .COLUMNS(12).Header1.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LabelPicked,loFormSet.GetHeaderText("LANG_AutoAlloc_LabelPicked",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .COLUMNS(12).CURRENTCONTROL = "Text1"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.Columns(13).Header1.Caption = LANG_AutoAlloc_LabelPikAmnt
  .COLUMNS(13).Header1.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LabelPikAmnt,loFormSet.GetHeaderText("LANG_AutoAlloc_LabelPikAmnt",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .COLUMNS(13).CURRENTCONTROL = "Text1"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.Columns(14).Header1.Caption = LANG_AutoAlloc_LabelReasone
  .COLUMNS(14).Header1.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LabelReasone,loFormSet.GetHeaderText("LANG_AutoAlloc_LabelReasone",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .COLUMNS(14).CURRENTCONTROL = "Text1"
  .SETALL('READONLY',.T.,'COLUMN')

  IF loFormSet.activemode = 'S'
    .COLUMNS(1).CONTROLSOURCE  = 0
    .COLUMNS(2).CONTROLSOURCE  = ""
    .COLUMNS(3).CONTROLSOURCE  = ""
    .COLUMNS(4).CONTROLSOURCE  = ""
    .COLUMNS(5).CONTROLSOURCE  = ""
    .COLUMNS(6).CONTROLSOURCE  = ""
    .COLUMNS(7).CONTROLSOURCE  = ""
    .COLUMNS(8).CONTROLSOURCE  = ""
    .COLUMNS(9).CONTROLSOURCE  = ""
    .COLUMNS(10).CONTROLSOURCE = ""
    .COLUMNS(11).CONTROLSOURCE = ""
    .COLUMNS(12).CONTROLSOURCE = ""
    .COLUMNS(13).CONTROLSOURCE = ""
    .COLUMNS(14).CONTROLSOURCE = ""
  ELSE
    .COLUMNS(1).CONTROLSOURCE  = loFormSet.lcTmpOrdLn+'.lnSel'
    .COLUMNS(2).CONTROLSOURCE  = loFormSet.lcTmpOrdLn+'.ORDER'
    .COLUMNS(3).CONTROLSOURCE  = loFormSet.lcTmpOrdLn+'.ACCOUNT'
    .COLUMNS(4).CONTROLSOURCE  = loFormSet.lcTmpOrdLn+'.STORE'
    .COLUMNS(5).CONTROLSOURCE  = loFormSet.lcTmpOrdLn+'.STYLE'
    .COLUMNS(6).CONTROLSOURCE  = loFormSet.lcTmpOrdLn+'.DYELOT'
    .COLUMNS(7).CONTROLSOURCE  = loFormSet.lcTmpOrdLn+'.GROUP'

    *! B610516,1 HIA 09/15/13 T20130910.0026 - Automatic Allocation - Available column is not shown [Begin]
    *.COLUMNS(8).CONTROLSOURCE  = MAX(0, STYDYE.TOTSTK - STYDYE.TOTALO)
    *B610534,1 MMT 09/29/2013 Modify Automatic allocation screen to include WIP in Avail. based on setup{T20130910.0026}[Start]
    *.COLUMNS(8).CONTROLSOURCE  = 'MAX(0, STYDYE.TOTSTK - STYDYE.TOTALO)'
    .COLUMNS(8).CONTROLSOURCE  = 'MAX(0,IIF(THISFormSet.llTotAvlbl,STYDYE.TOTWIP,0)+STYDYE.TOTSTK - STYDYE.TOTALO)'
    *B610534,1 MMT 09/29/2013 Modify Automatic allocation screen to include WIP in Avail. based on setup{T20130910.0026}[End]
    *! B610516,1 HIA 09/15/13 T20130910.0026 - Automatic Allocation - Available column is not shown [End]

    .COLUMNS(9).CONTROLSOURCE  = loFormSet.lcTmpOrdLn+'.TOTQTY'
    *:   B609735,1 MMT 11/20/2011 Allocation screen has fields for Open Amnt and Pik Amnt but no data appears[Start]
    *.Columns(10).ControlSource = EVALUATE(loFormSet.lcTmpOrdLn+'.TOTQTY')*EVALUATE(loFormSet.lcTmpOrdLn+'.PRICE')
    .COLUMNS(10).CONTROLSOURCE = loFormSet.lcTmpOrdLn+'.TOTQTY*'+loFormSet.lcTmpOrdLn+'.PRICE'
    *:   B609735,1 MMT 11/20/2011 Allocation screen has fields for Open Amnt and Pik Amnt but no data appears[End]
    .COLUMNS(11).CONTROLSOURCE = loFormSet.lcTmpOrdLn+'.PIKTKT'
    .COLUMNS(12).CONTROLSOURCE = loFormSet.lcTmpOrdLn+'.TOTPIK'
    *:   B609735,1 MMT 11/20/2011 Allocation screen has fields for Open Amnt and Pik Amnt but no data appears[Start]
    *.Columns(13).ControlSource = EVALUATE(loFormSet.lcTmpOrdLn+'.TOTPIK')*EVALUATE(loFormSet.lcTmpOrdLn+'.PRICE')
    .COLUMNS(13).CONTROLSOURCE = loFormSet.lcTmpOrdLn+'.TOTPIK*'+loFormSet.lcTmpOrdLn+'.PRICE'
    *:   B609735,1 MMT 11/20/2011 Allocation screen has fields for Open Amnt and Pik Amnt but no data appears[End]
    .COLUMNS(14).CONTROLSOURCE = loFormSet.lcTmpOrdLn+'.cReason'
  ENDIF
  .REFRESH
ENDWITH

*!*************************************************************
*! Name      : lfTolBrShw
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Control Enabling & Disabling Tool bar buttons
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfTolBrShw
PARAMETERS loFormSet

*HBG 1/24/2005 Modify code to apply the new interface [Begin]
*!*	oAriaApplication.oToolBar.cmdEdit.Enabled   = loFormSet.llCtrStat7
*!*	oAriaApplication.oToolBar.cmdDelete.Enabled = loFormSet.llCtrStat8
*!*	oAriaApplication.oToolBar.cmdSelect.Enabled = loFormSet.llCtrStat9
*!*	oAriaApplication.oToolBar.cmdFind.Enabled   = loFormSet.llCtrStat10
loFormSet.oToolBar.cmdEdit.ENABLED   = loFormSet.llCtrStat7
loFormSet.oToolBar.cmdDelete.ENABLED = loFormSet.llCtrStat8
loFormSet.oToolBar.cmdSelect.ENABLED = loFormSet.llCtrStat9
loFormSet.oToolBar.cmdFind.ENABLED   = loFormSet.llCtrStat10
*HBG [End]

*!*************************************************************
*! Name      : lfPrntShw
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Control Enabling & Disabling Print buttons in Tool bar
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfPrntShw
PARAMETERS loFormSet

*HBG 1/24/2005 Modify code to apply the new interface [Begin]
*oAriaApplication.oToolBar.cmdprint.Enabled  = loFormSet.llCtrStat6
loFormSet.oToolBar.cmdprint.ENABLED  = loFormSet.llCtrStat6
*HBG [End]


*!*************************************************************
*! Name      : lfNavShw
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Control Enabling & Disabling navigation buttons in Tool bar
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfNavShw
PARAMETERS loFormSet

*HBG 1/24/2005 Modify code to apply the new interface [Begin]
*!*	oAriaApplication.oToolBar.cmdTop.Enabled    = loFormSet.llCtrStat1
*!*	oAriaApplication.oToolBar.cmdprev.Enabled   = loFormSet.llCtrStat4
*!*	oAriaApplication.oToolBar.cmdend.Enabled    = loFormSet.llCtrStat2
*!*	oAriaApplication.oToolBar.cmdnext.Enabled   = loFormSet.llCtrStat3
loFormSet.oToolBar.cmdTop.ENABLED    = loFormSet.llCtrStat1
loFormSet.oToolBar.cmdprev.ENABLED   = loFormSet.llCtrStat4
loFormSet.oToolBar.cmdend.ENABLED    = loFormSet.llCtrStat2
loFormSet.oToolBar.cmdnext.ENABLED   = loFormSet.llCtrStat3
*HBG [End]

*!*************************************************************
*! Name      : lfBundFlds
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Bound Objects on the screen
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfBundFlds
PARAMETERS loFormSet


=SEEK(EVALUATE(loFormSet.lcTmpOrdLn+'.Style'), 'STYLE')
=SEEK(EVALUATE(loFormSet.lcTmpOrdLn+'.Style')+EVALUATE(loFormSet.lcTmpOrdLn+'.cWareCode')+EVALUATE(loFormSet.lcTmpOrdLn+'.Dyelot'), 'STYDYE')
=SEEK('S' + STYLE.SCALE , 'SCALE')

SELECT (loFormSet.lcTmpOrdLn)
SCATTER FIELDS LIKE PIK* TO laPik
SCATTER FIELDS LIKE QTY* TO laQty

WITH loFormSet.AriaForm1.cntDetail
  .keyStyle.VALUE = EVALUATE(loFormSet.lcTmpOrdLn+'.Style')
  .txtDesc.VALUE  = STYLE.Desc1
  .keyDyelot.keytextbox.VALUE = EVALUATE(loFormSet.lcTmpOrdLn+'.Dyelot')
  .txtGroup.VALUE = EVALUATE(loFormSet.lcTmpOrdLn+'.Group')
  .txtstore.VALUE = EVALUATE(loFormSet.lcTmpOrdLn+'.Store')
  lcWareCode = EVALUATE(loFormSet.lcTmpOrdLn+'.cWareCode')
  lnElem     = ASCAN(loFormSet.laWareHous,lcWareCode)
  .cboLocation.VALUE = IIF(lnElem <> 0, ASUBSCRIPT(loFormSet.laWareHous, lnElem, 1),.cboLocation.VALUE)

  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    .sizesbreak1.txtsize&lcI..VALUE = loFormSet.laSizes[lnI]
  ENDFOR

  FOR lnI = 1 TO SCALE.CNT
    lcI = STR(lnI,1)
    .sizesbreak1.txtsize&lcI..VALUE = SCALE.Sz&lcI
    .sizesbreak1.txtQty&lcI..VALUE  = MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.Avl'+lcI) - EVALUATE(loFormSet.lcTmpOrdLn+'.Pik'+lcI),0)
    .txtord&lcI..VALUE = laQty[lnI]
    .txtWip&lcI..VALUE = EVALUATE(loFormSet.lcTmpOrdLn+'.Cut'+lcI)
    .txtPik&lcI..VALUE = laPik[lnI]
  ENDFOR

  IF SCALE.CNT <> 0
    FOR lnI = SCALE.CNT + 1 TO 8
      lcI = STR(lnI,1)
      .sizesbreak1.txtsize&lcI..VALUE = ""
      .sizesbreak1.txtQty&lcI..VALUE  = 0
      .txtord&lcI..VALUE = 0
      .txtWip&lcI..VALUE = 0
      .txtPik&lcI..VALUE = 0
    ENDFOR
  ENDIF

  .sizesbreak1.txttotalQty.VALUE = MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.TotAvl') - EVALUATE(loFormSet.lcTmpOrdLn+'.TotPik'),0)
  .txttotalOrd.VALUE = EVALUATE(loFormSet.lcTmpOrdLn+'.TotQty')
  .txttotalPik.VALUE = EVALUATE(loFormSet.lcTmpOrdLn+'.TotPik')
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
  *.lblWIP.Caption = IIF(STYLE.Make .OR. EOF(loFormSet.lcTmpOrdLn) , 'C/T' , 'P/O')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.lblWIP.Caption = IIF(STYLE.Make .OR. EOF(loFormSet.lcTmpOrdLn) , LANG_AutoAlloc_NO_CT    , LANG_AutoAlloc_NO_PO)
  .lblWIP.CAPTION = IIF(STYLE.MAKE .OR. EOF(loFormSet.lcTmpOrdLn) , IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_NO_CT,loFormSet.GetHeaderText("LANG_AutoAlloc_NO_CT",loFormSet.HeaderAlias))    , IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_NO_PO,loFormSet.GetHeaderText("LANG_AutoAlloc_NO_PO",loFormSet.HeaderAlias)))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[end]
  .keyDyelot.keytextbox.ENABLED  = !EVALUATE(loFormSet.lcTmpOrdLn+'.Picked')
  .keyDyelot.keyCmd.ENABLED      = !EVALUATE(loFormSet.lcTmpOrdLn+'.Picked')
  .cboLocation.ENABLED = !EVALUATE(loFormSet.lcTmpOrdLn+'.Picked')
ENDWITH

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.AriaForm1.grdOrders.cmdSelect.Caption = IIF(EVALUATE(loFormSet.lcTmpOrdLn+'.lnSel')=0,LANG_AUTOALLOC_ButtSelect,LANG_AUTOALLOC_ButtUnSelect)
loFormSet.AriaForm1.grdOrders.cmdSelect.CAPTION = IIF(EVALUATE(loFormSet.lcTmpOrdLn+'.lnSel')=0,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_ButtSelect,loFormSet.GetHeaderText("LANG_AUTOALLOC_ButtSelect",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_ButtUnSelect,loFormSet.GetHeaderText("LANG_AUTOALLOC_ButtUnSelect",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]


*!*************************************************************
*! Name      : lpShow
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Local Show Procedure
*!*************************************************************
*! Called from : Control panel global Show
*!*************************************************************
*! Calls       : lfvScope() , lfShowGets() , lfvpbSel , lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
PROCEDURE lpShow
PARAMETERS loFormSet

*-- Control Enabling & Disabling Tool bar buttons
STORE .F. TO  loFormSet.llCtrStat7,loFormSet.llCtrStat8,loFormSet.llCtrStat9,loFormSet.llCtrStat10
=lfTolBrShw(loFormSet)


*-- IF The user has generated piking tikets for all the order lines in scope
*-- Clear for new selection
loFormSet.lnBrRecNo = RECNO(loFormSet.lcTmpOrdLn)

=lfBundBrow(loFormSet)


*-- Bound Objects on the screen
=lfBundFlds(loFormSet)

*-- IF the current record is selected
IF EVALUATE(loFormSet.lcTmpOrdLn+'.lnSel') = 1
  =lfShowGets(.T.,loFormSet)
ELSE
  loFormSet.llCh3Stat = .F.
  loFormSet.lapikst = .F.
  =lfDisblGts(loFormSet)
ENDIF    && End of IF


*-- end of lpShow.

*!*************************************************************
*! Name      : lfShowGets
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Function to control the Enabling and Disabling of
*!             the Objects in the screen [lcAutAlCh3]
*!*************************************************************
*! Called from : lpShow() , lfvScope() , lfAloScr() , lfRelScr() ,
*!               lfwBrows() , lfvStyle() , lfvWareCode() , lfvDyelot() ,
*!               lfvSelect() , lfvSelAll() , lfvInvert() , lfBrowTrap()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  [.T.] IF We want to refresh the Enabling and
*!                             Disabling of all the Objects in the screen
*!                             [lcAutAlCh3]
*!                       [.F.] IF We want to refresh the Enabling and
*!                             Disabling of the Allocated Get fields only
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfShowGets
PARAMETERS llParm,loFormSet


lcStyle    = EVALUATE(loFormSet.lcTmpOrdLn+'.Style')
lcWareCode = EVALUATE(loFormSet.lcTmpOrdLn+'.cWareCode')
lcDyelot   = EVALUATE(loFormSet.lcTmpOrdLn+'.Dyelot')
=SEEK(lcStyle + lcWareCode + lcDyelot , 'STYDYE')
=SEEK(lcStyle , 'STYLE')
=SEEK('S' + STYLE.SCALE , 'SCALE')

*IF We want to refresh the Enabling and Disabling of all the Objects in the
*screen [lcAutAlCh3]
IF llParm
  loFormSet.llCh3Stat = IIF(EVALUATE(loFormSet.lcTmpOrdLn+'.TotPik') = 0 , .T. , .F.)
  loFormSet.AriaForm1.cntDetail.cboLocation.ENABLED = loFormSet.llCh3Stat
  *IF The system use Dyelots
  IF loFormSet.llUseDyes
    loFormSet.llDyelotSt = IIF(STYLE.cDye_Flg = 'N' , .F. , loFormSet.llCh3Stat)
    loFormSet.AriaForm1.cntDetail.keyDyelot.keytextbox.VALUE = IIF(STYLE.cDye_Flg = 'N' , SPACE(10) , loFormSet.AriaForm1.cntDetail.keyDyelot.keytextbox.VALUE)
    loFormSet.AriaForm1.cntDetail.keyDyelot.ENABLED = loFormSet.llDyelotSt
  ELSE    && Else
    loFormSet.AriaForm1.cntDetail.keyStyle.ENABLED = loFormSet.llCh3Stat
  ENDIF    && End of IF
ENDIF    && End of IF

*IF we Are at the end of the STYDYE file or the current Style is Dyelot
*Yes and the Dyelot field is empty
IF EOF('STYDYE') .OR. (loFormSet.llUseDyes .AND. STYLE.cDye_Flg = 'Y' .AND. EMPTY(loFormSet.AriaForm1.cntDetail.keyDyelot.keytextbox.VALUE))
  loFormSet.lapikst = .F.
ELSE    && Else
  FOR lnI = 1 TO 8
    loFormSet.lapikst[lnI] = IIF(SCALE.CNT < lnI , .F. , .T.)
  ENDFOR
ENDIF    && End of IF

FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  loFormSet.AriaForm1.cntDetail.txtPik&lcI..ENABLED = loFormSet.lapikst[lnI]
  loFormSet.AriaForm1.cntDetail.sizesbreak1.txtsize&lcI..ENABLED = .T.
ENDFOR


*!*************************************************************
*! Name      : lfDisblGts
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Function to Disable the Objects in the screen [lcAutAlCh3]
*!*************************************************************
*! Called from : lpShow() , lfvScope() , lfAloScr() , lfRelScr() ,
*!               lfwBrows() , lfvStyle() , lfvWareCode() , lfvDyelot() ,
*!               lfvSelect() , lfvSelAll() , lfvInvert() , lfBrowTrap()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfDisblGts
PARAMETERS loFormSet

loFormSet.AriaForm1.cntDetail.cboLocation.ENABLED = loFormSet.llCh3Stat
loFormSet.AriaForm1.cntDetail.keyDyelot.keytextbox.ENABLED = loFormSet.llCh3Stat
loFormSet.AriaForm1.cntDetail.keyDyelot.ENABLED = loFormSet.llCh3Stat
loFormSet.AriaForm1.cntDetail.keyStyle.ENABLED = loFormSet.llCh3Stat
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  loFormSet.AriaForm1.cntDetail.txtPik&lcI..ENABLED = loFormSet.lapikst[lnI]
  loFormSet.AriaForm1.cntDetail.sizesbreak1.txtsize&lcI..ENABLED = .T.
ENDFOR

*!*************************************************************
*! Name      : lfGetWareH
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Function to get the fill the warehouse desc. and
*!             warehouse code arrays
*!*************************************************************
*! Called from : ALAUTAL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfGetWareH
PARAMETERS loFormSet

SELECT IIF(EMPTY(cDesc),PADR(cWareCode,35),cDesc) AS cDescrip, cWareCode ;
  FROM WAREHOUS ;
  WHERE lStyInv;
  INTO ARRAY loFormSet.laWareHous;
  ORDER BY cDescrip

*!*************************************************************
*! Name      : lfwBrows
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : When function of the Browse [lcOrdLBrow]
*!*************************************************************
*! Called from : The Browse [lcOrdLBrow]
*!*************************************************************
*! Calls       : lfShowGets() , lfRefresh()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfwBrows
PARAMETERS loFormSet

loFormSet.llforceall = .F.

loFormSet.lnBrRecNo  = RECNO(loFormSet.lcTmpOrdLn)


*-- Bound Objects on the screen
=lfBundFlds(loFormSet)

*IF the current record is selected
IF EVALUATE(loFormSet.lcTmpOrdLn+'.lnSel') = 1
  =lfShowGets(.T.,loFormSet)
ELSE
  loFormSet.llCh3Stat = .F.
  loFormSet.lapikst = .F.
  =lfDisblGts(loFormSet)
ENDIF    && End of IF


llEOF = .F.          && Flag to know if we are at the First record of the temp. Order lines file
llBOF = .F.          && Flag to know if we are at the Last record of the temp. Order lines file

*IF The temp. Order lines file is not empty
IF !EOF(loFormSet.lcTmpOrdLn) .AND. !BOF(loFormSet.lcTmpOrdLn)
  SKIP 1
  *IF We are at the End of the temp. Order lines file
  IF EOF(loFormSet.lcTmpOrdLn)
    llEOF = .T.
    SKIP -2
    *IF We are at the Begin of the temp. Order lines file
    IF BOF(loFormSet.lcTmpOrdLn)
      llBOF = .T.
    ELSE    && Else
      SKIP 1
    ENDIF    && End of IF
  ELSE    && Else
    SKIP -2
    *IF We are at the Begin of the temp. Order lines file
    IF BOF(loFormSet.lcTmpOrdLn)
      llBOF = .T.
    ELSE    && Else
      SKIP 1
    ENDIF    && End of IF
  ENDIF    && End of IF
ELSE    && Else
  llEOF = .T.
  llBOF = .T.
ENDIF    && End of IF

DO CASE
CASE cPosition = 'T'
  STORE .F. TO loFormSet.llCtrStat1 , loFormSet.llCtrStat4  && First button  , Priveus button
  STORE .T. TO loFormSet.llCtrStat2 , loFormSet.llCtrStat3  && Last button , Next button
CASE cPosition = 'E'
  STORE .F. TO loFormSet.llCtrStat2 , loFormSet.llCtrStat3  && Last button , Next button
  STORE .T. TO loFormSet.llCtrStat1 , loFormSet.llCtrStat4  && First button  , Priveus button
CASE EMPTY(cPosition)
  STORE .T. TO loFormSet.llCtrStat2 , loFormSet.llCtrStat3  && Last button , Next button
  STORE .T. TO loFormSet.llCtrStat1 , loFormSet.llCtrStat4  && First button  , Priveus button
ENDCASE
=lfNavShw(loFormSet)

*!*************************************************************
*! Name      : lfCntrlObj
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Control the displaying of object on the screen
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfCntrlObj
PARAMETERS loFormSet

WITH loFormSet.AriaForm1.cntDetail
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.lblWIP.Caption = IIF(STYLE.Make .OR. EOF(loFormSet.lcTmpOrdLn),LANG_AutoAlloc_CT,LANG_AutoAlloc_PO)
  .lblWIP.CAPTION = IIF(STYLE.MAKE .OR. EOF(loFormSet.lcTmpOrdLn),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_CT,loFormSet.GetHeaderText("LANG_AutoAlloc_CT",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_PO,loFormSet.GetHeaderText("LANG_AutoAlloc_PO",loFormSet.HeaderAlias)))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *.lblDyelot.Caption = IIF(loFormSet.llUseConfg,LANG_AutoAlloc_Configuration,LANG_AutoAlloc_Dyelot)
  .lblDyelot.CAPTION = IIF(loFormSet.llUseConfg,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_Configuration,loFormSet.GetHeaderText("LANG_AutoAlloc_Configuration",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_Dyelot,loFormSet.GetHeaderText("LANG_AutoAlloc_Dyelot",loFormSet.HeaderAlias)))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  .lblDyelot.VISIBLE = loFormSet.llUseDyes
  .keyDyelot.VISIBLE = loFormSet.llUseDyes
  .lblLocation.VISIBLE = loFormSet.llMultWare
  .cboLocation.VISIBLE = loFormSet.llMultWare
ENDWITH

*!*************************************************************
*! Name      : lfvStyle
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid function of the Style
*!*************************************************************
*! Called from :  Screen ALAUTAL [lcAutAlCh3 - Style field]
*!*************************************************************
*! Calls       : lfGScalCnt() , gfStyBrw() , gfModalGen() , gpAdStyWar()
*!               lfShowGets() , lfRefresh()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvStyle
PARAMETERS llBrowse,loFormSet

PRIVATE lnOScalCnt , lnNScalCnt , llObjRet , lcOldAlias ,;
  lcOScale , lcNScale , lnSclRecNo

lcStyle    = loFormSet.AriaForm1.cntDetail.keyStyle.VALUE
lcOldVal   = loFormSet.AriaForm1.cntDetail.keyStyle.oldvalue
lcWareCode = EVALUATE(loFormSet.lcTmpOrdLn+'.cWareCode')
*IF the Style field was empty or not changed and the user dose not
*want to Browse
IF (EMPTY(lcStyle) .OR. lcStyle == lcOldVal) .AND. !llBrowse
  lcStyle = lcOldVal
  RETURN
ENDIF    && End of IF

lcOldAlias = ALIAS()               && Variable to save the old Alis
lnOScalCnt = lfGScalCnt(STYLE)     && Variable to save the old Scale number of sizes
lcOScale   = STYLE.SCALE             && Variable to save the old Scale

*IF The user want to Browse or if the Style he entered is not in the file
IF '?' $ lcStyle .OR. llBrowse .OR. !SEEK(lcStyle , 'STYLE')
  ON KEY LABEL ALT+B
  lnSclRecNo = IIF(EOF('SCALE') .OR. BOF('SCALE') , 0 , RECNO('SCALE'))    && Variable to save the scale file record number
  lcStyle = gfStyBrw('I',"","",.F.)

  *IF The user selected a Style from the Style Browse
  IF !EMPTY(lcStyle)
    lcStyle = PADR(lcStyle , 19)
  ELSE    && Else
    lcStyle = lcOldVal

    *IF We have a valid SCALE record number
    IF lnSclRecNo <> 0
      GO lnSclRecNo IN 'SCALE'
    ELSE    && Else
      GO BOTTOM IN 'SCALE'
      SKIP 1 IN 'SCALE'
    ENDIF    && End of IF
  ENDIF    && End of IF
  llBrowse = .F.
ENDIF    && End of IF


*IF The user has changed the Style
IF lcStyle <> lcOldVal
  lnNScalCnt = lfGScalCnt(lcStyle)       && Variable to save the new Scale number of sizes
  lcNScale = STYLE.SCALE                 && Variable to save the new Scale

  *IF the new Scale number of sizes is less than the old Scale number of sizes
  IF lnNScalCnt < lnOScalCnt

    *** Message : "The number of sizes for ð/Scale ð is less than the"
    ***           "number of sizes for ð/Scale ð . Cannot proceed    "
    ***           "                        < Ok >                    "
    =gfModalGen('TRM44009B00000','DIALOG' , loFormSet.lcStyleTtl + '|' + ALLTRIM(lcStyle) + '/' + ALLTRIM(lcNScale) + '|' + loFormSet.lcStyleTtl + '|' + ALLTRIM(lcOldVal) + '/' + ALLTRIM(lcOScale))
    lcStyle = lcOldVal          && Restore the old value
  ELSE    && Else
    loFormSet.lcStyScale = lcNScale
  ENDIF    && End of IF
ENDIF    && End of IF

*IF The user has changed the Style
IF lcStyle <> lcOldVal
  =SEEK('O' + EVALUATE(loFormSet.lcTmpOrdLn+'.Order') , 'ORDHDR')

  *IF The Style division is not the same as the Order division
  IF STYLE.cDivision <> ORDHDR.cDivision

    *** Message : "ð / ð confilct!                               "
    ***           "                        < Ok >                    "
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *=gfModalGen('TRM42086B00000','DIALOG' , loFormSet.lcStyleTtl + LANG_AutoAlloc_MsgDivision)
    =gfModalGen('TRM42086B00000','DIALOG' , loFormSet.lcStyleTtl + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_MsgDivision,loFormSet.GetHeaderText("LANG_AutoAlloc_MsgDivision",loFormSet.HeaderAlias)))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    lcStyle = lcOldVal          && Restore the old value
  ENDIF    && End of IF
ENDIF    && End of IF

*IF The user has changed the Style
IF lcStyle <> lcOldVal
  =SEEK('O' + EVALUATE(loFormSet.lcTmpOrdLn+'.Order') , 'ORDHDR')

  *IF The Style season is not the same as the Order season
  IF ORDHDR.Season <> '*' .AND. STYLE.Season <> ORDHDR.Season

    *** Message : "Style / ð confilct!                               "
    ***           "                        < Ok >                    "
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *=gfModalGen('TRM42086B00000','DIALOG' , loFormSet.lcStyleTtl + LANG_AutoAlloc_MsgSeason)
    =gfModalGen('TRM42086B00000','DIALOG' , loFormSet.lcStyleTtl + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_MsgSeason,loFormSet.GetHeaderText("LANG_AutoAlloc_MsgSeason",loFormSet.HeaderAlias)))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    lcStyle = lcOldVal          && Restore the old value
  ENDIF    && End of IF
ENDIF    && End of IF

SELECT (lcOldAlias)

=SEEK(lcStyle , 'STYLE')

*IF the Style was changed and the new Style is not assigned to the Warehouse
IF lcStyle <> lcOldVal .AND. !SEEK(lcStyle + lcWareCode + SPACE(10) , 'STYDYE')
  *IF Statment to check if the user is going to add the Style to the
  *Warehouse
  *** Message : "ð is not assigned to warehouse ð. "
  ***           "     < Yes >           < No >     "
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *IF gfModalGen('TRM42001B00006','DIALOG' , loFormSet.lcStyleTtl + ' ' + ALLTRIM(lcStyle) + '|' + ALLTRIM(lcWareCode) + LANG_AutoAlloc_MsgAskAssign) = 1
  IF gfModalGen('TRM42001B00006','DIALOG' , loFormSet.lcStyleTtl + ' ' + ALLTRIM(lcStyle) + '|' + ALLTRIM(lcWareCode) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_MsgAskAssign,loFormSet.GetHeaderText("LANG_AutoAlloc_MsgAskAssign",loFormSet.HeaderAlias))) = 1
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    =gpAdStyWar(lcStyle , SPACE(10) , lcWareCode)
  ENDIF    && End of IF
ENDIF    && End of IF

=SEEK(lcStyle + lcWareCode + SPACE(10) , 'STYDYE')


*IF the Style was changed
IF lcStyle <> lcOldVal
  =lfShowGets(.F.,loFormSet)
ENDIF    && End of IF

=SEEK(lcStyle, 'STYLE')
loFormSet.AriaForm1.cntDetail.keyStyle.VALUE = lcStyle
loFormSet.AriaForm1.cntDetail.txtDesc.VALUE  = STYLE.Desc1
=lfAllocate(3 , RECNO(loFormSet.lcTmpOrdLn),loFormSet)

*!*************************************************************
*! Name      : lfGScalCnt
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Function to Get the number of sizes for a specific Style
*!*************************************************************
*! Called from : lfvStyle()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  The Style we want to get the number of sizes for
*!*************************************************************
*! Return      : The number of sizes for the specific Style
*!*************************************************************
*
FUNCTION lfGScalCnt
PARAMETERS lcParm

PRIVATE lcScale
=SEEK(lcParm , 'STYLE')
lcScale = STYLE.SCALE                 && Variable to hold the Scale code of the Style we want to get the number of sizes for
=SEEK('S' + lcScale , 'SCALE')

RETURN SCALE.CNT

*!*************************************************************
*! Name      : lfvWareCode
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid function of the Warehouse
*!*************************************************************
*! Called from :  Screen ALAUTAL [lcAutAlCh3 - Warehouse field]
*!*************************************************************
*! Calls       : gfBrowWare() , gpAdStyWar() , lfShowGets() , lfRefresh()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvWareCode
PARAMETERS loFormSet

PRIVATE lcOldWare

lnWareCode = loFormSet.AriaForm1.cntDetail.cboLocation.VALUE
lnOldVal   = loFormSet.AriaForm1.cntDetail.cboLocation.oldvalue
lcStyle    = EVALUATE(loFormSet.lcTmpOrdLn+'.Style')
lcDyelot   = EVALUATE(loFormSet.lcTmpOrdLn+'.Dyelot')

lcOldWare  = loFormSet.laWareHous[lnOldVal,2]
lcWareCode = loFormSet.laWareHous[lnWareCode,2]

*IF the Warehouse field was empty or not changed
IF (EMPTY(lcWareCode) .OR. lcWareCode == lcOldWare)
  lcWareCode = lcOldWare
  lnWareCode = lnOldVal
  loFormSet.AriaForm1.cntDetail.cboLocation.VALUE = lnWareCode
  RETURN 0
ENDIF    && End of IF

*IF the Warehouse was changed and the configuration is not assigned to the new
*Warehouse
IF loFormSet.llUseConfg AND !SEEK(lcStyle + lcWareCode + lcDyelot , 'STYDYE') .AND. lcWareCode <> lcOldWare

  *** Message : "Configuration [] is not assigned to warehouse []. Cannot change. "
  ***           "   <OK>   "
  lcWareDesc = loFormSet.laWareHous[lnWareCode,1]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *=gfModalGen('TRM42001B00000','DIALOG' , LANG_AutoAlloc_LabelReasCnfgNtAssgn1 + ALLTRIM(lcDyelot) + '|' + lcWareDesc + LANG_AutoAlloc_CannotChange)
  =gfModalGen('TRM42001B00000','DIALOG' , IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LabelReasCnfgNtAssgn1,loFormSet.GetHeaderText("LANG_AutoAlloc_LabelReasCnfgNtAssgn1",loFormSet.HeaderAlias)) + ALLTRIM(lcDyelot) + '|' + lcWareDesc + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_CannotChange,loFormSet.GetHeaderText("LANG_AutoAlloc_CannotChange",loFormSet.HeaderAlias)))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.AriaForm1.cntDetail.cboLocation.VALUE = lnOldVal
  loFormSet.AriaForm1.cntDetail.cboLocation.REQUERY()
  RETURN 0
ENDIF    && End of IF


IF !SEEK(lcStyle + lcWareCode + SPACE(10) , 'STYDYE') .AND. lcWareCode <> lcOldWare

  *IF Statment to check if the user is going to add the Style to the
  *Warehouse
  *** Message : "ð is not assigned to warehouse ð. "
  ***           "     < Yes >           < No >     "
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *IF gfModalGen('TRM42001B00006','DIALOG' , loFormSet.lcStyleTtl + ' ' + ALLTRIM(lcStyle) + '|' + ALLTRIM(lcWareCode) + LANG_AutoAlloc_MsgAskAssign) = 1
  IF gfModalGen('TRM42001B00006','DIALOG' , loFormSet.lcStyleTtl + ' ' + ALLTRIM(lcStyle) + '|' + ALLTRIM(lcWareCode) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_MsgAskAssign,loFormSet.GetHeaderText("LANG_AutoAlloc_MsgAskAssign",loFormSet.HeaderAlias))) = 1
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    =gpAdStyWar(lcStyle , SPACE(10) , lcWareCode)
  ENDIF    && End of IF
ENDIF    && End of IF


IF SEEK(lcStyle + lcOldWare + SPACE(10) , 'STYDYE')
  SELECT STYDYE
  = RLOCK()
  REPLACE Ord1 WITH MAX((Ord1 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty1')) , 0) ,;
    Ord2 WITH MAX((Ord2 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty2')) , 0) ,;
    Ord3 WITH MAX((Ord3 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty3')) , 0) ,;
    Ord4 WITH MAX((Ord4 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty4')) , 0) ,;
    Ord5 WITH MAX((Ord5 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty5')) , 0) ,;
    Ord6 WITH MAX((Ord6 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty6')) , 0) ,;
    Ord7 WITH MAX((Ord7 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty7')) , 0) ,;
    Ord8 WITH MAX((Ord8 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty8')) , 0) ,;
    TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
  =gfAdd_Info('STYDYE',loFormSet)
  UNLOCK
ENDIF    && End of IF
IF SEEK(lcStyle + lcOldWare + lcDyelot  , 'STYDYE')
  SELECT STYDYE
  = RLOCK()
  REPLACE Ord1 WITH MAX((Ord1 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty1')) , 0) ,;
    Ord2 WITH MAX((Ord2 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty2')) , 0) ,;
    Ord3 WITH MAX((Ord3 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty3')) , 0) ,;
    Ord4 WITH MAX((Ord4 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty4')) , 0) ,;
    Ord5 WITH MAX((Ord5 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty5')) , 0) ,;
    Ord6 WITH MAX((Ord6 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty6')) , 0) ,;
    Ord7 WITH MAX((Ord7 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty7')) , 0) ,;
    Ord8 WITH MAX((Ord8 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty8')) , 0) ,;
    TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
  =gfAdd_Info('STYDYE',loFormSet)
  UNLOCK
ENDIF    && End of

SELECT (loFormSet.lcTmpOrdLn)
REPLACE cWareCode WITH lcWareCode
lcCurDye = lcDyelot
IF SEEK(lcStyle + lcWareCode + SPACE(10) , 'STYDYE')
  SELECT STYDYE
  = RLOCK()
  REPLACE Ord1 WITH MAX((Ord1 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty1')) , 0) ,;
    Ord2 WITH MAX((Ord2 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty2')) , 0) ,;
    Ord3 WITH MAX((Ord3 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty3')) , 0) ,;
    Ord4 WITH MAX((Ord4 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty4')) , 0) ,;
    Ord5 WITH MAX((Ord5 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty5')) , 0) ,;
    Ord6 WITH MAX((Ord6 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty6')) , 0) ,;
    Ord7 WITH MAX((Ord7 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty7')) , 0) ,;
    Ord8 WITH MAX((Ord8 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty8')) , 0) ,;
    TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
  lcCurDye = SPACE(10)
  =gfAdd_Info('STYDYE',loFormSet)
  UNLOCK
ENDIF

IF !EMPTY(lcDyelot) AND SEEK(lcStyle + lcWareCode + lcDyelot , 'STYDYE')
  SELECT STYDYE
  = RLOCK()
  REPLACE Ord1 WITH MAX((Ord1 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty1')) , 0) ,;
    Ord2 WITH MAX((Ord2 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty2')) , 0) ,;
    Ord3 WITH MAX((Ord3 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty3')) , 0) ,;
    Ord4 WITH MAX((Ord4 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty4')) , 0) ,;
    Ord5 WITH MAX((Ord5 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty5')) , 0) ,;
    Ord6 WITH MAX((Ord6 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty6')) , 0) ,;
    Ord7 WITH MAX((Ord7 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty7')) , 0) ,;
    Ord8 WITH MAX((Ord8 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty8')) , 0) ,;
    TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
  lcCurDye = STYDYE.DyeLot
  =gfAdd_Info('STYDYE',loFormSet)
  UNLOCK
ENDIF

=SEEK(lcStyle + lcWareCode + lcCurDye, 'STYDYE')
SELECT (loFormSet.lcTmpOrdLn)
REPLACE DyeLot WITH lcCurDye
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  *:   B610534,1 MMT 09/29/2013 Modify Automatic allocation screen to include WIP in Avail. based on setup{T20130910.0026}[Start]
  *REPLACE Avl&lcI WITH STYDYE.Stk&lcI
  REPLACE Avl&lcI WITH IIF(loFormSet.llTotAvlbl,STYDYE.WIP&lcI,0)+STYDYE.Stk&lcI
  *:   B610534,1 MMT 09/29/2013 Modify Automatic allocation screen to include WIP in Avail. based on setup{T20130910.0026}[End]
ENDFOR
*:   B610534,1 MMT 09/29/2013 Modify Automatic allocation screen to include WIP in Avail. based on setup{T20130910.0026}[Start]
*REPLACE TotAvl WITH STYDYE.TotStk
REPLACE TotAvl WITH IIF(loFormSet.llTotAvlbl,STYDYE.TOTWIP,0)+STYDYE.TotStk
*:   B610534,1 MMT 09/29/2013 Modify Automatic allocation screen to include WIP in Avail. based on setup{T20130910.0026}[End]

*IF the Warehouse was changed
IF lcWareCode <> lcOldWare
  =lfShowGets(.F.,loFormSet)
ENDIF    && End of IF

loFormSet.AriaForm1.cntDetail.cboLocation.VALUE = lnWareCode
=lfAllocate(3 , RECNO(loFormSet.lcTmpOrdLn),loFormSet)
=lfBundFlds(loFormSet)

*!*************************************************************
*! Name      : lfvDyelot
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid function of the Dyelot
*!*************************************************************
*! Called from :  Screen ALAUTAL [lcAutAlCh3 - Dyelot field]
*!*************************************************************
*! Calls       : lfShowGets() , lfRefresh() , SDyeBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvDyelot
PARAMETERS llBrowse,loFormSet

PRIVATE llObjRet , lcOldAlias

lcDyelot = PADR(loFormSet.AriaForm1.cntDetail.keyDyelot.keytextbox.VALUE ,10)
lcOldVal = PADR(loFormSet.AriaForm1.cntDetail.keyDyelot.keytextbox.oldvalue ,10)
lcStyle  = EVALUATE(loFormSet.lcTmpOrdLn+'.Style')
lcWareCode = EVALUATE(loFormSet.lcTmpOrdLn+'.cWareCode')

*IF the Dyelot field was empty or not changed and the user dose not want to
*Browse
IF (EMPTY(lcDyelot) .OR. ALLTRIM(lcDyelot) == ALLTRIM(lcOldVal)) .AND. !llBrowse
  *IF The Dyelot field was Changed [if the User removed the Dyelot]
  IF lcDyelot <> lcOldVal
    =lfShowGets(.F.,loFormSet)
  ENDIF    && End of IF
  loFormSet.AriaForm1.cntDetail.keyDyelot.keytextbox.VALUE = lcDyelot
  RETURN
ENDIF    && End of IF

lcOldAlias = ALIAS()               && Variable to save the old Alis

*IF The user want to Browse or if the Dyelot he entered is not in the file
IF '?' $ lcDyelot .OR. llBrowse .OR. !SEEK(lcStyle + lcWareCode + lcDyelot , 'STYDYE')
  llObjRet = SDyeBrow(lcStyle , @lcDyelot , .F. , lcWareCode, .T., .T.,.F.,.F.,loFormSet.llUseConfg)
  lcDyelot = IIF(llObjRet,lcDyelot,lcOldVal)
  llBrowse = .F.
ENDIF    && End of IF

SELECT (lcOldAlias)

=SEEK(lcStyle + lcWareCode + lcDyelot , 'STYDYE')
SELECT (loFormSet.lcTmpOrdLn)
REPLACE DyeLot WITH lcDyelot
FOR lnI = 1 TO 8
  lcI = STR(lnI,1)
  *:   B610534,1 MMT 09/29/2013 Modify Automatic allocation screen to include WIP in Avail. based on setup{T20130910.0026}[Start]
  *REPLACE Avl&lcI WITH STYDYE.Stk&lcI
  REPLACE Avl&lcI WITH  IIF(loFormSet.llTotAvlbl,STYDYE.WIP&lcI,0)+STYDYE.Stk&lcI
  *:   B610534,1 MMT 09/29/2013 Modify Automatic allocation screen to include WIP in Avail. based on setup{T20130910.0026}[End]
ENDFOR
*:   B610534,1 MMT 09/29/2013 Modify Automatic allocation screen to include WIP in Avail. based on setup{T20130910.0026}[Start]
*REPLACE TotAvl WITH STYDYE.TotStk
REPLACE TotAvl WITH IIF(loFormSet.llTotAvlbl,STYDYE.TOTWIP,0)+STYDYE.TotStk
*:   B610534,1 MMT 09/29/2013 Modify Automatic allocation screen to include WIP in Avail. based on setup{T20130910.0026}[End]
IF SEEK(lcStyle + lcWareCode + lcOldVal , 'STYDYE')
  SELECT STYDYE
  = RLOCK()
  REPLACE Ord1 WITH MAX((Ord1 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty1')) , 0) ,;
    Ord2 WITH MAX((Ord2 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty2')) , 0) ,;
    Ord3 WITH MAX((Ord3 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty3')) , 0) ,;
    Ord4 WITH MAX((Ord4 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty4')) , 0) ,;
    Ord5 WITH MAX((Ord5 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty5')) , 0) ,;
    Ord6 WITH MAX((Ord6 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty6')) , 0) ,;
    Ord7 WITH MAX((Ord7 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty7')) , 0) ,;
    Ord8 WITH MAX((Ord8 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty8')) , 0) ,;
    TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
  =gfAdd_Info('STYDYE',loFormSet)
  UNLOCK
ENDIF

IF SEEK(lcStyle + lcWareCode + lcDyelot , 'STYDYE')
  SELECT STYDYE
  = RLOCK()
  REPLACE Ord1 WITH MAX((Ord1 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty1')) , 0) ,;
    Ord2 WITH MAX((Ord2 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty2')) , 0) ,;
    Ord3 WITH MAX((Ord3 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty3')) , 0) ,;
    Ord4 WITH MAX((Ord4 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty4')) , 0) ,;
    Ord5 WITH MAX((Ord5 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty5')) , 0) ,;
    Ord6 WITH MAX((Ord6 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty6')) , 0) ,;
    Ord7 WITH MAX((Ord7 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty7')) , 0) ,;
    Ord8 WITH MAX((Ord8 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty8')) , 0) ,;
    TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
  =gfAdd_Info('STYDYE',loFormSet)
  UNLOCK
ENDIF


*IF the Dyelot was changed
IF lcDyelot <> lcOldVal
  =lfShowGets(.F.,loFormSet)
ENDIF    && End of IF

loFormSet.AriaForm1.cntDetail.keyDyelot.keytextbox.VALUE = lcDyelot
=lfAllocate(3 , RECNO(loFormSet.lcTmpOrdLn),loFormSet)
=lfBundFlds(loFormSet)
*!*************************************************************
*! Name      : lfvAloQty
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid function of the Allocated Get fields
*!*************************************************************
*! Called from :  Screen ALAUTAL [lcAutAlCh3 - Allocated Get fields]
*!*************************************************************
*! Calls       : gfModalGen() , lfRefresh()
*!*************************************************************
*! Passed Parameters : [The number of the Allocated Get field to Valid]
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvAloQty
PARAMETERS lnParm,loFormSet

*-- MAB 09/01/1999 Check Stock quantity (Before Force allocation) [Begin]
PRIVATE lcParm,lnAvalStk
lcParm = ALLTRIM(STR(lnParm))

lnQty    = loFormSet.AriaForm1.cntDetail.txtord&lcParm..VALUE
lnTotQty = loFormSet.AriaForm1.cntDetail.txttotalOrd.VALUE
lnPik    = loFormSet.AriaForm1.cntDetail.txtPik&lcParm..VALUE
lnTotPik = loFormSet.AriaForm1.cntDetail.txttotalPik.VALUE
lcOldVal = loFormSet.AriaForm1.cntDetail.txtPik&lcParm..oldvalue

IF lnPik = lcOldVal
  RETURN
ENDIF

*T20060818.0001(C200876) TMI Custom Release PikTkt from custom table for DL[Start]
IF ASCAN(loFormSet.laEvntTrig,PADR('CHKPACK',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0 .AND. ;
    !loFormSet.mDoTrigger(PADR('CHKPACK',10))
  RETURN
ENDIF
*T20060818.0001(C200876) TMI [End  ]

*IF the Allocated quantity is less than 0
IF lnPik < 0
  *-- MAB 01/09/1999 Force user to enter positive values. [Begin]
  *Message : 44081 ==> A negative value is not allowed.
  *Button  : 00000 ==> < Ok >
  =gfModalGen('INM44081B00000', 'DIALOG')
  lnPik = lcOldVal
  loFormSet.AriaForm1.cntDetail.txtPik&lcParm..VALUE = lnPik
  RETURN
  *-- MAB 01/09/1999 Force user to enter positive values. [End  ]
ENDIF    && End of IF
lcPiktkt = EVALUATE(loFormSet.lcTmpOrdLn+'.PikTkt')
lcPiktkt = IIF(lcPiktkt='******'," ",lcPiktkt)
IF !loFormSet.llOpnPack  AND !USED('PACK_HDR')
  lcAlsNow  = SELECT(0)
  loFormSet.llOpnPack  = gfOpenFile(oAriaApplication.DataDir+'PACK_HDR',oAriaApplication.DataDir+'PACK_HDR','SH')
  SELECT (lcAlsNow)
ENDIF
IF !EMPTY(lcPiktkt) AND SEEK(lcPiktkt,'PIKTKT') AND PIKTKT.STATUS = 'O' AND SEEK(lcPiktkt,'PACK_HDR')
  =gfModalGen("INM44060B00000" , "DIALOG" , PADR(lcPiktkt,6))
  loFormSet.AriaForm1.cntDetail.txtPik&lcParm..VALUE = lcOldVal
  RETURN
ENDIF

lnAvalStk = IIF(EOF('STYLE') , 0 , MAX(STYDYE.Stk&lcParm - STYDYE.Alo&lcParm, 0))
IF (lnPik > lnAvalStk) AND !lfForceMe(loFormSet)
  lnPik = lcOldVal
  loFormSet.AriaForm1.cntDetail.txtPik&lcParm..VALUE = lnPik
  RETURN
ENDIF

*IF the Allocated quantity is greater than the Ordered quantity
IF lnPik > lnQty
  *IF Statment to check if the user is going to Increase the ordered
  *quantity
  *** Message : "Quantity allocated is greater than ordered. Increase "
  ***           "ordered quantity?                                    "
  ***           "             < Yes >   <Yes All>   < No >            "
  IF gfModalGen('TRM44002B00006','DIALOG') = 1
    lnTotQty = lnTotQty - lnQty + lnPik
    lnQty    = lnPik
    loFormSet.llIncOrd = .T.
  ELSE    && Else
    lnPik = lcOldVal
  ENDIF    && End of IF
ENDIF    && End of IF

*IF the Allocated quantity was changed
IF lnPik <> lcOldVal
  loFormSet.lnchangalo = IIF(lnPik = EVALUATE(loFormSet.lcTmpOrdLn+'.Pik'+lcParm) , loFormSet.lnchangalo - 1 ,;
    IIF(lcOldVal = EVALUATE(loFormSet.lcTmpOrdLn+'.Pik'+lcParm) , loFormSet.lnchangalo + 1 ,;
    loFormSet.lnchangalo))
  lnTotPik = lnTotPik + lnPik - lcOldVal
ENDIF    && End of IF

loFormSet.AriaForm1.cntDetail.txtord&lcParm..VALUE = lnQty
loFormSet.AriaForm1.cntDetail.txttotalOrd.VALUE    = lnTotQty
loFormSet.AriaForm1.cntDetail.txtPik&lcParm..VALUE = lnPik
loFormSet.AriaForm1.cntDetail.txttotalPik.VALUE    = lnTotPik
SELECT (loFormSet.lcTmpOrdLn)
REPLACE TotAlo	   WITH TotAlo - Alo&lcParm,;
  TotQty     WITH TotQty - QTY&lcParm
REPLACE Alo&lcParm WITH loFormSet.AriaForm1.cntDetail.txtPik&lcParm..VALUE,;
  TotAlo	   WITH TotAlo + Alo&lcParm,;
  QTY&lcParm WITH loFormSet.AriaForm1.cntDetail.txtord&lcParm..VALUE,;
  TotQty     WITH TotQty + QTY&lcParm

=lfAllocate(3 , RECNO(loFormSet.lcTmpOrdLn),loFormSet)
*T20060818.0001(C200876) TMI Custom Release PikTkt from custom table for DL[Start]
IF lnPik <> lcOldVal .AND. ASCAN(loFormSet.laEvntTrig,PADR('ALMODAUT',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
  =loFormSet.mDoTrigger(PADR('ALMODAUT',10))
ENDIF
*T20060818.0001(C200876) TMI [End  ]
=lfBundFlds(loFormSet)

*!*************************************************************
*! Name      : lfForceMe
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Force allocation. (Manual Force)
*!*************************************************************
FUNCTION lfForceMe
PARAMETERS loFormSet

IF loFormSet.llforceall
  RETURN
ENDIF

PRIVATE lnOption, llRetVal , lcButton , lcMsgType , lcOptMsg

lnOption = 1

IF loFormSet.llAlwForce
  lcButton  = "44002"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *lcOptMsg  = LANG_AutoAlloc_MsgAskForce1
  lcOptMsg  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_MsgAskForce1,loFormSet.GetHeaderText("LANG_AutoAlloc_MsgAskForce1",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  lcMsgType = "QR"
ELSE
  lcButton = "00000"
  lcOptMsg = " "
  lcMsgType = "TR"
ENDIF

lnOption = gfModalGen(lcMsgType+'M44000B'+lcButton, 'DIALOG',;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_MsgAskForce2,loFormSet.GetHeaderText("LANG_AutoAlloc_MsgAskForce2",loFormSet.HeaderAlias))+;
  "|"+lcOptMsg)

IF !loFormSet.llAlwForce
  lnOption = 3
ENDIF

loFormSet.llforceall = (lnOption = 2) && Avoiding entering this function again.
llRetVal   = (lnOption < 3)
RETURN llRetVal
*-- end of lfForceMe.

*!*************************************************************
*! Name      : lfAllocate
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Function to Allocate one or more line
*!*************************************************************
*! Called from : ALAUTAL.PRG , lfvScope() , lfAloScr() , lfGenScr() ,
*!               lfBrowTrap()
*!*************************************************************
*! Calls       : lfTmpAlo() , lfAloQty() , lfEditLine()
*!*************************************************************
*! Passed Parameters : 1) 1 to Allocate with the Option grid conditions
*!                      , 2 to Allocate the selected records
*!                      , 3 to edit a record
*!                     2) The number of the record to edit
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfAllocate
PARAMETERS lnParm , lnRecNo , loFormSet

PRIVATE lnTotRec , lnCurent , lcForCond , llAloGoup , lcGroupStr ,;
  llGrpFrRec , lnAvlFAlo , lcPikWare , lnRecNumb , llAloSep ,;
  lnOldRec , lnRecCount , lnPrepRec


lnTotRec = IIF(lnParm = 1 , loFormSet.lnRecNumbr , loFormSet.lnSelRec)     && Varible to hold the Total count to be done for the thermometer
lnCurent = 0                                             			      && Varible to hold the current count to be done for the thermometer

SELECT (loFormSet.lcTmpOrdLn)
*IF Not an incompleted session and not to edit a record
IF lnParm <> 3
  lnRecCount = RECCOUNT() - loFormSet.lnDellRec        && Varible to hold the Total count to be done for the thermometer
  lnPrepRec = 0                              && Varible to hold the current count to be done for the thermometer
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loFormSet.oPross.lblFirstLabel.Caption = LANG_AutoAlloc_LableProgress
  loFormSet.oPross.lblFirstLabel.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LableProgress,loFormSet.GetHeaderText("LANG_AutoAlloc_LableProgress",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.oPross.TotalProgress = lnRecCount
  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  loFormSet.oPross.AUTOCENTER = .T.
  *HBG [End]
  loFormSet.oPross.SHOW()
  *SCAN Loop to scan the temp. Order lines file
  SCAN
    lnPrepRec = lnPrepRec + 1
    REPLACE nProcNo WITH 0
    loFormSet.oPross.CurrentProgress(lnPrepRec)
  ENDSCAN    && End of SCAN Loop
ENDIF    && End of IF

SET ORDER TO TAG STYDYE IN STYDYE
SET ORDER TO TAG ORDLINE IN ORDLINE
SET ORDER TO TAG STYLE IN STYLE
SET ORDER TO TAG ORDHDR IN ORDHDR
SELECT (loFormSet.lcTmpOrdLn)
SET ORDER TO TAG (loFormSet.lcTmpOrdLn)

*IF Not to edit a record
IF lnParm <> 3
  SET RELATION TO STYLE INTO STYLE
ELSE    && Else
  SET RELATION TO
ENDIF    && End of IF

*DO CASE Statment
DO CASE

  *CASE of Allocate with the Option grid conditions
CASE (lnParm = 1) OR (lnParm = 2)
  *-- if include wip and select by C/T or P/O and we have C/Ts or P/Os.
  IF loFormSet.lcRpIncWip $ 'AS' AND loFormSet.lcRpScpMod $ 'KP' AND !EMPTY(loFormSet.laIncExprs[1,6])
    lnI = 0
    DIMENSION loFormSet.laString1[1]
    STORE "" TO loFormSet.laString1
    SELECT (loFormSet.laIncExprs[1,6])
    SCAN
      lnI = lnI + 1
      DIMENSION loFormSet.laString1[lnI]
      loFormSet.laString1[lnI] = PO
    ENDSCAN
  ENDIF

  *-- if include wip and exclude select by C/T or P/O and we have C/Ts or P/Os.
  IF loFormSet.lcRpIncWip $ 'AS' AND loFormSet.llExclude AND (loFormSet.lcRpExSlct $ 'KP') AND !EMPTY(loFormSet.laFiltExp[1,6])
    lnI = 0
    DIMENSION loFormSet.laString2[1]
    STORE "" TO loFormSet.laString2
    SELECT (loFormSet.laFiltExp[1,6])
    SCAN
      lnI = lnI + 1
      DIMENSION loFormSet.laString2[lnI]
      loFormSet.laString2[lnI] = PO
    ENDSCAN
  ENDIF

  SELECT (loFormSet.lcTmpOrdLn)
  IF loFormSet.llUseDyes AND (loFormSet.llFabDye OR loFormSet.llUseConfg) AND !loFormSet.llRpExlDye
    lcOldGroup = ''
    = lfDyeAlo(lnParm,loFormSet)
  ELSE  && else No dyelot allocation.
    = lfNormAlo(lnParm,loFormSet)
  ENDIF


  *CASE of edit a record
CASE lnParm = 3
  SELECT (loFormSet.lcTmpOrdLn)
  lnOldRec = RECNO()        && Variable to save the record number

  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GO lnRecNo
  ENDIF

  *IF Not an incompleted session
  REPLACE nProcNo WITH 0
  =lfEditLine(loFormSet)

  IF BETWEEN(lnOldRec,1,RECCOUNT())
    GO lnOldRec
  ENDIF

  SCATTER FIELDS LIKE QTY* TO laQty
  FOR lnI = 1 TO SCALE.CNT
    lcI = STR(lnI,1)
    loFormSet.AriaForm1.cntDetail.txtord&lcI..VALUE = laQty[lnI]
  ENDFOR
  loFormSet.AriaForm1.cntDetail.txttotalOrd.VALUE = EVALUATE(loFormSet.lcTmpOrdLn+'.TotQty')
ENDCASE    && End of DO CASE Statment

*-- Handle status of tool bar and option menu
=lfHandlObj(loFormSet)

SELECT (loFormSet.lcTmpOrdLn)
SET RELATION TO

*-- end of lfAllocate.

*!*************************************************************
*! Name      : lfEditLine
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Function to update the needed files when the current
*!             record of the temp. Order lines file is Edited from
*!             the screen [lcAutAlCh3]
*!*************************************************************
*! Called from : lfAllocate()
*!*************************************************************
*! Calls       : gfAdd_Info()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfEditLine
PARAMETERS loFormSet
PRIVATE lcAltStyle , lcOldWare , lnCnT1 , lnCnT2 , lcOScale ,;
  lnCanc , lcOldWare , lnElm , lcElm

lnCnT1 = 8                  && Variable to hold the number of sizes for the current Style
lnCnT2 = 8                  && Variable to hold the number of sizes for the Original Style
lcOScale = ''               && Variable to hold the Scale code of the Original Style
DIMENSION laPik[8],laQty[8]
STORE 0 TO laPik,laQty
IF EMPTY(loFormSet.AriaForm1.cntDetail.keyStyle.VALUE)
  FOR lnI = 1 TO SCALE.CNT
    lcI = STR(lnI,1)
    laQty[lnI] = loFormSet.AriaForm1.cntDetail.txtord&lcI..VALUE
    laPik[lnI] = loFormSet.AriaForm1.cntDetail.txtPik&lcI..VALUE
  ENDFOR
  lnTotQty   = loFormSet.AriaForm1.cntDetail.txttotalOrd.VALUE
  lnTotPik   = loFormSet.AriaForm1.cntDetail.txttotalPik.VALUE
  lcStyle    = loFormSet.AriaForm1.cntDetail.keyStyle.VALUE
  lnWareCode = loFormSet.AriaForm1.cntDetail.cboLocation.VALUE
  lcWareCode = loFormSet.laWareHous[lnWareCode,2]
  lcDyelot   = loFormSet.AriaForm1.cntDetail.keyDyelot.keytextbox.VALUE
ELSE
  FOR lnI = 1 TO SCALE.CNT
    lcI = STR(lnI,1)
    laQty[lnI] = EVALUATE(loFormSet.lcTmpOrdLn+'.Qty'+lcI)
    laPik[lnI] = EVALUATE(loFormSet.lcTmpOrdLn+'.Alo'+lcI)
  ENDFOR
  lnTotQty   = EVALUATE(loFormSet.lcTmpOrdLn+'.TotQty')
  lnTotPik   = EVALUATE(loFormSet.lcTmpOrdLn+'.TotAlo')
  lcStyle    = EVALUATE(loFormSet.lcTmpOrdLn+'.Style')
  lcDyelot   = EVALUATE(loFormSet.lcTmpOrdLn+'.Dyelot')
  lnWareCode = ASCAN(loFormSet.laWareHous,EVALUATE(loFormSet.lcTmpOrdLn+'.cWareCode'))
  lnWareCode = ASUBSCRIPT(loFormSet.laWareHous, lnWareCode , 1)
  lcWareCode = loFormSet.laWareHous[lnWareCode,2]
ENDIF


*B611762,1 ES 05/29/2019 - Style wrong ordered quantity in the Rebalance program - [Start]
IF !USED('Ordline_M')
  =gfOpenTable('Ordline','Ordline','SH','Ordline_M')
ENDIF
=SEEK('O' + ORDER + STR(LINENO , 6) , 'Ordline_M', 'ORDLINE')
*B611762,1 ES 05/29/2019 - Style wrong ordered quantity in the Rebalance program - [End]


*IF nProcNo [The step number (for the rollback)] equal 0
IF nProcNo = 0
  REPLACE PoAlo1    WITH laPik[1] - pik1 ,;
    PoAlo2    WITH laPik[2] - pik2 ,;
    PoAlo3    WITH laPik[3] - pik3 ,;
    PoAlo4    WITH laPik[4] - pik4 ,;
    PoAlo5    WITH laPik[5] - pik5 ,;
    PoAlo6    WITH laPik[6] - pik6 ,;
    PoAlo7    WITH laPik[7] - pik7 ,;
    PoAlo8    WITH laPik[8] - pik8 ,;
    Tot_PoAlo WITH lnTotPik - totpik ,;
    Book1     WITH laQty[1] ,;
    Book2     WITH laQty[2] ,;
    Book3     WITH laQty[3] ,;
    Book4     WITH laQty[4] ,;
    Book5     WITH laQty[5] ,;
    Book6     WITH laQty[6] ,;
    Book7     WITH laQty[7] ,;
    Book8     WITH laQty[8] ,;
    TotBook   WITH lnTotQty ,;
    lLok_Stat WITH EMPTY(AltStyle) ,;
    AltStyle  WITH IIF(lcStyle = STYLE , AltStyle , STYLE) ,;
    SCALE     WITH IIF(lcStyle = STYLE , SCALE , loFormSet.lcStyScale) ,;
    STYLE     WITH lcStyle ,;
    DyeLot    WITH lcDyelot ,;
    Season    WITH cWareCode ,;
    cWareCode WITH lcWareCode ,;
    nProcNo   WITH 1

  =RLOCK()
  UNLOCK
ENDIF    && End of IF

lcAltStyle = IIF(EMPTY(AltStyle) .OR. !lLok_Stat , STYLE , AltStyle)    && Variable to hold the original Style


*IF This line Style was substituted befor and the line is to be released
IF !lLok_Stat .AND. Tot_PoAlo + totpik = 0
  =SEEK(STYLE , 'STYLE')
  =SEEK( 'S'+STYLE.SCALE ,'SCALE')
  lnCnT1 = SCALE.CNT
  =SEEK(AltStyle , 'STYLE')
  =SEEK( 'S'+STYLE.SCALE ,'SCALE')
  lnCnT2 = SCALE.CNT
  lcOScale = SCALE.SCALE
ENDIF    && End of IF

*IF nProcNo [The step number (for the rollback)] equal 1
IF nProcNo = 1

  *IF There is a record for the current Style and Warehouse in the STYDYE
  *file
  IF SEEK(STYLE + cWareCode + SPACE(10) , 'STYDYE')
    SELECT STYDYE
    = RLOCK()
    REPLACE Alo1   WITH Alo1   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo1') ,;
      Alo2   WITH Alo2   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo2') ,;
      Alo3   WITH Alo3   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo3') ,;
      Alo4   WITH Alo4   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo4') ,;
      Alo5   WITH Alo5   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo5') ,;
      Alo6   WITH Alo6   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo6') ,;
      Alo7   WITH Alo7   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo7') ,;
      Alo8   WITH Alo8   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo8') ,;
      TotAlo WITH TotAlo + EVALUATE(loFormSet.lcTmpOrdLn+'.Tot_PoAlo')

    =gfAdd_Info('STYDYE',loFormSet)
    UNLOCK
  ENDIF    && End of IF
  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 2
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

*IF nProcNo [The step number (for the rollback)] equal 2
IF nProcNo = 2

  *IF There is a record for the cuurent Style and Warehouse and Dyelot in
  *the STYDYE file
  IF !EMPTY(DyeLot) .AND. SEEK(STYLE + cWareCode + DyeLot , 'STYDYE')
    SELECT STYDYE
    = RLOCK()
    REPLACE Alo1   WITH Alo1   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo1') ,;
      Alo2   WITH Alo2   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo2') ,;
      Alo3   WITH Alo3   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo3') ,;
      Alo4   WITH Alo4   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo4') ,;
      Alo5   WITH Alo5   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo5') ,;
      Alo6   WITH Alo6   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo6') ,;
      Alo7   WITH Alo7   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo7') ,;
      Alo8   WITH Alo8   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo8') ,;
      TotAlo WITH TotAlo + EVALUATE(loFormSet.lcTmpOrdLn+'.Tot_PoAlo')
    =gfAdd_Info('STYDYE',loFormSet)
    UNLOCK
  ENDIF    && End of IF
  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 3
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

*IF nProcNo [The step number (for the rollback)] equal 3
IF nProcNo = 3

  *IF There is a record for the current Style in the STYLE file
  IF SEEK(STYLE , 'STYLE')
    SELECT STYLE
    = RLOCK()
    REPLACE Alo1   WITH Alo1   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo1') ,;
      Alo2   WITH Alo2   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo2') ,;
      Alo3   WITH Alo3   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo3') ,;
      Alo4   WITH Alo4   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo4') ,;
      Alo5   WITH Alo5   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo5') ,;
      Alo6   WITH Alo6   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo6') ,;
      Alo7   WITH Alo7   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo7') ,;
      Alo8   WITH Alo8   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo8') ,;
      TotAlo WITH TotAlo + EVALUATE(loFormSet.lcTmpOrdLn+'.Tot_PoAlo')

    =gfAdd_Info('STYLE',loFormSet)
    UNLOCK
  ENDIF    && End of IF
  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 4
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

*IF nProcNo [The step number (for the rollback)] equal 4
IF nProcNo = 4

  *IF There is a record for the old Style and Warehouse in the STYDYE
  *file
  IF SEEK(lcAltStyle + cWareCode + SPACE(10) , 'STYDYE')
    SELECT STYDYE
    = RLOCK()
*B611762,1 ES 05/29/2019 - Style wrong ordered quantity in the Rebalance program - [Start]
*!*	    REPLACE Ord1 WITH MAX((Ord1 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty1')) , 0) ,;
*!*	      Ord2 WITH MAX((Ord2 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty2')) , 0) ,;
*!*	      Ord3 WITH MAX((Ord3 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty3')) , 0) ,;
*!*	      Ord4 WITH MAX((Ord4 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty4')) , 0) ,;
*!*	      Ord5 WITH MAX((Ord5 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty5')) , 0) ,;
*!*	      Ord6 WITH MAX((Ord6 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty6')) , 0) ,;
*!*	      Ord7 WITH MAX((Ord7 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty7')) , 0) ,;
*!*	      Ord8 WITH MAX((Ord8 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty8')) , 0) ,;
*!*	      TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8

    REPLACE Ord1 WITH MAX((Ord1 - EVALUATE('Ordline_M'+'.Qty1')) , 0) ,;
      Ord2 WITH MAX((Ord2 - EVALUATE('Ordline_M'+'.Qty2')) , 0) ,;
      Ord3 WITH MAX((Ord3 - EVALUATE('Ordline_M'+'.Qty3')) , 0) ,;
      Ord4 WITH MAX((Ord4 - EVALUATE('Ordline_M'+'.Qty4')) , 0) ,;
      Ord5 WITH MAX((Ord5 - EVALUATE('Ordline_M'+'.Qty5')) , 0) ,;
      Ord6 WITH MAX((Ord6 - EVALUATE('Ordline_M'+'.Qty6')) , 0) ,;
      Ord7 WITH MAX((Ord7 - EVALUATE('Ordline_M'+'.Qty7')) , 0) ,;
      Ord8 WITH MAX((Ord8 - EVALUATE('Ordline_M'+'.Qty8')) , 0) ,;
      TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
*B611762,1 ES 05/29/2019 - Style wrong ordered quantity in the Rebalance program - [End]
    =gfAdd_Info('STYDYE',loFormSet)
    UNLOCK
  ENDIF    && End of IF

  IF !EMPTY(DyeLot) AND SEEK(lcAltStyle + cWareCode + DyeLot , 'STYDYE')
    SELECT STYDYE
    = RLOCK()
 *B611762,1 ES 05/29/2019 - Style wrong ordered quantity in the Rebalance program - [Start]   
*!*	    REPLACE Ord1 WITH MAX((Ord1 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty1')) , 0) ,;
*!*	      Ord2 WITH MAX((Ord2 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty2')) , 0) ,;
*!*	      Ord3 WITH MAX((Ord3 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty3')) , 0) ,;
*!*	      Ord4 WITH MAX((Ord4 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty4')) , 0) ,;
*!*	      Ord5 WITH MAX((Ord5 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty5')) , 0) ,;
*!*	      Ord6 WITH MAX((Ord6 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty6')) , 0) ,;
*!*	      Ord7 WITH MAX((Ord7 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty7')) , 0) ,;
*!*	      Ord8 WITH MAX((Ord8 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty8')) , 0) ,;
*!*	      TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
      
      REPLACE Ord1 WITH MAX((Ord1 - EVALUATE('Ordline_M'+'.Qty1')) , 0) ,;
      Ord2 WITH MAX((Ord2 - EVALUATE('Ordline_M'+'.Qty2')) , 0) ,;
      Ord3 WITH MAX((Ord3 - EVALUATE('Ordline_M'+'.Qty3')) , 0) ,;
      Ord4 WITH MAX((Ord4 - EVALUATE('Ordline_M'+'.Qty4')) , 0) ,;
      Ord5 WITH MAX((Ord5 - EVALUATE('Ordline_M'+'.Qty5')) , 0) ,;
      Ord6 WITH MAX((Ord6 - EVALUATE('Ordline_M'+'.Qty6')) , 0) ,;
      Ord7 WITH MAX((Ord7 - EVALUATE('Ordline_M'+'.Qty7')) , 0) ,;
      Ord8 WITH MAX((Ord8 - EVALUATE('Ordline_M'+'.Qty8')) , 0) ,;
      TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
*B611762,1 ES 05/29/2019 - Style wrong ordered quantity in the Rebalance program - [End]

    =gfAdd_Info('STYDYE',loFormSet)
    UNLOCK
  ENDIF    && End of IF

  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 5
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

*IF nProcNo [The step number (for the rollback)] equal 5
IF nProcNo = 5
  *IF There is a record for the current Style and Warehouse in the STYDYE
  *file
  IF SEEK(STYLE + cWareCode + SPACE(10) , 'STYDYE')
    SELECT STYDYE
    = RLOCK()
*B611762,1 ES 05/29/2019 - Style wrong ordered quantity in the Rebalance program - [Start]
*!*	    REPLACE Ord1 WITH (Ord1 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty1')),;
*!*	      Ord2 WITH (Ord2 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty2')),;
*!*	      Ord3 WITH (Ord3 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty3')),;
*!*	      Ord4 WITH (Ord4 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty4')),;
*!*	      Ord5 WITH (Ord5 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty5')),;
*!*	      Ord6 WITH (Ord6 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty6')),;
*!*	      Ord7 WITH (Ord7 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty7')),;
*!*	      Ord8 WITH (Ord8 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty8')),;
*!*	      TotOrd WITH (TotOrd + EVALUATE(loFormSet.lcTmpOrdLn+'.TotQty'))
     REPLACE Ord1 WITH (Ord1 + MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.Qty1'),EVALUATE('Ordline_M'+'.Qty1'))),;
      Ord2 WITH (Ord2 + MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.Qty2'),EVALUATE('Ordline_M'+'.Qty2'))),;
      Ord3 WITH (Ord3 + MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.Qty3'),EVALUATE('Ordline_M'+'.Qty3'))),;
      Ord4 WITH (Ord4 + MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.Qty4'),EVALUATE('Ordline_M'+'.Qty4'))),;
      Ord5 WITH (Ord5 + MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.Qty5'),EVALUATE('Ordline_M'+'.Qty5'))),;
      Ord6 WITH (Ord6 + MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.Qty6'),EVALUATE('Ordline_M'+'.Qty6'))),;
      Ord7 WITH (Ord7 + MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.Qty7'),EVALUATE('Ordline_M'+'.Qty7'))),;
      Ord8 WITH (Ord8 + MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.Qty8'),EVALUATE('Ordline_M'+'.Qty8'))),;
      TotOrd WITH  Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
*B611762,1 ES 05/29/2019 - Style wrong ordered quantity in the Rebalance program - [End]

    =gfAdd_Info('STYDYE',loFormSet)
    UNLOCK
  ENDIF    && End of IF

  IF !EMPTY(DyeLot) AND SEEK(STYLE + cWareCode + DyeLot , 'STYDYE')
    SELECT STYDYE
    = RLOCK()

*B611762,1 ES 05/29/2019 - Style wrong ordered quantity in the Rebalance program - [Start]
*!*	    REPLACE Ord1 WITH (Ord1 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty1')),;
*!*	      Ord2 WITH (Ord2 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty2')),;
*!*	      Ord3 WITH (Ord3 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty3')),;
*!*	      Ord4 WITH (Ord4 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty4')),;
*!*	      Ord5 WITH (Ord5 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty5')),;
*!*	      Ord6 WITH (Ord6 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty6')),;
*!*	      Ord7 WITH (Ord7 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty7')),;
*!*	      Ord8 WITH (Ord8 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty8')),;
*!*	      TotOrd WITH (TotOrd + EVALUATE(loFormSet.lcTmpOrdLn+'.TotQty'))
     REPLACE Ord1 WITH (Ord1 +  MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.Qty1'),EVALUATE('Ordline_M'+'.Qty1'))),;
      Ord2 WITH (Ord2 +  MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.Qty2'),EVALUATE('Ordline_M'+'.Qty2'))),;
      Ord3 WITH (Ord3 +  MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.Qty3'),EVALUATE('Ordline_M'+'.Qty3'))),;
      Ord4 WITH (Ord4 +  MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.Qty4'),EVALUATE('Ordline_M'+'.Qty4'))),;
      Ord5 WITH (Ord5 +  MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.Qty5'),EVALUATE('Ordline_M'+'.Qty5'))),;
      Ord6 WITH (Ord6 +  MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.Qty6'),EVALUATE('Ordline_M'+'.Qty6'))),;
      Ord7 WITH (Ord7 +  MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.Qty7'),EVALUATE('Ordline_M'+'.Qty7'))),;
      Ord8 WITH (Ord8 +  MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.Qty8'),EVALUATE('Ordline_M'+'.Qty8'))),;
      TotOrd WITH  Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
*B611762,1 ES 05/29/2019 - Style wrong ordered quantity in the Rebalance program - [End]
    =gfAdd_Info('STYDYE',loFormSet)
    UNLOCK
  ENDIF    && End of IF

  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 6
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

*IF nProcNo [The step number (for the rollback)] equal 6
IF nProcNo = 6

  *IF There is a record for the old Style in the STYLE file
  IF SEEK(lcAltStyle , 'STYLE')
    SELECT STYLE
    = RLOCK()

*B611762,1 ES 05/29/2019 - Style wrong ordered quantity in the Rebalance program - [Start]
*!*	    REPLACE Ord1 WITH MAX((Ord1 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty1')) , 0) ,;
*!*	      Ord2 WITH MAX((Ord2 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty2')) , 0) ,;
*!*	      Ord3 WITH MAX((Ord3 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty3')) , 0) ,;
*!*	      Ord4 WITH MAX((Ord4 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty4')) , 0) ,;
*!*	      Ord5 WITH MAX((Ord5 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty5')) , 0) ,;
*!*	      Ord6 WITH MAX((Ord6 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty6')) , 0) ,;
*!*	      Ord7 WITH MAX((Ord7 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty7')) , 0) ,;
*!*	      Ord8 WITH MAX((Ord8 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty8')) , 0) ,;
*!*	      TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
     REPLACE Ord1 WITH MAX((Ord1 - EVALUATE('Ordline_M'+'.Qty1')) , 0) ,;
      Ord2 WITH MAX((Ord2 - EVALUATE('Ordline_M'+'.Qty2')) , 0) ,;
      Ord3 WITH MAX((Ord3 - EVALUATE('Ordline_M'+'.Qty3')) , 0) ,;
      Ord4 WITH MAX((Ord4 - EVALUATE('Ordline_M'+'.Qty4')) , 0) ,;
      Ord5 WITH MAX((Ord5 - EVALUATE('Ordline_M'+'.Qty5')) , 0) ,;
      Ord6 WITH MAX((Ord6 - EVALUATE('Ordline_M'+'.Qty6')) , 0) ,;
      Ord7 WITH MAX((Ord7 - EVALUATE('Ordline_M'+'.Qty7')) , 0) ,;
      Ord8 WITH MAX((Ord8 - EVALUATE('Ordline_M'+'.Qty8')) , 0) ,;
      TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
*B611762,1 ES 05/29/2019 - Style wrong ordered quantity in the Rebalance program - [End]

    =gfAdd_Info('STYLE',loFormSet)
    UNLOCK

  ENDIF    && End of IF
  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 7
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

*IF nProcNo [The step number (for the rollback)] equal 7
IF nProcNo = 7

  *IF There is a record for the current Style in the STYLE file
  IF SEEK(STYLE , 'STYLE')
    SELECT STYLE
    = RLOCK()

*B611762,1 ES 05/29/2019 - Style wrong ordered quantity in the Rebalance program - [Start]
*!*	    REPLACE Ord1 WITH (Ord1 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty1')),;
*!*	      Ord2 WITH (Ord2 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty2')),;
*!*	      Ord3 WITH (Ord3 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty3')),;
*!*	      Ord4 WITH (Ord4 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty4')),;
*!*	      Ord5 WITH (Ord5 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty5')),;
*!*	      Ord6 WITH (Ord6 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty6')),;
*!*	      Ord7 WITH (Ord7 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty7')),;
*!*	      Ord8 WITH (Ord8 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty8')),;
*!*	      TotOrd WITH (TotOrd + EVALUATE(loFormSet.lcTmpOrdLn+'.TotQty'))
     REPLACE Ord1 WITH (Ord1 + MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.Qty1'),EVALUATE('Ordline_M'+'.Qty1'))),;
       Ord2 WITH (Ord2 + MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.Qty2'),EVALUATE('Ordline_M'+'.Qty2'))),;
      Ord3 WITH (Ord3 + MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.Qty3'),EVALUATE('Ordline_M'+'.Qty3'))),;
      Ord4 WITH (Ord4 +MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.Qty4'),EVALUATE('Ordline_M'+'.Qty4'))),;
      Ord5 WITH (Ord5 + MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.Qty5'),EVALUATE('Ordline_M'+'.Qty5'))),;
      Ord6 WITH (Ord6 + MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.Qty6'),EVALUATE('Ordline_M'+'.Qty6'))),;
      Ord7 WITH (Ord7 + MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.Qty7'),EVALUATE('Ordline_M'+'.Qty7'))),;
      Ord8 WITH (Ord8 + MAX(EVALUATE(loFormSet.lcTmpOrdLn+'.Qty8'),EVALUATE('Ordline_M'+'.Qty8'))),;
      TotOrd WITH  Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
*B611762,1 ES 05/29/2019 - Style wrong ordered quantity in the Rebalance program - [End]

    =gfAdd_Info('STYLE',loFormSet)
    UNLOCK
  ENDIF    && End of IF
  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 8
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

*IF nProcNo [The step number (for the rollback)] equal 8
IF nProcNo = 8

  *IF We will need to update the Order Header
 *B611762,1 ES 05/29/2019 - Style wrong ordered quantity in the Rebalance program - [Start]
 * IF lnCnT1 > lnCnT2 .OR. TotBook <> TotQty
   IF lnCnT1 > lnCnT2 .OR. EVALUATE(loFormSet.lcTmpOrdLn+'.TotQty') > Ordline_M.TotQty OR EVALUATE(loFormSet.lcTmpOrdLn+'.TotBook') > Ordline_M.TotBook 
 *B611762,1 ES 05/29/2019 - Style wrong ordered quantity in the Rebalance program - [End]
 
    lnCanc = 0          && Varible to hold the canceled quantity

    *FOR Loop to get the  canceled quantity and amount
    IF lnCnT1 > lnCnT2
      FOR lnElm = lnCnT2 + 1 TO lnCnT1
        lcElm = STR(lnElm , 1)
        lnCanc = lnCanc + Book&lcElm
      ENDFOR    && End of FOR Loop
    ENDIF    && End of IF
    SELECT ORDHDR

    *IF There is a record for this Order line in the ORDHDR file
    IF SEEK('O' + EVALUATE(loFormSet.lcTmpOrdLn+'.Order'))
      =RLOCK()
*B611762,1 ES 05/29/2019 - Style wrong ordered quantity in the Rebalance program - [Start] 
*!*	      REPLACE OPEN      WITH OPEN + EVALUATE(loFormSet.lcTmpOrdLn+'.TotBook') - EVALUATE(loFormSet.lcTmpOrdLn+'.TotQty') - lnCanc ,;
*!*	        OpenAmt   WITH OpenAmt + ((EVALUATE(loFormSet.lcTmpOrdLn+'.TotBook') - EVALUATE(loFormSet.lcTmpOrdLn+'.TotQty') - lnCanc) * EVALUATE(loFormSet.lcTmpOrdLn+'.Price')) ,;
*!*	        BookAmt   WITH BookAmt + ((EVALUATE(loFormSet.lcTmpOrdLn+'.TotBook') - EVALUATE(loFormSet.lcTmpOrdLn+'.TotQty') - lnCanc) * EVALUATE(loFormSet.lcTmpOrdLn+'.Price')) ,;
*!*	        Book      WITH Book + EVALUATE(loFormSet.lcTmpOrdLn+'.TotBook') - EVALUATE(loFormSet.lcTmpOrdLn+'.TotQty') - lnCanc
       REPLACE OPEN      WITH OPEN + EVALUATE(loFormSet.lcTmpOrdLn+'.TotQty') - Ordline_M.TotQty - lnCanc ,;
        OpenAmt   WITH OpenAmt + ((EVALUATE(loFormSet.lcTmpOrdLn+'.TotQty') - Ordline_M.TotQty- lnCanc) * EVALUATE(loFormSet.lcTmpOrdLn+'.Price')) ,;
        BookAmt   WITH BookAmt + ((EVALUATE(loFormSet.lcTmpOrdLn+'.TotBook') - Ordline_M.TotBook - lnCanc) * EVALUATE(loFormSet.lcTmpOrdLn+'.Price')) ,;
        Book      WITH Book + EVALUATE(loFormSet.lcTmpOrdLn+'.TotBook') - Ordline_M.TotBook - lnCanc
*B611762,1 ES 05/29/2019 - Style wrong ordered quantity in the Rebalance program - [End]
      =gfAdd_Info('ORDHDR',loFormSet)
      UNLOCK
    ENDIF
*B611762,1 ES 05/29/2019 - Style wrong ordered quantity in the Rebalance program - [Start]
ELSE
    SELECT ORDHDR
    IF SEEK('O' + EVALUATE(loFormSet.lcTmpOrdLn+'.Order'))
      =RLOCK()
      =gfAdd_Info('ORDHDR',loFormSet)
      UNLOCK
    ENDIF
*B611762,1 ES 05/29/2019 - Style wrong ordered quantity in the Rebalance program - [End]
  ENDIF    && End of IF

  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 9
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

*IF nProcNo [The step number (for the rollback)] equal 9
IF nProcNo = 9

  *IF There is a record for this Order line in the ORDLINE file
  IF SEEK('O' + ORDER + STR(LINENO , 6) , 'ORDLINE')
    SELECT ORDLINE
    = RLOCK()
    REPLACE pik1     WITH pik1 + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo1') ,;
      pik2     WITH pik2 + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo2') ,;
      pik3     WITH pik3 + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo3') ,;
      pik4     WITH pik4 + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo4') ,;
      pik5     WITH pik5 + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo5') ,;
      pik6     WITH pik6 + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo6') ,;
      pik7     WITH pik7 + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo7') ,;
      pik8     WITH pik8 + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo8') ,;
      totpik   WITH totpik + EVALUATE(loFormSet.lcTmpOrdLn+'.Tot_PoAlo') ,;
      pikdate  WITH IIF(totpik = 0 , {} , oAriaApplication.SystemDate) ,;
      PIKTKT   WITH IIF(totpik = 0 , '' ,;
      IIF(EMPTY(PIKTKT),'******',PIKTKT)) ,;
      picked   WITH IIF(totpik = 0 , .F. , .T.)

    =gfAdd_Info('ORDLINE',loFormSet)
    UNLOCK
  ENDIF    && End of IF
  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 10
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

*IF nProcNo [The step number (for the rollback)] equal 10
IF nProcNo = 10

  *IF There is a record for this Order line in the ORDLINE file
  IF SEEK('O' + ORDER + STR(LINENO , 6) , 'ORDLINE')
    SELECT ORDLINE
    = RLOCK()
   *B611762,1 ES 05/29/2019 - Style wrong ordered quantity in the Rebalance program - [Start] 
*!*	    REPLACE Book1  WITH Book1 + EVALUATE(loFormSet.lcTmpOrdLn+'.Book1') - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty1') ,;
*!*	      Book2  WITH IIF(lnCnT2 < 2 , 0 , Book2 + EVALUATE(loFormSet.lcTmpOrdLn+'.Book2') - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty2')) ,;
*!*	      Book3  WITH IIF(lnCnT2 < 3 , 0 , Book3 + EVALUATE(loFormSet.lcTmpOrdLn+'.Book3') - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty3')) ,;
*!*	      Book4  WITH IIF(lnCnT2 < 4 , 0 , Book4 + EVALUATE(loFormSet.lcTmpOrdLn+'.Book4') - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty4')) ,;
*!*	      Book5  WITH IIF(lnCnT2 < 5 , 0 , Book5 + EVALUATE(loFormSet.lcTmpOrdLn+'.Book5') - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty5')) ,;
*!*	      Book6  WITH IIF(lnCnT2 < 6 , 0 , Book6 + EVALUATE(loFormSet.lcTmpOrdLn+'.Book6') - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty6')) ,;
*!*	      Book7  WITH IIF(lnCnT2 < 7 , 0 , Book7 + EVALUATE(loFormSet.lcTmpOrdLn+'.Book7') - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty7')) ,;
*!*	      Book8  WITH IIF(lnCnT2 < 8 , 0 , Book8 + EVALUATE(loFormSet.lcTmpOrdLn+'.Book8') - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty8')) ,;
*!*	      TotBook WITH Book1 + Book2 + Book3 + Book4 + Book5 + Book6 + Book7 + Book8
    REPLACE Book1  WITH MAX(Book1,  EVALUATE(loFormSet.lcTmpOrdLn+'.Book1')),;
      Book2  WITH IIF(lnCnT2 < 2 , 0 , MAX(Book2,  EVALUATE(loFormSet.lcTmpOrdLn+'.Book2'))) ,;
      Book3  WITH IIF(lnCnT2 < 3 , 0 , MAX(Book3,  EVALUATE(loFormSet.lcTmpOrdLn+'.Book3'))) ,;
      Book4  WITH IIF(lnCnT2 < 4 , 0 , MAX(Book4,  EVALUATE(loFormSet.lcTmpOrdLn+'.Book4'))) ,;
      Book5  WITH IIF(lnCnT2 < 5 , 0 , MAX(Book5,  EVALUATE(loFormSet.lcTmpOrdLn+'.Book5'))) ,;
      Book6  WITH IIF(lnCnT2 < 6 , 0 , MAX(Book6,  EVALUATE(loFormSet.lcTmpOrdLn+'.Book6'))) ,;
      Book7  WITH IIF(lnCnT2 < 7 , 0 , MAX(Book7,  EVALUATE(loFormSet.lcTmpOrdLn+'.Book7'))) ,;
      Book8  WITH IIF(lnCnT2 < 8 , 0 , MAX(Book8,  EVALUATE(loFormSet.lcTmpOrdLn+'.Book8'))) ,;
      TotBook WITH Book1 + Book2 + Book3 + Book4 + Book5 + Book6 + Book7 + Book8
*B611762,1 ES 05/29/2019 - Style wrong ordered quantity in the Rebalance program - [End]

    =gfAdd_Info('ORDLINE',loFormSet)
    UNLOCK
  ENDIF    && End of IF
  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 11
  =RLOCK()
  UNLOCK
ENDIF    && End of IF


*IF nProcNo [The step number (for the rollback)] equal 11
IF nProcNo = 11

  *IF There is a record for this Order line in the ORDLINE file
  IF SEEK('O' + ORDER + STR(LINENO , 6) , 'ORDLINE')
    SELECT ORDLINE
    = RLOCK()

    *:   B610517,1 MMT 09/16/2013 Fix bug of empty scale field in ordline after allocation using automatic allocation screen[Start]
    *!*	    REPLACE QTY1      WITH EVALUATE(loFormSet.lcTmpOrdLn+'.Book1') ,;
    *!*	      Qty2      WITH IIF(lnCnT2 < 2 , 0 , EVALUATE(loFormSet.lcTmpOrdLn+'.Book2')) ,;
    *!*	      Qty3      WITH IIF(lnCnT2 < 3 , 0 , EVALUATE(loFormSet.lcTmpOrdLn+'.Book3')) ,;
    *!*	      Qty4      WITH IIF(lnCnT2 < 4 , 0 , EVALUATE(loFormSet.lcTmpOrdLn+'.Book4')) ,;
    *!*	      Qty5      WITH IIF(lnCnT2 < 5 , 0 , EVALUATE(loFormSet.lcTmpOrdLn+'.Book5')) ,;
    *!*	      Qty6      WITH IIF(lnCnT2 < 6 , 0 , EVALUATE(loFormSet.lcTmpOrdLn+'.Book6')) ,;
    *!*	      Qty7      WITH IIF(lnCnT2 < 7 , 0 , EVALUATE(loFormSet.lcTmpOrdLn+'.Book7')) ,;
    *!*	      Qty8      WITH IIF(lnCnT2 < 8 , 0 , EVALUATE(loFormSet.lcTmpOrdLn+'.Book8')) ,;
    *!*	      TotQty    WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
    *!*	      STYLE     WITH IIF(TotPik = 0 .AND. !EVALUATE(loFormSet.lcTmpOrdLn+'.lLok_Stat') , EVALUATE(loFormSet.lcTmpOrdLn+'.AltStyle') , EVALUATE(loFormSet.lcTmpOrdLn+'.Style')),;
    *!*	      AltStyle  WITH IIF(TotPik = 0 , SPACE(12) , EVALUATE(loFormSet.lcTmpOrdLn+'.AltStyle')) ,;
    *!*	      DyeLot    WITH EVALUATE(loFormSet.lcTmpOrdLn+'.DyeLot') ,;
    *!*	      cWareCode WITH EVALUATE(loFormSet.lcTmpOrdLn+'.cWareCode') ,;
    *!*	      SCALE     WITH IIF(TotPik = 0 .AND. !lLok_Stat , lcOScale , EVALUATE(loFormSet.lcTmpOrdLn+'.Scale'))
	
	 *!* B611762,1 DM 10/04/2019 T20181017.0004 - Take Max Qty1 in order not to be greater than the Ordered qty   - {Start}
		*!*	    REPLACE QTY1      WITH EVALUATE(loFormSet.lcTmpOrdLn+'.Book1') ,;
		*!*	      Qty2      WITH IIF(lnCnT2 < 2 , 0 , EVALUATE(loFormSet.lcTmpOrdLn+'.Book2')) ,;
		*!*	      Qty3      WITH IIF(lnCnT2 < 3 , 0 , EVALUATE(loFormSet.lcTmpOrdLn+'.Book3')) ,;
		*!*	      Qty4      WITH IIF(lnCnT2 < 4 , 0 , EVALUATE(loFormSet.lcTmpOrdLn+'.Book4')) ,;
		*!*	      Qty5      WITH IIF(lnCnT2 < 5 , 0 , EVALUATE(loFormSet.lcTmpOrdLn+'.Book5')) ,;
		*!*	      Qty6      WITH IIF(lnCnT2 < 6 , 0 , EVALUATE(loFormSet.lcTmpOrdLn+'.Book6')) ,;
		*!*	      Qty7      WITH IIF(lnCnT2 < 7 , 0 , EVALUATE(loFormSet.lcTmpOrdLn+'.Book7')) ,;
		*!*	      Qty8      WITH IIF(lnCnT2 < 8 , 0 , EVALUATE(loFormSet.lcTmpOrdLn+'.Book8')) ,;
		*!*	      TotQty    WITH QTY1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
		*!*	      STYLE     WITH IIF(totpik = 0 .AND. !EVALUATE(loFormSet.lcTmpOrdLn+'.lLok_Stat') , EVALUATE(loFormSet.lcTmpOrdLn+'.AltStyle') , EVALUATE(loFormSet.lcTmpOrdLn+'.Style')),;
		*!*	      AltStyle  WITH IIF(totpik = 0 , SPACE(12) , EVALUATE(loFormSet.lcTmpOrdLn+'.AltStyle')) ,;
		*!*	      DyeLot    WITH EVALUATE(loFormSet.lcTmpOrdLn+'.DyeLot') ,;
		*!*	      cWareCode WITH EVALUATE(loFormSet.lcTmpOrdLn+'.cWareCode') ,;
		*!*	      SCALE     WITH IIF(totpik = 0 .AND. !EVALUATE(loFormSet.lcTmpOrdLn+'.lLok_Stat'), lcOScale , EVALUATE(loFormSet.lcTmpOrdLn+'.Scale'))
   
   	  REPLACE QTY1      WITH MAX(QTY1,EVALUATE(loFormSet.lcTmpOrdLn+'.Book1')) ,;
      Qty2      WITH IIF(lnCnT2 < 2 , 0 , MaX(QTY2,EVALUATE(loFormSet.lcTmpOrdLn+'.Book2'))) ,;
      Qty3      WITH IIF(lnCnT2 < 3 , 0 , MAX(Qty3,EVALUATE(loFormSet.lcTmpOrdLn+'.Book3'))) ,;
      Qty4      WITH IIF(lnCnT2 < 4 , 0 , MAX(Qty4,EVALUATE(loFormSet.lcTmpOrdLn+'.Book4'))) ,;
      Qty5      WITH IIF(lnCnT2 < 5 , 0 , MAX(Qty5,EVALUATE(loFormSet.lcTmpOrdLn+'.Book5'))) ,;
      Qty6      WITH IIF(lnCnT2 < 6 , 0 , MAX(Qty6,EVALUATE(loFormSet.lcTmpOrdLn+'.Book6'))) ,;
      Qty7      WITH IIF(lnCnT2 < 7 , 0 , MAX(Qty7,EVALUATE(loFormSet.lcTmpOrdLn+'.Book7'))) ,;
      Qty8      WITH IIF(lnCnT2 < 8 , 0 , MAX(Qty8,EVALUATE(loFormSet.lcTmpOrdLn+'.Book8'))) ,;
      TotQty    WITH QTY1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
      STYLE     WITH IIF(totpik = 0 .AND. !EVALUATE(loFormSet.lcTmpOrdLn+'.lLok_Stat') , EVALUATE(loFormSet.lcTmpOrdLn+'.AltStyle') , EVALUATE(loFormSet.lcTmpOrdLn+'.Style')),;
      AltStyle  WITH IIF(totpik = 0 , SPACE(12) , EVALUATE(loFormSet.lcTmpOrdLn+'.AltStyle')) ,;
      DyeLot    WITH EVALUATE(loFormSet.lcTmpOrdLn+'.DyeLot') ,;
      cWareCode WITH EVALUATE(loFormSet.lcTmpOrdLn+'.cWareCode') ,;
      SCALE     WITH IIF(totpik = 0 .AND. !EVALUATE(loFormSet.lcTmpOrdLn+'.lLok_Stat'), lcOScale , EVALUATE(loFormSet.lcTmpOrdLn+'.Scale'))

    *!* B611762,1 DM 10/04/2019 T20181017.0004 - Take Max Qty1 in order not to be greater than the Ordered qty - {End}
    
    *:   B610517,1 MMT 09/16/2013 Fix bug of empty scale field in ordline after allocation using automatic allocation screen[End]

    *!* B610670,1 HIA 02/04/2014 T20140123.0021 - DCC - duplicate Pick Tickets with wrong or no qtys [Begin]
    REPLACE cAllocatBy      WITH  IIF( totpik > 0 , 'A' , ' ')
    *!* B610670,1 HIA 02/04/2014 T20140123.0021 - DCC - duplicate Pick Tickets with wrong or no qtys [End]

    =gfAdd_Info('ORDLINE',loFormSet)
    UNLOCK
  ENDIF    && End of IF
  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 12
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

*IF nProcNo [The step number (for the rollback)] equal 12
IF nProcNo = 12
  loFormSet.lnSelAlo = IIF(totpik = 0 .AND. Tot_PoAlo > 0 , loFormSet.lnSelAlo + 1 ,;
    IIF(totpik > 0 .AND. totpik + Tot_PoAlo = 0 ,;
    loFormSet.lnSelAlo - 1 , loFormSet.lnSelAlo))

  loFormSet.lnAloRec = IIF(totpik = 0 .AND. Tot_PoAlo > 0 , loFormSet.lnAloRec + 1 ,;
    IIF(totpik > 0 .AND. totpik + Tot_PoAlo = 0 ,;
    loFormSet.lnAloRec - 1 , loFormSet.lnAloRec))
  *:   B610517,1 MMT 09/16/2013 Fix bug of empty scale field in ordline after allocation using automatic allocation screen[Start]
  *!*	  REPLACE Pik1     WITH Pik1 + PoAlo1 ,;
  *!*	    Pik2     WITH Pik2 + PoAlo2 ,;
  *!*	    Pik3     WITH Pik3 + PoAlo3 ,;
  *!*	    Pik4     WITH Pik4 + PoAlo4 ,;
  *!*	    Pik5     WITH Pik5 + PoAlo5 ,;
  *!*	    Pik6     WITH Pik6 + PoAlo6 ,;
  *!*	    Pik7     WITH Pik7 + PoAlo7 ,;
  *!*	    Pik8     WITH Pik8 + PoAlo8 ,;
  *!*	    TotPik   WITH TotPik + Tot_PoAlo ,;
  *!*	    PikDate  WITH IIF(TotPik = 0 , {} , oAriaApplication.SystemDate) ,;
  *!*	    PikTkt   WITH IIF(TotPik = 0 , '' ,;
  *!*	    IIF(EMPTY(PikTkt),'******',PikTkt)) ,;
  *!*	    Picked   WITH IIF(TotPik = 0 , .F. , .T.) ,;
  *!*	    QTY1     WITH Book1 ,;
  *!*	    Qty2     WITH IIF(lnCnT2 < 2 , 0 , Book2) ,;
  *!*	    Qty3     WITH IIF(lnCnT2 < 3 , 0 , Book3) ,;
  *!*	    Qty4     WITH IIF(lnCnT2 < 4 , 0 , Book4) ,;
  *!*	    Qty5     WITH IIF(lnCnT2 < 5 , 0 , Book5) ,;
  *!*	    Qty6     WITH IIF(lnCnT2 < 6 , 0 , Book6) ,;
  *!*	    Qty7     WITH IIF(lnCnT2 < 7 , 0 , Book7) ,;
  *!*	    Qty8     WITH IIF(lnCnT2 < 8 , 0 , Book8) ,;
  *!*	    TotQty   WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
  *!*	    STYLE    WITH IIF(TotPik = 0 .AND. !lLok_Stat , AltStyle , STYLE),;
  *!*	    AltStyle WITH IIF(TotPik = 0 , SPACE(12) , AltStyle) ,;
  *!*	    SCALE    WITH IIF(TotPik = 0 .AND. !lLok_Stat , lcOScale , SCALE) ,;
  *!*	    nProcNo  WITH 99
  REPLACE pik1     WITH pik1 + PoAlo1 ,;
    pik2     WITH pik2 + PoAlo2 ,;
    pik3     WITH pik3 + PoAlo3 ,;
    pik4     WITH pik4 + PoAlo4 ,;
    pik5     WITH pik5 + PoAlo5 ,;
    pik6     WITH pik6 + PoAlo6 ,;
    pik7     WITH pik7 + PoAlo7 ,;
    pik8     WITH pik8 + PoAlo8 ,;
    totpik   WITH totpik + Tot_PoAlo ,;
    pikdate  WITH IIF(totpik = 0 , {} , oAriaApplication.SystemDate) ,;
    PIKTKT   WITH IIF(totpik = 0 , '' ,;
    IIF(EMPTY(PIKTKT),'******',PIKTKT)) ,;
    picked   WITH IIF(totpik = 0 , .F. , .T.) ,;
    QTY1     WITH Book1 ,;
    Qty2     WITH IIF(lnCnT2 < 2 , 0 , Book2) ,;
    Qty3     WITH IIF(lnCnT2 < 3 , 0 , Book3) ,;
    Qty4     WITH IIF(lnCnT2 < 4 , 0 , Book4) ,;
    Qty5     WITH IIF(lnCnT2 < 5 , 0 , Book5) ,;
    Qty6     WITH IIF(lnCnT2 < 6 , 0 , Book6) ,;
    Qty7     WITH IIF(lnCnT2 < 7 , 0 , Book7) ,;
    Qty8     WITH IIF(lnCnT2 < 8 , 0 , Book8) ,;
    TotQty   WITH QTY1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
    STYLE    WITH IIF(totpik = 0 .AND. !EVALUATE(loFormSet.lcTmpOrdLn+'.lLok_Stat'), AltStyle , STYLE),;
    AltStyle WITH IIF(totpik = 0 , SPACE(12) , AltStyle) ,;
    SCALE    WITH IIF(totpik = 0 .AND. !EVALUATE(loFormSet.lcTmpOrdLn+'.lLok_Stat'), lcOScale , SCALE) ,;
    nProcNo  WITH 99
  *:   B610517,1 MMT 09/16/2013 Fix bug of empty scale field in ordline after allocation using automatic allocation screen[END]
  IF totpik <> 0
    REPLACE cReason WITH ""
  ENDIF

  =RLOCK()
  UNLOCK

  loFormSet.AriaForm1.cntDetail.keyStyle.VALUE = STYLE
  lnElem = ASCAN(loFormSet.laWareHous,lcWareCode)
  loFormSet.AriaForm1.cntDetail.cboLocation.VALUE = ASUBSCRIPT(loFormSet.laWareHous, lnElem, 1)
  loFormSet.AriaForm1.cntDetail.keyDyelot.keytextbox.VALUE  = IIF(STYLE.cDye_Flg = 'N' , SPACE(10) , DyeLot)
ENDIF    && End of IF      ORDHDR
*-- end of lfEditLine.

*!*************************************************************
*! Name      : lfHandlSel
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Handle Select / UnSelect Cases.
*!*************************************************************
*! Passed Parameters  : Apply Method
*!*************************************************************
*!
FUNCTION lfHandlSel
PARAMETERS lcApplyMth,loFormSet

lnRecNo = RECNO()
STORE 0 TO loFormSet.lnSelRec,loFormSet.lnSelAlo,loFormSet.lnAloRec

SCAN
  IF lnSel = 1
    loFormSet.lnSelRec = loFormSet.lnSelRec + 1
    IF totpik <> 0
      loFormSet.lnSelAlo = loFormSet.lnSelAlo + 1
    ENDIF
  ENDIF
  IF totpik <> 0
    loFormSet.lnAloRec = loFormSet.lnAloRec + 1
  ENDIF
ENDSCAN

IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF
*-- Select / UnSelect Record.
IF lcApplyMth = "SEL_UNSEL"
  =lfSelUnSel(loFormSet)
ELSE  && Sel All, Sel None, Or Invert
  PRIVATE lcScanExpr
  DO CASE
  CASE lcApplyMth = "SEL_ALL"  && Select All Case
    lcScanExpr = [FOR lnSel = 0]

  CASE lcApplyMth = "SEL_NON"  && Select None Case
    lcScanExpr = [FOR lnSel = 1]

  CASE lcApplyMth = "INVERT"  && Invert Case
    lcScanExpr = []

  ENDCASE
  SCAN &lcScanExpr
    =lfSelUnSel(loFormSet)
  ENDSCAN
  GO loFormSet.lnBrRecNo
ENDIF

loFormSet.llSelAllSt = IIF(loFormSet.lnSelRec = RECCOUNT() , .F. , .T.)
loFormSet.llSelNonSt = IIF(loFormSet.lnSelRec = 0 , .F. , .T.)
loFormSet.AriaForm1.grdOrders.cmdSelect.ENABLED     = !EOF(loFormSet.lcTmpOrdLn)
loFormSet.AriaForm1.grdOrders.cmdSelect.REFRESH
loFormSet.AriaForm1.grdOrders.cmdSelectAll.ENABLED  = loFormSet.llSelAllSt
loFormSet.AriaForm1.grdOrders.cmdSelectAll.REFRESH
loFormSet.AriaForm1.grdOrders.cmdSelectNone.ENABLED = loFormSet.llSelNonSt
loFormSet.AriaForm1.grdOrders.cmdSelectNone.REFRESH
loFormSet.AriaForm1.grdOrders.cmdInvert.ENABLED     = !EOF(loFormSet.lcTmpOrdLn)
loFormSet.AriaForm1.grdOrders.cmdInvert.REFRESH

IF loFormSet.lnSelRec = 0
  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *OAriaApplication.oToolBar.ChangeButtonStatus('pbAlo','DISABLE')
  *OAriaApplication.oToolBar.ChangeButtonStatus('pbRel','DISABLE')
  *OAriaApplication.oToolBar.ChangeButtonStatus('pbGen','DISABLE')
  loFormSet.oToolBar.ChangeButtonStatus('pbAlo','DISABLE')
  loFormSet.oToolBar.ChangeButtonStatus('pbRel','DISABLE')
  loFormSet.oToolBar.ChangeButtonStatus('pbGen','DISABLE')
  *HBG [End]
ELSE
  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *OAriaApplication.oToolBar.ChangeButtonStatus('pbAlo','ENABLED')
  loFormSet.oToolBar.ChangeButtonStatus('pbAlo','ENABLED')
  *HBG [End]
  lcRelSt = IIF(loFormSet.lnSelAlo = 0 , 'DISABLE', 'ENABLED')
  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *OAriaApplication.oToolBar.ChangeButtonStatus('pbRel',lcRelSt)
  *OAriaApplication.oToolBar.ChangeButtonStatus('pbGen',lcRelSt)
  loFormSet.oToolBar.ChangeButtonStatus('pbRel',lcRelSt)
  loFormSet.oToolBar.ChangeButtonStatus('pbGen',lcRelSt)
  *HBG [End]
ENDIF

*IF The current record is Selected
IF lnSel = 1
  =lfShowGets(.T.,loFormSet)
ELSE
  loFormSet.llCh3Stat = .F.
  loFormSet.lapikst = .F.
  =lfDisblGts(loFormSet)
ENDIF    && End of IF

*-- end of lfHandlSel.

*!*************************************************************
*! Name      : lfSelUnSel
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Select / UnSelect Record.
*!*************************************************************
*!
FUNCTION lfSelUnSel
PARAMETERS loFormSet

REPLACE lnSel WITH IIF(lnSel = 0 ,1,0)

loFormSet.lnSelRec = IIF(lnSel = 1,loFormSet.lnSelRec + 1,loFormSet.lnSelRec - 1)
loFormSet.lnSelAlo = IIF(totpik <> 0 AND lnSel = 1, loFormSet.lnSelAlo + 1 , loFormSet.lnSelAlo)

*-- end of lfSelUnSel.

*!******************************************************************************
*! Name      : lfChngDate
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Check setup complete date by order line if it is yes
*!           : replace ORDHDR by ORDLINE in the laFiltExp array else replace
*!           : ORDLINE by ORDHDR. Then replace ORDLINE by lcChildFil
*!******************************************************************************
FUNCTION lfChngDate
PARAMETERS loFormSet
PRIVATE lnCompDate

*-- if setup is complete date by order line replace ORDLINE & ORDHDR by lcChildFil in laFiltExp
*-- array, else replace ORDLINE by ORDHDR [Begin]
IF loFormSet.llLinCmplt
  lnCompDate = ASCAN(loFormSet.laFiltExp,'ORDLINE.COMPLETE')
  IF lnCompDate > 0
    loFormSet.laFiltExp[lnCompDate] = "EVAL(lcChildFil+'.COMPLETE')"
  ENDIF
  lnCompDate = ASCAN(loFormSet.laFiltExp,'ORDHDR.COMPLETE')
  IF lnCompDate > 0
    loFormSet.laFiltExp[lnCompDate] = "EVAL(lcChildFil+'.COMPLETE')"
  ENDIF
ELSE
  lnCompDate = ASCAN(loFormSet.laFiltExp,'ORDLINE.COMPLETE')
  IF lnCompDate > 0
    loFormSet.laFiltExp[lnCompDate] = 'ORDHDR.COMPLETE'
  ENDIF
ENDIF && if setup is complete date by order line [Begin]
*-- end of lfChngDate

*!*************************************************************
*! Name      : lfCreatExp
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Function to create the selected critirea expression
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfCreatExp
PARAMETERS lnIncExcMd,loFormSet

PRIVATE lnBlok , lnExpArLen , lnScopExpr , lnOrdrExpr , lnArayLen ,;
  lcScopFile , lcScopFild , lcScopOptm , laTmpIndex

lnBlok = 1        && Varible to hold the OR number

lcScopFile = ' '         && Varible to hold the name of the main file of the data selection
lcScopFild = ' '         && Varible to hold the name of the main field of the data selection
lcScopOptm = ' '         && Varible to hold the full index expression for the Optmization

*DO CASE Statment
DO CASE
  *CASE of Select By all
CASE loFormSet.lcRpScpMod = ' '
  *DO CASE Statment
  DO CASE
    *CASE of there is a selection criteria on the Style
  CASE lfExpFind('STYLE.')
    loFormSet.lcRpScpMod = 'S'
    lcScopFile = 'STYLE.'
    IF lfExpFind('FABRIC')
      lcScopFild = 'FABRIC'
    ENDIF
    *CASE of there is a selection criteria on the Order header
  CASE lfExpFind('ORDHDR.')
    loFormSet.lcRpScpMod = 'O'
    lcScopFile = 'ORDHDR.'
    lcScopFild = 'ORDER'
    lcScopOptm = 'CORDTYPE + ORDER'
  ENDCASE    && End of DO CASE Statment

  *CASE of Select By Style
CASE loFormSet.lcRpScpMod = 'S'
  lcScopFile = 'STYLE.'
  lcScopFild = 'STYLE'
  lcScopOptm = 'STYLE'
  *CASE of Select By Order
CASE loFormSet.lcRpScpMod = 'O'
  lcScopFile = 'ORDHDR.'
  lcScopFild = 'ORDER'
  lcScopOptm = 'CORDTYPE + ORDER'
  *CASE of Select By Account
CASE loFormSet.lcRpScpMod = 'A'
  lcScopFile = 'ORDHDR.'
  lcScopFild = 'ACCOUNT'
  lcScopOptm = 'ACCOUNT + CORDTYPE + ORDER'
  *CASE of Select By Cut ticket
CASE loFormSet.lcRpScpMod = 'K'
  lcScopFile = 'POSHDR1.'
  lcScopFild = 'PO'
  lcScopOptm = 'CBUSDOCU + CSTYTYPE + PO'
  *CASE of Select By PO
CASE loFormSet.lcRpScpMod = 'P'
  lcScopFile = 'POSHDR1.'
  lcScopFild = 'PO'
  lcScopOptm = 'CBUSDOCU + CSTYTYPE + PO'
ENDCASE    && End of DO CASE Statment

*FOR Loop to scan the array laOGVrFlt rows
FOR lnExpArLen = 1 TO ALEN(loFormSet.laFiltExp , 1)
  IF loFormSet.laFiltExp[lnExpArLen,7]="R" AND !('STYLE' $ loFormSet.laFiltExp[lnExpArLen,1]) AND !('CWARECODE' $ loFormSet.laFiltExp[lnExpArLen,1])
    IF USED(loFormSet.laFiltExp[lnExpArLen,6]) AND;
        RECCOUNT(loFormSet.laFiltExp[lnExpArLen,6]) > 0
      loFormSet.lcOptmFile = loFormSet.laFiltExp[lnExpArLen,6]
    ELSE
      loFormSet.lcOptmFile = ''
    ENDIF
    LOOP
  ENDIF


  *IF There is a value for this row or if this is one of the OR rows
  IF !lfEmpty(loFormSet.laFiltExp[lnExpArLen , 6] , loFormSet.laFiltExp[lnExpArLen , 3] = 'D');
      .OR. UPPER(ALLTRIM(loFormSet.laFiltExp[lnExpArLen , 1])) = '.OR.'

    *DO CASE Statment
    DO CASE
      *CASE of one of the OR rows
    CASE UPPER(ALLTRIM(loFormSet.laFiltExp[lnExpArLen , 1])) = '.OR.'
      lnBlok = lnBlok + 1

      *CASE of one of the main file fields
    CASE lcScopFile $ loFormSet.laFiltExp[lnExpArLen , 1]
      lnScopExpr = ALEN(loFormSet.laScopExpr , 1)
      DIMENSION loFormSet.laScopExpr(lnScopExpr + 1 , 2)
      loFormSet.laScopExpr[lnScopExpr + 1 , 1] = lnBlok
      loFormSet.laScopExpr[lnScopExpr + 1 , 2] = lfGetExp(lnExpArLen , lcScopFile , lcScopFild , lcScopOptm)
      *Otherwise
    OTHERWISE
      lnOrdrExpr = ALEN(loFormSet.laNormExpr , 1)
      DIMENSION loFormSet.laNormExpr(lnOrdrExpr + 1 , 2)
      loFormSet.laNormExpr[lnOrdrExpr + 1 , 1] = lnBlok
      loFormSet.laNormExpr[lnOrdrExpr + 1 , 2] = lfGetExp(lnExpArLen , '' , '' , '')
    ENDCASE    && End of DO CASE Statment
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*IF There is any selection criteria on the main file
IF ALEN(loFormSet.laScopExpr , 1) > 1

  *FOR Loop to scan the array loFormSet.laScopExpr rows
  FOR lnArayLen = 2 TO ALEN(loFormSet.laScopExpr , 1)
    loFormSet.laScopExpr[1,2] = loFormSet.laScopExpr[1,2] + IIF(lnArayLen = 2 , '' ,;
      IIF(loFormSet.laScopExpr[lnArayLen,1] <> loFormSet.laScopExpr[lnArayLen - 1,1] ,;
      ') .OR. (' , ' .AND. ')) +;
      loFormSet.laScopExpr[lnArayLen,2]
  ENDFOR    && End of FOR Loop
  loFormSet.laScopExpr[1,2] = '(' + loFormSet.laScopExpr[1,2] + ')'
ENDIF    && End of IF

*IF There is any selection criteria at any file but the main file
IF ALEN(loFormSet.laNormExpr , 1) > 1
  *FOR Loop to scan the array loFormSet.laNormExpr rows
  FOR lnArayLen = 2 TO ALEN(loFormSet.laNormExpr , 1)
    loFormSet.laNormExpr[1,2] = loFormSet.laNormExpr[1,2] + IIF(lnArayLen = 2 , '' ,;
      IIF(loFormSet.laNormExpr[lnArayLen,1] <> loFormSet.laNormExpr[lnArayLen - 1,1] ,;
      ') .OR. (' , ' .AND. ')) +;
      loFormSet.laNormExpr[lnArayLen,2]
  ENDFOR    && End of FOR Loop
  loFormSet.laNormExpr[1,2] = '(' + loFormSet.laNormExpr[1,2] + ')'
ENDIF    && End of IF

loFormSet.llStylRel = IIF(loFormSet.lcRpScpMod <> 'S' , .T. , .F.)
loFormSet.llOrdrRel = IIF(INLIST(loFormSet.lcRpScpMod , 'O' , 'A') , .F. , .T.)

SELECT (loFormSet.lcTmpOrdLn)
SET ORDER TO

*!*************************************************************
*! Name      : lfExpFind
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Function to find if there is any selection criteria
*!             on a specific file
*!*************************************************************
*! Called from : lfCreatExp()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : File name to look for
*!*************************************************************
*! Return      : .T. if found
*!               .F. Otherwise
*!*************************************************************
*
FUNCTION lfExpFind
PARAMETERS lcFile


PRIVATE lnCount
*FOR Loop to scan the array loFormSet.laFiltExp rows
FOR lnCount = 1 TO ALEN(loFormSet.laFiltExp , 1)
  *IF The field belong to the file we are looking for and the value [The
  *right hand side] is not empty
  IF lcFile $ loFormSet.laFiltExp[lnCount, 1] .AND. !EMPTY(loFormSet.laFiltExp[lnCount , 6])
    RETURN .T.
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

RETURN .F.

*!*************************************************************
*! Name      : lfEmpty
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Function to check if the Option grid elmint is empty
*!*************************************************************
*! Called from : lfCreatExp()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) The Option grid value array column
*!                     2) .T. if the field is a Date field ,
*!                        .F. Otherwise
*!*************************************************************
*! Return      : .T. if empty Value , .F. Otherwise
*!*************************************************************
*
FUNCTION lfEmpty
PARAMETERS lcParm , llParm

*IF Character
IF TYPE('lcParm') = 'C'
  lcParm = STRTRAN(lcParm , '|' , '')
  lcParm = IIF(llParm , STRTRAN(lcParm , '/' , '') , lcParm)
ENDIF    && End of IF

RETURN EMPTY(lcParm)


*!*************************************************************
*! Name      : lfSelData
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Function to create the temp. Order lines file
*!*************************************************************
*! Called from : lfvScope()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T. if there is any records for the selection criteria
*!               .F. Otherwise
*!*************************************************************
*
FUNCTION lfSelData
PARAMETERS loFormSet
PRIVATE lcForCond1 , lcOrdHExp , lcStaus , lcBulk ,;
  lnOldRecCo , laTmpIndex , lnProssCurt

lcSTRDATE = SET("Strictdate")
SET STRICTDATE TO 0

IF EMPTY(loFormSet.lcIndexExp)
  STORE IIF(EMPTY(loFormSet.lcOldIndex),lfEvalIndx(),loFormSet.lcOldIndex) TO loFormSet.lcOldIndex,loFormSet.lcIndexExp
ENDIF

*-- lnCrSelect : Save current selected alias no.
*-- lcChildFil : Variable to hold name of child loop file[ORDLINE or Temp. ORDLINE]
PRIVATE lnCrSelect,lcChildFil
lcChildFil = IIF(loFormSet.llExclude,loFormSet.lcTmpOrdLn,'ORDLINE')

SELECT (loFormSet.lcTmpOrdLn)

IF !loFormSet.llExclude
  STORE 0 TO loFormSet.lnDellRec
  SELECT (loFormSet.lcTmpOrdLn)
  DELETE ALL
  =lfErsBldPk(.T.)
  =lfBundBrow(loFormSet)
ENDIF

SET ORDER TO
GO TOP

lnOldRecCo = RECCOUNT(loFormSet.lcTmpOrdLn)        && Variable to hold the old number of records in temp. Order lines file
m.lnSel = 0
m.nProcNo = 0

STORE '' TO  m.Priority,m.Entered,m.cDivision,m.cStyGroup,m.Fabric,;
  m.PATTERN,m.cStyMajor,m.Trancd,m.CtktNo,m.cFabColor,m.cPeggedDye

lcStaus = IIF(INLIST(loFormSet.lcRpScpMod , 'O' , 'A') , 'Status' , 'ORDHDR.Status')    && Variable to hold the name of the Order status field
lcBulk = IIF(INLIST(loFormSet.lcRpScpMod , 'O' , 'A') , 'Bulk' , 'ORDHDR.Bulk')    && Variable to hold the name of the Order bulk field
lcOrdHExp = '(' + IIF(INLIST(loFormSet.lcRpScpMod , 'O' , 'A') , '' , 'ORDHDR.') +;
  'cOrdType <> "C"' + ' .AND. ' + IIF(loFormSet.llRpIncHOr , 'INLIST(' +;
  lcStaus + ',"O","H")' , lcStaus + ' = "O"') + IIF(loFormSet.llRpExlBlk ,;
  ' .AND. ' + lcBulk + ' <> "Y")' , ')')          && Variable to hold the Order header filter

PRIVATE lcForCond2,lcAlocFlt

lcAlocFlt = IIF(loFormSet.lcRpAloNot = "A",'','!')
lcForCond2 = '(TotQty <> 0' +;
  IIF(loFormSet.lcRpAloNot="B",'',' .AND. &lcAlocFlt.Picked') +;
  IIF(loFormSet.llStylRel , ' .AND. !EOF("STYLE") ' +;
  IIF(loFormSet.llUseDyes .AND. loFormSet.llRpExlDye , ' .AND. STYLE.cDye_Flg <> "Y"' , '') , '') +;
  IIF(loFormSet.llRpPikSep , '' , ' .AND. !EMPTY(Group)') +;
  IIF(loFormSet.llRpPikCor , '' , ' .AND. EMPTY(Group)') +;
  IIF(INLIST(loFormSet.lcRpScpMod , 'O' , 'A') , ')' , ' .AND. ' + lcOrdHExp + ')') +;
  IIF(EMPTY(loFormSet.laNormExpr[1,2]) , '' , ' .AND. ' +;
  ALLTRIM(loFormSet.laNormExpr[1,2])) +;
  IIF(loFormSet.lcRpScpMod $ ' |S','.AND. CUSTOMER.STATUS = "A"', '')

*IF We need a relation between the lcChildFil file and the STYLE file
IF loFormSet.llStylRel
  SELECT STYLE
  SET ORDER TO TAG STYLE
  SELECT (lcChildFil)
  SET RELATION TO STYLE INTO STYLE
ENDIF    && End of IF

*IF We need a relation between the lcChildFil file and the ORDHDR file
IF loFormSet.llOrdrRel
  SELECT ORDHDR
  SET ORDER TO TAG ORDHDR
  SELECT (lcChildFil)
  SET RELATION TO 'O' + ORDER INTO ORDHDR ADDITIVE
ENDIF    && End of IF

*DO CASE Statment
DO CASE

  *CASE of Select by Style
CASE loFormSet.lcRpScpMod = 'S'
  *>>>Use Both <condition is needed in lcForCond2>
  lcForCond1 = ALLTRIM(loFormSet.laScopExpr[1,2]) + IIF(EMPTY(loFormSet.laScopExpr[1,2]) ,;
    'Status <> "X"' , ' .AND. Status <> "X"') +;
    IIF(loFormSet.llUseDyes .AND. loFormSet.llRpExlDye ,;
    ' .AND. STYLE.cDye_Flg <> "Y"' , '')      && Variable to hold the Main file filter expression

  PRIVATE lcTmpTag
  lcTmpTag = IIF(loFormSet.llExclude,loFormSet.lcTmStyTag,'ORDLINES')  && Style index.
  SET ORDER TO TAG &lcTmpTag IN (lcChildFil)

  SELECT (lcChildFil)
  SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER ADDITIVE

  SELECT STYLE
  IF EMPTY(loFormSet.lcOptmFile)
    SET ORDER TO
    =lfUpdatSty(.F.,loFormSet)
  ELSE
    SET ORDER TO STYLE
    SELECT(loFormSet.lcOptmFile)
    SCAN
      =lfUpdatSty(.T.,loFormSet)
    ENDSCAN
  ENDIF
  SET ORDER TO TAG STYLE IN STYLE

  SELECT (lcChildFil)
  SET RELATION OFF INTO CUSTOMER


  *CASE of Select by Order or Select by Account
CASE loFormSet.lcRpScpMod = 'O' .OR.  loFormSet.lcRpScpMod = 'A'
  *>>>Use Both - condition is needed in lcForCond1 only
  lcWaitStr = IIF(loFormSet.lcRpScpMod = 'O' , 'order ' , 'account ')
  lcForCond1 = ALLTRIM(loFormSet.laScopExpr[1,2]) + IIF(EMPTY(loFormSet.laScopExpr[1,2]) ,;
    lcOrdHExp , ' .AND. ' + lcOrdHExp) +;
    '.AND. CUSTOMER.STATUS = "A" '
  lcForCond2 = STRTRAN(lcForCond2,'.AND. CUSTOMER.STATUS = "A"')

  PRIVATE lcTmpTag
  lcTmpTag = IIF(loFormSet.llExclude,loFormSet.lcTmpIndex,'ORDLINE')  && most Child file index.
  SET ORDER TO TAG &lcTmpTag IN (lcChildFil)

  SELECT ORDHDR
  IF EMPTY(loFormSet.lcOptmFile)
    SET ORDER TO
    =lfUpdatOrd(.F.,loFormSet)
  ELSE
    IF loFormSet.lcRpScpMod = 'O'
      SET ORDER TO ORDHDR
    ELSE
      SET ORDER TO Ordacct
    ENDIF
    SELECT(loFormSet.lcOptmFile)
    SCAN
      =lfUpdatOrd(.T.,loFormSet)
    ENDSCAN
  ENDIF
  SET ORDER TO TAG ORDHDR IN ORDHDR

  *CASE of Select by Cut ticket
CASE loFormSet.lcRpScpMod = 'K'
  *>>> Use both - condition is needed in lcForCond1 only
  lcForCond1 = ALLTRIM(loFormSet.laScopExpr[1,2]) + IIF(EMPTY(loFormSet.laScopExpr[1,2]) ,;
    'Status <> "X"' , ' .AND. Status <> "X"') +;
    ' .AND. CUSTOMER.STATUS = "A" '
  lcForCond2 = STRTRAN(lcForCond2,'.AND. CUSTOMER.STATUS = "A"')
  IF !loFormSet.llExclude
    SET ORDER TO TAG ORDLINE IN ORDLINE
  ENDIF
  SET ORDER TO TAG (loFormSet.lcTmpIndex) IN (loFormSet.lcTmpOrdLn)

  SELECT POSHDR1
  IF EMPTY(loFormSet.lcOptmFile)
    SET ORDER TO
    =lfUpdatCt(.F.,loFormSet)
  ELSE
    SET ORDER TO POSHDR
    SELECT(loFormSet.lcOptmFile)
    SCAN
      =lfUpdatCt(.T.,loFormSet)
    ENDSCAN
  ENDIF
  SET ORDER TO POSHDR IN POSHDR1

  *CASE of Select by PO
CASE loFormSet.lcRpScpMod = 'P'
  *>>> Use Both - condition is needed in lcForCond1 only
  lcForCond1 = ALLTRIM(loFormSet.laScopExpr[1,2]) + IIF(EMPTY(loFormSet.laScopExpr[1,2]) ,;
    'Status <> "X"' , ' .AND. Status <> "X"') +;
    ' .AND. CUSTOMER.STATUS = "A" '
  lcForCond2 = STRTRAN(lcForCond2,'.AND. CUSTOMER.STATUS = "A"')

  IF !loFormSet.llExclude
    SET ORDER TO TAG ORDLINE IN ORDLINE
  ENDIF
  SET ORDER TO TAG (loFormSet.lcTmpIndex) IN (loFormSet.lcTmpOrdLn)

  SELECT POSHDR1
  IF EMPTY(loFormSet.lcOptmFile)
    SET ORDER TO
    =lfUpdatPo(.F.,loFormSet)
  ELSE
    SET ORDER TO POSHDR
    SELECT(loFormSet.lcOptmFile)
    SCAN
      =lfUpdatPo(.T.,loFormSet)
    ENDSCAN
  ENDIF
  SET ORDER TO TAG POSHDR IN POSHDR1

  *CASE of Select by all
CASE loFormSet.lcRpScpMod = " "
  *>>> use lcForCond2 only
  SELECT (lcChildFil)
  SET ORDER TO
  SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER ADDITIVE
  COUNT FOR &lcForCond2 TO lnTotRec
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loFormSet.oPross.lblFirstLabel.Caption = IIF(loFormSet.llExclude,LANG_AutoAlloc_ExcludingOrder,LANG_AutoAlloc_SelectingOrder)
  loFormSet.oPross.lblFirstLabel.CAPTION = IIF(loFormSet.llExclude,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_ExcludingOrder,loFormSet.GetHeaderText("LANG_AutoAlloc_ExcludingOrder",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_SelectingOrder,loFormSet.GetHeaderText("LANG_AutoAlloc_SelectingOrder",loFormSet.HeaderAlias)))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.oPross.TotalProgress = lnTotRec
  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  loFormSet.oPross.AUTOCENTER = .T.
  *HBG [End]
  loFormSet.oPross.SHOW()
  lnProssCurt = 0
  SCAN FOR &lcForCond2
    IF loFormSet.llExclude
      lnProssCurt = lnProssCurt + 1
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *loFormSet.oPross.lblSecondLabel.Caption = LANG_AutoAlloc_ExcludingOrder + Order
      loFormSet.oPross.lblSecondLabel.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_ExcludingOrder,loFormSet.GetHeaderText("LANG_AutoAlloc_ExcludingOrder",loFormSet.HeaderAlias)) + ORDER
      *N000682,1 11/20/2012 MMT Globlization changes[End]

      loFormSet.oPross.CurrentProgress(lnProssCurt)
      =lfExclRec(loFormSet)
    ELSE  && Scope mode
      lnProssCurt = lnProssCurt + 1
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *loFormSet.oPross.lblSecondLabel.Caption = LANG_AutoAlloc_SelectingOrder + Order
      loFormSet.oPross.lblSecondLabel.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_SelectingOrder,loFormSet.GetHeaderText("LANG_AutoAlloc_SelectingOrder",loFormSet.HeaderAlias)) + ORDER
      *N000682,1 11/20/2012 MMT Globlization changes[End]

      loFormSet.oPross.CurrentProgress(lnProssCurt)
      SCATTER MEMVAR MEMO
      FOR lnI= 1  TO 8
        lcI = STR(lnI,1)
        m.Alo&lcI = m.PIK&lcI
      ENDFOR
      m.TotAlo = m.totpik
      =lfUpdAloVr(loFormSet)
    ENDIF
  ENDSCAN    && End of SCAN Loop
  loFormSet.oPross.HIDE()
  SELECT (lcChildFil)
  SET RELATION OFF INTO CUSTOMER

ENDCASE    && End of DO CASE Statment

SELECT (lcChildFil)
SET RELATION TO
SET ORDER TO TAG ORDLINST IN ORDLINE

GO TOP IN (loFormSet.lcTmpOrdLn)
loFormSet.lnRecNumbr = RECCOUNT(loFormSet.lcTmpOrdLn) - IIF(loFormSet.llExclude,loFormSet.lnDellRec,lnOldRecCo)
loFormSet.lcOptmFile = ""

SET STRICTDATE TO &lcSTRDATE

RETURN IIF(loFormSet.llExclude,!EOF(loFormSet.lcTmpOrdLn),RECCOUNT(loFormSet.lcTmpOrdLn) > lnOldRecCo)
*-- end of lfSelData.

*!*************************************************************
*! Name      : lfRefScr
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Refresh screen if exclude delete all records.
*!*************************************************************
*! Calls     : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfRefScr()
*!*************************************************************
*E300989,1
*
FUNCTION lfRefScr
PARAMETERS loFormSet

SELECT (loFormSet.lcTmpOrdLn)
=lfwBrows(loFormSet)  && Refresh browse and screen.
=lfDisblGts(loFormSet)
*-- Handle status of tool bar and option menu
=lfHandlObj(loFormSet)

*-- end of lfRefScr.

*!*************************************************************
*! Name      : lfEvalIndx
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Create index expression
*!*************************************************************
*! Return      : Index expression.
*!*************************************************************
*
FUNCTION lfEvalIndx

PRIVATE lnI , lcIndExpr , llHaveOrd
llHaveOrd = .F.
lnI = 0
lcIndExpr = ''
FOR lnI = 1 TO 4
  IF EMPTY(loFormSet.laSortAry[lnI,2])
    EXIT
  ENDIF
  IF loFormSet.laSortAry[lnI,2] = [ORDER]
    llHaveOrd = .T.
  ENDIF
  IF !EMPTY(lcIndExpr)
    lcIndExpr = lcIndExpr + '+'
  ENDIF
  lcIndExpr = lcIndExpr + loFormSet.laSortAry[lnI,2]
ENDFOR
lcIndExpr = lcIndExpr + IIF(llHaveOrd,'',[+ ORDER ])
IF loFormSet.llRpExlDye OR !(loFormSet.llFabDye)
  lcIndExpr = lcIndExpr + [+STORE+GROUP+STR(LINENO , 6)]
ELSE
  lcIndExpr = lcIndExpr + [+STORE+FABRIC+CFABCOLOR+GROUP+STYLE+CWARECODE]
ENDIF
RETURN lcIndExpr
*-- end of lfEvalIndx.


*!*************************************************************
*! Name      : lfGenScr
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid function of push button Generate pick tickets
*!*************************************************************
*! Called from : Control Panel [Push button Generate pick tickets] ,
*!               Option popup , lfvScope()
*!*************************************************************
*! Calls       : gfThermo() , lfGetPkTkt() , lfGenPikTk()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfGenScr
PARAMETERS loFormSet

*B610705,1 TMI 03/25/2014 15:22 [Start] DCC - duplicate Pick Tickets with wrong or no qtys 
IF !USED('Ordline_Picked')
  =gfOpenTable("Ordline",'Ordline','SH','Ordline_Picked')
ENDIF

lnSaveRecNum = RECNO(loFormSet.lcTmpOrdLn)

SELECT (loFormSet.lcTmpOrdLn)
SCAN FOR lnSel=1
  IF gfSeek(EVALUATE(loFormSet.lcTmpOrdLn+'.cOrdtype')+EVALUATE(loFormSet.lcTmpOrdLn+'.Order')+STR(EVALUATE(loFormSet.lcTmpOrdLn+'.LineNo'),6),'Ordline_Picked') &&AND !EMPTY(Ordline_Picked.PIKTKT)
    REPLACE PIKTKT WITH Ordline_Picked.PIKTKT,;
      pik1 WITH  Ordline_Picked.pik1,;
      pik2 WITH  Ordline_Picked.pik2,;
      pik3 WITH  Ordline_Picked.pik3,;
      pik4 WITH  Ordline_Picked.pik4,;
      pik5 WITH  Ordline_Picked.pik5,;
      pik6 WITH  Ordline_Picked.pik6,;
      pik7 WITH  Ordline_Picked.pik7,;
      pik8 WITH  Ordline_Picked.pik8,;
      totpik  WITH  Ordline_Picked.totpik,;
      picked  WITH  Ordline_Picked.picked,;
      pikdate WITH  Ordline_Picked.pikdate IN (loFormSet.lcTmpOrdLn)

    REPLACE Qty1 WITH  MIN(qty1,Ordline_Picked.Qty1),;
            Qty2 WITH  MIN(qty2,Ordline_Picked.Qty2),;
            Qty3 WITH  MIN(qty3,Ordline_Picked.Qty3),;
            Qty4 WITH  MIN(qty4,Ordline_Picked.Qty4),;
            Qty5 WITH  MIN(qty5,Ordline_Picked.Qty5),;
            Qty6 WITH  MIN(qty6,Ordline_Picked.Qty6),;
            Qty7 WITH  MIN(qty7,Ordline_Picked.Qty7),;
            Qty8 WITH  MIN(qty8,Ordline_Picked.Qty8),;
            TotQty  WITH  qty1+qty2+qty3+qty4+qty5+qty6+qty7+qty8 IN (loFormSet.lcTmpOrdLn)
  ENDIF
ENDSCAN

IF BETWEEN(lnSaveRecNum,1,RECCOUNT(loFormSet.lcTmpOrdLn))
  GO lnSaveRecNum IN (loFormSet.lcTmpOrdLn)
ENDIF
*B610705,1 TMI 03/25/2014 15:21 [End  ] 


lcStyle    = EVALUATE(loFormSet.lcTmpOrdLn+'.Style')
lcDyelot   = EVALUATE(loFormSet.lcTmpOrdLn+'.Dyelot')
lcWareCode = EVALUATE(loFormSet.lcTmpOrdLn+'.cWareCode')

SELECT (loFormSet.lcTmpOrdLn)

PRIVATE lcPiktkt , lnTotRec , lnCurent , lnRecCount , lnPrepRec
lnTotPik = loFormSet.AriaForm1.cntDetail.txttotalPik.VALUE
*IF Any of the Syle or Warehouse or Dyelot was changed
IF lcStyle + lcWareCode + lcDyelot <> STYLE + cWareCode + DyeLot
  llAloRec = IIF(lnTotPik = 0 , .F. , .T.)
ELSE    && Else
  llAloRec = IIF(loFormSet.llIncOrd .OR. loFormSet.lnchangalo > 0 , .T. , .F.)
ENDIF    && End of IF

*IF The current record was edited
IF llAloRec
  =lfAllocate(3,RECNO(loFormSet.lcTmpOrdLn),loFormSet)
ENDIF    && End of IF

llAloRec = .F.
loFormSet.llIncOrd = .F.
loFormSet.lnchangalo = 0

SET ORDER TO TAG ORDHDR IN ORDHDR
SELECT (loFormSet.lcTmpOrdLn)
SET ORDER TO TAG (loFormSet.lcTmpOrdLn)
SET RELATION TO 'O' + ORDER INTO ORDHDR

COUNT FOR lnSel = 1 AND totpik <> 0 TO lnTotRec

lnCurent = 0                   && Varible to hold the current count to be done for the thermometer

lnRecCount = RECCOUNT() - loFormSet.lnDellRec   && Varible to hold the Total count to be done for the thermometer
lnPrepRec = 0                                  && Varible to hold the current count to be done for the thermometer
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.oPross.lblFirstLabel.Caption = LANG_AutoAlloc_LableProgress
loFormSet.oPross.lblFirstLabel.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LableProgress,loFormSet.GetHeaderText("LANG_AutoAlloc_LableProgress",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

loFormSet.oPross.TotalProgress = lnRecCount
*HBG 1/24/2005 Modify code to apply the new interface [Begin]
loFormSet.oPross.AUTOCENTER = .T.
*HBG [End]
loFormSet.oPross.SHOW()
*SCAN Loop to scan the temp. Order lines file
SCAN
  lnPrepRec = lnPrepRec + 1
  REPLACE nProcNo WITH 0
  loFormSet.oPross.CurrentProgress(lnPrepRec)
ENDSCAN    && End of SCAN Loop

*E303530,1 MMT 11/30/2014 Add Triggers for [T20141118.0011][Start]
IF ASCAN(loFormSet.laEvntTrig,PADR('DONOTPIK',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
  loFormSet.mDoTrigger(PADR('DONOTPIK',10))
ENDIF
*E303530,1 MMT 11/30/2014 Add Triggers for [T20141118.0011][End]

*T20060818.0001(C200876) TMI [Start] Check first if there is enough stock or not before making update in the Bin location Files
IF ASCAN(loFormSet.laEvntTrig,PADR('CHKFIRST',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
  IF !loFormSet.mDoTrigger(PADR('CHKFIRST',10))
    RETURN
  ENDIF
ENDIF
*T20060818.0001(C200876) TMI [End  ]

*SCAN Loop to scan the temp. Order lines file FOR the selected
*and allocated records and for nProcNo < 4
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.oPross.lblFirstLabel.Caption = LANG_AutoAlloc_LableAssign
loFormSet.oPross.lblFirstLabel.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LableAssign,loFormSet.GetHeaderText("LANG_AutoAlloc_LableAssign",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

loFormSet.oPross.TotalProgress = lnTotRec
*HBG 1/24/2005 Modify code to apply the new interface [Begin]
loFormSet.oPross.AUTOCENTER = .T.
*HBG [End]
loFormSet.oPross.SHOW()
SCAN FOR lnSel = 1 .AND. totpik > 0 .AND. nProcNo < 4
  IF !EMPTY(PIKTKT) AND PIKTKT # '******'
    LOOP
  ENDIF
  lnCurent = lnCurent + 1

  *IF the first step for this record
  IF nProcNo = 0
    IF SEEK(ORDER+STORE+cWareCode,loFormSet.lcTmpPkTk)
      lcPiktkt = EVALUATE(loFormSet.lcTmpPkTk+'.PikTkt')
    ELSE
      lcPiktkt = loFormSet.oAlObj.lfGetPkTkt(ORDER , ORDHDR.cDivision , STORE , cWareCode ,;
        loFormSet.lnRpGenNew , IIF(loFormSet.llRpPkHPck,'Y','N'))
    ENDIF
  ENDIF    && End of IF
  =lfGenPikTk(lcPiktkt,loFormSet)
  loFormSet.oPross.lblSecondLabel.CAPTION = ORDER + '/' + STYLE
  loFormSet.oPross.CurrentProgress(lnCurent)
ENDSCAN    && End of SCAN Loop

*E303612,1 MMT 10/25/2015 Add trigger to Automatic allocation to check pickpack table[T20150908.0008][start]
IF ASCAN(loFormSet.laEvntTrig,PADR('CHKPIKPACK',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
  =loFormSet.mDoTrigger(PADR('CHKPIKPACK',10))
ENDIF
*E303612,1 MMT 10/25/2015 Add trigger to Automatic allocation to check pickpack table[T20150908.0008][End]

*IF There was a selected and allocated records
IF lnTotRec > 0
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loFormSet.oPross.lblFirstLabel.Caption = LANG_AutoAlloc_LableAssign
  loFormSet.oPross.lblFirstLabel.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LableAssign,loFormSet.GetHeaderText("LANG_AutoAlloc_LableAssign",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.oPross.CurrentProgress(lnTotRec)
ENDIF    && End of IF

SELECT (loFormSet.lcTmpOrdLn)
SET ORDER TO TAG (loFormSet.lcTmpOrdLn)
LOCATE

*IF The system use Dyelots
IF loFormSet.llUseDyes
  SET RELATION TO STYLE + cWareCode + DyeLot INTO STYDYE
ELSE    && Else
  SET RELATION TO STYLE + cWareCode + SPACE(10) INTO STYDYE
ENDIF    && End of IF

*-- IF There is no records in the file [IF we have generated Pick tickets for
*-- all the records]
IF EOF()
  loFormSet.activemode = 'S'
  =loFormSet.changeMode('S')
ENDIF    && End of IF

*-- Handle status of tool bar and option menu
=lfHandlObj(loFormSet)

SELECT (loFormSet.lcTmpOrdLn)
SET RELATION OFF INTO ORDHDR
IF loFormSet.llReject
  *** Message : "Can  not allocate all lines. Some lines hve been rejected."
  ***           "              < Ok >               "
  =gfModalGen("TRM44122B00000","DIALOG")
ENDIF
loFormSet.changeMode(loFormSet.activemode )
*-- end of lfGenScr.

*!*************************************************************
*! Name      : lfErsBldPk
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Erase and Build temp. Pick file.
*!*************************************************************
*B603111,1
FUNCTION lfErsBldPk
PARAMETERS llBuild
PRIVATE lcCurrAls
lcCurrAls = SELECT(0)

*-- CLOSE piktkts generated in this session [Begin]
IF USED(loFormSet.lcTmpPkTk)
  USE IN (loFormSet.lcTmpPkTk)
ENDIF

IF FILE(oAriaApplication.Workdir + loFormSet.lcTmpPkTk + '.DBF')
  ERASE (oAriaApplication.Workdir+loFormSet.lcTmpPkTk+'.DBF')          && Erase the Temp file.
ENDIF

IF FILE(oAriaApplication.Workdir+loFormSet.lcTmpPkTk+'.CDX')
  ERASE (oAriaApplication.Workdir+loFormSet.lcTmpPkTk+'.CDX')        && Erase the Temp file.
ENDIF

IF llBuild
  CREATE TABLE (oAriaApplication.Workdir + loFormSet.lcTmpPkTk) ;
    (ORDER  C(6), STORE C(8), cWareCode C(6),PIKTKT C(6),cLineNo C(6))
  ZAP
  *B609356,1 SMA 07/25/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
  *INDEX ON Order + Store + cWareCode + PikTkt + cLineNo TAG (loFormSet.lcTmpPkTk) OF ;
  (oAriaApplication.WorkDir+loFormSet.lcTmpPkTk+'.CDX')
  INDEX ON ORDER + STORE + cWareCode + PIKTKT + cLineNo TAG (loFormSet.lcTmpPkTk)
  *B609356,1 SMA 07/25/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
ENDIF
SELECT (lcCurrAls)
*-- end of lfErsBldPk.

*!*************************************************************
*! Name      : lfExclRec
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Exclude this line.
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*!
FUNCTION lfExclRec
PARAMETERS loFormSet

loFormSet.lnDellRec = loFormSet.lnDellRec + 1
DELETE
*-- end of lfExclRec.

*!*************************************************************
*! Name      : lfUpdAloVr
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Update allocation variables at start.
*!*************************************************************
*! Called from : ....
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfUpdAloVr()
*!*************************************************************
FUNCTION lfUpdAloVr
PARAMETERS loFormSet

PRIVATE lnCurSelAl

lnCurSelAl = SELECT(0)
m.TotAvl = 0
FOR lnI= 1  TO 8
  lcI = STR(lnI,1)
  m.Avl&lcI = 0
ENDFOR
STORE "" TO m.Trancd , m.CtktNo , m.cFabColor , m.Priority , m.Entered , m.cDivision,;
  m.cStyGroup  , m.Fabric , m.PATTERN , m.cStyMajor , m.cPeggedDye
= lfFillMVar() &&Get memory variables that will add to temp. file
INSERT INTO (loFormSet.lcTmpOrdLn) FROM MEMVAR

SELECT (loFormSet.lcTmpOrdLn)
REPLACE cSortField WITH EVALUATE(loFormSet.lcIndexExp),lnSel WITH IIF(picked,1,0)


IF !loFormSet.llPartAlo AND totpik < TotQty
  loFormSet.llPartAlo = .T.
ENDIF

SELECT (lnCurSelAl)
*-- end of lfUpdAloVr.

*!*************************************************************
*! Name      : lfFillMVar
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Fill New temporary fields with its data.
*!*************************************************************
*! Calls     : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfFillMVar()
*!*************************************************************
FUNCTION lfFillMVar
m.Priority   = ORDHDR.Priority
m.Entered    = ORDHDR.Entered
m.cDivision  = STYLE.cDivision
m.cStyGroup  = STYLE.cStyGroup
m.Fabric     = STYLE.Fabric
m.PATTERN    = STYLE.PATTERN
m.cStyMajor  = STYLE.cStyMajor
m.cPeggedDye = m.DyeLot

=SEEK(m.Style + m.cWareCode + m.DyeLot , 'STYDYE')
m.TotAvl = 0
FOR lnI= 1  TO 8
  lcI = STR(lnI,1)
  *:   B610534,1 MMT 09/29/2013 Modify Automatic allocation screen to include WIP in Avail. based on setup{T20130910.0026}[Start]
  *m.Avl&lcI = STYDYE.Stk&lcI
  m.Avl&lcI =  IIF(loFormSet.llTotAvlbl,STYDYE.WIP&lcI,0)+STYDYE.Stk&lcI
  *:   B610534,1 MMT 09/29/2013 Modify Automatic allocation screen to include WIP in Avail. based on setup{T20130910.0026}[End]
  m.TotAvl = m.TotAvl + m.Avl&lcI
ENDFOR

*-- if either MF or PO modules is installed
IF ('MF' $ oAriaApplication.CompanyInstalledModules .OR. 'PO' $ oAriaApplication.CompanyInstalledModules)
  *-- if user did not select By (C/T or P/O)
  IF !(loFormSet.lcRpScpMod $ 'KP')
    PRIVATE lnAliasNo,lcPickOrd
    lnAliasNo = SELECT(0)
    *B610272,1 HIA 03/18/2013 Aria XP - DCC R12 slowing the system [T20130111.0006][Start]
    *lcCutpKCond = "TranCd IN ('1,2') AND [Order] = '" + ORDHDR.ORDER + "' AND STR(cOrdLine,6) = " + STR(ORDLINE.LINENO,6)
    *llCutpKBom = .F.
    *IF lfOpenSql(loFormSet,'CUTPICK','CUTPICK1',lcCutpKCond)
    *  SELECT CUTPICK1
    *  LOCATE
    *  llCutpKBom = !EOF()
    *ENDIF

    IF !USED('CUTPICK1')
      =gfOpenTable("CUTPICK",'CUTORD','SH','CUTPICK1')
    ENDIF
    llCutpKBom = gfSeek('1'+ORDHDR.ORDER+STR(m.LineNo,6),'CUTPICK1','CUTORD') OR gfSeek('2'+ORDHDR.ORDER+STR(m.LineNo,6),'CUTPICK1','CUTORD')

    *B610272,1 HIA 03/18/2013 Aria XP - DCC R12 slowing the system [T20130111.0006][End]

    SELECT (lnAliasNo)
  ENDIF
  IF llCutpKBom
    m.Trancd = CUTPICK1.Trancd
    m.CtktNo = CUTPICK1.CtktNo
  ENDIF
ENDIF
*-- Get primary fabric and its corresponding color, from BOM file[Begin
m.cFabColor = ''
IF loFormSet.llFabDye AND !loFormSet.llRpExlDye AND (STYLE.cDye_Flg = 'Y')  AND ;
    !EMPTY(STYLE.Fabric)
  PRIVATE lnAliasNo,lcFabOrd
  lnAliasNo = SELECT(0)
  *B610272,1 HIA 03/18/2013 Aria XP - DCC R12 slowing the system [T20130111.0006][Start]
  *lcStyMaj  = SUBSTR(STYLE.STYLE,1,loFormSet.lnMajorLen)
  lcStyMaj  = SUBSTR(m.Style,1,loFormSet.lnMajorLen)
  *B610272,1 HIA 03/18/2013 Aria XP - DCC R12 slowing the system [T20130111.0006][End]

  lcBomCond = "CINVTYPE = '0001' AND CITMMAJOR = '"+ lcStyMaj +"' AND cCatgTyp = 'F'"
  llFoundBom = .F.
  *B610272,1 HIA 03/18/2013 Aria XP - DCC R12 slowing the system [T20130111.0006][Start]
  *IF lfOpenSql(loFormSet,'BOM','BOM1',lcBomCond)
  *  SELECT BOM1
  *  LOCATE
  *  IF !EOF()
  *    llFoundBom = .T.
  *  ENDIF
  *ENDIF

  IF !USED('BOM1')
    =gfOpenTable('BOM','MULTIBOM','SH','BOM1')
  ENDIF
  =gfSeek('0001'+lcStyMaj,'BOM1','MULTIBOM')
  SELECT 'BOM1'
  LOCATE REST WHILE CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6)='0001'+lcStyMaj FOR cCatgTyp = 'F'
  IF FOUND()
    llFoundBom = .T.
  ENDIF
  *B610272,1 HIA 03/18/2013 Aria XP - DCC R12 slowing the system [T20130111.0006][End]



  IF llFoundBom
    SELECT BOM1
    LOCATE REST FOR LIKE(STRTRAN(CITMMASK,'*','?'),PADR(STYLE.STYLE,19))
    IF FOUND()
      m.cFabColor = IIF(SUBSTR(ITEM,loFormSet.lnFabMajor+2,loFormSet.lnFabNonMaj) # '******' , SUBSTR(ITEM,loFormSet.lnFabMajor+2,loFormSet.lnFabNonMaj), ;
        IIF(loFormSet.lcFree_Clr='C',;
        SUBSTR(STYLE.STYLE,loFormSet.lnNonMajSt,loFormSet.lnColorLen),''))
      IF !EMPTY(m.cFabColor)
        llNotFItem = .T.
        lcitemCond = "CINVTYPE = '0002' AND STYLE = '"+ BOM1.ITEM +"'"
        *B610272,1 HIA 03/18/2013 Aria XP - DCC R12 slowing the system [T20130111.0006][Start]
        *IF lfOpenSql(loFormSet,'ITEM','ITEM1',lcitemCond )
        *  SELECT ITEM1
        *  SET ORDER TO TAG STYLE IN ITEM1
        *  LOCATE
        *  IF !EOF()
        *
        IF !USED('ITEM1')
          =gfOpenTable('ITEM','STYLE','SH','ITEM1')
        ENDIF
        IF gfSeek('0002'+BOM1.ITEM)
          *B610272,1 HIA 03/18/2013 Aria XP - DCC R12 slowing the system [T20130111.0006][End]

          llNotFItem = (ITEM1.cDye_Flg # 'Y')
        ELSE
          llNotFItem = .T.
        ENDIF
        *B610272,1 HIA 03/18/2013 Aria XP - DCC R12 slowing the system [T20130111.0006][Start]
        *ENDIF
        *B610272,1 HIA 03/18/2013 Aria XP - DCC R12 slowing the system [T20130111.0006][End]
        IF llNotFItem
          m.cFabColor = ''
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  SELECT (lnAliasNo)

ENDIF
*-- Get primary fabric and its corresponding color, from BOM file[End..
*-- end of lfFillMVar.


*!*************************************************************
*! Name      : lfUpdatSty
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Update Select by Style.
*!*************************************************************
FUNCTION lfUpdatSty
PARAMETERS llPointSty,loFormSet

PRIVATE lcScanExpr
IF llPointSty
  lcScanExpr = [REST WHILE Style= PADR(EVALUATE(loFormSet.lcOptmFile+'.cStyMajor'),loFormSet.lnMajorLen)]
  lcCountExpr= [Style= PADR(EVALUATE(loFormSet.lcOptmFile+'.cStyMajor'),loFormSet.lnMajorLen)]
  =SEEK(PADR(EVALUATE(loFormSet.lcOptmFile+'.cStyMajor'),loFormSet.lnMajorLen),"Style")
ELSE
  lcScanExpr = []
  lcCountExpr= []
ENDIF

SELECT STYLE
lcExpr = lcCountExpr + IIF(EMPTY(lcCountExpr),'',' AND ') + lcForCond1
COUNT FOR &lcExpr TO lnTotRec
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.oPross.lblFirstLabel.Caption = IIF(loFormSet.llExclude,LANG_AutoAlloc_ExcludingOrder,LANG_AutoAlloc_SelectingOrder)
loFormSet.oPross.lblFirstLabel.CAPTION = IIF(loFormSet.llExclude,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_ExcludingOrder,loFormSet.GetHeaderText("LANG_AutoAlloc_ExcludingOrder",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_SelectingOrder,loFormSet.GetHeaderText("LANG_AutoAlloc_SelectingOrder",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

loFormSet.oPross.TotalProgress = lnTotRec
*HBG 1/24/2005 Modify code to apply the new interface [Begin]
loFormSet.oPross.AUTOCENTER = .T.
*HBG [End]
loFormSet.oPross.SHOW()
lnProssCurt = 0
IF llPointSty
  =SEEK(PADR(EVALUATE(loFormSet.lcOptmFile+'.cStyMajor'),loFormSet.lnMajorLen),"Style")
ENDIF
*SCAN Loop to scan the STYLE file FOR the Style filter expression
SCAN &lcScanExpr FOR &lcForCond1
  *IF There is Order lines for this Style
  IF SEEK(STYLE , lcChildFil)
    =lfStyInLns(loFormSet)
  ENDIF
ENDSCAN    && End of SCAN Loop
*-- end of lfUpdatSty.

*!*************************************************************
*! Name      : lfStyInLns
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Scan style in lines file.
*!*************************************************************
FUNCTION lfStyInLns
PARAMETERS loFormSet

SELECT (lcChildFil)
*SCAN Loop to scan the lcChildFil file FOR the rest of the selectio
*criteria filter expression
SCAN REST WHILE STYLE+DTOS(COMPLETE)+cordtype+ORDER+STORE+STR(LINENO,6) = STYLE.STYLE ;
    FOR   &lcForCond2
  IF loFormSet.llExclude
    lnProssCurt = lnProssCurt + 1
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *loFormSet.oPross.lblSecondLabel.Caption = LANG_AutoAlloc_ExcludingOrder+ Style
    loFormSet.oPross.lblSecondLabel.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_ExcludingOrder,loFormSet.GetHeaderText("LANG_AutoAlloc_ExcludingOrder",loFormSet.HeaderAlias))+ STYLE
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    loFormSet.oPross.CurrentProgress(lnProssCurt)
    =lfExclRec(loFormSet)
  ELSE  && Scope mode
    lnProssCurt = lnProssCurt + 1
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *loFormSet.oPross.lblSecondLabel.Caption = LANG_AutoAlloc_SelectingOrder+ Style
    loFormSet.oPross.lblSecondLabel.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_SelectingOrder,loFormSet.GetHeaderText("LANG_AutoAlloc_SelectingOrder",loFormSet.HeaderAlias))+ STYLE
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    loFormSet.oPross.CurrentProgress(lnProssCurt)
    SCATTER MEMVAR MEMO
    FOR lnI= 1  TO 8
      lcI = STR(lnI,1)
      m.Alo&lcI = m.PIK&lcI
    ENDFOR
    m.TotAlo = m.totpik
    =lfUpdAloVr(loFormSet)
  ENDIF
ENDSCAN    && End of SCAN Loop
loFormSet.oPross.HIDE()

SELECT STYLE
*-- end of lfStyInLns.

*!*************************************************************
*! Name      : lfUpdatOrd
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Update Select by (Order/Account).
*!*************************************************************
FUNCTION lfUpdatOrd
PARAMETERS llPointSty,loFormSet
PRIVATE lcScanExpr , lnProssCurt
IF llPointSty
  IF loFormSet.lcRpScpMod = 'O'
    =SEEK("O"+EVALUATE(loFormSet.lcOptmFile+'.Order'),"OrdHdr")
    lcScanExpr = [REST WHILE cOrdType+Order="O"+EVALUATE(loFormSet.lcOptmFile+'.Order')]
    lcCountExpr = [cOrdType+Order="O"+EVALUATE(loFormSet.lcOptmFile+'.Order')]
  ELSE
    IF SEEK(EVALUATE(loFormSet.lcOptmFile+'.Account')+"O","OrdHdr")
      lcScanExpr = [REST WHILE account+cordtype+order=EVALUATE(loFormSet.lcOptmFile+'.Account')+"O"]
      lcCountExpr= [account+cordtype+order=EVALUATE(loFormSet.lcOptmFile+'.Account')+"O"]
    ELSE
      RETURN
    ENDIF
  ENDIF
ELSE
  lcScanExpr = []
  lcCountExpr= []
ENDIF

*SCAN Loop to scan the ORDHDR file FOR the Order header filter expression
SELECT ORDHDR

IF loFormSet.llLinCmplt
  SET RELATION TO cordtype + ORDER INTO ORDLINE ADDITIVE
ENDIF
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER ADDITIVE
lcExpr = lcCountExpr + IIF(EMPTY(lcCountExpr),'',' AND ') + lcForCond1
COUNT FOR &lcExpr TO lnTotRec
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.oPross.lblFirstLabel.Caption = IIF(loFormSet.llExclude,LANG_AutoAlloc_Excluding + lcWaitStr ,LANG_AutoAlloc_Selecting + lcWaitStr)
loFormSet.oPross.lblFirstLabel.CAPTION = IIF(loFormSet.llExclude,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_Excluding,loFormSet.GetHeaderText("LANG_AutoAlloc_Excluding",loFormSet.HeaderAlias)) + lcWaitStr ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_Selecting,loFormSet.GetHeaderText("LANG_AutoAlloc_Selecting",loFormSet.HeaderAlias)) + lcWaitStr)
*N000682,1 11/20/2012 MMT Globlization changes[End]

loFormSet.oPross.TotalProgress = lnTotRec
*HBG 1/24/2005 Modify code to apply the new interface [Begin]
loFormSet.oPross.AUTOCENTER = .T.
*HBG [End]
loFormSet.oPross.SHOW()
lnProssCurt = 0
IF llPointSty
  IF loFormSet.lcRpScpMod = 'O'
    =SEEK("O"+EVALUATE(loFormSet.lcOptmFile+'.Order'),"OrdHdr")
  ELSE
    =SEEK(EVALUATE(loFormSet.lcOptmFile+'.Account')+"O","OrdHdr")
  ENDIF
ENDIF
SCAN &lcScanExpr FOR &lcForCond1
  *!B610272,1 HIA 03/18/2013 Aria XP - DCC R12 slowing the system [T20130111.0006][Start]
  IF loFormSet.llExclude
    lnProssCurt = lnProssCurt + 1
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *loFormSet.oPross.lblSecondLabel.Caption = LANG_AutoAlloc_Excluding + lcWaitStr + &lcWaitStr
    loFormSet.oPross.lblSecondLabel.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_Excluding,loFormSet.GetHeaderText("LANG_AutoAlloc_Excluding",loFormSet.HeaderAlias)) + lcWaitStr + &lcWaitStr
    *N000682,1 11/20/2012 MMT Globlization changes[End]
    loFormSet.oPross.CurrentProgress(lnProssCurt )

  ELSE  && Scope mode
    lnProssCurt = lnProssCurt + 1
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *loFormSet.oPross.lblSecondLabel.Caption =  LANG_AutoAlloc_Selecting + lcWaitStr + &lcWaitStr
    loFormSet.oPross.lblSecondLabel.CAPTION =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_Selecting,loFormSet.GetHeaderText("LANG_AutoAlloc_Selecting",loFormSet.HeaderAlias)) + lcWaitStr + &lcWaitStr
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    loFormSet.oPross.CurrentProgress(lnProssCurt )

  ENDIF
  *!B610272,1 HIA 03/18/2013 Aria XP - DCC R12 slowing the system [T20130111.0006][End]

  IF SEEK('O' + ORDER , lcChildFil)
    SELECT (lcChildFil)
    *SCAN Loop to scan the (lcChildFil) file FOR the rest of the selection
    *criteria filter expression
    SCAN REST WHILE cordtype+ORDER+STR(LINENO,6) = "O"+ORDHDR.ORDER ;
        FOR &lcForCond2
      IF loFormSet.llExclude
        *!B610272,1 HIA 03/18/2013 Aria XP - DCC R12 slowing the system [T20130111.0006][Start]
        *lnProssCurt = lnProssCurt + 1
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *loFormSet.oPross.lblSecondLabel.Caption = LANG_AutoAlloc_Excluding + lcWaitStr + &lcWaitStr
        *loFormSet.oPross.lblSecondLabel.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_Excluding,loFormSet.GetHeaderText("LANG_AutoAlloc_Excluding",loFormSet.HeaderAlias)) + lcWaitStr + &lcWaitStr
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        *loFormSet.oPross.CurrentProgress(lnProssCurt )
        *!B610272,1 HIA 03/18/2013 Aria XP - DCC R12 slowing the system [T20130111.0006][End]
        =lfExclRec(loFormSet)
      ELSE  && Scope mode
        *!B610272,1 HIA 03/18/2013 Aria XP - DCC R12 slowing the system [T20130111.0006][Start]
        *lnProssCurt = lnProssCurt + 1
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *loFormSet.oPross.lblSecondLabel.Caption =  LANG_AutoAlloc_Selecting + lcWaitStr + &lcWaitStr
        *loFormSet.oPross.lblSecondLabel.CAPTION =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_Selecting,loFormSet.GetHeaderText("LANG_AutoAlloc_Selecting",loFormSet.HeaderAlias)) + lcWaitStr + &lcWaitStr
        *N000682,1 11/20/2012 MMT Globlization changes[End]
        *loFormSet.oPross.CurrentProgress(lnProssCurt )
        *!B610272,1 HIA 03/18/2013 Aria XP - DCC R12 slowing the system [T20130111.0006][End]

        SCATTER MEMVAR MEMO
        FOR lnI= 1  TO 8
          lcI = STR(lnI,1)
          m.Alo&lcI = m.PIK&lcI
        ENDFOR
        m.TotAlo = m.totpik
        =lfUpdAloVr(loFormSet)
      ENDIF
    ENDSCAN    && End of SCAN Loop
    SELECT ORDHDR
  ENDIF    && End of IF
ENDSCAN    && End of SCAN Loop
loFormSet.oPross.HIDE()

IF loFormSet.llLinCmplt
  SET RELATION OFF INTO ORDLINE
ENDIF
SELECT ORDHDR
SET RELATION OFF INTO CUSTOMER
*-- end of lfUpdatOrd.

*!*************************************************************
*! Name      : lfRelQty
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Function to update the needed files to Release the allocation
*!             from the current record of the temp. Order lines file
*!*************************************************************
*! Called from : lfRelScr()
*!*************************************************************
*! Calls       : gfAdd_Info()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfRelQty
PARAMETERS loFormSet

PRIVATE lnCnT1 , lnCnT2 , lcOScale , lnElm , lcElm

lnCnT1 = 0                  && Variable to hold the number of sizes for the current Style
lnCnT2 = 0                  && Variable to hold the number of sizes for the Original Style
lcOScale = ''               && Variable to hold the Scale code of the Original Style

SELECT (loFormSet.lcTmpOrdLn)

*IF This line Style was substituted befor
IF !EMPTY(AltStyle)
  =SEEK(STYLE , 'STYLE')
  =SEEK( 'S'+STYLE.SCALE ,'SCALE')
  lnCnT1 = SCALE.CNT
  =SEEK(AltStyle , 'STYLE')
  =SEEK( 'S'+STYLE.SCALE ,'SCALE')
  lnCnT2 = SCALE.CNT
  lcOScale = SCALE.SCALE
ENDIF    && End of IF

llLastLine = .F.

*-- Update PickLine File.
*IF nProcNo [The step number (for the rollback)] equal 0
IF nProcNo = 0

  IF !EMPTY(PIKTKT) AND (PIKTKT <> '******')
    IF !loFormSet.llOpnPack  AND !USED('PACK_HDR')
      lcAlsNow  = SELECT(0)
      loFormSet.llOpnPack  = gfOpenFile(oAriaApplication.DataDir+'PACK_HDR',oAriaApplication.DataDir+'PACK_HDR','SH')
      SELECT (lcAlsNow)
    ENDIF

    *IF The Pick ticket has a packing list
    SET ORDER TO PACK_HDR IN PACK_HDR
    IF SEEK(PIKTKT,"PACK_HDR")
      *-- Message:
      *-- "picking tickets having packing lists were not released"
      *--                         < Ok >
      =gfModalGen("TRM44004B00000","ALERT")
      RETURN
    ELSE    && Else

      IF SEEK(ORDER+STORE+cWareCode+PIKTKT+STR(LINENO,6),loFormSet.lcTmpPkTk)
        SELECT (loFormSet.lcTmpPkTk)
        DELETE
        SELECT (loFormSet.lcTmpOrdLn)
      ENDIF

      llLastLine = lfBldRel(PIKTKT,loFormSet)

      IF llLastLine

        IF !loFormSet.llOpnPikLn AND !USED('PIKLINE')
          loFormSet.llOpnPikLn = gfOpenFile(oAriaApplication.DataDir+'PIKLINE','','SH')
        ENDIF

        SELECT (loFormSet.lcRelLine)
        SCAN
          SCATTER MEMVAR MEMO
          SELECT PIKLINE
          INSERT INTO ('PIKLINE') FROM MEMVAR
          =gfAdd_Info('PIKLINE',loFormSet)
          SELECT (loFormSet.lcRelLine)
          DELETE
        ENDSCAN
      ENDIF
    ENDIF    && End of IF
  ENDIF

  SELECT (loFormSet.lcTmpOrdLn)
  =RLOCK()
  REPLACE nProcNo   WITH 1
  UNLOCK

ENDIF

*-- Update PIKTKT file.
IF nProcNo = 1
  IF !EMPTY(PIKTKT) AND (PIKTKT <> '******') AND SEEK(PIKTKT,'PIKTKT') AND PIKTKT.STATUS <> 'X'
    IF llLastLine
      SELECT PIKTKT
      REPLACE STATUS WITH "X"
      =gfAdd_Info('PIKTKT',loFormSet)
      *:   B609785,1 MMT 12/26/2011 Creating PIKTKT does not fire an event in request builder[Start]
      lnWorkAria = loFormSet.nWorkArea
      loFormSet.nWorkArea = 'PIKTKT'
      loFormSet.mDoTrigger('DELETE')
      loFormSet.nWorkArea = lnWorkAria
      *E612337,1 MMT 02/10/2021 Automatic Allocation does not update Entity table when picking ticket is created/released [T20201008.0003][Start]
      loFormSet.mupdateentitytable()
      *E612337,1 MMT 02/10/2021 Automatic Allocation does not update Entity table when picking ticket is created/released [T20201008.0003][End]
      *:   B609785,1 MMT 12/26/2011 Creating PIKTKT does not fire an event in request builder[END]
      *:B608316,1 MMT 10/11/2007 convert 3PL Provider Enhancemnt from 27[Start]
      IF 'AS' $ oAriaApplication.CompanyInstalledModules AND SEEK(cWareCode,'WareHous','WareHous') AND  SEEK('W'+WAREHOUS.cThrdPLPr,'EDIACPRT','ACCFACT') AND ;
          SEEK(EDIACPRT.cPartCode+'940','EDIPD','PARTTRANS') AND SEEK('940'+PADR(PIKTKT,40)+'W'+WAREHOUS.cThrdPLPr,'EDITRANS','TYPEKEY')
        SELECT 'EDITRANS'
        DELETE
      ENDIF
      *:B608316,1 MMT 10/11/2007 convert 3PL Provider Enhancemnt from 27(End)
      *:   C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][Start]
      IF ASCAN(loFormSet.laEvntTrig,PADR('CANPCKPACK',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
        =loFormSet.mDoTrigger(PADR('CANPCKPACK',10))
      ENDIF
      *:   C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][End]


    ENDIF
  ENDIF

  SELECT (loFormSet.lcTmpOrdLn)
  =RLOCK()
  REPLACE nProcNo   WITH 2
  UNLOCK

ENDIF

*T20060818.0001(C200876) TMI Custom Release PikTkt from custom table for DL[Start]
IF ASCAN(loFormSet.laEvntTrig,PADR('ALRELAUT',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0 ;
    =loFormSet.mDoTrigger(PADR('ALRELAUT',10))
ENDIF
*T20060818.0001(C200876) TMI [End  ]

IF nProcNo = 2
  *IF There is a record for this Style and Warehouse in the STYDYE file
  IF SEEK(STYLE + cWareCode + SPACE(10) , 'STYDYE')
    SELECT STYDYE
    = RLOCK()
    REPLACE Alo1   WITH Alo1   - EVALUATE(loFormSet.lcTmpOrdLn+'.Pik1') ,;
      Alo2   WITH Alo2   - EVALUATE(loFormSet.lcTmpOrdLn+'.Pik2') ,;
      Alo3   WITH Alo3   - EVALUATE(loFormSet.lcTmpOrdLn+'.Pik3') ,;
      Alo4   WITH Alo4   - EVALUATE(loFormSet.lcTmpOrdLn+'.Pik4') ,;
      Alo5   WITH Alo5   - EVALUATE(loFormSet.lcTmpOrdLn+'.Pik5') ,;
      Alo6   WITH Alo6   - EVALUATE(loFormSet.lcTmpOrdLn+'.Pik6') ,;
      Alo7   WITH Alo7   - EVALUATE(loFormSet.lcTmpOrdLn+'.Pik7') ,;
      Alo8   WITH Alo8   - EVALUATE(loFormSet.lcTmpOrdLn+'.Pik8') ,;
      TotAlo WITH TotAlo - EVALUATE(loFormSet.lcTmpOrdLn+'.TotPik')

    =gfAdd_Info('STYDYE',loFormSet)
    UNLOCK
  ENDIF    && End of IF
  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 3
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

IF nProcNo = 3

  *IF There is a record for this Style and Warehouse and Dyelot in the
  * STYDYE file
  IF !EMPTY(DyeLot) .AND. SEEK(STYLE + cWareCode + DyeLot , 'STYDYE')
    SELECT STYDYE
    = RLOCK()
    REPLACE Alo1   WITH Alo1    - EVALUATE(loFormSet.lcTmpOrdLn+'.Pik1'),;
      Alo2   WITH Alo2    - EVALUATE(loFormSet.lcTmpOrdLn+'.Pik2'),;
      Alo3   WITH Alo3    - EVALUATE(loFormSet.lcTmpOrdLn+'.Pik3'),;
      Alo4   WITH Alo4    - EVALUATE(loFormSet.lcTmpOrdLn+'.Pik4'),;
      Alo5   WITH Alo5    - EVALUATE(loFormSet.lcTmpOrdLn+'.Pik5'),;
      Alo6   WITH Alo6    - EVALUATE(loFormSet.lcTmpOrdLn+'.Pik6'),;
      Alo7   WITH Alo7    - EVALUATE(loFormSet.lcTmpOrdLn+'.Pik7'),;
      Alo8   WITH Alo8    - EVALUATE(loFormSet.lcTmpOrdLn+'.Pik8'),;
      TotAlo WITH TotAlo  - EVALUATE(loFormSet.lcTmpOrdLn+'.TotPik'),;
      Ord1   WITH Ord1    - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty1'),;
      Ord2   WITH Ord2    - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty2'),;
      Ord3   WITH Ord3    - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty3'),;
      Ord4   WITH Ord4    - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty4'),;
      Ord5   WITH Ord5    - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty5'),;
      Ord6   WITH Ord6    - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty6'),;
      Ord7   WITH Ord7    - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty7'),;
      Ord8   WITH Ord8    - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty8'),;
      TotOrd   WITH TotOrd  - EVALUATE(loFormSet.lcTmpOrdLn+'.TotQty')
    =gfAdd_Info('STYDYE',loFormSet)
    UNLOCK
  ENDIF    && End of IF
  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 4
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

IF nProcNo = 4
  *IF There is a record for this Style in the STYLE file
  IF SEEK(STYLE , 'STYLE')
    SELECT STYLE
    = RLOCK()
    REPLACE Alo1   WITH Alo1   - EVALUATE(loFormSet.lcTmpOrdLn+'.Pik1'),;
      Alo2   WITH Alo2   - EVALUATE(loFormSet.lcTmpOrdLn+'.Pik2'),;
      Alo3   WITH Alo3   - EVALUATE(loFormSet.lcTmpOrdLn+'.Pik3'),;
      Alo4   WITH Alo4   - EVALUATE(loFormSet.lcTmpOrdLn+'.Pik4'),;
      Alo5   WITH Alo5   - EVALUATE(loFormSet.lcTmpOrdLn+'.Pik5'),;
      Alo6   WITH Alo6   - EVALUATE(loFormSet.lcTmpOrdLn+'.Pik6'),;
      Alo7   WITH Alo7   - EVALUATE(loFormSet.lcTmpOrdLn+'.Pik7'),;
      Alo8   WITH Alo8   - EVALUATE(loFormSet.lcTmpOrdLn+'.Pik8'),;
      TotAlo WITH TotAlo - EVALUATE(loFormSet.lcTmpOrdLn+'.TotPik')

    =gfAdd_Info('STYLE',loFormSet)
    UNLOCK
  ENDIF    && End of IF
  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 5

  =RLOCK()
  UNLOCK
ENDIF    && End of IF

IF nProcNo = 5
  *IF There is a record for this Order line in the ORDLINE file
  IF SEEK('O' + ORDER + STR(LINENO , 6) , 'ORDLINE')

    *T20060818.0001(C200876) TMI Custom Release PikTkt for binlocation
    IF ASCAN(loFormSet.laEvntTrig,PADR('ALRELORD',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
      =loFormSet.mDoTrigger(PADR('ALRELORD',10))
    ENDIF
    *T20060818.0001(C200876) TMI [End  ]

    SELECT ORDLINE
    = RLOCK()
    REPLACE pik1    WITH 0 ,;
      pik2    WITH 0 ,;
      pik3    WITH 0 ,;
      pik4    WITH 0 ,;
      pik5    WITH 0 ,;
      pik6    WITH 0 ,;
      pik7    WITH 0 ,;
      pik8    WITH 0 ,;
      totpik  WITH 0 ,;
      pikdate WITH {} ,;
      PIKTKT  WITH '' ,;
      picked  WITH .F. ,;
      DyeLot  WITH EVALUATE(loFormSet.lcTmpOrdLn+'.cPeggedDye')

    *!* B610670,1 HIA 02/04/2014 T20140123.0021 - DCC - duplicate Pick Tickets with wrong or no qtys [Begin]
    REPLACE cAllocatBy      WITH  ' '
    *!* B610670,1 HIA 02/04/2014 T20140123.0021 - DCC - duplicate Pick Tickets with wrong or no qtys [End]

    =SEEK('O'+EVALUATE(loFormSet.lcTmpOrdLn+'.Order'),'OrdHdr')
    IF ORDHDR.cWareCode<>cWareCode
      REPLACE cWareCode WITH ORDHDR.cWareCode
    ENDIF

    =gfAdd_Info('ORDLINE',loFormSet)
    UNLOCK
  ENDIF    && End of IF
  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 6
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

IF nProcNo = 6
  *IF This line Style was substituted befor and the the number of sizes for
  *the current Style is greater than the number of sizes for the Original
  *Style
  IF !EMPTY(AltStyle) .AND. lnCnT1 > lnCnT2
    lnCanc = 0          && Varible to hold the canceled quantity
    lnCancAmt = 0       && Varible to hold the canceled amount

    *FOR Loop to get the  canceled quantity and amount
    FOR lnElm = lnCnT2 + 1 TO lnCnT1
      lcElm = STR(lnElm , 1)
      lnCanc = lnCanc + QTY&lcElm
    ENDFOR    && End of FOR Loop
    lnCancAmt = lnCanc * Price
    SELECT ORDHDR
    *IF There is a record for this Order line in the ORDHDR file
    IF SEEK('O' + EVALUATE(loFormSet.lcTmpOrdLn+'.Order'))
      =RLOCK()
      REPLACE OPEN      WITH OPEN - lnCanc ,;
        OpenAmt   WITH OpenAmt - lnCancAmt ,;
        BookAmt   WITH BookAmt - lnCancAmt ,;
        Book      WITH Book - lnCanc

      =gfAdd_Info('ORDHDR',loFormSet)
      UNLOCK
    ENDIF
  ENDIF    && End of IF


  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 7
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

IF nProcNo = 7
  IF !EMPTY(AltStyle)
    SELECT ORDLINE
    *IF There is a record for this Order line in the ORDLINE file
    IF SEEK('O' + ORDER + STR(LINENO , 6))
      =RLOCK()
      REPLACE Qty2 WITH IIF(lnCnT2 < 2 , 0 , Qty2) ,;
        Qty3 WITH IIF(lnCnT2 < 3 , 0 , Qty3) ,;
        Qty4 WITH IIF(lnCnT2 < 4 , 0 , Qty4) ,;
        Qty5 WITH IIF(lnCnT2 < 5 , 0 , Qty5) ,;
        Qty6 WITH IIF(lnCnT2 < 6 , 0 , Qty6) ,;
        Qty7 WITH IIF(lnCnT2 < 7 , 0 , Qty7) ,;
        Qty8 WITH IIF(lnCnT2 < 8 , 0 , Qty8) ,;
        TotQty WITH QTY1 + Qty2 + Qty3 + Qty4 + Qty5 + Qty6 + Qty7 + Qty8 ,;
        Book2 WITH IIF(lnCnT2 < 2 , 0 , Book1) ,;
        Book3 WITH IIF(lnCnT2 < 3 , 0 , Book3) ,;
        Book4 WITH IIF(lnCnT2 < 4 , 0 , Book4) ,;
        Book5 WITH IIF(lnCnT2 < 5 , 0 , Book5) ,;
        Book6 WITH IIF(lnCnT2 < 6 , 0 , Book6) ,;
        Book7 WITH IIF(lnCnT2 < 7 , 0 , Book7) ,;
        Book8 WITH IIF(lnCnT2 < 8 , 0 , Book8) ,;
        TotBook WITH Book1 + Book2 + Book3 + Book4 + Book5 + Book6 + Book7 + Book8 ,;
        STYLE WITH EVALUATE(loFormSet.lcTmpOrdLn+'.AltStyle') ,;
        AltStyle WITH SPACE(12) ,;
        SCALE    WITH lcOScale

      =gfAdd_Info('ORDLINE',loFormSet)
      UNLOCK
    ENDIF    && End of IF
  ENDIF    && End of IF
  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 8
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

IF nProcNo = 8

  *IF This line Style was substituted befor and There is a record for the
  *Current Style and Warehouse in the STYDYE file
  IF !EMPTY(AltStyle) .AND. SEEK(STYLE + cWareCode + SPACE(10) , 'STYDYE')
    SELECT STYDYE
    = RLOCK()
    REPLACE Ord1 WITH MAX((Ord1 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty1')) , 0) ,;
      Ord2 WITH MAX((Ord2 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty2')) , 0) ,;
      Ord3 WITH MAX((Ord3 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty3')) , 0) ,;
      Ord4 WITH MAX((Ord4 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty4')) , 0) ,;
      Ord5 WITH MAX((Ord5 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty5')) , 0) ,;
      Ord6 WITH MAX((Ord6 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty6')) , 0) ,;
      Ord7 WITH MAX((Ord7 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty7')) , 0) ,;
      Ord8 WITH MAX((Ord8 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty8')) , 0) ,;
      TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8

    =gfAdd_Info('STYDYE',loFormSet)
    UNLOCK

  ENDIF    && End of IF
  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 9
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

IF nProcNo = 9
  *IF This line Style was substituted befor and There is a record for the
  *Original Style and Warehouse in the STYDYE file
  IF !EMPTY(AltStyle) .AND. SEEK(AltStyle + cWareCode + SPACE(10) , 'STYDYE')
    SELECT STYDYE
    = RLOCK()
    REPLACE Ord1 WITH (Ord1 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty1')),;
      Ord2 WITH (Ord2 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty2')),;
      Ord3 WITH (Ord3 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty3')),;
      Ord4 WITH (Ord4 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty4')),;
      Ord5 WITH (Ord5 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty5')),;
      Ord6 WITH (Ord6 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty6')),;
      Ord7 WITH (Ord7 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty7')),;
      Ord8 WITH (Ord8 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty8')),;
      TotOrd WITH (TotOrd + EVALUATE(loFormSet.lcTmpOrdLn+'.TotQty'))

    =gfAdd_Info('STYDYE',loFormSet)
    UNLOCK

  ENDIF    && End of IF
  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 10
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

IF nProcNo = 10
  *IF This line Style was substituted befor and There is a record for the
  *Curren Style in the STYLE file
  IF !EMPTY(AltStyle) .AND. SEEK(STYLE , 'STYLE')
    SELECT STYLE
    = RLOCK()
    REPLACE Ord1 WITH MAX((Ord1 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty1')) , 0) ,;
      Ord2 WITH MAX((Ord2 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty2')) , 0) ,;
      Ord3 WITH MAX((Ord3 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty3')) , 0) ,;
      Ord4 WITH MAX((Ord4 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty4')) , 0) ,;
      Ord5 WITH MAX((Ord5 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty5')) , 0) ,;
      Ord6 WITH MAX((Ord6 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty6')) , 0) ,;
      Ord7 WITH MAX((Ord7 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty7')) , 0) ,;
      Ord8 WITH MAX((Ord8 - EVALUATE(loFormSet.lcTmpOrdLn+'.Qty8')) , 0) ,;
      TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8

    =gfAdd_Info('STYLE',loFormSet)
    UNLOCK

  ENDIF    && End of IF
  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 11
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

IF nProcNo = 11
  *IF This line Style was substituted befor and There is a record for the
  *Original Style in the STYLE file
  IF !EMPTY(AltStyle) .AND. SEEK(AltStyle , 'STYLE')
    SELECT STYLE
    = RLOCK()
    REPLACE Ord1 WITH (Ord1 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty1')),;
      Ord2 WITH (Ord2 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty2')),;
      Ord3 WITH (Ord3 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty3')),;
      Ord4 WITH (Ord4 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty4')),;
      Ord5 WITH (Ord5 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty5')),;
      Ord6 WITH (Ord6 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty6')),;
      Ord7 WITH (Ord7 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty7')),;
      Ord8 WITH (Ord8 + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty8')),;
      TotOrd WITH (TotOrd + EVALUATE(loFormSet.lcTmpOrdLn+'.TotQty'))

    =gfAdd_Info('STYLE',loFormSet)
    UNLOCK

  ENDIF    && End of IF
  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 12
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

IF nProcNo = 12
  REPLACE pik1    WITH 0   ,;
    pik2    WITH 0   ,;
    pik3    WITH 0   ,;
    pik4    WITH 0   ,;
    pik5    WITH 0   ,;
    pik6    WITH 0   ,;
    pik7    WITH 0   ,;
    pik8    WITH 0   ,;
    totpik  WITH 0   ,;
    pikdate WITH {}  ,;
    PIKTKT  WITH ''  ,;
    picked  WITH .F. ,;
    DyeLot  WITH cPeggedDye  ,;
    nProcNo WITH 13
  REPLACE Alo1    WITH 0 ,;
    Alo2    WITH 0 ,;
    Alo3    WITH 0 ,;
    Alo4    WITH 0 ,;
    Alo5    WITH 0 ,;
    Alo6    WITH 0 ,;
    Alo7    WITH 0 ,;
    Alo8    WITH 0
  *:   B610573,1 MMT 11/04/2013 Automatic Allocation calculated total picked Qty incorrectly[T20131031.0007][Start]
  REPLACE TotAlo WITH 0
  *:   B610573,1 MMT 11/04/2013 Automatic Allocation calculated total picked Qty incorrectly[T20131031.0007][End]
  REPLACE cReason WITH ""
  =SEEK('O'+EVALUATE(loFormSet.lcTmpOrdLn+'.Order'),'OrdHdr')
  IF ORDHDR.cWareCode<>cWareCode
    REPLACE cWareCode WITH ORDHDR.cWareCode
    *B612690,1 MMT 09/11/2023 Automatic Allocation corrupts the Order Qty of the Styles when the user changes the Pick From warehouse[T-ERP-20230907.0001][Start]
    lnElem     = ASCAN(loFormSet.laWareHous,ORDHDR.cWareCode)
    IF lnElem  > 0
      loFormSet.AriaForm1.cntDetail.cboLocation.VALUE = IIF(lnElem <> 0, ASUBSCRIPT(loFormSet.laWareHous, lnElem, 1),loFormSet.AriaForm1.cntDetail.cboLocation.VALUE)
      lfvWareCode(loFormSet)
    ENDIF  
    *B612690,1 MMT 09/11/2023 Automatic Allocation corrupts the Order Qty of the Styles when the user changes the Pick From warehouse[T-ERP-20230907.0001][End]
  ENDIF
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

IF nProcNo = 13
  *IF This line Style was substituted befor
  IF !EMPTY(AltStyle)
    REPLACE Qty2 WITH IIF(lnCnT2 < 2 , 0 , Qty2) ,;
      Qty3 WITH IIF(lnCnT2 < 3 , 0 , Qty3) ,;
      Qty4 WITH IIF(lnCnT2 < 4 , 0 , Qty4) ,;
      Qty5 WITH IIF(lnCnT2 < 5 , 0 , Qty5) ,;
      Qty6 WITH IIF(lnCnT2 < 6 , 0 , Qty6) ,;
      Qty7 WITH IIF(lnCnT2 < 7 , 0 , Qty7) ,;
      Qty8 WITH IIF(lnCnT2 < 8 , 0 , Qty8) ,;
      TotQty WITH QTY1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
      STYLE WITH AltStyle ,;
      AltStyle WITH SPACE(12) ,;
      SCALE    WITH lcOScale

  ENDIF    && End of IF
  REPLACE nProcNo   WITH 99
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

IF SEEK('O'+EVALUATE(loFormSet.lcTmpOrdLn+'.Order'),'OrdHdr') AND ;
    ORDHDR.cWareCode <> EVALUATE(loFormSet.lcTmpOrdLn+'.cWareCode')
  REPLACE cWareCode WITH ORDHDR.cWareCode IN (loFormSet.lcTmpOrdLn)
ENDIF

*-- end of lfRelQty.

*!*************************************************************
*! Name      : lfBldRel
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Build temp. rele Pick alias.
*!*************************************************************
FUNCTION lfBldRel
PARAMETERS lcRelPik,loFormSet

PRIVATE llFindLine , lcRelPik

IF !USED(loFormSet.lcRelLine)
  CREATE TABLE (oAriaApplication.Workdir+loFormSet.lcRelLine) FROM ARRAY loFormSet.laFileStru
  SELECT (loFormSet.lcRelLine)
  DELETE ALL
ENDIF

SELECT (loFormSet.lcTmpOrdLn)
SCATTER MEMVAR MEMO
INSERT INTO (loFormSet.lcRelLine) FROM MEMVAR
REPLACE PIKTKT WITH SPACE(6)

IF !USED(loFormSet.lcTmpRelPk)
  USE (oAriaApplication.Workdir+loFormSet.lcTmpOrdLn) AGAIN ALIAS (loFormSet.lcTmpRelPk) IN 0
ENDIF
SELECT (loFormSet.lcTmpRelPk)
IF FILE(oAriaApplication.Workdir+loFormSet.lcTmpRelPk+'.CDX')
  SET ORDER TO TAG (loFormSet.lcTmpRelPk) OF (oAriaApplication.Workdir+loFormSet.lcTmpRelPk+'.CDX')
ELSE
  INDEX ON PIKTKT  TAG (loFormSet.lcTmpRelPk) OF (oAriaApplication.Workdir+loFormSet.lcTmpRelPk+'.CDX')
ENDIF
llFindLine = SEEK(lcRelPik)
USE IN (loFormSet.lcTmpRelPk)

SELECT (loFormSet.lcTmpOrdLn)
REPLACE PIKTKT WITH lcRelPik

RETURN !llFindLine
*-- end of lfBldRel.

*!*************************************************************
*! Name      : lfAloScr
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid function of push button Allocate
*!*************************************************************
*! Called from : Control Panel [Push button Allocate] , Option popup
*!*************************************************************
*! Calls       : lfAllocate() , lfvpbSel() , lfShowGets() ,
*!               lfRefresh()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfAloScr
PARAMETERS loFormSet
SELECT (loFormSet.lcTmpOrdLn)

lcStyle    = loFormSet.AriaForm1.cntDetail.keyStyle.VALUE
lnWareCode = loFormSet.AriaForm1.cntDetail.cboLocation.VALUE
lcWareCode = loFormSet.laWareHous[lnWareCode,2]
lcDyelot   = loFormSet.AriaForm1.cntDetail.keyDyelot.keytextbox.VALUE
lnTotPik = loFormSet.AriaForm1.cntDetail.txttotalPik.VALUE
*IF Any of the Syle or Warehouse or Dyelot was changed
IF lcStyle + lcWareCode + lcDyelot <> STYLE + cWareCode + DyeLot
  llAloRec = IIF(lnTotPik = 0 , .F. , .T.)
ELSE    && Else
  llAloRec = IIF(loFormSet.llIncOrd .OR. loFormSet.lnchangalo > 0 , .T. , .F.)
ENDIF    && End of IF

*IF The current record was edited
IF llAloRec
  =lfAllocate(3 , RECNO(loFormSet.lcTmpOrdLn),loFormSet)
ENDIF    && End of IF

llAloRec = .F.
loFormSet.llIncOrd = .F.
loFormSet.lnchangalo = 0


loFormSet.lnBrRecNo = RECNO(loFormSet.lcTmpOrdLn)

=lfAllocate(2,.F.,loFormSet)

SELECT (loFormSet.lcTmpOrdLn)

*IF The system use Dyelots
IF loFormSet.llUseDyes
  SET RELATION TO STYLE + cWareCode + DyeLot INTO STYDYE
ELSE    && Else
  SET RELATION TO STYLE + cWareCode + SPACE(10) INTO STYDYE
ENDIF    && End of IF
GO loFormSet.lnBrRecNo


*-- Handle status of tool bar and option menu
=lfHandlObj(loFormSet)


=SEEK(STYLE , 'STYLE')
=SEEK('S' + STYLE.SCALE , 'SCALE')

=lfBundFlds(loFormSet)

*IF the current record is selected
IF lnSel = 1
  =lfShowGets(.T.,loFormSet)
ELSE
  loFormSet.llCh3Stat = .F.
  loFormSet.lapikst   = .F.
  =lfDisblGts(loFormSet)
ENDIF    && End of IF


*!*************************************************************
*! Name      : lfGetExp
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Function to create expression from the Option grid
*!             variable filter array rows
*!*************************************************************
*! Called from : lfCreatExp()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1)Option grid variable filter array row number ,
*!                     2)Main file for the data selection
*!                     3)Main field for the data selection
*!                     4)Full index expression for the Optmization
*!*************************************************************
*! Return      : The expression string
*!*************************************************************
*
FUNCTION lfGetExp

PARAMETERS lnElmeNumb , lcScopFile , lcScopFild , lcScopOptm

PRIVATE lcReturn , lcField , lcTypeSep1 , lcTypeSep2 , lcVal

lcReturn = ''          && Varible to hold the value to be returened

lcField = ALLTRIM(UPPER(loFormSet.laFiltExp[lnElmeNumb , 1]))        && Varible to hold the field name

*IF There is a Main file for the data selection
IF !EMPTY(lcScopFile)
  lcField = STRTRAN(lcField , lcScopFile , '')
ENDIF    && End of IF

*IF There is a Main field for the data selection
IF !EMPTY(lcScopFild)
  lcField = STRTRAN(lcField , lcScopFild , lcScopOptm)
ENDIF    && End of IF

lcTypeSep1 = IIF(loFormSet.laFiltExp[lnElmeNumb , 3] = 'C', '"' ,;
  IIF(loFormSet.laFiltExp[lnElmeNumb , 3] = 'D' , '{' , ''))    && Varible to hold the Begin seperator

lcTypeSep2 = IIF(loFormSet.laFiltExp[lnElmeNumb , 3] = 'C', '"' ,;
  IIF(loFormSet.laFiltExp[lnElmeNumb , 3] = 'D' , '}' , ''))    && Varible to hold the End seperator

*DO CASE Statment
DO CASE
CASE lcScopFild = 'PO'
  lcVal = lcTypeSep1 + 'PP' + STRTRAN(loFormSet.laFiltExp[lnElmeNumb , 6] , '|' ,;
    lcTypeSep2 + ',' + lcTypeSep1 + 'PP') + lcTypeSep2        && Varible to hold the right hand side of the expression

CASE lcScopFild = 'ORDER' .AND.  lcField = lcScopOptm
  lcVal = lcTypeSep1 + 'O' + STRTRAN(loFormSet.laFiltExp[lnElmeNumb , 6] , '|' ,;
    lcTypeSep2 + ',' + lcTypeSep1 + 'O') + lcTypeSep2        && Varible to hold the right hand side of the expression

CASE lcScopFild = 'FABRIC'
  SELECT (loFormSet.laFiltExp[lnElmeNumb , 6])
  lcValues = ""
  llFirst  = .T.
  SCAN
    lcValues = lcValues + IIF(llFirst,"","|") + PADR(cStyMajor,7)
    llFirst  = .F.
  ENDSCAN
  lcVal = lcTypeSep1 + STRTRAN(lcValues , '|' ,lcTypeSep2 + ',' + lcTypeSep1) + lcTypeSep2        && Varible to hold the right hand side of the expression

OTHERWISE
  IF loFormSet.laFiltExp[lnElmeNumb , 7] = "R"
    lcValues = ""
    llFirst  = .T.
    SELECT (loFormSet.laFiltExp[lnElmeNumb , 6])
    SCAN
      lcValues = lcValues + IIF(llFirst,"","|") + EVALUATE(ALLTRIM(SUBSTR(loFormSet.laFiltExp[lnElmeNumb,1],ATC('.',loFormSet.laFiltExp[lnElmeNumb,1])+1,LEN(loFormSet.laFiltExp[lnElmeNumb,1]))))
      llFirst  = .F.
    ENDSCAN
    lcVal = lcTypeSep1 + STRTRAN(lcValues , '|' ,lcTypeSep2 + ',' + lcTypeSep1) + lcTypeSep2        && Varible to hold the right hand side of the expression
  ELSE
    lcVal = lcTypeSep1 + STRTRAN(lfTStr(loFormSet.laFiltExp[lnElmeNumb , 6]) , '|' ,;
      lcTypeSep2 + ',' + lcTypeSep1) + lcTypeSep2        && Varible to hold the right hand side of the expression
  ENDIF

ENDCASE    && End of DO CASE Statment

*DO CASE Statment
DO CASE

  *CASE The operator is [IS LIKE]
CASE UPPER(ALLTRIM(loFormSet.laFiltExp[lnElmeNumb , 5])) = 'LIKE'
  lcReturn = lcField + ' ' +;
    IIF(loFormSet.laFiltExp[lnElmeNumb , 4] , '= ' , '<> ') + lcVal

  *CASE The operator is [GREATER THAN]
CASE UPPER(ALLTRIM(loFormSet.laFiltExp[lnElmeNumb , 5])) = 'GREATER THAN'
  lcReturn = lcField + ' ' +;
    IIF(loFormSet.laFiltExp[lnElmeNumb , 4] , '> ' , '<= ') + lcVal

  *CASE The operator is [LESS THAN]
CASE UPPER(ALLTRIM(loFormSet.laFiltExp[lnElmeNumb , 5])) = 'LESS THAN'
  lcReturn = lcField + ' ' +;
    IIF(loFormSet.laFiltExp[lnElmeNumb , 4] , '< ' , '>= ') + lcVal

  *CASE The operator is [GREATER OR EQUAL]
CASE UPPER(ALLTRIM(loFormSet.laFiltExp[lnElmeNumb , 5])) = 'GREATER OR EQUAL'
  lcReturn = lcField + ' ' +;
    IIF(loFormSet.laFiltExp[lnElmeNumb , 4] , '>= ' , '< ') + lcVal

  *CASE The operator is [LESS OR EQUAL]
CASE UPPER(ALLTRIM(loFormSet.laFiltExp[lnElmeNumb , 5])) = 'LESS OR EQUAL'
  lcReturn = lcField + ' ' +;
    IIF(loFormSet.laFiltExp[lnElmeNumb , 4] , '<= ' , '> ') + lcVal

  *CASE The operator is [BETWEEN]
CASE UPPER(ALLTRIM(loFormSet.laFiltExp[lnElmeNumb , 5])) = 'BETWEEN'
  lcReturn = IIF(loFormSet.laFiltExp[lnElmeNumb , 4] , '' , '!') +;
    'BETWEEN(' + lcField + ',' + lcVal + ')'

  *CASE The operator is [IN LIST]
CASE UPPER(ALLTRIM(loFormSet.laFiltExp[lnElmeNumb , 5])) = 'IN LIST'
  IF lcScopFild = 'FABRIC'
    lcField  = 'FABRIC'
  ENDIF
  *B607789,1 WAM 09/28/2006 Comment Hend changes and Return the original statment
  *:   B610039,1 MMT 08/08/2012 Automatic Allocation screen gives error when user select more than 25 codes from OG[Start]
  lcTmpCrName = gfTempName()
  IF !EMPTY(loFormSet.laFiltExp[lnElmeNumb , 6]) AND !USED(loFormSet.laFiltExp[lnElmeNumb , 6]) AND lfConvertToCursor(loFormSet.laFiltExp[lnElmeNumb , 6],'CCodeName',lcTmpCrName)
    lcReturn = IIF(loFormSet.laFiltExp[lnElmeNumb , 4] , '' , '!') +;
      "Seek("+lcField+",'"+lcTmpCrName +"')"
  ELSE
    *:   B610039,1 MMT 08/08/2012 Automatic Allocation screen gives error when user select more than 25 codes from OG[END]
    lcReturn = IIF(loFormSet.laFiltExp[lnElmeNumb , 4] , '' , '!') +;
      'INLIST(' + lcField + ',' + lcVal + ')'
    *:   B610039,1 MMT 08/08/2012 Automatic Allocation screen gives error when user select more than 25 codes from OG[Start]
  ENDIF
  *:   B610039,1 MMT 08/08/2012 Automatic Allocation screen gives error when user select more than 25 codes from OG[End]
  *B607674,1 HBG 07/26/2005 Fix bug of too many urgument in case of InList [Begin]
  *lcReturn = IIF(loFormSet.laFiltExp[lnElmeNumb , 4] , '' , '!') +;
  'INLIST(' + lcField + ',' + lcVal + ')'
  *lcReturn = "SEEK("+lcField +",'"+loFormSet.laFiltExp[lnElmeNumb , 6]+"')"
  *B607674,1 [End]
  *B607789,1 WAM 09/28/2006 (End)

ENDCASE    && End of DO CASE Statment
RETURN lcReturn

*!*************************************************************
*! Name      : lfGenPikTk
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Function to update the needed files to Generate a Pick
*!             ticket for the current record of the temp. Order lines file
*!*************************************************************
*! Called from : lfGenScr()
*!*************************************************************
*! Calls       : gfAdd_Info()
*!*************************************************************
*! Passed Parameters : Pick ticket number
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfGenPikTk
PARAMETERS lcParm,loFormSet

*IF nProcNo [The step number (for the rollback)] equal 0
IF nProcNo = 0
  REPLACE PIKTKT  WITH lcParm ,;
    nProcNo WITH 1
  =RLOCK()
  UNLOCK
ENDIF    && End of IF


*:B608316,1 MMT 10/11/2007 convert 3PL Provider Enhancemnt from 27[Start]
lcTmpOrdLn = loFormSet.lcTmpOrdLn
IF 'AS' $ oAriaApplication.CompanyInstalledModules AND SEEK(&lcTmpOrdLn..cWareCode,'WareHous','WareHous') AND  SEEK('W'+WAREHOUS.cThrdPLPr,'EDIACPRT','ACCFACT') AND ;
    SEEK(EDIACPRT.cPartCode+'940','EDIPD','PARTTRANS')
  lnUseAlas   = ALIAS()
  SELECT EDITRANS
  IF !SEEK('940'+PADR(&lcTmpOrdLn..PIKTKT,40)+'W'+WAREHOUS.cThrdPLPr,'EDITRANS','TYPEKEY')
    INSERT INTO 'EDITRANS' (CEDITRNTYP,KEY,TYPE,CPARTNER,lInterComp) VALUES ;
      ('940',&lcTmpOrdLn..PIKTKT,'W',WAREHOUS.cThrdPLPr,EDIACPRT.lInterComp)
  ENDIF
  REPLACE cStatus WITH 'N'
  =gfAdd_Info('EDITRANS')
  SELECT(lnUseAlas)
ENDIF
*:B608316,1 MMT 10/11/2007 convert 3PL Provider Enhancemnt from 27(End)



PRIVATE lcOrdlOrd
lcOrdlOrd = ORDER("ORDLINE")
SET ORDER TO ORDLINE IN ORDLINE

IF nProcNo = 1
  *IF There is a record for this Order line in the ORDLINE file
  IF SEEK('O' + ORDER + STR(LINENO , 6) , 'ORDLINE')
    SELECT ORDLINE
    = RLOCK()
    REPLACE PIKTKT  WITH EVALUATE(loFormSet.lcTmpOrdLn+'.PikTkt') , pikdate WITH oAriaApplication.SystemDate

    =gfAdd_Info('ORDLINE',loFormSet)
    UNLOCK
  ENDIF    && End of IF
  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 2
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

*IF nProcNo [The step number (for the rollback)] equal 2
IF nProcNo = 2

  SELECT PIKTKT

  *IF There is a no record for this Pick ticket number in the PIKTKT file
  IF !SEEK(EVALUATE(loFormSet.lcTmpOrdLn+'.PikTkt'))
    APPEND BLANK
    = RLOCK()
    REPLACE ACCOUNT   WITH EVALUATE(loFormSet.lcTmpOrdLn+'.Account') ,;
      STORE     WITH EVALUATE(loFormSet.lcTmpOrdLn+'.Store') ,;
      ORDER     WITH EVALUATE(loFormSet.lcTmpOrdLn+'.Order') ,;
      PIKTKT    WITH EVALUATE(loFormSet.lcTmpOrdLn+'.PikTkt') ,;
      DATE      WITH oAriaApplication.SystemDate ,;
      cWareCode WITH EVALUATE(loFormSet.lcTmpOrdLn+'.cWareCode') ,;
      CustPo    WITH IIF(ORDHDR.MultiPo , EVALUATE(loFormSet.lcTmpOrdLn+'.CustPo') , ORDHDR.CustPo) ,;
      STATUS    WITH 'O'

    =gfAdd_Info('PIKTKT',loFormSet)
    UNLOCK
    *:   B609785,1 MMT 12/26/2011 Creating PIKTKT does not fire an event in request builder[Start]
    lnWorkAria = loFormSet.nWorkArea
    loFormSet.nWorkArea = 'PIKTKT'
    loFormSet.mDoTrigger('INSERT')
    loFormSet.nWorkArea = lnWorkAria
    *E612337,1 MMT 02/10/2021 Automatic Allocation does not update Entity table when picking ticket is created/released [T20201008.0003][Start]
    loFormSet.mupdateentitytable()
    *E612337,1 MMT 02/10/2021 Automatic Allocation does not update Entity table when picking ticket is created/released [T20201008.0003][End]
    
    *:   B609785,1 MMT 12/26/2011 Creating PIKTKT does not fire an event in request builder[END]
  ENDIF    && End of IF

  m.Order = ORDER
  m.Store = STORE
  m.cWareCode = cWareCode
  m.PIKTKT = PIKTKT
  m.cLineNo = STR(EVALUATE(loFormSet.lcTmpOrdLn+'.LineNo'),6)
  INSERT INTO (loFormSet.lcTmpPkTk) FROM MEMVAR

  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 3
  =RLOCK()
  UNLOCK
  *:   C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][Start]
  IF ASCAN(loFormSet.laEvntTrig,PADR('UPDPCKPACK',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
    =loFormSet.mDoTrigger(PADR('UPDPCKPACK',10))
  ENDIF
  *:   C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][End]
ENDIF    && End of IF

SET ORDER TO &lcOrdlOrd. IN ORDLINE

*T20060818.0001(C200876) TMI Custom Save PikTkt To custom table for DL[Start]
IF ASCAN(loFormSet.laEvntTrig,PADR('SVBNALOC',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
  PRIVATE loActFormSet
  loActFormSet = loFormSet
  =loFormSet.mDoTrigger(PADR('SVBNALOC',10))
ENDIF
*T20060818.0001(C200876) TMI [End  ]

*-- end of lfGenPikTk.


*!*************************************************************
*! Name      : lfUpdatPo
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Update Select by (PO).
*!*************************************************************
FUNCTION lfUpdatPo
PARAMETERS llPointSty,loFormSet

PRIVATE lcScanExpr , lnProssCurt

SELECT POSHDR1

SELECT ORDLINE
SET RELATION TO 'M'+ORDLINE.ACCOUNT INTO CUSTOMER ADDITIVE

SELECT POSHDR1
IF llPointSty
  lcScanExpr = [REST WHILE cBusDocu+cStyType+Po= "PP"+EVALUATE(loFormSet.lcOptmFile+'.PO')]
  lcCountExpr= [cBusDocu+cStyType+Po= "PP"+EVALUATE(loFormSet.lcOptmFile+'.PO')]
  =SEEK("PP"+EVALUATE(loFormSet.lcOptmFile+'.PO'),"POSHDR1")
ELSE
  lcScanExpr = []
  lcCountExpr = []
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcWaitMsg = IIF(loFormSet.llExclude,LANG_AutoAlloc_WaitMsgExcl,LANG_AutoAlloc_WaitMsgSelc)
lcWaitMsg = IIF(loFormSet.llExclude,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_WaitMsgExcl,loFormSet.GetHeaderText("LANG_AutoAlloc_WaitMsgExcl",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_WaitMsgSelc,loFormSet.GetHeaderText("LANG_AutoAlloc_WaitMsgSelc",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

lcExpr = lcCountExpr + IIF(EMPTY(lcCountExpr),'',' AND ') + lcForCond1
COUNT FOR &lcExpr TO lnTotRec
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.oPross.lblFirstLabel.Caption = lcWaitMsg + LANG_AutoAlloc_WaitMsgPO
loFormSet.oPross.lblFirstLabel.CAPTION = lcWaitMsg + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_WaitMsgPO,loFormSet.GetHeaderText("LANG_AutoAlloc_WaitMsgPO",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

loFormSet.oPross.TotalProgress = lnTotRec
*HBG 1/24/2005 Modify code to apply the new interface [Begin]
loFormSet.oPross.AUTOCENTER = .T.
*HBG [End]
loFormSet.oPross.SHOW()
lnProssCurt = 0
IF llPointSty
  =SEEK("PP"+EVALUATE(loFormSet.lcOptmFile+'.PO'),"POSHDR1")
ENDIF
SCAN &lcScanExpr FOR &lcForCond1

  lnProssCurt = lnProssCurt + 1
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loFormSet.oPross.lblSecondLabel.Caption = lcWaitMsg + LANG_AutoAlloc_WaitMsgPO + PO
  loFormSet.oPross.lblSecondLabel.CAPTION = lcWaitMsg + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_WaitMsgPO,loFormSet.GetHeaderText("LANG_AutoAlloc_WaitMsgPO",loFormSet.HeaderAlias)) + PO
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.oPross.CurrentProgress(lnProssCurt )

  *IF There is lines for this PO in the CUTPICK file
  lcCutpKCond = "TranCd = '2' AND ctktno = '" + PO + "'"
  llCutpKBom = .F.
  IF lfopensql(loFormSet,'CUTPICK','CUTPICK1',lcCutpKCond)
    SELECT CUTPICK1
    LOCATE
    llCutpKBom = !EOF()
  ENDIF
  IF llCutpKBom
    SELECT CUTPICK1
    SCAN
      IF loFormSet.llExclude
        IF SEEK('O' + ORDER + cOrdLine , loFormSet.lcTmpOrdLn)
          SELECT (loFormSet.lcTmpOrdLn)
          SCAN REST FOR &lcForCond2
            IF (TOTCUT - CUTPICK1.TotQty) = 0
              =lfExclRec(loFormSet)
            ELSE
              REPLACE Cut1 WITH Cut1 - CUTPICK1.QTY1 ,;
                Cut2 WITH Cut2 - CUTPICK1.Qty2 ,;
                Cut3 WITH Cut3 - CUTPICK1.Qty3 ,;
                Cut4 WITH Cut4 - CUTPICK1.Qty4 ,;
                Cut5 WITH Cut5 - CUTPICK1.Qty5 ,;
                Cut6 WITH Cut6 - CUTPICK1.Qty6 ,;
                Cut7 WITH Cut7 - CUTPICK1.Qty7 ,;
                Cut8 WITH Cut8 - CUTPICK1.Qty8 ,;
                TOTCUT  WITH TOTCUT - CUTPICK1.TotQty

              IF !(lcOldSlct $ 'KP')
                REPLACE ExcCut1 WITH ExcCut1 + CUTPICK1.QTY1 ,;
                  ExcCut2 WITH ExcCut2 + CUTPICK1.Qty2 ,;
                  ExcCut3 WITH ExcCut3 + CUTPICK1.Qty3 ,;
                  ExcCut4 WITH ExcCut4 + CUTPICK1.Qty4 ,;
                  ExcCut5 WITH ExcCut5 + CUTPICK1.Qty5 ,;
                  ExcCut6 WITH ExcCut6 + CUTPICK1.Qty6 ,;
                  ExcCut7 WITH ExcCut7 + CUTPICK1.Qty7 ,;
                  ExcCut8 WITH ExcCut8 + CUTPICK1.Qty8 ,;
                  TotExcCut  WITH TotExcCut + CUTPICK1.TotQty
              ENDIF
            ENDIF
          ENDSCAN
        ENDIF
      ELSE      && Normal scope case

        *IF There is no record for this CUTPICK order line in the Temp.
        *Order lines file
        IF !SEEK('O' + ORDER + cOrdLine , loFormSet.lcTmpOrdLn)

          *IF There is a record for this CUTPICK order line in the
          *ORDLINE file
          IF SEEK('O' + ORDER + cOrdLine , 'ORDLINE')
            *IF Statment to check the rest of the selection criteria
            *filter expression
            lnCrSelect = SELECT(0)
            SELECT ORDLINE
            IF &lcForCond2
              SCATTER MEMVAR MEMO
              m.Cut1 = CUTPICK1.QTY1
              m.Cut2 = CUTPICK1.Qty2
              m.Cut3 = CUTPICK1.Qty3
              m.Cut4 = CUTPICK1.Qty4
              m.Cut5 = CUTPICK1.Qty5
              m.Cut6 = CUTPICK1.Qty6
              m.Cut7 = CUTPICK1.Qty7
              m.Cut8 = CUTPICK1.Qty8
              m.TOTCUT = CUTPICK1.TotQty
              FOR lnI= 1  TO 8
                lcI = STR(lnI,1)
                m.Alo&lcI = m.PIK&lcI
              ENDFOR
              m.TotAlo = m.totpik

              =lfUpdAloVr(loFormSet)
            ENDIF    && End of IF
            SELECT (lnCrSelect)
          ENDIF    && End of IF
        ELSE    && Else
          SELECT (loFormSet.lcTmpOrdLn)
          REPLACE Cut1 WITH Cut1 + CUTPICK1.QTY1 ,;
            Cut2 WITH Cut2 + CUTPICK1.Qty2 ,;
            Cut3 WITH Cut3 + CUTPICK1.Qty3 ,;
            Cut4 WITH Cut4 + CUTPICK1.Qty4 ,;
            Cut5 WITH Cut5 + CUTPICK1.Qty5 ,;
            Cut6 WITH Cut6 + CUTPICK1.Qty6 ,;
            Cut7 WITH Cut7 + CUTPICK1.Qty7 ,;
            Cut8 WITH Cut8 + CUTPICK1.Qty8 ,;
            TOTCUT  WITH TOTCUT + CUTPICK1.TotQty
        ENDIF    && End of IF
      ENDIF      && end if loFormSet.llExclude
      SELECT CUTPICK1
    ENDSCAN    && End of SCAN Loop
    SELECT POSHDR1
  ENDIF    && End of IF
ENDSCAN    && End of SCAN Loop
loFormSet.oPross.HIDE()

SELECT POSHDR1

SELECT ORDLINE
SET RELATION OFF INTO CUSTOMER
SELECT POSHDR1
*-- end of lfUpdatPo.

*!*************************************************************
*! Name      : lfUpdatCt
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Update Select by (C/T).
*!*************************************************************
FUNCTION lfUpdatCt
PARAMETERS llPointSty,loFormSet
PRIVATE lcScanExpr, lnProssCurt


SELECT POSHDR1
SELECT ORDLINE
SET RELATION TO 'M'+ORDLINE.ACCOUNT INTO CUSTOMER ADDITIVE
SELECT POSHDR1
LOCATE
IF llPointSty
  lcScanExpr = [REST WHILE cBusDocu+cStyType+Po = 'PU' + EVALUATE(loFormSet.lcOptmFile+'.Po')]
  lcCountExpr= [cBusDocu+cStyType+Po = 'PU' + EVALUATE(loFormSet.lcOptmFile+'.Po')]
  =SEEK('PU'+EVALUATE(loFormSet.lcOptmFile+'.PO'),"POSHDR1")
ELSE
  lcScanExpr = []
  lcCountExpr= []
ENDIF
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcWaitMsg = IIF(loFormSet.llExclude,LANG_AutoAlloc_WaitMsgExcl,LANG_AutoAlloc_WaitMsgSelc)
lcWaitMsg = IIF(loFormSet.llExclude,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_WaitMsgExcl,loFormSet.GetHeaderText("LANG_AutoAlloc_WaitMsgExcl",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_WaitMsgSelc,loFormSet.GetHeaderText("LANG_AutoAlloc_WaitMsgSelc",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

lcExpr = lcCountExpr + IIF(EMPTY(lcCountExpr),'',' AND ') + lcForCond1
COUNT FOR &lcExpr TO lnTotRec
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.oPross.lblFirstLabel.Caption = lcWaitMsg + LANG_AutoAlloc_WaitMsgCT
loFormSet.oPross.lblFirstLabel.CAPTION = lcWaitMsg + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_WaitMsgCT,loFormSet.GetHeaderText("LANG_AutoAlloc_WaitMsgCT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

loFormSet.oPross.TotalProgress = lnTotRec
*HBG 1/24/2005 Modify code to apply the new interface [Begin]
loFormSet.oPross.AUTOCENTER = .T.
*HBG [End]
loFormSet.oPross.SHOW()
lnProssCurt = 0
IF llPointSty
  =SEEK('PU'+EVALUATE(loFormSet.lcOptmFile+'.PO'),"POSHDR1")
ENDIF
SCAN &lcScanExpr FOR &lcForCond1
  lnProssCurt = lnProssCurt + 1
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loFormSet.oPross.lblSecondLabel.Caption = lcWaitMsg + LANG_AutoAlloc_WaitMsgCT + PO
  loFormSet.oPross.lblSecondLabel.CAPTION = lcWaitMsg + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_WaitMsgCT,loFormSet.GetHeaderText("LANG_AutoAlloc_WaitMsgCT",loFormSet.HeaderAlias)) + PO
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.oPross.CurrentProgress(lnProssCurt )
  *IF There is lines for this Cut ticket in the CUTPICK file
  lcCutpKCond = "TranCd = '1' AND ctktno = '" + PO + "'"
  llCutpKBom = .F.
  IF lfopensql(loFormSet,'CUTPICK','CUTPICK1',lcCutpKCond)
    SELECT CUTPICK1
    LOCATE
    llCutpKBom = !EOF()
  ENDIF
  IF llCutpKBom
    SELECT CUTPICK1
    SCAN
      IF loFormSet.llExclude
        *E300989,1 This new code for exclude condition. [Begin]
        IF SEEK('O' + ORDER + cOrdLine , loFormSet.lcTmpOrdLn)
          SELECT (loFormSet.lcTmpOrdLn)
          SCAN REST FOR &lcForCond2
            IF (TOTCUT - CUTPICK1.TotQty) = 0
              =lfExclRec(loFormSet)
            ELSE

              REPLACE Cut1 WITH Cut1 - CUTPICK1.QTY1 ,;
                Cut2 WITH Cut2 - CUTPICK1.Qty2 ,;
                Cut3 WITH Cut3 - CUTPICK1.Qty3 ,;
                Cut4 WITH Cut4 - CUTPICK1.Qty4 ,;
                Cut5 WITH Cut5 - CUTPICK1.Qty5 ,;
                Cut6 WITH Cut6 - CUTPICK1.Qty6 ,;
                Cut7 WITH Cut7 - CUTPICK1.Qty7 ,;
                Cut8 WITH Cut8 - CUTPICK1.Qty8 ,;
                TOTCUT  WITH TOTCUT - CUTPICK1.TotQty

              IF !(lcOldSlct $ 'KP')
                REPLACE ExcCut1 WITH ExcCut1 + CUTPICK1.QTY1 ,;
                  ExcCut2 WITH ExcCut2 + CUTPICK1.Qty2 ,;
                  ExcCut3 WITH ExcCut3 + CUTPICK1.Qty3 ,;
                  ExcCut4 WITH ExcCut4 + CUTPICK1.Qty4 ,;
                  ExcCut5 WITH ExcCut5 + CUTPICK1.Qty5 ,;
                  ExcCut6 WITH ExcCut6 + CUTPICK1.Qty6 ,;
                  ExcCut7 WITH ExcCut7 + CUTPICK1.Qty7 ,;
                  ExcCut8 WITH ExcCut8 + CUTPICK1.Qty8 ,;
                  TotExcCut  WITH TotExcCut + CUTPICK1.TotQty
              ENDIF

            ENDIF
          ENDSCAN
        ENDIF
      ELSE      && Normal scope case
        *IF There is no record for this CUTPICK order line in the Temp.
        *Order lines file
        IF !SEEK('O' + ORDER + cOrdLine , loFormSet.lcTmpOrdLn)

          *IF There is a record for this CUTPICK1 order line in the
          *ORDLINE file
          IF SEEK('O' + ORDER + cOrdLine , 'ORDLINE')
            SELECT ORDLINE

            *IF Statment to check the rest of the selection criteria
            *filter expression
            IF &lcForCond2
              SCATTER MEMVAR MEMO
              m.Cut1 = CUTPICK1.QTY1
              m.Cut2 = CUTPICK1.Qty2
              m.Cut3 = CUTPICK1.Qty3
              m.Cut4 = CUTPICK1.Qty4
              m.Cut5 = CUTPICK1.Qty5
              m.Cut6 = CUTPICK1.Qty6
              m.Cut7 = CUTPICK1.Qty7
              m.Cut8 = CUTPICK1.Qty8
              m.TOTCUT = CUTPICK1.TotQty
              FOR lnI= 1  TO 8
                lcI = STR(lnI,1)
                m.Alo&lcI = m.PIK&lcI
              ENDFOR
              m.TotAlo = m.totpik

              =lfUpdAloVr(loFormSet)
            ENDIF    && End of IF
            SELECT CUTPICK1
          ENDIF    && End of IF
        ELSE    && Else
          SELECT (loFormSet.lcTmpOrdLn)
          REPLACE Cut1 WITH Cut1 + CUTPICK1.QTY1 ,;
            Cut2 WITH Cut2 + CUTPICK1.Qty2 ,;
            Cut3 WITH Cut3 + CUTPICK1.Qty3 ,;
            Cut4 WITH Cut4 + CUTPICK1.Qty4 ,;
            Cut5 WITH Cut5 + CUTPICK1.Qty5 ,;
            Cut6 WITH Cut6 + CUTPICK1.Qty6 ,;
            Cut7 WITH Cut7 + CUTPICK1.Qty7 ,;
            Cut8 WITH Cut8 + CUTPICK1.Qty8 ,;
            TOTCUT  WITH TOTCUT + CUTPICK1.TotQty

        ENDIF    && End of IF
      ENDIF  && end if loFormSet.llExclude
      SELECT CUTPICK1
    ENDSCAN    && End of SCAN Loop
    SELECT POSHDR1
  ENDIF    && End of IF
ENDSCAN    && End of SCAN Loop
loFormSet.oPross.HIDE()

SELECT POSHDR1
SELECT ORDLINE
SET RELATION OFF INTO CUSTOMER
SELECT POSHDR1
*-- end of lfUpdatCt.


*!*************************************************************
*! Name      : lfNormAlo
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : allocation with out dyelot (Separates)
*!*************************************************************
*! Called from : lfAllocate
*!*************************************************************
*! Calls       : lfNormProc,gfThermo
*!*************************************************************
*! Passed Parameters : Allocation case (All Records/Selected Records)
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfNormAlo(1)
*!*************************************************************
FUNCTION lfNormAlo
PARAMETERS lnAlcCase,loFormSet


*SCAN Loop to scan the temp. Order lines file for nProcNo less than 5
*and to skip Style with dyelot Yes and empty dyelot field

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.oPross.lblFirstLabel.Caption = LANG_AutoAlloc_ProgrsAllo
loFormSet.oPross.lblFirstLabel.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_ProgrsAllo,loFormSet.GetHeaderText("LANG_AutoAlloc_ProgrsAllo",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

loFormSet.oPross.TotalProgress = lnTotRec
*HBG 1/24/2005 Modify code to apply the new interface [Begin]
loFormSet.oPross.AUTOCENTER = .T.
*HBG [End]
loFormSet.oPross.SHOW()
llCanAlo = .F.

*-* C201125 Hesham Elmasry (HES) Just to enter the next Condition
IF ASCAN(loFormSet.laEvntTrig,PADR('GROPALLO',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0 AND gfGetMemVar('M_MULALLOC')
  loFormSet.lnOldCor = loFormSet.lnRpPikCor
ENDIF
*-* C201125 Hesham Elmasry (HES)

*-* C201125 Hesham Elmasry (HES) Fill the empty group with a special charachter
IF ASCAN(loFormSet.laEvntTrig,PADR('NORALFEG',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
  loFormSet.mDoTrigger(PADR('NORALFEG',10))
ENDIF
*-* C201125 Hesham Elmasry (HES)

*:   B610579,1 MMT 11/10/2013 Automtaic allocation hanges when first sort by Start Date[Start]
lnCurrAlias = SELECT(0)
CREATE CURSOR 'OrderGroups' (cSortField C(80),ORDER C(6))
INDEX ON cSortField TAG 'OrderGrp'
SELECT(lnCurrAlias)
llOrderError = .F.
*:   B610579,1 MMT 11/10/2013 Automtaic allocation hanges when first sort by Start Date[End]


SCAN FOR nProcNo < 5 .AND. (STYLE.cDye_Flg # "Y") AND IIF(lnAlcCase=1,.T.,(lnSel <> 0))
  *IF The line is Allocated befor
  llCanAlo = .T.

  *:   B610579,1 MMT 11/10/2013 Automtaic allocation hanges when first sort by Start Date[Start]
  lnCurrSelAls = ALIAS()
  IF !SEEK(&lnCurrSelAls..cSortField,'OrderGroups','OrderGrp')
    INSERT INTO 'OrderGroups' VALUES (&lnCurrSelAls..cSortField,&lnCurrSelAls..ORDER)
  ELSE
    llOrderError = .T.
    EXIT
  ENDIF
  *:   B610579,1 MMT 11/10/2013 Automtaic allocation hanges when first sort by Start Date[End]

  =lfNormProc(loFormSet)  && Normal process.
  *-- if you must skip this record, because it's allocated before.
  IF loFormSet.llMustLoop
    loFormSet.llMustLoop = .F.
    LOOP
  ENDIF
ENDSCAN    && End of SCAN Loop

*:B610579,1 MMT 11/10/2013 Automtaic allocation hanges when first sort by Start Date[Start]
IF llOrderError
  SELECT 'OrderGroups'
  SET ORDER TO
  GO BOTTOM
  =gfModalGen("TRM44151B00000" , "DIALOG",OrderGroups.ORDER)
  SELECT(lnCurrAlias)
  RETURN
ENDIF
*:B610579,1 MMT 11/10/2013 Automtaic allocation hanges when first sort by Start Date[End]


*-* C201125 Hesham Elmasry (HES) && Empty the group of the custom special charachter as it was
IF ASCAN(loFormSet.laEvntTrig,PADR('NORALEFG',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
  loFormSet.mDoTrigger(PADR('NORALEFG',10))
ENDIF
*-* C201125 Hesham Elmasry (HES)

IF !llCanAlo AND loFormSet.llUseDyes
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *REPLACE ALL cReason WITH IIF(!EMPTY(cReason),cReason ,LANG_AutoAlloc_LabelReasMAsetup)
  REPLACE ALL cReason WITH IIF(!EMPTY(cReason),cReason ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LabelReasMAsetup,loFormSet.GetHeaderText("LANG_AutoAlloc_LabelReasMAsetup",loFormSet.HeaderAlias)))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.llReject = .T.
ENDIF

*-- end of lfNormAlo.

*!*************************************************************
*! Name      : lfNormProc
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Normal allocation processes.
*!*************************************************************
*! Called from : lfNormAlo
*!*************************************************************
*! Calls       : lfTmpAlo,lfAloQty
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfNormProc()
*!*************************************************************
FUNCTION lfNormProc
PARAMETERS loFormSet

*IF there piktkt or totqty > totpik (Cancel allocation)
*note that in dyelot version there is an error (does not update pikline file)
*-- if this record is allocated before, but not picked.
IF picked AND !('******' $ PIKTKT)
  =lfMarkLine(loFormSet)
  loFormSet.llMustLoop = .T.
  *! B610291,1 HIA 04/04/2013 Aria XP - Aria4xp - Master Data - Budgets, T20130111.0006 [Start]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loFormSet.oPross.lblSecondLabel.Caption = LANG_AutoAlloc_ProgrsOrdNum + Order
  loFormSet.oPross.lblSecondLabel.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_ProgrsOrdNum,loFormSet.GetHeaderText("LANG_AutoAlloc_ProgrsOrdNum",loFormSet.HeaderAlias)) + ORDER
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.oPross.CurrentProgress(lnCurent)

  *! B610291,1 HIA 04/04/2013 Aria XP - Aria4xp - Master Data - Budgets, T20130111.0006 [End]
  RETURN
ENDIF    && End of IF

*-* C201125 Hesham Elmasry (HES) Just to enter the next Condition
IF ASCAN(loFormSet.laEvntTrig,PADR('GROPALLO',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0 AND gfGetMemVar('M_MULALLOC')
  IF GROUP = loFormSet.lcCustSpCh
    IF loFormSet.lnRpPikCor = 0
      loFormSet.lnRpPikCor = 1
    ELSE
      loFormSet.lnRpPikCor = loFormSet.lnOldCor
    ENDIF
  ENDIF
ENDIF
*-* C201125 Hesham Elmasry (HES)

*IF Force allocation is No [in the Option grid] and Pick coordinate
*Min. % is greater than 0 and the Order line group is not empty
IF !loFormSet.llRpForAlo .AND. loFormSet.lnRpPikCor > 0 .AND. !EMPTY(GROUP)

  lnRecNumb = RECNO()          && Variable to save the record number
  lcGroupStr =  ORDER + STORE + GROUP      && Variable to hold the Order number + Store + Group
  llAloGoup = .T.                     && Flag to know if we are going to allocate this group or not
  *IF Not an incompleted session and not the begin of the temp. Order
  *lines file and NPROCNO = 0
  IF !BOF() .AND. nProcNo = 0
    SKIP -1
    llGrpFrRec = IIF(ORDER + STORE + GROUP = lcGroupStr , .F. , .T.)    && Flag to know if this is the first line in the group

    *IF Not the begin of the temp. Order lines file
    IF !BOF()
      SKIP 1
    ELSE    && Else
      LOCATE
    ENDIF    && End of IF
  ELSE    && Else
    llGrpFrRec = IIF(nProcNo = 0 , .T. , .F.)    && Flag to know if this is the first line in the group
  ENDIF    && End of IF
  lcCurStyWr = ''

  *-* C201125 Hesham Elmasry (HES) Initializing the appropriate formset's properties and the calculation flag
  IF ASCAN(loFormSet.laEvntTrig,PADR('NORPRDCV',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
    loFormSet.mDoTrigger(PADR('NORPRDCV',10))
  ENDIF
  *-* C201125 Hesham Elmasry (HES)

  *SCAN Loop to scan the temp. Order lines file IF this was the first
  *line in the group FOR the same Order and Store and Group
  SCAN REST WHILE llGrpFrRec .AND. ORDER + STORE + GROUP = lcGroupStr

    * HES C201125 START
    IF ASCAN(loFormSet.laEvntTrig,PADR('GROPALLO',10),1,ALEN(loFormSet.laEvntTrig,1),1) = 0 OR !gfGetMemVar('M_MULALLOC')
      * HES C201125 FINISH

      lnCurent = lnCurent + 1
      *IF We are going to Allocate this group
      IF llAloGoup
        lcPikWare = IIF(EMPTY(loFormSet.lcRpPkFWrh) , cWareCode , loFormSet.lcRpPkFWrh)    && Variable to hold the Warehouse to allocate from
        loFormSet.llCalWip = (lcCurStyWr = STYLE + cWareCode)
        llAloGoup = lfTmpAlo(.T. , lcPikWare , .F. , loFormSet.lnRpCutUnt , loFormSet.lnRpPikCor,loFormSet)
        lcCurStyWr = STYLE + cWareCode
      ENDIF    && End of IF

      * HES C201125 START
    ELSE
      * HES C201125 FINISH

      lcPikWare = IIF(EMPTY(loFormSet.lcRpPkFWrh) , cWareCode , loFormSet.lcRpPkFWrh)    && Variable to hold the Warehouse to allocate from
      = lfTmpAlo(.T. , lcPikWare , .F. , loFormSet.lnRpCutUnt , loFormSet.lnRpPikCor,loFormSet)

      * HES C201125 START
    ENDIF
    * HES C201125 FINISH

  ENDSCAN    && End of SCAN Loop

  *-* C201125 Hesham Elmasry (HES) Valid the Total (AVL & REQ) and check if it fullfilled with Min%
  IF ASCAN(loFormSet.laEvntTrig,PADR('NORPRVMIN',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
    IF !loFormSet.mDoTrigger(PADR('NORPRVMIN',10))
      *:   B610579,1 MMT 11/10/2013 Automtaic allocation hanges when first sort by Start Date[Start]
      *B610697,1 TMI 03/12/2014 18:36 [Start] just comment this line as per the review
      *GO lnRecNumb
      *B610697,1 TMI 03/12/2014 18:36 [End  ] 
      *:   B610579,1 MMT 11/10/2013 Automtaic allocation hanges when first sort by Start Date[End]
      RETURN
    ENDIF
  ENDIF
  *-* C201125 Hesham Elmasry (HES)

  *IF We are going to Allocate this group
  IF llAloGoup
    lnCurent = IIF(llGrpFrRec , lnCurent , lnCurent + 1)
    GO lnRecNumb
    *SCAN Loop to scan the temp. Order lines file FOR the same Order
    *and Store and Group
    SCAN REST WHILE ORDER + STORE + GROUP = lcGroupStr
      lcPikWare = IIF(EMPTY(loFormSet.lcRpPkFWrh) , cWareCode , loFormSet.lcRpPkFWrh)    && Variable to hold the Warehouse to allocate from
      =lfAloQty(lcPikWare,.F.,loFormSet)
    ENDSCAN    && End of SCAN Loop
  ENDIF    && End of IF
  SKIP -1
ELSE    && Else (Force allocation or pick coordinate group min% = 0 or !empty(group))
  lcPikWare = IIF(EMPTY(loFormSet.lcRpPkFWrh) , cWareCode , loFormSet.lcRpPkFWrh)    && Variable to hold the Warehouse to allocate from
  lnCurent = lnCurent + 1
  IF loFormSet.llRpForAlo
    loFormSet.llCalWip = .F.
    llAloSep = lfTmpAlo(.F. , lcPikWare , .T.,.F.,.F.,loFormSet)  && Flag to know if we are going to allocate this record
    *IF We are going to allocate this record
    IF llAloSep
      =lfAloQty(lcPikWare,.F.,loFormSet)
    ENDIF    && End of IF
  ELSE    && Else (coordinate group min% > 0 or empty(group))
    llAloSep = lfTmpAlo(.T. , lcPikWare , .F. , loFormSet.lnRpCutUnt ,;
      IIF(EMPTY(GROUP) , loFormSet.lnRpPikSep , loFormSet.lnRpPikCor),loFormSet)      && Flag to know if we are going to allocate this record
    *IF We are going to force the allocation
    IF llAloSep
      =lfAloQty(lcPikWare,.F.,loFormSet)
    ENDIF    && End of IF
  ENDIF    && End of IF
ENDIF    && End of IF
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.oPross.lblSecondLabel.Caption = LANG_AutoAlloc_ProgrsOrdNum + Order
loFormSet.oPross.lblSecondLabel.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_ProgrsOrdNum,loFormSet.GetHeaderText("LANG_AutoAlloc_ProgrsOrdNum",loFormSet.HeaderAlias)) + ORDER
*N000682,1 11/20/2012 MMT Globlization changes[End]

loFormSet.oPross.CurrentProgress(lnCurent)


*-- end of lfNormProc.

*!*************************************************************
*! Name      : lfMarkLine
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Mark current record because it's allocated before
*!           : then increment allocation variables.
*!*************************************************************
*! Called from : lfDyeAlo
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfMarkLine()
*!*************************************************************
FUNCTION lfMarkLine
PARAMETERS loFormSet

REPLACE lnSel WITH 1
lnCurent = lnCurent + 1
*-- end of lfMarkLine.

*!*************************************************************
*! Name      :
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Function to Allocate the availabel quantity in the
*!             temp. Order lines file only.
*!*************************************************************
*! Called from : lfAllocate()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) [.T.] to Check if we will be able to Allocate
*!                        first and then Allocate ,
*!                        [.F.] to Allocate without checking
*!                     2) The Warehouse to Allocate from
*!                     3) [.T.] If we are going to force the allocation
*!                        [.F.] Otherwise
*!                     4) Cut of units
*!                     5) Min. % to Allocate
*!*************************************************************
*! Return      : [.T.] If it was able to Allocate
*!               [.F.] Otherwise
*!*************************************************************
*
FUNCTION lfTmpAlo
PARAMETERS llParm , lcWareCode , llForce , lnCutUnt , lnMinPer , loFormSet


PRIVATE llReturn , lnAvlFAlo , lnI , laOnHand , laWanted , laAllocPer
lnI = 0
DIMENSION laOnHand[8] , laWanted[8] , laAllocPer[8]
STORE 0 TO laOnHand , laWanted
laAllocPer = 1


llForce = IIF(TYPE('llForce') <> 'L' , .F. ,  llForce)        && Flag to know if we are going to force the Allocation
lnCutUnt = IIF(TYPE('lnCutUnt') <> 'N' , 0 , lnCutUnt)        && Variable to hold the Cut of units
lnMinPer = IIF(TYPE('lnMinPer') <> 'N' , 0 , lnMinPer)        && Variable to hold the Min. % to Allocate
llReturn = .T.            && Variable to hold the returned value
lnAvlFAlo = 0             && Variable to hold the availabel for Allocation quantity
lnTotReq  = 0             && Variable to hold the Required  for Allocation quantity

*IF There is a record for this Style and Warehouse and Dyelot in the STYDYE
*file
IF SEEK(STYLE + lcWareCode + DyeLot , 'STYDYE')
  *IF We are to Check if we will be able to Allocate first and we are not
  *going to force the Allocation
  IF loFormSet.lcRpIncWip $ 'AS'  AND !loFormSet.llCalWip
    =IIF(loFormSet.lcRpIncWip = 'A',lfEvalWip(STYLE,lcWareCode,loFormSet),lfEvalShp(STYLE,lcWareCode,loFormSet))
  ELSE
    IF loFormSet.lcRpIncWip = 'N'
      loFormSet.laWIP = 0
    ENDIF
  ENDIF

  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    laOnHand[lnI] = ROUND((STYDYE.Stk&lcI+loFormSet.laWIP[lnI]-STYDYE.Alo&lcI-lnCutUnt) *;
      laAllocPer[lnI],0)
    laWanted[lnI] = MAX(IIF(loFormSet.lcRpScpMod $ 'KP',Cut&lcI,QTY&lcI)-PIK&lcI-ExcCut&lcI,0)
    lnAvlFAlo = lnAvlFAlo + MAX(MIN(laOnHand[lnI],laWanted[lnI]),0)
  ENDFOR

  *-* C201125 Hesham Elmasry (HES) Calculate the accumulative AVl & REQ for a specific group
  IF ASCAN(loFormSet.laEvntTrig,PADR('TMPALCALC',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
    IF !loFormSet.mDoTrigger(PADR('TMPALCALC',10))
      RETURN
    ENDIF
  ENDIF
  *-* C201125 Hesham Elmasry (HES)

  IF llParm .AND. !llForce
    lnTotReq = IIF(loFormSet.lcRpScpMod $ 'KP',TOTCUT,TotQty) - totpik - TotExcCut
    llReturn = lnAvlFAlo <> 0 .AND. (lnAvlFAlo/lnTotReq >= lnMinPer/100)
    *E303530,1 MMT 11/30/2014 Add Triggers for [T20141118.0011][Start]
    IF ASCAN(loFormSet.laEvntTrig,PADR('ALOCPART',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
      llReturnValue = llReturn 
      IF loFormSet.mDoTrigger(PADR('ALOCPART',10))
        llReturn = .T.
      ENDIF 
    ENDIF
    *E303530,1 MMT 11/30/2014 Add Triggers for [T20141118.0011][End]
    IF !llReturn AND !picked
      DO CASE
      CASE lnAvlFAlo = 0
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *REPLACE cReason WITH IIF(!EMPTY(cReason),cReason ,LANG_AutoAlloc_LabelReasAvlQty)
        REPLACE cReason WITH IIF(!EMPTY(cReason),cReason ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LabelReasAvlQty,loFormSet.GetHeaderText("LANG_AutoAlloc_LabelReasAvlQty",loFormSet.HeaderAlias)))
        *N000682,1 11/20/2012 MMT Globlization changes[End]

        loFormSet.llReject = .T.
      CASE (lnAvlFAlo/lnTotReq < lnMinPer/100)
        IF EMPTY(GROUP)
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
          *REPLACE cReason WITH IIF(!EMPTY(cReason),cReason ,LANG_AutoAlloc_LabelResaAvlQtCov1 )
          REPLACE cReason WITH IIF(!EMPTY(cReason),cReason ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LabelResaAvlQtCov1,loFormSet.GetHeaderText("LANG_AutoAlloc_LabelResaAvlQtCov1",loFormSet.HeaderAlias)) )
          *N000682,1 11/20/2012 MMT Globlization changes[End]

          loFormSet.llReject = .T.
        ELSE
          lcGroup = GROUP
          lnRecNo = RECNO()
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
          *REPLACE cReason WITH IIF(!EMPTY(cReason),cReason ,LANG_AutoAlloc_LabelResaAvlQtCov2) FOR GROUP = lcGroup
          REPLACE cReason WITH IIF(!EMPTY(cReason),cReason ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LabelResaAvlQtCov2,loFormSet.GetHeaderText("LANG_AutoAlloc_LabelResaAvlQtCov2",loFormSet.HeaderAlias))) FOR GROUP = lcGroup
          *N000682,1 11/20/2012 MMT Globlization changes[End]

          loFormSet.llReject = .T.
          IF BETWEEN(lnRecNo,1,RECCOUNT())
            GOTO lnRecNo
          ENDIF
        ENDIF
      ENDCASE
    ENDIF
  ENDIF    && End of IF


  REPLACE Avl1    WITH laOnHand[1] ,;
    Avl2    WITH laOnHand[2] ,;
    Avl3    WITH laOnHand[3] ,;
    Avl4    WITH laOnHand[4] ,;
    Avl5    WITH laOnHand[5] ,;
    Avl6    WITH laOnHand[6] ,;
    Avl7    WITH laOnHand[7] ,;
    Avl8    WITH laOnHand[8] ,;
    TotAvl  WITH Avl1 + Avl2 + Avl3 + Avl4 + Avl5 + Avl6 + Avl7 + Avl8

  *-* C201125 Hesham Elmasry (HES) Approve the Calculation
  IF ASCAN(loFormSet.laEvntTrig,PADR('TMPALRETR',10),1,ALEN(loFormSet.laEvntTrig,1),1) > 0
    loFormSet.mDoTrigger(PADR('TMPALRETR',10))
  ENDIF
  *-* C201125 Hesham Elmasry (HES)

  *IF We will be able to Allocate
  IF llReturn

    *IF We are not going to force the Allocation
    IF !llForce
      REPLACE PoAlo1    WITH MAX(MIN(laOnHand[1],laWanted[1]),0) ,;
        PoAlo2    WITH MAX(MIN(laOnHand[2],laWanted[2]),0) ,;
        PoAlo3    WITH MAX(MIN(laOnHand[3],laWanted[3]),0) ,;
        PoAlo4    WITH MAX(MIN(laOnHand[4],laWanted[4]),0) ,;
        PoAlo5    WITH MAX(MIN(laOnHand[5],laWanted[5]),0) ,;
        PoAlo6    WITH MAX(MIN(laOnHand[6],laWanted[6]),0) ,;
        PoAlo7    WITH MAX(MIN(laOnHand[7],laWanted[7]),0) ,;
        PoAlo8    WITH MAX(MIN(laOnHand[8],laWanted[8]),0) ,;
        Tot_PoAlo WITH PoAlo1 + PoAlo2 + PoAlo3 + PoAlo4 + PoAlo5 + PoAlo6 + PoAlo7 + PoAlo8

    ELSE    && Else
      REPLACE PoAlo1    WITH MAX(IIF(loFormSet.lcRpScpMod $ 'KP',Cut1,QTY1) - pik1 - ExcCut1,0) ,;
        PoAlo2    WITH MAX(IIF(loFormSet.lcRpScpMod $ 'KP',Cut2,Qty2) - pik2 - ExcCut2,0) ,;
        PoAlo3    WITH MAX(IIF(loFormSet.lcRpScpMod $ 'KP',Cut3,Qty3) - pik3 - ExcCut3,0) ,;
        PoAlo4    WITH MAX(IIF(loFormSet.lcRpScpMod $ 'KP',Cut4,Qty4) - pik4 - ExcCut4,0) ,;
        PoAlo5    WITH MAX(IIF(loFormSet.lcRpScpMod $ 'KP',Cut5,Qty5) - pik5 - ExcCut5,0) ,;
        PoAlo6    WITH MAX(IIF(loFormSet.lcRpScpMod $ 'KP',Cut6,Qty6) - pik6 - ExcCut6,0) ,;
        PoAlo7    WITH MAX(IIF(loFormSet.lcRpScpMod $ 'KP',Cut7,Qty7) - pik7 - ExcCut7,0) ,;
        PoAlo8    WITH MAX(IIF(loFormSet.lcRpScpMod $ 'KP',Cut8,Qty8) - pik8 - ExcCut8,0) ,;
        Tot_PoAlo WITH PoAlo1 + PoAlo2 + PoAlo3 + PoAlo4 + PoAlo5 + PoAlo6 + PoAlo7 + PoAlo8
    ENDIF    && End of IF

    =RLOCK()
    UNLOCK
  ENDIF    && End of IF
  RETURN llReturn
ELSE    && Else
  IF !SEEK(STYLE + lcWareCode, 'STYDYE')
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *REPLACE cReason WITH IIF(!EMPTY(cReason),cReason ,LANG_AutoAlloc_LabelResaStyleNtAssgn+lcWareCode)
    REPLACE cReason WITH IIF(!EMPTY(cReason),cReason ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LabelResaStyleNtAssgn,loFormSet.GetHeaderText("LANG_AutoAlloc_LabelResaStyleNtAssgn",loFormSet.HeaderAlias))+lcWareCode)
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    loFormSet.llReject = .T.
  ELSE
    IF !EMPTY(DyeLot)
      REPLACE cReason WITH IIF(!EMPTY(cReason),cReason ,;
        IIF(loFormSet.llUseConfg,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LabelReasCnfgNtAssgn1,loFormSet.GetHeaderText("LANG_AutoAlloc_LabelReasCnfgNtAssgn1",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LabelReasDyeNtAssgn1,loFormSet.GetHeaderText("LANG_AutoAlloc_LabelReasDyeNtAssgn1",loFormSet.HeaderAlias))) + ;
        ALLTRIM(DyeLot) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LabelReasDyeNtAssgn2,loFormSet.GetHeaderText("LANG_AutoAlloc_LabelReasDyeNtAssgn2",loFormSet.HeaderAlias)) + ALLTRIM(lcWareCode))
      *N000682,1 11/20/2012 MMT Globlization changes[End]

      loFormSet.llReject = .T.
    ENDIF
  ENDIF
  RETURN .F.
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfEvalWip
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Evaluate wip to add it to availablity if user want.
*!*************************************************************
*! Called from : lfCalAval
*!*************************************************************
*! Calls       : lfKPWip
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfEvalWip()
*!*************************************************************
FUNCTION lfEvalWip
PARAMETERS lcWipSty,lcWipWare,loFormSet
PRIVATE lcCurAlias,lcStyDyRec

lcCurAlias = SELECT(0)

loFormSet.laWIP = 0
*-- if select By C/T or P/O (Wips for selected only)
IF loFormSet.lcRpScpMod $ 'KP' AND !EMPTY(loFormSet.laIncExprs[1,6])
  = lfKPWip(lcWipSty,lcWipWare,'loFormSet.laIncExprs',1)
  loFormSet.laWIP[9] = loFormSet.laWIP[1]+loFormSet.laWIP[2]+loFormSet.laWIP[3]+loFormSet.laWIP[4]+;
    loFormSet.laWIP[5]+loFormSet.laWIP[6]+loFormSet.laWIP[7]+loFormSet.laWIP[8]

  IF loFormSet.laWIP[9] < 0
    loFormSet.laWIP = 0
    RETURN
  ENDIF
ELSE

  SELECT STYDYE
  lcStyDyRec = STYLE+cWareCode+DyeLot  && Save current stydye record.
  =SEEK(lcWipSty+lcWipWare+SPACE(10))  && Seek master location record.
  SCATTER FIELDS WIP1,WIP2,WIP3,WIP4,WIP5,WIP6,WIP7,WIP8,TOTWIP TO loFormSet.laWIP
  =SEEK(lcStyDyRec)                    && Restore current stydye record.
ENDIF

*-- if exclude some records(C/t or P/o)
IF (loFormSet.laWIP[9] > 0) AND loFormSet.llExclude AND (loFormSet.lcRpExSlct $ 'KP') AND;
    !EMPTY(loFormSet.laFiltExp[1,6])
  = lfKPWip(lcWipSty,lcWipWare,'loFormSet.laFiltExp',-1)
ENDIF

loFormSet.laWIP[9] = loFormSet.laWIP[1]+loFormSet.laWIP[2]+loFormSet.laWIP[3]+loFormSet.laWIP[4]+;
  loFormSet.laWIP[5]+loFormSet.laWIP[6]+loFormSet.laWIP[7]+loFormSet.laWIP[8]

SELECT (loFormSet.lcTmpOrdLn)
=RLOCK()
REPLACE TOTWIP WITH loFormSet.laWIP[9]
UNLOCK
SELECT (lcCurAlias)
*-- end of lfEvalWip.

*!*************************************************************
*! Name      : lfKPWip
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Evaluate wip C/t or P/o Cases.
*!*************************************************************
*! Called from : lfEvalWip
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfKPWip()
*!*************************************************************
*E300989,1
*
FUNCTION lfKPWip
PARAMETERS lcWipSty,lcWipWare,lcFiltArr,lnCalNo

PRIVATE lcLineFile,lcKeyExpr,lcKey,lnSelct
lnSelct = SELECT(0)
lcKeyExpr = 'CBUSDOCU+CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD'

lcJ = IIF(lnCalNo=1,'1','2')
FOR lnStrPos = 1 TO ALEN(loFormSet.laString&lcJ,1)
  lcKey = 'P'+IIF(loFormSet.lcRpScpMod='K','U','P')+loFormSet.laString&lcJ[lnStrPos]+lcWipSty
  lcPOSLCond = "CBUSDOCU = 'P' AND CSTYTYPE = "+ IIF(loFormSet.lcRpScpMod='K','U','P') +" AND PO = '" +;
    loFormSet.laString&lcJ[lnStrPos]  + "' STYLE = '" + lcWipSty +"'"
  llPOSLNOpn = .F.
  IF lfopensql(loFormSet,'POSLN','POSLN1',lcPOSLCond)
    SELECT POSLN1
    LOCATE
    llPOSLNOpn = !EOF()
  ENDIF
  IF llPOSLNOpn
    SCAN FOR (cWareCode = lcWipWare)
      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        loFormSet.laWIP[lnI] = loFormSet.laWIP[lnI] + IIF(Trancd='1',QTY&lcI,IIF(Trancd $ '245',-1*QTY&lcI,0)) * lnCalNo
      ENDFOR
    ENDSCAN
  ENDIF
ENDFOR
SELECT (lnSelct)
*-- end of lfKPWip.

*!*************************************************************
*! Name      : lfAloQty
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Function to update the needed files to Allocate
*!             the current record of the temp. Order lines file
*!*************************************************************
*! Called from : lfAllocate()
*!*************************************************************
*! Calls       : gfAdd_Info()
*!*************************************************************
*! Passed Parameters : The Warehouse to Allocate from
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfAloQty
PARAMETERS lcWareCode,llHaveDye,loFormSet
PRIVATE lnProcVal

SELECT (loFormSet.lcTmpOrdLn)

IF loFormSet.llChkAprov
  lcExced = lfChkAprov(loFormSet)
  IF  lcExced  = 'F' OR lcExced = 'E' && If Exceed befor or in this Line
    IF lcExced = 'E'  && If Exceed in this Line get the message
      *Message 44108 : The approved amount has covered some lines only in order XXXXX.
      *                Cannot allocate the rest.
      =gfModalGen("TRM44108B00000" , "DIALOG",EVALUATE(loFormSet.lcTmpOrdLn+'.Order'))
    ENDIF
    *-- Rest The Alocated Qty of this line
    REPLACE PoAlo1  WITH 0,;
      PoAlo2  WITH 0,;
      PoAlo3  WITH 0,;
      PoAlo4  WITH 0,;
      PoAlo5  WITH 0,;
      PoAlo6  WITH 0,;
      PoAlo7  WITH 0,;
      PoAlo8  WITH 0,;
      Tot_PoAlo WITH 0 IN (loFormSet.lcTmpOrdLn)
    RETURN
  ENDIF
ENDIF

*IF nProcNo [The step number (for the rollback)] equal 0
IF nProcNo = 0

  *IF There is a record for this Style and Warehouse in the STYDYE file
  IF SEEK(STYLE + lcWareCode + SPACE(10) , 'STYDYE')
    SELECT STYDYE
    = RLOCK()
    REPLACE Alo1      WITH Alo1   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo1') ,;
      Alo2      WITH Alo2   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo2') ,;
      Alo3      WITH Alo3   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo3') ,;
      Alo4      WITH Alo4   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo4') ,;
      Alo5      WITH Alo5   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo5') ,;
      Alo6      WITH Alo6   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo6') ,;
      Alo7      WITH Alo7   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo7') ,;
      Alo8      WITH Alo8   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo8') ,;
      TotAlo    WITH TotAlo + EVALUATE(loFormSet.lcTmpOrdLn+'.Tot_PoAlo')

    =gfAdd_Info('STYDYE',loFormSet)
    UNLOCK

    *:   B608600,1 MMT 07/01/2008 Move Ordered Qty to the warehouse when user change it[Start]
    IF EVALUATE(loFormSet.lcTmpOrdLn+'.cWareCode') <> lcWareCode
      REPLACE Ord1      WITH Ord1   + EVALUATE(loFormSet.lcTmpOrdLn+'.QTY1') ,;
        Ord2      WITH Ord2   + EVALUATE(loFormSet.lcTmpOrdLn+'.QTY2') ,;
        Ord3      WITH Ord3   + EVALUATE(loFormSet.lcTmpOrdLn+'.QTY3') ,;
        Ord4      WITH Ord4   + EVALUATE(loFormSet.lcTmpOrdLn+'.QTY4') ,;
        Ord5      WITH Ord5   + EVALUATE(loFormSet.lcTmpOrdLn+'.QTY5') ,;
        Ord6      WITH Ord6   + EVALUATE(loFormSet.lcTmpOrdLn+'.QTY6') ,;
        Ord7      WITH Ord7   + EVALUATE(loFormSet.lcTmpOrdLn+'.QTY7') ,;
        Ord8      WITH Ord8   + EVALUATE(loFormSet.lcTmpOrdLn+'.QTY8') ,;
        TotOrd    WITH TotOrd + EVALUATE(loFormSet.lcTmpOrdLn+'.TotQTY')

      IF SEEK(STYLE + EVALUATE(loFormSet.lcTmpOrdLn+'.cWareCode') + SPACE(10) , 'STYDYE')
        REPLACE Ord1      WITH MAX(Ord1 - EVALUATE(loFormSet.lcTmpOrdLn+'.QTY1'),0) ,;
          Ord2      WITH MAX(Ord2   - EVALUATE(loFormSet.lcTmpOrdLn+'.QTY2'),0) ,;
          Ord3      WITH MAX(Ord3   - EVALUATE(loFormSet.lcTmpOrdLn+'.QTY3'),0) ,;
          Ord4      WITH MAX(Ord4   - EVALUATE(loFormSet.lcTmpOrdLn+'.QTY4'),0) ,;
          Ord5      WITH MAX(Ord5   - EVALUATE(loFormSet.lcTmpOrdLn+'.QTY5'),0) ,;
          Ord6      WITH MAX(Ord6   - EVALUATE(loFormSet.lcTmpOrdLn+'.QTY6'),0) ,;
          Ord7      WITH MAX(Ord7   - EVALUATE(loFormSet.lcTmpOrdLn+'.QTY7'),0) ,;
          Ord8      WITH MAX(Ord8   - EVALUATE(loFormSet.lcTmpOrdLn+'.QTY8'),0) ,;
          TotOrd    WITH MAX(TotOrd - EVALUATE(loFormSet.lcTmpOrdLn+'.TotQTY'),0)
      ENDIF
    ENDIF
    *:   B608600,1 MMT 07/01/2008 Move Ordered Qty to the warehouse when user change it[End]



  ENDIF    && End of IF
  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 1
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

*IF nProcNo [The step number (for the rollback)] equal 1
IF nProcNo = 1

  *IF There is a record for this Style and Warehouse and Dyelot in the
  * STYDYE file
  IF !EMPTY(DyeLot) .AND. SEEK(STYLE + lcWareCode + DyeLot , 'STYDYE')

    SELECT STYDYE
    = RLOCK()
    REPLACE Alo1   WITH Alo1   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo1') ,;
      Alo2   WITH Alo2   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo2') ,;
      Alo3   WITH Alo3   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo3') ,;
      Alo4   WITH Alo4   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo4') ,;
      Alo5   WITH Alo5   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo5') ,;
      Alo6   WITH Alo6   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo6') ,;
      Alo7   WITH Alo7   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo7') ,;
      Alo8   WITH Alo8   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo8') ,;
      TotAlo WITH TotAlo + EVALUATE(loFormSet.lcTmpOrdLn+'.Tot_PoAlo')

    IF !EVALUATE(loFormSet.lcTmpOrdLn+'.Picked')
      REPLACE Ord1 WITH Ord1   + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty1')  ,;
        Ord2 WITH Ord2   + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty2')  ,;
        Ord3 WITH Ord3   + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty3')  ,;
        Ord4 WITH Ord4   + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty4')  ,;
        Ord5 WITH Ord5   + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty5')  ,;
        Ord6 WITH Ord6   + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty6')  ,;
        Ord7 WITH Ord7   + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty7')  ,;
        Ord8 WITH Ord8   + EVALUATE(loFormSet.lcTmpOrdLn+'.Qty8')  ,;
        TotOrd WITH TotOrd + EVALUATE(loFormSet.lcTmpOrdLn+'.TotQty')
    ENDIF

    =gfAdd_Info('STYDYE',loFormSet)
    UNLOCK

    *:   B608600,1 MMT 07/01/2008 Move Ordered Qty to the warehouse when user change it[START]
    IF EVALUATE(loFormSet.lcTmpOrdLn+'.cWareCode') <> lcWareCode
      IF SEEK(STYLE + EVALUATE(loFormSet.lcTmpOrdLn+'.cWareCode') + DyeLot  , 'STYDYE')
        REPLACE Ord1      WITH MAX(Ord1 - EVALUATE(loFormSet.lcTmpOrdLn+'.QTY1'),0) ,;
          Ord2      WITH MAX(Ord2   - EVALUATE(loFormSet.lcTmpOrdLn+'.QTY2'),0) ,;
          Ord3      WITH MAX(Ord3   - EVALUATE(loFormSet.lcTmpOrdLn+'.QTY3'),0) ,;
          Ord4      WITH MAX(Ord4   - EVALUATE(loFormSet.lcTmpOrdLn+'.QTY4'),0) ,;
          Ord5      WITH MAX(Ord5   - EVALUATE(loFormSet.lcTmpOrdLn+'.QTY5'),0) ,;
          Ord6      WITH MAX(Ord6   - EVALUATE(loFormSet.lcTmpOrdLn+'.QTY6'),0) ,;
          Ord7      WITH MAX(Ord7   - EVALUATE(loFormSet.lcTmpOrdLn+'.QTY7'),0) ,;
          Ord8      WITH MAX(Ord8   - EVALUATE(loFormSet.lcTmpOrdLn+'.QTY8'),0) ,;
          TotOrd    WITH MAX(TotOrd - EVALUATE(loFormSet.lcTmpOrdLn+'.TotQTY'),0)
      ENDIF
    ENDIF
    *:   B608600,1 MMT 07/01/2008 Move Ordered Qty to the warehouse when user change it[End]



  ENDIF    && End of IF
  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 2
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

*IF nProcNo [The step number (for the rollback)] equal 2
IF nProcNo = 2

  *IF There is a record for this Style in the STYLE file
  IF !EOF('STYLE')
    SELECT STYLE
    = RLOCK()
    REPLACE Alo1   WITH Alo1   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo1') ,;
      Alo2   WITH Alo2   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo2') ,;
      Alo3   WITH Alo3   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo3') ,;
      Alo4   WITH Alo4   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo4') ,;
      Alo5   WITH Alo5   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo5') ,;
      Alo6   WITH Alo6   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo6') ,;
      Alo7   WITH Alo7   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo7') ,;
      Alo8   WITH Alo8   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo8') ,;
      TotAlo WITH TotAlo + EVALUATE(loFormSet.lcTmpOrdLn+'.Tot_PoAlo')

    =gfAdd_Info('STYLE',loFormSet)
    UNLOCK
  ENDIF    && End of IF
  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 3
  =RLOCK()
  UNLOCK
ENDIF    && End of IF

*IF nProcNo [The step number (for the rollback)] equal 3
IF nProcNo = 3

  *IF There is a record for this Order line in the ORDLINE file
  IF SEEK('O' + ORDER + STR(LINENO , 6) , 'ORDLINE')
    SELECT ORDLINE
    = RLOCK()
    *B610945,1 MMT 02/08/2015 Automatic allocation doubles the picked Qty[T20150205.0025][Start]
*!*	    REPLACE pik1      WITH pik1   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo1') ,;
*!*	      pik2      WITH pik2   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo2') ,;
*!*	      pik3      WITH pik3   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo3') ,;
*!*	      pik4      WITH pik4   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo4') ,;
*!*	      pik5      WITH pik5   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo5') ,;
*!*	      pik6      WITH pik6   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo6') ,;
*!*	      pik7      WITH pik7   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo7') ,;
*!*	      pik8      WITH pik8   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo8') ,;
*!*	      totpik    WITH totpik + EVALUATE(loFormSet.lcTmpOrdLn+'.Tot_PoAlo') ,;
*!*	      pikdate   WITH oAriaApplication.SystemDate ,;
*!*	      PIKTKT    WITH IIF(EMPTY(PIKTKT),'******',PIKTKT) ,;
*!*	      picked    WITH .T. ,;
*!*	      cWareCode WITH lcWareCode,;
*!*	      DyeLot    WITH IIF(llHaveDye,EVALUATE(loFormSet.lcTmpOrdLn+'.Dyelot'),DyeLot)
    REPLACE pik1      WITH MIN(Qty1,pik1   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo1')) ,;
      pik2      WITH MIN(Qty2,pik2   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo2')) ,;
      pik3      WITH MIN(Qty3,pik3   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo3')) ,;
      pik4      WITH MIN(Qty4,pik4   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo4')) ,;
      pik5      WITH MIN(Qty5,pik5   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo5')) ,;
      pik6      WITH MIN(Qty6,pik6   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo6')) ,;
      pik7      WITH MIN(Qty7,pik7   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo7')) ,;
      pik8      WITH MIN(Qty8,pik8   + EVALUATE(loFormSet.lcTmpOrdLn+'.PoAlo8')) ,;
      totpik    WITH MIN(TotQty,totpik + EVALUATE(loFormSet.lcTmpOrdLn+'.Tot_PoAlo')) ,;
      pikdate   WITH oAriaApplication.SystemDate ,;
      PIKTKT    WITH IIF(EMPTY(PIKTKT),'******',PIKTKT) ,;
      picked    WITH .T. ,;
      cWareCode WITH lcWareCode,;
      DyeLot    WITH IIF(llHaveDye,EVALUATE(loFormSet.lcTmpOrdLn+'.Dyelot'),DyeLot)
    *B610945,1 MMT 02/08/2015 Automatic allocation doubles the picked Qty[T20150205.0025][End]
    *!* B610670,1 HIA 02/04/2014 T20140123.0021 - DCC - duplicate Pick Tickets with wrong or no qtys [Begin]
    REPLACE cAllocatBy      WITH  IIF( totpik > 0 , 'A' , ' ')
    *!* B610670,1 HIA 02/04/2014 T20140123.0021 - DCC - duplicate Pick Tickets with wrong or no qtys [End]

    =gfAdd_Info('ORDLINE',loFormSet)
    UNLOCK

  ENDIF    && End of IF
  SELECT (loFormSet.lcTmpOrdLn)
  REPLACE nProcNo   WITH 4
  =RLOCK()
  UNLOCK
ENDIF    && End of IF


lnProcVal = 4

*IF nProcNo [The step number (for the rollback)] equal lnProcVal
*IF nProcNo = 4
IF nProcNo = lnProcVal

  =SEEK('O' + ORDER + STR(LINENO , 6) , 'ORDLINE')
  *B610945,1 MMT 02/08/2015 Automatic allocation doubles the picked Qty[T20150205.0025][Start]
*!*	  REPLACE pik1      WITH pik1   + PoAlo1 ,;
*!*	    pik2      WITH pik2   + PoAlo2 ,;
*!*	    pik3      WITH pik3   + PoAlo3 ,;
*!*	    pik4      WITH pik4   + PoAlo4 ,;
*!*	    pik5      WITH pik5   + PoAlo5 ,;
*!*	    pik6      WITH pik6   + PoAlo6 ,;
*!*	    pik7      WITH pik7   + PoAlo7 ,;
*!*	    pik8      WITH pik8   + PoAlo8 ,;
*!*	    totpik    WITH totpik + Tot_PoAlo ,;
*!*	    pikdate   WITH IIF(EOF('ORDLINE') , oAriaApplication.SystemDate , ORDLINE.pikdate) ,;
*!*	    PIKTKT    WITH IIF(EMPTY(PIKTKT),'******',PIKTKT) ,;
*!*	    picked    WITH .T. ,;
*!*	    cWareCode WITH lcWareCode ,;
*!*	    nProcNo   WITH 99 ,;
*!*	    lnSel     WITH 1
  REPLACE pik1      WITH MIN(QTY1,pik1   + PoAlo1 ),;
    pik2      WITH MIN(QTY2,pik2   + PoAlo2) ,;
    pik3      WITH MIN(QTY3,pik3   + PoAlo3) ,;
    pik4      WITH MIN(QTY4,pik4   + PoAlo4) ,;
    pik5      WITH MIN(QTY5,pik5   + PoAlo5) ,;
    pik6      WITH MIN(QTY6,pik6   + PoAlo6) ,;
    pik7      WITH MIN(QTY7,pik7   + PoAlo7) ,;
    pik8      WITH MIN(QTY8,pik8   + PoAlo8) ,;
    totpik    WITH MIN(TOTQTY,totpik + Tot_PoAlo) ,;
    pikdate   WITH IIF(EOF('ORDLINE') , oAriaApplication.SystemDate , ORDLINE.pikdate) ,;
    PIKTKT    WITH IIF(EMPTY(PIKTKT),'******',PIKTKT) ,;
    picked    WITH .T. ,;
    cWareCode WITH lcWareCode ,;
    nProcNo   WITH 99 ,;
    lnSel     WITH 1
  *B610945,1 MMT 02/08/2015 Automatic allocation doubles the picked Qty[T20150205.0025][End]
  =RLOCK()
  UNLOCK


*B611762,1 ES 05/29/2019 - Style wrong ordered quantity in the Rebalance program - [Start]
IF SEEK('O' + ORDER  , 'ORDHDR', 'ORDHDR')
    SELECT ORDHDR
    = RLOCK()
    =gfAdd_Info('ORDHDR',loFormSet)
    UNLOCK
    SELECT (loFormSet.lcTmpOrdLn)
 ENDIF  
*B611762,1 ES 05/29/2019 - Style wrong ordered quantity in the Rebalance program - [End]

ENDIF    && End of IF
*-- end of lfAloQty.

*:**************************************************************************
*: Name      : lfChkAprov
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*: Purpose   : Check Order Approve Amount.
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*: Return      : ....
*:**************************************************************************
*: Example     : = lfChkAprov()
*:**************************************************************************
*:B804233,1
FUNCTION lfChkAprov
PARAMETERS loFormSet

IF !SEEK('O'+ORDER,loFormSet.lcTmpOrdAp)
  IF SEEK('O'+ORDER,'OrdHdr')
    INSERT INTO (loFormSet.lcTmpOrdAp) (TYPE,ORDER,AprAmnt,TotQty);
      VALUES ('O',ORDHDR.ORDER,ORDHDR.ApprAmt,EVALUATE(loFormSet.lcTmpOrdLn+'.Tot_PoAlo') * EVALUATE(loFormSet.lcTmpOrdLn+'.Price'))
  ENDIF
ELSE
  IF EVALUATE(loFormSet.lcTmpOrdAp+'.lExceed')
    RETURN 'F'  && It Exceeded Before
  ELSE
    *wael
    *REPLACE TotQty WITH loFormSet.lcTmpOrdAp+'.TotQty' +;
    (EVALUATE(loFormSet.lcTmpOrdLn+'.Tot_PoAlo') * EVALUATE(loFormSet.lcTmpOrdLn+'.Price')) IN (loFormSet.lcTmpOrdAp)
    REPLACE TotQty WITH EVALUATE(loFormSet.lcTmpOrdAp+'.TotQty') +;
      (EVALUATE(loFormSet.lcTmpOrdLn+'.Tot_PoAlo') * EVALUATE(loFormSet.lcTmpOrdLn+'.Price')) IN (loFormSet.lcTmpOrdAp)
    *wael
  ENDIF
ENDIF

IF EVALUATE(loFormSet.lcTmpOrdAp+'.TotQty') > EVALUATE(loFormSet.lcTmpOrdAp+'.AprAmnt')
  REPLACE lExceed WITH .T. IN (loFormSet.lcTmpOrdAp)
  RETURN 'E'  && It Ecxeeds Now
ELSE
  REPLACE lExceed WITH .F. IN (loFormSet.lcTmpOrdAp)
  RETURN 'N'  && Not Exceed
ENDIF


*!*************************************************************
*! Name      : lfDyeAlo
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : dyelot allocation.
*!*************************************************************
*! Called from : lfAllocate
*!*************************************************************
*! Calls       : lfNormProc , gfThermo
*!*************************************************************
*! Passed Parameters : Allocation case (All Records/Selected Records)
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfDyeAlo(1)
*!*************************************************************
FUNCTION lfDyeAlo
PARAMETERS lnFromOgMn,loFormSet

PRIVATE lnActvLine , laDye_Rel
lnActvLine = 0
DECLARE laDye_Rel[1]
laDye_Rel = ""
llSaveCond = loFormSet.llRpCond

*-- scan file for selected records.
SCAN FOR nProcNo < 5 AND IIF(lnFromOgMn = 2,lnSel <> 0,.T.)
  lcFabric = PADR(Fabric,loFormSet.lnFabMajor)+loFormSet.lcFabSep+PADR(cFabColor,loFormSet.lnFabNonMaj)
  IF EMPTY(cFabColor) OR !SEEK('0002'+lcFabric,'DYE_REL1') OR loFormSet.llUseConfg
    =(STYLE.cDye_Flg = "N" OR !EMPTY(DyeLot)) AND lfNormProc(loFormSet)
    IF STYLE.cDye_Flg = "Y" AND EMPTY(DyeLot)
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *REPLACE cReason WITH IIF(!EMPTY(cReason),cReason ,IIF(loFormSet.llUseConfg,LANG_AutoAlloc_LabelReasConfigAssign,LANG_AutoAlloc_LabelReasDyelotAssign))
      REPLACE cReason WITH IIF(!EMPTY(cReason),cReason ,IIF(loFormSet.llUseConfg,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LabelReasConfigAssign,loFormSet.GetHeaderText("LANG_AutoAlloc_LabelReasConfigAssign",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LabelReasDyelotAssign,loFormSet.GetHeaderText("LANG_AutoAlloc_LabelReasDyelotAssign",loFormSet.HeaderAlias))))
      *N000682,1 11/20/2012 MMT Globlization changes[End]

      loFormSet.llReject = .T.
    ENDIF
    loFormSet.llMustLoop = .F.
    LOOP
  ENDIF

  loFormSet.llRpCond = !EMPTY(GROUP) AND llSaveCond
  *-- if we want to allocate separate line conditionally only.
  IF EMPTY(GROUP) AND (loFormSet.lnRpPikSep > 0) AND loFormSet.llRpCond
    PRIVATE lnActivRec
    lnActivRec = RECNO()
    SET FILTER TO RECNO() = lnActivRec
  ENDIF

  lcGrpExpr   = loFormSet.laIndexExp[loFormSet.lnRpSort1] + ' + ' +loFormSet.laIndexExp[loFormSet.lnRpSort2] + ' + ' +;
    loFormSet.laIndexExp[loFormSet.lnRpSort3] + IIF(INLIST(4 , loFormSet.lnRpSort1 , loFormSet.lnRpSort2 ,;
    loFormSet.lnRpSort3) , '' , '+ ORDER') +;
    ' + STORE + FABRIC + CFABCOLOR + GROUP'
  lcCurGrpVal = EVALUATE(lcGrpExpr)

  *-- if it's the last scaned group and no allocation.
  IF lcOldGroup = lcCurGrpVal
    loFormSet.llMustLoop = .F.
    = lfNormProc(loFormSet)
    LOOP
  ENDIF
  lcAllWare  = ''   && Allocation location
  lcStartDye = ''   && Start dyelot
  lcPegged   = ''   && pegged dyelot
  lcOldSWare = ''   && old style/location for group lines.
  lcCurStyWr = ''   && old style/location for calculate wips.
  llEndProc  = .F.  && .T. if we end dyelot alloc. logic process.
  llAnOther  = .F.  && .T. if we must search for another dyelot.
  llNoDye    = .F.  && .T. if loop all dyelots and does have matching availability.
  llGrpStart = .T.  && .T. if we are at the start of group.
  llPitch    = .F.  && Pitch dyelot relation table is initially .F. for current group.

  *-- laOpnStock : Array hold available quantity.
  DIMENSION laOpnStock[9]
  STORE 0 TO laOpnStock

  *-- Process step value (descriped in the following lines)
  lnProcStep = 1

  *-- lnSkipTo : Skip up and down in dyelot relationship file(-1,+2,-3,+4)
  STORE 1 TO lnSkipTo
  lcLastSkip = 'F'  && last skip 'F' means Forward and 'B' backword.

  *-- loop until finish allocation process or exit.
  DO WHILE !llEndProc

    *-- loop Around current group.
    SCAN REST WHILE (&lcGrpExpr = lcCurGrpVal) ;
        FOR IIF(lnFromOgMn = 2,lnSel <> 0,.T.)
      =SEEK(STYLE+cWareCode+DyeLot,'STYDYE')
      lcAllWare = IIF(EMPTY(loFormSet.lcRpPkFWrh) , cWareCode , loFormSet.lcRpPkFWrh)    && Variable to hold the Warehouse to allocate from
      *-- save current stydye record.
      lcStyDyeRc = STYDYE.STYLE+STYDYE.cWareCode+STYDYE.DyeLot
      *-- if all process not finished and not find this dyelot in stydye file.
      IF !llEndProc AND !SEEK(STYLE+lcAllWare,'STYDYE')
        llEndProc = .T.  && end all process
        lnProcStep = 2   && step became 2.
      ENDIF  && end if all process not finished and not find this dyelot in stydye file.
      = SEEK(lcStyDyeRc,'STYDYE')  && Restore previous stydye record.
      DO CASE
        *-- First step check if there is pegged dyelot or start from
        *-- begin of dyelot relationship file.
      CASE lnProcStep = 1  && get pegged dyelot.
        *-- previous dyelot does not have sufficient quantity,[Begin
        *-- to allocate then get another one.
        IF llAnOther
          llAnOther  = .F.
          lcFabric = PADR(Fabric,loFormSet.lnFabMajor)+loFormSet.lcFabSep+PADR(cFabColor,loFormSet.lnFabNonMaj)
          llNoDye = !lfAnother(lcFabric)  && Find another closet dyelot.
          *-- if you still have dyelots.
          IF !llNoDye
            lcStartDye = DYE_REL1.DyeLot  && Select Current start dyelot
          ENDIF
          EXIT
        ELSE   && have pegged dyelot or start with new dyelot.
          lcStartDye    = IIF(EMPTY(lcStartDye),DyeLot,lcStartDye)
          lcPegged      = lcStartDye
          IF !EMPTY(lcStartDye)
            EXIT  && to start check for availability step.
          ENDIF
        ENDIF
        *-- previous dyelot does not have sufficient quantity,[End..

        *-- Second step check availability for current group, for
        *-- dyelot selected in step 1.
      CASE lnProcStep = 2  && check STYDYE availability for this group.

        IF !llEndProc
          *-- if you find this record in stydye calculate its OpnStock.
          *-- and if insufficient select another dyelot.
          IF !lfCalAval(SEEK(STYLE+lcAllWare+lcStartDye,'STYDYE'))
            llAnOther = .T.
            EXIT
          ENDIF
          lcOldSWare = STYLE+cWareCode
        ENDIF

        *-- Third step Allocate sufficient group.
      CASE lnProcStep = 3  && Allocate Current group.

        *-- if this line have new allocated
        IF Tot_PoAlo > 0
          = lfAlocMast(loFormSet)   && Allocate in master files.
        ELSE  && else if there is previously allocation and came from OG.
          *-- if allocated before.
          IF totpik <> 0 AND lnFromOgMn = 1
            = lfMarkLine(loFormSet)  && Mark record and increment allocation variables.
            LOOP          && Go to next record in current group.
          ENDIF    && End of IF
        ENDIF

        *-- Fourth step if and only if fail in allocation and user
        *-- want to allocate unconditional.
      CASE lnProcStep = 4  && Unconditional Allocate Current group.

        lcPegged = DyeLot
        SET ORDER TO TAG DYE_REL IN DYE_REL1
        *-- if you find this Fabric/Color pair in Dyelot Relation file.
        lcFabric = PADR(Fabric,loFormSet.lnFabMajor)+loFormSet.lcFabSep+PADR(cFabColor,loFormSet.lnFabNonMaj)

        IF SEEK('0002'+lcFabric+IIF(EMPTY(lcPegged),'',lcPegged),'DYE_REL1')
          SET ORDER TO TAG SEQUENCE IN DYE_REL1

          *-- llFindDye : .T. if you find your allocation dyelot.
          llFindDye = .F.

          *-- if there is no previously pegged dyelot.
          IF EMPTY(lcPegged)
            *-- if you find dyelot relation dyelot in stydye[Begin
            *-- exit loop else loop all dyelot relation records for
            *-- current Fabric/Color pair.
            DO WHILE .T.
              IF SEEK(STYLE+lcAllWare+DYE_REL1.DyeLot,'STYDYE')
                llFindDye = .T.
                EXIT
              ELSE
                SKIP 1 IN DYE_REL1
                lcFabric = PADR(Fabric,loFormSet.lnFabMajor)+loFormSet.lcFabSep+PADR(cFabColor,loFormSet.lnFabNonMaj)
                IF DYE_REL1.cItem # lcFabric
                  llFindDye = .F.
                  EXIT
                ENDIF
              ENDIF
            ENDDO
            *-- if you find dyelot relation dyelot in stydye[End..
          ELSE  && else there pegged dyelot  .
            *-- The following means if there pegged dyelot to this
            *-- line or any other start allocation with it when find it
            *-- in stydye file, else try with another.
            llFindDye = SEEK(STYLE+lcAllWare+lcPegged,'STYDYE')
          ENDIF  && end if there is no previously pegged dyelot.
          STORE .F. TO llNoDye,llPitch
          lcLastSkip = 'F'
          lnSkipTo   = 1

          *-- loop while you find dyelot.
          DO WHILE llFindDye

            IF lfCalAval(.T.,.T.)
              llFindDye = .F.  && to evaluate the next line.

              IF Tot_PoAlo > 0
                = lfAlocMast(loFormSet)   && Allocate to master files.
              ELSE
                IF totpik <> 0 AND lnFromOgMn = 1
                  = lfMarkLine(loFormSet)  && Mark record and increment alloc. variables.
                ENDIF    && End of IF
              ENDIF
            ELSE
              *-- loop to find another dyelots.
              DO WHILE .T.

                *-- if you did not collect this data before.
                IF (lnActvLine <> RECNO(loFormSet.lcTmpOrdLn))
                  lcFabric = PADR(Fabric,loFormSet.lnFabMajor)+loFormSet.lcFabSep+PADR(cFabColor,loFormSet.lnFabNonMaj)
                  =lfDyeRel(lcFabric)
                ENDIF
                llNoDye = EMPTY(laDye_Rel) OR (ALEN(laDye_Rel,1) = 1)

                *-- if previous dyelot is last one in this group.
                IF llNoDye
                  EXIT
                ELSE  && else you still have dyelots in dyelot relation file.
                  lcFabric = PADR(Fabric,loFormSet.lnFabMajor)+loFormSet.lcFabSep+PADR(cFabColor,loFormSet.lnFabNonMaj)
                  IF EMPTY(lfGetDyelt(lcFabric))
                    llNoDye = .T.
                    EXIT
                  ENDIF
                  *-- if you find this dyelot in stydye file.
                  IF SEEK(STYLE+lcAllWare+DYE_REL1.DyeLot,'STYDYE')
                    EXIT
                  ELSE  && this dyelot not in stydye file.
                    LOOP
                  ENDIF  && end if you find this dyelot in stydye file.
                ENDIF    && end if previous dyelot is last one in this group.
              ENDDO  && end loop to find another dyelots.
              llFindDye = !llNoDye
            ENDIF
          ENDDO

        ENDIF  && end if you find this Fabric/Color pair in Dyelot Relation file.

        IF lnSel = 0 AND totpik <> 0 AND lnFromOgMn = 1
          = lfMarkLine(loFormSet)  && Mark record and increment alloc. variables.
        ENDIF    && End of IF
      ENDCASE
    ENDSCAN  && end scan current group.

    *-- if step is to find dyelot avaialable for whole group.
    IF lnProcStep < 3
      = SEEK(lcCurGrpVal)  && Restore start of group.
    ELSE
      SKIP -1
    ENDIF  && end if step is to find dyelot avaialable for whole group.

    DO CASE
    CASE lnProcStep = 1

      *-- if no dyelots pegged to this group.
      *-- repeate previous steps with start of dyelot relation file.
      IF EMPTY(lcStartDye)
        *-- if Find matching dyelots for this Fabric/Color
        lcFabric = PADR(Fabric,loFormSet.lnFabMajor)+loFormSet.lcFabSep+PADR(cFabColor,loFormSet.lnFabNonMaj)
        IF SEEK('0002'+lcFabric,'DYE_REL1')
          lcStartDye = DYE_REL1.DyeLot
        ELSE  && No Matching dyelots for this Fabric/Color
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
          *REPLACE cReason WITH IIF(!EMPTY(cReason),cReason ,LANG_AutoAlloc_LabelReasNOFabDye+lcFabric)
          REPLACE cReason WITH IIF(!EMPTY(cReason),cReason ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LabelReasNOFabDye,loFormSet.GetHeaderText("LANG_AutoAlloc_LabelReasNOFabDye",loFormSet.HeaderAlias))+lcFabric)
          *N000682,1 11/20/2012 MMT Globlization changes[End]

          loFormSet.llReject = .T.
          llEndProc = .T.
          =SEEK(lcCurGrpVal)
          lcOldGroup = lcCurGrpVal
          loFormSet.llMustLoop = .F.
          = lfNormProc(loFormSet)
          LOOP
        ENDIF  && end if Find matching dyelots for this Fabric/Color.

      ELSE     && Find dyelot pegged to this group.

        IF llGrpStart AND !EMPTY(lcPegged)
          SET ORDER TO TAG DYE_REL IN DYE_REL1
          lcFabric = PADR(Fabric,loFormSet.lnFabMajor)+loFormSet.lcFabSep+PADR(cFabColor,loFormSet.lnFabNonMaj)
          IF SEEK('0002'+lcFabric+lcStartDye,'DYE_REL1')
            llGrpStart = .F.  && do not enter this code again.
          ELSE
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
            *REPLACE cReason WITH IIF(!EMPTY(cReason),cReason ,LANG_AutoAlloc_LabelReasSameFabDye)
            REPLACE cReason WITH IIF(!EMPTY(cReason),cReason ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_LabelReasSameFabDye,loFormSet.GetHeaderText("LANG_AutoAlloc_LabelReasSameFabDye",loFormSet.HeaderAlias)))
            *N000682,1 11/20/2012 MMT Globlization changes[End]

            loFormSet.llReject = .T.
            llEndProc = .T.  && process ended from first step.
            =SEEK(lcCurGrpVal)
            lcOldGroup = lcCurGrpVal
            loFormSet.llMustLoop = .F.
            = lfNormProc(loFormSet)
            LOOP  && end this group.
          ENDIF
          SET ORDER TO TAG SEQUENCE IN DYE_REL1
        ENDIF
      ENDIF    && end if no dyelots pegged to this group.

      IF (lnProcStep # 4) AND !llNoDye AND !SEEK(STYLE+lcAllWare+lcStartDye,'STYDYE')
        llAnOther = .T.
      ENDIF

    CASE lnProcStep = 2

      *-- if no dyelots have availability.
      IF llEndProc  && no stydye records for one or more Style(s).
        LOOP
      ENDIF

      *-- current group was allocated.
    CASE lnProcStep = 3 OR lnProcStep = 4
      llEndProc  = .T.
    ENDCASE

    *-- If None of Dye_Rel Dyelots was found in StyDye file OR no. availability.
    IF (lnProcStep # 4) AND llNoDye

      *-- if user allocate conditionally and not to force allocation.
      IF !loFormSe.llRpForAlo AND loFormSet.llRpCond

        *-- if we want to allocate separate line conditionally only.
        IF EMPTY(GROUP) AND (loFormSet.lnRpPikSep > 0)
          SET FILTER TO
          IF BETWEEN(lnActivRec,1,RECCOUNT())
            GO lnActivRec    && Record pointer is on tested record
            SKIP  && Skip Current Line
            *-- if it is new group
            IF &lcGrpExpr <> lcCurGrpVal
              llEndProc = .T.
            ENDIF
            GO lnActivRec    && Again Re-Point Active record
          ENDIF
        ELSE
          LOCATE REST FOR &lcGrpExpr > lcCurGrpVal  && Skip current group
          llEndProc = .T.
        ENDIF
      ELSE  && User want to force allocation or want to allocate unconditionally.
        lnProcStep = 4
      ENDIF
    ENDIF    && end If None of Dye_Rel Dyelots was found in StyDye file.

    lnProcStep = IIF(lnProcStep = 4,lnProcStep,IIF(llAnOther,1,lnProcStep+1))
  ENDDO  && end loop until finish allocation process or exit.

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loFormSet.oPross.lblSecondLabel.Caption = LANG_AutoAlloc_ProgrsOrdNum + Order
  loFormSet.oPross.lblSecondLabel.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AutoAlloc_ProgrsOrdNum,loFormSet.GetHeaderText("LANG_AutoAlloc_ProgrsOrdNum",loFormSet.HeaderAlias)) + ORDER
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.oPross.CurrentProgress(lnCurent)

  *-- if we want to allocate separate line conditionally only.
  IF EMPTY(GROUP) AND (loFormSet.lnRpPikSep > 0) AND loFormSet.llRpCond
    SET FILTER TO
  ENDIF

ENDSCAN
*-- end of lfDyeAlo.

*!*************************************************************
*! Name      : lfAnother
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Get another closest dyelot, to current dyelot.
*!*************************************************************
*! Called from : lfDyeAlo
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : Fabric/Color Pair
*!*************************************************************
*! Return      : .T. if you find another dyelot.
*!*************************************************************
*! Example     : =lfAnother()
*!*************************************************************
FUNCTION lfAnother
PARAMETERS lcFabClrPr

*-- if no pegged dyelot Start with Top of Fabric/Color group,
*-- and step is always 1 and forward.
IF llPitch OR EMPTY(lcPegged)
  IF EMPTY(lcPegged)
    lnPitch = 1
  ENDIF
  SKIP lnPitch IN DYE_REL1
  IF (lnPitch = 1 AND EOF('DYE_REL1')) OR BOF('DYE_REL1') OR ;
      (DYE_REL1.cItem # lcFabClrPr)  OR ;
      (DYE_REL1.DyeLot = lcStartDye)
    llPitch = .F.
    RETURN .F.
  ENDIF
ELSE  && else there is pegged dyelot then Start from within Fabric/Color group
  IF lcLastSkip = 'F'
    SKIP -lnSkipTo IN DYE_REL1
    lcLastSkip = 'B'
    IF BOF('DYE_REL1') OR ;
        (DYE_REL1.cItem # lcFabClrPr) OR ;
        (DYE_REL1.DyeLot = lcStartDye)

      SKIP lnSkipTo IN DYE_REL1
      IF EOF('DYE_REL1') OR ;
          (DYE_REL1.cItem # lcFabClrPr) OR ;
          (DYE_REL1.DyeLot = lcStartDye)
        RETURN .F.
      ELSE
        llPitch = .T.
        lnPitch = 1
      ENDIF
    ENDIF
  ELSE
    SKIP lnSkipTo IN DYE_REL1
    lcLastSkip = 'F'
    IF EOF('DYE_REL1') OR ;
        (DYE_REL1.cItem # lcFabClrPr) OR ;
        (DYE_REL1.DyeLot = lcStartDye)
      SKIP -(lnSkipTo+1) IN DYE_REL1
      IF BOF('DYE_REL1') OR ;
          (DYE_REL1.cItem # lcFabClrPr) OR ;
          (DYE_REL1.DyeLot = lcStartDye)
        RETURN .F.
      ELSE
        llPitch = .T.
        lnPitch = -1
      ENDIF
    ENDIF
  ENDIF
  lnSkipTo = lnSkipTo + 1
ENDIF
RETURN .T.

*!*************************************************************
*! Name      : lfGetDyelt
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Get another closest dyelot for the selected Fabric Color pair
*!*************************************************************
FUNCTION lfGetDyelt
PARAMETERS lcFabClrPr
PRIVATE lcFabClrPr , lnInDyeArry , lcRetDye

lcRetDye = ""
lnInDyeArry = ASCAN(laDye_Rel,DYE_REL1.DyeLot)
IF lnInDyeArry > 0
  lnInDyeArry = ASUBSCRIPT(laDye_Rel,lnInDyeArry,1)
  =ADEL(laDye_Rel,lnInDyeArry)
  DECLARE laDye_Rel[ALEN(laDye_Rel,1) - 1 , ALEN(laDye_Rel,2)]
  lnInDyeArry = ASCAN(laDye_Rel,DYE_REL1.DyeLot)
  IF lnInDyeArry > 0
    IF ALEN(laDye_Rel,1) = 1
      lcRetDye  = laDye_Rel[1,1]
      laDye_Rel = ""
    ELSE
      IF EMPTY(lcPegged) OR (lnInDyeArry = 1)
        lcRetDye  = laDye_Rel[1,1]
      ELSE
        IF lcLastSkip = 'F'
          lcLastSkip = 'B'
          lcRetDye  = laDye_Rel[lnInDyeArry - 1,1]
        ELSE
          lcLastSkip = 'F'
          lcRetDye  = laDye_Rel[lnInDyeArry,1]
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  PRIVATE lnActAlias , lcDyRelOrd
  lnActAlias = SELECT(0)
  SELECT DYE_REL1
  lcDyRelOrd = ORDER()
  SET ORDER TO DYE_REL
  =SEEK('0002'+lcFabClrPr + lcRetDye , "DYE_REL1")
  SET ORDER TO &lcDyRelOrd
  SELECT (lnActAlias)
ENDIF

RETURN lcRetDye
*-- end of lfGetDyelt.

*!*************************************************************
*! Name      : lfDyeRel
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Fill Dyelot array with all dyelots for the selected Fabric Color pair
*!*************************************************************
FUNCTION lfDyeRel
PARAMETERS lcFabClr
PRIVATE lnActAlias , lcDyRelOrd , lcFabClr , lcActivDye
lnActAlias = SELECT(0)
SELECT DYE_REL1
lcDyRelOrd = ORDER()
lcActivDye = Fabric+COLOR+DyeLot

SET ORDER TO

SELECT DyeLot, cDye_Seq;
  FROM DYE_REL1;
  WHERE CINVTYPE+cItem+DyeLot = '0002'+lcFabClr ;
  ORDER BY cDye_Seq INTO ARRAY laDye_Rel

SET ORDER TO DYE_REL
=SEEK(lcActivDye)

SET ORDER TO &lcDyRelOrd
SELECT (lnActAlias)
lnActvLine = RECNO(lcTmpOrdLn)
*-- end of lfDyeRel.

*!*************************************************************
*! Name      : lfAlocMast
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Allocate to master files.
*!*************************************************************
*! Called from : lfDyeAlo
*!*************************************************************
*! Calls       : lfRelQty , lfAloQty
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfAlocMast()
*!*************************************************************
FUNCTION lfAlocMast
PARAMETERS loFormSet
*-- if This line is allocated before to another dyelot
*-- release this allocation, because the same group must be allocated
*-- to the same dyelot.
IF !EMPTY(DyeLot) AND (STYDYE.DyeLot # DyeLot) AND totpik <> 0
  = lfRelQty(loFormSet)  && release allocation.
ENDIF

*-- replace current dyelot field with selected dyelot.
=RLOCK()
REPLACE DyeLot  WITH STYDYE.DyeLot,;
  nProcNo WITH 0
UNLOCK
=lfAloQty(lcAllWare,.T.,loFormSet)  && Allocate to the desired location.
*-- end of lfAlocMast.

*-----------------------Scope OG Functions --------------------
*!*************************************************************
*! Name      : lfwFilter
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Selection grid when function
*!*************************************************************
*! Called from : Selection OG
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfwFilter()
*!*************************************************************
FUNCTION lfwFilter
lnDummyPos = lfItmPos('llDummy')

=lfvIncHold()
=lfvExlBulk()

*-- end of lfwFilter.

*!*************************************************************
*! Name      : lfItmPos
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- end of lfItmPos.

*!*************************************************************
*! Name      : lfvIncHold
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Include Hold Orders.
*!*************************************************************
FUNCTION lfvIncHold
lcSOrdStat = IIF(llRpIncHOr,"OH","O")
*-- end of lfvIncHold.

*!*************************************************************
*! Name      : lfvExlBulk
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Exclude bulk order Orders.
*!*************************************************************
FUNCTION lfvExlBulk
lcBulkExp = IIF(llRpExlBlk,""," AND BULK='N'")
*-- end of lfvExlBulk.

*!*************************************************************
*! Name      : lfMakeArrs
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Fill Include/Exclude arrays.
*!*************************************************************
*! Called from : Selection OG
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfMakeArrs()
*!*************************************************************
FUNCTION lfMakeArrs

DIMENSION loFormSet.laIncExprs[1,ALEN(laOGFxFlt,2)],;
  loFormSet.laExcExprs[1,ALEN(laOGFxFlt,2)]
STORE '' TO loFormSet.laIncExprs,loFormSet.laExcExprs

lnJ = 1
FOR lnI = 1 TO ALEN(laOGFxFlt,1)
  lnJ = IIF(lnI = lnDummyPos,1,lnJ)
  IF !INLIST(lnI,lnDummyPos,lnDummyPos+1) AND !EMPTY(laOGFxFlt[lnI,6])
    IF (lnI < lnDummyPos) AND !EMPTY(loFormSet.laIncExprs[1,1])
      lnJ = lnJ + 1
      DIMENSION loFormSet.laIncExprs[lnJ,ALEN(laOGFxFlt,2)]
    ENDIF

    IF (lnI > lnDummyPos) AND !EMPTY(loFormSet.laExcExprs[1,1])
      lnJ = lnJ + 1
      DIMENSION loFormSet.laExcExprs[lnJ,ALEN(laOGFxFlt,2)]
    ENDIF

    lcSubArray = IIF(lnI < lnDummyPos,'loFormSet.laIncExprs','loFormSet.laExcExprs')
    FOR lnK = 1 TO ALEN(laOGFxFlt,2)
      &lcSubArray[lnJ,lnK] = laOGFxFlt[lnI,lnK]
    ENDFOR

  ENDIF
ENDFOR


*-- end of lfMakeArrs.

*!*************************************************************
*! Name      : lfFillVar
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Get the value of the variables returned from O.G.
*!*************************************************************
FUNCTION lfFillVar
PARAMETERS lnParam , loFormSet

DO CASE
  *-- Scope O.G.
CASE lnParam = 1
  loFormSet.llExclude   = llExclude
  loFormSet.llRpGdExcl  = llRpGdExcl
  loFormSet.llRpExlDye  = llRpExlDye
  loFormSet.llRpExlBlk  = llRpExlBlk
  loFormSet.llRpIncHOr  = llRpIncHOr
  loFormSet.lcRpExSlct  = lcRpExSlct
  loFormSet.lcRpSepCor  = lcRpSepCor
  loFormSet.lcRpScpMod  = lcRpScpMod
  loFormSet.lnDummyPos  = lnDummyPos
  loFormSet.llRpPikSep  = llRpPikSep
  loFormSet.llRpPikCor  = llRpPikCor
  loFormSet.lnColorLen  = lnColorLen
  loFormSet.lnNonMajSt  = lnNonMajSt
  loFormSet.lnRngAlias  = lnRngAlias
  loFormSet.lcSOrdStat  = lcSOrdStat
  *-- Allocate O.G.
CASE lnParam = 2
  loFormSet.lnRpPikSep  = lnRpPikSep
  loFormSet.lnRpPikCor  = lnRpPikCor
  loFormSet.lnRpCutUnt  = lnRpCutUnt
  loFormSet.lnRpSort1   = lnRpSort1
  loFormSet.lnRpSort2   = lnRpSort2
  loFormSet.lnRpSort3   = lnRpSort3
  loFormSet.lnRpSort4   = lnRpSort4
  loFormSet.lcRpPkFWrh  = lcRpPkFWrh
  loFormSet.lcRpIncWip  = lcRpIncWip
  loFormSet.llRpForAlo  = llRpForAlo
  loFormSet.llRpCond    = llRpCond
  loFormSet.lnRngAlias  = lnRngAlias
  loFormSet.lcSOrdStat  = lcSOrdStat
  DIMENSION loFormSet.laSortAry[4,2]
  =ACOPY(laSortAry,loFormSet.laSortAry)
  *-- Pick O.G.
CASE lnParam = 3
  loFormSet.lnRpGenNew = lnRpGenNew
  loFormSet.llRpPkHPck = llRpPkHPck
ENDCASE

loFormSet.llRpGenPik = llRpGenPik
loFormSet.lcRpAloNot = lcRpAloNot

*!*************************************************************
*! Name      : lfvExclOrd
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Exclude certain orders validation.
*!*************************************************************
*! Called from : Selection OG
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfvExclOrd()
*!*************************************************************
FUNCTION lfvExclOrd

llRpGdExcl = !llRpGdExcl
lcRpExSlct = ' '  && Default select by all.

CLEARREAD()


*-- end of lfvExclOrd.

*!*************************************************************
*! Name      : lfvSlctExc
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Exclude select by validation.
*!*************************************************************
*! Called from : Selection OG
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfvSlctExc()
*!*************************************************************
FUNCTION lfvSlctExc
lcRpExSlct = laOGFxFlt[lnDummyPos+1,6]

llClrSty2 = (lcRpExSlct # "S")
llClrOrd2 = (lcRpExSlct # "O")
llClrAcc2 = (lcRpExSlct # "A")

*B608997,1 AHS 09/17/2009 Adding new variable to hold the title of POs and cuttkts when Exclude = YES on OG [Start]
*lcPOTlt  = IIF(lcRpExSlct = 'P',"Purchase order number    ","Cutting ticket number    ")
lcPOExcl = IIF(lcRpExSlct = 'P',"Purchase order number    ","Cutting ticket number    ")
*lcBrwFld = IIF(lcRpScpMod = 'P',"PO :R :H= 'PO#    ', STATUS :R :H= 'Status' , Vendor :R :H= 'Vendor' ,"+;
"APVENDOR.cVenComp :R :H= 'Name' , Entered  :R :H= 'Entered' , Complete :R :H= 'Complete' ,"+;
"Open :R :H= 'Open' :P='999999' , POTOTAL  :R :H= 'PoTotal' :P='9999999999.99'" ,;
"PO :R :H='CutTkt#', STYLE :R :H='Style', STATUS :R :H='Status',"+;
"ENTERED :R :H='Issue', COMPLETE :R :H='Complete', SEASON :R :H='Season',"+;
"CDIVISION :R :H='Division'")
lcBrwFldExcl = IIF(lcRpExSlct = 'P',"PO :R :H= 'PO#    ', STATUS :R :H= 'Status' , Vendor :R :H= 'Vendor' ,"+;
  "APVENDOR.cVenComp :R :H= 'Name' , Entered  :R :H= 'Entered' , Complete :R :H= 'Complete' ,"+;
  "Open :R :H= 'Open' :P='999999' , POTOTAL  :R :H= 'PoTotal' :P='9999999999.99'" ,;
  "PO :R :H='CutTkt#', STYLE :R :H='Style', STATUS :R :H='Status',"+;
  "ENTERED :R :H='Issue', COMPLETE :R :H='Complete', SEASON :R :H='Season',"+;
  "CDIVISION :R :H='Division'")
*B608997,1 AHS 09/17/2009 Adding new variable to hold the title of POs and cuttkts when Exclude = YES on OG [End]

*NSTYORDER :R :H='Budget' :P = '999999',"+;
"Receive :R :H='Received' :P = '999999', Damage :R :H='Damaged' :P = '999999', Open :R :H='Open' :P = '999999'")


CLEARREAD()
*-- end of lfvSlctExc.

*!*************************************************************
*! Name      : lfvFabric
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid function of the Fabric
*!*************************************************************
*! Called from : Option grid [Fabric Get field]
*!*************************************************************
*! Calls       : FaBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvFabric

lcObjName = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field

*-- IF The user want to Browse or if the Fabric he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'FABRIC'))
  llObjRet = FaBrow(@lcObjVal , '*')
  lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfwOldVal
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Get fields and some of the Option grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfwOldVal

lcOldVal = EVALUATE(OGSYS18())      && Varible to hold the old value

*!*************************************************************
*! Name      : lfvOrdWare
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid function of the Order Warehouse
*!*************************************************************
*! Called from : Option grid [Order Warehouse Get field]
*!*************************************************************
*! Calls       : gfBrowWare()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvOrdWare

lcObjName = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field

*-- IF The user want to Browse or if the Warehouse he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , lcPkWOldVl , lcObjVal)
  lcPkWOldVl = lcObjVal
  &lcObjName. = lcObjVal
ENDIF    && End of IF


*!*************************************************************
*! Name      : lfvOrder
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid function of the Order number
*!*************************************************************
*! Called from : Option grid [Order number Get field]
*!*************************************************************
*! Calls       : OrdBrowO()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvOrder

PRIVATE lcObjNam , lcObjVal , llObjRet

lcObjNam = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field
lcObjVal = IIF(EMPTY(lcObjVal) , lcObjVal , PADL(ALLTRIM(lcObjVal) , 6 , '0'))

*-- IF The user want to Browse or if the Order number he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('O' + lcObjVal , 'ORDHDR'))
  llBrowse = .T.
  llObjRet = OrdBrowO(@lcObjVal , .F. , 'O')
  lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
  llBrowse = .F.
ENDIF    && End of IF
&lcObjNam = lcObjVal


*!*************************************************************
*! Name      : lfvOGStyle
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid function of the Style
*!*************************************************************
*! Called from : Option grid [Style Get field]
*!*************************************************************
*! Calls       : gfStyBrw()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvOGStyle

PRIVATE lnCurSelct,lcStyOrder
lnCurSelct = SELECT(0)
SELECT STYLE
lcStyOrder = ORDER()
SET ORDER TO cStyle

lcObjName = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field

*--IF The user want to Browse or if the Style he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'STYLE'))

  lcObjVal = gfStyBrw('M',"","",.F.)  &&Browse style major only.

  lcObjVal = IIF(!EMPTY(lcObjVal) , lcObjVal , lcOldVal)
  &lcObjName = lcObjVal

ENDIF    && End of IF


SELECT STYLE
SET ORDER TO &lcStyOrder
SELECT (lnCurSelct)
*-- end of lfvOGStyle.

*!*************************************************************
*! Name      : lfvPO
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid function of the PO number
*!*************************************************************
*! Called from : Option grid [PO number Get field]
*!*************************************************************
*! Calls       : PosBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvPO

lcObjName = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field
lcObjVal = IIF(EMPTY(lcObjVal) , lcObjVal , PADL(ALLTRIM(lcObjVal) , 6 , '0'))

*-- IF The user want to Browse or if the PO number he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('PP' + lcObjVal , 'POSHDR1'))
  llObjRet = PosBrow(@lcObjVal , '' , 'P')
  lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
ENDIF    && End of IF
&lcObjName = lcObjVal

*!*************************************************************
*! Name      : lfvCutTkt
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid function of the Cut ticket number
*!*************************************************************
*! Called from : Option grid [Cut ticket number Get field]
*!*************************************************************
*! Calls       : CutBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvCutTkt

lcObjName = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field
lcObjVal = IIF(EMPTY(lcObjVal) , lcObjVal , PADL(ALLTRIM(lcObjVal) , 6 , '0'))

*-- IF The user want to Browse or if the Cut ticket number he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('PU' + lcObjVal , 'POSHDR1'))
  llObjRet = CutBrow(@lcObjVal)
  lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
ENDIF    && End of IF
&lcObjName = lcObjVal

*!*************************************************************
*! Name      : lfvAccount
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid function of the Account
*!*************************************************************
*! Called from : Option grid [Account Get field]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvAccount

lcObjName = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvSepCor
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Separates/Coordinate validation
*!*************************************************************
*! Called from : Selection OG
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfvSepCor()
*!*************************************************************
FUNCTION lfvSepCor
llRpPikSep = (lcRpSepCor $ 'BS')
llRpPikCor = (lcRpSepCor $ 'BC')
=lfwFilter()
*-- end of lfvSepCor.

*!*************************************************************
*! Name      : lfvScopMod
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid function of the Select by
*!*************************************************************
*! Called from : Option grid [Select by Option]
*!*************************************************************
*! Calls       : lfChangeGrid()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvScopMod

llClrSty1 = (lcRpScpMod # "S")
llClrOrd1 = (lcRpScpMod # "O")
llClrAcc1 = (lcRpScpMod # "A")

lcPOTlt  = IIF(lcRpScpMod = 'P',"Purchase order number    ","Cutting ticket number    ")
lcBrwFld = IIF(lcRpScpMod = 'P',"PO :R :H= 'PO#    ', STATUS :R :H= 'Status' , Vendor :R :H= 'Vendor' ,"+;
  "APVENDOR.cVenComp :R :H= 'Name' , Entered  :R :H= 'Entered' , Complete :R :H= 'Complete' ,"+;
  "Open :R :H= 'Open' :P='999999' , POTOTAL  :R :H= 'PoTotal' :P='9999999999.99'" ,;
  "PO :R :H='CutTkt#', STYLE :R :H='Style', STATUS :R :H='Status',"+;
  "ENTERED :R :H='Issue', COMPLETE :R :H='Complete', SEASON :R :H='Season',"+;
  "CDIVISION :R :H='Division'")
*NSTYORDER :R :H='Budget' :P = '999999',"+;
"Receive :R :H='Received' :P = '999999', Damage :R :H='Damaged' :P = '999999', Open :R :H='Open' :P = '999999'")
CLEARREAD()

*:**************************************************************************
*: Name      : lfClearRep
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*: Purpose   : Hide and unhide the option of order hold reason.
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*: Return      : ....
*:**************************************************************************
*: Example     : = lfIncOnHld()
*:**************************************************************************
FUNCTION lfIncOnHld

*-- Clear read to Hide and unhide the option of order hold reason.
CLEARREAD()

*!*************************************************************
*! Name      : lfsrAcc1
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Account In Range Filter. 1
*!*************************************************************
FUNCTION lfsrAcc1
PARAMETERS lcParm
=lfsrAcc(lcParm,"1")
*-- end of lfsrAcc1.

*!*************************************************************
*! Name      : lfsrAcc2
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Account In Range Filter. 2
*!*************************************************************
FUNCTION lfsrAcc2
PARAMETERS lcParm
=lfsrAcc(lcParm,"2")
*-- end of lfsrAcc2.

*!*************************************************************
*! Name      : lfsrAcc
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Account In Range
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm,lcFlagNo
DO CASE
CASE lcParm = 'S'
  lnRngAlias = SELECT(0)
  GO TOP IN CUSTOMER
CASE lcParm = 'R'
  llClrAcc&lcFlagNo = .F.
  SELECT (lnRngAlias)
ENDCASE
*-- end of lfsrAcc.

*!*************************************************************
*! Name      : lfsrPO1
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : PO In Range Filter. 1
*!*************************************************************
*!
FUNCTION lfsrPO1
PARAMETERS lcParm
=lfsrPO(lcParm,"1")
*-- end of lfsrPO1.

*!*************************************************************
*! Name      : lfsrPO2
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : PO In Range Filter. 2
*!*************************************************************
*!
FUNCTION lfsrPO2
PARAMETERS lcParm
=lfsrPO(lcParm,"2")
*-- end of lfsrPO2.

*!*************************************************************
*! Name      : lfsrPO
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : P/O In Range
*!*************************************************************
FUNCTION lfsrPO
PARAMETERS lcParm,lcFlagNo

IF lcRpScpMod = 'K'
  llClrPO&lcFlagNo = .F.
ELSE
  DO CASE
  CASE lcParm = 'S'
    lnRngAlias = SELECT(0)
    SET ORDER TO VENCODE IN APVENDOR
    SELECT POSHDR
    SET RELATION TO POSHDR.vendor INTO APVENDOR ADDITIVE
  CASE lcParm = 'R'
    SELECT POSHDR
    SET RELATION OFF INTO APVENDOR
    llClrPO&lcFlagNo = .F.
    SELECT (lnRngAlias)
  ENDCASE
ENDIF
*-- end of lfsrPO.

*!*************************************************************
*! Name      : lfSrSty1
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Style In Range Filter. 1
*!*************************************************************
*!
FUNCTION lfSrSty1
PARAMETERS lcParm
=lfSRStyle(lcParm,"1")
*-- end of lfSrSty1.

*!*************************************************************
*! Name      : lfSrSty2
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Style In Range Filter. 2
*!*************************************************************
*!
FUNCTION lfSrSty2
PARAMETERS lcParm
=lfSRStyle(lcParm,"2")
*-- end of lfSrSty2.

*!*************************************************************
*! Name      : lfSRStyle
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Style In Range Filter.
*!*************************************************************
*!
FUNCTION lfSRStyle
PARAMETERS lcParm,lcFlagNo
DO CASE
CASE lcParm = 'S'  && Set code
  lnRngAlias = SELECT(0)
  *-- open this file in another alias to set order to Style Major
  *-- unique index.
  USE (oAriaApplication.DataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG STYLE IN 0
  SELECT STYLE
  SET ORDER TO TAG cStyle
  SET RELATION TO STYLE.STYLE INTO STYLE_X
  GO TOP IN STYLE
CASE lcParm = 'R'  && Reset code
  USE IN STYLE_X
  SELECT STYLE
  SET ORDER TO TAG STYLE
  llClrSty&lcFlagNo = .F.
  SELECT (lnRngAlias)
ENDCASE
*-- end of lfSRStyle.

*!*************************************************************
*! Name      : lfSrOrd1
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Order In Range Filter. 1
*!*************************************************************
*!
FUNCTION lfSrOrd1
PARAMETERS lcParm
=lfSROrder(lcParm,"1")
*-- end of lfSrOrd1.

*!*************************************************************
*! Name      : lfSrOrd2
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Order In Range Filter. 2
*!*************************************************************
*!
FUNCTION lfSrOrd2
PARAMETERS lcParm
=lfSROrder(lcParm,"2")
*-- end of lfSrOrd2.

*!*************************************************************
*! Name      : lfSROrder
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Order In Range
*!*************************************************************
FUNCTION lfSROrder
PARAMETERS lcParm,lcFlagNo
DO CASE
CASE lcParm = 'S'
  lnRngAlias = SELECT(0)
  *:   B609534,1 MMT 02/22/2011 When tryin to use the automatic allocation order screen it freezes[Start]
  *SELECT ORDHDR
  *B610884,1 MMT 10/16/2014 Add "Add user" field to order browser in Auto. Allocation screen[T20141006.0003][Start]
*!*	  SELECT cordtype,ORDER, STORE, ACCOUNT , Entered , Season, cDivision , cTERMCODE, SHIPVIA , STATUS, OPEN , CCURRCODE,BULK ;
*!*	    FROM ORDHDR WHERE cordtype+ORDER ='O' AND OPEN > 0 AND IIF(llRpIncHOr,STATUS$"OH",STATUS = "O") AND ;
*!*	    IIF(llRpExlBlk,.T., BULK='N') ORDER BY ORDER INTO CURSOR 'TMPORDHDR'
  SELECT cordtype,ORDER, STORE, ACCOUNT , Entered , Season, cDivision , cTERMCODE, SHIPVIA , STATUS, OPEN , CCURRCODE,BULK ,CADD_USER;
    FROM ORDHDR WHERE cordtype+ORDER ='O' AND OPEN > 0 AND IIF(llRpIncHOr,STATUS$"OH",STATUS = "O") AND ;
    IIF(llRpExlBlk,.T., BULK='N') ORDER BY ORDER INTO CURSOR 'TMPORDHDR'
  *B610884,1 MMT 10/16/2014 Add "Add user" field to order browser in Auto. Allocation screen[T20141006.0003][End]    
  SELECT 'TMPORDHDR'
  *B610886,1 MMT 10/19/2014 Automatic allocation screen order browser has no index[T20141016.0005][Start]
  INDEX on ORDER TAG 'TMPORDHDR'
  *B610886,1 MMT 10/19/2014 Automatic allocation screen order browser has no index[T20141016.0005][End]
  *:   B609534,1 MMT 02/22/2011 When tryin to use the automatic allocation order screen it freezes[End]
  lcCustRel = [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)]
  SET ORDER TO CUSTOMER IN CUSTOMER
  SET RELATION TO &lcCustRel INTO CUSTOMER && To customer file.
  GO TOP

CASE lcParm = 'R'
  *:   B609534,1 MMT 02/22/2011 When tryin to use the automatic allocation order screen it freezes[Start]
  *SELECT ORDHDR
  SELECT 'TMPORDHDR'
  *:   B609534,1 MMT 02/22/2011 When tryin to use the automatic allocation order screen it freezes[End]
  SET RELATION OFF INTO CUSTOMER && To customer file.
  llClrOrd&lcFlagNo = .F.
  SELECT (lnRngAlias)

ENDCASE
*-- end of lfSROrder.


*!*************************************************************
*! Name      : lfStySum
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT STYLE_X
  =SEEK(lcSty)
  SUM &lccomp TO lnTotcomp WHILE ALLTRIM(cStyMajor) = ALLTRIM(lcSty)
  SELECT STYLE
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF
  DO CASE
  CASE lnAddToVar = 1
    lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
    lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
    lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF
RETURN INT(lnTotcomp)
*-- end of lfStySum.

*!*************************************************************
*! Name      : lfTStr
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Function to change any value to a string
*!*************************************************************
*! Called from : lfGetExp()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Value of any type
*!*************************************************************
*! Return      : The passed value into String
*!*************************************************************
*
FUNCTION lfTStr
PARAMETERS laValTSw

PRIVATE lnNonDicml , lnDicml , lnDecmlVal

*DO CASE Statment
DO CASE

  *Case of Character or Memo
CASE TYPE('laValTSw') = 'C' .OR. TYPE('laValTSw') = 'M'
  RETURN ALLTRIM(laValTSw)

  *Case of Logical
CASE TYPE('laValTSw') = 'L'
  RETURN IIF(laValTSw , '.T.' , '.F.')

  *Case of Date
CASE TYPE('laValTSw') = 'D'
  RETURN DTOC(laValTSw)

  *Case of Numeric
CASE TYPE('laValTSw') = 'N'
  lnNonDicml = LEN(ALLTRIM(STR(laValTSw , 100 , 0)))    && Variable to hold the length of the Non decimal part
  lnDicml = 0   							              && Variable to hold the length of the decimal part
  lnDecmlVal = ABS(laValTSw - INT(laValTSw))            && Variable to hold the absolute value of the decimal part
  *DO WHILE Loop to get the length of the decimal part
  DO WHILE lnDecmlVal > 0
    lnDicml = lnDicml + 1
    lnDecmlVal = (lnDecmlVal * 10) - INT(lnDecmlVal * 10)
  ENDDO    && End of DO WHILE Loop
  lnDicml = IIF(lnDicml = 0 , lnDicml , lnDicml + 1)
  RETURN ALLTRIM(STR(laValTSw , lnNonDicml + lnDicml + 1 , lnDicml))

  *Otherwise
OTHERWISE
  RETURN ''
ENDCASE

*!*************************************************************
*! Name      : lfvIncWIP
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid function to Include WIP option in Allocate O.G.
*!*************************************************************
*! Called from : Alocate O.G>
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Value of any type
*!*************************************************************
*! Return      : The passed value into String
*!*************************************************************
*
FUNCTION lfvIncWIP

CLEARREAD()

*!*************************************************************
*! Name      : lfwAloc
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Allocation logic grid when function.
*!*************************************************************
*! Called from : Allocation logic OG
*!*************************************************************
*! Calls       : lfShowItem
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfwAloc()
*!*************************************************************
FUNCTION lfwAloc

IF lnOGSeting = 1
  laSortAry[1,1] = 1
  laSortAry[1,2] = [DTOS(COMPLETE)]
  laSortAry[2,1] = 2
  laSortAry[2,2] = [PRIORITY]
  laSortAry[3,1] = 3
  laSortAry[3,2] = [DTOS(START)]
  laSortAry[4,1] = 4
  laSortAry[4,2] = [ORDER]
  lnRpSort1 = 1
  lnRpSort2 = 2
  lnRpSort3 = 3
  lnRpSort4 = 4
  FOR lnCounter = 1 TO 4
    lcCounter = STR(lnCounter,1)
    =lfOGShowGet('lnRpSort'+lcCounter)
  ENDFOR
  *B611569,1 MMT 05/07/2018 Automatic Allocation - Allocation logic grid does not save filter correctly[T20180430.0035][Start]
  llRpForAlo  = loFormSet.llRpForAlo
  *B611569,1 MMT 05/07/2018 Automatic Allocation - Allocation logic grid does not save filter correctly[T20180430.0035][End]
ENDIF
*B607815,1 WAM 10/31/2006 Initialize OG variable
*B611569,1 MMT 05/07/2018 Automatic Allocation - Allocation logic grid does not save filter correctly[T20180430.0035][Start]
*llRpForAlo  = loFormSet.llRpForAlo
*B611569,1 MMT 05/07/2018 Automatic Allocation - Allocation logic grid does not save filter correctly[T20180430.0035][End]
*B607815,1 WAM 10/31/2006 (End)
IF llRpForAlo
  *-- Pick separates %
  IF llRpPikSep
    lnRpPikSep = 100
    =lfShowItem('lnRpPikSep')
  ENDIF

  *-- Pick coordinate %
  IF llRpPikCor
    lnRpPikCor = 100
    =lfShowItem('lnRpPikCor')
  ENDIF

  *-- cut-off units
  lnRpCutUnt = 0
  =lfShowItem('lnRpCutUnt')

  *-- allocate conditionally.

  IF (llUseDyes AND llFabDye)
    llRpCond = (lcRpSepCor='S')
    =lfShowItem('llRpCond')
  ENDIF

ENDIF
*-- When function for allocation grid.
*-- end of lfwAloc.

*!*************************************************************
*! Name      : lfShowItem
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Enable/Disable Allocation grid objects.
*!*************************************************************
*! Called from : lfvOGFrcAl
*!*************************************************************
*! Calls       : lfOGShowGet()
*!*************************************************************
*! Passed Parameters : Object,Object State(.T. or .F.)
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfShowItem
PARAMETERS lcItem,llState
PRIVATE lnItemPos
lnItemPos = ASCAN(laOGObjType,UPPER(lcItem))
IF lnItemPos > 0
  lnItemPos = ASUBSCRIPT(laOGObjType,lnItemPos,1)
  laOGObjCnt[lnItemPos] = llState
ENDIF
=lfOGShowGet(UPPER(lcItem))
*-- end of lfShowItem.

*!*************************************************************
*! Name      : lfvPikSep
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid function of the Pick Separates Min %
*!*************************************************************
*! Called from : Option grid [Pick Separates Min % Get field]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvPikSep

*IF The Pick Separates Min % value is less than 0 or greater than 100
IF lnRpPikSep < 0 .OR. lnRpPikSep > 100
  *** Message : "ð range from ð to ð           "
  ***           "            < Ok >            "
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *=gfModalGen("TRM00272B00000" , "DIALOG" , LANG_AUTOALLOC_MsgPickSep)
  =gfModalGen("TRM00272B00000" , "DIALOG" , IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_MsgPickSep,loFormSet.GetHeaderText("LANG_AUTOALLOC_MsgPickSep",loFormSet.HeaderAlias)))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  lnRpPikSep = lcOldVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvPikCor
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid function of the Pick coordinate Min %
*!*************************************************************
*! Called from : Option grid [Pick coordinate Min % Get field]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvPikCor

*IF The Pick coordinate Min % value is less than 0 or greater than 100
IF lnRpPikCor < 0 .OR. lnRpPikCor > 100

  *** Message : "ð range from ð to ð           "
  ***           "            < Ok >            "
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *=gfModalGen("TRM00272B00000" , "DIALOG" , LANG_AUTOALLOC_MsgPickCoord)
  =gfModalGen("TRM00272B00000" , "DIALOG" , IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_MsgPickCoord,loFormSet.GetHeaderText("LANG_AUTOALLOC_MsgPickCoord",loFormSet.HeaderAlias)))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  lnRpPikCor = lcOldVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvCutUnt
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid function of the Cut of units
*!*************************************************************
*! Called from : Option grid [Cut of units Get field]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvCutUnt

*IF The Cut of units value is less than 0
IF lnRpCutUnt < 0

  *** Message : "ð should be greater than zero."
  ***           "            < Ok >            "
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *=gfModalGen("TRM00234B00000" , "DIALOG" , LANG_AUTOALLOC_MsgCutOffUnt)
  =gfModalGen("TRM00234B00000" , "DIALOG" , IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AUTOALLOC_MsgCutOffUnt,loFormSet.GetHeaderText("LANG_AUTOALLOC_MsgCutOffUnt",loFormSet.HeaderAlias)))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  lnRpCutUnt = lcOldVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvPkFWrh
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid function of the Pick from warehouse
*!*************************************************************
*! Called from : Option grid [Pick from warehouse Get field]
*!*************************************************************
*! Calls       : lfvOrdWare()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvPkFWrh

=lfvOrdWare()
*--

*!*************************************************************
*! Name      : lfvOGFrcAl
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid function of the Force allocation
*!*************************************************************
*! Called from : Option grid [Force allocation Option]
*!*************************************************************
*! Calls       : lfShowItem()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvOGFrcAl

*-- if user want to force allocation

IF llRpForAlo
  *-- Pick separates %
  IF llRpPikSep
    lnRpPikSep = 100
    =lfShowItem('lnRpPikSep')
  ENDIF

  *-- Pick coordinate %
  IF llRpPikCor
    lnRpPikCor = 100
    =lfShowItem('lnRpPikCor')
  ENDIF

  *-- cut-off units
  lnRpCutUnt = 0
  =lfShowItem('lnRpCutUnt')

  *-- allocate conditionally.
  IF (llUseDyes AND llFabDye)
    llRpCond = (lcRpSepCor='S')
    =lfShowItem('llRpCond')
  ENDIF

ELSE  && else user want to unforce allocation.

  *-- Pick separates %
  IF llRpPikSep
    =lfShowItem('lnRpPikSep',.T.)
  ENDIF

  *-- Pick coordinate %
  IF llRpPikCor
    =lfShowItem('lnRpPikCor',.T.)
  ENDIF

  *-- cut-off units
  =lfShowItem('lnRpCutUnt',.T.)

  *-- allocate conditionally.
  IF (llUseDyes AND llFabDye)
    =lfShowItem('llRpCond',.T.)
  ENDIF

ENDIF
*-- end of lfvOGFrcAl.

*!*************************************************************
*! Name      : lfvSortBy
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : All Sort by validations
*!*************************************************************
*! Passed Parameters : Sort Number (1,2,3, Or 4)
*!*************************************************************
*! Return      : ....
*!*************************************************************
FUNCTION lfvSortBy
PARAMETERS lnSortItem

PRIVATE lcObjName , lnObjVal , lcObjGet , llOldValue , lnI , lcItmObj
llOldValue = .F.
lcObjGet  = OGSYS18()
lcObjName = "lnRpSort" + STR(lnSortItem,1)
lnObjVal  = EVALUATE(lcObjName)

lnI = 0
IF lnObjVal = 6
  FOR lnI = lnSortItem + 1 TO 4
    lcItmObj = "lnRpSort" + STR(lnI,1)
    IF EVALUATE(lcItmObj) <> 6
      llOldValue = .T.
      EXIT
    ENDIF
  ENDFOR
ELSE
  IF lnSortItem > 2
    FOR lnI = lnSortItem-1 TO 2 STEP -1
      lcItmObj = "lnRpSort" + STR(lnI,1)
      IF EVALUATE(lcItmObj) = 6
        llOldValue = .T.
        EXIT
      ENDIF
    ENDFOR
  ENDIF
ENDIF
llOldValue = IIF(llOldValue,llOldValue,;
  (lnObjVal<> 6) AND (ASCAN(laSortAry,lnObjVal) > 0))

IF llOldValue
  *-- Restore old values.
  STORE lcOldVal TO &lcObjName , &lcObjGet
  SHOW GET &lcObjGet
ELSE
  *-- Sort By Arrays make Sort Index.
  laSortAry[lnSortItem,1] = lnObjVal
  laSortAry[lnSortItem,2] = loFormSet.laIndexExp[lnObjVal]
ENDIF
*-- end of lfvSortBy.


*!*************************************************************
*! Name      : lfPickTkt
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : All Sort by validations
*!*************************************************************
*! Passed Parameters : Sort Number (1,2,3, Or 4)
*!*************************************************************
*! Return      : ....
*!*************************************************************
FUNCTION lfPickTkt

IF lcRpIncWip = 'S'
  lnShpNoElm = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'SHPMTHDR.SHIPNO',1),1)
  lnShpDtElm = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'SHPMTHDR.ETA',1),1)
  llFirst = .T.
  lcShpNO  = ""
  IF !EMPTY(laOGFxFlt[lnShpNoElm,6])
    SELECT (laOGFxFlt[lnShpNoElm,6])
    SCAN
      lcShpNO = lcShpNO + IIF(llFirst,"",",")+ "'" + ShipNo + "'"
      llFirst = .F.
    ENDSCAN
  ENDIF

  lcShpDat = "ETA BETWEEN '" + SUBSTR(laOGFxFlt[lnShpDtElm ,6],1,ATC('|',laOGFxFlt[lnShpDtElm ,6])-1)+;
    "' AND '"+SUBSTR(laOGFxFlt[lnShpDtElm ,6],ATC('|',laOGFxFlt[lnShpDtElm ,6])+1,10)+"'"

  DO CASE
  CASE !EMPTY(lcShpNO) AND !EMPTY(laOGFxFlt[lnShpDtElm ,6])
    loFormSet.lcShpCond = "CBUSDOCU = 'P' AND CSHPTYPE = 'P' AND SHIPNO IN (" +lcShpNO + ") AND " + lcShpDat
  CASE !EMPTY(lcShpNO) AND EMPTY(laOGFxFlt[lnShpDtElm ,6])
    loFormSet.lcShpCond = "CBUSDOCU = 'P' AND CSHPTYPE = 'P' AND SHIPNO IN (" +lcShpNO + ")"
  CASE EMPTY(lcShpNO) AND !EMPTY(laOGFxFlt[lnShpDtElm ,6])
    loFormSet.lcShpCond = "CBUSDOCU = 'P' AND CSHPTYPE = 'P' AND " + lcShpDat
  CASE EMPTY(lcShpNO) AND EMPTY(laOGFxFlt[lnShpDtElm ,6])
    loFormSet.lcShpCond = "CBUSDOCU = 'P' AND CSHPTYPE = 'P'"
  ENDCASE
ENDIF

*!*************************************************************
*! Name      : lfEvalShp
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Evaluate Open shipment to add it to availablity if user want.
*!*************************************************************
*! Passed Parameters : Sort Number (1,2,3, Or 4)
*!*************************************************************
*! Return      : ....
*!*************************************************************
FUNCTION lfEvalShp
PARAMETERS lcShpSty,lcShpWare,loFormSet

lcCurAlias = SELECT(0)

loFormSet.laWIP = 0

lcKeyExpr  = 'shipno+cstytype+cBusDocu+po+style+STR(lineno,6)+trancd'

*-- Collect Open Qty of the open shipments
FOR lnStrPos = 1 TO ALEN(loFormSet.laShpExp,1)
  lcKey = loFormSet.laShpExp[lnStrPos]+ 'PP'
  lcPOSLCond = "shipno = '" + loFormSet.laShpExp[lnStrPos] + "' AND CSTYTYPE = 'P' AND cBusDocu = 'P'"
  llPOSLNOpn = .F.
  IF lfopensql(loFormSet,'POSLN','POSLN1',lcPOSLCond)
    SELECT POSLN1
    LOCATE
    llPOSLNOpn = !EOF()
  ENDIF
  IF llPOSLNOpn
    SCAN FOR (STYLE = lcShpSty) AND (cWareCode = lcShpWare)
      *-- If select By C/T or P/O , collect only shipments for selected C/T or P/O
      IF loFormSet.lcRpScpMod $ 'KP' AND !EMPTY(loFormSet.laIncExprs[1,6])
        IF ASCAN(loFormSet.laString1,PO,1) = 0
          LOOP
        ENDIF
      ENDIF
      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        loFormSet.laWIP[lnI] = loFormSet.laWIP[lnI] + IIF(Trancd='3',QTY&lcI,0)
      ENDFOR
    ENDSCAN
  ENDIF
ENDFOR

*-- If select By C/T or P/O and there is an excluded C/T or P/O
IF loFormSet.llExclude AND (loFormSet.lcRpExSlct $ 'KP') AND !EMPTY(loFormSet.laFiltExp[1,6])
  FOR lnStrPos = 1 TO ALEN(loFormSet.laShpExp,1)
    lcKey = loFormSet.laShpExp[lnStrPos]+ 'PP'
    IF SEEK(lcKey)
      SCAN REST WHILE &lcKeyExpr = lcKey;
          FOR (STYLE = lcShpSty) AND (cWareCode = lcShpWare)
        IF ASCAN(loFormSet.laString2,PO,1) <> 0
          FOR lnI = 1 TO 8
            lcI = STR(lnI,1)
            loFormSet.laWIP[lnI] = loFormSet.laWIP[lnI] - IIF(Trancd='3',QTY&lcI,0)
          ENDFOR
        ENDIF
      ENDSCAN
    ENDIF
  ENDFOR
ENDIF
loFormSet.laWIP[9] = loFormSet.laWIP[1]+loFormSet.laWIP[2]+loFormSet.laWIP[3]+loFormSet.laWIP[4]+;
  loFormSet.laWIP[5]+loFormSet.laWIP[6]+loFormSet.laWIP[7]+loFormSet.laWIP[8]

SELECT (lcCurAlias )

*!*************************************************************
*! Name      : lfSumFab
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : sum a specific field for the current item in Fabric file.
*!*************************************************************
*! Passed Parameters : Sort Number (1,2,3, Or 4)
*!*************************************************************
*! Return      : ....
*!*************************************************************

*! B999999,1 WSH 05/09/2005, Change the function name as it conflicts with a function in Aria.Prg and
*!                           get quantity fields from ItemLoc file ... [Start]
*!*  FUNCTION lfSumFab
*!*  PARAMETERS lcFab,lccomp
*!*  lnTotcomp = 0
*!*  SELECT ITEM
*!*  lcLastOrd = ORDER()
*!*  SET ORDER TO CSTYLE
*!*  SUM &lcCOMP TO lnTotcomp WHILE CINVTYPE+STYLE = '0002'+lcFab
*!*  SET ORDER TO (lcLastOrd)
*!*  GO RECNO()
*!*  RETURN INT(lnTotcomp)
FUNCTION lfFabSum
LPARAMETERS lcFab, lccomp

LOCAL lnTotcomp, lnAlias
lnAlias   = SELECT(0)
lnTotcomp = 0

SELECT ITEM
lcLastOrd = ORDER()
SET ORDER TO cStyle
SUM &lccomp TO lnTotcomp WHILE CINVTYPE+STYLE = '0002'+lcFab
SET ORDER TO (lcLastOrd)

SELECT (lnAlias)
RETURN INT(lnTotcomp)

*!*  LOCAL lnAlias, lnTotcomp, lcStatment, lnConnHandl
*!*  lnAlias   = SELECT(0)
*!*  lnTotcomp = 0

*!*  SELECT (lcTmpFab)
*!*  IF SEEK(RTRIM(lcFab))
*!*    SUM &lcCOMP. TO lnTotcomp REST WHILE Fabric = lcFab
*!*  ENDIF

*!*  SELECT (lnAlias)
*!*  RETURN INT(lnTotcomp)
*! B999999,1 WSH 05/09/2005, [End]

*!*************************************************************
*! Name      : lfSrFab
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : PO In Range Filter. 1
*!*************************************************************
*!
FUNCTION lfSrFab
PARAMETERS lcParm

SELECT ITEM
SET ORDER TO cStyle
LOCATE

*!*************************************************************
*! Name      : lfHandlObj
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Handle status of tool bar and option menu
*!*************************************************************
*!
FUNCTION lfHandlObj
PARAMETERS loFormSet

STORE 0 TO loFormSet.lnSelRec,loFormSet.lnSelAlo,loFormSet.lnAloRec

SELECT (loFormSet.lcTmpOrdLn)
lnRecNo = RECNO()
GO TOP
REPLACE cPosition WITH 'T'
SCAN
  IF lnSel = 1
    loFormSet.lnSelRec = loFormSet.lnSelRec + 1
    IF totpik <> 0
      loFormSet.lnSelAlo = loFormSet.lnSelAlo + 1
    ENDIF
  ENDIF
  IF totpik <> 0
    loFormSet.lnAloRec = loFormSet.lnAloRec + 1
  ENDIF
ENDSCAN
GO BOTTOM
REPLACE cPosition WITH 'E'

IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF

loFormSet.llSelAllSt = IIF(loFormSet.lnSelRec = RECCOUNT(), .F. , .T.)
loFormSet.llSelNonSt = IIF(loFormSet.lnSelRec = 0 , .F. , .T.)
loFormSet.AriaForm1.grdOrders.cmdSelect.ENABLED     = !EOF(loFormSet.lcTmpOrdLn)
loFormSet.AriaForm1.grdOrders.cmdSelect.REFRESH
loFormSet.AriaForm1.grdOrders.cmdSelectAll.ENABLED  = loFormSet.llSelAllSt
loFormSet.AriaForm1.grdOrders.cmdSelectAll.REFRESH
loFormSet.AriaForm1.grdOrders.cmdSelectNone.ENABLED = loFormSet.llSelNonSt
loFormSet.AriaForm1.grdOrders.cmdSelectNone.REFRESH
loFormSet.AriaForm1.grdOrders.cmdInvert.ENABLED     = !EOF(loFormSet.lcTmpOrdLn)
loFormSet.AriaForm1.grdOrders.cmdInvert.REFRESH

*HBG 1/24/2005 Modify code to apply the new interface [Begin]
*OAriaApplication.oToolBar.ChangeButtonStatus('pbScop','ENABLED')
loFormSet.oToolBar.ChangeButtonStatus('pbScop','ENABLED')
*HBG [End]
IF loFormSet.lnSelRec = 0
  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *OAriaApplication.oToolBar.ChangeButtonStatus('pbAlo','DISABLE')
  *OAriaApplication.oToolBar.ChangeButtonStatus('pbRel','DISABLE')
  *OAriaApplication.oToolBar.ChangeButtonStatus('pbGen','DISABLE')
  loFormSet.oToolBar.ChangeButtonStatus('pbAlo','DISABLE')
  loFormSet.oToolBar.ChangeButtonStatus('pbRel','DISABLE')
  loFormSet.oToolBar.ChangeButtonStatus('pbGen','DISABLE')
  *HBG  [End]
ELSE
  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *OAriaApplication.oToolBar.ChangeButtonStatus('pbAlo','ENABLED')
  loFormSet.oToolBar.ChangeButtonStatus('pbAlo','ENABLED')
  *HBG [End]
  lcRelSt = IIF(loFormSet.lnSelAlo = 0 , 'DISABLE', 'ENABLED')
  *HBG 1/24/2005 Modify code to apply the new interface [Begin]
  *OAriaApplication.oToolBar.ChangeButtonStatus('pbRel',lcRelSt)
  *OAriaApplication.oToolBar.ChangeButtonStatus('pbGen',lcRelSt)
  loFormSet.oToolBar.ChangeButtonStatus('pbRel',lcRelSt)
  loFormSet.oToolBar.ChangeButtonStatus('pbGen',lcRelSt)
  *HBG [End]
ENDIF

*B131801,1 MMT 26/04/2006 remove progress bar after finishing [Start]
loFormSet.AriaForm1.grdOrders.grdMultiSelectionGrid.REFRESH()
loFormSet.AriaForm1.grdOrders.grdMultiSelectionGrid.AFTERROWCOLCHANGE()
loFormSet.oPross.HIDE()
*B131801,1 MMT 26/04/2006 remove progress bar after finishing [End]

*!*************************************************************
*! Name      : lfvGenPktk
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid fuction for generate new Piktkt No
*!*************************************************************
*!
FUNCTION lfvGenPktk

CLEARREAD()

*!*************************************************************
*! Name      : lfSrFab
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : PO In Range Filter. 1
*!*************************************************************
*!
FUNCTION lfNavigate
PARAMETERS loFormSet,lcGoTo

DO CASE
CASE lcGoTo = 'T'  && Top
  SELECT (loFormSet.lcTmpOrdLn)
  GO TOP
CASE lcGoTo = 'N'  && Next
  SELECT (loFormSet.lcTmpOrdLn)
  IF !EOF()
    SKIP 1
  ENDIF
CASE lcGoTo = 'P'  && Previous
  SELECT (loFormSet.lcTmpOrdLn)
  IF !BOF()
    SKIP -1
  ENDIF
CASE lcGoTo = 'E'  && End
  SELECT (loFormSet.lcTmpOrdLn)
  GO BOTTOM
ENDCASE

*:   B610039,1 MMT 08/08/2012 Automatic Allocation screen gives error when user select more than 25 codes from OG[Start]
*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 08/08/2012
*! Purpose   : Convert a list of values into a cursor
*!*************************************************************
*!
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName
laTempacstru[1,2]='C'
laTempacstru[1,3]= 6
laTempacstru[1,4]= 0
= gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1
  lnEnd=AT('|',lcValuesToConvert)
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp )
    APPEND BLANK
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"")
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO
  IF lnEnd = 0
    SELECT(lcCursorTemp )
    APPEND BLANK
    REPLACE &lcFieldName  WITH lcValuesToConvert
  ENDIF
ENDIF
RETURN .T.
*:   B610039,1 MMT 08/08/2012 Automatic Allocation screen gives error when user select more than 25 codes from OG[End]
