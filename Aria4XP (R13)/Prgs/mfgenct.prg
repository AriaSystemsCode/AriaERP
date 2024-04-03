*:************************************************************************
*: Program file  : MFGENCT.PRG
*: Program desc. : Generate C/t and P/O form orders and Ggenerate C/T from plan
*: For screen    : MFGENCT.SCX
*:         System: Aria 4XP
*:         Module: Manufactering Module
*:      Developer: AHMED MAHER (AMH)
*: Passed Parameters   : lcChoice: P.  For generate P/O from orders
*:                                 C.  For generate C/T from orders
*:                                 L.  For generate C/T from Plan
*: Tracking Job Number : N037499
*:************************************************************************
*: Example             : DO MFGENCT
*: Modifictions:
*: B607844,1 MMT 11/28/2006 Fix bug of not all lines of order are updated in Cutpick T20061122.0004,T20061114.0002
*: B607874,1 MMT 12/13/2006 Fix bug of not all lines of order are updated in Cutpick in case generate PO from SO T20061108.0001
*: B607958,1 WAM 02/01/2007 Fix bug of specific orders not shown when the first order line is deleted
*: B608032,1 MMT 04/10/07 fix bug of not reset Exchange rate when change mode to Select Mode T20070223.0007
*: B608058,1 MMT 04/24/20007 fix bug of not saving packing info while generating PO From SO T20070219.0005
*: B608122,1 TMI 06/13/2007 Call the gfSequence global function with passing the new parameters lcTranType,lcTable,lcTag ( T20070524.0013)
*: C200804,1 HIA 06/28/2007 Create un\grouped po lines accoring to llGrpSO flag [custom developed as standard as many customers need this feature]
*: C200804,1 ALA 06/30/2008 Create un\grouped po lines accoring to llRPGrpSO flag [custom developed as standard as many customers need this feature]
*: B608264,1 WAM 09/10/2007 Update dyelot WIP and WO quantity when generate PO from SO
*: N000606,1 NNA 10/06/2007 Add new option to run (Generate P/O from OTS) from this program
*: N000587,1 WAM 12/01/2007 Don't read duty currency and exchange rate.
*: N000587,1 WAM 12/01/2007 Get currency, exchange rate and unit for each cost element from BOM file
*: B608553,1 MMT 05/13/2008 Fix bug of Po no already exist message  [T20080408.0004]
*: B608597,1 ALAA 06/13/2008 Commnetd out to rename varibale llGrpSO to be llRPGrpSO
*: B608612,1 MHM 07/13/2008 Modify the generate PO from sales order program to call the PO cost sheet program using the DO Fox
*: B608670,1 WAM 08/28/2008 Get currency, exchange rate and unit for each cost element from BOM file
*: B608675,1 WAM 09/02/2008 Fix bug while generate CT from SO results in create CT without lines [T20080827.0006]
*: B608873,1 WAM 05/25/2009 Changes and fixes in generate PO from sales orders [T20081218.0028]
*: B608925,1 MMT 07/05/2009 Fix bug of Object Overlapping in PO Default Screen if Not Multi. Currency Co.[T20080918.0002]
*! B609028,1 MMT 10/08/2009 Fix bug of can not edit p.price in line detail screen[T20080429.0012]
*! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[T20091118.0003]
*! B609359,1 MMT 07/22/2010 Make Order Browser Browse from ORdhdr not from Tmp. File[T20100203.0030]
*! B609696,1 MMT 10/13/2011 Fix bug of not updating field apvendor.nVenOpnPo [T20110622.0022]
*! B609767,1 SAB 12/11/2011 Fix error when run the Generate PO from open to sell [T20110521.0001]
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[T20100922.0014]
*! E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004]
*! B609917,1 MMT 05/14/2012 Generate PO from SO calculates PO Qty incorrectly  in case of percent <> 100[T20120329.0001]
*! B610089,1 MMT 09/18/2012 Generate PO from SO saves incorrect currency unit in POSHDR{T20120912.0001}
*! B610190,1 MMT 01/03/2013 Modify PO screen and Generate PO from SO to Handle price/size in cost sheet[T20121220.0001]
*! B610190,2 MMT 01/13/2013 Modify Generate PO from OTS to Handle price/size in cost sheet[T20121220.0001]
*! B610202,1 HIA 01/17/2013 Aria4xp - PO - Generate PO from SO/Open to sell [T20130107.0006]
*! B610213,1 HIA 01/23/2013 Aria4xp - PO - Generate PO from SO/Open to sel [T20130107.0006]
*! B610572,1 TMI 10/31/2013 Adjust the header of grdPOLines in case of C/T [T20131031.0009 Task] 
*! B611059,1 MMT 09/28/2015 Generate PO from SO does not add record to EDITRNAS table[T20150130.0005]
*:************************************************************************
PARAMETERS lcChoice

lcChoice   = IIF(TYPE('lcChoice')='C',lcChoice,'P')

#INCLUDE R:\aria4xp\prgs\mfgenct.h

PRIVATE lcAllVar
lcAllVar = 'lcChoice,laFabrics,laPercet,lcOrdAlias,llOrdRange,lcStyGroup,lcTmpOrder,llRetToSel,llSelMode,lcOldNdx,'+;
  'lnTotal1,lnTotal2,lnTotal3,lnTotal4,lnTotal5,lnTotal6,lnTotal7,lnTotal8,lcOldCtkNo,lnColorStr,lnColorLen,'+;
  'lnUnit1,lnUnit2,lnPRate,lnDRate,lcTerm,lcShip,lnType,puType,llColorExt,llFirst,lcModal,lcSort,'+;
  'lcRPSort1,lcRPSort2,lcRPSort3,lcVenName,lcCont,'+;
  'lcPhone,lcHdrFile,lcLinFile,lcPOH,lcPOLine,lcOrdLine,lcCutPick,lcTmpOrd,lcPrintAll,lcOrdGroup,lcFabrics,'+;
  'lcStyUnAll,lcStyTmp,lcVendor,lcWareCode,lcPCurr,lcDCurr,lcMajorPic,lnMajorLen,lcFirstCT,lcLastCt,'+;
  'lcIType1,lcIType2,lcIType3,lcIType4,lcIType5,lcIType6,lcIType7,lcSelFile,llRPUnAWip,llRpBOPFab,llAllocate,'+;
  'lcSign1,lcSign2,laSeason,laTerm,laDiv,laShip,laGroup,lcRpBaseOn,llDispPric,llStyMark,lnTotQty,lnSelPrice,'+;
  'lnAllQty,lnRotSub,lnGrosMrgn,llWareHous,llDyelot,llEditExRt,llMulCurr,lcMSLbl1,lcMSLbl2,lcMSLbl3,lcMSLbl4,'+;
  'lcMSLbl5,lcMSLbl6,lcMSLbl7,lcISLbl1,lcISLbl2,lcISLbl3,lcISLbl4,lcISLbl5,lcISLbl6,lcISLbl7,'+;
  'llFDyelot,llGManualy,loPOSHDR,loCUSTOMER,loORDLINE,lcGenProj,lcDefTemp,lcPoType,'+;
  'lc_PMPrjDt,lc_PrjHist,lc_PrjAudt,lc_PMPrjRl,lc_Parser,MsgNo,llDoProj,lcMjrTtl,lcNMjrTl,lnMjorCnt,'+;
  'lnOldVal,loOrdHdr,loStyle,loScale,loCodes,loPMPRJHD,loPMPRJDT,loPMPRJRL,loSycCurr,loApVendor,loWAREHOUS,'+;
  'loCUTPICK,loPOSLN,llStyConfg,lcRpCtStat,ldRpFrDate,loForCast,lcTmpFrCst,loInvLine,lcWarehouse'

*C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [BEGIN]
*lcAllVar = lcAllVar + ',llGRpSO'
*C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [END]
*608597,1 ALAA [Begin] Commnetd out to rename varibale llGrpSO to be llRPGrpSO
lcAllVar = lcAllVar + ',llRPGRpSO'
*608597,1 ALAA [END] Commnetd out to rename varibale llGrpSO to be llRPGrpSO

*B608873,1 WAM 05/25/2009 Add a new option to generate one PO per each style major. Read the complete and availabe dates in the PO defaults
lcAllVar = lcAllVar + ',llRPGrpByST,ldComplete,ldAvailable'
*B608873,1 WAM 05/25/2009 (End)

*! B609028,1 MMT 10/08/2009 Fix bug of can not edit p.price in line detail screen[Start]
lcAllVar = lcAllVar + ',lEditStyPrice,lnPOldVal'
*! B609028,1 MMT 10/08/2009 Fix bug of can not edit p.price in line detail screen[End]

*! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[Start]
lcAllVar = lcAllVar + ',LLRPGNLPJ'
*! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[End]

DIMENSION laAllVar[1,1]
STORE '' TO laAllVar
=gfSubStr(lcAllVar,@laAllVar,',')

LOCAL lnI
FOR lnI = 1 TO ALEN(laAllVar,1)
  PRIVATE &laAllVar[lnI,1].
ENDFOR

DIMENSION laFabrics[1],laPercet[8]
STORE ''  TO lcOrdAlias, lcStyGroup, lcWarehouse
STORE .F. TO llOrdRange
lcTmpOrder = gfTempName()
lcTmpFrCst = gfTempName()
STORE .F. TO llRetToSel , llSelMode
lcOldNdx  = ''

*-- Array to take the percentage of generation
STORE 100 TO laPercet

*-- Variable to hold the total qtys to be generated
STORE  0  TO lnTotal1,lnTotal2,lnTotal3,lnTotal4,lnTotal5,lnTotal6,lnTotal7,lnTotal8
lcOldCtkNo = ''

*-- Hold the lenght of the color and its start position in case
*-- there is a color segment in style stucture
STORE  0  TO lnColorStr,lnColorLen

*-- Hold the unit of the foreign curr.
STORE  1  TO lnUnit1,lnUnit2,lnPRate,lnDRate

*-- Hold the number of term,ship,term
STORE  1  TO lnType,puType
STORE ""  TO lcTerm,lcShip

STORE .F. TO llColorExt

*-- To check if the first time to run the option grid
STORE .T. TO llFirst

STORE ''  TO lcModal,lcSort,lcRPSort1,lcRPSort2,lcRPSort3,;
  lcVenName,lcCont,lcPhone,lcHdrFile,lcLinFile

STORE '' TO  lcPOH,lcPOLine,lcOrdLine,lcCutPick,lcTmpOrd,lcPrintAll,lcOrdGroup,lcFabrics,lcStyUnAll,lcStyTmp,lnOldVal

STORE '' TO  laFabrics,lcVendor,lcWareCode,lcPCurr,lcDCurr,lcMajorPic,lnMajorLen,;
  lcFirstCT,lcLastCt,lcIType1,lcIType2,lcIType3,lcIType4,lcIType5,lcIType6,lcIType7,lcSelFile

STORE .F. TO llRPUnAWip,llRpBOPFab,llAllocate
STORE '*' TO lcSign1,lcSign2
*C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [BEGIN]
*STORE .F. TO llGrpSO
*C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [END]
*608597,1 ALAA [Begin] Commnetd out to rename varibale llGrpSO to be llRPGrpSO
STORE .F. TO llRPGrpSO
*608597,1 ALAA [END] Commnetd out to rename varibale llGrpSO to be llRPGrpSO

*B608873,1 WAM 05/25/2009 Add a new option to generate one PO per each style major. Read the complete and availabe dates in the PO defaults
STORE .F. TO llRPGrpByST
STORE oAriaApplication.SystemDate+90 TO ldComplete,ldAvailable
*B608873,1 WAM 05/25/2009 (End)

lcRpCtStat = 'B'
ldRpFrDate = {}

DIMENSION laSeason[1], laTerm[1], laDiv[1], laShip[1], laGroup[1]
STORE "" TO laSeason,laTerm,laDiv,laShip,laGroup
lcRpBaseOn = "P"

*------- llDispPric  -> hold setting variable to display selling price & gross margin
*------- lnTotQty    -> Hold TOTAl Qty Of one Order
*------- lnSelPrice  -> Hold Average Selling Price
*------- lnAllQty    -> Hold All Qty For All Orders
*------- lnRotSub    -> Hold Cost Price Or Selling price Depend on llStyMark
*------- lnGrosMrgn  -> Hold Gross Margin
llDispPric  = .F.
llStyMark   = .F.

*N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
*IF lcChoice $ 'PC'
  *llDispPric = IIF(lcChoice='P',gfGetMemVar('M_PoDspPrc'),gfGetMemVar('M_MfDspPrc'))
IF lcChoice $ 'PCO'
  llDispPric = IIF(lcChoice$'PO',gfGetMemVar('M_PoDspPrc'),gfGetMemVar('M_MfDspPrc'))
*N000606,1 NNA (End)

  llStyMark  = gfGetMemVar('M_stymark')   ='T'
ENDIF
STORE 0 TO lnTotQty,lnSelPrice,lnAllQty,lnRotSub,lnGrosMrgn

*--Get the values from the memory variable
llWareHous = ALLTRIM(gfGetMemVar('M_WareHouse'))= 'Y'
IF llWareHous
  =gfOpenFile(oAriaApplication.DAtaDir+ 'STYDYE'  , 'STYDYE' , 'SH')
ENDIF

llDyelot   = ALLTRIM(gfGetMemVar('M_DYELOT'))   = 'Y'
llEditExRt = gfGetMemVar('LLEDITEXRA')
llMulCurr  = gfGetMemVar('llMulCurr')
lcMSLbl1   = gfGetMemVar('M_CMSLbl1')
lcMSLbl2   = gfGetMemVar('M_CMSLbl2')
lcMSLbl3   = gfGetMemVar('M_CMSLbl3')
lcMSLbl4   = gfGetMemVar('M_CMSLbl4')
lcMSLbl5   = gfGetMemVar('M_CMSLbl5')
lcMSLbl6   = gfGetMemVar('M_CMSLbl6')
lcMSLbl7   = gfGetMemVar('M_CMSLbl7')
lcISLbl1   = gfGetMemVar('M_CISLbl1')
lcISLbl2   = gfGetMemVar('M_CISLbl2')
lcISLbl3   = gfGetMemVar('M_CISLbl3')
lcISLbl4   = gfGetMemVar('M_CISLbl4')
lcISLbl5   = gfGetMemVar('M_CISLbl5')
lcISLbl6   = gfGetMemVar('M_CISLbl6')
lcISLbl7   = gfGetMemVar('M_CISLbl7')
lcIType1   = gfGetMemVar('M_cIType1')
lcIType2   = gfGetMemVar('M_cIType2')
lcIType3   = gfGetMemVar('M_cIType3')
lcIType4   = gfGetMemVar('M_cIType4')
lcIType5   = gfGetMemVar('M_cIType5')
lcIType6   = gfGetMemVar('M_cIType6')
lcIType7   = gfGetMemVar('M_cIType7')
llFDyelot  = gfGetMemVar('M_MATDYE')='Y'
llGManualy = IIF(lcChoice='C',(gfGetMemVar('M_GENCTNUM') = "Y"),(gfGetMemVar('M_GENSTORN') = "Y"))
llStyConfg = gfGetMemVar('M_STYCNFG')='Y'

*! B609028,1 MMT 10/08/2009 Fix bug of can not edit p.price in line detail screen[Start]
lEditStyPrice = (gfGetMemVar('M_UPDPOPR') = 'Y')
lnPOldVal = 0
*! B609028,1 MMT 10/08/2009 Fix bug of can not edit p.price in line detail screen[End]

*! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[Start]
LLRPGNLPJ = .F.
*! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[End]

*-- Picture of the style major.
lcMajorPic = gfItemMask("PM")

*-- Lenght of the the style major.
lnMajorLen = LEN(lcMajorPic)
llRpIgPick = .F.

*-- lcGenProj = Project Generating Type [Automatic or Inquire]
*-- lcDefTemp = Project Default Template
*-- lcPoType  = To Show 'P/O' or 'C/T' in the Msg. of Generating Project Inquir
*-- MsgNo     = to Prevent the Msg. of Generating Project Inquir from Appearing more than one time
*--             if we Generating more one PO or Cut Ticket
*-- llDoProj  = to Save .F. if the user chose to not Generate Progect and if there are more one PO or
*--             CutTicket , So in this case Program cann't generate Progect for the Second PO or CutTkt
STORE " " TO lcGenProj,lcDefTemp,lcPoType
STORE " " TO lc_PMPrjDt,lc_PrjHist,lc_PrjAudt,lc_PMPrjRl,lc_Parser
STORE 0 TO MsgNo
STORE .T. TO llDoProj

*! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[Start]
*!*  IF lcChoice $ 'CP'
*!*    IF lcChoice = 'P'
IF lcChoice $ 'CPLO'
  IF lcChoice $ 'OP'
*! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[End]
    lcGenProj = gfGetMemVar('M_POGENPRJ')
  ELSE
    lcGenProj = gfGetMemVar('M_MFGENPRJ')
  ENDIF
  IF lcGenProj $ 'AI'
    *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[Start]
    *IF lcChoice = 'P'
    IF lcChoice $ 'PO'
    *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[End]
      lcDefTemp = gfGetMemVar('M_PODEFTMP')
    ELSE
      lcDefTemp = gfGetMemVar('M_MFDEFTMP')
    ENDIF
    *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[Start]
    lcPoType   = IIF(lcChoice = 'P',LANG_MFGENCT_PO,LANG_MFGENCT_CT)
    *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[End]
  ENDIF
ENDIF

*-- Get the information of the major part for the style sturct.
lcMjrTtl  = ALLTRIM(gfItemMask('HM'))+'...'
lcNMjrTl  = ALLTRIM(gfItemMask('HN'))+'...'
lnMjorCnt = gfItemMask("SM")

*-- Remote Table objects.
STORE .NULL. TO loPosHdr,loCustomer,loOrdLine,loOrdHdr,loStyle,loScale,loCodes,;
  loPMPRJHD,loPMPRJDT,loPMPRJRL,loSycCurr,loApVendor,loWAREHOUS,loCUTPICK,loPOSLN,loForCast,loInvLine

*-- Call of the main screen
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
*DO FORM (oAriaApplication.ScreenHome+"MFGENCT.SCX")
=gfCallForm('MFGENCT')
*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
*!*************************************************************
*! Name      : lfOrdWhen
*! Developer : Ahmed Maher
*! Date      : 11/25/2004
*! Purpose   : When function of the order lines browse
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfOrdWhen()
*!*************************************************************
FUNCTION lfOrdWhen
  LPARAMETERS loFormSet
  WITH loFormSet.AriaForm1
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *.cmdselect.CAPTION = IIF(EVALUATE(loFormSet.lcOrdLine+'.lSelect'),LANG_MFGENCT_UNSELECT,LANG_MFGENCT_SELECT)
    .cmdselect.CAPTION = IIF(EVALUATE(loFormSet.lcOrdLine+'.lSelect'),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_UNSELECT,loFormSet.GetHeaderText("LANG_MFGENCT_UNSELECT",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_SELECT,loFormSet.GetHeaderText("LANG_MFGENCT_SELECT",loFormSet.HeaderAlias)))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    .cmdselect.ENABLED = loFormSet.ActiveMode='E' AND !loFormSet.llAllocate
    =loFormSet.loStyle.SEEK(EVALUATE(loFormSet.lcOrdLine+'.Style')) AND loFormSet.loScale.SEEK('S'+STYLE.SCALE)
    .txtScaleDesc.VALUE = SCALE.cScl_Desc
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      .txtSize&lcCount..VALUE = EVALUATE('Scale.SZ'+lcCount)
    ENDFOR
  ENDWITH

  *!*************************************************************
  *! Name      : lfShow
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 11/25/2004
  *! Purpose   : Show procedure for the objects of the screen with different modes
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   :
  *!*************************************************************
FUNCTION lfShow
LPARAMETERS loFormSet,lcModeToChange
  DO CASE
    CASE lcModeToChange='S'
      =lfClrAllo(loFormSet)
      WITH loFormSet.AriaForm1
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *.cmdNotes.CAPTION    = LANG_MFGENCT_ALLOCATE
        .cmdNotes.CAPTION    = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ALLOCATE,loFormSet.GetHeaderText("LANG_MFGENCT_ALLOCATE",loFormSet.HeaderAlias))
        *N000682,1 11/20/2012 MMT Globlization changes[End]

        .cmdPO.ENABLED       = .F.
        .cmdGenerate.ENABLED = .F.
        oAriaApplication.otoolbar.ChangeButtonStatus('cmdScope','ENABLED')
      ENDWITH
      loFormSet.llAllocate = .F.
      =lfAfterInit(loFormSet)
      IF loFormSet.llSelMode
        =lfRetToSel(loFormSet)
        *-- IF the temp order line not empty allow user to select anoter
        *-- order or group of orders without go to OG by go to Edit mode [Begin]
        IF loFormSet.llRetToSel
          loFormSet.ActiveMode = 'E'
          STORE .F. TO loFormSet.llRetToSel , loFormSet.llSelMode
          =lfShow(loFormSet,loFormSet.ActiveMode)
        ENDIF
      ENDIF

      WITH loFormSet.AriaForm1
        .txtScaleDesc.VALUE = ''
        FOR lnCount = 1 TO 8
          lcCount = STR(lnCount,1)
          .txtSize&lcCount..VALUE = ''
        ENDFOR
      ENDWITH

      STORE 1   TO loFormSet.lnPRate,loFormSet.lnDRate
      STORE 100 TO loFormSet.laPercet
      STORE .F. TO loFormSet.llAllocate

      *B608032,1 MMT 04/10/07 fix bug of not reset Exchange rate when change mode to Select Mode[Start]
      STORE oAriaApplication.BaseCurrency TO loFormSet.lcPCurr,loFormSet.lcDCurr
      STORE ""  TO loFormSet.lcTerm,loFormSet.lcShip,loFormSet.lcVendor,loFormSet.lcWareCode,loFormSet.lcVenName
      *B608032,1 MMT 04/10/07 fix bug of not reset Exchange rate when change mode to Select Mode[End]

      IF loFormSet.llFirst
        =lfvScope(loFormSet)
      ENDIF
      IF !EMPTY(loFormSet.lcFirstCT)
        WITH loFormSet.AriaForm1
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
          *.cmdNotes.CAPTION = LANG_MFGENCT_NOTES
          .cmdNotes.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_NOTES,loFormSet.GetHeaderText("LANG_MFGENCT_NOTES",loFormSet.HeaderAlias))
          *N000682,1 11/20/2012 MMT Globlization changes[End]

          .cmdNotes.ENABLED = .T.

          *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
          *.cmdPO.Caption    = IIF(loFormSet.lcChoice='P',LANG_MFGENCT_POCMD,"\<"+LANG_MFGENCT_CT)
          .cmdPO.Caption    = IIF(loFormSet.lcChoice $ 'PO',;
          IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_POCMD,loFormSet.GetHeaderText("LANG_MFGENCT_POCMD",loFormSet.HeaderAlias)),;
          "\<"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CT,loFormSet.GetHeaderText("LANG_MFGENCT_CT",loFormSet.HeaderAlias)))
          *N000682,1 11/20/2012 MMT Globlization changes[End]

          *N000606,1 NNA (End)

          .cmdPO.ENABLED    = .T.
        ENDWITH
      ENDIF

      loFormSet.laPanelObj[1,6] = "SVE"
      GO TOP IN (loFormSet.lcHdrFile)
      IF EOF(loFormSet.lcHdrFile)
        oAriaApplication.otoolbar.ChangeButtonStatus('cmdScope','ENABLED')
      ELSE
        oAriaApplication.otoolbar.ChangeButtonStatus('cmdScope','DISABLE')
      ENDIF
      loFormSet.RefreshAll

    CASE lcModeToChange='V'
      loFormSet.llSelMode = .T.
      IF !EMPTY(loFormSet.lcFirstCT)
        WITH loFormSet.AriaForm1
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
          *.cmdNotes.CAPTION = LANG_MFGENCT_NOTES
          .cmdNotes.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_NOTES,loFormSet.GetHeaderText("LANG_MFGENCT_NOTES",loFormSet.HeaderAlias))
          *N000682,1 11/20/2012 MMT Globlization changes[End]

          .cmdNotes.ENABLED = .T.

           *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
           *.cmdPO.Caption = IIF(loFormSet.lcChoice='P',LANG_MFGENCT_POCMD,"\<"+LANG_MFGENCT_CT)
           *N000682,1 11/20/2012 MMT Globlization changes[Start]
           *.cmdPO.Caption = IIF(loFormSet.lcChoice $ 'PO',LANG_MFGENCT_POCMD,"\<"+LANG_MFGENCT_CT)
           .cmdPO.Caption = IIF(loFormSet.lcChoice $ 'PO',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_POCMD,loFormSet.GetHeaderText("LANG_MFGENCT_POCMD",loFormSet.HeaderAlias)),"\<"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CT,loFormSet.GetHeaderText("LANG_MFGENCT_CT",loFormSet.HeaderAlias)))
           *N000682,1 11/20/2012 MMT Globlization changes[End]

           *N000606,1 NNA (End)

          .cmdPO.ENABLED    = .T.
        ENDWITH
      ENDIF
      *B608873,1 WAM 05/25/2009 Refresh screen in view mode
      =lfAfterInit(loFormSet)
      *B608873,1 WAM 05/25/2009 (End)

    CASE lcModeToChange='E'

      WITH loFormSet.AriaForm1
        *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
        *.cmdGenerate.Caption = "\<"+LANG_MFGENCT_GENERATE+" "+IIF(loFormSet.lcChoice='P',LANG_MFGENCT_PO,LANG_MFGENCT_CT)
        *IF (!loFormSet.llRpBOPFab OR loFormSet.llAllocate) AND (!EMPTY(loFormSet.lcVendor) OR loFormSet.lcChoice <> 'P')
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *.cmdGenerate.Caption = "\<"+LANG_MFGENCT_GENERATE+" "+IIF(INLIST(loFormSet.lcChoice,'P','O'),LANG_MFGENCT_PO,LANG_MFGENCT_CT)
        .cmdGenerate.Caption = "\<"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_GENERATE,loFormSet.GetHeaderText("LANG_MFGENCT_GENERATE",loFormSet.HeaderAlias))+" "+IIF(INLIST(loFormSet.lcChoice,'P','O'),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_PO,loFormSet.GetHeaderText("LANG_MFGENCT_PO",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CT,loFormSet.GetHeaderText("LANG_MFGENCT_CT",loFormSet.HeaderAlias)))
        *N000682,1 11/20/2012 MMT Globlization changes[End]

        IF (!loFormSet.llRpBOPFab OR loFormSet.llAllocate) AND (!EMPTY(loFormSet.lcVendor) OR !INLIST(loFormSet.lcChoice,'P','O'))
        *N000606,1 NNA (END)
          .cmdGenerate.ENABLED = .T.
        ELSE
          .cmdGenerate.ENABLED = .F.
        ENDIF

        .cmdDetLines.ENABLED = .F.
        IF loFormSet.llAllocate
          .cmdselect.ENABLED  = .F.
          .cmdSelAll.ENABLED  = .F.
          .cmdSelNone.ENABLED = .F.
          .cmdInvert.ENABLED  = .F.
          .cmdPODef.ENABLED   = .T.

          oAriaApplication.otoolbar.ChangeButtonStatus('cmdScope','DISABLE')
          .cmdNotes.ENABLED   = .T.
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.cmdNotes.CAPTION   = LANG_MFGENCT_CLEARALO
.cmdNotes.CAPTION   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CLEARALO,loFormSet.GetHeaderText("LANG_MFGENCT_CLEARALO",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

          .cmdPO.ENABLED      = .T.
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.cmdPO.CAPTION      = LANG_MFGENCT_ALORDERS
.cmdPO.CAPTION      = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ALORDERS,loFormSet.GetHeaderText("LANG_MFGENCT_ALORDERS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

        ELSE
          =lfvSelect('',loFormSet)
          .cmdPO.ENABLED      = .F.
          IF loFormSet.llRpBOPFab
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.cmdPO.CAPTION    = LANG_MFGENCT_ALORDERS
.cmdPO.CAPTION    = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ALORDERS,loFormSet.GetHeaderText("LANG_MFGENCT_ALORDERS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

          ELSE
            *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
            *.cmdPO.Caption    = IIF(loFormSet.lcChoice='P',LANG_MFGENCT_POCMD,"\<"+LANG_MFGENCT_CT)
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.cmdPO.Caption    = IIF(loFormSet.lcChoice $ 'PO',LANG_MFGENCT_POCMD,"\<"+LANG_MFGENCT_CT)
.cmdPO.Caption    = IIF(loFormSet.lcChoice $ 'PO',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_POCMD,loFormSet.GetHeaderText("LANG_MFGENCT_POCMD",loFormSet.HeaderAlias)),"\<"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CT,loFormSet.GetHeaderText("LANG_MFGENCT_CT",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000606,1 NNA (End)
          ENDIF
          oAriaApplication.otoolbar.ChangeButtonStatus('cmdScope','ENABLED')
        ENDIF
      ENDWITH

    CASE lcModeToChange='A'
      WITH loFormSet.AriaForm1
        .cmdDetLines.ENABLED = .T.
        oAriaApplication.otoolbar.ChangeButtonStatus('cmdScope','DISABLE')
        .cmdselect.ENABLED   = .F.
        .cmdSelAll.ENABLED   = .F.
        .cmdSelNone.ENABLED  = .F.
        .cmdInvert.ENABLED   = .F.
        .cmdPODef.ENABLED    = .F.

        *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
        *.cmdGenerate.Caption = "\<"+LANG_MFGENCT_CLEARGEN+" "+IIF(loFormSet.lcChoice='P',LANG_MFGENCT_PO,LANG_MFGENCT_CT)
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *.cmdGenerate.Caption = "\<"+LANG_MFGENCT_CLEARGEN+" "+IIF(loFormSet.lcChoice $ 'PO',LANG_MFGENCT_PO,LANG_MFGENCT_CT)
        .cmdGenerate.Caption = "\<"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CLEARGEN,loFormSet.GetHeaderText("LANG_MFGENCT_CLEARGEN",loFormSet.HeaderAlias))+" "+IIF(loFormSet.lcChoice $ 'PO',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_PO,loFormSet.GetHeaderText("LANG_MFGENCT_PO",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CT,loFormSet.GetHeaderText("LANG_MFGENCT_CT",loFormSet.HeaderAlias)))
        *N000682,1 11/20/2012 MMT Globlization changes[End]

        *N000606,1 NNA (End)

        .cmdNotes.CAPTION    = IIF(loFormSet.llRpBOPFab,IIF(loFormSet.llAllocate,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CLEARALO,loFormSet.GetHeaderText("LANG_MFGENCT_CLEARALO",loFormSet.HeaderAlias)),;
          IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ALLOCATE,loFormSet.GetHeaderText("LANG_MFGENCT_ALLOCATE",loFormSet.HeaderAlias))),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_NOTES,loFormSet.GetHeaderText("LANG_MFGENCT_NOTES",loFormSet.HeaderAlias)))

        .cmdNotes.ENABLED    = .F.
        .cmdPO.ENABLED       = .F.
        *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
        *.cmdPO.Caption       = IIF(loFormSet.llRpBOPFab  ,LANG_MFGENCT_ALORDERS,;
                                IIF(loFormSet.lcChoice='P',LANG_MFGENCT_POCMD,"\<"+LANG_MFGENCT_CT))
        .cmdPO.Caption       = IIF(loFormSet.llRpBOPFab  ,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ALORDERS,loFormSet.GetHeaderText("LANG_MFGENCT_ALORDERS",loFormSet.HeaderAlias)),;
                               IIF(loFormSet.lcChoice $ 'PO',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_POCMD,loFormSet.GetHeaderText("LANG_MFGENCT_POCMD",loFormSet.HeaderAlias)),"\<"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CT,loFormSet.GetHeaderText("LANG_MFGENCT_CT",loFormSet.HeaderAlias))))
        *N000606,1 NNA (End)

      ENDWITH
  ENDCASE

  *!*************************************************************
  *! Name      : lfvScope
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 11/29/2004
  *! Purpose   : Function called for the selection criteria (Option Grid)
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfvScope()
  *!*************************************************************
FUNCTION lfvScope
  LPARAMETERS loFormSet
  loFormSet.llRetToSel = .F.

  PRIVATE llClrOrd,lnRngAlias,lcExpr,loParentForm
  llClrOrd   = .F.
  lnRngAlias = 0
  loParentForm = loFormSet

  PRIVATE llRpBOPFab,llRPUnAWip,lcRPSort1,lcRPSort2,lcRPSort3,lcTmpOrder
  llRpBOPFab = loFormSet.llRpBOPFab
  llRPUnAWip = loFormSet.llRPUnAWip
  lcRPSort1  = loFormSet.lcRPSort1
  lcRPSort2  = loFormSet.lcRPSort2
  lcRPSort3  = loFormSet.lcRPSort3
  lcTmpOrder = loFormSet.lcTmpOrder

  lcRpCtStat = loFormSet.lcRpCtStat
  lcRpBaseOn = loFormSet.lcRpBaseOn
  ldRpFrDate = loFormSet.ldRpFrDate

  lnColorStr = loFormSet.lnColorStr
  lnColorLen = loFormSet.lnColorLen
  *C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [BEGIN]
  *PRIVATE llGrpSO
  *llGrpSO =loFormSet.llGrpSO
  *C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [END]

  *608597,1 ALAA [Begin] Commnetd out to rename varibale llGrpSO to be llRPGrpSO
  *PRIVATE llGrpSO
  *llGrpSO =loFormSet.llRPGrpSO
  PRIVATE llRPGrpSO
  llRPGrpSO =loFormSet.llRPGrpSO

  *B608873,1 WAM 05/25/2009 Add a new option to generate one PO per each style major
  llRPGrpByST =loFormSet.llRPGrpByST
  *B608873,1 WAM 05/25/2009 (End)

  *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[Start]
  LLRPGNLPJ =loFormSet.LLRPGNLPJ
  lcGenProj =loFormSet.lcGenProj
  *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[End]

  *608597,1 ALAA [End] Commnetd out to rename varibale llGrpSO to be llRPGrpSO

    *-- Call differrent option grid in case of generate from plan
  *lcExpr = gfOpGrid(IIF(loFormSet.lcChoice='L','MFGENP','MFGENC'),.T.,.F.,.F.,.T.,.T.)
  *loFormSet.lcSelFile = IIF(loFormSet.lcChoice='L',loFormSet.lcStyTmp,loFormSet.lcOrdLine)
  lcExpr = gfOpGrid(IIF(loFormSet.lcChoice='O','POGENPL',IIF(loFormSet.lcChoice='L','MFGENP','MFGENC')),.T.,.F.,.F.,.T.,.T.)
  loFormSet.lcSelFile = IIF(INLIST(loFormSet.lcChoice,'L','O'),loFormSet.lcStyTmp,loFormSet.lcOrdLine)

  loFormSet.llRpBOPFab = llRpBOPFab
  loFormSet.llRPUnAWip = llRPUnAWip
  loFormSet.lcRPSort1  = lcRPSort1
  loFormSet.lcRPSort2  = lcRPSort2
  loFormSet.lcRPSort3  = lcRPSort3
  **C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [BEGIN]
  *loFormSet.llGrpSO = llGrpSO
  *C200804,1 Create un\grouped po lines accoring to llRPGrpSO flag HIA [END]
  *608597,1 ALAA [Begin] Commnetd out to rename varibale llGrpSO to be llRPGrpSO
  loFormSet.llRPGrpSO = llRPGrpSO
  *608597,1 ALAA [END] Commnetd out to rename varibale llGrpSO to be llRPGrpSO

  *B608873,1 WAM 05/25/2009 Add a new option to generate one PO per each style major.
  loFormSet.llRPGrpByST = llRPGrpByST
  *B608873,1 WAM 05/25/2009 (End)

  *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[Start]
  loFormSet.LLRPGNLPJ = LLRPGNLPJ
  *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[End]

  WITH loFormSet.AriaForm1
    .cmdNotes.ENABLED = .F.
    .cmdPO.ENABLED    = .F.
    IF loFormSet.llRpBOPFab
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.cmdNotes.CAPTION = LANG_MFGENCT_ALLOCATE
.cmdNotes.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ALLOCATE,loFormSet.GetHeaderText("LANG_MFGENCT_ALLOCATE",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.cmdPO.CAPTION    = LANG_MFGENCT_ALORDERS
.cmdPO.CAPTION    = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ALORDERS,loFormSet.GetHeaderText("LANG_MFGENCT_ALORDERS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    ELSE
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.cmdNotes.CAPTION = LANG_MFGENCT_NOTES
.cmdNotes.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_NOTES,loFormSet.GetHeaderText("LANG_MFGENCT_NOTES",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
      *.cmdPO.Caption    = IIF(loFormSet.lcChoice='P',LANG_MFGENCT_POCMD,"\<"+LANG_MFGENCT_CT)
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.cmdPO.CAPTION    = IIF(INLIST(loFormSet.lcChoice,'P','O'),LANG_MFGENCT_POCMD,"\<"+LANG_MFGENCT_CT)
.cmdPO.CAPTION    = IIF(INLIST(loFormSet.lcChoice,'P','O'),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_POCMD,loFormSet.GetHeaderText("LANG_MFGENCT_POCMD",loFormSet.HeaderAlias)),"\<"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CT,loFormSet.GetHeaderText("LANG_MFGENCT_CT",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000606,1 NNA (End)
    ENDIF
  ENDWITH
  SELECT (loFormSet.lcSelFile)
  loFormSet.llFirst = .F.
  *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
  *-- Case of generate form plan set the filter on the style file
  *!*	IF loFormSet.lcChoice = 'L'
  *!*	  IF IIF(lcRpBaseOn='T',lfSelForcast(loFormSet),lfSelStyle(loFormSet))
  *!*	    =lfStyWhen(loFormSet)
  *!*	    loFormSet.ActiveMode = 'E'
  *!*	  ENDIF
  *!*	  *-- Case of generate form plan set the filter on the ordline file
  *!*	ELSE
  *!*	  IF lfSelOrd(loFormSet)
  *!*	    =lfOrdWhen(loFormSet)
  *!*	    loFormSet.ActiveMode = 'E'
  *!*	  ENDIF
  *!*	ENDIF
  DO CASE
    CASE loFormSet.lcChoice = 'O'
      =lfSelStyle(loFormSet,loFormSet.lcChoice)
      =lfStyWhen(loFormSet)
      loFormSet.ActiveMode = 'E'
    CASE loFormSet.lcChoice = 'L'
      IF IIF(lcRpBaseOn='T',lfSelForcast(loFormSet),lfSelStyle(loFormSet,loFormSet.lcChoice))
        =lfStyWhen(loFormSet)
        loFormSet.ActiveMode = 'E'
      ENDIF
      *-- Case of generate form Sales ORder set the filter on the ordline file
    OTHERWISE
      IF lfSelOrd(loFormSet)
        =lfOrdWhen(loFormSet)
        loFormSet.ActiveMode = 'E'
      ENDIF
  ENDCASE
  *N000606,1 NNA (End)

  =lfShow(loFormSet,loFormSet.ActiveMode)

  =lfAfterInit(loFormSet)
  loFormSet.RefreshAll

  *!*************************************************************
  *! Name      : lfvStyle
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 11/30/2004
  *! Purpose   : Function called from the validation of the style option grig
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfvStyle()
  *!*************************************************************
FUNCTION lfvStyle

  LOCAL lcObjNam , lcObjVal
  SET ORDER TO TAG STYLE IN STYLE
  lcObjNam = OGSYS18()
  lcObjVal = SUBSTR(EVALUATE(lcObjNam),1,LEN(loParentForm.lcMajorPic))
  IF !EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'STYLE')
    llBrowse = .T.
    lcObjVal = gfStyBrw('M',"","",.F.)
    llBrowse = .F.
  ENDIF
  loOgScroll.&lcObjNam. = lcObjVal

  *!*************************************************************
  *! Name      : lfvStyFab
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 11/30/2004
  *! Purpose   : Function called to validate style primary fabric.
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfvStyFab()
  *!*************************************************************
FUNCTION lfvStyFab

  LOCAL lcObjNam,lcFabric,loFabric
  lcObjNam = OGSYS18()
  lcFabric = EVALUATE(lcObjNam)
  loFabric = CREATEOBJECT('RemoteTable','ITEM','CSTYLE','FABRIC',SET("Datasession"))
  IF !EMPTY(lcFabric) .AND. !loFabric.SEEK("0002"+PADR(lcFabric,19))
    LOCAL lcMajor,lcBrowChr
    lcMajor   = RTRIM(lcFabric)
    lcBrowChr = RIGHT(lcMajor,1)
    lcMajor   = IIF(lcBrowChr=='?',SUBSTR(lcMajor,1,LEN(lcMajor)-1),lcMajor)
    lcFabric  = gfItemBrow("0002",PADR(lcMajor,19),"","*","M","",.T.)
    lcFabric  = PADR(SUBSTR(lcFabric,1,LEN(gfItemMask("PM","","0002"))),19)
  ENDIF
  loOgScroll.&lcObjNam. = lcFabric

  *!*************************************************************
  *! Name      : lfvVendor
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 11/30/2004
  *! Purpose   : Function called from the validation of the vendor option grig
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfvVendor()
  *!*************************************************************
FUNCTION lfvVendor
  PARAMETERS lcParm
  LOCAL lcObjNam , lcObjVal , llObjRet
  lcObjNam = OGSYS18()           && Varible to hold  the name of the memory variable used to create the current GET field
  lcObjVal = EVALUATE(lcObjNam)  && Varible to hold  the value of the current GET field
  SET ORDER TO TAG Vencode IN APVENDOR
  *-- IF The user want to Browse or if the Vendor he entered is not in the file
  IF !EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'APVENDOR')
    llBrowse = .T.
    llObjRet = gfApVnBrow(@lcObjVal,.F.,IIF(loParentForm.lcChoice='P','S','C'))
    lcObjVal = IIF(llObjRet , lcObjVal , SPACE(01))
    llBrowse = .F.
  ENDIF    && End of IF
  loOgScroll.&lcObjNam. = lcObjVal
  *!*************************************************************
  *! Name      : lfvDetail
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/14/2004
  *! Purpose   : to call the screen of the details lines
  *!             of the generated P/O or C/T
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfvDetail()
  *!*************************************************************
FUNCTION lfvDetail
  LPARAMETERS loFormSet

  *-- Set a relation between the lines file and temp. cutpick and ordhdr file
  SELECT STYLE
  loFormSet.loStyle.SETORDER('STYLE')
  SELECT (loFormSet.lcPOLine)

  *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
  *IF loFormSet.lcChoice='P'
  IF INLIST(loFormSet.lcChoice,'P','O')
  *N000606,1 NNA (END)

    *B608873,1 WAM 05/25/2009 Add a new option to generate one PO per each style major.
    IF loFormSet.llRPGrpByST
      lcPOH = loFormSet.lcPOH
      SET KEY TO &lcPOH..PO+&lcPOH..CDIVISION+&lcPOH..CPURCODE+&lcPOH..CSTYGRADE+LEFT(&lcPOH..Style,loFormSet.lnMajorLen)
      =SEEK(&lcPOH..PO+&lcPOH..CDIVISION+&lcPOH..CPURCODE+&lcPOH..CSTYGRADE+LEFT(&lcPOH..Style,loFormSet.lnMajorLen))
    ELSE
    *B608873,1 WAM 05/25/2009 (End)
      SET KEY TO EVALUATE(loFormSet.lcPOH+'.PO+'+      loFormSet.lcPOH+'.CDIVISION+'+;
      loFormSet.lcPOH+'.CPURCODE+'+loFormSet.lcPOH+'.CSTYGRADE')
      =SEEK(EVALUATE(loFormSet.lcPOH+'.PO+'+      loFormSet.lcPOH+'.CDIVISION+'+;
      loFormSet.lcPOH+'.CPURCODE+'+loFormSet.lcPOH+'.CSTYGRADE'))
      *B608873,1 WAM 05/25/2009 Add a new option to generate one PO per each style major.
    ENDIF
    *B608873,1 WAM 05/25/2009 (End)
  ELSE
    SET KEY TO EVALUATE(loFormSet.lcPOH+'.PO+'+           loFormSet.lcPOH+'.CDIVISION+'+;
      loFormSet.lcPOH+'.SEASON+'+loFormSet.lcPOH+'.CWEEK+'+loFormSet.lcPOH+'.CYEAR+SUBSTR('+loFormSet.lcPOH+'.STYLE,1,'+STR(loFormSet.lnMajorLen)+')')
    =SEEK(EVALUATE(loFormSet.lcPOH+'.PO+'+                loFormSet.lcPOH+'.CDIVISION+'+;
      loFormSet.lcPOH+'.SEASON+'+loFormSet.lcPOH+'.CWEEK+'+loFormSet.lcPOH+'.CYEAR+SUBSTR('+loFormSet.lcPOH+'.STYLE,1,'+STR(loFormSet.lnMajorLen)+')'))
  ENDIF

  SELECT (loFormSet.lcLinFile)
  GOTO TOP
  PRIVATE loParentForm
  loParentForm = loFormSet

  *C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [BEGIN]

  *N000606,1 NNA 10/06/2007 (Begin) set order in loParentForm.lcCutPick only if not OTS option
  *IF !loFormSet.llGrpSO
  *IF !loFormSet.llGrpSO AND loFormSet.lcChoice <>'O'
  *N000606,1 NNA (End)

    **SET ORDER TO CSTYLIN IN (loParentForm.lcCutPick)
   *ENDIF
  *C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [END]

  *608597,1 ALAA [Begin] Commnetd out to rename varibale llGrpSO to be llRPGrpSO
  *N000613,1 ALA 06/30/2008 (Begin) set order in loParentForm.lcCutPick only if not OTS option
  *IF !loFormSet.llGrpSO

  *B608675,1 WAM 09/02/2008 Fix bug while generate CT from SO results in create CT without lines
  *IF !loFormSet.llRPGrpSO AND loFormSet.lcChoice <>'O'
  IF !loFormSet.llRPGrpSO AND !INLIST(loFormSet.lcChoice,'O','L')
  *B608675,1 WAM 09/02/2008 (End)

  *N000613,1 ALA(End)
  *608597,1 ALAA [END] Commnetd out to rename varibale llGrpSO to be llRPGrpSO

    SET ORDER TO CSTYLIN IN (loParentForm.lcCutPick)
  ENDIF
  *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
  *DO FORM (oAriaApplication.ScreenHome+"MFGENCT2.SCX")
  =gfCallForm('MFGENCT2')
  *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
  *C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [BEGIN]
  *608597,1 ALAA [Begin] Commnetd out to rename varibale llGrpSO to be llRPGrpSO
  *N000606,1 NNA 10/06/2007 (Begin) set order in loParentForm.lcCutPick only if not OTS option
  *IF !loFormSet.llGrpSO

  *B608675,1 WAM 09/02/2008 Fix bug while generate CT from SO results in create CT without lines
  *IF !loFormSet.llRPGrpSO AND loFormSet.lcChoice <>'O'
  IF !loFormSet.llRPGrpSO AND !INLIST(loFormSet.lcChoice,'O','L')
  *B608675,1 WAM 09/02/2008 (End)

  *N000606,1 NNA (End)

  *N000613,1 ALA (End)
  *608597,1 ALAA [END] Commnetd out to rename varibale llGrpSO to be llRPGrpSO

    SET ORDER TO CSTYCLR IN (loParentForm.lcCutPick)
  ENDIF
  *C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [END]

  SELECT (loFormSet.lcPOLine)
  SET KEY TO
  SELECT (loFormSet.lcLinFile)

  *!*************************************************************
  *! Name      : lfvCust
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 11/30/2004
  *! Purpose   : Called from the validation of the cutomer in the selection grid
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfvCust()
  *!*************************************************************
FUNCTION lfvCust
LOCAL lcObjNam , lcObjVal

lcObjNam = OGSYS18()           && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(lcObjNam)  && Varible to hold  the value of the current GET field
SET ORDER TO TAG CUSTOMER IN CUSTOMER
IF !EMPTY(lcObjVal) .AND. !SEEK('M'+lcObjVal , 'CUSTOMER')
  llBrowse = .T.
  xAccount = lcObjVal
  DO CUSBROWM WITH xAccount
  lcObjVal = xAccount
  llBrowse = .F.
ENDIF    && End of IF
loOgScroll.&lcObjNam. = lcObjVal

  *!*************************************************************
  *! Name      : lfSelOrd
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/05/2004
  *! Purpose   : Called from scope function to set the filter on the ordline file
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfSelOrd()
  *!*************************************************************
FUNCTION lfSelOrd
  LPARAMETERS loFormSet

  LOCAL lcStyOrd ,lnAlias
  *-- If the user return form the selection grid with cancel
  IF lcExpr = '.F.'
    RETURN(.F.)
  ENDIF
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *WAIT LANG_MFGENCT_SELORDLINE WINDOW NOWAIT
  WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_SELORDLINE,loFormSet.GetHeaderText("LANG_MFGENCT_SELORDLINE",loFormSet.HeaderAlias)) WINDOW NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  LOCAL ARRAY laSort[6]
  laSort[1] = 'DTOS(OrdHdr.Entered)'
  laSort[2] = 'DTOS(OrdLine.Start)'
  laSort[3] = 'DTOS(OrdLine.Complete)'
  laSort[4] = 'OrdLine.Order'
  laSort[5] = 'OrdHdr.Priority'
  laSort[6] = 'OrdLine.Account'
  loFormSet.lcSort = "''"
  FOR lnSort =1 TO 3
    loFormSet.lcSort = loFormSet.lcSort + '+' + laSort[AT(EVAL('loFormSet.lcRPSORT'+STR(lnSort,1)),'ESCOPA')]
  ENDFOR

  SELECT STYLE
  lcStyOrd = TAG()
  SET ORDER TO TAG STYLE

  SELECT (loFormSet.lcOrdLine)
  ZAP

  *-- Set the required relations
  SELECT ORDLINE

  *!*  *C200361,1 AMH trigger for custom RON20 to ignore picked ordline's [Start]
  *!*  IF ASCAN(laEvntTrig , PADR('IGPICK',10)) <> 0
  *!*    =gfDoTriger('POGENPO',PADR('IGPICK',10))
  *!*  ENDIF
  *!*  *C200361,1 AMH [End]

  LOCAL lcScanExp
  lcScanExp = ""

  IF loFormSet.llOrdRange
    SELECT (loFormSet.lcOrdAlias)
  ELSE
    SELECT (loFormSet.lcTmpOrder)
  ENDIF
  lcScanExp = lcExpr + ' AND TOTQTY-TOTCUT > 0'

  SCAN
    lcOrder = ORDER
    loFormSet.loOrdLine.SEEK('O'+lcOrder)
    SELECT ORDLINE
    SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+lcOrder
      loFormSet.loOrdHdr.SEEK(ORDLINE.CORDTYPE+ORDLINE.ORDER)
      loFormSet.loStyle.SEEK(ORDLINE.STYLE)
      loFormSet.loCodes.SEEK('N'+'COLOR     '+SUBSTR(ORDLINE.STYLE,loFormSet.lnColorStr,loFormSet.lnColorLen))

      IF !EVALUATE(lcScanExp)
        LOOP
      ENDIF

      SCATTER MEMVAR
      m.cDivision = ORDHDR.cDivision
      m.cSortExp  = EVAL(loFormSet.lcSort)
      m.cStyGrade = STYLE.cStyGrade
      m.cPurCode  = STYLE.cPurCode
      loFormSet.loCustomer.SEEK('M'+m.ACCOUNT)
      m.BtName    = CUSTOMER.BtName
      m.nOpn1     = m.Qty1 - m.Cut1
      m.nOpn2     = m.Qty2 - m.Cut2
      m.nOpn3     = m.Qty3 - m.Cut3
      m.nOpn4     = m.Qty4 - m.Cut4
      m.nOpn5     = m.Qty5 - m.Cut5
      m.nOpn6     = m.Qty6 - m.Cut6
      m.nOpn7     = m.Qty7 - m.Cut7
      m.nOpn8     = m.Qty8 - m.Cut8
      m.nTotOpn   = m.TotQty - m.TotCut
      IF !SEEK(m.cSortExp+ORDLINE.ORDER+STR(ORDLINE.LINENO,6),loFormSet.lcOrdLine)
        INSERT INTO (loFormSet.lcOrdLine) FROM MEMVAR
      ENDIF
    ENDSCAN
  ENDSCAN

  SELECT STYLE
  SET ORDER TO TAG &lcStyOrd.

  SELECT(loFormSet.lcOrdLine)
  GOTO TOP
  IF EOF()
    =lfvSelect('',loFormSet)
    loFormSet.AriaForm1.cmdselect.ENABLED = .F.
    loFormSet.ActiveMode = 'S'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *=gfModalGen('INM38129B38018','DIALOG',LOWER(LANG_MFGENCT_ORDER))
     =gfModalGen('INM38129B38018','DIALOG',LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ORDER,loFormSet.GetHeaderText("LANG_MFGENCT_ORDER",loFormSet.HeaderAlias))))
     *N000682,1 11/20/2012 MMT Globlization changes[End]

    RETURN(.F.)
  ENDIF

  *!*************************************************************
  *! Name      : lfvSelect
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/05/2004
  *! Purpose   : Called from the selection buttons
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfvSelect()
  *!*************************************************************
FUNCTION lfvSelect
  LPARAMETERS lcSelect,loFormSet

  LOCAL lnAlias,lnRecNo,lcOrdTag
  lnAlias = SELECT(0)
  SELECT (loFormSet.lcSelFile)
  lnRecNo = RECNO()

  DO CASE
    CASE lcSelect = 'S'
      REPLACE lSelect WITH !lSelect,;
        cSelect WITH IIF(!lSelect,'',"�")
    CASE lcSelect = 'A'
      REPLACE ALL lSelect WITH .T.,;
        cSelect WITH "�"
    CASE lcSelect = 'N'
      REPLACE ALL lSelect WITH .F.,;
        cSelect WITH " "
    CASE lcSelect = 'V'
      REPLACE ALL lSelect WITH !lSelect,;
        cSelect WITH IIF(!lSelect,'',"�")
    OTHERWISE
  ENDCASE

  lcOrdTag = TAG()
  SET ORDER TO TAG 'TICKET'

  WITH loFormSet.AriaForm1
    IF SEEK("�")

      *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
      *IF loFormSet.lcChoice = 'L'
        *SUM REST PLAN1,PLAN2,PLAN3,PLAN4,PLAN5,PLAN6,PLAN7,PLAN8;
          TO   lnTotal1,lnTotal2,lnTotal3,lnTotal4,lnTotal5,lnTotal6,lnTotal7,lnTotal8 ;
          WHILE cSelect = "�"
      IF INLIST(loFormSet.lcChoice,'L','O')
        SUM REST PLAN1,PLAN2,PLAN3,PLAN4,PLAN5,PLAN6,PLAN7,PLAN8;
          TO   loFormSet.lnTotal1,loFormSet.lnTotal2,loFormSet.lnTotal3,loFormSet.lnTotal4,;
               loFormSet.lnTotal5,loFormSet.lnTotal6,loFormSet.lnTotal7,loFormSet.lnTotal8 ;
          WHILE cSelect = "�"
      *N000606,1 NNA (End)

      ELSE
        SUM REST (Qty1-Cut1),(Qty2-Cut2),(Qty3-Cut3),(Qty4-Cut4),;
          (Qty5-Cut5),(Qty6-Cut6),(Qty7-Cut7),(Qty8-Cut8) ;
          TO   loFormSet.lnTotal1,loFormSet.lnTotal2,loFormSet.lnTotal3,loFormSet.lnTotal4,;
          loFormSet.lnTotal5,loFormSet.lnTotal6,loFormSet.lnTotal7,loFormSet.lnTotal8 ;
          WHILE cSelect = "�"
      ENDIF
      .cmdSelNone.ENABLED = .T.
      .cmdPODef.ENABLED   = .T.

      *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
      *.cmdGenerate.Caption = "\<"+LANG_MFGENCT_GENERATE+" "+IIF(loFormSet.lcChoice='P',LANG_MFGENCT_PO,LANG_MFGENCT_CT)
      *IF (!loFormSet.llRpBOPFab OR loFormSet.llAllocate) AND (!EMPTY(loFormSet.lcVendor) OR loFormSet.lcChoice <> 'P')
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.cmdGenerate.Caption = "\<"+LANG_MFGENCT_GENERATE+" "+IIF(loFormSet.lcChoice $ 'PO',LANG_MFGENCT_PO,LANG_MFGENCT_CT)
.cmdGenerate.Caption = "\<"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_GENERATE,loFormSet.GetHeaderText("LANG_MFGENCT_GENERATE",loFormSet.HeaderAlias))+" "+IIF(loFormSet.lcChoice $ 'PO',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_PO,loFormSet.GetHeaderText("LANG_MFGENCT_PO",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CT,loFormSet.GetHeaderText("LANG_MFGENCT_CT",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      IF (!loFormSet.llRpBOPFab OR loFormSet.llAllocate) AND (!EMPTY(loFormSet.lcVendor) OR !INLIST(loFormSet.lcChoice,'P','O'))
      *N000606,1 NNA (End)

        .cmdGenerate.ENABLED = .T.
      ELSE
        .cmdGenerate.ENABLED = .F.
      ENDIF
      .cmdNotes.CAPTION = IIF(loFormSet.llRpBOPFab,IIF(loFormSet.llAllocate,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CLEARALO,loFormSet.GetHeaderText("LANG_MFGENCT_CLEARALO",loFormSet.HeaderAlias)),;
                          IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ALLOCATE,loFormSet.GetHeaderText("LANG_MFGENCT_ALLOCATE",loFormSet.HeaderAlias))),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_NOTES,loFormSet.GetHeaderText("LANG_MFGENCT_NOTES",loFormSet.HeaderAlias)))
      IF loFormSet.llRpBOPFab AND loFormSet.ActiveMode="E"
        .cmdNotes.ENABLED = .T.
      ELSE
        .cmdNotes.ENABLED = .F.
      ENDIF
    ELSE
      STORE 0 TO loFormSet.lnTotal1,loFormSet.lnTotal2,loFormSet.lnTotal3,loFormSet.lnTotal4,;
        loFormSet.lnTotal5,loFormSet.lnTotal6,loFormSet.lnTotal7,loFormSet.lnTotal8
      .cmdSelNone.ENABLED  = .F.
      .cmdGenerate.ENABLED = .F.
      .cmdNotes.ENABLED    = .F.
      .cmdPODef.ENABLED    = .F.
    ENDIF
    IF SEEK(" ")
      .cmdSelAll.ENABLED = .T.
    ELSE
      .cmdSelAll.ENABLED = .F.
    ENDIF
    IF SEEK("�") OR SEEK(" ")
      .cmdInvert.ENABLED = .T.
    ELSE
      .cmdInvert.ENABLED = .F.
    ENDIF
  ENDWITH

  SET ORDER TO TAG &lcOrdTag.
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GOTO lnRecNo
  ENDIF
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.AriaForm1.cmdselect.CAPTION = IIF(lSelect,LANG_MFGENCT_UNSELECT,LANG_MFGENCT_SELECT)
loFormSet.AriaForm1.cmdselect.CAPTION = IIF(lSelect,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_UNSELECT,loFormSet.GetHeaderText("LANG_MFGENCT_UNSELECT",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_SELECT,loFormSet.GetHeaderText("LANG_MFGENCT_SELECT",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *-- refresh the selection browse
  SELECT (lnAlias)

  *!*************************************************************
  *! Name      : lfvGenerate
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/08/2004
  *! Purpose   : Called from the Genetrate or Clear button
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfvGenerate()
  *!*************************************************************
FUNCTION lfvGenerate
  LPARAMETERS loFormSet
  LOCAL lnAlias
  loFormSet.llRetToSel = .F.
  loFormSet.loWAREHOUS = IIF(TYPE('loFormSet.loWAREHOUS')='O',loFormSet.loWAREHOUS,CREATEOBJECT('RemoteTable','WAREHOUS','WAREHOUS','WAREHOUS',loFormSet.DATASESSIONID))
  =lfCrtDTemp(loFormSet)
  lnAlias = SELECT(0)
  GOTO TOP IN (loFormSet.lcHdrFile)
  PRIVATE llStartGen
  llStartGen = .F.

  IF EOF(loFormSet.lcHdrFile)
    *-- Generate mode
    IF loFormSet.lcChoice = 'L'
      llStartGen = .T.
      =lfUpdCtPlan(loFormSet)
    ELSE
      PRIVATE loParentForm
      loParentForm = loFormSet
      *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
      *DO FORM (oAriaApplication.ScreenHome+"MFGENCT3.SCX")
      =gfCallForm('MFGENCT3')
      *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
    ENDIF
  ELSE
    *-- Clear generated mode
    =lfDelFiles(.F.,loFormSet)
  ENDIF
  GOTO TOP IN (loFormSet.lcSelFile)

  *B608873,1 WAM 05/25/2009 Refresh PO grid
  loFormSet.Ariaform1.grdPOLines.Refresh
  *B608873,1 WAM 05/25/2009 (End)

  *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
  *=IIF(loFormSet.lcChoice='L',lfStyWhen(loFormSet),lfOrdWhen(loFormSet))
  =IIF(loFormSet.lcChoice $ 'LO',lfStyWhen(loFormSet),lfOrdWhen(loFormSet))
  *N000606,1 NNA (End)

  IF llStartGen
    loFormSet.ActiveMode = "A"
  ELSE
    loFormSet.ActiveMode = "E"
  ENDIF
  SELECT(lnAlias)

  loFormSet.ChangeMode(loFormSet.ActiveMode)

  *!*************************************************************
  *! Name      : lfPODef
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/07/2004
  *! Purpose   : Called from the P/O defaults button
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfPODef()
  *!*************************************************************
FUNCTION lfPODef
  LPARAMETERS loFormSet

  loFormSet.loSycCurr  = IIF(TYPE('loFormSet.loSycCurr')='O',loFormSet.loSycCurr,CREATEOBJECT('RemoteTable','SYCCURR' ,'CCURRCODE','SYCCURR' ,loFormSet.DATASESSIONID))
  loFormSet.loApVendor = IIF(TYPE('loFormSet.loAPVENDOR')='O',loFormSet.loApVendor,CREATEOBJECT('RemoteTable','APVENDOR','Vencode'  ,'APVENDOR',loFormSet.DATASESSIONID))

  *-- Call the P/O defaults screen
  PRIVATE loParentForm
  loParentForm = loFormSet
  *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
  *DO FORM (oAriaApplication.ScreenHome+"MFGENCT4.SCX")
  =gfCallForm('MFGENCT4')
  *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
  IF !EMPTY(loFormSet.lcVendor) AND (!loFormSet.llRpBOPFab OR loFormSet.llAllocate)
    loFormSet.AriaForm1.cmdGenerate.ENABLED = .T.
  ELSE
    loFormSet.AriaForm1.cmdGenerate.ENABLED = .F.
  ENDIF

  *!*************************************************************
  *! Name      : lfvDefVen
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/08/2004
  *! Purpose   : Validation of the vendor at the PO defaults screen
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfvDefVen()
  *!*************************************************************
FUNCTION lfvDefVen
  LPARAMETERS llBrowse,loFormSet

  LOCAL lnAlias
  lnAlias = SELECT(0)

  *-- If the enterd vendor does not ecxit in the apvendor file
  IF llBrowse OR (!EMPTY(loFormSet.lcVendor) AND !loParentForm.loApVendor.SEEK(loFormSet.lcVendor)) OR ;
      !('S' $ APVENDOR.cVenSupTyp .OR. 'C' $ APVENDOR.cVenSupTyp)
    LOCAL lcVendor
    lcVendor = loFormSet.lcVendor
    =gfApVnBrow(@lcVendor,.F.,'CS')
    loFormSet.lcVendor = lcVendor
  ENDIF

  LOCAL llRet
  llRet = .T.
  IF !EMPTY(loFormSet.lcVendor)
    loFormSet.lcVenName = APVENDOR.cVenComp
    IF loParentForm.loCodes.SEEK('N'+'CTERMCODE '+APVENDOR.CTERMCODE)
      loFormSet.lcTerm = APVENDOR.CTERMCODE
    ELSE
      loFormSet.lcTerm = loFormSet.AriaForm1.cboTerms.codedefaultvalue
    ENDIF

    *-- if the system id set to use multi currency
    IF loParentForm.llMulCurr
      loFormSet.lcPCurr = APVENDOR.ccurrcode
      loFormSet.lcDCurr = oAriaApplication.BaseCurrency
      *! B610089,1 MMT 09/18/2012 Generate PO from SO saves incorrect currency unit in POSHDR{Start}
      *LOCAL lnUnit1,lnUnit2
      *! B610089,1 MMT 09/18/2012 Generate PO from SO saves incorrect currency unit in POSHDR{END}
      lnUnit1 = loFormSet.lnUnit1
      lnUnit2 = loFormSet.lnUnit2
      loFormSet.lnPRate = gfchkrate('lnUnit1',loFormSet.lcPCurr ,oAriaApplication.systemdate,loParentForm.llEditExRt,oAriaApplication.ActiveCompanyID,.F.)
      loFormSet.lnDRate = gfchkrate('lnUnit2',loFormSet.lcDCurr ,oAriaApplication.systemdate,loParentForm.llEditExRt,oAriaApplication.ActiveCompanyID,.F.)
      loFormSet.lnUnit1 = lnUnit1
      loFormSet.lnUnit2 = lnUnit2

      *-- if there is no valid rate for the entered currency,give a message
      *-- (No valid rate .default to the base currency)
      IF loFormSet.lnPRate <= 0
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *IF loParentForm.llEditExRt OR gfModalGen('INM36004B36001','DIALOG',LANG_MFGENCT_PRICE) = 1
        IF loParentForm.llEditExRt OR gfModalGen('INM36004B36001','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_PRICE,loFormSet.GetHeaderText("LANG_MFGENCT_PRICE",loFormSet.HeaderAlias))) = 1
        *N000682,1 11/20/2012 MMT Globlization changes[End]

          loFormSet.lcPCurr = oAriaApplication.BaseCurrency
          loFormSet.lnPRate = 1
          loFormSet.lnUnit1 = 1
        ELSE
          llRet = 3
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  *-- Check if the user has the right to edit the rate
  IF loFormSet.lcPCurr = oAriaApplication.BaseCurrency OR !loParentForm.llEditExRt
    loFormSet.AriaForm1.txtPriceRate.ENABLED = .F.
  ELSE
    loFormSet.AriaForm1.txtPriceRate.ENABLED = .T.
  ENDIF
  IF loFormSet.lcDCurr = oAriaApplication.BaseCurrency OR !loParentForm.llEditExRt
    loFormSet.AriaForm1.txtDutyRate.ENABLED = .F.
  ELSE
    loFormSet.AriaForm1.txtDutyRate.ENABLED = .T.
  ENDIF
  loFormSet.REFRESH
  SELECT (lnAlias)
  RETURN llRet

  *!*************************************************************
  *! Name      : lfvDefWare
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/09/2004
  *! Purpose   : Validation of the warehouse
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfvDefWare()
  *!*************************************************************
FUNCTION lfvDefWare
  LPARAMETERS llBrowse

  IF llBrowse OR !loParentForm.loWAREHOUS.SEEK(loParentForm.lcWareCode)
    loParentForm.lcWareCode = gfbrowware(.T.,.F.,.F.,.F.,.F.,'S')
  ENDIF

  RETURN !EMPTY(loParentForm.lcWareCode)

  *!*************************************************************
  *! Name      : lfvDefPCurr
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/08/2004
  *! Purpose   : Validation of the default price currency
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfvDefPCurr()
  *!*************************************************************
FUNCTION lfvDefPCurr
  LPARAMETERS llBrowse,loFormSet

  *-- if the entered currency does not exist in the currency code file
  IF llBrowse OR !loParentForm.loSycCurr.SEEK(loFormSet.lcPCurr)
    LOCAL lcPCurr
    lcPCurr = loFormSet.lcPCurr
    IF !gfcurrbrow(@lcPCurr)
      lcPCurr = oAriaApplication.BaseCurrency
    ENDIF
    loFormSet.lcPCurr = lcPCurr
  ENDIF

  *-- get the final exchange rate of the price currency
  *! B610089,1 MMT 09/18/2012 Generate PO from SO saves incorrect currency unit in POSHDR{Start}
  *LOCAL lnUnit1
  *! B610089,1 MMT 09/18/2012 Generate PO from SO saves incorrect currency unit in POSHDR{END}
  lnUnit1 = loFormSet.lnUnit1
  loFormSet.lnPRate = gfchkrate('lnUnit1',loFormSet.lcPCurr ,oAriaApplication.systemdate,loParentForm.llEditExRt,oAriaApplication.ActiveCompanyID,.F.)
  loFormSet.lnUnit1 = lnUnit1

  *-- if there is no valid rate
  LOCAL llRet
  llRet = .T.
  IF loFormSet.lnPRate <= 0
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *IF loParentForm.llEditExRt OR gfModalGen('INM36004B36001','DIALOG',LANG_MFGENCT_PRICE) = 1
    IF loParentForm.llEditExRt OR gfModalGen('INM36004B36001','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_PRICE,loFormSet.GetHeaderText("LANG_MFGENCT_PRICE",loFormSet.HeaderAlias))) = 1
    *N000682,1 11/20/2012 MMT Globlization changes[End]

      loFormSet.lcPCurr = oAriaApplication.BaseCurrency
      loFormSet.lnPRate = 1
      loFormSet.lnUnit1 = 1
    ELSE
      llRet = 0
    ENDIF
  ENDIF

  IF loFormSet.lcPCurr = oAriaApplication.BaseCurrency OR !loParentForm.llEditExRt
    loFormSet.AriaForm1.txtPriceRate.ENABLED = .F.
  ELSE
    loFormSet.AriaForm1.txtPriceRate.ENABLED = .T.
  ENDIF
  loFormSet.REFRESH
  RETURN llRet

  *!*************************************************************
  *! Name      : lfvDefDCurr
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/08/2004
  *! Purpose   : Validation of the default duty currency
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfvDefDCurr()
  *!*************************************************************
FUNCTION lfvDefDCurr
  LPARAMETERS llBrowse,loFormSet

  IF llBrowse OR !loParentForm.loSycCurr.SEEK(loFormSet.lcDCurr)
    LOCAL lcDCurr
    lcDCurr = loFormSet.lcDCurr
    IF !gfcurrbrow(@lcDCurr)
      lcDCurr = oAriaApplication.BaseCurrency
    ENDIF
    loFormSet.lcDCurr = lcDCurr
  ENDIF
  *! B610089,1 MMT 09/18/2012 Generate PO from SO saves incorrect currency unit in POSHDR{Start}
  *LOCAL lnUnit2
  *! B610089,1 MMT 09/18/2012 Generate PO from SO saves incorrect currency unit in POSHDR{END}
  lnUnit2 = loFormSet.lnUnit2
  loFormSet.lnDRate = gfchkrate('lnUnit2',loFormSet.lcDCurr ,oAriaApplication.systemdate,loParentForm.llEditExRt,oAriaApplication.ActiveCompanyID,.F.)
  loFormSet.lnUnit2 = lnUnit2
  loFormSet.lcSign2 = gfGetExSin("",ALLTRIM(loFormSet.lcDCurr))

  *-- if there is no valid rate
  LOCAL llRet
  llRet = .T.
  IF loFormSet.lnDRate <= 0
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *IF loParentForm.llEditExRt OR gfModalGen('INM36004B36001','DIALOG',LANG_MFGENCT_DUTY) = 1
    IF loParentForm.llEditExRt OR gfModalGen('INM36004B36001','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_DUTY,loFormSet.GetHeaderText("LANG_MFGENCT_DUTY",loFormSet.HeaderAlias))) = 1
    *N000682,1 11/20/2012 MMT Globlization changes[End]

      loFormSet.lcDCurr = oAriaApplication.BaseCurrency
      loFormSet.lnDRate = 1
      loFormSet.lnUnit2 = 1
    ELSE
      llRet = 0
    ENDIF
  ENDIF
  IF loFormSet.lcDCurr = oAriaApplication.BaseCurrency OR !loParentForm.llEditExRt
    loFormSet.AriaForm1.txtDutyRate.ENABLED = .F.
  ELSE
    loFormSet.AriaForm1.txtDutyRate.ENABLED = .T.
  ENDIF
  loFormSet.REFRESH
  RETURN llRet

  *!*************************************************************
  *! Name      : lfvOK
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/08/2004
  *! Purpose   : Validation of <OK> of the default PO screen
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfvOK()
  *!*************************************************************
FUNCTION lfvOK
  LPARAMETERS loFormSet

  *-- if the vendor is left empty, give the user a message
  *-- (vendor field cannot be left empty)
  IF EMPTY(loFormSet.lcVendor)
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *=gfModalGen('INM36002B36000','DIALOG',LANG_MFGENCT_VENDOR)
    =gfModalGen('INM36002B36000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_VENDOR,loFormSet.GetHeaderText("LANG_MFGENCT_VENDOR",loFormSet.HeaderAlias)))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    RETURN .F.
  ENDIF
  *B608873,1 WAM 05/25/2009 Read the complete and availabe dates in the PO defaults
  IF EMPTY(loFormSet.ariaform1.dtPickerComplete.value)
    *-You cannot leave the Completion date empty.
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *=gfModalGen('INM34005B34000','DIALOG','Completion')    
    =gfModalGen('INM34005B34000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_COMPLETION,loFormSet.GetHeaderText("LANG_MFGENCT_COMPLETION",loFormSet.HeaderAlias)))
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
    RETURN .F.
  ELSE
    IF oAriaApplication.systemdate > loFormSet.ariaform1.dtPickerComplete.value
      *-Completion date cannot be prior to entered date.
      *N000682,1 MMT 12/09/2012 Globalization changes[Start]
      *=gfModalGen('INM34003B34000','DIALOG','Completion')      
      =gfModalGen('INM34003B34000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_COMPLETION,loFormSet.GetHeaderText("LANG_MFGENCT_COMPLETION",loFormSet.HeaderAlias)))
      *N000682,1 MMT 12/09/2012 Globalization changes[END]
      RETURN .F.
    ENDIF
  ENDIF

  IF EMPTY(loFormSet.ariaform1.dtPickerAvailable.value )
    *-You cannot leave the Available date empty.
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *=gfModalGen('INM34005B34000','DIALOG','Available')    
    =gfModalGen('INM34005B34000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_AVLBLE,loFormSet.GetHeaderText("LANG_MFGENCT_AVLBLE",loFormSet.HeaderAlias)))
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
    RETURN .F.
  ELSE
    IF oAriaApplication.systemdate  > loFormSet.ariaform1.dtPickerAvailable.value
      *-Available date cannot be prior to entered date.
      *N000682,1 MMT 12/09/2012 Globalization changes[Start]
      *=gfModalGen('INM34003B34000','DIALOG','Available')      
      =gfModalGen('INM34003B34000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_AVLBLE,loFormSet.GetHeaderText("LANG_MFGENCT_AVLBLE",loFormSet.HeaderAlias)))
      *N000682,1 MMT 12/09/2012 Globalization changes[END]
      RETURN .F.
    ELSE
      IF loFormSet.ariaform1.dtPickerComplete.value  > loFormSet.ariaform1.dtPickerAvailable.value
        *N000682,1 MMT 12/09/2012 Globalization changes[Start]
        *=gfModalGen('QRM34211B00000','ALERT', 'Available date')        
        =gfModalGen('QRM34211B00000','ALERT', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_AVLDATE,loFormSet.GetHeaderText("LANG_MFGENCT_AVLDATE",loFormSet.HeaderAlias)))
        *N000682,1 MMT 12/09/2012 Globalization changes[END]
        RETURN .F.
      ENDIF
    ENDIF
  ENDIF
  *B608873,1 WAM 05/25/2009 (End)

  =loParentForm.loApVendor.SEEK(loFormSet.lcVendor)
  loParentForm.lcPhone   = APVENDOR.cphoneno
  loParentForm.lcCont    = APVENDOR.cVenCont
  loParentForm.lcVendor  = loFormSet.lcVendor
  loParentForm.lcVenName = loFormSet.lcVenName
  loParentForm.lcShip    = loFormSet.lcShip
  loParentForm.lcTerm    = loFormSet.lcTerm
  loParentForm.lcPCurr   = loFormSet.lcPCurr
  loParentForm.lnPRate   = loFormSet.lnPRate
  loParentForm.lcDCurr   = loFormSet.lcDCurr
  loParentForm.lnDRate   = loFormSet.lnDRate
  loParentForm.lnUnit1   = loFormSet.lnUnit1
  loParentForm.lnUnit2   = loFormSet.lnUnit2
  loParentForm.lcSign1   = loFormSet.lcSign1
  loParentForm.lcSign2   = loFormSet.lcSign2

  *B608873,1 WAM 05/25/2009 Read the complete and availabe dates in the PO defaults
  loParentForm.ldComplete  = loFormSet.ariaform1.dtPickerComplete.value
  loParentForm.ldAvailable = loFormSet.ariaform1.dtPickerAvailable.value
  *B608873,1 WAM 05/25/2009 (End)

  RETURN .T.

  *!*************************************************************
  *! Name      : lfShowDef
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/08/2004
  *! Purpose   : Show function for the objects of the PO default screen
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfShowDef()
  *!*************************************************************
FUNCTION lfShowDef
  LPARAMETERS loFormSet

  IF loFormSet.lcPCurr = oAriaApplication.BaseCurrency OR !loParentForm.llEditExRt
    loFormSet.AriaForm1.txtPriceRate.ENABLED = .F.
  ELSE
    loFormSet.AriaForm1.txtPriceRate.ENABLED = .T.
  ENDIF
  IF loFormSet.lcDCurr = oAriaApplication.BaseCurrency OR !loParentForm.llEditExRt
    loFormSet.AriaForm1.txtDutyRate.ENABLED = .F.
  ELSE
    loFormSet.AriaForm1.txtDutyRate.ENABLED = .T.
  ENDIF

  *!*************************************************************
  *! Name      : lfvGenCtPO
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/09/2004
  *! Purpose   : Function to check if the generation can bedone or not
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfvGenCtPO()
  *!*************************************************************
FUNCTION lfvGenCtPO
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *LPARAMETERS loFormSet
  PARAMETERS loFormSet
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  *-- If the location is left empty give the user a message
  *-- (You have to enter a warehouse code)
  IF loParentForm.llWareHous AND EMPTY(loParentForm.lcWareCode)
    =gfModalGen('INM38138B38018','DIALOG')
    RETURN
  ENDIF
  *-- Check if the if any generation ratios entered will
  *-- generate positive quantities or not
  FOR lnCount = 1 TO 8
    lcElemNo = ALLTRIM(STR(lnCount))
    IF loParentForm.laPercet[lnCount]>0 .AND. ;
        INT((loParentForm.laPercet[lnCount] * EVALUATE('loParentForm.lnTotal' + lcElemNo) /100)) > 0
      llStartGen = .T.
      EXIT
    ENDIF
  ENDFOR

  IF EMPTY(loParentForm.lcWareCode)
    GO TOP IN 'WAREHOUS'
    loParentForm.lcWareCode = WAREHOUS.cWareCode
  ENDIF

  IF llStartGen
    loFormSet.RELEASE

    *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
    *lcFile = IIF(loParentForm.llRpBOPFab,loParentForm.lcTmpOrd,loParentForm.lcOrdLine)
    *=IIF(loParentForm.lcChoice='P',lfUpdPOPik(lcFile),lfUpdCtPik(lcFile))
    lcFile = IIF(loParentForm.llRpBOPFab,loParentForm.lcTmpOrd,IIF(loParentForm.lcChoice ='O',loParentForm.lcStyTmp,loParentForm.lcOrdLine))
    =IIF(loParentForm.lcChoice ='P',lfUpdPOPik(lcFile),IIF(loParentForm.lcChoice ='O',lfUpdPOOTS(lcFile),lfUpdCtPik(lcFile)))
    *N000606,1 NNA (End)

    SELECT (loParentForm.lcHdrFile)
    DELETE ALL FOR nStyOrder=0
    GOTO TOP
    IF EOF()
      llStartGen = .F.

      *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
      *=gfModalGen('INM38130B38018','DIALOG',IIF(loParentForm.lcChoice = 'P',LANG_MFGENCT_POORDERS,LANG_MFGENCT_CTORDERS))
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *=gfModalGen('INM38130B38018','DIALOG',IIF(INLIST(loParentForm.lcChoice,'P','O'),LANG_MFGENCT_POORDERS,LANG_MFGENCT_CTORDERS))
      =gfModalGen('INM38130B38018','DIALOG',IIF(INLIST(loParentForm.lcChoice,'P','O'),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_POORDERS,loFormSet.GetHeaderText("LANG_MFGENCT_POORDERS",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CTORDERS,loFormSet.GetHeaderText("LANG_MFGENCT_CTORDERS",loFormSet.HeaderAlias))))
      *N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000606,1 NNA (End)

    ENDIF
  ELSE
    *-- (Using the specified generation percentages will create
    *-- 'cutting tickets with zero quantities. Cannot proceed
    *-- 'with generation.)

    *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
    *=gfModalGen('INM38131B38018','DIALOG',IIF(loParentForm.lcChoice = 'P',LANG_MFGENCT_POORDERS,LANG_MFGENCT_CTORDERS))
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *=gfModalGen('INM38131B38018','DIALOG',IIF(INLIST(loParentForm.lcChoice,'P','O'),LANG_MFGENCT_POORDERS,LANG_MFGENCT_CTORDERS))
    =gfModalGen('INM38131B38018','DIALOG',IIF(INLIST(loParentForm.lcChoice,'P','O'),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_POORDERS,loFormSet.GetHeaderText("LANG_MFGENCT_POORDERS",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CTORDERS,loFormSet.GetHeaderText("LANG_MFGENCT_CTORDERS",loFormSet.HeaderAlias))))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000606,1 NNA (End)

  ENDIF

  *!*************************************************************
  *! Name      : lfwOldVals
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/12/2004
  *! Purpose   : get the old value of the editable fields on the browse
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfwOldVals()
  *!*************************************************************
FUNCTION lfwOldVals
  LPARAMETERS loFormSet,loThis

  loFormSet.lnOldVal = loThis.VALUE

  *!*************************************************************
  *! Name      : lfvWareH
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/12/2004
  *! Purpose   : Called from the location field in the browse of the header file
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfvWareH()
  *!*************************************************************
FUNCTION lfvWareH
  LPARAMETERS loFormSet
  LOCAL lnAlias
  lnAlias = SELECT(0)
  IF !loFormSet.loWAREHOUS.SEEK(EVALUATE(loFormSet.lcHdrFile+'.cWareCode'))
    SELECT WAREHOUS
    loFormSet.lcWareCode = gfbrowware(.T.,.F.,.F.,.F.,.F.,'S')

    IF EMPTY(loFormSet.lcWareCode)
      loFormSet.lcWareCode = loFormSet.lnOldVal
    ENDIF
    REPLACE (loFormSet.lcHdrFile+'.cWareCode') WITH loFormSet.lcWareCode
  ENDIF
  SELECT (lnAlias)
  RETURN .T.

  *!*************************************************************
  *! Name      : lfwBrDylot
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/19/2004
  *! Purpose   : Called from the dyelot field in the browse of the line file
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfwBrDylot()
  *!*************************************************************
FUNCTION lfwBrDylot

  *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
  *IF loParentForm.llRpBOPFab OR (loParentForm.lcChoice$'CP' AND !EMPTY(PO))
  IF loParentForm.llRpBOPFab OR (loParentForm.lcChoice$'CPO' AND !EMPTY(PO))
  *N000606,1 NNA (End)

    RETURN(.F.)
  ENDIF
  loParentForm.loStyle.SEEK(STYLE)
  RETURN (STYLE.cDye_FLg='Y')

  *!*************************************************************
  *! Name      : lfvDyelot
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/19/2004
  *! Purpose   : Called from the dyelot field in the browse of the line file
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfvDyelot()
  *!*************************************************************
FUNCTION lfvDyelot

  IF EMPTY(Dyelot) AND STYLE.cDye_FLg='Y'
    IF loParentForm.llStyConfg
      =gfModalGen('INM38289B38018','DIALOG')
    ELSE
      =gfModalGen('INM38132B38018','DIALOG')
    ENDIF
  ENDIF

  IF loParentForm.lcChoice <> 'L'
    lcDyelot = Dyelot
    lcKey=IIF(loParentForm.lcChoice='P',PO+cDivision+cPurCode+cStyGrade+STYLE+cWareCode,;
      PO+cDivision+SEASON+STYLE+cWareCode)
    lcExp=IIF(loParentForm.lcChoice='P','CTKTNO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+CWARECODE+DYELOT',;
      'CTKTNO+CDIVISION+SEASON+STYLE+CWARECODE+DYELOT')
    SELECT (loParentForm.lcCutPick)
    REPLACE ALL Dyelot WITH lcDyelot FOR EVAL(lcExp) = lcKey
    SELECT(loParentForm.lcLinFile)
  ENDIF

  *!*************************************************************
  *! Name      : lfwBrOldVal
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/19/2004
  *! Purpose   : Called from the Qty fields in the browse of the line file
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfwBrOldVal()
  *!*************************************************************
FUNCTION lfwBrOldVal
  LPARAMETERS lnCnt,lcAlias

  *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
  *IF (loParentForm.lcChoice $ 'CP' AND !EMPTY(EVALUATE(loParentForm.lcLinFile+'.PO')))
  IF (INLIST(loParentForm.lcChoice,'C','P','O') AND !EMPTY(EVALUATE(loParentForm.lcLinFile+'.PO')))
  *N000606,1 NNA (End)

    RETURN .F.
  ENDIF
  IF lnCnt <= SCALE.CNT
    loParentForm.lnOldVal = EVALUATE(lcAlias+'.Qty'+STR(lnCnt,1))
  ELSE
    RETURN .F.
  ENDIF

  *!*************************************************************
  *! Name      : lfvCTQty
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/19/2004
  *! Purpose   : Called from the Qty fields in the browse of the line file
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfvCTQty()
  *!*************************************************************
FUNCTION lfvCTQty
  LPARAMETERS lcCnt

  LOCAL lcQtyFld, lcOrdFld, lnAddedQty
  lcQtyFld = loParentForm.lcLinFile+'.Qty'+lcCnt
  lcOrdFld = 'Ord'+lcCnt
  *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice

  *B608675,1 WAM 09/02/2008 Fix bug while generate CT from SO results in create CT without lines
*!*	  IF loParentForm.lcChoice='O' THEN
*!*	    =lfvPoSzQty(lcCnt)
*!*	    RETURN
*!*	  ENDIF
  *B608675,1 WAM 09/02/2008 (End)
  IF EVALUATE(lcQtyFld) < EVALUATE(lcOrdFld)
    *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
    *B608675,1 WAM 09/02/2008 Fix bug while generate CT from SO results in create CT without lines
    *IF loParentForm.lcChoice='O' THEN
    IF INLIST(loParentForm.lcChoice,'O','L') THEN
    *B608675,1 WAM 09/02/2008 (End)
      IF EVALUATE(lcQtyFld)<0 THEN
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *=gfModalGen('TRM42000B40011','DIALOG',LANG_MFGENCT_PO+'|'+LANG_MFGENCT_PO)
        =gfModalGen('TRM42000B40011','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_PO,loFormSet.GetHeaderText("LANG_MFGENCT_PO",loFormSet.HeaderAlias))+'|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_PO,loFormSet.GetHeaderText("LANG_MFGENCT_PO",loFormSet.HeaderAlias)))
        *N000682,1 11/20/2012 MMT Globlization changes[End]

      ENDIF
    ELSE
    *N000606,1 NNA (END)
      =gfModalGen('INM38133B38018','DIALOG',IIF(loParentForm.lcChoice='P',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_PO,loFormSet.GetHeaderText("LANG_MFGENCT_PO",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CT,loFormSet.GetHeaderText("LANG_MFGENCT_CT",loFormSet.HeaderAlias)))+'|'+;
      IIF(loParentForm.lcChoice='P',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_PO,loFormSet.GetHeaderText("LANG_MFGENCT_PO",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CT,loFormSet.GetHeaderText("LANG_MFGENCT_CT",loFormSet.HeaderAlias))))
    *N000606,1 NNA (END)
    ENDIF
    *N000606,1 NNA (END)
    REPLACE (lcQtyFld) WITH loParentForm.lnOldVal
  ELSE
    IF loParentForm.llRpBOPFab
      SELECT (loParentForm.lcFabrics)
      =SEEK(EVALUATE(loParentForm.lcLinFile+'.Fabric'))
      LOCATE REST WHILE Fabric+cPriority+Dyelot+cWareCode=EVALUATE(loParentForm.lcLinFile+'.Fabric');
        FOR Dyelot+cWareCode = EVALUATE(loParentForm.lcLinFile+'.Dyelot+'+loParentForm.lcLinFile+'.cFabWare')
      SELECT (loParentForm.lcLinFile)
      IF (EVALUATE(lcQtyFld) - loParentForm.lnOldVal)*nYeild > EVALUATE(loParentForm.lcFabrics+'.ONHAND-'+loParentForm.lcFabrics+'.nAllocated')
        *Message : 38150
        *Not Enough Inventory available to allocate this order line
        *Button : 00000
        *Ok
        =gfModalGen('TRM38150B00000','ALERT')
        REPLACE (lcQtyFld) WITH loParentForm.lnOldVal
        RETURN
      ENDIF
      SELECT (loParentForm.lcFabrics)
      REPLACE nAllocated WITH nAllocated + (EVALUATE(lcQtyFld)-loParentForm.lnOldVal)*EVALUATE(loParentForm.lcLinFile+'.nYeild')
    ENDIF
    SELECT (loParentForm.lcLinFile)
    lnAddedQty = EVALUATE(lcQtyFld) - loParentForm.lnOldVal

    LOCAL ARRAY laUnitFCost[7],laUnitICost[7]
    FOR lnI = 1 TO 7
      lcI = STR(lnI,1)
      *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
      *laUnitFCost[lnI] = IIF(TotQty=0,IIF(loParentForm.lcChoice='P',;
         EVALUATE('STYLE.nICost'+lcI),;
         EVALUATE('STYLE.nMCost'+lcI)),;
         EVALUATE('nFCost'+lcI)/TotQty)
      *laUnitICost[lnI] = IIF(TotQty=0,IIF(loParentForm.lcChoice='P',;
         EVALUATE('STYLE.nICost'+lcI),;
         EVALUATE('STYLE.nMCost'+lcI)),;
         EVALUATE('nICost'+lcI)/TotQty)

      laUnitFCost[lnI] = IIF(TotQty=0,IIF(loParentForm.lcChoice $ 'PO',;
         EVALUATE('STYLE.nICost'+lcI),;
         EVALUATE('STYLE.nMCost'+lcI)),;
         EVALUATE('nFCost'+lcI)/TotQty)
      laUnitICost[lnI] = IIF(TotQty=0,IIF(loParentForm.lcChoice $ 'PO',;
         EVALUATE('STYLE.nICost'+lcI),;
         EVALUATE('STYLE.nMCost'+lcI)),;
         EVALUATE('nICost'+lcI)/TotQty)
         *N000606,1 NNA (End)

    ENDFOR

    *! B609028,1 MMT 10/08/2009 Fix bug of can not edit p.price in line detail screen[Start]
*!*	    REPLACE TotQty  WITH TotQty  + lnAddedQty,;
*!*	            nFCost1 WITH nFCost1 + lnAddedQty * laUnitFCost[1],;
*!*	            nFCost2 WITH nFCost2 + lnAddedQty * laUnitFCost[2],;
*!*	            nFCost3 WITH nFCost3 + lnAddedQty * laUnitFCost[3],;
*!*	            nFCost4 WITH nFCost4 + lnAddedQty * laUnitFCost[4],;
*!*	            nFCost5 WITH nFCost5 + lnAddedQty * laUnitFCost[5],;
*!*	            nFCost6 WITH nFCost6 + lnAddedQty * laUnitFCost[6],;
*!*	            nFCost7 WITH nFCost7 + lnAddedQty * laUnitFCost[7],;
*!*	            Gros_Price WITH laUnitFCost[1],;
*!*	            nICost1 WITH nICost1 + lnAddedQty * laUnitICost[1],;
*!*	            nICost2 WITH nICost2 + lnAddedQty * laUnitICost[2],;
*!*	            nICost3 WITH nICost3 + lnAddedQty * laUnitICost[3],;
*!*	            nICost4 WITH nICost4 + lnAddedQty * laUnitICost[4],;
*!*	            nICost5 WITH nICost5 + lnAddedQty * laUnitICost[5],;
*!*	            nICost6 WITH nICost6 + lnAddedQty * laUnitICost[6],;
*!*	            nICost7 WITH nICost7 + lnAddedQty * laUnitICost[7]
    REPLACE TotQty  WITH TotQty  + lnAddedQty,;
            nFCost1 WITH nFCost1 + lnAddedQty * laUnitFCost[1],;
            nFCost2 WITH nFCost2 + lnAddedQty * laUnitFCost[2],;
            nFCost3 WITH nFCost3 + lnAddedQty * laUnitFCost[3],;
            nFCost4 WITH nFCost4 + lnAddedQty * laUnitFCost[4],;
            nFCost5 WITH nFCost5 + lnAddedQty * laUnitFCost[5],;
            nFCost6 WITH nFCost6 + lnAddedQty * laUnitFCost[6],;
            nFCost7 WITH nFCost7 + lnAddedQty * laUnitFCost[7],;
            Gros_Price WITH IIF(loParentForm.lcChoice='P',Gros_Price ,laUnitFCost[1]),;
            nICost1 WITH nICost1 + lnAddedQty * laUnitICost[1],;
            nICost2 WITH nICost2 + lnAddedQty * laUnitICost[2],;
            nICost3 WITH nICost3 + lnAddedQty * laUnitICost[3],;
            nICost4 WITH nICost4 + lnAddedQty * laUnitICost[4],;
            nICost5 WITH nICost5 + lnAddedQty * laUnitICost[5],;
            nICost6 WITH nICost6 + lnAddedQty * laUnitICost[6],;
            nICost7 WITH nICost7 + lnAddedQty * laUnitICost[7]
    *! B609028,1 MMT 10/08/2009 Fix bug of can not edit p.price in line detail screen[End]


    SELECT (loParentForm.lcHdrFile)
    REPLACE nStyOrder WITH nStyOrder + lnAddedQty ,;
            nFCost1   WITH nFCost1   + lnAddedQty * laUnitFCost[1],;
            nFCost2   WITH nFCost2   + lnAddedQty * laUnitFCost[2],;
            nFCost3   WITH nFCost3   + lnAddedQty * laUnitFCost[3],;
            nFCost4   WITH nFCost4   + lnAddedQty * laUnitFCost[4],;
            nFCost5   WITH nFCost5   + lnAddedQty * laUnitFCost[5],;
            nFCost6   WITH nFCost6   + lnAddedQty * laUnitFCost[6],;
            nFCost7   WITH nFCost7   + lnAddedQty * laUnitFCost[7],;
            TOTCOST   WITH nFCost1+nFCost2+nFCost3+nFCost4+nFCost5+nFCost6+nFCost7,;
            nICost1   WITH nICost1   + lnAddedQty * laUnitICost[1],;
            nICost2   WITH nICost2   + lnAddedQty * laUnitICost[2],;
            nICost3   WITH nICost3   + lnAddedQty * laUnitICost[3],;
            nICost4   WITH nICost4   + lnAddedQty * laUnitICost[4],;
            nICost5   WITH nICost5   + lnAddedQty * laUnitICost[5],;
            nICost6   WITH nICost6   + lnAddedQty * laUnitICost[6],;
            nICost7   WITH nICost7   + lnAddedQty * laUnitICost[7]
    SELECT (loParentForm.lcLinFile)

    *B608675,1 WAM 09/02/2008 Fix bug while generate CT from SO results in create CT without lines
    *IF loParentForm.lcChoice <> 'L'
    IF INLIST(loParentForm.lcChoice,'P','C')
    *B608675,1 WAM 09/02/2008 (End)

      =lfPickWhen()
    ENDIF
  ENDIF

  *!*************************************************************
  *! Name      : lfwPick
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/19/2004
  *! Purpose   : Called from the when of the POLine browse
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfwPick()
  *!*************************************************************
FUNCTION lfwPick

  LOCAL lcKeyVal
  *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
  *IF loParentForm.lcChoice = 'L'
  IF INLIST(loParentForm.lcChoice ,'L','O')
  *N000606,1 NNA (End)
    RETURN
  ENDIF
  SELECT (loParentForm.lcLinFile)
  loParentForm.loStyle.SEEK(STYLE)
  loParentForm.loScale.SEEK('S'+STYLE.SCALE)

  *C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [BEGIN]
  *IF loParentForm.lcChoice = 'P'
  *  lcKeyVal = PO+cDivision+cPurCode+cStyGrade+STYLE+cWareCode+Dyelot
  *ELSE
  *  lcKeyVal = PO+cDivision+SEASON+STYLE+cWareCode+Dyelot
  *ENDIF

  *608597,1 ALAA [Begin] Commnetd out to rename varibale llGrpSO to be llRPGrpSO
  **IF !loParentForm.llGrpSO
  IF !loParentForm.llRPGrpSO
  *608597,1 ALAA [END] Commnetd out to rename varibale llGrpSO to be llRPGrpSO

    IF loParentForm.lcChoice = 'P'
      lcKeyVal = PO+STR(LINENO,6)+cDivision+cPurCode+cStyGrade+STYLE+cWareCode+Dyelot
    ELSE
      lcKeyVal = PO+STR(LINENO,6)+cDivision+SEASON+STYLE+cWareCode+Dyelot
    ENDIF
  ELSE
    IF loParentForm.lcChoice = 'P'
      lcKeyVal = PO+cDivision+cPurCode+cStyGrade+STYLE+cWareCode+Dyelot
    ELSE
      lcKeyVal = PO+cDivision+SEASON+STYLE+cWareCode+Dyelot
    ENDIF
  ENDIF
  *C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [END]

  SET KEY TO lcKeyVal IN (loParentForm.lcCutPick)
  =SEEK(lcKeyVal,loParentForm.lcCutPick)
  =lfPickWhen()
  SELECT (loParentForm.lcLinFile)

  *!*************************************************************
  *! Name      : lfPickWhen
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/19/2004
  *! Purpose   : Called from the when of the CutPick browse
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfPickWhen()
  *!*************************************************************
FUNCTION lfPickWhen

*B608675,1 WAM 09/02/2008 Fix bug while generate CT from SO results in create CT without lines
*!*	*N000606,1 NNA 10/06/2007 (Begin) If generate PO from OTS then return
*!*	IF loParentForm.lcChoice = 'O'
*!*	  RETURN
*!*	ENDIF
*!*	*N000606,1 NNA (End)
*B608675,1 WAM 09/02/2008 (End)
  SELECT (loParentForm.lcCutPick)
  loParentForm.loOrdHdr.SEEK('O'+ORDER)

  *!*************************************************************
  *! Name      : lfSavScr
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/12/2004
  *! Purpose   : Local procedure for the save
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : DO lfSavScr
  *!*************************************************************
FUNCTION lfSavScr
  LPARAMETERS loFormSet

  SELECT (loFormSet.lcHdrFile)

  *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
  *IF loFormSet.lcChoice$'PC' AND EOF()
  IF loFormSet.lcChoice$'PCO' AND EOF()
  *N000606,1 NNA (End)

    RETURN .F.
  ENDIF

  IF loFormSet.llGManualy .AND. SEEK(SPACE(06))

    *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
    *lcStrToAdd = IIF(loFormSet.lcChoice = 'P',LANG_MFGENCT_POORDERS,LANG_MFGENCT_CTORDERS)
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcStrToAdd = IIF(loFormSet.lcChoice = 'PO',LANG_MFGENCT_POORDERS,LANG_MFGENCT_CTORDERS)
    lcStrToAdd = IIF(loFormSet.lcChoice = 'PO',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_POORDERS,loFormSet.GetHeaderText("LANG_MFGENCT_POORDERS",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CTORDERS,loFormSet.GetHeaderText("LANG_MFGENCT_CTORDERS",loFormSet.HeaderAlias)))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000606,1 NNA (End)

    lnNoThing  = gfModalGen('TRM38047B00000','DIALOG',lcStrToAdd)
    RETURN .F.
  ENDIF

  PRIVATE loStyDye
  loStyDye = CREATEOBJECT('RemoteTable','STYDYE','STYDYE','STYDYE',loFormSet.DATASESSIONID)
  loFormSet.loStyle.SETORDER('STYLE')
  DO CASE
    *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
    *CASE loFormSet.lcChoice = 'P'
    CASE INLIST(loFormSet.lcChoice,'P','O')
    *N000606,1 NNA (End)

      IF !lfUpdPO(loFormSet)
        loStyDye.DESTROY
        RETURN .F.
      ENDIF
    CASE loFormSet.lcChoice = 'C'
      IF !lfUpdCT(loFormSet)
        loStyDye.DESTROY
        RETURN .F.
      ENDIF
    CASE loFormSet.lcChoice = 'L'
      IF !lfUpdCTP(loFormSet)
        loStyDye.DESTROY
        RETURN .F.
      ENDIF
  ENDCASE
  loStyDye.DESTROY

  *-- if there is at least one record generated
  IF !loFormSet.llGManualy .AND. !EMPTY(loFormSet.lcFirstCT)
    *-- Give the numbers of the generated P/Os or C/Ts
    *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
    *=gfModalGen('INM38134B38018','DIALOG',IIF(loFormSet.lcChoice = 'P',LANG_MFGENCT_PO,LANG_MFGENCT_CT)+'|'+ALLTRIM(loFormSet.lcFirstCT)+"|"+ALLTRIM(loFormSet.lcLastCt))
    *IF loFormSet.lcChoice <> 'L'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *=gfModalGen('INM38134B38018','DIALOG',IIF(loFormSet.lcChoice $ 'PO',LANG_MFGENCT_PO,LANG_MFGENCT_CT)+'|'+ALLTRIM(loFormSet.lcFirstCT)+"|"+ALLTRIM(loFormSet.lcLastCt))
    =gfModalGen('INM38134B38018','DIALOG',IIF(loFormSet.lcChoice $ 'PO',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_PO,loFormSet.GetHeaderText("LANG_MFGENCT_PO",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CT,loFormSet.GetHeaderText("LANG_MFGENCT_CT",loFormSet.HeaderAlias)))+'|'+ALLTRIM(loFormSet.lcFirstCT)+"|"+ALLTRIM(loFormSet.lcLastCt))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    IF !INLIST(loFormSet.lcChoice,'L','O')
    *N000606,1 NNA (End)

      =lfRefOrder(loFormSet)
    ENDIF
  ENDIF

  IF loFormSet.llRpBOPFab
    *Message : 38151
    *Whould you like to print a process summary report?
    *Button : 38006
    *Yes,No
    IF gfModalGen('QRM38151B38006','ALERT') =1
      =lfPrnSumm(loFormSet)
    ENDIF
  ENDIF

  *-- Phase II when the project programs has finished.
   IF loFormSet.lcGenProj $ 'AI'
      =lfGenProj(loFormSet)
   ENDIF

  *B131131,1 KHM 05/02/2006 [Begin]
  loFormSet.ACTIVATE()
  *B131131,1 KHM 05/02/2006 [End]

  loFormSet.ActiveMode = "V"

  *!*************************************************************
  *! Name      : lfvOrQty
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/19/2004
  *! Purpose   : Called from the qty fileds of browse lines
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfvOrQty()
  *!*************************************************************
FUNCTION lfvOrQty
  LPARAMETERS lcNo

  LOCAL lcQtyFld, lnCurRecNo, lnAddedQty, lnAvailQty
  lcQtyFld = 'Qty'+lcNo

  SELECT (loParentForm.lcCutPick)
  IF loParentForm.loOrdLine.SEEK('O'+ORDER+cOrdLine)
    lnAvailQty = EVALUATE('ORDLINE.'+lcQtyFld+' - ORDLINE.Cut'+lcNo)
    IF (EVALUATE(lcQtyFld) < 0 .AND. gfModalGen('INM38136B38018','DIALOG')=1) .OR. ;
        (EVALUATE(lcQtyFld) > lnAvailQty .AND. ;
        gfModalGen('INM38137B38018','DIALOG',ALLTRIM(STR(lnAvailQty)))=1)
      SELECT (loParentForm.lcCutPick)
      REPLACE (lcQtyFld) WITH loParentForm.lnOldVal
    ELSE
      lnAddedQty = EVALUATE(lcQtyFld) - loParentForm.lnOldVal
      IF loParentForm.llRpBOPFab
        SELECT (loParentForm.lcFabrics)
        =SEEK(EVALUATE(loParentForm.lcLinFile+'.Fabric'))
        LOCATE REST WHILE Fabric+cPriority+Dyelot+cWareCode=EVALUATE(loParentForm.lcLinFile+'.Fabric');
          FOR Dyelot+cWareCode = EVALUATE(loParentForm.lcLinFile+'.Dyelot+'+loParentForm.lcLinFile+'.cFabWare')
        REPLACE nAllocated WITH nAllocated + lnAddedQty*EVALUATE(loParentForm.lcLinFile+'.nYeild')
      ENDIF
      SELECT (loParentForm.lcLinFile)

      LOCAL ARRAY laUnitFCost[7],laUnitICost[7]
      FOR lnI = 1 TO 7
        lcI = STR(lnI,1)
        laUnitFCost[lnI] = IIF(TotQty=0,IIF(loParentForm.lcChoice='P',;
          EVALUATE('STYLE.nICost'+lcI),;
          EVALUATE('STYLE.nMCost'+lcI)),;
          EVALUATE('nFCost'+lcI)/TotQty)
        laUnitICost[lnI] = IIF(TotQty=0,IIF(loParentForm.lcChoice='P',;
          EVALUATE('STYLE.nICost'+lcI),;
          EVALUATE('STYLE.nMCost'+lcI)),;
          EVALUATE('nICost'+lcI)/TotQty)
      ENDFOR

      =RLOCK()
      REPLACE (lcQtyFld)   WITH EVALUATE(lcQtyFld)+lnAddedQty,;
        TotQty       WITH TotQty            +lnAddedQty,;
        ('Ord'+lcNo) WITH Ord&lcNo          +lnAddedQty,;
        TotOrd       WITH TotOrd            +lnAddedQty,;
        nFCost1      WITH nFCost1           +lnAddedQty*laUnitFCost[1],;
        nFCost2      WITH nFCost2           +lnAddedQty*laUnitFCost[2],;
        nFCost3      WITH nFCost3           +lnAddedQty*laUnitFCost[3],;
        nFCost4      WITH nFCost4           +lnAddedQty*laUnitFCost[4],;
        nFCost5      WITH nFCost5           +lnAddedQty*laUnitFCost[5],;
        nFCost6      WITH nFCost6           +lnAddedQty*laUnitFCost[6],;
        nFCost7      WITH nFCost7           +lnAddedQty*laUnitFCost[7],;
        Gros_Price   WITH laUnitFCost[1],;
        nICost1      WITH nICost1           +lnAddedQty*laUnitICost[1],;
        nICost2      WITH nICost2           +lnAddedQty*laUnitICost[2],;
        nICost3      WITH nICost3           +lnAddedQty*laUnitICost[3],;
        nICost4      WITH nICost4           +lnAddedQty*laUnitICost[4],;
        nICost5      WITH nICost5           +lnAddedQty*laUnitICost[5],;
        nICost6      WITH nICost6           +lnAddedQty*laUnitICost[6],;
        nICost7      WITH nICost7           +lnAddedQty*laUnitICost[7]
      UNLOCK
      IF loParentForm.lcChoice $ 'CP'
        SELECT (loParentForm.lcHdrFile)
        =RLOCK()
        REPLACE nStyOrder WITH nStyOrder + lnAddedQty,;
          TotOrd    WITH TotOrd    + lnAddedQty,;
          nFCost1   WITH nFCost1   + lnAddedQty * laUnitFCost[1],;
          nFCost2   WITH nFCost2   + lnAddedQty * laUnitFCost[2],;
          nFCost3   WITH nFCost3   + lnAddedQty * laUnitFCost[3],;
          nFCost4   WITH nFCost4   + lnAddedQty * laUnitFCost[4],;
          nFCost5   WITH nFCost5   + lnAddedQty * laUnitFCost[5],;
          nFCost6   WITH nFCost6   + lnAddedQty * laUnitFCost[6],;
          nFCost7   WITH nFCost7   + lnAddedQty * laUnitFCost[7],;
          TOTCOST   WITH nFCost1+nFCost2+nFCost3+nFCost4+nFCost5+nFCost6+nFCost7,;
          nICost1   WITH nICost1   + lnAddedQty * laUnitICost[1],;
          nICost2   WITH nICost2   + lnAddedQty * laUnitICost[2],;
          nICost3   WITH nICost3   + lnAddedQty * laUnitICost[3],;
          nICost4   WITH nICost4   + lnAddedQty * laUnitICost[4],;
          nICost5   WITH nICost5   + lnAddedQty * laUnitICost[5],;
          nICost6   WITH nICost6   + lnAddedQty * laUnitICost[6],;
          nICost7   WITH nICost7   + lnAddedQty * laUnitICost[7]
        UNLOCK
      ENDIF
      SELECT (loParentForm.lcCutPick)
      =RLOCK()
      REPLACE TotQty WITH TotQty + lnAddedQty
      UNLOCK
    ENDIF
  ENDIF

  *!*************************************************************
  *! Name      : lfSelForcast
  *! Developer : Wael Ali MOhamed
  *! Date      : 06/30/2005
  *! Purpose   : Collect styles
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfSelForcast(loFormSet)
  *!*************************************************************
FUNCTION lfSelForcast
  LPARAMETERS loFormSet
  PRIVATE lnAlias,lcCutTag,lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,llContinue
  lnAlias = SELECT()
  IF TYPE('lcExpr')='L'
    RETURN(.F.)
  ENDIF
  loFormSet.loPOSLN = IIF(TYPE('loFormSet.loPOSLN')='O',loFormSet.loPOSLN,CREATEOBJECT('RemoteTable','POSLN','POSLN','POSLN',loFormSet.DATASESSIONID))
  loFormSet.loInvLine = IIF(TYPE('loFormSet.loInvLine')='O',loFormSet.loInvLine,CREATEOBJECT('RemoteTable','INVLINE','INVLINE','INVLINE',loFormSet.DATASESSIONID))
  loFormSet.loForCast = IIF(TYPE('loFormSet.loForCast')='O',loFormSet.loForCast,CREATEOBJECT('RemoteTable','FORCAST','FORCAST','FORCAST',loFormSet.DATASESSIONID))
  loFormSet.loPOSLN.SETORDER('POSLNS')
  loFormSet.loInvLine.SETORDER('INVLINEO')
  loFormSet.loOrdLine.SETORDER('ORDLINES')

  SELECT (loFormSet.lcStyTmp)
  ZAP
  STORE .F. TO llContinue
  CREATE CURSOR (loFormSet.lcTmpFrCst) (STYLE C(19), cYear C(4), cWeek C(2), Qty1 N(6), Qty2 N(6), Qty3 N(6), Qty4 N(6), Qty5 N(6), Qty6 N(6), Qty7 N(6), Qty8 N(6))
  INDEX ON STYLE+cYear+cWeek TAG (loFormSet.lcTmpFrCst)

  IF loFormSet.llOrdRange
    SELECT (loFormSet.lcOrdAlias)
    SCAN
      lcStyMajor = SUBSTR(cStyMajor,1,loFormSet.lnMajorLen)
      SELECT STYLE
      loFormSet.loStyle.SEEK(lcStyMajor)
      SCAN REST WHILE STYLE = lcStyMajor FOR &lcExpr
        *N000682,1 MMT 12/09/2012 Globalization changes[Start]
        *WAIT 'Collecting data for style: '+STYLE WINDOW NOWAIT
        WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_COLLWAITSTYLE,loFormSet.GetHeaderText("LANG_MFGENCT_COLLWAITSTYLE",loFormSet.HeaderAlias))+' '+STYLE WINDOW NOWAIT
        *N000682,1 MMT 12/09/2012 Globalization changes[Start]
        llContinue = IIF(lfStyleReq(loFormSet,STYLE.STYLE),.T.,llContinue)
      ENDSCAN
    ENDSCAN
  ELSE
    IF EMPTY(loFormSet.lcStyGroup)
      SELECT STYLE
      loFormSet.loStyle.SETORDER('STYLE')
      loFormSet.loStyle.SEEK('')
      SCAN FOR &lcExpr
        *N000682,1 MMT 12/09/2012 Globalization changes[Start]
        *WAIT 'Collecting data for style: '+STYLE WINDOW NOWAIT
        WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_COLLWAITSTYLE,loFormSet.GetHeaderText("LANG_MFGENCT_COLLWAITSTYLE",loFormSet.HeaderAlias))+' '+STYLE WINDOW NOWAIT
        *N000682,1 MMT 12/09/2012 Globalization changes[END]
        llContinue = IIF(lfStyleReq(loFormSet,STYLE.STYLE),.T.,llContinue)
      ENDSCAN
    ELSE
      SELECT STYLE
      loFormSet.loStyle.SETORDER('STYGROP')
      loFormSet.loStyle.SEEK(loFormSet.lcStyGroup)
      SCAN REST WHILE cStyGroup+STYLE = loFormSet.lcStyGroup FOR &lcExpr
        *N000682,1 MMT 12/09/2012 Globalization changes[Start]
        *WAIT 'Collecting data for style: '+STYLE WINDOW NOWAIT      
        WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_COLLWAITSTYLE,loFormSet.GetHeaderText("LANG_MFGENCT_COLLWAITSTYLE",loFormSet.HeaderAlias))+' '+STYLE WINDOW NOWAIT
        *N000682,1 MMT 12/09/2012 Globalization changes[END]        
        llContinue = IIF(lfStyleReq(loFormSet,STYLE.STYLE),.T.,llContinue)
      ENDSCAN
    ENDIF
  ENDIF
  WAIT CLEAR
  loFormSet.loStyle.SETORDER('STYLE')

  IF !llContinue
    =lfvSelect('',loFormSet)
    loFormSet.AriaForm1.cmdselect.ENABLED = .F.
    loFormSet.ActiveMode = 'S'
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *=gfModalGen('INM38129B38018','DIALOG','styles')
    =gfModalGen('INM38129B38018','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_STYLES,loFormSet.GetHeaderText("LANG_MFGENCT_STYLES",loFormSet.HeaderAlias)))
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
  ENDIF
  GO TOP IN (loFormSet.lcStyTmp)
  SELECT(lnAlias)
  RETURN(llContinue)

  *!*************************************************************
  *! Name      : lfStyleReq
  *! Developer : Wael Ali MOhamed
  *! Date      : 06/30/2005
  *! Purpose   : Get style forcast quantitiy
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfStyleReq(loFormSet, lcStyle)
  *!*************************************************************
FUNCTION lfStyleReq
  LPARAMETERS loFormSet, lcStyle
  LOCAL llForcastLines
  llForcastLines  = .F.
  IF loFormSet.loForCast.SEEK(lcStyle)

    IF loFormSet.loPOSLN.SEEK('0001'+lcStyle+'PU')
      SELECT POSLN
      DO WHILE cinvtype+STYLE+cbusdocu+cstytype+PO+STR(LINENO,6)+TRANCD = '0001'+lcStyle+'PU'
        lcPo = PO
        llWIP = loFormSet.loPosHdr.SEEK('PU'+lcPo) AND POSHDR.COMPLETE <= ldRpFrDate AND POSHDR.STATUS <> 'X'
        STORE 0 TO lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8
        SCAN REST WHILE cinvtype+STYLE+cbusdocu+cstytype+PO+STR(LINENO,6)+TRANCD = '0001'+lcStyle+'PU'+lcPo FOR llWIP
          FOR lnCount = 1 TO 8
            lcCount = STR(lnCount,1)
            lnQty&lcCount = lnQty&lcCount+IIF(TRANCD='1',Qty&lcCount ,-1*Qty&lcCount )
          ENDFOR
        ENDSCAN
        IF !SEEK(lcStyle+PADR(YEAR(POSHDR.COMPLETE),4)+PADL(WEEK(POSHDR.COMPLETE),2,'0'),loFormSet.lcTmpFrCst)
          INSERT INTO (loFormSet.lcTmpFrCst) (STYLE, cYear, cWeek) VALUES (lcStyle,PADR(YEAR(POSHDR.COMPLETE),4),PADL(WEEK(POSHDR.COMPLETE),2,'0'))
        ENDIF
        REPLACE Qty1 WITH Qty1 + lnQty1 ,;
          Qty2 WITH Qty2 + lnQty2 ,;
          Qty3 WITH Qty3 + lnQty3 ,;
          Qty4 WITH Qty4 + lnQty4 ,;
          Qty5 WITH Qty5 + lnQty5 ,;
          Qty6 WITH Qty6 + lnQty6 ,;
          Qty7 WITH Qty7 + lnQty7 ,;
          Qty8 WITH Qty8 + lnQty8 IN (loFormSet.lcTmpFrCst)
      ENDDO
    ENDIF
    SELECT FORCAST
    LOCATE REST WHILE STYLE+STR(nyear,4)+STR(nweek,2) = lcStyle ;
      FOR   nyear <= YEAR(ldRpFrDate) AND nweek <= WEEK(ldRpFrDate)
    IF FOUND()
      lnStk1 = STYLE.Stk1
      lnStk2 = STYLE.Stk2
      lnStk3 = STYLE.Stk3
      lnStk4 = STYLE.Stk4
      lnStk5 = STYLE.Stk5
      lnStk6 = STYLE.Stk6
      lnStk7 = STYLE.Stk7
      lnStk8 = STYLE.Stk8
      SCAN REST WHILE STYLE+STR(nyear,4)+STR(nweek,2) = lcStyle ;
          FOR nyear <= YEAR(ldRpFrDate) AND nweek <= WEEK(ldRpFrDate)
        lnForCst1 = nForQty1
        lnForCst2 = nForQty2
        lnForCst3 = nForQty3
        lnForCst4 = nForQty4
        lnForCst5 = nForQty5
        lnForCst6 = nForQty6
        lnForCst7 = nForQty7
        lnForCst8 = nForQty8
        ldForCstD = lfvWeek(PADR(nyear,4), nweek)
        IF loFormSet.loOrdLine.SEEK(lcStyle)
          SELECT ORDLINE
          SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = lcStyle
            IF BETWEEN(COMPLETE,ldForCstD,ldForCstD+6)
              lnForCst1 = lnForCst1 - ORDLINE.Qty1
              lnForCst2 = lnForCst2 - ORDLINE.Qty2
              lnForCst3 = lnForCst3 - ORDLINE.Qty3
              lnForCst4 = lnForCst4 - ORDLINE.Qty4
              lnForCst5 = lnForCst5 - ORDLINE.Qty5
              lnForCst6 = lnForCst6 - ORDLINE.Qty6
              lnForCst7 = lnForCst7 - ORDLINE.Qty7
              lnForCst8 = lnForCst8 - ORDLINE.Qty8
              IF loFormSet.loInvLine.SEEK(ORDLINE.ORDER+STR(ORDLINE.LINENO,6))
                SELECT INVLINE
                SCAN REST WHILE ORDER+STR(LINENO,6)+invoice = ORDLINE.ORDER+STR(ORDLINE.LINENO,6)
                  IF BETWEEN(InvDate,ldForCstD,ldForCstD+6)
                    lnForCst1 = lnForCst1 - INVLINE.Qty1
                    lnForCst2 = lnForCst2 - INVLINE.Qty2
                    lnForCst3 = lnForCst3 - INVLINE.Qty3
                    lnForCst4 = lnForCst4 - INVLINE.Qty4
                    lnForCst5 = lnForCst5 - INVLINE.Qty5
                    lnForCst6 = lnForCst6 - INVLINE.Qty6
                    lnForCst7 = lnForCst7 - INVLINE.Qty7
                    lnForCst8 = lnForCst8 - INVLINE.Qty8
                  ENDIF
                ENDSCAN
              ENDIF
            ENDIF
          ENDSCAN
        ENDIF
        FOR lnCount = 1 TO 8
          lcCount = STR(lnCount,1)
          lnForCst&lcCount = MAX(lnForCst&lcCount,0)
          IF lnStk&lcCount >= lnForCst&lcCount
            lnPlan&lcCount = 0
            lnStk&lcCount  = lnStk&lcCount - lnForCst&lcCount
          ELSE
            lnPlan&lcCount = lnForCst&lcCount - lnStk&lcCount
            lnStk&lcCount = 0
            SELECT (loFormSet.lcTmpFrCst)
            =SEEK(lcStyle)
            SCAN REST WHILE STYLE = lcStyle FOR VAL(cYear) <= FORCAST.nyear AND VAL(cWeek)<= FORCAST.nweek AND lnPlan&lcCount > 0
              IF lnPlan&lcCount - Qty&lcCount >= 0
                lnPlan&lcCount = lnPlan&lcCount - Qty&lcCount
                REPLACE Qty&lcCount WITH 0
              ELSE
                REPLACE Qty&lcCount WITH Qty&lcCount - lnPlan&lcCount
                lnPlan&lcCount = 0
              ENDIF
            ENDSCAN
          ENDIF
        ENDFOR
        IF lnPlan1+lnPlan2+lnPlan3+lnPlan4+lnPlan5+lnPlan6+lnPlan7+lnPlan8 > 0
          SELECT STYLE
          SCATTER MEMVAR
          m.cWeek = PADL(FORCAST.nweek,2,'0')
          m.cYear = PADL(FORCAST.nyear,4,'0')
          m.PLAN1 = lnPlan1
          m.PLAN2 = lnPlan2
          m.PLAN3 = lnPlan3
          m.PLAN4 = lnPlan4
          m.PLAN5 = lnPlan5
          m.PLAN6 = lnPlan6
          m.PLAN7 = lnPlan7
          m.PLAN8 = lnPlan8
          m.TOTPLAN = m.PLAN1+m.PLAN2+m.PLAN3+m.PLAN4+m.PLAN5+m.PLAN6+m.PLAN7+m.PLAN8
          llForcastLines = .T.
          INSERT INTO (loFormSet.lcStyTmp) FROM MEMVAR
        ENDIF
      ENDSCAN
    ENDIF
  ENDIF
  RETURN llForcastLines

  ****This function will be done in Phase II
  *!*************************************************************
  *! Name      : lfSelStyle
  *! Developer : Wael Aly Mohamed
  *! Date      : 10/10/1998
  *! Purpose   : Select the records matching the criteria
  *!             from the style file in case of generate C/T from Plan
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfSelStyle()
  *!*************************************************************
FUNCTION lfSelStyle
  LPARAMETERS loFormSet, lcChoice


  PRIVATE lnAlias,lcCutTag,lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,llContinue
  lnAlias = SELECT()
  *N000606,1 NNA 10/06/2007 (Begin) Call another function to Select the records matching the criteria
  *N000606,1 NNA             from the style file in case of generate PO from OTS
  *IF TYPE('lcExpr')='L'
  *  RETURN(.F.)
  *ENDIF
  IF lcExpr = '.F.'
    RETURN(.F.)
  ENDIF
  IF loFormSet.lcChoice='O'
    =lfPoSelSty(loFormSet)
    RETURN
  ENDIF
  *N000606,1 NNA (End)
  loFormSet.loPOSLN = IIF(TYPE('loFormSet.loPOSLN')='O',loFormSet.loPOSLN,CREATEOBJECT('RemoteTable','POSLN','POSLN','POSLN',loFormSet.DATASESSIONID))
  SELECT (loFormSet.lcStyTmp)
  ZAP
  STORE .F. TO llContinue
  loFormSet.loPOSLN.SETORDER('POSLNS')
  IF loFormSet.llOrdRange
    SELECT (loFormSet.lcOrdAlias)
    SCAN
      lcStyMajor = SUBSTR(cStyMajor,1,loFormSet.lnMajorLen)
      SELECT STYLE
      loFormSet.loStyle.SEEK(lcStyMajor)
      SCAN REST WHILE STYLE = lcStyMajor FOR &lcExpr
        *N000682,1 MMT 12/09/2012 Globalization changes[Start]
        *WAIT 'Collecting data for style: '+STYLE WINDOW NOWAIT
        WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_COLLWAITSTYLE,loFormSet.GetHeaderText("LANG_MFGENCT_COLLWAITSTYLE",loFormSet.HeaderAlias))+' '+STYLE WINDOW NOWAIT
        *N000682,1 MMT 12/09/2012 Globalization changes[END]
        IF lcChoice = 'O'
          llContinue = IIF(lfStyleOTS(loFormSet),.T.,llContinue)
        ELSE
          llContinue = IIF(lfStylePlan(loFormSet),.T.,llContinue)
        ENDIF
      ENDSCAN
    ENDSCAN
  ELSE
    IF EMPTY(loFormSet.lcStyGroup)
      SELECT STYLE
      loFormSet.loStyle.SETORDER('STYLE')
      loFormSet.loStyle.SEEK('')
      SCAN FOR &lcExpr
        *N000682,1 MMT 12/09/2012 Globalization changes[Start]
        *WAIT 'Collecting data for style: '+STYLE WINDOW NOWAIT        
        WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_COLLWAITSTYLE,loFormSet.GetHeaderText("LANG_MFGENCT_COLLWAITSTYLE",loFormSet.HeaderAlias))+' '+STYLE WINDOW NOWAIT
        *N000682,1 MMT 12/09/2012 Globalization changes[END]
        IF lcChoice = 'O'
          llContinue = IIF(lfStyleOTS(loFormSet),.T.,llContinue)
        ELSE
          llContinue = IIF(lfStylePlan(loFormSet),.T.,llContinue)
        ENDIF
      ENDSCAN
    ELSE
      SELECT STYLE
      loFormSet.loStyle.SETORDER('STYGROP')
      loFormSet.loStyle.SEEK(loFormSet.lcStyGroup)
      SCAN REST WHILE cStyGroup+STYLE = loFormSet.lcStyGroup FOR &lcExpr
        *N000682,1 MMT 12/09/2012 Globalization changes[Start]
        *WAIT 'Collecting data for style: '+STYLE WINDOW NOWAIT        
        WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_COLLWAITSTYLE,loFormSet.GetHeaderText("LANG_MFGENCT_COLLWAITSTYLE",loFormSet.HeaderAlias))+' '+STYLE WINDOW NOWAIT
        *N000682,1 MMT 12/09/2012 Globalization changes[END]
        IF lcChoice = 'O'
          llContinue = IIF(lfStyleOTS(loFormSet),.T.,llContinue)
        ELSE
          llContinue = IIF(lfStylePlan(loFormSet),.T.,llContinue)
        ENDIF
      ENDSCAN
    ENDIF
  ENDIF
  WAIT CLEAR
  loFormSet.loStyle.SETORDER('STYLE')
  IF !llContinue
    =lfvSelect('',loFormSet)
    loFormSet.AriaForm1.cmdselect.ENABLED = .F.
    loFormSet.ActiveMode = 'S'
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *-=gfModalGen('INM38129B38018','DIALOG','styles')    
    =gfModalGen('INM38129B38018','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_STYLES,loFormSet.GetHeaderText("LANG_MFGENCT_STYLES",loFormSet.HeaderAlias)))
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
  ENDIF
  GO TOP IN (loFormSet.lcStyTmp)
  SELECT(lnAlias)
  RETURN(llContinue)
	*!*************************************************************
	*! Name      : lfPoSelSty
	*! Developer : Nader Nabil (NNA)
	*! Date      : 10/17/2007
	*! Purpose   : Select the records matching the criteria
	*!             from the style file in case of generate PO from OTS
	*!*************************************************************
	*! Parameters: None
	*!*************************************************************
	*! Returns   : None
	*!*************************************************************
	*! Example   : =lfPoSelSty()
	*!*************************************************************
	FUNCTION lfPoSelSty
	LPARAMETERS loFormSet
	llOpenMe   = .F.
	lcBasFile  = 'STYLE'
	lcWarehuse = ''
	loFormSet.loPOSLN = IIF(TYPE('loFormSet.loPOSLN')='O',loFormSet.loPOSLN,CREATEOBJECT('RemoteTable','POSLN','POSLN','POSLN',loFormSet.DataSessionId))
	llWareHous = ALLTRIM(gfGetMemVar('M_WareHouse'))= 'Y'
	IF llWareHous
	  llOpenMe =gfOpenTable( oAriaApplication.DataDir+'STYDYE', oAriaApplication.DataDir+'STYDYE','SH')
	  lnStPos = ATC('STYDYE.CWARECODE',lcExpr)
	  IF lnStPos > 0
	    lcBasFile = 'STYDYE'
	    lcTempVr  = SUBSTR(lcExpr,lnStPos,LEN(lcExpr))
	    ln1_AndPos = ATC('AND',lcTempVr)
	    IF ln1_AndPos = 0
	      lcWarExp = lcTempVr
	    ELSE
	      lcWarExp   = SUBST(lcExpr,lnStPos,ln1_AndPos-1)
	    ENDIF
	    lnWareHPos = ATC('=',lcWarExp)
	    lcWarehuse = SUBSTR(lcWarExp,lnWareHPos+1,LEN(lcWarExp))
	    lcExpr     =  STRTRAN(lcExpr,lcWarExp,' .T. ')
	  ENDIF
	ENDIF
	SELECT (loFormSet.lcStyTmp)
	ZAP
	SELECT STYLE
	SET ORDER TO TAG STYLE
	SET RELATION TO 'N'+'COLOR     '+SUBSTR(Style.Style,lnColorStr,lnColorLen) INTO CODES ADDITIVE
	IF llWareHous
	  SET RELATION TO style  INTO STYDYE ADDITIVE
	ENDIF

	STORE .F. TO llContinue
	IF EMPTY(loFormSet.lcStyGroup)
	  SELECT(lcBasFile)
	  loFormSet.loStyle.SETORDER('STYLE')
	  loFormSet.loSTYLE.SEEK('')
	  SELECT STYLE	
	  *! B609767,1 SAB 12/11/2011 Fix error when run the Generate PO from open to sell [Start]	
	  IF OCCURS("CODES.CCODE_NO", lcExpr) > 0
	    LOCAL lnAlias, lcClrExpr, lcNewClrExpr, lcColor, lcColorCursor, lnClrStartPos, lnClrExpLen
  	  lnAlias = SELECT()
	    lnClrStartPos = AT("INLIST(CODES.CCODE_NO",lcExpr)
  	  lnClrExpLen = AT(")",SUBSTR(lcExpr,lnClrStartPos))
	    lcClrExpr = SUBSTR(lcExpr,lnClrStartPos, lnClrExpLen)+ ","
	
  	  lcColorCursor = gfTempName()
	    DIMENSION laTempacstru[1,4]
        laTempacstru[1,1]='CCODE_NO'
  	  laTempacstru[1,2]='C'
	    laTempacstru[1,3]= 6
  	  laTempacstru[1,4]= 0
	    =gfCrtTmp(lcColorCursor,@laTempacstru,"CCODE_NO",lcColorCursor,.T.)
  	
	    FOR i = 1 TO OCCURS(",", lcClrExpr)-1
  	    lcColor = STRTRAN(SUBSTR(lcClrExpr,AT(",",lcClrExpr,i)+1,AT(",",lcClrExpr,i+1)-(AT(",",lcClrExpr,i)+1)),"'","")
	      SELECT (lcColorCursor)
  	    APPEND BLANK
	      REPLACE CCODE_NO WITH lcColor
  	  ENDFOR
	    SELECT CODES
  	  lcExpr = STUFF(lcExpr, lnClrStartPos, lnClrExpLen, "SEEK(CODES.CCODE_NO,lcColorCursor)")
	    SELECT (lnAlias)
	  ENDIF
      *! B609767,1 SAB 12/11/2011 Fix error when run the Generate PO from open to sell [End]
	  SCAN FOR &lcExpr AND !MAKE
      *N000682,1 MMT 12/09/2012 Globalization changes[Start]
      *WAIT 'Collecting data for style: '+Style WINDOW NOWAIT      
	    WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_COLLWAITSTYLE,loFormSet.GetHeaderText("LANG_MFGENCT_COLLWAITSTYLE",loFormSet.HeaderAlias))+' '+Style WINDOW NOWAIT
      *N000682,1 MMT 12/09/2012 Globalization changes[END]
	    SCATTER MEMVAR
	    SELECT(lcBasFile)
	    IF !EMPTY(lcWarehuse)
	      =SEEK(Style.Style+&lcWarehuse.)
	    ENDIF
  	  m.PLAN1   = MIN((STK1+WIP1)-ORD1,0)
		  m.PLAN2   = MIN((STK2+WIP2)-ORD2,0)
		  m.PLAN3   = MIN((STK3+WIP3)-ORD3,0)
		  m.PLAN4   = MIN((STK4+WIP4)-ORD4,0)
		  m.PLAN5   = MIN((STK5+WIP5)-ORD5,0)
		  m.PLAN6   = MIN((STK6+WIP6)-ORD6,0)
		  m.PLAN7   = MIN((STK7+WIP7)-ORD7,0)
		  m.PLAN8   = MIN((STK8+WIP8)-ORD8,0)
		  m.TOTPLAN = m.PLAN1+m.PLAN2+m.PLAN3+m.PLAN4+m.PLAN5+m.PLAN6+m.PLAN7+m.PLAN8
		  IF m.TOTPLAN < 0
		    llContinue = .T.
		    m.PLAN1   = ABS(m.PLAN1)
		    m.PLAN2   = ABS(m.PLAN2)
		    m.PLAN3   = ABS(m.PLAN3)
		    m.PLAN4   = ABS(m.PLAN4)
		    m.PLAN5   = ABS(m.PLAN5)
		    m.PLAN6   = ABS(m.PLAN6)
		    m.PLAN7   = ABS(m.PLAN7)
		    m.PLAN8   = ABS(m.PLAN8)
		    m.TOTPLAN = m.PLAN1+m.PLAN2+m.PLAN3+m.PLAN4+m.PLAN5+m.PLAN6+m.PLAN7+m.PLAN8
		    INSERT INTO (loFormSet.lcStyTmp) FROM MEMVAR
		  ENDIF
		  SELECT Style

	  ENDSCAN
	ELSE
	  SELECT(lcBasFile)
	  loFormSet.loStyle.SETORDER('STYGROP')
	  loFormSet.loSTYLE.SEEK(loFormSet.lcStyGroup)
	  SCAN REST WHILE cStyGroup+style = loFormSet.lcStyGroup FOR &lcExpr AND !MAKE
      *N000682,1 MMT 12/09/2012 Globalization changes[Start]
      *WAIT 'Collecting data for style: '+Style WINDOW NOWAIT      
	    WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_COLLWAITSTYLE,loFormSet.GetHeaderText("LANG_MFGENCT_COLLWAITSTYLE",loFormSet.HeaderAlias))+' '+Style WINDOW NOWAIT
      *N000682,1 MMT 12/09/2012 Globalization changes[END]
	    llContinue = IIF(lfStylePlan(loFormSet),.T.,llContinue)
	  ENDSCAN
	ENDIF
	SET RELATION OFF INTO CODES

	IF llWareHous
	  SET RELATION OFF INTO STYDYE
	ENDIF

	IF llOpenMe
	  =gfCloseTable('STYDYE')
	ENDIF

	WAIT CLEAR
	loFormSet.loStyle.SETORDER('STYLE')
	IF !llContinue
	  *-- if no records matching the criteria give the user a message
	  =lfvSelect('',loFormSet)
	  loFormSet.AriaForm1.cmdSelect.Enabled = .F.
	  loFormSet.ActiveMode = 'S'
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *=gfModalGen('INM38129B38018','DIALOG','styles')
	  =gfModalGen('INM38129B38018','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_STYLES,loFormSet.GetHeaderText("LANG_MFGENCT_STYLES",loFormSet.HeaderAlias)))
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
	ENDIF
	GO TOP IN (loFormSet.lcStyTmp)
	SELECT(lnAlias)
	RETURN(llContinue)

	*!* End of FUNCTION lfPoSelSty.
  *!*************************************************************
  *! Name      : lfStylePlan
  *! Developer : Wael Ali MOhamed
  *! Date      : 06/30/2005
  *! Purpose   : Get style plan quantitiy
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfStylePlan(loFormSet)
  *!*************************************************************

FUNCTION lfStylePlan
  LPARAMETERS loFormSet

  SELECT STYLE
  SCATTER MEMVAR

  SELECT POSLN
  loFormSet.loPOSLN.SEEK('0001'+m.STYLE+'PU')
  STORE 0 TO lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8

  DO WHILE cinvtype+STYLE+cbusdocu+cstytype+PO+STR(LINENO,6)+TRANCD = '0001'+m.STYLE+'PU'
    lcPo = PO
    llWIP = loFormSet.loPosHdr.SEEK('PU'+lcPo) AND POSHDR.STATUS <> 'X'
    SCAN REST WHILE cinvtype+STYLE+cbusdocu+cstytype+PO+STR(LINENO,6)+TRANCD = '0001'+m.STYLE+'PU'+lcPo FOR llWIP
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        lnQty&lcCount = lnQty&lcCount+IIF(POSHDR.STATUS<>'C',;
          IIF(TRANCD='1',Qty&lcCount ,IIF(TRANCD$'34',(-1*Qty&lcCount ),0)),IIF(TRANCD='2',Qty&lcCount ,0))
      ENDFOR
    ENDSCAN
  ENDDO
  SELECT STYLE
	*N000606,1 NNA 10/06/2007 (Begin) Check if we are in PO from OTS or Not
	IF loFormSet.lcChoice <>'O' THEN
	*N000606,1 NNA (End)

	  m.PLAN1 = MAX(m.PLAN1-IIF(lcRpBaseOn='P',lnQty1,(Stk1+WIP1)-ORD1),0)
	  m.PLAN2 = MAX(m.PLAN2-IIF(lcRpBaseOn='P',lnQty2,(Stk2+WIP2)-ORD2),0)
	  m.PLAN3 = MAX(m.PLAN3-IIF(lcRpBaseOn='P',lnQty3,(Stk3+WIP3)-ORD3),0)
	  m.PLAN4 = MAX(m.PLAN4-IIF(lcRpBaseOn='P',lnQty4,(Stk4+WIP4)-ORD4),0)
	  m.PLAN5 = MAX(m.PLAN5-IIF(lcRpBaseOn='P',lnQty5,(Stk5+WIP5)-ORD5),0)
	  m.PLAN6 = MAX(m.PLAN6-IIF(lcRpBaseOn='P',lnQty6,(Stk6+WIP6)-ORD6),0)
	  m.PLAN7 = MAX(m.PLAN7-IIF(lcRpBaseOn='P',lnQty7,(Stk7+WIP7)-ORD7),0)
	  m.PLAN8 = MAX(m.PLAN8-IIF(lcRpBaseOn='P',lnQty8,(Stk8+WIP8)-ORD8),0)
	  m.TOTPLAN = m.PLAN1+m.PLAN2+m.PLAN3+m.PLAN4+m.PLAN5+m.PLAN6+m.PLAN7+m.PLAN8

  *N000606,1 NNA 10/06/2007 (Begin) Check if we are in PO from OTS or Not
  ELSE
	  m.PLAN1   = MIN((STK1+WIP1)-ORD1,0)
	  m.PLAN2   = MIN((STK2+WIP2)-ORD2,0)
	  m.PLAN3   = MIN((STK3+WIP3)-ORD3,0)
	  m.PLAN4   = MIN((STK4+WIP4)-ORD4,0)
	  m.PLAN5   = MIN((STK5+WIP5)-ORD5,0)
	  m.PLAN6   = MIN((STK6+WIP6)-ORD6,0)
	  m.PLAN7   = MIN((STK7+WIP7)-ORD7,0)
	  m.PLAN8   = MIN((STK8+WIP8)-ORD8,0)
	  m.TOTPLAN = m.PLAN1+m.PLAN2+m.PLAN3+m.PLAN4+m.PLAN5+m.PLAN6+m.PLAN7+m.PLAN8
    IF m.TOTPLAN < 0
	    llContinue = .T.
	    m.PLAN1   = ABS(m.PLAN1)
	    m.PLAN2   = ABS(m.PLAN2)
	    m.PLAN3   = ABS(m.PLAN3)
	    m.PLAN4   = ABS(m.PLAN4)
	    m.PLAN5   = ABS(m.PLAN5)
	    m.PLAN6   = ABS(m.PLAN6)
	    m.PLAN7   = ABS(m.PLAN7)
	    m.PLAN8   = ABS(m.PLAN8)
	    m.TOTPLAN = m.PLAN1+m.PLAN2+m.PLAN3+m.PLAN4+m.PLAN5+m.PLAN6+m.PLAN7+m.PLAN8
    ENDIF
	ENDIF
	*N000606,1 NNA (End)
	
  IF m.TOTPLAN > 0
    INSERT INTO (loFormSet.lcStyTmp) FROM MEMVAR
    RETURN .T.
  ENDIF
  RETURN .F.

  *!*************************************************************
  *! Name      : lfStyleOts
  *! Developer : Wael Ali MOhamed
  *! Date      : 06/30/2005
  *! Purpose   : Get style OTS quantitiy
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfStyleOTS(loFormSet)
  *!*************************************************************

FUNCTION lfStyleOTS
  LPARAMETERS loFormSet
  SELECT STYLE
  SCATTER MEMVAR
  IF !EMPTY(loParentForm.lcWarehouse)
    SELECT STYDYE
    =SEEK(STYLE.STYLE+loParentForm.lcWarehouse)
  ENDIF

  m.PLAN1 = MIN(Stk1+WIP1-ORD1,0)
  m.PLAN2 = MIN(Stk2+WIP2-ORD2,0)
  m.PLAN3 = MIN(Stk3+WIP3-ORD3,0)
  m.PLAN4 = MIN(Stk4+WIP4-ORD4,0)
  m.PLAN5 = MIN(Stk5+WIP5-ORD5,0)
  m.PLAN6 = MIN(Stk6+WIP6-ORD6,0)
  m.PLAN7 = MIN(Stk7+WIP7-ORD7,0)
  m.PLAN8 = MIN(Stk8+WIP8-ORD8,0)
  m.TOTPLAN = m.PLAN1+m.PLAN2+m.PLAN3+m.PLAN4+m.PLAN5+m.PLAN6+m.PLAN7+m.PLAN8

  IF m.TOTPLAN > 0
    INSERT INTO (loFormSet.lcStyTmp) FROM MEMVAR
    RETURN .T.
  ENDIF
  RETURN .F.

  *!*************************************************************
  *! Name      : lfStyWhen
  *! Developer : Samah Wilson
  *! Date      : 03/22/1998
  *! Purpose   : Called from the when of the style browse
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfStyWhen()
  *!*************************************************************
FUNCTION lfStyWhen
  LPARAMETERS loFormSet
  *N000606,1 NNA 10/06/2007 (Begin) Get lnColorLen and lnColorStr from Loformset not from loParentForm
  *lnColorLen = loParentForm.lnColorLen
  *lnColorStr = loParentForm.lnColorStr
  lnColorLen = loFormSet.lnColorLen
  lnColorStr = loFormSet.lnColorStr
  *N000606,1 NNA (End)
  WITH loFormSet.AriaForm1
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.cmdselect.CAPTION = IIF(EVALUATE(loFormSet.lcStyTmp+'.lSelect'),LANG_MFGENCT_UNSELECT,LANG_MFGENCT_SELECT)
.cmdselect.CAPTION = IIF(EVALUATE(loFormSet.lcStyTmp+'.lSelect'),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_UNSELECT,loFormSet.GetHeaderText("LANG_MFGENCT_UNSELECT",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_SELECT,loFormSet.GetHeaderText("LANG_MFGENCT_SELECT",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    .cmdselect.ENABLED = loFormSet.ActiveMode='E' AND !loFormSet.llAllocate
    = loFormSet.loStyle.SEEK(EVALUATE(loFormSet.lcStyTmp+'.Style')) AND loFormSet.loScale.SEEK('S'+STYLE.SCALE)
    .txtScaleDesc.VALUE = SCALE.cScl_Desc
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      .txtSize&lcCount..VALUE = EVALUATE('Scale.SZ'+lcCount)
    ENDFOR
  ENDWITH

  *** Convert this function in Phase II.
  *!*************************************************************
  *! Name      : lfUpdCtPlan
  *! Developer : Samah Wilson
  *! Date      : 03/22/1998
  *! Purpose   : Update the tmp files in case of generate CT from plan
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfUpdCtPlan()
  *!*************************************************************
FUNCTION lfUpdCtPlan
  LPARAMETERS loFormSet
  PRIVATE lnAlias,lnTotQty,lnEstCost1,lnEstCost2,lnEstCost3,lnEstCost4,lnEstCost5,lnEstCost6,lnEstCost7,lnLineNo,lcCutTKt

  lnAlias = SELECT()
  PRIVATE loBomHeader
  loBomHeader = CREATEOBJECT('RemoteTable','BomHeadr','BomHeadr','BomHeadr',loFormSet.DATASESSIONID)

  lcStyTmp = loFormSet.lcStyTmp
  lcPoHdr  = loFormSet.lcPOH
  SELECT (lcStyTmp)
  SET ORDER TO TAG 'TICKET'
  =SEEK("�")
  DO WHILE cSelect+cWeek+cYear+cDivision+SEASON+STYLE = "�"
    lcMajor = PADR(SUBSTR(STYLE,1,loFormSet.lnMajorLen),19)
    =loFormSet.loStyle.SEEK(STYLE)
    loBomHeader.SEEK('0001'+lcMajor+'M')
    ldForDate = lfvWeek(cYear,VAL(cWeek))
    ldStart = IIF(loFormSet.lcRpBaseOn='T',ldForDate-STYLE.leadtime,oAriaApplication.systemdate)
    ldComplete = IIF(loFormSet.lcRpBaseOn='T',ldForDate,oAriaApplication.systemdate+60)
    SELECT BomHeadr
    LOCATE REST WHILE cinvtype+cItmMajor+cCstShtTyp+cCstSht_Id = '0001'+lcMajor+'M' FOR lDefCstSht
    lcCstSht_ID = cCstSht_Id

    SELECT (lcPoHdr)
    APPEND BLANK
    REPLACE STYLE     WITH lcMajor ,;
      SEASON    WITH STYLE.SEASON   ,;
      cDivision WITH STYLE.cDivision,;
      PATTERN   WITH STYLE.PATTERN  ,;
      cWareCode WITH STYLE.cDefWare ,;
      STATUS    WITH loFormSet.lcRpCtStat,;
      ENTERED   WITH oAriaApplication.systemdate  ,;
      START     WITH ldStart ,;
      COMPLETE  WITH ldComplete  ,;
      cCstSht_Id WITH lcCstSht_ID ,;
      cWeek      WITH &lcStyTmp..cWeek ,;
      cYear      WITH &lcStyTmp..cYear
    SELECT (lcStyTmp)
    llMultiWare = .F.
    lcCutTKt = cWeek+cYear+cDivision+SEASON+ALLTRIM(lcMajor)
    STORE 0 TO lnTotQty,lnEstCost1,lnEstCost2,lnEstCost3,lnEstCost4,lnEstCost5,lnEstCost6,lnEstCost7,lnLineNo
    SCAN REST WHILE cSelect+cWeek+cYear+cDivision+SEASON+STYLE = "�"+lcCutTKt
      =loFormSet.loStyle.SEEK(STYLE)
      lnLineNo = lnLineNo + 1

      SELECT(loFormSet.lcPOLine)
      APPEND BLANK
      REPLACE cbusdocu  WITH 'P' ,;
        cstytype  WITH 'U' ,;
        cinvtype  WITH '0001' ,;
        TRANCD    WITH '1'  ,;
        LINENO    WITH lnLineNo ,;
        cStyGrade WITH STYLE.cStyGrade ,;
        STYLE     WITH STYLE.STYLE  ,;
        cDivision WITH STYLE.cDivision ,;
        SEASON    WITH STYLE.SEASON ,;
        SCALE     WITH STYLE.SCALE ,;
        cWeek     WITH &lcStyTmp..cWeek ,;
        cYear     WITH &lcStyTmp..cYear ,;
        cWareCode WITH STYLE.cDefWare ,;
        COMPLETE  WITH &lcPoHdr..COMPLETE ,;
        cCstSht_Id WITH lcCstSht_ID,;
        Qty1      WITH &lcStyTmp..PLAN1 ,;
        Qty2      WITH &lcStyTmp..PLAN2 ,;
        Qty3      WITH &lcStyTmp..PLAN3 ,;
        Qty4      WITH &lcStyTmp..PLAN4 ,;
        Qty5      WITH &lcStyTmp..PLAN5 ,;
        Qty6      WITH &lcStyTmp..PLAN6 ,;
        Qty7      WITH &lcStyTmp..PLAN7 ,;
        Qty8      WITH &lcStyTmp..PLAN8 ,;
        TotQty    WITH &lcStyTmp..TOTPLAN
      REPLACE nFCost1 WITH TotQty * BomHeadr.nCost1 ,;
        nFCost2 WITH TotQty * BomHeadr.nCost2 ,;
        nFCost3 WITH TotQty * BomHeadr.nCost3 ,;
        nFCost4 WITH TotQty * BomHeadr.nCost4 ,;
        nFCost5 WITH TotQty * BomHeadr.nCost5 ,;
        nFCost6 WITH TotQty * BomHeadr.nCost6 ,;
        nFCost7 WITH TotQty * BomHeadr.nCost7 ,;
        nICost1 WITH nFCost1 ,;
        nICost2 WITH nFCost2 ,;
        nICost3 WITH nFCost3 ,;
        nICost4 WITH nFCost4 ,;
        nICost5 WITH nFCost5 ,;
        nICost6 WITH nFCost6 ,;
        nICost7 WITH nFCost7

      lnTotQty   = lnTotQty   + TotQty
      lnEstCost1 = lnEstCost1 + TotQty * BomHeadr.nCost1
      lnEstCost2 = lnEstCost2 + TotQty * BomHeadr.nCost2
      lnEstCost3 = lnEstCost3 + TotQty * BomHeadr.nCost3
      lnEstCost4 = lnEstCost4 + TotQty * BomHeadr.nCost4
      lnEstCost5 = lnEstCost5 + TotQty * BomHeadr.nCost5
      lnEstCost6 = lnEstCost6 + TotQty * BomHeadr.nCost6
      lnEstCost7 = lnEstCost7 + TotQty * BomHeadr.nCost7
      llMultiWare= IIF(cWareCode <> &lcPoHdr..cWareCode,.T.,llMultiWare)
    ENDSCAN
    SELECT (lcPoHdr)
    REPLACE cbusdocu  WITH 'P' ,;
      cstytype  WITH 'U' ,;
      LastLine  WITH LastLine ,;
      nStyOrder WITH lnTotQty ,;
      nICost1 WITH lnEstCost1 ,;
      nICost2 WITH lnEstCost2 ,;
      nICost3 WITH lnEstCost3 ,;
      nICost4 WITH lnEstCost4 ,;
      nICost5 WITH lnEstCost5 ,;
      nICost6 WITH lnEstCost6 ,;
      nICost7 WITH lnEstCost7 ,;
      POTotal WITH nICost1+nICost2+nICost3+nICost4+nICost5+nICost6+nICost7,;
      TOTCOST WITH nICost1+nICost2+nICost3+nICost4+nICost5+nICost6+nICost7,;
      nFCost1 WITH nICost1 ,;
      nFCost2 WITH nICost2 ,;
      nFCost3 WITH nICost3 ,;
      nFCost4 WITH nICost4 ,;
      nFCost5 WITH nICost5 ,;
      nFCost6 WITH nICost6 ,;
      nFCost7 WITH nICost7 ,;
      lMultiWare WITH llMultiWare

    SELECT (lcStyTmp)
  ENDDO
  SET ORDER TO TAG (lcStyTmp) IN (lcStyTmp)
  loBomHeader.DESTROY
  SELECT(lnAlias)

  *!*************************************************************
  *! Name      : lfvNote
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/20/2004
  *! Purpose   : Calls the notes for the selected record of the generated P/Os or C/Ts
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfvNote()
  *!*************************************************************
FUNCTION lfvNote
  LPARAMETERS loFormSet

  IF loFormSet.ActiveMode = 'E'
    IF loFormSet.llAllocate
      =lfClrAllo(loFormSet)
      =lfvSelect('',loFormSet)
      WITH loFormSet.AriaForm1
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.cmdNotes.CAPTION    = LANG_MFGENCT_ALLOCATE
.cmdNotes.CAPTION    = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ALLOCATE,loFormSet.GetHeaderText("LANG_MFGENCT_ALLOCATE",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

        .cmdPO.ENABLED       = .F.
        .cmdGenerate.ENABLED = .F.
        oAriaApplication.otoolbar.ChangeButtonStatus('cmdScope','ENABLED')
      ENDWITH
      loFormSet.llAllocate = !loFormSet.llAllocate
      =lfOrdWhen(loFormSet)
    ELSE
      IF lfGetFab(loFormSet) AND lfFabPri(loFormSet) AND lfBaseOnFab(loFormSet)
        WITH loFormSet.AriaForm1
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.cmdNotes.CAPTION    = LANG_MFGENCT_CLEARALO
.cmdNotes.CAPTION    = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CLEARALO,loFormSet.GetHeaderText("LANG_MFGENCT_CLEARALO",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

          .cmdselect.ENABLED   = .F.
          .cmdSelAll.ENABLED   = .F.
          .cmdSelNone.ENABLED  = .F.
          .cmdInvert.ENABLED   = .F.
          .cmdPO.ENABLED       = .T.

          *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
          *IF !EMPTY(loFormSet.lcVendor) OR loFormSet.lcChoice <> 'P'
          IF !EMPTY(loFormSet.lcVendor) OR !INLIST(loFormSet.lcChoice,'P','O')
          *N000606,1 NNA (END)

            .cmdGenerate.ENABLED = .T.
          ENDIF
          oAriaApplication.otoolbar.ChangeButtonStatus('cmdScope','DISABLE')
        ENDWITH
        loFormSet.llAllocate = !loFormSet.llAllocate
      ENDIF
    ENDIF
  ELSE
    SELECT (loFormSet.lcHdrFile)
    IF EOF()
      GOTO TOP
    ENDIF
    LOCAL lcNoteType

    *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
    *IF loFormSet.lcChoice='P'
    IF INLIST(loFormSet.lcChoice,'P','O')
    *N000606,1 NNA (END)
      lcNoteType = 'P'
    ELSE
      lcNoteType = 'I'
    ENDIF
    =NotePad(lcNoteType,PO)
  ENDIF

  *!*************************************************************
  *! Name      : lfEditHdr
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/23/2004
  *! Purpose   : Called the screen of P/O or C/T to edit header for the generated records
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfEditHdr()
  *!*************************************************************
FUNCTION lfEditHdr
  LPARAMETERS loFormSet

  IF loFormSet.ActiveMode = 'E'
    =lfShowAll(loFormSet)
  ELSE
    SELECT(loFormSet.lcHdrFile)
    IF EOF()
      GOTO TOP
    ENDIF

    *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
    *IF loFormSet.lcChoice = 'P'
    IF INLIST(loFormSet.lcChoice ,'P','O')
    *N000606,1 NNA (END)

      lcParam1 = 'POSTY'
      lcParam2 = "'P','P','"
      lcParam3 = 'PO'
    ELSE
      lcParam1 = 'MFCUTKT'
      lcParam2 = "'"
      lcParam3 = 'MF'
    ENDIF
    lcParam1 = 'AWR'+lcParam1
    lcParam2 = lcParam2+EVALUATE(loFormSet.lcPOH+'.PO')+"'"
    =oAriaApplication.DoProgram(lcParam1,lcParam2,'',lcParam3)
  ENDIF

  *!*************************************************************
  *! Name      : lfClsScr
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 11/29/2004
  *! Purpose   : Revert & Close Screen
  *!*************************************************************
  *! Calls     : lfDelFiles(),lfUpdUnCmS()
  *!*************************************************************
  *! Parameters: none
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfClsScr()
  *!*************************************************************
PROCEDURE lfClsScr
  LPARAMETERS loFormSet
  IF loFormSet.ActiveMode $ "EA"
    RETURN
  ENDIF
  loFormSet.llRetToSel = .F.
  =lfDelFiles(.T.,loFormSet)

  *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
  *=IIF(loFormSet.lcChoice='L',lfStyWhen(loFormSet),lfOrdWhen(loFormSet))
  =IIF(loFormSet.lcChoice $ 'LO',lfStyWhen(loFormSet),lfOrdWhen(loFormSet))
  *N000606,1 NNA (End)

  =lfShow(loFormSet,loFormSet.ActiveMode)

  *!*************************************************************
  *! Name      : lfDelFiles
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 11/29/2004
  *! Purpose   : Delete temporary files
  *!*************************************************************
  *! Calls     : None
  *!*************************************************************
  *! Parameters: none
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfDelFiles()
  *!*************************************************************
FUNCTION lfDelFiles
  LPARAMETERS llDelOrders,loFormSet

  SELECT (loFormSet.lcHdrFile)
  ZAP
  IF USED(loFormSet.lcLinFile)
    SELECT (loFormSet.lcLinFile)
    ZAP
  ENDIF
  IF llDelOrders
    SELECT (loFormSet.lcSelFile)
    ZAP
  ENDIF
  IF INLIST(loFormSet.lcChoice,'C','P') AND USED(loFormSet.lcCutPick)
    SELECT (loFormSet.lcCutPick)
    ZAP
  ENDIF

  ****This function will be done in Phase II
  *!*************************************************************
  *! Name      : lfvCTPlan
  *! Developer : Wael Aly Mohamed
  *! Date      : 10/10/1998
  *! Purpose   : Validate cutting ticket from plan quantity
  *!*************************************************************
  *! Calls     : None
  *!*************************************************************
  *! Parameters: none
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfvCTPlan()
  *!*************************************************************
FUNCTION lfvCTPlan
  PRIVATE lcQtyFld, lnAddedQty

  lcQtyFld   = SYS(18)
  lnAddedQty = EVALUATE(lcQtyFld) - lnOldVal
  =RLOCK()
  REPLACE TotQty WITH TotQty + lnAddedQty,;
    nCost1 WITH nCost1 + lnAddedQty * STYLE.nMCost1,;
    nCost2 WITH nCost2 + lnAddedQty * STYLE.nMCost2,;
    nCost3 WITH nCost3 + lnAddedQty * STYLE.nMCost3,;
    nCost4 WITH nCost4 + lnAddedQty * STYLE.nMCost4,;
    nCost5 WITH nCost5 + lnAddedQty * STYLE.nMCost5
  UNLOCK
  SELECT (lcHdrFile)
  =RLOCK()
  REPLACE Pcs_Bud    WITH Pcs_Bud    + lnAddedQty ,;
    nEst_Cost1 WITH nEst_Cost1 + lnAddedQty * STYLE.nMCost1,;
    nEst_Cost2 WITH nEst_Cost2 + lnAddedQty * STYLE.nMCost2,;
    nEst_Cost3 WITH nEst_Cost3 + lnAddedQty * STYLE.nMCost3,;
    nEst_Cost4 WITH nEst_Cost4 + lnAddedQty * STYLE.nMCost4,;
    nEst_Cost5 WITH nEst_Cost5 + lnAddedQty * STYLE.nMCost5
  UNLOCK
  SELECT (lcLinFile)
  SHOW WINDOW (lcBrTtl1) REFRESH SAME

  *!*************************************************************
  *! Name      : lfCrtTemp
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 11/30/2004
  *! Purpose   : Create Temporary files
  *!*************************************************************
  *! Calls     : gfCrtTmp()
  *!*************************************************************
  *! Parameters: none
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfCrtTemp()
  *!*************************************************************
FUNCTION lfCrtTemp
  LPARAMETERS loFormSet

  *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
  *IF loFormSet.lcChoice <> 'L'
  IF !INLIST(loFormSet.lcChoice ,'L','O')
  *N000606,1 NNA (End)
    SELECT ORDLINE
    =AFIELDS(laFilField)
    lnAlen = ALEN(laFilField,1)
    DIMENSION laFilField[lnAlen+21,18]
    laFilField[lnAlen+1,1] = 'cSelect'
    laFilField[lnAlen+1,2] = 'C'
    laFilField[lnAlen+1,3] = 1
    laFilField[lnAlen+1,4] = 0
    laFilField[lnAlen+2,1] = 'cSortExp'
    laFilField[lnAlen+2,2] = 'C'
    laFilField[lnAlen+2,3] = 30
    laFilField[lnAlen+2,4] = 0
    laFilField[lnAlen+3,1] = 'cDivision'
    laFilField[lnAlen+3,2] = 'C'
    laFilField[lnAlen+3,3] = 6
    laFilField[lnAlen+3,4] = 0
    laFilField[lnAlen+4,1] = 'cPurCode'
    laFilField[lnAlen+4,2] = 'C'
    laFilField[lnAlen+4,3] = 6
    laFilField[lnAlen+4,4] = 0
    laFilField[lnAlen+5,1] = 'cStyGrade'
    laFilField[lnAlen+5,2] = 'C'
    laFilField[lnAlen+5,3] = 1
    laFilField[lnAlen+5,4] = 0
    laFilField[lnAlen+6,1] = 'Fabric'
    laFilField[lnAlen+6,2] = 'C'
    laFilField[lnAlen+6,3] = 19
    laFilField[lnAlen+6,4] = 0
    laFilField[lnAlen+7,1] = 'lSelect'
    laFilField[lnAlen+7,2] = 'L'
    laFilField[lnAlen+7,3] = 1
    laFilField[lnAlen+7,4] = 0
    laFilField[lnAlen+8,1] = 'cFabWare'
    laFilField[lnAlen+8,2] = 'C'
    laFilField[lnAlen+8,3] = 6
    laFilField[lnAlen+8,4] = 0
    laFilField[lnAlen+9,1] = 'nRequired'
    laFilField[lnAlen+9,2] = 'N'
    laFilField[lnAlen+9,3] = 12
    laFilField[lnAlen+9,4] = 3
    laFilField[lnAlen+10,1] = 'nWIPUsed'
    laFilField[lnAlen+10,2] = 'N'
    laFilField[lnAlen+10,3] = 12
    laFilField[lnAlen+10,4] = 3
    laFilField[lnAlen+11,1] = 'nYeild'
    laFilField[lnAlen+11,2] = 'N'
    laFilField[lnAlen+11,3] = 7
    laFilField[lnAlen+11,4] = 3
    laFilField[lnAlen+12,1] = 'BtName'
    laFilField[lnAlen+12,2] = 'C'
    laFilField[lnAlen+12,3] = 30
    laFilField[lnAlen+12,4] = 0
    laFilField[lnAlen+13,1] = 'nOpn1'
    laFilField[lnAlen+13,2] = 'N'
    laFilField[lnAlen+13,3] = 6
    laFilField[lnAlen+13,4] = 0
    laFilField[lnAlen+14,1] = 'nOpn2'
    laFilField[lnAlen+14,2] = 'N'
    laFilField[lnAlen+14,3] = 6
    laFilField[lnAlen+14,4] = 0
    laFilField[lnAlen+15,1] = 'nOpn3'
    laFilField[lnAlen+15,2] = 'N'
    laFilField[lnAlen+15,3] = 6
    laFilField[lnAlen+15,4] = 0
    laFilField[lnAlen+16,1] = 'nOpn4'
    laFilField[lnAlen+16,2] = 'N'
    laFilField[lnAlen+16,3] = 6
    laFilField[lnAlen+16,4] = 0
    laFilField[lnAlen+17,1] = 'nOpn5'
    laFilField[lnAlen+17,2] = 'N'
    laFilField[lnAlen+17,3] = 6
    laFilField[lnAlen+17,4] = 0
    laFilField[lnAlen+18,1] = 'nOpn6'
    laFilField[lnAlen+18,2] = 'N'
    laFilField[lnAlen+18,3] = 6
    laFilField[lnAlen+18,4] = 0
    laFilField[lnAlen+19,1] = 'nOpn7'
    laFilField[lnAlen+19,2] = 'N'
    laFilField[lnAlen+19,3] = 6
    laFilField[lnAlen+19,4] = 0
    laFilField[lnAlen+20,1] = 'nOpn8'
    laFilField[lnAlen+20,2] = 'N'
    laFilField[lnAlen+20,3] = 6
    laFilField[lnAlen+20,4] = 0
    laFilField[lnAlen+21,1] = 'nTotOpn'
    laFilField[lnAlen+21,2] = 'N'
    laFilField[lnAlen+21,3] = 7
    laFilField[lnAlen+21,4] = 0

    FOR lnI = 7 TO 16
      FOR lnJ = 1 TO 21
        laFilField[lnAlen+lnJ,lnI] = ''
      ENDFOR
    ENDFOR
    FOR lnJ = 1 TO 21
      STORE 0 TO laFilField[lnAlen+lnJ,17],laFilField[lnAlen+lnJ,18]
    ENDFOR

    DECLARE laIndex[3,2]
    laIndex[1,1] = IIF(loFormSet.lcChoice='C','CSELECT+CDIVISION+SEASON+STYLE+DYELOT',;
      'CSELECT+CDIVISION+CPURCODE+CSTYGRADE+STYLE+DYELOT')
    laIndex[1,2] = 'TICKET'
    laIndex[2,1] = 'cSortExp+ORDER+STR(LINENO,6)'
    laIndex[2,2] = loFormSet.lcOrdLine
    laIndex[3,1] = 'cSelect+Fabric+cSortExp+Order+Store+Group'
    laIndex[3,2] = 'Groups'
    =gfCrtTmp(loFormSet.lcOrdLine,@laFilField,@laIndex)
    SET ORDER TO TAG (loFormSet.lcOrdLine) IN (loFormSet.lcOrdLine)

    DIMENSION laFilField[lnAlen+22,18]
    laFilField[lnAlen+22,1] = 'StName'
    laFilField[lnAlen+22,2] = 'C'
    laFilField[lnAlen+22,3] = 25
    laFilField[lnAlen+22,4] = 0

    FOR lnI = 7 TO 16
      laFilField[lnAlen+22,lnI] = ''
    ENDFOR
    STORE 0 TO laFilField[lnAlen+22,17],laFilField[lnAlen+22,18]

    DECLARE laIndex[2,2]
    laIndex[1,1] = IIF(loFormSet.lcChoice='C','cSelect+cDivision+Season+Style+Dyelot',;
      'cSelect+cDivision+cPurCode+cStyGrade+Style+Dyelot')
    laIndex[1,2] = 'TICKET'
    laIndex[2,1] = 'Fabric+cWareCode+Order+Dyelot+STR(LineNO,6)'
    laIndex[2,2] = 'Fabrics'

    =gfCrtTmp(loFormSet.lcTmpOrd,@laFilField,@laIndex)

    DIMENSION laFabNdx[2,2]
    laFabNdx[1,1] = 'Fabric+cPriority+Dyelot+cWareCode'
    laFabNdx[1,2] = 'lcFabrics'
    laFabNdx[2,1] = 'Fabric+STR(OnHand,12,3)+cPriority+Dyelot+cWareCode'
    laFabNdx[2,2] = 'OnHand'

    =gfCrtTmp(loFormSet.lcFabrics,[(Fabric C(19),cWareCode C(6),Dyelot C(10),cPriority C(4),cNewPrior C(4),OnHand N(12,3),;
                                  nRequired N(12,3),nAllocated N(12,3),nUnAllWIP N(12,3),nWIPUsed N(12,3))],@laFabNdx)

    =gfCrtTmp(loFormSet.lcStyUnAll,[(Style C(19),Dyelot C(10),nUnAllWIP N(12,3),nWIPUsed N(12,3))],;
      [Style+Dyelot],loFormSet.lcStyUnAll)

    =gfCrtTmp(loFormSet.lcOrdGroup,[(Fabric C(19),Order C(6),STORE C(8),GROUP C(1),nRequired N(12,3),nUnAllWIP N(12,3),;
                                   CWareCode C(6),Dyelot C(10))],[Fabric+Order+Store+Group],loFormSet.lcOrdGroup)
  ENDIF
  SELECT POSHDR
  =AFIELDS(laFilField)
  lnAlen = ALEN(laFilField,1)

  *B608873,1 WAM 05/25/2009 Read project template code in the PO grid
  *DIMENSION laFilField[lnAlen+6,18]
  DIMENSION laFilField[lnAlen+7,18]
  *B608873,1 WAM 05/25/2009 (End)
  laFilField[lnAlen+1,1] = 'nSteps'
  laFilField[lnAlen+1,2] = 'N'
  laFilField[lnAlen+1,3] = 2
  laFilField[lnAlen+1,4] = 0
  laFilField[lnAlen+2,1] = 'cTmpPo'
  laFilField[lnAlen+2,2] = 'C'
  laFilField[lnAlen+2,3] = 6
  laFilField[lnAlen+2,4] = 0
  laFilField[lnAlen+3,1] = 'TotCost'
  laFilField[lnAlen+3,2] = 'N'
  laFilField[lnAlen+3,3] = 14
  laFilField[lnAlen+3,4] = 3
  laFilField[lnAlen+4,1] = 'cCstSht_ID'
  laFilField[lnAlen+4,2] = 'C'
  laFilField[lnAlen+4,3] = 6
  laFilField[lnAlen+4,4] = 0
  laFilField[lnAlen+5,1] = 'CWEEK'
  laFilField[lnAlen+5,2] = 'C'
  laFilField[lnAlen+5,3] = 2
  laFilField[lnAlen+5,4] = 0
  laFilField[lnAlen+6,1] = 'CYEAR'
  laFilField[lnAlen+6,2] = 'C'
  laFilField[lnAlen+6,3] = 4
  laFilField[lnAlen+6,4] = 0

  *B608873,1 WAM 05/25/2009 Read project template code in the PO grid
  laFilField[lnAlen+7,1] = 'CDEFTEMPL'
  laFilField[lnAlen+7,2] = 'C'
  laFilField[lnAlen+7,3] = 4
  laFilField[lnAlen+7,4] = 0
  *B608873,1 WAM 05/25/2009 (End)

  FOR lnI = 7 TO 16
    *B608873,1 WAM 05/25/2009 Read project template code in the PO grid
    *FOR lnJ = 1 TO 6
    FOR lnJ = 1 TO 7
    *B608873,1 WAM 05/25/2009 (End)
      laFilField[lnAlen+lnJ,lnI] = ''
    ENDFOR
  ENDFOR
  *B608873,1 WAM 05/25/2009 Read project template code in the PO grid
  *FOR lnJ = 1 TO 6
  FOR lnJ = 1 TO 7
  *B608873,1 WAM 05/25/2009 (End)
    STORE 0 TO laFilField[lnAlen+lnJ,17],laFilField[lnAlen+lnJ,18]
  ENDFOR
  IF INLIST(loFormSet.lcChoice,'C','L')
    =gfCrtTmp(loFormSet.lcPOH,@laFilField,[PO+CDIVISION+SEASON+CWEEK+CYEAR+STYLE],[CSTYLE])
  ELSE
    *B608873,1 WAM 05/25/2009 Add new option to generate one PO per each style major
    *=gfCrtTmp(loFormSet.lcPOH,@laFilField,[PO+CDIVISION+CPURCODE+CSTYGRADE],[CVENDIV])
    =gfCrtTmp(loFormSet.lcPOH,@laFilField,[PO+CDIVISION+CPURCODE+CSTYGRADE+STYLE],[CVENDIV])
    *B608873,1 WAM 05/25/2009 (End)
  ENDIF
  IF loFormSet.lcGenProj $ 'AI'
    =lfCrtprjTm(loFormSet)
  ENDIF
  *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
  *IF loFormSet.lcChoice = 'L'
  IF INLIST(loFormSet.lcChoice , 'L','O')
  *N000606,1 NNA (End)
    *! B609767,1 SAB 12/11/2011 Fix error when run the Generate PO from open to sell [Start]
    *SELECT STYLE
    *=AFIELDS(laFilField)
    *lnAlen = ALEN(laFilField,1)
    *DIMENSION laFilField[lnAlen+4,18]
    *laFilField[lnAlen+1,1] = 'cSelect'
    *laFilField[lnAlen+1,2] = 'C'
    *laFilField[lnAlen+1,3] = 1
    *laFilField[lnAlen+1,4] = 0
    *laFilField[lnAlen+2,1] = 'lSelect'
    *laFilField[lnAlen+2,2] = 'L'
    *laFilField[lnAlen+2,3] = 1
    *laFilField[lnAlen+2,4] = 0
    *laFilField[lnAlen+3,1] = 'CWEEK'
    *laFilField[lnAlen+3,2] = 'C'
    *laFilField[lnAlen+3,3] = 2
    *laFilField[lnAlen+3,4] = 0
    *laFilField[lnAlen+4,1] = 'CYEAR'
    *laFilField[lnAlen+4,2] = 'C'
    *laFilField[lnAlen+4,3] = 4
    *laFilField[lnAlen+4,4] = 0

	  *N000606,1 NNA 10/06/2007 (Begin) Add Fields for the PO sizes' qty.
	  *!*	  FOR lnI = 7 TO 16
	  *!*	    FOR lnJ = 1 TO 4
	  *!*	      lafilfield[lnAlen+lnJ,lnI] = ''
	  *!*	    ENDFOR
	  *!*	  ENDFOR
	  *!*	  FOR lnJ = 1 TO 4
	  *!*	    STORE 0 TO lafilfield[lnAlen+lnJ,17],lafilfield[lnAlen+lnJ,18]
	  *!*	  ENDFOR

	*  lnAlen = ALEN(lafilfield,1)
	*  DIMENSION lafilfield[lnAlen+9,18]
	*  FOR lnI=1 TO 9
	*    lafilfield[lnAlen+lnI,1] = 'nPoQty'+STR(lnI,1)
	*    lafilfield[lnAlen+lnI,2] = 'N'
	*    lafilfield[lnAlen+lnI,3] = 6
	*    lafilfield[lnAlen+lnI,4] = 0
	*    IF lnI=9 THEN
	*      lafilfield[lnAlen+lnI,1] = 'nPoTotQty'
	*      lafilfield[lnAlen+lnI,2] = 'N'
	*      lafilfield[lnAlen+lnI,3] = 6
	*      lafilfield[lnAlen+lnI,4] = 0
	*    ENDIF
	*  ENDFOR
	*  lnAlen = lnAlen-4
	*  FOR lnI = 7 TO 16
	*    FOR lnJ = 1 TO 13
	*      lafilfield[lnAlen+lnJ,lnI] = ''
	*    ENDFOR
	*  ENDFOR
	*  FOR lnJ = 1 TO 4
	*    STORE 0 TO lafilfield[lnAlen+lnJ,17],lafilfield[lnAlen+lnJ,18]
	*  ENDFOR
	*  *N000606,1 NNA (End)


    LOCAL lnIndex
    lnIndex = 0
    DIMENSION laFilField[30, 18]

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'STYLE'
    laFilField[lnIndex, 2] = 'C'
    laFilField[lnIndex, 3] = 19
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'CDIVISION'
    laFilField[lnIndex, 2] = 'C'
    laFilField[lnIndex, 3] = 6
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'CPURCODE'
    laFilField[lnIndex, 2] = 'C'
    laFilField[lnIndex, 3] = 6
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'CSTYGRADE'
    laFilField[lnIndex, 2] = 'C'
    laFilField[lnIndex, 3] = 1
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'SEASON'
    laFilField[lnIndex, 2] = 'C'
    laFilField[lnIndex, 3] = 6
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'PLAN1'
    laFilField[lnIndex, 2] = 'N'
    laFilField[lnIndex, 3] = 7
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'PLAN2'
    laFilField[lnIndex, 2] = 'N'
    laFilField[lnIndex, 3] = 7
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'PLAN3'
    laFilField[lnIndex, 2] = 'N'
    laFilField[lnIndex, 3] = 7
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'PLAN4'
    laFilField[lnIndex, 2] = 'N'
    laFilField[lnIndex, 3] = 7
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'PLAN5'
    laFilField[lnIndex, 2] = 'N'
    laFilField[lnIndex, 3] = 7
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'PLAN6'
    laFilField[lnIndex, 2] = 'N'
    laFilField[lnIndex, 3] = 7
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'PLAN7'
    laFilField[lnIndex, 2] = 'N'
    laFilField[lnIndex, 3] = 7
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'PLAN8'
    laFilField[lnIndex, 2] = 'N'
    laFilField[lnIndex, 3] = 7
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'TOTPLAN'
    laFilField[lnIndex, 2] = 'N'
    laFilField[lnIndex, 3] = 7
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'cSelect'
    laFilField[lnIndex, 2] = 'C'
    laFilField[lnIndex, 3] = 1
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'lSelect'
    laFilField[lnIndex, 2] = 'L'
    laFilField[lnIndex, 3] = 1
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'CWEEK'
    laFilField[lnIndex, 2] = 'C'
    laFilField[lnIndex, 3] = 2
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'CYEAR'
    laFilField[lnIndex, 2] = 'C'
    laFilField[lnIndex, 3] = 4
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'nPoQty1'
    laFilField[lnIndex, 2] = 'N'
    laFilField[lnIndex, 3] = 6
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'nPoQty2'
    laFilField[lnIndex, 2] = 'N'
    laFilField[lnIndex, 3] = 6
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'nPoQty3'
    laFilField[lnIndex, 2] = 'N'
    laFilField[lnIndex, 3] = 6
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'nPoQty4'
    laFilField[lnIndex, 2] = 'N'
    laFilField[lnIndex, 3] = 6
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'nPoQty5'
    laFilField[lnIndex, 2] = 'N'
    laFilField[lnIndex, 3] = 6
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'nPoQty6'
    laFilField[lnIndex, 2] = 'N'
    laFilField[lnIndex, 3] = 6
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'nPoQty7'
    laFilField[lnIndex, 2] = 'N'
    laFilField[lnIndex, 3] = 6
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'nPoQty8'
    laFilField[lnIndex, 2] = 'N'
    laFilField[lnIndex, 3] = 6
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'nPoTotQty'
    laFilField[lnIndex, 2] = 'N'
    laFilField[lnIndex, 3] = 6
    laFilField[lnIndex, 4] = 0

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'PRICEA'
    laFilField[lnIndex, 2] = 'N'
    laFilField[lnIndex, 3] = 12
    laFilField[lnIndex, 4] = 2

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'PRICEB'
    laFilField[lnIndex, 2] = 'N'
    laFilField[lnIndex, 3] = 12
    laFilField[lnIndex, 4] = 2

    lnIndex = lnIndex + 1
    laFilField[lnIndex, 1] = 'PRICEC'
    laFilField[lnIndex, 2] = 'N'
    laFilField[lnIndex, 3] = 12
    laFilField[lnIndex, 4] = 2
    *! B609767,1 SAB 12/11/2011 Fix error when run the Generate PO from open to sell [End]

    DECLARE laIndex[2,2]

    *N000606,1 NNA 10/06/2007 (Begin) Chenage index in case of generate PO from TOS
    IF loFormSet.lcChoice ='O' THEN
      laIndex[1,1] = 'CSELECT+CDIVISION+CPURCODE+CSTYGRADE+STYLE'
    ELSE
    *N000606,1 NNA (End)

      laIndex[1,1] = 'CSELECT+CWEEK+CYEAR+CDIVISION+SEASON+STYLE'

    *N000606,1 NNA (Begin)
    ENDIF
    *N000606,1 NNA (End)

    laIndex[1,2] = 'TICKET'
    laIndex[2,1] = 'STYLE'
    laIndex[2,2] = loFormSet.lcStyTmp
    =gfCrtTmp(loFormSet.lcStyTmp,@laFilField,@laIndex)
    SET ORDER TO TAG (loFormSet.lcStyTmp) IN (loFormSet.lcStyTmp)
  ENDIF
  *!*************************************************************
  *! Name      : lfCrtDTemp
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/08/2004
  *! Purpose   : Create Temporary details files
  *!*************************************************************
  *! Calls     : gfCrtTmp()
  *!*************************************************************
  *! Parameters: none
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfCrtDTemp()
  *!*************************************************************
FUNCTION lfCrtDTemp
  LPARAMETERS loFormSet

  *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
  *IF loFormSet.lcChoice $ 'LC' AND !USED(loFormSet.lcPOLine)

  *B608675,1 WAM 09/02/2008 Fix bug while generate CT from SO results in create CT without lines
  *IF loFormSet.lcChoice $ 'LCO' AND !USED(loFormSet.lcPOLine)
  IF loFormSet.lcChoice $ 'LC'
  *B608675,1 WAM 09/02/2008 (End)

  *N000606,1 NNA (End)

    loFormSet.loPOSLN = IIF(TYPE('loFormSet.loPOSLN')='O',loFormSet.loPOSLN,CREATEOBJECT('RemoteTable','POSLN','POSLN','POSLN',loFormSet.DATASESSIONID))
    SELECT POSLN
    =AFIELDS(laFilField)
    lnAlen = ALEN(laFilField,1)

    *N000606,1 NNA 10/06/2007 (Begin) Extend the Array to add a new filed called 'cPurcode'
    *DIMENSION lafilfield[lnAlen+8,18]

    *B608675,1 WAM 09/02/2008 Fix bug while generate CT from SO results in create CT without lines
    *DIMENSION lafilfield[lnAlen+9,18]

    *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[Start]
    *DIMENSION lafilfield[lnAlen+8,18]
    DIMENSION lafilfield[lnAlen+9,18]
    *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[End]

    *B608675,1 WAM 09/02/2008 (End)

    *N000606,1 NNA (End)

    laFilField[lnAlen+1,1] = 'CDIVISION'
    laFilField[lnAlen+1,2] = 'C'
    laFilField[lnAlen+1,3] = 6
    laFilField[lnAlen+1,4] = 0
    laFilField[lnAlen+2,1] = 'SEASON'
    laFilField[lnAlen+2,2] = 'C'
    laFilField[lnAlen+2,3] = 6
    laFilField[lnAlen+2,4] = 0
    laFilField[lnAlen+3,1] = 'nSteps'
    laFilField[lnAlen+3,2] = 'N'
    laFilField[lnAlen+3,3] = 2
    laFilField[lnAlen+3,4] = 0
    laFilField[lnAlen+4,1] = 'Fabric'
    laFilField[lnAlen+4,2] = 'C'
    laFilField[lnAlen+4,3] = 19
    laFilField[lnAlen+4,4] = 0
    laFilField[lnAlen+5,1] = 'cFabWare'
    laFilField[lnAlen+5,2] = 'C'
    laFilField[lnAlen+5,3] = 6
    laFilField[lnAlen+5,4] = 0
    laFilField[lnAlen+6,1] = 'nYeild'
    laFilField[lnAlen+6,2] = 'N'
    laFilField[lnAlen+6,3] = 7
    laFilField[lnAlen+6,4] = 3
    laFilField[lnAlen+7,1] = 'CWEEK'
    laFilField[lnAlen+7,2] = 'C'
    laFilField[lnAlen+7,3] = 2
    laFilField[lnAlen+7,4] = 0
    laFilField[lnAlen+8,1] = 'CYEAR'
    laFilField[lnAlen+8,2] = 'C'
    laFilField[lnAlen+8,3] = 4
    laFilField[lnAlen+8,4] = 0

  *B608675,1 WAM 09/02/2008 Fix bug while generate CT from SO results in create CT without lines
*!*	    *N000606,1 NNA 10/06/2007 (Begin) Add new filed for the purchase code
*!*	    lafilfield[lnAlen+9,1] = 'cPurCode'
*!*	    lafilfield[lnAlen+9,2] = 'C'
*!*	    lafilfield[lnAlen+9,3] = 6
*!*	    lafilfield[lnAlen+9,4] = 0
*!*	    *N000606,1 NNA (End)
  *B608675,1 WAM 09/02/2008 (End)

    *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[Start]
    lafilfield[lnAlen+9,1] = 'CDEFTEMPL'
    lafilfield[lnalen+9,2] = 'c'
    lafilfield[lnalen+9,3] = 4
	  lafilfield[lnalen+9,4] = 0
    *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[End]


    FOR lnI = 7 TO 16
     *N000606,1 NNA 10/06/2007 (Begin) Extend the for Loop to include the new field
      *FOR lnJ = 1 TO 8

      *B608675,1 WAM 09/02/2008 Fix bug while generate CT from SO results in create CT without lines
      *FOR lnJ = 1 TO 9

      *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[Start]
      *FOR lnJ = 1 TO 8
      FOR lnJ = 1 TO 9
      *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[End]

      *B608675,1 WAM 09/02/2008 (End)
      *N000606,1 NNA (End)
        laFilField[lnAlen+lnJ,lnI] = ''
      ENDFOR
    ENDFOR

    *N000606,1 NNA 10/06/2007 (Begin) Extend the for Loop to include the new field
    *FOR lnJ = 1 TO 8

    *B608675,1 WAM 09/02/2008 Fix bug while generate CT from SO results in create CT without lines
    *FOR lnJ = 1 TO 9

    *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[Start]
    *FOR lnJ = 1 TO 8
    FOR lnJ = 1 TO 9
        *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[End]

    *B608675,1 WAM 09/02/2008 (End)

    *N000606,1 NNA (End)

      STORE 0 TO laFilField[lnAlen+lnJ,17],laFilField[lnAlen+lnJ,18]
    ENDFOR

    *N000606,1 NNA 10/06/2007 (Begin) Change table index to be compatible with table's fileds
    *=gfCrtTmp(loFormSet.lcPOLine,@lafilfield,[PO+CDIVISION+SEASON+CWEEK+CYEAR+STYLE+DYELOT],[CSTYCLR])

    *B608675,1 WAM 09/02/2008 Fix bug while generate CT from SO results in create CT without lines
    *=gfCrtTmp(loFormSet.lcPOLine,@lafilfield,[PO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+CWARECODE+DYELOT],[CSTYCLR])
    =gfCrtTmp(loFormSet.lcPOLine,@lafilfield,[PO+CDIVISION+SEASON+CWEEK+CYEAR+STYLE+DYELOT],[CSTYCLR])
    *B608675,1 WAM 09/02/2008 (End)

    *N000606,1 NNA (End)

  ENDIF
  *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
  *IF loFormSet.lcChoice <> 'L' AND !USED(loFormSet.lcCutPick)

  *B608675,1 WAM 09/02/2008 Fix bug while generate CT from SO results in create CT without lines
  *IF !INLIST(loFormSet.lcChoice ,'L','O') AND !USED(loFormSet.lcCutPick)
  IF !INLIST(loFormSet.lcChoice ,'L','O')
  *B608675,1 WAM 09/02/2008 (End)
  *N000606,1 NNA (End)

    loFormSet.loCUTPICK = IIF(TYPE('loFormSet.loCUTPICK')='O',loFormSet.loCUTPICK,CREATEOBJECT('RemoteTable','CUTPICK','CUTPICK','CUTPICK',loFormSet.DATASESSIONID))
    SELECT CUTPICK
    =AFIELDS(laFilField)
    lnAlen = ALEN(laFilField,1)
    DIMENSION laFilField[lnAlen+7,18]
    laFilField[lnAlen+1,1] = 'CDIVISION'
    laFilField[lnAlen+1,2] = 'C'
    laFilField[lnAlen+1,3] = 6
    laFilField[lnAlen+1,4] = 0
    laFilField[lnAlen+2,1] = 'SEASON'
    laFilField[lnAlen+2,2] = 'C'
    laFilField[lnAlen+2,3] = 6
    laFilField[lnAlen+2,4] = 0
    laFilField[lnAlen+3,1] = 'cPurCode'
    laFilField[lnAlen+3,2] = 'C'
    laFilField[lnAlen+3,3] = 6
    laFilField[lnAlen+3,4] = 0
    laFilField[lnAlen+4,1] = 'cStyGrade'
    laFilField[lnAlen+4,2] = 'C'
    laFilField[lnAlen+4,3] = 1
    laFilField[lnAlen+4,4] = 0
    laFilField[lnAlen+5,1] = 'CWARECODE'
    laFilField[lnAlen+5,2] = 'C'
    laFilField[lnAlen+5,3] = 6
    laFilField[lnAlen+5,4] = 0
    laFilField[lnAlen+6,1] = 'Dyelot'
    laFilField[lnAlen+6,2] = 'C'
    laFilField[lnAlen+6,3] = 10
    laFilField[lnAlen+6,4] = 0
    laFilField[lnAlen+7,1] = 'nSteps'
    laFilField[lnAlen+7,2] = 'N'
    laFilField[lnAlen+7,3] = 2
    laFilField[lnAlen+7,4] = 0

    FOR lnI = 7 TO 16
      FOR lnJ = 1 TO 7
        laFilField[lnAlen+lnJ,lnI] = ''
      ENDFOR
    ENDFOR
    FOR lnJ = 1 TO 7
      STORE 0 TO laFilField[lnAlen+lnJ,17],laFilField[lnAlen+lnJ,18]
    ENDFOR


    *C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [BEGIN]
    *DECLARE laIndex[1,2]
    *laIndex[1,1] = IIF(loFormSet.lcChoice='C','CTKTNO+CDIVISION+SEASON+STYLE+CWARECODE+DYELOT',;
    *                         'CTKTNO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+CWARECODE+DYELOT')
    *laIndex[1,2] = 'CSTYCLR'

    *608597,1 ALAA [Begin] Commnetd out to rename varibale llGrpSO to be llRPGrpSO
    *IF !loFormSet.llGrpSO
    IF !loFormSet.llRPGrpSO
     *608597,1 ALAA [END] Commnetd out to rename varibale llGrpSO to be llRPGrpSO
      DECLARE laIndex[2,2]
      laIndex[1,1] = IIF(loFormSet.lcChoice='C','CTKTNO+CDIVISION+SEASON+STYLE+CWARECODE+DYELOT',;
        'CTKTNO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+CWARECODE+DYELOT')

      laIndex[1,2] = 'CSTYCLR'

      laIndex[2,1] = IIF(loFormSet.lcChoice='C','CTKTNO+CTKTLINENO+CDIVISION+SEASON+STYLE+CWARECODE+DYELOT',;
        'CTKTNO+CTKTLINENO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+CWARECODE+DYELOT')
      laIndex[2,2] = 'CSTYLIN'
    ELSE
      DECLARE laIndex[1,2]
      laIndex[1,1] = IIF(loFormSet.lcChoice='C','CTKTNO+CDIVISION+SEASON+STYLE+CWARECODE+DYELOT',;
        'CTKTNO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+CWARECODE+DYELOT')

      laIndex[1,2] = 'CSTYCLR'
    ENDIF

    *C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [END]
    =gfCrtTmp(loFormSet.lcCutPick,@laFilField,@laIndex)
  ENDIF

  *B608675,1 WAM 09/02/2008 Fix bug while generate CT from SO results in create CT without lines
  *IF loFormSet.lcChoice = 'P' AND !USED(loFormSet.lcPOLine)
  IF INLIST(loFormSet.lcChoice ,'O', 'P')
  *B608675,1 WAM 09/02/2008 (End)

    loFormSet.loPOSLN = IIF(TYPE('loFormSet.loPOSLN')='O',loFormSet.loPOSLN,CREATEOBJECT('RemoteTable','POSLN','POSLN','POSLN',loFormSet.DATASESSIONID))
    SELECT POSLN
    =AFIELDS(laFilField)
    lnAlen = ALEN(laFilField,1)
    *! B609028,1 MMT 10/08/2009 Fix bug of can not edit p.price in line detail screen[Start]
    *DIMENSION laFilField[lnAlen+6,18]

    *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[Start]
    *DIMENSION laFilField[lnAlen+7,18]
    DIMENSION laFilField[lnAlen+8,18]
    *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[End]

    *! B609028,1 MMT 10/08/2009 Fix bug of can not edit p.price in line detail screen[End]
    laFilField[lnAlen+1,1] = 'CDIVISION'
    laFilField[lnAlen+1,2] = 'C'
    laFilField[lnAlen+1,3] = 6
    laFilField[lnAlen+1,4] = 0
    laFilField[lnAlen+2,1] = 'cPurCode'
    laFilField[lnAlen+2,2] = 'C'
    laFilField[lnAlen+2,3] = 6
    laFilField[lnAlen+2,4] = 0
    laFilField[lnAlen+3,1] = 'nSteps'
    laFilField[lnAlen+3,2] = 'N'
    laFilField[lnAlen+3,3] = 2
    laFilField[lnAlen+3,4] = 0
    laFilField[lnAlen+4,1] = 'Fabric'
    laFilField[lnAlen+4,2] = 'C'
    laFilField[lnAlen+4,3] = 19
    laFilField[lnAlen+4,4] = 0
    laFilField[lnAlen+5,1] = 'cFabWare'
    laFilField[lnAlen+5,2] = 'C'
    laFilField[lnAlen+5,3] = 6
    laFilField[lnAlen+5,4] = 0
    laFilField[lnAlen+6,1] = 'nYeild'
    laFilField[lnAlen+6,2] = 'N'
    laFilField[lnAlen+6,3] = 7
    laFilField[lnAlen+6,4] = 3
    *! B609028,1 MMT 10/08/2009 Fix bug of can not edit p.price in line detail screen[Start]
    laFilField[lnAlen+7,1] = 'nNetPrc'
    laFilField[lnAlen+7,2] = 'N'
    laFilField[lnAlen+7,3] = 13
    laFilField[lnAlen+7,4] = 3
    *! B609028,1 MMT 10/08/2009 Fix bug of can not edit p.price in line detail screen[End]

    *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[Start]
    lafilfield[lnAlen+8,1] = 'CDEFTEMPL'
    lafilfield[lnalen+8,2] = 'c'
    lafilfield[lnalen+8,3] = 4
    lafilfield[lnalen+8,4] = 0
    *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[End]

    FOR lnI = 7 TO 16

      *! B609028,1 MMT 10/08/2009 Fix bug of can not edit p.price in line detail screen[Start]
      *FOR lnJ = 1 TO 6

      *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[Start]
      *FOR lnJ = 1 TO 7
      FOR lnJ = 1 TO 8
      *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[End]

      *! B609028,1 MMT 10/08/2009 Fix bug of can not edit p.price in line detail screen[End]

        laFilField[lnAlen+lnJ,lnI] = ''
      ENDFOR
    ENDFOR

    *! B609028,1 MMT 10/08/2009 Fix bug of can not edit p.price in line detail screen[Start]
    *FOR lnJ = 1 TO 6

    *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[Start]
    *FOR lnJ = 1 TO 7
    FOR lnJ = 1 TO 8
    *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[End]

    *! B609028,1 MMT 10/08/2009 Fix bug of can not edit p.price in line detail screen[End]

      STORE 0 TO laFilField[lnAlen+lnJ,17],laFilField[lnAlen+lnJ,18]
    ENDFOR

    =gfCrtTmp(loFormSet.lcPOLine,@laFilField,[PO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+DYELOT],[CSTYCLR])
  ENDIF

  *!*************************************************************
  *! Name      : lfUpdCtPik
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 04/24/2005
  *! Purpose   : Generate C/Ts for selected order lines
  *!*************************************************************
  *! Calls     : lfUnAllCt()
  *!*************************************************************
  *! Parameters: lcOrdFile  : Order line temporary file name
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfUpdCtPik()
  *!*************************************************************
FUNCTION lfUpdCtPik

  LPARAMETERS lcOrdFile

  LOCAL lnAlias,lcStyOrd,lcStyMaj,lcDivision,lcSeason,lcStyle,lcDyelot,lnCost1,lnCost2,;
    lnCost3,lnCost4,lnCost5,lnCost6,lnCost7,lnOrder,lnAllocat,lnCutQty1,lnCutQty2,lnCutQty3,;
    lnCutQty4,lnCutQty5,lnCutQty6,lnCutQty7,lnCutQty8
  LOCAL lnOrdQty1,lnOrdQty2,lnOrdQty3,lnOrdQty4,lnOrdQty5,lnOrdQty6,lnOrdQty7,lnOrdQty8

  PRIVATE loBomHeader
  loBomHeader = CREATEOBJECT('RemoteTable','BomHeadr','BomHeadr','BomHeadr',loParentForm.DATASESSIONID)

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *WAIT LANG_MFGENCT_GENCTORD WINDOW NOWAIT
  WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_GENCTORD,loParentForm.GetHeaderText("LANG_MFGENCT_GENCTORD",loParentForm.HeaderAlias)) WINDOW NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[End]


  lnAlias = SELECT(0)
  SELECT STYLE
  lcStyOrd = TAG()
  loParentForm.loStyle.SETORDER('STYLE')
  SELECT (lcOrdFile)
  lcOrdTag = TAG()
  SET ORDER TO TAG TICKET
  =IIF(loParentForm.llRPUnAWip,lfUnAllCt(lcOrdFile),.T.)
  SELECT (lcOrdFile)
  =SEEK("�")
  DO WHILE cSelect+cDivision+SEASON+STYLE+Dyelot="�"
    lcStyMaj   = SUBSTR(STYLE,1,loParentForm.lnMajorLen)
    lcDivision = cDivision
    lcStyGrade = cStyGrade
    lcSeason   = SEASON
    loParentForm.loStyle.SEEK(lcStyMaj)

    SELECT (loParentForm.lcPOH)
    IF !SEEK(SPACE(6)+lcDivision+lcSeason+lcStyMaj)
      APPEND BLANK
      REPLACE cstytype   WITH 'U',;
        STATUS     WITH 'H',;
        cDivision  WITH lcDivision,;
        ENTERED    WITH oAriaApplication.systemdate,;
        START      WITH oAriaApplication.systemdate,;
        COMPLETE   WITH ENTERED+60,;
        cWareCode  WITH IIF(loParentForm.lnType=2 OR !loParentForm.llWareHous,STYLE.cDefWare,loParentForm.lcWareCode),;
        lMultiWare WITH (loParentForm.lnType=2),;
        STYLE      WITH lcStyMaj,;
        SEASON     WITH lcSeason,;
        cCstSht_Id WITH lfGetCstSht(lcStyMaj,loParentForm),;
        PATTERN    WITH STYLE.PATTERN,;
        cStyGrade  WITH lcStyGrade
    ENDIF
    STORE 0 TO lnCost1,lnCost2,lnCost3,lnCost4,lnCost5,lnCost6,lnCost7,lnOrder,lnAllocat

    IF loParentForm.llDispPric
      STORE 0 TO loParentForm.lnSelPrice,loParentForm.lnAllQty,loParentForm.lnRotSub,loParentForm.lnGrosMrgn
    ENDIF

    SELECT (lcOrdFile)
    DO WHILE cSelect+cDivision+SEASON+STYLE+Dyelot=;
        "�"+lcDivision+lcSeason+lcStyMaj
      lcStyle  = STYLE
      lcDyelot = Dyelot
      lcFabric = Fabric
      lcFabWare= cFabWare
      lnYeild  = nYeild
      loParentForm.loStyle.SEEK(lcStyle)
      STORE 0 TO lnCutQty1,lnCutQty2,lnCutQty3,lnCutQty4,lnCutQty5,lnCutQty6,lnCutQty7,lnCutQty8
      STORE 0 TO lnOrdQty1,lnOrdQty2,lnOrdQty3,lnOrdQty4,lnOrdQty5,lnOrdQty6,lnOrdQty7,lnOrdQty8
      SCAN REST WHILE cSelect+cDivision+SEASON+STYLE+Dyelot = ;
          "�"+lcDivision+lcSeason+lcStyle+lcDyelot ;
          FOR   IIF(loParentForm.llRpBOPFab,nRequired-nWIPUsed > 0,.T.)
        SELECT(loParentForm.lcCutPick)
        APPEND BLANK
        REPLACE TRANCD    WITH '1',;
          cDivision WITH lcDivision,;
          SEASON    WITH lcSeason,;
          ORDER     WITH EVALUATE(lcOrdFile+'.ORDER'),;
          cOrdLine  WITH STR(EVALUATE(lcOrdFile+'.LINENO'),6),;
          STYLE     WITH lcStyle,;
          Dyelot    WITH lcDyelot,;
          cWareCode WITH IIF(loParentForm.lnType=2 OR !loParentForm.llWareHous,STYLE.cDefWare,loParentForm.lcWareCode)

        REPLACE Qty1   WITH INT(MIN(loParentForm.laPercet[1],100)/100*(EVALUATE(lcOrdFile+'.QTY1-'+lcOrdFile+'.CUT1'))),;
          Qty2   WITH INT(MIN(loParentForm.laPercet[2],100)/100*(EVALUATE(lcOrdFile+'.QTY2-'+lcOrdFile+'.CUT2'))),;
          Qty3   WITH INT(MIN(loParentForm.laPercet[3],100)/100*(EVALUATE(lcOrdFile+'.QTY3-'+lcOrdFile+'.CUT3'))),;
          Qty4   WITH INT(MIN(loParentForm.laPercet[4],100)/100*(EVALUATE(lcOrdFile+'.QTY4-'+lcOrdFile+'.CUT4'))),;
          Qty5   WITH INT(MIN(loParentForm.laPercet[5],100)/100*(EVALUATE(lcOrdFile+'.QTY5-'+lcOrdFile+'.CUT5'))),;
          Qty6   WITH INT(MIN(loParentForm.laPercet[6],100)/100*(EVALUATE(lcOrdFile+'.QTY6-'+lcOrdFile+'.CUT6'))),;
          Qty7   WITH INT(MIN(loParentForm.laPercet[7],100)/100*(EVALUATE(lcOrdFile+'.QTY7-'+lcOrdFile+'.CUT7'))),;
          Qty8   WITH INT(MIN(loParentForm.laPercet[8],100)/100*(EVALUATE(lcOrdFile+'.QTY8-'+lcOrdFile+'.CUT8'))),;
          TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
        *! B609917,1 MMT 05/14/2012 Generate PO from SO calculates PO Qty incorrectly  in case of percent <> 100[Start]
        IF !loParentForm.llRPGrpSO
        *! B609917,1 MMT 05/14/2012 Generate PO from SO calculates PO Qty incorrectly  in case of percent <> 100[END]
          lnCutQty1 = lnCutQty1+INT(loParentForm.laPercet[1]/100*(EVALUATE(lcOrdFile+'.QTY1-'+lcOrdFile+'.CUT1')))
          lnCutQty2 = lnCutQty2+INT(loParentForm.laPercet[2]/100*(EVALUATE(lcOrdFile+'.QTY2-'+lcOrdFile+'.CUT2')))
          lnCutQty3 = lnCutQty3+INT(loParentForm.laPercet[3]/100*(EVALUATE(lcOrdFile+'.QTY3-'+lcOrdFile+'.CUT3')))
          lnCutQty4 = lnCutQty4+INT(loParentForm.laPercet[4]/100*(EVALUATE(lcOrdFile+'.QTY4-'+lcOrdFile+'.CUT4')))
          lnCutQty5 = lnCutQty5+INT(loParentForm.laPercet[5]/100*(EVALUATE(lcOrdFile+'.QTY5-'+lcOrdFile+'.CUT5')))
          lnCutQty6 = lnCutQty6+INT(loParentForm.laPercet[6]/100*(EVALUATE(lcOrdFile+'.QTY6-'+lcOrdFile+'.CUT6')))
          lnCutQty7 = lnCutQty7+INT(loParentForm.laPercet[7]/100*(EVALUATE(lcOrdFile+'.QTY7-'+lcOrdFile+'.CUT7')))
          lnCutQty8 = lnCutQty8+INT(loParentForm.laPercet[8]/100*(EVALUATE(lcOrdFile+'.QTY8-'+lcOrdFile+'.CUT8')))
        *! B609917,1 MMT 05/14/2012 Generate PO from SO calculates PO Qty incorrectly  in case of percent <> 100[Start]
        ELSE
          lnCutQty1 = lnCutQty1+EVALUATE(lcOrdFile+'.QTY1-'+lcOrdFile+'.CUT1')
          lnCutQty2 = lnCutQty2+EVALUATE(lcOrdFile+'.QTY2-'+lcOrdFile+'.CUT2')
          lnCutQty3 = lnCutQty3+EVALUATE(lcOrdFile+'.QTY3-'+lcOrdFile+'.CUT3')
          lnCutQty4 = lnCutQty4+EVALUATE(lcOrdFile+'.QTY4-'+lcOrdFile+'.CUT4')
          lnCutQty5 = lnCutQty5+EVALUATE(lcOrdFile+'.QTY5-'+lcOrdFile+'.CUT5')
          lnCutQty6 = lnCutQty6+EVALUATE(lcOrdFile+'.QTY6-'+lcOrdFile+'.CUT6')
          lnCutQty7 = lnCutQty7+EVALUATE(lcOrdFile+'.QTY7-'+lcOrdFile+'.CUT7')
          lnCutQty8 = lnCutQty8+EVALUATE(lcOrdFile+'.QTY8-'+lcOrdFile+'.CUT8')
        ENDIF
        *! B609917,1 MMT 05/14/2012 Generate PO from SO calculates PO Qty incorrectly  in case of percent <> 100[END]

        lnOrdQty1 = lnOrdQty1+EVALUATE(loParentForm.lcCutPick+'.Qty1')
        lnOrdQty2 = lnOrdQty2+EVALUATE(loParentForm.lcCutPick+'.Qty2')
        lnOrdQty3 = lnOrdQty3+EVALUATE(loParentForm.lcCutPick+'.Qty3')
        lnOrdQty4 = lnOrdQty4+EVALUATE(loParentForm.lcCutPick+'.Qty4')
        lnOrdQty5 = lnOrdQty5+EVALUATE(loParentForm.lcCutPick+'.Qty5')
        lnOrdQty6 = lnOrdQty6+EVALUATE(loParentForm.lcCutPick+'.Qty6')
        lnOrdQty7 = lnOrdQty7+EVALUATE(loParentForm.lcCutPick+'.Qty7')
        lnOrdQty8 = lnOrdQty8+EVALUATE(loParentForm.lcCutPick+'.Qty8')
        IF TotQty <= 0
          DELETE
        ENDIF

        IF loParentForm.llDispPric
          STORE 0 TO lnTotQty
          FOR lnI = 1 TO 8
            lcI = STR(lnI,1)
            lnTotQty=lnTotQty+INT(loParentForm.laPercet[lnI]/100*(EVALUATE(lcOrdFile+'.QTY'+lcI+'-'+lcOrdFile+'.CUT'+lcI)))
          ENDFOR
          loParentForm.lnAllQty   = loParentForm.lnAllQty   + lnTotQty
          loParentForm.lnSelPrice = loParentForm.lnSelPrice + (lnTotQty*EVALUATE(lcOrdFile+'.Price'))
        ENDIF
        **end scan
      *C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [BEGIN]

      *608597,1 ALAA [Begin] Commnetd out to rename varibale llGrpSO to be llRPGrpSO
      *IF !loParentForm.llGrpSO
      IF !loParentForm.llRPGrpSO
      *608597,1 ALAA [END] Commnetd out to rename varibale llGrpSO to be llRPGrpSO

        IF lnCutQty1+lnCutQty2+lnCutQty3+lnCutQty4+lnCutQty5+lnCutQty6+lnCutQty7+lnCutQty8 > 0
          SELECT (loParentForm.lcPOLine)
          *IF !SEEK(SPACE(6)+lcDivision+lcSeason+lcStyle+lcDyelot)
            SELECT (loParentForm.lcPOH)
            REPLACE LastLine WITH LastLine+1

            SELECT(loParentForm.lcCutPick)
            REPLACE cTktLineNo     WITH STR(EVALUATE(loParentForm.lcPOH+'.LastLine'),6)

            SELECT (loParentForm.lcPOLine)
            APPEND BLANK
            REPLACE STYLE     WITH lcStyle ,;
              Dyelot    WITH lcDyelot,;
              TRANCD    WITH '1'     ,;
              LINENO    WITH EVALUATE(loParentForm.lcPOH+'.LastLine'),;
              cDivision WITH lcDivision,;
              SEASON    WITH lcSeason  ,;
              COMPLETE   WITH EVALUATE(loParentForm.lcPOH+'.Complete'),;
              cCstSht_Id WITH EVALUATE(loParentForm.lcPOH+'.cCstSht_ID'),;
              Fabric    WITH lcFabric  ,;
              cFabWare  WITH lcFabWare ,;
              nYeild    WITH lnYeild   ,;
              cStyGrade WITH lcStyGrade ,;
              cWareCode WITH IIF(loParentForm.lnType=2 OR !loParentForm.llWareHous,STYLE.cDefWare,loParentForm.lcWareCode)
          
          *ENDIF

          REPLACE Qty1 WITH INT(loParentForm.laPercet[1]/100*(EVALUATE(lcOrdFile+'.QTY1-'+lcOrdFile+'.CUT1'))) ,;
            Qty2 WITH INT(loParentForm.laPercet[2]/100*(EVALUATE(lcOrdFile+'.QTY2-'+lcOrdFile+'.CUT2'))),;
            Qty3 WITH INT(loParentForm.laPercet[3]/100*(EVALUATE(lcOrdFile+'.QTY3-'+lcOrdFile+'.CUT3'))) ,;
            Qty4 WITH INT(loParentForm.laPercet[4]/100*(EVALUATE(lcOrdFile+'.QTY4-'+lcOrdFile+'.CUT4'))) ,;
            Qty5 WITH INT(loParentForm.laPercet[5]/100*(EVALUATE(lcOrdFile+'.QTY5-'+lcOrdFile+'.CUT5'))) ,;
            Qty6 WITH INT(loParentForm.laPercet[6]/100*(EVALUATE(lcOrdFile+'.QTY6-'+lcOrdFile+'.CUT6'))) ,;
            Qty7 WITH INT(loParentForm.laPercet[7]/100*(EVALUATE(lcOrdFile+'.QTY7-'+lcOrdFile+'.CUT7'))) ,;
            Qty8 WITH INT(loParentForm.laPercet[8]/100*(EVALUATE(lcOrdFile+'.QTY8-'+lcOrdFile+'.CUT8'))) ,;
            ORD1 WITH EVALUATE(loParentForm.lcCutPick+'.Qty1') ,;
            ORD2 WITH EVALUATE(loParentForm.lcCutPick+'.Qty2') ,;
            ORD3 WITH EVALUATE(loParentForm.lcCutPick+'.Qty3') ,;
            ORD4 WITH EVALUATE(loParentForm.lcCutPick+'.Qty4') ,;
            ORD5 WITH EVALUATE(loParentForm.lcCutPick+'.Qty5') ,;
            ORD6 WITH EVALUATE(loParentForm.lcCutPick+'.Qty6') ,;
            ORD7 WITH EVALUATE(loParentForm.lcCutPick+'.Qty7') ,;
            ORD8 WITH EVALUATE(loParentForm.lcCutPick+'.Qty8') ,;
            TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
            TotOrd WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8



          REPLACE nFCost1 WITH TotQty * STYLE.nMCost1 ,;
            nFCost2 WITH TotQty * STYLE.nMCost2 ,;
            nFCost3 WITH TotQty * STYLE.nMCost3 ,;
            nFCost4 WITH TotQty * STYLE.nMCost4 ,;
            nFCost5 WITH TotQty * STYLE.nMCost5 ,;
            nFCost6 WITH TotQty * STYLE.nMCost6 ,;
            nFCost7 WITH TotQty * STYLE.nMCost7 ,;
            Gros_Price WITH STYLE.nMCost1 ,;
            nICost1 WITH TotQty * STYLE.nMCost1 ,;
            nICost2 WITH TotQty * STYLE.nMCost2 ,;
            nICost3 WITH TotQty * STYLE.nMCost3 ,;
            nICost4 WITH TotQty * STYLE.nMCost4 ,;
            nICost5 WITH TotQty * STYLE.nMCost5 ,;
            nICost6 WITH TotQty * STYLE.nMCost6 ,;
            nICost7 WITH TotQty * STYLE.nMCost7

          IF loParentForm.llDispPric
            loParentForm.loStyle.SEEK(lcStyle)
            loParentForm.lnRotSub   = IIF(loParentForm.llStyMark,STYLE.TOTCOST,loParentForm.lnSelPrice)
            loParentForm.lnGrosMrgn = IIF(loParentForm.lnRotSub=0,0,((loParentForm.lnSelPrice-STYLE.TOTCOST)/loParentForm.lnRotSub)*100)
            REPLACE nSelPrice WITH loParentForm.lnSelPrice ,;
              nGrosMrgn WITH loParentForm.lnGrosMrgn
            STORE 0 TO loParentForm.lnSelPrice,loParentForm.lnAllQty
          ENDIF

          lnCost1   = lnCost1 + nFCost1
          lnCost2   = lnCost2 + nFCost2
          lnCost3   = lnCost3 + nFCost3
          lnCost4   = lnCost4 + nFCost4
          lnCost5   = lnCost5 + nFCost5
          lnCost6   = lnCost6 + nFCost6
          lnCost7   = lnCost7 + nFCost7
          lnOrder   = lnOrder + TotOrd
          lnAllocat = lnAllocat + TotQty
        ENDIF
      ENDIF
      *C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [END]

      ENDSCAN

      IF loParentForm.llDispPric
        loParentForm.lnSelPrice = loParentForm.lnSelPrice / loParentForm.lnAllQty
      ENDIF
      *C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [BEGIN]
     *608597,1 ALAA [Begin] Commnetd out to rename varibale llGrpSO to be llRPGrpSO
      *IF loParentForm.llGrpSO
        IF loParentForm.llRPGrpSO
        *C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [END]
        *608597,1 ALAA [END] Commnetd out to rename varibale llGrpSO to be llRPGrpSO
        *! B609917,1 MMT 05/14/2012 Generate PO from SO calculates PO Qty incorrectly  in case of percent <> 100[Start]
        lnCutQty1 = INT(loParentForm.laPercet[1]/100*(lnCutQty1))
        lnCutQty2 = INT(loParentForm.laPercet[2]/100*(lnCutQty2))
        lnCutQty3 = INT(loParentForm.laPercet[3]/100*(lnCutQty3))
        lnCutQty4 = INT(loParentForm.laPercet[4]/100*(lnCutQty4))
        lnCutQty5 = INT(loParentForm.laPercet[5]/100*(lnCutQty5))
        lnCutQty6 = INT(loParentForm.laPercet[6]/100*(lnCutQty6))
        lnCutQty7 = INT(loParentForm.laPercet[7]/100*(lnCutQty7))
        lnCutQty8 = INT(loParentForm.laPercet[8]/100*(lnCutQty8))
        *! B609917,1 MMT 05/14/2012 Generate PO from SO calculates PO Qty incorrectly  in case of percent <> 100[END]

        IF lnCutQty1+lnCutQty2+lnCutQty3+lnCutQty4+lnCutQty5+lnCutQty6+lnCutQty7+lnCutQty8 > 0
          SELECT (loParentForm.lcPOLine)
          IF !SEEK(SPACE(6)+lcDivision+lcSeason+lcStyle+lcDyelot)
            SELECT (loParentForm.lcPOH)
            REPLACE LastLine WITH LastLine+1

            SELECT (loParentForm.lcPOLine)
            APPEND BLANK
            REPLACE STYLE     WITH lcStyle ,;
              Dyelot    WITH lcDyelot,;
              TRANCD    WITH '1'     ,;
              LINENO    WITH EVALUATE(loParentForm.lcPOH+'.LastLine'),;
              cDivision WITH lcDivision,;
              SEASON    WITH lcSeason  ,;
              COMPLETE   WITH EVALUATE(loParentForm.lcPOH+'.Complete'),;
              cCstSht_Id WITH EVALUATE(loParentForm.lcPOH+'.cCstSht_ID'),;
              Fabric    WITH lcFabric  ,;
              cFabWare  WITH lcFabWare ,;
              nYeild    WITH lnYeild   ,;
              cStyGrade WITH lcStyGrade ,;
              cWareCode WITH IIF(loParentForm.lnType=2 OR !loParentForm.llWareHous,STYLE.cDefWare,loParentForm.lcWareCode)
          ENDIF
          REPLACE Qty1 WITH lnCutQty1 ,;
            Qty2 WITH lnCutQty2 ,;
            Qty3 WITH lnCutQty3 ,;
            Qty4 WITH lnCutQty4 ,;
            Qty5 WITH lnCutQty5 ,;
            Qty6 WITH lnCutQty6 ,;
            Qty7 WITH lnCutQty7 ,;
            Qty8 WITH lnCutQty8 ,;
            ORD1 WITH lnOrdQty1 ,;
            ORD2 WITH lnOrdQty2 ,;
            ORD3 WITH lnOrdQty3 ,;
            ORD4 WITH lnOrdQty4 ,;
            ORD5 WITH lnOrdQty5 ,;
            ORD6 WITH lnOrdQty6 ,;
            ORD7 WITH lnOrdQty7 ,;
            ORD8 WITH lnOrdQty8 ,;
            TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
            TotOrd WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
          REPLACE nFCost1 WITH TotQty * STYLE.nMCost1 ,;
            nFCost2 WITH TotQty * STYLE.nMCost2 ,;
            nFCost3 WITH TotQty * STYLE.nMCost3 ,;
            nFCost4 WITH TotQty * STYLE.nMCost4 ,;
            nFCost5 WITH TotQty * STYLE.nMCost5 ,;
            nFCost6 WITH TotQty * STYLE.nMCost6 ,;
            nFCost7 WITH TotQty * STYLE.nMCost7 ,;
            Gros_Price WITH STYLE.nMCost1 ,;
            nICost1 WITH TotQty * STYLE.nMCost1 ,;
            nICost2 WITH TotQty * STYLE.nMCost2 ,;
            nICost3 WITH TotQty * STYLE.nMCost3 ,;
            nICost4 WITH TotQty * STYLE.nMCost4 ,;
            nICost5 WITH TotQty * STYLE.nMCost5 ,;
            nICost6 WITH TotQty * STYLE.nMCost6 ,;
            nICost7 WITH TotQty * STYLE.nMCost7

          IF loParentForm.llDispPric
            loParentForm.loStyle.SEEK(lcStyle)
            loParentForm.lnRotSub   = IIF(loParentForm.llStyMark,STYLE.TOTCOST,loParentForm.lnSelPrice)
            loParentForm.lnGrosMrgn = IIF(loParentForm.lnRotSub=0,0,((loParentForm.lnSelPrice-STYLE.TOTCOST)/loParentForm.lnRotSub)*100)
            REPLACE nSelPrice WITH loParentForm.lnSelPrice ,;
              nGrosMrgn WITH loParentForm.lnGrosMrgn
            STORE 0 TO loParentForm.lnSelPrice,loParentForm.lnAllQty
          ENDIF

          lnCost1   = lnCost1 + nFCost1
          lnCost2   = lnCost2 + nFCost2
          lnCost3   = lnCost3 + nFCost3
          lnCost4   = lnCost4 + nFCost4
          lnCost5   = lnCost5 + nFCost5
          lnCost6   = lnCost6 + nFCost6
          lnCost7   = lnCost7 + nFCost7
          lnOrder   = lnOrder + TotOrd
          lnAllocat = lnAllocat + TotQty
          *! B609917,1 MMT 05/14/2012 Generate PO from SO calculates PO Qty incorrectly  in case of percent <> 100[Start]
          IF loParentForm.laPercet[1] <> 100 OR loParentForm.laPercet[2] <> 100 OR loParentForm.laPercet[3] <> 100 OR loParentForm.laPercet[3] <> 100;
             OR loParentForm.laPercet[4] <> 100 OR loParentForm.laPercet[6] <> 100 OR loParentForm.laPercet[7] <> 100 OR loParentForm.laPercet[8] <> 100
            FOR lnCntPer = 1 TO 8
              IF loParentForm.laPercet[lnCntPer] <> 100
                lcCntPer = STR(lnCntPer ,1)
                SELECT(loParentForm.lcCutPick)
                lcOrderCutPick = ORDER()
                SET ORDER TO (lcOrderCutPick) DESCENDING
                =SEEK(SPACE(6)+lcDivision+lcSeason+lcStyle+EVALUATE(loParentForm.lcPOLine+'.cwarecode')+lcDyelot)
                SUM Qty&lcCntPer. TO lnSumQty REST WHILE CTKTNO+CDIVISION+SEASON+STYLE+CWARECODE+DYELOT= ;
                                               SPACE(6)+lcDivision+lcSeason+lcStyle+EVALUATE(loParentForm.lcPOLine+'.cwarecode')+lcDyelot
                IF lnSumQty <> EVALUATE(loParentForm.lcPOLine+'.Qty'+lcCntPer)
                  =SEEK(SPACE(6)+lcDivision+lcSeason+lcStyle+EVALUATE(loParentForm.lcPOLine+'.cwarecode')+lcDyelot)
                  REPLACE Qty&lcCntPer. WITH Qty&lcCntPer. + (EVALUATE(loParentForm.lcPOLine+'.Qty'+lcCntPer) - lnSumQty),;
                          TotQty  WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 IN (loParentForm.lcCutPick)
                ENDIF
                SELECT(loParentForm.lcCutPick)
                SET ORDER TO (lcOrderCutPick)
              ENDIF
            ENDFOR
          ENDIF
          *! B609917,1 MMT 05/14/2012 Generate PO from SO calculates PO Qty incorrectly  in case of percent <> 100[End]

        ENDIF
        *C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [BEGIN]
         *608597,1 ALAA [Begin] Commnetd out to rename varibale llGrpSO to be llRPGrpSO
      ENDIF
      *C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [END]
      *608597,1 ALAA [END] Commnetd out to rename varibale llGrpSO to be llRPGrpSO
      SELECT (lcOrdFile)
    ENDDO
    SELECT (loParentForm.lcPOH)
    REPLACE nStyOrder WITH lnAllocat ,;
      TotOrd    WITH lnOrder   ,;
      nFCost1   WITH lnCost1   ,;
      nFCost2   WITH lnCost2   ,;
      nFCost3   WITH lnCost3   ,;
      nFCost4   WITH lnCost4   ,;
      nFCost5   WITH lnCost5   ,;
      nFCost6   WITH lnCost6   ,;
      nFCost7   WITH lnCost7   ,;
      TOTCOST   WITH nFCost1+nFCost2+nFCost3+nFCost4+nFCost5+nFCost6+nFCost7,;
      nICost1   WITH lnCost1   ,;
      nICost2   WITH lnCost2   ,;
      nICost3   WITH lnCost3   ,;
      nICost4   WITH lnCost4   ,;
      nICost5   WITH lnCost5   ,;
      nICost6   WITH lnCost6   ,;
      nICost7   WITH lnCost7
    IF nStyOrder = 0
      DELETE
    ENDIF
    SELECT (lcOrdFile)
  ENDDO
  loBomHeader.DESTROY

  loParentForm.loStyle.SETORDER(lcStyOrd)
  SET ORDER TO TAG (lcOrdTag) IN (lcOrdFile)
  SELECT (lnAlias)
  WAIT CLEAR

  *!*************************************************************
  *! Name      : lfUpdPOPik
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/09/2004
  *! Purpose   : Generate P/O's
  *!*************************************************************
  *! Calls     : gfGetExSin(),lfUnAllPO()
  *!*************************************************************
  *! Parameters: lcOrdFile : Order line temporary file name
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfUpdPOPik(lcOrdFile)
  *!*************************************************************
FUNCTION lfUpdPOPik
  LPARAMETERS lcOrdFile

  LOCAL lnAlias,lcStyOrd,lcPUntSin,lcDUntSin,lcPExSign,lcDExSign,lcDivision,;
    lcPurCode,lcStyGrade,lnTCost1,lnTCost2,lnTCost3,lnTCost4,lnTCost5,lnTCost6,lnTCost7,;
    lnOrder,lnAllocat,lnTFCost1,lnTFCost2,lnTFCost3,lnTFCost4,lnTFCost5,lnTFCost6,lnTFCost7
  LOCAL lnCutQty1,lnCutQty2,lnCutQty3,lnCutQty4,lnCutQty5,lnCutQty6,lnCutQty7,lnCutQty8,;
    lnOrdQty1,lnOrdQty2,lnOrdQty3,lnOrdQty4,lnOrdQty5,lnOrdQty6,lnOrdQty7,lnOrdQty8

  PRIVATE loBomHeader
  loBomHeader = CREATEOBJECT('RemoteTable','BomHeadr','BomHeadr','BomHeadr',loParentForm.DATASESSIONID)
  *N000587,1 WAM 12/01/2007 Get currency, exchange rate and unit for each cost element from BOM file
  loBom = CREATEOBJECT('RemoteTable','Bom','MULTIBOM','Bom',loParentForm.DATASESSIONID)
  *N000587,1 WAM 12/01/2007 (End)


  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *WAIT LANG_MFGENCT_GENPOORD WINDOW NOWAIT
  WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_GENPOORD,loParentForm.GetHeaderText("LANG_MFGENCT_GENPOORD",loParentForm.HeaderAlias)) WINDOW NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  STORE '/' TO lcPUntSin,lcDUntSin
  lcPExSign = gfGetExSin(@lcPUntSin,loParentForm.lcPCurr)
  lcDExSign = gfGetExSin(@lcDUntSin,loParentForm.lcDCurr)
  lnAlias = SELECT(0)
  SELECT STYLE
  lcStyOrd = TAG()
  loParentForm.loStyle.SETORDER('STYLE')
  SELECT (lcOrdFile)
  lcOrdTag = TAG()
  SET ORDER TO TAG TICKET

  =IIF(loParentForm.llRPUnAWip,lfUnAllPO(lcOrdFile),.T.)
  SELECT (lcOrdFile)
  =SEEK("�")
  DO WHILE cSelect+cDivision +cPurCode+cStyGrade+STYLE+Dyelot="�"

    lcDivision = cDivision
    lcPurCode  = cPurCode
    lcStyGrade = cStyGrade
    *B608873,1 WAM 05/25/2009 Add new option to generate one PO per each style major
    lcStyMajor = IIF(loParentForm.llRPGrpByST, PADR(LEFT(Style,loParentForm.lnMajorLen),19),SPACE(19))
    *B608873,1 WAM 05/25/2009 (End)

    SELECT (loParentForm.lcPOH)
    *B608873,1 WAM 05/25/2009 Add new option to generate one PO per each style major
    *IF !SEEK(SPACE(6)+lcDivision+lcPurCode+lcStyGrade)
    IF !SEEK(SPACE(6)+lcDivision+lcPurCode+lcStyGrade+lcStyMajor)
    *B608873,1 WAM 05/25/2009 (End)
      APPEND BLANK
      REPLACE cstytype   WITH 'P',;
              Vendor     WITH loParentForm.lcVendor,;
              STATUS     WITH 'H',;
              cDivision  WITH lcDivision,;
              cPurCode   WITH lcPurCode,;
              ENTERED    WITH oAriaApplication.systemdate,;
              COMPLETE   WITH ENTERED+90,;
              SHIPVIA    WITH loParentForm.lcShip,;
              CTERMCODE  WITH loParentForm.lcTerm,;
              cWareCode  WITH loParentForm.lcWareCode ,;
              cStyGrade  WITH lcStyGrade,;
              cPriceCur  WITH loParentForm.lcPCurr,;
              nPriceRat  WITH loParentForm.lnPRate,;
              nCurrUnit  WITH loParentForm.lnUnit1,;
              cDutyCur   WITH loParentForm.lcDCurr,;
              nDutyRat   WITH loParentForm.lnDRate,;
              nDCurUnit  WITH loParentForm.lnUnit2,;
              lMultiWare WITH (loParentForm.lnType=2)

      *B608873,1 WAM 05/25/2009 Add new option to generate one PO per each style major
      *B608873,1 WAM 05/25/2009 Update PO with default complate andavailable dates and default project template
      REPLACE Available WITH loParentForm.ldAvailable ,;
              COMPLETE  WITH loParentForm.ldComplete ,;
              CDEFTEMPL WITH loParentForm.lcDefTemp ,;
              STYLE     WITH lcStyMajor
      *B608873,1 WAM 05/25/2009 (End)
    ENDIF
    STORE 0 TO lnTCost1,lnTCost2,lnTCost3,lnTCost4,lnTCost5,lnTCost6,lnTCost7,lnOrder,lnAllocat,;
      lnTFCost1,lnTFCost2,lnTFCost3,lnTFCost4,lnTFCost5,lnTFCost6,lnTFCost7

    IF loParentForm.llDispPric
      STORE 0 TO loParentForm.lnSelPrice,loParentForm.lnAllQty,loParentForm.lnRotSub,loParentForm.lnGrosMrgn
    ENDIF
    SELECT (lcOrdFile)

    *B608873,1 WAM 05/25/2009 Add new option to generate one PO per each style major
    *DO WHILE cSelect+cDivision +cPurCode+cStyGrade+STYLE+Dyelot = ;
        "�"+lcDivision+lcPurCode+lcStyGrade
    DO WHILE cSelect+cDivision +cPurCode+cStyGrade+STYLE+Dyelot = ;
        "�"+lcDivision+lcPurCode+lcStyGrade+IIF(loParentForm.llRPGrpByST, LEFT(lcStyMajor ,loParentForm.lnMajorLen),'')
    *B608873,1 WAM 05/25/2009 (End)
      lcStyle  = STYLE
      lcDyelot = Dyelot
      lcFabric = Fabric
      lcFabWare= cFabWare
      lnYeild  = nYeild
      STORE 0 TO lnCutQty1,lnCutQty2,lnCutQty3,lnCutQty4,lnCutQty5,lnCutQty6,lnCutQty7,lnCutQty8
      STORE 0 TO lnOrdQty1,lnOrdQty2,lnOrdQty3,lnOrdQty4,lnOrdQty5,lnOrdQty6,lnOrdQty7,lnOrdQty8
      SCAN REST WHILE cSelect+cDivision +cPurCode+cStyGrade+STYLE+Dyelot = ;
          "�"+lcDivision+lcPurCode+lcStyGrade+lcStyle+lcDyelot ;
          FOR   IIF(loParentForm.llRpBOPFab,nRequired-nWIPUsed > 0,.T.)
        SELECT(loParentForm.lcCutPick)
        APPEND BLANK
        REPLACE TRANCD    WITH '2',;
                ORDER     WITH EVALUATE(lcOrdFile+'.ORDER'),;
                cOrdLine  WITH STR(EVALUATE(lcOrdFile+'.LINENO'),6),;
                STYLE     WITH lcStyle ,;
                Dyelot    WITH lcDyelot,;
                cWareCode WITH loParentForm.lcWareCode ,;
                cDivision WITH lcDivision ,;
                cPurCode  WITH lcPurCode ,;
                cStyGrade WITH lcStyGrade

        REPLACE Qty1   WITH INT(MIN(loParentForm.laPercet[1],100)/100*(EVALUATE(lcOrdFile+'.QTY1-'+lcOrdFile+'.CUT1'))),;
                Qty2   WITH INT(MIN(loParentForm.laPercet[2],100)/100*(EVALUATE(lcOrdFile+'.QTY2-'+lcOrdFile+'.CUT2'))),;
                Qty3   WITH INT(MIN(loParentForm.laPercet[3],100)/100*(EVALUATE(lcOrdFile+'.QTY3-'+lcOrdFile+'.CUT3'))),;
                Qty4   WITH INT(MIN(loParentForm.laPercet[4],100)/100*(EVALUATE(lcOrdFile+'.QTY4-'+lcOrdFile+'.CUT4'))),;
                Qty5   WITH INT(MIN(loParentForm.laPercet[5],100)/100*(EVALUATE(lcOrdFile+'.QTY5-'+lcOrdFile+'.CUT5'))),;
                Qty6   WITH INT(MIN(loParentForm.laPercet[6],100)/100*(EVALUATE(lcOrdFile+'.QTY6-'+lcOrdFile+'.CUT6'))),;
                Qty7   WITH INT(MIN(loParentForm.laPercet[7],100)/100*(EVALUATE(lcOrdFile+'.QTY7-'+lcOrdFile+'.CUT7'))),;
                Qty8   WITH INT(MIN(loParentForm.laPercet[8],100)/100*(EVALUATE(lcOrdFile+'.QTY8-'+lcOrdFile+'.CUT8'))),;
                TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
        *! B609917,1 MMT 05/14/2012 Generate PO from SO calculates PO Qty incorrectly  in case of percent <> 100[Start]
        IF !loParentForm.llRPGrpSO
        *! B609917,1 MMT 05/14/2012 Generate PO from SO calculates PO Qty incorrectly  in case of percent <> 100[END]
          lnCutQty1 = lnCutQty1+INT(loParentForm.laPercet[1]/100*(EVALUATE(lcOrdFile+'.QTY1-'+lcOrdFile+'.CUT1')))
          lnCutQty2 = lnCutQty2+INT(loParentForm.laPercet[2]/100*(EVALUATE(lcOrdFile+'.QTY2-'+lcOrdFile+'.CUT2')))
          lnCutQty3 = lnCutQty3+INT(loParentForm.laPercet[3]/100*(EVALUATE(lcOrdFile+'.QTY3-'+lcOrdFile+'.CUT3')))
          lnCutQty4 = lnCutQty4+INT(loParentForm.laPercet[4]/100*(EVALUATE(lcOrdFile+'.QTY4-'+lcOrdFile+'.CUT4')))
          lnCutQty5 = lnCutQty5+INT(loParentForm.laPercet[5]/100*(EVALUATE(lcOrdFile+'.QTY5-'+lcOrdFile+'.CUT5')))
          lnCutQty6 = lnCutQty6+INT(loParentForm.laPercet[6]/100*(EVALUATE(lcOrdFile+'.QTY6-'+lcOrdFile+'.CUT6')))
          lnCutQty7 = lnCutQty7+INT(loParentForm.laPercet[7]/100*(EVALUATE(lcOrdFile+'.QTY7-'+lcOrdFile+'.CUT7')))
          lnCutQty8 = lnCutQty8+INT(loParentForm.laPercet[8]/100*(EVALUATE(lcOrdFile+'.QTY8-'+lcOrdFile+'.CUT8')))
        *! B609917,1 MMT 05/14/2012 Generate PO from SO calculates PO Qty incorrectly  in case of percent <> 100[Start]
        ELSE
          lnCutQty1 = lnCutQty1+EVALUATE(lcOrdFile+'.QTY1-'+lcOrdFile+'.CUT1')
          lnCutQty2 = lnCutQty2+EVALUATE(lcOrdFile+'.QTY2-'+lcOrdFile+'.CUT2')
          lnCutQty3 = lnCutQty3+EVALUATE(lcOrdFile+'.QTY3-'+lcOrdFile+'.CUT3')
          lnCutQty4 = lnCutQty4+EVALUATE(lcOrdFile+'.QTY4-'+lcOrdFile+'.CUT4')
          lnCutQty5 = lnCutQty5+EVALUATE(lcOrdFile+'.QTY5-'+lcOrdFile+'.CUT5')
          lnCutQty6 = lnCutQty6+EVALUATE(lcOrdFile+'.QTY6-'+lcOrdFile+'.CUT6')
          lnCutQty7 = lnCutQty7+EVALUATE(lcOrdFile+'.QTY7-'+lcOrdFile+'.CUT7')
          lnCutQty8 = lnCutQty8+EVALUATE(lcOrdFile+'.QTY8-'+lcOrdFile+'.CUT8')
        ENDIF
        *! B609917,1 MMT 05/14/2012 Generate PO from SO calculates PO Qty incorrectly  in case of percent <> 100[END]

        lnOrdQty1 = lnOrdQty1+EVALUATE(loParentForm.lcCutPick+'.Qty1')
        lnOrdQty2 = lnOrdQty2+EVALUATE(loParentForm.lcCutPick+'.Qty2')
        lnOrdQty3 = lnOrdQty3+EVALUATE(loParentForm.lcCutPick+'.Qty3')
        lnOrdQty4 = lnOrdQty4+EVALUATE(loParentForm.lcCutPick+'.Qty4')
        lnOrdQty5 = lnOrdQty5+EVALUATE(loParentForm.lcCutPick+'.Qty5')
        lnOrdQty6 = lnOrdQty6+EVALUATE(loParentForm.lcCutPick+'.Qty6')
        lnOrdQty7 = lnOrdQty7+EVALUATE(loParentForm.lcCutPick+'.Qty7')
        lnOrdQty8 = lnOrdQty8+EVALUATE(loParentForm.lcCutPick+'.Qty8')

        IF loParentForm.llDispPric
          STORE 0 TO lnTotQty
          FOR lnI = 1 TO 8
            lcI = STR(lnI,1)
            lnTotQty=lnTotQty+INT(loParentForm.laPercet[lnI]/100*(EVALUATE(lcOrdFile+'.QTY'+lcI+'-'+lcOrdFile+'.CUT'+lcI)))
          ENDFOR
          loParentForm.lnAllQty   = loParentForm.lnAllQty   + lnTotQty
          loParentForm.lnSelPrice = loParentForm.lnSelPrice + (lnTotQty*EVALUATE(lcOrdFile+'.Price'))
        ENDIF

        *C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [BEGIN]
        *608597,1 ALAA [Begin] Commnetd out to rename varibale llGrpSO to be llRPGrpSO
        *IF !loParentForm.llGrpSO AND (lnCutQty1+lnCutQty2+lnCutQty3+lnCutQty4+lnCutQty5+lnCutQty6+lnCutQty7+lnCutQty8 > 0)
        IF !loParentForm.llRPGrpSO AND (lnCutQty1+lnCutQty2+lnCutQty3+lnCutQty4+lnCutQty5+lnCutQty6+lnCutQty7+lnCutQty8 > 0)
        *608597,1 ALAA [END] Commnetd out to rename varibale llGrpSO to be llRPGrpSO

          SELECT (loParentForm.lcPOH)
          REPLACE LastLine WITH LastLine + 1

          *: B608058,1 MMT 04/24/20007 fix bug of not saving packing info while generating PO From SO[Start]
          =loParentForm.loStyle.SEEK(lcStyle)
          *: B608058,1 MMT 04/24/20007 fix bug of not saving packing info while generating PO From SO[End]
          SELECT(loParentForm.lcCutPick)
          REPLACE cTktLineNo WITH STR(EVALUATE(loParentForm.lcPOH+'.LastLine'),6)

          SELECT (loParentForm.lcPOLine)
          APPEND BLANK
          REPLACE STYLE WITH lcStyle    ,;
                  Dyelot     WITH lcDyelot   ,;
                  TRANCD     WITH '1'        ,;
                  cDivision  WITH lcDivision ,;
                  cWareCode  WITH loParentForm.lcWareCode ,;
                  LINENO     WITH EVALUATE(loParentForm.lcPOH+'.LastLine') ,;
                  Vendor     WITH loParentForm.lcVendor   ,;
                  SHIPVIA    WITH loParentForm.lcShip     ,;
                  COMPLETE   WITH EVALUATE(loParentForm.lcPOH+'.Complete'),;
                  cCstSht_Id WITH lfGetCstSht(lcStyle,loParentForm),;
                  cPurCode   WITH lcPurCode  ,;
                  cStyGrade  WITH lcStyGrade ,;
                  Fabric     WITH lcFabric   ,;
                  cFabWare   WITH lcFabWare  ,;
                  nYeild     WITH lnYeild

          *: B608058,1 MMT 04/24/20007 fix bug of not saving packing info while generating PO From SO[Start]
          REPLACE nmspackhgt WITH STYLE.nmspackhgt,;
                  nmspackLEN WITH STYLE.nmspackLEN,;
                  nmspackWDT WITH STYLE.nmspackWDT,;
                  nmspackQTY WITH STYLE.nmspackQTY,;
                  nmspackWGT WITH STYLE.nmspackWGT,;
                  ninpackhgt WITH STYLE.ninpackhgt,;
                  ninpackLEN WITH STYLE.ninpackLEN,;
                  ninpackWDT WITH STYLE.ninpackWDT,;
                  ninpackQTY WITH STYLE.ninpackQTY,;
                  ninpackWGT WITH STYLE.ninpackWGT,;
                  cpacktype  WITH STYLE.cpacktype ,;
                  cpackUom   WITH STYLE.cpackUom
          *: B608058,1 MMT 04/24/20007 fix bug of not saving packing info while generating PO From SO[End]
          *! B610202,1 HIA 01/17/2013 Aria4xp - PO - Generate PO from SO/Open to sell [T20130107.0006][Start]
          REPLACE cvensty WITH STYLE.cvensty
          *! B610202,1 HIA 01/17/2013 Aria4xp - PO - Generate PO from SO/Open to sell [T20130107.0006][End]
                        
          *ENDIF
          REPLACE Qty1 WITH INT(loParentForm.laPercet[1]/100*(EVALUATE(lcOrdFile+'.QTY1-'+lcOrdFile+'.CUT1'))) ,;
                  Qty2 WITH INT(loParentForm.laPercet[2]/100*(EVALUATE(lcOrdFile+'.QTY2-'+lcOrdFile+'.CUT2'))),;
                  Qty3 WITH INT(loParentForm.laPercet[3]/100*(EVALUATE(lcOrdFile+'.QTY3-'+lcOrdFile+'.CUT3'))) ,;
                  Qty4 WITH INT(loParentForm.laPercet[4]/100*(EVALUATE(lcOrdFile+'.QTY4-'+lcOrdFile+'.CUT4'))) ,;
                  Qty5 WITH INT(loParentForm.laPercet[5]/100*(EVALUATE(lcOrdFile+'.QTY5-'+lcOrdFile+'.CUT5'))) ,;
                  Qty6 WITH INT(loParentForm.laPercet[6]/100*(EVALUATE(lcOrdFile+'.QTY6-'+lcOrdFile+'.CUT6'))) ,;
                  Qty7 WITH INT(loParentForm.laPercet[7]/100*(EVALUATE(lcOrdFile+'.QTY7-'+lcOrdFile+'.CUT7'))) ,;
                  Qty8 WITH INT(loParentForm.laPercet[8]/100*(EVALUATE(lcOrdFile+'.QTY8-'+lcOrdFile+'.CUT8'))) ,;
                  ORD1 WITH EVALUATE(loParentForm.lcCutPick+'.Qty1') ,;
                  ORD2 WITH EVALUATE(loParentForm.lcCutPick+'.Qty2') ,;
                  ORD3 WITH EVALUATE(loParentForm.lcCutPick+'.Qty3') ,;
                  ORD4 WITH EVALUATE(loParentForm.lcCutPick+'.Qty4') ,;
                  ORD5 WITH EVALUATE(loParentForm.lcCutPick+'.Qty5') ,;
                  ORD6 WITH EVALUATE(loParentForm.lcCutPick+'.Qty6') ,;
                  ORD7 WITH EVALUATE(loParentForm.lcCutPick+'.Qty7') ,;
                  ORD8 WITH EVALUATE(loParentForm.lcCutPick+'.Qty8') ,;
                  TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
                  TotOrd WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
          =loParentForm.loStyle.SEEK(lcStyle)
          lnGros_Price = 0
          *N000587,1 WAM 12/01/2007 Get currency, exchange rate and unit for each cost element from BOM file
          STORE 0 TO lnCost1, lnCost2,lnCost3,lnCost4,lnCost5,lnCost6,lnCost7
          STORE 0 TO lnECost1, lnECost2, lnECost3, lnECost4, lnECost5, lnECost6, lnECost7
          lcCstShtTyp = IIF(loParentForm.lcChoice = 'P','I','M')
          lcCstSht_ID = EVALUATE(loParentForm.lcPOLine+'.cCstSht_Id')
          *B610190,1 MMT 01/03/2013 Modify PO screen and Generate PO from SO to Handle price/size in cost sheet[Start]
          llCstShtPerSize = loBomHeader.SEEK('0001'+PADR(SUBSTR(lcStyle,1,loParentForm.lnMajorLen),19)+"I"+PADR(lcCstSht_ID,6)) AND BomHeadr.lbasonsiz
          IF llCstShtPerSize AND loBom.SEEK('0001'+PADR(SUBSTR(lcStyle,1,loParentForm.lnMajorLen),19)+lcCstShtTyp+lcCstSht_ID)
            DIMENSION lasizeprice[1,6]
            lasizeprice = ''
            SELECT BOM
            SCAN REST WHILE cinvtype+cItmMajor+cCstShtTyp+cCstSht_Id+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6) =;
                            '0001'+PADR(SUBSTR(lcStyle,1,loParentForm.lnMajorLen),19)+lcCstShtTyp+lcCstSht_ID  FOR ccatgtyp = 'P' 
              IF !LIKE(STRTRAN(cItmMask,'*','?'),lcStyle) .OR. (!EMPTY(MSIZES) .AND. ATCLINE(STYLE.SCALE+'~',MSIZES)=0) .OR. ;
                 (!EMPTY(MSZCROSREF) .AND. ATCLINE(STYLE.SCALE+',',MSZCROSREF)=0)
                LOOP
              ENDIF
              IF !EMPTY(BOM.mSizes)
                =SEEK(lcStyle,'STYLE','STYLE')
                lnMSizeLines = MEMLINES(BOM.mSizes)
                IF lnMSizeLines > 0
                  FOR lnCntLine =  1 TO lnMSizeLines
                    lcMSizes = MLINE(BOM.mSizes,lnCntLine)
                    IF !EMPTY(lcMSizes)
                      lnWPos = ATC('~',lcMSizes)
                      IF !(Style.Scale $ lcMSizes)
                        LOOP
                      ENDIF
                      lnScales = SUBSTR(lcMSizes,lnWPos+1)
                      IF EMPTY(lasizeprice[1,1])
                        lasizeprice[1,1]= SUBSTR(lcMSizes,1,lnWPos-1)
                        lasizeprice[1,2]= lnScales
                        lasizeprice[1,3]= Bom.TotCost
                        lasizeprice[1,4]= Bom.nExRate
                        lasizeprice[1,5]= Bom.nCurrUnit
                        lasizeprice[1,6]= Bom.cCurrCode
                      ELSE
                        lnPosInArr = ALEN(lasizeprice,1)
                        DIMENSION lasizeprice[lnPosInArr+1,6]
                        lasizeprice[lnPosInArr+1,1]= SUBSTR(lcMSizes,1,lnWPos-1)
                        lasizeprice[lnPosInArr+1,2]= lnScales
                        lasizeprice[lnPosInArr+1,3]= Bom.TotCost
                        lasizeprice[lnPosInArr+1,4]= Bom.nExRate
                        lasizeprice[lnPosInArr+1,5]= Bom.nCurrUnit
                        lasizeprice[lnPosInArr+1,6]= Bom.cCurrCode
                        
                      ENDIF
                    ENDIF
                  ENDFOR
                ENDIF                   
              ENDIF 
            ENDSCAN
            IF ALEN(lasizeprice,1) <= 1
              llCstShtPerSize = .F.
            ENDIF 
          ENDIF 
          SELECT (loParentForm.lcPOLine)
          *B610190,1 MMT 01/03/2013 Modify PO screen and Generate PO from SO to Handle price/size in cost sheet[End]
          IF loBom.SEEK('0001'+PADR(SUBSTR(lcStyle,1,loParentForm.lnMajorLen),19)+lcCstShtTyp+lcCstSht_ID)
            SELECT BOM
            SCAN REST WHILE cinvtype+cItmMajor+cCstShtTyp+cCstSht_Id+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6) =;
                            '0001'+PADR(SUBSTR(lcStyle,1,loParentForm.lnMajorLen),19)+lcCstShtTyp+lcCstSht_ID
              IF !LIKE(STRTRAN(cItmMask,'*','?'),lcStyle) .OR. (!EMPTY(MSIZES) .AND. ATCLINE(STYLE.SCALE+'~',MSIZES)=0) .OR. ;
                 (!EMPTY(MSZCROSREF) .AND. ATCLINE(STYLE.SCALE+',',MSZCROSREF)=0)
                LOOP
              ENDIF
              IF Typ = '8'
                LOOP
              ENDIF
              lcCostType = Typ
              STORE '/' TO lcUntSin
              lnExRate   = IIF(ISNULL(nExRate) OR nExRate=0,1,nExRate)
              lnCurrUnit = IIF(ISNULL(nCurrUnit) OR nCurrUnit=0,1,nCurrUnit)
              lcCurrCode = IIF(ISNULL(cCurrCode) OR EMPTY(cCurrCode),oAriaApplication.BaseCurrency,cCurrCode)
              lcExSign = gfGetExSin(@lcUntSin, lcCurrCode)
              lnCost&lcCostType  = EVALUATE(loParentForm.lcPOLine+'.TotQty')*TotCost
              lnECost&lcCostType = lnCost&lcCostType &lcExSign lnExRate &lcUntSin lnCurrUnit
              lnGros_Price       = IIF(lcCostType='1',TotCost,lnGros_Price)
              *B610190,1 MMT 01/03/2013 Modify PO screen and Generate PO from SO to Handle price/size in cost sheet[Start]
              IF !llCstShtPerSize OR  (llCstShtPerSize AND ccatgtyp <> 'P')
              *B610190,1 MMT 01/03/2013 Modify PO screen and Generate PO from SO to Handle price/size in cost sheet[END]
                lnTCost&lcCostType  = lnTCost&lcCostType  + lnECost&lcCostType
                lnTFCost&lcCostType = lnTFCost&lcCostType + lnCost&lcCostType
              *B610190,1 MMT 01/03/2013 Modify PO screen and Generate PO from SO to Handle price/size in cost sheet[Start]
              ENDIF
              *B610190,1 MMT 01/03/2013 Modify PO screen and Generate PO from SO to Handle price/size in cost sheet[END]
            ENDSCAN
            SELECT (loParentForm.lcPOLine)
          ELSE
          *N000587,1 WAM 12/01/2007 (End)

            FOR lnCount = 1 TO 7
              lcCount = STR(lnCount,1)
              DO CASE
                CASE loParentForm.lcIType&lcCount = 'P'
                  lnCost&lcCount = IIF(loParentForm.lcPCurr=STYLE.cPriceCur,TotQty*STYLE.nICost&lcCount,0)
                  lnECost&lcCount= lnCost&lcCount &lcPExSign loParentForm.lnPRate &lcPUntSin loParentForm.lnUnit1
                  lnGros_Price   = IIF(lnCount=1,IIF(loParentForm.lcPCurr=STYLE.cPriceCur,STYLE.nICost1,0),lnGros_Price)
                CASE INLIST(loParentForm.lcIType&lcCount,'M','D')
                  lnCost&lcCount = IIF(loParentForm.lcDCurr=STYLE.cDutyCur,TotQty*STYLE.nICost&lcCount,0)
                  lnECost&lcCount= lnCost&lcCount &lcDExSign loParentForm.lnDRate &lcDUntSin loParentForm.lnUnit2
                  lnGros_Price   = IIF(lnCount=1,IIF(loParentForm.lcDCurr=STYLE.cDutyCur,STYLE.nICost1,0),lnGros_Price)
                OTHERWISE
                  lnCost&lcCount = TotQty*STYLE.nICost&lcCount
                  lnECost&lcCount= lnCost&lcCount
                  lnGros_Price   = IIF(lnCount=1,STYLE.nICost1,lnGros_Price)
              ENDCASE
              lnTCost&lcCount  = lnTCost&lcCount  + lnECost&lcCount
              lnTFCost&lcCount = lnTFCost&lcCount + lnCost&lcCount
            ENDFOR
          *N000587,1 WAM 12/01/2007 Get currency, exchange rate and unit for each cost element from BOM file
          ENDIF
          *N000587,1 WAM 12/01/2007 (End)
          *B610190,1 MMT 01/03/2013 Modify PO screen and Generate PO from SO to Handle price/size in cost sheet[Start]
          IF llCstShtPerSize 
            DIMENSION laCutPikArrAy[1]
            laCutPikArrAy =""
            lnTotQty = 0
            llFirstLine = .T.
            FOR lnSzcnt = 1 TO ALEN(lasizeprice,1)
              lcSizes = lasizeprice[lnSzCnt,2]
              IF llFirstLine 
                lnTotQty = TotQty
                SCATTER MEMO MEMVAR
                SELECT (loParentForm.lcPOH)
                REPLACE LastLine WITH LastLine + 1                          
                SELECT(loParentForm.lcCutPick)
                SCATTER MEMO TO laCutPikArrAy 
                REPLACE cTktLinenO WITH STR(m.LineNO,6)
              ELSE
                APPEND BLANK
                m.LineNO = EVALUATE(loParentForm.lcPOH+".LastLine")
                SELECT (loParentForm.lcPOH)
                REPLACE LastLine WITH LastLine + 1    
                SELECT (loParentForm.lcPOLine)                      
                GATHER MEMO MEMVAR  
                SELECT(loParentForm.lcCutPick)
                APPEND BLANK 
                GATHER FROM laCutPikArrAy MEMO 
                REPLACE cTktLinenO WITH STR(m.LineNO,6)
              ENDIF   
              SELECT (loParentForm.lcPOLine)
              FOR lnCntSize = 1 TO 8
                lcCntSz = STR(lnCntSize,1)
                IF lcCntSz $ lcSizes
                  IF m.Qty&lcCntSz. > 0 
                    m.Qty&lcCntSz. = 0
                  ENDIF 
                ELSE
                  REPLACE Qty&lcCntSz. WITH 0 
                ENDIF
              ENDFOR  
              SELECT(loParentForm.lcCutPick)
              REPLACE QTY1 WITH EVALUATE(loParentForm.lcPOLine+'.qty1'),;
                      QTY2 WITH EVALUATE(loParentForm.lcPOLine+'.qty2'),;
                      QTY3 WITH EVALUATE(loParentForm.lcPOLine+'.qty3'),;
                      QTY4 WITH EVALUATE(loParentForm.lcPOLine+'.qty4'),;
                      QTY5 WITH EVALUATE(loParentForm.lcPOLine+'.qty5'),;
                      QTY6 WITH EVALUATE(loParentForm.lcPOLine+'.qty6'),;                                                                                        
                      QTY7 WITH EVALUATE(loParentForm.lcPOLine+'.qty7'),;
                      QTY8 WITH EVALUATE(loParentForm.lcPOLine+'.qty8'),;
                      TOTQTY WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
              SELECT (loParentForm.lcPOLine)
              REPLACE TOTQTY WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
                      ORD1 WITH MIN(Qty1,Ord1),;
                      ORD2 WITH MIN(Qty2,Ord2),;
                      ORD3 WITH MIN(Qty3,Ord3),;
                      ORD4 WITH MIN(Qty4,Ord4),;
                      ORD5 WITH MIN(Qty5,Ord5),;
                      ORD6 WITH MIN(Qty6,Ord6),;
                      ORD7 WITH MIN(Qty7,Ord7),;
                      ORD8 WITH MIN(Qty8,Ord8),;
                      TotOrd WITH ORD1+ ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
              STORE '/' TO lcUntSin
              lnExRate   = IIF(ISNULL(lasizeprice[lnSzCnt,4]) OR lasizeprice[lnSzCnt,4]=0,1,lasizeprice[lnSzCnt,4])
              lnCurrUnit = IIF(ISNULL(lasizeprice[lnSzCnt,5]) OR lasizeprice[lnSzCnt,5]=0,1,lasizeprice[lnSzCnt,5])
              lcCurrCode = IIF(ISNULL(lasizeprice[lnSzCnt,6]) OR EMPTY(lasizeprice[lnSzCnt,6]),oAriaApplication.BaseCurrency,lasizeprice[lnSzCnt,6])
              lcExSign = gfGetExSin(@lcUntSin, lcCurrCode)
              lnCost1 = EVALUATE(loParentForm.lcPOLine+'.TotQty')*lasizeprice[lnSzCnt,3]
              lnECost1 = lnCost1 &lcExSign lnExRate &lcUntSin lnCurrUnit
              lnGros_Price = lasizeprice[lnSzCnt,3]
              IF lnSzcnt <> ALEN(lasizeprice,1)
                REPLACE nFCost1 WITH nFCost1+ lnCost1   ,;
                        nFCost2 WITH nFCost2+ ((lnCost2* TotQty)/lnTotQty)  ,;
                        nFCost3 WITH nFCost3+ ((lnCost3* TotQty)/lnTotQty)  ,;
                        nFCost4 WITH nFCost4+ ((lnCost4* TotQty)/lnTotQty) ,;
                        nFCost5 WITH nFCost5+ ((lnCost5* TotQty)/lnTotQty)  ,;
                        nFCost6 WITH nFCost6+ ((lnCost6* TotQty)/lnTotQty)  ,;
                        nFCost7 WITH nFCost7+ ((lnCost7* TotQty)/lnTotQty)  ,;
                        Gros_Price WITH lnGros_Price,;
                        nICost1 WITH nICost1+ lnECost1 ,;
                        nICost2 WITH nICost2+ ((lnECost2* TotQty)/lnTotQty) ,;
                        nICost3 WITH nICost3+ ((lnECost3* TotQty)/lnTotQty) ,;
                        nICost4 WITH nICost4+ ((lnECost4* TotQty)/lnTotQty) ,;
                        nICost5 WITH nICost5+ ((lnECost5* TotQty)/lnTotQty) ,;
                        nICost6 WITH nICost6+ ((lnECost6* TotQty)/lnTotQty) ,;
                        nICost7 WITH nICost7+ ((lnECost7* TotQty)/lnTotQty),;
                        nNetPrc WITH lnGros_Price,;
                        Disc_Pcnt WITH 0
                IF loParentForm.llDispPric
                  lnTotCost               = (nICost1+nICost2+nICost3+nICost4+nICost5+nICost6+nICost7)/TotQty
                  loParentForm.lnRotSub   = IIF(loParentForm.llStyMark,lnTotCost, (lnTotQty*EVALUATE(lcOrdFile+'.Price')) )
                  loParentForm.lnGrosMrgn = IIF(loParentForm.lnRotSub=0,0,(((lnTotQty*EVALUATE(lcOrdFile+'.Price'))-lnTotCost)/loParentForm.lnRotSub)*100)
                  REPLACE nSelPrice WITH (lnTotQty*EVALUATE(lcOrdFile+'.Price')) ,;
                          nGrosMrgn WITH loParentForm.lnGrosMrgn
                  STORE 0 TO loParentForm.lnSelPrice,loParentForm.lnAllQty
                ENDIF
                lnOrder   = lnOrder   + TotOrd
                lnAllocat = lnAllocat + TotQty
              ELSE
                FOR lnCstCnt=2 TO 7
                  lcCstCnt = STR(lnCstCnt,1)
                  lnCost&lcCstCnt. = ((lnCost&lcCstCnt. * TotQty)/lnTotQty)
                ENDFOR   
              ENDIF 
              lnTCost1 = lnTCost1+ lnECost1
              lnTFCost1= lnTFCost1+ lnCost1
              llFirstLine =.F.            
            ENDFOR 
            SELECT (loParentForm.lcPOH)
            REPLACE LastLine WITH LastLine - 1 
            SELECT (loParentForm.lcPOLine)
          ENDIF
          *B610190,1 MMT 01/03/2013 Modify PO screen and Generate PO from SO to Handle price/size in cost sheet[END]
          
          REPLACE nFCost1 WITH nFCost1+ lnCost1  ,;
                  nFCost2 WITH nFCost2+ lnCost2  ,;
                  nFCost3 WITH nFCost3+ lnCost3  ,;
                  nFCost4 WITH nFCost4+ lnCost4  ,;
                  nFCost5 WITH nFCost5+ lnCost5  ,;
                  nFCost6 WITH nFCost6+ lnCost6  ,;
                  nFCost7 WITH nFCost7+ lnCost7  ,;
                  Gros_Price WITH lnGros_Price,;
                  nICost1 WITH nICost1+ lnECost1 ,;
                  nICost2 WITH nICost2+ lnECost2 ,;
                  nICost3 WITH nICost3+ lnECost3 ,;
                  nICost4 WITH nICost4+ lnECost4 ,;
                  nICost5 WITH nICost5+ lnECost5 ,;
                  nICost6 WITH nICost6+ lnECost6 ,;
                  nICost7 WITH nICost7+ lnECost7

  		  *! B609028,1 MMT 10/08/2009 Fix bug of can not edit p.price in line detail screen[Start]	
  		  REPLACE nNetPrc WITH lnGros_Price,;
					Disc_Pcnt WITH 0
  		  *! B609028,1 MMT 10/08/2009 Fix bug of can not edit p.price in line detail screen[End]


          IF loParentForm.llDispPric
            lnTotCost               = (nICost1+nICost2+nICost3+nICost4+nICost5+nICost6+nICost7)/TotQty
            loParentForm.lnRotSub   = IIF(loParentForm.llStyMark,lnTotCost, (lnTotQty*EVALUATE(lcOrdFile+'.Price')) )
            loParentForm.lnGrosMrgn = IIF(loParentForm.lnRotSub=0,0,(((lnTotQty*EVALUATE(lcOrdFile+'.Price'))-lnTotCost)/loParentForm.lnRotSub)*100)
            REPLACE nSelPrice WITH (lnTotQty*EVALUATE(lcOrdFile+'.Price')) ,;
                    nGrosMrgn WITH loParentForm.lnGrosMrgn
            STORE 0 TO loParentForm.lnSelPrice,loParentForm.lnAllQty
          ENDIF
          lnOrder   = lnOrder   + TotOrd
          lnAllocat = lnAllocat + TotQty
        ENDIF
        *C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [END]
      ENDSCAN

      *C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [BEGIN]
     *608597,1 ALAA [Begin] Commnetd out to rename varibale llGrpSO to be llRPGrpSO
      *IF loParentForm.llGrpSO
      IF loParentForm.llRPGrpSO
        *C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [END]
        *608597,1 ALAA [END] Commnetd out to rename varibale llGrpSO to be llRPGrpSO
        *! B609917,1 MMT 05/14/2012 Generate PO from SO calculates PO Qty incorrectly  in case of percent <> 100[Start]
        lnCutQty1 = INT(loParentForm.laPercet[1]/100*(lnCutQty1))
        lnCutQty2 = INT(loParentForm.laPercet[2]/100*(lnCutQty2))
        lnCutQty3 = INT(loParentForm.laPercet[3]/100*(lnCutQty3))
        lnCutQty4 = INT(loParentForm.laPercet[4]/100*(lnCutQty4))
        lnCutQty5 = INT(loParentForm.laPercet[5]/100*(lnCutQty5))
        lnCutQty6 = INT(loParentForm.laPercet[6]/100*(lnCutQty6))
        lnCutQty7 = INT(loParentForm.laPercet[7]/100*(lnCutQty7))
        lnCutQty8 = INT(loParentForm.laPercet[8]/100*(lnCutQty8))
        *! B609917,1 MMT 05/14/2012 Generate PO from SO calculates PO Qty incorrectly  in case of percent <> 100[END]

        IF lnCutQty1+lnCutQty2+lnCutQty3+lnCutQty4+lnCutQty5+lnCutQty6+lnCutQty7+lnCutQty8 > 0
          SELECT (loParentForm.lcPOLine)

          IF !SEEK(SPACE(6)+lcDivision+lcPurCode+lcStyGrade+lcStyle+lcDyelot)

            SELECT (loParentForm.lcPOH)
            REPLACE LastLine WITH LastLine + 1

            *: B608058,1 MMT 04/24/20007 fix bug of not saving packing info while generating PO From SO[Start]
            =loParentForm.loStyle.SEEK(lcStyle)
            *: B608058,1 MMT 04/24/20007 fix bug of not saving packing info while generating PO From SO[End]

            SELECT (loParentForm.lcPOLine)
            APPEND BLANK
            REPLACE STYLE      WITH lcStyle    ,;
                    Dyelot     WITH lcDyelot   ,;
                    TRANCD     WITH '1'        ,;
                    cDivision  WITH lcDivision ,;
                    cWareCode  WITH loParentForm.lcWareCode ,;
                    LINENO     WITH EVALUATE(loParentForm.lcPOH+'.LastLine') ,;
                    Vendor     WITH loParentForm.lcVendor   ,;
                    SHIPVIA    WITH loParentForm.lcShip     ,;
                    COMPLETE   WITH EVALUATE(loParentForm.lcPOH+'.Complete'),;
                    cCstSht_Id WITH lfGetCstSht(lcStyle,loParentForm),;
                    cPurCode   WITH lcPurCode  ,;
                    cStyGrade  WITH lcStyGrade ,;
                    Fabric     WITH lcFabric   ,;
                    cFabWare   WITH lcFabWare  ,;
                    nYeild     WITH lnYeild

            *: B608058,1 MMT 04/24/20007 fix bug of not saving packing info while generating PO From SO[Start]
            REPLACE nmspackhgt WITH STYLE.nmspackhgt,;
                    nmspackLEN WITH STYLE.nmspackLEN,;
                    nmspackWDT WITH STYLE.nmspackWDT,;
                    nmspackQTY WITH STYLE.nmspackQTY,;
                    nmspackWGT WITH STYLE.nmspackWGT,;
                    ninpackhgt WITH STYLE.ninpackhgt,;
                    ninpackLEN WITH STYLE.ninpackLEN,;
                    ninpackWDT WITH STYLE.ninpackWDT,;
                    ninpackQTY WITH STYLE.ninpackQTY,;
                    ninpackWGT WITH STYLE.ninpackWGT,;
                    cpacktype  WITH STYLE.cpacktype ,;
                    cpackUom   WITH STYLE.cpackUom
            *: B608058,1 MMT 04/24/20007 fix bug of not saving packing info while generating PO From SO[End]
          *! B610202,1 HIA 01/17/2013 Aria4xp - PO - Generate PO from SO/Open to sell [T20130107.0006][Start]
          REPLACE cvensty WITH STYLE.cvensty
          *! B610202,1 HIA 01/17/2013 Aria4xp - PO - Generate PO from SO/Open to sell [T20130107.0006][End]

          ENDIF
          REPLACE Qty1 WITH lnCutQty1 ,;
                  Qty2 WITH lnCutQty2 ,;
                  Qty3 WITH lnCutQty3 ,;
                  Qty4 WITH lnCutQty4 ,;
                  Qty5 WITH lnCutQty5 ,;
                  Qty6 WITH lnCutQty6 ,;
                  Qty7 WITH lnCutQty7 ,;
                  Qty8 WITH lnCutQty8 ,;
                  ORD1 WITH lnOrdQty1 ,;
                  ORD2 WITH lnOrdQty2 ,;
                  ORD3 WITH lnOrdQty3 ,;
                  ORD4 WITH lnOrdQty4 ,;
                  ORD5 WITH lnOrdQty5 ,;
                  ORD6 WITH lnOrdQty6 ,;
                  ORD7 WITH lnOrdQty7 ,;
                  ORD8 WITH lnOrdQty8 ,;
                  TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
                  TotOrd WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
          *! B609917,1 MMT 05/14/2012 Generate PO from SO calculates PO Qty incorrectly  in case of percent <> 100[Start]
          IF loParentForm.laPercet[1] <> 100 OR loParentForm.laPercet[2] <> 100 OR loParentForm.laPercet[3] <> 100 OR loParentForm.laPercet[3] <> 100;
             OR loParentForm.laPercet[4] <> 100 OR loParentForm.laPercet[6] <> 100 OR loParentForm.laPercet[7] <> 100 OR loParentForm.laPercet[8] <> 100
            FOR lnCntPer = 1 TO 8
              IF loParentForm.laPercet[lnCntPer] <> 100
                lcCntPer = STR(lnCntPer ,1)
                SELECT(loParentForm.lcCutPick)
                lcOrderCutPick = ORDER()
                SET ORDER TO (lcOrderCutPick) DESCENDING
                =SEEK(SPACE(6)+lcDivision+lcPurCode+lcStyGrade+lcStyle+loParentForm.lcWareCode+lcDyelot)
                SUM Qty&lcCntPer. TO lnSumQty REST WHILE CTKTNO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+CWARECODE+DYELOT = ;
                                               SPACE(6)+lcDivision+lcPurCode+lcStyGrade+lcStyle+loParentForm.lcWareCode+lcDyelot
                IF lnSumQty <> EVALUATE(loParentForm.lcPOLine+'.Qty'+lcCntPer)
                  =SEEK(SPACE(6)+lcDivision+lcPurCode+lcStyGrade+lcStyle+loParentForm.lcWareCode+lcDyelot)
                  REPLACE Qty&lcCntPer. WITH Qty&lcCntPer. + (EVALUATE(loParentForm.lcPOLine+'.Qty'+lcCntPer) - lnSumQty),;
                          TotQty  WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 IN (loParentForm.lcCutPick)
                ENDIF
                SELECT(loParentForm.lcCutPick)
                SET ORDER TO (lcOrderCutPick)
              ENDIF
            ENDFOR
          ENDIF
          *! B609917,1 MMT 05/14/2012 Generate PO from SO calculates PO Qty incorrectly  in case of percent <> 100[END]

          =loParentForm.loStyle.SEEK(lcStyle)
          lnGros_Price = 0
          *B608670,1 WAM 08/28/2008 Get currency, exchange rate and unit for each cost element from BOM file
          STORE 0 TO lnCost1, lnCost2,lnCost3,lnCost4,lnCost5,lnCost6,lnCost7
          STORE 0 TO lnECost1, lnECost2, lnECost3, lnECost4, lnECost5, lnECost6, lnECost7
          lcCstShtTyp = IIF(loParentForm.lcChoice = 'P','I','M')
          lcCstSht_ID = EVALUATE(loParentForm.lcPOLine+'.cCstSht_Id')
          *B610190,1 MMT 01/03/2013 Modify PO screen and Generate PO from SO to Handle price/size in cost sheet[Start]
          llCstShtPerSize = loBomHeader.SEEK('0001'+PADR(SUBSTR(lcStyle,1,loParentForm.lnMajorLen),19)+"I"+PADR(lcCstSht_ID,6)) AND BomHeadr.lbasonsiz
          IF llCstShtPerSize AND loBom.SEEK('0001'+PADR(SUBSTR(lcStyle,1,loParentForm.lnMajorLen),19)+lcCstShtTyp+lcCstSht_ID)
            DIMENSION lasizeprice[1,6]
            lasizeprice = ''
            SELECT BOM
            SCAN REST WHILE cinvtype+cItmMajor+cCstShtTyp+cCstSht_Id+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6) =;
                            '0001'+PADR(SUBSTR(lcStyle,1,loParentForm.lnMajorLen),19)+lcCstShtTyp+lcCstSht_ID  FOR ccatgtyp = 'P' 
              IF !LIKE(STRTRAN(cItmMask,'*','?'),lcStyle) .OR. (!EMPTY(MSIZES) .AND. ATCLINE(STYLE.SCALE+'~',MSIZES)=0) .OR. ;
                 (!EMPTY(MSZCROSREF) .AND. ATCLINE(STYLE.SCALE+',',MSZCROSREF)=0)
                LOOP
              ENDIF
              IF !EMPTY(BOM.mSizes)
                =SEEK(lcStyle,'STYLE','STYLE')
                lnMSizeLines = MEMLINES(BOM.mSizes)
                IF lnMSizeLines > 0
                  FOR lnCntLine =  1 TO lnMSizeLines
                    lcMSizes = MLINE(BOM.mSizes,lnCntLine)
                    IF !EMPTY(lcMSizes)
                      lnWPos = ATC('~',lcMSizes)
                      IF !(Style.Scale $ lcMSizes)
                        LOOP
                      ENDIF
                      lnScales = SUBSTR(lcMSizes,lnWPos+1)
                      IF EMPTY(lasizeprice[1,1])
                        lasizeprice[1,1]= SUBSTR(lcMSizes,1,lnWPos-1)
                        lasizeprice[1,2]= lnScales
                        lasizeprice[1,3]= Bom.TotCost
                        lasizeprice[1,4]= Bom.nExRate
                        lasizeprice[1,5]= Bom.nCurrUnit
                        lasizeprice[1,6]= Bom.cCurrCode
                      ELSE
                        lnPosInArr = ALEN(lasizeprice,1)
                        DIMENSION lasizeprice[lnPosInArr+1,6]
                        lasizeprice[lnPosInArr+1,1]= SUBSTR(lcMSizes,1,lnWPos-1)
                        lasizeprice[lnPosInArr+1,2]= lnScales
                        lasizeprice[lnPosInArr+1,3]= Bom.TotCost
                        lasizeprice[lnPosInArr+1,4]= Bom.nExRate
                        lasizeprice[lnPosInArr+1,5]= Bom.nCurrUnit
                        lasizeprice[lnPosInArr+1,6]= Bom.cCurrCode
                        
                      ENDIF
                    ENDIF
                  ENDFOR
                ENDIF                   
              ENDIF 
            ENDSCAN
            IF ALEN(lasizeprice,1) <= 1
              llCstShtPerSize = .F.
            ENDIF 
          ENDIF 
          SELECT (loParentForm.lcPOLine)
          *B610190,1 MMT 01/03/2013 Modify PO screen and Generate PO from SO to Handle price/size in cost sheet[END]
          IF loBom.SEEK('0001'+PADR(SUBSTR(lcStyle,1,loParentForm.lnMajorLen),19)+lcCstShtTyp+lcCstSht_ID)
            SELECT BOM
            SCAN REST WHILE cinvtype+cItmMajor+cCstShtTyp+cCstSht_Id+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6) =;
                            '0001'+PADR(SUBSTR(lcStyle,1,loParentForm.lnMajorLen),19)+lcCstShtTyp+lcCstSht_ID
              IF !LIKE(STRTRAN(cItmMask,'*','?'),lcStyle) .OR. (!EMPTY(MSIZES) .AND. ATCLINE(STYLE.SCALE+'~',MSIZES)=0) .OR. ;
                 (!EMPTY(MSZCROSREF) .AND. ATCLINE(STYLE.SCALE+',',MSZCROSREF)=0)
                LOOP
              ENDIF
              IF Typ = '8'
                LOOP
              ENDIF
              lcCostType = Typ
              STORE '/' TO lcUntSin
              lnExRate   = IIF(ISNULL(nExRate) OR nExRate=0,1,nExRate)
              lnCurrUnit = IIF(ISNULL(nCurrUnit) OR nCurrUnit=0,1,nCurrUnit)
              lcCurrCode = IIF(ISNULL(cCurrCode) OR EMPTY(cCurrCode),oAriaApplication.BaseCurrency,cCurrCode)
              lcExSign = gfGetExSin(@lcUntSin, lcCurrCode)
              lnCost&lcCostType  = EVALUATE(loParentForm.lcPOLine+'.TotQty')*TotCost
              lnECost&lcCostType = lnCost&lcCostType &lcExSign lnExRate &lcUntSin lnCurrUnit
              lnGros_Price       = IIF(lcCostType='1',TotCost,lnGros_Price)
              *B610190,1 MMT 01/03/2013 Modify PO screen and Generate PO from SO to Handle price/size in cost sheet[Start]
              IF !llCstShtPerSize OR  (llCstShtPerSize AND ccatgtyp <> 'P')              
              *B610190,1 MMT 01/03/2013 Modify PO screen and Generate PO from SO to Handle price/size in cost sheet[END]
                lnTCost&lcCostType  = lnTCost&lcCostType  + lnECost&lcCostType
                lnTFCost&lcCostType = lnTFCost&lcCostType + lnCost&lcCostType
              *B610190,1 MMT 01/03/2013 Modify PO screen and Generate PO from SO to Handle price/size in cost sheet[Start]
              ENDIF
              *B610190,1 MMT 01/03/2013 Modify PO screen and Generate PO from SO to Handle price/size in cost sheet[END]
            ENDSCAN
            SELECT (loParentForm.lcPOLine)
          ELSE
          *B608670,1 WAM 08/28/2008 (End)

            FOR lnCount = 1 TO 7
              lcCount = STR(lnCount,1)
              DO CASE
                CASE loParentForm.lcIType&lcCount = 'P'
                  lnCost&lcCount = IIF(loParentForm.lcPCurr=STYLE.cPriceCur,TotQty*STYLE.nICost&lcCount,0)
                  lnECost&lcCount= lnCost&lcCount &lcPExSign loParentForm.lnPRate &lcPUntSin loParentForm.lnUnit1
                  lnGros_Price   = IIF(lnCount=1,IIF(loParentForm.lcPCurr=STYLE.cPriceCur,STYLE.nICost1,0),lnGros_Price)
                CASE INLIST(loParentForm.lcIType&lcCount,'M','D')
                  lnCost&lcCount = IIF(loParentForm.lcDCurr=STYLE.cDutyCur,TotQty*STYLE.nICost&lcCount,0)
                  lnECost&lcCount= lnCost&lcCount &lcDExSign loParentForm.lnDRate &lcDUntSin loParentForm.lnUnit2
                  lnGros_Price   = IIF(lnCount=1,IIF(loParentForm.lcDCurr=STYLE.cDutyCur,STYLE.nICost1,0),lnGros_Price)
                OTHERWISE
                  lnCost&lcCount = TotQty*STYLE.nICost&lcCount
                  lnECost&lcCount= lnCost&lcCount
                  lnGros_Price   = IIF(lnCount=1,STYLE.nICost1,lnGros_Price)
              ENDCASE
              lnTCost&lcCount  = lnTCost&lcCount  + lnECost&lcCount
              lnTFCost&lcCount = lnTFCost&lcCount + lnCost&lcCount
            ENDFOR
          *B608670,1 WAM 08/28/2008 Get currency, exchange rate and unit for each cost element from BOM file
          ENDIF
          *B608670,1 WAM 08/28/2008 (End)
          *B610190,1 MMT 01/03/2013 Modify PO screen and Generate PO from SO to Handle price/size in cost sheet[Start]
          IF llCstShtPerSize 
            DIMENSION laCutPikArrAy[1]
            laCutPikArrAy =""
            * SELECT(loParentForm.lcCutPick)
            llFirstLine = .T.
            FOR lnSzcnt = 1 TO ALEN(lasizeprice,1)
              lcSizes = lasizeprice[lnSzCnt,2]
              IF llFirstLine 
                lnTotQty = TotQty
                SCATTER MEMO MEMVAR
                SELECT (loParentForm.lcPOH)
                REPLACE LastLine WITH LastLine + 1                          
                SELECT(loParentForm.lcCutPick)
                SCATTER MEMO TO laCutPikArrAy 
              ELSE
                APPEND BLANK
                m.LineNO = EVALUATE(loParentForm.lcPOH+".LastLine")
                SELECT (loParentForm.lcPOH)
                REPLACE LastLine WITH LastLine + 1    
                SELECT (loParentForm.lcPOLine)                      
                GATHER MEMO MEMVAR  
                SELECT(loParentForm.lcCutPick)
                APPEND BLANK 
                GATHER FROM laCutPikArrAy MEMO 
                REPLACE cTktLinenO WITH STR(m.LineNO,6)
              ENDIF   
              SELECT (loParentForm.lcPOLine)
              FOR lnCntSize = 1 TO 8
                lcCntSz = STR(lnCntSize,1)
                IF lcCntSz $ lcSizes
                  IF m.Qty&lcCntSz. > 0 
                    m.Qty&lcCntSz. = 0
                  ENDIF 
                ELSE
                  REPLACE Qty&lcCntSz. WITH 0 
                ENDIF
              ENDFOR  
              SELECT(loParentForm.lcCutPick)
              REPLACE QTY1 WITH EVALUATE(loParentForm.lcPOLine+'.qty1'),;
                      QTY2 WITH EVALUATE(loParentForm.lcPOLine+'.qty2'),;
                      QTY3 WITH EVALUATE(loParentForm.lcPOLine+'.qty3'),;
                      QTY4 WITH EVALUATE(loParentForm.lcPOLine+'.qty4'),;
                      QTY5 WITH EVALUATE(loParentForm.lcPOLine+'.qty5'),;
                      QTY6 WITH EVALUATE(loParentForm.lcPOLine+'.qty6'),;                                                                                        
                      QTY7 WITH EVALUATE(loParentForm.lcPOLine+'.qty7'),;
                      QTY8 WITH EVALUATE(loParentForm.lcPOLine+'.qty8'),;
                      TOTQTY WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
              SELECT (loParentForm.lcPOLine)
              REPLACE TOTQTY WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
                      ORD1 WITH MIN(Qty1,Ord1),;
                      ORD2 WITH MIN(Qty2,Ord2),;
                      ORD3 WITH MIN(Qty3,Ord3),;
                      ORD4 WITH MIN(Qty4,Ord4),;
                      ORD5 WITH MIN(Qty5,Ord5),;
                      ORD6 WITH MIN(Qty6,Ord6),;
                      ORD7 WITH MIN(Qty7,Ord7),;
                      ORD8 WITH MIN(Qty8,Ord8),;
                      TotOrd WITH ORD1+ ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
              STORE '/' TO lcUntSin
              lnExRate   = IIF(ISNULL(lasizeprice[lnSzCnt,4]) OR lasizeprice[lnSzCnt,4]=0,1,lasizeprice[lnSzCnt,4])
              lnCurrUnit = IIF(ISNULL(lasizeprice[lnSzCnt,5]) OR lasizeprice[lnSzCnt,5]=0,1,lasizeprice[lnSzCnt,5])
              lcCurrCode = IIF(ISNULL(lasizeprice[lnSzCnt,6]) OR EMPTY(lasizeprice[lnSzCnt,6]),oAriaApplication.BaseCurrency,lasizeprice[lnSzCnt,6])
              lcExSign = gfGetExSin(@lcUntSin, lcCurrCode)
              lnCost1 = EVALUATE(loParentForm.lcPOLine+'.TotQty')*lasizeprice[lnSzCnt,3]
              lnECost1 = lnCost1 &lcExSign lnExRate &lcUntSin lnCurrUnit
              lnGros_Price = lasizeprice[lnSzCnt,3]
              IF lnSzcnt <> ALEN(lasizeprice,1)
                REPLACE nFCost1 WITH nFCost1+ lnCost1  ,;
                        nFCost2 WITH nFCost2+ ((lnCost2  * TotQty)/lnTotQty) ,;
                        nFCost3 WITH nFCost3+ ((lnCost3 * TotQty)/lnTotQty)  ,;
                        nFCost4 WITH nFCost4+ ((lnCost4  * TotQty)/lnTotQty) ,;
                        nFCost5 WITH nFCost5+ ((lnCost5  * TotQty)/lnTotQty) ,;
                        nFCost6 WITH nFCost6+ ((lnCost6  * TotQty)/lnTotQty) ,;
                        nFCost7 WITH nFCost7+ ((lnCost7  * TotQty)/lnTotQty) ,;
                        Gros_Price WITH lnGros_Price,;
                        nICost1 WITH nICost1+ lnECost1 ,;
                        nICost2 WITH nICost2+ ((lnECost2 * TotQty)/lnTotQty) ,;
                        nICost3 WITH nICost3+ ((lnECost3  * TotQty)/lnTotQty),;
                        nICost4 WITH nICost4+ ((lnECost4  * TotQty)/lnTotQty),;
                        nICost5 WITH nICost5+ ((lnECost5  * TotQty)/lnTotQty),;
                        nICost6 WITH nICost6+ ((lnECost6  * TotQty)/lnTotQty),;
                        nICost7 WITH nICost7+ ((lnECost7 * TotQty)/lnTotQty),;
                        nNetPrc WITH lnGros_Price,;
                        Disc_Pcnt WITH 0
                IF loParentForm.llDispPric
                  lnTotCost               = (nICost1+nICost2+nICost3+nICost4+nICost5+nICost6+nICost7)/TotQty
                  loParentForm.lnRotSub   = IIF(loParentForm.llStyMark,lnTotCost, (lnTotQty*EVALUATE(lcOrdFile+'.Price')) )
                  loParentForm.lnGrosMrgn = IIF(loParentForm.lnRotSub=0,0,(((lnTotQty*EVALUATE(lcOrdFile+'.Price'))-lnTotCost)/loParentForm.lnRotSub)*100)
                  REPLACE nSelPrice WITH (lnTotQty*EVALUATE(lcOrdFile+'.Price')) ,;
                          nGrosMrgn WITH loParentForm.lnGrosMrgn
                  STORE 0 TO loParentForm.lnSelPrice,loParentForm.lnAllQty
                ENDIF
                lnOrder   = lnOrder   + TotOrd
                lnAllocat = lnAllocat + TotQty
              ELSE
                FOR lnCstCnt=2 TO 7
                  lcCstCnt = STR(lnCstCnt,1)
                  lnCost&lcCstCnt. = ((lnCost&lcCstCnt. * TotQty)/lnTotQty)
                ENDFOR   
              ENDIF 
              lnTCost1 = lnTCost1+ lnECost1
              lnTFCost1= lnTFCost1+ lnCost1
              llFirstLine =.F.            
            ENDFOR 
            SELECT (loParentForm.lcPOH)
            REPLACE LastLine WITH LastLine - 1 
            SELECT (loParentForm.lcPOLine)
          ENDIF
          *B610190,1 MMT 01/03/2013 Modify PO screen and Generate PO from SO to Handle price/size in cost sheet[END]

          REPLACE nFCost1 WITH nFCost1+ lnCost1  ,;
                  nFCost2 WITH nFCost2+ lnCost2  ,;
                  nFCost3 WITH nFCost3+ lnCost3  ,;
                  nFCost4 WITH nFCost4+ lnCost4  ,;
                  nFCost5 WITH nFCost5+ lnCost5  ,;
                  nFCost6 WITH nFCost6+ lnCost6  ,;
                  nFCost7 WITH nFCost7+ lnCost7  ,;
                  Gros_Price WITH lnGros_Price,;
                  nICost1 WITH nICost1+ lnECost1 ,;
                  nICost2 WITH nICost2+ lnECost2 ,;
                  nICost3 WITH nICost3+ lnECost3 ,;
                  nICost4 WITH nICost4+ lnECost4 ,;
                  nICost5 WITH nICost5+ lnECost5 ,;
                  nICost6 WITH nICost6+ lnECost6 ,;
                  nICost7 WITH nICost7+ lnECost7
			
  		  *! B609028,1 MMT 10/08/2009 Fix bug of can not edit p.price in line detail screen[Start]	
  		  REPLACE nNetPrc WITH lnGros_Price,;
					Disc_Pcnt WITH 0
  		  *! B609028,1 MMT 10/08/2009 Fix bug of can not edit p.price in line detail screen[End]
			
          IF loParentForm.llDispPric
            lnTotCost               = (nICost1+nICost2+nICost3+nICost4+nICost5+nICost6+nICost7)/TotQty
            loParentForm.lnRotSub   = IIF(loParentForm.llStyMark,lnTotCost,loParentForm.lnSelPrice)
            loParentForm.lnGrosMrgn = IIF(loParentForm.lnRotSub=0,0,((loParentForm.lnSelPrice-lnTotCost)/loParentForm.lnRotSub)*100)
            REPLACE nSelPrice WITH loParentForm.lnSelPrice ,;
                    nGrosMrgn WITH loParentForm.lnGrosMrgn
            STORE 0 TO loParentForm.lnSelPrice,loParentForm.lnAllQty
          ENDIF
          lnOrder   = lnOrder   + TotOrd
          lnAllocat = lnAllocat + TotQty
        ENDIF
        *C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [BEGIN]
        *608597,1 ALAA [Begin] Commnetd out to rename varibale llGrpSO to be llRPGrpSO
      ENDIF
      *C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [END]
      *608597,1 ALAA [END] Commnetd out to rename varibale llGrpSO to be llRPGrpSO
      SELECT (lcOrdFile)
    ENDDO
    SELECT (loParentForm.lcPOH)
    REPLACE nStyOrder WITH lnAllocat ,;
            TotOrd    WITH lnOrder   ,;
            nFCost1   WITH lnTFCost1 ,;
            nFCost2   WITH lnTFCost2 ,;
            nFCost3   WITH lnTFCost3 ,;
            nFCost4   WITH lnTFCost4 ,;
            nFCost5   WITH lnTFCost5 ,;
            nFCost6   WITH lnTFCost6 ,;
            nFCost7   WITH lnTFCost7 ,;
            TOTCOST   WITH nFCost1+nFCost2+nFCost3+nFCost4+nFCost5+nFCost6+nFCost7,;
            nICost1   WITH lnTCost1  ,;
            nICost2   WITH lnTCost2  ,;
            nICost3   WITH lnTCost3  ,;
            nICost4   WITH lnTCost4  ,;
            nICost5   WITH lnTCost5  ,;
            nICost6   WITH lnTCost6  ,;
            nICost7   WITH lnTCost7
    IF nStyOrder = 0
      DELETE
    ENDIF
    SELECT (lcOrdFile)
  ENDDO

  loBomHeader.DESTROY

  loParentForm.loStyle.SETORDER(lcStyOrd)
  SET ORDER TO TAG (lcOrdTag) IN (lcOrdFile)
  SELECT (lnAlias)
  WAIT CLEAR

  *!*************************************************************
  *! Name      : lfGetFab
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/20/2004
  *! Purpose   : Get order lines styles primary fabrics
  *!*************************************************************
  *! Calls     : None
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None.
  *!*************************************************************
  *! Example   :  =lfGetFab()
  *!*************************************************************
FUNCTION lfGetFab
  LPARAMETERS loFormSet

  LOCAL lnAlias,loDye_Rel,loFabric,loFabDye,loBom,lcCstShtTyp,lnMatMjrLen,lnMatClrStr,lnMatClrLen,lcPoOrder
  lnAlias = SELECT(0)
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *WAIT LANG_MFGENCT_PRIMFABREQU WINDOW NOWAIT
  WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_PRIMFABREQU,loFormSet.GetHeaderText("LANG_MFGENCT_PRIMFABREQU",loFormSet.HeaderAlias)) WINDOW NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[End]


  IF loFormSet.llFDyelot
    loDye_Rel = CREATEOBJECT('RemoteTable','DYE_REL','DYE_REL','DYE_REL',loFormSet.DATASESSIONID)
  ENDIF
  loFabric = CREATEOBJECT('RemoteTable','ITEM','STYLE','FABRIC',loFormSet.DATASESSIONID)
  loFabDye = CREATEOBJECT('RemoteTable','ITEMLOC','STYDYE','FABDYE',loFormSet.DATASESSIONID)
  loBom    = CREATEOBJECT('RemoteTable','Bom','MULTIBOM','Bom',loFormSet.DATASESSIONID)

  PRIVATE loBomHeader
  loBomHeader = CREATEOBJECT('RemoteTable','BomHeadr','BomHeadr','BomHeadr',loFormSet.DATASESSIONID)

  *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
  *IF loFormSet.llRPUnAWip AND loFormSet.lcChoice = 'P'
  IF loFormSet.llRPUnAWip AND INLIST(loFormSet.lcChoice ,'P','O')
  *N000606,1 NNA (END)

    loFormSet.loPOSLN = IIF(TYPE('loFormSet.loPOSLN')='O',loFormSet.loPOSLN,CREATEOBJECT('RemoteTable','POSLN','POSLNS','POSLN',loFormSet.DATASESSIONID))
    SELECT POSLN
    lcPoOrder = ORDER()
    loFormSet.loPOSLN.SETORDER('POSLNS')
  ENDIF

  *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
  *lcCstShtTyp = IIF(loFormSet.lcChoice = 'P','I','M')
  lcCstShtTyp = IIF(INLIST(loFormSet.lcChoice,'P','O'),'I','M')
  *N000606,1 NNA (END)

  lnMatMjrLen = LEN(gfItemMask("PM",'','0002'))

  DECLARE laMatSeg[1]
  =gfItemMask(@laMatSeg,'','0002')
  FOR lnCnt = 1 TO ALEN(laMatSeg,1)
    IF laMatSeg[lnCnt , 1] = "C"
      lnMatClrStr = laMatSeg[lnCnt , 4]
      lnMatClrLen = LEN(laMatSeg[lnCnt , 3])
    ENDIF
  ENDFOR

  loFormSet.loStyle.SETORDER('STYLE')
  SELECT (loFormSet.lcOrdLine)
  SET ORDER TO TAG 'TICKET'
  =SEEK("�")
  SCAN REST WHILE cSelect = "�"
    lnReqQty = TotQty
    lcStyle  = STYLE
    =loFormSet.loStyle.SEEK(lcStyle)
    lcCstSht_ID = lfGetCstSht(lcStyle,loFormSet)
    SELECT BOM
    =loBom.SEEK('0001'+PADR(SUBSTR(lcStyle,1,loFormSet.lnMajorLen),19)+lcCstShtTyp+lcCstSht_ID)
    LOCATE REST WHILE cinvtype+cItmMajor+cCstShtTyp+cCstSht_Id+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6) =;
      '0001'+PADR(SUBSTR(lcStyle,1,loFormSet.lnMajorLen),19)+lcCstShtTyp+lcCstSht_ID;
      FOR   (cCatgTyp='F' OR (cCatgTyp='T' AND trim_invt)) AND;
      PADR(ITEM,lnMatMjrLen) = PADR(STYLE.Fabric,lnMatMjrLen) AND ;
      LIKE(STRTRAN(CITMMASK,'*','?'),lcStyle) AND ;
      IIF(!EMPTY(MSIZES),ATCLINE(STYLE.SCALE+'~',MSIZES)<>0,.T.) AND ;
      IIF(!EMPTY(MSZCROSREF),ATCLINE(STYLE.SCALE+',',MSZCROSREF)<>0,.T.)
    lcItem = IIF(SUBSTR(ITEM,lnMatClrStr,lnMatClrLen)=REPLICATE('*',lnMatClrLen),;
      STUFF(ITEM,lnMatClrStr,lnMatClrLen,SUBSTR(lcStyle,loFormSet.lnColorStr,loFormSet.lnColorLen)),ITEM)
    STORE 0 TO lnYardage
    SCAN REST WHILE cinvtype+cItmMajor+cCstShtTyp+cCstSht_Id+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6) = ;
        '0001'+PADR(SUBSTR(lcStyle,1,loFormSet.lnMajorLen),19)+lcCstShtTyp+lcCstSht_ID;
        FOR   (cCatgTyp='F' OR (cCatgTyp='T' AND trim_invt)) AND LIKE(STRTRAN(ITEM,'*','?'),lcItem)
      IF !LIKE(STRTRAN(CITMMASK,'*','?'),lcStyle) .OR. ;
          (!EMPTY(MSIZES) .AND. ATCLINE(EVALUATE(loFormSet.lcOrdLine+'.Scale')+'~',MSIZES)=0) .OR. ;
          (!EMPTY(MSZCROSREF) .AND. ATCLINE(EVALUATE(loFormSet.lcOrdLine+'.Scale')+',',MSZCROSREF)=0)
        LOOP
      ENDIF
      lcSizes = ''
      IF !EMPTY(MSIZES)
        lcSizes = SUBSTR(MLINE(MSIZES,ATCLINE(EVALUATE(loFormSet.lcOrdLine+'.Scale')+'~',MSIZES)),5)
      ENDIF
      lnQuantity = 0
      FOR lnCount = 1 TO 8
        lnQuantity = lnQuantity + IIF(EMPTY(lcSizes) OR (STR(lnCount,1) $ lcSizes),;
          EVALUATE(loFormSet.lcOrdLine+'.Qty'+STR(lnCount,1))-EVALUATE(loFormSet.lcOrdLine+'.Cut'+STR(lnCount,1)),0)
      ENDFOR
      lnYardage = lnYardage + lnQuantity*nBomTotQty
    ENDSCAN
    lnYeild = IIF(lnReqQty=0,0,lnYardage/lnReqQty)
    SELECT (loFormSet.lcOrdGroup)
    IF !SEEK(lcItem+EVALUATE(loFormSet.lcOrdLine+'.Order+'+loFormSet.lcOrdLine+'.Store+'+loFormSet.lcOrdLine+'.Group'))
      APPEND BLANK
      REPLACE Fabric WITH lcItem                                ,;
        ORDER  WITH EVALUATE(loFormSet.lcOrdLine+'.Order'),;
        STORE  WITH EVALUATE(loFormSet.lcOrdLine+'.Store'),;
        GROUP  WITH EVALUATE(loFormSet.lcOrdLine+'.Group')
    ENDIF
    REPLACE nRequired WITH nRequired + lnYardage
    SELECT (loFormSet.lcOrdLine)
    =RLOCK()
    REPLACE Fabric    WITH lcItem   ,;
      nRequired WITH lnYardage,;
      nYeild    WITH lnYeild
    UNLOCK
    SELECT (loFormSet.lcFabrics)
    IF !SEEK(lcItem)
      SELECT FABDYE
      =loFabDye.SEEK('0002'+lcItem)
      lnPriority = 0
      SCAN REST WHILE cinvtype+STYLE+cWareCode+Dyelot = '0002'+lcItem;
          FOR   IIF(loFormSet.llFDyelot,!EMPTY(Dyelot),.T.)
        SCATTER MEMVAR FIELDS STYLE,cWareCode,Dyelot,TotStk
        m.Fabric = m.STYLE
        m.OnHand = m.TotStk
        IF loFormSet.llFDyelot AND SEEK('0002'+m.STYLE+m.Dyelot,'Dye_Rel')
          m.cPriority = Dye_Rel.cDye_Seq
        ELSE
          lnPriority  = lnPriority + 1
          m.cPriority = lnPriority
        ENDIF
        m.nUnAllWIP = 0

        *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
        *IF loFormSet.llRPUnAWip AND loFormSet.lcChoice = 'P'
        IF loFormSet.llRPUnAWip AND INLIST(loFormSet.lcChoice,'P','O')
        *N000606,1 NNA (END)

          SELECT POSLN
          =loFormSet.loPOSLN.SEEK('0001'+lcStyle)
          SUM REST TotQty-TotOrd TO m.nUnAllWIP ;
            WHILE cinvtype+STYLE+cbusdocu+cstytype+PO+STR(LINENO,6)+TRANCD = '0001'+lcStyle ;
            FOR TRANCD='1' AND;
            IIF(loFormSet.loPosHdr.SEEK(cbusdocu+cstytype+PO),!INLIST(POSHDR.STATUS,'X','C'),.F.) AND Dyelot=m.Dyelot
        ENDIF
        m.nUnAllWIP = m.nUnAllWIP*lnYeild
        SELECT (loFormSet.lcFabrics)
        APPEND BLANK
        GATHER MEMVAR FIELDS Fabric,cWareCode,Dyelot,OnHand,cPriority,nUnAllWIP
        SELECT (loFormSet.lcStyUnAll)
        IF !SEEK(lcStyle+m.Dyelot)
          INSERT INTO (loFormSet.lcStyUnAll) (STYLE,Dyelot) VALUES (lcStyle,m.Dyelot)
        ENDIF
        REPLACE nUnAllWIP WITH nUnAllWIP + m.nUnAllWIP
        SELECT (loFormSet.lcOrdGroup)
        REPLACE nUnAllWIP WITH nUnAllWIP + m.nUnAllWIP
      ENDSCAN
    ENDIF
  ENDSCAN
  SET ORDER TO TAG (loFormSet.lcOrdLine) IN (loFormSet.lcOrdLine)
  GO TOP IN (loFormSet.lcOrdLine)

  *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
  *IF loFormSet.llRPUnAWip AND loFormSet.lcChoice = 'P'
  IF loFormSet.llRPUnAWip AND INLIST(loFormSet.lcChoice ,'P','O')
  *N000606,1 NNA (END)

    SELECT POSLN
    loFormSet.loPOSLN.SETORDER(lcPoOrder)
  ENDIF
  SELECT (lnAlias)
  WAIT CLEAR

  *!*************************************************************
  *! Name      : lfClrAllo
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/20/2004
  *! Purpose   : Clear Allocation
  *!*************************************************************
  *! Calls     : None
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   :  None.
  *!*************************************************************
  *! Example   :  =lfClrAllo()
  *!*************************************************************
FUNCTION lfClrAllo
  LPARAMETERS loFormSet

  *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
  *IF loFormSet.lcChoice <> 'L'
  IF !INLIST(loFormSet.lcChoice,'L','O')
  *N000606,1 NNA (End)
    SELECT (loFormSet.lcOrdGroup)
    DELETE ALL
    SELECT (loFormSet.lcFabrics)
    DELETE ALL
    SELECT (loFormSet.lcStyUnAll)
    DELETE ALL
    SELECT (loFormSet.lcTmpOrd)
    DELETE ALL
  ENDIF


  *!*************************************************************
  *! Name      : lfUnAllCt
  *! Developer : AHMED MAHER
  *! Date      : 04/26/205
  *! Purpose   : Allocate order lines to existing unallocated C/Ts
  *!*************************************************************
  *! Calls     : None
  *!*************************************************************
  *! Parameters: lcOrdFile : Order line temporary file name
  *!*************************************************************
  *! Returns   :  None.
  *!*************************************************************
  *! Example   :  =lfUnAllCt()
  *!*************************************************************
FUNCTION lfUnAllCt
  LPARAMETERS lcOrdFile

  LOCAL lnAlias,lnReqQty1,lnReqQty2,lnReqQty3,lnReqQty4,lnReqQty5,lnReqQty6,;
    lnReqQty7,lnReqQty8,lnTotReq,lnOrd1,lnOrd2,lnOrd3,lnOrd4,lnOrd5,;
    lnOrd6,lnOrd7,lnOrd8,lnCut1,lnCut2,lnCut3,lnCut4,lnCut5,lnCut6,;
    lnCut7,lnCut8,lnTotCut

  lnAlias = SELECT(0)
  loParentForm.loCUTPICK.SETORDER('CUTPICK')
  loParentForm.loPOSLN.SETORDER('POSLNS')
  SELECT POSLN

  SELECT (lcOrdFile)
  =SEEK("�")
  SCAN REST WHILE cSelect+cDivision+SEASON+STYLE+Dyelot="�"
    =loParentForm.loStyle.SEEK(STYLE)
    lnReqQty1 = INT(loParentForm.laPercet[1]/100*(Qty1-Cut1))
    lnReqQty2 = INT(loParentForm.laPercet[2]/100*(Qty2-Cut2))
    lnReqQty3 = INT(loParentForm.laPercet[3]/100*(Qty3-Cut3))
    lnReqQty4 = INT(loParentForm.laPercet[4]/100*(Qty4-Cut4))
    lnReqQty5 = INT(loParentForm.laPercet[5]/100*(Qty5-Cut5))
    lnReqQty6 = INT(loParentForm.laPercet[6]/100*(Qty6-Cut6))
    lnReqQty7 = INT(loParentForm.laPercet[7]/100*(Qty7-Cut7))
    lnReqQty8 = INT(loParentForm.laPercet[8]/100*(Qty8-Cut8))
    lnTotReq  = lnReqQty1+lnReqQty2+lnReqQty3+lnReqQty4+lnReqQty5+lnReqQty6+lnReqQty7+lnReqQty8
    STORE 0 TO lnOrd1,lnOrd2,lnOrd3,lnOrd4,lnOrd5,lnOrd6,lnOrd7,lnOrd8
    IF loParentForm.loPOSLN.SEEK('0001'+STYLE+'PU')
      SELECT POSLN
      SCAN REST WHILE cinvtype+STYLE+cbusdocu+cstytype+PO+STR(LINENO,6)+TRANCD = '0001'+EVALUATE(lcOrdFile+'.Style')+'PU';
          FOR   IIF(loParentForm.llRpBOPFab,Dyelot=EVALUATE(lcOrdFile+'.Dyelot'),.T.) AND TRANCD='1' AND ;
          TotQty > TotOrd AND lnTotReq > 0 AND loParentForm.loPosHdr.SEEK(cbusdocu+cstytype+PO) AND !INLIST(POSHDR.STATUS,'X','C')
        SELECT (loParentForm.lcPOLine)
        llFound=SEEK(POSLN.PO)
        =SEEK(POSLN.PO+EVALUATE(lcOrdFile+'.cDivision')+EVALUATE(lcOrdFile+'.Season')+POSLN.STYLE+POSLN.Dyelot)
        SUM REST  TotOrd TO lnAddOrd ;
          WHILE PO+cDivision+SEASON+STYLE= ;
          POSLN.PO+EVALUATE(lcOrdFile+'.cDivision')+EVALUATE(lcOrdFile+'.Season')+POSLN.STYLE+POSLN.Dyelot
        IF POSLN.TotQty <= POSLN.TotOrd+lnAddOrd OR (!llFound AND POSHDR.FLAG='Y')
          LOOP
        ENDIF
        IF !llFound
          SELECT POSHDR
          loParentForm.loPosHdr.REPLACE("FLAG WITH 'Y'")

          LOCAL lnConnectionHandlar,lcTranCode,llUpdate
          lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
          IF TYPE('lcTranCode') = 'N'
            llUpdate = loParentForm.loPosHdr.TABLEUPDATE(lcTranCode)
            IF !llUpdate
              =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
            ELSE
              lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
              IF lnConnectionHandlar # 1
                =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        SELECT POSLN
        lnCut1 = MIN(Qty1-ORD1,lnReqQty1)
        lnCut2 = MIN(Qty2-ORD2,lnReqQty2)
        lnCut3 = MIN(Qty3-ORD3,lnReqQty3)
        lnCut4 = MIN(Qty4-ORD4,lnReqQty4)
        lnCut5 = MIN(Qty5-ORD5,lnReqQty5)
        lnCut6 = MIN(Qty6-ORD6,lnReqQty6)
        lnCut7 = MIN(Qty7-ORD7,lnReqQty7)
        lnCut8 = MIN(Qty8-ORD8,lnReqQty8)
        lnTotCut=lnCut1+lnCut2+lnCut3+lnCut4+lnCut5+lnCut6+lnCut7+lnCut8
        SELECT (loParentForm.lcCutPick)
        APPEND BLANK
        REPLACE cTKtNo    WITH POSLN.PO ,;
          TRANCD    WITH '1'      ,;
          cDivision WITH EVALUATE(lcOrdFile+'.cDivision'),;
          SEASON    WITH EVALUATE(lcOrdFile+'.Season'),;
          ORDER     WITH EVALUATE(lcOrdFile+'.ORDER'),;
          cOrdLine  WITH STR(EVALUATE(lcOrdFile+'.LINENO'),6),;
          STYLE     WITH EVALUATE(lcOrdFile+'.Style'),;
          Dyelot    WITH POSLN.Dyelot              ,;
          cWareCode WITH POSLN.cWareCode
        REPLACE Qty1      WITH MIN(EVALUATE(lcOrdFile+'.Qty1')-lnOrd1,lnCut1) ,;
          Qty2      WITH MIN(EVALUATE(lcOrdFile+'.Qty2')-lnOrd2,lnCut2) ,;
          Qty3      WITH MIN(EVALUATE(lcOrdFile+'.Qty3')-lnOrd3,lnCut3) ,;
          Qty4      WITH MIN(EVALUATE(lcOrdFile+'.Qty4')-lnOrd4,lnCut4) ,;
          Qty5      WITH MIN(EVALUATE(lcOrdFile+'.Qty5')-lnOrd5,lnCut5) ,;
          Qty6      WITH MIN(EVALUATE(lcOrdFile+'.Qty6')-lnOrd6,lnCut6) ,;
          Qty7      WITH MIN(EVALUATE(lcOrdFile+'.Qty7')-lnOrd7,lnCut7) ,;
          Qty8      WITH MIN(EVALUATE(lcOrdFile+'.Qty8')-lnOrd8,lnCut8) ,;
          TotQty    WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8

        lnOrd1 = lnOrd1 + Qty1
        lnOrd2 = lnOrd2 + Qty2
        lnOrd3 = lnOrd3 + Qty3
        lnOrd4 = lnOrd4 + Qty4
        lnOrd5 = lnOrd5 + Qty5
        lnOrd6 = lnOrd6 + Qty6
        lnOrd7 = lnOrd7 + Qty7
        lnOrd8 = lnOrd8 + Qty8

        SELECT (lcOrdFile)
        REPLACE Cut1   WITH Cut1 + EVALUATE(loParentForm.lcCutPick+'.QTY1') ,;
          Cut2   WITH Cut2 + EVALUATE(loParentForm.lcCutPick+'.QTY2') ,;
          Cut3   WITH Cut3 + EVALUATE(loParentForm.lcCutPick+'.QTY3') ,;
          Cut4   WITH Cut4 + EVALUATE(loParentForm.lcCutPick+'.QTY4') ,;
          Cut5   WITH Cut5 + EVALUATE(loParentForm.lcCutPick+'.QTY5') ,;
          Cut6   WITH Cut6 + EVALUATE(loParentForm.lcCutPick+'.QTY6') ,;
          Cut7   WITH Cut7 + EVALUATE(loParentForm.lcCutPick+'.QTY7') ,;
          Cut8   WITH Cut8 + EVALUATE(loParentForm.lcCutPick+'.QTY8') ,;
          TotCut WITH Cut1+Cut2+Cut3+Cut4+Cut5+Cut6+Cut7+Cut8

        SELECT (loParentForm.lcPOH)
        IF !SEEK(POSLN.PO+EVALUATE(lcOrdFile+'.cDivision')+EVALUATE(lcOrdFile+'.Season')+;
            PADR(SUBSTR(EVALUATE(lcOrdFile+'.Style'),1,loParentForm.lnMajorLen),19))
          =loParentForm.loStyle.SEEK(SUBSTR(EVALUATE(lcOrdFile+'.Style'),1,loParentForm.lnMajorLen))
          APPEND BLANK
          REPLACE cstytype   WITH 'U' ,;
            STATUS     WITH POSHDR.STATUS ,;
            LastLine   WITH 0   ,;
            PO         WITH POSLN.PO  ,;
            cDivision  WITH EVALUATE(lcOrdFile+'.cDivision'),;
            SEASON     WITH EVALUATE(lcOrdFile+'.Season'),;
            ENTERED    WITH POSHDR.ENTERED   ,;
            COMPLETE   WITH POSHDR.COMPLETE  ,;
            cWareCode  WITH POSHDR.cWareCode ,;
            STYLE      WITH SUBSTR(EVALUATE(lcOrdFile+'.Style'),1,loParentForm.lnMajorLen)      ,;
            PATTERN    WITH POSHDR.PATTERN ,;
            lMultiWare WITH POSHDR.lMultiWare
        ENDIF

        SELECT (loParentForm.lcPOLine)
        IF !SEEK(POSLN.PO+EVALUATE(lcOrdFile+'.cDivision')+EVALUATE(lcOrdFile+'.Season')+EVALUATE(lcOrdFile+'.Style')+POSLN.Dyelot)
          APPEND BLANK
          REPLACE PO         WITH POSLN.PO             ,;
            TRANCD     WITH '1'                  ,;
            STYLE      WITH EVALUATE(lcOrdFile+'.Style'),;
            cDivision  WITH EVALUATE(lcOrdFile+'.cDivision'),;
            cWareCode  WITH POSLN.cWareCode      ,;
            SEASON     WITH EVALUATE(lcOrdFile+'.Season'),;
            LINENO     WITH POSLN.LINENO         ,;
            COMPLETE   WITH POSLN.COMPLETE       ,;
            cCstSht_Id WITH POSLN.cCstSht_Id     ,;
            Dyelot     WITH POSLN.Dyelot         ,;
            Fabric     WITH EVALUATE(lcOrdFile+'.Fabric')   ,;
            cFabWare   WITH EVALUATE(lcOrdFile+'.cFabWare') ,;
            nYeild     WITH EVALUATE(lcOrdFile+'.nYeild')

          SELECT (loParentForm.lcPOH)
          REPLACE LastLine WITH LastLine + 1
        ENDIF
        SELECT (loParentForm.lcPOLine)
        REPLACE Qty1 WITH Qty1 + lnCut1 ,;
          Qty2 WITH Qty2 + lnCut2 ,;
          Qty3 WITH Qty3 + lnCut3 ,;
          Qty4 WITH Qty4 + lnCut4 ,;
          Qty5 WITH Qty5 + lnCut5 ,;
          Qty6 WITH Qty6 + lnCut6 ,;
          Qty7 WITH Qty7 + lnCut7 ,;
          Qty8 WITH Qty8 + lnCut8 ,;
          ORD1 WITH ORD1 + EVALUATE(loParentForm.lcCutPick+'.Qty1') ,;
          ORD2 WITH ORD2 + EVALUATE(loParentForm.lcCutPick+'.Qty2') ,;
          ORD3 WITH ORD3 + EVALUATE(loParentForm.lcCutPick+'.Qty3') ,;
          ORD4 WITH ORD4 + EVALUATE(loParentForm.lcCutPick+'.Qty4') ,;
          ORD5 WITH ORD5 + EVALUATE(loParentForm.lcCutPick+'.Qty5') ,;
          ORD6 WITH ORD6 + EVALUATE(loParentForm.lcCutPick+'.Qty6') ,;
          ORD7 WITH ORD7 + EVALUATE(loParentForm.lcCutPick+'.Qty7') ,;
          ORD8 WITH ORD8 + EVALUATE(loParentForm.lcCutPick+'.Qty8') ,;
          TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
          TotOrd WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
        REPLACE nFCost1 WITH TotQty * STYLE.nMCost1 ,;
          nFCost2 WITH TotQty * STYLE.nMCost2 ,;
          nFCost3 WITH TotQty * STYLE.nMCost3 ,;
          nFCost4 WITH TotQty * STYLE.nMCost4 ,;
          nFCost5 WITH TotQty * STYLE.nMCost5 ,;
          nFCost6 WITH TotQty * STYLE.nMCost6 ,;
          nFCost7 WITH TotQty * STYLE.nMCost7 ,;
          Gros_Price WITH nFCost1,;
          nICost1 WITH TotQty * STYLE.nMCost1 ,;
          nICost2 WITH TotQty * STYLE.nMCost2 ,;
          nICost3 WITH TotQty * STYLE.nMCost3 ,;
          nICost4 WITH TotQty * STYLE.nMCost4 ,;
          nICost5 WITH TotQty * STYLE.nMCost5 ,;
          nICost6 WITH TotQty * STYLE.nMCost6 ,;
          nICost7 WITH TotQty * STYLE.nMCost7
        lnReqQty1 = lnReqQty1 - Qty1
        lnReqQty2 = lnReqQty2 - Qty2
        lnReqQty3 = lnReqQty3 - Qty3
        lnReqQty4 = lnReqQty4 - Qty4
        lnReqQty5 = lnReqQty5 - Qty5
        lnReqQty6 = lnReqQty6 - Qty6
        lnReqQty7 = lnReqQty7 - Qty7
        lnReqQty8 = lnReqQty8 - Qty8
        lnTotReq  = lnReqQty1+lnReqQty2+lnReqQty3+lnReqQty4+lnReqQty5+lnReqQty6+lnReqQty7+lnReqQty8

        SELECT (loParentForm.lcPOH)
        REPLACE nStyOrder WITH nStyOrder + lnTotCut  ,;
          TotOrd    WITH TotOrd  + EVALUATE(loParentForm.lcPOLine+'.TotOrd') ,;
          nFCost1   WITH nFCost1 + lnTotCut*STYLE.nMCost1 ,;
          nFCost2   WITH nFCost2 + lnTotCut*STYLE.nMCost2 ,;
          nFCost3   WITH nFCost3 + lnTotCut*STYLE.nMCost3 ,;
          nFCost4   WITH nFCost4 + lnTotCut*STYLE.nMCost4 ,;
          nFCost5   WITH nFCost5 + lnTotCut*STYLE.nMCost5 ,;
          nFCost6   WITH nFCost6 + lnTotCut*STYLE.nMCost6 ,;
          nFCost7   WITH nFCost7 + lnTotCut*STYLE.nMCost7 ,;
          TOTCOST   WITH nFCost1+nFCost2+nFCost3+nFCost4+nFCost5+nFCost6+nFCost7,;
          nICost1   WITH nICost1 + lnTotCut*STYLE.nMCost1,;
          nICost2   WITH nICost2 + lnTotCut*STYLE.nMCost2,;
          nICost3   WITH nICost3 + lnTotCut*STYLE.nMCost3,;
          nICost4   WITH nICost4 + lnTotCut*STYLE.nMCost4,;
          nICost5   WITH nICost5 + lnTotCut*STYLE.nMCost5,;
          nICost6   WITH nICost6 + lnTotCut*STYLE.nMCost6,;
          nICost7   WITH nICost7 + lnTotCut*STYLE.nMCost7
      ENDSCAN
    ENDIF
  ENDSCAN
  SELECT POSLN
  SELECT (lnAlias)

  *!*************************************************************
  *! Name      : lfUnAllPO
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/09/2004
  *! Purpose   : Allocate order lines to existing unallocated POs
  *!*************************************************************
  *! Calls     : None
  *!*************************************************************
  *! Parameters: lcOrdFile : Order line temporary file name
  *!*************************************************************
  *! Returns   :  None.
  *!*************************************************************
  *! Example   :  =lfUnAllPO()
  *!*************************************************************
FUNCTION lfUnAllPO
  LPARAMETERS lcOrdFile

  LOCAL lnAlias,lnReqQty1,lnReqQty2,lnReqQty3,lnReqQty4,lnReqQty5,lnReqQty6,;
    lnReqQty7,lnReqQty8,lnTotReq,lnOrd1,lnOrd2,lnOrd3,lnOrd4,lnOrd5,;
    lnOrd6,lnOrd7,lnOrd8,lnCut1,lnCut2,lnCut3,lnCut4,lnCut5,lnCut6,;
    lnCut7,lnCut8,lnTotCut,lcExSign,lcUntSin

  lnAlias = SELECT(0)
  loParentForm.loCUTPICK.SETORDER('CUTPICK')
  loParentForm.loPOSLN.SETORDER('POSLNS')
  SELECT POSLN

  STORE '' TO lcExSign,lcUntSin
  SELECT (lcOrdFile)
  =SEEK("�")
  SCAN REST WHILE cSelect+cDivision+cPurCode+cStyGrade+STYLE+Dyelot = "�"
    =loParentForm.loStyle.SEEK(STYLE)
    lnReqQty1 = INT(loParentForm.laPercet[1]/100*(Qty1-Cut1))
    lnReqQty2 = INT(loParentForm.laPercet[2]/100*(Qty2-Cut2))
    lnReqQty3 = INT(loParentForm.laPercet[3]/100*(Qty3-Cut3))
    lnReqQty4 = INT(loParentForm.laPercet[4]/100*(Qty4-Cut4))
    lnReqQty5 = INT(loParentForm.laPercet[5]/100*(Qty5-Cut5))
    lnReqQty6 = INT(loParentForm.laPercet[6]/100*(Qty6-Cut6))
    lnReqQty7 = INT(loParentForm.laPercet[7]/100*(Qty7-Cut7))
    lnReqQty8 = INT(loParentForm.laPercet[8]/100*(Qty8-Cut8))
    lnTotReq  = lnReqQty1+lnReqQty2+lnReqQty3+lnReqQty4+lnReqQty5+lnReqQty6+lnReqQty7+lnReqQty8
    STORE 0 TO lnOrd1,lnOrd2,lnOrd3,lnOrd4,lnOrd5,lnOrd6,lnOrd7,lnOrd8
    IF loParentForm.loPOSLN.SEEK('0001'+STYLE+'PP')
      SELECT POSLN
      SCAN REST WHILE cinvtype+STYLE+cbusdocu+cstytype+PO+STR(LINENO,6)+TRANCD = '0001'+EVALUATE(lcOrdFile+'.Style')+'PP';
          FOR   IIF(loParentForm.llRpBOPFab,Dyelot=EVALUATE(lcOrdFile+'.Dyelot'),.T.) AND TRANCD='1' AND ;
          TotQty > TotOrd AND lnTotReq > 0 AND loParentForm.loPosHdr.SEEK(cbusdocu+cstytype+PO) AND !INLIST(POSHDR.STATUS,'X','C')
        SELECT (loParentForm.lcPOLine)
        llFound=SEEK(POSLN.PO)
        =SEEK(POSLN.PO+EVALUATE(lcOrdFile+'.cDivision')+POSHDR.cPurCode+POSLN.cStyGrade+POSLN.STYLE+POSLN.Dyelot)
        SUM REST  TotOrd TO lnAddOrd ;
          WHILE PO+cDivision+cPurCode+cStyGrade+STYLE= ;
          POSLN.PO+EVALUATE(lcOrdFile+'.cDivision')+POSHDR.cPurCode+POSLN.cStyGrade+POSLN.STYLE+POSLN.Dyelot
        IF POSLN.TotQty <= POSLN.TotOrd+lnAddOrd OR (!llFound AND POSHDR.FLAG='Y')
          LOOP
        ENDIF
        IF !llFound
          SELECT POSHDR
          loParentForm.loPosHdr.REPLACE("FLAG WITH 'Y'")

          LOCAL lnConnectionHandlar,lcTranCode,llUpdate
          lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
          IF TYPE('lcTranCode') = 'N'
            llUpdate = loParentForm.loPosHdr.TABLEUPDATE(lcTranCode)
            IF !llUpdate
              =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
            ELSE
              lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
              IF lnConnectionHandlar # 1
                =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        SELECT POSLN
        lnCut1 = MIN(Qty1-ORD1,lnReqQty1)
        lnCut2 = MIN(Qty2-ORD2,lnReqQty2)
        lnCut3 = MIN(Qty3-ORD3,lnReqQty3)
        lnCut4 = MIN(Qty4-ORD4,lnReqQty4)
        lnCut5 = MIN(Qty5-ORD5,lnReqQty5)
        lnCut6 = MIN(Qty6-ORD6,lnReqQty6)
        lnCut7 = MIN(Qty7-ORD7,lnReqQty7)
        lnCut8 = MIN(Qty8-ORD8,lnReqQty8)
        lnTotCut=lnCut1+lnCut2+lnCut3+lnCut4+lnCut5+lnCut6+lnCut7+lnCut8
        SELECT (loParentForm.lcCutPick)
        APPEND BLANK
        REPLACE cTKtNo    WITH POSLN.PO ,;
          TRANCD    WITH '2'      ,;
          ORDER     WITH EVALUATE(lcOrdFile+'.ORDER'),;
          cOrdLine  WITH STR(EVALUATE(lcOrdFile+'.LINENO'),6),;
          STYLE     WITH EVALUATE(lcOrdFile+'.Style'),;
          Dyelot    WITH POSLN.Dyelot              ,;
          cWareCode WITH POSLN.cWareCode           ,;
          cDivision WITH EVALUATE(lcOrdFile+'.cDivision'),;
          cPurCode  WITH POSHDR.cPurCode           ,;
          cStyGrade WITH STYLE.cStyGrade
        REPLACE Qty1      WITH MIN(EVALUATE(lcOrdFile+'.Qty1')-lnOrd1,lnCut1) ,;
          Qty2      WITH MIN(EVALUATE(lcOrdFile+'.Qty2')-lnOrd2,lnCut2) ,;
          Qty3      WITH MIN(EVALUATE(lcOrdFile+'.Qty3')-lnOrd3,lnCut3) ,;
          Qty4      WITH MIN(EVALUATE(lcOrdFile+'.Qty4')-lnOrd4,lnCut4) ,;
          Qty5      WITH MIN(EVALUATE(lcOrdFile+'.Qty5')-lnOrd5,lnCut5) ,;
          Qty6      WITH MIN(EVALUATE(lcOrdFile+'.Qty6')-lnOrd6,lnCut6) ,;
          Qty7      WITH MIN(EVALUATE(lcOrdFile+'.Qty7')-lnOrd7,lnCut7) ,;
          Qty8      WITH MIN(EVALUATE(lcOrdFile+'.Qty8')-lnOrd8,lnCut8) ,;
          TotQty    WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8

        lnOrd1 = lnOrd1 + Qty1
        lnOrd2 = lnOrd2 + Qty2
        lnOrd3 = lnOrd3 + Qty3
        lnOrd4 = lnOrd4 + Qty4
        lnOrd5 = lnOrd5 + Qty5
        lnOrd6 = lnOrd6 + Qty6
        lnOrd7 = lnOrd7 + Qty7
        lnOrd8 = lnOrd8 + Qty8

        SELECT (lcOrdFile)
        REPLACE Cut1   WITH Cut1 + EVALUATE(loParentForm.lcCutPick+'.QTY1') ,;
          Cut2   WITH Cut2 + EVALUATE(loParentForm.lcCutPick+'.QTY2') ,;
          Cut3   WITH Cut3 + EVALUATE(loParentForm.lcCutPick+'.QTY3') ,;
          Cut4   WITH Cut4 + EVALUATE(loParentForm.lcCutPick+'.QTY4') ,;
          Cut5   WITH Cut5 + EVALUATE(loParentForm.lcCutPick+'.QTY5') ,;
          Cut6   WITH Cut6 + EVALUATE(loParentForm.lcCutPick+'.QTY6') ,;
          Cut7   WITH Cut7 + EVALUATE(loParentForm.lcCutPick+'.QTY7') ,;
          Cut8   WITH Cut8 + EVALUATE(loParentForm.lcCutPick+'.QTY8') ,;
          TotCut WITH Cut1+Cut2+Cut3+Cut4+Cut5+Cut6+Cut7+Cut8

        SELECT (loParentForm.lcPOH)
        IF !SEEK(POSLN.PO+EVALUATE(lcOrdFile+'.cDivision')+STYLE.cPurCode+STYLE.cStyGrade)
          APPEND BLANK
          REPLACE cstytype   WITH 'P' ,;
            STATUS     WITH 'H' ,;
            LastLine   WITH 0   ,;
            PO         WITH POSLN.PO  ,;
            Vendor     WITH POSHDR.Vendor ,;
            cDivision  WITH EVALUATE(lcOrdFile+'.cDivision'),;
            cPurCode   WITH STYLE.cPurCode   ,;
            ENTERED    WITH POSHDR.ENTERED   ,;
            COMPLETE   WITH POSHDR.COMPLETE  ,;
            SHIPVIA    WITH POSHDR.SHIPVIA   ,;
            CTERMCODE  WITH POSHDR.CTERMCODE ,;
            cWareCode  WITH POSHDR.cWareCode ,;
            cStyGrade  WITH STYLE.cStyGrade  ,;
            cPriceCur  WITH POSHDR.cPriceCur ,;
            nPriceRat  WITH POSHDR.nPriceRat ,;
            nCurrUnit  WITH POSHDR.nCurrUnit ,;
            cDutyCur   WITH POSHDR.cDutyCur  ,;
            nDutyRat   WITH POSHDR.nDutyRat  ,;
            nDCurUnit  WITH POSHDR.nDCurUnit ,;
            lMultiWare WITH POSHDR.lMultiWare
        ENDIF

        SELECT (loParentForm.lcPOLine)

        *C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [BEGIN]
        *608597,1 ALAA [Begin] Commnetd out to rename varibale llGrpSO to be llRPGrpSO
        *IF !SEEK(POSLN.PO+EVALUATE(lcOrdFile+'.cDivision')+STYLE.cPurCode+STYLE.cStyGrade+EVALUATE(lcOrdFile+'.Style')+POSLN.Dyelot)
        IF (!SEEK(POSLN.PO+EVALUATE(lcOrdFile+'.cDivision')+STYLE.cPurCode+STYLE.cStyGrade+EVALUATE(lcOrdFile+'.Style')+POSLN.Dyelot) AND loParentForm.llRPGrpSO ) OR !loParentForm.llRPGrpSO
          *C200804,1 Create un\grouped po lines accoring to llGrpSO flag HIA [END]
         *608597,1 ALAA [END] Commnetd out to rename varibale llGrpSO to be llRPGrpSO

          APPEND BLANK
          REPLACE PO         WITH POSLN.PO             ,;
            TRANCD     WITH '1'                  ,;
            STYLE      WITH EVALUATE(lcOrdFile+'.Style'),;
            cDivision  WITH EVALUATE(lcOrdFile+'.cDivision'),;
            cWareCode  WITH POSLN.cWareCode      ,;
            LINENO     WITH POSLN.LINENO         ,;
            Vendor     WITH POSHDR.Vendor        ,;
            SHIPVIA    WITH POSLN.SHIPVIA        ,;
            COMPLETE   WITH POSLN.COMPLETE       ,;
            cCstSht_Id WITH POSLN.cCstSht_Id     ,;
            cPurCode   WITH STYLE.cPurCode       ,;
            cStyGrade  WITH STYLE.cStyGrade      ,;
            Dyelot     WITH POSLN.Dyelot         ,;
            Fabric     WITH EVALUATE(lcOrdFile+'.Fabric')   ,;
            cFabWare   WITH EVALUATE(lcOrdFile+'.cFabWare') ,;
            nYeild     WITH EVALUATE(lcOrdFile+'.nYeild')

          SELECT (loParentForm.lcPOH)
          REPLACE LastLine WITH LastLine + 1
        ENDIF

        SELECT (loParentForm.lcPOLine)
        REPLACE Qty1 WITH Qty1 + lnCut1 ,;
          Qty2 WITH Qty2 + lnCut2 ,;
          Qty3 WITH Qty3 + lnCut3 ,;
          Qty4 WITH Qty4 + lnCut4 ,;
          Qty5 WITH Qty5 + lnCut5 ,;
          Qty6 WITH Qty6 + lnCut6 ,;
          Qty7 WITH Qty7 + lnCut7 ,;
          Qty8 WITH Qty8 + lnCut8 ,;
          ORD1 WITH ORD1 + EVALUATE(loParentForm.lcCutPick+'.Qty1') ,;
          ORD2 WITH ORD2 + EVALUATE(loParentForm.lcCutPick+'.Qty2') ,;
          ORD3 WITH ORD3 + EVALUATE(loParentForm.lcCutPick+'.Qty3') ,;
          ORD4 WITH ORD4 + EVALUATE(loParentForm.lcCutPick+'.Qty4') ,;
          ORD5 WITH ORD5 + EVALUATE(loParentForm.lcCutPick+'.Qty5') ,;
          ORD6 WITH ORD6 + EVALUATE(loParentForm.lcCutPick+'.Qty6') ,;
          ORD7 WITH ORD7 + EVALUATE(loParentForm.lcCutPick+'.Qty7') ,;
          ORD8 WITH ORD8 + EVALUATE(loParentForm.lcCutPick+'.Qty8') ,;
          TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ,;
          TotOrd WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
        lnGros_Price = 0
        FOR lnCount = 1 TO 7
          lcCount = STR(lnCount,1)
          DO CASE
            CASE loParentForm.lcIType&lcCount = 'P'
              lcExSign       = gfGetExSin(@lcUntSin,POSHDR.cPriceCur)
              lnCost&lcCount = IIF(POSHDR.cPriceCur=STYLE.cPriceCur,lnTotCut*STYLE.nICost&lcCount,0)
              lnECost&lcCount= lnCost&lcCount &lcExSign POSHDR.nPriceRat &lcUntSin POSHDR.nCurrUnit
              lnGros_Price   = IIF(lnCount=1,IIF(POSHDR.cPriceCur=STYLE.cPriceCur,STYLE.nICost1,0),lnGros_Price)
            CASE INLIST(loParentForm.lcIType&lcCount,'M','D')
              lcExSign       = gfGetExSin(@lcUntSin,POSHDR.cDutyCur)
              lnCost&lcCount = IIF(POSHDR.cDutyCur=STYLE.cDutyCur,lnTotCut*STYLE.nICost&lcCount,0)
              lnECost&lcCount= lnCost&lcCount &lcExSign POSHDR.nDutyRat &lcUntSin POSHDR.nDCurUnit
              lnGros_Price   = IIF(lnCount=1,IIF(POSHDR.cDutyCur=STYLE.cDutyCur,STYLE.nICost1,0),lnGros_Price)
            OTHERWISE
              lnCost&lcCount = lnTotCut*STYLE.nICost&lcCount
              lnECost&lcCount= lnCost&lcCount
              lnGros_Price   = IIF(lnCount=1,STYLE.nICost1,lnGros_Price)
          ENDCASE
        ENDFOR
        REPLACE nFCost1 WITH nFCost1 + lnCost1 ,;
          nFCost2 WITH nFCost2 + lnCost2 ,;
          nFCost3 WITH nFCost3 + lnCost3 ,;
          nFCost4 WITH nFCost4 + lnCost4 ,;
          nFCost5 WITH nFCost5 + lnCost5 ,;
          nFCost6 WITH nFCost6 + lnCost6 ,;
          nFCost7 WITH nFCost7 + lnCost7 ,;
          Gros_Price WITH lnGros_Price,;
          nICost1 WITH nICost1 + lnECost1,;
          nICost2 WITH nICost2 + lnECost2,;
          nICost3 WITH nICost3 + lnECost3,;
          nICost4 WITH nICost4 + lnECost4,;
          nICost5 WITH nICost5 + lnECost5,;
          nICost6 WITH nICost6 + lnECost6,;
          nICost7 WITH nICost7 + lnECost7
        lnReqQty1 = lnReqQty1 - Qty1
        lnReqQty2 = lnReqQty2 - Qty2
        lnReqQty3 = lnReqQty3 - Qty3
        lnReqQty4 = lnReqQty4 - Qty4
        lnReqQty5 = lnReqQty5 - Qty5
        lnReqQty6 = lnReqQty6 - Qty6
        lnReqQty7 = lnReqQty7 - Qty7
        lnReqQty8 = lnReqQty8 - Qty8
        lnTotReq  = lnReqQty1+lnReqQty2+lnReqQty3+lnReqQty4+lnReqQty5+lnReqQty6+lnReqQty7+lnReqQty8

        SELECT (loParentForm.lcPOH)
        REPLACE nStyOrder WITH nStyOrder + lnTotCut  ,;
          TotOrd    WITH TotOrd  + EVALUATE(loParentForm.lcPOLine+'.TotOrd') ,;
          nFCost1   WITH nFCost1 + lnCost1 ,;
          nFCost2   WITH nFCost2 + lnCost2 ,;
          nFCost3   WITH nFCost3 + lnCost3 ,;
          nFCost4   WITH nFCost4 + lnCost4 ,;
          nFCost5   WITH nFCost5 + lnCost5 ,;
          nFCost6   WITH nFCost6 + lnCost6 ,;
          nFCost7   WITH nFCost7 + lnCost7 ,;
          TOTCOST   WITH nFCost1+nFCost2+nFCost3+nFCost4+nFCost5+nFCost6+nFCost7,;
          nICost1   WITH nICost1 + lnECost1,;
          nICost2   WITH nICost2 + lnECost2,;
          nICost3   WITH nICost3 + lnECost3,;
          nICost4   WITH nICost4 + lnECost4,;
          nICost5   WITH nICost5 + lnECost5,;
          nICost6   WITH nICost6 + lnECost6,;
          nICost7   WITH nICost7 + lnECost7
      ENDSCAN
    ENDIF
  ENDSCAN
  SELECT POSLN
  SELECT (lnAlias)

  *!*************************************************************
  *! Name      : lfFabPri
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/21/2004
  *! Purpose   : Prioritize Fabric/COlor/warehouse/dyelot
  *!*************************************************************
  *! Calls     : lfFabClr(),MFFABPRI.SPR
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   :  None.
  *!*************************************************************
  *! Example   :  =lfFabPri()
  *!*************************************************************
FUNCTION lfFabPri
  LPARAMETERS loFormSet

  PRIVATE lcFabric,lcText,lnAlias,puFabrics,lnPcnt1,lnPcnt2,lnPcnt3,lnPcnt4,lnPcnt5,lnPcnt6,lnPcnt7,lnPcnt8

  STORE SPACE(19) TO lcFabric
  lnPcnt1 = loFormSet.laPercet[1]
  lnPcnt2 = loFormSet.laPercet[2]
  lnPcnt3 = loFormSet.laPercet[3]
  lnPcnt4 = loFormSet.laPercet[4]
  lnPcnt5 = loFormSet.laPercet[5]
  lnPcnt6 = loFormSet.laPercet[6]
  lnPcnt7 = loFormSet.laPercet[7]
  lnPcnt8 = loFormSet.laPercet[8]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcText  = LANG_MFGENCT_LCTEXT1+IIF(loFormSet.llRPUnAWip,' '+LANG_MFGENCT_LCTEXT2,'')
lcText  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_LCTEXT1,loFormSet.GetHeaderText("LANG_MFGENCT_LCTEXT1",loFormSet.HeaderAlias))+IIF(loFormSet.llRPUnAWip,' '+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_LCTEXT2,loFormSet.GetHeaderText("LANG_MFGENCT_LCTEXT2",loFormSet.HeaderAlias)),'')
*N000682,1 11/20/2012 MMT Globlization changes[End]

  lnAlias = SELECT(0)
  SELECT DISTINCT Fabric FROM (loFormSet.lcFabrics) INTO ARRAY loFormSet.laFabrics
  IF _TALLY > 0
    DEFINE POPUP puFabPri MARGIN MOVER RELATIVE SCROLL MARK CHR(16)
    puFabrics = 1
    PRIVATE lnFabReq
    STORE 0 TO lnFabReq
    =lfFabClr(loFormSet)

    PRIVATE loParentForm
    loParentForm = loFormSet
    *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
    *DO FORM (oAriaApplication.ScreenHome+"MFFABPRI.SCX")
    =gfCallForm('MFFABPRI')
    *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
    llReturn = .T.
    RETURN llReturn
  ELSE
    *Message : 38164
    *No fabrics selected. No materials to allocate from.
    *Button : 00000
    *Ok
    =gfModalGen('TRM38164B00000','DIALOG')
    llReturn = .F.
    RETURN llReturn
  ENDIF

  *!*************************************************************
  *! Name      : lfFabClr
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/21/2004
  *! Purpose   : Get fabric colors
  *!*************************************************************
  *! Calls     : lfFillPopup()
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   :  None.
  *!*************************************************************
  *! Example   :  =lfFabClr()
  *!*************************************************************
FUNCTION lfFabClr
  LPARAMETERS loFormSet

  =lfFillPopup(loFormSet)
  lcFabric = loFormSet.laFabrics[puFabrics]
  =lfFabReq(lcFabric,0,loFormSet)

  *!*************************************************************
  *! Name      : lfvFabrics
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/21/2004
  *! Purpose   : Validate selected fabric
  *!*************************************************************
  *! Calls     : lfFabClr()
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   :  None.
  *!*************************************************************
  *! Example   :  =lfvFabrics()
  *!*************************************************************
FUNCTION lfvFabrics
  =lfFabClr(loParentForm)

  *!*************************************************************
  *! Name      : lfReFabPri
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/21/2004
  *! Purpose   : Reprioritize fabric/color/warehouse/dyelot
  *!*************************************************************
  *! Calls     : None
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   :  None.
  *!*************************************************************
  *! Example   :  =lfReFabPri()
  *!*************************************************************
FUNCTION lfReFabPri
  LPARAMETERS loFormSet

  SELECT (loFormSet.lcFabrics)
  FOR lnCount = 1 TO CNTBAR('puFabPri')
    =SEEK(lcFabric+PADL(lnCount,4,'0'))
    REPLACE cNewPrior WITH PADL(GETBAR('puFabPri',lnCount),4,'0')
  ENDFOR
  REPLACE ALL cPriority WITH cNewPrior FOR Fabric=lcFabric

  *!*************************************************************
  *! Name      : lfFillPopup
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/21/2004
  *! Purpose   : Rebrowse fabric/color/warehouse/dyelot
  *!*************************************************************
  *! Calls     : lfReFabPri()
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   :  None.
  *!*************************************************************
  *! Example   :  =lfFillPopup()
  *!*************************************************************
FUNCTION lfFillPopup
  LPARAMETERS loFormSet

  LOCAL lcPrompt,lnCount,lcOldIndx

  =lfReFabPri(loFormSet)
  SELECT (loFormSet.lcFabrics)
  lcOldIndx = ORDER()
  SET ORDER TO OnHand DESCENDING

  RELEASE BAR ALL OF puFabPri
  lnCount = 0
  =SEEK(loFormSet.laFabrics[puFabrics])
  SCAN REST WHILE Fabric = loFormSet.laFabrics[puFabrics]
    lnCount = lnCount + 1
    lcPrompt = cWareCode+SPACE(3)+Dyelot+SPACE(1)+STR(OnHand,10,3)+;
      IIF(loFormSet.llRPUnAWip,SPACE(1)+STR(nUnAllWIP,10,3),'')
    DEFINE BAR lnCount OF puFabPri PROMPT lcPrompt FONT 'Courier New', 9
  ENDSCAN
  SET ORDER TO (lcOldIndx)

  *!*************************************************************
  *! Name      : lfvOkFabPri
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/21/2004
  *! Purpose   : Rebrowse fabric/color/warehouse/dyelot
  *!*************************************************************
  *! Calls     : lfReFabPri()
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   :  None.
  *!*************************************************************
  *! Example   :  =lfvOkFabPri()
  *!*************************************************************
FUNCTION lfvOkFabPri

  =lfReFabPri(loParentForm)
  IF loParentForm.laPercet[1]<>lnPcnt1 OR loParentForm.laPercet[2]<>lnPcnt2 OR loParentForm.laPercet[3]<>lnPcnt3 OR ;
      loParentForm.laPercet[4]<>lnPcnt4 OR loParentForm.laPercet[5]<>lnPcnt5 OR loParentForm.laPercet[6]<>lnPcnt6 OR ;
      loParentForm.laPercet[7]<>lnPcnt7 OR loParentForm.laPercet[8]<>lnPcnt8
    =lfFabReq('',0,loParentForm)
  ENDIF
  SELECT(lnAlias)

  *!*************************************************************
  *! Name      : lfFabReq
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/21/2004
  *! Purpose   : Recalculate fabric required quantity based on percentage allocated
  *!*************************************************************
  *! Calls     : lfRefresh()
  *!*************************************************************
  *! Parameters: lcFabric : Fabric
  *!             lcColor  : Color
  *!*************************************************************
  *! Returns   :  None.
  *!*************************************************************
  *! Example   :  =lfFabReq(lcFabric,lcColor)
  *!*************************************************************
FUNCTION lfFabReq
  PARAMETERS lcFabric,lnCnt,loFormSet,lnOldValue

  IF !EMPTY(lcFabric) AND IIF(lnCnt=0,.F.,loFormSet.laPercet[lnCnt]=lnOldValue)
    RETURN
  ENDIF

  SELECT (loFormSet.lcOrdLine)
  SET ORDER TO TAG 'Groups'
  =SEEK("�"+lcFabric)

  lnFabReq = 0
  SCAN REST WHILE cSelect+Fabric+cSortExp+ORDER+STORE+GROUP = "�"+lcFabric
    lnReqQty = (INT((Qty1-Cut1)*loFormSet.laPercet[1]/100)+INT((Qty2-Cut2)*loFormSet.laPercet[2]/100)+;
      INT((Qty3-Cut3)*loFormSet.laPercet[3]/100)+INT((Qty4-Cut4)*loFormSet.laPercet[4]/100)+;
      INT((Qty5-Cut5)*loFormSet.laPercet[5]/100)+INT((Qty6-Cut6)*loFormSet.laPercet[6]/100)+;
      INT((Qty7-Cut7)*loFormSet.laPercet[7]/100)+INT((Qty8-Cut8)*loFormSet.laPercet[8]/100))*nYeild
    lnFabReq = lnFabReq + lnReqQty

    SELECT (loFormSet.lcOrdGroup)
    =SEEK(EVALUATE(loFormSet.lcOrdLine+'.Fabric+'+loFormSet.lcOrdLine+'.Order+'+loFormSet.lcOrdLine+'.Store+'+;
      loFormSet.lcOrdLine+'.Group'))
    REPLACE nRequired WITH nRequired - EVALUATE(loFormSet.lcOrdLine+'.nRequired') + lnReqQty
    SELECT (loFormSet.lcOrdLine)
    REPLACE nRequired WITH lnReqQty
  ENDSCAN

  *!*************************************************************
  *! Name      : lfBaseOnFab
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/21/2004
  *! Purpose   : Allocated/unallocated order lines screen
  *!*************************************************************
  *! Calls     : None
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   :  None.
  *!*************************************************************
  *! Example   :  =lfBaseOnFab()
  *!*************************************************************
FUNCTION lfBaseOnFab
  LPARAMETERS loFormSet

  LOCAL lcOrderBy
  SELECT (loFormSet.lcOrdLine)
  SET ORDER TO TAG GROUPS
  =SEEK("�")
  DO WHILE cSelect+Fabric+cSortExp+ORDER+STORE+GROUP = "�"
    lcOrdGrp = cSelect+Fabric+cSortExp+ORDER+STORE+GROUP
    lcGroup  = Fabric+ORDER+STORE+GROUP
    =SEEK(lcGroup,loFormSet.lcOrdGroup)
    SELECT (loFormSet.lcFabrics)
    =SEEK(EVALUATE(loFormSet.lcOrdLine+'.Fabric'))
    LOCATE REST WHILE Fabric+cPriority+Dyelot+cWareCode = EVALUATE(loFormSet.lcOrdLine+'.Fabric');
      FOR   OnHand+nUnAllWIP-nAllocated-nWIPUsed >= EVALUATE(loFormSet.lcOrdGroup+'.nRequired')
    llOnHand = FOUND()
    SELECT (loFormSet.lcOrdLine)
    SCAN REST WHILE cSelect+Fabric+cSortExp+ORDER+STORE+GROUP=lcOrdGrp
      SCATTER MEMVAR
      m.cSelect  = IIF(llOnHand,"�",' ')
      m.Dyelot   = IIF(llOnHand,EVALUATE(loFormSet.lcFabrics+'.Dyelot'),SPACE(10))
      m.cFabWare = IIF(llOnHand,EVALUATE(loFormSet.lcFabrics+'.cWareCode'),SPACE(6))
      loFormSet.loCustomer.SEEK('M'+m.ACCOUNT)
      m.StName   = SUBSTR(CUSTOMER.StName,1,25)
      INSERT INTO (loFormSet.lcTmpOrd) FROM MEMVAR
      SELECT (loFormSet.lcOrdGroup)
      =SEEK(m.Fabric+m.Order+m.Store+m.Group)
      REPLACE cWareCode WITH m.cFabWare,;
        Dyelot    WITH m.Dyelot
      IF llOnHand
        SELECT (loFormSet.lcStyUnAll)
        =SEEK(m.STYLE+m.Dyelot)
        lnWIPUsed = MIN(m.nRequired,nUnAllWIP-nWIPUsed)
        REPLACE nWIPUsed WITH nWIPUsed + lnWIPUsed
        SELECT (loFormSet.lcFabrics)
        REPLACE nAllocated WITH nAllocated + m.nRequired-lnWIPUsed ,;
          nWIPUsed   WITH nWIPUsed   + lnWIPUsed
        SELECT (loFormSet.lcTmpOrd)
        REPLACE nWIPUsed WITH lnWIPUsed
      ENDIF
    ENDSCAN
  ENDDO
  SET ORDER TO TAG (loFormSet.lcOrdLine) IN (loFormSet.lcOrdLine)
  GO TOP IN (loFormSet.lcOrdLine)

  *!*************************************************************
  *! Name      : lfShowAll
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/23/2004
  *! Purpose   : Allocated/unallocated order lines screen
  *!*************************************************************
  *! Calls     : lfClearKey(),MFGENCT5.SPR
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   :  None.
  *!*************************************************************
  *! Example   :  =lfShowAll()
  *!*************************************************************
FUNCTION lfShowAll
  LPARAMETERS loFormSet

  LOCAL lnAlias
  lnAlias = SELECT(0)
  SELECT (loFormSet.lcTmpOrd)
  SET ORDER TO TAG 'Fabrics'

  GO TOP IN (loFormSet.lcFabrics)
  STORE 0 TO lnFabRec,lnOrdRec
  lcPrompt  = IIF(EVALUATE(loFormSet.lcFabrics+'.cWareCode') = EVALUATE(loFormSet.lcTmpOrd+'.cFabWare') AND;
    EVALUATE(loFormSet.lcFabrics+'.Dyelot')    = EVALUATE(loFormSet.lcTmpOrd+'.Dyelot')   AND;
    EVALUATE(loFormSet.lcTmpOrd+'.lSelect'),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_DEALLOCATE,loFormSet.GetHeaderText("LANG_MFGENCT_DEALLOCATE",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ALLOCATE1,loFormSet.GetHeaderText("LANG_MFGENCT_ALLOCATE1",loFormSet.HeaderAlias)))

  llAllStat = EVALUATE(loFormSet.lcFabrics+'.cWareCode') = EVALUATE(loFormSet.lcTmpOrd+'.cFabWare') AND;
    EVALUATE(loFormSet.lcFabrics+'.Dyelot')    = EVALUATE(loFormSet.lcTmpOrd+'.Dyelot')

  PRIVATE loParentForm
  loParentForm = loFormSet
  *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
  *DO FORM (oAriaApplication.ScreenHome+"MFGENCT5.SCX")
  =gfCallForm('MFGENCT5')
  *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
  SELECT (loFormSet.lcTmpOrd)
  SET ORDER TO TAG 'TICKET'
  SELECT (lnAlias)

  *!*************************************************************
  *! Name      : lfGetPcs
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/13/2004
  *! Purpose   : Get order line open pieces.
  *!*************************************************************
  *! Calls     : None
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   :  None.
  *!*************************************************************
  *! Example   :  =lfGetPcs()
  *!*************************************************************
FUNCTION lfGetPcs
  LPARAMETERS loFormSet

  RETURN(INT(loFormSet.laPercet[1]/100*(Qty1-Cut1))+INT(loFormSet.laPercet[2]/100*(Qty2-Cut2))+;
    INT(loFormSet.laPercet[3]/100*(Qty3-Cut3))+INT(loFormSet.laPercet[4]/100*(Qty4-Cut4))+;
    INT(loFormSet.laPercet[5]/100*(Qty5-Cut5))+INT(loFormSet.laPercet[6]/100*(Qty6-Cut6))+;
    INT(loFormSet.laPercet[7]/100*(Qty7-Cut7))+INT(loFormSet.laPercet[8]/100*(Qty8-Cut8)))

  *!*************************************************************
  *! Name      : lfShowOrd
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/27/2004
  *! Purpose   : Show Allocate/Unallocate order lines
  *!*************************************************************
  *! Calls     : None
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   :  None.
  *!*************************************************************
  *! Example   :  =lfShowOrd()
  *!*************************************************************
FUNCTION lfShowOrd
  LPARAMETERS loFormSet

  LOCAL lcPrompt,llAllStat
  lcPrompt = IIF(EVALUATE(loParentForm.lcFabrics+'.cWareCode')=EVALUATE(loParentForm.lcTmpOrd+'.cFabWare') AND;
    EVALUATE(loParentForm.lcFabrics+'.Dyelot')   =EVALUATE(loParentForm.lcTmpOrd+'.Dyelot')   AND;
    EVALUATE(loParentForm.lcTmpOrd+'.lSelect'),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_DEALLOCATE,loFormSet.GetHeaderText("LANG_MFGENCT_DEALLOCATE",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ALLOCATE1,loFormSet.GetHeaderText("LANG_MFGENCT_ALLOCATE1",loFormSet.HeaderAlias)))

  llAllStat = EVALUATE(loParentForm.lcFabrics+'.cWareCode') = ALLTRIM(EVALUATE(loParentForm.lcTmpOrd+'.cFabWare')) AND;
    EVALUATE(loParentForm.lcFabrics+'.Dyelot')    = ALLTRIM(EVALUATE(loParentForm.lcTmpOrd+'.Dyelot')) AND;
    !EOF(loParentForm.lcTmpOrd)

  loFormSet.AriaForm1.cmdAllo.CAPTION = lcPrompt
  loFormSet.AriaForm1.cmdAllo.ENABLED = llAllStat

  *!*************************************************************
  *! Name      : lfAllOrd
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/27/2004
  *! Purpose   : Allocate/Unallocate order lines
  *!*************************************************************
  *! Calls     : gfModalGen(),lfShowOrd()
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   :  None.
  *!*************************************************************
  *! Example   :  =lfAllOrd()
  *!*************************************************************
FUNCTION lfAllOrd
  LPARAMETERS loFormSet

  LOCAL lcKey,lnWIPUsed
  SELECT (loParentForm.lcTmpOrd)
  lcKey= Fabric+cWareCode+ORDER+Dyelot+STR(LINENO,6)

  IF !EVALUATE(loParentForm.lcTmpOrd+'.lSelect')
    IF EMPTY(EVALUATE(loParentForm.lcOrdGroup+'.cWarecode+'+loParentForm.lcOrdGroup+'.Dyelot')) AND;
        EVALUATE(loParentForm.lcOrdGroup+'.nRequired-'+loParentForm.lcOrdGroup+'.nUnAllWIP') >;
        EVALUATE(loParentForm.lcFabrics+'.ONHAND')
      *Message : 38165
      *Not enough inventory available to allocate this order lines group
      *Button : 00000
      *Ok
      =gfModalGen('TRM38165B00000','ALERT')
      RETURN
    ENDIF

    IF EVALUATE(loParentForm.lcTmpOrd+'.nRequired-('+loParentForm.lcStyUnAll+'.nUnAllWIP-'+;
        loParentForm.lcStyUnAll+'.nWIPUsed)') >;
        EVALUATE(loParentForm.lcFabrics+'.ONHAND-'+loParentForm.lcFabrics+'.nAllocated')
      *Message : 38149
      *Not enough inventory available to allocate this order line
      *Button : 00000
      *Ok
      =gfModalGen('TRM38149B00000','ALERT')
      RETURN
    ENDIF

    SELECT (loParentForm.lcStyUnAll)
    lnWIPUsed=MIN(EVALUATE(loParentForm.lcTmpOrd+'.nRequired'),nUnAllWIP-nWIPUsed)
    REPLACE nWIPUsed WITH nWIPUsed + lnWIPUsed
    SELECT (loParentForm.lcFabrics)
    REPLACE nAllocated WITH nAllocated + EVALUATE(loParentForm.lcTmpOrd+'.nRequired') - lnWIPUsed
    SELECT (loParentForm.lcTmpOrd)
    =SEEK(lcKey)
    REPLACE cSelect  WITH "�",;
      lSelect  WITH .T.,;
      cFabWare WITH EVALUATE(loParentForm.lcFabrics+'.cWareCode'),;
      Dyelot   WITH EVALUATE(loParentForm.lcFabrics+'.Dyelot'),;
      nWIPUsed WITH lnWIPUsed

    SKIP
    lnRec1 = RECNO()
    SKIP -1

    SELECT (loParentForm.lcOrdGroup)
    REPLACE cWareCode WITH EVALUATE(loParentForm.lcFabrics+'.cWareCode'),;
      Dyelot    WITH EVALUATE(loParentForm.lcFabrics+'.Dyelot')

    SELECT (loParentForm.lcTmpOrd)
    IF lnRec1 > RECCOUNT()
      GO TOP
    ELSE
      GO lnRec1
    ENDIF
  ELSE
    SELECT (loParentForm.lcStyUnAll)
    REPLACE nWIPUsed WITH nWIPUsed - EVALUATE(loParentForm.lcTmpOrd+'.nWIPUsed')
    SELECT (loParentForm.lcFabrics)
    REPLACE nAllocated WITH nAllocated - EVALUATE(loParentForm.lcTmpOrd+'.nRequired-'+loParentForm.lcTmpOrd+'.nWIPUsed')
    SELECT (loParentForm.lcTmpOrd)
    =SEEK(lcKey)

    SKIP
    lnRec1 = RECNO()
    SKIP -1

    REPLACE cSelect  WITH ' ',;
      lSelect  WITH .F.,;
      cFabWare WITH SPACE(6),;
      Dyelot   WITH SPACE(10),;
      nWIPUsed WITH 0

    IF lnRec1 > RECCOUNT()
      GO TOP
    ELSE
      GO lnRec1
    ENDIF

    IF EVALUATE(loParentForm.lcFabrics+'.nAllocated')=0
      SELECT (loParentForm.lcOrdGroup)
      REPLACE cWareCode WITH '',;
        Dyelot    WITH ''
      SELECT (loParentForm.lcTmpOrd)
    ENDIF
  ENDIF

  =lfShowOrd(loFormSet)

  *!*************************************************************
  *! Name      : lfUpdPO
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/12/2004
  *! Purpose   : Update the main files in case of generating PO
  *!*************************************************************
  *! Calls     : gfModalGen(),gfSequence()
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfUpdPO()
  *!*************************************************************
FUNCTION lfUpdPO
  LPARAMETERS loFormSet


  LOCAL lcDivision,lcPurCode,lcPo,lcGrade,lcPONumber , llUpdBom , llGnCTBom
  PRIVATE loBom
  loBom = CREATEOBJECT('RemoteTable','Bom','MULTIBOM','Bom',loFormSet.DATASESSIONID)
  llGnCTBom = (gfGetMemVar("M_CRTCSTSH")  = "T")

  *-- if the system support dyelots
  IF loFormSet.llDyelot
    SELECT (loFormSet.lcPOLine)
    SCAN FOR EMPTY(Dyelot)
      *-- if the style is dyelot and the dyelot fields is empty
      *-- stop saving and give the user a message
      IF loFormSet.loStyle.SEEK(STYLE) AND (STYLE.cDye_FLg = 'Y')
        *-- You have to enter dyelot for the required dyelots records
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *=gfModalGen('INM38135B38018','DIALOG',LANG_MFGENCT_POORDERS)
        =gfModalGen('INM38135B38018','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_POORDERS,loFormSet.GetHeaderText("LANG_MFGENCT_POORDERS",loFormSet.HeaderAlias)))
        *N000682,1 11/20/2012 MMT Globlization changes[End]

        RETURN .F.
      ENDIF
    ENDSCAN
  ENDIF
  LOCAL lcShipVia
  loFormSet.loPOSLN.SETORDER('POSLN')
  loFormSet.lcFirstCT  = SPACE(06)
  SELECT (loFormSet.lcPOH)
  *-- Scan the tmp.POHdr for qty > 0 to generate auto. PO numbers
  SCAN FOR nStyOrder > 0
    lcPo       = PO
    llUpdBom   = .T.
    lcDivision = cDivision
    lcGrade    = cStyGrade
    lcPurCode  = cPurCode
    lcShipVia  = SPACE(6)
    *! B609696,1 MMT 10/13/2011 Fix bug of not updating field apvendor.nVenOpnPo [Start]
    lnVenOpnPo = 0
    *! B609696,1 MMT 10/13/2011 Fix bug of not updating field apvendor.nVenOpnPo [END]

    *B608873,1 WAM 05/25/2009 Add new option to create one PO per each style major
    lcStyle = Style
    *B608873,1 WAM 05/25/2009 (End)

    *B608246,1 MMT 08/29/2007 fix bug of error while updating while saving [Start]
    *lcPONumber = IIF(EMPTY(PO),IIF(EMPTY(cTmpPo),gfSequence('PO','','',cDivision),cTmpPo),PO)
    lcPONumber = IIF(EMPTY(PO),IIF(EMPTY(cTmpPo),gfSequence('PO' ,'','',cDivision ,'','PP','POSHDR','POSHDR'),cTmpPo),PO)
    *B608246,1 MMT 08/29/2007 fix bug of error while updating while saving [END]

    =RLOCK()
    REPLACE cTmpPo WITH lcPONumber
    UNLOCK
    loFormSet.lcFirstCT = IIF(EMPTY(loFormSet.lcFirstCT),lcPONumber,loFormSet.lcFirstCT)
    loFormSet.lcLastCt  = lcPONumber
    SELECT (loFormSet.lcPOLine)

    *B608873,1 WAM 05/25/2009 Add new option to create one PO per each style major
    *=SEEK(lcPo+lcDivision+lcPurCode+lcGrade)
    *SCAN REST WHILE PO+cDivision+cPurCode+cStyGrade+STYLE+Dyelot = lcPo+lcDivision+lcPurCode+lcGrade FOR TotQty > 0
    =SEEK(lcPo+lcDivision+lcPurCode+lcGrade+IIF(loFormSet.llRPGrpByST,LEFT(lcStyle,loFormSet.lnMajorLen),''))
    SCAN REST WHILE PO+cDivision+cPurCode+cStyGrade+STYLE+Dyelot = ;
    lcPo+lcDivision+lcPurCode+lcGrade+IIF(loFormSet.llRPGrpByST,LEFT(lcStyle,loFormSet.lnMajorLen),'') FOR TotQty > 0
    *B608873,1 WAM 05/25/2009 (End)

      *B131131,1 KHM 05/02/2006 [Begin]
      *llUpdBom = IIF(llUpdBom,loBom.SEEK(cInvType+SUBSTR(Style,1,loFormSet.lnMajorLen)),llUpdBom)
      llUpdBom = IIF(llUpdBom,loBom.SEEK('0001'+SUBSTR(STYLE,1,loFormSet.lnMajorLen)),llUpdBom)
      *B131131,1 KHM 05/02/2006 [End]

      IF EMPTY(lcShipVia)
        lcShipVia = SHIPVIA
      ELSE
        IF lcShipVia # '.T.' AND SHIPVIA # lcShipVia
          lcShipVia = '.T.'
        ENDIF
      ENDIF

      IF !loFormSet.loPOSLN.SEEK('PP'+lcPONumber+STYLE+STR(LINENO,6)+TRANCD)
        SCATTER MEMVAR
        m.PO = lcPONumber
        =loFormSet.loStyle.SEEK(m.STYLE)
        m.Scale    = STYLE.SCALE
        m.nFCost1  = m.nFCost1 / m.TotQty
        m.nFCost2  = m.nFCost2 / m.TotQty
        m.nFCost3  = m.nFCost3 / m.TotQty
        m.nFCost4  = m.nFCost4 / m.TotQty
        m.nFCost5  = m.nFCost5 / m.TotQty
        m.nFCost6  = m.nFCost6 / m.TotQty
        m.nFCost7  = m.nFCost7 / m.TotQty
        m.nICost1  = m.nICost1 / m.TotQty
        m.nICost2  = m.nICost2 / m.TotQty
        m.nICost3  = m.nICost3 / m.TotQty
        m.nICost4  = m.nICost4 / m.TotQty
        m.nICost5  = m.nICost5 / m.TotQty
        m.nICost6  = m.nICost6 / m.TotQty
        m.nICost7  = m.nICost7 / m.TotQty
        m.cstytype = 'P'
        m.cbusdocu = 'P'
        m.cinvtype = '0001'
        *N000606,1 NNA 10/06/2007 (Begin) Seek At Stydye Table
        *IF !loStyDye.SEEK(m.STYLE+m.cWareCode+SPACE(10))
        IF !loStyDye.SEEK(m.STYLE+m.cWareCode+SPACE(10),'STYDYE')
        *N000606,1 NNA (End)

          *N000682,1 11/20/2012 MMT Globlization changes[Start]
          *WAIT LANG_MFGENCT_ASSIGNSTYLE + ALLTRIM(m.STYLE) + " " + LANG_MFGENCT_TOWAREHOUSE + " " + ALLTRIM(m.cWareCode) WINDOW NOWAIT
          WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ASSIGNSTYLE,loFormSet.GetHeaderText("LANG_MFGENCT_ASSIGNSTYLE",loFormSet.HeaderAlias)) + ALLTRIM(m.STYLE) + " " + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_TOWAREHOUSE,loFormSet.GetHeaderText("LANG_MFGENCT_TOWAREHOUSE",loFormSet.HeaderAlias)) + " " + ALLTRIM(m.cWareCode) WINDOW NOWAIT
          *N000682,1 11/20/2012 MMT Globlization changes[End]

          DO gpAdStyWar WITH m.STYLE,SPACE(10),m.cWareCode
          *N000606,1 NNA 10/06/2007 (Begin) Seek At Stydye File
          *=loStyDye.SEEK(m.STYLE+m.cWareCode+SPACE(10))
           =loStyDye.SEEK(m.STYLE+m.cWareCode+SPACE(10),'STYDYE')
           *N000606,1 NNA (End)
          WAIT CLEAR
        ENDIF
        IF EVALUATE(loFormSet.lcPOLine+'.nSteps') < 1 AND STYLE.lInvSty
          SELECT STYDYE
          =RLOCK()
          loStyDye.REPLACE("WIP1   WITH WIP1   + m.Qty1  ,"+;
            "WIP2   WITH WIP2   + m.Qty2  ,"+;
            "WIP3   WITH WIP3   + m.Qty3  ,"+;
            "WIP4   WITH WIP4   + m.Qty4  ,"+;
            "WIP5   WITH WIP5   + m.Qty5  ,"+;
            "WIP6   WITH WIP6   + m.Qty6  ,"+;
            "WIP7   WITH WIP7   + m.Qty7  ,"+;
            "WIP8   WITH WIP8   + m.Qty8  ,"+;
            "TotWIP WITH TotWIP + m.TotQty,"+;
            "nWO1   WITH nWO1   + m.Qty1  ,"+;
            "nWO2   WITH nWO2   + m.Qty2  ,"+;
            "nWO3   WITH nWO3   + m.Qty3  ,"+;
            "nWO4   WITH nWO4   + m.Qty4  ,"+;
            "nWO5   WITH nWO5   + m.Qty5  ,"+;
            "nWO6   WITH nWO6   + m.Qty6  ,"+;
            "nWO7   WITH nWO7   + m.Qty7  ,"+;
            "nWO8   WITH nWO8   + m.Qty8  ,"+;
            "nTotWo WITH nTotWo + m.TotQty")
          UNLOCK
          SELECT (loFormSet.lcPOLine)
          =RLOCK()
          REPLACE nSteps WITH 1
          UNLOCK
        ENDIF
        IF EVALUATE(loFormSet.lcPOLine+'.nSteps') < 2 AND STYLE.lInvSty
          SELECT STYLE
          =RLOCK()
          loFormSet.loStyle.REPLACE("WIP1   WITH WIP1   + m.Qty1  ,"+;
            "WIP2   WITH WIP2   + m.Qty2  ,"+;
            "WIP3   WITH WIP3   + m.Qty3  ,"+;
            "WIP4   WITH WIP4   + m.Qty4  ,"+;
            "WIP5   WITH WIP5   + m.Qty5  ,"+;
            "WIP6   WITH WIP6   + m.Qty6  ,"+;
            "WIP7   WITH WIP7   + m.Qty7  ,"+;
            "WIP8   WITH WIP8   + m.Qty8  ,"+;
            "TotWIP WITH TotWIP + m.TotQty,"+;
            "nWO1   WITH nWO1   + m.Qty1  ,"+;
            "nWO2   WITH nWO2   + m.Qty2  ,"+;
            "nWO3   WITH nWO3   + m.Qty3  ,"+;
            "nWO4   WITH nWO4   + m.Qty4  ,"+;
            "nWO5   WITH nWO5   + m.Qty5  ,"+;
            "nWO6   WITH nWO6   + m.Qty6  ,"+;
            "nWO7   WITH nWO7   + m.Qty7  ,"+;
            "nWO8   WITH nWO8   + m.Qty8  ,"+;
            "nTotWo WITH nTotWo + m.TotQty")
          UNLOCK
          SELECT (loFormSet.lcPOLine)
          =RLOCK()
          REPLACE nSteps WITH 2
          UNLOCK
        ENDIF
        m.cAdd_User = oAriaApplication.User_ID
        m.cAdd_Time = TIME()
        m.dAdd_Date = oAriaApplication.systemdate
        SELECT (loFormSet.lcPOLine)
        =RLOCK()
        REPLACE nSteps WITH 3
        UNLOCK
        *! B609696,1 MMT 10/13/2011 Fix bug of not updating field apvendor.nVenOpnPo [Start]
        lnVenOpnPo = lnVenOpnPo + (m.TOTQTY*m.nFCost1)
        *! B609696,1 MMT 10/13/2011 Fix bug of not updating field apvendor.nVenOpnPo [END]

        loFormSet.loPOSLN.INSERT('FROM MEMVAR')
        *B608264,1 WAM 09/10/2007 Update dyelot WIP and WO quantity when generate PO from SO
        IF EVALUATE(loFormSet.lcPOLine+'.nSteps') < 4 AND STYLE.lInvSty AND !EMPTY(m.Dyelot) AND loStyDye.SEEK(m.STYLE+m.cWareCode+m.Dyelot)
          SELECT STYDYE
          =RLOCK()
          loStyDye.REPLACE("WIP1   WITH WIP1   + m.Qty1  ,"+;
            "WIP2   WITH WIP2   + m.Qty2  ,"+;
            "WIP3   WITH WIP3   + m.Qty3  ,"+;
            "WIP4   WITH WIP4   + m.Qty4  ,"+;
            "WIP5   WITH WIP5   + m.Qty5  ,"+;
            "WIP6   WITH WIP6   + m.Qty6  ,"+;
            "WIP7   WITH WIP7   + m.Qty7  ,"+;
            "WIP8   WITH WIP8   + m.Qty8  ,"+;
            "TotWIP WITH TotWIP + m.TotQty,"+;
            "nWO1   WITH nWO1   + m.Qty1  ,"+;
            "nWO2   WITH nWO2   + m.Qty2  ,"+;
            "nWO3   WITH nWO3   + m.Qty3  ,"+;
            "nWO4   WITH nWO4   + m.Qty4  ,"+;
            "nWO5   WITH nWO5   + m.Qty5  ,"+;
            "nWO6   WITH nWO6   + m.Qty6  ,"+;
            "nWO7   WITH nWO7   + m.Qty7  ,"+;
            "nWO8   WITH nWO8   + m.Qty8  ,"+;
            "nTotWo WITH nTotWo + m.TotQty")
          UNLOCK
          SELECT (loFormSet.lcPOLine)
          =RLOCK()
          REPLACE nSteps WITH 4
          UNLOCK
        ENDIF
        *B608264,1 WAM 09/10/2007 (End)

      ELSE
        IF EVALUATE(loFormSet.lcPOLine+'.nSteps') < 1
          SELECT POSLN
          =RLOCK()
          loFormSet.loPOSLN.REPLACE("Ord1   WITH Ord1 + EVALUATE(loFormSet.lcPOLine+'.Ord1') ,"+;
            "Ord2   WITH Ord2 + EVALUATE(loFormSet.lcPOLine+'.Ord2') ,"+;
            "Ord3   WITH Ord3 + EVALUATE(loFormSet.lcPOLine+'.Ord3') ,"+;
            "Ord4   WITH Ord4 + EVALUATE(loFormSet.lcPOLine+'.Ord4') ,"+;
            "Ord5   WITH Ord5 + EVALUATE(loFormSet.lcPOLine+'.Ord5') ,"+;
            "Ord6   WITH Ord6 + EVALUATE(loFormSet.lcPOLine+'.Ord6') ,"+;
            "Ord7   WITH Ord7 + EVALUATE(loFormSet.lcPOLine+'.Ord7') ,"+;
            "Ord8   WITH Ord8 + EVALUATE(loFormSet.lcPOLine+'.Ord8') ,"+;
            "TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8")
          UNLOCK
          SELECT (loFormSet.lcPOLine)
          =RLOCK()
          REPLACE nSteps WITH 1
          UNLOCK
        ENDIF
      ENDIF
      *N000606,1 NNA 10/06/2007 (Begin) go into the next section only if lcChoice not in 'P' or 'O'
      *MMT
      *IF !INLIST(loFormSet.lcChoice,'O','P') THEN
      IF !INLIST(loFormSet.lcChoice,'O') THEN
      *MMT
      *N000606,1 NNA (End)

	      SELECT (loFormSet.lcCutPick)

	      *B607874,1 MMT 12/13/2006 Fix bug of not all lines of order are updated in Cutpick [Start]
	      SET KEY TO
	      *B607874,1 MMT 12/13/2006 Fix bug of not all lines of order are updated in Cutpick [End]

	      =SEEK(lcPo+lcDivision+lcPurCode+lcGrade+EVALUATE(loFormSet.lcPOLine+'.Style+'+loFormSet.lcPOLine+'.cWareCode+'+;
	        loFormSet.lcPOLine+'.Dyelot'))
	      SCAN REST WHILE cTKtNo+cDivision+cPurCode+cStyGrade+STYLE+cWareCode+Dyelot = lcPo+lcDivision+lcPurCode+lcGrade+;
	          EVALUATE(loFormSet.lcPOLine+'.Style+'+loFormSet.lcPOLine+'.cWareCode+'+loFormSet.lcPOLine+'.Dyelot');
	          FOR TotQty > 0

	        SCATTER MEMVAR

	        *!*        *C200376,1 AMH trigger for custom PUFFA to Update the style critical path file [Start]
	        *!*        IF ASCAN(laEvntTrig , PADR('UPDSTYCR',10)) <> 0
	        *!*          =gfDoTriger('POGENPO',PADR('UPDSTYCR',10))
	        *!*        ENDIF
	        *!*        *C200376,1 AMH [End]

	        SELECT ORDHDR
	        =loFormSet.loOrdHdr.SEEK('O'+m.Order)
	        IF EVALUATE(loFormSet.lcCutPick+'.nSteps') < 1
	          =RLOCK()
	          loFormSet.loOrdHdr.REPLACE("TotCut WITH TotCut + m.TotQty")
	          UNLOCK
	          SELECT (loFormSet.lcCutPick)
	          =RLOCK()
	          REPLACE nSteps WITH 1
	          UNLOCK
	        ENDIF
	        SELECT ORDLINE
	        =loFormSet.loOrdLine.SEEK('O'+m.Order+m.cOrdLine)
	        IF EVALUATE(loFormSet.lcCutPick+'.nSteps') < 2
	          =RLOCK()
	          loFormSet.loOrdLine.REPLACE("Dyelot WITH m.Dyelot     ,"+;
	            "Cut1   WITH Cut1 + m.Qty1,"+;
	            "Cut2   WITH Cut2 + m.Qty2,"+;
	            "Cut3   WITH Cut3 + m.Qty3,"+;
	            "Cut4   WITH Cut4 + m.Qty4,"+;
	            "Cut5   WITH Cut5 + m.Qty5,"+;
	            "Cut6   WITH Cut6 + m.Qty6,"+;
	            "Cut7   WITH Cut7 + m.Qty7,"+;
	            "Cut8   WITH Cut8 + m.Qty8,"+;
	            "TotCut WITH TotCut + m.TotQty")
	          UNLOCK
	          SELECT (loFormSet.lcCutPick)
	          =RLOCK()
	          REPLACE nSteps WITH 2
	          UNLOCK
	        ENDIF

	        m.cTKtNo     = lcPONumber
	        *C200804,1 HIA 06/28/2007 Create un\grouped po lines accoring to llGrpSO flag [custom developed as standard as many customers need this feature][Begin]
	        *m.cTktLineNo = STR(EVALUATE(loFormSet.lcPOLine+'.LineNo'),6)

	        *608597,1 ALAA [Begin] Commnetd out to rename varibale llGrpSO to be llRPGrpSO
	        *IF loFormSet.llGrpSO
	        *! B610190,1 MMT 01/03/2013 Modify PO screen and Generate PO from SO to Handle price/size in cost sheet[Start]
	        *IF loFormSet.llRPGrpSO
	        IF loFormSet.llRPGrpSO AND EMPTY(m.cTktLineNo)
	        *! B610190,1 MMT 01/03/2013 Modify PO screen and Generate PO from SO to Handle price/size in cost sheet[END]
	       *608597,1 ALAA [END] Commnetd out to rename varibale llGrpSO to be llRPGrpSO

	          m.cTktLineNo = STR(EVALUATE(loFormSet.lcPOLine+'.LineNo'),6)
	        ENDIF
	        *C200804,1 HIA 06/28/2007 Create un\grouped po lines accoring to llGrpSO flag [custom developed as standard as many customers need this feature][End]
	        IF EVALUATE(loFormSet.lcCutPick+'.nSteps') < 3
	          loFormSet.loCUTPICK.INSERT("FROM MEMVAR")
	          SELECT (loFormSet.lcCutPick)
	          =RLOCK()
	          REPLACE nSteps WITH 3
	          UNLOCK
	        ENDIF
	      ENDSCAN
	      REPLACE ALL cTKtNo WITH lcPONumber;
	        FOR cTKtNo+cDivision+cPurCode+cStyGrade+STYLE+cWareCode+Dyelot = lcPo+lcDivision+lcPurCode+lcGrade+;
	        EVALUATE(loFormSet.lcPOLine+'.Style+'+loFormSet.lcPOLine+'.cWareCode+'+loFormSet.lcPOLine+'.Dyelot');
	        AND TotQty > 0

	    *N000606,1 NNA (Begin)
	    ENDIF
      *N000606,1 NNA (End)

    ENDSCAN


    IF !loFormSet.loPosHdr.SEEK('PP'+lcPONumber)
      SELECT (loFormSet.lcPOH)
      SCATTER MEMVAR
      m.PO        = lcPONumber
      m.cMultiLot = 'S'
      =loFormSet.loWAREHOUS.SEEK(m.cWareCode)
      m.cOutAddr1 = WAREHOUS.cAddress1
      m.cOutAddr2 = WAREHOUS.cAddress2
      m.cOutAddr3 = WAREHOUS.cAddress3
      m.cOutAddr4 = WAREHOUS.cAddress4
      m.cOutAddr5 = WAREHOUS.cAddress5
      m.ShpName   = WAREHOUS.cDesc

      *B608873,1 WAM 05/25/2009 Commeted out
      *m.Available = COMPLETE
      *B608873,1 WAM 05/25/2009 (End)
      m.Open      = nStyOrder
      m.cstytype  = "P"
      m.cbusdocu  = "P"
      m.Contact   = loFormSet.lcCont
      m.Phone     = loFormSet.lcPhone
      m.POTotal   = nICost1+nICost2+nICost3+nICost4+nICost5+nICost6+nICost7
      m.cAdd_User = oAriaApplication.User_ID
      m.cAdd_Time = TIME()
      m.dAdd_Date = oAriaApplication.systemdate

      IF !EMPTY(lcShipVia) AND lcShipVia # '.T.'
        m.SHIPVIA = lcShipVia
      ENDIF

      =RLOCK()
      REPLACE nSteps WITH 1
      UNLOCK
      loFormSet.loPosHdr.INSERT("FROM MEMVAR")
      *! B609696,1 MMT 10/13/2011 Fix bug of not updating field apvendor.nVenOpnPo [Start]
      IF "AP" $ oAriaApplication.CompanyInstalledModules
        =loFormSet.loApVendor.SEEK(m.vendor,'VENCODE')
        loFormSet.loApVendor.REPLACE("nVenOpnPO WITH nVenOpnPO + lnVenOpnPo")
      ENDIF
      *! B609696,1 MMT 10/13/2011 Fix bug of not updating field apvendor.nVenOpnPo [End]

    ELSE
      IF EVALUATE(loFormSet.lcPOH+'.nSteps') < 1
        SELECT POSHDR
        =RLOCK()
        loFormSet.loPosHdr.REPLACE("FLAG   WITH '' ,TotOrd WITH TotOrd + EVALUATE(loFormSet.lcPOH+'.TotOrd')")
        UNLOCK
        SELECT (loFormSet.lcPOH)
        =RLOCK()
        REPLACE nSteps WITH 1
        UNLOCK
      ENDIF
    ENDIF
    *! B611059,1 MMT 09/28/2015 Generate PO from SO does not add record to EDITRNAS table[T20150130.0005][Start]
    IF 'EB' $ oAriaApplication.CompanyInstalledModules
      lcAlias = SELECT()    
      =gfOpenFile(oAriaApplication.DataDir+'EDIACPRT',oAriaApplication.DataDir+'ACCFACT','SH')
      =gfOpenFile(oAriaApplication.DataDir+'EDIPD',oAriaApplication.DataDir+'PARTTRANS','SH')
      =gfOpenFile(oAriaApplication.DataDir+'EDITRANS',oAriaApplication.DataDir+'TYPEKEY','SH')
      lcVendor = EVALUATE(loFormSet.lcPOH+".Vendor")
      lcPoNo = lcPONumber
      IF SEEK('V'+lcVendor,'EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'850','EDIPD')
        SELECT EDITRANS
        IF !SEEK('850'+PADR(lcPoNo,40)+'V'+lcVendor)
          INSERT INTO 'EDITRANS' (cEdiTrnTyp,Key,Type,cPartner) ;
                        VALUES ('850',lcPoNo,'V',lcVendor)
        ENDIF
        REPLACE cStatus WITH 'N'
      ENDIF 
    ENDIF
    *! B611059,1 MMT 09/28/2015 Generate PO from SO does not add record to EDITRNAS table[T20150130.0005][End]
    *-- Update master tables.
    *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
    IF loFormSet.lcChoice='O' THEN
	    DECLARE laTableUpdate[4]
	    laTableUpdate[1] = loStyDye
  	  laTableUpdate[2] = loFormSet.loStyle
	    laTableUpdate[3] = loFormSet.loPosLn
  	  laTableUpdate[4] = loFormSet.loPosHdr
      *! B609696,1 MMT 10/13/2011 Fix bug of not updating field apvendor.nVenOpnPo [Start]
      IF "AP" $ oAriaApplication.CompanyInstalledModules
        DIMENSION laTableUpdate[5]
        laTableUpdate[5] = loFormSet.loApVendor
      ENDIF
      *! B609696,1 MMT 10/13/2011 Fix bug of not updating field apvendor.nVenOpnPo [END]

    ELSE
	    *N000606,1 NNA (End)

	    DECLARE laTableUpdate[7]
	    laTableUpdate[1] = loStyDye
	    laTableUpdate[2] = loFormSet.loStyle
	    laTableUpdate[3] = loFormSet.loPOSLN
	    laTableUpdate[4] = loFormSet.loOrdHdr
	    laTableUpdate[5] = loFormSet.loOrdLine
	    laTableUpdate[6] = loFormSet.loPosHdr
	    laTableUpdate[7] = loFormSet.loCUTPICK
      *! B609696,1 MMT 10/13/2011 Fix bug of not updating field apvendor.nVenOpnPo [Start]
      IF "AP" $ oAriaApplication.CompanyInstalledModules
        DIMENSION laTableUpdate[8]
        laTableUpdate[8] = loFormSet.loApVendor
      ENDIF
      *! B609696,1 MMT 10/13/2011 Fix bug of not updating field apvendor.nVenOpnPo [END]
    *N000606,1 NNA 10/06/2007 (Begin)
    ENDIF
    *N000606,1 NNA (End)

    IF !lfTableUpdate()
      RETURN .F.
    ENDIF

    *--- Get the module seting for generating cost sheet
    *--- if this seting is alwayes ("A") we will call function to call
    *--- C/T,PO's Cost Sheet program to generate Cost Sheet.
    *B131131,1 KHM 05/02/2006 Pass the correct parameter of PO [Begin]
    *llDumy = llUpdBom .AND. loFormSet.ActiveMode="A" .AND. llGnCTBom .AND. lfGenCutBom('',POSHDR.PO,loFormSet)
    llDumy = llUpdBom .AND. loFormSet.ActiveMode="A" .AND. llGnCTBom .AND. lfGenCutBom('',lcPONumber,loFormSet)
    *B131131,1 KHM 05/02/2006 [End]
  ENDSCAN

  loBom.DESTROY

  SELECT (loFormSet.lcPOH)
  REPLACE ALL PO WITH cTmpPo
  GO TOP

  *!*************************************************************
  *! Name      : lfUpdCT
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 04/24/2005
  *! Purpose   : Update the original files in case of generating C/T from orders
  *!*************************************************************
  *! Calls     : gfModalGen(),gfSequence()
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfUpdCT()
  *!*************************************************************
FUNCTION lfUpdCT
  LPARAMETERS loFormSet

  LOCAL lcDivision,lcPo,lcPONumber,llUpdBom,llGnCTBom,lcSeason
  PRIVATE loBom
  loBom = CREATEOBJECT('RemoteTable','Bom','MULTIBOM','Bom',loFormSet.DATASESSIONID)
  llGnCTBom = (gfGetMemVar("M_GNCTBOM")  = "T")

  *-- if the system support dyelots
  IF loFormSet.llDyelot
    SELECT (loFormSet.lcPOLine)
    SCAN FOR EMPTY(Dyelot)
      *-- if the style is dyelot and the dyelot fields is empty
      *-- stop saving and give the user a message
      IF loFormSet.loStyle.SEEK(STYLE) AND (STYLE.cDye_FLg = 'Y')
        *-- You have to enter dyelot for the required dyelots records
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *=gfModalGen('INM38135B38018','DIALOG',LANG_MFGENCT_CTORDERS)
        =gfModalGen('INM38135B38018','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CTORDERS,loFormSet.GetHeaderText("LANG_MFGENCT_CTORDERS",loFormSet.HeaderAlias)))
        *N000682,1 11/20/2012 MMT Globlization changes[End]

        RETURN .F.
      ENDIF
    ENDSCAN
  ENDIF

  loFormSet.loPOSLN.SETORDER('POSLN')
  loFormSet.lcFirstCT  = SPACE(06)
  SELECT (loFormSet.lcPOH)
  *-- scan the tmp. Cuttkth for qtys > 0 to gfenerate auto. C/T numbers
  SCAN FOR nStyOrder > 0
    loFormSet.loStyle.SEEK(STYLE)
    lcStyle    = SUBSTR(STYLE,1,loFormSet.lnMajorLen)
    lcPo       = PO
    llUpdBom   = .T.
    lcDivision = cDivision
    lcSeason   = SEASON
    lcYear     = cYear
    lcWeek     = cWeek
    *B608122,1 TMI [Start] Call the gfSequence global function with passing the new parameters lcTranType,lcTable,lcTag
    *lcPONumber = IIF(EMPTY(PO),IIF(EMPTY(cTmpPo),gfSequence("CUTTKT","","",lcDivision),cTmpPo),PO)
    lcPONumber = IIF(EMPTY(PO),IIF(EMPTY(cTmpPo),gfSequence('CUTTKT','','',lcDivision,'','PU','POSHDR','POSHDR'),cTmpPo),PO)
    *B608122,1 TMI [End  ]
    =RLOCK()
    REPLACE cTmpPo WITH lcPONumber
    UNLOCK
    loFormSet.lcFirstCT = IIF(EMPTY(loFormSet.lcFirstCT),lcPONumber,loFormSet.lcFirstCT)
    loFormSet.lcLastCt  = lcPONumber
    SELECT (loFormSet.lcPOLine)
    =SEEK(lcPo+lcDivision+lcSeason+lcWeek+lcYear+lcStyle)
    SCAN REST WHILE PO+cDivision+SEASON+cWeek+cYear+STYLE+Dyelot = lcPo+lcDivision+lcSeason+lcWeek+lcYear+lcStyle FOR TotQty > 0
      llUpdBom = IIF(llUpdBom,loBom.SEEK(cinvtype+SUBSTR(STYLE,1,loFormSet.lnMajorLen)),llUpdBom)
      IF !loFormSet.loPOSLN.SEEK('PU'+lcPONumber+'0001'+STYLE+STR(LINENO,6)+TRANCD)
        SCATTER MEMVAR
        m.PO = lcPONumber
        =loFormSet.loStyle.SEEK(m.STYLE)
        m.Scale    = STYLE.SCALE
        m.nFCost1  = m.nFCost1 / m.TotQty
        m.nFCost2  = m.nFCost2 / m.TotQty
        m.nFCost3  = m.nFCost3 / m.TotQty
        m.nFCost4  = m.nFCost4 / m.TotQty
        m.nFCost5  = m.nFCost5 / m.TotQty
        m.nFCost6  = m.nFCost6 / m.TotQty
        m.nFCost7  = m.nFCost7 / m.TotQty
        m.nICost1  = m.nICost1 / m.TotQty
        m.nICost2  = m.nICost2 / m.TotQty
        m.nICost3  = m.nICost3 / m.TotQty
        m.nICost4  = m.nICost4 / m.TotQty
        m.nICost5  = m.nICost5 / m.TotQty
        m.nICost6  = m.nICost6 / m.TotQty
        m.nICost7  = m.nICost7 / m.TotQty
        m.cstytype = 'U'
        m.cbusdocu = 'P'
        m.cinvtype = '0001'
        IF !loStyDye.SEEK(m.STYLE+m.cWareCode+SPACE(10))
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
          *WAIT LANG_MFGENCT_ASSIGNSTYLE + ALLTRIM(m.STYLE) + " " + LANG_MFGENCT_TOWAREHOUSE + " " + ALLTRIM(m.cWareCode) WINDOW NOWAIT
          WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ASSIGNSTYLE,loFormSet.GetHeaderText("LANG_MFGENCT_ASSIGNSTYLE",loFormSet.HeaderAlias)) + ALLTRIM(m.STYLE) + " " + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_TOWAREHOUSE,loFormSet.GetHeaderText("LANG_MFGENCT_TOWAREHOUSE",loFormSet.HeaderAlias)) + " " + ALLTRIM(m.cWareCode) WINDOW NOWAIT
          *N000682,1 11/20/2012 MMT Globlization changes[End]

          DO gpAdStyWar WITH m.STYLE,SPACE(10),m.cWareCode
          WAIT CLEAR
        ENDIF
        IF !EMPTY(m.Dyelot) AND !loStyDye.SEEK(m.STYLE+m.cWareCode+m.Dyelot)
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
          *WAIT LANG_MFGENCT_ASSIGNSTYLE + ALLTRIM(m.STYLE) + " " + LANG_MFGENCT_TOWAREHOUSE + " " + ALLTRIM(m.cWareCode) WINDOW NOWAIT
          WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ASSIGNSTYLE,loFormSet.GetHeaderText("LANG_MFGENCT_ASSIGNSTYLE",loFormSet.HeaderAlias)) + ALLTRIM(m.STYLE) + " " + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_TOWAREHOUSE,loFormSet.GetHeaderText("LANG_MFGENCT_TOWAREHOUSE",loFormSet.HeaderAlias)) + " " + ALLTRIM(m.cWareCode) WINDOW NOWAIT
          *N000682,1 11/20/2012 MMT Globlization changes[End]

          DO gpAdStyWar WITH m.STYLE,m.Dyelot,m.cWareCode
          WAIT CLEAR
        ENDIF
        IF EVALUATE(loFormSet.lcPOLine+'.nSteps') < 1 AND STYLE.lInvSty
          SELECT STYDYE
          =loStyDye.SEEK(m.STYLE+m.cWareCode+SPACE(10))
          =RLOCK()
          loStyDye.REPLACE("WIP1   WITH WIP1   + m.Qty1  ,"+;
            "WIP2   WITH WIP2   + m.Qty2  ,"+;
            "WIP3   WITH WIP3   + m.Qty3  ,"+;
            "WIP4   WITH WIP4   + m.Qty4  ,"+;
            "WIP5   WITH WIP5   + m.Qty5  ,"+;
            "WIP6   WITH WIP6   + m.Qty6  ,"+;
            "WIP7   WITH WIP7   + m.Qty7  ,"+;
            "WIP8   WITH WIP8   + m.Qty8  ,"+;
            "TotWIP WITH TotWIP + m.TotQty,"+;
            "nWO1   WITH nWO1   + m.Qty1  ,"+;
            "nWO2   WITH nWO2   + m.Qty2  ,"+;
            "nWO3   WITH nWO3   + m.Qty3  ,"+;
            "nWO4   WITH nWO4   + m.Qty4  ,"+;
            "nWO5   WITH nWO5   + m.Qty5  ,"+;
            "nWO6   WITH nWO6   + m.Qty6  ,"+;
            "nWO7   WITH nWO7   + m.Qty7  ,"+;
            "nWO8   WITH nWO8   + m.Qty8  ,"+;
            "nTotWo WITH nTotWo + m.TotQty")
          UNLOCK
          SELECT (loFormSet.lcPOLine)
          =RLOCK()
          REPLACE nSteps WITH 1
          UNLOCK
        ENDIF
        IF !EMPTY(m.Dyelot) AND EVALUATE(loFormSet.lcPOLine+'.nSteps') < 2 AND STYLE.lInvSty
          SELECT STYDYE
          =loStyDye.SEEK(m.STYLE+m.cWareCode+m.Dyelot)
          =RLOCK()
          loStyDye.REPLACE("WIP1   WITH WIP1   + m.Qty1  ,"+;
            "WIP2   WITH WIP2   + m.Qty2  ,"+;
            "WIP3   WITH WIP3   + m.Qty3  ,"+;
            "WIP4   WITH WIP4   + m.Qty4  ,"+;
            "WIP5   WITH WIP5   + m.Qty5  ,"+;
            "WIP6   WITH WIP6   + m.Qty6  ,"+;
            "WIP7   WITH WIP7   + m.Qty7  ,"+;
            "WIP8   WITH WIP8   + m.Qty8  ,"+;
            "TotWIP WITH TotWIP + m.TotQty,"+;
            "nWO1   WITH nWO1   + m.Qty1  ,"+;
            "nWO2   WITH nWO2   + m.Qty2  ,"+;
            "nWO3   WITH nWO3   + m.Qty3  ,"+;
            "nWO4   WITH nWO4   + m.Qty4  ,"+;
            "nWO5   WITH nWO5   + m.Qty5  ,"+;
            "nWO6   WITH nWO6   + m.Qty6  ,"+;
            "nWO7   WITH nWO7   + m.Qty7  ,"+;
            "nWO8   WITH nWO8   + m.Qty8  ,"+;
            "nTotWo WITH nTotWo + m.TotQty")
          UNLOCK
          SELECT (loFormSet.lcPOLine)
          =RLOCK()
          REPLACE nSteps WITH 2
          UNLOCK
        ENDIF
        IF EVALUATE(loFormSet.lcPOLine+'.nSteps') < 3 AND STYLE.lInvSty
          SELECT STYLE
          =RLOCK()
          loFormSet.loStyle.REPLACE("WIP1   WITH WIP1   + m.Qty1  ,"+;
            "WIP2   WITH WIP2   + m.Qty2  ,"+;
            "WIP3   WITH WIP3   + m.Qty3  ,"+;
            "WIP4   WITH WIP4   + m.Qty4  ,"+;
            "WIP5   WITH WIP5   + m.Qty5  ,"+;
            "WIP6   WITH WIP6   + m.Qty6  ,"+;
            "WIP7   WITH WIP7   + m.Qty7  ,"+;
            "WIP8   WITH WIP8   + m.Qty8  ,"+;
            "TotWIP WITH TotWIP + m.TotQty,"+;
            "nWO1   WITH nWO1   + m.Qty1  ,"+;
            "nWO2   WITH nWO2   + m.Qty2  ,"+;
            "nWO3   WITH nWO3   + m.Qty3  ,"+;
            "nWO4   WITH nWO4   + m.Qty4  ,"+;
            "nWO5   WITH nWO5   + m.Qty5  ,"+;
            "nWO6   WITH nWO6   + m.Qty6  ,"+;
            "nWO7   WITH nWO7   + m.Qty7  ,"+;
            "nWO8   WITH nWO8   + m.Qty8  ,"+;
            "nTotWo WITH nTotWo + m.TotQty")
          UNLOCK
          SELECT (loFormSet.lcPOLine)
          =RLOCK()
          REPLACE nSteps WITH 3
          UNLOCK
        ENDIF
        m.cAdd_User = oAriaApplication.User_ID
        m.cAdd_Time = TIME()
        m.dAdd_Date = oAriaApplication.systemdate
        SELECT (loFormSet.lcPOLine)
        =RLOCK()
        REPLACE nSteps WITH 4
        UNLOCK
        loFormSet.loPOSLN.INSERT('FROM MEMVAR')
      ELSE
        IF EVALUATE(loFormSet.lcPOLine+'.nSteps') < 1
          SELECT POSLN
          =RLOCK()
          loFormSet.loPOSLN.REPLACE("Ord1   WITH Ord1 + EVALUATE(loFormSet.lcPOLine+'.Ord1') ,"+;
            "Ord2   WITH Ord2 + EVALUATE(loFormSet.lcPOLine+'.Ord2') ,"+;
            "Ord3   WITH Ord3 + EVALUATE(loFormSet.lcPOLine+'.Ord3') ,"+;
            "Ord4   WITH Ord4 + EVALUATE(loFormSet.lcPOLine+'.Ord4') ,"+;
            "Ord5   WITH Ord5 + EVALUATE(loFormSet.lcPOLine+'.Ord5') ,"+;
            "Ord6   WITH Ord6 + EVALUATE(loFormSet.lcPOLine+'.Ord6') ,"+;
            "Ord7   WITH Ord7 + EVALUATE(loFormSet.lcPOLine+'.Ord7') ,"+;
            "Ord8   WITH Ord8 + EVALUATE(loFormSet.lcPOLine+'.Ord8') ,"+;
            "TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8")
          UNLOCK
          SELECT (loFormSet.lcPOLine)
          =RLOCK()
          REPLACE nSteps WITH 1
          UNLOCK
        ENDIF
      ENDIF
      SELECT (loFormSet.lcCutPick)

      *B607844,1 MMT 11/28/2006 Fix bug of not all lines of order are updated in Cutpick [Start]
      SET KEY TO
      *B607844,1 MMT 11/28/2006 Fix bug of not all lines of order are updated in Cutpick [End]
      =SEEK(lcPo+lcDivision+lcSeason+EVALUATE(loFormSet.lcPOLine+'.Style+'+loFormSet.lcPOLine+'.cWareCode+'+;
        loFormSet.lcPOLine+'.Dyelot'))
      SCAN REST WHILE cTKtNo+cDivision+SEASON+STYLE+cWareCode+Dyelot=lcPo+lcDivision+lcSeason+;
          EVALUATE(loFormSet.lcPOLine+'.Style+'+loFormSet.lcPOLine+'.cWareCode+'+loFormSet.lcPOLine+'.Dyelot');
          FOR TotQty > 0
        SCATTER MEMVAR
        SELECT ORDHDR
        =loFormSet.loOrdHdr.SEEK('O'+m.Order)
        IF EVALUATE(loFormSet.lcCutPick+'.nSteps') < 1
          =RLOCK()
          loFormSet.loOrdHdr.REPLACE("TotCut WITH TotCut + m.TotQty")
          UNLOCK
          SELECT (loFormSet.lcCutPick)
          =RLOCK()
          REPLACE nSteps WITH 1
          UNLOCK
        ENDIF
        SELECT ORDLINE
        =loFormSet.loOrdLine.SEEK('O'+m.Order+m.cOrdLine)
        IF EVALUATE(loFormSet.lcCutPick+'.nSteps') < 2
          =RLOCK()
          loFormSet.loOrdLine.REPLACE("Dyelot WITH m.Dyelot     ,"+;
            "Cut1   WITH Cut1 + m.Qty1,"+;
            "Cut2   WITH Cut2 + m.Qty2,"+;
            "Cut3   WITH Cut3 + m.Qty3,"+;
            "Cut4   WITH Cut4 + m.Qty4,"+;
            "Cut5   WITH Cut5 + m.Qty5,"+;
            "Cut6   WITH Cut6 + m.Qty6,"+;
            "Cut7   WITH Cut7 + m.Qty7,"+;
            "Cut8   WITH Cut8 + m.Qty8,"+;
            "TotCut WITH TotCut + m.TotQty")
          UNLOCK
          SELECT (loFormSet.lcCutPick)
          =RLOCK()
          REPLACE nSteps WITH 2
          UNLOCK
        ENDIF
        m.cTKtNo     = lcPONumber

        *C200804,1 HIA 06/28/2007 Create un\grouped po lines accoring to llGrpSO flag [custom developed as standard as many customers need this feature][Begin]
        *m.cTktLineNo = STR(EVALUATE(loFormSet.lcPOLine+'.LineNo'),6)

         *608597,1 ALAA [Begin] Commnetd out to rename varibale llGrpSO to be llRPGrpSO
         *IF loFormSet.llGrpSO
          IF loFormSet.llRPGrpSO
         *608597,1 ALAA [END] Commnetd out to rename varibale llGrpSO to be llRPGrpSO

          m.cTktLineNo = STR(EVALUATE(loFormSet.lcPOLine+'.LineNo'),6)
        ENDIF
        *C200804,1 HIA 06/28/2007 Create un\grouped po lines accoring to llGrpSO flag [custom developed as standard as many customers need this feature][End]
          loFormSet.loCUTPICK.INSERT("FROM MEMVAR")
          SELECT (loFormSet.lcCutPick)
          =RLOCK()
          REPLACE nSteps WITH 3
          UNLOCK
      ENDSCAN
      REPLACE ALL cTKtNo WITH lcPONumber;
        FOR cTKtNo+cDivision+SEASON+STYLE+cWareCode+Dyelot = lcPo+lcDivision+lcSeason+;
        EVALUATE(loFormSet.lcPOLine+'.Style+'+loFormSet.lcPOLine+'.cWareCode+'+loFormSet.lcPOLine+'.Dyelot');
        AND TotQty > 0
    ENDSCAN

    IF !loFormSet.loPosHdr.SEEK('PU'+lcPONumber)
      SELECT (loFormSet.lcPOH)
      SCATTER MEMVAR
      m.PO        = lcPONumber
      m.cMultiLot = 'S'
      =loFormSet.loWAREHOUS.SEEK(m.cWareCode)
      m.cOutAddr1 = WAREHOUS.cAddress1
      m.cOutAddr2 = WAREHOUS.cAddress2
      m.cOutAddr3 = WAREHOUS.cAddress3
      m.cOutAddr4 = WAREHOUS.cAddress4
      m.cOutAddr5 = WAREHOUS.cAddress5
      m.ShpName   = WAREHOUS.cDesc
      m.Available = COMPLETE
      m.Open      = nStyOrder
      m.cinvtype  = '0001'
      m.cstytype  = "U"
      m.cbusdocu  = "P"
      m.Contact   = loFormSet.lcCont
      m.Phone     = loFormSet.lcPhone
      m.POTotal   = nICost1+nICost2+nICost3+nICost4+nICost5+nICost6+nICost7
      m.cAdd_User = oAriaApplication.User_ID
      m.cAdd_Time = TIME()
      m.dAdd_Date = oAriaApplication.systemdate
      m.Link_Code = "DEFDEF"
      m.cPriceCur = oAriaApplication.BaseCurrency
      m.cDutyCur  =oAriaApplication.BaseCurrency
      m.nPriceRat = 1
      m.nCurrUnit = 1
      m.nDutyRat  = 1
      m.nDCurUnit = 1
      =RLOCK()
      REPLACE nSteps WITH 1
      UNLOCK
      loFormSet.loPosHdr.INSERT("FROM MEMVAR")
    ELSE
      IF EVALUATE(loFormSet.lcPOH+'.nSteps') < 1
        SELECT POSHDR
        =RLOCK()
        loFormSet.loPosHdr.REPLACE("FLAG   WITH '' ,TotOrd WITH TotOrd + EVALUATE(loFormSet.lcPOH+'.TotOrd')")
        UNLOCK
        SELECT (loFormSet.lcPOH)
        =RLOCK()
        REPLACE nSteps WITH 1
        UNLOCK
      ENDIF
    ENDIF

    *-- Update master tables.
    DECLARE laTableUpdate[7]
    laTableUpdate[1] = loStyDye
    laTableUpdate[2] = loFormSet.loStyle
    laTableUpdate[3] = loFormSet.loPOSLN
    laTableUpdate[4] = loFormSet.loOrdHdr
    laTableUpdate[5] = loFormSet.loOrdLine
    laTableUpdate[6] = loFormSet.loPosHdr
    laTableUpdate[7] = loFormSet.loCUTPICK

    IF !lfTableUpdate()
      RETURN .F.
    ENDIF

    *--- Get the module seting for generating cost sheet
    *--- if this seting is alwayes ("A") we will call function to call
    *--- C/T,PO's Cost Sheet program to generate Cost Sheet.
    llDumy = llUpdBom .AND. loFormSet.ActiveMode="A" .AND. llGnCTBom .AND. lfGenCutBom(ALLTRIM(POSHDR.STYLE),POSHDR.PO,loFormSet)
  ENDSCAN

  loBom.DESTROY

  SELECT (loFormSet.lcPOH)
  REPLACE ALL PO WITH cTmpPo
  GO TOP

  *** Convert this function in Phase II.
  *!*************************************************************
  *! Name      : lfUpdCTP
  *! Developer : Wael ALy Mohamed
  *! Date      : 10/10/1998
  *! Purpose   : Update original files in case of generate CT from plan
  *!*************************************************************
  *! Calls     : gfModalGen(),gfSequence()
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfUpdCTP()
  *!*************************************************************
FUNCTION lfUpdCTP
  LPARAMETERS loFormSet
  PRIVATE lcPONumber,lcDivision,lcSeason

  IF loFormSet.llDyelot
    SELECT (loFormSet.lcPOLine)
    SCAN FOR EMPTY(Dyelot)
      *-- if the style is dyelot and the dyelot fields is empty
      *-- stop saving and give the user a message
      IF loFormSet.loStyle.SEEK(STYLE) AND (STYLE.cDye_FLg = 'Y')
        *-- You have to enter dyelot for the required dyelots records
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
        *=gfModalGen('INM38135B38018','DIALOG',LANG_MFGENCT_CTORDERS)
        =gfModalGen('INM38135B38018','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CTORDERS,loFormSet.GetHeaderText("LANG_MFGENCT_CTORDERS",loFormSet.HeaderAlias)))
        *N000682,1 11/20/2012 MMT Globlization changes[End]

        RETURN .F.
      ENDIF
    ENDSCAN
  ENDIF
  loFormSet.loPOSLN.SETORDER('POSLN')
  loFormSet.lcFirstCT  = SPACE(06)
  SELECT (loFormSet.lcPOH)
  *-- scan the tmp. Cuttkth for qtys > 0 to gfenerate auto. C/T numbers
  SCAN FOR nStyOrder > 0
    lcPo       = PO
    llUpdBom   = .T.
    lcStyle    = SUBSTR(STYLE,1,loFormSet.lnMajorLen)
    lcDivision = cDivision
    lcSeason   = SEASON
    lcYear     = cYear
    lcWeek     = cWeek
    lcStatus   = STATUS
    *B608122,1 TMI [Start] Call the gfSequence global function with passing the new parameters lcTranType,lcTable,lcTag
    *lcPONumber = IIF(EMPTY(PO),IIF(EMPTY(cTmpPo),gfSequence("CUTTKT","","",lcDivision),cTmpPo),PO)
    lcPONumber = IIF(EMPTY(PO),IIF(EMPTY(cTmpPo),gfSequence('CUTTKT','','',lcDivision,'','PU','POSHDR','POSHDR'),cTmpPo),PO)
    *B608122,1 TMI [End  ]
    =RLOCK()
    REPLACE cTmpPo WITH lcPONumber
    UNLOCK
    loFormSet.lcFirstCT = IIF(EMPTY(loFormSet.lcFirstCT),lcPONumber,loFormSet.lcFirstCT)
    loFormSet.lcLastCt  = lcPONumber

    SELECT (loFormSet.lcPOLine)
    =SEEK(lcPo+lcDivision+lcSeason+lcWeek+lcYear+lcStyle)
    SCAN REST WHILE PO+cDivision+SEASON+cWeek+cYear+STYLE+Dyelot = lcPo+lcDivision+lcSeason+lcWeek+lcYear+lcStyle FOR TotQty > 0
      IF !loFormSet.loPOSLN.SEEK('PU'+lcPONumber+'0001'+STYLE+STR(LINENO,6)+TRANCD)
        loFormSet.loStyle.SEEK(STYLE)

        SCATTER MEMVAR
        m.PO = lcPONumber
        m.nFCost1  = m.nFCost1 / m.TotQty
        m.nFCost2  = m.nFCost2 / m.TotQty
        m.nFCost3  = m.nFCost3 / m.TotQty
        m.nFCost4  = m.nFCost4 / m.TotQty
        m.nFCost5  = m.nFCost5 / m.TotQty
        m.nFCost6  = m.nFCost6 / m.TotQty
        m.nFCost7  = m.nFCost7 / m.TotQty
        m.nICost1  = m.nICost1 / m.TotQty
        m.nICost2  = m.nICost2 / m.TotQty
        m.nICost3  = m.nICost3 / m.TotQty
        m.nICost4  = m.nICost4 / m.TotQty
        m.nICost5  = m.nICost5 / m.TotQty
        m.nICost6  = m.nICost6 / m.TotQty
        m.nICost7  = m.nICost7 / m.TotQty
        IF !loStyDye.SEEK(m.STYLE+m.cWareCode+SPACE(10))
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
          *WAIT LANG_MFGENCT_ASSIGNSTYLE + ALLTRIM(m.STYLE) + " " + LANG_MFGENCT_TOWAREHOUSE + " " + ALLTRIM(m.cWareCode) WINDOW NOWAIT
          WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ASSIGNSTYLE,loFormSet.GetHeaderText("LANG_MFGENCT_ASSIGNSTYLE",loFormSet.HeaderAlias)) + ALLTRIM(m.STYLE) + " " + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_TOWAREHOUSE,loFormSet.GetHeaderText("LANG_MFGENCT_TOWAREHOUSE",loFormSet.HeaderAlias)) + " " + ALLTRIM(m.cWareCode) WINDOW NOWAIT
          *N000682,1 11/20/2012 MMT Globlization changes[End]

          DO gpAdStyWar WITH m.STYLE,SPACE(10),m.cWareCode
          WAIT CLEAR
        ENDIF
        IF !EMPTY(m.Dyelot) AND !loStyDye.SEEK(m.STYLE+m.cWareCode+m.Dyelot)
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
          *WAIT LANG_MFGENCT_ASSIGNSTYLE + ALLTRIM(m.STYLE) + " " + LANG_MFGENCT_TOWAREHOUSE + " " + ALLTRIM(m.cWareCode) WINDOW NOWAIT
          WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ASSIGNSTYLE,loFormSet.GetHeaderText("LANG_MFGENCT_ASSIGNSTYLE",loFormSet.HeaderAlias)) + ALLTRIM(m.STYLE) + " " + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_TOWAREHOUSE,loFormSet.GetHeaderText("LANG_MFGENCT_TOWAREHOUSE",loFormSet.HeaderAlias)) + " " + ALLTRIM(m.cWareCode) WINDOW NOWAIT
          *N000682,1 11/20/2012 MMT Globlization changes[End]

          DO gpAdStyWar WITH m.STYLE,m.Dyelot,m.cWareCode
          WAIT CLEAR
        ENDIF
        IF STYLE.lInvSty AND lcStatus <> 'B'
          =loStyDye.SEEK(m.STYLE+m.cWareCode+SPACE(10))
          SELECT STYDYE
          =RLOCK()
          loStyDye.REPLACE("WIP1   WITH WIP1   + m.Qty1  ,"+;
            "WIP2   WITH WIP2   + m.Qty2  ,"+;
            "WIP3   WITH WIP3   + m.Qty3  ,"+;
            "WIP4   WITH WIP4   + m.Qty4  ,"+;
            "WIP5   WITH WIP5   + m.Qty5  ,"+;
            "WIP6   WITH WIP6   + m.Qty6  ,"+;
            "WIP7   WITH WIP7   + m.Qty7  ,"+;
            "WIP8   WITH WIP8   + m.Qty8  ,"+;
            "TotWIP WITH TotWIP + m.TotQty,"+;
            "nWO1   WITH nWO1   + m.Qty1  ,"+;
            "nWO2   WITH nWO2   + m.Qty2  ,"+;
            "nWO3   WITH nWO3   + m.Qty3  ,"+;
            "nWO4   WITH nWO4   + m.Qty4  ,"+;
            "nWO5   WITH nWO5   + m.Qty5  ,"+;
            "nWO6   WITH nWO6   + m.Qty6  ,"+;
            "nWO7   WITH nWO7   + m.Qty7  ,"+;
            "nWO8   WITH nWO8   + m.Qty8  ,"+;
            "nTotWo WITH nTotWo + m.TotQty")
          UNLOCK
          IF !EMPTY(m.Dyelot)
            =loStyDye.SEEK(m.STYLE+m.cWareCode+m.Dyelot)
            SELECT STYDYE
            =RLOCK()
            loStyDye.REPLACE("WIP1   WITH WIP1   + m.Qty1  ,"+;
              "WIP2   WITH WIP2   + m.Qty2  ,"+;
              "WIP3   WITH WIP3   + m.Qty3  ,"+;
              "WIP4   WITH WIP4   + m.Qty4  ,"+;
              "WIP5   WITH WIP5   + m.Qty5  ,"+;
              "WIP6   WITH WIP6   + m.Qty6  ,"+;
              "WIP7   WITH WIP7   + m.Qty7  ,"+;
              "WIP8   WITH WIP8   + m.Qty8  ,"+;
              "TotWIP WITH TotWIP + m.TotQty,"+;
              "nWO1   WITH nWO1   + m.Qty1  ,"+;
              "nWO2   WITH nWO2   + m.Qty2  ,"+;
              "nWO3   WITH nWO3   + m.Qty3  ,"+;
              "nWO4   WITH nWO4   + m.Qty4  ,"+;
              "nWO5   WITH nWO5   + m.Qty5  ,"+;
              "nWO6   WITH nWO6   + m.Qty6  ,"+;
              "nWO7   WITH nWO7   + m.Qty7  ,"+;
              "nWO8   WITH nWO8   + m.Qty8  ,"+;
              "nTotWo WITH nTotWo + m.TotQty")
            UNLOCK
          ENDIF
          SELECT STYLE
          =RLOCK()
          loFormSet.loStyle.REPLACE("WIP1   WITH WIP1   + m.Qty1  ,"+;
            "WIP2   WITH WIP2   + m.Qty2  ,"+;
            "WIP3   WITH WIP3   + m.Qty3  ,"+;
            "WIP4   WITH WIP4   + m.Qty4  ,"+;
            "WIP5   WITH WIP5   + m.Qty5  ,"+;
            "WIP6   WITH WIP6   + m.Qty6  ,"+;
            "WIP7   WITH WIP7   + m.Qty7  ,"+;
            "WIP8   WITH WIP8   + m.Qty8  ,"+;
            "TotWIP WITH TotWIP + m.TotQty,"+;
            "nWO1   WITH nWO1   + m.Qty1  ,"+;
            "nWO2   WITH nWO2   + m.Qty2  ,"+;
            "nWO3   WITH nWO3   + m.Qty3  ,"+;
            "nWO4   WITH nWO4   + m.Qty4  ,"+;
            "nWO5   WITH nWO5   + m.Qty5  ,"+;
            "nWO6   WITH nWO6   + m.Qty6  ,"+;
            "nWO7   WITH nWO7   + m.Qty7  ,"+;
            "nWO8   WITH nWO8   + m.Qty8  ,"+;
            "nTotWo WITH nTotWo + m.TotQty")
          UNLOCK
        ENDIF
        m.cAdd_User = oAriaApplication.User_ID
        m.cAdd_Time = TIME()
        m.dAdd_Date = oAriaApplication.systemdate
        loFormSet.loPOSLN.INSERT('FROM MEMVAR')
      ENDIF
    ENDSCAN
    IF !loFormSet.loPosHdr.SEEK('PU'+lcPONumber)
      SELECT (loFormSet.lcPOH)
      SCATTER MEMVAR
      m.PO        = lcPONumber
      m.cMultiLot = 'S'
      IF !m.lMultiWare AND loFormSet.loWAREHOUS.SEEK(m.cWareCode)
        m.cOutAddr1 = WAREHOUS.cAddress1
        m.cOutAddr2 = WAREHOUS.cAddress2
        m.cOutAddr3 = WAREHOUS.cAddress3
        m.cOutAddr4 = WAREHOUS.cAddress4
        m.cOutAddr5 = WAREHOUS.cAddress5
        m.ShpName   = WAREHOUS.cDesc
      ENDIF
      m.Available = m.COMPLETE
      m.Open      = m.nStyOrder
      m.cinvtype  = '0001'
      m.cAdd_User = oAriaApplication.User_ID
      m.cAdd_Time = TIME()
      m.dAdd_Date = oAriaApplication.systemdate
      m.Link_Code = "DEFDEF"
      m.cPriceCur = oAriaApplication.BaseCurrency
      m.cDutyCur  =oAriaApplication.BaseCurrency
      m.nPriceRat = 1
      m.nCurrUnit = 1
      m.nDutyRat  = 1
      m.nDCurUnit = 1
      loFormSet.loPosHdr.INSERT("FROM MEMVAR")
    ENDIF

    *-- Update master tables.
    DECLARE laTableUpdate[4]
    laTableUpdate[1] = loStyDye
    laTableUpdate[2] = loFormSet.loStyle
    laTableUpdate[3] = loFormSet.loPOSLN
    laTableUpdate[4] = loFormSet.loPosHdr

    IF !lfTableUpdate()
      RETURN .F.
    ENDIF

  ENDSCAN

  SELECT (loFormSet.lcPOH)
  REPLACE ALL PO WITH cTmpPo
  GO TOP

  *!*************************************************************
  *! Name      : lfFabPrint
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/27/2004
  *! Purpose   : Print Fabric/Color order allocation information
  *!*************************************************************
  *! Calls     : gfOpGrid(),pSetup(),gfDispRep
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfFabPrint()
  *!*************************************************************
FUNCTION lfFabPrint
  LPARAMETERS loFormSet

  *loFormSet.AriaForm1.grdOrdLine.Column7.ControlSource = ""
  *loFormSet.AriaForm1.grdOrdLine.Column9.ControlSource = ""
  *loFormSet.AriaForm1.grdFabrics.Column6.ControlSource = ""
  loFormSet.AriaForm1.grdFabrics.RECORDSOURCE   = ""
  loFormSet.AriaForm1.grdOrdLine.RECORDSOURCE   = ""
  loFormSet.AriaForm1.grdOrdLine.CHILDORDER     = ""
  loFormSet.AriaForm1.grdOrdLine.RELATIONALEXPR = ""
  loFormSet.AriaForm1.grdOrdLine.LINKMASTER     = ""

  LOCAL lnAlias,lnMatMjrLen,lnMatClrStr,lnMatClrLen
  lnAlias   = SELECT(0)
  lcRPOrd   = 'B'
  lcSumDeta = 'D'
  lcTmp     = ''
  lcFabExpr = ''
  lcOrdExpr = ''
  lcFrxNam  = 'MFGENC1.FRX'
  lcExpr    = gfOpGrid('MFGENC1',.T.,.F.,.F.,.T.,.T.)

  lnMatMjrLen = LEN(gfItemMask("PM",'','0002'))

  DECLARE laMatSeg[1]
  =gfItemMask(@laMatSeg,'','0002')
  FOR lnCnt = 1 TO ALEN(laMatSeg,1)
    IF laMatSeg[lnCnt , 1] = "C"
      lnMatClrStr = laMatSeg[lnCnt , 4]
      lnMatClrLen = LEN(laMatSeg[lnCnt , 3])
    ENDIF
  ENDFOR

  *--If choose RUN from the option grid
  IF lcExpr <> '.F.'
    SELECT (loParentForm.lcFabrics)
    SET RELATION TO && Reset the orginal relation
    CREAT CURSOR lcTmp (cType C(1)) && A cursor that used in the .FRX
    APPEND BLANK
    REPLACE cType WITH 'F'
    SET RELATION TO IIF(cType = 'F' , '' , '*') INTO (loParentForm.lcFabrics)
    SET SKIP TO (loParentForm.lcFabrics)
    *-- If choose RUN with any filters
    IF lcExpr <> '.T.'
      *-- Pripare the filter expretion
      lcFabExpr = lcExpr
      lcOrdExpr = lcExpr
      lcFabExpr = STRTRAN(lcFabExpr,'FABRIC.FABRIC','SUBSTR('+loParentForm.lcFabrics+'.Fabric,1,'+STR(lnMatMjrLen,2,0)+')')
      lcFabExpr = STRTRAN(lcFabExpr,'FABRIC.COLOR' ,'SUBSTR('+loParentForm.lcFabrics+'.Fabric,'+STR(lnMatClrStr,2,0)+','+STR(lnMatClrLen,1,0)+')')
      lcOrdExpr = STRTRAN(lcOrdExpr,'FABRIC.FABRIC','SUBSTR('+loParentForm.lcTmpOrd +'.Fabric,1,'+STR(lnMatMjrLen,2,0)+')')
      lcOrdExpr = STRTRAN(lcOrdExpr,'FABRIC.COLOR' ,'SUBSTR('+loParentForm.lcTmpOrd +'.Fabric,'+STR(lnMatClrStr,2,0)+','+STR(lnMatClrLen,1,0)+')')

      *-- we will print the report in detail
      IF lcSumDeta = 'D'
        SELECT (loParentForm.lcTmpOrd)
        SET RELATION TO   && Reset the orginal relation

        *-- In case we well print ALLOCATED or UNALOCATED orders
        DO CASE
          CASE lcRPOrd = 'A'  && WE will print allocated orders only
            lcOrdExpr = lcOrdExpr + ' AND ' + EVALUATE('loParentForm.lcTmpOrd') + ".cSelect= '�' "
          CASE lcRPOrd = 'U'   && WE will print UNallocated orders only
            lcOrdExpr = lcOrdExpr + ' AND ' + EVALUATE('loParentForm.lcTmpOrd') + ".cSelect= ' ' "
        ENDCASE
        SELECT lcTmp
        APPEND BLANK
        REPLACE cType WITH 'O'
        SET RELATION TO IIF(cType = 'O' , '' , '*') INTO (loParentForm.lcTmpOrd) ADDITIVE
        SET SKIP TO (loParentForm.lcTmpOrd) , (loParentForm.lcFabrics)
      ENDIF && end of (IF lcSumDeta = 'D')

      lcExpr = '(EOF(loParentForm.lcTmpOrd) AND ' + lcFabExpr + ') OR (EOF(loParentForm.lcFabrics) AND ' + lcOrdExpr + ')'

      SELECT lcTmp
      IF pSetup(.T.)
        *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
        IF FILE(oAriaApplication.CLIENTREPORTHOME+lcFrxNam)
          REPORT FORM (oAriaApplication.CLIENTREPORTHOME+lcFrxNam) TO PRINTER NOCONSOLE NOEJECT FOR &lcExpr.
        ELSE
        *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
          REPORT FORM (oAriaApplication.ReportHome+lcFrxNam) TO PRINTER NOCONSOLE NOEJECT FOR &lcExpr.
        *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
        ENDIF
        *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
      ENDIF
    ELSE  && We choose RUN without any filters
      *-- we will print the report in detail
      IF lcSumDeta = 'D'
        lcOrdExpr = '.T.'
        SELECT (loParentForm.lcTmpOrd)
        SET RELATION TO   && Reset the orginal relation

        *-- In case we well print ALLOCATED or UNALOCATED orders
        DO CASE
          CASE lcRPOrd = 'A'  && WE will print allocated orders only
            lcOrdExpr = EVALUATE('loParentForm.lcTmpOrd') + ".cSelect= '�' "
          CASE lcRPOrd = 'U'   && WE will print UNallocated orders only
            lcOrdExpr = EVALUATE('loParentForm.lcTmpOrd') + ".cSelect= ' ' "
        ENDCASE

        SELECT lcTmp
        APPEND BLANK
        REPLACE cType WITH 'O'
        SET RELATION TO IIF(cType = 'O' , '' , '*') INTO (loParentForm.lcTmpOrd) ADDITIVE
        SET SKIP TO (loParentForm.lcTmpOrd) , (loParentForm.lcFabrics)

        lcExpr = '(EOF(loParentForm.lcTmpOrd)) OR (EOF(loParentForm.lcFabrics) AND ' + lcOrdExpr + ')'
      ENDIF && end of IF lcSumDeta = 'D'

      IF pSetup(.T.)
        *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
        IF FILE(oAriaApplication.CLIENTREPORTHOME+lcFrxNam)
          REPORT FORM (oAriaApplication.CLIENTREPORTHOME+lcFrxNam) TO PRINTER NOCONSOLE NOEJECT FOR &lcExpr.
        ELSE
        *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
          REPORT FORM (oAriaApplication.ReportHome+lcFrxNam) TO PRINTER NOCONSOLE NOEJECT FOR &lcExpr.
        *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
        ENDIF
        *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
      ENDIF
    ENDIF && end of (IF lcExpr <> '.T.')
  ENDIF && end of (IF lcExpr <> '.F.')

  *-- Close the cursor and restore the old relations
  *-- in case of reset the relation [start]
  IF USED(lcTmp)
    SET RELATION TO
    USE IN lcTmp
    SELECT (loParentForm.lcFabrics)
    lcRelExpr = loParentForm.lcFabrics+'.Fabric'+IIF(EMPTY(loParentForm.lcTmpOrd+'.cFabWare'),'','+'+loParentForm.lcFabrics+'.CWARECODE')
    SET RELATION TO &lcRelExpr. INTO (loParentForm.lcTmpOrd)
  ENDIF

  *loFormSet.AriaForm1.grdFabrics.Column6.ControlSource = "lfBalance('')"
  WITH loFormSet.AriaForm1
    WITH .grdFabrics
      .RECORDSOURCE             = loParentForm.lcFabrics
      .Column1.CONTROLSOURCE    = loParentForm.lcFabrics+'.Fabric'
      .Column2.CONTROLSOURCE    = loParentForm.lcFabrics+'.CWARECODE'
      .Column3.CONTROLSOURCE    = loParentForm.lcFabrics+'.DYELOT'
      .Column4.CONTROLSOURCE    = loParentForm.lcFabrics+'.ONHAND'
      .Column5.CONTROLSOURCE    = loParentForm.lcFabrics+'.nAllocated'
      .Column6.CONTROLSOURCE    = "lfBalance('')"
      .Column2.VISIBLE          = loParentForm.llWareHous
      .Column3.VISIBLE          = loParentForm.llDyelot
    ENDWITH

    SELECT(loParentForm.lcTmpOrd)
    WITH .grdOrdLine
      .RECORDSOURCE             = loParentForm.lcTmpOrd
      .CHILDORDER               = "Fabrics"
      .RELATIONALEXPR           = loParentForm.lcFabrics+'.Fabric'+IIF(EMPTY(loParentForm.lcTmpOrd+'.cFabWare'),'','+'+loParentForm.lcFabrics+'.CWARECODE')
      .LINKMASTER               = loParentForm.lcFabrics
      .Column1.CONTROLSOURCE    = loParentForm.lcTmpOrd+'.lSelect'
      .Column2.CONTROLSOURCE    = loParentForm.lcTmpOrd+'.Order'
      .Column3.CONTROLSOURCE    = loParentForm.lcTmpOrd+'.Account'
      .Column4.CONTROLSOURCE    = loParentForm.lcTmpOrd+'.StName'
      .Column5.CONTROLSOURCE    = loParentForm.lcTmpOrd+'.Store'
      .Column6.CONTROLSOURCE    = loParentForm.lcTmpOrd+'.Style'
      .Column7.CONTROLSOURCE    = 'lfGetPcs(loParentForm)'
      .Column8.CONTROLSOURCE    = loParentForm.lcTmpOrd+'.nRequired'
      .Column9.CONTROLSOURCE    = "lfUnAllo('')"

      IF loParentForm.llRPUnAWip
        FOR lnI = 4 TO 9
          lcI = STR(lnI,1)
          .COLUMN&lcI..COLUMNORDER = lnI - 1
        ENDFOR
      ELSE
        .Column9.VISIBLE = .F.
      ENDIF
    ENDWITH
  ENDWITH

  *SELECT(loParentForm.lcTmpOrd)
  *loFormSet.AriaForm1.grdOrdLine.Column7.ControlSource = 'lfGetPcs(loParentForm)'
  *loFormSet.AriaForm1.grdOrdLine.Column9.ControlSource = "lfUnAllo('')"
  SELECT (lnAlias)

  *!*************************************************************
  *! Name      : lfvSelFabr
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/29/2004
  *! Purpose   : Validate Fabrics
  *!*************************************************************
  *! Calls     : FABROW()
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfvSelFabr()
  *!*************************************************************
FUNCTION lfvSelFabr

  LOCAL lcObjNam,lcFabric,loFabric
  lcObjNam = OGSYS18()
  lcFabric = EVALUATE(lcObjNam)
  loFabric = CREATEOBJECT('RemoteTable','ITEM','CSTYLE','FABRIC',SET("Datasession"))
  IF !EMPTY(lcFabric) .AND. !loFabric.SEEK("0002"+PADR(lcFabric,19))
    LOCAL lcMajor,lcBrowChr
    lcMajor   = RTRIM(lcFabric)
    lcBrowChr = RIGHT(lcMajor,1)
    lcMajor   = IIF(lcBrowChr=='?',SUBSTR(lcMajor,1,LEN(lcMajor)-1),lcMajor)
    lcFabric  = gfItemBrow("0002",PADR(lcMajor,19),"","*","M","",.T.)
    lcFabric  = PADR(SUBSTR(lcFabric,1,LEN(gfItemMask("PM","","0002"))),19)
  ENDIF
  loOgScroll.&lcObjNam. = lcFabric

  *!*************************************************************
  *! Name      : lfPrnSumm
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/12/2004
  *! Purpose   : Print Allocation Process Summary Report
  *!*************************************************************
  *! Calls     : pSetup(),gfDispRep
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfPrnSumm()
  *!*************************************************************
FUNCTION lfPrnSumm
  LPARAMETERS loFormSet

  LOCAL lnAlias,loFabric,loFabDye
  lnAlias = SELECT(0)
  loFormSet.loCUTPICK.SETORDER('Cutord')

  loFabric = CREATEOBJECT('RemoteTable','ITEM','STYLE','FABRIC',loFormSet.DATASESSIONID)
  loFabDye = CREATEOBJECT('RemoteTable','ITEMLOC','STYDYE','FABDYE',loFormSet.DATASESSIONID)

  DIMENSION laFileStru[14, 18]
  lnFileStru = 1
  laFileStru[lnFileStru ,1] = 'cPrntOrd'
  laFileStru[lnFileStru ,2] = 'C'
  laFileStru[lnFileStru ,3] = 1
  laFileStru[lnFileStru ,4] = 0
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'Fabric'
  laFileStru[lnFileStru ,2] = 'C'
  laFileStru[lnFileStru ,3] = 19
  laFileStru[lnFileStru ,4] = 0
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'cFabWare'
  laFileStru[lnFileStru ,2] = 'C'
  laFileStru[lnFileStru ,3] = 6
  laFileStru[lnFileStru ,4] = 0
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'Dyelot'
  laFileStru[lnFileStru ,2] = 'C'
  laFileStru[lnFileStru ,3] = 10
  laFileStru[lnFileStru ,4] = 0
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'ORDER'
  laFileStru[lnFileStru ,2] = 'C'
  laFileStru[lnFileStru ,3] = 6
  laFileStru[lnFileStru ,4] = 0
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'CutTKt'
  laFileStru[lnFileStru ,2] = 'C'
  laFileStru[lnFileStru ,3] = 6
  laFileStru[lnFileStru ,4] = 0
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'TotQty'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 7
  laFileStru[lnFileStru ,4] = 0
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'nRequired'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 12
  laFileStru[lnFileStru ,4] = 3
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'Account'
  laFileStru[lnFileStru ,2] = 'C'
  laFileStru[lnFileStru ,3] = 5
  laFileStru[lnFileStru ,4] = 0
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'STYLE'
  laFileStru[lnFileStru ,2] = 'C'
  laFileStru[lnFileStru ,3] = 19
  laFileStru[lnFileStru ,4] = 0
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'STORE'
  laFileStru[lnFileStru ,2] = 'C'
  laFileStru[lnFileStru ,3] = 8
  laFileStru[lnFileStru ,4] = 0
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'Desc'
  laFileStru[lnFileStru ,2] = 'C'
  laFileStru[lnFileStru ,3] = 20
  laFileStru[lnFileStru ,4] = 0
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'OnHand'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 13
  laFileStru[lnFileStru ,4] = 3
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'BtName'
  laFileStru[lnFileStru ,2] = 'C'
  laFileStru[lnFileStru ,3] = 30
  laFileStru[lnFileStru ,4] = 0

  FOR lnI = 7 TO 16
    FOR lnJ = 1 TO 14
      laFileStru[lnJ,lnI] = ''
    ENDFOR
  ENDFOR
  FOR lnJ = 1 TO 14
    STORE 0 TO laFileStru[lnJ,17],laFileStru[lnJ,18]
  ENDFOR

  =gfCrtTmp(loFormSet.lcPrintAll,@laFileStru,'cPrntOrd+FABRIC+cFabWare+Dyelot+Account+STYLE+ORDER+STORE',loFormSet.lcPrintAll)

  SELECT (loFormSet.lcTmpOrd)
  SET ORDER TO TAG 'TICKET'
  =SEEK('�')
  SCAN REST WHILE cSelect = '�'
    SCATTER MEMVAR
    SELECT CUTPICK
    =loFormSet.loCUTPICK.SEEK(IIF(loFormSet.lcChoice='C','1','2')+EVALUATE(loFormSet.lcTmpOrd+'.order')+STR(EVALUATE(loFormSet.lcTmpOrd+'.LineNo'),6))
    SCAN REST WHILE TRANCD+ORDER+cOrdLine = IIF(loFormSet.lcChoice='C','1','2')+EVALUATE(loFormSet.lcTmpOrd+'.order')+STR(EVALUATE(loFormSet.lcTmpOrd+'.LineNo'),6) ;
        FOR SEEK(cTKtNo,loFormSet.lcCutPick)
      loFabric.SEEK('0002'+m.Fabric)
      loFabDye.SEEK('0002'+m.Fabric+m.cFabWare+m.Dyelot)
      loFormSet.loCustomer.SEEK('M'+m.ACCOUNT)
      INSERT INTO (loFormSet.lcPrintAll);
        (cPrntOrd,Fabric,cFabWare,Dyelot,ORDER,CutTKt,TotQty,nRequired,ACCOUNT,STYLE,STORE,DESC,OnHand,BtName) VALUES ;
        ('0',m.Fabric,m.cFabWare,m.Dyelot,m.Order,CUTPICK.cTKtNo,CUTPICK.TotQty,CUTPICK.TotQty*m.nYeild,m.ACCOUNT,m.STYLE,;
        m.STORE,Fabric.DESC,FABDYE.TotStk,CUSTOMER.BtName)
    ENDSCAN
  ENDSCAN
  =SEEK(' ')
  SCAN REST WHILE cSelect = ' '
    SCATTER MEMVAR
    m.TotQty=lfGetPcs(loFormSet)
    loFormSet.loCustomer.SEEK('M'+m.ACCOUNT)
    INSERT INTO (loFormSet.lcPrintAll);
      (cPrntOrd,ORDER,TotQty,nRequired,ACCOUNT,STYLE,STORE,BtName) VALUES ;
      ('1',m.Order,m.TotQty,m.TotQty*m.nYeild,m.ACCOUNT,m.STYLE,m.Store,CUSTOMER.BtName)
  ENDSCAN
  SELECT (loFormSet.lcPrintAll)
  GO TOP
  IF pSetup(.T.)
    lcFrxNam = 'MFGENC.FRX'
    PRIVATE llWareHous,lcChoice
    llWareHous = loFormSet.llWareHous
    lcChoice   = loFormSet.lcChoice
    DO gfDispRep WITH (lcFrxNam)
  ENDIF
  USE IN (loFormSet.lcPrintAll)
  SELECT (lnAlias)

  *!*************************************************************
  *! Name             : lfChngIndx
  *! Developer        : AHMED MAHER (AMH)
  *! Date             : 11/30/2004
  *! Purpose          : Change and restore the index of the style file
  *!                    for browse in range
  *!*************************************************************
  *! Calls     : None
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfChngIndx()
  *!*************************************************************
FUNCTION lfChngIndx
  LPARAMETERS lcNdxFlag

  LOCAL lcOldAlis
  *-- When entering the in range browse
  IF lcNdxFlag = 'S'
    lcOldAlis = ALIAS()                      && Save the old alias
    SELECT STYLE
    loParentForm.lcOldNdx = ORDER()          && Save the old order
    SET ORDER TO TAG Cstyle
    SELECT (lcOldAlis)                       && Restore the old alias
  ELSE && IF getting out from the in range browse
    lcOldAlis = ALIAS()                      && Save the old alias
    SELECT STYLE
    SET ORDER TO TAG (loParentForm.lcOldNdx) && Restore the old order
    SELECT (lcOldAlis)                       && Restore the old alias
  ENDIF && End of IF lcNdxFlag = 'S'

  *!*************************************************************
  *! Name             : lfvRepForm
  *! Developer        : AHMED MAHER (AMH)
  *! Date             : 12/29/2004
  *! Purpose          : To disable the selection in order type in the
  *!                    option grid
  *!*************************************************************
  *! Calls     : None
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfvRepForm()
  *!*************************************************************
FUNCTION lfvRepForm

  CLEARREAD()

  *!*************************************************************
  *! Name             : lfvBroWCt
  *! Developer        : AHMED MAHER - (AMH)
  *! Date             : 12/12/2004
  *! Purpose          : CutTkt Valid Fuunction
  *!*************************************************************
  *! Calls     : None
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : = lfvBroWCt()
  *!*************************************************************
FUNCTION lfvBroWCt
  LPARAMETERS loFormSet

  LOCAL lnOldAls , lcNewCutNo , llRet , lcStrToAdd , lcHdrVal
  lnOldAls = SELECT(0)

  IF !loFormSet.llGManualy
    RETURN
  ENDIF

  *: B608553,1 MMT 05/13/2008 Fix bug of Po no already exist message  [Start]
  IF loFormSet.ActiveMode <> 'A'
    RETURN
  ENDIF
  *: B608553,1 MMT 05/13/2008 Fix bug of Po no already exist message  [End]

  *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
  *IF loFormSet.lcChoice = 'P'
  IF INLIST(loFormSet.lcChoice ,'P','O')
  *N000606,1 NNA (END)

    lcHdrVal   = PO+cDivision+cPurCode+cStyGrade
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcStrToAdd = LANG_MFGENCT_PO
lcStrToAdd = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_PO,loFormSet.GetHeaderText("LANG_MFGENCT_PO",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  ELSE
    lcHdrVal   = PO+cDivision+SEASON+SUBSTR(STYLE,1,loFormSet.lnMajorLen)
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcStrToAdd = LANG_MFGENCT_CT
    lcStrToAdd = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CT,loFormSet.GetHeaderText("LANG_MFGENCT_CT",loFormSet.HeaderAlias))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

  ENDIF
  lcNewCutNo = IIF(!EMPTY(PO),'PP'+PO,PO)
  llRet = .F.
  IF EMPTY(lcNewCutNo)
    lnNoThing = gfModalGen('TRM38047B00000','DIALOG',lcStrToAdd)
    REPLACE PO WITH loFormSet.lcOldCtkNo
  ELSE
    IF LEN(ALLTRIM(lcNewCutNo))-2 < 6
      lnNoThing = gfModalGen('TRM38048B00000','DIALOG',lcStrToAdd)
      REPLACE PO WITH loFormSet.lcOldCtkNo
    ELSE
      IF loFormSet.loPosHdr.SEEK(lcNewCutNo) .OR. SEEK(SUBSTR(lcNewCutNo,3),loFormSet.lcLinFile)
        lnNoThing = gfModalGen('TRM38049B00000','DIALOG',lcStrToAdd)
        lcNewCutNo  = ''
        SELECT (loFormSet.lcHdrFile)
        REPLACE PO WITH loFormSet.lcOldCtkNo
      ELSE
        llRet = .T.
      ENDIF
    ENDIF
  ENDIF
  IF llRet
    SELECT (loFormSet.lcLinFile)
    IF !SEEK(lcHdrVal)
      =SEEK(loFormSet.lcOldCtkNo+SUBSTR(lcHdrVal,7))

      *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
      *IF loFormSet.lcChoice = 'P'
      IF INLIST(loFormSet.lcChoice ,'P','O')
      *N000606,1 NNA (END)

        REPLACE PO WITH SUBSTR(lcNewCutNo,3) FOR PO+cDivision+cPurCode+cStyGrade+STYLE+Dyelot = loFormSet.lcOldCtkNo+SUBSTR(lcHdrVal,7)
      ELSE
        REPLACE PO WITH SUBSTR(lcNewCutNo,3) FOR PO+cDivision+SEASON+STYLE+Dyelot = loFormSet.lcOldCtkNo+SUBSTR(lcHdrVal,7)
      ENDIF
    ENDIF
    *--- cTKtNo
    *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
    *IF loFormSet.lcChoice = 'L'
    IF loFormSet.lcChoice $ 'LO'
    *N000606,1 NNA (End)
      SELECT (loFormSet.lcHdrFile)
    ELSE
      SELECT (loFormSet.lcCutPick)
    ENDIF
    lcPoCtNo  = SUBSTR(lcNewCutNo,3)
    lcPoCtExp = IIF(loFormSet.lcChoice = 'P',;
      'CTKTNO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+CWARECODE+DYELOT',;
      'CTKTNO+CDIVISION+SEASON+STYLE+CWARECODE+DYELOT')
    IF !SEEK(lcPoCtNo)
      =SEEK(loFormSet.lcOldCtkNo+SUBSTR(lcHdrVal,7))
      REPLACE cTKtNo  WITH lcPoCtNo FOR &lcPoCtExp = loFormSet.lcOldCtkNo+SUBSTR(lcHdrVal,7)
    ENDIF
  ENDIF
  SELECT(lnOldAls)

  *!*************************************************************
  *! Name             : lfwOldCtkt
  *! Developer        : AHMED MAHER (AMH)
  *! Date             : 12/12/2004
  *! Purpose          : Get old no.
  *!*************************************************************
  *! Calls     : None
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : = lfwOldCtkt()
  *!*************************************************************
FUNCTION lfwOldCtkt
  LPARAMETERS loFormSet

  loFormSet.lcOldCtkNo = PO
  RETURN loFormSet.llGManualy

  *!*************************************************************
  *! Name             : lfCheckMaj
  *! Developer        : AHMED MAHER - (AMH)
  *! Date             : 11/25/2004
  *! Purpose          : Check style code structure.
  *!*************************************************************
  *! Calls     : None
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : = lfCheckMaj()
  *!*************************************************************
FUNCTION lfCheckMaj
  LPARAMETERS loFormSet

  LOCAL lnAlias,loIcIstru,llRet
  lnAlias = SELECT(0)
  loIcIstru = CREATEOBJECT('RemoteTable','ICISTRU','Segno',.F.,loFormSet.DATASESSIONID)
  llRet = .F.

  IF TYPE('loIcIstru')='O'
    llRet = loIcIstru.SEEK('U1')
    loIcIstru = .NULL.
  ENDIF

  SELECT (lnAlias)

  *!*************************************************************
  *! Name             : lfGenCutBom
  *! Developer        : AHMED MAHER (AMH)
  *! Date             : 12/12/2004
  *! Purpose          : Generate Cost Sheet Automaticaly.
  *!*************************************************************
  *! Calls     : None
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : = lfGenCutBom()
  *!*************************************************************
FUNCTION lfGenCutBom
  LPARAMETERS lcCtktSty,lcGenCTkt,loFormSet

  LOCAL lnOldAls
  IF loFormSet.lcChoice <> 'L'
    lnOldAls = SELECT(0)
    IF loBom.SEEK(lcCtktSty)

      *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
      *IF loFormSet.lcChoice = 'P'
      IF INLIST(loFormSet.lcChoice ,'P','O')
      *N000606,1 NNA (End)
        lcParameter = "'" + lcGenCTkt + "',.T."

        *: B608612,1 MHM 07/13/2008 Modify the generate PO from sales order program to call the PO cost
        *--                         sheet program using the DO Fox  [Start]
        *=oAriaApplication.DoProgram('AWRPOCSSH',lcParameter,.F.,'PO')
        *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
        IF FILE(oAriaApplication.CLIENTAPPLICATIONHOME+"PO\POCSSH.FXP")
          DO (oAriaApplication.CLIENTAPPLICATIONHOME+"PO\POCSSH.FXP") WITH lcGenCTkt ,.T.
        ELSE
        *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
          DO (oAriaApplication.ApplicationHome+"PO\POCSSH.FXP") WITH lcGenCTkt ,.T.
        *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
        ENDIF
        *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
        *: B608612,1 MHM 07/13/2008 Modify the generate PO from sales order program to call the PO cost
        *--                         sheet program using the DO Fox  [End]

      ELSE
        lcParameter = "'M','" + lcGenCTkt + "'"+",.T."
        *: B608612,1 MHM 07/13/2008 Modify the generate PO from sales order program to call the PO cost
        *--                         sheet program using the DO Fox  [Start]
        *=oAriaApplication.DoProgram('AWRMFCSSH',lcParameter,.F.,'MF')
        *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
        IF  FILE(oAriaApplication.CLIENTAPPLICATIONHOME+"MF\MFCSSH.FXP")
          DO (oAriaApplication.CLIENTAPPLICATIONHOME+"MF\MFCSSH.FXP") WITH lcGenCTkt ,.T.
        ELSE
        *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
          DO (oAriaApplication.ApplicationHome+"MF\MFCSSH.FXP") WITH lcGenCTkt ,.T.
        *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
        ENDIF
        *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
        *: B608612,1 MHM 07/13/2008 Modify the generate PO from sales order program to call the PO cost
        *--                         sheet program using the DO Fox  [End]

      ENDIF
    ELSE
      *-- XXXXXXXXXX cost sheet not found for XXXXXXXXXX : 'XXXXXXXXXX'.
      *-- Cannot proceed with the cutting ticket cost sheet.
      *-- < Ok >
      lcStr      = PROPER(ALLTRIM(loFormSet.lcMjrTtl)) + "|" +;
        PROPER(ALLTRIM(loFormSet.lcMjrTtl)) + "|" +;
        ALLTRIM(lcGenCTkt)
      = gfModalGen("TRM38095B00000","DIALOG",lcStr)
    ENDIF
    SELECT(lnOldAls)
  ENDIF

  *!*************************************************************
  *! Name             : lfGetOrd
  *! Developer        : AHMED MAHER (AMH)
  *! Date             : 11/30/2004
  *! Purpose          : Get the first Order in the selected criteria
  *!*************************************************************
  *! Calls     : None
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : = lfGetOrd()
  *!*************************************************************
FUNCTION lfGetOrd
  LOCAL lnInd
  STORE ""  TO loParentForm.lcOrdAlias
  STORE .F. TO loParentForm.llOrdRange

  lnInd = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'ORDHDR.ORDER'),1)
  IF lnInd <> 0
    loParentForm.lcOrdAlias = laOgFxFlt[lnInd,6]
    loParentForm.llOrdRange = (!EMPTY(loParentForm.lcOrdAlias) .AND. USED(loParentForm.lcOrdAlias) .AND. RECCOUNT(loParentForm.lcOrdAlias)>0)
  ENDIF

  *!*************************************************************
  *! Name             : lfGetSty
  *! Developer        : Wael Ali MOhamed
  *! Date             : 06/30/2006
  *! Purpose          : Get selected style range or style group
  *!*************************************************************
  *! Calls     : None
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : = lfGetSty()
  *!*************************************************************
FUNCTION lfGetSty

  STORE ""  TO loParentForm.lcOrdAlias, loParentForm.lcStyGroup
  STORE .F. TO loParentForm.llOrdRange

  lnPosition = ASCAN(laOgVrFlt,'STYLE.CSTYMAJOR')
  IF lnPosition > 0
    lnInd = ASUBSCRIPT(laOgVrFlt,lnPosition,1)
    IF lnInd <> 0
      loParentForm.lcOrdAlias = laOgVrFlt[lnInd,6]
      loParentForm.llOrdRange = (!EMPTY(loParentForm.lcOrdAlias) .AND. USED(loParentForm.lcOrdAlias) .AND. RECCOUNT(loParentForm.lcOrdAlias)>0)
    ENDIF
  ENDIF
  lnPosition = ASCAN(laOgVrFlt,'STYLE.CSTYGROUP')
  IF lnPosition > 0
    lnInd = ASUBSCRIPT(laOgVrFlt,lnPosition,1)
    IF lnInd <> 0
      loParentForm.lcStyGroup = laOgVrFlt[lnInd,6]
    ENDIF
  ENDIF
  lnPosition = ASCAN(laOgVrFlt,'WAREHOUS.CWARECODE')
  IF lnPosition > 0
    lnInd = ASUBSCRIPT(laOgVrFlt,lnPosition,1)
    IF lnInd <> 0
      loParentForm.lcWarehouse = laOgVrFlt[lnInd,6]
    ENDIF
  ENDIF
  loParentForm.lcRpCtStat = lcRpCtStat
  loParentForm.lcRpBaseOn = lcRpBaseOn
  loParentForm.ldRpFrDate = ldRpFrDate

  *!*************************************************************
  *! Name             : lfRetToSel
  *! Developer        : AHMED MAHER (AMH)
  *! Date             : 11/29/2004
  *! Purpose          : Allow user to select anoter order or group
  *!                  : group of orders to generate PO without go to OG
  *!*************************************************************
  *! Calls     : None
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
  *! Example   : =lfRetToSel()
  *!*************************************************************
FUNCTION lfRetToSel
  LPARAMETERS loFormSet

  LOCAL lcBrowFile,lcForExpr
  *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
  *lcBrowFile = IIF(loFormSet.lcChoice='L',loFormSet.lcStyTmp,loFormSet.lcOrdLine)
  lcBrowFile = IIF(loFormSet.lcChoice $ 'LO',loFormSet.lcStyTmp,loFormSet.lcOrdLine)
  *N000606,1 NNA (End)
  *-- Delete the previous selected order or orders
  SELECT (lcBrowFile)
  SET ORDER TO TAG TICKET
  IF loFormSet.lcChoice='L'
    lcForExpr = 'CSELECT+CDIVISION+SEASON+STYLE'
  ELSE
    *N000606,1 NNA 10/06/2007 (Begin) In case of Generate PO from OTs then Set order to a different Tag
    IF loFormSet.lcChoice='O' THEN
       lcForExpr = 'CSELECT+CDIVISION+CPURCODE+CSTYGRADE+STYLE'
    ELSE
    *N000606,1 NNA (End)
       lcForExpr = IIF(loFormSet.lcChoice='C','CSELECT+CDIVISION+SEASON+STYLE+DYELOT',;
         'CSELECT+CDIVISION+CPURCODE+CSTYGRADE+STYLE+DYELOT')
    *N000606,1 NNA (Begin)
    ENDIF
    *N000606,1 NNA (End)
  ENDIF
  =SEEK("�")
  SCAN REST FOR &lcForExpr. = "�"
    BLANK
    DELETE
  ENDSCAN

  *N000606,1 NNA 10/06/2007 (Begin) In case of Generate PO from OTs then Set order to a different Tag
  IF loFormSet.lcChoice<>'O' THEN
  *N000606,1 NNA (End)

    SET ORDER TO TAG (loFormSet.lcOrdLine)

  *N000606,1 NNA (Begin)
  ENDIF
  *N000606,1 NNA (End)

  GOTO TOP
  *-- IF the temp order line not empty allow user to select anoter
  *-- order or group of orders without go to OG
  IF !EOF()
    loFormSet.llRetToSel = .T.
  ENDIF

  *-- Delete the saved purchase order or orders
  SELECT (loFormSet.lcHdrFile)
  DELETE ALL

  *-- To delete the records that found in the PO Line (Temporary file)
  IF USED(loFormSet.lcPOLine)
    SELECT(loFormSet.lcPOLine)
    DELETE ALL
  ENDIF

  loFormSet.lcFirstCT = ""
  loFormSet.lcLastCt  = ""

  *-- End of lfRetToSel

  *!*************************************************************
  *! Name      : lfSROrder
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 11/30/2004
  *! Purpose   : Order In Range
  *!*************************************************************
FUNCTION lfSROrder
  LPARAMETERS lcParm

  LOCAL lnDataSessionID,lcCustRel,lcTmpPath
  DO CASE
    CASE lcParm = 'S'

      lnRngAlias = SELECT(0)
      lnDataSessionID = SET("Datasession")

      SET DATASESSION TO loParentForm.DATASESSIONID
      SELECT (loParentForm.lcTmpOrder)
      lcTmpPath = DBF(loParentForm.lcTmpOrder)
      USE IN (loParentForm.lcTmpOrder)

      SET DATASESSION TO lnDataSessionID
      USE (lcTmpPath) IN 0 ALIAS (loParentForm.lcTmpOrder) ORDER (loParentForm.lcTmpOrder)
      SELECT (loParentForm.lcTmpOrder)
      lcCustRel = [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)]

      SET ORDER TO CUSTOMER IN CUSTOMER
      SET RELATION TO &lcCustRel. INTO CUSTOMER && To customer file.
      GO TOP
      *! B609359,1 MMT 07/22/2010 Make Order Browser Browse from ORdhdr not from Tmp. File[Start]
      SET ORDER TO TAG ORDHDR IN ORDHDR
      *! B609359,1 MMT 07/22/2010 Make Order Browser Browse from ORdhdr not from Tmp. File[End]
    CASE lcParm = 'R'
      SELECT (loParentForm.lcTmpOrder)
      SET RELATION OFF INTO CUSTOMER && To customer file.
      lnDataSessionID = SET("Datasession")
      lcTmpPath = DBF(loParentForm.lcTmpOrder)
      USE IN (loParentForm.lcTmpOrder)

      SET DATASESSION TO loParentForm.DATASESSIONID
      USE (lcTmpPath) IN 0 ALIAS (loParentForm.lcTmpOrder) ORDER (loParentForm.lcTmpOrder)

      SET DATASESSION TO lnDataSessionID
      llClrOrd = .F.
      SELECT (lnRngAlias)

  ENDCASE
  *-- end of lfSROrder.

  *!*************************************************************
  *! Name      : lfGetOrder
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 11/25/2004
  *! Purpose   : Function to fill the temp file of unalocated orders.
  *!*************************************************************
FUNCTION lfGetOrder
  LPARAMETERS loFormSet
  loFormSet.loOrdHdr = CREATEOBJECT('RemoteTable','ORDHDR','ORDHDR','ORDHDR',loFormSet.DATASESSIONID)

  IF TYPE('loFormSet.loOrdHdr')#'O'
    RETURN .F.
  ENDIF

  SELECT ORDHDR
  =AFIELDS(laFldStru)
  =gfCrtTmp(loFormSet.lcTmpOrder,@laFldStru,"CORDTYPE+ORDER",loFormSet.lcTmpOrder)
  SELECT ORDHDR
  loFormSet.loOrdHdr.SEEK('O')
  SCAN REST WHILE CORDTYPE+ORDER = 'O' FOR !(ORDHDR.STATUS $ "XCB")
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *WAIT WINDOW LANG_MFGENCT_UNALLORDERS+CHR(13)+LANG_MFGENCT_ORDER+' : '+ORDER NOWAIT
    WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_UNALLORDERS,loFormSet.GetHeaderText("LANG_MFGENCT_UNALLORDERS",loFormSet.HeaderAlias))+CHR(13)+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ORDER,loFormSet.GetHeaderText("LANG_MFGENCT_ORDER",loFormSet.HeaderAlias))+' : '+ORDER NOWAIT
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    SCATTER MEMVAR
    IF loFormSet.loOrdLine.SEEK(CORDTYPE+ORDER)
      SELECT ORDLINE
      *B607844,1 MMT 11/28/2006 Fix bug of not all lines of order are updated in Cutpick[Start]
      *SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = ORDHDR.CORDTYPE+ORDHDR.ORDER
      *B607958,1 WAM 02/01/2007 Fix bug of specific orders not shown when the first order line is deleted
      *SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = ORDHDR.CORDTYPE+ORDHDR.ORDER AND !DELETED()
      SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = ORDHDR.CORDTYPE+ORDHDR.ORDER FOR !DELETED()
        *B607958,1 WAM 02/01/2007 (End)

        *B607844,1 MMT 11/28/2006 Fix bug of not all lines of order are updated in Cutpick[End]
        IF TotQty - TotCut > 0
          INSERT INTO (lcTmpOrder) FROM MEMVAR
          EXIT
        ENDIF
      ENDSCAN
    ENDIF
  ENDSCAN
  WAIT CLEAR
  *-- end of lfGetOrder.

FUNCTION lfGetStyles

  *!*************************************************************
  *! Name      : lfRefOrder
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/12/2004
  *! Purpose   : Function to Refresh the temp file of unalocated orders.
  *!*************************************************************
FUNCTION lfRefOrder
  LPARAMETERS loFormSet

  IF loFormSet.llOrdRange
    SELECT (loFormSet.lcOrdAlias)
  ELSE
    SELECT (loFormSet.lcTmpOrder)
  ENDIF
  SCAN
    llDelete = .T.
    lcOrder = ORDER
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *WAIT WINDOW LANG_MFGENCT_CHECKUNALLO+CHR(13)+LANG_MFGENCT_ORDER+' : '+lcOrder NOWAIT
    WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CHECKUNALLO,loFormSet.GetHeaderText("LANG_MFGENCT_CHECKUNALLO",loFormSet.HeaderAlias))+CHR(13)+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ORDER,loFormSet.GetHeaderText("LANG_MFGENCT_ORDER",loFormSet.HeaderAlias))+' : '+lcOrder NOWAIT
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    IF loFormSet.loOrdLine.SEEK('O'+lcOrder)
      SELECT ORDLINE
      SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+lcOrder
        IF TotQty - TotCut > 0
          llDelete = .F.
          EXIT
        ENDIF
      ENDSCAN
      IF llDelete
        IF loFormSet.llOrdRange .AND. !SEEK('O'+lcOrder,loFormSet.lcTmpOrder)
          LOOP
        ENDIF
        SELECT (loFormSet.lcTmpOrder)
        DELETE
      ENDIF
    ENDIF
  ENDSCAN
  WAIT CLEAR
  *-- end of lfRefOrder.

  *!***********************************************************************************
  *! Name      : lfCrtprjTm
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 11/30/2004
  *! Purpose   : Creat temp files needed to generat project
  *!***********************************************************************************
  *! Calls     :
  *!***********************************************************************************
  *! Parameters: None
  *!***********************************************************************************
  *! Returns   : None.
  *!***********************************************************************************
  *! Example   : =lfCrtprjTm()
  *!***********************************************************************************
FUNCTION lfCrtprjTm
  LPARAMETERS loFormSet

*B608873,1 WAM 05/25/2009 OPen project tempalte file
=gfOpenTable( 'PMPTHHD', 'PMPTHHD','SH')
*B608873,1 WAM 05/25/2009 Following lines are commented out

*!*	  DIMENSION laFilField[4,18]
*!*	  laFilField[1,1] = 'cOprt_Ctg'
*!*	  laFilField[1,2] = 'C'
*!*	  laFilField[1,3] = 3
*!*	  laFilField[1,4] = 0
*!*	  laFilField[2,1] = 'cOprt_ID'
*!*	  laFilField[2,2] = 'C'
*!*	  laFilField[2,3] = 5
*!*	  laFilField[2,4] = 0
*!*	  laFilField[3,1] = 'dStrtDate'
*!*	  laFilField[3,2] = 'D'
*!*	  laFilField[3,3] = 8
*!*	  laFilField[3,4] = 0
*!*	  laFilField[4,1] = 'nDurIndic'
*!*	  laFilField[4,2] = 'N'
*!*	  laFilField[4,3] = 1
*!*	  laFilField[4,4] = 0

*!*	  FOR lnI = 7 TO 16
*!*	    FOR lnJ = 1 TO 4
*!*	      laFilField[lnJ,lnI] = ''
*!*	    ENDFOR
*!*	  ENDFOR
*!*	  FOR lnJ = 1 TO 4
*!*	    STORE 0 TO laFilField[lnJ,17],laFilField[lnJ,18]
*!*	  ENDFOR

*!*	  =gfCrtTmp(loFormSet.lc_Parser,@laFilField)

*!*	  loFormSet.loPMPRJHD = CREATEOBJECT('RemoteTable','PMPRJHD','PMPRJHD','PMPRJHD',loFormSet.DATASESSIONID)
*!*	  loFormSet.loPMPRJDT = CREATEOBJECT('RemoteTable','PMPRJDT','PMPRJDT','PMPRJDT',loFormSet.DATASESSIONID)
*!*	  loFormSet.loPMPRJRL = CREATEOBJECT('RemoteTable','PMPRJRL','PMPRJRL','PMPRJRL',loFormSet.DATASESSIONID)
*!*	  *-- Create a table with the same structure as PMPRJDT
*!*	  *-- adding two more fields for saving.
*!*	  SELECT PMPRJDT
*!*	  =AFIELDS(laFileStru)
*!*	  lnFileStru = ALEN(laFileStru, 1)
*!*	  DIMENSION laFileStru(lnFileStru + 3, 18)
*!*	  lnFileStru = lnFileStru + 1
*!*	  laFileStru[lnFileStru ,1] = 'cStatus'
*!*	  laFileStru[lnFileStru ,2] = 'C'
*!*	  laFileStru[lnFileStru ,3] = 1
*!*	  laFileStru[lnFileStru ,4] = 0
*!*	  lnFileStru = lnFileStru + 1
*!*	  laFileStru[lnFileStru ,1] = 'nRecNo'
*!*	  laFileStru[lnFileStru ,2] = 'N'
*!*	  laFileStru[lnFileStru ,3] = 10
*!*	  laFileStru[lnFileStru ,4] = 0
*!*	  lnFileStru = lnFileStru + 1
*!*	  laFileStru[lnFileStru ,1] = 'cMComplt'
*!*	  laFileStru[lnFileStru ,2] = 'C'
*!*	  laFileStru[lnFileStru ,3] = 3
*!*	  laFileStru[lnFileStru ,4] = 0

*!*	  FOR lnI = 7 TO 16
*!*	    FOR lnJ = 1 TO 3
*!*	      laFileStru[lnJ,lnI] = ''
*!*	    ENDFOR
*!*	  ENDFOR
*!*	  FOR lnJ = 1 TO 3
*!*	    STORE 0 TO laFileStru[lnJ,17],laFileStru[lnJ,18]
*!*	  ENDFOR

*!*	  DECLARE laIndex[2,3]
*!*	  laIndex[1,1] = 'cPrj_Typ+cPrj_ID+cStyle+cOprt_Ctg+cOprt_ID'
*!*	  laIndex[1,2] = 'PMPRJDT'
*!*	  laIndex[2,1] = 'cPrj_Typ+cPrj_ID+cStyle+cCtg_Seq+cOprt_Seq'
*!*	  laIndex[2,2] = 'PMPRJDTS'
*!*	  laIndex[2,3] = 'cPrj_Typ+cPrj_Id+cOprt_Ctg+cOprt_Id+cOprt_Res'
*!*	  laIndex[2,3] = 'PMPRJUSR'
*!*	  =gfCrtTmp(loFormSet.lc_PMPrjDt,@laFileStru,@laIndex)

*!*	  CREATE CURSOR (lc_PrjHist) (cOpr_ID C(20),cUser_ID C(10),cRemain C(3),cCompDate C(8),cActnDate C(8),cStatus C(15))
*!*	  INDEX ON cOpr_ID TAG PMPRJDT OF (lc_PrjHist)

*!*	  DIMENSION laFileStru(6, 18)
*!*	  lnFileStru = 1
*!*	  laFileStru[lnFileStru ,1] = 'cOpr_ID'
*!*	  laFileStru[lnFileStru ,2] = 'C'
*!*	  laFileStru[lnFileStru ,3] = 20
*!*	  laFileStru[lnFileStru ,4] = 0
*!*	  lnFileStru = lnFileStru + 1
*!*	  laFileStru[lnFileStru ,1] = 'cUser_ID'
*!*	  laFileStru[lnFileStru ,2] = 'C'
*!*	  laFileStru[lnFileStru ,3] = 10
*!*	  laFileStru[lnFileStru ,4] = 0
*!*	  lnFileStru = lnFileStru + 1
*!*	  laFileStru[lnFileStru ,1] = 'cRemain'
*!*	  laFileStru[lnFileStru ,2] = 'C'
*!*	  laFileStru[lnFileStru ,3] = 3
*!*	  laFileStru[lnFileStru ,4] = 0
*!*	  lnFileStru = lnFileStru + 1
*!*	  laFileStru[lnFileStru ,1] = 'cCompDate'
*!*	  laFileStru[lnFileStru ,2] = 'C'
*!*	  laFileStru[lnFileStru ,3] = 8
*!*	  laFileStru[lnFileStru ,4] = 0
*!*	  lnFileStru = lnFileStru + 1
*!*	  laFileStru[lnFileStru ,1] = 'cActnDate'
*!*	  laFileStru[lnFileStru ,2] = 'C'
*!*	  laFileStru[lnFileStru ,3] = 8
*!*	  laFileStru[lnFileStru ,4] = 0
*!*	  lnFileStru = lnFileStru + 1
*!*	  laFileStru[lnFileStru ,1] = 'cStatus'
*!*	  laFileStru[lnFileStru ,2] = 'C'
*!*	  laFileStru[lnFileStru ,3] = 15
*!*	  laFileStru[lnFileStru ,4] = 0

*!*	  FOR lnI = 7 TO 16
*!*	    FOR lnJ = 1 TO 6
*!*	      laFileStru[lnJ,lnI] = ''
*!*	    ENDFOR
*!*	  ENDFOR
*!*	  FOR lnJ = 1 TO 6
*!*	    STORE 0 TO laFileStru[lnJ,17],laFileStru[lnJ,18]
*!*	  ENDFOR

*!*	  =gfCrtTmp(loFormSet.lc_PrjHist,@laFileStru,'cOpr_ID','PMPRJDT')

*!*	  =gfCrtTmp(loFormSet.lc_PrjAudt,@laFileStru,'cOprt_Ctg+cOprt_ID','PMPRJDT')

*!*	  SELECT PMPRJRL
*!*	  =AFIELDS(laFileStru)
*!*	  lnFileStru = ALEN(laFileStru, 1)
*!*	  DIMENSION laFileStru(lnFileStru + 2, 18)
*!*	  lnFileStru = lnFileStru + 1
*!*	  laFileStru[lnFileStru ,1] = 'cStatus'
*!*	  laFileStru[lnFileStru ,2] = 'C'
*!*	  laFileStru[lnFileStru ,3] = 1
*!*	  laFileStru[lnFileStru ,4] = 0
*!*	  lnFileStru = lnFileStru + 1
*!*	  laFileStru[lnFileStru ,1] = 'nRecNo'
*!*	  laFileStru[lnFileStru ,2] = 'N'
*!*	  laFileStru[lnFileStru ,3] = 10
*!*	  laFileStru[lnFileStru ,4] = 0

*!*	  FOR lnI = 7 TO 16
*!*	    FOR lnJ = 1 TO 2
*!*	      laFileStru[lnJ,lnI] = ''
*!*	    ENDFOR
*!*	  ENDFOR
*!*	  FOR lnJ = 1 TO 2
*!*	    STORE 0 TO laFileStru[lnJ,17],laFileStru[lnJ,18]
*!*	  ENDFOR

*!*	  DECLARE laIndex[2,2]
*!*	  laIndex[1,1] = 'cPrj_Typ+cPrj_ID+cStyle+cPrd_Ctg +cPrd_ID'
*!*	  laIndex[1,2] = 'PMPRJRLP'
*!*	  laIndex[2,1] = 'cPrj_Typ+cPrj_ID+cStyle+cOprt_Ctg+cOprt_ID'
*!*	  laIndex[2,2] = 'PMPRJRL'
*!*	  =gfCrtTmp(loFormSet.lc_PMPrjRl,@laFileStru,@laIndex)
*!*	  *-- End Of Function (lfCrtprjTm)

*B608873,1 WAM 05/25/2009 (End)


  *** Convert this function in Phase II.
  *!***********************************************************************************
  *! Name      : lfGenProj
  *! Developer : NADER NABIL ABD ELMONEM (NNA)
  *! Date      : 04/20/2004
  *! Purpose   : Generat project for the PO according to setup setting of the module
  *!***********************************************************************************
  *! Calls     :
  *!***********************************************************************************
  *! Parameters: None
  *!***********************************************************************************
  *! Returns   :  None.
  *!***********************************************************************************
  *! Example   :  =lfGenProj()
  *!***********************************************************************************
  *!B122503,1
FUNCTION lfGenProj
LPARAMETERS loFormSet

*B608873,1 WAM 05/25/2009 Create projects for generated POs
IF loFormSet.lcGenProj = 'I'  AND gfModalGen('INM38251B32000','DIALOG',loFormSet.lcPoType)=2
  RETURN
ENDIF

*! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[Start]
IF !loFormSet.LLRPGNLPJ
*! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[End]

  SELECT (loFormSet.LCHDRFILE)
  SCAN

   *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[Start]
   *!*      DO (oAriaApplication.ApplicationHome+'MFPROJ.FXP') WITH IIF(INLIST(loFormSet.lcChoice,'C','L'),'C','P'),;
   *!*      EVALUATE(loFormSet.LCHDRFILE+'.PO'),REPLICATE('*',12),EVALUATE(loFormSet.LCHDRFILE+'.CDEFTEMPL')
   IF !EMPTY(EVALUATE(loFormSet.LCHDRFILE+'.CDEFTEMPL'))
     *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
     IF FILE(oAriaApplication.CLIENTAPPLICATIONHOME+'MFPROJ.FXP')
       DO (oAriaApplication.CLIENTAPPLICATIONHOME+'MFPROJ.FXP') WITH IIF(INLIST(loFormSet.lcChoice,'C','L'),'C','P'),;
          EVALUATE(loFormSet.LCHDRFILE+'.PO'),REPLICATE('*',loFormSet.lnMajorLen),0,EVALUATE(loFormSet.LCHDRFILE+'.CDEFTEMPL')
     ELSE
     *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
       DO (oAriaApplication.ApplicationHome+'MFPROJ.FXP') WITH IIF(INLIST(loFormSet.lcChoice,'C','L'),'C','P'),;
          EVALUATE(loFormSet.LCHDRFILE+'.PO'),REPLICATE('*',loFormSet.lnMajorLen),0,EVALUATE(loFormSet.LCHDRFILE+'.CDEFTEMPL')
     *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
     ENDIF
     *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
   ENDIF
   *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[End]

  ENDSCAN

*! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[Start]
ELSE
  DO CASE
    CASE INLIST(loFormSet.lcChoice,'P','O')
      SELECT (loFormSet.LCHDRFILE)
      SCAN
        lcDivision = cDivision
        lcGrade    = cStyGrade
        lcPurCode  = cPurCode
        lcShipVia  = SPACE(6)
        lcPo    = po
        lcStyle = Style
        SELECT (loFormSet.lcPOLine)
        =SEEK(SPACE(6)+lcDivision+lcPurCode+lcGrade+IIF(loFormSet.llRPGrpByST,LEFT(lcStyle,loFormSet.lnMajorLen),''))
        SCAN REST WHILE PO+cDivision+cPurCode+cStyGrade+STYLE+Dyelot = ;
        SPACE(6)+lcDivision+lcPurCode+lcGrade+IIF(loFormSet.llRPGrpByST,LEFT(lcStyle,loFormSet.lnMajorLen),'') FOR TotQty > 0
          lnLineNum = LineNo
          lcStyleNo = Style
          lcTemplate =  IIF(!EMPTY(CDEFTEMPL),CDEFTEMPL,EVALUATE(loFormSet.LCHDRFILE+'.CDEFTEMPL'))
          IF !EMPTY(lcTemplate)
            *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
		    IF FILE(oAriaApplication.CLIENTAPPLICATIONHOME+'MFPROJ.FXP')
              DO (oAriaApplication.CLIENTAPPLICATIONHOME+'MFPROJ.FXP') WITH 'P',;
              lcPo,lcStyleNo ,lnLineNum ,lcTemplate
		    ELSE
		    *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
              DO (oAriaApplication.ApplicationHome+'MFPROJ.FXP') WITH 'P',;
              lcPo,lcStyleNo ,lnLineNum ,lcTemplate
            *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
            ENDIF
            *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
          ENDIF
        ENDSCAN
      ENDSCAN

    CASE loFormSet.lcChoice = 'C'
      SELECT (loFormSet.LCHDRFILE)
      SCAN
        lcStyle    = SUBSTR(STYLE,1,loFormSet.lnMajorLen)
        lcPo       = PO
        lcDivision = cDivision
        lcSeason   = SEASON
        lcYear     = cYear
        lcWeek     = cWeek
        SELECT (loFormSet.lcPOLine)
        =SEEK(SPACE(6)+lcDivision+lcSeason+lcWeek+lcYear+lcStyle)
        SCAN REST WHILE PO+cDivision+SEASON+cWeek+cYear+STYLE+Dyelot = SPACE(6)+lcDivision+lcSeason+lcWeek+lcYear+lcStyle FOR TotQty > 0
          lnLineNum = LineNo
          lcStyleNo = Style
          lcTemplate =  IIF(!EMPTY(CDEFTEMPL),CDEFTEMPL,EVALUATE(loFormSet.LCHDRFILE+'.CDEFTEMPL'))
          IF !EMPTY(lcTemplate)
            *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
            IF FILE(oAriaApplication.CLIENTAPPLICATIONHOME+'MFPROJ.FXP')
              DO (oAriaApplication.CLIENTAPPLICATIONHOME+'MFPROJ.FXP') WITH 'C',;
              lcPo,lcStyleNo ,lnLineNum ,lcTemplate
            ELSE
            *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
              DO (oAriaApplication.ApplicationHome+'MFPROJ.FXP') WITH 'C',;
              lcPo,lcStyleNo ,lnLineNum ,lcTemplate
            *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
            ENDIF
            *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
          ENDIF
        ENDSCAN
      ENDSCAN
    CASE loFormSet.lcChoice = 'L'
      SELECT (loFormSet.LCHDRFILE)
      SCAN
        lcPo       = PO
        lcStyle    = SUBSTR(STYLE,1,loFormSet.lnMajorLen)
        lcDivision = cDivision
        lcSeason   = SEASON
        lcYear     = cYear
        lcWeek     = cWeek
        SELECT (loFormSet.lcPOLine)
        =SEEK(lcPo+lcDivision+lcSeason+lcWeek+lcYear+lcStyle)
        SCAN REST WHILE PO+cDivision+SEASON+cWeek+cYear+STYLE+Dyelot = lcPo+lcDivision+lcSeason+lcWeek+lcYear+lcStyle FOR TotQty > 0
          lnLineNum = LineNo
          lcStyleNo = Style
          lcTemplate =  IIF(!EMPTY(CDEFTEMPL),CDEFTEMPL,EVALUATE(loFormSet.LCHDRFILE+'.CDEFTEMPL'))
          IF !EMPTY(lcTemplate)
            *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
            IF FILE(oAriaApplication.CLIENTAPPLICATIONHOME+'MFPROJ.FXP')
              DO (oAriaApplication.CLIENTAPPLICATIONHOME+'MFPROJ.FXP') WITH 'C',;
              lcPo,lcStyleNo ,lnLineNum ,lcTemplate
            ELSE
			*! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
              DO (oAriaApplication.ApplicationHome+'MFPROJ.FXP') WITH 'C',;
              lcPo,lcStyleNo ,lnLineNum ,lcTemplate
            *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[Start]
            ENDIF
            *! E303029,1 MMT 12/26/2011 correct the calling of non-major programs within the SaaS environemnt[END]
          ENDIF
        ENDSCAN
      ENDSCAN
  ENDCASE
ENDIF
*! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[End]
*B608873,1 WAM 05/25/2009 (End)

  *** Convert this function in Phase II.
  *!***********************************************************************************
  *! Name      : lfUpdMastr
  *! Developer : NADER NABIL ABD ELMONEM (NNA)
  *! Date      : 04/20/2004
  *! Purpose   : Update master files of production schedule
  *!***********************************************************************************
  *! Calls     :
  *!***********************************************************************************
  *! Parameters: None
  *!***********************************************************************************
  *! Returns   :  None.
  *!***********************************************************************************
  *! Example   :  =lfUpdMastr()
  *!***********************************************************************************
  *!B122503,1
FUNCTION lfUpdMastr
  INSERT INTO PMPRJHD (cprj_typ,cprj_id,Cstyle,cprj_sdsc,cpath_id,cprj_stts,;
    llaststrt,dEst_Strt,dEst_Fnsh,dreq_strt,dreq_fnsh,lschedual,llok_stat);
    VALUES (lcChoice,IIF(lcChoice = 'P',&lcHdrFile..PO,&lcHdrFile..CutTKt),;
    IIF(lcChoice = 'P',REPLICATE('*', 12),&lcHdrFile..STYLE),;
    IIF(lcChoice = 'P',ALLTRIM(&lcHdrFile..Vendor)+', All Styles',&lcHdrFile..DESC),;
    lcDefTemp,'P',.F.,m.dEst_Strt,m.dEst_Fnsh,&lcHdrFile..ENTERED,;
    &lcHdrFile..COMPLETE,.F.,.T.)

  SELECT (lc_PMPrjDt)
  SCAN FOR cprj_id = IIF(lcChoice = 'P',&lcHdrFile..PO,&lcHdrFile..CutTKt)
    SCATTER MEMVAR MEMO
    SELECT PMPRJDT
    m.LORGINAL = .T.
    INSERT INTO PMPRJDT FROM MEMVAR

    IF !EMPTY(m.cOprt_res) OR !EMPTY(m.cGroup_Id)
      IF !SEEK(m.cprj_typ+ m.cprj_id+m.cOprt_Ctg+m.cOprt_ID,'SYSCHDUL')
        INSERT INTO SYSCHDUL;
          (cconttype , cseqnumber , ccont_id , csubject,;
          ctrantype,nestdur,ccompleted,cUser_ID,dtrandate,dcmpltdate,COPERSTAT,;
          lPredComp);
          VALUES(m.cprj_typ,m.cprj_id,m.cOprt_Ctg+m.cOprt_ID,;
          m.coprt_dsc,'W',m.nest_dur,'N',;
          IIF(EMPTY(m.cOprt_res),m.cGroup_Id,m.cOprt_res),;
          IIF(EMPTY(m.dclc_strt),m.dEst_Strt,m.dclc_strt),;
          IIF(EMPTY(m.dclc_Fnsh),m.dEst_Fnsh,m.dclc_Fnsh),'O',.T.)
      ELSE
        REPLACE SYSCHDUL.cUser_ID   WITH IIF(EMPTY(m.cOprt_res),m.cGroup_Id,m.cOprt_res),;
          SYSCHDUL.csubject   WITH m.coprt_dsc,;
          SYSCHDUL.ctrantype  WITH 'W',;
          SYSCHDUL.nestdur    WITH m.nest_dur ,;
          SYSCHDUL.ccompleted WITH 'N'  ,;
          SYSCHDUL.dtrandate  WITH IIF(EMPTY(m.dclc_strt),m.dEst_Strt,m.dclc_strt),;
          SYSCHDUL.dcmpltdate WITH IIF(EMPTY(m.dclc_Fnsh),m.dEst_Fnsh,m.dclc_Fnsh),;
          SYSCHDUL.COPERSTAT  WITH 'O',;
          SYSCHDUL.lPredComp  WITH .T.
      ENDIF

      IF SEEK(m.cprj_typ+ m.cprj_id+m.cOprt_Ctg+m.cOprt_ID,'SYSCHDUL')
        IF m.nAct_Dur <> 0 OR !EMPTY(m.dAct_Fnsh)
          REPLACE SYSCHDUL.COPERSTAT  WITH 'C',;
            SYSCHDUL.ccompleted WITH 'Y'
        ENDIF
        IF m.lVoid
          REPLACE SYSCHDUL.COPERSTAT WITH 'X'
        ENDIF
      ENDIF
    ENDIF

    *-- Update related records for every line.
    SELECT (lc_PMPrjRl)
    *-- Add cOprt_Ctg

    SCAN FOR cprj_typ + cprj_id + Cstyle + cOprt_Ctg  + cOprt_ID = ;
        &lc_PMPrjDt..cprj_typ + &lc_PMPrjDt..cprj_id+;
        &lc_PMPrjDt..Cstyle + &lc_PMPrjDt..cOprt_Ctg + &lc_PMPrjDt..cOprt_ID

      SCATTER MEMVAR MEMO

      DO CASE
        CASE SEEK(m.cprj_typ+m.cprj_id+m.cPrd_Ctg+m.cPrd_Id+'O','SYSCHDUL')
          =SEEK(m.cprj_typ+ m.cprj_id+m.cOprt_Ctg+m.cOprt_ID+'O','SYSCHDUL')
          REPLACE SYSCHDUL.lPredComp WITH .F.
        CASE SEEK(m.cprj_typ+m.cprj_id+m.cPrd_Ctg+m.cPrd_Id+'C','SYSCHDUL')
          =SEEK(m.cprj_typ+ m.cprj_id+m.cOprt_Ctg+m.cOprt_ID+'O','SYSCHDUL')
          REPLACE SYSCHDUL.lPredComp WITH .T.
        CASE SEEK(m.cprj_typ+m.cprj_id+m.cPrd_Ctg+m.cPrd_Id+'X','SYSCHDUL')
          =SEEK(m.cprj_typ+ m.cprj_id+m.cOprt_Ctg+m.cOprt_ID+'O','SYSCHDUL')
          REPLACE SYSCHDUL.lPredComp WITH .T.
      ENDCASE

      SELECT PMPRJRL
      INSERT INTO PMPRJRL FROM MEMVAR
    ENDSCAN

  ENDSCAN

  *-- Update Audit Trail
  lcProg  = 'POSTY'
  lcKey   = lcChoice + IIF(lcChoice = 'P',&lcHdrFile..PO,&lcHdrFile..CutTKt)
  DO CASE
    CASE lcChoice = 'A'      && Adorment Order
      lcEvent    = 'RESCHDPA'
      lcProg     = 'MFADPO'
      lcApObjNam = 'MFADPO'
    CASE lcChoice = 'D'      && Dye Order
      lcEvent    = 'RESCHDPD'
      lcProg     = 'MFDPO'
      lcApObjNam = 'MFDPO'
    CASE lcChoice = 'P'      && PO
      lcEvent    = 'RESCHDPO'
      lcProg     = 'POSTY'
      lcApObjNam = 'POSTY'
    CASE lcChoice = 'N'      && Inter-Location PO
      lcEvent    = 'RESCHDPN'
      lcProg     = 'POINTRC'
      lcApObjNam = 'POINTRC'
    CASE lcChoice = 'R'      && Return PO
      lcEvent    = 'RESCHDPR'
      lcProg     = 'RETPO'
      lcApObjNam = 'RETPO'
    CASE lcChoice = 'C'      && Cut Ticket
      lcEvent    = 'RESCHDCT'
      lcProg     = 'MFCUTKT'
      lcApObjNam = 'MFCUTKT'
  ENDCASE

  IF !SEEK(PADR(lcProg,10)+PADR(lcKey,20),'AUDTRAIL')
    SELECT (lc_PMPrjDt)
    SCAN
      IF SEEK(&lc_PMPrjDt..cprj_typ + &lc_PMPrjDt..cprj_id + &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID,'SYSCHDUL')
        IF !(&lc_PMPrjDt..LORGINAL)
          SELECT SYSCHDUL
          lcStauts = SYSCHDUL.COPERSTAT
          LOCATE REST WHILE cconttype+cseqnumber+ccont_id+COPERSTAT+cUser_ID =;
            &lc_PMPrjDt..cprj_typ + &lc_PMPrjDt..cprj_id + &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID ;
            FOR COPERSTAT <> lcStauts
          IF !FOUND()
            =SEEK(&lc_PMPrjDt..cprj_typ + &lc_PMPrjDt..cprj_id + &lc_PMPrjDt..cOprt_Ctg+&lc_PMPrjDt..cOprt_ID+lcStauts,'SYSCHDUL')
          ENDIF
        ENDIF
        IF SYSCHDUL.lPredComp
          SELECT (lc_PMPrjDt)
          SCATTER MEMVAR MEMO
          ldest_Fnsh = m.dEst_Fnsh
          lnrem_dur  = m.nrem_dur
          lcInform = ""
          DO lfUpdAdTrl IN gcAppHome+"MFPROJ" WITH lcChoice
          SELECT (lc_PrjAudt)
          DO gcAppHome+"SY\"+"GFAUDTRL" WITH lcProg , lcKey , lcApObjNam ,lcEvent,lcInform
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF

  SELECT PMPRJHD
  REPLACE llok_stat WITH .F.

  *-- End Of Function (lfUpdMastr)

  *!*************************************************************
  *! Name      : lfFormInit
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 11/09/2004
  *! Purpose   : function called from the INIT event of the form
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfFormInit
  LPARAMETERS loFormSet

 
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  lcPoType   = IIF(lcChoice $ 'PO',;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_PO,loFormSet.GetHeaderText("LANG_MFGENCT_PO",loFormSet.HeaderAlias)),;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CT,loFormSet.GetHeaderText("LANG_MFGENCT_CT",loFormSet.HeaderAlias)))
  *N000682,1 11/20/2012 MMT Globlization changes[End]


  IF TYPE("oARiaApplication") = "O" AND !oAriaApplication.ALLOWADD
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *=gfModalGen('INM00088B00000','DIALOG',LANG_MFGENCT_ADDING)
    =gfModalGen('INM00088B00000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ADDING,loFormSet.GetHeaderText("LANG_MFGENCT_ADDING",loFormSet.HeaderAlias)))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    RETURN .F.
  ENDIF

  =lfAddPro(loFormSet)
  loFormSet.loPosHdr   = CREATEOBJECT('RemoteTable','POSHDR'  ,'POSHDR'  ,'POSHDR'  ,loFormSet.DATASESSIONID)
  loFormSet.loCustomer = CREATEOBJECT('RemoteTable','CUSTOMER','CUSTOMER','CUSTOMER',loFormSet.DATASESSIONID)
  loFormSet.loOrdLine  = CREATEOBJECT('RemoteTable','ORDLINE' ,'ORDLINE' ,'ORDLINE' ,loFormSet.DATASESSIONID)
  loFormSet.loScale    = CREATEOBJECT('RemoteTable','SCALE'   ,'SCALE'   ,'SCALE'   ,loFormSet.DATASESSIONID)
  loFormSet.loStyle    = CREATEOBJECT('RemoteTable','STYLE'   ,'STYLE'   ,'STYLE'   ,loFormSet.DATASESSIONID)
  loFormSet.loCodes    = CREATEOBJECT('RemoteTable','CODES'   ,'cCode_No','CODES'   ,loFormSet.DATASESSIONID)

  *--- Open the SQL tables.
  IF !(TYPE('loFormSet.loPOSHDR')+TYPE('loFormSet.loCUSTOMER')+TYPE('loFormSet.loORDLINE')+;
      TYPE('loFormSet.loSCALE')+TYPE('loFormSet.loSTYLE')+TYPE('loFormSet.loCODES')=='OOOOOO')
    RETURN .F.
  ENDIF

  *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
  *IF loFormSet.lcChoice # 'P'
  IF !INLIST(loFormSet.lcChoice,'P','O')
  *N000606,1 NNA (End)
    loFormSet.AriaForm1.cmdPODef.VISIBLE = .F.
  ENDIF
  *--Add and icon for the scope in the control pannel
  DIMENSION loFormSet.laPanelObj[1,6]
  loFormSet.laPanelObj[1,1] = 'cmdScope'
  loFormSet.laPanelObj[1,2] = oAriaApplication.BitmapHome+'SCOPE.BMP'
  loFormSet.laPanelObj[1,3] = "mvScope"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laPanelObj[1,4] = LANG_MFGENCT_SCOPE &&'Scope'
loFormSet.laPanelObj[1,4] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_SCOPE,loFormSet.GetHeaderText("LANG_MFGENCT_SCOPE",loFormSet.HeaderAlias)) &&'Scope'
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laPanelObj[1,5] = LANG_MFGENCT_SCOPE &&'Scope'
loFormSet.laPanelObj[1,5] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_SCOPE,loFormSet.GetHeaderText("LANG_MFGENCT_SCOPE",loFormSet.HeaderAlias)) &&'Scope'
*N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.laPanelObj[1,6] = "SVE"

  *-- Check if the color is one of the segment and get its information
  *-- (lenght, start position))
  IF !lfCheckMaj(loFormSet)
    *--Item structure not found, Cannot Proceed.
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *=gfModalGen('QRM42080B42001','DIALOG',LANG_MFGENCT_ITMSTRNFND)
    =gfModalGen('QRM42080B42001','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ITMSTRNFND,loFormSet.GetHeaderText("LANG_MFGENCT_ITMSTRNFND",loFormSet.HeaderAlias)))
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    RETURN .F.
  ENDIF

  DECLARE laStySeg[1]
  =gfItemMask(@laStySeg)
  FOR lnCnt = loFormSet.lnMjorCnt + 1 TO ALEN(laStySeg,1)
    IF laStySeg[lnCnt , 1] = "C"
      *-- Flag to know if there is color in the style code strucure.
      loFormSet.llColorExt = .T.
      *-- Var. hold the start position of the color segment in the style code strucure.
      loFormSet.lnColorStr = laStySeg[lnCnt , 4]
      *-- Var. hold the color segment lenght in the style code strucure.
      loFormSet.lnColorLen = LEN(laStySeg[lnCnt , 3])
    ENDIF
  ENDFOR

  loFormSet.lcPOH    = gfTempName()  && temprory PosHdr
  loFormSet.lcPOLine = gfTempName()  && temprory PosLine
  *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
  *IF loFormSet.lcChoice <> 'L'
  IF !INLIST(loFormSet.lcChoice ,'L','O')
  *N000606,1 NNA (End)
    loFormSet.lcCutPick   = gfTempName()  && temporary pick ticket
    loFormSet.lcOrdLine   = gfTempName()  && temporary order line
    loFormSet.lcTmpOrd    = gfTempName()  && Temporary allocated/unallocated order lines
    loFormSet.lcPrintAll  = gfTempName()  && Temporary allocated/unallocated order lines
    loFormSet.lcOrdGroup  = gfTempName()  && Temporary Order Groups File Name
    loFormSet.lcFabrics   = gfTempName()  && Temporary Fabric/Color/Dyelot File Name
    loFormSet.lcStyUnAll  = gfTempName()
  ELSE
    loFormSet.lcStyTmp    = gfTempName()  && temprory Style
  ENDIF
  loFormSet.lcLinFile = loFormSet.lcPOLine  &&&& Variable to hold the header file
  loFormSet.lcHdrFile = loFormSet.lcPOH
  STORE oAriaApplication.BaseCurrency TO loFormSet.lcPCurr,loFormSet.lcDCurr

  *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
  *IF loFormSet.lcChoice <> 'L'
  IF !INLIST(loFormSet.lcChoice ,'L','O')
  *N000606,1 NNA (End)
    =lfGetOrder(loFormSet)
  ENDIF
  =lfCrtTemp(loFormSet)

  *-- end of lfFormInit.

  *!*************************************************************
  *! Name      : lfAfterInit
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 11/09/2004
  *! Purpose   : function called from the INIT event of the form after the default code of AriaFormSet.
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfAfterInit
  LPARAMETERS loFormSet
  
 
  WITH loFormSet.AriaForm1
    DO CASE
      CASE loFormSet.lcChoice='O'
        *E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][Start]
        loFormSet.Ariaform1.cmdOrderNote.visible = .F.
		*E303079,1 MMT 03/05/2012 Fixing Media issues[T20120304.0004][End]

        WITH .grdOrdLines
          .RECORDSOURCE = ''
          .COLUMNCOUNT = 20
          .RECORDSOURCE = loFormSet.lcStyTmp
          .COLUMNS(1).CONTROLSOURCE = loFormSet.lcStyTmp+".lSelect"
          .COLUMNS(2).CONTROLSOURCE = loFormSet.lcStyTmp+".Style"
          .COLUMNS(2).Header1.CAPTION = ALLTRIM(gfItemMask('HM'))
          .COLUMNS(2).WIDTH = 150
          .COLUMNS(3).CONTROLSOURCE = loFormSet.lcStyTmp+".PLAN1"
           *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
           *.COLUMNS(3).Header1.CAPTION = "OTS1 "
           .COLUMNS(3).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_OTS1,loFormSet.GetHeaderText("LANG_MFGENCT_OTS1",loFormSet.HeaderAlias))
           *N000682,1 11/20/2012 MMT Globlization changes[End]

          *N000682,1 MMT 11/20/2012 Globalization Changes[End]
          .COLUMNS(3).Header1.ALIGNMENT= 1
          .COLUMNS(3).WIDTH = 50
          .COLUMNS(4).CONTROLSOURCE = loFormSet.lcStyTmp+".PLAN2"
          *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
          *.COLUMNS(4).Header1.CAPTION = "OTS2 "
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
          *.COLUMNS(4).Header1.CAPTION =LANG_MFGENCT_OTS2
          .COLUMNS(4).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_OTS2,loFormSet.GetHeaderText("LANG_MFGENCT_OTS2",loFormSet.HeaderAlias))
          *N000682,1 11/20/2012 MMT Globlization changes[End]

          *N000682,1 11/20/2012 MMT Globlization changes[End]

          .COLUMNS(4).Header1.ALIGNMENT= 1
          .COLUMNS(4).WIDTH = 50
          .COLUMNS(5).CONTROLSOURCE = loFormSet.lcStyTmp+".PLAN3"
          *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
          *          .COLUMNS(5).Header1.CAPTION = "OTS3 "
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(5).Header1.CAPTION =LANG_MFGENCT_OTS3
.COLUMNS(5).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_OTS3,loFormSet.GetHeaderText("LANG_MFGENCT_OTS3",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

          *N000682,1 11/20/2012 MMT Globlization changes[End]

          .COLUMNS(5).Header1.ALIGNMENT= 1
          .COLUMNS(5).WIDTH = 50
          .COLUMNS(6).CONTROLSOURCE = loFormSet.lcStyTmp+".PLAN4"
          *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
          *          .COLUMNS(6).Header1.CAPTION = "OTS4 "
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(6).Header1.CAPTION =LANG_MFGENCT_OTS4
.COLUMNS(6).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_OTS4,loFormSet.GetHeaderText("LANG_MFGENCT_OTS4",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

          *N000682,1 MMT 11/20/2012 Globalization Changes[End]

          .COLUMNS(6).Header1.ALIGNMENT= 1
          .COLUMNS(6).WIDTH = 50
          .COLUMNS(7).CONTROLSOURCE = loFormSet.lcStyTmp+".PLAN5"
          *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
          *          .COLUMNS(7).Header1.CAPTION = "OTS5 "
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(7).Header1.CAPTION =LANG_MFGENCT_OTS5
.COLUMNS(7).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_OTS5,loFormSet.GetHeaderText("LANG_MFGENCT_OTS5",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

          *N000682,1 11/20/2012 MMT Globlization changes[End]

          .COLUMNS(7).Header1.ALIGNMENT= 1
          .COLUMNS(7).WIDTH = 50
          .COLUMNS(8).CONTROLSOURCE = loFormSet.lcStyTmp+".PLAN6"
          *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
          *          .COLUMNS(8).Header1.CAPTION = "OTS6 "
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(8).Header1.CAPTION =LANG_MFGENCT_OTS6
.COLUMNS(8).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_OTS6,loFormSet.GetHeaderText("LANG_MFGENCT_OTS6",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

          *N000682,1 MMT 11/20/2012 Globalization Changes[End]
          .COLUMNS(8).Header1.ALIGNMENT= 1
          .COLUMNS(8).WIDTH = 50
          .COLUMNS(9).CONTROLSOURCE = loFormSet.lcStyTmp+".PLAN7"
          *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
          *.COLUMNS(9).Header1.CAPTION = "OTS7 "
          .COLUMNS(9).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_OTS7,loFormSet.GetHeaderText("LANG_MFGENCT_OTS7",loFormSet.HeaderAlias))
          *N000682,1 11/20/2012 MMT Globlization changes[End]

          *N000682,1 MMT 11/20/2012 Globalization Changes[End]
          .COLUMNS(9).Header1.ALIGNMENT= 1
          .COLUMNS(9).WIDTH = 50
          .COLUMNS(10).CONTROLSOURCE = loFormSet.lcStyTmp+".PLAN8"
          *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
          *          .COLUMNS(10).Header1.CAPTION = "OTS8 "
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
           *.COLUMNS(10).Header1.CAPTION =LANG_MFGENCT_OTS8
         .COLUMNS(10).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_OTS8,loFormSet.GetHeaderText("LANG_MFGENCT_OTS8",loFormSet.HeaderAlias))
          *N000682,1 11/20/2012 MMT Globlization changes[End]

          *N000682,1 MMT 11/20/2012 Globalization Changes[End]
          .COLUMNS(10).Header1.ALIGNMENT= 1
          .COLUMNS(10).WIDTH = 50
          .COLUMNS(11).CONTROLSOURCE = loFormSet.lcStyTmp+".TOTPLAN"
          *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
          *.COLUMNS(11).Header1.CAPTION = "Tot. OTS "
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(11).Header1.CAPTION =LANG_MFGENCT_TOT_OTS
.COLUMNS(11).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_TOT_OTS,loFormSet.GetHeaderText("LANG_MFGENCT_TOT_OTS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

          *N000682,1 MMT 11/20/2012 Globalization Changes[End]
          .COLUMNS(11).Header1.ALIGNMENT= 1
          .COLUMNS(11).WIDTH = 50
          .COLUMNS(12).CONTROLSOURCE = loFormSet.lcStyTmp+".nPoQty1"
          *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
          *          .COLUMNS(12).Header1.CAPTION = "Ord1 "
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(12).Header1.CAPTION =LANG_MFGENCT_ORD1
.COLUMNS(12).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ORD1,loFormSet.GetHeaderText("LANG_MFGENCT_ORD1",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

          *N000682,1 11/20/2012 MMT Globlization changes[End]

          .COLUMNS(12).Header1.ALIGNMENT= 1
          .COLUMNS(12).WIDTH = 50
          .COLUMNS(12).currentcontrol="TEXT1"
          .COLUMNS(13).CONTROLSOURCE = loFormSet.lcStyTmp+".nPoQty2"
          *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
          *.COLUMNS(13).Header1.CAPTION = "Ord2 "
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(13).Header1.CAPTION =LANG_MFGENCT_ORD2
.COLUMNS(13).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ORD2,loFormSet.GetHeaderText("LANG_MFGENCT_ORD2",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

          *N000682,1 MMT 11/20/2012 Globalization Changes[End]
          .COLUMNS(13).Header1.ALIGNMENT= 1
          .COLUMNS(13).WIDTH = 50
          .COLUMNS(14).CONTROLSOURCE = loFormSet.lcStyTmp+".nPoQty3"
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
          *.COLUMNS(14).Header1.CAPTION = "Ord3 "
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(14).Header1.CAPTION =LANG_MFGENCT_ORD3
.COLUMNS(14).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ORD3,loFormSet.GetHeaderText("LANG_MFGENCT_ORD3",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

          *N000682,1 11/20/2012 MMT Globlization changes[End]
          .COLUMNS(14).Header1.ALIGNMENT= 1
          .COLUMNS(14).WIDTH = 50
          .COLUMNS(15).CONTROLSOURCE = loFormSet.lcStyTmp+".nPoQty4"
          *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
          *          .COLUMNS(15).Header1.CAPTION = "Ord4 "
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(15).Header1.CAPTION =LANG_MFGENCT_ORD4
.COLUMNS(15).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ORD4,loFormSet.GetHeaderText("LANG_MFGENCT_ORD4",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

          *N000682,1 MMT 11/20/2012 Globalization Changes[End]
          .COLUMNS(15).Header1.ALIGNMENT= 1
          .COLUMNS(15).WIDTH = 50
          .COLUMNS(16).CONTROLSOURCE = loFormSet.lcStyTmp+".nPoQty5"
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
          *.COLUMNS(16).Header1.CAPTION = "Ord5 "
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(16).Header1.CAPTION =LANG_MFGENCT_ORD5
.COLUMNS(16).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ORD5,loFormSet.GetHeaderText("LANG_MFGENCT_ORD5",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

          *N000682,1 MMT 11/20/2012 Globalization Changes[End]
          .COLUMNS(16).Header1.ALIGNMENT= 1
          .COLUMNS(16).WIDTH = 50
          .COLUMNS(17).CONTROLSOURCE = loFormSet.lcStyTmp+".nPoQty6"
          *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
          *.COLUMNS(17).Header1.CAPTION = "Ord6 "
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(17).Header1.CAPTION =LANG_MFGENCT_ORD6
.COLUMNS(17).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ORD6,loFormSet.GetHeaderText("LANG_MFGENCT_ORD6",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

          *N000682,1 MMT 11/20/2012 Globalization Changes[End]
          .COLUMNS(17).Header1.ALIGNMENT= 1
          .COLUMNS(17).WIDTH = 50
          .COLUMNS(18).CONTROLSOURCE = loFormSet.lcStyTmp+".nPoQty7"
          *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
          *          .COLUMNS(18).Header1.CAPTION = "Ord7 "
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(18).Header1.CAPTION =LANG_MFGENCT_ORD7
.COLUMNS(18).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ORD7,loFormSet.GetHeaderText("LANG_MFGENCT_ORD7",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

          *N000682,1 11/20/2012 MMT Globlization changes[End]
          .COLUMNS(18).Header1.ALIGNMENT= 1
          .COLUMNS(18).WIDTH = 50
          .COLUMNS(19).CONTROLSOURCE = loFormSet.lcStyTmp+".nPoQty8"
          *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
          *          .COLUMNS(19).Header1.CAPTION = "Ord8 "
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(19).Header1.CAPTION =LANG_MFGENCT_ORD8
.COLUMNS(19).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ORD8,loFormSet.GetHeaderText("LANG_MFGENCT_ORD8",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

          *N000682,1 MMT 11/20/2012 Globalization Changes[End]
          .COLUMNS(19).Header1.ALIGNMENT= 1
          .COLUMNS(19).WIDTH = 50
          .COLUMNS(20).CONTROLSOURCE = loFormSet.lcStyTmp+".nPoTotQty"
          *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
          *.COLUMNS(20).Header1.CAPTION = "Tot. Ord "
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
          *.COLUMNS(20).Header1.CAPTION =LANG_MFGENCT_TOT_ORD
          .COLUMNS(20).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_TOT_ORD,loFormSet.GetHeaderText("LANG_MFGENCT_TOT_ORD",loFormSet.HeaderAlias))
         *N000682,1 11/20/2012 MMT Globlization changes[End]

          *N000682,1 11/20/2012 MMT Globlization changes[End]

          .COLUMNS(20).Header1.ALIGNMENT= 1
          .COLUMNS(20).WIDTH = 50
          *N000606,1 NNA 10/06/2007 (BEGIN) Chnage columns format to show Zeros if they're empty
          FOR I=12 TO 20
            LCI = ALLTRIM(STR(I))
            .COLUMN&LCI..FORMAT="99999999"
          ENDFOR
          *N000606,1 NNA (END)
        ENDWITH
      CASE loFormSet.lcChoice='L'
         *MEDIA
         loFormSet.Ariaform1.cmdOrderNote.visible = .F.
         *MEDIA

        WITH .grdOrdLines
          .RECORDSOURCE = ''
          IF loFormSet.lcRpBaseOn='T'
            .COLUMNCOUNT = 13
            .RECORDSOURCE = loFormSet.lcStyTmp
            .COLUMNS(1).CONTROLSOURCE = loFormSet.lcStyTmp+".lSelect"
            .COLUMNS(2).CONTROLSOURCE = loFormSet.lcStyTmp+".Style"
            .COLUMNS(2).Header1.CAPTION = ALLTRIM(gfItemMask('HM'))
            .COLUMNS(2).WIDTH = 150
            .COLUMNS(3).CONTROLSOURCE = loFormSet.lcStyTmp+".CWEEK"
            *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
            *.COLUMNS(3).Header1.CAPTION = "Week"
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(3).Header1.CAPTION =LANG_MFGENCT_WEEK
.COLUMNS(3).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_WEEK,loFormSet.GetHeaderText("LANG_MFGENCT_WEEK",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 11/20/2012 MMT Globlization changes[End]
            .COLUMNS(3).Header1.ALIGNMENT= 0
            .COLUMNS(3).WIDTH = 50
            .COLUMNS(4).CONTROLSOURCE = loFormSet.lcStyTmp+".CYEAR"
            *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
            *.COLUMNS(4).Header1.CAPTION = "Year"
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(4).Header1.CAPTION =LANG_MFGENCT_YEAR
.COLUMNS(4).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_YEAR,loFormSet.GetHeaderText("LANG_MFGENCT_YEAR",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 11/20/2012 MMT Globlization changes[End]

            .COLUMNS(4).Header1.ALIGNMENT= 0
            .COLUMNS(4).WIDTH = 50
            .COLUMNS(5).CONTROLSOURCE = loFormSet.lcStyTmp+".PLAN1"
            *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
            *.COLUMNS(5).Header1.CAPTION = "Qty1"
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(5).Header1.CAPTION =LANG_MFGENCT_QTY1
.COLUMNS(5).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_QTY1,loFormSet.GetHeaderText("LANG_MFGENCT_QTY1",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 MMT 11/20/2012 Globalization Changes[End]
            .COLUMNS(5).Header1.ALIGNMENT= 1
            .COLUMNS(5).WIDTH = 50
            .COLUMNS(6).CONTROLSOURCE = loFormSet.lcStyTmp+".PLAN2"
            *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
            *            .COLUMNS(6).Header1.CAPTION = "Qty2"
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(6).Header1.CAPTION =LANG_MFGENCT_QTY2
.COLUMNS(6).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_QTY2,loFormSet.GetHeaderText("LANG_MFGENCT_QTY2",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 MMT 11/20/2012 Globalization Changes[End]
            .COLUMNS(6).Header1.ALIGNMENT= 1
            .COLUMNS(6).WIDTH = 50
            .COLUMNS(7).CONTROLSOURCE = loFormSet.lcStyTmp+".PLAN3"
            *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
            *            .COLUMNS(7).Header1.CAPTION = "Qty3"
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(7).Header1.CAPTION =LANG_MFGENCT_QTY3
.COLUMNS(7).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_QTY3,loFormSet.GetHeaderText("LANG_MFGENCT_QTY3",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 11/20/2012 MMT Globlization changes[End]

            .COLUMNS(7).Header1.ALIGNMENT= 1
            .COLUMNS(7).WIDTH = 50
            .COLUMNS(8).CONTROLSOURCE = loFormSet.lcStyTmp+".PLAN4"
            *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
            *            .COLUMNS(8).Header1.CAPTION = "Qty4"
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(8).Header1.CAPTION =LANG_MFGENCT_QTY4
.COLUMNS(8).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_QTY4,loFormSet.GetHeaderText("LANG_MFGENCT_QTY4",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 MMT 11/20/2012 Globalization Changes[End]
            .COLUMNS(8).Header1.ALIGNMENT= 1
            .COLUMNS(8).WIDTH = 50
            .COLUMNS(9).CONTROLSOURCE = loFormSet.lcStyTmp+".PLAN5"
            *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
            *            .COLUMNS(9).Header1.CAPTION = "Qty5"
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(9).Header1.CAPTION =LANG_MFGENCT_QTY5
.COLUMNS(9).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_QTY5,loFormSet.GetHeaderText("LANG_MFGENCT_QTY5",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 MMT 11/20/2012 Globalization Changes[End]
            .COLUMNS(9).Header1.ALIGNMENT= 1
            .COLUMNS(9).WIDTH = 50
            .COLUMNS(10).CONTROLSOURCE = loFormSet.lcStyTmp+".PLAN6"
            *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
            *            .COLUMNS(10).Header1.CAPTION = "Qty6"
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(10).Header1.CAPTION =LANG_MFGENCT_QTY6
.COLUMNS(10).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_QTY6,loFormSet.GetHeaderText("LANG_MFGENCT_QTY6",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 MMT 11/20/2012 Globalization Changes[End]
            .COLUMNS(10).Header1.ALIGNMENT= 1
            .COLUMNS(10).WIDTH = 50
            .COLUMNS(11).CONTROLSOURCE = loFormSet.lcStyTmp+".PLAN7"
            *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
            *            .COLUMNS(11).Header1.CAPTION = "Qty7"
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(11).Header1.CAPTION =LANG_MFGENCT_QTY7
.COLUMNS(11).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_QTY7,loFormSet.GetHeaderText("LANG_MFGENCT_QTY7",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 11/20/2012 MMT Globlization changes[End]
            .COLUMNS(11).Header1.ALIGNMENT= 1
            .COLUMNS(11).WIDTH = 50
            .COLUMNS(12).CONTROLSOURCE = loFormSet.lcStyTmp+".PLAN8"
            *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
            *            .COLUMNS(12).Header1.CAPTION = "Qty8"
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(12).Header1.CAPTION =LANG_MFGENCT_QTY8
.COLUMNS(12).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_QTY8,loFormSet.GetHeaderText("LANG_MFGENCT_QTY8",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 11/20/2012 MMT Globlization changes[End]

            .COLUMNS(12).Header1.ALIGNMENT= 1
            .COLUMNS(12).WIDTH = 50
            .COLUMNS(13).CONTROLSOURCE = loFormSet.lcStyTmp+".TOTPLAN"
            *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
            *            .COLUMNS(13).Header1.CAPTION = "TotQty"
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(13).Header1.CAPTION =LANG_MFGENCT_TOTQTY
.COLUMNS(13).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_TOTQTY,loFormSet.GetHeaderText("LANG_MFGENCT_TOTQTY",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 MMT 11/20/2012 Globalization Changes[End]

            .COLUMNS(13).Header1.ALIGNMENT= 1
            .COLUMNS(13).WIDTH = 50
          ELSE
            .COLUMNCOUNT = 11
            .RECORDSOURCE = loFormSet.lcStyTmp
            .COLUMNS(1).CONTROLSOURCE = loFormSet.lcStyTmp+".lSelect"
            .COLUMNS(2).CONTROLSOURCE = loFormSet.lcStyTmp+".Style"
            .COLUMNS(2).Header1.CAPTION = ALLTRIM(gfItemMask('HM'))
            .COLUMNS(2).WIDTH = 150
            .COLUMNS(3).CONTROLSOURCE = loFormSet.lcStyTmp+".PLAN1"
            *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
            *            .COLUMNS(3).Header1.CAPTION = "Qty1"
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(3).Header1.CAPTION =LANG_MFGENCT_QTY1
.COLUMNS(3).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_QTY1,loFormSet.GetHeaderText("LANG_MFGENCT_QTY1",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 11/20/2012 MMT Globlization changes[End]

            .COLUMNS(3).Header1.ALIGNMENT= 1
            .COLUMNS(3).WIDTH = 50
            .COLUMNS(4).CONTROLSOURCE = loFormSet.lcStyTmp+".PLAN2"
            *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
            *            .COLUMNS(4).Header1.CAPTION = "Qty2"
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(4).Header1.CAPTION =LANG_MFGENCT_QTY2
.COLUMNS(4).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_QTY2,loFormSet.GetHeaderText("LANG_MFGENCT_QTY2",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 11/20/2012 MMT Globlization changes[End]

            .COLUMNS(4).Header1.ALIGNMENT= 1
            .COLUMNS(4).WIDTH = 50
            .COLUMNS(5).CONTROLSOURCE = loFormSet.lcStyTmp+".PLAN3"
            *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
            *            .COLUMNS(5).Header1.CAPTION = "Qty3"
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(5).Header1.CAPTION =LANG_MFGENCT_QTY3
.COLUMNS(5).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_QTY3,loFormSet.GetHeaderText("LANG_MFGENCT_QTY3",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 11/20/2012 MMT Globlization changes[End]

            .COLUMNS(5).Header1.ALIGNMENT= 1
            .COLUMNS(5).WIDTH = 50
            .COLUMNS(6).CONTROLSOURCE = loFormSet.lcStyTmp+".PLAN4"
            *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
            *            .COLUMNS(6).Header1.CAPTION = "Qty4"
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(6).Header1.CAPTION =LANG_MFGENCT_QTY4
.COLUMNS(6).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_QTY4,loFormSet.GetHeaderText("LANG_MFGENCT_QTY4",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 11/20/2012 MMT Globlization changes[End]

            .COLUMNS(6).Header1.ALIGNMENT= 1
            .COLUMNS(6).WIDTH = 50
            .COLUMNS(7).CONTROLSOURCE = loFormSet.lcStyTmp+".PLAN5"
            *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
            *            .COLUMNS(7).Header1.CAPTION = "Qty5"
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(7).Header1.CAPTION =LANG_MFGENCT_QTY5
.COLUMNS(7).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_QTY5,loFormSet.GetHeaderText("LANG_MFGENCT_QTY5",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 11/20/2012 MMT Globlization changes[End]

            .COLUMNS(7).Header1.ALIGNMENT= 1
            .COLUMNS(7).WIDTH = 50
            .COLUMNS(8).CONTROLSOURCE = loFormSet.lcStyTmp+".PLAN6"
            *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
            *            .COLUMNS(8).Header1.CAPTION = "Qty6"
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(8).Header1.CAPTION =LANG_MFGENCT_QTY6
.COLUMNS(8).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_QTY6,loFormSet.GetHeaderText("LANG_MFGENCT_QTY6",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 11/20/2012 MMT Globlization changes[End]

            .COLUMNS(8).Header1.ALIGNMENT= 1
            .COLUMNS(8).WIDTH = 50
            .COLUMNS(9).CONTROLSOURCE = loFormSet.lcStyTmp+".PLAN7"
            *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
            *            .COLUMNS(9).Header1.CAPTION = "Qty7"
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(9).Header1.CAPTION =LANG_MFGENCT_QTY7
.COLUMNS(9).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_QTY7,loFormSet.GetHeaderText("LANG_MFGENCT_QTY7",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 11/20/2012 MMT Globlization changes[End]

            .COLUMNS(9).Header1.ALIGNMENT= 1
            .COLUMNS(9).WIDTH = 50
            .COLUMNS(10).CONTROLSOURCE = loFormSet.lcStyTmp+".PLAN8"
            *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
            *            .COLUMNS(10).Header1.CAPTION = "Qty8"
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(10).Header1.CAPTION =LANG_MFGENCT_QTY8
.COLUMNS(10).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_QTY8,loFormSet.GetHeaderText("LANG_MFGENCT_QTY8",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 MMT 11/20/2012 Globalization Changes[End]
            .COLUMNS(10).Header1.ALIGNMENT= 1
            .COLUMNS(10).WIDTH = 50
            .COLUMNS(11).CONTROLSOURCE = loFormSet.lcStyTmp+".TOTPLAN"
            *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
            *            .COLUMNS(11).Header1.CAPTION = "TotQty"
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.COLUMNS(11).Header1.CAPTION =LANG_MFGENCT_TOTQTY
.COLUMNS(11).Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_TOTQTY,loFormSet.GetHeaderText("LANG_MFGENCT_TOTQTY",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 11/20/2012 MMT Globlization changes[End]

            .COLUMNS(11).Header1.ALIGNMENT= 1
            .COLUMNS(11).WIDTH = 50
          ENDIF
        ENDWITH
      OTHERWISE
        WITH .grdOrdLines
          .RECORDSOURCE = ''
          .RECORDSOURCE = loFormSet.lcOrdLine
          .Column1.CONTROLSOURCE  = loFormSet.lcOrdLine+".lSelect"
          .Column2.CONTROLSOURCE  = loFormSet.lcOrdLine+".Order"
          .Column3.CONTROLSOURCE  = loFormSet.lcOrdLine+".Account"
          .Column4.CONTROLSOURCE  = loFormSet.lcOrdLine+".BTNAME"
          .Column5.CONTROLSOURCE  = loFormSet.lcOrdLine+".Style"
          .Column6.CONTROLSOURCE  = loFormSet.lcOrdLine+".nOpn1"
          .Column7.CONTROLSOURCE  = loFormSet.lcOrdLine+".nOpn2"
          .Column8.CONTROLSOURCE  = loFormSet.lcOrdLine+".nOpn3"
          .Column9.CONTROLSOURCE  = loFormSet.lcOrdLine+".nOpn4"
          .Column10.CONTROLSOURCE = loFormSet.lcOrdLine+".nOpn5"
          .Column11.CONTROLSOURCE = loFormSet.lcOrdLine+".nOpn6"
          .Column12.CONTROLSOURCE = loFormSet.lcOrdLine+".nOpn7"
          .Column13.CONTROLSOURCE = loFormSet.lcOrdLine+".nOpn8"
          .Column14.CONTROLSOURCE = loFormSet.lcOrdLine+".nTotOpn"
          .Column15.CONTROLSOURCE = loFormSet.lcOrdLine+".QTY1"
          .Column16.CONTROLSOURCE = loFormSet.lcOrdLine+".QTY2"
          .Column17.CONTROLSOURCE = loFormSet.lcOrdLine+".QTY3"
          .Column18.CONTROLSOURCE = loFormSet.lcOrdLine+".QTY4"
          .Column19.CONTROLSOURCE = loFormSet.lcOrdLine+".QTY5"
          .Column20.CONTROLSOURCE = loFormSet.lcOrdLine+".QTY6"
          .Column21.CONTROLSOURCE = loFormSet.lcOrdLine+".QTY7"
          .Column22.CONTROLSOURCE = loFormSet.lcOrdLine+".QTY8"
          .Column23.CONTROLSOURCE = loFormSet.lcOrdLine+".totQty"
          .Column24.CONTROLSOURCE = loFormSet.lcOrdLine+".Cut1"
          .Column25.CONTROLSOURCE = loFormSet.lcOrdLine+".Cut2"
          .Column26.CONTROLSOURCE = loFormSet.lcOrdLine+".Cut3"
          .Column27.CONTROLSOURCE = loFormSet.lcOrdLine+".Cut4"
          .Column28.CONTROLSOURCE = loFormSet.lcOrdLine+".Cut5"
          .Column29.CONTROLSOURCE = loFormSet.lcOrdLine+".Cut6"
          .Column30.CONTROLSOURCE = loFormSet.lcOrdLine+".Cut7"
          .Column31.CONTROLSOURCE = loFormSet.lcOrdLine+".Cut8"
          .Column32.CONTROLSOURCE = loFormSet.lcOrdLine+".totcut"
          .Column33.CONTROLSOURCE = loFormSet.lcOrdLine+".Start"
          .Column34.CONTROLSOURCE = loFormSet.lcOrdLine+".Complete"
        ENDWITH
    ENDCASE
    *B608873,1 WAM 05/25/2009 Changes in PO grid
    WITH .grdPOLines
      DO CASE
        CASE INLIST(loFormSet.lcChoice,'P','O')
          .RECORDSOURCE = loFormSet.lcHdrFile
          .Column1.CONTROLSOURCE  = loFormSet.lcHdrFile+".PO"
          .Column2.CONTROLSOURCE  = loFormSet.lcHdrFile+".VENDOR"
          .Column3.CONTROLSOURCE  = loFormSet.lcHdrFile+".CDIVISION"
          .Column4.CONTROLSOURCE  = loFormSet.lcHdrFile+".CWARECODE"
          .Column5.CONTROLSOURCE  = 'THISFORMSET.mGetSVIADesc()'
          .Column6.CONTROLSOURCE  = 'THISFORMSET.mGetTermDesc()'
          .Column7.CONTROLSOURCE  = loFormSet.lcHdrFile+".CDEFTEMPL"
          .Column8.CONTROLSOURCE  = loFormSet.lcHdrFile+".ENTERED"
          .Column9.CONTROLSOURCE  = loFormSet.lcHdrFile+".COMPLETE"
          .Column10.CONTROLSOURCE  = loFormSet.lcHdrFile+".AVAILABLE"
	
  	      .Column11.CONTROLSOURCE = loFormSet.lcHdrFile+".nStyOrder"
	        .Column11.INPUTMASK     = "99999999999"
          *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
          *	      .Column11.Header1.CAPTION = "Tot. Qty."
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
	        .Column11.Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_TOT_QTY,loFormSet.GetHeaderText("LANG_MFGENCT_TOT_QTY",loFormSet.HeaderAlias))
          *N000682,1 11/20/2012 MMT Globlization changes[End]

          .Column12.CONTROLSOURCE = loFormSet.lcHdrFile+".totord"
          .Column13.CONTROLSOURCE = loFormSet.lcHdrFile+".totcost"
          .Column14.CONTROLSOURCE = loFormSet.lcHdrFile+".nFcost1"
          .Column14.Header1.CAPTION = EVALUATE('loFormSet.lc'+IIF(loFormSet.lcChoice='P','I','M')+'SLbl1')
          .Column15.CONTROLSOURCE = loFormSet.lcHdrFile+".nFcost2"
	      .Column15.Header1.CAPTION = EVALUATE('loFormSet.lc'+IIF(loFormSet.lcChoice='P','I','M')+'SLbl2')
  	      .Column16.CONTROLSOURCE = loFormSet.lcHdrFile+".nFcost3"
          .Column16.Header1.CAPTION = EVALUATE('loFormSet.lc'+IIF(loFormSet.lcChoice='P','I','M')+'SLbl3')
	      .Column17.CONTROLSOURCE = loFormSet.lcHdrFile+".nFcost4"
  	      .Column17.Header1.CAPTION = EVALUATE('loFormSet.lc'+IIF(loFormSet.lcChoice='P','I','M')+'SLbl4')
	      .Column18.CONTROLSOURCE = loFormSet.lcHdrFile+".nFcost5"
	      .Column18.Header1.CAPTION = EVALUATE('loFormSet.lc'+IIF(loFormSet.lcChoice='P','I','M')+'SLbl5')
  	      .Column19.CONTROLSOURCE = loFormSet.lcHdrFile+".nFcost6"
	      .Column19.Header1.CAPTION = EVALUATE('loFormSet.lc'+IIF(loFormSet.lcChoice='P','I','M')+'SLbl6')
	      .Column20.CONTROLSOURCE = loFormSet.lcHdrFile+".nFcost7"
  	      .Column20.Header1.CAPTION = EVALUATE('loFormSet.lc'+IIF(loFormSet.lcChoice='P','I','M')+'SLbl7')
        CASE INLIST(loFormSet.lcChoice,'L','C')
          .RECORDSOURCE = loFormSet.lcHdrFile
          .Column1.CONTROLSOURCE  = loFormSet.lcHdrFile+".PO"
          *B610572,1 [T20131031.0009 Task] TMI 10/31/2013 19:32 [Start] adjust the header of the column1 according the C/T
          .Column1.Header1.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CT,loFormSet.GetHeaderText("LANG_MFGENCT_CT",loFormSet.HeaderAlias))
          *B610572,1 [T20131031.0009 Task] TMI 10/31/2013 19:32 [End  ] 
          .Column2.CONTROLSOURCE  = loFormSet.lcHdrFile+".STYLE"
          .Column2.Header1.CAPTION = ALLTRIM(gfItemMask('HM'))
          .Column2.WIDTH           = 152
          .Column3.CONTROLSOURCE  = loFormSet.lcHdrFile+".CDIVISION"
          .Column4.CONTROLSOURCE  = loFormSet.lcHdrFile+".CWARECODE"
          .Column5.CONTROLSOURCE   = loFormSet.lcHdrFile+".PATTERN"
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column5.Header1.CAPTION = LANG_MFGENCT_PATTERN
.Column5.Header1.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_PATTERN,loFormSet.GetHeaderText("LANG_MFGENCT_PATTERN",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

          .Column5.FORMAT          = '!'
          .Column5.INputMask       = REPLICATE('!',10)
          .Column5.WIDTH           = 80
          .Column6.CONTROLSOURCE   = loFormSet.lcHdrFile+".CCSTSHT_ID"
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column6.Header1.CAPTION = LANG_MFGENCT_COSTSHEET
.Column6.Header1.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_COSTSHEET,loFormSet.GetHeaderText("LANG_MFGENCT_COSTSHEET",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

          .Column6.FORMAT          = '!'
          .Column6.INputMask       = REPLICATE('!',6)
          .Column6.WIDTH           = 65
          .Column7.CONTROLSOURCE  = loFormSet.lcHdrFile+".CDEFTEMPL"
          .Column8.CONTROLSOURCE  = loFormSet.lcHdrFile+".ENTERED"
          .Column9.CONTROLSOURCE  = loFormSet.lcHdrFile+".START"
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column9.Header1.CAPTION = LANG_MFGENCT_START
.Column9.Header1.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_START,loFormSet.GetHeaderText("LANG_MFGENCT_START",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

          .Column10.CONTROLSOURCE  = loFormSet.lcHdrFile+".COMPLETE"
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column10.Header1.CAPTION = LANG_MFGENCT_COMPLETE
.Column10.Header1.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_COMPLETE,loFormSet.GetHeaderText("LANG_MFGENCT_COMPLETE",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      	  .Column11.CONTROLSOURCE = loFormSet.lcHdrFile+".nStyOrder"
	        .Column11.INPUTMASK     = "99999999999"
          *N000682,1 MMT 11/20/2012 Globalization Changes[Start]
          *	      .Column11.Header1.CAPTION = "Tot. Qty."
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*	      .Column11.Header1.CAPTION =LANG_MFGENCT_TOT_QTY
	      .Column11.Header1.CAPTION =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_TOT_QTY,loFormSet.GetHeaderText("LANG_MFGENCT_TOT_QTY",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

          *N000682,1 MMT 11/20/2012 Globalization Changes[End]
          .Column12.CONTROLSOURCE = loFormSet.lcHdrFile+".totord"
          .Column13.CONTROLSOURCE = loFormSet.lcHdrFile+".totcost"
          .Column14.CONTROLSOURCE = loFormSet.lcHdrFile+".nFcost1"
          .Column14.Header1.CAPTION = EVALUATE('loFormSet.lc'+IIF(loFormSet.lcChoice='P','I','M')+'SLbl1')
          .Column15.CONTROLSOURCE = loFormSet.lcHdrFile+".nFcost2"
	      .Column15.Header1.CAPTION = EVALUATE('loFormSet.lc'+IIF(loFormSet.lcChoice='P','I','M')+'SLbl2')
	      .Column16.CONTROLSOURCE = loFormSet.lcHdrFile+".nFcost3"
          .Column16.Header1.CAPTION = EVALUATE('loFormSet.lc'+IIF(loFormSet.lcChoice='P','I','M')+'SLbl3')
  	      .Column17.CONTROLSOURCE = loFormSet.lcHdrFile+".nFcost4"
	      .Column17.Header1.CAPTION = EVALUATE('loFormSet.lc'+IIF(loFormSet.lcChoice='P','I','M')+'SLbl4')
  	      .Column18.CONTROLSOURCE = loFormSet.lcHdrFile+".nFcost5"
	      .Column18.Header1.CAPTION = EVALUATE('loFormSet.lc'+IIF(loFormSet.lcChoice='P','I','M')+'SLbl5')
  	      .Column19.CONTROLSOURCE = loFormSet.lcHdrFile+".nFcost6"
	      .Column19.Header1.CAPTION = EVALUATE('loFormSet.lc'+IIF(loFormSet.lcChoice='P','I','M')+'SLbl6')
	      .Column20.CONTROLSOURCE = loFormSet.lcHdrFile+".nFcost7"
	      .Column20.Header1.CAPTION = EVALUATE('loFormSet.lc'+IIF(loFormSet.lcChoice='P','I','M')+'SLbl7')
      ENDCASE
      .Column4.VISIBLE = loFormSet.llWareHous
      .Column7.VISIBLE = INLIST(loFormSet.lcGenProj,'A','I')
    ENDWITH
    *B608873,1 WAM 05/25/2009 Following lines are commented out

*!*	    WITH .grdPOLines
*!*	      .RECORDSOURCE = ''
*!*	      .RECORDSOURCE = loFormSet.lcHdrFile
*!*	      .Column1.CONTROLSOURCE  = loFormSet.lcHdrFile+".PO"

*!*	      *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
*!*	      *IF loFormSet.lcChoice='P'
*!*	      IF INLIST(loFormSet.lcChoice,'P','O')
*!*	      *N000606,1 NNA (End)

*!*	        .Column2.CONTROLSOURCE = loFormSet.lcHdrFile+".VENDOR"
*!*	        .Column6.CONTROLSOURCE = loFormSet.lcHdrFile+".ShipVia"
*!*	        .Column7.CONTROLSOURCE = loFormSet.lcHdrFile+".CTERMCODE"

*!*	      ELSE
*!*	        .Column1.Header1.CAPTION = LANG_MFGENCT_CT+'#'
*!*	        .Column2.CONTROLSOURCE   = loFormSet.lcHdrFile+".STYLE"
*!*	        .Column2.Header1.CAPTION = ALLTRIM(gfItemMask('HM'))
*!*	        .Column2.WIDTH           = 152
*!*	        .Column6.CONTROLSOURCE   = loFormSet.lcHdrFile+".PATTERN"
*!*	        .Column6.Header1.CAPTION = LANG_MFGENCT_PATTERN
*!*	        .Column6.WIDTH           = 80
*!*	        .Column7.CONTROLSOURCE   = loFormSet.lcHdrFile+".CCSTSHT_ID"
*!*	        .Column7.Header1.CAPTION = LANG_MFGENCT_COSTSHEET
*!*	        .Column7.WIDTH           = 65
*!*	        *B607844,1 MMT 11/28/2006 Fix bug of not all lines of order are updated in Cutpick [Start]
*!*	*        .Column7.ENABLED = .T.
*!*	*        .Column7.READONLY = .F.
*!*	*        .Column7.Text1.READONLY = .F.
*!*	*        .Column7.Text1.ENABLED = .T.
*!*	        *B607844,1 MMT 11/28/2006 Fix bug of not all lines of order are updated in Cutpick [End]

*!*	      ENDIF
*!*	      .Column3.CONTROLSOURCE  = loFormSet.lcHdrFile+".CDIVISION"
*!*	      IF loFormSet.llWareHous
*!*	        .Column4.CONTROLSOURCE = loFormSet.lcHdrFile+".CWARECODE"
*!*	      ELSE
*!*	        .Column4.VISIBLE = .F.
*!*	      ENDIF
*!*	      *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
*!*	      *IF loFormSet.lcChoice='P'
*!*	      IF INLIST(loFormSet.lcChoice,'P','O')
*!*	      *N000606,1 NNA (END)
*!*	        .Column8.CONTROLSOURCE  = loFormSet.lcHdrFile+".Entered"
*!*	        .Column8.Header1.CAPTION  = "Entered"
*!*	      ELSE
*!*	        .Column8.CONTROLSOURCE  = loFormSet.lcHdrFile+".Start"
*!*	        .Column8.Header1.CAPTION  = "Start"
*!*	      ENDIF
*!*	      .Column9.CONTROLSOURCE  = loFormSet.lcHdrFile+".Complete"

*!*	      *N000606,1 NNA 10/06/2007 (Begin) Have my case in case of Generate PO from OTS
*!*	      *NADER
*!*	      IF loFormSet.lcChoice = 'O' THEN
*!*	       .COLUMNCOUNT = 18
*!*	        FOR I=1 TO 18
*!*	          LCI =ALLTRIM(STR(I))
*!*	          IF LCI<>"5"
*!*	            .COLUMN&lcI..CURRENTCONTROL= "TEXT1"
*!*	          ENDIF
*!*	          IF INLIST(LCI,"8","9") THEN
*!*	            .COLUMN&lcI..CURRENTCONTROL= "ARIATEXTBOX1"
*!*	          ENDIF
*!*	        ENDFOR
*!*	        .Column7.ENABLED = .F.
*!*	        .Column7.READONLY = .T.
*!*	        .Column7.Text1.READONLY = .T.
*!*	        .Column7.Text1.ENABLED = .F.
*!*	
*!*		      .Column10.CONTROLSOURCE   = loFormSet.lcHdrFile+".nStyOrder"
*!*		      .Column10.INPUTMASK       = "99999999999"
*!*		      .Column10.Header1.CAPTION = "Order"
*!*	        .Column11.CONTROLSOURCE   = loFormSet.lcHdrFile+".totord"
*!*		      .Column11.INPUTMASK       = "99999999999"
*!*		      .Column11.Header1.CAPTION = "OTS"
*!*		      .Column12.CONTROLSOURCE   = loFormSet.lcHdrFile+".totcost"
*!*		      .Column13.CONTROLSOURCE   = loFormSet.lcHdrFile+".nFcost1"
*!*		      .Column13.Header1.CAPTION = EVALUATE('loFormSet.lc'+'I'+'SLbl1')
*!*		      .Column14.CONTROLSOURCE   = loFormSet.lcHdrFile+".nFcost2"
*!*		      .Column14.Header1.CAPTION = EVALUATE('loFormSet.lc'+'I'+'SLbl2')
*!*		      .Column15.CONTROLSOURCE   = loFormSet.lcHdrFile+".nFcost3"
*!*		      .Column15.Header1.CAPTION = EVALUATE('loFormSet.lc'+'I'+'SLbl3')
*!*		      .Column16.CONTROLSOURCE   = loFormSet.lcHdrFile+".nFcost4"
*!*		      .Column16.Header1.CAPTION = EVALUATE('loFormSet.lc'+'I'+'SLbl4')
*!*		      .Column17.CONTROLSOURCE   = loFormSet.lcHdrFile+".nFcost5"
*!*		      .Column17.Header1.CAPTION = EVALUATE('loFormSet.lc'+'I'+'SLbl5')
*!*		      .Column18.CONTROLSOURCE   = loFormSet.lcHdrFile+".nFcost6"
*!*		      .Column18.Header1.CAPTION = EVALUATE('loFormSet.lc'+'I'+'SLbl6')
*!*		      .Column19.CONTROLSOURCE   = loFormSet.lcHdrFile+".nFcost7"
*!*		      .Column19.Header1.CAPTION = EVALUATE('loFormSet.lc'+'I'+'SLbl7')
*!*	      ELSE
*!*	      *N000606,1 NNA (End)
*!*		
*!*		      .Column10.CONTROLSOURCE = loFormSet.lcHdrFile+".nStyOrder"
*!*		      .Column10.INPUTMASK     = "99999999999"
*!*		      .Column10.Header1.CAPTION = "Tot. Qty."
*!*		      IF loFormSet.lcChoice = 'L'
*!*		        .Column11.VISIBLE = .F.
*!*		      ELSE
*!*		        .Column11.CONTROLSOURCE = loFormSet.lcHdrFile+".totord"
*!*		      ENDIF
*!*		      .Column12.CONTROLSOURCE = loFormSet.lcHdrFile+".totcost"
*!*		      .Column13.CONTROLSOURCE = loFormSet.lcHdrFile+".nFcost1"
*!*		      .Column13.Header1.CAPTION = EVALUATE('loFormSet.lc'+IIF(loFormSet.lcChoice='P','I','M')+'SLbl1')
*!*		      .Column14.CONTROLSOURCE = loFormSet.lcHdrFile+".nFcost2"
*!*		      .Column14.Header1.CAPTION = EVALUATE('loFormSet.lc'+IIF(loFormSet.lcChoice='P','I','M')+'SLbl2')
*!*		      .Column15.CONTROLSOURCE = loFormSet.lcHdrFile+".nFcost3"
*!*		      .Column15.Header1.CAPTION = EVALUATE('loFormSet.lc'+IIF(loFormSet.lcChoice='P','I','M')+'SLbl3')
*!*		      .Column16.CONTROLSOURCE = loFormSet.lcHdrFile+".nFcost4"
*!*		      .Column16.Header1.CAPTION = EVALUATE('loFormSet.lc'+IIF(loFormSet.lcChoice='P','I','M')+'SLbl4')
*!*		      .Column17.CONTROLSOURCE = loFormSet.lcHdrFile+".nFcost5"
*!*		      .Column17.Header1.CAPTION = EVALUATE('loFormSet.lc'+IIF(loFormSet.lcChoice='P','I','M')+'SLbl5')
*!*		      .Column18.CONTROLSOURCE = loFormSet.lcHdrFile+".nFcost6"
*!*		      .Column18.Header1.CAPTION = EVALUATE('loFormSet.lc'+IIF(loFormSet.lcChoice='P','I','M')+'SLbl6')
*!*		      .Column19.CONTROLSOURCE = loFormSet.lcHdrFile+".nFcost7"
*!*		      .Column19.Header1.CAPTION = EVALUATE('loFormSet.lc'+IIF(loFormSet.lcChoice='P','I','M')+'SLbl7')

*!*	      *N000606,1 NNA (Begin)
*!*	      ENDIF
*!*	      *N000606,1 NNA (End)
*!*	    ENDWITH
    *B608873,1 WAM 05/25/2009 (End)

  ENDWITH
  *--end of lfAfterInit.

  *!*************************************************************
  *! Name      : lfAddPro
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 11/09/2004
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
      loFormSet.ADDPROPERTY(laAllVar[lnI,1]+'['+ALLTRIM(STR(lnRow))+;
        IIF(lnCol>0,','+ALLTRIM(STR(lnCol)),'')+']')
      lcACopy = '=ACOPY(' + laAllVar[lnI,1] + ',loFormSet.' + laAllVar[lnI,1] + ')'
      &lcACopy.
    ELSE
      loFormSet.ADDPROPERTY(laAllVar[lnI,1],EVALUATE(laAllVar[lnI,1]))
    ENDIF
  ENDFOR
  *--end of lfAddPro.

  *!*************************************************************
  *! Name      : lfMfGenCt4Init
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/07/2004
  *! Purpose   : function called from the init event of form MFGENCT4.
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfMfGenCt4Init
  LPARAMETERS loFormSet

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loFormSet.AriaForm1.CAPTION = LANG_MFGENCT_PODEFAULTS
  loFormSet.AriaForm1.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_PODEFAULTS,loFormSet.GetHeaderText("LANG_MFGENCT_PODEFAULTS",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]


  loFormSet.lcVendor  = loParentForm.lcVendor
  loFormSet.lcVenName = loParentForm.lcVenName
  loFormSet.lcShip    = loParentForm.lcShip
  loFormSet.lcTerm    = loParentForm.lcTerm
  loFormSet.lcPCurr   = loParentForm.lcPCurr
  loFormSet.lnPRate   = loParentForm.lnPRate
  loFormSet.lcDCurr   = loParentForm.lcDCurr
  loFormSet.lnDRate   = loParentForm.lnDRate
  loFormSet.lnUnit1   = loParentForm.lnUnit1
  loFormSet.lnUnit2   = loParentForm.lnUnit2
  loFormSet.lcSign1   = loParentForm.lcSign1
  loFormSet.lcSign2   = loParentForm.lcSign2


  WITH loFormSet.AriaForm1
    *B608873,1 WAM 05/25/2009 Read default complate and availabe dates
    .dtPickerComplete.value   = loParentForm.ldComplete
    .dtPickerAvailable.value  = loParentForm.ldAvailable
    *B608873,1 WAM 05/25/2009 (End)
    loFormSet.lcShip = IIF(EMPTY(loFormSet.lcShip),.cboShipVia.codedefaultvalue,loFormSet.lcShip)
    loFormSet.lcTerm = IIF(EMPTY(loFormSet.lcTerm),.cboTerms.codedefaultvalue,loFormSet.lcTerm)
    .lblPriceCurrency.VISIBLE = loParentForm.llMulCurr
    .lbl4.VISIBLE             = loParentForm.llMulCurr
    .kbPriceCurrency.VISIBLE  = loParentForm.llMulCurr
    .lblPriceRate.VISIBLE     = loParentForm.llMulCurr
    .lbl5.VISIBLE             = loParentForm.llMulCurr
    .txtPriceRate.VISIBLE     = loParentForm.llMulCurr
    *N000587,1 WAM 12/01/2007 Don't read duty currency and exchange rate.
*!*	    .lblDutyCurrency.VISIBLE  = loParentForm.llMulCurr
*!*	    .lbl6.VISIBLE             = loParentForm.llMulCurr
*!*	    .kbDutyCurrency.VISIBLE   = loParentForm.llMulCurr
*!*	    .lblDutyRate.VISIBLE      = loParentForm.llMulCurr
*!*	    .lbl7.VISIBLE             = loParentForm.llMulCurr
*!*	    .txtDutyRate.VISIBLE      = loParentForm.llMulCurr
    .lblDutyCurrency.VISIBLE  = .F.
    .lbl6.VISIBLE             = .F.
    .kbDutyCurrency.VISIBLE   = .F.
    .lblDutyRate.VISIBLE      = .F.
    .lbl7.VISIBLE             = .F.
    .txtDutyRate.VISIBLE      = .F.
    *N000587,1 WAM 12/01/2007 (End)

    *--Resize the screen in case of single currency.
    LOCAL lnTopDef
    IF !loParentForm.llMulCurr

      *: B608925,1 MMT 07/05/2009 Fix bug of Object Overlapping in PO Default Screen if Not Multi. Currency Co.[Start]
      *lnTopDef       = .cboTerms.TOP  - .txtDutyRate.TOP
      lnTopDef       = .dtPickerComplete.TOP  - .txtDutyRate.TOP
      *: B608925,1 MMT 07/05/2009 Fix bug of Object Overlapping in PO Default Screen if Not Multi. Currency Co.[End]

      .shpAll.HEIGHT = .shpAll.HEIGHT + lnTopDef
      .cmdOk.TOP     = .cmdOk.TOP     + lnTopDef
      .cmdCancel.TOP = .cmdCancel.TOP + lnTopDef
      .HEIGHT        = .HEIGHT        + lnTopDef
    ENDIF
  ENDWITH
  *--end of lfMfGenCt4Init.

  *!*************************************************************
  *! Name      : lfMfGenCt3Init
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/08/2004
  *! Purpose   : function called from the init event of form MFGENCT3.
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfMfGenCt3Init
  LPARAMETERS loFormSet

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loFormSet.AriaForm1.CAPTION = LANG_MFGENCT_GENERATE
  loFormSet.AriaForm1.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_GENERATE,loFormSet.GetHeaderText("LANG_MFGENCT_GENERATE",loFormSet.HeaderAlias))
  loFormSet.AriaForm1.cboWareType.RowSource = ""+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_SINGLE,loFormSet.GetHeaderText("LANG_MFGENCT_SINGLE",loFormSet.HeaderAlias))+","+;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_MULTIPLE,loFormSet.GetHeaderText("LANG_MFGENCT_MULTIPLE",loFormSet.HeaderAlias))+""
  loFormSet.AriaForm1.cboWareType.ReQuery()
  *N000682,1 11/20/2012 MMT Globlization changes[End]


  WITH loFormSet.AriaForm1
    .lblWare.VISIBLE     = loParentForm.llWareHous
    .KbWare.VISIBLE      = loParentForm.llWareHous
    .lblWareType.VISIBLE = loParentForm.llWareHous
    .cboWareType.VISIBLE = loParentForm.llWareHous
    .cntPer.ENABLED      = !loParentForm.llRpBOPFab

    *--Resize the screen in case of single WareHouse.
    LOCAL lnTopDef
    IF !loParentForm.llWareHous
      lnTopDef         = .KbWare.TOP      - .cntPer.TOP
      .shpAll.HEIGHT   = .shpAll.HEIGHT   - lnTopDef
      .cntPer.TOP      = .cntPer.TOP      - lnTopDef
      .lblPer.TOP      = .lblPer.TOP      - lnTopDef
      .cmdGenerate.TOP = .cmdGenerate.TOP - lnTopDef
      .cmdCancel.TOP   = .cmdCancel.TOP   - lnTopDef
      .HEIGHT          = .HEIGHT          - lnTopDef
    ENDIF
  ENDWITH
  *--end of lfMfGenCt3Init.

  *!*************************************************************
  *! Name      : lfTableUpdate
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/12/2004
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
  *! Name      : lfGetCstSht
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/13/2004
  *! Purpose   : function to Get the Default cost sheet.
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfGetCstSht
  LPARAMETERS lcStyle,loFormSet

  LOCAL lcRet,lnAlias
  lcRet = ''
  lnAlias = SELECT(0)
  *! B610190,2 MMT 01/13/2013 Modify Generate PO from OTS to Handle price/size in cost sheet[Start]
  *IF loBomHeader.SEEK('0001'+PADR(SUBSTR(lcStyle,1,loFormSet.lnMajorLen),19)+IIF(loFormSet.lcChoice='P','I','M'))
  IF loBomHeader.SEEK('0001'+PADR(SUBSTR(lcStyle,1,loFormSet.lnMajorLen),19)+IIF(loFormSet.lcChoice$'OP','I','M'))  
  *! B610190,2 MMT 01/13/2013 Modify Generate PO from OTS to Handle price/size in cost sheet[END]  
    SELECT BomHeadr
    LOCATE FOR lDefCstSht

    IF FOUND()
      lcRet = cCstSht_Id
    ENDIF
  ENDIF

  SELECT (lnAlias)
  RETURN lcRet
  *--end of lfGetCstSht.

  *!*************************************************************
  *! Name      : lfMfGenCt2Init
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/19/2004
  *! Purpose   : function called from the init event of form MFGENCT2.
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfMfGenCt2Init
  LPARAMETERS loFormSet
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loFormSet.AriaForm1.CAPTION = LANG_MFGENCT_DETAILS
  loFormSet.AriaForm1.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_DETAILS,loFormSet.GetHeaderText("LANG_MFGENCT_DETAILS",loFormSet.HeaderAlias))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
  IF loParentForm.lcChoice='O' THEN
    =lfDetScr(loFormSet)
    RETURN
  ENDIF
  *N000606,1 NNA (End)

  WITH loFormSet.AriaForm1
    IF loParentForm.lcChoice <> 'L'
      LOCAL lnI,lcI
      WITH .cntDetails
        FOR lnI = 1 TO 7
          lcI = STR(lnI,1)
          .lblCost&lcI..CAPTION          = EVALUATE('loParentForm.lc'+IIF(loParentForm.lcChoice='P','I','M')+'SLbl'+lcI)
          .txtCost&lcI..CONTROLSOURCE    = loParentForm.lcLinFile+'.nFCost'+lcI
          .txtCost&lcI.Ord.CONTROLSOURCE = "lfOrdCost('"+lcI+"')"
        ENDFOR
        .txtTotalCost.CONTROLSOURCE    = "lfTotCost('')"
        .txtTotalCostOrd.CONTROLSOURCE = "lfOrdTotCost('')"
        .lblPoLine.CAPTION             = IIF(loParentForm.lcChoice='P',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_PO,loFormSet.GetHeaderText("LANG_MFGENCT_PO",loFormSet.HeaderAlias))+" "+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_LINE,loFormSet.GetHeaderText("LANG_MFGENCT_LINE",loFormSet.HeaderAlias)),;
           IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CT,loFormSet.GetHeaderText("LANG_MFGENCT_CT",loFormSet.HeaderAlias))+" "+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_LINE,loFormSet.GetHeaderText("LANG_MFGENCT_LINE",loFormSet.HeaderAlias)))
        .txtEntered.CONTROLSOURCE = 'OrdHdr.entered'
        .txtStart.CONTROLSOURCE   = 'OrdHdr.Start'
        .txtComplete.CONTROLSOURCE = 'OrdHdr.Complete'
      ENDWITH
      .cmdClose.VISIBLE = .F.
    ENDIF
    LOCAL lnHeight
    lnHeight = .cntPoLIne.TOP - .cntHeader.TOP
    lnDiff = .cntPoLIne.TOP +.cntPoLIne.HEIGHT - .grdPOLine.TOP-.grdPOLine.HEIGHT
    DO CASE
      CASE loParentForm.lcChoice = 'C'
        WITH .cntPoLIne
          .TOP    = .TOP - lnHeight
          .HEIGHT = .HEIGHT + lnHeight
          .AriaShape1.HEIGHT = .AriaShape1.HEIGHT + lnHeight
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
          *.AriaTitleLabel1.CAPTION = LANG_MFGENCT_DTLCTORDERS
         .AriaTitleLabel1.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_DTLCTORDERS,loFormSet.GetHeaderText("LANG_MFGENCT_DTLCTORDERS",loFormSet.HeaderAlias))
         *N000682,1 11/20/2012 MMT Globlization changes[End]

        ENDWITH
        .cntHeader.VISIBLE = .F.
        WITH .grdPOLine
          .TOP    = .TOP - lnHeight
          .HEIGHT = .HEIGHT + lnHeight
        ENDWITH
      CASE loParentForm.lcChoice = 'L'
        .cntHeader.VISIBLE = .F.
        .cntDetails.VISIBLE = .F.
        .cntPoLIne.TOP = .cntHeader.TOP
        .cntPoLIne.HEIGHT = .cmdClose.TOP - .cntHeader.TOP
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.cntPoLIne.AriaTitleLabel1.CAPTION = LANG_MFGENCT_DTLCTORDERS
.cntPoLIne.AriaTitleLabel1.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_DTLCTORDERS,loFormSet.GetHeaderText("LANG_MFGENCT_DTLCTORDERS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

        .grdPOLine.TOP = .grdPOLine.TOP - lnHeight
        .grdPOLine.HEIGHT = .cntPoLIne.TOP + .cntPoLIne.HEIGHT - .grdPOLine.TOP - lnDiff
        .grdPOLine.ROWHEIGHT = 17
    ENDCASE
    WITH .grdPOLine
      .RECORDSOURCE             = loParentForm.lcPOLine
      .Column2.CONTROLSOURCE    = loParentForm.lcPOLine+'.Style'
      .Column3.CONTROLSOURCE    = loParentForm.lcPOLine+'.Dyelot'
      .Column4.CONTROLSOURCE    = loParentForm.lcPOLine+'.Qty1'
      .Column5.CONTROLSOURCE    = loParentForm.lcPOLine+'.Qty2'
      .Column6.CONTROLSOURCE    = loParentForm.lcPOLine+'.Qty3'
      .Column7.CONTROLSOURCE    = loParentForm.lcPOLine+'.Qty4'
      .Column8.CONTROLSOURCE    = loParentForm.lcPOLine+'.Qty5'
      .Column9.CONTROLSOURCE    = loParentForm.lcPOLine+'.Qty6'
      .Column10.CONTROLSOURCE   = loParentForm.lcPOLine+'.Qty7'
      .Column11.CONTROLSOURCE   = loParentForm.lcPOLine+'.Qty8'
      .Column12.CONTROLSOURCE   = loParentForm.lcPOLine+'.TotQty'
      .Column1.CONTROLSOURCE    = loParentForm.lcPOLine+'.nFCost1'
      .Column13.CONTROLSOURCE   = loParentForm.lcPOLine+'.nFCost2'
      .Column14.CONTROLSOURCE   = loParentForm.lcPOLine+'.nFCost3'
      .Column15.CONTROLSOURCE   = loParentForm.lcPOLine+'.nFCost4'
      .Column16.CONTROLSOURCE   = loParentForm.lcPOLine+'.nFCost5'
      .Column17.CONTROLSOURCE   = loParentForm.lcPOLine+'.nFCost6'
      .Column18.CONTROLSOURCE   = loParentForm.lcPOLine+'.nFCost7'
      .Column19.CONTROLSOURCE   = "lfTotCost('')"
      .Column20.CONTROLSOURCE   = loParentForm.lcPOLine+'.nSelPrice'
      .Column21.CONTROLSOURCE   = loParentForm.lcPOLine+'.nGrosMrgn'
      .Column22.CONTROLSOURCE   = loParentForm.lcPOLine+'.cCstSht_ID'
      .Column23.CONTROLSOURCE   = loParentForm.lcPOLine+'.Complete'
      .Column24.CONTROLSOURCE   = loParentForm.lcPOLine+'.ShipVia'
      .Column3.VISIBLE          = loParentForm.llDyelot
      .Column22.VISIBLE         = (loParentForm.lcChoice='P')
      .Column24.VISIBLE         = (loParentForm.lcChoice='P')
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column3.Header1.CAPTION  = IIF(loParentForm.llStyConfg,LANG_MFGENCT_CONFIG,LANG_MFGENCT_DYELOT)
.Column3.Header1.CAPTION  = IIF(loParentForm.llStyConfg,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CONFIG,loFormSet.GetHeaderText("LANG_MFGENCT_CONFIG",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_DYELOT,loFormSet.GetHeaderText("LANG_MFGENCT_DYELOT",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      .Column1.Header1.CAPTION  = EVALUATE('loParentForm.lc'+IIF(loParentForm.lcChoice='P','I','M')+'SLbl1')
      .Column13.Header1.CAPTION = EVALUATE('loParentForm.lc'+IIF(loParentForm.lcChoice='P','I','M')+'SLbl2')
      .Column14.Header1.CAPTION = EVALUATE('loParentForm.lc'+IIF(loParentForm.lcChoice='P','I','M')+'SLbl3')
      .Column15.Header1.CAPTION = EVALUATE('loParentForm.lc'+IIF(loParentForm.lcChoice='P','I','M')+'SLbl4')
      .Column16.Header1.CAPTION = EVALUATE('loParentForm.lc'+IIF(loParentForm.lcChoice='P','I','M')+'SLbl5')
      .Column17.Header1.CAPTION = EVALUATE('loParentForm.lc'+IIF(loParentForm.lcChoice='P','I','M')+'SLbl6')
      .Column18.Header1.CAPTION = EVALUATE('loParentForm.lc'+IIF(loParentForm.lcChoice='P','I','M')+'SLbl7')
      .Column20.VISIBLE         = loParentForm.llDispPric
      .Column21.VISIBLE         = loParentForm.llDispPric
      *! B609028,1 MMT 10/08/2009 Fix bug of can not edit p.price in line detail screen[Start]
      IF loParentForm.lcChoice = 'P'
        .Column25.VISIBLE         = .T.
        .Column26.VISIBLE         = .T.
        .Column27.VISIBLE         = .T.
        .Column25.CONTROLSOURCE   = loParentForm.lcPOLine+'.Gros_Price'
        .Column26.CONTROLSOURCE   = loParentForm.lcPOLine+'.Disc_Pcnt'
        .Column27.CONTROLSOURCE   = loParentForm.lcPOLine+'.nNetPrc'
        .Column25.Readonly = .F.
        .Column26.Readonly = .F.
        .Column27.Readonly = .F.
      ELSE
        .Column25.VISIBLE         = .F.
        .Column26.VISIBLE         = .F.
        .Column27.VISIBLE         = .F.
      ENDIF
      *! B609028,1 MMT 10/08/2009 Fix bug of can not edit p.price in line detail screen[End]

      *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[Start]
      IF loParentForm.lcChoice $ 'PCO' AND INLIST(loParentForm.lcGenProj,'A','I') AND loParentForm.LLRPGNLPJ
        .Column28.VISIBLE         = .T.
        .Column28.CONTROLSOURCE   = loParentForm.lcPOLine+'.CDEFTEMPL'
        .Column28.Readonly = .F.
      ELSE
        .Column28.VISIBLE         = .F.
      ENDIF
      *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[End]


    ENDWITH
    IF loParentForm.lcChoice <> 'L'
      WITH .cntDetails.grdOrdLine
        .RECORDSOURCE             = loParentForm.lcCutPick
        .Column2.CONTROLSOURCE    = loParentForm.lcCutPick+'.Order'
        .Column4.CONTROLSOURCE    = loParentForm.lcCutPick+'.Qty1'
        .Column5.CONTROLSOURCE    = loParentForm.lcCutPick+'.Qty2'
        .Column6.CONTROLSOURCE    = loParentForm.lcCutPick+'.Qty3'
        .Column7.CONTROLSOURCE    = loParentForm.lcCutPick+'.Qty4'
        .Column8.CONTROLSOURCE    = loParentForm.lcCutPick+'.Qty5'
        .Column9.CONTROLSOURCE    = loParentForm.lcCutPick+'.Qty6'
        .Column10.CONTROLSOURCE   = loParentForm.lcCutPick+'.Qty7'
        .Column11.CONTROLSOURCE   = loParentForm.lcCutPick+'.Qty8'
        .Column12.CONTROLSOURCE   = loParentForm.lcCutPick+'.TotQty'
        .Column1.CONTROLSOURCE    = "lfOrdCost('1')"
        .Column13.CONTROLSOURCE   = "lfOrdCost('2')"
        .Column14.CONTROLSOURCE   = "lfOrdCost('3')"
        .Column15.CONTROLSOURCE   = "lfOrdCost('4')"
        .Column16.CONTROLSOURCE   = "lfOrdCost('5')"
        .Column17.CONTROLSOURCE   = "lfOrdCost('6')"
        .Column18.CONTROLSOURCE   = "lfOrdCost('7')"
        .Column19.CONTROLSOURCE   = "lfOrdTotCost('')"
        .Column1.Header1.CAPTION  = EVALUATE('loParentForm.lc'+IIF(loParentForm.lcChoice='P','I','M')+'SLbl1')
        .Column13.Header1.CAPTION = EVALUATE('loParentForm.lc'+IIF(loParentForm.lcChoice='P','I','M')+'SLbl2')
        .Column14.Header1.CAPTION = EVALUATE('loParentForm.lc'+IIF(loParentForm.lcChoice='P','I','M')+'SLbl3')
        .Column15.Header1.CAPTION = EVALUATE('loParentForm.lc'+IIF(loParentForm.lcChoice='P','I','M')+'SLbl4')
        .Column16.Header1.CAPTION = EVALUATE('loParentForm.lc'+IIF(loParentForm.lcChoice='P','I','M')+'SLbl5')
        .Column17.Header1.CAPTION = EVALUATE('loParentForm.lc'+IIF(loParentForm.lcChoice='P','I','M')+'SLbl6')
        .Column18.Header1.CAPTION = EVALUATE('loParentForm.lc'+IIF(loParentForm.lcChoice='P','I','M')+'SLbl7')
      ENDWITH
    ENDIF
  ENDWITH
  *--end of lfMfGenCt2Init.
  *!*************************************************************
  *! Name      : lfOrdCost
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/19/2004
  *! Purpose   : Get the order line cost.
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfOrdCost
  LPARAMETERS lcCnt
  *N000606,1 NNA 10/06/2007 (Begin) Add the OTS option to lcChoice
  IF loParentForm.lcChoice='O' THEN
    =loParentForm.loStyle.SEEK(EVALUATE(loParentForm.lcLinFile+'.Style'))
    RETURN EVALUATE(loParentForm.lcLinFile+'.TotOrd*STYLE.nICost'+lcCnt)
  ELSE
  *N000606,1 NNA (END)

    RETURN EVALUATE(loParentForm.lcCutPick+'.TotQty*STYLE.nICost'+lcCnt)

  *N000606,1 NNA (BEGIN)
  ENDIF
  *N000606,1 NNA (END)

  *--end of lfOrdCost.
  *!*************************************************************
  *! Name      : lfOrdTotCost
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/19/2004
  *! Purpose   : Get the order line total cost.
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfOrdTotCost
  LPARAMETERS lcPara

  RETURN lfOrdCost('1')+lfOrdCost('2')+lfOrdCost('3')+lfOrdCost('4')+lfOrdCost('5')+lfOrdCost('6')+lfOrdCost('7')
  *--end of lfOrdTotCost.

  *!*************************************************************
  *! Name      : lfTotCost
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/19/2004
  *! Purpose   : Get the PO line total cost.
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfTotCost
  LPARAMETERS lcPara

  RETURN EVALUATE(loParentForm.lcLinFile+'.nFCost1+'+loParentForm.lcLinFile+'.nFCost2+'+;
    loParentForm.lcLinFile+'.nFCost3+'+loParentForm.lcLinFile+'.nFCost4+'+;
    loParentForm.lcLinFile+'.nFCost5+'+loParentForm.lcLinFile+'.nFCost6+'+loParentForm.lcLinFile+'.nFCost7')
  *--end of lfTotCost.

  *!*************************************************************
  *! Name      : lfvCstSht
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/19/2004
  *! Purpose   : Validate the cost sheet ID for PO lines.
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfvCstSht

  IF EVALUATE(loParentForm.lcLinFile+'.cCstSht_ID') = loParentForm.lnOldVal
    RETURN
  ENDIF

  LOCAL lcCstShtType, lcTmpBomHd, loTmpBomHd, lcTmpBom, loTmpBom
  LOCAL lnI, lcTmpPoLn, laOldECst, laNewECst, laOldFCst, laNewFCst
  PRIVATE lcPMethod,lcDMethod,lcPUnMeth,lcDUnMeth,lnCurrUnt1, lnCurrUnt2
  DIMENSION laOldECst[7], laNewECst[7], laOldFCst[7], laNewFCst[7]
  STORE 0  TO laOldECst, laNewECst, laOldFCst, laNewFCst
  STORE '' TO lcPMethod,lcDMethod,lcPUnMeth,lcDUnMeth
  STORE 1  TO lnCurrUnt1, lnCurrUnt2

  *-- Get the style cost sheet type. "I" for Style PO, "M" for C/T.
  lcCstShtType = IIF(loParentForm.lcChoice = 'P','I','M')
  *-- Get the selected price currency and rate.
  lcPriceCurr  = loParentForm.lcPCurr
  lnPricRate   = loParentForm.lnPRate
  *-- Get the selected duty currency and rate.
  lcDutyCurr   = loParentForm.lcDCurr
  lnDutyRate   = loParentForm.lnDRate
  *-- Style major
  lcStyMajr    = SUBSTR(EVALUATE(loParentForm.lcLinFile+'.Style'),1,loParentForm.lnMajorLen)
  lcInvType    = '0001'
  *-- Temporary PO Line
  lcTmpPoLn    = loParentForm.lcLinFile

  *-- To hold the bomheader file
  lcTmpBomHd   = gfTempName()
  *-- Get the style cost sheets for the selected style
  loTmpBomHd   = CREATEOBJECT('RemoteTable','BomHeadr','BomHeadr',lcTmpBomHd,loParentForm.DATASESSIONID)
  loTmpBomHd.SEEK(lcInvType+PADR(lcStyMajr,19)+lcCstShtType)

  lcTmpBom     = gfTempName()
  loTmpBom     = CREATEOBJECT('RemoteTable','Bom','MULTIBOM',lcTmpBom,loParentForm.DATASESSIONID)

  *-- Index cItmMajor+cCstShtTyp+cCstSht_Id
  IF !SEEK(lcInvType+PADR(lcStyMajr,19)+lcCstShtType+EVALUATE(lcTmpPoLn+'.cCstSht_ID'),lcTmpBomHd)
    LOCAL lcCstShtID
    lcCstShtID = ''
    SELECT (lcTmpBomHd)

    LOCAL lcBrowChr
    lcCstShtID = RTRIM(EVALUATE(lcTmpPoLn+'.cCstSht_ID'))
    lcBrowChr  = RIGHT(lcCstShtID,1)
    lcCstShtID = IIF(lcBrowChr=='?',SUBSTR(lcCstShtID,1,LEN(lcCstShtID)-1),lcCstShtID)
    =SEEK(lcInvType+PADR(lcStyMajr,19)+lcCstShtType+lcCstShtID)

    IF gfCstShtBrow('',IIF(loParentForm.lcChoice = 'P',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_IMPSTYCOST,loFormSet.GetHeaderText("LANG_MFGENCT_IMPSTYCOST",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_MFGSTYCOST,loFormSet.GetHeaderText("LANG_MFGENCT_MFGSTYCOST",loFormSet.HeaderAlias))),;
        @lcCstShtID,lcTmpBomHd,1)
      SELECT (lcTmpPoLn)
      REPLACE cCstSht_Id WITH lcCstShtID
    ELSE
      SELECT (lcTmpPoLn)
      REPLACE cCstSht_Id WITH loParentForm.lnOldVal
      RETURN
    ENDIF
  ENDIF

  IF !EMPTY(EVALUATE(lcTmpPoLn+'.cCstSht_ID')) AND EVALUATE(lcTmpPoLn+'.cCstSht_ID') = loParentForm.lnOldVal
    RETURN
  ENDIF

  SELECT (lcTmpPoLn)
  SCATTER MEMVAR

  *-- Informing message in case of cost sheet is hold, cancelled or in work
  lcStatus = " "
  DO CASE
    CASE EVALUATE(lcTmpBomHd+'.cStatus') = "H"
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcStatus = LANG_MFGENCT_HOLD
lcStatus = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_HOLD,loFormSet.GetHeaderText("LANG_MFGENCT_HOLD",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    CASE EVALUATE(lcTmpBomHd+'.cStatus') = "X"
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcStatus = LANG_MFGENCT_CANCELLED
lcStatus = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CANCELLED,loFormSet.GetHeaderText("LANG_MFGENCT_CANCELLED",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    CASE EVALUATE(lcTmpBomHd+'.cStatus') = "W"
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
      *lcStatus = LANG_MFGENCT_INWORK
      lcStatus = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_INWORK,loFormSet.GetHeaderText("LANG_MFGENCT_INWORK",loFormSet.HeaderAlias))
      *N000682,1 11/20/2012 MMT Globlization changes[End]

  ENDCASE
  IF EVALUATE(lcTmpBomHd+'.cStatus') $ "H|X|W"
    *-Informing: Cost sheet status is 'hold', 'cancelled'.
    =gfModalGen('INM34185B34000','DIALOG',lcStatus)
  ENDIF

  *-- Get the old estimated and foreign costs
  FOR lnI = 1 TO 7
    lcI = STR(lnI,1)
    STORE EVALUATE('m.nICost'+lcI) TO laOldECst[lnI]
    STORE EVALUATE('m.nFCost'+lcI) TO laOldFCst[lnI]
  ENDFOR

  loTmpBom.SEEK(lcInvType+PADR(lcStyMajr,19)+lcCstShtType+m.cCstSht_Id)
  LOCAL ARRAY laCost[7]
  STORE 0 TO laCost
  *-- Calculate the cost for style/color.
  loParentForm.AriaForm1.mfCSTSC.mgetstyclrcst(m.STYLE,STYLE.SCALE,gfGetMemVar('M_USEEXSSC'),lcTmpBom,'',@laCost)

  *-- Put the foreign costs in memory variables to be saved in the temporary file.
  m.nFCost1 = IIF(!loParentForm.llMulCurr OR STYLE.cPriceCur=lcPriceCurr,laCost[1]*m.TotQty,0)
  m.nFCost2 = IIF(!loParentForm.llMulCurr OR STYLE.cDutyCur =lcDutyCurr,laCost[2]*m.TotQty,0)
  m.nFCost3 = IIF(!loParentForm.llMulCurr OR STYLE.cDutyCur =lcDutyCurr,laCost[3]*m.TotQty,0)
  m.nFCost4 = IIF(!loParentForm.llMulCurr OR STYLE.cDutyCur =lcDutyCurr,laCost[4]*m.TotQty,0)
  m.nFCost5 = IIF(!loParentForm.llMulCurr OR STYLE.cDutyCur =lcDutyCurr,laCost[5]*m.TotQty,0)
  m.nFCost6 = IIF(!loParentForm.llMulCurr OR STYLE.cDutyCur =lcDutyCurr,laCost[6]*m.TotQty,0)
  m.nFCost7 = IIF(!loParentForm.llMulCurr OR STYLE.cDutyCur =lcDutyCurr,laCost[7]*m.TotQty,0)
  m.Disc_Pcnt  = 0
  m.Gros_Price = IIF(!loParentForm.llMulCurr OR STYLE.cPriceCur=lcPriceCurr,laCost[1],0)

  *--Read equivalent costs in base currency.
  m.nICost1 = lfEquivCost('1',m.nFCost1,lnPricRate,lnCurrUnt1,lcPriceCurr,lcDutyCurr)
  m.nICost2 = lfEquivCost('2',m.nFCost2,lnDutyRate,lnCurrUnt2,lcPriceCurr,lcDutyCurr)
  m.nICost3 = lfEquivCost('3',m.nFCost3,lnDutyRate,lnCurrUnt2,lcPriceCurr,lcDutyCurr)
  m.nICost4 = lfEquivCost('4',m.nFCost4,lnDutyRate,lnCurrUnt2,lcPriceCurr,lcDutyCurr)
  m.nICost5 = lfEquivCost('5',m.nFCost5,lnDutyRate,lnCurrUnt2,lcPriceCurr,lcDutyCurr)
  m.nICost6 = lfEquivCost('6',m.nFCost6,lnDutyRate,lnCurrUnt2,lcPriceCurr,lcDutyCurr)
  m.nICost7 = lfEquivCost('7',m.nFCost7,lnDutyRate,lnCurrUnt2,lcPriceCurr,lcDutyCurr)

  *-- Temporary PO line file
  SELECT (lcTmpPoLn)
  GATHER MEMVAR

  *-- Get the new estimated and foreign costs
  FOR lnI = 1 TO 7
    lcI = STR(lnI,1)
    STORE EVALUATE('m.nICost'+lcI) TO laNewECst[lnI]
    STORE EVALUATE('m.nFCost'+lcI) TO laNewFCst[lnI]
  ENDFOR
  *-- Calculate the estimated costs in the summary folder
  *-- Pass the old estimated cost, new estimated cost
  *-- old esitmted foreign cost, new estimated foreign cost as an arrays by reference
  *-- Get the multi currency set up
  =lfCalEstCst(@laOldECst,@laNewECst,@laOldFCst,@laNewFCst,loParentForm.llMulCurr,.T.,lcPriceCurr,lnPricRate,lcDutyCurr)
  loTmpBomHd.DESTROY
  loTmpBom.DESTROY
  *--end of lfvCstSht.

  *!*************************************************************
  *! Name      : lfEquivCost
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/19/2004
  *! Purpose   : Get the Equivlant cost.
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfEquivCost
  LPARAMETERS lcCstNo,lnFrnCost,lnCurRate,lnCurUnt, lcPricCurr, lcDutyCurr

  lnCstType = EVALUATE('loParentForm.lcIType'+lcCstNo)
  IF lnCstType $ 'PMD'
    IF lnCstType='P'
      lcPMethod = gfGetExSin(@lcPUnMeth,lcPricCurr)
      lcPMethod = IIF(EMPTY(lcPMethod),'*',lcPMethod)
      lcPUnMeth = IIF(EMPTY(lcPUnMeth),'/',lcPUnMeth)
      lnEquCost = lnFrnCost &lcPMethod lnCurRate &lcPUnMeth lnCurUnt
    ELSE
      lcDMethod = gfGetExSin(@lcDUnMeth,lcDutyCurr)
      lcDMethod = IIF(EMPTY(lcDMethod),'*',lcDMethod)
      lcDUnMeth = IIF(EMPTY(lcDUnMeth),'/',lcDUnMeth)
      lnEquCost = lnFrnCost &lcDMethod lnCurRate &lcDUnMeth lnCurUnt
    ENDIF
  ELSE
    lnEquCost = lnFrnCost
  ENDIF
  lnEquCost=ROUND(lnEquCost,3)
  RETURN (lnEquCost)
  *--end of lfEquivCost.

  *!*************************************************************
  *! Name      : lfCalEstCst
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/19/2004
  *! Purpose   : Get the PosHdr cost.
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfCalEstCst
  LPARAMETERS laEOldCst,laENewCst,laFOldEst,laFNewEst,llMultiCurr,llEquvCst1,lcPriceCurr,lnPricRate,lcDutyCurr

  LOCAL lcTmpPoHdr
  STORE ''  TO lcPMethod,lcDMethod,lcPUnMeth,lcDUnMeth
  STORE 1 TO lnCurrUnt1, lnCurrUnt2
  lcTmpPoHdr  = loParentForm.lcHdrFile

  SELECT (lcTmpPoHdr)
  *-- Estimated costs
  REPLACE nICost2   WITH nICost2   - laEOldCst[2] + laENewCst[2],;
    nICost3   WITH nICost3   - laEOldCst[3] + laENewCst[3],;
    nICost4   WITH nICost4   - laEOldCst[4] + laENewCst[4],;
    nICost5   WITH nICost5   - laEOldCst[5] + laENewCst[5],;
    nICost6   WITH nICost6   - laEOldCst[6] + laENewCst[6],;
    nICost7   WITH nICost7   - laEOldCst[7] + laENewCst[7]
  *-- Foreign Costs
  REPLACE nFCost1   WITH nFCost1 - laFOldEst[1] + laFNewEst[1],;
    nFCost2   WITH nFCost2 - laFOldEst[2] + laFNewEst[2],;
    nFCost3   WITH nFCost3 - laFOldEst[3] + laFNewEst[3],;
    nFCost4   WITH nFCost4 - laFOldEst[4] + laFNewEst[4],;
    nFCost5   WITH nFCost5 - laFOldEst[5] + laFNewEst[5],;
    nFCost6   WITH nFCost6 - laFOldEst[6] + laFNewEst[6],;
    nFCost7   WITH nFCost7 - laFOldEst[7] + laFNewEst[7]

  IF llEquvCst1
    IF llMultiCurr
      *-- Get the equivelant Purchase Price
      REPLACE nICost1 WITH lfEquivCost('1',nFCost1,lnPricRate,lnCurrUnt1,lcPriceCurr,lcDutyCurr)
    ELSE
      REPLACE nICost1 WITH nFCost1
    ENDIF
    REPLACE POTotal   WITH nICost1+nICost2+nICost3+nICost4+nICost5+nICost6+nICost7
  ENDIF
  *--end of lfCalEstCst.

  *!*************************************************************
  *! Name      : lfvComplete
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/21/2004
  *! Purpose   : Validate the complete date of PosLn.
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfvComplete

  SELECT (loParentForm.lcLinFile)
  IF EMPTY(COMPLETE)
    *-You cannot leave the Completion date empty.
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM34005B34000','DIALOG',LANG_MFGENCT_COMPLETION)
=gfModalGen('INM34005B34000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_COMPLETION,loFormSet.GetHeaderText("LANG_MFGENCT_COMPLETION",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    REPLACE COMPLETE WITH loParentForm.lnOldVal
    RETURN 0
  ELSE
    IF EVALUATE(loParentForm.lcHdrFile+'.Entered') > COMPLETE
      *-Completion date cannot be prior to entered date.
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM34003B34000','DIALOG',LANG_MFGENCT_COMPLETION)
=gfModalGen('INM34003B34000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_COMPLETION,loFormSet.GetHeaderText("LANG_MFGENCT_COMPLETION",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      REPLACE COMPLETE WITH loParentForm.lnOldVal
      RETURN 0
    ENDIF
  ENDIF
  *--end of lfvComplete.

  *!*************************************************************
  *! Name      : lfMfFabPriInit
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/21/2004
  *! Purpose   : function called from the init event of form MFFABPRI.
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfMfFabPriInit
  LPARAMETERS loFormSet

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.AriaForm1.CAPTION = LANG_MFGENCT_FABRICS
loFormSet.AriaForm1.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_FABRICS,loFormSet.GetHeaderText("LANG_MFGENCT_FABRICS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.AriaForm1.lblList.CAPTION = lcText
  *--end of lfMfFabPriInit.

  *!*************************************************************
  *! Name      : lfMfGenCt5Init
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/26/2004
  *! Purpose   : function called from the init event of form MFGENCT5.
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfMfGenCt5Init
  LPARAMETERS loFormSet

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.AriaForm1.CAPTION = LANG_MFGENCT_SOALLO
loFormSet.AriaForm1.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_SOALLO,loFormSet.GetHeaderText("LANG_MFGENCT_SOALLO",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]


  WITH loFormSet.AriaForm1
    .cmdAllo.CAPTION = lcPrompt
    .cmdAllo.ENABLED = llAllStat

    WITH .grdFabrics
      .RECORDSOURCE             = loParentForm.lcFabrics
      .Column1.CONTROLSOURCE    = loParentForm.lcFabrics+'.Fabric'
      .Column2.CONTROLSOURCE    = loParentForm.lcFabrics+'.CWARECODE'
      .Column3.CONTROLSOURCE    = loParentForm.lcFabrics+'.DYELOT'
      .Column4.CONTROLSOURCE    = loParentForm.lcFabrics+'.ONHAND'
      .Column5.CONTROLSOURCE    = loParentForm.lcFabrics+'.nAllocated'
      .Column6.CONTROLSOURCE    = "lfBalance('')"
      .Column2.VISIBLE          = loParentForm.llWareHous
      .Column3.VISIBLE          = loParentForm.llDyelot
    ENDWITH

    WITH .grdOrdLine
      .RECORDSOURCE             = loParentForm.lcTmpOrd
      .Column1.CONTROLSOURCE    = loParentForm.lcTmpOrd+'.lSelect'
      .Column2.CONTROLSOURCE    = loParentForm.lcTmpOrd+'.Order'
      .Column3.CONTROLSOURCE    = loParentForm.lcTmpOrd+'.Account'
      .Column4.CONTROLSOURCE    = loParentForm.lcTmpOrd+'.StName'
      .Column5.CONTROLSOURCE    = loParentForm.lcTmpOrd+'.Store'
      .Column6.CONTROLSOURCE    = loParentForm.lcTmpOrd+'.Style'
      .Column7.CONTROLSOURCE    = 'lfGetPcs(loParentForm)'
      .Column8.CONTROLSOURCE    = loParentForm.lcTmpOrd+'.nRequired'
      .Column9.CONTROLSOURCE    = "lfUnAllo('')"

      IF loParentForm.llRPUnAWip
        FOR lnI = 4 TO 9
          lcI = STR(lnI,1)
          .COLUMN&lcI..COLUMNORDER = lnI - 1
        ENDFOR
      ELSE
        .Column9.VISIBLE = .F.
      ENDIF
    ENDWITH
  ENDWITH
  *--end of lfMfGenCt5Init.

  *!*************************************************************
  *! Name      : lfBalance
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/27/2004
  *! Purpose   : Get the Balance of Fabric.
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfBalance
  LPARAMETERS lcDummy

  RETURN EVALUATE(loParentForm.lcFabrics+'.Onhand-'+loParentForm.lcFabrics+'.nAllocated')
  *--end of lfBalance.

  *!*************************************************************
  *! Name      : lfUnAllo
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 12/27/2004
  *! Purpose   : Get the UnAllocated Qty.
  *!*************************************************************
  *! Parameters: None
  *!*************************************************************
  *! Returns   : None
  *!*************************************************************
FUNCTION lfUnAllo
  LPARAMETERS lcDummy

  =SEEK(EVALUATE(loParentForm.lcTmpOrd+'.Style+'+loParentForm.lcFabrics+'.Dyelot'),loParentForm.lcStyUnAll)
  RETURN EVALUATE(loParentForm.lcStyUnAll+'.nUnAllWIP')
  *--end of lfUnAllo.

  *!*************************************************************
  *! Name      : lfSRCust
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 11/30/2004
  *! Purpose   : Customer In Range
  *!*************************************************************
FUNCTION lfSRCust
  LPARAMETERS lcParm

  DO CASE
    CASE lcParm = 'S'
      SELECT CUSTOMER
      SET ORDER TO CUSTOMER
      GO TOP
    CASE lcParm = 'R'
      *--
  ENDCASE
  *-- end of lfSRCust.

  *!*************************************************************
  *! Name      : lfEsc
  *! Developer : AHMED MAHER (AMH)
  *! Date      : 04/13/2005
  *! Purpose   : Reset date field to its old value.
  *!*************************************************************
FUNCTION lfEsc
  LPARAMETERS loColumn

  IF loColumn.VALUE = CTOD(loColumn.TAG)
    ON KEY LABEL ESC
    KEYBOARD '{ESC}'
  ELSE
    loColumn.VALUE = CTOD(loColumn.TAG)
  ENDIF
  *--end of lfEsc.

FUNCTION lfvBasedOn
  CLEARREAD()

  *!*************************************************************
  *! Name      : lfvWeek
  *: Developer : Wael Ali MOhamed
  *: DATE      : 06/30/2005
  *! Purpose   : GEt Week start date
  *!*************************************************************
  *! Calls     : None.
  *!*************************************************************
  *! Parameters: lcYear, lnWeek
  *!*************************************************************
  *! Returns   : None.
  *!*************************************************************
FUNCTION lfvWeek
  LPARAMETERS lcYear, lnWeek

  ldDate  = CTOD('01/01/'+lcYear)
  ldEndDate   = GOMONTH(ldDate,12) - 1
  No_Of_weeks = MAX(CEILING(((ldEndDate-DOW(ldEndDate)) - (ldDate-DOW(ldDate)+1))/7),1)
  St_Day      = ldDate-DOW(ldDate)+1

  IF lnWeek = 1
    ldDispDate = St_Day
  ELSE
    ldDispDate = ((lnWeek-1)*7) + St_Day
  ENDIF
  RETURN ldDispDate

FUNCTION lfwoption


  *!*************************************************************
  *! Name      : lfCstBrow
  *: Developer : Mariam Mazhar
  *: DATE      : 11/28/2006
  *! Purpose   : Browse Cost Sheet
  *B607844
  *!*************************************************************
FUNCTION lfCstBrow
  PARAMETERS loFormsetPar,lcValue

  IF loFormsetPar.lcChoice <> 'C'
    RETURN 0
  ELSE
    PRIVATE loBomHead
    lcTempBomHdr = gfTempName()
    loBomHead = CREATEOBJECT('RemoteTable','BomHeadr','BomHeadr',lcTempBomHdr,loFormsetPar.DATASESSIONID)

    lcCstShtID   = lcValue
    lcStyMajor   = SUBSTR(EVALUATE(loFormsetPar.lcHdrFile+'.STYLE'),1,loFormsetPar.lnMajorLen)
    lcCstShtType = "M"
    lcMaj = EVALUATE(loFormsetPar.lcHdrFile+'.STYLE')

    IF !loBomHead.SEEK('0001'+PADR(lcStyMajor,19)+lcCstShtType+PADR(lcCstShtID,6))
      loBomHead.SQLRUN("Select * from BOMHEADR where cItmMajor = '"+lcMaj+"' and cCstShtTyp = 'M' AND cCstSht_Id <>'" + "      "+"'")
      IF gfCstShtBrow(['0001'+lcMaj],'Style Cost Sheets',@lcCstShtID,lcTempBomHdr,1)
        IF INLIST(&lcTempBomHdr..cStatus,'H','X','W')
          *-- Message : 34185
          *-- Cost sheet status is 'hold', 'cancelled'.
          *-- Button : 00000
          *-- < Ok
          *N000682,1 MMT 12/09/2012 Globalization changes[Start]
          *=gfModalGen('INM34185B34000','DIALOG',ICASE(&lcTempBomHdr..cStatus = "H","hold",&lcTempBomHdr..cStatus = "X", "cancelled",&lcTempBomHdr..cStatus = "W","in work")          
          *
          =gfModalGen('INM34185B34000','DIALOG',ICASE(&lcTempBomHdr..cStatus = "H",;
          IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_HOLD,loFormsetPar.GetHeaderText("LANG_MFGENCT_HOLD",loFormsetPar.HeaderAlias)),;
          &lcTempBomHdr..cStatus = "X",;
           IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CANCELLED,loFormsetPar.GetHeaderText("LANG_MFGENCT_CANCELLED",loFormsetPar.HeaderAlias)),&lcTempBomHdr..cStatus = "W",;
           IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_INWORK,loFormsetPar.GetHeaderText("LANG_MFGENCT_INWORK",loFormsetPar.HeaderAlias)))
          *N000682,1 MMT 12/09/2012 Globalization changes[END]
          PRIVATE loBomHeader
          loBomHeader = CREATEOBJECT('RemoteTable','BomHeadr','BomHeadr','BomHeadr',loFormsetPar.DATASESSIONID)
          REPLACE cCstSht_Id WITH  lfGetCstSht(lcStyMajor   ,loFormsetPar) IN (loFormsetPar.lcHdrFile)
          RETURN 0
        ENDIF
        REPLACE cCstSht_Id WITH  lcCstShtID IN (loFormsetPar.lcHdrFile)
        lcStyle    = SUBSTR(EVALUATE(loFormsetPar.lcHdrFile+'.STYLE'),1,loFormsetPar.lnMajorLen)
        lcPo       = EVALUATE(loFormsetPar.lcHdrFile+'.PO')
        lcDivision = EVALUATE(loFormsetPar.lcHdrFile+'.CDIVISION')
        lcSeason   = EVALUATE(loFormsetPar.lcHdrFile+'.Season')
        lcYear     = EVALUATE(loFormsetPar.lcHdrFile+'.cYear')
        lcWeek     = EVALUATE(loFormsetPar.lcHdrFile+'.cWeek')
        REPLACE ALL cCstSht_Id WITH  EVALUATE(loFormsetPar.lcHdrFile+'.cCstSht_ID') IN (loFormsetPar.lcPOLine) FOR PO+cDivision+SEASON+cWeek+cYear+STYLE+Dyelot = lcPo+lcDivision+lcSeason+lcWeek+lcYear
        RETURN 1
      ELSE
        PRIVATE loBomHeader
        loBomHeader = CREATEOBJECT('RemoteTable','BomHeadr','BomHeadr','BomHeadr',loFormsetPar.DATASESSIONID)
        REPLACE cCstSht_Id WITH lfGetCstSht(lcStyMajor   ,loFormsetPar)  IN (loFormsetPar.lcHdrFile)
        RETURN 0
      ENDIF
    ENDIF
  ENDIF
*:*************************************************************
*: Name      : lfUpdPOOTS
*: Developer : Nader Nabil - (NNA)
*: Date      : 10/21/2007
*: Purpose   : Generate P/O's
*:*************************************************************
*: Calls     : gfGetExSin(),lfUnAllPO()
*:*************************************************************
*: Parameters: lcOrdFile : Order line temporary file name
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfUpdPOOTS(lcOrdFile)
*:*************************************************************
FUNCTION lfUpdPOOTS
PARAMETERS lcOrdFile
PRIVATE lnAlias,lcStyOrd,lcPUntSin,lcDUntSin,lcPExSign,lcDExSign,lcDivision,;
  lcPurCode,lcStyGrade,lnTCost1,lnTCost2,lnTCost3,lnTCost4,lnTCost5,lnTCost6,lnTCost7,;
  lnOrder,lnAllocat,lnTFCost1,lnTFCost2,lnTFCost3,lnTFCost4,lnTFCost5,lnTFCost6,lnTFCost7
PRIVATE lnCutQty1,lnCutQty2,lnCutQty3,lnCutQty4,lnCutQty5,lnCutQty6,lnCutQty7,lnCutQty8,;
  lnOrdQty1,lnOrdQty2,lnOrdQty3,lnOrdQty4,lnOrdQty5,lnOrdQty6,lnOrdQty7,lnOrdQty8,lcOld_Ordr
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *WAIT LANG_MFGENCT_GENPOORD WINDOW NOWAIT
  WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_GENPOORD,loParentForm.GetHeaderText("LANG_MFGENCT_GENPOORD",loParentForm.HeaderAlias)) WINDOW NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[End]
*! B610190,2 MMT 01/13/2013 Modify Generate PO from OTS to Handle price/size in cost sheet[Start]
PRIVATE loBomHeader
loBomHeader = CREATEOBJECT('RemoteTable','BomHeadr','BomHeadr','BomHeadr',loParentForm.DATASESSIONID)
loBom = CREATEOBJECT('RemoteTable','Bom','MULTIBOM','Bom',loParentForm.DATASESSIONID)
*! B610190,2 MMT 01/13/2013 Modify Generate PO from OTS to Handle price/size in cost sheet[End]
STORE '/' TO lcPUntSin,lcDUntSin
lcPExSign = gfGetExSin(@lcPUntSin,loParentForm.lcPCurr)
lcDExSign = gfGetExSin(@lcDUntSin,loParentForm.lcDCurr)
lnAlias = SELECT(0)
lnColorLen = loParentForm.lnColorLen
lnColorStr = loParentForm.lnColorStr
SELECT STYLE
lcStyOrd = TAG()
loParentForm.loStyle.SetOrder('STYLE')
SELECT (lcOrdFile)
lcOrdTag = TAG()
SET ORDER TO TAG TICKET
=IIF(loParentForm.llRPUnAWip,lfUnAllPO(lcOrdFile),.T.)
SELECT (lcOrdFile)
=SEEK("�")
STORE 0 TO lnTCost1,lnTCost2,lnTCost3,lnTCost4,lnTCost5,lnTCost6,lnTCost7,lnOrder,lnAllocat,;
           lnTFCost1,lnTFCost2,lnTFCost3,lnTFCost4,lnTFCost5,lnTFCost6,lnTFCost7 , lnStyPrice
DO WHILE CSELECT+CDIVISION+CPURCODE+CSTYGRADE+STYLE = "�"
  =SEEK(STYLE,'STYLE')
  lcDivision = STYLE.CDIVISION
  lcPurCode  = STYLE.cPurCode
  lcStyGrade = STYLE.cStyGrade
  lcStyle    = Style.STYLE
  lcDyelot   = ''
  lcFabric   = Style.Fabric
  lcFabClr   = ''
  lcFabWare  = ''
  lnYeild    = 0.0
  SELECT (loParentForm.lcPOH)
  IF !SEEK(SPACE(6)+lcDivision+lcPurCode+lcStyGrade)
    APPEND BLANK
    REPLACE cStyType   WITH 'P',;
            Vendor     WITH loParentForm.lcVendor,;
            STATUS     WITH 'H',;
            CDIVISION  WITH lcDivision,;
            cPurCode   WITH lcPurCode,;
            ENTERED    WITH oariaapplication.systemdate,;
            START      WITH oariaapplication.systemdate,;
            COMPLETE   WITH ENTERED+90,;
            SHIPVIA    WITH loParentForm.lcShip,;
            CTERMCODE  WITH loParentForm.lcTerm,;
            cWareCode  WITH loParentForm.lcWareCode ,;
            cStyGrade  WITH lcStyGrade,;
            cPriceCur  WITH loParentForm.lcPCurr,;
            nPriceRat  WITH loParentForm.lnPRate,;
            nCurrUnit  WITH loParentForm.lnUnit1,;
            cDutyCur   WITH loParentForm.lcDCurr,;
            nDutyRat   WITH loParentForm.lnDRate,;
            nDCurUnit  WITH loParentForm.lnUnit2,;
            lMultiWare WITH (loParentForm.lnType=2)
    STORE 0 TO lnTCost1,lnTCost2,lnTCost3,lnTCost4,lnTCost5,lnOrder,lnAllocat,;
            lnTFCost1,lnTFCost2,lnTFCost3,lnTFCost4,lnTFCost5
  ENDIF

  IF loParentForm.llDispPric
    STORE 0 TO lnSelPrice,lnAllQty,lnRotSub,lnGrosMrgn
  ENDIF
  SELECT (lcOrdFile)
  SCAN REST while CSELECT+CDIVISION+CPURCODE+CSTYGRADE+STYLE = "�" + lcDivision+lcPurCode+lcStyGrade+lcStyle
    =SEEK(STYLE,'STYLE')
    lcDivision = STYLE.CDIVISION
    lcPurCode  = STYLE.cPurCode
    lcStyGrade = STYLE.cStyGrade
    lcStyle    = Style.STYLE

    STORE 0 TO lnCutQty1,lnCutQty2,lnCutQty3,lnCutQty4,lnCutQty5,lnCutQty6,lnCutQty7,lnCutQty8
    STORE 0 TO lnOrdQty1,lnOrdQty2,lnOrdQty3,lnOrdQty4,lnOrdQty5,lnOrdQty6,lnOrdQty7,lnOrdQty8

    lnCutQty1 = lnCutQty1+INT(loParentForm.laPercet[1]/100 * &lcOrdFile..Plan1)
    lnCutQty2 = lnCutQty2+INT(loParentForm.laPercet[2]/100 * &lcOrdFile..Plan2)
    lnCutQty3 = lnCutQty3+INT(loParentForm.laPercet[3]/100 * &lcOrdFile..Plan3)
    lnCutQty4 = lnCutQty4+INT(loParentForm.laPercet[4]/100 * &lcOrdFile..Plan4)
    lnCutQty5 = lnCutQty5+INT(loParentForm.laPercet[5]/100 * &lcOrdFile..Plan5)
    lnCutQty6 = lnCutQty6+INT(loParentForm.laPercet[6]/100 * &lcOrdFile..Plan6)
    lnCutQty7 = lnCutQty7+INT(loParentForm.laPercet[7]/100 * &lcOrdFile..Plan7)
    lnCutQty8 = lnCutQty8+INT(loParentForm.laPercet[8]/100 * &lcOrdFile..Plan8)
    lnOrdQty1 = lnOrdQty1 + &lcOrdFile..Plan1
    lnOrdQty2 = lnOrdQty2 + &lcOrdFile..Plan2
    lnOrdQty3 = lnOrdQty3 + &lcOrdFile..Plan3
    lnOrdQty4 = lnOrdQty4 + &lcOrdFile..Plan4
    lnOrdQty5 = lnOrdQty5 + &lcOrdFile..Plan5
    lnOrdQty6 = lnOrdQty6 + &lcOrdFile..Plan6
    lnOrdQty7 = lnOrdQty7 + &lcOrdFile..Plan7
    lnOrdQty8 = lnOrdQty8 + &lcOrdFile..Plan8
    IF loParentForm.llDispPric
      STORE 0 TO lnTotQty
      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        lnTotQty = lnTotQty + INT(loParentForm.laPercet[lnI]/100 * (&lcOrdFile..Plan&lcI))
      ENDFOR
      lnAllQty   = lnAllQty + lnTotQty
      lnStyPrice = IIF(&lcOrdFile..PriceA>0 , &lcOrdFile..PriceA , ;
                   IIF(&lcOrdFile..PriceB>0 , &lcOrdFile..PriceB ,&lcOrdFile..PriceC))
      lnselPrice = lnSelPrice + ( lnTotQty*lnStyPrice )
    ENDIF
    *-- Update the Temp Browse File At main Screen.
    IF lnCutQty1+lnCutQty2+lnCutQty3+lnCutQty4+lnCutQty5+lnCutQty6+lnCutQty7+lnCutQty8 > 0
      REPLACE nPOQty1 WITH lnCutQty1 ,;
              nPOQty2 WITH lnCutQty2 ,;
              nPOQty3 WITH lnCutQty3 ,;
              nPOQty4 WITH lnCutQty4 ,;
              nPOQty5 WITH lnCutQty5 ,;
              nPOQty6 WITH lnCutQty6 ,;
              nPOQty7 WITH lnCutQty7 ,;
              nPOQty8 WITH lnCutQty8 ,;
              nPOTotQty WITH nPOQty1+nPOQty2+nPOQty3+nPOQty4+nPOQty5+nPOQty6+nPOQty7+nPOQty8
    ENDIF
  ENDSCAN
  IF loParentForm.llDispPric
    lnSelPrice = lnSelPrice / lnAllQty
  ENDIF
  IF lnCutQty1+lnCutQty2+lnCutQty3+lnCutQty4+lnCutQty5+lnCutQty6+lnCutQty7+lnCutQty8 > 0
    SELECT (loParentForm.lcPOLine)
    IF !SEEK(SPACE(6)+lcDivision+lcPurCode+lcStyGrade+lcStyle+lcDyelot)
      SELECT (loParentForm.lcPOH)
      REPLACE LASTLINE WITH LASTLINE + 1
      SELECT (loParentForm.lcPOLine)
      APPEND BLANK
      REPLACE STYLE      WITH lcStyle    ,;
              DYELOT     WITH lcDyelot   ,;
              TRANCD     WITH '1'        ,;
              CDIVISION  WITH lcDivision ,;
              cPurCode   WITH lcPurCode  ,;
              cWareCode  WITH loParentForm.lcWareCode ,;
              LINENO     WITH EVALUATE(loParentForm.lcPOH+'.LastLine') ,;
              Vendor     WITH loParentForm.lcVendor   ,;
              ShipVia    WITH loParentForm.lcShip     ,;
              Complete   WITH EVALUATE(loParentForm.lcPOH+'.Complete'),;
              Fabric     WITH lcFabric   ,;
              cFabWare   WITH lcFabWare  ,;
              cStyGrade  WITH lcStyGrade ,;
              nYeild     WITH lnYeild
      *! B610213,1 HIA 01/23/2013 Aria4xp - PO - Generate PO from SO/Open to sel [T20130107.0006][Start]
      REPLACE cvensty WITH STYLE.cvensty
      *! B610213,1 HIA 01/23/2013 Aria4xp - PO - Generate PO from SO/Open to sel [T20130107.0006][End]
              
    ENDIF
    REPLACE QTY1 WITH lnCutQty1 ,;
            QTY2 WITH lnCutQty2 ,;
            QTY3 WITH lnCutQty3 ,;
            QTY4 WITH lnCutQty4 ,;
            QTY5 WITH lnCutQty5 ,;
            QTY6 WITH lnCutQty6 ,;
            QTY7 WITH lnCutQty7 ,;
            QTY8 WITH lnCutQty8 ,;
            Ord1 WITH lnOrdQty1 ,;
            Ord2 WITH lnOrdQty2 ,;
            Ord3 WITH lnOrdQty3 ,;
            Ord4 WITH lnOrdQty4 ,;
            Ord5 WITH lnOrdQty5 ,;
            Ord6 WITH lnOrdQty6 ,;
            Ord7 WITH lnOrdQty7 ,;
            Ord8 WITH lnOrdQty8 ,;
            TOTQTY WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
            TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
    =loParentForm.loStyle.SEEK(lcStyle)
    lnGros_Price = 0
    *! B610190,2 MMT 01/13/2013 Modify Generate PO from OTS to Handle price/size in cost sheet[Start]
    REPLACE cCstSht_Id WITH lfGetCstSht(lcStyle,loParentForm)
    STORE 0 TO lnfCost1, lnFCost2,lnFCost3,lnFCost4,lnFCost5,lnFCost6,lnFCost7
    STORE 0 TO lnICost1, lnICost2, lnICost3, lnICost4, lnICost5, lnICost6, lnICost7
    lcCstShtTyp = IIF(loParentForm.lcChoice $ 'OP','I','M')
    lcCstSht_ID = EVALUATE(loParentForm.lcPOLine+'.cCstSht_Id')
    llCstShtPerSize = loBomHeader.SEEK('0001'+PADR(SUBSTR(lcStyle,1,loParentForm.lnMajorLen),19)+"I"+PADR(lcCstSht_ID,6)) AND BomHeadr.lbasonsiz
    IF llCstShtPerSize AND loBom.SEEK('0001'+PADR(SUBSTR(lcStyle,1,loParentForm.lnMajorLen),19)+lcCstShtTyp+lcCstSht_ID)
      DIMENSION lasizeprice[1,6]
      lasizeprice = ''
      SELECT BOM
      SCAN REST WHILE cinvtype+cItmMajor+cCstShtTyp+cCstSht_Id+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6) =;
                      '0001'+PADR(SUBSTR(lcStyle,1,loParentForm.lnMajorLen),19)+lcCstShtTyp+lcCstSht_ID  FOR ccatgtyp = 'P' 
        IF !LIKE(STRTRAN(cItmMask,'*','?'),lcStyle) .OR. (!EMPTY(MSIZES) .AND. ATCLINE(STYLE.SCALE+'~',MSIZES)=0) .OR. ;
           (!EMPTY(MSZCROSREF) .AND. ATCLINE(STYLE.SCALE+',',MSZCROSREF)=0)
          LOOP
        ENDIF
        IF !EMPTY(BOM.mSizes)
          =SEEK(lcStyle,'STYLE','STYLE')
          lnMSizeLines = MEMLINES(BOM.mSizes)
          IF lnMSizeLines > 0
            FOR lnCntLine =  1 TO lnMSizeLines
              lcMSizes = MLINE(BOM.mSizes,lnCntLine)
              IF !EMPTY(lcMSizes)
                lnWPos = ATC('~',lcMSizes)
                IF !(Style.Scale $ lcMSizes)
                  LOOP
                ENDIF
                lnScales = SUBSTR(lcMSizes,lnWPos+1)
                IF EMPTY(lasizeprice[1,1])
                  lasizeprice[1,1]= SUBSTR(lcMSizes,1,lnWPos-1)
                  lasizeprice[1,2]= lnScales
                  lasizeprice[1,3]= Bom.TotCost
                  lasizeprice[1,4]= Bom.nExRate
                  lasizeprice[1,5]= Bom.nCurrUnit
                  lasizeprice[1,6]= Bom.cCurrCode
                ELSE
                  lnPosInArr = ALEN(lasizeprice,1)
                  DIMENSION lasizeprice[lnPosInArr+1,6]
                  lasizeprice[lnPosInArr+1,1]= SUBSTR(lcMSizes,1,lnWPos-1)
                  lasizeprice[lnPosInArr+1,2]= lnScales
                  lasizeprice[lnPosInArr+1,3]= Bom.TotCost
                  lasizeprice[lnPosInArr+1,4]= Bom.nExRate
                  lasizeprice[lnPosInArr+1,5]= Bom.nCurrUnit
                  lasizeprice[lnPosInArr+1,6]= Bom.cCurrCode
                  
                ENDIF
              ENDIF
            ENDFOR
          ENDIF                   
        ENDIF 
      ENDSCAN
      IF ALEN(lasizeprice,1) <= 1
        llCstShtPerSize = .F.
      ENDIF 
    ENDIF 
    SELECT (loParentForm.lcPOLine)
    IF loBom.SEEK('0001'+PADR(SUBSTR(lcStyle,1,loParentForm.lnMajorLen),19)+lcCstShtTyp+lcCstSht_ID)
      SELECT BOM
      SCAN REST WHILE cinvtype+cItmMajor+cCstShtTyp+cCstSht_Id+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6) =;
                      '0001'+PADR(SUBSTR(lcStyle,1,loParentForm.lnMajorLen),19)+lcCstShtTyp+lcCstSht_ID
        IF !LIKE(STRTRAN(cItmMask,'*','?'),lcStyle) .OR. (!EMPTY(MSIZES) .AND. ATCLINE(STYLE.SCALE+'~',MSIZES)=0) .OR. ;
           (!EMPTY(MSZCROSREF) .AND. ATCLINE(STYLE.SCALE+',',MSZCROSREF)=0)
          LOOP
        ENDIF
        IF Typ = '8'
          LOOP
        ENDIF
        lcCostType = Typ
        STORE '/' TO lcUntSin
        lnExRate   = IIF(ISNULL(nExRate) OR nExRate=0,1,nExRate)
        lnCurrUnit = IIF(ISNULL(nCurrUnit) OR nCurrUnit=0,1,nCurrUnit)
        lcCurrCode = IIF(ISNULL(cCurrCode) OR EMPTY(cCurrCode),oAriaApplication.BaseCurrency,cCurrCode)
        lcExSign = gfGetExSin(@lcUntSin, lcCurrCode)
        lnFCost&lcCostType  = EVALUATE(loParentForm.lcPOLine+'.TotQty')*TotCost
        lnICost&lcCostType = lnFCost&lcCostType &lcExSign lnExRate &lcUntSin lnCurrUnit
        lnGros_Price       = IIF(lcCostType='1',TotCost,lnGros_Price)
        IF !llCstShtPerSize OR  (llCstShtPerSize AND ccatgtyp <> 'P')
          lnTCost&lcCostType= lnTCost&lcCostType+ lnICost&lcCostType
          lnTFCost&lcCostType= lnTFCost&lcCostType+ lnFCost&lcCostType
        ENDIF
      ENDSCAN
      SELECT (loParentForm.lcPOLine)
    ELSE
    *! B610190,2 MMT 01/13/2013 Modify Generate PO from OTS to Handle price/size in cost sheet[End]    
    FOR lnCount = 1 TO 7
      lcCount = STR(lnCount,1)
      DO CASE
        CASE loParentForm.lcIType&lcCount = 'P'
          lnFCost&lcCount = IIF(loParentForm.lcPCurr=Style.cPriceCur,TOTQTY*STYLE.nICost&lcCount,0)
          lnICost&lcCount= lnFCost&lcCount &lcPExSign loParentForm.lnPRate &lcPUntSin loParentForm.lnUnit1
          lnGros_Price   = IIF(lnCount=1,IIF(loParentForm.lcPCurr=Style.cPriceCur,STYLE.nICost1,0),lnGros_Price)
        CASE INLIST(loParentForm.lcIType&lcCount,'M','D')
          lnFCost&lcCount = IIF(loParentForm.lcDCurr=Style.cDutyCur,TOTQTY*STYLE.nICost&lcCount,0)
          lnICost&lcCount= lnFCost&lcCount &lcDExSign loParentForm.lnDRate &lcDUntSin loParentForm.lnUnit2
          lnGros_Price   = IIF(lnCount=1,IIF(loParentForm.lcDCurr=Style.cDutyCur,STYLE.nICost1,0),lnGros_Price)
        OTHERWISE
          lnFCost&lcCount = TOTQTY*STYLE.nICost&lcCount
          lnICost&lcCount= lnFCost&lcCount
          lnGros_Price   = IIF(lnCount=1,STYLE.nICost1,lnGros_Price)
      ENDCASE
      lnTCost&lcCount  = lnTCost&lcCount  + lnICost&lcCount
      lnTFCost&lcCount = lnTFCost&lcCount + lnFCost&lcCount
    ENDFOR
    *! B610190,2 MMT 01/13/2013 Modify Generate PO from OTS to Handle price/size in cost sheet[Start]
    ENDIF
    IF llCstShtPerSize 
      lnTotQty = 0
      llFirstLine = .T.
      FOR lnSzcnt = 1 TO ALEN(lasizeprice,1)
        lcSizes = lasizeprice[lnSzCnt,2]
        IF llFirstLine 
          lnTotQty = TotQty
          SCATTER MEMO MEMVAR
          SELECT (loParentForm.lcPOH)
          REPLACE LastLine WITH LastLine + 1                          
        ELSE
          APPEND BLANK
          m.LineNO = EVALUATE(loParentForm.lcPOH+".LastLine")
          SELECT (loParentForm.lcPOH)
          REPLACE LastLine WITH LastLine + 1    
          SELECT (loParentForm.lcPOLine)                      
          GATHER MEMO MEMVAR  
        ENDIF   
        SELECT (loParentForm.lcPOLine)
        FOR lnCntSize = 1 TO 8
          lcCntSz = STR(lnCntSize,1)
          IF lcCntSz $ lcSizes
            IF m.Qty&lcCntSz. > 0 
              m.Qty&lcCntSz. = 0
            ENDIF 
          ELSE
            REPLACE Qty&lcCntSz. WITH 0 
          ENDIF
        ENDFOR  
        SELECT (loParentForm.lcPOLine)
        REPLACE TOTQTY WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
                ORD1 WITH MIN(Qty1,Ord1),;
                ORD2 WITH MIN(Qty2,Ord2),;
                ORD3 WITH MIN(Qty3,Ord3),;
                ORD4 WITH MIN(Qty4,Ord4),;
                ORD5 WITH MIN(Qty5,Ord5),;
                ORD6 WITH MIN(Qty6,Ord6),;
                ORD7 WITH MIN(Qty7,Ord7),;
                ORD8 WITH MIN(Qty8,Ord8),;
                TotOrd WITH ORD1+ ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
        STORE '/' TO lcUntSin
        lnExRate   = IIF(ISNULL(lasizeprice[lnSzCnt,4]) OR lasizeprice[lnSzCnt,4]=0,1,lasizeprice[lnSzCnt,4])
        lnCurrUnit = IIF(ISNULL(lasizeprice[lnSzCnt,5]) OR lasizeprice[lnSzCnt,5]=0,1,lasizeprice[lnSzCnt,5])
        lcCurrCode = IIF(ISNULL(lasizeprice[lnSzCnt,6]) OR EMPTY(lasizeprice[lnSzCnt,6]),oAriaApplication.BaseCurrency,lasizeprice[lnSzCnt,6])
        lcExSign = gfGetExSin(@lcUntSin, lcCurrCode)
        lnfCost1 = EVALUATE(loParentForm.lcPOLine+'.TotQty')*lasizeprice[lnSzCnt,3]
        lnICost1 = lnFCost1 &lcExSign lnExRate &lcUntSin lnCurrUnit
        lnGros_Price = lasizeprice[lnSzCnt,3]
        IF lnSzcnt <> ALEN(lasizeprice,1)
          REPLACE nfCost1  WITH nFCost1 +lnFCost1 ,;
                  nfCost2  WITH nFCost2 + ((lnFCost2 * TotQty)/lnTotQty)  ,;
                  nfCost3  WITH nFCost3 + ((lnFCost3 * TotQty)/lnTotQty)  ,;
                  nfCost4  WITH nFCost4 + ((lnFCost4 * TotQty)/lnTotQty)  ,;
                  nfCost5  WITH nFCost5 + ((lnFCost5 * TotQty)/lnTotQty)  ,;
                  nfCost6  WITH nFCost6 + ((lnFCost6 * TotQty)/lnTotQty)  ,;
                  nfCost7  WITH nFCost7 + ((lnFCost7 * TotQty)/lnTotQty)  ,;
                  nICost1  WITH nICost1+ lnICost1 ,;
                  nICost2  WITH nICost2+ ((lnICost2  * TotQty)/lnTotQty),;
                  nICost3  WITH nICost3+ ((lnICost3  * TotQty)/lnTotQty),;
                  nICost4  WITH nICost4+ ((lnICost4  * TotQty)/lnTotQty),;
                  nICost5  WITH nICost5+ ((lnICost5  * TotQty)/lnTotQty),;
                  nICost6  WITH nICost6+ ((lnICost6  * TotQty)/lnTotQty),;
                  nICost7  WITH nICost7+ ((lnICost7 * TotQty)/lnTotQty),;
                  Gros_Price WITH lnGros_Price,;
                  Disc_Pcnt WITH 0
          SELECT (loParentForm.lcPOLine)
          IF loParentForm.llDispPric
            lnTotCost   = (nIcost1+nIcost2+nIcost3+nIcost4+nIcost5)/TOTQTY
            lnRotSub    = IIF(loParentForm.llStyMark,lnTotCost,lnSelPrice)
            lnGrosMrgn  = IIF(lnRotSub=0,0,((lnSelPrice - lnTotCost)/lnRotSub)*100)
            REPLACE nSelPrice WITH lnSelPrice ,;
              nGrosMrgn WITH lnGrosMrgn
            STORE 0 TO lnSelPrice,lnAllQty
          ENDIF
          lnOrder   = lnOrder   + TOTORD
          lnAllocat = lnAllocat + TOTQTY
        ELSE
          FOR lnCstCnt=2 TO 7
            lcCstCnt = STR(lnCstCnt,1)
            lnfCost&lcCstCnt. = ((lnfCost&lcCstCnt. * TotQty)/lnTotQty)
          ENDFOR   
          REPLACE Gros_Price WITH lnGros_Price
        ENDIF 
        lnTCost1 = lnTCost1+ lniCost1
        lnTFCost1= lnTFCost1+ lnfCost1
        llFirstLine =.F.            
        SELECT (loParentForm.lcPOLine)
        IF TOTQTY = 0
          DELETE
        ENDIF 
       ENDFOR 
      SELECT (loParentForm.lcPOH)
      REPLACE LastLine WITH LastLine - 1 
      SELECT (loParentForm.lcPOLine)
    ENDIF
    *! B610190,2 MMT 01/13/2013 Modify Generate PO from OTS to Handle price/size in cost sheet[End] 
    REPLACE nfCost1  WITH nFCost1 + lnFCost1  ,;
            nfCost2  WITH nFCost2 + lnFCost2  ,;
            nfCost3  WITH nFCost3 + lnFCost3  ,;
            nfCost4  WITH nFCost4 + lnFCost4  ,;
            nfCost5  WITH nFCost5 + lnFCost5  ,;
            nfCost6  WITH nFCost6 + lnFCost6  ,;
            nfCost7  WITH nFCost7 + lnFCost7  ,;
            nICost1  WITH nICost1+ lnICost1 ,;
            nICost2  WITH nICost2+ lnICost2 ,;
            nICost3  WITH nICost3+ lnICost3 ,;
            nICost4  WITH nICost4+ lnICost4 ,;
            nICost5  WITH nICost5+ lnICost5 ,;
            nICost6  WITH nICost6+ lnICost6 ,;
            nICost7  WITH nICost7+ lnICost7
    SELECT (loParentForm.lcPOLine)
    IF loParentForm.llDispPric
      lnTotCost   = (nIcost1+nIcost2+nIcost3+nIcost4+nIcost5)/TOTQTY
      lnRotSub    = IIF(loParentForm.llStyMark,lnTotCost,lnSelPrice)
      lnGrosMrgn  = IIF(lnRotSub=0,0,((lnSelPrice - lnTotCost)/lnRotSub)*100)
      REPLACE nSelPrice WITH lnSelPrice ,;
        nGrosMrgn WITH lnGrosMrgn
      STORE 0 TO lnSelPrice,lnAllQty
    ENDIF
    lnOrder   = lnOrder   + TOTORD
    lnAllocat = lnAllocat + TOTQTY
  ENDIF
  SELECT (lcOrdFile)
  SELECT (loParentForm.lcPOH)
  REPLACE nStyOrder WITH lnAllocat ,;
          TotOrd    WITH lnOrder   ,;
          NFCOST1   WITH lnTFCost1 ,;
          NFCOST2   WITH lnTFCost2 ,;
          NFCOST3   WITH lnTFCost3 ,;
          NFCOST4   WITH lnTFCost4 ,;
          NFCOST5   WITH lnTFCost5 ,;
          NFCOST6   WITH lnTFCost6 ,;
          NFCOST7   WITH lnTFCost7 ,;
          NICOST1   WITH lnTCost1  ,;
          NICOST2   WITH lnTCost2  ,;
          NICOST3   WITH lnTCost3  ,;
          NICOST4   WITH lnTCost4  ,;
          NICOST5   WITH lnTCost5  ,;
          NICOST6   WITH lnTCost6  ,;
          NICOST7   WITH lnTCost7  ,;
          TOTCOST   WITH lnTFCost1+lnTFCost2+lnTFCost3+lnTFCost4+lnTFCost5+lnTFCost6+lnTFCost7
  IF nStyOrder = 0
    DELETE
  ENDIF
  SELECT (lcOrdFile)
ENDDO
SET ORDER TO TAG (lcStyOrd) IN STYLE
SET ORDER TO TAG (lcOrdTag) IN (lcOrdFile)
SELECT (lnAlias)
WAIT CLEAR
*-- End OF Function lfUpdPOOTS.
*:*************************************************************
*: Name      : lfDetScr
*: Developer : Nader Nabil - (NNA)
*: Date      : 10/21/2007
*: Purpose   : Generate P/O's
*:*************************************************************
*: Calls     :
*:*************************************************************
*: Parameters: lcOrdFile : Order line temporary file name
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfDetScr
*:*************************************************************
FUNCTION lfDetScr
LPARAMETERS loFormSet
WITH loFormSet.AriaForm1
  LOCAL lnI,lcI
  .cntDetails.VISIBLE=.F.
  FOR lnI = 1 TO 7
    lcI = STR(lnI,1)
    .lblCost&lcI..Visible          = .T.
    .lblCost&lcI..CAPTION          = EVALUATE('loParentForm.lc'+'I'+'SLbl'+lcI)
    .txtCost&lcI..CONTROLSOURCE    = loParentForm.lcLinFile+'.nFCost'+lcI
    .txtCost&lcI..visible          = .T.
    .txtCost&lcI.Ord.CONTROLSOURCE = "lfOrdCost('"+lcI+"')"
    .txtCost&lcI.Ord.Visible       = .T.
  ENDFOR
  .txtTotalCost.CONTROLSOURCE    = "lfTotCost('')"
  .txtTotalCostOrd.CONTROLSOURCE = "lfOrdTotCost('')"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.lblPoLine.CAPTION = LANG_MFGENCT_PO+" "+LANG_MFGENCT_LINE
.lblPoLine.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_PO,loFormSet.GetHeaderText("LANG_MFGENCT_PO",loFormSet.HeaderAlias))+" "+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_LINE,loFormSet.GetHeaderText("LANG_MFGENCT_LINE",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.lblOrdLine.CAPTION = 'OTS'+" "+LANG_MFGENCT_LINE
.lblOrdLine.CAPTION = 'OTS'+" "+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_LINE,loFormSet.GetHeaderText("LANG_MFGENCT_LINE",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  .cmdClose.VISIBLE        = .T.
  .cntHeader.VISIBLE       = .T.
  .lblOrdLine.VISIBLE      = .T.
  .lblPoLine.VISIBLE       = .T.
  .txtTotalCost.VISIBLE    = .T.
  .lblTotalCost.VISIBLE    = .T.
  .txtTotalCostOrd.VISIBLE = .T.
  .lbl7.VISIBLE = .T.
  .lbl8.VISIBLE = .T.
  .grdPOLine.HEIGHT = 304
  .cntPoLIne.AriaShape1.HEIGHT = 310
  WITH .grdPOLine
		.RECORDSOURCE             = loParentForm.lcPOLine
		.Column2.CONTROLSOURCE    = loParentForm.lcPOLine+'.Style'
    .Column3.CONTROLSOURCE    = loParentForm.lcPOLine+'.Dyelot'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.Column3.Header1.CAPTION  = IIF(loParentForm.llStyConfg,LANG_MFGENCT_CONFIG,LANG_MFGENCT_DYELOT)  	
.Column3.Header1.CAPTION  = IIF(loParentForm.llStyConfg,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_CONFIG,loFormSet.GetHeaderText("LANG_MFGENCT_CONFIG",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_DYELOT,loFormSet.GetHeaderText("LANG_MFGENCT_DYELOT",loFormSet.HeaderAlias)))  	
*N000682,1 11/20/2012 MMT Globlization changes[End]

  	
  	.Column4.CONTROLSOURCE    = loParentForm.lcPOLine+'.Qty1'
    .Column5.CONTROLSOURCE    = loParentForm.lcPOLine+'.Qty2'
		.Column6.CONTROLSOURCE    = loParentForm.lcPOLine+'.Qty3'
		.Column7.CONTROLSOURCE    = loParentForm.lcPOLine+'.Qty4'
		.Column8.CONTROLSOURCE    = loParentForm.lcPOLine+'.Qty5'
		.Column9.CONTROLSOURCE    = loParentForm.lcPOLine+'.Qty6'
		.Column10.CONTROLSOURCE   = loParentForm.lcPOLine+'.Qty7'
		.Column11.CONTROLSOURCE   = loParentForm.lcPOLine+'.Qty8'
		.Column12.CONTROLSOURCE   = loParentForm.lcPOLine+'.TotQty'
		
		.Column1.CONTROLSOURCE    = loParentForm.lcPOLine+'.nFCost1'
		.Column1.Header1.CAPTION  = EVALUATE('loParentForm.lc'+'I'+'SLbl1')

		.Column13.CONTROLSOURCE   = loParentForm.lcPOLine+'.nFCost2'
		.Column13.Header1.CAPTION = EVALUATE('loParentForm.lc'+'I'+'SLbl2')

		.Column14.CONTROLSOURCE   = loParentForm.lcPOLine+'.nFCost3'
		.Column14.Header1.CAPTION = EVALUATE('loParentForm.lc'+'I'+'SLbl3')

		.Column15.CONTROLSOURCE   = loParentForm.lcPOLine+'.nFCost4'
		.Column15.Header1.CAPTION = EVALUATE('loParentForm.lc'+'I'+'SLbl4')

		.Column16.CONTROLSOURCE   = loParentForm.lcPOLine+'.nFCost5'
		.Column16.Header1.CAPTION = EVALUATE('loParentForm.lc'+'I'+'SLbl5')

		.Column17.CONTROLSOURCE   = loParentForm.lcPOLine+'.nFCost6'
		.Column17.Header1.CAPTION = EVALUATE('loParentForm.lc'+'I'+'SLbl6')

		.Column18.CONTROLSOURCE   = loParentForm.lcPOLine+'.nFCost7'
		.Column18.Header1.CAPTION = EVALUATE('loParentForm.lc'+'I'+'SLbl7')

		.Column19.CONTROLSOURCE   = "lfTotCost('')"
		.Column20.CONTROLSOURCE   = loParentForm.lcPOLine+'.nSelPrice'
		.Column21.CONTROLSOURCE   = loParentForm.lcPOLine+'.nGrosMrgn'

    *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[Start]
    IF INLIST(loParentForm.lcGenProj,'A','I') AND loParentForm.LLRPGNLPJ
      .Column28.VISIBLE         = .T.
      .Column28.CONTROLSOURCE   = loParentForm.lcPOLine+'.CDEFTEMPL'
      .Column28.Readonly = .F.
    ELSE
      .Column28.VISIBLE         = .F.
    ENDIF
    *! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[End]


    IF !loParentForm.llDyelot
	    .removeobject("column3")
    ENDIF
    IF !loParentForm.llDispPric
      .removeobject("column20")
      .removeobject("column21")
	  ENDIF
    .removeobject("column22")
    .removeobject("column23")
    .removeobject("column24")
  ENDWITH
ENDWITH
*-- End OF Function lfDetScr.
*:*************************************************************
*: Name      : lfvPoSzQty
*: Developer : Nader Nabil - (NNA)
*: Date      : 10/21/2007
*: Purpose   : Called from the Qty fields in the browse of the line file
*:*************************************************************
*: Calls     :
*:*************************************************************
*: Parameters: lcOrdFile : Order line temporary file name
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfvPoSzQty
*:*************************************************************
FUNCTION lfvPoSzQty
  LPARAMETERS lcCnt
  LOCAL lcQtyFld, lcOrdFld, lnAddedQty
  lcQtyFld = loParentForm.lcLinFile+'.Qty'+lcCnt
  lcOrdFld = 'Ord'+lcCnt
  IF EVALUATE(lcQtyFld)<0 THEN
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM42000B40011','DIALOG',LANG_MFGENCT_PO+'|'+LANG_MFGENCT_PO)
=gfModalGen('TRM42000B40011','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_PO,loFormSet.GetHeaderText("LANG_MFGENCT_PO",loFormSet.HeaderAlias))+'|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_PO,loFormSet.GetHeaderText("LANG_MFGENCT_PO",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    REPLACE (lcQtyFld) WITH loParentForm.lnOldVal
  ELSE
    SELECT (loParentForm.lcLinFile)
    lnAddedQty = EVALUATE(lcQtyFld) - loParentForm.lnOldVal
    LOCAL ARRAY laUnitFCost[7],laUnitICost[7]
    FOR lnI = 1 TO 7
      lcI = STR(lnI,1)
      laUnitFCost[lnI] = IIF(TotQty=0,EVALUATE('STYLE.nICost'+lcI),EVALUATE('nFCost'+lcI)/TotQty)
      laUnitICost[lnI] = IIF(TotQty=0,EVALUATE('STYLE.nICost'+lcI),EVALUATE('nICost'+lcI)/TotQty)
    ENDFOR
    REPLACE TotQty  WITH TotQty  + lnAddedQty,;
      nFCost1    WITH nFCost1 + lnAddedQty * laUnitFCost[1],;
      nFCost2    WITH nFCost2 + lnAddedQty * laUnitFCost[2],;
      nFCost3    WITH nFCost3 + lnAddedQty * laUnitFCost[3],;
      nFCost4    WITH nFCost4 + lnAddedQty * laUnitFCost[4],;
      nFCost5    WITH nFCost5 + lnAddedQty * laUnitFCost[5],;
      nFCost6    WITH nFCost6 + lnAddedQty * laUnitFCost[6],;
      nFCost7    WITH nFCost7 + lnAddedQty * laUnitFCost[7],;
      Gros_Price WITH laUnitFCost[1],;
      nICost1    WITH nICost1 + lnAddedQty * laUnitICost[1],;
      nICost2    WITH nICost2 + lnAddedQty * laUnitICost[2],;
      nICost3    WITH nICost3 + lnAddedQty * laUnitICost[3],;
      nICost4    WITH nICost4 + lnAddedQty * laUnitICost[4],;
      nICost5    WITH nICost5 + lnAddedQty * laUnitICost[5],;
      nICost6    WITH nICost6 + lnAddedQty * laUnitICost[6],;
      nICost7    WITH nICost7 + lnAddedQty * laUnitICost[7]

    SELECT (loParentForm.lcHdrFile)
    REPLACE nStyOrder WITH nStyOrder + lnAddedQty ,;
      nFCost1   WITH nFCost1   + lnAddedQty * laUnitFCost[1],;
      nFCost2   WITH nFCost2   + lnAddedQty * laUnitFCost[2],;
      nFCost3   WITH nFCost3   + lnAddedQty * laUnitFCost[3],;
      nFCost4   WITH nFCost4   + lnAddedQty * laUnitFCost[4],;
      nFCost5   WITH nFCost5   + lnAddedQty * laUnitFCost[5],;
      nFCost6   WITH nFCost6   + lnAddedQty * laUnitFCost[6],;
      nFCost7   WITH nFCost7   + lnAddedQty * laUnitFCost[7],;
      TOTCOST   WITH nFCost1+nFCost2+nFCost3+nFCost4+nFCost5+nFCost6+nFCost7,;
      nICost1   WITH nICost1   + lnAddedQty * laUnitICost[1],;
      nICost2   WITH nICost2   + lnAddedQty * laUnitICost[2],;
      nICost3   WITH nICost3   + lnAddedQty * laUnitICost[3],;
      nICost4   WITH nICost4   + lnAddedQty * laUnitICost[4],;
      nICost5   WITH nICost5   + lnAddedQty * laUnitICost[5],;
      nICost6   WITH nICost6   + lnAddedQty * laUnitICost[6],;
      nICost7   WITH nICost7   + lnAddedQty * laUnitICost[7]
    SELECT (loParentForm.lcLinFile)
    =lfPickWhen()
  ENDIF
*-->End of Function lfvPoSzQty.


FUNCTION lfvPOVend
LPARAMETERS loFormSet
LOCAL lcVendor, lnAlias, lcHdrFile

IF INLIST(loFormSet.lcChoice,'P','O')
lnAlias = SELECT()
lcHdrFile = loFormSet.lcHdrFile
lcVendor = &lcHdrFile..Vendor
*-- If the enterd vendor does not ecxit in the apvendor file
IF !loFormSet.loApVendor.SEEK(lcVendor) OR !('S' $ APVENDOR.cVenSupTyp .OR. 'C' $ APVENDOR.cVenSupTyp)
  =gfApVnBrow(@lcVendor,.F.,'CS')
ENDIF
lcVendor = IIF(EMPTY(lcVendor),loFormSet.lnOldVal,lcVendor)

REPLACE VENDOR WITH lcVendor IN (loFormSet.lcHdrFile)

lcLineKey = &lcHdrFile..Po+&lcHdrFile..cDivision+&lcHdrFile..cPurCode+&lcHdrFile..cStyGrade+;
IIF(loFormSet.llRPGrpByST,LEFT(&lcHdrFile..Style,loFormSet.lnMajorLen),'')

SELECT (loFormSet.lcPOLine)
=SEEK(lcLineKey)
REPLACE REST VENDOR WITH lcVendor  ;
 WHILE PO+cDivision+cPurCode+cStyGrade+STYLE+Dyelot = lcLineKey
ENDIF

FUNCTION lfvProjTemp
LPARAMETERS loFormSet

*-- If the enterd vendor does not ecxit in the apvendor file
lnAlias = SELECT()
SELECT 'PMPTHHD'
IF !GFSEEK(EVALUATE(loFormSet.lcHdrFile+'.CDEFTEMPL'),'PMPTHHD')
  GFSEEK('')
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
*!*    lcBrFields = "Cpath_id  :H='Template'," +;
*!*                "Cpath_dsc :H='Description'"
*!*    IF gfBrows('','Cpath_id','laFields','Templates')
  lcBrFields = "Cpath_id  :H='"+;
               IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_TEMPLATE,loFormSet.GetHeaderText("LANG_MFGENCT_TEMPLATE",loFormSet.HeaderAlias))+"'," +;
               "Cpath_dsc :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_TEMPLATEDESC,loFormSet.GetHeaderText("LANG_MFGENCT_TEMPLATEDESC",loFormSet.HeaderAlias))+"'"
  IF gfBrows('','Cpath_id','laFields',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_MTEMPLATE,loFormSet.GetHeaderText("LANG_MFGENCT_MTEMPLATE",loFormSet.HeaderAlias)))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
     REPLACE CDEFTEMPL WITH PMPTHHD.Cpath_id IN (loFormSet.lcHdrFile)
  ENDIF
ENDIF

FUNCTION lfvLEntered
LPARAMETERS loFormSet

lcHdrFile = loFormSet.lcHdrFile
IF EMPTY(&lcHdrFile..Entered)
  *-You cannot leave the entered date empty.
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *=gfModalGen('INM34005B34000','DIALOG','entered')
  =gfModalGen('INM34005B34000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_ENTERED,loFormSet.GetHeaderText("LANG_MFGENCT_ENTERED",loFormSet.HeaderAlias)))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  REPLACE Entered WITH loFormSet.lnOldVal IN (loFormSet.lcHdrFile)
  RETURN
ENDIF
IF &lcHdrFile..Entered  > &lcHdrFile..Complete
  *-Completion date cannot be prior to entered date.
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *=gfModalGen('INM34003B34000','DIALOG','Completion')
  =gfModalGen('INM34003B34000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_COMPLETION,loFormSet.GetHeaderText("LANG_MFGENCT_COMPLETION",loFormSet.HeaderAlias)))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  REPLACE Entered WITH loFormSet.lnOldVal IN (loFormSet.lcHdrFile)
  RETURN
ENDIF

IF INLIST(loFormSet.lcChoice,'P','O') AND &lcHdrFile..Entered  > &lcHdrFile..Available
  *-Available date cannot be prior to entered date.
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  * =gfModalGen('INM34003B34000','DIALOG','Available')
  =gfModalGen('INM34003B34000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_AVLBLE,loFormSet.GetHeaderText("LANG_MFGENCT_AVLBLE",loFormSet.HeaderAlias)))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  REPLACE Entered WITH loFormSet.lnOldVal IN (loFormSet.lcHdrFile)
  RETURN
ENDIF
IF INLIST(loFormSet.lcChoice,'C','L') AND &lcHdrFile..Entered  > &lcHdrFile..Start
  *-Available date cannot be prior to entered date.
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
  *=gfModalGen('INM34003B34000','DIALOG','Start')
  =gfModalGen('INM34003B34000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_START,loFormSet.GetHeaderText("LANG_MFGENCT_START",loFormSet.HeaderAlias)))
  *N000682,1 MMT 12/09/2012 Globalization changes[END]
  REPLACE Entered WITH loFormSet.lnOldVal IN (loFormSet.lcHdrFile)
  RETURN
ENDIF

FUNCTION lfvLComplete
LPARAMETERS loFormSet

lcHdrFile = loFormSet.lcHdrFile

IF INLIST(loFormSet.lcChoice,'P','O')
  IF EMPTY(&lcHdrFile..Complete)
    *-You cannot leave the Completion date empty.
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *=gfModalGen('INM34005B34000','DIALOG','Completion')
    =gfModalGen('INM34005B34000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_COMPLETION,loFormSet.GetHeaderText("LANG_MFGENCT_COMPLETION",loFormSet.HeaderAlias)))
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
    REPLACE Complete WITH loFormSet.lnOldVal IN (loFormSet.lcHdrFile)
    RETURN
  ENDIF
  IF &lcHdrFile..Entered > &lcHdrFile..Complete
    *-Completion date cannot be prior to entered date.
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *=gfModalGen('INM34003B34000','DIALOG','Completion')    
    =gfModalGen('INM34003B34000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_COMPLETION,loFormSet.GetHeaderText("LANG_MFGENCT_COMPLETION",loFormSet.HeaderAlias)))
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
    REPLACE Complete WITH loFormSet.lnOldVal IN (loFormSet.lcHdrFile)
    RETURN
  ENDIF
  SELECT (loFormSet.lcPOLine)
  lcLineKey = &lcHdrFile..Po+&lcHdrFile..cDivision+&lcHdrFile..cPurCode+&lcHdrFile..cStyGrade+;
  IIF(loFormSet.llRPGrpByST,LEFT(&lcHdrFile..Style,loFormSet.lnMajorLen),'')
  =SEEK(lcLineKey)
  REPLACE REST Complete WITH &lcHdrFile..Complete ;
   WHILE PO+cDivision+cPurCode+cStyGrade+STYLE+Dyelot = lcLineKey
ELSE
  IF EMPTY(&lcHdrFile..Start)
    *-You cannot leave the Completion date empty.
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *=gfModalGen('INM34005B34000','DIALOG','Start')    
    =gfModalGen('INM34005B34000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_START,loFormSet.GetHeaderText("LANG_MFGENCT_START",loFormSet.HeaderAlias)))
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
    REPLACE Start WITH loFormSet.lnOldVal IN (loFormSet.lcHdrFile)
    RETURN
  ENDIF
  IF &lcHdrFile..Start > &lcHdrFile..Complete
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    * =gfModalGen("TRM38291B00000","DIALOG",'Complete|start')    
    =gfModalGen("TRM38291B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_COMPLETE,loFormSet.GetHeaderText("LANG_MFGENCT_COMPLETE",loFormSet.HeaderAlias))+'|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_START,loFormSet.GetHeaderText("LANG_MFGENCT_START",loFormSet.HeaderAlias)))
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
     REPLACE Start WITH loFormSet.lnOldVal IN (loFormSet.lcHdrFile)
    RETURN
  ENDIF
ENDIF

FUNCTION lfvLAvailable
LPARAMETERS loFormSet

lcHdrFile = loFormSet.lcHdrFile

IF INLIST(loFormSet.lcChoice,'P','O')
  IF EMPTY(&lcHdrFile..Available)
    *-You cannot leave the Available date empty.
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *=gfModalGen('INM34005B34000','DIALOG','Available')    
    =gfModalGen('INM34005B34000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_AVLBLE,loFormSet.GetHeaderText("LANG_MFGENCT_AVLBLE",loFormSet.HeaderAlias)))
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
    REPLACE Available WITH loFormSet.lnOldVal IN (loFormSet.lcHdrFile)
    RETURN
  ENDIF
  IF &lcHdrFile..Entered  > &lcHdrFile..Available
    *-Available date cannot be prior to entered date.
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *=gfModalGen('INM34003B34000','DIALOG','Available')    
    =gfModalGen('INM34003B34000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_AVLBLE,loFormSet.GetHeaderText("LANG_MFGENCT_AVLBLE",loFormSet.HeaderAlias)))
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
    REPLACE Available WITH loFormSet.lnOldVal IN (loFormSet.lcHdrFile)
    RETURN
  ENDIF
  IF &lcHdrFile..Complete > &lcHdrFile..Available
    REPLACE Available WITH loFormSet.lnOldVal IN (loFormSet.lcHdrFile)
    RETURN
  ENDIF
ELSE
  IF EMPTY(&lcHdrFile..Complete)
    *-You cannot leave the Available date empty.
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *=gfModalGen('INM34005B34000','DIALOG','Complete')    
    =gfModalGen('INM34005B34000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_COMPLETE,loFormSet.GetHeaderText("LANG_MFGENCT_COMPLETE",loFormSet.HeaderAlias)))
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
    REPLACE Complete WITH loFormSet.lnOldVal IN (loFormSet.lcHdrFile)
    RETURN
  ENDIF
  IF &lcHdrFile..Complete < &lcHdrFile..Start
    *-Available date cannot be prior to entered date.
    *N000682,1 MMT 12/09/2012 Globalization changes[Start]
    *=gfModalGen("TRM38291B00000","DIALOG",'Complete|start')    
    =gfModalGen("TRM38291B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_COMPLETE,loFormSet.GetHeaderText("LANG_MFGENCT_COMPLETE",loFormSet.HeaderAlias))+'|'+;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_START,loFormSet.GetHeaderText("LANG_MFGENCT_START",loFormSet.HeaderAlias)))
    *N000682,1 MMT 12/09/2012 Globalization changes[END]
    REPLACE Complete WITH loFormSet.lnOldVal IN (loFormSet.lcHdrFile)
    RETURN
  ENDIF
  SELECT (loFormSet.lcPOLine)
  lcLineKey = &lcHdrFile..Po+&lcHdrFile..cDivision+&lcHdrFile..Season+&lcHdrFile..cYear+&lcHdrFile..cWeek+;
              SUBSTR(&lcHdrFile..STYLE,1,loFormSet.lnMajorLen)
  =SEEK(lcLineKey)
  REPLACE REST Complete WITH &lcHdrFile..Complete ;
   WHILE PO+cDivision+SEASON+cWeek+cYear+STYLE+Dyelot  = lcLineKey
ENDIF

FUNCTION lfvOrderNote
LPARAMETERS loFormSet

=NOTEPAD('B',EVALUATE(loFormSet.lcOrdLine+'.ORder'))

*! B609028,1 MMT 10/08/2009 Fix bug of can not edit p.price in line detail screen[Start]
*!*************************************************************
*! Name      : lfvGPrice
*! Developer : Mariam Mazhar
*! Date      : 10/08/2009
*! Purpose   : function to Validate gross price
*!*************************************************************
FUNCTION lfvGPrice
  *-- Temporary PO Line
  lcTmpPoLn    = loParentForm.lcLinFile

  IF loParentForm.lnPOldVal = &lcTmpPoLn..Gros_Price
    RETURN
  ENDIF

  LOCAL lnI,  laOldECst, laNewECst, laOldFCst, laNewFCst
  PRIVATE lcPMethod,lcDMethod,lcPUnMeth,lcDUnMeth,lnCurrUnt1, lnCurrUnt2
  DIMENSION laOldECst[7], laNewECst[7], laOldFCst[7], laNewFCst[7]
  STORE 0  TO laOldECst, laNewECst, laOldFCst, laNewFCst
  STORE '' TO lcPMethod,lcDMethod,lcPUnMeth,lcDUnMeth
  STORE 1  TO lnCurrUnt1, lnCurrUnt2
  *-- Get the selected price currency and rate.
  lcPriceCurr  = loParentForm.lcPCurr
  lnPricRate   = loParentForm.lnPRate
  *-- Get the selected duty currency and rate.
  lcDutyCurr   = loParentForm.lcDCurr
  lnDutyRate   = loParentForm.lnDRate
  *-- Style major
  lcStyMajr    = SUBSTR(EVALUATE(loParentForm.lcLinFile+'.Style'),1,loParentForm.lnMajorLen)
  lcInvType    = '0001'



	 SELECT (lcTmpPoLn)
	 SCATTER MEMO MEMVAR

   IF loParentForm.lEditStyPrice AND STYLE.cPriceCur = lcPriceCurr AND;
      (STYLE.NICOST1 <> 0) AND (m.Gros_Price <> STYLE.NICOST1)
     lcMsg = "Style"
     IF gfModalGen('QRM36227B34001','DIALOG',lcMsg+"|"+STR(STYLE.NICOST1,2)+"|"+lcMsg) = 2
       m.Gros_Price = STYLE.NICOST1
       REPLACE Gros_Price WITH m.Gros_Price
     ENDIF
   ENDIF


	 FOR lnI = 1 TO 7
	   lcI = STR(lnI,1)
	   STORE EVALUATE('m.nICost'+lcI) TO laOldECst[lnI],laNewECst[lnI]
	   STORE EVALUATE('m.nFCost'+lcI) TO laOldFCst[lnI],laNewFCst[lnI]
	 ENDFOR

	 m.nfCost1 = ROUND(m.Gros_Price*(100-m.Disc_Pcnt)/100,3)
	 m.nICost1 = lfEquivCost('1',m.nFCost1,lnPricRate,lnCurrUnt1,lcPriceCurr,lcDutyCurr)
   REPLACE nfCost1 WITH m.nfCost1* TotQty,;
           niCost1 WITH m.niCost1* TotQty,;
           nNetPrc WITH m.nfCost1

   laNewECst[1] = niCost1
   laNewFCst[1] = nfCost1

	 =lfCalEstCst(@laOldECst,@laNewECst,@laOldFCst,@laNewFCst,loParentForm.llMulCurr,.T.,lcPriceCurr,lnPricRate,lcDutyCurr)

*!*************************************************************
*! Name      : lfvPriceDisc
*! Developer : Mariam Mazhar
*! Date      : 10/08/2009
*! Purpose   : function to Validate Price discount
*!*************************************************************
FUNCTION lfvPriceDisc
  *-- Temporary PO Line
  lcTmpPoLn    = loParentForm.lcLinFile

  IF loParentForm.lnPOldVal = &lcTmpPoLn..Disc_Pcnt
    RETURN
  ENDIF


 LOCAL lnI,  laOldECst, laNewECst, laOldFCst, laNewFCst
  PRIVATE lcPMethod,lcDMethod,lcPUnMeth,lcDUnMeth,lnCurrUnt1, lnCurrUnt2
  DIMENSION laOldECst[7], laNewECst[7], laOldFCst[7], laNewFCst[7]
  STORE 0  TO laOldECst, laNewECst, laOldFCst, laNewFCst
  STORE '' TO lcPMethod,lcDMethod,lcPUnMeth,lcDUnMeth
  STORE 1  TO lnCurrUnt1, lnCurrUnt2
  *-- Get the selected price currency and rate.
  lcPriceCurr  = loParentForm.lcPCurr
  lnPricRate   = loParentForm.lnPRate
  *-- Get the selected duty currency and rate.
  lcDutyCurr   = loParentForm.lcDCurr
  lnDutyRate   = loParentForm.lnDRate
  *-- Style major
  lcStyMajr    = SUBSTR(EVALUATE(loParentForm.lcLinFile+'.Style'),1,loParentForm.lnMajorLen)
  lcInvType    = '0001'
  SELECT (lcTmpPoLn)
  SCATTER MEMO MEMVAR
  FOR lnI = 1 TO 7
    lcI = STR(lnI,1)
    STORE EVALUATE('m.nICost'+lcI) TO laOldECst[lnI],laNewECst[lnI]
    STORE EVALUATE('m.nFCost'+lcI) TO laOldFCst[lnI],laNewFCst[lnI]
  ENDFOR

  m.nfCost1 = ROUND(m.Gros_Price*(100-m.Disc_Pcnt)/100,3)
  m.nICost1 = lfEquivCost('1',m.nFCost1,lnPricRate,lnCurrUnt1,lcPriceCurr,lcDutyCurr)
  REPLACE nfCost1 WITH m.nfCost1* TotQty,;
          niCost1 WITH m.niCost1* TotQty,;
          nNetPrc WITH m.nfCost1

  laNewECst[1] = niCost1
  laNewFCst[1] = nfCost1

  =lfCalEstCst(@laOldECst,@laNewECst,@laOldFCst,@laNewFCst,loParentForm.llMulCurr,.T.,lcPriceCurr,lnPricRate,lcDutyCurr)
*!*************************************************************
*! Name      : lfvNetPrc
*! Developer : Mariam Mazhar
*! Date      : 10/08/2009
*! Purpose   : function to Validate Net Price
*!*************************************************************
FUNCTION lfvNetPrc
  *-- Temporary PO Line
  lcTmpPoLn    = loParentForm.lcLinFile

  IF loParentForm.lnPOldVal = &lcTmpPoLn..nNetPrc
    RETURN
  ENDIF

  LOCAL lnI,  laOldECst, laNewECst, laOldFCst, laNewFCst
  PRIVATE lcPMethod,lcDMethod,lcPUnMeth,lcDUnMeth,lnCurrUnt1, lnCurrUnt2
  DIMENSION laOldECst[7], laNewECst[7], laOldFCst[7], laNewFCst[7]
  STORE 0  TO laOldECst, laNewECst, laOldFCst, laNewFCst
  STORE '' TO lcPMethod,lcDMethod,lcPUnMeth,lcDUnMeth
  STORE 1  TO lnCurrUnt1, lnCurrUnt2
  *-- Get the selected price currency and rate.
  lcPriceCurr  = loParentForm.lcPCurr
  lnPricRate   = loParentForm.lnPRate
  *-- Get the selected duty currency and rate.
  lcDutyCurr   = loParentForm.lcDCurr
  lnDutyRate   = loParentForm.lnDRate
  *-- Style major
  lcStyMajr    = SUBSTR(EVALUATE(loParentForm.lcLinFile+'.Style'),1,loParentForm.lnMajorLen)
  lcInvType    = '0001'
  SELECT (lcTmpPoLn)
  SCATTER MEMO MEMVAR
  FOR lnI = 1 TO 7
    lcI = STR(lnI,1)
    STORE EVALUATE('m.nICost'+lcI) TO laOldECst[lnI],laNewECst[lnI]
    STORE EVALUATE('m.nFCost'+lcI) TO laOldFCst[lnI],laNewFCst[lnI]
  ENDFOR

  m.nfCost1 = m.nNetPrc
  m.Disc_pcnt = MAX(IIF(m.Gros_Price=0,0,100-m.nFCost1*100/m.Gros_Price),0)

  IF m.Disc_pcnt = 0
    m.Gros_Price = m.nNetPrc
  ENDIF

  IF m.Gros_Price = 0 AND m.Disc_pcnt = 0
     m.Gros_Price = m.nNetPrc
  ENDIF

  IF m.Gros_Price = 0
     m.Disc_pcnt = 0
  ENDIF

  m.nICost1 = lfEquivCost('1',m.nFCost1,lnPricRate,lnCurrUnt1,lcPriceCurr,lcDutyCurr)
  REPLACE nfCost1 WITH m.nfCost1* TotQty,;
          niCost1 WITH m.niCost1* TotQty,;
          Disc_pcnt WITH m.Disc_pcnt,;
          Gros_Price  WITH m.Gros_Price
  laNewECst[1] = niCost1
  laNewFCst[1] = nfCost1

  =lfCalEstCst(@laOldECst,@laNewECst,@laOldFCst,@laNewFCst,loParentForm.llMulCurr,.T.,lcPriceCurr,lnPricRate,lcDutyCurr)
*!*************************************************************
*! Name      : lfvNetPrc
*! Developer : Mariam Mazhar
*! Date      : 10/08/2009
*! Purpose   : function to Keep Price field old value
*!*************************************************************
FUNCTION lfWPFlds
LPARAMETERS lnFldId
lcTmpPoLn    = loParentForm.lcLinFile
DO CASE
  CASE lnFldId= 1
    loParentForm.lnPOldVal = &lcTmpPoLn..Gros_Price
  CASE lnFldId= 2
    loParentForm.lnPOldVal = &lcTmpPoLn..Disc_pcnt
  CASE lnFldId= 3
    loParentForm.lnPOldVal = &lcTmpPoLn..nNetPrc
ENDCASE
RETURN .T.
*! B609028,1 MMT 10/08/2009 Fix bug of can not edit p.price in line detail screen[End]

*! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[Start]
*!*************************************************************
*! Name      : lfvLnProjTemp
*! Developer : Mariam Mazhar
*! Date      : 12/02/2009
*! Purpose   : function to Validate Line Template
*!*************************************************************
FUNCTION lfvLnProjTemp
LPARAMETERS loFormSet
lnAlias = SELECT()
IF !USED('PMPTHHD')
  =gfOpenTable( 'PMPTHHD', 'PMPTHHD','SH')
ENDIF
SELECT 'PMPTHHD'

IF !GFSEEK(EVALUATE(loParentForm.lcPOLine+'.CDEFTEMPL'),'PMPTHHD')
  GFSEEK('')
  *N000682,1 MMT 12/09/2012 Globalization changes[Start]
*!*    lcBrFields = "Cpath_id  :H='Template'," +;
*!*                "Cpath_dsc :H='Description'"
*!*    IF gfBrows('','Cpath_id','laFields','Templates')
  lcBrFields = "Cpath_id  :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_TEMPLATE,loFormSet.GetHeaderText("LANG_MFGENCT_TEMPLATE",loFormSet.HeaderAlias))+"'," +;
              "Cpath_dsc :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_TEMPLATEDESC,loFormSet.GetHeaderText("LANG_MFGENCT_TEMPLATEDESC",loFormSet.HeaderAlias))+"'"
  IF gfBrows('','Cpath_id','laFields',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFGENCT_MTEMPLATE,loFormSet.GetHeaderText("LANG_MFGENCT_MTEMPLATE",loFormSet.HeaderAlias)))
  *N000682,1 MMT 12/09/2012 Globalization changes[End]
     REPLACE CDEFTEMPL WITH PMPTHHD.Cpath_id IN (loParentForm.lcPOLine)
  ENDIF
ENDIF
*! E302650,1 MMT 12/02/2009 Give user ability to create project per PO/CT Line[End]
