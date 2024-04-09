*:************************************************************************
*: Program file  : POGENSD.PRG
*: Program desc. : Generate P/O form orders for Studio Direct
*: For screen    : POGENSD.SCX (0,1,2,3)
*:         System: Aria Advantage Series - Version 2.7
*:         Module: Style Purchase Order (PO)
*:      Developer: AHMED MAHER (AMH)
*: Tracking Job Number : C102562
*:************************************************************************
*: Example             : DO POGENSD
*:************************************************************************
*: Modifications       : 
*: C102610,1 AMH 04/24/2002 Add new selection criteria on the option 
*: C102610,1                grid to allow the user to filter the sales 
*: C102610,1                order upon selecting them by using the SO status.
*: C102673,1 AMH 07/21/2002 Add option for agg. date.
*: B606454,1 AMH 09/04/2002 Fix the problem of not creating a PO according to the aggregation date.
*:************************************************************************
DIMENSION laColors[1,2],laPercet[8],laStySeg[1],laWareType[2],laDefProc[10]

DIMENSION laHdFlt[1] , laFxFlt[1] , laVrFlt[1]
STORE "" TO lcOrderFil
STORE .F. TO llRetToSel , llSelMode , llOrdUse
lcOldNdx  = ''
lcTktTtl  = "Purchase Orders"
lcSrcTtl  = "Order Lines"
lcBrTtl1  = 'Details of Purchase Order'
lcBrTtl2  = 'Quantity Allocated from Order Lines'
lnSessNo  = gnProgCopy           && This is to hold the no of the opened session form global variable
lcSession = SPACE(1)             && This is to hold the session id for the uncompleted session
lcUserID  = PADR(gcUser_id, 10)  && This is to hold the user id
lnUnCmSeRc = 0                   && This is to hold the uncomplete session record no in the "uncmsess" file
lnStep     = 0                   && This is to hold the save step number
lcScrMode  = ""                  && This is to hold the screen mode to be used in the uncomplete session
*-- Array to take the percentage of generation
STORE 100 TO laPercet

*-- Variable to hold the total qtys to be generated
STORE  0  TO lnTotal1,lnTotal2,lnTotal3,lnTotal4,lnTotal5,lnTotal6,lnTotal7,lnTotal8
lcOldCtkNo = ''
*-- To hold the record number when browsing
STORE  0  TO lnOrdRecNo,lnCutRecNo,lnStyRecNo

*-- Hold the lenght of the color and its start position in case
*-- there is a color segment in style stucture
STORE  0  TO lnColorStr,lnColorLen

*-- Hold the unit of the foreign curr.
STORE  1  TO lnUnit1,lnUnit2,lnPRate,lnDRate

*-- Hold the number of term,ship,term
STORE  1  TO lnTerm,lnShip,lnType,puType

STORE .F. TO llBrowse,llColorExt,llChecked

*-- To check if the first time to run the option grid
STORE .T. TO llFirst

STORE ''  TO lnOldVal,lcModal,lcSort,;
  lcSZ1,lcSZ2,lcSZ3,lcSZ4,lcSZ5,lcSZ6,lcSZ7,lcSZ8,lcScalDesc,;
  lcVenName,lcCont,lcPhone,lcHdrFile,lcLinFile

STORE '' TO  lcPOH,lcPOLine,lcOrdLine,lcCutPick,lcCuttktL,lcCuttktH,;
  lcTmpOrd,lcPrintAll,lcOrdGroup,lcFabrics,lcStyUnAll,lcStyTmp

STORE '' TO  lcWinCh1,lcWinC11,lcWinC12,lcWinC13,lcWinC14,lcWinC15

STORE '' TO  laColors,lcVendor,lcWareCode,lcPCurr,lcDCurr,cMajorPic,lnMajorLen,;
  lcFirstCT,lcLastCt,lcIType1,lcIType2,lcIType3,lcIType4,;
  lcIType5,lcSelFile,lcProgID,lcProgTxt

STORE .F. TO llRPUnAWip,llRpBOPFab,llAllocate,llRpBoSty,llRpBrnApp,llRpEmbApp
STORE '*' TO lcSign1,lcSign2

*C102610,1 AMH Initilaizing the varible of new option [Start]
STORE .F. TO llRpOpnOnl
*C102610,1 AMH [End]

DIMENSION laSeason[1], laTerm[1], laDiv[1], laCodInfo [2,10] ,laShip[1], laGroup[1]
STORE "" TO laSeason,laTerm, laDiv, laCodInfo,laShip,laGroup,lcRpStyle
DECLARE laFromBrow[2]
laFromBrow[1] = lcTktTtl
laFromBrow[2] = lcSrcTtl

lcGenPrompt = "Gene\<rate C/T"

*-- To call the lpshow when starting th program
llNoShow  = .F.

*------- llDispPric  -> hold setting variable to display selling price & gross margin
*------- lnTotQty    -> Hold TOTAl Qty Of one Order
*------- lnSelPrice  -> Hold Average Selling Price
*------- lnAllQty    -> Hold All Qty For All Orders
*------- lnRotSub    -> Hold Cost Price Or Selling price Depend on llStyMark
*------- lnGrosMrgn  -> Hold Gross Margin

llDispPric = .F.
llStyMark  = .F.
llDispPric = gfGetMemVar('M_PoDspPrc')
llStyMark  = gfGetMemVar('M_stymark')   ='T'
IF llDispPric
  STORE 0 TO lnTotQty,lnSelPrice,lnAllQty,lnRotSub,lnGrosMrgn
ENDIF

*--Get the values from the memory variable
llWareHous = ALLTRIM(gfGetMemVar('M_WareHouse'))= 'Y'
llDyelot   = ALLTRIM(gfGetMemVar('M_DYELOT'))   = 'Y'
llEditExRt = gfGetMemVar('LLEDITEXRA')
llMulCurr  = gfGetMemVar('llMulCurr')
lcMSLbl1   = gfGetMemVar('M_CMSLbl1')
lcMSLbl2   = gfGetMemVar('M_CMSLbl2')
lcMSLbl3   = gfGetMemVar('M_CMSLbl3')
lcMSLbl4   = gfGetMemVar('M_CMSLbl4')
lcMSLbl5   = gfGetMemVar('M_CMSLbl5')
lcISLbl1   = gfGetMemVar('M_CISLbl1')
lcISLbl2   = gfGetMemVar('M_CISLbl2')
lcISLbl3   = gfGetMemVar('M_CISLbl3')
lcISLbl4   = gfGetMemVar('M_CISLbl4')
lcISLbl5   = gfGetMemVar('M_CISLbl5')
lcIType1   = gfGetMemVar('M_cIType1')
lcIType2   = gfGetMemVar('M_cIType2')
lcIType3   = gfGetMemVar('M_cIType3')
lcIType4   = gfGetMemVar('M_cIType4')
lcIType5   = gfGetMemVar('M_cIType5')
llFDyelot   = gfGetMemVar('M_MATDYE')='Y'

llGManualy = IIF(gcAct_Appl = 'MF',(gfGetMemVar('M_GENCTNUM') = "Y"),;
                                   (gfGetMemVar('M_GENSTORN') = "Y"))

DECLARE laWareType[2],laOGVrFlt[1]
laWareType[1] = 'Single'
laWareType[2] = 'Multiple'
laOGVrFlt =''
*-- For the local save
laDefProc[9]  = .F.
*-- For the local close
laDefProc[10] = .F.

*-- Picture of the style major.
lcMajorPic = gfItemMask("PM")

*-- Lenght of the the style major.
lnMajorLen = LEN(lcMajorPic)

*-- lcProgId is used to be stored in the "uncmsess" file
lcProgID  = PADR("POFROMORD",10)
lcProgTxt = 'Generate PO from orders'

*-- Variables that need to be saved and restored when detecting an
*-- uncomplete program session.

DIMENSION laVars[25]
laVars[01] = "lcScrMode"
laVars[02] = "llFirst"
laVars[03] = "lcLinFile"
laVars[04] = "lcHdrFile"
laVars[05] = "lcSelFile"
laVars[06] = "lnShip"
laVars[07] = "lnTerm"
laVars[08] = "lcPCurr"
laVars[09] = "lcDCurr"
laVars[10] = "lnPRate"
laVars[11] = "lnDRate"
laVars[12] = "lcVendor"
laVars[13] = "laPercet[1]"
laVars[14] = "laPercet[2]"
laVars[15] = "laPercet[3]"
laVars[16] = "laPercet[4]"
laVars[17] = "laPercet[5]"
laVars[18] = "laPercet[6]"
laVars[19] = "laPercet[7]"
laVars[20] = "laPercet[8]"
laVars[21] = "lcWareCode"
laVars[22] = "lnUnCmSeRc"
laVars[23] = "llRPUnAWip"
laVars[24] = "llRpBOPFab"
laVars[25] = "llAllocate"

*--Add and icon for the scope in the control pannel
DECLARE laPanelObj[1,3]
STORE '' TO laPanelObj
laPanelObj[1,1] = 'pbScope'
laPanelObj[1,2] = gcBmpHome+'SCOPE.BMP'
laPanelObj[1,3] = [DISABLE VALID lfvScope()]

*-- Initialze the arrays for terms,division, and season
laCodInfo[1,01] = "CTERMCODE"    && Field Name
laCodInfo[1,02] = "laTerm"       && Array Name
laCodInfo[1,03] = "lnTerm"       && Popup Name
laCodInfo[1,04] = ""             && Popup Status  ("D"->Default,"A"->All)
laCodInfo[1,05] = .F.            && Include "N/A" (.T.->Yes,.F.,No)
laCodInfo[1,06] = .F.            && Include "ALL" (.T.->Yes,.F.,No)
laCodInfo[1,07] = ""             && Alternative File (For default val.)
laCodInfo[1,08] = ""             && Use this index for the Alternative file.
laCodInfo[1,09] = ""             && Seek this expretion.
laCodInfo[1,10] = "CTERMCODE"        && Alternative Field Name

laCodInfo[2,01] = "SHIPVIA   "
laCodInfo[2,02] = "laShip"
laCodInfo[2,03] = "lnShip"
laCodInfo[2,04] = ""
laCodInfo[2,05] = .F.
laCodInfo[2,06] = .F.
laCodInfo[2,07] = ""
laCodInfo[2,08] = ""
laCodInfo[2,09] = ""
laCodInfo[2,10] = "SHIPVIA"
llFromBom = .F.

*******************************************************************************

IF !gfSetup()
  RETURN
ENDIF
*******************************************************************************
IF !WEXIST(gcBaseWind)
  *-- Get the information of the major part for the style sturct.
  lcMjrTtl   = ALLTRIM(gfItemMask('HM'))+'...'
  lcNMjrTl   = ALLTRIM(gfItemMask('HN'))+'...'
  lnMjorCnt  = gfItemMask("SM")

  *-- Check if the color is one of the segment and get its information
  *-- (lenght, start position))
  IF !lfCheckMaj()
    *--Item structure not found, Cannot Proceed.
    =gfModalGen('QRM42080B42001','DIALOG','Item structure not found.')
    glQuitting = .T.
    RETURN
  ENDIF
  =gfItemMask(@laStySeg)
  STORE "" TO laStyCSeg , laStyNSeg
  FOR lnCnt = lnMjorCnt + 1 TO ALEN(laStySeg,1)
    IF laStySeg[lnCnt , 1] = "C"
      *-- Flag to know if there is color in the style code strucure.
      llColorExt = .T.
      *-- Var. hold the start position of the color segment in the style code strucure.
      lnColorStr = laStySeg[lnCnt , 4]
      *-- Var. hold the color segment lenght in the style code strucure.
      lnColorLen = LEN(laStySeg[lnCnt , 3])
    ENDIF
  ENDFOR

  lcWinCh1 = gfTempName()
  lcWinC11 = gfTempName()
  lcWinC12 = gfTempName()
  lcWinC13 = gfTempName()
  lcWinC14 = gfTempName()
  lcWinC15 = gfTempName()

  lcPOH    = gfTempName()  && temprory PosHdr
  lcPOLine = gfTempName()  && temprory PosLine

  lcCutPick  = gfTempName()  && temporary pick ticket
  lcOrdLine  = gfTempName()  && temporary order line
  lcTmpOrd   = gfTempName()  && Temporary allocated/unallocated order lines
  lcPrintAll = gfTempName()  && Temporary allocated/unallocated order lines
  lcOrdGroup = gfTempName()  && Temporary Order Groups File Name
  lcFabrics  = gfTempName()  && Temporary Fabric/Color/Dyelot File Name
  lcStyUnAll = gfTempName()
  lcLinFile  = lcPOLine      &&&& Variable to hold the header file
  lcHdrFile  = lcPOH
  lcSession  = gfsequence('CSESSION')
ELSE
  IF laScrMode[4] AND llFromBom
    laScrMode    = .F.
    laScrMode[1] = .T.
    llFromBom = .F.
  ENDIF
ENDIF

PUSH KEY

llReBrowse = .T.
=lfClearKey()
STORE '' TO lcBrowseTl
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

*-- Call of the main screen
DO (gcScrDir+gcWinAppl+'\POGENSD.SPX')

RELEASE WINDOWS (lcSrcTtl),(lcTktTtl),(lcTktTtl),(lcSrcTtl)
POP KEY

*-- Erase all the created temporary files, close all the conditionally
*-- opened aliases.
IF glQuitting
  =lfUpdUnCmS("I", SPACE(0))
  IF USED(lcPOH)
    USE IN (lcPOH)
    ERASE (gcWorkDir+lcPOH+".DBF")
    ERASE (gcWorkDir+lcPOH+".CDX")
    ERASE (gcWorkDir+lcPOH+".FPT")
  ENDIF
  IF USED(lcPOLine)
    USE IN (lcPOLine)
    ERASE (gcWorkDir+lcPOLine+".DBF")
    ERASE (gcWorkDir+lcPOLine+".CDX")
  ENDIF
  IF USED(lcCutPick)
    USE IN (lcCutPick)
    ERASE (gcWorkDir+lcCutPick+".DBF")
    ERASE (gcWorkDir+lcCutPick+".CDX")
  ENDIF
  IF USED(lcOrdLine)
    USE IN (lcOrdLine)
    ERASE (gcWorkDir+lcOrdLine+".DBF")
    ERASE (gcWorkDir+lcOrdLine+".CDX")
    ERASE (gcWorkDir+lcOrdLine+".FPT")
  ENDIF
  IF USED(lcTmpOrd)
    USE IN (lcTmpOrd)
    ERASE (gcWorkDir+lcTmpOrd+".DBF")
    ERASE (gcWorkDir+lcTmpOrd+".CDX")
  ENDIF
  IF USED(lcOrdGroup)
    USE IN (lcOrdGroup)
    ERASE (gcWorkDir+lcOrdGroup+".DBF")
    ERASE (gcWorkDir+lcOrdGroup+".CDX")
  ENDIF
  IF USED(lcFabrics)
    USE IN (lcFabrics)
    ERASE (gcWorkDir+lcFabrics+".DBF")
    ERASE (gcWorkDir+lcFabrics+".CDX")
  ENDIF
  IF USED(lcStyUnAll)
    USE IN (lcStyUnAll)
    ERASE (gcWorkDir+lcStyUnAll+".DBF")
    ERASE (gcWorkDir+lcStyUnAll+".CDX")
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfDeactMain
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Deactivate main Screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfDeactMain()
*!*************************************************************
FUNCTION lfDeactMain

IF WONTOP()=lcSrcTtl .OR. WONTOP()=lcTktTtl
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  glFromBrow = .T.
  IF WONTOP()=lcSrcTtl
    ON KEY LABEL TAB     DO lpMovTab  WITH lcWinC13,'pbSelect'
    ON KEY LABEL BACKTAB DO lpBackTab WITH 'gwcContrl1','pbCls'
  ELSE
    ON KEY LABEL TAB     DO lpMovTab  WITH lcWinC15,'pbGenerate'
    ON KEY LABEL BACKTAB DO lpBackTab WITH lcWinC13,IIF(laScrMode[3],'pbInvert','ibToBrow')
  ENDIF
ELSE
  =lfReadAct()
ENDIF

RETURN .F.

*!*************************************************************
*! Name      : lfBrowCut
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Browse function for the header file
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfBrowCut()
*!*************************************************************
FUNCTION lfBrowCut
PRIVATE lcBrowStr

SELECT (lcHdrFile)
lnCutRecNo=RECNO()
lcBrowStr = "CMARK=IIF(RECNO()=lnCutRecNo,'>',' '):H=' ',PO :H='PO#' :P='999999' :V=lfvBroWCt() :W=lfwOldCtkt(),;
  VENDOR:R,CDIVISION:R:H='Division',"+IIF(llWareHous,"CWARECODE:P='@!':H='ShipTo.':V=lfvWareH():W=lfwOldVals(),","")+;
  "ShipVia:R,CTERMCODE:R:H='Terms',Entered:V=(!EMPTY(Entered) AND Entered <= Complete):F:E='Entered date must be less than or equal completion date',"+;
  "Complete:V=(!EMPTY(Complete) AND Complete>=Entered):F:E='Completion date must be greater than or equal Entered date',"+;
  "nStyOrder:R :H='Order',totord:H='Allocated':R,totcost=nFcost1+nFcost2+nFcost3+nFcost4+nFcost5:H='Tot. Cost',"+;
  "nFcost1:H=lcISLbl1:R,nFcost2:H=lcISLbl2:R,nFcost3:H=lcISLbl3:R,nFcost4:H=lcISLbl4:R,nFcost5:R:H=lcISLbl5"
BROWSE FIELDS &lcBrowStr;
  NOAPPEND ;
  NOWAIT   ;
  NOMENU   ;
  NOCLEAR  ;
  TITLE lcTktTtl ;
  WHEN lfCutWhen() ;
  WINDOW (lcWinC11) IN WINDOW (gcBaseWind)

*!*************************************************************
*! Name      : lfCutWhen
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : When fumction for the browse of the header file
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfCutWhen()
*!*************************************************************
FUNCTION lfCutWhen

lnCutRecNo=RECNO(lcHdrFile)
SHOW WINDOW (lcTktTtl) REFRESH SAME

*!*************************************************************
*! Name      : lfBrowOrd
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Browse of order lines returned form the selection criteria
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfBrowOrd()
*!*************************************************************
FUNCTION lfBrowOrd
PRIVATE lcBrowStr

SELECT (lcOrdLine)
lnOrdRecNo=RECNO()
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER
lcBrowStr = 'TotDb=IIF(RECNO()=lnOrdRecNo,">"," "):H=" ",cSelect :H=" ":R,Order:R,Account:H="Account":R,CUSTOMER.BTNAME:H="Account Name":R,Style:R,'+;
  'nOpn1=Qty1-Cut1:H="Open1":R,nOpn2=Qty2-Cut2:H="Open2":R,nOpn3=Qty3-Cut3:H="Open3":R,nOpn4=Qty4-Cut4:H="Open4":R,nOpn5=Qty5-Cut5:H="Open5":R,nOpn6=Qty6-Cut6:H="Open6":R,nOpn7=Qty7-Cut7:H="Open7":R,'+;
  'nOpn8=Qty8-Cut8:H="Open8":R,nTotOpn=totQty-totcut:H="Tot. Open":R,QTY1:H="Ord 1":R,QTY2:H="Ord 2":R,QTY3:H="Ord 3":R,QTY4:H="Ord 4":R,QTY5:H="Ord 5":R,QTY6:H="Ord 6":R,QTY7:H="Ord 7":R,QTY8:H="Ord 8":R,'+;
  'totQty:H="Tot. Ord":R,Cut1:H="Cut 1":R,Cut2:H="Cut 2":R,Cut3:H="Cut 3":R,'+;
  'Cut4:H="Cut 4":R,Cut5:H="Cut 5":R,Cut6:H="Cut 6":R,Cut7:H="Cut 7":R,Cut8:H="Cut 8":R,totcut:H="Tot. Cut":R, Start:H="Start Date":R, Complete:H="Complete Date":R'
BROWSE FIELDS &lcBrowStr;
  NOAPPEND ;
  NOWAIT   ;
  NOMENU   ;
  NOCLEAR  ;
  TITLE lcSrcTtl  ;
  WHEN lfOrdWhen() ;
  WINDOW (lcWinC12) IN WINDOW (gcBaseWind) ;
  VALID lfReadAct()

*!*************************************************************
*! Name      : lfOrdWhen
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : When function of the order lines browse
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfOrdWhen()
*!*************************************************************
FUNCTION lfOrdWhen

lcStatus = IIF(laScrMode[3] AND !llAllocate,'ENABlE','DISABLE')
SHOW GET pbSelect,1 PROMPT IIF(EMPTY(&lcOrdLine..cSelect),"\<Select","Un\<Select") &lcStatus
=SEEK(&lcOrdLine..Style,'Style') AND SEEK('S'+Style.Scale,'Scale')
lcScalDesc = Scale.cScl_Desc
FOR lnCount = 1 TO 8
  lcCount = STR(lnCount,1)
  lcSZ&lcCount = Scale.SZ&lcCount
ENDFOR
lnOrdRecNo = RECNO(lcOrdLine)
SHOW WINDOW (lcSrcTtl) REFRESH SAME
=lfReFresh(lcWinC14)

*!*************************************************************
*! Name      : lfBrowLines
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Browse of the lines file
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfBrowLines()
*!*************************************************************
FUNCTION lfBrowLines
PRIVATE lnAlias,lcBrowFilds

lnAlias = SELECT()
SELECT (lcPOLine)

*C102673,1 AMH Add option for agg. date [Start]
*SET KEY TO &lcPOH..PO+&lcPOH..CSTYBASE+&lcPOH..CPURCODE+DTOS(&lcPOH..DCOMPLETE)+&lcPOH..CVENDOR+&lcPOH..CDIVISION
*=SEEK(&lcPOH..PO+&lcPOH..CSTYBASE+&lcPOH..CPURCODE+DTOS(&lcPOH..DCOMPLETE)+&lcPOH..CVENDOR+&lcPOH..CDIVISION)
SET KEY TO &lcPOH..PO+&lcPOH..CSTYBASE+&lcPOH..CPURCODE+DTOS(&lcPOH..DAGGDATE)+&lcPOH..CVENDOR+&lcPOH..CDIVISION
=SEEK(&lcPOH..PO+&lcPOH..CSTYBASE+&lcPOH..CPURCODE+DTOS(&lcPOH..DAGGDATE)+&lcPOH..CVENDOR+&lcPOH..CDIVISION)
*C102673,1 AMH [End]

lnRecBrow1 = RECNO()
lcBrowFilds = "CMARK=IIF(RECNO()=lnRecBrow1,'>',' '):H=' ',Style  :R," +;
  IIF(llDyelot,"Dyelot :H='Dyelot':W=lfwBrDylot():V=lfvDyelot():P='@!':F,"," ")+;
  "Qty1 :H='Qty 1':W=lfwBrOldVal():V=lfvCTQty(),"+;
  "Qty2 :H='Qty 2':W=lfwBrOldVal():V=lfvCTQty(),"+;
  "Qty3 :H='Qty 3':W=lfwBrOldVal():V=lfvCTQty(),"+;
  "Qty4 :H='Qty 4':W=lfwBrOldVal():V=lfvCTQty(),"+;
  "Qty5 :H='Qty 5':W=lfwBrOldVal():V=lfvCTQty(),"+;
  "Qty6 :H='Qty 6':W=lfwBrOldVal():V=lfvCTQty(),"+;
  "Qty7 :H='Qty 7':W=lfwBrOldVal():V=lfvCTQty(),"+;
  "Qty8 :H='Qty 8':W=lfwBrOldVal():V=lfvCTQty(),"+;
  "TotQty :H='Tot. Po':R,nCost1 :H=lcISLbl1:R,"+;
  "nCost2 :H=lcISLbl2:R,nCost3 :H=lcISLbl3:R,"+;
  "nCost4 :H=lcISLbl4:R,nCost5 :H=lcISLbl5:R,"+;
  "nTtCst = nCost1+nCost2+nCost3+nCost4+nCost5:H='Tot.Cost':P='999999999999.99':R"
IF llDispPric
  lcBrowFilds =  lcBrowFilds + [,nSelPrice :H='Selling Price' :R:P='999999999.99',;
    nGrosMrgn :H='Gross Margin' :R:P='9999.99']
ENDIF

GOTO TOP
BROWSE FIELDS &lcBrowFilds;
  NOAPPEND ;
  NOWAIT   ;
  NOMENU   ;
  NOCLEAR  ;
  NODELETE ;
  TITLE lcBrTtl1;
  WHEN lfwPick();
  WINDOW POGENS22 IN WINDOW POGENSD2
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfBrOrdLin
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Browse for the temp. cutpick file
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfBrOrdLin()
*!*************************************************************
FUNCTION lfBrOrdLin
PRIVATE lnAlias,lcBrowFilds

lnAlias = SELECT()
SELECT (lcCutPick)
lnRecBrow2 = RECNO()
lcBrowF1 = "CMARK=IIF(RECNO()=lnRecBrow2,'>',' '):H=' ',Order:H='Order#':R,"+;
  "Qty1:H='Cut 1':6:W=lfwBrOldVal():V=lfvOrQty(),"+;
  "Qty2:H='Cut 2':6:W=lfwBrOldVal():V=lfvOrQty(),"+;
  "Qty3:H='Cut 3':6:W=lfwBrOldVal():V=lfvOrQty(),"+;
  "Qty4:H='Cut 4':6:W=lfwBrOldVal():V=lfvOrQty(),"+;
  "Qty5:H='Cut 5':6:W=lfwBrOldVal():V=lfvOrQty(),"+;
  "Qty6:H='Cut 6':6:W=lfwBrOldVal():V=lfvOrQty(),"+;
  "Qty7:H='Cut 7':6:W=lfwBrOldVal():V=lfvOrQty(),"+;
  "Qty8:H='Cut 8':6:W=lfwBrOldVal():V=lfvOrQty(),"+;
  "TotQty:H='Tot.Cut':R"
lcBrowF2 =  ",nCost1= TotQty * STYLE.nICost1:H=lcISLbl1 :R,"+;
  "nCost2 = TotQty * STYLE.nICost2 :H=lcISLbl2 :R,"+;
  "nCost3 = TotQty * STYLE.nICost3 :H=lcISLbl3 :R,"+;
  "nCost4 = TotQty * STYLE.nICost4 :H=lcISLbl4 :R,"+;
  "nCost5 = TotQty * STYLE.nICost5 :H=lcISLbl5 :R,"+;
  "nTtCst = TotQty *(STYLE.nICost1 + STYLE.nICost2 + ;
  STYLE.nICost3 + STYLE.nICost4 + STYLE.nICost5) :H='Total Cost':R"
lcBrowFilds = lcBrowF1+lcBrowF2

BROWSE FIELDS &lcBrowFilds;
  NOAPPEND ;
  NOWAIT   ;
  NOMENU   ;
  NOCLEAR  ;
  NODELETE ;
  WHEN lfPickWhen();
  TITLE lcBrTtl2;
  WINDOW POGENS23 IN WINDOW POGENSD2
SELECT (lnAlias)

*!*************************************************************
*! Name      : lpShow
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Show procedure for the objects of the screen with different modes
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :
*!*************************************************************
PROCEDURE lpShow

IF llReBrowse
  =lfBrowOrd()
  =lfBrowCut()
  llReBrowse = .F.
ENDIF
SHOW GET ibToBrow ENABLE
SHOW GET ibToHdr  ENABLE
SHOW GET pbSlct   DISABLE
SHOW GET pbEdt    DISABLE
SHOW GET pbDlt    DISABLE

DO CASE
CASE laScrMode[1]
  IF !llChecked
    IF lfChkUnCmS()
      RETURN
    ENDIF
  ELSE
    lcScrMode = "S"
  ENDIF
  
  IF llSelMode 
    =lfRetToSel()
    *-- IF the temp order line not empty allow user to select anoter 
    *-- order or group of orders without go to OG by go to Edit mode [Begin]
    IF llRetToSel
      STORE .F. TO laScrMode
      laScrMode[3] = .T.
      STORE .F. TO llRetToSel , llSelMode
      DO lpShow
    ENDIF
  ENDIF
  
  STORE gcBaseCurr TO lcPCurr,lcDCurr
  STORE ''  TO lcSZ1,lcSZ2,lcSZ3,lcSZ4,lcSZ5,lcSZ6,lcSZ7,lcSZ8,lcScalDesc
  STORE 1   TO lnPRate,lnDRate
  STORE 100 TO laPercet
  STORE .F. TO llAllocate
  IF llFirst
    =lfvScope()
  ENDIF
  IF !EMPTY(lcFirstCT)
    SHOW GET pbNote,1  PROMPT '\<Notes' ENABLE
    SHOW GET pbHeadr,1 PROMPT '\<P/O'   ENABLE
  ENDIF
  GO TOP IN  (lcHdrFile)
  IF EOF(lcHdrFile)
    SHOW GET pbScope ENABLE
  ELSE
    SHOW GET pbScope DISABLE
  ENDIF
CASE laScrMode[2]
  llSelMode = .T.
  SHOW GET pbSlct ENABLE
  IF !EMPTY(lcFirstCT)
    SHOW GET pbNote,1  PROMPT '\<Notes' ENABLE
    SHOW GET pbHeadr,1 PROMPT '\<P/O'   ENABLE
  ENDIF

CASE laScrMode[3]
  lcScrMode = "E"
  llCUpDate = .T.
  SHOW GET pbSav DISABLE
  SHOW GET pbGenerate,1 PROMPT "\<Generate P/O" ENABLE
  SHOW GET pbDetail DISABLE
  IF llAllocate
    SHOW GET pbSelect  DISABLE
    SHOW GET pbSelAll  DISABLE
    SHOW GET pbSelNone DISABLE
    SHOW GET pbInvert  DISABLE
    SHOW GET pbScope   DISABLE
    SHOW GET pbNote,1  PROMPT 'Clear Allo\<cation' ENABLE
    SHOW GET pbHeadr,1 PROMPT 'Allocated \<Orders' ENABLE
  ELSE
    =lfvSelect('')
    IF llRpBOPFab
      SHOW GET pbHeadr,1 PROMPT 'Allocated \<Orders' DISABLE
    ELSE
      SHOW GET pbHeadr,1 PROMPT '\<P/O' DISABLE
    ENDIF
    SHOW GET pbScope ENABLE
  ENDIF
  SHOW GET pbCls,1 PROMPT gcBmpHome+"Cancel.BMP"
  =lfUpdVars()
CASE laScrMode[4]
  lcScrMode = 'A'
  llCUpDate = .T.
  SHOW GET pbDetail  ENABLE
  SHOW GET pbScope   DISABLE
  SHOW GET pbSelect  DISABLE
  SHOW GET pbSelAll  DISABLE
  SHOW GET pbSelNone DISABLE
  SHOW GET pbInvert  DISABLE
  SHOW GET pbSav     ENABLE
  SHOW GET pbGenerate,1 PROMPT "\<Clear P/O"
  SHOW GET pbNote,1     PROMPT IIF(llRpBOPFab,IIF(llAllocate,'Clear Allo\<cation','Allo\<cate'),'\<Notes') DISABLE
  SHOW GET pbHeadr,1    PROMPT IIF(llRpBOPFab,'Allocated \<Orders','\<P/O') DISABLE
  SHOW GET pbCls,1 PROMPT gcBmpHome+"Cancel.BMP"
  =lfUpdVars()
ENDCASE

*!*************************************************************
*! Name      : lfvScope
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Function called for the selection criteria (Option Grid)
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvScope()
*!*************************************************************
FUNCTION lfvScope

llRetToSel = .F.

*-- Call differrent option grid in case of generate from plan
PUSH KEY
lcExpr    = gfOpGrid('POGENP',.T.)
POP KEY
lcSelFile = lcOrdLine
IF llRpBOPFab
  SHOW GET pbNote,1  PROMPT 'Allo\<cate'  DISABLE
  SHOW GET pbHeadr,1 PROMPT 'Allocated \<Orders' DISABLE
ELSE
  SHOW GET pbNote,1  PROMPT '\<Notes' DISABLE
  SHOW GET pbHeadr,1 PROMPT '\<P/O' DISABLE
ENDIF
=gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')
=gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
=gfOpenFile(gcDataDir+'CODES',gcDataDir+'cCode_No','SH')
=gfwCodePop(@laCodInfo, "CTERMCODE", "D")
=gfwCodePop(@laCodInfo, "SHIPVIA",   "D")

SELECT (lcSelFile)
llNothing = IIF(llFirst,lfAdUnCmSR(),'')
llFirst = .F.
IF lfSelOrd()
  =lfOrdWhen()
  STORE .F. TO laScrMode
  laScrMode[3] = .T.
ENDIF
DO lpShow

*!*************************************************************
*! Name      : lfwOption
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Function called from the when of the option grid
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwOption()
*!*************************************************************
FUNCTION lfwOption

PRIVATE lnClrHElm , lnVrFltElm , llRefresh

*IF the company uses style structure containing a color sector
IF !llColorExt

  lnClrHElm = ASCAN(laOGFieldN , 'CODES.CCODE_NO')      && Variable to hold the number of the array elment for the color in the Option grid Available for filter array

  *IF The Warehouse is in the Available for filter fields
  IF lnClrHElm > 0
    =gfADel(@laOGFieldN , lnClrHElm , 1)
    =gfADel(@laOGFieldH , lnClrHElm , 1)
  ENDIF    && End of IF

  llRefresh = .F.          && Flag to know if we need to refresh the Option Grid or not

  DO WHILE .T.
    lnVrFltElm = ASCAN(laOGVrFlt , 'CODES.CCODE_NO')      && Variable to hold the number of the array element for the color in the Option grid Variable filter array
    lnVrFltElm = IIF(lnVrFltElm = 0 , 0 , ASUBSCRIPT(laOGVrFlt , lnVrFltElm , 1))

    *IF There is a row for the color in the Variable filter
    IF lnVrFltElm > 0
      =gfADel(@laOGVrFlt , lnVrFltElm , 1)
      lnOGVarFl = lnOGVarFl - 1        && Variable to hold the number of rows in the variable filter [Used by Option grid]
      llRefresh = .T.
    ELSE
      EXIT
    ENDIF    && End of IF
  ENDDO

  *IF We are to refresh the option grid
  IF llRefresh
    CLEAR READ
    RETURN
  ENDIF    && End of IF
ENDIF    && End of IF
=lfvBoSty()

*!*************************************************************
*! Name      : lfvStyle
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Function called from the validation of the style option grig
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvStyle()
*!*************************************************************
FUNCTION lfvStyle
PRIVATE lcObjNam , lcObjVal
SET ORDER TO TAG STYLE IN STYLE
lcObjNam = SYS(18)
lcObjVal = SUBSTR(EVALUATE(SYS(18)),1,LEN(lcMajorPic))
IF !EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'STYLE')
  llBrowse = .T.
  lcObjVal = gfStyBrw('M',"","",.F.)
  llBrowse = .F.
ENDIF
&lcObjNam = lcObjVal

*!*************************************************************
*! Name      : lfvDetail
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
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

*-- Set a relation between the lines file and temp. cutpick and ordhdr file
SELECT STYLE
SET ORDER TO TAG STYLE
SET RELATION TO 'S'+SCALE INTO SCALE
SELECT (lcLinFile)
SET RELATION TO STYLE INTO STYLE

*C102673,1 AMH Add option for agg. date [Start]
*SET RELATION TO PO+CSTYBASE+CPURCODE+DTOS(DCOMPLETE)+CVENDOR+CDIVISION+STYLE+CWARECODE+DYELOT+CORDERLN INTO (lcCutPick) ADDITIVE
SET RELATION TO PO+CSTYBASE+CPURCODE+DTOS(DAGGDATE)+CVENDOR+CDIVISION+STYLE+CWARECODE+DYELOT+CORDERLN INTO (lcCutPick) ADDITIVE
*C102673,1 AMH [End]

SELECT (lcCutPick)
SET RELATION TO 'O'+ORDER INTO ORDHDR ADDITIVE
STORE 0 TO lnRecBrow1,lnRecBrow2
SELECT(lcLinFile)
GOTO TOP
PUSH KEY
=lfClearKey()
laTerm[1]=EVALUATE(lcPoH+'.CTERMCODE')+'-'+gfCodDes(EVALUATE(lcPoH+'.CTERMCODE'), 'CTERMCODE ')
DO (gcScrDir+gcWinAppl+'\POGENSD2.SPX')
SELECT (lcLinFile)
SET RELATION TO
SET KEY TO

SELECT (lcCutPick)
SET RELATION TO
POP KEY

*!*************************************************************
*! Name      : lfDeactDet
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Deactivate main Screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfDeactDet()
*!*************************************************************
FUNCTION lfDeactDet
IF WONTOP()=lcBrTtl1  .OR. WONTOP()=lcBrTtl2
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  glFromBrow = .T.
  IF WONTOP()=lcBrTtl1
    ON KEY LABEL TAB     DO lpMovTab  WITH 'POGENS24','ibToPick'
    ON KEY LABEL BACKTAB DO lpBackTab WITH 'POGENS24','pbClose'
  ELSE
    ON KEY LABEL TAB     DO lpMovTab  WITH 'POGENS24','pbClose'
    ON KEY LABEL BACKTAB DO lpBackTab WITH 'POGENS24','ibToLine'
  ENDIF
ELSE
  glFromBrow = .F.
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfSROrder
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Called from the OG of the order
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfSROrder()
*!*************************************************************
FUNCTION lfSROrder
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'
    SELECT ORDHDR
    LOCATE
  CASE lcParm = 'R'
    SELECT ORDHDR
ENDCASE
*-- end of lfSROrder.

*!*************************************************************
*! Name      : lfSelOrd
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Called from scope function to set the filter on the ordline file
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfSelOrd()
*!*************************************************************
FUNCTION lfSelOrd
PRIVATE lcStyOrd ,lnalias
*-- If the user return form the selection grid with cancel
IF TYPE('lcExpr')='L'
  RETURN(.F.)
ENDIF
WAIT 'Selecting order lines.' WINDOW NOWAIT
lcSort = "'OrdLine.Order'"

=gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH')

SELECT STYLE
lcStyOrd = TAG()
SET ORDER TO TAG STYLE

SELECT (lcOrdLine)
DELETE ALL

*-- Set the required relations
SELECT ORDLINE
SET KEY TO 'O'

SET RELATION TO Ordline.cOrdType+ Ordline.Order INTO Ordhdr ADDITIVE
SET RELATION TO Ordline.style INTO STYLE ADDITIVE
SET RELATION TO 'N'+'COLOR     '+SUBSTR(Ordline.Style,lnColorStr,lnColorLen);
  INTO CODES ADDITIVE
SET RELATION TO ORDER+STR(LINENO,6) INTO PLINFO ADDITIVE

*-- Get the required records from the ordline file due to the expression
*-- returned form the selection grid
SELECT ORDLINE
lcScanExp = ""
IF llOrdUse
  SELECT (lcOrderFil)
  LOCATE
  SELECT ORDLINE
  SEEK "O" + EVALUATE(lcOrderFil+'.ORDER')
  
  *C102610,1 AMH Remove the PLINFO check from the for expration [Start]
  *lcScanExp = 'REST WHILE cOrdType+Order+STR(LineNo,6) >= "O"+EVALUATE(lcOrderFil+".ORDER")'+;
              ' FOR '+ lcExpr + ' AND SEEK(ORDER,lcOrderFil) AND !STYLE.MAKE AND !EMPTY(STYLE.VENDOR) AND '+;
              '!(Ordhdr.Status $ "XCB") AND TOTQTY-TOTCUT > 0'+;
              ' AND IIF(llRpBoSty,STYLE=ALLTRIM(lcRpStyle),.T.)'+;
              ' AND IIF(LPLBL,PLINFO.LBRAPP=llRpBrnApp AND PLINFO.LEMBAPP=llRpEmbApp,.T.)'
  lcScanExp = 'REST WHILE cOrdType+Order+STR(LineNo,6) >= "O"+EVALUATE(lcOrderFil+".ORDER")'+;
              ' FOR '+ lcExpr + ' AND SEEK(ORDER,lcOrderFil) AND !STYLE.MAKE AND !EMPTY(STYLE.VENDOR) AND '+;
              '!(Ordhdr.Status $ "XCB") AND TOTQTY-TOTCUT > 0'+;
              ' AND IIF(llRpBoSty,STYLE=ALLTRIM(lcRpStyle),.T.)'
ELSE
  *lcScanExp = " FOR "+ lcExpr + " AND !STYLE.MAKE AND !EMPTY(STYLE.VENDOR) AND "+;
              "!(Ordhdr.Status $ 'XCB') AND TOTQTY-TOTCUT > 0"+;
              " AND IIF(llRpBoSty,STYLE=ALLTRIM(lcRpStyle),.T.)"+;
              " AND IIF(LPLBL,PLINFO.LBRAPP=llRpBrnApp AND PLINFO.LEMBAPP=llRpEmbApp,.T.)"
  lcScanExp = " FOR "+ lcExpr + " AND !STYLE.MAKE AND !EMPTY(STYLE.VENDOR) AND "+;
              "!(Ordhdr.Status $ 'XCB') AND TOTQTY-TOTCUT > 0"+;
              " AND IIF(llRpBoSty,STYLE=ALLTRIM(lcRpStyle),.T.)"
  *C102610,1 AMH [End]
  
ENDIF

*C102610,1 AMH Add the new criteria to select open sales order only [Start]
lcScanExp = lcScanExp + ' AND IIF(llRpOpnOnl,ORDHDR.STATUS="O",.T.)'
*C102610,1 AMH [End]

SCAN &lcScanExp
  
  *C102610,1 AMH Check on the PLINFO here [Start]
  IF LPLBL
    DO CASE
      CASE !llRpBrnApp .AND. !llRpEmbApp
        IF PLINFO.LBRAPP .OR. PLINFO.LEMBAPP
          LOOP
        ENDIF
      CASE !llRpBrnApp .AND. llRpEmbApp
        IF !PLINFO.LEMBAPP
          LOOP
        ENDIF
      CASE llRpBrnApp .AND. !llRpEmbApp
        IF !PLINFO.LBRAPP
          LOOP
        ENDIF
      CASE llRpBrnApp .AND. llRpEmbApp
        IF !PLINFO.LBRAPP .OR. !PLINFO.LEMBAPP
          LOOP
        ENDIF
    ENDCASE
  ENDIF
  *C102610,1 AMH [End]
  
  SCATTER MEMVAR
  m.cDivision = ORDHDR.cDivision
  m.cSortExp  = EVAL(lcSort)
  m.cStyBase  = IIF(llRpBoSty,SUBSTR(STYLE,1,5),'')
  m.cPurCode  = STYLE.cPurCode
  m.dComplete = ORDHDR.COMPLETE
  
  *C102673,1 AMH Add option for agg. date [Start]  
  *B606454,1 AMH 09/04/2002 Change the m.dComplete to be m.dAggDate [Start]
  *m.dComplete = ORDHDR.DAGGDATE
  m.dAggDate = ORDHDR.DAGGDATE
  *B606454,1 AMH 09/04/2002 [End]
  *C102673,1 AMH [End]
  
  m.cVendor   = STYLE.VENDOR
  IF !SEEK(m.cSortExp+ORDLINE.ORDER+STR(ORDLINE.LINENO,6),lcOrdLine)
    INSERT INTO (lcOrdLine) FROM MEMVAR
  ENDIF
ENDSCAN
SELECT ORDLINE
SET KEY TO

*-- Reset the relation
SET RELATION TO
SELECT STYLE
SET RELATION TO
SET ORDER TO TAG &lcStyOrd

SELECT(lcOrdLine)
GOTO TOP
IF EOF()
  =lfUpdUnCmS('I',SPACE(0) )
  =lfvSelect('')  
  SHOW GET pbSelect  DISABLE
  SHOW WINDOW (lcSrcTtl) REFRESH SAME
  laScrMode = .F.
  laScrMode[1] = .T.
  =gfModalGen('INM38129B38018','DIALOG','orders')
  RETURN(.F.)
ENDIF

*!*************************************************************
*! Name      : lfvSelect
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Called from the selection buttons
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvSelect()
*!*************************************************************
FUNCTION lfvSelect
PARAMETER lcSelect
PRIVATE lnAlias,lnRecNo

lnAlias = SELECT()
SELECT (lcSelFile)
lnRecNo = RECNO()
DO CASE
CASE lcSelect = 'S'
  REPLACE cSelect WITH IIF(cSelect="�",'',"�")
CASE lcSelect = 'A'
  REPLACE ALL cSelect WITH "�"
CASE lcSelect = 'N'
  REPLACE ALL cSelect WITH " "
CASE lcSelect = 'V'
  REPLACE ALL cSelect WITH IIF(cSelect="�",'',"�")
OTHERWISE
ENDCASE
lcOrdTag = TAG()
SET ORDER TO TAG 'TICKET'
IF SEEK("�")
  SUM REST (QTY1-CUT1),(QTY2-CUT2),(QTY3-CUT3),(QTY4-CUT4),;
    (QTY5-CUT5),(QTY6-CUT6),(QTY7-CUT7),(QTY8-CUT8) ;
    TO   lnTotal1,lnTotal2,lnTotal3,lnTotal4,lnTotal5,lnTotal6,lnTotal7,lnTotal8 ;
    WHILE cSelect = "�"
  SHOW GET pbSelNone ENABLE
  SHOW GET pbGenerate,1 PROMPT "\<Generate P/O" ENABLE
  lcNote=IIF(llRpBOPFab,IIF(llAllocate,'Clear Allo\<cation','Allo\<cate'),'\<Notes')
  IF llRpBOPFab AND laScrMode[3]
    SHOW GET pbNote,1 PROMPT lcNote ENABLE
  ELSE
    SHOW GET pbNote,1 PROMPT lcNote DISABLE
  ENDIF
ELSE
  STORE 0 TO lnTotal1,lnTotal2,lnTotal3,lnTotal4,lnTotal5,lnTotal6,lnTotal7,lnTotal8
  SHOW GET pbSelNone  DISABLE
  SHOW GET pbGenerate DISABLE
  SHOW GET pbNote     DISABLE
ENDIF
IF SEEK(" ")
  SHOW GET pbSelAll ENABLE
ELSE
  SHOW GET pbSelAll DISABLE
ENDIF
IF SEEK("�") OR SEEK(" ")
  SHOW GET pbInvert ENABLE
ELSE
  SHOW GET pbInvert DISABLE
ENDIF
SET ORDER TO TAG &lcOrdTag
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF
SHOW GET pbSelect,1 PROMPT IIF(EMPTY(cSelect),"\<Select","Un\<Select")
*-- refresh the selection browse
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvGenerate
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Called from the Genetrate or Clear button
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvGenerate()
*!*************************************************************
FUNCTION lfvGenerate
PRIVATE lnAlias

llRetToSel = .F.
=gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
=lfCrtDTemp()

lnAlias = SELECT()
GOTO TOP IN (lcHdrFile)
llStartGen = .F.

IF EOF(lcHdrFile)
  *-- Generate mode
  lnType = puType
  =lfvGenCtPO()
ELSE
  *-- Clear generated mode
  =lfDelFiles(.F.)
ENDIF
=lfCutWhen()
GOTO TOP IN (lcSelFile)
=lfOrdWhen()
STORE .T. TO llcUpdate
STORE .F. TO laScrMode
IF llStartGen
  laScrMode[4] = .T.
ELSE
  laScrMode[3] = .T.
ENDIF
SELECT(lnAlias)
DO lpShow

*!*************************************************************
*! Name      : lfvGenCtPO
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Function to check if the generation can be done or not
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvGenCtPO()
*!*************************************************************
FUNCTION lfvGenCtPO

*-- Check if the if any generation ratios entered will
*-- generate positive quantities or not
FOR lnCount = 1 TO 8
  lcElemNo = ALLTRIM(STR(lnCount))
  IF laPercet[lnCount]>0 .AND. ;
      INT((laPercet[lnCount] * EVALUATE('lnTotal' + lcElemNo) /100)) > 0
    llStartGen = .T.
    EXIT
  ENDIF
ENDFOR
IF EMPTY(lcWareCode)
  GO TOP IN 'WAREHOUS'
  lcWareCode = WAREHOUS.cWareCode
ENDIF

IF llStartGen
  lcFile = IIF(llRpBOPFab,lcTmpOrd,lcOrdLine)
  =lfUpdPOPik(lcFile)
  SELECT (lcHdrFile)
  DELETE ALL FOR nStyOrder=0
  GOTO TOP
  IF EOF()
    llStartGen = .F.
    =gfModalGen('INM38130B38018','DIALOG','purchase orders')
  ENDIF
ELSE
  *-- (Using the specified generation percentages will create
  *-- 'cutting tickets with zero quantities. Cannot proceed
  *-- 'with generation.)
  =gfModalGen('INM38131B38018','DIALOG','purchase orders')
ENDIF

*!*************************************************************
*! Name      : lfwOldVals
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : get the old value of the editable fields on the browse
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwOldVals()
*!*************************************************************
FUNCTION lfwOldVals

lnOldVal = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvWareH
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Called from the location field in the browse of the header file
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvWareH()
*!*************************************************************
FUNCTION lfvWareH
PRIVATE lcWareCode,lnAlias

lnAlias = SELECT()
IF !SEEK(&lcHdrFile..cWareCode,'WAREHOUS')
  SELECT WareHous
  lcWareCode = gfBrowWare(.T.)
  IF EMPTY(lcWareCode)
    lcWareCode = lnOldVal
  ENDIF
  REPLACE &lcHdrFile..cWareCode WITH lcWareCode
ENDIF
SELECT (lnAlias)
RETURN .T.
*!*************************************************************
*! Name      : lfwBrDylot
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Called from the dyelot field in the browse of the line file
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwBrDylot()
*!*************************************************************
FUNCTION lfwBrDylot

IF llRpBOPFab OR !EMPTY(PO)
  RETURN(.F.)
ENDIF
=SEEK(STYLE,'STYLE')
RETURN (STYLE.cDye_FLg='Y')

*!*************************************************************
*! Name      : lfvDyelot
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
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
  =gfModalGen('INM38132B38018','DIALOG')
ENDIF

lcDyelot = Dyelot

*C102673,1 AMH Add option for agg. date [Start]
*lcKey=PO+CSTYBASE+CPURCODE+DTOS(DCOMPLETE)+CVENDOR+CDIVISION+STYLE+CWARECODE+CORDERLN
*lcExp='CTKTNO+CSTYBASE+CPURCODE+DTOS(DCOMPLETE)+CVENDOR+CDIVISION+STYLE+CWARECODE+DYELOT+CORDERLN'
lcKey=PO+CSTYBASE+CPURCODE+DTOS(DAGGDATE)+CVENDOR+CDIVISION+STYLE+CWARECODE+CORDERLN
lcExp='CTKTNO+CSTYBASE+CPURCODE+DTOS(DAGGDATE)+CVENDOR+CDIVISION+STYLE+CWARECODE+DYELOT+CORDERLN'
*C102673,1 AMH [End]

SELECT (lcCutPick)
REPLACE ALL Dyelot WITH lcDyelot FOR EVAL(lcExp) = lcKey
SELECT(lcLinFile)

*!*************************************************************
*! Name      : lfwBrOldVal
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Called from the Qty fields in the browse of the line file
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwBrOldVal()
*!*************************************************************
FUNCTION lfwBrOldVal

IF !EMPTY(&lcLinFile..PO)
  RETURN .F.
ENDIF
IF VAL(RIGHT(SYS(18),1)) <= SCALE.CNT
  lnOldVal = EVALUATE(SYS(18))
ELSE
  RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfvCTQty
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Called from the Qty fields in the browse of the line file
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvCTQty()
*!*************************************************************
FUNCTION lfvCTQty
PRIVATE lcQtyFld, lcOrdFld, lnAddedQty

lcQtyFld = lcLinFile+'.'+SYS(18)
lcOrdFld = 'Ord'+RIGHT(lcQtyFld,1)
IF EVALUATE(lcQtyFld) < EVALUATE(lcOrdFld)
  =gfModalGen('INM38133B38018','DIALOG','P/O'+'|'+'P/O')
  REPLACE &lcQtyFld WITH lnOldVal
ELSE
  IF llRpBOPFab
    SELECT (lcFabrics)
    =SEEK(&lcLinFile..Fabric+&lcLinFile..cFabClr)
    LOCATE REST WHILE Fabric+COLOR+cPriority+Dyelot+cWareCode=&lcLinFile..Fabric+&lcLinFile..cFabClr ;
      FOR Dyelot+cWareCode = &lcLinFile..Dyelot+&lcLinFile..cFabWare
    SELECT (lcLinFile)
    IF (EVAL(lcQtyFld) - lnOldVal)*nYeild > ;
        (&lcFabrics..ONHAND-&lcFabrics..nAllocated)
      *-- Message : 38150
      *-- Not Enough Inventory available to allocate this order line
      *-- Button : 00000
      *-- Ok
      =gfModalGen('TRM38150B00000','ALERT')
      REPLACE &lcQtyFld WITH lnOldVal
      RETURN
    ENDIF
    SELECT (lcFabrics)
    REPLACE nAllocated WITH nAllocated + (EVAL(lcQtyFld)-lnOldVal)*&lcLinFile..nYeild
  ENDIF
  SELECT (lcLinFile)
  lnAddedQty = EVALUATE(lcQtyFld) - lnOldVal
  REPLACE TotQty WITH TotQty + lnAddedQty,;
    nCost1 WITH nCost1 + lnAddedQty * STYLE.nICost1,;
    nCost2 WITH nCost2 + lnAddedQty * STYLE.nICost2,;
    nCost3 WITH nCost3 + lnAddedQty * STYLE.nICost3,;
    nCost4 WITH nCost4 + lnAddedQty * STYLE.nICost4,;
    nCost5 WITH nCost5 + lnAddedQty * STYLE.nICost5
  SELECT (lcHdrFile)
  REPLACE nStyOrder WITH nStyOrder + lnAddedQty ,;
    nICost1   WITH nICost1 + lnAddedQty * STYLE.nICost1,;
    nICost2   WITH nICost2 + lnAddedQty * STYLE.nICost2,;
    nICost3   WITH nICost3 + lnAddedQty * STYLE.nICost3,;
    nICost4   WITH nICost4 + lnAddedQty * STYLE.nICost4,;
    nICost5   WITH nICost5 + lnAddedQty * STYLE.nICost5
  SELECT (lcLinFile)
  =lfPickWhen()
ENDIF
SHOW WINDOW (lcBrTtl1) REFRESH SAME

*!*************************************************************
*! Name      : lfwPick
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Called from the when of the lines browse
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwPick()
*!*************************************************************
FUNCTION lfwPick

lnRecBrow1 = RECNO(lcLinFile)
SHOW WINDOW (lcBrTtl1) REFRESH SAME
=lfPickWhen()

*!*************************************************************
*! Name      : lfPickWhen
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Called from the when of the CutPick browse
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfPickWhen()
*!*************************************************************
FUNCTION lfPickWhen

lnRecBrow2 = RECNO(lcCutPick)
SHOW WINDOW (lcBrTtl2) REFRESH SAME
=lfRefresh('POGENS24')
=lfRefresh('POGENS25')

*!*************************************************************
*! Name      : lpSavScr
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Local procedure for the save
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lpSavScr
*!*************************************************************
PROCEDURE lpSavScr

SELECT (lcHdrFile)
IF EOF()
  llCSave = .F.
ENDIF
IF llGManualy .AND. SEEK(SPACE(06))
  lcStrToAdd = "Purchase Orders"
  lnNoThing = gfModalGen('TRM38047B00000','DIALOG',lcStrToAdd)
  llCSave = .F.
  RETURN
ENDIF
=lfUpdUnCmS('O',"pbSav" )
=gfOpenFile(gcDataDir+'STYDYE', gcDataDir+'STYDYE','SH')

IF llCSave
  SET ORDER TO TAG STYLE IN STYLE
  =lfUpdPO()
ENDIF
=lfUpdUnCmS(IIF(llCSave,'C','O'),SPACE(0))
=gfCloseFile('STYDYE')

IF !llCSave
  RETURN
ENDIF
*-- if there is at least one record generated
IF !llGManualy .AND. !EMPTY(lcFirstCT)
  *-- Give the numbers of the generated P/Os or C/Ts
  =gfModalGen('INM38134B38018','DIALOG','P/O'+'|'+ALLTRIM(lcFirstCT)+"|"+ALLTRIM(lcLastCt))
ENDIF
IF llRpBOPFab
  *-- Message : 38151
  *-- Whould you like to print a process summary report?
  *-- Button : 38006
  *-- Yes,No
  IF gfModalGen('QRM38151B38006','ALERT') =1
    =lfPrnSumm()
  ENDIF
ENDIF

STORE .F. TO laScrMode
laScrMode[2] = .T.

*!*************************************************************
*! Name      : lfvOrQty
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Called from the qty fileds of browse lines
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvOrQty()
*!*************************************************************
FUNCTION lfvOrQty
PRIVATE lcQtyFld, lcNo, lnCurRecNo, lnAddedQty, lnAvailQty

lcQtyFld = SYS(18)
lcNo     = RIGHT(lcQtyFld,1)     && (1--8 according to the calling field)
IF SEEK('O'+ORDER + cOrdLine, 'ORDLINE')
  lnAvailQty = ORDLINE.&lcQtyFld - ORDLINE.Cut&lcNo
  IF (EVALUATE(lcQtyFld) < 0 .AND. gfModalGen('INM38136B38018','DIALOG')=1) .OR. ;
      (EVALUATE(lcQtyFld) > lnAvailQty .AND. ;
      gfModalGen('INM38137B38018','DIALOG',ALLTRIM(STR(lnAvailQty)))=1)

    REPLACE &lcQtyFld WITH lnOldVal
  ELSE
    lnAddedQty = EVALUATE(lcQtyFld) - lnOldVal
    IF llRpBOPFab
      SELECT (lcFabrics)
      =SEEK(&lcLinFile..Fabric+&lcLinFile..cFabClr)
      LOCATE REST WHILE Fabric+COLOR+cPriority+Dyelot+cWareCode=&lcLinFile..Fabric+&lcLinFile..cFabClr ;
        FOR Dyelot+cWareCode = &lcLinFile..Dyelot+&lcLinFile..cFabWare
      REPLACE nAllocated WITH nAllocated + lnAddedQty*&lcLinFile..nYeild
    ENDIF
    SELECT (lcLinFile)
    =RLOCK()
    REPLACE &lcQtyFld  WITH &lcQtyFld  + lnAddedQty,;
      TotQty     WITH TotQty     + lnAddedQty,;
      Ord&lcNo   WITH Ord&lcNo   + lnAddedQty,;
      TotOrd     WITH TotOrd     + lnAddedQty,;
      nCost1     WITH nCost1     + lnAddedQty * STYLE.nICost1,;
      nCost2     WITH nCost2     + lnAddedQty * STYLE.nICost2,;
      nCost3     WITH nCost3     + lnAddedQty * STYLE.nICost3,;
      nCost4     WITH nCost4     + lnAddedQty * STYLE.nICost4,;
      nCost5     WITH nCost5     + lnAddedQty * STYLE.nICost5
    UNLOCK
    SELECT (lcPOH)
    =RLOCK()
    REPLACE nStyOrder WITH nStyOrder + lnAddedQty,;
      TotOrd    WITH TotOrd    + lnAddedQty,;
      nICost1   WITH nICost1   + lnAddedQty * STYLE.nICost1,;
      nICost2   WITH nICost2   + lnAddedQty * STYLE.nICost2,;
      nICost3   WITH nICost3   + lnAddedQty * STYLE.nICost3,;
      nICost4   WITH nICost4   + lnAddedQty * STYLE.nICost4,;
      nICost5   WITH nICost5   + lnAddedQty * STYLE.nICost5
    UNLOCK
    SELECT (lcCutPick)
    =RLOCK()
    REPLACE TotQty WITH TotQty + lnAddedQty
    UNLOCK
  ENDIF
  =lfRefresh('POGENS25')
  SHOW WINDOW (lcBrTtl1) REFRESH SAME
  SHOW WINDOW (lcBrTtl2) REFRESH SAME
ENDIF

*!*************************************************************
*! Name      : lfvNote
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Calls the notes for the selected record of the generated P/Os or C/Ts
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvNote()
*!*************************************************************
FUNCTION lfvNote

SELECT (lcHdrFile)
IF EOF()
  GOTO TOP
ENDIF
lcBaseFile = 'POSHDR'
lcSydKey   = '|POSHDR'
=NotePad('P',PO)
lcBaseFile = ''
lcSydKey   = ''
llByMe =gfOpenFile(gcDataDir+'NotePad','NotePad','SH')
IF SEEK('P'+&lcHdrFile..PO,'NotePad')
  SELECT POSHDR
  =SEEK('P'+&lcHdrFile..PO)
  REPLACE lhasNotes WITH .T.
ENDIF
USE IN IIF(llByMe,'NotePad',0)

*!*************************************************************
*! Name      : lfEditHdr
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Called the screen of P/O or C/T to edit header for the generated records
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfEditHdr()
*!*************************************************************
FUNCTION lfEditHdr

IF laScrMode[3]
  =lfShowAll()
ELSE
  SELECT(lcHdrFile)
  IF EOF()
    GOTO TOP
  ENDIF
  lcCall  = 'P'
  lcParam = "'"+lcCall+"','"+&lcPOH..PO+ "'"
  DO gpDoProg WITH 'AWRPOSTYLE',.F.,'PO',lcParam
ENDIF

*!*************************************************************
*! Name      : lfChkUnCmS
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Check if there is uncomplete session
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfChkUnCmS()
*!*************************************************************
FUNCTION lfChkUnCmS
PARAMETERS llFromSetUp
PRIVATE lnAlias, llGoOut, lnReprocess

IF llFromSetUp
  llChecked = .T.
ENDIF
llGoOut = .F.
IF gfUnCompSession(lcProgID,lnsessno,lcProgTxt)
  =lfBrowOrd()
  llGoOut   = .T.
  STORE .F. TO laScrMode
  laScrMode[ATC(lcScrMode,"SVEA")] = .T.
  llCUpdate  = laScrMode[3] OR laScrMode[4]
  lcSession  = UnCmSess.cSession
  lnOldTerm  = lnTerm
  lnOldShip  = lnShip
  lnShip    = 1
  lnTerm    = 1
  =gfwCodePop(@laCodInfo, "CTERMCODE", "L")
  =gfwCodePop(@laCodInfo, "SHIPVIA", "L")
  lnShip = lnOldShip
  lnTerm = lnOldTerm
  SHOW GETS
  IF VARREAD() = 'PBSAV'
    DO lpSavscr
  ENDIF
ELSE
  =lfCrtTemp()
ENDIF
RETURN llGoOut

*!*************************************************************
*! Name      : lfAdUnCmSR
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Adding record in uncomplete session file
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfAdUnCmSR()
*!*************************************************************
FUNCTION lfAdUnCmSR
PRIVATE lnAlias

lnAlias = SELECT()
SELECT UnCmSess
IF !SEEK('I')
  APPEND BLANK
ENDIF
lnUnCmSeRc = RECNO()
BLANK
REPLACE STATUS     WITH 'O'      ,;
  cUTranType WITH lcProgID ,;
  cUserId    WITH lcUserID ,;
  cSession   WITH lcSession,;
  cProgram   WITH lcProgID ,;
  cCurrScr   WITH lcProgID ,;
  cCurrObj   WITH VARREAD(),;
  dTranDate  WITH gdSysDate,;
  cTranTime  WITH TIME()
=RLOCK()
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfUpdVars
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Update the variable string
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfUpdVars()
*!*************************************************************
FUNCTION lfUpdVars
PRIVATE lnAlias

lnAlias = SELECT()
lcFiles = "lcOrdLine," + lcOrdLine + "," +ORDER(lcOrdLine)  + ";" + ;
  "lcPOH,"     + lcPOH     + "," +ORDER(lcPOH)      + ";" + ;
  IIF(laScrMode[4],"lcCutPick,"+ lcCutPick + "," +ORDER(lcCutPick) + ";" + ;
  "lcPOLine,"  + lcPOLine+ "," +ORDER(lcPOLine)   + ";",'') + ;
  IIF(llRpBOPFab,"lcOrdGroup,"+ lcOrdGroup+ "," +ORDER(lcOrdGroup) + ";" + ;
  "lcFabrics," + lcFabrics + "," +ORDER(lcFabrics)  + ";" + ;
  "lcTmpOrd,"  + lcTmpOrd  + "," +ORDER(lcTmpOrd)   + ";",'') + ;
  IIF(llRPUnAWip,"lcStyUnAll,"+ lcStyUnAll+ "," +ORDER(lcStyUnAll) + ";",'')
SELECT UnCmSess
=RLOCK()
=IIF(lnUnCmSeRc=0,.T.,gfSavSess(lcProgID, lcFiles, @laVars))
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfUpdUnCmS
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Update the current object and the status of the session
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfUpdUnCmS()
*!*************************************************************

FUNCTION lfUpdUnCmS
PARAMETERS lcStatus,lcCurObj

PRIVATE llByMe
IF !USED('UnCmSess')
  llByMe = gfOpenFile(gcDataDir+'UnCmSess',gcDataDir+'Trans','SH')
ENDIF
IF lnUnCmSeRc <> 0
  lnAlias  = SELECT()
  SELECT UnCmSess
  GOTO lnUnCmSeRc
  REPLACE cCurrObj WITH lcCurObj,;
    STATUS   WITH lcStatus
  IF STATUS $ "IC"
    UNLOCK
  ENDIF
  SELECT(lnAlias)
ENDIF

*!*************************************************************
*! Name      : lpClsScr
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Revert & Close Screen
*!*************************************************************
*! Calls     : lfDelFiles(),lfUpdUnCmS()
*!*************************************************************
*! Parameters: none
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lpClsScr()
*!*************************************************************
PROCEDURE lpClsScr

llRetToSel = .F.
=lfDelFiles(.T.)
=lfOrdWhen() AND lfCutWhen()
IF SEEK('O'+lcProgID+lcUserID,'UnCmSess')
  lnUnCmSeRc = RECNO('UnCmSess')
ENDIF
=lfUpdUnCmS("I", SPACE(0))

*!*************************************************************
*! Name      : lfDelFiles
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
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
PARAMETERS llDelOrders

SELECT (lcHdrFile)
SCAN
  IF SEEK('P'+PO,'POSHDR')
    SELECT POSHDR
    REPLACE FLAG WITH ' '
  ENDIF
  SELECT (lcHdrFile)
  BLANK
  DELETE
ENDSCAN
IF USED(lcLinFile)
  SELECT (lcLinFile)
  SCAN
    BLANK
    DELETE
  ENDSCAN
ENDIF
IF llDelOrders
  SELECT (lcSelFile)
  SCAN
    BLANK
    DELETE
  ENDSCAN
ENDIF
IF USED(lcCutPick)
  SELECT (lcCutPick)
  SCAN
    BLANK
    DELETE
  ENDSCAN
ENDIF

*!*************************************************************
*! Name      : lfCrtTemp
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
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

SELECT OrdLine
=AFIELDS(laFilField)
lnAlen = ALEN(laFilField,1)

*C102673,1 AMH Add option for agg. date [Start]
*DIMENSION laFilField[lnAlen+13,4]
DIMENSION laFilField[lnAlen+14,4]
*C102673,1 AMH [End]

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
laFilField[lnAlen+5,1] = 'cStyBase'
laFilField[lnAlen+5,2] = 'C'
laFilField[lnAlen+5,3] = 5
laFilField[lnAlen+5,4] = 0
laFilField[lnAlen+6,1] = 'Fabric'
laFilField[lnAlen+6,2] = 'C'
laFilField[lnAlen+6,3] = 7
laFilField[lnAlen+6,4] = 0
laFilField[lnAlen+7,1] = 'cFabClr'
laFilField[lnAlen+7,2] = 'C'
laFilField[lnAlen+7,3] = 6
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
laFilField[lnAlen+12,1] = 'cVendor'
laFilField[lnAlen+12,2] = 'C'
laFilField[lnAlen+12,3] = 8
laFilField[lnAlen+12,4] = 0
laFilField[lnAlen+13,1] = 'dComplete'
laFilField[lnAlen+13,2] = 'D'
laFilField[lnAlen+13,3] = 8
laFilField[lnAlen+13,4] = 0

*C102673,1 AMH Add option for agg. date [Start]
laFilField[lnAlen+14,1] = 'dAggDate'
laFilField[lnAlen+14,2] = 'D'
laFilField[lnAlen+14,3] = 8
laFilField[lnAlen+14,4] = 0
*C102673,1 AMH [End]

DECLARE laIndex[3,2]

*C102673,1 AMH Add option for agg. date [Start]
*laIndex[1,1] = 'CSELECT+CSTYBASE+CPURCODE+DTOS(DCOMPLETE)+CVENDOR+CDIVISION'
laIndex[1,1] = 'CSELECT+CSTYBASE+CPURCODE+DTOS(DAGGDATE)+CVENDOR+CDIVISION'
*C102673,1 AMH [End]

laIndex[1,2] = 'TICKET'
laIndex[2,1] = 'cSortExp+ORDER+STR(LINENO,6)'
laIndex[2,2] = '(lcOrdLine)'
laIndex[3,1] = 'cSelect+Fabric+cFabClr+cSortExp+Order+Store+Group'
laIndex[3,2] = 'Groups'
=gfCrtTmp(lcOrdLine,@lafilfield,@laIndex)
SET ORDER TO TAG (lcOrdLine) IN (lcOrdLine)

SELECT POSHDR
=AFIELDS(lafilfield)
lnAlen = ALEN(laFilField,1)

*C102673,1 AMH Add option for agg. date [Start]
*DIMENSION laFilField[lnAlen+5,4]
DIMENSION laFilField[lnAlen+6,4]
*C102673,1 AMH [End]

laFilField[lnAlen+1,1] = 'cStyBase'
laFilField[lnAlen+1,2] = 'C'
laFilField[lnAlen+1,3] = 5
laFilField[lnAlen+1,4] = 0
lafilfield[lnAlen+2,1] = 'nSteps'
lafilfield[lnAlen+2,2] = 'N'
lafilfield[lnAlen+2,3] = 2
lafilfield[lnAlen+2,4] = 0
lafilfield[lnAlen+3,1] = 'cTmpPo'
lafilfield[lnAlen+3,2] = 'C'
lafilfield[lnAlen+3,3] = 6
lafilfield[lnAlen+3,4] = 0
lafilfield[lnAlen+4,1] = 'dComplete'
lafilfield[lnAlen+4,2] = 'D'
lafilfield[lnAlen+4,3] = 8
lafilfield[lnAlen+4,4] = 0
lafilfield[lnAlen+5,1] = 'cVendor'
lafilfield[lnAlen+5,2] = 'C'
lafilfield[lnAlen+5,3] = 8
lafilfield[lnAlen+5,4] = 0

*C102673,1 AMH Add option for agg. date [Start]
lafilfield[lnAlen+6,1] = 'dAggDate'
lafilfield[lnAlen+6,2] = 'D'
lafilfield[lnAlen+6,3] = 8
lafilfield[lnAlen+6,4] = 0
*C102673,1 AMH [End]

*C102673,1 AMH Add option for agg. date [Start]
*=gfCrtTmp(lcPOH,@lafilfield,[PO+CSTYBASE+CPURCODE+DTOS(DCOMPLETE)+CVENDOR+CDIVISION],[CVENDIV])
=gfCrtTmp(lcPOH,@lafilfield,[PO+CSTYBASE+CPURCODE+DTOS(DAGGDATE)+CVENDOR+CDIVISION],[CVENDIV])
*C102673,1 AMH [End]

*!*************************************************************
*! Name      : lfCrtDTemp
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
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

IF !USED(lcCutPick)
  =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTPICK','SH')
  =AFIELDS(lafilfield)
  lnAlen = ALEN(lafilfield,1)
  
  *C102673,1 AMH Add option for agg. date [Start]
  *DIMENSION lafilfield[lnAlen+10,4]
  DIMENSION lafilfield[lnAlen+11,4]
  *C102673,1 AMH [End]
  
  lafilfield[lnAlen+1,1] = 'CDIVISION'
  lafilfield[lnAlen+1,2] = 'C'
  lafilfield[lnAlen+1,3] = 6
  lafilfield[lnAlen+1,4] = 0
  lafilfield[lnAlen+2,1] = 'SEASON'
  lafilfield[lnAlen+2,2] = 'C'
  lafilfield[lnAlen+2,3] = 6
  lafilfield[lnAlen+2,4] = 0
  lafilfield[lnAlen+3,1] = 'cPurCode'
  lafilfield[lnAlen+3,2] = 'C'
  lafilfield[lnAlen+3,3] = 6
  lafilfield[lnAlen+3,4] = 0
  lafilfield[lnAlen+4,1] = 'CWARECODE'
  lafilfield[lnAlen+4,2] = 'C'
  lafilfield[lnAlen+4,3] = 6
  lafilfield[lnAlen+4,4] = 0
  laFilField[lnAlen+5,1] = 'Dyelot'
  laFilField[lnAlen+5,2] = 'C'
  laFilField[lnAlen+5,3] = 10
  laFilField[lnAlen+5,4] = 0
  lafilfield[lnAlen+6,1] = 'nSteps'
  lafilfield[lnAlen+6,2] = 'N'
  lafilfield[lnAlen+6,3] = 2
  lafilfield[lnAlen+6,4] = 0
  laFilField[lnAlen+7,1] = 'cStyBase'
  laFilField[lnAlen+7,2] = 'C'
  laFilField[lnAlen+7,3] = 5
  laFilField[lnAlen+7,4] = 0
  lafilfield[lnAlen+8,1] = 'dComplete'
  lafilfield[lnAlen+8,2] = 'D'
  lafilfield[lnAlen+8,3] = 8
  lafilfield[lnAlen+8,4] = 0
  lafilfield[lnAlen+9,1] = 'cVendor'
  lafilfield[lnAlen+9,2] = 'C'
  lafilfield[lnAlen+9,3] = 8
  lafilfield[lnAlen+9,4] = 0
  lafilfield[lnAlen+10,1] = 'cOrderLn'
  lafilfield[lnAlen+10,2] = 'C'
  lafilfield[lnAlen+10,3] = 6
  lafilfield[lnAlen+10,4] = 0
  
  *C102673,1 AMH Add option for agg. date [Start]
  lafilfield[lnAlen+11,1] = 'dAggDate'
  lafilfield[lnAlen+11,2] = 'D'
  lafilfield[lnAlen+11,3] = 8
  lafilfield[lnAlen+11,4] = 0
  *C102673,1 AMH [End]
  
  DECLARE laIndex[1,2]
  
  *C102673,1 AMH Add option for agg. date [Start]
  *laIndex[1,1] = 'CTKTNO+CSTYBASE+CPURCODE+DTOS(DCOMPLETE)+CVENDOR+CDIVISION+STYLE+CWARECODE+DYELOT+CORDERLN'
  laIndex[1,1] = 'CTKTNO+CSTYBASE+CPURCODE+DTOS(DAGGDATE)+CVENDOR+CDIVISION+STYLE+CWARECODE+DYELOT+CORDERLN'
  *C102673,1 AMH [End]
  
  laIndex[1,2] = 'CSTYCLR'
  =gfCrtTmp(lcCutPick,@lafilfield,@laIndex)
ENDIF
IF !USED(lcPOLine)
  =gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLN','SH')
  =AFIELDS(lafilfield)
  lnAlen = ALEN(lafilfield,1)
  
  *C102673,1 AMH Add option for agg. date [Start]
  *DIMENSION lafilfield[lnAlen+11,4]
  DIMENSION lafilfield[lnAlen+12,4]
  *C102673,1 AMH [End]
  
  lafilfield[lnAlen+1,1] = 'CDIVISION'
  lafilfield[lnAlen+1,2] = 'C'
  lafilfield[lnAlen+1,3] = 6
  lafilfield[lnAlen+1,4] = 0
  lafilfield[lnAlen+2,1] = 'cPurCode'
  lafilfield[lnAlen+2,2] = 'C'
  lafilfield[lnAlen+2,3] = 6
  lafilfield[lnAlen+2,4] = 0
  lafilfield[lnAlen+3,1] = 'nSteps'
  lafilfield[lnAlen+3,2] = 'N'
  lafilfield[lnAlen+3,3] = 2
  lafilfield[lnAlen+3,4] = 0
  laFilField[lnAlen+4,1] = 'Fabric'
  laFilField[lnAlen+4,2] = 'C'
  laFilField[lnAlen+4,3] = 7
  laFilField[lnAlen+4,4] = 0
  laFilField[lnAlen+5,1] = 'cFabClr'
  laFilField[lnAlen+5,2] = 'C'
  laFilField[lnAlen+5,3] = 6
  laFilField[lnAlen+5,4] = 0
  laFilField[lnAlen+6,1] = 'cFabWare'
  laFilField[lnAlen+6,2] = 'C'
  laFilField[lnAlen+6,3] = 6
  laFilField[lnAlen+6,4] = 0
  laFilField[lnAlen+7,1] = 'nYeild'
  laFilField[lnAlen+7,2] = 'N'
  laFilField[lnAlen+7,3] = 7
  laFilField[lnAlen+7,4] = 3
  laFilField[lnAlen+8,1] = 'cStyBase'
  laFilField[lnAlen+8,2] = 'C'
  laFilField[lnAlen+8,3] = 5
  laFilField[lnAlen+8,4] = 0
  lafilfield[lnAlen+9,1] = 'dComplete'
  lafilfield[lnAlen+9,2] = 'D'
  lafilfield[lnAlen+9,3] = 8
  lafilfield[lnAlen+9,4] = 0
  lafilfield[lnAlen+10,1] = 'cVendor'
  lafilfield[lnAlen+10,2] = 'C'
  lafilfield[lnAlen+10,3] = 8
  lafilfield[lnAlen+10,4] = 0
  lafilfield[lnAlen+11,1] = 'cOrderLn'
  lafilfield[lnAlen+11,2] = 'C'
  lafilfield[lnAlen+11,3] = 6
  lafilfield[lnAlen+11,4] = 0
  
  *C102673,1 AMH Add option for agg. date [Start]
  lafilfield[lnAlen+12,1] = 'dAggDate'
  lafilfield[lnAlen+12,2] = 'D'
  lafilfield[lnAlen+12,3] = 8
  lafilfield[lnAlen+12,4] = 0
  *C102673,1 AMH [End]
  
  *C102673,1 AMH Add option for agg. date [Start]
  *=gfCrtTmp(lcPOLine,@lafilfield,[PO+CSTYBASE+CPURCODE+DTOS(DCOMPLETE)+CVENDOR+CDIVISION+STYLE+DYELOT+CORDERLN],[CSTYCLR])
  =gfCrtTmp(lcPOLine,@lafilfield,[PO+CSTYBASE+CPURCODE+DTOS(DAGGDATE)+CVENDOR+CDIVISION+STYLE+DYELOT+CORDERLN],[CSTYCLR])
  *C102673,1 AMH [End]
  
ENDIF

*!*************************************************************
*! Name      : lfUpdPOPik
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Generate P/O's
*!*************************************************************
*! Calls     : gfGetExSin()
*!*************************************************************
*! Parameters: lcOrdFile : Order line temporary file name
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfUpdPOPik(lcOrdFile)
*!*************************************************************
FUNCTION lfUpdPOPik
PARAMETERS lcOrdFile
PRIVATE lnAlias,lcStyOrd,lcPUntSin,lcDUntSin,lcPExSign,lcDExSign,lcDivision,;
  lcPurCode,lnTCost1,lnTCost2,lnTCost3,lnTCost4,lnTCost5,;
  lnOrder,lnAllocat,lnTFCost1,lnTFCost2,lnTFCost3,lnTFCost4,lnTFCost5
PRIVATE lnCutQty1,lnCutQty2,lnCutQty3,lnCutQty4,lnCutQty5,lnCutQty6,lnCutQty7,lnCutQty8,;
  lnOrdQty1,lnOrdQty2,lnOrdQty3,lnOrdQty4,lnOrdQty5,lnOrdQty6,lnOrdQty7,lnOrdQty8

WAIT 'Generating Purchase Orders. Please standby....' WINDOW NOWAIT
STORE '/' TO lcPUntSin,lcDUntSin
lcPExSign = gfGetExSin(@lcPUntSin,lcPCurr)
lcDExSign = gfGetExSin(@lcDUntSin,lcDCurr)
lnAlias = SELECT()
SELECT STYLE
lcStyOrd = TAG()
SET ORDER TO TAG STYLE

SELECT (lcOrdFile)
lcOrdTag = TAG()
SET ORDER TO TAG TICKET
SELECT (lcOrdFile)
=SEEK("�")

*C102673,1 AMH Add option for agg. date [Start]
*DO WHILE CSELECT+CSTYBASE+CPURCODE+DTOS(DCOMPLETE)+CVENDOR+CDIVISION="�"
DO WHILE CSELECT+CSTYBASE+CPURCODE+DTOS(DAGGDATE)+CVENDOR+CDIVISION="�"
*C102673,1 AMH [End]

  lcDivision = cDivision
  lcPurCode  = cPurCode
  lcStyBase  = cStyBase
  
  *C102673,1 AMH Add option for agg. date [Start]
  *lcComplete = DTOS(dComplete)
  lcComplete = DTOS(dAggDate)
  *C102673,1 AMH [End]
  
  lcVendor   = cVendor
  =SEEK(lcVendor,'APVENDOR')

  SELECT (lcPOH)
  IF !SEEK(SPACE(6)+lcStyBase+lcPurCode+lcComplete+lcVendor+lcDivision)
    APPEND BLANK
    REPLACE cStyType   WITH 'P' ,;
            Vendor     WITH lcVendor ,;
            STATUS     WITH 'H',;
            CDIVISION  WITH lcDivision,;
            cPurCode   WITH lcPurCode ,;
            ENTERED    WITH gdSysDate ,;
            COMPLETE   WITH ENTERED+90,;
            SHIPVIA    WITH laShip[lnShip,2],;
            CTERMCODE  WITH APVENDOR.CTERMCODE,;
            cWareCode  WITH lcWareCode,;
            cStyBase   WITH lcStyBase,;
            dComplete  WITH EVALUATE(lcOrdFile+'.DCOMPLETE'),;
            cVendor    WITH lcVendor,;
            cPriceCur  WITH lcPCurr ,;
            nPriceRat  WITH lnPRate ,;
            nCurrUnit  WITH lnUnit1 ,;
            cDutyCur   WITH lcDCurr ,;
            nDutyRat   WITH lnDRate ,;
            nDCurUnit  WITH lnUnit2 ,;
            lMultiWare WITH (lnType=2)
    
    *C102673,1 AMH Add option for agg. date [Start]
    REPLACE DAGGDATE   WITH EVALUATE(lcOrdFile+'.DAGGDATE')
    *C102673,1 AMH Add option for agg. date [Start]
    
  ENDIF
  STORE 0 TO lnTCost1,lnTCost2,lnTCost3,lnTCost4,lnTCost5,lnOrder,lnAllocat,;
             lnTFCost1,lnTFCost2,lnTFCost3,lnTFCost4,lnTFCost5

  IF llDispPric
    STORE 0 TO lnSelPrice,lnAllQty,lnRotSub,lnGrosMrgn
  ENDIF

  SELECT (lcOrdFile)
  
  *C102673,1 AMH Add option for agg. date [Start]
  *SCAN REST WHILE CSELECT+CSTYBASE+CPURCODE+DTOS(DCOMPLETE)+CVENDOR+CDIVISION = ;
      "�"+lcStyBase+lcPurCode+lcComplete+lcVendor+lcDivision
  SCAN REST WHILE CSELECT+CSTYBASE+CPURCODE+DTOS(DAGGDATE)+CVENDOR+CDIVISION = ;
      "�"+lcStyBase+lcPurCode+lcComplete+lcVendor+lcDivision
  *C102673,1 AMH [End]
  
    lcStyle  = STYLE
    lcDyelot = Dyelot
    lcFabric = Fabric
    lcFabClr = cFabClr
    lcFabWare= cFabWare
    lnYeild  = nYeild
    STORE 0 TO lnCutQty1,lnCutQty2,lnCutQty3,lnCutQty4,lnCutQty5,lnCutQty6,lnCutQty7,lnCutQty8
    STORE 0 TO lnOrdQty1,lnOrdQty2,lnOrdQty3,lnOrdQty4,lnOrdQty5,lnOrdQty6,lnOrdQty7,lnOrdQty8

    SELECT(lcCutPick)
    APPEND BLANK
    REPLACE TRANCD    WITH '2',;
            ORDER     WITH &lcOrdFile..ORDER,;
            CORDLINE  WITH STR(&lcOrdFile..LINENO,6),;
            STYLE     WITH lcSTYLE ,;
            Dyelot    WITH lcDyelot,;
            cWareCode WITH lcWareCode ,;
            CDIVISION WITH lcDivision ,;
            cPurCode  WITH lcPurCode,;
            cStyBase  WITH lcStyBase,;
            cVendor   WITH lcVendor,;
            dComplete WITH EVALUATE(lcOrdFile+'.DCOMPLETE'),;
            CORDERLN  WITH STR(EVALUATE(lcOrdFile+'.LINENO'),6)

    *C102673,1 AMH Add option for agg. date [Start]
    REPLACE DAGGDATE  WITH EVALUATE(lcOrdFile+'.DAGGDATE')
    *C102673,1 AMH [End]
  
    REPLACE QTY1      WITH &lcOrdFile..QTY1-&lcOrdFile..CUT1,;
            QTY2      WITH &lcOrdFile..QTY2-&lcOrdFile..CUT2,;
            QTY3      WITH &lcOrdFile..QTY3-&lcOrdFile..CUT3,;
            QTY4      WITH &lcOrdFile..QTY4-&lcOrdFile..CUT4,;
            QTY5      WITH &lcOrdFile..QTY5-&lcOrdFile..CUT5,;
            QTY6      WITH &lcOrdFile..QTY6-&lcOrdFile..CUT6,;
            QTY7      WITH &lcOrdFile..QTY7-&lcOrdFile..CUT7,;
            QTY8      WITH &lcOrdFile..QTY8-&lcOrdFile..CUT8,;
            TOTQTY    WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8

    lnCutQty1 = lnCutQty1 + &lcOrdFile..QTY1-&lcOrdFile..CUT1
    lnCutQty2 = lnCutQty2 + &lcOrdFile..QTY2-&lcOrdFile..CUT2
    lnCutQty3 = lnCutQty3 + &lcOrdFile..QTY3-&lcOrdFile..CUT3
    lnCutQty4 = lnCutQty4 + &lcOrdFile..QTY4-&lcOrdFile..CUT4
    lnCutQty5 = lnCutQty5 + &lcOrdFile..QTY5-&lcOrdFile..CUT5
    lnCutQty6 = lnCutQty6 + &lcOrdFile..QTY6-&lcOrdFile..CUT6
    lnCutQty7 = lnCutQty7 + &lcOrdFile..QTY7-&lcOrdFile..CUT7
    lnCutQty8 = lnCutQty8 + &lcOrdFile..QTY8-&lcOrdFile..CUT8
    lnOrdQty1 = lnOrdQty1 + &lcCutPick..Qty1
    lnOrdQty2 = lnOrdQty2 + &lcCutPick..Qty2
    lnOrdQty3 = lnOrdQty3 + &lcCutPick..Qty3
    lnOrdQty4 = lnOrdQty4 + &lcCutPick..Qty4
    lnOrdQty5 = lnOrdQty5 + &lcCutPick..Qty5
    lnOrdQty6 = lnOrdQty6 + &lcCutPick..Qty6
    lnOrdQty7 = lnOrdQty7 + &lcCutPick..Qty7
    lnOrdQty8 = lnOrdQty8 + &lcCutPick..Qty8

    IF llDispPric
      STORE 0 TO lnTotQty
      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        lnTotQty = lnTotQty + &lcOrdFile..QTY&lcI-&lcOrdFile..CUT&lcI
      ENDFOR
      lnAllQty = lnAllQty + lnTotQty
      lnselPrice = lnSelPrice + ( lnTotQty*&lcOrdFile..Price )
      lnSelPrice = lnSelPrice / lnAllQty
    ENDIF

    IF lnCutQty1+lnCutQty2+lnCutQty3+lnCutQty4+lnCutQty5+lnCutQty6+lnCutQty7+lnCutQty8 > 0
      SELECT (lcPOLine)
      SELECT (lcPOH)
      REPLACE LASTLINE WITH LASTLINE + 1
      =SEEK(lcStyle,'Style')
      SELECT (lcPOLine)
      APPEND BLANK
      REPLACE STYLE     WITH lcStyle    ,;
              DYELOT    WITH lcDyelot   ,;
              TRANCD    WITH '1'        ,;
              CDIVISION WITH lcDivision ,;
              cWareCode WITH lcWareCode ,;
              LINENO    WITH &lcPOH..LastLine,;
              Vendor    WITH lcVendor   ,;
              cPurCode  WITH lcPurCode  ,;
              cStyGrade WITH STYLE.CSTYGRADE,;
              Fabric    WITH lcFabric   ,;
              cFabClr   WITH lcFabClr   ,;
              cFabWare  WITH lcFabWare  ,;
              nYeild    WITH lnYeild,;
              cStyBase  WITH lcStyBase,;
              dComplete WITH EVALUATE(lcOrdFile+'.DCOMPLETE'),;
              CVENDOR   WITH lcVendor,;
              CORDERLN  WITH STR(EVALUATE(lcOrdFile+'.LINENO'),6)
    
      *C102673,1 AMH Add option for agg. date [Start]
      REPLACE DAGGDATE  WITH EVALUATE(lcOrdFile+'.DAGGDATE')
      *C102673,1 AMH [End]
    
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
      FOR lnCount = 1 TO 5
        lcCount = STR(lnCount,1)
        DO CASE
          CASE lcIType&lcCount = 'P'
            lnCost&lcCount = IIF(lcPCurr=Style.cPriceCur,TOTQTY*STYLE.nICost&lcCount,0)
            lnECost&lcCount= lnCost&lcCount &lcPExSign lnPRate &lcPUntSin lnUnit1
          CASE INLIST(lcIType&lcCount,'M','D')
            lnCost&lcCount = IIF(lcDCurr=Style.cDutyCur,TOTQTY*STYLE.nICost&lcCount,0)
            lnECost&lcCount= lnCost&lcCount &lcDExSign lnDRate &lcDUntSin lnUnit2
          OTHERWISE
            lnCost&lcCount = TOTQTY*STYLE.nICost&lcCount
            lnECost&lcCount= lnCost&lcCount
        ENDCASE
        lnTCost&lcCount  = lnTCost&lcCount  + lnECost&lcCount
        lnTFCost&lcCount = lnTFCost&lcCount + lnCost&lcCount
      ENDFOR
      REPLACE nCost1  WITH nCost1 + lnCost1  ,;
              nCost2  WITH nCost2 + lnCost2  ,;
              nCost3  WITH nCost3 + lnCost3  ,;
              nCost4  WITH nCost4 + lnCost4  ,;
              nCost5  WITH nCost5 + lnCost5  ,;
              nECost1 WITH nECost1+ lnECost1 ,;
              nECost2 WITH nECost2+ lnECost2 ,;
              nECost3 WITH nECost3+ lnECost3 ,;
              nECost4 WITH nECost4+ lnECost4 ,;
              nECost5 WITH nECost5+ lnECost5
      IF llDispPric
        lnTotCost   = (nEcost1+nEcost2+nEcost3+nEcost4+nEcost5)/TOTQTY
        lnRotSub    = IIF(llStyMark,lnTotCost,lnSelPrice)
        lnGrosMrgn  = IIF(lnRotSub=0,0,((lnSelPrice - lnTotCost)/lnRotSub)*100)
        REPLACE nSelPrice WITH lnSelPrice ,;
          nGrosMrgn WITH lnGrosMrgn
        STORE 0 TO lnSelPrice,lnAllQty
      ENDIF
      lnOrder   = lnOrder   + TOTORD
      lnAllocat = lnAllocat + TOTQTY
    ENDIF
  ENDSCAN
  SELECT (lcPOH)
  REPLACE nStyOrder WITH lnAllocat ,;
          TotOrd    WITH lnOrder   ,;
          NFCOST1   WITH lnTFCost1 ,;
          NFCOST2   WITH lnTFCost2 ,;
          NFCOST3   WITH lnTFCost3 ,;
          NFCOST4   WITH lnTFCost4 ,;
          NFCOST5   WITH lnTFCost5 ,;
          NICOST1   WITH lnTCost1  ,;
          NICOST2   WITH lnTCost2  ,;
          NICOST3   WITH lnTCost3  ,;
          NICOST4   WITH lnTCost4  ,;
          NICOST5   WITH lnTCost5
  IF nStyOrder = 0
    DELETE
  ENDIF
  SELECT (lcOrdFile)
ENDDO

SET ORDER TO TAG (lcStyOrd) IN STYLE
SET ORDER TO TAG (lcOrdTag) IN (lcOrdFile)
SELECT (lnAlias)
WAIT CLEAR

*!*************************************************************
*! Name      : lfReadAct
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : READ Activate function
*!*************************************************************
*! Calls     : lfClearKey.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfReadAct()
*!*************************************************************
FUNCTION lfReadAct
=gfStopBrow()

=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

*!*************************************************************
*! Name      : lfClearKey
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Clear Hot Keys
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfClearKey()
*!*************************************************************
FUNCTION lfClearKey

ON KEY LABEL ALT+B
ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+HOME
ON KEY LABEL CTRL+END
ON KEY LABEL TAB
ON KEY LABEL BACKTAB

*!*************************************************************
*! Name      : lpMovTab
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpMovTab WITH 'lcWinCh21', 'm.Store',.T.
*!*************************************************************
PROCEDURE lpMovTab
PARAMETERS lcWindName, lcObjName,llToCheck


ON KEY LABEL TAB
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*!*************************************************************
*! Name      : lpBackTab
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  DO lpBackTab WITH 'lcWinCh21', 'm.Store',.T.
*!*************************************************************
PROCEDURE lpBackTab
PARAMETERS lcWindName, lcObjName,llToCheck


ON KEY LABEL BACKTAB
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*!*************************************************************
*! Name      : lfGetPcs
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
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
RETURN(INT(laPercet[1]/100*(Qty1-Cut1))+INT(laPercet[2]/100*(Qty2-Cut2))+;
  INT(laPercet[3]/100*(Qty3-Cut3))+INT(laPercet[4]/100*(Qty4-Cut4))+;
  INT(laPercet[5]/100*(Qty5-Cut5))+INT(laPercet[6]/100*(Qty6-Cut6))+;
  INT(laPercet[7]/100*(Qty7-Cut7))+INT(laPercet[8]/100*(Qty8-Cut8)))

*!*************************************************************
*! Name      : lfUpdPO
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
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
PRIVATE lcDivision,lcPurCode,lcPO,lcPONumber , llUpdBom , llBomByMe,;
        llGnCTBom
llBomByMe = gfOpenFile(gcDataDir+'Bom',gcDataDir+'Bom','SH')

llGnCTBom = (gfGetMemVar("M_CRTCSTSH")  = "T")
*-- if the system support dyelots
IF llDyelot
  SELECT (lcPOLine)
  SCAN FOR EMPTY(DYELOT)
    *-- if the style is dyelot and the dyelot fields is empty
    *-- stop saving and give the user a message
    IF SEEK(STYLE,'STYLE') AND (STYLE.CDYE_FLG = 'Y')
      *-- You have to enter dyelot for the required dyelots records
      =gfModalGen('INM38135B38018','DIALOG','P/Os')
      llCSave = .F.
      RETURN
    ENDIF
  ENDSCAN
ENDIF
SET ORDER TO TAG POSLN IN POSLN
lcFirstCT  = SPACE(06)
SELECT (lcPOH)
*-- Scan the tmp.POHdr for qty > 0 to generate auto. PO numbers
SCAN FOR nStyOrder > 0
  lcPO       = PO
  llUpdBom = .T.
  lcDivision = CDIVISION
  lcPurCode  = CPURCODE
  lcStyBase  = CSTYBASE
  
  *C102673,1 AMH Add option for agg. date [Start]
  *lcComplete = DTOS(DCOMPLETE)
  lcComplete = DTOS(DAGGDATE)
  *C102673,1 AMH [End]
  
  lcVendor   = CVENDOR
  lcPONumber = IIF(EMPTY(PO),IIF(EMPTY(cTmpPo),gfSequence('PO','','',CDIVISION),cTmpPo),PO)
  =RLOCK()
  REPLACE cTmpPo WITH lcPONumber
  UNLOCK
  lcFirstCT = IIF(EMPTY(lcFirstCT),lcPONumber,lcFirstCT)
  lcLastCt  = lcPONumber
  SELECT (lcPOLine)
  =SEEK(lcPO+lcStyBase+lcPurCode+lcComplete+lcVendor+lcDivision)
  
  *C102673,1 AMH Add option for agg. date [Start]
  *SCAN REST WHILE PO+CSTYBASE+CPURCODE+DTOS(DCOMPLETE)+CVENDOR+CDIVISION+STYLE+DYELOT+CORDERLN=;
      lcPO+lcStyBase+lcPurCode+lcComplete+lcVendor+lcDivision FOR  TOTQTY > 0
  SCAN REST WHILE PO+CSTYBASE+CPURCODE+DTOS(DAGGDATE)+CVENDOR+CDIVISION+STYLE+DYELOT+CORDERLN=;
      lcPO+lcStyBase+lcPurCode+lcComplete+lcVendor+lcDivision FOR  TOTQTY > 0
  *C102673,1 AMH [End]
  
    llUpdBom  =  IIF(llUpdBom,SEEK(SUBSTR(Style,1,lnMajorLen),'BOM'),llUpdBom)
    IF !SEEK('P'+lcPONumber+STYLE+STR(LINENO,6)+TRANCD,'POSLN')
      SCATTER MEMVAR
      m.PO = lcPONumber
      =SEEK(m.STYLE,'STYLE')
      m.Scale    = STYLE.SCALE
      m.nCost1   = m.nCost1 / m.TotQty
      m.nCost2   = m.nCost2 / m.TotQty
      m.nCost3   = m.nCost3 / m.TotQty
      m.nCost4   = m.nCost4 / m.TotQty
      m.nCost5   = m.nCost5 / m.TotQty
      m.nECost1  = m.nECost1 / m.TotQty
      m.nECost2  = m.nECost2 / m.TotQty
      m.nECost3  = m.nECost3 / m.TotQty
      m.nECost4  = m.nECost4 / m.TotQty
      m.nECost5  = m.nECost5 / m.TotQty
      m.cStyType = 'P'
      IF !SEEK(m.STYLE+m.CWARECODE+SPACE(10),'STYDYE')
        WAIT "Assigning Style"+ALLTRIM(m.STYLE)+" to warehouse "+ALLTRIM(m.cWareCode) WINDOW NOWAIT
        DO gpAdStyWar WITH m.STYLE,SPACE(10),m.cWareCode
        =SEEK(m.STYLE+m.CWARECODE+SPACE(10),'STYDYE')
        WAIT CLEAR
      ENDIF
      IF &lcPOLine..nSteps < 1 AND STYLE.lInvSty
        SELECT STYDYE
        =RLOCK()
        REPLACE WIP1   WITH WIP1   + m.Qty1  ,;
          WIP2   WITH WIP2   + m.Qty2  ,;
          WIP3   WITH WIP3   + m.Qty3  ,;
          WIP4   WITH WIP4   + m.Qty4  ,;
          WIP5   WITH WIP5   + m.Qty5  ,;
          WIP6   WITH WIP6   + m.Qty6  ,;
          WIP7   WITH WIP7   + m.Qty7  ,;
          WIP8   WITH WIP8   + m.Qty8  ,;
          TotWIP WITH TotWIP + m.TotQty,;
          nWO1   WITH nWO1   + m.Qty1  ,;
          nWO2   WITH nWO2   + m.Qty2  ,;
          nWO3   WITH nWO3   + m.Qty3  ,;
          nWO4   WITH nWO4   + m.Qty4  ,;
          nWO5   WITH nWO5   + m.Qty5  ,;
          nWO6   WITH nWO6   + m.Qty6  ,;
          nWO7   WITH nWO7   + m.Qty7  ,;
          nWO8   WITH nWO8   + m.Qty8  ,;
          nTotWo WITH nTotWo + m.TotQty
        UNLOCK
        =gfTraceKey('STYDYE',STYLE+CWARECODE+DYELOT,'M')
        SELECT (lcPOLine)
        =RLOCK()
        REPLACE nSteps WITH 1
        UNLOCK
      ENDIF
      IF &lcPOLine..nSteps < 2 AND STYLE.lInvSty
        SELECT STYLE
        =RLOCK()
        REPLACE WIP1   WITH WIP1   + m.Qty1  ,;
          WIP2   WITH WIP2   + m.Qty2  ,;
          WIP3   WITH WIP3   + m.Qty3  ,;
          WIP4   WITH WIP4   + m.Qty4  ,;
          WIP5   WITH WIP5   + m.Qty5  ,;
          WIP6   WITH WIP6   + m.Qty6  ,;
          WIP7   WITH WIP7   + m.Qty7  ,;
          WIP8   WITH WIP8   + m.Qty8  ,;
          TotWIP WITH TotWIP + m.TotQty,;
          nWO1   WITH nWO1   + m.Qty1  ,;
          nWO2   WITH nWO2   + m.Qty2  ,;
          nWO3   WITH nWO3   + m.Qty3  ,;
          nWO4   WITH nWO4   + m.Qty4  ,;
          nWO5   WITH nWO5   + m.Qty5  ,;
          nWO6   WITH nWO6   + m.Qty6  ,;
          nWO7   WITH nWO7   + m.Qty7  ,;
          nWO8   WITH nWO8   + m.Qty8  ,;
          nTotWo WITH nTotWo + m.TotQty
        UNLOCK
        =gfTraceKey('STYLE',STYLE,'M')
        SELECT (lcPOLine)
        =RLOCK()
        REPLACE nSteps WITH 2
        UNLOCK
      ENDIF
      m.cAdd_User = gcUser_id
      m.cAdd_Time = TIME()
      m.dAdd_Date = gdSysDate
      SELECT (lcPOLine)
      =RLOCK()
      REPLACE nSteps WITH 3
      UNLOCK
      INSERT INTO POSLN FROM MEMVAR
    ELSE
      IF &lcPOLine..nSteps < 1
        SELECT POSLN
        =RLOCK()
        REPLACE Ord1 WITH Ord1 + &lcPOLine..Ord1 ,;
          Ord2 WITH Ord2 + &lcPOLine..Ord2 ,;
          Ord3 WITH Ord3 + &lcPOLine..Ord3 ,;
          Ord4 WITH Ord4 + &lcPOLine..Ord4 ,;
          Ord5 WITH Ord5 + &lcPOLine..Ord5 ,;
          Ord6 WITH Ord6 + &lcPOLine..Ord6 ,;
          Ord7 WITH Ord7 + &lcPOLine..Ord7 ,;
          Ord8 WITH Ord8 + &lcPOLine..Ord8 ,;
          TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
        UNLOCK
        =gfTraceKey('POSLN','P'+lcPONumber+cRsession+Shipno+STYLE+STR(LINENO,6)+Trancd,'M')
        SELECT (lcPOLine)
        =RLOCK()
        REPLACE nSteps WITH 1
        UNLOCK
      ENDIF
    ENDIF
    SELECT (lcCutPick)
    =SEEK(lcPO+lcStyBase+lcPurCode+lcComplete+lcVendor+lcDivision+POSLn.Style+POSLn.cWareCode+POSLn.Dyelot+EVALUATE(lcPoLine+'.CORDERLN'))
    SCATTER MEMVAR
    SELECT ORDHDR
    =SEEK('O'+m.ORDER)
    IF &lcCutPick..nSteps < 1
      =RLOCK()
      REPLACE TotCut WITH TotCut + m.TotQty
      UNLOCK
      =gfTraceKey('ORDHDR','O'+ORDER,'M')
      SELECT (lcCutPick)
      =RLOCK()
      REPLACE nSteps WITH 1
      UNLOCK
    ENDIF
    SELECT ORDLINE
    =SEEK('O'+m.Order+m.cOrdLine)
    IF &lcCutPick..nSteps < 2
      =RLOCK()
      REPLACE Dyelot WITH m.Dyelot     ,;
        Cut1   WITH Cut1 + m.Qty1,;
        Cut2   WITH Cut2 + m.Qty2,;
        Cut3   WITH Cut3 + m.Qty3,;
        Cut4   WITH Cut4 + m.Qty4,;
        Cut5   WITH Cut5 + m.Qty5,;
        Cut6   WITH Cut6 + m.Qty6,;
        Cut7   WITH Cut7 + m.Qty7,;
        Cut8   WITH Cut8 + m.Qty8,;
        TotCut WITH TotCut + m.TotQty
      UNLOCK
      =gfTraceKey('ORDLINE','O'+ORDER+STR(LINENO,6),'M')
      SELECT (lcCutPick)
      =RLOCK()
      REPLACE nSteps WITH 2
      UNLOCK
    ENDIF
    m.CTKTNO     = lcPONumber
    m.cTktLineNo = STR(POSLn.LineNo,6)
    IF &lcCutPick..nSteps < 3
      INSERT INTO CUTPICK FROM MEMVAR
      =gfTraceKey('CUTPICK',m.trancd+m.ctktno+m.order+m.style+m.cordline,'A')
      SELECT (lcCutPick)
      =RLOCK()
      REPLACE nSteps WITH 3
      UNLOCK
    ENDIF
    REPLACE CTKTNO WITH lcPONumber
  ENDSCAN
  IF !SEEK('P'+lcPONumber,'POSHDR')
    SELECT (lcPOH)
    SCATTER MEMVAR
    m.PO = lcPONumber
    m.cMultiLot= 'S'
    =SEEK(m.cWarecode,'WAREHOUS')
    m.cOutAddr1 = WareHous.cAddress1
    m.cOutAddr2 = WareHous.cAddress2
    m.cOutAddr3 = WareHous.cAddress3
    m.cOutAddr4 = WareHous.cAddress4
    m.cOutAddr5 = WareHous.cAddress5
    m.ShpName   = WareHous.cDesc
    m.Available = COMPLETE
    m.Open      = nStyOrder
    m.cStyType  = "P"
    m.Contact   = lcCont
    m.Phone     = lcPhone
    m.POTOTAL   = NICOST1+NICOST2+NICOST3+NICOST4+NICOST5
    m.cAdd_User = gcUser_id
    m.cAdd_Time = TIME()
    m.dAdd_Date = gdSysDate
    =RLOCK()
    REPLACE nSteps WITH 1
    UNLOCK
    INSERT INTO POSHDR FROM MEMVAR
    =gfTraceKey('POSHDR','P'+lcPONumber,'A')
    =gfTraceKey('POSLN','P'+lcPONumber+cRsession+Shipno+STYLE+STR(LINENO,6)+Trancd,'A')
  ELSE
    IF &lcPOH..nSteps < 1
      SELECT POSHDR
      =RLOCK()
      REPLACE FLAG   WITH '' ,;
        TotOrd WITH TotOrd + &lcPOH..TotOrd
      UNLOCK
      SELECT (lcPOH)
      =RLOCK()
      REPLACE nSteps WITH 1
      UNLOCK
      =gfTraceKey('POSHDR','P'+lcPONumber,'M')
    ENDIF
  ENDIF
  *--- Get the module seting for generating cost sheet
  *--- if this seting is alwayes ("A") we will call function to call
  *--- C/T,PO's Cost Sheet program to generate Cost Sheet.
  llDumy = llUpdBom .AND. laScrMode[4] .AND. llGnCTBom .AND. lfGenCutBom('',POSHDR.PO)
ENDSCAN
IF llBomByMe
  USE IN BOM
ENDIF
SELECT (lcPOH)
REPLACE ALL PO WITH cTmpPo
GO TOP

*!*************************************************************
*! Name      : lfChngIndx
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Change and restore the index of the style file
*!             for browse in range
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
PARAMETER lcNdxFlag

PRIVATE lcOldAlis
*-- When entering the in range browse
IF lcNdxFlag = 'S'
  lcOldAlis = ALIAS()            && Save the old alias
  SELECT STYLE
  lcOldNdx = ORDER()             && Save the old order
  SET ORDER TO TAG Cstyle
  SELECT (lcOldAlis)             && Restore the old alias
ELSE && IF getting out from the in range browse
  lcOldAlis = ALIAS()            && Save the old alias
  SELECT STYLE
  SET ORDER TO TAG lcOldNdx      && Restore the old order
  SELECT (lcOldAlis)             && Restore the old alias
ENDIF && End of IF lcNdxFlag = 'S'

*!*************************************************************
*! Name      : lfvRepForm
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : To disable the selection in order type in the
*!             option grid
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

CLEAR READ

*!*************************************************************
*! Name      : lfvBroWCt
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : CutTkt Valid Fuunction
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

PRIVATE lnOldAls , lcNewCutNo , llRet , lcToSeekIn , lcStrToAdd , lcHdrVal

lnOldAls = SELECT(0)

IF !llGManualy
  REPLACE PO WITH ''
  RETURN
ENDIF
lcHdrVal   = PO+CDIVISION+CPURCODE
lcNewCutNo = IIF(!EMPTY(PO),'P'+PO,PO)
lcToSeekIn = 'POSHDR'
lcStrToAdd = "P/O"
llRet = .F.
IF EMPTY(lcNewCutNo)
  lnNoThing = gfModalGen('TRM38047B00000','DIALOG',lcStrToAdd)
  REPLACE PO WITH lcOldCtkNo
ELSE
  IF LEN(ALLTRIM(lcNewCutNo))-1 < 6
    lnNoThing = gfModalGen('TRM38048B00000','DIALOG',lcStrToAdd)
    REPLACE PO WITH lcOldCtkNo
  ELSE
    IF SEEK(lcNewCutNo,lcToSeekIn) .OR. SEEK(SUBSTR(lcNewCutNo,2),lcLinFile)
      lnNoThing = gfModalGen('TRM38049B00000','DIALOG',lcStrToAdd)
      lcNewCutNo  = ''
      SELECT (lcHdrFile)
      REPLACE PO WITH lcOldCtkNo
    ELSE
      llRet = .T.
    ENDIF
  ENDIF
ENDIF
IF llRet
  SELECT (lcLinFile)
  IF !SEEK(lcHdrVal)
    =SEEK(lcOldCtkNo+SUBSTR(lcHdrVal,7))
    REPLACE PO WITH SUBSTR(lcNewCutNo,2) FOR +;
                   PO+CDIVISION+CPURCODE+STYLE+DYELOT = +;
                   lcOldCtkNo+SUBSTR(lcHdrVal,7)
  ENDIF
  *--- cTKtNo
  SELECT (lcCutPick)
  lcPoCtNo  = SUBSTR(lcNewCutNo,2)
  lcPoCtExp = 'CTKTNO+CDIVISION+CPURCODE+STYLE+CWARECODE+DYELOT'
  IF !SEEK(lcPoCtNo)
  
    =SEEK(lcOldCtkNo+SUBSTR(lcHdrVal,7))
    REPLACE cTKtNo  WITH lcPoCtNo FOR &lcPoCtExp = +;
                                      lcOldCtkNo+SUBSTR(lcHdrVal,7)
  ENDIF
ENDIF
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfwOldCtkt
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Get old no.
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

lcOldCtkNo = PO

*!*************************************************************
*! Name      : lfCheckMaj
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Check style code structure.
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

PRIVATE lnAlias
lnAlias=SELECT(0)
llStruOp=gfOpenFile(gcDataDir+'ICISTRU','Segno','SH')
IF !SEEK('U1','ICISTRU')
  IF USED('ICISTRU') AND llStruOp
    USE IN ICISTRU
  ENDIF
  SELECT (lnAlias)
  RETURN .F.
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfGenCutBom   E301426
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Generate Cost Sheet Automaticaly.
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
PARAMETER lcCtktSty  , lcGenCTkt

PRIVATE lnOldAls , llByMe

lnOldAls = SELECT(0)
llByMe = gfOpenFile(gcDataDir+'Bom',gcDataDir+'Bom','SH')
llFromBom = .T.
IF SEEK(lcCtktSty, "Bom")
  lcParameter = "'" + lcGenCTkt + "'"+",.T."
  llFunCal = .T.
  lcOldBaseW = gcBaseWind
  gcBaseWind = "AARPOCSSH"
  lcBaseFile = 'POSHDR'
  glFirsTime = .T.
  DO gcAppHome+"MFCSSH.FXP" WITH 'I',lcGenCTkt,.T.
  gcBaseWind = lcOldBaseW
  lcBaseWind = lcOldBaseW
  lcBaseFile = ''
  glFirsTime = .F.
  laScrMode  = .F.
  laScrMode[4] = .T.
ELSE
  *-- XXXXXXXXXX cost sheet not found for XXXXXXXXXX : 'XXXXXXXXXX'. 
  *-- Cannot proceed with the cutting ticket cost sheet.
  *-- < Ok >
  lcStr      = PROPER(ALLTRIM(lcMjrTtl)) + "|" +;
               PROPER(ALLTRIM(lcMjrTtl)) + "|" +;
               ALLTRIM(lcGenCTkt)
  = gfModalGen("TRM38095B00000","DIALOG",lcStr)
ENDIF
IF llByMe
  USE IN Bom
ENDIF
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfRetToSel
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/04/2002
*! Purpose   : Allow user to select anoter order or group 
*!           : group of orders to generate PO without go to OG
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
PRIVATE lcBrowFile 

lcBrowFile = lcOrdLine
*-- Delete the previous selected order or orders
SELECT (lcBrowFile)
SET ORDER TO TAG TICKET
lcForExpr = 'CSELECT+CDIVISION+CPURCODE+STYLE+DYELOT'
=SEEK("�")
SCAN REST FOR &lcForExpr = "�"
  BLANK
  DELETE
ENDSCAN
SET ORDER TO TAG (lcOrdLine)
GOTO TOP
*-- IF the temp order line not empty allow user to select anoter 
*-- order or group of orders without go to OG
IF !EOF()
  llRetToSel = .T.
ENDIF
*-- Delete the saved purchase order or orders 
=lfBrowOrd()
SELECT (lcHdrFile)
DELETE ALL

*-- To delete the records that found in the PO Line (Temporary file)
IF USED(lcPoLine)
  SELECT(lcPoLine)
  DELETE ALL
ENDIF  

=lfBrowCut()
lcFirstCT = ""
lcLastCt  = ""

*-- End of lfRetToSel

*!*************************************************************
*! Name      : lfvBoSty
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/05/2002
*! Purpose   : valid function of use base style number option.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : = lfvBoSty()
*!*************************************************************
FUNCTION lfvBoSty

lnRpStyPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lcRpStyle'),1)
laOGObjCnt[lnRpStyPo] = llRpBoSty
= lfOGShowGet('lcRpStyle')
*--end of lfvBoSty

*!*************************************************************
*! Name             : lfGetOrd
*! Developer        : AHMED MAHER (AMH)
*! Date             : 03/10/2002
*! Purpose          : Get the selected Orders in the selected criteria
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
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'ORDHDR.ORDER'),1)
IF lnPosition > 0
  lcOrderFil = laOGFxFlt[lnPosition,6]
  llOrdUse   = IIF(!EMPTY(lcOrderFil) .AND. USED(lcOrderFil) .AND. RECCOUNT(lcOrderFil)>0,.T.,.F.)
ENDIF