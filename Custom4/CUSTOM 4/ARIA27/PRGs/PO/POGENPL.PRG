*:****************************************************************
*: Program file  : POGENPL.PRG
*: Program desc. : Generate Style Pos based on Open To Sell. 
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Style Purchase Order -  [PO]
*: Developer     : ABDOU ELGENDI -  [ABD]
*: Date          : 09/17/2003
*: Tracking Job Number : E500455,1
*:****************************************************************
*: Calls         : G.FUNCTIONS: gfSetup 
*:               : FUNCTIONS  : lfvScope    , lfwOption  , lfvStyle
*:               :            : lfvVendor   , lfCheckMaj , lfClearKey
*:               :            : lfStyBrow   , lfStyWhen  , lfAdUnCmSR
*:               :            : lfBrowCut   , lfvBroWCt  , lfwOldCtkt
*:               :            : lfvWareH    , lfwOldVals , lfCutWhen
*:               :            : lfChkUnCmS  , lfUpdVars  , lfUpdUnCmS
*:               :            : lfCrtTemp   , lfCrtDTemp , lfSelStyle
*:               :            : lfEditHdr   , lfDelFiles , lfvSelect
*:               :            : lfReadAct   , lfClearKey , lfDeactMain
*:               :            : lfShowDef   , lfvDefVen  , lfvDefPCurr
*:               :            : lfvDefDCurr , lfvOK      , lfvGenerate
*:               :            : lfvDefWare  , lfvGenCtPO , lfUpdPOPik
*:               :            : lfvDetail   , lfDeactDet , lfBrowLines
*:               :            : lfwPick     , lfPickWhen , lfwBrDylot
*:               :            : lfvDyelot   , lfwBrOldVal, lfvCTQty
*:               :            : lfPODef     , lfUpdPO    , lfGenCutBom
*:               :            : lfvNote     , lfUpdGpric.
*:               -------------------------------------------------
*:               : PROCEDURE  : lpShow      , lpClsScr  , lpMovTab
*:               :            : lpBackTab   , lpSavScr
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:E500455,1
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:B127460,1 EIH 05/17/2005 Fix bug that incorrect Gros_Price in POSLN with 
*:****************************************************************

*-- lcWindTitle :- Window Titel.
*-- Define needed Variables.
EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc
DIMENSION laColors[1,2],laPercet[8],laStySeg[1],laWareType[2],laDefProc[10]

STORE '' TO laData,laKeyField,lcWinC11,lcWinC12,lcWinC13,lcWinC14,lcWinC15,lnOldVal,lcModal,;
            lcSZ1,lcSZ2,lcSZ3,lcSZ4,lcSZ5,lcSZ6,lcSZ7,lcSZ8,lcScalDesc,lcFiles,;
            lcBrowseTl , lcStyTmp, lcVenName,lcCont,lcPhone,lcHdrFile,lcLinFile,lcPOH,;
            lcPOLine, lcOldCtkNo 

STORE .F. TO llBrowse
*-- Browse title of the mainbrowse of Style details.
lcSrcTtl   = "Styles"

*-- To check if the first time to run the option grid
STORE .T. TO llFirst

lcTktTtl  = "Purchase Orders"
lcSrcTtl  = "Styles"

lcBrTtl1  = 'Details of Purchase Order'
lcBrTtl2  = lcBrTtl1

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

*-- Hold the lenght of the color and its start position in case
*-- there is a color segment in style stucture
STORE  0  TO lnColorStr,lnColorLen,;
              lnOrdRecNo,lnCutRecNo,lnStyRecNo && &&To hold the record number when browsing

*-- Hold the unit of the foreign curr.
STORE  1  TO lnUnit1,lnUnit2,lnPRate,lnDRate
STORE '*' TO lcSign1,lcSign2

STORE .F. TO llRPUnAWip,llRpBOPFab,llAllocate

*-- Hold the number of term,ship,term
STORE  1  TO lnTerm,lnShip,lnType,puType

STORE .F. TO llBrowse,llColorExt,llChecked

STORE '' TO  laColors,lcVendor,lcWareCode,lcPCurr,lcDCurr,cMajorPic,lnMajorLen,;
  lcFirstCT,lcLastCt,lcIType1,lcIType2,lcIType3,lcIType4,;
  lcIType5,lcSelFile

DIMENSION laSeason[1], laTerm[1], laDiv[1], laCodInfo [2,10] ,laShip[1], laGroup[1], laSetups[21,2] 
STORE "" TO laSeason,laTerm, laDiv, laCodInfo,laShip,laGroup,laSetups
DECLARE laFromBrow[2]
laFromBrow[1] = lcTktTtl
laFromBrow[2] = lcSrcTtl

*------- llDispPric  -> hold setting variable to display selling price & gross margin
*------- lnTotQty    -> Hold TOTAl Qty Of one Order
*------- lnSelPrice  -> Hold Average Selling Price
*------- lnAllQty    -> Hold All Qty For All Orders
*------- lnRotSub    -> Hold Cost Price Or Selling price Depend on llStyMark
*------- lnGrosMrgn  -> Hold Gross Margin

llDispPric  = .F.
llStyMark   = .F.

llDispPric = gfGetMemVar('M_PoDspPrc')
llStyMark  = gfGetMemVar('M_stymark')  ='T'

IF llDispPric
  STORE 0 TO lnTotQty,lnSelPrice,lnAllQty,lnRotSub,lnGrosMrgn
ENDIF


*--Get the values from the memory variable
laSetups[01,1]  = 'M_WareHouse'
laSetups[02,1]  = 'M_DYELOT'
laSetups[03,1]  = 'LLEDITEXRA'
laSetups[04,1]  = 'llMulCurr'
laSetups[05,1]  = 'M_CMSLbl1'
laSetups[06,1]  = 'M_CMSLbl2'
laSetups[07,1]  = 'M_CMSLbl3'
laSetups[08,1]  = 'M_CMSLbl4'
laSetups[09,1]  = 'M_CMSLbl5'
laSetups[10,1]  = 'M_CISLbl1'
laSetups[11,1]  = 'M_CISLbl2'
laSetups[12,1]  = 'M_CISLbl3'
laSetups[13,1]  = 'M_CISLbl4'
laSetups[14,1]  = 'M_CISLbl5'
laSetups[15,1]  = 'M_cIType1'
laSetups[16,1]  = 'M_cIType2'
laSetups[17,1]  = 'M_cIType3'
laSetups[18,1]  = 'M_cIType4'
laSetups[19,1]  = 'M_cIType5'
laSetups[20,1]  = 'M_MATDYE'
laSetups[21,1]  = 'M_GENSTORN'

=gfGetMemVar(@laSetups,gcAct_Comp)

llWareHous = (laSetups[01,2] = 'Y')
llDyelot   = (laSetups[02,2] = 'Y')
llEditExRt =  laSetups[03,2]
llMulCurr  =  laSetups[04,2]
lcMSLbl1   =  laSetups[05,2]
lcMSLbl2   =  laSetups[06,2]
lcMSLbl3   =  laSetups[07,2]
lcMSLbl4   =  laSetups[08,2]
lcMSLbl5   =  laSetups[09,2]
lcISLbl1   =  laSetups[10,2]
lcISLbl2   =  laSetups[11,2]
lcISLbl3   =  laSetups[12,2]
lcISLbl4   =  laSetups[13,2]
lcISLbl5   =  laSetups[14,2]
lcIType1   =  laSetups[15,2]
lcIType2   =  laSetups[16,2]
lcIType3   =  laSetups[17,2]
lcIType4   =  laSetups[18,2]
lcIType5   =  laSetups[19,2]
llFDyelot  =  (laSetups[20,2] = 'Y')
llGManualy =  (laSetups[21,2] = 'Y')

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
lcProgID  = PADR("POGENPL",10)
lcProgTxt = 'Generate Style Pos based on Open To Sell'

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
  
*-- End Define needed Variables.

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

IF !gfSetup()
  RETURN
ENDIF

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


  lcWinC11 = gfTempName()
  lcWinC12 = gfTempName()
  lcWinC13 = gfTempName()
  lcWinC14 = gfTempName()
  lcWinC15 = gfTempName()
  
  lcPOH      = gfTempName()  && temprory PosHdr
  lcPOLine   = gfTempName()  && temprory PosLine
  lcStyTmp   = gfTempName()   && temprory Style

  lcLinFile = lcPOLine && Variable to hold the Line file
  lcHdrFile = lcPOH    && Variable to hold the header file
  lcSession = gfsequence('CSESSION')

ENDIF

llReBrowse = .T.
*-- To allow the program to enter the lpShow procedure at the calling time
llNoShow   = .F.
lcWinTitle = 'Generate Style Pos based on Open To Sell'
*-- Title of the screen Mainvct4
lcLineT      = SPACE(00)

*-- Calling of the main screen MAINVCT
PUSH KEY
=lfClearKey()
STORE '' TO lcBrowseTl
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

*-- Call of the main screen
DO (gcScrDir+gcWinAppl+'\POGENPL.SPX')
RELEASE WINDOWS (lcSrcTtl),(lcTktTtl),(lcTktTtl),(lcSrcTtl)
POP KEY

*-- Delete all the temp. files when closing the program
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

  IF USED(lcStyTmp)
    USE IN (lcStyTmp)
    ERASE (gcWorkDir+lcStyTmp+".DBF")
    ERASE (gcWorkDir+lcStyTmp+".CDX")
   ENDIF
ENDIF

*-- End OF Code.
*:*************************************************************
*: Name      : lpShow
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Show function
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lpShow()
*:*************************************************************
*:
PROCEDURE lpShow

IF llReBrowse
  =lfStyBrow()
  =lfBrowCut()
  llReBrowse = .F.
ENDIF
SHOW GET ibToBrow ENABLE
SHOW GET ibToHdr  ENABLE
SHOW GET pbSlct   DISABLE
SHOW GET pbEdt    DISABLE
SHOW GET pbDlt    DISABLE

DO CASE
  ***--- S E L E C T   M O D E ---***
  CASE laScrMode[1]

    IF !llChecked
      IF lfChkUnCmS()
        RETURN
      ENDIF
    ELSE
      lcScrMode = "S"
    ENDIF


    STORE gcBaseCurr TO lcPCurr,lcDCurr
    STORE ''  TO lcSZ1,lcSZ2,lcSZ3,lcSZ4,lcSZ5,lcSZ6,lcSZ7,lcSZ8,lcScalDesc
    STORE 1   TO lnPRate,lnDRate
    STORE 100 TO laPercet
    STORE .F. TO llAllocate
    IF llFirst
      =lfvScope()
    ENDIF

  GO TOP IN  (lcHdrFile)
  IF EOF(lcHdrFile)
    SHOW GET pbScope ENABLE
  ELSE
    SHOW GET pbScope DISABLE
  ENDIF
  
    
  ***--- V I E W   M O D E     ---***
  CASE laScrMode[2]

  llSelMode = .T.
  SHOW GET pbSlct ENABLE
  IF !EMPTY(lcFirstCT)
    SHOW GET pbNote,1  PROMPT '\<Notes' ENABLE
    SHOW GET pbHeadr,1 PROMPT "\<P/O" ENABLE
  ENDIF

  ***--- E D I T   M O D E     ---***
  CASE laScrMode[3]

  lcScrMode = "E"
  llCUpDate = .T.
  SHOW GET pbSav DISABLE
  IF EMPTY(lcVendor)
    SHOW GET pbGenerate,1 PROMPT "\<Generate P/O" DISABLE
  ELSE
    SHOW GET pbGenerate,1 PROMPT "\<Generate P/O" ENABLE    
  ENDIF

  SHOW GET pbDetail DISABLE
  IF llAllocate
    SHOW GET pbSelect  DISABLE
    SHOW GET pbSelAll  DISABLE
    SHOW GET pbSelNone DISABLE
    SHOW GET pbInvert  DISABLE
    SHOW GET pbDefault ENABLE
    SHOW GET pbScope   DISABLE
    SHOW GET pbNote,1  PROMPT 'Clear Allo\<cation' ENABLE
    SHOW GET pbHeadr,1 PROMPT 'Allocated \<Orders' ENABLE
  ELSE
    =lfvSelect('')
    SHOW GET pbHeadr,1 PROMPT '\<P/O' DISABLE
    SHOW GET pbScope ENABLE
  ENDIF
  SHOW GET pbCls,1 PROMPT gcBmpHome+"Cancel.BMP"
  =lfUpdVars()


  ***--- A D D   M O D E       ---***
  CASE laScrMode[4]

  lcScrMode = 'A'
  llCUpDate = .T.
  SHOW GET pbDetail  ENABLE
  SHOW GET pbScope   DISABLE
  SHOW GET pbSelect  DISABLE
  SHOW GET pbSelAll  DISABLE
  SHOW GET pbSelNone DISABLE
  SHOW GET pbInvert  DISABLE
  SHOW GET pbDefault DISABLE
  SHOW GET pbSav     ENABLE
  SHOW GET pbGenerate,1 PROMPT "\<Clear P/O"
  SHOW GET pbNote,1     PROMPT '\<Notes' DISABLE
  SHOW GET pbHeadr,1    PROMPT '\<P/O' DISABLE
  SHOW GET pbCls,1 PROMPT gcBmpHome+"Cancel.BMP"
  =lfUpdVars()
  
ENDCASE

*-- End OF lpShow
*:*************************************************************
*: Name      : lfvScope
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Called for the selection criteria (Option Grid)
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfvScope ()
*:*************************************************************
*:
FUNCTION lfvScope

*-- Call differrent option grid in case of generate from plan
lcExpr    = gfOpGrid('POGENPL',.T.)

lcSelFile = lcStyTmp
SHOW GET pbNote,1  PROMPT '\<Notes' DISABLE
SHOW GET pbHeadr,1 PROMPT '\<P/O' DISABLE
=gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')
=gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
=gfOpenFile(gcDataDir+'CODES',gcDataDir+'cCode_No','SH')

*IF llDyelot
*  =gfOpenFile(gcDataDir+'DYELOT',gcDataDir+'DYELOT','SH')
*ENDIF
=gfwCodePop(@laCodInfo, "CTERMCODE", "D")
=gfwCodePop(@laCodInfo, "SHIPVIA",   "D")

SELECT (lcSelFile)
llNothing = IIF(llFirst,lfAdUnCmSR(),'')
llFirst = .F.

IF lfSelStyle()
  =lfStyWhen()
  STORE .F. TO laScrMode
  laScrMode[3] = .T.
ENDIF

DO lpShow

*-- End OF lfvScope
*:*************************************************************
*: Name      : lfwOption
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Function called from the when of the option grid
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfwOption()
*:*************************************************************
FUNCTION lfwOption


*-- End of lfwOption
*:*************************************************************
*: Name      : lfvStyle
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Function called from the validation of the style option grig
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfvStyle()
*:*************************************************************
*:
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

*-- End OF lfvStyle
*:*************************************************************
*: Name      : lfvVendor
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Function called from the validation of the vendor option grig
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfvVendor()
*:*************************************************************
FUNCTION lfvVendor
PRIVATE lcObjNam , lcObjVal , llObjRet

lcObjNam = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
SET ORDER TO TAG Vencode IN APVENDOR
*-- IF The user want to Browse or if the Vendor he entered is not in the file
IF !EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'APVENDOR')
  llBrowse = .T.
  *-- E301268,1 HDM [Start] filter vendor browse by sup. type

  llObjRet = gfApVnBrow(@lcObjVal,.F.,'S')
  lcObjVal = IIF(llObjRet , lcObjVal , SPACE(01))
  llBrowse = .F.
ENDIF    && End of IF
&lcObjNam = lcObjVal

*-- End OF lfvVendor
*:*************************************************************
*: Name      : lfCheckMaj
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Check style code structure.
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfCheckMaj()
*:*************************************************************
*:
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

*-- End OF lfCheckMaj
*:*************************************************************
*: Name      : lfStyBrow
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Browse the selected style from the selection grid
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfStyBrow()
*:*************************************************************
*:
FUNCTION lfStyBrow
PRIVATE lnAlias

lnAlias = SELECT()
SELECT(lcStyTmp)
lcBrowStr = 'TotDb=IIF(RECNO()=lnStyRecNo,">"," "):R:H=" ",cSelect :R:H=" ",Style:R ,'+;
  'Plan1:R:H="OTS1",Plan2:R:H="OTS2",' +;
  'Plan3:R:H="OTS3",Plan4:R:H="OTS4",' +;
  'Plan5:R:H="OTS5",Plan6:R:H="OTS6",' +;
  'Plan7:R:H="OTS7",Plan8:R:H="OTS8",TotPlan:R:H="Tot. OTS",' +;
  'nPOQty1:R:H="Ord1",nPOQty2:R:H="Ord2",' +;
  'nPOQty3:R:H="Ord3",nPOQty4:R:H="Ord4",' +;
  'nPOQty5:R:H="Ord5",nPOQty6:R:H="Ord6",' +;
  'nPOQty7:R:H="Ord7",nPOQty8:R:H="Ord8",nPOTotQty:R:H="Tot. Order"'
BROWSE FIELDS &lcBrowStr;
  NOAPPEND ;
  NOWAIT   ;
  NOMENU   ;
  NOCLEAR  ;
  TITLE lcSrcTtl;
  WHEN lfStyWhen() ;
  WINDOW (lcWinC12) IN WINDOW (gcBaseWind) ;
  VALID lfReadAct()
SELECT(lnAlias)

*-- End OF lfStyBrow
*:*************************************************************
*: Name      : lfStyWhen
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Called from the when of the style browse
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfStyWhen()
*:*************************************************************
*:
FUNCTION lfStyWhen
PRIVATE lcStatus

lcStatus = IIF(laScrMode[3],'ENABlE','DISABLE')
SHOW GET pbSelect,1 PROMPT IIF(EMPTY(&lcStyTmp..cSelect),"\<Select","Un\<Select") &lcStatus
=SEEK(&lcStyTmp..Style,'Style') AND SEEK('S'+Style.Scale,'Scale')
lcScalDesc = Scale.cScl_Desc
FOR lnCount = 1 TO 8
  lcCount = STR(lnCount,1)
  lcSZ&lcCount = Scale.SZ&lcCount
ENDFOR
lnStyRecNo=RECNO(lcStyTmp)
SHOW WINDOW (lcSrcTtl) REFRESH SAME
=lfReFresh(lcWinC11)

*-- End OF lfStyWhen
*:*************************************************************
*: Name      : lfAdUnCmSR
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Adding record in uncomplete session file
*:*************************************************************
*: Calls     :
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : NONE
*:*************************************************************
*: Returns            : NONE
*:*************************************************************
*: Example   : =lfAdUnCmSR()
*:*************************************************************
*:
FUNCTION lfAdUnCmSR
PRIVATE lnAlias

lnAlias = SELECT()
SELECT UnCmSess

IF !SEEK('I'+PADR(lcProgID,10)+PADR(gcUser_id,10)) AND RLOCK()
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

*-- End OF lfAdUnCmSR
*:*************************************************************
*: Name      : lfBrowCut
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Browse function for the header file
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfBrowCut()
*:*************************************************************
*:
FUNCTION lfBrowCut
PRIVATE lcBrowStr

SELECT (lcHdrFile)
lnCutRecNo=RECNO()
lcBrowStr = "CMARK=IIF(RECNO()=lnCutRecNo,'>',' '):H=' ',PO :H='PO#' :P='999999' :V=lfvBroWCt() :W=lfwOldCtkt(),;
  VENDOR:R,CDIVISION:R:H='Division',"+IIF(llWareHous,"CWARECODE:P='@!':H='ShipTo.':V=lfvWareH():W=lfwOldVals(),","")+;
  "ShipVia:R,CTERMCODE:R:H='Terms',Entered:V=(!EMPTY(Entered) AND Entered <= Complete):F:E='Entered date must be less than or equal completion date',"+;
  "Complete:V=(!EMPTY(Complete) AND Complete>=Entered):F:E='Completion date must be greater than or equal Entered date',"+;
  "nStyOrder:R :H='Order',totord:H='OTS':R,totcost=nFcost1+nFcost2+nFcost3+nFcost4+nFcost5:H='Tot. Cost',"+;
  "nFcost1:H=lcISLbl1:R,nFcost2:H=lcISLbl2:R,nFcost3:H=lcISLbl3:R,nFcost4:H=lcISLbl4:R,nFcost5:R:H=lcISLbl5"

BROWSE FIELDS &lcBrowStr;
  NOAPPEND ;
  NOWAIT   ;
  NOMENU   ;
  NOCLEAR  ;
  TITLE lcTktTtl ;
  WHEN lfCutWhen() ;
  WINDOW (lcWinC14) IN WINDOW (gcBaseWind)

*-- End OF lfBrowCut
*:*************************************************************
*: Name      : lfvBroWCt
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : CutTkt Valid Fuunction
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfvBroWCt()
*:*************************************************************
*:
FUNCTION lfvBroWCt

PRIVATE lnOldAls , lcNewCutNo , llRet , lcToSeekIn , lcStrToAdd , lcHdrVal

lnOldAls = SELECT(0)

IF !llGManualy
    REPLACE PO     WITH ''
  RETURN
ENDIF

lcHdrVal   = PO+CDIVISION+CPURCODE+CSTYGRADE
lcNewCutNo = IIF(!EMPTY(PO),'P'+PO,PO)
lcToSeekIn = 'POSHDR'
lcStrToAdd = "P/O"
llRet = .F.

IF EMPTY(lcNewCutNo)
  lnNoThing = gfModalGen('TRM38047B00000','DIALOG',lcStrToAdd)
  REPLACE PO     WITH lcOldCtkNo
ELSE
  IF LEN(ALLTRIM(lcNewCutNo))-1 < 6
    lnNoThing = gfModalGen('TRM38048B00000','DIALOG',lcStrToAdd)
      REPLACE PO     WITH lcOldCtkNo
  ELSE
    IF SEEK(lcNewCutNo,lcToSeekIn) .OR. SEEK(SUBSTR(lcNewCutNo,2),lcLinFile)
      lnNoThing = gfModalGen('TRM38049B00000','DIALOG',lcStrToAdd)
      lcNewCutNo  = ''
      SELECT (lcHdrFile)
      REPLACE PO     WITH lcOldCtkNo
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
                     PO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+DYELOT = +;
                     lcOldCtkNo+SUBSTR(lcHdrVal,7)
  ENDIF
ENDIF
SELECT(lnOldAls)

*-- End Of lfvBroWCt
*:*************************************************************
*: Name      : lfwOldCtkt
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Get old no.
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfwOldCtkt()
*:*************************************************************
*:
FUNCTION lfwOldCtkt

lcOldCtkNo = PO

*-- End OF lfwOldCtkt
*:*************************************************************
*: Name      : lfvWareH
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Called from the location field in the browse of the header file
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfvWareH()
*:*************************************************************
*:
FUNCTION lfvWareH
PRIVATE lcWareCode,lnAlias

lnAlias = SELECT()
IF !SEEK(&lcHdrFile..cWareCode,'WAREHOUS')
  SELECT WareHous
  lcWareCode  = gfbrowware(.T.,.F.,.F.,.F.,.F.,'S')
  IF EMPTY(lcWareCode)
    lcWareCode = lnOldVal
  ENDIF
  REPLACE &lcHdrFile..cWareCode WITH lcWareCode
ENDIF
SELECT (lnAlias)
RETURN .T.

*-- End OF lfvWareH
*:*************************************************************
*: Name      : lfwOldVals
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : get the old value of the editable fields on the browse
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfwOldVals()
*:*************************************************************
*:
FUNCTION lfwOldVals

lnOldVal = EVALUATE(SYS(18))

*-- End OF lfwOldVals
*:*************************************************************
*: Name      : lfCutWhen
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : When fumction for the browse of the header file
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfCutWhen()
*:*************************************************************
*:
FUNCTION lfCutWhen

lnCutRecNo=RECNO(lcHdrFile)
SHOW WINDOW (lcTktTtl) REFRESH SAME

*-- End Of lfCutWhen.
*:*************************************************************
*: Name      : lfChkUnCmS
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Check if there is uncomplete session
*:*************************************************************
*: Calls     :
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : NONE
*:*************************************************************
*: Returns            : NONE
*:*************************************************************
*: Example   : =lfChkUnCmS()
*:*************************************************************
*:
FUNCTION lfChkUnCmS
PARAMETERS llFromSetUp
PRIVATE lnAlias, llGoOut, lnReprocess

IF llFromSetUp
  llChecked = .T.
ENDIF
llGoOut = .F.
IF gfUnCompSession(lcProgID,lnsessno,lcProgTxt)
  =lfStyBrow()
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

*-- End OF lfChkUnCmS
*:*************************************************************
*: Name      : lfUpdVars
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Update the variable string
*:*************************************************************
*: Calls     :
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : NONE
*:*************************************************************
*: Returns            : NONE
*:*************************************************************
*: Example   : =lfUpdVars()
*:*************************************************************
*:
FUNCTION lfUpdVars
PRIVATE lnAlias

lnAlias = SELECT()
lcFiles = "lcPOH,"     + lcPOH     + "," +ORDER(lcPOH)    + ";" + ;
          "lcStyTmp,"  + lcStyTmp + "," + ";"
SELECT UnCmSess
=RLOCK()
=IIF(lnUnCmSeRc=0,.T.,gfSavSess(lcProgID, lcFiles, @laVars))
SELECT(lnAlias)

*-- End OF lfUpdVars
*:*************************************************************
*: Name      : lfUpdUnCmS
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Update the current object and the status of the session
*:*************************************************************
*: Calls     :
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : NONE
*:*************************************************************
*: Returns            : NONE
*:*************************************************************
*: Example   : =lfUpdUnCmS()
*:*************************************************************
*:
FUNCTION lfUpdUnCmS
PARAMETERS lcStatus,lcCurObj

PRIVATE llByMe
IF !USED('UnCmSess')
  llByMe = gfOpenFile(gcDataDir+'UnCmSess',gcDataDir+'Trans','SH')
ENDIF
IF BETWEEN(lnUnCmSeRc,1,RECCOUNT('UnCmSess'))
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

*-- End OF lfUpdUnCmS
*:*************************************************************
*: Name      : lfCrtTemp
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Create Temporary files
*:*************************************************************
*: Calls     : gfCrtTmp()
*:*************************************************************
*: Parameters: none
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfCrtTemp()
*:*************************************************************
*:
FUNCTION lfCrtTemp


SELECT POSHDR
=AFIELDS(lafilfield)
lnAlen = ALEN(laFilField,1)
DIMENSION laFilField[lnAlen+3,4]
laFilField[lnAlen+1,1] = 'cStyGrade'
laFilField[lnAlen+1,2] = 'C'
laFilField[lnAlen+1,3] = 1
laFilField[lnAlen+1,4] = 0
lafilfield[lnAlen+2,1] = 'nSteps'
lafilfield[lnAlen+2,2] = 'N'
lafilfield[lnAlen+2,3] = 2
lafilfield[lnAlen+2,4] = 0
lafilfield[lnAlen+3,1] = 'cTmpPo'
lafilfield[lnAlen+3,2] = 'C'
lafilfield[lnAlen+3,3] = 6
lafilfield[lnAlen+3,4] = 0
=gfCrtTmp(lcPOH,@lafilfield,[PO+CDIVISION+CPURCODE+CSTYGRADE],[CVENDIV])

SELECT STYLE
=AFIELDS(lafilfield)
lnAlen = ALEN(lafilfield,1)
DIMENSION lafilfield[lnAlen+10,4]
lafilfield[lnAlen+1,1] = 'cSelect'
lafilfield[lnAlen+1,2] = 'C'
lafilfield[lnAlen+1,3] = 1
lafilfield[lnAlen+1,4] = 0

lafilfield[lnAlen+2,1] = 'nPOQty1'
lafilfield[lnAlen+2,2] = 'N'
lafilfield[lnAlen+2,3] = 6
lafilfield[lnAlen+2,4] = 0


lafilfield[lnAlen+3,1] = 'nPOQty2'
lafilfield[lnAlen+3,2] = 'N'
lafilfield[lnAlen+3,3] = 6
lafilfield[lnAlen+3,4] = 0

lafilfield[lnAlen+4,1] = 'nPOQty3'
lafilfield[lnAlen+4,2] = 'N'
lafilfield[lnAlen+4,3] = 6
lafilfield[lnAlen+4,4] = 0

lafilfield[lnAlen+5,1] = 'nPOQty4'
lafilfield[lnAlen+5,2] = 'N'
lafilfield[lnAlen+5,3] = 6
lafilfield[lnAlen+5,4] = 0

lafilfield[lnAlen+6,1] = 'nPOQty5'
lafilfield[lnAlen+6,2] = 'N'
lafilfield[lnAlen+6,3] = 6
lafilfield[lnAlen+6,4] = 0

lafilfield[lnAlen+7,1] = 'nPOQty6'
lafilfield[lnAlen+7,2] = 'N'
lafilfield[lnAlen+7,3] = 6
lafilfield[lnAlen+7,4] = 0

lafilfield[lnAlen+8,1] = 'nPOQty7'
lafilfield[lnAlen+8,2] = 'N'
lafilfield[lnAlen+8,3] = 6
lafilfield[lnAlen+8,4] = 0

lafilfield[lnAlen+9,1] = 'nPOQty8'
lafilfield[lnAlen+9,2] = 'N'
lafilfield[lnAlen+9,3] = 6
lafilfield[lnAlen+9,4] = 0

lafilfield[lnAlen+10,1] = 'nPOTotQty'
lafilfield[lnAlen+10,2] = 'N'
lafilfield[lnAlen+10,3] = 6
lafilfield[lnAlen+10,4] = 0


DECLARE laIndex[2,2]
laIndex[1,1] = 'CSELECT+CDIVISION+SEASON+STYLE'
laIndex[1,2] = 'TICKET'
laIndex[2,1] = 'STYLE'
laIndex[2,2] = lcStyTmp
=gfCrtTmp(lcStyTmp,@lafilfield,@laIndex)
SET ORDER TO TAG (lcStyTmp) IN (lcStyTmp)

*-- End OF lfCrtTemp
*:*************************************************************
*: Name      : lfCrtDTemp
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Create Temporary details files
*:*************************************************************
*: Calls     : gfCrtTmp()
*:*************************************************************
*: Parameters: none
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfCrtDTemp()
*:*************************************************************
*:
FUNCTION lfCrtDTemp

IF !USED(lcPOLine)
  =gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLN','SH')
  =AFIELDS(lafilfield)
  lnAlen = ALEN(lafilfield,1)
  DIMENSION lafilfield[lnAlen+7,4]
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
  =gfCrtTmp(lcPOLine,@lafilfield,[PO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+DYELOT],[CSTYCLR])
ENDIF

*-- End Of lfCrtDTemp.
*:*************************************************************
*: Name      : lfSelStyle
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Select the records matching the criteria
*:             from the style file in case of generate C/T from Plan
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfSelStyle()
*:*************************************************************
*:
FUNCTION lfSelStyle

PRIVATE lnAlias,lcCutTag,lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,;
  lnQty8,llContinue

lnAlias = SELECT()
IF TYPE('lcExpr')='L'
  RETURN(.F.)
ENDIF
llOpenMe   = .F.
lcBasFile  = 'STYLE'
lcWarehuse = ''
IF llWareHous
  llOpenMe =gfOpenFile(gcDataDir+'STYDYE', gcDataDir+'STYDYE','SH')
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

SELECT (lcStyTmp)
DELETE ALL
SELECT STYLE
SET ORDER TO TAG STYLE

SET RELATION TO 'N'+'COLOR     '+SUBSTR(Style.Style,lnColorStr,lnColorLen);
  INTO CODES ADDITIVE

IF llWareHous
  SET RELATION TO style  INTO STYDYE ADDITIVE
ENDIF

STORE .F. TO llContinue
SCAN FOR &lcExpr AND !MAKE 
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
    STORE 0 TO M.nPOQty1,M.nPOQty2,M.nPOQty3,M.nPOQty4,M.nPOQty5,M.nPOQty6,M.nPOQty7,M.nPOQty8,M.nPOTotQty
    
    INSERT INTO (lcStyTmp) FROM MEMVAR
  ENDIF
  SELECT Style
ENDSCAN

SET RELATION OFF INTO CODES

IF llWareHous
  SET RELATION OFF INTO STYDYE
ENDIF

IF llOpenMe
  USE IN STYDYE
ENDIF

IF !llContinue
  *-- if no records matching the criteria give the user a message
  =lfUpdUnCmS('I',SPACE(0) )
  =lfvSelect('')  
  SHOW GET pbSelect  DISABLE
  SHOW WINDOW (lcSrcTtl) REFRESH SAME
  laScrMode = .F.
  laScrMode[1] = .T.
  =gfModalGen('INM38129B38018','DIALOG','styles')
ENDIF
GO TOP IN (lcStyTmp)

SELECT(lnAlias)
RETURN(llContinue)

*-- End OF lfSelStyle.
*:*************************************************************
*: Name      : lpClsScr
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Revert & Close Screen
*:*************************************************************
*: Calls     : lfDelFiles(),lfUpdUnCmS()
*:*************************************************************
*: Parameters: none
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lpClsScr()
*:*************************************************************
*:
PROCEDURE lpClsScr

llRetToSel = .F.

=lfDelFiles(.T.)
=lfStyWhen() AND lfCutWhen()
IF SEEK('O'+lcProgID+lcUserID,'UnCmSess')
  lnUnCmSeRc = RECNO('UnCmSess')
ENDIF
=lfUpdUnCmS("I", SPACE(0))

*-- End OF lpClsScr
*:*************************************************************
*: Name      : lfDelFiles
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Delete temporary files
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: none
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfDelFiles()
*:*************************************************************
*:
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


IF USED(lcStyTmp)
  SELECT(lcStyTmp)
  SCAN
    REPLACE nPOQty1 WITH 0,;
            nPOQty2 WITH 0,;
            nPOQty3 WITH 0,;
            nPOQty4 WITH 0,;
            nPOQty5 WITH 0,;
            nPOQty6 WITH 0,;
            nPOQty7 WITH 0,;
            nPOQty8 WITH 0,;
            nPOTotQty WITH 0

  ENDSCAN
  LOCATE
ENDIF

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

*-- End OF lfDelFiles
*:*************************************************************
*: Name      : lfvSelect
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Called from the selection buttons
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfvSelect()
*:*************************************************************
*:
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
  SUM REST PLAN1,PLAN2,PLAN3,PLAN4,PLAN5,PLAN6,PLAN7,PLAN8;
  TO   lnTotal1,lnTotal2,lnTotal3,lnTotal4,lnTotal5,lnTotal6,lnTotal7,lnTotal8 ;
  WHILE cSelect = "�"
  SHOW GET pbSelNone ENABLE
  SHOW GET pbDefault ENABLE
  IF !EMPTY(lcVendor)
    SHOW GET pbGenerate,1 PROMPT "\<Generate P/O" ENABLE
  ELSE
    SHOW GET pbGenerate,1 PROMPT "\<Generate P/O" DISABLE
  ENDIF
  lcNote='\<Notes'
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
  SHOW GET pbDefault  DISABLE
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

*-- End OF lfvselect
*:*************************************************************
*: Name      : lfReadAct
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : READ Activate function
*:*************************************************************
*: Calls     : lfClearKey.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  =lfReadAct()
*:*************************************************************
*:
FUNCTION lfReadAct

=gfStopBrow()

=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

*-- End OF lfReadAct
*:*************************************************************
*: Name      : lfClearKey
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Clear Hot Keys
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  =lfClearKey()
*:*************************************************************
*:
FUNCTION lfClearKey

ON KEY LABEL ALT+B
ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+HOME
ON KEY LABEL CTRL+END
ON KEY LABEL TAB
ON KEY LABEL BACKTAB

*-- End OF lfClearKey
*:*************************************************************
*: Name      : lpMovTab
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Trap of tab key.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  DO lpMovTab WITH 'lcWinC121', 'm.Store',.T.
*:*************************************************************
*:
PROCEDURE lpMovTab
PARAMETERS lcWindName, lcObjName,llToCheck


ON KEY LABEL TAB
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*-- End OF lpMovTab
*:*************************************************************
*: Name      : lpBackTab
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Trap of tab key.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  DO lpBackTab WITH 'lcWinC121', 'm.Store',.T.
*:*************************************************************
*:
PROCEDURE lpBackTab
PARAMETERS lcWindName, lcObjName,llToCheck


ON KEY LABEL BACKTAB
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*-- End OF lpBackTab
*:*************************************************************
*: Name      : lfDeactMain
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Deactivate main Screen
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  =lfDeactMain()
*:*************************************************************
*:
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
= lfReadAct()

ENDIF

RETURN .F.

*-- End OF lfDeactMain
*:*************************************************************
*: Name      : lfPODef
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Called from the P/O defaults button
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfPODef()
*:*************************************************************
*:
FUNCTION lfPODef

=gfOpenFile(gcSysHome+'SYCCURR', gcSysHome+'CCURRCODE','SH')
=gfOpenFile(gcDataDir+'APVENDOR', gcDataDir+'Vencode','SH')

*-- Call the P/O defaults screen
DO (gcScrDir+gcWinAppl+'\POGENPL6.SPX')
=gfCloseFile('SYCCURR')
=gfCloseFile('APVENDOR')

IF !EMPTY(lcVendor) AND (!llRpBOPFab OR llAllocate)
  SHOW GET pbGenerate ENABLE
ELSE
  SHOW GET pbGenerate DISABLE
ENDIF

*-- End OF lfPODef
*:*************************************************************
*: Name      : lfShowDef
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Show function for the objects of the PO default screen
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfShowDef()
*:*************************************************************
*:
FUNCTION lfShowDef

IF lcPCurr = gcBaseCurr OR !llEditExRt
  SHOW GET lnPRate DISABLE
ELSE
  SHOW GET lnPRate ENABLE
ENDIF
IF lcDCurr = gcBaseCurr OR !llEditExRt
  SHOW GET lnDRate DISABLE
ELSE
  SHOW GET lnDRate ENABLE
ENDIF

*-- End OF lfShowDef.
*:*************************************************************
*: Name      : lfvDefVen
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Validation of the vendor at the PO defaults screen
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfvDefVen()
*:*************************************************************
*:
FUNCTION lfvDefVen

lnAlias = SELECT()

*-- If the enterd vendor does not ecxit in the apvendor file
IF llBrowse OR (!EMPTY(lcVendor) AND !SEEK(lcVendor,'APVENDOR')) .OR. ;
  !('S' $ APVENDOR.cVenSupTyp .OR. 'C' $ APVENDOR.cVenSupTyp)
  *-- filter vendor browse by sup. type
  =gfApVnBrow(@lcVendor,.F.,'CS')
ENDIF
llBrowse = .F.

IF !EMPTY(lcVendor)
  lcVenName = ApVendor.cVenComp
  IF SEEK('N'+'CTERMCODE '+APVENDOR.CTERMCODE,'CODES')
    = gfwCodePop(@laCodInfo, "CTERMCODE", "L")
    lnTerm = ASUBSCRIPT(laTerm,ASCAN(laTerm,APVENDOR.CTERMCODE),1)
    SHOW GET lnTerm
  ELSE
    = gfwCodePop(@laCodInfo, "CTERMCODE", "D")
  ENDIF
  *-- if the system id set to use multi currency
  IF llMulCurr
    lcPCurr = ApVendor.ccurrcode
    lcDcurr = gcBaseCurr
    
    lnPRate = gfchkrate('lnUnit1',lcPCurr ,gdSysDate,llEditExRt,gcAct_Comp,.F.)
    lnDRate = gfchkrate('lnUnit2',lcDcurr ,gdSysDate,llEditExRt,gcAct_Comp,.F.)
    *-- if there is no valid rate for the entered currency,give a message
    *-- (No valid rate .default to the base currency)
    IF lnPRate <= 0
      =gfModalGen('INM36004B36001','DIALOG','price') = 1
      lcPCurr = gcBaseCurr
      lnPRate = 1
    ENDIF
    IF lnDRate <= 0
      =gfModalGen('INM36004B36001','DIALOG','duty') = 1
      lcDCurr = gcBaseCurr
      lnDRate = 1
    ENDIF
  ENDIF
ENDIF
*-- Check if the user has the right to edit the rate
IF lcPCurr = gcBaseCurr OR !llEditExRt
  SHOW GET lnPRate DISABLE
ELSE
  SHOW GET lnPRate ENABLE
ENDIF
IF lcDCurr = gcBaseCurr OR !llEditExRt
  SHOW GET lnDRate DISABLE
ELSE
  SHOW GET lnDRate ENABLE
ENDIF
SHOW GET lcPCurr
SHOW GET lcDCurr
=lfRefresh('MFGENCT4')
SELECT (lnAlias)

*-- End OF lfvDefVen
*:*************************************************************
*: Name      : lfvDefPCurr
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Validation of the default price currency
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfvDefPCurr()
*:*************************************************************
*:
FUNCTION lfvDefPCurr

*-- if the entered currency does not exist in the currency code file
IF llBrowse OR !SEEK(lcPCurr,"SycCurr")
  IF !gfcurrbrow(@lcPCurr)
    lcPCurr = gcBaseCurr
  ENDIF
ENDIF
llBrowse = .F.
*-- get the final exchange rate of the price currency
lnPRate = gfchkrate('lnUnit1',lcPCurr ,gdSysDate,llEditExRt,gcAct_Comp,.F.)
*-- if there is no valid rate
IF lnPRate <= 0
  IF gfModalGen('INM36004B36001','DIALOG','price') = 1
    lcPCurr = gcBaseCurr
    lnPRate = 1
    lnUnit1 = 1
  ELSE
    _CUROBJ = OBJNUM(lcPCurr)
  ENDIF
ENDIF
IF lcPCurr = gcBaseCurr OR !llEditExRt
  SHOW GET lnPRate DISABLE
ELSE
  SHOW GET lnPRate ENABLE
ENDIF

*-- End OF lfvDefPCurr.
*:*************************************************************
*: Name      : lfvDefDCurr
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Validation of the default duty currency
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfvDefDCurr()
*:*************************************************************
*:
FUNCTION lfvDefDCurr

IF llBrowse OR !SEEK(lcDCurr,"SycCurr")
  IF !gfcurrbrow(@lcDCurr)
    lcDCurr = gcBaseCurr
  ENDIF
ENDIF
llBrowse = .F.
lnDRate = gfchkrate('lnUnit2',lcDCurr ,gdSysDate,llEditExRt,gcAct_Comp,.F.)
lcSign2 = gfGetExSin("",ALLTRIM(lcDCurr))
IF lnPRate <= 0
  IF gfModalGen('INM36004B36001','DIALOG','duty') = 1
    lcDCurr = gcBaseCurr
    lnDRate = 1
    lnUnit2 = 1
  ELSE
    _CUROBJ = OBJNUM(lnDRate)
  ENDIF
ENDIF
IF lcDCurr = gcBaseCurr OR !llEditExRt
  SHOW GET lnDRate DISABLE
ELSE
  SHOW GET lnDRate ENABLE
ENDIF

*-- End OF lfvDefDCurr.
*:*************************************************************
*: Name      : lfvOK
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Validation of <OK> of the default PO screen
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfvOK()
*:*************************************************************
*:
FUNCTION lfvOK

*-- if the vendor is left empty, give the user a message
*-- (vendor field cannot be left empty)
IF EMPTY(lcVendor)
  =gfModalGen('INM36002B36000','DIALOG','Vendor')
  _CUROBJ = OBJNUM(lcVendor)
  RETURN
ENDIF
=SEEK(lcVendor,'APVENDOR')
lcPhone = ApVendor.cphoneno
lcCont  = ApVendor.cVenCont
CLEAR READ
=lfUpdVars()

*-- End OF lfvOK
*:*************************************************************
*: Name      : lfvGenerate
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Called from the Genetrate or Clear button
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfvGenerate()
*:*************************************************************
*:
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
  DO (gcScrDir+gcWinAppl+'\POGENPL7.SPX')
ELSE
  *-- Clear generated mode
  =lfDelFiles(.F.)
ENDIF

*-- 
=lfCutWhen()

GOTO TOP IN (lcSelFile)
=lfStyWhen()
STORE .T. TO llcUpdate
STORE .F. TO laScrMode
IF llStartGen
  laScrMode[4] = .T.
ELSE
  laScrMode[3] = .T.
ENDIF
SELECT(lnAlias)

DO lpShow

*-- End OF lfvGenerate
*:*************************************************************
*: Name      : lfvDefWare
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Validation of the warehouse
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfvDefWare()
*:*************************************************************
*:
FUNCTION lfvDefWare

IF llBrowse OR !SEEK(lcWareCode,'WAREHOUS')
  lcWareCode= gfbrowware(.T.,.F.,.F.,.F.,.F.,'S')
ENDIF
llBrowse = .F.

*-- End OF lfvDefWare
*:*************************************************************
*: Name      : lfvGenCtPO
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Function to check if the generation can bedone or not
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfvGenCtPO()
*:*************************************************************
*:
FUNCTION lfvGenCtPO

*-- If the location is left empty give the user a message
*-- (You have to enter a warehouse code)
IF llWareHous AND EMPTY(lcWareCode)
  =gfModalGen('INM38138B38018','DIALOG')
  RETURN
ENDIF
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
  CLEAR READ
  lcFile = lcStyTmp
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
  *-- 'purchase orders with zero quantities. Cannot proceed
  *-- 'with generation.)
  =gfModalGen('INM38131B38018','DIALOG','purchase orders')
ENDIF

*-- End OF lfvGenCtPO.
*:*************************************************************
*: Name      : lfUpdPOPik
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Generate P/O's
*:*************************************************************
*: Calls     : gfGetExSin(),lfUnAllPO()
*:*************************************************************
*: Parameters: lcOrdFile : Order line temporary file name
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfUpdPOPik(lcOrdFile)
*:*************************************************************
*:
FUNCTION lfUpdPOPik
PARAMETERS lcOrdFile

PRIVATE lnAlias,lcStyOrd,lcPUntSin,lcDUntSin,lcPExSign,lcDExSign,lcDivision,;
  lcPurCode,lcStyGrade,lnTCost1,lnTCost2,lnTCost3,lnTCost4,lnTCost5,;
  lnOrder,lnAllocat,lnTFCost1,lnTFCost2,lnTFCost3,lnTFCost4,lnTFCost5
PRIVATE lnCutQty1,lnCutQty2,lnCutQty3,lnCutQty4,lnCutQty5,lnCutQty6,lnCutQty7,lnCutQty8,;
  lnOrdQty1,lnOrdQty2,lnOrdQty3,lnOrdQty4,lnOrdQty5,lnOrdQty6,lnOrdQty7,lnOrdQty8,lcOld_Ordr

WAIT 'Generating Purchase Orders. Please standby....' WINDOW NOWAIT
STORE '/' TO lcPUntSin,lcDUntSin
lcPExSign = gfGetExSin(@lcPUntSin,lcPCurr)
lcDExSign = gfGetExSin(@lcDUntSin,lcDCurr)
lnAlias = SELECT()
SELECT STYLE
lcStyOrd = TAG()
SET ORDER TO TAG STYLE

SELECT (lcOrdFile)
lcOld_Ordr = ORDER()
SET ORDER TO TAG 'TICKET'
=SEEK("�")


STORE 0 TO lnTCost1,lnTCost2,lnTCost3,lnTCost4,lnTCost5,lnOrder,lnAllocat,;
           lnTFCost1,lnTFCost2,lnTFCost3,lnTFCost4,lnTFCost5

DO WHILE CSELECT+CDIVISION+SEASON+STYLE = "�"
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


  SELECT (lcPOH)
  IF !SEEK(SPACE(6)+lcDivision+lcPurCode+lcStyGrade)
    APPEND BLANK
    REPLACE cStyType   WITH 'P' ,;
            Vendor     WITH lcVendor ,;
            STATUS     WITH 'H',;
            CDIVISION  WITH lcDivision,;
            cPurCode   WITH lcPurCode ,;
            ENTERED    WITH gdSysDate ,;
            COMPLETE   WITH ENTERED+90,;
            SHIPVIA    WITH laShip[lnShip,2]   ,;
            CTERMCODE  WITH laTerm[lnTerm,2]   ,;
            cWareCode  WITH lcWareCode ,;
            cStyGrade  WITH lcStyGrade ,;
            cPriceCur  WITH lcPCurr ,;
            nPriceRat  WITH lnPRate ,;
            nCurrUnit  WITH lnUnit1 ,;
            cDutyCur   WITH lcDCurr ,;
            nDutyRat   WITH lnDRate ,;
            nDCurUnit  WITH lnUnit2 ,;
            lMultiWare WITH (lnType=2)

    STORE 0 TO lnTCost1,lnTCost2,lnTCost3,lnTCost4,lnTCost5,lnOrder,lnAllocat,;
               lnTFCost1,lnTFCost2,lnTFCost3,lnTFCost4,lnTFCost5
            
  ENDIF

  IF llDispPric
    STORE 0 TO lnSelPrice,lnAllQty,lnRotSub,lnGrosMrgn
  ENDIF
  

  SELECT (lcOrdFile)
  llMultiWare = .F.
  lcPO_No = CDIVISION+SEASON+STYLE
  STORE 0 TO lnCutQty1,lnCutQty2,lnCutQty3,lnCutQty4,lnCutQty5,lnCutQty6,lnCutQty7,lnCutQty8
  STORE 0 TO lnOrdQty1,lnOrdQty2,lnOrdQty3,lnOrdQty4,lnOrdQty5,lnOrdQty6,lnOrdQty7,lnOrdQty8
  
  SCAN REST WHILE  CSELECT+CDIVISION+SEASON+STYLE  = "�" + lcPO_No

    =SEEK(STYLE,'STYLE')
    lcDivision = STYLE.CDIVISION
    lcPurCode  = STYLE.cPurCode
    lcStyGrade = STYLE.cStyGrade
    lcStyle    = Style.STYLE
  
    lnCutQty1 = lnCutQty1+INT(laPercet[1]/100 * &lcOrdFile..Plan1)
    lnCutQty2 = lnCutQty2+INT(laPercet[2]/100 * &lcOrdFile..Plan2)
    lnCutQty3 = lnCutQty3+INT(laPercet[3]/100 * &lcOrdFile..Plan3)
    lnCutQty4 = lnCutQty4+INT(laPercet[4]/100 * &lcOrdFile..Plan4)
    lnCutQty5 = lnCutQty5+INT(laPercet[5]/100 * &lcOrdFile..Plan5)
    lnCutQty6 = lnCutQty6+INT(laPercet[6]/100 * &lcOrdFile..Plan6)
    lnCutQty7 = lnCutQty7+INT(laPercet[7]/100 * &lcOrdFile..Plan7)
    lnCutQty8 = lnCutQty8+INT(laPercet[8]/100 * &lcOrdFile..Plan8)
    
    lnOrdQty1 = lnOrdQty1 + &lcOrdFile..Plan1
    lnOrdQty2 = lnOrdQty2 + &lcOrdFile..Plan2
    lnOrdQty3 = lnOrdQty3 + &lcOrdFile..Plan3
    lnOrdQty4 = lnOrdQty4 + &lcOrdFile..Plan4
    lnOrdQty5 = lnOrdQty5 + &lcOrdFile..Plan5
    lnOrdQty6 = lnOrdQty6 + &lcOrdFile..Plan6
    lnOrdQty7 = lnOrdQty7 + &lcOrdFile..Plan7
    lnOrdQty8 = lnOrdQty8 + &lcOrdFile..Plan8
    IF llDispPric
      STORE 0 TO lnTotQty
      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        lnTotQty = lnTotQty + INT(laPercet[lnI]/100 * (&lcOrdFile..Plan&lcI))
      ENDFOR
      lnAllQty = lnAllQty + lnTotQty
      lnselPrice = lnSelPrice + ( lnTotQty*&lcOrdFile..Price )
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

  IF llDispPric
    lnSelPrice = lnSelPrice / lnAllQty
  ENDIF
  IF lnCutQty1+lnCutQty2+lnCutQty3+lnCutQty4+lnCutQty5+lnCutQty6+lnCutQty7+lnCutQty8 > 0
    SELECT (lcPOLine)
    IF !SEEK(SPACE(6)+lcDivision+lcPurCode+lcStyGrade+lcStyle+lcDyelot)
      SELECT (lcPOH)
      REPLACE LASTLINE WITH LASTLINE + 1
      SELECT (lcPOLine)
      APPEND BLANK
      REPLACE STYLE     WITH lcStyle    ,;
              DYELOT    WITH lcDyelot   ,;
              TRANCD    WITH '1'        ,;
              CDIVISION WITH lcDivision ,;
              cWareCode WITH lcWareCode ,;
              LINENO    WITH &lcPOH..LastLine ,;
              Vendor    WITH lcVendor   ,;
              cPurCode  WITH lcPurCode  ,;
              cStyGrade WITH lcStyGrade ,;
              Fabric    WITH lcFabric   ,;
              cFabClr   WITH lcFabClr   ,;
              cFabWare  WITH lcFabWare  ,;
              nYeild    WITH lnYeild

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
    =SEEK(lcStyle,'Style')
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
  SELECT (lcOrdFile)

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

SELECT (lcOrdFile)
SET ORDER TO &lcOld_Ordr
SET ORDER TO TAG (lcStyOrd) IN STYLE
SELECT (lnAlias)
WAIT CLEAR

*-- End OF lfUpdPOPik
*:*************************************************************
*: Name      : lfvDetail
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : to call the screen of the details lines
*:             of the generated P/O or C/T
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfvDetail()
*:*************************************************************
*:
FUNCTION lfvDetail

*-- Set a relation between the lines file and temp. cutpick and ordhdr file
SELECT STYLE
SET ORDER TO TAG STYLE
SET RELATION TO 'S'+SCALE INTO SCALE
SELECT (lcLinFile)
SET RELATION TO STYLE INTO STYLE

STORE 0 TO lnRecBrow1,lnRecBrow2
SELECT(lcLinFile)
GOTO TOP

PUSH KEY
=lfClearKey()
DO (gcScrDir+gcWinAppl+'\POGENPL8.SPX')
SELECT (lcLinFile)
SET RELATION TO
SET KEY TO
POP KEY

*-- End OF lfvDetail.
*:*************************************************************
*: Name      : lfDeactDet
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Deactivate main Screen
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  =lfDeactDet()
*:*************************************************************
*:
FUNCTION lfDeactDet

IF WONTOP()=lcBrTtl1
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  glFromBrow = .T.
  IF WONTOP()=lcBrTtl1
    ON KEY LABEL TAB     DO lpMovTab  WITH 'POGENP85','pbClose'
    ON KEY LABEL BACKTAB DO lpBackTab WITH 'POGENP85','pbClose'
  ENDIF
ELSE
  glFromBrow = .F.
ENDIF
RETURN .F.

*-- End OF lfDeactDet
*:*************************************************************
*: Name      : lfBrowLines
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Browse of the lines file
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfBrowLines()
*:*************************************************************
*:
FUNCTION lfBrowLines
PRIVATE lnAlias,lcBrowFilds

lnAlias = SELECT()
SELECT (lcPOLine)
SET KEY TO &lcPOH..PO+&lcPOH..CDIVISION+&lcPOH..CPURCODE+&lcPOH..CSTYGRADE
=SEEK(&lcPOH..PO+&lcPOH..CDIVISION+&lcPOH..CPURCODE+&lcPOH..CSTYGRADE)
lnRecBrow1 = RECNO()

lcBrowFilds = "CMARK=IIF(RECNO()=lnRecBrow1,'>',' '):H=' ',Style  :R," +;
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
  TITLE lcBrTtl1;
  WHEN lfwPick();
  WINDOW POGENP82 IN WINDOW POGENPL8
SELECT (lnAlias)

*-- End OF lfBrowLines
*:*************************************************************
*: Name      : lfwPick
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Called from the when of the lines browse
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfwPick()
*:*************************************************************
*:
FUNCTION lfwPick

lnRecBrow1 = RECNO(lcLinFile)
SHOW WINDOW (lcBrTtl1) REFRESH SAME
=lfPickWhen()

*-- End OF lfwPick.
*:*************************************************************
*: Name      : lfPickWhen
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Called from the when of the CutPick browse
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfPickWhen()
*:*************************************************************
*:
FUNCTION lfPickWhen

SHOW WINDOW (lcBrTtl2) REFRESH SAME
=lfRefresh('POGENP85')

*-- End OF lfPickWhen
*:*************************************************************
*: Name      : lfwBrDylot
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Called from the dyelot field in the browse of the line file
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfwBrDylot()
*:*************************************************************
FUNCTION lfwBrDylot

IF !EMPTY(PO)
  RETURN(.F.)
ENDIF
=SEEK(STYLE,'STYLE')
RETURN (STYLE.cDye_FLg='Y')

*-- End Of lfwBrDylot
*:*************************************************************
*: Name      : lfvDyelot
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Called from the dyelot field in the browse of the line file
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfvDyelot()
*:*************************************************************
FUNCTION lfvDyelot

IF EMPTY(Dyelot) AND STYLE.cDye_FLg='Y'
  =gfModalGen('INM38132B38018','DIALOG')
ENDIF

*-- End OF lfvDyelot
*:*************************************************************
*: Name      : lfwBrOldVal
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Called from the Qty fields in the browse of the line file
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfwBrOldVal()
*:*************************************************************
FUNCTION lfwBrOldVal

IF !EMPTY(&lcLinFile..PO)) 
  RETURN .F.
ENDIF
IF VAL(RIGHT(SYS(18),1)) <= SCALE.CNT
  lnOldVal = EVALUATE(SYS(18))
ELSE
  RETURN .F.
ENDIF

*-- End of lfwBrOldVal
*:*************************************************************
*: Name      : lfvCTQty
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Called from the Qty fields in the browse of the line file
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfvCTQty()
*:*************************************************************
*:
FUNCTION lfvCTQty
PRIVATE lcQtyFld, lcOrdFld, lnAddedQty

lcQtyFld = lcLinFile+'.'+SYS(18)
lcOrdFld = 'Ord'+RIGHT(lcQtyFld,1)

STORE '/' TO lcPUntSin,lcDUntSin
lcPExSign = gfGetExSin(@lcPUntSin,lcPCurr)
lcDExSign = gfGetExSin(@lcDUntSin,lcDCurr)
STORE 0 TO lnTCost1,lnTCost2,lnTCost3,lnTCost4,lnTCost5 ,;
           lnTFCost1,lnTFCost2,lnTFCost3,lnTFCost4,lnTFCost5

IF EVALUATE(lcQtyFld) < 0
  *-- Message : 42000
  *-- Negative values are not allowed.
  *-- Button  : 40011
  *-- Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  REPLACE &lcQtyFld WITH lnOldVal
ELSE
  SELECT (lcLinFile)
  lnAddedQty = EVALUATE(lcQtyFld) - lnOldVal
  =SEEK(Style,'Style')
  FOR lnCount = 1 TO 5
    lcCount = STR(lnCount,1)
    DO CASE
      CASE lcIType&lcCount = 'P'
        lnCost&lcCount = IIF(lcPCurr=Style.cPriceCur,lnAddedQty*STYLE.nICost&lcCount,0)
        lnECost&lcCount= lnCost&lcCount &lcPExSign lnPRate &lcPUntSin lnUnit1
      CASE INLIST(lcIType&lcCount,'M','D')
        lnCost&lcCount = IIF(lcDCurr=Style.cDutyCur,lnAddedQty*STYLE.nICost&lcCount,0)
        lnECost&lcCount= lnCost&lcCount &lcDExSign lnDRate &lcDUntSin lnUnit2
      OTHERWISE
        lnCost&lcCount = lnAddedQty*STYLE.nICost&lcCount
        lnECost&lcCount= lnCost&lcCount
    ENDCASE
    lnTCost&lcCount  = lnTCost&lcCount  + lnCost&lcCount
    lnTFCost&lcCount = lnTFCost&lcCount + lnECost&lcCount
  ENDFOR
  
  SELECT (lcLinFile)
  REPLACE nCost1  WITH nCost1 + lnCost1  ,;
          nCost2  WITH nCost2 + lnCost2  ,;
          nCost3  WITH nCost3 + lnCost3  ,;
          nCost4  WITH nCost4 + lnCost4  ,;
          nCost5  WITH nCost5 + lnCost5  ,;
          nECost1 WITH nECost1+ lnECost1 ,;
          nECost2 WITH nECost2+ lnECost2 ,;
          nECost3 WITH nECost3+ lnECost3 ,;
          nECost4 WITH nECost4+ lnECost4 ,;
          nECost5 WITH nECost5+ lnECost5 ,;
          TOTQTY WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
   
  SELECT (lcHdrFile)
  REPLACE nStyOrder WITH nStyOrder + lnAddedQty ,;
          NFCOST1   WITH NFCOST1 + lnTFCost1 ,;
          NFCOST2   WITH NFCOST2 + lnTFCost2 ,;
          NFCOST3   WITH NFCOST3 + lnTFCost3 ,;
          NFCOST4   WITH NFCOST4 + lnTFCost4 ,;
          NFCOST5   WITH NFCOST5 + lnTFCost5 ,;
          NICOST1   WITH NICOST1 + lnTCost1  ,;
          NICOST2   WITH NICOST2 + lnTCost2  ,;
          NICOST3   WITH NICOST3 + lnTCost3  ,;
          NICOST4   WITH NICOST4 + lnTCost4  ,;
          NICOST5   WITH NICOST5 + lnTCost5

  *-- Update the BRowse File With the new
  SELECT (lcStyTmp)
  lcPrvOrder = ORDER()
  lnPrvRecNo = RECNO()
  SET ORDER TO TAG (lcStyTmp)
  SELECT (lcLinFile)
  IF SEEK(STYLE,lcStyTmp)
    SELECT (lcStyTmp)
    lcPOOrdFld = 'nPOQty' + RIGHT(lcQtyFld,1)
    REPLACE &lcPOOrdFld WITH EVALUATE(lcQtyFld),;
             nPOTotQty  WITH nPOQty1+nPOQty2+nPOQty3+nPOQty4+nPOQty5+nPOQty6+nPOQty7+nPOQty8
  ENDIF
  SET ORDER TO &lcPrvOrder
  GOTO lnPrvRecNo

  SELECT (lcLinFile)
  =lfPickWhen()
ENDIF
SHOW WINDOW (lcBrTtl1) REFRESH SAME

*-- End OF lfvCTQty
*:*************************************************************
*: Name      : lpSavScr
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Local procedure for the save
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : DO lpSavScr
*:*************************************************************
*:
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
    = lfUpdPO()
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

STORE .F. TO laScrMode
laScrMode[2] = .T.

*-- End OF lpSavScr.
*:*************************************************************
*: Name      : lfUpdPO
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Update the main files in case of generating PO
*:*************************************************************
*: Calls     : gfModalGen(),gfSequence()
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfUpdPO()
*:*************************************************************
FUNCTION lfUpdPO
PRIVATE lcDivision,lcPurCode,lcPO,lcGrade,lcPONumber , llUpdBom , llBomByMe,;
        llGnCTBom
llBomByMe = gfOpenFile(gcDataDir+'Bom',gcDataDir+'Bom','SH')

llGnCTBom = (gfGetMemVar("M_CRTCSTSH")  = "T")
*-- if the system support dyelots
SET ORDER TO TAG POSLN IN POSLN
lcFirstCT  = SPACE(06)
SELECT (lcPOH)
SCAN FOR nStyOrder > 0
  lcPO       = PO
  llUpdBom   = .T.
  lcDivision = CDIVISION
  lcGrade    = cStyGrade
  lcPurCode  = CPURCODE
  lcPONumber = IIF(EMPTY(PO),IIF(EMPTY(cTmpPo),gfSequence('PO','','',CDIVISION),cTmpPo),PO)
  =RLOCK()
  REPLACE cTmpPo WITH lcPONumber
  UNLOCK
  lcFirstCT = IIF(EMPTY(lcFirstCT),lcPONumber,lcFirstCT)
  lcLastCt  = lcPONumber
  SELECT (lcPOLine)
  =SEEK(lcPO+lcDivision+lcPurCode+lcGrade)
  SCAN REST WHILE PO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+DYELOT =;
      lcPO+lcDivision+lcPurCode+lcGrade FOR  TOTQTY > 0
    llUpdBom  =  IIF(llUpdBom,SEEK(SUBSTR(Style,1,lnMajorLen),'BOM'),llUpdBom)
    IF !SEEK('P'+lcPONumber+STYLE+STR(LINENO,6)+TRANCD,'POSLN')
      SCATTER MEMVAR
      STORE 0 TO M.Ord1,M.Ord2,M.Ord3,M.Ord4,M.Ord5,M.Ord6,M.Ord7,M.Ord8,M.TotOrd
      m.PO = lcPONumber
      =SEEK(m.STYLE,'STYLE')
      m.Scale    = STYLE.SCALE
      m.nCost1   = m.nCost1  / m.TotQty
      m.nCost2   = m.nCost2  / m.TotQty
      m.nCost3   = m.nCost3  / m.TotQty
      m.nCost4   = m.nCost4  / m.TotQty
      m.nCost5   = m.nCost5  / m.TotQty
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
      
      *-- Update Stydye File.
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
      
      *-- Update Style File.
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
      *-- Update Posline.
      INSERT INTO POSLN FROM MEMVAR
    ENDIF
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

  *- Update the G.Price and discount after generating the cost sheet in case Automatic.
  = lfUpdGpric ()

ENDSCAN
IF llBomByMe
  USE IN BOM
ENDIF

SELECT (lcPOH)
REPLACE ALL PO WITH cTmpPo
GO TOP

*-- End of lfUpdPO.
*:*************************************************************
*: Name      : lfGenCutBom  
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Generate Cost Sheet Automaticaly.
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfGenCutBom()
*:*************************************************************
*:
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


*-- End OF lfGenCutBom
*:*************************************************************
*: Name      : lfvNote
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Calls the notes for the selected record of the generated P/Os or C/Ts
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfvNote()
*:*************************************************************
*:
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


*-- End OFlfvNote.
*:*************************************************************
*: Name      : lfEditHdr
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Called the screen of P/O or C/T to edit header for the generated records
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfEditHdr()
*:*************************************************************
*:
FUNCTION lfEditHdr

SELECT(lcHdrFile)
IF EOF()
  GOTO TOP
ENDIF
lcCall  = 'P'
lcParam = "'"+lcCall+"','"+&lcPOH..PO+ "'"
DO gpDoProg WITH 'AWRPOSTYLE',.F.,'PO',lcParam

RETURN
*-- End OF lfEditHdr
*:*************************************************************
*: Name      : lfUpdGpric
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 09/17/2003
*: Purpose   : Function to Update the G.Price and discount 
*:           : After generating the cost sheet in case Automatic.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfUpdGpric()
*:*************************************************************
*:
FUNCTION lfUpdGpric
PRIVATE lnAlias 

lnAlias  = SELECT(0)
SELECT (lcPOLine)
=SEEK(lcPO+lcDivision+lcPurCode+lcGrade)
SCAN REST WHILE PO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+DYELOT =;
  lcPO+lcDivision+lcPurCode+lcGrade FOR  TOTQTY > 0
  IF SEEK('P'+lcPONumber+STYLE+STR(LINENO,6)+TRANCD,'POSLN')
    
    *B127460,1 EIH 05/17/2005 m.style <> current style [Begin]
    *=SEEK(m.STYLE,'STYLE')
    =SEEK(STYLE,'STYLE')
    *B127460,1 EIH 05/17/2005 [End]
    
    SELECT POSLN
    REPLACE Gros_Price WITH  Style.Gros_Price,;
            Disc_Pcnt  WITH  Style.Disc_Pcnt
    SELECT (lcPOLine)
  ENDIF
ENDSCAN

SELECT(lnAlias)
*-- End OF lfUpdGpric
*:*************************************************************