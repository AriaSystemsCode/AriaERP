*:************************************************************************
*: Program file  : MFGENCT.PRG
*: Program desc. : Generate C/t and P/O form orders and Ggenerate C/T from plan
*: For screen    : MFGENCT.SCX (0,1,2,3)
*:         System: Aria Advantage Series - Version 2.7
*:         Module: Manufactering Module
*:      Developer: Samah Wilson Kirolloss
*: Passed Parameters   : lcChoice: P.  For generate P/O from orders
*:                                 C.  For generate C/T from orders
*:                                 L.  For generate C/T from Plan
*: Tracking Job Number : E300806
*:************************************************************************
*: Example             : DO MFGENCT
*:************************************************************************
*E300976,1 SWK 08/09/1998 Add dyelots for style P/Os
*B602181,1 WAM 11/10/1998 Hide allocated lines while generating CT from plan
*B602227,1 WAM 11/21/1998 UPdate CT lines cost.
*B602263,1 WAM 11/26/1998 Fix Updating CT lines from plan
*B602428,1 WAM 01/07/1999 Check if primary fabric has inventory
*B602437,1 WAM 01/16/1999 Fix allocate/deallocate orders
*B602467,1 WAM 01/26/1999 Allocate order open quantity instead of ordered quantity
*B602471,1 WAM 01/26/1999 Force allocate all order lines group for one dyelot
*B602490,1 WAM 01/26/1999 Readjust buttons after terminate the program
*E301089,1 WAM 01/26/1999 Consider decimal point in fabric onhand
*B602497,1 WAM 01/31/1999 Do not process order Contracts
*B602503,1 WAM 02/05/1999 Use order open quantity instead of ordered quantity
*B602503,1                and Adjust report fields.
*B602510,1 WAM 02/10/1999 update warehouse when the system not use multi warehouse
*B602557,1 WAM 02/19/1999 Disable Scope button after generate tickets
*B602549,1 WAM 02/20/1999 Fix updating ticket lines
*E301077,57 WAM 02/19/1999 Inhance openning files to speed up transaction
*B802056,1 WAM 03/22/1999 Add style primary fabric to selection grid
*E301182,1 WAM 03/23/1999 Update CT/PO line number in the CUTPICK file.
*B801961,1 HS  03/23/1999 Declare some new arrays that will be used in the
*B801961,1                filter expression that will be returned by the
*B801961,1                option grid in the case of "In list" operator.
*B802224,1 WAM 05/19/1999 Fix computing fabric requirements.
*E301268,1 HDM 06/21/1999 filter vendor browse by sup. type
*B802376   WAB 07/08/1999 a warning message should popup when close from
*B802376                  window control bottom
*B802447,1 ABD 07/22/1999 Delete The Old Record In The Temp File.
*B603078,1 Reham On 08/04/1999
*B603078,1 Browse vendor that suplies styles & contractors.
*E301289,1 WAB 08/17/1999 Adding selling price & gross margin on PO lines & CT lines
*B802602,1 RAMY 09/14/1999 fix the picture of the "Primary Fabric" option
*                          to prevent the browse from being displayed
*                          if the user typed valid Fabric code directly.
*B802601,1 RAMY 09/15/1999 Change the operator of the "Style Major" option
*                          to In-Range.
*                          Note: we made some changes in SYREPUVR file.
*B802606,1 RAMY 09/16/1999 When allocate sales orders Based on fabric = yes
*                          sort the popup of onhnad qty and dyelot by
*                          onhand.
*B802607,1 RAMY 09/21/1999 fixing some problems the brows in allocated order
*                          screen and added new option to report to print
*                          summary or detail
*B802673,1 RAMY 10/26/1999 Avoid run the vendor browse in the Default Vendor when gen PO from
*                          Sales order evenif enterd valid value
*B802331,1 AMM 12/16/1999 Fix the bug of not updating the CUTPICK file in some cases
*B802867,1 AMM 12/19/1999 Fix the bug of wrong filter on color when generate from plan
*B603516,1 SSH 14/02/2000 Fix the bug of not generate C/T or PO Automaticaly
*B603516,1 SSH            even if the system is set to generate manualy.
*B603348,1 ABD 03/20/2000 Update the picture of filed Price & Total Cost 
*B603348,1 ABD            In the screen mfgenc25.scx in screen set mfgenct2.scx.
*B602781,1 SSH 09/04/00   Fix the bug Variable lcSeg not found
*B603521,1 SSH 10/04/00   Fix the bug Alias '' Not Found.
*B603557,1 SSH 01/04/00   Fix the bug of alias not found if Press notespad.
*B603559,1 SSH 01/04/00   Fix the bug of alias not found if cancel session.
*E301426,1 SSH 11/06/2000 Generate C/T,PO Cost Sheet Automaticalt if the.
*E301426,1 SSH            setup is generate C/t Automaticaly.
*B803635,1 HBG 18/09/2000 Optomizing the selection of the orders
*B603794,1 HBG 19/09/2000 Allow user to select another order or group of 
*B603794,1                orders after saving without back to OG 
*B803686,1 RAMY 09/24/2000 Fix the bug of error when printing any report 
*B803686,1                 because it was using gfDispRep.
*                          Note-> There is a new program added in the reports dir. 
*                                 with the name 'MFGENCT'
*B604003,1 KHM 11/01/2000 Fix the bug of generating the PO number automatically
*B604003,1                even if the setup of generate PO number manually 
*B604003,1                was set to "Y".
*B604011,1 HBG 11/05/2000 Fix bug of Variabel "Daylot" not found when generating C\T 
*B604011,1                from plan
*B604029,1 HBG 11/26/2000 Allowe accessing P/O or C/T and Notes buttons after saving
*B604515,1 KHM 06/05/2001 Fix the bug of not adding records in the cutpick
*B604515,1                file when generate PO # or CT # setting is set 
*B604515,1                to manually.
*E301746,1 KHM 10/14/2001 Store the base currency to the duty.
*B605517,1 KHM 02/13/2002 Fix the bug of when allocating the cursor goes 
*B605517,1                to the end of the file
*C200361,1 AMH 06/24/2002 trigger for custom RON20 to ignore picked ordline's.
*B606205,1 KHM 07/03/2002 Fix the bug of displaying all the locations even if they were not
*B606205,1                marked to be a style finished goods inventory.
*C200387,1 AMH 07/21/2002 Trigger for customer RON20 to ignore fully picked or allocated
*C200387,1                ordlines.
*:**********************************************************************************
PARAMETERS lcChoice
************************************************************************************
lcChoice   = IIF(TYPE('lcChoice')='C',lcChoice,'P')
************************************************************************************
DIMENSION laColors[1,2],laPercet[8],laStySeg[1],laWareType[2],laDefProc[10]

*B801961,1 Add this line to declare some new arrays that will be used in the
*          filter expression that will be returned by the option grid in
*          the case of "In list" operator [Begin]
DIMENSION laHdFlt[1] , laFxFlt[1] , laVrFlt[1]
*B801961,1 Add this line to declare some new arrays that will be used [End]

*B803635,1 HBG variable to hold the first order in the selected 
*B803635,1     criteria [Begin]
STORE "" TO lcFOrder,lcTOrder
*B803635,1 [End]

*B603794,1 HBG Flag to allow user to select another order or group of orders
*B603794,1     after saving without back to OG [Begin]
*B604029,1 HBG 11/26/2000 Add flag 'llSelMode' to check if P/O or C/T is saved 
*B604029,1                and we'll move to select mode [Begin]
*llRetToSel = .F.
STORE .F. TO llRetToSel , llSelMode 
*B604029,1 [End]
*B603794,1 [End]

*B802601,1 RAMY [start]
*B802601,1 Add this variable to hold the old index for the style file when
*B802601,1 enter the in range brows
lcOldNdx  = ''
*B802601,1 RAMY [end]
lcTktTtl  = IIF(lcChoice='P',"Purchase Orders","Cutting Tickets")
lcSrcTtl  = IIF(lcChoice='L',"Styles","Order Lines")


*B602181,1 Hide allocated lines while generating CT from plan
lcBrTtl1  = IIF(lcChoice='P','Details of Purchase Order','Details of Cutting Ticket')

lcBrTtl2  = IIF(lcChoice='L',lcBrTtl1,'Quantity Allocated from Order Lines')

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
*B603516,1 SSH 14/02/2000 Variable to hold old no.
lcOldCtkNo = ''
*B603516,1 SSH (END)
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

STORE ''  TO lnOldVal,lcModal,lcSort,lcRPSort1,lcRPSort2,lcRPSort3,;
  lcSZ1,lcSZ2,lcSZ3,lcSZ4,lcSZ5,lcSZ6,lcSZ7,lcSZ8,lcScalDesc,;
  lcVenName,lcCont,lcPhone,lcHdrFile,lcLinFile

STORE '' TO  lcPOH,lcPOLine,lcOrdLine,lcCutPick,lcCuttktL,lcCuttktH,;
  lcTmpOrd,lcPrintAll,lcOrdGroup,lcFabrics,lcStyUnAll,lcStyTmp

STORE '' TO  lcWinCh1,lcWinC11,lcWinC12,lcWinC13,lcWinC14,lcWinC15

STORE '' TO  laColors,lcVendor,lcWareCode,lcPCurr,lcDCurr,cMajorPic,lnMajorLen,;
  lcFirstCT,lcLastCt,lcIType1,lcIType2,lcIType3,lcIType4,;
  lcIType5,lcSelFile,lcProgID,lcProgTxt

STORE .F. TO llRPUnAWip,llRpBOPFab,llAllocate
STORE '*' TO lcSign1,lcSign2

DIMENSION laSeason[1], laTerm[1], laDiv[1], laCodInfo [2,10] ,laShip[1], laGroup[1]
STORE "" TO laSeason,laTerm, laDiv, laCodInfo,laShip,laGroup
*B802376   WAB declare array for holding window Title
DECLARE laFromBrow[2]
laFromBrow[1] = lcTktTtl
laFromBrow[2] = lcSrcTtl

lcGenPrompt = "Gene\<rate C/T"

*-- To call the lpshow when starting th program
llNoShow  = .F.

*E301289,1 WAB - initialize variables
*E301289,1 WAB - Get Setting Variable ( Display selling Price & gross Margin )
*E301289,1 WAB - IF the lcChoice =  P  the prog. For generate P/O from orders
*E301289,1 WAB - Or the lcChoice =  C  the prog. For generate C/T from orders
*E301289,1 WAB - START
*------- llDispPric  -> hold setting variable to display selling price & gross margin
*------- lnTotQty    -> Hold TOTAl Qty Of one Order
*------- lnSelPrice  -> Hold Average Selling Price
*------- lnAllQty    -> Hold All Qty For All Orders
*------- lnRotSub    -> Hold Cost Price Or Selling price Depend on llStyMark
*------- lnGrosMrgn  -> Hold Gross Margin

llDispPric  = .F.
llStyMark   = .F.
IF lcChoice $ 'PC'
  llDispPric = IIF(lcChoice='P',gfGetMemVar('M_PoDspPrc'),gfGetMemVar('M_MfDspPrc'))
  llStyMark  = gfGetMemVar('M_stymark')   ='T'
ENDIF
IF llDispPric
  STORE 0 TO lnTotQty,lnSelPrice,lnAllQty,lnRotSub,lnGrosMrgn
ENDIF
*E301289,1 WAB - END

*--Get the values from the memory variable
llWareHous = ALLTRIM(gfGetMemVar('M_WareHouse'))= 'Y'
*B802331,1 AMM Comment out !!
*llWareHous = .F.
*B802331,1 AMM end
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

*B603516,1 SSH 14/02/2000 Get the system setting for generate no.

*B604003,1 KHM 11/01/2000 (Begin) Checing the active module in order to get
*B604003,1 KHM            the appropriate setting of manual number for PO & CT.
*llGManualy = (gfGetMemVar('M_GENCTNUM') = "Y")
llGManualy = IIF(gcAct_Appl = 'MF',(gfGetMemVar('M_GENCTNUM') = "Y"),;
                                   (gfGetMemVar('M_GENSTORN') = "Y"))

*B604003,1 KHM 11/01/2000 (End)

*B603516,1 SSH 14/02/2000(END)

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
DO CASE
CASE lcChoice = 'P'
  lcProgID  = PADR("POFROMORD",10)
  lcProgTxt = 'Generate PO from orders'
CASE lcChoice = 'C'
  lcProgID = PADR("CTFROMORD", 10)
  lcProgTxt = 'Generate CT from orders'
CASE lcChoice = 'L'
  lcProgID = PADR("CTFROMPL", 10)
  lcProgTxt = 'Generate CT from plan'
ENDCASE

*-- Variables that need to be saved and restored when detecting an
*-- uncomplete program session.

DIMENSION laVars[26]
laVars[01] = "lcScrMode"
laVars[02] = "lcChoice"
laVars[03] = "llFirst"
laVars[04] = "lcLinFile"
laVars[05] = "lcHdrFile"
laVars[06] = "lcSelFile"
laVars[07] = "lnShip"
laVars[08] = "lnTerm"
laVars[09] = "lcPCurr"
laVars[10] = "lcDCurr"
laVars[11] = "lnPRate"
laVars[12] = "lnDRate"
laVars[13] = "lcVendor"
laVars[14] = "laPercet[1]"
laVars[15] = "laPercet[2]"
laVars[16] = "laPercet[3]"
laVars[17] = "laPercet[4]"
laVars[18] = "laPercet[5]"
laVars[19] = "laPercet[6]"
laVars[20] = "laPercet[7]"
laVars[21] = "laPercet[8]"
laVars[22] = "lcWareCode"
laVars[23] = "lnUnCmSeRc"
laVars[24] = "llRPUnAWip"
laVars[25] = "llRpBOPFab"
laVars[26] = "llAllocate"

*--Add and icon for the scope in the control pannel
DECLARE laPanelObj[1,3]
STORE '' TO laPanelObj
laPanelObj[1,1] = 'pbScope'
laPanelObj[1,2] = gcBmpHome+'SCOPE.BMP'

*B602557,1 Disable Scope button after generate tickets
*laPanelObj[1,3] = [VALID lfvScope()]
laPanelObj[1,3] = [DISABLE VALID lfvScope()]
*B602557,1 (End)

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
*E301426,1 SSH 11/06/2000 Variable to indicate if we are comming from Cost Sheet.
llFromBom = .F.
*E301426,1 SSH [END]

*C200361,1 AMH Add variable to new option for RON20 [Start]
llRpIgPick = .F.
*C200361,1 AMH [End]

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
  *B602781,1 SSH 09/04/00 *** Begin *** Fix the bug Variable lcSeg not found
  IF !lfCheckMaj()
    *--Item structure not found, Cannot Proceed.
    =gfModalGen('QRM42080B42001','DIALOG','Item structure not found.')
    glQuitting = .T.
    RETURN
  ENDIF
  *B602781,1 SSH 09/04/00 *** End ***
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

  IF lcChoice $ 'LC'
    lcCuttktH = gfTempName()  && temprory cut ticket header
    lcCuttktL = gfTempName()  && temprory cut ticket line
  ELSE
    lcPOH    = gfTempName()  && temprory PosHdr
    lcPOLine = gfTempName()  && temprory PosLine
  ENDIF

  IF lcChoice <> 'L'
    lcCutPick   = gfTempName()  && temporary pick ticket
    lcOrdLine   = gfTempName()  && temporary order line
    lcTmpOrd    = gfTempName()  && Temporary allocated/unallocated order lines
    lcPrintAll  = gfTempName()  && Temporary allocated/unallocated order lines
    lcOrdGroup  = gfTempName()  && Temporary Order Groups File Name
    lcFabrics   = gfTempName()  && Temporary Fabric/Color/Dyelot File Name
    lcStyUnAll  = gfTempName()
  ELSE
    lcStyTmp = gfTempName()   && temprory Style
  ENDIF
  *E301077,57 Inhance openning files to speed up transaction
  *=gfwCodePop(@laCodInfo, "CTERMCODE", "D")
  *=gfwCodePop(@laCodInfo, "SHIPVIA",   "D")
  *E301077,57 (End)
  lcLinFile = IIF(lcChoice = 'P',lcPOLine,lcCUTTKTL)  &&&& Variable to hold the header file
  lcHdrFile = IIF(lcChoice = 'P',lcPOH,lcCuttktH)
  lcSession = gfsequence('CSESSION')
*E301426,1 SSH If comming back from the cutting ticket bill of material program
               *-- then switch the screen mode to the select mode.
ELSE
  IF laScrMode[4] AND llFromBom
    laScrMode    = .F.
    laScrMode[1] = .T.
    llFromBom = .F.
  ENDIF
*E301426,1 SSH [END]
ENDIF

PUSH KEY
*E301077,57 Inhance openning files to speed up transaction
*llDye_Rel = llFDyelot AND ;
gfOpenFile(gcDataDir+'DYE_REL',gcDataDir+'DYE_REL','SH')
*llFabric  = gfOpenFile(gcDataDir+'FABRIC',gcDataDir+'FABRIC','SH')
*llFabDye  = gfOpenFile(gcDataDir+'FABDYE',gcDataDir+'FABDYE','SH')
*llBOM     = gfOpenFile(gcDataDir+'BOM',gcDataDir+'BOM','SH')
*E301077,57 (End)

llReBrowse = .T.
=lfClearKey()
STORE '' TO lcBrowseTl
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

*-- Call of the main screen
DO (gcScrDir+'MFGENCT.SPR')

RELEASE WINDOWS (lcSrcTtl),(lcTktTtl),(lcTktTtl),(lcSrcTtl)
POP KEY

*E301077,57 Inhance openning files to speed up transaction
*IF llDye_Rel
*  USE IN 'DYE_REL'
*ENDIF
*IF llFabric
* USE IN 'FABRIC'
*ENDIF
*IF llFabDye
*  USE IN 'FABDYE'
*ENDIF
*IF llBOM
*  USE IN 'BOM'
*ENDIF
*E301077,57 (End)

*-- Erase all the created temporary files, close all the conditionally
*-- opened aliases.
IF glQuitting
  =lfUpdUnCmS("I", SPACE(0))
  IF lcChoice $ 'LC'
    IF USED(lcCuttktH)
      USE IN (lcCuttktH)
      ERASE (gcWorkDir+lcCuttktH+".DBF")
      ERASE (gcWorkDir+lcCuttktH+".CDX")
      ERASE (gcWorkDir+lcCuttktH+".FPT")
    ENDIF
    IF USED(lcCuttktL)
      USE IN (lcCuttktL)
      ERASE (gcWorkDir+lcCuttktL+".DBF")
      ERASE (gcWorkDir+lcCuttktL+".CDX")
    ENDIF
  ELSE
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
  ENDIF
  IF lcChoice <> 'L'
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
  ELSE
    IF USED(lcStyTmp)
      USE IN (lcStyTmp)
      ERASE (gcWorkDir+lcStyTmp+".DBF")
      ERASE (gcWorkDir+lcStyTmp+".CDX")
    ENDIF
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
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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
  *glFromBrow = .F.
ENDIF

RETURN .F.

*!*************************************************************
*! Name      : lfBrowCut
*! Developer : Samah Wilson
*! Date      : 03/22/1998
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
IF lcChoice $ 'CL'
*B603516,1 SSH 14/02/2000 Enable trans. no if generate manualy.
  *lcBrowStr = "CMARK=IIF(RECNO()=lnCutRecNo,'>',' '):H=' ',CutTKt :R,STYLE:R,"+IIF(llWareHous,"CWARECODE:P='@!':H='Location':V=lfvWareH():W=lfwOldVals(),","")+"Pattern,"+;
    "CDIVISION:R:H='Division',"+;
    "Entered :V=(!EMPTY(Entered) AND Entered <= Complete):F:E='Entered date must be less than or equal completion date',"+;
    "Complete:V=(!EMPTY(Complete) AND Complete>=Entered):F:E='Completion date must be greater than or equal Entered date',"+;
    "pcs_bud:H='Budget':R,totord:H='Allocated':R,totcost=nest_cost1+nest_cost2+nest_cost3+nest_cost4+nest_cost5:H='Tot. Cost',"+;
    "nest_cost1:H=lcMSLbl1:R,nest_cost2:H=lcMSLbl2:R,nest_cost3:H=lcMSLbl3:R,nest_cost4:H=lcMSLbl4:R,nest_cost5:R:H=lcMSLbl5"

  lcBrowStr = "CMARK=IIF(RECNO()=lnCutRecNo,'>',' '):H=' ',CutTKt :P='999999' :V=lfvBroWCt() :W=lfwOldCtkt(),;
    STYLE:R,"+IIF(llWareHous,"CWARECODE:P='@!':H='Location':V=lfvWareH():W=lfwOldVals(),","")+"Pattern,"+;
    "CDIVISION:R:H='Division',"+;
    "Entered :V=(!EMPTY(Entered) AND Entered <= Complete):F:E='Entered date must be less than or equal completion date',"+;
    "Complete:V=(!EMPTY(Complete) AND Complete>=Entered):F:E='Completion date must be greater than or equal Entered date',"+;
    "pcs_bud:H='Budget':R,totord:H='Allocated':R,totcost=nest_cost1+nest_cost2+nest_cost3+nest_cost4+nest_cost5:H='Tot. Cost',"+;
    "nest_cost1:H=lcMSLbl1:R,nest_cost2:H=lcMSLbl2:R,nest_cost3:H=lcMSLbl3:R,nest_cost4:H=lcMSLbl4:R,nest_cost5:R:H=lcMSLbl5"
*B603516,1 SSH 14/02/2000  (End)


ELSE
*B603516,1 SSH 14/02/2000 Enable trans. no if generate manualy.

*  lcBrowStr = "CMARK=IIF(RECNO()=lnCutRecNo,'>',' '):H=' ',PO :R :H='PO#',VENDOR:R,CDIVISION:R:H='Division',"+IIF(llWareHous,"CWARECODE:P='@!':H='ShipTo.':V=lfvWareH():W=lfwOldVals(),","")+;
    "ShipVia:R,CTERMCODE:R:H='Terms',Entered:V=(!EMPTY(Entered) AND Entered <= Complete):F:E='Entered date must be less than or equal completion date',"+;
    "Complete:V=(!EMPTY(Complete) AND Complete>=Entered):F:E='Completion date must be greater than or equal Entered date',"+;
    "nStyOrder:R :H='Order',totord:H='Allocated':R,totcost=nFcost1+nFcost2+nFcost3+nFcost4+nFcost5:H='Tot. Cost',"+;
    "nFcost1:H=lcISLbl1:R,nFcost2:H=lcISLbl2:R,nFcost3:H=lcISLbl3:R,nFcost4:H=lcISLbl4:R,nFcost5:R:H=lcISLbl5"

  lcBrowStr = "CMARK=IIF(RECNO()=lnCutRecNo,'>',' '):H=' ',PO :H='PO#' :P='999999' :V=lfvBroWCt() :W=lfwOldCtkt(),;
    VENDOR:R,CDIVISION:R:H='Division',"+IIF(llWareHous,"CWARECODE:P='@!':H='ShipTo.':V=lfvWareH():W=lfwOldVals(),","")+;
    "ShipVia:R,CTERMCODE:R:H='Terms',Entered:V=(!EMPTY(Entered) AND Entered <= Complete):F:E='Entered date must be less than or equal completion date',"+;
    "Complete:V=(!EMPTY(Complete) AND Complete>=Entered):F:E='Completion date must be greater than or equal Entered date',"+;
    "nStyOrder:R :H='Order',totord:H='Allocated':R,totcost=nFcost1+nFcost2+nFcost3+nFcost4+nFcost5:H='Tot. Cost',"+;
    "nFcost1:H=lcISLbl1:R,nFcost2:H=lcISLbl2:R,nFcost3:H=lcISLbl3:R,nFcost4:H=lcISLbl4:R,nFcost5:R:H=lcISLbl5"
*B603516,1 SSH 14/02/2000 (END)
ENDIF
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
*! Developer : Samah Wilson
*! Date      : 03/22/1998
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
IF lcChoice = 'P'
  SHOW WINDOW (lcTktTtl) REFRESH SAME
ELSE
  SHOW WINDOW (lcTktTtl) REFRESH SAME
ENDIF

*!*************************************************************
*! Name      : lfBrowOrd
*! Developer : Samah Wilson
*! Date      : 03/22/1998
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
*! Developer : Samah Wilson
*! Date      : 03/22/1998
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
*! Developer : Samah Wilson
*! Date      : 03/22/1998
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
IF lcChoice $ 'CL'
  SELECT (lcCutTktL)
  SET KEY TO &lcCutTKtH..CUTTKT+&lcCutTKtH..CDIVISION+&lcCutTKtH..SEASON+SUBSTR(&lcCutTKtH..STYLE,1,lnMajorLen)
  =SEEK(&lcCutTKtH..CUTTKT+&lcCutTKtH..CDIVISION+&lcCutTKtH..SEASON+SUBSTR(&lcCutTKtH..STYLE,1,lnMajorLen))
  lnRecBrow1 = RECNO()
  *B603348,1 ABD Update the picture of filed Total Cost . [Begin]
  *lcBrowFilds = "CMARK=IIF(RECNO()=lnRecBrow1,'>',' '):H=' ',Style:R," +;
    IIF(llDyelot,"Dyelot :H='Dyelot':W=lfwBrDylot():V=lfvDyelot():P='@!':F,"," ")+;
    "Qty1:H='Cut 1':W=lfwBrOldVal():V=IIF(lcChoice='C',lfvCTQty(),lfvCTPlan()),"+;
    "Qty2:H='Cut 2':W=lfwBrOldVal():V=IIF(lcChoice='C',lfvCTQty(),lfvCTPlan()),"+;
    "Qty3:H='Cut 3':W=lfwBrOldVal():V=IIF(lcChoice='C',lfvCTQty(),lfvCTPlan()),"
  *lcBrowFilds = lcBrowFilds +;
    "Qty4:H='Cut 4':W=lfwBrOldVal():V=IIF(lcChoice='C',lfvCTQty(),lfvCTPlan()),"+;
    "Qty5:H='Cut 5':W=lfwBrOldVal():V=IIF(lcChoice='C',lfvCTQty(),lfvCTPlan()),"+;
    "Qty6:H='Cut 6':W=lfwBrOldVal():V=IIF(lcChoice='C',lfvCTQty(),lfvCTPlan()),"+;
    "Qty7:H='Cut 7':W=lfwBrOldVal():V=IIF(lcChoice='C',lfvCTQty(),lfvCTPlan()),"+;
    "Qty8:H='Cut 8':W=lfwBrOldVal():V=IIF(lcChoice='C',lfvCTQty(),lfvCTPlan()),"+;
    "TotQty :H='Tot.Cut':R,"+;
    "nCost1 :H=lcMSLbl1:R,nCost2 :H=lcMSLbl2:R,"+;
    "nCost3 :H=lcMSLbl3:R,nCost4 :H=lcMSLbl4:R,"+;
    "nCost5 :H=lcMSLbl5:R,nTtCst = nCost1+nCost2+nCost3+nCost4+nCost5:H='Tot.Cost':P='99999999.99':R"
    
  lcBrowFilds = "CMARK=IIF(RECNO()=lnRecBrow1,'>',' '):H=' ',Style:R," +;
    IIF(llDyelot,"Dyelot :H='Dyelot':W=lfwBrDylot():V=lfvDyelot():P='@!':F,"," ")+;
    "Qty1:H='Cut 1':W=lfwBrOldVal():V=IIF(lcChoice='C',lfvCTQty(),lfvCTPlan()),"+;
    "Qty2:H='Cut 2':W=lfwBrOldVal():V=IIF(lcChoice='C',lfvCTQty(),lfvCTPlan()),"+;
    "Qty3:H='Cut 3':W=lfwBrOldVal():V=IIF(lcChoice='C',lfvCTQty(),lfvCTPlan()),"
  lcBrowFilds = lcBrowFilds +;
    "Qty4:H='Cut 4':W=lfwBrOldVal():V=IIF(lcChoice='C',lfvCTQty(),lfvCTPlan()),"+;
    "Qty5:H='Cut 5':W=lfwBrOldVal():V=IIF(lcChoice='C',lfvCTQty(),lfvCTPlan()),"+;
    "Qty6:H='Cut 6':W=lfwBrOldVal():V=IIF(lcChoice='C',lfvCTQty(),lfvCTPlan()),"+;
    "Qty7:H='Cut 7':W=lfwBrOldVal():V=IIF(lcChoice='C',lfvCTQty(),lfvCTPlan()),"+;
    "Qty8:H='Cut 8':W=lfwBrOldVal():V=IIF(lcChoice='C',lfvCTQty(),lfvCTPlan()),"+;
    "TotQty :H='Tot.Cut':R,"+;
    "nCost1 :H=lcMSLbl1:R,nCost2 :H=lcMSLbl2:R,"+;
    "nCost3 :H=lcMSLbl3:R,nCost4 :H=lcMSLbl4:R,"+;
    "nCost5 :H=lcMSLbl5:R,nTtCst = nCost1+nCost2+nCost3+nCost4+nCost5:H='Tot.Cost':P='999999999999.99':R"
  *B603348,1 ABD [End]
ELSE
  SELECT (lcPOLine)
  SET KEY TO &lcPOH..PO+&lcPOH..CDIVISION+&lcPOH..CPURCODE+&lcPOH..CSTYGRADE
  =SEEK(&lcPOH..PO+&lcPOH..CDIVISION+&lcPOH..CPURCODE+&lcPOH..CSTYGRADE)
  lnRecBrow1 = RECNO()
  *E300976,1 SWK 08/09/1998 Add dyelots for style P/Os
  *B603348,1 ABD Update the picture of filed Total Cost . [Begin]
  *lcBrowFilds = "CMARK=IIF(RECNO()=lnRecBrow1,'>',' '):H=' ',Style  :R," +;
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
    "nTtCst = nCost1+nCost2+nCost3+nCost4+nCost5:H='Tot.Cost':P='99999999.99':R"

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
  *B603348,1 ABD [End]

  *lcBrowFilds = "Style  :R," +;
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
  "nTtCst = nCost1+nCost2+nCost3+nCost4+nCost5:H='Tot.Cost':P='99999999.99':R"
*E300976,1(End)

ENDIF
*E301289,1 WAB - Add Selling Price & Gross Margin to browse fileds in case of
*E301289,1 WAB - ( Purchase Order ) and the llDispPric  is true
*E301289,1 WAB - START
IF llDispPric
  lcBrowFilds =  lcBrowFilds + [,nSelPrice :H='Selling Price' :R:P='999999999.99',;
    nGrosMrgn :H='Gross Margin' :R:P='9999.99']
ENDIF
*E301289,1 WAB - END

GOTO TOP
BROWSE FIELDS &lcBrowFilds;
  NOAPPEND ;
  NOWAIT   ;
  NOMENU   ;
  NOCLEAR  ;
  TITLE lcBrTtl1;
  WHEN IIF(lcChoice $ 'PC',lfwPick(),.T.);
  WINDOW MFGENC22 IN WINDOW MFGENCT2
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfBrOrdLin
*! Developer : Samah Wilson
*! Date      : 03/22/1998
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
IF lcChoice='P'
  lcBrowF2 =  ",nCost1= TotQty * STYLE.nICost1:H=lcISLbl1 :R,"+;
    "nCost2 = TotQty * STYLE.nICost2 :H=lcISLbl2 :R,"+;
    "nCost3 = TotQty * STYLE.nICost3 :H=lcISLbl3 :R,"+;
    "nCost4 = TotQty * STYLE.nICost4 :H=lcISLbl4 :R,"+;
    "nCost5 = TotQty * STYLE.nICost5 :H=lcISLbl5 :R,"+;
    "nTtCst = TotQty *(STYLE.nICost1 + STYLE.nICost2 + ;
    STYLE.nICost3 + STYLE.nICost4 + STYLE.nICost5) :H='Total Cost':R"
ELSE
  lcBrowF2 =  ",nCost1= TotQty * STYLE.nMCost1:H=lcMSLbl1 :R,"+;
    "nCost2 = TotQty * STYLE.nMCost2:H=lcMSLbl2 :R,"+;
    "nCost3 = TotQty * STYLE.nMCost3:H=lcMSLbl3 :R,"+;
    "nCost4 = TotQty * STYLE.nMCost4:H=lcMSLbl4 :R,"+;
    "nCost5 = TotQty * STYLE.nMCost5:H=lcMSLbl5 :R,"+;
    "nTtCst = TotQty *(STYLE.nMCost1 + STYLE.nMCost2 + ;
    STYLE.nMCost3 + STYLE.nMCost4 + STYLE.nMCost5);
    :H='Total Cost':R"
ENDIF
lcBrowFilds = lcBrowF1+lcBrowF2

BROWSE FIELDS &lcBrowFilds;
  NOAPPEND ;
  NOWAIT   ;
  NOMENU   ;
  NOCLEAR  ;
  NODELETE ;
  WHEN lfPickWhen();
  TITLE lcBrTtl2;
  WINDOW MFGENC23 IN WINDOW MFGENCT2
SELECT (lnAlias)

*!*************************************************************
*! Name      : lpShow
*! Developer : Samah Wilson
*! Date      : 03/22/1998
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
  =IIF(lcChoice='L',lfStyBrow(),lfBrowOrd())
  =lfBrowCut()
  llReBrowse = .F.
ENDIF
SHOW GET ibToBrow ENABLE
SHOW GET ibToHdr  ENABLE
SHOW GET pbSlct   DISABLE
SHOW GET pbEdt    DISABLE
SHOW GET pbDlt    DISABLE

*B603794,1 HBG IF user generate PO and save , Back to Edit mode to allow 
*B603794,1     him to select again[Begin]
*B604029,1 HBG 11/26/2000  Comment this line and move it in the case of select mode [Begin]
*IF llRetToSel
 * STORE .F. TO laScrMode
 * laScrMode[3] = .T.
*ENDIF
*B604029,1 [End]
*B603794,1 [End]

DO CASE
CASE laScrMode[1]
  IF !llChecked
    IF lfChkUnCmS()
      RETURN
    ENDIF
  ELSE
    lcScrMode = "S"
  ENDIF
  
  *B604029,1 HBG 11/26/2000  Allow user to select anoter order or group [Begin]
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
  *B604029,1 [End]
  
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
    SHOW GET pbHeadr,1 PROMPT IIF(lcChoice='P',"\<P/O","\<C/T") ENABLE
  ENDIF
  *B602557,1 Disable Scope button after generate tickets
  *SHOW GET pbScope ENABLE
  GO TOP IN  (lcHdrFile)
  IF EOF(lcHdrFile)
    SHOW GET pbScope ENABLE
  ELSE
    SHOW GET pbScope DISABLE
  ENDIF
  *B602557,1 (End)

*B604029,1 HBG 11/26/2000 Add case of view mode to enableing the open button 
*B604029,1                and P/O or C/T and Notes buttons [End]
CASE laScrMode[2]
  llSelMode = .T.
  SHOW GET pbSlct ENABLE
  IF !EMPTY(lcFirstCT)
    SHOW GET pbNote,1  PROMPT '\<Notes' ENABLE
    SHOW GET pbHeadr,1 PROMPT IIF(lcChoice='P',"\<P/O","\<C/T") ENABLE
  ENDIF
*B604029,1 [End]

CASE laScrMode[3]
  lcScrMode = "E"
  llCUpDate = .T.
  SHOW GET pbSav DISABLE
  IF (!llRpBOPFab OR llAllocate) AND (!EMPTY(lcVendor) OR lcChoice <> 'P')
    SHOW GET pbGenerate,1 PROMPT IIF(lcChoice = 'P',"\<Generate P/O","\<Generate C/T") ENABLE
  ELSE
    SHOW GET pbGenerate,1 PROMPT IIF(lcChoice = 'P',"\<Generate P/O","\<Generate C/T") DISABLE
  ENDIF
  SHOW GET pbDetail DISABLE
  IF llAllocate
    SHOW GET pbSelect  DISABLE
    SHOW GET pbSelAll  DISABLE
    SHOW GET pbSelNone DISABLE
    SHOW GET pbInvert  DISABLE
    *B602490,1 Readjust buttons after terminate the program
    *SHOW GET pbDefault DISABLE
    SHOW GET pbDefault ENABLE
    *B602490,1 (End)

    SHOW GET pbScope   DISABLE
    SHOW GET pbNote,1  PROMPT 'Clear Allo\<cation' ENABLE
    *B602467,1 Change Hot key
    *SHOW GET pbHeadr,1 PROMPT 'Allo\<cated Orders' ENABLE
    SHOW GET pbHeadr,1 PROMPT 'Allocated \<Orders' ENABLE
  ELSE
    =lfvSelect('')
    IF llRpBOPFab
      *B602467,1 Change Hot key
      *SHOW GET pbHeadr,1 PROMPT 'Allo\<cated Orders' DISABLE
      SHOW GET pbHeadr,1 PROMPT 'Allocated \<Orders' DISABLE
    ELSE
      SHOW GET pbHeadr,1 PROMPT IIF(lcChoice='P','\<P/O','\<C/T') DISABLE
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
  SHOW GET pbDefault DISABLE
  SHOW GET pbSav     ENABLE
  SHOW GET pbGenerate,1 PROMPT IIF(lcChoice='P',"\<Clear P/O","\<Clear C/T")
  SHOW GET pbNote,1     PROMPT IIF(llRpBOPFab,IIF(llAllocate,'Clear Allo\<cation','Allo\<cate'),'\<Notes') DISABLE
  *B602467,1 Change Hot key
  *SHOW GET pbHeadr,1    PROMPT IIF(llRpBOPFab,'Allo\<cated Orders',IIF(lcChoice='P','\<P/O','\<C/T')) DISABLE
  SHOW GET pbHeadr,1    PROMPT IIF(llRpBOPFab,'Allocated \<Orders',IIF(lcChoice='P','\<P/O','\<C/T')) DISABLE
  SHOW GET pbCls,1 PROMPT gcBmpHome+"Cancel.BMP"
  =lfUpdVars()
ENDCASE

*!*************************************************************
*! Name      : lfvScope
*! Developer : Samah Wilson
*! Date      : 03/22/1998
*! Purpose   : Function called for the selection criteria (Option Grid)
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvScope()
*!*************************************************************
FUNCTION lfvScope

*B603794,1 HBG Return the flag to false befor calling the OG [Begin]
llRetToSel = .F.
*B603794,1 [End]

*-- Call differrent option grid in case of generate from plan
lcExpr    = gfOpGrid(IIF(lcChoice='L','MFGENP','MFGENC'),.T.)
lcSelFile = IIF(lcChoice='L',lcStyTmp,lcOrdLine)
IF llRpBOPFab
  SHOW GET pbNote,1  PROMPT 'Allo\<cate'  DISABLE
  SHOW GET pbHeadr,1 PROMPT 'Allocated \<Orders' DISABLE
ELSE
  SHOW GET pbNote,1  PROMPT '\<Notes' DISABLE
  SHOW GET pbHeadr,1 PROMPT IIF(lcChoice='P','\<P/O','\<C/T') DISABLE
ENDIF
*E301077,57 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')
=gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
=gfOpenFile(gcDataDir+'CODES',gcDataDir+'cCode_No','SH')
=gfwCodePop(@laCodInfo, "CTERMCODE", "D")
=gfwCodePop(@laCodInfo, "SHIPVIA",   "D")
*E301077,57 (End)

SELECT (lcSelFile)
llNothing = IIF(llFirst,lfAdUnCmSR(),'')
llFirst = .F.
*-- Case of generate form plan set the filter on the style file
IF lcChoice = 'L'
  IF lfSelStyle()
    =lfStyWhen()
    STORE .F. TO laScrMode
    laScrMode[3] = .T.
  ENDIF
  *-- Case of generate form plan set the filter on the ordline file
ELSE
  IF lfSelOrd()
    =lfOrdWhen()
    STORE .F. TO laScrMode
    laScrMode[3] = .T.
  ENDIF
ENDIF
DO lpShow

*!*************************************************************
*! Name      : lfwOption
*! Developer : Samah Wilson
*! Date      : 03/22/1998
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
IF lcChoice <> 'P'
  lnClrHElm = ASCAN(laOGFieldN , 'STYLE.CPURCODE')

  IF lnClrHElm > 0
    =gfADel(@laOGFieldN , lnClrHElm , 1)
    =gfADel(@laOGFieldH , lnClrHElm , 1)
  ENDIF    && End of IF

  llRefresh = .F.          && Flag to know if we need to refresh the Option Grid or not

  DO WHILE .T.
    lnVrFltElm = ASCAN(laOGVrFlt , 'STYLE.CPURCODE')      && Variable to hold the number of the array element for the color in the Option grid Variable filter array
    lnVrFltElm = IIF(lnVrFltElm = 0 , 0 , ASUBSCRIPT(laOGVrFlt , lnVrFltElm , 1))

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
ENDIF

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Samah Wilson
*! Date      : 03/22/1998
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
*! Name      : lfvStyFab
*! Developer : Wael Ali Mohamed
*! Date      : 03/22/1999           *B802056,1
*! Purpose   : Function called to validate style primary fabric.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvStyFab()
*!*************************************************************
FUNCTION lfvStyFab
PRIVATE lcObjNam , lcFabric

lcObjNam = SYS(18)
lcFabric = EVALUATE(SYS(18))
=gfOpenFile(gcDataDir+'FABRIC',gcDataDir+'FABRIC','SH')
IF !EMPTY(lcFabric) .AND. !SEEK(lcFabric,'FABRIC')
  =FABROW(@lcFabric,'*',.T.)
ENDIF
&lcObjNam = lcFabric

*!*************************************************************
*! Name      : lfvVendor
*! Developer : Samah Wilson
*! Date      : 03/22/1998
*! Purpose   : Function called from the validation of the vendor option grig
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvVendor()
*!*************************************************************
FUNCTION lfvVendor
PRIVATE lcObjNam , lcObjVal , llObjRet

lcObjNam = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
SET ORDER TO TAG Vencode IN APVENDOR
*-- IF The user want to Browse or if the Vendor he entered is not in the file
IF !EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'APVENDOR')
  llBrowse = .T.
  *-- E301268,1 HDM [Start] filter vendor browse by sup. type
  *llObjRet = gfApVnBrow(@lcObjVal)
  llObjRet = gfApVnBrow(@lcObjVal,.F.,IIF(lcChoice='P','S','C'))
  *-- E301268,1 HDM [End]
  lcObjVal = IIF(llObjRet , lcObjVal , SPACE(01))
  llBrowse = .F.
ENDIF    && End of IF
&lcObjNam = lcObjVal
*!*************************************************************
*! Name      : lfvDetail
*! Developer : Samah Wilson
*! Date      : 03/22/1998
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
IF lcChoice $ 'PC'
  IF lcChoice = 'P'
    SET RELATION TO PO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+CWARECODE+DYELOT INTO (lcCutPick) ADDITIVE
  ELSE
    SET RELATION TO CUTTKT+CDIVISION+SEASON+STYLE+CWARECODE+DYELOT INTO (lcCutPick) ADDITIVE
  ENDIF
  SELECT (lcCutPick)
  SET RELATION TO 'O'+ORDER INTO ORDHDR ADDITIVE
ENDIF
STORE 0 TO lnRecBrow1,lnRecBrow2
SELECT(lcLinFile)
GOTO TOP
PUSH KEY
=lfClearKey()
DO (gcScrDir+'MFGENCT2.SPX')
SELECT (lcLinFile)
SET RELATION TO
*B602503,1 Clear detail file key
SET KEY TO
*B602503,1 (End)

IF lcChoice $ 'PC'
  SELECT (lcCutPick)
  SET RELATION TO
ENDIF
POP KEY

*!*************************************************************
*! Name      : lfDeactDet
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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
    IF lcChoice='L'
      ON KEY LABEL TAB     DO lpMovTab  WITH 'MFGENC26','pbLClose'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'MFGENC26','pblClose'
    ELSE
      ON KEY LABEL TAB     DO lpMovTab  WITH 'MFGENC24','ibToPick'
      ON KEY LABEL BACKTAB DO lpBackTab WITH 'MFGENC24','pbClose'
    ENDIF
  ELSE
    ON KEY LABEL TAB     DO lpMovTab  WITH 'MFGENC24','pbClose'
    ON KEY LABEL BACKTAB DO lpBackTab WITH 'MFGENC24','ibToLine'
  ENDIF
ELSE
  glFromBrow = .F.
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfvOrder
*! Developer : Samah Wilson
*! Date      : 03/22/1998
*! Purpose   : Called from the validation of the order in the selection grid
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvOrder()
*!*************************************************************
FUNCTION lfvOrder

PRIVATE lcObjNam , lcObjVal , llObjRet

lcObjNam = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
lcObjVal = IIF(EMPTY(lcObjVal) , lcObjVal , PADL(ALLTRIM(lcObjVal) , 6 , '0'))
SET ORDER TO TAG ORDHDR IN ORDHDR
*-- IF The user want to Browse or if the Order number he entered is not in the file
IF !EMPTY(lcObjVal) .AND. !SEEK('O' + lcObjVal , 'ORDHDR')
  llBrowse = .T.
  llObjRet = OrdBrowO(@lcObjVal , .F. , 'O')
  lcObjVal = IIF(llObjRet , lcObjVal , SPACE(01))
  llBrowse = .F.
ENDIF    && End of IF
&lcObjNam = lcObjVal

*!*************************************************************
*! Name      : lfvCust
*! Developer : Samah Wilson
*! Date      : 03/22/1998
*! Purpose   : Called from the validation of the cutomer in the selection grid
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvCust()
*!*************************************************************
FUNCTION lfvCust
PRIVATE lcObjNam , lcObjVal , llObjRet

lcObjNam = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
SET ORDER TO TAG CUSTOMER IN CUSTOMER
IF !EMPTY(lcObjVal) .AND. !SEEK('M'+lcObjVal , 'CUSTOMER')
  llBrowse = .T.
  xAccount = lcObjVal
  DO CUSBROWM WITH xAccount
  lcObjVal = xAccount
  llBrowse = .F.
ENDIF    && End of IF
&lcObjNam = lcObjVal

*!*************************************************************
*! Name      : lfSelOrd
*! Developer : Samah Wilson
*! Date      : 03/22/1998
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
DECLARE laSort[6]
laSort[1] = 'DTOS(OrdHdr.Entered)'
laSort[2] = 'DTOS(OrdLine.Start)'
laSort[3] = 'DTOS(OrdLine.Complete)'
laSort[4] = 'OrdLine.Order'
laSort[5] = 'OrdHdr.Priority'
laSort[6] = 'OrdLine.Account'
lcSort = "''"
FOR lnSort =1 TO 3
  lcSort = lcSort + '+' + laSort[AT(EVAL('lcRPSORT'+STR(lnSort,1)),'ESCOPA')]
ENDFOR

*E301077,57 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH')
*E301077,57 (End)

SELECT STYLE
lcStyOrd = TAG()
SET ORDER TO TAG STYLE

*B802447,1  Delete The Old Record In The Temp File   [ BEGIN ]
SELECT (lcOrdLine)
DELETE ALL
*B802447,1 [ END ]


*SELECT(lcOrdLine)
*REPLACE ALL Flag WITH 'R'

*-- Set the required relations
SELECT ORDLINE
*B602497,1 Do not process order Contracts
SET KEY TO 'O'

SET RELATION TO Ordline.cOrdType+ Ordline.Order INTO Ordhdr ADDITIVE
SET RELATION TO Ordline.style INTO STYLE ADDITIVE
SET RELATION TO 'N'+'COLOR     '+SUBSTR(Ordline.Style,lnColorStr,lnColorLen);
  INTO CODES ADDITIVE

*-- Get the required records from the ordline file due to the expression
*-- returned form the selection grid
SELECT ORDLINE
*B802224,1
*SCAN FOR &lcExpr AND IIF(lcChoice='P',!STYLE.MAKE,STYLE.MAKE AND STYLE.lInvSty) AND;
!(Ordhdr.Status $ 'XCB')

*C200361,1 AMH trigger for custom RON20 to ignore picked ordline's [Start]
IF ASCAN(laEvntTrig , PADR('IGPICK',10)) <> 0
  =gfDoTriger('POGENPO',PADR('IGPICK',10))
ENDIF
*C200361,1 AMH [End]
        
*B803635,1 HBG IF user select a group of orders, Seek for the first 
*B803635,1     order of them[Begin]
lcScanExp = ""
IF !EMPTY(lcFOrder)
  SEEK "O" + lcFOrder
  lcScanExp = 'REST WHILE cOrdType+Order+STR(LineNo,6) >= "O"+lcFOrder AND'+;
              ' cOrdType+Order+STR(LineNo,6) <= "O"+lcTOrder'+;
              ' FOR '+ lcExpr + ' AND IIF(lcChoice="P",!STYLE.MAKE,STYLE.MAKE AND STYLE.lInvSty) AND'+;
              '!(Ordhdr.Status $ "XCB") AND TOTQTY-TOTCUT > 0'
ELSE
  lcScanExp = " FOR "+ lcExpr + " AND IIF(lcChoice='P',!STYLE.MAKE,STYLE.MAKE AND STYLE.lInvSty) AND"+;
              "!(Ordhdr.Status $ 'XCB') AND TOTQTY-TOTCUT > 0"
ENDIF

*SCAN FOR &lcExpr AND IIF(lcChoice='P',!STYLE.MAKE,STYLE.MAKE AND STYLE.lInvSty) AND;
         !(Ordhdr.Status $ 'XCB') AND TOTQTY-TOTCUT > 0
SCAN &lcScanExp
*B803635,1 [End]

  SCATTER MEMVAR
  m.cDivision = ORDHDR.cDivision
  m.cSortExp  = EVAL(lcSort)
  m.cStyGrade = STYLE.cStyGrade
  m.cPurCode  = STYLE.cPurCode
  
  *C200387,1 AMH trigger for custom RON20 to generate PO with the balance quantity [Start]
  IF ASCAN(laEvntTrig , PADR('GETBAL',10)) <> 0
    =gfDoTriger('POGENPO',PADR('GETBAL',10))
  ENDIF
  *C200387,1 AMH [End]
  
  IF !SEEK(m.cSortExp+ORDLINE.ORDER+STR(ORDLINE.LINENO,6),lcOrdLine)
    INSERT INTO (lcOrdLine) FROM MEMVAR
  ENDIF
  *B802224,1
  *REPLACE &lcOrdLine..Flag WITH 'N'
ENDSCAN
*B602497,1 Do not process order Contracts
SET KEY TO

*-- Reset the relation
SET RELATION TO
SELECT STYLE
SET RELATION TO
SET ORDER TO TAG &lcStyOrd

*B802224,1
*DELETE ALL FOR (Flag = 'R' OR (TOTQTY-TOTCUT<=0))
*WAIT CLEAR

SELECT(lcOrdLine)
GOTO TOP
IF EOF()
  =lfUpdUnCmS('I',SPACE(0) )
  *B603794,1 HBG IF no records matching the criteria refresh the screen and
  *B603794,1     give the user a message [Begin]
  =lfvSelect('')  
  SHOW GET pbSelect  DISABLE
  SHOW WINDOW (lcSrcTtl) REFRESH SAME
  laScrMode = .F.
  laScrMode[1] = .T.
  *B603794,1 [End]

  
  =gfModalGen('INM38129B38018','DIALOG','orders')
  RETURN(.F.)
ENDIF

*!*************************************************************
*! Name      : lfvSelect
*! Developer : Samah Wilson
*! Date      : 03/22/1998
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
  IF lcChoice = 'L'
    SUM REST PLAN1,PLAN2,PLAN3,PLAN4,PLAN5,PLAN6,PLAN7,PLAN8;
      TO   lnTotal1,lnTotal2,lnTotal3,lnTotal4,lnTotal5,lnTotal6,lnTotal7,lnTotal8 ;
      WHILE cSelect = "�"
  ELSE
    SUM REST (QTY1-CUT1),(QTY2-CUT2),(QTY3-CUT3),(QTY4-CUT4),;
      (QTY5-CUT5),(QTY6-CUT6),(QTY7-CUT7),(QTY8-CUT8) ;
      TO   lnTotal1,lnTotal2,lnTotal3,lnTotal4,lnTotal5,lnTotal6,lnTotal7,lnTotal8 ;
      WHILE cSelect = "�"
  ENDIF
  SHOW GET pbSelNone ENABLE
  SHOW GET pbDefault ENABLE
  IF (!llRpBOPFab OR llAllocate) AND (!EMPTY(lcVendor) OR lcChoice <> 'P')
    SHOW GET pbGenerate,1 PROMPT IIF(lcChoice = 'P',"\<Generate P/O","\<Generate C/T") ENABLE
  ELSE
    SHOW GET pbGenerate,1 PROMPT IIF(lcChoice = 'P',"\<Generate P/O","\<Generate C/T") DISABLE
  ENDIF
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

*!*************************************************************
*! Name      : lfvGenerate
*! Developer : Samah Wilson
*! Date      : 03/22/1998
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

*B603794,1 HBG Return the flag to false[Begin]
llRetToSel = .F.
*B603794,1 [End]

*E301077,57 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
=lfCrtDTemp()
*E301077,57 (End)

lnAlias = SELECT()
GOTO TOP IN (lcHdrFile)
llStartGen = .F.

IF EOF(lcHdrFile)
  *-- Generate mode
  IF lcChoice = 'L'
    llStartGen = .T.
    =lfUpdCtPlan()
  ELSE
    DO (gcScrDir+'MFGENCT3.SPX')
  ENDIF
ELSE
  *-- Clear generated mode
  =lfDelFiles(.F.)
ENDIF
=lfCutWhen()
GOTO TOP IN (lcSelFile)
=IIF(lcChoice='L',lfStyWhen(),lfOrdWhen())
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
*! Name      : lfPODef
*! Developer : Samah Wilson
*! Date      : 03/22/1998
*! Purpose   : Called from the P/O defaults button
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfPODef()
*!*************************************************************
FUNCTION lfPODef

*E301077,51 Inhance openning files to speed up transaction
=gfOpenFile(gcSysHome+'SYCCURR', gcSysHome+'CCURRCODE','SH')
=gfOpenFile(gcDataDir+'APVENDOR', gcDataDir+'Vencode','SH')
*E301077,51 (End)

*-- Call the P/O defaults screen
DO (gcScrDir+'MFGENCT4.SPX')
*E301077,51 Inhance openning files to speed up transaction
=gfCloseFile('SYCCURR')
=gfCloseFile('APVENDOR')
*E301077,51 (End)

IF !EMPTY(lcVendor) AND (!llRpBOPFab OR llAllocate)
  SHOW GET pbGenerate ENABLE
ELSE
  SHOW GET pbGenerate DISABLE
ENDIF

*!*************************************************************
*! Name      : lfvDefVen
*! Developer : Samah Wilson
*! Date      : 03/22/1998
*! Purpose   : Validation of the vendor at the PO defaults screen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvDefVen()
*!*************************************************************
FUNCTION lfvDefVen

lnAlias = SELECT()

*-- If the enterd vendor does not ecxit in the apvendor file
*B603078,1 Reham On 08/04/1999    *** Begin ***
*B603078,1 Change the seek condition to browse vendor that supplies styles or contactors.
*IF llBrowse OR (!EMPTY(lcVendor) AND !SEEK(lcVendor,'APVENDOR'))
*B802673,1 RAMY Change this line to fix browsing the vendor evenif enterd valid value [start]
*IF llBrowse OR (!EMPTY(lcVendor) AND (!SEEK(lcVendor,'APVENDOR') .OR. !(ApVendor.cvensuptyp $ "CS")))
IF llBrowse OR (!EMPTY(lcVendor) AND !SEEK(lcVendor,'APVENDOR')) .OR. ;
    !('S' $ APVENDOR.cVenSupTyp .OR. 'C' $ APVENDOR.cVenSupTyp)
*B802673,1 RAMY [end]

  *-- E301268,1 HDM [Start] filter vendor browse by sup. type
  *=gfApVnBrow(@lcVendor)
  *=gfApVnBrow(@lcVendor,.F.,'C')
  *B603078,1 Browse vendor that suplies styles & contractors.
  =gfApVnBrow(@lcVendor,.F.,'CS')
  *-- E301268,1 HDM [End]
  *B603078,1 Reham On 08/04/1999    *** End   ***
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
    
    *E301746,1 KHM 10/14/2001 (Begin) Store the base curruncy to the duty.
    lcDcurr = gcBaseCurr
    *E301746,1 KHM 10/14/2001 (End)
    
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

*!*************************************************************
*! Name      : lfvDefWare
*! Developer : Samah Wilson
*! Date      : 03/22/1998
*! Purpose   : Validation of the warehouse
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvDefWare()
*!*************************************************************
FUNCTION lfvDefWare

IF llBrowse OR !SEEK(lcWareCode,'WAREHOUS')
  *B606205,1 KHM 07/03/2002 (Begin) Changing the passed parameters in order to display the
  *B606205,1                style finished goods locations only.
  *lcWareCode= gfBrowWare( .T. )
  lcWareCode= gfbrowware(.T.,.F.,.F.,.F.,.F.,'S')
  *B606205,1 KHM 07/03/2002 (End)  
ENDIF
llBrowse = .F.

*!*************************************************************
*! Name      : lfvDefPCurr
*! Developer : Samah Wilson
*! Date      : 03/22/1998
*! Purpose   : Validation of the default price currency
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvDefPCurr()
*!*************************************************************
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

*!*************************************************************
*! Name      : lfvDefDCurr
*! Developer : Samah Wilson
*! Date      : 03/22/1998
*! Purpose   : Validation of the default duty currency
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvDefDCurr()
*!*************************************************************
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

*!*************************************************************
*! Name      : lfvOK
*! Developer : Samah Wilson
*! Date      : 03/22/1998
*! Purpose   : Validation of <OK> of the default PO screen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvOK()
*!*************************************************************
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

*!*************************************************************
*! Name      : lfShowDef
*! Developer : Samah Wilson
*! Date      : 03/22/1998
*! Purpose   : Show function for the objects of the PO default screen
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfShowDef()
*!*************************************************************
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

*!*************************************************************
*! Name      : lfvGenCtPO
*! Developer : Samah Wilson
*! Date      : 03/22/1998
*! Purpose   : Function to check if the generation can bedone or not
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvGenCtPO()
*!*************************************************************
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
*B602510,1 update warehouse when the system not use multi warehouse
IF EMPTY(lcWareCode)
  GO TOP IN 'WAREHOUS'
  lcWareCode = WAREHOUS.cWareCode
ENDIF
*B602510,1 (End)

IF llStartGen
  CLEAR READ
  lcFile = IIF(llRpBOPFab,lcTmpOrd,lcOrdLine)
  =IIF(lcChoice='P',lfUpdPOPik(lcFile),lfUpdCtPik(lcFile))
  SELECT (lcHdrFile)
  DELETE ALL FOR IIF(lcChoice='P',nStyOrder=0,Pcs_Bud=0)
  GOTO TOP
  IF EOF()
    llStartGen = .F.
    =gfModalGen('INM38130B38018','DIALOG',IIF(lcChoice = 'P','purchase orders','cutting tickets'))
  ENDIF
ELSE
  *-- (Using the specified generation percentages will create
  *-- 'cutting tickets with zero quantities. Cannot proceed
  *-- 'with generation.)
  =gfModalGen('INM38131B38018','DIALOG',IIF(lcChoice = 'P','purchase orders','cutting tickets'))
ENDIF

*!*************************************************************
*! Name      : lfwOldVals
*! Developer : Samah Wilson
*! Date      : 03/22/1998
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
*! Developer : Samah Wilson
*! Date      : 03/22/1998
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
  *B606205,1 KHM 07/03/2002 (Begin) Changing the passed parameters in order to display the
  *B606205,1                style finished goods locations only.
  *lcWareCode = gfBrowWare(.T.)
  lcWareCode  = gfbrowware(.T.,.F.,.F.,.F.,.F.,'S')
  *B606205,1 KHM 07/03/2002 (End)
  
  IF EMPTY(lcWareCode)
    lcWareCode = lnOldVal
  ENDIF
  REPLACE &lcHdrFile..cWareCode WITH lcWareCode
ENDIF
SELECT (lnAlias)
RETURN .T.
*!*************************************************************
*! Name      : lfwBrDylot
*! Developer : Samah Wilson
*! Date      : 03/22/1998
*! Purpose   : Called from the dyelot field in the browse of the line file
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwBrDylot()
*!*************************************************************
FUNCTION lfwBrDylot

*B602437,1 Do not modify dylots when ticket line allocated.
*IF (lcChoice='P' AND !EMPTY(PO)) OR (lcChoice='C' AND !EMPTY(CutTkt))
IF llRpBOPFab OR (lcChoice='P' AND !EMPTY(PO)) OR (lcChoice='C' AND !EMPTY(CutTkt))
  *B602437,1 (End)
  RETURN(.F.)
ENDIF
=SEEK(STYLE,'STYLE')
RETURN (STYLE.cDye_FLg='Y')

*!*************************************************************
*! Name      : lfvDyelot
*! Developer : Samah Wilson
*! Date      : 03/22/1998
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
  *B602437,1 Commented out
  *RETURN .F.
  *B602437,1 (End)
ENDIF

IF lcChoice <> 'L'
  lcDyelot = Dyelot
  lcKey=IIF(lcChoice='P',PO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+CWARECODE,;
    CUTTKT+CDIVISION+SEASON+STYLE+CWARECODE)
  lcExp=IIF(lcChoice='P','CTKTNO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+CWARECODE+DYELOT',;
    'CTKTNO+CDIVISION+SEASON+STYLE+CWARECODE+DYELOT')
  SELECT (lcCutPick)
  REPLACE ALL Dyelot WITH lcDyelot FOR EVAL(lcExp) = lcKey
  SELECT(lcLinFile)
ENDIF

*!*************************************************************
*! Name      : lfwBrOldVal
*! Developer : Samah Wilson
*! Date      : 03/22/1998
*! Purpose   : Called from the Qty fields in the browse of the line file
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwBrOldVal()
*!*************************************************************
FUNCTION lfwBrOldVal

IF (lcChoice = 'P' AND !EMPTY(&lcLinFile..PO)) OR ;
    (lcChoice = 'C' AND !EMPTY(&lcLinFile..CUTTKT))
  RETURN .F.
ENDIF
IF VAL(RIGHT(SYS(18),1)) <= SCALE.CNT
  lnOldVal = EVALUATE(SYS(18))
ELSE
  RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfvCTQty
*! Developer : Samah Wilson
*! Date      : 03/22/1998
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
  =gfModalGen('INM38133B38018','DIALOG',IIF(lcChoice = 'P','P/O','C/T')+'|'+IIF(lcChoice = 'P','P/O','C/T'))
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
      *E300725,1 Message : 38150
      *E300725,1 Not Enough Inventory available to allocate this order line
      *E300725,1 Button : 00000
      *E300725,1 Ok
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
    nCost1 WITH nCost1 + lnAddedQty * IIF(lcChoice='P',STYLE.nICost1,STYLE.nMCost1),;
    nCost2 WITH nCost2 + lnAddedQty * IIF(lcChoice='P',STYLE.nICost2,STYLE.nMCost2),;
    nCost3 WITH nCost3 + lnAddedQty * IIF(lcChoice='P',STYLE.nICost3,STYLE.nMCost3),;
    nCost4 WITH nCost4 + lnAddedQty * IIF(lcChoice='P',STYLE.nICost4,STYLE.nMCost4),;
    nCost5 WITH nCost5 + lnAddedQty * IIF(lcChoice='P',STYLE.nICost5,STYLE.nMCost5)
  SELECT (lcHdrFile)
  IF lcChoice = 'P'
    REPLACE nStyOrder WITH nStyOrder + lnAddedQty ,;
      nICost1   WITH nICost1 + lnAddedQty * STYLE.nICost1,;
      nICost2   WITH nICost2 + lnAddedQty * STYLE.nICost2,;
      nICost3   WITH nICost3 + lnAddedQty * STYLE.nICost3,;
      nICost4   WITH nICost4 + lnAddedQty * STYLE.nICost4,;
      nICost5   WITH nICost5 + lnAddedQty * STYLE.nICost5
  ELSE
    REPLACE Pcs_Bud    WITH Pcs_Bud + lnAddedQty ,;
      nEst_Cost1 WITH nEst_Cost1 + lnAddedQty * STYLE.nMCost1,;
      nEst_Cost2 WITH nEst_Cost2 + lnAddedQty * STYLE.nMCost2,;
      nEst_Cost3 WITH nEst_Cost3 + lnAddedQty * STYLE.nMCost3,;
      nEst_Cost4 WITH nEst_Cost4 + lnAddedQty * STYLE.nMCost4,;
      nEst_Cost5 WITH nEst_Cost5 + lnAddedQty * STYLE.nMCost5
  ENDIF
  SELECT (lcLinFile)
  =lfPickWhen()
ENDIF
SHOW WINDOW (lcBrTtl1) REFRESH SAME

*!*************************************************************
*! Name      : lfwPick
*! Developer : Samah Wilson
*! Date      : 03/22/1998
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
*! Developer : Samah Wilson
*! Date      : 03/22/1998
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
=lfRefresh('MFGENC24')
=lfRefresh('MFGENC25')

*!*************************************************************
*! Name      : lpSavScr
*! Developer : Samah Wilson
*! Date      : 03/22/1998
*! Purpose   : Local procedure for the save
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lpSavScr
*!*************************************************************
PROCEDURE lpSavScr

*B603516,1 SSH 14/02/2000 Check if there is any empty no.
SELECT (lcHdrFile)
*B603559,1 SSH 01/04/00   Fix the bug of alias not found if cancel session.
IF lcChoice='P' AND EOF()
  llCSave = .F.
ENDIF
IF lcChoice='C' AND EOF()
  llCSave = .F.
ENDIF
*B603559,1 SSH 01/04/00   Fix the bug of alias not found if cancel session.
IF llGManualy .AND. SEEK(SPACE(06))
  lcStrToAdd = IIF(lcChoice = 'P',"Purchase Orders","Cutting Ticket")
  lnNoThing = gfModalGen('TRM38047B00000','DIALOG',lcStrToAdd)
  llCSave = .F.
  RETURN
ENDIF
*B603516,1 SSH 14/02/2000 (END)
=lfUpdUnCmS('O',"pbSav" )
*E301077,51 Inhance openning files to speed up transaction
=gfOpenFile(gcDataDir+'STYDYE', gcDataDir+'STYDYE','SH')
*E301077,51 (End)

*B603559,1 SSH 01/04/00   Fix the bug of alias not found if cancel session.
IF llCSave
*B603559,1 SSH 01/04/00   Fix the bug of alias not found if cancel session.
  SET ORDER TO TAG STYLE IN STYLE
  DO CASE
  CASE lcChoice = 'P'
    =lfUpdPO()
  CASE lcChoice = 'C'
    =lfUpdCT()
  CASE lcChoice = 'L'
    =lfUpdCTP()
  ENDCASE
*B603559,1 SSH 01/04/00   Fix the bug of alias not found if cancel session.
ENDIF
*B603559,1 SSH 01/04/00   Fix the bug of alias not found if cancel session.
=lfUpdUnCmS(IIF(llCSave,'C','O'),SPACE(0))
*E301077,51 Inhance openning files to speed up transaction
=gfCloseFile('STYDYE')
*E301077,51 (End)


IF !llCSave
  *B603794,1 HBG Allow user to select anoter order or group 
  *B603794,1     group of orders to generate PO without go to OG [Begin]
  *B604029,1 HBG 11/26/2000 This function will be called in the select mode [Begin]
  *=lfRetToSel(.F.)
  *B604029,1 [End]
  *B603794,1 [End]
  RETURN
ENDIF
*-- if there is at least one record generated
*B603516,1 SSH 14/02/2000 Dont display if manualy.
*IF !EMPTY(lcFirstCT)
IF !llGManualy .AND. !EMPTY(lcFirstCT)
*B603516,1 SSH 14/02/2000  (End)
  *-- Give the numbers of the generated P/Os or C/Ts
  =gfModalGen('INM38134B38018','DIALOG',IIF(lcChoice = 'P','P/O','C/T')+'|'+ALLTRIM(lcFirstCT)+"|"+ALLTRIM(lcLastCt))
ENDIF
IF llRpBOPFab
  *E300725,1 Message : 38151
  *E300725,1 Whould you like to print a process summary report?
  *E300725,1 Button : 38006
  *E300725,1 Yes,No
  IF gfModalGen('QRM38151B38006','ALERT') =1
    =lfPrnSumm()
  ENDIF
ENDIF

*B603794,1 HBG Allow user to select anoter order or group 
*B603794,1     group of orders to generate PO without go to OG [Begin]
*B604029,1 HBG 11/26/2000 This function will be called in the select mode 
*B604029,1                and moving to view mode[Begin]
*=lfRetToSel(.T.)
STORE .F. TO laScrMode
laScrMode[2] = .T.
*B604029,1 [End]
*B603794,1 [End]


*!*************************************************************
*! Name      : lfvOrQty
*! Developer : Samah Wilson
*! Date      : 03/22/1998
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
      nCost1     WITH nCost1     + lnAddedQty * IIF(lcChoice='P',STYLE.nICost1,STYLE.nMCost1),;
      nCost2     WITH nCost2     + lnAddedQty * IIF(lcChoice='P',STYLE.nICost2,STYLE.nMCost2),;
      nCost3     WITH nCost3     + lnAddedQty * IIF(lcChoice='P',STYLE.nICost3,STYLE.nMCost3),;
      nCost4     WITH nCost4     + lnAddedQty * IIF(lcChoice='P',STYLE.nICost4,STYLE.nMCost4),;
      nCost5     WITH nCost5     + lnAddedQty * IIF(lcChoice='P',STYLE.nICost5,STYLE.nMCost5)
    UNLOCK
    IF lcChoice = 'P'
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
    ELSE
      SELECT (lcCutTktH)
      =RLOCK()
      REPLACE Pcs_Bud    WITH Pcs_Bud    + lnAddedQty,;
        TotOrd     WITH TotOrd     + lnAddedQty,;
        nEst_Cost1 WITH nEst_Cost1 + lnAddedQty * STYLE.nMCost1,;
        nEst_Cost2 WITH nEst_Cost2 + lnAddedQty * STYLE.nMCost2,;
        nEst_Cost3 WITH nEst_Cost3 + lnAddedQty * STYLE.nMCost3,;
        nEst_Cost4 WITH nEst_Cost4 + lnAddedQty * STYLE.nMCost4,;
        nEst_Cost5 WITH nEst_Cost5 + lnAddedQty * STYLE.nMCost5
      UNLOCK
    ENDIF
    SELECT (lcCutPick)
    =RLOCK()
    REPLACE TotQty WITH TotQty + lnAddedQty
    UNLOCK
  ENDIF
  =lfRefresh('MFGENC25')
  SHOW WINDOW (lcBrTtl1) REFRESH SAME
  SHOW WINDOW (lcBrTtl2) REFRESH SAME
ENDIF

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
PRIVATE lnAlias,lcCutTag,lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,;
  lnQty8,llContinue

lnAlias = SELECT()
IF TYPE('lcExpr')='L'
  RETURN(.F.)
ENDIF
=gfOpenFile(gcDataDir+'CUTTKTL',gcDataDir+'CUTTKTL','SH')
SELECT CutTktL
lcCutTag = ORDER('CutTktL')
SET ORDER TO TAG Cuttktls
SET RELATION TO CutTkt INTO CutTktH
SELECT (lcStyTmp)
DELETE ALL
SELECT STYLE
SET ORDER TO TAG STYLE
*B802867,1 AMM Set relation to code
SET RELATION TO 'N'+'COLOR     '+SUBSTR(Style.Style,lnColorStr,lnColorLen);
  INTO CODES ADDITIVE
*B802867,1 AMM end
STORE .F. TO llContinue
SCAN FOR (&lcExpr AND MAKE AND TOTPLAN > 0 )
  SCATTER MEMVAR
  SELECT CutTktL
  =SEEK(m.STYLE)
  STORE 0 TO lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8
  SCAN REST WHILE STYLE+CUTTKT+TRANCD = m.Style FOR (CutTktH.Status<>'X')
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      lnQty&lcCount = lnQty&lcCount+IIF(CutTktH.Status<>'C',;
        IIF(TranCd='1',Qty&lcCount ,IIF(TranCd$'34',(-1*Qty&lcCount ),0)),;
        IIF(TranCd='2',Qty&lcCount ,0))
    ENDFOR
  ENDSCAN
  m.PLAN1   = MAX(m.PLAN1-lnQty1,0)
  m.PLAN2   = MAX(m.PLAN2-lnQty2,0)
  m.PLAN3   = MAX(m.PLAN3-lnQty3,0)
  m.PLAN4   = MAX(m.PLAN4-lnQty4,0)
  m.PLAN5   = MAX(m.PLAN5-lnQty5,0)
  m.PLAN6   = MAX(m.PLAN6-lnQty6,0)
  m.PLAN7   = MAX(m.PLAN7-lnQty7,0)
  m.PLAN8   = MAX(m.PLAN8-lnQty8,0)
  m.TOTPLAN = m.PLAN1+m.PLAN2+m.PLAN3+m.PLAN4+m.PLAN5+m.PLAN6+m.PLAN7+m.PLAN8
  IF m.TOTPLAN > 0
    llContinue = .T.
    INSERT INTO (lcStyTmp) FROM MEMVAR
  ENDIF
ENDSCAN
*B802867,1 AMM Remove the relation
SET RELATION OFF INTO CODES
*B802867,1 AMM end

IF !llContinue
  *-- if no records matching the criteria give the user a message
  =lfUpdUnCmS('I',SPACE(0) )
  *B603794,1 HBG IF no records matching the criteria refresh the screen[Begin]
  =lfvSelect('')  
  SHOW GET pbSelect  DISABLE
  SHOW WINDOW (lcSrcTtl) REFRESH SAME
  laScrMode = .F.
  laScrMode[1] = .T.
  *B603794,1 [End]
  =gfModalGen('INM38129B38018','DIALOG','styles')
ENDIF
GO TOP IN (lcStyTmp)
SELECT CutTktL
SET RELATION TO
SELECT(lnAlias)
RETURN(llContinue)

*!*************************************************************
*! Name      : lfStyBrow
*! Developer : Samah Wilson
*! Date      : 03/22/1998
*! Purpose   : Browse the selected style from the selection grid
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfStyBrow()
*!*************************************************************
FUNCTION lfStyBrow
PRIVATE lnAlias

lnAlias = SELECT()
SELECT(lcStyTmp)
lcBrowStr = 'TotDb=IIF(RECNO()=lnStyRecNo,">"," "):R:H=" ",cSelect :R:H=" ",Style:R ,'+;
  'Plan1:R:H="Qty1",Plan2:R:H="Qty2",' +;
  'Plan3:R:H="Qty3",Plan4:R:H="Qty4",' +;
  'Plan5:R:H="Qty5",Plan6:R:H="Qty6",' +;
  'Plan7:R:H="Qty7",Plan8:R:H="Qty8",TotPlan:R:H="TotQty"'
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
=lfReFresh(lcWinC14)

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
PRIVATE lnAlias,lnTotQty,lnEstCost1,lnEstCost2,lnEstCost3,lnEstCost4,;
  lnEstCost5,lnLineNo,lcCutTKt

lnAlias = SELECT()
SELECT (lcStyTmp)
SET ORDER TO TAG 'TICKET'
=SEEK("�")
DO WHILE CSELECT+CDIVISION+SEASON+STYLE = "�"
  =SEEK(STYLE,'STYLE')
  SELECT (lcCuttktH)
  APPEND BLANK
  REPLACE STYLE     WITH SUBSTR(&lcStyTmp..STYLE,1,lnMajorLen) ,;
    DESC      WITH STYLE.Desc     ,;
    SEASON    WITH STYLE.Season   ,;
    CDIVISION WITH STYLE.CDIVISION,;
    PATTERN   WITH STYLE.Pattern  ,;
    cWareCode WITH STYLE.cDefWare ,;
    STATUS    WITH 'H',;
    ENTERED   WITH gdSysDate,;
    COMPLETE  WITH ENTERED+60

  SELECT (lcStyTmp)
  llMultiWare = .F.
  lcCutTKt = CDIVISION+SEASON+SUBSTR(STYLE,1,lnMajorLen)
  STORE 0 TO lnTotQty,lnEstCost1,lnEstCost2,lnEstCost3,lnEstCost4,lnEstCost5,lnLineNo
  SCAN REST WHILE CSELECT+CDIVISION+SEASON+STYLE = "�"+lcCutTKt
    =SEEK(STYLE,'STYLE')
    lnLineNo = lnLineNo + 1
    SELECT(lcCuttktL)
    APPEND BLANK
    REPLACE STYLE     WITH STYLE.STYLE  ,;
      TranCD    WITH '1'      ,;
      CDIVISION WITH STYLE.CDIVISION ,;
      SEASON    WITH STYLE.SEASON ,;
      LINENO    WITH lnLineNo ,;
      cWareCode WITH STYLE.cDefWare ,;
      QTY1      WITH &lcStyTmp..Plan1 ,;
      QTY2      WITH &lcStyTmp..Plan2 ,;
      QTY3      WITH &lcStyTmp..Plan3 ,;
      QTY4      WITH &lcStyTmp..Plan4 ,;
      QTY5      WITH &lcStyTmp..Plan5 ,;
      QTY6      WITH &lcStyTmp..Plan6 ,;
      QTY7      WITH &lcStyTmp..Plan7 ,;
      QTY8      WITH &lcStyTmp..Plan8 ,;
      TOTQTY    WITH &lcStyTmp..TotPlan
    *B602227,1 Update CT lines cost.
    REPLACE nCost1 WITH TotQty * Style.nMCost1 ,;
      nCost2 WITH TotQty * Style.nMCost2 ,;
      nCost3 WITH TotQty * Style.nMCost3 ,;
      nCost4 WITH TotQty * Style.nMCost4 ,;
      nCost5 WITH TotQty * Style.nMCost5
    lnTotQty   = lnTotQty   + TOTQTY
    lnEstCost1 = lnEstCost1 + TOTQTY*STYLE.nMCost1
    lnEstCost2 = lnEstCost2 + TOTQTY*STYLE.nMCost2
    lnEstCost3 = lnEstCost3 + TOTQTY*STYLE.nMCost3
    lnEstCost4 = lnEstCost4 + TOTQTY*STYLE.nMCost4
    lnEstCost5 = lnEstCost5 + TOTQTY*STYLE.nMCost5
    llMultiWare= IIF(cWareCode <> &lcCuttktH..cWareCode,.T.,llMultiWare)
  ENDSCAN
  SELECT(lcCuttktH)
  REPLACE LastLine   WITH LastLine   ,;
    Pcs_Bud    WITH lnTotQty   ,;
    nEst_Cost1 WITH lnEstCost1 ,;
    nEst_Cost2 WITH lnEstCost2 ,;
    nEst_Cost3 WITH lnEstCost3 ,;
    nEst_Cost4 WITH lnEstCost4 ,;
    nEst_Cost5 WITH lnEstCost5 ,;
    lMultiWare WITH llMultiWare
  SELECT (lcStyTmp)
ENDDO
SET ORDER TO TAG (lcStyTmp) IN (lcStyTmp)
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvNote
*! Developer : Samah Wilson
*! Date      : 03/22/1998
*! Purpose   : Calls the notes for the selected record of the generated P/Os or C/Ts
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvNote()
*!*************************************************************
FUNCTION lfvNote

IF laScrMode[3]
  IF llAllocate
    =lfClrAllo()
    =lfvSelect('')
    SHOW GET pbNote,1 PROMPT 'Allo\<cate' ENABLE
    SHOW GET pbHeadr    DISABLE
    SHOW GET pbGenerate DISABLE
    SHOW GET pbScope    ENABLE
    *B803686,1 RAMY [start]
    llAllocate = !llAllocate
    =lfUpdVars()
    *B803686,1 RAMY [end]
  ELSE
    *B803686,1 RAMY Add the if condition to chek if the user has 
    *               fabrics or not (in func. lfFabPri()) [start]
    *=lfGetFab() AND lfFabPri() AND lfBaseOnFab()
    IF lfGetFab() AND lfFabPri() AND lfBaseOnFab()
    *B803686,1 RAMY [end]
      SHOW GET pbNote,1 PROMPT 'Clear Allo\<cation' ENABLE
      SHOW GET pbSelect  DISABLE
      SHOW GET pbSelAll  DISABLE
      SHOW GET pbSelNone DISABLE
      SHOW GET pbInvert  DISABLE
      SHOW GET pbHeadr   ENABLE
      SHOW GET pbScope   DISABLE
      IF !EMPTY(lcVendor) OR lcChoice <> 'P'
        SHOW GET pbGenerate ENABLE
      ENDIF
      *B803686,1 RAMY [start]
      llAllocate = !llAllocate
      =lfUpdVars()
      *B803686,1 RAMY [end]
    ENDIF
  ENDIF
   *B803686,1 RAMY Remove these lines and add them in top[start]
  *llAllocate = !llAllocate
  *=lfUpdVars()
  *B803686,1 RAMY [end]
ELSE
  SELECT (lcHdrFile)
  IF EOF()
    GOTO TOP
  ENDIF
  *B603557,1 SSH 01/04/00 [Begin] Fix the bug of alias not found If press notespad. 
  IF lcChoice='P'
    lcBaseFile = 'POSHDR'
    lcSydKey   = '|POSHDR'
  ELSE
    lcBaseFile = 'CutTktH'
    lcSydKey   = '|CutTktH'
  ENDIF
  *B603557,1 SSH 01/04/00  [End]
  =IIF(lcChoice='P',NotePad('P',PO),NotePad('I',CutTkt))
  *B603557,1 SSH 01/04/00 [Begin] Fix the bug of alias not found If press notespad. 
  lcBaseFile = ''
  lcSydKey   = ''
  DO CASE
    CASE lcChoice = 'C'
      llByMe =gfOpenFile(gcDataDir+'NotePad','NotePad','SH')
      IF SEEK('I'+&lcHdrFile..CutTkt,'NotePad')
        SELECT CutTktH
        =SEEK(&lcHdrFile..CutTkt)
        REPLACE lhasNotes WITH .T.
      ENDIF
      USE IN IIF(llByMe,'NotePad',0)
    CASE lcChoice = 'P'
      llByMe =gfOpenFile(gcDataDir+'NotePad','NotePad','SH')
      IF SEEK('P'+&lcHdrFile..PO,'NotePad')
        SELECT POSHDR
        =SEEK('P'+&lcHdrFile..PO)
        REPLACE lhasNotes WITH .T.
      ENDIF
      USE IN IIF(llByMe,'NotePad',0)
  ENDCASE
  *B603557,1 SSH 01/04/00  [End]
ENDIF

*!*************************************************************
*! Name      : lfEditHdr
*! Developer : Samah Wilson
*! Date      : 03/22/1998
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
  IF lcChoice = 'P'
    lcCall  = 'P'
    lcParam = "'"+lcCall+"','"+&lcPOH..PO+ "'"
    DO gpDoProg WITH 'AWRPOSTYLE',.F.,'PO',lcParam
  ELSE
    DO gpDoProg WITH 'AWRMFCUTKT',.F.,'MF',"'" +&lcCuttktH..CutTkt+"'"
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfChkUnCmS
*! Developer : Samah Wilson (SWK)
*! Date      : 22/03/98
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
  IF lcChoice <> 'L'
    =lfBrowOrd()
  ELSE
    =lfStyBrow()
  ENDIF
  llGoOut   = .T.
  STORE .F. TO laScrMode
  laScrMode[ATC(lcScrMode,"SVEA")] = .T.
  llCUpdate  = laScrMode[3] OR laScrMode[4]
  lcSession  = UnCmSess.cSession
  *E301077,57 Inhance openning files to speed up transaction
  IF lcChoice='P'
    lnOldTerm  = lnTerm
    lnOldShip  = lnShip
    lnShip    = 1
    lnTerm    = 1
    =gfwCodePop(@laCodInfo, "CTERMCODE", "L")
    =gfwCodePop(@laCodInfo, "SHIPVIA", "L")
    lnShip = lnOldShip
    lnTerm = lnOldTerm
  ENDIF
  *E301077,57 (End)
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
*! Developer : Samah Wilson (SWK)
*! Date      : 09/10/97
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
*! Developer : Samah Wilson (SWK)
*! Date      : 09/10/97
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
DO CASE
CASE lcChoice = 'P'
  lcFiles = "lcOrdLine," + lcOrdLine + "," +ORDER(lcOrdLine)  + ";" + ;
    "lcPOH,"     + lcPOH     + "," +ORDER(lcPOH)      + ";" + ;
    IIF(laScrMode[4],"lcCutPick,"+ lcCutPick + "," +ORDER(lcCutPick) + ";" + ;
    "lcPOLine,"  + lcPOLine+ "," +ORDER(lcPOLine)   + ";",'') + ;
    IIF(llRpBOPFab,"lcOrdGroup,"+ lcOrdGroup+ "," +ORDER(lcOrdGroup) + ";" + ;
    "lcFabrics," + lcFabrics + "," +ORDER(lcFabrics)  + ";" + ;
    "lcTmpOrd,"  + lcTmpOrd  + "," +ORDER(lcTmpOrd)   + ";",'') + ;
    IIF(llRPUnAWip,"lcStyUnAll,"+ lcStyUnAll+ "," +ORDER(lcStyUnAll) + ";",'')
CASE lcChoice = 'C'
  lcFiles = "lcCuttktH," + lcCuttktH+ ","  +ORDER(lcCuttktH)  + ";" + ;
    "lcOrdLine," + lcOrdLine+ ","  +ORDER(lcOrdLine)  + ";" + ;
    IIF(laScrMode[4],"lcCuttktL,"+ lcCuttktL+ "," +ORDER(lcCuttktL) + ";" + ;
    "lcCutPick,"  + lcCutPick+ "," +ORDER(lcCutPick)   + ";",'') + ;
    IIF(llRpBOPFab,"lcOrdGroup,"+ lcOrdGroup+ "," +ORDER(lcOrdGroup) + ";" + ;
    "lcFabrics," + lcFabrics + "," +ORDER(lcFabrics)  + ";" + ;
    "lcTmpOrd,"  + lcTmpOrd  + "," +ORDER(lcTmpOrd)   + ";",'') + ;
    IIF(llRPUnAWip,"lcStyUnAll,"+ lcStyUnAll+ "," +ORDER(lcStyUnAll) + ";",'')
CASE lcChoice = 'L'
  lcFiles = "lcCuttktH," + lcCuttktH+ "," +ORDER(lcCuttktH)+ ";" + ;
    IIF(laScrMode[4],"lcCuttktL," + lcCuttktL+ "," +ORDER(lcCuttktL)+ ";",'') + ;
    "lcStyTmp,"  + lcStyTmp + "," + ";"
ENDCASE
SELECT UnCmSess
=RLOCK()
=IIF(lnUnCmSeRc=0,.T.,gfSavSess(lcProgID, lcFiles, @laVars))
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfUpdUnCmS
*! Developer : Samah Wilson (SWK)
*! Date      : 09/10/97
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

*E301426,1 SSH 11/06/2000 Check if not used UnCmSess open it.
PRIVATE llByMe
IF !USED('UnCmSess')
  llByMe = gfOpenFile(gcDataDir+'UnCmSess',gcDataDir+'Trans','SH')
ENDIF
*E301426,1 SSH [End]
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
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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

*B603794,1 HBG Return the flag to fals befor Close or cancel the screen[Begin]
llRetToSel = .F.
*B603794,1 [End]

=lfDelFiles(.T.)
=IIF(lcChoice='L',lfStyWhen(),lfOrdWhen()) AND lfCutWhen()
IF SEEK('O'+lcProgID+lcUserID,'UnCmSess')
  lnUnCmSeRc = RECNO('UnCmSess')
ENDIF
=lfUpdUnCmS("I", SPACE(0))

*!*************************************************************
*! Name      : lfDelFiles
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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
  IF lcChoice='P' AND SEEK('P'+PO,'POSHDR')
    SELECT POSHDR
    REPLACE FLAG WITH ' '
  ENDIF
  IF lcChoice='C' AND SEEK(CUTTKT,'CUTTKTH')
    SELECT CUTTKTH
    REPLACE RECFLAG WITH ' '
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
IF INLIST(lcChoice,'C','P') AND USED(lcCutPick)
  SELECT (lcCutPick)
  SCAN
    BLANK
    DELETE
  ENDSCAN
ENDIF

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
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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

IF lcChoice $ 'LC'
  SELECT CUTTKTH
  =AFIELDS(lafilfield)
  lnAlen = ALEN(lafilfield,1)
  DIMENSION lafilfield[lnAlen+3,4]
  lafilfield[lnAlen+1,1] = 'LASTLINE'
  lafilfield[lnAlen+1,2] = 'N'
  lafilfield[lnAlen+1,3] = 6
  lafilfield[lnAlen+1,4] = 0
  lafilfield[lnAlen+2,1] = 'nSteps'
  lafilfield[lnAlen+2,2] = 'N'
  lafilfield[lnAlen+2,3] = 2
  lafilfield[lnAlen+2,4] = 0
  lafilfield[lnAlen+3,1] = 'cTmpCutTkt'
  lafilfield[lnAlen+3,2] = 'C'
  lafilfield[lnAlen+3,3] = 6
  lafilfield[lnAlen+3,4] = 0
  =gfCrtTmp(lcCuttktH,@lafilfield,[CUTTKT+CDIVISION+SEASON+STYLE],[CSTYLE])
  *E301077,57 Inhance openning files to speed up transaction
  *SELECT CUTTKTL
  *=AFIELDS(lafilfield)
  *lnAlen = ALEN(lafilfield,1)
  *DIMENSION lafilfield[lnAlen+7,4]
  *lafilfield[lnAlen+1,1] = 'CDIVISION'
  *lafilfield[lnAlen+1,2] = 'C'
  *lafilfield[lnAlen+1,3] = 6
  *lafilfield[lnAlen+1,4] = 0
  *lafilfield[lnAlen+2,1] = 'SEASON'
  *lafilfield[lnAlen+2,2] = 'C'
  *lafilfield[lnAlen+2,3] = 6
  *lafilfield[lnAlen+2,4] = 0
  *lafilfield[lnAlen+3,1] = 'nSteps'
  *lafilfield[lnAlen+3,2] = 'N'
  *lafilfield[lnAlen+3,3] = 2
  *lafilfield[lnAlen+3,4] = 0
  *laFilField[lnAlen+4,1] = 'Fabric'
  *laFilField[lnAlen+4,2] = 'C'
  *laFilField[lnAlen+4,3] = 7
  *laFilField[lnAlen+4,4] = 0
  *laFilField[lnAlen+5,1] = 'cFabClr'
  *laFilField[lnAlen+5,2] = 'C'
  *laFilField[lnAlen+5,3] = 6
  *laFilField[lnAlen+5,4] = 0
  *laFilField[lnAlen+6,1] = 'cFabWare'
  *laFilField[lnAlen+6,2] = 'C'
  *laFilField[lnAlen+6,3] = 6
  *laFilField[lnAlen+6,4] = 0
  *laFilField[lnAlen+7,1] = 'nYeild'
  *laFilField[lnAlen+7,2] = 'N'
  *laFilField[lnAlen+7,3] = 7
  *laFilField[lnAlen+7,4] = 3
  *=gfCrtTmp(lcCuttktL,@lafilfield,[CUTTKT+CDIVISION+SEASON+STYLE+DYELOT],[CSTYCLR])
  *E301077,57 (End)
ENDIF
IF lcChoice <> 'L'
  *E301077,57 Inhance openning files to speed up transaction
  *SELECT CUTPICK
  *=AFIELDS(lafilfield)
  *lnAlen = ALEN(lafilfield,1)
  *DIMENSION lafilfield[lnAlen+7,4]
  *lafilfield[lnAlen+1,1] = 'CDIVISION'
  *lafilfield[lnAlen+1,2] = 'C'
  *lafilfield[lnAlen+1,3] = 6
  *lafilfield[lnAlen+1,4] = 0
  *lafilfield[lnAlen+2,1] = 'SEASON'
  *lafilfield[lnAlen+2,2] = 'C'
  *lafilfield[lnAlen+2,3] = 6
  *lafilfield[lnAlen+2,4] = 0
  *lafilfield[lnAlen+3,1] = 'cPurCode'
  *lafilfield[lnAlen+3,2] = 'C'
  *lafilfield[lnAlen+3,3] = 6
  *lafilfield[lnAlen+3,4] = 0
  *laFilField[lnAlen+4,1] = 'cStyGrade'
  *laFilField[lnAlen+4,2] = 'C'
  *laFilField[lnAlen+4,3] = 1
  *laFilField[lnAlen+4,4] = 0
  *lafilfield[lnAlen+5,1] = 'CWARECODE'
  *lafilfield[lnAlen+5,2] = 'C'
  *lafilfield[lnAlen+5,3] = 6
  *lafilfield[lnAlen+5,4] = 0
  *laFilField[lnAlen+6,1] = 'Dyelot'
  *laFilField[lnAlen+6,2] = 'C'
  *laFilField[lnAlen+6,3] = 10
  *laFilField[lnAlen+6,4] = 0
  *lafilfield[lnAlen+7,1] = 'nSteps'
  *lafilfield[lnAlen+7,2] = 'N'
  *lafilfield[lnAlen+7,3] = 2
  *lafilfield[lnAlen+7,4] = 0
  *DECLARE laIndex[1,2]
  *laIndex[1,1] = IIF(lcChoice='C',;
  *              'CTKTNO+CDIVISION+SEASON+STYLE+CWARECODE+DYELOT',;
  *              'CTKTNO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+CWARECODE+DYELOT')
  *laIndex[1,2] = 'CSTYCLR'
  *=gfCrtTmp(lcCutPick,@lafilfield,@laIndex)
  *E301077,57 (End)

  SELECT OrdLine
  =AFIELDS(laFilField)
  lnAlen = ALEN(laFilField,1)
  DIMENSION laFilField[lnAlen+11,4]
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
  *E301089,1 Consider decimal point in fabric onhand
  *laFilField[lnAlen+9,1] = 'nRequired'
  *laFilField[lnAlen+9,2] = 'N'
  *laFilField[lnAlen+9,3] = 8
  *laFilField[lnAlen+9,4] = 9
  *laFilField[lnAlen+10,1] = 'nWIPUsed'
  *laFilField[lnAlen+10,2] = 'N'
  *laFilField[lnAlen+10,3] = 8
  *laFilField[lnAlen+10,4] = 0

  laFilField[lnAlen+9,1] = 'nRequired'
  laFilField[lnAlen+9,2] = 'N'
  laFilField[lnAlen+9,3] = 12
  laFilField[lnAlen+9,4] = 3
  laFilField[lnAlen+10,1] = 'nWIPUsed'
  laFilField[lnAlen+10,2] = 'N'
  laFilField[lnAlen+10,3] = 12
  laFilField[lnAlen+10,4] = 3
  *E301089,1 (End)

  laFilField[lnAlen+11,1] = 'nYeild'
  laFilField[lnAlen+11,2] = 'N'
  laFilField[lnAlen+11,3] = 7
  laFilField[lnAlen+11,4] = 3

  DECLARE laIndex[3,2]
  laIndex[1,1] = IIF(lcChoice='C','CSELECT+CDIVISION+SEASON+STYLE+DYELOT',;
    'CSELECT+CDIVISION+CPURCODE+CSTYGRADE+STYLE+DYELOT')
  laIndex[1,2] = 'TICKET'
  laIndex[2,1] = 'cSortExp+ORDER+STR(LINENO,6)'
  laIndex[2,2] = '(lcOrdLine)'
  laIndex[3,1] = 'cSelect+Fabric+cFabClr+cSortExp+Order+Store+Group'
  laIndex[3,2] = 'Groups'
  =gfCrtTmp(lcOrdLine,@lafilfield,@laIndex)
  SET ORDER TO TAG (lcOrdLine) IN (lcOrdLine)

  DECLARE laIndex[2,2]
  laIndex[1,1] = IIF(lcChoice='C','cSelect+cDivision+Season+Style+Dyelot',;
    'cSelect+cDivision+cPurCode+cStyGrade+Style+Dyelot')
  laIndex[1,2] = 'TICKET'
 
  *B605517,1 KHM 02/13/2002 (Begin) Changing the index by changing the
  *B605517,1                orders between dyelot and order
  laIndex[2,1] = 'Fabric+cFabClr+cWareCode+Dyelot+Order+STR(LineNO,6)'
  laIndex[2,1] = 'Fabric+cFabClr+cWareCode+Order+Dyelot+STR(LineNO,6)'
  *B605517,1 KHM 02/13/2002 (End)
 
  laIndex[2,2] = 'Fabrics'

  =gfCrtTmp(lcTmpOrd,@lafilfield,@laIndex)

  *E301089,1 Consider decimal point in fabric onhand
  *=gfCrtTmp(lcFabrics,[(Fabric C(7),Color C(6),cWareCode C(6),Dyelot C(10),;
  cPriority C(4),cNewPrior C(4),OnHand N(8),nRequired N(8,0),nAllocated N(8,0),nUnAllWIP N(8,0),nWIPUsed N(8,0))],;
  [Fabric+Color+cPriority+Dyelot+cWareCode],lcFabrics)
*=gfCrtTmp(lcStyUnAll,[(Style C(19),Dyelot C(10),nUnAllWIP N(8,0),nWIPUsed N(8,0))],;
[Style+Dyelot],lcStyUnAll)
*=gfCrtTmp(lcOrdGroup,[(Fabric C(7),Color C(6),Order C(6),;
STORE C(8), GROUP C(1),nRequired N(8))],;
[Fabric+Color+Order+Store+Group],lcOrdGroup)

*B802606,1 RAMY Change this line to add a new index to the temp file
*B802606,1      (lcFabrics) [start]
*=gfCrtTmp(lcFabrics,[(Fabric C(7),Color C(6),cWareCode C(6),Dyelot C(10),;
cPriority C(4),cNewPrior C(4),OnHand N(12,3),nRequired N(12,3),nAllocated N(12,3),nUnAllWIP N(12,3),nWIPUsed N(12,3))],;
[Fabric+Color+cPriority+Dyelot+cWareCode],lcFabrics)

DIMENSION laFabNdx[2,2]
laFabNdx[1,1] = 'Fabric+Color+cPriority+Dyelot+cWareCode'
laFabNdx[1,2] = 'lcFabrics'
laFabNdx[2,1] = 'Fabric+Color+STR(OnHand,12,3)+cPriority+Dyelot+cWareCode'
laFabNdx[2,2] = 'OnHand'

=gfCrtTmp(lcFabrics,[(Fabric C(7),Color C(6),cWareCode C(6),Dyelot C(10),;
  cPriority C(4),cNewPrior C(4),OnHand N(12,3),nRequired N(12,3),nAllocated N(12,3),nUnAllWIP N(12,3),nWIPUsed N(12,3))],;
  @laFabNdx)

*B802606,1 RAMY Change this line to add a new index to the temp file [end]

=gfCrtTmp(lcStyUnAll,[(Style C(19),Dyelot C(10),nUnAllWIP N(12,3),nWIPUsed N(12,3))],;
  [Style+Dyelot],lcStyUnAll)
*B602471,1 Force allocate all order lines group for one dyelot
=gfCrtTmp(lcOrdGroup,[(Fabric C(7),Color C(6),Order C(6),;
  STORE C(8), GROUP C(1),nRequired N(12,3),nUnAllWIP N(12,3),CWareCode C(6),Dyelot C(10))],;
  [Fabric+Color+Order+Store+Group],lcOrdGroup)
*E301089,1 (End)
*B602471,1 (End)
ENDIF

DO CASE
CASE lcChoice = 'P'
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
  *E301077,57 Inhance openning files to speed up transaction
  *SELECT POSLN
  *=AFIELDS(lafilfield)
  *lnAlen = ALEN(lafilfield,1)
  *DIMENSION lafilfield[lnAlen+7,4]
  *lafilfield[lnAlen+1,1] = 'CDIVISION'
  *lafilfield[lnAlen+1,2] = 'C'
  *lafilfield[lnAlen+1,3] = 6
  *lafilfield[lnAlen+1,4] = 0
  *lafilfield[lnAlen+2,1] = 'cPurCode'
  *lafilfield[lnAlen+2,2] = 'C'
  *lafilfield[lnAlen+2,3] = 6
  *lafilfield[lnAlen+2,4] = 0
  *lafilfield[lnAlen+3,1] = 'nSteps'
  *lafilfield[lnAlen+3,2] = 'N'
  *lafilfield[lnAlen+3,3] = 2
  *lafilfield[lnAlen+3,4] = 0
  *laFilField[lnAlen+4,1] = 'Fabric'
  *laFilField[lnAlen+4,2] = 'C'
  *laFilField[lnAlen+4,3] = 7
  *laFilField[lnAlen+4,4] = 0
  *laFilField[lnAlen+5,1] = 'cFabClr'
  *laFilField[lnAlen+5,2] = 'C'
  *laFilField[lnAlen+5,3] = 6
  *laFilField[lnAlen+5,4] = 0
  *laFilField[lnAlen+6,1] = 'cFabWare'
  *laFilField[lnAlen+6,2] = 'C'
  *laFilField[lnAlen+6,3] = 6
  *laFilField[lnAlen+6,4] = 0
  *laFilField[lnAlen+7,1] = 'nYeild'
  *laFilField[lnAlen+7,2] = 'N'
  *laFilField[lnAlen+7,3] = 7
  *laFilField[lnAlen+7,4] = 3
  *=gfCrtTmp(lcPOLine,@lafilfield,[PO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+DYELOT],[CSTYCLR])
  *E301077,57 (End)

CASE lcChoice = 'L'
  SELECT STYLE
  =AFIELDS(lafilfield)
  lnAlen = ALEN(lafilfield,1)
  DIMENSION lafilfield[lnAlen+1,4]
  lafilfield[lnAlen+1,1] = 'cSelect'
  lafilfield[lnAlen+1,2] = 'C'
  lafilfield[lnAlen+1,3] = 1
  lafilfield[lnAlen+1,4] = 0

  DECLARE laIndex[2,2]
  laIndex[1,1] = 'CSELECT+CDIVISION+SEASON+STYLE'
  laIndex[1,2] = 'TICKET'
  laIndex[2,1] = 'STYLE'
  laIndex[2,2] = lcStyTmp
  =gfCrtTmp(lcStyTmp,@lafilfield,@laIndex)
  SET ORDER TO TAG (lcStyTmp) IN (lcStyTmp)
ENDCASE

*!*************************************************************
*! Name      : lfCrtDTemp
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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

IF lcChoice $ 'LC' AND !USED(lcCuttktL)
  =gfOpenFile(gcDataDir+'CUTTKTL',gcDataDir+'CUTTKTL','SH')
  =AFIELDS(lafilfield)
  lnAlen = ALEN(lafilfield,1)
  DIMENSION lafilfield[lnAlen+7,4]
  lafilfield[lnAlen+1,1] = 'CDIVISION'
  lafilfield[lnAlen+1,2] = 'C'
  lafilfield[lnAlen+1,3] = 6
  lafilfield[lnAlen+1,4] = 0
  lafilfield[lnAlen+2,1] = 'SEASON'
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
  =gfCrtTmp(lcCuttktL,@lafilfield,[CUTTKT+CDIVISION+SEASON+STYLE+DYELOT],[CSTYCLR])
ENDIF
IF lcChoice <> 'L' AND !USED(lcCutPick)
  =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTPICK','SH')
  =AFIELDS(lafilfield)
  lnAlen = ALEN(lafilfield,1)
  DIMENSION lafilfield[lnAlen+7,4]
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
  laFilField[lnAlen+4,1] = 'cStyGrade'
  laFilField[lnAlen+4,2] = 'C'
  laFilField[lnAlen+4,3] = 1
  laFilField[lnAlen+4,4] = 0
  lafilfield[lnAlen+5,1] = 'CWARECODE'
  lafilfield[lnAlen+5,2] = 'C'
  lafilfield[lnAlen+5,3] = 6
  lafilfield[lnAlen+5,4] = 0
  laFilField[lnAlen+6,1] = 'Dyelot'
  laFilField[lnAlen+6,2] = 'C'
  laFilField[lnAlen+6,3] = 10
  laFilField[lnAlen+6,4] = 0
  lafilfield[lnAlen+7,1] = 'nSteps'
  lafilfield[lnAlen+7,2] = 'N'
  lafilfield[lnAlen+7,3] = 2
  lafilfield[lnAlen+7,4] = 0
  DECLARE laIndex[1,2]
  laIndex[1,1] = IIF(lcChoice='C',;
    'CTKTNO+CDIVISION+SEASON+STYLE+CWARECODE+DYELOT',;
    'CTKTNO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+CWARECODE+DYELOT')
  laIndex[1,2] = 'CSTYCLR'
  =gfCrtTmp(lcCutPick,@lafilfield,@laIndex)
ENDIF
IF lcChoice = 'P' AND !USED(lcPOLine)
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

*!*************************************************************
*! Name      : lfUpdCtPik
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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
PARAMETERS lcOrdFile
PRIVATE lnAlias,lcStyOrd,lcStyMaj,lcDivision,lcSeason,lcStyle,lcDyelot,lnCost1,lnCost2,;
  lnCost3,lnCost4,lnCost5,lnOrder,lnAllocat,lnCutQty1,lnCutQty2,lnCutQty3,;
  lnCutQty4,lnCutQty5,lnCutQty6,lnCutQty7,lnCutQty8
PRIVATE lnOrdQty1,lnOrdQty2,lnOrdQty3,lnOrdQty4,lnOrdQty5,lnOrdQty6,lnOrdQty7,lnOrdQty8

WAIT 'Generating Cutting Tickets. Please standby....' WINDOW NOWAIT

lnAlias = SELECT()
SELECT STYLE
lcStyOrd = TAG()
SET ORDER TO TAG STYLE
SELECT (lcOrdFile)
lcOrdTag = TAG()
SET ORDER TO TAG TICKET
=IIF(llRPUnAWip OR llRPUnAWip,lfUnAllCt(lcOrdFile),.T.)
SELECT (lcOrdFile)
=SEEK("�")
DO WHILE cSelect+cDivision+Season+STYLE+Dyelot="�"
  lcStyMaj   = SUBSTR(STYLE,1,lnMajorLen)
  lcDivision = cDivision
  lcSeason   = Season
  =SEEK(lcStyMaj,'Style')
  SELECT (lcCutTktH)
  IF !SEEK(SPACE(6)+lcDivision+lcSeason+lcStyMaj)
    APPEND BLANK
    *B602437,1 Update Multi warehouse field
    *B802331,1 AMM Update with the right ware house
    *REPLACE cWareCode  WITH lcWareCode    ,;
      STYLE      WITH lcStyMaj      ,;
      DESC       WITH Style.Desc    ,;
      SEASON     WITH lcSeason      ,;
      CDIVISION  WITH lcDivision    ,;
      PATTERN    WITH Style.Pattern ,;
      STATUS     WITH 'H'           ,;
      ENTERED    WITH gdSysDate     ,;
      COMPLETE   WITH ENTERED+60    ,;
      lMultiWare WITH (lnType=2)
    REPLACE cWareCode  WITH IIF(lnType = 2 OR !llWareHous,Style.cDefWare,lcWareCode) ,;
      STYLE      WITH lcStyMaj      ,;
      DESC       WITH Style.Desc    ,;
      SEASON     WITH lcSeason      ,;
      CDIVISION  WITH lcDivision    ,;
      PATTERN    WITH Style.Pattern ,;
      STATUS     WITH 'H'           ,;
      ENTERED    WITH gdSysDate     ,;
      COMPLETE   WITH ENTERED+60    ,;
      lMultiWare WITH (lnType=2)
   *B802331,1 AMM end
  ENDIF
  STORE 0 TO lnCost1,lnCost2,lnCost3,lnCost4,lnCost5,lnOrder,lnAllocat

  *E301289,1 WAB - Set variable Amount to 0
  *E301289,1 WAB - START
  IF llDispPric
    STORE 0 TO lnSelPrice,lnAllQty,lnRotSub,lnGrosMrgn
  ENDIF
  *E301289,1 WAB - END

  SELECT (lcOrdFile)
  DO WHILE cSelect+cDivision+Season+STYLE+Dyelot=;
      "�"+lcDivision+lcSeason+lcStyMaj
    lcStyle  = STYLE
    lcDyelot = Dyelot
    lcFabric = Fabric
    lcFabClr = cFabClr
    lcFabWare= cFabWare
    lnYeild  = nYeild
    =SEEK(lcStyle,'Style')
    STORE 0 TO lnCutQty1,lnCutQty2,lnCutQty3,lnCutQty4,lnCutQty5,lnCutQty6,lnCutQty7,lnCutQty8
    STORE 0 TO lnOrdQty1,lnOrdQty2,lnOrdQty3,lnOrdQty4,lnOrdQty5,lnOrdQty6,lnOrdQty7,lnOrdQty8
    SCAN REST WHILE cSelect+cDivision+Season+STYLE+Dyelot = ;
        "�"+lcDivision+lcSeason+lcStyle+lcDyelot ;
        FOR   IIF(llRpBOPFab,nRequired-nWIPUsed > 0,.T.)

      SELECT (lcCutPick)
      APPEND BLANK
      REPLACE TRANCD    WITH '1'         ,;
        CDIVISION WITH lcDivision  ,;
        SEASON    WITH lcSeason ,;
        ORDER     WITH &lcOrdFile..ORDER,;
        CORDLINE  WITH STR(&lcOrdFile..LINENO,6),;
        STYLE     WITH lcStyle ,;
        DYelot    WITH lcDyelot ,;
        cWareCode WITH IIF(lnType = 2 OR !llWareHous,Style.cDefWare,lcWareCode) ,;
        QTY1      WITH INT(MIN(laPercet[1],100)/100*(&lcOrdFile..QTY1-&lcOrdFile..CUT1)),;
        QTY2      WITH INT(MIN(laPercet[2],100)/100*(&lcOrdFile..QTY2-&lcOrdFile..CUT2)),;
        QTY3      WITH INT(MIN(laPercet[3],100)/100*(&lcOrdFile..QTY3-&lcOrdFile..CUT3)),;
        QTY4      WITH INT(MIN(laPercet[4],100)/100*(&lcOrdFile..QTY4-&lcOrdFile..CUT4)),;
        QTY5      WITH INT(MIN(laPercet[5],100)/100*(&lcOrdFile..QTY5-&lcOrdFile..CUT5)),;
        QTY6      WITH INT(MIN(laPercet[6],100)/100*(&lcOrdFile..QTY6-&lcOrdFile..CUT6)),;
        QTY7      WITH INT(MIN(laPercet[7],100)/100*(&lcOrdFile..QTY7-&lcOrdFile..CUT7)),;
        QTY8      WITH INT(MIN(laPercet[8],100)/100*(&lcOrdFile..QTY8-&lcOrdFile..CUT8)),;
        TOTQTY    WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8

      lnCutQty1 = lnCutQty1+INT(laPercet[1]/100 * (&lcOrdFile..QTY1-&lcOrdFile..CUT1))
      lnCutQty2 = lnCutQty2+INT(laPercet[2]/100 * (&lcOrdFile..QTY2-&lcOrdFile..CUT2))
      lnCutQty3 = lnCutQty3+INT(laPercet[3]/100 * (&lcOrdFile..QTY3-&lcOrdFile..CUT3))
      lnCutQty4 = lnCutQty4+INT(laPercet[4]/100 * (&lcOrdFile..QTY4-&lcOrdFile..CUT4))
      lnCutQty5 = lnCutQty5+INT(laPercet[5]/100 * (&lcOrdFile..QTY5-&lcOrdFile..CUT5))
      lnCutQty6 = lnCutQty6+INT(laPercet[6]/100 * (&lcOrdFile..QTY6-&lcOrdFile..CUT6))
      lnCutQty7 = lnCutQty7+INT(laPercet[7]/100 * (&lcOrdFile..QTY7-&lcOrdFile..CUT7))
      lnCutQty8 = lnCutQty8+INT(laPercet[8]/100 * (&lcOrdFile..QTY8-&lcOrdFile..CUT8))
      lnOrdQty1 = lnOrdQty1 + &lcCutPick..Qty1
      lnOrdQty2 = lnOrdQty2 + &lcCutPick..Qty2
      lnOrdQty3 = lnOrdQty3 + &lcCutPick..Qty3
      lnOrdQty4 = lnOrdQty4 + &lcCutPick..Qty4
      lnOrdQty5 = lnOrdQty5 + &lcCutPick..Qty5
      lnOrdQty6 = lnOrdQty6 + &lcCutPick..Qty6
      lnOrdQty7 = lnOrdQty7 + &lcCutPick..Qty7
      lnOrdQty8 = lnOrdQty8 + &lcCutPick..Qty8
      IF TotQty <= 0
        DELETE
      ENDIF
      *E301289,1 WAB - Calculate the total qty of the qty that will be generate in the po
      *E301289,1 WAB - For one Order
      *E301289,1 WAB - and caluclate total qty for all Orders and the total price
      *E301289,1 WAB - START

      IF llDispPric
        *        STORE 0 TO lnTotQty,lnSelPrice,lnAllQty,lnRotSub,lnGrosMrgn
        STORE 0 TO lnTotQty
        FOR lnI = 1 TO 8
          lcI = STR(lnI,1)
          lnTotQty = lnTotQty + INT(laPercet[lnI]/100 * (&lcOrdFile..QTY&lcI-&lcOrdFile..CUT&lcI))
        ENDFOR
        lnAllQty = lnAllQty + lnTotQty
        lnselPrice = lnSelPrice + ( lnTotQty*&lcOrdFile..Price )
      ENDIF
      *E301289,1 WAB - END


    ENDSCAN
    *E301289,1 WAB - Calculate the Average Selling Price
    *E301289,1 WAB - Start

    IF llDispPric
      lnSelPrice = lnSelPrice / lnAllQty
    ENDIF

    *E301289,1 WAB - END

    IF lnCutQty1+lnCutQty2+lnCutQty3+lnCutQty4+lnCutQty5+lnCutQty6+lnCutQty7+lnCutQty8 > 0
      SELECT (lcCutTktL)
      IF !SEEK(SPACE(6)+lcDivision+lcSeason+lcStyle+lcDyelot)
        SELECT (lcCutTktH)
        REPLACE LastLine WITH LastLine+1
        SELECT (lcCutTktL)
        APPEND BLANK
        *B802331,1 AMM Update with the right warehouse
        *REPLACE STYLE     WITH lcStyle ,;
          DYELOT    WITH lcDyelot,;
          TRANCD    WITH '1'     ,;
          LINENO    WITH &lcCutTktH..LastLine,;
          CDIVISION WITH lcDivision,;
          SEASON    WITH lcSeason  ,;
          Fabric    WITH lcFabric  ,;
          cFabClr   WITH lcFabClr  ,;
          cFabWare  WITH lcFabWare ,;
          nYeild    WITH lnYeild   ,;
          cWareCode WITH lcWareCode

        REPLACE STYLE     WITH lcStyle ,;
          DYELOT    WITH lcDyelot,;
          TRANCD    WITH '1'     ,;
          LINENO    WITH &lcCutTktH..LastLine,;
          CDIVISION WITH lcDivision,;
          SEASON    WITH lcSeason  ,;
          Fabric    WITH lcFabric  ,;
          cFabClr   WITH lcFabClr  ,;
          cFabWare  WITH lcFabWare ,;
          nYeild    WITH lnYeild   ,;
          cWareCode WITH IIF(lnType = 2 OR !llWareHous,Style.cDefWare,lcWareCode)
        *B802331,1 AMM end

      ENDIF
      =RLOCK()
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
        TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+QTy7+Qty8 ,;
        TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
      REPLACE nCost1 WITH TotQty * Style.nMCost1 ,;
        nCost2 WITH TotQty * Style.nMCost2 ,;
        nCost3 WITH TotQty * Style.nMCost3 ,;
        nCost4 WITH TotQty * Style.nMCost4 ,;
        nCost5 WITH TotQty * Style.nMCost5

      *E301289,1 WAB - Calculate the Gross Margin [devide the diferent
      *E301289,1 WAB - between price and cost  [by cost or by selling price]
      *E301289,1 WAB - depense on llstymark (setup)
      *E301289,1 WAB - & replace avr. price & gors margin in temp file
      *E301289,1 WAB - START
      IF llDispPric
        =SEEK(lcStyle,'Style')
        lnRotSub   = IIF(llStyMark,Style.TotCost,lnSelPrice)
        lnGrosMrgn = IIF(lnRotSub=0,0,((lnSelPrice - Style.TotCost)/lnRotSub)*100)
        REPLACE nSelPrice WITH lnSelPrice,;
          nGrosMrgn WITH lnGrosMrgn
        STORE 0 TO lnSelPrice,lnAllQty
      ENDIF
      *E301289,1 WAB - END
      UNLOCK
      lnCost1   = lnCost1 + nCost1
      lnCost2   = lnCost2 + nCost2
      lnCost3   = lnCost3 + nCost3
      lnCost4   = lnCost4 + nCost4
      lnCost5   = lnCost5 + nCost5
      lnOrder   = lnOrder + TotOrd
      lnAllocat = lnAllocat + TotQty
    ENDIF
    SELECT (lcOrdFile)
  ENDDO
  SELECT (lcCutTktH)
  =RLOCK()
  REPLACE nEst_Cost1 WITH lnCost1 ,;
    nEst_Cost2 WITH lnCost2 ,;
    nEst_Cost3 WITH lnCost3 ,;
    nEst_Cost4 WITH lnCost4 ,;
    nEst_Cost5 WITH lnCost5 ,;
    Pcs_Bud    WITH lnAllocat,;
    TotOrd     WITH lnOrder
  UNLOCK
  SELECT (lcOrdFile)
ENDDO
SET ORDER TO TAG (lcStyOrd) IN STYLE
SET ORDER TO TAG (lcOrdTag) IN (lcOrdFile)
SELECT (lnAlias)
WAIT CLEAR

*!*************************************************************
*! Name      : lfUpdPOPik
*! Developer : Wael Aly Mohamed
*! Date      : 03/22/1998
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
PARAMETERS lcOrdFile
PRIVATE lnAlias,lcStyOrd,lcPUntSin,lcDUntSin,lcPExSign,lcDExSign,lcDivision,;
  lcPurCode,lcStyGrade,lnTCost1,lnTCost2,lnTCost3,lnTCost4,lnTCost5,;
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
=IIF(llRPUnAWip OR llRPUnAWip,lfUnAllPO(lcOrdFile),.T.)
SELECT (lcOrdFile)
=SEEK("�")
DO WHILE cSelect+cDivision +cPurCode+cStyGrade+STYLE+Dyelot="�"

  lcDivision = cDivision
  lcPurCode  = cPurCode
  lcStyGrade = cStyGrade

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
  ENDIF
  STORE 0 TO lnTCost1,lnTCost2,lnTCost3,lnTCost4,lnTCost5,lnOrder,lnAllocat,;
             lnTFCost1,lnTFCost2,lnTFCost3,lnTFCost4,lnTFCost5

  *E301289,1 WAB - Set variable Amount to 0
  *E301289,1 WAB - START
  IF llDispPric
    STORE 0 TO lnSelPrice,lnAllQty,lnRotSub,lnGrosMrgn
  ENDIF
  *E301289,1 WAB - END

  SELECT (lcOrdFile)
  DO WHILE cSelect+cDivision +cPurCode+cStyGrade+STYLE+Dyelot = ;
      "�"+lcDivision+lcPurCode+lcStyGrade
    lcStyle  = STYLE
    lcDyelot = Dyelot
    lcFabric = Fabric
    lcFabClr = cFabClr
    lcFabWare= cFabWare
    lnYeild  = nYeild
    STORE 0 TO lnCutQty1,lnCutQty2,lnCutQty3,lnCutQty4,lnCutQty5,lnCutQty6,lnCutQty7,lnCutQty8
    STORE 0 TO lnOrdQty1,lnOrdQty2,lnOrdQty3,lnOrdQty4,lnOrdQty5,lnOrdQty6,lnOrdQty7,lnOrdQty8


    SCAN REST WHILE cSelect+cDivision +cPurCode+cStyGrade+STYLE+Dyelot = ;
        "�"+lcDivision+lcPurCode+lcStyGrade+lcStyle+lcDyelot ;
        FOR   IIF(llRpBOPFab,nRequired-nWIPUsed > 0,.T.)

      SELECT(lcCutPick)
      APPEND BLANK
      REPLACE TRANCD    WITH '2',;
              ORDER     WITH &lcOrdFile..ORDER,;
              CORDLINE  WITH STR(&lcOrdFile..LINENO,6),;
              STYLE     WITH lcSTYLE ,;
              Dyelot    WITH lcDyelot,;
              cWareCode WITH lcWareCode ,;
              CDIVISION WITH lcDivision ,;
              cPurCode  WITH lcPurCode ,;
              cStyGrade WITH lcStyGrade

      REPLACE QTY1      WITH INT(MIN(laPercet[1],100)/100*(&lcOrdFile..QTY1-&lcOrdFile..CUT1)),;
              QTY2      WITH INT(MIN(laPercet[2],100)/100*(&lcOrdFile..QTY2-&lcOrdFile..CUT2)),;
              QTY3      WITH INT(MIN(laPercet[3],100)/100*(&lcOrdFile..QTY3-&lcOrdFile..CUT3)),;
              QTY4      WITH INT(MIN(laPercet[4],100)/100*(&lcOrdFile..QTY4-&lcOrdFile..CUT4)),;
              QTY5      WITH INT(MIN(laPercet[5],100)/100*(&lcOrdFile..QTY5-&lcOrdFile..CUT5)),;
              QTY6      WITH INT(MIN(laPercet[6],100)/100*(&lcOrdFile..QTY6-&lcOrdFile..CUT6)),;
              QTY7      WITH INT(MIN(laPercet[7],100)/100*(&lcOrdFile..QTY7-&lcOrdFile..CUT7)),;
              QTY8      WITH INT(MIN(laPercet[8],100)/100*(&lcOrdFile..QTY8-&lcOrdFile..CUT8)),;
              TOTQTY    WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8

      lnCutQty1 = lnCutQty1+INT(laPercet[1]/100 * (&lcOrdFile..QTY1-&lcOrdFile..CUT1))
      lnCutQty2 = lnCutQty2+INT(laPercet[2]/100 * (&lcOrdFile..QTY2-&lcOrdFile..CUT2))
      lnCutQty3 = lnCutQty3+INT(laPercet[3]/100 * (&lcOrdFile..QTY3-&lcOrdFile..CUT3))
      lnCutQty4 = lnCutQty4+INT(laPercet[4]/100 * (&lcOrdFile..QTY4-&lcOrdFile..CUT4))
      lnCutQty5 = lnCutQty5+INT(laPercet[5]/100 * (&lcOrdFile..QTY5-&lcOrdFile..CUT5))
      lnCutQty6 = lnCutQty6+INT(laPercet[6]/100 * (&lcOrdFile..QTY6-&lcOrdFile..CUT6))
      lnCutQty7 = lnCutQty7+INT(laPercet[7]/100 * (&lcOrdFile..QTY7-&lcOrdFile..CUT7))
      lnCutQty8 = lnCutQty8+INT(laPercet[8]/100 * (&lcOrdFile..QTY8-&lcOrdFile..CUT8))
      lnOrdQty1 = lnOrdQty1 + &lcCutPick..Qty1
      lnOrdQty2 = lnOrdQty2 + &lcCutPick..Qty2
      lnOrdQty3 = lnOrdQty3 + &lcCutPick..Qty3
      lnOrdQty4 = lnOrdQty4 + &lcCutPick..Qty4
      lnOrdQty5 = lnOrdQty5 + &lcCutPick..Qty5
      lnOrdQty6 = lnOrdQty6 + &lcCutPick..Qty6
      lnOrdQty7 = lnOrdQty7 + &lcCutPick..Qty7
      lnOrdQty8 = lnOrdQty8 + &lcCutPick..Qty8


      *E301289,1 WAB - Calculate the total qty of the qty that will be generate in the po
      *E301289,1 WAB - For one Order
      *E301289,1 WAB - and caluclate total qty for all Orders and the total price
      *E301289,1 WAB - START

      IF llDispPric
        STORE 0 TO lnTotQty
        FOR lnI = 1 TO 8
          lcI = STR(lnI,1)
          lnTotQty = lnTotQty + INT(laPercet[lnI]/100 * (&lcOrdFile..QTY&lcI-&lcOrdFile..CUT&lcI))
        ENDFOR
        lnAllQty = lnAllQty + lnTotQty
        lnselPrice = lnSelPrice + ( lnTotQty*&lcOrdFile..Price )
      ENDIF
      *E301289,1 WAB - END

    ENDSCAN

    *E301289,1 WAB - Calculate the Average Selling Price
    *E301289,1 WAB - Start
    IF llDispPric
      lnSelPrice = lnSelPrice / lnAllQty
    ENDIF
    *E301289,1 WAB - END

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
      *E301289,1 WAB - Calculate the Gross Margin [devide the diferent
      *E301289,1 WAB - between price and cost  [by cost or by selling price]
      *E301289,1 WAB - depense on llstymark (setup)
      *E301289,1 WAB - & replace avr. price & gors margin in temp file
      *E301289,1 WAB - START
      IF llDispPric
        lnTotCost   = (nEcost1+nEcost2+nEcost3+nEcost4+nEcost5)/TOTQTY
        lnRotSub    = IIF(llStyMark,lnTotCost,lnSelPrice)
        lnGrosMrgn  = IIF(lnRotSub=0,0,((lnSelPrice - lnTotCost)/lnRotSub)*100)
        REPLACE nSelPrice WITH lnSelPrice ,;
          nGrosMrgn WITH lnGrosMrgn
        STORE 0 TO lnSelPrice,lnAllQty
      ENDIF
      *E301289,1 WAB - END
      lnOrder   = lnOrder   + TOTORD
      lnAllocat = lnAllocat + TOTQTY
    ENDIF
    SELECT (lcOrdFile)
  ENDDO
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
*! Name      : lfGetFab
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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
PRIVATE lnAlias

WAIT 'Computing primary fabrics requirements.' WINDOW NOWAIT

*E301077,57 Inhance openning files to speed up transaction
=IIF(llFDyelot,gfOpenFile(gcDataDir+'DYE_REL',gcDataDir+'DYE_REL','SH'),.T.)
=gfOpenFile(gcDataDir+'FABRIC',gcDataDir+'FABRIC','SH')
=gfOpenFile(gcDataDir+'FABDYE',gcDataDir+'FABDYE','SH')
=gfOpenFile(gcDataDir+'BOM',gcDataDir+'BOM','SH')
*E301077,57 (End)

lnAlias = SELECT()
IF llRPUnAWip AND lcChoice = 'P'
  =gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLNS','SH')
  SELECT POSLN
  SET RELATION TO CSTYTYPE+PO INTO POSHDR
ENDIF
IF llRPUnAWip AND lcChoice = 'C'
  =gfOpenFile(gcDataDir+'CUTTKTL',gcDataDir+'CUTTKTLS','SH')
  SELECT CUTTKTL
  SET RELATION TO CUTTKT INTO CUTTKTH
ENDIF
SET ORDER TO TAG STYLE IN STYLE
SELECT (lcOrdLine)
SET ORDER TO TAG 'TICKET'
=SEEK("�")
SCAN REST WHILE cSelect = "�"
  *B802224,1 Fix computing fabric requirements.
  lnReqQty = Totqty
  *B802224,1 (End)


  lcStyle  = STYLE
  =SEEK(lcStyle,'Style')
  SELECT BOM
  =SEEK(SUBSTR(lcStyle,1,lnMajorLen))
  LOCATE REST WHILE cItmMajor+Typ+cItmMask+MfgCode+Item+Iclr = ;
    SUBSTR(lcStyle,1,lnMajorLen)     ;
    FOR   (cCatgTyp='F' OR (cCatgTyp='T' AND trim_invt)) AND ;
    Item = PADR(Style.Fabric,19) AND ;
    LIKE(STRTRAN(cItmMask,'*','?'),lcStyle) AND ;
    IIF(!EMPTY(MSIZES),ATCLINE(Style.Scale+'~',MSIZES)<>0,.T.) AND ;
    IIF(!EMPTY(MSZCROSREF),ATCLINE(Style.Scale+',',MSZCROSREF)<>0,.T.)
  lcFabClr = IIF(IClr='******',SUBSTR(lcStyle,lnColorStr,lnColorLen),IClr)
  *B802224,1 Fix computing fabric requirements.
  *STORE 0 TO lnReqQty,lnYardage
  STORE 0 TO lnYardage
  *B802224,1 (End)
  SCAN REST WHILE cItmMajor+Typ+cItmMask+MfgCode+Item+Iclr = ;
      SUBSTR(lcStyle,1,lnMajorLen)     ;
      FOR   (cCatgTyp='F' OR (cCatgTyp='T' AND trim_invt)) AND ;
      Item = PADR(Style.Fabric,19) AND ;
      IIF(IClr='******',.T.,IClr=lcFabClr)
    IF !LIKE(STRTRAN(cItmMask,'*','?'),lcStyle) .OR. ;
        (!EMPTY(MSIZES) .AND. ATCLINE(&lcOrdLine..Scale+'~',MSIZES)=0) .OR. ;
        (!EMPTY(MSZCROSREF) .AND. ATCLINE(&lcOrdLine..Scale+',',MSZCROSREF)=0)
      LOOP
    ENDIF
    lcSizes = ''
    IF !EMPTY(MSIZES)
      lcSizes = SUBSTR(MLINE(MSIZES,ATCLINE(&lcOrdLine..Scale+'~',MSIZES)),5)
    ENDIF
    lnQuantity = 0
    FOR lnCount = 1 TO 8
      *B602467,1 Allocate order open quantity instead of ordered quantity
      *lnQuantity = lnQuantity + IIF(EMPTY(lcSizes) OR (STR(lnCount,1) $ lcSizes),;
      EVAL(lcOrdLine+'.Qty'+STR(lnCount,1)),0)
    lnQuantity = lnQuantity + IIF(EMPTY(lcSizes) OR (STR(lnCount,1) $ lcSizes),;
      EVAL(lcOrdLine+'.Qty'+STR(lnCount,1))-EVAL(lcOrdLine+'.Cut'+STR(lnCount,1)),0)
    *B602467,1 (End)
  ENDFOR
  *B802224,1 Commented out
  *lnReqQty  = lnReqQty  + lnQuantity
  *B802224,1 (End)

  lnYardage = lnYardage + lnQuantity*nBomTotQty
ENDSCAN
lnYeild = IIF(lnReqQty=0,0,lnYardage/lnReqQty)
SELECT (lcOrdGroup)
IF !SEEK(Style.Fabric+lcFabClr+&lcOrdLine..Order+&lcOrdLine..Store+&lcOrdLine..Group)
  APPEND BLANK
  REPLACE Fabric WITH Style.Fabric      ,;
    COLOR  WITH lcFabClr          ,;
    ORDER  WITH &lcOrdLine..Order ,;
    STORE  WITH &lcOrdLine..Store ,;
    GROUP  WITH &lcOrdLine..Group
ENDIF
REPLACE nRequired WITH nRequired + lnYardage
SELECT (lcOrdLine)
=RLOCK()
REPLACE Fabric    WITH Style.Fabric ,;
  cFabClr   WITH lcFabClr     ,;
  nRequired WITH lnYardage    ,;
  nYeild    WITH lnYeild
UNLOCK
lcFabClr = Style.Fabric+lcFabClr
SELECT (lcFabrics)
IF !SEEK(lcFabClr)
  SELECT FABDYE
  =SEEK(lcFabClr)
  lnPriority = 0
  SCAN REST WHILE Fabric+COLOR+cWarecode+Dyelot = lcFabClr ;
      FOR   IIF(llFDyelot,!EMPTY(Dyelot),.T.)
    SCATTER MEMVAR FIELDS Fabric,COLOR,cWareCode,Dyelot,OnHand
    IF llFDyelot AND SEEK(m.Fabric+m.Color+m.Dyelot,'Dye_Rel')
      m.cPriority = Dye_Rel.cDye_Seq
    ELSE
      lnPriority  = lnPriority + 1
      m.cPriority = lnPriority
    ENDIF
    m.nUnAllWIP = 0
    IF llRPUnAWip AND lcChoice = 'P'
      SELECT POSLN
      =SEEK(lcStyle)
      SUM REST TotQty-TotOrd TO m.nUnAllWIP ;
        WHILE STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = lcStyle ;
        FOR TRANCD='1' AND !INLIST(POSHDR.STATUS,'X','C') AND Dyelot=m.Dyelot
    ENDIF
    IF llRPUnAWip AND lcChoice = 'C'
      SELECT CUTTKTL
      =SEEK(lcStyle)
      SUM REST TotQty-TotOrd TO m.nUnAllWIP ;
        WHILE STYLE+CUTTKT+TRANCD = lcStyle ;
        FOR TRANCD = '1' AND !INLIST(CUTTKTH.STATUS,'X','C') AND Dyelot=m.Dyelot
    ENDIF
    m.nUnAllWIP = m.nUnAllWIP*lnYeild
    SELECT (lcFabrics)
    APPEND BLANK
    GATHER MEMVAR FIELDS Fabric,COLOR,cWareCode,Dyelot,OnHand,cPriority,nUnAllWIP
    SELECT (lcStyUnAll)
    IF !SEEK(lcStyle+m.Dyelot)
      INSERT INTO (lcStyUnAll) (STYLE,Dyelot) VALUES (lcStyle,m.Dyelot)
    ENDIF
    REPLACE nUnAllWIP WITH nUnAllWIP + m.nUnAllWIP
    *B602471,1 Force allocate all order lines group for one dyelot
    SELECT (lcOrdGroup)
    REPLACE nUnAllWIP WITH nUnAllWIP + m.nUnAllWIP
    *B602471,1 (End)
  ENDSCAN
ENDIF
ENDSCAN
SET ORDER TO TAG (lcOrdLine) IN (lcOrdLine)
GO TOP IN (lcOrdLine)
IF llRPUnAWip AND lcChoice = 'P'
  SELECT POSLN
  SET RELATION OFF INTO POSHDR
ENDIF
IF llRPUnAWip AND lcChoice = 'C'
  SELECT CUTTKTL
  SET RELATION OFF INTO CUTTKTH
ENDIF
SELECT (lnAlias)
WAIT CLEAR

*!*************************************************************
*! Name      : lfClrAllo
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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

SELECT (lcOrdGroup)
DELETE ALL
SELECT (lcFabrics)
DELETE ALL
SELECT (lcStyUnAll)
DELETE ALL
SELECT (lcTmpOrd)
DELETE ALL

*!*************************************************************
*! Name      : lfUnAllCt
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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
PARAMETERS lcOrdFile
PRIVATE lnAlias,lnReqQty1,lnReqQty2,lnReqQty3,lnReqQty4,lnReqQty5,lnReqQty6,;
  lnReqQty7,lnReqQty8,lnTotReq,lnOrd1,lnOrd2,lnOrd3,lnOrd4,lnOrd5,;
  lnOrd6,lnOrd7,lnOrd8,lnCut1,lnCut2,lnCut3,lnCut4,lnCut5,lnCut6,lnCut7,lnCut8,lnTotCut
lnAlias = SELECT()
SET ORDER TO TAG CUTPICK  IN CUTPICK
SET ORDER TO TAG CUTTKTLS IN CUTTKTL
SELECT CUTTKTL
SET RELATION TO CUTTKT INTO CUTTKTH ADDITIVE

SELECT (lcOrdFile)
=SEEK("�")
SCAN REST WHILE cSelect+cDivision+Season+STYLE+Dyelot="�"
  =SEEK(STYLE,'Style')
  lnReqQty1 = INT(laPercet[1]/100*(Qty1-Cut1))
  lnReqQty2 = INT(laPercet[2]/100*(Qty2-Cut2))
  lnReqQty3 = INT(laPercet[3]/100*(Qty3-Cut3))
  lnReqQty4 = INT(laPercet[4]/100*(Qty4-Cut4))
  lnReqQty5 = INT(laPercet[5]/100*(Qty5-Cut5))
  lnReqQty6 = INT(laPercet[6]/100*(Qty6-Cut6))
  lnReqQty7 = INT(laPercet[7]/100*(Qty7-Cut7))
  lnReqQty8 = INT(laPercet[8]/100*(Qty8-Cut8))
  lnTotReq  = lnReqQty1+lnReqQty2+lnReqQty3+lnReqQty4+lnReqQty5+lnReqQty6+lnReqQty7+lnReqQty8
  STORE 0 TO lnOrd1,lnOrd2,lnOrd3,lnOrd4,lnOrd5,lnOrd6,lnOrd7,lnOrd8
  IF SEEK(STYLE,'CUTTKTL')
    SELECT CUTTKTL
    SCAN REST WHILE STYLE+cuttkt+trancd = &lcOrdFile..Style ;
              FOR   IIF(llRpBOPFab,Dyelot=&lcOrdFile..Dyelot,.T.) AND TRANCD = '1' AND ;
                    TotQty > TotOrd AND lnTotReq > 0 AND !INLIST(CUTTKTH.STATUS,'X','C')

      SELECT (lcCutTktL)
      llFound=SEEK(CUTTKTL.CUTTKT)
      =SEEK(CUTTKTL.CUTTKT+&lcOrdFile..cDivision+&lcOrdFile..Season++CUTTKTL.Style+CUTTKTL.Dyelot)
      SUM REST  TotOrd TO lnAddOrd ;
          WHILE CUTTKT+CDIVISION+SEASON+STYLE+DYELOT = ;
                CUTTKTL.CUTTKT+&lcOrdFile..cDivision+&lcOrdFile..Season++CUTTKTL.Style+CUTTKTL.Dyelot
      IF CUTTKTL.TotQty <= CUTTKTL.TotOrd+lnAddOrd OR (!llFound AND CUTTKTH.RECFLAG='Y')
        LOOP
      ENDIF
      IF !llFound
        SELECT CUTTKTH
        REPLACE RECFLAG WITH 'Y'
      ENDIF
      SELECT CUTTKTL
      lnCut1 = MIN(Qty1-Ord1,lnReqQty1)
      lnCut2 = MIN(Qty2-Ord2,lnReqQty2)
      lnCut3 = MIN(Qty3-Ord3,lnReqQty3)
      lnCut4 = MIN(Qty4-Ord4,lnReqQty4)
      lnCut5 = MIN(Qty5-Ord5,lnReqQty5)
      lnCut6 = MIN(Qty6-Ord6,lnReqQty6)
      lnCut7 = MIN(Qty7-Ord7,lnReqQty7)
      lnCut8 = MIN(Qty8-Ord8,lnReqQty8)
      lnTotCut=lnCut1+lnCut2+lnCut3+lnCut4+lnCut5+lnCut6+lnCut7+lnCut8
      SELECT (lcCutPick)
      APPEND BLANK
      REPLACE cTKtNo    WITH CUTTKTL.CutTkt            ,;
              TRANCD    WITH IIF(lcChoice='C','1','2') ,;
              CDIVISION WITH &lcOrdFile..cDivision     ,;
              SEASON    WITH &lcOrdFile..Season        ,;
              ORDER     WITH &lcOrdFile..ORDER         ,;
              CORDLINE  WITH STR(&lcOrdFile..LINENO,6) ,;
              STYLE     WITH &lcOrdFile..Style         ,;
              cWareCode WITH CUTTKTL.cWareCode         ,;
              Dyelot    WITH CUTTKTL.Dyelot            ,;
              QTY1      WITH MIN(&lcOrdFile..Qty1-lnOrd1,lnCut1) ,;
              QTY2      WITH MIN(&lcOrdFile..Qty2-lnOrd2,lnCut2) ,;
              QTY3      WITH MIN(&lcOrdFile..Qty3-lnOrd3,lnCut3) ,;
              QTY4      WITH MIN(&lcOrdFile..Qty4-lnOrd4,lnCut4) ,;
              QTY5      WITH MIN(&lcOrdFile..Qty5-lnOrd5,lnCut5) ,;
              QTY6      WITH MIN(&lcOrdFile..Qty6-lnOrd6,lnCut6) ,;
              QTY7      WITH MIN(&lcOrdFile..Qty7-lnOrd7,lnCut7) ,;
              QTY8      WITH MIN(&lcOrdFile..Qty8-lnOrd8,lnCut8) ,;
              TOTQTY    WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
      lnOrd1 = lnOrd1 + QTY1
      lnOrd2 = lnOrd2 + QTY2
      lnOrd3 = lnOrd3 + QTY3
      lnOrd4 = lnOrd4 + QTY4
      lnOrd5 = lnOrd5 + QTY5
      lnOrd6 = lnOrd6 + QTY6
      lnOrd7 = lnOrd7 + QTY7
      lnOrd8 = lnOrd8 + QTY8

      SELECT (lcOrdFile)
      REPLACE Cut1   WITH Cut1 + &lcCutPick..QTY1 ,;
              Cut2   WITH Cut2 + &lcCutPick..QTY2 ,;
              Cut3   WITH Cut3 + &lcCutPick..QTY3 ,;
              Cut4   WITH Cut4 + &lcCutPick..QTY4 ,;
              Cut5   WITH Cut5 + &lcCutPick..QTY5 ,;
              Cut6   WITH Cut6 + &lcCutPick..QTY6 ,;
              Cut7   WITH Cut7 + &lcCutPick..QTY7 ,;
              Cut8   WITH Cut8 + &lcCutPick..QTY8 ,;
              TotCut WITH Cut1+Cut2+Cut3+Cut4+Cut5+Cut6+Cut7+Cut8

      SELECT (lcCutTktH)
      IF !SEEK(CUTTKTL.CutTkt+&lcOrdFile..cDivision+&lcOrdFile..Season+;
               PADR(SUBSTR(&lcOrdFile..Style,1,lnMajorLen),19))
        =SEEK(SUBSTR(&lcOrdFile..Style,1,lnMajorLen),'Style')
        APPEND BLANK
        REPLACE CutTKt     WITH CUTTKTL.CutTkt ,;
                cWareCode  WITH IIF(lnType=2 OR !llWareHous,Style.cDefWare,lcWareCode) ,;
                STYLE      WITH SUBSTR(&lcOrdFile..Style,1,lnMajorLen)      ,;
                DESC       WITH CUTTKTH.Desc    ,;
                PATTERN    WITH CUTTKTH.Pattern ,;
                STATUS     WITH CUTTKTH.STATUS  ,;
                ENTERED    WITH CUTTKTH.ENTERED ,;
                COMPLETE   WITH CUTTKTH.COMPLETE,;
                SEASON     WITH &lcOrdFile..Season  ,;
                CDIVISION  WITH &lcOrdFile..CDIVISION
      ENDIF
      SELECT (lcCutTktL)
      IF !SEEK(CUTTKTL.CutTkt+&lcOrdFile..cDivision+&lcOrdFile..Season+&lcOrdFile..Style+&lcOrdFile..Dyelot)
        APPEND BLANK
        REPLACE CutTKt    WITH CUTTKTL.CutTkt        ,;
                STYLE     WITH &lcOrdFile..Style     ,;
                DYELOT    WITH CUTTKTL.Dyelot        ,;
                LINENO    WITH &lcCutTktH..LastLine+1,;
                TRANCD    WITH '1'                   ,;
                CDIVISION WITH &lcOrdFile..cDivision ,;
                Season    WITH &lcOrdFile..Season    ,;
                nYeild    WITH &lcOrdFile..nYeild    ,;
                cWareCode WITH CUTTKTL.cWareCode
        SELECT (lcCutTktH)
        REPLACE LastLine WITH LastLine + 1
      ENDIF
      SELECT (lcCutTktL)
      REPLACE QTY1 WITH QTY1 + lnCut1 ,;
              QTY2 WITH QTY2 + lnCut2 ,;
              QTY3 WITH QTY3 + lnCut3 ,;
              QTY4 WITH QTY4 + lnCut4 ,;
              QTY5 WITH QTY5 + lnCut5 ,;
              QTY6 WITH QTY6 + lnCut6 ,;
              QTY7 WITH QTY7 + lnCut7 ,;
              QTY8 WITH QTY8 + lnCut8 ,;
              Ord1 WITH Ord1 + &lcCutPick..Qty1 ,;
              Ord2 WITH Ord2 + &lcCutPick..Qty2 ,;
              Ord3 WITH Ord3 + &lcCutPick..Qty3 ,;
              Ord4 WITH Ord4 + &lcCutPick..Qty4 ,;
              Ord5 WITH Ord5 + &lcCutPick..Qty5 ,;
              Ord6 WITH Ord6 + &lcCutPick..Qty6 ,;
              Ord7 WITH Ord7 + &lcCutPick..Qty7 ,;
              Ord8 WITH Ord8 + &lcCutPick..Qty8 ,;
              TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+QTy7+Qty8 ,;
              TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
      REPLACE nCost1 WITH TotQty * Style.nMCost1 ,;
              nCost2 WITH TotQty * Style.nMCost2 ,;
              nCost3 WITH TotQty * Style.nMCost3 ,;
              nCost4 WITH TotQty * Style.nMCost4 ,;
              nCost5 WITH TotQty * Style.nMCost5
      lnReqQty1 = lnReqQty1 - QTY1
      lnReqQty2 = lnReqQty2 - QTY2
      lnReqQty3 = lnReqQty3 - QTY3
      lnReqQty4 = lnReqQty4 - QTY4
      lnReqQty5 = lnReqQty5 - QTY5
      lnReqQty6 = lnReqQty6 - QTY6
      lnReqQty7 = lnReqQty7 - QTY7
      lnReqQty8 = lnReqQty8 - QTY8
      lnTotReq  = lnReqQty1+lnReqQty2+lnReqQty3+lnReqQty4+;
        lnReqQty5+lnReqQty6+lnReqQty7+lnReqQty8

      SELECT (lcCutTktH)
      REPLACE Pcs_Bud    WITH Pcs_Bud + lnTotCut ,;
              TotOrd     WITH TotOrd  + &lcCutPick..TotQty ,;
              nEst_Cost1 WITH nEst_Cost1 + lnTotCut*Style.nMCost1 ,;
              nEst_Cost2 WITH nEst_Cost2 + lnTotCut*Style.nMCost2 ,;
              nEst_Cost3 WITH nEst_Cost3 + lnTotCut*Style.nMCost3 ,;
              nEst_Cost4 WITH nEst_Cost4 + lnTotCut*Style.nMCost4 ,;
              nEst_Cost5 WITH nEst_Cost5 + lnTotCut*Style.nMCost5
    ENDSCAN
  ENDIF
ENDSCAN
SELECT CUTTKTL
SET RELATION OFF INTO CUTTKTH
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfUnAllPO
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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
PARAMETERS lcOrdFile
PRIVATE lnAlias,lnReqQty1,lnReqQty2,lnReqQty3,lnReqQty4,lnReqQty5,lnReqQty6,;
  lnReqQty7,lnReqQty8,lnTotReq,lnOrd1,lnOrd2,lnOrd3,lnOrd4,lnOrd5,;
  lnOrd6,lnOrd7,lnOrd8,lnCut1,lnCut2,lnCut3,lnCut4,lnCut5,lnCut6,;
  lnCut7,lnCut8,lnTotCut,lcExSign,lcUntSin

lnAlias = SELECT()
SET ORDER TO TAG CUTPICK IN CUTPICK
SET ORDER TO TAG POSLNS  IN POSLN
SELECT POSLN
SET RELATION TO cStyType+PO INTO POSHDR ADDITIVE

STORE '' TO lcExSign,lcUntSin
SELECT (lcOrdFile)
=SEEK("�")
SCAN REST WHILE cSelect+cDivision+cPurCode+cStyGrade+STYLE+Dyelot = "�"

  =SEEK(STYLE,'Style')
  lnReqQty1 = INT(laPercet[1]/100*(Qty1-Cut1))
  lnReqQty2 = INT(laPercet[2]/100*(Qty2-Cut2))
  lnReqQty3 = INT(laPercet[3]/100*(Qty3-Cut3))
  lnReqQty4 = INT(laPercet[4]/100*(Qty4-Cut4))
  lnReqQty5 = INT(laPercet[5]/100*(Qty5-Cut5))
  lnReqQty6 = INT(laPercet[6]/100*(Qty6-Cut6))
  lnReqQty7 = INT(laPercet[7]/100*(Qty7-Cut7))
  lnReqQty8 = INT(laPercet[8]/100*(Qty8-Cut8))
  lnTotReq  = lnReqQty1+lnReqQty2+lnReqQty3+lnReqQty4+lnReqQty5+lnReqQty6+lnReqQty7+lnReqQty8
  STORE 0 TO lnOrd1,lnOrd2,lnOrd3,lnOrd4,lnOrd5,lnOrd6,lnOrd7,lnOrd8
  IF SEEK(STYLE+'P','POSLN')
    SELECT POSLN
    SCAN REST WHILE STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = &lcOrdFile..Style+'P' ;
              FOR   IIF(llRpBOPFab,Dyelot=&lcOrdFile..Dyelot,.T.) AND TRANCD='1' AND ;
                    TotQty > TotOrd AND lnTotReq > 0  AND !INLIST(POSHDR.STATUS,'X','C')
      SELECT (lcPOLine)
      llFound=SEEK(POSLN.PO)
      =SEEK(POSLN.PO+&lcOrdFile..cDivision+POSHDR.cPurCode+POSLN.cStyGrade+POSLN.Style+POSLN.Dyelot)
      SUM REST  TotOrd TO lnAddOrd ;
          WHILE PO+CDIVISION+CPURCODE+CSTYGRADE+STYLE= ;
                POSLN.PO+&lcOrdFile..cDivision+POSHDR.cPurCode+POSLN.cStyGrade+POSLN.Style+POSLN.Dyelot
      IF POSLN.TotQty <= POSLN.TotOrd+lnAddOrd OR (!llFound AND POSHDR.FLAG='Y')
        LOOP
      ENDIF
      IF !llFound
        SELECT POSHDR
        REPLACE FLAG WITH 'Y'
      ENDIF
      SELECT POSLN
      lnCut1 = MIN(Qty1-Ord1,lnReqQty1)
      lnCut2 = MIN(Qty2-Ord2,lnReqQty2)
      lnCut3 = MIN(Qty3-Ord3,lnReqQty3)
      lnCut4 = MIN(Qty4-Ord4,lnReqQty4)
      lnCut5 = MIN(Qty5-Ord5,lnReqQty5)
      lnCut6 = MIN(Qty6-Ord6,lnReqQty6)
      lnCut7 = MIN(Qty7-Ord7,lnReqQty7)
      lnCut8 = MIN(Qty8-Ord8,lnReqQty8)
      lnTotCut=lnCut1+lnCut2+lnCut3+lnCut4+lnCut5+lnCut6+lnCut7+lnCut8
      SELECT (lcCutPick)
      APPEND BLANK
      REPLACE cTKtNo    WITH POSLN.PO ,;
              TRANCD    WITH '2'      ,;
              ORDER     WITH &lcOrdFile..ORDER         ,;
              CORDLINE  WITH STR(&lcOrdFile..LINENO,6) ,;
              STYLE     WITH &lcOrdFile..Style         ,;
              Dyelot    WITH POSLN.Dyelot              ,;
              CWARECODE WITH POSLN.CWARECODE           ,;
              CDIVISION WITH &lcOrdFile..cDivision     ,;
              cPurCode  WITH POSHDR.cPurCode           ,;
              cStyGrade WITH Style.cStyGrade
      REPLACE QTY1      WITH MIN(&lcOrdFile..Qty1-lnOrd1,lnCut1) ,;
              QTY2      WITH MIN(&lcOrdFile..Qty2-lnOrd2,lnCut2) ,;
              QTY3      WITH MIN(&lcOrdFile..Qty3-lnOrd3,lnCut3) ,;
              QTY4      WITH MIN(&lcOrdFile..Qty4-lnOrd4,lnCut4) ,;
              QTY5      WITH MIN(&lcOrdFile..Qty5-lnOrd5,lnCut5) ,;
              QTY6      WITH MIN(&lcOrdFile..Qty6-lnOrd6,lnCut6) ,;
              QTY7      WITH MIN(&lcOrdFile..Qty7-lnOrd7,lnCut7) ,;
              QTY8      WITH MIN(&lcOrdFile..Qty8-lnOrd8,lnCut8) ,;
              TOTQTY    WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8

      lnOrd1 = lnOrd1 + QTY1
      lnOrd2 = lnOrd2 + QTY2
      lnOrd3 = lnOrd3 + QTY3
      lnOrd4 = lnOrd4 + QTY4
      lnOrd5 = lnOrd5 + QTY5
      lnOrd6 = lnOrd6 + QTY6
      lnOrd7 = lnOrd7 + QTY7
      lnOrd8 = lnOrd8 + QTY8

      SELECT (lcOrdFile)
      REPLACE Cut1   WITH Cut1 + &lcCutPick..QTY1 ,;
              Cut2   WITH Cut2 + &lcCutPick..QTY2 ,;
              Cut3   WITH Cut3 + &lcCutPick..QTY3 ,;
              Cut4   WITH Cut4 + &lcCutPick..QTY4 ,;
              Cut5   WITH Cut5 + &lcCutPick..QTY5 ,;
              Cut6   WITH Cut6 + &lcCutPick..QTY6 ,;
              Cut7   WITH Cut7 + &lcCutPick..QTY7 ,;
              Cut8   WITH Cut8 + &lcCutPick..QTY8 ,;
              TotCut WITH Cut1+Cut2+Cut3+Cut4+Cut5+Cut6+Cut7+Cut8

      SELECT (lcPOH)
      IF !SEEK(POSLN.PO+&lcOrdFile..cDivision+Style.cPurCode+Style.cStyGrade)
        APPEND BLANK
        REPLACE cStyType   WITH 'P' ,;
                STATUS     WITH 'H' ,;
                LastLine   WITH 0   ,;
                PO         WITH PosLn.Po  ,;
                Vendor     WITH POSHDR.Vendor ,;
                CDIVISION  WITH &lcOrdFile..cDivision,;
                cPurCode   WITH Style.cPurCode   ,;
                ENTERED    WITH POSHDR.ENTERED   ,;
                COMPLETE   WITH POSHDR.COMPLETE  ,;
                SHIPVIA    WITH POSHDR.SHIPVIA   ,;
                CTERMCODE  WITH POSHDR.CTERMCODE ,;
                cWareCode  WITH POSHDR.cWareCode ,;
                cStyGrade  WITH Style.cStyGrade  ,;
                cPriceCur  WITH POSHDR.cPriceCur ,;
                nPriceRat  WITH POSHDR.nPriceRat ,;
                nCurrUnit  WITH POSHDR.nCurrUnit ,;
                cDutyCur   WITH POSHDR.cDutyCur  ,;
                nDutyRat   WITH POSHDR.nDutyRat  ,;
                nDCurUnit  WITH POSHDR.nDCurUnit ,;
                lMultiWare WITH POSHDR.lMultiWare
      ENDIF
      SELECT (lcPOLine)
      IF !SEEK(POSLN.PO+&lcOrdFile..cDivision+Style.cPurCode+Style.cStyGrade+&lcOrdFile..Style+POSLN.Dyelot)
        APPEND BLANK
        REPLACE PO        WITH POSLN.PO             ,;
                TRANCD    WITH '1'                  ,;
                STYLE     WITH &lcOrdFile..Style    ,;
                CDIVISION WITH &lcOrdFile..cDivision,;
                cWareCode WITH POSLN.cWareCode      ,;
                LINENO    WITH POSLN.Lineno         ,;
                Vendor    WITH POSHDR.VENDOR        ,;
                cPurCode  WITH Style.cPurCode       ,;
                cStyGrade WITH Style.cStyGrade      ,;
                Dyelot    WITH POSLN.Dyelot         ,;
                Fabric    WITH &lcOrdFile..Fabric   ,;
                cFabClr   WITH &lcOrdFile..cFabClr  ,;
                cFabWare  WITH &lcOrdFile..cFabWare ,;
                nYeild    WITH &lcOrdFile..nYeild

        SELECT (lcPOH)
        REPLACE LASTLINE WITH LASTLINE + 1
      ENDIF
      SELECT (lcPOLine)
      REPLACE QTY1 WITH QTY1 + lnCut1 ,;
              QTY2 WITH QTY2 + lnCut2 ,;
              QTY3 WITH QTY3 + lnCut3 ,;
              QTY4 WITH QTY4 + lnCut4 ,;
              QTY5 WITH QTY5 + lnCut5 ,;
              QTY6 WITH QTY6 + lnCut6 ,;
              QTY7 WITH QTY7 + lnCut7 ,;
              QTY8 WITH QTY8 + lnCut8 ,;
              Ord1 WITH Ord1 + &lcCutPick..Qty1 ,;
              Ord2 WITH Ord2 + &lcCutPick..Qty2 ,;
              Ord3 WITH Ord3 + &lcCutPick..Qty3 ,;
              Ord4 WITH Ord4 + &lcCutPick..Qty4 ,;
              Ord5 WITH Ord5 + &lcCutPick..Qty5 ,;
              Ord6 WITH Ord6 + &lcCutPick..Qty6 ,;
              Ord7 WITH Ord7 + &lcCutPick..Qty7 ,;
              Ord8 WITH Ord8 + &lcCutPick..Qty8 ,;
              TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+QTy7+Qty8 ,;
              TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8
      FOR lnCount = 1 TO 5
        lcCount = STR(lnCount,1)
        DO CASE
          CASE lcIType&lcCount = 'P'
            lcExSign       = gfGetExSin(@lcUntSin,POSHDR.cPriceCur)
            lnCost&lcCount = IIF(POSHDR.cPriceCur=Style.cPriceCur,lnTotCut*STYLE.nICost&lcCount,0)
            lnECost&lcCount= lnCost&lcCount &lcExSign POSHDR.nPriceRat &lcUntSin POSHDR.nCurrUnit
          CASE INLIST(lcIType&lcCount,'M','D')
            lcExSign       = gfGetExSin(@lcUntSin,POSHDR.cDutyCur)
            lnCost&lcCount = IIF(POSHDR.cDutyCur=Style.cDutyCur,lnTotCut*STYLE.nICost&lcCount,0)
            lnECost&lcCount= lnCost&lcCount &lcExSign POSHDR.nDutyRat &lcUntSin POSHDR.nDCurUnit
          OTHERWISE
            lnCost&lcCount = lnTotCut*STYLE.nICost&lcCount
            lnECost&lcCount= lnCost&lcCount
        ENDCASE
      ENDFOR
      REPLACE nCost1  WITH nCost1 + lnCost1 ,;
              nCost2  WITH nCost2 + lnCost2 ,;
              nCost3  WITH nCost3 + lnCost3 ,;
              nCost4  WITH nCost4 + lnCost4 ,;
              nCost5  WITH nCost5 + lnCost5 ,;
              nECost1 WITH nECost1+ lnECost1,;
              nECost2 WITH nECost2+ lnECost2,;
              nECost3 WITH nECost3+ lnECost3,;
              nECost4 WITH nECost4+ lnECost4,;
              nECost5 WITH nECost5+ lnECost5
      lnReqQty1 = lnReqQty1 - QTY1
      lnReqQty2 = lnReqQty2 - QTY2
      lnReqQty3 = lnReqQty3 - QTY3
      lnReqQty4 = lnReqQty4 - QTY4
      lnReqQty5 = lnReqQty5 - QTY5
      lnReqQty6 = lnReqQty6 - QTY6
      lnReqQty7 = lnReqQty7 - QTY7
      lnReqQty8 = lnReqQty8 - QTY8
      lnTotReq  = lnReqQty1+lnReqQty2+lnReqQty3+lnReqQty4+;
                  lnReqQty5+lnReqQty6+lnReqQty7+lnReqQty8

      SELECT (lcPOH)
      REPLACE nStyOrder WITH nStyOrder + lnTotCut  ,;
              TotOrd    WITH TotOrd  + &lcPOLine..TotOrd ,;
              NFCOST1   WITH NFCOST1 + lnCost1 ,;
              NFCOST2   WITH NFCOST2 + lnCost2 ,;
              NFCOST3   WITH NFCOST3 + lnCost3 ,;
              NFCOST4   WITH NFCOST4 + lnCost4 ,;
              NFCOST5   WITH NFCOST5 + lnCost5 ,;
              NICOST1   WITH NICOST1 + lnECost1,;
              NICOST2   WITH NICOST2 + lnECost2,;
              NICOST3   WITH NICOST3 + lnECost3,;
              NICOST4   WITH NICOST4 + lnECost4,;
              NICOST5   WITH NICOST5 + lnECost5
    ENDSCAN
  ENDIF
ENDSCAN
SELECT POSLN
SET RELATION OFF INTO POSHDR
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfFabPri
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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
PRIVATE lcFabric , lcColor , lcText , lnAlias , puFabrics , lnOldValue,;
  lnPcnt1  , lnPcnt2  ,lnPcnt3 ,lnPcnt4  ,lnPcnt5    ,lnPcnt6   ,;
  lnPcnt7  , lnPcnt8

STORE SPACE(7) TO lcFabric
STORE SPACE(6) TO lcColor
lnPcnt1 = laPercet[1]
lnPcnt2 = laPercet[2]
lnPcnt3 = laPercet[3]
lnPcnt4 = laPercet[4]
lnPcnt5 = laPercet[5]
lnPcnt6 = laPercet[6]
lnPcnt7 = laPercet[7]
lnPcnt8 = laPercet[8]
lcText = IIF(llRPUnAWip,'Location  Dyelot        Onhand  Unall. WIP',;
  'Location  Dyelot    Onhand')
lnAlias  = SELECT()
SELECT DISTINCT Fabric FROM (lcFabrics) INTO ARRAY laFabrics
*B602428,1 Check if no primary fabric onhand
IF _TALLY > 0
  DEFINE POPUP puFabPri MARGIN MOVER RELATIVE SCROLL MARK CHR(16)
  puFabrics = 1
  STORE 0 TO lnFabReq,lnOldValue
  =lfFabClr()
  DO (gcScrDir+'MFFABPRI.SPR')
  *B803686,1 RAMY Add the return statment to check the function return value [start]
  llReturn = .T.
  RETURN llReturn
  *B803686,1 RAMY [end]
ELSE
  *B602428,1 Message : 38164
  *B602428,1 No fabrics selected. No materials to allocate from.
  *B602428,1 Button : 00000
  *B602428,1 Ok
  =gfModalGen('TRM38164B00000','DIALOG')
  *B803686,1 RAMY Add the return statment to check the function return value [start]
  llReturn = .F.
  RETURN llReturn
  *B803686,1 RAMY [end]
ENDIF

*!*************************************************************
*! Name      : lfFabClr
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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
SELECT DISTINCT COLOR FROM (lcFabrics) WHERE Fabric=laFabrics[puFabrics] INTO ARRAY laColors
puColors = 1
=lfFillPopup()
lcFabric = laFabrics[puFabrics]
lcColor  = laColors[puColors]
=lfFabReq(lcFabric,lcColor)

*!*************************************************************
*! Name      : lfvFabrics
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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
=lfFabClr()

*!*************************************************************
*! Name      : lfvColors
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
*! Purpose   : Validate selected fabric/Color
*!*************************************************************
*! Calls     : lfFillPopup()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvColors()
*!*************************************************************
FUNCTION lfvColors

=lfFillPopup()
lcColor =laColors[puColors]
=lfFabReq(lcFabric,lcColor)

*!*************************************************************
*! Name      : lfReFabPri
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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
SELECT (lcFabrics)
FOR lnCount = 1 TO CNTBAR('puFabPri')
  =SEEK(lcFabric+lcColor+PADL(lnCount,4,'0'))
  REPLACE cNewPrior WITH PADL(GETBAR('puFabPri',lnCount),4,'0')
ENDFOR
REPLACE ALL cPriority WITH cNewPrior ;
  FOR Fabric+COLOR=lcFabric+lcColor

*!*************************************************************
*! Name      : lfFillPopup
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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
PRIVATE lcPrompt,lnCount

=lfReFabPri()
SELECT (lcFabrics)

*B802606,1 RAMY Add these lines to sort the output on the OnHand qty. [start]
PRIVATE lcOldIndx

lcOldIndx = ORDER()
SET ORDER TO OnHand DESCENDING
*B802606,1 RAMY Add these lines to sort the output on the OnHand qty. [End]

RELEASE BAR ALL OF puFabPri
lnCount = 0
=SEEK(laFabrics[puFabrics]+laColors[puColors])
SCAN REST WHILE FABRIC+COLOR = laFabrics[puFabrics]+laColors[puColors]
  lnCount = lnCount + 1
  lcPrompt = cWareCode+SPACE(3)+Dyelot+SPACE(1)+STR(OnHand,10,3)+;
    IIF(llRPUnAWip,SPACE(1)+STR(nUnAllWIP,10,3),'')
  DEFINE BAR lnCount OF puFabPri PROMPT lcPrompt
ENDSCAN

*B802606,1 RAMY Add this line to restore the old index [start]
SET ORDER TO lcOldIndx
*B802606,1 RAMY Add this line to restore the old index [end]

SHOW GET lsFabPri

*!*************************************************************
*! Name      : lfvOkFabPri
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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

=lfReFabPri()
IF laPercet[1]<>lnPcnt1 OR laPercet[2]<>lnPcnt2 OR laPercet[3]<>lnPcnt3 OR ;
    laPercet[4]<>lnPcnt4 OR laPercet[5]<>lnPcnt5 OR laPercet[6]<>lnPcnt6 OR ;
    laPercet[7]<>lnPcnt7 OR laPercet[8]<>lnPcnt8
  =lfFabReq('','')
ENDIF
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfFabReq
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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
PARAMETERS lcFabric,lcColor

IF !EMPTY(lcFabric+lcColor) AND EVAL(SYS(18))=lnOldValue
  RETURN
ENDIF
SELECT (lcOrdLine)
SET ORDER TO TAG 'Groups'
=SEEK("�"+lcFabric+lcColor)
lnFabReq = 0
SCAN REST WHILE cSelect+Fabric+cFabClr+cSortExp+ORDER+STORE+GROUP=;
    "�"+lcFabric+lcColor
  *B602467,1 Allocate order open quantity instead of ordered quantity
  *lnReqQty = (QTY1*laPercet[1]+QTY2*laPercet[2]+QTY3*laPercet[3]+QTY4*laPercet[4]+;
  QTY5*laPercet[5]+QTY6*laPercet[6]+QTY7*laPercet[7]+QTY8*laPercet[8])*nYeild/100
lnReqQty = ( INT((QTY1-Cut1)*laPercet[1]/100)+INT((QTY2-Cut2)*laPercet[2]/100)+;
  INT((QTY3-Cut3)*laPercet[3]/100)+INT((QTY4-Cut4)*laPercet[4]/100)+;
  INT((QTY5-Cut5)*laPercet[5]/100)+INT((QTY6-Cut6)*laPercet[6]/100)+;
  INT((QTY7-Cut7)*laPercet[7]/100)+INT((QTY8-Cut8)*laPercet[8]/100) )*nYeild
*B602467,1 (End)

lnFabReq = lnFabReq + lnReqQty

SELECT (lcOrdGroup)
=SEEK(&lcOrdLine..Fabric+&lcOrdLine..cFabClr+&lcOrdLine..Order+;
  &lcOrdLine..Store+&lcOrdLine..Group)
REPLACE nRequired WITH nRequired - &lcOrdLine..nRequired + lnReqQty
SELECT (lcOrdLine)
REPLACE nRequired WITH lnReqQty
ENDSCAN
=lfRefresh('MFFABPRI')

*!*************************************************************
*! Name      : lfBaseOnFab
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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
PRIVATE lcOrderBy

SELECT (lcOrdLine)
SET ORDER TO TAG GROUPS
=SEEK("�")
DO WHILE cSelect+Fabric+cFabClr+cSortExp+ORDER+STORE+GROUP = "�"
  lcOrdGrp = cSelect+Fabric+cFabClr+cSortExp+ORDER+STORE+GROUP
  lcGroup  = Fabric+cFabClr+ORDER+STORE+GROUP
  =SEEK(lcGroup,lcOrdGroup)
  SELECT (lcFabrics)
  =SEEK(&lcOrdLine..Fabric+&lcOrdLine..CFabClr)
  LOCATE REST WHILE Fabric+COLOR+cPriority+Dyelot+cWareCode = ;
    &lcOrdLine..Fabric+&lcOrdLine..CFabClr    ;
    FOR   OnHand+nUnAllWIP-nAllocated-nWIPUsed >= &lcOrdGroup..nRequired
  llOnHand = FOUND()
  SELECT (lcOrdLine)
  SCAN REST WHILE cSelect+Fabric+cFabClr+cSortExp+ORDER+STORE+GROUP=lcOrdGrp
    SCATTER MEMVAR
    m.cSelect  = IIF(llOnHand,"�",' ')
    m.Dyelot   = IIF(llOnHand,&lcFabrics..Dyelot,SPACE(10))
    m.cFabWare = IIF(llOnHand,&lcFabrics..cWareCode,SPACE(6))
    INSERT INTO (lcTmpOrd) FROM MEMVAR
    *B602471,1 Force allocate all order lines group for one dyelot
    SELECT (lcOrdGroup)
    =SEEK(m.Fabric+m.cFabClr+m.Order+m.Store+m.Group)
    REPLACE CWareCode WITH m.cFabWare ,;
      Dyelot    WITH m.Dyelot
    *B602471,1 (End)
    IF llOnHand
      SELECT (lcStyUnAll)
      =SEEK(m.Style+m.Dyelot)
      lnWIPUsed = MIN(m.nRequired,nUnAllWIP-nWIPUsed)
      REPLACE nWIPUsed WITH nWIPUsed + lnWIPUsed
      SELECT (lcFabrics)
      REPLACE nAllocated WITH nAllocated + m.nRequired-lnWIPUsed ,;
        nWIPUsed   WITH nWIPUsed + lnWIPUsed
      SELECT (lcTmpOrd)
      REPLACE nWIPUsed WITH lnWIPUsed
    ENDIF
  ENDSCAN
ENDDO
SET ORDER TO TAG (lcOrdLine) IN (lcOrdLine)
GO TOP IN (lcOrdLine)

*!*************************************************************
*! Name      : lfShowAll
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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
PRIVATE lnAlias

lnAlias = SELECT()
SELECT (lcFabrics)
SET RELATION TO Fabric+COLOR INTO (lcTmpOrd)

SELECT (lcTmpOrd)
SET ORDER TO TAG 'Fabrics'
SET RELATION TO STYLE+&lcFabrics..Dyelot INTO (lcStyUnAll)
SET RELATION TO 'M'+Account INTO CuSTOMER ADDITIVE
*B602471,1 Force allocate all order lines group for one dyelot


GO TOP IN (lcFabrics)
lcFabTitl = 'Fabrics'
lcFabOrd  = 'Allocated/Deallocated Order Lines'
STORE 0 TO lnFabRec,lnOrdRec
=lfClearKey()
STORE '' TO lcBrowseTl
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

*B602437,1 Fix allocate/deallocate orders
lcPrompt = IIF(&lcFabrics..cWareCode=ALLTRIM(&lcTmpOrd..cFabWare) AND ;
  &lcFabrics..Dyelot=ALLTRIM(&lcTmpOrd..Dyelot) AND !EMPTY(&lcTmpOrd..cSelect),;
  "De\<allocate","\<Allocate")
lcAllStat = IIF(&lcFabrics..cWareCode=ALLTRIM(&lcTmpOrd..cFabWare) AND ;
  &lcFabrics..Dyelot=ALLTRIM(&lcTmpOrd..Dyelot),'ENABLE','DISABLE')
*B602437,1 (End)

DO (gcScrDir+'MFGENCT5.SPR')
SELECT (lcTmpOrd)
SET ORDER TO TAG 'TICKET'
SET RELATION TO
SELECT (lcFabrics)
SET RELATION TO
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfReadAct
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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
*B802376 WAB -  No need for check if glFromBrow cause gfstopbrow()
*B802376        check for that.
*IF glFromBrow
*  =gfStopBrow()
*  glFromBrow = .F.
*ENDIF
=gfStopBrow()

=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

*!*************************************************************
*! Name      : lfClearKey
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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
*! Name      : lfdShowFab
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
*! Purpose   : Allocated order lines screen deactivate function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfdShowFab()
*!*************************************************************
FUNCTION lfdShowFab

IF WONTOP()=lcFabTitl .OR. WONTOP()=lcFabOrd
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  glFromBrow = .T.
  IF WONTOP()=lcFabTitl
    ON KEY LABEL TAB     DO lpMovTab WITH 'MFGENC53','ibBOrd'
    ON KEY LABEL BACKTAB DO lpBackTab WITH 'MFGENC53','pbOk'
  ELSE
    ON KEY LABEL TAB     DO lpMovTab WITH 'MFGENC53','pbAllocate'
    ON KEY LABEL BACKTAB DO lpBackTab WITH 'MFGENC53','ibBFab'
  ENDIF
ELSE
  glFromBrow = .F.
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfwShowAll
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
*! Purpose   : Browse Fabric/Color/Warehouse/Dyelot & Allocated/Unallocated
*!             order lines
*!*************************************************************
*! Calls     : lfShowOrd(),lfShowOrd()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfwShowAll()
*!*************************************************************
FUNCTION lfwShowAll

SELECT (lcFabrics)
lcFields = "CMARK=IIF(RECNO()=lnFabRec,'>',' '):H=' ',Fabric :R,Color:R,"+IIF(llWareHous,"CWARECODE :H='Location' :R,","")+;
  IIF(llDyelot,"DYELOT :H='Dyelot' :R,","")+"ONHAND :R:H='Onhand',"+;
  "nAllocated :H='Allocated' :R,nBalance=Onhand-nAllocated :H='Balance' :R"
BROWSE FIELDS &lcFields ;
  NOAPPEND ;
  NOWAIT   ;
  NOMENU   ;
  NOCLEAR  ;
  TITLE lcFabTitl ;
  WHEN lfShowFab() ;
  WINDOW MFGENC51 IN WINDOW MFGENCT5
SELECT (lcTmpOrd)

*B602503,1 Browse order open quantity instead of ordered quantity
*lcFields = "CMARK=IIF(RECNO()=lnOrdRec,'>',' '):H=' ',cSelect :H=' ' :R,Order :H='Order#':R,"+;
IIF(llRPUnAWip,'',"Account:R,")+"Name=SUBSTR(Customer.StName,1,25) :R:H='Account Name',"+;
"Store :H='Store' :R,Style :R,TotQty :H='Pieces' :R, nRequired :H='YDS RQD.' :R"+;
+IIF(llRPUnAWip,",&lcStyUnAll..nUnAllWIP :R :H='YDS WIP',Account:R",'')

lcFields = "CMARK=IIF(RECNO()=lnOrdRec,'>',' '):H=' ',cSelect :H=' ' :R,Order :H='Order#':R,"+;
  IIF(llRPUnAWip,'',"Account:R,")+"Name=SUBSTR(Customer.StName,1,25) :R:H='Account Name',"+;
  "Store :H='Store' :R,Style :R,nPieces=lfGetPcs() :H='Pieces' :7 :R, nRequired :H='YDS RQD.' :R"+;
  +IIF(llRPUnAWip,",&lcStyUnAll..nUnAllWIP :R :H='YDS WIP',Account:R",'')
*B602503,1 (End)

*B602471,1 Force allocate all order lines group for one dyelot
*BROWSE FIELDS &lcFields ;
NOAPPEND ;
NOWAIT   ;
NOMENU   ;
NOCLEAR  ;
TITLE lcFabOrd  ;
WHEN lfShowOrd();
FOR &lcFabrics..cWareCode = ALLTRIM(cFabWare) AND &lcFabrics..Dyelot = ALLTRIM(Dyelot) ;
WINDOW MFGENC52 IN WINDOW MFGENCT5

*B802607,1 add these lines to change the FOR condition in
*          the order browes [start]
*BROWSE FIELDS &lcFields ;
*       NOAPPEND ;
*       NOWAIT   ;
*       NOMENU   ;
*       NOCLEAR  ;
*       TITLE lcFabOrd  ;
*       WHEN lfShowOrd();
*       FOR &lcFabrics..cWareCode=ALLTRIM(&lcOrdGroup..cWareCode) AND;
*       &lcFabrics..Dyelot   =ALLTRIM(&lcOrdGroup..Dyelot)  ;
*       WINDOW MFGENC52 IN WINDOW MFGENCT5

BROWSE FIELDS &lcFields ;
  NOAPPEND ;
  NOWAIT   ;
  NOMENU   ;
  NOCLEAR  ;
  TITLE lcFabOrd  ;
  WHEN lfShowOrd();
  FOR &lcFabrics..cWareCode=ALLTRIM(&lcTmpOrd..cWareCode) AND;
  IIF(!EMPTY(&lcTmpOrd..Dyelot) , &lcFabrics..Dyelot= ;
  RTRIM(&lcTmpOrd..Dyelot), .T.)                ;
  WINDOW MFGENC52 IN WINDOW MFGENCT5
*B802607,1 add these lines to change the FOR condition [end]


*B602471,1 (End)

*!*************************************************************
*! Name      : lfGetPcs
*! Developer : Wael Aly Mohamed
*! Date      : 02/05/1999
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
*! Name      : lfShowFab
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
*! Purpose   : Show Fabric/Color/Warehouse/Dyelot Onhand quantity
*!*************************************************************
*! Calls     : lfShowOrd()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfShowFab()
*!*************************************************************
FUNCTION lfShowFab

lnFabRec = RECNO(lcFabrics)
SHOW WINDOW (lcFabTitl) REFRESH SAME
=lfShowOrd()

*!*************************************************************
*! Name      : lfShowOrd
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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
PRIVATE lcPrompt,lcAllStat

lnOrdRec = RECNO(lcTmpOrd)
SHOW WINDOW (lcFabOrd) REFRESH SAME

*B602437,1 Fix allocate/deallocate orders
*SHOW GET pbAllocate,1 PROMPT IIF(EMPTY(&lcTmpOrd..cSelect),"\<Allocate","De\<allocate")
lcPrompt = IIF(&lcFabrics..cWareCode=ALLTRIM(&lcTmpOrd..cFabWare) AND ;
  &lcFabrics..Dyelot=ALLTRIM(&lcTmpOrd..Dyelot) AND !EMPTY(&lcTmpOrd..cSelect),;
  "De\<allocate","\<Allocate")
lcAllStat = IIF(&lcFabrics..cWareCode=ALLTRIM(&lcTmpOrd..cFabWare) AND ;
  &lcFabrics..Dyelot=RTRIM(&lcTmpOrd..Dyelot),'ENABLE','DISABLE')
SHOW GET pbAllocate,1 PROMPT lcPrompt &lcAllStat
*B602437,1 (End)

*!*************************************************************
*! Name      : lfAllOrd
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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
PRIVATE lckey,lnWipUsed

SELECT (lcTmpOrd)

*B605517,1 KHM 02/13/2002 (Begin) Changing the index by changing the
*B605517,1                orders between dyelot and order
*lckey= Fabric+cFabClr+cWareCode+Dyelot+ORDER+STR(LINENO,6)
lckey= Fabric+cFabClr+cWareCode+ORDER+Dyelot+STR(LINENO,6)
*B605517,1 KHM 02/13/2002 (End)

IF EMPTY(&lcTmpOrd..cSelect)
  *B602471,1 Force allocate all order lines group for one dyelot
  IF EMPTY(&lcOrdGroup..cWarecode+&lcOrdGroup..Dyelot) AND ;
      &lcOrdGroup..nRequired - &lcOrdGroup..nUnAllWIP > &lcFabrics..ONHAND
    *E300725,1 Message : 38165
    *E300725,1 Not enough inventory available to allocate this order lines group
    *E300725,1 Button : 00000
    *E300725,1 Ok
    =gfModalGen('TRM38165B00000','ALERT')
    RETURN
  ENDIF
  *B602471,1 (End)

  IF &lcTmpOrd..nRequired-(&lcStyUnAll..nUnAllWIP-&lcStyUnAll..nWIPUsed) > ;
      &lcFabrics..ONHAND-&lcFabrics..nAllocated
    *E300725,1 Message : 38149
    *E300725,1 Not enough inventory available to allocate this order line
    *E300725,1 Button : 00000
    *E300725,1 Ok
    =gfModalGen('TRM38149B00000','ALERT')
    RETURN
  ENDIF

  SELECT (lcStyUnAll)
  lnWipUsed=MIN(&lcTmpOrd..nRequired,nUnAllWIP-nWIPUsed)
  REPLACE nWIPUsed WITH nWIPUsed + lnWIPUsed
  SELECT (lcFabrics)
  REPLACE nAllocated WITH nAllocated + &lcTmpOrd..nRequired-lnWipUsed
  SELECT (lcTmpOrd)
  =SEEK(lckey)
  REPLACE cSelect  WITH "�" ,;
    cFabWare WITH &lcFabrics..cWareCode ,;
    Dyelot   WITH &lcFabrics..Dyelot ,;
    nWIPUsed WITH lnWIPUsed

    *-- khm
    SKIP
    lnRec1 = RECNO()
    SKIP -1
    *-- khm
    
  *B602471,1 Force allocate all order lines group for one dyelot
  SELECT (lcOrdGroup)
  REPLACE CWareCode WITH &lcFabrics..cWareCode ,;
    Dyelot    WITH &lcFabrics..Dyelot
  SELECT (lcTmpOrd)

  *-- khm
  IF lnRec1 > RECCOUNT()
    GO TOP
  ELSE
    GO lnRec1
  ENDIF
  *-- khm
  
  *B602471,1 (End)
ELSE

  SELECT (lcStyUnAll)
  REPLACE nWIPUsed WITH nWIPUsed - &lcTmpOrd..nWIPUsed
  SELECT (lcFabrics)
  REPLACE nAllocated WITH nAllocated - (&lcTmpOrd..nRequired-&lcTmpOrd..nWIPUsed)
  SELECT (lcTmpOrd)
  =SEEK(lckey)

  *B802607,1 RAMY  Add these lines to store the next record number [START]
  SKIP
  lnRec1 = RECNO()
  SKIP -1
  *B802607,1 RAMY  Add these lines to store the next record number [END]

  REPLACE cSelect  WITH ' ' ,;
    cFabWare WITH SPACE(6)  ,;
    Dyelot   WITH SPACE(10) ,;
    nWIPUsed WITH 0

  *B802607,1 RAMY Add these lines to go the next record after modification [START]
  IF lnRec1 > RECCOUNT()
    GO TOP
  ELSE
    GO lnRec1
  ENDIF
  *B802607,1 RAMY Add these lines to go the next record after modification [END]

  *B602471,1 Force allocate all order lines group for one dyelot
  IF &lcFabrics..nAllocated=0
    SELECT (lcOrdGroup)
    REPLACE CWareCode WITH ''  ,;
      Dyelot    WITH ''
    SELECT (lcTmpOrd)
  ENDIF
  *B602471,1 (End)
ENDIF

=lfShowOrd()

*!*************************************************************
*! Name      : lfUpdPO
*! Developer : Wael ALy Mohamed
*! Date      : 10/10/1998
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
*E301426,1 SSH Add new private Variabel.
*PRIVATE lcDivision,lcPurCode,lcPO,lcGrade,lcPONumber
PRIVATE lcDivision,lcPurCode,lcPO,lcGrade,lcPONumber , llUpdBom , llBomByMe,;
        llGnCTBom
llBomByMe = gfOpenFile(gcDataDir+'Bom',gcDataDir+'Bom','SH')

*B604003,1 KHM 11/01/2000 (Begin) Change the memory variable to the PO one.
*llGnCTBom = (gfGetMemVar("M_GNCTBOM")  = "T")
llGnCTBom = (gfGetMemVar("M_CRTCSTSH")  = "T")
*B604003,1 KHM (END)
*E301426,1 SSH [END]
*E300976,1 SWK 08/09/1998 Add dyelots for style P/Os
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
*E300976,1 (End)
SET ORDER TO TAG POSLN IN POSLN
lcFirstCT  = SPACE(06)
SELECT (lcPOH)
*-- Scan the tmp.POHdr for qty > 0 to generate auto. PO numbers
SCAN FOR nStyOrder > 0
  lcPO       = PO
  *E301426,1 SSH Variable to tell if all PO's styles has cost sheet.
  llUpdBom = .T.
  *E301426,1 SSH Add new private Variabel.
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
    *E301426,1 SSH Variable to tell if all PO's styles has cost sheet.
    llUpdBom  =  IIF(llUpdBom,SEEK(SUBSTR(Style,1,lnMajorLen),'BOM'),llUpdBom)
    *E301426,1 SSH [END]
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
        *=gfTraceKey('POSLN','P'+lcPONumber+style+STR(lineno,6)+TRANCD,'M')
        =gfTraceKey('POSLN','P'+lcPONumber+cRsession+Shipno+STYLE+STR(LINENO,6)+Trancd,'M')
        SELECT (lcPOLine)
        =RLOCK()
        REPLACE nSteps WITH 1
        UNLOCK
      ENDIF
    ENDIF
    *E301182,1 Update PO line number in the CUTPICK file.
    SELECT (lcCutPick)
    =SEEK(lcPO+lcDivision+lcPurCode+lcGrade+POSLn.Style+POSLn.cWareCode+POSLn.Dyelot)
    SCAN REST WHILE CTKTNO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+CWARECODE+DYELOT =;
        lcPO+lcDivision+lcPurCode+lcGrade+POSLn.Style+;
        POSLn.cWareCode+POSLn.Dyelot FOR TotQty > 0
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
    ENDSCAN
    REPLACE ALL CTKTNO WITH lcPONumber ;
      FOR CTKTNO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+CWARECODE+DYELOT =;
      lcPO+lcDivision+lcPurCode+lcGrade+POSLn.Style+;
      POSLn.cWareCode+POSLn.Dyelot AND TotQty > 0
    *E301182,1 (End)
  ENDSCAN
  *E301182,1 Following lines have been commented out
  *SELECT (lcCutPick)
  *=SEEK(lcPO+lcDivision+lcPurCode+lcGrade)
  *SCAN REST WHILE CTKTNO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+CWARECODE+DYELOT =;
  *                lcPO+lcDivision+lcPurCode+lcGrade FOR TotQty > 0
  *  SCATTER MEMVAR
  *  SELECT ORDHDR
  *  =SEEK('O'+m.ORDER)
  *  IF &lcCutPick..nSteps < 1
  *    =RLOCK()
  *    REPLACE TotCut WITH TotCut + m.TotQty
  *    UNLOCK
  *    =gfTraceKey('ORDHDR','O'+ORDER,'M')
  *    SELECT (lcCutPick)
  *    =RLOCK()
  *    REPLACE nSteps WITH 1
  *    UNLOCK
  *  ENDIF
  *  SELECT ORDLINE
  *  =SEEK('O'+m.Order+m.cOrdLine)
  *  IF &lcCutPick..nSteps < 2
  *    =RLOCK()
  *    REPLACE Dyelot WITH m.Dyelot     ,;
  *            Cut1   WITH Cut1 + m.Qty1,;
  *            Cut2   WITH Cut2 + m.Qty2,;
  *            Cut3   WITH Cut3 + m.Qty3,;
  *            Cut4   WITH Cut4 + m.Qty4,;
  *            Cut5   WITH Cut5 + m.Qty5,;
  *            Cut6   WITH Cut6 + m.Qty6,;
  *            Cut7   WITH Cut7 + m.Qty7,;
  *            Cut8   WITH Cut8 + m.Qty8,;
  *            TotCut WITH TotCut + m.TotQty
  *    UNLOCK
  *    =gfTraceKey('ORDLINE','O'+ORDER+STR(LINENO,6),'M')
  *    SELECT (lcCutPick)
  *    =RLOCK()
  *    REPLACE nSteps WITH 2
  *    UNLOCK
  *  ENDIF
  *  m.CTKTNO = lcPONumber
  *  IF &lcCutPick..nSteps < 3
  *    INSERT INTO CUTPICK FROM MEMVAR
  *    =gfTraceKey('CUTPICK',m.trancd+m.ctktno+m.order+m.style+m.cordline,'A')
  *    SELECT (lcCutPick)
  *    =RLOCK()
  *    REPLACE nSteps WITH 3
  *    UNLOCK
  *  ENDIF
  *ENDSCAN
  *REPLACE ALL CTKTNO WITH lcPONumber ;
  *        FOR CTKTNO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+CWARECODE+DYELOT =;
  *            lcPO+lcDivision+lcPurCode+lcGrade AND TotQty > 0
  *E301182,1 (End)
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
    *=gfTraceKey('POSLN','P'+lcPONumber,'A')
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
  *E301426,1 SSH 
  *--- Get the module seting for generating cost sheet
  *--- if this seting is alwayes ("A") we will call function to call
  *--- C/T,PO's Cost Sheet program to generate Cost Sheet.
  llDumy = llUpdBom .AND. laScrMode[4] .AND. llGnCTBom .AND. lfGenCutBom('',POSHDR.PO)
  *B604029,1 HBG 11/26/2000 Move this code out of the scan to fix bug alias not found [Begin]
  *IF llBomByMe
   * USE IN BOM
  *ENDIF
  *B604029,1 [End]
  *E301426,1 SSH [END]
ENDSCAN
*B604029,1 HBG 11/26/2000 Close BOM file after finish the scan to fix bug of alias not found [Begin]
IF llBomByMe
  USE IN BOM
ENDIF
*B604029,1 [End]
SELECT (lcPOH)
REPLACE ALL PO WITH cTmpPo
GO TOP

*!*************************************************************
*! Name      : lfUpdCT
*! Developer : Wael Aly Mohamed
*! Date      : 10/10/1998
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
*E301426,1 SSH Add New Private Variable.
*PRIVATE lcCTNumber,lcCutTKt,lcDivision,lcSeason
PRIVATE lcCTNumber,lcCutTKt,lcDivision,lcSeason,llGnCTBom
*E301426,1 SSH[END]

*-- if the system support dyelots
IF llDyelot
  SELECT(lcCuttktL)
  SCAN FOR EMPTY(DYELOT)
    *-- if the style is dyelot and the dyelot fields is empty
    *-- stop saving and give the user a message
    IF SEEK(STYLE,'STYLE') AND (STYLE.CDYE_FLG = 'Y')
      *-- You have to enter dyelot for the required dyelots records
      =gfModalGen('INM38135B38018','DIALOG','C/Ts')
      llCSave   = .F.
      RETURN
    ENDIF
  ENDSCAN
ENDIF
*E301426,1 SSH Get Module Setup , Generate Cost Sheet Automaticaly.
llGnCTBom = (gfGetMemVar("M_GNCTBOM")  = "T")
*E301426,1 SSH[END]
SET ORDER TO TAG CUTTKTL IN CUTTKTL
SELECT (lcCuttktH)
*-- scan the tmp. Cuttkth for qtys > 0 to gfenerate auto. C/T numbers
SCAN FOR Pcs_Bud > 0
  =SEEK(STYLE,'STYLE')
  lcStyle    = SUBSTR(STYLE,1,lnMajorLen)
  lcCutTKt   = CutTkt
  lcDivision = CDIVISION
  lcSeason   = Season
  lcCTNumber = IIF(EMPTY(CutTkt),IIF(EMPTY(cTmpCutTkt),;
    gfSequence("CUTTKT","","",lcDivision),cTmpCutTkt),CutTkt)
  =RLOCK()
  REPLACE cTmpCutTkt WITH lcCTNumber
  UNLOCK
  lcFirstCT  = IIF(EMPTY(lcFirstCT),lcCTNumber,lcFirstCT)
  lcLastCt   = lcCTNumber

  SELECT (lcCuttktL)
  *B602549,1 Fix updating ticket lines
  *=SEEK(lcCutTKt+lcDivision+lcSeason)
  *SCAN REST WHILE CUTTKT+CDIVISION+SEASON+STYLE+DYELOT=;
  lcCutTKt+lcDivision+lcSeason FOR TOTQTY > 0
=SEEK(lcCutTKt+lcDivision+lcSeason+lcStyle)
SCAN REST WHILE CUTTKT+CDIVISION+SEASON+STYLE+DYELOT=;
    lcCutTKt+lcDivision+lcSeason+lcStyle FOR TOTQTY > 0
  *B602549,1 (End)
  IF !SEEK(lcCTNumber+STYLE+dyelot+trancd,'CUTTKTL')
    SCATTER MEMVAR
    =SEEK(m.STYLE,'STYLE')
    m.nCost1 = STYLE.nMCost1
    m.nCost2 = STYLE.nMCost2
    m.nCost3 = STYLE.nMCost3
    m.nCost4 = STYLE.nMCost4
    m.nCost5 = STYLE.nMCost5
    m.CUTTKT = lcCTNumber
    IF !SEEK(m.STYLE+m.CWARECODE+SPACE(10),'STYDYE')
      WAIT "Assigning Style"+ALLTRIM(m.STYLE)+" to warehouse "+ALLTRIM(m.cWareCode) WINDOW NOWAIT
      DO gpAdStyWar WITH m.STYLE,SPACE(10),m.cWareCode
      WAIT CLEAR
      =SEEK(m.STYLE+m.CWARECODE+SPACE(10),'STYDYE')
    ENDIF
    IF STYLE.lInvSty AND &lcCuttktL..nSteps < 1
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
      SELECT (lcCuttktL)
      =RLOCK()
      REPLACE nSteps WITH 1
      UNLOCK
    ENDIF
    IF STYLE.lInvSty AND &lcCuttktL..nSteps < 2
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
      SELECT (lcCuttktL)
      =RLOCK()
      REPLACE nSteps WITH 2
      UNLOCK
    ENDIF
    SELECT (lcCuttktL)
    =RLOCK()
    REPLACE nSteps WITH 3
    UNLOCK
    m.cAdd_User = gcUser_id
    m.cAdd_Time = TIME()
    m.dAdd_Date = gdSysDate
    INSERT INTO CUTTKTL FROM MEMVAR
  ELSE
    IF &lcCuttktL..nSteps < 1
      SELECT CUTTKTL
      =RLOCK()
      REPLACE Ord1 WITH Ord1 + &lcCuttktL..Ord1 ,;
        Ord2 WITH Ord2 + &lcCuttktL..Ord2 ,;
        Ord3 WITH Ord3 + &lcCuttktL..Ord3 ,;
        Ord4 WITH Ord4 + &lcCuttktL..Ord4 ,;
        Ord5 WITH Ord5 + &lcCuttktL..Ord5 ,;
        Ord6 WITH Ord6 + &lcCuttktL..Ord6 ,;
        Ord7 WITH Ord7 + &lcCuttktL..Ord7 ,;
        Ord8 WITH Ord8 + &lcCuttktL..Ord8 ,;
        TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
      UNLOCK
      SELECT (lcCuttktL)
      =RLOCK()
      REPLACE nSteps WITH 1
      UNLOCK
    ENDIF
  ENDIF
  *E301182,1 Update CT line number in the CUTPICK file.
  SELECT (lcCutPick)
  =SEEK(lcCutTKt+lcDivision+lcSeason+CutTktL.Style+CutTktL.cWareCode+CutTktL.Dyelot)
  SCAN REST WHILE CTKTNO+CDIVISION+SEASON+STYLE+CWARECODE+DYELOT=;
      lcCutTKt+lcDivision+lcSeason+CutTktL.Style+;
      CutTktL.cWareCode+CutTktL.Dyelot FOR TOTQTY > 0
    SCATTER MEMVAR
    SELECT ORDHDR
    =SEEK('O'+m.ORDER)
    IF &lcCutPick..nSteps < 1
      =RLOCK()
      REPLACE TotCut WITH TotCut + m.TotQty
      UNLOCK
      SELECT (lcCutPick)
      =RLOCK()
      REPLACE nSteps WITH 1
      UNLOCK
    ENDIF
    SELECT ORDLINE
    =SEEK('O'+m.ORDER+m.CORDLINE)
    IF &lcCutPick..nSteps < 2
      =RLOCK()
      REPLACE Dyelot WITH m.Dyelot    ,;
        CUT1   WITH CUT1+m.Qty1 ,;
        CUT2   WITH CUT2+m.Qty2 ,;
        CUT3   WITH CUT3+m.Qty3 ,;
        CUT4   WITH CUT4+m.Qty4 ,;
        CUT5   WITH CUT5+m.Qty5 ,;
        CUT6   WITH CUT6+m.Qty6 ,;
        CUT7   WITH CUT7+m.Qty7 ,;
        CUT8   WITH CUT8+m.Qty8 ,;
        TOTCUT WITH TOTCUT+m.TOTQTY
      UNLOCK
      SELECT (lcCutPick)
      =RLOCK()
      REPLACE nSteps WITH 2
      UNLOCK
    ENDIF
    m.CTKTNO     = lcCTNumber
    m.cTktLineNo = STR(CutTktL.LineNo,6)
    IF &lcCutPick..nSteps < 3
      INSERT INTO CUTPICK FROM MEMVAR
      SELECT (lcCutPick)
      =RLOCK()
      REPLACE nSteps WITH 3
      UNLOCK
    ENDIF
  ENDSCAN
  REPLACE ALL CTKTNO WITH lcCTNumber ;
    FOR CTKTNO+CDIVISION+SEASON+STYLE+CWARECODE+DYELOT = ;
    lcCutTKt+lcDivision+lcSeason+CutTktL.Style+;
    CutTktL.cWareCode+CutTktL.Dyelot AND TOTQTY > 0
  *E301182,1 (End)
ENDSCAN
*E301182,1 Following lines have been commented out
*SELECT (lcCutPick)
**B602549,1 Fix updating ticket lines
**=SEEK(lcCutTKt+lcDivision+lcSeason)
**SCAN REST WHILE CTKTNO+CDIVISION+SEASON+STYLE+CWARECODE+DYELOT=;
*                lcCutTKt+lcDivision+lcSeason FOR TOTQTY > 0
*=SEEK(lcCutTKt+lcDivision+lcSeason+lcStyle)
*SCAN REST WHILE CTKTNO+CDIVISION+SEASON+STYLE+CWARECODE+DYELOT=;
*                lcCutTKt+lcDivision+lcSeason+lcStyle FOR TOTQTY > 0
**B602549,1 (End)
*  SCATTER MEMVAR
*  SELECT ORDHDR
*  =SEEK('O'+m.ORDER)
*  IF &lcCutPick..nSteps < 1
*    =RLOCK()
*    REPLACE TotCut WITH TotCut + m.TotQty
*    UNLOCK
*    SELECT (lcCutPick)
*    =RLOCK()
*    REPLACE nSteps WITH 1
*    UNLOCK
*  ENDIF
*  SELECT ORDLINE
*  =SEEK('O'+m.ORDER+m.CORDLINE)
*  IF &lcCutPick..nSteps < 2
*    =RLOCK()
*    REPLACE Dyelot WITH m.Dyelot    ,;
*            CUT1   WITH CUT1+m.Qty1 ,;
*            CUT2   WITH CUT2+m.Qty2 ,;
*            CUT3   WITH CUT3+m.Qty3 ,;
*            CUT4   WITH CUT4+m.Qty4 ,;
*            CUT5   WITH CUT5+m.Qty5 ,;
*            CUT6   WITH CUT6+m.Qty6 ,;
*            CUT7   WITH CUT7+m.Qty7 ,;
*            CUT8   WITH CUT8+m.Qty8 ,;
*            TOTCUT WITH TOTCUT+m.TOTQTY
*    UNLOCK
*    SELECT (lcCutPick)
*    =RLOCK()
*    REPLACE nSteps WITH 2
*    UNLOCK
*  ENDIF
*  m.CTKTNO = lcCTNumber
*  IF &lcCutPick..nSteps < 3
*    INSERT INTO CUTPICK FROM MEMVAR
*    SELECT (lcCutPick)
*    =RLOCK()
*    REPLACE nSteps WITH 3
*    UNLOCK
*  ENDIF
*ENDSCAN
*REPLACE ALL CTKTNO WITH lcCTNumber ;
*        FOR CTKTNO+CDIVISION+SEASON+STYLE+CWARECODE+DYELOT = ;
*            lcCutTKt+lcDivision+lcSeason+lcStyle AND TOTQTY > 0
*E301182,1 (End)
IF !SEEK(lcCTNumber,'CUTTKTH')
  SELECT (lcCuttktH)
  SCATTER MEMVAR
  m.Pcs_Opn  = m.Pcs_Bud
  m.CutTKt   = lcCTNumber
  m.cMultiLot= 'S'
  =RLOCK()
  REPLACE nSteps WITH 1
  UNLOCK
  m.cAdd_User = gcUser_id
  m.cAdd_Time = TIME()
  m.dAdd_Date = gdSysDate
  INSERT INTO CUTTKTH FROM MEMVAR
ELSE
  IF &lcCuttktH..nSteps < 1
    SELECT CUTTKTH
    =RLOCK()
    REPLACE RECFLAG WITH '' ,;
      TotOrd  WITH TotOrd + &lcCuttktH..TotOrd
    UNLOCK
    SELECT (lcCuttktH)
    =RLOCK()
    REPLACE nSteps WITH 1
    UNLOCK
  ENDIF
ENDIF
  *E301426,1 SSH 
  *--- Get the module seting for generating cost sheet
  *--- if this seting is alwayes ("A") we will call function to call
  *--- C/T,PO's Cost Sheet program to generate Cost Sheet.
  llDumy = laScrMode[4] .AND. llGnCTBom .AND. lfGenCutBom(ALLTRIM(CUTTKTH.Style),CUTTKTH.CutTkt)
  *E301426,1 SSH [END]
ENDSCAN
REPLACE ALL CutTkt WITH cTmpCutTkt
GO TOP

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
PRIVATE lcCTNumber,lcCutTKt,lcDivision,lcSeason

*-- if the system uses the dyelot
IF llDyelot
  SELECT(lcCuttktL)
  SCAN FOR EMPTY(DYELOT)
    *-- if the style is dyelot yes and the dyelot field is empty
    *-- give the user a message (Dyelot field cannot be empty)
    IF SEEK(STYLE,'STYLE') AND (STYLE.CDYE_FLG = 'Y')
      *-- You have to enter dyelot for the required dyelots records
      =gfModalGen('INM38135B38018','DIALOG','C/Ts')
      llCSave   = .F.
      RETURN
    ENDIF
  ENDSCAN
ENDIF
SELECT(lcCuttktH)
SCAN FOR Pcs_Bud > 0
  *B602263,1 Fix Updating CT lines from plan
  *=SEEK(STYLE,'STYLE')
  lcStyle = SUBSTR(STYLE,1,lnMajorLen)
  =SEEK(lcStyle,'STYLE')
  *B602263,1 (End)

  lcCutTKt   = CutTkt
  lcDivision = STYLE.CDIVISION
  lcSeason   = Season
  lcCTNumber = IIF(EMPTY(CutTkt),IIF(EMPTY(cTmpCutTkt),;
    gfSequence("CUTTKT","","",lcDivision),cTmpCutTkt),CutTkt)
  =RLOCK()
  REPLACE cTmpCutTkt WITH lcCTNumber
  UNLOCK
  lcFirstCT = IIF(EMPTY(lcFirstCT),lcCTNumber,lcFirstCT)
  lcLastCt  = lcCTNumber
  SELECT (lcCuttktL)
  =SEEK(lcCutTKt+lcDivision+lcSeason+lcStyle)
  SCAN REST WHILE CUTTKT+CDIVISION+SEASON+STYLE+DYELOT=;
      lcCutTKt+lcDivision+lcSeason+lcStyle FOR TOTQTY > 0
    SCATTER MEMVAR
    =SEEK(m.STYLE,'STYLE')
    IF !SEEK(m.STYLE+m.CWARECODE+SPACE(10),'STYDYE')
      WAIT "Assigning Style"+ALLTRIM(m.STYLE)+" to warehouse "+ALLTRIM(m.cWareCode) WINDOW NOWAIT
      DO gpAdStyWar WITH m.STYLE,SPACE(10),m.cWareCode
      WAIT CLEAR
      =SEEK(m.STYLE+m.CWARECODE+SPACE(10),'STYDYE')
    ENDIF
    IF STYLE.lInvSty AND &lcCuttktL..nSteps < 1
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
      SELECT (lcCuttktL)
      =RLOCK()
      REPLACE nSteps WITH 1
      UNLOCK
    ENDIF
    IF STYLE.lInvSty AND &lcCuttktL..nSteps < 2
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
      SELECT (lcCuttktL)
      =RLOCK()
      REPLACE nSteps WITH 2
      UNLOCK
    ENDIF
    IF !SEEK(lcCTNumber+m.style+m.dyelot+m.trancd,'CUTTKTL')
      m.nCost1 = STYLE.nMCost1
      m.nCost2 = STYLE.nMCost2
      m.nCost3 = STYLE.nMCost3
      m.nCost4 = STYLE.nMCost4
      m.nCost5 = STYLE.nMCost5
      m.CUTTKT = lcCTNumber
      INSERT INTO CUTTKTL FROM MEMVAR
    ENDIF
  ENDSCAN
  IF !SEEK(lcCTNumber,'CUTTKTH')
    SELECT (lcCuttktH)
    SCATTER MEMVAR
    m.Pcs_Opn = m.Pcs_Bud
    m.CutTKt  = lcCTNumber
    INSERT INTO CUTTKTH FROM MEMVAR
  ENDIF
ENDSCAN
REPLACE ALL CutTkt WITH cTmpCutTkt
GO TOP

*!*************************************************************
*! Name      : lfFabPrint
*! Developer : Wael ALy Mohamed
*! Date      : 10/10/1998
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
PRIVATE lnAlias

lnAlias = SELECT()
lcRPOrd   = 'B'

*B802607,1 Variables used in option grid to hold the report format
*          and [start]
lcSumDeta = 'D'
lcTmp     = ''
lcFabExpr = ''
lcOrdExpr = ''
lcOGPlatForm = 'WINDOWS'
lcFrxNam     = 'MFGENC1.FRX'
*B802607,1 Variables used in option grid to hold the report format [end]

lcExpr = gfOpGrid('MFGENC1',.T.)
*B802607,1 Change these lines to add new option in the report that will
*          print summary or detail  [start]

*  *B602503,1 Get order open quantity instead of ordered quantity
*  *SELECT *,IIF(cSelect='�',nRequired,0) AS nAllocated FROM (lcTmpOrd) WHERE &lcExpr INTO CURSOR (lcPrintAll) ;
*  ORDER BY FABRIC,cFabClr,Account,Style,Order,Store
*  SELECT *,lfGetpcs() AS nPieces,IIF(cSelect='�',nRequired,00000000.000) AS nAllocated FROM (lcTmpOrd) WHERE &lcExpr INTO CURSOR (lcPrintAll) ;
*  ORDER BY FABRIC,cFabClr,Account,Style,Order,Store
*  *B602503,1 (End)
*
*  IF _TALLY > 0
*    SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER
*    SET RELATION TO Fabric+cFabClr INTO Fabric ADDITIVE
*    IF pSetup(.T.)
*      *B602437,1 Fix report path
*      *DO gfDispRep WITH (gcRepHome+'MFGENC1.FRX')
*      DO gfDispRep WITH ('MFGENC1.FRX')
*      *B602437,1 (End)
*    ENDIF
*  ENDIF
*  USE IN (lcPrintAll)

*--If choose RUN from the option grid
IF TYPE('lcExpr') <> 'L'
  SELECT (lcFabrics)
  SET RELATION TO && Reset the orginal relation
  CREAT CURSOR lcTmp (cType C(1)) && A cursor that used in the .FRX
  APPEND BLANK
  REPLACE cType WITH 'F'
  SET RELATION TO IIF(cType = 'F' , '' , '*') INTO (lcFabrics)
  SET SKIP TO (lcFabrics)
  *-- If choose RUN with any filters
  IF lcExpr <> '.T.'
    *-- Pripare the filter expretion
    lcFabExpr = lcExpr
    lcOrdExpr = lcExpr
    lcFabExpr = STRTRAN(lcFabExpr,'FABRIC.FABRIC', lcFabrics+'.Fabric')
    lcFabExpr = STRTRAN(lcFabExpr,'FABRIC.COLOR', lcFabrics+'.Color')
    lcOrdExpr = STRTRAN(lcOrdExpr,'FABRIC.FABRIC',lcTmpOrd+'.Fabric')
    lcOrdExpr = STRTRAN(lcOrdExpr,'FABRIC.COLOR',lcTmpOrd+'.cFabClr')
    *lcExpr = '(EOF(lcTmpOrd) AND ' + lcFabExpr + ') OR (EOF(lcFabrics) AND ' + lcOrdExpr + ')'
    *-- we will print the report in detail



    IF lcSumDeta = 'D'
      SELECT (lcTmpOrd)
      SET RELATION TO   && Reset the orginal relation
      *-- In case we well print ALLOCATED or UNALOCATED orders
      DO CASE
      CASE lcRPOrd = 'A'  && WE will print allocated orders only
        lcOrdExpr = lcOrdExpr + ' AND ' + EVALUATE('lcTmpOrd') + ".cSelect= '�' "
      CASE lcRPOrd = 'U'   && WE will print UNallocated orders only
        lcOrdExpr = lcOrdExpr + ' AND ' + EVALUATE('lcTmpOrd') + ".cSelect= ' ' "
      ENDCASE
      SELECT lcTmp
      APPEND BLANK
      REPLACE cType WITH 'O'
      SET RELATION TO IIF(cType = 'O' , '' , '*') INTO (lcTmpOrd) ADDITIVE
      SET SKIP TO (lcTmpOrd) , (lcFabrics)
    ENDIF && end of (IF lcSumDeta = 'D')

    lcExpr = '(EOF(lcTmpOrd) AND ' + lcFabExpr + ') OR (EOF(lcFabrics) AND ' + lcOrdExpr + ')'

    SELECT lcTmp
    IF pSetup(.T.)
      lcCons = SET('CONS')
      SET CONS OFF
      *B803686 RAMY [start]
      *DO gfDispRe WITH gcRepHome + EVALUATE('lcFrxNam') , 'FOR ' + lcExpr
      DO (gcRepHome + IIF(lcChoice = 'P' , 'POREPORT.APP' , 'MFREPORT.APP')) WITH ;
         'MFPRGNCT' , .T.
      *B803686 RAMY [end]
      SET CONS &lcCons
    ENDIF
  ELSE  && We choose RUN without any filters
    *-- we will print the report in detail
    IF lcSumDeta = 'D'
      lcOrdExpr = '.T.'
      SELECT (lcTmpOrd)
      SET RELATION TO   && Reset the orginal relation
      *-- In case we well print ALLOCATED or UNALOCATED orders
      DO CASE
      CASE lcRPOrd = 'A'  && WE will print allocated orders only
        lcOrdExpr = EVALUATE('lcTmpOrd') + ".cSelect= '�' "
      CASE lcRPOrd = 'U'   && WE will print UNallocated orders only
        lcOrdExpr = EVALUATE('lcTmpOrd') + ".cSelect= ' ' "
      ENDCASE
      SELECT lcTmp
      APPEND BLANK
      REPLACE cType WITH 'O'
      SET RELATION TO IIF(cType = 'O' , '' , '*') INTO (lcTmpOrd) ADDITIVE
      SET SKIP TO (lcTmpOrd) , (lcFabrics)

      lcExpr = '(EOF(lcTmpOrd)) OR (EOF(lcFabrics) AND ' + lcOrdExpr + ')'

    ENDIF && end of IF lcSumDeta = 'D'

    IF pSetup(.T.)
      lcCons = SET('CONS')
      SET CONS OFF
      *-B803686 RAMY [start]
      *DO gfDispRe WITH gcRepHome + EVALUATE('lcFrxNam') , 'FOR ' + lcExpr
      DO (gcRepHome + IIF(lcChoice = 'P' , 'POREPORT.APP' , 'MFREPORT.APP')) WITH ;
         'MFPRGNCT' , .T.
      *-B803686 RAMY [end]
      SET CONS &lcCons
    ENDIF
  ENDIF && end of (IF lcExpr <> '.T.')
ENDIF && end of (IF TYPE('lcExpr') <> 'L')
*-- Close the cursor and restore the old relations
*-- in case of reset the relation [start]
IF USED(lcTmp)
  SET RELATION TO
  USE IN lcTmp
  SELECT (lcFabrics)
  SET RELATION TO Fabric + COLOR INTO (lcTmpOrd)
ENDIF
SELECT (lcTmpOrd)
*To restore the old relation If we were print detail format
IF EMPTY(RELATION(1))
  SET RELATION TO STYLE+&lcFabrics..Dyelot INTO (lcStyUnAll)
  SET RELATION TO 'M'+Account INTO CuSTOMER ADDITIVE
ENDIF  && end of (IF EMPTY(RELATION(1)))
*-- Close the cursor and restore the old relations [end]

*B802607,1 Add new option in the report that will
*          priont summary or detail  [end]

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvSelFabr
*! Developer : Wael ALy Mohamed
*! Date      : 10/10/1998
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
PRIVATE lcObjNam,lcObjVal

lcObjNam = SYS(18)
lcObjVal = ALLTRIM(EVALUATE(SYS(18)))
IF !EMPTY(lcObjVal) .AND. !SEEK(lcObjVal,'Fabric')
  lcObjVal = IIF(FABROW(@lcObjVal,'*',.T.),lcObjVal,SPACE(7))
ENDIF
&lcObjNam = lcObjVal

*!*************************************************************
*! Name      : lfPrnSumm
*! Developer : Wael ALy Mohamed
*! Date      : 10/10/1998
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
PRIVATE lnAlias

lnAlias = SELECT()
SET ORDER TO TAG 'Cutord' IN CUTPICK
CREATE CURSOR (lcPrintAll) ;
  (cPrntOrd C(1),Fabric C(7),cFabClr C(6),cFabWare C(6),Dyelot C(10), ORDER C(6),;
  CutTKt C(6),TotQty N(7),nRequired N(12,3),Account C(5),STYLE C(19),STORE C(8))
INDEX ON cPrntOrd+FABRIC+cFabClr+cFabWare+Dyelot+Account+STYLE+ORDER+STORE TAG (lcPrintAll)
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER
SET RELATION TO Fabric+cFabClr INTO Fabric ADDITIVE
SET RELATION TO Fabric+cFabClr+cFabWare+Dyelot INTO FabDye ADDITIVE

SELECT (lcTmpOrd)
SET ORDER TO TAG 'TICKET'
=SEEK('�')
SCAN REST WHILE cSelect = '�'
  SCATTER MEMVAR
  SELECT CUTPICK
  =SEEK(IIF(lcChoice='C','1','2')+&lcTmpOrd..order+STR(&lcTmpOrd..LineNo,6))
  SCAN REST WHILE trancd+ORDER+cordline = ;
      IIF(lcChoice='C','1','2')+&lcTmpOrd..order+STR(&lcTmpOrd..LineNo,6) ;
      FOR SEEK(cTKtNo,lcCutPick)
    INSERT INTO (lcPrintAll) ;
      (cPrntOrd,Fabric,cFabClr,cFabWare,Dyelot,ORDER,CutTKt,TotQty,nRequired,;
      Account,STYLE,STORE) VALUES ;
      ('0',m.Fabric,m.cFabClr,m.cFabware,m.Dyelot,m.Order,CUtPICK.cTKtNo,;
      CUtPick.TotQty,CUtPick.TotQty*m.nYeild,m.Account,m.Style,m.Store)
  ENDSCAN
ENDSCAN
=SEEK(' ')
SCAN REST WHILE cSelect = ' '
  SCATTER MEMVAR
  *B602503,1 Get order open quantity instead of ordered quantity
  m.TotQty=lfGetpcs()
  *B602503,1 (End)

  INSERT INTO (lcPrintAll) ;
    (cPrntOrd,ORDER,TOtQty,nRequired,Account,STYLE,STORE) VALUES ;
    ('1',m.Order,m.TotQty,m.TotQty*m.nYeild,m.Account,m.Style,m.Store)
ENDSCAN
SELECT (lcPrintAll)
GO TOP
IF pSetup(.T.)
  *B602437,1 Fix report path
  *DO gfDispRep WITH (gcRepHome+'MFGENC.FRX')
  *-B803686 RAMY [start]
  *DO gfDispRep WITH ('MFGENC.FRX')
  lcFrxNam = 'MFGENC.FRX'
  lcOGPlatForm = 'WINDOWS'
  DO (gcRepHome + IIF(lcChoice = 'P' , 'POREPORT.APP' , 'MFREPORT.APP')) WITH ;
      'MFPRGNCT' , .T.
  *-B RAMY [end]
  *B602437,1 (End)
ENDIF
USE IN (lcPrintAll)
SELECT (lnAlias)

*!*************************************************************
*! Name             : lfChngIndx
*! Developer        : Ramy Mabrouk
*! Date             : 09/15/1999
*! Purpose          : Change and restore the index of the style file
*!                    for browse in range
*! Tracking Entry # : B802601,1
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
*! Name             : lfvRepForm
*! Developer        : Ramy Mabrouk
*! Date             : 09/27/1999
*! Purpose          : To disable the selection in order type in the
*!                    option grid
*! Tracking Entry # : B802607,1
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
*! Name             : lfvBroWCt
*! Developer        : Ahmed Salah Shalaby - (SSH)
*! Date             : 09/27/1999
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
*B603516,1 SSH 14/02/2000 Valid function for cuttkt/PO
FUNCTION lfvBroWCt

PRIVATE lnOldAls , lcNewCutNo , llRet , lcToSeekIn , lcStrToAdd , lcHdrVal

lnOldAls = SELECT(0)

IF !llGManualy
  IF lcChoice = 'P'
    REPLACE PO     WITH ''
  ELSE
    REPLACE CUtTkt WITH ''
  ENDIF
  RETURN
ENDIF
IF lcChoice = 'P'
  lcHdrVal   = PO+CDIVISION+CPURCODE+CSTYGRADE
  lcNewCutNo = IIF(!EMPTY(PO),'P'+PO,PO)
  lcToSeekIn = 'POSHDR'
  lcStrToAdd = "P/O"
ELSE
  lcHdrVal   = CUTTKT+CDIVISION+SEASON+SUBSTR(STYLE,1,lnMajorLen)
  lcNewCutNo = CutTkt
  lcToSeekIn = 'CutTktH'
  lcStrToAdd = "Cutting Ticket"
ENDIF
llRet = .F.
IF EMPTY(lcNewCutNo)
  lnNoThing = gfModalGen('TRM38047B00000','DIALOG',lcStrToAdd)
  *B603521,1 SSH 10/04/00   Fix the bug Alias '' Not Found. [Begin]
  IF lcChoice = 'P'
    REPLACE PO     WITH lcOldCtkNo
  ELSE
    REPLACE CUtTkt WITH lcOldCtkNo
  ENDIF
  *B603521,1 SSH 10/04/00   Fix the bug Alias '' Not Found.[End]
ELSE
  IF LEN(ALLTRIM(lcNewCutNo))-IIF(lcChoice='P',1,0) < 6
    lnNoThing = gfModalGen('TRM38048B00000','DIALOG',lcStrToAdd)
    *B604515,1 KHM 06/05/2001 (Begin) Replacing the PO or CT with space
    IF lcChoice = 'P'
      REPLACE PO     WITH lcOldCtkNo
    ELSE
      REPLACE CUtTkt WITH lcOldCtkNo
    ENDIF
    *B604515,1 KHM 06/05/2001 (End)

  ELSE
    IF SEEK(lcNewCutNo,lcToSeekIn) .OR. SEEK(IIF(lcChoice='P',SUBSTR(lcNewCutNo,2),lcNewCutNo),lcLinFile)
      lnNoThing = gfModalGen('TRM38049B00000','DIALOG',lcStrToAdd)
      lcNewCutNo  = ''
      SELECT (lcHdrFile)
      IF lcChoice = 'P'
        REPLACE PO     WITH lcOldCtkNo
      ELSE
        REPLACE CUtTkt WITH lcOldCtkNo
      ENDIF
    ELSE
      llRet = .T.
    ENDIF
  ENDIF
ENDIF
IF llRet
  SELECT (lcLinFile)
  IF !SEEK(lcHdrVal)
    =SEEK(lcOldCtkNo+SUBSTR(lcHdrVal,7))
    IF lcChoice = 'P'
      *B604515,1 KHM 06/05/2001 (Begin) changing the replacement in order
      *B604515,1                to replace all the lines instead of the 
      *B604515,1                first one only.      
      *REPLACE PO     WITH SUBSTR(lcNewCutNo,2)
      REPLACE PO WITH SUBSTR(lcNewCutNo,2) FOR +;
                     PO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+DYELOT = +;
                     lcOldCtkNo+SUBSTR(lcHdrVal,7)
      *B604515,1 KHM 06/05/2001 (End)
      
    ELSE
      *B604515,1 KHM 06/05/2001 (Begin) changing the replacement in order
      *B604515,1                to replace all the lines instead of the 
      *B604515,1                first one only.      
      *REPLACE CUtTkt WITH lcNewCutNo
      REPLACE CUtTkt WITH lcNewCutNo FOR +;
              CUTTKT+CDIVISION+SEASON+STYLE+DYELOT = +;
              lcOldCtkNo+SUBSTR(lcHdrVal,7)
      *B604515,1 KHM 06/05/2001 (End)
    ENDIF
  ENDIF
  *--- cTKtNo
  *B603521,1 SSH 10/04/00   Fix the bug Alias '' Not Found. [Begin]
  IF lcChoice = 'L'
    SELECT (lcHdrFile)
  ELSE
    SELECT (lcCutPick)
  ENDIF
  *B603521,1 SSH 10/04/00   Fix the bug Alias '' Not Found. [End]
  
  *B604515,1 KHM 06/05/2001 (Begin) Adding a new variable (lcPoCtNo) to 
  *B604515,1                hold the either the PO or the CT. Also adding
  *B604515,1                a new variable (lcPoCtExp) to hold the full 
  *B604515,1                expression either for PO or for CT.
  *IF !SEEK(lcNewCutNo)
  lcPoCtNo  = IIF(lcChoice = 'P', SUBSTR(lcNewCutNo,2), lcNewCutNo)
  lcPoCtExp = IIF(lcChoice = 'P',;
                  'CTKTNO+CDIVISION+CPURCODE+CSTYGRADE+STYLE+CWARECODE+DYELOT',;
                  'CTKTNO+CDIVISION+SEASON+STYLE+CWARECODE+DYELOT')
  IF !SEEK(lcPoCtNo)
  *B604515,1 KHM 06/05/2001 (End)
  
    =SEEK(lcOldCtkNo+SUBSTR(lcHdrVal,7))
    *B604515,1 KHM 06/05/2001 (Begin) Changing the replace in order to replace
    *B604515,1                all the lines instead of the first one only.
    *REPLACE cTKtNo  WITH lcNewCutNo
    REPLACE cTKtNo  WITH lcPoCtNo FOR &lcPoCtExp = +;
                                      lcOldCtkNo+SUBSTR(lcHdrVal,7)
    *B604515,1 KHM 06/05/2001 (End)
  ENDIF
ENDIF
SELECT(lnOldAls)

*!*************************************************************
*! Name             : lfwOldCtkt
*! Developer        : Ahmed Salah Shalaby - (SSH)
*! Date             : 09/27/1999
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
*B603516,1 SSH 14/02/2000 Valid function for cuttkt/PO
FUNCTION lfwOldCtkt

lcOldCtkNo = IIF(lcChoice = 'P' ,PO ,CutTkt)


*!*************************************************************
*! Name             : lfCheckMaj
*! Developer        : Ahmed Salah Shalaby - (SSH)
*! Date             : 09/27/1999
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
*B602781,1 SSH 09/04/00 *** Begin *** Fix the bug Variable lcSeg not found
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
*B602781,1 SSH 09/04/00 *** End ***



*!*************************************************************
*! Name             : lfGenCutBom   E301426
*! Developer        : Ahmed Salah Shalaby - (SSH)
*! Date             : 11/06/2000
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
*E301426,1 SSH 
FUNCTION lfGenCutBom
PARAMETER lcCtktSty  , lcGenCTkt

PRIVATE lnOldAls , llByMe

IF lcChoice <> 'L'
  lnOldAls = SELECT(0)
  llByMe = gfOpenFile(gcDataDir+'Bom',gcDataDir+'Bom','SH')
  llFromBom = .T.
  IF SEEK(lcCtktSty, "Bom")
    IF lcChoice = 'P'
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
      *DO gpDoProg WITH "AWRPOCSSH", .F., "PO", lcParameter
    ELSE
      lcParameter = "'M','" + lcGenCTkt + "'"+",.T."
      llFunCal = .T.
      lcOldBaseW = gcBaseWind
      gcBaseWind = "AWRMFCSSH"
      lcBaseFile = 'CUTTKTH'
      glFirsTime = .T.
      DO gcAppHome+"MFCSSH.FXP" WITH 'M',lcGenCTkt,.T.
      gcBaseWind = lcOldBaseW
      lcBaseWind = lcOldBaseW
      lcBaseFile = ''
      glFirsTime = .F.
      laScrMode  = .F.
      laScrMode[4] = .T.
    ENDIF
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
ENDIF

*!*************************************************************
*! Name             : lfGetOrd
*! Developer        : Hend Ghanem (HBG)
*! Date             : 18/09/2000
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
*B803635,1
FUNCTION lfGetOrd
PRIVATE lnInd

STORE "" TO lcFOrder , lcTOrder 
lnInd = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'ORDHDR.ORDER'),1)
IF lnInd <> 0
  lcFOrder = ALLTRIM(LEFT(laOgFxFlt[lnInd,6],6))
  lcTOrder = ALLTRIM(RIGHT(laOgFxFlt[lnInd,6],6))
ENDIF

*!*************************************************************
*! Name             : lfRetToSel
*! Developer        : Hend Ghanem (HBG)
*! Date             : 19/09/2000
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
*B603794,1
FUNCTION lfRetToSel 
*B604029,1 HBG 11/26/2000 We don't need the parameter because we'll 
*B604029,1                call it from the select mode [Begin]
*PARAMETER llSave
*B604029,1 [End]
PRIVATE lcBrowFile 

*B604029,1 HBG 11/26/2000 Comment the following block because we won't check if
*B604029,1                we'll save or not this function is called from the 
*B604029,1                select mode [Begin]
*IF !llSave
 * llRetToSel = .T.
 * RETURN
*ENDIF
*B604029,1 [End]

lcBrowFile = IIF(lcChoice='L',lcStyTmp,lcOrdLine)
*-- Delete the previous selected order or orders
SELECT (lcBrowFile)
SET ORDER TO TAG TICKET
*B604011,1 HBG 11/05/2000 Cet the for expression according to if we 
*B604011,1                generate C\T from Sales order or plan [Begin]
IF lcChoice='L'
  lcForExpr = 'CSELECT+CDIVISION+SEASON+STYLE'
ELSE
  lcForExpr = IIF(lcChoice='C','CSELECT+CDIVISION+SEASON+STYLE+DYELOT',;
                         'CSELECT+CDIVISION+CPURCODE+CSTYGRADE+STYLE+DYELOT')
ENDIF
*B604011 [End]
=SEEK("�")
*B604011,1 HBG 11/05/2000 Scan for "lcForExpr" [Begin]
*SCAN REST FOR cSelect+cDivision+Season+STYLE+Dyelot = "�"
SCAN REST FOR &lcForExpr = "�"
*B604011 [End]
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
*B604011,1 HBG 11/05/2000 Call Browes function depinding on 
*B604011,1                if we generate C\T from Sales order or plan[Begin]
*=lfBrowOrd()
=IIF(lcChoice='L',lfStyBrow(),lfBrowOrd())
*B604011 [End]
SELECT (lcHdrFile)
DELETE ALL

*-- To delete the records that found in the PO Line (Temporary file)
IF lcChoice = 'P'
  IF USED(lcPoLine)
    SELECT(lcPoLine)
    DELETE ALL
  ENDIF  
ELSE
  *-- To delete the records that found in the CutTkt Line (Temporary file)
  IF USED(lcCUTTKTL)
    SELECT(lcCUTTKTL)
    DELETE ALL
  ENDIF  
ENDIF

=lfBrowCut()
lcFirstCT = ""
lcLastCt  = ""

*-- End of lfRetToSel 