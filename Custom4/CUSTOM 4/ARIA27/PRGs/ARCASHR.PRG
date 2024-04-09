*:----------------------------------------------------------------
*: Program file        : ARCASHR.PRG
*: Program description : Cash Receipt - (Customer Payments)
*: For screen          : ARCASHR.SPR
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : Account Receivable - (AR)
*: Developer Name      : Yasser Mohammed Aly
*: Tracking Job Number : E300697
*:----------------------------------------------------------------
*: Calls               : 
*:----------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------
*: Example             : DO ARCASHR
*:----------------------------------------------------------------
*: Modification
*E300921,1  TAK 07/15/98 Added to update the APPAYMNT file in case
*E300921,1               of PS Module was installed.
*E300921,1               Disable the Checking account if the selected
*E300921,1               bank type was Safe. 
*E301077,52 YMA 03/03/99 Opening Files.
*B602666,4  YMA 03/24/99 Added the CARPTYPE field in the master CREDIT 
*B602666,4               file to be able to save the payment type 
*B602666,4               for each payment transaction.
*B602788,1  MAN 04/14/99 IF there is  AP Link Disable the NEW button until
*B602788,1               the checking account is entered
*B602954,1  AHM 05/31/99 After select the bank code the screen is disable
*B602954,1               (you can't press new)
*E301245,1 SSE 06/09/1999 Added a new field nHgWtrMark and calcualte its value
*                         from NETBAL field , if less than NETBAL update with 
*                         NETBAL else leave it with existing value
*E301266,1 AKA 06/23/99 Adding one more field in  APPAYMNT file (batch) and update it 
*E301266,1              with the value of CREDIT.BATCH 
*E301281,1 AHM 07/06/99 Allow automatic key off from cash receipt
*B603056,1 RENEE 07/13/99 1. Show/Update payment type popup whether or not
*B603056,1 					 we have a G/L link.
*B603056,1 				  2. Update the Credit description with the description
*B603056,1 				     of the payment type.	 
*B603091,1 TAK 08/04/99 Add the ability to print the recipt if active module was PS.
*B603127,1 WALID ABOU EL MAGD 08/25/99 Fix the bug all transactions that made
*B603127,1 In the same sesion take the same batch number     
*B603187,1 RENEE 10/05/1999 Allow a tape total between 0 and 1
*B802696,1 BWA 14/10/1999 Fix the bug which is if the bank has many check account
*B802696,1                the user can not select any other account the system always takes the last account no.
*B603352,1 SSH 26/12/1999 fix the bug of goto top in browse file after keyed off.
*B603550,1 BWA 04/05/2000 Fix the bug of Alias "SYCCURR" not found.
*B603639,1 NAD 05/14/2000 Fix bug  "attemping to lock"  Message is displayed preventing
*B603639,1                cash receipt entry when more than one user uses the program
*E301420,1 NAD 07/06/2000 increase the Amount Field picture to be '99999999999.99'
*B603055,1 NAD 08/29/2000 1 - Fix Bug Cash receipts save the some lines twice.      
*B603055,1                2 - Currency unit must not be equal to O.
*B602838,1 NAD 09/24/2000 Not to allow transactions for hold account. 
*E301521,1 AHM 01/01/2001 Extract the saving function in individulal program to be used from the NC module
*B607231,1 KHM 06/15/2003 Adding the factor code to the screen.
*B124037,1 NNA 09/22/2004 Fix bug that when you make a Cash Receipt with a 'Non AR Cash' , you'll find
*B124037,1 NNA            that the Check No don't save ( Fixed in the Arcashr5.scx by Adding the Valid Function)
*:--------------------------------------------------------------------------

EXTERNAL ARRAY laCtrStat, laDefProc

DIMENSION laScrMode[4], laType[1]
llNoThing  = lfBldType(.T.)

*-- To force the control pannel to call the local close 
*-- procedure(lpClsScr)
laDefProc[10] = .F.

DIMENSION laArPayT[1], laNArPayT[1]
laArPayT = SPACE(0)

DIMENSION laCodeInf1 [1,10]
laCodeInf1 = SPACE(0)
laCodeInf1[1,01] = "CARPTYPE"
laCodeInf1[1,02] = "laArPayT"
laCodeInf1[1,03] = "lnArPayT"
laCodeInf1[1,04] = ""
laCodeInf1[1,05] = .F.
laCodeInf1[1,06] = .F.
laCodeInf1[1,07] = ""
laCodeInf1[1,08] = ""
laCodeInf1[1,09] = ""
laCodeInf1[1,10] = ""

DIMENSION laCodeInf2 [1,10]
laCodeInf2 = SPACE(0)
laCodeInf2[1,01] = "CARPTYPE"
laCodeInf2[1,02] = "laNArPayT"
laCodeInf2[1,03] = "lnNArPayT"
laCodeInf2[1,04] = ""
laCodeInf2[1,05] = .F.
laCodeInf2[1,06] = .F.
laCodeInf2[1,07] = ""
laCodeInf2[1,08] = ""
laCodeInf2[1,09] = ""
laCodeInf2[1,10] = ""

*-- Used screen set windows names.
lcChWin2   = SPACE(1)
lcChWin3   = SPACE(1)
lcChWin4   = SPACE(1)
lcChWin5   = SPACE(1)
lcChWin6   = SPACE(1)
lcChWin7   = SPACE(1)
lcBrWind   = SPACE(1)

*-- Main temproray file name.
lcTmpCredt = SPACE(1)

*-- System setup variables.
llGLLink   = (ALLTRIM(gfGetMemVar("M_LINK_GL")) = "Y")
llApLink   = (OCCURS('AP',gcCmpModules)<>0) OR (OCCURS('PS',gcCmpModules)<>0)
llMltCur   = SPACE(1)
llEditExRt = SPACE(1)

glFromBrow = .F.                       && I'm not comming from any browse.

*-- The following list of variables will tell if we opened the 
*-- related (conditionaly opened) files or they were opened from
*-- before.

*E301077,52 YMA 03/03/99 Start - There is no need for these variables
*E301077,52 YMA 03/03/99 because the "gfOpenFile" will handel the file close.
*llOpenGL   = .F.
*llOpApPayM = .F.
*llOpApBank = .F.
*llOpApStup = .F.
*llOpApChec = .F.
*llOpSycCur = .F.
*E301077,52 YMA 03/03/99 end.

*-- Save the original Escape button trapping in this variable
*-- just to re-load it when going out of the browse window.
lcEscTrap  = ON("KEY","ESC")

*-- These variables are to tell the status of all the objects
*-- according to the object type.
lcTrpStat  = "ENABLE"               && Trapping Invisable Buttons.
lcKeyStat  = "ENABLE"               && Key Objects.
lcObjStat  = "DISABLE"              && Normal Objects.
lcKeyOffS  = "DISABLE"              && Key-Off button.

*-- Initialize the used variables in the screen.
llFrmBrow  = .F.

*B607231,1 KHM 06/15/2003 (Begin) Initializing the following variables.
llBrowse = .F.
lcFactor = " "
*B607231,1 KHM 06/15/2003 (End)


*-- Fiscal year and period variables.
lcGLFYear  = SPACE(1)
lcGLPeriod = SPACE(1)

*-- The GL account actual width on the screen.
*-- The variable "lnAcLen" is initialized in the "AR.PRG" program, it tells
*-- the length of any GL account in characters.
lnAccWidth = lnAcLen * 1.150

*-- Variables that hold the red and green colors used to display the 
*-- "Difference" object on the screen.
lcRedClr   = "RGB(,,,255,0,0)"
lcGreenClr = "RGB(,,,0,128,0)"

*-- Variables that hold the values of the objects in the header window.
ldBatDate  = gdSysDate
llValidDat = .F.
lnTapCount = 0
lnActCount = 0
lnTapTotal = 0
lnActTotal = 0
lnDiff     = 0
lcBankCode = SPACE(1)
lcChkAcnt  = SPACE(1)
lcCurCode  = SPACE(1)
lnExRate   = 1
lnCurrUnit = 0
lcUntSin   = '/'
lcExRSin   = '/'

lcOBankCod = SPACE(1)
lcOChkAcnt = SPACE(1)
lcOCurCode = SPACE(1)
lnOExRate  = 1
ldOBatDate = gdSysDate
*B602788,1 Added Variable to indicate if the check code is defaulted from the
*B602788,1 bank selection
llDefChk = .F.
*-- The browse title and it's record number.
lcPayBrTl  = "Payments"
lnRecNo    = 0

*-- Variables that hold the values of the objects in the details screen
*-- (The Browse Edit Regon)
lnType     = 1
lnArType   = 1
lnNArType  = 1
lnAmount   = 0
lcRef      = SPACE(1)
lcAccount  = SPACE(1)
lcAccName  = SPACE(1)
lcCheck    = SPACE(1)
lcGLAcct2  = SPACE(1)

lnOType    = 1
lnOAmount  = 0
lcORef     = SPACE(1)
lcOAccount = SPACE(1)
lcOCheck   = SPACE(1)
lcOGLAcct2 = SPACE(1) 

*-- Following variables are used to deal with the uncomplete program 
*-- session concept.
lcScrMode  = SPACE(1)
lnThisSess = gnProgCopy
lcSession  = SPACE(1)
lcUserID   = PADR(gcUser_id, 10)
lcProgID   = PADR("CASH_RE", 10)
lnUnCmSeRc = 0
llGoAndChk = .T.                       && Can we go and check the existance of any incomplete session from the "lpShow" procedure?
llChkUnCom = .T.                       && Can we check the existance of any incomplete session before drawing the screen (From the screen setup)?

lcBatSeqNo = SPACE(0)

*-- Variables that need to be saved and restored when detecting an
*-- uncomplete program session.
DIMENSION laVars[18]
laVars[01] = "lcScrMode"
laVars[02] = "ldBatDate"
laVars[03] = "llValidDat"
laVars[04] = "lnTapCount"
laVars[05] = "lnActCount"
laVars[06] = "lnTapTotal"
laVars[07] = "lnActTotal"
laVars[08] = "lnDiff"
laVars[09] = "lcBankCode"
laVars[10] = "lcChkAcnt"
laVars[11] = "lcCurCode"
laVars[12] = "lnExRate"
laVars[13] = "lnCurrUnit"
laVars[14] = "lcUntSin"
laVars[15] = "lcExRSin"
laVars[16] = "lcGLAcct2"
laVars[17] = "lnUnCmSeRc"
laVars[18] = "lcBatSeqNo"

IF !gfSetup()
  RETURN
ENDIF  

*-- Get the system settings.
llMltCur   = gfGetMemVar("llMulCurr")
llEditExRt = gfGetMemVar("LLEDITEXRA")

*-- Define this variable just to avoid getting an error message
*-- when swtich the screen mode from the control pannel screen.
*-- Because the Control Pannel screen assumes that the program 
*-- has a header and details file, so it scatters the header file
*-- information useing this variable.
lcScFields   = "Account"

*-- This variable tells if we should re-size the windows defined
*-- or not, it will be true only at the first time you run the
*-- screen.
llFrstTime   = .F.

*-- To control the "Control Panel" show.
llNoShow     = .F.

IF !WEXIST(gcBaseWind)
  *-- This variable tells if we should re-size the windows defined
  *-- or not, it will be true only at the first time you run the
  *-- screen.
  llFrstTime = .T.
  
  *E301077,52 YMA 03/03/99 Start- Open the credit and the customer files.
  = gfOpenFile (gcDataDir+"CREDIT", "CREDIT", "SH")
  = gfOpenFile (gcDataDir+"CUSTOMER", "CUSTOMER", "SH")
  
  *E301077,52 YMA 03/03/99 End.

  *-- None of the screen's objects has been updated.
  llCUpDate  = .F.
  
  *-- Empty the screen base file to avoid geeting an error when 
  *-- switching the screen modes.
  lcBaseFile = SPACE(0)
  
  *-- Windows Names.
  lcChWin2   = gfTempName()
  lcChWin3   = gfTempName()
  lcChWin4   = gfTempName()
  lcChWin5   = gfTempName()
  lcChWin6   = gfTempName()
  lcChWin7   = gfTempName()
  lcBrWind   = IIF(llGLLink AND !llApLink, lcChWin3, lcChWin7)
  lcTmpCredt = gfTempName()

  *B603056,1 Update payment type arrays whether or not we have a GL link
  *IF llGLLink
  *B603056,1 end
    llNoThing = gfwCodePop(@laCodeInf1, "CARPTYPE", "N")
    llNoThing = gfwCodePop(@laCodeInf2, "CARPTYPE", "N")
  *B603056,1 Update payment type arrays whether or not we have a GL link
  *ENDIF
  *B603056,1 end
  
  *E301077,52 YMA 03/03/99 Start- Commented out and moved to the 
  *E301077,52 YMA 03/03/99 payment type valid function.
  *-- Open the GL_Link file if the system is linked with the GL.
  *IF llGLLink
  *  llOpenGL = gfOpenFile (gcDataDir+"GL_LINK", "GL_LINK", "SH")
  *ENDIF

  *-- Open the nedded AP files if the AP module is installed.
  *IF llApLink
  *  llOpApChec = gfOpenFile (gcDataDir+"ApChecks","BankCheck","SH")
  *  llOpApStup = gfOpenFile (gcDataDir+"ApSetup" ,""         ,"SH")
  *  llOpApBank = gfOpenFile (gcDataDir+"ApBanks" ,"BankCode" ,"SH")
  *  llOpApPayM = gfOpenFile (gcDataDir+"ApPayMnt",""         ,"SH")
  *ENDIF
  *E301077,52 YMA 03/03/99 End.

  *-- Set the currency variables according to the setting of
  *-- multi currency.
  IF llMltCur
    lcCurCode  = SPACE(1)
    lnExRate   = 1
    lnCurrUnit = 1

    *E301077,52 YMA 03/03/99 Start- Commented out
    *llOpSycCur = gfOpenFile(gcSysHome+"SycCurr","cCurrCode",'SH')
    *E301077,52 YMA 03/03/99 End.
  ELSE
    lcCurCode  = gcBaseCurr
    lnExRate   = 1
    lnCurrUnit = 1
    lcUntSin   = '/'
    lcExRSin   = '/'
    
    *E301077,52 YMA 03/03/99 Start- Commented out
    *llOpSycCur = .F.
    *E301077,52 YMA 03/03/99 End.
  ENDIF 
  
  *-- Get a session number, to be updated in the uncomplete session
  *-- number file foe each session.
  lcSession  = gfsequence('CSESSION')
  
ENDIF

lnAlias = SELECT(0)

*E301077,52 YMA 03/03/99 Start- Open the credit and the customer files.
= gfOpenFile (gcDataDir+"CODES", "cCode_no", "SH")
*SELECT CODES
*lcTag = TAG()
*SET ORDER TO TAG cCode_no

*B603056,1 Get payment type whether or not we have a GL link
*llcCond = IIF(llGLLink, SEEK("N"+"CARPTYPE"), .T.)
llcCond = SEEK("N"+"CARPTYPE")
*B603056,1 end

*SET ORDER TO &lcTag
*E301077,52 YMA 03/03/99 End.

IF llcCond
  *-- The following part of the code is to...
  *-- 1. Define a new bar in the "File" pad to be used to activate
  *--    the browse at any time of the program.
  *-- 2. Re-Define the "Save" bar process to run the "gfCPSave" written
  *--    in this program as a local "Save" procedure instead or running
  *--    the global save procedure.
  *-- 3. Run the screen.
  *-- 4. Restore the original "Save" bar process.
  PUSH KEY
  PUSH MENU _MSYSMENU
  lnSavBarNo = lfGtSavBar()
  DEFINE BAR 100 OF P01PU01 PROMPT (lcPayBrTl) KEY CTRL+B
  ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcPayBrTl)
  ON SELECTION BAR lnSavBarNo OF P01PU01 DO gfCPSave
  *B603055,1 NAD 08/29/2000  (Start) Trap the Ctrl+Ins to overwrite the global (New) menu pad option.
  ON KEY LABEL Ctrl+Ins DO lpCtrlIns
  *B603055,1 NAD (End)
  DO (gcScrDir+"ARCashR.SPR")
  POP KEY
  POP MENU _MSYSMENU
  RELEASE WINDOW (lcPayBrTl)
  RELEASE BAR 100 OF P01PU01
  *B603055,1 NAD 08/29/2000  (Start) Clear the trap 
  ON KEY LABEL Ctrl+Ins 
  *B603055,1 NAD (End)
ELSE
  glQuitting = .T.
  =gfModalGen('TRM40141B00000','DIALOG')
ENDIF

*-- Erase the generated temporary file, and close all the
*-- conditinaly opened files when closeing the screen.
IF glQuitting
  IF USED(lcTmpCredt)
    USE IN (lcTmpCredt)
  ENDIF
  ERASE (gcWorkDir+lcTmpCredt+".DBF")
  ERASE (gcWorkDir+lcTmpCredt+".CDX")
  ERASE (gcWorkDir+lcTmpCredt+".FPT")
  
 *E301077,52 YMA 03/03/99 Start - There is no need to close the files because 
 *E301077,52 YMA 03/03/99 the "gfOpenFile" will handel the file close.
 *USE IN IIF(llOpenGL  , "GL_LINK" , 0)
 *USE IN IIF(llOpApPayM, "ApPayMnt", 0)
 *USE IN IIF(llOpApBank, "ApBanks" , 0)
 *USE IN IIF(llOpApStup, "ApSetup" , 0) 
 *USE IN IIF(llOpApChec, "ApChecks", 0)
 *USE IN IIF(llOpSycCur, "SycCurr" , 0)
 *E301077,52 YMA 03/03/99 End.
ENDIF  



* ----------------------------------------------------------------

FUNCTION lfGtSavBar

USE (gcSysHome+"SYCMenu") IN 0 ORDER Pross_ID AGAIN ALIAS MenuFile
lnRetVal = IIF(SEEK("GFCPSAVE", "MenuFile"), VAL(MenuFile.cBar_Pos), 51)
USE IN MenuFile
RETURN (lnRetVal)

* ----------------------------------------------------------------

FUNCTION lpShow
PRIVATE lnAlias
lnAlias = SELECT()


DO CASE
  CASE laScrMode[1]                    && Select Mode
    *B603127,1 make this variable empty each time we enter the select mode[Start]
    lcBatSeqNo = SPACE(0)
    *B603127,1 [End..]
    IF llGoAndChk AND lfChkUnComS()
      RETURN
    ENDIF
    
    STORE 0 TO lnTapCount, lnActCount, lnTapTotal, lnActTotal, lnDiff
    STORE SPACE(1) TO lcBankCode, lcChkAcnt, lcCurCode, lcOldCur
    
    lcScrMode  = "S"
    ldBatDate  = gdSysDate
    llValidDat = .F.
    llNoThing  = lfBldType  (.T.)
    llNoThing  = lfSetCurIn ()
    llNoThing  = lfLoadInfo (.F.)
    llNoThing  = lfUpdTape  (0,0)
    llNoThing  = lfSwtType  ()

    lcObjStat  = "DISABLE"
    SHOW GETS WINDOW (lcChWin2) DISABLE ONLY
    SHOW GETS WINDOW (lcChWin4) DISABLE ONLY
    SHOW GETS WINDOW (lcChWin5) DISABLE ONLY
    SHOW GETS WINDOW (lcChWin6) DISABLE ONLY
    
    lcKeyStat  = "ENABLE"
    SHOW GET ldBatDate  &lcKeyStat
    SHOW GET lnTapCount &lcKeyStat
    SHOW GET lnTapTotal &lcKeyStat
    
    lcKeyOffS  = "DISABLE"
    SHOW GET pbKeyOff &lcKeyOffS
    
    ACTIVATE WINDOW (lcChWin2)
    _CUROBJ   = OBJNUM(ldBatDate)

  CASE laScrMode[2]                    && View Mode
    llGoAndChk = .T.
    
    *-- Disable all the windows defined.
    lcObjStat  = "DISABLE"
    lcKeyStat  = "DISABLE"
    SHOW GETS WINDOW (lcChWin2) DISABLE ONLY
    SHOW GETS WINDOW (lcChWin4) DISABLE ONLY
    SHOW GETS WINDOW (lcChWin5) DISABLE ONLY
    SHOW GETS WINDOW (lcChWin6) DISABLE ONLY
    
    *-- Control the "Control Panel" objects.
    STORE "DISABLE" TO  laCtrStat[01],laCtrStat[02],laCtrStat[03],;
                        laCtrStat[04],laCtrStat[06],laCtrStat[07],;
                        laCtrStat[08],laCtrStat[09],laCtrStat[10],;
                        laCtrStat[11]
    STORE "ENABLE"  TO  laCtrStat[05],laCtrStat[12]
    SHOW GET pbTop     &laCtrStat[01]
    SHOW GET pbBtm     &laCtrStat[02]
    SHOW GET pbNxt     &laCtrStat[03]
    SHOW GET pbPrvs    &laCtrStat[04]
    SHOW GET pbCpNew   &laCtrStat[05]
    SHOW GET pbcpPrint &laCtrStat[06]
    SHOW GET pbEdt     &laCtrStat[07]
    SHOW GET pbDlt     &laCtrStat[08]
    SHOW GET pbSlct    &laCtrStat[09]
    SHOW GET pbBrws    &laCtrStat[10]
    SHOW GET pbSav     &laCtrStat[11]
    SHOW GET pbCls     &laCtrStat[12]
    
    *-- Refresh the browse.
    SELECT (lcTmpCredt)
    *B603352,1 SSH 26/12/1999 fix the bug of goto top in browse file after keyed off.
    *GOTO TOP
    IF !EOF()
      SKIP
    ELSE
      GOTO BOTT
    ENDIF
    *B603352,1 SSH(End)
    llNoThing = lfwBrow()
    lcScrMode  = "V"
    
  CASE laScrMode[3]                    && Edit Mode

  CASE laScrMode[4]                    && Add Mode
    llGoAndChk = .T.
    
    lcObjStat  = ""
    SHOW GETS WINDOW (lcChWin2) ENABLE  ONLY
    IF llMltCur AND llEditExRt
      lcSt = IIF(lcCurCode = gcBaseCurr, "DISABLE", "ENABLE")
      SHOW GET lnExRate &lcSt
    ENDIF
  
    lcKeyStat  = "DISABLE"
    SHOW GET ldBatDate  &lcKeyStat
    SHOW GET lnTapCount &lcKeyStat
    SHOW GET lnTapTotal &lcKeyStat
    
    IF llApLink AND EMPTY(lcBankCode)
      SHOW GET lcChkAcnt  DISABLE
      SHOW GET pbBrChkAct DISABLE
    ENDIF  
    
    SELECT (lcTmpCredt)
    lnTmpRecNo = RECNO()
    GOTO TOP
    IF EOF()
      llNoThing  = lfBldType (.T.)
      llNoThing  = lfLoadInfo(.F.)
      llNoThing  = lfUpdTape(0, 0)
      llNoThing  = lfSwtType()
      SHOW WINDOW (lcPayBrTl) REFRESH
      SHOW GETS WINDOW (lcChWin4) DISABLE ONLY
      SHOW GET pbNew    ENABLE
      SHOW GET pbRemove DISABLE
    ELSE
      llNoThing  = lfBldType (.F.)
      IF lnTmpRecNo > 0 AND lnTmpRecNo <= RECCOUNT()
        GOTO lnTmpRecNo
      ELSE
        GOTO TOP
      ENDIF  
      llNoThing = lfwBrow()
      SHOW GET pbNew    ENABLE
      SHOW GET pbRemove ENABLE
    ENDIF

    lcKeyOffS  = "DISABLE"
    SHOW GET pbKeyOff &lcKeyOffS
    
    *-- Create an "Open" record in the uncomplete session file.
    lcScrMode  = "A"
    
    llNoThing  = IIF(lnUnCmSeRc=0, lfAdUnCmSR(), .T.) 
ENDCASE

lcTrpStat  = "ENABLE"
SHOW GET ibTrap1 &lcTrpStat
SHOW GET ibTrap2 &lcTrpStat

laCtrStat[10] = "DISABLE"
SHOW GET pbBrws &laCtrStat[10]

SELECT(lnAlias)

* ----------------------------------------------------------------

FUNCTION lfLoadInfo
PARAMETERS llFromFile
PRIVATE lnAlias

IF llFromFile
  lnAlias = SELECT()
  SELECT (lcTmpCredt)
  lnType     = ASUBSCRIPT(laType,ASCAN(laType,lNonAR),1)
  lcRef      = Reference
  lnAmount   = Amount
  lcAccount  = Account
  lcAccName  = IIF(SEEK("M"+Account,"Customer"),Customer.BTName,SPACE(1))
  lcCheck    = Store
  
  *B607231,1 KHM 06/15/2003 (Begin) Getting the factor code for the selected customer.
  lcFactor = Customer.cFacCode  
  *B607231,1 KHM 06/15/2003 (End)
  
  *B602788,1  Added IF Condition
  IF !EOF() AND !EMPTY(lcChkAcnt)
    lcGLAcct2  = cAdjAcct
  ENDIF  
  
  *B603056,1 Get payment type whether or not we have a GL link
  *IF llGLLink
  *B603056,1 end
    IF lNonAR
      llNoThing  = gfwCodePop(@laCodeInf2, "CARPTYPE", "V,"+cArPType)
    ELSE
      llNoThing  = gfwCodePop(@laCodeInf1, "CARPTYPE", "V,"+cArPType)
    ENDIF
  *B603056,1 Get payment type whether or not we have a GL link
  *ENDIF
  *B603056,1 end
  
  SELECT(lnAlias)
ELSE
  lnType     = 1
  lnAmount   = 0 
  lcGLAcct2  = IIF(laScrMode[1], SPACE(24), lcGLAcct2)


  *B603056,1 Get payment type whether or not we have a GL link
  *IF llGLLink
  *B603056,1 end
    llNoThing  = gfwCodePop(@laCodeInf2, "CARPTYPE", "N")
    llNoThing  = gfwCodePop(@laCodeInf1, "CARPTYPE", "N")
  *B603056,1 Get payment type whether or not we have a GL link
  *ENDIF
  *B603056,1 end
  STORE SPACE(1) TO lcRef, lcAccount, lcAccName,;
                    lcCheck

  *B607231,1 KHM 06/15/2003 (Begin) Re-Initializing lcFactor.
  lcFactor = " "
  *B607231,1 KHM 06/15/2003 (End)

ENDIF

SHOW GET lnType
SHOW GET lcAccount
SHOW GET lcRef
SHOW GET lcCheck
SHOW GET lnAmount
SHOW GET lcGLAcct2
llNoThing = lfRefresh()

* ----------------------------------------------------------------

FUNCTION lfUpdTape

PARAMETERS lnOldAmnt, lnNewAmnt
PRIVATE    lnOldAmnt, lnNewAmnt


lnOldAmnt  = ABS(lnOldAmnt)
lnNewAmnt  = ABS(lnNewAmnt)
lnActTotal = lnActTotal - lnOldAmnt + lnNewAmnt
lnDiff     = lnTapTotal - lnActTotal
llNoThing  = lfRefresh()
*B603639,1 (Start)  Commented out
*llNoThing  = lfUpdVars()
*B603639,1 (End)
* ----------------------------------------------------------------

FUNCTION lfSwtType
PRIVATE lcStatus

lcStatus = IIF(laScrMode[1] OR laScrMode[2],"DISABLE","ENABLE")
IF lnType = 1
  lnArType = lnType
  SHOW GETS WINDOW (lcChWin4) &lcStatus ONLY
  SHOW GETS WINDOW (lcChWin5) DISABLE   ONLY
  SHOW WINDOW (lcChWin4) TOP
  _CUROBJ = OBJNUM(lnArType)
ELSE
  lnNArType = lnType
  SHOW GETS WINDOW (lcChWin5) &lcStatus ONLY
  SHOW GETS WINDOW (lcChWin4) DISABLE   ONLY
  SHOW WINDOW (lcChWin5) TOP
  _CUROBJ = OBJNUM(lnNArType)
ENDIF
llNoThing = lfRefresh()

* ----------------------------------------------------------------

FUNCTION lfBldType
PARAMETERS llAddNA

IF llAddNA
  DIMENSION laType[1,2]
  laType[1,1]  = "N/A"
  laType[1,2]  = .F.
ELSE
  DIMENSION laType[2,2]
  laType[1,1]  = "AR Cash"
  laType[1,2]  = .F.
  laType[2,1]  = "Non AR Cash"
  laType[2,2]  = .T.
ENDIF

* ----------------------------------------------------------------

FUNCTION lfGetGLAcc
PARAMETERS lcLinkCode, lcCatgKey

lcLinkCode = IIF(EMPTY(lcLinkCode),"DEFDEF",lcLinkCode)
RETURN IIF(llGlLink,;
       IIF(SEEK(lcLinkCode+lcCatgKey,"GL_LINK"),GL_LINK.GLAcnt,SPACE(0)),;
       SPACE(0))

* ----------------------------------------------------------------

FUNCTION lfSetCurIn
PARAMETERS llShowGMsg, llShowPMsg

llShowGMsg = IIF(TYPE("llShowGMsg") = "U", .T., llShowGMsg)
llShowPMsg = IIF(TYPE("llShowPMsg") = "U", .F., llShowPMsg)
llRetVal   = .T.

IF !llMltCur OR EMPTY(lcCurCode)
  lcCurCode  = gcBaseCurr
  lnExRate   = 1
  lnCurrUnit = 1
  lcUntSin   = '/'
  lcExRSin   = '/'
ELSE
  lcUntSin   = SPACE(1)
  lnOldExR   = lnExRate
  lnExRate   = gfChkRate('lnCurrUnit', lcCurCode , ldBatDate , .T. ,;
                          gcAct_Comp , gcBaseCurr, !llShowGMsg)  
  *B603055,1 NAD 08/29/2000 (Start) Currency unit must not be equal to O
  lnCurrUnit =IIF(lnCurrUnit=0,1,lnCurrUnit)
  *B603055,1 NAD  (End)
  IF llShowGMsg
    lcCurCode = IIF(lnExRate = 0, gcBaseCurr, lcCurCode)
    lnExRate  = IIF(lnExRate = 0, 1, lnExRate)
  ELSE
    IF lnExRate = 0
      lnExRate  = lnOldExR
      lcCurCode = lcOldCur
      llRetVal  = .F.
      IF llShowPMsg
        *-- Currency "XXXXX" for checking account "XXXXXXXXXX"
        *-- has no valid exchange rate.
        *-- < ok >
        lcStr = lcCurCode + "|" + lcChkAcnt
        = gfModalGen("INM40063B00000","DIALOG",lcStr)
      ENDIF
    ENDIF
  ENDIF  
  
  lcExRSin  = gfGetExSin(@lcUntSin, lcCurCode)
ENDIF

SHOW GET lcCurCode
IF llEditExRt
  lcSt = IIF(lcCurCode = gcBaseCurr, "DISABLE", "ENABLE")
  SHOW GET lnExRate &lcSt
ELSE
  SHOW GET lnExRate
ENDIF

llNoThing = lfRefresh(lcChWin2)

RETURN (llRetVal)

* ----------------------------------------------------------------

FUNCTION lfSetGlAc2
PARAMETERS llShCrGMsg, llShCrPMsg
PRIVATE    llRetVal

IF !EMPTY(lcChkAcnt) AND SEEK(lcBankCode+lcChkAcnt,"ApChecks")
  lcGLAcct2 = ApChecks.cChkGLAcc
  lcOldCur  = lcCurCode
  lcCurCode = IIF(llMltCur, ApChecks.cCurrCode, SPACE(1))
ELSE
  lcGLAcct2 = SPACE(1)
  lcOldCur  = SPACE(1)
  lcCurCode = SPACE(1)
ENDIF
llRetVal = lfSetCurIn(llShCrGMsg, llShCrPMsg)

RETURN (llRetVal)

* ----------------------------------------------------------------

FUNCTION lfBrowse
PRIVATE lnAlias

lnAlias  = SELECT()
SELECT (lcTmpCredt)
lnRecNo  = RECNO()
lcFields = "cMarker=IIF(RECNO()=lnRecNo,'>',' '):1:R:H=' ':W=.F.," +;
           "cType=laType[ASCAN(laType,lNonAR)-1]:15:R:H='Type',"   +;
           "Reference:30:R,Amount:11:R,Account:R,Store:H='Check':R"+;
           IIF(llGlLink,",cARGlAcc:R:P=lcAcMask:H='AR/Non AR Account'","")
BROWSE FIELDS &lcFields WINDOW (lcBrWind) IN WINDOW (gcBaseWind) ;
       LOCK 0 SAVE NOWAIT NOEDIT NOCLEAR NOAPPEND NODELETE ;
       NOMENU TITLE (lcPayBrTl) WHEN lfwBrow() VALID lfvBrow()

SELECT(lnAlias)

* ----------------------------------------------------------------

FUNCTION lfvBrow

*-- Call the global function "gfStopBrow" going out of one of the 
*-- screen's browses.
IF WONTOP() # (lcPayBrTl)
  glFromBrow = .T.
  llNoThing  = gfStopBrow()
ENDIF

* ----------------------------------------------------------------

FUNCTION lfwBrow

lnRecNo    = RECNO()
llNoThing  = lfLoadInfo(.T.)
llNoThing  = lfSwtType()
IF laScrMode[2]
  lcKeyOffS = IIF(EMPTY(lcAccount), "DISABLE", "ENABLE")
  SHOW GET pbKeyOff &lcKeyOffS
ENDIF
SHOW WINDOW (lcPayBrTl) REFRESH

* ----------------------------------------------------------------

FUNCTION lfTrap

glFromBrow = WONTOP() = lcPayBrTl

*-- If any of the screen's browses is active then trap the 
*-- Tab, ShiftTab, Ctrl+Enter, Ctrl+Home and Ctrl+End keys.
IF glFromBrow
  ON KEY LABEL TAB        DO lfTab
  ON KEY LABEL BACKTAB    DO lfShiftTab
  ON KEY LABEL ESC        DO gfCPClose
  ON KEY LABEL Ctrl+ENTER lnDummy = 1
  ON KEY LABEL Ctrl+HOME  lnDummy = 1
  ON KEY LABEL Ctrl+W     lnDummy = 1
  ON KEY LABEL Ctrl+END   lnDummy = 1
ENDIF  

* ----------------------------------------------------------------

FUNCTION lfUnTrap

*-- If going out of one of the browses, call the global function
*-- to stop the browse and reset the global flag.
IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF

IF WONTOP() # lcPayBrTl
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL Ctrl+ENTER
  ON KEY LABEL Ctrl+HOME
  ON KEY LABEL Ctrl+END
  ON KEY LABEL Ctrl+W
  ON KEY LABEL ESC &lcEscTrap
ENDIF  

* ----------------------------------------------------------------

FUNCTION lfTab

DO CASE
  CASE laScrMode[1]                       && Select Mode
    lcGoTo = "gwcContrl1"
  CASE laScrMode[2]                       && View Mode
    lcGoTo = lcChWin6
  CASE laScrMode[4]                       && Add Mode
    lcGoTo = IIF(lNonAr, lcChWin5, lcChWin4) 
ENDCASE
ACTIVATE WINDOW (lcGoTo)

* ----------------------------------------------------------------

FUNCTION lfShiftTab

IF laScrMode[2]
  ACTIVATE WINDOW (lcChWin6)
ELSE
  ACTIVATE WINDOW (lcChWin2)
  IF laScrMode[1]
    _CUROBJ = OBJNUM(lnTapTotal)
  ELSE
    DO CASE
      CASE llApLink AND !llEditExRt
        llEmp   = EMPTY(lcBankCode)
        _CUROBJ = IIF(llEmp,OBJNUM(lcBankCode),OBJNUM(lcChkAcnt))
      CASE llApLink AND llEditExRt
        _CUROBJ = OBJNUM(lnExRate)
      CASE !llApLink AND llGLLink
        _CUROBJ = OBJNUM(lcGLAcct2)
      CASE !llApLink AND !llGLLink AND llEditExRt
        _CUROBJ = OBJNUM(lnExRate)
      CASE !llApLink AND !llGLLink AND !llEditExRt
        _CUROBJ = IIF(llMltCur,OBJNUM(lcCurCode),OBJNUM(lcChkAcnt))
    ENDCASE
  ENDIF  
ENDIF

* ----------------------------------------------------------------

FUNCTION lfvDate
PRIVATE llRetVal

IF INLIST(LASTKEY(), 13, 9, 15)
  llValidDat = CheckPrd(ldBatDate,"lcGLFYear","lcGLPeriod",'CR')
  IF llValidDat
    = lfvKey()
    = gfUpdate()
  ELSE
    ldBatDate = ldOBatDate
    _CUROBJ   = OBJNUM(ldBatDate)
  ENDIF  
ENDIF

* ----------------------------------------------------------------

FUNCTION lfvKey
IF !llApLink  
  *B603187,1 Allow tape total < 1
  *IF llValidDat AND lnTapCount >= 1 AND lnTapTotal >= 1
  IF llValidDat AND lnTapCount >= 1 AND lnTapTotal > 0
  *B603187,1 end
    laScrMode    = .F.
    laScrMode[4] = .T.                     && Select, View, Edit, Add
    SHOW GETS
    _CUROBJ = OBJNUM(lcBankCode)
  ENDIF
ELSE
  DO CASE
    *B603187,1 Allow tape total < 1
    *CASE llValidDat AND lnTapCount >= 1 AND lnTapTotal >= 1 AND !EMPTY(lcChkAcnt)
    CASE llValidDat AND lnTapCount >= 1 AND lnTapTotal > 0 AND !EMPTY(lcChkAcnt)
    *B603187,1 end
      laScrMode    = .F.
      laScrMode[4] = .T.                     && Select, View, Edit, Add
      SHOW GETS

    *B603187,1 Allow tape total < 1
    *CASE llValidDat AND lnTapCount >= 1 AND lnTapTotal >= 1
    CASE llValidDat AND lnTapCount >= 1 AND lnTapTotal > 0
    *B603187,1 end
      SHOW GET lcBankCode ENABLE
      SHOW GET pbBrBank   ENABLE
      *SHOW GET lcChkAcnt  ENABLE 
      _CUROBJ = OBJNUM(lcBankCode)
  ENDCASE

ENDIF
* ----------------------------------------------------------------

FUNCTION lfvBank
PRIVATE lnAlias, lcBrowTitl, lcBrFields, lcFile_Ttl, lcBrFields, laTemp

IF !(lcOBankCod == lcBankCode) OR llFrmBrow
  
  *E301077,52 YMA 03/03/99 Start - Open the ApBanks, ApChecks file.
  = gfOpenFile (gcDataDir+"ApBanks" ,"BankCode" ,"SH")
  = gfOpenFile (gcDataDir+"ApChecks","BankCheck","SH")
  *E301077,52 YMA 03/03/99 End.
    
  IF (!EMPTY(lcBankCode) AND !SEEK(lcBankCode, "ApBanks")) OR llFrmBrow
    DIMENSION laTemp[1]
    laTemp     = SPACE(0)
    llFrmBrow  = .F.
    lnAlias    = SELECT(0)
    lcFile_Ttl = 'Banks'
    lcBrFields = "cBnkCode  :R :H='Bank Code':8,"+;
                 "cBnkShDes :R :H='Bank Des.':15"
    SELECT ApBanks
    IF gfBrows(.F.,"cBnkCode",'laTemp')
      lcBankCode = laTemp[1]
    ELSE
      lcBankCode = lcOBankCod
      _CUROBJ    = _CUROBJ
    ENDIF
    SELECT(lnAlias)
  ENDIF
  
  IF EMPTY(lcBankCode)
    *B602788,1 MAN Start
    *lcStat     = "DISABLE"
    *lcChkAcnt  = SPACE(1)
	IF llApLink  AND laScrMode[4] AND !EMPTY(lcOBankCod)
	  lcBankCode = lcOBankCod
	  lcStat     = "ENABLE"
    ELSE
      lcStat     = "DISABLE"
      lcChkAcnt  = SPACE(1)
    ENDIF  
    llDefChk = .F.
    *B602788,1 MAN End
  ELSE
    lcStat    = "ENABLE"
    lcChkAcnt = IIF(SEEK(lcBankCode,'APChecks'),ApChecks.cChkAcct,SPACE(1))
    *B602788,1 MAN We just defaulted the check code
    llDefChk = .T.
  ENDIF

  llValidRat  = lfSetGlAc2(.F., .F.)
  IF !EMPTY(lcBankCode) AND !EMPTY(lcChkAcnt) AND !llValidRat
    STORE SPACE(1) TO lcChkAcnt, lcGLAcct2, lcCurCode, lcOldCur
    llRetVal = lfSetCurIn()
  ENDIF

  *E300921,1 (Start) Disable the Checking account if the selected bank type was Safe. 
  IF APBANKS.cBnkType = 'S'
    lcStat = "DISABLE"
  ENDIF    
  *E300921,1 (End).
  
  *B802696,1 BWA 14/10/1999 Fix the bug which is if the bank has many check 
  *SHOW GET lcBankCode
  *SHOW GET lcChkAcnt  &lcStat
  *SHOW GET pbBrChkAct &lcStat
  *llNoThing = lfUpdVars()
  
  *B602954 (Start) 
  IF APBANKS.cBnkType = 'S'
    =lfvKey()
  ENDIF
  SHOW GET lcBankCode
  SHOW GET lcChkAcnt  &lcStat
  SHOW GET pbBrChkAct &lcStat
  *B603639,1 (Start)  Update in Add mode only
  *llNoThing = lfUpdVars()
  llNoThing = IIF(laScrMode[4],lfUpdVars(),.T.)
  *B603639,1 (End)
 
  *B802696,1 BWA 14/10/1999 [EMD]
  *B602954 (End)
ENDIF

* ----------------------------------------------------------------

FUNCTION lfvChecks

*B802696,1 BWA 14/10/1999 Fix the bug which is if the bank has many check 
IF MDOWN()
  RETURN
ENDIF
*B802696,1 BWA 14/10/1999 [END]

lnAlias    = SELECT(0)
*B602788,1 MAN Added OR llDefChk to go in the validation process if the 
*B602788,1 MAN Check code is defaulted
IF !(lcChkAcnt == lcOChkAcnt) OR llFrmBrow OR llDefChk 
  *B602788,1 MAN 
  llDefChk =.F.
  *E301077,52 YMA 03/03/99 Start - Open the ApChecks file.
  = gfOpenFile (gcDataDir+"ApChecks","BankCheck","SH")
  *E301077,52 YMA 03/03/99 End.
  
  IF (!EMPTY(lcChkAcnt) AND !SEEK(lcBankCode+lcChkAcnt,"ApChecks")) OR llFrmBrow
  
    DIMENSION laTemp[1]
    laTemp     = SPACE(0)
    llFrmBrow  = .F.
    lcBrowTitl = "Checks"
    lcBrFields = "cBnkCode :R :H='Bank Code':8,"+;
                 "cChkAcct :R :H='Checking Account':12"

    SELECT ApChecks
    IF gfBrows("lcBankCode","cChkAcct",'laTemp')
      lcChkAcnt = laTemp[1]
    ELSE
      lcChkAcnt = lcOChkAcnt
      _CUROBJ   = _CUROBJ
    ENDIF
  ENDIF
  
  *B802696,1 BWA 14/10/1999 Fix the bug which is if the bank has many check 
  *B802696,1 BWA            account the user can not select onther one[start]
  *IF llApLink  AND laScrMode[4] AND !EMPTY(lcOChkAcnt)
  IF EMPTY(lcChkAcnt)
  *B802696,1 BWA 14/10/1999 [END]
     lcChkAcnt = lcOChkAcnt
  ENDIF   
  IF lfSetGlAc2(.F., .T.)
    SELECT (lcTmpCredt)
    REPLACE ALL cAdjAcct WITH lcGLAcct2
    GOTO TOP
    llNoThing = lfwBrow()
  ELSE
    lcChkAcnt = lcOChkAcnt
    _CUROBJ   = _CUROBJ
  ENDIF
  *B603639,1 (Start)  Update in Add mode only
  *llNoThing = lfUpdVars()
  llNoThing = IIF(laScrMode[4],lfUpdVars(),.T.)
  *B603639,1 (End)

  *B602788,1 MAN Call the Key Validation function to validate the key
  =lfvKey()
ENDIF
  
SELECT (lnAlias)

* ----------------------------------------------------------------

FUNCTION lfvCurCode
PRIVATE lcTmpCode

IF !(lcCurCode == lcOCurCode) OR llFrmBrow

  *B603550,1 BWA 04/05/2000 Fix the bug of Alias "SYCCURR" not found.[START]
  = gfOpenFile(gcSysHome+"SycCurr","cCurrCode",'SH')
  *B603550,1 [END]
  
  IF !EMPTY(lcCurCode) OR llFrmBrow
    lcTmpCode = IIF(llFrmBrow, LEFT(lcCurCode,2), lcCurCode) 
    lcCurCode = IIF(gfCurrBrow(@lcTmpCode), lcTmpCode, lcOCurCode)
    IF EMPTY(lcCurCode) OR EMPTY(lcTmpCode)
      _CUROBJ = OBJNUM(lcCurCode)
    ENDIF
    llFrmBrow = .F.
  ENDIF  
  lcOldCur  = lcOCurCode
  llNoThing = lfSetCurIn(.T., .F.)
  *B603639,1 (Start)  Update in Add mode only
  *llNoThing = lfUpdVars()
  llNoThing = IIF(laScrMode[4],lfUpdVars(),.T.)
  *B603639,1 (End)
ENDIF

* ----------------------------------------------------------------

FUNCTION lfvRate

IF lnExRate <= 0
  *-- Currency exchange rate should not be less than one.
  *-- <  ok >
  lnNoThing = gfModalGen("INM40061B00000")
  lnExRate  = lnOExRate
  _CUROBJ   = OBJNUM(lnExRate)
ENDIF

* ----------------------------------------------------------------

FUNCTION lfvCashRcA
PRIVATE lnAlias, lnTmpRecNo

IF !(lcOGLAcct2 == lcGLAcct2) OR llFrmBrow
  llNoThing = lfvGLAccnt()
  llFrmBrow = .F.
  lcGLAcct2 = IIF(EMPTY(lcGLAcct2), lcOGLAcct2, lcGLAcct2)
  *B603639,1 (Start)  Update in Add mode only
  *llNoThing = lfUpdVars()
  llNoThing = IIF(laScrMode[4],lfUpdVars(),.T.)
  *B603639,1 (End)
ENDIF

* ----------------------------------------------------------------

FUNCTION lfvType
PRIVATE lnAlias

lnAlias = SELECT(0)
IF !(lnOType == lnType)
  SELECT (lcTmpCredt)
  llNoThing = RLOCK()
  REPLACE lNonAR WITH !(lnType=1)
  IF lnType = 2
    STORE SPACE(1) TO lcAccount, lcOAccount, lcAccName 
    REPLACE Account WITH lcAccount
  ENDIF
  UNLOCK

  *B603056,1 Get payment type whether or not we have a GL link
  *IF llGLLink
  *B603056,1 end
    IF lNonAR
      llNoThing = gfwCodePop(@laCodeInf2, "CARPTYPE", "D")
    ELSE
      llNoThing = gfwCodePop(@laCodeInf1, "CARPTYPE", "D")
    ENDIF
    llNoThing = lfvArPayT(.T.)
  *B603056,1 Get payment type whether or not we have a GL link
  *ENDIF
  *B603056,1 end
  
  llNoThing = lfSwtType()
  SHOW WINDOW (lcPayBrTl) REFRESH
ENDIF  
SELECT(lnAlias)

* ----------------------------------------------------------------

FUNCTION lfvArType

lnType    = lnArType
llNoThing = lfvType()

* ----------------------------------------------------------------

FUNCTION lfvNArType

lnType    = lnNArType
llNoThing = lfvType()

* ----------------------------------------------------------------

FUNCTION lfvAccount
PRIVATE lnAlias
lnAlias = SELECT(0)
IF !(lcAccount == lcOAccount) OR llFrmBrow
  IF (!EMPTY(lcAccount) AND !SEEK("M"+lcAccount, "Customer")) OR llFrmBrow
    SELECT Customer
    = CUSBROWM(@lcAccount,"","")
    SELECT(lnAlias)
    IF EMPTY(lcAccount)
      lcAccount = lcOAccount
      _CUROBJ   = OBJNUM(lcAccount)
    ENDIF
  ENDIF
  IF !(lcAccount == lcOAccount)  
      *B602838,1 NAD 09/24/2000 (Start) Ask before making transaction for non active account.
      *lnChoice = IIF(Customer.Status = 'X', gfModalgen("INM40059B40008"), 2)
      lnChoice = IIF(Customer.Status <> 'A', gfModalgen("INM40059B40008"), 2)
      *B602838,1 NAD 09/24/2000 (End)
      IF lnChoice = 1
        lcAccount = lcOAccount
        _CUROBJ   = OBJNUM(lcAccount)
      ELSE
        lcAccName = IIF(EMPTY(lcAccount), SPACE(1), Customer.BTName)
        llNoThing = lfRefresh()
        SELECT (lcTmpCredt)
        llNoThing = RLOCK()
        REPLACE Account  WITH lcAccount
        
        *B607231,1 KHM 06/15/2003 (Begin) Replacing the factor code.
        REPLACE cFacCode WITH Customer.cFacCode
        *B607231,1 KHM 06/15/2003 (End)        
        UNLOCK
        
        *B607231,1 KHM 06/15/2003 (Begin) Getting the factor from the selected customer.
        lcFactor = Customer.cFacCode
        SHOW GET lcFactor
        *B607231,1 KHM 06/15/2003 (End)
        
        SHOW WINDOW (lcPayBrTl) REFRESH
        SHOW GET lcAccount    
      ENDIF
    ENDIF
  llFrmBrow = .F.
ENDIF
SELECT(lnAlias)

* ----------------------------------------------------------------

FUNCTION lfvRef
PRIVATE lnAlias

IF !(lcRef == lcORef)
  lnAlias = SELECT(0)
  SELECT (lcTmpCredt)
  llNoThing = RLOCK()
  REPLACE Reference WITH lcRef
  UNLOCK
  SHOW WINDOW (lcPayBrTl) REFRESH
  SELECT(lnAlias)
ENDIF  

* ----------------------------------------------------------------

FUNCTION lfvCheck
PRIVATE lnAlias

IF !(lcCheck == lcOCheck)
  lnAlias = SELECT(0)
  SELECT (lcTmpCredt)
  llNoThing = RLOCK()
  REPLACE Store WITH lcCheck
  UNLOCK
  SHOW WINDOW (lcPayBrTl) REFRESH
  SELECT(lnAlias)
ENDIF

* ----------------------------------------------------------------

FUNCTION lfvAmount
PRIVATE lnAlias
*E301420,1 (Start) To prevenet bug numeric over flow 
IF lnAmount > 9999999999.00
  * Message XXXXX cannot exceed 9999999999
  =gfModalGen("TRM40171BOOOO" , "DIALOG", 'Amount' +'|'+'9999999999') 
  lnAmount =  lnOAmount  
  RETURN
ENDIF 
*E301420,1 (End)

IF !(lnAmount == lnOAmount)
  lnAmount  = -1 * (ABS(lnAmount))
  llNoThing = lfUpdTape(&lcTmpCredt..Amount, lnAmount)
  *B603639,1  (Start) Update the uncomplete session record
  llNoThing  = lfUpdVars()
  *B603639,1  (End)
  lnAlias   = SELECT(0)
  SELECT (lcTmpCredt)
  llNoThing = RLOCK()
  REPLACE Amount WITH lnAmount
  UNLOCK
  SHOW WINDOW (lcPayBrTl) REFRESH
  SELECT(lnAlias)
ENDIF  

* ----------------------------------------------------------------

FUNCTION lfvNew
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT (lcTmpCredt)
APPEND BLANK
llNoThing = RLOCK()
*-- AAMER Update the postdate
REPLACE lNonAR    WITH .F.        ,;
        cAdjAcct  WITH lcGLAcct2  ,;
        cCurrCode WITH lcCurCode  ,;
        nCurrUnit WITH lnCurrUnit ,;
        nExRate   WITH lnExRate   ,;
        dPostDate WITH ldBatDate     
UNLOCK
lnActCount = lnActCount + 1
*B603056,1 Get payment type whether or not we have a GL link
*IF llGLLink
*B603056,1 end
  llNoThing  = gfwCodePop(@laCodeInf1, "CARPTYPE", "D") AND lfvArPayT(.T.)
*B603056,1 Get payment type whether or not we have a GL link
*ENDIF
*B603056,1 end
llNoThing  = lfUpdVars()
llNoThing  = lpShow()
_CUROBJ    = OBJNUM(lnArType)
SELECT(lnAlias)

* ----------------------------------------------------------------

FUNCTION lfvRem
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT (lcTmpCredt)
llNoThing = lfUpdTape(Amount, 0)
llNoThing = RLOCK()
BLANK
DELETE
UNLOCK
SKIP
IF EOF()
  SKIP -1
ENDIF
lnActCount = lnActCount - 1
llNoThing  = lfUpdVars()
llNoThing  = lpShow()
_CUROBJ    = OBJNUM(pbNew)
SELECT(lnAlias)

* ----------------------------------------------------------------

FUNCTION gfvCpNew

laScrMode    = .F.
laScrMode[1] = .T.
SHOW GETS

* ----------------------------------------------------------------

FUNCTION lpClsScr
PRIVATE lnAlias

llNoThing = lfUpdUnCmS("Initia", SPACE(0), .F.)

* ----------------------------------------------------------------

FUNCTION gfCPSave
PRIVATE lnCurAls

*E301521,1 (AHM) Extract the saving function in individulal program (Start)
DO lfSavCashR IN (gcapphome+'ARCSHRSV.PRG')
RETURN
*E301521,1 (End)

*E301521,1 (AHM) all the coming code will not be excuted as a result of the above return statment

SELECT (lcTmpCredt)
LOCATE FOR AMOUNT = 0 AND !DELETED()
IF FOUND()
  *-- One or more of the transaction(s) has zero amount, Transaction(s)
  *-- with zero amount will be ignored.
  *-- < Modify > < Continue >
  IF gfModalGen("QRM40071B40009") = 1
    = lfwBrow()
    _CUROBJ = OBJNUM(lnAmount)
    RETURN
  ENDIF
ENDIF

*-- Validate if there are no transactions...
SELECT (lcTmpCredt)
COUNT ALL FOR Amount <> 0 TO lnTotTrans
IF lnTotTrans = 0
  *-- No transactions entered, cannot proceed.
  *-- < ok >
  = gfModalGen("TRM40070B00000")
  RETURN
ENDIF

*-- Checking the actual number of payments with the tape count.
IF lnTapCount <> lnActCount
  *-- The actual number of payments (999) is not equal to the 
  *-- tape count (999).
  *-- < Modify > < Continue >
  lcStr1 = "The actual number of payments ("+ALLTRIM(STR(lnActCount))+")"
  lcStr2 = "the tape count ("+ ALLTRIM(STR(lnTapCount))+")"
  IF gfModalGen("QRM40060B40009","ALERT",lcStr1+"|"+lcStr2) = 1
    GOTO TOP IN (lcTmpCredt)
    KEYBOARD "{CTRL+B}" PLAIN
    RETURN
  ENDIF  
ENDIF

*-- Checking the actual amount with the tape total.
IF lnTapTotal <> lnActTotal
  *-- The actual amount (999.99) is not equal to the tape total (999.99).
  *-- < Modify > < Continue >
  lcStr1 = "The actual amount (" + ALLTRIM(STR(lnActTotal,10,2)) + ")"
  lcStr2 = "the tape total ("    + ALLTRIM(STR(lnTapTotal,10,2)) + ")"
  IF gfModalGen("QRM40060B40009","ALERT",lcStr1+"|"+lcStr2) = 1
    GOTO TOP IN (lcTmpCredt)
    KEYBOARD "{CTRL+B}" PLAIN
    RETURN
  ENDIF  
ENDIF

IF llApLink 
  IF EMPTY(lcBankCode)
    *-- You have to enter the Bank Code.
    *-- < ok >
    = gfModalGen("TRM00250B00000", "ALERT", "Bank Code")
    _CUROBJ = OBJNUM(lcBankCode)
    RETURN
  ENDIF

  IF EMPTY(lcChkAcnt) 
    *-- You have to enter the Checking Account.
    *-- < ok >
    = gfModalGen("TRM00250B00000", "ALERT", "Checking Account")
    _CUROBJ = OBJNUM(lcChkAcnt)
    RETURN
  ENDIF
ENDIF

IF llMltCur 
  *-- Be sure that the currencey code field is not empty if the system
  *-- is setup to use multiable currencies.
  IF EMPTY(lcCurCode)
    *-- You have to enter the Currency Code.
    *-- < ok >
    = gfModalGen("TRM00250B00000", "ALERT", "Currency Code")
    _CUROBJ = IIF(llApLink, OBJNUM(lcChkAcnt), OBJNUM(lcCurCode))
    RETURN
  ENDIF  

  *-- The rate will never be less that one because it is validated
  *-- either from the valid rate function which is called from the 
  *-- object if the rate is editable, or it is validated from the 
  *-- currency code object or the bank code or checking account, but 
  *-- there is only one case that might let the rate equal to zero, 
  *-- this case is to enter the rate as zero and do not press enter
  *-- on the field but press escape, at that time the global escape 
  *-- trapping will be excuted without excuting the object valid
  *-- function.
  IF lnExRate <= 0
    *-- Currency exchange rate should not be less than one.
    *-- <  ok >
    = gfModalGen("INM40061B00000")
    IF llEditExRt
      _CUROBJ = OBJNUM(lnExRate)
    ENDIF  
    RETURN
  ENDIF
ENDIF

IF llGlLink AND !llApLink AND EMPTY(lcGLAcct2)
  *-- You have to enter the Cash Receipt Account.
  *-- < ok >
  = gfModalGen("TRM00250B00000", "ALERT", "Cash Receipt Account")
  _CUROBJ = OBJNUM(lcGLAcct2)
  RETURN
ENDIF

IF llGlLink
  SELECT (lcTmpCredt)
  LOCATE FOR EMPTY(STRTRAN(cARGLAcc,"-",SPACE(1)))
  IF FOUND()
    = lfwBrow()

    *-- You have to enter the AR Account.
    *-- < ok >
    lcStr = "correct " + IIF(lNonAr, "Non ", "") + "AR payment type"
    = gfModalGen("TRM00250B00000", "ALERT", lcStr)
    _CUROBJ = IIF(lNonAr, OBJNUM(lnNArPayT), OBJNUM(lnArPayT))
    RETURN
  ENDIF
ENDIF

SELECT (lcTmpCredt)
LOCATE FOR !lNonAr AND EMPTY(Account)
IF FOUND()
  = lfwBrow()
  *-- You have to enter the Account Code.
  *-- < ok >
  = gfModalGen("TRM00250B00000", "ALERT", "Account Code")
  _CUROBJ = OBJNUM(lcAccount)
  RETURN
ENDIF

*-- Start updating the master files.
lnCurAls   = SELECT(0)
llOpSeqn   = gfOpenFile(gcDataDir+"Sequence", "cSeq_Type","SH")
llOpnHis   = gfOpenFile(gcDataDir+"ARCUSHST", "Acthst"   ,"SH")

*-- Update the uncomplete session file with the object name.
llNoThing  = lfUpdUnCmS("Open", "pbSav", .T.)

lcBatSeqNo = IIF(EMPTY(lcBatSeqNo), gfSequence("BATCH", gcAct_Comp), lcBatSeqNo)
llNoThing  = lfUpdVars()

lnCurRec = 0
lcMsg1   = "Updating master credit file"

SELECT (lcTmpCredt)
SCAN FOR Amount <> 0
  lnCurRec   = lnCurRec + 1
  lcTrnSeqNo = gfSequence("TRAN", gcAct_Comp)
  lcMsg2     = "Transaction Number : " + lcTrnSeqNo
  lcPayComp  = &lcTmpCredt..Reference
  llNoThing  = IIF(&lcTmpCredt..lNonAr, .T., lfUpdCust())
  llNoThing  = IIF(&lcTmpCredt..lNonAr, .T., lfUpdCusHs())
  llNoThing  = IIF(llApLink, lfUpdApPay(), .T.)
  llNoThing  = lfUpdTmp()
  llNoThing  = gfThermo(lnTotTrans, lnCurRec, lcMsg1 ,lcMsg2)
ENDSCAN

*B603091,1 Start, Added.
IF gcAct_Appl='PS' AND ;
  ( EMPTY(gfGetMemVar("M_PRTRCET")) OR gfGetMemVar("M_PRTRCET") $ 'AI' ) 
  *--Do you wish to print the case receipt? Y/N
  =lfPrtRectpt()
ENDIF
*B603091,1 End.

llNoThing = IIF(llGlLink, lfUpdGL(), .T.)

SELECT Credit
APPEND FROM (gcWorkDir+lcTmpCredt) FOR Amount <> 0

*-- Update the uncomplete session file.
llNoThing = lfUpdUnCmS("Complete", SPACE(0), .F.)
laScrMode = "V"
*B603639,1 (Start) to Add new record in the UnCMSESS file
lnUnCmSeRc=0
*B603639,1 (End) to Add new record in the UnCMSESS file

*-- Generated batch number is : XXXXXXXX
*-- < ok >
lnNoThing = gfModalGen("INM40064B00000", "DIALOG", lcBatSeqNo)

USE IN IIF(llOpSeqn, "Sequence" , 0) 
USE IN IIF(llOpnHis, "ARCUSHST" , 0)

SELECT(lnCurAls)

laScrMode    = .F.
laScrMode[2] = .T.
SHOW GETS

* ----------------------------------------------------------------

FUNCTION lfUpdCust

IF &lcTmpCredt..nSteps < 1
  IF SEEK("M" + &lcTmpCredt..Account, "Customer")
    lcPayComp = Customer.BTName
    lnEqvAmnt = ROUND(&lcTmpCredt..Amount &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)
    SELECT Customer
    llNoThing = RLOCK()
    REPLACE OpenCr WITH OpenCr + lnEqvAmnt ,;
            NetBal WITH NetBal + lnEqvAmnt
    UNLOCK
    
    *E301245 function to update nHgWtrMark Field with NETBAL field [Begin.]
    =lfHgWUpdat() 
    *E301245 function to update nHgWtrMark Field with NETBAL field [End.]

  ENDIF  
  = lfUpdStp(lcTmpCredt, 1)
ENDIF  

* ----------------------------------------------------------------

FUNCTION lfUpdCusHs

IF &lcTmpCredt..nSteps < 2
  IF BETWEEN(VAL(lcGLPeriod), 1, 13)
    lcPeriod  = PADL(ALLTRIM(lcGLPeriod), 2, "0" )
    lnEqvAmnt = ROUND(&lcTmpCredt..Amount &lcExRSin lnExRate &lcUntSin lnCurrUnit, 2)
    lnEqvAmnt = ABS(lnEqvAmnt)
    IF SEEK(&lcTmpCredt..Account+lcGLFYear, "ARCusHst")
      SELECT ARCusHst
      llNoThing = RLOCK()
      REPLACE nPayment&lcPeriod WITH nPayment&lcPeriod + lnEqvAmnt ,;
              nPayment          WITH nPayment          + lnEqvAmnt
      UNLOCK
    ENDIF
  ENDIF
  = lfUpdStp(lcTmpCredt, 2)
ENDIF  

* ----------------------------------------------------------------

FUNCTION lfUpdApPay

IF &lcTmpCredt..nSteps < 3
  
  *E301077,52 YMA 03/03/99 Start - Open the ApPaymnt file.
  = gfOpenFile (gcDataDir+"ApPayMnt","","SH")
  *E301077,52 YMA 03/03/99 End.
  
  SELECT ApPaymnt
  APPEND BLANK
  llNoThing = RLOCK()
  llNoThing = gfAdd_Info()

  *E301266,1 AKA (Start)
  * Assign generated batch no.  to ApPaymnt.Batch
  *REPLACE cPayType  WITH "A"                              ,;
          cPayMeth  WITH IIF(&lcTmpCredt..lNonAr,'N','A') ,;
          cBnkCode  WITH lcBankCode                       ,;
          cChkAcct  WITH lcChkAcnt                        ,;
          dPayDate  WITH ldBatDate                        ,;
          cFisFYear WITH lcGLFYear                        ,;
          cFspprdid WITH lcGLPeriod                       ,;
          cPayDocNo WITH &lcTmpCredt..Store               ,;
          cPayClNo  WITH &lcTmpCredt..Account             ,;
          cPayComp  WITH lcPayComp                        ,;
          nPayAmnt  WITH &lcTmpCredt..Amount              ,;
          cCurrCode WITH lcCurCode                        ,;
          nExRate   WITH lnExRate                         ,;
          nCurrUnit WITH lnCurrUnit                       ,;
          cPayRecSt WITH "O"
  
          
  REPLACE cPayType  WITH "A"                              ,;
          cPayMeth  WITH IIF(&lcTmpCredt..lNonAr,'N','A') ,;
          cBnkCode  WITH lcBankCode                       ,;
          cChkAcct  WITH lcChkAcnt                        ,;
          dPayDate  WITH ldBatDate                        ,;
          cFisFYear WITH lcGLFYear                        ,;
          cFspprdid WITH lcGLPeriod                       ,;
          cPayDocNo WITH &lcTmpCredt..Store               ,;
          cPayClNo  WITH &lcTmpCredt..Account             ,;
          cPayComp  WITH lcPayComp                        ,;
          nPayAmnt  WITH &lcTmpCredt..Amount              ,;
          cCurrCode WITH lcCurCode                        ,;
          nExRate   WITH lnExRate                         ,;
          nCurrUnit WITH lnCurrUnit                       ,;
          cPayRecSt WITH "O"                              ,;
          Batch     WITH lcBatSeqNo  
          
  *E301266,1 AKA (End)
  
  UNLOCK
  = lfUpdStp(lcTmpCredt, 3)
ENDIF        
* ----------------------------------------------------------------

FUNCTION lfUpdTmp

IF &lcTmpCredt..nSteps < 4
  SELECT (lcTmpCredt)
  llNoThing = RLOCK()
  llNoThing = gfAdd_Info()
  
  *B603056,1 Replace Credit description with the description of the
  *B603056,1 payment type
  *REPLACE cAdjAcct   WITH lcGLAcct2                       ,;
          cBnkCode   WITH lcBankCode                      ,;
          cChkAcct   WITH lcChkAcnt                       ,;
          TranDate   WITH ldBatDate                       ,;
          TranType   WITH "4"                             ,;
          Tran       WITH lcTrnSeqNo                      ,;
          Desc       WITH "PAYMENT-THANK YOU"             ,;
          Batch      WITH lcBatSeqNo                      ,;
          cCurrCode  WITH lcCurCode                       ,;
          nCurrUnit  WITH lnCurrUnit                      ,;
          nExRate    WITH lnExRate
  REPLACE cAdjAcct   WITH lcGLAcct2                       ,;
          cBnkCode   WITH lcBankCode                      ,;
          cChkAcct   WITH lcChkAcnt                       ,;
          TranDate   WITH ldBatDate                       ,;
          TranType   WITH "4"                             ,;
          Tran       WITH lcTrnSeqNo                      ,;
          Desc       WITH gfCodDes(cArPType,'CARPTYPE')   ,;
          Batch      WITH lcBatSeqNo                      ,;
          cCurrCode  WITH lcCurCode                       ,;
          nCurrUnit  WITH lnCurrUnit                      ,;
          nExRate    WITH lnExRate
  *B603056,1 end
  UNLOCK
  = lfUpdStp(lcTmpCredt, 4)
ENDIF

* ----------------------------------------------------------------

FUNCTION lfUpdGL

GOTO BOTTOM IN (lcTmpCredt)
IF &lcTmpCredt..nSteps < 5
  lcTmpGLD   = gfTempName()
  TmpCrdtGL  = gfTempName()
  llOpGlDist = gfOpenFile(gcDataDir+"GLDIST" ,"GLDISTAC","SH")  

  SELECT GLDIST
  COPY STRUCTURE TO (gcWorkDir+lcTmpGLD)
  = gfOpenFile(gcWorkDir+lcTmpGLD ,"","EX")

*--MAN Start
*  SELECT (lcTmpCredt)
*  SELECT *, SUM(Amount) AS nAmt ;
    FROM (lcTmpCredt) ;
    GROUP BY cARGlAcc, cAdjAcct, Link_Code ; 
  INTO DBF (gcWorkDir+TmpCrdtGL)
  
*  SELECT (TmpCrdtGL)
*  SCAN 
*    DO GLDIST WITH &TmpCrdtGL..Link_Code,'002',ABS(&TmpCrdtGL..nAmt)      ,;
                   'CR',lcBatSeqNo,ldBatDate,lcGLFYear,lcGLPeriod,'&lcTmpGLD',;
                   &TmpCrdtGL..cAdjAcct,lcCurCode,lnCurrUnit,lnExRate
*    DO GLDIST WITH &TmpCrdtGL..Link_Code,'001',-ABS(&TmpCrdtGL..nAmt)     ,;
                   'CR',lcBatSeqNo,ldBatDate,lcGLFYear,lcGLPeriod,'&lcTmpGLD',;
                   &TmpCrdtGL..cARGlAcc,lcCurCode,lnCurrUnit,lnExRate
*  ENDSCAN 
    
*  USE IN (TmpCrdtGL)
    
  SELECT (lcTmpCredt)
  SCAN 
    DO GLDIST WITH &lcTmpCredt..Link_Code,'002',ABS(&lcTmpCredt..Amount)      ,;
                   'CR',&lcTmpCredt..Tran,ldBatDate,lcGLFYear,lcGLPeriod,'&lcTmpGLD',;
                   &lcTmpCredt..cAdjAcct,lcCurCode,lnCurrUnit,lnExRate
    DO GLDIST WITH &lcTmpCredt..Link_Code,'001',-ABS(&lcTmpCredt..Amount)     ,;
                   'CR',&lcTmpCredt..Tran,ldBatDate,lcGLFYear,lcGLPeriod,'&lcTmpGLD',;
                   &lcTmpCredt..cARGlAcc,lcCurCode,lnCurrUnit,lnExRate
  ENDSCAN 
*-- MAN End

  lcGLSeqNo = gfSequence("GLSESSION", gcAct_Comp)
  SELECT (lcTmpGLD)
  REPLACE ALL GLSESSION WITH lcGLSeqNo
  USE IN (lcTmpGLD)
    
  SELECT GLDIST
  APPEND FROM (gcWorkDir+lcTmpGLD)
  
  USE IN IIF(llOpGlDist, "GLDIST" , 0) 
  
  = lfUpdStp(lcTmpCredt, 5)
ENDIF

* ----------------------------------------------------------------

FUNCTION lfvKeyOff

IF !EMPTY(lcAccount)
 *B602838,1 NAD 09/24/2000(Start) Do not allow key of for non active account.
 PRIVATE lnCusRec, lclstAlias
 lclstAlias =ALIAS()
 lnCusRec=RECNO('Customer')
 = SEEK('M'+lcAccount,'Customer')
 IF Customer.Status <> 'A'
   *Message : 40022
    *Non active account . Cannot proceed.
    *Button : 00000
    *Ok
    =gfModalGen('TRM40172B00000','ALERT')
    IF BETWEEN(lnCusRec,1,RECCOUNT('Customer'))
      SELECT Customer      
      GOTO lnCusRec      
    ENDIF  
    SELECT (lclstAlias)
    RETURN
 ENDIF
 SELECT Customer      
 GOTO lnCusRec
 SELECT (lclstAlias)
 *B602838,1 NAD 09/24/2000(End)
  *E301281 (Start)
  IF gfModalGen('INM40161B40015','DIALOG') = 2
    PRIVATE lcProg,lcDosApps,llFound,lcCurPath,lcProgPath
    lcProg = IIF(ATC(' ',"ARKEYOF")>0,SUBSTR("ARKEYOF",1,ATC(' ',"ARKEYOF")-1),"ARKEYOF")
    lcDosApps = gcAppHome+gcWinAppl+'\'
    lcCurPath = SET ('FULLPATH')
    SET FULLPATH ON
    llFound = FILE(lcDosApps+lcProg +'.FXP') .OR. ;
              FILE(lcDosApps+lcProg +'.PRG') .OR. ;
              FILE(lcDosApps+lcProg +'.EXE')
    IF !llFound
      lcDosApps  = gcAppHome
      llFound = FILE(lcDosApps+lcProg +'.FXP') .OR. ;
                FILE(lcDosApps+lcProg +'.PRG') .OR. ;
                FILE(lcDosApps+lcProg +'.EXE')
    ENDIF   

    IF llFound
      lcProgPath = lcDosApps+lcProg+'.FXP'
      DO lfExterAuto WITH "CR",lcAccount,lcCurCode,ldBatDate,;
                          lnExRate,lnCurrUnit IN (lcProgPath)
    ENDIF
    SET FULLPATH &lcCurPath
    *B603352,1 SSH 26/12/1999 fix the bug of goto top in browse file after keyed off.
    SELECT (lcTmpCredt)
    IF !EOF()
      SKIP
    ELSE
      GOTO BOTT
    ENDIF
    llNoThing = lfBrowse()
    *B603352,1 SSH(End)
  ELSE
  *E301281 (End)

    lcParam  = "'" +;
               DTOC(ldBatDate)     + "','" +;
               lcGLFYear           + "','" +;
               lcGLPeriod          + "','" +;
               lcAccount           + "','" +;
               lcCurCode           + "','" +;
               STR(lnExRate,9,4)   + "','" +;
               STR(lnCurrUnit,4,0) + "','" +;
               lcUntSin            + "','" +;
               lcExRSin            + "'"
    DO gpDoProg WITH 'AWRARKEYOF',.F.,'AR', lcParam

  *E301281 (Start)
  ENDIF
  *E301281 (End)
ENDIF  

*:----------------------------------------------------------------
*: Name       : lfChkUnComS
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : To check if there is any privous incomplete 
*:              session for the program.
*:----------------------------------------------------------------
*: Parameters : llFrmSetup : is this call from the screen setup
*:                           or from the lpShow procedure ?
*:                           if .T. means that the call is from 
*:                           the screen setup, otherwise means
*:                           that the call is from the lpShow 
*:                           procedure.
*:----------------------------------------------------------------
*: Returns    : .T. -> Yes I found a privous incomplete session.
*:              .F. -> No I did not find any privous incomplete 
*:                     session.
*:----------------------------------------------------------------
*: Example    : = lfChkUnComS()
*:----------------------------------------------------------------
FUNCTION lfChkUnComS
PARAMETERS llFrmSetup
PRIVATE    llFondSess

*E301077,52 YMA 03/03/99 YMA Open the incomplete session file.
= gfOpenFile (gcDataDir+"UNCMSESS", "TRANS", "SH")
*E301077,52 YMA 03/03/99 End.

*-- If I'm called from the "lpShow" procedure, or I'm called 
*-- from the screen setup for the first time..
llFondSess = .F.
IF (llFrmSetup AND llChkUnCom) OR !llFrmSetup
  
  *-- No, do not check the incomplete session from 
  *-- the screen setup again..
  llChkUnCom = .F.
  
  *-- If I'm called from the screen setup, do not check again
  *-- from the lpShow, otherwise do what you are asked to do..
  llGoAndChk = IIF(llFrmSetup, .F., llGoAndChk)
  
  *-- Is there any previous incomplete program session ???
  
  IF gfUnCompSession(lcProgID, lnThisSess, "Cash Receipt")
  
    *-- If yes, please tell me where did we stop in that session..
    lcCurObj  = ALLTRIM(UnCmSess.cCurrObj)
    
    *-- Restore the neccissary values.
    lcSession = UnCmSess.cSession
    llCUpDate = .T.
    llNoThing = lfBldType (.F.)
    laScrMode = .F.
    laScrMode[ATC(lcScrMode,"SVEA")] = .T.
    
    *-- If you do not know where did we stop, please 
    *-- go to the normal add mode.
    IF EMPTY(lcCurObj)
      *-- Browse the temp file.
      llNoThiong = lfBrAndZom()
      SHOW GETS
    ELSE
      *-- If you know where did we stop in the previous incomplete 
      *-- program session, and you are sure that we were in the
      *-- middle of updating the master files and you are calling
      *-- me from the screen setup, then please go on and finish 
      *-- that update, otherwise reset your variables and go to 
      *-- the normal add mode, and set your focus on the last object
      *-- you were focused on in the last incomplete session...
      IF llFrmSetup AND UPPER(lcCurObj) = "PBSAV"
        *-- Go to the select mode just to give the user the 
        *-- feeling that you are running normal.
        llFondSess = .F.
        
        *-- go on and finish the update..
        lnNoThing  = gfCPSave()
        
        *-- Remember; when you finish to create the 
        *-- temp files again (Just to balnk them out)..
        llNoThing  = lfCratTemp()
        
        *-- Browse the temp file.
        llNoThiong = lfBrAndZom()
      ELSE
        *-- Browse the temp file.
        llNoThiong = lfBrAndZom()
        
        SHOW GETS
        _CUROBJ = OBJNUM(&lcCurObj)
        KEYBOARD "{ENTER}" PLAIN CLEAR
      ENDIF
    ENDIF
  ELSE
    *-- If there is no incomplete session, please create the temp
    *-- files.
    llNoThing = lfCratTemp() AND lfBrAndZom()
  ENDIF
ELSE
  = lfBrAndZom()
ENDIF

RETURN (llFondSess)


*:----------------------------------------------------------------
*: Name       : lfAdUnCmSR
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : To browse the temp credit file and zoom it if needed.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfBrAndZom()
*:----------------------------------------------------------------
FUNCTION lfBrAndZom

llNoThing = lfBrowse()
*IF !(llGLLink AND !llApLink)
*  ZOOM WINDOW (lcPayBrTl) NORMAL ;
*  FROM 10.583-2.8, 2.000 SIZE WROWS(lcPayBrTl)+2.8,WCOLS(lcChWin3)+3.5
*ENDIF
*MODIFY WINDOW (lcchwin3) NOZOOM

*:----------------------------------------------------------------
*: Name       : lfAdUnCmSR
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : To add an incomplete session record.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfAdUnCmSR()
*:----------------------------------------------------------------
FUNCTION lfAdUnCmSR
PRIVATE lnAlias

lnAlias = SELECT(0)

*-- Look for any record with the status "Initial".
SELECT UnCmSess
*B000000,1 Hesham (Start)
IF !SEEK('I'+PADR(lcProgID,10)+PADR(gcUser_id,10))
*IF !SEEK('I')
*B000000,1 Hesham (End)
  *-- If you do not find any, please add a new record.
  APPEND BLANK
ENDIF

*-- Save teh record number..
lnUnCmSeRc = RECNO()

*-- Be sure to blank the record..
BLANK

*-- Update the record with my program information.
REPLACE Status     WITH 'O'       ,;
        cUTranType WITH lcProgID  ,;
        cProgram   WITH lcProgID  ,;
        cCurrScr   WITH lcProgID  ,;
        cUserId    WITH gcUser_id ,;
        cSession   WITH lcSession ,;
        cCurrObj   WITH SPACE(0)  ,;
        dTranDate  WITH gdSysDate ,;
        cTranTime  WITH TIME()
llNoThing = lfUpdVars()


llNoThing = RLOCK()

SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfUpdUnCmS
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : To update the incomplete session record.
*:----------------------------------------------------------------
*: Parameters : lcStatus : Update the record with this status.
*:              lcCurObj : Update the record with this object name.
*:              llSetCon : Lock\Unlock the record.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfUpdUnCmS()
*:----------------------------------------------------------------
FUNCTION lfUpdUnCmS
PARAMETERS lcStatus, lcCurObj, llSetCon
PRIVATE lnAlias

*-- If there is an incomplete session record for this session..
IF lnUnCmSeRc <> 0
  lnAlias  = SELECT(0)
  lcStatus = UPPER(LEFT(lcStatus,1))

  *-- Go and update it..
  SELECT UnCmSess
  GOTO lnUnCmSeRc
  UNLOCK
  REPLACE cCurrObj WITH lcCurObj ,;
          Status   WITH lcStatus
  llNoThing = RLOCK()

  *-- If you are asked to unlock the record, please do..
  IF !llSetCon
    UNLOCK 
  ENDIF

  SELECT(lnAlias)
ENDIF

*:----------------------------------------------------------------
*: Name       : lfUpdVars
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : To update the incomplete session record with the
*:              needed files names and variables.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfUpdVars()
*:----------------------------------------------------------------
FUNCTION lfUpdVars
PRIVATE lnAlias

*-- Build the files names string.
lcFiles = "lcTmpCredt," + lcTmpCredt + ", ;"
*-- Go and update the incomplete session record.
llNoThing = gfSavSess(lcProgID, lcFiles, @laVars, lcSession)

*:----------------------------------------------------------------
*: Name       : lfCratTemp
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : To create the program temp files.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfCratTemp()
*:----------------------------------------------------------------
FUNCTION lfCratTemp
PRIVATE lnAlias

lnAlias = SELECT(0)

*-- Read the "Credit" file structure into an array.
SELECT Credit
lnCrdtFlds = AFIELDS(laCrdtStrc)

*-- Add the "nSteps" field to the temp file to handel the 
*-- incomplete session.

lnCrdtFlds = lnCrdtFlds + 1
DIMENSION laCrdtStrc[lnCrdtFlds, 4]
laCrdtStrc[lnCrdtFlds, 1] = "nSteps"
laCrdtStrc[lnCrdtFlds, 2] = "N"
laCrdtStrc[lnCrdtFlds, 3] = 2
laCrdtStrc[lnCrdtFlds, 4] = 0

*B602666,4 YMA 03/24/99 Commented out, because the field is added to the standard 
*B602666,4 YMA 03/24/99 credit and history file.
lnCrdtFlds = lnCrdtFlds + 1
*DIMENSION laCrdtStrc[lnCrdtFlds, 4]
*laCrdtStrc[lnCrdtFlds, 1] = "cArPType"
*laCrdtStrc[lnCrdtFlds, 2] = "C"
*laCrdtStrc[lnCrdtFlds, 3] = 6
*laCrdtStrc[lnCrdtFlds, 4] = 0
*B602666,4 YMA 03/24/99 End.

*B603091,1 Added Tag on account.
DIMENSION laTags[1,2]
laTags[1,1] = 'Account'
laTags[1,2] = 'Account'
*B603091,1 End.
*-- Create the three temp files.
llNoThing = gfCrtTmp(lcTmpCredt, @laCrdtStrc, @laTags)
SELECT (lcTmpCredt)
SET ORDER TO

SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfUpdStp
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : To update the nStep field in a specific temp file..
*:----------------------------------------------------------------
*: Parameters : lcFileName : Please update this file.
*:              lnStepNo   : Please update the step field with this
*:                           number.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfUpdStp("ARHIST", 1)
*:----------------------------------------------------------------
FUNCTION lfUpdStp
PARAMETERS lcFileName, lnStepNo
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT (lcFileName)
*-- Update the needed file with the new step number..
llNoThing  = RLOCK()
REPLACE nSteps WITH lnStepNo
UNLOCK
SELECT(lnAlias)

* ----------------------------------------------------------------

FUNCTION lfvArPayT
PARAMETERS llFrmPrg
PRIVATE lnAlias

lcValue = IIF(&lcTmpCredt..lNonAR   ,;
              laNArPayT[lnNArPayT,2] ,;
              laArPayT[lnArPayT,2]  )

*-- Blank the related GL information variables.
lcGLAcct1 = SPACE(0)

*-- Create a temp array to hold the "gfRltFld" parameter.
DIMENSION laTermAry[1,2]
laTermAry[1,1] = "CARGLACC"
laTermAry[1,2] = "lcGLAcct1"

*-- Fill the related GL information from the codes file.
llNoThing = gfRltFld(lcValue, @laTermAry, "CARPTYPE")

llAccept = .T.
lcEmpty  = STRTRAN(lcAcMask,"9",SPACE(1))
lcEmpty  = STRTRAN(lcEmpty ,"!",SPACE(1))
lcEmpty  = STRTRAN(lcEmpty ,"X",SPACE(1))
lcEmpty  = STRTRAN(lcEmpty ,"#",SPACE(1))
llEmpty  = EMPTY(lcGLAcct1) OR (lcGLAcct1 = lcEmpty)

*B603056,1 Validate and update G/L accounts if we have a G/L link
*IF llEmpty
IF llEmpty .AND. llGLLink
*B603056,1 end
  IF &lcTmpCredt..lNonAR
    IF !llFrmPrg
      *-- The selected payment method has no related AR account. 
      *-- Cannot accept.
      *-- < Ok >
      llAccept = gfModalGen("TRM40120B00000") = 500
    ENDIF
  ELSE
    *E301077,52 YMA 03/03/99 Start- Open the GL_Link file.
    = gfOpenFile (gcDataDir+"GL_LINK", "GL_LINK", "SH")
    *E301077,52 YMA 03/03/99 End.
    
    llNoThing  = SEEK("M"+&lcTmpCredt..Account, "Customer")
    lcGLAcct1  = lfGetGLAcc(Customer.Link_Code,"001")
  ENDIF
ENDIF
lnAlias = SELECT(0)
SELECT (lcTmpCredt)
llNoThing = RLOCK()
REPLACE cArPType WITH lcValue ,;
        cARGlAcc WITH lcGLAcct1
UNLOCK
SHOW WINDOW (lcPayBrTl) REFRESH
SELECT(lnAlias)

IF !llAccept
  _CUROBJ = _CUROBJ
ENDIF

*:----------------------------------------------------------------


*!***************************************************************
*! Name      : lfPrtRectpt                      *B603091,1 Added.
*! Developer : TAK
*! Date      : 08/04/1999
*! Purpose   : Print cash receipt.
*!***************************************************************
FUNCTION lfPrtRectpt

*--Get the current site warehouse code.
=gfOpenFile (gcDataDir+"WAREHOUS", "WAREHOUS", "SH")
GO TOP
IF gfGetMemVar('M_SYSTYPE') = 'P' AND (OCCURS('CM',gcCmpModules)<>0)
  LOCATE FOR cSiteId = gcCurSite
ENDIF
SELECT (lcTmpCredt)
lcFlTag=TAG()
SET ORDER TO TAG Account
GO TOP

*--Start to print.
lcPhonPict = gfPhoneTem()
lcRpName = 'PSCRCPT'
lcConsSet = SET('CONSOLE')
SET CONSOLE OFF
lcDevice = gcDevice 
gcDevice = 'PRINTER'
lcOGPlatForm = 'DOS'
lcOutFile = gcOutFile
_PEJECT   = "NONE"
DEFINE WINDOW lcTmpWin FROM 1,1 TO 2,2
ACTIVATE WINDOW lcTmpWin NOSHOW
DO gfDispRe WITH gcRepHome+gcWinAppl+"\"+lcRpName
RELEASE WINDOW lcTmpWin
gcDevice = lcDevice 
gcOutFile = lcOutFile
SET CONSOLE &lcConsSet
SET PRINT TO
SET DEVICE TO SCREEN
SELECT (lcTmpCredt)
SET ORDER TO TAG &lcFlTag
RETURN


*!**************************************************************************
*! Name      : lfHgWUpdat
*! Developer : Sameh (SSE)
*! Date      : 06/09/1999
*! Purpose   : Update nHgWtrMark field (Customer) with NETBAL, if NETBAL is greater
*!*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************************
*! Passed Parameters  : None
*!*************************************************************************
*! Returns            : None
*!*************************************************************************
*! Example   : =lfHgWUpdat()
*!*************************************************************************
*E301245,1
FUNCTION lfHgWUpdat
=RLOCK()
REPLACE nHgWtrMark WITH IIF(NETBAL>nHgWtrMark,NETBAL,nHgWtrMark)
UNLOCK
*-- End of lfHgWUpdat.


*!**************************************************************************
*! Name      : lpCtrlIns
*! Developer : Nader Anis  (NAD)
*! Date      : 08/29/2000
*! Purpose   : To overwrite the global (New) menu pad option.
*! Ref       : B603055,1 
*!*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************************
*! Passed Parameters  : None
*!*************************************************************************
*! Returns            : None
*!*************************************************************************
*! Example   :  DO lpCtrlIns
*!*************************************************************************

PROCEDURE lpCtrlIns
IF laScrMode[2]=.T.
  =gfvCPNew()
ENDIF  


*!*************************************************************
*! Name      : lfvFactor
*! Developer : Khalid Mohi El-Din
*! Date      : 06/15/2003
*! Purpose   : Valid function for the factor field
*!*************************************************************
*! Calls     : FacChk
*!*************************************************************
*! Example            :  =lfvFactor()
*!*************************************************************
*! *B607231,1 KHM 06/15/2003 
*!*************************************************************

FUNCTION lfvFactor

=gfOpenFile(gcSysHome+'SycFact',gcSysHome+'cFacCode','SH')

*-- If press the browse icon or enter a factor does not exist in the
*-- factors file, call global function to browse factors.
IF llBrowse .OR. (!EMPTY(lcFactor) .AND. !SEEK(lcFactor , "SycFact"))
  SELECT SycFact
  IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
    GO RECNO(0)
  ELSE
    GO TOP
  ENDIF
  *-- Save the current browse fields.
  lcSaveBrow = lcBrFields
  lcBrFields = [cFacCode :10:H="Factor",cFacComp :32:H="Factor Company",]+;
    [cPhoneNo :22:H="Phone",cFacCont :32:H="Contact",]+;
    [cCont_Code :17:H="Country Code"]

  *-- Save the current browse title.
  lcOld_ttl  = lcFile_ttl
  lcFile_ttl = "Factors"
  DECLARE laFactors[1]
  laFactors[1] = lcFactor

  *-- Call the browse with the order file.
  =gfBrows(.F. , "cFacCode" , "laFactors")

  *-- Restore the previous browse fields & browse title.
  lcBrFields = lcSaveBrow
  lcFile_ttl = lcOld_ttl
  *lcFactor   = IIF(lcFactor = laFactors[1] , SPACE(6) , laFactors[1])
  lcFactor   =  laFactors[1]
ENDIF
SHOW GET lcFactor

*-- Set the browse flag to false.
llBrowse = .F.

*-- Add the factor to the current record & refresh the browse.
SELECT (lcTmpCredt)
*-- Lock the file to grantee the phiscal update.
=RLOCK()
REPLACE cFacCode WITH lcFactor
UNLOCK
