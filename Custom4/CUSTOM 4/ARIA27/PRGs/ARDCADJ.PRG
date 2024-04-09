*:************************************************************************************
*: Program file  : ARDCADJ.PRG
*: Program desc. : Program To Enter Credit Or Debit Adjustment.
*: For screen    : ARDCADJ.SCX (0,1,2)
*:         System: ARIA BUSINESS SYSTEM
*:         Module: Accounts Receivable Module
*:      Developer: Reham Al-Allamy
*:************************************************************************************
*: Documented *E300459,1 Rewrite the program to work in the 2.7 version               
*:************************************************************************************
*: Calls         :
*:         Procedures : lpTab
*:                      lpShTab
*:         Programs   : None
*:         Screens    : ARDCADJ.SPX (Main Debit & Credit Adjustment Screen)
*:                      ARCHRBK.SPX (Chargeback Screen)
*:         Functions  : gfSetup
*:                      lfGtSavBar
*:                      lfwBrowTrn
*:                      lfvBrows
*:                      lfWhenBrow
*:                      lfActMain
*:                      lfDactMain
*:                      lfvKey
*:                      lfvBatchDt
*:                      lfOldCVal
*:                      lfOldNVal
*:                      lfvCurrency
*:                      lfvExRate
*:                      lfvTrnCnt
*:                      lfvAmount
*:                      lfvAccount
*:                      lfvReson
*:                      lfvRef
*:                      lfvFactor
*:                      lfvTrnAmt
*:                      lfvNewTrn
*:                      lfvRemTrn
*:                      lpClsScr
*:                      gfCPSave
*:                      lfvInvoice
*:                      lfvRepCode
*:                      lfvComm
*:                      lfvChrgOk
*:                      lfUpdVars
*:                      lfAdUnCmSR
*:                      lfCrtUnComp
*:*************************************************************
*: Passed Parameters  : MCHOICE    -> Debit or credit
*:                      lcMainAcct -> Passed account code.
*:*************************************************************
*: Example            :
*:  DO gpDoProg WITH 'AWRARDCADJ',.F.,'AR',"D","00001"
*:*************************************************************
*:  Modifications :
*B601927,1 Hesham El-Sheltawi 12/15/97
*B601927,1 make the program call the uncomplete session global functions
*B601927,1 insted of making these functions as local functions
*E300921,1 TAK 07/15/98
*E300921,1 Added to update the APPAYMNT file in case of PS Module was installed.
*B801808,1 Reham On 11/15/98
*B801808,1 Fix the error of "File Appaymnt not found".
*B801807,1 Reham On 11/15/98
*B801807,1 Restore the old currency & its exchange rate in case of return
*B801807,1 with zero exchange rate from "gfChkRate".
*E301077,14 Reham On 12/29/98
*E301077,14 Reduce the opened file on the program level:
*E301077,14 1- For Credit Adjustment program, change the files in SydObjct:
*E301077,14    From : GLDIST,CUSTOMER,CODES,CREDIT,INVHDR,SALESREP,REPCOMM,UNCMSESS,SYUEROR,ARCUSHST,SYCFACT|GLDISTAC,CUSTOMER,CCODE_NO,CREDIT,INVHDRA,SALESREP,REPCOMM,TRANS,CSESSION,ACTHST,CFACCODE
*E301077,14    To   : CUSTOMER,CODES,CREDIT,INVHDR,SALESREP,REPCOMM,UNCMSESS|CUSTOMER,CCODE_NO,CREDIT,INVHDRA,SALESREP,REPCOMM,TRANS
*E301077,14 2- For Debit Adjustment program, change the files in SydObjct:
*E301077,14    From : DEBIT,GLDIST,CUSTOMER,CODES,UNCMSESS,SYUEROR,ARCUSHST,SYCFACT|DEBIT,GLDISTAC,CUSTOMER,CCODE_NO,TRANS,CSESSION,ACTHST,CFACCODE
*E301077,14    To   : DEBIT,CUSTOMER,CODES,UNCMSESS|DEBIT,CUSTOMER,CCODE_NO,TRANS
*E301077,14 3- Open the removed files from SydObjct in the current program.
*B602537,1 Reham On 17/02/99
*B602537,1 Fix the error of record is out range upon exit..
*E301245,1 SSE 06/09/1999 Added a new field nHgWtrMark and calcualte its value
*                         from NETBAL field , if less than NETBAL update with
*                         NETBAL else leave it with existing value
*E301266,1 AKA 06/23/99 Adding one more field in  APPAYMNT file (batch) and update it
*E301266,1              with the value of CREDIT.BATCH
*B603127,1 WALID ABOU EL MAGD 08/25/99 Fix the bug all transactions that made
*B603127,1 In the same sesion take the same batch number
*E301326,1 WAB 09/28/1999 when select debit adjustment code (consider as chargeback) the trantype 
*E301326,1                Will BE '3' same as that generated from key off screen  
*E301326,1                Otherwise it will generate normal debit adjustment record .
*B603187,1 RENEE 10/05/1999 Allow a tape total between 0 and 1
*C101632,1 ADEL 10/30/1999 1- Standard modifications in case of chargeback: (IF llChrgBack)
*C101632,1                   * Add a new get field "T. Date" in case of entering chrageback.
*C101632,1                   * Add a new get field "Store" in case of entering chrageback.
*C101632,1                 2- A customized debit & credit for " Krazi Kat" needed to some modifacations
*C101632,1                    These modifications will merely be in effect if llChrgBack (i.e chargeback case)
*C101632,1                    (TYPE("llKRAChrge")="L" AND llKRAChrge). llKRAChrge is As this varialbellChrgBack  is defined merely in the custom program ARKRADC.PRG
*B603329,1 SSH 26/12/1999 Fix the bug of not get the corect second account name.
*B603414,1 NAD 02/01/2000 - Change the header for DESC field from Description to Adjustment reason
*B603418,1 NAD 02/23/2000   Change the DueDate Field in the Debit adjustment Screen to be equal 
*B603418,1                  to batch date (Trandate Field)
*E301369,1 NAD 05/02/2000   Add the due date screen field. 
*B603612,1 NAD 05/02/2000   Fix bug llChrgBack in the credit adjustment screen
*E301420,1 NAD 05/28/2000   Extend the transaction amount, Tape amount  fields to be 9999999999.99
*E301420,1                  and the difference,actual amount field to be 9999999999999.99  
*E301420,1                  and lnCharge1 ,lnCharge2 In ARCHRBK.SCX
*B803369,1 NAD 07/20/2000   Factor code entered in customer screen doesn't show up by default. 
*E500351,1 NAD 07/24/2000   Add the description field in both the debit and the credit adjustment screens.
*B802622,1 ABD 08/02/2000 Add the decscription of the reason code to the decscription filed by default
*B802622,1 ABD            When the user select the reason decscription.
*E500374,5 HBG 08/15/2000 the transaction will be posted to the A/R account of the  
*E500374,5                Factor instead of the customer if there is a Factor code
*B602838,1 NAD 09/24/2000 Not to allow transaction for hold account.
*B603959,1 ASH 10/11/2000 Allow Hold account to do Credit/Debit Adjusments.
*B604059,1 ASH 12/07/2000 Wrong updating the charge back field in the customer file.
*E301521,1 AHM 01/01/2001 Extract the saving function in individulal program to be used from the NC module
*B604340,1 AME 07/09/2001 fix the bug of not showing repcode untill select some fields in back charge screen.
*E301802,1 SSE 02/03/2002 Enhancement add an option menu for Auto Carry Account option.
*B606646,1 RAE 11/18/2002 Sales rep comm doesn't appear by defalut in sales rep charge back screen.
*B607459,1 KHM 08/05/2003 Adding the mdown() to valid when leaving the field using the mouse.
*:**************************************************************************************************
*
PARAMETER MCHOICE , lcMainAcct

*E301802,1 Define variable for Auto Carry Account. [Begin]
PRIVATE llAutoAcct
llAutoAcct = .F.
*E301802,1 Define variable for Auto Carry Account. [End]

*-- Variable hold the adjustment type.
lcMode    = IIF(MCHOICE = "D" , "Debit" , "Credit" )
*-- Determine the used file in the current screen.
lcCurAlis = IIF(MCHOICE = "D" , "Debit" , "Credit" )

*-- To force the control pannel to call the local close
*-- procedure(lpClsScr)
laDefProc[10] = .F.

*-- Initialize all the variables needed in this program..

*-- Flags to know if the files are opened or not.
*E500374,5 HBG 08/15/2000 Add flag to open factor file [Begin]
*STORE .F.       TO llOpnCurr , llOpnGLLnk , llOpnAPChk , llOpnApPay , llOpenGLD 
STORE .F.       TO llOpnCurr , llOpnGLLnk , llOpnAPChk , llOpnApPay , llOpenGLD , llOpenFac
*E500374,5 HBG 08/15/2000 [End] 
*-- Variables hold the record no. if any file was open in another session.
STORE 0         TO lnRecCurr , lnRecGLLnk , lnRecAPChk , lnRecApPay , lnRecGLDst

*-- Variables hold the company setups values.
STORE .F.       TO llLink_Gl , llLink_AP , llMulCurr , llEditExRt , llRepCB ,;
  llValidDat , llAddMode
*-- Store true to the currency flags because there is default base currency.
STORE .T.       TO llValidCur , llValidRat

*-- Define the numeric variables used in the main screen.
STORE 0         TO lnTrnCnt , lnAmount , lnActual , lnDiff , ;
  lnOldValue , lnMarker , lnSavBarNo
*-- Define the string variables used in this program.
STORE ""        TO lcAccount , lcAccName , lcCode , lcRef , ;
  lcFactor , lcOldValue , lcAdjAcct , lcLinkCode , ;
  lcBnkCode , lcChkAcct
*E500351,1 (Start) Variable to hold the description.
STORE "" TO lcDesc
*E500351,1 (End) 
*-- Define string Variables used in chargeback salesrep screen if credit adj.
STORE " "       TO lcInvoice , lcRepCode1 , lcRepName1 , lcRepCode2 , ;
  lcRepName2 , lcTerms , lcOrder , lcStore , lcCustPo
*-- Define numeric Variables used in chargeback salesrep screen if credit adj.
STORE  0        TO lnDiscount , lnCharge0 , lnComm1 , lnCharge1 , lnComm2 , lnCharge2

*-- Assign the temporary files names'.
STORE " "       TO lcAdjTemp  , lcRepCBTmp , lcGLTemp , ;
  lcDC_Adj0 , lcDC_Adj1 , lcDC_Adj2 , lcAP_Temp

*-- Define variables the status (Disable or Enable) for the different objects.
STORE "DISABLE" TO lcRatStat , lcRemStat , lcNewStat , lcAccStat

*-- Define the arrays used in the transaction popup.
DECLARE laReson[1] , laCodInfo[1,10]

*-- Declare a flag to determine whether the system is
*-- linkeg to GL and AP or not.
llGLAP      = .F.


*E301369,1 (Start) Add the due date screen field in case of debit adjustment only
IF MCHOICE= 'D'
  ldDueDate={}
ENDIF  
*E301369,1 (End)
 
*E301326,1 WAB - define variable for chkbox field (Debit on Account)
*E301326,1 WAB - in case of debit adjustment
*E301326,1 WAB - START 
IF MCHOICE = "D" 
  llChrgBack = .F.
ENDIF
*E301326,1 WAB - END

*-- Variables for the current year & period.
lcGLFYear  = SPACE(4)
lcGLPeriod = SPACE(2)
ldBatchDt  = gdSysDate
lcBatchNo  = ""
ldOldDate  = {}
lnTrnAmt   = 0              && Variable hold the transaction amount.
lnAdjCnt   = 0              && Variable to hold the no. of adjustments.
llBrowse   = .F.            && Variable to hold flag to display browse.
lcAdjBrwTl = 'Transactions' && Variable hold the browse title.
*lcCurrCode = ' '            && Variable to hold currency code.
*lnExRate   = 0              && Variable to hold exchange rate.
*lnCurrUnit = 0              && Variable to hold currency unit.
*lcUntSin   = ' '            && Variable to hold unit sign.
*lcExRSin   = ' '            && Variable to hold exchange rate sign.
lcCurrCode = gcBaseCurr      && Variable to hold currency code.
lnExRate   = 1               && Variable to hold exchange rate.
lnCurrUnit = 1               && Variable to hold exchange rate.
lcUntSin   = '/'             && Variable to hold unit sign.
lcExRSin   = '/'             && Variable to hold exchange rate sign.
*B801807,1 Reham On 11/15/98  *** Begin ***
*B801807,1 Variable to hold the old exchange rate.
lnOldRate  = 0
*B801807,1 Reham On 11/15/98  *** End   ***

*C101632,1 (Begin) Initialize the two variables.(ldTranDate is initialized as string not to appear on the screen if t is date and disable)
STORE ' ' TO ldTranDate,lcStore
*C101632,1 (End)  

*-- Get the prompt of the available pictures on the screen.
lcABrowBmp  = gcBmpHome + "ExtKey.BMP"
lcANewBmp   = gcBmpHome + "New.BMP"
lcARemBmp   = gcBmpHome + "Rem.BMP"
lcAOkBmp    = gcBmpHome + "Ok.BMP"
lcACanBmp   = gcBmpHome + "Can.BMP"

*-- Variables that hold the red and green colors used to display the
*-- "Difference" object on the screen.
lcRedClr   = "RGB(,,,255,0,0)"
lcGreenClr = "RGB(,,,0,128,0)"

*-- Variable hold the main screen fields.
lcScFields = "Account"

*-- Following variables are used to deal with the uncomplete program
*-- session concept.
lcSession  = SPACE(1)
lnUnCmSeRc = 0
llGoAndChk = .T.
lcScrMode  = SPACE(1)
lnSessNo   = gnProgCopy
lcProgID   = IIF(MCHOICE = "D" , PADR("DEBIT_ADJ", 10) , PADR("CREDIT_ADJ", 10))
llContinue = .F.

*-- Variable to tell if are we going to check the existance of
*-- any incomplete session in the screen's setup or not.
llChkUnCom = .T.

*-- Variables that need to be saved and restored when detecting an
*-- uncomplete program session.
DIMENSION laVars[16]
laVars[01] = "lcScrMode"
laVars[02] = "ldBatchDt"
laVars[03] = "lnTrnCnt"
laVars[04] = "lnAmount"
laVars[05] = "lnActual"
laVars[06] = "lnDiff"
laVars[07] = "lcCurrCode"
laVars[08] = "lnExRate"
laVars[09] = "lnCurrUnit"
laVars[10] = "lcUntSin"
laVars[11] = "lcExRSin"
laVars[12] = "lnUnCmSeRc"
laVars[13] = "lcGLFYear"
laVars[14] = "lcGLPeriod"
laVars[15] = "lcBatchNo"
laVars[16] = "lnAdjCnt"
*-- Call global function in the main program to do the following : _
*-- Intialise all the variables & open all the files needed in
*-- this session and controling disabling and enabling of the
*-- menu bars and writting the screen names in the window bars.
IF !gfSetup()
  RETURN
ENDIF

llNoShow  = .F.     && Flag to force the execution of the show procedure.

*E301077,14 Reham On 12/29/98   *** Begin ***
*E301077,14 Open the company file.
=gfOpenFile(gcSysHome+'SycComp',gcSysHome+'cComp_Id','SH')
*E301077,14 Reham On 12/29/98   *** End   ***

*-- If entering the screen for the first time.
IF !WEXIST(gcBaseWind)

  *-- Array hold the header's codes info.
  laCodInfo[1,01] = IIF(MCHOICE = "D" , "TRANCODE" , "CCREDITCOD")  && Field Name
  laCodInfo[1,02] = "laReson"      && Array Name
  laCodInfo[1,03] = "lnReson"      && Popup Name
  laCodInfo[1,04] = ""             && Popup Status  ("D"->Default,"A"->All)
  laCodInfo[1,05] = .F.            && Include "N/A" (.T.->Yes,.F.,No)
  laCodInfo[1,06] = .F.            && Include "ALL" (.T.->Yes,.F.,No)
  laCodInfo[1,07] = lcAdjTemp      && Alternative File (For default val.)
  laCodInfo[1,08] = lcAdjTemp      && Use this index for the Alternative file.
  laCodInfo[1,09] = "&lcAdjTemp..Account + &lcAdjTemp..Tran + DTOS(&lcAdjTemp..TranDate)"    && Seek this expretion.
  laCodInfo[1,10] = IIF(MCHOICE = "D" , "TRANCODE" , "CCREDITCOD")  && Alternative Field Name

  *-- Empty the screen base file to avoid geeting an error when
  *-- switching the screen modes.
  lcBaseFile = SPACE(0)

  *-- Flag to know if there is any of the screen's objects has been updated or not.
  llCUpDate  = .F.

  llLink_Gl  = ALLTRIM(gfGetMemVar("M_LINK_GL")) = "Y"
  *E300921,1 (Start) Ap link was active if AP or PS modules was installed.
  *llLink_AP  = (OCCURS('AP',gcCmpModules) <> 0)
  llLink_AP  = (OCCURS('AP',gcCmpModules) <> 0) OR (OCCURS('PS',gcCmpModules) <> 0)
  *E300921,1 (End).
  *-- Check if the system is setup to use multiple currencies.
  llMulCurr  = gfGetMemVar('llMulCurr' , gcAct_Comp)
  llEditExRt = gfGetMemVar('LLEDITEXRA' , gcAct_Comp)
  llRepCB    = IIF(MCHOICE = "C" .AND. ALLTRIM(gfGetMemVar("M_REPCB")) = 'Y', .T. , .F.)

  *-- Assign the temporary files names'.
  lcAdjTemp  = gfTempName()
  lcRepCBTmp = gfTempName()
  lcGLTemp   = gfTempName()
  lcAP_Temp  = gfTempName()

  *-- Assign the temporary window names.
  lcDC_Adj0  = gfTempName()
  lcDC_Adj1  = gfTempName()
  lcDC_Adj2  = gfTempName()

  =gfwCodePop(@laCodInfo , IIF(MCHOICE="D","TRANCODE","CCREDITCOD") , "N")

  *-- Get a session number, to be updated in the uncomplete session
  *-- number file foe each session.
  lcSession  = gfsequence('CSESSION')

ENDIF

*-- Variable to control the enabling & disabling of the exchange rate object.
lcRatStat  = IIF(laScrMode[1] AND !(lcCurrCode = gcBaseCurr) AND llEditExRt , "ENABLE" , "DISABLE")

*-- Variable hold the status of the remove button.
lcRemStat  = IIF(laScrMode[4] .AND. lnAdjCnt > 0 , "ENABLE" , "DISABLE")
*-- Variable hold the status of the new button.
lcNewStat  = IIF(laScrMode[4] , "ENABLE" , "DISABLE")
lcAccStat  = IIF(EMPTY(lcMainAcct) , IIF(laScrMode[4] .AND. lnAdjCnt > 0 , "ENABLE" , "DISABLE" ) , "DISABLE" )

SELECT (lcCurAlis)
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER

*-- Clear all the trapped keys.
PUSH KEY
PUSH MENU _MSYSMENU
lnSavBarNo = lfGtSavBar()
ON SELECTION BAR lnSavBarNo OF P01PU01 DO gfCPSave

*-- Define bar to activate the browse.
DEFINE BAR 100 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 101 OF P01PU01 PROMPT lcAdjBrwTl KEY ALT+B
*-- Activate the browse when selecting its bar.
ON SELECTION BAR 101 OF P01PU01 ACTIVATE WINDOW (lcAdjBrwTl)

*-- Call local function to open the needed files.
=lfOpnFil()
*-- Call the screen.
*DO (gcScrDir+gcWinAppl+"\ARDCADJ.SPX")
DO (gcScrDir+"ARDCADJ.SPX")

*-- Call local function to close the files opened in this session...
=lfClsFil()

*-- Release the browse window.
RELEASE WINDOW (lcAdjBrwTl)

POP MENU _MSYSMENU
POP KEY

SELECT (lcCurAlis)
SET RELATION TO

*-- If quitting the program erase the temp. files.
IF glQuitting
  *-- Erase the temp. files.
  IF USED(lcGLTemp)
    USE IN (lcGLTemp)
  ENDIF
  ERASE gcWorkDir + lcGLTemp  + ".DBF"

  IF USED(lcAdjTemp)
    USE IN (lcAdjTemp)
  ENDIF
  ERASE gcWorkDir + lcAdjTemp + ".DBF"
  ERASE gcWorkDir + lcAdjTemp + ".CDX"

  IF USED(lcRepCBTmp)
    USE IN (lcRepCBTmp)
  ENDIF
  ERASE gcWorkDir + lcRepCBTmp + ".DBF"
  ERASE gcWorkDir + lcRepCBTmp + ".CDX"
ENDIF

*!*************************************************************
*! Name      : lfGtSavBar
*! Developer : Reham Al-Allamy
*! Date      : 07/31/1997
*! Purpose   : Function to return the bar no of the save bar.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lfGtSavBar
*!*************************************************************
*
FUNCTION lfGtSavBar
PRIVATE lnRetVal

*-- Open the menu file to get the bar no. of the save bar.
USE (gcSysHome+"SYCMenu") IN 0 ORDER Pross_ID AGAIN ALIAS MenuFile

lnRetVal = IIF(SEEK("GFCPSAVE", "MenuFile"), VAL(MenuFile.cBar_Pos), 51)

USE IN MenuFile

RETURN (lnRetVal)

*!*************************************************************
*! Name      : lpShow
*! Developer : Reham Al-Allamy
*! Date      : 07/30/1997
*! Purpose   : Show procedure.
*!*************************************************************
*! Calls     : lfwBrowTrn, lfCrtUnComp, gfwCodePop,
*!             gfRltFld, lfRefresh, lfChkUnComS
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lpShow
*!*************************************************************
*
PROCEDURE lpShow

DO CASE
CASE laScrMode[1]

  *B603127,1 make this variable empty each time we enter the select mode[Start]
  lcBatchNo  = ""
  *B603127,1 [End..]
  IF llGoAndChk AND lfChkUnComS()
    RETURN
  ENDIF
  *-- Variables for the current year & period.
  lcScrMode  = "S"            && Variable hold the current mode.
  lcGLFYear  = SPACE(4)       && Variable hold gl fiscal year.
  lcGLPeriod = SPACE(2)       && Variable hold gl fiscal period.
  ldBatchDt  = gdSysDate      && Variable hold the batch date.
  *    lcCurrCode = ' '            && Variable to hold currency code.
  *    lnExRate   = 0              && Variable to hold exchange rate.
  *    lnCurrUnit = 0              && Variable to hold currency unit.
  *    lcUntSin   = ' '            && Variable to hold unit sign.
  *    lcExRSin   = ' '            && Variable to hold exchange rate sign.
  lcCurrCode = gcBaseCurr      && Variable to hold currency code.
  lnExRate   = 1               && Variable to hold exchange rate.
  lnCurrUnit = 1               && Variable to hold exchange rate.
  lcUntSin   = '/'             && Variable to hold unit sign.
  lcExRSin   = '/'             && Variable to hold exchange rate sign.

  STORE .F. TO llValidDat     && Flag to validate the batch date.

  *-- If multi currency, define flags to validate the currency & the rate.
  *IF llMulCurr
  *  STORE .F. TO llValidCur , llValidRat
  *ELSE
  llValidCur = .T.        && Flag to avoid validating the currency code.
  llValidRat = .T.        && Flag to avoid validating the Exchange rate.
  *ENDIF
  *-- Define variables hold the available objects in the main screen.
  STORE 0   TO lnTrnCnt , lnAmount , lnActual , lnDiff , ;
    lnOldValue , lnMarker , lnTrnAmt , lnAdjCnt
  STORE ""  TO lcAccount , lcAccName , lcCode , lcRef , ;
    lcFactor , lcOldValue , lcAdjAcct , lcLinkCode , ;
    lcBnkCode , lcChkAcct
    
  *E500351,1 (Start) clear the variable in the select mode
  STORE "" TO lcDesc
  *E500351,1 (End)
  *-- Variables if credit & chargeback salesrep.
  STORE " " TO lcInvoice , lcRepCode1 , lcRepName1 , lcRepCode2 , ;
    lcRepName2 , lcTerms , lcOrder , lcStore , lcCustPo
  STORE  0  TO lnDiscount , lnCharge0 , lnComm1 , lnCharge1 , lnComm2 , lnCharge2

  *-- Store "Disable" in all the status variables for the objects exist
  *-- in the main screen.
  STORE "DISABLE" TO lcRemStat , lcNewStat , lcAccStat

  *E301326,1 WAB - define variable for chkbox field (Debit on Account)
  *E301326,1 WAB - in case of debit adjustment
  *E301326,1 WAB - START 
  IF MCHOICE = "D" 
    llChrgBack = .F.
  ENDIF
  *E301326,1 WAB - END

  *-- Fill the transaction adjustment code with N/A.
  =gfwCodePop(@laCodInfo, IIF(MCHOICE="D","TRANCODE","CCREDITCOD") , "N")

  *-- Zap all the used temp. file.
  IF USED(lcGLTemp)
    SELECT (lcGLTemp)
    ZAP
  ENDIF
  *-- Call function to recreate the temp. files used in this program.
  =lfCrtUnComp()
  *E301369,1 (Start) initialais the due date screen feild
  IF MCHOICE = 'D' AND !EMPTY(lcAccount)
    STORE {}  TO ldDudate 
  ENDIF 
  *E301369,1 (End)
CASE laScrMode[4]
  IF !llAddMode
    *-- Flag to force entering the lpClsScr procedure.
    llCUpDate = .T.
    *-- Flag to know if enter the add mode before or not.
    llAddMode  = .T.

    *-- Fill the popups with the default.
    =gfwCodePop(@laCodInfo, IIF(MCHOICE="D","TRANCODE","CCREDITCOD") , "D")
    lcCode = laReson[lnReson , 2]

    *-- Get the related fields for the adjustment code.
    DECLARE laAdjRltFd[3,2]
    laAdjRltFd[1,1] = 'CADJACCT'
    laAdjRltFd[1,2] = 'lcAdjAcct'
    laAdjRltFd[2,1] = 'CBNKCODE'
    laAdjRltFd[2,2] = 'lcBnkCode'
    laAdjRltFd[3,1] = 'CCHKACCT'
    laAdjRltFd[3,2] = 'lcChkAcct'
    =gfRltFld(lcCode , @laAdjRltFd , IIF(MCHOICE="D","TRANCODE","CCREDITCOD"))
  ENDIF
  lcScrMode  = "A"
  llNoThing  = IIF(lnUnCmSeRc=0, lfAdUnCmSR(), .T.)
  *-- Variable hold the status of the remove button.
  lcRemStat  = IIF(laScrMode[4] .AND. lnAdjCnt > 0 , "ENABLE" , "DISABLE")

  *-- Variable hold the status of the new button.
  lcNewStat  = IIF(laScrMode[4] , "ENABLE" , "DISABLE")
  lcAccStat  = IIF(EMPTY(lcMainAcct) , IIF(laScrMode[4] .AND. lnAdjCnt > 0 , "ENABLE" , "DISABLE" ) , "DISABLE" )
ENDCASE

*-- Call the browse function.
=lfwBrowTrn()
*-- Call the browse when function.
=lfWhenBrow()

*E301326,1 WAB - Remove this part since the same code is included in 
*E301326,1 WAB - lfWhenBrow() called above this paragraph of code
*E301326,1 WAB - START 
*-- Refresh the objects in the main screen.
*SHOW GET ibAccount &lcAccStat
*SHOW GET lcAccount &lcAccStat
*SHOW GET lnReson   &lcRemStat
*SHOW GET lcRef     &lcRemStat
*SHOW GET ibFactor  &lcRemStat
*SHOW GET lcFactor  &lcRemStat
*SHOW GET lnTrnAmt  &lcRemStat
*SHOW GET pbRem     &lcRemStat
*SHOW GET pbUpdate  &lcRemStat
*SHOW GET pbNew     &lcNewStat
*E301326,1 WAB - END

*-- Call function in the screen spr to refresh the say fields
*-- in the main screen
=lfRefresh()

*!*************************************************************
*! Name      : lfwBrowTrn
*! Developer : Reham Al-Allamy
*! Date      : 07/30/97
*! Purpose   : Function call the browse called from main screen
*!           : Setup.
*!*************************************************************
*! Calls     : lfWhenBrow, lfvBrows
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfwBrowTrn()
*!*************************************************************
*
FUNCTION lfwBrowTrn

*-- Display the transactions lines in the browse from the temp. files
SELECT (lcAdjTemp)
lnMarker = RECNO()

*E301326,1 WAB - add field in browse fields (debit on acc.)
*E301326,1 WAB - in case of debit adjustment
*E301326,1 WAB - START 
*BROWSE FIELDS ;
  cMarker=IIF(RECNO()=lnMarker ,">"," "):H=" ":R:1:W=.F. ,;
  Account  :H="Acc #" :8 ,;
  lcDummy1=LOOKUP(CUSTOMER.BtName,"M"+&lcAdjTemp..Account,CUSTOMER.ACCOUNT,"CUSTOMER") :H="Name":22,;
  DESC :H="Description":23 ,;
  REFERENCE :H="Reference":20 ,;
  cFacCode :H="Factor":9 ,;
  Amount :H="Amount":14 ;
  WINDOW  (lcDC_Adj1) ;
  IN WINDOW (gcBaseWind) ;
  NOMENU           ;
  NOEDIT           ;
  NOAPPEND         ;
  NODELETE         ;
  NOWAIT           ;
  SAVE             ;
  NOCLEAR          ;
  WHEN lfWhenBrow()  ;
  VALID :F lfvBrows() ;
  TITLE lcAdjBrwTl

*-browse fields
*B603414,1 NAD 02/01/2000 (Start) - Change the header for DESC field from Description to Adjustment reason
*PRIVATE lcFields
*lcFields =  [cMarker=IIF(RECNO()=lnMarker ,">"," "):H=" ":R:1:W=.F. ,]+;
            [Account  :H="Acc #" :8 ,]+;
            [lcDummy1=LOOKUP(CUSTOMER.BtName,"M"+&lcAdjTemp..Account,CUSTOMER.ACCOUNT,"CUSTOMER") :H="Name":22,]+;
            [DESC :H="Description":23 ,]+;
            [REFERENCE :H="Reference":18 ,]+;
            [cFacCode :H="Factor":9 ,]+;
            [Amount :H="Amount":10 ]
            
            
PRIVATE lcFields
*E500351,1 (Start) Change the header of the description field from "Adjustment Reason" to "discreption".
*lcFields =  [cMarker=IIF(RECNO()=lnMarker ,">"," "):H=" ":R:1:W=.F. ,]+;
            [Account  :H="Acc #" :8 ,]+;
            [lcDummy1=LOOKUP(CUSTOMER.BtName,"M"+&lcAdjTemp..Account,CUSTOMER.ACCOUNT,"CUSTOMER") :H="Name":22,]+;
            [DESC :H="Adjustment Reason":23 ,]+;
            [REFERENCE :H="Reference":18 ,]+;
            [cFacCode :H="Factor":9 ,]+;
            [Amount :H="Amount":10 ]  
            
lcFields =  [cMarker=IIF(RECNO()=lnMarker ,">"," "):H=" ":R:1:W=.F. ,]+;
            [Account  :H="Acc #" :8 ,]+;
            [lcDummy1=LOOKUP(CUSTOMER.BtName,"M"+&lcAdjTemp..Account,CUSTOMER.ACCOUNT,"CUSTOMER") :H="Name":22,]+;
            [DESC :H="Description":23 ,]+;
            [REFERENCE :H="Reference":18 ,]+;
            [cFacCode :H="Factor":9 ,]+;
            [Amount :H="Amount":10 ]                   
            
            
                      
*E500351,1 (End)         
*B603414,1 NAD (End)         

*-- in case of debit adjustment add field in browse fields
IF MCHOICE = "D"
  lcFields = lcFields + [,cChrgBack :H="Debit on Acc.":15 ]
ENDIF

*C101632,1 (Begin) Add the new 2 get fields to the broswe.
IF MCHOICE = 'D'
  lcFields = lcFields + [,TranDate :H="Tran. Date":10,Store :H="Store":8 ]
ENDIF
*C101632,1 (End)

BROWSE FIELDS &lcFields;
  WINDOW  (lcDC_Adj1) ;
  IN WINDOW (gcBaseWind) ;
  NOMENU           ;
  NOEDIT           ;
  NOAPPEND         ;
  NODELETE         ;
  NOWAIT           ;
  SAVE             ;
  NOCLEAR          ;
  WHEN lfWhenBrow()  ;
  VALID :F lfvBrows() ;
  TITLE lcAdjBrwTl
*E301326,1 WAB - END

*!*************************************************************
*! Name      : lfvBrows
*! Developer : Reham Al-Allamy
*! Date      : 07/30/97
*! Purpose   : Valid function for the browse.
*!*************************************************************
*! Calls     : gfStopBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvBrows()
*!*************************************************************
*
FUNCTION lfvBrows

*-- Valid function executed inside the browse. to stop the browse if the
*-- current window not the browse window.

IF !WONTOP(lcAdjBrwTl)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfWhenBrow
*! Developer : Reham Al-Allamy
*! Date      : 07/30/97
*! Purpose   : When function for the browse.
*!*************************************************************
*! Calls     : gfwCodePop, lfRefresh
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfWhenBrow()
*!*************************************************************
*
FUNCTION lfWhenBrow

*-- Refresh the objects in the screen if any movement happened in the browse.
SELECT (lcAdjTemp)
*-- Change the record pointer.
lnMarker   = RECNO()

IF BOF() .OR. EOF()
  *-- Initialize the screen variables.
  lcAccount = ""
  lcAccName = ""
  lcCode    = ""
  lcAdjAcct = ""
  lcBnkCode = ""
  lcChkAcct = ""
  lcRef     = ""
  lcFactor  = ""
  lnTrnAmt  = 0
  *E500351,1 (Start) Clear the description variable 
  lcDesc=""
  *E500351,1 (End)
  *E301326,1 WAB - define variable for chkbox filed (charge back)
  *E301326,1 WAB - in case of debit adjustment
  *E301326,1 WAB - START 
  IF MCHOICE = "D"        
    llChrgBack = .F. 
  ENDIF
  *E301326,1 WAB - END

  *C101632,1 (Begin) Empty store and initialize trandate with '' not to appear on the screen.
  lcStore    = SPACE(8)
  ldTranDate = ''
  *E301369,1 (Start)  Add the due date screen field
  IF MCHOICE = "D" 
    ldDueDate={}
  ENDIF
  *E301369,1 (End)
  
  *C101632,1 (End)
   
  *-- If there is no lines in the browse, display N/A reason value in the popup.
  =gfwCodePop(@laCodInfo, IIF(MCHOICE="D","TRANCODE","CCREDITCOD") , "N")
ELSE
  *-- Fill the objects on the screen with the fields from the current line.
  lcAccount = ACCOUNT
  lcAccName = LOOKUP(CUSTOMER.BtName,"M"+&lcAdjTemp..Account,CUSTOMER.ACCOUNT,"CUSTOMER")
  lcCode    = IIF(MCHOICE="D" , TRANCODE , CCREDITCOD)
  lcAdjAcct = cAdjAcct
  lcBnkCode = cBnkCode
  lcChkAcct = cChkAcct
  lcRef     = REFERENCE
  lcFactor  = cFacCode
  lnTrnAmt  = AMOUNT
  *E500351,1 (Start) fill the description field with data.
  lcDesc    = Desc
  *E500351,1 (End)
  *E301326,1 WAB - Fill the object(charge back) with .T. if the field from the current line.
  *E301326,1 WAB - equal = 'Yes' *- in case of debit adjustment
  *E301326,1 WAB - START 
  IF MCHOICE = "D" 
    llChrgBack = (cChrgBack = 'Yes')
  ENDIF
  *E301326,1 WAB - END

  *C101632,1 (Begin) Get thier value.
  lcStore    = STORE
  ldTranDate = TRANDATE
  *E301369,1 (Start) Add the due date screen field
  IF MCHOICE = "D" 
    ldDueDate=DUEDATE
  ENDIF  
  *E301369,1 (End)
  *C101632,1 (End)
   
  *-- If there is one line in the browse, display its reason value in the popup.
  =gfwCodePop(@laCodInfo, IIF(MCHOICE="D","TRANCODE","CCREDITCOD") , "V,"+lcCode)
ENDIF

*-- Refresh the objects in the main screen.
SHOW GET ibAccount &lcAccStat
SHOW GET lcAccount &lcAccStat
SHOW GET lnReson   &lcRemStat
SHOW GET lcRef     &lcRemStat
SHOW GET ibFactor  &lcRemStat
SHOW GET lcFactor  &lcRemStat
SHOW GET lnTrnAmt  &lcRemStat
SHOW GET pbRem     &lcRemStat
*E500351,1 (Start) Control the status of the description field.
SHOW GET lcDesc    &lcRemStat
*E500351,1 (End)
*E301369,1 (Start) add the due date screen field
IF MCHOICE='D'
  IF  !EMPTY(lcAccount)
    SHOW GET ldDueDate &lcRemStat
  ELSE
    SHOW GET ldDueDate DISABLE
  ENDIF
ENDIF
*E301369,1 (End)
*E301326,1 Remove this line because there is no pbUpdate object.
*SHOW GET pbUpdate  &lcRemStat
*E301326,1 end
SHOW GET pbNew     &lcNewStat
*E301326,1 WAB - Refresh the object(charge back) in case of debit adjustment
*E301326,1 WAB - START 
IF MCHOICE = "D" 
  SHOW GET llChrgBack &lcRemStat
ENDIF
*E301326,1 WAB - END

*C101632,1 (Begin) Show the two fields when charge back.
*B603418,1 NAD (Start)  Show get the fields after calling lfrefresh() 
*IF MCHOICE = "D"  AND llChrgBack        
*  SHOW GET ldTranDate ENABLE
*  SHOW GET lcStore ENABLE
*ENDIF 
*-- Call function in the screen spr to refresh the say fields
*-- in the main screen
=lfRefresh()
*C101632,1 (End)

IF MCHOICE = "D"  AND llChrgBack
  SHOW GET ldTranDate ENABLE
  SHOW GET lcStore ENABLE
ENDIF 
*B603418,1 NAD (End)
SHOW WINDOW (lcAdjBrwTl) REFRESH SAME

*!*************************************************************
*! Name      : lfActMain
*! Developer : Reham Al-Allamy
*! Date      : 07/30/97
*! Purpose   : Activate function for Debit & credit Adj.
*!*************************************************************
*! Calls     : gfStopBrow
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfActMain()
*!*************************************************************
*
FUNCTION lfActMain

*-- If coming from the browse, Call global function to stop the browse.
IF glFromBrow
  = gfStopBrow()
  *-- Set the browse flag to false.
  glFromBrow = .F.
ENDIF

*-- If the screen browse is not active then clear the trapped keys.
IF !INLIST(WONTOP(), lcAdjBrwTl)
  *-- Clear all the trapped keys.
  ON KEY LABEL CTRL+Q
  ON KEY LABEL CTRL+W
  ON KEY LABEL Ctrl+ENTER
  ON KEY LABEL Ctrl+HOME
  ON KEY LABEL Ctrl+END
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
ENDIF

*!*************************************************************
*! Name      : lfDactMain
*! Developer : Reham Al-allamy
*! Date      : 07/30/97
*! Purpose   : Deactivate function for AR500
*!*************************************************************
*! Calls     : lpTab , lpShTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  .f.
*!*************************************************************
*! Example   :  =lfDactMain()
*!*************************************************************
*
FUNCTION lfDactMain

*-- Set the global flag "glFromBrow" to true only the screen browse is active.
glFromBrow = INLIST(WONTOP(), lcAdjBrwTl)

*-- If any of the screen's browses is active then trap the
*-- Tab, ShiftTab, Ctrl+Enter, Ctrl+Home and Ctrl+End keys.
IF glFromBrow
  ON KEY LABEL CTRL+Q     lnDummy = 1
  ON KEY LABEL CTRL+W     lnDummy = 1
  ON KEY LABEL Ctrl+HOME  lnDummy = 1
  ON KEY LABEL Ctrl+END   lnDummy = 1
  ON KEY LABEL Ctrl+ENTER lnDummy = 1
  ON KEY LABEL TAB     DO lpTab
  ON KEY LABEL BACKTAB DO lpShTab
ENDIF

*!*************************************************************
*! Name      : lpTab
*! Developer : Reham Al-Allamy
*! Date      : 02/15/1998
*! Purpose   : Trap of tab key.
*!*************************************************************
*! Calls     : lpTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lpTab
*!*************************************************************
*
PROCEDURE lpTab

*-- Clear the TAB trap.
ON KEY LABEL TAB

DO CASE
  *-- In select mode, Activate object <pbCpTask, task list> in the toolbar.
CASE laScrMode[1]
  ACTIVATE WINDOW gwcContrl1
  _CUROBJ = OBJNUM(pbCpTask)
  *-- In add mode, Activate object <pbNew> in window (lcDC_Adj2).
CASE laScrMode[4]
  ACTIVATE WINDOW (lcDC_Adj2)
  _CUROBJ = OBJNUM(pbNew)
ENDCASE

*-- Trap the TAB key.
ON KEY LABEL TAB DO lpTab

*!*************************************************************
*! Name      : lpShTab
*! Developer : Reham Al-Allamy
*! Date      : 02/15/1998
*! Purpose   : Trap of backtab key.
*!*************************************************************
*! Calls     : lpShTab
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lpShTab
*!*************************************************************
*
PROCEDURE lpShTab

*-- Clear the backtab key.
ON KEY LABEL BACKTAB

DO CASE
  *-- In select mode, Activate object "lnAmount" in window (lcDC_Adj0).
CASE laScrMode[1]
  ACTIVATE WINDOW (lcDC_Adj0)
  _CUROBJ = OBJNUM(lnAmount)
  *-- In add mode, Activate object <CANCEL> in the toolbar.
CASE laScrMode[4]
  ACTIVATE WINDOW gwcContrl1
  _CUROBJ = OBJNUM(pbCls)
ENDCASE

*-- Trap the BACKTAB key.
ON KEY LABEL BACKTAB DO lpShTab

*!*************************************************************
*! Name      : lfvKey
*! Developer : Reham Al-Allamy
*! Date      : 07/30/97
*! Purpose   : Valid function for the batch date.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvKey()
*!*************************************************************
*
FUNCTION lfvKey

*-- If validate the transaction date, the currency code, the exchange rate,
*-- and both lines count & total amount greater than 1.
*B603187,1 Allow tape total < 1
*IF llValidDat .AND. llValidCur .AND. llValidRat .AND. ;
    lnTrnCnt >= 1 .AND. lnAmount >= 1
IF llValidDat .AND. llValidCur .AND. llValidRat .AND. ;
    lnTrnCnt >= 1 .AND. lnAmount > 0
*B603187,1 end    
  *-- Change the mode to add mode & set current object to the new button.
  laScrMode    = .F.
  laScrMode[4] = .T.
  SHOW GETS
  _CUROBJ = OBJNUM(pbNew)
ENDIF

*!*************************************************************
*! Name      : lfvBatchDt
*! Developer : Reham Al-Allamy
*! Date      : 07/30/97
*! Purpose   : Valid function for the batch date.
*!*************************************************************
*! Calls     : lfvKey
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfvBatchDt()
*!*************************************************************
*
FUNCTION lfvBatchDt

IF !(ldOldDate == ldBatchDt) .OR. !llValidDat
  *-- You cannot leave the batch date empty.
  IF EMPTY(ldBatchDt)
    *** Batch date cannot be empty. ***
    *** < Ok > ***
    llValidDat = .F.
    =gfModalGen("INM40072B00000" , "Dialog")
    _CUROBJ = OBJNUM(ldBatchDt)
    RETURN
  ENDIF

  *-- Check the period & the year for the entered date.
  llValidDat = CHECKPRD(ldBatchDt,'lcGLFYear','lcGLPeriod','AJ')
  IF llValidDat
    *-- Call function to change the mode to the add mode if all
    *-- the keys are valid.
    =lfvKey()
  ELSE
    _CUROBJ = OBJNUM(ldBatchDt)
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfOldCVal
*! Developer : Reham Al-allamy
*! Date      : 07/30/97
*! Purpose   : Function to store old value of the current string filed.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfOldCVal()
*!*************************************************************
*
FUNCTION lfOldCVal

*-- Save the old value of the current object.
lcOldValue = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfOldNVal
*! Developer : Reham Al-allamy
*! Date      : 07/30/97
*! Purpose   : Function to store old value of the current numeric filed.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfOldNVal()
*!*************************************************************
*
FUNCTION lfOldNVal

lnOldValue = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvCurrency
*! Developer : Reham Al-Allamy
*! Date      : 07/30/97
*! Purpose   : Valid function for currency fields
*!*************************************************************
*! Calls     : gfCurrBrow, lfvKey
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvCurrency()
*!*************************************************************
*
FUNCTION lfvCurrency

*-- Added condition to prevent user from editing currency
*--if the this RA has lines or return by invioce.
IF (llBrowse .OR. (!(lcCurrCode == lcOldValue)))
  lcCurCode = lcCurrCode
  *B801807,1 Reham On 11/15/98  *** Begin ***
  *B801807,1 Save the old exchange rate.
  lnOldRate = lnExRate
  *B801807,1 Reham On 11/15/98  *** End   ***
  *-- Call the global currency browse.
  IF !gfCurrBrow(@lcCurCode)
    lcCurrCode = lcOldValue
  ELSE
    lcCurrCode = lcCurCode
  ENDIF

  *llValidCur = IIF(!EMPTY(lcCurrCode) , .T. , .F. )
  IF !EMPTY(lcCurrCode)
    llValidCur = .T.
    lnExRate   = gfChkRate('lnCurrUnit' , lcCurrCode , ldBatchDt , .T. , ;
      gcAct_Comp , .F. , .T.)
    llValidRat = .T.
    IF lcCurrCode = gcBaseCurr
      STORE 1 TO lnExRate , lnCurrUnit
      lcRatStat  = "DISABLE"
    ELSE
      IF lnExRate <= 0
        *B801807,1 Reham On 11/15/98  *** Begin ***
        *B801807,1 Restore the old currency & its exchange rate in case of return
        *B801807,1 with zero exchange rate from "gfChkRate".
        *IF llEditExRt
        *  *** The exchange rate must be greater than zero. ***
        *  *** <  Ok  > ***
        *  =gfModalGen("INM40073B00000" , "DIALOG")
        *  llValidRat = .F.
        *ELSE
        *  llValidRat = .F.
        *ENDIF
        *** A valid xxx to xxx exchange rate could not be found on xxx. ***
        *** <  Ok  > ***
        =gfModalGen('INM00262B00000','ALERT',ALLTRIM(lcCurrCode)+'|'+ALLTRIM(gcBaseCurr)+'|'+DTOC(ldBatchDt))
        lcCurrCode = lcOldValue
        lnExRate   = lnOldRate
        *B801807,1 Reham On 11/15/98  *** End   ***
      ENDIF
      lcRatStat  = IIF(laScrMode[1] AND !(lcCurrCode = gcBaseCurr) AND llEditExRt , "ENABLE" , "DISABLE")
    ENDIF
    *-- Get exchange rate sign and unit sign.
    lcUntSin = ' '
    lcExRSin = gfGetExSin(@lcUntSin, lcCurrCode)
  ELSE
    llValidCur = .F.
  ENDIF
ENDIF

SHOW GET lcCurrCode
SHOW GET lnExRate &lcRatStat

llBrowse = .F.

IF llValidCur
  *-- Call function to change the mode to the add mode if all
  *-- the keys are valid.
  =lfvKey()
ENDIF

*!*************************************************************
*! Name      : lfvExRate
*! Developer : Reham Al-Allamy
*! Date      : 07/30/97
*! Purpose   : Valid function for exchange rate fields
*!*************************************************************
*! Calls     : lfvKey
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvExRate()
*!*************************************************************
*
FUNCTION lfvExRate

IF lnExRate <=0
  *** The exchange rate must be greater than zero. ***
  *** <  Ok  > ***
  =gfModalGen("INM40073B00000" , "DIALOG")
  lnExRate = IIF(lnOldValue > 0 , lnOldValue , lnExRate)
ENDIF

llValidRat = (IIF(lnExRate > 0 , .T. , .F.))

IF llValidRat
  *-- Call function to change the mode to the add mode if all
  *-- the keys are valid.
  =lfvKey()
ENDIF

*!*************************************************************
*! Name      : lfvTrnCnt
*! Developer : Reham Al-Allamy
*! Date      : 07/30/97
*! Purpose   : Valid function for transactions count.
*!*************************************************************
*! Calls     : lfvKey
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvTrnCnt()
*!*************************************************************
*
FUNCTION lfvTrnCnt

*-- You cannot leave the transaction count empty.
IF lnTrnCnt <= 0
  *** Count has to be greater than zero. ***
  *** <  Ok  > ***
  =gfModalGen("INM40075B00000" , "DIALOG")
ENDIF

*-- Call function to change the mode to the add mode if all
*-- the keys are valid.
=lfvKey()

*!*************************************************************
*! Name      : lfvAmount
*! Developer : Reham Al-Allamy
*! Date      : 07/30/97
*! Purpose   : Valid function for currency fields
*!*************************************************************
*! Calls     : lfvKey, lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAmount()
*!*************************************************************
*
FUNCTION lfvAmount

*-- You have to enter value in the tape amount.
IF lnAmount <= 0
  *** Total Tape amount has to be greater than zero. ***
  *** < Ok > ***
  =gfModalGen("INM40074B00000" , "Dialog")
ENDIF

*-- Adjust the differenece if change the tape total.
IF lnAmount <> lnOldValue
  lnDiff   = lnAmount - lnActual
  =lfRefresh()
ENDIF

*-- Call function to change the mode to the add mode if all
*-- the keys are valid.
=lfvKey()

*!*************************************************************
*! Name      : lfvAccount
*! Developer : Reham Al-Allamy
*! Date      : 07/30/97
*! Purpose   : Valid function for the account field
*!*************************************************************
*! Calls     : CUSBROWM, lfwBrowTrn, lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAccount()
*!*************************************************************
*
FUNCTION lfvAccount

*-- Save the current record pointer in the temp. file.
lnCurrAct = RECNO(lcAdjTemp)

IF EMPTY(lcAccount)
  llBrowse = .T.
ENDIF

SELECT CUSTOMER
*-- If the account variable was not empty or click the browse icon.
IF llBrowse .OR. !EMPTY(lcAccount)
  llBrowse = IIF("?" $ lcAccount , .T. , llBrowse)
  *-- Seek in the customer file for the entered account.
  IF !SEEK('M' + SUBSTR(lcAccount,1,5)) .OR. llBrowse
    *-- Call the global customer browse.
    DO CUSBROWM WITH lcAccount
  ENDIF
  lcAccount = IIF(EMPTY(lcAccount) , lcOldValue , lcAccount)
  *B803369,1 (Start) Get the default factor for the account
  =SEEK('M' + lcAccount)
  lcFactor=Customer.cFacCode
  SHOW GET lcFactor 
  *B803369,1 (End)
  *B602838,1 NAD 09/24/2000 (Start) Not to allow transaction for hold accounts 
  *B603959,1 ASH 10/11/2000 (Start) Allow Hold account to do Credit/Debit Adjusments.
  *IF Customer.Status <>'A'
  IF !(Customer.Status $ 'HA')
  *B603959,1 ASH 10/11/2000 (End)
    *Message : 40022
    *Non active account . Cannot proceed.
    *Button : 00000
    *Ok
    =gfModalGen('TRM40172B00000','ALERT')
    lcAccount = lcOldValue
    _CUROBJ = _CUROBJ
    llBrowse = .F.
    RETURN  
  ENDIF    
  *B602838,1 NAD 09/24/2000 (End)
  
  *E301369,1 (Start) Get the End of month data from the account term code to calculate the due date
  IF MCHOICE = 'D' AND lcAccount <> lcOldValue AND SEEK('M'+PADR(lcAccount,5),'CUSTOMER') 
  
    STORE SPACE(0) TO lcTEOM
    STORE 0 TO lnTDaysDue ,lnEOMDay
    STORE {} TO ldDueDate
    DECLARE laTrmRltd[3,2]
    laTrmRltd[1,1]='EOM'
    laTrmRltd[1,2]='lcTEOM'
    laTrmRltd[2,1]='NTERDUED'
    laTrmRltd[2,2]='lnTDaysDue'
    laTrmRltd[3,1]='EOMDAY'
    laTrmRltd[3,2]='lnEomDay'  
    =gfRltFld(Customer.cTermCode,@laTrmRltd,'CTERMCODE')
    lcTEOM = ALLTRIM(lcTEOM)
    lnEOMDay = IIF(TYPE('lnEOMDay') <> 'N' .OR. lnEOMDay = 0,20,lnEOMDay-1)
    
    IF UPPER(ALLTRIM(gcContCode))='ENG'
      *-- get the new due date
      ldDueDate = IIF(lcTEOM <> 'Y',ldBatchDt+lnTDaysDue,;
                    CTOD('01'+SUBSTR(DTOC(GOMONTH(ldBatchDt,1)),3))-1+lnTDaysDue)
    ELSE 
      *-- EOM 	End of Month 		m/10/yy
      *-- if  day of invoice > End of Month date 		
      *-- m/10/yy + 2 monthes+ net due date	
      *-- if day of invoice < End of Month date 		
      *-- m/10/yy + 1 monthes+ net due date                
      ldDueDate= IIF(lcTEOM <> 'Y',ldBatchDt + lnTDaysDue,;
                 GOMONTH(CTOD(SUBSTR(DTOC(ldBatchDt),1,3) +'10'+;
                 SUBSTR(DTOC(ldBatchDt),6,5)),IIF(DAY(ldBatchDt)>lnEOMDay,2,1))+lnTDaysDue)
     
    ENDIF  
    SHOW GET ldDueDate ENABLED
  ENDIF
  *E301369,1 (End)
    
  *-- Restore the record pointer in the temp. file to prevent
  *-- the error of changing the pointer.
  SELECT (lcAdjTemp)
  IF lnCurrAct <> 0 .AND. lnCurrAct <= RECCOUNT(lcAdjTemp)
    GOTO lnCurrAct
  ENDIF

  *-- Lock the file to grantee the phiscal update.
  =RLOCK()
  *-- Update the temp file & refresh the browse.
  REPLACE ACCOUNT WITH lcAccount
  *B803369,1 NAD 07/20/2000 (Start) Update the tmp file with the default factor.
  REPLACE cFacCode WITH lcFactor
  *B803369,1 (End)
  *E301369,1 (Start)  Save the due dat to the temp file
  IF MCHOICE='D'
    REPLACE DUEDATE WITH ldDueDate
  ENDIF
  *E301369,1 (End) 
  
  UNLOCK

  *-- Call the browse when function.
  =lfwBrowTrn()

  *-- Restore the record pointer in the temp. file to prevent the error
  *-- of changing the pointer because of the lookup.
  IF lnCurrAct <> 0 .AND. lnCurrAct <= RECCOUNT(lcAdjTemp)
    GOTO lnCurrAct
  ENDIF

  SHOW WINDOW (lcAdjBrwTl) REFRESH

  *-- Get the account name.
  *B603329,1 SSH 26/12/1999 (Start) Correct the customer file pointer.
  =IIF(EMPTY(lcAccount),.T.,SEEK('M'+lcAccount,'CUSTOMER'))
  *B603329,1 SSH 26/12/1999 (End)
  lcAccName = IIF(!EMPTY(lcAccount) , CUSTOMER.BTNAME , " ")

  *-- Call function in the screen spr to refresh the say fields
  *-- in the main screen
  =lfRefresh()
ENDIF

SELECT (lcAdjTemp)

*-- Set the browse flag to false.
llBrowse = .F.

*!*************************************************************
*! Name      : lfvReson
*! Developer : Reham Al-Allamy
*! Date      : 07/31/1997
*! Purpose   : Valid function of the adjustment reason popup.
*!*************************************************************
*! Calls     : gfRltFld
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvReson()
*!*************************************************************
*
FUNCTION lfvReson

*-- Validate the header reason.
lcCode = laReson[lnReson , 2]

*-- Get the related fields to the adjustment code selected.
IF !EMPTY(lcCode)
  
  DECLARE laAdjRltFd[3,2]
  laAdjRltFd[1,1] = 'CADJACCT'
  laAdjRltFd[1,2] = 'lcAdjAcct'
  laAdjRltFd[2,1] = 'CBNKCODE'
  laAdjRltFd[2,2] = 'lcBnkCode'
  laAdjRltFd[3,1] = 'CCHKACCT'
  laAdjRltFd[3,2] = 'lcChkAcct'
 
  =gfRltFld(lcCode , @laAdjRltFd , IIF(MCHOICE="D","TRANCODE","CCREDITCOD"))

ELSE
  lcAdjAcct = ''
  lcBnkCode = ''
  lcChkAcct = ''
ENDIF

*-- Add the transaction code & its description & the link code to
*-- the current record & refresh the browse.
SELECT (lcAdjTemp)
*-- Lock the file to grantee the phiscal update.
=RLOCK()

IF MCHOICE="D"
  *-- If debit adjustment.
  *E500351,1 (Start) Not to update the description field with adjustment reason code
  *REPLACE TRANCODE  WITH lcCode ;
    DESC      WITH gfCodDes(lcCode , "TRANCODE") ;
    cAdjAcct  WITH lcAdjAcct ;
    cBnkCode  WITH lcBnkCode ;
    cChkAcct  WITH lcChkAcct
    
  REPLACE TRANCODE  WITH lcCode    ;
          cAdjAcct  WITH lcAdjAcct ;
          cBnkCode  WITH lcBnkCode ;
          cChkAcct  WITH lcChkAcct    
  *E500351,1 (End)
  *B802622,1 ABD Add the decscription of the reason code to the decscription filed by default
  *B802622,1 ABD When the user select the reason decscription. [Begin]
  REPLACE DESC WITH gfCodDes(lcCode , "TRANCODE")
  lcDesc = DESC
  SHOW GET lcDesc
  *B802622,1 ABD [End]

  
ELSE
  *-- If credit adjustment.
  *E500351,1 (Start) Not to update the description field with adjustment reason code
  *REPLACE CCREDITCOD WITH lcCode ;
    DESC       WITH gfCodDes(lcCode , "CCREDITCOD") ;
    cAdjAcct   WITH lcAdjAcct ;
    cBnkCode  WITH lcBnkCode ;
    cChkAcct  WITH lcChkAcct
 

  REPLACE CCREDITCOD WITH lcCode    ;
          cAdjAcct   WITH lcAdjAcct ;
          cBnkCode   WITH lcBnkCode ;
          cChkAcct   WITH lcChkAcct
  *E500351,1 (End)
  *B802622,1 ABD Add the decscription of the reason code to the decscription filed by default
  *B802622,1 ABD When the user select the reason decscription. [Begin]
  REPLACE DESC WITH gfCodDes(lcCode , "CCREDITCOD") 
  lcDesc = DESC
  SHOW GET lcDesc
  *B802622,1 ABD [End]
ENDIF
UNLOCK

*-- Refresh the main browse with the selected adj. code.
SHOW WINDOW (lcAdjBrwTl) REFRESH


*!*************************************************************
*! Name      : lfvRef
*! Developer : Reham Al-Allamy
*! Date      : 07/30/97
*! Purpose   : Valid function for the reference field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRef()
*!*************************************************************
*
FUNCTION lfvRef

*-- Add the reference to the current record & refresh the browse.
SELECT (lcAdjTemp)
*-- Lock the file to grantee the phiscal update.
=RLOCK()
REPLACE REFERENCE WITH lcRef
UNLOCK
SHOW WINDOW (lcAdjBrwTl) REFRESH

*!*************************************************************
*! Name      : lfvFactor
*! Developer : Reham Al-Allamy
*! Date      : 07/30/97
*! Purpose   : Valid function for the factor field
*!*************************************************************
*! Calls     : FacChk
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvFactor()
*!*************************************************************
*
FUNCTION lfvFactor

*E301077,14 Reham On 12/29/98   *** Begin ***
*E301077,14 Open the factor file.
=gfOpenFile(gcSysHome+'SycFact',gcSysHome+'cFacCode','SH')
*E301077,14 Reham On 12/29/98   *** End   ***

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
  lcFactor   = IIF(lcFactor = laFactors[1] , SPACE(6) , laFactors[1])
ENDIF
SHOW GET lcFactor

*-- Set the browse flag to false.
llBrowse = .F.

*-- Add the factor to the current record & refresh the browse.
SELECT (lcAdjTemp)
*-- Lock the file to grantee the phiscal update.
=RLOCK()
REPLACE cFacCode WITH lcFactor
UNLOCK
SHOW WINDOW (lcAdjBrwTl) REFRESH

*!*************************************************************
*! Name      : lfvTrnAmt
*! Developer : Reham Al-Allamy
*! Date      : 07/30/97
*! Purpose   : Valid function for the amount field
*!*************************************************************
*! Calls     : lfRefresh, gfRltFld
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvTrnAmt()
*!*************************************************************
*
FUNCTION lfvTrnAmt
  *E301420,1 (Start) To prevenet bug numeric over flow
  lnMaxNum= IIF(MCHOICE='D',99999999999.00,9999999999.00) 
  IF lnTrnAmt >lnMaxNum 
    * Message XXXXX cannot exceed 9999999999
    =gfModalGen("TRM40171BOOOO" , "DIALOG", 'Amount' +'|'+ ALLTRIM(STR(lnMaxNum,11))) 
    lnTrnAmt =  lnOldValue  
    RETURN
  ENDIF 
 *E301420,1 (End)
IF lnTrnAmt <> lnOldValue   
     
  lnTrnAmt   = IIF(lnTrnAmt <> lnOldValue .AND. lnTrnAmt = 0 , lnOldValue , lnTrnAmt)
  SHOW GET lnTrnAmt
  lnOldValue = ABS(lnOldValue)
  lnTrnAmt   = ABS(lnTrnAmt)

  *-- Calculate the actual amount.
  lnActual   = lnActual + lnTrnAmt - lnOldValue

  *-- Calculate the different between entered tape total & the actual amount.
  lnDiff     = lnAmount - lnActual

  *-- Update the needed variabnles.
  llNoThing  = lfUpdVars()

  *-- Call function in the screen spr to refresh the say fields
  *-- in the main screen
  =lfRefresh()

  lnOldValue = IIF(MCHOICE = "D" , ABS(lnOldValue) , -1*(ABS(lnOldValue)))
  lnTrnAmt   = IIF(MCHOICE = "D" , ABS(lnTrnAmt) , -1*(ABS(lnTrnAmt)))

  *-- Add the amount to the current record & refresh the browse.
  SELECT (lcAdjTemp)
  *-- Lock the file to grantee the phiscal update.
  =RLOCK()
  REPLACE AMOUNT WITH lnTrnAmt
  UNLOCK
  SHOW WINDOW (lcAdjBrwTl) REFRESH
ENDIF

*-- If the entered amount was greater than 0 & it's credit adjustment
*-- & there is availability to add salesrep chargebacks.
IF lnTrnAmt <> 0 .AND. llRepCB
  *-- Variables if credit & chargeback salesrep.
  STORE " " TO lcInvoice , lcRepCode1 , lcRepName1 , lcRepCode2 , ;
    lcRepName2 , lcOrder , lcStore , lcCustPo , lcTerms
  STORE  0  TO lnCharge0 , lnComm1 , lnCharge1 , lnComm2 , lnCharge2 , lnDiscount
  STORE .F. TO llEditCom1 , llEditCom2 , llEditRep
  *-- This variable to keep track of the inserted records.
  lcTran = ALLTRIM('T' + ALLTRIM(STR(RECNO(lcAdjTemp),5)))
  *-- Search for any chargebacks related to the current adjustment.
  SELECT (lcRepCBTmp)
  IF SEEK(lcTran , lcRepCBTmp)
    *-- Get the first salesrep information.
    lcInvoice  = ALLTRIM(RIGHT(&lcRepCBTmp..DESC,6))
    lcOrder    = &lcRepCBTmp..ORDER
    lcStore    = &lcRepCBTmp..STORE
    lcCustPo   = &lcRepCBTmp..CUSTPO
    lcRepCode1 = &lcRepCBTmp..REPCODE
    lcRepName1 = LOOKUP(SALESREP.Name,lcRepCode1,SALESREP.REPCODE,"SALESREP")
    lnComm1    = &lcRepCBTmp..COMMPCNT
    lnCharge1  = &lcRepCBTmp..AMOUNT
    lnCharge0  = &lcRepCBTmp..nOrg_Amnt
    *-- Skip to see if the following record related to the current adjustment.
    SKIP
    IF ALLTRIM(TRAN) == lcTran
      *-- Get the second salesrep information.
      lcRepCode2 = &lcRepCBTmp..REPCODE
      lcRepName2 = LOOKUP(SALESREP.Name,lcRepCode2,SALESREP.REPCODE,"SALESREP")
      lnComm2    = &lcRepCBTmp..COMMPCNT
      lnCharge2  = &lcRepCBTmp..AMOUNT
    ENDIF
    SKIP -1
    llEditCom1 = IIF(!EMPTY(lcRepCode1) , .T. , .F. )
    llEditCom2 = IIF(!EMPTY(lcRepCode2) , .T. , .F. )

    *-- Variables if credit & chargeback salesrep.
    lcTerms    = ""
    lnDiscount = 0
    llEditRep  = .F.
    IF !EMPTY(lcInvoice)
      SELECT INVHDR
      IF SEEK(lcAccount + lcInvoice)
        lcTerms    = INVHDR.cTermCode
        lnDiscount = INVHDR.trde_disc
      ENDIF
    ENDIF

    IF EMPTY(lcTerms)
      IF SEEK("M"+lcAccount , "CUSTOMER")
        lcTerms = CUSTOMER.cTermCode
      ENDIF
      *-- Get Trade Discount If there is Any.
      IF !EMPTY(lcTerms)
        *-- Get the trade discount for the current term.
        DECLARE laTrmRltFd[1,2]
        laTrmRltFd[1,1] = 'NTERDISCR'
        laTrmRltFd[1,2] = 'lnDiscount'
        =gfRltFld(lcTerms , @laTrmRltFd , "CTERMCODE")
      ELSE
        lnDiscount = 0
      ENDIF
    ENDIF

    *-- Calculate the chargebacks according to the new amount.
    lnCharge0 = IIF(!EMPTY(lnDiscount) , lnTrnAmt - (lnTrnAmt * (lnDiscount/100)), lnTrnAmt)
    SELECT (lcRepCBTmp)
    *-- Update first sales rep. info.
    IF !EMPTY(lcRepCode1) .AND. SEEK(lcTran + lcRepCode1 , lcRepCBTmp)
      lnCharge1 = IIF(!EMPTY(lcRepCode1) , ABS(ROUND(lnCharge0 * (lnComm1/100),2)) * -1 , 0 )
      *-- Lock the file to grantee the phiscal update.
      =RLOCK()
      REPLACE AMOUNT    WITH lnCharge1 ;
        nOrg_Amnt WITH -1 * ABS(lnCharge0)
      UNLOCK
    ENDIF
    *-- Update second sales rep. info.
    IF !EMPTY(lcRepCode2) .AND. SEEK(lcTran + lcRepCode2 , lcRepCBTmp)
      lnCharge2 = IIF(!EMPTY(lcRepCode2) , ABS(ROUND(lnCharge0 * (lnComm2/100),2)) * -1 , 0 )
      *-- Lock the file to grantee the phiscal update.
      =RLOCK()
      REPLACE AMOUNT    WITH lnCharge2 ;
        nOrg_Amnt WITH -1 * ABS(lnCharge0)
      UNLOCK
    ENDIF
  ENDIF
  
  SELECT INVHDR
  
  *B607459,1 KHM 08/05/2003 (Begin) Adding the mdown() to valid when leaving the field using 
  *B607459,1                the mouse.
  *IF LASTKEY() = 13
  IF LASTKEY() = 13 OR MDOWN()
  *B607459,1 KHM 08/05/2003 (End)
  
    PUSH KEY
    ON KEY
    *-- Call the chargebacks screen.
    *DO (gcScrDir+gcWinAppl+"\ARCHRBK.SPX")
    *B604340,1 AME [Start] 
    IF SEEK('M'+lcAccount  , "CUSTOMER") 
      lcRepCode1 = CUSTOMER.SALESREP
      lcRepCode2 = CUSTOMER.REP2
    ENDIF
    IF !EMPTY(lcRepCode1) .OR. !EMPTY(lcRepCode2)
      *-- Get the sales rep. names.
      lcRepName1 = LOOKUP(SALESREP.Name,lcRepCode1,SALESREP.REPCODE,"SALESREP")
      lcRepName2 = LOOKUP(SALESREP.Name,lcRepCode2,SALESREP.REPCODE,"SALESREP")
      *B606646,1 RAE Sales rep comm doesn't appear by defalut in sales rep charge back screen.[start]
      lnComm1    = CUSTOMER.COMM
      lnComm2    = CUSTOMER.COMM2
      lnCharge0 = IIF(!EMPTY(lnDiscount) , lnTrnAmt - (lnTrnAmt * (lnDiscount/100)), lnTrnAmt)
      lnCharge1 = IIF(!EMPTY(lcRepCode1) , ABS(ROUND(lnCharge0 * (lnComm1/100),2)) * -1 , 0 )
	  lnCharge2 = IIF(!EMPTY(lcRepCode2) , ABS(ROUND(lnCharge0 * (lnComm2/100),2)) * -1 , 0 )
      *B606646,1 RAE [end]
    ENDIF
    *B604340,1 AME [End]
    DO (gcScrDir+"ARCHRBK.SPX")
    POP KEY
  ENDIF
ENDIF

*C101632,1 (Begin) Call the custom notepad screen for KRAZY KAT.
IF ASCAN(laEvntTrig,PADR("NOTES",10)) <> 0
  = gfDoTriger('ARDBADJ','NOTES')
ENDIF  
*C101632,1 (End)
SELECT (lcAdjTemp)

*!*************************************************************
*! Name      : lfvNewTrn
*! Developer : Reham Al-Allamy
*! Date      : 07/30/97
*! Purpose   : Valid function for the new button
*!*************************************************************
*! Calls     : gfwCodePop, gfRltFld, lfRefresh
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvNewTrn()
*!*************************************************************
*
FUNCTION lfvNewTrn

*-- If there is empty account, use it , if not , enter a new record.
SELECT (lcAdjTemp)
LOCATE FOR EMPTY(ACCOUNT)
IF !FOUND()
  
  *-- If called from the key off program, use the account that
  *-- sent as a parameter as a default account.
  
  *E301802,1 Leave the same account if auto carry account option is on. [Begin]
  *lcAccount = IIF(!EMPTY(lcMainAcct) , lcMainAcct , " ")
  IF MCHOICE="D"
    lcAccount = IIF(!EMPTY(lcMainAcct) , lcMainAcct , IIF(llAutoAcct,IIF(EMPTY(lcAccount)," ",lcAccount)," "))
  ELSE
    lcAccount = IIF(!EMPTY(lcMainAcct) , lcMainAcct , " ")
  ENDIF  
  *E301802,1 Leave the same account if auto carry account option is on. [End]
  
  lcAccName = LOOKUP(CUSTOMER.BtName,"M"+lcAccount,CUSTOMER.ACCOUNT,"CUSTOMER")

  *-- Initialize the needed variables
  STORE "" TO lcRef , lcFactor
  *E500351,1 (Start) Clear the variable with any new transaction.
  STORE "" TO lcDesc
  *E500351,1 (End)
  lnTrnAmt = 0
  *E301369,1 (Start)  Empty the due date field
  IF MCHOICE='D'
    ldDueDate={}
  ENDIf  
  *E301369,1 (End) 
  
  *-- Fill the debit or credit adj. popup with the right code.
  =gfwCodePop(@laCodInfo, IIF(MCHOICE="D","TRANCODE","CCREDITCOD") , "D")
  lcCode = laReson[lnReson , 2]

  DECLARE laAdjRltFd[3,2]

  laAdjRltFd[1,1] = 'CADJACCT'
  laAdjRltFd[1,2] = 'lcAdjAcct'
  laAdjRltFd[2,1] = 'CBNKCODE'
  laAdjRltFd[2,2] = 'lcBnkCode'
  laAdjRltFd[3,1] = 'CCHKACCT'
  laAdjRltFd[3,2] = 'lcChkAcct'

  =gfRltFld(lcCode , @laAdjRltFd , IIF(MCHOICE="D","TRANCODE","CCREDITCOD"))

  *B802622,1 ABD Add the decscription of the reason code to the decscription filed by default
  *B802622,1 ABD When the user select the reason decscription. [Begin]
  lcDesc = gfCodDes(lcCode ,IIF(MCHOICE="D","TRANCODE","CCREDITCOD"))
  SHOW GET lcDesc
  *B802622,1 ABD [End]
  
  *-- Insert new record in the file.
  *E301326,1 WAB - insert "No" to field 'Debit on acc.' by default
  *E301326,1 WAB - in case of debit adjustment
  *E301326,1 WAB - START
  *INSERT INTO &gcWorkDir.&lcAdjTemp ;
    (account , trantype ) ;
    VALUES (lcAccount , IIF(MCHOICE="D","2","5"))

  IF MCHOICE = "D"

    *B802622,1 ABD Add the decscription of the reason code to the decscription filed by default
    *B802622,1 ABD When the user select the reason decscription. [Begin]
    *INSERT INTO &gcWorkDir.&lcAdjTemp ;
    *(account , trantype , cChrgBack ) ;
    *VALUES (lcAccount , "2" , "No")
    INSERT INTO &gcWorkDir.&lcAdjTemp ;
           (account , trantype , cChrgBack,DESC ) ;
    VALUES (lcAccount , "2" , "No",lcDesc)
    *B802622,1 ABD [End]
  ELSE
    *B802622,1 ABD Add the decscription of the reason code to the decscription filed by default
    *B802622,1 ABD When the user select the reason decscription. [Begin]
    *INSERT INTO &gcWorkDir.&lcAdjTemp ;
    *  (account , trantype ) ;
    *  VALUES (lcAccount , "5" )
      
    INSERT INTO &gcWorkDir.&lcAdjTemp ;
             (account , trantype,DESC ) ;
      VALUES (lcAccount , "5" , lcDesc)
    *B802622,1 ABD [End]
  ENDIF
  *E301326,1 WAB - END

  *-- Lock the file to grantee the phiscal update.
  =RLOCK()
  *-- If entering a debit adjustment, save the due date field.
  IF MCHOICE = "D"
   *B603418,1 NAD (Start) Replace Due Date And Tran Date with Batch Date                      
   *REPLACE DUEDATE    WITH gdSysDate ;
      TRANCODE  WITH lcCode    ;
      DESC      WITH gfCodDes(lcCode , "TRANCODE") ;
      cAdjAcct  WITH lcAdjAcct ;
      cBnkCode  WITH lcBnkCode ;
      cChkAcct  WITH lcChkAcct
      
    *E301369,1 (Start)  Save the DueDate Feild
    *REPLACE DUEDATE    WITH ldBatchDt ;
      TRANDATE  WITH ldBatchDt ;
      TRANCODE  WITH lcCode    ;
      DESC      WITH gfCodDes(lcCode , "TRANCODE") ;
      cAdjAcct  WITH lcAdjAcct ;
      cBnkCode  WITH lcBnkCode ;
      cChkAcct  WITH lcChkAcct
      *E500351,1 (Start) Not to update the description field with adjustment reason code
      *E500351,1         and update it with the description field.
      *REPLACE DUEDATE    WITH ldDueDate ;
        TRANDATE  WITH ldBatchDt ;
        TRANCODE  WITH lcCode    ;
        DESC      WITH gfCodDes(lcCode , "TRANCODE") ;
        cAdjAcct  WITH lcAdjAcct ;
        cBnkCode  WITH lcBnkCode ;
        cChkAcct  WITH lcChkAcct
      
      
      REPLACE DUEDATE    WITH ldDueDate ;
              TRANDATE  WITH ldBatchDt ;
              TRANCODE  WITH lcCode    ;
              DESC      WITH lcDesc    ;
              cAdjAcct  WITH lcAdjAcct ;
              cBnkCode  WITH lcBnkCode ;
              cChkAcct  WITH lcChkAcct
     
     *E500351,1 (End)
      
    *E301369,1 (End) 
    *B603418,1 NAD (End)  
  ELSE
    *B603418,1 NAD (Start) Replace Due Date And Tran Date with Batch Date                      
    *REPLACE cCreditCod WITH lcCode    ;
      DESC      WITH gfCodDes(lcCode , "CCREDITCOD") ;
      cAdjAcct  WITH lcAdjAcct ;
      cBnkCode  WITH lcBnkCode ;
      cChkAcct  WITH lcChkAcct
     *E500351,1 (Start) Not to update the description field with adjustment reason code
     *E500351,1         and update it with the description field.
     *REPLACE cCreditCod WITH lcCode    ;
        TRANDATE  WITH ldBatchDt;
        DESC      WITH gfCodDes(lcCode , "CCREDITCOD") ;
        cAdjAcct  WITH lcAdjAcct ;
        cBnkCode  WITH lcBnkCode ;
        cChkAcct  WITH lcChkAcct  
     
     REPLACE cCreditCod WITH lcCode      ;
         TRANDATE  WITH ldBatchDt        ;
         DESC      WITH lcDesc           ;
         cAdjAcct  WITH lcAdjAcct        ;
         cBnkCode  WITH lcBnkCode        ; 
         cChkAcct  WITH lcChkAcct       
     *E500351,1 (End)  
         
    *B603418,1 NAD (End) 
  ENDIF
  UNLOCK

  *-- Add 1 to the no. of the accounts.
  lnAdjCnt = lnAdjCnt + 1
  *-- Update the needed variabnles.
  llNoThing = lfUpdVars()

  *-- Variable hold the status of the remove button.
  lcRemStat  = IIF(laScrMode[4] .AND. lnAdjCnt > 0 , "ENABLE" , "DISABLE")
  *-- Variable hold the status of the new button.
  lcAccStat  = IIF(EMPTY(lcMainAcct) , IIF(laScrMode[4] .AND. lnAdjCnt > 0 , "ENABLE" , "DISABLE" ) , "DISABLE" )

  *-- Refresh the screen objects.
  SHOW GET ibAccount &lcAccStat
  SHOW GET lcAccount &lcAccStat
  SHOW GET lnReson   &lcRemStat
  SHOW GET lcRef     &lcRemStat
  SHOW GET ibFactor  &lcRemStat
  SHOW GET lcFactor  &lcRemStat
  SHOW GET lnTrnAmt  &lcRemStat
  SHOW GET pbRem     &lcRemStat
  *E500351,1 (Start) Control the status of the field.
  SHOW GET lcDesc    &lcRemStat
  *E500351,1 (End) 
  
  *E301369,1 (Start) disabale the due date and make eanbled in the valid function of the account
  SHOW GET ldDueDate DISABLE
  *E301369,1 (End)
  *E301326,1 Remove this line because there is no pbUpdate object.
  *SHOW GET pbUpdate  &lcRemStat
  *E301326,1 end

  *E301326,1 WAB - Refresh the object(charge back) in case of debit adjustment
  *E301326,1 WAB - START 
  IF MCHOICE = "D" 
    llChrgBack = .F.
    SHOW GET llChrgBack &lcRemStat
  ENDIF
  *E301326,1 WAB - END
  *-- Call function in the screen spr to refresh the say fields
  *-- in the main screen
  =lfRefresh()
  
  *E301802,1 Call the browse function. [Begin]
  IF llAutoAcct AND MCHOICE="D"
    =lfwBrowTrn()
  ENDIF  
  *E301802,1 Call the browse function. [End]
  
ENDIF

*-- If called from the menu, point to the account field to enter it,
*-- If called from the keyoff program, point to the code field.
_CUROBJ = IIF(EMPTY(lcAccount) , OBJNUM(lcAccount) , OBJNUM(lnReson))

*!*************************************************************
*! Name      : lfvRemTrn
*! Developer : Reham Al-Allamy
*! Date      : 07/30/97
*! Purpose   : Valid function for the remove button
*!*************************************************************
*! Calls     : lfWhenBrow
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRemTrn()
*!*************************************************************
*
FUNCTION lfvRemTrn

*** Are you sure you want to remove this transaction? ***
*** < Yes > - < No > ***
IF gfModalGen("QRM40077B00006" , "DIALOG") = 1
  SELECT (lcAdjTemp)
  lnActual  = lnActual - ABS(&lcAdjTemp..AMOUNT)
  lnDiff    = lnAmount - lnActual
  lcTran    = ALLTRIM('T' + ALLTRIM(STR(RECNO(lcAdjTemp),5)))
  DELETE
  lnAdjCnt  = lnAdjCnt - 1
  *-- Update the needed variabnles.
  llNoThing = lfUpdVars()
  *-- Variable hold the status of the remove button.
  lcRemStat = IIF(laScrMode[4] .AND. lnAdjCnt > 0 , "ENABLE" , "DISABLE")
  *-- Variable hold the status of the new button.
  lcAccStat = IIF(EMPTY(lcMainAcct) , IIF(laScrMode[4] .AND. lnAdjCnt > 0 , "ENABLE" , "DISABLE" ) , "DISABLE" )

  GO TOP
  =lfWhenBrow()

  *-- Delete the related salesreps chargebacks.
  IF llRepCB
    SELECT (lcRepCBTmp)
    IF SEEK(lcTran)
      SCAN WHILE TRAN = lcTran
        DELETE
      ENDSCAN
    ENDIF
    SELECT (lcAdjTemp)
  ENDIF
ENDIF
*B603612,1 (Start) Commented out
*C101632,1 (Begin)
*lcStore    = SPACE(8)
*ldTranDate = ''
*IF llChrgBack        
*  SHOW GET ldTranDate ENABLE
*  SHOW GET lcStore ENABLE
*ENDIF  
*C101632,1 (End)
*B603612,1 (End)
*!*************************************************************
*! Name      : lpClsScr
*! Developer : Reham Al-Allamy
*! Date      : 07/31/1997
*! Purpose   : Cancel new Adjustments.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpClsScr()
*!*************************************************************
*
FUNCTION lpClsScr

*-- Flag to know if enter the add mode before or not.
llAddMode  = .F.

*-- Save status "I" for the ignored record in the uncompleted session file.
SELECT unCmSess
IF lnUnCmSeRc <> 0 .AND. lnUnCmSeRc <= RECCOUNT()
  GOTO lnUnCmSeRc
  REPLACE STATUS WITH 'I'
  UNLOCK
ENDIF

lnUnCmSeRc = 0

*!*************************************************************
*! Name      : gfCPSave
*! Developer : Reham Al-Allamy
*! Date      : 07/31/97
*! Purpose   : Save valid function to update all the transactions
*!*************************************************************
*! Calls     : gfTherm, GLDIST, gfOpenFile
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =gfCPSave()
*!*************************************************************
*
FUNCTION gfCPSave

*E301521,1 (AHM) Extract the saving function in individulal program (Start)
DO lfSavDCAdj IN (gcapphome+'ARDCAJSV.PRG')
RETURN
*E301521,1 (End)

*E301521,1 (AHM) All the coming code will not be excuted as a result of the above return statment
SELECT (lcAdjTemp)
LOCATE FOR EMPTY(Account) .OR. IIF(MCHOICE = "D" , EMPTY(TranCode) , EMPTY(cCreditCod))
IF FOUND()
  GO TOP
  *** There is one or more code(s) is missing.  Cannot update! ***
  *** < Ok > ***
  =gfModalGen("INM40078B00000" , "DIALOG")
  _CUROBJ = OBJNUM(lcAccount)
  RETURN
ENDIF

STORE 0 TO lnCount , lnZero

*-- Get the transaction no.
COUNT FOR !DELETED() .AND. &lcAdjTemp..AMOUNT <> 0 TO lnCount

*-- Get the transaction amount.
COUNT FOR &lcAdjTemp..AMOUNT = 0 TO lnZero

IF lnZero >= 1
  *** One or more transaction(s) has zero amount. ***
  *** Transaction(s) with zero amount will be ignored. ***
  *** < OK > ***
  =gfModalGen("INM40071B00000" , "DIALOG")
ENDIF

IF lnCount = 0
  *** There is no transactions entered. ***
  *** < OK > ***
  =gfModalGen("INM40083B00000" , "DIALOG")
  _CUROBJ = OBJNUM(PBNEW)
  RETURN
ENDIF

*-- If the budget amount not equal the actual amount.
IF ((lnActual <> lnAmount) .OR. (lnTrnCnt <> lnCount) .OR. lnTrnCnt=0 .OR. lnAmount=0)
  DO CASE
  CASE lnActual <> lnAmount
    *** Actual amount not equal Tape amount. ***
    *** < Modify> - < Continue > ***
    lnOption = gfModalGen("QRM40079B40009" , "DIALOG")
  CASE lnTrnCnt <> lnCount
    lcCount = "(" + ALLTRIM(STR(lnCount)) + ")"
    *** Actual count "+ lcCount +" not equal Tape count. ***
    *** < Modify> - < Continue > ***
    lnOption = gfModalGen("QRM40080B40009" , "DIALOG" , lcCount)
  ENDCASE
  IF lnOption = 1
    _CUROBJ = OBJNUM(PBNEW)
    RETURN
  ENDIF
ENDIF

IF EMPTY(lcBatchNo)
  *-- Get the batch no.
  lcBatchNo = gfsequence('BATCH')
ENDIF

*-- Save the save push button in the session processes.
SELECT UNCMSESS
IF lnUnCmSeRc <> 0 .AND. lnUnCmSeRc <= RECCOUNT()
  GOTO lnUnCmSeRc
  REPLACE cCurrObj WITH 'pbSav'
  UNLOCK
  =RLOCK()
ENDIF

*-- Update the needed variabnles.
llNoThing = lfUpdVars()

*** Batch No.  : "+lcBatchNo + CHR(13) + "Batch Date : "+DTOC(ldBatchDt) ***
*** < Ok > ***
lcTmpStr = lcBatchNo + "|" + DTOC(ldBatchDt)
=gfModalGen("INM40081B00000" , "DIALOG" , lcTmpStr)

*-- Flag to know if enter the add mode before or not.
llAddMode  = .F.

*-- Check if there is salesreps' charge backs or not.
llUpdRep = .F.
IF llRepCB
  SELECT (lcRepCBTmp)
  DELETE ALL FOR AMOUNT = 0
  GOTO TOP
  llUpdRep = IIF(EOF() , .F. , .T.)
ENDIF

*-- update the master credit or debit file with the entered transaction.
lnCurRec  = 1
lnTotRec  = lnCount
SELECT (lcAdjTemp)
SET ORDER TO
GO TOP
SCAN FOR !DELETED() .AND. AMOUNT <> 0
  SELECT (lcAdjTemp)
  *-- Call the global function that execute the thermometer.
  =gfTherm (lnTotRec,lnCurRec,"Updating the " + lcMode + "File.")
  lnCurRec = lnCurRec + 1
  IF &lcAdjTemp..nSteps < 1
    *-- Call the global sequence function to get transaction no.
    *lcTranNo = IIF(MCHOICE = "D" , gfsequence('DEBIT') , gfsequence('CREDIT'))
    lcToSnd  = IIF(MCHOICE = "D" , 'DEBIT' , 'CREDIT')

    *C101632,1 (Begin) Get the sequence no. from CRMEMO in case of "credit" only for
    *C101632,1         the custom program ARKRADC.PRG for 'KRAZI KAT'. Refer to '100820' in 26.
    IF (ASCAN(laEvntTrig,PADR("SEQNO",10)) <> 0)
      = gfDoTriger('ARDBADJ','SEQNO')
    ENDIF  

    *C101632,1 (End)
    lcTranNo = gfsequence(lcToSnd, gcAct_Comp, "", "", "TRAN")
    *-- Update the temp. file with the current info.
    SELECT (lcAdjTemp)
    *-- Lock the file to grantee the phiscal update.
    =RLOCK()
    
    *C101632,1 (Begin)  Replace TRANDATE   WITH ldTranDate in case of debit
    *REPLACE TRAN      WITH lcTranNo ;
      TRANDATE  WITH ldBatchDt ;
      dPostDate WITH ldBatchDt ;
      BATCH     WITH lcBatchNo ;
      cCurrCode WITH lcCurrCode ;
      nExRate   WITH lnExRate ;
      nCurrUnit WITH lnCurrUnit

    *B603418,1 NAD (Start) Not to Replace the tran date and save it as it is in the temp file
    *REPLACE TRAN WITH lcTranNo ;
      TRANDATE   WITH IIF(MCHOICE = 'D' AND llChrgBack ,ldTranDate,ldBatchDt);
      dPostDate  WITH ldBatchDt ;
      BATCH      WITH lcBatchNo ;
      cCurrCode  WITH lcCurrCode ;
      nExRate    WITH lnExRate ;
      nCurrUnit  WITH lnCurrUnit
      
    REPLACE TRAN WITH lcTranNo ;
      dPostDate  WITH ldBatchDt ;
      BATCH      WITH lcBatchNo ;
      cCurrCode  WITH lcCurrCode ;
      nExRate    WITH lnExRate ;
      nCurrUnit  WITH lnCurrUnit  
    *B603418,1 NAD (End)
    *C101632,1 (End)
    
    UNLOCK
    SCATTER MEMVAR MEMO
         
    *C101632,1 (Begin) In case of charge back for Krazi Kat update the charge back date with batch date AND UPDTE NOTES.
    IF (ASCAN(laEvntTrig,PADR("CHRGDATE",10)) <> 0)
      = gfDoTriger('ARDBADJ','CHRGDATE')
    ENDIF  
    *C101632,1 (End)

    *-- Update the master file.
    SELECT (lcCurAlis)
    APPEND BLANK
    *-- Lock the file to grantee the phiscal update.
    =RLOCK()
    GATHER MEMVAR MEMO    
    *-- Call global function to add audit fields info.
    =gfAdd_Info(lcCurAlis)
    UNLOCK
    SELECT (lcAdjTemp)
    *-- Lock the file to grantee the phiscal update.
    =RLOCK()
    REPLACE nSteps WITH 1
    UNLOCK
  ENDIF

  IF &lcAdjTemp..nSteps < 2
    *-- Update the customer file with : open credit, current, total age
    *-- and net balance.
    SELECT CUSTOMER
    IF SEEK('M'+&lcAdjTemp..ACCOUNT)
      *-- Lock the file to grantee the phiscal update.
      =RLOCK()
      *-- Add the equivelant amount to customer file.
      lnEqvAmnt = ROUND(&lcAdjTemp..AMOUNT &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)
      REPLACE OPENCR  WITH IIF(MCHOICE = "D" , OPENCR , OPENCR + lnEqvAmnt) ;
        CURRENT WITH IIF(MCHOICE = "D" , CURRENT + lnEqvAmnt , CURRENT) ;
        TOTAGE  WITH IIF(MCHOICE = "D" , TOTAGE + lnEqvAmnt, TOTAGE) ;
        NETBAL  WITH IIF(MCHOICE = "D" , NETBAL + lnEqvAmnt,NETBAL+(-1 * ABS(lnEqvAmnt)))
             
      *C101632,1 (Begin) Appending the chgback field in case of charge BACK. 
      *B604059,1 ASH 12/07/2000 (Begin) Update only in case of charge back transaction.
      *IF MCHOICE = "D" AND llChrgBack
      IF MCHOICE = "D" AND UPPER(&lcAdjTemp..CCHRGBACK)='YES'
      *B604059,1 ASH 12/07/2000 (End)
        REPLACE CHGBACK  WITH CHGBACK+&lcAdjTemp..Amount
      ENDIF
      *C101632,1 (End)
      
      UNLOCK

      *E301245 function to update nHgWtrMark Field with NETBAL field [Begin.]
      =lfHgWUpdat()
      *E301245 function to update nHgWtrMark Field with NETBAL field [End.]
      lcLinkCode = IIF(!EMPTY(CUSTOMER.Link_Code) , CUSTOMER.Link_Code , "DEFDEF")
      *E500374,5  HBG  08/15/2000 If there is a factor ,take the Link Code of it [Begin]
      IF !EMPTY(&lcAdjTemp..cFacCode)
        lcLinkCode = lfGetFacLk(lcLinkCode)
      ENDIF 
      *E500374,5 [End]
    ELSE
      lcLinkCode = "DEFDEF"
      *E500374,5  HBG  08/15/2000 If there is a factor ,take the Link Code of it [Begin]
      IF !EMPTY(&lcAdjTemp..cFacCode)
        lcLinkCode = lfGetFacLk(lcLinkCode)
      ENDIF 
      *E500374,5 [End]
    ENDIF

    SELECT (lcAdjTemp)
    *-- Lock the file to grantee the phiscal update.
    =RLOCK()
    REPLACE nSteps WITH 2
    UNLOCK
  ELSE
    lcLinkCode = IIF(SEEK('M'+&lcAdjTemp..ACCOUNT , "CUSTOMER") , IIF(!EMPTY(CUSTOMER.Link_Code) , CUSTOMER.Link_Code , "DEFDEF") , "DEFDEF")
    *E500374,5  HBG  08/15/2000 If there is a factore ,take the Link Code of it [Begin]
    IF !EMPTY(&lcAdjTemp..cFacCode)
      lcLinkCode = lfGetFacLk(lcLinkCode)
    ENDIF 
    *E500374,5 [End]
  ENDIF

  IF &lcAdjTemp..nSteps < 3
    SELECT (lcAdjTemp)
    IF llUpdRep
      lcTran = ALLTRIM('T' + ALLTRIM(STR(RECNO(lcAdjTemp),5)))
      *-- Update the related rep. commissions records with the currency info.
      SELECT (lcRepCBTmp)
      SCAN FOR ALLTRIM(TRAN) = lcTran
        *-- Lock the file to grantee the phiscal update.
        =RLOCK()
        REPLACE TRAN      WITH lcTranNo ;
          cCurrCode WITH lcCurrCode ;
          nExRate   WITH lnExRate ;
          nCurrUnit WITH lnCurrUnit ;
          DATE  WITH ldBatchDt ;
          BATCH WITH lcBatchNo
        UNLOCK
      ENDSCAN
    ENDIF
    SELECT (lcAdjTemp)
    *-- Lock the file to grantee the phiscal update.
    =RLOCK()
    REPLACE nSteps WITH 3
    UNLOCK
  ENDIF

  IF &lcAdjTemp..nSteps < 4
    IF !EMPTY(lcGLFYear) .AND. !EMPTY(lcGLPeriod) .AND. BETWEEN(VAL(lcGLPeriod) , 1 , 13)
      *E301077,14 Reham On 12/29/98   *** Begin ***
      *E301077,14 Open the factor file.
      =gfOpenFile(gcDataDir+'ARCUSHST',gcDataDir+'ACTHST','SH')
      *E301077,14 Reham On 12/29/98   *** End   ***

      lcGLPeriod = PADL(ALLTRIM(lcGLPeriod) , 2 , "0" )
      *-- Update the customer history file.
      SELECT ARCUSHST
      *-- Lock the file to grantee the phiscal update.
      =RLOCK()
      IF SEEK(&lcAdjTemp..Account + lcGLFYear , "ARCUSHST")
        IF MCHOICE = "D"
          REPLACE nDrAdj            WITH nDrAdj + ROUND(ABS(&lcAdjTemp..Amount) &lcExRSin lnExRate &lcUntSin lnCurrUnit,2) ;
            nDrAdj&lcGLPeriod WITH nDrAdj&lcGLPeriod + ROUND(ABS(&lcAdjTemp..Amount) &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)
        ELSE
          REPLACE nCrAdj            WITH nCrAdj + ROUND(ABS(&lcAdjTemp..Amount) &lcExRSin lnExRate &lcUntSin lnCurrUnit,2) ;
            nCrAdj&lcGLPeriod WITH nCrAdj&lcGLPeriod + ROUND(ABS(&lcAdjTemp..Amount) &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)
        ENDIF
        UNLOCK
      ENDIF
      *E301077,14 Reham On 12/29/98   *** Begin ***
      =gfCloseFile('ARCUSHST')
      *E301077,14 Reham On 12/29/98   *** End   ***
    ENDIF
    SELECT (lcAdjTemp)
    *-- Lock the file to grantee the phiscal update.
    =RLOCK()
    REPLACE nSteps WITH 4
    UNLOCK
  ENDIF
  IF &lcAdjTemp..nSteps < 5
    *-- Default the Adj. Account with the saved one in the adj. file.
    lcGLAcc = &lcAdjTemp..cAdjAcct

    *-- If there is AP Link, add a record in the AP Payment file.
    IF llLink_AP AND !EMPTY(&lcAdjTemp..cBnkCode) AND !EMPTY(&lcAdjTemp..cChkAcct)
      *-- Add record in the AP payment file.
      SELECT APPAYMNT
      APPEND BLANK
      *-- Lock the file to grantee the phiscal update.
      =RLOCK()

      *E301266,1 AKA (Start)
      *REPLACE cPayType  WITH 'A' ;
      *        cPayMeth  WITH 'A' ;
      *        cBnkCode  WITH &lcAdjTemp..cBnkCode ;
      *        cChkAcct  WITH &lcAdjTemp..cChkAcct ;
      *        dPayDate  WITH ldBatchDt ;
      *        cFisFYear WITH lcGLFYear ;
      *        cFspprdid WITH lcGLPeriod ;
      *        cPayDocNo WITH CUSTOMER.Store ;
      *        cPayClNo  WITH CUSTOMER.Account ;
      *        cPayComp  WITH IIF(!EMPTY(&lcAdjTemp..REFERENCE),&lcAdjTemp..REFERENCE,CUSTOMER.BtName) ;
      *        nPayAmnt  WITH &lcAdjTemp..Amount ;
      *        cPayRecSt WITH 'O'

      REPLACE cPayType  WITH 'A' ;
        cPayMeth  WITH 'A' ;
        cBnkCode  WITH &lcAdjTemp..cBnkCode ;
        cChkAcct  WITH &lcAdjTemp..cChkAcct ;
        dPayDate  WITH ldBatchDt ;
        cFisFYear WITH lcGLFYear ;
        cFspprdid WITH lcGLPeriod ;
        cPayDocNo WITH CUSTOMER.Store ;
        cPayClNo  WITH CUSTOMER.Account ;
        cPayComp  WITH IIF(!EMPTY(&lcAdjTemp..REFERENCE),&lcAdjTemp..REFERENCE,CUSTOMER.BtName) ;
        nPayAmnt  WITH &lcAdjTemp..Amount ;
        cPayRecSt WITH 'O'  ;
        Batch     WITH lcBatchNo ;
        cCurrCode WITH lcCurrCode ;
        nExRate   WITH lnExRate ;
        nCurrUnit WITH lnCurrUnit


      *E301266,1 AKA (End)


      *-- Call global function to add audit fields info.
      =gfAdd_Info("APPAYMNT")
      UNLOCK

      *-- Get the GL account from the AP checks file.
      lcGLAcc = IIF(SEEK(&lcAdjTemp..cBnkCode + &lcAdjTemp..cChkAcct , ;
        "APChecks") , APChecks.cChkGLAcc , lcGLAcc)
    ENDIF

    *-- Open GLDIST file to call General Ledger distribution procedure.
    IF llLink_Gl
      *-- Call GL Distribution procedure 2 times for each line of this adj.
      SELECT (lcAdjTemp)
      IF MCHOICE = "C"
        *-- If credit adjustment.
        DO GLDIST WITH 'DEFDEF','009',-(&lcAdjTemp..AMOUNT), ;
          'CA',&lcAdjTemp..TRAN,ldBatchDt,lcGLFYear,lcGLPeriod,'&lcGLTemp', ;
          lcGLAcc,lcCurrCode,lnCurrUnit,lnExRate
        DO GLDIST WITH lcLinkCode,'001',&lcAdjTemp..AMOUNT, ;
          'CA',&lcAdjTemp..TRAN,ldBatchDt,lcGLFYear,lcGLPeriod,'&lcGLTemp', ;
          '',lcCurrCode,lnCurrUnit,lnExRate
      ELSE
        *-- If debit adjustment.
        DO GLDIST WITH '','010',-(&lcAdjTemp..AMOUNT), ;
          'DA',&lcAdjTemp..TRAN,ldBatchDt,lcGLFYear,lcGLPeriod,'&lcGLTemp', ;
          lcGLAcc,lcCurrCode,lnCurrUnit,lnExRate
        DO GLDIST WITH lcLinkCode,'001',&lcAdjTemp..AMOUNT, ;
          'DA',&lcAdjTemp..TRAN,ldBatchDt,lcGLFYear,lcGLPeriod,'&lcGLTemp', ;
          '',lcCurrCode,lnCurrUnit,lnExRate
      ENDIF

      SELECT (lcCurAlis)
      *-- Lock the file to grantee the phiscal update.
      =RLOCK()
      REPLACE cArGLAcc WITH &lcGLTemp..GLAccount ;
        cAdjAcct WITH lcGLAcc
      UNLOCK

      SELECT (lcAdjTemp)
      *-- Lock the file to grantee the phiscal update.
      =RLOCK()
      REPLACE cArGLAcc WITH &lcGLTemp..GLAccount ;
        cAdjAcct WITH lcGLAcc
      UNLOCK
    ENDIF

    SELECT (lcAdjTemp)
    *-- Lock the file to grantee the phiscal update.
    =RLOCK()
    REPLACE nSteps WITH 5
    UNLOCK
  ENDIF
  SELECT (lcAdjTemp)
ENDSCAN
SET ORDER TO TAG (lcAdjTemp)

*-- Append from the temp. GL distribution file to General Ledger
*-- distribution file.
IF llLink_Gl
  *-- Generate a unique session no.
  lcGLSession = gfsequence('GLSESSION')
  SELECT (lcGLTemp)
  REPLACE ALL GLSESSION WITH lcGLSession
  USE
  *-- Update the GL distribution file.
  SELECT GLDIST
  APPEND FROM &gcWorkDir.&lcGLTemp
  =gfOpenFile(gcWorkDir+lcGLTemp , "" ,"EX")
ENDIF

*-- If credit transaction & there is salesreps chargebacks to be updated.
IF llUpdRep .AND. llRepCB
  SELECT (lcRepCBTmp)
  GOTO TOP
  lnCurRec  = 1
  lnTotRec  = RECCOUNT(lcRepCBTmp)
  *-- Scan in the salesreps chargebacks file to update the salesrep & repcomm files.
  SCAN
    *-- Call global function to display thermometer.
    =gfTherm (lnTotRec,lnCurRec,"Updating the salesrep commission File.")
    lnCurRec = lnCurRec + 1
    *-- Calculated the equivalent amount.
    lnCurAmt    = ROUND(&lcRepCBTmp..AMOUNT &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)
    lnRpFrnAmnt = &lcRepCBTmp..AMOUNT
    *-- Update the salesrep info if exist in its file.
    SELECT SALESREP
    IF SEEK(&lcRepCBTmp..REPCODE)
      IF &lcRepCBTmp..nSteps < 1
        lnNewBal = SALESREP.BALANCE + lnCurAmt
        *-- Lock the file to grantee the phiscal update.
        =RLOCK()
        REPLACE CURRENT WITH CURRENT + lnCurAmt ;
          BALANCE WITH lnNewBal
        UNLOCK

        SELECT (lcRepCBTmp)
        *-- Lock the file to grantee the phiscal update.
        =RLOCK()
        REPLACE nSteps WITH 1
        UNLOCK
      ENDIF

      SELECT (lcRepCBTmp)
      IF &lcRepCBTmp..nSteps < 2
        *-- Lock the file to grantee the phiscal update.
        =RLOCK()
        *-- Update Repcomm amount with equivalent amount and foreign
        *-- amount with foreign amount.
        REPLACE BALANCE  WITH lnNewBal ;
          Amount   WITH lnCurAmt ;
          nForAmnt WITH lnRpFrnAmnt
        REPLACE nSteps WITH 2
        UNLOCK
      ENDIF
    ENDIF
  ENDSCAN
  SELECT (lcRepCBTmp)
  USE
  *-- Update the salesreps commesion file.
  SELECT REPCOMM
  APPEND FROM &gcWorkDir.&lcRepCBTmp
  =gfOpenFile(gcWorkDir+lcRepCBTmp , gcWorkDir+lcRepCBTmp ,"EX")
ENDIF

*-- Finish the whole session.
SELECT UNCMSESS
IF lnUnCmSeRc <> 0 .AND. lnUnCmSeRc <= RECCOUNT()
  GOTO lnUnCmSeRc
  REPLACE STATUS WITH 'C'
  llContinue = .F.
  UNLOCK
ENDIF
lnUnCmSeRc = 0

*-- Go to select mode.
laScrMode    = "S"
laScrMode    = .F.
laScrMode[1] = .T.
SHOW GETS

*!*************************************************************
*! Name      : lfvInvoice
*! Developer : Reham Al-Allamy
*! Date      : 07/30/97
*! Purpose   : Valid function for the invoice object.
*!*************************************************************
*! Calls     : AriaBrow, gfRltFld
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvInvoice()
*!*************************************************************
*
FUNCTION lfvInvoice

*-- Add zeros to the left of the invoice variable.
lcInvoice = IIF(!EMPTY(lcInvoice) .AND. !llBrowse , PADL(ALLTRIM(lcInvoice) , 6, "0") , lcInvoice)

IF !EMPTY(lcInvoice) .OR. llBrowse .OR. lcOldValue <> lcInvoice
  *-- If the invoce exist, get its info.
  IF SEEK(lcAccount + lcInvoice , "INVHDR") .AND. !llBrowse
    lcRepCode1 = REP1
    lnComm1    = COMM1
    lcRepCode2 = REP2
    lnComm2    = COMM2
    lcTerms    = cTermCode
    lnDiscount = trde_disc
    lcOrder    = ORDER
    lcStore    = STORE
    lcCustPo   = CUSTPO
  ELSE
    *-- If the customer has inoices, browse the available invoices.
    IF SEEK(lcAccount , "INVHDR")
      lcBrfields = "INVOICE :H='Invoice',STATUS :H='S',INVDATE :H='Date',"+;
        "STORE :H='Store',ORDER :H='Order',ACCOUNT :H='Account':7,"+;
        "TOTALCHG :H='Amount',PIKTKT :H='Piktkt',CUSTPO :H='Cust P/O'"
      DECLARE laInv[1]
      laInv = ""
      =AriaBrow('lcAccount',"Invoices",gnBrHSRow1,gnBrHSCol1,;
        gnBrHSRow2, gnBrHSCol2,'','','INVOICE','laInv')

      IF !EMPTY(laInv[1])
        lcInvoice  = INVOICE
        lcRepCode1 = REP1
        lnComm1    = COMM1
        lcRepCode2 = REP2
        lnComm2    = COMM2
        lcTerms    = cTermCode
        lnDiscount = trde_disc
        lcOrder    = ORDER
        lcStore    = STORE
        lcCustPo   = CUSTPO
      ELSE
        *-- Blank all the Variables.
        STORE " " TO lcInvoice , lcRepCode1 , lcRepName1 , lcRepCode2 , ;
          lcRepName2 , lcTerms , lcOrder , lcStore , lcCustPo
        STORE  0  TO lnDiscount , lnCharge0 , lnComm1 , lnCharge1 , lnComm2 , lnCharge2
        STORE .F. TO llEditRep
      ENDIF
    ELSE
      *-- If there is no invoices, blank the related variables.
      *** There is no records to display. ***
      *** <  OK  > ***
      =gfModalGen("INM00052B00000" , "DIALOG")
      *-- Blank all the Variables.
      STORE " " TO lcInvoice , lcRepCode1 , lcRepName1 , lcRepCode2 , ;
        lcRepName2 , lcTerms , lcOrder , lcStore , lcCustPo
      STORE  0  TO lnDiscount , lnCharge0 , lnComm1 , lnCharge1 , lnComm2 , lnCharge2
      STORE .F. TO llEditRep
    ENDIF
  ENDIF
ENDIF

IF EMPTY(lcTerms) .AND. SEEK("M"+lcAccount , "CUSTOMER")
  lcTerms = IIF(EMPTY(lcTerms) , CUSTOMER.cTermCode , lcTerms)
  *-- Get Trade Discount If exist.
  IF !EMPTY(lcTerms)
    *-- Get the trade discount for the current term.
    DECLARE laTrmRltFd[1,2]
    laTrmRltFd[1,1] = 'NTERDISCR'
    laTrmRltFd[1,2] = 'lnDiscount'
    =gfRltFld(lcTerms , @laTrmRltFd , "CTERMCODE")
  ELSE
    lnDiscount = 0
  ENDIF
ENDIF

*-- If there is no invoice or rep. codes enetered.
IF EMPTY(lcInvoice) .AND. EMPTY(lcRepCode1) .AND. EMPTY(lcRepCode2)
  *-- Default the rep. codes to the rep. codes from the customer fle.
  IF EMPTY(lcRepCode1)
    IF SEEK("M"+lcAccount , "CUSTOMER")
      lcRepCode1 = CUSTOMER.SALESREP
      lnComm1    = CUSTOMER.COMM
      lcRepCode2 = CUSTOMER.REP2
      lnComm2    = CUSTOMER.COMM2
      lcTerms    = IIF(EMPTY(lcTerms) , CUSTOMER.cTermCode , lcTerms)
      *-- Get Trade Discount If exist.
      IF !EMPTY(lcTerms)
        *-- Get the trade discount for the current term.
        DECLARE laTrmRltFd[1,2]
        laTrmRltFd[1,1] = 'NTERDISCR'
        laTrmRltFd[1,2] = 'lnDiscount'
        =gfRltFld(lcTerms , @laTrmRltFd , "CTERMCODE")
      ELSE
        lnDiscount = 0
      ENDIF
    ENDIF
  ENDIF
  llEditRep = .T.
  IF EMPTY(lcRepCode1)
    SHOW GET lcRepCode1
    SHOW GET lcRepCode2
    _CUROBJ   = OBJNUM(lcRepCode1)
    RETURN
  ELSE
    SHOW GET lcRepCode1
    SHOW GET lcRepCode2
  ENDIF
ENDIF

IF !EMPTY(lcRepCode1) .OR. !EMPTY(lcRepCode2)
  *-- Get the sales rep. names.
  lcRepName1 = LOOKUP(SALESREP.Name,lcRepCode1,SALESREP.REPCODE,"SALESREP")
  lcRepName2 = LOOKUP(SALESREP.Name,lcRepCode2,SALESREP.REPCODE,"SALESREP")
  *  *-- Get Trade Discount If exist.
  *  IF !EMPTY(lcTerms)
  *    *-- Get the trade discount for the current term.
  *    DECLARE laTrmRltFd[1,2]
  *    laTrmRltFd[1,1] = 'NTERDISCR'
  *    laTrmRltFd[1,2] = 'lnDiscount'
  *    =gfRltFld(lcTerms , @laTrmRltFd , "CTERMCODE")
  *  ELSE
  *    lnDiscount = 0
  *  ENDIF
  *-- Calculate the sales reps. charges.
  lnCharge0  = IIF(!EMPTY(lnDiscount) , lnTrnAmt - (lnTrnAmt * (lnDiscount/100)), lnTrnAmt)
  lnCharge1  = IIF(!EMPTY(lcRepCode1) , ABS(ROUND(lnCharge0 * (lnComm1/100),2)) * -1 , 0 )
  lnCharge2  = IIF(!EMPTY(lcRepCode2) , ABS(ROUND(lnCharge0 * (lnComm2/100),2)) * -1 , 0 )
ENDIF

llEditCom1 = IIF(!EMPTY(lcRepCode1) , .T. , .F. )
llEditCom2 = IIF(!EMPTY(lcRepCode2) , .T. , .F. )

*-- Refresh the objects in the chargeback screen.
SHOW GET lcInvoice
SHOW GET lnDiscount
SHOW GET lcRepCode1
SHOW GET lcRepName1
SHOW GET lcRepCode2
SHOW GET lcRepName2
SHOW GET lnComm1
SHOW GET lnCharge1
SHOW GET lnComm2
SHOW GET lnCharge2

*-- Set the browse flag to false.
llBrowse = .F.

*!*************************************************************
*! Name      : lfvRepCode
*! Developer : Reham Al-Allamy
*! Date      : 07/30/97
*! Purpose   : Valid function for the salesreps object.
*!*************************************************************
*! Calls     : REPCHK
*!*************************************************************
*! Passed Parameters  :  lcRepNo -> Rep code (1 - 2)
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRepCode("1")
*!*************************************************************
*
FUNCTION lfvRepCode
PARAMETERS lcRepNo

IF !EMPTY(lcRepCode&lcRepNo)
  *-- If the sales rep. does no exist in the salesrep file.
  IF !SEEK(lcRepCode&lcRepNo , "SALESREP")
    *-- Call global function to browse salesreps.
    DO REPCHK WITH lcRepCode&lcRepNo
  ENDIF
  IF !EMPTY(lcRepCode&lcRepNo)
    *-- If valid salesrep, calculate charge back for this salesrep.
    lcRepName&lcRepNo = LOOKUP(SALESREP.Name , lcRepCode&lcRepNo , ;
      SALESREP.REPCODE , "SALESREP")
    lnCharge0         = IIF(!EMPTY(lnDiscount) , ;
      lnTrnAmt - (lnTrnAmt * (lnDiscount/100)), lnTrnAmt)
    lnCharge&lcRepNo  = IIF(!EMPTY(lcRepCode&lcRepNo) , ;
      ABS(ROUND(lnCharge0 * (lnComm&lcRepNo/100),2)) * -1 , 0 )

    llEditCom&lcRepNo = IIF(!EMPTY(lcRepCode&lcRepNo) , .T. , .F. )

    *-- Refresh the objects in the charge back screen.
    SHOW GET lnDiscount
    SHOW GET lcRepCode&lcRepNo
    SHOW GET lcRepName&lcRepNo
    SHOW GET lnComm&lcRepNo
    SHOW GET lnCharge&lcRepNo
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvComm
*! Developer : Reham Al-Allamy
*! Date      : 07/30/97
*! Purpose   : Valid function for the commession object.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcCommNo -> The commession for rep (1-2)
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvComm("1")
*!*************************************************************
*
FUNCTION lfvComm
PARAMETERS lcCommNo

IF lnComm&lcCommNo <> lnOldValue
  *-- If change the rep. commissions, calculate the charge back.
  lnCharge0 = IIF(!EMPTY(lnDiscount) , lnTrnAmt - (lnTrnAmt * (lnDiscount/100)), lnTrnAmt)
  lnCharge&lcCommNo = IIF(!EMPTY(lcRepCode&lcCommNo) , ABS(ROUND(lnCharge0 * (lnComm&lcCommNo/100),2)) * -1 , 0 )
  *-- Refresh the commesion percentage & amount objects.
  SHOW GET lnComm&lcCommNo
  SHOW GET lnCharge&lcCommNo
ENDIF

*!*************************************************************
*! Name      : lfvChrgOk
*! Developer : Reham Al-Allamy
*! Date      : 07/30/97
*! Purpose   : Valid function for the OK button.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvChrgOk()
*!*************************************************************
*
FUNCTION lfvChrgOk

*** Are you sure you want to enter salesrep chargeback without an invoice # ? ***
*** < Yes > - < No > ***
IF EMPTY(lcInvoice) .AND.  ;
    gfModalGen("QRM40076B00006" , "DIALOG") = 2
  _CUROBJ = OBJNUM(lcInvoice)
  RETURN
ENDIF

SELECT (lcRepCBTmp)
FOR lnCount = 1 TO 2
  *-- If the rep. code not empty, add record in the salesrep temp. file.
  lcCount = ALLTRIM(STR(lnCount))
  IF !EMPTY(lcRepCode&lcCount)
    *-- If the current salesrep not found in the temp. chargebacks
    *-- file, add the transaction to the file.
    IF !SEEK(lcTran + lcCount)
      APPEND BLANK
    ENDIF
    *-- Lock the file to grantee the phiscal update.
    =RLOCK()
    REPLACE REPCODE    WITH lcRepCode&lcCount ;
      TRAN       WITH lcTran ;
      ORDER      WITH lcOrder ;
      ACCOUNT    WITH lcAccount ;
      STORE      WITH lcStore ;
      CUSTPO     WITH lcCustPo ;
      COMMPCNT   WITH lnComm&lcCount ;
      AMOUNT     WITH lnCharge&lcCount ;
      nOrg_Amnt  WITH -1 * ABS(lnCharge0) ;
      DATE       WITH gdSysDate ;
      DESC       WITH IIF(!EMPTY(lcInvoice) , 'CREDIT ADJ.   '+lcInvoice , 'CREDIT ADJ.') ;
      TRANTYPE   WITH '5' ;
      STATUS     WITH 'O' ;
      REPNO      WITH lcCount
    *-- Call global function to add audit fields info.
    =gfAdd_Info(lcRepCBTmp)
    UNLOCK
  ELSE
    IF SEEK(lcTran + lcCount)
      DELETE
    ENDIF
  ENDIF
ENDFOR
CLEAR READ

*!*************************************************************
*! Name      : lfUpdVars
*! Developer : Reham Al-Allamy
*! Date      : 08/03/97
*! Purpose   : Update the comment field with the available variables.
*!*************************************************************
*! Calls     : gfSavSess
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfUpdVars()
*!*************************************************************
*
FUNCTION lfUpdVars

*-- Concatinate the files variable.
lcFiles = 'lcAdjTemp,'+lcAdjTemp+','+lcAdjTemp+';'+ ;
  IIF(MCHOICE="C",'lcRepCBTmp,'+lcRepCBTmp+','+lcRepCBTmp+';','')

*-- Call global function to update the uncompleted session record in the
*-- uncompleted session file with the files names & screen variables.
llNoThing = IIF(lnUnCmSeRc=0, .T., gfSavSess(lcProgID, lcFiles, @laVars, lcSession))

*!*************************************************************
*! Name      : lfAdUnCmSR
*! Developer : Reham Al-Allamy
*! Date      : 08/03/97
*! Purpose   : Add record in the uncompleted session file.
*!*************************************************************
*! Calls     : lfUpdVars
*!*************************************************************
*! Passed Parameters  :  lcAddObj-> Current object pressed.
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfAdUnCmSR()
*!*************************************************************
*
FUNCTION lfAdUnCmSR
PARAMETERS lcAddObj
PRIVATE lcCurAlias , lcAddObj

*-- If the current object was not sent as a parameter, define it.
IF TYPE("lcAddObj") $ "UL"
  lcAddObj = SPACE(10)
ENDIF

*-- Save current alias.
lcCurAlias = ALIAS()

*-- Add record in the uncompleted session file.
SELECT 'UNCMSESS'
IF !SEEK('I')
  APPEND BLANK
ENDIF
lnUnCmSeRc = RECNO()
BLANK
REPLACE STATUS     WITH 'O'       ;
  cUTranType WITH lcProgID  ;
  cUserId    WITH gcUser_id ;
  cSession   WITH lcSession ;
  cProgram   WITH "ARDCADJ" ;
  cCurrScr   WITH "ARDCADJ" ;
  dTranDate  WITH gdSysDate ;
  cCurrObj   WITH lcAddObj  ;
  cTranTime  WITH TIME()

*-- Lock the current record in the uncompleted session file.
=RLOCK()

*-- Call local function to update the temp. files & needed variables
*-- fields in the uncompleted session file record.
=lfUpdVars()

*-- Restore the previous alias.
SELECT (lcCurAlias)

*!*************************************************************
*! Name      : lfChkUnComS
*! Developer : Reham Al-Allamy
*! Date      : 02/16/1998
*! Purpose   : Check if there is previous uncompleted session.
*!*************************************************************
*! Calls     : gfUnCompSession, lpShow, gfCPSave
*!*************************************************************
*! Parameters: llFrmSetup -> From screen setup or lpShow
*!*************************************************************
*! Returns   : llFondSess -> .T. if there is session found.
*!*************************************************************
*! Example   :  =lfChkUnComS()
*!*************************************************************
*
FUNCTION lfChkUnComS
PARAMETERS llFrmSetup
PRIVATE    llFondSess

*-- Flag to know if found uncompleted session or not.
llFondSess = .F.
*-- Flag to know if check the uncompleted session.
llGoAndChk = IIF(llFrmSetup, .F., llGoAndChk)

*-- If coming from the screen setup & should check the uncompleted session
*-- Or coming from the lpshow procedure.
IF (llFrmSetup .AND. llChkUnCom) .OR. !llFrmSetup
  llChkUnCom = .F.
  *-- Call global function to know if there is uncompleted session was saved in
  *-- the uncompleted session file.
  IF gfUnCompSession(lcProgID, lnSessNo, lcMode+" Adjustment")
    *-- Flag to execute the activate folders.
    llFromSPR  = .T.
    *-- Restore the previous mode.
    STORE .F. TO laScrMode
    laScrMode[ATC(lcScrMode,"SVEA")] = .T.
    STORE .T. TO llContinue, llCUpDate
    lcSession = UnCmSess.cSession
    *-- Get the object should be pressed.
    lcCurObj  = UPPER(ALLTRIM(UnCmSess.cCurrObj))

    *-- If there is no object was saved and not coming from setup.
    IF EMPTY(lcCurObj) .OR. !llFrmSetup
      *-- Set the found session flag to true.
      llFondSess = .T.
      *-- Go to refresh the screen.
      SHOW GETS
    ELSE
      *-- If the saved object was the <SAVE> button.
      IF lcCurObj  = "PBSAV"
        *-- Call local procedure save.
        =gfCPSave()
        *-- Set the found session flag to false.
        llFondSess   = .F.
        *-- Set the current mode to be select mode.
        laScrMode    = .F.
        laScrMode[1] = .T.
        *-- Call the show procedure.
        DO lpShow
      ELSE
        *-- Set the found session flag to true.
        llFondSess = .T.
        *-- Refresh the screen.
        SHOW GETS
      ENDIF
    ENDIF
  ELSE
    *-- Call local function to create the needed temp. files.
    =lfCrtUnComp()
  ENDIF
ENDIF

*-- Return with flag if found uncompleted session or not.
RETURN (llFondSess)

*!*************************************************************
*! Name      : lfCrtUnComp
*! Developer : Reham Al-Allamy
*! Date      : 12/21/97
*! Purpose   : Create all the needed temp files.
*!*************************************************************
*! Calls     : gfCrtTmp
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfCrtUnComp()
*!*************************************************************
*
FUNCTION lfCrtUnComp

*B601927,1 Hesham El-Sheltawi (Start)
*B601927,1 function to create the temprory files for the uncomplete session
*B601927,1 feature

*-- For Credits Check To See If Charge Back To Salesrep.
IF llRepCB
  SELECT REPCOMM
  =AFIELDS(laFileStru)
  *-- Add field for keep tracking the sales reps. for each transaction.
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]
  laFileStru[lnFileStru+1,1] = 'REPNO'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 1
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'NSTEPS'
  laFileStru[lnFileStru+2,2] = 'N'
  laFileStru[lnFileStru+2,3] = 2
  laFileStru[lnFileStru+2,4] = 0
  *-- Make the index by tran + the rep no (alwayse 1 , 2 , 1 & 2).
  =gfCrtTmp(lcRepCBTmp,@laFileStru,[ALLTRIM(TRAN)+REPNO],lcRepCBTmp)
ENDIF

*-- Create temp. file for (Debit -or- Credit) file according to the current program.
SELECT (lcCurAlis)

=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)

*E301326,1 WAB - add field for charge back in case of debit adjustment
*E301326,1 WAB - in case of debit adjustment
*E301326,1 WAB - START 
*DIMENSION laFileStru[lnFileStru+1,4]
DIMENSION laFileStru[lnFileStru+IIF(MCHOICE="D",2,1),4]

*E301326,1 WAB - END

laFileStru[lnFileStru+1,1] = 'NSTEPS'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0

*E301326,1 WAB - fill the array with th structure of the new field (charge back)
*E301326,1 WAB - in case of debit adjustment
*E301326,1 WAB - START 
IF MCHOICE = "D" 
  laFileStru[lnFileStru+2,1] = 'cChrgBack'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 3
  laFileStru[lnFileStru+2,4] = 0
ENDIF
*E301326,1 WAB - END

*C101632,1 (Begin) Add New fileds to the temp file in case of charge back fOR KRAZI KAT. 
IF ASCAN(laEvntTrig,PADR("ADDFIELD",10)) <> 0
  = gfDoTriger('ARDBADJ','ADDFIELD')
ENDIF  
*C101632,1 (End)

=gfCrtTmp(lcAdjTemp,@laFileStru,[Account + Tran + DTOS(TranDate)],lcAdjTemp)


*!*************************************************************
*! Name      : lfClsFil
*! Developer : Reham Al-Allamy
*! Date      : 06/07/98
*! Purpose   : Close the files opened in the current session.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClsFil()
*!*************************************************************
*
FUNCTION lfClsFil

*-- If multi currency.
IF llMulCurr
  IF llOpnCurr
    *-- If the file was opened in this session, close it.
    USE IN SycCurr
  ELSE
    *B602537,1 Reham On 02/17/99   *** Begin ***
    *-- File was opened in previous session, Set the opinter to th right record.
    *GOTO lnRecCurr IN SycCurr
    *B602537,1 Reham On 02/17/99   *** End   ***
  ENDIF
ENDIF

*-- If the system is linked to GL.
IF llLink_Gl
  IF llOpenGLD
    *-- If the file was opened in this session, close it.
    USE IN GLDIST
  ELSE
    *B602537,1 Reham On 02/17/99   *** Begin ***
    *-- File was opened in previous session, Set the opinter to th right record.
    *GOTO lnRecGLDst IN GLDIST
    *B602537,1 Reham On 02/17/99   *** End   ***
  ENDIF
ENDIF

*-- If the system is linked to GL and AP.
*B801808,1 Reham On 11/15/98   *** Begin ***
*B801808,1 Fix the error of "File ApPayMnt Not Found".
*IF llLink_Gl AND llLink_AP
IF llLink_Gl
  *B801808,1 Reham On 11/15/98   *** End   ***
  IF llOpnGLLnk
    *-- If the file was opened in this session, close it.
    USE IN GL_LINK
  ELSE
    *B602537,1 Reham On 02/17/99   *** Begin ***
    *-- File was opened in previous session, Set the opinter to th right record.
    *GOTO lnRecGLLnk IN GL_LINK
    *B602537,1 Reham On 02/17/99   *** End   ***
  ENDIF
  *B801808,1 Reham On 11/15/98   *** Begin ***
ENDIF
IF llLink_AP
  *B801808,1 Reham On 11/15/98   *** End   ***
  IF llOpnAPChk
    *-- If the file was opened in this session, close it.
    USE IN ApChecks
  ELSE
    *B602537,1 Reham On 02/17/99   *** Begin ***
    *-- File was opened in previous session, Set the opinter to th right record.
    *GOTO lnRecAPChk IN ApChecks
    *B602537,1 Reham On 02/17/99   *** End   ***
  ENDIF

  IF llOpnApPay
    *-- If the file was opened in this session, close it.
    USE IN Appaymnt
  ELSE
    *B602537,1 Reham On 02/17/99   *** Begin ***
    *-- File was opened in previous session, Set the opinter to th right record.
    *GOTO lnRecApPay IN Appaymnt
    *B602537,1 Reham On 02/17/99   *** End   ***
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfOpnFil
*! Developer : Reham Al-Allamy
*! Date      : 06/07/98
*! Purpose   : Open the needed files for the current program.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfOpnFil()
*!*************************************************************
*
FUNCTION lfOpnFil

*-- Open currency file.
IF llMulCurr
  IF !USED('SycCurr')
    *-- Open the file if it was not open.
    =gfOpenFile(gcSysHome+'SycCurr',gcSysHome+'cCurrCode','SH')
    llOpnCurr = .T.
  ELSE
    SELECT SYCCURR
    SET ORDER TO TAG cCurrCode
    *-- Save the current record no. if the file was open.
    lnRecCurr = RECNO("SycCurr")
  ENDIF
ELSE
  lcCurrCode = gcBaseCurr && Variable to hold currency code.
  lnExRate   = 1          && Variable to hold exchange rate.
  lnCurrUnit = 1          && Variable to hold exchange rate.
  lcUntSin   = '/'        && Variable to hold unit sign.
  lcExRSin   = '/'        && Variable to hold exchange rate sign.
  *  llValidCur = .T.        && Flag to avoid validating the currency code.
  *  llValidRat = .T.        && Flag to avoid validating the Exchange rate.
ENDIF
llValidCur = .T.        && Flag to avoid validating the currency code.
llValidRat = .T.        && Flag to avoid validating the Exchange rate.

*-- If the system is linked to GL and AP, open the files
*-- that will be used.
*B801808,1 Reham On 11/15/98   *** Begin ***
*B801808,1 Fix the error of "File ApPayMnt Not Found".
*IF llLink_Gl AND llLink_AP
IF llLink_Gl
  *B801808,1 Reham On 11/15/98   *** End   ***
  llGLAP = .T.
  IF !USED('GL_LINK')
    *-- Open the file if it was not open.
    =gfOpenFile(gcDataDir+'GL_LINK',gcDataDir+'GL_LINK','SH')
    llOpnGLLnk = .T.
  ELSE
    SELECT GL_LINK
    SET ORDER TO TAG GL_LINK
    *-- Save the current record no. if the file was open.
    lnRecGLLnk = RECNO("GL_LINK")
  ENDIF
  *B801808,1 Reham On 11/15/98   *** Begin ***
  *B801808,1 Fix the error of "File ApPayMnt Not Found".
ENDIF
IF llLink_AP
  *B801808,1 Reham On 11/15/98   *** End   ***
  IF !USED('ApChecks')
    *-- Open the file if it was not open.
    =gfOpenFile('&gcDataDir.ApChecks','&gcDataDir.BankCheck','SH')
    llOpnAPChk = .T.
  ELSE
    SELECT ApChecks
    SET ORDER TO TAG BankCheck
    *-- Save the current record no. if the file was open.
    lnRecAPChk = RECNO("ApChecks")
  ENDIF

  IF !USED('Appaymnt')
    *-- Open the file if it was not open.
    =gfOpenFile('&gcDataDir.Appaymnt',' ','SH')
    llOpnApPay = .T.
  ELSE
    *-- Save the current record no. if the file was open.
    lnRecApPay = RECNO("Appaymnt")
  ENDIF
ENDIF

*-- Open GLDIST file to execute the GL distribution function.
IF llLink_Gl
  IF !USED("GLDIST")
    llOpenGLD = .T.
    =gfOpenFile(gcDataDir+"GLDIST",gcDataDir+"GLDISTAC","SH")
  ELSE
    llOpenGLD = .F.
    SELECT GLDIST
    *-- Save the current record no. if the file was open.
    lnRecGLDst = RECNO("GLDIST")
    SET ORDER TO TAG GLDISTAC
  ENDIF
  IF !FILE(gcWorkDir+lcGLTemp)
    *-- Create temp. file hold the gl entries.
    =AFIELDS(laFileStru)
    CREATE TABLE (gcWorkDir+lcGLTemp) FROM ARRAY laFileStru
  ENDIF
ENDIF


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

*E301326,1 WAB - START 
*!**************************************************************************
*! Name      : lfvChrgBack()
*! Developer : Sameh (SSE)
*! Date      : 06/09/1999
*! Purpose   : replace cChrgBack field with "YES"  when use checked 
*!             the screen object(llchrgbak) OR WITH "NO" IF screen 
*!             object(llchrgbak) is empty and replace trantype '3' 
*!             (charge back type ) if llchrgbak is true
*!*************************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************************
*! Passed Parameters  : None
*!*************************************************************************
*! Returns            : None
*!*************************************************************************
*! Example   : =lfvChrgBack()
*!*************************************************************************
FUNCTION lfvChrgBack
PRIVATE lcCurAlias

lcCurAlias = SELECT(0)
SELECT (lcAdjTemp)
REPLACE cChrgBack WITH IIF(llChrgBack,'Yes','No'),;
        TranType  WITH IIF(llChrgBack,'3','2')

*C101632,1 (Begin) Intialize the two variables and update them.
lcStore    = SPACE(8)
*E500351,1 (Start) Change the value of ldTranDate with " " to be invisible.
*ldTranDate = IIF(llChrgBack,ldBatchDt,{})
*REPLACE Store     WITH SPACE(8),;
        TranDate  WITH ldTranDate
ldTranDate = IIF(llChrgBack,ldBatchDt," ")

REPLACE Store     WITH SPACE(8),;
        TranDate  WITH IIF(llChrgBack,ldTranDate,{})
*E500351,1 (End)

*--Refresh the two variables.   
*B603418,1 NAD (Start)  Show get the fields after calling lfrefresh()  
*IF llChrgBack        
*  SHOW GET ldTranDate ENABLE
*  SHOW GET lcStore ENABLE
*ENDIF
=lfRefresh()
IF llChrgBack        
  SHOW GET ldTranDate ENABLE
  SHOW GET lcStore ENABLE
ENDIF
*B603418,1 NAD (End)

*C101632,1 (End)
        
SELECT (lcCurAlias)
*E301326,1 WAB - END                


*!**************************************************************************
*! Name      : lfvStore
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 10/29/1999
*! Purpose   : Validate STORE field in the scareen in case of chargeback.
*! Refer to  : C(101632)
*!*************************************************************************
FUNCTION lfvStore
PRIVATE lcCurAlias

lcCurAlias = SELECT(0)
SELECT (lcAdjTemp)
*-- Lock the file to grantee the phiscal update.
=RLOCK()
*-- Store field.
REPLACE STORE WITH lcStore
UNLOCK
SHOW WINDOW (lcAdjBrwTl) REFRESH
SELECT (lcCurAlias)
RETURN

*!**************************************************************************
*! Name      : lfvTrnDate
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 10/29/1999
*! Purpose   : Validate transaction field in the scareen in case of chargeback.
*! Refer to  : C(101632)
*!*************************************************************************
FUNCTION lfvTrnDate
PRIVATE lcCurAlias
*B604059,1 ASH 12/07/2000 (Begin) Transaction date cannot be empty.
IF EMPTY(ldTrandate)
  *** 'The Transaction date cannot be empty.' ***
  *** < Ok > ***
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'The Transaction Date cannot be empty.')
  ldTrandate = ldBatchDt
  SHOW GET ldTrandate    
  _CUROBJ = OBJNUM(ldTrandate)
  RETURN
ENDIF
*B604059,1 ASH 12/07/2000 (End)
lcCurAlias = SELECT(0)
IF ldTrandate > ldBatchDt
    *** 'The transaction date cannot follow the Batch Date.' ***
    *** < Ok > ***
    = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'The transaction date cannot follow the Batch Date.')    
    _CUROBJ = OBJNUM(ldTrandate)
    RETURN
ENDIF
SELECT (lcAdjTemp)
*-- Lock the file to grantee the phiscal update.
=RLOCK()
*-- Tran. date field.
REPLACE TRANDATE WITH ldTranDate
UNLOCK
SHOW WINDOW (lcAdjBrwTl) REFRESH
SELECT (lcCurAlias)
RETURN

*!*************************************************************
*! Name      : lfvDueDate
*! Developer : Nader Anis 
*! Date      : 05/03/2000
*! Purpose   : Valid function for the due date field
*! Ref       : E301369,1
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lfvDueDate
*!*************************************************************

FUNCTION lfvDueDate
IF EMPTY(ldDueDate)
    *** 'The Due date cannot be empty.' ***
    *** < Ok > ***
    = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'The Due Date cannot be empty.')
    ldDueDate=lcOLdvalue
    SHOW GET ldDueDate    
    _CUROBJ = OBJNUM(ldDueDate)
    RETURN
ENDIF
IF ldDueDate < ldBatchDt 
    *** 'The Batch date cannot follow the Due Date' ***
    *** < Ok > ***
    = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'The Batch Date cannot follow the Due Date.')
    ldDueDate=lcOLdvalue
    SHOW GET ldDueDate    
    _CUROBJ = OBJNUM(ldDueDate)
    RETURN
ENDIF
lcAlias= ALIAS()
SELECT (lcAdjTemp)
=RLOCK()
REPLACE DUEDATE WITH ldDueDate
UNLOCK 
SELECT(lcAlias)

*!*************************************************************
*! Name      : lfwDueDate
*! Developer : Nader Anis 
*! Date      : 05/03/2000
*! Purpose   : When function for the due date field
*! Ref       : E301369,1
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lfwDueDate
*!*************************************************************
FUNCTION lfwDueDate

lcOLdvalue=ldDueDate

*--End Of lfwdueDate




*!*************************************************************
*! Name      : lfvDesc
*! Developer : Nader Anis 
*! Date      : 07/24/20000
*! Purpose   : Valid function for the description field
*! Ref       : *E500351,1
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION lfvDesc
*-- Add the reference to the current record & refresh the browse.
PRIVATE lcAlias
lcAlias=ALIAS()
SELECT (lcAdjTemp)
*-- Lock the file to grantee the phiscal update.
=RLOCK()
REPLACE DESC WITH lcDesc
UNLOCK

*E301802,1 Move again to new push button. [Begin]
IF MCHOICE="D"
  _CUROBJ = OBJNUM(pbNew)
ENDIF  
*E301802,1 Move again to new push button. [End]

SELECT (lcAlias)


*!*************************************************************
*! Name      : lfGetFacLk
*! Developer : Hend Ghanem 
*! Date      : 08/15/20000
*! Purpose   : Get link Code of The factor
*! Ref       : E500374,5
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION lfGetFacLk
PARAMETER lclink
PRIVATE lcLnkCod 

IF !llOpenFac
  llOpenFac = gfOpenFile(gcDataDir+'FACTOR',gcDataDir+'FACTOR','SH') 
ENDIF

IF SEEK(&lcAdjTemp..cfaccode,'FACTOR')
  lcLnkCod = IIF(!EMPTY(FACTOR.Link_code) , FACTOR.Link_code , lclink)
ELSE
  lcLnkCod = lclink  
ENDIF


RETURN lcLnkCod 

*!**************************************************************************
*! Name      : lfActPad
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 02/03/2002
*! Purpose   : Bulid a new menu pad [Options]
*!**************************************************************************
*! Notes     : Called from ARDCADJ.SCX when function.
*!**************************************************************************
*! Example   :  =lfActPad()
*!**************************************************************************
*E301802,1 Adding the option menu pad.
FUNCTION lfActPad

IF MCHOICE="D"
  DEFINE PAD _INQUIRY OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
  ON PAD _INQUIRY OF _msysmenu ACTIVATE POPUP _INQURYPOP

  DEFINE POPUP _INQURYPOP MARGIN SHADOW
  DEFINE BAR 1 OF _INQURYPOP PROMPT '\<Auto Carry Account'
  SET MARK OF BAR 1 OF _INQURYPOP TO llAutoAcct
  ON SELECTION POPUP _INQURYPOP DO lpAutoAcct
ENDIF  
*-- End of lfActPad.

*!**************************************************************************
*! Name      : lpAutoAcct
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 02/03/2002
*! Purpose   : Save the account selected into a public variable.
*!**************************************************************************
*! Example   : DO lpAutoAcct
*!**************************************************************************
*E301802,1 Validation for the option menu.
PROCEDURE lpAutoAcct
DO CASE
  CASE BAR() = 1
    llAutoAcct = !llAutoAcct
    SET MARK OF BAR 1 OF _INQURYPOP TO llAutoAcct
ENDCASE
*-- End of lpAutoAcct.

FUNCTION LFVTRDISC
lnCharge0 = IIF( !EMPTY(lnDiscount),lnTrnAmt - (lnTrnAmt*(lnDiscount/100)),lnTrnAmt)
IF lnComm1<>0
   lnCharge1 = IIF(!EMPTY(lcRepCode1),ABS(ROUND(lnCharge0*(lnComm1/100),2))*-1,0)
   SHOW GET lnComm1
   SHOW GET lnCharge1
ENDIF
IF lnComm2<>0
   lnCharge2 = IIF(!EMPTY(lcRepCode2),ABS(ROUND(lnCharge0*(lnComm2/100),2))*-1,0)
   SHOW GET lnComm2
   SHOW GET lnCharge2
ENDIF
