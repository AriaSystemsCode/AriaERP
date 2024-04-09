*:----------------------------------------------------------------
*: Program File        : ARKEYOF.PRG
*: Program Description : Apply Debit\Credit (Key-Off).
*: For Screen          : ARKEYOFF.SPR
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : Accounts Receivable - (AR)
*: Developer Name      : Yasser Mohammed Aly - (YMA)
*: Tracking Job Number : 300697.
*:----------------------------------------------------------------
*: Called From         : 1. System Menu.
*:                       2. Cash Receipt - (Customer Payments).
*:----------------------------------------------------------------
*: Passed Parameters   : ldPDate    : Key Off Date.
*:                       lcPFYear   : Fiscal Year.
*:                       lcPFPeriod : Fiscal Period.
*:                       lcPAcc     : Customer Code.
*:                       lcPCur     : Currency Code.
*:                       lcPExR     : Currency Exchange Rate.
*:                       lcPCurUn   : Currency Unit.
*:                       lcPUnSin   : Currency Unit Sign.
*:                       lcPExRSin  : Currency Exchange Rate Sign.
*:----------------------------------------------------------------
*: Note that the transaction types used to update the "ARHIST", 
*:      "DEBIT" and "CREDIT" files are..
*:      Type  Meaning
*:      0     Returns (Credit Memo).
*:      1     Invoice, Direct Invoice.
*:      2     Debit Adjustment.
*:      3     Charge Back (Debit On Account).
*:      4     Payment.
*:      5     Credit Adjustment.
*:      6     Credit On Account.
*:      7     Allowance (Credit Adjustment in this program).
*:      8     Charge Back (Only in the ARHIST).
*:      9     Credit On Account (Only in the ARHIST).
*:----------------------------------------------------------------
*: Example             : DO ARKEYOF
*:----------------------------------------------------------------
*: Modifications
*: E301077,53 YMA 03/03/99 Opening files.
*: B602856,01 YMA 05/09/99 Fixed the bug of not generating the GL 
*: B602856,01              entries if keying off multi customers 
*: B602856,01              in the same session.
*: B602897,01 YMA 05/20/99 Added the new debit code "R" and the 
*: B602897,01              credit code "I" into consideration.
*: E301231,01 YMA 05/20/99 Eleminate creating the GL entries for 
*: E301231,01              any GL account if it add up to zero 
*: E301231,01              amount.
*: B602945,01 AHM/06/21/99 Fixing the bug of restoring current alias
*: B602945,01              in browse folder removing valid function
*: B602945,01              even the user choose cancel removing
*: B602980,01 AHM 06/21/99 Fixing the bug of wrong updating OpenAmt field
*: B602980,01              in ArHist file if credit side selected before 
*: B602980,01              debit side
*: E301231,04 AHM 06/21/99 Eleminate creating the GL entries for 
*: E301231,04              any GL account if it add up to zero amout, for
*: E301231,04              Transaction type "KO", only
*: B602583,01 AHM 06/21/99 Fix the bug of missing update glaccount field
*: B602583,01              for exchange rate gl entry
*: B802133,01 AHM 06/22/99 Fix the bug of having non AR cash records in 
*: B802133,01              credit browse if we press escape in Add mode
*: E301276,01 AHM 06/22/99 Automatic Key-OF
*: E301281,01 AHM 07/06/99 Calling Automatic key-of from cash receipt prog.
*: B802456,1  MAB 07/29/99 Define External Automatic Key-Off flag.
*: B603081,1  AHM 08/01/99 Fixing the bug of not creating KO GL entries
*: B603081,1               in case of exchange rate difference existence
*: B603191,1  MAN 08/08/99 Automatic key off :
*: B603191,1  			   extra credit records are selected to be keyed
*: B603191,1  			   off against debit records. This causes a wrong
*: B603191,1  			   calculation of customer balance.
*: E301266,1 AKA 06/23/99 Adding one more field in  APPAYMNT file (batch) and update it 
*: E301266,1              with the value of CREDIT.BATCH 
*: B603127,1 WALID ABOU EL MAGD 08/25/99 Fix the bug all transactions that made
*: B603127,1 In the same sesion take the same batch number     
*: B603254,1 RAMY 11/15/1999 In Salesrep Chargeback screen the name of the
*: B603254,1                 first and second salesrep is shown even there 
*: B603254,1                 are no salesreps selected.
*: B603254,1                 Note: Repaired in 'archbkrp.scx'
*: B603414,1 NAD 02/02/2000  Replace TranCode with cCreditcod if lcFile is the credit file
*: B603414,1                 in lfaddInAll Function
*: B603651,1 NAD 05/18/2000  Fix bug  "attemping to lock" Message is displayed in add mode
*: B603651,1                 if one then one user was using the screen
*: E301429,1 NAD 06/18/2000  Increase the amount field in key off screen.
*: E301429,1                 (ARKEYO2.SCX,ARKEYO41.SCX,ARKEYO51.SCX) 
*: B803369,1 NAD 07/20/2000  Factor code entered in customer screen doesn't show up by default. 
*: B802622,1 ABD 08/02/2000 Add the decscription of the reason code to the decscription filed by default
*: B802622,1 ABD            When the user select the reason decscription.
*: E500374,5 HBG 08/15/2000 the transaction will be posted to the A/R account of the  
*: E500374,5                Factor instead of the customer if there is a Factor code
*: B603860,1 NAD 08/30/2000 Fix bug of not updating the allownce fields in arcushst file.
*: B602838,1 NAD 09/24/2000 Not to allow transactions for hold account.
*: B803653,1 NAD 09/24/2000 Correct the spelling of word apply.
*: B802420,1 NAD 09/24/2000 Key_off date must not be less than trandate.        
*: E301509,1 AHM 11/16/2000 Add new parameter in the program and lfExterAuto function
*: E301509,1                 that showes if the calling for this program is called from EDI module
*: B803881,1 SSH 01/29/2001 Enhancement performance
*: B604363,1 SSH 04/03/2001 Restore Old Selete setting.
*: N000232,1 HS  04/12/2001 Add fields cBnkCode & cChkAcc to ARHIST file
*: B803973,1 AME 06/06/2001 Fix The bug of not showing debits/credits if we didnot press
*: B803973,1                ENTER at keyoff date.
*: B804270,1 BWA 08/13/2001 Fix the bug making transactions to the hold accounts.[START]
*: B605667,1 SSE 03/28/2002 Fix bug of Updating nCrAdj instead of nAllow in ArCusHst.
*: B606627,1 RAE 11/21/2002 The keyed off invoice still appear as an open debit in case 
*:							of "Invoice Material Sales Order"
*: B606894,1 KHM 03/18/2003 Fix the bug of getting transactions that fall later than
*: B606894,1                the keyoff date
*: B607013,1 ABD 06/16/2003 Prevent the customer to select any transaction has the posting 
*: B607013,1 ABD            Date is greater than the key off date if the company is linked to GL.  
*: B607455,1 AMH 07/27/2003 Fix the bug of position is out of screen.
*: B038431,1 ASH 08/21/2004 Fix bug of variable 'trantype' not found.
*:----------------------------------------------------------------

PARAMETERS ldPDate , lcPFYear, lcPFPeriod, lcPAcc  , lcPCur,;
           lcPExR  , lcPCurUn, lcPUnSin  , lcPExRSin, llFromEdi

llFrARCash = (PARAMETERS() = 9)        && Are we called from the cash Receipt?

DIMENSION laScrMode[4]                 && Screen modes array.
DIMENSION laFoldWinds[4,2]             && Folder screens array.
DIMENSION laShowCur[3]                 && Customer summery popup.
DIMENSION laAdjCodIn[2,10]             && Adjustment folder codes array.
DIMENSION laAccCodIn [2,10]            && On Account foldes array.
DIMENSION laDCAdj[1]                   && Adjustment reason array.
DIMENSION laDCAcc[1]                   && On Account reason array.
DIMENSION laAdjType[4]                 && Adjustment Type array.
DIMENSION laAccType[3]                 && On Account Type array.
DIMENSION laDefProc[ALEN(LADEFPROC)]   && Default procedures array.

*-- Load system setup values.
llGLLink   = (UPPER(ALLTRIM(gfGetMemVar("M_LINK_GL"  ))) = "Y")    && GL Link status.
llMultiWar = (UPPER(ALLTRIM(gfGetMemVar("M_WAREHOUSE"))) = "Y")    && Multi warehouse status.
llMltCur   = gfGetMemVar("llMulCurr")                              && Multi currency status.
llChBkREep = (OCCURS("SR", gcCmpModules)#0) AND ;
             (UPPER(ALLTRIM(gfGetMemVar("M_REPCB")))="Y")          && Charge back sales rep setup.

*-- Force local procedures to be excuted.
laDefProc[09] = .F.                    && Force local save procedure.
laDefProc[10] = .F.                    && Force local cancel procedure.

llCUpDate  = .F.                       && None of the screens objects is updated.
llAlowNew  = .F.                       && Do not allow to use the "New" button in the control pannel screen.
glFromBrow = .F.                       && I'm not comming from any browse.

*-- Folders information variables.
lcFolder     = SPACE(0)                && Folder window name.
lcwFoldChng  = SPACE(0)                && Call this function when changing the folder.
lcFoldPush   = SPACE(0)                && Folder activation button name.
laFoldWind   = SPACE(0)                && Folder screens array.
lcFoldPrnt   = SPACE(0)                && Folder screen parent.
lnActFolder  = 1                       && Active folder number.
lnFolderCEnd = 0                       && Folder column end position.
lnFolderREnd = 0                       && Folder row end position.

*-- Used windows names.
lcKOChWin2 = SPACE(0)                  && For Screen ARKEYO2 .SCX
lcKOChWin3 = SPACE(0)                  && For Screen ARKEYO3 .SCX
lcKOChWn31 = SPACE(0)                  && For Screen ARKEYO31.SCX
lcKOChWn32 = SPACE(0)                  && For Screen ARKEYO32.SCX
lcKOChWn33 = SPACE(0)                  && For Screen ARKEYO33.SCX
lcKOChWin4 = SPACE(0)                  && For Screen ARKEYO4 .SCX
lcKOChWn41 = SPACE(0)                  && For Screen ARKEYO41.SCX
lcKOChWn42 = SPACE(0)                  && For Screen ARKEYO42.SCX
lcKOChWin5 = SPACE(0)                  && For Screen ARKEYO5 .SCX
lcKOChWn51 = SPACE(0)                  && For Screen ARKEYO51.SCX
lcKOChWn52 = SPACE(0)                  && For Screen ARKEYO52.SCX
lcKOChWn53 = SPACE(0)                  && For Screen ARKEYO53.SCX
lcKOChWin6 = SPACE(0)                  && For Screen ARKEYO6 .SCX
lcKOChWn61 = SPACE(0)                  && For Screen ARKEYO61.SCX
lcKOChWn62 = SPACE(0)                  && For Screen ARKEYO62.SCX

*-- Debit browse information.
lcDebBrTtl = "Debits"                  && Debit browse title.
lnDbRecNo  = 0                         && Debit browse record number.
lcDbFltr   = SPACE(0)                  && Debit browse filter.

*-- Credit browse information.
lcCrdBrTtl = "Credits"                 && Credit browse title.
lnCdRecNo  = 0                         && Credit browse record number.
lcCrFltr   = SPACE(0)                  && Credit browse filter.

*-- Adjustment browse information.
lcDCAdjTtl = "Adjustments"             && Adjustments browse title.
lcDcBrFeld = SPACE(0)                  && Adjustments browse fields.
lnDCAdjRcN = 0                         && Adjustments browse record number.

*-- On Account browse information.
lcDCAccTtl = "Debit/Credit on Account" && On account browse title.
lnDCAccRcN = 0                         && On account browse record number.

*-- "ALL Transactions" browse information.
lcDCAllTtl = "Session's Transactions"  && All transactions browse title.
lnDCAllRcN = 0                         && All transactions browse record number.

*-- Temporary files names.
lcDCAdjTmp = SPACE(0)                  && Adjustments temporary file name.
lcDCOnATmp = SPACE(0)                  && On Account  temporary file name.
lcDCAllTmp = SPACE(0)                  && All transactions temporary file name.
lcRpComTmp = SPACE(0)                  && Rep. commission temporary file name.

lcOldValue = SPACE(0)                  && Old value for all objects.
llFrmBrow  = .F.                       && Browse button flag.

*-- Objects Status.
lcKeyStat  = "ENABLE"                  && Key objects status.
lcObjStat  = "DISABLE"                 && Normal objects status.

*-- Initialize the header variables.
ldKODate   = gdSysDate                 && The key-off date.
ldOKODate  = {}                        && Old value of the key-off date.
llValidDat = .F.                       && Is the key-off date valid ?
lcGLFYear  = SPACE(0)                  && Gl Fiscal year.
lcGLPeriod = SPACE(0)                  && Gl Fiscal Period.

lcAccount  = SPACE(5)                  && Customer code.
lcOAccount = SPACE(0)                  && Customer old code.
llValidAcc = .F.                       && Is the customer code valid ?
lcAccName  = SPACE(0)                  && Customer Name.
lcAccCity  = SPACE(0)                  && Customer City.

lcCusFact  = SPACE(0)                  && Customer default factor code.
*B803369,1 (Start) 
lcAccFact  = SPACE(0)                  && Variable to hold the factor in the debit\credit on account screen
lcFactor   = SPACE(0)                  && Variable to hold the factor in the debit\credit adjustment screen
*B803369,1 (End)
lnCusDebts = 0                         && Customer total debits.
lnCusCrdts = 0                         && Customer total credits.
lnCusNtBal = 0                         && Customer net balance.

lcCurCode  = SPACE(5)                  && Currency code.
lcOCurCode = SPACE(0)                  && Currency old code.
llValidCur = .F.                       && Is the currency code valid ?
lnExRate   = 1                         && Currency exchange rate.
lnCurrUnit = 1                         && Currency unit.
lcUntSin   = "/"                       && Currency unit sign.
lcExRSin   = "/"                       && Exchange rate sign.

lnShowCur  = 1                         && Customer summery popup variable.
laShowCur  = SPACE(0)                  && Customer summery popup array.

lnPTrans   = 0                         && Total debits selected.
lnNTrans   = 0                         && Total credits selected.
lnBalance  = 0                         && Net of the selected transactions.

*E301281,1 (Start)
llExtAuto = .F.
*E301281,1 (End)

*-- 1st folder variables
lcDbSelPrm = "Select"                  && "Select\Unselect" button prompt (Debit part)
lcCrSelPrm = "Select"                  && "Select\Unselect" button prompt (Credit part)

*- Filter check buttons variables
lcTrType   = SPACE(0)                  && Filter the transactions for these types only.
lcLNum0    = SPACE(0)                  && Low value for the "Returns" range.
lcHNum0    = SPACE(0)                  && Hi  value for the "Returns" range.
lcLNum1    = SPACE(0)                  && Low value for the "Invoices" range.
lcHNum1    = SPACE(0)                  && Hi  value for the "Invoices" range.
lcLNum2    = SPACE(0)                  && Low value for the "D. Adj." range.
lcHNum2    = SPACE(0)                  && Hi  value for the "D. Adj." range.
lcLNum3    = SPACE(0)                  && Low value for the "Charge Backs" range.
lcHNum3    = SPACE(0)                  && Hi  value for the "Charge Backs" range.
lcLNum4    = SPACE(0)                  && Low value for the "Payments" range.
lcHNum4    = SPACE(0)                  && Hi  value for the "Payments" range.
lcLNum5    = SPACE(0)                  && Low value for the "C. Adj." range.
lcHNum5    = SPACE(0)                  && Hi  value for the "C. Adj." range.
lcLNum6    = SPACE(0)                  && Low value for the "C. On acc." range.
lchNum6    = SPACE(0)                  && Hi  value for the "C. On acc." range.

*-- 2nd Folder variables.
llNoThing  = lfBldTyPop(.T., "J")      && Build the Adjustment type popup.
laAdjCodIn = SPACE(0)                  && Debit\Credit Adjustment code information array.
laDCAdj    = SPACE(0)                  && Debit\Credit Adjustment array.
lcDCAdj    = SPACE(0)                  && Debit\Credit Adjustment value.

*-- 3rd Folder variables.
llNoThing  = lfBldTyPop(.T., "C")      && Build the "on account" type popup.
lcRedClr   = "RGB(,,,255,0,0)"         && The "Red" color RGB.
lcGreenClr = "RGB(,,,0,128,0)"         && The "Green" color RGB.
lcFacTrnPr = "Factor"                  && Factor\Tran# field prompt.
lnAccDebit = 0                         && Total entered debit on account transactions.
lnAccCredt = 0                         && Total entered credit on account transactions.
lnAccEntrd = 0                         && Debit\Credit on account transactions net.
lnAccDiffr = 0                         && Difference between the net balance of the total seected transactions and the net Debit\Credit on account transactions.
laAccCodIn = SPACE(0)                  && Debit\Credit On Account code information array.
lnOAccType = 3                         && Old value of the "On Account" transaction type popup.
lcDCAcc    = SPACE(0)                  && Debit\Credit On Account value.
laDCAcc    = SPACE(0)                  && Debit\Credit On Account array.

*-- 4th Folder variables.
*-- Following variables is used as a flag to control the keyboard 
*-- trapping for the 4th folder, it will be used by one of the invisible
*-- buttons that is located in that folder.
*-- When this variable takes a .T. value that means that the invisible 
*-- button have been reached from the "Remove" button, otherwise means 
*-- that it have been reached from the browse, the behavior of the 
*-- invisible button will differ according to the value of this variable.
llFrmRemBt = .F.

*-- Codes Arrays.
*-- In this program there are two codes popups, one of them is located
*-- in the "Debit\Credit Adjustment" folder, and the other is located
*-- in the "Debit\Credit On Account" folder.
*-- Both of them display the codes from either the "TRANCODE" or the
*-- "CCREDITCOD" fields according to the record's transaction type.
*-- Since we need to deal with the two popups at the same time, that 
*-- means that one of them might has a list of the "TRANCODE" codes, and
*-- the other has a list of the "CCREDITCOD" at the same time, that
*-- means that we cannot use only one code information array to deal with
*-- the two popups at the same time.
*-- So, we will use two different code information arrays, one for the
*-- popup that is in the "Debit\Credit Adjustment" folder and we will 
*-- name it "laAdjCodIn". The other one is to handel the popup that is
*-- in the "Debit\Credit On Account" folder and we will name it 
*-- "laAccCodIn" as follows....
laAdjCodIn[1,01] = "TRANCODE" 	       && Field Name.
laAdjCodIn[1,02] = "laDCAdj"           && Array Name.
laAdjCodIn[1,03] = "lnDCAdj"           && Popup Name.
laAdjCodIn[1,04] = ""                  && Popup Status.
laAdjCodIn[1,05] = .F.                 && Include "N/A".
laAdjCodIn[1,06] = .F.                 && Include "ALL".
laAdjCodIn[1,07] = ""                  && Alternative File.
laAdjCodIn[1,08] = ""                  && Use this index for the Alternative.
laAdjCodIn[1,09] = ""                  && Seek this expression.
laAdjCodIn[1,10] = ""                  && Alternative Field Name

laAdjCodIn[2,01] = "CCREDITCOD"        && Field Name.
laAdjCodIn[2,02] = "laDCAdj"           && Array Name.
laAdjCodIn[2,03] = "lnDCAdj"           && Popup Name.
laAdjCodIn[2,04] = ""                  && Popup Status.
laAdjCodIn[2,05] = .F.                 && Include "N/A".
laAdjCodIn[2,06] = .F.                 && Include "ALL".
laAdjCodIn[2,07] = ""                  && Alternative File.
laAdjCodIn[2,08] = ""                  && Use this index for the Alternative.
laAdjCodIn[2,09] = ""                  && Seek this expression.
laAdjCodIn[2,10] = ""                  && Alternative Field Name

laAccCodIn[1,01] = "TRANCODE" 	       && Field Name.
laAccCodIn[1,02] = "laDCAcc"           && Array Name.
laAccCodIn[1,03] = "lnDCAcc"           && Popup Name.
laAccCodIn[1,04] = ""                  && Popup Status.
laAccCodIn[1,05] = .F.                 && Include "N/A".
laAccCodIn[1,06] = .F.                 && Include "ALL".
laAccCodIn[1,07] = ""                  && Alternative File.
laAccCodIn[1,08] = ""                  && Use this index for the Alternative.
laAccCodIn[1,09] = ""                  && Seek this expression.
laAccCodIn[1,10] = ""                  && Alternative Field Name

laAccCodIn[2,01] = "CCREDITCOD"        && Field Name.
laAccCodIn[2,02] = "laDCAcc"           && Array Name.
laAccCodIn[2,03] = "lnDCAcc"           && Popup Name.
laAccCodIn[2,04] = ""                  && Popup Status.
laAccCodIn[2,05] = .F.                 && Include "N/A".
laAccCodIn[2,06] = .F.                 && Include "ALL".
laAccCodIn[2,07] = ""                  && Alternative File.
laAccCodIn[2,08] = ""                  && Use this index for the Alternative.
laAccCodIn[2,09] = ""                  && Seek this expression.
laAccCodIn[2,10] = ""                  && Alternative Field Name

*E301077,53 YMA 03/03/99 Start - There is no need for these variables
*E301077,53 YMA 03/03/99 because the "gfOpenFile" will handel the file close.
*-- Files open flags...
*llOpSycCur       = .F.                 && Did we open the currency file?
*llOpnSls         = .F.                 && Did we open the sales rep file?
*llOpnCom         = .F.                 && Did we open the Rep. commission file?
*E301077,53 YMA 03/03/99 End.

*-- Following variables are manipulated in the local save procedure,
*-- they need to be defined globally just to be able to save there
*-- values in the incomplete session file.
lcAdTrnSeq = SPACE(0)                  && Debit\Credit Adjustment transaction code.
lcAcTrnSeq = SPACE(0)                  && Debit\Credit On Account transaction code.
lcHisSeq   = SPACE(0)                  && History value.
lcGlSess   = SPACE(0)                  && GL Session value.
lcRepBat   = SPACE(0)                  && Sales rep. commission batch.
llUpdGlDif = .T.                       && Did we update the GL difference yet?
llUpdMstGL = .T.                       && Did we update the master GL file yet?

*-- Following variables are used to handel the incomplete program 
*-- session concept.
lcSession  = SPACE(1)                  && Sequence number to flag the program sessions.
lnThisSess = gnProgCopy                && Current running session number.
lnUnCmSeRc = 0                         && The incomplete session record number.
lcProgID   = PADR("KEYOFF", 10)        && The program ID in the incomplete session file.
llUnComp   = .F.                       && Are we comming from an incomplete session?
llGoAndChk = .T.                       && Can we go and check the existance of any incomplete session from the "lpShow" procedure?
llChkUnCom = .T.                       && Can we check the existance of any incomplete session before drawing the screen (From the screen setup)?

*-- Variables that need to be saved and restored when detecting an
*-- incomplete program session.
DIMENSION laVars[31]
laVars[01] = "lnActFolder"
laVars[02] = "lnUnCmSeRc"
laVars[03] = "ldKODate"
laVars[04] = "lnExRate"
laVars[05] = "lnCurrUnit"
laVars[06] = "lcUntSin"
laVars[07] = "lcExRSin"
laVars[08] = "lcAccount"
laVars[09] = "lcCurCode"
laVars[10] = "lcAccName"
laVars[11] = "lcAccCity"
laVars[12] = "lnBalance"
laVars[13] = "lnPTrans"
laVars[14] = "lnNTrans"
laVars[15] = "lnCusDebts"
laVars[16] = "lnCusCrdts"
laVars[17] = "lnCusNtBal"
laVars[18] = "lcCusFact"
laVars[19] = "lnAccEntrd"
laVars[20] = "lnAccDebit"
laVars[21] = "lnAccDiffr"
laVars[22] = "lnAccCredt"
laVars[23] = "lnShowCur"
laVars[24] = "llCUpDate"
laVars[25] = "lcAdTrnSeq"
laVars[26] = "lcAcTrnSeq"
laVars[27] = "llUpdGlDif"
laVars[28] = "llUpdMstGL"
laVars[29] = "lcHisSeq"
laVars[30] = "lcGlSess"
laVars[31] = "lcRepBat"

*-- Please give me the permission to proceed running this session 
*-- of the program, if you do not want to, sorry, I'll be quitting...
IF gfSetup()

  *-- Define this variable just to avoid getting an error message
  *-- when swtich the screen mode from the control pannel screen.
  *-- Because the Control Pannel screen assumes that the program 
  *-- has a header and details file, so it scatters the header file
  *-- information useing this variable.
  lcScFields = "Account"

  *-- Empty the screen base file to avoid geeting an error when 
  *-- switching the screen modes.
  lcBaseFile = SPACE(0)

  *-- To control the "Control Panel" show.
  llNoShow   = .F.
  
  *-- Trade discount button prompt. (Debit\Credit Adjstment folder)
  lcTrDsPrm  = IIF(ALLTRIM(gcContCode)="ENG","Terms","Trade")+ ;
               SPACE(1)+"Discount..."
  
  *-- If there is another session of the program is running, please
  *-- display a message and go out of the program..
  IF gcBaseWind <> 'AAR'
    *-- You cannot open a new 'Apply Debits\Credits' screen 
    *-- while the current one is running.
    *-- < Ok >
    llGoBack = gfModalGen('TRM40125B00000') = 1
  ELSE
    *-- This function-call will assign unique values to the variables that
    *-- need to be uniquely identified per session.
    *-- It will return .T. only if this program is called from the 
    *-- "Cash Receipt" program and the customer that's passed as a parameter
    *-- is now locked (beeing keyed off by another user).
    *-- In this case a message will be displayed and the program 
    *-- will be terminated.
    llGoBack = IIF(WEXIST(gcBaseWind), .F., lfNtWnExst())
  ENDIF

  *-- If I do not want to go back to the calling program ("Cash Receipt")
  *-- then please proceed.
  IF llGoBack
    glQuitting = .T.
  ELSE
    *-- Set the used order in the invoice header file.
    *E301077,53 YMA 03/03/99 Start - Commented out and moved to the invoice valid function
    *SET ORDER TO TAG InvHdrA IN InvHdr
    *E301077,53 YMA 03/03/99 End.
    
    lnFltMode  = IIF(llFrARCash OR WEXIST(gcBaseWind), 2, 4)
    llNoThing  = lfSetFlter("Debit" , .T., lnFltMode) 
    llNoThing  = lfSetFlter("Credit", .F., lnFltMode)

    *-- Save the keyboard trapping.
    PUSH KEY

    *-- Save the active system menu.
    PUSH MENU _MSYSMENU

    *-- Define a menu bar with a short-cut key for each browse window 
    *-- in the program just to be able to activate the browse window
    *-- using the its menu bar short-cut key.
    DEFINE BAR 099 OF P01PU01 PROMPT "\-" SKIP FOR .T.
    DEFINE BAR 100 OF P01PU01 PROMPT lcDebBrTtl KEY ALT+D SKIP FOR (lnActFolder#1)
    DEFINE BAR 101 OF P01PU01 PROMPT lcCrdBrTtl KEY ALT+I SKIP FOR (lnActFolder#1)
    DEFINE BAR 102 OF P01PU01 PROMPT lcDCAdjTtl KEY ALT+J SKIP FOR (lnActFolder#2)
    DEFINE BAR 103 OF P01PU01 PROMPT lcDCAccTtl KEY ALT+A SKIP FOR (lnActFolder#3)
    DEFINE BAR 104 OF P01PU01 PROMPT lcDCAllTtl KEY ALT+L SKIP FOR (lnActFolder#4)
    ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcDebBrTtl)
    ON SELECTION BAR 101 OF P01PU01 ACTIVATE WINDOW (lcCrdBrTtl)
    ON SELECTION BAR 102 OF P01PU01 ACTIVATE WINDOW (lcDCAdjTtl)
    ON SELECTION BAR 103 OF P01PU01 ACTIVATE WINDOW (lcDCAccTtl)
    ON SELECTION BAR 104 OF P01PU01 ACTIVATE WINDOW (lcDCAllTtl)

    *E301276,01 (Start)
    DIMENSION laPanelObj[1,3]
    STORE '' TO laPanelObj
    *--Object Automatic button.
    laPanelObj[1,1] = 'pbAutoKey'
    laPanelObj[1,2] = gcBmpHome+'GENRATE.BMP'
    laPanelObj[1,3] = [VALID lfvAutoKey() MESSAGE 'Auto Key-off' DISABLE ]
    *E301276,01 (End)

    *-- Call the screen.
    *DO (gcScrDir+gcWinAppl+"\ARKeyOff.SPR")
    DO (gcScrDir+"ARKeyOff.SPR")

    *-- Return the previously saved system menu
    POP MENU _MSYSMENU

    *-- Return the previously saved keyboard trapping.
    POP KEY

    *-- Release the browses windows.
    RELEASE WINDOW (lcDebBrTtl)
    RELEASE WINDOW (lcCrdBrTtl)
    RELEASE WINDOW (lcDCAdjTtl)
    RELEASE WINDOW (lcDCAccTtl)
    RELEASE WINDOW (lcDCAllTtl)
    
    *-- Release the menu bars.
    RELEASE BAR 099 OF P01PU01
    RELEASE BAR 100 OF P01PU01
    RELEASE BAR 101 OF P01PU01
    RELEASE BAR 102 OF P01PU01
    RELEASE BAR 103 OF P01PU01
    RELEASE BAR 104 OF P01PU01

    llNoThing  = lfSetFlter("Debit" , .T., 1)          && Reset the debits filter.
    llNoThing  = lfSetFlter("Credit", .F., 1)          && Reset the credit filter.
    llNoThing  = IIF(glQuitting, lfClsData(), .T.)     && If end of session; close, restore the temp files.
  ENDIF
ENDIF  

*:----------------------------------------------------------------
*: Name       : lfNtWnExst
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Assign unique values to the variables that
*:              need to be uniquely identified per session.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : .T. -> If this program is called from the 
*:                     "Cash Receipt" program and the passed
*:                     customer is now beeing keyed off.
*:              .F. -> Otherwise.
*:----------------------------------------------------------------
*: Example    : llGoOut=IIF(WEXIST(gcBaseWind), .F., lfNtWnExst())
*:----------------------------------------------------------------
FUNCTION lfNtWnExst

*-- None of the screen's objects has been updated.
llCUpDate  = .F.
  
*-- Assign a unique window name for each of the defined windows.
lcKOChWin2 = gfTempName()
lcKOChWin3 = gfTempName()
lcKOChWn31 = gfTempName()
lcKOChWn32 = gfTempName()
lcKOChWn33 = gfTempName()
lcKOChWin4 = gfTempName()
lcKOChWn41 = gfTempName()
lcKOChWn42 = gfTempName()
lcKOChWin5 = gfTempName()
lcKOChWn51 = gfTempName()
lcKOChWn52 = gfTempName()
lcKOChWn53 = gfTempName()
lcKOChWin6 = gfTempName()
lcKOChWn61 = gfTempName()
lcKOChWn62 = gfTempName()

*-- Assign a unique temporary name for each of the used files.
lcDCAdjTmp = gfTempName()
lcDCOnATmp = gfTempName()
lcDCAllTmp = gfTempName()
lcRpComTmp = gfTempName()
  
*-- Initialize the folders variables.
lcwFoldChng      = "= lfActFolder()"
lcFoldPush       = "pbFolder"
lnFolderCEnd     = 102.50
lnFolderREnd     = 2.00
lcFolder         = gfTempName()
lcFoldPrnt       = gcBaseWind
lnActFolder      = 1

*-- The folders window names and titles.
laFoldWinds[1,1] = "Debits<->Credits"
laFoldWinds[1,2] = lcKOChWin3
laFoldWinds[2,1] = "Debit/Credit Adj."
laFoldWinds[2,2] = lcKOChWin4
laFoldWinds[3,1] = "Debit/Credit on Acc."
laFoldWinds[3,2] = lcKOChWin5
laFoldWinds[4,1] = "Browse"
laFoldWinds[4,2] = lcKOChWin6


*-- Get a unique session number, to be used to update the incomplete
*-- session file.
lcSession = gfsequence('CSESSION')


*-- I'm not comming from an incomplete session of the program.
llUnComp  = .F.

*-- Open the currency file if it is not opened.
*E301077,53 YMA 03/03/1999 Start - Commented out and moved to the proper function.
*IF llMltCur
*  llOpSycCur = gfOpenFile(gcSysHome+"SycCurr","cCurrCode",'SH')
*ENDIF
*E301077,53 YMA 03/03/1999 End.

*-- Default the header variables either from the customer
*-- payment program if it is the calling program or
*-- balnk them if called from the menu.
llGoBack  = .F.
IF llFrARCash
  llCUpDate    = .T.
  ldKODate     = CTOD(ldPDate)
  lcGLFYear    = lcPFYear
  lcGLPeriod   = lcPFPeriod
  lnExRate     = VAL(lcPExR)
  lnCurrUnit   = VAL(lcPCurUn)
  lcUntSin     = lcPUnSin
  lcExRSin     = lcPExRSin
  lcAccount    = lcPAcc
  lcCurCode    = lfDefACur(lcPCur)
  llGoBack     = !lfvAccount(.F.)
  IF !llGoBack        
    llGoBack   = !lfLokCus(lcAccount, .T.)
    llValidAcc = .T.
    llValidCur = .T.
    llValidDat = .T.
    lnShowCur  = 1
    llNoThing  = lfBldShow(lcCurCode)
    laScrMode  = .F.
    laScrMode[4] = .T.
  ENDIF
ELSE
  *-- Set the currency variables according to the setting of
  *-- multi currency.
  IF llMltCur
    lcCurCode  = SPACE(1)
    lnExRate   = 1
    lnCurrUnit = 1
  ELSE
    lcCurCode  = gcBaseCurr
    lnExRate   = 1
    lnCurrUnit = 1
    lcUntSin   = '/'
    lcExRSin   = '/'
  ENDIF
ENDIF

RETURN (llGoBack)

*:----------------------------------------------------------------
*: Name       : lfDefACur
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : To validate and default a passed currency code.
*:----------------------------------------------------------------
*: Parameters : lcCur -> Currency code that needs to be defaulted.
*:----------------------------------------------------------------
*: Returns    : The currency code actualy defaulted.
*:----------------------------------------------------------------
*: Example    : lcCurCode = lfDefACur("USDLR")
*:----------------------------------------------------------------
FUNCTION lfDefACur
PARAMETERS lcCur

*E301077,53 YMA 03/03/1999 Start - Open the currency file.
IF llMltCur
  = gfOpenFile(gcSysHome+"SycCurr","cCurrCode",'SH')
ENDIF
*E301077,53 YMA 03/03/1999 End.

*-- If the passed curency code is valid...
IF llMltCur AND SEEK(lcCur, "SycCurr")
  lcCurCode  = lcCur            && Return it..
  llNoThing  = lfStCurInf()     && Set the currency related information.
ELSE
  lcCurCode  = gcBaseCurr       && Default the base currency.
ENDIF

RETURN (lcCurCode)

*:----------------------------------------------------------------
*: Name       : lfClsData
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Close and restore the temporary created files.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfClsData()
*:----------------------------------------------------------------
FUNCTION lfClsData

*-- Close and erase the temporary "Adjustments" file.
USE IN (lcDCAdjTmp)
ERASE (gcWorkDir+lcDCAdjTmp+".DBF")
ERASE (gcWorkDir+lcDCAdjTmp+".CDX")
ERASE (gcWorkDir+lcDCAdjTmp+".FPT")

*-- Close and erase the temporary "On Account" file.
USE IN (lcDCOnATmp)
ERASE (gcWorkDir+lcDCOnATmp+".DBF")
ERASE (gcWorkDir+lcDCOnATmp+".CDX")
ERASE (gcWorkDir+lcDCOnATmp+".FPT")

*-- Close and erase the temporary "All Transactions" file.
USE IN (lcDCAllTmp)
ERASE (gcWorkDir+lcDCAllTmp+".DBF")
ERASE (gcWorkDir+lcDCAllTmp+".CDX")
ERASE (gcWorkDir+lcDCAllTmp+".FPT")

*-- Close and erase the temporary "Sales rep commission" file.
IF FILE(gcWorkDir+lcRpComTmp+".DBF") 
  IF USED(lcRpComTmp)
    USE IN (lcRpComTmp)
  ENDIF
  ERASE (gcWorkDir+lcRpComTmp+"DBF")
  ERASE (gcWorkDir+lcRpComTmp+"CDX")
  ERASE (gcWorkDir+lcRpComTmp+"FPT")
ENDIF

*-- Close the master files opened by this program.
*E301077,53 YMA 03/03/99 Start - the gfOpenFile will handel this.
*USE IN IIF(llOpSycCur                   , "SycCurr" , 0)
*USE IN IIF(llOpnSls AND USED("SalesRep"), "SalesRep", 0)
*USE IN IIF(llOpnCom AND USED("RepComm") , "RepComm" , 0)
*E301077,53 YMA 03/03/99 End.

*:----------------------------------------------------------------
*: Name       : lpShow
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : To control all the screen objects values and status
*:              in all the screen modes.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : DO lpShow
*:----------------------------------------------------------------
FUNCTION lpShow

DO CASE
  *-- Select Mode.
  CASE laScrMode[1]
    *B603127,1 make this variable empty each time we enter the select mode [Start]
    lcRepBat   = SPACE(0)
    *B603127,1  [End..]

    IF llGoAndChk AND lfChkUnComS()
      RETURN
    ENDIF
    
    *-- Rest all the used variables.
    *B602856,1 05/09/99 YMA To fix the bug of not generating the GL entries
    *B602856,1 05/09/99 YMA if keying off multi customers in the same 
    *B602856,1 05/09/99 YMA session.
    STORE SPACE(0) TO lcAdTrnSeq , lcAcTrnSeq , lcHisSeq   , lcGlSess   ,;
                      lcRepBat
    STORE .T.      TO llUpdMstGL , llUpdGlDif
    *B602856,1 05/09/99 YMA End.
    
    STORE 0        TO lnBalance  , lnPTrans   , lnNTrans   , lnCusDebts ,;
                      lnCusCrdts , lnCusNtBal , lnAccEntrd , lnAccDebit ,;
                      lnAccDiffr , lnAccCredt
    STORE SPACE(1) TO lcAccount  , lcAccName  , lcAccCity  , lcTrType   ,;
                      lcLNum0    , lcLNum1    , lcLNum2    , lcLNum3    ,;
                      lcLNum4    , lcLNum5    , lcLNum6    , lcHNum0    ,;
                      lcHNum1    , lcHNum2    , lcHNum3    , lcHNum4    ,;
                      lcHNum5    , lchNum6    , lcDbFltr   , lcCrFltr
    *B803369,1 (Start) clear the variables that hold the factor
    STORE SPACE(0) TO lcFactor ,lcAccFact,lcCusFact
    *B803369,1 (End)
    STORE .F.      TO cbDbFilter , cbCrFilter , llValidDat , llValidAcc ,;
                      llValidCur
    
       
    *E301281,1 (Start)
    llExtrAuto = .F.
    *E301281,1 (End)
    
    *-- Reset the header variables.
    lcCurCode  = IIF(llMltCur, SPACE(1), gcBaseCurr)
    lnExRate   = 1
    lnCurrUnit = 1
    lcUntSin   = "/"
    lcExRSin   = "/"
    ldKODate   = gdSysDate
    lnShowCur  = 1
    llNoThing  = lfBldShow(SPACE(0))    && Empty the customer summery popup.
    SHOW GET lnShowCur
    
    *-- Default the "Select\Unselect" filter buttons.
    lcDbSelPrm = "Select"
    lcCrSelPrm = "Select"
    
    *-- Enable\Disable the objects in the screen.
    lcObjStat  = "DISABLE"
    lcKeyStat  = "ENABLE"
    SHOW GETS WINDOW (lcKOChWin2) DISABLE ONLY
    SHOW GET ldKODate   &lcKeyStat
    SHOW GET lcAccount  &lcKeyStat
    SHOW GET lcCurCode  &lcKeyStat
    SHOW GET pbBrAcct   &lcKeyStat
    SHOW GET pbBrCurr   &lcKeyStat

    *E301276,01 (Start)
      SHOW GET pbAutoKey DISABLE
    *E301276,01 (End)
    
    *-- Default the folders variables.
    llNoThing  = lfBldTyPop(.T., "J")
    llNoThing  = lfBldTyPop(.T., "C")
    lnAdjType  = 3
    lnAccType  = 4
    lnOAccType = 1
    lcDCAdj    = laAdjCodIn[1,1]
    lcDCAcc    = laAccCodIn[1,1]
    llNoThing  = gfwCodePop (@laAdjCodIn, lcDCAdj, "N")
    llNoThing  = gfwCodePop (@laAccCodIn, lcDCAcc, "N")

    *-- Activate the key-off date.
    ACTIVATE WINDOW (lcKOChWin2)
    _CUROBJ   = OBJNUM(ldKODate)

    *B802133 (Start)
    llNoThing  = lfSetFlter("Debit" , .T., 4) 
    llNoThing  = lfSetFlter("Credit", .F., 4)
    *B802133 (End)

  *-- View Mode. (There is no view mode in this program).
  CASE laScrMode[2]

  *-- Edit Mode.  (There is no edit mode in this program).
  CASE laScrMode[3]

  *-- Add Mode.
  CASE laScrMode[4]
  
    *-- Create an "Open" record in the incomplete session file.
    llNoThing  = IIF(lnUnCmSeRc=0, lfAdUnCmSR(), .T.) 

    *-- If not comming from incomplete session then refill the 
    *-- customer summery popup.
    IF !llUnComp
      lnShowCur  = 1
      lcOldValue = 0 
      llNoThing  = lfvSumPop()
    ENDIF
    
    *-- Enable\Disable the objects in the screen.
    llGoAndChk = .T.
    lcObjStat  = "ENABLE"
    lcKeyStat  = "DISABLE"
    SHOW GETS WINDOW (lcKOChWin2) DISABLE ONLY
    SHOW GET lnShowCur &lcObjStat
    
    *E301276,01 (Start)
      SHOW GET pbAutoKey ENABLE
    *E301276,01 (End)
ENDCASE

*-- The "lfActFolder()" function is normally called from the spr
*-- right after activating the screen, and again it is called 
*-- when refreshing the screen for the first time using the refresh
*-- snippet of the spr read command.
*-- This causes the screen to flicker when it is showed for the
*-- for the first time.
*-- To avoid this flicker we used the "llFromSPR" flag which will tell
*-- that we are comming from the SPR or not, and if we are, than we 
*-- will not refresh the folders.
llNoThing = IIF(llFromSPR, .T., lfActFolder()) 

*:----------------------------------------------------------------
*: Name       : lfActFolder
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Control the changing of the folder screens.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfActFolder()
*:----------------------------------------------------------------
FUNCTION lfActFolder
PRIVATE lnAlias

*-- If comming from the SPR then reset the flag and do nothing, 
*-- otherwise refresh the active folder objects.
IF llFromSPR
  llFromSPR = .F.
ELSE  
  *-- For each active folder, disable all the objects in the other 
  *-- folders, enable the trapping invisible buttons in the active
  *-- folder, and refresh its browses.
  lnAlias = SELECT(0)
  DO CASE
    CASE lnActFolder = 1
      SHOW GETS WINDOW (lcKOChWn41) DISABLE ONLY
      SHOW GETS WINDOW (lcKOChWn51) DISABLE ONLY
      SHOW GETS WINDOW (lcKOChWn62) DISABLE ONLY
      SHOW GET ibTrap1  ENABLE
      SHOW GET ibTrap12 ENABLE
      llNoThing = lfwDbBrow()
      llNoThing = lfwCdBrow()

    CASE lnActFolder = 2
      SHOW GETS WINDOW (lcKOChWn33) DISABLE ONLY
      SHOW GETS WINDOW (lcKOChWn51) DISABLE ONLY
      SHOW GETS WINDOW (lcKOChWn62) DISABLE ONLY
      SHOW GET ibTrap2  ENABLE
      llNoThing = lfwDCAdjBr()
  
    CASE lnActFolder = 3
      SHOW GETS WINDOW (lcKOChWn33) DISABLE ONLY
      SHOW GETS WINDOW (lcKOChWn41) DISABLE ONLY
      SHOW GETS WINDOW (lcKOChWn62) DISABLE ONLY
      SHOW GET ibTrap3 ENABLE
      llNoThing = lfwDCAccBr() AND lfRefresh(lcKOChWn52)
  
    CASE lnActFolder = 4
      SHOW GETS WINDOW (lcKOChWn33) DISABLE ONLY
      SHOW GETS WINDOW (lcKOChWn41) DISABLE ONLY
      SHOW GETS WINDOW (lcKOChWn51) DISABLE ONLY
      SHOW GET ibTrap4  ENABLE
      SHOW GET ibTrap42 ENABLE

      *-- Since that the validation of some of the objects in the other
      *-- folders modify the record pointer in the "All Transactions" 
      *-- temp file, Delete records and add records, we need to be sure
      *-- that the record pointer in this file is pointing to the correct
      *-- record number before showing the browse (Activating the folder).
      SELECT (lcDCAllTmp)
      LOCATE
      IF lnDCAllRcN>0 AND lnDCAllRcN<=RECCOUNT()
        GOTO lnDCAllRcN
      ENDIF
      IF BOF() OR EOF() OR DELETED()
        GOTO TOP
      ENDIF
      llNoThing = lfwAlBrow()
  ENDCASE
  SELECT(lnAlias)
ENDIF

*-- Refresh the memo field in the incomplete session file with the 
*-- value of the active folder number that will be used to activate 
*-- the folder that was active in the incomplete session of the program.
llNoThing = lfUpdVars()

*:----------------------------------------------------------------
*: Name       : lfUpdBal
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Update and refresh the total selected 
*:              Debits\Credits and net balance objects that are
*:              located in the header part of the screen.
*:----------------------------------------------------------------
*: Parameters : lcType : "P" for positive numbers (Debits).
*:                       "N" for nigative numbers (Credits).
*:              lnQty  : The amount that needs to be updated.
*:              lnSign : -1 Means to subtract the amount from the
*:                          existing amount.
*:                       +1 Means to add the amount to the existing
*:                          amount.
*:              llRef  : .T. Means to refresh the objects on the 
*:                           screen after the modification.
*:                       .F. Means not to refresh the objects on the 
*:                           screen after the modification.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfUpdBal("P", 10, -1, .T.)
*:----------------------------------------------------------------
FUNCTION lfUpdBal
PARAMETERS lcType, lnQty, lnSign, llRef

*-- Get the variable name that needs to be updated according 
*-- to the "Type" sent to the function.
*-- This variable will either be "lnPTrans" or "lnNTrans"
lcVar      = "ln" + lcType + "Trans"

*-- Update the variable.
&lcVar     = ABS(&lcVar)   + (lnSign*ABS(lnQty))

*-- Update the balance amount.
lnBalance  = ABS(lnPTrans) - ABS(lnNTrans)

*-- Update the difference variable that is located in the 
*-- Debit\Credit on account folder.
lnAccDiffr = lnBalance     - lnAccEntrd
lnNTrans   = -1            * ABS(lnNTrans)

*E301281,1 (Start)
IF !llExtAuto
*E301281,1 (End)

  *-- Refresh the screen if needed.
  llNoThing  = IIF(llRef, lfRefresh(lcKOChWin2), .T.)

  *-- Update the incomplete session record with the new values.
  llNoThing  = lfUpdVars()

*E301281,1 (Start)
ENDIF
*E301281,1 (End)

*:----------------------------------------------------------------
*: Name       : lfBrDebits
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Debit Browse.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfBrDebits()
*:----------------------------------------------------------------
FUNCTION lfBrDebits
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT Debit
*B802420,1 NAD 09/24/2000 (Start) Key_off date must not be less than trandate.  
*BROWSE FIELDS cMarker=IIF(RECNO()=lnDbRecNo,'>',''):1:R:H='':W=.F.,;
              cSelctd=IIF(SEEK(Account+TranType+Tran+cInstalNo,lcDCAllTmp),'»',''):1:R:H='':W=.F.,;
              Tran     :08:R:H='Tran #'            ,;
              Desc     :20:R:H='Description'       ,;
              Amount   :11:R:H='Amount'            ,;
              TranDate    :R:H='Tran. Date'        ,;
              Store       :R:H='Store'             ,;
              Reference   :R:H='Reference'         ,;
              cInstalNo   :R:H='Installment Number' ;
       WINDOW (lcKOChWn31) IN WINDOW (lcKOChWin3) ;
       LOCK 0 SAVE NOWAIT NOEDIT NOCLEAR NOAPPEND NODELETE ;
       NOMENU TITLE (lcDebBrTtl) WHEN lfwDbBrow() VALID :F lfvBrow()
       
BROWSE FIELDS cMarker=IIF(RECNO()=lnDbRecNo,'>',''):1:R:H='':W=.F.,;
              cSelctd=IIF(SEEK(Account+TranType+Tran+cInstalNo,lcDCAllTmp),'»',''):1:R:H='':W=.F.,;
              Tran     :08:R:H='Tran #'            ,;
              Desc     :20:R:H='Description'       ,;
              Amount   :11:R:H='Amount'            ,;
              TranDate    :R:H='Tran. Date'        ,;
              Store       :R:H='Store'             ,;
              Reference   :R:H='Reference'         ,;
              cInstalNo   :R:H='Installment Number' ;
              FOR  TranDate <=  ldKODate            ; 
       WINDOW (lcKOChWn31) IN WINDOW (lcKOChWin3) ;
       LOCK 0 SAVE NOWAIT NOEDIT NOCLEAR NOAPPEND NODELETE ;
       NOMENU TITLE (lcDebBrTtl) WHEN lfwDbBrow() VALID :F lfvBrow()

*B802420,1 NAD (End)
SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfwDbBrow
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Debit Browse "When" clause.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfwDbBrow()
*:----------------------------------------------------------------
FUNCTION lfwDbBrow
PRIVATE lnAlias

*-- Select the "Debit" file and update the record pointer variable.
lnAlias = SELECT(0)
SELECT Debit
lnDbRecNo  = RECNO()

*-- If there is no records displayed in the browse, default the 
*-- "Select\Unselect" button to display "Select", and disable
*-- the debit control pannel objects.
*-- Otherwise, adjust the "Select\Unselect" button according to the
*-- records status (selected or not), and disable\enable the debit 
*-- control pannel objects according to the screen mode.
IF EOF() OR BOF()
  lcDbSelPrm = "Select"
  lcStatus   = "DISABLE"
ELSE
  lcDbSelPrm = IIF(SEEK(Account+TranType+Tran+cInstalNo,lcDCAllTmp) ,;
               "Unselect", "Select")
  lcStatus   = IIF(laScrMode[4], "ENABLE", "DISABLE")
ENDIF

*-- Determine the status of the debit filter check box according
*-- to the screen mode and the filter variable.
lcFltStat  = IIF(laScrMode[4], "ENABLE", "DISABLE")
cbDbFilter = !EMPTY(lcDbFltr)

*-- Refresh the debit control pannel objects.
SHOW GET pbDbSelect ,1 PROMPT lcDbSelPrm
SHOW GET pbDbSelect &lcStatus
SHOW GET pbDbSelAll &lcStatus
SHOW GET pbDbInvert &lcStatus
SHOW GET pbDbSelNon &lcStatus
SHOW GET cbDbFilter &lcFltStat

*-- Refresh the browse window.
SHOW WINDOW (lcDebBrTtl) REFRESH
SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfBrCrdits
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Credit Browse.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfBrCrdits()
*:----------------------------------------------------------------
FUNCTION lfBrCrdits
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT Credit
*B802420,1 NAD 09/24/2000 (Start) Key_off date must not be less than trandate.
*BROWSE FIELDS cMarker=IIF(RECNO()=lnCdRecNo,'>',''):1:R:H='':W=.F.,;
              cSelctd=IIF(SEEK(Account+TranType+Tran,lcDCAllTmp),'»',''):1:R:H='':W=.F.,;
              Tran    :08:R:H='Tran #'           ,;
              Desc    :20:R:H='Description'      ,;
              Amount  :11:R:H='Amount'           ,;
              TranDate   :R:H='Tran. Date'       ,;
              Store      :R:H='Store'            ,;
              Reference  :R:H='Reference'         ;
       WINDOW (lcKOChWn32) IN WINDOW (lcKOChWin3) ;
       LOCK 0 SAVE NOWAIT NOEDIT NOCLEAR NOAPPEND ;
       NODELETE NOMENU TITLE (lcCrdBrTtl) WHEN lfwCdBrow() ;
       VALID :F lfvBrow()


BROWSE FIELDS cMarker=IIF(RECNO()=lnCdRecNo,'>',''):1:R:H='':W=.F.,;
              cSelctd=IIF(SEEK(Account+TranType+Tran,lcDCAllTmp),'»',''):1:R:H='':W=.F.,;
              Tran    :08:R:H='Tran #'           ,;
              Desc    :20:R:H='Description'      ,;
              Amount  :11:R:H='Amount'           ,;
              TranDate   :R:H='Tran. Date'       ,;
              Store      :R:H='Store'            ,;
              Reference  :R:H='Reference'         ;
              FOR  TranDate <=  ldKODate          ;    
       WINDOW (lcKOChWn32) IN WINDOW (lcKOChWin3) ;
       LOCK 0 SAVE NOWAIT NOEDIT NOCLEAR NOAPPEND ;
       NODELETE NOMENU TITLE (lcCrdBrTtl) WHEN lfwCdBrow() ;
       VALID :F lfvBrow()
*B802420,1 NAD (End)
SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfwCdBrow
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Credit Browse "When" clause.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfwCdBrow()
*:----------------------------------------------------------------
FUNCTION lfwCdBrow
PRIVATE lnAlias

*-- Select the "Credit" file and update the record pointer variable.
lnAlias = SELECT(0)
SELECT Credit
lnCdRecNo  = RECNO()

*-- If there is no records displayed in the browse, default the 
*-- "Select\Unselect" button to display "Select", and disable
*-- the credit control pannel objects.
*-- Otherwise, adjust the "Select\Unselect" button according to the
*-- records status (selected or not), and disable\enable the credit
*-- control pannel objects according to the screen mode.
IF EOF() OR BOF()
  lcCrSelPrm = "Select"
  lcStatus   = "DISABLE"
ELSE
  lcCrSelPrm = IIF(SEEK(Account+TranType+Tran,lcDCAllTmp), "Unselect", "Select")
  lcStatus   = "ENABLE"
ENDIF

*-- Determine the status of the credit filter check box according
*-- to the screen mode and the filter variable.
lcFltStat  = IIF(laScrMode[4], "ENABLE", "DISABLE")
cbCrFilter = !EMPTY(lcCrFltr)

*-- Refresh the credit control pannel objects.
SHOW GET pbCrSelect ,1 PROMPT lcCrSelPrm
SHOW GET pbCrSelect &lcStatus
SHOW GET pbCrSelAll &lcStatus
SHOW GET pbCrInvert &lcStatus
SHOW GET pbCrSelNon &lcStatus
SHOW GET cbCrFilter &lcFltStat

*-- Refresh the browse window.
SHOW WINDOW (lcCrdBrTtl) REFRESH
SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfBrAdjust
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Adjustments Browse.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfBrAdjust()
*:----------------------------------------------------------------
FUNCTION lfBrAdjust
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT (lcDCAdjTmp)
lcDcBrFeld = "cMarker=IIF(RECNO()=lnDCAdjRcN,'>',''):01:R:H='':W=.F.,"+;
             "cTyp   =SUBSTR('Debit Adj. Credit Adj.N/A',((ATC(TranType,'27 ')-1)*11)+1,11):11:R:H='Type',"+;
             "cReason  :10:R:H='Reason',"              +;
             "Desc     :10:R:H='Description',"         +;
             "Amount   :11:R:H='Amount',"              +;
             "Reference   :R:H='Reference'"            +;
             IIF(llGLLink, ",dPostDate:R:H='Post Date'", "")

BROWSE FIELDS &lcDcBrFeld WINDOW (lcKOChWn42) IN WINDOW (lcKOChWin4) ;
       LOCK 0 SAVE NOWAIT NOEDIT NOCLEAR NOAPPEND NODELETE NOMENU    ;
       TITLE (lcDCAdjTtl) WHEN lfwDCAdjBr() VALID :F lfvBrow()
SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfwDCAdjBr
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Adjustments Browse "When" clause.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfwDCAdjBr()
*:----------------------------------------------------------------
FUNCTION lfwDCAdjBr
PRIVATE lnAlias

*-- Select the "Adjustments" file and update its record pointer 
*-- variable.
lnAlias = SELECT(0)
SELECT (lcDCAdjTmp)
lnDCAdjRcN = RECNO()

*-- If the browse is empty (No records).
IF BOF() OR EOF()
  *-- Let the "Type" popup show N/A.
  lnAdjType  = 3

  *-- Let the "Reason" popup show N/A.
  lcDCAdj    = laAdjCodIn[1,1]
  llNoThing  = gfwCodePop (@laAdjCodIn, lcDCAdj, "N")
  lnDCAdj    = 1
  
  *-- Blank out the rest of the objects.
  lcAdjRef   = SPACE(0)
  lcAdjDes   = SPACE(0)
  ldAdjPDate = {}
  lnAdjAmont = 0
  cbCharBakS = .F.
  
  *-- The "New" button status is according to the screen mode, all the
  *-- other objects are disable.
  lcNewStst  = IIF(laScrMode[4], "ENABLE", "DISABLE")
  STORE "DISABLE" TO lcStat, lcTypStst, lcRemStat, lcChrBkSt, lcTrDisSt
ELSE
  *-- Determine the trasnaction type...
  lnAdjType  = ATC(TranType, "27 ")
  
  *-- If the transaction type is "N\A"..
  IF lnAdjType = 3
    *-- Let the "Reason" popup show N/A.
    lcDCAdj    = laAdjCodIn[1,1]
    llNoThing  = gfwCodePop (@laAdjCodIn, lcDCAdj, "N")
    lnDCAdj    = 1
    
    *-- Uncheck the "Charge Back Sales Rep" check box.
    cbCharBakS = .F.
    
    *-- The "Type" popup status is according to the screen mode.
    lcTypStst  = IIF(laScrMode[4], "ENABLE", "DISABLE")
    
    *-- Disable all the other objects.
    STORE "DISABLE" TO lcStat, lcTrDisSt
  ELSE
    *-- If the transaction type is either "Debit" or "Credit" 
    *-- Adjustment, then....
    
    *-- Let the reason popup show the records value.
    lcDCAdj    = laAdjCodIn[lnAdjType,1]
    llNoThing  = gfwCodePop(@laAdjCodIn, lcDCAdj, "V,"+TranCode)
    
    *-- Check\Uncheck the "Charge Back Sales Rep" check box.
    llSeekCond = USED(lcRpComTmp)
    cbCharBakS = IIF(llSeekCond, SEEK(&lcDCAdjTmp..Tran, lcRpComTmp), .F.)
    
    *-- Disable the key field (Transaction Type), and enable\Diable 
    *-- the rest of the edit region objects according to the screen 
    *-- mode.
    lcTypStst  = "DISABLE"
    lcStat     = IIF(laScrMode[4], "ENABLE", "DISABLE")
    lcTrDisSt  = lcStat
  ENDIF
  *-- Detrmine the status of the "Charge Back Sales Rep" check box, 
  *-- the "Remove" and "New" buttons.
  lcChrBkSt  = IIF(lnAdjType=2 AND Amount#0, "ENABLE", "DISABLE")
  lcRemStat  = IIF(laScrMode[4]            , "ENABLE", "DISABLE")
  lcNewStst  = IIF(laScrMode[4]            , "ENABLE", "DISABLE")
  
  *-- Load the objects values from the file.
  lcAdjRef   = Reference
  lcAdjDes   = Desc
  ldAdjPDate = dPostDate
  lnAdjAmont = Amount
  *B803369,1 (Start) restore the factore code from the browse
  IF !EMPTY(TranType)
    lcFactor   = cFacCode
  ENDIF
  *B803369,1 (End) 
ENDIF

*-- Refresh the objects.
SHOW GET lnAdjType  &lcTypStst
SHOW GET lnDCAdj    &lcStat
SHOW GET lcAdjRef   &lcStat
SHOW GET lcAdjDes   &lcStat
SHOW GET lnAdjAmont &lcStat
SHOW GET ldAdjPDate &lcStat
SHOW GET pbCharBakS &lcStat
SHOW GET pbAdjRem   &lcRemStat
SHOW GET pbAdjNew   &lcNewStst
SHOW GET pbTradeDis &lcTrDisSt
SHOW GET cbCharBakS &lcChrBkSt
*B803369,1 (Start) Refresh the factor field.
SHOW GET PbBrFactr  &lcStat
SHOW GET lcFactor   &lcStat
*B803369,1 (End) 

*-- Refresh the browse window.
SHOW WINDOW (lcDCAdjTtl) REFRESH

SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfBrOnAcc
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Debit\Credit On Account Browse.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfBrOnAcc()
*:----------------------------------------------------------------
FUNCTION lfBrOnAcc
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT (lcDCOnATmp)
BROWSE FIELDS cMarker=IIF(RECNO()=lnDCAccRcN,'>',''):01:R:H='':W=.F.,;
              cTyp   =SUBSTR("Debit On Acc. Credit On Acc.Open "    +;
              "Balance  N/A"                                        ,;
              ((ATC(TranType,"36* ")-1)*14)+1,14):14:R:H='Type'     ,;
              cReason  :10:R:H='Reason'                             ,;
              Desc     :10:R:H='Description'                        ,;
              Amount   :11:R:H='Amount'                             ,;
              TranDate    :R:H='Tran. Date'                         ,;
              Store       :R:H='Store'                              ,;
              Reference   :R:H='Reference'                          ,;
              cFacCode    :R:H='Factor'                              ;
       WINDOW (lcKOChWn53) IN WINDOW (lcKOChWin5)                    ;
       LOCK 0 SAVE NOWAIT NOEDIT NOCLEAR NOAPPEND                    ;
       NODELETE NOMENU TITLE (lcDCAccTtl) WHEN lfwDCAccBr()          ;
       VALID :F lfvBrow()
SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfwDCAdjBr
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Debit\Credit On Account Browse "When" clause.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfwDCAdjBr()
*:----------------------------------------------------------------
FUNCTION lfwDCAccBr
PRIVATE lnAlias

*-- Select the "Debit\Credit On Account" file and update its 
*-- record pointer variable.
lnAlias = SELECT(0)
SELECT (lcDCOnATmp)
lnDCAccRcN = RECNO()

*-- If the browse is empty (No records).
IF BOF() OR EOF()
  *-- Let the "Type" popup show N/A.
  lnAccType  = 4

  *-- Let the "Reason" popup show N/A.
  lcDCAcc    = laAccCodIn[1,1]
  llNoThing  = gfwCodePop (@laAccCodIn, lcDCAcc, "N")
  lnDCAcc    = 1

  *-- Blank out the rest of the objects.
   *B803369,1 (Start) not to clear the factor
   *STORE SPACE(0)  TO lcAccStore, lcAccRef , lcAccDes,;
                     lnAccAmont, lcAccFact, lcAccTran
                     
   STORE SPACE(0)  TO lcAccStore, lcAccRef , lcAccDes,;
                     lnAccAmont,  lcAccTran
   *B803369,1 (End) 
  ldAccDate  = {}
  
  *-- Disable all the edit region objects.
  STORE "DISABLE" TO lcTypStst, lcStat, lcRemStat, lcSharSta
  
  *-- The "New" button status depends on the screen mode.
  lcNewStst  = IIF(laScrMode[4], "ENABLE", "DISABLE")
  
  *-- Default the "Factor\Tran#" field title to "Factor".
  lcFacTrnPr = "Factor"
ELSE
  *-- Determine the trasnaction type...
  lnAccType  = ATC(TranType, "36* ")
  
  DO CASE
    *-- If the transaction type is either "Debit" or "Credit"
    *-- On Account...
    CASE BETWEEN(lnAccType,1,2)
      lcDCAcc    = laAccCodIn[lnAccType,1]
      llNoThing  = gfwCodePop(@laAccCodIn, lcDCAcc, "V,"+TranCode)
      lcTypStst  = "DISABLE"
      lcStat     = "ENABLE"
      lcSharSta  = "ENABLE"
      lcFacTrnPr = "Factor"

    *-- If the transaction type is "Open Balance"
    CASE lnAccType = 3
      lcDCAcc    = laAccCodIn[1,1]
      llNoThing  = gfwCodePop(@laAccCodIn, lcDCAcc, "N")
      lcTypStst  = "DISABLE"
      lcStat     = "DISABLE"
      lcSharSta  = "ENABLE"
      lcFacTrnPr = "Tran. #"

    *-- If the transaction type is "N\A".
    CASE lnAccType = 4
      lcDCAcc    = laAccCodIn[1,1]
      llNoThing  = gfwCodePop(@laAccCodIn, lcDCAcc, "N")
      lcTypStst  = "ENABLE"
      lcStat     = "DISABLE"
      lcSharSta  = "DISABLE"
      lcFacTrnPr = "Factor"
  ENDCASE
  lcNewStst  = IIF(laScrMode[4], "ENABLE", "DISABLE")
  lcRemStat  = IIF(laScrMode[4], "ENABLE", "DISABLE")

  *-- Load the objects values from the file.
  ldAccDate  = TranDate
  lcAccStore = Store
  lcAccRef   = Reference
  lcAccDes   = Desc
  lnAccAmont = Amount
  *B803369,1 (Start) get the factor code from the temp file.
  *lcAccFact=cFacCode
  IF !EMPTY(TranType)
    lcAccFact = cFacCode
  ENDIF
  *B803369,1 (End) 
  lcAccTran  = cOpnBalTrn
ENDIF

*-- Refresh the objects.
SHOW GET lnAccType  &lcTypStst
SHOW GET lnDCAcc    &lcStat
SHOW GET ldAccDate  &lcStat
SHOW GET lcAccStore &lcStat
SHOW GET lcAccRef   &lcStat
SHOW GET lcAccDes   &lcStat
SHOW GET lnAccAmont &lcSharSta
SHOW GET pbAccNew   &lcNewStst
SHOW GET pbAccRem   &lcRemStat

*-- The two objects "lcAccFact" and "lcAccTran" is overlapped, so
*-- it's important to show the transaction's type corresponding
*-- object above the other one...
IF lnAccType = 3
  *-- If the transactoin type in "Open Balance" then display the
  *-- transaction number field above the factor field.
  SHOW GET lcAccFact  &lcStat
  SHOW GET lcAccTran  DISABLE
  SHOW GET pbBrFact   DISABLE
ELSE
  *-- Otherwise, display the factor field above the transaction number 
  *-- field.
  SHOW GET lcAccTran  DISABLE
  SHOW GET lcAccFact  &lcStat
  SHOW GET pbBrFact   &lcStat
ENDIF

*-- Refresh the browse window.
SHOW WINDOW (lcDCAccTtl) REFRESH

*-- Refresh the screens SAYS.
= lfRefresh(lcKOChWn51)

SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfBrAll
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : "All Selected Transactions" Browse.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfBrAll()
*:----------------------------------------------------------------
FUNCTION lfBrAll
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT (lcDCAllTmp)
BROWSE FIELDS cMarker=IIF(RECNO()=lnDCAllRcN,'>',' '):1:R:H=' ':W=.F.,;
              cTranNo=IIF(TranType='7'OR(TranType='2' AND Deb_Adj='Y'), SPACE(0), Tran):08:R:H='Tran #',;
              Desc     :20:R:H='Description'     ,;
              Amount   :11:R:H='Amount'          ,;
              TranDate    :R:H='Tran. Date'      ,;
              Store       :R:H='Store'           ,;
              Reference   :R:H='Reference'        ;
       WINDOW (lcKOChWn61) IN WINDOW (lcKOChWin6) ;
       LOCK 0 SAVE NOWAIT NOEDIT NOCLEAR NOAPPEND ;
       NODELETE NOMENU TITLE (lcDCAllTtl) WHEN lfwAlBrow() ;
       VALID :F lfvBrow()
SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfwAlBrow
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : "All Selected Transactions" Browse "When" clause.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfwAlBrow()
*:----------------------------------------------------------------
FUNCTION lfwAlBrow
PRIVATE lnAlias

*-- Select the "All Selected Transactions" file and update its 
*-- record pointer variable.
lnAlias = SELECT(0)
SELECT (lcDCAllTmp)
lnDCAllRcN = RECNO()

*-- Determine the status of the "Remove" button according to
*-- the screen mode and the number of records in the file.
lcRemStat  = IIF(EOF() OR BOF(), "DISABLE", ;
             IIF(laScrMode[4], "ENABLE", "DISABLE"))
SHOW GET pbAllRem &lcRemStat

*-- Resresh the browse window.
SHOW WINDOW (lcDCAllTtl) REFRESH
SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfvBrow
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for all the browses in the screen..
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfvBrow()
*:----------------------------------------------------------------
FUNCTION lfvBrow

*-- Call the global function "gfStopBrow" going out of one of the 
*-- screen's browses.
IF !INLIST(WONTOP(), lcDebBrTtl, lcCrdBrTtl, lcDCAdjTtl,;
                     lcDCAccTtl, lcDCAllTtl)
  glFromBrow = .T.
  llNoThing  = gfStopBrow()
ENDIF

*:----------------------------------------------------------------
*: Name       : lfTrap
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Read Deactivate function, used to trap the 
*:              keyboard keys when activating one of the screen's
*:              browses.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfTrap()
*:----------------------------------------------------------------
FUNCTION lfTrap

*-- Set the global flag "glFromBrow" to true only if one of the
*-- screen's browses is active.
glFromBrow = INLIST(WONTOP(), lcDebBrTtl, lcCrdBrTtl,;
                   lcDCAdjTtl, lcDCAccTtl, lcDCAllTtl)

*-- If any of the screen's browses is active then trap the 
*-- Tab, ShiftTab, Ctrl+Enter, Ctrl+Home and Ctrl+End keys.
IF glFromBrow
  ON KEY LABEL TAB     DO lfTab
  ON KEY LABEL BACKTAB DO lfShtTab
  ON KEY LABEL Ctrl+ENTER lnDummy = 1
  ON KEY LABEL Ctrl+HOME  lnDummy = 1
  ON KEY LABEL Ctrl+END   lnDummy = 1
ENDIF  

*:----------------------------------------------------------------
*: Name       : lfClrTrap
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Read Activate function, used to untrap the 
*:              keyboard keys when going out of one of the 
*:              screen's browses.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfClrTrap()
*:----------------------------------------------------------------
FUNCTION lfClrTrap

*-- If going out of one of the browses, call the global function
*-- to stop the browse and reset the global flag.
IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF

*-- If none of the screen's browses is active then clear the 
*-- trapped keys.
IF !INLIST(WONTOP(), lcDebBrTtl, lcCrdBrTtl,;
          lcDCAdjTtl, lcDCAccTtl, lcDCAllTtl)
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL Ctrl+ENTER
  ON KEY LABEL Ctrl+HOME 
  ON KEY LABEL Ctrl+END  
ENDIF

*:----------------------------------------------------------------
*: Name       : lfTab
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : The "Tab" key trapping that will be excuted 
*:              whenever one of the browses is active.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfTab()
*:----------------------------------------------------------------
FUNCTION lfTab

DO CASE
  *-- If we are in the "Debit" browse..
  CASE WONTOP() = lcDebBrTtl
    IF laScrMode[1]
      *-- And we are in the select mode, Activate the "Debit" browse.
      KEYBOARD "{ALT+I}" PLAIN
    ELSE
      *-- And we are in the add mode, select the "Debit Filter" check
      *-- box.
      ACTIVATE WINDOW (lcKOChWn33)
      _CUROBJ = OBJNUM(cbDbFilter)
    ENDIF
    
  *-- If we are in the "Credit" browse..
  CASE WONTOP() = lcCrdBrTtl
    IF laScrMode[1]
      *-- And we are in the select mode, go to the control pannel.
      ACTIVATE WINDOW gwcContrl1
      _CUROBJ = OBJNUM(pbcptask)
    ELSE
      *-- And we are in the add mode, select the "Credit Filter" check
      *-- box.
      ACTIVATE WINDOW (lcKOChWn33)
      _CUROBJ = OBJNUM(cbCrFilter)
    ENDIF
    
  *-- If we are in the "D\C Adjustment" browse..
  CASE WONTOP() = lcDCAdjTtl
    *-- Go to the control pannel.
    ACTIVATE WINDOW gwcContrl1
    _CUROBJ = IIF(laScrMode[1], OBJNUM(pbcptask), OBJNUM(pbSav))

  *-- If we are in the "D\C On Account" browse..
  CASE WONTOP() = lcDCAccTtl
    *-- Go to the control pannel.
    ACTIVATE WINDOW gwcContrl1
    _CUROBJ = IIF(laScrMode[1], OBJNUM(pbcptask), OBJNUM(pbSav))
    
  *-- If we are in the "All selected transactions" browse..
  CASE WONTOP() = lcDCAllTtl
    IF laScrMode[1] 
      *-- And we are in the select mode, Go to the control pannel.
      ACTIVATE WINDOW gwcContrl1
      _CUROBJ = OBJNUM(pbcptask)
    ELSE
      IF BOF(lcDCAllTmp) OR EOF(lcDCAllTmp)
        *-- And we are in the add mode, and the file is empty
        *-- Go to the control pannel.
        ACTIVATE WINDOW gwcContrl1
        _CUROBJ = OBJNUM(pbSav)
      ELSE
        *-- And we are in the add mode, and the file is not empty
        *-- Go to the "Remove" button.
        ACTIVATE WINDOW (lcKOChWn62)
        _CUROBJ = OBJNUM(pbAllRem)
      ENDIF
    ENDIF
ENDCASE

*:----------------------------------------------------------------
*: Name       : lfShtTab
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : The "Shift-Tab" key trapping that will be excuted 
*:              whenever one of the browses is active.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfShtTab()
*:----------------------------------------------------------------
FUNCTION lfShtTab

DO CASE
  *-- If we are in the "Debit" browse..
  CASE WONTOP() = lcDebBrTtl
    *-- Activate the "folder" button..
    ACTIVATE WINDOW (lcFolder)
    _CUROBJ = OBJNUM(ibFolder[1])
    
  *-- If we are in the "Credit" browse..
  CASE WONTOP() = lcCrdBrTtl
    IF laScrMode[1]
      *-- And we are in the select mode, activate the "Debit" browse.
      KEYBOARD "{ALT+D}" PLAIN
    ELSE
      *-- And we are in the add mode, go to the filter button if the 
      *-- fiule is empty or otherwise to the select none button.
      ACTIVATE WINDOW (lcKOChWn33)
      _CUROBJ = IIF(BOF("Debit") OR EOF("Debit"),;
                OBJNUM(cbDbFilter), OBJNUM(pbDbSelNon))
    ENDIF

  *-- If we are in the "D\C Adjustment" browse..
  CASE WONTOP() = lcDCAdjTtl
    IF laScrMode[1]
      *-- And we are in the select mode, Activate the "folder" button..
      ACTIVATE WINDOW (lcFolder)
      _CUROBJ = OBJNUM(ibFolder[2])
    ELSE
      *-- And we are in the add mode, activate the "New" button if the
      *-- file is empty, otherwise go to the "Remove" button.
      ACTIVATE WINDOW (lcKOChWn41)
      _CUROBJ = IIF(BOF(lcDCAdjTmp) OR EOF(lcDCAdjTmp), ;
                OBJNUM(pbAdjNew), OBJNUM(pbAdjRem))
    ENDIF

  *-- If we are in the "D\C On Account" browse..
  CASE WONTOP() = lcDCAccTtl
    IF laScrMode[1]
      *-- And we are in the select mode, Activate the "folder" button..
      ACTIVATE WINDOW (lcFolder)
      _CUROBJ = OBJNUM(ibFolder[3])
    ELSE
      *-- And we are in the add mode, activate the "New" button if the
      *-- file is empty, otherwise go to the "Remove" button.
      ACTIVATE WINDOW (lcKOChWn51)
      _CUROBJ = IIF(BOF(lcDCOnATmp) OR EOF(lcDCOnATmp), ;
                OBJNUM(pbAccNew), OBJNUM(pbAccRem))
    ENDIF

  *-- If we are in the "All Selected Transactions" browse..
  CASE WONTOP() = lcDCAllTtl
    *-- Activate the "folder" button..
    ACTIVATE WINDOW (lcFolder)
    _CUROBJ = OBJNUM(ibFolder[4])
ENDCASE

*:----------------------------------------------------------------
*: Name       : lfLokCus
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Lock\Unlock a specific customer in the customer 
*:              file.
*:----------------------------------------------------------------
*: Parameters : lcCusCode : Customer code that needs to be 
*:                          locked\Unlocked.
*:              llLock    : .T. -> Lock
*:                          .F. -> Un-Lock
*:----------------------------------------------------------------
*: Returns    : .T.-> The function was able to accomplish the task.
*:              .F.-> The function was not able to accomplish the 
*:                    task.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfLokCus(lcCustCode, .T.)
*:----------------------------------------------------------------
FUNCTION lfLokCus
PARAMETERS lcCusCode, llLock
PRIVATE lnAlias, llRetVal

lnAlias  = SELECT(0)
llRetVal = .F.
SELECT Customer

*-- IF the customer record exist in the customer file.
IF SEEK("M"+lcCusCode)

  llRetVal = gfObj_Lock(llLock)

*  IF Flag = "L" AND lcRepWith = "L"
*    *-- If the customer record is already locked, and we are
*    *-- trying to re-lock it, dislay a message and return false.
*    *-- "Account 'XXXXX' is being keyed off by another."
*    *-- < Ok >
*    lcMsg      = lcCusCode + "|" + SPACE(1)
*    llValidAcc = gfModalGen('TRM40087B00000',"ALERT",lcMsg) = 5
*  ELSE
*    *-- Lock\Unlock the customer...
*    *-- and return true.
*    = RLOCK()
*    REPLACE Flag WITH lcRepWith
*    UNLOCK
*    llRetVal   = .T.
*  ENDIF
ENDIF
SELECT(lnAlias)

RETURN (llRetVal)

*:----------------------------------------------------------------
*: Name       : lfvKey
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Check if the three key fields (key-off date, 
*:              customer code, and currency code are valid or not
*:              and if yes, switch the screen mode to the "Add" 
*:              mode..
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfvKey()
*:----------------------------------------------------------------
FUNCTION lfvKey
PRIVATE lnAlias

*-- If the three key fields are valid..
IF llValidDat AND llValidAcc AND IIF(llMltCur, llValidCur, .T.)
  *-- If I can lock the customer record in the customer file,
  *-- go and lock it..
  IF lfLokCus(lcAccount, .T.)
    lnAlias   = SELECT(0)
    llCUpDate = .T.
  
    *-- Be sure to activate the debit filter in the debit file..
    llNoThing  = lfSetFlter("Debit" , .T., 2)
    SELECT Debit
    LOCATE

    *-- Be sure to activate the credit filter in the credit file..
    llNoThing  = lfSetFlter("Credit", .F., 2)
    SELECT Credit
    LOCATE
    SELECT(lnAlias)
  
    *-- Swtich the screen mode to the "Add" mode..
    laScrMode    = .F.
    laScrMode[4] = .T.
    SHOW GETS
  ELSE
    *-- If I could not lock the customer, then go to the customer code
    *-- object.
    _CUROBJ = OBJNUM(lcAccount)
  ENDIF
ENDIF

*:----------------------------------------------------------------
*: Name       : lfvDate
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Key-Off date valid function.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : @ 10,10 GET ldDate VALID lfvDate()
*:----------------------------------------------------------------
FUNCTION lfvDate
PRIVATE llRetVal

*-- If the user pressed "Enter" and did not use the mouse..
*-- We usually use this condition to validate all the key fields..
*B803973,1 AME [START]  Do Valid when leaving the field in any case
*B803973,1 AME          Not only in case of press ENTER 
*IF LASTKEY() = 13 AND !MDOWN()
IF WONTOP() <> 'GWCCONTRL1'  && IF PRESS ANY CONTROL KEY.
*B803973,1 AME [END]
  *-- Chech and update the fiscal year and period.
  
  llValidDat = CheckPrd(ldKODate,"lcGLFYear","lcGLPeriod",'KO')
  
  IF llValidDat
    *-- if the date is valid, go and check the resat of the key 
    *-- fields "Customer code, and currency code".
    = lfvKey()
    = gfUpdate()
  ELSE
    *-- if the date is invalid, remain in the date field.
    ldKODate = ldOKODate
    _CUROBJ  = _CUROBJ
  ENDIF  
ENDIF


*:----------------------------------------------------------------
*: Name       : lfvAccount
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Customer code valid function.
*:----------------------------------------------------------------
*: Parameters : llFrmScrn : .T. -> Called from ther object on the 
*:                                 screen.
*:                          .F. -> Called from the program.
*:----------------------------------------------------------------
*: Returns    : If the "Key-Off" program is called from the 
*:              "Cash Receipt" program and the customer code that is
*:              sent to the program as a parameter is beeing keyed 
*:              off at the same time, the function will display a 
*:              message and return false, otherwise it will return 
*:              true.
*:----------------------------------------------------------------
*: Example    : @ 10,10 GET lcAccount VALID lfvAccount()
*:----------------------------------------------------------------
FUNCTION lfvAccount
PARAMETERS llFrmScrn
PRIVATE lnAlias, llRetVal

llRetVal = .T.

*E301077,53 YMA 03/03/99 Start - Open the cusomer file.
= gfOpenFile(gcDataDir+"Customer","Customer",'SH')
*E301077,53 YMA 03/03/99 End.

*-- If the user pressed "Enter" and did not use the mouse or the
*-- function is called from the program (not from the screen object).
*B803973,4 AME [START]  Do Valid when leaving the field in any case
*B803973,4 AME          Not only in case of press ENTER 
*IF (llFrmScrn AND LASTKEY() = 13 AND !MDOWN()) OR !llFrmScrn
IF (llFrmScrn AND WONTOP() <> 'GWCCONTRL1' .AND. !(MDOWN() .AND. WONTOP() = lcKOChWin2)) OR !llFrmScrn
*B803973,4 AME [END]
  lnAlias = SELECT(0)
  SELECT Customer
  
  *-- If the customer browse button is pressed or the code is
  *-- not empty.
  IF llFrmBrow OR !EMPTY(lcAccount)
    
    *-- Browse the customer file if you are asked to, or if the 
    *-- customer code is invalid.
    IF llFrmBrow OR !SEEK("M"+lcAccount)
      xAccount = lcAccount
      DO CusBrowM WITH xAccount
      lcAccount = xAccount
      llFrmBrow = .F.
    ENDIF
  
    *-- If the customer code empty..
    IF EMPTY(lcAccount)
      *-- Remain in customer code object.
      lcAccount = lcOAccount
      _CUROBJ   = _CUROBJ
    ENDIF
  ENDIF
  
  *-- Load the customer related information and set the validity flag
  *-- for the customer code, and refresh the screen.
  
  llNoThing  = lfAccInfo(lcAccount)
  llValidAcc = !EMPTY(lcAccount) AND SEEK("M"+lcAccount, "Customer") ;
               AND EMPTY(Customer.Flag)
 
  
  llNoThing  = IIF(WEXIST(gcBaseWind), lfRefresh(lcKOChWin2), .T.) 
  
  SELECT(lnAlias)
ENDIF  

RETURN (llRetVal)

*:----------------------------------------------------------------
*: Name       : lfAccInfo
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Set\Load the customer code related information.
*:----------------------------------------------------------------
*: Parameters : lcAccCode : Please Set\Load the related 
*:                          information for this customer code.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfAccInfo()
*:----------------------------------------------------------------
FUNCTION lfAccInfo
PARAMETERS lcAccCode
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT Customer

IF EMPTY(lcAccCode) OR !SEEK("M"+lcAccCode)
  *-- If the customer code is empty or it is not valid, blank all
  *-- the customer related information.
  lnCusDebts = 0
  lnCusCrdts = 0
  lnCusNtBal = 0
  lcAccName  = SPACE(1)
  lcAccCity  = SPACE(1)
  lcCusFact  = SPACE(1)

  
ELSE

  *B602838,1 NAD 09/24/2000 (Start) Not to allow transaction for hold accounts 
  *B804270,1 BWA 08/13/2001 Fix the bug making transactions to the hold accounts.[START]
  *IF Customer.Status <>'A'
  *  *Message : 40022
  *  *Non active account . Cannot proceed.
  *  *Button : 00000
  *  *Ok
  *  =gfModalGen('TRM40172B00000','ALERT')
  *  lcAccount = lcOAccount
  *  _CUROBJ = _CUROBJ
  *  RETURN  
  *ENDIF
  *B804270,1 BWA 08/13/2001.[END]
  *B602838,1 NAD 09/24/2000 (End)

  *-- If the customer code is valid, load its information from 
  *-- the customer file.
  lnCusDebts = Customer.TotAge
  lnCusCrdts = Customer.OpenCr
  lnCusNtBal = Customer.TotAge + Customer.OpenCr
  lcAccName  = Customer.BtName
  lcAccCity  = Customer.cAddress32
  lcCusFact  = Customer.cFacCode  
  
  *-- If we are called from the "Cash Receipt" program and the
  *-- currence code is not empty, then default the currence code
  *-- with the default customer currence.
  IF !llFrARCash AND llMltCur AND EMPTY(lcCurCode)
    lcCurCode  = lfDefACur(Customer.cCurrCode)
    llWaitForC = .T.
  ENDIF
ENDIF

SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfvCurCode
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Currency code valid function.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : @ 10,10 GET lcCurr VALID lfvCurCode()
*:----------------------------------------------------------------
FUNCTION lfvCurCode
PRIVATE lcCode

*-- If the user pressed "Enter" and did not use the mouse,
*B803973,4 AME [START]  Do Valid when leaving the field in any case
*B803973,4 AME          Not only in case of press ENTER 
*IF LASTKEY() = 13 AND !MDOWN()
IF WONTOP() <> 'GWCCONTRL1' .AND. !(MDOWN() .AND. WONTOP() = lcKOChWin2) && IF PRESS ANY CONTROL KEY.
  PRIVATE lnAlias
  lnAlias = SELECT(0)
*B803973,4 AME [END]

  *E301077,53 YMA 03/03/1999 Start - Open the currency file.
  = gfOpenFile(gcSysHome+"SycCurr","cCurrCode",'SH')
  *E301077,53 YMA 03/03/1999 End.

  llWntToBrw = .F.                && Did we go to the browse ?
  llSelFrmBr = .F.                && Did we select from the browse ?
  
  *-- If the currency browse button is pressed or the code is
  *-- not empty and not valid, Browse all the available currencys.
  IF llFrmBrow OR (!EMPTY(lcCurCode) AND !SEEK(lcCurCode, "SycCurr"))
    llWntToBrw = .T.                      && Yes we went to the browse
    llBrowse   = .T.
    llSelFrmBr = gfCurrBrow(@lcCurCode)   && Did we select from the browse ?
    IF !llSelFrmBr
      lcCurCode = lcOCurCode
      _CUROBJ   = _CUROBJ
    ENDIF
    llFrmBrow  = .F.
  ENDIF
  
  IF !EMPTY(lcCurCode) AND SEEK(lcCurCode, "SycCurr")
    *-- If the user selected a valid currency code, load the currency
    *-- related information, build the customer summery popup, and set
    *-- the currency validity flag.
    llNoThing  = lfStCurInf()
    llNoThing  = lfBldShow (lcCurCode)
    llValidCur = IIF(llWntToBrw, llSelFrmBr, .T.)
  ELSE
    *-- If the user did not select a currency code, Blank the customer 
    *-- summery popup, and reset the currency validity flag.
    llNoThing  = lfBldShow (SPACE(0))
    llValidCur = .F.
  ENDIF
  
  *-- Refresh the screen.
  llNoThing = IIF(WEXIST(gcBaseWind) , lfRefresh(lcKOChWin2), .T.)   
  *B803973,4 AME [Start] restore old alias
  SELECT (lnAlias)
  *B803973,4 AME [End]
ENDIF

*:----------------------------------------------------------------
*: Name       : lfStCurInf
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Set\Load the currency code related information.
*:----------------------------------------------------------------
*: Parameters : lcAccCode : Please Set\Load the related 
*:                          information for this currency code.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfStCurInf()
*:----------------------------------------------------------------
FUNCTION lfStCurInf

*-- Check and fill the currency exchange rate, the unit sign
*-- and the exchange rate sign according to the selected
*-- currency code.
lcUntSin   = SPACE(1)
lnExRate   = gfChkRate('lnCurrUnit', lcCurCode , ldKODate , .T. ,;
                       gcAct_Comp , gcBaseCurr, .F.)
lcCurCode  = IIF(lnExRate = 0, gcBaseCurr, lcCurCode)
lnExRate   = IIF(lnExRate = 0, 1, lnExRate)
lcExRSin   = gfGetExSin(@lcUntSin, lcCurCode)

*:----------------------------------------------------------------
*: Name       : lfvSumPop
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Customer summery popup valid function.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfvSumPop()
*:----------------------------------------------------------------
FUNCTION lfvSumPop
PRIVATE lnAlias

*-- If the popup value changed...
IF lcOldValue # lnShowCur
  *-- Blank the customer total debits\credits\net balance.
  STORE 0 TO lnCusDebts, lnCusCrdts, lnCusNtBal
  
  *-- If the user selects to show "All Transactions in Base Currency"
  IF lnShowCur = 1
    IF SEEK("M"+lcAccount, "Customer")
      *-- Go to the customer record and load the totals.
      lnCusDebts = Customer.TotAge
      lnCusCrdts = Customer.OpenCr
    ENDIF
  ELSE
    *-- If the user selects to show "XXXXXXXXX Transaction"
    *-- Re-open the debit and the credit files in other aliases
    lnAlias = SELECT(0)
    USE (gcDataDir+"Debit" ) AGAIN IN 0 ALIAS DBFile
    USE (gcDataDir+"Credit") AGAIN IN 0 ALIAS CRFile

    *-- Accumulate all the customer's debits.
    SELECT DBFile
    SUM ALL Amount TO lnCusDebts                               ;
        FOR Account+Tran+cInstalNo+DTOS(TranDate) = lcAccount  ;
        AND cCurrCode                             = (lcCurCode)
    
    *-- Accumulate all the customer's credits.
    SELECT CRFile
    SUM ALL Amount TO lnCusCrdts                     ;
        FOR Account+Tran+DTOS(TranDate) = lcAccount  ;
        AND cCurrCode                   = (lcCurCode)
    
    *-- If the user selects to show "Equivalent XXXXXXXXX Transactions".
    IF lnShowCur = 3
      *-- Convert the accumulated totals to there equvilant amount.
      lnCusDebts = ROUND(lnCusDebts &lcExRSin lnExRate &lcUntSin lnCurrUnit, 2)
      lnCusCrdts = ROUND(lnCusCrdts &lcExRSin lnExRate &lcUntSin lnCurrUnit, 2)
    ENDIF
    
    *-- Be sure to close the opend files.
    USE IN DBFile
    USE IN CRFile
    SELECT(lnAlias)
  ENDIF
  
  *-- Compute the net balance and refresh the screen.
  lnCusNtBal = lnCusDebts + lnCusCrdts
  llNoThing  = lfRefresh(lcKOChWin2)
ENDIF

*-- Update the incomplete session record with the updated variables.
llNoThing  = lfUpdVars()

*:----------------------------------------------------------------
*: Name       : lfBldShow
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Build the customer summery popup.
*:----------------------------------------------------------------
*: Parameters : lcCodeToUse : This is the currency code to be used
*:                            to fill the popup, if it is empty
*:                            the popup will only show...
*:                            "All Transactions in Base Currency".
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfBldShow()
*:----------------------------------------------------------------
FUNCTION lfBldShow
PARAMETERS lcCodeToUse

IF EMPTY(lcCodeToUse)
  *-- If the currency code is empty, remove all the bars in the 
  *-- popup and show only "All Transactions in Base Currency"
  DIMENSION laShowCur[1]
  laShowCur[1] = "All Transactions in Base Currency"
ELSE
  *-- Fill the popup.
  lnArSize = IIF(lcCodeToUse == gcBaseCurr, 2, 3)
  DIMENSION laShowCur[lnArSize]
  laShowCur[1] = "All Transactions in Base Currency"
  laShowCur[2] = lcCurCode + " Transaction"
  IF !(lcCodeToUse == gcBaseCurr)
    laShowCur[3] = "Equivalent " + lcCodeToUse + " Transactions"
  ENDIF  
ENDIF

*:----------------------------------------------------------------
*: Name       : lfvSelect
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Select\Unselect buttons valid function.
*:----------------------------------------------------------------
*: Parameters : lcToWork : "D" -> Work with the debit file please.
*:                         "C" -> Work with the credit file please.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfvSelect("D")
*:----------------------------------------------------------------
FUNCTION lfvSelect
*E301276,01 (Start)
*PARAMETERS lcToWork
PARAMETERS lcToWork,llAutoKey
PRIVATE llAutoKey
*E301276,01 (End)

PRIVATE lcFile, lnAlias

*-- Determine the file name to use, and the total variable
*-- to be updated when Select\Unselect records.
lcFile  = IIF(lcToWork = "D", "Debit", "Credit")
lcType  = IIF(lcToWork = "D", "P", "N")

*B607013,1 ABD - Prevent the customer to select any transaction has the posting 
*B607013,1 ABD - Date is greater than the key off date if the company is linked to GL.  [Begin]
*B038431,1 ASH 08/21/04 (Begin) Fix bug of variable 'trantype' not found by adding &lcFile.. to the trantype.
IF lcToWork $ "DC" .AND. llGLLink .AND. &lcFile..Trantype $ '10' .AND.;
  IIF(lcToWork = "D",Debit.dPostDate,Credit.dPostDate) > ldKODate
*B038431,1 ASH 08/21/04 (End)
  *-- Message text :- The posting date of transaction #  ð exceeds the key off date. Cannot select this transaction.
  *-- Message No.  :- 40185
  *-- Buttom Text  :- OK
  *-- Buttom No.   :- OOOO
  = gfModalGen("TRM40185BOOOO" , "DIALOG",IIF(lcToWork = "D",Debit.Tran,Credit.Tran))
  RETURN
ENDIF
*B607013,1 ABD - [End]


*B602980 (Start)
PRIVATE lnDCSig
lnDCSig = IIF(lcToWork='D',+1,-1)
*B602980 (End)

lnAlias = SELECT(0)

*-- The debit file has the field "cInstalNo" in its index, so we
*-- have to use it to know if the current record is currently
*-- selected or not.
lcInst = IIF(UPPER(lcFile)="DEBIT", &lcFile..cInstalNo, SPACE(0)) 

*-- Flag to force the function "lfRemOpen" to display a message
*-- if the current selected record that needs to be unselected
*-- has an open balance.
llShow = .T.

lcExp     = &lcFile..Account+&lcFile..TranType+&lcFile..Tran+lcInst
llNoThing = IIF(SEEK(lcExp, lcDCAllTmp) AND &lcDCAllTmp..cShToOpn = "N",;
                lfRemOpen(.T.), .F.)
SELECT (lcDCAllTmp)
IF SEEK(lcExp)
  *-- If the record exists in the "All Selected Transactions" temp
  *-- file, that means that it is selected and we are going to 
  *-- unselect it...
  BLANK
  DELETE 
  
  llNoThing = lfUpdBal(lcType, &lcFile..Amount, -1, .T.)
ELSE
  *-- If the record does not exist in the "All Selected Transactions" temp
  *-- file, that means that it is not selected and we are going to 
  *-- select it...

  *B602980 (Start)
  = lfUpdBal(lcType, &lcFile..Amount, +1, .T.)
  *= lfUpdBal(lcType, &lcFile..Amount, lnDCSig * +1, .T.)
  *B602980 (End)
  
  = lfaddInAll(lcFile)
ENDIF

SELECT (lnAlias)

*E301281,1 (Start)
IF !llExtAuto
*E301281,1 (End)
  *-- Refresh the browse window, and activate it.
  llNoThing = IIF(lcToWork = "D", lfwDbBrow(), lfwCdBrow())
  lcBrToGo  = IIF(lcToWork = "D", "{ALT+D}" , "{ALT+I}")
*E301281,1 (Start)
ENDIF
*E301281,1 (End)

*E301276,01 (Start)
IF !llAutoKey
*E301276,01 (End)
  KEYBOARD lcBrToGo+"{DNARROW}"+"{DNARROW}" PLAIN
*E301276,01 (Start)
ENDIF
*E301276,01 (End)

*:----------------------------------------------------------------
*: Name       : lfvSelAll
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : The "Select All" buttons valid function.
*:----------------------------------------------------------------
*: Parameters : lcToWork : "D" -> Work with the debit file please.
*:                         "C" -> Work with the credit file please.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfvSelAll("D")
*:----------------------------------------------------------------
FUNCTION lfvSelAll

PARAMETERS lcToWork

PRIVATE lcFile, lnAlias, lnTmpRcNo

*-- Determine the file name to use, and the total variable
*-- to be updated when selecting all records.
lcFile  = IIF(lcToWork = "D", "Debit", "Credit")
lcType  = IIF(lcToWork = "D", "P", "N")
lnAlias = SELECT(0)

*-- Save the debit\credit record pointer.
SELECT (lcFile)
lnTmpRcNo = RECNO()

*-- The debit file has the field "cInstalNo" in its index, so we
*-- have to use it to know if the current record is currently
*-- selected or not.
lcInst = IIF(UPPER(lcFile)="DEBIT", &lcFile..cInstalNo, SPACE(0)) 

*-- Go through all the not selected records and select them.

*B606894,1 KHM 03/18/2003 (Begin) Get the transactions that fall withing the keyoff date.
*SCAN FOR !SEEK(Account+TranType+Tran+lcInst, lcDCAllTmp)
SCAN FOR !SEEK(Account+TranType+Tran+lcInst, lcDCAllTmp) AND TranDate <=  ldKODate
*B606894,1 KHM 03/18/2003 (End)

  *B607013,1 ABD - Prevent the customer to select any transaction has the posting 
  *B607013,1 ABD - Date is greater than the key off date if the company is linked to GL.  [Begin]
  IF lcToWork $ "DC" .AND. llGLLink .AND. (Trantype $ '10').AND.;
    IIF(lcToWork = "D",Debit.dPostDate,Credit.dPostDate) > ldKODate
    *-- Message text :- The posting date of transaction #  ð exceeds the key off date. Cannot select this transaction.
    *-- Message No.  :- 40185
    *-- Buttom Text  :- OK
    *-- Buttom No.   :- OOOO
    = gfModalGen("TRM40185BOOOO" , "DIALOG",IIF(lcToWork = "D",Debit.Tran,Credit.Tran))
    LOOP
    ENDIF
  *B607013,1 ABD - [End]

  WAIT WINDOW "Processing Transaction Number : " + Tran NOWAIT
  = lfUpdBal(lcType, &lcFile..Amount, +1, .T.)
  = lfaddInAll(lcFile)
ENDSCAN
WAIT CLEAR

*-- Adjust the Debit\Credit file pointer to the previously saved record 
*-- number.
IF lnTmpRcNo > 0 AND lnTmpRcNo < RECCOUNT()
  GOTO lnTmpRcNo
ELSE
  GOTO TOP
ENDIF

*E301281,1 (Start)
IF !llExtAuto
*E301281,1 (End)

  *-- Refresh the browse window, and activate it.
  llNoThing = IIF(lcToWork = "D", lfwDbBrow(), lfwCdBrow())
  lcBrToGo  = IIF(lcToWork = "D", "{ALT+D}" , "{ALT+I}")
  KEYBOARD lcBrToGo PLAIN

*E301281,1 (Start)
ENDIF
*E301281,1 (End)

SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfvInvert
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : The "Invert" buttons valid function.
*:----------------------------------------------------------------
*: Parameters : lcToWork : "D" -> Work with the debit file please.
*:                         "C" -> Work with the credit file please.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfvInvert("D")
*:----------------------------------------------------------------
FUNCTION lfvInvert
PARAMETERS lcToWork
PRIVATE lcFile, lnAlias, lnTmpRcNo

*-- Determine the file name to use, and the total variable
*-- to be updated when selecting all records.
lcFile  = IIF(lcToWork = "D", "Debit", "Credit")
lcType  = IIF(lcToWork = "D", "P", "N")

*-- Flag to force the function "lfRemOpen" to display a message
*-- if the current selected record that needs to be unselected
*-- has an open balance.
llShow  = .T.

*-- Save the debit\credit record pointer.
lnAlias = SELECT(0)
SELECT (lcFile)
lnTmpRcNo = RECNO()

*-- Go through all the records
*B606894,1 KHM 03/18/2003 (Begin) Get the transactions that fall withing the keyoff date.
*SCAN
SCAN FOR TranDate <=  ldKODate
*B606894,1 KHM 03/18/2003 (End)

  *B607013,1 ABD - Prevent the customer to select any transaction has the posting 
  *B607013,1 ABD - Date is greater than the key off date if the company is linked to GL.  [Begin]
  IF lcToWork $ "DC" .AND. llGLLink .AND. (Trantype $ '10').AND.;
    IIF(lcToWork = "D",Debit.dPostDate,Credit.dPostDate) > ldKODate
    *-- Message text :- The posting date of transaction #  ð exceeds the key off date. Cannot select this transaction.
    *-- Message No.  :- 40185
    *-- Buttom Text  :- OK
    *-- Buttom No.   :- OOOO
    = gfModalGen("TRM40185BOOOO" , "DIALOG", IIF(lcToWork = "D",Debit.Tran,Credit.Tran))
    LOOP
    ENDIF
  *B607013,1 ABD - [End]

  *-- Save the record number because the dialog that might be 
  *-- displayed in "lfRemOpen" causes the browse screen to be
  *-- refreshed which cause the record pointer to be lost...
  *-- It will be resored right after the "lfRemOpen" calling.
  *-- The same thing will be done for the "lcDCAllTmp" file pointer
  *-- by using the SEEK command before the "lfRemOpen" call and
  *-- again before the delete command, that's to insure that we
  *-- dealing with the correct record.
  lnRecNo = RECNO()
  WAIT WINDOW "Processing Transaction Number : " + Tran NOWAIT
  lcInst = IIF(UPPER(lcFile)="DEBIT", &lcFile..cInstalNo, SPACE(0)) 
  lcExp  = Account+TranType+Tran+lcInst
  IF SEEK(lcExp, lcDCAllTmp) AND &lcDCAllTmp..cShToOpn = "N"
    llNoThing = lfRemOpen(.F.)
  ENDIF

  *-- Restore the record pointer that might have been changed in the
  *-- "lfRemOpen" function.
  SELECT (lcFile)
  GOTO lnRecNo
  IF SEEK(Account+TranType+Tran+lcInst, lcDCAllTmp)
    *-- If the current record is selected, go and unselect it
    SELECT (lcDCAllTmp)
    BLANK
    DELETE 
    llNoThing = lfUpdBal(lcType, &lcFile..Amount, -1, .T.)
  ELSE
    *-- If the current record is not selected, go and select it
    = lfUpdBal(lcType, &lcFile..Amount, +1, .T.)
    = lfaddInAll(lcFile)
  ENDIF  
ENDSCAN
WAIT CLEAR

*-- Adjust the Debit\Credit file pointer to the previously saved record 
*-- number.
IF lnTmpRcNo > 0 AND lnTmpRcNo < RECCOUNT()
  GOTO lnTmpRcNo
ELSE
  GOTO TOP
ENDIF

*-- Refresh the browse window, and activate it.
llNoThing = IIF(lcToWork = "D", lfwDbBrow(), lfwCdBrow())
lcBrToGo  = IIF(lcToWork = "D", "{ALT+D}" , "{ALT+I}")
KEYBOARD lcBrToGo PLAIN

SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfvSelNon
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : The "Select None" buttons valid function.
*:----------------------------------------------------------------
*: Parameters : lcToWork : "D" -> Work with the debit file please.
*:                         "C" -> Work with the credit file please.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfvSelNon("D")
*:----------------------------------------------------------------
FUNCTION lfvSelNon
PARAMETERS lcToWork
PRIVATE lcFile, lnAlias, lnTmpRcNo

*-- Determine the file name to use, and the total variable
*-- to be updated when selecting all records.
lcFile  = IIF(lcToWork = "D", "Debit", "Credit")
lcType  = IIF(lcToWork = "D", "P", "N")

*-- Flag to force the function "lfRemOpen" to display a message
*-- if the current selected record that needs to be unselected
*-- has an open balance.
llShow  = .T.

*-- Save the debit\credit record pointer.
lnAlias = SELECT(0)
SELECT (lcFile)
lnTmpRcNo = RECNO()

*-- Go through all the records
SCAN 
  *-- Save the record number because the dialog that might be 
  *-- displayed in "lfRemOpen" causes the browse screen to be
  *-- refreshed which cause the record pointer to be lost...
  *-- It will be resored right after the "lfRemOpen" calling.
  *-- The same thing will be done for the "lcDCAllTmp" file pointer
  *-- by using the SEEK command before the "lfRemOpen" call and
  *-- again before the delete command, that's to insure that we
  *-- dealing with the correct record.
  lnRecNo = RECNO()
  WAIT WINDOW "Processing Transaction Number : " + Tran NOWAIT
  lcInst  = IIF(UPPER(lcFile)="DEBIT", &lcFile..cInstalNo, SPACE(0)) 
  lcExp   = Account+TranType+Tran+lcInst
  IF SEEK(lcExp, lcDCAllTmp)
    = lfRemOpen(.F.)
  ENDIF

  *-- Restore the record pointer that might have been changed in the
  *-- "lfRemOpen" function.
  SELECT (lcFile)
  GOTO lnRecNo
  
  *-- If the current record is selected, go and unselect it.
  IF SEEK(Account+TranType+Tran+lcInst, lcDCAllTmp)
    SELECT (lcDCAllTmp)
    BLANK
    DELETE 
    llNoThing = lfUpdBal(lcType, &lcFile..Amount, -1, .T.)
  ENDIF
ENDSCAN
WAIT CLEAR

*-- Adjust the Debit\Credit file pointer to the previously saved record 
*-- number.
IF lnTmpRcNo > 0 AND lnTmpRcNo < RECCOUNT()
  GOTO lnTmpRcNo
ELSE
  GOTO TOP
ENDIF

*-- Refresh the browse window, and activate it.
llNoThing = IIF(lcToWork = "D", lfwDbBrow(), lfwCdBrow())
lcBrToGo  = IIF(lcToWork = "D", "{ALT+D}" , "{ALT+I}")
KEYBOARD lcBrToGo PLAIN

SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfRemOpen
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : To clear the "Open Balance" (if any) for a specific 
*:              selected debit\credit record when unselecting it.
*:----------------------------------------------------------------
*: Parameters : llRemOne : .T. -> We are processing one record.
*:                         .F. -> We are processing all records.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfRemOpen(.T.)
*:----------------------------------------------------------------
FUNCTION lfRemOpen
PARAMETERS llRemOne
PRIVATE lnAlias

lnAlias = SELECT(0)

*-- Go And check if there is an open balance for the current
*-- transaction.
SELECT (lcDCOnATmp)
LOCATE FOR cOpnBalTrn = &lcFile..Tran

*-- If you find an "Open Balance"..
IF FOUND()
  *-- Mark the record in the temp "All Selected Transactions" file
  *-- with "T", to be able to browse it next time you try to assign
  *-- an "Open Balance" for this transaction.
  SELECT (lcDCAllTmp)
  = RLOCK()
  REPLACE cShToOpn WITH "Y"
  UNLOCK
  
  *-- Go and remove the "Open Balance" from the "D\C On Account" temp
  *-- file.
  SELECT (lcDCOnATmp)
  lcAmnt    = ALLTRIM(STR(Amount,11,2)) 
  llNoThing = lfvAccRem(.F., .F.)
  
  *-- If we should display the message..
  IF llShow
    *-- Determine the message text, display it, and reset the 
    *-- flag to prevent displaying it again for any comming 
    *-- selected transaction.

    *-- The selected transaction has an open balance of '999.99'. 
    *-- The open balance will be ignored."
    *-- < Ok >
    *--                     OR 
    *-- One or more transaction(s) has open balance(s).
    *-- "Open balance(s) will be ignored.    
    *-- < Ok >
    lcMsg     = IIF(llRemOne, "M40088B00000", "M40089B00000")
    lcPrm     = IIF(llRemOne, lcAmnt, SPACE(0))
    lnNoThing = gfModalGen('IN'+lcMsg,"ALERT",lcPrm)
    llShow    = .F.
  ENDIF
ENDIF

SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfvFilter
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Filter check boxs valid function. 
*:----------------------------------------------------------------
*: Parameters : lcFltr : filter variable to work with.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfvFilter(lcDbFltr)
*:----------------------------------------------------------------
FUNCTION lfvFilter
PARAMETERS lcFltr

*-- Restore the filter values from the corresponding filter
*-- variable.
lcVarRead  = SYS(18)
lcFlType   = IIF("DB" $ lcVarRead, "Debit", "Credit")
ldLDate    = CTOD(lfGtFltFld("TRANDATE>={", "}"))
ldHDate    = CTOD(lfGtFltFld("TRANDATE<={", "}"))
lcFStore   = lfGtFltFld("STORE='", "'")

*-- Initialize the status of the filter check boxs from the previously
*-- selected transactions.
cbTran0    = "0" $ lcTrType
cbTran1    = "1" $ lcTrType
cbTran2    = "2" $ lcTrType
cbTran3    = "3" $ lcTrType
cbTran4    = "4" $ lcTrType
cbTran5    = "5" $ lcTrType
cbTran6    = "6" $ lcTrType

*-- The Disable\Enable status of the filter check boxs and ranges.
lcSt0      = IIF(cbTran0, "ENABLE", "DISABLE")
lcSt1      = IIF(cbTran1, "ENABLE", "DISABLE")
lcSt2      = IIF(cbTran2, "ENABLE", "DISABLE")
lcSt3      = IIF(cbTran3, "ENABLE", "DISABLE")
lcSt4      = IIF(cbTran4, "ENABLE", "DISABLE")
lcSt5      = IIF(cbTran5, "ENABLE", "DISABLE")
lcSt6      = IIF(cbTran6, "ENABLE", "DISABLE")

*-- Clear all the keyboard key trapping, and trap only the
*-- escape button.
PUSH KEY
ON KEY
ON KEY LABEL ESCAPE DO lfFltEsc
lcFilToUse = UPPER(lcFlType)
llUseIns   = lcFilToUse = "DEBIT"
lcUseAlias = gfTempName()

*-- Re-Open the corresponding file to be used in the ranges validation
USE (gcDataDir+lcFilToUse) IN 0 AGAIN ALIAS (lcUseAlias) ORDER (lcFilToUse)

*-- Run the screen.
*DO (gcScrDir+gcWinAppl+"\AR"+UPPER(LEFT(lcFlType,1))+"Fltr.SPR")
DO (gcScrDir+"AR"+UPPER(LEFT(lcFlType,1))+"Fltr.SPR")

*-- Be sure to close the opend file, and restore the previously
*-- saved keys trapping.
USE IN (lcUseAlias)
POP KEY

*-- Set the selected filter on the proper file, and set the filter check
*-- box, and variable.
IF UPPER(lcFlType) = "DEBIT"
  lcDbFltr   = lcFltr
  cbDbFilter = !EMPTY(lcDbFltr)
  SHOW GET cbDbFilter
  = lfSetFlter("Debit", .T., IIF(cbDbFilter, 3, 2)) AND lfwDbBrow()
ELSE
  lcCrFltr   = lcFltr
  cbCrFilter = !EMPTY(lcCrFltr)
  SHOW GET cbCrFilter
  = lfSetFlter("Credit", .F., IIF(cbCrFilter, 3, 2)) AND lfwCdBrow()
ENDIF

*:----------------------------------------------------------------
*: Name       : lfSetFlter
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Set\Reset the base and additinal filter expression 
*:              on the Debit\Credit files.
*:----------------------------------------------------------------
*: Parameters : lcFile      : File name to be used to set the 
*:                            filter on.
*:              llUseInstal : Should we use the Installment number
*:                            field in filter expression or not,
*:                            This parameter should only be true
*:                            if you are dealing with the "Debit"
*:                            file or a temp file with the "Debit"
*:                            file structure.
*:              lnMode      : Which filter to apply..
*:                            1. Clear the filter.
*:                            2. Set the base filter which is
*:                               on the Account+Currency codes.
*:                            3. Add additinal filter expression
*:                               to the base filter.
*:              lcPassFltr  : The additinal filter expression
*:                            (if any).
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfSetFlter("DEBIT", .T., 2)
*:----------------------------------------------------------------
FUNCTION lfSetFlter
PARAMETERS lcFile, llUseInstal, lnMode, lcPassFltr
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT (lcFile)
*B803881,1 SSH 01/29/2001 (Begin) Enhancement performance
PRIVATE lcOldDel , lcOldKey
lcOldDel = SET("DELETE")
lcOldKey = SET("ORDER")
SET DELETE OFF
SET ORDER TO
*B803881,1 SSH [END]
DO CASE
  *-- Clear the filter.
  CASE lnMode = 1
    SET FILTER TO
  *-- Set the base filter.
  CASE lnMode = 2
    IF llMltCur
      *-- If the system is setup to use multi currency, then include
      *-- the currency code in the filter.
      IF llUseInstal
        *-- Use the Installment number field in filter expression
        *B803881,1 SSH 01/29/2001 (Begin) Enhancement performance.
        *SET FILTER TO Account+Tran+cInstalNo+DTOS(TranDate)=(lcAccount) AND;
                      cCurrCode                           = (lcCurCode)
        SET FILTER TO Account+Tran+cInstalNo+DTOS(TranDate)=(lcAccount) AND;
                      cCurrCode                           = (lcCurCode) .AND. !DELETED()
        *B#803881,1 SSH (End)
      ELSE
        *-- Do not use the Installment number field in filter expression
        *B803881,1 SSH 01/29/2001 (Begin) Enhancement performance
        
        *SET FILTER TO Account+Tran+DTOS(TranDate) = (lcAccount) AND ;
                      cCurrCode                   = (lcCurCode)
        SET FILTER TO Account+Tran+DTOS(TranDate) = (lcAccount) AND ;
                     cCurrCode                   = (lcCurCode) .AND. !DELETED()
        *B803881,1 SSH (End)
      ENDIF    && Installment number field
    ELSE       && Multi Currency
      *-- If the system is setup not to use multi currency, then do not
      *-- include the currency code in the filter.
      IF llUseInstal
        *-- Use the Installment number field in filter expression
        *B803881,1 SSH 01/29/2001 (Begin) Enhancement performance
        *SET FILTER TO Account+Tran+cInstalNo+DTOS(TranDate)=(lcAccount)
        SET FILTER TO Account+Tran+cInstalNo+DTOS(TranDate)=(lcAccount) .AND. !DELETED()
        *: B#803881,1 SSH (End)
      ELSE
        *-- Do not use the Installment number field in filter expression
        *B803881,1 SSH 01/29/2001 (Begin) Enhancement performance
        *SET FILTER TO Account+Tran+DTOS(TranDate) = (lcAccount)
        SET FILTER TO Account+Tran+DTOS(TranDate) = (lcAccount) .AND. !DELETED()
        * B803881,1 SSH (End)
      ENDIF    && Installment number field
    ENDIF      && Multi Currency

  *-- Set an additinal filter expression
  CASE lnMode = 3
    *-- Be sure that the file has the base filter set.
    llNoThing = lfSetFlter(lcFile, llUseInstal, 2)
    
    *-- Get the base filter expression.
    lcOrgFil  = FILTER()

    *-- Get What should we add to the base filter ...
    lcToAdd   = IIF(TYPE("lcPassFltr")="C" AND !EMPTY(lcPassFltr),lcPassFltr,;
                IIF(UPPER(lcFile)="DEBIT", lcDbFltr, lcCrFltr))

    *-- Add the additinal filter expression
    IF !EMPTY(lcToAdd)
      lcFilter = lcOrgFil + IIF(EMPTY(lcOrgFil), "", ".AND.(") + lcToAdd + ")"
      *B803881,1 SSH 01/29/2001 (Begin) Enhancement performance
      SET FILTER TO &lcFilter  .AND. !DELETED()
      *B803881,1 SSH (End)
    ENDIF

  *-- Set the filter to false.
  CASE lnMode = 4
    *B803881,1 SSH 01/29/2001 (Begin) Enhancement performance
    *SET FILTER TO .F.
    IF llUseInstal
      SET FILTER TO Account+Tran+cInstalNo+DTOS(TranDate) = SPACE(11)  .AND. !DELETED()
    ELSE
      SET FILTER TO Account+Tran+DTOS(TranDate) = SPACE(11) .AND. !DELETED()
    ENDIF    && Installment number field
    *B803881,1 SSH [END]

ENDCASE
*B803881,1 SSH 01/29/2001 (Begin) Enhancement performance
SELECT (lcFile)
*: B604363,1 SSH 04/03/2001 Restore Old Selete setting.
*lcOldDel = SET("DELETE")
*lcOldKey = SET("ORDER")
*: B604363,1 SSH [END] Restore Old Selete setting.
SET DELETE &lcOldDel
SET ORDER TO &lcOldKey
*B803881,1 SSH [END]

*-- Activate the filter.
LOCATE
SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfFltEsc
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Filter screen Escape trapping function.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : ON KEY ESCAPE DO lfFltEsc
*:----------------------------------------------------------------
FUNCTION lfFltEsc

*-- When the user press escape, go to the "Cancel" button.
ON KEY LABEL ESCAPE
_CUROBJ = OBJNUM(pbFClose)
KEYBOARD "{ENTER}"

*:----------------------------------------------------------------
*: Name       : lfvChkBox
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Filter categories check boxs valid function.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfvChkBox()
*:----------------------------------------------------------------
FUNCTION lfvChkBox

*-- What is the active check bos name...
lcTran    = SYS(18)

*-- Which transaction number I'm dealing with ?
lcTranNum = RIGHT(ALLTRIM(lcTran),1)

*-- If the check button for the transaction I'm dealing
*-- with is checked then the status of its ranges will be
*-- enable, otherwise it will be disable.
lcStat    = IIF(&lcTran, "ENABLE", "DISABLE")

*-- Get the browse button name, the range variables names, and 
*-- refresh them.
lcBr1     = "pbBrL"  + lcTranNum
lcNm1     = "lcLNum" + lcTranNum
lcBr2     = "pbBrH"  + lcTranNum
lcNm2     = "lcHNum" + lcTranNum
&lcNm1    = IIF(&lcTran, &lcNm1, SPACE(6))
&lcNm2    = IIF(&lcTran, &lcNm2, SPACE(6))
SHOW GET &lcBr1 &lcStat
SHOW GET &lcNm1 &lcStat
SHOW GET &lcBr2 &lcStat
SHOW GET &lcNm2 &lcStat

*:----------------------------------------------------------------
*: Name       : lfvSetFlt
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Filter screen, "Set Filter" and "Clear Filter" 
*:              buttons valid function.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfvSetFlt()
*:----------------------------------------------------------------
FUNCTION lfvSetFlt

llSet  = SYS(18) = UPPER("pbSet")
lcFltr = SPACE(0)

IF llSet
  *-- If comming from "Set Filter" button
  lcTrType = SPACE(0)
  
  *-- Go through all the transactions.
  FOR lnTrnNum = 0 TO 6
    lcTrnNum = STR(lnTrnNum,1)
    lcTrn    = "cbTran" + lcTrnNum
    lcTrType = lcTrType + IIF(&lcTrn, lcTrnNum, SPACE(0))
    
    IF EVAL(lcTrn)                                        AND ;
       ((UPPER(lcFlType) = "DEBIT"  AND lcTrnNum $ "123")  OR ;
        (UPPER(lcFlType) = "CREDIT" AND lcTrnNum $ "0456"))
      *-- Build the filter variable for the current file 
      *-- (Debit\Credit).
      lcNm1     = "lcLNum" + lcTrnNum
      lcNm2     = "lcHNum" + lcTrnNum
      lcSubFlt0 = "TranType='" + lcTrnNum + "'"
      lcSubFlt1 = IIF(EMPTY(&lcNm1), SPACE(0), "TRAN>='"+&lcNm1+"'")
      lcSubFlt2 = IIF(EMPTY(&lcNm2), SPACE(0), "TRAN<='"+&lcNm2+"'")
      lcSubFlt  = lcSubFlt0 + IIF(EMPTY(lcSubFlt1), "", ".AND." +lcSubFlt1)
      lcSubFlt  = lcSubFlt  + IIF(EMPTY(lcSubFlt2), "", ".AND." +lcSubFlt2)
      lcFltr    = lcFltr + IIF(EMPTY(lcFltr),"",".OR.") +"("+lcSubFlt+")"
    ENDIF
  ENDFOR

  *-- Add the date range to the filter string if it is not
  *-- empty.
  IF !EMPTY(ldLDate)
    lcFltr = lcFltr + IIF(EMPTY(lcFltr), "", " AND ") +;
             "TRANDATE>={" + DTOC(ldLDate) + "}"
  ENDIF

  *-- Add the date range to the filter string if it is not
  *-- empty.
  IF !EMPTY(ldHDate)
    lcFltr = lcFltr + IIF(EMPTY(lcFltr), "", " AND ") +;
             "TRANDATE<={" + DTOC(ldHDate) + "}"
  ENDIF

  *-- Add the store to the filter string if it is not empty.
  IF !EMPTY(lcFStore)
    lcFltr = lcFltr + IIF(EMPTY(lcFltr), "", " AND ") +;
             "STORE='" + ALLTRIM(UPPER(lcFStore)) + "'"
  ENDIF
ELSE
  *-- If comming from "Clear Filter" button, clear all the
  *-- filter variables.
  STORE SPACE(0) TO lcTrType, lcLNum0, lcLNum1, lcLNum2, lcLNum3,;
                    lcLNum4 , lcLNum5, lcLNum6, lcHNum0, lcHNum1,;
                    lcHNum2 , lcHNum3, lcHNum4, lcHNum5, lchNum6
ENDIF

*:----------------------------------------------------------------
*: Name       : lfwSavOld
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Global "When" function to save the object's old
*:              value.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfwSavOld()
*:----------------------------------------------------------------
FUNCTION lfwSavOld

*-- Save the value of the currently edited object.
lcOldValue = EVAL(SYS(18))

*:----------------------------------------------------------------
*: Name       : lfvRange
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Filter ranges valid function.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing =lfvRange()
*:----------------------------------------------------------------
FUNCTION lfvRange

*-- Get the name of the currently edited object.
lcVarRead = SYS(18)
lcValue   = &lcVarRead

*-- If the object's value is not empty or the object's browse
*-- button is pressed.
IF !EMPTY(lcValue) OR llFrmBrow
  *-- Filter the used file for the selected transaction type.
  lcAlias   = SELECT(0)
  lcTrnNum  = RIGHT(lcVarRead,1)
  lnTrnNum  = VAL(lcTrnNum)
  SELECT (lcUseAlias)
  llNoThing = lfSetFlter(lcUseAlias, llUseIns, 2)
  llNoThing = lfSetFlter(lcUseAlias, llUseIns, 3, "TranType='"+lcTrnNum+"'")
  
  *-- If the edited value is invalid or the object's browse
  *-- button is pressed.
  IF llFrmBrow OR !SEEK(lcAccount+lcValue)
    *-- Browse the transactions..
    llFrmBrow = .F.
    lcNewVal  = lfFltrBrow()           
    IF EMPTY(lcNewVal)
      &lcVarRead = lcOldValue
      _CUROBJ    = _CUROBJ
    ELSE
      &lcVarRead = lcNewVal
    ENDIF
  ENDIF

  *-- Clear the filter.
  llNoThing = lfSetFlter(lcUseAlias, llUseIns, 1)

  *-- Validate the hi, low value for the range..
  lcLVal    = EVAL("lcLNum"+lcTrnNum)
  lcHVal    = EVAL("lcHNum"+lcTrnNum)
  IF !EMPTY(lcLVal) AND !EMPTY(lcHVal) AND lcHVal<lcLVal
    *-- From Through range error, Cannot proceed.
    *-- < Ok >
    lnNoThing  = gfModalGen('TRM40090B00000')
    &lcVarRead = lcOldValue
    _CUROBJ    = _CUROBJ
  ENDIF

  SELECT(lcAlias)
ENDIF  

*:----------------------------------------------------------------
*: Name       : lfvDatRang
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Filter dates range valid function.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfvDatRang()
*:----------------------------------------------------------------
FUNCTION lfvDatRang

*-- Get the currently edited date object.
lcVarRead = SYS(18)

*-- If the date entered is not a valid date range, display 
*-- a message and remain in the object.
IF !EMPTY(ldLDate) AND !EMPTY(ldHDate) AND ldHDate<ldLDate
  *-- From Through range error, Cannot proceed.
  *-- < Ok >
  lnNoThing  = gfModalGen('TRM40090B00000')
  &lcVarRead = lcOldValue
  _CUROBJ    = _CUROBJ
ENDIF

*:----------------------------------------------------------------
*: Name       : lfFltrBrow
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Browse function used in the filter ranges 
*:              validation.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : llNoThing = lfFltrBrow()
*:----------------------------------------------------------------
FUNCTION lfFltrBrow

SELECT (lcUseAlias)
DIMENSION laRetVal[1]

*-- Browse these fields.
laRetVal   = SPACE(0)
lcBrFields = "Tran:08:R:H='Tran #',"      +;
             "Desc:20:R:H='Description'," +;
             "Amount:11:R:H='Amount',"    +;
             "TranDate:H='Tran. Date',"   +;
             "Store:H='Store',"           +;
             "Reference:H='Reference'"

*-- Transactions type description, used to title the browse.
lcAll      = PADR("Credit Memos      ",20)+PADR("Invoices          ",20)+;
             PADR("Debit Adjustments ",20)+PADR("Charge Backs      ",20)+;
             PADR("Customer Payments ",20)+PADR("Credit Adjustments",20)+;
             PADR("Credits On Account",20)
lcFile_Ttl = "Select " + ALLTRIM(SUBSTR(lcAll, IIF(lnTrnNum=0,1,lnTrnNum*20), 20))
lcRetVal   = SPACE(0)

*-- Browse the transaction, if the user selects any thing, return that
*-- selection.
IF gfBrows(.F.,"Tran",'laRetVal')
  lcRetVal = laRetVal[1]
ENDIF

RETURN (lcRetVal)

*:----------------------------------------------------------------
*: Name       : lfGtFltFld
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : To restore a filter value from a filter expression.
*:----------------------------------------------------------------
*: Parameters : lcToSearch : What are you looking for in the filter
*:                           expression.
*:              lcTerminat : What I'm looking for, is terminated 
*:                           using this terminator..
*:----------------------------------------------------------------
*: Returns    : The value wanted from the filter.
*:----------------------------------------------------------------
*: Example    : lcStore = lfGtFltFld("STORE='", "'")
*:----------------------------------------------------------------
FUNCTION lfGtFltFld
PARAMETERS lcToSearch, lcTerminat
PRIVATE    lcRet, lnPos

*-- Look for the sent value in the filter expression
lcRet = SPACE(0)
lnPos = ATC(UPPER(lcToSearch), UPPER(lcFltr))

*-- If the value asked for exist in the expression then
*-- return the contint of that value..
IF lnPos > 0
  lcNewStr = SUBSTR(lcFltr, lnPos+LEN(lcToSearch))
  lcRet    = ALLTRIM(SUBSTR(lcNewStr, 1, ATC(lcTerminat, lcNewStr)-1))
ENDIF

RETURN (lcRet)

*:----------------------------------------------------------------
*: Name       : lfaddInAll
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Add a record in the "All Transactions" temp file.
*:----------------------------------------------------------------
*: Parameters : lcFile : The file that will be used to update
*:                       the "All Transactions" file from.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfaddInAll("DEBIT")
*:----------------------------------------------------------------
FUNCTION lfaddInAll
PARAMETERS lcFile
PRIVATE lcFile, lnAlias

lnAlias   = SELECT(0)
lcDelStat = SET("DELETE")

*-- Select the source file, and get the data from the current
*-- record.
SELECT (lcFile)
SCATTER MEMVAR MEMO
*B603414,1 NAD 02/02/2000 (Start) replace Trancode with cCreditcode if lcFile is the credit file
m.TranCode= IIF (UPPER(lcFile)="CREDIT",cCreditCod,m.TranCode)
*B603414,1 NAD (End) 
m.cShToOpn = IIF(UPPER(lcFile) $ "DEBIT & CREDIT", "Y", "N")

*-- Add a new record in the "All Transactions" temp file
*-- and update it with the data from the source file.
SELECT (lcDCAllTmp)
SET DELETE OFF
IF SEEK(SPACE(6)) AND DELETED()
  RECALL
ELSE
  APPEND BLANK
ENDIF
GATHER MEMVAR MEMO

SET DELETE &lcDelStat
GOTO TOP
SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfBldTyPop
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Build the type popups.
*:----------------------------------------------------------------
*: Parameters : llIncNA : .T. -> Incluse "N\A" in the popup.
*:                        .F. -> Do not incluse "N\A" in the popup.
*:              lcType  : "J" -> Debit/Credit Adjustment type popup.
*:                        "C" -> Debit/Credit On Account type popup.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfBldTyPop(.T., "J")
*:----------------------------------------------------------------
FUNCTION lfBldTyPop
PARAMETERS llIncNA, lcType

DO CASE
  *-- Debit/Credit Adjustment
  CASE lcType = "J"
    lnDim = IIF(llIncNA, 3, 2)
    DIMENSION laAdjType[lnDim]
    laAdjType[1] = "Debit Adjustment"
    laAdjType[2] = "Credit Adjustment"
    IF llIncNA
      laAdjType[3] = "N/A"
    ENDIF
  
  *-- Debit/Credit On Account
  CASE lcType = "C"
    lnDim = IIF(llIncNA, 4, 3)
    DIMENSION laAccType[lnDim]
    laAccType[1] = "Debit On Account"
    laAccType[2] = "Credit On Account"
    laAccType[3] = "Open Balance"
    IF llIncNA
      laAccType[4] = "N/A"
    ENDIF
ENDCASE

*:----------------------------------------------------------------
*: Name       : lfGetGLInf
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Get the GL related information wich are..
*:              GL Account, Bank Code, and checking account from 
*:              the codes file.
*:----------------------------------------------------------------
*: Parameters : lcValue   : The code value.
*:              lcFldName : The code field name.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfGetGLInf("000023", "CCREDITCOD")
*:----------------------------------------------------------------
FUNCTION lfGetGLInf
PARAMETERS lcValue, lcFldName

*-- Create a temp array to hold the "gfRltFld" parameter.
DIMENSION laTermAry[3,2]
laTermAry[1,1] = "CBNKCODE  "
laTermAry[1,2] = "lcBankCode"
laTermAry[2,1] = "CCHKACCT  "
laTermAry[2,2] = "lcChkAcc"
laTermAry[3,1] = "CADJACCT  "
laTermAry[3,2] = "lcGLAccnt"

*-- Blank the related GL information variables.
lcBankCode     = SPACE(0)
lcChkAcc       = SPACE(0)
lcGLAccnt      = SPACE(0)

*-- Fill the related GL information from the codes file.
llNoThing      = gfRltFld(lcValue, @laTermAry, lcFldName)

*-- Remove the un-needed spaces...
lcBankCode     = RTRIM(lcBankCode)
lcChkAcc       = RTRIM(lcChkAcc)
lcGLAccnt      = RTRIM(lcGLAccnt)

*:----------------------------------------------------------------
*: Name       : lfvAdjNew
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "New" button in the 
*:              D\C Adjustment folder.
*:----------------------------------------------------------------
*: Parameters : llFrmTrade : Is this call form the "Trade Discount"
*:                           button, or from the screen.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvAdjNew()
*:----------------------------------------------------------------
FUNCTION lfvAdjNew
PARAMETERS llFrmTrade
PRIVATE lnAlias

*-- Look for any empty record in the temp file before appending
*-- a new one.

*B803369,1 (Start) The default factor.
lcFactor=lcCusFact
*B803369,1 (End) 
lnAlias = SELECT(0)
SELECT (lcDCAdjTmp)
LOCATE FOR EMPTY(TranType)

*-- If there is no empty recors then add a new one.
IF !FOUND()
  APPEND BLANK
  = RLOCK()
  REPLACE Account   WITH lcAccount              ,;
          Tran      WITH 'T'+SUBSTR(SYS(3),4,5) ,;
          TranDate  WITH ldKODate               ,;
          Amount    WITH 0                      ,;
          cCurrCode WITH lcCurCode              ,;
          nExRate   WITH lnExRate               ,;
          nCurrUnit WITH lnCurrUnit
  UNLOCK
ENDIF

*-- If not called from teh "Trade Discount" button, please
*-- refresh the browse.
IF !llFrmTrade
  llNoThing  = lfwDCAdjBr()
  _CUROBJ    = OBJNUM(lnAdjType)
ENDIF  

SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfvAdjRem
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "Remove" button in the 
*:              D\C Adjustment folder.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvAdjRem()
*:----------------------------------------------------------------
FUNCTION lfvAdjRem
*E301276,01 (Start)
PARAMETERS llAutoKey
PRIVATE llAutoKey
*E301276,01 (End)
PRIVATE lnAlias


*-- If the user confirmed to delete the record..
*-- Are you sure, You want to delete this transaction ?
*-- < Yes >   < No > 
*E301276,01 (Start)
*IF gfModalGen('QRM40077B40000') = 1
IF llAutoKey OR gfModalGen('QRM40077B40000') = 1
*E301276,01 (End)

  lnAlias = SELECT(0)
  
  *-- If the transaction type field is not empty, means that the
  *-- record is full.
  IF !EMPTY(&lcDCAdjTmp..TranType) 
    *-- Clear the sales rep charge back if any.
    IF USED(lcRpComTmp)
      = lfvCBRClr(&lcDCAdjTmp..Tran)
    ENDIF

    *-- Update the selected transactions summery in the header.
    = lfUpdBal(SUBSTR("PN",lnAdjType,1), &lcDCAdjTmp..Amount, -1, .T.)
  ENDIF
  
  *-- Remove the corresponding line in the "All Selected Transactions"
  *-- temp file.
  SELECT (lcDCAdjTmp)
  IF SEEK(Account+TranType+Tran+cInstalNo, lcDCAllTmp)
    SELECT(lcDCAllTmp)
    BLANK
    DELETE
  ENDIF
  
  *-- Remove the current selected record.
  SELECT (lcDCAdjTmp)
  BLANK
  DELETE
  GOTO TOP
  
  *E301276,01 (Start)
  *llNoThing = lfwDCAdjBr()    
  *B803369,1 (Start) Refresh the objects in case of manual key off.
  *IF llAutoKey       
  IF !llAutoKey   
  *B803369,1 (End)
    llNoThing = lfwDCAdjBr()
  ENDIF
  *E301276,01 (End)
  
  SELECT(lnAlias)  
ENDIF

*:----------------------------------------------------------------
*: Name       : lfvAdjType
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "Type" popup in the 
*:              D\C Adjustment folder.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvAdjType()
*:----------------------------------------------------------------
FUNCTION lfvAdjType
PRIVATE lnAlias

*-- If the user selects anything rather than "N\A".
IF lcOldValue # lnAdjType AND lnAdjType < 3
  lnAlias = SELEC(0)
  SELECT (lcDCAdjTmp)
  
  *-- Default the reason code, the GL information for the current
  *-- record.
  lcDCAdj    = laAdjCodIn[lnAdjType,1]
  llNoThing  = gfwCodePop (@laAdjCodIn, lcDCAdj, "D")
  lcBankCode = SPACE(0)
  lcChkAcc   = SPACE(0)
  lcGLAccnt  = SPACE(0)
  llNoThing  = lfGetGLInf(laDCAdj[lnDCAdj,2],laAdjCodIn[lnAdjType,1])
  llNoThing  = RLOCK()
  *B803369,1 (Start) update the temp file with the default factor.
  *REPLACE TranType  WITH SUBSTR("27",lnAdjType,1) ,;
          TranCode  WITH laDCAdj[lnDCAdj,2]       ,;
          cReason   WITH laDCAdj[lnDCAdj,1]       ,;
          dPostDate WITH ldKODate                 ,;
          cYear     WITH lcGLFYear                ,;
          cPrd      WITH lcGLPeriod               ,;
          cBnkCode  WITH lcBankCode               ,;
          cChkAcct  WITH lcChkAcc                 ,;
          cAdjAcct  WITH lcGLAccnt                ,;
          Deb_Adj   WITH IIF(TranType = "2", "Y", Deb_Adj) 
 
  REPLACE TranType  WITH SUBSTR("27",lnAdjType,1) ,;
          TranCode  WITH laDCAdj[lnDCAdj,2]       ,;
          cReason   WITH laDCAdj[lnDCAdj,1]       ,;
          dPostDate WITH ldKODate                 ,;
          cYear     WITH lcGLFYear                ,;
          cPrd      WITH lcGLPeriod               ,;
          cBnkCode  WITH lcBankCode               ,;
          cChkAcct  WITH lcChkAcc                 ,;
          cAdjAcct  WITH lcGLAccnt                ,;
          Deb_Adj   WITH IIF(TranType = "2", "Y", Deb_Adj) ,;
          cFacCode  WITH lcFactor
  
  UNLOCK
  *B803369,1 (End) 
  

  *-- Add a copy of this record in the "All Selected transactions"
  *-- temp file, and refresh the browse.
  = lfaddInAll(lcDCAdjTmp)
  = lfwDCAdjBr()
  SELEC(lnAlias)
ENDIF  

*:----------------------------------------------------------------
*: Name       : lfvAdjCode
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "Reason" popup in the 
*:              D\C Adjustment folder.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvAdjCode()
*:----------------------------------------------------------------
FUNCTION lfvAdjCode
PRIVATE lnAlias

*-- Get the related GL information for the new selected reason.
lnAlias    = SELECT(0)
lcBankCode = SPACE(0)
lcChkAcc   = SPACE(0)
lcGLAccnt  = SPACE(0)
llNoThing  = lfGetGLInf(laDCAdj[lnDCAdj,2],laAdjCodIn[lnAdjType,1])

*-- Update the new selected reason code and its related GL information.
SELECT (lcDCAdjTmp)
llNoThing  = RLOCK()
REPLACE TranCode  WITH laDCAdj[lnDCAdj,2] ,;
        cReason   WITH laDCAdj[lnDCAdj,1] ,;
        cBnkCode  WITH lcBankCode         ,;
        cChkAcct  WITH lcChkAcc           ,;
        cAdjAcct  WITH lcGLAccnt

*B802622,1 ABD Add the decscription of the reason code to the decscription filed by default
*B802622,1 ABD When the user select the reason decscription. [Begin]
REPLACE DESC WITH laDCAdj[lnDCAdj,1]
lcAdjDes = DESC
SHOW GET lcAdjDes
*B802622,1 ABD [End]

UNLOCK

*-- Update the corresponding record in the "All Selected Transactions" 
*-- temp file.
SCATTER MEMVAR MEMO
IF SEEK(Account+TranType+Tran, lcDCAllTmp)
  SELECT(lcDCAllTmp)
  GATHER MEMVAR MEMO
ENDIF

*-- Refresh the browse.
SHOW WINDOW (lcDCAdjTtl) REFRESH
SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfvAdjRef
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "Refereance" field in the 
*:              D\C Adjustment folder.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvAdjRef()
*:----------------------------------------------------------------
FUNCTION lfvAdjRef
PRIVATE lnAlias

*-- If the field has been changed..
IF !(lcOldValue == lcAdjRef)
  lnAlias = SELECT(0)
  SELECT (lcDCAdjTmp)
  
  *-- Update the new value.
  llNoThing  = RLOCK()
  REPLACE Reference WITH lcAdjRef
  UNLOCK
  
  *-- Update the corresponding record in the "All Selected Transactions" 
  *-- temp file.
  SCATTER MEMVAR MEMO
  IF SEEK(Account+TranType+Tran, lcDCAllTmp)
    SELECT(lcDCAllTmp)
    GATHER MEMVAR MEMO
  ENDIF
  
  *-- Refresh the browse.
  SHOW WINDOW (lcDCAdjTtl) REFRESH
  SELECT(lnAlias)
ENDIF

*:----------------------------------------------------------------
*: Name       : lfvAdjDes
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "Description" field in the 
*:              D\C Adjustment folder.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvAdjDes()
*:----------------------------------------------------------------
FUNCTION lfvAdjDes
PRIVATE lnAlias

*-- If the field has been changed..
IF !(lcOldValue == lcAdjDes)
  lnAlias = SELECT(0)
  SELECT (lcDCAdjTmp)

  *-- Update the new value.
  llNoThing  = RLOCK()
  REPLACE Desc WITH lcAdjDes
  UNLOCK

  *-- Update the corresponding record in the "All Selected Transactions" 
  *-- temp file.
  SCATTER MEMVAR MEMO
  IF SEEK(Account+TranType+Tran, lcDCAllTmp)
    SELECT(lcDCAllTmp)
    GATHER MEMVAR MEMO
  ENDIF

  *-- Refresh the browse.
  SHOW WINDOW (lcDCAdjTtl) REFRESH
  SELECT(lnAlias)
ENDIF

*:----------------------------------------------------------------
*: Name       : lfvAdjAmnt
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "Amount" field in the 
*:              D\C Adjustment folder.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvAdjAmnt()
*:----------------------------------------------------------------
FUNCTION lfvAdjAmnt
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT (lcDCAdjTmp)



*E301429,1 (Start) To prevenet bug numeric over flow
PRIVATE lnMaxAmt
lnMaxAmt=IIF(lnAdjType=2,9999999999.00,99999999999.00)
IF lnAdjAmont >lnMaxAmt
  * Message XXXXX cannot exceed 9999999999
  =gfModalGen("TRM40171BOOOO" , "DIALOG", 'Amount' +'|'+ ALLTRIM(STR(lnMaxAmt,11)) )
  lnAdjAmont =  lcOldValue 
  RETURN
ENDIF 
*E301429,1 (End)

*-- If the field has been changed..
IF Amount <> lnAdjAmont
  *-- Update the selected transactions totlas in the header.
  = lfUpdBal(SUBSTR("PN",lnAdjType,1), Amount, -1, .F.)
  
  *-- Adjust the amount sign according to the transaction type.
  lnAdjAmont = IIF(lnAdjType=2, -1, +1) * ABS(lnAdjAmont)
  
  *-- Update the new value.
  llNoThing  = RLOCK()
  REPLACE Amount WITH lnAdjAmont
  UNLOCK

  *-- Update the corresponding record in the "All Selected Transactions" 
  *-- temp file.
  SCATTER MEMVAR MEMO
  IF SEEK(Account+TranType+Tran, lcDCAllTmp)
    SELECT(lcDCAllTmp)
    GATHER MEMVAR MEMO
  ENDIF

  *-- Update the selected transactions totlas in the header, and refresh
  *-- the browse.
  = lfUpdBal(SUBSTR("PN",lnAdjType,1), Amount, +1, .T.)
  SHOW WINDOW (lcDCAdjTtl) REFRESH
ENDIF

*-- Enable\Disable the Sales Rep Charge back check box.
lcChrBkSt  = IIF(lnAdjType=2 AND Amount#0, "ENABLE", "DISABLE")
SHOW GET cbCharBakS &lcChrBkSt
SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfvAdjDate
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "Transaction Date" field 
*:              in the D\C Adjustment folder.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvAdjDate()
*:----------------------------------------------------------------
FUNCTION lfvAdjDate

*-- Temporary fiscal year, period variables.
lcRGLFYear = SPACE(0)
lcRGLPriod = SPACE(0)

*-- Check and fill the fiscal year, period for the entered date.
IF CheckPrd(ldAdjPDate, "lcRGLFYear", "lcRGLPriod", "KO")
  lnAlias = SELECT(0)
  SELECT (lcDCAdjTmp)

  *-- Update the new values.
  llNoThing  = RLOCK()
  REPLACE dPostDate WITH ldAdjPDate ,;
          cYear     WITH lcRGLFYear ,;
          cPrd      WITH lcRGLPriod
  UNLOCK

  *-- Update the corresponding record in the "All Selected Transactions" 
  *-- temp file.
  SCATTER MEMVAR MEMO
  IF SEEK(Account+TranType+Tran, lcDCAllTmp)
    SELECT(lcDCAllTmp)
    GATHER MEMVAR MEMO
  ENDIF

  *-- Refresh the browse.
  SHOW WINDOW (lcDCAdjTtl) REFRESH
  SELECT(lnAlias)
ELSE
  *-- If the date is invalid, restore the old value and remain 
  *-- in the object.
  ldAdjPDate = lcOldValue
  _CUROBJ    = _CUROBJ
ENDIF  

*:----------------------------------------------------------------
*: Name       : lfvTradDis
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "Trade Discount" button
*:              in the D\C Adjustment folder.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvTradDis()
*:----------------------------------------------------------------
FUNCTION lfvTradDis
PRIVATE lnAlias

*-- Use the "All Selected Transactions" temp file in another
*-- alias.
lnAlias = SELECT(0)
USE (gcWorkDir+lcDCAllTmp) IN 0 ;
AGAIN ALIAS TmpAll ORDER (lcDCAllTmp) SHARED

*-- If there is any invoice or return selected..
SELECT TmpAll
IF SEEK(lcAccount+"0") OR SEEK(lcAccount+"1")
  *-- Accumulate the trade discount for all the selected invoices.
  SUM ALL -1*Dsc_Amt                            ;
      FOR Account+TranType+Tran+cInstalNo = lcAccount+"1" ;
       TO lnFrmInv

  *-- Accumulate the trade discount for all the selected returns.
  SUM ALL -1*Dsc_Amt                            ;
      FOR Account+TranType+Tran+cInstalNo = lcAccount+"0" ;
       TO lnFrmRet

  *E301077,53 YMA 03/03/99 Start - Open the invoice header file.
  = gfOpenFile(gcDataDir+"InvHdr","InvHdrA",'SH')
  *E301077,53 YMA 03/03/99 End.
   
  IF lnFrmRet = 0 AND lnFrmInv = 0
    *-- If all the selected invoices and returns have no trade 
    *-- discount, display a message and do not do any thing.
    
    *-- The selected transaction(s) has no trade discount to display.
    *-- < ok >
    lcToSend = STRTRAN(LOWER(ALLTRIM(lcTrDsPrm)), "...", "")
    = gfModalGen('TRM40091B00000',"ALERT", lcToSend)
  ELSE
    *-- If there is any trade discount amount..
    
    *-- Compute the net discount, and build the "Net" object tilte 
    *-- variable.
    lnNet     = lnFrmInv + lnFrmRet
    lnDiff    = ABS(lnFrmInv) - ABS(lnFrmRet)
    lcNetProm = "Net "
    
    *-- The browse title and fields.
    lcTrBrTtl = "Keyed Off Transactions With Discount"
    lcFields  = SPACE(0)
    
    *-- Flag to tell if the user selectes OK..
    llOk      = .F.
    
    *-- Dewfault the apply radio button to the "Net"
    rbApply   = 3
    
    *-- We will add the Invoice\Return terms field to the browse
	lcTrmFld  = "cTerms=gfCodDes(IIF(TranType='1',InvHdr.cTermCode,"+;
                "Return.cTermCode),'CTERMCODE'):22:R:H='Terms',"                  
    IF lnDiff # 0
      lcNetProm = lcNetProm + IIF(lnDiff>0, "Credit","Debit")
    ENDIF
    
    *-- Open the return header file.
    USE (gcDataDir+"RetHdr") IN 0  ;
    AGAIN ALIAS Return ORDER RetHdrA SHARED 
    
    *-- Set a relation between the temp file and the invoice\return
    *-- files to be able to display the terms codes.
    SELECT TmpAll
    SET RELATION TO lcAccount+Tran INTO InvHdr ADDITIVE
    SET RELATION TO lcAccount+Tran INTO Return ADDITIVE
    LOCATE
    
    *-- Clear the main screen key trapping, and set the trade 
    *-- discount screen trapping.
    PUSH KEY    
    ON KEY
    ON KEY LABEL ESCAPE  DO lfTradEsc
    ON KEY LABEL TAB     DO lfTradTab
    ON KEY LABEL BACKTAB DO lfTradSTb
    ON KEY LABEL Ctrl+ENTER lnDummy = 1
    ON KEY LABEL Ctrl+HOME  lnDummy = 1
    ON KEY LABEL Ctrl+END   lnDummy = 1
    ON KEY LABEL ALT+B  ACTIVATE WINDOW (lcTrBrTtl)
    
    *-- Run the screen.
    *DO (gcScrDir+gcWinAppl+"\ARTRDIS.SPR")
    DO (gcScrDir+"ARTRDIS.SPR")
        
    *-- Restore the main screen key trapping.
    POP KEY
    
    *-- Clear the relations.
    SET RELATION OFF INTO Invhdr
    SET RELATION OFF INTO Return
    
    *-- Close the return header file. 
    USE IN Return
    
    *-- Call the "Ok" valid function if the user pressed OK.
    *-- This function is not called as a valid function for the
    *-- "Ok" button in the screen, because we need to default the
    *-- amount field in the folder screen with the selected amount 
    *-- from the trade discount screen, and set the focus on that 
    *-- field right after deactivating the trade discount screen,
    *-- this might be diffecult if we call this function when the
    *-- trade discount screen is still active.
    llNoThing = IIF(llOk, lfvTradOk(), .T.)
  ENDIF
ELSE
  *-- No selected transactions to display.
  *--  < Ok >
  = gfModalGen('TRM40092B00000')
ENDIF

*-- Close the temp file in the new alias.
USE IN TmpAll

SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfTrUpMsg
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : To display two kinds of confirming messages to the
*:              user before updating the amount field, and return
*:              the user response.
*:----------------------------------------------------------------
*: Parameters : lnMsgTyp : The message type.
*:              lnOrgAmn : The orignal amount.
*:              lnNewAmn : The new selected amount.
*:----------------------------------------------------------------
*: Returns    : .T. -> Go on and update.
*:              .F. -> Please do not update.
*:----------------------------------------------------------------
*: Example    : = lfTrUpMsg()
*:----------------------------------------------------------------
FUNCTION lfTrUpMsg
PARAMETERS lnMsgTyp, lnOrgAmn, lnNewAmn
PRIVATE llRet

llRet = .T.
DO CASE
  *-- If the normal action should be done...
  CASE lnMsgTyp = 1
    *-- If the old amount is not equal to the new amount, display
    *-- the message and return the user response.
    IF lnOrgAmn # lnNewAmn
      *-- Are you sure you want to replace the transaction
      *-- amount '999.99' with '999.99'
      *-- < Yes > < No >
      lcBefore = ALLTRIM(STR(lnOrgAmn,9,2))
      lcAfter  = ALLTRIM(STR(lnNewAmn,9,2))
      lcToSend = lcBefore + "|" + lcAfter
      llRet    = gfModalGen('QRM40093B40000',"ALERT", lcToSend) = 1
    ENDIF

  *-- If we are going to change the transaction type.
  CASE lnMsgTyp = 2
    *-- Applying a XXXXXXXX amount will automatically change
    *-- the transaction type to be a XXXXXXXX adjustment.
    *-- < Proceed > < Cancel >
    lcMsg = LOWER(lcDCMessg) + "|" + LOWER(lcDCMessg)
    llRet = gfModalGen('QRM40094B40003',"ALERT", lcMsg) = 1

    IF llRet
      lcLookFor = &lcDCAdjTmp..Tran
      llFound   = USED(lcRpComTmp) AND SEEK(lcLookFor, lcRpComTmp)
      IF &lcDCAdjTmp..TranType = "7" AND llFound
        *-- Changing the transaction type to a debit adjustment will 
        *-- clear the 'Sales Rep. Commission' charge backs applied 
        *-- on that transaction.
        *-- < Proceed > < Cancel >
        llRet = gfModalGen('QRM40095B40003') = 1
      ENDIF
    ENDIF
ENDCASE
RETURN (llRet)

*:----------------------------------------------------------------
*: Name       : lfvTradOk
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "Ok" button in the 
*:              "trade Discount" screen.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvTradOk()
*:----------------------------------------------------------------
FUNCTION lfvTradOk
PRIVATE lnAlias, lcTranType, lnOldAmnt, lnNewAmnt

lnAlias    = SELECT(0)
lnOldAmnt  = &lcDCAdjTmp..Amount
lnNewAmnt  = EVAL(SUBSTR("LNFRMINVLNFRMRETLNNET",((rbApply-1)*8)+1,8))
lnSnNum    = IIF(lnNewAmnt > 0, 1, 2)
lcDCMessg  = PROPER(ALLTRIM(SUBSTR("DEBIT CREDITNET",((lnSnNum-1)*6)+1,6)))
llNormAct  = (&lcDCAdjTmp..TranType = "2" AND lnNewAmnt >= 0) OR ;
             (&lcDCAdjTmp..TranType = "7" AND lnNewAmnt <= 0)
lnMsgNum   = IIF(llNormAct, 1, 2)

*-- Display a confim message, if the user selects to proceed..
IF lfTrUpMsg(lnMsgNum, lnOldAmnt, lnNewAmnt)
  
  *-- If the action to be taken is to change the trasaction type...
  IF !llNormAct
    lcExp      = lcAccount+&lcDCAdjTmp..TranType+&lcDCAdjTmp..Tran

    *-- Remove the current record.
    llNoThing  = lfvAllRem(.T., lcExp)
    
    *-- Add a new one.
    llNoThing  = lfvAdjNew(.T.)
    
    *-- Adjust the transaction type, and update the defaults.
    lcOldValue = 0
    lnAdjType  = lnSnNum
    llNoThing  = lfvAdjType()
  ENDIF
  
  *-- Update the amount.
  lnAdjAmont = lnNewAmnt
  llNoThing  = lfvAdjAmnt()
ENDIF

*-- Set the focus on the amount field in the folder screen
_CUROBJ = OBJNUM(lnAdjAmont)

SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfTradEsc
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : "Trade discount" screen trapping "ESACAPE" button.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfTradEsc()
*:----------------------------------------------------------------
FUNCTION lfTradEsc

*-- Clear the trapping and activate the "Cancel" button.
ON KEY LABEL ESCAPE
_CUROBJ = OBJNUM(pbTrdCan)
KEYBOARD "{ENTER}"

*:----------------------------------------------------------------
*: Name       : lfTradTab
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : "Trade discount" screen trapping "TAB" button.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfTradTab()
*:----------------------------------------------------------------
FUNCTION lfTradTab

ON KEY LABEL Tab 
IF WONTOP() = (lcTrBrTtl)
  *-- If the browse is the window on top, activate the "Apply"
  *-- radio button.
  ACTIVATE WINDOW ArTrDs3
  _CUROBj = OBJNUM(rbApply)
ELSE
  *-- If the browse is not the window on top...
  IF _CUROBj = OBJNUM(pbTrdCan)
    *-- If the active object is the "Cancel" button, go to the browse.
    KEYBOARD "{ALT+B}"
  ELSE
    *-- If the active object is not the last one on the screen, go 
    *-- normal..
    _CUROBj = _CUROBj + 1
  ENDIF
ENDIF
ON KEY LABEL Tab DO lfTradTab

*:----------------------------------------------------------------
*: Name       : lfTradSTb
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : "Trade discount" screen trapping "SHIFT TAB" button.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfTradSTb()
*:----------------------------------------------------------------
FUNCTION lfTradSTb

ON KEY LABEL BACKTAB
IF WONTOP() = (lcTrBrTtl)
  *-- If the browse is the window on top, activate the "Cancel"
  *-- button.
  ACTIVATE WINDOW ArTrDs3
  _CUROBj = OBJNUM(pbTrdCan)
ELSE
  *-- If the browse is not the window on top...
  IF _CUROBj = OBJNUM(rbApply)
    *-- If the active object is the "Apply" radio button, 
    *-- go to the browse.
    KEYBOARD "{ALT+B}"
  ELSE
    *-- If the active object is not the first one on the screen, go 
    *-- normal..
    _CUROBj = _CUROBj - 1
  ENDIF
ENDIF
ON KEY LABEL BACKTAB DO lfTradSTb

*:----------------------------------------------------------------
*: Name       : lfTrdDisBr
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Trade discount browse.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfTrdDisBr()
*:----------------------------------------------------------------
FUNCTION lfTrdDisBr

lnAlias = SELECT(0)
SELECT TmpAll

*-- Browse fields.
lcFields = "Tran                   :08:R:H='Tran.#',"       +;
           "Desc                   :20:R:H='Description',"  +;
           "Amount                 :11:R:H='Amount',"       +;
           "nDisc=-1*Dsc_Amt       :13:R:H='Disc. Amount'," +;
           lcTrmFld                                         +;
           "TranDate               :     H='Tran. Date',"   +;
           "Reference              :     H='Reference'"
BROWSE FIELDS &lcFields ;
       FOR Account+TranType+Tran+cInstalNo = lcAccount AND ;
       TranType $ "01" AND Dsc_Amt # 0                     ;
       WINDOW ArTrDs2 IN WINDOW ARTRDS1 LOCK 0 SAVE NOWAIT ;
       NOEDIT NOCLEAR NOAPPEND NODELETE NOMENU TITLE (lcTrBrTtl)
SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfvChBkRp
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "Charge back sales rep" 
*:              button in the D\C Adjustment folder.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvChBkRp()
*:----------------------------------------------------------------
FUNCTION lfvChBkRp
PRIVATE lcAlias

*-- Define the screen's global variables...
lcAlias    = SELECT(0)
lnTradDis  = 0                     && Trade discount amount.
lnTradPer  = 0                     && Trade discount percentage.
lnCom1     = 0                     && Sales rep 1 commission.
lnCom2     = 0                     && Sales rep 2 commission.
lnChr1     = 0                     && Charge back for sales rep 1.
lnChr2     = 0                     && Charge back for sales rep 2.
lcInvNum   = SPACE(6)              && Invoice number.
lcRep1     = SPACE(0)              && Sales rep 1 code.
lcRep2     = SPACE(0)              && Sales rep 2 code.
lcRCBOrder = SPACE(0)              && Order Number.
lcRCBStore = SPACE(0)              && Store Code.
lcRCBCstPo = SPACE(0)              && Customer PO Number.
lnTotAmn   = ABS(lnAdjAmont)       && Total Amount
lnNetAmn   = ABS(lnAdjAmont)       && Net Amount

lcRepStat  = "ENABLE"              && Sales rep codes status.
lcComm1St  = "ENABLE"              && Sales rep 1 commission status.
lcComm2St  = "ENABLE"              && Sales rep 2 commission status.
lcTrdAmSt  = "ENABLE"              && Trade discount amount status.

*-- Open the sales rep file if it is not opened.
= gfOpenFile(gcDataDir+"SalesRep", "SalesRep", "SH")

*-- Open the sales rep commission file if it is not opened.
= gfOpenFile(gcDataDir+"RepComm", "RepComm", "SH")

*-- Create and open the rep commission temp file if it is 
*-- not created.
IF !FILE(gcWorkDir+lcRpComTmp+".DBF")
  SELECT RepComm
  lnFields = AFIELDS(laFields)
  DIMENSION laFields[lnFields+4,4]
  laFields[lnFields+1,1] = "cRepNum"
  laFields[lnFields+1,2] = "C"
  laFields[lnFields+1,3] = 1
  laFields[lnFields+1,4] = 0
  laFields[lnFields+2,1] = "cInvNum"
  laFields[lnFields+2,2] = "C"
  laFields[lnFields+2,3] = 6
  laFields[lnFields+2,4] = 0
  laFields[lnFields+3,1] = "nTradDis"
  laFields[lnFields+3,2] = "N"
  laFields[lnFields+3,3] = 11
  laFields[lnFields+3,4] = 2
  laFields[lnFields+4,1] = "nStep"
  laFields[lnFields+4,2] = "N"
  laFields[lnFields+4,3] = 2
  laFields[lnFields+4,4] = 0
  llNoThing = gfCrtTmp(lcRpComTmp, @laFields)
  USE IN (lcRpComTmp)
  USE (gcWorkDir+lcRpComTmp) IN 0 EXCLUSIVE
  SELECT(lcRpComTmp)
  INDEX ON Tran+cRepNum+RepCode TAG (lcRpComTmp)
  USE IN (lcRpComTmp)
  USE (gcWorkDir+lcRpComTmp) IN 0 SHARED ORDER (lcRpComTmp)
ENDIF

*-- Load the screen's variables from the temp file if there is any
*-- previous charge backs for the currend transaction.
SELECT (lcRpComTmp)
IF SEEK(&lcDCAdjTmp..Tran)
  SCAN REST WHILE &lcDCAdjTmp..Tran = &lcRpComTmp..Tran
    lcRepNum       = cRepNum
    lcInvNum       = cInvNum
    lnTradDis      = nTradDis
    lcRep&lcRepNum = RepCode
    lnCom&lcRepNum = CommPcnt
    lnChr&lcRepNum = Amount
    lcRCBOrder     = Order
    lcRCBStore     = Store
    lcRCBCstPo     = CustPo
  ENDSCAN
  *-- Compute the trade discount percentage, and net amount.
  llNoThing = lfGtTrdPer(.F.) 
  llNoThing = lfGtNtAmnt(.F.)
ENDIF

*-- Refresh the screen with the initial status after loading
*-- the variables from the temp file (if any).
llNothing = lfRefScrn (.F.)

*-- Clear the main screen trapping, and set the charge back
*-- screen trapping.
PUSH KEY
ON KEY
ON KEY LABEL ESCAPE DO lfRepEsc

*-- Run the screen.
*DO (gcScrDir+gcWinAppl+"\ARChBkRp.SPR")
DO (gcScrDir+"ARChBkRp.SPR")

*-- Restore the main screen trapping.
POP KEY

*-- Set the status of the charge back sales rep check box.
cbCharBakS = SEEK(&lcDCAdjTmp..Tran, lcRpComTmp)
SHOW GET cbCharBakS

*-- Update the incomplete session file.
llNotThing = lfUpdVars()

SELECT(lcAlias)

*:----------------------------------------------------------------
*: Name       : lfvChBkRp
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Charge back screen trapping (Escape Button).
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfRepEsc()
*:----------------------------------------------------------------
FUNCTION lfRepEsc

*-- Activate the "Cancel" button.
ON KEY LABEL ESCAPE
_CUROBJ = OBJNUM(pbChrBkCan)
KEYBOARD "{ENTER}"

*:----------------------------------------------------------------
*: Name       : lfRefScrn
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Refresh the "Charge back Sales Rep" screen objects.
*:----------------------------------------------------------------
*: Parameters : llRef : .T. -> Refresh the objects.
*:                      .F. -> Just update the "Disable\Enable"
*:                             initial status of the objects.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfRefScrn()
*:----------------------------------------------------------------
FUNCTION lfRefScrn
PARAMETERS llRef

*-- Update the "Disable\Enable" initial status of the objects.
lcTrdAmSt = IIF(EMPTY(lcInvNum), "ENABLE" , "DISABLE")
lcRepStat = IIF(EMPTY(lcInvNum), "ENABLE" , "DISABLE")
lcComm1St = IIF(EMPTY(lcRep1)  , "DISABLE", "ENABLE" )
lcComm2St = IIF(EMPTY(lcRep2)  , "DISABLE", "ENABLE" )

*-- Refresh the objects if wanted to..
IF llRef
  llNoThing = lfCBRRef()                && Refresh the says.
  SHOW GET lcRep1    &lcRepStat
  SHOW GET lcRep2    &lcRepStat
  SHOW GET pbBrRp1   &lcRepStat
  SHOW GET pbBrRp2   &lcRepStat
  SHOW GET lnCom1    &lcComm1St
  SHOW GET lnCom2    &lcComm2St
  SHOW GET lnTradDis &lcTrdAmSt
  SHOW GET lnTradPer &lcTrdAmSt
ENDIF

*:----------------------------------------------------------------
*: Name       : lfInvBrow
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : To browse all the invoices for a specfic account.
*:----------------------------------------------------------------
*: Parameters : lcInvoice : Pointer to the invoice number variable.
*:              lcAccount : Account code to filter the browsed
*:                          invoices on.
*:----------------------------------------------------------------
*: Returns    : .T. -> The user selected an invoice.
*:              .F. -> The user did not select.
*:----------------------------------------------------------------
*: Example    : llSelected = lfInvBrow(@lcInvoice, lcAccount)
*:----------------------------------------------------------------
FUNCTION lfInvBrow
PARAMETERS lcInvoice,lcAccount
PRIVATE lnAlias, lnCurrTag, llRetVal, lcBrowTitl, lcFile_Ttl, lcBrFields

*-- Browse title.
lcFile_Ttl = 'Invoices'
llRetVal   = .F.

*-- Check if there is any invoices in the invoice header file.
GO TOP IN 'INVHDR'
IF EOF('INVHDR')
  *-- The invoice file is empty.
  llNoThing = gfModalGen('INM40036B00000')
  lcInvoice = SPACE(6)
ELSE
  lnAlias = SELECT(0)

  *-- If there is any invoices for this account..
  SELECT INVHDR
  IF SEEK(lcAccount)
    GO TOP

    *-- Browse these fields.
    lcBrFields = "Invoice, InvDate, Account, Store, Order, Ship, "+;
                 "TotalChg:H='Total Charge'"                      +;
                 IIF(llMultiWar, ",cWareCode:H='Warehouse'", "")  +;
                 IIF(llMltCur, ",nExRate:H='Exchange Rate'", "")
    llRetVal   = AriaBrow([lcAccount], lcFile_Ttl, gnBrFSRow1, ;
                          gnBrFSCol1, gnBrFSRow2 , gnBrFSCol2, ;
                          '', '', 'Invoice','laBrowArr')
    
    *-- If the user selects any invoce, update the invoice variable.
    lcInvoice  = IIF(llRetVal, InvHdr.Invoice, SPACE(6))
  ELSE
    *-- If there is no invoices for that account...
    *-- No invoices found for account : XXXXX
    *-- < Ok >
    lcStr     = 'for account : ' + lcAccount
    llNoThing = gfModalGen('INM40084B00000','ALERT',lcStr)
    lcInvoice = SPACE(6)
  ENDIF
  SELECT (lnAlias)
ENDIF
RETURN (llRetVal)

*:----------------------------------------------------------------
*: Name       : lfGtChrBk
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Compute the charge back amount based on a 
*:              commission.
*:----------------------------------------------------------------
*: Parameters : lnCom : The commission to be used.
*:----------------------------------------------------------------
*: Returns    : The computed charge back.
*:----------------------------------------------------------------
*: Example    : lnChrBack = lfGtChrBk(120)
*:----------------------------------------------------------------
FUNCTION lfGtChrBk
PARAMETERS lnCom
RETURN -1 * ABS(ROUND(lnNetAmn*(lnCom/100),2))

*:----------------------------------------------------------------
*: Name       : lfGtChrBk
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Compute the trade discount percentage based on the
*:              trade discount amount.
*:----------------------------------------------------------------
*: Parameters : llRef : .T. -> Please refresh the screen after
*:                             calculating the percentage.
*:                      .F. -> Do not refresh the screen after
*:                             calculating the percentage.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfGtTrdPer(.T.)
*:----------------------------------------------------------------
FUNCTION lfGtTrdPer
PARAMETERS llRef

*-- Calculat the percentage.
lnTradPer = lnTradDis*100/lnTotAmn

*-- Refresh the screen if asked for..
llNoThing = IIF(llRef, lfCBRRef(), .T.) 
SHOW GET lnTradPer

*:----------------------------------------------------------------
*: Name       : lfGtNtAmnt
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Calculate the net amount based on the
*:              trade discount amount and the total amount.
*:----------------------------------------------------------------
*: Parameters : llRef : .T. -> Please refresh the screen after
*:                             calculating the net amount.
*:                      .F. -> Do not refresh the screen after
*:                             calculating the net amount.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfGtNtAmnt(.T.)
*:----------------------------------------------------------------
FUNCTION lfGtNtAmnt
PARAMETERS llRef

*-- Calculat the amount.
lnNetAmn  = lnTotAmn - lnTradDis

*-- Refresh the screen if asked for..
llNoThing = IIF(llRef, lfCBRRef(), .T.) 

*:----------------------------------------------------------------
*: Name       : lfvCBRInv
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the invoice field in the 
*:              "Charge Back Sales Rep" screen.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvCBRInv()
*:----------------------------------------------------------------
FUNCTION lfvCBRInv

*E301077,53 YMA 03/03/99 Start - Open the invoice header file.
= gfOpenFile(gcDataDir+"InvHdr","InvHdrA",'SH')
*E301077,53 YMA 03/03/99 End.

*-- If the invoice browse button is pressed, or the invoice number
*-- changed..
IF llFrmBrow OR (lcInvNum#lcOldValue AND LASTKEY()=13)
  
  *-- If the invoice browse button is pressed, the invoice number
  *-- is not valid, browse all the customer invoices.
  IF llFrmBrow OR (!EMPTY(lcInvNum) AND !SEEK(lcAccount+lcInvNum, "InvHdr"))
    llFrmBrow = lfInvBrow (@lcInvNum, lcAccount) AND .F.
    IF EMPTY(lcInvNum)
      lcInvNum = lcOldValue
      _CUROBJ  = _CUROBJ 
      RETURN
    ENDIF
  ENDIF

  DO CASE
    *-- If the blanked out the invoice number...
    CASE !EMPTY(lcOldValue) AND EMPTY(lcInvNum)
      *-- You have blanked the invoice number, would you 
      *-- like to replace current sales rep. information 
      *-- with customer defaults.
      *-- < Yes > < No >
      lnChoice = gfModalGen('QRM40096B40000')

    *-- If the user entered a valid invoice number...
    CASE EMPTY(lcOldValue) AND !EMPTY(lcInvNum)
      *-- You have entered an invoice number, the current 
      *-- sales rep. information will be replaced with the 
      *-- invoice values!
      *-- < Ok >
      lnChoice = gfModalGen('TRM40097B00000')
      
    *-- If the user left the invoice number blank...
    CASE EMPTY(lcOldValue) AND EMPTY(lcInvNum)
      *-- Would you like to replace the sales rep. 
      *-- information with the customer defaults.
      *-- < Yes > < No >
      lnChoice = gfModalGen('QRM40098B40000')

    *-- If the invoice number still the same...
    CASE !EMPTY(lcOldValue) AND !EMPTY(lcInvNum)
      lnChoice = IIF(lcOldValue=lcInvNum, 5, 1)
  ENDCASE
  
  *-- If the user selectes "Yes" from the previous set of
  *-- messages, or the only button in the message was < Ok >.
  IF lnChoice = 1
    
    *-- Be sure that we are dealing with the correct customer record
    *-- in the customer file, and the correct invoice record in the 
    *-- invoice file..
    llNoThing = SEEK("M"+lcAccount, "Customer")
    lcInvNum  = IIF(SEEK(lcAccount+lcInvNum,"InvHdr"), lcInvNum, SPACE(6))
    
    *-- If the invoice number is empty, load the customer defaults, 
    *-- otherwise load the invoice information.
    IF EMPTY(lcInvNum)
      lcRep1     = Customer.SalesRep
      lcRep2     = Customer.Rep2
      lnCom1     = Customer.Comm
      lnCom2     = Customer.Comm2 
      lcRCBOrder = SPACE(06)
      lcRCBStore = SPACE(08)
      lcRCBCstPo = SPACE(10)

      *-- Get the trade discount information related to the 
      *-- term code defaulted.       
      DIMENSION laTermAry[1,2]
      laTermAry[1,1] = "NTERDISCR"
      laTermAry[1,2] = "lnTradPer"
      llNoThing = gfRltFld(Customer.cTermCode, @laTermAry, "CTERMCODE")
    ELSE
      lcRep1     = InvHdr.Rep1
      lcRep2     = InvHdr.Rep2
      lnCom1     = InvHdr.Comm1
      lnCom2     = InvHdr.Comm2
      lcRCBOrder = InvHdr.Order
      lcRCBStore = InvHdr.Store
      lcRCBCstPo = InvHdr.CustPo
      lnTradPer  = InvHdr.Trde_Disc
    ENDIF

    *-- Calculate the net amount.
    lnTradDis = lnTotAmn*lnTradPer/100
    llNoThing = lfGtNtAmnt(.F.)
    
    *-- Compute the reps charge back amounts.
    lnChr1    = lfGtChrBk(lnCom1)
    lnChr2    = lfGtChrBk(lnCom2)
  ENDIF
  
  *-- Refresh the screen's objects.
  llNoThing = lfRefScrn(.T.)
ENDIF  

*:----------------------------------------------------------------
*: Name       : lfvCBRDisA
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "Trade Discount Amount" 
*:              field in the "Charge Back Sales Rep" screen.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvCBRDisA()
*:----------------------------------------------------------------
FUNCTION lfvCBRDisA

*-- Compute the trade discount percentage, and the net amount.
llNoThing = lfGtTrdPer(.F.) 
llNoThing = lfGtNtAmnt(.F.)

*-- Compute the default reps charge backs.
lnChr1    = lfGtChrBk(lnCom1)
lnChr2    = lfGtChrBk(lnCom2)

*-- Refresh the says.
llNoThing = lfCBRRef()

*:----------------------------------------------------------------
*: Name       : lfvCBRDisP
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "Trade Discount percentage" 
*:              field in the "Charge Back Sales Rep" screen.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvCBRDisP()
*:----------------------------------------------------------------
FUNCTION lfvCBRDisP

*-- Compute the trade discount percentage, and the net amount.
lnTradDis = lnTotAmn*lnTradPer/100
llNoThing = lfGtNtAmnt(.F.)

*-- Compute the default reps charge backs.
lnChr1    = lfGtChrBk(lnCom1)
lnChr2    = lfGtChrBk(lnCom2)

*-- Refresh the screen.
SHOW GET lnTradDis
= lfCBRRef()

*:----------------------------------------------------------------
*: Name       : lfvCBRRep
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "Sale Rep Codes" 
*:              in the "Charge Back Sales Rep" screen.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvCBRRep()
*:----------------------------------------------------------------
FUNCTION lfvCBRRep

*-- Determine the currently edited rep code, and its commission,
*-- charge back values.
lcRead   = SYS(18)
lcVal    = RIGHT(lcRead, 1)
lcComVar = "lnCom" + lcVal
lcChrVar = "lnChr" + lcVal
lcValue  = &lcRead

*-- If the rep code browse key is pressed, or the rep ceode is
*-- invalid, browse all the sales reps..
IF llFrmBrow OR (!EMPTY(lcValue) AND !SEEK(lcValue, "SalesRep"))
  llBrowse = llFrmBrow
  DO RepChk WITH lcValue, .T.
  STORE .F. TO llFrmBrow, llBrowse
  IF EMPTY(lcValue)
    &lcRead = lcOldValue
    _CUROBJ = _CUROBJ
  ELSE
    &lcRead = lcValue
  ENDIF
ENDIF

*-- Compute the commission and the charge back values, and update
*-- there status..
lcComm    = IIF(EMPTY(&lcRead), "DISABLE", "ENABLE")
&lcComVar = IIF(EMPTY(&lcRead), 0        , &lcComVar)
&lcChrVar = lfGtChrBk(&lcComVar)

*-- Refresh the screen.
llNoThing = lfCBRRef()
SHOW GET &lcRead
SHOW GET &lcComVar &lcComm

*:----------------------------------------------------------------
*: Name       : lfvCBROk
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "Ok"  button
*:              in the "Charge Back Sales Rep" screen.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvCBROk()
*:----------------------------------------------------------------
FUNCTION lfvCBROk
PRIVATE lnAlias

*-- Be sure to clear all the charge backs records that might
*-- have been entered for this transaction.
lnAlias   = SELECT(0)
llNoThing = lfvCBRClr(&lcDCAdjTmp..Tran)

*-- If one of the sales reps has a charge back amount...
IF lnChr1 # 0 OR lnChr2 # 0
  
  *-- Loop on the two reps
  FOR lnRepNum = 1 TO 2
    lcRepNum = STR(lnRepNum,1)

    *-- Update only for the not empty rep codes..
    IF !EMPTY(lcRep&lcRepNum)
      SELECT (lcRpComTmp)
      APPEND BLANK
      llNoThing = RLOCK()
      llNoThing = gfAdd_Info()
      REPLACE RepCode    WITH lcRep&lcRepNum             ,; 
              cRepNum    WITH lcRepNum                   ,;
              nTradDis   WITH lnTradDis                  ,;
              cInvNum    WITH lcInvNum                   ,;
              Tran       WITH &lcDCAdjTmp..Tran          ,;
              Order      WITH lcRCBOrder                 ,;
              Account    WITH lcAccount                  ,;
              Store      WITH lcRCBStore                 ,;
              CustPo     WITH lcRCBCstPo                 ,;
              CommPcnt   WITH lnCom&lcRepNum             ,;
              Amount     WITH lnChr&lcRepNum             ,;
              Date       WITH ldKODate                   ,;
              Desc       WITH "CREDIT ADJ.  " + lcInvNum ,;
              TranType   WITH "5"                        ,;
              Status     WITH "O"                        ,;
              cCurrCode  WITH lcCurCode                  ,;
              nExRate    WITH lnExRate                   ,;
              nCurrUnit  WITH lnCurrUnit
      UNLOCK
    ENDIF
  ENDFOR
ENDIF
SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfvCBRClr
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "Clear"  button
*:              in the "Charge Back Sales Rep" screen.
*:----------------------------------------------------------------
*: Parameters : lcTranCode : Transaction code we are working
*:                           with.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvCBRClr("000124")
*:----------------------------------------------------------------
FUNCTION lfvCBRClr
PARAMETERS lcTranCode
PRIVATE lnAlias

lnAlias = SELECT(0)

*-- If there is any charge backs for this transaction...
SELECT (lcRpComTmp)
IF SEEK(lcTranCode)
  *-- Go and clear it..
  BLANK ALL FOR Tran+cRepNum+RepCode = lcTranCode
  DELETE ALL FOR EMPTY(Tran)
ENDIF

SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfvAccNew
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "New" button
*:              in the "Debit\Credit On Account" folder.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvAccNew()
*:----------------------------------------------------------------
FUNCTION lfvAccNew
*E301276,01 (Start)
PARAMETERS llAutoKey
PRIVATE llAutoKey
*E301276,01 (End)
PRIVATE lnAlias


*B803369,1 (Start) the default factor.
IF !llAutoKey
  lcAccFact=lcCusFact
ENDIF
*B803369,1 (End) 
*-- Look for any empty record in the temp file..
lnAlias = SELECT(0)
SELECT (lcDCOnATmp)
LOCATE FOR EMPTY(TranType)

*-- If not found, add a new one, and replace the fields with the
*-- defaults.
IF !FOUND()
  APPEND BLANK
  llNoThing = RLOCK()
  REPLACE Account    WITH lcAccount              ,;
          Tran       WITH 'T'+SUBSTR(SYS(3),4,5) ,;
          TranDate   WITH ldKODate               ,;
          Amount     WITH 0                      ,;
          cCurrCode  WITH lcCurCode              ,;
          nExRate    WITH lnExRate               ,;
          nCurrUnit  WITH lnCurrUnit
  UNLOCK
ENDIF  

*-- Refresh the screen, and set the focus on the type popuop.

*E301276,01 (Start)
IF !llAutoKey
*E301276,01 (End)

  llNoThing  = lfwDCAccBr()
  _CUROBJ    = OBJNUM(lnAccType)

*E301276,01 (Start)
ENDIF
*E301276,01 (End)

SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfvAccRem
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "Remove" button
*:              in the "Debit\Credit On Account" folder.
*:----------------------------------------------------------------
*: Parameters : llDispMsg  : Would you like to display the 
*:                           confirmation message or not.
*:              llSekInAll : Would you like to seek for the 
*:                           orignal transaction for the open 
*:                           balance record in the "All Transactions"
*:                           temp file (if any) or not?
*:              These two parameters will be true if the function is 
*:              called from the screen, otherwise it will be false.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvAccRem(.T., .T.)
*:----------------------------------------------------------------
FUNCTION lfvAccRem

*E301276,01 (Start)
*PARAMETERS llDispMsg, llSekInAll
PARAMETERS llDispMsg, llSekInAll, llAutoKey
PRIVATE llAutoKey
*E301276,01 (End)
*-- Display the confirmation message if you are asked to.
llDelRec  = .T.
IF llDispMsg
  *-- Are you sure, You want to delete this transaction ?
  *-- < Yes >   < No > 
  llDelRec = gfModalGen('QRM40077B40000') = 1
ENDIF  

*-- If the user confirem the deletion..
IF llDelRec
  lnAlias = SELECT(0)
  
  *-- Update the total selected D\C on account transactions.
  SELECT (lcDCOnATmp)
  llNoThing = IIF(Amount=0, .T., lfUpdAccBal(-1, Amount, llSekInAll))

  *-- If this record is an open balance record, and the original
  *-- transaction code for this open balance exists, you are asked
  *-- to update the original transaction record, then go and update
  *-- it.
  IF !EMPTY(cOpnBalTrn) AND llSekInAll
    lcExp = Account+cOpnTrnTyp+cOpnBalTrn+cInstalNo
    IF SEEK(lcExp, lcDCAllTmp)
      SELECT (lcDCAllTmp)
      llNoThing = RLOCK()
      REPLACE cShToOpn WITH "Y"
      UNLOCK
    ENDIF
  ENDIF
 
  *-- Delete the selected record.
  SELECT (lcDCOnATmp)
  BLANK
  DELETE
  GOTO TOP
  
  *-- Refresh the browse.
  *E301276,01 (Start)
  *llNoThing = IIF(llSekInAll, lfwDCAccBr(), .F.) 
  llNoThing = IIF(llSekInAll AND !llAutoKey, lfwDCAccBr(), .F.) 
  *E301276,01 (End)
  SELECT(lnAlias)  
ENDIF

*:----------------------------------------------------------------
*: Name       : lfvAccType
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "Type" popup
*:              in the "Debit\Credit On Account" folder.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvAccType()
*:----------------------------------------------------------------
FUNCTION lfvAccType

*-- If the user selected anything rather that "N\A".
IF lnOAccType # lnAccType AND lnAccType < 4
  lnAlias = SELEC(0)
  SELECT (lcDCOnATmp)
  
  IF lnAccType = 3
    *-- If "Open Balance".
    = lfAddOpen()
  ELSE
    *-- If "Debit\Credit On Account".
    
    *-- Default the reason code, and the transaction dates.
    lcDCAcc    = laAccCodIn[lnAccType,1]
    llNoThing  = gfwCodePop (@laAccCodIn, lcDCAcc, "D")
    llNoThing  = RLOCK()
    *B803369,1 (Start) Update the default factor in the temp file .
    *REPLACE TranType   WITH SUBSTR("36*",lnAccType,1)               ,;
            TranCode   WITH laDCAcc[lnDCAcc,2]                      ,;
            cReason    WITH laDCAcc[lnDCAcc,1]                      ,;
            ChgBk_Date WITH IIF(TranType="3", ldKODate, ChgBk_Date) ,;
            Credt_Date WITH IIF(TranType="6", ldKODate, Credt_Date) ,;
            cFacCode   WITH lcCusFact
    REPLACE TranType   WITH SUBSTR("36*",lnAccType,1)               ,;
            TranCode   WITH laDCAcc[lnDCAcc,2]                      ,;
            cReason    WITH laDCAcc[lnDCAcc,1]                      ,;
            ChgBk_Date WITH IIF(TranType="3", ldKODate, ChgBk_Date) ,;
            Credt_Date WITH IIF(TranType="6", ldKODate, Credt_Date) ,;
            cFacCode   WITH lcAccFact
   *B803369,1 (End) 
    
    UNLOCK
  
  
  ENDIF
  
  *-- Refresh the screen.
  llNoThing  = lfwDCAccBr()
  SELEC(lnAlias)
ENDIF  

*:----------------------------------------------------------------
*: Name       : lfAddOpen
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : To add an "Open Balance" record in the temp file.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfAddOpen()
*:----------------------------------------------------------------
FUNCTION lfAddOpen
*E301276,01 (Start)
PARAMETERS lcKeyExp
PRIVATE lcKeyExp
*E301276,01 (End)

PRIVATE lnAlias

*-- Use the "All Selected Transactions" file in another
*-- alias.

lnAlias  = SELECT(0)
USE (gcWorkDir+lcDCAllTmp) IN 0 AGAIN ALIAS TmpAll ORDER Show

*-- Browse all the selected transactions for the user to pick up one.
lnTranAmn = 0
lcTrnType = SPACE(0)

*E301276,01 (Start)
*lcAccTran = lfTranBr()
IF TYPE('lcKeyExp') = 'C'
  PRIVATE lcTmpAlTag
  lcTmpAlTag = ORDER('TmpAll')
  SET ORDER TO lcDCAllTmp IN TmpAll
  lcAccTran = IIF(SEEK(lcKeyExp,'TmpAll'),TmpAll.Tran,SPACE(0))
  SET ORDER TO &lcTmpAlTag IN TmpAll
ELSE
  lcAccTran = lfTranBr()
ENDIF
*E301276,01 (End)
  

IF EMPTY(lcAccTran)
  *-- If the user did not select any transaction, delete this record.
  SELECT (lcDCOnATmp)
  BLANK
  DELETE
  GOTO TOP
ELSE
  *-- If the user selected a transaction...
  *-- Flag the selected record not to redisplay it again in 
  *-- future.
  SELECT TmpAll
  llNoThing = RLOCK()
  REPLACE TmpAll.cShToOpn WITH "N"
  UNLOCK

  *-- Update the "D\C On Account" record.
  SELECT (lcDCOnATmp)
  llNoThing = RLOCK()
  *B606627,1 RAE Add TranType = "M" in case of Invoice Material Sales Order.[start]
  *REPLACE TranType   WITH "*"              ,;
          cOpnTrnTyp WITH TmpAll.TranType  ,;
          nOpnTrnAmn WITH TmpAll.Amount    ,;
          Desc       WITH TmpAll.Desc      ,;
          TranDate   WITH TmpAll.TranDate  ,;
          Store      WITH TmpAll.Store     ,;
          Reference  WITH TmpAll.Reference ,;
          cInstalNo  WITH TmpAll.cInstalNo ,;
          cOpnBalTrn WITH lcAccTran        ,;
          cOpenType  WITH IIF(cOpnTrnTyp $ "123", "3", "6")

  REPLACE TranType   WITH "*"              ,;
          cOpnTrnTyp WITH TmpAll.TranType  ,;
          nOpnTrnAmn WITH TmpAll.Amount    ,;
          Desc       WITH TmpAll.Desc      ,;
          TranDate   WITH TmpAll.TranDate  ,;
          Store      WITH TmpAll.Store     ,;
          Reference  WITH TmpAll.Reference ,;
          cInstalNo  WITH TmpAll.cInstalNo ,;
          cOpnBalTrn WITH lcAccTran        ,;
          cOpenType  WITH IIF(cOpnTrnTyp $ "123M", "3", "6")
  *B606627,1 RAE [end]
  UNLOCK
  
  *-- Compute the default "Open Balance" amount.
  llDifSin   = IIF(lnAccDiffr    < 0, "N", "P")
  llTrnSin   = IIF(TmpAll.Amount < 0, "N", "P")
  lnAmont    = MIN(ABS(TmpAll.Amount), ABS(lnAccDiffr))
  lcOldValue = 0
  
  *-- Default the amount only if the computed default value
  *-- is less than the original transaction amount...
  IF llTrnSin = llDifSin AND ABS(TmpAll.Amount) # ABS(lnAmont)
    lnAccAmont = lnAmont * IIF(lcTrnType$"123", 1, -1)
    llNoThing  = lfvAccAmn()
  ENDIF
ENDIF

*-- Close the opened file.
USE IN TmpAll
SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfTranBr
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Browse all the selected transactions.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : The transaction number selected.
*:----------------------------------------------------------------
*: Example    : = lfTranBr()
*:----------------------------------------------------------------
FUNCTION lfTranBr
PRIVATE lnAlias, lcFile_Ttl

lnAlias  = SELECT(0)
lcRetVal = SPACE(0)

*-- If there is any selected transactions.
SELECT TmpAll
IF SEEK("Y")
  lcKey      = "Y"
  
  *-- Browse the following fields, using this title..
  lcFile_Ttl = "Keyed Off Trransactions"
  lcBrFields = "Tran     :08:R:H='Tran #'      ,"+;
               "Desc     :20:R:H='Description' ,"+;
               "Amount   :11:R:H='Amount'      ,"+;
               "TranDate    :R:H='Tran. Date'  ,"+;
               "Store       :R:H='Store'       ,"+;
               "Reference   :R:H='Reference'"
  llRetVal   = AriaBrow([lcKey], lcFile_Ttl, gnBrFSRow1, ;
                        gnBrFSCol1, gnBrFSRow2 , gnBrFSCol2, ;
                        '', '', 'Tran','laBrowArr')

  *-- If the user selected any transaction, return the transaction
  *-- number, otherwise do not return any thing.
  lcRetVal   = IIF(llRetVal, TmpAll.Tran, SPACE(0))
ELSE
  *-- No selected transactions to display.
  *-- < Ok >
  = gfModalGen('TRM40092B00000')
ENDIF

SELECT(lnAlias)

RETURN (lcRetVal)

*:----------------------------------------------------------------
*: Name       : lfvAccCode
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "Reason" Popup
*:              in the "Debit\Credit On Account" folder.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvAccCode()
*:----------------------------------------------------------------
FUNCTION lfvAccCode
PRIVATE lnAlias

lnAlias = SELECT(0)

*-- Update the transaction code, and the reason description with
*-- the selected code information.
SELECT (lcDCOnATmp)
llNoThing = RLOCK()
REPLACE TranCode WITH laDCAcc[lnDCAcc,2] ,;
        cReason  WITH laDCAcc[lnDCAcc,1]
        
*B802622,1 ABD Add the decscription of the reason code to the decscription filed by default
*B802622,1 ABD When the user select the reason decscription. [Begin]
REPLACE DESC WITH laDCAcc[lnDCAcc,1]
lcAccDes = DESC
SHOW GET lcAccDes
*B802622,1 ABD [End]
        
UNLOCK
SHOW WINDOW (lcDCAdjTtl) REFRESH
SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfvAccDate
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "Date" field
*:              in the "Debit\Credit On Account" folder.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvAccDate()
*:----------------------------------------------------------------
FUNCTION lfvAccDate
PRIVATE lnAlias

*-- If the date field has been changed...
IF !(lcOldValue == ldAccDate)
  
  *-- The transaction date should be less than the key-off date.
  IF ldAccDate > ldKODate
    *-- The transaction date cannot be greater than the 
    *-- key-off date : 99/99/99
    *-- < Ok >
    = gfModalGen('TRM40099B00000', "ALERT", DTOC(ldKODate))
    ldAccDate = lcOldValue
    _CUROBJ   = _CUROBJ
  ELSE
    *-- Update the date field..
    lnAlias = SELECT(0)
    SELECT (lcDCOnATmp)
    llNoThing = RLOCK()
    REPLACE TranDate WITH ldAccDate
    UNLOCK
    SHOW WINDOW (lcDCAccTtl) REFRESH
    SELECT(lnAlias)
  ENDIF
ENDIF

*:----------------------------------------------------------------
*: Name       : lfvAccStor
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "Store" field
*:              in the "Debit\Credit On Account" folder.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvAccStor()
*:----------------------------------------------------------------
FUNCTION lfvAccStor
PRIVATE lnAlias

*-- If the store has been changed...
IF !(lcOldValue == lcAccStore)
  lnAlias = SELECT(0)
  
  *-- Update the new value in the file.
  SELECT (lcDCOnATmp)
  llNoThing = RLOCK()
  REPLACE Store WITH lcAccStore
  UNLOCK
  SHOW WINDOW (lcDCAccTtl) REFRESH
  SELECT(lnAlias)
ENDIF

*:----------------------------------------------------------------
*: Name       : lfvAccRef
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "Reference" field
*:              in the "Debit\Credit On Account" folder.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvAccRef()
*:----------------------------------------------------------------
FUNCTION lfvAccRef
PRIVATE lnAlias

*-- If the reference has been changed...
IF !(lcOldValue == lcAccRef)
  lnAlias = SELECT(0)
  SELECT (lcDCOnATmp)

  *-- Update the new value in the file.
  llNoThing = RLOCK()
  REPLACE Reference WITH lcAccRef
  UNLOCK
  SHOW WINDOW (lcDCAccTtl) REFRESH
  SELECT(lnAlias)
ENDIF

*:----------------------------------------------------------------
*: Name       : lfvAccDes
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "Description" field
*:              in the "Debit\Credit On Account" folder.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvAccDes()
*:----------------------------------------------------------------
FUNCTION lfvAccDes
PRIVATE lnAlias

*-- If the description has been changed...
IF !(lcOldValue == lcAccDes)
  lnAlias = SELECT(0)
  SELECT (lcDCOnATmp)

  *-- Update the new value in the file.
  llNoThing = RLOCK()
  REPLACE Desc WITH lcAccDes
  UNLOCK
  SHOW WINDOW (lcDCAccTtl) REFRESH
  SELECT(lnAlias)
ENDIF

*:----------------------------------------------------------------
*: Name       : lfvAccFact
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "Factor" field
*:              in the "Debit\Credit On Account" folder.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvAccFact()
*:----------------------------------------------------------------
FUNCTION lfvAccFact

*-- If the factor has been changed, or the factor browse
*-- is pressed.
IF llFrmBrow OR !(lcOldValue == lcAccFact)
  
  *E301077,53 YMA 03/03/99 Start - Open the Factor file.
  = gfOpenFile(gcSysHome+"SYCFact","cFacCode",'SH')
  *E301077,53 YMA 03/03/99 End.
  
  *-- If the factor browse is pressed, or the factor code is
  *-- not valid.
  IF llFrmBrow OR (!EMPTY(lcAccFact) AND !SEEK(lcAccFact, "SYCFact"))
    llBrowse  = .T.
    
    *-- Browse the factors.
    lcAccFact = lfBrFactor()
    IF EMPTY(lcAccFact)
      lcAccFact = lcOldValue
      _CUROBJ   = _CUROBJ
    ENDIF
  ENDIF
  
  *-- Update the new factor field in the file.
  llFrmBrow = .F.
  lnAlias   = SELECT(0)
  SELECT (lcDCOnATmp)
  llNoThing = RLOCK()
  REPLACE cFacCode  WITH lcAccFact
  UNLOCK
  *E500374,5  HBG  08/15/2000 Save the factore in All transaction temp file [Begin]
  SELECT (lcDCAllTmp)
  = RLOCK()
  REPLACE cFacCode WITH lcAccFact
  UNLOCK
  *E500374,5 [End]
  SHOW WINDOW (lcDCAccTtl) REFRESH
  SELECT(lnAlias)
ENDIF

*:----------------------------------------------------------------
*: Name       : lfBrFactor
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Factor browse...
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfBrFactor()
*:----------------------------------------------------------------
FUNCTION lfBrFactor
PRIVATE lnAlias, lcRetVal, lcBrowTitl, lcFile_Ttl, lcBrFields

lnAlias    = SELECT(0)
lcRetVal   = SPACE(0)

*-- Browse these fields, with this title..
lcFile_Ttl = "Factors"
lcBrFields = "cFacCode:H='Factor',"                  +;
             "cFacComp:H='Company',"                 +;
             "cPhoneNo:H='Phone':P=GFPHONETEM():18," +;
             "cFaxNo  :H='Fax'  :P=GFPHONETEM():18"

SELECT SYCFact
GO TOP
*-- If the file is empty..
IF EOF()
  *-- No factors found
  *-- < Ok >
  = gfModalGen('TRM40100B00000')
ELSE
  *-- Browse and return the selected record (if any).
  DECLARE laTemp[1]
  llRetVal  = gfBrows(.F., 'cFacCode', 'laTemp')                       
  lcRetVal  = IIF(llRetVal, SYCFact.cFacCode, SPACE(6))
ENDIF

SELECT(lnAlias)
RETURN (lcRetVal)

*:----------------------------------------------------------------
*: Name       : lfvAccAmn
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "Amount" field
*:              in the "Debit\Credit On Account" folder.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvAccAmn()
*:----------------------------------------------------------------
FUNCTION lfvAccAmn
PRIVATE lnAlias

*-- If the amount has been changed..
IF !(lcOldValue == lnAccAmont)
  lnAlias = SELECT(0)
  SELECT (lcDCOnATmp)
  
  *-- Adjust the amount sign according to the transaction type.
  lcChkTyp   = IIF(TranType = "*", cOpenType, TranType)
  lnAccAmont = IIF(lcChkTyp = "3", +1, -1) * ABS(lnAccAmont)
   

  *E301429,1 (Start) To prevenet bug numeric over flow
  PRIVATE lnMaxAmt
  lnMaxAmt=IIF(lcChkTyp = "3",99999999999.00,9999999999.00)
  IF ABS(lnAccAmont) >lnMaxAmt
    * Message XXXXX cannot exceed 9999999999
    =gfModalGen("TRM40171BOOOO" , "DIALOG", 'Amount' +'|'+ALLTRIM(STR(lnMaxAmt,11)))
    lnAccAmont =  lcOldValue
    RETURN
  ENDIF 
  *E301429,1 (End)     
  
  *-- If the transaction is "Open Balance" and the open
  *-- balance is grater than or equal to the original transaction 
  *-- amount, display a message and resore the old amount.
  IF TranType = "*" AND ABS(lnAccAmont) >= ABS(nOpnTrnAmn)
    *-- The open balance amount '99.99', cannot be greater 
    *-- or equal to the invoice amount '99.99'.
    *-- < Ok >
    lcAccAmont = ALLTRIM(STR(lnAccAmont,11,2))
    lcTrnAmont = ALLTRIM(STR(nOpnTrnAmn,11,2))
    lcToSend   = lcAccAmont + "|" + lcTrnAmont
    lnNoThing  = gfModalGen('TRM40101B00000', "ALERT", lcToSend)
    lnAccAmont = lcOldValue
    _CUROBJ    = _CUROBJ
  ELSE
    *-- If the transaction is valid, update the selected D\C on
    *-- account total transactions, and update the field with
    *-- the new amount.
    llNoThing = lfUpdAccBal(-1, Amount    , .F.)
    llNoThing = lfUpdAccBal(+1, lnAccAmont, .T.)
    llNoThing = RLOCK()
    REPLACE Amount WITH lnAccAmont
    UNLOCK
    
    *-- Update the corresponding record in the "All Selected TRansaction"
    *-- file for the open balance original transaction with the new
    *-- open balance amount.
    IF TranType = "*" AND SEEK(Account+cOpnTrnTyp+cOpnBalTrn+cInstalNo, lcDCAllTmp)
      SELECT(lcDCAllTmp)
      llNoThing = RLOCK()
      REPLACE &lcDCAllTmp..nTrnNewAmn WITH lnAccAmont
      UNLOCK
    ENDIF
    
    *E301281,1 (Start)
    IF !llExtAuto
    *E301281,1 (End)
      *-- Refresh the browse.
      SHOW WINDOW (lcDCAccTtl) REFRESH
    *E301281,1 (Start)
    ENDIF
    *E301281,1 (End)
      
  ENDIF
  SELECT(lnAlias)
ENDIF

*:----------------------------------------------------------------
*: Name       : lfUpdAccBal
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : To update and refresh the say fields in the 
*:              "D\C On Account" folder.
*:----------------------------------------------------------------
*: Parameters : lnSign : -1 -> Subtract.
*:                       +1 -> Add.
*:              lnAmnt : The amount.
*:              llRef  : .T. -> Refresh the screen after updating 
*:                              the variables.
*:                       .F. -> Do not refresh the screen after 
*:                              updating the variables.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfUpdAccBal(-1, 10, .T.)
*:----------------------------------------------------------------
FUNCTION lfUpdAccBal
PARAMETERS lnSign, lnAmnt, llRef

*-- Determine the variable to be updated according to the 
*-- transaction type.
lcChkTyp   = IIF(&lcDCOnATmp..TranType = "*", cOpenType, TranType)
lcVar      = IIF(lcChkTyp = "3", "lnAccDebit", "lnAccCredt")

*-- Update the variable, the total, and the difference..
&lcVar     = &lcVar     + (lnSign*lnAmnt)
lnAccEntrd = lnAccDebit + lnAccCredt
lnAccDiffr = lnBalance  - lnAccEntrd

*E301281,1 (Start)
IF !llExtAuto
*E301281,1 (END)
  *-- Refresh the screen if you are asked for.
  llNoThing  = IIF(llRef, lfRefresh(lcKOChWn52), .T.)
  llNoThing  = lfUpdVars()
*E301281,1 (Start)
ENDIF
*E301281,1 (END)
  

*:----------------------------------------------------------------
*: Name       : lfvAllRem
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Valid function for the "Remove" button in the
*:              "Browse" folder.
*:----------------------------------------------------------------
*: Parameters : llFrmTrade : .T. -> Called from the trade discount
*:                                  validation.
*:                           .F. -> Called from the screen.
*:              lcSeekExp  : Use this expression to go to the
*:                           correct record in the "All Selected
*:                           Transaction" temp file if called from
*:                           the "Trade Discount" valid function.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvAllRem(.F.,"")
*:----------------------------------------------------------------
FUNCTION lfvAllRem
PARAMETERS llFrmTrade,lcSeekExp
PRIVATE lnAlias, llRem

llRem = .T.
IF llFrmTrade
  *-- If called from the "Trade Discount" valid function, adjust
  *-- the record pointer to the correct record in the "All Selected
  *-- Transaction" temp file.
  llRem = SEEK(lcSeekExp, lcDCAllTmp)
ELSE
  *-- If called normaly, confirm the deletion with the user.
  *-- Are you sure, You want to delete this transaction ?
  *-- < Yes >   < No > 
  llRem = gfModalGen('QRM40077B40000') = 1
ENDIF

*-- If the deletion is confirmed...
IF llRem
  lnAlias = SELECT(0)
  SELECT (lcDCAllTmp)
  
  *-- Update the totals in the screen header.
  lnPos     = ATC(TranType,"12304567")
  IF BETWEEN(lnPos,1,8)
    = lfUpdBal(IIF(lnPos<4,"P","N"), Amount, -1, !llFrmTrade)
  ENDIF
  
  *-- If the transaction has a "Charge Back Sales Rep" transactions
  *-- assigned to it, please clear these charge backs.
  IF TranType = "7" AND USED(lcRpComTmp)
    llNoThing = lfvCBRClr(&lcDCAllTmp..Tran)
  ENDIF
  
  IF TranType = "7" OR (TranType = "2" AND Deb_Adj = "Y")
    *-- If the transaction is a D\C Adjustment that is added in
    *-- this session, please remove it from the temp adjustment
    *-- file.
    SELECT (lcDCAdjTmp)
    LOCATE FOR Tran = &lcDCAllTmp..Tran
    IF FOUND()
      BLANK
      DELETE
    ENDIF
    GOTO TOP
  ELSE
    *-- If the transaction selected is a normal D\C transaction
    *-- and if it has an open balance. please go and clear this
    *-- open balance.
    lcFile    = lcDCAllTmp
    llShow    = .T.
    llNoThing = IIF(cShToOpn = "N", lfRemOpen(.T.), .T.)
  ENDIF
  
  *-- Delete the selected transaction, and refresh the browse.
  SELECT (lcDCAllTmp)
  BLANK
  DELETE
  GOTO TOP
  llNoThing = IIF(llFrmTrade, .T., lfwAlBrow()) 

  *B602945,01 (Start)
  SELECT(lnAlias)
  *B602945,01 (End)

ENDIF  

*B602945,01 (Start)
*SELECT(lnAlias)
*B602945,01 (End)

*:----------------------------------------------------------------
*: Name       : lpClsScr
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Local control pannel "Cancel" button validation.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : DO lpClsScr
*:----------------------------------------------------------------
PROCEDURE lpClsScr

*-- Unlock the customer record.
llNoThing = lfLokCus(lcAccount, .F.)

*-- Clear the uncomplete session record.
llNoThing = lfUpdUnCmS("Initia", SPACE(0), .F.)

*:----------------------------------------------------------------
*: Name       : lpSavScr
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Local control pannel "Save" button validation.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : DO lpSavScr
*:----------------------------------------------------------------
PROCEDURE lpSavScr

llCSave = .F.

*-- If there is any transaction selected.
IF lnPTrans > 0 OR lnNTrans < 0
  *-- If the screen is balanced..
  IF lnBalance = lnAccDebit + lnAccCredt
    *-- If there is no zero amount transaction have been done...
    IF lfChkZeros()
      
      *E301281,1 (Start)
      IF !llExtAuto
      *E301281,1 (End)
        *-- Update the incomplete session file with the object name.
        llNoThing = lfUpdVars()
        llNoThing = lfUpdUnCmS("Open", "pbSav", .T.)
      *E301281,1 (Start)
      ENDIF
      *E301281,1 (End)
      
      *-- Call the save procedure...
      
      
      
      *llCSave   = lfKeyOff()
      llCSave   = lfKeyOff(.F., .F., .F., .F., .F., .F., .F.,llFromEdi)
      
      *E301281,1 (Start)
      IF !llExtAuto
      *E301281,1 (End)
        *-- Update the incomplete session file.
       
        llNoThing = lfUpdUnCmS("Complete", SPACE(0), .F.)
        
      *E301281,1 (Start)
      ENDIF
      *E301281,1 (End)

    ENDIF
  ELSE
    *E301281,1 (Start)
    IF !llExtAuto
    *E301281,1 (End)

      *-- Show that the screen is not balanced.
      llNoThing = lfShowBal()
      
    *E301281,1 (Start)
    ENDIF
    *E301281,1 (End)
      
  ENDIF
ELSE
  *-- No transactions entered! Update ignored.
  *-- < Ok >
  = gfModalGen('TRM40102B00000')
ENDIF

*E301281,1 (Start)
IF llExtAuto
  llSaveComp = llCSave
ENDIF
*E301281,1 (End)

*:----------------------------------------------------------------
*: Name       : lfShowBal
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : To show that the screen is not balanced.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfShowBal()
*:----------------------------------------------------------------
FUNCTION lfShowBal

*-- Save and clear the original screen trapping.
PUSH KEY    
ON KEY

*-- Run the screen.
*DO (gcScrDir+gcWinAppl+"\ARShowBl.SPR")
DO (gcScrDir+"ARShowBl.SPR")

*-- Restore the original screen trapping.
POP KEY

*:----------------------------------------------------------------
*: Name       : lfChkZeros
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Check if there is any transaction has been done
*:              with a zero amount..
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfChkZeros()
*:----------------------------------------------------------------
FUNCTION lfChkZeros
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT (lcDCAllTmp)

*-- Look for any transaction has been done with a zero amount in
*-- the "All Selected Transactions" temp file.
lcPromp = SPACE(0)
lnGoToF = 0
llFound = .F.
*SCAN FOR Account+TranType+Tran+cInstalNo = lcAccount AND Amount = 0
*  lcPromp = "'Debit\Credit'"
*  llFound = .T.
*  lnGoToF = 4
*  EXIT
*ENDSCAN

*-- If you did not find any transaction in the previous check
*-- then check the "D\C On Account" temp file.
IF !llFound
  SELECT (lcDCOnATmp)
  LOCATE FOR (TranType="*" AND EMPTY(cOpnBalTrn)) OR Amount = 0
  llFound = FOUND()
  lnGoToF = 3
  lcObj   = IIF(TranType="*" AND EMPTY(cOpnBalTrn), "lcAccTran", "lnAccAmont")
  lcPromp = IIF(llFound, "'Debit\Credit On Account'", lcPromp)
ENDIF  

*-- If you find any transaction has been done with a zero amount
*-- Display a message, activate the corresponding folder, and
*-- return false.
*E301281,1 (Start)
*IF llFound
IF llFound AND !llExtAuto
*E301281,1 (End)
  *-- One or more XXXXXXX transaction(s) has a zero amount,
  *-- Cannot proceed.
  *-- < Ok >
  lnNoThing   = gfModalGen('TRM40103B00000', "ALERT", lcPromp)
  lnLastFold  = lnActFolder
  lnActFolder = lnGoToF
  llNoThing   = lfChngFolder(lnActFolder)
  IF lnGoToF = 3
    _CUROBJ = OBJNUM(&lcObj)
  ENDIF
ENDIF

SELECT(lnAlias)

RETURN (!llFound)

*:----------------------------------------------------------------
*: Name       : lfKeyOff
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Update the master files...
*:----------------------------------------------------------------
*: Parameters : lcAcc   : Key-off customer code.
*:              ldKOD   : Key-off date.
*:              lnTotDb : Total Keyed-off Debits.
*:              lnTotCr : Total Keyed-off Credits.
*:              lcArHs  : Temp file that holds the keyed-off 
*:                        transactions
*:              lcOnAc  : Temp file that holds the "D\C On Account"
*:                        transactions
*:              lcRpCm  : Temp file that holds the "Charge Back
*:                        Sales Reps" transactions.
*:----------------------------------------------------------------
*: Notes      : This function is designed to be called either
*:              from this program or from any other program.
*:              1. When calling this function from this program you
*:                 do not have to pass any of the previous paramters,
*:                 because the function will detect the corresponding
*:                 variables, values, and files.
*:              2. When calling this function from outside the 
*:                 program, please consider the following...
*:                 2.1. The first five parameters are mandatory
*:                      parameters, means that you have to send 
*:                      all of them.
*:                 2.2. The rest of the parameters are optional,
*:                      means that you might send them if you want
*:                      or you might not.
*:                 2.3. When you create your temporary files that 
*:                      will be sent to this function , please be 
*:                      sure that you use the same file structure
*:                      used to create the same temporary files 
*:                      used in this program, following is a list
*:                      of the files names and there corresponding
*:                      names in the parameter list..
*:                      Paramter Name   File Name In This program
*:                      lcArHs          lcDCAllTmp
*:                      lcOnAc          lcDCOnATmp
*:                      lcRpCm          lcRpComTmp
*:                      To check the temporary files structure in
*:                      this program, please read the function
*:                      called "lfCratTemp" in this program.
*:                 2.3. This function is also designed to handel 
*:                      the incomplete session concept.
*:                      For you, to be able to make this function 
*:                      work properly for the incomplete session 
*:                      concept, please consider the following...
*:                      2.3.1. Define the following variables
*:                             in your program with the same 
*:                             names :
*:                             - lcProgID     Your program ID.
*:                             - lcSession    Sequence number.
*:                             - lnUnCmSeRc   The incomplete session
*:                                            record number.
*:                             - laVars       The incomplete session 
*:                                            variables array.
*:                             - lcAdTrnSeq   SPACE(0)
*:                             - lcAcTrnSeq   SPACE(0)
*:                             - lcHisSeq     SPACE(0)
*:                             - lcGlSess     SPACE(0)
*:                             - lcRepBat     SPACE(0)
*:                             - llUpdGlDif   .T.
*:                             - llUpdMstGL   .T.
*:                      2.3.2. Be sure to include the following 
*:                             list of variables in the incomplete 
*:                             session variables array called
*:                             laVars...
*:                             - lnUnCmSeRc      - lcGlSess
*:                             - lcAdTrnSeq      - lcRepBat
*:                             - lcAcTrnSeq      - llUpdGlDif
*:                             - lcHisSeq        - llUpdMstGL
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfKeyOff()
*:----------------------------------------------------------------
*B802456,1
FUNCTION lfKeyOff
PARAMETERS lcAcc, ldKOD, lnTotDb, lnTotCr, lcArHs, lcOnAc, lcRpCm,llFromEdi
PRIVATE lnAlias

*-- Check the passed parameters and set the there values.
lnAlias    = SELECT(0)
lcAcc      = IIF(TYPE("lcAcc")   = "C", lcAcc  , lcAccount)
ldKOD      = IIF(TYPE("ldKOD")   = "D", ldKOD  , ldKODate )
lnTotDb    = IIF(TYPE("lnTotDb") = "N", lnTotDb, lnPTrans ) 
lnTotCr    = IIF(TYPE("lnTotCr") = "N", lnTotCr, lnNTrans )

*B802456,1 If the variable is not defined, then the program is not called 
*B802456,1 from another program in automatic key off mode. 
llExtAuto = IIF(TYPE("llExtAuto")="U",.F.,llExtAuto)
*B802456,1  [End  ]

*E301077,53 YMA 03/03/99 Start - Added the following...
*-- Variables that will hold the alias names for the files that will
*-- opened again.
lcDBFile   = SPACE(0)
lcCRFile   = SPACE(0)
lcHsFile   = SPACE(0)
lcCusHsF   = SPACE(0)
*E301077,53 YMA 03/03/99 End.

*-- Restore the Multi currency, the GL and the AP system settings.
llGLLink   = UPPER(ALLTRIM(gfGetMemVar("M_LINK_GL"))) = "Y"
llMltCur   = gfGetMemVar("llMulCurr")
llApLink   = OCCURS("AP", gcCmpModules)#0

*-- Initialize the GL related variables.
lcGLYear   = SPACE(0)
lcGLPer    = SPACE(0)
llNoThing  = CheckPrd(ldKOD,"lcGLYear","lcGLPer",'KO',.T.)
lcCusLnk   = SPACE(0)

*-- Initialize the customer relatedd variables.
lcAccName  = SPACE(0) 

*-- Temp GL destripution file.
lcTmpGLD   = SPACE(0)

*-- Currency related variables.
lcUnitSign = "/"
lnExchDif  = 0

*-- Files open flags.
llOpGlDist = .F.
llOpenGL   = .F.
llOpnSR    = .F.
llOpnRC    = .F.
llOpnAPC   = .F.
llOpnAPP   = .F.
llOpnCus   = .F.
*E500374,5 HBG 08/16/2000 Flag to check if FACTORfile is opened or not [Begin]
llOpenFac  = .F.
*E500374,5 [END]

*-- Flag to tell that the program has created the On Account transactions file
*-- in case of calling this function from outside the program.
llCrteOnAc = .F.

*-- If you can open the files...
IF lfOpnFils()
  *-- If the customer code is valid..
 
  IF SEEK("M" + lcAcc, "Customer")
    *-- Load the custmer informatrion.
    *E500374,5  HBG  08/15/2000 comment these lines , they will not be used in this place [Begin]
    *lcCusLnk   = Customer.Link_Code
    *lcCusLnk   = IIF(EMPTY(lcCusLnk), "DEFDEF", lcCusLnk)
    *E500374,5 [End]
    lcAccName  = Customer.BtName
    lcOnAcGLAc = SPACE(0)
    *E500374,5  HBG  08/15/2000 comment these lines , they will not be used in this place [Begin]
    *IF llGlLink AND SEEK(lcCusLnk+"001","GL_LINK")
      *lcOnAcGLAc = GL_LINK.GLAcnt
    *ENDIF
    *E500374,5 [End]
    *-- Update the D\C Adjustments.
    llNoThing  = lfUpdAdj() 
    
    *-- Update the D\C On Account.
    llNoThing  = lfUpdOnAcc()
    
    *-- Update the Transactions difference.
    llCond     = llMltCur AND llGLLink AND lnExchDif#0
    llNoThing  = IIF(llCond  , lfUpdDiff() , .T.)
    
    *-- Update the master GL file.
    llNoThing  = IIF(llGLLink, lfUpdMstGL(), .T.)
    
    *-- Unlock the customer record.
    llNoThing  = lfLokCus(lcAcc, .F.)
    
    *B607455,1 AMH 07/27/2003 (Begin) Saving the variables.
    SAVE TO (gcWorkDir+"ARKEYOFF.MEM") ALL
    *B607455,1 AMH 07/27/2003 (End)
    
    *-- Age the customer
    *E301509,1 (Start)
    IF llFromEdi
      DO (gcAppHome+"NC\ARAGEAR.PRG") WITH lcAcc
    ELSE
    *E301509,1 (End)      
      DO (gcAppHome+"ARAGEAR.PRG") WITH lcAcc      
    *E301509,1 (Start)
    ENDIF  
    *E301509,1 (End)
    
    *B607455,1 AMH 07/27/2003 (Begin) Restoring the variables.
    RESTORE FROM (gcWorkDir+"ARKEYOFF.MEM") ADDITIVE
    *B607455,1 AMH 07/27/2003 (End)

  ENDIF
  WAIT CLEAR

  *-- Close the opened files.
  llNoThing = lfClsFils()
ENDIF

SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfOpnFils
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : To insure that the needed files are open.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : .T. -> Yes I was able to open all the files.
*:              .F. -> No, I was not able to open the files.
*:----------------------------------------------------------------
*: Example    : = lfOpnFils()
*:----------------------------------------------------------------
FUNCTION lfOpnFils
PRIVATE llRet

llRet = .T.

IF TYPE("lcOnAc") $ "LU" AND TYPE("lcDCOnATmp") = "C"
  *-- If the "D\C On Account" temp file name is not sent as a parameter 
  *-- assign its value to the program's "D\C On Account" temp file name.
  lcOnAc = lcDCOnATmp
ELSE
  *-- Assign a temp name if it is not passed.
  IF TYPE("lcOnAc") $ "LU" OR (TYPE("lcOnAc") = "C" AND EMPTY(lcOnAc))
    lcOnAc = gfTempName()
  ENDIF

  *-- If the temp file exist..
  IF FILE(gcWorkDir+lcOnAc+".DBF")
    *-- And not used, use it.
    IF !USED(lcOnAc)
      USE (gcWorkDir+lcOnAc) IN 0 SHARED
    ENDIF
  ELSE
    *-- Create it, if it does not exist.
    llCrteOnAc = .T.
    
    *E301077,53 YMA 03/03/99 Start - Open the history file before the SQL, and use it.
    *llOpnArHis = !USED("ArHist")
    *SELECT * FROM (gcDataDir+"ArHist")  ;
    * WHERE Account+Tran+cInstalNo = "~" ;
    *  INTO DBF(gcWorkDir+lcOnAc)
    
    llTmpAlias = SPACE(0)
    llOpnArHis = gfOpenFile(gcDataDir+"ArHist", "", "SH", @llTmpAlias, .T.)
    SELECT * FROM (llTmpAlias)  ;
     WHERE Account+Tran+cInstalNo = "~" ;
      INTO DBF(gcWorkDir+lcOnAc)
    
    *E301077,53 YMA 03/03/99 End.

    USE IN (lcOnAc)
    USE (gcWorkDir+lcOnAc) IN 0 SHARED
    IF llOpnArHis
      *E301077,53 YMA 03/03/99 Start - Use gfCloseFile instead.
      = gfCloseFile(llTmpAlias)
      *USE IN ArHist
      *E301077,53 YMA 03/03/99 End.
    ENDIF
  ENDIF
ENDIF

IF TYPE("lcRpCm") $ "LU" AND TYPE("lcRpComTmp") = "C"
  *-- If the "Charge back sales rep" temp file name is not sent 
  *-- as a parameter, assign its value to the program's 
  *-- "Charge back sales rep" temp file name.
  lcRpCm = lcRpComTmp
ELSE
  *-- Assign a dummy name for it, if it is not passed.
  IF TYPE("lcRpCm") $ "LU" OR (TYPE("lcRpCm") = "C" AND EMPTY(lcRpCm))
    lcRpCm = gfTempName()
  ENDIF

  *-- If the temp file exist..And not used, use it.
  IF FILE(gcWorkDir+lcRpCm+".DBF") AND !USED(lcRpCm)
    USE (gcWorkDir+lcRpCm) IN 0 ORDER (lcRpCm) SHARED
  ENDIF
ENDIF

IF TYPE("lcArHs") $ "LU" AND TYPE("lcDCAllTmp") = "C"
  *-- If the "All Selected Transactions" temp file name is not sent 
  *-- as a parameter, assign its value to the program's 
  *-- "All Selected Transactions" temp file name.
  lcArHs = lcDCAllTmp
ELSE
  *-- Assign a dummy name for it, if it is not passed.
  IF TYPE("lcArHs") $ "LU" OR (TYPE("lcArHs") = "C" AND EMPTY(lcArHs))
    llRet = .F.
  ELSE
    *-- If the temp file exist..
    IF FILE(gcWorkDir+lcArHs+".DBF")
      *-- And not used, use it.
      IF !USED(lcOnAc)
        USE (gcWorkDir+lcArHs) IN 0 SHARED
      ENDIF
    ELSE
      *-- Return false, and do not do any thing.
      llRet = .F.
    ENDIF
  ENDIF
ENDIF

*-- If you were able to do all the previous steps...
IF llRet
  *-- Open these files with different alias names.
  *E301077,53 YMA 03/03/99 Start - Use gfOpenFile instead.
  *USE (gcDataDir+"Debit" )   AGAIN IN 0 ALIAS DBFile SHARED ORDER Debit
  *USE (gcDataDir+"Credit")   AGAIN IN 0 ALIAS CRFile SHARED ORDER Credit
  *USE (gcDataDir+"ArHist")   AGAIN IN 0 ALIAS HsFile SHARED
  *USE (gcDataDir+"ARCUSHST") AGAIN IN 0 ALIAS CusHsF SHARED ORDER Acthst
  = gfOpenFile(gcDataDir+"Debit"   , "Debit" , "SH", @lcDBFile, .T.)
  = gfOpenFile(gcDataDir+"Credit"  , "Credit", "SH", @lcCRFile, .T.)
  = gfOpenFile(gcDataDir+"ArHist"  , ""      , "SH", @lcHsFile, .T.)
  = gfOpenFile(gcDataDir+"ARCUSHST", "Acthst", "SH", @lcCusHsF, .T.)
  *E301077,53 YMA 03/03/99 End.

  *-- Be sure to open the customer file with this tag...
  IF USED("Customer")
    SET ORDER TO TAG Customer IN Customer
  ELSE
    llOpnCus = gfOpenFile(gcDataDir+"Customer", "Customer", "SH")
  ENDIF 

  *-- If the "Charge back sales rep" temp file is opened, open
  *-- its related files...
  IF USED(lcRpCm) 
    IF USED("SalesRep")
      SET ORDER TO TAG SalesRep IN SalesRep
    ELSE
      llOpnSR = gfOpenFile(gcDataDir+"SalesRep", "SalesRep", "SH")
    ENDIF

    IF USED("RepComm")
      SET ORDER TO TAG RepComm IN RepComm
    ELSE
      llOpnRC = gfOpenFile(gcDataDir+"RepComm", "RepComm", "SH")
    ENDIF
  ENDIF

  *-- If the system is setup to use the GL Link, please create
  *-- the temp GL file.
  IF llGLLink
    llOpenGL   = gfOpenFile(gcDataDir+"GL_LINK", "GL_LINK", "SH")
    lcTmpGLD   = gfTempName()
    llOpGlDist = gfOpenFile(gcDataDir+"GLDIST" ,"GLDISTAC","SH")  
    SELECT GLDIST
    COPY STRU TO (gcWorkDir+lcTmpGLD)
    USE (gcWorkDir+lcTmpGLD) IN 0 SHARED
  ENDIF
ENDIF

RETURN (llRet)

*:----------------------------------------------------------------
*: Name       : lfClsFils
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Close all the opend files.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfClsFils()
*:----------------------------------------------------------------
FUNCTION lfClsFils

*E301077,53 YMA 03/03/99 Start - Use gfCloseFile instead.
*USE IN DBFile
*USE IN CRFile
*USE IN HsFile
*USE IN CusHsF
*USE IN IIF(llOpnSR   , "SalesRep", 0)
*USE IN IIF(llOpnRC   , "RepComm" , 0)
*USE IN IIF(llOpnAPC  , "ApChecks", 0)
*USE IN IIF(llOpnAPP  , "APPaymnt", 0)
*USE IN IIF(llOpnCus  , "Customer", 0)
*USE IN IIF(llOpenGL  , "GL_LINK" , 0)
*USE IN IIF(llOpGlDist, "GLDIST"  , 0)

= gfCloseFile(lcDBFile)
= gfCloseFile(lcCRFile)
= gfCloseFile(lcHsFile)
= gfCloseFile(lcCusHsF)

= IIF(llOpnSR   , gfCloseFile("SalesRep") , .F.)
= IIF(llOpnRC   , gfCloseFile("RepComm")  , .F.)
= IIF(llOpnAPC  , gfCloseFile("ApChecks") , .F.)
= IIF(llOpnAPP  , gfCloseFile("APPaymnt") , .F.)
= IIF(llOpnCus  , gfCloseFile("Customer") , .F.)
= IIF(llOpenGL  , gfCloseFile("GL_LINK")  , .F.)
= IIF(llOpGlDist, gfCloseFile("GLDIST")   , .F.)
*E301077,53 YMA 03/03/99 End.


IF USED(lcTmpGLD)
  USE IN (lcTmpGLD)
ENDIF
ERASE (gcWorkDir+lcTmpGLD+".DBF")
ERASE (gcWorkDir+lcTmpGLD+".CDX")
ERASE (gcWorkDir+lcTmpGLD+".FPT")

IF llCrteOnAc
  IF USED(lcOnAc)
    USE IN (lcOnAc)
  ENDIF
  ERASE (gcWorkDir+lcOnAc+".DBF")
  ERASE (gcWorkDir+lcOnAc+".CDX")
  ERASE (gcWorkDir+lcOnAc+".FPT")
ENDIF

*:----------------------------------------------------------------
*: Name       : lfUpdAdj
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Update the D\C adjustments in the master files.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfUpdAdj()
*:----------------------------------------------------------------
FUNCTION lfUpdAdj
PRIVATE lnAlias
lnAlias = SELECT(0)

*-- Go through all the selected transactions...
SELECT(lcArHs)
SCAN

  WAIT WINDOW "Updating Transaction Number : " + Tran NOWAIT
  
  *E500374,5  HBG  08/15/2000 If there is a factore ,take the Link Code of it,If not 
  *E500374,5                  take Customer's Link Code[Begin]
  lcCusLnk = lfGeLnkCd()
  *E500374,5 [End]

  *-- If the D\C adjustment is done in this session...
  IF TranType = "7" OR (TranType = "2" AND Deb_Adj = "Y")
    IF &lcArHs..nStep < 1
      *-- Generate a transaction sequence number, and save it
      *-- in the uncomplete session file.
      lcToSnd    = IIF(TranType="2", "DEBIT", "CREDIT")
      lcAdTrnSeq = gfsequence(lcToSnd, gcAct_Comp, "", "", "TRAN")
      llNoThing  = lfUpdVars()
      
      llNoThing  = lfUpdStp(lcArHs, 1)
    ENDIF
    
    *-- Update the Customer history file
    SELECT(lcArHs)
    IF &lcArHs..nStep < 2 AND !EMPTY(cYear) AND !EMPTY(cPrd) AND ;
       BETWEEN(VAL(cPrd), 1, 13)
      llNoThing = lfUpdCusH()
      llNoThing = lfUpdStp(lcArHs, 2)
    ENDIF
    
    *-- Update the GL temp files.
    llNoThing = lfUpdAdjGL()
    
    *-- Update the sales rep commission file..
    llNoThing = IIF(TranType = "7" AND USED(lcRpCm), lfUpdRpCm(), .T.) 
  ELSE
    *-- Update the D\C transactions selected from the 
    *-- Debit, Credit master files..
    llNoThing  = lfUpdDCTrn()
  ENDIF
  
  *-- Register a reverse GL transaction..
  IF &lcArHs..nStep < 6 AND llGLLink
    SELECT(lcArHs)
    
    DO GLDIST WITH "DEFDEF"   , "001"    , Amount*-1, "KO"      ,;
                   lcAdTrnSeq , ldKOD    , lcGLYear , lcGLPer   ,;
                   '&lcTmpGLD', cARGLAcc , cCurrCode, nCurrUnit ,;
                   nExRate
    llNoThing  = lfUpdStp(lcArHs, 6)
  ENDIF
  
  SELECT(lcArHs)
  IF EMPTY(lcHisSeq)
    *-- Generate a history sequence, and save it in the 
    *-- uncomplete session file.
    lcHisSeq  = gfsequence("HISTORY")
    
    *E301281,1 (Start)
    IF !llExtAuto
    *E301281,1 (End)
      llNoThing = lfUpdVars()
    *E301281,1 (Start)
    ENDIF
    *E301281,1 (End)
  ENDIF

  *-- Compute the transactions currencey difference..
  IF llMltCur
    lcRateSign = gfGetExSin(@lcUnitSign,cCurrCode)  
    lnNewVal   = (Amount*-1 &lcRateSign nExRate &lcUnitSign nCurrUnit)
    lnExchDif  = lnExchDif + lnNewVal
  ENDIF

  *-- Add the audit information.
  SELECT(lcArHs)
  llNoThign = gfAdd_Info(lcArHs)

  *-- Load the information from the temp file, and modify it
  *-- to include the sequence numbers, and the total debits, credits.
  
  SELECT(lcArHs)
  
  SCATTER MEMVAR MEMO
  m.Tran     = lcAdTrnSeq
  m.HistDate = ldKOD
  m.History  = lcHisSeq
  m.TotDb    = lnTotDb
  m.TotCr    = lnTotCr
  m.OpenAmt  = lnTotDb+lnTotCr

  IF &lcArHs..nStep < 7
    *-- Update the master "AR History" file..
    SELECT (lcHsFile)
    APPEND BLANK
    GATHER MEMVAR MEMO
    lcKey     = ACCOUNT+HISTORY+TRANTYPE+TRAN+CINSTALNO
    llNoThing = gfTraceKey("ArHist",lcKey,"A")
    llNoThing = lfUpdStp(lcArHs, 7)
  ENDIF
ENDSCAN

SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfUpdDCTrn
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Update the D\C transactions that are selected
*:              from the debit\credit master files
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfUpdDCTrn()
*:----------------------------------------------------------------
FUNCTION lfUpdDCTrn
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT(lcArHs)

*-- Determine the file name to work with, and the expression used 
*-- to go to the correct record in that file..
lcAdTrnSeq = Tran

*B602897,1 05/20/99 YMA Added the new debit code "R" into consideration.

*B606627,1 RAE Add TranType = "M" in case of Invoice Material Sales Order.[start]
*lcFile     = IIF(TranType $ "R123", lcDBFile, lcCRFile)
*lcExp      = IIF(TranType $ "R123",;
             &lcArHs..Account+&lcArHs..Tran+&lcArHs..cInstalNo+DTOS(&lcArHs..TranDate),;
             &lcArHs..Account+&lcArHs..Tran+DTOS(&lcArHs..TranDate))

lcFile     = IIF(TranType $ "RM123", lcDBFile, lcCRFile)             
lcExp      = IIF(TranType $ "RM123",;
             &lcArHs..Account+&lcArHs..Tran+&lcArHs..cInstalNo+DTOS(&lcArHs..TranDate),;
             &lcArHs..Account+&lcArHs..Tran+DTOS(&lcArHs..TranDate))
*B606627,1 RAE [end]

*B000000,1 Hesham (Start)
*B000000,1 Change the value of the key passed to the trace function to be
*B000000,1 like the master file tag

*B606627,1 RAE Add TranType = "M" in case of Invoice Material Sales Order.[start]
*lcTraceExp = IIF(TranType $ "R123",;
             TranType+&lcArHs..Tran+&lcArHs..cInstalNo,;
             TranType+&lcArHs..Tran)
lcTraceExp = IIF(TranType $ "RM123",;
             TranType+&lcArHs..Tran+&lcArHs..cInstalNo,;
             TranType+&lcArHs..Tran)
*B606627,1 RAE [end]

*B602897,1 05/20/99 YMA End.

*B000000,1 Hesham (End)
*-- If you are able to go to the needed record number in the master
*-- Debit\Credit file..


IF SEEK(lcExp, lcFile)
  *-- If the transaction has no Open Balance..
  IF cShToOpn = "Y"
    *-- Delete the master record.
    SELECT (lcFile)
    DELETE
    lcFlToSend = IIF(lcFile = lcDBFile, "Debit", "Credit")
    llNoThing  = gfTraceKey(lcFlToSend, lcTraceExp, "D")
  ELSE
    IF &lcArHs..nStep < 1
      *-- Update the amount with the open balance amount in the
      *-- temp file.
      SELECT(lcArHs)
      llNoThing = RLOCK()
      REPLACE Amount WITH &lcFile..Amount - nTrnNewAmn
      UNLOCK
      llNoThing  = lfUpdStp(lcArHs, 1)
    ENDIF

    IF &lcArHs..nStep < 2
      *-- Update the master file with the new open balance amount.
      SELECT (lcFile)
      llNoThing = RLOCK()
      REPLACE Amount WITH &lcArHs..nTrnNewAmn
      UNLOCK
      llNoThing = lfUpdStp(lcArHs, 2)
      llNoThing = gfTraceKey(lcFile, lcTraceExp, "M")
    ENDIF
  ENDIF
      
ENDIF

SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfUpdAdjGL
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Update the temp GL file with the GL enteries that
*:              will be done for each D|C Adjustment record done
*:              in this session.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfUpdAdjGL()
*:----------------------------------------------------------------
FUNCTION lfUpdAdjGL
PRIVATE lnAlias

lnAlias = SELECT(0)

*-- Default the GL account that will be used from the 
*-- temp file, which comes from the related fields for the 
*-- record's reason code.
SELECT(lcArHs)
lcGLAc = cAdjAcct

*-- If the AP is installed and the bank code and the checking account
*-- fields are not empty...
IF !EMPTY(cBnkCode) AND !EMPTY(cChkAcct) AND llApLink
  *-- Be sure that the AP Payment, and the AP Checks fiels
  *-- are open..
  IF USED("ApChecks")
    SET ORDER TO TAG Bankcheck IN ApChecks
  ELSE
    llOpnAPC = gfOpenFile(gcDataDir+"ApChecks", "Bankcheck", "SH")
  ENDIF 
    
  IF !USED("APPaymnt")
    llOpnAPP = gfOpenFile(gcDataDir+"APPaymnt", "", "SH")
  ENDIF
  
  *-- Go and update the AP Payment file, and get the GL Account
  *-- that will be used from the APChecks file using the 
  *-- bank code and the checking account
  lcGLAc = lfUpdApPay()
ENDIF

SELECT(lcArHs)
*-- If the GL installed..
IF llGLLink
  lcCatg  = IIF(TranType = "7", "009", "010")
  lcType  = IIF(TranType = "7",  "CA",  "DA")

  *-- Update the GL temp file with the first GL transactions.
  SELECT(lcArHs)
  IF &lcArHs..nStep < 4
    
    DO GLDIST WITH "DEFDEF", lcCatg, Amount*-1, lcType, lcAdTrnSeq, dPostDate ,;
                   cYear, cPrd, '&lcTmpGLD', lcGLAc  , cCurrCode, nCurrUnit   ,;
                   nExRate
    *-- Save the GL account used to generate the GL transaction 
    *-- in the temp file to be able to update it in the master
    *-- AR History file later on...
    SELECT(lcArHs)
    = RLOCK()
    REPLACE cAdjAcct WITH lcGLAc
    UNLOCK
    
    llNoThing = lfUpdStp(lcArHs, 4)
  ENDIF
  
  *-- Update the GL temp file with the 2nd GL transactions.
  SELECT(lcArHs)
  
  IF &lcArHs..nStep < 5
    
    DO GLDIST WITH lcCusLnk,  "001", Amount*+1, lcType, lcAdTrnSeq, dPostDate     ,;
                   cYear, cPrd, '&lcTmpGLD', SPACE(0), cCurrCode, nCurrUnit ,;
                   nExRate
    
    *-- Save the GL account used to generate the GL transaction 
    *-- in the temp file to be able to update it in the master
    *-- AR History file later on...
    SELECT(lcArHs)
    = RLOCK()
    REPLACE cARGLAcc WITH &lcTmpGLD..GLAccount
    UNLOCK
    
    lNoThing = lfUpdStp(lcArHs, 5)
  ENDIF
ENDIF
SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfUpdApPay
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Update the master AP Payment file..
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : The GL account that should be used to generate 
*:              the GL transactions..
*:----------------------------------------------------------------
*: Example    : = lfUpdApPay()
*:----------------------------------------------------------------
FUNCTION lfUpdApPay
PRIVATE lnAlias

lnAlias   = SELECT(0)
lcPayComp = IIF(EMPTY(&lcArHs..Reference),lcAccName, &lcArHs..Reference)

IF &lcArHs..nStep < 3
  SELECT APPaymnt
  APPEND BLANK
  llNoThing = RLOCK()
  llNoThing = gfAdd_Info()
  
  *E301266,1 AKA (Start)
  *REPLACE cPayType  WITH 'A'                ,;
  *        cPayMeth  WITH 'A'                ,;
  *        cPayRecSt WITH 'O'                ,;
  *        cPayDocNo WITH lcHisSeq           ,;
  *        cPayClNo  WITH lcAcc              ,;
  *        cPayComp  WITH lcPayComp          ,;
  *        cBnkCode  WITH &lcArHs..cBnkCode  ,;
  *        cChkAcct  WITH &lcArHs..cChkAcct  ,;
  *        dPayDate  WITH &lcArHs..dPostDate ,;
  *        cFisFYear WITH &lcArHs..cYear     ,;
  *        cFspprdid WITH &lcArHs..cPrd      ,;
  *        nPayAmnt  WITH &lcArHs..Amount
  
  
  REPLACE cPayType  WITH 'A'                ,;
          cPayMeth  WITH 'A'                ,;
          cPayRecSt WITH 'O'                ,;
          cPayDocNo WITH lcHisSeq           ,;
          cPayClNo  WITH lcAcc              ,;
          cPayComp  WITH lcPayComp          ,;
          cBnkCode  WITH &lcArHs..cBnkCode  ,;
          cChkAcct  WITH &lcArHs..cChkAcct  ,;
          dPayDate  WITH &lcArHs..dPostDate ,;
          cFisFYear WITH &lcArHs..cYear     ,;
          cFspprdid WITH &lcArHs..cPrd      ,;
          nPayAmnt  WITH &lcArHs..Amount    ,;
          batch     WITH lcRepBat           ,; 
          cCurrCode WITH lcCurCode          ,; 
          nExRate   WITH lnExRate           ,;
          nCurrUnit WITH lnCurrUnit

  *E301266,1 AKA (End)          
  
  UNLOCK
  llNoThing = gfTraceKey("APPaymnt", CPAYTYPE+CPAYMETH+CBNKCODE+CCHKACCT+CPAYDOCNO, "A")
  llNoThing = lfUpdStp(lcArHs, 3)
ENDIF

SELECT(lnAlias)

RETURN (IIF(SEEK(&lcArHs..cBnkCode+&lcArHs..cChkAcct,"ApChecks"),;
                 ApChecks.cChkGLAcc, lcGLAc))

*:----------------------------------------------------------------
*: Name       : lfUpdMstGL
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Update the master GL Distribution file..
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfUpdMstGL()
*:----------------------------------------------------------------
FUNCTION lfUpdMstGL
PRIVATE lnAlias

lnAlias  = SELECT(0)

*-- Get a unique GL session number.
IF EMPTY(lcGlSess)
  lcGlSess  = gfsequence("GLSESSION")

  *E301281,1 (Start)
  IF !llExtAuto
  *E301281,1 (End)
    llNoThing = lfUpdVars()
  *E301281,1 (Start)
  ENDIF
  *E301281,1 (End)

ENDIF

*E301231,01 YMA 05/20/99 Commented out the following code and replaced it with 
*E301231,01 YMA 05/20/99 with the code that follows it to eleminate creating 
*E301231,01 YMA 05/20/99 the GL entries for any GL account if it add up to 
*E301231,01 YMA 05/20/99 zero amount.

*-- Update the temp GL Distribution file with the generate 
*-- GL session number.
*SELECT (lcTmpGLD)
*REPLACE ALL GLSESSION WITH lcGlSess
*USE IN (lcTmpGLD)

*-- Add the records to the master GL Distribution file.
*IF llUpdMstGL
*  SELECT GLDIST
*  APPEND FROM (gcWorkDir+lcTmpGLD)
*  llUpdMstGL = .F.
*  llNoThing  = lfUpdVars()
*ENDIF  

USE (gcWorkDir+lcTmpGLD) IN 0 SHARED AGAIN ALIAS UseToSum
DIMENSION laAccounts[1,2]
laAccounts = SPACE(0)
SELECT (lcTmpGLD)

SCAN
  
  *B602794 (Start)
  *Rewrite this part
  
  *lcGLAcc = &lcTmpGLD..GLAccount
  *IF ASCAN(laAccounts, lcGLAcc) = 0
  *  SELECT UseToSum
  *  SUM ALL nGLAmount FOR GLAccount = lcGLAcc TO lnGLAmount
  *  lnLen = ALEN(laAccounts, 1)
  *  lnLen = lnLen + IIF(lnLen = 1 AND EMPTY(laAccounts[1,1]), 0, 1)
  *  DIMENSION laAccounts[lnLen,2]
  *  laAccounts[lnLen, 1] = lcGLAcc
  *  laAccounts[lnLen, 2] = lnGLAmount # 0
  *ENDIF
  
  lcGLAcc = &lcTmpGLD..GLAccount + &lcTmpGLD..Tran_Type
  IF &lcTmpGLD..Tran_Type # 'KO'
    lnLen = ALEN(laAccounts, 1)
    lnLen = lnLen + IIF(lnLen = 1 AND EMPTY(laAccounts[1,1]), 0, 1)
    DIMENSION laAccounts[lnLen,2]
    laAccounts[lnLen, 1] = lcGLAcc 
    laAccounts[lnLen, 2] = .T.
  ELSE
    IF ASCAN(laAccounts, lcGLAcc) = 0
      SELECT UseToSum
      *B603081 (Start)
      *SUM ALL nGLAmount FOR GLAccount+Tran_Type = lcGLAcc  TO lnGLAmount
      SUM ALL nEqvAmnt FOR GLAccount+Tran_Type = lcGLAcc  TO lnGLAmount
      *B603081 (End)

      lnLen = ALEN(laAccounts, 1)
      lnLen = lnLen + IIF(lnLen = 1 AND EMPTY(laAccounts[1,1]), 0, 1)
      DIMENSION laAccounts[lnLen,2]
      laAccounts[lnLen, 1] = lcGLAcc
      laAccounts[lnLen, 2] = lnGLAmount # 0
    ENDIF
  ENDIF
  *B602794 (End)  
  
  SELECT (lcTmpGLD)
  REPLACE GLSESSION WITH lcGlSess
ENDSCAN
USE IN UseToSum

IF llUpdMstGL
  SELECT (lcTmpGLD)
  *SCAN FOR laAccounts[ASUBSCRIPT(laAccounts,ASCAN(laAccounts,&lcTmpGLD..GLAccount),1),2]
  SCAN FOR laAccounts[ASUBSCRIPT(laAccounts,;
                      ASCAN(laAccounts,&lcTmpGLD..GLAccount+&lcTmpGLD..Tran_Type),1),2]
    SELECT (lcTmpGLD)
    SCATTER MEMVAR MEMO
    SELECT GLDIST
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDSCAN
  USE IN (lcTmpGLD)
  
  llUpdMstGL = .F.
  
  *E301281,1 (Start)
  IF !llExtAuto
  *E301281,1 (End)
    llNoThing  = lfUpdVars()
  *E301281,1 (Start)
  ENDIF
  *E301281,1 (End)
    
ENDIF
*E301231,01 YMA 05/20/99 End.

SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfUpdRpCm
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Update the master Rep Commissoin file..
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfUpdRpCm()
*:----------------------------------------------------------------
FUNCTION lfUpdRpCm
PRIVATE lnAlias

lnAlias = SELECT(0)

*-- Go to the rep commission records corresponding to the
*-- current transaction number..
SELECT (lcRpCm)
IF SEEK(&lcArHs..Tran)
  WAIT WINDOW "Updating Sales Rep. commission for : " + RepCode NOWAIT
  
  *-- Generate a unique batch number.
  IF EMPTY(lcRepBat)
    lcRepBat  = gfsequence("BATCH")
    llNoThing = lfUpdVars()
  ENDIF
  
  *-- Go through all the sales reps charged for this transaction..
  SCAN REST WHILE Tran = &lcArHs..Tran 
    
    *-- Go to the corresponding sales rep record in the 
    *-- master sales rep file.
    IF SEEK(&lcRpCm..RepCode, "SalesRep")
      *-- Compute the equivalent amount.
      lnRpFrnAmnt = &lcRpCm..Amount
      lnRpEqvAmnt = ROUND(&lcRpCm..Amount &lcExRSin lnExRate;
                          &lcUntSin lnCurrUnit,2)

      IF &lcRpCm..nStep < 1
        *-- Update the master sales rep file.
        SELECT ("SalesRep")
        llNoThing = RLOCK()
        REPLACE Current WITH Current + lnRpEqvAmnt ,;
                Balance WITH Balance + lnRpEqvAmnt
        UNLOCK
        llNoThing = gfTraceKey("SalesRep", REPCODE, "M")
        llNoThing = lfUpdStp(lcRpCm, 1)
      ENDIF
        
      *-- Load the record from the temp file, and modify the vale
      *-- of some of the fields.
      SELECT (lcRpCm)
      SCATTER MEMVAR MEMO
      m.Tran     = lcAdTrnSeq
      m.Batch    = lcRepBat
      m.Amount   = lnRpEqvAmnt
      m.nForAmnt = lnRpFrnAmnt
      m.Balance  = Balance+lnRpEqvAmnt
  
      *-- Update the master rep commission file.
      IF &lcRpCm..nStep < 2
        SELECT RepComm
        APPEND BLANK
        GATHER MEMVAR MEMO
        llNoThing = lfUpdStp(lcRpCm, 2)
        llNoThing = gfTraceKey("RepComm", REPCODE+DTOS(DATE)+TRAN+TRANTYPE, "A")
      ENDIF
    ENDIF
  ENDSCAN
ENDIF

SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfUpdOnAcc
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Update the "D\C On Account" transactions in the
*:              master files.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfUpdOnAcc()
*:----------------------------------------------------------------
FUNCTION lfUpdOnAcc
PRIVATE lnAlias

lnAlias = SELECT(0)

*-- Go through just the D\C on account transactions, in other words
*-- do not include the Open Balance transactions, because it will 
*-- be handeled when saving the original selected D\C adjustment.

SELECT(lcOnAc)
SCAN FOR TranType $ "36"
  WAIT WINDOW "Updating Transaction Number : " + Tran NOWAIT
  
  
  *E500374,5  HBG  08/15/2000 If there is a factore ,take the Link Code of it,If not 
  *E500374,5                  take Customer's Link Code[Begin]
  lcCusLnk = lfGeLnkCd()
  IF SEEK("M" + lcAcc, "Customer")
    IF llGlLink AND SEEK(lcCusLnk+"001","GL_LINK")
      lcOnAcGLAc = GL_LINK.GLAcnt
    ENDIF
  ENDIF  
  *E500374,5 [End]
  *-- Generate a sequence number for the current transaction.
  IF &lcOnAc..nStep < 1
    lcToSnd    = IIF(TranType="3", "DEBIT", "CREDIT")
    lcAcTrnSeq = gfsequence(lcToSnd, gcAct_Comp, "", "", "TRAN")

    llNoThing  = lfUpdVars()
    llNoThing  = lfUpdStp(lcOnAc, 1)
  ENDIF

  *-- Load the record's information from the temp file, and
  *-- modify some of the fields values.
  SELECT(lcOnAc)
 
  SCATTER MEMVAR MEMO
  m.Tran       = lcAcTrnSeq
  m.HistDate   = ldKOD
  m.History    = lcHisSeq
  m.cARGLAcc   = lcOnAcGLAc
  
  m.cCreditCod = IIF(TranType = "3", SPACE(0), m.TranCode)
  
  *-- Update the master debit\credit files.
  IF &lcOnAc..nStep < 2
    lcFile = IIF(TranType = "3", lcDBFile, lcCRFile)
    SELECT(lcFile)
    APPEND BLANK
    GATHER MEMVAR MEMO

    lcFlToSend = IIF(lcFile = lcDBFile, "Debit", "Credit")
    lcExToSend0 = IIF(lcFile = lcDBFile, ACCOUNT+TRAN+CINSTALNO+DTOS(TRANDATE), ACCOUNT+TRAN+DTOS(TRANDATE))
    *B000000,1 Hesham (Start)
    *B000000,1 Change the value of the key passed to the trace function to be
    *B000000,1 like the master file tag
    lcTraceExp = IIF(lcFile = lcDBFile,TranType+TRAN+CINSTALNO,TranType+Tran)
*   llNoThing  = gfTraceKey(lcFlToSend, lcExToSend, "A")
    llNoThing  = gfTraceKey(lcFlToSend, lcTraceExp, "A")    
    *B000000,1 Hesham (End)
    llNoThing = lfUpdStp(lcOnAc, 2)
  ENDIF
  
  *-- Register a reverse GL transaction..
  IF &lcOnAc..nStep < 3 AND llGLLink
    SELECT (lcOnAc)
    
    DO GLDIST WITH "DEFDEF"   , "001"     , Amount   , "KO"     ,;
                   lcAcTrnSeq , ldKOD     , lcGLYear , lcGLPer  ,;
                   '&lcTmpGLD', lcOnAcGLAc, cCurrCode, nCurrUnit,;
                   nExRate

    SELECT (lcOnAc)
    llNoThing  = lfUpdStp(lcOnAc, 3)
  ENDIF
  
  *-- Update the master AR History file.
  IF &lcOnAc..nStep < 4

    SELECT (lcOnAc)
    *-- Compute the transactions currencey difference..
    IF llMltCur
      lcRateSign = gfGetExSin(@lcUnitSign,cCurrCode)  
      lnNewVal   = (Amount &lcRateSign nExRate &lcUnitSign nCurrUnit)
      lnExchDif  = lnExchDif + lnNewVal
    ENDIF

    m.TranType   = IIF(TranType = "3", "8", "9")
    m.TranCode   = IIF(TranType = "3", m.TranCode, m.cCreditCod)
    SELECT (lcHsFile)
    APPEND BLANK 
    GATHER MEMVAR MEMO
    lcKey     = ACCOUNT+HISTORY+TRANTYPE+TRAN+CINSTALNO
    llNoThing = gfTraceKey("ArHist",lcKey,"A")
    llNoThing = lfUpdStp(lcOnAc, 4)
  ENDIF
ENDSCAN

SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfUpdDiff
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Do a GL transaction with the total amount currency
*:              difference if any.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfUpdDiff()
*:----------------------------------------------------------------
FUNCTION lfUpdDiff
PRIVATE lnAlias

lnAlias   = SELECT(0)
lcGlVers  = gfGetMemVar("M_GL_VERS")
lcGLDir   = IIF(lcGlVers#"A",gfGetMemVar("M_GL_DIR"),IIF(SEEK(gcPrnt_Cmp,'SycComp'),gfGetDataDir(ALLTRIM(SycComp.cCom_dDir)),''))
lcUseAcc  = SPACE(0)

DO CASE
  *-- in case of linking with SBT 
  *B602583 (Start)
  *CASE INLIST(lcGlVers, "SBT7", "SBT2.5", "OTHERS")
  CASE INLIST(lcGlVers, "S", "O")
  *B602583 (End)
    lcUseAcc = gfGetMemVar('LNEXRATACC', gcAct_Comp)
    
  *-- in case of linking with aria  
  *B602583 (Start)
  *CASE lcGlVers = 'ARIAGL'
  CASE lcGlVers = 'A'  
  *B602583 (End)
    llOpnSetup = gfOpenFile(lcGLDir+"GLSETUP", "", "SH")
    llOpnAccCh = gfOpenFile(lcGLDir+"GLACCHAR","","SH")
    lcUseAcc   = ALLTRIM(GLSETUP.cSetExMj)
    llOpnAcCod = gfOpenFile(gcDataDir+"ACCOD", "Accsegno", "SH")
  
    *B602583 (Start)
    *SELECT ("SYCACCOD")
    *IF SEEK(gcAct_Comp)
    GO TOP
    *B602583 (End)
      lnAcsSegSz = nAcsSegSz 
      lnAcsNoSeg = nAcsNoSeg
      lcAcsMask  = "X" + SUBSTR(cAcsMask,2)
      lcAcsMask  = ALLTRIM(STRTRAN(lcAcsMask,'#','9'))
    *B602583 (Start)
    *ENDIF
    *B602583 (End)

    *-- Prepare account code according to the account code 
    *-- mask (lcAcsMask) as well as the field content
    lcUseAcc = lcUseAcc +;
    STRTRAN(SUBSTR(ALLTRIM(lcAcsMask),LEN(lcUseAcc)+1),'9','0') +;
    REPLICATE(' ',FSIZE('cAcctCode','GLACCHAR')-lnAcsSegSz)
    
    *E301077,53 YMA 03/03/77 Use gfCloseFile instead.
    *USE IN IIF(llOpnAcCod, "SYCACCOD", 0)
    *USE IN IIF(llOpnSetup, "GLSETUP" , 0)
    *USE IN IIF(llOpnAccCh, "GLACCHAR", 0)
    *= IIF(llOpnAcCod, gfCloseFile("SYCACCOD"), .F.)
    = IIF(llOpnAcCod, gfCloseFile("ACCOD"), .F.)
    = IIF(llOpnSetup, gfCloseFile("GLSETUP") , .F.)
    = IIF(llOpnAccCh, gfCloseFile("GLACCHAR"), .F.)
    *E301077,53 YMA 03/03/77 End.    
ENDCASE

IF llUpdGlDif
  SELECT(lcArHs)
  *B603081 (Start)
  lnExchDif = - lnExchDif
  *B603081 (End)
  
  *E500374,5  HBG  08/15/2000 If there is a factore ,take the Link Code of it,If not 
  *E500374,5                  take Customer's Link Code[Begin]
  lcCusLnk = lfGeLnkCd()
  *E500374,5 [End]
  
  DO GLDIST WITH lcCusLnk, "009", lnExchDif, "EX", "" ,;
                 ldKOD, lcGLYear, lcGLPer, '&lcTmpGLD',;
                 lcUseAcc
  llUpdGlDif = .F.
  llNoThing  = lfUpdVars()
ENDIF                 
               
SELECT(lnAlias)
   
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
*E301509,1 (Start)
PARAMETERS llFromEdi
*E301509,1 (End)

PRIVATE lnAlias

lnAlias = SELECT(0)

*E301077,53 YMA 03/03/99 Start - Open the history file.
= gfOpenFile(gcDataDir+"ARHist","ARHISTHT",'SH')
*E301077,53 YMA 03/03/99 End.

*-- Read the AR History file structure into an array.
SELECT ArHist
lnArHsFlds = AFIELDS(laArHsStr)

*-- Add the "nStep" field to all the temp files to handel 
*-- the incomplete session.
lnArHsFlds = lnArHsFlds + 1
*E301509,1 (Start)
*DIMENSION laArHsStr[lnArHsFlds, 4]
IF llFromEDI
  DIMENSION laArHsStr[lnArHsFlds, 16]
ELSE
  DIMENSION laArHsStr[lnArHsFlds, 4]
ENDIF
*E301509,1 (End)    
laArHsStr[lnArHsFlds, 1] = "nStep"
laArHsStr[lnArHsFlds, 2] = "N"
laArHsStr[lnArHsFlds, 3] = 2
laArHsStr[lnArHsFlds, 4] = 0
*E301509,1 (Start)
IF llFromEDI
  laArHsStr[lnArHsFlds,05] = .F.
  laArHsStr[lnArHsFlds,06] = .F.
  laArHsStr[lnArHsFlds,07] = ''
  laArHsStr[lnArHsFlds,08] = ''
  laArHsStr[lnArHsFlds,09] = ''
  laArHsStr[lnArHsFlds,10] = ''
  laArHsStr[lnArHsFlds,11] = ''
  laArHsStr[lnArHsFlds,12] = ''
  laArHsStr[lnArHsFlds,13] = ''
  laArHsStr[lnArHsFlds,14] = ''
  laArHsStr[lnArHsFlds,15] = ''
  laArHsStr[lnArHsFlds,16] = ''
ENDIF
*E301509,1 (End)

*-- Copy AR History file structure with the new added field 
*-- to three different arrays to be used to create the 
*-- three different temp files.
lnCDAdFlds = ACOPY(laArHsStr, laDCAdjStr)
lnCDOnFlds = ACOPY(laArHsStr, laDCOnAStr)
lnCDAlFlds = ACOPY(laArHsStr, laDCAllStr)

*-- Debit\Credit Adjustment Temporary file.
lnCDAdFlds = lnArHsFlds

*-- The reason description temp field.
lnCDAdFlds = lnCDAdFlds + 1
*E301509,1 (Start)
*DIMENSION laDCAdjStr[lnCDAdFlds, 4]
IF llFromEDI
  DIMENSION laDCAdjStr[lnCDAdFlds, 16]
ELSE
  DIMENSION laDCAdjStr[lnCDAdFlds, 4]
ENDIF
*E301509,1 (End)
laDCAdjStr[lnCDAdFlds,1] = "cReason"
laDCAdjStr[lnCDAdFlds,2] = "C"
laDCAdjStr[lnCDAdFlds,3] = 30
laDCAdjStr[lnCDAdFlds,4] = 0
*E301509,1 (Start)
IF llFromEDI
  laDCAdjStr[lnCDAdFlds,05] = .F.
  laDCAdjStr[lnCDAdFlds,06] = .F.
  laDCAdjStr[lnCDAdFlds,07] = ''
  laDCAdjStr[lnCDAdFlds,08] = ''
  laDCAdjStr[lnCDAdFlds,09] = ''
  laDCAdjStr[lnCDAdFlds,10] = ''
  laDCAdjStr[lnCDAdFlds,11] = ''
  laDCAdjStr[lnCDAdFlds,12] = ''
  laDCAdjStr[lnCDAdFlds,13] = ''
  laDCAdjStr[lnCDAdFlds,14] = ''
  laDCAdjStr[lnCDAdFlds,15] = ''
  laDCAdjStr[lnCDAdFlds,16] = ''
ENDIF
*E301509,1 (End)

*-- The record fiscal year.
lnCDAdFlds = lnCDAdFlds + 1

*E301509,1 (Start)
*DIMENSION laDCAdjStr[lnCDAdFlds, 4]
IF llFromEDI
  DIMENSION laDCAdjStr[lnCDAdFlds, 16]
ELSE
  DIMENSION laDCAdjStr[lnCDAdFlds, 4]
ENDIF
*E301509,1 (End)

laDCAdjStr[lnCDAdFlds,1] = "cYear"
laDCAdjStr[lnCDAdFlds,2] = "C"
laDCAdjStr[lnCDAdFlds,3] = 4
laDCAdjStr[lnCDAdFlds,4] = 0
*E301509,1 (Start)
IF llFromEDI
  laDCAdjStr[lnCDAdFlds,05] = .F.
  laDCAdjStr[lnCDAdFlds,06] = .F.
  laDCAdjStr[lnCDAdFlds,07] = ''
  laDCAdjStr[lnCDAdFlds,08] = ''
  laDCAdjStr[lnCDAdFlds,09] = ''
  laDCAdjStr[lnCDAdFlds,10] = ''
  laDCAdjStr[lnCDAdFlds,11] = ''
  laDCAdjStr[lnCDAdFlds,12] = ''
  laDCAdjStr[lnCDAdFlds,13] = ''
  laDCAdjStr[lnCDAdFlds,14] = ''
  laDCAdjStr[lnCDAdFlds,15] = ''
  laDCAdjStr[lnCDAdFlds,16] = ''
ENDIF
*E301509,1 (End)

*-- The record fiscal period.
lnCDAdFlds = lnCDAdFlds + 1
*E301509,1 (Start)
*DIMENSION laDCAdjStr[lnCDAdFlds, 4]
IF llFromEDI
  DIMENSION laDCAdjStr[lnCDAdFlds, 16]
ELSE
  DIMENSION laDCAdjStr[lnCDAdFlds, 4]
ENDIF
*E301509,1 (End)

laDCAdjStr[lnCDAdFlds,1] = "cPrd"
laDCAdjStr[lnCDAdFlds,2] = "C"
laDCAdjStr[lnCDAdFlds,3] = 2
laDCAdjStr[lnCDAdFlds,4] = 0
*E301509,1 (Start)
IF llFromEDI
  laDCAdjStr[lnCDAdFlds,05] = .F.
  laDCAdjStr[lnCDAdFlds,06] = .F.
  laDCAdjStr[lnCDAdFlds,07] = ''
  laDCAdjStr[lnCDAdFlds,08] = ''
  laDCAdjStr[lnCDAdFlds,09] = ''
  laDCAdjStr[lnCDAdFlds,10] = ''
  laDCAdjStr[lnCDAdFlds,11] = ''
  laDCAdjStr[lnCDAdFlds,12] = ''
  laDCAdjStr[lnCDAdFlds,13] = ''
  laDCAdjStr[lnCDAdFlds,14] = ''
  laDCAdjStr[lnCDAdFlds,15] = ''
  laDCAdjStr[lnCDAdFlds,16] = ''
ENDIF
*E301509,1 (End)

*-- The bank code for the reason code.
*-- (we get the value for this field from the codes file 
*-- as a related field for the selected reason code )

*N000232,1 HS  04/12/2001 Following lines are commented out
*N000232,1 HS  04/12/2001 Fields cBnkCode & cChkAcct already added to ARHIST file
*lnCDAdFlds = lnCDAdFlds + 1
**E301509,1 (Start)
**DIMENSION laDCAdjStr[lnCDAdFlds, 4]
*IF llFromEDI
*  DIMENSION laDCAdjStr[lnCDAdFlds, 16]
*ELSE
*  DIMENSION laDCAdjStr[lnCDAdFlds, 4]
*ENDIF
**E301509,1 (End)
*
*laDCAdjStr[lnCDAdFlds,1] = "cBnkCode"
*laDCAdjStr[lnCDAdFlds,2] = "C"
*laDCAdjStr[lnCDAdFlds,3] = 8
*laDCAdjStr[lnCDAdFlds,4] = 0
**E301509,1 (Start)
*IF llFromEDI
*  laDCAdjStr[lnCDAdFlds,05] = .F.
*  laDCAdjStr[lnCDAdFlds,06] = .F.
*  laDCAdjStr[lnCDAdFlds,07] = ''
*  laDCAdjStr[lnCDAdFlds,08] = ''
*  laDCAdjStr[lnCDAdFlds,09] = ''
*  laDCAdjStr[lnCDAdFlds,10] = ''
*  laDCAdjStr[lnCDAdFlds,11] = ''
*  laDCAdjStr[lnCDAdFlds,12] = ''
*  laDCAdjStr[lnCDAdFlds,13] = ''
*  laDCAdjStr[lnCDAdFlds,14] = ''
*  laDCAdjStr[lnCDAdFlds,15] = ''
*  laDCAdjStr[lnCDAdFlds,16] = ''
*ENDIF
**E301509,1 (End)
*
**-- The checking account for the reason code.
**-- (we get the value for this field from the codes file 
**-- as a related field for the selected reason code )
*lnCDAdFlds = lnCDAdFlds + 1
**E301509,1 (Start)
**DIMENSION laDCAdjStr[lnCDAdFlds, 4]
*IF llFromEDI
*  DIMENSION laDCAdjStr[lnCDAdFlds, 16]
*ELSE
*  DIMENSION laDCAdjStr[lnCDAdFlds, 4]
*ENDIF
**E301509,1 (End)
*
*laDCAdjStr[lnCDAdFlds,1] = "cChkAcct"
*laDCAdjStr[lnCDAdFlds,2] = "C"
*laDCAdjStr[lnCDAdFlds,3] = 12
*laDCAdjStr[lnCDAdFlds,4] = 0
**E301509,1 (Start)
*IF llFromEDI
*  laDCAdjStr[lnCDAdFlds,05] = .F.
*  laDCAdjStr[lnCDAdFlds,06] = .F.
*  laDCAdjStr[lnCDAdFlds,07] = ''
*  laDCAdjStr[lnCDAdFlds,08] = ''
*  laDCAdjStr[lnCDAdFlds,09] = ''
*  laDCAdjStr[lnCDAdFlds,10] = ''
*  laDCAdjStr[lnCDAdFlds,11] = ''
*  laDCAdjStr[lnCDAdFlds,12] = ''
*  laDCAdjStr[lnCDAdFlds,13] = ''
*  laDCAdjStr[lnCDAdFlds,14] = ''
*  laDCAdjStr[lnCDAdFlds,15] = ''
*  laDCAdjStr[lnCDAdFlds,16] = ''
*ENDIF
**E301509,1 (End)
*N000232,1 HS  04/12/2001 Following lines are commented out (End)

*-- Debit\Credit On Account Temporary file.
lnCDOnFlds = lnArHsFlds

*-- The reason description temp field.
lnCDOnFlds = lnCDOnFlds + 1
*E301509,1 (Start)
*DIMENSION laDCOnAStr[lnCDOnFlds, 4]
IF llFromEDI
  DIMENSION laDCOnAStr[lnCDOnFlds, 16]
ELSE
  DIMENSION laDCOnAStr[lnCDOnFlds, 4]
ENDIF
*E301509,1 (End)

laDCOnAStr[lnCDOnFlds,1] = "cReason"
laDCOnAStr[lnCDOnFlds,2] = "C"
laDCOnAStr[lnCDOnFlds,3] = 30
laDCOnAStr[lnCDOnFlds,4] = 0
*E301509,1 (Start)
IF llFromEDI
  laDCOnAStr[lnCDOnFlds,05] = .F.
  laDCOnAStr[lnCDOnFlds,06] = .F.
  laDCOnAStr[lnCDOnFlds,07] = ''
  laDCOnAStr[lnCDOnFlds,08] = ''
  laDCOnAStr[lnCDOnFlds,09] = ''
  laDCOnAStr[lnCDOnFlds,10] = ''
  laDCOnAStr[lnCDOnFlds,11] = ''
  laDCOnAStr[lnCDOnFlds,12] = ''
  laDCOnAStr[lnCDOnFlds,13] = ''
  laDCOnAStr[lnCDOnFlds,14] = ''
  laDCOnAStr[lnCDOnFlds,15] = ''
  laDCOnAStr[lnCDOnFlds,16] = ''
ENDIF
*E301509,1 (End)

*-- The open balance transaction code.
lnCDOnFlds = lnCDOnFlds + 1
*E301509,1 (Start)
*DIMENSION laDCOnAStr[lnCDOnFlds, 4]
IF llFromEDI
  DIMENSION laDCOnAStr[lnCDOnFlds, 16]
ELSE
  DIMENSION laDCOnAStr[lnCDOnFlds, 4]
ENDIF
*E301509,1 (End)

laDCOnAStr[lnCDOnFlds,1] = "cOpnBalTrn"
laDCOnAStr[lnCDOnFlds,2] = "C"
laDCOnAStr[lnCDOnFlds,3] = 6
laDCOnAStr[lnCDOnFlds,4] = 0
*E301509,1 (Start)
IF llFromEDI
  laDCOnAStr[lnCDOnFlds,05] = .F.
  laDCOnAStr[lnCDOnFlds,06] = .F.
  laDCOnAStr[lnCDOnFlds,07] = ''
  laDCOnAStr[lnCDOnFlds,08] = ''
  laDCOnAStr[lnCDOnFlds,09] = ''
  laDCOnAStr[lnCDOnFlds,10] = ''
  laDCOnAStr[lnCDOnFlds,11] = ''
  laDCOnAStr[lnCDOnFlds,12] = ''
  laDCOnAStr[lnCDOnFlds,13] = ''
  laDCOnAStr[lnCDOnFlds,14] = ''
  laDCOnAStr[lnCDOnFlds,15] = ''
  laDCOnAStr[lnCDOnFlds,16] = ''
ENDIF
*E301509,1 (End)

*-- The type open balance. (is it a debit\credit on account)
lnCDOnFlds = lnCDOnFlds + 1
*E301509,1 (Start)
*DIMENSION laDCOnAStr[lnCDOnFlds, 4]
IF llFromEDI
  DIMENSION laDCOnAStr[lnCDOnFlds, 16]
ELSE
  DIMENSION laDCOnAStr[lnCDOnFlds, 4]
ENDIF
*E301509,1 (End)

laDCOnAStr[lnCDOnFlds,1] = "cOpenType"
laDCOnAStr[lnCDOnFlds,2] = "C"
laDCOnAStr[lnCDOnFlds,3] = 1
laDCOnAStr[lnCDOnFlds,4] = 0
*E301509,1 (Start)
IF llFromEDI
  laDCOnAStr[lnCDOnFlds,05] = .F.
  laDCOnAStr[lnCDOnFlds,06] = .F.
  laDCOnAStr[lnCDOnFlds,07] = ''
  laDCOnAStr[lnCDOnFlds,08] = ''
  laDCOnAStr[lnCDOnFlds,09] = ''
  laDCOnAStr[lnCDOnFlds,10] = ''
  laDCOnAStr[lnCDOnFlds,11] = ''
  laDCOnAStr[lnCDOnFlds,12] = ''
  laDCOnAStr[lnCDOnFlds,13] = ''
  laDCOnAStr[lnCDOnFlds,14] = ''
  laDCOnAStr[lnCDOnFlds,15] = ''
  laDCOnAStr[lnCDOnFlds,16] = ''
ENDIF
*E301509,1 (End)

*-- The transaction type for the transaction that has the open balance.
lnCDOnFlds = lnCDOnFlds + 1
*E301509,1 (Start)
*DIMENSION laDCOnAStr[lnCDOnFlds, 4]
IF llFromEDI
  DIMENSION laDCOnAStr[lnCDOnFlds, 16]
ELSE
  DIMENSION laDCOnAStr[lnCDOnFlds, 4]
ENDIF
*E301509,1 (End)

laDCOnAStr[lnCDOnFlds,1] = "cOpnTrnTyp"
laDCOnAStr[lnCDOnFlds,2] = "C"
laDCOnAStr[lnCDOnFlds,3] = 1
laDCOnAStr[lnCDOnFlds,4] = 0
*E301509,1 (Start)
IF llFromEDI
  laDCOnAStr[lnCDOnFlds,05] = .F.
  laDCOnAStr[lnCDOnFlds,06] = .F.
  laDCOnAStr[lnCDOnFlds,07] = ''
  laDCOnAStr[lnCDOnFlds,08] = ''
  laDCOnAStr[lnCDOnFlds,09] = ''
  laDCOnAStr[lnCDOnFlds,10] = ''
  laDCOnAStr[lnCDOnFlds,11] = ''
  laDCOnAStr[lnCDOnFlds,12] = ''
  laDCOnAStr[lnCDOnFlds,13] = ''
  laDCOnAStr[lnCDOnFlds,14] = ''
  laDCOnAStr[lnCDOnFlds,15] = ''
  laDCOnAStr[lnCDOnFlds,16] = ''
ENDIF
*E301509,1 (End)

*-- The amount for the transaction that has the open balance.
lnCDOnFlds = lnCDOnFlds + 1
*E301509,1 (Start)
*DIMENSION laDCOnAStr[lnCDOnFlds, 4]
IF llFromEDI
  DIMENSION laDCOnAStr[lnCDOnFlds, 16]
ELSE
  DIMENSION laDCOnAStr[lnCDOnFlds, 4]
ENDIF
*E301509,1 (End)

laDCOnAStr[lnCDOnFlds,1] = "nOpnTrnAmn"
laDCOnAStr[lnCDOnFlds,2] = "N"
laDCOnAStr[lnCDOnFlds,3] = 11
laDCOnAStr[lnCDOnFlds,4] = 2
*E301509,1 (Start)
IF llFromEDI
  laDCOnAStr[lnCDOnFlds,05] = .F.
  laDCOnAStr[lnCDOnFlds,06] = .F.
  laDCOnAStr[lnCDOnFlds,07] = ''
  laDCOnAStr[lnCDOnFlds,08] = ''
  laDCOnAStr[lnCDOnFlds,09] = ''
  laDCOnAStr[lnCDOnFlds,10] = ''
  laDCOnAStr[lnCDOnFlds,11] = ''
  laDCOnAStr[lnCDOnFlds,12] = ''
  laDCOnAStr[lnCDOnFlds,13] = ''
  laDCOnAStr[lnCDOnFlds,14] = ''
  laDCOnAStr[lnCDOnFlds,15] = ''
  laDCOnAStr[lnCDOnFlds,16] = ''
ENDIF
*E301509,1 (End)

*-- All keyed-off transactions temporary file.
lnCDAlFlds = lnArHsFlds

*-- The record fiscal year.
lnCDAlFlds = lnCDAlFlds + 1
*E301509,1 (Start)
*DIMENSION laDCAllStr[lnCDAlFlds, 4]
IF llFromEDI
  DIMENSION laDCAllStr[lnCDAlFlds, 16]
ELSE
  DIMENSION laDCAllStr[lnCDAlFlds, 4]
ENDIF
*E301509,1 (End)

laDCAllStr[lnCDAlFlds,1] = "cYear"
laDCAllStr[lnCDAlFlds,2] = "C"
laDCAllStr[lnCDAlFlds,3] = 4
laDCAllStr[lnCDAlFlds,4] = 0
*E301509,1 (Start)
IF llFromEDI
  laDCAllStr[lnCDAlFlds,05] = .F.
  laDCAllStr[lnCDAlFlds,06] = .F.
  laDCAllStr[lnCDAlFlds,07] = ''
  laDCAllStr[lnCDAlFlds,08] = ''
  laDCAllStr[lnCDAlFlds,09] = ''
  laDCAllStr[lnCDAlFlds,10] = ''
  laDCAllStr[lnCDAlFlds,11] = ''
  laDCAllStr[lnCDAlFlds,12] = ''
  laDCAllStr[lnCDAlFlds,13] = ''
  laDCAllStr[lnCDAlFlds,14] = ''
  laDCAllStr[lnCDAlFlds,15] = ''
  laDCAllStr[lnCDAlFlds,16] = ''
ENDIF
*E301509,1 (End)
  
*-- The record fiscal period.
lnCDAlFlds = lnCDAlFlds + 1
*E301509,1 (Start)
*DIMENSION laDCAllStr[lnCDAlFlds, 4]
IF llFromEDI
  DIMENSION laDCAllStr[lnCDAlFlds, 16]
ELSE
  DIMENSION laDCAllStr[lnCDAlFlds, 4]
ENDIF
*E301509,1 (End)

laDCAllStr[lnCDAlFlds,1] = "cPrd"
laDCAllStr[lnCDAlFlds,2] = "C"
laDCAllStr[lnCDAlFlds,3] = 2
laDCAllStr[lnCDAlFlds,4] = 0
*E301509,1 (Start)
IF llFromEDI
  laDCAllStr[lnCDAlFlds,05] = .F.
  laDCAllStr[lnCDAlFlds,06] = .F.
  laDCAllStr[lnCDAlFlds,07] = ''
  laDCAllStr[lnCDAlFlds,08] = ''
  laDCAllStr[lnCDAlFlds,09] = ''
  laDCAllStr[lnCDAlFlds,10] = ''
  laDCAllStr[lnCDAlFlds,11] = ''
  laDCAllStr[lnCDAlFlds,12] = ''
  laDCAllStr[lnCDAlFlds,13] = ''
  laDCAllStr[lnCDAlFlds,14] = ''
  laDCAllStr[lnCDAlFlds,15] = ''
  laDCAllStr[lnCDAlFlds,16] = ''
ENDIF
*E301509,1 (End)

*-- The bank code for the reason code.
*-- (we get the value for this field from the codes file 
*-- as a related field for the selected reason code )
*lnCDAlFlds = lnCDAlFlds + 1
**E301509,1 (Start)
**DIMENSION laDCAllStr[lnCDAlFlds, 4]
*IF llFromEDI
*  DIMENSION laDCAllStr[lnCDAlFlds, 16]
*ELSE
*  DIMENSION laDCAllStr[lnCDAlFlds, 4]
*ENDIF
**E301509,1 (End)
*
*laDCAllStr[lnCDAlFlds,1] = "cBnkCode"
*laDCAllStr[lnCDAlFlds,2] = "C"
*laDCAllStr[lnCDAlFlds,3] = 8
*laDCAllStr[lnCDAlFlds,4] = 0
**E301509,1 (Start)
*IF llFromEDI
*  laDCAllStr[lnCDAlFlds,05] = .F.
*  laDCAllStr[lnCDAlFlds,06] = .F.
*  laDCAllStr[lnCDAlFlds,07] = ''
*  laDCAllStr[lnCDAlFlds,08] = ''
*  laDCAllStr[lnCDAlFlds,09] = ''
*  laDCAllStr[lnCDAlFlds,10] = ''
*  laDCAllStr[lnCDAlFlds,11] = ''
*  laDCAllStr[lnCDAlFlds,12] = ''
*  laDCAllStr[lnCDAlFlds,13] = ''
*  laDCAllStr[lnCDAlFlds,14] = ''
*  laDCAllStr[lnCDAlFlds,15] = ''
*  laDCAllStr[lnCDAlFlds,16] = ''
*ENDIF
**E301509,1 (End)
*
**-- The checking account for the reason code.
**-- (we get the value for this field from the codes file 
**-- as a related field for the selected reason code )
*lnCDAlFlds = lnCDAlFlds + 1
**E301509,1 (Start)
**DIMENSION laDCAllStr[lnCDAlFlds, 4]
*IF llFromEDI
*  DIMENSION laDCAllStr[lnCDAlFlds, 16]
*ELSE
*  DIMENSION laDCAllStr[lnCDAlFlds, 4]
*ENDIF
**E301509,1 (End)
*
*laDCAllStr[lnCDAlFlds,1] = "cChkAcct"
*laDCAllStr[lnCDAlFlds,2] = "C"
*laDCAllStr[lnCDAlFlds,3] = 12
*laDCAllStr[lnCDAlFlds,4] = 0
**E301509,1 (Start)
*IF llFromEDI
*  laDCAllStr[lnCDAlFlds,05] = .F.
*  laDCAllStr[lnCDAlFlds,06] = .F.
*  laDCAllStr[lnCDAlFlds,07] = ''
*  laDCAllStr[lnCDAlFlds,08] = ''
*  laDCAllStr[lnCDAlFlds,09] = ''
*  laDCAllStr[lnCDAlFlds,10] = ''
*  laDCAllStr[lnCDAlFlds,11] = ''
*  laDCAllStr[lnCDAlFlds,12] = ''
*  laDCAllStr[lnCDAlFlds,13] = ''
*  laDCAllStr[lnCDAlFlds,14] = ''
*  laDCAllStr[lnCDAlFlds,15] = ''
*  laDCAllStr[lnCDAlFlds,16] = ''
*ENDIF
**E301509,1 (End)

*-- If this transaction has an open balance, this field
*-- will have a value of "N", otherwise it will have a value
*-- of "Y".
lnCDAlFlds = lnCDAlFlds + 1
*E301509,1 (Start)
*DIMENSION laDCAllStr[lnCDAlFlds, 4]
IF llFromEDI
  DIMENSION laDCAllStr[lnCDAlFlds, 16]
ELSE
  DIMENSION laDCAllStr[lnCDAlFlds, 4]
ENDIF
*E301509,1 (End)

laDCAllStr[lnCDAlFlds,1] = "cShToOpn"
laDCAllStr[lnCDAlFlds,2] = "C"
laDCAllStr[lnCDAlFlds,3] = 1
laDCAllStr[lnCDAlFlds,4] = 0
*E301509,1 (Start)
IF llFromEDI
  laDCAllStr[lnCDAlFlds,05] = .F.
  laDCAllStr[lnCDAlFlds,06] = .F.
  laDCAllStr[lnCDAlFlds,07] = ''
  laDCAllStr[lnCDAlFlds,08] = ''
  laDCAllStr[lnCDAlFlds,09] = ''
  laDCAllStr[lnCDAlFlds,10] = ''
  laDCAllStr[lnCDAlFlds,11] = ''
  laDCAllStr[lnCDAlFlds,12] = ''
  laDCAllStr[lnCDAlFlds,13] = ''
  laDCAllStr[lnCDAlFlds,14] = ''
  laDCAllStr[lnCDAlFlds,15] = ''
  laDCAllStr[lnCDAlFlds,16] = ''
ENDIF
*E301509,1 (End)

*-- The new amount for the transaction that has an open balance.
lnCDAlFlds = lnCDAlFlds + 1
*E301509,1 (Start)
*DIMENSION laDCAllStr[lnCDAlFlds, 4]
IF llFromEDI
  DIMENSION laDCAllStr[lnCDAlFlds, 16]
ELSE
  DIMENSION laDCAllStr[lnCDAlFlds, 4]
ENDIF
*E301509,1 (End)

laDCAllStr[lnCDAlFlds,1] = "nTrnNewAmn"
laDCAllStr[lnCDAlFlds,2] = "N"
laDCAllStr[lnCDAlFlds,3] = 11
laDCAllStr[lnCDAlFlds,4] = 2
*E301509,1 (Start)
IF llFromEDI
  laDCAllStr[lnCDAlFlds,05] = .F.
  laDCAllStr[lnCDAlFlds,06] = .F.
  laDCAllStr[lnCDAlFlds,07] = ''
  laDCAllStr[lnCDAlFlds,08] = ''
  laDCAllStr[lnCDAlFlds,09] = ''
  laDCAllStr[lnCDAlFlds,10] = ''
  laDCAllStr[lnCDAlFlds,11] = ''
  laDCAllStr[lnCDAlFlds,12] = ''
  laDCAllStr[lnCDAlFlds,13] = ''
  laDCAllStr[lnCDAlFlds,14] = ''
  laDCAllStr[lnCDAlFlds,15] = ''
  laDCAllStr[lnCDAlFlds,16] = ''
ENDIF
*E301509,1 (End)

*-- Create the three temp files.
llNoThing = gfCrtTmp(lcDCAdjTmp, @laDCAdjStr)
llNoThing = gfCrtTmp(lcDCOnATmp, @laDCOnAStr)
llNoThing = gfCrtTmp(lcDCAllTmp, @laDCAllStr)

*-- Close them.
USE IN (lcDCAdjTmp)
USE IN (lcDCOnATmp)
USE IN (lcDCAllTmp)

*-- Re-open the file exclusively to index it.
USE (gcWorkDir+lcDCAllTmp) IN 0 EXCLUSIVE
SELECT(lcDCAllTmp)
INDEX ON Account+TranType+Tran+cInstalNo TAG (lcDCAllTmp)
INDEX ON Account+Tran+cInstalNo          TAG Tran
INDEX ON cShToOpn                        TAG Show
USE IN (lcDCAllTmp)

*-- Re-Open the files shared.
USE (gcWorkDir+lcDCAdjTmp) IN 0 SHARED
USE (gcWorkDir+lcDCOnATmp) IN 0 SHARED
USE (gcWorkDir+lcDCAllTmp) IN 0 SHARED ORDER (lcDCAllTmp)

SELECT(lnAlias)

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

*E301077,53 YMA 03/03/99 YMA Open the incomplete session file.
= gfOpenFile (gcDataDir+"UNCMSESS", "TRANS", "SH")
*E301077,53 YMA 03/03/99 End.

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
  *B803653,1 NAD 09/24/2000 (Start) Correct spelling of word apply.
  *IF gfUnCompSession(lcProgID, lnThisSess, "Applay Debits/Credits")
  IF gfUnCompSession(lcProgID, lnThisSess, "Apply Debits/Credits") 
  *B803653,1 NAD (End)
    *-- If yes, please tell me where did we stop in that session..
    lcCurObj  = ALLTRIM(UnCmSess.cCurrObj)
    
    *-- If you do not know where did we stop, please reset the 
    *-- variables and go to the normal add mode.
    IF EMPTY(lcCurObj)
      llNoThing = lfRestVar()
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
        *-- go on and finish the update..
        lnNoThing    = lpSavScr()
        
        *-- Remember; when you finish to create the 
        *-- temp files again (Just to balnk them out)..
        llNoThing    = lfCratTemp()
        
        *-- And go to the select mode just to give the user the 
        *-- feeling that you are running normal.
        lnActFolder  = 1
        laScrMode    = .F.
        laScrMode[1] = .T.
        llFondSess   = .F.
        llFromSPR    = .T.
        DO lpShow
      ELSE
        llNoThing = lfRestVar()
        SHOW GETS
        _CUROBJ = OBJNUM(&lcCurObj)
        KEYBOARD "{ENTER}" PLAIN CLEAR
      ENDIF
    ENDIF
  ELSE
    *-- If there is no incomplete session, please create the temp
    *-- files.
    llNoThing = lfCratTemp()
  ENDIF
ENDIF

*-- Do all the needed browses..
llNoThing = lfBrDebits() AND lfBrCrdits()
llNoThing = lfBrAdjust() AND lfBrOnAcc () AND lfBrAll()

RETURN (llFondSess)

*:----------------------------------------------------------------
*: Name       : lfRestVar
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : Reset the needed variables when finding any previous
*:              incomplete session.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfRestVar()
*:----------------------------------------------------------------
FUNCTION lfRestVar

llFondSess   = .T.
laScrMode    = .F.
laScrMode[4] = .T.
llFromSPR    = .T.
llUnComp     = .T.
llValidAcc   = .T.
llValidCur   = .T.
lcSession    = UnCmSess.cSession
llNoThing    = lfChngFolder(lnActFolder)
llValidDat   = CheckPrd(ldKODate,"lcGLFYear","lcGLPeriod",'KO',.T.)
llNoThing    = lfSetFlter("Debit" , .T., 2)
llNoThing    = lfSetFlter("Credit", .F., 2)
llNoThing    = lfBldShow(lcCurCode)

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
IF !SEEK('I')
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
    *B603651,1 (Start) to Add new record in the UnCMSESS file
    lnUncmSeRc=0
    *B603651,1 (End)
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

IF TYPE("lcArHs") = "C"
  lcFiles = "lcOnAc," + lcOnAc + ", ;" +;
            "lcArHs," + lcArHs + ","   + ORDER(lcArHs) + ";"
  IF USED(lcRpCm)
    lcFiles = lcFiles +;
            "lcRpCm," + lcRpCm + ","   + ORDER(lcRpCm) + ";"
  ENDIF
ELSE
  *-- Build the files names string.
  lcFiles = "lcDCAdjTmp," + lcDCAdjTmp + ", ;" +;
            "lcDCOnATmp," + lcDCOnATmp + ", ;" +;
            "lcDCAllTmp," + lcDCAllTmp + ","   + ORDER(lcDCAllTmp) + ";"
  *-- If the temp sales commeission file is used, please include
  *-- it in the files string.
  IF USED(lcRpComTmp)
    lcFiles = lcFiles +;
            "lcRpComTmp," + lcRpComTmp + ","   + ORDER(lcRpComTmp) + ";"
  ENDIF
ENDIF

*-- Go and update the incomplete session record.
llNoThing = IIF(lnUnCmSeRc=0, .T., gfSavSess(lcProgID, lcFiles, @laVars, lcSession))

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
REPLACE nStep WITH lnStepNo
UNLOCK
SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfUpdCusH
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 02/21/98
*: Purpose    : To update the customer history file.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfUpdCusH()
*:----------------------------------------------------------------
FUNCTION lfUpdCusH
PRIVATE lnAlias

*-- Go to the customer\year record in the customer history file.
IF SEEK(lcAcc+&lcArHs..cYear, lcCusHsF)
  lnAlias = SELECT(0)
  SELECT (lcCusHsF)
  
  *-- Specify the fields to be updated, if the transaction
  *-- is an allowance, we will update the "nCrAdj" fields,
  *-- otherwise the "nDrAdj" field will be updated.
  lcPer     = PADL(ALLTRIM(&lcArHs..cPrd), 2, "0")
  *B603860,1 NAD  08/30/2000  (Start) Update nallow field instead of nCrAdj in case ot type 7
  *lcFld    = IIF(&lcArHs..TranType = "7", "nCrAdj", "nDrAdj")
  lcFld     = IIF(&lcArHs..TranType = "7", "nAllow", "nDrAdj")
  *B603860,1 NAD  (End)
  
  *B605667,1 SSE Commented out. [Begin]
  *lcFld     = IIF(&lcArHs..TranType = "7", "nCrAdj", "nDrAdj")
  *B605667,1 SSE Commented out. [End]

  lcTot     = lcFld + lcPer
  lnAmn     = ABS(&lcArHs..Amount)
  lnEqvAmn  = ROUND(lnAmn &lcExRSin lnExRate &lcUntSin lnCurrUnit, 2)
              
  *-- Update the customer history record.
  llNoThing = RLOCK()
  REPLACE &lcFld WITH &lcFld + lnEqvAmn ,;
          &lcTot WITH &lcTot + lnEqvAmn
  UNLOCK
  SELECT(lnAlias)
ENDIF

*:----------------------------------------------------------------
*: Name       : lfvAutoKey
*: Developer  : Ahmed Amer - (AHM)
*: Date       : 06/22/99
*: Purpose    : Automatic key-of
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvAutoKey()
*:----------------------------------------------------------------

FUNCTION lfvAutoKey

*E301281,1 (Start)
PARAMETERS llExtAuto
PRIVATE llRet
llRet = .T.
*E301281,1 (End)

PRIVATE lnCurAlias,lnTotDr,lnTotCr

lnCurAlias = SELECT(0)

SELECT DEBIT
SUM(Amount) FOR Account+Tran+cInstalNo+DTOS(TranDate) = '' TO lnTotDr
lnTotDr = ABS(lnTotDr)

SELECT CREDIT
SUM(Amount) FOR Account+Tran+DTOS(TranDate) = '' TO lnTotCr
lnTotCr = ABS(lnTotCr)


IF lnTotDr = 0 OR lnTotCr = 0
  *-- Cannot do Automatic Key-Off. Either the credit or the debit amount, 
  *-- or both are 0 balance.
  *-- <OK>

  *E301281,1 (Start)
  llRet = .F.
  IF TYPE('lcProg') $ 'UL' OR lcProg = "CR"
  *E301281,1 (End)
  
    lnNoThing  = gfModalGen('INM40159B00000')

  *E301281,1 (Start)
  ENDIF
  *E301281,1 (End)

ELSE
  *---Empty temp. files from old transactions
  *-- Automatic key-off will overwrite any transaction you have made before. 
  *-- Do you want to continue?
  *-- <Yes>, <No>

  *E301281,1 (Start)
  *IF gfModalGen('QRM40160B00006') = 1
  IF llExtAuto OR gfModalGen('QRM40160B00006') = 1
  *E301281,1 (End)
  
    *E301281,1 (Start)
    IF !llExtAuto
    *E301281,1 (End)
  
      IF lnActFolder <> 1
        lnLastFold  = lnActFolder
        lnActFolder = 1
        llNoThing   = lfChngFolder(1)
      ENDIF
  
      =lfvSelNon("C")
      =lfvSelNon("D")
      SELECT (lcDCAdjTmp)
      SCAN
        = lfvAdjRem(.T.)
      ENDSCAN
      SELECT (lcDCOnATmp)
      SCAN
        =lfvAccRem(.F.,.T.,.T.)
      ENDSCAN
    
    *E301281,1 (Start)
    ENDIF
    *E301281,1 (End)

    PRIVATE lnTotAmt,lcKeyExp
    lnTotAmt = 0
    *---Add new transactions in tmp. files
    DO CASE
      CASE lnTotDr < lnTotCr
        =lfvSelAll('D')
        SELECT Credit
        GO TOP
        *B603191,1 Scan while lnTotAmt < lnTotDr
        *SCAN WHILE lnTotAmt <= lnTotDr
        SCAN WHILE lnTotAmt < lnTotDr
        *B603191,1 end
          lcKeyExp = Credit.Account+Credit.TranType+Credit.Tran
          = lfvSelect('C',.T.)
          lnTotAmt = lnTotAmt + ABS(Credit.AMOUNT)
        ENDSCAN
        IF lnTotAmt > lnTotDr
          =lfvAccNew(.T.)
          =lfAddOpen(lcKeyExp)
        ENDIF
      
      CASE lnTotCr < lnTotDr
        =lfvSelAll('C')
        SELECT Debit
        GO TOP
        *B603191,1 Scan while lnTotAmt < lnTotDr
        *SCAN WHILE lnTotAmt <= lnTotCr
        SCAN WHILE lnTotAmt < lnTotCr
        *B603191,1 end
          lcKeyExp = Debit.Account+Debit.TranType+Debit.Tran+Debit.cInstalNo
          = lfvSelect('D',.T.)
          lnTotAmt = lnTotAmt + Debit.AMOUNT
        ENDSCAN
        IF lnTotAmt > lnTotCr
          =lfvAccNew(.T.)
          =lfAddOpen(lcKeyExp)
        ENDIF

      CASE lnTotDr = lnTotCr
        =lfvSelAll('D')  
        =lfvSelAll('C')
    ENDCASE
  ENDIF
ENDIF
SELECT (lnCurAlias)

RETURN llRet

****************************************************************************

FUNCTION lfExterAuto
*E301509,1 (Start)
*AHM 11/16/2000 Add new parameter for EDI
*PARAMETERS lcProg,lcAccount,lcCurCode,ldKODate,lnExRate,lnCurrUnit
PARAMETERS lcProg,lcAccount,lcCurCode,ldKODate,lnExRate,lnCurrUnit,llFromEDI
*E301509,1 (End)

PRIVATE lcProg,lcAccount,lcCurCode,ldKODate,lnExRate,lnCurrUnit,llMltCur,;
        lcDCAdjTmp,lcDCOnATmp,lcDCAllTmp,lcRpComTmp,;
        lnPTrans,lnNTrans,lnBalance,lnAccEntrd,;
        lnAccDebit,lnAccCredt,lnAccDiffr,;
        lcAdTrnSeq,lcAcTrnSeq,lcHisSeq,lcGlSess,;
        llUpdMstGL,llSaveComp


STORE 0 TO lnPTrans,lnNTrans,lnBalance,lnAccEntrd,;
           lnAccDebit,lnAccCredt,lnAccDiffr,;
           lcAdTrnSeq,lcAcTrnSeq,lcHisSeq,lcGlSess

llSaveComp = .F.
llUpdMstGL = .T.

llNoThing = gfOpenFile(gcDataDir+"Credit","Credit",'SH')
llNoThing = gfOpenFile(gcDataDir+"Debit" ,"Debit" ,'SH')

llMltCur   = gfGetMemVar("llMulCurr")

lcDCAdjTmp = gfTempName()
lcDCOnATmp = gfTempName()
lcDCAllTmp = gfTempName()
lcRpComTmp = gfTempName()
*
*--- Create the program temp files.

*E301509,1 (Start)
*AHM 11/16/2000 Add new parameter for EDI
*llNoThing = lfCratTemp()
llNoThing = lfCratTemp(llFromEDI)
*E301509,1 (End)
*---------------------------------------------------------------------------
*
*--- Set the base filter.
llNoThing  = lfSetFlter("Debit" , .T., 2) 
llNoThing  = lfSetFlter("Credit", .F., 2)
*---------------------------------------------------------------------------

IF lfvAutoKey(.T.)
  llExtAuto = .T.
  DO lpSavScr
ENDIF

*---------------------------------------------------------------------------

*-- Reset the filter
llNoThing  = lfSetFlter("Debit" , .T., 1)
llNoThing  = lfSetFlter("Credit", .F., 1)

llNoThing = gfCloseFile("Credit")
llNoThing = gfCloseFile("Debit")

*-- Close and erase the temporary files
llNoThing =lfClsData()

IF lcProg = "CR" AND llSaveComp
  *     Automatic key-off completed.  
  lnNoThing  = gfModalGen('INM40162B00000')
ENDIF




*:----------------------------------------------------------------
*: Name       : lfvFactor
*: Developer  : Nader Anis Mitry - (NAD)
*: Date       : 07/20/2000
*: Purpose    : Valid function for the "Factor" field
*:              in the "Debit\Credit" folder.
*: Refrence   : B803369,1
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvFactor()
*:----------------------------------------------------------------
FUNCTION lfvFactor

*-- If the factor has been changed, or the factor browse
*-- is pressed.

IF llFrmBrow OR !(lcOldValue == lcFactor)
  
  *E301077,53 YMA 03/03/99 Start - Open the Factor file.
  = gfOpenFile(gcSysHome+"SYCFact","cFacCode",'SH')
  *E301077,53 YMA 03/03/99 End.
  
  *-- If the factor browse is pressed, or the factor code is
  *-- not valid.
  IF llFrmBrow OR (!EMPTY(lcFactor) AND !SEEK(lcFactor, "SYCFact"))
    llBrowse  = .T.
    
    *-- Browse the factors.
    lcFactor = lfBrFactor()
    IF EMPTY(lcFactor)
      lcFactor = lcOldValue
      _CUROBJ   = _CUROBJ
    ENDIF
  ENDIF
  
  *-- Update the new factor field in the file.
  llFrmBrow = .F.
  lnAlias   = SELECT(0)
  SELECT (lcDCAdjTmp)
  = RLOCK()
  REPLACE cFacCode WITH lcFactor
  UNLOCK
  *E500374,5  HBG  08/15/2000 Save the factore in All transaction temp file [Begin]
  SELECT (lcDCAllTmp)
  = RLOCK()
  REPLACE cFacCode WITH lcFactor
  UNLOCK
  *E500374,5 [End]
  SHOW WINDOW (lcDCAccTtl) REFRESH
  SELECT(lnAlias)
ENDIF

*:----------------------------------------------------------------
*: Name       : lfGeLnkCd
*: Developer  : Hend Ghanem (HBG)
*: Date       : 08/16/2000
*: Purpose    : If there is a factor get link code of it 
*: Refrence   : E500374,5
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfGeLnkCd()
*:----------------------------------------------------------------

FUNCTION lfGeLnkCd
PRIVATE lcLnkCod , lcCuralias

lcLnkCod = ""
lcCuralias = ALIAS()

IF !EMPTY(cFacCode)
  IF !llOpenFac
    llOpenFac = gfOpenFile(gcDataDir+'FACTOR',gcDataDir+'FACTOR','SH') 
  ENDIF
  IF SEEK(&lcCuralias..cFaccode,'FACTOR')
    lcLnkCod = FACTOR.Link_code 
  ENDIF
ELSE
  IF SEEK("M" + lcAcc, "Customer")
    lcLnkCod = Customer.Link_Code
  ENDIF  
ENDIF  
lcLnkCod = IIF(EMPTY(lcLnkCod), "DEFDEF", lcLnkCod)  

SELECT (lcCuralias)  

RETURN lcLnkCod
  
  
