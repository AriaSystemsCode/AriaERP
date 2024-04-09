************************************************************************
*:
*: Procedure file: APMNCHP.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Hisham Ramsis Philips
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: 
*:               : lfDispBrow  && Building the browse. 
*:               : lfwBrows    && Fill variable from record.  
*:               : lfvSelect   && Select or unselect certain invoice. 
*:               : lfvSelAll   && Select all invoices. 
*:               : lfvSelNon   && Unselect all selected invoices. 
*:               : lfvInvert   && Select unselected invoices and unselect selected invoices. 
*:               : lfvPySlInv  && Activate the Pay selected invoices screen.  
*:               : lfvPayNum   && Check if the payment no. exists and not duplicate. 
*:               : lfvChkNum   && Check if the chknum exists and not duplicate.
*:               : lfvProceed  && Pay selected approve payments. 
*:               : lfVSelCan   && get the old next check number if there is no checks added in between adding and cancelling the check no. 
*:               : lfwPayDat   && Save old payment date.
*:               : lfvPayDat   && Valdation of the invoce date.  
*:               : lfvScope    && set scope expersion filter.
*:               : lfvVendor   && Activate verndor window. 
*:               : lfvTot      && get total all selected approve amount,discount,adjustment,1099. *** 
*:               : lfvInvoice  && Activate invoice window. 
*:               : lfvClose    && pass true to global function quitting. 
*:               : lfPushKey   && trap keys 
*:               : lfwVend     && Save old vendor code.  
*:               : lfvVend     && Validation of vendors.   
*:               : lfShowDs    && Control push botton condition Dis/Enable. 
*:               : lfvDivision && Get division code.  
*:               : lfvparior   && parior not accept zero.
*:               : lfvMethod   && Activate pay method popup. 
*:               : lfvFrTodat  && validate from through date in scope window.
*:               : lfvDat      && Branch between due date and discount date in a scope wine. 
*:               : lfvOk       && fill the filter expresion when the <ok.> pressed in scope window. 
*:               : lfvCancel   && restore old values if push cancel from scope. 
*:               : lfwBank     && save the old bank 
*:               : lfvBank     && Validation of bank.
*:               : lfDefBank   && Get default bank and checks.  
*:               : lfwChecks   && Save old check. 
*:               : lfvChecks   && Validation of checks.   
*:               : lfvpbSel    && let Select prompt change according to its condition. 
*:               : lfDeAct     && Call trap function and release trap function. 
*:               : lfBrowTrap  && Define trap keys when browse window is actinve.
*:               : lpDoTrap    && Do each trap action.
*:               : lfBrowUnTrap&& Release the trap.
*:               : lfActBrows  && Activate the browse window. 
*:               : lfDefinePad && Define browse pad.
*:               : lpTrap      && Making program trap keys.
*:               : lfwCurr     && 
*:               : lfvCurr     && Currency validation.
*:
*:      Documented 00/00/1994
*:************************************************************************
*B600492,1 Reham 06/22/95 Change the fields width in the browse fields variable under windows.
*B600580,1 ORABY 07/26/95 The program try to lock the payment record mode than one time, so the
*B600580,1                system thinks that the user is editing the same record in another secssion
*B600581,1 ORABY 07/26/95 An object (Invisable button -  (ibNoAprPa)) was defined in windows screen but not in dos screen
*B600581,1 M.H   09/19/95 Variable 'ibNoAprPa' not found under DOS so I add the condition to to use this object under WINDOWS only.
*B800072,1 ORABY 08/15/95 The program still try to lock the payment record mode than one time, so the
*B800072,1                system thinks that the user is editing the same record in another secssion
*E300296,1 M.H   10/10/95 Add the currency to the AP module.
*E300316,1 HISH  11/30/95. Used global function gfGetExSin to get currency 
*E300316,1                 equation signs.
*E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin.
*E300296,1 M.H 02/04/96 Define a new variable to hold the symbol of the approved amount.
*B600970,1 M.H 02/06/96 Update the last check #,last payment date fields.
*B601013,1 RENEE 04/16/96. 1. Add the exchange rate to APYSLINV.SPR, hence, 
*B601013,1                    rearrange validation route, and allow editing rate
*B601013,1                     according to the company setting
*B601013,1                     Added functions lfvPayDate(), lfwOldVal(),
*B601013,1                     and lfvExRate(). Stopped using function
*B601013,1                     lfwPayDat()
*B601013,1                 2. If cash payment, fix the filter option as to 
*B601013,1                    filter on the approved currency instead of 
*B601013,1                    the invoice currency.
*B601112,1 M.H 06/20/96 Change the contents of lcTInvDate to 'Due Date' insted of 'Inv. Date'
*B601416,1 HS    01/06/97 Change the calling of gfGetExSin so if the Invoice 
*B601416,1                currency is the base currency I make it the To  
*B601416,1                currency and if not the Approve currency is 
*B601416,1                the To currency 
*B601526,1  HS  01/19/97  Add the new field [nInvFAAp] in the APINVHDR file 
*B601526,1                [the approved amount in the approve currency] 
*B601526,1                and (Replace the calculation using lcExSin3 and
*B601526,1                lcExSin4 with the new field ) 
*B601589,1  HS  01/27/97   Fill the field [APDIST.nApdLinNo] with 1 in the 
*B601589,1                 case of calculated adjustment to be able to know
*B601589,1                 the calculated adjustment from the approved one.
*B601599,1 HS    02/13/97 Fix the 1099 amount   
*B601217,1 HS    02/20/97 Unselect the the selected invoices if the Vendor
*B601217,1                code .or. the Bank code .or. the Checking account  
*B601217,1                was changed.
*B601553,1  HS   03/25/97 1)Check if the approved payment is not greater than
*B601553,1                zero and if so give the user a message and stop
*B601553,1                the calling of APYSLINV.SPR
*B601553,1                2)Unlock the APCHECKS record in the case of
*B601553,1                Manual check.
*B601610,1  HS  03/27/97  Add the new field nInvFAAp [Approved amount in 
*B601610,1                foreign currency] to the browse if the Company
*B601610,1                was using the multiy currency.
*B601536,1  HS   03/27/97 Change the titles of the summary screens
*E300643,1  HS 04/15/97 1)Make some changes for we have change the file
*E300643,1              SYCCODES name to CODES and make it a data file
*E300643,1              [Change its dir. from SYSFILES to DBFS]
*E300643,1              2)Make some changes for we have change the function
*E300643,1              [gfCodDes]
*E300663,1  HS 04/23/97 Change the calling of the function [gfSequence]
*E300663,1              for the changes we have made to that function.
*E300683,1 AHMED 06/04/97 Add screens directory path to the calling of SPRS
*B801208,4 AMM 08/12/97 Optimize Speed
*B601553,4  HS 04/06/98 Give the user the capability to make approved
*B601553,4              payment with negative amount
*B601957,1  HS 07/06/98 Add the paid 1099 amount, invoice discount
*B601957,1              and invoice adjustment in the distribution file
*B601957,1              in the debit memo case as well as the invoice
*B601957,1              case.
*E301077,79 AMM 03/04/99 Enhance opening and closing files.
*E300789,4  AMM 03/07/99 Remove field cComp_id from files Accod, FisHD, FsPrd, FsHld, Codes
*B603689,1 AMH (Ahmed Maher) Fix Exchange Rate update in ApPaymnt
*B603679,1 KHM 07/10/2000 Fix the bug of wrong replacement of cCurrCode,
*B603679,1                nExRate and nCurrUnit by using the old 
*B603679,1                replacement before bug#603689
*B604453,1 AAN 05/09/2001 Adding a warning message to be displayed if the payment date
*B604453,1                is prior to the Posting date.
*B604733,1 SSE 09/18/2001 Fix bug of displaying wrong currency conversion.
*B129142,1 EIH 08/30/2005 Fix bug of saving wrong exchange rate when we edit it.

*B131145,1 EIH 03/20/2006 Fix bug of saving in wrong record of APVENHST file when we change 
*B131145,1 EIH 03/20/2006 The payment date to the next year.
*B132275,1 TMI 06/22/2006 Fix the bug that voiding payment while Ap Invoice screen is opened in edit mode
*B608901,1 WAM 06/22/2009 Don't change selection flag when move the vertical scroll bar [T20090513.0002]
*B608942,1 TMI 07/22/2009 if a record is used by a user check that this user is in SYUSTATC of both A27 and A4XP [T20090224.0001]
*:************************************************************************ 
* Manual checks, Non check payments, Cash payments. **
*
PARAMETER lnPyChMN         && Refere where the program is called from 
PRIVATE laBankObjs 
DECLARE laPayMeth[1,2], laSelect[1,2], laWndObj[4,3], laBankObjs[2,3]     

** lcVenSum     : Vendor summary window name
** lcInvSum     : Invoice summary window name
** lcSelect     : Variable to hold sign if the record selected.
** lcDivDisc    : Variable to hold division description in browse.
** lcFuncName   : Variable to hold Refresh window. 
** lcBrowName   : Variable to hold Brows Window name. 
** lcReadNam1   : Variable to hold Read Window name.
** lcReadNam2   : Variable to hold Read Window name.
** lcPayNum     : Variable to hold Payment number.
** lcExprsion   : Variable to hold filter expresion.
** lcOldExpr    : Variable to hold old filter expresion.
** lcPeriod     : Variable to hold First period. 
** lcFisPrd     : Variable to hold First period.
** lcYear       : Variable to hold first year.
** lcFisYer     : Variable to hold first year.
** lcTAprAmnt   : The Approve amount in approve message.
** lcVendCode   : Variable to hold vendor code.
** lcOldVndcd   : Variable to hold old vendor code.
** lcVendComp   : Variable to hold vendor company.
** lcVPhone     : Variable to hold vendor phone.
** lcDivision   : Variable to hold division description.
** lcCInvRef    : Variable to hold current Inv. Ref.
** lcPriority   : Variable to hold Paroirity.
** lcPaymeth    : Variable to hold pay method.
** lcOVendCod   : Variable to hold old vendor code.
** lcOldVnCmp   : Variable to hold old vendor company.
** lcOldDivDs   : Variable to hold old division description.
** lcOldDvCod   : Variable to hold old division code.
** lcOldPrty    : Variable to hold old Paroirity.
** lcOldpymth   : Variable to hold old pay method.
** lcOMethod    : Variable to hold old method.
** lcTThrou     : Variable to hold word through.
** lcTFrom      : Variable to hold word From.
** lcTBlnkCode  : Variable to hold text not applicable. 
** lcTInvNo     : Variable to hold text 
** lcTPrior     : Variable to hold text
** lcTPayMeth   : Variable to hold text
** lcTAprPay    : Variable to hold text
** lcTAprDisc   : Variable to hold text
** lcTAprAdj    : Variable to hold text
** lcTInvDate   : Variable to hold text
** lcTDivDes    : Variable to hold text
** lcTOpnAmnt   : Variable to hold text
** lcTCurrCode  : Variable to hold text
** lcTCurRate   : Variable to hold text
** lcSesNum     : Variable to hold session number.
** lcCurYear    : Variable to hold current year.
** lcBrowStr    : Variable to hold browse string.
** lcOldVal     : Variable to hold old vendor code, company and phone.
** lcObjectSt   : Variable to hold object status enable or disable.
** lcObjDisp    : Variable to hold object display status.
** lcInvoice    : Variable to hold invoice window title.
** lcBrTtl      : Variable to hold Browse Window Title.
** lcBankCode   : Variable to hold bank code.
** lcOldBank    : Variable to hold old bank code.
** lcCheckcode  : Variable to hold check code. 
** lcOldCheck   : Variable to hold old check code. 
** lcInvRef     : Variable to hold Inv. Ref.
** lcOldInvRf   : Variable to hold old Inv. Ref.
** lcEscTrp     : Variable to hold escape trap.
** lcColPair    : Variable to hold 
** lcColor      : Variable to hold 
** lcVendTtl    : Variable to hold vendor window title.
** lcDivCode    : Variable to hold division code.
** lcTAll       : Variable to hold text all.
** lcCodeFilt   : Variable to hold code filter.
** lcBrowFile   : Vairable to hold the Browse File.
** lcCurrCode   : Variable to hold the currency code.
** lcOldCurr    : Variable to hold the old currency.
*E300316,1 HISH  11/30/95.  Added currency equation sign variables. (Begin)
** in case of exchange currency from invoice currency to company base currency.
** lcExSin1             Variable to hold the first sign in the equation.
** lcExSin2             Variable to hold the second sign in the equation.
** in case of exchange currency from invoice currency to approved currency.
** lcExSin3             Variable to hold the first sign in the equation.
** lcExSin4             Variable to hold the second sign in the equation.
** in case of exchange currency from approved currency to company base currency.
** lcExSin5             Variable to hold the first sign in the equation.
** lcExSin7             Variable to hold the second sign in the equation.
*E300316,1 (End)
*E300296,1 M.H 02/04/96 Define a new variable to hold the symbol of the approved amount.
** lcCurrSmbl   : Variable to hold the approved currency symbol.
** lcBaseSmbl   : Variable to hold the base currency symbol.
*E300296,1 End.

** lnOScope     : Variable to hold old rb Scope.
** lnArlen      : Variable to hold Select array length.
** lnkeyno      : Variable to Select order in vendor browse.
** lnDivDes     : Variable to hold pu Division description.
** lnMethod     : Variable to hold ib Method of payment.
** lnVnTotPay   : Variable to hold vendor history tot pay.
** lnTotPay     : Variable to hold Total Payment.
** lnTotDisc    : Variable to hold Total Discount.
** lnTotAdj     : Variable to hold Total Adjustment.
** lnTot1099    : Variable to hold Total 1099.
** lnAprToPay   : Variable to hold approve to pay amount.
** lnAprDisc    : Variable to hold approve discount.
** lnAprAdj     : Variable to hold approve adjustment.
** lnOldAprTPy  : Variable to hold old approve to pay amount.
** lnOldAprDisc : Variable to hold old approve discount.
** lnOldAprAdj  : Variable to hold old approve adjustment.
** lnBrRecNo    : Browse record number 
** lnChkNum     : Variable to hold Check number.
** lnGnChkNm    : Variable to hold old check number.
** lnOChkNum    : Variable to hold old check number. 
** lnTime       : Variable to hold time to consider duble click.

** puDiv        : Variable to hold pop up division.
** puDivDes     : Variable to hold pu Division Description.
** puDivision   : Variable to hold pop division.

** ldOldPdat    : Variable to hold old payment date. 
** ldFrDueDat   : Variable to hold from due date.
** ldToDueDat   : Variable to hold to due date.
** ldFrDisDat   : Variable to hold from discount date.
** ldToDisDat   : Variable to hold to discount date. 
** ldOFrDueDt   : Variable to hold old from due date.
** ldOToDueDt   : Variable to hold old to due date.
** ldOFrDisDt   : Variable to hold old from discount date.
** ldOToDisDt   : Variable to hold old to discount date. 
** ldPayDat     : Variable to hold Payment Date.

** llBrowse     : Variable to hold flag in case of activate browse by mouse.
** llAprPart    : Variable to hold parameter to branch between appr. fully and appr. part.
** llScope      : Variable to hold if you want scope to be done.
** llVendExst   : If Vendor object is not exist in scope screen.
** llDisBro     : If refresh window in display function.
** llChkCan     : Variable to hold
** llFromTrap   : Variable to hold if call model screen from browse trap.
** llDispbr     : Flag to show browse refresh if vendor or bank or checks changed only.
** llNoContrl   : Variable to hold there is not Control panel.
** llproceed    : Variable to hold flag to proceed.

** rbScope      : Variable to hold rb Scope.
** rbDuDsDt     : Variable to hold rb due date and discount date.
** rbODuDsDt    : Variable to hold rb due date and discount date.
** rbScopeOn    : Variable to hold rb Scope On in rb Scope.
** ibmethod     : Variable to hold ib Method of payment.

** laCtrStat    : To disable the browse pad in the menu

STORE .T.    TO llNoContrl , llproceed
STORE .F.    TO llBrowse   , llAprPart   , llScope    , llVendExst ,;
                llDisBro   , llChkCan    , llFromTrap , llDispbr    
STORE 0      TO lnBrRecNo  , lnChkNum    , lnGnChkNm  , lnOChkNum   
STORE 0.00   TO lnVnTotPay , lnTotPay    , lnTotDisc  , lnTotAdj   ,;
                lnTot1099  , lnAprToPay  , lnAprDisc  , lnAprAdj   ,;
                lnOldAprTPy, lnOldAprDisc, lnOldAprAdj 
STORE 1      TO rbScope    , rbDuDsDt    , rbODuDsDt  , puDivDes   ,;
                ibmethod   , lnOScope    , lnArlen    , lnkeyno    ,;
                lnDivDes   , lnMethod
STORE 3      TO rbScopeOn

STORE gdSysDate TO ldPayDat    
STORE {}     TO ldOldPdat  , ldFrDueDat  , ldToDueDat , ldFrDisDat ,;
                ldToDisDat , ldOFrDueDt  , ldOToDueDt , ldOFrDisDt ,;
                ldOToDisDt

STORE " "    TO lcVenSum   , lcInvSum    , lcSelect   , lcDivDisc  ,;
                lcFuncName , lcBrowName  , lcReadNam1 , lcReadNam2 ,;
                lcPayNum   , lcExprsion  , lcOldExpr  , lcPeriod   ,;
                lcFisPrd   , lcYear      , lcFisYer   , lcTAprAmnt ,;
                lcVendCode , lcOldVndcd  , lcVendComp , lcVPhone   ,;
                lcDivision , lcCInvRef   , lcPriority , lcPaymeth  ,;
                lcOVendCod , lcOldVnCmp  , lcOldDivDs , lcOldDvCod ,;
                lcOldPrty  , lcOldpymth  , lcOMethod  , lcTThrou   ,;
                lcTFrom    , lcTBlnkCode , lcTInvNo   , lcTPrior   ,;
                lcTPayMeth , lcTAprPay   , lcTAprDisc , lcTAprAdj  ,;
                lcTInvDate , lcTDivDes   , lcTOpnAmnt , lcSesNum   ,;
                lcCurYear  , lcBrowStr   , lcOldVal   , lcObjectSt ,;
                lcObjDisp  , puDiv       , puDivision , lcMarker   ,;
                lcTCurrCode, lcTCurRate  , lcCurrCode , lcOldCurr  ,;
                lcExSin1   , lcExSin2    , lcExSin3   , lcExSin4   ,;
                lcExSin5   , lcExSin6    , lcCurrSmbl , lcBaseSmbl


*B601217,1 Add This line to add a new varibles lcVendrCod , lcCurCod [Begin]
STORE " "    TO lcVendrCod , lcCurCod
*MAN
lcCurrCode = gcBaseCurr
*B601217,1 Add This line to add a new varibles [End]

STORE _DBLCLICK+SECONDS() TO lnTime

STORE "*"          TO lcDivCode
STORE "All"        TO lcTAll            

*B601536,1 Change this line to change the title [Begin]
*STORE "Vendors"    TO lcVendTtl
STORE "Vendor"    TO lcVendTtl
*B601536,1 Change this line to change the title [End]

STORE "DISABLE"    TO laCtrStat   
STORE "APINVHDR"   TO lcBrowFile
STORE "CDIVISION " TO lcCodeFilt
STORE SCHEME(1,2)  TO lcColPair,lcColor
STORE ""           TO lcEscTrp    
STORE SPACE(8)     TO lcBankCode,lcOldBank
STORE SPACE(12)    TO lcCheckcode,lcOldCheck
STORE SPACE(15)    TO lcInvRef,lcOldInvRf

*B601536,1 Change this line to change the title [Begin]
*STORE "Invoices"   TO lcInvoice,lcBrTtl
STORE "Invoices"   TO lcBrTtl
STORE "Invoice"   TO lcInvoice
*B601536,1 Change this line to change the title [End]

*B601217,1 Add this lines to add a new varibles lcBnkCod , lcChekCod [Begin]
STORE SPACE(8)     TO lcBnkCod
STORE SPACE(12)    TO lcChekCod
*B601217,1 Add this line to add a new varible lcBnkCod [End]

lcHidObjDlg = IIF(_WINDOWS,',,,,,,,,,RGB(192,192,192,192,192,192)',lcHidObjDlg)  
lcgry1      = IIF(rbDuDsDt=2,"RGB(192,192,192,192,192,192),,,,,,,,,",;
                  "RGB(128,128,128,128,128,128),,,,,,,,,")
lcwht1      = IIF(rbDuDsDt=2,"RGB(192,192,192,192,192,192),,,,,,,,,",;
                  "RGB(255,255,255,255,255,255),,,,,,,,,")                
lcgry2      = IIF(rbDuDsDt=1,"RGB(192,192,192,192,192,192),,,,,,,,,",;
                  "RGB(128,128,128,128,128,128),,,,,,,,,")
lcwht2      = IIF(rbDuDsDt=1,"RGB(192,192,192,192,192,192),,,,,,,,,",;
                  "RGB(255,255,255,255,255,255),,,,,,,,,")                

*B601013,1 Get multi currency settings
lnOldVal = 0
IF gfGetMemVar('LLMulCurr')
  llMultiCr   = .T.
  llEditEx    = gfGetMemVar('LLEDITEXRA')
  llAddRate   = gfGetMemVar('LLEXCHRATE')
ELSE
  STORE .F. TO llEditEx, llMultiCr, llAddRate
ENDIF  
*B601013,1 end.

IF !gfSetup()
  RETURN
ENDIF

*** Array laFields is used by lfSekVnd() and formed as follows :
*** A row for every field to be browsed
*** Column 1 : the object name corresponding to the field
*** Column 2 : the physical field name
*** Column 3 : the tag to be used in ssearching
DECLARE laFields[3,3], laInvADt[1]
laFields[1,1] = 'lcVendCode'         &&'lcVendCode'
laFields[1,2] = 'cVendCode'
laFields[1,3] = 'VENCODE'
laFields[2,1] = 'lcVendComp'         && 'lcVenComp'
laFields[2,2] = 'cVenComp'
laFields[2,3] = 'VENCOMP'
laFields[3,1] = 'lcVPhone'           &&'lcPhoneNo'
laFields[3,2] = 'cPhoneNo'
laFields[3,3] = 'VENPHONE'

*** Get current year form parent company. 
*E301077,79 AMM Open file.
*SELECT SYCCOMP
*lcSavOrd = ORDER() 
*SET ORDER TO CCOMP_ID
*MAN Added PRIVATE
PRIVATE llOpComp
llOpComp = gfSysOpen(gcSysHome+'SYCCOMP','CCOMP_ID','SH')
*E301077,79 AMM end

IF SEEK(gcPrnt_Cmp)
   lcCurYear=CCURR_YER
ENDIF 
*E301077,79 AMM Close file
*IF !EMPTY(lcSavOrd)
*  SET ORDER TO &lcSavOrd
*ENDIF  
IF llOpComp
  *MAN Used gfSysClose
  =gfSysClose('SYCCOMP')
  *=gfSysClose('SYCCOMP')
ENDIF
*E301077,79 AMM end

*** Get payment methods array
lnPayMLen = gfGetVld('CVENPMETH',@laPayMeth,.T.)

lnAryPos = ASCAN(laPayMeth,'C',1)
IF lnAryPos > 0	
  =ADEL(laPayMeth,ASUBSCRIPT(laPayMeth, lnAryPos, 1)) 
  DIMENSION laPayMeth[ALEN(laPayMeth,1)-1,2]  
ENDIF

IF lnPyChMN <> 3 && If the program not called from cash payments bar
  lnAryPos = ASCAN(laPayMeth,'H',1)
  IF lnAryPos > 0	
    =ADEL(laPayMeth,ASUBSCRIPT(laPayMeth, lnAryPos, 1)) 
    DIMENSION laPayMeth[ALEN(laPayMeth,1)-1,2]  
  ENDIF
ENDIF

lcPayMeth = laPayMeth[1,1]  && Variable to hold method.
lcMethod  = laPayMeth[1,2]  && Variable to hold method code.

IF _WINDOWS

  *E300643,1 Change this lines for the changes we have made 
  *          to SYCCODES [Begin]
  *puDiv = SYCCODES.cdiscrep
  *DEFINE POPUP puDivision prompt field SYCCODES.cdiscrep scroll;
  *FROM 9.00,12.88 TO 13.75,43.15;
  *MESSAGE gfObj_msg()

  puDiv = CODES.cdiscrep
  DEFINE POPUP puDivision prompt field CODES.cdiscrep scroll;
  FROM 9.00,12.88 TO 13.75,43.15;
  MESSAGE gfObj_msg()

  *E300643,1 Change this lines [End]
  
  ON SELECTION POPUP puDivision DO lfvDivision
ENDIF

*** Get names to Brawse and Read window ***
IF !WEXIST(gcBaseWind)
 
  lcObjectSt  = 'DISABLE'

  *E300643,1 Change this line for the changes we have made 
  *          to (gfCodDes) [Begin]
  *lcTBlnkCode = gfCodDes(' ')
  lcTBlnkCode = gfCodDes(' ' , ' ')
  *E300643,1 Change this line [End]
  
  *** Get the active session number. *** 

  *E300663,1 Change this line for the changes we have 
  *          made to (gfSequence) [Begin]
  *lcSesNum=PADL(gfSequence("APSESS",1,gcAct_Comp),8,'0')
  lcSesNum = gfSequence('CAPSESSNO')
  *E300663,1 Change this line [End]
  
  *** Variable holding Filter expresion ***
  lcExprsion = "(NINVAMNT-NINVPAID-NINVDISTK-NINVADJ<> 0)"
  IF lnPyChMN = 3 && If the program called from cash payments bar
    *E300296,1 M.H   10/10/95 Add the currency to the AP module.
    *E300296,1 M.H Add the vendor to the currency to the filter exp.
    *lcExprsion = lcExprsion + ".AND. APINVHDR.CVENPMETH = 'H'"
    
    *B601013,1 Change filter expression as to filter on approved currency.
    *lcExprsion = lcExprsion + ".AND. APINVHDR.CVENPMETH = 'H' .AND. APINVHDR.CCURRCODE = lcCurrCode"

    *B601217,1 Change this line to use the new varible lcCurCod [Begin]
    *lcExprsion = lcExprsion + ".AND. APINVHDR.CVENPMETH = 'H' .AND. APINVHDR.cAprCurCod = lcCurrCode"
    lcExprsion = lcExprsion + ".AND. APINVHDR.CVENPMETH = 'H' .AND. APINVHDR.cAprCurCod = lcCurCod"
    *B601217,1 Change this line to use the new varible [End] 

    *B601013,1 end.
  ELSE

    *B601217,1 Change this line to use the new varibles lcBnkCod 
    *           , lcChekCod [Begin]
    *lcExprsion = lcExprsion+".AND. CVENPMETH <> 'H' .AND." +;
    *             "CBNKCODE+CCHKACCT=lcBankCode+lcCheckcode"

    lcExprsion = lcExprsion+".AND. CVENPMETH <> 'H' .AND." +;
                 "CBNKCODE+CCHKACCT=lcBnkCod+lcChekCod"

    *B601217,1 Change this line to use the new varibles [End]

    *** Prepare an array to hold bank objects to be used for
    *** global bank and checking accounts validations as follows :
    *** One row for every object, such that 
    *** row no. 1 holds bank object names,
    *** row no. 2 holds checking account object names
    *** row no. 3 holds the corresponding G/L account object names,(optional)
    *** Columns are ordered as follows :
    *** Column no. 1 : invisible button name for corresponding object
    *** Column no. 2 : object name (e.g. bank object name)
    *** Column no. 3 : object description name(if required)
    laBankObjs      = ' '
    laBankObjs[1,1] = 'ibBank'         && Bank code invisible button
    laBankObjs[1,2] = 'lcBankCode'     && Bank code 
    laBankObjs[2,1] = 'ibChecks'       && Checking account invisible button
    laBankObjs[2,2] = 'lcCheckCode'    && Checking account 
   
  ENDIF
  SELECT APINVHDR
  SET FILTER TO CVENDCODE + CINVNO = ' *' && Filter return no record.
  lcBrowName = gfTempName()   
  lcReadNam1 = gfTempName()
  lcReadNam2 = gfTempName()
  lcVenSum   = gfTempName()
  lcVenSum   ="CWR"+SUBSTR(lcVenSum,4)
  lcInvSum   = gfTempName()
  lcInvSum   ="CWR"+SUBSTR(lcInvSum,4)

ELSE && If window exist
  SELECT APINVHDR
  lcSavOrd = ORDER() 
  SET ORDER TO 0

  *B601217,1 Change this line to use the new varible lcVendrCod [Begin]
  *SET FILTER TO APINVHDR.CVENDCODE+APINVHDR.CINVNO = lcVendCode .AND.;
  *              (ABS(NINVAMTAP)+ABS(NINVDISAP)+ABS(NINVADJAP)>0).AND.;
  *              CVENPRIOR<> '0' .AND. CINVSTAT<> 'V'.AND.;
  *              EVALUATE(lcExprsion)

  SET FILTER TO APINVHDR.CVENDCODE+APINVHDR.CINVNO = lcVendrCod  .AND.;
                (ABS(NINVAMTAP)+ABS(NINVDISAP)+ABS(NINVADJAP)>0).AND.;
                CVENPRIOR<> '0' .AND. CINVSTAT<> 'V'.AND.;
                EVALUATE(lcExprsion)

  *B601217,1 Change this line to use the new varible lcVendrCod [End]

  SET ORDER TO &lcSavOrd

  IF _DOS && Return the popup values
    lcPayMeth = lcOldpymth  
    lcDivision = lcOldDivDs
  ENDIF  
  lcMethod   = lcOMethod
  rbScope    = lnOScope
  IF rbScope = 1
    ***Exprsion in apinvhdr file to be filtered.***
    lcExprsion = "(NINVAMNT-NINVPAID-NINVDISTK-NINVADJ<> 0)"
    IF lnPyChMN = 3 && If the program called from cash payments bar
      *E300296,1 M.H   10/10/95 Add the currency to the AP module.
      *E300296,1 M.H Add the vendor to the currency to the filter exp.
      *lcExprsion = lcExprsion + ".AND. APINVHDR.CVENPMETH = 'H'"
      *B601013,1 Change filter expression as to filter on approved currency.
      *lcExprsion = lcExprsion + ".AND. APINVHDR.CVENPMETH = 'H' .AND. APINVHDR.CCURRCODE = lcCurrCode"

      *B601217,1 Change this line to use the new varible lcCurCod [Begin]
      *lcExprsion = lcExprsion + ".AND. APINVHDR.CVENPMETH = 'H' .AND. APINVHDR.cAprCurCod = lcCurrCode"
      lcExprsion = lcExprsion + ".AND. APINVHDR.CVENPMETH = 'H' .AND. APINVHDR.cAprCurCod = lcCurCod"
      *B601217,1 Change this line to use the new varible lcCurCod [End]

      *B601013,1 end.
    ELSE

      *B601217,1 Change this line to use the new varibles lcBnkCod
      *           , lcChekCod [Begin]
      *lcExprsion = lcExprsion + ".AND. CVENPMETH <> 'H' .AND." +;
      *             "CBNKCODE+CCHKACCT=lcBankCode+lcCheckcode"

      lcExprsion = lcExprsion + ".AND. CVENPMETH <> 'H' .AND." +;
                   "CBNKCODE+CCHKACCT=lcBnkCod+lcChekCod"

      *B601217,1 Change this line to use the new varibles [Begin]

    ENDIF             
  ELSE
    =lfvOk() && returning the existing browse filter.
  ENDIF    
ENDIF

*** fill the array holding frist and last object in base window and each children
laWndObj [1,1] = lcReadNam1
laWndObj [1,2] = "ibVend"
laWndObj [1,3] = "pbClose"

laWndObj [2,1] = lcReadNam2
laWndObj [2,2] = "ibVend"
laWndObj [2,3] = "pbClose"

laWndObj [3,1] = lcVenSum                      
laWndObj [3,2] = "PBVECLOSE"
laWndObj [3,3] = "PBVECLOSE"

laWndObj [4,1] = lcInvSum
laWndObj [4,2] = "PBINCLOSE"
laWndObj [4,3] = "PBINCLOSE"

*** store scape trap.
lcEscTrp=ON("KEY","ESC")

*** Trab some keys before browse. ***
=lfPushKey()

*** Get Codes from codes file.

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

*E300789,4  AMM Adjust filter to fit the new index
*SET FILTER TO (CCOMP_ID+CRLTFIELD+CFLD_NAME = gcAct_Comp+'N'+lcCodeFilt) OR;
              (CCOMP_ID+CRLTFIELD+CFLD_NAME ='  '+'N'+'N/A') OR;
              (CCOMP_ID+CRLTFIELD+CFLD_NAME ='  '+'N'+'ALL')
SET FILTER TO (CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N'+lcCodeFilt) OR;
              (CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N'+'N/A') OR;
              (CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N'+'ALL')
*E300789,4  AMM end


*** Set relation between invoice header, vendor, and division file ***
SELECT APVENDOR
SET ORDER TO TAG VENCODE

SELECT APDIV
SET ORDER TO TAG DIVISION

SELECT APVENHST
SET ORDER TO TAG VENDYEAR

SELECT APINVHDR

SET RELATION TO APINVHDR.CVENDCODE INTO APVENDOR ADDITIVE
SET RELATION TO APINVHDR.CDIVISION INTO APDIV ADDITIVE
SET RELATION TO APINVHDR.CVENDCODE+ APINVHDR.CFISFYEAR INTO APVENHST ADDITIVE

*E300683,1 Call *.SPR from screens directory
* DO APMNCHP.SPR 
DO (gcScrDir + gcWinAppl + '\APMNCHP.SPR')
*E300683,1 end          

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

SET FILTER TO 

SELECT APINVHDR
*** Clear relation ***
SET FILTER TO
SET RELATION TO

*** restores on key labels that were placed on the stack with push key. ***
POP KEY

RELEASE PAD _BROWSE OF _MSYSMENU
***Release the browse window.
RELEASE WINDOW (lcBrTtl)

IF glQuitting
  HIDE WINDOW(gcBaseWind)
  RELEASE WINDOW (lcVenSum)
  RELEASE WINDOW (lcInvSum)
  RELEASE POPUPS puDivision
ENDIF

glFromBrow  = .F.

*!**************************************************************************
*!
*!      Function: lfDispBrow
*!
*!**************************************************************************
* Build browse.
FUNCTION lfDispBrow

lnBrRecNo  = RECNO('APINVHDR')

IF _DOS
  IF gfGetMemVar('LLMULCURR')

    *B601610,1 Change this line to add the new field nInvFAAp to the 
    *          browse [Begin]
    *lcBrowStr="lcMarker=IIF(RECNO()=lnBrRecNo,'',' '):1:H=' ':W=.F.,"+;
    *          "lcSelect=IIF(ASCAN(laSelect,APINVHDR.CINVNO)<> 0 , 'û', ' '):R :H='Inc',"+;
    *          "CINVNO:R :H=lcTInvNo,"+;
    *          "CVENPRIOR:H=lcTPrior,"+;
    *          "PayMeth=lfGetPayMeth(CVENPMETH):H=lcTPayMeth:18,"+;
    *          "NINVAMTAP:H=lcTAprPay,"+;
    *          "NINVDISAP:H=lcTAprDisc,"+;
    *          "NINVADJAP:H=lcTAprAdj,"+;
    *          "NINVAMNT=(NINVAMNT - NINVPAID - NINVDISTK - NINVADJ):R :H=lcTOpnAmnt,"+;
    *          "DINVDUDAT:R :H=lcTInvDate,"+;
    *          "CCURRCODE:H=lcTCurrCode,"+;
    *          "NEXRATE:H=lcTCurRate,"+;
    *          "lcDivDisc=IIF(EMPTY(cDivision),lcTBlnkCode,LOOKUP(SYCCODES.cDiscrep,gcAct_Comp+APINVHDR.CDIVISION,SYCCODES.cCode_No,'CODES')):R :10:H=lcTDivDes"

    lcBrowStr="lcMarker=IIF(RECNO()=lnBrRecNo,'',' '):1:H=' ':W=.F.,"+;
              "lcSelect=IIF(ASCAN(laSelect,APINVHDR.CINVNO)<> 0 , 'û', ' '):R :H='Inc',"+;
              "CINVNO:R :H=lcTInvNo,"+;
              "CVENPRIOR:H=lcTPrior,"+;
              "PayMeth=lfGetPayMeth(CVENPMETH):H=lcTPayMeth:18,"+;
              "NINVAMTAP:H=lcTAprPay,"+;
              "nInvFAAp :H = 'Apr foreign amt',"+;
              "NINVDISAP:H=lcTAprDisc,"+;
              "NINVADJAP:H=lcTAprAdj,"+;
              "NINVAMNT=(NINVAMNT - NINVPAID - NINVDISTK - NINVADJ):R :H=lcTOpnAmnt,"+;
              "DINVDUDAT:R :H=lcTInvDate,"+;
              "CCURRCODE:H=lcTCurrCode,"+;
              "NEXRATE:H=lcTCurRate,"+;
              "lcDivDisc=IIF(EMPTY(cDivision),lcTBlnkCode,LOOKUP(SYCCODES.cDiscrep,gcAct_Comp+APINVHDR.CDIVISION,SYCCODES.cCode_No,'CODES')):R :10:H=lcTDivDes"

    *B601610,1 Change this line [End]

  ELSE

    lcBrowStr="lcMarker=IIF(RECNO()=lnBrRecNo,'',' '):1:H=' ':W=.F.,"+;
              "lcSelect=IIF(ASCAN(laSelect,APINVHDR.CINVNO)<> 0 , 'û', ' '):R :H='Inc',"+;
              "CINVNO:R :H=lcTInvNo,"+;
              "CVENPRIOR:H=lcTPrior,"+;
              "PayMeth=lfGetPayMeth(CVENPMETH):H=lcTPayMeth:18,"+;
              "NINVAMTAP:H=lcTAprPay,"+;
              "NINVDISAP:H=lcTAprDisc,"+;
              "NINVADJAP:H=lcTAprAdj,"+;
              "NINVAMNT=(NINVAMNT - NINVPAID - NINVDISTK - NINVADJ):R :H=lcTOpnAmnt,"+;
              "DINVDUDAT:R :H=lcTInvDate,"+;
              "lcDivDisc=IIF(EMPTY(cDivision),lcTBlnkCode,LOOKUP(SYCCODES.cDiscrep,gcAct_Comp+APINVHDR.CDIVISION,SYCCODES.cCode_No,'CODES')):R :10:H=lcTDivDes"
  ENDIF  

  *B801208,4 AMM start add rest
  BROWSE REST FIELDS &lcBrowStr;
         WINDOW (lcBrowName);
         WHEN lfwBrows() .AND. lfvpbSel();
         VALID :F lfvBrowse();
         IN WINDOW (gcBaseWind) ;
         LOCK 0;
         NOAPPEND;
         NOCLEAR;
         NODELETE;
         NOWAIT;
         NOEDIT;
         SAVE;
         COLOR SCHEME 13;
         TITLE lcBrTtl
  *B801208,4 AMM end
ELSE
  IF gfGetMemVar('LLMULCURR')
    *B600492,1 Change the fields width under windows in the browse.

    *B601610,1 Change this line to add the new field nInvFAAp to the 
    *          browse [Begin]
    *lcBrowStr="lcMarker=IIF(RECNO()=lnBrRecNo,'',' '):1:H=' ':W=.F.,"+;
    *          "lcSelect=IIF(ASCAN(laSelect,APINVHDR.CINVNO)<> 0 , 'û', ' '):R :H='û',"+;
    *          "CINVNO:12:R :H=lcTInvNo,"+;
    *          "CVENPRIOR:H=lcTPrior,"+;
    *          "PayMeth=lfGetPayMeth(CVENPMETH):H=lcTPayMeth:1,"+;
    *          "NINVAMNT=(NINVAMNT - NINVPAID - NINVDISTK - NINVADJ):15:R :H=lcTOpnAmnt,"+;
    *          "NINVAMTAP:15:H=lcTAprPay,"+;
    *          "NINVDISAP:15:H=lcTAprDisc,"+;
    *          "NINVADJAP:15:H=lcTAprAdj,"+;
    *          "DINVDUDAT:R :H=lcTInvDate,"+;
    *          "CCURRCODE:H=lcTCurrCode,"+;
    *          "NEXRATE:H=lcTCurRate,"+;
    *          "lcDivDisc=IIF(EMPTY(cDivision),lcTBlnkCode,LOOKUP(SYCCODES.cDiscrep,gcAct_Comp+APINVHDR.CDIVISION,SYCCODES.cCode_No,'CODES')):R :10:H=lcTDivDes"

    *E300643,1 Change this line for the changes we have made 
    *          to SYCCODES [Begin]
    *lcBrowStr="lcMarker=IIF(RECNO()=lnBrRecNo,'',' '):1:H=' ':W=.F.,"+;
    *          "lcSelect=IIF(ASCAN(laSelect,APINVHDR.CINVNO)<> 0 , 'û', ' '):R :H='û',"+;
    *          "CINVNO:12:R :H=lcTInvNo,"+;
    *          "CVENPRIOR:H=lcTPrior,"+;
    *          "PayMeth=lfGetPayMeth(CVENPMETH):H=lcTPayMeth:1,"+;
    *          "NINVAMNT=(NINVAMNT - NINVPAID - NINVDISTK - NINVADJ):15:R :H=lcTOpnAmnt,"+;
    *          "NINVAMTAP:15:H=lcTAprPay,"+;
    *          "nInvFAAp :15:H = 'Apr foreign amt',"+;
    *          "NINVDISAP:15:H=lcTAprDisc,"+;
    *          "NINVADJAP:15:H=lcTAprAdj,"+;
    *          "DINVDUDAT:R :H=lcTInvDate,"+;
    *          "CCURRCODE:H=lcTCurrCode,"+;
    *          "NEXRATE:H=lcTCurRate,"+;
    *          "lcDivDisc=IIF(EMPTY(cDivision),lcTBlnkCode,LOOKUP(SYCCODES.cDiscrep,gcAct_Comp+APINVHDR.CDIVISION,SYCCODES.cCode_No,'CODES')):R :10:H=lcTDivDes"

    *E300789,4  AMM Adjust the Lookup in the codes file to adjust the new index
    *lcBrowStr="lcMarker=IIF(RECNO()=lnBrRecNo,'',' '):1:H=' ':W=.F.,"+;
              "lcSelect=IIF(ASCAN(laSelect,APINVHDR.CINVNO)<> 0 , 'û', ' '):R :H='û',"+;
              "CINVNO:12:R :H=lcTInvNo,"+;
              "CVENPRIOR:H=lcTPrior,"+;
              "PayMeth=lfGetPayMeth(CVENPMETH):H=lcTPayMeth:1,"+;
              "NINVAMNT=(NINVAMNT - NINVPAID - NINVDISTK - NINVADJ):15:R :H=lcTOpnAmnt,"+;
              "NINVAMTAP:15:H=lcTAprPay,"+;
              "nInvFAAp :15:H = 'Apr foreign amt',"+;
              "NINVDISAP:15:H=lcTAprDisc,"+;
              "NINVADJAP:15:H=lcTAprAdj,"+;
              "DINVDUDAT:R :H=lcTInvDate,"+;
              "CCURRCODE:H=lcTCurrCode,"+;
              "NEXRATE:H=lcTCurRate,"+;
              "lcDivDisc=IIF(EMPTY(cDivision),lcTBlnkCode,LOOKUP(CODES.cDiscrep,gcAct_Comp+APINVHDR.CDIVISION+'N'+'CDIVISION',CODES.cCode_No,'CODES')):R :10:H=lcTDivDes"
    lcBrowStr="lcMarker=IIF(RECNO()=lnBrRecNo,'',' '):1:H=' ':W=.F.,"+;
              "lcSelect=IIF(ASCAN(laSelect,APINVHDR.CINVNO)<> 0 , 'û', ' '):R :H='û',"+;
              "CINVNO:12:R :H=lcTInvNo,"+;
              "CVENPRIOR:H=lcTPrior,"+;
              "PayMeth=lfGetPayMeth(CVENPMETH):H=lcTPayMeth:1,"+;
              "NINVAMNT=(NINVAMNT - NINVPAID - NINVDISTK - NINVADJ):15:R :H=lcTOpnAmnt,"+;
              "NINVAMTAP:15:H=lcTAprPay,"+;
              "nInvFAAp :15:H = 'Apr foreign amt',"+;
              "NINVDISAP:15:H=lcTAprDisc,"+;
              "NINVADJAP:15:H=lcTAprAdj,"+;
              "DINVDUDAT:R :H=lcTInvDate,"+;
              "CCURRCODE:H=lcTCurrCode,"+;
              "NEXRATE:H=lcTCurRate,"+;
              "lcDivDisc=IIF(EMPTY(cDivision),lcTBlnkCode,LOOKUP(CODES.cDiscrep,'N'+APINVHDR.CDIVISION+'N'+'CDIVISION',CODES.cCode_No,'CODES')):R :10:H=lcTDivDes"
     *E300789,4  AMM end

    *E300643,1 Change this line [End]
    
    *B601610,1 Change this line to add the new field nInvFAAp to the 

  ELSE

    *E300643,1 Change this line for the changes we have made 
    *          to SYCCODES [Begin]
    *lcBrowStr="lcMarker=IIF(RECNO()=lnBrRecNo,'',' '):1:H=' ':W=.F.,"+;
    *          "lcSelect=IIF(ASCAN(laSelect,APINVHDR.CINVNO)<> 0 , 'û', ' '):R :H='û',"+;
    *          "CINVNO:12:R :H=lcTInvNo,"+;
    *          "CVENPRIOR:H=lcTPrior,"+;
    *          "PayMeth=lfGetPayMeth(CVENPMETH):H=lcTPayMeth:1,"+;
    *          "NINVAMNT=(NINVAMNT - NINVPAID - NINVDISTK - NINVADJ):15:R :H=lcTOpnAmnt,"+;
    *          "NINVAMTAP:15:H=lcTAprPay,"+;
    *          "NINVDISAP:15:H=lcTAprDisc,"+;
    *          "NINVADJAP:15:H=lcTAprAdj,"+;
    *          "DINVDUDAT:R :H=lcTInvDate,"+;
    *          "lcDivDisc=IIF(EMPTY(cDivision),lcTBlnkCode,LOOKUP(SYCCODES.cDiscrep,gcAct_Comp+APINVHDR.CDIVISION,SYCCODES.cCode_No,'CODES')):R :10:H=lcTDivDes"

    *E300789,4  AMM
    *lcBrowStr="lcMarker=IIF(RECNO()=lnBrRecNo,'',' '):1:H=' ':W=.F.,"+;
              "lcSelect=IIF(ASCAN(laSelect,APINVHDR.CINVNO)<> 0 , 'û', ' '):R :H='û',"+;
              "CINVNO:12:R :H=lcTInvNo,"+;
              "CVENPRIOR:H=lcTPrior,"+;
              "PayMeth=lfGetPayMeth(CVENPMETH):H=lcTPayMeth:1,"+;
              "NINVAMNT=(NINVAMNT - NINVPAID - NINVDISTK - NINVADJ):15:R :H=lcTOpnAmnt,"+;
              "NINVAMTAP:15:H=lcTAprPay,"+;
              "NINVDISAP:15:H=lcTAprDisc,"+;
              "NINVADJAP:15:H=lcTAprAdj,"+;
              "DINVDUDAT:R :H=lcTInvDate,"+;
              "lcDivDisc=IIF(EMPTY(cDivision),lcTBlnkCode,LOOKUP(CODES.cDiscrep,gcAct_Comp+APINVHDR.CDIVISION+'N'+'CDIVISION',CODES.cCode_No,'CODES')):R :10:H=lcTDivDes"
    lcBrowStr="lcMarker=IIF(RECNO()=lnBrRecNo,'',' '):1:H=' ':W=.F.,"+;
              "lcSelect=IIF(ASCAN(laSelect,APINVHDR.CINVNO)<> 0 , 'û', ' '):R :H='û',"+;
              "CINVNO:12:R :H=lcTInvNo,"+;
              "CVENPRIOR:H=lcTPrior,"+;
              "PayMeth=lfGetPayMeth(CVENPMETH):H=lcTPayMeth:1,"+;
              "NINVAMNT=(NINVAMNT - NINVPAID - NINVDISTK - NINVADJ):15:R :H=lcTOpnAmnt,"+;
              "NINVAMTAP:15:H=lcTAprPay,"+;
              "NINVDISAP:15:H=lcTAprDisc,"+;
              "NINVADJAP:15:H=lcTAprAdj,"+;
              "DINVDUDAT:R :H=lcTInvDate,"+;
              "lcDivDisc=IIF(EMPTY(cDivision),lcTBlnkCode,LOOKUP(CODES.cDiscrep,'N'+APINVHDR.CDIVISION+'N'+'CDIVISION',CODES.cCode_No,'CODES')):R :10:H=lcTDivDes"
    *E300789,4  AMM end

    *E300643,1 Change this line [End]
    
  ENDIF
  
  *B801208,4 AMM start add rest
  BROWSE REST FIELDS &lcBrowStr;
         WINDOW (lcBrowName) ;
         WHEN lfwBrows() .AND. lfvpbSel();
         VALID :F lfvBrowse();
         IN WINDOW (gcBaseWind) ;
         LOCK 0;
         NOAPPEND;
         NOCLEAR;
         NODELETE;
         NOWAIT;
         NOEDIT;
         SAVE;
         TITLE lcBrTtl
  *B801208,4 AMM end
ENDIF

*!**************************************************************************
*!
*!        Function : lfvBrowse
*!
*!**************************************************************************
* 
FUNCTION lfvBrowse

IF WONTOP(lcBrTtl)    && Close from corner in dos. (browse exist,brows window not exist)
  IF !WEXIST(lcBrowName) 
    HIDE WINDOW(gcBaseWind)
    glFromBrow = .F.
    CLEAR READ
    KEYBOARD CHR(13)
    IF !glQuitting   
      =lfvClose()      && Unlock records if close from corner under dos. 
    ENDIF  
    RETURN TO APMNCHP.SPR
  ENDIF
ELSE                 && Close from corner in windows. (browse not exit, base window not vis.)
  =lfBrowUnTrap() .AND. lfClrMous() 
  IF !WVISIBLE(gcBaseWind)
    HIDE WINDOW(gcBaseWind)
    glFromBrow = .F.
    IF !glQuitting   
      =lfvClose()      && Unlock selected recored from screen corner under win.
    ENDIF  
    SELECT APINVHDR
    SET FILTER TO
    SET RELATION TO
  ENDIF
  ***in case of switching between two browses in different screens.
  IF WPARENT(WONTOP()) <> gcBaseWind  .AND. !WONTOP(lcVenSum) .AND. !WONTOP(lcInvSum)  .AND. !WONTOP('SCOPE')

    *E300643,1 Change this line for the changes we have made 
    *          to SYCCODES [Begin]
    *SELECT SYCCODES
    SELECT CODES
    *E300643,1 Change this line [End]
    
    SET FILTER TO 
    SELECT APINVHDR
    SET FILTER TO
    SET RELATION TO
    RELEASE PAD _BROWSE OF _MSYSMENU
    RELEASE WINDOW (lcBrTtl)
  ENDIF
  =gfStopBrow()
ENDIF    

*!**************************************************************************
*!
*!      Function: lfGetPayMeth
*!
*!**************************************************************************
* 
FUNCTION lfGetPayMeth
PARAMETER lcPayCode

lnposition= ASCAN(laPayMeth,lcPayCode,1)
IF lnposition <>  0	
  RETURN laPayMeth[ASUBSCRIPT(laPayMeth, lnposition, 1),1] 
ELSE
  RETURN " "
ENDIF

*!**************************************************************************
*!
*!      Function: lfMousClk
*!
*!**************************************************************************
* mouse trap 
FUNCTION lfMousClk

ON KEY LABEL LEFTMOUSE DO lpMClkSel
RETURN .T.

*!**************************************************************************
*!
*!      Procedure: lpMClkSel
*!
*!**************************************************************************
*If mouse double click select record.
PROCEDURE lpMClkSel

IF lcObjectSt <> 'DISABLE'
  IF (SECONDS() < lnTime) .AND. WONTOP()=lcBrTtl
    *B608901,1 WAM 06/22/2009 Don't change selection flag when move the vertical scroll bar 
    *=lfvSelect()
    *B608901,1 WAM 06/22/2009 (End)
    IF ASCAN(laSelect,APINVHDR.CINVNO) <>  0 
      SHOW GET pbSelect,1 PROMPT 'UnSe\<lect'
    ELSE
      SHOW GET pbSelect,1 PROMPT 'Se\<lect'  
    ENDIF  
  ENDIF
  lnTime = _DBLCLICK+SECONDS()
ENDIF  

*!**************************************************************************
*!
*!      Function: lfClrMous
*!
*!**************************************************************************
*Clear mouse trap. 
FUNCTION lfClrMous

ON KEY LABEL LEFTMOUSE

*!**************************************************************************
*!
*!      Function: lfwBrows
*!
*!**************************************************************************
* Fill variable from record.
*
FUNCTION lfwBrows

lcBankCode = CBNKCODE 
lcCheckCode= CCHKACCT
lnAprToPay = NINVAMTAP
lnAprDisc  = NINVDISAP
lnAprAdj   = NINVADJAP

lnBrRecNo  = RECNO('APINVHDR')
SHOW WINDOW (lcBrTtl) REFRESH
IF WVISIBLE(lcVenSum) .OR. WVISIBLE(lcInvSum)
  =lfRefresh(IIF(WVISIBLE(lcVenSum), lcVenSum, "")+;
             IIF(WVISIBLE(lcInvSum), ","+lcInvSum, ""))
ENDIF

*!**************************************************************************
*!
*!      Function: lfvNoAprPa
*!
*!**************************************************************************
*if there is no approve payment in the bank checking account. 
*
FUNCTION lfvNoAprPa

IF EMPTY(APINVHDR.CINVNO)
  IF(lnPyChMN<> 3)
    IF !EMPTY(lcBankCode)
      *** Message: " No approve payment for bank checking account."
      *** Choices: "                      < Ok >                  "  
      =gfModalGen("TRM04057B00000","DIALOG")
    ENDIF  
  ELSE
    *** Message: " There is no approve payment available."
    *** Choices: "                      < Ok >                  "  
   =gfModalGen("TRM04141B00000","DIALOG")
  ENDIF  
ENDIF

*!**************************************************************************
*!
*!      Function: lfvSelect
*!
*!**************************************************************************
* Select or unselect certain invoice.
*
FUNCTION lfvSelect

lnFound  = ASCAN(laSelect, APINVHDR.CINVNO)
IF lnFound <>  0
  lnFound  = ASUBSCRIPT(laSelect,ASCAN(laSelect, APINVHDR.CINVNO),1)
  = ADEL(laSelect,lnFound)    	
  lnArlen=IIF(lnArlen=1,lnArlen,lnArlen-1)
  DIMENSION laSelect[lnArlen,2]
  =gfObj_lock(.F.)  
ELSE
  *B132275,1  TMI [Start] use the local version of gfObj_Lock function instead
  *IF gfObj_lock(.T.) && lock the pointed record.
  IF lfObj_lock(.T.) && lock the pointed record.
    *B132275,1  TMI [End  ] 
    lnArlen=lnArlen+1
    = AINS(laSelect,1)
    laSelect[1,1]= APINVHDR.CINVNO
    laSelect[1,2]= RECNO()
    DIMENSION laSelect[lnArlen,2]
  ENDIF
ENDIF

IF WVISIBLE(lcVenSum) && Calculate Total vendor window if it's opened.
  =lfvTot()
ENDIF  

lnBrRecNo  = RECNO('APINVHDR')
SHOW WINDOW (lcBrTtl) REFRESH  && refreshing the browse window. 

*!**************************************************************************
*!
*!      Function: lfvSelAll
*!
*!**************************************************************************
* Select all invoices.
*
FUNCTION lfvSelAll
PRIVATE lnTotInv1 , lnTotInv2

lnTotInv1   = 0
lnTotInv2   = 0
lcSavOrd = ORDER() 
SET ORDER TO 0
COUNT TO lnTotInv1
SET ORDER TO &lcSavOrd

SCAN  && Scan in file.
  lnTotInv2 = lnTotInv2 + 1
  =gfThermo(lnTotInv1,lnTotInv2,"Selecting all invoices...","Vendor: "+CVENDCODE+"  Invoice: "+CINVNO)
  lnFound  = ASCAN(laSelect, APINVHDR.CINVNO)
  IF lnFound = 0
    *B132275,1  TMI [Start] use the local version of gfObj_Lock function instead
    *IF gfObj_lock(.T.) && lock the pointed record.
    IF lfObj_lock(.T.) && lock the pointed record.
      *B132275,1  TMI [End  ] 
      lnArlen=lnArlen+1
      = AINS(laSelect,1)
      laSelect[1,1]= APINVHDR.CINVNO
      laSelect[1,2]= RECNO()
      DIMENSION laSelect[lnArlen,2]
    ENDIF
  ENDIF
ENDSCAN
SHOW GET pbSelect,1 PROMPT 'UnSe\<lect'
*B801208,4 AMM start
*GO TOP
LOCATE
*B801208,4 AMM end
lnBrRecNo  = RECNO('APINVHDR')
SHOW WINDOW (lcBrTtl) REFRESH  && refreshing the browse window.

*!**************************************************************************
*!
*!      Function: lfvSelNon
*!
*!**************************************************************************
* Unselect all selected invoices.
*
FUNCTION lfvSelNon
PRIVATE lnTotInv1 , lnTotInv2

lnTotInv1   = 0
lnTotInv2   = 0
lcSavOrd = ORDER() 
SET ORDER TO 0
COUNT TO lnTotInv1
SET ORDER TO &lcSavOrd

SCAN 
  lnTotInv2 = lnTotInv2 + 1
  =gfThermo(lnTotInv1,lnTotInv2,"Unselecting...","Vendor: "+CVENDCODE+"  Invoice: "+CINVNO)
  lnFound  = ASCAN(laSelect, APINVHDR.CINVNO)
  IF lnFound <>  0
    lnFound  = ASUBSCRIPT(laSelect,ASCAN(laSelect, APINVHDR.CINVNO),1)
    = ADEL(laSelect,lnFound)    	
    lnArlen=IIF(lnArlen=1,lnArlen,lnArlen-1)
    DIMENSION laSelect[lnArlen,2]
    =gfObj_lock(.F.)  
  ENDIF
ENDSCAN

SHOW GET pbSelect,1 PROMPT 'Se\<lect'

*B801208,4 AMM start
*GO TOP
LOCATE
*B801208,4 AMM end

lnBrRecNo  = RECNO('APINVHDR')
SHOW WINDOW (lcBrTtl) REFRESH  && refreshing the browse window. 

*!**************************************************************************
*!
*!      Function: lfvInvert
*!
*!**************************************************************************
* Select unselected invoices and unselect selected invoices.
FUNCTION lfvInvert

PRIVATE lnTotInv1 , lnTotInv2

lnTotInv1   = 0
lnTotInv2   = 0
lcSavOrd = ORDER() 
SET ORDER TO 0
COUNT TO lnTotInv1
SET ORDER TO &lcSavOrd

SCAN 
  lnTotInv2 = lnTotInv2 + 1
  =gfThermo(lnTotInv1,lnTotInv2,"Inverting all invoices...","Vendor: "+CVENDCODE+"  Invoice: "+CINVNO)
  lnFound  = ASCAN(laSelect, APINVHDR.CINVNO)
  IF lnFound <>  0
    lnFound  = ASUBSCRIPT(laSelect,ASCAN(laSelect, APINVHDR.CINVNO),1)
    = ADEL(laSelect,lnFound)    	
    lnArlen=IIF(lnArlen=1,lnArlen,lnArlen-1)
    DIMENSION laSelect[lnArlen,2]
    =gfObj_lock(.F.)  
  ELSE 
    *B132275,1  TMI [Start] use the local version of gfObj_Lock function instead
    *IF gfObj_lock(.T.) && lock the pointed record.
    IF lfObj_lock(.T.) && lock the pointed record.
      *B132275,1  TMI [End  ] 
      lnArlen=lnArlen+1
      = AINS(laSelect,1)
      laSelect[1,1]= APINVHDR.CINVNO
      laSelect[1,2]= RECNO()
      DIMENSION laSelect[lnArlen,2]
    ENDIF
  ENDIF
ENDSCAN

*B801208,4 AMM start
*GO TOP
LOCATE
*B801208,4 AMM end

lnBrRecNo  = RECNO('APINVHDR')
    
IF lnArlen = 1      && if there is no selected invoice.
  SHOW GET pbSelect,1 PROMPT 'Se\<lect'
ELSE
  IF laSelect[lnArlen-1,1]= APINVHDR.CINVNO &&If the frist invoice in the browse is selected.
    SHOW GET pbSelect,1 PROMPT 'UnSe\<lect'
  ELSE
    SHOW GET pbSelect,1 PROMPT 'Se\<lect'  
  ENDIF  
ENDIF

SHOW WINDOW (lcBrTtl) REFRESH  && refreshing the browse window. 

*!**************************************************************************
*!
*!      Function: lfvPySlInv
*!
*!**************************************************************************
*Activate the Pay selected invoices screen.
FUNCTION lfvPySlInv

*B132275,1  TMI [Start] if there is no selected checks then don't do anything
IF EMPTY(laSelect[1]) .AND. TYPE('laSelect[1]') = 'L'
  *** Message :" No selected invoices to pay. "
  *** Choice  :"            < Ok >            "
  =gfModalGen("TRM04045B00000","DIALOG")
  RETURN
ENDIF
*B132275,1  TMI [End  ] 

IF lnPyChMN = 1  && Calling from manual check.
  ***Get the next manual check number from apchecks*** 
  SELECT APCHECKS
  IF SEEK(lcBankCode+lcCheckCode,'APCHECKS')
    *B800072,1 We do not need to lock the record because it is already lo
    *IF gfObj_lock(.T.) && lock the pointed record.
    *B132275,1  TMI [Start] use the local version of "gfObj_lock" function
    *IF (ALLTRIM(cLok_User) = ALLTRIM(gcUser_ID)) .OR. gfObj_lock(.T.) && lock the pointed record.
    IF (ALLTRIM(cLok_User) = ALLTRIM(gcUser_ID)) .OR. lfObj_lock(.T.) && lock the pointed record.
    *B132275,1  TMI [End  ] 
      lnOChkNum  = APCHECKS.NCHKNXTMN
      lnGnChkNm  = APCHECKS.NCHKNXTMN+1
      lnChkNum = lnOChkNum
      REPLACE APCHECKS.NCHKNXTMN WITH lnGnChkNm
      =gfAdd_Info()  && Add the audit information to the record.
    ELSE

      *B601553,1 Change this line to change the choice [Begin]
      *  *** Message :" The checking account lcBankCode-lcCheckCode is being edited "
      *  ***         :" Cannot generate the next check number. "  
      *  *** Choice  :"                  < Retry >      < Cancel >            "
      *=gfModalGen("TRM04055B00015","DIALOG",ALLTRIM(lcBankCode)+'-'+ALLTRIM(lcCheckCode))

      *** Message :" The checking account lcBankCode-lcCheckCode is being edited "
      ***         :" Cannot generate the next check number. "  
      *** Choice  :"                   < OK >               "
      =gfModalGen("TRM04055B00000","DIALOG",ALLTRIM(lcBankCode)+'-'+ALLTRIM(lcCheckCode))

      *B601553,1 Change this line to change the choice [End]

      *B601553,1 Add this line to return if the lock didnt work [Begin]
      SELECT APINVHDR
      RETURN
      *B601553,1 Add this line to return if the lock didnt work [End]

    ENDIF   

  ENDIF 
  SELECT APINVHDR
ENDIF

= lfvTot() && Sun Selected Totamoount, tot_disc, tot_Agj,tot_1099

IF lnArlen=1
  *** Message :" No selected invoices to pay. "
  *** Choice  :"            < Ok >            "
  =gfModalGen("TRM04045B00000","DIALOG")
ELSE

  *B601553,1 Add this lines to check that the total approved payment is
  *not greater than zero if so give the user a message and return [Begin] 
  
  *B601553,1 IF Statment to check if the total approved payment is not 
  *greater than zero
  IF !lnTotPay > 0

    *B601553,4 Change these line to give the user the capability to
    *approves a negative amount [Begin]
    
    *** Message: " Approved payment for vendor ð must be greater than zero."
    *** Choices: "                           < Ok >                        "  
    *=gfModalGen("INM04084B00000" , "DIALOG" , ALLTRIM(lcVendCode))
    
    *** Message: "Approved payment for vendor ð is less than or equal zero."
    *** Choices: "                < Ok >       < Cancel >                  "  
    lnUserRep = gfModalGen("INM04084B04004" , "DIALOG" ,;
                           ALLTRIM(lcVendCode))
    
    *B601553,4 Change these line to give the user the capability [End]
    
    *B601553,4 Add these IF condition to give the user the capability to
    *approves a negative amount [Begin]
    
    *B601553,4 If the user selected to cancel the process
    IF lnUserRep = 2
    
    *B601553,4 Add these IF condition to give the user the capability [End]
      
      IF lnPyChMN = 1 .AND. !EOF('APCHECKS')
        lcSavAlias = ALIAS() 
        SELECT APCHECKS
        =gfObj_lock(.F.)    
        SELECT (lcSavAlias)
      ENDIF
      RETURN
      
    *B601553,4 Add these IF condition to give the user the capability to
    *approves a negative amount [Begin]
    ENDIF    && End of IF lnUserRep = 2
    *B601553,4 Add these IF condition to give the user the capability [End]
    
  ENDIF    && End of IF
  *B601553,1 Add this lines [End]

  *E300296,1 M.H 02/04/96 Define a new variable to hold the symbol of the approved amount.
  lcCurrSmbl = LOOKUP(SYCCURR.cCurrSmbl,APINVHDR.CAPRCURCOD,SYCCURR.cCurrCode,'CCURRCODE')
  lcBaseSmbl = PADR(ALLTRIM(SET('CURRENCY','TO')),3,' ')
  *E300296,1 M.H 02/04/96 End.
  
  *B601013,1 Default the exchange rate with the payment date rate.
  lnChkExUnt = 1
  IF APINVHDR.CAPRCURCOD <> gcBaseCurr 
    IF EMPTY(ldPayDat)
      lnChkExRat  = 0
      lcRateDisp  = 'DISABLE'
    ELSE  
      lcRateDisp  = IIF(llEditEx , 'ENABLE', 'DISABLE')
      lnChkExRat  = gfChkRate('lnChkExUnt',;
                    APINVHDR.CAPRCURCOD,ldPayDat)
    ENDIF                
  ELSE
    lcRateDisp  = 'DISABLE'
    lnChkExRat  = 1
  ENDIF                
  *B601013,1 Initialize old payment date.
  ldOldPDat = {}
  *B601013,1 end.
  
  *** Release trap before branching to another screen. ***
  ON KEY
  RELEASE PAD _BROWSE OF _MSYSMENU
  RELEASE WINDOW (lcBrTtl)
    
  *E300683,1 Call *.SPR from screens directory
  * DO APYSLINV.SPR 
  DO (gcScrDir + gcWinAppl + '\APYSLINV.SPR')
  *E300683,1 end          

  *** Push the same keys again ***
  =lfPushKey()
  =lfDispBrow()
  IF _DOS AND !(llFromTrap)  &&if we came from brows window no need to define browse pad.
    =lfDefinePad()
  ENDIF
  
  *B801208,4 AMM start
  *GO TOP              && Activate the browse
  LOCATE
  *B801208,4 AMM end
  
  lnBrRecNo  = RECNO('APINVHDR')
  IF ASCAN(laSelect,APINVHDR.CINVNO)<> 0 
    SHOW GET pbSelect,1 PROMPT 'UnSe\<lect'
  ELSE
    SHOW GET pbSelect,1 PROMPT 'Se\<lect'  
  ENDIF  
  =lfShowDs(.F.,.T.)  && Disable object if there is no more approve records.
ENDIF  

*B601553,1 Add this lines to unlock the APCHECKS record [Begin]
IF lnPyChMN = 1 .AND. !EOF('APCHECKS')
  lcSavAlias = ALIAS() 
  SELECT APCHECKS
  =gfObj_lock(.F.)    
  SELECT (lcSavAlias)
ENDIF
*B601553,1 Add this lines to unlock the APCHECKS record [End]

*!**************************************************************************
*!
*!      Function: lfvProceed
*!
*!**************************************************************************
*
FUNCTION lfvProceed

llProcOk  = .T.
lnArCount = ALEN(laSelect,1) 

*B601013,1 Change date validation as follows 
** Get the company linked. **
*IF lfVDtMsg(gcPrnt_Cmp ,@lcFisPrd,@lcFisYer,ldPayDat)
*  lcPeriod  = lcFisPrd
*  lcYear    = lcFisYer
*  ldOldPDat = ldPayDat 
*ELSE
*  ldPayDat = ldOldPDat  
*  SHOW GET ldPayDat
*  RETURN
*  _CUROBJ = OBJNUM(ldPayDat)
*ENDIF  
*B601013,1 Check if the date is empty,
IF EMPTY(ldPayDat)
  **  Message: "You have to enter The ð.       "
  **  Choices: "              ® Ok ¯           "  
  =gfModalGen("TRM04066B00000","DIALOG",'payment date')
  _CUROBJ = OBJNUM(ldPayDat)
  RETURN
ELSE
*B601013,1 Validate the date if it has not been validated before
  =lfvPayDate()
  IF EMPTY(ldPayDat)
    RETURN
  ENDIF  
ENDIF  
*B601013,1 end.

*E301077,79 AMM Open file
=gfOpenFile(gcDataDir+'APPAYMNT','TYPMETHNO','SH')
*E301077,79 AMM end

** In case of calling bar is manual check payment or non check payment.
IF lnPyChMN <>  1   
  IF EMPTY(lcPayNum)
    **  Message: "You have to enter The ð.       "
    **  Choices: "              ® Ok ¯           "  
    =gfModalGen("TRM04066B00000","DIALOG",'payment number')
    _CUROBJ = OBJNUM(lcPayNum)
    RETURN  && <-- quit function.
  ELSE
    SELECT APPAYMNT
    IF SEEK('P'+'H'+SPACE(8)+SPACE(12)+lcPayNum) .OR.;
       SEEK('P'+'N'+lcBankCode+lcCheckCode+lcPayNum)
      **  Message: " The payment number already exists. "
      **  Choices: "                 ® Ok ¯             "
      =gfModalGen("TRM04051B00000","DIALOG")
      _CUROBJ=OBJNUM(lcPayNum)
      RETURN  && <--- quit function.
    ENDIF 
    SELECT APINVHDR
  ENDIF
ELSE  && in case of manual check.
  IF EMPTY(lnChkNum)
    **  Message: " You have to enter The ð.  "
    **  Choices: "             ® Ok ¯        "
    =gfModalGen("TRM04066B00000","DIALOG",'Check number')
    lnChkNum=lnGnChkNm
    SHOW GET lnChkNum
    _CUROBJ = OBJNUM(lnChkNum)
    RETURN && <--- quit function.
  ELSE
    IF lnChkNum < 0
      **  Message: " Negative values are not allowed. "
      **  Choices: "              ® Ok ¯              "  
      =gfModalGen("TRM04087B00000","DIALOG")
      lnChkNum=lnOChkNum
      SHOW GET lnChkNum
      RETURN && <--- quit function.
    ELSE
      SELECT APPAYMNT
      IF SEEK('P'+'M'+lcBankCode+lcCheckCode+PADL(lnChkNum,8,'0'))
        **  Message: " A manual check already exists with this number."
        **           " You can not have duplicate manual check numbers"
        **           " for the same checking account.                 "    
        **  Choices: "                        ® Ok ¯                  "
        =gfModalGen("TRM04053B00000","DIALOG")
        _CUROBJ=OBJNUM(lnChkNum)
        RETURN  && <--- quit function.
      ELSE
        ** If check number changed manually add one to next check number.
        SELECT APCHECKS  
        IF lnChkNum <>  lnOChkNum  && If generated check no changed.
          REPLACE APCHECKS.NCHKNXTMN WITH lnChkNum+1
          =gfAdd_Info()  && Add the audit information to the record.
        ENDIF  
      ENDIF
    ENDIF  
  ENDIF  
ENDIF

*B601013,1 Check if the exchange rate is 0.
*E300296,1 M.H Add the currency to the AP module.
*E300296,1 M.H Check the Checking account currency rate with gcBaseCurr with the proceed date.
*lnChkExUnt = 0
*lnChkExRat = 0
*IF gfGetMemVar('LLMULCURR')
*  lnChkExRat = gfChkRate('lnChkExUnt',APINVHDR.CAPRCURCOD,ldPayDat,.T.,.F.)
  IF lnChkExRat = 0
    lnChkExRat  = gfChkRate('lnChkExUnt',;
                          APINVHDR.CAPRCURCOD,ldPayDat, .T.,;
                          gcAct_Comp, gcBaseCurr, .T.)
    IF lnChkExRat = 0 
      IF !llAddRate
        ** MESSAGE : " A valid ð to ð exchange rate could not "
        **           " be found on ð.                         "
        **           "                  ® Ok ¯                "
        =gfModalGen("QRM04157B00000","DIALOG",ALLTRIM(APINVHDR.CAPRCURCOD)+'|'+ALLTRIM(gcBaseCurr)+'|'+DTOC(ldPayDat))
      ENDIF
      llProcOk  = .F.
      *IF lnPyChMN <> 3
      _CUROBJ=OBJNUM(ldPayDat)
      *ELSE
      *_CUROBJ=OBJNUM(lcCurrCode)
      *ENDIF
      RETURN
    ENDIF  
    SHOW GET lnChkExRat
  ENDIF
*ELSE
*  lnChkExUnt = 1
*  lnChkExRat = 1
*ENDIF  
*E300296,1 M.H End.
*B601013,1

SELECT APINVHDR
FOR lnRecCount= 1 TO lnArCount-1 
  GO laSelect[lnRecCount,2] && go to the seleced record.
  ** If invoice date is greater than the payment date. **
  
  
  *B604453,1 Amin, Change the validation to include the posting date [Start]  
  *IF APINVHDR.DINVDATE > ldPayDat 
  IF APINVHDR.DINVDATE > ldPayDat OR APINVHDR.DPOSTDATE > ldPayDat 
    *B604453,1 Amin, Change the validation to include the posting date [End]  
  
    **  Message: " Invoice xxxx date is greater than the payment date.  "
    **             All selected invoices should have dates less than or "
    **             equal to payment date.                               "
    **  Choices: "                          ® Ok ¯                      "
    lcMessage = ALLTRIM(APINVHDR.CINVNO)
  
    *B604453,1 Amin, Change the Message [Start]
    *=gfModalGen("TRM04046B00000","DIALOG",lcMessage)    
    =gfModalGen("TRM04198B00000","DIALOG",lcMessage)
    *B604453,1 Amin, Change the Message [End]    
    llProcOk  = .F.
    _CUROBJ=OBJNUM(ldPayDat)
    EXIT
    *B600580,1 start    
    *B600580,1 no need to lock again as the record is alredy locked  
    *  ELSE && if invoice date is smaller than or equal the payment date.
    *    IF ! gfObj_lock(.T.) && lock the pointed record.
    *      llProcOk =.F.
    *      EXIT
    *    ENDIF  
    *B600580,1 end
  ENDIF  
ENDFOR   

*B604733,1 Get the equivalent exchange rate. [Begin]
PRIVATE lnExchUnit , lnExchRate
STORE 0 TO lnExchUnit , lnExchRate
IF gfGetMemVar('LLMULCURR')
  lnExchRate = gfChkRate('lnExchUnit',ApInvHdr.CAPRCURCOD,ldPayDat,.T.,.F.)
  IF lnExchRate = 0
    STORE 1 TO lnExchUnit , lnExchRate
  ENDIF
ELSE
  STORE 1 TO lnExchUnit , lnExchRate    
ENDIF
*B604733,1 Get the equivalent exchange rate. [End]

IF llProcOk && If valid date and locking ok.
  SELECT APPAYMNT
  APPEND BLANK

  *E300296,1 M.H Add the currency to the AP module.
  ** Add 3 APINVHDR approved fields Curr,Rate,Unit 

  *REPLACE CPAYTYPE   WITH "P",;
          DPAYDATE   WITH ldPayDat,;
          CFISFYEAR  WITH lcYear,;
          CFSPPRDID  WITH lcPeriod,;
          CPAYCLNO   WITH lcVendCode,;
          NPAYAMNT   WITH 0,;
          NPAYDISC   WITH 0,;
          NPAYADJ    WITH 0,;
          NINV1099A  WITH 0,;
          CPAYRECST  WITH 'O';
          CBNKCODE   WITH lcBankCode,;
          CCHKACCT   WITH lcCheckCode,;
          CPAYMETH   WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
          CPAYCOMP   WITH IIF(lnArlen = 2,APINVHDR.COUTCOMP,APVENDOR.CVENCOMP),;
          CPAYDOCNO  WITH IIF(lnPyChMN = 1,PADL(lnChkNum,FSIZE('CPAYDOCNO'),'0'),PADL(lcPayNum,FSIZE('CPAYDOCNO'),'0'))

*B603689,1 AMH (Ahmed Maher). Repalce  NEXRATE & NCURRUNIT with the correct values (Start)
*  REPLACE CPAYTYPE   WITH "P",;
          DPAYDATE   WITH ldPayDat,;
          CFISFYEAR  WITH lcYear,;
          CFSPPRDID  WITH lcPeriod,;
          CPAYCLNO   WITH lcVendCode,;
          NPAYAMNT   WITH 0,;
          NPAYDISC   WITH 0,;
          NPAYADJ    WITH 0,;
          NINV1099A  WITH 0,;
          CPAYRECST  WITH 'O';
          CBNKCODE   WITH lcBankCode,;
          CCHKACCT   WITH lcCheckCode,;
          CPAYMETH   WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
          CPAYCOMP   WITH IIF(lnArlen = 2,APINVHDR.COUTCOMP,APVENDOR.CVENCOMP),;
          CPAYDOCNO  WITH IIF(lnPyChMN = 1,PADL(lnChkNum,FSIZE('CPAYDOCNO'),'0'),PADL(lcPayNum,FSIZE('CPAYDOCNO'),'0')),;
          CCURRCODE  WITH APINVHDR.CAPRCURCOD,;
          NEXRATE    WITH APINVHDR.NAPREXRAT,;
          NCURRUNIT  WITH APINVHDR.NAPRCURUNT

  *B603679,1 KHM 07/10/2000 (Begin) Changing the replacement of cCurrCode,
  *B603679,1                nExRate and nCurrUnit by using the old 
  *B603679,1                replacement before fixing bug#603689.
  *REPLACE CPAYTYPE   WITH "P",;
          DPAYDATE   WITH ldPayDat,;
          CFISFYEAR  WITH lcYear,;
          CFSPPRDID  WITH lcPeriod,;
          CPAYCLNO   WITH lcVendCode,;
          NPAYAMNT   WITH 0,;
          NPAYDISC   WITH 0,;
          NPAYADJ    WITH 0,;
          NINV1099A  WITH 0,;
          CPAYRECST  WITH 'O';
          CBNKCODE   WITH lcBankCode,;
          CCHKACCT   WITH lcCheckCode,;
          CPAYMETH   WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
          CPAYCOMP   WITH IIF(lnArlen = 2,APINVHDR.COUTCOMP,APVENDOR.CVENCOMP),;
          CPAYDOCNO  WITH IIF(lnPyChMN = 1,PADL(lnChkNum,FSIZE('CPAYDOCNO'),'0'),PADL(lcPayNum,FSIZE('CPAYDOCNO'),'0')),;
          CCURRCODE WITH APINVHDR.CAPRCURCOD,;
          NEXRATE   WITH lnChkExRat,;
          NCURRUNIT WITH lnChkExUnt
          
  *B604733,1 Update currency unit and rate with equivalent [Begin] 
  *REPLACE CPAYTYPE   WITH "P",;
  *        DPAYDATE   WITH ldPayDat,;
  *        CFISFYEAR  WITH lcYear,;
  *        CFSPPRDID  WITH lcPeriod,;
  *        CPAYCLNO   WITH lcVendCode,;
  *        NPAYAMNT   WITH 0,;
  *        NPAYDISC   WITH 0,;
  *        NPAYADJ    WITH 0,;
  *        NINV1099A  WITH 0,;
  *        CPAYRECST  WITH 'O';
  *        CBNKCODE   WITH lcBankCode,;
  *        CCHKACCT   WITH lcCheckCode,;
  *        CPAYMETH   WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
  *        CPAYCOMP   WITH IIF(lnArlen = 2,APINVHDR.COUTCOMP,APVENDOR.CVENCOMP),;
  *        CPAYDOCNO  WITH IIF(lnPyChMN = 1,PADL(lnChkNum,FSIZE('CPAYDOCNO'),'0'),PADL(lcPayNum,FSIZE('CPAYDOCNO'),'0')),;
  *        CCURRCODE  WITH APINVHDR.CAPRCURCOD,;
  *        NEXRATE    WITH APINVHDR.NAPREXRAT,;
  *        NCURRUNIT  WITH APINVHDR.NAPRCURUNT

  *B129142,1 EIH 08/30/2005 Fix bug of saving wrong exchange rate when we edit it [Begin].
  *REPLACE CPAYTYPE   WITH "P",;
          DPAYDATE   WITH ldPayDat,;
          CFISFYEAR  WITH lcYear,;
          CFSPPRDID  WITH lcPeriod,;
          CPAYCLNO   WITH lcVendCode,;
          NPAYAMNT   WITH 0,;
          NPAYDISC   WITH 0,;
          NPAYADJ    WITH 0,;
          NINV1099A  WITH 0,;
          CPAYRECST  WITH 'O';
          CBNKCODE   WITH lcBankCode,;
          CCHKACCT   WITH lcCheckCode,;
          CPAYMETH   WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
          CPAYCOMP   WITH IIF(lnArlen = 2,APINVHDR.COUTCOMP,APVENDOR.CVENCOMP),;
          CPAYDOCNO  WITH IIF(lnPyChMN = 1,PADL(lnChkNum,FSIZE('CPAYDOCNO'),'0'),PADL(lcPayNum,FSIZE('CPAYDOCNO'),'0')),;
          CCURRCODE  WITH APINVHDR.CAPRCURCOD,;
          NEXRATE    WITH lnExchRate,;
          NCURRUNIT  WITH lnExchUnit
  
  
  REPLACE CPAYTYPE   WITH "P",;
          DPAYDATE   WITH ldPayDat,;
          CFISFYEAR  WITH lcYear,;
          CFSPPRDID  WITH lcPeriod,;
          CPAYCLNO   WITH lcVendCode,;
          NPAYAMNT   WITH 0,;
          NPAYDISC   WITH 0,;
          NPAYADJ    WITH 0,;
          NINV1099A  WITH 0,;
          CPAYRECST  WITH 'O';
          CBNKCODE   WITH lcBankCode,;
          CCHKACCT   WITH lcCheckCode,;
          CPAYMETH   WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
          CPAYCOMP   WITH IIF(lnArlen = 2,APINVHDR.COUTCOMP,APVENDOR.CVENCOMP),;
          CPAYDOCNO  WITH IIF(lnPyChMN = 1,PADL(lnChkNum,FSIZE('CPAYDOCNO'),'0'),PADL(lcPayNum,FSIZE('CPAYDOCNO'),'0')),;
          CCURRCODE  WITH APINVHDR.CAPRCURCOD,;
          NEXRATE    WITH lnChkExRat,;
          NCURRUNIT  WITH lnExchUnit
          
  *B129142,1 EIH 08/30/2005 [End].
  
  *B604733,1 Update currency unit and rate with equivalent [End] 

   *B603679,1 KHM 07/10/2000 (End)
   
*B603689,1 AMH (Ahmed Maher). Repalce  NEXRATE & NCURRUNIT with the correct values (End)

  IF lnPyChMN = 1     && Case calling bar is Manual check payment.
    REPLACE APCHECKS.DCHKLMDAT WITH ldPayDat,;
            APCHECKS.NCHKLMAMT WITH 0
  ENDIF

  =gfAdd_Info()  && Add the audit information to the record.

  REPLACE APVENDOR.NVENLPAYA WITH 0  && Init the last payment amount.

  SELECT APINVHDR
  ** Do process for all selected invoices. **
  FOR lnRecCount= 1 TO lnArCount-1 
    GO laSelect[lnRecCount,2] && go to the seleced record.
    *E300316,1 HISH  11/30/95. Got the equation signs. (Begin)
    *E300316,1 to exchange currency from invoice currency to company base currency.
    *E300316,1 HISH 01/08/96  Passed pointer parameter to get Unit sgin. (Begin)
    lcExSin2 = ' '
    lcExSin1 = gfGetExSin(@lcExSin2,cCurrCode)
    *lcExSin2 = IIF(lcExSin1 = '*','/','*')
    
    *E300316,1 to exchange currency from invoice currency to approved currency.
    lcExSin4 = ' '

    *B601526,1 Ther is no need for this lines now [Begin]

    *B601416,1 Change the calling of gfGetExSin so if the [BEGIN]
    * Invoice currency is the base currency I make it the To currency 
    * and if not the Approve currency is the To currency 
    *lcExSin3 = gfGetExSin(@lcExSin4,cCurrCode,cAprCurCod)
    *B601416,1 IF Statment to check if the Invoice currency is the 
    * same as the base currency
     *IF cCurrCode = gcBaseCurr 
     *  lcExSin3 = gfGetExSin(@lcExSin4,cAprCurCod,cCurrCode)
     *  lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
     *  lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
     *ELSE   && Else
     *  lcExSin3 = gfGetExSin(@lcExSin4,cCurrCode,cAprCurCod)
     *ENDIF  && End of IF 
    *B601416,1 Change the calling of gfGetExSin [END]

    *B601526,1 Ther is no need for this lines now [End]

    *lcExSin4 = IIF(lcExSin3 = '*','/','*')

    *E300316,1 to exchange currency from approved currency to company base currency.
    lcExSin6 = ' '
    lcExSin5 = gfGetExSin(@lcExSin6,cAprCurCod)
    *lcExSin6 = IIF(lcExSin5 = '*','/','*')
    *E300316,1 (End)

    =gfThermo(lnArCount-1,lnRecCount,"Paying selected invoices...","Vendor: "+CVENDCODE+"  Invoice: "+CINVNO)
    **  Updating records in payment file.  **
 
    *E300296,1 M.H Add the currency to the AP module.
    * Calculate the amount to pay,Adjustment,Discount,1099 with the currency rate / unit.
    *E300316,1 HISH  11/30/95. Used the variables hold signs in the equation. (Begin)
    *lnAprPayB  = ROUND(APINVHDR.NINVAMTAP * APINVHDR.NAPREXRAT / APINVHDR.NAPRCURUNT * lnChkExRat / lnChkExUnt,2)

    *B601526,1 Change this line (Replace the calculation using lcExSin3  
    *          and lcExSin4 with the new field ) [Begin]
    *lnAprPayB  = ROUND(APINVHDR.NINVAMTAP &lcExSin3 APINVHDR.NAPREXRAT &lcExSin4 APINVHDR.NAPRCURUNT &lcExSin5 lnChkExRat &lcExSin6 lnChkExUnt,2)
    lnAprPayB  = ROUND(APINVHDR.NINVFAAP &lcExSin5 lnChkExRat &lcExSin6 lnChkExUnt,2)
    *B601526,1 Change this line [End]
        
    *lnAprDisB  = ROUND(APINVHDR.NINVDISAP * APINVHDR.NEXRATE/APINVHDR.NCURRUNIT,2)
    lnAprDisB  = ROUND(APINVHDR.NINVDISAP &lcExSin1 APINVHDR.NEXRATE &lcExSin2 APINVHDR.NCURRUNIT,2)
    
    *lnAprAdjB  = ROUND(APINVHDR.NINVADJAP * APINVHDR.NEXRATE/APINVHDR.NCURRUNIT,2)
    lnAprAdjB  = ROUND(APINVHDR.NINVADJAP &lcExSin1 APINVHDR.NEXRATE &lcExSin2 APINVHDR.NCURRUNIT,2)
    
    *lnApr1099B = ROUND(APINVHDR.NINVA1099 * APINVHDR.NEXRATE/APINVHDR.NCURRUNIT,2)
    lnApr1099B = ROUND(APINVHDR.NINVA1099 &lcExSin1 APINVHDR.NEXRATE &lcExSin2 APINVHDR.NCURRUNIT,2)
    *E300316,1 (End)

*   REPLACE APPAYMNT.NPAYAMNT  WITH APPAYMNT.NPAYAMNT  + APINVHDR.NINVAMTAP,;
            APPAYMNT.NPAYDISC  WITH APPAYMNT.NPAYDISC  + APINVHDR.NINVDISAP,;
            APPAYMNT.NPAYADJ   WITH APPAYMNT.NPAYADJ   + APINVHDR.NINVADJAP,;
            APPAYMNT.NINV1099A WITH APPAYMNT.NINV1099A + APINVHDR.NINVA1099

    *E300296,1 M.H Add the currency to the AP module.
    ** Add the currency calculations. 
    *E300316,1 HISH  11/30/95. Used the variables hold signs in the equation. (Begin)
    *REPLACE APPAYMNT.NPAYAMNT  WITH APPAYMNT.NPAYAMNT  + ROUND(APINVHDR.NINVAMTAP * APINVHDR.NAPREXRAT/APINVHDR.NAPRCURUNT,2),;
            APPAYMNT.NPAYDISC  WITH APPAYMNT.NPAYDISC  + lnAprDisB,;
            APPAYMNT.NPAYADJ   WITH APPAYMNT.NPAYADJ   + lnAprAdjB,;
            APPAYMNT.NINV1099A WITH APPAYMNT.NINV1099A + lnApr1099B

    *B601526,1 Change this line (Replace the calculation using lcExSin3  
    *          and lcExSin4 with the new field ) [Begin]
    *REPLACE APPAYMNT.NPAYAMNT  WITH APPAYMNT.NPAYAMNT  +;
    *        ROUND(APINVHDR.NINVAMTAP &lcExSin3 APINVHDR.NAPREXRAT &lcExSin4 APINVHDR.NAPRCURUNT,2),;
    *        APPAYMNT.NPAYDISC  WITH APPAYMNT.NPAYDISC  + lnAprDisB,;
    *        APPAYMNT.NPAYADJ   WITH APPAYMNT.NPAYADJ   + lnAprAdjB,;
    *        APPAYMNT.NINV1099A WITH APPAYMNT.NINV1099A + lnApr1099B

    REPLACE APPAYMNT.NPAYAMNT  WITH APPAYMNT.NPAYAMNT  +;
            ROUND(APINVHDR.NINVFAAP , 2),;
            APPAYMNT.NPAYDISC  WITH APPAYMNT.NPAYDISC  + lnAprDisB,;
            APPAYMNT.NPAYADJ   WITH APPAYMNT.NPAYADJ   + lnAprAdjB,;
            APPAYMNT.NINV1099A WITH APPAYMNT.NINV1099A + lnApr1099B

    *B601526,1 Change this line [End]
    
    *E300316,1 (End)

    =gfAdd_Info('APPAYMNT')  && Add the audit information to the record.
    
    **  Updating records in vendor file.  **
    *E300296,1 M.H Add the currency to the AP module.
    ** Add the currency calculations. 
*    REPLACE APVENDOR.DVENLPAYD WITH ldPayDat,;
            APVENDOR.NVENLPAYA WITH APVENDOR.NVENLPAYA + APINVHDR.NINVAMTAP ,;
            APVENDOR.NVEN1099B WITH APVENDOR.NVEN1099B + APINVHDR.NINV1099A ,;
            APVENDOR.NVENOPNDR WITH APVENDOR.NVENOPNDR + IIF(APINVHDR.NINVAMNT < 0,;
                                    APINVHDR.NINVAMTAP + APINVHDR.NINVDISAP + APINVHDR.NINVADJAP,0),;
            APVENDOR.NVENBAL   WITH APVENDOR.NVENBAL   - (APINVHDR.NINVAMTAP+APINVHDR.NINVDISAP+APINVHDR.NINVADJAP),;
            APVENDOR.NVENCPAY  WITH APVENDOR.NVENCPAY  + APINVHDR.NINVAMTAP,;
            APVENDOR.CVENLPAYN WITH IIF(lnPyChMN = 1,PADL(lnChkNum,FSIZE('CVENLPAYN','APVENDOR'),'0'),PADL(lcPayNum,FSIZE('CVENLPAYN','APVENDOR'),'0'))	
    *E300316,1 HISH  11/30/95. Used the variables hold signs in the equation. (Begin)
    *REPLACE APVENDOR.DVENLPAYD WITH ldPayDat,;
            APVENDOR.NVENLPAYA WITH APVENDOR.NVENLPAYA + lnAprPayB,;
            APVENDOR.NVEN1099B WITH APVENDOR.NVEN1099B + lnApr1099B,;
            APVENDOR.NVENOPNDR WITH APVENDOR.NVENOPNDR + ROUND(IIF(APINVHDR.NINVAMNT < 0,;
                                    APINVHDR.NINVAMTAP + APINVHDR.NINVDISAP + APINVHDR.NINVADJAP,0) * APINVHDR.NEXRATE / APINVHDR.NCURRUNIT,2),;
            APVENDOR.NVENBAL   WITH APVENDOR.NVENBAL   - ROUND((APINVHDR.NINVAMTAP+APINVHDR.NINVDISAP+APINVHDR.NINVADJAP) * APINVHDR.NEXRATE / APINVHDR.NCURRUNIT,2),;
            APVENDOR.NVENCPAY  WITH APVENDOR.NVENCPAY  + lnAprPayB,;
            APVENDOR.CVENLPAYN WITH IIF(lnPyChMN = 1,PADL(lnChkNum,FSIZE('CVENLPAYN','APVENDOR'),'0'),PADL(lcPayNum,FSIZE('CVENLPAYN','APVENDOR'),'0'))	
    
    REPLACE APVENDOR.DVENLPAYD WITH ldPayDat,;
            APVENDOR.NVENLPAYA WITH APVENDOR.NVENLPAYA + lnAprPayB,;
            APVENDOR.NVEN1099B WITH APVENDOR.NVEN1099B + lnApr1099B,;
            APVENDOR.NVENOPNDR WITH APVENDOR.NVENOPNDR + ROUND(IIF(APINVHDR.NINVAMNT < 0,;
                               APINVHDR.NINVAMTAP + APINVHDR.NINVDISAP + APINVHDR.NINVADJAP;
                               &lcExSin1 APINVHDR.NEXRATE &lcExSin2 APINVHDR.NCURRUNIT,0),2),;
            APVENDOR.NVENBAL   WITH APVENDOR.NVENBAL   -;
                               ROUND((APINVHDR.NINVAMTAP+APINVHDR.NINVDISAP+APINVHDR.NINVADJAP);
                               &lcExSin1 APINVHDR.NEXRATE &lcExSin2 APINVHDR.NCURRUNIT,2),;
            APVENDOR.NVENCPAY  WITH APVENDOR.NVENCPAY  + lnAprPayB,;
            APVENDOR.CVENLPAYN WITH IIF(lnPyChMN = 1,PADL(lnChkNum,FSIZE('CVENLPAYN','APVENDOR'),'0'),PADL(lcPayNum,FSIZE('CVENLPAYN','APVENDOR'),'0'))	
    *E300316,1 (End)            


    =gfAdd_Info('APVENDOR')  && Add the audit information to the record.

    ** Updating records in apchecks file. **
    IF lnPyChMN = 1        && Case calling bar is Manual check payment.
      *REPLACE APCHECKS.NCHKLMAMT WITH APCHECKS.NCHKLMAMT + APINVHDR.NINVAMTAP
      *E300316,1 HISH  11/30/95. Used the variables hold signs in the equation. (Begin)
      *REPLACE APCHECKS.NCHKLMAMT WITH APCHECKS.NCHKLMAMT + ROUND(APINVHDR.NINVAMTAP * APINVHDR.NAPREXRAT / APINVHDR.NAPRCURUNT,2)

      *B601526,1 Change this line (Replace the calculation using lcExSin3  
      *          and lcExSin4 with the new field ) [Begin]
      *REPLACE APCHECKS.NCHKLMAMT WITH;
      *        APCHECKS.NCHKLMAMT + ROUND(APINVHDR.NINVAMTAP;
      *        &lcExSin3 APINVHDR.NAPREXRAT &lcExSin4 APINVHDR.NAPRCURUNT,2)

      REPLACE APCHECKS.NCHKLMAMT WITH;
              APCHECKS.NCHKLMAMT + ROUND(APINVHDR.NINVFAAP , 2)

      *B601526,1 Change this line [End]
      
      *E300316,1 (End)
      =gfAdd_Info('APCHECKS')  && Add the audit information to the record.
    ENDIF
    
    ** Updating records in apvendhst file. **
    *E300296,1 M.H Add the currency to the AP module.
    ** Add the currency calculations. 
    
    
    *B131145,1 EIH 03/20/2006 Fix bug of saving in wrong record of APVENHST file when we change 
	*B131145,1 EIH 03/20/2006 The payment date to the next year.[Begin]
    IF !EMPTY(lcYear)
      =SEEK(APINVHDR.cvendcode+lcYear,'APVENHST')
    ENDIF  
    *B131145,1 EIH 03/20/2006 [End]

*    REPLACE APVENHST.NVNHDISTKN WITH APVENHST.NVNHDISTKN + APINVHDR.NINVDISAP,;
            APVENHST.NVNHTOTPA  WITH APVENHST.NVNHTOTPA  + APINVHDR.NINVAMTAP,;
            APVENHST.NVNHADJ    WITH APVENHST.NVNHADJ    + APINVHDR.NINVADJAP

    REPLACE APVENHST.NVNHDISTKN WITH APVENHST.NVNHDISTKN + lnAprDisB,;
            APVENHST.NVNHTOTPA  WITH APVENHST.NVNHTOTPA  + lnAprPayB,;
            APVENHST.NVNHADJ    WITH APVENHST.NVNHADJ    + lnAprAdjB

    *E300296,1 M.H Add the currency to the AP module.
    ** Add the currency calculations. 
    IF lnPyChMN = 1        && Case calling bar is Manual check payment.
      *REPLACE APVENHST.NVNHMCHKP WITH APVENHST.NVNHMCHKP + APINVHDR.NINVAMTAP
      REPLACE APVENHST.NVNHMCHKP WITH APVENHST.NVNHMCHKP + lnAprPayB
    ELSE
      IF lnPyChMN = 2      && Case calling bar is Non check payment.
       *REPLACE APVENHST.NVNHNCHKP WITH APVENHST.NVNHNCHKP + APINVHDR.NINVAMTAP
        REPLACE APVENHST.NVNHNCHKP WITH APVENHST.NVNHNCHKP + lnAprPayB
      ELSE                 && Case calling bar is Cash payment.
       *REPLACE APVENHST.NVNHCASHP WITH APVENHST.NVNHCASHP + APINVHDR.NINVAMTAP
        REPLACE APVENHST.NVNHCASHP WITH APVENHST.NVNHCASHP + lnAprPayB
      ENDIF
    ENDIF
            
    *E300296,1 M.H Add the currency to the AP module.
    ** Add the currency calculations. 
    IF !EMPTY(lcPeriod)
      lcfield="NVNHPAY"+ALLTRIM(STR(VAL(lcPeriod)))
     *REPLACE APVENHST.&lcfield WITH APVENHST.&lcfield + APINVHDR.NINVAMTAP
      REPLACE APVENHST.&lcfield WITH APVENHST.&lcfield + lnAprPayB
    ENDIF  

    =gfAdd_Info('APVENHST')  && Add the audit information to the record.

    ** Add to Distrebution File **
    *E301077,79 AMM Open file
    *SELECT APDIST
    =gfOpenFile(gcDataDir+'APDIST','INVVEND','SH')
    *E301077,79 AMM end
    ** Add 1st record in distrebution file. **
    *E300296,1 M.H Add the currency to the AP module.
    ** Add the currency calculations. 

    APPEND BLANK
*    REPLACE CVENDCODE WITH lcVendCode,;
            CINVNO    WITH APINVHDR.CINVNO,;
            DAPDTRDAT WITH ldPayDat,;
            LAPDPOST  WITH .F.,;
            CAPDGLACT WITH APINVHDR.CAPACCT,;
            NAPDAMNT  WITH (APINVHDR.NINVAMTAP+APINVHDR.NINVDISAP+APINVHDR.NINVADJAP),;
            CAPDACTID WITH "A",;
            CFISFYEAR WITH lcYear,;
            CFSPPRDID WITH lcPeriod,;
            CAPSESSNO WITH lcSesNum,;
            CBNKCODE  WITH lcBankCode,;  
            CCHKACCT  WITH lcCheckcode,;
            CAPDTRTYP WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
            CAPDREF   WITH IIF(lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0'))

    *E300296,1 M.H Add the currency to the AP module.
    * Add the Eq. (APINVHDR.NINVAMTAP * APINVHDR.NEXRATE / APINVHDR.NCURRUNIT + lnAprDisB + lnAprAdjB )
    * Add the inv Currency,Rate,Unit
    *E300316,1 HISH  11/30/95. Used the variables hold signs in the equation. (Begin)
    REPLACE CVENDCODE WITH lcVendCode,;
            CINVNO    WITH APINVHDR.CINVNO,;
            DAPDTRDAT WITH ldPayDat,;
            LAPDPOST  WITH .F.,;
            CAPDGLACT WITH APINVHDR.CAPACCT,;
            NAPDAMNT  WITH (APINVHDR.NINVAMTAP+APINVHDR.NINVDISAP+APINVHDR.NINVADJAP),;
            CAPDACTID WITH "A",;
            CFISFYEAR WITH lcYear,;
            CFSPPRDID WITH lcPeriod,;
            CAPSESSNO WITH lcSesNum,;
            CBNKCODE  WITH lcBankCode,;  
            CCHKACCT  WITH lcCheckcode,;
            CAPDTRTYP WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
            CAPDREF   WITH IIF(lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0')),;
            CCURRCODE WITH APINVHDR.CCURRCODE,;
            NEXRATE   WITH APINVHDR.NEXRATE,;
            NCURRUNIT WITH APINVHDR.NCURRUNIT,;
            NEQVAMNT  WITH ROUND((APINVHDR.NINVAMTAP &lcExSin1 APINVHDR.NEXRATE;
                           &lcExSin2 APINVHDR.NCURRUNIT + lnAprDisB + lnAprAdjB ),2)
            
            *NEQVAMNT  WITH ROUND((APINVHDR.NINVAMTAP * APINVHDR.NEXRATE / APINVHDR.NCURRUNIT + lnAprDisB + lnAprAdjB ),2)
    *E300316,1 (End)            

    =gfAdd_Info('APDIST')  && Add the audit information to the record.

    ** Add 2nd record for the paid 1099 amount in distrebution file. **

    *B601957,1 Change this line to add the paid 1099 amount in distribution
    *file in the debit memo case as well as the invoice case [Begin]
    
    *IF APINVHDR.NINVA1099 > 0
    
    *B601957,1 If there is a paid 1099 amount
    IF APINVHDR.NINVA1099 <> 0
    
    *B601957,1 Change this line to add the paid 1099 amount [End]
      
      APPEND BLANK
     * REPLACE CVENDCODE WITH lcVendCode,;
              CINVNO    WITH APINVHDR.CINVNO,;
              DAPDTRDAT WITH ldPayDat,;
              LAPDPOST  WITH .F.,;
              CAPDSTAT  WITH "V",;
              CAPDGLACT WITH " ",;
              NAPDAMNT  WITH APINVHDR.NINVA1099,;
              CAPDACTID WITH "B",;
              CFISFYEAR WITH lcYear,;
              CFSPPRDID WITH lcPeriod,;
              CAPSESSNO WITH lcSesNum,;
              CBNKCODE  WITH lcBankCode,;  
              CCHKACCT  WITH lcCheckcode,;
              CAPDTRTYP WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
              CAPDREF   WITH IIF(lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0'))

      *E300296,1 M.H Add the currency to the AP module.
      * Add the Eq. lnApr1099B
      * Add the inv Currency,Rate,Unit
      REPLACE CVENDCODE WITH lcVendCode,;
              CINVNO    WITH APINVHDR.CINVNO,;
              DAPDTRDAT WITH ldPayDat,;
              LAPDPOST  WITH .F.,;
              CAPDSTAT  WITH "V",;
              CAPDGLACT WITH " ",;
              NAPDAMNT  WITH APINVHDR.NINVA1099,;
              CAPDACTID WITH "B",;
              CFISFYEAR WITH lcYear,;
              CFSPPRDID WITH lcPeriod,;
              CAPSESSNO WITH lcSesNum,;
              CBNKCODE  WITH lcBankCode,;  
              CCHKACCT  WITH lcCheckcode,;
              CAPDTRTYP WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
              CAPDREF   WITH IIF(lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0')),;
              CCURRCODE WITH APINVHDR.CCURRCODE,;
              NEXRATE   WITH APINVHDR.NEXRATE,;
              NCURRUNIT WITH APINVHDR.NCURRUNIT,;
              NEQVAMNT  WITH APINVHDR.NINVA1099

      =gfAdd_Info('APDIST')  && Add the audit information to the record.
    ENDIF

    ** Add 3rd record in distrebution file. **
    APPEND BLANK
*   REPLACE CVENDCODE WITH lcVendCode,;
            CINVNO    WITH APINVHDR.CINVNO,;
            DAPDTRDAT WITH ldPayDat,;
            LAPDPOST  WITH .F.,;
            CAPDGLACT WITH APINVHDR.CCHKGLACC,;
            NAPDAMNT  WITH -APINVHDR.NINVAMTAP,;
            CAPDACTID WITH "C",;
            CFISFYEAR WITH lcYear,;
            CFSPPRDID WITH lcPeriod,;
            CAPSESSNO WITH lcSesNum,;
            CBNKCODE  WITH lcBankCode,;  
            CCHKACCT  WITH lcCheckcode,;
            CAPDTRTYP WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
            CAPDREF   WITH IIF(lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0'))

    *E300296,1 M.H Add the currency to the AP module.
    * Add the Eq. lnAprPayB
    * Add the checking Currency,Rate,Unit
    *E300316,1 HISH  11/30/95. Used the variables hold signs in the equation. (Begin)
    *REPLACE CVENDCODE WITH lcVendCode,;
            CINVNO    WITH APINVHDR.CINVNO,;
            DAPDTRDAT WITH ldPayDat,;
            LAPDPOST  WITH .F.,;
            CAPDGLACT WITH APINVHDR.CCHKGLACC,;
            NAPDAMNT  WITH -APINVHDR.NINVAMTAP*APINVHDR.NAPREXRAT/APINVHDR.NAPRCURUNT,;
            CAPDACTID WITH "C",;
            CFISFYEAR WITH lcYear,;
            CFSPPRDID WITH lcPeriod,;
            CAPSESSNO WITH lcSesNum,;
            CBNKCODE  WITH lcBankCode,;  
            CCHKACCT  WITH lcCheckcode,;
            CAPDTRTYP WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
            CAPDREF   WITH IIF(lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0')),;
            CCURRCODE WITH APINVHDR.CAPRCURCOD,;
            NEXRATE   WITH lnChkExRat,;
            NCURRUNIT WITH lnChkExUnt,;
            NEQVAMNT  WITH -lnAprPayB

    *B601526,1 Change this line (Replace the calculation using lcExSin3  
    *          and lcExSin4 with the new field ) [Begin]
    *REPLACE CVENDCODE WITH lcVendCode,;
    *        CINVNO    WITH APINVHDR.CINVNO,;
    *        DAPDTRDAT WITH ldPayDat,;
    *        LAPDPOST  WITH .F.,;
    *        CAPDGLACT WITH APINVHDR.CCHKGLACC,;
    *        NAPDAMNT  WITH -APINVHDR.NINVAMTAP &lcExSin3 APINVHDR.NAPREXRAT;
    *                        &lcExSin4 APINVHDR.NAPRCURUNT,;
    *        CAPDACTID WITH "C",;
    *        CFISFYEAR WITH lcYear,;
    *        CFSPPRDID WITH lcPeriod,;
    *        CAPSESSNO WITH lcSesNum,;
    *        CBNKCODE  WITH lcBankCode,;  
    *        CCHKACCT  WITH lcCheckcode,;
    *        CAPDTRTYP WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
    *        CAPDREF   WITH IIF(lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0')),;
    *        CCURRCODE WITH APINVHDR.CAPRCURCOD,;
    *        NEXRATE   WITH lnChkExRat,;
    *        NCURRUNIT WITH lnChkExUnt,;
    *        NEQVAMNT  WITH -lnAprPayB            

    REPLACE CVENDCODE WITH lcVendCode,;
            CINVNO    WITH APINVHDR.CINVNO,;
            DAPDTRDAT WITH ldPayDat,;
            LAPDPOST  WITH .F.,;
            CAPDGLACT WITH APINVHDR.CCHKGLACC,;
            NAPDAMNT  WITH -APINVHDR.NINVFAAP,;
            CAPDACTID WITH "C",;
            CFISFYEAR WITH lcYear,;
            CFSPPRDID WITH lcPeriod,;
            CAPSESSNO WITH lcSesNum,;
            CBNKCODE  WITH lcBankCode,;  
            CCHKACCT  WITH lcCheckcode,;
            CAPDTRTYP WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
            CAPDREF   WITH IIF(lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0')),;
            CCURRCODE WITH APINVHDR.CAPRCURCOD,;
            NEXRATE   WITH lnChkExRat,;
            NCURRUNIT WITH lnChkExUnt,;
            NEQVAMNT  WITH -lnAprPayB            

    *B601526,1 Change this line [End]
    
    *E300316,1 (End) 

    =gfAdd_Info('APDIST')  && Add the audit information to the record.
      
    ** Add 4th record in distrebution file. in case of invoice discount>0 **
    
    *B601957,1 Change this line to add the invoice discount in distribution
    *file in the debit memo case as well as the invoice case [Begin]
    
    *IF APINVHDR.NINVDISAP > 0
    
    *B601957,1 If there is a discount
    IF APINVHDR.NINVDISAP <> 0
    
    *B601957,1 Change this line to add the invoice discount [End]
      
      lcApdGlAct=IIF(lnPyChMN = 3,;
                    IIF(SEEK(APINVHDR.CDIVISION,'APDIV');
                    AND !EMPTY(APDIV.CDISCACCT),;
                    APDIV.CDISCACCT,APSETUP.CDISCACCT),;
                    IIF(SEEK(APINVHDR.CBNKCODE+APINVHDR.CCHKACCT,'APCHECKS');
                       AND !EMPTY (APCHECKS.CDISCACCT),;
                       APCHECKS.CDISCACCT,;
                       IIF(SEEK(APINVHDR.CDIVISION,'APDIV') AND !EMPTY(APDIV.CDISCACCT),;
                       APDIV.CDISCACCT,APSETUP.CDISCACCT)))

      APPEND BLANK       
      *REPLACE CVENDCODE  WITH lcVendCode,;
              CINVNO     WITH APINVHDR.CINVNO,;
              DAPDTRDAT  WITH ldPayDat,;
              LAPDPOST   WITH .F.,;
              NAPDAMNT   WITH -APINVHDR.NINVDISAP,;
              CAPDACTID  WITH "S",;
              CFISFYEAR  WITH lcYear,;
              CFSPPRDID  WITH lcPeriod,;
              CAPSESSNO  WITH lcSesNum,;
              CBNKCODE   WITH lcBankCode,;  
              CCHKACCT   WITH lcCheckcode,;
              CAPDTRTYP  WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
              CAPDREF    WITH IIF(lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0')),;
              CAPDGLACT  WITH lcApdGlAct 

      *E300296,1 M.H Add the currency to the AP module.
      * Add the Eq. lnAprDisB
      * Add the inv Currency,Rate,Unit
      REPLACE CVENDCODE  WITH lcVendCode,;
              CINVNO     WITH APINVHDR.CINVNO,;
              DAPDTRDAT  WITH ldPayDat,;
              LAPDPOST   WITH .F.,;
              NAPDAMNT   WITH -APINVHDR.NINVDISAP,;
              CAPDACTID  WITH "S",;
              CFISFYEAR  WITH lcYear,;
              CFSPPRDID  WITH lcPeriod,;
              CAPSESSNO  WITH lcSesNum,;
              CBNKCODE   WITH lcBankCode,;  
              CCHKACCT   WITH lcCheckcode,;
              CAPDTRTYP  WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
              CAPDREF    WITH IIF(lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0')),;
              CAPDGLACT  WITH lcApdGlAct ,;
              CCURRCODE  WITH APINVHDR.CCURRCODE,;
              NEXRATE    WITH APINVHDR.NEXRATE,;
              NCURRUNIT  WITH APINVHDR.NCURRUNIT,;
              NEQVAMNT   WITH -lnAprDisB

      =gfAdd_Info('APDIST')  && Add the audit information to the record.
    ENDIF
      
    ** Add 5th record in distrebution file. in case of invoice adjustment>0 **
    
    *B601957,1 Change this line to add the invoice adjustment in distribution
    *file in the debit memo case as well as the invoice case [Begin]
    
    *IF APINVHDR.NINVADJAP > 0
    
    *B601957,1 If there is an adjustment
    IF APINVHDR.NINVADJAP <> 0
    
    *B601957,1 Change this line to add the invoice adjustment [End]
      
      lcApdGlAct=IIF(lnPyChMN = 3,;
                    IIF(SEEK(APINVHDR.CDIVISION,'APDIV');
                    AND !EMPTY(APDIV.CADJACCT),;
                    APDIV.CADJACCT,APSETUP.CADJACCT),;
                    IIF(SEEK(APINVHDR.CBNKCODE+APINVHDR.CCHKACCT,'APCHECKS');
                       AND !EMPTY (APCHECKS.CADJACCT),;
                       APCHECKS.CADJACCT,;
                       IIF(SEEK(APINVHDR.CDIVISION,'APDIV') AND !EMPTY(APDIV.CADJACCT),;
                       APDIV.CADJACCT,APSETUP.CADJACCT)))
    
       APPEND BLANK       
       *REPLACE CVENDCODE  WITH lcVendCode,;
               CINVNO     WITH APINVHDR.CINVNO,;
               DAPDTRDAT  WITH ldPayDat,;
               LAPDPOST   WITH .F.,;
               NAPDAMNT   WITH -APINVHDR.NINVADJAP,;
               CAPDACTID  WITH "J",;
               CFISFYEAR  WITH lcYear,;
               CFSPPRDID  WITH lcPeriod,;
               CAPSESSNO  WITH lcSesNum,;
               CBNKCODE   WITH lcBankCode,;  
               CCHKACCT   WITH lcCheckcode,;
               CAPDTRTYP  WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
               CAPDREF    WITH IIF(lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0')),;
               CAPDGLACT  WITH lcApdGlAct 

       *E300296,1 M.H Add the currency to the AP module.
       * Add the Eq. lnAprAdjB
       * Add the inv Currency,Rate,Unit
       REPLACE CVENDCODE  WITH lcVendCode,;
               CINVNO     WITH APINVHDR.CINVNO,;
               DAPDTRDAT  WITH ldPayDat,;
               LAPDPOST   WITH .F.,;
               NAPDAMNT   WITH -APINVHDR.NINVADJAP,;
               CAPDACTID  WITH "J",;
               CFISFYEAR  WITH lcYear,;
               CFSPPRDID  WITH lcPeriod,;
               CAPSESSNO  WITH lcSesNum,;
               CBNKCODE   WITH lcBankCode,;  
               CCHKACCT   WITH lcCheckcode,;
               CAPDTRTYP  WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
               CAPDREF    WITH IIF(lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0')),;
               CAPDGLACT  WITH lcApdGlAct ,;
               CCURRCODE  WITH APINVHDR.CCURRCODE,;
               NEXRATE    WITH APINVHDR.NEXRATE,;
               NCURRUNIT  WITH APINVHDR.NCURRUNIT,;
               NEQVAMNT   WITH -lnAprAdjB

       =gfAdd_Info('APDIST')  && Add the audit information to the record.
    ENDIF
    *E300316,1 HISH  11/30/95. Used the variables hold signs in the equation. (Begin)
    *lnExchDiff = lnAprPayB - ROUND(APINVHDR.NINVAMTAP * APINVHDR.NEXRATE / APINVHDR.NCURRUNIT,2)
    lnExchDiff = lnAprPayB - ROUND(APINVHDR.NINVAMTAP &lcExSin1 APINVHDR.NEXRATE;
                             &lcExSin2 APINVHDR.NCURRUNIT,2)
    *E300316,1 (End)
    *E300296,1 M.H Add the currency to the AP module.
    * Add the Eq. lnExchDiff
    * Add the base Currency,Rate = 1,Unit= 1
    IF lnExchDiff <> 0
      APPEND BLANK       

      *B601589,1 Change this line [Begin]
      *REPLACE CVENDCODE  WITH lcVendCode,;
      *        CINVNO     WITH APINVHDR.CINVNO,;
      *        DAPDTRDAT  WITH ldPayDat,;
      *        LAPDPOST   WITH .F.,;
      *        NAPDAMNT   WITH lnExchDiff,;
      *        CAPDACTID  WITH "J",;
      *        CFISFYEAR  WITH lcYear,;
      *        CFSPPRDID  WITH lcPeriod,;
      *        CAPSESSNO  WITH lcSesNum,;
      *        CBNKCODE   WITH lcBankCode,;  
      *        CCHKACCT   WITH lcCheckcode,;
      *        CAPDTRTYP  WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
      *        CAPDREF    WITH IIF(lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0')),;
      *        CAPDGLACT  WITH lcExDifAcc,;
      *        CCURRCODE  WITH gcBaseCurr,;
      *        NEXRATE    WITH 1,;
      *        NCURRUNIT  WITH 1,;
      *        NEQVAMNT   WITH lnExchDiff

      REPLACE CVENDCODE  WITH lcVendCode,;
              CINVNO     WITH APINVHDR.CINVNO,;
              DAPDTRDAT  WITH ldPayDat,;
              LAPDPOST   WITH .F.,;
              NAPDAMNT   WITH lnExchDiff,;
              CAPDACTID  WITH "J",;
              CFISFYEAR  WITH lcYear,;
              CFSPPRDID  WITH lcPeriod,;
              CAPSESSNO  WITH lcSesNum,;
              CBNKCODE   WITH lcBankCode,;  
              CCHKACCT   WITH lcCheckcode,;
              CAPDTRTYP  WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
              CAPDREF    WITH IIF(lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0')),;
              CAPDGLACT  WITH lcExDifAcc,;
              CCURRCODE  WITH gcBaseCurr,;
              NEXRATE    WITH 1,;
              NCURRUNIT  WITH 1,;
              NEQVAMNT   WITH lnExchDiff,;
              nApdLinNo  WITH 1
              
      *B601589,1 Change this line [End]
      

      =gfAdd_Info('APDIST')  && Add the audit information to the record.
    ENDIF
    *E300296,1 M.H Add the currency to the AP module.
    * Clear The aproved Unit,Rate
    SELECT APINVHDR
*B600970,1 M.H 02/06/96 Update the last check #,last payment date fields.
*    REPLACE NINVPAID   WITH NINVPAID  + NINVAMTAP,;
            NINVDISTK  WITH NINVDISTK + NINVDISAP,;
            NINVADJ    WITH NINVADJ   + NINVADJAP,;
            NINV1099A  WITH NINV1099A + NINVA1099,;
            CBNKCODE   WITH "",;
            CCHKACCT   WITH "",;
            CCHKGLACC  WITH "",;
            NINVAMTAP  WITH 0,;
            NINVDISAP  WITH 0,;
            NINVADJAP  WITH 0,;
            NINVA1099  WITH 0,;
            CAPRCURCOD WITH '',;
            NAPREXRAT  WITH 0,;
            NAPRCURUNT WITH 0

    *B601526,1 Add the new field (nInvFAAp) [Begin]   
    *REPLACE NINVPAID   WITH NINVPAID  + NINVAMTAP,;
    *        NINVDISTK  WITH NINVDISTK + NINVDISAP,;
    *        NINVADJ    WITH NINVADJ   + NINVADJAP,;
    *        NINV1099A  WITH NINV1099A + NINVA1099,;
    *        CBNKCODE   WITH "",;
    *        CCHKACCT   WITH "",;
    *        CCHKGLACC  WITH "",;
    *        NINVAMTAP  WITH 0,;
    *        NINVDISAP  WITH 0,;
    *        NINVADJAP  WITH 0,;
    *        NINVA1099  WITH 0,;
    *        CAPRCURCOD WITH '',;
    *        NAPREXRAT  WITH 0,;
    *        NAPRCURUNT WITH 0,;
    *        DCHKDATE  WITH ldPayDat,;
    *        CCHKNO    WITH IIF(lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0'))

    REPLACE NINVPAID   WITH NINVPAID  + NINVAMTAP,;
            NINVDISTK  WITH NINVDISTK + NINVDISAP,;
            NINVADJ    WITH NINVADJ   + NINVADJAP,;
            NINV1099A  WITH NINV1099A + NINVA1099,;
            CBNKCODE   WITH "",;
            CCHKACCT   WITH "",;
            CCHKGLACC  WITH "",;
            NINVAMTAP  WITH 0,;
            NINVDISAP  WITH 0,;
            NINVADJAP  WITH 0,;
            NINVA1099  WITH 0,;
            CAPRCURCOD WITH '',;
            NAPREXRAT  WITH 0,;
            NAPRCURUNT WITH 0,;
            DCHKDATE  WITH ldPayDat,;
            CCHKNO    WITH IIF(lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0')),;
            nInvFAAp  WITH 0
            
    *B601526,1 Add the new field (nInvFAAp) [Begin]   

*B600970,1 M.H End.

    lnAprPayB  = 0
    lnAprDisB  = 0
    lnAprAdjB  = 0
    lnApr1099B = 0

    =gfAdd_Info('APINVHDR')  && Add the audit information to the record.
    =gfObj_lock(.F.) && unlock the pointed record. 
  ENDFOR
  ***Remove paid record from selected array.
  DIMENSION laSelect[1,2]  
  laSelect = " "
  lnArLen  = 1
  CLEAR READ
ELSE && if date is invalid or can not lock any record.
  *** Release locking. ***
  SELECT APINVHDR
  FOR lnRecCount= 1 TO lnArCount-1 
    GO laSelect[lnRecCount,2] && go to the seleced record.
    =gfObj_lock(.F.) && unlock the pointed record. 
  ENDFOR  
ENDIF  

*!**************************************************************************
*!
*!      Function: lfVSelCan
*!
*!**************************************************************************
* Get the old next check number if there is no checks added in between adding and cancelling the check no.
*
FUNCTION lfVSelCan

IF lnPyChMN = 1  && Calling from manual check.
  SELECT APCHECKS
  ***if the next check no in the file is the same with next check number entered. ***
  IF SEEK(lcBankCode+lcCheckCode,'APCHECKS') .AND. APCHECKS.NCHKNXTMN = lnGnChkNm
    *B800072,1 We do not need to lock the record because it is already lo
    *IF gfObj_lock(.T.) && lock the pointed record. &&>>> 
    IF (ALLTRIM(cLok_User) = ALLTRIM(gcUser_ID)) .OR. gfObj_lock(.T.) && lock the pointed record.
      REPLACE APCHECKS.NCHKNXTMN WITH lnOChkNum
      =gfAdd_Info()  && Add the audit information to the record.
    ENDIF   
  ENDIF 
  SELECT APINVHDR
ENDIF
CLEAR READ
GO TOP

*!**************************************************************************
*!
*!      Function: lfwPayDat
*!
*!**************************************************************************
*Save old payment 
FUNCTION lfwPayDat

ldOldPDat = ldPayDat 

*!**************************************************************************
*!
*!      Function: lfvScope
*!
*!**************************************************************************
* set scope expersion filter.
FUNCTION lfvScope

*** If you want to display all invoices. ***
IF rbScope = 1

  *** Fill expresion with inetial filter. ***
  lcExprsion = "(ABS(NINVAMTAP)+ABS(NINVDISAP)+ABS(NINVADJAP)>0).AND."+ ;
               "(NINVAMNT-NINVPAID-NINVDISTK-NINVADJ<> 0).AND."+ ;
               "CVENPRIOR<> '0' .AND. CINVSTAT<> 'V'"
  
  IF lnPyChMN = 3
    *E300296,1 M.H   10/10/95 Add the currency to the AP module.
    *E300296,1 M.H Add the vendor to the currency to the filter exp.
    *lcExprsion = lcExprsion +" .AND. CVENPMETH = 'H'"
    
    *B601013,1 Change filter expression as to filter on approved currency.
    *lcExprsion = lcExprsion +" .AND. CVENPMETH = 'H' .AND. APINVHDR.CCURRCODE = lcCurrCode"

    *B601217,1 Change this line to use the new varible lcCurCod [Begin]
    *lcExprsion = lcExprsion +" .AND. CVENPMETH = 'H' .AND. APINVHDR.cAprCurCod = lcCurrCode"
    lcExprsion = lcExprsion +" .AND. CVENPMETH = 'H' .AND. APINVHDR.cAprCurCod = lcCurCod"
    *B601217,1 Change this line to use the new varible lcCurCod [End]

    *B601013,1 end.
  ELSE

    *B601217,1 Change this line to use the new varibles lcBnkCod
    *          , lcChekCod [Begin]
    *ahmed
    *lcExprsion = lcExprsion +" .AND. CVENPMETH <>'H' .AND."+;
                 "CBNKCODE+CCHKACCT=lcBankCode+lcCheckcode"

    lcExprsion = lcExprsion +" .AND. CVENPMETH <>'H' .AND."+;
                 "CBNKCODE+CCHKACCT=lcBnkCod+lcChekCod"

    *B601217,1 Change this line to use the new varibles [Begin]

  ENDIF

ELSE && If the user want to select certain invoices.

  ON KEY && restore the keys traped
  *** ON HIGHER LEVEL CREATE VARIABLES TO HOLD OLD VAR. ***
  *** SAVE CURRENT VARIABLES IN OLD VAR.
  RELEASE PAD _BROWSE OF _MSYSMENU
  lcOldExpr  =  lcExprsion

  *E300683,1 Call *.SPR from screens directory
  * DO APSCOPE.SPR 
  DO (gcScrDir + gcWinAppl + '\APSCOPE.SPR')
  *E300683,1 end          

  ***Save Scope to be an old value***
  ldOToDisDt = ldToDisDat
  ldOFrDisDt = ldFrDisDat
  ldOToDueDt = ldToDueDat
  ldOFrDueDt = ldFrDueDat
  lcOldPrty  = lcPriority 
  lcOVendCod = lcVendCode 
  lcOldVnCmp = lcVendComp
  lcOMethod  = lcMethod
  lcOldDvCod = lcDivCode  
  lcOldpymth = lcPayMeth
  lcOldInvRf = lcInvRef
  rbODuDsDt  = rbDuDsDt

  *** Push the same keys again ***
  =lfPushKey()
  IF llScope            && if the user push <ok> to do the filter he selected.
    llScope = .F.
  ENDIF
ENDIF  
lnBrRecNo  = RECNO('APINVHDR')

*B801208,4 AMM start
*GO TOP  && Refresh browse filter 
*B801208,4 AMM end

=lfShowDs(.T.,.T.)  && Show all control enable or disable.
lnOScope = rbScope

*!**************************************************************************
*!
*!      Function: lfvVendor
*!
*!**************************************************************************
* Activate verndor window.
*
FUNCTION lfvVendor

** Get Y.T.D. Payments from vendor history.
SELECT APVENHST
lcSavOrd = ORDER() 
SET ORDER TO VENDYEAR

IF SEEK(lcVendCode+lcCurYear)
  lnVnTotPay=NVNHTOTPA
ENDIF 

IF !EMPTY(lcSavOrd)
  SET ORDER TO &lcSavOrd
ENDIF  

SELECT APINVHDR
** Get Tot Payment, Discount, 1099 pay.
=lfvTot()
** Refresh the invoice screen with values. 
=lfRefresh()
CLEAR TYPEAHEAD && Clear keyboard butter.
** Call the child screen through the global function activate window. ***
=gfActWind(lcVenSum,lcVendTtl,gcBaseWind)

SELECT APINVHDR

*!**************************************************************************
*!
*!      Function: lfvInvoice
*!
*!**************************************************************************
* Activate invoice window.
*
FUNCTION lfvInvoice

** Refresh the invoice screen with values. 
=lfRefresh()
CLEAR TYPEAHEAD && Clear keyboard butter.
** Call the child screen through the global function activate window. **
=gfActWind(lcInvSum,lcInvoice,gcBaseWind)

*!**************************************************************************
*!
*!      Function: lfvTot
*!
*!**************************************************************************
* Get total all selected approve amount,discount,adjustment,1099. ***
* calling from pay selected invoices and vendor screens.***
*
FUNCTION lfvTot

SELECT APINVHDR
lnTotPay  = 0
lnTotDisc = 0
lnTotAdj  = 0
lnTot1099 = 0
lnRecNo   = RECNO() && get the record no.
lnArCount = ALEN(laSelect,1)    
IF lnArCount > 1 && if the array is not empty
  FOR lnRecCount = 1 TO lnArCount-1        
    GO laSelect[lnRecCount,2]
    
    *E300316,1 HISH  11/30/95. Got the equantion signs. (Begin)
    *E300316,1 to exchange currency from invoice currency to company base currency.
    *E300316,1 HISH 01/08/96  Passed pointer parameter to get Unit sgin. (Begin)
    lcExSin2 = ' '
    lcExSin1 = gfGetExSin(@lcExSin2,cCurrCode)
    *lcExSin2 = IIF(lcExSin1 = '*','/','*')
    *E300316,1 to exchange currency from invoice currency to approved currency.
    lcExSin4 = ' '

    *B601526,1 Ther is no need for this lines any more [Begin]
    
    *B601416,1 Change the calling of gfGetExSin so if the [BEGIN]
    * Invoice currency is the base currency I make it the To currency 
    * and if not the Approve currency is the To currency 
    *lcExSin3 = gfGetExSin(@lcExSin4,cCurrCode,cAprCurCod)
    *B601416,1 IF Statment to check if the Invoice currency is the 
    * same as the base currency
     *IF cCurrCode = gcBaseCurr 
     *  lcExSin3 = gfGetExSin(@lcExSin4,cAprCurCod,cCurrCode)
     *  lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
     *  lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
     *ELSE   && Else
     * lcExSin3 = gfGetExSin(@lcExSin4,cCurrCode,cAprCurCod)
     *ENDIF  && End of IF 
    *B601416,1 Change the calling of gfGetExSin [END]

    *B601526,1 Ther is no need for this lines any more [End]

    *lcExSin4 = IIF(lcExSin3 = '*','/','*')
    *E300316,1 (End)
    
    *E300296,1 M.H Sum The Approved amounts for payment using the Checking account curr.
    
    *E300316,1 HISH  11/30/95. Used the variables hold signs in the equation. (Begin)
    *lnTotPay  = lnTotPay  + ROUND(APINVHDR.NINVAMTAP * APINVHDR.NAPREXRAT / APINVHDR.NAPRCURUNT,2)

    *B601526,1 Change this line (Replace the calculation using lcExSin3  
    *          and lcExSin4 with the new field ) [Begin]
    *lnTotPay  = lnTotPay  + ROUND(APINVHDR.NINVAMTAP &lcExSin3 APINVHDR.NAPREXRAT &lcExSin4 APINVHDR.NAPRCURUNT,2)
    lnTotPay  = lnTotPay  + ROUND(APINVHDR.NINVFAAP , 2)
    *B601526,1 Change this line [End]
    
    *E300316,1 (End)
    * Sum all other approved amounts using the base currency
    *E300316,1 HISH  11/30/95. Used the variables hold signs in the equation. (Begin)
    *lnTotDisc = lnTotDisc + ROUND(APINVHDR.NINVDISAP * APINVHDR.NEXRATE / APINVHDR.NCURRUNIT,2)
    lnTotDisc = lnTotDisc + ROUND(APINVHDR.NINVDISAP &lcExSin1;
                            APINVHDR.NEXRATE &lcExSin2 APINVHDR.NCURRUNIT,2)
    *lnTotAdj  = lnTotAdj  + ROUND(APINVHDR.NINVADJAP * APINVHDR.NEXRATE / APINVHDR.NCURRUNIT,2)
    lnTotAdj  = lnTotAdj  + ROUND(APINVHDR.NINVADJAP &lcExSin1 APINVHDR.NEXRATE;
                            &lcExSin2 APINVHDR.NCURRUNIT,2)
    *lnTot1099 = lnTot1099 + ROUND(APINVHDR.NINV1099A * APINVHDR.NEXRATE / APINVHDR.NCURRUNIT,2)

    *B601599,1 Exchange NINV1099A field with NINVA1099 field
    *lnTot1099 = lnTot1099 + ROUND(APINVHDR.NINV1099A &lcExSin1;
    *                        APINVHDR.NEXRATE &lcExSin2 APINVHDR.NCURRUNIT,2)
    lnTot1099 = lnTot1099 + ROUND(APINVHDR.NINVA1099 &lcExSin1;
                            APINVHDR.NEXRATE &lcExSin2 APINVHDR.NCURRUNIT,2)
    *B601599,1 Change this line to use the correct field [End]  
    *E300316,1 (End)
  ENDFOR
ENDIF  

IF lnRecNo <= RECCOUNT()  && be sure that the is not empty.
  GO lnRecNo      && go to the last record pointed.
ENDIF

=lfRefresh()

*!**************************************************************************
*!
*!      Function: lfvclose
*!
*!**************************************************************************
*
FUNCTION lfvclose

IF WONTOP(lcVenSum)
  =gfChClose()
  RETURN
ELSE
  IF WONTOP(lcInvSum)
    =gfChClose()
    RETURN
  ENDIF
ENDIF  

*** Release locking. ***
SELECT APINVHDR

lnArCount = ALEN(laSelect,1) 
** Release locking if exists.
FOR lnRecCount= 1 TO lnArCount-1 
  =gfThermo(lnArCount-1,lnRecCount,"Unselecting...","Vendor: "+CVENDCODE+"  Invoice: "+CINVNO)
  GO laSelect[lnRecCount,2] && go to the seleced record.
  =gfObj_lock(.F.) && unlock the pointed record. 
ENDFOR

IF !WONTOP(lcBrTtl)
  ** Close program. **
  glQuitting = .T.
  CLEAR READ
ELSE && IF EXIT FROM BROWSE WINDOW
  HIDE WINDOW(gcBaseWind)
  ** Close program. **
  glFromBrow = .F.
  glQuitting = .T.
  CLEAR READ
  KEYBOARD CHR(13)
  RETURN TO APMNCHP.SPR
ENDIF  

*!**************************************************************************
*!
*!      Function: lfDefBank
*!
*!**************************************************************************
* Get default bank and checks.
*
FUNCTION lfDefBank

*** set default bank and check ***
IF lnPyChMN <>  3 && if not calling from cash payment
  IF !EMPTY(lcVendCode)
    IF lcVendCode <> lcOldVndcd
      DO CASE
        CASE SEEK(lcVendCode,'APVENDOR') .AND. !EMPTY(APVENDOR.CBNKCODE) && if found values in vendor file.
            lcBankCode = APVENDOR.CBNKCODE
            lcCheckCode= APVENDOR.CCHKACCT
        CASE !EMPTY(APDIV.CBNKCODE)    && if found values in division file.
          lcBankCode = APDIV.CBNKCODE
          lcCheckCode= APDIV.CCHKACCT
        OTHERWISE                      && get value from setup file.
          lcBankCode = APSETUP.CBNKCODE
          lcCheckCode= APSETUP.CCHKACCT
      ENDCASE
      
      *B601217,1 Add this lines to use the new varibles lcBnkCod
      *           , lcChekCod [Begin]
      lcBnkCod = lcBankCode
      lcChekCod = lcCheckCode
      *B601217,1 Add this lines to use the new varibles [End]

    ENDIF  
    _CUROBJ=OBJNUM(lcBankCode)
  ENDIF  
ENDIF  

lcObjDisp = IIF(EMPTY(lcBankCode), "DISABLE", "ENABLE")

SELECT APINVHDR

*!**************************************************************************
*!
*!      Function: lfwVend
*!
*!**************************************************************************
* Save old vendor code.
*
FUNCTION lfwVend
lcOldVal   = EVALUATE(SYS(18))
lcOldVndcd = lcVendCode

*!**************************************************************************
*!
*!      Function: lfvVend
*!
*!**************************************************************************
*
FUNCTION lfvVend

IF lfSekVnd('APVENDOR', lcOldVal, llBrowse)

  *E300296,1 M.H Add the currency to the AP module.
  *E300296,1 M.H Default the cuurrency code by the vendor currency.
  lcCurrCode = IIF(gfGetMemVar('LLMULCURR'),APVENDOR.CCURRCODE,gcBaseCurr)
  *E300296,1 M.H End.

  *B601217,1 Add this lines to unselect the selected invoices if the Vendor
  *          code was changed [Begin]
  *B601217,1 	IF Statment to check if the Vendor code was changed
  IF lcVendCode <> lcOldVndcd
    =lfClearSel()
  ENDIF    && End of IF
  lcVendrCod = lcVendCode
  lcCurCod = lcCurrCode
  *B601217,1 Add this lines [End]

  *B601217,1 Change this line to use the new varible lcVendrCod [Begin]
  *SET FILTER TO APINVHDR.CVENDCODE+APINVHDR.CINVNO = lcVendCode .AND.;
  *            (ABS(NINVAMTAP)+ABS(NINVDISAP)+ABS(NINVADJAP)>0).AND.;
  *            CVENPRIOR<> '0' .AND. CINVSTAT<> 'V'.AND.;
  *            EVALUATE(lcExprsion)

  SET FILTER TO APINVHDR.CVENDCODE+APINVHDR.CINVNO = lcVendrCod .AND.;
              (ABS(NINVAMTAP)+ABS(NINVDISAP)+ABS(NINVADJAP)>0).AND.;
              CVENPRIOR<> '0' .AND. CINVSTAT<> 'V'.AND.;
              EVALUATE(lcExprsion)

  *B601217,1 Change this line to use the new varible lcVendrCod [End]

  IF lnPyChMN <>  3 && if not calling from cash payment
    =lfDefBank()
    SELECT APINVHDR
    
    *B801208,4 AMM start   
    *GO TOP
    *B801208,4 AMM end
    
    =lfShowDs(.F.,.T.)
  ELSE && in case of cash payment
    SELECT APINVHDR
    
    *B801208,4 AMM start  
    *GO TOP
    *B801208,4 AMM end
    
    =lfShowDs(.F.,.T.)
    
    *BEGIN B600581,1 M.H 09/19/95
    IF _WINDOWS
      _CUROBJ = OBJNUM(ibNoAprPa) 
    ENDIF  
    *END   B600581,1 M.H 09/19/95
    
  ENDIF  
  llDispbr = .T.
ENDIF
llBrowse=.F.

*!**************************************************************************
*!
*!      Function: lfShowDs
*!
*!**************************************************************************
* Control push button condition Dis/Enable.
*
FUNCTION lfShowDs
PARAMETER llIfScope,llDisBro

ON KEY LABEL TAB lnDummi = 1

IF EMPTY(lcVendCode)
  SHOW GET lcBankCode  DISABLE
  SHOW GET lcCheckCode DISABLE
  SHOW GET ibBank      DISABLE
  SHOW GET ibChecks    DISABLE
  SHOW GET ibCurrency  DISABLE
  SHOW GET lcCurrCode  DISABLE
ELSE
  SHOW GET lcBankCode  ENABLE
  SHOW GET ibBank      ENABLE
  SHOW GET lcCheckCode &lcObjDisp
  SHOW GET ibChecks    &lcObjDisp
  SHOW GET ibCurrency  ENABLE
  SHOW GET lcCurrCode  ENABLE
ENDIF

IF llDisBro = .T.
  
  *B801208,4 AMM start
  lcOldTag=TAG()
  SET ORDER TO
  LOCATE
  SET ORDER TO TAG &lcOldTag.
  *B801208,4 AMM end
  
  SHOW WINDOW (lcBrTtl) REFRESH  && refresh the browse window.
  llDispbr=.F.
ENDIF

** To go top only for the first time in browse to make high selection performance. 
*B801208,4 AMM start Add .AND. !llDisBro
IF EMPTY(APINVHDR.CINVNO)  .AND. !llDisBro
  *B801208,4 AMM Remove GO TOP , add LOCATE
  *GO TOP 
  LOCATE
*B801208,4 AMM end  
ENDIF  

IF EMPTY(APINVHDR.CINVNO) .OR. EMPTY(IIF(lnPyChMN<> 3,lcBankCode,APINVHDR.CINVNO))
  SHOW GET pbInvoice   DISABLE
  SHOW GET pbVendor    DISABLE
  SHOW GET pbSelect    DISABLE
  SHOW GET pbSelAll    DISABLE
  SHOW GET pbSelNon    DISABLE
  SHOW GET pbInvert    DISABLE
  SHOW GET pbPySlInv   DISABLE    
  lcObjectSt = 'DISABLE'
  IF !(llIfScope)
    SHOW GET rbScope DISABLE    
  ENDIF  
  IF WVISIBLE(lcVenSum) && close vendor window if it's opened.
    =gfChClose(lcVenSum)
  ENDIF  
  IF WVISIBLE(lcInvSum) && close invoice window if it's opened.
    =gfChClose(lcInvSum)
  ENDIF
ELSE && if the browse has elements.
  SHOW GET pbInvoice ENABLE
  SHOW GET pbVendor  ENABLE
  SHOW GET pbSelect  ENABLE
  SHOW GET pbSelAll  ENABLE
  SHOW GET pbSelNon  ENABLE
  SHOW GET pbInvert  ENABLE
  SHOW GET pbPySlInv ENABLE
  lcObjectSt = 'ENABLE'
  IF !(llIfScope)
    SHOW GET rbScope ENABLE    
  ENDIF  
ENDIF 
ON KEY LABEL TAB DO lpTrap

*!**************************************************************************
*!
*!      Function: lfwDivision
*!
*!**************************************************************************
*
FUNCTION lfwDivision

lcCodeFilt = 'CDIVISION '

*!**************************************************************************
*!
*!      Function: lfvDivision
*!
*!**************************************************************************
* Get division code. 
*
FUNCTION lfvDivision
DO CASE
  CASE _DOS
    lcDivCode = gfActPop(6,12,11,44,'SYCCODES','cCode_No','cDiscrep',@lcDivision)
  CASE _WINDOWS

    *E300643,1 Change this lines for the changes we have made 
    *          to SYCCODES [Begin]
    *puDiv = SYCCODES.cdiscrep
    *lcDivCode = SYCCODES.cCode_No
    puDiv = CODES.cdiscrep
    lcDivCode = CODES.cCode_No
    *E300643,1 Change this lines [End]
    
    SHOW GET puDiv
    =gfUpdate()
ENDCASE

IF _WINDOWS
  DEACTIVATE POPUP puDivision
ENDIF  

=lfRefresh()

*!**************************************************************************
*!
*!      Function: lfvparior
*!
*!**************************************************************************
* Parior not accept zero.
*
FUNCTION lfvparior

IF lcPriority = "0"
  lcPriority  = " "
  SHOW GET lcPriority
ENDIF
SHOW GET lcPriority

*!**************************************************************************
*!
*!      Function: lfvMethod
*!
*!**************************************************************************
* Activate pay method popup.
*
FUNCTION lfvMethod

DO CASE
  CASE _DOS
    ** Activate pop up and get payment method codes. ***
     lcMethod = gfActPop(8,49,13,70,'laPayMeth',2,1,@lcPayMeth)
  CASE _WINDOWS
    ** Get payment method code. ***
    lcMethod = laPayMeth[lnMethod,2]
ENDCASE

=lfRefresh()

*!**************************************************************************
*!
*!      Function: lfvFrTodat
*!
*!**************************************************************************
* Validate from through date in scope window.
*
FUNCTION lfvFrTodat
IF rbDuDsDt = 1 && in case of due date.
  ** If there is from and through date. ***
  IF !EMPTY(ldFrDueDat) .AND. !EMPTY(ldToDueDat)
    ** If from date is greater than through date.***
    IF ldFrDueDat > ldToDueDat   
      ** MESSAGE : "From Data can not be less than Through Date"
      **           "                       ® Ok ¯ 
      =gfModalGen("TRM04028B00000","DIALOG",lcTThrou+"|"+lcTFrom)      
      _CUROBJ=OBJNUM(rbDuDsDt)
    ENDIF               
  ENDIF
ELSE && in case of discount date.
  ** If there is from and through date.***
  IF !EMPTY(ldFrDisDat) .AND. !EMPTY(ldToDisDat)
    ** If from date is greater than through date.***
    IF ldFrDisDat > ldToDisDat   
      ** MESSAGE : "From Data can not be less than Through Date"
      **           "                       ® Ok ¯ 
      =gfModalGen("TRM04028B00000","DIALOG",lcTThrou+"|"+lcTFrom)
      _CUROBJ=OBJNUM(rbDuDsDt)
    ENDIF               
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function: lfvDat
*!
*!**************************************************************************
* Branch between due date and discount date in a scope wine.
*
FUNCTION lfvDat

IF rbDuDsDt = 1  && if due date.
  SHOW GET ldFrDueDat ENABLE
  SHOW GET ldToDueDat ENABLE
  ldFrDisDat = {}
  ldToDisDat = {}
  SHOW GET ldFrDisDat DISABLE
  SHOW GET ldToDisDat DISABLE
  lcgry1      = "RGB(128,128,128,128,128,128),,,,,,,,," 
  lcwht1      = "RGB(255,255,255,255,255,255),,,,,,,,,"
  lcgry2      = "RGB(192,192,192,192,192,192),,,,,,,,,"
  lcwht2      = "RGB(192,192,192,192,192,192),,,,,,,,," 
ELSE             && if discount date.
  SHOW GET ldFrDisDat ENABLE
  SHOW GET ldToDisDat ENABLE
  ldFrDueDat = {}
  ldToDueDat = {}
  SHOW GET ldFrDueDat DISABLE
  SHOW GET ldToDueDat DISABLE
  lcgry1      = "RGB(192,192,192,192,192,192),,,,,,,,," 
  lcwht1      = "RGB(192,192,192,192,192,192),,,,,,,,," 
  lcgry2      = "RGB(128,128,128,128,128,128),,,,,,,,," 
  lcwht2      = "RGB(255,255,255,255,255,255),,,,,,,,,"
ENDIF

=lfRefresh()

*!**************************************************************************
*!
*!      Function: lpShow
*!
*!**************************************************************************
*
FUNCTION lpShow

IF rbDuDsDt = 1  && if due date.
  SHOW GET ldFrDueDat ENABLE
  SHOW GET ldToDueDat ENABLE
  ldFrDisDat = {}
  ldToDisDat = {}
  SHOW GET ldFrDisDat DISABLE
  SHOW GET ldToDisDat DISABLE
ELSE             && if discount date.
  SHOW GET ldFrDisDat ENABLE
  SHOW GET ldToDisDat ENABLE
  ldFrDueDat = {}
  ldToDueDat = {}
  SHOW GET ldFrDueDat DISABLE
  SHOW GET ldToDueDat DISABLE
ENDIF

*E300643,1 Change this line for the changes we have made to (gfCodDes) [Begin]
*lcDivision = gfCodDes(lcDivCode)
lcDivision = gfCodDes(lcDivCode , 'CDIVISION')
*E300643,1 Change this line for the changes we have made to (gfCodDes) [End]

puDiv      = lcDivision

IF _DOS
  lcPayMeth = laPayMeth[AT(lcMethod,' PMNH'),1]
ELSE
  lnMethod = AT(lcMethod,' PMNH')
  SHOW GET lnMethod
  SHOW GET puDiv
ENDIF  

=lfRefresh()

SELECT APINVHDR

*!**************************************************************************
*!
*!      Function: lfvOk
*!
*!**************************************************************************
* Fill the filter expresion when the <ok.> pressed in scope window.
*
FUNCTION lfvOk
** Clear the expresion **
lcExprsion = ""
** Flag to deferntiate between <ok.> and <cancel> **
llScope = .T.
** List for : Invoice - Debit memos - Both **
DO CASE
  CASE rbScopeOn = 1
    lcExprsion = "(NINVAMNT-NINVPAID-NINVDISTK-NINVADJ > 0)" 
    IF lnPyChMN = 3
      *E300296,1 M.H   10/10/95 Add the currency to the AP module.
      *E300296,1 M.H Add the vendor to the currency to the filter exp.
      *lcExprsion = lcExprsion +" .AND. CVENPMETH = 'H'"

      *B601013,1 Change filter expression as to filter on approved currency.
      *lcExprsion = lcExprsion +" .AND. CVENPMETH = 'H' .AND. APINVHDR.CCURRCODE = lcCurrCode"

      *B601217,1 Change this line to use the new varible lcCurCod [Begin]
      *lcExprsion  = lcExprsion +" .AND. CVENPMETH = 'H' .AND. APINVHDR.cAprCurCod = lcCurrCode"
      lcExprsion  = lcExprsion +" .AND. CVENPMETH = 'H' .AND. APINVHDR.cAprCurCod = lcCurCod"
      *B601217,1 Change this line to use the new varible lcCurCod [End]

      *B601013,1 end.
    ELSE

      *B601217,1 Change this line to use the new varibles lcBnkCod
      *           , lcChekCod [Begin]
      *lcExprsion =lcExprsion + " .AND. CVENPMETH <> 'H' .AND."+;
      *           "CBNKCODE+CCHKACCT = lcBankCode+lcCheckcode"

      lcExprsion =lcExprsion + " .AND. CVENPMETH <> 'H' .AND."+;
                 "CBNKCODE+CCHKACCT = lcBnkCod+lcChekCod"

      *B601217,1 Change this line to use the new varibles [End]

    ENDIF

  CASE rbScopeOn = 2
    lcExprsion = "(NINVAMNT-NINVPAID-NINVDISTK-NINVADJ < 0)"
    IF lnPyChMN = 3 && If the program called from cash payments bar
      *E300296,1 M.H   10/10/95 Add the currency to the AP module.
      *E300296,1 M.H Add the vendor to the currency to the filter exp.
      *lcExprsion = lcExprsion +" .AND. CVENPMETH = 'H'"
      *B601013,1 Change filter expression as to filter on approved currency.      
      *lcExprsion = lcExprsion +" .AND. CVENPMETH = 'H' .AND. APINVHDR.CCURRCODE = lcCurrCode"

      *B601217,1 Change this line to use the new varible lcCurCod [Begin]
      *lcExprsion = lcExprsion +" .AND. CVENPMETH = 'H' .AND. APINVHDR.cAprCurCod = lcCurrCode"
      lcExprsion = lcExprsion +" .AND. CVENPMETH = 'H' .AND. APINVHDR.cAprCurCod = lcCurCod"
      *B601217,1 Change this line to use the new varible lcCurCod [End]

      *B601013,1 end.
    ELSE

      *B601217,1 Change this line to use the new varibles lcBnkCod
      *           , lcChekCod [Begin]
      *lcExprsion = lcExprsion + " .AND. CVENPMETH <>'H' .AND."+;
      *             "CBNKCODE+CCHKACCT = lcBankCode+lcCheckcode"

      lcExprsion = lcExprsion + " .AND. CVENPMETH <>'H' .AND."+;
                   "CBNKCODE+CCHKACCT = lcBnkCod+lcChekCod"

      *B601217,1 Change this line to use the new varibles [End]

    ENDIF
    
  CASE rbScopeOn = 3
    lcExprsion = "(NINVAMNT-NINVPAID-NINVDISTK-NINVADJ<> 0)"
    IF lnPyChMN = 3 && If the program called from cash payments bar
      *E300296,1 M.H   10/10/95 Add the currency to the AP module.
      *E300296,1 M.H Add the vendor to the currency to the filter exp.
      *lcExprsion = lcExprsion +" .AND. CVENPMETH = 'H'"
      *B601013,1 Change filter expression as to filter on approved currency.            
      *lcExprsion = lcExprsion +" .AND. CVENPMETH = 'H' .AND. APINVHDR.CCURRCODE = lcCurrCode"

      *B601217,1 Change this line to use the new varible lcCurCod [Begin]
      *lcExprsion = lcExprsion +" .AND. CVENPMETH = 'H' .AND. APINVHDR.cAprCurCod = lcCurrCode"
      lcExprsion = lcExprsion +" .AND. CVENPMETH = 'H' .AND. APINVHDR.cAprCurCod = lcCurCod"
      *B601217,1 Change this line to use the new varible lcCurCod [End]

      *B601013,1 end.
    ELSE

      *B601217,1 Change this line to use the new varibles lcBnkCod
      *           , lcChekCod [Begin]
      *lcExprsion = lcExprsion + " .AND. CVENPMETH <> 'H' .AND." +;
      *            "CBNKCODE+CCHKACCT=lcBankCode+lcCheckcode"

      lcExprsion = lcExprsion + " .AND. CVENPMETH <> 'H' .AND." +;
                  "CBNKCODE+CCHKACCT=lcBnkCod+lcChekCod"

      *B601217,1 Change this line to use the new varibles [End]

    ENDIF
    
ENDCASE

** Division **
IF lcDivCode <> "*"
  lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
               "CDIVISION = lcDivCode"
ENDIF
                            
** Payment Pariority. **
IF !EMPTY(lcPriority) 
  lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
               "CVENPRIOR = lcPriority"
ENDIF

** Payment method. **
IF !EMPTY(lcMethod)
  lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
               "CVENPMETH = lcMethod"
ENDIF

*** Invoice reference. ***
IF !EMPTY(lcInvRef)
  lcCInvRef  = STRTRAN(UPPER(lcInvRef)," ","?")
  lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
               "LIKE(lcCInvRef,UPPER(CINVREF))"
ENDIF

** Due date. **
IF rbDuDsDt = 1
  DO CASE
    ** !EMPTY "from"  and  EMPTY "to"
    CASE !EMPTY(ldFrDueDat) .AND. EMPTY(ldToDueDat)
      lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
                   "DINVDUDAT >= ldFrDueDat"
    ** EMPTY "from"  and  !EMPTY  "to"
    CASE  EMPTY(ldFrDueDat) .AND. !EMPTY(ldToDueDat)
      lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
                   "DINVDUDAT <= ldToDueDat"
    ** !EMPTY "from"  and  !EMPTY "to"
    CASE !EMPTY(ldFrDueDat) .AND. !EMPTY(ldToDueDat)
      IF ldFrDueDat > ldToDueDat   && if from date greater than through date.
        ** MESSAGE : "From Data can not be less than Through Date."
        **           "                       ® Ok ¯               "
        =gfModalGen("TRM04028B00000","DIALOG",lcTThrou+"|"+lcTFrom)
        _CUROBJ=OBJNUM(rbDuDsDt)
        llScope = .F.
        RETURN  
        ldFrDueDat={}
        ldToDueDat={}
      ELSE
        lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
                     "BETWEEN(DINVDUDAT,ldFrDueDat,ldToDueDat)"
      ENDIF               
  ENDCASE
ELSE

  ** Discount date from. **
  DO CASE
    ** !EMPTY "from"  and  EMPTY "to"
    CASE !EMPTY(ldFrDisDat) .AND. EMPTY(ldToDisDat)
      lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
                   "DINVDUDAT >= ldFrDisDat"
    ** EMPTY "from"  and  !EMPTY  "to"
    CASE  EMPTY(ldFrDisDat) .AND. !EMPTY(ldToDisDat)
      lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
                   "DINVDUDAT <= ldToDisDat"
    ** !EMPTY "from"  and  !EMPTY "to"
    CASE !EMPTY(ldFrDisDat) .AND. !EMPTY(ldToDisDat)
      IF ldFrDisDat > ldToDisDat   
        ** MESSAGE : "From Data can not be less than Through Date"
        **           "                       ® Ok ¯ 
        =gfModalGen("TRM04028B00000","DIALOG",lcTThrou+"|"+lcTFrom)
        _CUROBJ=OBJNUM(rbDuDsDt)
        RETURN
        ldFrDisDat={}
        ldToDisDat={}
      ELSE
        lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
                     "BETWEEN(DINVDUDAT,ldFrDisDat,ldToDisDat)"  
      ENDIF               
  ENDCASE
ENDIF

*B801208,4 AMM start
*GO TOP      && Refresh the browse filter.
LOCATE
*B801208,4 AMM end

IF EOF()
  ** MESSAGE : " No Open invoices mateching selected "
  **           " invoices for vendor ð.              "
  **           "                   ® Ok ¯            "
  =gfModalGen("TRM04035B00000","DIALOG",ALLTRIM(lcVendCode))
  RETURN
ENDIF
CLEAR READ

*!**************************************************************************
*!
*!      Function: lfvCancel
*!
*!**************************************************************************
* Restore old values if push cancel from scope.
*
FUNCTION lfvCancel

llScope    = .F.

** RESTORE OLD VAR. IN CURRENT VAR. **

ldToDisDat = ldOToDisDt
ldFrDisDat = ldOFrDisDt
ldToDueDat = ldOToDueDt
ldFrDueDat = ldOFrDueDt
lcPriority = lcOldPrty 
lcMethod   = lcOMethod
lcDivCode  = lcOldDvCod
lcDivision = lcOldDivDs
lcPayMeth  = lcOldpymth
lcInvRef   = lcOldInvRf
rbODuDsDt  = rbDuDsDt
lcExprsion = lcOldExpr  && Retrieve the old filter expresion.

SELECT APINVHDR
*B801208,4 AMM start
*GO TOP && Refresh the browse filter.
LOCATE
*B801208,4 AMM end


*!**************************************************************************
*!
*!      Function: lfwBank
*!
*!**************************************************************************
* Save the old bank.
*
FUNCTION lfwBank
lcOldVal  = lcBankCode

*!**************************************************************************
*!
*!      Function: lfvBank
*!
*!**************************************************************************
* Validation of bank.
*
FUNCTION lfvBank
PRIVATE llVldObj

IF llBrowse .OR. EVALUATE(SYS(18)) <> lcOldVal 
  llVldObj = lfBnkChk(@laBankObjs, lcOldVal, @llBrowse, @lcObjDisp)

  *B601217,1 Add this lines to unselect the selected invoices if the Bank
  *          code was changed [Begin]
  *B601217,1 IF Statment to check if the Bank code was changed
  IF lcBankCode <> lcOldVal
    =lfClearSel()
  ENDIF   && End of IF
  lcBnkCod = lcBankCode
  *ahmed
  lcChekCod = lcCheckCode
  *ahmed end
  *B601217,1 Add this lines [End]  

  SELECT APINVHDR
  *B801208,4 AMM start
  *GO TOP
  *B801208,4 AMM end
  
  =lfShowDs(.F.,.T.)
  RETURN IIF(llVldObj, .T., 1)
ENDIF  

*!**************************************************************************
*!
*!      Function: lfwChecks
*!
*!**************************************************************************
* Save old check.
*
FUNCTION lfwChecks

lcOldVal   = lcCheckCode

*!**************************************************************************
*!
*!      Function: lfvChecks
*!
*!**************************************************************************
*
FUNCTION lfvChecks
PRIVATE llVldObj

llVldObj = .T.
IF llBrowse .OR. EVALUATE(SYS(18)) <> lcOldVal 
  llVldObj = lfBnkChk(@laBankObjs,lcOldVal,@llBrowse,@lcObjDisp)

  *B601217,1 Add this lines to unselect the selected invoices if the Checking
  *          account was changed [Begin]
  IF lcCheckCode <> lcOldVal
    =lfClearSel()
  ENDIF
  lcChekCod = lcCheckCode
  *B601217,1 Add this lines [End]

  SELECT APINVHDR
  *B801208,4 AMM start 
  *GO TOP
  *B801208,4 AMM end
  
  =lfShowDs(.F., .T.)
** Else, if neither coming from browsing button, nor does the new field
** value equal the old one, 
ELSE
  IF llDispbr 
    SELECT APINVHDR
    *B801208,4 AMM start 
    *GO TOP
    *B801208,4 AMM end
    
    =lfShowDs(.F.,.T.)
  ENDIF
ENDIF  
RETURN IIF(llVldObj, .T., 1)

*!**************************************************************************
*!
*!      Function: lfvpbSel
*!
*!**************************************************************************
* Let Select prompt change according to its condition.
*
FUNCTION lfvpbSel

IF ASCAN(laSelect,APINVHDR.CINVNO)<> 0 
  SHOW GET pbSelect,1 PROMPT 'UnSe\<lect'
ELSE
  SHOW GET pbSelect,1 PROMPT 'Se\<lect'
ENDIF 

RETURN .T.

*!**************************************************************************
*!
*!      FUNCTION : lfDeAct
*!
*!**************************************************************************
*
FUNCTION lfDeAct

IF WONTOP(lcBrTtl)
  glFromBrow = .T.
  RELEASE PAD _BROWSE OF _MSYSMENU
  =lfBrowTrap()
  =lfMousClk()
ELSE    
  =lfBrowUnTrap() 
  =lfClrMous() 
  ** Unlock selected record if close from corner or release trap if change to another program.
  IF EMPTY(WONTOP()) .OR. WONTOP("FNDATION") .OR. WPARENT(WONTOP()) <> gcBaseWind
    IF !WVISIBLE(gcBaseWind)
      =lfvClose()
    ENDIF
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      FUNCTION : lfBrowTrap
*!
*!**************************************************************************
*
FUNCTION lfBrowTrap

ON KEY LABEL Alt+E      DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+S      DO  lpDoTrap WITH LASTKEY()
ON KEY LABEL Alt+D      DO  lpDoTrap WITH LASTKEY()        
ON KEY LABEL Alt+V      DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+L      DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+A      DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+N      DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+I      DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+P      DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+C      DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL ESC        DO  lpDoTrap WITH LASTKEY()

*!**************************************************************************
*!
*!      PROCEDURE: lpDoTrap
*!
*!**************************************************************************
*
PROCEDURE lpDoTrap
PARAMETER lnLstKey

KEYBOARD '{SHIFT+HOME}'
IF !EOF('APINVHDR')
  DO CASE
    CASE lnLstKey = 27 
      IF _DOS    
        HIDE MENU _MSYSMENU
        SHOW MENU _MSYSMENU
      ENDIF  
      =lfvclose()
    CASE lnLstKey = 18 
      =lfvScope()
    CASE lnLstKey = 31
      rbScope=2
      SHOW GET rbScope
      =lfvScope()
      ON KEY LABEL Alt+S   DO  lpDoTrap WITH LASTKEY()
    CASE lnLstKey = 32
      ACTIVATE WINDOW (lcReadNam1)
      _CUROBJ=OBJNUM(pbVendor)
      =lfvVendor()
    CASE lnLstKey = 47
      ACTIVATE WINDOW (lcReadNam1)
      _CUROBJ=OBJNUM(pbVendor)
      =lfvInvoice()      
    CASE lnLstKey = 38 
      =lfvSelect()  
      =lfvpbSel()
    CASE lnLstKey = 30
      =lfvSelAll()
    CASE lnLstKey = 49
      =lfvSelNon() 
    CASE lnLstKey = 23
      =lfvInvert() 
    CASE lnLstKey = 25 
      =lfBrowUnTrap() .AND. lfClrMous() 
      glFromBrow = .F.
      llFromTrap = .T.
      =lfvPySlInv()
      llFromTrap = .F.
      =lfBrowTrap()
      =lfMousClk()
      glFromBrow = .T.
  ENDCASE
ENDIF  

IF lnLstKey = 46 
  ACTIVATE WINDOW (lcReadNam2)
  =lfvclose()
ENDIF
  
*!**************************************************************************
*!
*!      FUNCTION : lfBrowUnTrap
*!
*!**************************************************************************
*
FUNCTION lfBrowUnTrap

ON KEY LABEL Alt+E        
ON KEY LABEL Alt+S
ON KEY LABEL Alt+D
ON KEY LABEL Alt+V
ON KEY LABEL Alt+L
ON KEY LABEL Alt+A
ON KEY LABEL Alt+N
ON KEY LABEL Alt+I
ON KEY LABEL Alt+P
ON KEY LABEL Alt+C
ON KEY LABEL ESC DO lfvClose

*!**************************************************************************
*!
*!      FUNCTION : lfPushKey
*!
*!**************************************************************************
* Trap keys.
*
FUNCTION lfPushKey

** places all current on key labels on a stack in memory.
PUSH KEY
** Deactivate control keys for browse window 
** ESCAPE, TAB
ON KEY LABEL TAB        DO  lpTrap
ON KEY LABEL BACKTAB    DO  lpTrap 
ON KEY LABEL ESC        DO lfvClose
ON KEY LABEL Ctrl+ENTER DO lfvclose

*!**************************************************************************
*!
*!      Function: lfActBrows
*!
*!**************************************************************************
*
FUNCTION lfActBrows

RELEASE PAD _BROWSE OF _MSYSMENU
ACTIVATE WINDOW (lcBrTtl)

*!**************************************************************************
*!
*!      Function: lfDefinePad
*!
*!**************************************************************************
*
FUNCTION lfDefinePad
IF _DOS
  =lfBrowUnTrap() 
  =lfMousClk()
  DEFINE PAD _BROWSE OF _MSYSMENU PROMPT '\<Browse' KEY ALT+B
  ON SELECTION PAD _BROWSE OF _msysmenu DO lfActBrows
ENDIF

*!**************************************************************************
*!
*!      Procedure: lpTrap
*!
*!**************************************************************************
*
PROCEDURE lpTrap
 
** check if the window on top is  browse window.
ON KEY LABEL TAB         lnDummi = 1
ON KEY LABEL BACKTAB     lnDummi = 1 

DO CASE
  ** Tab. or Right cursor **  
  CASE LASTKEY() = 9 .OR. LASTKEY() = 4
    IF _DOS
      HIDE MENU _MSYSMENU
      SHOW MENU _MSYSMENU
    ENDIF  
    IF WONTOP(lcReadNam1)
      IF lnPyChMN = 3 && If called from cash payment
        IF _CUROBJ = OBJNUM(pbInvoice)
           ACTIVATE WINDOW (lcBrTtl)
        ELSE
          IF _CUROBJ = OBJNUM(lcVendCode) OR _CUROBJ = OBJNUM(lcVendComp);
            OR _CUROBJ = OBJNUM(lcVPhone)
            =lfvVend()
             _CUROBJ = _CUROBJ + 1
          ENDIF
            _CUROBJ = _CUROBJ + 1
        ENDIF
      ELSE
        IF _CUROBJ = OBJNUM(pbInvoice)
           ACTIVATE WINDOW (lcBrTtl)
        ELSE
          IF _CUROBJ = OBJNUM(lcVendCode) OR _CUROBJ = OBJNUM(lcVendComp);
            OR _CUROBJ = OBJNUM(lcVPhone)
            =lfvVend()
            _CUROBJ = OBJNUM(lcBankCode)
          ENDIF  
          IF _CUROBJ = OBJNUM(lcCheckCode)
            =lfvChecks()                 
            
            *B801208,4 AMM start 
            *GO TOP
            *B801208,4 AMM end
            
            IF EMPTY(APINVHDR.CINVNO)
              =lfvNoAprPa()
              ACTIVATE WINDOW(lcReadNam2)
            ELSE
              ACTIVATE WINDOW (lcReadNam1)
              _CUROBJ = OBJNUM(rbScope)
            ENDIF   
          ELSE
            IF _CUROBJ = OBJNUM(lcBankCode)
              =lfvBank()
              ACTIVATE WINDOW (lcReadNam2)
              _CUROBJ = _CUROBJ + 1
            ELSE
              _CUROBJ = _CUROBJ + 1
            ENDIF  
          ENDIF   
        ENDIF
      ENDIF
    ELSE
      IF WONTOP(lcBrTtl)
        ACTIVATE WINDOW (lcReadNam2)
        _CUROBJ=OBJNUM(pbSelect)
      ELSE
        IF _CUROBJ = OBJNUM(pbClose)
          ACTIVATE WINDOW (lcReadNam1)
          _CUROBJ=OBJNUM(lcVendCode)
        ELSE
          _CUROBJ = _CUROBJ + 1
        ENDIF
      ENDIF  
    ENDIF
    
  ** Shift Tab key or Left cursor key **
  CASE  LASTKEY() = 15 .OR. LASTKEY() = 19
    IF _DOS
      HIDE MENU _MSYSMENU
      SHOW MENU _MSYSMENU
    ENDIF  
    IF WONTOP(lcReadNam2)
      IF _CUROBJ = OBJNUM(pbSelect)
        ACTIVATE WINDOW (lcBrTtl)
      ELSE
        IF _CUROBJ = OBJNUM(pbClose) .AND. EMPTY(APINVHDR.CINVNO)
          ACTIVATE WINDOW(lcReadNam1)
          _CUROBJ= OBJNUM(lcCheckCode)
        ELSE  
          _CUROBJ = _CUROBJ - 1
        ENDIF
      ENDIF  
    ELSE  
      IF WONTOP(lcBrTtl)
        ACTIVATE WINDOW (lcReadNam1)
         _CUROBJ = OBJNUM(pbInvoice)
      ELSE
        IF WONTOP (lcReadNam1)  
          IF _CUROBJ = OBJNUM(pbInvoice) .OR. _CUROBJ = OBJNUM(pbVendor)
            _CUROBJ = _CUROBJ - 1
          ELSE
            IF _CUROBJ = OBJNUM(lcVendCode)
              ACTIVATE WINDOW (lcReadNam2)    
              _CUROBJ = OBJNUM(pbClose)
            ELSE
              _CUROBJ = _CUROBJ - 2
            ENDIF  
          ENDIF
        ENDIF
      ENDIF
    ENDIF
ENDCASE

ON KEY LABEL TAB         DO  lpTrap
ON KEY LABEL BACKTAB     DO  lpTrap 


*!**************************************************************************
*!
*!      FUNCTION: lfwCurr
*!
*!**************************************************************************
*E300296,1 M.H 10/10/95 Add the currency to the AP module.
*
FUNCTION lfwCurr

lcOldCurr = lcCurrCode

*!**************************************************************************
*!
*!      FUNCTION: lfvCurr
*!
*!**************************************************************************
*E300296,1 M.H 10/10/95 Add the currency to the AP module.
*
FUNCTION lfvCurr

*B601217,1 Add this line [Begin]
*B601217,1 IF Statment to check if the currency code did not change 
*          and the user is not browsing 
IF lcCurrCode = lcOldCurr .AND. !llBrowse
  RETURN
ENDIF  
*B601217,1 Add this line [Begin]

*B601217,1 Change this line and reset the flag llBrowse [Begin] 
*IF llBrowse .OR. !SEEK(lcCurrCode,'SYCCURR') .OR. ATC("?",lcCurrCode) > 0 .AND. LASTKEY() = 13
IF llBrowse .OR. ATC("?",lcCurrCode) > 0 .OR. !SEEK(lcCurrCode,'SYCCURR')
  llBrowse = .F.
*B601217,1 Change this line [End]

  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp     = ''
  lcSavBrFld = lcBrFields
  lcSavTitle = lcFile_Ttl
  lcFile_Ttl = "Currency"
  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
               "CCURRDESC :R :H= 'Description',  " +;
               "CCURRSMBL :R :H= 'Symbol'"
  =gfBrows('','CCURRCODE','laTemp')
  lcBrFields = lcSavBrFld
  lcFile_Ttl = lcSavTitle
  IF EMPTY(laTemp[1])
    lcCurrCode = lcOldCurr
  ELSE
    lcCurrCode = laTemp[1]

    *B601217,1 Add this lines to unselect the selected invoices if the Vendor
    *          code was changed [Begin]
    *B601217,1 	IF Statment to check if the Currency code was changed
    IF lcCurrCode <> lcOldCurr
      =lfClearSel()
    ENDIF    && End of IF
    lcCurCod = lcCurrCode
    *B601217,1 Add this lines [End]

    SELECT APINVHDR
    GO TOP
    =lfShowDs(.F.,.T.)
  ENDIF
ELSE

  *B601217,1 Add this lines to unselect the selected invoices if the Vendor
  *          code was changed [Begin]
  *B601217,1 	IF Statment to check if the Currency code was changed
  IF lcCurrCode <> lcOldCurr
    =lfClearSel()
  ENDIF    && End of IF
  lcCurCod = lcCurrCode
  *B601217,1 Add this lines [End]

  SELECT APINVHDR
  GO TOP
  =lfShowDs(.F.,.T.)
ENDIF

SELECT APINVHDR


*!*************************************************************
*! Name      : lfwOldVal                            *B601013,1   
*! Developer : RENEE - Renee Ezzat
*! Date      : 04/16/1996
*! Purpose   : When function for lnChkExRat field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal
lnOldVal = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvPayDate                             *B601013,1 
*! Developer : RENEE - Renee Ezzat
*! Date      : 04/16/1996
*! Purpose   : Valid function for ldPayDat field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvPayDate()
*!*************************************************************
FUNCTION lfvPayDate
*B601013,1 Validate the payment date if a new one , or if changed.
IF EMPTY(ldPayDat)
  lnChkExRat = 0
  SHOW GET lnChkExRat DISABLE
ENDIF
IF ldPayDat <> ldOldPDat  
  *B601013,1 If the payment date is empty, do not validate and
  *B601013,1 clear the exchange rate
  *IF EMPTY(ldPayDat)
  *  lnChkExRat = 0
  *  SHOW GET lnChkExRat DISABLE
  *ELSE
    IF lfVDtMsg(gcPrnt_Cmp ,@lcFisPrd,@lcFisYer,ldPayDat)
      lcPeriod  = lcFisPrd
      lcYear    = lcFisYer
      ldOldPDat = ldPayDat 
    
      *B601013,1 Upon selecting a valid date, get the exchange rate
      *B601013,1 for the currency.
      lnChkExUnt = 1
      IF APINVHDR.CAPRCURCOD <> gcBaseCurr
        lnChkExRat  = gfChkRate('lnChkExUnt',;
                      APINVHDR.CAPRCURCOD,ldPayDat, .T.,;
                      gcAct_Comp, gcBaseCurr, .T.)
        IF lnChkExRat = 0 .AND. !llAddRate
          *B601013,1 MESSAGE : " A valid ð to ð exchange rate could not "
          *B601013,1           " be found for ð.                        "
          *B601013,1           "                  ® Ok ¯                "
          =gfModalGen("QRM04157B00000","DIALOG",;
             ALLTRIM(APINVHDR.CAPRCURCOD)+'|'+ALLTRIM(gcBaseCurr)+'|'+DTOC(ldPayDat))
        ENDIF
        IF llEditEx
          SHOW GET lnChkExRat ENABLE
        ELSE
          SHOW GET lnChkExRat DISABLE
        ENDIF               
      ELSE  
        lnChkExRat  = 1
        SHOW GET lnChkExRat DISABLE 
      ENDIF                
      *B601013,1 end
    ELSE
      ldPayDat  = ldOldPDat  
      IF EMPTY(ldPayDat)
        lnChkExRat = 0
        SHOW GET lnChkExRat DISABLE
      ENDIF  
      _CUROBJ   = OBJNUM(ldPayDat)
      RETURN
    ENDIF  
  *ENDIF  
ENDIF

*!*************************************************************
*! Name      : lfvExRate                             *B601013,1 
*! Developer : RENEE - Renee Ezzat
*! Date      : 04/16/1996
*! Purpose   : Valid function for lnChkExRat field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvExRate()
*!*************************************************************
*E300296,4 Added this function as a validation for the new 
*E300296,4 field, lnChkExRat
FUNCTION lfvExRate
*E300296,4 if the entered value is not greater than zero,
*E300296,4 Message :  "   ð should be greater than ð.   "
*E300296,4                 ®   OK   ¯
IF lnChkExRat <> lnOldVal .AND. lnChkExRat < 0 .AND. ;
   gfModalGen("TRM04072B00000","DIALOG", lcTExRateMsg) > 0
  *E300296,4 Old adjustment amount field is also filled from the
  *E300296,4 when function of this field, to save a variable.
  *E300296,4 i.e. At this point it actually contains the old exchange
  *E300296,4 rate.
  lnChkExRat = lnOldVal
  _CUROBJ = _CUROBJ
ENDIF  
*E300296,4 end                

*!*************************************************************
*! Name      : lfClearSel                              
*! Developer : Haytham El_Sheltawi
*! Date      : 03/03/1997
*! Purpose   : Function to unselect the selected invoices 
*!*************************************************************
*! Calls     : gfObj_lock()
*!*************************************************************
*! Called From  : lfvVend , lfvBank , lfvChecks
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClearSel()
*!*************************************************************
*B601217,1 This function was added by HS for the bug
*!*************************************************************
*
FUNCTION lfClearSel

PRIVATE lnCount , lcFilt

lcOldAlias = ALIAS()   && Save the old alias 
SELECT APINVHDR


*FOR Loop to scan the array laSelect
FOR lnCount = 1 TO ALEN(laSelect , 1) 
  *IF Statment to check if the 2 columns of this row is numeric 
  IF TYPE('laSelect[lnCount , 2]') = 'N'
    GOTO laSelect[lnCount , 2]
    =gfObj_lock(.F.)  
  ENDIF    && End of IF
ENDFOR   && End of FOR

lnArlen = 1
DIMENSION laSelect[1,2]
laSelect = .F.

SHOW GET pbSelect,1 PROMPT 'Se\<lect'

*B801208,4 AMM start
*GO TOP
*lnBrRecNo  = RECNO('APINVHDR')
*SHOW WINDOW (lcBrTtl) REFRESH  && refreshing the browse window.
*B801208,4 AMM end

SELECT &lcOldAlias

*!*************************************************************
*! Name      : lfObj_Lock
*! Developer : Mohamed Shokry
*=> Added here by TMI for the issue *B132275,1
*! Date      : 06/21/2006
*! Purpose   : To object lock any record in any file
*!*************************************************************
FUNCTION lfObj_Lock
PARAMETERS lLok_Set
PRIVATE lnRecNo,lRet_Flag


PRIVATE lnOldrpSt

lRet_Flag = .F.
lLok_It   = .F.
llLocked  = .F.
*** Go to the same record to get a fresh copy in the buffer
lnRecNo = RECNO()

DO WHILE .T.

  IF lnRecNo <= RECCOUNT()
    GO lnRecNo
    llLocked = RLOCK() 
    IF DELETED()
      UNLOCK
      =gfModalGen('INM00095B00000','ALERT')
      laScrMode     = .F.
      laScrMode [1] = .T.
      SHOW GETS
      RETURN .F.
    ENDIF
   
  ENDIF  

  *B608942,1 TMI 07/20/2009 06:14:25 PM [Start] 
  IF glRunFrmA4
    lnSlct = SELECT(0)
    IF !USED('A4USTATC')
      =gfOpenFile(gcA4SYSFLDR+'SYUSTATC','CUSER_ID','SH','A4USTATC')
    ENDIF
    SELECT (lnSlct)
  ENDIF    
  *B608942,1 TMI 07/20/2009 06:14:25 PM [End  ] 
  
  *** Chek if the record is in use by another user
  IF lLok_Set 
    *** Chek if the field cLok_User in the structur
    IF !lLok_Stat .AND. llLocked
      *** Record is not locked you may lock it
      lLok_It   = .T.
    ELSE
      IF !EMPTY(cLok_User)

        lnOldrpSt = SET('REPROCESS')          
        SET REPROCESS TO 1
        *B608942,1 TMI 07/20/2009 07:23:22 PM [Start] check if the user recorded in cLok_user field lies in SYUSTATC file of either A27 or A4xp
        *IF SEEK ('INI'+'OLDVARS'+cLok_User,'syuStatc') 
          IF SEEK ('INI'+'OLDVARS'+cLok_User,'syuStatc') .OR. IIF(glRunFrmA4,SEEK('INI'+'OLDVARS'+cLok_User,'A4USTATC'),.F.)
          *B608942,1 TMI 07/20/2009 07:23:29 PM [End  ] 
          UNLOCK
          *** Display the message "Record is in use by user AAAA"
          lnSavRec   = IIF(RECNO('SYUUSER')>RECCOUNT('SYUUSER'),0,;
                       RECNO('SYUUSER'))
          lcLok_User = ALLTRIM(PROPER(LOOKUP(syuUser.cUsr_name,cLok_User,;
                       syuUser.cUser_id,'cUser_id')))
          IF lnSavRec > 0
            GO lnSavRec IN SYUUSER
          ENDIF  
             
          *** Record is in use by user ????    
          *IF gfModalGen("INM00028B00015","ALERT",lcLok_User) = 1
          lcRtyCncMs = "Invoice "+ALLTRIM(APINVHDR.CINVNO)+" for vendor "+ ALLTRIM(APINVHDR.CVendCode)+" is being edited by user " + lcLok_User+"."
          IF  gfModalGen("INM00274B00015","ALERT",lcRtyCncMs) = 1
            LOOP
          ENDIF  
          lLok_It    = .F.
          lRet_Flag  = .F.
        ELSE
          lLok_It    = .T. 
        ENDIF          
        SET REPROCESS TO  lnOldrpSt
      ELSE
        *** Display the message "Record is in use by another"
        IF gfModalGen("INM00029B00015","ALERT") = 1
          LOOP
        ENDIF  
        lLok_It    = .F.
        lRet_Flag  = .F.
      ENDIF   
    ENDIF

  ELSE
    *** Chek if these three field in the file structur
    IF TYPE ('cLok_User') <> "U" .AND. ;
       TYPE ('dLok_Date') <> "U" .AND. ;
       TYPE ('cLok_Time') <> "U" 

      *** Unlock the record
      REPLACE lLok_Stat WITH .F. , ;   
              cLok_User WITH ""  , ;
              dLok_Date WITH {}  , ;
              cLok_Time WITH ""
      lRet_Flag  = .T.
    ENDIF  
  ENDIF

  EXIT
ENDDO

*** Chek if you have to lock the record or not
IF lLok_It  
  *** Chek if these three field in the file structur
  IF TYPE ('cLok_User') <> "U" .AND. ;
     TYPE ('dLok_Date') <> "U" .AND. ;
     TYPE ('cLok_Time') <> "U" 
    *** Lock the record for this user with date and time
    WAIT WINDOW NOWAIT ' ... Locking ... '
    REPLACE lLok_Stat WITH .T.       , ;   
            cLok_User WITH gcUser_ID , ;
            dLok_Date WITH DATE()    , ;
            cLok_Time WITH gfGetTime()
    WAIT CLEAR
    lRet_Flag  = .T.    
  ENDIF
ENDIF


UNLOCK


RETURN lRet_Flag



