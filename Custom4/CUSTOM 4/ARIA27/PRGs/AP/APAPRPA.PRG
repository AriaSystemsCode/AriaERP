*:************************************************************************
*:
*: Procedure file: APAPRPA.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Hisham Ramsis Philips
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: lfDispBrow
*:               : lfGetPayMeth
*:               : lfwInvBrow
*:               : lfwAccount
*:               : lfvLcGlact
*:               : lfvSortBy
*:               : lfvScope
*:               : lfvVendor
*:               : lfvTot
*:               : lfvInvoice
*:               : lfvClose
*:               : lfvDisAppr
*:               : lfvDApprAll
*:               : lfvApprAll
*:               : lfShAprAll
*:               : lfvpbAprAl
*:               : lfShow
*:               : lfGetDefu
*:               : lfvApprPart
*:               : lfvAprov
*:               : lfvCanPay
*:               : lfwAprDisc
*:               : lfvAprDisc
*:               : lfwAprToPay
*:               : lfvAprToPay
*:               : lfw1099amnt
*:               : lfv1099amnt
*:               : lfwAprAdj
*:               : lfvAprAdj
*:               : lfwVend
*:               : lfvVend
*:               : lfwDivision
*:               : lfvDivision
*:               : lfvparior
*:               : lfvMethod
*:               : lfvFrTodat
*:               : lfvDat
*:               : lfvOk
*:               : lfvCancel
*:               : lfwBank
*:               : lfwChecks
*:               : lfvBnkChk    
*:               : lfwGlAcct
*:               : lfvGlAcct
*:               : lfDeAct
*:               : lfBrowTrap
*:               : lpDoTrap
*:               : lfBrowUnTrap
*:               : lfPushKey
*:               : lfActBrows
*:               : lfDefinePad
*:               : lpTrap
*:               :
*:
*:      Documented 00/00/1994
*:      
*:************************************************************************
*B500787,1 RENEE 06/22/95. Fixed browse refreshing after Approving all or
*B500787,1                 disapproving all to point at the correct record
*B500787,1                 in the browse without having to access te browse
*B500787,1                 first.
*B600492,1 Reham On 06/22/95
*B600492,1 Change the browse fields width (Invoice no - vendor code).
*E300258,4 MAN 06/29/95 Add the check on supressing 1099 processing
*B600643,1 Hesham El_Sheltawi 08/20/95 When openning the calculator we get DO nesting too deep
*E300296,4 RENEE 10/31/95 Add currency exchange checks (multi currency)
*B600808,1 RENEE 11/12/95 1. Fixed the bug of not refreshing bank\checking
*B600808,1                   accounts and Gl accounts approving one by one.
*B600808,1                   in the case of approve fully.
*B600808,1                2. Fixed the bug of incorrect defaulting oc cash
*B600808,1                   payment account by the checking account Gl 
*B600808,1                   account instead of the cash payment account.
*B600808,1                   in cases of approve fully and partially.
*B600808,1                3.Approve partially screen :
*B600808,1                   a. If an invoice is disapproved, and then we enter the approve partially
*B600808,1                      screen, the approved adjustment field is not zero??
*B600808,1                   b. If an approved invoice is to be partially approved (again), the system
*B600808,1                      allows the approval of amounts greater than the open invoice amount.
*B600808,1                   c. If the approved to pay field is left empty, while any of the other  
*B600808,1                      fields are not, we can still approve. We shouldn't be able to approve
*B600808,1                      if this field is zero.
*B600808,1                   d. Cash payment approval screen :
*B600808,1                      Added approved 1099 amount field.
*B600846,1 HISH 11/29/95  Got the default approved to pay amount and discount amount,
*B600846,1                which is displayed in approved partially screen.
*B600849,1 HISH 11/29/95  Used gfChkRate to get exchange rate and unit to changed currency
*B600849,1                from invoice currency to approve currency.
*E300316,1 HISH 12/05/95. Used global function gfGetExSin to get currency 
*E300316,1                equation signs.
*B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
*E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin.
*B601013,4 HISH  04/18/96. Got the exchange rate, and currency unit signs.
*B601416,1 HS    12/26/96 Change the calling of gfChkRate and gfGetExSin 
*B601416,1                in the Approve for payment case so if the Invoice 
*B601416,1                currency is the base currency I make it the To  
*B601416,1                currency and if not the Approve currency is 
*B601416,1                the To currency 
*B601519,1 HS    01/02/97 Fix the Approve for payments screen [Approve partially]
*B601519,1                    1)The replacement of the data.
*B601519,1                    2)And refresh the Exchange rate field and 
*B601519,1                      the Approved amount in approve currency. 
*B601519,1                in the case of entering the exchange rate on the
*B601519,1                fly  
*B601526,1  HS  16/01/97  Use the new field [APINVHDR.nInvFAAp] in the 
*B601526,1                Approve for payments screens [Fill the new field
*B601526,1                with the approved amount in the approve currency] 
*B601581,1 Hesham El-Sheltawi 03/09/97
*B601581,1 Check if the Thermometer window exist then force it to close
*B601581,1 in the all appreoving Functions
*B601648,1 Hesham El-Sheltawi 03/09/97
*B601648,1 If we select "Selected Invoices" radio button and return
*B601648,1 from the scope screen by cancel then select "select all"
*B601648,1 Radio button then go to the "Selected Invoices" radio button
*B601648,1 again two or three times "Invalid subscript refrence" error
*B601648,1 will occur
*B601554,1 Hesham El-Sheltawi 03/09/97
*B601554,1 in the Apscope.scx  screen put picture for the invoice ref.
*B601554,1 and if the user remove the filter on a vendor code initialize
*B601554,1 the variable with null so it dont effect the filter
*B601648,4 RENEE 03/13/97 Select scope->Cancel --> wrong scope option
*B601569,1  HS  03/30/97  Get the default approved currency and exchange rate
*B601569,1                and aproved amounts in the case of cash payment
*B601667,1  HS  03/31/97  Fix the from currency code and the to currency
*B601667,1                code in the Message :
*B601667,1                    " A valid ð to ð exchange rate could not "
*B601667,1                    " be found for ð.                        "
*B601668,1  HS  04/03/97  Stop the message :
*B601668,1                "No Open invoices mateching selected invoices criteria."
*B601668,1                IF the user selected to scope then selected cansel
*E300643,1  HS 04/14/97 1)Make some changes for we have change the file
*E300643,1              SYCCODES name to CODES and make it a data file
*E300643,1              [Change its dir. from SYSFILES to DBFS]
*E300643,1              2)Make some changes for we have changed the function
*E300643,1              [gfCodDes]
*E301077,80 IHB  03/03/1999 Enhance opening and closing files
*E300789,4 IHB  07/03/1999 Remove company ID from ACCOD, FISHD, FSPRD, FSHLD, CODES
*B605939,1 Alb  06/30/2002 add inv. date and discount days 
*B607126,1 ALB 06/18/2003 Disable dicount/Adjs fields in case of inoice < 0
*B128400,1 MHM 10/04/2006 Prevent user from overwrite if two session opened
*B608903,1 TMI 06/22/2009 fix the bug that the opening the partial approve screen locks the APINVHDR record and does not release it
*B608942,1 TMI 07/22/2009 if a record is used by a user check that this user is in SYUSTATC of both A27 and A4XP [T20090224.0001]
*:************************************************************************************
*
PRIVATE laBankObjs 
DECLARE laPayMeth[1,2]  && Array to hold Popups.
DECLARE laWndObj[4,3], laBankObjs[3,3] 
** laWndObj has screen name & first & last obj. for each screen.
** laBankObjs has bank objects names for validations

** ibmethod     Variable to hold ib Method of payment.
** laCtrStat    To disable the browse pad in the menu
** cMarker      Vairable to mark current record.
**
** rbSort       Variable to hold rb Sort By.
** rbDuDsDt     Variable to hold rb due date and discount date.
** rbODuDsDt    Variable to hold rb due date and discount date.
** rbScope      Variable to hold rb Scope.
** rbAprAll     Variable to hold rb Apr with default or not in approve all. 
** rbScopeOn    Variable to hold rb Scope On in rb Scope.
**
** puDivDes     Variable to hold pop up division.
** puDiv        Variable to hold pop up division.
** puDivision   Variable to hold pop up division.
**
** lnOSort      Variable to hold old rb Sort By.
** lnOScope     Variable to hold old rb Scope.
** lnMethod     Variable to hold ib Method of payment.
** lnDivDes     Variable to hold pu Division description.
** lnPyChMN     Variable to Display payment method in scope screen.
** lnTotPay     Variable to hold Total Payment.
** lnTotDisc    Variable to hold Total Discount.
** lnTotAdj     Variable to hold Total Adjustment.
** lnBrRecNo    Browse record number 
** lnAprToPay   Variable to hold approve to pay amount.
** lnAprDisc    Variable to hold approve discount.
** lnAprAdj     Variable to hold approve adjustment.
** ln1099amnt   Variable to hold 1099 ammount.
** lnOldAprTPy  Variable to hold old approve to pay amount.
** lnOldAprDisc Variable to hold old approve discount.
** lnOldAprAdj  Variable to hold old approve adjustment.
** lnO1099amnt  Variable to hold old 1099 amount.
** lnSesAmnt    Variable to hold sesion approve amount.
** lnSesDisc    Variable to hold sesion approve discount.
** lnSesAdj     Variable to hold sesion approve adjustment.
** lnSes1099    Variable to hold sesion approve 1099.
** lnScopeOn    Variable to hole rb scope old value.
**
** lc1099St     Variable to hold approve 1099 state.
** lcFuncName   Variable to hold Refresh window. 
** lcVenSum     Vendor  summary window name
** lcInvSum     Invoice summary window name
** lcBrowName   Variable to hold Brows Window name. 
** lcReadNam1   Variable to hold Read Window name.
** lcReadNam2   Variable to hold Read Window name.
** lcExprsion   Variable to hold filter expresion.
** lcOldBank    Variable to hold old bank code.
** lcOldCheck   Variable to hold old check code. 
** lcOldGlAcc   Variable to hold old GL account.
** lcHOldGlAc   Variable to hold old cash account.
** lcOldAcc     Variable to hold    
** lcOldGLDes   Variable to hold old Account description.
** lcTAprAmnt   The Approve amount in approve message.
** lcDivision   Variable to hold division description.
** lcCInvRef    Variable to hold current Inv. Ref.
** lcPriority   Variable to hold Paroirity.
** lcPaymeth    Variable to hold pay method.
** lcOldDvCod   Variable to hold old division code.
** lcOMethod    Variable to hold old method.
** lcVend       Variable to hold Vendor Code to get tot payment for each vend.
** lcVendor     Variable to hold vendor code.
** lcTInvoice   Variable to hold word Invoice.
** lcTDebitM    Variable to hold word debit memo.
** lcTThrou     Variable to hold word through.
** lcTFrom      Variable to hold word From.
** lcOldPrty    Variable to hold old Paroirity.
** lcOldpymth   Variable to hold old pay method.
** lcTBlnkCode  Variable to hold text not aplicable.
** lcDivDisc    Variable to hold division discription.
** lcTInvNo     Variable to hold text
** lcTVendor    Variable to hold text
** lcTPrior     Variable to hold text
** lcTAprPay    Variable to hold text
** lcTAprDisc   Variable to hold text
** lcTAprAdj    Variable to hold text
** lcTApr1099   Variable to hold text
** lcTOpnAmnt   Variable to hold text
** lcTPayMeth   Variable to hold text
** lcTInvDate   Variable to hold text
** lcTDivDes    Variable to hold text
** lcTBank      Variable to hold text
** lcTGlAcct    Variable to hold text
** lcCurYear    Variable to hold current year.
** lcDivCode    Variable to hold division code.
** lcBrowFile   Vairable to hold the Browse File.
** lcVendTtl    Variable to hold vendor window title.
** lcInvoice    Variable to hold invoice window title.
** lcBrTtl      Variable to hold Browse Window Title.
** lcBankCode   Variable to hold bank code.
** lcCheckcode  Variable to hold check code. 
** lcGlAcct     Variable to hold GL account.
** lcHGlAct     Variable to hold cash account.
** lcInvRef     Variable to hold Inv. Ref.
** lcOldInvRf   Variable to hold old Inv. Ref.
** lcCodeFilt   Variable to hold field filter. 
** lcColPair    Variable to hold 
** lcColor      Variable to hold 
** lcVendCode   Variable to hold vendor code initial val. must be null.
** lcOldVndcd   Variable to hold old vendor code initial val. must be null.
** lcVendComp   Variable to hold vendor company.
** lcOVendCod   Variable to hold old vendor code.
** lcOldVnCmp   Variable to hold old vendor company.
** lcOldDivDs   Variable to hold old division description.
** lcEscTrp     Variable to hold escape trap.
** lcOldVal     Variable to hold old field value from when functions
*E300316,1 HISH 12/05/95.  Added currency equation sign variables. (Begin)
** in case of exchange currency from invoice currency to company base currency.
** lcExSin1             Variable to hold the first sign in the equation.
** lcExSin2             Variable to hold the second sign in the equation.
** in case of exchange currency from invoice currency to approved currency.
** lcExSin3             Variable to hold the first sign in the equation.
** lcExSin4             Variable to hold the second sign in the equation.
*E300316,1 (End)

** ldFrDueDat   Variable to hold from due date.
** ldToDueDat   Variable to hold to due date.
** ldFrDisDat   Variable to hold from discount date.
** ldToDisDat   Variable to hold to discount date. 
** ldOFrDueDt   Variable to hold old from due date.
** ldOToDueDt   Variable to hold old to due date.
** ldOFrDisDt   Variable to hold old from discount date.
** ldOToDisDt   Variable to hold old to discount date. 
**
** llScope      Variable to hold if you want scope to be done.
** llAprPart    Variable to hold parameter to branch between appr. fully and appr. part.
** llBrowse     Variable to hold flag in case of activate browse by mouse.
** llCanclAp    True if 'Cancel' is selected
** llVendSumm   True if vendor screen is open
** llExit       Flag indecate if the user want to exit thermo.
** llVendExst   If Vendor object is exist in scope screen.
** llNoContrl   No control panel (AP.PRG)
*E300296,4 lcTAprCurCod Text of field header of approved currency field
*E300296,4 lcTPayAcct   Text of cash payment account check
STORE ' '  TO lc1099St   , lcFuncName , lcVenSum   , lcInvSum   ,;
              cMarker    , lcBrowName , lcReadNam1 , lcReadNam2 ,;
              lcExprsion , lcOldBank  , lcOldCheck , lcOldGlAcc ,;
              lcHOldGlAc , lcOldAcc   , lcOldGLDes , lcTAprAmnt ,;
              lcDivision , lcCInvRef  , lcPriority , lcPaymeth  ,;
              lcOldDvCod , lcOMethod  , lcVend     , lcVendor   ,;
              lcTInvoice , lcTDebitM  , lcTThrou   , lcTFrom    ,;
              lcOldPrty  , lcOldpymth , lcOldVal   , lcTBlnkCode,;
              lcDivDisc  , lcTInvNo   , lcTVendor  , lcTPrior   ,;
              lcTAprPay  , lcTAprDisc , lcTAprAdj  , lcTApr1099 ,;
              lcTOpnAmnt , lcTPayMeth , lcTInvDate , lcTDivDes  ,;
              lcTBank    , lcTGlAcct  , lcCurYear  , puDiv      ,;
              puDivision , lcTAprCurCod, lcTPayAcct, lcExSin1   ,;
              lcExSin2   , lcExSin3    , lcExSin4

              

STORE ''   TO lcVendCode , lcOldVndcd , lcVendComp , lcOVendCod ,;
              lcOldVnCmp , lcOldDivDs , lcEscTrp

STORE 1    TO rbSort     , lnOSort    , rbScope    , lnOScope   ,;
              rbAprAll   , ibmethod   , puDivDes   , lnMethod   ,;
              rbDuDsDt   , rbODuDsDt  , lnDivDes

STORE 0    TO lnPyChMN   , lnTotPay   , lnTotDisc  , lnTotAdj   ,;
              lnBrRecNo

STORE 0.00 TO lnAprToPay , lnAprDisc  , lnAprAdj   , ln1099amnt ,;
              lnOldAprTPy, lnOldAprAdj, lnO1099amnt, lnSesAmnt  ,;
              lnSesDisc  , lnSesAdj   , lnSes1099  , lnOldAprDisc
              

STORE 3    TO rbScopeOn  , lnScopeOn

STORE {}   TO ldFrDueDat , ldToDueDat , ldFrDisDat , ldToDisDat ,;
              ldOFrDueDt , ldOToDueDt , ldOFrDisDt , ldOToDisDt

STORE .F.  TO llScope    , llAprPart  , llBrowse   , llCanclAp  ,;
              llVendSumm , llExit
STORE .T.  TO llVendExst ,llNoContrl

STORE SPACE(8)     TO lcBankCode
STORE SPACE(12)    TO lcCheckcode
STORE SPACE(15)    TO lcInvRef,lcOldInvRf
STORE lcEmptyAcc   TO lcGlAcct,lcHGlAct
STORE SCHEME(1,2)  TO lcColPair,lcColor
STORE "Invoices"   TO lcInvoice,lcBrTtl
STORE "*"          TO lcDivCode  
STORE "APINVHDR"   TO lcBrowFile  
STORE "Vendors"    TO lcVendTtl
STORE "CDIVISION " TO lcCodeFilt  
STORE "DISABLE"    TO laCtrStat

lcHidObjDlg = IIF(_WINDOWS,',,,,,,,,,RGB(192,192,192,192,192,192)',lcHidObjDlg)  
lcgry1      = IIF(rbDuDsDt=2,"RGB(192,192,192,192,192,192),,,,,,,,,",;
                  "RGB(128,128,128,128,128,128),,,,,,,,,")
lcwht1      = IIF(rbDuDsDt=2,"RGB(192,192,192,192,192,192),,,,,,,,,",;
                  "RGB(255,255,255,255,255,255),,,,,,,,,")                
lcgry2      = IIF(rbDuDsDt=1,"RGB(192,192,192,192,192,192),,,,,,,,,",;
                  "RGB(128,128,128,128,128,128),,,,,,,,,")
lcwht2      = IIF(rbDuDsDt=1,"RGB(192,192,192,192,192,192),,,,,,,,,",;
                  "RGB(255,255,255,255,255,255),,,,,,,,,")                

IF !gfSetup()
  RETURN
ENDIF

*E300296,4 Prepare currency fields
*E300296,4 lcAprCurCod  : Approval currency code
*E300296,4 lcAprCshCod  : Approval currency code
*E300296,4 lnAprExRat   : Approval currency exchange rate between the 
*E300296,4                approval currency and the invoice currency
*E300296,4 lnAprCshRat  : Cash payment exchange rate field in APAPRALL.SPR
*E300296,4 lnAprCurUnt  : Approval currency unit
*E300296,4 lnExchAmnt   : Approved amount in approval currency
*E300296,4 llMultiCr    : .T. if multi currency
*E300296,4 llEdirEx     : .T. if editing an exchange rate per transaction
*E300296,4                is allowed
*E300296,4 llExchRt     : .T. if adding an exchange rate on the fly is allowed
*E300296,4 lcRateDisp   : Display status of the exchange rate field,
*E300296,4                in Approve partially screen, whether enabled
*E300296,4                or disabled
*E300296,4 lnOldVal     : old exchange rate value
*E300296,4 lcOldCurr    : old lcAprCurCod
*E300296,4 lnOldUnt     : old lnAprCurUnt
*E300296,4 lnOldExRat   : old lnAprExRat
*E300296,4 lcTExRateMsg : message text
*E300683,1 AHMED 06/04/97 Add screens directory path to the calling of SPRS


STORE SPACE(5) TO lcAprCurCod, lcOldCurr, lcAprCshCod
STORE 0 TO lnAprExRat, lnExchAmnt, lnAprCurUnt, lnOldVal,;
           lnOldUnt, lnOldExRat, lnAprCshRat
lcTExRateMsg = 'The exchange rate|zero'
IF gfGetMemVar('LLMULCURR')
  llMultiCr  = .T.
  llEditEx    = gfGetMemVar('LLEDITEXRA')
  llExchRt    = gfGetMemVar('LLEXCHRATE')
  lcBaseSmbl  = IIF(SEEK(gcBaseCurr, 'SYCCURR'),;
                    ALLTRIM(SYCCURR.cCurrSmbl), '')
ELSE
  STORE .F. TO llMultiCr, llEditEx, llExchRt
  lcBaseSmbl  = ''
ENDIF
*E300296,4 end.

*** Get current year form parent company. 
SELECT SYCCOMP
lcSavOrd = ORDER() 
SET ORDER TO CCOMP_ID

IF SEEK(gcPrnt_Cmp)
   lcCurYear=CCURR_YER
ENDIF 

IF !EMPTY(lcSavOrd)
  SET ORDER TO &lcSavOrd
ENDIF  

*** Get payment methods array
lnPayMLen = gfGetVld('CVENPMETH',@laPayMeth,.T.)
lnAryPos = ASCAN(laPayMeth,'C',1)
IF lnAryPos > 0	
  =ADEL(laPayMeth,ASUBSCRIPT(laPayMeth, lnAryPos, 1)) 
  DIMENSION laPayMeth[ALEN(laPayMeth,1)-1,2]  
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


*** Get chart of accounts ***
*** Get names to Brawse and Read window ***
IF !WEXIST(gcBaseWind)

  *** Variable holding Filter expresion ***
  lcExprsion = "((APINVHDR.NINVAMNT -APINVHDR.NINVPAID - "+;
               "APINVHDR.NINVDISTK -APINVHDR.NINVADJ) <> 0)"

  *E300643,1 Change this line for the changes we have made 
  *          to (gfCodDes) [Begin]
  *lcTBlnkCode = gfCodDes(' ')
  lcTBlnkCode = gfCodDes(' ' , ' ')
  *E300643,1 Change this line [End]
  
  lcBrowName  = gfTempName()   
  lcReadNam1  = gfTempName()
  lcReadNam2  = gfTempName()
  lcVenSum    = gfTempName()
  lcVenSum    ="CWR"+SUBSTR(lcVenSum,4)
  lcInvSum    = gfTempName()
  lcInvSum    ="CWR"+SUBSTR(lcInvSum,4)

  *** Prepare an array to hold bank objects to be used for
  *** global bank and checking accounts validations as follows :
  *** One row for every object, such that 
  *** row no. 1 holds bank object names,
  *** row no. 2 holds checking account object names
  *** row no. 3 holds the corresponding G/L account object names,
  *** Columns are ordered as follows :
  *** Column no. 1 : invisible button name for corresponding object
  *** Column no. 2 : object name (e.g. bank object name)
  *** Column no. 3 : object description name(if required)
  laBankObjs  = ' '
  laBankObjs[1,1] = 'ibBank'         && Bank code invisible button
  laBankObjs[1,2] = 'lcBankCode'     && Bank code 
  laBankObjs[2,1] = 'ibChecks'       && Checking account invisible button
  laBankObjs[2,2] = 'lcCheckCode'     && Checking account 
  laBankObjs[3,1] = 'ibGlAcc'        && G/L account invisible button  
  laBankObjs[3,2] = 'lcGlAcct'     && G/L account
   
ELSE && If window exist
  rbScopeOn=lnScopeOn  && restoring radio button value.
  IF _DOS && Return the popup values
    lcPayMeth = lcOldpymth
    lcDivCode = lcOldDvCod
  ENDIF  
  lcMethod   = lcOMethod
  rbScope    = lnOScope
  rbSort     = lnOSort
  IF rbScope = 1
    lcExprsion = "((APINVHDR.NINVAMNT -APINVHDR.NINVPAID - "+;
                  "APINVHDR.NINVDISTK -APINVHDR.NINVADJ) <> 0)"

  ELSE
    =lfvOk() && returning the existing browse filter.
  ENDIF    
ENDIF

*** fill the array holding frist and last object in base window and each children
laWndObj [1,1] = lcReadNam1
laWndObj [1,2] = "RBSORT"
laWndObj [1,3] = "pbClose"

laWndObj [2,1] = lcReadNam2
laWndObj [2,2] = "RBSORT"
laWndObj [2,3] = "pbClose"

laWndObj [3,1] = lcVenSum
laWndObj [3,2] = "PBVECLOSE"
laWndObj [3,3] = "PBVECLOSE"

laWndObj [4,1] = lcInvSum
laWndObj [4,2] = "PBINCLOSE"
laWndObj [4,3] = "PBINCLOSE"

*** store scape trap.
lcEscTrp=ON("KEY","ESC")
*** Trap some keys before browse. ***
PUSH KEY
=lfPushKey()

*** Get Codes from codes file.

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

*E300789,4 IHB Remove company ID [start]
*SET FILTER TO (CCOMP_ID+CRLTFIELD+CFLD_NAME = gcAct_Comp+'N'+lcCodeFilt) OR;
              (CCOMP_ID+CRLTFIELD+CFLD_NAME ='  '+'N'+'N/A') OR;
              (CCOMP_ID+CRLTFIELD+CFLD_NAME ='  '+'N'+'ALL')
SET FILTER TO (CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N'+lcCodeFilt) OR;
              (CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N'+'N/A') OR;
              (CDEFCODE+CRLTFIELD+CFLD_NAME = 'N'+'N'+'ALL')
*E300789,4 IHB [end]

*** Set relation between invoice header, vendor, and division file ***

SELECT APVENHST
SET ORDER TO TAG VENDYEAR

SELECT APVENDOR
SET ORDER TO TAG VENCODE

SELECT APDIV
SET ORDER TO TAG DIVISION

SELECT APINVHDR
SET ORDER TO TAG VENDINV

SET RELATION TO APINVHDR.CVENDCODE+lcCurYear INTO APVENHST ADDITIVE

SET RELATION TO APINVHDR.CVENDCODE           INTO APVENDOR ADDITIVE
SET RELATION TO APINVHDR.CDIVISION           INTO APDIV ADDITIVE

lcSavOrd = ORDER() 
SET ORDER TO 0
SET FILTER TO APINVHDR.CVENDCODE+APINVHDR.CINVNO = lcVendCode .AND.;
              CINVSTAT<>'V' .AND.  EVALUATE(lcExprsion)
SET ORDER TO &lcSavOrd
GO TOP    &&Go to the top of the file. 

*E300316,1 HISH 12/05/95. Got the equation signs. (Begin)
*E300316,1 to exchange currency from invoice currency to approved currency.
*E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
lcExSin4 = ' '

*B601416,1 Change the calling of gfGetExSin so if the [BEGIN]
* Invoice currency is the base currency I make it the To currency 
* and if not the Approve currency is the To currency 
*lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,APINVHDR.cAprCurCod)
*B601416,1 IF Statment to check if the Invoice currency is the 
* same as the base currency
IF APINVHDR.cCurrCode = gcBaseCurr 
  lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cAprCurCod,APINVHDR.cCurrCode)
  lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
  lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
ELSE   && Else
  lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,APINVHDR.cAprCurCod)
ENDIF  && End of IF 
*B601416,1 Change the calling of gfGetExSin [END]

*lcExSin4 = IIF(lcExSin3 = '*','/','*')
*E300316,1 (End)

IF EOF()
  ** MESSAGE : "There are no open invoices."
  **           "        ® Ok ¯            "
  =gfModalGen("TRM04101B00000","DIALOG",'open invoices')
  glQuitting=.T.  
ELSE  
  IF EMPTY(APSETUP.CBNKCODE) .OR. EMPTY(APSETUP.CCHKACCT)
    ** MESSAGE : "You have to enter default bank code and checking account"
    **           "in the accounts payable setup. "
    **           "            ® Ok ¯             "
    =gfModalGen("TRM04103B00000","DIALOG")
    glQuitting=.T.  
  ELSE
    *E300683,1 Call *.SPR from screens directory
    * DO APAPRPA.SPR 
    DO (gcScrDir + gcWinAppl + '\APAPRPA.SPR')
    *E300683,1 end          
    
  ENDIF  
ENDIF

*** restores on key labels that were placed on the stack with push key. ***
POP KEY

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

SET FILTER TO 

RELEASE PAD _BROWSE OF _MSYSMENU
SELECT APINVHDR

*** Clear relation ***
SET FILTER TO
SET RELATION TO

RELEASE PAD _BROWSE OF _MSYSMENU
***Release the browse window.
RELEASE WINDOW (lcBrTtl)

IF glQuitting
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
* Display browse.
FUNCTION lfDispBrow
PRIVATE lcCurAlias

lcCurAlias = ALIAS()

SELECT APINVHDR
lnBrRecNo  = RECNO('APINVHDR')

*B600492,1 Change the browse fields width (Invoice no - vendor code).
*E300258,4 MAN Add the check on supressing 1099 processing

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*lcBrowStr = "cMarker=IIF(RECNO()=lnBrRecNo,'',' '):1:H=' ':W=.F.,"+; 
*            "CINVNO    :12:R :H=lcTInvNo,"+;
*            "CVENDCODE :8:R :H=lcTVendor,"+;
*            "CVENPRIOR :R :H=lcTPrior,"+;
*	         "NINVAMTAP=IIF(EMPTY(NINVAMTAP),0.00,NINVAMTAP) :R :H=lcTAprPay,"+;
*	         "NINVDISAP=IIF(EMPTY(NINVDISAP),0.00,NINVDISAP) :R :H=lcTAprDisc,"+;
*	         "NINVADJAP=IIF(EMPTY(NINVADJAP),0.00,NINVADJAP) :R :H=lcTAprAdj,"+;
*	         IIF(llApS1099,"","NINVA1099=IIF(EMPTY(NINVA1099),0.00,NINVA1099) :R :H=lcTApr1099,")+;
*	         "NINVAMNT=(NINVAMNT - NINVPAID - NINVDISTK - NINVADJ):R :H=lcTOpnAmnt,"+;
* 	         "PayMeth=lfGetPayMeth(CVENPMETH):H=lcTPayMeth:18,"+;
*            "DINVDUDAT:R :H=lcTInvDate,"+;
*            "lcDivDisc=IIF(EMPTY(CDIVISION),lcTBlnkCode,LOOKUP(SYCCODES.cDiscrep,gcAct_Comp+APINVHDR.CDIVISION,SYCCODES.cCode_No,'CODES')):R :10:H=lcTDivDes,"+;	      
*	         "CBNKCODE :H=lcTBank,"+;
*            "CCHKACCT :H='Checking Account', CCHKGLACC :H=lcTGlAcct"

*E300789,4 IHB Remove company ID [start]
*lcBrowStr = "cMarker=IIF(RECNO()=lnBrRecNo,'',' '):1:H=' ':W=.F.,"+; 
            "CINVNO    :12:R :H=lcTInvNo,"+;
            "CVENDCODE :8:R :H=lcTVendor,"+;
	        "CVENPRIOR :R :H=lcTPrior,"+;
	        "NINVAMTAP=IIF(EMPTY(NINVAMTAP),0.00,NINVAMTAP) :R :H=lcTAprPay,"+;
	        "NINVDISAP=IIF(EMPTY(NINVDISAP),0.00,NINVDISAP) :R :H=lcTAprDisc,"+;
	        "NINVADJAP=IIF(EMPTY(NINVADJAP),0.00,NINVADJAP) :R :H=lcTAprAdj,"+;
	        IIF(llApS1099,"","NINVA1099=IIF(EMPTY(NINVA1099),0.00,NINVA1099) :R :H=lcTApr1099,")+;
	        "NINVAMNT=(NINVAMNT - NINVPAID - NINVDISTK - NINVADJ):R :H=lcTOpnAmnt,"+;
  	        "PayMeth=lfGetPayMeth(CVENPMETH):H=lcTPayMeth:18,"+;
            "DINVDUDAT:R :H=lcTInvDate,"+;
            "lcDivDisc=IIF(EMPTY(CDIVISION),lcTBlnkCode,LOOKUP(CODES.cDiscrep,gcAct_Comp+APINVHDR.CDIVISION,CODES.cCode_No,'CODES')):R :10:H=lcTDivDes,"+;	      
	        "CBNKCODE :H=lcTBank,"+;
            "CCHKACCT :H='Checking Account', CCHKGLACC :H=lcTGlAcct"

*B605939,1 Alb  Albert add inv. date and dicoount days Begin
*lcBrowStr = "cMarker=IIF(RECNO()=lnBrRecNo,'',' '):1:H=' ':W=.F.,"+; 
            "CINVNO    :12:R :H=lcTInvNo,"+;
            "CVENDCODE :8:R :H=lcTVendor,"+;
	        "CVENPRIOR :R :H=lcTPrior,"+;
	        "NINVAMTAP=IIF(EMPTY(NINVAMTAP),0.00,NINVAMTAP) :R :H=lcTAprPay,"+;
	        "NINVDISAP=IIF(EMPTY(NINVDISAP),0.00,NINVDISAP) :R :H=lcTAprDisc,"+;
	        "NINVADJAP=IIF(EMPTY(NINVADJAP),0.00,NINVADJAP) :R :H=lcTAprAdj,"+;
	        IIF(llApS1099,"","NINVA1099=IIF(EMPTY(NINVA1099),0.00,NINVA1099) :R :H=lcTApr1099,")+;
	        "NINVAMNT=(NINVAMNT - NINVPAID - NINVDISTK - NINVADJ):R :H=lcTOpnAmnt,"+;
  	        "PayMeth=lfGetPayMeth(CVENPMETH):H=lcTPayMeth:18,"+;
            "DINVDUDAT:R :H=lcTInvDate,"+;
            "lcDivDisc=IIF(EMPTY(cDivision),lcTBlnkCode,LOOKUP(CODES.cDiscrep,'N'+APINVHDR.CDIVISION+'N'+'CDIVISION',CODES.cCode_No,'CODES')):R :10:H=lcTDivDes,"+;
	        "CBNKCODE :H=lcTBank,"+;
            "CCHKACCT :H='Checking Account', CCHKGLACC :H=lcTGlAcct"
*E300789,4 IHB Remove company ID [end]


lcBrowStr = "cMarker=IIF(RECNO()=lnBrRecNo,'',' '):1:H=' ':W=.F.,"+; 
            "CINVNO    :12:R :H=lcTInvNo,"+;
            "CVENDCODE :8:R :H=lcTVendor,"+;
	        "CVENPRIOR :R :H=lcTPrior,"+;
	        "NINVAMTAP=IIF(EMPTY(NINVAMTAP),0.00,NINVAMTAP) :R :H=lcTAprPay,"+;
	        "NINVDISAP=IIF(EMPTY(NINVDISAP),0.00,NINVDISAP) :R :H=lcTAprDisc,"+;
	        "NINVADJAP=IIF(EMPTY(NINVADJAP),0.00,NINVADJAP) :R :H=lcTAprAdj,"+;
	        IIF(llApS1099,"","NINVA1099=IIF(EMPTY(NINVA1099),0.00,NINVA1099) :R :H=lcTApr1099,")+;
	        "NINVAMNT=(NINVAMNT - NINVPAID - NINVDISTK - NINVADJ):R :H=lcTOpnAmnt,"+;
  	        "PayMeth=lfGetPayMeth(CVENPMETH):H=lcTPayMeth:18,"+;
            "DINVDATE:R :H=lcTInvDate,"+ "DINVDUDAT:R :H='Due Date',"+;
            "NTERDISCD:R :H='Discount Days',"+;
            "lcDivDisc=IIF(EMPTY(cDivision),lcTBlnkCode,LOOKUP(CODES.cDiscrep,'N'+APINVHDR.CDIVISION+'N'+'CDIVISION',CODES.cCode_No,'CODES')):R :10:H=lcTDivDes,"+;
	        "CBNKCODE :H=lcTBank,"+;
            "CCHKACCT :H='Checking Account', CCHKGLACC :H=lcTGlAcct"            

*B605939,1 Alb   Albert add inv. date and dicoount days End

*E300643,1 Change this line for the changes we have made to SYCCODES [End]

IF _DOS
  BROWSE FIELDS &lcBrowStr;
            WINDOW (lcBrowName) ;
            WHEN lfwInvBrow();
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
ELSE
  BROWSE FIELDS &lcBrowStr;
            WINDOW (lcBrowName) ;
            WHEN lfwInvBrow();
            VALID :F lfvBrowse();
            IN WINDOW (gcBaseWind) ;
            LOCK 0;
            NOAPPEND;
            NOCLEAR;
            NODELETE;
            NOWAIT;
            NOEDIT;
            SAVE;
            NOMENU;
            TITLE lcBrTtl
ENDIF

SELECT (lcCurAlias)

*!**************************************************************************
*!
*!        Function : lfvBrowse
*!
*!**************************************************************************
* 
FUNCTION lfvBrowse

IF WONTOP(lcBrTtl)   && Close from corner in dos. (browse exist,brows window not exist)
  IF !WEXIST(lcBrowName) 
    glFromBrow = .F.
    glQuitting = .T.
    CLEAR READ
    KEYBOARD CHR(13)
    RETURN TO APAPRPA.SPR
  ELSE
    IF PROG()='GPMENUBAR'
      =lfBrowUnTrap()
      =gfStopBrow()  
    ENDIF  
  ENDIF
ELSE                 && Close from corner in windows. (browse not exit, base window not vis.)
  =lfBrowUnTrap()
  IF !WVISIBLE(gcBaseWind)
    glQuitting = .T.
    *B600643,1 Move the stop browse in the case the window is closed but the browse still exist
    =gfStopBrow()     
  ENDIF  
  *B600643,1 Move the stop browse in the case the window is closed but the browse still exist
  *=gfStopBrow()     
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

IF lnposition <> 0	
  RETURN laPayMeth[ASUBSCRIPT(laPayMeth,lnposition,1),1] 
ELSE
  RETURN " " 
ENDIF

*!**************************************************************************
*!
*!      Function: lfwInvBrow
*!
*!**************************************************************************
* Fill variable from record.
*
FUNCTION lfwInvBrow

lcVendor    = CVENDCODE
*B600808,1 No need for filling the variables here.
*lcBankCode  = CBNKCODE 
*lcCheckCode = CCHKACCT
*lcGLAcct    = IIF(!EMPTY(CCHKGLACC),CCHKGLACC,lcEmptyAcc)  
*lcHGlAct    = lcGLAcct
*lnAprToPay  = NINVAMTAP
*lnAprDisc   = NINVDISAP
*lnAprAdj    = NINVADJAP
*ln1099amnt  = NINVA1099
*B600808,1 end.

lnBrRecNo   = RECNO('APINVHDR')
SHOW WINDOW (lcBrTtl) REFRESH
IF _DOS
  SHOW GET pbInvoice,1 PROMPT 'I\<nv. '+ALLTRIM(APINVHDR.CINVNO)
ENDIF

IF lcVend <> APINVHDR.CVENDCODE .AND. WVISIBLE(lcVenSum)
   lcVend=APINVHDR.CVENDCODE
  =lfvTot()
ENDIF

*E300316,1 HISH 12/05/95. Got the equation signs. (Begin)
*E300316,1 to exchange currency from invoice currency to approved currency.
*E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
lcExSin4 = ' '

*B601416,1 Change the calling of gfGetExSin so if the [BEGIN]
* Invoice currency is the base currency I make it the To currency 
* and if not the Approve currency is the To currency 
*lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,APINVHDR.cAprCurCod)
*B601416,1 IF Statment to check if the Invoice currency is the 
* same as the base currency
IF APINVHDR.cCurrCode = gcBaseCurr 
  lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cAprCurCod,APINVHDR.cCurrCode)
  lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
  lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
ELSE   && Else
  lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,APINVHDR.cAprCurCod)
ENDIF  && End of IF 
*B601416,1 Change the calling of gfGetExSin [END]

*lcExSin4 = IIF(lcExSin3 = '*','/','*')
*E300316,1 (End)
=lfREFRESH()

*!**************************************************************************
*!
*!      Function: lfwAccount
*!
*!**************************************************************************
* Get a description of a certain code.
*
FUNCTION lfwAccount
PARAMETERS lcAccount

lcOldGlAcc = lcAccount
lcHOldGlAc = lcAccount

*!**************************************************************************
*!
*!      Function: lfvlcGlact
*!
*!**************************************************************************
* Get AP accounts codes and description. 
*
FUNCTION lfvlcGlact

=lfApAcs()

=gfUpdate()

=lfRefresh() 

*!**************************************************************************
*!
*!      Function: lfvSortBy
*!
*!**************************************************************************
* Order the browse according to vindor or invoice.
FUNCTION lfvSortBy

IF rbSort = 1
  SET ORDER TO TAG VENDINV
ELSE
  SET ORDER TO TAG INVVEND
ENDIF

lnOSort = rbSort && save rb to return back if we go to another screen  

lnBrRecNo   = RECNO('APINVHDR') 

SHOW WINDOW (lcBrTtl) REFRESH

*!**************************************************************************
*!
*!      Function: lfvScope
*!
*!**************************************************************************
*
FUNCTION lfvScope

*** If you want to display all invoices. ***
IF rbScope = 1
  lcVendCode=""               && Return to initial vendor state.
  lcVendComp=SPACE(30)
  *** Fill expresion with initial filter. ***
  lcExprsion = "((APINVHDR.NINVAMNT - APINVHDR.NINVPAID - "+;
               "APINVHDR.NINVDISTK -APINVHDR.NINVADJ) <> 0).AND."+;
               "(CINVSTAT<>'V')"
  
ELSE && If the user want to select certain invoices.
  PUSH KEY  && restore the keys traped
  ON KEY 

  RELEASE PAD _BROWSE OF _MSYSMENU

  *B601648,4 Store the old division value to old division code, so that 
  *B601648,4 if the user does not select a scope, old values are returned
  lcOldDvCod = lcDivCode
  *B601648,4 end
  
  *** ON HIGHER LEVEL CREATE VARIABLES TO HOLD OLD VAR. ***
  *** SAVE CURRENT VARIABLES IN OLD VAR.
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
  *** Push the same keys again after coming ***
  *** from global function browse. ***
  POP KEY
  
  *B601668,1 Remove this line to keep the flag [Begin]
  *IF llScope = .T. && if the user push <ok> to do the filter he selected.
  *  llScope = .F.
  *ENDIF  
  *B601668,1 Remove this line to keep the flag [End]

ENDIF  

GO TOP  && Refreshing browse filter.
lnBrRecNo   = RECNO('APINVHDR') 
SHOW WINDOW (lcBrTtl) REFRESH  && refresh the browse window.
 
*** If the browse has no elements colse all branches no need to use.***
IF EOF() 
  IF _DOS
    SHOW GET pbInvoice,1 PROMPT 'I\<nvoice... '
  ENDIF  
  SHOW GET pbVendor  DISABLE
  SHOW GET pbInvoice DISABLE
  SHOW GET pbDAprAll DISABLE
  SHOW GET pbAprAll  DISABLE
  SHOW GET pbAprpart DISABLE
  SHOW GET pbAprFul  DISABLE
  SHOW GET pbDAprprt DISABLE
  SHOW GET rbSort    DISABLE    
  IF WVISIBLE(lcVenSum) && close vendor window if it's opened.
    =gfChClose(lcVenSum)
  ENDIF  
  IF WVISIBLE(lcInvSum) && close invoice window if it's opened.
    =gfChClose(lcInvSum)
  ENDIF

  *B601668,1 Add this lines to add a condition to stop the message 
  *          IF the user selected to scope then selected cansel [Begin]
  IF rbScope = 1 .OR. llScope 
    IF llScope .AND. rbScope <> 1
      llScope = .F.    && Clear the flag
    ENDIF    && End of IF
  *B601668,1 Add this lines [End]

    ** MESSAGE : "No Open invoices mateching selected invoices criteria."
    **           "                       ® Ok ¯ 
    =gfModalGen("TRM04089B00000","DIALOG",'open invoices')

  *B601668,1 Add this line to add a condition to stop the message 
  *          IF the user selected to scope then selected cansel [Begin]
  ENDIF    && End of IF
  *B601668,1 Add this line [End]

ELSE && if the browse has elements.
  IF _DOS
    SHOW GET pbInvoice,1 PROMPT 'I\<nv. '+ALLTRIM(APINVHDR.CINVNO)
  ENDIF
  SHOW GET pbVendor  ENABLE
  SHOW GET pbInvoice ENABLE
  SHOW GET pbDAprAll ENABLE
  SHOW GET pbAprAll  ENABLE
  SHOW GET pbAprpart ENABLE
  SHOW GET pbAprFul  ENABLE
  SHOW GET pbDAprprt ENABLE
  SHOW GET rbSort    ENABLE
ENDIF
lnOScope   = rbScope
=lfRefresh()

*!**************************************************************************
*!
*!      Function: lfvVendor
*!
*!**************************************************************************
*** Activate verndor window. ***
*
FUNCTION lfvVendor
=lfvTot()
RELEASE PAD _BROWSE OF _MSYSMENU
*** Call the child screen through the global function activate window. ***
=gfActWind(lcVenSum,lcVendTtl,gcBaseWind)
IF _DOS
  =lfDefinePad()
ENDIF

*!**************************************************************************
*!
*!      Function: lfvTot
*!
*!**************************************************************************
*** Activate verndor window. ***
*
FUNCTION lfvTot

IF rbSort = 2
  SET ORDER TO TAG VENDINV
ENDIF

lnRecNo = RECNO()
IF EMPTY(lcVendor)
  lcVendor= APINVHDR.CVENDCODE
ENDIF

IF SEEK(lcVendor,'APINVHDR')
  SUM REST APINVHDR.NINVAMTAP,APINVHDR.NINVDISAP,APINVHDR.NINVADJAP;
       TO  lnTotPay          ,lnTotDisc         ,lnTotAdj ;
       WHILE APINVHDR.CVENDCODE = lcVendor
ENDIF
IF rbSort = 2
  SET ORDER TO TAG INVVEND
ENDIF
IF lnRecNo <= RECCOUNT()  && be sure that the is not empty.
  GO lnRecNo      
ENDIF
=lfRefresh()

*!**************************************************************************
*!
*!      Function: lfvInvoice
*!
*!**************************************************************************
*** Activate invoice window. ***
*
FUNCTION lfvInvoice

RELEASE PAD _BROWSE OF _MSYSMENU
*** Call the child screen through the global function activate window. ***
=gfActWind(lcInvSum,lcInvoice,gcBaseWind)
IF _DOS
  =lfDefinePad()
ENDIF
  
*!**************************************************************************
*!
*!      Function: lfvClose
*!
*!**************************************************************************
*
FUNCTION lfvClose

IF !WONTOP(lcBrTtl)
  *MAN
  *IF _DOS
    *** Close program. ***
    glQuitting = .T.
    CLEAR READ
  *ELSE
  *  &lcEscTrp
  *ENDIF
ELSE && IF EXIT FROM BROWSE WINDOW
  HIDE WINDOW(gcBaseWind)
  *** Close program. ***
  glFromBrow = .F.
  glQuitting = .T.
  CLEAR READ
  KEYBOARD CHR(13)
  RETURN TO APAPRPA.SPR
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvDisAppr
*!
*!**************************************************************************
* Disaprove the pointed record. 
FUNCTION lfvDisAppr

*E300316,1 HISH 12/05/95. Got the equation signs. (Begin)
*E300316,1 to exchange currency from invoice currency to company base currency.
*E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
lcExSin2 = ' '
lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
*lcExSin2 = IIF(lcExSin1 = '*','/','*')
*E300316,1 (End)

*:->B128400 new function to modify message appear [Start]
*IF gfObj_lock(.T.) && lock the pointed record.
IF lfObj_lock(.T.) && lock the pointed record.
*:->B128400 [End]

   *** intialize the approved value.***
   *E300296,4 Show totals in base currency
   *lnSesAmnt = lnSesAmnt  - NINVAMTAP
   *lnSesDisc = lnSesDisc  - NINVDISAP
   *lnSesAdj  = lnSesAdj   - NINVADJAP
   *lnSes1099 = lnSes1099  - NINVA1099
   *E300316,1 HISH 12/05/95. Used the variables hold signs in the equation. (Begin)                            
   *lnRateVal = APINVHDR.nExRate / APINVHDR.nCurrUnit   
   *lnSesAmnt = lnSesAmnt  - ROUND(APINVHDR.nInvAmtAp * lnRateVal, 2)
   *lnSesDisc = lnSesDisc  - ROUND(APINVHDR.nInvDisAp * lnRateVal, 2)
   *lnSesAdj  = lnSesAdj   - ROUND(APINVHDR.nInvAdjAp * lnRateVal, 2)
   *lnSes1099 = lnSes1099  - ROUND(APINVHDR.nInvA1099 * lnRateVal, 2)
   
   lnRateVal = APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit   
   lnSesAmnt = lnSesAmnt  - ROUND(APINVHDR.nInvAmtAp &lcExSin1 lnRateVal, 2)
   lnSesDisc = lnSesDisc  - ROUND(APINVHDR.nInvDisAp &lcExSin1 lnRateVal, 2)
   lnSesAdj  = lnSesAdj   - ROUND(APINVHDR.nInvAdjAp &lcExSin1 lnRateVal, 2)
   lnSes1099 = lnSes1099  - ROUND(APINVHDR.nInvA1099 &lcExSin1 lnRateVal, 2)
   *E300316,1 (End)
   *E300296,4 end.
   
   SHOW GET lnSesAmnt
   SHOW GET lnSesDisc
   SHOW GET lnSesAdj 
   SHOW GET lnSes1099

   *E300296,4 Clear approved currency fields as well 
   *E300296,4 (cAprCurCod, nAprCurUnt, nAprExRat)
   *REPLACE APINVHDR.NINVAMTAP WITH 0 ,;
           APINVHDR.NINVDISAP WITH 0 ,;
           APINVHDR.NINVADJAP WITH 0 ,;
           APINVHDR.NINVA1099 WITH 0 ,;
           APINVHDR.CBNKCODE  WITH '',;
           APINVHDR.CCHKACCT  WITH '',;
           APINVHDR.CCHKGLACC WITH ''
           
  *B601526,1 Change this line [Begin]
  *REPLACE APINVHDR.NINVAMTAP  WITH 0 ,;
  *        APINVHDR.NINVDISAP  WITH 0 ,;
  *        APINVHDR.NINVADJAP  WITH 0 ,;
  *        APINVHDR.NINVA1099  WITH 0 ,;
  *        APINVHDR.CBNKCODE   WITH '',;
  *        APINVHDR.CCHKACCT   WITH '',;
  *        APINVHDR.CCHKGLACC  WITH '',;
  *        APINVHDR.cAprCurCod WITH '',;
  *        APINVHDR.nAprCurUnt WITH 0,;
  *        APINVHDR.nAprExRat  WITH 0

  REPLACE APINVHDR.NINVAMTAP  WITH 0 ,;
          APINVHDR.NINVDISAP  WITH 0 ,;
          APINVHDR.NINVADJAP  WITH 0 ,;
          APINVHDR.NINVA1099  WITH 0 ,;
          APINVHDR.CBNKCODE   WITH '',;
          APINVHDR.CCHKACCT   WITH '',;
          APINVHDR.CCHKGLACC  WITH '',;
          APINVHDR.cAprCurCod WITH '',;
          APINVHDR.nAprCurUnt WITH 0,;
          APINVHDR.nAprExRat  WITH 0,;
          APINVHDR.nInvFAAp   WITH 0

  *B601526,1 Change this line [End]

  *E300296,4 end.
           
  =gfAdd_Info()  && Add the audit information to the record.           
  =gfObj_lock(.F.)  && unlocking the pointed record
  IF !BOF()
    SKIP -1
    ***Refresh the record pointer. ***
    lnBrRecNo  = RECNO('APINVHDR')
    ***Refresh the invoice push button. ***
    IF _DOS
      SHOW GET pbInvoice,1 PROMPT 'I\<nv. '+ALLTRIM(APINVHDR.CINVNO)
    ENDIF
    ***Refresh say field open apen amount. ***
    =lfRefresh()
  ENDIF    
  SHOW WINDOW (lcBrTtl) REFRESH
ENDIF

*!**************************************************************************
*!
*!      Function: lfvDApprAll
*!
*!**************************************************************************
*Disapprove all records in browse.
FUNCTION lfvDApprAll
PRIVATE lnTotInv1 , lnTotInv2

lnTotInv1   = 0
lnTotInv2   = 0
lcSavOrd = ORDER() 
SET ORDER TO 0

COUNT TO lnTotInv1
SET ORDER TO &lcSavOrd
***Exit from disapproving loop.
lcEscap=SET("ESCAPE")
SET ESCAPE ON
ON ESCAPE STORE .T. TO llExit
SCAN
  lnTotInv2 = lnTotInv2 + 1
  =gfThermo(lnTotInv1,lnTotInv2,"Disapproving invoices...",;
            "Vendor: "+CVENDCODE+"  Invoice: "+CINVNO)
  *E300316,1 HISH 12/05/95. Got the equation signs. (Begin)
  *E300316,1 to exchange currency from invoice currency to company base currency.
  *E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
  lcExSin2 = ' '
  lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
  *lcExSin2 = IIF(lcExSin1 = '*','/','*')
  *E300316,1 (End)
            
  *:->B128400 new function to modify message appear [Start]
  *IF gfObj_lock(.T.) && lock the pointed record.    
  IF lfObj_lock(.T.) && lock the pointed record.    
  *:->B128400 [End]
  
    *E300296,4 Show totals in base currency
    *lnSesAmnt = lnSesAmnt  - NINVAMTAP
    *lnSesDisc = lnSesDisc  - NINVDISAP
    *lnSesAdj  = lnSesAdj   - NINVADJAP
    *lnSes1099 = lnSes1099  - NINVA1099
    *E300316,1 HISH 12/05/95. Used the variables hold signs in the equation. (Begin)                                
    *lnRateVal = APINVHDR.nExRate / APINVHDR.nCurrUnit   
    *lnSesAmnt = lnSesAmnt  - ROUND(APINVHDR.nInvAmtAp * lnRateVal, 2)
    *lnSesDisc = lnSesDisc  - ROUND(APINVHDR.nInvDisAp * lnRateVal, 2)
    *lnSesAdj  = lnSesAdj   - ROUND(APINVHDR.nInvAdjAp * lnRateVal, 2)
    *lnSes1099 = lnSes1099  - ROUND(APINVHDR.nInvA1099 * lnRateVal, 2)
    
    lnRateVal = APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit   
    lnSesAmnt = lnSesAmnt  - ROUND(APINVHDR.nInvAmtAp &lcExSin1 lnRateVal, 2)
    lnSesDisc = lnSesDisc  - ROUND(APINVHDR.nInvDisAp &lcExSin1 lnRateVal, 2)
    lnSesAdj  = lnSesAdj   - ROUND(APINVHDR.nInvAdjAp &lcExSin1 lnRateVal, 2)
    lnSes1099 = lnSes1099  - ROUND(APINVHDR.nInvA1099 &lcExSin1 lnRateVal, 2)
    *E300296,4 end.

    SHOW GET lnSesAmnt
    SHOW GET lnSesDisc
    SHOW GET lnSesAdj 
    SHOW GET lnSes1099

    *E300296,4 Clear approved currency fields as well 
    *E300296,4 (cAprCurCod, nAprCurUnt, nAprExRat)
    *REPLACE APINVHDR.NINVAMTAP WITH 0 ,;
            APINVHDR.NINVDISAP WITH 0 ,;
            APINVHDR.NINVADJAP WITH 0 ,;
            APINVHDR.NINVA1099 WITH 0 ,;
            APINVHDR.CBNKCODE  WITH '',;
            APINVHDR.CCHKACCT  WITH '',;
            APINVHDR.CCHKGLACC WITH ''
    
    *B601526,1 Change this line [Begin]
    *REPLACE APINVHDR.NINVAMTAP  WITH 0 ,;
    *        APINVHDR.NINVDISAP  WITH 0 ,;
    *        APINVHDR.NINVADJAP  WITH 0 ,;
    *        APINVHDR.NINVA1099  WITH 0 ,;
    *        APINVHDR.CBNKCODE   WITH '',;
    *        APINVHDR.CCHKACCT   WITH '',;
    *        APINVHDR.CCHKGLACC  WITH '',;
    *        APINVHDR.cAprCurCod WITH '',;
    *        APINVHDR.nAprCurUnt WITH 0,;
    *        APINVHDR.nAprExRat  WITH 0

    REPLACE APINVHDR.NINVAMTAP  WITH 0 ,;
            APINVHDR.NINVDISAP  WITH 0 ,;
            APINVHDR.NINVADJAP  WITH 0 ,;
            APINVHDR.NINVA1099  WITH 0 ,;
            APINVHDR.CBNKCODE   WITH '',;
            APINVHDR.CCHKACCT   WITH '',;
            APINVHDR.CCHKGLACC  WITH '',;
            APINVHDR.cAprCurCod WITH '',;
            APINVHDR.nAprCurUnt WITH 0,;
            APINVHDR.nAprExRat  WITH 0,;
            APINVHDR.nInvFAAp   WITH 0

    *B601526,1 Change this line [End]

    *E300296,4 end.
    
    =gfAdd_Info()  && Add the audit information to the record.        
    =gfObj_lock(.F.) && lock the pointed record.
  ENDIF  
  IF llExit  &&If Escape pressed.
    lnTotInv1 = lnTotInv2
    IF wvisible("GWDTHERMO")
      RELEASE WINDOW ("GWDTHERMO")
    ENDIF
    llExit    = .F.
    EXIT
  ENDIF
ENDSCAN
*B601581,1 Hesham El-Sheltawi (Start)
*B601581,1 Check if the Thermometer window exist then force it to close
IF WEXIST('gwdThermo')
  =gfThermo(lnTotInv1,lnTotInv1,"Disapproving invoices ...",;
          "Vendor: "+CVENDCODE+"  Invoice: "+CINVNO)
ENDIF
*B601581,1 Hesham El-Sheltawi (End)
GO BOTTOM
SET ESCAPE &lcEscap

*B500787,1 Removed browse activation and replaced it with a call to the
*B500787,1 BROWSE COMMAND WHEN function so as to refresh all data, as
*B500787,1 well as the browse and the file pointer with the values of the 
*B500787,1 current record. (the last record in the browse in this case)
*ACTIVATE WINDOW (lcBrTtl) 
=lfwInvBrow()
*B500787,1 end.

*!**************************************************************************
*!
*!      Function: lfvApprAll
*!
*!**************************************************************************
* Saving the old values and calling approve all window
FUNCTION lfvApprAll

*** get default bank, checks, and gl account.
lcBankCode = APSETUP.CBNKCODE
lcCheckCode= APSETUP.CCHKACCT
*B600808,1 Default the G/L account whether or not it is empty.
*** get account if there is a bank code and check code available. ***
*IF EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')) ;
*  .AND. !EMPTY(lcBankCode) .AND. !EMPTY(lcCheckCode)
*B600808,1 end.
  IF SEEK(lcBankCode+lcCheckCode,'APCHECKS')
    lcGlAcct = IIF(!EMPTY(APCHECKS.CCHKGLACC),APCHECKS.CCHKGLACC, lcEmptyAcc)
    *B600808,1 Cash payment account is edfaulted by the 
    *B600808,1 cash payment account in APSETUP and not bey lcGLAcct
    *lcHGlAct = lcGlAcct
    *B600808,1 end.
    *E300296,4 Get the currency of the checking account.
    lcAprCurCod = APCHECKS.cCurrCode
    *E300296,4 end.
   ENDIF
*B600808,1 Remove ENDIF   
*ENDIF
*B600808,1 end.

*B600808,1 Get default Cash payment account.
lcHGlAct    = APSETUP.cCashAcct 
*B600808,1 end.

*E300296,4 Default the cash payment currency with the base currency.
lcAprCshCod = gcBaseCurr
*E300296,4 end.

*B600808,1 There is no need to store the following values if we default
*B600808,1 every time we get into the screen.
*lcOldBank   = lcBankCode
*lcOldCheck  = lcCheckCode
*lcOldGlAcc  = lcGLAcct
*lcHOldGlAc  = lcHGlAct
*B600808,1 end.

PUSH KEY
ON KEY      && refresh the key traped

RELEASE PAD _BROWSE OF _MSYSMENU


*E300683,1 Call *.SPR from screens directory
* DO APAPRALL.SPR  && calling the approved all window.
DO (gcScrDir + gcWinAppl + '\APAPRALL.SPR')
*E300683,1 end          

*** Push the same keys again after coming ***
*** from global function browse. ***
POP KEY

*B500787,1 Removed browse activation and replaced it with a call to the
*B500787,1 BROWSE COMMAND WHEN function so as to refresh all data, as
*B500787,1 well as the browse and the file pointer with the values of the 
*B500787,1 current record. (the last record in the browse in this case)
*ACTIVATE WINDOW (lcBrTtl) 
=lfwInvBrow()
*B500787,1 end.

*!**************************************************************************
*!
*!      Function: lfShAprAll
*!
*!**************************************************************************
* control showing objects in case of approve default or approve accounts entered.
FUNCTION lfShAprAll

*** in case of approve default disable all objects.
IF rbAprAll = 1 
  SHOW GET lcBankCode  DISABLE
  SHOW GET ibBank      DISABLE
  SHOW GET lcCheckCode DISABLE
  SHOW GET ibChecks    DISABLE
  SHOW GET lcGlAcct    DISABLE
  SHOW GET lcHGlAct    DISABLE
  SHOW GET ibGLAcc     DISABLE
  SHOW GET ibHGLAcc    DISABLE
  
  *E300296,4 Disable currency fields as well
  SHOW GET ibAprCurCod DISABLE
  SHOW GET lcAprCshCod DISABLE
  *E300296,4 end.
  
  *** in case of approve accounts the user entered. ***
ELSE
  lcOldBank   = lcBankCode   && get the bank old value.
  lcOldCheck  = lcCheckCode  && get the check old value.
  lcOldGlAcc  = lcGLAcct     && get the account old value.
  lcHOldGlAc  = lcHGlAct
  IF EMPTY(lcBankCode) && if empty of bank.
    SHOW GET lcBankCode ENABLE
    SHOW GET ibBank     ENABLE
  ELSE
    IF EMPTY(lcCheckCode) && if empty of check.
      SHOW GET lcBankCode  ENABLE
      SHOW GET ibBank      ENABLE
      SHOW GET lcCheckCode ENABLE
      SHOW GET ibChecks    ENABLE
    ELSE                  && if empty of account.
      SHOW GET lcBankCode  ENABLE
      SHOW GET ibBank      ENABLE
      SHOW GET lcCheckCode ENABLE
      SHOW GET ibChecks    ENABLE
      SHOW GET lcGlAcct    ENABLE
      SHOW GET ibGLAcc     ENABLE
      
      *B600808,1 Enable cash payment account any way
      *B600808,1 Move the following lines outside condition.
      *SHOW GET lcHGlAct    ENABLE
      *SHOW GET ibHGLAcc    ENABLE
      *B600808,1 end.
    ENDIF
  ENDIF
  
  *B600808,1 Enable cash payment account any way
  SHOW GET lcHGlAct    ENABLE
  SHOW GET ibHGLAcc    ENABLE
  *B600808,1 end.

  *E300296,4 Enable cash currency fields if the cash payment
  *E300296,4 account is not empty.
  IF lcHGlAct <> lcEmptyAcc
    SHOW GET ibAprCurCod ENABLE
    SHOW GET lcAprCshCod ENABLE
  ENDIF
  *E300296,4 end.
ENDIF
SHOW GET pbCancel   ENABLE

*!**************************************************************************
*!
*!      Function: lfvpbAprAl
*!
*!**************************************************************************
* approve the all browse record in the file.
FUNCTION lfvpbAprAl
PRIVATE lcCurAlias, lnTotInv1 , lnTotInv2

lnTotInv1   = 0
lnTotInv2   = 0

*B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
lcVen0Prior = ''
*B600860,1 End.

SELECT APINVHDR

lcSavOrd = ORDER() 

SET ORDER TO 0
COUNT TO lnTotInv1
SET ORDER TO &lcSavOrd
**Exit from approving loop.**
lcEscap=SET("ESCAPE")
SET ESCAPE ON
ON ESCAPE STORE .T. TO llExit

*** in case of appove with default values. ***
lnSesAmnt = 0
lnSesDisc = 0
lnSesAdj  = 0
SHOW GET lnSesAdj LEVEL RDLEVEL()-1

*B600808,1 Add Approved 1099  amount
lnSes1099 = 0
SHOW GET lnSes1099 LEVEL RDLEVEL()-1
*B600808,1 end.

IF rbAprAll = 1
  SCAN 
    *B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
    IF !((CVENPRIOR = '0') .OR. (APVENDOR.CVENPRIOR = '0'))
    *B600860,1 End.
      *E300296,4 Get the totals after approval so as to calculate the
      *E300296,4 equivalent amount in base currency.
      *lnSesAmnt = lnSesAmnt + APINVHDR.NINVAMNT-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ-APINVHDR.NINVDISOF
      *lnSesDisc = lnSesDisc + APINVHDR.NINVDISOF
      *SHOW GET lnSesAmnt LEVEL RDLEVEL()-1
      *SHOW GET lnSesDisc LEVEL RDLEVEL()-1
      *E300296,4 end.
   
      lnTotInv2 = lnTotInv2 + 1
      =gfThermo(lnTotInv1,lnTotInv2,"Approving invoices...","Vendor: "+CVENDCODE+"  Invoice: "+CINVNO)
      
      *:->B128400 new function to modify message appear [Start]
      *IF gfObj_lock(.T.) && lock the pointed record.    
      IF lfObj_lock(.T.) && lock the pointed record.    
      *:->B128400 [End]
      
        ***If the vendor or invoice is on hold. ***
        *B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
        *IF (CVENPRIOR <> '0') .AND. (APVENDOR.CVENPRIOR <> '0')
        *B600860,1 End.
        IF CVENPMETH <> 'H'
          lcBankCode   = CBNKCODE && get bank initial value.
          lcCheckCode  = CCHKACCT
          lcGlAcct     = IIF(!EMPTY(CCHKGLACC),CCHKGLACC,lcEmptyAcc)  
          *B600808,1 Cash payment account does not default from 
          *B600808,1 the bank/checking account.
          *lcHGlAct   = lcGlAcct
          *B600808,1 end.

          IF EMPTY(lcBankCode) && if there is no initial value.
            DO CASE
              CASE !EMPTY(APVENDOR.CBNKCODE) && try to get approve value form vendor file.
                lcBankCode = APVENDOR.CBNKCODE
                lcCheckCode= APVENDOR.CCHKACCT
                *B600808,1 Clear G/L account 
                lcGlAcct   = lcEmptyAcc
                *B600808,1 end.      

              CASE !EMPTY(APDIV.CBNKCODE)    && try to get approve value form division file.
                lcBankCode = APDIV.CBNKCODE
                lcCheckCode= APDIV.CCHKACCT
                *B600808,1 Clear G/L account 
                lcGlAcct   = lcEmptyAcc
                *B600808,1 end.      

              OTHERWISE                      && try to get approve value form setup file.
                lcBankCode = APSETUP.CBNKCODE
                lcCheckCode= APSETUP.CCHKACCT
                *B600808,1 Clear G/L account 
                lcGlAcct   = lcEmptyAcc
                *B600808,1 end.      
            ENDCASE
          
          *B600808,1 Add an ENDIFto close the empty bank condition
          *B600808,1 here. Get the defaults anyway.
          ENDIF  
          *B600808,1 end.
          
          *** get account value in case of empty and the is bank and check.
          *B600808,1 Move condition below.
          * IF EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')) ;
                .AND. !EMPTY(lcBankCode) .AND. !EMPTY(lcCheckCode)
          IF SEEK(lcBankCode+lcCheckCode,'APCHECKS')
            *lcGlAcct = IIF(!EMPTY(APCHECKS.CCHKGLACC),APCHECKS.CCHKGLACC,lcEmptyAcc)  
            *B600808,1 No cash payment account
            *lcHGlAct = lcGlAcct
            *B600808,1 end.
            *** replace the default values got to the file. ***
            *B600808,1 Check here for empty account
            IF EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')) 
              lcGlAcct = IIF(!EMPTY(APCHECKS.CCHKGLACC),APCHECKS.CCHKGLACC,lcEmptyAcc)  
            ENDIF
            *B600808,1 end.

            *E300296,4 Get default currency of the checking account.
            lcAprCurCod   = APCHECKS.cCurrCode

            *E300296,4 Get a corresponding exchange rate
            *E300296,4 between the approval currency and the invoice currency.
            *E300296,4 If the currency is different, implies that the system
            *E300296,4 is multi currency, call the global function gfChkRate
            *E300296,4 that validates an exchange rate.

            IF APINVHDR.cCurrCode <> lcAPrCurCod
              *B600849,1 HISH 11/29/95. Change parameter to get rate and unit to change from invoice (Begin)
              *B600849,1                currency to approved currency.
              *lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCurCod,APINVHDR.dInvDate, ;
                                      .T., .F., APINVHDR.cCurrCode, .T.)

              *B601416,1 Change the calling of gfChkRate so if the [BEGIN]
              * Invoice currency is the base currency I make it the To currency 
              * and if not the Approve currency is the To currency 
              *lnAprExRat = gfChkRate('lnAprCurUnt', APINVHDR.cCurrCode ,APINVHDR.dInvDate, ;
              *                        .T., .F., lcAprCurCod, .T.)
              *B601416,1 IF Statment to check if the Invoice currency is the 
              * same as the base currency
              IF APINVHDR.cCurrCode = gcBaseCurr
                lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCurCod ,APINVHDR.dInvDate, ;
                                        .T., .F., APINVHDR.cCurrCode, .T.)
              ELSE      && Else
                lnAprExRat = gfChkRate('lnAprCurUnt', APINVHDR.cCurrCode ,APINVHDR.dInvDate, ;
                                        .T., .F., lcAprCurCod, .T.)
              ENDIF     && End of IF
              *B601416,1 Change the calling of gfChkRate [END]

              *B600849,1 (End)                        
            ELSE
              STORE 1 TO lnAprExRat, lnAprCurUnt
            ENDIF      && ENDIF APINVHDR.cCurrCode <> lcAPrCurCod                   
            *E300296,4 end.
          ENDIF
          
          *E300316,1 HISH 12/05/95. Got the equation signs. (Begin)
          *E300316,1 to exchange currency from invoice currency to company base currency.
          *E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
          lcExSin2 = ' '
          lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
          *lcExSin2 = IIF(lcExSin1 = '*','/','*')
          *E300316,1 HISH 12/05/95. to exchange currency from invoice currency to approved currency.
          lcExSin4 = ' '

          *B601416,1 Change the calling of gfGetExSin so if the [BEGIN]
          * Invoice currency is the base currency I make it the To currency 
          * and if not the Approve currency is the To currency 
          *lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
          *B601416,1 IF Statment to check if the Invoice currency is the 
          * same as the base currency
          IF APINVHDR.cCurrCode = gcBaseCurr 
            lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,APINVHDR.cCurrCode)
            lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
            lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
          ELSE   && Else
            lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
          ENDIF  && End of IF 
         *B601416,1 Change the calling of gfGetExSin [END]

          *lcExSin4 = IIF(lcExSin3 = '*','/','*')
          *E300316,1 (End)
          
          *E300296,4 If the exchange rate = 0, do not approve, and present
          *E300296,4 the following message
          IF lnAprExRat = 0
            *E300296,4 IF editing the exchange rate on the fly is not 
            *E300296,4 allowed, present the following message:
            *E300296,4 Message : " A valid ð to ð exchange rate could not "
            *E300296,4           " be found for ð.                        "
            *E300296,4           "                  ® Ok ¯                "

            *B601667,1 Change this line to fix the parameters [Begin]
            *=gfModalGen("INM04157B00000", "DIALOG",;
            *             ALLTRIM(APINVHDR.cCurrCode)+'|' ;
            *             +ALLTRIM(lcAprCurCod)+;
            *             '|'+DTOC(APINVHDR.dInvDate))

            *B601667,1 IF Statment to check if the Invoice currency is the
            *          Base Currency
            IF APINVHDR.cCurrCode = gcBaseCurr
              =gfModalGen("INM04157B00000", "DIALOG",;
                           ALLTRIM(lcAprCurCod)+'|' ;
                           +ALLTRIM(APINVHDR.cCurrCode)+;
                           '|'+DTOC(APINVHDR.dInvDate))
            ELSE    && Else
              =gfModalGen("INM04157B00000", "DIALOG",;
                           ALLTRIM(APINVHDR.cCurrCode)+'|' ;
                           +ALLTRIM(lcAprCurCod)+;
                           '|'+DTOC(APINVHDR.dInvDate))
            ENDIF     && End of IF
            *B601667,1 Change this line to fix the parameters [End]

          ELSE
            *E300296,4 Include currency fields update.
            *B600808,1 Clear Approved 1099 amount field.
            *REPLACE CBNKCODE  WITH lcBankCode ,;
                     CCHKACCT  WITH lcCheckCode,;
                     CCHKGLACC WITH IIF(EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')),;
                                        SPACE(lnApsAcLen),lcGlAcct),;
                     NINVAMTAP WITH (APINVHDR.NINVAMNT-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ-APINVHDR.NINVDISOF),;
                     NINVDISAP WITH APINVHDR.NINVDISOF,;
                     NINVADJAP WITH 0

            *B601526,1 Change this line [Begin]
            *REPLACE CBNKCODE   WITH lcBankCode ,;
            *        CCHKACCT   WITH lcCheckCode,;
            *        CCHKGLACC  WITH IIF(EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')),;
            *                           SPACE(lnApsAcLen),lcGlAcct),;
            *        NINVAMTAP  WITH (APINVHDR.NINVAMNT-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ-APINVHDR.NINVDISOF),;
            *        NINVDISAP  WITH APINVHDR.NINVDISOF,;
            *        NINVADJAP  WITH 0,;
            *        nInvA1099  WITH 0,;
            *        cAprCurCod WITH lcAprCurCod,;
            *        nAprCurUnt WITH lnAprCurUnt,;
            *        nAprExRat  WITH lnAprExRat

            REPLACE CBNKCODE   WITH lcBankCode ,;
                    CCHKACCT   WITH lcCheckCode,;
                    CCHKGLACC  WITH IIF(EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')),;
                                       SPACE(lnApsAcLen),lcGlAcct),;
                    NINVAMTAP  WITH (APINVHDR.NINVAMNT-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ-APINVHDR.NINVDISOF),;
                    NINVDISAP  WITH APINVHDR.NINVDISOF,;
                    NINVADJAP  WITH 0,;
                    nInvA1099  WITH 0,;
                    cAprCurCod WITH lcAprCurCod,;
                    nAprCurUnt WITH lnAprCurUnt,;
                    nAprExRat  WITH lnAprExRat,;
                    nInvFAAp   WITH ROUND((APINVHDR.NINVAMNT-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ-APINVHDR.NINVDISOF) &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2)         
                    
            *B601526,1 Change this line [End]

            *B600808,1 end.       
            =gfAdd_Info('APINVHDR')  && Add the audit information to the record.        
          ENDIF
        ELSE     && If Cash Payment
          DO CASE
            *B600808,1 Add a case to get the cash payment account of 
            *B600808,1 the invoice, 
            CASE !EMPTY(APINVHDR.cChkGLAcc)
              lcHGlAcct   = CCHKGLACC
            *B600808,1 end.
            CASE !EMPTY(APVENDOR.CCASHACCT)
              lcHGlAct= APVENDOR.CCASHACCT
            CASE !EMPTY(APINVHDR.CDIVISION) .AND. !EMPTY(APDIV.CCASHACCT)
              lcHGlAct= APDIV.CCASHACCT
            OTHERWISE 
              lcHGlAct = IIF(!EMPTY(APSETUP.CCASHACCT),;
                         APSETUP.CCASHACCT,lcEmptyAcc) 
          ENDCASE      

          *E300296,4 Use the currency of the invoice.
          *E300296,4 Include currency fields update.
          *B600808,1 Clear Approved 1099 amount field as well.
          *** replace the default values got to the file. ***
          *REPLACE CCHKGLACC WITH IIF(EMPTY(STRTRAN(STRTRAN(lcHGlAct,'-'),'0')),;
                                            SPACE(lnApsAcLen), lcHGlAct ),;
                   NINVAMTAP WITH (APINVHDR.NINVAMNT-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ-APINVHDR.NINVDISOF),;
                   NINVDISAP WITH APINVHDR.NINVDISOF,;
                   NINVADJAP WITH 0
          
          *B601526,1 Change this line [Begin]
          *REPLACE CCHKGLACC  WITH IIF(EMPTY(STRTRAN(STRTRAN(lcHGlAct,'-'),'0')),;
          *                           SPACE(lnApsAcLen), lcHGlAct),;
          *        NINVAMTAP  WITH (APINVHDR.NINVAMNT-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ-APINVHDR.NINVDISOF),;
          *        NINVDISAP  WITH APINVHDR.NINVDISOF,;
          *        NINVADJAP  WITH 0,;
          *        nInvA1099  WITH 0,;
          *        cAprCurCod WITH APINVHDR.cCurrCode,;
          *        nAprCurUnt WITH 1,;
          *        nAprExRat  WITH 1

          REPLACE CCHKGLACC  WITH IIF(EMPTY(STRTRAN(STRTRAN(lcHGlAct,'-'),'0')),;
                                     SPACE(lnApsAcLen), lcHGlAct),;
                  NINVAMTAP  WITH (APINVHDR.NINVAMNT-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ-APINVHDR.NINVDISOF),;
                  NINVDISAP  WITH APINVHDR.NINVDISOF,;
                  NINVADJAP  WITH 0,;
                  nInvA1099  WITH 0,;
                  cAprCurCod WITH APINVHDR.cCurrCode,;
                  nAprCurUnt WITH 1,;
                  nAprExRat  WITH 1,;
                  nInvFAAp   WITH ROUND((APINVHDR.NINVAMNT-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ-APINVHDR.NINVDISOF) &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2)         

          *B601526,1 Change this line [End]

          *B600808,1 end.       
          *E300296,4 end.               
          =gfAdd_Info('APINVHDR')  && Add the audit information to the record.          
        ENDIF  
        
        *B601013,4 HISH  04/18/96. Got the equation signs. (Begin)
        *B601013,4  to exchange currency from invoice currency to company base currency.
        lcExSin2 = ' '
        lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
        *B601013,4 (End)

        *E300296,4 Get the totals in base currency.
        *E300316,1 HISH 12/05/95. Used the variables hold signs in the equation. (Begin)
        *lnRateVal = APINVHDR.nExRate / APINVHDR.nCurrUnit   
        *lnSesAmnt = lnSesAmnt + ;
                     ROUND(APINVHDR.nInvAmtAp * lnRateVal, 2)
        *lnSesDisc = lnSesDisc  + ROUND(APINVHDR.nInvDisAp * lnRateVal, 2)

        lnRateVal = APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit   
        lnSesAmnt = lnSesAmnt + ROUND(APINVHDR.nInvAmtAp &lcExSin1 lnRateVal, 2)
        lnSesDisc = lnSesDisc + ROUND(APINVHDR.nInvDisAp &lcExSin1 lnRateVal, 2)
        *E300316,1 (End)           
        
        SHOW GET lnSesAmnt LEVEL RDLEVEL()-1
        SHOW GET lnSesDisc LEVEL RDLEVEL()-1
        *E300296,4 end.        
      *B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
      *ENDIF  
      *B600860,1 End.
        =gfObj_lock(.F.) && lock the pointed record.
      ENDIF  
      IF llExit  &&If Escape pressed.
        lnTotInv1 = lnTotInv2
        IF WVISIBLE("GWDTHERMO")
          RELEASE WINDOW ("GWDTHERMO")
        ENDIF
        llExit    = .F.
        EXIT
      ENDIF
    ELSE
      *B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
      DO CASE
        CASE CVENPRIOR = '0'
          lcvendorr=ALLTRIM(APVENDOR.CVENDCODE)+' Invoice No. '+ALLTRIM(CINVNO)
          ** MESSAGE : " Vendor XXXXXX has payment priority 0."
          **           " This vendor in on hold.              " 
          **           "                ® OK ¯                " 
          =gfModalGen("TRM04060B00000","DIALOG",lcvendorr)
        
        CASE APVENDOR.CVENPRIOR = '0' .AND. lcVen0Prior <> CVENDCODE 
          lcvendorr=ALLTRIM(APVENDOR.CVENDCODE)
          ** MESSAGE : " Vendor XXXXXX has payment priority 0."
          **           " This vendor in on hold.              " 
          **           "                ® OK ¯                " 
          =gfModalGen("TRM04060B00000","DIALOG",lcvendorr)
      ENDCASE
      lcVen0Prior = CVENDCODE
      lnTotInv2 = lnTotInv2 + 1
      *B600860,1 End.
    ENDIF
  ENDSCAN
  *B601581,1 Hesham El-Sheltawi (Start)
  *B601581,1 Check if the Thermometer window exist then force it to close
  IF WEXIST('gwdThermo')
    =gfThermo(lnTotInv1,lnTotInv1,"Approving invoices ...","Vendor: "+CVENDCODE+"  Invoice: "+CINVNO)  
  ENDIF
  *B601581,1 Hesham El-Sheltawi (End)
  GO BOTTOM
  CLEAR READ
ELSE && in case of appove with entered values.
  ** if any approved values are empty. **
  IF(EMPTY(lcBankCode) .OR. EMPTY(lcCheckCode);
                       .OR. EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0'));
                       .OR. (ATC("?",lcGlAcct)>0));
                       .OR. EMPTY(STRTRAN(STRTRAN(lcHGlAct,'-'),'0')); 
                       .OR. (ATC("?",lcHGlAct)>0))
    ** MESSAGE : " You have to enter the bank code,the     "
    **           " checking account,the GL account,and Cash payment."
    **           "                    ® Ok ¯               "
    =gfModalGen("TRM04090B00000","DIALOG")

    IF EMPTY(lcBankCode)
      SHOW GET lcBankCode
      _CUROBJ = OBJNUM(lcBankCode)
    ELSE
      IF EMPTY(lcCheckCode)
        SHOW GET lcCheckCode
        _CUROBJ = OBJNUM(lcCheckCode)
      ELSE
        SHOW GET lcGlAcct
        SHOW GET lcHGlAcct
        _CUROBJ = OBJNUM(lcGlAcct)
      ENDIF  
    ENDIF  
    *E300296,4 Add checks for currency fields
    
    RETURN
  ENDIF  
  *E300296,4 Add checks for currency fields
  IF llMultiCr .AND. EMPTY(lcAprCshCod)
    *** Message : " You have to enter the ð."
    ***                      <   OK   >
    =gfModalGen("TRM04066B00000","DIALOG",'cash payment approved currency')
    _CUROBJ = OBJNUM(lcAprCshCod)
    RETURN   
  ELSE && if all approved values is full.
    CLEAR READ
  ENDIF

  lcCurAlias  = ALIAS()

  SELECT APINVHDR
    
  SCAN 
    *B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
    IF !((CVENPRIOR = '0') .OR. (APVENDOR.CVENPRIOR = '0'))
    *B600860,1 End.
      *E300296,4 Get the totals after approval so as to calculate the
      *E300296,4 equivalent amount in base currency.
      *lnSesAmnt = lnSesAmnt + APINVHDR.NINVAMNT-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ-APINVHDR.NINVDISOF
      *lnSesDisc = lnSesDisc + APINVHDR.NINVDISOF    
      *SHOW GET lnSesAmnt LEVEL RDLEVEL()-1
      *SHOW GET lnSesDisc LEVEL RDLEVEL()-1
      *E300296,4 end.
    
      lnTotInv2 = lnTotInv2 + 1
      =gfThermo(lnTotInv1,lnTotInv2,"Approving invoices...","Vendor: "+CVENDCODE+"  Invoice: "+CINVNO)

      *:->B128400 new function to modify message appear [Start] 
      *IF gfObj_lock(.T.) && lock the pointed record.
      IF lfObj_lock(.T.) && lock the pointed record.
      *:->B128400 [End]

        ***If the vendor or invoice is on hold. ***
        *B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
        * IF (CVENPRIOR <> '0') .AND. (APVENDOR.CVENPRIOR <> '0')
        *B600860,1 End.
        IF CVENPMETH <> 'H' 
          *E300296,4 Get exchange rate between the checking account
          *E300296,4 currency and the invoice currency.
          *E300296,4 If the currency is different, implies that the system
          *E300296,4 is multi currency, call the global function gfChkRate
          *E300296,4 that validates an exchange rate.
          IF APINVHDR.cCurrCode <> lcAPrCurCod
            *B600849,1 HISH 11/29/95. Change parameter to get rate and unit to change from invoice (Begin)
            *B600849,1                currency to approved currency.
            *lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCurCod,APINVHDR.dInvDate, ;
                                      .T., .F., APINVHDR.cCurrCode, .T.)

            *B601416,1 Change the calling of gfChkRate so if the [BEGIN]
            * Invoice currency is the base currency I make it the To currency 
            * and if not the Approve currency is the To currency 
            *lnAprExRat = gfChkRate('lnAprCurUnt', APINVHDR.cCurrCode ,APINVHDR.dInvDate, ;
            *                        .T., .F., lcAprCurCod, .T.)
            *B601416,1 IF Statment to check if the Invoice currency is the 
            * same as the base currency
            IF APINVHDR.cCurrCode = gcBaseCurr
              lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCurCod ,APINVHDR.dInvDate, ;
                                      .T., .F., APINVHDR.cCurrCode, .T.)
            ELSE      && Else
              lnAprExRat = gfChkRate('lnAprCurUnt', APINVHDR.cCurrCode ,APINVHDR.dInvDate, ;
                                      .T., .F., lcAprCurCod, .T.)
            ENDIF     && End of IF
            *B601416,1 Change the calling of gfChkRate [END]

            *B600849,1 (End)                          
            *E300316,1 HISH 12/05/95. Got the equation signs. (Begin)
            *E300316,1 to exchange currency from invoice currency to company base currency.
            *E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)              
            lcExSin2 = ' '
            lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
            *lcExSin2 = IIF(lcExSin1 = '*','/','*')
            *E300316,1 HISH 12/05/95. to exchange currency from invoice currency to approved currency.
            lcExSin4 = ' '

            *B601416,1 Change the calling of gfGetExSin so if the [BEGIN]
            * Invoice currency is the base currency I make it the To currency 
            * and if not the Approve currency is the To currency 
            *lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
            *B601416,1 IF Statment to check if the Invoice currency is the 
            * same as the base currency
            IF APINVHDR.cCurrCode = gcBaseCurr 
              lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,APINVHDR.cCurrCode)
              lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
              lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
            ELSE   && Else
              lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
            ENDIF  && End of IF 
            *B601416,1 Change the calling of gfGetExSin [END]

            *lcExSin4 = IIF(lcExSin3 = '*','/','*')
            *E300316,1 (End)
          ELSE
            STORE 1 TO lnAprExRat, lnAprCurUnt
            *E300316,1 HISH 12/05/95. Got the equation signs. (Begin)
            *E300316,1 to exchange currency from invoice currency to company base currency.
            *E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
            lcExSin2 = ' '
            lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
            *lcExSin2 = IIF(lcExSin1 = '*','/','*')
            *E300316,1 HISH 12/05/95. to exchange currency from invoice currency to approved currency.
            lcExSin4 = ' '

            *B601416,1 Change the calling of gfGetExSin so if the [BEGIN]
            * Invoice currency is the base currency I make it the To currency 
            * and if not the Approve currency is the To currency 
            *lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
            *B601416,1 IF Statment to check if the Invoice currency is the 
            * same as the base currency
            IF APINVHDR.cCurrCode = gcBaseCurr 
              lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,APINVHDR.cCurrCode)
              lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
              lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
            ELSE   && Else
              lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
            ENDIF  && End of IF 
            *B601416,1 Change the calling of gfGetExSin [END]

            *lcExSin4 = IIF(lcExSin3 = '*','/','*')
            *E300316,1 (End)
          ENDIF      && ENDIF APINVHDR.cCurrCode <> lcAPrCurCod                   
          *E300296,4 end.

          *E300296,4 If the exchange rate = 0, do not approve, and present
          *E300296,4 the following message
          IF lnAprExRat = 0
            *E300296,4 IF editing the exchange rate on the fly is not 
            *E300296,4 allowed, present the following message:
            *E300296,4 Message : " A valid ð to ð exchange rate could not "
            *E300296,4           " be found for ð.                        "
            *E300296,4           "                  ® Ok ¯                "

            *B601667,1 Change this line to fix the parameters [Begin]
            *=gfModalGen("INM04157B00000", "DIALOG",;
            *             ALLTRIM(APINVHDR.cCurrCode)+'|' ;
            *             +ALLTRIM(lcAprCurCod)+;
            *             '|'+DTOC(APINVHDR.dInvDate))

            *B601667,1 IF Statment to check if the Invoice currency is the
            *          Base Currency
            IF APINVHDR.cCurrCode = gcBaseCurr
              =gfModalGen("INM04157B00000", "DIALOG",;
                           ALLTRIM(lcAprCurCod)+'|' ;
                           +ALLTRIM(APINVHDR.cCurrCode)+;
                           '|'+DTOC(APINVHDR.dInvDate))
            ELSE    && Else
              =gfModalGen("INM04157B00000", "DIALOG",;
                           ALLTRIM(APINVHDR.cCurrCode)+'|' ;
                           +ALLTRIM(lcAprCurCod)+;
                           '|'+DTOC(APINVHDR.dInvDate))
            ENDIF    && End of IF
            *B601667,1 Change this line to fix the parameters [End]

          ELSE
            *E300296,4 Include currency fields update.
            *B600808,1 Clear Approved 1099 amount field as well.
            *REPLACE CBNKCODE  WITH lcBankCode ,;
                     CCHKACCT  WITH lcCheckCode,;
                     CCHKGLACC WITH IIF(EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')),;
                                              SPACE(lnApsAcLen),lcGlAcct),;
                     NINVAMTAP WITH (NINVAMNT-NINVPAID-NINVDISTK-NINVADJ-NINVDISOF),;
                     NINVDISAP WITH NINVDISOF,;
                     NINVADJAP WITH 0
                   
            *B601526,1 Change this line [Begin]
            *REPLACE CBNKCODE   WITH lcBankCode ,;
            *        CCHKACCT   WITH lcCheckCode,;
            *        CCHKGLACC  WITH IIF(EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')),;
            *                                  SPACE(lnApsAcLen),lcGlAcct),;
            *        NINVAMTAP  WITH (NINVAMNT-NINVPAID-NINVDISTK-NINVADJ-NINVDISOF),;
            *        NINVDISAP  WITH NINVDISOF,;
            *        NINVADJAP  WITH 0,;
            *        nInvA1099  WITH 0,;
            *        cAprCurCod WITH lcAprCurCod,;
            *        nAprCurUnt WITH lnAprCurUnt,;
            *        nAprExRat  WITH lnAprExRat

            REPLACE CBNKCODE   WITH lcBankCode ,;
                    CCHKACCT   WITH lcCheckCode,;
                    CCHKGLACC  WITH IIF(EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')),;
                                              SPACE(lnApsAcLen),lcGlAcct),;
                    NINVAMTAP  WITH (NINVAMNT-NINVPAID-NINVDISTK-NINVADJ-NINVDISOF),;
                    NINVDISAP  WITH NINVDISOF,;
                    NINVADJAP  WITH 0,;
                    nInvA1099  WITH 0,;
                    cAprCurCod WITH lcAprCurCod,;
                    nAprCurUnt WITH lnAprCurUnt,;
                    nAprExRat  WITH lnAprExRat,;
                    nInvFAAp   WITH ROUND((APINVHDR.NINVAMNT-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ-APINVHDR.NINVDISOF) &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2)         

            *B601526,1 Change this line [End]

            *B600808,1 end.
            =gfAdd_Info()  && Add the audit information to the record.        
          ENDIF
        ELSE    && Else if payment method is cash payment.
          *E300296,4 Get exchange rate between the checking account
          *E300296,4 currency and the invoice currency.
          *E300296,4 If the currency is different, implies that the system
          *E300296,4 is multi currency, call the global function gfChkRate
          *E300296,4 that validates an exchange rate.
          IF APINVHDR.cCurrCode <> lcAprCshCod
            *B600849,1 HISH 11/29/95. Change parameter to get rate and unit to change from invoice (Begin)
            *B600849,1                currency to approved currency.
            *lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCshCod,APINVHDR.dInvDate, ;
                                      .T., .F., APINVHDR.cCurrCode, .T.)

            *B601416,1 Change the calling of gfChkRate so if the [BEGIN]
            * Invoice currency is the base currency I make it the To currency 
            * and if not the Approve currency is the To currency 
            *lnAprExRat = gfChkRate('lnAprCurUnt', APINVHDR.cCurrCode ,APINVHDR.dInvDate, ;
            *                        .T., .F., lcAprCshCod, .T.)
            *B601416,1 IF Statment to check if the Invoice currency is the 
            * same as the base currency
            IF APINVHDR.cCurrCode = gcBaseCurr
              lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCshCod ,APINVHDR.dInvDate, ;
                                      .T., .F., APINVHDR.cCurrCode, .T.)
            ELSE      && Else
              lnAprExRat = gfChkRate('lnAprCurUnt', APINVHDR.cCurrCode ,APINVHDR.dInvDate, ;
                                      .T., .F., lcAprCshCod, .T.)
            ENDIF     && End of IF
            *B601416,1 Change the calling of gfChkRate [END]

            *B600849,1 (End)                                      
            *E300316,1 HISH 12/05/95. Got the equation signs. (Begin)
            *E300316,1 to exchange currency from invoice currency to company base currency.
            *E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)              
            lcExSin2 = ' '
            lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
            *lcExSin2 = IIF(lcExSin1 = '*','/','*')
            *E300316,1 HISH 12/05/95. to exchange currency from invoice currency to approved currency.
            lcExSin4 = ' '

            *B601416,1 Change the calling of gfGetExSin so if the [BEGIN]
            * Invoice currency is the base currency I make it the To currency 
            * and if not the Approve currency is the To currency 
            *lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCshCod)
            *B601416,1 IF Statment to check if the Invoice currency is the 
            * same as the base currency
            IF APINVHDR.cCurrCode = gcBaseCurr 
              lcExSin3 = gfGetExSin(@lcExSin4,lcAprCshCod,APINVHDR.cCurrCode)
              lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
              lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
            ELSE   && Else
              lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCshCod)
            ENDIF  && End of IF 
            *B601416,1 Change the calling of gfGetExSin [END]

            *lcExSin4 = IIF(lcExSin3 = '*','/','*')
            *E300316,1 (End)
          ELSE
            STORE 1 TO lnAprExRat, lnAprCurUnt
            *B601013,4 HISH  04/18/96. Got the equation signs. (Begin)
            *B601013,4 exchange currency from invoice currency to company base currency.
            lcExSin2 = ' '
            lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
            *B601013,4 exchange currency from invoice currency to approved currency.
            lcExSin4 = ' '

            *B601416,1 Change the calling of gfGetExSin so if the [BEGIN]
            * Invoice currency is the base currency I make it the To currency 
            * and if not the Approve currency is the To currency 
            *lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCshCod)
            *B601416,1 IF Statment to check if the Invoice currency is the 
            * same as the base currency
            IF APINVHDR.cCurrCode = gcBaseCurr 
              lcExSin3 = gfGetExSin(@lcExSin4,lcAprCshCod,APINVHDR.cCurrCode)
              lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
              lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
            ELSE   && Else
              lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCshCod)
            ENDIF  && End of IF 
            *B601416,1 Change the calling of gfGetExSin [END]

            *B601013,4 (End)
          ENDIF      && ENDIF APINVHDR.cCurrCode <> lcAprCshCod                   
          *E300296,4 end.

          *E300296,4 If the exchange rate = 0, do not approve, and present
          *E300296,4 the following message
          IF lnAprExRat = 0
            *E300296,4 IF editing the exchange rate on the fly is not 
            *E300296,4 allowed, present the following message:
            *E300296,4 Message : " A valid ð to ð exchange rate could not "
            *E300296,4           " be found for ð.                        "
            *E300296,4           "                  ® Ok ¯                "

            *B601667,1 Change this line to fix the parameters [Begin]
            *=gfModalGen("INM04157B00000", "DIALOG",;
            *             ALLTRIM(APINVHDR.cCurrCode)+'|' ;
            *             +ALLTRIM(lcAprCshCod)+;
            *             '|'+DTOC(APINVHDR.dInvDate))

            *B601667,1 IF Statment to check if the Invoice currency is the
            *          Base Currency
            IF APINVHDR.cCurrCode = gcBaseCurr
              =gfModalGen("INM04157B00000", "DIALOG",;
                           ALLTRIM(lcAprCshCod)+'|' ;
                           +ALLTRIM(APINVHDR.cCurrCode)+;
                           '|'+DTOC(APINVHDR.dInvDate))
            ELSE     && Else
              =gfModalGen("INM04157B00000", "DIALOG",;
                           ALLTRIM(APINVHDR.cCurrCode)+'|' ;
                           +ALLTRIM(lcAprCshCod)+;
                           '|'+DTOC(APINVHDR.dInvDate))
            ENDIF    && End of IF
            *B601667,1 Change this line to fix the parameters [End]

          ELSE
            *E300296,4 Include currency fields update.
            *B600808,1 Clear Approved 1099 amount field as well.
            *REPLACE CCHKGLACC WITH IIF(EMPTY(STRTRAN(STRTRAN(lcHGlAct,'-'),'0')),;
                                              SPACE(lnApsAcLen),lcHGlAct),;
                     NINVAMTAP WITH (NINVAMNT-NINVPAID-NINVDISTK-NINVADJ-NINVDISOF),;
                     NINVDISAP WITH NINVDISOF,;
                     NINVADJAP WITH 0                  

            *B601526,1 Change this line [Begin] 
            *REPLACE CCHKGLACC WITH IIF(EMPTY(STRTRAN(STRTRAN(lcHGlAct,'-'),'0')),;
            *                                 SPACE(lnApsAcLen),lcHGlAct),;
            *        NINVAMTAP WITH (NINVAMNT-NINVPAID-NINVDISTK-NINVADJ-NINVDISOF),;
            *        NINVDISAP WITH NINVDISOF,;
            *        NINVADJAP WITH 0,;
            *        nInvA1099  WITH 0,;
            *        cAprCurCod WITH lcAprCshCod,;
            *        nAprCurUnt WITH lnAprCurUnt,;
            *        nAprExRat  WITH lnAprExRat                  

            REPLACE CCHKGLACC WITH IIF(EMPTY(STRTRAN(STRTRAN(lcHGlAct,'-'),'0')),;
                                             SPACE(lnApsAcLen),lcHGlAct),;
                    NINVAMTAP WITH (NINVAMNT-NINVPAID-NINVDISTK-NINVADJ-NINVDISOF),;
                    NINVDISAP WITH NINVDISOF,;
                    NINVADJAP WITH 0,;
                    nInvA1099  WITH 0,;
                    cAprCurCod WITH lcAprCshCod,;
                    nAprCurUnt WITH lnAprCurUnt,;
                    nAprExRat  WITH lnAprExRat,;
                    nInvFAAp   WITH ROUND((APINVHDR.NINVAMNT-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ-APINVHDR.NINVDISOF) &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2)         

            *B601526,1 Change this line [End]


            *B600808,1 end.         
            =gfAdd_Info()  && Add the audit information to the record.        
          ENDIF
          *E300296,4 end.  
        ENDIF                  
        *E300296,4 Get the totals in base currency.
        *E300316,1 HISH 12/05/95. Used the variables hold signs in the equation. (Begin)        
        *lnRateVal = APINVHDR.nExRate / APINVHDR.nCurrUnit   
        *lnSesAmnt = lnSesAmnt + ;
        *            ROUND(APINVHDR.nInvAmtAp * lnRateVal, 2)
        *lnSesDisc = lnSesDisc  + ROUND(APINVHDR.nInvDisAp * lnRateVal, 2)
        lnRateVal = APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit   
        lnSesAmnt = lnSesAmnt + ROUND(APINVHDR.nInvAmtAp &lcExSin1 lnRateVal, 2)
        lnSesDisc = lnSesDisc + ROUND(APINVHDR.nInvDisAp &lcExSin1 lnRateVal, 2)
        *E300316,1 (End)
        
        SHOW GET lnSesAmnt LEVEL RDLEVEL()-1
        SHOW GET lnSesDisc LEVEL RDLEVEL()-1
        *E300296,4 end.        
      *B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
      *ENDIF
      *B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
        =gfObj_lock(.F.) && lock the pointed record.
      ENDIF
      IF llExit  &&If Escape pressed.
        lnTotInv1 = lnTotInv2
        IF wvisible("GWDTHERMO")
          RELEASE WINDOW ("GWDTHERMO")
      ENDIF
        llExit    = .F.
        EXIT
      ENDIF
    ELSE
      *B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
      DO CASE
        CASE CVENPRIOR = '0'
          lcvendorr=ALLTRIM(APVENDOR.CVENDCODE)+' Invoice No. '+ALLTRIM(CINVNO)
          ** MESSAGE : " Vendor XXXXXX has payment priority 0."
          **           " This vendor in on hold.              " 
          **           "                ® OK ¯                " 
          =gfModalGen("TRM04060B00000","DIALOG",lcvendorr)
        
        CASE APVENDOR.CVENPRIOR = '0' .AND. lcVen0Prior <> CVENDCODE 
          lcvendorr=ALLTRIM(APVENDOR.CVENDCODE)
          ** MESSAGE : " Vendor XXXXXX has payment priority 0."
          **           " This vendor in on hold.              " 
          **           "                ® OK ¯                " 
          =gfModalGen("TRM04060B00000","DIALOG",lcvendorr)
      ENDCASE
      lcVen0Prior = CVENDCODE
      lnTotInv2 = lnTotInv2 + 1
      *B600860,1 End.
    ENDIF
  ENDSCAN  
  *B601581,1 Hesham El-Sheltawi (Start)
  *B601581,1 Check if the Thermometer window exist then force it to close
  IF WEXIST('gwdThermo')
    =gfThermo(lnTotInv1,lnTotInv1,"Approving invoices ...","Vendor: "+CVENDCODE+"  Invoice: "+CINVNO)  
  ENDIF
  *B601581,1 Hesham El-Sheltawi (End)
  SELECT (lcCurAlias)
  GO BOTTOM
ENDIF  

SET ESCAPE &lcEscap

*!**************************************************************************
*!
*!      Function: lfShow
*!
*!**************************************************************************
* Show objects in case of approve fully or partialy.
FUNCTION lfShow

IF EMPTY(lcBankCode) && if the is a bank.
  SHOW GET lcBankCode ENABLE
  SHOW GET ibBank     ENABLE
ELSE
  IF EMPTY(lcCheckCode) && if the is a check.
    SHOW GET lcBankCode  ENABLE
    SHOW GET ibBank      ENABLE
    SHOW GET lcCheckCode ENABLE
    SHOW GET ibChecks    ENABLE
  ELSE                  && if the is account.
    SHOW GET lcBankCode  ENABLE
    SHOW GET ibBank      ENABLE
    SHOW GET lcCheckCode ENABLE
    SHOW GET ibChecks    ENABLE
    SHOW GET lcGlAcct    ENABLE
    SHOW GET lcHGlAct    ENABLE
    SHOW GET ibGLAcc     ENABLE
  ENDIF
ENDIF

IF llAprPart  && in case of approve partially.
  SHOW GET lnAprToPay ENABLE
  SHOW GET lnAprDisc  ENABLE
  SHOW GET lnAprAdj   ENABLE
ENDIF  

IF _DOS
  SHOW GET pbInvoice,1 PROMPT 'I\<nv. '+ALLTRIM(APINVHDR.CINVNO)
ENDIF  
SHOW GET pbCancel   ENABLE

*B607126,1 ALB Disable dicount/Adjs fields in case of inoice < 0 [Begin]
IF lnAprToPay < 0
  Show GET lnAprDisc DISABLE
  Show GET lnAprAdj  DISABLE
ENDIF
*B607126,1 ALB Disable dicount/Adjs fields in case of inoice < 0 [end]


*!**************************************************************************
*!
*!      Function: lfGetDefu
*!
*!**************************************************************************
* get dufault bank, check, and gl account.
*
FUNCTION lfGetDefu

*B600808,1 Remove the following check. Get the default anyway.
*IF EMPTY(lcBankCode) 
*B600808,1 Get the defaul bank code, checking account and G/L checking
*B600808,1 account if the payment method is not cash payment,
*B600808,1 otherwise get the default cash payment account.
IF APINVHDR.cVenPMeth <> 'H'
*B600808,1 end.
  DO CASE
    CASE !EMPTY(APINVHDR.CBNKCODE) && if found values in invoice header file.
      lcBankCode = APINVHDR.CBNKCODE
      lcCheckCode= APINVHDR.CCHKACCT
      lcGlAcct   = IIF(!EMPTY(APINVHDR.CCHKGLACC),APINVHDR.CCHKGLACC, lcEmptyAcc)
      *B600808,1 Get cash payment account below. Removed.
      *lcHGlAct   = lcGlAcct
      *B600808,1 end.
    CASE !EMPTY(APVENDOR.CBNKCODE) && if found values in vendor file.
      lcBankCode = APVENDOR.CBNKCODE
      lcCheckCode= APVENDOR.CCHKACCT
      *B600808,1 Clear G/L account
      lcGlAcct   = lcEmptyAcc
      *B600808,1 end.
    CASE !EMPTY(APDIV.CBNKCODE)    && if found values in division file.
      lcBankCode = APDIV.CBNKCODE
      lcCheckCode= APDIV.CCHKACCT
      *B600808,1 Clear G/L account
      lcGlAcct   = lcEmptyAcc
      *B600808,1 end.
    OTHERWISE                      && get value from setup file.
      lcBankCode = APSETUP.CBNKCODE
      lcCheckCode= APSETUP.CCHKACCT
      *B600808,1 Clear G/L account
      lcGlAcct   = lcEmptyAcc
      *B600808,1 end.
  ENDCASE
  *** get account if there is a bank code and check code available. ***
  *B600808,1 Seek the checking account whether or not the G/L account
  *B600808,1 is empty.
  *IF  EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')) ;
        .AND. !EMPTY(lcBankCode) .AND. !EMPTY(lcCheckCode)
  IF !EMPTY(lcBankCode) .AND. !EMPTY(lcCheckCode)    
  *B600808,1 end.
    IF SEEK(lcBankCode+lcCheckCode,'APCHECKS')
      IF EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0'))
        lcGlAcct    = IIF(!EMPTY(APCHECKS.CCHKGLACC),APCHECKS.CCHKGLACC, lcEmptyAcc)
        *B600808,1 Get cash payment account below. Removed.
        *lcHGlAct   = lcGlAcct
        *B600808,1 end.
      ENDIF    && ENDIF EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0'))
      *B600808,1 end.
               
      *E300296,4 Get default currency of the checking account.
      lcAprCurCod   = APCHECKS.cCurrCode
      *E300296,4 Get a corresponding exchange rate
      *E300296,4 between the approval currency and the invoice currency.
      *E300296,4 If the currency is different, implies that the system
      *E300296,4 is multi currency, call the global function gfChkRate
      *E300296,4 that validates an exchange rate.
      IF APINVHDR.cCurrCode <> lcAPrCurCod
        *B600849,1 HISH 11/29/95. Change parameter to get rate and unit to change from invoice (Begin)
        *B600849,1                currency to approved currency.
        *lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCurCod,APINVHDR.dInvDate, ;
                               !llAprPart, .F., APINVHDR.cCurrCode, .T.)

        *B601416,1 Change the calling of gfChkRate so if the [BEGIN]
        * Invoice currency is the base currency I make it the To currency 
        * and if not the Approve currency is the To currency 
        *lnAprExRat = gfChkRate('lnAprCurUnt', APINVHDR.cCurrCode ,APINVHDR.dInvDate, ;
        *                        !llAprPart, .F., lcAprCurCod, .T.)
        *B601416,1 IF Statment to check if the Invoice currency is the 
        * same as the base currency
        IF APINVHDR.cCurrCode = gcBaseCurr
          lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCurCod ,APINVHDR.dInvDate, ;
                                  !llAprPart, .F., APINVHDR.cCurrCode, .T.)
        ELSE      && Else
          lnAprExRat = gfChkRate('lnAprCurUnt', APINVHDR.cCurrCode ,APINVHDR.dInvDate, ;
                                  !llAprPart, .F., lcAprCurCod, .T.)
        ENDIF     && End of IF
        *B601416,1 Change the calling of gfChkRate [END]

        *B600849,1 (End)                               
        *E300316,1 HISH 12/05/95. Got the equation signs. (Begin)
        *E300316,1 to exchange currency from invoice currency to company base currency.
        *E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)                      
        lcExSin2 = ' '
        lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
        *lcExSin2 = IIF(lcExSin1 = '*','/','*')
        *E300316,1 HISH 12/05/95. to exchange currency from invoice currency to approved currency.
        lcExSin4 = ' '

        *B601416,1 Change the calling of gfGetExSin so if the [BEGIN]
        * Invoice currency is the base currency I make it the To currency 
        * and if not the Approve currency is the To currency 
        *lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
        *B601416,1 IF Statment to check if the Invoice currency is the 
        * same as the base currency
        IF APINVHDR.cCurrCode = gcBaseCurr 
          lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,APINVHDR.cCurrCode)
          lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
          lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
        ELSE   && Else
          lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
        ENDIF  && End of IF 
        *B601416,1 Change the calling of gfGetExSin [END]

        *lcExSin4 = IIF(lcExSin3 = '*','/','*')
        *E300316,1 (End)
      ELSE
        STORE 1 TO lnAprExRat, lnAprCurUnt
        *E300316,1 HISH 12/05/95. Got the equation signs. (Begin)
        *E300316,1 to exchange currency from invoice currency to company base currency.
        *E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)                      
        lcExSin2 = ' '
        lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
        *lcExSin2 = IIF(lcExSin1 = '*','/','*')
        *E300316,1 to exchange currency from invoice currency to approved currency.
        lcExSin4 = ' '

        *B601416,1 Change the calling of gfGetExSin so if the [BEGIN]
        * Invoice currency is the base currency I make it the To currency 
        * and if not the Approve currency is the To currency 
        *lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
        *B601416,1 IF Statment to check if the Invoice currency is the 
        * same as the base currency
        IF APINVHDR.cCurrCode = gcBaseCurr 
          lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,APINVHDR.cCurrCode)
          lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
          lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
        ELSE   && Else
          lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
        ENDIF  && End of IF 
        *B601416,1 Change the calling of gfGetExSin [END]

        *lcExSin4 = IIF(lcExSin3 = '*','/','*')
        *E300316,1 (End)
      ENDIF      && ENDIF APINVHDR.cCurrCode <> lcAPrCurCod                   
      *E300296,4 end.
    ENDIF      && ENDIF SEEK(lcBankCode+lcCheckCode,'APCHECKS')  
  ENDIF      && ENDIF !EMPTY(lcBankCode) .AND. !EMPTY(lcCheckCode)    

*B600808,1 Else, if payment method is cash payment
ELSE
  *B600808,1 Get the default cash payment account
  lcGlAcct = IIF(!EMPTY(APINVHDR.cChkGLAcc), APINVHDR.cChkGLAcc,;
                 IIF(!EMPTY(APVENDOR.cCashAcct), APVENDOR.cCashAcct, ;
                     IIF(!EMPTY(APDIV.cCashAcct), APDIV.cCashAcct, ;
                         IIF(!EMPTY(APSETUP.cCashAcct), APSETUP.cCashAcct, ;
                             lcEmptyAcc))))
  *E300296,4 If not multi currency,
  *E300296,4 or it the payment method is cash payment,
  *E300296,4 Default currency with that of the invoice.
  lcAprCurCod = APINVHDR.cCurrCode
  STORE 1 TO lnAprExRat, lnAprCurUnt
  *E300316,1 HISH 12/05/95. Got the equation signs. (Begin)
  *E300316,1 to exchange currency from invoice currency to company base currency.
  *E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)                      
  lcExSin2 = ' '
  lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
  *lcExSin2 = IIF(lcExSin1 = '*','/','*')
  *E300316,1  to exchange currency from invoice currency to approved currency.
  lcExSin4 = ' '

  *B601416,1 Change the calling of gfGetExSin so if the [BEGIN]
  * Invoice currency is the base currency I make it the To currency 
  * and if not the Approve currency is the To currency 
  *lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
  *B601416,1 IF Statment to check if the Invoice currency is the 
  * same as the base currency
  IF APINVHDR.cCurrCode = gcBaseCurr 
    lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,APINVHDR.cCurrCode)
    lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
    lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
  ELSE   && Else
    lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
  ENDIF  && End of IF 
  *B601416,1 Change the calling of gfGetExSin [END]

  *lcExSin4 = IIF(lcExSin3 = '*','/','*')
  *E300316,1 (End)
  *E300296,4 end.
ENDIF
*B600808,1 end.

*!**************************************************************************
*!
*!      Function: lfvApprPart
*!
*!**************************************************************************
* this function calling from push button approve fully and partialy
*
FUNCTION lfvApprPart
PARAMETERS llAprPart

*:->B128400 new function to modify message appear [Start]
IF !lfObj_lock(.T.) 
  RETURN
ENDIF
*:->B128400 [End]

*** get the default values from approved values. ***
=lfGetDefu()
*** Get approve to pay and approve discount defalt. *** 
*B600846,1 HISH 11/29/95. Moved this part below with the same bug number (Begin)
*lnAprToPay=APINVHDR.NINVAMNT-NINVPAID-NINVADJ-;
          IIF(APINVHDR.NINVDISTK<APINVHDR.NINVDISOF,;
              APINVHDR.NINVDISOF-APINVHDR.NINVDISTK,0.00)
*lnAprDisc=IIF(APINVHDR.NINVDISTK<APINVHDR.NINVDISOF,;
          APINVHDR.NINVDISOF-APINVHDR.NINVDISTK,0.00)
*B600846,1. (End)

lcOldBank   = lcBankCode
lcOldCheck  = lcCheckCode
lcOldGlAcc  = lcGLAcct
lcHOldGlAc  = lcHGlAct

*E300296,4 Store old currency fields values
lcOldCurr   = lcAprCurCod
lnOldUnt    = lnAprCurUnt
lnOldExRat  = lnAprExRat
*E300296,4 end.

PUSH KEY
ON KEY

*** In case of approve partially get old values of payment, discount, and adjustment.
*** calling approved partially window.
IF APVENDOR.CVENPRIOR = '0' .OR. APINVHDR.CVENPRIOR = '0'
   lcvendorr=ALLTRIM(APVENDOR.CVENDCODE)
   ** MESSAGE : " Vendor XXXXXX has payment priority 0."
   **           " This vendor in on hold.              " 
   **           "                ® OK ¯                " 
   =gfModalGen("TRM04060B00000","DIALOG",lcvendorr)
   RETURN
ELSE
  IF APINVHDR.CINVSTAT = 'A'
     lcCanApr= "approve"
     ** MESSAGE : " You Can not CCCC a debit memo created by an advanced"
     **           " payment.                                            "
     **           "                    ® Ok ¯                           "
     =gfModalGen("TRM04058B00000","DIALOG",lcCanApr)
     RETURN
  ELSE    
*B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
*    IF (APINVHDR.NINVAMNT < 0) .AND. (APINVHDR.CVENPRIOR = '0')
    IF (APINVHDR.NINVAMNT < 0) .AND. (APINVHDR.CVENPRIOR = '0' .OR. APVENDOR.CVENPRIOR = '0')
*B600860,1 End.
      lcDebit="Debit memo "+APINVHDR.CINVNO
      ** MESSAGE : " YYYY XXXXX has payment priority 0 "
      **           " YYYY is on hold.                  "             
      **           "                    ® Ok ¯         "  
      =gfModalGen("TRM04059B00000","DIALOG",lcDebit+"|"+lcTDebitM)
      RETURN
    ELSE
*B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
*      IF (APINVHDR.NINVAMNT > 0) .AND. (APINVHDR.CVENPRIOR = '0')
      IF (APINVHDR.NINVAMNT > 0) .AND. (APINVHDR.CVENPRIOR = '0' .OR. APVENDOR.CVENPRIOR = '0')
*B600860,1 End.
        lcDebit="Invoice "+ALLTRIM(APINVHDR.CINVNO)
        ** MESSAGE : " YYYY XXXXX has payment priority 0 "
        **           " YYYY is on hold.                  "             
        **           "                    ® Ok ¯         "   
        =gfModalGen("TRM04059B00000","DIALOG",lcDebit+"|"+lcTInvoice)
        RETURN
      ELSE
        IF llAprPart  && Case approve part.
          IF EMPTY(APVENDOR.CVEN1099t)  && If the vendor 1099 amount is empty.
            lc1099St = 'DISABLE'
          ELSE
            lc1099St = 'ENABLE'
          ENDIF

          lnOldAprTPy = lnAprToPay
          lnOldAprDisc= lnAprDisc
          lnOldAprAdj = lnAprAdj
          lnO1099amnt = ln1099amnt
          
          *B600846,1 HISH 11/29/95. Put variable defualt values in a condition, (Begin)
          *B600846,1                that if this invoice aproved priviously put 
          *B600846,1                the last approve amount other with put the
          *B600846,1                open amount - paid amount - adjusted amount -discount.
          IF NINVAMTAP = 0
            lnAprToPay=APINVHDR.NINVAMNT-NINVPAID-NINVADJ-;
                       IIF(APINVHDR.NINVDISTK<APINVHDR.NINVDISOF,;
                       APINVHDR.NINVDISOF-APINVHDR.NINVDISTK,0.00)
             lnAprDisc=IIF(APINVHDR.NINVDISTK<APINVHDR.NINVDISOF,;
                       APINVHDR.NINVDISOF-APINVHDR.NINVDISTK,0.00)
             lnAprAdj   = 0
             ln1099amnt = 0
          ELSE
            *B600808,1 Initialize variables prior to the screen entry
            *B600808,1 instead of the when of the browse.
            lnAprToPay  = NINVAMTAP
            lnAprDisc   = NINVDISAP
            lnAprAdj    = NINVADJAP
            ln1099amnt  = NINVA1099
            *B600808,1 end.
          ENDIF            
          *B600846,1 (END)
          
          llCanclAp   = .F.            

          *E300296,4 Get the approved to pay amount in approval currency.
          *E300316,1 HISH 12/05/95. Used the variables hold signs in the equation. (Begin)                            
          *lnExchAmnt  = IIF(lnAprCurUnt > 0,;
                            ROUND(lnAprToPay * lnAprExRat / lnAprCurUnt, 2),;
                            0)
          lnExchAmnt  = IIF(lnAprCurUnt > 0 AND lnAprExRat > 0,;
                            ROUND(lnAprToPay &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2),;
                            0)
          *E300316,1 (End)                            
          *E300296,4 Disable rate field if editing is not allowed,
          *E300296,4 or if the currency code is that of the invoice.
          *E300296,4 or if the checking account is empty,
          lcRateDisp  = IIF(!llEditEx .OR. ;
                            APINVHDR.cCurrcode = APINVHDR.cAprCurCod .OR.;
                            (APINVHDR.CVENPMETH <> 'H' .AND. ;
                             EMPTY(lcCheckCode)),;
                            'DISABLE', 'ENABLE')
          *E300296,4 end.              
          
          IF APINVHDR.CVENPMETH <> 'H'
            RELEASE PAD _BROWSE OF _MSYSMENU
            *E300683,1 Call *.SPR from screens directory
            * DO APAPRPRT.SPR  && calling the approved all window.
            DO (gcScrDir + gcWinAppl + '\APAPRPRT.SPR')
            *E300683,1 end          
            
            *B608903,1 TMI [Start] Release the lock after the screen has been closed
            SELECT APINVHDR
            =lfObj_lock(.F.)
            *B608903,1 TMI [End  ] Release the lock after the screen has been closed
            
          ELSE
            RELEASE PAD _BROWSE OF _MSYSMENU
            
            *B601569,1 Add this line to set the default currency and 
            *          exchange rate [Begin]
            =lfSetDef()
            *B601569,1 Add this line [End]
            
            *E300683,1 Call *.SPR from screens directory
            * DO APAPPRT2.SPR 
            DO (gcScrDir + gcWinAppl + '\APAPPRT2.SPR')
            *E300683,1 end          
            
          ENDIF
          ***Refresh the invoice push button. ***
          IF _DOS
            SHOW GET pbInvoice,1 PROMPT 'I\<nv. '+ALLTRIM(APINVHDR.CINVNO)
          ENDIF
          ***Refresh say field open apen amount. ***
          =lfRefresh()
          lnBrRecNo  = RECNO('APINVHDR')
          SHOW WINDOW (lcBrTtl) REFRESH 
        ENDIF  
      ENDIF
    ENDIF
  ENDIF
ENDIF      

SHOW GET lnSesAmnt
SHOW GET lnSesDisc
SHOW GET lnSesAdj 
SHOW GET lnSes1099

*** Push the same keys again after coming ***
*** from global function browse. ***
POP KEY

*!**************************************************************************
*!
*!      Function: lfvAprov
*!
*!**************************************************************************
* Validation function of the <ok> push button of the Approved for payment.
*
FUNCTION lfvAprov

*** in case of approve partially. ***
IF llAprPart            && approve part validation. 
  ***if there is bankcode, check, and account but there isn't any payment, discount, or adjustment.
  *B600808,1 Change validation sequence
  *IF !EMPTY(lcBankCode+lcCheckCode+STRTRAN(STRTRAN(lcGlAcct,'-'),'0')) .AND.;
    lnAprToPay+lnAprDisc+lnAprAdj  = 0
  DO CASE
    *B600808,1 Copied from below.
    *B600808,1 Check if approved to pay amount is not zero.
    *** in case of not enter approved to pay amount. ***
    *CASE !EMPTY(lcBankCode+lcCheckCode+STRTRAN(STRTRAN(lcGlAcct,'-'),'0')) ;
           .AND. lnAprToPay = 0
    CASE lnAprToPay = 0       
      ** MESSAGE : " You have to enter the approved to pay   "
      **           " amount.                                 "
      **           "                    ® Ok ¯               "
      =gfModalGen("TRM04022B00000","DIALOG")
      *SHOW GET lnAprToPay
      _CUROBJ = OBJNUM(lnAprToPay)
      RETURN

    *B600808,1 Remove. There is no check for non check payment, 
    *B600808,1 probably meant cash payment. Substitute by the 
    *B600808,1 following case. In case of cash payments.
    *** in case of non check payment. ***
    *CASE CVENPMETH = 'N' .AND. EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0'))
       ** MESSAGE : You have to enter the ð account.
       **           "                    ® Ok ¯               "
       *=gfModalGen("TRM04020B00000","DIALOG",'GL')
     CASE cVenPMeth = 'H' .AND. EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0'))
        *B600808,1  Message : " You have to enter the ð account.     "
        *B600808,1                       ®  Ok  ¯             
        =gfModalGen("TRM04020B00000","DIALOG",lcTPayAcct)
        _CUROBJ = OBJNUM(lcGlAcct)
        RETURN
     
    *B600808,1 Remove. There is no special case for non check payment, 
    *B600808,1 probably meant cash payment. Substitute by the 
    *B600808,1 following case (in case of payment methods other than
    *B600808,1 cash payments).
    *** in case of there isn't bank, or check, or account. ***
    *CASE CVENPMETH <> 'N' .AND.;
    *     lnAprToPay+lnAprDisc+lnAprAdj > 0 .AND.;
    *     (EMPTY(lcBankCode) .OR. EMPTY(lcCheckCode);
    *                        .OR. EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0'));
    *                        .OR. (ATC("?",lcGlAcct)>0) )
    CASE cVenPMeth <> 'H'  .AND.;
         (EMPTY(lcBankCode) .OR.;
         EMPTY(lcCheckCode) .OR.; 
         EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')))
      ** MESSAGE : " You have to enter the bank code,the     "
      **           " checking account,and the GL account.    "
      **           "                    ® Ok ¯               "
      =gfModalGen("TRM04021B00000","DIALOG")
      IF EMPTY(lcBankCode)
        *SHOW GET lcBankCode
        _CUROBJ = OBJNUM(lcBankCode)
      ELSE
        IF EMPTY(lcCheckCode)
          *SHOW GET lcCheckCode
          _CUROBJ = OBJNUM(lcCheckCode)
        ELSE
           *SHOW GET lcGlAcct
           _CUROBJ = OBJNUM(lcGlAcct)
         ENDIF  
       ENDIF  
       RETURN 
      
    *B600808,1 Copied above to match the payable invoices validation.
    *** in case of not enter approved to pay amount. ***
    *CASE !EMPTY(lcBankCode+lcCheckCode+STRTRAN(STRTRAN(lcGlAcct,'-'),'0')) ;
    *     .AND. lnAprToPay = 0
    *  ** MESSAGE : " You have to enter the approved to pay   "
    *  **           " amount.                                 "
    *  **           "                    ® Ok ¯               "
    *  =gfModalGen("TRM04022B00000","DIALOG")
    *  SHOW GET lnAprToPay
    *  _CUROBJ = OBJNUM(lnAprToPay)
    *  RETURN
      
    *B600808,1 Check for 1099 amount. Change check condition.
    *CASE !EMPTY(lcBankCode+lcCheckCode+STRTRAN(STRTRAN(lcGlAcct,'-'),'0')) ;
           .AND. (!BETWEEN(ln1099amnt,lnAprToPay,0);
           .AND. !BETWEEN(ln1099amnt,0,lnAprToPay))
    CASE cVenPMeth <> 'H'  .AND. !llApS1099 .AND.;
         (!BETWEEN(ln1099amnt,lnAprToPay,0) .AND. ;
          !BETWEEN(ln1099amnt,0,lnAprToPay))
      ** MESSAGE : " The 1099 amount cannot be greater than  "
      **           " the ð.             "
      **           "                 ® Ok ¯                  "  
      =gfModalGen("TRM04061B00000","DIALOG","approved amount to pay")
      ln1099amnt=lnO1099amnt
      SHOW GET ln1099amnt
      _CUROBJ = OBJNUM(ln1099amnt)
      RETURN

    *E300296,4 Add a case for empty exchange rate    
    CASE (CVENPMETH = 'H' .OR. !EMPTY(lcBankCode)) .AND. lnAprExRat = 0
      IF lcAprCurCod <> APINVHDR.cCurrCode
        *B600849,1 HISH 11/29/95. Change parameter to get rate and unit to change from invoice (Begin)
        *B600849,1                currency to approved currency.
        *lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCurCod,APINVHDR.dInvDate, ;
                                .T., .F., APINVHDR.cCurrCode, .T.)

        *B601416,1 Change the calling of gfChkRate so if the [BEGIN]
        * Invoice currency is the base currency I make it the To currency 
        * and if not the Approve currency is the To currency 
        *lnAprExRat = gfChkRate('lnAprCurUnt', APINVHDR.cCurrCode ,APINVHDR.dInvDate, ;
        *                        .T., .F., lcAprCurCod, .T.)
        *B601416,1 IF Statment to check if the Invoice currency is the 
        * same as the base currency
        IF APINVHDR.cCurrCode = gcBaseCurr
          lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCurCod ,APINVHDR.dInvDate, ;
                                  .T., .F., APINVHDR.cCurrCode, .T.)
        ELSE      && Else
          lnAprExRat = gfChkRate('lnAprCurUnt', APINVHDR.cCurrCode ,APINVHDR.dInvDate, ;
                                  .T., .F., lcAprCurCod, .T.)
        ENDIF     && End of IF
        *B601416,1 Change the calling of gfChkRate [END]

        *E300316,1 HISH 12/05/95. Got the equation signs. (Begin)
        *E300316,1 to exchange currency from invoice currency to company base currency.
        *E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)                      
        lcExSin2 = ' '
        lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
        *lcExSin2 = IIF(lcExSin1 = '*','/','*')
        *E300316,1 to exchange currency from invoice currency to approved currency.
        lcExSin4 = ' '

        *B601416,1 Change the calling of gfGetExSin so if the [BEGIN]
        * Invoice currency is the base currency I make it the To currency 
        * and if not the Approve currency is the To currency 
        *lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
        *B601416,1 IF Statment to check if the Invoice currency is the 
        * same as the base currency
        IF APINVHDR.cCurrCode = gcBaseCurr 
          lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,APINVHDR.cCurrCode)
          lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
          lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
        ELSE   && Else
          lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
        ENDIF  && End of IF 
        *B601416,1 Change the calling of gfGetExSin [END]

        *lcExSin4 = IIF(lcExSin3 = '*','/','*')
        *E300316,1 (End)
        *B600849,1 (End)                             
        *E300296,4 If the exchange rate = 0, do not approve, and present
        *E300296,4 the following message
        IF lnAprExRat = 0
          *E300296,4 IF editing the exchange rate on the fly is not 
          *E300296,4 allowed, present the following message:
          *E300296,4 Message : " A valid ð to ð exchange rate could not "
          *E300296,4           " be found for ð.                        "
          *E300296,4           "                  ® Ok ¯                "

          *B601667,1 Change this line to fix the parameters [Begin]
          *=gfModalGen("INM04157B00000", "DIALOG",;
          *             ALLTRIM(APINVHDR.cCurrCode)+'|' ;
          *             +ALLTRIM(lcAprCurCod)+;
          *             '|'+DTOC(APINVHDR.dInvDate))

          *B601667,1 IF Statment to check if the Invoice currency is the
          *          Base Currency
          IF APINVHDR.cCurrCode = gcBaseCurr
            =gfModalGen("INM04157B00000", "DIALOG",;
                         ALLTRIM(lcAprCurCod)+'|' ;
                         +ALLTRIM(APINVHDR.cCurrCode)+;
                         '|'+DTOC(APINVHDR.dInvDate))
          ELSE     && Else
            =gfModalGen("INM04157B00000", "DIALOG",;
                         ALLTRIM(APINVHDR.cCurrCode)+'|' ;
                         +ALLTRIM(lcAprCurCod)+;
                         '|'+DTOC(APINVHDR.dInvDate))
          ENDIF     && End of IF
          *B601667,1 Change this line to fix the parameters [End]

          RETURN
        ENDIF  
      ELSE
        STORE 1 TO lnAprExRat, lnAprCurUnt
      ENDIF                         
      *E300296,4 end.

*B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
    CASE APVENDOR.CVENPRIOR = '0' .OR. CVENPRIOR = '0'
      lcvendorr=ALLTRIM(APVENDOR.CVENDCODE)
      ** MESSAGE : " Vendor XXXXXX has payment priority 0."
      **           " This vendor in on hold.              " 
      **           "                ® OK ¯                " 
      =gfModalGen("TRM04060B00000","DIALOG",lcvendorr)
      RETURN
*B600860,1 End.
    OTHERWISE
        CLEAR READ
  ENDCASE
  
  *B600808,1 Removed IF..ELSE..ENDIF condition
  *ELSE
  *  IF (EMPTY(lcBankCode) .OR. EMPTY(lcCheckCode);
  *                          .OR.  EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0'));
  *                          .OR. (ATC("?",lcGlAcct)>0) )
  *    ** MESSAGE : " You have to enter the bank code,the     "
  *    **           " checking account,and the GL account.    "
  *    **           "                    ® Ok ¯               "
  *    =gfModalGen("TRM04021B00000","DIALOG")
  *    RETURN
  *  ENDIF  
  *B600808,1 end.
  *CLEAR READ
  *ENDIF
ELSE && in case of approved fully
  
  *B600808,1 If payment method is not cash payment,
  IF APINVHDR.cVenPMeth <> 'H'
  *B600808,1 end.
    IF (EMPTY(lcBankCode) .OR. EMPTY(lcCheckCode);
                          .OR.  EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0'));
                          .OR. (ATC("?",lcGlAcct)>0) )
      ** MESSAGE : " You have to enter the bank code,the     "
      **           " checking account,and the GL account.    "
      **           "                    ® Ok ¯               "
      =gfModalGen("TRM04021B00000","DIALOG")
      RETURN
    ENDIF  

    *E300296,4 If multi currency and the exchange rate is zero, 
    *E300296,4 do not approve
    IF llMultiCr .AND. lnAprExRat = 0
      *E300296,4 IF editing the exchange rate on the fly is not 
      *E300296,4 allowed, present the following message:
      *E300296,4 Message : " A valid ð to ð exchange rate could not "
      *E300296,4           " be found for ð.                        "
      *E300296,4           "                  ® Ok ¯                "

      *B601667,1 Change this line to fix the parameters [Begin]
      *=gfModalGen("INM04157B00000", "DIALOG",;
      *             ALLTRIM(APINVHDR.cCurrCode)+'|' ;
      *             +ALLTRIM(lcAprCurCod)+;
      *             '|'+DTOC(APINVHDR.dInvDate))

      *B601667,1 IF Statment to check if the Invoice currency is the
      *          Base Currency
      IF APINVHDR.cCurrCode = gcBaseCurr
        =gfModalGen("INM04157B00000", "DIALOG",;
                     ALLTRIM(lcAprCurCod)+'|' ;
                     +ALLTRIM(APINVHDR.cCurrCode)+;
                     '|'+DTOC(APINVHDR.dInvDate))
      ELSE    && Else
        =gfModalGen("INM04157B00000", "DIALOG",;
                     ALLTRIM(APINVHDR.cCurrCode)+'|' ;
                     +ALLTRIM(lcAprCurCod)+;
                     '|'+DTOC(APINVHDR.dInvDate))
      ENDIF     && End of IF
      *B601667,1 Change this line to fix the parameters [End]


      RETURN
    ENDIF    && ENDIF llMultiCr .AND. lnAprExRat = 0
  ENDIF    && ENDIF APINVHDR.cVenPMeth <> 'H'
*B600860,1 M.H  12/04/95  The vendor or invoice that have payment priority = 0 will not be approved.
  IF APVENDOR.CVENPRIOR = '0' .OR. CVENPRIOR = '0'
    lcvendorr=ALLTRIM(APVENDOR.CVENDCODE)
    ** MESSAGE : " Vendor XXXXXX has payment priority 0."
    **           " This vendor in on hold.              " 
    **           "                ® OK ¯                " 
    =gfModalGen("TRM04060B00000","DIALOG",lcvendorr)
    RETURN
  ENDIF
*B600860,1 End.
ENDIF

SELECT APINVHDR

*** lock the pointed record and replaced the approved values. ***

*:->B128400 Move validation before opening the screen [Start]
*IF gfObj_lock(.T.) 

*B128400,1 WAM 10/12/2006 Commented out. Invoice already locked when click Partial approve
*IF lfObj_lock(.T.) && Added by TMI 08/24/2006
IF IIF(llAprPart, .T., lfObj_lock(.T.) )
*B128400,1 WAM 10/12/2006 (End)

*:->B128400 [End]

  IF CVENPMETH <> "H"

    *B601519,1 Change this line [Begin]
    *REPLACE CBNKCODE  WITH lcBankCode ,;
    *        CCHKACCT  WITH lcCheckCode,;
    *        CCHKGLACC WITH IIF(EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')),;
    *                                 SPACE(lnApsAcLen), lcGlAcct)

    *B601519,1 IF Statment to check if the Approved amount in the Approved 
    *             currency is not 0 or this vlidation function is not for  
    *             the Push Button pbApprov 
    IF lnExchAmnt <> 0 .OR. SYS(18) <> 'PBAPPROV'
      REPLACE CBNKCODE  WITH lcBankCode ,;
              CCHKACCT  WITH lcCheckCode,;
              CCHKGLACC WITH IIF(EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')),;
                                       SPACE(lnApsAcLen), lcGlAcct)
    ENDIF       && End of IF
    *B601519,1 Change this line [End]
    
  ELSE
    IF !EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0'))

      *B601519,1 Change this line [Begin]
      *REPLACE CCHKGLACC WITH lcGlAcct

      *B601519,1 IF Statment to check if the Approved amount in the Approved 
      *             currency is not 0 or this vlidation function is not for  
      *             the Push Button pbApprov 
      IF lnExchAmnt <> 0 .OR. SYS(18) <> 'PBAPPROV'
        REPLACE CCHKGLACC WITH lcGlAcct
      ENDIF
      *B601519,1 Change this line [End]

    ELSE
      ** MESSAGE : " You have to enter the bank code,the     "
      **           " checking account,and the GL account.    "
      **           "                    ® Ok ¯               "
      =gfModalGen("TRM04021B00000","DIALOG")
    ENDIF  
  ENDIF          
  ***in case of approved partially replaced the rest of approved values. ***
  IF llAprPart   && approve partially.
    *E300296,4 Show totals in base currency
    *lnSesAmnt = lnSesAmnt + lnAprToPay - NINVAMTAP
    *lnSesDisc = lnSesDisc + lnAprDisc  - NINVDISAP
    *lnSesAdj  = lnSesAdj  + lnAprAdj   - NINVADJAP
    *lnSes1099 = lnSes1099 + ln1099amnt - NINVA1099
    *E300316,1 HISH 12/05/95. Used the variables hold signs in the equation. (Begin)            
    *lnRateVal = APINVHDR.nExRate / APINVHDR.nCurrUnit    
    *lnSesAmnt = lnSesAmnt + ROUND((lnAprToPay - NINVAMTAP) * lnRateVal, 2)
    *lnSesDisc = lnSesDisc + ROUND((lnAprDisc  - NINVDISAP) * lnRateVal, 2)
    *lnSesAdj  = lnSesAdj  + ROUND((lnAprAdj   - NINVADJAP) * lnRateVal, 2)
    *lnSes1099 = lnSes1099 + ROUND((ln1099amnt - NINVA1099) * lnRateVal, 2)
    
    lnRateVal = APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit    

    *B601519,1 Change this line [Begin]
    *lnSesAmnt = lnSesAmnt + ROUND((lnAprToPay - NINVAMTAP) &lcExSin1 lnRateVal, 2)
    *B601519,1 IF Statment to check if the Approved amount in the Approved 
    *             currency is not 0 or this vlidation function is not for  
    *             the Push Button pbApprov 
    IF lnExchAmnt <> 0 .OR. SYS(18) <> 'PBAPPROV'
      lnSesAmnt = lnSesAmnt + ROUND((lnAprToPay - NINVAMTAP) &lcExSin1 lnRateVal, 2)
    ENDIF    && End of IF
    *B601519,1 Change this line [End]
        
    lnSesDisc = lnSesDisc + ROUND((lnAprDisc  - NINVDISAP) &lcExSin1 lnRateVal, 2)
    lnSesAdj  = lnSesAdj  + ROUND((lnAprAdj   - NINVADJAP) &lcExSin1 lnRateVal, 2)
    lnSes1099 = lnSes1099 + ROUND((ln1099amnt - NINVA1099) &lcExSin1 lnRateVal, 2)
    *E300316,1 (End)
    *E300296,4 end.
    
    *E300296,4 Update approved currency fields as well.
    *REPLACE NINVAMTAP WITH lnAprToPay ,;
             NINVDISAP WITH lnAprDisc  ,;
             NINVADJAP WITH lnAprAdj,;
             NINVA1099 WITH ln1099amnt

    *B601519,1 Change this line [Begin]
    *REPLACE NINVAMTAP  WITH lnAprToPay ,;
    *        NINVDISAP  WITH lnAprDisc  ,;
    *        NINVADJAP  WITH lnAprAdj   ,;
    *        NINVA1099  WITH ln1099amnt ,;
    *        cAprCurCod WITH lcAprCurCod,;
    *        nAprExRat  WITH lnAprExRat ,;
    *        nAprCurUnt WITH lnAprCurUnt         

    *B601519,1 IF Statment to check if the Approved amount in the Approved 
    *             currency is not 0  
    IF lnExchAmnt <> 0 
    
      *B601526,1 Change this line [Begin]
      *REPLACE NINVAMTAP  WITH lnAprToPay ,;
      *        NINVDISAP  WITH lnAprDisc  ,;
      *        NINVADJAP  WITH lnAprAdj   ,;
      *        NINVA1099  WITH ln1099amnt ,;
      *        cAprCurCod WITH lcAprCurCod,;
      *        nAprExRat  WITH lnAprExRat ,;
      *        nAprCurUnt WITH lnAprCurUnt,;         

      REPLACE NINVAMTAP  WITH lnAprToPay ,;
              NINVDISAP  WITH lnAprDisc  ,;
              NINVADJAP  WITH lnAprAdj   ,;
              NINVA1099  WITH ln1099amnt ,;
              cAprCurCod WITH lcAprCurCod,;
              nAprExRat  WITH lnAprExRat ,;
              nAprCurUnt WITH lnAprCurUnt,;         
              nInvFAAp   WITH lnExchAmnt         

      *B601526,1 Change this line [Begin]

    ENDIF   && End of IF
    *B601519,1 Change this line [End]
      
    *E300296,4 end.         
  ELSE           && approve fully. 

    *E300296,4 Show totals in base currency
    *E300296,4 Clear approved 1099 field as well.
    *lnSesAmnt = lnSesAmnt + APINVHDR.NINVAMNT-APINVHDR.NINVPAID-;
      APINVHDR.NINVDISTK-APINVHDR.NINVADJ-APINVHDR.NINVDISOF - APINVHDR.NINVAMTAP
    *lnSesDisc = lnSesDisc + APINVHDR.NINVDISOF - APINVHDR.NINVDISAP
    *lnSesAdj  = lnSesAdj  - APINVHDR.NINVADJAP
    *E300316,1 HISH 12/05/95. Used the variables hold signs in the equation. (Begin)                
    *lnRateVal = APINVHDR.nExRate / APINVHDR.nCurrUnit    
    *lnSesAmnt = lnSesAmnt + ROUND((APINVHDR.NINVAMNT ;
                                 - APINVHDR.NINVPAID ;
                                 - APINVHDR.NINVDISTK;
                                 - APINVHDR.NINVADJ  ;
                                 - APINVHDR.NINVDISOF;
                                 - APINVHDR.NINVAMTAP) * lnRateVal, 2)
    *lnSesDisc = lnSesDisc + ROUND((APINVHDR.NINVDISOF;
                                 - APINVHDR.NINVDISAP) * lnRateVal, 2)
    *lnSesAdj  = lnSesAdj  - ROUND(APINVHDR.NINVADJAP   * lnRateVal, 2)
    *B600808,1 Calculate 1099 amounts as well.
    *lnSes1099 = lnSes1099 - ROUND(APINVHDR.nInvA1099   * lnRateVal, 2)
    *B600808,1 end.
    
    lnRateVal = APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit    
    lnSesAmnt = lnSesAmnt + ROUND((APINVHDR.NINVAMNT ;
                                 - APINVHDR.NINVPAID ;
                                 - APINVHDR.NINVDISTK;
                                 - APINVHDR.NINVADJ  ;
                                 - APINVHDR.NINVDISOF;
                                 - APINVHDR.NINVAMTAP) &lcExSin1 lnRateVal, 2)
    lnSesDisc = lnSesDisc + ROUND((APINVHDR.NINVDISOF;
                                 - APINVHDR.NINVDISAP) &lcExSin1 lnRateVal, 2)
    lnSesAdj  = lnSesAdj  - ROUND(APINVHDR.NINVADJAP   &lcExSin1 lnRateVal, 2)
    lnSes1099 = lnSes1099 - ROUND(APINVHDR.nInvA1099   &lcExSin1 lnRateVal, 2)
    *E300316,1 (End)
    *E300296,4 end.
  
    SHOW GET lnSesAmnt
    SHOW GET lnSesDisc
    *B600808,1 Show lnSesAdj, lnSes1099
    SHOW GET lnSesAdj
    SHOW GET lnSes1099
    *B600808,1 end.
    
    *E300296,4 Update approved currency fields as well.    
    *B600808,1 Also clear aproved 1099 amount field.
    *REPLACE NINVAMTAP WITH (APINVHDR.NINVAMNT-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ-APINVHDR.NINVDISOF),;
             NINVDISAP WITH APINVHDR.NINVDISOF,;
             NINVADJAP WITH 0
    
    *B601526,1 Change this line [Begin]
    *REPLACE NINVAMTAP  WITH (APINVHDR.NINVAMNT-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ-APINVHDR.NINVDISOF),;
    *        NINVDISAP  WITH APINVHDR.NINVDISOF,;
    *        NINVADJAP  WITH 0,;
    *        nInvA1099  WITH 0,;
    *        cAprCurCod WITH lcAprCurCod,;
    *        nAprExRat  WITH lnAprExRat ,;
    *        nAprCurUnt WITH lnAprCurUnt,;

    REPLACE NINVAMTAP  WITH (APINVHDR.NINVAMNT-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ-APINVHDR.NINVDISOF),;
            NINVDISAP  WITH APINVHDR.NINVDISOF,;
            NINVADJAP  WITH 0,;
            nInvA1099  WITH 0,;
            cAprCurCod WITH lcAprCurCod,;
            nAprExRat  WITH lnAprExRat ,;
            nAprCurUnt WITH lnAprCurUnt,;
            nInvFAAp   WITH ROUND((APINVHDR.NINVAMNT-APINVHDR.NINVPAID-APINVHDR.NINVDISTK-APINVHDR.NINVADJ-APINVHDR.NINVDISOF) &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2)         
     
    *B601526,1 Change this line [End]

    *B600808,1 end.        
    *E300296,4 end.         
  ENDIF 
  =gfAdd_Info()  && Add the audit information to the record.        
  =gfObj_lock(.F.)  && unlock the pointed record.

  *B601519,1 Change this lines [Begin]
  *SKIP IIF(EOF(),0,1)   && if !eof go to the next record.
  *SKIP IIF(EOF(),-1,0)  && if went to eof return to the last record.

  *B601519,1 IF Statment to check if the Approved amount in the Approved 
  *             currency is not 0 or this vlidation function is not for  
  *             the Push Button pbApprov 
  IF lnExchAmnt <> 0 .OR. SYS(18) <> 'PBAPPROV' 
    SKIP IIF(EOF(),0,1)   && if !eof go to the next record.
    SKIP IIF(EOF(),-1,0)  && if went to eof return to the last record.
  ENDIF   && End of IF
  *B601519,1 Change this lines [Begin]
    
  IF !llAprPart
    ***Refresh the invoice push button. ***
    IF _DOS
      SHOW GET pbInvoice,1 PROMPT 'I\<nv. '+ALLTRIM(APINVHDR.CINVNO)
    ENDIF
    ***Refresh say field open apen amount. ***
    =lfRefresh()
    lnBrRecNo  = RECNO('APINVHDR')
    SHOW WINDOW (lcBrTtl) REFRESH 
  ENDIF

*:->B128400 Move validation to opening screen [Start]
ENDIF  && line decommented by TMI 08/24/2006
*:->B128400 [End]


*B601519,1 Add this lines [Begin]
lnExchAmnt  = IIF(lnAprCurUnt > 0 AND lnAprExRat > 0,;
                  ROUND(lnAprToPay &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2), 0)
SHOW GET lnAprExRat
SHOW GET lnExchAmnt
*B601519,1 Add this lines [End]

*!**************************************************************************
*!
*!      Function: lfvCanPay
*!
*!**************************************************************************
* Validation function of the cancel push button of the Approved for payment.
*
FUNCTION lfvCanPay

*** restore the old approved values. ***
lcBankCode  = lcOldBank 
lcCheckCode = lcOldCheck
lcGLAcct    = lcOldGlAcc
lcHGlAct    = lcHOldGlAc

lfvCanPay  = .T.

*** restore the old approved values in case of approved partially. ***
IF llAprPart

  *E300296,4 Restore old currency fields values
  lcAprCurCod = lcOldCurr
  lnAprCurUnt = lnOldUnt    
  lnAprExRat  = lnOldExRat 
  *E300296,4 end.
  
  lnAprToPay = lnOldAprTPy
  lnAprDisc  = lnOldAprDisc
  lnAprAdj   = lnOldAprAdj
  ln1099amnt = lnO1099amnt
ENDIF  

SELECT APINVHDR
llCanclAp   = .T.      &&set cancelled flag
CLEAR READ

*!**************************************************************************
*!
*!      Function: lfwAprDisc
*!
*!**************************************************************************
* When function of discount.
*
FUNCTION lfwAprDisc

*** save old discount value. ***
lnOldAprDisc = lnAprDisc

*!**************************************************************************
*!
*!      Function: lfvAprDisc
*!
*!**************************************************************************
* Valid function of the discount.
*
FUNCTION lfvAprDisc

*** ifinvoice amount is positive. *** 
IF APINVHDR.NINVAMNT >= 0
  *** if the total approved ammount not between zero and invoice amount. ***
  *B600808,1 Compare the new approved amount with the open invoice amount
  *B600808,1 excluding the old approved amounts, 
  *B600808,1 i.e. remove the approved amounts field from the open invoice
  *B600808,1 amount calculation.
  *IF !BETWEEN(lnAprToPay+lnAprDisc+lnAprAdj,0,;
     APINVHDR.NINVAMNT - APINVHDR.NINVPAID - APINVHDR.NINVDISTK - APINVHDR.NINVADJ; 
   + APINVHDR.NINVAMTAP+ APINVHDR.NINVDISAP+APINVHDR.NINVADJAP)
  IF !BETWEEN(lnAprToPay + lnAprDisc + lnAprAdj, 0, ;
               APINVHDR.nInvAmnt  - APINVHDR.nInvPaid ;
             - APINVHDR.nInvDisTk - APINVHDR.nInvAdj) 
  *B600808,1 end.

    ** MESSAGE : " Total approved amount can not be greater than "
    **           " the open amount.                              "
    **           "                       ® Ok ¯ 
    =gfModalGen("TRM04015B00000","DIALOG",lcTAprAmnt+"|"+lcTInvoice)
    lnAprDisc = lnOldAprDisc       && get the old discount amount.
    SHOW GET lnAprToPay
  ENDIF
ELSE && if the invoice amount is negative. ***
  *** if the total approved ammount not between invoice amount and zero. ***
  *B600808,1 Compare the new approved amount with the open invoice amount
  *B600808,1 excluding the old approved amounts, 
  *B600808,1 i.e. remove the approved amounts field from the open invoice
  *B600808,1 amount calculation.
  *IF !BETWEEN(lnAprToPay+lnAprDisc+lnAprAdj,;
              APINVHDR.NINVAMNT - APINVHDR.NINVPAID  - ;
              APINVHDR.NINVDISTK - APINVHDR.NINVADJ  + ;
              APINVHDR.NINVAMTAP + APINVHDR.NINVDISAP+ ;
              APINVHDR.NINVADJAP,0)
  IF !BETWEEN(lnAprToPay + lnAprDisc + lnAprAdj, ;
                APINVHDR.nInvAmnt  - APINVHDR.nInvPaid ;
              - APINVHDR.nInvDisTk - APINVHDR.nInvAdj, 0) 
  *B600808,1 end.
  
    ** MESSAGE : " Total approved amount can not be greater than "
    **           " the open amount.                              "
    **           "                       ® Ok ¯ 
    =gfModalGen("TRM04015B00000","DIALOG",lcTAprAmnt+"|"+lcTDebitM)
    lnAprDisc = lnOldAprDisc        && get the old discount amount.
    SHOW GET lnAprToPay
  ENDIF  
ENDIF   

*!**************************************************************************
*!
*!      Function: lfwAprToPay
*!
*!**************************************************************************
* When function of to pay.
*
FUNCTION lfwAprToPay

*** save the old approve to pay value ***
lnOldAprTPy = lnAprToPay
 
*!**************************************************************************
*!
*!      Function: lfvAprToPay
*!
*!**************************************************************************
* Validation function of to pay.
*
FUNCTION lfvAprToPay

*** if the invoice amount is positive. ***
IF APINVHDR.NINVAMNT >= 0
   *** if the total approved ammount not between zero and invoice amount. ***
   *B600808,1 Compare the new approved amount with the open invoice amount
   *B600808,1 excluding the old approved amounts, 
   *B600808,1 i.e. remove the approved amounts field from the open invoice
   *B600808,1 amount calculation.
   *IF !BETWEEN(lnAprToPay+lnAprDisc+lnAprAdj,0,;
               APINVHDR.NINVAMNT - APINVHDR.NINVPAID - APINVHDR.NINVDISTK - APINVHDR.NINVADJ; 
               + APINVHDR.NINVAMTAP+ APINVHDR.NINVDISAP+APINVHDR.NINVADJAP)
   IF !BETWEEN(lnAprToPay + lnAprDisc + lnAprAdj, 0, ;
                APINVHDR.nInvAmnt  - APINVHDR.nInvPaid ;
              - APINVHDR.nInvDisTk - APINVHDR.nInvAdj) 
   *B600808,1 end.
     ** MESSAGE : " Total approved amount can not be greater than "
     **           " the open amount.                              "
     **           "                       ® Ok ¯ 
     =gfModalGen("TRM04015B00000","DIALOG",lcTAprAmnt+"|"+lcTInvoice)
     lnAprToPay = lnOldAprTPy   && save the old payment amount. 
     SHOW GET lnAprToPay
   ENDIF
*** if the invoice amount is negative   
ELSE
  *** if the total approved ammount not between invoice amount and zero. ***
  *B600808,1 Compare the new approved amount with the open invoice amount
  *B600808,1 excluding the old approved amounts, 
  *B600808,1 i.e. remove the approved amounts field from the open invoice
  *B600808,1 amount calculation.
  *IF !BETWEEN(lnAprToPay+lnAprDisc+lnAprAdj,;
              APINVHDR.NINVAMNT - APINVHDR.NINVPAID  - ;
              APINVHDR.NINVDISTK - APINVHDR.NINVADJ  + ;
              APINVHDR.NINVAMTAP + APINVHDR.NINVDISAP+ ;
              APINVHDR.NINVADJAP,0)
  IF !BETWEEN(lnAprToPay + lnAprDisc + lnAprAdj,;
              APINVHDR.nInvAmnt - APINVHDR.nInvPaid  - ;
              APINVHDR.nInvDisTk - APINVHDR.nInvAdj, 0)
  *B600808,1 end.              
    ** MESSAGE : " Total approved amount can not be greater than "
    **           " the open amount.                              "
    **           "                       ® Ok ¯ 
    =gfModalGen("TRM04015B00000","DIALOG",lcTAprAmnt+"|"+lcTDebitM)
    lnAprToPay = lnOldAprTPy    && get the old payment amount.
    SHOW GET lnAprToPay
  ENDIF  
ENDIF   

*E300296,4 Get the approved to pay amount in approval currency.
*E300316,1 HISH 12/05/95. Used the variables hold signs in the equation. (Begin)                
*lnExchAmnt  = IIF(lnAprCurUnt > 0,;
                  ROUND(lnAprToPay * lnAprExRat / lnAprCurUnt, 2), 0)
lnExchAmnt  = IIF(lnAprCurUnt > 0 AND lnAprExRat > 0,;
                  ROUND(lnAprToPay &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2), 0)
*E300316,1 (End)
SHOW GET lnExchAmnt
*E300296,4 end.

*!**************************************************************************
*!
*!      Function: lfw1099amnt
*!
*!**************************************************************************
* When function of 1099 AMOUNT.
*
FUNCTION lfw1099amnt

*** save the old approve to pay value ***
lnO1099amnt = ln1099amnt
 
*!**************************************************************************
*!
*!      Function: lfv1099amnt
*!
*!**************************************************************************
* Validation function of 1099 AMOUNT.
*
FUNCTION lfv1099amnt

IF !BETWEEN(ln1099amnt,lnAprToPay,0) .AND. !BETWEEN(ln1099amnt,0,lnAprToPay)
   ** MESSAGE : " The 1099 amount cannot be greater than  "
   **           " the ð.     "
   **           "                 ® Ok ¯                  "  
   =gfModalGen("TRM04061B00000","DIALOG","approved amount to pay")
   ln1099amnt=lnO1099amnt
   SHOW GET ln1099amnt
ENDIF

*!**************************************************************************
*!
*!      Function: lfwAprAdj
*!
*!**************************************************************************
* When function of Appr. Adjust.
*
FUNCTION lfwAprAdj

*** save the old adjustment value. ***
lnOldAprAdj = lnAprAdj

*!**************************************************************************
*!
*!      Function: lfvAprAdj
*!
*!**************************************************************************
* Valid function of the Appr. Adjust.
*
FUNCTION lfvAprAdj

*** if the invoice ammount is positive. ***
IF APINVHDR.NINVAMNT >= 0
  *** if the total approved ammount not between zero and invoice amount. ***
  *B600808,1 Compare the new approved amount with the open invoice amount
  *B600808,1 excluding the old approved amounts, 
  *B600808,1 i.e. remove the approved amounts field from the open invoice
  *B600808,1 amount calculation.
  *IF !BETWEEN(lnAprToPay+lnAprDisc+lnAprAdj,0,;
                 APINVHDR.NINVAMNT - APINVHDR.NINVPAID - APINVHDR.NINVDISTK - APINVHDR.NINVADJ; 
                 + APINVHDR.NINVAMTAP+ APINVHDR.NINVDISAP+APINVHDR.NINVADJAP)
  IF !BETWEEN(lnAprToPay + lnAprDisc + lnAprAdj, 0, ;
                APINVHDR.nInvAmnt  - APINVHDR.nInvPaid ;
              - APINVHDR.nInvDisTk - APINVHDR.nInvAdj) 
  *B600808,1 end.
     ** MESSAGE : " Total approved amount can not be greater than "
     **           " the open amount.                              "
     **           "                       ® Ok ¯ 
     =gfModalGen("TRM04015B00000","DIALOG",lcTAprAmnt+"|"+lcTInvoice)
     lnAprAdj = lnOldAprAdj            && get the old adjustment amount.
     SHOW GET lnAprToPay
   ENDIF
*** if the invoice ammount is negative. ***   
ELSE 
  *** if the total approved ammount not between invoice amount and zero. ***
  *B600808,1 Compare the new approved amount with the open invoice amount
  *B600808,1 excluding the old approved amounts, 
  *B600808,1 i.e. remove the approved amounts field from the open invoice
  *B600808,1 amount calculation.
  *IF !BETWEEN(lnAprToPay+lnAprDisc+lnAprAdj,;
              APINVHDR.NINVAMNT - APINVHDR.NINVPAID  - ;
              APINVHDR.NINVDISTK - APINVHDR.NINVADJ  + ;
              APINVHDR.NINVAMTAP + APINVHDR.NINVDISAP+ ;
              APINVHDR.NINVADJAP,0)
  IF !BETWEEN(lnAprToPay + lnAprDisc + lnAprAdj, ;
                APINVHDR.nInvAmnt  - APINVHDR.nInvPaid ;
              - APINVHDR.nInvDisTk - APINVHDR.nInvAdj, 0) 
  *B600808,1 end.
  
    ** MESSAGE : " Total approved amount can not be greater than "
    **           " the open amount.                              "
    **           "                       ® Ok ¯ 
    =gfModalGen("TRM04015B00000","DIALOG",lcTAprAmnt+"|"+lcTDebitM)
    lnAprAdj = lnOldAprAdj              && get the old adjustment value.
    SHOW GET lnAprToPay
  ENDIF  
ENDIF   

*!**************************************************************************
*!
*!      Function: lfwVend
*!
*!**************************************************************************
* Save old vendor code.
*
FUNCTION lfwVend

lcOldVndcd = lcVendCode

*!**************************************************************************
*!
*!      Function: lfvVend
*!
*!**************************************************************************
*
FUNCTION lfvVend

*** if the new vend selected is the old vendor and mouse not clicked.
IF lcOldVndcd = lcVendCode .AND. !(llBrowse)
 RETURN                                        && go out of function.
ENDIF  

*** save the current alias. ***
lcSavAlias = ALIAS()

*** if not empty of vendor or the mouse clicked. ***
IF llBrowse .OR. !EMPTY(lcVendCode) 
  SELECT APVENDOR
  lcSavOrder = SET('ORDER')   && Save old order
  SET ORDER TO TAG VENCODE
  IF !llBrowse
    *** If the vendor enter by the user and exist in the file.***
    SEEK(ALLTRIM(UPPER(lcVendCode)))
  ENDIF
  IF FOUND() .AND. !llBrowse
    lcVendCode=CVENDCODE
    lcVendComp=CVENCOMP
  ELSE
    *** Define an array get Vendors codes and company.***
    IF llBrowse .OR. !SEEK(lcVendCode) .OR. ATC("?",lcVendCode) > 0
      DIMENSION laTemp[2]
      laTemp = '' && fill the array.
      *** Save the old fields and title.***   
      lcSavBrFld=lcBrFields 
      lcSavTitle=lcFile_Ttl 
    
      *** Get new fields name and title.***    
      lcBrFields="CVENDCODE :H= 'Vendor',;
                  CVENCOMP  :H= 'Company'"
      lcFile_Ttl="Vendors"
      *** Browse ***
      RELEASE PAD _BROWSE OF _MSYSMENU

      =gfBrows(.F.,'CVENDCODE,CVENCOMP','laTemp')
      *** Get the old fields name and title.***    
      lcBrFields=lcSavBrFld
      lcFile_Ttl=lcSavTitle
      *** if not empty of browse. ***
      IF !EMPTY(laTemp[1])
        lcVendCode = laTemp[1]  && get the vendor code.
        lcVendComp = laTemp[2]  && get the vendor company.
      ELSE                      && if empty
        lcVendCode = lcOldVndcd && get the old vendor code.
      ENDIF
    ENDIF  
  ENDIF
ENDIF

IF EMPTY(lcVendCode)
   lcVendComp = ""
   *B601554,1 Hesham El-Sheltawi (Start)
   *B601554,1 make the vendor code = "" so it dont effect the filter
   lcVendCode = ""
   *B601554,1 Hesham El-Sheltawi (End)   
ENDIF   

llBrowse=.F.

SELECT(lcSavAlias) 
=lfRefresh()

*!**************************************************************************
*!
*!      Function: lfwDivision
*!
*!**************************************************************************
*
FUNCTION lfwDivision

lcCodeFilt = 'CDIVISION'

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

    *E300643,1 Change this line for the changes we have made 
    *          to SYCCODES [Begin]
    *lcDivCode = gfActPop(6,14,12,47,'SYCCODES','cCode_No','cDiscrep',@lcDivision)
    lcDivCode = gfActPop(6,14,12,47,'CODES','cCode_No','cDiscrep',@lcDivision)
    *E300643,1 Change this line [End]
    
  CASE _WINDOWS

    *E300643,1 Change this lines for the changes we have made 
    *          to SYCCODES [Begin]
    *puDiv = SYCCODES.cdiscrep
    *lcDivCode = SYCCODES.cCode_No
    puDiv = CODES.cdiscrep
    lcDivCode = CODES.cCode_No
    *E300643,1 Change this lines [End]

    SHOW GET puDiv
ENDCASE

=gfUpdate()
=lfRefresh()
IF _WINDOWS
  DEACTIVATE POPUP puDivision
ENDIF  

SELECT APINVHDR

*!**************************************************************************
*!
*!      Function: lfvParior
*!
*!**************************************************************************
*parior not accept zero.
*
FUNCTION lfvParior

IF lcPriority = "0"
  lcPriority  = " "
  SHOW GET lcPriority
ENDIF

=lfRefresh()

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
    *** activate pop up and get payment method codes. ***
    lcMethod = gfActPop(8,49,13,70,'laPayMeth',2,1,@lcPayMeth)
  CASE _WINDOWS
    *** get payment method code. ***
    lcMethod = laPayMeth[lnMethod,2]
ENDCASE

=lfRefresh()

*!**************************************************************************
*!
*!      Function: lfvFrTodat
*!
*!**************************************************************************
*validate from through date in scope window.
*
FUNCTION lfvFrTodat

IF rbDuDsDt = 1 && in case of due date.
  ***if there is from and through date. ***
  IF !EMPTY(ldFrDueDat) .AND. !EMPTY(ldToDueDat)
    ***if from date is greater than through date.***
    IF ldFrDueDat > ldToDueDat   
      ** MESSAGE : "From Data can not be less than Through Date"
      **           "                       ® Ok ¯ 
      =gfModalGen("TRM04028B00000","DIALOG",lcTThrou+"|"+lcTFrom)
      _CUROBJ=OBJNUM(rbDuDsDt)
    ENDIF               
  ENDIF
ELSE && in case of discount date.
  ***if there is from and through date.***
  IF !EMPTY(ldFrDisDat) .AND. !EMPTY(ldToDisDat)
    ***if from date is greater than through date.***
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
*Branch between due date and discount date in a scope wine.
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

***clear the expresion***
lcExprsion = ""
***flag to deferntiate between <ok.> and <cancel>***
llScope = .T.

*** List for : Invoice - Debit memos - Both ***
DO CASE
  CASE rbScopeOn = 1
    lcExprsion = "((APINVHDR.NINVAMNT -APINVHDR.NINVPAID - "+;
                 "APINVHDR.NINVDISTK -APINVHDR.NINVADJ) > 0)"
  CASE rbScopeOn = 2
    lcExprsion = "((APINVHDR.NINVAMNT -APINVHDR.NINVPAID -"+ ;
                 "APINVHDR.NINVDISTK -APINVHDR.NINVADJ) < 0)"
  CASE rbScopeOn = 3
    lcExprsion = "((APINVHDR.NINVAMNT -APINVHDR.NINVPAID -"+ ;
                 "APINVHDR.NINVDISTK -APINVHDR.NINVADJ) <> 0)"
ENDCASE
***store radio button value.***
lnScopeOn=rbScopeOn

*** Division ***
IF lcDivCode <> "*"
  lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
               "CDIVISION = lcDivCode"
ENDIF
                            
*** Payment Pariority. ***
IF !EMPTY(lcPriority) 
  lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
               "CVENPRIOR = lcPriority"
ENDIF

***  Payment method.  ***
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

***    Due date.   ***
IF rbDuDsDt = 1
  DO CASE
    *** !empty "from"  and  empty "to"
    CASE !EMPTY(ldFrDueDat) .AND. EMPTY(ldToDueDat)
      lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
                   "DINVDUDAT >= ldFrDueDat"
    *** empty "from"  and  !empty  "to"
    CASE  EMPTY(ldFrDueDat) .AND. !EMPTY(ldToDueDat)
      lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
                   "DINVDUDAT <= ldToDueDat"
    *** !empty "from"  and  !empty "to"
    CASE !EMPTY(ldFrDueDat) .AND. !EMPTY(ldToDueDat)
      IF ldFrDueDat > ldToDueDat   && if from date greater than through date.
        ** MESSAGE : "From Data can not be less than Through Date"
        **           "                       ® Ok ¯ 
        =gfModalGen("TRM04028B00000","DIALOG",lcTThrou+"|"+lcTFrom)
        _CUROBJ=OBJNUM(rbDuDsDt)
        RETURN
      ELSE
        lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
                     "BETWEEN(DINVDUDAT,ldFrDueDat,ldToDueDat)"
      ENDIF               
  ENDCASE
ELSE
  ***  Discount date from.  ***
  DO CASE
    *** !empty "from"  and  empty "to"
    CASE !EMPTY(ldFrDisDat) .AND. EMPTY(ldToDisDat)
      lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
                   "DINVDUDAT >= ldFrDisDat"
    *** empty "from"  and  !empty  "to"
    CASE  EMPTY(ldFrDisDat) .AND. !EMPTY(ldToDisDat)
      lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
                   "DINVDUDAT <= ldToDisDat"
    *** !empty "from"  and  !empty "to"
    CASE !EMPTY(ldFrDisDat) .AND. !EMPTY(ldToDisDat)
      IF ldFrDisDat > ldToDisDat   
        ** MESSAGE : "From Data can not be less than Through Date"
        **           "                       ® Ok ¯ 
        =gfModalGen("TRM04028B00000","DIALOG",lcTThrou+"|"+lcTFrom)
        _CUROBJ=OBJNUM(rbDuDsDt)
        RETURN
      ELSE
        lcExprsion = lcExprsion + IIF(EMPTY(lcExprsion),"",".AND.") + ;
                     "BETWEEN(DINVDUDAT,ldFrDisDat,ldToDisDat)"  
      ENDIF               
  ENDCASE
ENDIF
*B600643,1 Stop the read clearing and make the OK key termianting in the screen
*CLEAR READ

*!**************************************************************************
*!
*!      Function: lfvCancel
*!
*!**************************************************************************
* Restore old values if push cancel from scope.
*
FUNCTION lfvCancel

llScope    = .F.

*** RESTORE OLD VAR. IN CURRENT VAR.
ldToDisDat = ldOToDisDt
ldFrDisDat = ldOFrDisDt
ldToDueDat = ldOToDueDt
ldFrDueDat = ldOFrDueDt
lcPriority = lcOldPrty 
lcVendCode = lcOVendCod
lcVendComp = lcOldVnCmp
lcMethod   = lcOMethod
lcDivCode  = lcOldDvCod
*B601648,1 Hesham El-Sheltawi (Start)
*B601648,1 Remark the next line because it gives an error 
*B601648,1 if we go in the Select Invoice scope screen again
*B601648,1 and at the same time the variable lcMethod was reinitialized 
*B601648,1 Previously
*lcMethod   = lcOldpymth
*B601648,1 Hesham El-Sheltawi (End)
lcInvRef   = lcOldInvRf
rbODuDsDt  = rbDuDsDt

*!**************************************************************************
*!
*!      Function: lfwBank
*!
*!**************************************************************************
* save the old bank
FUNCTION lfwBank
lcOldVal  = lcBankCode

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
*!      Function: lfvBnkChk
*!
*!**************************************************************************
* Valid function for get fields lcBankCode (bank code), and lcCheckCode
* (checking account code)
*
FUNCTION lfvBnkChk

*E300296,4 Store the old currency field values before validation
*PRIVATE llVldObj
PRIVATE lcOldCurr, lnOldRate, lnOldUnit, llVldObj
IF llBrowse .OR. EVALUATE(SYS(18)) <> lcOldVal 
  *E300296,4 lcOldCurr : old currency code
  *E300296,4 lnOldRate : old exchange rate
  *E300296,4 lnOldUnit : old currency unit
  lcOldCurr = lcAprCurCod
  lnOldRate = lnAprExRat
  lnOldUnit = lnAprCurUnt
  *E300296,4 end.

  llVldObj = lfBnkChk(@laBankObjs, lcOldVal, @llBrowse)

  *E300296,4 Get the currency of the checking account.
  IF llVldObj .AND. EVALUATE(SYS(18)) <> lcOldVal 
    lcAprCurCod = APCHECKS.cCurrCode
    IF lcAprCurCod <> APINVHDR.cCurrCode
      *B600849,1 HISH 11/29/95. Change parameter to get rate and unit to change from invoice (Begin)
      *B600849,1                currency to approved currency.
      *lnAprExRat  = gfChkRate('lnAprCurUnt'    , lcAprCurCod,;
                              APINVHDR.dInvDate, .F., .F., ;
                              APINVHDR.cCurrCode, .T.)

      *B601416,1 Change the calling of gfChkRate so if the [BEGIN]
      * Invoice currency is the base currency I make it the To currency 
      * and if not the Approve currency is the To currency 
      *lnAprExRat = gfChkRate('lnAprCurUnt', APINVHDR.cCurrCode ,APINVHDR.dInvDate, ;
      *                        .F., .F., lcAprCurCod, .T.)
      *B601416,1 IF Statment to check if the Invoice currency is the 
      * same as the base currency
      IF APINVHDR.cCurrCode = gcBaseCurr
        lnAprExRat = gfChkRate('lnAprCurUnt', lcAprCurCod ,APINVHDR.dInvDate, ;
                                .F., .F., APINVHDR.cCurrCode, .T.)
      ELSE      && Else
        lnAprExRat = gfChkRate('lnAprCurUnt', APINVHDR.cCurrCode ,APINVHDR.dInvDate, ;
                                .F., .F., lcAprCurCod, .T.)
      ENDIF     && End of IF               
      *B601416,1 Change the calling of gfChkRate [END]

      *B600849,1 (End)                              
      *E300316,1 HISH 12/05/95. Got the equation signs. (Begin)
      *E300316,1 to exchange currency from invoice currency to company base currency.
      *E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)                      
      lcExSin2 = ' '
      lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
      *lcExSin2 = IIF(lcExSin1 = '*','/','*')
      *E300316,1 to exchange currency from invoice currency to approved currency.
      lcExSin4 = ' '

      *B601416,1 Change the calling of gfGetExSin so if the [BEGIN]
      * Invoice currency is the base currency I make it the To currency 
      * and if not the Approve currency is the To currency 
      *lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
      *B601416,1 IF Statment to check if the Invoice currency is the 
      * same as the base currency
      IF APINVHDR.cCurrCode = gcBaseCurr 
        lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,APINVHDR.cCurrCode)
        lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
        lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
      ELSE   && Else
        lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
      ENDIF  && End of IF 
      *B601416,1 Change the calling of gfGetExSin [END]

      *lcExSin4 = IIF(lcExSin3 = '*','/','*')
      *E300316,1 (End)
      *E300316,1 HISH 12/05/95. Used the variables hold signs in the equation. (Begin)                            
      *lnExchAmnt  = IIF(lnAprCurUnt > 0,;
                        ROUND(lnAprToPay * lnAprExRat / lnAprCurUnt, 2), 0)
      lnExchAmnt  = IIF(lnAprCurUnt > 0 AND lnAprExRat > 0,;
                        ROUND(lnAprToPay &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2), 0)
      *E300316,1 (End)                        
    ELSE
      STORE 1 TO lnAprExRat, lnAprCurUnt
      lnExchAmnt  = lnAprToPay
    ENDIF
    SHOW GET lcAprCurCod
    SHOW GET lnExchAmnt    
    IF llEditEx .AND. lcAprCurCod <> APINVHDR.cCurrCode
      SHOW GET lnAprExRat ENABLE
    ELSE
      SHOW GET lnAprExRat DISABLE
    ENDIF    
  ELSE
    IF !EMPTY(lcBankCode)
      *E300296,4 Restore old currency fields
      lcAprCurCod  = lcOldCurr
      lnAprExRat   = lnOldRate 
      lnAprCurUnt  = lnOldUnit
    ELSE
      lcAprCurCod = SPACE(5)
      STORE 0 TO lnAprExRat, lnAprCurUnt, lnExchAmnt
      SHOW GET lcAprCurCod
      SHOW GET lnAprExRat DISABLE
      SHOW GET lnExchAmnt
    ENDIF   
  ENDIF
  *E300296,4 end.

  =lfRefresh()
  SELECT APINVHDR
  RETURN IIF(llVldObj, .T., 1)
ENDIF

*!**************************************************************************
*!
*!      Function: lfwGlAcct
*!
*!**************************************************************************
*
FUNCTION lfwGlAcct
lcOldAcc    = EVALUATE(SYS(18))

*!**************************************************************************
*!
*!      Function: lfvGlAcct
*!
*!**************************************************************************
*
FUNCTION lfvGlAcct
PRIVATE lcAcct

lcAcct      = SYS(18)

IF llBrowse .OR. !EMPTY(STRTRAN(STRTRAN(&lcAcct,'-'),'0'))
  IF !lfApAcs(.F.,llBrowse)
    &lcAcct   = lcOldAcc 
  ENDIF
ENDIF
IF EMPTY(STRTRAN(STRTRAN(&lcAcct,'-'),'0'))
  &lcAcct   = lcOldAcc
ENDIF
llBrowse = .F.
=lfRefresh()

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
ELSE
  =lfBrowUnTrap() 
ENDIF

*!**************************************************************************
*!
*!      FUNCTION : lfBrowTrap
*!
*!**************************************************************************
*
FUNCTION lfBrowTrap

ON KEY LABEL Alt+V       DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+I       DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+L       DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+E       DO  lpDoTrap WITH LASTKEY()             
ON KEY LABEL Alt+O       DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+N       DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+P       DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+Y       DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+D       DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+A       DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+S       DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+C       DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL ESC         DO  lpDoTrap WITH LASTKEY()
ON KEY LABEL Ctrl+ENTER  DO  lfvclose

*!**************************************************************************
*!
*!      PROCEDURE: lpDoTrap
*!
*!**************************************************************************
*
PROCEDURE lpDoTrap
PARAMETER lnLstKey

KEYBOARD '{Z}'CLEAR

IF !EOF('APINVHDR')
  DO CASE
    CASE lnLstKey = 27 
      IF _DOS
        HIDE MENU _MSYSMENU
        SHOW MENU _MSYSMENU
      ENDIF  
      ACTIVATE WINDOW (lcReadNam2)
      _CUROBJ = OBJNUM(pbClose)
      KEYBOARD "{ENTER}"
    CASE lnLstKey = 47 
      rbSort=1
      SHOW GET rbSort
      =lfvSortBy()
    CASE lnLstKey = 23
      rbSort=2
      SHOW GET rbSort
      =lfvSortBy()
    CASE lnLstKey = 24 
      ACTIVATE WINDOW (lcReadNam1)
      =lfvVendor()
    CASE lnLstKey = 49
      ACTIVATE WINDOW (lcReadNam1)
      =lfvInvoice()
    CASE lnLstKey = 25
      =lfGetDefu() 
      =lfvAprov() 
    CASE lnLstKey = 21
      =lfvApprPart(.T.)
    CASE lnLstKey = 32 
      =lfvDisAppr()
    CASE lnLstKey = 30 
      =lfvApprAll()
    CASE lnLstKey = 31 
      =lfvDApprAll()  
  ENDCASE
ENDIF  

DO CASE
  CASE lnLstKey = 38
    rbScope=1
    SHOW GET rbScope
    =lfvScope()
  CASE lnLstKey = 18
    rbScope=2
    SHOW GET rbScope
    =lfvScope()
  CASE lnLstKey = 46 
    ACTIVATE WINDOW (lcReadNam2)
    =lfvclose()
ENDCASE


*!**************************************************************************
*!
*!      FUNCTION : lfBrowUnTrap
*!
*!**************************************************************************
*
FUNCTION lfBrowUnTrap

ON KEY LABEL Alt+V
ON KEY LABEL Alt+I
ON KEY LABEL Alt+L
ON KEY LABEL Alt+E        
ON KEY LABEL Alt+O
ON KEY LABEL Alt+N
ON KEY LABEL Alt+P
ON KEY LABEL Alt+Y
ON KEY LABEL Alt+D
ON KEY LABEL Alt+A
ON KEY LABEL Alt+S
ON KEY LABEL Alt+C
ON KEY LABEL Ctrl+ENTER
*MAN
*IF _DOS
*  ON KEY LABEL ESC 
*ELSE
  ON KEY LABEL ESC &lcEscTrp
*ENDIF

*!**************************************************************************
*!
*!      FUNCTION : lfPushKey
*!
*!**************************************************************************
* trap keys
FUNCTION lfPushKey

*** places all current on key labels on a stack in memory.
*PUSH KEY

*** deactivate control keys for browse window 
***  TAB
ON KEY LABEL TAB         DO  lpTrap
ON KEY LABEL BACKTAB     DO  lpTrap 
*MAN
*IF _DOS
*  ON KEY LABEL ESC
*ELSE
  ON KEY LABEL ESC &lcEscTrp
*ENDIF

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
 
*** check if the window on top is  browse window
ON KEY LABEL TAB         lnDummi = 1
ON KEY LABEL BACKTAB     lnDummi = 1 

DO CASE
  ***Tab. or Right cursor***  
  CASE LASTKEY() = 9 .OR. LASTKEY() = 4
    IF _DOS
      HIDE MENU _MSYSMENU
      SHOW MENU _MSYSMENU
    ENDIF      
    IF WONTOP(lcReadNam1)
      IF _CUROBJ = OBJNUM(pbInvoice)
        ACTIVATE WINDOW (lcBrTtl)
      ELSE
        _CUROBJ = _CUROBJ + 1
      ENDIF
    ELSE  
      IF WONTOP(lcBrTtl)
        ACTIVATE WINDOW (lcReadNam2)
        _CUROBJ=OBJNUM(pbAprFul)
      ELSE
        IF _CUROBJ = OBJNUM(pbClose)
          ACTIVATE WINDOW (lcReadNam1)
           _CUROBJ=OBJNUM(rbSort)
        ELSE
          _CUROBJ = _CUROBJ + 1
        ENDIF
      ENDIF
    ENDIF  

  ***Shift Tab key or Left cursor key***    
  CASE  LASTKEY() = 15 .OR. LASTKEY() = 19
    IF _DOS
      HIDE MENU _MSYSMENU
      SHOW MENU _MSYSMENU
    ENDIF      
    IF WONTOP(lcReadNam2)
      IF _CUROBJ = OBJNUM(pbAprFul)
        ACTIVATE WINDOW (lcBrTtl)
      ELSE
        _CUROBJ = _CUROBJ - 1
      ENDIF
    ELSE  
      IF WONTOP(lcBrTtl)
        ACTIVATE WINDOW (lcReadNam1)
        _CUROBJ=OBJNUM(pbInvoice)
      ELSE
        IF _CUROBJ = OBJNUM(rbSort)
          ACTIVATE WINDOW (lcReadNam2)
           _CUROBJ=OBJNUM(pbClose)
        ELSE
          _CUROBJ = _CUROBJ - 1
        ENDIF
      ENDIF
    ENDIF  
ENDCASE

ON KEY LABEL TAB         DO  lpTrap
ON KEY LABEL BACKTAB     DO  lpTrap 

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : RENEE - Renee Ezzat
*! Date      : 11/02/1995
*! Purpose   : When function for lnAprExRat field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwOldVal()
*!*************************************************************
*E300296,4 Added this function as a when function for the new 
*E300296,4 field, lnAprExRat
FUNCTION lfwOldVal
lnOldVal = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvExRate
*! Developer : RENEE - Renee Ezzat
*! Date      : 11/02/1995
*! Purpose   : Valid function for lnAprExRat field
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
*E300296,4 field, lnAprExRat
FUNCTION lfvExRate
*E300296,4 if the entered value is not greater than zero,
*E300296,4 Message :  "   ð should be greater than ð.   "
*E300296,4                 ®   OK   ¯
IF lnAprExRat <> lnOldVal
  IF lnAprExRat < 0 .AND. ;
    gfModalGen("TRM04072B00000","DIALOG", lcTExRateMsg) > 0
    lnAprExRat = lnOldVal
  ENDIF  
  *E300316,1 HISH 12/05/95. Got the equation signs. (Begin)
  *E300316,1 to exchange currency from invoice currency to approved currency.
  *E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)                        
  lcExSin4 = ' '

  *B601416,1 Change the calling of gfGetExSin so if the [BEGIN]
  * Invoice currency is the base currency I make it the To currency 
  * and if not the Approve currency is the To currency 
  *lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
  *B601416,1 IF Statment to check if the Invoice currency is the 
  * same as the base currency
  IF APINVHDR.cCurrCode = gcBaseCurr 
    lcExSin3 = gfGetExSin(@lcExSin4,lcAprCurCod,APINVHDR.cCurrCode)
    lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
    lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
  ELSE   && Else
    lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,lcAprCurCod)
  ENDIF  && End of IF 
  *B601416,1 Change the calling of gfGetExSin [END]

  *lcExSin4 = IIF(lcExSin3 = '*','/','*')
  *E300316,1 (End)

  *E300296,4 Get the approved to pay amount in approval currency.
  *E300316,1 HISH 12/05/95. Used the variables hold signs in the equation. (Begin)
  *lnExchAmnt  = IIF(lnAprCurUnt > 0,;
                    ROUND(lnAprToPay * lnAprExRat / lnAprCurUnt, 2), 0)
  lnExchAmnt  = IIF(lnAprCurUnt > 0 AND lnAprExRat > 0,;
                    ROUND(lnAprToPay &lcExSin3 lnAprExRat &lcExSin4 lnAprCurUnt, 2), 0)
  *E300316,1 (End)                    
  SHOW GET lnExchAmnt
ENDIF 
*E300296,4 end                

*!*************************************************************
*! Name      : lfwAprCurCod
*! Developer : RENEE - Renee Ezzat
*! Date      : 11/15/1995
*! Purpose   : When function for lcAprCurCod field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwAprCurCod()
*!*************************************************************
*E300296,4 Added this function as a when function for the new 
*E300296,4 field, lcAprCurCod
FUNCTION lfwAprCurCod
lcOldVal = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvAprCurCod
*! Developer : RENEE - Renee Ezzat
*! Date      : 11/15/1995
*! Purpose   : Valid function for lcAprCurCod field
*!*************************************************************
*! Calls     : lfGetExt()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAprCurCod()
*!*************************************************************
*E300296,4 Added this function as a validation for the new 
*E300296,4 field, lcAprCurCod
FUNCTION lfvAprCurCod
PARAMETERS llGetExRate
PRIVATE laRetVal, lcCurObj
DECLARE laRetVal[1]
laRetVal     = " "
lcCurObj = SYS(18)

*E300296,4 Validate an entered currency code, or browse for one
*E300296,4 Call lfGetExt()
*IF lfGetExt('SYCCURR', 'CCURRCODE', lcOldVal, llBrowse,  'cCurrCode',;
            @laRetVal, 'cCurrCode', .F., .F., gcSysHome)
IF lfGetExt('SYCCURR', 'CCURRCODE', lcOldVal, llBrowse,  'cCurrCode',;
            @laRetVal)
  &lcCurObj   = laRetVal[1]
  IF llGetExRate
    IF laRetVal[1] <> APINVHDR.cCurrCode
      *B600849,1 HISH 11/29/95. Change parameter to get rate and unit to change from invoice (Begin)
      *B600849,1                currency to approved currency.
      *STORE gfChkRate('lnAprCurUnt'    , laRetVal[1],     ;
                              APINVHDR.dInvDate, .T., .F.,;
                              APINVHDR.cCurrCode, .T.)    ;
            TO lnAprExRat

      *B601416,1 Change the calling of gfChkRate so if the [BEGIN]
      * Invoice currency is the base currency I make it the To currency 
      * and if not the Approve currency is the To currency 
      *STORE gfChkRate('lnAprCurUnt'    ,APINVHDR.cCurrCode,     ;
      *                        APINVHDR.dInvDate, .T., .F.,;
      *                        laRetVal[1], .T.)    ;
      *      TO lnAprExRat 
      *B601416,1 IF Statment to check if the Invoice currency is the 
      * same as the base currency
      IF APINVHDR.cCurrCode = gcBaseCurr
        STORE gfChkRate('lnAprCurUnt'    ,laRetVal[1],     ;
                                APINVHDR.dInvDate, .T., .F.,;
                                APINVHDR.cCurrCode, .T.)    ;
              TO lnAprExRat 
      ELSE      && Else
        STORE gfChkRate('lnAprCurUnt'    ,APINVHDR.cCurrCode,     ;
                                APINVHDR.dInvDate, .T., .F.,;
                                laRetVal[1], .T.)    ;
              TO lnAprExRat 
      ENDIF     && End of IF
      *B601416,1 Change the calling of gfChkRate [END]

            
      *B600849,1 (End)   
      *E300316,1 HISH 12/05/95. Got the equation signs. (Begin)
      *E300316,1 to exchange currency from invoice currency to company base currency.
      *E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)                        
      lcExSin2 = ' '
      lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.cCurrCode)
      *lcExSin2 = IIF(lcExSin1 = '*','/','*')
      *E300316,1 to exchange currency from invoice currency to approved currency.
      lcExSin4 = ' '

      *B601416,1 Change the calling of gfGetExSin so if the [BEGIN]
      * Invoice currency is the base currency I make it the To currency 
      * and if not the Approve currency is the To currency 
      *lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,laRetVal[1])
      *B601416,1 IF Statment to check if the Invoice currency is the 
      * same as the base currency
      IF APINVHDR.cCurrCode = gcBaseCurr 
        lcExSin3 = gfGetExSin(@lcExSin4,laRetVal[1],APINVHDR.cCurrCode)
        lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
        lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
      ELSE   && Else
        lcExSin3 = gfGetExSin(@lcExSin4,APINVHDR.cCurrCode,laRetVal[1])
      ENDIF  && End of IF 
      *B601416,1 Change the calling of gfGetExSin [END]

      *lcExSin4 = IIF(lcExSin3 = '*','/','*')
      *E300316,1 (End)
      
      *E300316,1 HISH 12/05/95. Used the variables hold signs in the equation. (Begin)                         
      *lnExchAmnt  = IIF(lnAprCurUnt > 0,;
                          ROUND(lnAprToPay * lnAprExRat ;
                                         / lnAprCurUnt, 2), 0)
      lnExchAmnt  = IIF(lnAprCurUnt > 0 AND lnAprExRat > 0,;
                          ROUND(lnAprToPay &lcExSin3 lnAprExRat ;
                                         &lcExSin4 lnAprCurUnt, 2), 0)
      *E300316,1 (End)                                         
      
      IF llEditEx
        SHOW GET lnAprExRat ENABLE
      ELSE
        SHOW GET lnAprExRat DISABLE
      ENDIF  
    ELSE
      STORE 1 TO lnAprExRat, lnAprCurUnt
      lnExchAmnt = lnAprToPay  
      SHOW GET lnAprExRat DISABLE
    ENDIF  
    SHOW GET lnExchAmnt
  ENDIF
ENDIF  
llBrowse     = .F.

*!*************************************************************
*! Name      : lfSetDef                              
*! Developer : Haytham El_Sheltawi
*! Date      : 03/30/1997
*! Purpose   : Function to set the default currency and exchange rate
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : lfvApprPart
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfSetDef()
*!*************************************************************
*B601569,1 This function was added by HS for the bug
*!*************************************************************
*
FUNCTION lfSetDef

IF !EMPTY(APINVHDR.nInvAmtAp)
  lcAprCurCod = APINVHDR.cAprCurCod
  lnAprExRat = APINVHDR.nAprExRat
  lnExchAmnt = APINVHDR.nInvFAAp
  lnAprToPay = APINVHDR.nInvAmtAp
  lnAprDisc = APINVHDR.nInvDisAp
  lnAprAdj = APINVHDR.nInvAdjAp
  ln1099Amnt = APINVHDR.nInvA1099
  lnAprCurUnt = APINVHDR.nCurrUnit

  IF APINVHDR.cCurrCode = gcBaseCurr 
    lcExSin3 = gfGetExSin(@lcExSin4 , lcAprCurCod , APINVHDR.cCurrCode)
    lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
    lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
  ELSE   && Else
    lcExSin3 = gfGetExSin(@lcExSin4 , APINVHDR.cCurrCode , lcAprCurCod)
  ENDIF  && End of IF
ENDIF

*!*************************************************************
*! Name      : lfObj_Lock
*! Developer : Mohamed Shokry
*! Date      : 
*! Purpose   : To object lock any record in any file
*!*************************************************************
*! Calls     : 
*!      Called by: GFCPDELETE()             (function  in ARIA3.PRG)
*!      Called by: GFCPEDIT()               (function  in ARIA3.PRG)
*!      Called by: GFCPSAVE()               (function  in ARIA3.PRG)
*!      Called by: GFCPCLOSE()              (function  in ARIA3.PRG)
*!          Calls: GFMODALGEN()             (function  in ARIA3.PRG)
*!          Calls: GFGETTIME()              (function  in ARIA3.PRG)
*!*************************************************************
*! Passed Parameters  : flage to lock or unlock
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*:->B128400 new function to modify message appear
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
          lcRtyCncMs = "Invoice "+ALLTRIM(APINVHDR.CINVNO)+" for vendor "+ ALLTRIM(CVendCode)+" is being edited by user " + lcLok_User+"."
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
    REPLACE lLok_Stat WITH .T.       , ;   
             cLok_User WITH gcUser_ID , ;
             dLok_Date WITH DATE()    , ;
             cLok_Time WITH gfGetTime()

    lRet_Flag  = .T.    
  ENDIF
ENDIF


UNLOCK


RETURN lRet_Flag

