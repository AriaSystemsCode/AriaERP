*:************************************************************************
*:
*: Procedure file: APADVPAY.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Hisham Ramsis Philips
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: lfvPayNum
*:               : lfvChkNum
*:               : lfvClose
*:               : lfwDebMemN
*:               : lfvDebMemN
*:               : lfwPayAmnt
*:               : lfvPayAmnt
*:               : lfwPayDat
*:               : lfvPayDat
*:               : lfw1099Amnt
*:               : lfv1099Amnt
*:               : lfvPay
*:               : lfVProPay
*:               : lfVCanPay
*:               : lfObjDis
*:               : lfDefBank
*:               : lfvDivision
*:               : lfwOldValue
*:               : lfvBnkChk
*:               : lfwGlAcct
*:               : lfvGlAcct
*:               : lfvRemit
*:               : lfvFactor
*:               : lfwVend
*:               : lfvVendor
*:
*:      Documented 00/00/1994
*:************************************************************************
*E300296,1 M.H   11/05/95 Add the currency to the AP module.
*E300316,1 HISH  11/28/95 Used global function gfGetExSin to get currency 
*E300316,1                equation signs.
*E300316,1 HISH  01/08/96 Passed pointer parameter to get Unit sgin.
*B601013,1 RENEE 04/17/96 1. If not cash payment, enable editing the 
*B601013,1                   exchange rate of the bank\checking account.     
*B601013,1                2. Variable not found 'lnOldRate' (validating
*B601013,1                   exchange rate
*B601013,1                3. Do not accept the payment if a valid exchange
*B601013,1                   rate does not exist for the currency code.
*B601013,1                4. Reset currency browsing button variable.
*B601013,1                5. Dos : Non cash payent screen. APADVPA.SPR
*B601013,1                   Text on the bank code displays 'Currency'
*B601013,1                - Function lfwPayDat is removed from APADVPA.SPR
*B601092,1 M.H   06/03/96 Miss calculation in the equavelant field in case of cash payment.
*C100728,1 M.H   12/08/96 Add the posting date field to the APINVHDR file.
*E300643,1  HS 04/14/97 1)Make some changes for we have change the file
*E300643,1              SYCCODES name to CODES and make it a data file
*E300643,1              [Change its dir. from SYSFILES to DBFS]
*E300643,1              2)Make some changes for we have change the function
*E300643,1              [gfCodDes]
*E300663,1  HS 04/23/97 Change the calling of the function [gfSequence]
*E300663,1              for the changes we have made to that function.
*E300683,1 AHMED 06/04/97 Add screens directory path to the calling of SPRS
*E301077,79 AMM 03/04/99 Enhance opening and closing files.
*E300789,4  AMM 03/07/99 Remove field cComp_id from files Accod, FisHD, FsPrd, FsHld, Codes
*B803185,1 WMA 04/12/00 1) Get Address 3,4,5 Separated With Comma.
*B803185,1   			2) Get and save the Country.
*B603647,1 AME 06/06/2000 Get the Ex. Rate & Unit or curr. by Function GfchkRate
*B603689,1 AMH 06/18/00 Fix Exchange Rate update in ApPaymnt
*B606829,1 MHM 11/20/2003 The bank G/L account doesn't refresh if we change Vendor
*:*****************************************************************************
** Manual checks, Non check payments, Cash payments. ***
PARAMETER lnPyChMN         && Refere where the program is called from 
PRIVATE laBankObjs 
DECLARE laRemitTo[3,2],laBankObjs[3,3]

** laRemitTo            Array to hold Remit to Popup options
** laBankObjs           Array to hold bank objects'names for validations      
** lcSesNum             Variable to hold session number.
** lcOldCheck           Variable to hold old check code. 
** lcVendCode           Variable to hold vendor code.
** lcOVndVal            Variable to hold old vendor code,comp,phone.
** lcOVndcod            Variable to hold old vendor code.
** lcVendComp           Variable to hold vendor company.
** lcOVndcmp            Variable to hold old vendor company.
** lcVPhone             Variable to hold vendor phone.
** lcOVPhone            Variable to hold old vendor phone.
** lcVendDiv            Variable to hold vendor division.
** lcDivision           Variable to hold division description.
** lcDivCode            Variable to hold division code.
** lcCInvRef            VAriable to hold current Inv. Ref.
** lcOVendCod           Variable to hold old vendor code.
** lcOldVnCmp           Variable to hold old vendor company.
** lcOldDivDs           Variable to hold old division description.
** lcOldDvCod           Variable to hold old division code.
** lcOldAcc             Variable to hold old account.
** lcOldVal             Variable to hold old character field value
** lcDebMemN            Variable to hold debit memo number.
** lcODebMemN           Variable to hold old debit memo number.
** lcTinv               Variable to hold word invoice. 
** lcRem1               Variable to hold remit to first line.
** lcRem2               Variable to hold remit to scond line.
** lcRem3               Variable to hold remit to third line.
** lcRem4               Variable to hold remit to forth line.
** lcPeriod             Variable to hold First period. 
** lcFisPrd             Variable to hold First period.
** lcYear               Variable to hold first year.
** lcFisYer             Variable to hold first year.
** lcRef                Variable to hold reference.
** lcTDebMem            Variable to hold word Debit memo.
** lcTRef               Variable to hold word Reference.
** lcTsession           Variable to hold word Session.
** lcPhone              Variable to hold vendor telephone.
** lcCompany            Variable to hold vendor company.
** lcRemitTo            say field for remit to popup (DOS)
** lcInvRemit           remit to code ('V', 'F','O')     
** lcFactor             factor code
** lcTAcct              Variable to hold     
** lcFactStat           state of display of factor field and inv. button
** lcRemitStat          state of display of remit to fields
** lcDataStat           state of display of remit to popup
** lcGlAcct             Variable to hold Gl account. 
** lcAPAcct             Variable to hold AP account. 
** lcColPair            Variable to hold 
** lcColor              Variable to hold     
** lcInvRef             Variable to hold Inv. Ref.
** lcOldInvRf           Variable to hold old Inv. Ref.
** lcBankCode           Variable to hold bank code.
** lcCheckcode          Variable to hold check code. 
**
** puDiv                Variable to define pop up.
** puDivision           Variable to define pop up.
** puRemitTo            remit to popup variable
**
** lnDivDes             Variable to hold pu Division description.
** lnOldRemit           Variable to hold old remit.
** ln1099Amnt           Variable to hold 1099 amount.
** lnO1099Amnt          Variable to hold old 1099 amount.
** lnPayAmnt            Variable to hold pay amount. 
** lnOPayAmnt           Variable to hold old pay amount.
** lnChkNum             Variable to hold Check number.
** lnOChkNum            Variable to hold old check number. 
** lnGnChkNm            Variable to hold Generated check number.
** lnRemitLen           remit to popup width
**
** llBrowse             Variable to hold left mouse clicked or not. 
** llPay                Variable to hold if paid or not.
** llNoContrl           no control panel (AP.PRG)
**
** ldPayDat             Variable to hold Payment Date.
** ldOPayDat            Variable to hold old payment date. 
**
** laCtrStat            To disable the browse pad in the menu

*E300296,1 M.H   11/05/95 Add the currency variables.
** lcOldCurr            Variable to hold the old currency.
** lcCurrCode           Variable to hold the currency.

** lnCurrRate           Variable to hold the currency rate.
** lnCurrUnit           Variable to hold the currency unit.
** lnOldRate            Variable to hold the old currency rate.
*E300296,1 M.H End.

*E300316,1 HISH  11/28/95. Added currency equation sign variables. (Begin)
** lcExSin1             Variable to hold the first sign in the equation.
** lcExSin2             Variable to hold the second sign in the equation.
*E300316,1 (End)

*B803185,1 WMA 04/12/00 Add Var For Country And Array to Hold Address 1,2,3 and Country  ****Begin****
** lcRem5               Variable to hold Country
** laCompAdr[4]         Array to Hold Address 1,2,3 and Country 
*B803185,1 WMA 04/12/00                                                                  **** End ****

STORE  ' '        TO lcSesNum  , lcVendCode , lcOVndVal , lcOVndcod ,;
                     lcVendComp, lcOVndcmp  , lcVPhone  , lcOVPhone ,;
                     lcVendDiv , lcDivision , lcObjDisp , lcDivCode ,;
                     lcCInvRef , lcOVendCod , lcOldVnCmp, lcOldDivDs,;
                     lcOldDvCod, lcOldVal   , lcOldAcc  , lcDebMemN ,;
                     lcODebMemN, lcTinv     , lcRem1    , lcRem2    ,;
                     lcRem3    , lcRem4     , lcPeriod  , lcFisPrd  ,;
                     lcYear    , lcFisYer   , lcRef     , lcTDebMem ,;
                     lcTRef    , lcTsession , lcPhone   , lcCompany ,;
                     lcRemitTo , lcInvRemit , lcFactor  , lcTAcct   ,;
                     puDiv     , puDivision , lcOldCurr , lcCurrCode,;
                     lcExSin1  , lcExSin2

*B803185,1 WMA 04/12/00 [Begin]
*    	    1) Define Var lcRem5 to hold the Country.
*			2) Define Array to Hold Address 1,2,3 and Country.
STORE ' ' TO lcRem5
DIMENSION laCompAdr[4]
*B803185,1 WMA 04/12/00 [End]

*B601013,1 Add lcRateDisp for the display status of the exchange rate.
STORE "DISABLE"   TO lcFactStat, lcRemitStat, lcDataStat, laCtrStat ,;
                     lcObjDisp , lcRateDisp
STORE lcEmptyAcc  TO lcGlAcct  , lcAPAcct
STORE SCHEME(1,2) TO lcColPair , lcColor
STORE SPACE(15)   TO lcInvRef  , lcOldInvRf
STORE SPACE(12)   TO lcCheckcode
STORE SPACE(8)    TO lcBankCode
STORE 1           TO puRemitTo , lnDivDes   , lnOldRemit
STORE 0.00        TO ln1099Amnt, lnO1099Amnt, lnPayAmnt , lnOPayAmnt

*E300296,1 M.H default the currency rate,unit by 0.
*STORE 0           TO lnChkNum  , lnOChkNum  , lnGnChkNm , lnRemitLen,;
                     lnCurrRate, lnCurrUnit

*B601013,1 Add lnOldRate declaration
*STORE 0           TO lnChkNum  , lnOChkNum  , lnGnChkNm , lnRemitLen,;
                     lnCurrRate, lnCurrUnit
STORE 0           TO lnChkNum  , lnOChkNum  , lnGnChkNm , lnRemitLen,;
                     lnCurrRate, lnCurrUnit , lnOldRate
*E300296,1 M.H End.
                     
STORE .F.         TO llBrowse  , llPay
STORE .T.         TO llNoContrl
STORE gdSysDate   TO ldPayDat
STORE {}          TO ldOPayDat

*B601013,1 Get multi currency settings
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

** Array laFields is used by lfSekVnd() and formed as follows :
** A row for every field to be browsed
** Column 1 : the object name corresponding to the field
** Column 2 : the physical field name
** Column 3 : the tag to be used in ssearching

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

IF _DOS
  lcTAcct       = IIF(lnPyChMN = 3,"Cash accnt. :","G/L account :")
ELSE
  lcTAcct       = IIF(lnPyChMN = 3,"Cash acct. :","G/L acct.   :")  
ENDIF

IF _WINDOWS

  *E300643,1 Change this lines for the changes we have made to 
  *          SYCCODES [Begin]
  *puDiv = SYCCODES.cdiscrep
  *
  *DEFINE POPUP puDivision prompt field SYCCODES.cdiscrep scroll;
  *FROM 4.75,50.63 TO 10.75,73.75 ;
  *MESSAGE gfObj_msg()

  puDiv = CODES.cdiscrep

  DEFINE POPUP puDivision prompt field CODES.cdiscrep scroll;
  FROM 4.75,50.63 TO 10.75,73.75 ;
  MESSAGE gfObj_msg()
  *E300643,1 Change this lines [End]
  
  ON SELECTION POPUP puDivision DO lfvDivision
ENDIF

** Get names to Brawse and Read window **
IF !WEXIST(gcBaseWind)
  ** Get the active session number. **
  IF EMPTY(lcSesNum)

    *E300663,1 Change this line for the changes we have made to 
    *          (gfSequence) [Begin]
    *lcSesNum    = PADL(gfSequence("APSESS",1,gcAct_Comp),8,'0')
    lcSesNum    = gfSequence('CAPSESSNO')
    *E300663,1 Change this line [End]
    
  ENDIF  

  ** Prepare Remit to array from SYDFIELD and get its maximum width
  lnRemitLen = gfGetVld('cInvRemit',@laRemitTo)
  lcInvRemit = 'V'
  lcRemitTo  = laRemitTo[1,1]   
  
  ** Prepare an array to hold bank objects to be used for
  ** global bank and checking accounts validations as follows :
  ** One row for every object, such that 
  ** row no. 1 holds bank object names,
  ** row no. 2 holds checking account object names
  ** row no. 3 holds the corresponding G/L account object names,
  ** Columns are ordered as follows :
  ** Column no. 1 : invisible button name for corresponding object
  ** Column no. 2 : object name (e.g. bank object name)
  ** Column no. 3 : object description name(if required)
  laBankObjs  = ' '
  laBankObjs[1,1] = 'ibBank'         && Bank code invisible button
  laBankObjs[1,2] = 'lcBankCode'     && Bank code 
  laBankObjs[2,1] = 'ibChecks'       && Checking account invisible button
  laBankObjs[2,2] = 'lcCheckCode'    && Checking account 
  laBankObjs[3,1] = 'ibGlAcc'        && G/L account invisible button  
  laBankObjs[3,2] = 'lcGlAcct'       && G/L account

ELSE
  puRemitTo  = lnOldRemit
  lcObjDisp  = IIF(lnPyChMN = 3, lcDataStat, lcObjDisp)
  *B601013,1 Add a variable for the display status of the exchange rate 
  *B601013,1 field
  lcRateDisp = IIF(EMPTY(lcCurrCode) .OR. lcCurrCode = gcBaseCurr .OR. !llEditEx,; 
                    'DISABLE', lcObjDisp)
  *B601013,1 end.
ENDIF


** Get Codes from codes file.

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

*E300789,4  AMM Adjust the filter to adjust new structure.
*SET FILTER TO (CCOMP_ID+CRLTFIELD+CFLD_NAME = gcAct_Comp+'N'+'CDIVISION') OR;
              (CCOMP_ID+CRLTFIELD+CFLD_NAME ='  '+'N'+'N/A')
SET FILTER TO (cDefCode+CRLTFIELD+CFLD_NAME = 'N'+'N'+'CDIVISION') OR;
              (cDefCode+CRLTFIELD+CFLD_NAME ='N'+'N'+'N/A')
*E300789,4  AMM end

SELECT APPAYMNT
*E300683,1 Call *.SPR from screens directory
* DO APADVPA.SPR 
DO (gcScrDir + gcWinAppl + '\APADVPA.SPR')
*E300683,1 end          

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

SET FILTER TO 

SELECT APPAYMNT

RELEASE POPUPS puDivision

*!**************************************************************************
*!
*!      Function: lfvPayNum
*!
*!**************************************************************************
*Check if the payment no. exists and not duplicate.
FUNCTION lfvPayNum
    
IF EMPTY(lcPayNum)
   ** Message: " You have to enter the ð.     "
   ** Choices: "            ® Ok ¯            "  
   =gfModalGen("TRM04066B00000","DIALOG",'payment number')
ELSE
  SELECT APPAYMNT
  lcSavOrder = SET('ORDER')
  IF SYS(22) <> 'TYPMETHNO' 
    SET ORDER TO TAG TYPMETHNO
  ENDIF  
  IF lnPyChMN = 3        && Case calling bar is Cash payment.
    IF SEEK('PH'+SPACE(8)+SPACE(12)+lcPayNum)
      ** Message: " The payment number already exists. "
      ** Choices: "               ® Ok ¯               "  
      =gfModalGen("TRM04051B00000","DIALOG")
    ENDIF
  ELSE                   && Case calling bar is Non check payment.
    IF SEEK('PN'+lcBankCode+lcCheckcode+lcPayNum)
      ** Message: " The payment number already exists. "
      ** Choices: "               ® Ok ¯               "  
      =gfModalGen("TRM04051B00000","DIALOG")
    ENDIF
  ENDIF
  SET ORDER TO &lcSavOrder
ENDIF

SELECT APPAYMNT

*!**************************************************************************
*!
*!      Function: lfvChkNum
*!
*!**************************************************************************
*Check if the chknum exists and not duplicate.
*
FUNCTION lfvChkNum

IF EMPTY(lnChkNum)
   lnChkNum=lnGnChkNm
ELSE
  SELECT APPAYMNT
  lcSavOrder = SET('ORDER')
  IF SYS(22) <> 'TYPMETHNO' 
    SET ORDER TO TAG TYPMETHNO
  ENDIF  
  IF SEEK('PM'+lcBankCode+lcCheckCode+PADL(lnChkNum,8,'0'))
    ** Message: " A manual check already exists with this number."
    **          " You can not have duplicate manual check numbers"
    **          " for the same checking account.                 "
    ** Choices: "                       ® Ok ¯                   "
    =gfModalGen("TRM04053B00000","DIALOG")
  ELSE
    SELECT APCHECKS  
    IF lnChkNum <> lnOChkNum  && If generated check no changed.
      REPLACE NCHKNXTMN WITH lnChkNum
      =gfAdd_Info()  && Add the audit information to the record.
    ENDIF  
  ENDIF
  SELECT APPAYMNT
  SET ORDER TO &lcSavOrder
ENDIF

*!**************************************************************************
*!
*!      Function: lfvClose
*!
*!**************************************************************************
*
FUNCTION lfvClose

glQuitting = .T.

*!**************************************************************************
*!
*!      Function: lfwDebMemN
*!
*!**************************************************************************
* 
FUNCTION lfwDebMemN

lcODebMemN = lcDebMemN
 
*!**************************************************************************
*!
*!      Function: lfvDebMemN
*!
*!**************************************************************************
* 
FUNCTION lfvDebMemN

*!**************************************************************************
*!
*!      Function: lfwPayAmnt
*!
*!**************************************************************************
* 
FUNCTION lfwPayAmnt

lnOPayAmnt = lnPayAmnt

*!**************************************************************************
*!
*!      Function: lfvPayAmnt 
*!
*!**************************************************************************
* 
FUNCTION lfvPayAmnt

*!**************************************************************************
*!
*!      Function: lfwPayDat
*!
*!**************************************************************************
*Save old payment 
*
FUNCTION lfwPayDat

ldOPayDat = ldPayDat 

*!**************************************************************************
*!
*!      Function: lfvPayDat
*!
*!**************************************************************************
* Valdation of the invoce date.
*
FUNCTION lfvPayDat 

*E300296,1 M.H Validate the currency rate with the new date.
IF ldPayDat <> ldOPayDat
  *B601013,1 If the payment date is empty, do not validate and
  *B601013,1 clear the exchange rate
  IF EMPTY(ldPayDat)
    lnCurrRate = 0
    SHOW GET lnCurrRate DISABLE
  ELSE
    *B601013,1 end.
    ** Get the year and period from apsetup. **
    IF lfVDtMsg(gcPrnt_Cmp,@lcFisPrd,@lcFisYer,ldPayDat)
      lcPeriod = lcFisPrd
      lcYear   = lcFisYer
      *B601013,1 Store the current date as old date
      ldOPayDat = ldPayDat 
      *B601013,1 Get the exchange rate if the date is valid.
      IF llMultiCr
        lnCurrRate = gfChkRate('lnCurrUnit',lcCurrCode,ldPayDat,;
                     .T.,gcAct_Comp, gcBaseCurr, .T.)
        *E300316,1 HISH  11/28/95. Got the equation signs. (Begin)
        *E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
        lcExSin2 = ' '
        lcExSin1 = gfGetExSin(@lcExSin2,lcCurrCode)
        *E300316,1 (End)
        IF lnCurrRate = 0 .AND. !llAddRate
          ** MESSAGE : " A valid ð to ð exchange rate could not "
          **           " be found for ð.                        "
          **           "                  ® Ok ¯                "
          =gfModalGen("QRM04157B00000","DIALOG",;
                       ALLTRIM(lcCurrCode)+'|'+ALLTRIM(gcBaseCurr);
                       +'|'+DTOC(ldPayDat))
        ENDIF
        IF lcCurrCode = gcBaseCurr .OR. !llEditEx
          SHOW GET lnCurrRate DISABLE
        ELSE
          SHOW GET lnCurrRate ENABLE
        ENDIF
      ENDIF
    ELSE
      ldPayDat = ldOPayDat  
      *B601013,1 If the payment date is empty, clear the exchange rate
      IF EMPTY(ldPayDat)
        lnCurrRate = 0
        SHOW GET lnCurrRate DISABLE
      ENDIF  
      *B601013,1 end.
      _CUROBJ  = OBJNUM(ldPayDat)
      RETURN
    ENDIF  
  *B601013,1 Add an ENDIF
  ENDIF
  *B601013,1 Commented the next lines and moved the checks to the upper
  *B601013,1 paragraph.
  *IF gfGetMemVar('LLMULCURR')
  *  lnCurrRate = gfChkRate('lnCurrUnit',lcCurrCode,ldPayDat,.F.,.F.)
  *  *E300316,1 HISH  11/28/95. Got the equation signs. (Begin)
  *  *E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)
  *  lcExSin2 = ' '
  *  lcExSin1 = gfGetExSin(@lcExSin2,lcCurrCode)
  *  *lcExSin2 = IIF(lcExSin1 = '*','/','*')
  *  *E300316,1 (End)
  *  IF lnCurrRate = 0 .AND. lcCurrCode <> gcBaseCurr
  *    ** MESSAGE : " A valid ð to ð exchange rate could not "
  *    **           " be found for ð.                        "
  *    **           "                  ® Ok ¯                "
  *    =gfModalGen("QRM04157B00000","DIALOG",ALLTRIM(gcBaseCurr)+'|'+ALLTRIM(lcCurrCode)+'|'+DTOC(ldPayDat))
  *    lnCurrRate = 1
  *    lnCurrUnit = 1
  *  ENDIF
  *  IF lcCurrCode = gcBaseCurr .OR. !gfGetMemVar('LLEDITEXRA')
  *    SHOW GET lnCurrRate DISABLE
  *  ELSE
  *    SHOW GET lnCurrRate ENABLE
  *  ENDIF
  *ENDIF
  *B601013,1 end.
ENDIF
*E300296,1 M.H End.
*B601013,1 end.


*!**************************************************************************
*!
*!      Function: lfw1099Amnt
*!
*!**************************************************************************
* 
FUNCTION lfw1099Amnt

lnO1099Amnt = ln1099Amnt

*!**************************************************************************
*!
*!      Function: lfv1099Amnt
*!
*!**************************************************************************
* 
FUNCTION lfv1099Amnt

SHOW GET ln1099Amnt ENABLE

*!**************************************************************************
*!
*!      Function: lfvPay
*!
*!**************************************************************************
*Activate the Pay invoices screen.
FUNCTION lfvPay

IF lnPyChMN <> 3 .AND. EMPTY(lcBankCode) 
  ** If the bank code is empty, present the following message 
  ** and return to the bank code field
  ** Message : "   You have to enter the ð.  "
  **                 ® OK  ¯
  =gfModalGen("TRM04066B00000","DIALOG",lcTBnkCode)
  _CUROBJ = OBJNUM(lcBankCode)
  RETURN 
ENDIF

*B601013,1 Initialize multi currency variables before rate checking
*E300296,1 M.H 11/05/95 Default the currency by the base currency if
*E300296,1 M.H 11/05/95 not multi currency.
IF !llMultiCr
  lcCurrCode = gcBaseCurr
  lnCurrRate = 1
  lnCurrUnit = 1
  lcExSin2   = ' '
  lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
ENDIF  
*E300296,1 M.H End.
*B601013,1 end

*B601013,1 Change date validation as follows 
*B601013,1 Check if the date is empty,
IF EMPTY(ldPayDat)
  **  Message: "You have to enter The ð.       "
  **  Choices: "              ® Ok ¯           "  
  =gfModalGen("TRM04066B00000","DIALOG",'payment date')
  _CUROBJ = OBJNUM(ldPayDat)
  RETURN
ELSE
*B601013,1 Validate the date if it has not been validated before
  =lfvPayDat()
  IF EMPTY(ldPayDat) 
    RETURN
  ENDIF  

  *B601013,1 Check if the exchange rate is 0.
  IF lnCurrRate = 0
    lnCurrRate = gfChkRate('lnCurrUnit',lcCurrCode,ldPayDat,;
                     .T.,gcAct_Comp, gcBaseCurr, .T.)
    IF lnCurrRate = 0 
      IF !llAddRate
        ** MESSAGE : " A valid ð to ð exchange rate could not "
        **           " be found on ð.                         "
        **           "                  ® Ok ¯                "
        =gfModalGen("QRM04157B00000","DIALOG",ALLTRIM(lcCurrCode)+'|'+ALLTRIM(gcBaseCurr)+'|'+DTOC(ldPayDat))
      ENDIF  
      _CUROBJ=OBJNUM(ldPayDat)
      RETURN
    ENDIF  
    SHOW GET lnCurrRate
  ENDIF
ENDIF  
*B601013,1 end.

IF EMPTY(lcDebMemN)
  ** Message: " You have to enter the ð.    "
  ** Choices: "           ® Ok ¯            "  
  =gfModalGen("TRM04066B00000","DIALOG",lcTDebMem)
  lcDebMemN = lcODebMemN
  _CUROBJ=OBJNUM(lcDebMemN)
  RETURN
ELSE
  SELECT APINVHDR
  lcSavOrder = SET('ORDER')
  SET ORDER TO TAG VENDINV
  IF SEEK(lcVendCode+lcDebMemN)
    lcTinvno = lcTinv+lcDebMemN 
    ** Message: " Invoice XXXXXXXXXX is already exist "
    **          " for vendor YYYYYYY.                 "
    ** Choices: "               ® Ok ¯                "  
    =gfModalGen("TRM04024B00000","DIALOG",lcTinvno+"|"+ALLTRIM(lcVendCode))
    lcDebMemN = lcODebMemN
    _CUROBJ=OBJNUM(lcDebMemN)
    RETURN
  ENDIF  
  SET ORDER TO &lcSavOrder
  SELECT APPAYMNT
ENDIF  

IF lnPayAmnt <= 0
  ** Message: " The payment amount should be greater"
  **          " than zero.                          "
  ** Choices: "               ® Ok ¯                "  
  =gfModalGen("TRM04096B00000","DIALOG")
  lnPayAmnt = lnOPayAmnt
  _CUROBJ=OBJNUM(lnPayAmnt)
  RETURN
ENDIF  

IF !BETWEEN(ln1099Amnt,0,lnPayAmnt)
  ** Message : " The 1099 amount must be between 0 and XXXXXX"
  ** Choices : "                    ® Ok ¯                   "
  =gfModalGen("TRM04017B00000","DIALOG","0|"+STR(lnPayAmnt))
  ln1099Amnt = lnO1099Amnt
  _CUROBJ=OBJNUM(ln1099Amnt)
  RETURN
ENDIF

*B601013,1 Remove date validation. Done above.
** Get the year and period from apsetup. **
*IF lfVDtMsg(gcPrnt_Cmp,@lcFisPrd,@lcFisYer,ldPayDat)
*  lcPeriod = lcFisPrd
*  lcYear   = lcFisYer
*ELSE
*  ldPayDat = ldOPayDat  
*  _CUROBJ  = OBJNUM(ldPayDat)
*  RETURN
*ENDIF  
*B601013,1 end.

** If the invoice is to be remit to factor, and there is
** no factor code, do not proceed and present the following message
IF lcInvRemit = 'F' .AND. EMPTY(lcFactor)
  ** Message : " You have to enter the ð.      "
  ** Choices : "             ® Ok ¯            "  
  =gfModalGen("TRM04066B00000","DIALOG",lcTFactor)
  _CUROBJ = OBJNUM(lcFactor)
  RETURN
ENDIF

IF lnPyChMN = 1   && Calling from manual check.
  **Get the next manual check number from apchecks** 
  SELECT APCHECKS
  IF SEEK(lcBankCode+lcCheckCode)
    IF gfObj_lock(.T.) && lock the pointed record.
      lnOChkNum = NCHKNXTMN
      lnGnChkNm = NCHKNXTMN + 1
      lnChkNum  = lnOChkNum
      REPLACE NCHKNXTMN WITH lnGnChkNm
      =gfAdd_Info()  && Add the audit information to the record.
      =gfObj_lock(.F.)
    ELSE
      ** Message:" The checking account lcBankCode-lcCheckCode is"
      **         " being edited.   Cannot generate the next check"
      **         " number.                                       "
      ** Choices:"             < Retry >      < Cancel >         "
      =gfModalGen("TRM04055B00015","DIALOG",ALLTRIM(lcBankCode);
                   +"-"+ALLTRIM(lcCheckCode))
    ENDIF
  ENDIF 
  SELECT APPAYMNT
ENDIF

*E300683,1 Call *.SPR from screens directory
* DO APPYADIN.SPR 
DO (gcScrDir + gcWinAppl + '\APPYADIN.SPR')
*E300683,1 end          

IF llPay  &&Return form pay not from cancel 
  =lfObjDis(.T.)
  _CUROBJ=OBJNUM(lcVendCode) 
  llPay =.F.
ENDIF

*!**************************************************************************
*!
*!      Function: lfVProPay
*!
*!**************************************************************************
*
FUNCTION lfVProPay

SELECT APINVHDR
lcSavOrder = SET('ORDER')
SET ORDER TO TAG VENDINV

IF SEEK(lcVendCode+lcDebMemN)
  SET ORDER TO &lcSavOrder
  lcTinvno = lcTinv+lcDebMemN
  ** Message: " Invoice XXXXXXXXXX is already exist"
  **          " for vendor YYYYYYY.                "
  ** Choices: "               ® Ok ¯               "  
  =gfModalGen("TRM04024B00000","DIALOG",lcTinvno+"|"+ALLTRIM(lcVendCode))
  lcDebMemN = lcODebMemN
  _CUROBJ =OBJNUM(lcDebMemN)
  RETURN
ENDIF  

SET ORDER TO &lcSavOrder
IF lnPyChMN = 1  && if manual check.
  IF(EMPTY(lcBankCode) .OR. EMPTY(lcCheckCode);
      .OR. EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')) ;
      .OR. EMPTY(STRTRAN(STRTRAN(lcApAcct,'-'),'0')))
    ** MESSAGE: " You have to enter the bank code,the   "
    **          " checking account,and the GL account.  "
    ** Choices: "                 ® Ok ¯                "
    =gfModalGen("TRM04021B00000","DIALOG")
    RETURN
  ENDIF
ENDIF

*B601013,1 Moved the following part up before rate checking
*E300296,1 M.H 11/05/95 Default the currency by the base currency if
*E300296,1 M.H 11/05/95 not multi currency.
*IF !gfGetMemVar('LLMULCURR')
*  lcCurrCode = gcBaseCurr
*  lnCurrRate = 1
*  lnCurrUnit = 1
*ENDIF  
*E300296,1 M.H End.
*B601013,1 end.

IF lnPyChMN <> 1   && if not manual check.
  IF EMPTY(lcPayNum)
    ** Message: " You have to enter The ð.           "
    ** Choices: "               ® Ok ¯               "  
    =gfModalGen("TRM04066B00000","DIALOG",'payment number')
    _CUROBJ = OBJNUM(lcPayNum)
    RETURN  && <------ quit function.
  ELSE
    SELECT APPAYMNT
    IF SEEK('PH'+SPACE(8)+SPACE(12)+lcPayNum) .OR.;
       SEEK('PN'+lcBankCode+lcCheckCode+lcPayNum)
      ** Message: " The payment number already exists. "
      ** Choices: "               ® Ok ¯               "  
      =gfModalGen("TRM04051B00000","DIALOG")
      _CUROBJ=OBJNUM(lcPayNum)
      RETURN  && <------ quit function.
    ENDIF 
  ENDIF
ELSE && in case of manual check. 
  IF EMPTY(lnChkNum)
    lnChkNum=lnGnChkNm
    SHOW GET lnChkNum
    RETURN && <------ quit function.
  ELSE
    IF lnChkNum < 0
      ** Message: " Negative values are not allowed.   "
      ** Choices: "               ® Ok ¯               "  
      =gfModalGen("TRM04087B00000","DIALOG")
      lnChkNum=lnOChkNum
      SHOW GET lnChkNum
      RETURN && <------ quit function.
    ELSE
      SELECT APPAYMNT
      IF SEEK('PM'+lcBankCode+lcCheckCode+PADL(lnChkNum,8,'0'))
        ** Message: " A manual check already exists with this number."
        **          " You can not have duplicate manual check numbers"
        **          " for the same checking account.                 "    
        ** Choices: "                      ® Ok ¯                    "
        =gfModalGen("TRM04053B00000","DIALOG")
        _CUROBJ=OBJNUM(lnChkNum)
        RETURN  && <------ quit function.
      ELSE
        SELECT APCHECKS  
        IF lnChkNum <> lnOChkNum  && If generated check no changed.
          REPLACE NCHKNXTMN WITH lnChkNum
          =gfAdd_Info()  && Add the audit information to the record.
        ENDIF  
      ENDIF
    ENDIF  
  ENDIF
ENDIF  

** Make sure that the record will be modified are locked. **
*E301077,79 AMM Open file
=gfOpenFile(gcDataDir+'APVENHST','VENDYEAR','SH')
*E301077,79 AMM end

SELECT APVENDOR
IF SEEK(lcVendCode)
  SELECT APVENHST
  lcSavOrder = SET('ORDER')
  IF SYS(22) <> 'VENDYEAR' 
    SET ORDER TO TAG VENDYEAR
  ENDIF
  
  IF SEEK(lcVendCode+lcYear)
    IF gfRlock('APVENDOR',.T.) .AND. gfRlock('APVENHST',.T.)
      *B603647,1  Check the Checking account currency rate with gcBaseCurr with the proceed date.  AME[Start]
      lnChkExUnt = 0
      lnChkExRat = 0
      IF gfGetMemVar('LLMULCURR')
        *B603689,1 AMH (Ahmed Maher) Fix fields Rate,Unit (Start)
        *lnChkExRat = gfChkRate('lnChkExUnt',lcCurrCode,ApPayMnt.DPAYDATE,.T.,.F.)
        lnChkExRat = gfChkRate('lnChkExUnt',lcCurrCode,ldPayDat,.T.,.F.)
        *B603689,1 AMH (Ahmed Maher) Fix fields Rate,Unit (End)
        IF lnChkExRat = 0
          lnChkExUnt = 1
          lnChkExRat = 1
        ENDIF
      ELSE
        lnChkExUnt = 1
        lnChkExRat = 1
      ENDIF  
      *B603647,1 AME[End]

      ** Add record to appaymnt file. **
      SELECT APPAYMNT
      APPEND BLANK
      *E300296,1 M.H 11/05/95 Save the currency to the APPAYMNT file.
      *REPLACE DPAYDATE    WITH ldPayDat,;
              CFISFYEAR   WITH lcYear,;
              CFSPPRDID   WITH lcPeriod,;
              CPAYCLNO    WITH lcVendCode,;
              CPAYCOMP    WITH APVENDOR.CVENCOMP,;
              NPAYAMNT    WITH lnPayAmnt,;
              NINV1099A   WITH ln1099Amnt,;
              CBNKCODE    WITH lcBankCode,;
              CCHKACCT    WITH lcCheckCode,;
              LPAYADVAN   WITH .T.,;
              CPAYTYPE    WITH "P",;
              CPAYRECST   WITH 'O';
              CPAYMETH    WITH IIF(lnPyChMN = 1,"M",IIF(lnPyChMN = 2,'N','H')),;
              CPAYDOCNO   WITH IIF(lnPyChMN = 1,PADL(lnChkNum,FSIZE('CPAYDOCNO'),'0'),PADL(lcPayNum,FSIZE('CPAYDOCNO'),'0'))
      *B603647,1 AME[Start] 
      *REPLACE DPAYDATE   WITH ldPayDat,;
              CFISFYEAR  WITH lcYear,;
              CFSPPRDID  WITH lcPeriod,;
              CPAYCLNO   WITH lcVendCode,;
              CPAYCOMP   WITH APVENDOR.CVENCOMP,;
              NPAYAMNT   WITH lnPayAmnt,;
              NINV1099A  WITH ln1099Amnt,;
              CBNKCODE   WITH lcBankCode,;
              CCHKACCT   WITH lcCheckCode,;
              LPAYADVAN  WITH .T.,;
              CPAYTYPE   WITH "P",;
              CPAYRECST  WITH 'O';
              CPAYMETH   WITH IIF(lnPyChMN = 1,"M",IIF(lnPyChMN = 2,'N','H')),;
              CPAYDOCNO  WITH IIF(lnPyChMN = 1,PADL(lnChkNum,FSIZE('CPAYDOCNO'),'0'),PADL(lcPayNum,FSIZE('CPAYDOCNO'),'0')),;
              CCURRCODE  WITH lcCurrCode,;
              NEXRATE    WITH lnCurrRate,;
              NCURRUNIT  WITH lnCurrUnit

      REPLACE DPAYDATE   WITH ldPayDat,;
              CFISFYEAR  WITH lcYear,;
              CFSPPRDID  WITH lcPeriod,;
              CPAYCLNO   WITH lcVendCode,;
              CPAYCOMP   WITH APVENDOR.CVENCOMP,;
              NPAYAMNT   WITH lnPayAmnt,;
              NINV1099A  WITH ln1099Amnt,;
              CBNKCODE   WITH lcBankCode,;
              CCHKACCT   WITH lcCheckCode,;
              LPAYADVAN  WITH .T.,;
              CPAYTYPE   WITH "P",;
              CPAYRECST  WITH 'O';
              CPAYMETH   WITH IIF(lnPyChMN = 1,"M",IIF(lnPyChMN = 2,'N','H')),;
              CPAYDOCNO  WITH IIF(lnPyChMN = 1,PADL(lnChkNum,FSIZE('CPAYDOCNO'),'0'),PADL(lcPayNum,FSIZE('CPAYDOCNO'),'0')),;
              CCURRCODE  WITH lcCurrCode,;
              NEXRATE    WITH lnChkExRat,;
              NCURRUNIT  WITH lnChkExUnt
      *B603647,1 AME[End]              
      *E300296,1 M.H End.
              
      =gfAdd_Info()  && Add the audit information to the record.

      ** Add record to apinvhdr file. **

      SELECT APINVHDR
      APPEND BLANK

      *REPLACE COUTCOMP   WITH lcRem1,;
              COUTADDR1  WITH lcRem2,;
              COUTADDR2  WITH lcRem3,;
              COUTADDR3  WITH lcRem4,;
              NINVAMNT   WITH 0 - lnPayAmnt,;
              NINV1099A  WITH ln1099Amnt,;
              CVENDCODE  WITH lcVendCode,;
              CINVNO     WITH lcDebMemN,;
              CDIVISION  WITH lcDivCode,;
              DINVDATE   WITH ldPayDat,;
              CINVREF    WITH lcRef,;
              DINVDUDAT  WITH ldPayDat,;
              CINVSTAT   WITH "A",;
              CAPACCT    WITH IIF(EMPTY(STRTRAN(STRTRAN(lcApAcct,'-'),'0')),;
                                   SPACE(lnApsAcLen),lcAPAcct),;
              CFISFYEAR  WITH lcYear,;
              CFSPPRDID  WITH lcPeriod,;
              CINVREMIT  WITH lcInvRemit,;
              CFACCODE   WITH lcFactor,;
              CVENPMETH  WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
              CVENPRIOR  WITH APVENDOR.CVENPRIOR

      *E300296,1 M.H 11/05/95 Save the currency to the APINVHDR file.
*C100728,1 M.H Begin.
*      REPLACE COUTCOMP   WITH lcRem1,;
              COUTADDR1  WITH lcRem2,;
              COUTADDR2  WITH lcRem3,;
              COUTADDR3  WITH lcRem4,;
              NINVAMNT   WITH 0 - lnPayAmnt,;
              NINV1099A  WITH ln1099Amnt,;
              CVENDCODE  WITH lcVendCode,;
              CINVNO     WITH lcDebMemN,;
              CDIVISION  WITH lcDivCode,;
              DINVDATE   WITH ldPayDat,;
              CINVREF    WITH lcRef,;
              DINVDUDAT  WITH ldPayDat,;
              CINVSTAT   WITH "A",;
              CAPACCT    WITH IIF(EMPTY(STRTRAN(STRTRAN(lcApAcct,'-'),'0')),;
                                   SPACE(lnApsAcLen),lcAPAcct),;
              CFISFYEAR  WITH lcYear,;
              CFSPPRDID  WITH lcPeriod,;
              CINVREMIT  WITH lcInvRemit,;
              CFACCODE   WITH lcFactor,;
              CVENPMETH  WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
              CVENPRIOR  WITH APVENDOR.CVENPRIOR,;
              CCURRCODE  WITH lcCurrCode,;
              NEXRATE    WITH lnCurrRate,;
              NCURRUNIT  WITH lnCurrUnit
  
      IF TYPE('APINVHDR.DPOSTDATE') = 'D'
        
        *B803185,1 WMA 04/12/00 [Begin]
        *           This line added "Replace COUTADDR4 WITH lcRem5", to Save the 
        *			country field into COUTADDR4. 
        
        *REPLACE COUTCOMP   WITH lcRem1,;
                 COUTADDR1  WITH lcRem2,;
                 COUTADDR2  WITH lcRem3,;
                 COUTADDR3  WITH lcRem4,;
                 NINVAMNT   WITH 0 - lnPayAmnt,;
                 NINV1099A  WITH ln1099Amnt,;
                 CVENDCODE  WITH lcVendCode,;
                 CINVNO     WITH lcDebMemN,;
                 CDIVISION  WITH lcDivCode,;
                 DINVDATE   WITH ldPayDat,;
                 CINVREF    WITH lcRef,;
                 DINVDUDAT  WITH ldPayDat,;
                 CINVSTAT   WITH "A",;
                 CAPACCT    WITH IIF(EMPTY(STRTRAN(STRTRAN(lcApAcct,'-'),'0')),;
                                     SPACE(lnApsAcLen),lcAPAcct),;
                 CFISFYEAR  WITH lcYear,;
                 CFSPPRDID  WITH lcPeriod,;
                 CINVREMIT  WITH lcInvRemit,;
                 CFACCODE   WITH lcFactor,;
                 CVENPMETH  WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
                 CVENPRIOR  WITH APVENDOR.CVENPRIOR,;
                 CCURRCODE  WITH lcCurrCode,;
                 NEXRATE    WITH lnCurrRate,;
                 NCURRUNIT  WITH lnCurrUnit,;
                 DPOSTDATE  WITH ldPayDat
         
        REPLACE COUTCOMP   WITH lcRem1,;
                COUTADDR1  WITH lcRem2,;
                COUTADDR2  WITH lcRem3,;
                COUTADDR3  WITH lcRem4,;
                COUTADDR4  WITH lcRem5,;
                NINVAMNT   WITH 0 - lnPayAmnt,;
                NINV1099A  WITH ln1099Amnt,;
                CVENDCODE  WITH lcVendCode,;
                CINVNO     WITH lcDebMemN,;
                CDIVISION  WITH lcDivCode,;
                DINVDATE   WITH ldPayDat,;
                CINVREF    WITH lcRef,;
                DINVDUDAT  WITH ldPayDat,;
                CINVSTAT   WITH "A",;
                CAPACCT    WITH IIF(EMPTY(STRTRAN(STRTRAN(lcApAcct,'-'),'0')),;
                                     SPACE(lnApsAcLen),lcAPAcct),;
                CFISFYEAR  WITH lcYear,;
                CFSPPRDID  WITH lcPeriod,;
                CINVREMIT  WITH lcInvRemit,;
                CFACCODE   WITH lcFactor,;
                CVENPMETH  WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
                CVENPRIOR  WITH APVENDOR.CVENPRIOR,;
                CCURRCODE  WITH lcCurrCode,;
                NEXRATE    WITH lnCurrRate,;
                NCURRUNIT  WITH lnCurrUnit,;
                DPOSTDATE  WITH ldPayDat
      *B803185,1 WMA 04/12/00 [End]
      ELSE
        *B803185,1 WMA 04/12/00 [Begin]
        *           This line added "Replace COUTADDR4 WITH lcRem5", to Save the 
        *			country field into COUTADDR4. 

        *REPLACE COUTCOMP   WITH lcRem1,;
                 COUTADDR1  WITH lcRem2,;
                 COUTADDR2  WITH lcRem3,;
                 COUTADDR3  WITH lcRem4,;
                 NINVAMNT   WITH 0 - lnPayAmnt,;
                 NINV1099A  WITH ln1099Amnt,;
                 CVENDCODE  WITH lcVendCode,;
                 CINVNO     WITH lcDebMemN,;
                 CDIVISION  WITH lcDivCode,;
                 DINVDATE   WITH ldPayDat,;
                 CINVREF    WITH lcRef,;
                 DINVDUDAT  WITH ldPayDat,;
                 CINVSTAT   WITH "A",;
                 CAPACCT    WITH IIF(EMPTY(STRTRAN(STRTRAN(lcApAcct,'-'),'0')),;
                                   SPACE(lnApsAcLen),lcAPAcct),;
                 CFISFYEAR  WITH lcYear,;
                 CFSPPRDID  WITH lcPeriod,;
                 CINVREMIT  WITH lcInvRemit,;
                 CFACCODE   WITH lcFactor,;
                 CVENPMETH  WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
                 CVENPRIOR  WITH APVENDOR.CVENPRIOR,;
                 CCURRCODE  WITH lcCurrCode,;
                 NEXRATE    WITH lnCurrRate,;
                 NCURRUNIT  WITH lnCurrUnit
        
        REPLACE COUTCOMP   WITH lcRem1,;
                 COUTADDR1  WITH lcRem2,;
                 COUTADDR2  WITH lcRem3,;
                 COUTADDR3  WITH lcRem4,;
                 COUTADDR4  WITH lcRem5,;
                 NINVAMNT   WITH 0 - lnPayAmnt,;
                 NINV1099A  WITH ln1099Amnt,;
                 CVENDCODE  WITH lcVendCode,;
                 CINVNO     WITH lcDebMemN,;
                 CDIVISION  WITH lcDivCode,;
                 DINVDATE   WITH ldPayDat,;
                 CINVREF    WITH lcRef,;
                 DINVDUDAT  WITH ldPayDat,;
                 CINVSTAT   WITH "A",;
                 CAPACCT    WITH IIF(EMPTY(STRTRAN(STRTRAN(lcApAcct,'-'),'0')),;
                                   SPACE(lnApsAcLen),lcAPAcct),;
                 CFISFYEAR  WITH lcYear,;
                 CFSPPRDID  WITH lcPeriod,;
                 CINVREMIT  WITH lcInvRemit,;
                 CFACCODE   WITH lcFactor,;
                 CVENPMETH  WITH IIF(lnPyChMN = 1,'M',IIF(lnPyChMN = 2,'N','H')),;
                 CVENPRIOR  WITH APVENDOR.CVENPRIOR,;
                 CCURRCODE  WITH lcCurrCode,;
                 NEXRATE    WITH lnCurrRate,;
                 NCURRUNIT  WITH lnCurrUnit
        *B803185,1 WMA 04/12/00 [End]
      ENDIF
*C100728,1 M.H End.
      *E300296,1 M.H End.
      
      =gfAdd_Info()  && Add the audit information to the record.

      ** Modify record in apvendor file. **
      SELECT APVENDOR
            
      IF SEEK(lcVendCode)
        *E300296,1 M.H 11/05/95 Save the currency to the APVENDOR file.
        *REPLACE DVENLPAYD  WITH ldPayDat,;
                NVENLPAYA  WITH lnPayAmnt,;
                NVEN1099B  WITH NVEN1099B + ln1099Amnt,;
                NVENOPNDR  WITH NVENOPNDR + lnPayAmnt,;
                NVENBAL    WITH NVENBAL   - lnPayAmnt,;
                NVENCPAY   WITH NVENCPAY  + lnPayAmnt 
        *E300316,1 HISH  11/28/95. Used the variables hold signs in the equation. (Begin)
        *REPLACE DVENLPAYD  WITH ldPayDat,;
                NVENLPAYA  WITH ROUND(lnPayAmnt * lnCurrRate / lnCurrUnit,2),;
                NVEN1099B  WITH NVEN1099B + ROUND(ln1099Amnt * lnCurrRate / lnCurrUnit,2),;
                NVENOPNDR  WITH NVENOPNDR + ROUND(lnPayAmnt * lnCurrRate / lnCurrUnit,2),;
                NVENBAL    WITH NVENBAL   - ROUND(lnPayAmnt * lnCurrRate / lnCurrUnit,2),;
                NVENCPAY   WITH NVENCPAY  + ROUND(lnPayAmnt * lnCurrRate / lnCurrUnit,2) 
                 
        REPLACE DVENLPAYD  WITH ldPayDat,;
                NVENLPAYA  WITH IIF(lnCurrRate > 0 AND lnCurrUnit > 0,;
                                ROUND(lnPayAmnt &lcExSin1 lnCurrRate &lcExSin2 lnCurrUnit,2),0),;
                NVEN1099B  WITH NVEN1099B + IIF(lnCurrRate > 0 AND lnCurrUnit > 0,;
                                ROUND(ln1099Amnt &lcExSin1 lnCurrRate &lcExSin2 lnCurrUnit,2),0),;
                NVENOPNDR  WITH NVENOPNDR + IIF(lnCurrRate > 0 AND lnCurrUnit > 0,;
                                ROUND(lnPayAmnt &lcExSin1 lnCurrRate &lcExSin2 lnCurrUnit,2),0),;
                NVENBAL    WITH NVENBAL   - IIF(lnCurrRate > 0 AND lnCurrUnit > 0,;
                                ROUND(lnPayAmnt &lcExSin1 lnCurrRate &lcExSin2 lnCurrUnit,2),0),;
                NVENCPAY   WITH NVENCPAY  + IIF(lnCurrRate > 0 AND lnCurrUnit > 0,;
                                ROUND(lnPayAmnt &lcExSin1 lnCurrRate &lcExSin2 lnCurrUnit,2),0)
        *E300316,1 (End)        
        *E300296,1 M.H End.
        =gfAdd_Info()  && Add the audit information to the record.
      ENDIF  

      ** Modify record in vendor history file. **
      SELECT APVENHST
      lcSavOrder = SET('ORDER')
      IF SYS(22) <> 'VENDYEAR' 
        SET ORDER TO TAG VENDYEAR
      ENDIF

      IF SEEK(lcVendCode+lcYear)
        *E300296,1 M.H 11/05/95 Save the currency to the APVENHST file.
        *REPLACE NVNHTOTPA WITH NVNHTOTPA + lnPayAmnt
        *E300316,1 HISH  11/28/95. Used the variables hold signs in the equation. (Begin)
        *REPLACE NVNHTOTPA WITH NVNHTOTPA + ROUND(lnPayAmnt * lnCurrRate / lnCurrUnit,2)
        REPLACE NVNHTOTPA WITH NVNHTOTPA + IIF(lnCurrRate > 0 AND lnCurrUnit > 0,;
                               ROUND(lnPayAmnt &lcExSin1 lnCurrRate &lcExSin2 lnCurrUnit,2),0)
        *E300316,1 (End)
        *E300296,1 M.H End.
        IF lnPyChMN = 1        && Case calling bar is Manual check payment.
          *E300296,1 M.H 11/05/95 Save the currency to the APVENHST file.
          *REPLACE NVNHMCHKP WITH NVNHMCHKP + lnPayAmnt
          *E300316,1 HISH  11/28/95. Used the variables hold signs in the equation. (Begin)
          *REPLACE NVNHMCHKP WITH NVNHMCHKP + ROUND(lnPayAmnt * lnCurrRate / lnCurrUnit,2)
          REPLACE NVNHMCHKP WITH NVNHMCHKP + IIF(lnCurrRate > 0 AND lnCurrUnit > 0,;
                                 ROUND(lnPayAmnt &lcExSin1 lnCurrRate &lcExSin2 lnCurrUnit,2),0)
          *E300316,1 (End)
          *E300296,1 M.H End.
        ELSE
          IF lnPyChMN = 2      && Case calling bar is Non check payment.
            *E300296,1 M.H 11/05/95 Save the currency to the APVENHST file.
            *REPLACE NVNHNCHKP WITH NVNHNCHKP + lnPayAmnt
            *E300316,1 HISH  11/28/95. Used the variables hold signs in the equation. (Begin)
            *REPLACE NVNHNCHKP WITH NVNHNCHKP + ROUND(lnPayAmnt * lnCurrRate / lnCurrUnit,2)
            REPLACE NVNHNCHKP WITH NVNHNCHKP + IIF(lnCurrRate > 0 AND lnCurrUnit > 0,;
                              ROUND(lnPayAmnt &lcExSin1 lnCurrRate &lcExSin2 lnCurrUnit,2),0)
            *E300316,1 (End)
            *E300296,1 M.H End.
          ELSE                 && Case calling bar is Cash payment.
            *E300296,1 M.H 11/05/95 Save the currency to the APVENHST file.
            *REPLACE NVNHCASHP WITH NVNHCASHP + lnPayAmnt
            *E300316,1 HISH  11/28/95. Used the variables hold signs in the equation. (Begin)
            *REPLACE NVNHCASHP WITH NVNHCASHP + ROUND(lnPayAmnt * lnCurrRate / lnCurrUnit,2)
            REPLACE NVNHCASHP WITH NVNHCASHP + IIF(lnCurrRate > 0 AND lnCurrUnit > 0,;
                              ROUND(lnPayAmnt &lcExSin1 lnCurrRate &lcExSin2 lnCurrUnit,2),0)
            *E300316,1 (End)
            *E300296,1 M.H End.
          ENDIF
        ENDIF   
        IF !EMPTY(lcPeriod)
          lcfield="NVNHPAY"+ALLTRIM(STR(VAL(lcPeriod)))
          *E300296,1 M.H 11/05/95 Save the currency to the APVENHST file.
          *REPLACE &lcfield WITH &lcfield + lnPayAmnt
          *E300316,1 HISH  11/28/95. Used the variables hold signs in the equation. (Begin)
          *REPLACE &lcfield WITH &lcfield + ROUND(lnPayAmnt * lnCurrRate / lnCurrUnit,2)
          REPLACE &lcfield WITH &lcfield + IIF(lnCurrRate > 0 AND lnCurrUnit > 0,;
                                ROUND(lnPayAmnt &lcExSin1 lnCurrRate &lcExSin2 lnCurrUnit,2),0)
          *E300316,1 (End)
          *E300296,1 M.H End.
        ENDIF  
      ENDIF  
      SET ORDER TO &lcSavOrder
      =gfAdd_Info()  && Add the audit information to the record.  
      *E301077,79 AMM Open file 
      =gfOpenFile(gcDataDir+'APDIST','','SH')
      *E301077,79 AMM end
      ** Add Record to distribution file. **
      SELECT APDIST
      ** Add 1st record in distrebution file. **
      APPEND BLANK
      *E300296,1 M.H 11/05/95 Save the currency to the APDIST file.
      *REPLACE CVENDCODE   WITH lcVendCode,;
              CINVNO      WITH lcDebMemN,;
              DAPDTRDAT   WITH ldPayDat,;
              LAPDPOST    WITH .F.,;
              CAPDGLACT   WITH IIF(EMPTY(STRTRAN(STRTRAN(lcApAcct,'-'),'0')),;
                                   SPACE(lnApsAcLen),lcAPAcct),;
              NAPDAMNT    WITH lnPayAmnt,;
              CAPDACTID   WITH "A",;
              CFISFYEAR   WITH lcYear,;
              CFSPPRDID   WITH lcPeriod,;
              CAPSESSNO   WITH lcSesNum,;
              CBNKCODE    WITH lcBankCode,;
              CCHKACCT    WITH lcCheckcode,;
              CAPDTRTYP   WITH IIF(lnPyChMn = 1,'M',IIF(lnPyChMn = 2,'N','H')),;
              CAPDREF     WITH IIF(lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0'))

      *E300316,1 HISH  11/28/95. Used the variables hold signs in the currency equation. (Begin)
      REPLACE CVENDCODE   WITH lcVendCode,;
              CINVNO      WITH lcDebMemN,;
              DAPDTRDAT   WITH ldPayDat,;
              LAPDPOST    WITH .F.,;
              CAPDGLACT   WITH IIF(EMPTY(STRTRAN(STRTRAN(lcApAcct,'-'),'0')),;
                                   SPACE(lnApsAcLen),lcAPAcct),;
              NAPDAMNT    WITH lnPayAmnt,;
              CAPDACTID   WITH "A",;
              CFISFYEAR   WITH lcYear,;
              CFSPPRDID   WITH lcPeriod,;
              CAPSESSNO   WITH lcSesNum,;
              CBNKCODE    WITH lcBankCode,;
              CCHKACCT    WITH lcCheckcode,;
              CAPDTRTYP   WITH IIF(lnPyChMn = 1,'M',IIF(lnPyChMn = 2,'N','H')),;
              CAPDREF     WITH IIF(lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0')),;
              CCURRCODE   WITH lcCurrCode,;
              NEXRATE     WITH lnCurrRate,;
              NCURRUNIT   WITH lnCurrUnit,;
              NEQVAMNT    WITH IIF(lnCurrRate > 0 AND lnCurrUnit > 0,;
                               ROUND(lnPayAmnt &lcExSin1 lnCurrRate &lcExSin2 lnCurrUnit,2),0)

*B601092,1 M.H 06/03/96 Begin.
*              NEQVAMNT    WITH IIF(lnCurrRate > 0 AND lnCurrUnit > 0,;
                               ROUND(lnPayAmnt &lcExSin1 lnCurrRate &lcExSin1 lnCurrUnit,2),0)
*B601092,1 M.H 06/03/96 End.
              *NEQVAMNT    WITH ROUND(lnPayAmnt * lnCurrRate/lnCurrUnit,2)
      *E300316,1 (End)              
      *E300296,1 M.H End.
      =gfAdd_Info()  && Add the audit information to the record.        
    
      ** Add 2nd record to distrebution file. **
      APPEND BLANK
      *E300296,1 M.H 11/05/95 Save the currency to the APDIST file.
      *REPLACE CVENDCODE   WITH lcVendCode,;
              CINVNO      WITH lcDebMemN,;
              CAPDGLACT   WITH IIF(EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')),;
                               SPACE(lnApsAcLen),lcGlAcct),;
              NAPDAMNT    WITH 0-lnPayAmnt,;
              CAPDACTID   WITH "C",;
              CFISFYEAR   WITH lcYear,;
              CFSPPRDID   WITH lcPeriod,;
              CAPSESSNO   WITH lcSesNum,;
              CBNKCODE    WITH lcBankCode,;
              CCHKACCT    WITH lcCheckcode,;
              DAPDTRDAT   WITH ldPayDat,;
              LAPDPOST    WITH .F.,;
              CAPDTRTYP   WITH IIF(lnPyChMn=1,'M',IIF(lnPyChMn=2,'N','H')),;
              CAPDREF     WITH IIF(lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0'))
      
      *E300316,1 HISH  11/28/95. Used the variables hold signs in the currency equation. (Begin)
      REPLACE CVENDCODE   WITH lcVendCode,;
              CINVNO      WITH lcDebMemN,;
              CAPDGLACT   WITH IIF(EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')),;
                               SPACE(lnApsAcLen),lcGlAcct),;
              NAPDAMNT    WITH 0-lnPayAmnt,;
              CAPDACTID   WITH "C",;
              CFISFYEAR   WITH lcYear,;
              CFSPPRDID   WITH lcPeriod,;
              CAPSESSNO   WITH lcSesNum,;
              CBNKCODE    WITH lcBankCode,;
              CCHKACCT    WITH lcCheckcode,;
              DAPDTRDAT   WITH ldPayDat,;
              LAPDPOST    WITH .F.,;
              CAPDTRTYP   WITH IIF(lnPyChMn=1,'M',IIF(lnPyChMn=2,'N','H')),;
              CAPDREF     WITH IIF(lnPyChMN = 1,PADL(lnChkNum,8,'0'),PADL(lcPayNum,8,'0')),;
              CCURRCODE   WITH lcCurrCode,;
              NEXRATE     WITH lnCurrRate,;
              NCURRUNIT   WITH lnCurrUnit,;
              NEQVAMNT    WITH 0-IIF(lnCurrRate > 0 AND lnCurrUnit > 0,;
                               ROUND(lnPayAmnt &lcExSin1 lnCurrRate &lcExSin2 lnCurrUnit,2),0)
              *NEQVAMNT    WITH 0-ROUND(lnPayAmnt * lnCurrRate/lnCurrUnit,2)
      *E300316,1 (End)        
      *E300296,1 M.H End.
      
      =gfAdd_Info()  && Add the audit information to the record.
      =gfRlock('APVENDOR',.F.)
      =gfRlock('APVENHST',.F.)
    ELSE
      =gfRlock('APVENDOR',.F.)
      =gfRlock('APVENHST',.F.)
      RETURN  
    ENDIF
  ENDIF
ENDIF  
llPay =.T.    &&Return form pay not from cancel 

CLEAR READ

*!**************************************************************************
*!
*!      Function: lfVCanPay
*!
*!**************************************************************************
* Get the old next check number if there is no checks added in between adding and cancelling the check no.
*
FUNCTION lfVCanPay

SELECT APCHECKS

IF lnPyChMN = 1  && Calling from manual check.
  **if the next check no in the file is the same with next check number entered. **
  IF SEEK(lcBankCode+lcCheckCode) .AND. NCHKNXTMN = lnGnChkNm
    IF gfObj_lock(.T.) && lock the pointed record.
      REPLACE NCHKNXTMN WITH lnOChkNum
    ENDIF   
  ENDIF 
ENDIF

SELECT APPAYMNT
CLEAR READ

*!**************************************************************************
*!
*!      Function: lfObjDis
*!
*!**************************************************************************
* 
FUNCTION lfObjDis
PARAMETER llPayDone

IF llPayDone
  lcRemitTo   = laRemitTo[1,1]
  lcInvRemit  = laRemitTo[1,2]
  puRemitTo   = 1
  lnOldRemit  = 1
  lcVendCode  = SPACE(8 )
  lcVendComp  = SPACE(30)
  lcVPhone    = SPACE(16)
  lcDivision  = " "
  puDiv       = " "
  lcFactor    = " "
  lcAPAcct    = lcEmptyAcc
  lcGlAcct    = lcEmptyAcc
  lcBankCode  = " "
  lcCheckCode = " "
  lcRem1      = " "   
  lcRem2      = " "   
  lcRem3      = " "   
  lcRem4      = " "   
  lcDebMemN   = " "
  lcRef       = " "
  lnPayAmnt   = 0.00
  ldPayDat    = gdSysDate
  ln1099Amnt  = 0.00
  *E300296,1 M.H 11/05/95 When the payment done we have to empty the currency fields.
  lcCurrCode  = " "
  lnCurrRate  = 0.00
  lnCurrUnit  = 0.00
  *E300296,1 M.H End.
  STORE 'DISABLE' TO lcFactStat, lcRemitStat
ENDIF

IF !EMPTY(lcVendCode)
  STORE 'ENABLE' TO lcDataStat
  lcFactStat = IIF(lcInvRemit = 'F', 'ENABLE', 'DISABLE')
  lcObjDisp  = IIF(lnPyChMN = 3, lcDataStat, lcObjDisp)
  SHOW GET lcDivision  ENABLE  
  SHOW GET puDiv       ENABLE
  SHOW GET ibDivision  ENABLE   
  SHOW GET ibAPAcc     ENABLE 
  SHOW GET lcAPAcct    ENABLE 
  SHOW GET ibGlAcc     &lcObjDisp
  SHOW GET lcGlAcct    &lcObjDisp
  SHOW GET ibBank      ENABLE 
  SHOW GET lcBankCode  ENABLE 
  SHOW GET ibChecks    &lcObjDisp
  SHOW GET lcCheckCode &lcObjDisp
  SHOW GET ibRemitTo   &lcDataStat
  SHOW GET puRemitTo   &lcDataStat
  SHOW GET lcDebMemN   ENABLE 
  SHOW GET lcRef       ENABLE 
  SHOW GET lnPayAmnt   ENABLE 
  SHOW GET ldPayDat    ENABLE 

  *E300296,1 M.H 11/05/95 When the payment done we have to empty the currency fields.
  IF gfGetMemVar('LLMULCURR')
    IF lnPyChMN = 3
      SHOW GET lcCurrCode  ENABLE
      SHOW GET ibCurrency  ENABLE
    ENDIF
    IF lcCurrCode = gcBaseCurr .OR. !gfGetMemVar('LLEDITEXRA')
      SHOW GET lnCurrRate DISABLE
    ELSE
      SHOW GET lnCurrRate ENABLE
    ENDIF
  ENDIF
  *E300296,1 M.H End.

  IF !EMPTY(APVENDOR.CVEN1099T)
    SHOW GET ln1099Amnt  ENABLE 
  ELSE
    SHOW GET ln1099Amnt  DISABLE   
  ENDIF  

  SHOW GET pbPayInv    ENABLE   

ELSE
  STORE 'DISABLE' TO lcDataStat, lcFactStat, lcObjDisp
  SHOW GET lcVendCode   
  SHOW GET lcVendComp   
  SHOW GET lcVPhone     
  SHOW GET lcDivision  DISABLE  
  SHOW GET puDiv       DISABLE
  SHOW GET ibDivision  DISABLE  
  SHOW GET ibAPAcc     DISABLE   
  SHOW GET lcAPAcct    DISABLE  
  SHOW GET ibGlAcc     DISABLE   
  SHOW GET lcGlAcct    DISABLE   
  SHOW GET ibBank      DISABLE   
  SHOW GET lcBankCode  DISABLE   
  SHOW GET ibChecks    DISABLE   
  SHOW GET lcCheckCode DISABLE   
  SHOW GET ibRemitTo   &lcDataStat
  SHOW GET puRemitTo   &lcDataStat
  SHOW GET lcDebMemN   DISABLE   
  SHOW GET lcRef       DISABLE   
  SHOW GET lnPayAmnt   DISABLE   
  SHOW GET ldPayDat    DISABLE   
  SHOW GET ln1099Amnt  DISABLE   
  SHOW GET pbPayInv    DISABLE   
  SHOW GET lcRem1      DISABLE
  SHOW GET lcRem2      DISABLE
  SHOW GET lcRem3      DISABLE
  SHOW GET lcRem4      DISABLE
  *E300296,1 M.H 11/05/95 When the payment done we have to empty the currency fields.
  SHOW GET lcCurrCode  DISABLE
  SHOW GET ibCurrency  DISABLE
  SHOW GET lnCurrRate DISABLE
  *E300296,1 M.H End.
ENDIF
SHOW GET lcFactor &lcFactStat
SHOW GET ibFactor &lcFactStat

=lfRefresh()

*!**************************************************************************
*!
*!      Function: lfDefBank
*!
*!**************************************************************************
* Get default division, bank, and checks.
*
FUNCTION lfDefBank

*E300643,1 Change this line for the changes we have made to (gfCodDes) [Begin]
*lcDivision = gfCodDes(lcDivCode)
lcDivision = gfCodDes(lcDivCode , 'CDIVISION')
*E300643,1 Change this line for the changes we have made to (gfCodDes) [End]

puDiv      = lcDivision

SELECT APVENDOR
** set default bank and check **
IF lnPyChMN <> 3  && if not calling from cash payment
  GO TOP
  IF !EMPTY(lcVendCode) .AND. ATC("?",lcVendCode) <= 0 
    IF lcVendCode <> lcOVndcod 
      DO CASE
        CASE SEEK(lcVendCode) .AND. !EMPTY(CBNKCODE) && if found values in vendor file.
          lcBankCode = CBNKCODE
          lcCheckCode= CCHKACCT

        CASE !EMPTY(APDIV.CBNKCODE)    && if found values in division file.
          lcBankCode = APDIV.CBNKCODE
          lcCheckCode= APDIV.CCHKACCT

        OTHERWISE                      && get value from setup file.
          lcBankCode = APSETUP.CBNKCODE
          lcCheckCode= APSETUP.CCHKACCT
      ENDCASE
      
      **E300296,1 M.H 11/05/95 Default the currency by the bank currency.
      IF gfGetMemVar('LLMULCURR')
        IF !EMPTY(lcCheckCode)
          IF SEEK(lcBankCode+lcCheckCode,'APCHECKS')
            lcCurrCode = IIF(EMPTY(APCHECKS.CCURRCODE),gcBaseCurr,APCHECKS.CCURRCODE)
          ELSE
            lcCurrCode = gcBaseCurr
          ENDIF
        ELSE
          lcCurrCode = gcBaseCurr
        ENDIF
      
      *B601013,1 Remove the following part since it is duplicated after
      *B601013,1 the call to function lfDefBank(), called from lfvVendor()
      *  *E300316,1 HISH  11/28/95. Got the equation signs. (Begin)
      *  *E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)        
      *  lcExSin2 = ' '
      *  lcExSin1 = gfGetExSin(@lcExSin2,lcCurrCode)
      *  *lcExSin2 = IIF(lcExSin1 = '*','/','*')
      *  *E300316,1 (End)
      *  IF lcCurrCode <> gcBaseCurr
      *    lnCurrRate = gfChkRate('lnCurrUnit',lcCurrCode,ldPayDat,.F.,.F.)
      *    IF lnCurrRate = 0 .AND. lcCurrCode <> gcBaseCurr
      *      ** MESSAGE : " A valid ð to ð exchange rate could not "
      *      **           " be found for ð.                        "
      *      **           "                  ® Ok ¯                "
      *      =gfModalGen("QRM04157B00000","DIALOG",ALLTRIM(gcBaseCurr)+'|'+ALLTRIM(lcCurrCode)+'|'+DTOC(ldPayDat))
      *      lnCurrRate = 1
      *      lnCurrUnit = 1
      *    ENDIF
      *  ELSE
      *    lnCurrRate = 1
      *    lnCurrUnit = 1
      *  ENDIF
      *  IF lcCurrCode = gcBaseCurr .OR. !gfGetMemVar('LLEDITEXRA')
      *    SHOW GET lnCurrRate DISABLE
      *  ELSE
      *    SHOW GET lnCurrRate ENABLE
      *  ENDIF
      *ELSE
      *  lcCurrCode = gcBaseCurr
      *  lnCurrRate = 1
      *  lnCurrUnit = 1
      *B601013,1 end.
      ENDIF
      **E300296,1 M.H End.
      

      ** get account if there is a bank code and check code available. **
      
      *B606829,1 MHM 11/20/2003 the bank G/L account doesn't refresh if we change Vendor [Start]
      *IF EMPTY(STRTRAN(STRTRAN(lcGlAcct,'-'),'0')) .AND. !EMPTY(lcBankCode) .AND. !EMPTY(lcCheckCode)
      IF !EMPTY(lcBankCode) .AND. !EMPTY(lcCheckCode)
      *B606829,1 MHM 11/20/2003 the bank G/L account doesn't refresh if we change Vendor [End]
      
        IF SEEK(lcBankCode+lcCheckCode,'APCHECKS')
          lcGlAcct = IIF(EMPTY(APCHECKS.CCHKGLACC),lcEmptyAcc, APCHECKS.CCHKGLACC)
        ELSE  
          IF !EMPTY(APVENDOR.CAPACCT)
            lcGlAcct = APVENDOR.CAPACCT
          ELSE 
            IF !EMPTY(APDIV.CAPACCT)
              lcGlAcct = APDIV.CAPACCT
            ELSE
              lcGlAcct = IIF(EMPTY(APSETUP.CAPACCT),lcEmptyAcc, APSETUP.CAPACCT)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      IF !EMPTY(APVENDOR.CAPACCT)
         lcAPAcct = APVENDOR.CAPACCT
      ELSE 
        IF !EMPTY(APDIV.CAPACCT)
          lcAPAcct = APDIV.CAPACCT
        ELSE
          lcAPAcct = IIF(EMPTY(APSETUP.CAPACCT),lcEmptyAcc, APSETUP.CAPACCT)
        ENDIF
      ENDIF
    ENDIF  
  ENDIF
ELSE
  IF SEEK(lcVendCode)
    IF !EMPTY(APVENDOR.CCASHACCT)
      lcGlAcct = APVENDOR.CCASHACCT
    ELSE 
      IF !EMPTY(APDIV.CCASHACCT)
        lcGlAcct = APDIV.CCASHACCT
      ELSE
        lcGlAcct = IIF(EMPTY(APSETUP.CCASHACCT),lcEmptyAcc, APSETUP.CCASHACCT)
      ENDIF
    ENDIF
  ENDIF  
  IF SEEK(lcVendCode)
    IF !EMPTY(APVENDOR.CAPACCT)
      lcAPAcct = APVENDOR.CAPACCT
    ELSE 
      IF !EMPTY(APDIV.CAPACCT)
        lcAPAcct = APDIV.CAPACCT
      ELSE
        lcAPAcct = IIF(EMPTY(APSETUP.CAPACCT),lcEmptyAcc, APSETUP.CAPACCT)
      ENDIF
    ENDIF
  ENDIF  
ENDIF  
lcObjDisp = IIF(!EMPTY(lcBankCode), "ENABLE", "DISABLE")
SELECT APPAYMNT


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

    *E300643,1 Change this line for the changes we have made to 
    *          SYCCODES [Begin]
    *lcDivCode = gfActPop(5,17,11,50,'SYCCODES','cCode_No','cDiscrep',@lcDivision)
    lcDivCode = gfActPop(5,17,11,50,'CODES','cCode_No','cDiscrep',@lcDivision)
    *E300643,1 Change this line [End]
    
  CASE _WINDOWS

    *E300643,1 Change this lines for the changes we have made to 
    *          SYCCODES [Begin]
    *puDiv = SYCCODES.cdiscrep
    *lcDivCode = SYCCODES.cCode_No
    puDiv = CODES.cdiscrep
    lcDivCode = CODES.cCode_No
    *E300643,1 Change this lines [End]
    
    SHOW GET puDiv
    =gfUpdate()
ENDCASE

=lfRefresh()

IF _WINDOWS
  DEACTIVATE POPUP puDivision
ENDIF  


*!**************************************************************************
*!
*!      Function: lfvBnkChk
*!
*!**************************************************************************
* Validation of bank and checking account fields
*
FUNCTION lfvBnkChk

IF !lfBnkChk(@laBankObjs, lcOldVal, @llBrowse, @lcObjDisp)
  *B601013,1 If the bankcode is empty, clear the currency firlds
  IF llMultiCr .AND. EMPTY(lcBankCode)
    lcCurrCode = SPACE(5)
    lnCurrRate = 0
    STORE ' ' TO lcExSin1, lcExSin2
    SHOW GET lnCurrRate DISABLE
  ENDIF
  *B601013,1 end.
  =lfRefresh()
  SELECT APPAYMNT
  RETURN 1
ENDIF  
*B601013,1 Get multi currency values only if the current field value
*B601013,1 is changed.
IF EVALUATE(SYS(18)) <> lcOldVaL
*B601013,1 end.
  *E300296,1 M.H 11/05/95 Default the currency by the bank currency.
  IF gfGetMemVar('LLMULCURR') 
    lcCurrCode = IIF(EMPTY(APCHECKS.CCURRCODE),gcBaseCurr,APCHECKS.CCURRCODE)
    *E300316,1 HISH  11/28/95. Got the equation signs. (Begin)
    *E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)        
    lcExSin2 = ' '
    lcExSin1 = gfGetExSin(@lcExSin2,lcCurrCode)
    *lcExSin2 = IIF(lcExSin1 = '*','/','*')
    *E300316,1 (End)

    *B601013,1 Allow defining an exchange rate on the fly.
    *lnCurrRate = lnCurrRate = gfChkRate('lnCurrUnit',lcCurrCode,ldPayDat,.F.,.F.)
    lnCurrRate  = gfChkRate('lnCurrUnit',lcCurrCode,ldPayDat,;
                  .T.,gcAct_Comp, gcBaseCurr, .T.)
    *B601013,1 end.              

    *B601013,1 Display the message if adding on the fly is not allowed
    *IF lnCurrRate = 0 .AND. lcCurrCode <> gcBaseCurr
    IF lnCurrRate = 0 .AND. !llAddRate
    *B601013,1 end.
      ** MESSAGE : " A valid ð to ð exchange rate could not "
      **           " be found for ð.                        "
      **           "                  ® Ok ¯                "
      =gfModalGen("QRM04157B00000","DIALOG",;
                   ALLTRIM(lcCurrCode)+'|'+ALLTRIM(gcBaseCurr)+'|'+DTOC(ldPayDat))
      *B601013,1 Do not set the currency rate to 1. Remove next lines.
      *lnCurrRate = 1
      *lnCurrUnit = 1
      *B601013,1 end.
    ENDIF
    *B601013,1 end.
    IF lcCurrCode = gcBaseCurr .OR. !gfGetMemVar('LLEDITEXRA')
      SHOW GET lnCurrRate DISABLE
    ELSE
      SHOW GET lnCurrRate ENABLE
    ENDIF
  ENDIF  
  *E300296,1 M.H End.  
  =lfRefresh()
ENDIF  
SELECT APPAYMNT

*!**************************************************************************
*!
*!      Function: lfwGlAcct
*!
*!**************************************************************************
*
FUNCTION lfwGlAcct

lcOldAcc = EVALUATE(SYS(18))

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

llBrowse = .F.

IF EMPTY(STRTRAN(STRTRAN(&lcAcct,'-'),'0'))
  &lcAcct   = lcOldAcc
ENDIF

=lfRefresh()
 
*!**************************************************************************
*!
*!      Function: lfvRemit
*!
*!**************************************************************************
*
FUNCTION lfvRemit
PARAMETERS llJustShow

** Default factor for the vendor
lcFactor   = IIF(EMPTY(lcFactor), APVENDOR.cFacCode, lcFactor)
*B803185,1 WMA 04/12/00 Send lcRem5 to Function lfRemit() For Get Country [Begin]
*lcInvRemit = lfRemit(lcInvRemit, !llJustShow, lcVendCode, lcFactor, @lcRemitStat,;
               'lcRem1', 'lcRem2', 'lcRem3', 'lcRem4',;
                13, 40, 'laRemitTo', @lcRemitTo, lnRemitLen)
lcInvRemit = lfRemit(lcInvRemit, !llJustShow, lcVendCode, lcFactor, @lcRemitStat,;
               'lcRem1', 'lcRem2', 'lcRem3', 'lcRem4',;
                13, 40, 'laRemitTo', @lcRemitTo, lnRemitLen, 'lcRem5')
*B803185,1 WMA 04/12/00 [End]
lcFactor   = IIF(lcInvRemit = 'F', lcFactor, SPACE(6))

lnOldRemit = puRemitTo
lcFactStat  = IIF(lcInvRemit = 'F', 'ENABLE', 'DISABLE')
SHOW GET lcFactor &lcFactStat
SHOW GET ibFactor &lcFactStat
=lfRefresh()

SELECT APPAYMNT

*!**************************************************************************
*!
*!      Function: lfvFactor
*!
*!**************************************************************************
* Valid function for get field lcFactor
*
FUNCTION lfvFactor
IF llBrowse .OR. !EMPTY(lcFactor) 
  *E301077,79 AMM Open file
  =gfOpenFile(gcSysHome+'SYCFACT','CFACCODE','SH')
  *E301077,79 AMM end
  IF lfGetFac(lcOldVal, llBrowse)
    lcRem1      = SYCFACT.cFacComp

    *B803185,1 WMA 04/12/00 Using gfGetAdr() Instead of lfGetAdr()  [Begin] 
    *B803185,1              To Get Address 3,4,5 Separated With Comma And Country   
	*lcRem2      = SYCFACT.cAddress1
    *lcRem3      = SYCFACT.cAddress2
    *lcRem4      = lfGetAdr('SYCFACT', 'CFACCODE')
  
    laCompAdr = ""
    =gfGetAdr('SYCFACT',"","","",@laCompAdr)
    lcRem2      = laCompAdr[1]
    lcRem3      = laCompAdr[2]
    lcRem4      = laCompAdr[3]
    lcRem5      = laCompAdr[4] 
    *B803185,1 WMA 04/12/00 [End]
  ENDIF  
ELSE
  *B803185,1 WMA 04/12/00 Initialize lcRem5  variable.[Begin] 
  *STORE SPACE(40) TO lcRem1, lcRem2, lcRem3, lcRem4 
  STORE SPACE(40) TO lcRem1, lcRem2, lcRem3, lcRem4 , lcRem5 
  *B803185,1 WMA 04/12/00 [End]
ENDIF   
SHOW GET lcRem1
SHOW GET lcRem2 
SHOW GET lcRem3 
SHOW GET lcRem4 
llBrowse = .F.


*!**************************************************************************
*!
*!      Function: lfwOldValue
*!
*!**************************************************************************
*
FUNCTION lfwOldValue
lcOldVal = EVALUATE(SYS(18))
 
*!**************************************************************************
*!
*!      Function: lfwVend
*!
*!**************************************************************************
* Save old vendor code.
*
FUNCTION lfwVend

lcOVndVal  = EVAL(SYS(18))
lcOVPhone  = lcVPhone  
lcOVndcmp  = lcVendComp
lcOVndcod  = lcVendCode

*!**************************************************************************
*!
*!      Function: lfvVendor
*!
*!**************************************************************************
*
FUNCTION lfvVendor

IF lfSekVnd('APVENDOR',lcOVndVal,llBrowse)
  IF APVENDOR.CVENPRIOR = '0'
    ** MESSAGE: " Vendor XXXXXX has payment priority 0."
    **          " This vendor in on hold.              " 
    ** Choices: "                ® OK ¯                " 
    =gfModalGen("TRM04060B00000","DIALOG",ALLTRIM(lcVendCode))
    lcVPhone   = lcOVPhone
    lcVendComp = lcOVndcmp
    lcVendCode = lcOVndcod
  ELSE
    lcDivCode  = APVENDOR.CDIVISION  
  ENDIF  

  IF EVAL(VARREAD()) <> lcOVndVal  &&If the vendor changed.
    =lfDefBank()
    =lfObjDis(.F.)
    =lfvRemit(.T.)

    *E300643,1 Change this line for the changes we have made 
    *          to (gfCodDes) [Begin]
    *lcDivision = gfCodDes(lcDivCode)  
    lcDivision = gfCodDes(lcDivCode , 'CDIVISION')  
    *E300643,1 Change this line [End]
    
    puDiv      = lcDivision
    _CUROBJ    = OBJNUM(ibDivision)
    lcDebMemN   = " "
    lcRef       = " "
    lnPayAmnt   = 0.00
    ldPayDat    = gdSysDate
    ln1099Amnt  = 0.00
  ENDIF  

  *E300296,1 M.H 11/05/95 Default by the vendor currency if its a cash payment.
  IF gfGetMemVar('LLMULCURR')
    IF lnPyChMN = 3
      lcCurrCode = IIF(EMPTY(APVENDOR.CCURRCODE),gcBaseCurr,APVENDOR.CCURRCODE)
      *B601013,1 Add an ENDIF here. Get the exchange rate in all cases,  
    ENDIF  

    *E300316,1 HISH  11/28/95. Got the equation signs. (Begin)
    *E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)              
    lcExSin2 = ' ' 
    lcExSin1 = gfGetExSin(@lcExSin2,lcCurrCode)
    *lcExSin2 = IIF(lcExSin1 = '*','/','*')
    *E300316,1 (End)
    
    *B601013,1 Allow defining an exchange rate on the fly.
    *lnCurrRate = gfChkRate('lnCurrUnit',lcCurrCode,ldPayDat,.F.,.F.)
    lnCurrRate  = gfChkRate('lnCurrUnit',lcCurrCode,ldPayDat,;
                  .T.,gcAct_Comp, gcBaseCurr, .T.)
    *B601013,1 end.              

    *B601013,1 Display the message if adding on the fly is not allowed
    *IF lnCurrRate = 0 .AND. lcCurrCode <> gcBaseCurr
    IF lnCurrRate = 0 .AND. !llAddRate
    *B601013,1 end.
      ** MESSAGE : " A valid ð to ð exchange rate could not "
      **           " be found for ð.                        "
      **           "                  ® Ok ¯                "
      =gfModalGen("QRM04157B00000","DIALOG",;
                  ALLTRIM(lcCurrCode)+'|'+ALLTRIM(gcBaseCurr)+'|'+DTOC(ldPayDat))
      *B601013,1 Do not set the currency rate to 1. Remove next lines.
      *lnCurrRate = 1
      *lnCurrUnit = 1
      *B601013,1 end.
    ENDIF
    IF lcCurrCode = gcBaseCurr .OR. !gfGetMemVar('LLEDITEXRA')
      SHOW GET lnCurrRate DISABLE
    ELSE
      SHOW GET lnCurrRate ENABLE
    ENDIF  
    SHOW GET lcCurrCode
  ENDIF
  *E300296,1 M.H End.
  SHOW GET lcVendCode
  SHOW GET lcVPhone
  SHOW GET lcVendComp
  SHOW GET lcDebMemN  
  SHOW GET lcRef      
  SHOW GET lnPayAmnt  
  SHOW GET ldPayDat   
  SHOW GET ln1099Amnt 
  SELECT APPAYMNT
ENDIF
llBrowse = .F.

*!**************************************************************************
*!
*!      FUNCTION: lfwCurr
*!
*!**************************************************************************
*E300296,1 M.H 11/05/95 Add the currency to the AP module.
*
FUNCTION lfwCurr

lcOldCurr = lcCurrCode

*!**************************************************************************
*!
*!      FUNCTION: lfvCurr
*!
*!**************************************************************************
*E300296,1 M.H 11/05/95 Add the currency to the AP module.
*
FUNCTION lfvCurr

IF llBrowse .OR. !SEEK(lcCurrCode,'SYCCURR') .OR. ATC("?",lcCurrCode) > 0 .AND. LASTKEY() = 13
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
  ENDIF
ENDIF

*E300316,1 HISH  11/28/95. Got the equation signs. (Begin)
*E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)              
lcExSin2 = ' '
lcExSin1 = gfGetExSin(@lcExSin2,lcCurrCode)
*lcExSin2 = IIF(lcExSin1 = '*','/','*')
*E300316,1 (End)

IF lcCurrCode <> lcOldCurr
  *B601013,1 Remove the following line
  *IF gfGetMemVar('LLMULCURR') .AND. lcCurrCode <> gcBaseCurr
  *B601013,1 end.

    *B601013,1 Allow adding an exchange rate on the fly.
    *lnCurrRate = gfChkRate('lnCurrUnit',lcCurrCode,ldPayDat,.F.,.F.)
    lnCurrRate = gfChkRate('lnCurrUnit',lcCurrCode,ldPayDat,;
                     .T.,gcAct_Comp, gcBaseCurr, .T.)
    *B601013,1 end.
    *B601013,1 Display te message if adding on the fly is not allowed
    *IF lnCurrRate = 0 
    IF lnCurrRate = 0 .AND. !llAddRate
    *B601013,1 end.
      ** MESSAGE : " A valid ð to ð exchange rate could not "
      **           " be found for ð.                        "
      **           "                  ® Ok ¯                "
      =gfModalGen("QRM04157B00000","DIALOG",;
        ALLTRIM(lcCurrCode)+'|'+ALLTRIM(gcBaseCurr)+'|'+DTOC(ldPayDat))
      *B601013,1 Do not set rate to 1
      *lnCurrRate = 1
      *lnCurrUnit = 1
      *B601013,1 end.
    ENDIF
    IF lcCurrCode = gcBaseCurr .OR. !gfGetMemVar('LLEDITEXRA')
      SHOW GET lnCurrRate DISABLE
    ELSE
      SHOW GET lnCurrRate ENABLE
    ENDIF  
  *B601013,1 Remove the following lines
  *ELSE
  *  lcCurrCode  = gcBaseCurr
  *  lnCurrRate  = 1
  *  lnCurrUnit  = 1
  *  SHOW GET lnCurrRate DISABLE
  *ENDIF
  *B601013,1 end.
ENDIF

*B601013,1 Reset browsing button variable
llBrowse  = .F.
*B601013,1 end.
=lfRefresh()

SELECT APINVHDR

*!**************************************************************************
*!
*!      FUNCTION: lfwCurrRate
*!
*!**************************************************************************
*E300296,1 M.H 11/05/95 Add the currency to the AP module.
*
FUNCTION lfwCurrRate

lnOldRate = lnCurrRate

*!**************************************************************************
*!
*!      FUNCTION: lfvCurrRate
*!
*!**************************************************************************
*E300296,1 M.H 11/05/95 Add the currency to the AP module.
*
FUNCTION lfvCurrRate
IF lnCurrRate <> lnOldRate .AND. lnCurrRate < 0
  *B601013,1 Add an appropriate message.
  *B601013,1 if the entered value is not greater than zero,
  *B601013,1 Message :  "   ð should be greater than ð.   "
  *B601013,1                  ®   OK   ¯
  =gfModalGen("TRM04072B00000","DIALOG", 'The exchange rate|zero') > 0
  lnCurrRate = lnOldRate
ENDIF
SHOW GET lnCurrRate
=lfRefresh()
