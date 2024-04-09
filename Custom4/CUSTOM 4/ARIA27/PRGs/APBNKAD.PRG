*:************************************************************************
*:
*: Procedure file: APBNKADJ.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Hisham Ramsis Philips
*:  Last modified: 05/14/95
*:
*:  Procs & Fncts: lfvAdjTyp
*:               : lfvClose
*:               : lfwAdjAmnt
*:               : lfvAdjAmnt
*:               : lfwAdjDat
*:               : lfvAdjDat
*:               : lfvDstled
*:               : lfvCrtAdj
*:               : lfVCanAdj
*:               : lfShow
*:               : lfDefBank
*:               : lfwOldVals
*:               : lfvBank
*:               : lfvChecks
*:               : lfwGlAcct
*:               : lfvGlAcct
*:               :
*:
*:      Documented 00/00/1994
*:************************************************************************
*B500708,1 MAN 05/14/95 Updating field APPAYMNT.CPAYRECST with "O" pen
*B500708,1              to fix the bug of not displaying the Bank Adj.
*B500708,1              in the bank reconciliation
*E300296,4 RENEE 10/23/95 Add currency exchange checks (multi currency)
*E300316,1 HISH  12/05/95. Used global function gfGetExSin to get currency 
*E300316,1                 equation signs.
*E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin.
*B601166,1 Hesham El-Sheltawi ON 07/22/96
*B601166,1 donot clear the bank code ,check code,adj. type,adj. date
*E300663,1  HS 04/23/97 Change the calling of the function [gfSequence]
*E300663,1              for the changes we have made to that function.
*E300683,1 AHMED 06/04/97 Add screens directory path to the calling of SPRS
*E300921,1 TAK 07/12/98 Added changes due to Point of Sale module (cash register).
*E300921,1              Added a bank type (S)afe with no checcking account.
*E300921,1              Changed the path of the screen due to sharing.
*E300921,1              Added a new code for bank adjustment reason.
*E301077,71 AMM 02/18/99 Reduce opened files 
*B802647,1  AKA  09/27/99 Refresh Adj. account name as long as the adj. reason code 
*B802647,1    			  is changed.  [This bug fixed in screen]
*C102056,1 ABD 12/03/2000 Make change on the 'Cash Register adjustment' screen
*C102056,1 ABD            Upon use in the PS Module.
*N000232,1 HS  04/12/2001 If this is a Cash Register Adjustment check if NC module
*N000232,1                is installed and the company setup "System Type" is set
*N000232,1                to "Point Of Sale", and if so add a record in the
*N000232,1                EDITrans to be used to send this Cash Register Adjustment.
*B604354,1 ABD 05/06/20001 Change all filed that have FoxFont font to be
*B604354,1                 Ms Sans Serif to write arabic in reference filed. 
*B604850,1 SSE 09/16/2001 Fix bug of displaying the currency code field blank.
*B608292,1 NNA 09/26/2007 Fix Bug that When keying a Bank adjustment for a bank code which is linked to the base currency, 
*B608292,1 NNA            the user is allowed to key an exchange rate. 
*:************************************************************************
 
PRIVATE laBankObjs 
DECLARE laAdjTyp[2,2], laBankObjs[3,3]       
** laAdjTyp    Array to hold Popups ,Select.
** laBankObjs  Array to hold bank objects'names for validations      
** lcSesNum    Variable to hold session number.
** lcSeqNum    Variable to hold sequence number.
** lcOldVal    Variable to hold old bank or checking account code.
** lcPeriod    Variable to hold First period. 
** lcFisPrd    Variable to hold First period.
** lcYear      Variable to hold first year.
** lcFisYer    Variable to hold first year.
** lcTsession  Variable to hold word Session.
** lcObjDisp   Variable to hold objects display status
** lcActDisp   Variable to hold account objects display status
** lcRef       Variable to hold
** lcDefChAcc  Variable to hold
** lcBankCode  Variable to hold bank code.
** lcCheckcode Variable to hold check code. 
*E300296,4 Add currency code and exchange rate fields
*E300296,4 lcCurrency  Variable to hold currency code.
*E300296,4 lcRateDisp  Exchange rate display status.
*E300296,4 lnCurrUnit  Variable to hold currency unit.
*E300296,4 lnExRate    Variable to hold exchange rate.
*E300296,4 end.
** lcAdjTyp    Variable to hold adj. type.
** lcAdjust    Variable to hold adj. code.
**
** laCtrStat   To disable the browse pad in the menu
**
*E300316,1 HISH  12/05/95. Added currency equation sign variables. (Begin)
** lcExSin1             Variable to hold the first sign in the equation.
** lcExSin2             Variable to hold the second sign in the equation.
*E300316,1 (End)

** ldOAdjDat   Variable to hold old Adjustment date. 
** ldAdjDat    Variable to hold Adjustment Date.
**
** llBrowse    Variable to hold left mouse clicked or not. 
** llNoContrl  no control panel (AP.PRG)


laAdjTyp[1,1] = "Deposit"
laAdjTyp[2,1] = "Charge"
lcAdjTyp      = laAdjTyp[1,1] 
lcAdjust      = laAdjTyp[1,2] 

STORE 0.00        TO lnAdjAmnt , lnOAdjAmnt
STORE 1           TO ibAdjTyp  , cbDstled   , lnAdjTyp, lnOldCbDst
STORE lcEmptyAcc  TO lcGlChkAct, lcGlDstAct , lcOldacc
STORE SCHEME(1,2) TO lcColPair , lcColor
STORE SPACE(8)    TO lcBankCode
STORE SPACE(12)   TO lcCheckcode

*E300296,4 Add currency code and exchange rate fields
lcCurrency  = ' '
STORE 0 TO lnExRate, lnCurrUnit
*E300296,4 end.

*E300921,1 (Start) Define variables for bank type and safe users no.
*E300921,1 Added a new code for bank adjustment reason.
DIMENSION  laBAdj[1]
laBAdj = ''
lnBAdj = 1
lcBnkTyp = 'B'
llNewSafe= .F.
lnSfUser = 1
*E301077,71 AMM Move this part after calling gfSetup()
*--Check the safe users and user safes.
*=gfOpenFile(gcSysHome+'sycinst','','SH')
*GO TOP IN SYUUser
*--If there is a users defined in the system and login required.
*llUsersExt = !EOF('SYUUser') AND SYCINST.lInsLogRq   
*E301077,71 AMM end
*E300921,1 (End).


STORE 'DISABLE'   TO laCtrStat
STORE {}          TO ldOAdjDat
STORE gdSysDate   TO ldAdjDat
STORE .F.         TO llBrowse
STORE .T.         TO llNoContrl
STORE ' '         TO lcSesNum  , lcSeqNum   , lcOldVal   , lcTsession ,;
                     lcPeriod  , lcFisPrd   , lcYear     , lcFisYer   ,;
                     lcObjDisp  , lcActDisp ,lcRef      , lcDefChAcc
*C102056,1 ABD Fix Bug variable not found. [Begin]
lcSfUserStr = ''
*C102056,1 ABD [Begin]

IF !gfSetup()
  RETURN
ENDIF

*C102056,1 ABD -Make change on the 'Cash Register adjustment screen
*C102056,1 ABD -Upon use in the PS Module. [Begin]

*- lcBnkRgstr  :- Variable hold Bank if call from AP module or Register if 
*--            :- Call from PS module.
*- If call from PS module.
IF gcWinAppl = "PS"
  llFromPs   = .T.
  lcWindTitl = "Cash Register Adjustment"
  lcBnkRgstr = "Register"
ELSE && call from AP Module.
  lcBnkRgstr = "Bank"
  llFromPS   = .F.  
ENDIF

*N000232,1 HS 04/12/2001 Add these lines to add some new flags [Begin]
*-- Flag to hold if the NC Module is installed for the active company or not
llNCInst = "NC" $ gcComp_Mdl

*-- Flag to know if the company setup "System Type" is set to "Point Of Sale" or not
llSite   = (gfGetMemVar("M_SYSTYPE ") = "P")

*-- Flag to hold if the EDITrans file were opened by this program or not
llOpEDITrn = .F.

*-- Flag to hold if the Codes file were opened by this program or not
llOpCodes  = .F.

*-- Flag to hold if the EDIAcPrt file were opened by this program or not
llOpAcPrt  = .F.

*-- Flag to hold if the EDIPD file were opened by this program or not
llOpEdiPd  = .F.

*N000232,1 HS 04/12/2001 Add these lines to add some new flags [End]

*-- change width of field cPayComp to carry 60 Chr not 30 Chr. 
lcRef = SPACE (60)
*-- Get the Cash Drower escape seq.
lcCashSeq = gfGetMemVar('M_CASHSEQ')
*-- Get the Cash com. port.
lcCashCom = gfGetMemVar('M_CASHCOM')
*C102056,1 ABD - [End]

*E300921,1 (Start) Check the AP intalled and open the files needed if 
*E300921,1 AP was installed else disable link to Gl.
llApLink = (OCCURS('AP',gcCmpModules)<>0)
*E301077,71 AMM Don't open here, just Open when needed.
*IF llApLink
  *=gfOpenFile(gcDataDir+'APSETUP','','SH')
  *=gfOpenFile(gcDataDir+'APDIST','PAYMNTS','SH')
*ELSE
IF !llApLink
*E301077,71 AMM end
  cbDstled = 0
  lnOldCbDst = 0
ENDIF
*E300921,1 (End).
*E301077,71 AMM This part moved from above.
*--Check the safe users and user safes.
llOpInst = gfOpenFile(gcSysHome+'sycinst','','SH')
*E301077,71 AMM Make sure that SUUSER is opened
*C102056,1 ABD Fix bug that table not ordered. [Begin]
*=gfOpenFile(gcSysHome+'SYUUser','','SH')
=gfOpenFile(gcSysHome+'SYUUser','CUSER_ID','SH')
*C102056,1 ABD [End]
*--If there is a users defined in the system and login required.
llUsersExt = !EOF('SYUUser') AND SYCINST.lInsLogRq   
*E301077,71 AMM Close file, no need for it.
IF llOpInst
  =gfCloseFile('sycinst')
ENDIF
*E301077,71 AMM end

*N000232,1 HS 04/12/2001 Add these lines to close the EDITrans, Codes and EDIAcPrt files
*N000232,1               (if they were opened by this program) [Begin].
IF llOpEDITrn
  =gfCloseFile('EDITrans')
ENDIF

IF llOpCodes
  =gfCloseFile('Codes')
ENDIF

IF llOpAcPrt
  =gfCloseFile('EDIAcPrt')
ENDIF

IF llOpEdiPd
 =gfCloseFile('EDIPD')
ENDIF
*N000232,1 HS 04/12/2001 Add these lines to close [End]

*** Initialize the "Codes Information" array.
DIMENSION laCodInfo [1,10]
laCodInfo = SPACE(0)
*** Fill the "Codes Information" array.
*C102056,1 ABD Define variable to hold bank Adj code or register Adj code. [Begin]
*laCodInfo[1,01] = "CBNKADJ"
lcCodPopup = IIF(llFromPS,"CREGADJRES","CBNKADJ")
laCodInfo[1,01] = lcCodPopup
*C102056,1 ABD [End]
laCodInfo[1,02] = "laBAdj"
laCodInfo[1,03] = "lnBAdj"
laCodInfo[1,04] = ""
laCodInfo[1,05] = .F.
laCodInfo[1,06] = .F.
laCodInfo[1,07] = ""    
laCodInfo[1,08] = ""
laCodInfo[1,09] = ""
laCodInfo[1,10] = ""
*C102056,1 ABD Get the  bank Adj code or register Adj code. [Begin]
*= gfwCodePop( @laCodInfo, "CBNKADJ" ,'D' )
= gfwCodePop( @laCodInfo, lcCodPopup ,'D' )
*C102056,1 ABD [End]
DECLARE laTrmRltFd[1,2]
*C102056,1 ABD Get the Gl bank Adj code or GL register Adj code. [Begin]
*laTrmRltFd[1,1] = 'CADJACCT  '
*=gfRltFld(laBAdj[1,2],@laTrmRltFd,"CBNKADJ")
laTrmRltFd[1,1] = IIF(llFromPS , 'CGLACCT   ','CADJACCT  ')
laTrmRltFd[1,2] = 'lcGlDstAct'
=gfRltFld(laBAdj[1,2],@laTrmRltFd,lcCodPopup)
*C102056,1 ABD [End]

lcGlDstAct = IIF(EMPTY(lcGlDstAct),lcEmptyAcc,lcGlDstAct)


*E300296,4 Enable rate exchange field if editing exchange rate per
*E300296,4 transaction is allowed from the company setup.
IF gfGetMemVar('LLMulCurr')
  llEditEx    = gfGetMemVar('LLEDITEXRA')
  llMultiCr   = .T.
ELSE
  STORE .F. TO llEditEx, llMultiCr
ENDIF  
*E300296,4 
lcRateDisp  = IIF(llEditEx .AND. !EMPTY(lcCheckCode),;
                  'ENABLE', 'DISABLE')
*E300296,4 end.
*E300316,1 HISH  12/05/95. Initialize variables hold currency equation signs. (Begin)
STORE ' ' TO lcExSin1, lcExSin2
*E300316,1 (End)
*** Get names to Brawse and Read window ***
IF !WEXIST(gcBaseWind)
  STORE IIF(lfDefBank(), "ENABLE", "DISABLE") TO lcObjDisp, lcActDisp
  *** Get the active session number. ***
  IF EMPTY(lcSesNum)

    *E300663,1 Change this line for the changes we have made to 
    *          (gfSequence) [Begin]
    *lcSesNum    = PADL(gfSequence("APSESS",1,gcAct_Comp),8,'0')
    lcSesNum    = gfSequence('CAPSESSNO')
    *E300663,1 Change this line [End]
    
  ENDIF  

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
  laBankObjs[2,2] = 'lcCheckCode'    && Checking account 
  laBankObjs[3,1] = 'ibchkAcc'       && G/L account invisible button  
  laBankObjs[3,2] = 'lcGlChkAct'     && G/L account
ELSE
  cbDstled  = lnOldCbDst
ENDIF
SELECT APPAYMNT

*E300683,1 Call *.SPR from screens directory
* DO APBNKAD.SPR 
*E300921,1 Change path due to sharing of this screen.
*DO (gcScrDir + gcWinAppl + '\APBNKAD.SPR')
DO (gcScrDir + 'APBNKAD.SPR')
*E300921,1 end          
*E300683,1 end          

lnOldCbDst = cbDstled

*!**************************************************************************
*!
*!      Function: lfvAdjTyp
*!
*!**************************************************************************
* Activate pay method popup.
*
FUNCTION lfvAdjTyp

DO CASE
  CASE _DOS
    *** activate pop up and get payment method codes. ***
    lcAdjust = gfActPop(4,20,8,34,'laAdjTyp',2,1,@lcAdjTyp)

  CASE _WINDOWS
    *** get payment method code. ***
    lcAdjTyp = laAdjTyp[lnAdjTyp,1]
ENDCASE

=lfRefresh()

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
*!      Function: lfwAdjAmnt
*!
*!**************************************************************************
* 
FUNCTION lfwAdjAmnt
*E300296,4 Get the content of the current object to enable the usage
*E300296,4 of this function for lnExRate field as well as lnAdjamnt field.
*lnOAdjAmnt = lnAdjAmnt
lnOAdjAmnt = EVALUATE(SYS(18))
*E300296,4 end.

*!**************************************************************************
*!
*!      Function: lfvAdjAmnt
*!
*!**************************************************************************
* 
FUNCTION lfvAdjAmnt

IF lnAdjAmnt <= 0
  ***  Message: " The adjustment amount should be greatre than zero"
  ***  Choice : " < Ok > "
  =gfModalGen("TRM04105B00000","DIALOG")
  lnAdjAmnt = lnOAdjAmnt
ENDIF  

*!**************************************************************************
*!
*!      Function: lfwAdjDat
*!
*!**************************************************************************
*Save old adjustment date.
*
FUNCTION lfwAdjDat

ldOAdjDat = ldAdjDat 

*!**************************************************************************
*!
*!      Function: lfvAdjDat
*!
*!**************************************************************************
* Valdation of the adjustment date.
*
FUNCTION lfvAdjDat

***Get the year and period from apsetup.***
IF lfVDtMsg(gcPrnt_Cmp ,@lcFisPrd,@lcFisYer)
  lcPeriod = lcFisPrd
  lcYear   = lcFisYer
  *E300296,4 If the date is valid, get the exchange rate for the
  *E300296,4 curreny of the checking account.
  *E300296,4 If muti currency,
  IF llMultiCr
    *E300296,4 If there is no exchange rate, or,
    *E300296,4 if the date is changed,
    *E300296,4 get a new exchange rate.
    IF lnExRate = 0 .OR. ldAdjDat <> ldOAdjDat
      IF lcCurrency = gcBaseCurr
        STORE 1 TO lnExRate, lnCurrUnit
      ELSE
        lnExRate  = gfChkRate('lnCurrUnit', lcCurrency, ldAdjDat, .T.)
      ENDIF
      *E300316,1 HISH  12/05/95. Got the equation signs. (Begin)
      *E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)     
      lcExSin2 = ' '
      lcExSin1 = gfGetExSin(@lcExSin2,lcCurrency)
      *lcExSin2 = IIF(lcExSin1 = '*','/','*')
      *E300316,1 (End)

      SHOW GET lnExRate             
    ENDIF     && END IF lnExRate = 0 .OR. ldAdjDat <> ldOAdjDat
  ENDIF     && END IF llMultiCr
  *E300296,4 end.
ELSE
  ldAdjDat = ldOAdjDat  
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvDstled
*!
*!**************************************************************************
*
FUNCTION lfvDstled

IF cbDstled = 1
  SHOW GET lcGlDstAct ENABLE
  SHOW GET lcGlChkAct ENABLE
  SHOW GET ibDstAcc   ENABLE
  SHOW GET ibchkAcc   ENABLE
  lcActDisp = "ENABLE" 
ELSE 
  *YMO
  *lcGlChkAct  = lcDefChAcc
  *lcGlDstAct  = lcEmptyAcc
  SHOW GET lcGlDstAct  DISABLE 
  SHOW GET lcGlChkAct  DISABLE
  SHOW GET ibDstAcc    DISABLE
  SHOW GET ibchkAcc    DISABLE
  lcActDisp = "DISABLE" 
ENDIF
=lfrefresh()

*!**************************************************************************
*!
*!      Function: lfvCrtAdj
*!
*!**************************************************************************
*
FUNCTION lfvCrtAdj

*C102056,1 ABD - Don't cheak all this data if you come from PS Module. [Begin]
IF !llFromPS
*C102056,1 ABD [End]
  IF cbDstLed=1
    IF(EMPTY(lcBankCode) .OR. EMPTY(lcCheckCode);
      .OR. EMPTY(STRTRAN(STRTRAN(lcGlChkAct,'-'),'0')) ;
      .OR. EMPTY(STRTRAN(STRTRAN(lcGlDstAct,'-'),'0')))
      ** MESSAGE : " You have to enter the bank code,the     "
      **           " GL checking account,and the GL disatribution account."
      **           "                    ® Ok ¯               "
      =gfModalGen("TRM04106B00000","DIALOG")
      RETURN
    ENDIF  
  ELSE
    IF(EMPTY(lcBankCode) .OR. EMPTY(lcCheckCode))
      ** MESSAGE : " You have to enter the bank code,and the     "
      **           " checking account"
      **           "                    ® Ok ¯               "
      =gfModalGen("TRM04107B00000","DIALOG")
      RETURN
    ENDIF
  ENDIF
  *C102056,1 ABD - . [Begin]  
ELSE
  *-- Cheak on the Bank Code and Check Code only if you come from PS Module.
  IF  EMPTY(lcBankCode) .OR. EMPTY(lcCheckCode)
    *-- Text Message :- You have to enter the ð account.
    *-- Text Number  :- 04020.
    *-- Text Button  :- Ok
    *-- Button Number:- 00000
    =gfModalGen("TRM04020B00000","DIALOG",'bank code,and the checking')
    RETURN
  ENDIF
ENDIF
*C102056,1 ABD [End]
IF lnAdjAmnt <= 0
  ***  Message: " The adjustment amount should be greater than zero"
  ***  Choice : " < Ok > "
  =gfModalGen("TRM04105B00000","DIALOG")
  lnAdjAmnt = lnOAdjAmnt
  _CUROBJ = OBJNUM(lnAdjAmnt)
  RETURN
ENDIF

IF lfVDtMsg(gcPrnt_Cmp ,@lcFisPrd,@lcFisYer,ldAdjDat)
  lcPeriod = lcFisPrd
  lcYear   = lcFisYer
ELSE
  ldAdjDat = ldOAdjDat
  _CUROBJ = OBJNUM(ldAdjDat)  
  RETURN
ENDIF  

*E300296,4 Check if there is a valid exchange rate before creating
*E300296,4 an adjustment. If not, get a valid exchange rate.
IF llMultiCr
  *E300296,4 IF exchange rate = 0, call the global validation,
  *E300296,4 display a message (4th parameter is .T.) 
  IF lnExRate = 0
    IF lcCurrency = gcBaseCurr
      STORE 1 TO lnExRate, lnCurrUnit
    ELSE
      lnExRate  = gfChkRate('lnCurrUnit', lcCurrency, ldAdjDat, .T.)
      *E300296,4 If no exchange rate is entered, return.
      IF lnExRate = 0 
        RETURN 
      ENDIF  
    ENDIF
  ENDIF
ELSE
  lcCurrency = gcBAseCurr
  STORE 1 TO lnExRate, lnCurrUnit
ENDIF 
*E300316,1 HISH  12/05/95. Got the equation signs. (Begin)
*E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)     
lcExSin2 = ' '
lcExSin1 = gfGetExSin(@lcExSin2,lcCurrency)
*lcExSin2 = IIF(lcExSin1 = '*','/','*')
*E300316,1 (End)

*E300296,4 end.

*C102056,1 ABD- Send the proper escape sequence to the cash drawer in order to 
*C102056,1 ABD- Open it automatically. [Begin]

*--- Open Cash Drawer
*--- lcCashSeq == Drawer Seq.
*--- lcCashCom == Drawer Com.
lcOldPrn = SET('PRINTER')
SET PRINTER TO &lcCashCom
SET DEVICE TO PRINTER
IF TYPE('EVAL(lcCashSeq)') <> 'U'
  @ 0,0 SAY &lcCashSeq
ENDIF
SET PRINTER TO
SET DEVICE TO SCREEN
*---  Restore old printer setting
*C102056,1 ABD- [End]

*E300663,1 Change this line for the changes we have 
*          made to (gfSequence) [Begin]
*lcSeqNum = PADL(gfSequence("BNKADJNO",1,gcAct_Comp),8,'0')
lcSeqNum = gfSequence('CPAYDOCNO')
*E300663,1 Change this line [End]

*** Add record to the payment file. ***
*B500708,1  Updating CPAYRECST WITH 'O'
*E300296,4  Update currency fields as well, cCurrCode, nExRate, nCurrUnit
SELECT APPAYMNT
APPEND BLANK
REPLACE CPAYTYPE  WITH "P";
        CPAYMETH  WITH "B";
        CBNKCODE  WITH lcBankCode;
        CCHKACCT  WITH lcCheckcode;
        CPAYDOCNO WITH lcSeqNum;
        DPAYDATE  WITH ldAdjDat;
        CFISFYEAR WITH lcYear;
        CFSPPRDID WITH lcPeriod;
        LPAYADVAN WITH .F.;
        NPAYAMNT  WITH IIF(lcAdjTyp="Deposit",0-lnAdjAmnt,lnAdjAmnt);
        CPAYCOMP  WITH lcRef;
        CPAYRECST WITH 'O',;
        cCurrCode WITH lcCurrency,;
        nExRate   WITH lnExRate,;
        nCurrUnit WITH lnCurrUnit
*E300296,4  end.

*E300921,1 Added to update a bank adjustment reason field.
REPLACE cBnkAdj WITH laBAdj[lnBAdj,2]
*E300921,1 end.

=gfAdd_Info()  && Add the audit information to the record.

IF lcBnkTyp = 'S'
  REPLACE CCHKACCT  WITH 'CASH REGISTR',;
          CADD_USER WITH lcCheckcode
ENDIF
        
*N000232,1 HS 04/12/2001 Check if NC module is installed and this is a Cash Register
*N000232,1               Adjustment and the company setup "System Type" is set to
*N000232,1               "Point Of Sale" then add a record in the EDITrans to be used to send this Cash Register Adjustment [Begin]
IF llNCInst .AND. llFromPS .AND. llSite
  IF !llOpCodes
    llOpCodes = gfOpenFile(gcDataDir + 'Codes' , 'IDRLTFNAME' , 'SH')
  ENDIF
  IF !llOpAcPrt
    llOpAcPrt = gfOpenFile(gcDataDir + 'EDIAcPrt' , '' , 'SH')
  ENDIF
  IF !llOpEDITrn
    llOpEDITrn = gfOpenFile(gcDataDir + 'EDITrans' , '' , 'SH')
  ENDIF
  IF !llOpEdiPd
    llOpEdiPd =gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PARTTRANS','SH')
  ENDIF
  SELECT CODES
  IF SEEK("N" + "Y" + PADR("CSITEID" , 10))
    LOCATE REST;
          WHILE cDefCode + cRltField + cFld_Name = "N" + "Y" + PADR("CSITEID" , 10);
            FOR cRltd_Nam = "CCMSITETYP" .AND. cRltd_Vlu = "B"
    IF FOUND()
      SELECT EDIAcPrt
      LOCATE FOR cSiteID = CODES.cCode_No
      IF FOUND() AND SEEK(EdiAcPrt.cPartCode+'812','EdiPd')
        INSERT INTO EDITrans;
                    (cEDITrnTyp,Key,cPartner,Type,cStatus,lInterComp)  ;
             VALUES ("812","6" + APPaymnt.cPayDocNo + APPaymnt.cBnkCode,EDIAcPrt.cPartner,;
                     EDIAcPrt.Type , "N",EDIAcPrt.lInterComp)
        =gfAdd_Info('EDITRANS')
      ENDIF
    ENDIF
  ENDIF
ENDIF
*N000232,1 HS 04/12/2001 Check if NC module is installed and this is a Cash Register [End]

IF cbDstled=1  && Distribute to ledger.
  *E301077,71 AMM Open file.
  =gfOpenFile(gcDataDir+'APDIST','PAYMNTS','SH')
  *E301077,71 AMM end
  ***Add record to distribution file.***
  *E300296,4  Update currency fields as well, cCurrCode, nExRate, nEqvAmnt
  SELECT APDIST
  APPEND BLANK
  
  *E300316,1 HISH  12/05/95. Used the variables hold signs in the currency equation. (Begin)
  REPLACE CINVNO WITH lcSeqNum;
          DAPDTRDAT WITH ldAdjDat;
          LAPDPOST  WITH .F.;
          CAPDREF   WITH lcSeqNum;
          CAPDGLACT WITH lcGlchkact;
          NAPDAMNT  WITH IIF(lcAdjTyp="Deposit",lnAdjAmnt,0-lnAdjAmnt);
          CAPDACTID WITH "C";
          CFISFYEAR WITH lcYear;
          CFSPPRDID WITH lcPeriod;
          CAPSESSNO WITH lcSesNum;
          CAPDTRTYP WITH "B";
          CBNKCODE  WITH lcBankCode;
          CCHKACCT  WITH lcCheckcode,;
          cCurrCode WITH lcCurrency,;
          nExRate   WITH lnExRate,;
          nEqvAmnt  WITH IIF(lnExRate > 0 AND lnCurrUnit > 0,;
                         ROUND(nAPDAmnt &lcExSin1 lnExRate &lcExSin2 lnCurrUnit, 2),0)
          
          *nEqvAmnt  WITH ROUND(nAPDAmnt * lnExRate / lnCurrUnit, 2)
  *E300316,1 (End)
  *E300296,4  end.            
          =gfAdd_Info()  && Add the audit information to the record.

  *E300921,1 (Start) if bank type was safe update checking account
  *E300921,1 with cash register string and safe user.
  IF lcBnkTyp = 'S'
    REPLACE CCHKACCT  WITH 'CASH REGISTR',;
            CADD_USER WITH lcCheckcode
  ENDIF
  *E300921,1 (End)
                   
  ***Add record to distribution file.***
  *E300296,4  Update currency fields as well, cCurrCode, nExRate, nEqvAmnt
  SELECT APDIST
  APPEND BLANK
  
  *E300316,1 HISH  12/05/95. Used the variables hold signs in the currency equation. (Begin)
  REPLACE CINVNO WITH lcSeqNum;
          DAPDTRDAT WITH ldAdjDat;
          LAPDPOST  WITH .F.;
          CAPDREF   WITH lcSeqNum;
          CAPDGLACT WITH lcGldstact;
          NAPDAMNT  WITH IIF(lcAdjTyp="Deposit",0-lnAdjAmnt,lnAdjAmnt);
          CAPDACTID WITH "D";
          CFISFYEAR WITH lcYear;
          CFSPPRDID WITH lcPeriod;
          CAPSESSNO WITH lcSesNum;
          CAPDTRTYP WITH "B";
          CBNKCODE  WITH lcBankCode;
          CCHKACCT  WITH lcCheckcode,;
          cCurrCode WITH lcCurrency,;
          nExRate   WITH lnExRate,;
          nEqvAmnt  WITH IIF(lnExRate > 0 AND lnCurrUnit > 0,;
                         ROUND(nAPDAmnt &lcExSin1 lnExRate &lcExSin2 lnCurrUnit, 2),0)
          *nEqvAmnt  WITH ROUND(nAPDAmnt * lnExRate / lnCurrUnit, 2)
  *E300316,1 (End)        
  *E300296,4  end,
  =gfAdd_Info()  && Add the audit information to the record.

  *E300921,1 (Start) if bank type was safe update checking account
  *E300921,1 with cash register string and safe user.
  IF lcBnkTyp = 'S'
    REPLACE CCHKACCT  WITH 'CASH REGISTR',;
            CADD_USER WITH lcCheckcode
  ENDIF
  *E300921,1 (End)
ENDIF
=lfVCanAdj()
_CUROBJ=OBJNUM(lcBankCode)

*!**************************************************************************
*!
*!      Function: lfVCanAdj
*!
*!**************************************************************************
FUNCTION lfVCanAdj
*B601166,1 Hesham El-Sheltawi (Start)
*B601166,1 donot clear the bank code and the check code
*B601166,1 after adjustment
*lcBankCode  = SPACE(8)
*lcCheckCode = SPACE(12) 
*B601166,1 Hesham El-Sheltawi (End)
*C102056,1 ABD Get the  bank Adj code or register Adj code. [Begin]
*= gfwCodePop( @laCodInfo, "CBNKADJ" ,'D' )
= gfwCodePop( @laCodInfo, lcCodPopup ,'D' )
*C102056,1 ABD [End]
DECLARE laTrmRltFd[1,2]
*C102056,1 ABD Get the  GL bank Adj code or GL register Adj code. [Begin]
*laTrmRltFd[1,1] = 'CADJACCT  '
laTrmRltFd[1,1] = IIF(llFromPS , 'CGLACCT   ','CADJACCT  ')
*C102056,1 ABD [End]
laTrmRltFd[1,2] = 'lcGlDstAct'

*C102056,1 ABD Get the  related filed for bank Adj code or register Adj code. [Begin]
*=gfRltFld(laBAdj[1,2],@laTrmRltFd,"CBNKADJ")
=gfRltFld(laBAdj[1,2],@laTrmRltFd,lcCodPopup)
*C102056,1 ABD [End]

lcGlDstAct = IIF(EMPTY(lcGlDstAct),lcEmptyAcc,lcGlDstAct)
*lcGlDstAct  = lcEmptyAcc

*lcGlChkAct  = lcEmptyAcc
lcRef = SPACE (60)
*B601166,1 Hesham El-Sheltawi (Start)
*B601166,1 donot reinitialize the adj. type,Adj Date,Dst. led
*B601166,1 after adjustment
*lcAdjTyp    = laAdjTyp[1,1]
*ldAdjDat    = gdSysDate
*cbDstLed    = 1
*B601166,1 Hesham El-Sheltawi (End)
lnAdjAmnt   = 0.00
*E300296,4 Add currency code and exchange rate fields
STORE IIF(lfDefBank(), "ENABLE", "DISABLE") TO lcObjDisp, lcActDisp
lcCurrency  = SPACE(5)
STORE 0 TO lnExRate, lnCurrUnit
*E300296,4 Disable Rate display as well
*STORE "DISABLE" TO lcObjDisp, lcActDisp
STORE "DISABLE" TO lcObjDisp, lcActDisp 
*E300296,4 end.

=lfShow("DISABLE")
SHOW GET ibChecks    DISABLE
SHOW GET lcCheckCode DISABLE
_CUROBJ     = OBJNUM(lcBankCode)



*!**************************************************************************
*!
*!      Function: lfShow
*!
*!**************************************************************************
* 
FUNCTION lfShow
PARAMETERS lcObjDisp

*E300296,4 Change the following to an IF..ENDIF instead of macro
*E300296,4 substitution, for speed of execution.
*SHOW GET lcGlDstAct &lcActDisp
*SHOW GET lcGlChkAct &lcActDisp
*SHOW GET ibDstAcc   &lcActDisp
*SHOW GET ibchkAcc   &lcActDisp
*SHOW GET lcRef      &lcObjDisp
*SHOW GET lnAdjAmnt  &lcObjDisp
*SHOW GET ldAdjDat   &lcObjDisp
*SHOW GET cbDstled   &lcObjDisp
*SHOW GET lcAdjTyp   &lcObjDisp
*SHOW GET ibAdjTyp   &lcObjDisp
*SHOW GET pbCrtAdj   &lcObjDisp
*SHOW GET pbCanAdj   &lcObjDisp
*SHOW GET lnAdjTyp   &lcObjDisp

*TTTT
*IF !llApLink
*  cbDstled = 0
*ENDIF


IF lcObjDisp = 'ENABLE'
  *lcActDisp = IIF(cbDstled=1, lcObjDisp, "DISABLE") 
  IF cbDstled=1
    lcActDisp = 'ENABLE'
    SHOW GET lcGlDstAct ENABLE
    SHOW GET lcGlChkAct ENABLE
    SHOW GET ibDstAcc   ENABLE
    SHOW GET ibchkAcc   ENABLE
  ELSE
    lcActDisp = 'DISABLE'
    SHOW GET lcGlDstAct DISABLE
    SHOW GET lcGlChkAct DISABLE
    SHOW GET ibDstAcc   DISABLE
    SHOW GET ibchkAcc   DISABLE
  ENDIF 

  SHOW GET lcRef      ENABLE
  SHOW GET lnAdjAmnt  ENABLE
  SHOW GET ldAdjDat   ENABLE
  SHOW GET cbDstled   ENABLE
  SHOW GET lcAdjTyp   ENABLE
  SHOW GET ibAdjTyp   ENABLE
  SHOW GET pbCrtAdj   ENABLE
  SHOW GET pbCanAdj   ENABLE
  SHOW GET lnAdjTyp   ENABLE
  SHOW GET lnBAdj     ENABLE
  *E300296,4 Show currency fields as well.
  SHOW GET lcCurrency 
    IF llEditEx
    SHOW GET lnExRate ENABLE
  ELSE  
    SHOW GET lnExRate DISABLE
  ENDIF  
  *E300296,4 end.
ELSE
  lcActDisp = 'DISABLE' 
  SHOW GET lcGlDstAct DISABLE
  SHOW GET lcGlChkAct DISABLE
  SHOW GET ibDstAcc   DISABLE
  SHOW GET ibchkAcc   DISABLE
  SHOW GET lcRef      DISABLE
  SHOW GET lnAdjAmnt  DISABLE
  SHOW GET ldAdjDat   DISABLE
  SHOW GET cbDstled   DISABLE
  SHOW GET lcAdjTyp   DISABLE
  SHOW GET ibAdjTyp   DISABLE
  SHOW GET pbCrtAdj   DISABLE
  SHOW GET pbCanAdj   DISABLE
  SHOW GET lnAdjTyp   DISABLE
  SHOW GET lnBAdj     DISABLE
  *E300296,4 Disable currency fields as well.
  SHOW GET lcCurrency 
  SHOW GET lnExRate   DISABLE
  *E300296,4 end.
ENDIF
*E300296,4 end.

*TTTT
*IF !llApLink
*  SHOW GET cbDstled DISABLE
*ENDIF

=lfRefresh()

*!**************************************************************************
*!
*!      Function: lfDefBank
*!
*!**************************************************************************
* Get default division, bank, and checks.
*
FUNCTION lfDefBank
PRIVATE llVldBnk
*E300921,1 Do not default the bank since no ap setup exist in Ap not installed.
*C102056,1 ABD - Do this default only when call screen from Ap module only.  [Begin]
*IF llAPLink
IF  llAPLink .AND.  !llFromPS
  *C102056,1 ABD - [End]
  *E301077,71 AMM Open file
  =gfOpenFile(gcDataDir+'APSETUP','','SH')
  *E301077,71 AMM end
  lcBankCode  = APSETUP.CBNKCODE
  lcCheckCode = APSETUP.CCHKACCT
ENDIF
*IF SEEK(lcBankCode+lcCheckCode,'APCHECKS')
IF llAPLink AND SEEK(lcBankCode+lcCheckCode,'APCHECKS')
*E300921,1 (End)
   STORE IIF(EMPTY(APCHECKS.CCHKGLACC),lcEmptyAcc, APCHECKS.CCHKGLACC) ;
         TO lcGlChkAct, lcDefChAcc
   llVldBnk = .T.      
   *E300296,4 Get the currency of the checking account.
   lcCurrency = APCHECKS.cCurrCode
   IF lcCurrency = gcBaseCurr
     STORE 1 TO lnExRate, lnCurrUnit
     
     *B608292,1 NNA 09/26/2007 (Begin) if the Bank's Currancy = the base Currancy then we get the exchange rate field disable
     STORE .F. TO llEditEx
     SHOW GET lnExRate DISABLE
     *B608292,1 NNA (End)
     
   ELSE
     lnExRate   = gfChkRate('lnCurrUnit', lcCurrency, ldAdjDat)   
   ENDIF  
   *E300316,1 HISH  12/05/95. Got the equation signs. (Begin)
   *E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)     
   lcExSin2 = ' '
   lcExSin1 = gfGetExSin(@lcExSin2,lcCurrency)
   *lcExSin2 = IIF(lcExSin1 = '*','/','*')
   *E300316,1 (End)
   *E300296,4 end.
ELSE
  lcBankCode  = SPACE(8)
  lcCheckCode = SPACE(12)
  lcCurrency  = SPACE(5)
  STORE 0 TO lnExRate, lnCurrUnit
  STORE lcEmptyAcc TO lcGlChkAct, lcDefChAcc
  llVldBnk = .F.      
ENDIF
RETURN llVldBnk

*!**************************************************************************
*!
*!      Function: lfwOldVals
*!
*!**************************************************************************
* save the old value of feilds upon entry 
*
FUNCTION lfwOldVals
lcOldVal = EVALUATE(SYS(18))

*!**************************************************************************
*!
*!      Function: lfvBank
*!
*!**************************************************************************
* Validation of bank.
*
FUNCTION lfvBank
*E300296,4 Store the old currency field values before validation
*PRIVATE llVldBnk
PRIVATE lcOldCurr, lnOldRate, lnOldUnit, llVldBnk
*E300296,4 lcOldCurr : old currency code
*E300296,4 lnOldRate : old exchange rate
*E300296,4 lnOldUnit : old currency unit
lcOldCurr = lcCurrency
lnOldRate = lnExRate
lnOldUnit = lnCurrUnit
*E300296,4 end.
*E301077,71 AMM Open file
=gfOpenFile(gcDataDir+'APBANKS','BANKCODE','SH')
*E301077,71 AMM end
llVldBnk  = .F.

*C102056,1 ABD -Made filter on BankCode to browse safe only when call from PS module 
*C102056,1 ABD -Only and don't show any detail about banks in this screen. [Begin]
IF llFromPS
  lcOldFltr = Filter()
  SET FILTER TO cBnkType = 'S'
ENDIF
*C102056,1 ABD - [End]

llVldBnk  = lfBnkChk(@laBankObjs, lcOldVal, @llBrowse, @lcObjDisp)

*C102056,1 ABD - Return old filter .[Begin]
IF llFromPS
  SET FILTER TO &lcOldFltr
ENDIF
*C102056,1 ABD - [End]


IF cbDstled = 0
  lcGlChkAct = lcDefChAcc
ENDIF

*E300921,1 Read the bank type and if it safe check the users.
=SEEK(lcBankCode,'APBANKS')
lcBnkTyp = APBANKS.cBnkType
IF lcBnkTyp = 'S' AND llUsersExt
  lcSfUserStr = APBANKS.mSafeUsers
  IF EMPTY(lcSfUserStr)
    llVldBnk = .F.
    *--'No users found for this safe.' <ok>
    =gfModalGen("TRM04166B00000","DIALOG")

    STORE IIF(lfDefBank(), "ENABLE", "DISABLE") TO lcObjDisp, lcActDisp
    SHOW GET lcCheckCode DISABLE
    SHOW GET ibChecks    DISABLE

    _CUROBJ = OBJNUM(lcBankCode)
    SHOW GET lcBankCode
  ELSE
    lcCheckCode = IIF(gcUser_id $ lcSfUserStr ,gcUser_id,' ')  
  ENDIF
ENDIF
*E300921,1 end.

*E300296,4 Get the currency of the checking account.

*B604850,1 Remove this extra condition. [Begin]
*IF llVldBnk .AND. lcBankCode <> lcOldVal
IF llVldBnk
*B604850,1 Remove this extra condition. [End]
  IF !EMPTY(lcBankCode)
    lcCurrency = APCHECKS.cCurrCode
    IF lcCurrency = gcBaseCurr
      STORE 1 TO lnExRate, lnCurrUnit
    
      *B608292,1 NNA 09/26/2007 (Begin) if the Bank's Currancy = the base Currancy then we get the exchange rate field disable
      STORE .F. TO llEditEx
      SHOW GET lnExRate DISABLE
      *B608292,1 NNA (End)

    ELSE
      lnExRate   = gfChkRate('lnCurrUnit', lcCurrency, ldAdjDat)  

      *B608292,1 NNA 09/26/2007 (Begin) if the Bank's Currancy = the base Currancy then we get the exchange rate field disable
      IF gfGetMemVar('LLEDITEXRA')
        STORE .T. TO llEditEx
      ENDIF
      SHOW GET lnExRate ENABLE
      *B608292,1 NNA (End)
      
    ENDIF   
  ELSE
    lcCurrency = SPACE(5)
    STORE 1 TO lnExRate, lnCurrUnit
  ENDIF  
  *E300316,1 HISH  12/05/95. Got the equation signs. (Begin)
  *E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)       
  lcExSin2 = ' '
  lcExSin1 = gfGetExSin(@lcExSin2,lcCurrency)
  *lcExSin2 = IIF(lcExSin1 = '*','/','*')
  *E300316,1 (End)
ELSE
  *E300296,4 Restore old currency fields
  lcCurrency = lcOldCurr
  lnExRate   = lnOldRate 
  lnCurrUnit = lnOldUnit
ENDIF
*E300296,4 end.

*B601166,1 Hesham El_Sheltawi (Start)
IF lcBnkTyp = 'B' AND llVldBnk
  lcObjDisp ='ENABLE'
  *C102056,1 ABD -Made filter on BankCode to browse safe only when call from PS module 
  *C102056,1 ABD -Only and don't show any detail about banks in this screen. [Begin]
  IF llFromPS
    lcOldFltr = Filter()
    SET FILTER TO cbnktype = 'S'
  ENDIF
  *C102056,1 ABD - [End]
  
  = lfBnkChk(@laBankObjs, lcOldVal, @llBrowse, @lcObjDisp)
  
  *C102056,1 ABD - Return old filter .[Begin]
  IF llFromPS
    SET FILTER TO &lcOldFltr
  ENDIF
  *C102056,1 ABD - [End]
  
  
  llOldVld=llVldBnk
  IF lfvChecks()
    SHOW GET ibChecks    ENABLE
    SHOW GET lcCheckCode ENABLE
  ENDIF
  llVldBnk=llOldVld
ENDIF  
*B601166,1 Hesham El_Sheltawi (End)

*E300921,1 Chech the safe users if bank is valid.
IF lcBnkTyp = 'S' AND llVldBnk AND lcBankCode <> lcOldVal
  SHOW GET lcCheckCode DISABLE
  SHOW GET ibChecks    DISABLE
  IF lcCheckCode <> gcUser_id OR !llUsersExt
    llNewSafe = .T.
    =lfvChecks()  
  ENDIF
ENDIF
*E300921,1 End.
=lfshow(lcObjDisp)
SELECT APPAYMNT

RETURN IIF(llVldBnk, .T., 1)

*!**************************************************************************
*!
*!      Function: lfvChecks
*!
*!**************************************************************************
*
FUNCTION lfvChecks

*E300296,4 Store the old currency field values before validation
*PRIVATE llVldBnk
PRIVATE lcOldCurr, lnOldRate, lnOldUnit, llVldBnk
*E300296,4 lcOldCurr : old currency code
*E300296,4 lnOldRate : old exchange rate
*E300296,4 lnOldUnit : old currency unit
lcOldCurr = lcCurrency
lnOldRate = lnExRate
lnOldUnit = lnCurrUnit
*E300296,4 end.
llVldBnk  = .F.

IF lcBnkTyp = 'B' 

  *C102056,1 ABD -Made filter on BankCode to browse safe only when call from PS module 
  *C102056,1 ABD -Only and don't show any detail about banks in this screen. [Begin]
  IF llFromPS
    lcOldFltr = Filter()
    SET FILTER TO cbnktype = 'S'
  ENDIF
  *C102056,1 ABD - [End]

  llVldBnk   = lfBnkChk(@laBankObjs, lcOldVal, @llBrowse)

  *C102056,1 ABD - Return old filter .[Begin]
  IF llFromPS
    SET FILTER TO &lcOldFltr
  ENDIF
  *C102056,1 ABD - [End]
  
  
  *E300296,4 Get the currency of the checking account.
  IF llVldBnk .AND. lcCheckCode <> lcOldVal
    lcCurrency = APCHECKS.cCurrCode
    IF lcCurrency = gcBaseCurr
      STORE 1 TO lnExRate, lnCurrUnit
    ELSE
      lnExRate   = gfChkRate('lnCurrUnit', lcCurrency, ldAdjDat)
    ENDIF   
    *E300316,1 HISH  12/05/95. Got the equation signs. (Begin)
    *E300316,1 HISH  01/08/96. Passed pointer parameter to get Unit sgin. (Begin)         
    lcExSin2 = ' '
    lcExSin1 = gfGetExSin(@lcExSin2,lcCurrency)
    *lcExSin2 = IIF(lcExSin1 = '*','/','*')
    *E300316,1 (End)
    SHOW GET lcCurrency
    IF llEditEx
      SHOW GET lnExRate ENABLE
    ELSE  
      SHOW GET lnExRate DISABLE
    ENDIF  
  ELSE
    *E300296,4 Restore old currency fields
    lcCurrency = lcOldCurr
    lnExRate   = lnOldRate 
    lnCurrUnit = lnOldUnit
  ENDIF
  *E300296,4 end.

*E300921,1 Added case for bank type is safe to validate users.
ELSE   &&lcBnkTyp = 'S' 
  IF llUsersExt
    *--'BROWSE FOR USERS'
    lnStrLn=1
    FOR lnCnt = 1 TO INT(LEN(lcSfUserStr)/11)
      lcSfUsr = SUBSTR(lcSfUserStr,lnStrLn,10)
      IF !EMPTY(lcSfUsr) 
        =SEEK(lcSfUsr,'SYUUSER')
        DIMENSION laSafeUsrs[lnCnt,3]
        laSafeUsrs[lnCnt,1] = SYUUSER.cUsr_Name
        laSafeUsrs[lnCnt,2] = SYUUSER.cUsr_Pass
        laSafeUsrs[lnCnt,3] = lcSfUsr
        lnStrLn =lnStrLn + 11 
      ELSE
        EXIT        
      ENDIF  
    ENDFOR 

    llNewSafe = .F.
    llAlowAcss = .F.
    lcUsrPass  = SPACE(8)
    lnUsrCnt = 1
    *--Call select user and edit user password screen.
    DO (gcScrDir + 'APSFUSR.SPR')

    IF !llAlowAcss
      STORE IIF(lfDefBank(), "ENABLE", "DISABLE") TO lcObjDisp, lcActDisp
      _CUROBJ = OBJNUM(lcBankCode)  
      RETURN
    ENDIF
    lcCheckCode = laSafeUsrs[lnSfUser,3]
  ELSE
    llVldBnk = .T.
  ENDIF
  
  SHOW GET lcCheckCode DISABLE
  SHOW GET ibChecks    DISABLE

ENDIF
*E300921,1 End.

IF cbDstled = 0
  lcGlChkAct = lcDefChAcc
  SHOW GET lcGlChkAct DISABLE
  SHOW GET ibChecks   DISABLE
  =lfRefresh()
ENDIF
SELECT APPAYMNT
RETURN IIF(llVldBnk, .T., 1)


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

*!*************************************************************
*! Name      : lfvExRate
*! Developer : RENEE - Renee Ezzat
*! Date      : 10/29/1995
*! Purpose   : Valid function for lnExRate field
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
*E300296,4 field, lnExRate
FUNCTION lfvExRate
*E300296,4 if the entered value is not greater than zero,
*E300296,4 Message :  "   ð should be greater than ð.   "
*E300296,4                 ®   OK   ¯
IF lnExRate < 0 .AND. ;
   gfModalGen("TRM04072B00000","DIALOG", lcTExRateMsg) > 0
  *E300296,4 Old adjustment amount field is also filled from the
  *E300296,4 when function of this field, to save a variable.
  *E300296,4 i.e. At this point it actually contains the old exchange
  *E300296,4 rate.
  lnExRate = lnOAdjAmnt
ENDIF  
*E300296,4 end                

*!*************************************************************
*! Name      : lfvUsr_Pass
*! Developer : Timour A. K.
*! Date      : 07/16/1998
*! Purpose   : Valid function for safe user password.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvUsr_Pass()
*!*************************************************************
FUNCTION lfvUsr_Pass

lcCurUsrPas = ALLTRIM(laSafeUsrs[lnSfUser,2])

IF SYS(2007,ALLTRIM(lcUsrPass))=lcCurUsrPas
  llAlowAcss = .T.
  CLEAR READ
ELSE
  llAlowAcss = .F.
  lcUsrPass  = SPACE(8)
  *--Invalid user password.
  =gfModalGen('TRM42138B42000','DIALOG','user password')
  IF lnUsrCnt = 3
    CLEAR READ
  ELSE
    lnUsrCnt = lnUsrCnt + 1
    _CUROBJ=OBJNUM(lcUsrPass)
    RETURN
  ENDIF
ENDIF
*--End OF lfvUsr_Pass.
*!*************************************************************
*! Name      : lfvAdjRes
*! Developer : Adou ElGendi
*! Date      : 07/16/1998
*! Purpose   : Valid function for safe user password.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvAdjRes()
*!*************************************************************
*
FUNCTION lfvAdjRes

DECLARE laTrmRltFd[1,2]
laTrmRltFd[1,1] = IIF(llFromPS , 'CGLACCT   ','CADJACCT  ')
laTrmRltFd[1,2] = 'lcGlDstAct'
=gfRltFld(laBAdj[lnBAdj,2],@laTrmRltFd,lcCodPopup)
lcGlDstAct= IIF(EMPTY(lcGlDstAct),lcEmptyAcc,lcGlDstAct)
SHOW GET lcGlDstAct

=lfRefresh()
*-- End Of lfvAdjRes.
*!*************************************************************
