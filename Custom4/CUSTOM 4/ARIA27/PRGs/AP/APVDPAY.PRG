
*:************************************************************************
*:
*: Procedure file: APVDPAY.PRG
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: Accounts Payable
*:         Author: Mohamed Hassan Mohamed
*:      Copyright (c) 
*:  Last modified: 10/27/1994
*:
*:  Procs & Fncts: lfwScope
*:               : lfvScope
*:               : lfwVendor
*:               : lfvVendor
*:               : lfShow
*:               : lfvPayMeth
*:               : lfvOk
*:               : lfvCancel
*:               : lfvClsChld
*:               : lfBrowse
*:               : lfwDisBrow
*:               : lfvBrowse
*:               : lfwInvoice
*:               : lfvInvoice
*:               : lfBrwInv
*:               : lfwBrwInv
*:               : lfvBrwInv
*:               : lfwVenDis
*:               : lfvVenDis
*:               : lfvClose
*:               : lfActBrows
*:               : lfDefinePad
*:               : lfSetTrap
*:               : lfResetTrap
*:               : lpTab
*:               : lpShiftTab
*:               : lfRelease
*:               : lfRelChld
*:               : lfStInvTrp
*:               : lfRsInvTrp
*:               : lpInvTab
*:               : lfwClsInv
*:               : lfvVoid
*:               : lfwVoid
*:               : 
*:
*:      Documented   /  /1994
*:
*:************************************************************************
*B600492,1 Reham On 06/22/95
*B600492,1 Change the width of payment no. & vendor.
*E300258,4 MAN 06/29/95 Add the check on supressing 1099 processing
*B600627,1 Malak On 08/16/95
*B600627,1 Disable <Invoice> button when the currnt payment 'Advance payment'
*E300292,1 M.Hassam 09/14/95 Add a warning message before void any payment.
*E300296,4 RENEE 10/31/95 Add currency exchange checks (multi currency)
*B800271,1 RENEE 10/31/95 Add a new field "Void Date" (ldVoidDate) so as to
*B800271,1                use instead of the payment date.
*B600829,1 M.H 12/03/95   Change the title of the scope screen from "Scope" to "Payments Scope"
*B600995,1 Hesham El-Sheltawi 03/10/96 change the voiding function to void the existing
*B600995,1 records and then generate new once with changing for the new and the
*B600995,1 old records the status to 'V'
*B601060,1 RENEE 05/09/96 If voiding an advance payment that has been applied
*B601060,1                to one or more invoices, we have to clear the 
*B601060,1                corresponding amounts of the apllied-to invoices.
*B800603,1 M.H On 05/20/96 Void payment do not point to the top of the file after voiding a payment.
*B601071,1 M.H On 05/20/96 Wrong button enabeling 'Invoice' if there is no payments to be voided.
*B601085,1 M.H On 05/28/96 Cleared payments should not show in the filter.
*B800740,1 M.H On 09/09/96 Add a new CASE to update the APVENHST.NVNHPCHKP in case of printed check.
*B601528,1   HS   02/18/97 Use the APDIST records with cApdActId A and S
*B601528,1                 and J to adjust the APINVHDR.NINVPAID instaed
*B601528,1                 of the record with cApdActId C for it has a 
*B601528,1                 defrunt currency.
*E300663,1  HS 04/23/97 Change the calling of the function [gfSequence]
*E300663,1              for the changes we have made to that function.
*E300683,1 AHMED 06/04/97 Add screens directory path to the calling of SPRS
*B601994,1   HS   04/06/98 1) Check if the Payment to be void is an
*B601994,1                    advance payment and it has already been paid,
*B601994,1                    and if so give the user a message and cancel
*B601994,1                    the Voiding process.
*B601994,1                 2) If the Payment to be void is an advance
*B601994,1                    payment void only the advance payment
*B601994,1                    debit memo records
*B601994,1                 3) If the payment to be void is an advance
*B601994,1                    payment and there is some application record
*B601994,1                    for the debit memo (The Advance payment
*B601994,1                    Debit memo), void these records.
*B601994,1                 4) If voiding a payment, on an invoice that has
*B601994,1                    a part applied against a debit memo, the application 
*B601994,1                    on the invoice should not be cleared by voiding the payment
*E301077,79 AMM 03/04/99 Enhance opening and closing files.
*E500370,1 SSE 08/09/2000 Enhancement of Adding Payment amount to the browse showing payment alsp
*E500370,1                change font of Browse screen ApVdPay1.Scx from FoxFont to standard Ms Sans Serif
*B603958,1 KAM 09/26/2000 to prevent voiding invoice within locked period
*B605486,1 SSH 26/Feb/2002 Do not brows for adjustment records
*B132275,1 TMI 06/11/2006 Fix a bug that the invoice payment is voided while the invoice itself is being edited
*B608717,1 WAM 10/08/2008 Don't validate original payment date [T20080924.0028]
*B608959,1 TMI 08/04/2009 Add a check that the line is not voided
*:************************************************************************
*
DECLARE laPayMeth[5,2],laWndObj[3,3],laFileStru[1]

** lcMain1       : Variable to hold the main screen 1 name.
** lcMain2       : Variable to hold the main screen 2 name.
** lcMain3       : Variable to hold the main screen 3 name.
** lcApDist      : Variable to hold the Temp. distribution file name.
** lcOldVend     : Variable to hold the old vendor in the screen.
** lcOldVendor   : Variable to hold the old vendor in the filter. 
** lcPayMeth     : Variable to hold the payment method.
** lcOldPayMth   : Variable to hold the old payment method of the filter.
** lcMainFilt    : Variable to hold the main filter of the payments file.
** lcFiltExp     : Variable to hold the extra filter.
** lcInvTtl      : Variable to hold the title of the invoice browse.
** lcBrowsTtl    : Variable to hold the browse title 'Distribution'.
** lcTempInv     : Variable to hold the Temp. invoice file.
** lcCurrYear    : Variable to hold the current year of the record.
** lcPayment     : Variable to hold the the payment discription.
** lcFiscalY     : Variable to the fiscal year.
** lcFiscalP     : Variable to hold the fiscal period.
** lcSequence    : Variable to hold the sequence no.
** lcVendor      : Variable to hold the vendor name.
** lcVendCode    : Variable to hold the vendor code.
** lcPayMethd    : Varibale of the say field in the screen.
** lcTVendTtl    : Variable to hold the title of the vendor screen.
** lcTInvTitl    : Variable to hold the title of the invoice screen.
** lcTScop       : Variable to hold the title of the scope screen.
** lcTNpPay      : Variable to hold the words 'payments to void'.
** lcTMsgTxt     : Variable to hold the word  'payments'.
** lcTMorePay    : Variable to hold the words 'more payments to void'.
**
** ldPayFrDat    : Variable to hold the payment from date.
** ldOldPayFrD   : Variable to hold the old payment from date.
** ldPayToDat    : Variable to hold the payment to date.
** ldOldPayToD   : Variable to hold the old payment to date.
** ldPrdFrDat    : Variable to hold the period from date.
** ldOldPrdFrD   : Variable to hold the old period from date.
** ldPrdToDat    : Variable to hold the period to date.
** ldOldPrdToD   : Variable to hold the old period to date.
*B800271,1 Add a variable to hold the void date, and another for the old
*B800271,1 date value, and old year, period                
*B800271,1 ldVoidDate : Variable to hold the void date.       
*B800271,1 ldOldDate  : Variable to hold the old void date.   
*B800271,1 lcOldFYear : Variable to hold old fiscal year     
*B800271,1 lcOldFPer  : Variable to hold old fiscal period    
*B800271,1 lcTPayDate : Variable to hold "payment date".
*B800271,1 lcTVoidDat : Variable to hold "Void date".
*B800271,1 end.
**
** lnBrRecNo     : Browse record number 
** lnYTDTotPay   : Variable to hold the year to date total payment.
**
** llBrowse      : Browse diplay flag.
** llNoContrl    : Variable to hold 
**
** puMethod      : Variable to hold the popup method.
** rbScope       : Variable to hold the radio button Scope.
** laCtrStat     : To disable the browse pad in the menu

*E500370,1 Add a Variable to detect whether Company is Single or Multi [Begin]
** llMultCurr    : Variable to detect Company Multi or Single Currency
llMultCurr = gfGetMemVar('llMulCurr')  
*E500370,1 Add a Variable to detect whether Company is Single or Multi [End]

STORE .F.       TO llBrowse
STORE .T.       TO llNoContrl
STORE 0         TO lnBrRecNo
STORE 0.00      TO lnYTDTotPay
STORE 1         TO puMethod   , rbScope

*B800271,1 Initialize ldOldDate variable with {}.
STORE {}        TO ldPayFrDat , ldOldPayFrD , ldPayToDat , ldOldPayToD ,;
                   ldPrdFrDat , ldOldPrdFrD , ldPrdToDat , ldOldPrdToD ,;
                   ldOldDate
*B800271,1 Initialize lcOldFYear, lcOldFPer with ''
STORE '' TO lcOldFYear, lcOldFPer        
*B800271,1 end.
                   
STORE SPACE(8)  TO lcVendor   , lcVendCode

STORE 'All'     TO lcPayMethd

*B600829,1 M.H 12/03/95 Change the title of the scope screen from "Scope" to "Payments Scope"
* STORE 'Scope'   TO lcTScop 
STORE "Payments Scope" TO lcTScop
*B600829,1 M.H 12/03/95 
STORE 'Vendor'  TO lcTVendTtl
STORE 'DISABLE' TO laCtrStat
STORE 'Invoice' TO lcTInvTitl

*B800271,1 Text added for void date messages
STORE 'void'         TO lcTVoid
STORE 'void date'    TO lcTVoidDate
STORE "or equal the payment date" TO lcTPayDate
STORE "Void date"    TO lcTVoidDat 

*B800271,1 end.

STORE 'payments to void'      TO lcTNpPay
STORE 'payments'              TO lcTMsgTxt 
STORE 'more payments to void' TO lcTMorePay

STORE ' '       TO lcMain1    , lcMain2     , lcMain3    , lcApDist    ,;
                   lcOldVend  , lcOldVendor , lcPayMeth  , lcOldPayMth ,;
                   lcMainFilt , lcFiltExp   , lcInvTtl   , lcBrowsTtl  ,;
                   lcTempInv  , lcCurrYear  , lcPayment  , lcFiscalY   ,;
                   lcFiscalP  , lcSequence

*B800271,1 Initialize void date with the system date.
STORE gdSysDate TO ldVoidDate
*B800271,1 end.
*** Array to hold the payment methods. ***
laPayMeth[1,1] = 'All'
laPayMeth[1,2] = ' '
laPayMeth[2,1] = 'Printed checks'
laPayMeth[2,2] = 'P'
laPayMeth[3,1] = 'Manual checks'
laPayMeth[3,2] = 'M'
laPayMeth[4,1] = 'Non check payments'
laPayMeth[4,2] = 'N'
laPayMeth[5,1] = 'Cash payment'
laPayMeth[5,2] = 'H'

IF !gfSetup()
  RETURN
ENDIF

IF !WEXIST(gcBaseWind)

  lcMain1     = gfTempName()   && Random name for the main screen 1.
  lcMain2     = gfTempName()   && Random name for the main screen 2.
  lcMain3     = gfTempName()   && Random name for the main screen 3.
  lcApDist    = gfTempName()   && Random name to open the distribution file again.
  lcTempInv   = gfTempName()   && Random name for invoice Temp file.

  IF EMPTY(lcSequence)
    ** Generate new Seq. No. **

    *E300663,1 Change this line for the changes we have 
    *          made to (gfSequence) [Begin]
    *lcSequence = PADL(ALLTRIM(STR(gfSequence('APSESS',1))),;
    *             FSIZE('cApSessNo','APDIST'),'0')
    lcSequence = gfSequence('CAPSESSNO')
    *E300663,1 Change this line [End]
    
  ENDIF
  
  SELECT APINVHDR

  ** Collect the PAYMENT file fields in an array to create the **
  ** the Temp. invoice file.                                   **
   
  =AFIELDS(laFileStru)

  ** Add 5 more fiilds to the invoice file structure. **
  
  lnFileStru = ALEN(laFileStru,1)
  
  DIMENSION laFileStru[lnFileStru+5,4]

  laFileStru[lnFileStru+1,1] = 'N1099'
  laFileStru[lnFileStru+1,2] = 'N'
  laFileStru[lnFileStru+1,3] = 15
  laFileStru[lnFileStru+1,4] = 2
  
  laFileStru[lnFileStru+2,1] = 'NPAID'
  laFileStru[lnFileStru+2,2] = 'N'
  laFileStru[lnFileStru+2,3] = 15
  laFileStru[lnFileStru+2,4] = 2
  
  laFileStru[lnFileStru+3,1] = 'NADJUST'
  laFileStru[lnFileStru+3,2] = 'N'
  laFileStru[lnFileStru+3,3] = 15
  laFileStru[lnFileStru+3,4] = 2

  laFileStru[lnFileStru+4,1] = 'NDISCOUNT'
  laFileStru[lnFileStru+4,2] = 'N'
  laFileStru[lnFileStru+4,3] = 10
  laFileStru[lnFileStru+4,4] = 2

  laFileStru[lnFileStru+5,1] = 'NTOTAL'
  laFileStru[lnFileStru+5,2] = 'N'
  laFileStru[lnFileStru+5,3] = 15
  laFileStru[lnFileStru+5,4] = 2

  *** Creat temp  file from the array.
  CREATE TABLE (gcWorkDir+lcTempInv) FROM ARRAY laFileStru

  SELECT APDIST
  SELECT 0
  USE (gcDataDir+'APDIST') AGAIN ALIAS (lcApDist)

  SELECT APPAYMNT
  ** Set filter to the PAYMENT file. **
*B601085,1 M.H On 05/28/96 Begin.
*  lcMainFilt = "APPAYMNT.CPAYTYPE = 'P' .AND. APPAYMNT.CPAYMETH $ 'PMNH' .AND. APPAYMNT.CPAYSTAT <> 'V'"
  *B605486,1 SSH 26/Feb/2002 Do not brows for adjustment records
  lcMainFilt = "APPAYMNT.CPAYTYPE = 'P' .AND. APPAYMNT.CPAYMETH $ 'PMN' .AND. APPAYMNT.CPAYSTAT <> 'V' .AND. APPAYMNT.CPAYRECST <> 'C'"
  *B605486,1 SSH [END]
*B601085,1 M.H On 05/28/96 End.
ENDIF

** Store the current year of the parent company. **
lcCurrYear  = IIF(SEEK(gcPrnt_Cmp,'SYCCOMP'),SYCCOMP.cCurr_Yer," ")               && current year

** Fill the array holding frist and last object in base **
** window and each children.                            **

laWndObj [1,1] = lcMain2
*B800271,1 Set ldVoidDate as the first element on the screen instead
*B800271,1 of rbScope.
*laWndObj [1,2] = "RBSCOPE"
laWndObj [1,2] = "ldVoidDate"
*B800271,1 end.
laWndObj [1,3] = "pbClose"

laWndObj [2,1] = lcMain3
*B800271,1 Set ldVoidDate as the first element on the screen instead
*B800271,1 of rbScope.
*laWndObj [2,2] = "RBSCOPE"
laWndObj [2,2] = "ldVoidDate"
*B800271,1 end.
laWndObj [2,3] = "pbClose"

laWndObj [3,1] = "CWRAPVDVEN"
laWndObj [3,2] = "pbClsVend"
laWndObj [3,3] = "pbClsVend"

** Set the relation between the payment and the     **
** vendor,vendor history and the distribution file. **

SELECT APVENDOR
SET ORDER TO TAG VENCODE

SELECT APVENHST
SET ORDER TO TAG VENDYEAR

SELECT APDIST
SET ORDER TO TAG PAYMNTS

SELECT APINVHDR
SET ORDER TO TAG INVVEND

SELECT (lcApDist)
SET ORDER TO TAG PAYMNTS

SELECT APPAYMNT
SET RELATION TO APPAYMNT.CPAYCLNO INTO APVENDOR ADDITIVE
SET RELATION TO APPAYMNT.CPAYCLNO+APPAYMNT.CFISFYEAR INTO APVENHST ADDITIVE
SET RELATION TO APPAYMNT.CPAYMETH+APPAYMNT.CBNKCODE+APPAYMNT.CCHKACCT+APPAYMNT.CPAYDOCNO INTO APDIST ADDITIVE

SELECT APDIST
SET RELATION TO APDIST.CINVNO+APDIST.CVENDCODE INTO APINVHDR ADDITIVE

** Key trapping because of the browse. **

PUSH KEY

SELECT APPAYMNT
IF !EMPTY(lcFiltExp)
  rbScope = 2
  SET FILTER TO &lcFiltExp
ELSE
  rbScope = 1
  SET FILTER TO &lcMainFilt
ENDIF

IF !WEXIST(gcBaseWind)
  GO TOP
ENDIF
  
IF EOF() .AND. rbScope = 1
  ** MESSAGE : "There are no ð ."
  **           "        ® Ok ¯            "
  =gfModalGen("TRM04101B00000","DIALOG",lcTNpPay)
  glQuitting=.T.  

  RELEASE PAD _BROWSE OF _MSYSMENU && Erase the browse pad from the main menu.

  POP KEY  && Clear key trapping.

  SELECT APPAYMNT
  SET RELATION TO
  SET FILTER TO

  SELECT APDIST
  SET RELATION TO

  IF USED(lcApDist)
    USE IN &lcApDist
  ENDIF

  IF USED(lcTempInv) 
    USE IN &lcTempInv
  ENDIF
  ERASE (gcWorkdir+lcTempInv+".DBF")  && Erase the Temp file.
  ERASE (gcWorkdir+lcTempInv+".CDX")  && Erase the Temp index.
  ERASE (gcWorkdir+lcTempInv+".FPT")  && Erase the Temp FPT.  
  RETURN
ELSE  
  *B800271,1 Set traps only when the browse is on top. 
  *=lfSetTrap() && Set the local trapping for the screen.
  *ON KEY LABEL TAB     DO lpTab
  *ON KEY LABEL BACKTAB DO lpShiftTab
  IF _DOS
    =lfDefinePad() && Define the Browse pad in the menu.
  ENDIF  
  ON KEY LABEL ALT+B DO lfActBrows
  *B800271,1 end.
  
*E300683,1 Call *.SPR from screens directory
*DO APVDPAY.SPR
DO (gcScrDir + gcWinAppl + '\APVDPAY.SPR')
*E300683,1 end

ENDIF

RELEASE PAD _BROWSE OF _MSYSMENU && Erase the browse pad from the main menu.

POP KEY  && Clear key trapping.

SELECT APPAYMNT
SET RELATION TO
SET FILTER TO

SELECT APDIST
SET RELATION TO

IF glQuitting
  =gfChClose('CWRAPVDVEN')

  HIDE WINDOW  (gcBaseWind)

  RELEASE WINDOW CWRAPVDVEN,(lcBrowsTtl)
*  RELEASE WINDOW (lcBrowsTtl)

  IF USED(lcApDist)
    USE IN &lcApDist
  ENDIF

  IF USED(lcTempInv) 
    USE IN &lcTempInv
  ENDIF
  ERASE (gcWorkdir+lcTempInv+".DBF")  && Erase the Temp file.
  ERASE (gcWorkdir+lcTempInv+".CDX")  && Erase the Temp index.
  ERASE (gcWorkdir+lcTempInv+".FPT")  && Erase the Temp FPT.  
ENDIF  
glFromBrow  = .F.

*!**************************************************************************
*!
*!      Function: lpShow
*!
*!**************************************************************************
*
FUNCTION lpShow

SELECT APPAYMNT

** If the scope has no record in the main file so we are going to **
** disable the P.B. vendor,P.B. invoice and the P.B. void.        **

IF EOF()
  IF WVISIBLE('CWRAPVDVEN')
    =gfChClose('CWRAPVDVEN')
  ENDIF
  SHOW GET pbVendor  DISABLE
  SHOW GET pbInvoice DISABLE
  SHOW GET pbVoid    DISABLE
ELSE
  SHOW GET pbVendor  ENABLE
  SHOW GET pbInvoice ENABLE
  SHOW GET pbVoid    ENABLE
ENDIF  

=lfwDisBrow()
*B800271,1 Set current object to void date.
_CUROBJ   = OBJNUM(ldVoidDate)  
*B800271,1 end.

*!**************************************************************************
*!
*!      Function: lfwScope
*!
*!**************************************************************************
*
FUNCTION lfwScope

IF _DOS
  =lfDefinePad()
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvScope
*!
*!**************************************************************************
*
FUNCTION lfvScope

SELECT APPAYMNT

IF rbScope = 1
  SET FILTER TO &lcMainFilt
  GO TOP
  lnBrRecNo  = RECNO('APPAYMNT') 
  lcFiltExp = ' '
ELSE
  RELEASE PAD _BROWSE OF _MSYSMENU
  ON KEY LABEL ALT+B
  
  ** Store the variables of the filter. **
  lcOldVendor= lcVendor
  lcOldPayMth= lcPayMeth
  ldOldPayFrD= ldPayFrDat
  ldOldPayToD= ldPayToDat
  ldOldPrdFrD= ldPrdFrDat
  ldOldPrdToD= ldPrdToDat
  PUSH KEY
  ON KEY

  *E300683,1 Call *.SPR from screens directory
  *  DO APVDSCP.SPR   && Run the screen.
  DO (gcScrDir + gcWinAppl + '\APVDSCP.SPR')
  *E300683,1 end
  POP KEY
  
  lnBrRecNo  = RECNO('APPAYMNT') 
ENDIF

** Refresh the browse window. **
SHOW WINDOW (lcBrowsTtl) REFRESH SAME

SELECT APPAYMNT

** If the scope has no record in the main file so we are going to **
** disable the P.B. vendor,P.B. invoice and the P.B. void.        **

IF EOF()
  IF WVISIBLE('CWRAPVDVEN')
    =gfChClose('CWRAPVDVEN')
  ENDIF
  ** MESSAGE : "No ð mateching selected invoices criteria."
  **           "                  ® Ok ¯                  "
  =gfModalGen("TRM04089B00000","DIALOG",lcTMsgTxt)
  rbScope = 1
  SET FILTER TO &lcMainFilt
  GO TOP
  SHOW GET rbScope
  lcFiltExp = ' '
ENDIF

lnBrRecNo  = RECNO('APPAYMNT') 
SHOW WINDOW (lcBrowsTtl) REFRESH SAME

*B601071,1 M.H On 05/20/96 Begin.
IF !EOF('APPAYMNT')
*B601071,1 M.H On 05/20/96 End.
  SHOW GET pbVendor  ENABLE
  SHOW GET pbInvoice ENABLE
  SHOW GET pbVoid    ENABLE
*B601071,1 M.H On 05/20/96 Begin.
ENDIF
*B601071,1 M.H On 05/20/96 End.

=lfwDisBrow()

*!**************************************************************************
*!
*!      Function: lfwVendor
*!
*!**************************************************************************
*
FUNCTION lfwVendor

lcOldVend = lcVendor  && Store the vendor object content.

*!**************************************************************************
*!
*!      Function: lfvVendor
*!
*!**************************************************************************
*
FUNCTION lfvVendor

SELECT APVENDOR
SET ORDER TO TAG VENCODE

lcSavExact= SET('EXACT')

SET EXACT ON

IF llBrowse .OR. !EMPTY(lcVendor) 
  IF llBrowse .OR. ATC("?",lcVendor) > 0 .OR. !SEEK(lcVendor) 
    DIMENSION laTemp[1]
    laTemp = ''
    lcSavBrFld=lcBrFields 
    lcSavTitle=lcFile_Ttl

    lcBrFields = "CVENDCODE :H= 'Vendor Code',;
                  CVENCOMP :H= 'Company',;
                  CPHONENO :H= 'Phone'"
 
    lcFile_Ttl = "Vendor"

    lnClosRec  = RECNO(0)
          
    IF BETWEEN(lnClosRec,1,RECCOUNT('APVENDOR'))
      GO lnClosRec
    ELSE
      GO TOP
    ENDIF
        
    =gfBrows(.F.,'CVENDCODE,CPHONENO,CVENCOMP','laTemp')
    
    lcBrFields = lcSavBrFld
    lcFile_Ttl = lcSavTitle
      
    IF !EMPTY(laTemp[1])
      lcVendor = laTemp[1]
    ELSE
      lcVendor = lcOldVend
    ENDIF
  ENDIF
ENDIF  

SET EXACT &lcSavExact

SHOW GET lcVendor

=lfRefresh()

llBrowse = .F.

SELECT APINVHDR

*!**************************************************************************
*!
*!      FUNCTION : lfShow
*!
*!**************************************************************************
* 
FUNCTION lfShow

IF _DOS
  lcPayMeth = IIF(EMPTY(lcPayMeth),' ',laPayMeth[AT(lcPayMeth,' PMNH'),2])
ELSE
  puMethod  = AT(lcPayMeth,' PMNH')
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvPayMeth
*!
*!**************************************************************************
*
FUNCTION lfvPayMeth
 
DO CASE
  CASE _DOS
    lcPayMeth = gfActPop(5,12,11,33,'laPayMeth',2,1,@lcPayMethd)
  CASE _WINDOWS
    lcPayMeth = laPayMeth[puMethod,2]
ENDCASE

=lfRefresh()

*!**************************************************************************
*!
*!      Function: lfvOk
*!
*!**************************************************************************
*
FUNCTION lfvOk

lcFiltExp = lcMainFilt

** Building the filter. **

IF !EMPTY(lcVendor)
  lcFiltExp = lcFiltExp + ".AND. APPAYMNT.CPAYCLNO = lcVendor"
ENDIF

IF !EMPTY(lcPayMeth)
  lcFiltExp = lcFiltExp + ".AND. APPAYMNT.CPAYMETH = lcPayMeth"
ENDIF

IF !EMPTY(ldPayFrDat)
  lcFiltExp = lcFiltExp + ".AND. APPAYMNT.DPAYDATE >= ldPayFrDat"
ENDIF

IF !EMPTY(ldPayToDat)
  lcFiltExp = lcFiltExp + ".AND. APPAYMNT.DPAYDATE =< ldPayToDat"
ENDIF

IF !EMPTY(ldPrdFrDat)
  lcFiltExp = lcFiltExp + ".AND. APPAYMNT.DPAYDATE >= ldPrdFrDat"
ENDIF

IF !EMPTY(ldPrdToDat)
  lcFiltExp = lcFiltExp + ".AND. APPAYMNT.DPAYDATE =< ldPrdToDat"
ENDIF

SELECT APPAYMNT
SET FILTER TO &lcFiltExp
LOCATE

IF _DOS
  =lfDefinePad()
ENDIF  

CLEAR READ

*!**************************************************************************
*!
*!      Function: lfvCancel
*!
*!**************************************************************************
*
FUNCTION lfvCancel

SELECT APPAYMNT

** Retrieve the old contents of the objects. **
lcVendor   = lcOldVendor
lcPayMeth  = lcOldPayMth
ldPayFrDat = ldOldPayFrD
ldPayToDat = ldOldPayToD
ldPrdFrDat = ldOldPrdFrD
ldPrdToDat = ldOldPrdToD

IF _DOS
  =lfDefinePad()
ENDIF  

CLEAR READ

*!**************************************************************************
*!
*!      Function: lfvClsChld
*!
*!**************************************************************************
*
FUNCTION lfvClsChld

IF WVISIBLE('CWRAPVDVEN')
  =gfChClose('CWRAPVDVEN')
ENDIF

IF _DOS
  =lfDefinePad()
ENDIF  

*!**************************************************************************
*!
*!      Function : lfBrowse
*!
*!**************************************************************************
*
FUNCTION lfBrowse
PRIVATE lcClrSchm

lcClrSchm = IIF(_DOS," COLOR SCHEME 13","NOMENU")

SELECT APPAYMNT
lnBrRecNo  = RECNO('APPAYMNT')
lcBrowsTtl = 'Payments'

*B600492,1 Change the width of payment no. & vendor.
*E500370,1 Add Payment to the browse showing payment [Begin]
*E500370,1 Change also the font from FoxFont to Ms Sans Serif in browse screen set ApVdPay1
*lcBrowStr  = "cMarker=IIF(RECNO()=lnBrRecNo,'',' '):1:H=' ':W=.F.,"+; 
*             "CPAYDOCNO :8:R :H='Pay. No.',"+;
*             "CADV = IIF(LPAYADVAN,'Yes','No') :4 :H='Adv.',"+;
*             "CPAYMETHD = IIF(CPAYMETH = 'P',"+;
*             "'Printed checks',IIF(CPAYMETH = 'M',"+;
*             "'Manual checks',IIF(CPAYMETH = 'N',"+;
*             "'Non check payments','Cash payment')))"+;
*             ":H='Payment method' :R,DPAYDATE :R :H='Payment date',"+;
             "CPAYCLNO :R :8:H='Vendor',CPAYCOMP :R :H='Company'"
IF llMultCurr
  lcBrowStr  = "cMarker=IIF(RECNO()=lnBrRecNo,'>',' '):1:H=' ':W=.F.,"+; 
               "CPAYDOCNO :8:R :H='Pay. No.',"+;
               "CADV = IIF(LPAYADVAN,'Yes','No') :4 :H='Adv.',"+;
               "CPAYMETHD = IIF(CPAYMETH = 'P',"+;
               "'Printed checks',IIF(CPAYMETH = 'M',"+;
               "'Manual checks',IIF(CPAYMETH = 'N',"+;
               "'Non check payments','Cash payment')))"+;
               ":H='Payment method' :R,DPAYDATE :R :H='Payment date',"+;
               "NPAYAMNT:16:R:H='Payment Amount',CCURRCODE :10:R:H='Currency',"+;
               "CPAYCLNO :R :8:H='Vendor',CPAYCOMP :R :H='Company'"
ELSE
  lcBrowStr  = "cMarker=IIF(RECNO()=lnBrRecNo,'>',' '):1:H=' ':W=.F.,"+; 
               "CPAYDOCNO :8:R :H='Pay. No.',"+;
               "CADV = IIF(LPAYADVAN,'Yes','No') :4 :H='Adv.',"+;
               "CPAYMETHD = IIF(CPAYMETH = 'P',"+;
               "'Printed checks',IIF(CPAYMETH = 'M',"+;
               "'Manual checks',IIF(CPAYMETH = 'N',"+;
               "'Non check payments','Cash payment')))"+;
               ":H='Payment method' :R,DPAYDATE :R :H='Payment date',"+;
               "NPAYAMNT:16:R:H='Payment Amount',"+;
               "CPAYCLNO :R :8:H='Vendor',CPAYCOMP :R :H='Company'"
ENDIF               
*E500370,1 Add Payment to the browse showing payment [End]

BROWSE FIELDS &lcBrowStr;
              WINDOW (lcMain1) ;
              WHEN lfwDisBrow();
              VALID :F lfvBrowse();
              IN WINDOW (gcBaseWind);
              LOCK 0;
              NOAPPEND;
              NOCLEAR;
              NODELETE;
              NOWAIT;
              SAVE;
              TITLE lcBrowsTtl;
              NOEDIT &lcClrSchm

*!**************************************************************************
*!
*!      Function : lfwDisBrow
*!
*!**************************************************************************
*
FUNCTION lfwDisBrow
*B800271,1 Set glFromBrow flagin deactivate function (lfRelease) upon 
*B800271,1 entry of the browse screen.
*glFromBrow = .T.
*B800271,1 end.

lnBrRecNo  = RECNO('APPAYMNT')
SHOW WINDOW (lcBrowsTtl) REFRESH

*B800271,1 Set traps in deactivate function (lfRelease) upon entry
*B800271,1 of the browse screen.
*=lfSetTrap()
*RELEASE PAD _BROWSE OF _MSYSMENU
*ON KEY LABEL ALT+B
*B800271,1 end.

IF lcVendCode <> APVENDOR.CVENDCODE
  lnYTDTotPay = IIF(SEEK(APVENDOR.CVENDCODE + lcCurrYear,'APVENHST'),;
                       APVENHST.nVnHTotPa, 0)
  lcVendCode  = APVENDOR.CVENDCODE
ENDIF       

IF WVISIBLE('CWRAPVDVEN')
  *B800271,1 
  *ON KEY LABEL TAB lnDummy = 1
  *ON KEY LABEL BACKTAB lnDummy = 1
  *ON KEY LABEL TAB     DO lpTab
  *ON KEY LABEL BACKTAB DO lpShiftTab
  *B800271,1 end.
  =lfRefresh()
ENDIF  

*B600627,1 Malak On 08/16/95 {Begin}
*B600627,1 Disable <Invoice> button when the currnt 
*B600627,1 payment 'Advance payment'
*B601071,1 M.H On 05/20/96 Begin.
*IF APPAYMNT.lPayAdvan 
IF APPAYMNT.lPayAdvan .OR. EOF('APPAYMNT')
*B601071,1 M.H On 05/20/96 End.
ELSE
  SHOW GET pbInvoice ENABLE
ENDIF
*B600627,1 Malak {End}

*!**************************************************************************
*!
*!        Function : lfvBrowse
*!
*!**************************************************************************
* 
FUNCTION lfvBrowse

IF WONTOP(lcBrowsTtl)
  IF !WEXIST(lcMain1)
    glFromBrow = .F.
    glQuitting = .T.
    CLEAR READ
    KEYBOARD CHR(13)
    RETURN TO APVDPAY.SPR
  ENDIF
ELSE
  *B800271,1 
  *=lfResetTrap()
  =lfReadAct()
  *B800271,1 end.
  
  IF !WVISIBLE(gcBaseWind)
    glQuitting = .T.
  ENDIF  
  =gfStopBrow()
ENDIF    

*!**************************************************************************
*!
*!      FUNCTION : lfwInvoice
*!
*!**************************************************************************
* 
*B800271,1 Not used anymore
*FUNCTION lfwInvoice

*IF _DOS
*  =lfDefinePad()
*ENDIF  

*!**************************************************************************
*!
*!      FUNCTION : lfvInvoice
*!
*!**************************************************************************
* 
FUNCTION lfvInvoice

lcInvoice = ' '

RELEASE PAD _BROWSE OF _MSYSMENU
ON KEY LABEL ALT+B
*B800271,1 Reset keys upon entry.
*=lfResetTrap()
PUSH KEY
ON KEY
*B800271,1 end.

SELECT(lcTempInv)
ZAP

*** Refresh relation between payments file and AP distribution file
SELECT APPAYMNT
IF !EOF()
  GO RECNO()
ENDIF  

SELECT APDIST

SCAN REST WHILE CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF = ;
                APPAYMNT.CPAYMETH+APPAYMNT.CBNKCODE+  ;
                APPAYMNT.CCHKACCT+APPAYMNT.CPAYDOCNO
  IF lcInvoice <> APDIST.CINVNO
    SELECT APINVHDR
    SCATTER MEMVAR MEMO

    SELECT(lcTempInv)
    APPEND BLANK
    GATHER MEMVAR MEMO
    lcInvoice = APDIST.CINVNO
  ENDIF

  SELECT(lcTempInv)
  
  DO CASE
    CASE APDIST.CAPDACTID = 'A'
      REPLACE NTOTAL    WITH APDIST.NAPDAMNT

    CASE APDIST.CAPDACTID = 'B'
      REPLACE N1099     WITH APDIST.NAPDAMNT

    CASE APDIST.CAPDACTID = 'C'
      REPLACE NPAID     WITH APDIST.NAPDAMNT

    CASE APDIST.CAPDACTID = 'J'
      REPLACE NADJUST   WITH APDIST.NAPDAMNT

    CASE APDIST.CAPDACTID = 'S'
      REPLACE NDISCOUNT WITH APDIST.NAPDAMNT
  ENDCASE
  SELECT APDIST
ENDSCAN

DO CASE
  CASE APPAYMNT.CPAYMETH = 'P'
    lcPayment = 'Printed checks'

  CASE APPAYMNT.CPAYMETH = 'M'
    lcPayment = 'Manual checks'

  CASE APPAYMNT.CPAYMETH = 'N'
    lcPayment = 'Non check payments'

  OTHERWISE  
    lcPayment = 'Cash payment'
ENDCASE

lcInvTtl = 'Invoice paid by '+lcPayment+' number '+APPAYMNT.CPAYDOCNO

lcInvTtl = ALLTRIM(lcInvTtl)

SELECT(lcTempInv)
GO TOP

=lfStInvTrp()


*E300683,1 Call *.SPR from screens directory
*DO APVDINV.SPR
DO (gcScrDir + gcWinAppl + '\APVDINV.SPR')
*E300683,1 end

*B800271,1 Remarked.
*=lfRsInvTrp()
*RELEASE PAD _BROWSE OF _MSYSMENU
*ON KEY LABEL ALT+B
*=lfSetTrap()
*IF _DOS
*  =lfDefinePad()
*ENDIF  
POP KEY
*B800271,1 end.

SELECT APPAYMNT

*!**************************************************************************
*!
*!      FUNCTION : lfBrwInv
*!
*!**************************************************************************
* 
FUNCTION lfBrwInv
PRIVATE lcClrSchm

lcClrSchm = IIF(_DOS," COLOR SCHEME 13","NOMENU")

SELECT(lcTempInv)

lcBrwStr  = "CINVNO :R :H='Inv. no.',"+;
            "DINVDATE :R :H='Inv. date',"+;
            "CINVREF :R :H='Reference',"+;
            "CINVREMIT = IIF(CINVREMIT = 'V','Vendor',IIF(CINVREMIT = 'F','Factor','Other')) :6 :R :H='Remit',"+;
            "CVENPMETH = IIF(CVENPMETH = 'P',"+;
            "'Printed checks',IIF(CVENPMETH = 'M',"+;
            "'Manual checks',IIF(CVENPMETH = 'N',"+;
            "'Non check payments','Cash payment'))) :R :H='Payment method',"+;
            "NINVDISOF :R :H='Disc. Offerd',"+;
            "NINVAMTAP :R :H='Appr. to pay',"+;
            "NINVDISAP :R :H='Disc. appr.'"
            
*E300258,4 MAN Add the check on supressing 1099 processing           
*E301077,79 AMM Adjust browse fields variable
*lcBrwStr1 = "NINVADJAP :R :H='Adj. appr.',"+;
            "CVENPRIOR :R :H='Payment priority',"+;
            "NTERDUED  :R :H='Net due days',"+;
            "NTERDISCD :R :H='Disc. days',"+;
            "NTERDISCR :R :H='Disc. percent',"+;
            "DINVDUDAT :R :H='Due date',"+;
            "CBNKCODE  :R :H='Bank code',"+;
            "CCHKACCT  :R :H='Bank checking account',"+;
            "CCHKGLACC :R :H='GL checking account',"+;
            "CCHKNO    :R :H='Check number',"+;
            "DCHKDATE  :R :H='Check date',"+;
            "CVENCCVEN :R :H='Credit card vendor',"+;
            "CVENCCINV :R :H='Credit card invoice',"+;
            "CAPACCT   :R :H='AP account',"+;
            "NTOTAL    :R :H='Total',"+;
            IIF(llApS1099,"", "N1099     :R :H='1099 amount',")+;
            "NPAID     :R :H='Amount paid',"+;
            "NADJUST   :R :H='Adj. applied',"+;
            "NDISCOUNT :R :H='Disc. taken',"+;
            "NINVPAID  :R :H='Total paid',"+;
            "NINVDISTK :R :H='Total Discount',"+;
            "NINVADJ   :R :H='Total Adjustment',"+;
            IIF(llApS1099,"", "NINV1099A :R :H='Total 1099 amount'")
lcBrwStr1 = "NINVADJAP :R :H='Adj. appr.',"+;
            "CVENPRIOR :R :H='Payment priority',"+;
            "NTERDUED  :R :H='Net due days',"+;
            "NTERDISCD :R :H='Disc. days',"+;
            "NTERDISCR :R :H='Disc. percent',"+;
            "DINVDUDAT :R :H='Due date',"+;
            "CBNKCODE  :R :H='Bank code',"+;
            "CCHKACCT  :R :H='Bank checking account',"+;
            "CCHKGLACC :R :H='GL checking account',"+;
            "CCHKNO    :R :H='Check number',"+;
            "DCHKDATE  :R :H='Check date',"+;
            "CVENCCVEN :R :H='Credit card vendor',"+;
            "CVENCCINV :R :H='Credit card invoice',"+;
            "CAPACCT   :R :H='AP account',"+;
            "NTOTAL    :R :H='Total',"+;
            IIF(llApS1099,"", "N1099     :R :H='1099 amount',")+;
            "NPAID     :R :H='Amount paid',"+;
            "NADJUST   :R :H='Adj. applied',"+;
            "NDISCOUNT :R :H='Disc. taken',"+;
            "NINVPAID  :R :H='Total paid',"+;
            "NINVDISTK :R :H='Total Discount',"+;
            "NINVADJ   :R :H='Total Adjustment'"+;
            IIF(llApS1099,"", ",NINV1099A :R :H='Total 1099 amount'")
*E301077,79 AMM end

BROWSE FIELDS &LcBrwStr+&lcBrwStr1;
              WINDOW APVDINV1 ;
              WHEN lfwBrwInv();
              VALID :F (WONTOP() = lcInvTtl .AND. lfwBrwInv()) .OR. gfStopBrow() ;
              IN WINDOW AWDAPVDINV;
              LOCK 0;
              NOAPPEND;
              NOCLEAR;
              NODELETE;
              NOWAIT;
              SAVE;
              TITLE lcInvTtl;
              NOEDIT &lcClrSchm

*!**************************************************************************
*!
*!      Function : lfwBrwInv
*!
*!**************************************************************************
*
FUNCTION lfwBrwInv

glFromBrow = .T.

RELEASE PAD _BROWSE OF _MSYSMENU
ON KEY LABEL ALT+B

=lfStInvTrp()

=lfRefresh()

*!**************************************************************************
*!
*!      Function : lfvBrwInv
*!
*!**************************************************************************
*
FUNCTION lfvBrwInv

=lfRsInvTrp()

*!**************************************************************************
*!
*!      FUNCTION : lfwVenDis
*!
*!**************************************************************************
* 
FUNCTION lfwVenDis

IF _DOS
  =lfDefinePad()
ENDIF  

*!**************************************************************************
*!
*!      FUNCTION : lfvVenDis
*!
*!**************************************************************************
* 
FUNCTION lfvVenDis

SELECT APPAYMNT
IF !EOF()
  GO RECNO()
ENDIF  

RELEASE PAD _BROWSE OF _MSYSMENU
ON KEY LABEL ALT+B

=lfRefresh()

=gfActWind('CWRAPVDVEN',lcTVendTtl,gcBaseWind)

*!**************************************************************************
*!
*!      FUNCTION : lfvClose
*!
*!**************************************************************************
* 
FUNCTION lfvClose

glQuitting = .T.

CLEAR READ

*!**************************************************************************
*!
*!      Function: lfActBrows
*!
*!**************************************************************************
*
FUNCTION lfActBrows

*B800271,1 Activate browse window only if the void payment screen is on top.
IF WPARENT(WOUTPUT()) = gcBaseWind
*B800271,1 end.
  RELEASE PAD _BROWSE OF _MSYSMENU

  ON KEY LABEL ALT+B

  ACTIVATE WINDOW (lcBrowsTtl)
  
*B800271,1 Add an ENDIF for the above condition.
ENDIF  
*B800271,1 end

*!**************************************************************************
*!
*!      Function: lfDefinePad
*!
*!**************************************************************************
*
FUNCTION lfDefinePad

DEFINE PAD _BROWSE OF _MSYSMENU PROMPT '\<Browse' 
ON KEY LABEL ALT+B DO lfActBrows
ON SELECTION PAD _BROWSE OF _msysmenu DO lfActBrows

*!**************************************************************************
*!
*!      Function: lfSetTrap
*!
*!**************************************************************************
*
FUNCTION lfSetTrap

ON KEY LABEL ALT+O      DO lfKeyTrap WITH LASTKEY()
ON KEY LABEL ALT+V      DO lfKeyTrap WITH LASTKEY() 
ON KEY LABEL ALT+S      DO lfKeyTrap WITH LASTKEY()
ON KEY LABEL ALT+A      DO lfKeyTrap WITH LASTKEY()
ON KEY LABEL ALT+I      DO lfKeyTrap WITH LASTKEY()
ON KEY LABEL ESC        DO lfKeyTrap WITH LASTKEY()
ON KEY LABEL Ctrl+ENTER DO lfKeyTrap WITH LASTKEY()

*B800271,1 Trap Tabs upon entry of the browse.
ON KEY LABEL TAB        DO lpTrapTab WITH lcMain3, OBJNUM(pbVoid)
ON KEY LABEL BACKTAB    DO lpTrapTab WITH lcMain2, OBJNUM(pbInvoice)
*B800271,1 end.

*!**************************************************************************
*!
*!      Function: lfResetTrap
*!
*!**************************************************************************
*
FUNCTION lfResetTrap

IF _DOS
  ON KEY LABEL ESC
ELSE  
  ON KEY LABEL ESC DO lfEscap  
ENDIF

ON KEY LABEL ALT+O
ON KEY LABEL ALT+V
ON KEY LABEL ALT+S
ON KEY LABEL ALT+A
ON KEY LABEL ALT+I
ON KEY LABEL Ctrl+ENTER

*B800271,1 Release tabsupon exit from the browse.
ON KEY LABEL TAB        
ON KEY LABEL BACKTAB    
*B800271,1 end.



*!**************************************************************************
*!
*!      Procedure:  lpTab
*!
*!**************************************************************************
*
*
*B800271,1 This procedure is not used anymore.
*PROCEDURE lpTab
*ON KEY LABEL TAB lnDummy = 1

*IF _DOS
*  HIDE MENU _MSYSMENU
*  SHOW MENU _MSYSMENU
*ENDIF      

*DO CASE
*  CASE WONTOP(lcMain2)
*    IF _CUROBJ = OBJNUM(pbInvoice)
*      ACTIVATE WINDOW (lcBrowsTtl)      
*    ELSE
*      _CUROBJ  = _CUROBJ + 1 
*    ENDIF

*  CASE WONTOP(lcBrowsTtl)
*    ACTIVATE WINDOW (lcMain3)
*    _CUROBJ = OBJNUM(pbVoid)
  
*  OTHERWISE
*    ON KEY LABEL TAB
*    KEYBOARD "{TAB}" PLAIN
*ENDCASE

*ON KEY LABEL TAB DO lpTab

*!**************************************************************************
*!
*!      Procedure:  lpShiftTab
*!
*!**************************************************************************
*
*B800271,1 This procedure is not used anymore.
*PROCEDURE lpShiftTab

*ON KEY LABEL BACKTAB lnDummy = 1

*IF _DOS
*  HIDE MENU _MSYSMENU
*  SHOW MENU _MSYSMENU
*ENDIF      

*DO CASE
*  CASE WONTOP(lcBrowsTtl)
*    ACTIVATE WINDOW (lcMain2)
*    _CUROBJ = OBJNUM(pbInvoice)
    
*  CASE WONTOP(lcMain2)
*   IF _CUROBJ = OBJNUM(rbScope)
*      ACTIVATE WINDOW (lcMain3)
*      _CUROBJ  = OBJNUM(pbClose)
*    ELSE
*      _CUROBJ = _CUROBJ - 1  
*    ENDIF

*  CASE WONTOP(lcMain3)
*    IF _CUROBJ = OBJNUM(pbVoid)
*      ACTIVATE WINDOW (lcBrowsTtl)
*    ELSE
*      _CUROBJ  = _CUROBJ - 1  
*    ENDIF
*ENDCASE  

*ON KEY LABEL BACKTAB DO lpShiftTab

*!**************************************************************************
*!
*!      Function: lfRelease
*!
*!**************************************************************************
*
FUNCTION lfRelease
IF WONTOP() = lcBrowsTtl
  *B800271,1 Set glFromBrow and trap keys upon entry of the browse. 
  glFromBrow = .T.
  =lfSetTrap()
ENDIF  
*B800271,1 Remarked the following
*  ON KEY LABEL ESC        DO lfKeyTrap WITH LASTKEY()
*  ON KEY LABEL ALT+O      DO lfKeyTrap WITH LASTKEY()
*   ON KEY LABEL ALT+V      DO lfKeyTrap WITH LASTKEY()
*   ON KEY LABEL ALT+S      DO lfKeyTrap WITH LASTKEY()
*   ON KEY LABEL ALT+A      DO lfKeyTrap WITH LASTKEY()
*   ON KEY LABEL ALT+I      DO lfKeyTrap WITH LASTKEY()
*   ON KEY LABEL Ctrl+ENTER DO lfKeyTrap WITH LASTKEY()
*ELSE
*  IF _DOS
*    ON KEY LABEL ESC
*  ELSE
*    ON KEY LABEL ESC DO lfEscap  
*  ENDIF  
*  ON KEY LABEL ALT+O
*  ON KEY LABEL ALT+V
*  ON KEY LABEL ALT+S
*  ON KEY LABEL ALT+A
*  ON KEY LABEL ALT+I
*  ON KEY LABEL Ctrl+ENTER
*ENDIF
*B800271,1 END.end.

IF WONTOP() = lcBrowsTtl .OR. WPARENT(WOUTPUT()) <> gcBaseWind
  RELEASE PAD _BROWSE OF _MSYSMENU
  ON KEY LABEL ALT+B
ENDIF


*!**************************************************************************
*!
*!      Function: lfRelChld
*!
*!**************************************************************************
*
FUNCTION lfRelChld

IF WONTOP() = lcInvTtl
  ON KEY LABEL ESC
ELSE
  IF _DOS
    ON KEY LABEL ESC
  ELSE
    ON KEY LABEL ESC
  ENDIF  
ENDIF

IF WONTOP() = lcInvTtl .OR. WPARENT(WOUTPUT()) <> 'AWDAPVDINV'
  RELEASE PAD _BROWSE OF _MSYSMENU
  ON KEY LABEL ALT+B
ENDIF

*!**************************************************************************
*!
*!      Function: lfStInvTrp
*!
*!**************************************************************************
*
FUNCTION lfStInvTrp

ON KEY LABEL ESC
ON KEY LABEL TAB     DO lpInvTab
ON KEY LABEL BACKTAB DO lpInvTab

*!**************************************************************************
*!
*!      Function: lfRsInvTrp
*!
*!**************************************************************************
*
FUNCTION lfRsInvTrp
ON KEY LABEL ESC
ON KEY LABEL TAB        
ON KEY LABEL BACKTAB    

*!**************************************************************************
*!
*!      Procedure:  lpInvTab
*!
*!**************************************************************************
*
PROCEDURE lpInvTab

IF WONTOP(lcInvTtl)
  ACTIVATE WINDOW 'APVDINV2'
  _CUROBJ = OBJNUM(pbClsInv)
ELSE
  RELEASE PAD _BROWSE OF _MSYSMENU
  ON KEY LABEL ALT+B
  ACTIVATE WINDOW (lcInvTtl)
ENDIF

*!**************************************************************************
*!
*!      Function: lfwClsInv
*!
*!**************************************************************************
*
FUNCTION lfwClsInv
=lfStInvTrp()

*!**************************************************************************
*!
*!      FUNCTION : lfwVoid
*!
*!**************************************************************************
* 
*B800271,1 Not used anymore
*FUNCTION lfwVoid

*IF _DOS
*  =lfDefinePad()
*ENDIF  

*!**************************************************************************
*!
*!      FUNCTION : lfvVoid
*!
*!**************************************************************************
* 
FUNCTION lfvVoid

SELECT APPAYMNT

llObjLock = .F.

*E300292,1 M.Hassam 09/14/95 Add a warning message before void any payment.
*Begin M.Hassan
IF gfModalGen("QRM04153B00006","ALERT",ALLTRIM(APPAYMNT.CPAYDOCNO)) = 2
  ** Message : "Are you sure you want to void payment # ð?"
  **           "          ® Yes ¯    ® No ¯                "
  RETURN 
ENDIF
*End M.Hassan

*B800271,1 Check that the void date is not empty.
IF EMPTY(ldVoidDate)
  *B800271,1 If the void date is empty, present a message and return.
  *B800271,1 Message: " You have to enter the ð.  "
  *B800271,1 Choices: "            ® Ok ¯         "
  =gfModalGen("TRM04066B00000", "DIALOG", lcTVoidDate)
  _CUROBJ = OBJNUM(ldVoidDate)
  RETURN 
ENDIF

IF ldVoidDate < APPAYMNT.DPAYDATE
  *B800271,1 If the entered value is not greater than zero,
  *B800271,1 Message :  "   ð should be greater than ð.   "
  *B800271,1                 ®   OK   ¯
  =gfModalGen("TRM04072B00000","DIALOG",lcTVoidDat+"|"+ lcTPayDate)
  _CUROBJ = OBJNUM(ldVoidDate)
  RETURN 
ENDIF
*B800271,1 end.

*B800271,1 Get the year-period of the void date
*IF !lfVlDate(gcPrnt_Cmp,@lcFiscalP,@lcFiscalY,APPAYMNT.DPAYDATE)

*B608717,1 WAM 10/08/2008 Don't validate original payment date 
*B608717,1 WAM 10/08/2008 Following lines are commented out

*IF !lfVlDate(gcPrnt_Cmp, '', '',APPAYMNT.DPAYDATE)
*B800271,1 end.
  ** Message : "You cannot void an invoice that    "
  **           "has date out of the posting window."
  **                       ® Ok ¯                  "
*  =gfModalGen("TRM04062B00000","DIALOG")
*  RETURN 
*ENDIF
*B608717,1 WAM 10/08/2008 (End)

*B603958,1 KAM(START)
llLockPrd = .F.
IF !lfVlDate(gcPrnt_Cmp,@lcFiscalP,@lcFiscalY,ldVoidDate,@llLockPrd)   
   =gfModalGen('TRM04113B00000', 'DIALOG', lcTVoid)
    _CUROBJ    = OBJNUM(ldVoidDate)
    RETURN
ENDIF   
IF llLockPrd 
  =gfModalGen("QRM01258B00000" , "DIALOG" )
  RETURN  
ENDIF  

*B603958,1 KAM(END)


*B601994,1 Check if the Payment to be void is an advance payment and it
*has already been paid and if so give the user a message and cancel
*the Voiding process [Begin]

SELECT APPAYMNT

*B601994,1 If the Payment to be void is an advance payment and it
*has already been paid
IF APPAYMNT.lPayAdvan .AND. ;
   (APINVHDR.nInvPaid + APINVHDR.nInvDisTk + APINVHDR.nInvAdj) <> 0 ;
   .AND. lfPayment()
  
  ** Message : "Advance payment ð is already paid on one or more payments."
  **           " Cannot void that advance payment.                        "
  **           "                          ® Ok ¯                          "
  =gfModalGen("TRM04164B00000" , "DIALOG" , ALLTRIM(APPAYMNT.cPayDocNo))
  
  RETURN
  
ENDIF    && End of IF APPAYMNT.lPayAdvan .AND. ...... .AND. lfPayment()
*B601994,1 Add these lines to check if the Payment to be void [End]

*B601060,1 Lock the payment record
SELECT APPAYMNT
*B132275,1  TMI [Start] use local function instead, with 'Retry', 'Cancel' buttons
*IF gfObj_Lock(.T.)
lcLokedRecs = ' ' && save locked records in APINVHDR file
IF lfObj_Lock(.T.)
*B132275,1  TMI [End  ] 

*B601060,1 end.

  SELECT APDIST
  *B608959,1 TMI 08/04/2009 [START] add a check that the line is not voided
  *SCAN REST WHILE APDIST.CAPDTRTYP+APDIST.CBNKCODE+;
                  APDIST.CCHKACCT +APDIST.CAPDREF = ;
                  APPAYMNT.CPAYMETH+APPAYMNT.CBNKCODE+;
                  APPAYMNT.CCHKACCT+APPAYMNT.CPAYDOCNO;
              FOR APDIST.CAPDACTID = 'A'
  SCAN REST WHILE APDIST.CAPDTRTYP+APDIST.CBNKCODE+;
                  APDIST.CCHKACCT +APDIST.CAPDREF = ;
                  APPAYMNT.CPAYMETH+APPAYMNT.CBNKCODE+;
                  APPAYMNT.CCHKACCT+APPAYMNT.CPAYDOCNO;
              FOR APDIST.CAPDACTID = 'A' AND APDIST.CAPDSTAT <> 'V'
              *B608959,1 TMI 08/04/2009 [END] add a check that the line is not voided
    SELECT APINVHDR
    *B132275,1  TMI [Start] use local function instead, with 'Retry', 'Cancel' buttons
    *IF !gfObj_Lock(.T.)    
    IF !lfObj_Lock(.T.)
      *B132275,1  TMI [End  ] 
      ** Message : "Invoice ð for vendor ð is being edited"
      **           "by user ð. Payment cannot be voided.  "
      **                         ® Ok ¯                   "
      =gfModalGen("TRM04100B00000","DIALOG",ALLTRIM(CINVNO)+"|"+ALLTRIM(CVENDCODE)+"|"+ALLTRIM(LOOKUP(SYUUSER.CUSR_NAME,CLOK_USER,SYUUSER.CUSER_ID,'CUSER_ID')))
      llObjLock = .T.
      EXIT
    ENDIF
    *B132275,1  TMI [Start] save locked records in APINVHDR file
    lcLokedRecs = lcLokedRecs + STR(RECNO('APINVHDR')) + '|'
    *B132275,1  TMI [End  ] 
    SELECT APDIST
  ENDSCAN  

  IF llObjLock
    SELECT APPAYMNT
    IF !EOF()
      GO RECNO()
    ENDIF  
    *B601060,1 UnLock the payment record
    =gfObj_Lock(.F.)
    *B601060,1 end.

    SELECT APDIST
    SCAN REST WHILE APDIST.CAPDTRTYP+APDIST.CBNKCODE+;
                    APDIST.CCHKACCT +APDIST.CAPDREF = ;
                    APPAYMNT.CPAYMETH+APPAYMNT.CBNKCODE+;
                    APPAYMNT.CCHKACCT+APPAYMNT.CPAYDOCNO;
                FOR APDIST.CAPDACTID = 'A'
      SELECT APINVHDR          
      *B132275,1  TMI [Start] Clear only APINVHDR records locked by this program 
      IF STR(RECNO('APINVHDR')) $ lcLokedRecs
        *B132275,1  TMI [End  ] 
        =gfObj_Lock(.F.)
        *B132275,1  TMI [Start] 
      ENDIF
      *B132275,1  TMI [End  ] 
      SELECT APDIST
    ENDSCAN  
    llObjLock = .F.
    RETURN
  ENDIF

  SELECT APPAYMNT
  IF !EOF()
    GO RECNO()
  ENDIF  

  *B601060,1 Scan through APDIST file for all invoices that may have 
  *B601060,1 been applied by the current debit memo.
  *B601060,1 An application record has cApDTrTyp = 'A'
  *B601060,1 Save environment
  SELECT APDIST
  lnApDistTag = VAL(SYS(21))
  *B601060,1 Store the vendor code and invoice number.
  lcVdVend   = APDIST.cVendCode
  lcVdDM     = APDIST.cInvNo 
 
  *B601060,1 INVVEND Tag expression is : CINVNO+CVENDCODE+CAPDTRTYP
  *B601060,1 Scan only negative records.
  SET ORDER TO TAG INVVEND
  IF SEEK(lcVdDM + lcVdVend + 'A')
    *B132275,1  TMI [Start] save locked records of APINVHDR file
    lcLokedRecs = ' '
    *B132275,1  TMI [End  ] 
    SCAN REST WHILE   cInvNo +   cVendCode + cAPDTrTyp = lcVdDM + lcVdVend + 'A';
              FOR nAPDAmnt < 0 .AND. cAPDActID = 'A'      
      IF SEEK(APDIST.cAPDRef + lcVdVend, 'APINVHDR')
        SELECT APINVHDR
        *B132275,1  TMI [Start] use local function instead, with 'Retry', 'Cancel' buttons
        *IF !gfObj_Lock(.T.)
        IF !lfObj_Lock(.T.)
          *B132275,1  TMI [End  ] 
          ** Message : "Invoice ð for vendor ð is being edited"
          **           "by user ð. Payment cannot be voided.  "
          **                         ® Ok ¯                   "
          =gfModalGen("TRM04100B00000","DIALOG",;
                       ALLTRIM(APINVHDR.cInvNo)+"|"+;
                       ALLTRIM(APINVHDR.CVENDCODE)+"|"+;
                       ALLTRIM(LOOKUP(SYUUSER.CUSR_NAME,APINVHDR.CLOK_USER,SYUUSER.CUSER_ID,'CUSER_ID')))
          llObjLock = .T.
          EXIT
        ENDIF
        *B132275,1  TMI [Start] save locked records of the APINVHDR file
        lcLokedRecs = lcLokedRecs + STR(RECNO('APINVHDR')) + '|'
        *B132275,1  TMI [End  ] 
        SELECT APDIST               
      ENDIF
    ENDSCAN
        
    *B601060,1 If at least one record is not locked, unlock all records.
    IF llObjLock .AND. SEEK(lcVdDM + lcVdVend + 'A')
      SCAN REST WHILE   cInvNo +   cVendCode + cAPDTrTyp = ;
                       lcVdDM + lcVdVend + 'A';
       FOR nAPDAmnt < 0 .AND. cAPDActID = 'A'      
        IF SEEK(APDIST.cAPDRef + lcVdVend, 'APINVHDR')
          SELECT APINVHDR
          *B132275,1  TMI [Start] Clear only records locked by this screen , not locked by other screens
          IF STR(RECNO('APINVHDR')) $ lcLokedRecs
            *B132275,1  TMI [End  ] 
            =gfObj_Lock(.F.)
            *B132275,1  TMI [Start] 
          ENDIF            
          *B132275,1  TMI [End  ] 
          SELECT APDIST               
        ENDIF
      ENDSCAN
 
      *B601060,1 UnLock the payment record
      SELECT APPAYMNT
      =gfObj_Lock(.F.)
      RETURN
    ENDIF
  ENDIF  
  *B601060,1 Restore environment
  SELECT APDIST
  SET ORDER TO (lnApDistTag)
  *B601060,1 end.
     
  *B601060,1 Refresh relation
  SELECT APPAYMNT
  IF !EOF()
    GO RECNO()
  ENDIF  
 *B601060,1 end.

  *B601994,1 Change this If condition to void the debit memo record
  *only if the payment to be void is an advance payment [Begin]
  
  *IF APINVHDR.CINVSTAT = 'A'
  
  *B601994,1 If the payment to be void is an advance payment
  IF APPAYMNT.lPayAdvan
  
  *B601994,1 Change this If condition to void the debit memo record [End]
    
    llAdvPay = .T.
    REPLACE APINVHDR.CINVSTAT  WITH 'V',;
            APINVHDR.LLOK_STAT WITH .F.,;
            APINVHDR.CLOK_USER WITH ' ',;
            APINVHDR.DLOK_DATE WITH {} ,;
            APINVHDR.CLOK_TIME WITH ' '
  ELSE
    llAdvPay = .F.
  ENDIF
  
  SELECT APDIST

  SCAN REST WHILE APDIST.CAPDTRTYP+APDIST.CBNKCODE+;
                  APDIST.CCHKACCT +APDIST.CAPDREF = ;
                  APPAYMNT.CPAYMETH+APPAYMNT.CBNKCODE+;
                  APPAYMNT.CCHKACCT+APPAYMNT.CPAYDOCNO;
              FOR APDIST.CAPDSTAT <> 'V'
    DO CASE
      CASE APDIST.CAPDACTID = 'A'

        SCATTER MEMVAR MEMO
        *B600995,1 HESHAM EL-SHELTAWI (START)
        *B600995,1 changing the status of the existing recort to 'V'
        REPLACE cApdStat WITH 'V'
        *B600995,1 (End)
      
        m.lApdPost  = .F.
        m.cApdStat  = 'V'
        m.nApdAmnt  = 0 - m.nApdAmnt
        m.cBatchNo  = ' '
        m.cTrnSleDn = ' '
        m.cFisFYear = lcFiscalY
        m.cFsPPrdId = lcFiscalP
        m.cApSessNo = lcSequence
      
        *E300296,4 Add a variable for nEqvAmnt field
        m.nEqvAmnt  = 0 - m.nEqvAmnt
        *E300296,4 end.

        *B800271,1 Use the void date instead of the payment date.      
        m.dAPDTrDat = ldVoidDate
        *B800271,1 end.
      
        SELECT(lcApDist)
        APPEND BLANK
        GATHER MEMVAR MEMO

        =gfAdd_Info()

        SELECT APDIST
      
        *B601060,1 Scan through APDIST file for all invoices that may have 
        *B601060,1 been applied by the current debit memo.
        *B601060,1 An application record has cApDTrTyp = 'A'
        *B601060,1 Save environment
        lnApDistRec = RECNO()
        lnApDistTag = VAL(SYS(21))
        *B601060,1 INVVEND Tag expression is : CINVNO+CVENDCODE+CAPDTRTYP
        *B601060,1 Scan only negative records.
        SET ORDER TO TAG INVVEND
        
        *B601994,1 Change this If condition to make the update only
        *if the payment to be void is an advance payment [Begin]
        
        *IF SEEK(m.cInvNo + m.cVendCode + 'A')
        
        *B601994,1 If the payment to be void is an advance payment and
        *there is some application record for the current debit memo
        *(The Advance payment Debit memo)
        IF llAdvPay .AND. SEEK(m.cInvNo + m.cVendCode + 'A')
        
        *B601994,1 Change this If condition [End]
          
          *B601994,1 Change this line to change the Scan loop for condition
          *to void all the application record for the current debit
          *memo [Begin]
          
          *SCAN REST WHILE   cInvNo +   cVendCode + cAPDTrTyp = ;
          *                m.cInvNo + m.cVendCode + 'A';
          *     FOR nAPDAmnt < 0 .AND. cAPDActID = 'A'      
          
          SCAN REST ;
              WHILE cInvNo +   cVendCode + cAPDTrTyp = ;
                    m.cInvNo + m.cVendCode + 'A' ;
                FOR cApdStat <> 'V'
          
          *B601994,1 Change this line to change the Scan loop for [End]
            
            *B601060,1 Update Vendor open debit here
            
            *B601994,1 Add this if condition for we have remove it from
            *the Scan loop for condition [Begin]
            IF nAPDAmnt < 0
            *B601994,1 Add this if condition for we have remove it [End]
              
              *B601994,1 Remove this if condition for we don't need it any more
              *because we have added this condition in a higher level [Begin]
              *IF llAdvPay
              *B601994,1 Remove this if condition for we don't need it [End]
                
              *B601994,1 Add this if condition for we have remove it from
              *the Scan loop for condition [Begin]
              IF cAPDActID = 'A'
              *B601994,1 Add this if condition for we have remove it [End]
                
                *B601060,1  Add the applied amount to the vendor's open debit.
                REPLACE APVENDOR.nVenOpnDr WITH APVENDOR.nVenOpnDr ;
                              + ABS(APDIST.nEqvAmnt)
                *B601060,1 Subtract applied amount from APVENHST.nVnHDMApp
                REPLACE APVENHST.nVnHDMApp WITH APVENHST.nVnHDMApp + ;
                               APDIST.nEqvAmnt
                
              *B601994,1 Add this if condition for we have remove it from
              *the Scan loop for condition [Begin]
              ENDIF    && End of IF cAPDActID = 'A'
              *B601994,1 Add this if condition for we have remove it [End]
              
              *B601994,1 Add these lines to clear the discount from the
              *APVENDOR and the APVENHST files if there is any [Begin]
              
              *B601994,1 If this is a discount record
              IF cAPDActID = 'S'
                SELECT APVENDOR
                REPLACE nVenBal   WITH nVenBal - APDIST.nApdAmnt ,;
                        nVenOpnDr WITH nVenOpnDr +;
                                       IIF(APINVHDR.nInvAmnt < 0 ,;
                                           APDIST.nApdAmnt , 0)
                
                SELECT APVENHST
                REPLACE nVnhDisTkn WITH nVnhDisTkn + APDIST.nApdAmnt
              
              ENDIF    && End of IF cAPDActID = 'S'
              
              =gfAdd_Info('APVENDOR')
              =gfAdd_Info('APVENHST')
              
              *B601994,1 Add these lines to clear the discount from [End]
              
              *B601994,1 Remove this if condition for we don't need it any more
              *because we have added this condition in a higher level [Begin]
              *ENDIF                
              *B601994,1 Remove this if condition for we don't need it [End]
              
              *B601060,1 end.
              
              IF SEEK(APDIST.cAPDRef + m.cVendCode, 'APINVHDR')
                SELECT APINVHDR
                
                *B601994,1 Add this if condition for we have remove it from
                *the Scan loop for condition [Begin]
                IF APDIST.cAPDActID = 'A'
                *B601994,1 Add this if condition for we have remove it [End]
                  
                  *B601060,1 Deduct the applied amount from the invoice
                  *B601060,1 (APDIST.nAPDAmnt is negative)
                  REPLACE nInvAdj WITH nInvAdj + APDIST.nAPDAmnt
                  
                *B601994,1 Add this if condition for we have remove it from
                *the Scan loop for condition [Begin]
                ENDIF    && End of IF cAPDActID = 'A'
                *B601994,1 Add this if condition for we have remove it [End]
                
                *B601994,1 Add these lines to clear the discount from the
                *APINVHDR file if there is any [Begin]
                
                *B601994,1 If this is a discount record
                IF APDIST.cAPDActID = 'S'
                  REPLACE nInvDisTk WITH nInvDisTk + APDIST.nAPDAmnt
                ENDIF    && End of IF cAPDActID = 'S'
                
                *B601994,1 Add these lines to clear the discount from [End]
                
                =gfAdd_Info()
                =gfObj_Lock(.F.)
                SELECT APDIST               
              ENDIF
                
            *B601994,1 Add this if condition for we have remove it from
            *the Scan loop for condition [Begin]
            ENDIF    && End of IF nAPDAmnt < 0
            *B601994,1 Add this if condition for we have remove it [End]
            
            *B601994,1 Add these lines to void the Application records of
            *the Advance payment Debit memo [Begin]
            
            REPLACE cApdStat WITH 'V'
            SCATTER MEMVAR MEMO
            
            m.lApdPost  = .F.
            m.nApdAmnt  = 0 - m.nApdAmnt
            m.cBatchNo  = ' '
            m.cTrnSleDn = ' '
            m.cFisFYear = lcFiscalY
            m.cFsPPrdId = lcFiscalP
            m.cApSessNo = lcSequence
            m.dAPDTrDat = ldVoidDate
            
            SELECT(lcApDist)
            APPEND BLANK
            GATHER MEMVAR MEMO
            
            =gfAdd_Info()
            
            SELECT APDIST
            
            *B601994,1 Add these lines to void the Application records [End]
            
          ENDSCAN
        ENDIF  
        *B601060,1 Restore environment
        SET ORDER TO (lnApDistTag)
        GO lnApDistRec
        *B601060,1 end.

        *B601528,1 Add this lines to use the records with cApdActId A and S
        *          and J [Begin]
        *B601528,1 IF Statment to check that the record pointer is not at
        *          the end of file in the file APINVHDR.DBF  
        IF !EOF('APINVHDR')
          *B601528,1 IF Statment to check if the payment is an advanced payment
          IF llAdvPay
            REPLACE APINVHDR.NINVPAID WITH 0
          ELSE    && Else
            REPLACE APINVHDR.NINVPAID WITH APINVHDR.NINVPAID - APDIST.NAPDAMNT
          ENDIF   && End of IF
          =gfAdd_Info('APINVHDR')
        ENDIF   && End of IF
        *B601528,1 Add this lines [End]
        
      CASE APDIST.CAPDACTID = 'B'

        *E300296,4 Update Invoice 1099A amount with APDIST.nAPDAmnt,
        *E300296,4 (same currency as that of the invoice), but,
        *E300296,4 update Vendor fields with APDIST.nEqvAmnt 
        *E300296,4 (in base currency), not the amount field.
        *REPLACE APINVHDR.NINV1099A WITH APINVHDR.NINV1099A + APDIST.NAPDAMNT,;
                APVENDOR.NINV1099B WITH APVENDOR.NINV1099B - APDIST.NAPDAMNT
        REPLACE APINVHDR.NINV1099A WITH APINVHDR.NINV1099A + APDIST.NAPDAMNT,;
                APVENDOR.NINV1099B WITH APVENDOR.NINV1099B - APDIST.nEqvAmnt
        *E300296,4 end.
      
      CASE APDIST.CAPDACTID = 'C'
 
        SCATTER MEMVAR MEMO
        *B600995,1 HESHAM EL-SHELTAWI (START)
        *B600995,1 changing the status of the existing recort to 'V'
        REPLACE cApdStat WITH 'V'
        *B600995,1 (End)
      
        m.lApdPost  = .F.
        m.cApdStat  = 'V'
        m.nApdAmnt  = 0 - m.nApdAmnt
        m.cBatchNo  = ' '
        m.cTrnSleDn = ' '
        m.cFisFYear = lcFiscalY
        m.cFsPPrdId = lcFiscalP
        m.cApSessNo = lcSequence
      
        *E300296,4 Add a variable for nEqvAmnt field
        m.nEqvAmnt  = 0 - m.nEqvAmnt
        *E300296,4 end.

        *B800271,1 Use the void date instead of the payment date.      
        m.dAPDTrDat = ldVoidDate
        *B800271,1 end.

        SELECT(lcApDist)
        APPEND BLANK
        GATHER MEMVAR MEMO

        =gfAdd_Info()
      
        SELECT APDIST
      
        *B601528,1 Remove this lines (To use the records with cApdActId A
        *          and S and J) [Begin]
        *IF !EOF('APINVHDR')
        *  REPLACE APINVHDR.NINVPAID WITH APINVHDR.NINVPAID + APDIST.NAPDAMNT
        *  =gfAdd_Info('APINVHDR')
        *ENDIF
        *B601528,1 Remove this lines [End]
        
        lcNewPayFld = 'APVENHST.NVNHPAY'+ALLTRIM(STR(INT(VAL(lcFiscalP))))
        IF llAdvPay
      
          *E300296,4 Update Vendor fields with the equivalent
          *E300296,4 amount (in base currency), not the amount field.
          *REPLACE APVENDOR.NVENOPNDR WITH APVENDOR.NVENOPNDR - IIF(APINVHDR.NINVAMNT < 0,0 - APDIST.NAPDAMNT,0),;
                   APVENDOR.NVENBAL   WITH APVENDOR.NVENBAL   - APDIST.NAPDAMNT                             ,;
                   APVENDOR.NVENCPAY  WITH APVENDOR.NVENCPAY  + APDIST.NAPDAMNT
          REPLACE APVENDOR.NVENOPNDR WITH APVENDOR.NVENOPNDR - IIF(APINVHDR.NINVAMNT < 0,0 - APDIST.nEqvAmnt,0),;
                  APVENDOR.NVENBAL   WITH APVENDOR.NVENBAL   - APDIST.nEqvAmnt                             ,;
                  APVENDOR.NVENCPAY  WITH APVENDOR.NVENCPAY  + APDIST.nEqvAmnt        
          *E300296,4 end.
          *B601060,1 Clear the debit memo paid amount.

          *B601528,1 Remove this line (To use the records with cApdActId A
          *          and S and J) [Begin]
          *REPLACE APINVHDR.NINVPAID WITH 0
          *B601528,1 Remove this line [End]
          
          *B601060,1 end.          
        ELSE
          *E300296,4 Update Vendor fields with the equivalent
          *E300296,4 amount (in base currency), not the amount field.
          *REPLACE APVENDOR.NVENOPNDR WITH APVENDOR.NVENOPNDR - IIF(APINVHDR.NINVAMNT < 0,APDIST.NAPDAMNT,0),;
                   APVENDOR.NVENBAL   WITH APVENDOR.NVENBAL   - APDIST.NAPDAMNT                             ,;
                   APVENDOR.NVENCPAY  WITH APVENDOR.NVENCPAY  + APDIST.NAPDAMNT
          REPLACE APVENDOR.NVENOPNDR WITH APVENDOR.NVENOPNDR - IIF(APINVHDR.NINVAMNT < 0,APDIST.nEqvAmnt,0),;
                  APVENDOR.NVENBAL   WITH APVENDOR.NVENBAL   - APDIST.nEqvAmnt                             ,;
                  APVENDOR.NVENCPAY  WITH APVENDOR.NVENCPAY  + APDIST.nEqvAmnt       
          *E300296,4 end.
        ENDIF          

        =gfAdd_Info('APVENDOR')
      
        *E300296,4 Update Vendor History fields with the equivalent
        *E300296,4 amount (in base currency), not the amount field.
        *REPLACE APVENHST.NVNHTOTPA WITH APVENHST.NVNHTOTPA    + APDIST.NAPDAMNT,;
                &lcNewPayFld       WITH EVALUATE(lcNewPayFld) + APDIST.NAPDAMNT
        REPLACE APVENHST.NVNHTOTPA WITH APVENHST.NVNHTOTPA    + APDIST.nEqvAmnt,;
                &lcNewPayFld       WITH EVALUATE(lcNewPayFld) + APDIST.nEqvAmnt
        *E300296,4 end.
       
        =gfAdd_Info('APVENHST')

        DO CASE
          CASE APPAYMNT.CPAYMETH = 'M'
            *E300296,4 Update Vendor History fields with the equivalent
            *E300296,4 amount (in base currency), not the amount field.
            *REPLACE APVENHST.NVNHMCHKP WITH APVENHST.NVNHMCHKP + APDIST.NAPDAMNT
            REPLACE APVENHST.NVNHMCHKP WITH APVENHST.NVNHMCHKP + APDIST.nEqvAmnt
            *E300296,4 end.
  
          CASE APPAYMNT.CPAYMETH = 'N'

            *E300296,4 Update Vendor History fields with the equivalent
            *E300296,4 amount (in base currency), not the amount field.
            *REPLACE APVENHST.NVNHNCHKP WITH APVENHST.NVNHNCHKP + APDIST.NAPDAMNT
            REPLACE APVENHST.NVNHNCHKP WITH APVENHST.NVNHNCHKP + APDIST.nEqvAmnt
            *E300296,4 end.

          CASE APPAYMNT.CPAYMETH = 'H'
  
            *E300296,4 Update Vendor History fields with the equivalent
            *E300296,4 amount (in base currency), not the amount field.
            *REPLACE APVENHST.NVNHCASHP WITH APVENHST.NVNHCASHP + APDIST.NAPDAMNT
            REPLACE APVENHST.NVNHCASHP WITH APVENHST.NVNHCASHP + APDIST.nEqvAmnt
            *E300296,4 end.          

*B800740,1 M.H Begin.
          CASE APPAYMNT.CPAYMETH = 'P'
            REPLACE APVENHST.NVNHPCHKP WITH APVENHST.NVNHPCHKP + APDIST.nEqvAmnt
*B800740,1 M.H End.
        ENDCASE

        SELECT APDIST
      
      CASE APDIST.CAPDACTID = 'S'

        SCATTER MEMVAR MEMO
        *B600995,1 HESHAM EL-SHELTAWI (START)
        *B600995,1 changing the status of the existing recort to 'V'
        REPLACE cApdStat WITH 'V'
        *B600995,1 (End)
      
        m.lApdPost  = .F.
        m.cApdStat  = 'V'
        m.nApdAmnt  = 0 - m.nApdAmnt
        m.cBatchNo  = ' '
        m.cTrnSleDn = ' '
        m.cFisFYear = lcFiscalY
        m.cFsPPrdId = lcFiscalP
        m.cApSessNo = lcSequence
      
        *E300296,4 Add a variable for nEqvAmnt field
        m.nEqvAmnt  = 0 - m.nEqvAmnt
        *E300296,4 end.

        *B800271,1 Use the void date instead of the payment date.      
        m.dAPDTrDat = ldVoidDate
        *B800271,1 end.

        SELECT(lcApDist)
        APPEND BLANK
        GATHER MEMVAR MEMO

        =gfAdd_Info()
      
        SELECT APDIST
        IF llAdvPay
          *E300296,4 Update Invoice fields with APDIST.nAPDAmnt,
          *E300296,4 (same currency as that of the invoice), but,
          *E300296,4 update Vendor fields with APDIST.nEqvAmnt 
          *E300296,4 (in base currency), not the amount field.
          *REPLACE APINVHDR.NINVDISTK  WITH APINVHDR.NINVDISTK  + APDIST.NAPDAMNT,;
                   APVENDOR.NVENOPNDR  WITH APVENDOR.NVENOPNDR  - IIF(APINVHDR.NINVAMNT < 0,0 - APDIST.NAPDAMNT,0),;
                   APVENDOR.NVENBAL    WITH APVENDOR.NVENBAL    - APDIST.NAPDAMNT
          REPLACE APINVHDR.NINVDISTK  WITH APINVHDR.NINVDISTK  + APDIST.NAPDAMNT,;
                  APVENDOR.NVENOPNDR  WITH APVENDOR.NVENOPNDR  - IIF(APINVHDR.NINVAMNT < 0,0 - APDIST.nEqvAmnt,0),;
                  APVENDOR.NVENBAL    WITH APVENDOR.NVENBAL    - APDIST.nEqvAmnt                 
          *E300296,4 end.        
        ELSE
          *E300296,4 Update Invoice fields with APDIST.nAPDAmnt,
          *E300296,4 (same currency as that of the invoice), but,
          *E300296,4 update Vendor fields with APDIST.nEqvAmnt 
          *E300296,4 (in base currency), not the amount field.
          *REPLACE APINVHDR.NINVDISTK  WITH APINVHDR.NINVDISTK  + APDIST.NAPDAMNT,;
                   APVENDOR.NVENOPNDR  WITH APVENDOR.NVENOPNDR  - IIF(APINVHDR.NINVAMNT < 0,APDIST.NAPDAMNT,0),;
                   APVENDOR.NVENBAL    WITH APVENDOR.NVENBAL    - APDIST.NAPDAMNT
          REPLACE APINVHDR.NINVDISTK  WITH APINVHDR.NINVDISTK  + APDIST.NAPDAMNT,;
                  APVENDOR.NVENOPNDR  WITH APVENDOR.NVENOPNDR  - IIF(APINVHDR.NINVAMNT < 0,APDIST.nEqvAmnt,0),;
                  APVENDOR.NVENBAL    WITH APVENDOR.NVENBAL    - APDIST.nEqvAmnt
          *E300296,4 end.                
        ENDIF
        =gfAdd_Info('APVENDOR')
              
        *E300296,4 Update Vendor History fields with APDIST.nEqvAmnt 
        *E300296,4 (in base currency), not the amount field.
        *REPLACE APVENHST.NVNHDISTKN WITH APVENHST.NVNHDISTKN + APDIST.NAPDAMNT
        REPLACE APVENHST.NVNHDISTKN WITH APVENHST.NVNHDISTKN + APDIST.nEqvAmnt
        *E300296,4 end.
      
        =gfAdd_Info('APVENHST')

        *B601528,1 Add this lines to use the records with cApdActId A and S
        *          and J [Begin] 
        *B601528,1 IF Statment to check that the record pointer is not at
        *          the end of file in the file APINVHDR.DBF  
        IF !EOF('APINVHDR')
          REPLACE APINVHDR.NINVPAID WITH APINVHDR.NINVPAID - APDIST.NAPDAMNT
          =gfAdd_Info('APINVHDR')
        ENDIF   && End of IF
        *B601528,1 Add this lines [End]

        SELECT APDIST

      CASE APDIST.CAPDACTID = 'J'
  
        SCATTER MEMVAR MEMO
        *B600995,1 HESHAM EL-SHELTAWI (START)
        *B600995,1 changing the status of the existing recort to 'V'
        REPLACE cApdStat WITH 'V'
        *B600995,1 (End)
 
        m.lApdPost  = .F.
        m.cApdStat  = 'V'
        m.nApdAmnt  = 0 - m.nApdAmnt
        m.cBatchNo  = ' '
        m.cTrnSleDn = ' '
        m.cFisFYear = lcFiscalY
        m.cFsPPrdId = lcFiscalP
        m.cApSessNo = lcSequence

        *E300296,4 Add a variable for nEqvAmnt field
        m.nEqvAmnt  = 0 - m.nEqvAmnt
        *E300296,4 end.

        *B800271,1 Use the void date instead of the payment date.      
        m.dAPDTrDat = ldVoidDate
        *B800271,1 end.
      
        SELECT(lcApDist)
        APPEND BLANK
        GATHER MEMVAR MEMO

        =gfAdd_Info()
      
        SELECT APDIST
        IF llAdvPay
          *E300296,4 Update Invoice fields with APDIST.nAPDAmnt,
          *E300296,4 (same currency as that of the invoice), but,
          *E300296,4 update Vendor fields with APDIST.nEqvAmnt 
          *E300296,4 (in base currency), not the amount field.
          *REPLACE APINVHDR.NINVADJ    WITH APINVHDR.NINVADJ   + APDIST.NAPDAMNT,;
                   APVENDOR.NVENOPNDR  WITH APVENDOR.NVENOPNDR - IIF(APINVHDR.NINVAMNT < 0,0 - APDIST.NAPDAMNT,0),;
                   APVENDOR.NVENBAL    WITH APVENDOR.NVENBAL   - APDIST.NAPDAMNT
          REPLACE APINVHDR.NINVADJ    WITH APINVHDR.NINVADJ   + APDIST.NAPDAMNT,;
                  APVENDOR.NVENOPNDR  WITH APVENDOR.NVENOPNDR - IIF(APINVHDR.NINVAMNT < 0,0 - APDIST.nEqvAmnt,0),;
                  APVENDOR.NVENBAL    WITH APVENDOR.NVENBAL   - APDIST.nEqvAmnt                
          *E300296,4 end.              
        ELSE
          *E300296,4 Update Invoice fields with APDIST.nAPDAmnt,
          *E300296,4 (same currency as that of the invoice), but,
          *E300296,4 update Vendor fields with APDIST.nEqvAmnt 
          *E300296,4 (in base currency), not the amount field.
          *REPLACE APINVHDR.NINVADJ    WITH APINVHDR.NINVADJ   + APDIST.NAPDAMNT,;
                   APVENDOR.NVENOPNDR  WITH APVENDOR.NVENOPNDR - IIF(APINVHDR.NINVAMNT < 0,APDIST.NAPDAMNT,0),;
                   APVENDOR.NVENBAL    WITH APVENDOR.NVENBAL   - APDIST.NAPDAMNT

          *B601528,1 Change this line to use the records with cApdActId A 
          *          , S and J and to check if the record is the Approved
          *          adjustment record [Begin] 
          *REPLACE APINVHDR.NINVADJ    WITH APINVHDR.NINVADJ   + APDIST.NAPDAMNT,;
          *        APVENDOR.NVENOPNDR  WITH APVENDOR.NVENOPNDR - IIF(APINVHDR.NINVAMNT < 0,APDIST.nEqvAmnt,0),;
          *        APVENDOR.NVENBAL    WITH APVENDOR.NVENBAL   - APDIST.nEqvAmnt        

          *B601528,1 IF Statment to check if the record is  Approved
          *          adjustment record and not a calculated one
          IF APDIST.NAPDLINNO <> 1
            REPLACE APINVHDR.NINVPAID WITH APINVHDR.NINVPAID - APDIST.NAPDAMNT,;
                    APINVHDR.NINVADJ    WITH APINVHDR.NINVADJ   + APDIST.NAPDAMNT,;
                    APVENDOR.NVENOPNDR  WITH APVENDOR.NVENOPNDR - IIF(APINVHDR.NINVAMNT < 0,APDIST.nEqvAmnt,0),;
                    APVENDOR.NVENBAL    WITH APVENDOR.NVENBAL   - APDIST.nEqvAmnt        

            =gfAdd_Info('APINVHDR')
          ENDIF  && End of IF
          *B601528,1 Change this line [End]
          
          *E300296,4 end.
        ENDIF          

        =gfAdd_Info('APVENDOR')

*      REPLACE APPAYMNT.NPAYADJ WITH APPAYMNT.NPAYADJ - APDIST.NAPDAMNT
*      =gfAdd_Info('APPAYMNT')     && Mohamed Abdel Salam

    ENDCASE

    SELECT APINVHDR          
    =gfObj_Lock(.F.)
    SELECT APDIST
  ENDSCAN  

  SELECT APPAYMNT
  IF !EOF()
    GO RECNO()
  ENDIF  

  SELECT APDIST

  SCAN REST WHILE APDIST.CAPDTRTYP+APDIST.CBNKCODE+;
                  APDIST.CCHKACCT +APDIST.CAPDREF = ;
                  APPAYMNT.CPAYMETH+APPAYMNT.CBNKCODE+;
                  APPAYMNT.CCHKACCT+APPAYMNT.CPAYDOCNO;
              FOR APDIST.CAPDACTID = 'A'
    SELECT APINVHDR          
    =gfObj_Lock(.F.)
    SELECT APDIST
  ENDSCAN  

  SELECT APPAYMNT
  *B601060,1 Clear locking after file update, move downwards
  *=gfObj_Lock(.F.)
  *B601060,1 end.

  *B800271,1 Use the void date instead of the system date.
  *REPLACE CPAYSTAT  WITH 'V',;
          DPAYVDATE WITH gdSysDate
  REPLACE CPAYSTAT  WITH 'V',;
          DPAYVDATE WITH ldVoidDate
  *B800271,1 end.

  =gfAdd_Info()
 
  *B601060,1 Clear locking after file update, moved from above
  =gfObj_Lock(.F.)
  *B601060,1 end.
  
  *B601060,1 Remove refreshing since it is done in lfwDisBrow() called
  *B601060,1 below
  *SHOW WINDOW (lcBrowsTtl) REFRESH SAME
  *B601060,1 end.
  
  lcFilter = IIF(EMPTY(lcFiltExp),lcMainFilt,lcFiltExp)

*B800603,1 M.H On 05/20/96 Begin.

  SKIP 1
  IF EOF()
    SKIP -1
    lnSavPoint = RECNO()
  ELSE
    lnSavPoint = RECNO()
  ENDIF    
*B800603,1 M.H On 05/20/96 End.
  
  LOCATE FOR &lcFilter

  IF !FOUND()
    IF WVISIBLE('CWRAPVDVEN')
      =gfChClose('CWRAPVDVEN')
    ENDIF
    ** MESSAGE : " There are no ð .               "
    **           "             ® Ok ¯             "
    =gfModalGen("TRM04101B00000","DIALOG",lcTMorePay)
    rbScope = 1
    SET FILTER TO &lcMainFilt
    GO TOP
    SHOW GET rbScope
    lcFiltExp = ' '
*B800603,1 M.H On 05/20/96 Begin.
  ELSE
    GO lnSavPoint
*B800603,1 M.H On 05/20/96 End.
  ENDIF  

  IF EOF()
    SHOW GET pbVendor  DISABLE
    SHOW GET pbInvoice DISABLE
    SHOW GET pbVoid    DISABLE
  ELSE
    SHOW GET pbVendor  ENABLE
    SHOW GET pbInvoice ENABLE
    SHOW GET pbVoid    ENABLE
  ENDIF  

  *B601060,1 Remove refreshing since it is done in lfwDisBrow() called
  *B601060,1 below
  *lnBrRecNo  = RECNO('APPAYMNT')
  *SHOW WINDOW (lcBrowsTtl) REFRESH SAME
  *B601060,1 end.
  =lfwDisBrow()
*B601060,1 Add an EndIf
ENDIF
*B601060,1 end.

*!**************************************************************************
*!
*!      FUNCTION : lfKeyTrap
*!
*!**************************************************************************
* 
FUNCTION lfKeyTrap
PARAMETERS lnLAstKey

DO CASE
  CASE lnLastKey = 27 .OR. lnLastKey = 10
    IF _DOS
      HIDE MENU _MSYSMENU
      SHOW MENU _MSYSMENU
    ENDIF  
    ACTIVATE WINDOW (lcMain3)
    _CUROBJ = OBJNUM(pbClose)
    KEYBOARD "{ENTER}"
    
  CASE lnLastKey = 24
    KEYBOARD "{SHIFT+HOME}"       && dummy key press to release menu access
    =lfvVoid()

  CASE lnLastKey = 47
    KEYBOARD "{Z}"       && dummy key press to release menu access
    =lfwVenDis()
    =lfvVenDis()

  CASE lnLastKey = 31
    KEYBOARD "{SHIFT+HOME}"       && dummy key press to release menu access
    *B800271,1 Activate upper window.
    ACTIVATE WINDOW (lcMain2)
    _CUROBJ = OBJNUM(rbScope)+1
    *B800271,1 end.
    =lfwScope()
    rbScope = 2
    SHOW GET rbScope
    =lfvScope()
    
  CASE lnLastKey = 30
    KEYBOARD "{SHIFT+HOME}"       && dummy key press to release menu access
    =lfwScope()
    rbScope = 1
    SHOW GET rbScope
    =lfvScope()

  CASE lnLastKey = 23
    KEYBOARD "{Z}"       && dummy key press to release menu access
    *B800271,1 Activate upper window.
    ACTIVATE WINDOW (lcMain2)
    _CUROBJ = OBJNUM(rbScope)+1
    *B800271,1 end.
    =lfwScope()
    =lfvInvoice()

ENDCASE

*!*************************************************************
*! Name      : lfvVoidDate
*! Developer : RENEE - Renee Ezzat
*! Date      : 11/01/1995
*! Purpose   : Valid function for ldVoidDate field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvVoidDate()
*!*************************************************************
*B800271,1 Added this function as a validation function for the new 
*B800271,1 field, ldVoidDate
FUNCTION lfvVoidDate
*B603958,1 KAM(START)
*IF ldVoidDate <> ldOldDate .AND. WVISIBLE(gcBaseWind)
*  lcOldFYear = lcFiscalY
*  lcOldFPer  = lcFiscalP
*  IF !EMPTY(ldVoidDate) .AND. !lfVlDate(gcPrnt_Cmp, @lcFiscalP, @lcFiscalY)
*    *B800271,1 If the date is invalid, present the following message
*    *B800271,1 and return the old value.
*    *B800271,1 Message : "The ð date should be within the posting window."
*    *B800271,1                           <    OK    >    
*    =gfModalGen('TRM04113B00000', 'DIALOG', lcTVoid)
*    ldVoidDate = ldOldDate
*    lcFiscalY  = lcOldFYear 
*    lcFiscalP  = lcOldFPer 
*    _CUROBJ    = OBJNUM(ldVoidDate)
*  ENDIF
*  ldOldDate = ldVoidDate
*ENDIF

Private  lcBaseWind
lcBaseWind = WVISIBLE(gcBaseWind)

IF EMPTY(ldVoidDate) .AND. lcBaseWind
_CUROBJ = OBJNUM(ldVoidDate)
  RETURN 
ENDIF

llLockPrd = .F.
IF !lfVlDate(gcPrnt_Cmp, @lcFiscalP, @lcFiscalY , ldVoidDate , @llLockPrd) .AND. lcBaseWind
  =gfModalGen('TRM04113B00000', 'DIALOG', lcTVoid)
  ldVoidDate = ldOldDate
  lcFiscalY  = lcOldFYear 
  lcFiscalP  = lcOldFPer 
 _CUROBJ      = OBJNUM(ldVoidDate)
  RETURN 
ENDIF
IF llLockPrd 
  =gfModalGen("QRM01258B00000" , "DIALOG" ) 
  _CUROBJ = OBJNUM(ldVoidDate)
  RETURN 
ENDIF
ldOldDate = ldVoidDate
RETURN
*B603958,1 KAM(end)

*!*************************************************************
*! Name      : lfReadAct
*! Developer : RENEE - Renee Ezzat
*! Date      : 11/01/1995
*! Purpose   : READ Activate function of APVDPAY.SCX
*!*************************************************************
*! Calls              : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  =lfReadAct()
*!*************************************************************
*B800271,1 Add a read activate function for trap releasing.
FUNCTION lfReadAct
*B800271,1 Restore old key settings and clear the current settings
=lfResetTrap()
IF _DOS
  =lfDefinePad() && Define the Browse pad in the menu.
ENDIF  
ON KEY LABEL ALT+B DO lfActBrows


*!*************************************************************
*! Name      : lpTrapTab
*! Developer : RENEE - Renee Ezzat
*! Date      : 11/01/1995
*! Purpose   : Trap of tab keys
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  DO lpTrapTab WITH 'MFG10002', OBJNUM(pbNew)
*!*************************************************************
*B800271,1 Add a tab/shifttab trapping function.
PROCEDURE lpTrapTab
PARAMETERS lcWindName, lnObjNum
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = lnObjNum

*!*************************************************************
*! Name      : lfPayment
*! Developer : Haytham El_Sheltawi
*! Date      : 04/06/98
*! Purpose   : Function to check if the advance payment has already
*!             been paid
*!*************************************************************
*! Called from : lfvVoid()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Return     : .T. If the advance payment has already been paid.
*!              .F. Otherwise.
*!*************************************************************
*B601994,1 This function was add by HS for the Bug.
*!*************************************************************
*
FUNCTION lfPayment

PRIVATE lcInvKey , lcSavOrd , llReturn

llReturn = .F.                                                && Variable to hold the value to be returned
lcInvKey = APINVHDR.cInvNo + APINVHDR.cVendCode               && Variable to hold the search key

SELECT APDIST
lcSavOrd = IIF(EMPTY(ORDER()) , '' , 'TAG ') + ORDER()        && Save the current order tag
SET ORDER TO TAG INVVEND

*--IF Statement to place the record pointer in the first record for the
*--advance payment debit memo in the APDIST file
IF SEEK(lcInvKey)
  LOCATE REST;
        WHILE cInvNo + cVendCode = lcInvKey ;
          FOR cApdActId = 'C' .AND. nApdAmnt > 0
  
  llReturn = FOUND()
ENDIF    && End of IF SEEK(lcInvKey)

SET ORDER TO &lcSavOrd

*--Refresh the relation
SELECT APPAYMNT

*--If the curent record is valid [not end of file]
IF !EOF()
  GO RECNO()
ENDIF    && End of IF !EOF()

RETURN llReturn



*!*************************************************************
*! Name      : lfObj_Lock
*! Developer : Mohamed Shokry, 
*=>  Added here by TMI 06/21/2006 for issue *B132275,1  
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
        IF SEEK ('INI'+'OLDVARS'+cLok_User,'syuStatc') 
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
    IF RECNO()<=RECCOUNT()
      GOTO RECNO()
    ENDIF
    lRet_Flag  = .T.    
  ENDIF
ENDIF


UNLOCK


RETURN lRet_Flag



