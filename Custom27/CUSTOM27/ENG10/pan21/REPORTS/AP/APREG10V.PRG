*:**************************************************************************
*: Program file  : APREG10V
*: Program desc. : Custom AP Cheque Print For Reggie (England).
*: System        : Aria Advantage Series.
*: Module        : Accounts Payable (AP)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Reference     : C200193
*:**************************************************************************
*: Functions     : lfLockFile , lfClearLok , lfUpInv , lfIncNxtChk , lfSumGrp
*:                 lfIniTotal , lfAllgTest , lfwGrid , lfvGrid , lfvBnkCode
*:                 lfvChkAcct , lfValidAcct , lfvNxtChk , lfvChkDate , lfShowOg
*:                 lfvOkAdvPay , lfvDivision , lfSortAddr , lfvVenCode , lfvRemit
*:                 lfReValidate , lfvFactor , lfCreateCur , lfSetRela , lfLokBank
*:                 lfUnLokBank , lfAdvPay , lfClearRep , lfBeforeRev , lfAdvance
*:                 lfvRepName , lfvPrintMode , lfvRePrnChk , lfRePrnUpd , lfFooter
*:                 lfSelectRec , lfwibBrows , lfvibBrows , lfSlcRec , lfvRpBet
*:                 lfvPrnChkRG , lfNxtChkUpd , lfInsVdChr , lfGetNetSm , lfSetCurSm
*:                 lfDefNxtChk , lfDefGlAcc ,  lfAdvPyCur
*:**************************************************************************
*: Notes : This code is an exact copy of standard Print Cheques report
*:**************************************************************************
*B604683,1 SSE 07/19/2001 Fix some bugs in report regarding font & alignment.
*C128816,1 MMR 09/26/2005 New custom to print 3 day payment and international payment txt files.
*C128816,2 MMR 11/06/2005 Populate the :20: field in the international payment txt files with 
*C128816,2 MMR            the user difined field APVENDOR.CSENDREF.
*:**************************************************************************
*C200193,1 Define an array for numeric words. [Begin]
DIMENSION laWords(15)
laWords = SPACE(15)
*C200193,1 Define an array for numeric words. [End]
lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
lcSavCurSm = SET('CURRENCY',1)    && Varible to save the seting of the currency symbol

PRIVATE llCurOpn
llCurOpn = USED("SYCCURR")
SELECT       cCurrCode , cCurrSmbl ;
  FROM       (gcSysHome + 'SYCCURR') ;
  INTO ARRAY laCurrSmbl     
IF !llCurOpn  
  USE IN SYCCURR
ENDIF  

lcNetSmbl = ALLTRIM(lfGetNetSm())      &&Varible to save the currency symbol of the checking account currency

*-- Check if user wants report to printer or file 
llPrintChk = IIF(UPPER(gcDevice)='PRINTER' OR UPPER(gcDevice)='FILE',.T.,.F.)
lcFisYear  = ''      && fiscal year
lcFisPrd   = ''      && fiscal period
lcLineTwo  = ''      && holds the total check amount (uesd in FRX)
lcInvRemit = ''      && invoice remit to
lcFactor   = ''      && factor code
lcTFactor  = ''      && 'factor code'
ldRPChkDat = ldChkDat
DIMENSION laFooter[11,6]  && Creat a new array to hold the footer records.
laFooter  = ''
lnFooter  = 0 
lcVendor  = '' && Variable to hold the vendor code.
lcPageNo  = ''
*-- If the vendor address is changed the invoices should be printed in a new chk.
lcAddress  = '' && Variable to hold the invoice address.

PRIVATE lcCurErHnd
lcCurErHnd = ON("ERROR")
ON ERROR STORE .T. TO llDumErHnd

lcVoid = gfTempName()
CREATE CURSOR (lcVoid) (gPic G)
APPEND BLANK
APPEND GENERAL gPIC FROM ( gcBmpHome+'VOIDCHK.BMP')
ON ERROR &lcCurErHnd

IF llTestChk            && In case of allignment test.
  IF lfLokBank()        && If could lock the bank record
    SELECT (lcRpTargt)
    *-- call the gfDispRe with .T. the last parameter to force printing to file first 
    *-- before the print to ignore any errors can be happed while printing
    DO gfDispRe WITH EVAL('lcRepForm'),.f.,.f.,'R',.T.    
    IF llPrintChk
      REPLACE APCHECKS.NCHKNXTPN WITH INT(VAL(lcNxtChkNo)) 
    ENDIF  
    =lfUnLokBank()      && unlock the bank record
  ENDIF  
  RETURN
ENDIF

lcPrExp    = lcRpExp + ' AND CBNKCODE+CCHKACCT = lcRpBnkCod+lcRpChkAct'
lnTotal    = 0         && Summation the total invoices in the main check
llEndGrp   = .T.       && if end of group
lcChkNo    = ''        && Hold the check number 
lnPrnCh  =0 && the payment group number being printed ... used in reprinting only
IF ! lfReValidate()    && Check if the data validated or not
  RETURN
ENDIF

lcPayPrd   = "NVNHPAY"+ALLTRIM(STR(VAL(lcFisPrd)))

*-- Declare some varibales will be used as a global in case of adv. pay. AP Invoice header fields  (Print checks currency)
lnCurrUnit  = 1            && Holds currency unit of printed checks
lnExRate    = 1   		   && Holds currency rate of printed checks
lcCurrCode  = gcBaseCurr   && Holds currency code of printed checks
lnEqvAmnt   = lnPaymnt    && Holds eqv. amount

*-- lcPrintMod variable that hold..
*-- 'A' --> in case of 'advance check payment'.
*-- 'V' --> in case of 'print approved checks'.
*-- 'R' --> in case of 'reprinting check'.
DO CASE
  CASE lcPrintMod = 'A'  && Print advance checks
    IF lfvOkAdvPay() .AND. lfLokBank()
      =lfCreateCur()  && Create temp file   
      =lfSetRela()    && Set relation between the files
      =lfAdvPyCur()   && Get currency code, rate and unit of advanced payment
      =lfAdvPay()     && Fill the data into files
      SELECT (lcRpTargt)
      *C128816,1 MMR 09/26/2005 New custom to print 3 day payment and international payment txt files.
      *DO gfDispRe WITH EVAL('lcRepForm'),.f.,.f.,'R',.T.    
      IF llBacs
        IF EMPTY(lcOutFile1) OR EMPTY(lcOutFile2)
          =gfModalGen("INM00000B00000","DIALOG",'','','There is no Output file selected')
          RETURN
         ENDIF
         =lfColData()
         IF RECCOUNT(lcRpTargt) > 0
           =lfTextRec()
         ELSE
           =gfModalGen("INM00000B00000","DIALOG",'','','There are no records matched')
           =FFLUSH(lcOutFile1)
           =FCLOSE(lnOutFile1)
           =FFLUSH(lcOutFile2)
           =FCLOSE(lnOutFile2)
           RETURN
         ENDIF
         =FFLUSH(lcOutFile1)
         =FCLOSE(lnOutFile1)
         =FFLUSH(lcOutFile2)
         =FCLOSE(lnOutFile2)
      ELSE
         DO gfDispRe WITH EVAL('lcRepForm'),.f.,.f.,'R',.T.    
      ENDIF
      *C128816,1 MMR.[End]

      =lfUnLokBank()  && unlock the bank record
    ENDIF

  CASE lcPrintMod = 'V'     && Print approved checks
    lcRpFiles = 'APINVHDR'
    lcSaveEsc  = SET('ESCAPE')  && Save escape setting
    lcSaveOnEs = ON('ESCAPE')
    SET ESCAP ON
    ON ESCAP DO gpSQLBrak
    _TALLY = 0
    SET TALK ON                 && Activate the system select therom.
    glEscPrs = .F.

    *-- Add order field invoice number so that the invoices are sorted in the print as the reprint
    SELECT  &lcRpFields, 000000 AS 'nNoOfInv', '    ' AS 'cPageNo';
    FROM  &lcRpFiles ;
    WHERE &lcPrExp   ;
    ORDER BY CVENDCODE,CADDRESS,CINVNO;
    INTO DBF (gcWorkDir+lcRpTargt)

    *-- Restore all enviroment 
    WAIT CLEAR
    SET TALK OFF
    ON ESCAPE  &lcSaveOnEs
    SET ESCAPE &lcSaveEsc  

    *-- Display the report to screen , file or printer
    *-- and check if there is any record or not
    IF _TALLY = 0      && No records collected
      *-- NO recoeds hove been collected
      =gfModalGen("INM00052B00000","DIALOG")
    ELSE
      =lfSetRela()        && Set relations between files
      IF lfLokBank()      && If could lock the bank record
        IF lfLockFile()   && If could lock the selected invoice records
                          && ,mark Begin of each group and calculate no
                          && of checks to be print for each group. 
          SELECT (lcRpTargt)
         *-- In case of DOTMATIX printing, advance after seach check by line feed not form feed
         lcPadvance = _PADVANCE
         _PADVANCE  = IIF(lcRpName ='1','LINEFEED','FORMFEED')
         _PLENGTH   = 42

         *-- call the gfDispRe with .T. the last parameter to force printing to file first 
         *-- before the print to ignore any errors can be happed while printing
         *-- Loop through the Invoices that is going to printed to update before printing 
         *-- in the approved invoices case
          
          *C200193,1 Update Master files in the 2 laser forms. [Begin]
          *IF llPrintChk AND (lcRpName='2' AND _WINDOWS)      &&lcRepForm = 'APREG200'
          IF llPrintChk
          *C128816,1 MMR 09/26/2005 New custom to print 3 day payment and international payment txt files.
          IF llBacs
            IF EMPTY(lcOutFile1) OR EMPTY(lcOutFile2)
              =gfModalGen("INM00000B00000","DIALOG",'','','There is no Output file selected')
              RETURN
            ENDIF
            =lfColData()
            IF RECCOUNT(lcRpTargt) > 0
              =lfTextRec()
            ELSE
              =gfModalGen("INM00000B00000","DIALOG",'','','There are no records matched')
              =FFLUSH(lcOutFile1)
              =FCLOSE(lnOutFile1)
              =FFLUSH(lcOutFile2)
              =FCLOSE(lnOutFile2)
              RETURN
            ENDIF
            =FFLUSH(lcOutFile1)
            =FCLOSE(lnOutFile1)
            =FFLUSH(lcOutFile2)
            =FCLOSE(lnOutFile2)
          ENDIF
          *C128816,1 MMR.[End]
          *C200193,1 Update Master files in the 2 laser forms. [End]

            GO TOP
            lcTmpChkNo=lcNxtChkNo
            lcChkPage = cPageNo
            SCAN
              =lfNxtChkUpd()            
              =lfUpInv()
            ENDSCAN
            lcNxtChkNo = lcTmpChkNo
          ENDIF
         *C128816,1 MMR 09/26/2005 New custom to print 3 day payment and international payment txt files.
         *DO gfDispRe WITH EVAL('lcRepForm'),.f.,.f.,'R',.T.    
         IF !llBacs
           DO gfDispRe WITH EVAL('lcRepForm'),.f.,.f.,'R',.T.    
         ENDIF 
         *C128816,1 MMR.[End]
         *-- Return _PADVANCE to its original value
         _PADVANCE = lcPadvance
        ENDIF
        =lfUnLokBank()    && unlock the bank record
      ENDIF
    ENDIF
  
  CASE lcPrintMod = 'R'  && Reprint Checks
    IF lfLokBank()       && If could lock the bank record
      =lfCreateCur()     && Create temp file   
      =lfSetRela()       && Set relation between the files
      =lfSelectRec()     && select all detail lines for selected check number. 
      SELECT (lcRpTargt)

      *-- In case of DOTMATIX printing, advance after seach check by line feed not form feed
      lcPadvance = _PADVANCE
      _PADVANCE  = IIF(lcRpName ='1','LINEFEED','FORMFEED')         
     
      *-- call the gfDispRe with .T. the last parameter to force printing to file first 
      *-- before the print to ignore any errors can be happed while printing
      DO gfDispRe WITH EVAL('lcRepForm'),.f.,.f.,'R',.T.    

      *-- Return _PADVANCE to its original value
      _PADVANCE = lcPadvance
      
      IF llPrintChk          && in case output device = 'Printer' or 'File'
        =lfRePrnUpd()        && update master file.
        STORE '' TO lcRpChkFrm,lcRpChkTo
        SHOW GET lcRpBnkCod
        SHOW GET lcRpChkAct
      ENDIF  
      =lfUnLokBank()         && unlock the bank record
    ENDIF

ENDCASE

SHOW GET lcNxtChkNo     && Refresh the next check number
*C128816,1 MMR 09/26/2005 New custom to print 3 day payment and international payment txt files.
*IF llPrintChk
IF llPrintChk AND RECCOUNT(lcRpTargt) > 0
  *-- Printing is completed.
  *=gfModalGen("INM04124B00000","DIALOG") 
  IF llBacs 

    =gfModalGen('TRM00000B00000','ALERT','','','Output files '+lcOutFile1+','+lcOutFile2+' has been created.')
  ELSE
    =gfModalGen("INM04124B00000","DIALOG") 
  ENDIF  
ENDIF

SET CURRENCY TO lcSavCurSm

IF USED(lcVoid)
  USE IN (lcVoid)
ENDIF

*!************************************************************************
*!
*!      FUNCTION lfLockFile
*!
*!************************************************************************
* Lock the invoice header file , Count no of checks to be printed for 
* each group and determin ,mark the begining of each group.
FUNCTION lfLockFile

SELECT (lcRpTargt)

lnCount     = 0
lnTotInvApv = 0
lnInvCount  = 1

GO BOTTOM

*C200193,1 make the address on Vender code only. [Begin]
*lcGroup = CVendCode + CAddress
lcGroup = CVendCode
*C200193,1 make the address on Vender code only. [End]

REPLACE nNoOfInv WITH 0
DO WHILE !BOF()

  *C200193,1 Compare against Vendor code. [Begin]
  *IF lcGroup = CVendCode + CAddress   && check if same group
  IF lcGroup = CVendCode               && check if same group
  *C200193,1 Compare against Vendor code. [Begin]

    lnTotInvApv = lnTotInvApv + nInvFAAp
    
    *-- lnRpStub nubmer of lines per page 12 for DOS, 14 for WINDOWS
    REPLACE nNoOfInv WITH CEILING(lnCount/lnRpStub)
  ELSE 
    *-- Check if the approval payment is less than 1 
    IF lnTotInvApv < 0 

       IF llPrintChk
         *-- unlock all locked records.
         =lfClearLok()
       ENDIF  

      *-- Message :"Approved payment for vendor ð can not be lees than zero"
      *-- Choice  :"                         < OK >                        "
      =gfModalGen("INM04162B00000","DIALOG",ALLTRIM(SUBSTR(lcGroup,1,8)))

      RETURN .F.
    ENDIF  
    
    REPLACE nNoOfInv WITH 0     && Indicate to End Of Group.
    lnCount     = 0
    lnTotInvApv = nInvFAAp
    
    *C200193,1 make the address on Vender code only. [Begin]
    *lcGroup = CVendCode + CAddress
    lcGroup = CVendCode
    *C200193,1 make the address on Vender code only. [End]

  ENDIF
  lnCount     = lnCount     + 1
  
  SELECT APINVHDR
  *-- If device = 'File' Or 'Printer' and can lock cureent record.
  IF llPrintChk AND !gfObj_Lock(.T.)
    *-- Invoice ð for vendor ð is being edited. 
    *-- Check printing is canceled.
    =gfModalGen("INM04082B00000","DIALOG",ALLTRIM(APINVHDR.CINVNO)+'|'+ALLTRIM(CVendCode))
    SELECT (lcRpTargt)
    =lfClearLok()
    RETURN .F.
  ENDIF

  SELECT (lcRpTargt)
  SKIP -1
  *-- Stop printing chks for vendors with negative amounts

  IF BOF() AND lnTotInvApv < 0 

    IF llPrintChk
      *-- unlock all locked records.
      =lfClearLok()
    ENDIF  

    *-- Message :"Approved payment for vendor ð can not be lees than zero"
    *-- Choice  :"                         < OK >                        "
    =gfModalGen("INM04162B00000","DIALOG",ALLTRIM(SUBSTR(lcGroup,1,8)))

    RETURN .F.
  ENDIF  
ENDDO

SELECT (lcRpTargt)
*-- Make sure that invoices for the same vendor+address is printed on the same chk.
lnPgCntr  = 1
lnInvCntr = 0
GO TOP

*C200193,1 make the address on Vender code only. [Begin]
*lcGroup = CVendCode + CAddress
lcGroup = CVendCode
*C200193,1 make the address on Vender code only. [End]

SCAN
  *C200193,1 Compare against Vendor code. [Begin] 
  *IF lcGroup  = CVendCode + CAddress .AND. lnInvCntr < lnRpStub
  IF lcGroup  = CVendCode .AND. lnInvCntr < lnRpStub
  *C200193,1 Compare against Vendor code. [End]

    lnInvCntr = lnInvCntr+1
  ELSE
    lnPgCntr  = lnPgCntr+1
    lnInvCntr = 1
  ENDIF
  
  *-- Field to hold the page number in the printing sequance, to be used by the chk form to advance pages.
  REPLACE cPageNo WITH PADL(lnPgCntr,4)

  *C200193,1 make the address on Vender code only. [Begin]
  *lcGroup = CVendCode + CAddress
  lcGroup = CVendCode
  *C200193,1 make the address on Vender code only. [End]

ENDSCAN
GO TOP
*-- End of lfLockFile.

*!************************************************************************
*!
*!      FUNCTION lfClearLok
*!
*!************************************************************************
* 
FUNCTION lfClearLok
* unlocking all locked records in invoice header file.

SCAN REST
  SELECT APINVHDR
  =gfObj_Lock(.F.) 
ENDSCAN
*-- End of lfClearLok.

*!************************************************************************
*!
*!      FUNCTION lfUpInv
*!
*!************************************************************************
* Update the informations inside certain files while printing
*
FUNCTION lfUpInv

IF llEndGrp
  *-- Change the method for calculating the chk # in order to update the files
  lcAls = ALIAS()
  SELECT (lcRpTargt)
  lnRecNow = IIF(!EOF(),IIF(!BOF(),RECNO(),0),-1)
  lcExstAdd = cvendcode+cAddress
  COUNT FOR cvendcode+cAddress = lcExstAdd TO lnNoOfIvs
  lnNoOfChks = INT(VAL(lcNxtChkNo)) + CEILING(lnNoOfIvs/lnRpStub)
  IF !EMPTY(lcAls)
    SELECT (lcAls)
    IF lnRecNow > 0
      GOTO lnRecNow
    ELSE
      IF lnRecNow = 0
        GOTO TOP
      ELSE
         GOTO BOTTOM
      ENDIF
    ENDIF
  ENDIF
  REPLACE APCHECKS.NCHKNXTPN WITH lnNoOfChks
  
  lnStubChkNo = INT(VAL(lcNxtChkNo))-1  
  lcChkNo = PADL(APCHECKS.NCHKNXTPN - 1,8,'0')
  SELECT APPAYMNT
  APPEND BLANK
  REPLACE                                ;
    CPAYTYPE  WITH 'P'                   ;
    CPAYDOCNO WITH lcChkNo               ;
    CPAYMETH  WITH 'P'                   ;
    CPAYSTAT  WITH 'B'                   ;
    DPAYDATE  WITH ldChkDat            ;
    CFISFYEAR WITH lcFisYear             ;
    CFSPPRDID WITH lcFisPrd              ;
    DPAYVDATE WITH {}                    ;
    CPAYCLNO  WITH &lcRpTargt..CVendCode ;
    CPAYCOMP  WITH &lcRpTargt..COUTCOMP  ;
    NPAYAMNT  WITH 0                     ;
    NPAYDISC  WITH 0                     ;
    NPAYADJ   WITH 0                     ;
    LPAYADVAN WITH .F.                   ;
    NINV1099A WITH 0                     ;
    CBNKCODE  WITH lcRpBnkCod            ;
    CPAYRECST WITH 'O'                   ;    
    CCHKACCT  WITH lcRpChkAct
  =gfAdd_Info()
          
  REPLACE                                ;
    APCHECKS.DCHKLPDAT WITH ldChkDat   ;
    APCHECKS.NCHKLPAMT WITH 0
  
  REPLACE                                ;
    APVENDOR.DVENLPAYD WITH ldChkDat   ;
    APVENDOR.NVENLPAYA WITH 0            ;
    APVENDOR.CVENLPAYN WITH lcChkNo      
  
  
  SELECT (lcRpTargt)
  lnRpTrgRec = RECNO()
  lnStubCnt  = 0 

  *-- Add the currency to the AP module.
  *-- Check the Checking account currency rate with gcBaseCurr with the proceed date.
  lnChkExUnt = 0
  lnChkExRat = 0
  IF gfGetMemVar('LLMULCURR')
    lnChkExRat = gfChkRate('lnChkExUnt',&lcRpTargt..CAPRCURCOD,ldChkDat,.T.,.F.)
    IF lnChkExRat = 0
      lnChkExUnt = 1
      lnChkExRat = 1
    ENDIF
  ELSE
    lnChkExUnt = 1
    lnChkExRat = 1
  ENDIF  
  
  SCAN REST 
    lnStubCnt = lnStubCnt + 1

    *-- Get the equation signs to exchange currency from invoice currency to company base currency.
    lcExSin2 = ' '
    lcExSin1 = gfGetExSin(@lcExSin2,&lcRpTargt..cCurrCode)

    *-- to exchange currency from invoice currency to approved currency.
    lcExSin4 = ' '
    lcExSin6 = ' '
    lcExSin5 = gfGetExSin(@lcExSin6,&lcRpTargt..cAprCurCode)

    *-- Add the currency to the AP module.
    *-- Calculate the amount to pay,Adjustment,Discount,1099 with the currency rate / unit.
    lnAprPayB  = ROUND(&lcRpTargt..NINVFAAP &lcExSin5 lnChkExRat &lcExSin6 lnChkExUnt,2)
    
    lnAprDisB  = ROUND(&lcRpTargt..NINVDISAP &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT,2)
    lnAprAdjB  = ROUND(&lcRpTargt..NINVADJAP &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT,2)
    lnApr1099B = ROUND(&lcRpTargt..NINVA1099 &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT,2)

    REPLACE APPAYMNT.NPAYAMNT  WITH APPAYMNT.NPAYAMNT  + ROUND(&lcRpTargt..NINVFAAP,2);
            APPAYMNT.NPAYDISC  WITH APPAYMNT.NPAYDISC  + lnAprDisB;
            APPAYMNT.NPAYADJ   WITH APPAYMNT.NPAYADJ   + lnAprAdjB;
            APPAYMNT.NINV1099A WITH APPAYMNT.NINV1099A + lnApr1099B

    REPLACE APPAYMNT.CCURRCODE  WITH APCHECKS.CCURRCODE ;
            APPAYMNT.NEXRATE   WITH lnChkExRat ;
            APPAYMNT.NCURRUNIT WITH lnChkExUnt
    =gfAdd_Info('APPAYMNT')  && Add the audit information to the record.

    REPLACE APVENDOR.NVENLPAYA WITH APVENDOR.NVENLPAYA + lnAprPayB ;
            APVENDOR.NVEN1099B WITH APVENDOR.NVEN1099B + lnApr1099B;
            APVENDOR.NVENCPAY  WITH APVENDOR.NVENCPAY  + lnAprPayB ;
            APVENDOR.NVENBAL   WITH APVENDOR.NVENBAL   - ROUND((&lcRpTargt..NINVAMTAP+&lcRpTargt..NINVDISAP+&lcRpTargt..NINVADJAP) &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT,2);
            APVENDOR.NVENOPNDR WITH APVENDOR.NVENOPNDR + ROUND(IIF(&lcRpTargt..NINVAMNT < 0,&lcRpTargt..NINVAMTAP + &lcRpTargt..NINVDISAP + &lcRpTargt..NINVADJAP &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT,0),2)

    =gfAdd_Info('APVENDOR')  && Add the audit information to the record.
                      
    REPLACE APCHECKS.NCHKLMAMT WITH APCHECKS.NCHKLMAMT + ROUND(&lcRpTargt..NINVFAAP,2)
    
    =gfAdd_Info('APCHECKS')  && Add the audit information to the record.

    REPLACE APVENHST.NVNHDISTKN WITH APVENHST.NVNHDISTKN + lnAprDisB;
            APVENHST.NVNHTOTPA  WITH APVENHST.NVNHTOTPA  + lnAprPayB;
            APVENHST.NVNHADJ    WITH APVENHST.NVNHADJ    + lnAprAdjB;
            APVENHST.NVNHMCHKP  WITH APVENHST.NVNHMCHKP  + lnAprPayB;
            APVENHST.&lcPayPrd  WITH APVENHST.&lcPayPrd  + lnAprPayB 

    =gfAdd_Info('APVENHST')  && Add the audit information to the record.
     
    SELECT APDIST 
    APPEND BLANK
    REPLACE CVENDCODE  WITH &lcRpTargt..CVENDCODE;
            CINVNO     WITH &lcRpTargt..CINVNO;
            CAPDTRTYP  WITH 'P';
            DAPDTRDAT  WITH ldChkDat;
            LAPDPOST   WITH  .F.;
            CAPDSTAT   WITH '';
            CAPDREF    WITH lcChkNo;
            CSTUBCHK   WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0');
            CAPDGLACT  WITH &lcRpTargt..CAPACCT;
            CAPDACTID  WITH 'A';
            CBATCHNO   WITH '';
            CTRNSLEDN  WITH '';
            CFISFYEAR  WITH lcFisYear;
            CFSPPRDID  WITH lcFisPrd;
            CAPSESSNO  WITH lcSession;
            CTAXCODE   WITH '';
            CBNKCODE   WITH lcRpBnkCod;
            CCHKACCT   WITH lcRpChkact;
            NAPDAMNT   WITH &lcRpTargt..NINVAMTAP + &lcRpTargt..NINVDISAP + &lcRpTargt..NINVADJAP;
            CCURRCODE  WITH &lcRpTargt..CCURRCODE;
            NEXRATE    WITH &lcRpTargt..NEXRATE;
            NCURRUNIT  WITH &lcRpTargt..NCURRUNIT;
            NEQVAMNT   WITH ROUND((&lcRpTargt..NINVAMTAP &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT + lnAprDisB + lnAprAdjB),2)

    =gfAdd_Info('APDIST')

    *-- add the paid 1099 amount in distribution file in the debit memo case as well as the invoice case [Begin]
    IF &lcRpTargt..NINVA1099 <> 0
      APPEND BLANK 
      REPLACE CVENDCODE   WITH &lcRpTargt..CVENDCODE;
              CINVNO      WITH &lcRpTargt..CINVNO;
              CAPDTRTYP   WITH 'P';
              DAPDTRDAT   WITH ldChkDat;
              LAPDPOST    WITH .F.;
              CAPDSTAT    WITH 'V';
              CAPDREF     WITH lcChkNo;
              CSTUBCHK    WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0');
              CAPDGLACT   WITH '';
              NAPDAMNT    WITH &lcRpTargt..NINVA1099;
              CAPDACTID   WITH 'B';
              CBATCHNO    WITH '';
              CTRNSLEDN   WITH '';
              CFISFYEAR   WITH lcFisYear;
              CFSPPRDID   WITH lcFisPrd;
              CAPSESSNO   WITH lcSession;
              CTAXCODE    WITH '';
              CBNKCODE    WITH lcRpBnkCod;
              CCHKACCT    WITH lcRpChkact;
              CCURRCODE   WITH &lcRpTargt..CCURRCODE;
              NEXRATE     WITH &lcRpTargt..NEXRATE;
              NCURRUNIT   WITH &lcRpTargt..NCURRUNIT;
              NEQVAMNT    WITH &lcRpTargt..NINVA1099
              
      =gfAdd_Info('APDIST')
    ENDIF

    APPEND BLANK
    REPLACE CVENDCODE   WITH &lcRpTargt..CVENDCODE;
            CINVNO      WITH &lcRpTargt..CINVNO;
            CAPDTRTYP   WITH 'P';
            DAPDTRDAT   WITH ldChkDat;
            LAPDPOST    WITH .F.;
            CAPDSTAT    WITH '';
            CAPDREF     WITH lcChkNo;
            CSTUBCHK    WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0');
            CAPDGLACT   WITH &lcRpTargt..CCHKGLACC;
            NAPDAMNT    WITH -&lcRpTargt..NINVFAAP;
            CAPDACTID   WITH 'C';
            CBATCHNO    WITH '';
            CTRNSLEDN   WITH '';
            CFISFYEAR   WITH lcFisYear;
            CFSPPRDID   WITH lcFisPrd;
            CAPSESSNO   WITH lcSession;
            CTAXCODE    WITH '';
            CBNKCODE    WITH lcRpBnkCod;
            CCHKACCT    WITH lcRpChkact;
            CCURRCODE   WITH &lcRpTargt..CAPRCURCOD;
            NEXRATE     WITH lnChkExRat;
            NCURRUNIT   WITH lnChkExUnt;
            NEQVAMNT    WITH -lnAprPayB

    =gfAdd_Info('APDIST')
      
    IF &lcRpTargt..NINVDISAP <> 0    
      APPEND BLANK
      REPLACE CVENDCODE WITH &lcRpTargt..CVENDCODE;
              CINVNO    WITH &lcRpTargt..CINVNO;
              CAPDTRTYP WITH 'P';
              DAPDTRDAT WITH ldChkDat;
              LAPDPOST  WITH .F.;
              CAPDSTAT  WITH '';
              CAPDREF   WITH lcChkNo;
              CSTUBCHK  WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0');
              NAPDAMNT  WITH -&lcRpTargt..NINVDISAP;
              CAPDACTID WITH 'S';
              CBATCHNO  WITH '';
              CTRNSLEDN WITH '';
              CFISFYEAR WITH lcFisYear;
              CFSPPRDID WITH lcFisPrd;
              CAPSESSNO WITH lcSession;
              CTAXCODE  WITH '';
              CBNKCODE  WITH lcRpBnkCod;
              CCHKACCT  WITH lcRpChkact;
              CAPDGLACT WITH IIF(!EMPTY(APCHECKS.CDISCACCT),APCHECKS.CDISCACCT,IIF(SEEK(&lcRpTargt..CDIVISION,'APDIV') AND !EMPTY(APDIV.CDISCACCT),APDIV.CDISCACCT,APSETUP.CDISCACCT));
              CCURRCODE WITH &lcRpTargt..CCURRCODE;
              NEXRATE   WITH &lcRpTargt..NEXRATE;
              NCURRUNIT WITH &lcRpTargt..NCURRUNIT;
              NEQVAMNT  WITH -lnAprDisB

      =gfAdd_Info('APDIST')
    ENDIF

    IF &lcRpTargt..NINVADJAP <> 0
      APPEND BLANK 
      REPLACE CVENDCODE WITH &lcRpTargt..CVENDCODE;
              CINVNO    WITH &lcRpTargt..CINVNO;
              CAPDTRTYP WITH 'P';
              DAPDTRDAT WITH ldChkDat;
              LAPDPOST  WITH .F.;
              CAPDSTAT  WITH '';
              CAPDREF   WITH lcChkNo;
              CSTUBCHK  WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0');
              NAPDAMNT  WITH -&lcRpTargt..NINVADJAP;
              CAPDACTID WITH 'J';
              CBATCHNO  WITH '';
              CTRNSLEDN WITH '';
              CFISFYEAR WITH lcFisYear;
              CFSPPRDID WITH lcFisPrd;
              CAPSESSNO WITH lcSession;
              CTAXCODE  WITH '';
              CBNKCODE  WITH lcRpBnkCod;
              CCHKACCT  WITH lcRpChkact;
              CAPDGLACT WITH IIF(!EMPTY(APCHECKS.CADJACCT),APCHECKS.CADJACCT,IIF(SEEK(&lcRpTargt..CDIVISION,'APDIV') AND !EMPTY(APDIV.CADJACCT),APDIV.CADJACCT,APSETUP.CADJACCT));
              CCURRCODE WITH &lcRpTargt..CCURRCODE;
              NEXRATE   WITH &lcRpTargt..NEXRATE;
              NCURRUNIT WITH &lcRpTargt..NCURRUNIT;
              NEQVAMNT  WITH -lnAprAdjB

      =gfAdd_Info('APDIST')

    ENDIF

    lnExchDiff = lnAprPayB - ROUND(&lcRpTargt..NINVAMTAP &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT,2)
    
    *-- Add the currency to the AP module.
    *-- Add the Eq. lnExchDiff
    *-- Add the base Currency,Rate = 1,Unit= 1
    IF lnExchDiff <> 0
      APPEND BLANK

      REPLACE CVENDCODE WITH &lcRpTargt..CVENDCODE;
              CINVNO    WITH &lcRpTargt..CINVNO; 
              CAPDTRTYP WITH 'P';
              DAPDTRDAT WITH ldChkDat;
              LAPDPOST  WITH .F.;
              CAPDSTAT  WITH '';
              CAPDREF   WITH lcChkNo;
              CSTUBCHK  WITH PADL(lnStubChkNo+CEILING(lnStubCnt/lnRpStub),8,'0');
              NAPDAMNT  WITH lnExchDiff;
              CAPDACTID WITH 'J';
             CBATCHNO  WITH '';
              CTRNSLEDN WITH '';
             CFISFYEAR WITH lcFisYear;
             CFSPPRDID WITH lcFisPrd;
             CAPSESSNO WITH lcSession;
             CTAXCODE  WITH '';
             CBNKCODE  WITH lcRpBnkCod;
             CCHKACCT  WITH lcRpChkact;
             CAPDGLACT WITH lcExDifAcc;
             CCURRCODE WITH gcBaseCurr;
             NEXRATE   WITH 1;
             NCURRUNIT WITH 1;
             NEQVAMNT  WITH lnExchDiff;
             NAPDLINNO WITH 1
             
      =gfAdd_Info('APDIST')  && Add the audit information to the record.
    ENDIF

*-- Update the last check #,last payment date fields. 

    REPLACE APINVHDR.NINVPAID   WITH &lcRpTargt..NINVPAID  + &lcRpTargt..NINVAMTAP;
            APINVHDR.NINVDISTK  WITH &lcRpTargt..NINVDISTK + &lcRpTargt..NINVDISAP;
            APINVHDR.NINVADJ    WITH &lcRpTargt..NINVADJ   + &lcRpTargt..NINVADJAP;
            APINVHDR.NINV1099A  WITH &lcRpTargt..NINV1099A + &lcRpTargt..NINVA1099;
            APINVHDR.CBNKCODE   WITH '';
            APINVHDR.CCHKACCT   WITH '';
            APINVHDR.CCHKGLACC  WITH '';
            APINVHDR.NINVAMTAP  WITH 0 ;
            APINVHDR.NINVDISAP  WITH 0 ;
            APINVHDR.NINVADJAP  WITH 0 ;
            APINVHDR.NINVA1099  WITH 0 ;
            APINVHDR.CAPRCURCOD WITH '';
            APINVHDR.NAPREXRAT  WITH 0 ;
            APINVHDR.NAPRCURUNT WITH 0 ;
            APINVHDR.DCHKDATE   WITH ldChkDat;
            APINVHDR.CCHKNO     WITH lcChkNo,;
            APINVHDR.nInvFAAp   WITH 0

    SELECT APINVHDR
    =gfObj_Lock(.F.)

    lnAprPayB  = 0
    lnAprDisB  = 0
    lnAprAdjB  = 0
    lnApr1099B = 0

    SELECT (lcRpTargt) 
    IF &lcRpTargt..nNoOfInv = 0
      EXIT
    ENDIF
  ENDSCAN
  GO lnRpTrgRec
ENDIF

IF &lcRpTargt..nNoOfInv = 0
  REPLACE APPAYMNT.CPAYSTAT  WITH ' '
  llEndGrp = .T.
ELSE  
  llEndGrp = .F.
ENDIF 

RETURN ''
*-- End of lfUpInv.

*!************************************************************************
*!
*!      FUNCTION lfIncNxtChk
*!
*!************************************************************************
* Increment the check number
FUNCTION lfIncNxtChk
*-- the next check number.
lcNxtChkNo = PADL(INT(VAL(lcNxtChkNo)) + 1,8,'0')

RETURN ''
*-- End of lfIncNxtChk.

*!************************************************************************
*!
*!      FUNCTION lfIniTotal
*!
*!************************************************************************
*
FUNCTION lfIniTotal

IF llPrTot
  lnTotal = 0 
  llPrTot = .F.
ENDIF 
IF lcPrintMod = 'R' &&AND lnTotal = 0
  lcInvToPrn = CINVNO+CVENDCODE+'P'
  SELECT APDIST  
  lnKeepRec=RECNO()
  PRIVATE lcOrder
  lcOrder = TAG()
  SET ORDER TO TAG INVVEND
  =SEEK(lcInvToPrn)  
  LOCATE REST WHILE CINVNO+CVENDCODE+CAPDTRTYP=lcInvToPrn FOR capdstat<>'V'
  lcChkPrntd = cStubChk
  lcMastChk = cApdRef
  SET ORDER TO TAG PAYMNTS
  =SEEK('P'+lcRpBnkCod+lcRpChkAct+lcMastChk)
   
   SUM  REST nApdAmnt WHILE CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF='P'+lcRpBnkCod+lcRpChkAct+lcMastChk  ;
             AND cStubChk<>lcChkPrntd FOR cApdStat <> 'V' AND cApdActId = 'C';
             TO lnTotal
  IF !EMPTY(lcOrder)           
    SET ORDER TO TAG &lcOrder
  ENDIF
  IF  BETWEEN(lnKeepRec,1,RECCOUNT())        
    GO lnKeepRec
  ENDIF  
  SELECT (lcRpTargt)
  lnTotal=ABS(lnTotal)
ENDIF
RETURN ''
*-- End of lfIniTotal.

*!************************************************************************
*!
*!      FUNCTION lfSumGrp
*!
*!************************************************************************
*
FUNCTION lfSumGrp

lnTotal = lnTotal + nInvFAAp
IF nNoOfInv = 0 
 llPrTot = .T.
ENDIF 
RETURN ''
*-- End of lfSumGrp

*!************************************************************************
*!
*!      FUNCTION lfAllgTest
*!
*!************************************************************************
*
FUNCTION lfAllgTest

llEndGrp  = .F.
lnTotal   = 0

lnNoOfFields = lfCreateCur()
SELECT (lcRpTargt)
APPEND BLANK
FOR lnCount = 1 TO lnNoOfFields
  lcFieldName = FIELD(lnCount)
  DO CASE

    CASE TYPE(lcFieldName) = 'C'
      REPLACE &lcFieldName WITH REPLICATE('X',FSIZE(lcFieldName))

    CASE TYPE(lcFieldName) = 'N'
      REPLACE &lcFieldName WITH VAL(REPLICATE('9',MIN(FSIZE(lcFieldName),6)))

    CASE TYPE(lcFieldName) = 'L'
      REPLACE &lcFieldName WITH .T.

    CASE TYPE(lcFieldName) = 'D'
      REPLACE &lcFieldName WITH DATE()

  ENDCASE
ENDFOR

llPrintChk = .F.
llTestChk  = .T.
=lfvRunRep()        && DO gfDispRe WITH EVAL('lcRepForm')
SHOW GET lcNxtChkNo
llTestChk = .F.
USE IN (lcRpTargt)
*-- End of lfAllgTest.

*!************************************************************************
*!
*!      FUNCTION lfwGrid
*!
*!************************************************************************
*
FUNCTION lfwGrid  

PRIVATE lcTitle
lcTitle = PROPER(ALLTRIM(WTITLE('GWDGRID')))
IF !('Session : ' $ lcTitle)
  MODIFY WINDOW GWDGRID TITLE lcTitle +PADL('Session : ' + lcSession,WCOLS()-45)
ENDIF

IF APSETUP.CAPSGLLINK = 'Y'
  SELECT('lcLinkChar')
  SET ORDER TO ACCTCODE
ENDIF  

laOpenFile = .F.

laOpenFile [1,1] = 'APVENDOR'
IF !USED('APVENDOR')
  USE &gcDataDir.APVENDOR IN 0
  laOpenFile [1,2] = .T.
ENDIF

SELECT APVENDOR
laOpenFile [1,3] = SYS(22)
SET ORDER TO VENCODE

laOpenFile [2,1] = 'APVENHST'
IF !USED('APVENHST')
  USE &gcDataDir.APVENHST IN 0
  laOpenFile [2,2] = .T.
ENDIF

SELECT APVENHST
laOpenFile [2,3] = SYS(22)
SET ORDER TO VENDYEAR

laOpenFile [3,1] = 'APBANKS'
IF !USED('APBANKS')
  USE &gcDataDir.APBANKS IN 0
  laOpenFile [3,2] = .T.
ENDIF

SELECT APBANKS
laOpenFile [3,3] = SYS(22)
SET ORDER TO BANKCODE
=SEEK(lcRpBnkCod)

laOpenFile [4,1] = 'APCHECKS'
IF !USED('APCHECKS')
  USE &gcDataDir.APCHECKS IN 0
  laOpenFile [4,2] = .T.
ENDIF

SELECT APCHECKS
laOpenFile [4,3] = SYS(22)
SET ORDER TO BANKCHECK
=SEEK(lcRpBnkCod+lcRpChkAct)

laOpenFile [5,1] = 'APINVHDR'
IF !USED('APINVHDR')
  USE &gcDataDir.APINVHDR IN 0 
  laOpenFile [5,2] = .T.
ENDIF

SELECT APINVHDR
laOpenFile [5,3] = SYS(22)
SET ORDER TO INVVEND

laOpenFile [6,1] = 'APPAYMNT'
IF !USED('APPAYMNT')
  USE &gcDataDir.APPAYMNT IN 0
  laOpenFile [6,2] = .T.
ENDIF

SELECT APPAYMNT
laOpenFile [6,3] = SYS(22)
SET ORDER TO TYPMETHDOC

laOpenFile [7,1] = 'APDIST'
IF !USED('APDIST')
  USE &gcDataDir.APDIST IN 0
  laOpenFile [7,2] = .T.
ENDIF

SELECT APDIST
laOpenFile [7,3] = SYS(22)
SET ORDER TO PAYMNTS

laOpenFile [8,1] = 'SYCFACT'
IF !USED('SYCFACT')
  USE &gcSysHome.SYCFACT IN 0 
  laOpenFile [8,2] = .T.
ENDIF

SELECT SYCFACT
laOpenFile [8,3] = SYS(22)
SET ORDER TO CFACCODE

laOpenFile [9,1] = 'SYCINT'
IF !USED('SYCINT')
  USE &gcSysHome.SYCINT IN 0
  laOpenFile [9,2] = .T.
ENDIF

SELECT SYCINT
laOpenFile [9,3] = SYS(22)
SET ORDER TO CCONTCODE

laOpenFile [10,1] = 'APDIV'
IF !USED('APDIV')
  USE &gcDataDir.APDIV IN 0
  laOpenFile [10,2] = .T.
ENDIF

SELECT APDIV
laOpenFile [10,3] = SYS(22)
SET ORDER TO DIVISION

IF !USED('CODES')
  USE (gcDataDir + 'CODES') IN 0
ENDIF

lcPrintMod = lcRpChkMod
lcOldMode = lcRpChkMod
STORE '' TO lcRpChkFrm,lcRpChkTo
lcRepForm = IIF(lcRpName='1','APREG100','APREG200')
DIME laFrxFiles[1,4]
STORE '' TO laFrxFiles
=ADIR(laFrxFiles,gcRepHome+'*.FRX')
IF !EMPTY(laFrxFiles[1,1])
  IF ASCAN(laFrxFiles,STUFF(lcRepForm,1,4,'__'+gcAct_comp)+'.FRX')>0 
    
    lcRepForm = STUFF(lcRepForm , 1 , 4 , '__' + gcAct_comp)
    
  ELSE  
    IF ASCAN(laFrxFiles,'__'+substr(lcRepForm,3)+'.FRX')>0 
      
      lcRepForm = '__' + SUBSTR(lcRepForm , 3)
      
    ENDIF
  ENDIF
ENDIF
*-- End of lfwGrid.  

*!************************************************************************
*!
*!      FUNCTION lfvGrid
*!
*!************************************************************************
*
FUNCTION lfvGrid    
FOR lnCounter = 1 TO ALEN(laOpenFile,1)
  IF laOpenFile[lnCounter,2] 
    IF USED(ALIAS(laOpenFile[lnCounter,1]))
      USE IN ALIAS(laOpenFile[lnCounter,1])
    ENDIF  
  ELSE
    SELECT (laOpenFile[lnCounter,1])
    SET ORDER TO laOpenFile[lnCounter,3]
  ENDIF
ENDFOR
*-- End of lfvGrid.
        
*!************************************************************************
*!
*!      Function lfvBnkCode
*!
*!************************************************************************
*
FUNCTION lfvBnkCode   
IF EMPTY(lcRpBnkCod)
  IF lcPrintMod = 'R' 
    STORE '' TO lcRpChkFrm,lcRpChkTo
  ENDIF
  lcRpChkAct = ''
  lcNxtChkNo = ''
  lcRpGlAcct = ''
  SHOW GET lcRpChkAct
  SHOW GET lcNxtChkNo
  SHOW GET lcRpGlAcct
  RETURN 
ENDIF

SELECT APBANKS
*-- Search for the current bank code
IF '?' $ lcRpBnkCod .OR. !SEEK(lcRpBnkCod))
  DECLARE laRpRetFld(1)
  lcBrFields    = 'CBnkCode:H="Code",CBNKLNDES:H="Description"'
  laRpRetFld[1] = ''
  =gfBrows([],'CBnkCode',"laRpRetFld",'Bank Codes ',.F.)
  IF EMPTY(laRpRetFld[1])  
    lcRpBnkCod = lcOldVal
  ELSE
    lcRpBnkCod = laRpRetFld[1]
  ENDIF
ENDIF

IF lcRpBnkCod <> lcOldVal .AND. !EMPTY(lcRpBnkCod)  && in case of press cancel and Empty(lcOldVal)
  =SEEK(lcRpBnkCod,'APCHECKS')
  lcRpChkAct = APCHECKS.CCHKACCT
  IF SEEK(lcRpBnkCod+lcRpChkAct,'APCHECKS')
    lcNxtChkNo = PADL(APCHECKS.NChkNxtPn,8,'0')
    lcRpGlAcct = APCHECKS.CCHKGLACC
    IF lcPrintMod = 'R' 
      STORE '' TO lcRpChkFrm,lcRpChkTo
    ENDIF
  ELSE
    *-- The bank has no checking accounts setup.
    =gfModalGen("INM04023B00000","DIALOG")
    lcRpBnkCod = lcOldVal
  ENDIF
  SHOW GET lcRpChkAct
  SHOW GET lcNxtChkNo
  SHOW GET lcRpGlAcct
ENDIF
*-- End of lfvBnkCode.
        
*!************************************************************************
*!
*!      FUNCTION lfvChkAcct
*!
*!************************************************************************
*  
FUNCTION lfvChkAcct            

IF EMPTY(lcRpBnkCod)
  *-- You have to enter the ð.
  =gfModalGen("TRM04066B00000","DIALOG",'bank code')
  lcRpChkAct = ''
  IF lcPrintMod = 'R' 
    STORE '' TO lcRpChkFrm,lcRpChkTo
  ENDIF
  RETURN
ENDIF

SELECT APCHECKS

*-- Search for the current checking account code
IF '?' $ lcRpChkAct .OR.!SEEK(ALLTRIM(lcRpBnkCod+lcRpChkAct))
  DECLARE laRpRetFld(1)
  lcBrFields    = 'CBnkCode:H="Bank Code",CChkAcct:H="Checking account"'
  laRpRetFld[1] = ''
  =gfBrows([lcRpBnkCod],'CChkAcct',"laRpRetFld",'Bank & Check Accounts ',.F.)

  IF EMPTY(laRpRetFld[1])  
    lcRpChkAct = lcOldVal
  ELSE
    lcRpChkAct = laRpRetFld[1]
  ENDIF
ENDIF

IF lcRpChkAct <> lcOldVal
  IF EMPTY(lcRpChkAct)  && in case of press cancel and Empty(lcOldVal)
    IF lcPrintMod = 'R' 
      STORE '' TO lcRpChkFrm,lcRpChkTo
    ENDIF
    lcNxtChkNo = ''
    lcRpGlAcct = ''
  ELSE
    lcNxtChkNo = PADL(APCHECKS.NChkNxtPn,8,'0')
    lcRpGlAcct = APCHECKS.CCHKGLACC
    IF lcPrintMod = 'R' 
      STORE '' TO lcRpChkFrm,lcRpChkTo
    ENDIF
  ENDIF
  SHOW GET lcRpChkAct
  SHOW GET lcNxtChkNo
  SHOW GET lcRpGlAcct
ENDIF  
*-- End of lfvChkAcct.

*!**************************************************************************
*!
*!      Function lfValidAcct
*!
*!**************************************************************************
*
FUNCTION lfValidAcct    
PARAMETERS lcSendData

RETURN = IIF(APSETUP.CAPSGLLINK='Y', ;
         IIF(SEEK(ALLTRIM(lcSendData),'lcLinkChar'),.T.,.F.),.T.)
*-- End of lfValidAcct.         

*!************************************************************************
*!
*!      Function lfvNxtChk
*!
*!************************************************************************
*
FUNCTION lfvNxtChk   

IF EMPTY(lcRpBnkCod)
  *** You have to enter the ð.
  =gfModalGen("TRM04066B00000","DIALOG",'bank code')
  lcNxtChkNo = ''
  RETURN
ENDIF

IF EMPTY(lcRpChkAct)
  *-- You have to enter the ð.
  =gfModalGen("TRM04066B00000","DIALOG",'checking account')
  lcNxtChkNo = ''
  RETURN
ENDIF

lcNxtChkNo = PADL(ALLTRIM(lcNxtChkNo),8,'0')
IF SEEK(lcRpBnkCod+lcRpChkAct,'APCHECKS')
  IF INT(VAL(lcNxtChkNo)) < APCHECKS.NChkNxtPn
    *** Next printed check number cannot be less than ð.
    =gfModalGen("INM04056B00000","DIALOG",ALLTRIM(STR(APCHECKS.NChkNxtPn)))
    lcNxtChkNo = PADL(APCHECKS.NChkNxtPn,8,'0')
  ENDIF
ELSE
  *-- ð account not valid.
  =gfModalGen("INM04077B00000","DIALOG",'Checking')
  lcRpChkAct = ''
  lcNxtChkNo = ''
  lcRpGlAcct = ''
  SHOW GET lcRpChkAct
  SHOW GET lcNxtChkNo
  SHOW GET lcRpGlAcct
ENDIF
SHOW GET lcNxtChkNo
*-- End of lfvNxtChk.

*!************************************************************************
*!
*!      Function lfvChkDate
*!
*!************************************************************************
* 
FUNCTION lfvChkDate   

IF !lfVDTMSG(gcPrnt_Cmp,@lcFisPrd,@lcFisYear,ldChkDat)
  ldChkDat = lcOldVal
  _CUROBJ    = _CUROBJ
ENDIF
SHOW GET ldChkDat
*-- End of lfvChkDate.

*!**************************************************************************
*!
*!      Function lfvOkAdvPay
*!
*!**************************************************************************
* 
FUNCTION lfvOkAdvPay      

IF EMPTY(lcDebMemN)
  *-- You have to enter the ð.
  =gfModalGen("TRM04066B00000","DIALOG",'debit memo number')
  _CUROBJ   = OBJNUM(lcDebMemN)
  RETURN .F.
ELSE
  SET ORDER TO TAG VENDINV IN APINVHDR
  IF SEEK(lcRpVenCod+lcDebMemN,'APINVHDR')
    *-- ð exists for the vendor ð.
    =gfModalGen("TRM04024B00000","DIALOG",'Invoice '+ALLTRIM(lcDebMemN)+ ;
                '|'+ALLTRIM(lcRpVenCod))
    _CUROBJ   = OBJNUM(lcDebMemN)
    RETURN .F.
  ENDIF  
  SET ORDER TO TAG INVVEND IN APINVHDR
ENDIF  

IF lnPaymnt <= 0
  *-- The amount to be applied should be greater than zero.
  =gfModalGen("TRM04029B00000","DIALOG")
  _CUROBJ   = OBJNUM(lnPaymnt)
  RETURN .F.
ELSE
  IF !BETWEEN(ln1099Amnt,0,lnPaymnt)
    *-- The 1099 amount must be between ð and ð.
    =gfModalGen("TRM04017B00000","DIALOG","0|"+STR(lnPaymnt))
    _CUROBJ   = OBJNUM(ln1099Amnt)
    RETURN .F.
  ENDIF
ENDIF

IF EMPTY(STRTRAN(STRTRAN(lcApAcct,'-'),'0')) 
  *-- 'ð cannot be empty.' 
  =gfModalGen("INM04074B00000","DIALOG",'AP account')
  _CUROBJ   = OBJNUM(lcApAcct)
  RETURN .F.
ENDIF  

IF ! lfValidAcct(lcApAcct)  
  *-- ð not found.
  =gfModalGen("INM04002B00000","DIALOG",'A/P account|'+ALLTRIM(lcApAcct))
  _CUROBJ   = OBJNUM(lcApAcct)
  RETURN .F.
ENDIF  

*-- If remitting to factor, and there is no factor code, do not proceed and present the following message
IF lcInvRemit = 'F' .AND. EMPTY(lcFactor)
  *-- Message : "   You have to enter the ð.  "
  *--                 <  OK  >
  =gfModalGen("TRM04066B00000","DIALOG",lcTFactor)
  _CUROBJ = OBJNUM(lcFactor)
  RETURN .F.
ENDIF
llOkAdvPay = .T.
CLEAR READ
*-- End of lfvOkAdvPay.

*!**************************************************************************
*!
*!      Function: lfvDivision
*!
*!**************************************************************************
*
FUNCTION lfvDivision  

DO CASE
  CASE _DOS
    lcAdvDiv =gfActPop(2,9,8,42,'SYCCODES','cCode_No','cDiscrep',@lcDivision)
    =lfRefresh()

  CASE _WINDOWS
    lcDivision = CODES.cdiscrep
    lcAdvDiv   = CODES.cCode_No
    
    SHOW GET lcDivision
ENDCASE

IF _WINDOWS
  DEACTIVATE POPUP puDivision
ENDIF  
*-- End of lfvDivision.

*!************************************************************************
*!
*!      Function lfShowOg
*!
*!************************************************************************
* 
FUNCTION lfShowOg     

DO CASE
  CASE lcPrintMod = 'V'
    laOGObjCnt[11] = .F.     

  CASE lcPrintMod = 'A'
  
  CASE lcPrintMod = 'R'  
  
ENDCASE

IF lcPrintMod <> 'R'  
  lcNxtChkNo = IIF(SEEK(lcRpBnkCod+lcRpChkAct,'APCHECKS'),PADL(APCHECKS.nChkNxtPn,8,'0'),'00000001')
  SHOW GET lcNxtChkNo
ENDIF
  
=lfOGShowGet()             && Refresh All Objects .
*-- End of lfShowOg.

*!************************************************************************
*!
*!      Function lfSortAddr
*!
*!************************************************************************
* Return the address sorted by the user
FUNCTION lfSortAddr   

DIMENSION laAddrOrd(3,2)
*-- Assign seq. no to the array by the order of the address fields in the file.
laAddrOrd[1,1]  = '3'
laAddrOrd[2,1]  = '4'
laAddrOrd[3,1]  = '5'    
lcReturnVal     = ' '

IF SEEK(APBANKS.cCont_Code,'SYCINT')
  *-- Assign the order of the address to the second column of the array.
  laAddrOrd[1,2]  = SYCINT.NPART3ORD
  laAddrOrd[2,2]  = SYCINT.NPART4ORD
  laAddrOrd[3,2]  = SYCINT.NPART5ORD
  =ASORT(laAddrOrd,2)  && Sort the Temp array.
  FOR lnCounter = 3 TO 5
    lcPostion   = laAddrOrd[lnCounter,1]
    lcReturnVal = ALLTRIM(lcReturnVal)+' '+ALLTRIM(APBANKS.cAddress&lcPostion)
  ENDFOR  
ELSE
  *-- We are going to take the address as it is.
  lcReturnVal = ALLTRIM(APBANKS.CADDRESS3) + ' ' + ;
                ALLTRIM(APBANKS.CADDRESS4) + ' ' + ;
                ALLTRIM(APBANKS.CADDRESS5)
ENDIF
RETURN lcReturnVal
*-- End of lfSortAddr.

*!************************************************************************
*!
*!      FUNCTION lfvVenCode
*!
*!************************************************************************
*
FUNCTION lfvVenCode  

lcRpCurFld  = VARREAD()

IF EMPTY(&lcRpCurFld.)
  RETURN
ENDIF

SELECT APVENDOR

*-- Search for the current vendor code
IF ('?' $ &lcRpCurFld. .OR. !SEEK(&lcRpCurFld.)) 
  DECLARE laRpRetFld(1)
  lcBrFields    = 'CVendCode:H="Vendor Code",CVENCOMP:H="Vendor name"'
  laRpRetFld[1] = ''
  =gfBrows([],'CVENDCODE',"laRpRetFld",'Vendor codes ',.F.)

  IF EMPTY(laRpRetFld[1])  
    &lcRpCurFld = lcOldVal
  ELSE
    IF APVENDOR.CVENPRIOR = '0'
      *-- Vendor ð has payment priority zero.  This vendor is on hold.
      =gfModalGen("INM04060B00000","DIALOG",ALLTRIM(APVENDOR.CVENDCODE))
      RETURN 
    ELSE  
      &lcRpCurFld = laRpRetFld[1]
    ENDIF
  ENDIF
ELSE
  IF APVENDOR.CVENPRIOR = '0'
    *-- Vendor ð has payment priority zero.  This vendor is on hold.
    =gfModalGen("INM04060B00000","DIALOG",ALLTRIM(APVENDOR.CVENDCODE))
    &lcRpCurFld = lcOldVal
    _CUROBJ     = _CUROBJ
    RETURN 
  ENDIF  
ENDIF

SHOW GET (lcRpCurFld)
lcRpVenCod = &lcRpCurFld 
*-- End of lfvVenCode.

*!************************************************************************
*!
*!      FUNCTION lfReValidate
*!
*!************************************************************************
* Before printing this function revalid the needed informations
FUNCTION lfReValidate  
*C128816,1 MMR 09/26/2005 New custom to print 3 day payment and international payment txt files.
llProgram = .F.
*C128816,1 MMR.[End]
DO CASE
  CASE lcPrintMod = 'R' .AND. (EMPTY(lcRpChkFrm) OR EMPTY(lcRpChkTo))
    *-- ð cannot be empty.
    =gfModalGen("INM04074B00000","DIALOG",'Reprint check number')
    RETURN .F.

  CASE EMPTY(lcRpBnkCod)            
    *-- ð cannot be empty   
    =gfModalGen("INM04074B00000","DIALOG",'Bank code')
    RETURN .F.
      
  CASE !SEEK(lcRpBnkCod,'APBANKS')   
    *-- ð not found.
    =gfModalGen("INM04002B00000","DIALOG",'Bank code')
    RETURN .F.

  CASE EMPTY(lcRpChkAct)                   
    *-- ð cannot be empty.
    =gfModalGen("INM04074B00000","DIALOG",'Checking account')
    RETURN .F.

  CASE !SEEK(ALLTRIM(lcRpBnkCod+lcRpChkAct),'APCHECKS')   
    *-- ð account not valid.
    =gfModalGen("INM04077B00000","DIALOG",'Checking')
    RETURN .F.

  CASE INT(VAL(lcNxtChkNo)) < APCHECKS.NChkNxtPn    
    *-- Next printed check number cannot be less than ð.
    =gfModalGen("INM04056B00000","DIALOG",ALLTRIM(STR(APCHECKS.NChkNxtPn)))
    RETURN .F.

  CASE lcPrintMod <> 'R' .AND. !lfVlDate(gcPrnt_Cmp,@lcFisPrd,@lcFisYear,ldChkDat)
    *-- Cehck date is not valid.
    =gfModalGen("INM04052B00000","DIALOG")
    RETURN .F.

  CASE lcPrintMod <> 'R' .AND. EMPTY(STRTRAN(STRTRAN(lcRpGlAcct,'-'),'0')) 
    *-- ð cannot be empty..
    =gfModalGen("INM04074B00000","DIALOG",'G/L account')
    RETURN .F.

  CASE lcPrintMod <> 'R' .AND. ! lfValidAcct(lcRpGlAcct)  
    *-- ð not found.
    =gfModalGen("INM04002B00000","DIALOG","G/L account|"+ALLTRIM(lcRpGlAcct))
    RETURN .F.

ENDCASE

IF lcPrintMod = 'A'   
  IF EMPTY(lcRpVenCod)
    *-- ð cannot be empty.
    =gfModalGen("INM04074B00000","DIALOG",'Vendor code')
    RETURN .F.
  ELSE
    IF SEEK(ALLTRIM(lcRpVenCod),'APVENDOR')
      IF APVENDOR.CVENPRIOR = '0'
        *-- Vendor ð has payment priority zero.  This vendor is on hold.
        =gfModalGen("INM04060B00000","DIALOG",ALLTRIM(lcRpVenCod))
        RETURN .F.
      ENDIF
    ELSE
      *-- ð not found.
      =gfModalGen("INM04002B00000","DIALOG",'Vendor code')
      RETURN .F.
    ENDIF
  ENDIF  
ENDIF

IF lcPrintMod = 'V'   
  lcRpVenCod = laOGFxFlt[1,6]
  IF ! EMPTY(lcRpVenCod)
    DECLARE laVendor[1]
    =gfSubStr(lcRpVenCod, @laVendor, "|")   
    FOR lnCounter = 1 TO ALEN(laVendor,1)
      =SEEK(ALLTRIM(laVendor[lnCounter]),'APVENDOR')
      IF APVENDOR.CVENPRIOR = '0'
        *-- Vendor ð has payment priority zero.  This vendor is on hold.
        =gfModalGen("INM04060B00000","DIALOG",ALLTRIM(laVendor[lnCounter]))
        RETURN .F.
      ENDIF
    ENDFOR  
  ENDIF  
ENDIF  
*-- End of lfReValidate.

*!**************************************************************************
*!
*!      Function: lfvRemit
*!
*!**************************************************************************
*
FUNCTION lfvRemit
*-- Default factor for the vendor
lcFactor   = IIF(EMPTY(lcFactor), APVENDOR.cFacCode, lcFactor)
lcInvRemit = gfRemit(lcInvRemit, .T. , lcRpVenCod, lcFactor,;
                @lcRemitStat, 'lcRem1', 'lcRem2', 'lcRem3', 'lcRem4', 'lcRem5', 'lcRem6',;
                2, 40, 'laRemitTo', @lcRemitTo, lnRemitLen)
lcFactor   = IIF(lcInvRemit = 'F', lcFactor, SPACE(6))
lcFactStat = IIF(lcInvRemit = 'F', 'ENABLE', 'DISABLE')
SHOW GET lcFactor &lcFactStat
SHOW GET ibFactor &lcFactStat

IF _DOS
  =lfRefresh()
ENDIF
*-- End of lfvRemit.  

*!**************************************************************************
*!
*!      Function: lfvFactor
*!
*!**************************************************************************
* Valid function for get field lcFactor
FUNCTION lfvFactor
IF llBrowse .OR. !EMPTY(lcFactor) 
  IF lfGetFac(lcOldVal, llBrowse)
    lcRem1      = SYCFACT.cFacComp
    lcRem2      = gfGetAdr('SYCFACT', 'CFACCODE',.F.,.F.,1)
    lcRem3      = gfGetAdr('SYCFACT', 'CFACCODE',.F.,.F.,2)
    lcRem4      = gfGetAdr('SYCFACT', 'CFACCODE',.F.,.F.,3)
    lcRem5      = gfGetAdr('SYCFACT', 'CFACCODE',.F.,.F.,4)     
    lcRem6      = gfGetAdr('SYCFACT', 'CFACCODE',.F.,.F.,5)
  ENDIF  
ELSE
  STORE SPACE(40) TO lcRem1, lcRem2, lcRem3, lcRem4, lcRem5, lcRem6
ENDIF   
SHOW GET lcRem1
SHOW GET lcRem2 
SHOW GET lcRem3 
SHOW GET lcRem4 
SHOW GET lcRem5 
SHOW GET lcRem6 
llBrowse = .F.
*-- End of lfvFactor.

*!************************************************************************
*!
*!      FUNCTION lfCreateCur
*!
*!************************************************************************
* function to create cursor
FUNCTION lfCreateCur   

DIMENSION laFieldStr(1,4)
SELECT APINVHDR
lnFieldNo = AFIELD(laFieldStr)

DIMENSION laFieldStr(lnFieldNo+3,4)
laFieldStr[lnFieldNo+1,1] = 'CADDRESS'
laFieldStr[lnFieldNo+1,2] = 'C'
laFieldStr[lnFieldNo+1,3] = 240
laFieldStr[lnFieldNo+1,4] = 0

laFieldStr[lnFieldNo+2,1] = 'nNoOfInv'
laFieldStr[lnFieldNo+2,2] = 'N'
laFieldStr[lnFieldNo+2,3] = 6
laFieldStr[lnFieldNo+2,4] = 0

laFieldStr[lnFieldNo+3,1] = 'cPageNo'
laFieldStr[lnFieldNo+3,2] = 'C'
laFieldStr[lnFieldNo+3,3] = 4
laFieldStr[lnFieldNo+3,4] = 0

*-- Create the tempery file to send to the report
CREATE CURSOR &lcRpTargt FROM ARRAY laFieldStr

RETURN lnFieldNo + 3
*-- End of lfCreateCur.

*!************************************************************************
*!
*!      FUNCTION lfSetRela
*!
*!************************************************************************
*
FUNCTION lfSetRela   

=SEEK(gcAct_Comp,'SYCCOMP')
SELECT(lcRpTargt)
SET RELATION TO Cinvno+Cvendcode      INTO APINVHDR ADDITIVE
SET RELATION TO Cvendcode             INTO APVENDOR ADDITIVE
SET RELATION TO Cvendcode + lcFisYear INTO APVENHST ADDITIVE
*-- End of lfSetRela.

*!************************************************************************
*!
*!      FUNCTION lfLokBank
*!
*!************************************************************************
* Lock the bank record  
FUNCTION lfLokBank

llRetVal = .T.

IF !llPrintChk
  RETURN llRetVal
ENDIF

llRetVal = .T.
SELECT APBANKS
IF SEEK(lcRpBnkCod)
  IF gfObj_Lock(.T.)
    IF SEEK(lcRpBnkCod+lcRpChkAct,'APCHECKS')
      IF INT(VAL(lcNxtChkNo)) < APCHECKS.NCHKNXTPN
        *** Next check number is ð. Do you wish to proceed with check printing?
        lnOption =gfModalGen("QRM04086B00012","DIALOG",ALLTRIM(STR(APCHECKS.NCHKNXTPN)))
        lcNxtChkNo = PADL(APCHECKS.NCHKNXTPN,8,'0')
        SHOW GET lcNxtChkNo     && Refresh Next Check No.
        IF lnOption = 2
          llRetVal = .F.
          =gfObj_Lock(.F.)
        ENDIF
      ENDIF
    ELSE
      *-- ð has been deleted.
      =gfModalGen("QRM04095B00000","DIALOG",'Checking account')
      llRetVal = .F.
      =gfObj_Lock(.F.)
    ENDIF  
  ELSE
    *-- Bank ð is being edited by user ð.
    =gfModalGen("INM04085B00000","DIALOG",lcRpBnkCod+'|'+CLOK_USER)
    llRetVal = .F.
  ENDIF  
ELSE    
  *-- ð has been deleted.
  =gfModalGen("QRM04095B00000","DIALOG",'Bank code')
  llRetVal = .F.
ENDIF

RETURN llRetVal
*-- End of lfLokBank.

*!************************************************************************
*!
*!      FUNCTION lfUnLokBank
*!
*!************************************************************************
* UnLock the bank record  
FUNCTION lfUnLokBank

*-- Unlock the APBANKS record after the preview.
SELECT APBANKS
=SEEK(lcRpBnkCod)
=gfObj_Lock(.F.)
*-- End of lfUnLokBank.

*!**************************************************************************
*!
*!      Function lfAdvPay
*!
*!**************************************************************************
* 
FUNCTION lfAdvPay   
SELECT (lcRpTargt)

APPEND BLANK
REPLACE CVENDCODE   WITH lcRpVenCod  ;
        CINVNO      WITH lcDebMemN   ;
        CDIVISION   WITH lcAdvDiv    ;
        DINVDATE    WITH ldChkDat    ;
        CINVREF     WITH lcRef       ;
        CINVREMIT   WITH lcInvRemit  ;
        CFACCODE    WITH lcFactor    ; 
        COUTCOMP    WITH lcRem1      ;
        COUTADDR1   WITH lcRem2      ;
        COUTADDR2   WITH lcRem3      ;
        COUTADDR3   WITH lcRem4      ;
        COUTADDR4   WITH lcRem5      ;  
        COUTADDR5   WITH lcRem6      ;  
        NINVAMNT    WITH 0           ;
        NINVDISOF   WITH 0           ;
        NINVAMTAP   WITH lnPaymnt    ;
        NINVDISAP   WITH 0           ;
        NINVADJAP   WITH 0           ;
        NINVPAID    WITH 0           ;
        NINVDISTK   WITH 0           ;
        NINVADJ     WITH 0           ;
        NINVA1099   WITH ln1099Amnt  ;
        NINV1099A   WITH 0           ;
        CVENPMETH   WITH 'P'         ;
        CTERMCODE   WITH ''          ;
        NTERDUED    WITH 0           ;
        NTERDISCD   WITH 0           ;
        NTERDISCR   WITH 0           ;
        DINVDUDAT   WITH ldChkDat    ;
        CBNKCODE    WITH ''          ;
        CCHKACCT    WITH ''          ;
        CCHKGLACC   WITH ''          ;
        CCHKNO      WITH ''          ;
        DCHKDATE    WITH {}          ;
        CINVSTAT    WITH 'A'         ;
        CVENCCVEN   WITH ''          ;
        CVENCCINV   WITH ''          ;
        CAPACCT     WITH lcApAcct    ;
        CFISFYEAR   WITH lcFisYear   ;
        CFSPPRDID   WITH lcFisPrd    ;
        nNoOfInv    WITH 0           ;
        nInvFaAp    WITH lnPaymnt
     
IF llPrintChk
  SELECT APINVHDR
  APPEND BLANK

*-- Update the last check #,last payment date fields.

  IF TYPE('APINVHDR.DPOSTDATE') = 'D'

    REPLACE CVENDCODE   WITH lcRpVenCod  ;
            CINVNO      WITH lcDebMemN   ;
            CDIVISION   WITH lcAdvDiv    ;
            DINVDATE    WITH ldChkDat    ;
            CINVREF     WITH lcRef       ;
            CINVREMIT   WITH lcInvRemit  ;
            CFACCODE    WITH lcFactor    ;
            COUTCOMP    WITH lcRem1      ;
            COUTADDR1   WITH lcRem2      ;
            COUTADDR2   WITH lcRem3      ;
            COUTADDR3   WITH lcRem4      ;
            COUTADDR4   WITH lcRem5      ;  
            COUTADDR5   WITH lcRem6      ;                
            NINVAMNT    WITH 0 - lnPaymnt;
            NINVDISOF   WITH 0           ;
            NINVAMTAP   WITH 0           ;
            NINVDISAP   WITH 0           ;
            NINVADJAP   WITH 0           ;
            NINVPAID    WITH 0           ;
            NINVDISTK   WITH 0           ;
            NINVADJ     WITH 0           ;
            NINVA1099   WITH 0           ;
            CVENPMETH   WITH 'P'         ;
            CTERMCODE   WITH ''          ;
            NTERDUED    WITH 0           ;
            NTERDISCD   WITH 0           ;
            NTERDISCR   WITH 0           ;
            DINVDUDAT   WITH ldChkDat    ;
            CBNKCODE    WITH ''          ;
            CCHKACCT    WITH ''          ;
            CCHKGLACC   WITH ''          ;
            CCHKNO      WITH ''          ;
            DCHKDATE    WITH {}          ;
            CINVSTAT    WITH 'A'         ;
            CVENCCVEN   WITH ''          ;
            CVENCCINV   WITH ''          ;
            CAPACCT     WITH lcApAcct    ;
            CFISFYEAR   WITH lcFisYear   ;
            CFSPPRDID   WITH lcFisPrd    ;
            CVENPRIOR   WITH APVENDOR.CVENPRIOR ;
            DCHKDATE    WITH ldChkDat    ;
            CCHKNO      WITH lcNxtChkNo  ;
            DPOSTDATE   WITH ldChkDat    ;
            nInvFAAp    WITH 0           ;
            cCurrCode   WITH lcCurrCode  ;
            nCurrUnit   WITH lnCurrUnit  ;
            nExRate     WITH lnExRate    

  ELSE
    
    REPLACE CVENDCODE   WITH lcRpVenCod  ;
            CINVNO      WITH lcDebMemN   ;
            CDIVISION   WITH lcAdvDiv    ;
            DINVDATE    WITH ldChkDat    ;
            CINVREF     WITH lcRef       ;
            CINVREMIT   WITH lcInvRemit  ;
            CFACCODE    WITH lcFactor    ;
            COUTCOMP    WITH lcRem1      ;
            COUTADDR1   WITH lcRem2      ;
            COUTADDR2   WITH lcRem3      ;
            COUTADDR3   WITH lcRem4      ;
            COUTADDR4   WITH lcRem5      ;  
            COUTADDR5   WITH lcRem6      ;                
            NINVAMNT    WITH 0 - lnPaymnt;
            NINVDISOF   WITH 0           ;
            NINVAMTAP   WITH 0           ;
            NINVDISAP   WITH 0           ;
            NINVADJAP   WITH 0           ;
            NINVPAID    WITH 0           ;
            NINVDISTK   WITH 0           ;
            NINVADJ     WITH 0           ;
            NINVA1099   WITH 0           ;
            CVENPMETH   WITH 'P'         ;
            CTERMCODE   WITH ''          ;
            NTERDUED    WITH 0           ;
            NTERDISCD   WITH 0           ;
            NTERDISCR   WITH 0           ;
            DINVDUDAT   WITH ldChkDat    ;
            CBNKCODE    WITH ''          ;
            CCHKACCT    WITH ''          ;
            CCHKGLACC   WITH ''          ;
            CCHKNO      WITH ''          ;
            DCHKDATE    WITH {}          ;
            CINVSTAT    WITH 'A'         ;
            CVENCCVEN   WITH ''          ;
            CVENCCINV   WITH ''          ;
            CAPACCT     WITH lcApAcct    ;
            CFISFYEAR   WITH lcFisYear   ;
            CFSPPRDID   WITH lcFisPrd    ;
            CVENPRIOR   WITH APVENDOR.CVENPRIOR ;
            DCHKDATE    WITH ldChkDat    ;
            CCHKNO      WITH lcNxtChkNo  ;
            nInvFAAp    WITH 0           ; 
            cCurrCode   WITH lcCurrCode  ;
            nCurrUnit   WITH lnCurrUnit  ;
            nExRate     WITH lnExRate    
            
  ENDIF

    =gfAdd_Info()

  SELECT APPAYMNT
  APPEND BLANK
  
     REPLACE CPAYTYPE  WITH 'P'      ;
          CPAYDOCNO WITH lcNxtChkNo  ;
          CPAYMETH  WITH 'P'         ;
          CPAYSTAT  WITH ''          ;
          DPAYDATE  WITH ldChkDat    ;
          CFISFYEAR WITH lcFisYear   ;
          CFSPPRDID WITH lcFisPrd    ;
          DPAYVDATE WITH {}          ;
          CPAYCLNO  WITH lcRpVenCode ;
          CPAYCOMP  WITH lcRem1      ;
          NPAYAMNT  WITH lnPaymnt    ;
          NPAYDISC  WITH 0           ;
          NPAYADJ   WITH 0           ;
          LPAYADVAN WITH .T.         ;
          NINV1099A WITH ln1099Amnt  ;
          CBNKCODE  WITH lcRpBnkCod  ;
          CPAYRECST WITH 'O'         ;        
          CCHKACCT  WITH lcRpChkAct  ;
 		  cCurrCode WITH lcCurrCode  ; 
		  nCurrUnit WITH lnCurrUnit  ;
		  nExRate   WITH lnExRate
    =gfAdd_Info()

  REPLACE APCHECKS.DCHKLPDAT WITH ldChkDat ;
          APCHECKS.NCHKLPAMT WITH lnPaymnt

  *-- Update the next check number
  REPLACE APCHECKS.NCHKNXTPN WITH INT(VAL(lcNxtChkNo)) + 1
 
  REPLACE APVENDOR.DVENLPAYD WITH ldChkDat                        ;
          APVENDOR.NVENLPAYA WITH lnPaymnt                        ;
          APVENDOR.NVEN1099B WITH APVENDOR.NVEN1099B + ln1099Amnt ;
          APVENDOR.NVENOPNDR WITH APVENDOR.NVENOPNDR + lnPaymnt   ;
          APVENDOR.NVENCPAY  WITH APVENDOR.NVENCPAY  + lnPaymnt   ;                                        
          APVENDOR.NVENBAL   WITH APVENDOR.NVENBAL   - lnPaymnt   ;
          APVENDOR.CVENLPAYN WITH lcNxtChkNo

  SELECT APVENHST
  REPLACE APVENHST.NVNHTOTPA  WITH APVENHST.NVNHTOTPA  + lnPaymnt ;
          APVENHST.NVNHPCHKP  WITH APVENHST.NVNHPCHKP  + lnPaymnt ;
          APVENHST.&lcPayPrd  WITH APVENHST.&lcPayPrd  + lnPaymnt

  SELECT APDIST 
  APPEND BLANK
  
  REPLACE CVENDCODE   WITH lcRpVenCod ;
          CINVNO      WITH lcDebMemN  ;
          CAPDTRTYP   WITH 'P'        ;
          DAPDTRDAT   WITH ldChkDat   ;
          LAPDPOST    WITH .F.        ;
          CAPDSTAT    WITH ''         ;
          CAPDREF     WITH lcNxtChkNo ;
          CSTUBCHK    WITH lcNxtChkNo ;
          CAPDGLACT   WITH lcApAcct   ;
          NAPDAMNT    WITH lnPaymnt   ;
          CAPDACTID   WITH 'A'        ;
          CBATCHNO    WITH ''         ;
          CTRNSLEDN   WITH ''         ;
          CFISFYEAR   WITH lcFisYear  ;
          CFSPPRDID   WITH lcFisPrd   ;
          CAPSESSNO   WITH lcSession  ;
          CTAXCODE    WITH ''         ;
          CBNKCODE    WITH lcRpBnkCod ;
          CCHKACCT    WITH lcRpChkAct ;
          NAPDLINNO   WITH 0          ;
          nEqvAmnt    WITH lnEqvAmnt  ; 
          nExRate     WITH lnExRate ;
          cCurrCode   WITH lcCurrCode ;
          nCurrUnit   WITH lnCurrUnit
  
  =gfAdd_Info()
  APPEND BLANK
  
  REPLACE CVENDCODE   WITH lcRpVenCod   ;
          CINVNO      WITH lcDebMemN    ;
          CAPDTRTYP   WITH 'P'          ;
          DAPDTRDAT   WITH ldChkDat     ;
          LAPDPOST    WITH .F.          ;
          CAPDSTAT    WITH ''           ;
          CAPDREF     WITH lcNxtChkNo   ;
          CSTUBCHK    WITH lcNxtChkNo   ;
          CAPDGLACT   WITH lcRpGlAcct   ;
          NAPDAMNT    WITH 0 - lnPaymnt ;
          CAPDACTID   WITH 'C'          ;
          CBATCHNO    WITH ''           ;
          CTRNSLEDN   WITH ''           ;
          CFISFYEAR   WITH lcFisYear    ;
          CFSPPRDID   WITH lcFisPrd     ;
          CAPSESSNO   WITH lcSession    ;
          CTAXCODE    WITH ''           ;
          CBNKCODE    WITH lcRpBnkCod   ;
          CCHKACCT    WITH lcRpChkAct   ;
          NAPDLINNO   WITH 1            ; 
          nEqvAmnt    WITH 0 - lnEqvAmnt; 
          nExRate     WITH lnExRate   ;
          cCurrCode   WITH lcCurrCode   ;
          nCurrUnit   WITH lnCurrUnit
  =gfAdd_Info()
ENDIF
*-- End of lfAdvPay.

*!************************************************************************
*!
*!      FUNCTION lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep   

IF USED(lcRpTargt)
  USE IN ALIAS(lcRpTargt)
  ERASE(gcWorkDir+lcRpTargt+'DBF')
  ERASE(gcWorkDir+lcRpTargt+'FPT')
ENDIF

glEscPrs = .F.
*-- End of lfClearRep.

*!************************************************************************
*!
*!      FUNCTION lfBeforeRev
*! 
*!************************************************************************
* 
FUNCTION lfBeforeRev 

IF ! llTestChk
  IF lcPrintMod = 'A'
    RETURN lfReValidate() .AND. lfAdvance()
  ELSE
   RETURN lfReValidate() 
  ENDIF
ENDIF
*-- End of lfBeforeRev.  

*!************************************************************************
*!
*!      FUNCTION lfAdvance
*!
*!************************************************************************
* 
FUNCTION lfAdvance

lcDebMemN  = SPACE(12)        && Debit memo number
lcAdvDiv   = " "              && var. to holds active division code
lcRef      = SPACE(16)        && Reference
lcFactor   = SPACE(6)
lnPaymnt   = 0                && Payment amount
ln1099Amnt = 0                && 1099 amount
lcRem1     = SPACE(40)        && Address 1
lcRem2     = SPACE(40)        && Address 2
lcRem3     = SPACE(40)        && Address 3
lcRem4     = SPACE(40)        && Address 4
lcRem5     = SPACE(40)        && Address 5
lcRem6     = SPACE(40)        && Address 6
lnRemit    = 1
llOkAdvPay = .F.
llBrowse   = .F.              && Variable to hold left mouse clicked or not. 

=SEEK(ALLTRIM(lcRpVenCod),'APVENDOR')
lc1099Stat = IIF(EMPTY(APVENDOR.cVen1099T),'DISABLE','ENABLE')

DECLARE laRemitTo[3,2]

*-- Prepare Remit to array from SYDFIELD and get its maximum width
lcFactStat   = 'DISABLE'
lcRemitStat  = 'DISABLE'

lnRemitLen   = gfGetVld('cInvRemit',@laRemitTo)
lcRemitTo    = laRemitTo[1,1]   
lcInvRemit   = laRemitTo[1,2]   
puRemitTo    = 1
lcRem1       = APVENDOR.cVenComp
lcRem2       = gfGetAdr('APVENDOR', 'VENCODE',lcRpVenCod,APVENDOR.cCont_Code,1)
lcRem3       = gfGetAdr('APVENDOR', 'VENCODE',lcRpVenCod,APVENDOR.cCont_Code,2)
lcRem4       = gfGetAdr('APVENDOR', 'VENCODE',lcRpVenCod,APVENDOR.cCont_Code,3)
lcRem5       = gfGetAdr('APVENDOR', 'VENCODE',lcRpVenCod,APVENDOR.cCont_Code,4)
lcRem6       = gfGetAdr('APVENDOR', 'VENCODE',lcRpVenCod,APVENDOR.cCont_Code,5)

SELECT CODES

SET FILTER TO (CDefCode+CRLTFIELD+CFLD_NAME = 'N'+'N' + 'CDIVISION');
              .OR. (cDefCode+CRLTFIELD+CFLD_NAME = 'N'+'N' + 'N/A')
LOCATE
SET ORDER TO TAG CODES

IF _WINDOWS

  DEFINE POPUP puDivision prompt field CODES.cdiscrep scroll;
  FROM 5.90,12.90 TO 10.20,44.81;
  MESSAGE gfObj_msg()
  
  ON SELECTION POPUP puDivision DO lfvDivision
ENDIF  

lcDivision = gfCodDes(APVENDOR.cDivision , 'CDIVISION')

IF SEEK('N'+APVENDOR.cDivision + 'N' + 'CDIVISION')
  lcAdvDiv    = APVENDOR.cDivision
ENDIF

IF ! EMPTY(APVENDOR.cApAcct)
  lcApAcct   = APVENDOR.cApAcct
ELSE
  IF ! EMPTY(lcAdvDiv)  
    =SEEK(lcAdvDiv,'APDIV')
    IF EMPTY(APDIV.cApAcct)
      lcApAcct = APSETUP.CAPACCT    
    ELSE
      lcApAcct = APDIV.cApAcct
    ENDIF  
  Else
    lcApAcct   = APSETUP.CAPACCT       
  ENDIF
ENDIF    

DO (gcRepHome + gcAct_Appl + '\APCKADVP.SPR')   && Run the advance payment screen 

SELECT CODES
SET FILTER TO 

IF _WINDOWS
  RELEASE POPUPS puDivision
ENDIF 

RETURN llOkAdvPay
*-- End of lfAdvance.

*!************************************************************************
*!
*!      FUNCTION lfvRepName
*!
*!************************************************************************
* 
FUNCTION lfvRepName

lcRepForm = IIF(lcRpName ='1','APREG100','APREG200')
DIME laFrxFiles[1,4]
STORE '' TO laFrxFiles
=ADIR(laFrxFiles,gcRepHome+'*.FRX')
IF !EMPTY(laFrxFiles[1,1])
  IF ASCAN(laFrxFiles,STUFF(lcRepForm,1,4,'__'+gcAct_comp)+'.FRX')>0 
    
    lcRepForm = STUFF(lcRepForm , 1 , 4 , '__' + gcAct_comp)
    
  ELSE  
    IF ASCAN(laFrxFiles,'__'+substr(lcRepForm,3)+'.FRX')>0 
      
      lcRepForm = '__' + SUBSTR(lcRepForm , 3)
      
    ENDIF
  ENDIF
ENDIF  

*-- Change the number of lines per stub in laser and dot matrix form.
lnRpStub  = 11
=lfRepPltFr(lcRepForm)
*-- End of lfvRepName.

*!************************************************************************
*!
*!      FUNCTION lfvPrintMode
*!
*!************************************************************************
* 
FUNCTION lfvPrintMode

IF lcPrintMod <> lcOldMode
  lcOldMode = lcPrintMod
  lcRpChkMod = lcPrintMod
  =lfChangeGrid('APREG10'+lcPrintMod)
ENDIF 
*C128816,1 MMR 09/26/2005 New custom to print 3 day payment and international payment txt files.
llBacs=.F.
=lfvBacs()
*C128816,1 MMR.[End]
*-- End of lfvPrintMode.

*!************************************************************************
*!
*!      FUNCTION lfvRePrnChk
*!
*!************************************************************************
* 
FUNCTION lfvRePrnChk
IF lcRePrnChk == lcOldVal
 RETURN
ELSE
 lcChkToPrn = '' 
ENDIF
 
IF !EMPTY(lcRePrnChk)  && By Mohamed Hassan 10/01/95
  lcRePrnChk = PADL(ALLTRIM(lcRePrnChk),8,'0')
  SHOW GET lcRePrnChk
ELSE   && By Mohamed Hassan 10/01/95
  RETURN    && By Mohamed Hassan 10/01/95
ENDIF    && By Mohamed Hassan 10/01/95
SELECT APPAYMNT

*IF !EMPTY(lcRePrnChk) .AND. !SEEK('P'+'P'+lcRePrnChk)  && By Mohamed Hassan 10/01/95
IF lcRePrnChk <> '00000000' .AND. !SEEK('P'+'P'+lcRePrnChk) 
  DIMENSION laTemp[1]
  laTemp     = ' '
  lcFile_Ttl = 'Printed checks'
  lcBrFields = "cPayDocNo :H= 'Check Code',"     +;
               "dPayDate  :H= 'Check Date',"     +;
               "cPayClNo  :H= 'Vendor Code',"    +;
               "nPayAmnt  :H= 'Net  amount',"     +;
               "nPayDisc  :H= 'Dis. amount',"   +;
               "nPayAdj   :H= 'Adj. amount'," +;
               "cPayAdvan = IIF(lPayAdvan,'Yes','No '):H= 'Adv'," +;
               "cAdd_User :H= 'Printed by'," +;
               "dAdd_Date :H= 'On date'," +;
               "cAdd_Time :H= 'On time'"

  =gfBrows([FOR CPAYTYPE+CPAYMETH='P'+'P' .AND. CPAYSTAT <> 'V'],'cPayDocNo','laTemp')
  
  IF EMPTY(laTemp[1])
    lcRePrnChk = lcOldVal
    _CUROBJ    = _CUROBJ
    RETURN
  ELSE 
    lcRePrnChk = laTemp[1]
  ENDIF
ENDIF  

*IF EMPTY(lcRePrnChk) 
IF EMPTY(lcRePrnChk) .OR. lcRePrnChk = '00000000'  && By Mohamed Hassan 10/01/95
  laOGObjCnt[10] = .F.
  lcRpBnkCod = ''
  lcRpChkAct = ''
  lcNxtChkNo = ''
  SHOW GET lcRpBnkCod
  SHOW GET lcRpChkAct
  SHOW GET lcNxtChkNo DISABLE
ELSE
  IF APPAYMNT.cPayStat = 'V'
    *** You cannot reprint a void check.
    =gfModalGen("INM04098B00000","DIALOG")
    lcRePrnChk = lcOldVal
    _CUROBJ    = _CUROBJ
    RETURN 
  ENDIF 

  lcFiscalP = ''
  lcFiscalY = ''
  IF !lfVlDate(gcPrnt_Cmp,@lcFiscalP,@lcFiscalY,APPAYMNT.DPAYDATE)
    *** The check date is out of posting window.
    =gfModalGen("INM04102B00000","DIALOG")
    lcRePrnChk = lcOldVal
    _CUROBJ    = _CUROBJ
    RETURN 
  ENDIF 

  IF !lcRePrnChk == lcOldVal .OR. lcRePrnChk <> '00000000'  && By Mohamed Hassan 10/01/95

    lcRpBnkCod = APPAYMNT.cBnkCode
    lcRpChkAct = APPAYMNT.cChkAcct
    DIMENSION laSStubs[1]
    STORE ' ' TO laSStubs

    SELECT DIST cStubChk ;
      FROM APDIST      ;
     WHERE CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+CAPDACTID=;
           'P'+lcRpBnkCod+lcRpChkAct+lcRePrnChk ;
      INTO ARRAY laSStubs

    IF _TALLY > 1    
      DIMENSION laScrMode[4]
      laScrMode[3]=.F.
      laScrMode[4]=.F.
      IF !EMPTY(lcChkToPrn)
        DIMENSION laTStubs[INT(LEN(lcChkToPrn)/8)]
        FOR lnNo = 1 TO ALEN(laTStubs)
          laTStubs[lnNo] = SUBSTR(lcChkToPrn,1+((lnNo-1)*8),8)
        ENDFOR
      ELSE  
        DIMENSION laTStubs[1]
        laTStubs[1]=lcRePrnChk
      ENDIF
      =gfMover(@laSStubs,@laTStubs,"Select Stub Checks",.T.)
      lcChkToPrn = ''
      =ASORT(laTStubs)
      FOR lnCntr = 1 TO ALEN(laTStubs)
        lcChkToPrn = lcChkToPrn + laTStubs[lnCntr]
      ENDFOR
      lcChkToPrn = ALLTRIM(lcChkToPrn)
    ELSE
      lcChkToPrn = ''  
    ENDIF  
    
    lcNxtChkNo = IIF(SEEK(lcRpBnkCod+lcRpChkAct,'APCHECKS'),PADL(APCHECKS.nChkNxtPn,8,'0'),'00000001')
    SHOW GET lcRpBnkCod
    SHOW GET lcRpChkAct
    laOGObjCnt[10] = .T.
    SHOW GET lcNxtChkNo ENABLE
  ENDIF  
ENDIF
*-- End of lfvRePrnChk.

*!**************************************************************************
*!
*!      FUNCTION : lfSlcRec
*!
*!**************************************************************************
*
FUNCTION lfSlcRec

lcInvoice = ' '
*-- if The checks to be reprinted is empty
IF EMPTY(lcChkToPrn)
  lcChkToPrn=lcRePrnChk
ENDIF
  
SELECT APDIST
SET RELATION TO APDIST.CINVNO + APDIST.CVENDCODE INTO APINVHDR ADDITIVE
=SEEK(APPAYMNT.CPAYMETH+APPAYMNT.CBNKCODE+APPAYMNT.CCHKACCT+APPAYMNT.CPAYDOCNO)
SCAN REST WHILE CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF = ;
               APPAYMNT.CPAYMETH+APPAYMNT.CBNKCODE  + ;
               APPAYMNT.CCHKACCT+APPAYMNT.CPAYDOCNO FOR cStubChk $ lcChkToPrn

  IF lcInvoice <> APDIST.CINVNO
    SELECT APINVHDR
    SCATTER MEMVAR MEMO
    SELECT (lcRpTargt)
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE CADDRESS WITH UPPER(cOutComp+cOutAddr1+cOutAddr2+cOutAddr3+cOutAddr4+cOutAddr5);
            nNoOfInv WITH CEILING(RECCOUNT()/lnRpStub);
            cPageNo  WITH PADL(CEILING(RECNO()/lnRpStub),4)

    lcInvoice = APDIST.CINVNO
  ENDIF

  SELECT (lcRpTargt)

  DO CASE
    CASE APDIST.CAPDACTID = 'C'
      REPLACE NINVFAAP     WITH -APDIST.NAPDAMNT 
      
    CASE APDIST.CAPDACTID = 'B'
      REPLACE NINVA1099    WITH -APDIST.NAPDAMNT

    CASE (APDIST.CAPDACTID = 'J' .AND. APDIST.NAPDLINNO <> 1) 

      REPLACE NINVADJAP    WITH -APDIST.NAPDAMNT
    CASE APDIST.CAPDACTID = 'S'
      REPLACE NINVDISAP    WITH -APDIST.NAPDAMNT
  ENDCASE
  SELECT APDIST
ENDSCAN

SELECT APDIST
SET RELATION TO 

SELECT (lcRpTargt)
IF lcRePrnChk $ lcChkToPrn
  REPLACE nNoOfInv  WITH 0
ENDIF  
GO TOP
*-- End of lfSlcRec.

*!************************************************************************
*!
*!      FUNCTION lfRePrnUpd
*!
*!************************************************************************
* 
*FUNCTION lfRePrnUpd
FUNCTION lfRePrnOld
SELECT (lcRpTargt)
GO TOP   && To refresh the relation between files.
SELECT APPAYMNT
lcOldPmnt = APPAYMNT.cPayDocNo

REPLACE APCHECKS.NCHKNXTPN WITH INT(VAL(lcNxtChkNo))
IF lcRePrnChk $ lcChkToPrn
  lcOldPmnt = APPAYMNT.cPayDocNo
  SCATTER MEMVAR MEMO
  REPLACE CPAYSTAT  WITH 'V'
  =gfAdd_Info()
  APPEND BLANK
  GATHER MEMVAR MEMO
  REPLACE CPAYDOCNO WITH PADL(INT(VAL(lcNxtChkNo)) - 1,8,'0')

  =gfAdd_Info()
          
  REPLACE APCHECKS.DCHKLPDAT WITH APPAYMNT.DPAYDATE ;
          APCHECKS.NCHKLPAMT WITH APPAYMNT.NPAYAMNT

  REPLACE APVENDOR.DVENLPAYD WITH APPAYMNT.DPAYDATE ;
          APVENDOR.NVENLPAYA WITH APPAYMNT.NPAYAMNT ;
          APVENDOR.CVENLPAYN WITH PADL(INT(VAL(lcNxtChkNo)) - 1,8,'0')
ENDIF

SELECT APDIST
=SEEK(APPAYMNT.CPAYMETH+APPAYMNT.CBNKCODE+APPAYMNT.CCHKACCT+lcOldPmnt)

lnPrinted = INT(LEN(lcChkToPrn)/8)
lcPrevChk = ''
SCAN REST FOR cApDtrTyp + cBnkCode + cChkAcct + cApdRef = ;
              APPAYMNT.CPAYMETH+APPAYMNT.CBNKCODE +       ;
              APPAYMNT.CCHKACCT+lcOldPmnt
  IF lcPrevChk <> cStubChk .AND. cStubChk $ lcChkToPrn
    lnPrinted = lnPrinted - 1
    lcPrevChk = cStubChk
  ENDIF
  IF cStubChk $ lcChkToPrn
    REPLACE cStubChk WITH PADL(INT(VAL(lcNxtChkNo)) - lnPrinted-1,8,'0')
  ENDIF
ENDSCAN              

IF lcRePrnChk $ lcChkToPrn
  REPLACE cApdRef    WITH PADL(INT(VAL(lcNxtChkNo)) - 1,8,'0'),             ;
          cApSessNo  WITH lcSession                ;
  ALL FOR cApDtrTyp + cBnkCode + cChkAcct + cApdRef = ;
          APPAYMNT.CPAYMETH+APPAYMNT.CBNKCODE+APPAYMNT.CCHKACCT+lcOldPmnt
ENDIF
*-- End of lfRePrnUpd.
           
*!**************************************************************************
*!
*!      Function: lfwibBrows
*!
*!**************************************************************************
*
FUNCTION lfwibBrows
IF !MDOWN()
  RETURN .F.
ENDIF
*-- End of lfwibBrows.

*!**************************************************************************
*!
*!      Function: lfvibBrows
*!
*!***************************************************************************
*
FUNCTION lfvibBrows
PARAMETERS lcBrObject
llBrowse = .T.
_CUROBJ  = OBJNUM(lcBrObject)
KEYBOARD "{ENTER}"
*-- End of lfvibBrows.

*!**************************************************************************
*!
*!      Function: lfFooter
*!
*!***************************************************************************
*
FUNCTION lfFooter
IF &lcRpTargt..cvendcode = lcVendor .AND. lnFooter < lnRpStub .AND. &lcRpTargt..caddress = lcaddress AND &lcRpTargt..cPageNo = lcPageNo
  lcaddress= &lcRpTargt..caddress
  lcPageNo = &lcRpTargt..cPageNo  
  lnFooter = lnFooter + 1
  laFooter[lnFooter,1] = &lcRpTargt..cinvno
  laFooter[lnFooter,2] = DTOC(&lcRpTargt..dinvdate)

  =lfSetCurSm(&lcRpTargt..cCurrCode)
  
  laFooter[lnFooter,3] = SUBSTR(ALLTRIM(TRANSFORM(ninvamnt,'@$ 9999,999,999.99')),1,15)
  laFooter[lnFooter,4] = SUBSTR(ALLTRIM(TRANSFORM(ninvdisap,'@$ 999,999.99')),1,10)
  laFooter[lnFooter,5] = SUBSTR(ALLTRIM(TRANSFORM(ninvadjap,'@$ 9999,999,999.99')),1,15)
  laFooter[lnFooter,6] = IIF(SET('CURRENCY') = 'LEFT' , RIGHT(lcNetSmbl +;
                             ALLTRIM(TRANSFORM(nInvFAAp,'@ 9999,999,999.99'));
                             , 15) , LEFT(ALLTRIM(TRANSFORM(nInvFAAp,;
                             '@ 9999,999,999.99')) + lcNetSmbl , 15) )
  
ELSE
  lnFooter = 1
  laFooter = ''
  lcVendor = &lcRpTargt..cvendcode
  lcaddress = &lcRpTargt..caddress
  lcPageNo = &lcRpTargt..cPageNo
  laFooter[lnFooter,1] = &lcRpTargt..cinvno
  laFooter[lnFooter,2] = DTOC(&lcRpTargt..dinvdate)
  
  =lfSetCurSm(&lcRpTargt..cCurrCode)

  laFooter[lnFooter,3] = SUBSTR(ALLTRIM(TRANSFORM(ninvamnt,'@$ 9999,999,999.99')),1,15)
  laFooter[lnFooter,4] = SUBSTR(ALLTRIM(TRANSFORM(ninvdisap,'@$ 999,999.99')),1,10)
  laFooter[lnFooter,5] = SUBSTR(ALLTRIM(TRANSFORM(ninvadjap,'@$ 9999,999,999.99')),1,15)
  laFooter[lnFooter,6] = IIF(SET('CURRENCY') = 'LEFT' , RIGHT(lcNetSmbl +;
                             ALLTRIM(TRANSFORM(nInvFAAp,'@ 9999,999,999.99'));
                             , 15) , LEFT(ALLTRIM(TRANSFORM(nInvFAAp,;
                             '@ 9999,999,999.99')) + lcNetSmbl , 15) )
  
ENDIF  
RETURN ''
*-- End of lfFooter

*!**************************************************************************
*!
*!      FUNCTION : lfSelectRec
*!
*!**************************************************************************
* Function to Select all the Checks within the range of the reprinted check
* in a temprory file
FUNCTION lfSelectRec
* Select the APDIST file and set the relation between it and the APPAYMNT file
* and change the order to the needed one
SELECT APDIST
SET RELATION TO CINVNO+CVENDCODE INTO APINVHDR ADDITIVE
lcOrder = TAG()
SET ORDER TO TAG CHECKS
* Seek for the First record that fall in the range of the checks
=SEEK('P'+lcRpBnkCod+lcRpChkAct+lcRpChkFrm,'APDIST')
STORE 0 TO lnPgCntr,lnCount
STORE '' TO lcInvoice,lcStubChk

*C200193,1 make sorting by Vendor Code only. [Begin]
*lcGroup = cVendCode + UPPER(APINVHDR.cOutComp+APINVHDR.cOutAddr1+APINVHDR.cOutAddr2+APINVHDR.cOutAddr3+;
*          APINVHDR.cOutAddr4+APINVHDR.cOutAddr5)
lcGroup = cVendCode
*C200193,1 make sorting by Vendor Code only. [End]

lcApdRef = cApdRef

* Scan through the APDIST file for all the checks that fall within the range
* of the checks to be reprinted and is not voided
SCAN REST WHILE CAPDTRTYP+CBNKCODE+CCHKACCT='P'+lcRpBnkCod+lcRpChkAct AND ;
     BETWEEN(cStubChk,lcRpChkFrm,lcRpChkTo) FOR cApdStat <> 'V' 

  * if the current invoice <> the old one
  *C200193,1 Compare by Vendor Code only. [Begin]
  *IF lcInvoice <> APDIST.CINVNO OR lcGroup <> cVendCode + UPPER(APINVHDR.cOutComp+APINVHDR.cOutAddr1+APINVHDR.cOutAddr2+APINVHDR.cOutAddr3+APINVHDR.cOutAddr4+APINVHDR.cOutAddr5)  
  IF lcInvoice <> APDIST.CINVNO OR !(lcGroup == cVendCode)
  *C200193,1 Compare by Vendor Code only. [End]

    *C200193,1 make sorting by Vendor Code only. [Begin]
    *lcGroup = cVendCode + UPPER(APINVHDR.cOutComp+APINVHDR.cOutAddr1+APINVHDR.cOutAddr2+APINVHDR.cOutAddr3+APINVHDR.cOutAddr4+APINVHDR.cOutAddr5)
    lcGroup = cVendCode
    *C200193,1 make sorting by Vendor Code only. [End]

    * if the stub check # has been chaned   
    IF lcStubChk <> cStubChk
    
       * if the master check # <> the last master check #
       * then replace the last record in the temprory file with 0
       * so the printing can feel that this is a master check
       * increament the counter with 1 and change the variable that hold
       * the last master check #
        IF lcApdRef = lcStubChk
         lnCount  = 1
         REPLACE &lcRpTargt..nNoofInv WITH 0
       ELSE  
         lnCount = lnCount + 1
       ENDIF
      lcApdRef = cApdRef
      lnPgCntr = lnPgCntr + 1
      lcStubChk = cStubChk
    ENDIF
    * Select the invoice header file get the values of the current record
    SELECT APINVHDR
    SCATTER MEMVAR MEMO
    * Select the temprory file and append the new record and store the values
    SELECT (lcRpTargt)
    APPEND BLANK
    GATHER MEMVAR MEMO
    * replace the page # that the check will be printed in with the needed page #  
    * and the # of invoices that will be printed in the same check with
    REPLACE cAddress WITH UPPER(APINVHDR.cOutComp+APINVHDR.cOutAddr1+APINVHDR.cOutAddr2+APINVHDR.cOutAddr3+;
            APINVHDR.cOutAddr4+APINVHDR.cOutAddr5),;
            nNoOfInv WITH CEILING(lnCount/lnRpStub) ;
            cPageNo  WITH PADL(lnPgCntr,4)
    lcInvoice = APDIST.cInvNo        
  ENDIF  
  * Select the temprory file and start updating the ammount values 
  SELECT (lcRpTargt)
  
  DO CASE
    CASE APDIST.CAPDACTID = 'C'

      REPLACE NINVFAAP     WITH -APDIST.NAPDAMNT 
      
    CASE APDIST.CAPDACTID = 'B'
      REPLACE NINVA1099    WITH -APDIST.NAPDAMNT

    CASE (APDIST.CAPDACTID = 'J' .AND. APDIST.NAPDLINNO <> 1)

      REPLACE NINVADJAP    WITH -APDIST.NAPDAMNT
    CASE APDIST.CAPDACTID = 'S'
      REPLACE NINVDISAP    WITH -APDIST.NAPDAMNT
  ENDCASE
  SELECT APDIST  
ENDSCAN

*If the last check is a master check 
IF !BOF()
  SKIP -1
ENDIF

* if the last selected record in from APDIST file its master check =
* its stub check that means the last check is a master check so 
* we have to update the temprory file nnoofinv value with 0
* so the printing can feel that this is a master check
IF cApdRef = cStubChk
  REPLACE &lcRpTargt..nNoOfInv  WITH 0
ENDIF  

SELECT APDIST
SET RELATION TO 
IF !EMPTY(lcOrder)
  SET ORDER TO TAG &lcOrder
ENDIF
GO TOP
*-- End of lfSelectRec.

*!************************************************************************
*!
*!      FUNCTION lfRePrnUpd
*!
*!************************************************************************
* function to update the nessecery files after the reprint checks operation
* complete
FUNCTION lfRePrnUpd
PRIVATE lcOrder 
* Select the checks file and update the bank account next check number field
* with the new value
SELECT APCHECKS
=SEEK(lcRpBnkCod+lcRpChkAct)
REPLACE NCHKNXTPN WITH INT(VAL(lcNxtChkNo))

* Select the Temprory file to get the number of checks that printed
* to update the fields of the APDIST file and other files
SELECT (lcRpTargt)
GO BOTT   && To refresh the relation between files.
lnNoOfChk = INT(VAL(cPageNo))

* Select the APDIST file to start updating
SELECT APDIST
lcOrder = TAG()
SET ORDER TO TAG CHECKS
=SEEK('P'+lcRpBnkCod+lcRpChkAct+lcRpChkFrm)

lcCurChk = ''
lnChkNo  = 0 
lcApDChkRec=''
* Scan through the APDIST file to update all the Invoices that was printed
* for a desired check number for specific banck account and not voided
SCAN REST FOR CAPDTRTYP+CBNKCODE+CCHKACCT='P'+lcRpBnkCod+lcRpChkAct AND ;
     BETWEEN(cStubChk,lcRpChkFrm,lcRpChkTo) AND cApdStat <> 'V'
  * Seek for the value of the next record if its in the range of checks
  * to be reprinted NOTE that this condition does not do any thing at
  * the first time entring the scan because the variable lcApDChkRec will
  * be empty
  IF !EMPTY(lcApDChkRec)   
    =SEEK('P'+lcRpBnkCod+lcRpChkAct+lcApDChkRec)   
  ENDIF
  * Skip for the next record and if its in the range of the Checks to be
  * reprinted get the value of the record to be used in next loop
  * we did that because the scan function does not work properly with
  * changing the value of the index that is use by the scan loop itself
  SKIP 1
  IF CAPDTRTYP+CBNKCODE+CCHKACCT='P'+lcRpBnkCod+lcRpChkAct AND ;
     BETWEEN(cStubChk,lcRpChkFrm,lcRpChkTo) AND !EOF()
     lcApDChkRec = cStubChk
  ELSE   
     lcApDChkRec = ''
  ENDIF
  * go to the previous record to update it
  SKIP -1      
  * if the current record check # <> the Previous one
  IF lcCurChk <> cStubChk
    lcCurChk  = cStubChk
    lnChkNo   = lnChkNo + 1  
    * if the stub check = the master check
    IF ALLTR(cStubChk) = ALLTR(cApdRef)
      * seek in the APPAYMNT file to update the record of the payment
      * with the new check #
      SELECT APPAYMNT
      IF SEEK('PP'+APDIST.cStubChk+lcRpBnkCod+lcRpChkAct)
        SCATTER MEMVAR MEMO
        REPLACE CPAYSTAT  WITH 'V'
        =gfAdd_Info()
        APPEND BLANK
        GATHER MEMVAR MEMO
        REPLACE CPAYDOCNO WITH PADL(INT(VAL(lcNxtChkNo)) - lnNoOfChk - 1 + lnChkNo ,8,'0'),;
                DPAYDATE WITH ldChkDat
       =gfAdd_Info()
       * Update the APCHECKS check date and ammount
        SELECT APCHECKS
        REPLACE APCHECKS.DCHKLPDAT WITH APPAYMNT.DPAYDATE ;
                APCHECKS.NCHKLPAMT WITH APPAYMNT.NPAYAMNT
        * Update the Vendor file with the new values        
        SELECT APVENDOR        
        IF SEEK(APDIST.cVendCode,'APVENDOR')
          REPLACE APVENDOR.DVENLPAYD WITH APPAYMNT.DPAYDATE ;
                  APVENDOR.NVENLPAYA WITH APPAYMNT.NPAYAMNT ;
                  APVENDOR.CVENLPAYN WITH PADL(INT(VAL(lcNxtChkNo)) - lnNoOfChk - 1 + lnChkNo ,8,'0')
        ENDIF          
        * Select APDIST file to Replace all the Stub checks that its master check is
        * the one that we are updating its # 
        SELECT APDIST
        lnCurRec= RECNO()
        SET ORDER TO TAG PAYMNTS
        =SEEK('P'+lcRpBnkCod+lcRpChkAct+lcCurChk)
        REPLACE ALL cApdRef WITH PADL(INT(VAL(lcNxtChkNo)) - lnNoOfChk - 1 + lnChkNo ,8,'0') ;
        FOR CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF+CINVNO+CAPDACTID=;
        'P'+lcRpBnkCod+lcRpChkAct+lcCurChk        
        GO lnCurRec
        SET ORDER TO TAG CHECKS
      ENDIF  
    ENDIF
    SELECT APDIST    
  ENDIF

  *-- Update the Stub check # with the new check #
  REPLACE cStubChk WITH PADL(INT(VAL(lcNxtChkNo)) - lnNoOfChk - 1 + lnChkNo ,8,'0')
  *-- Replace the invoice header check no with the new check number that this invoice is printed with
  IF SEEK(CINVNO+CVENDCODE,'APINVHDR')
    REPLACE APINVHDR.CCHKNO   WITH PADL(INT(VAL(lcNxtChkNo)) - lnNoOfChk - 1 + lnChkNo ,8,'0');
            APINVHDR.DCHKDATE WITH ldChkDat
  ENDIF

  *-- if there is any other records within the range of the checks to be reprinted then 
  *-- to to the next one to be updated Note that we seek for the next value to be updated 
  *-- and skip -1 because the Scan will Skip the record pointer after our skip -1
  IF !EMPTY(lcApDChkRec)   
    =SEEK('P'+lcRpBnkCod+lcRpChkAct+lcApDChkRec)   
    SKIP -1
  ENDIF
ENDSCAN

*-- Restore the old Order for the APDIST file
IF !EMPTY(lcOrder)
  SET ORDER TO TAG &lcOrder
ENDIF
*-- End of lfRePrnUpd.

*!************************************************************************
*!
*!      FUNCTION lfvRpBet
*!
*!************************************************************************
* valid function for the push button Reprint checks <Between> to branch
* to the between screen
FUNCTION lfvRpBet
DO LFVARBET WITH "Reprint check number",'C',"@! XXXXXXXX",8,[lfvPrnChkRG() WHEN lfwOldVal()],[LCRPCHKFRM],[LCRPCHKTO]
*-- End of lfvRpBet.

*!************************************************************************
*!
*!      FUNCTION lfvPrnChkRG
*!
*!************************************************************************
* valid function for the Reprint Checks Range
FUNCTION lfvPrnChkRG
PRIVATE lcBrFields,lcApdOrder
*IF old value <> the entered value and !empty of the object
IF lcOldVal <> EVAL(VARREAD()) AND !EMPTY(EVAL(VARREAD()))
  lcCurObj = VARREAD()
  *initialize the value entered by the user padded left with '0'
  &lcCurObj = PADL(ALLT(EVAL(VARREAD())),8,'0')
  SHOW GET &lcCurObj
  SELECT APDIST
  lcApdOrder  = TAG()
  SET ORDER TO TAG CHECKS
  *If the object =  '?' or can not find the value in the file then browse
  IF ('?' $ EVAL(lcCurObj)) OR !SEEK('P'+lcRpBnkCod+lcRpChkAct+EVAL(VARREAD()),'APDIST')
    lcBrFields = [cstubchk : H="Check Nubmer",dApdTrDat:H='Check Date',ChkType = IIF(cStubChk<>ALLT(cApdRef),'Stub','Master'):H='Check Type']+;
                 [,cVendCode:H="Vendor",cInvNo:H="Invoice #"]
    DECLARE laRetStub(1)
    laRetStub[1] = ''
    =gfBrows(['P'+lcRpBnkCod+lcRpChkAct FOR cApdActID = 'A' AND cApdStat <> 'V'],'cStubChk',"laRetStub",'Distribution ',.F.)
    *If the user selected a value from the browse then update the boject
    *else return the old value of the object
    IF EMPTY(laRetStub[1])  
      &lcCurObj = lcOldVal
    ELSE
      &lcCurObj = laRetStub[1]
    ENDIF
  ENDIF
  *if there is value in both the (FROM) and the (TO) objects then check
  *if there is any checks in this range to be print or not
  IF !EMPTY(lcOGValueF) AND !EMPTY(lcOGValueTo)  
    =SEEK('P'+lcRpBnkCod+lcRpChkAct+lcOGValueF,'APDIST')  
    LOCATE REST WHILE CAPDTRTYP+CBNKCODE+CCHKACCT='P'+lcRpBnkCod+lcRpChkAct;
           AND BETWEEN(cStubChk,lcOGValueF,lcOGValueTo) FOR CAPDSTAT <> 'V' 
    IF !FOUND()
      =gfModalGen(.f.,'DIALOG',.F.,.F.,'All checks in the selected range are void.')
      &lcCurObj = lcOldVal    
    ENDIF       
  ENDIF
  *refresh the object
  SHOW GET &lcCurObj
  IF !EMPTY(lcApdOrder)
    SET ORDER TO TAG &lcApdOrder
  ENDIF  
ENDIF  
*-- End of lfvPrnChkRG.

*!************************************************************************
*!
*!      FUNCTION lfInsVdChr
*!      make the lines that hold the VOID sign in the dotmatrix report
*!************************************************************************
*! function to initialize the VOID sign in the dotmatrix report
FUNCTION lfInsVdChr
PARAMETERS lcString,lnStrLen,lcStrAtr,lcInterSect
lcStrAtr = IIF(TYPE('lcStrAtr')='C',lcStrAtr,'')
lcString = ALLTRIM(lcString)
IF EMPTY(LCVOIDCHR)
  RETURN IIF(lcStrAtr='C',PADC(lcString,lnStrLen),IIF(lcStrAtr='R',;
             PADL(lcString,lnStrLen),PADR(lcString,lnStrLen)))
ENDIF  
lcString = IIF(lcStrAtr='C',PADC(lcString,lnStrLen),IIF(lcStrAtr='R',;
             PADL(lcString,lnStrLen),PADR(lcString,lnStrLen)))
lcInterRow=SUBSTR(lcInterSect,1,ATC('|',lcInterSect)-1)
lcInterLen=SUBSTR(lcInterSect,ATC('|',lcInterSect)+1)
lcInterRep = SUBSTR(lcInterLen,1,ATC(',',lcInterLen)-1)
lcInterRep = IIF(EMPTY(lcInterRep),lcInterLen,lcInterRep)
DO WHILE OCCURS(',',lcInterLen) < OCCURS(',',lcInterRow)
 lcInterLen = lcInterLen+','+lcInterRep
ENDDO
lcInterSect = lcInterRow+'|'+lcInterLen
DIMENSION laInterSec[1,2]
STORE '' TO laInterSec
=gfSubStr(lcInterSect,@laInterSec,',|')
FOR lnInterCount = 1 TO ALEN(laInterSec,1)
  lnStarPos1 = VAL(laInterSec[lnInterCount,1])
  lnVChrRpl = IIF(EMPTY(EVAL(laInterSec[lnInterCount,2])),EVAL(laInterSec[1,2]),EVAL(laInterSec[lnInterCount,2]))
  IF TYPE('lnStarPos1')='N' AND TYPE('lnVChrRpl')='N'
    FOR i = 0 TO lnVChrRpl - 1
      IF lnStarPos1+i <= LEN(lcString) AND EMPTY(SUBSTR(lcString,lnStarPos1+i,1)) 
        lcString = STUFF(lcString,lnStarPos1+i,1,LCVOIDCHR)
      ENDIF  
    ENDFOR
  ENDIF  
ENDFOR
RETURN lcString
*-- End of lfInsVdChr.

*!************************************************************************
*!
*!      FUNCTION lfNxtChkUpd
*!
*!************************************************************************
* 
FUNCTION lfNxtChkUpd
IF !EOF(lcRpTargt) AND &lcRpTargt..cPageNo <> lcChkPage
  lcNxtChkNo = PADL(INT(VAL(lcNxtChkNo)) + 1,8,'0')
  lcChkPage = &lcRpTargt..cPageNo 
ENDIF
*-- End of lfNxtChkUpd.

*!*************************************************************
*! Name      : lfGetNetSm
*! Developer : Haytham El_Sheltawi
*! Date      : 01/19/1997
*! Purpose   : Function to get the Currency symbol of the Bank Checking
*!             account currency
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  Currency symbol of the Bank Checking 
*!                       account currency
*!*************************************************************
*! Example            :  =lfGetNetSm()
*!*************************************************************
*
FUNCTION lfGetNetSm

PRIVATE lcOldAlias , lcExactSt , lcCurrency , lnCurSEl , llUsed
 
lcOldAlias = ALIAS()
lcExactSt = SET('EXACT')
SET EXACT ON
SELECT APCHECKS

lcCurrency = LOOKUP(cCurrCode , lcRpBnkCod + lcRpChkAct , cBnkCode , 'BANKCHECK')
lnCurSEl = ASCAN(laCurrSmbl , lcCurrency)

llUsed = .F.
IF !USED('SYCCURR')
  USE (gcSysHome + 'SYCCURR') IN 0
  llUsed = .T.
ENDIF 
lcRpCurDes = ALLTRIM(LOOKUP(SYCCURR.cCurrDesc , lcCurrency , SYCCURR.cCurrCode , 'CCURRCODE'))
lcRpCurDes = IIF(EMPTY(lcRpCurDes) , 'Dollars' , lcRpCurDes)
IF llUsed
  USE IN SYCCURR
ENDIF

SELECT (lcOldAlias)
SET EXACT &lcExactSt

*C200193,1 Get the Currency code instead of symbol. [Begin]
*RETURN IIF(lnCurSEl <> 0 , laCurrSmbl(lnCurSEl + 1) , '')
RETURN IIF(lnCurSEl <> 0 , lcCurrency , '')
*C200193,1 Get the Currency code instead of symbol. [End]
*-- End of lfGetNetSm.

*!*************************************************************
*! Name      : lfSetCurSm
*! Developer : Haytham El_Sheltawi
*! Date      : 01/19/1997
*! Purpose   : Function to reset the Currency symbol
*!*************************************************************
*! Passed Parameters  : Currency code
*!*************************************************************
*! Example            :  =lfSetCurSm()
*!*************************************************************
*
FUNCTION lfSetCurSm

PARAMETER lcCrCod

PRIVATE lcExactSt , lnCurSEl

lcExactSt = SET('EXACT')
SET EXACT ON
lnCurSEl = ASCAN(laCurrSmbl , lcCrCod)

SET CURRENCY TO IIF(lnCurSEl <> 0 , ALLTRIM(laCurrSmbl(lnCurSEl + 1)) , '')

SET EXACT &lcExactSt

RETURN ''
*-- End of lfSetCurSm.

*!*************************************************************
*! Name      : lfDefNxtChk
*! Developer : Ahmed Ibrahim
*! Date      : 03/08/1999
*! Purpose   : Function to Get next check number
*! Reference : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : Option Grid (Default value of variable "lcNxtChkNo")
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  Next check number
*!*************************************************************
*! Example            :  =lfDefNxtChk()
*!*************************************************************
FUNCTION lfDefNxtChk

=gfOpenFile(gcDataDir+'APCHECKS','Bankcheck','SH')
RETURN IIF(SEEK(lcRpBnkCod+lcRpChkAct,'APCHECKS'), ;
           PADL(APCHECKS.NChkNxtPn,8,'0'),'00000001')
*-- End of lfDefNxtChk.           

*!*************************************************************
*! Name      : lfDefGlAcc
*! Developer : Ahmed Ibrahim
*! Date      : 03/08/1999
*! Purpose   : Function to Get Check GL account
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : Option Grid (Default value of variable "lcRpGlAcct")
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  Check GL account
*!*************************************************************
*! Example            :  =lfDefGlAcc()
*!*************************************************************
FUNCTION lfDefGlAcc
=gfOpenFile(gcDataDir+'APCHECKS','Bankcheck','SH')
RETURN IIF(SEEK(lcRpBnkCod+lcRpChkAct,'APCHECKS'),APCHECKS.CCHKGLACC,' ')

FUNCTION lfSetAproe
*-- End of lfDefGlAcc

*:**************************************************************************
*! Name      : lfAdvPyCur
*! Developer : Amin Khodary 
*! Date      : 09/08/1999
*! Purpose   : Function to get the currency code,unit, rate if adv. pay. 
*:**************************************************************************
*! Example   :  lfAdvPyCur
*:**************************************************************************
*
FUNCTION lfAdvPyCur
IF gfGetMemVar('LLMulCurr')
  * Get currency code of selected Bank No +Check Account 
  lcCurrCode =IIF(SEEK(lcRpBnkCod+lcRpChkAct,'APCHECKS'),APCHECKS.cCurrCode,'')
  * If it's base currency no need to recalculate the equv. amount.
  IF lcCurrCode <>  gcBaseCurr 
    lnExRate = gfChkRate('lnCurrUnit',lcCurrCode,ldChkDat,.T.,gcAct_Comp, gcBaseCurr, .T.)
    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
    lnEqvAmnt  = IIF(lnExRate > 0 AND lnCurrUnit > 0, ROUND(lnEqvAmnt  &lcExSin1  lnExRate &lcExSin2 lnCurrUnit,2),0)
  ENDIF
ENDIF
*-- End of lfAdvPyCur.

*:**************************************************************************
*! Name      : lfSayNumber
*! Developer : Sameh Saiid Ezzat 
*! Date      : 06/26/2001
*! Purpose   : Convert the numeric to words. 
*:**************************************************************************
*! Example   : =lfSayNumber()
*:**************************************************************************
*
FUNCTION lfSayNumber
PARAMETERS lnSendNum,lcOtherLine
laWords = SPACE(15)
DIMENSION laDigit(10)

laDigit[1]  = 'ZERO '
laDigit[2]  = 'ONE '
laDigit[3]  = 'TWO '
laDigit[4]  = 'THREE '
laDigit[5]  = 'FOUR '
laDigit[6]  = 'FIVE '
laDigit[7]  = 'SIX '
laDigit[8]  = 'SEVEN '
laDigit[9]  = 'EIGHT '
laDigit[10] = 'NINE '


IF lnSendNum = 0
  RETURN ' ZERO ' 
ENDIF

lcSendNum = STR(lnSendNum,ALEN(laWords,1),2)
 
IF lnSendNum < 1
  RETURN  ' ZERO ' + ' and '+ SUBSTR(lcSendNum,ALEN(laWords,1)-1,2) + '/100 '
ENDIF

lcReturnWord = ''

FOR lnCount = 1 TO ALEN(laWords,1)
  DO CASE

    CASE SUBSTR(lcSendNum,lnCount,1) = '.'
      laWords[lnCount] = '~~~ '

    CASE SUBSTR(lcSendNum,lnCount,1) = ' '
      laWords[lnCount] = 'ZERO '

    OTHERWISE
      lnNumber = VAL(SUBSTR(lcSendNum,lnCount,1))
      laWords[lnCount] = laDigit(lnNumber + 1)

  ENDCASE
ENDFOR
*RETURN lcReturnWord
RETURN ''
*-- End of lfSayNumber
*!*************************************************************
*! Name : lfColData
*! Auth : Mostafa Rawash
*! Date : 09/26/2005
*! Purpose   : Collecting Data.
*!*************************************************************
*! Calls     : APREG10V.PRG
*!*************************************************************
*! Example   :  =lfColData()
*!*************************************************************
FUNCTION lfColData
*C128816,1 MMR 09/26/2005 New custom to print 3 day payment and international payment txt files.
*-- Check if the output file allready exists.
IF FILE(lcOutFile1) 
  IF gfModalGen('QRM00000B00006','ALERT','','','Output file '+lcOutFile1+' already exist. Overwrite it?') = 2
    RETURN
  ENDIF
ENDIF
IF FILE(lcOutFile2) 
  IF gfModalGen('QRM00000B00006','ALERT','','','Output file '+lcOutFile2+' already exist. Overwrite it?') = 2
    RETURN
  ENDIF
ENDIF
*-- Open the output files

lnOutFile1 = FCREATE(lcOutFile1)
IF lnOutFile1 < 0
  =gfModalGen('TRM00000B00000','ALERT','','','Cannot open output file. Cannot proceed.')
  RETURN
ENDIF
lnOutFile2 = FCREATE(lcOutFile2)
IF lnOutFile2 < 0
  =gfModalGen('TRM00000B00000','ALERT','','','Cannot open output file. Cannot proceed.')
  RETURN
ENDIF
*--End of Function lfColData.
*!*************************************************************
*! Name : lfTextRec
*! Auth : Mostafa Rawash
*! Date : 09/26/2005
*! Purpose   : Write Payments records in the output 3 day and international text files.
*!*************************************************************
*! Called from : APREG10V.PRG
*!*************************************************************
*! Modifications : None.
*!*************************************************************
*C128816,1 MMR 09/26/2005 New custom to print 3 day payment and international payment txt files.
FUNCTION lfTextRec
PRIVATE lcSegLine, lcTmpDate, lcInvDate , lnInvAmt ,cVendor,lcDate,lcCentury
cVendor=SPACE(8)
STORE SPACE(0) TO lcDate,lcCentury
STORE 0 TO lnInvAmt,lnRecno
SELECT (lcRpTargt)
INDEX ON cvendcode TAG lcRpTargt of (lcRpTargt)
SCAN
  lnRecno=RECNO()
  IF cvendcode#cVendor
    cVendor=cvendcode
    SEEK cVendor
    IF APVENDOR.lUseBacs 
      lnInvAmt=0
      SCAN REST WHILE cvendcode=cVendor 
          lnInvAmt = lnInvAmt + ((nInvFAAp+ninvdisap)-ninvdisap)
      ENDSCAN
      IF lnRecno<>RECNO()
        GOTO lnRecno
      ENDIF
    ENDIF  
      IF APVENDOR.lUseBacs 
        IF APVENDOR.cBacsType="A"
          lcSegLine = ''
          *-- SOrt Code (Vendor Sort Code)
          lcSegLine = lcSegLine + APVENDOR.cSortCode
          *-- Account No  (New field Vendor account number)
          lcSegLine = lcSegLine + APVENDOR.CACCNT
          *-- Blank Field (blank field position 15)
          lcSegLine = lcSegLine +' '
          *-- (blank field position 16-17)
          lcSegLine = lcSegLine +'  '
          *-- Bank Sort Code 
          lcSegLine = lcSegLine +Apbanks.cSortCode
          *-- Bank Check account 
          lcSegLine = lcSegLine +LEFT(Apchecks.cChkacct,8)
          *-- (blank field position 32-35) 
          lcSegLine = lcSegLine +'    '
          *-- Paid Amount
          lcSegLine = lcSegLine +PADR(ALLTRIM(STR(lnInvAmt,11,2)),11,' ')
          *-- Company name 
          lcSegLine = lcSegLine +LEFT(gcCompName,18)
          *-- (blank field position 65-82)
          lcSegLine = lcSegLine +'                  '
          *-- Vendor name
          lcSegLine = lcSegLine +LEFT(Apvendor.cvencomp,18)
          = FPUTS(lnOutFile1,lcSegLine)
        ENDIF  
        IF APVENDOR.cBacsType="B"
          lcSegLine = ''
          *-- Swift tag+Max 16 character
          *C128816,2 MMR 11/06/2005 Populate the :20: field in the international payment txt files with 
          *C128816,2 MMR            the user difined field APVENDOR.CSENDREF.[Start]
          *lcSegLine = ':20:'+'                '
          lcSegLine = ':20:'+ APVENDOR.CSENDRREF
          *C128816,2 MMR.[End]
          = FPUTS(lnOutFile2,lcSegLine)
          *-- Swift tag+date+currency code+amount of the payment
          lcCentury=SET("Century")
          SET Century OFF
          lcDate=DTOC(DATE())
          lcDate=STRTRAN(lcDate,'/','')
          lcSegLine=''
          lcSegLine = ':32A:'+lcDate+lcNetSmbl+PADR(ALLTRIM(STR(lnInvAmt,10,2)),10,' ')
          = FPUTS(lnOutFile2,lcSegLine)
          SET Century &lcCentury
          *-- Bank Check account
          lcSegLine='' 
          lcSegLine = ':50A:/'+Apchecks.cChkacct
          = FPUTS(lnOutFile2,lcSegLine)
          *-- Swift Code(New field in Apvendor)
          IF EMPTY(Apvendor.cSwift)
            lcSegLine='' 
            lcSegLine = ':57A:/'
            = FPUTS(lnOutFile2,lcSegLine)
          ELSE
            lcSegLine='' 
            lcSegLine = ':57A:/'+Apvendor.cSwift
            = FPUTS(lnOutFile2,lcSegLine)
          ENDIF  
          *-- Routing Code(New field in Apvendor)
          IF EMPTY(Apvendor.cSwift)
            lcSegLine='' 
            lcSegLine = ':57A://'+Apvendor.cRotngCode
            = FPUTS(lnOutFile2,lcSegLine)
          ENDIF  
          *-- Account No  (New field Vendor account number)
          lcSegLine='' 
          lcSegLine = ':59:/'+ APVENDOR.CACCNT
          = FPUTS(lnOutFile2,lcSegLine)
          lcSegLine=''
        ENDIF  
      ENDIF  
  ENDIF    
ENDSCAN
*--End of Function lfTextRec.
*!*************************************************************
*! Name : lfGetFile
*! Auth : Mostafa Rawash
*! Date : 09/26/2005
*! Ref  : C128816,1
*!*************************************************************
*! Synopsis : Give ability to user to choose name and path of;
*!            the output file.
*!*************************************************************
*! Called from : None.
*!*************************************************************
*! Modifications : None.
*!*************************************************************
*C128816,1 MMR 09/26/2005 New custom to print 3 day payment and international payment txt files.
FUNCTION lfGetFile
PARAMETER lcFile
PRIVATE ltOutFile , lcTalk
IF lcFile='3Day'  
  lcOutFile1 = ""                      && 3 Day payment file.
  ltOutFile = PUTFILE('Select the output file' , lcOutFile1 , "TXT" )
ELSE
  lcOutFile2 = ""                      && International payment file.
  ltOutFile = PUTFILE('Select the output file' , lcOutFile2 , "TXT" )
ENDIF
IF lcFile='3Day'     
  IF !EMPTY(ltOutFile)
    lcOutFile1 = ltOutFile 
    lcTalk = SET('TALK')
    SET TALK OFF
    SAVE TO gcDataDir + "MEMO" ALL LIKE lcOutFile1
    SET TALK &lcTalk.
  ENDIF  
ELSE
  IF !EMPTY(ltOutFile)
    lcOutFile2 = ltOutFile 
    lcTalk = SET('TALK')
    SET TALK OFF
    SAVE TO gcDataDir + "MEMO" ALL LIKE lcOutFile2
    SET TALK &lcTalk.
  ENDIF  
ENDIF  
IF lcFile='3Day'     
  IF !EMPTY(lcOutFile1) OR !EMPTY(lcOutFile2)
    IF lcOutFile1==lcOutFile2
      =gfModalGen('TRM00000B00000','ALERT','','','You have to select two different files. Cannot proceed.')
      lcOutFile1 = ""                      
      Return
    ENDIF
  ENDIF  
ELSE
  IF !EMPTY(lcOutFile1) OR !EMPTY(lcOutFile2)
    IF lcOutFile1==lcOutFile2
      =gfModalGen('TRM00000B00000','ALERT','','','You have to select two different files. Cannot proceed.')
      lcOutFile2 = ""                      
      Return
    ENDIF
  ENDIF  
ENDIF  
  
*-- End of lfGetFile
*!*************************************************************
*! Name        : lfvBacs
*! Developer   : Mostafa Rawash (MMR)
*! Date        : 09/26/2005
*! Ref         : C128816,1
*! Purpose     : Valid function for Disable Prievew button
*!*************************************************************
*! Called from : APREG10V.PRG
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvBacs()
*!*************************************************************
*C128816,1 MMR 09/26/2005 New custom to print 3 day payment and international payment txt files.
FUNCTION lfvBacs
IF llBacs
  llProgram = .T.
  Clear Read
ELSE
  llProgram = .F.
  Clear Read
ENDIF
*-- End of lfvBacs
