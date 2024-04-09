*:***************************************************************************
*  This program is a typical copy of APCHPPRV.PRG for check print 
*  Changes : the standard file APCHKPWL.FRX is replaced with APCHKPAN.FRX
*            to meet changes in position printing for ANU10
*  Developer : TMI - TAREK MOHMMED IBRAHIM
*  Date      : 05/09/2006
*  Entry     : 131676,1
*:***************************************************************************

*:************************************************************************
*:
*: Procedure file: APCHKPRV.RPR
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: Accounts Payable
*:         Author: Malak Hanna Aziz
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: lfLockFile
*:               : lfClearLok
*:               : lfUpInv
*:               : lfIncNxtChk
*:               : lfIniTotal
*:               : lfSumGrp
*:               : lfAllgTest
*:               : lfwGrid
*:               : lfvGrid
*:               : lfvBnkCode
*:               : lfvChkAcct
*:               : lfValidAcct
*:               : lfvNxtChk
*:               : lfvChkDate
*:               : lfvOkAdvPay
*:               : lfvDivision
*:               : lfShowOg
*:               : lfSortAddr
*:               : lfvVenCode
*:               : lfReValidate
*:               : lfvRemit
*:               : lfvFactor
*:               : lfCreateCur
*:               : lfSetRela
*:               : lfLokBank
*:               : lfUnLokBank
*:               : lfAdvPay
*:               : lfClearRep
*:               : lfBeforeRev
*:               : lfAdvance
*:               : lfvRepName
*:               : lfvPrintMode
*:               : lfvRePrnChk
*:               : lfSelectRec
*:               : lfRePrnUpd
*:               : lfwibBrows
*:               : lfvibBrows
*:               : lfFooter
*:               :
*:      Documented 00/00/1994
*:************************************************************************
*
lcSavCurSm = SET('CURRENCY',1)    &&Varible to save the seting of the currency symbol

*MAN Added check to see if the syccurr is open
PRIVATE llCurOpn
llCurOpn = USED("SYCCURR")
SELECT       cCurrCode , cCurrSmbl ;
  FROM       (gcSysHome + 'SYCCURR') ;
  INTO ARRAY laCurrSmbl     
IF !llCurOpn  
  USE IN SYCCURR
ENDIF  

lcNetSmbl = ALLTRIM(lfGetNetSm())      &&Varible to save the currency symbol of the checking account currency

*** Check if user wants report to printer or file 
llPrintChk = IIF(UPPER(gcDevice)='PRINTER' OR UPPER(gcDevice)='FILE',.T.,.F.)
lcFisYear  = ''      && fiscal year
lcFisPrd   = ''      && fiscal period
lcLineTwo  = ''      && holds the total check amount (uesd in FRX)
lcInvRemit = ''      && invoice remit to
lcFactor   = ''      && factor code
lcTFactor  = ''      && 'factor code'
ldRPChkDat = ldChkDat
lcRpExp = lcRpExp + IIF(lcRpVenPay = 'A','',' AND APINVHDR.CVENPMETH = lcRpVenPay')

lcRpExp = lcRpExp + ' AND APINVHDR.DINVDATE <= ldChkDat '

lcRpExp = lcRpExp + ' AND APINVHDR.DPOSTDATE <= ldChkDat'

*- Increase number of lines in ap check to 20 lines instate of 11 lines [Begin]
=gfDoTriger('APCHKPRV',PADR('CHNGLINE',10))
DIMENSION laFooter[lnRpStub,6]  && Creat a new array to hold the footer records.


laFooter  = ''
lnFooter  = 0 
lcVendor  = '' && Variable to hold the vendor code.
lcPageNo  = ''

lcAddress  = '' && Variable to hold the invoice address.

*- Although the standard Laser form stopped using the BMP
*- We didn't comment out the code that handels the BMP for compatibality
*- with any custom form. Added Error handling to maintain the case where
*- there is no installed application that handles BMP
PRIVATE lcCurErHnd
lcCurErHnd = ON("ERROR")
ON ERROR STORE .T. TO llDumErHnd

lcVoid = gfTempName()
*- Create the cursor.
CREATE CURSOR (lcVoid) (gPic G)
APPEND BLANK
APPEND GENERAL gPIC FROM ( gcBmpHome+'VOIDCHK.BMP')

ON ERROR &lcCurErHnd

IF llTestChk            && In case of allignment test.
  IF lfLokBank()        && If could lock the bank record
    SELECT (lcRpTargt)
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

lnCurrUnit  = 1            && Holds currency unit of printed checks
lnExRate    = 1   		   && Holds currency rate of printed checks
lcCurrCode  = gcBaseCurr   && Holds currency code of printed checks
lnEqvAmnt   = lnPaymnt    && Holds eqv. amount

*** lcPrintMod variable that hold..
*** 'A' --> in case of 'advance check payment'.
*** 'V' --> in case of 'print approved checks'.
*** 'R' --> in case of 'reprinting check'.

*-- Amin, 06/16/2005 Variable holds .T. is the printing process was ok.
lPrntCompl = .F.
*-- Amin, 06/16/2005 Variable holds .T. is the printing process was ok.


DO CASE
  CASE lcPrintMod = 'A'  && Print advance checks
    IF lfvOkAdvPay() .AND. lfLokBank()
	    *-- Amin, 06/16/2005 Variable holds .T. is the printing process was ok.
		lPrntCompl = .T.
		*-- Amin, 06/16/2005 Variable holds .T. is the printing process was ok.

      =lfCreateCur()  && Create temp file   
      =lfSetRela()    && Set relation between the files
      =lfAdvPyCur()   && Get currency code, rate and unit of advanced payment
      =lfAdvPay()     && Fill the data into files
      SELECT (lcRpTargt)

    *- call the gfDispRe with .T. the last parameter
    *- to force printing to file first before the print
    *- to ignore any errors can be happed while printing
    DO gfDispRe WITH EVAL('lcRepForm'),.f.,.f.,'R',.T.    

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

    SELECT  &lcRpFields, 000000 AS 'nNoOfInv', '    ' AS 'cPageNo';
    FROM  &lcRpFiles ;
    WHERE &lcPrExp   ;
    ORDER BY CVENDCODE,CADDRESS,CINVNO;
    INTO DBF (gcWorkDir+lcRpTargt)

    *** Restore all enviroment 
    WAIT CLEAR
    SET TALK OFF
    ON ESCAPE  &lcSaveOnEs
    SET ESCAPE &lcSaveEsc  

    *** Display the report to screen , file or printer
    *** and check if there is any record or not
    IF _TALLY = 0      && No records collected
      *** NO recoeds hove been collected
      =gfModalGen("INM00052B00000","DIALOG")
    ELSE
      =lfSetRela()        && Set relations between files
      IF lfLokBank()      && If could lock the bank record
        IF lfLockFile()   && If could lock the selected invoice records
                          && ,mark Begin of each group and calculate no
                          && of checks to be print for each group. 
          SELECT (lcRpTargt)
         lcPadvance = _PADVANCE
         _PADVANCE  = IIF(lcRpName ='1','LINEFEED','FORMFEED')
         _PLENGTH   = 42
         
          IF llPrintChk AND (lcRpName='2' AND _WINDOWS) &&lcRePForm = 'APCHKPAN'
            GO TOP
            lcTmpChkNo=lcNxtChkNo
            lcChkPage = cPageNo
            SCAN
              =lfNxtChkUpd()            
              =lfUpInv()
            ENDSCAN
            lcNxtChkNo = lcTmpChkNo
          ENDIF  
		    *- Variable holds .T. is the printing process was ok.
			lPrntCompl = .T.
          
         DO gfDispRe WITH EVAL('lcRepForm'),.f.,.f.,'R',.T.    

         _PADVANCE = lcPadvance
        ENDIF
        =lfUnLokBank()    && unlock the bank record
      ENDIF
    ENDIF
  
  CASE lcPrintMod = 'R'  && Reprint Checks
    IF lfLokBank()       && If could lock the bank record
	    *-- Amin, 06/16/2005 Variable holds .T. is the printing process was ok.
		lPrntCompl = .T.
		*-- Amin, 06/16/2005 Variable holds .T. is the printing process was ok.
    
      =lfCreateCur()     && Create temp file   
      =lfSetRela()       && Set relation between the files
      =lfSelectRec()     && select all detail lines for selected check number. 
      SELECT (lcRpTargt)

      *- In case of DOTMATIX printing, advance after seach check by line feed not form feed
      lcPadvance = _PADVANCE
         _PADVANCE  = IIF(lcRpName ='1','LINEFEED','FORMFEED')         

      *- call the gfDispRe with .T. the last parameter
      *- to force printing to file first before the print
      *- to ignore any errors can be happed while printing
      DO gfDispRe WITH EVAL('lcRepForm'),.f.,.f.,'R',.T.    

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

*-- Dispaly Print is Completed message if lPrntCompl was True
IF llPrintChk AND lPrntCompl 
  *** Printing is completed.
  =gfModalGen("INM04124B00000","DIALOG")  
ENDIF
*-- Amin, 06/16/2005 Dispaly Print is Completed message if lPrntCompl was True


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
lcGroup = CVendCode + CAddress 
REPLACE nNoOfInv WITH 0
DO WHILE !BOF()
  IF lcGroup = CVendCode + CAddress   && check if same group

    lnTotInvApv = lnTotInvApv + nInvFAAp
    
    *** lnRpStub nubmer of lines per page 12 for DOS, 14 for WINDOWS
    REPLACE nNoOfInv WITH CEILING(lnCount/lnRpStub)
  ELSE 
    *** Check if the approval payment is less than 1 

    IF lnTotInvApv < 0 

       IF llPrintChk
         *** unlock all locked records.
         =lfClearLok()
       ENDIF  

      *** Message :"Approved payment for vendor ð can not be lees than zero"
      *** Choice  :"                         < OK >                        "
      =gfModalGen("INM04162B00000","DIALOG",ALLTRIM(SUBSTR(lcGroup,1,8)))

      RETURN .F.
    ENDIF  
    
    REPLACE nNoOfInv WITH 0     && Indicate to End Of Group.
    lnCount     = 0

    lnTotInvApv = nInvFAAp
    
    lcGroup     = CVendCode + CAddress
  ENDIF
  lnCount     = lnCount     + 1
  SELECT APINVHDR
  *** If device = 'File' Or 'Printer' and can lock cureent record.

  IF llPrintChk AND !lfObj_Lock(.T.)

    SELECT (lcRpTargt)
    
    lcInvNo = APINVHDR.CINVNO
    lcVendCode = APINVHDR.CVendCode
    SCAN REST 
	  SELECT APINVHDR
	  IF APINVHDR.CINVNO+APINVHDR.CVendCode <> lcInvNo +lcVendCode 
	    =gfObj_Lock(.F.) 
	  ENDIF 
	ENDSCAN
    
    RETURN .F.
  ENDIF

  SELECT (lcRpTargt)
  SKIP -1

  IF BOF() AND lnTotInvApv < 0 

    IF llPrintChk
      *** unlock all locked records.
      =lfClearLok()
    ENDIF  

    =gfModalGen("INM04162B00000","DIALOG",ALLTRIM(SUBSTR(lcGroup,1,8)))

    RETURN .F.
  ENDIF  
ENDDO

SELECT (lcRpTargt)
lnPgCntr  = 1
lnInvCntr = 0
GO TOP
lcGroup = CVendCode + CAddress
SCAN 
  IF lcGroup  = CVendCode + CAddress .AND. lnInvCntr < lnRpStub
    lnInvCntr = lnInvCntr+1
  ELSE
    lnPgCntr  = lnPgCntr+1
    lnInvCntr = 1
  ENDIF
  
  REPLACE cPageNo WITH PADL(lnPgCntr,4)
  lcGroup = CVendCode + CAddress
ENDSCAN

GO TOP

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

*!************************************************************************
*!
*!      FUNCTION lfUpInv
*!
*!************************************************************************
* Update the informations inside certine files while printing
FUNCTION lfUpInv

IF llEndGrp
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

  *- M.H Add the currency to the AP module.
  *- M.H Check the Checking account currency rate with gcBaseCurr with the proceed date.
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

    *- M.H Get the equation signs. (Begin)
    *- to exchange currency from invoice currency to company base currency.
    lcExSin2 = ' '
    lcExSin1 = gfGetExSin(@lcExSin2,&lcRpTargt..cCurrCode)

    lcExSin4 = ' '

    lcExSin6 = ' '
    lcExSin5 = gfGetExSin(@lcExSin6,&lcRpTargt..cAprCurCode)

    *- Add the currency to the AP module.
    *- Calculate the amount to pay,Adjustment,Discount,1099 with the currency rate / unit.
    
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
*    REPLACE CVENDCODE   WITH &lcRpTargt..CVENDCODE;
             CINVNO      WITH &lcRpTargt..CINVNO;      
             CAPDTRTYP   WITH 'P';
             DAPDTRDAT   WITH ldChkDat;
             LAPDPOST    WITH .F.;
             CAPDSTAT    WITH '';
             CAPDREF     WITH lcChkNo;
             CSTUBCHK    WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0') ;
             CAPDGLACT   WITH &lcRpTargt..CCHKGLACC;
             NAPDAMNT    WITH - &lcRpTargt..NINVAMTAP;
             CAPDACTID   WITH 'C';
             CBATCHNO    WITH '';
             CTRNSLEDN   WITH '';
             CFISFYEAR   WITH lcFisYear;
             CFSPPRDID   WITH lcFisPrd;
             CAPSESSNO   WITH lcSession;
             CTAXCODE    WITH '';
             CBNKCODE    WITH lcRpBnkCod;
             CCHKACCT    WITH lcRpChkact


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

    *- Used the variables hold signs in the equation. (Begin)
    lnExchDiff = lnAprPayB - ROUND(&lcRpTargt..NINVAMTAP &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT,2)
    *- Add the currency to the AP module.
    * Add the Eq. lnExchDiff
    * Add the base Currency,Rate = 1,Unit= 1
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

*!************************************************************************
*!
*!      FUNCTION lfIncNxtChk
*!
*!************************************************************************
* Incriment the check number
FUNCTION lfIncNxtChk

lcNxtChkNo = PADL(INT(VAL(lcNxtChkNo)) + 1,8,'0')

RETURN ''

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
*- Summition the total for the reprinted check for all the stub check
*- that belongs to the same master check and printed before the check
*- that we are printing
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
  MODIFY WINDOW GWDGRID TITLE lcTitle +PADL('Session : ' + lcSession,WCOLS())
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

*****************************

laOpenFile [2,1] = 'APVENHST'
IF !USED('APVENHST')
  USE &gcDataDir.APVENHST IN 0
  laOpenFile [2,2] = .T.
ENDIF

SELECT APVENHST
laOpenFile [2,3] = SYS(22)
SET ORDER TO VENDYEAR

*****************************

laOpenFile [3,1] = 'APBANKS'
IF !USED('APBANKS')
  USE &gcDataDir.APBANKS IN 0
  laOpenFile [3,2] = .T.
ENDIF

SELECT APBANKS
laOpenFile [3,3] = SYS(22)
SET ORDER TO BANKCODE
=SEEK(lcRpBnkCod)

*****************************

laOpenFile [4,1] = 'APCHECKS'
IF !USED('APCHECKS')
  USE &gcDataDir.APCHECKS IN 0
  laOpenFile [4,2] = .T.
ENDIF

SELECT APCHECKS
laOpenFile [4,3] = SYS(22)
SET ORDER TO BANKCHECK
=SEEK(lcRpBnkCod+lcRpChkAct)

*****************************

laOpenFile [5,1] = 'APINVHDR'
IF !USED('APINVHDR')
  USE &gcDataDir.APINVHDR IN 0 
  laOpenFile [5,2] = .T.
ENDIF

SELECT APINVHDR
laOpenFile [5,3] = SYS(22)
SET ORDER TO INVVEND

*****************************

laOpenFile [6,1] = 'APPAYMNT'
IF !USED('APPAYMNT')
  USE &gcDataDir.APPAYMNT IN 0
  laOpenFile [6,2] = .T.
ENDIF

SELECT APPAYMNT
laOpenFile [6,3] = SYS(22)
SET ORDER TO TYPMETHDOC

*****************************

laOpenFile [7,1] = 'APDIST'
IF !USED('APDIST')
  USE &gcDataDir.APDIST IN 0
  laOpenFile [7,2] = .T.
ENDIF

SELECT APDIST
laOpenFile [7,3] = SYS(22)
SET ORDER TO PAYMNTS

*****************************

laOpenFile [8,1] = 'SYCFACT'
IF !USED('SYCFACT')
  USE &gcSysHome.SYCFACT IN 0 
  laOpenFile [8,2] = .T.
ENDIF

SELECT SYCFACT
laOpenFile [8,3] = SYS(22)
SET ORDER TO CFACCODE

*****************************

laOpenFile [9,1] = 'SYCINT'
IF !USED('SYCINT')
  USE &gcSysHome.SYCINT IN 0
  laOpenFile [9,2] = .T.
ENDIF

SELECT SYCINT
laOpenFile [9,3] = SYS(22)
SET ORDER TO CCONTCODE

*****************************

laOpenFile [10,1] = 'APDIV'
IF !USED('APDIV')
  USE &gcDataDir.APDIV IN 0
  laOpenFile [10,2] = .T.
ENDIF

SELECT APDIV
laOpenFile [10,3] = SYS(22)
SET ORDER TO DIVISION



*- Change this lines for the changes we have made to SYCCODES [Begin]
IF !USED('CODES')
  USE (gcDataDir + 'CODES') IN 0
ENDIF

lcPrintMod = lcRpChkMod
lcOldMode = lcRpChkMod
STORE '' TO lcRpChkFrm,lcRpChkTo
lcRepForm = IIF(lcRpName='1','APCHKPDD',IIF(_DOS,'APCHKPDL','APCHKPAN'))
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
*SET COMP &lcSetCom

*!************************************************************************
*!
*!      FUNCTION lfvGrid
*!
*!************************************************************************
*
FUNCTION lfvGrid    
*MAN
*FOR lnCounter = 1 TO 9
FOR lnCounter = 1 TO ALEN(laOpenFile,1)
  IF laOpenFile[lnCounter,2] 
    *MAN
    IF USED(ALIAS(laOpenFile[lnCounter,1]))
      USE IN ALIAS(laOpenFile[lnCounter,1])
    ENDIF  
  ELSE
    SELECT (laOpenFile[lnCounter,1])
    SET ORDER TO laOpenFile[lnCounter,3]
  ENDIF
ENDFOR
        
*!************************************************************************
*!
*!      Function lfvBnkCode
*!
*!************************************************************************
*
FUNCTION lfvBnkCode   
IF EMPTY(lcRpBnkCod)
  *- Hesham El-Sheltawi (Start)
  *- if the bank code changed then initialize the reprint
  *- check range variables
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
*** Search for the current bank code
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
    
    *- if the bank code changed then initialize the reprint
    *- check range variables
    IF lcPrintMod = 'R' 
      STORE '' TO lcRpChkFrm,lcRpChkTo
    ENDIF
    
  ELSE
    *** The bank has no checking accounts setup.
    =gfModalGen("INM04023B00000","DIALOG")
    lcRpBnkCod = lcOldVal
  ENDIF
  SHOW GET lcRpChkAct
  SHOW GET lcNxtChkNo
  SHOW GET lcRpGlAcct
ENDIF
        
*!************************************************************************
*!
*!      FUNCTION lfvChkAcct
*!
*!************************************************************************
*  
FUNCTION lfvChkAcct            

IF EMPTY(lcRpBnkCod)
  *** You have to enter the ð.
  =gfModalGen("TRM04066B00000","DIALOG",'bank code')
  lcRpChkAct = ''
  
  *- if the bank code changed then initialize the reprint
  *- check range variables
  IF lcPrintMod = 'R' 
    STORE '' TO lcRpChkFrm,lcRpChkTo
  ENDIF
  
  RETURN
ENDIF

SELECT APCHECKS

*** Search for the current checking account code
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
    
    *- if the bank code changed then initialize the reprint
    *- check range variables
    IF lcPrintMod = 'R' 
      STORE '' TO lcRpChkFrm,lcRpChkTo
    ENDIF
    
    lcNxtChkNo = ''
    lcRpGlAcct = ''
  ELSE
    lcNxtChkNo = PADL(APCHECKS.NChkNxtPn,8,'0')
    lcRpGlAcct = APCHECKS.CCHKGLACC
    
    *- if the bank code changed then initialize the reprint
    *- check range variables
    IF lcPrintMod = 'R' 
      STORE '' TO lcRpChkFrm,lcRpChkTo
    ENDIF
    
  ENDIF
  SHOW GET lcRpChkAct
  SHOW GET lcNxtChkNo
  SHOW GET lcRpGlAcct
ENDIF  

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
  *** You have to enter the ð.
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
  *** ð account not valid.
  =gfModalGen("INM04077B00000","DIALOG",'Checking')
  lcRpChkAct = ''
  lcNxtChkNo = ''
  lcRpGlAcct = ''
  SHOW GET lcRpChkAct
  SHOW GET lcNxtChkNo
  SHOW GET lcRpGlAcct
ENDIF
SHOW GET lcNxtChkNo

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

*!**************************************************************************
*!
*!      Function lfvOkAdvPay
*!
*!**************************************************************************
* 
FUNCTION lfvOkAdvPay      

IF EMPTY(lcDebMemN)
  *** You have to enter the ð.
  =gfModalGen("TRM04066B00000","DIALOG",'debit memo number')
  _CUROBJ   = OBJNUM(lcDebMemN)
  RETURN .F.
ELSE
  SET ORDER TO TAG VENDINV IN APINVHDR
  IF SEEK(lcRpVenCod+lcDebMemN,'APINVHDR')
    *** ð exists for the vendor ð.
    =gfModalGen("TRM04024B00000","DIALOG",'Invoice '+ALLTRIM(lcDebMemN)+ ;
                '|'+ALLTRIM(lcRpVenCod))
    _CUROBJ   = OBJNUM(lcDebMemN)
    RETURN .F.
  ENDIF  
  SET ORDER TO TAG INVVEND IN APINVHDR
ENDIF  

IF lnPaymnt <= 0
  *** The amount to be applied should be greater than zero.
  =gfModalGen("TRM04029B00000","DIALOG")
  _CUROBJ   = OBJNUM(lnPaymnt)
  RETURN .F.
ELSE
  IF !BETWEEN(ln1099Amnt,0,lnPaymnt)
    *** The 1099 amount must be between ð and ð.
    =gfModalGen("TRM04017B00000","DIALOG","0|"+STR(lnPaymnt))
    _CUROBJ   = OBJNUM(ln1099Amnt)
    RETURN .F.
  ENDIF
ENDIF

IF EMPTY(STRTRAN(STRTRAN(lcApAcct,'-'),'0')) 
  *** 'ð cannot be empty.' 
  =gfModalGen("INM04074B00000","DIALOG",'AP account')
  _CUROBJ   = OBJNUM(lcApAcct)
  RETURN .F.
ENDIF  

IF ! lfValidAcct(lcApAcct)  
  *** ð not found.
  =gfModalGen("INM04002B00000","DIALOG",'A/P account|'+ALLTRIM(lcApAcct))
  _CUROBJ   = OBJNUM(lcApAcct)
  RETURN .F.
ENDIF  

*** If remitting to factor, and there is no factor code, 
*** do not proceed and present the following message
IF lcInvRemit = 'F' .AND. EMPTY(lcFactor)
  *** Message : "   You have to enter the ð.  "
  ***                 <  OK  >
  =gfModalGen("TRM04066B00000","DIALOG",lcTFactor)
  _CUROBJ = OBJNUM(lcFactor)
  RETURN .F.
ENDIF
llOkAdvPay = .T.
CLEAR READ

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

    *- Change this lines for the changes we have made 
    *          to SYCCODES [Begin]
    lcDivision = CODES.cdiscrep
    lcAdvDiv   = CODES.cCode_No
    
    
    SHOW GET lcDivision
ENDCASE

IF _WINDOWS
  DEACTIVATE POPUP puDivision
ENDIF  

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

*!************************************************************************
*!
*!      Function lfSortAddr
*!
*!************************************************************************
* Return the address sorted by the user
FUNCTION lfSortAddr   

DIMENSION laAddrOrd(3,2)
** Assign seq. no to the array by the order of the address fields in the file.
laAddrOrd[1,1]  = '3'
laAddrOrd[2,1]  = '4'
laAddrOrd[3,1]  = '5'    
lcReturnVal     = ' '

IF SEEK(APBANKS.cCont_Code,'SYCINT')
  ** Assign the order of the address to the second column of the array.
  laAddrOrd[1,2]  = SYCINT.NPART3ORD
  laAddrOrd[2,2]  = SYCINT.NPART4ORD
  laAddrOrd[3,2]  = SYCINT.NPART5ORD
  =ASORT(laAddrOrd,2)  && Sort the Temp array.
  FOR lnCounter = 3 TO 5
    lcPostion   = laAddrOrd[lnCounter,1]
    lcReturnVal = ALLTRIM(lcReturnVal)+' '+ALLTRIM(APBANKS.cAddress&lcPostion)
  ENDFOR  
ELSE
  ** We are going to take the address as it is.
  lcReturnVal = ALLTRIM(APBANKS.CADDRESS3) + ' ' + ;
                ALLTRIM(APBANKS.CADDRESS4) + ' ' + ;
                ALLTRIM(APBANKS.CADDRESS5)
ENDIF
RETURN lcReturnVal

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

*** Search for the current vendor code
IF ('?' $ &lcRpCurFld. .OR. !SEEK(&lcRpCurFld.)) 
  DECLARE laRpRetFld(1)
  lcBrFields    = 'CVendCode:H="Vendor Code",CVENCOMP:H="Vendor name"'
  laRpRetFld[1] = ''
  =gfBrows([],'CVENDCODE',"laRpRetFld",'Vendor codes ',.F.)

  IF EMPTY(laRpRetFld[1])  
    &lcRpCurFld = lcOldVal
  ELSE
    IF APVENDOR.CVENPRIOR = '0'
      *** Vendor ð has payment priority zero.  This vendor is on hold.
      =gfModalGen("INM04060B00000","DIALOG",ALLTRIM(APVENDOR.CVENDCODE))
      RETURN 
    ELSE  
      &lcRpCurFld = laRpRetFld[1]
    ENDIF
  ENDIF
ELSE
  IF APVENDOR.CVENPRIOR = '0'
    *** Vendor ð has payment priority zero.  This vendor is on hold.
    =gfModalGen("INM04060B00000","DIALOG",ALLTRIM(APVENDOR.CVENDCODE))
    &lcRpCurFld = lcOldVal
    _CUROBJ     = _CUROBJ
    RETURN 
  ENDIF  
ENDIF

SHOW GET (lcRpCurFld)
lcRpVenCod = &lcRpCurFld 

*!************************************************************************
*!
*!      FUNCTION lfReValidate
*!
*!************************************************************************
* Before printing this function revalid the needed informations
FUNCTION lfReValidate  

DO CASE
*- change the checking on the from to variables
  CASE lcPrintMod = 'R' .AND. (EMPTY(lcRpChkFrm) OR EMPTY(lcRpChkTo))

    *** ð cannot be empty.
    =gfModalGen("INM04074B00000","DIALOG",'Reprint check number')
    RETURN .F.

  CASE EMPTY(lcRpBnkCod)            
    *** ð cannot be empty   
    =gfModalGen("INM04074B00000","DIALOG",'Bank code')
    RETURN .F.
      
  CASE !SEEK(lcRpBnkCod,'APBANKS')   
    *** ð not found.
    =gfModalGen("INM04002B00000","DIALOG",'Bank code')
    RETURN .F.

  CASE EMPTY(lcRpChkAct)                   
    *** ð cannot be empty.
    =gfModalGen("INM04074B00000","DIALOG",'Checking account')
    RETURN .F.

  CASE !SEEK(ALLTRIM(lcRpBnkCod+lcRpChkAct),'APCHECKS')   
    *** ð account not valid.
    =gfModalGen("INM04077B00000","DIALOG",'Checking')
    RETURN .F.

  CASE INT(VAL(lcNxtChkNo)) < APCHECKS.NChkNxtPn    
    *** Next printed check number cannot be less than ð.
    =gfModalGen("INM04056B00000","DIALOG",ALLTRIM(STR(APCHECKS.NChkNxtPn)))
    RETURN .F.

  CASE lcPrintMod <> 'R' .AND. !lfVlDate(gcPrnt_Cmp,@lcFisPrd,@lcFisYear,ldChkDat)
    *** Cehck date is not valid.
    =gfModalGen("INM04052B00000","DIALOG")
    RETURN .F.

  CASE lcPrintMod <> 'R' .AND. EMPTY(STRTRAN(STRTRAN(lcRpGlAcct,'-'),'0')) 
    *** ð cannot be empty..
    =gfModalGen("INM04074B00000","DIALOG",'G/L account')
    RETURN .F.

  CASE lcPrintMod <> 'R' .AND. ! lfValidAcct(lcRpGlAcct)  
    *** ð not found.
    =gfModalGen("INM04002B00000","DIALOG","G/L account|"+ALLTRIM(lcRpGlAcct))
    RETURN .F.

ENDCASE

IF lcPrintMod = 'A'   
  IF EMPTY(lcRpVenCod)
    *** ð cannot be empty.
    =gfModalGen("INM04074B00000","DIALOG",'Vendor code')
    RETURN .F.
  ELSE
    IF SEEK(ALLTRIM(lcRpVenCod),'APVENDOR')
      IF APVENDOR.CVENPRIOR = '0'
        *** Vendor ð has payment priority zero.  This vendor is on hold.
        =gfModalGen("INM04060B00000","DIALOG",ALLTRIM(lcRpVenCod))
        RETURN .F.
      ENDIF
    ELSE
      *** ð not found.
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
        *** Vendor ð has payment priority zero.  This vendor is on hold.
        =gfModalGen("INM04060B00000","DIALOG",ALLTRIM(laVendor[lnCounter]))
        RETURN .F.
      ENDIF
    ENDFOR  
  ENDIF  
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvRemit
*!
*!**************************************************************************
*
FUNCTION lfvRemit
*** Default factor for the vendor
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

*!**************************************************************************
*!
*!      Function: lfvFactor
*!
*!**************************************************************************
* Valid function for get field lcFactor
*
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

*** Create the tempery file to send to the report
CREATE CURSOR &lcRpTargt FROM ARRAY laFieldStr

RETURN lnFieldNo + 3

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
      *** ð has been deleted.
      =gfModalGen("QRM04095B00000","DIALOG",'Checking account')
      llRetVal = .F.
      =gfObj_Lock(.F.)
    ENDIF  
  ELSE
    *** Bank ð is being edited by user ð.
    =gfModalGen("INM04085B00000","DIALOG",lcRpBnkCod+'|'+CLOK_USER)
    llRetVal = .F.
  ENDIF  
ELSE    
  *** ð has been deleted.
  =gfModalGen("QRM04095B00000","DIALOG",'Bank code')
  llRetVal = .F.
ENDIF

RETURN llRetVal

*!************************************************************************
*!
*!      FUNCTION lfUnLokBank
*!
*!************************************************************************
* UnLock the bank record  
FUNCTION lfUnLokBank

  SELECT APBANKS
  =SEEK(lcRpBnkCod)
  =gfObj_Lock(.F.)

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

*** Prepare Remit to array from SYDFIELD and get its maximum width
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

*!************************************************************************
*!
*!      FUNCTION lfvRepName
*!
*!************************************************************************
* 
FUNCTION lfvRepName

lcRepForm = IIF(lcRpName ='1','APCHKPDD',IIF(_DOS,'APCHKPDL','APCHKPAN'))
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

lnRpStub  = 11
=lfRepPltFr(lcRepForm)

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
  =lfChangeGrid('APCHKPR'+lcPrintMod)
ENDIF  

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
 
IF !EMPTY(lcRePrnChk)  
  lcRePrnChk = PADL(ALLTRIM(lcRePrnChk),8,'0')
  SHOW GET lcRePrnChk
ELSE   
  RETURN    
ENDIF    
SELECT APPAYMNT

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

IF EMPTY(lcRePrnChk) .OR. lcRePrnChk = '00000000'  
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

  IF !lcRePrnChk == lcOldVal .OR. lcRePrnChk <> '00000000'  

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

*!**************************************************************************
*!
*!      FUNCTION : lfSelectRec
*!
*!**************************************************************************
FUNCTION lfSlcRec

lcInvoice = ' '
IF EMPTY(lcChkToPrn)
  lcChkToPrn=lcRePrnChk
ENDIF
  
SELECT APDIST
SET RELATION TO APDIST.CINVNO + APDIST.CVENDCODE INTO APINVHDR ADDITIVE
=SEEK(APPAYMNT.CPAYMETH+APPAYMNT.CBNKCODE+APPAYMNT.CCHKACCT+APPAYMNT.CPAYDOCNO)
*(MAN
*SCAN REST WHILE CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF = ;
               APPAYMNT.CPAYMETH+APPAYMNT.CBNKCODE  + ;
               APPAYMNT.CCHKACCT+APPAYMNT.CPAYDOCNO
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
*- Get the lcOldPmnt value before the IF condition
lcOldPmnt = APPAYMNT.cPayDocNo

REPLACE APCHECKS.NCHKNXTPN WITH INT(VAL(lcNxtChkNo))

IF lcRePrnChk $ lcChkToPrn
  *- Get the lcOldPmnt value before the IF condition
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

*!**************************************************************************
*!
*!      Function: lfFooter
*!
*!***************************************************************************
*
*- Function to fill the array with data.
FUNCTION lfFooter

IF &lcRpTargt..cvendcode = lcVendor .AND. lnFooter < lnRpStub .AND. &lcRpTargt..caddress = lcaddress AND &lcRpTargt..cPageNo = lcPageNo

  *- At the bottom stub make sure that the system does not print the invoices from previous chks.
  lcaddress= &lcRpTargt..caddress
  lcPageNo = &lcRpTargt..cPageNo  
  lnFooter = lnFooter + 1
  laFooter[lnFooter,1] = &lcRpTargt..cinvno
  laFooter[lnFooter,2] = DTOC(&lcRpTargt..dinvdate)

  
  =lfSetCurSm(&lcRpTargt..cCurrCode)
  
  laFooter[lnFooter,3] = SUBSTR(ALLTRIM(TRANSFORM(ninvamnt,;
                                '@$ 9999,999,999.99')),1,15)
  laFooter[lnFooter,4] = SUBSTR(ALLTRIM(TRANSFORM(ninvdisap,;
                                '@$ 999,999.99')),1,10)
  laFooter[lnFooter,5] = SUBSTR(ALLTRIM(TRANSFORM(ninvadjap,;
                                '@$ 9999,999,999.99')),1,15)
  laFooter[lnFooter,6] = IIF(SET('CURRENCY') = 'LEFT' , RIGHT(lcNetSmbl +;
                             ALLTRIM(TRANSFORM(nInvFAAp,'@ 9999,999,999.99'));
                             , 15) , LEFT(ALLTRIM(TRANSFORM(nInvFAAp,;
                             '@ 9999,999,999.99')) + lcNetSmbl , 15) )
  
ELSE
  lnFooter = 1
  laFooter = ''
  lcVendor = &lcRpTargt..cvendcode
  *- At the bottom stub make sure that the system does not print the invoices from previous chks.
  lcaddress = &lcRpTargt..caddress
  
  lcPageNo = &lcRpTargt..cPageNo
  
  laFooter[lnFooter,1] = &lcRpTargt..cinvno
  laFooter[lnFooter,2] = DTOC(&lcRpTargt..dinvdate)

  =lfSetCurSm(&lcRpTargt..cCurrCode)

  laFooter[lnFooter,3] = SUBSTR(ALLTRIM(TRANSFORM(ninvamnt,;
                                '@$ 9999,999,999.99')),1,15)
  laFooter[lnFooter,4] = SUBSTR(ALLTRIM(TRANSFORM(ninvdisap,;
                                '@$ 999,999.99')),1,10)
  laFooter[lnFooter,5] = SUBSTR(ALLTRIM(TRANSFORM(ninvadjap,;
                                '@$ 9999,999,999.99')),1,15)
  laFooter[lnFooter,6] = IIF(SET('CURRENCY') = 'LEFT' , RIGHT(lcNetSmbl +;
                             ALLTRIM(TRANSFORM(nInvFAAp,'@ 9999,999,999.99'));
                             , 15) , LEFT(ALLTRIM(TRANSFORM(nInvFAAp,;
                             '@ 9999,999,999.99')) + lcNetSmbl , 15) )
  
ENDIF  

*- Replace the "Adjustment" field is printed in the check stubs. [Begin]
=gfDoTriger('APCHKPRV',PADR('REFRANCE',10))


RETURN ''

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
lcGroup = cVendCode + UPPER(APINVHDR.cOutComp+APINVHDR.cOutAddr1+APINVHDR.cOutAddr2+APINVHDR.cOutAddr3+;
          APINVHDR.cOutAddr4+APINVHDR.cOutAddr5)
lcApdRef = cApdRef

* Scan through the APDIST file for all the checks that fall within the range
* of the checks to be reprinted and is not voided
SCAN REST WHILE CAPDTRTYP+CBNKCODE+CCHKACCT='P'+lcRpBnkCod+lcRpChkAct AND ;
     BETWEEN(cStubChk,lcRpChkFrm,lcRpChkTo) FOR cApdStat <> 'V' 

  * if the current invoice <> the old one
  IF lcInvoice <> APDIST.CINVNO OR lcGroup <> cVendCode + UPPER(APINVHDR.cOutComp+APINVHDR.cOutAddr1+APINVHDR.cOutAddr2+APINVHDR.cOutAddr3+APINVHDR.cOutAddr4+APINVHDR.cOutAddr5)  
    lcGroup = cVendCode + UPPER(APINVHDR.cOutComp+APINVHDR.cOutAddr1+APINVHDR.cOutAddr2+APINVHDR.cOutAddr3+APINVHDR.cOutAddr4+APINVHDR.cOutAddr5)
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
  * Update the Stub check # with the new check #
  REPLACE cStubChk WITH PADL(INT(VAL(lcNxtChkNo)) - lnNoOfChk - 1 + lnChkNo ,8,'0')
  IF SEEK(CINVNO+CVENDCODE,'APINVHDR')
    REPLACE APINVHDR.CCHKNO   WITH PADL(INT(VAL(lcNxtChkNo)) - lnNoOfChk - 1 + lnChkNo ,8,'0');
            APINVHDR.DCHKDATE WITH ldChkDat
  ENDIF
  IF !EMPTY(lcApDChkRec)   
    =SEEK('P'+lcRpBnkCod+lcRpChkAct+lcApDChkRec)   
    SKIP -1
  ENDIF
ENDSCAN
* Restore the old Order for the APDIST file
IF !EMPTY(lcOrder)
  SET ORDER TO TAG &lcOrder
ENDIF


*!************************************************************************
*!
*!      FUNCTION lfvRpBet
*!
*!************************************************************************
* valid function for the push button Reprint checks <Between> to branch
* to the between screen
FUNCTION lfvRpBet
DO LFVARBET WITH "Reprint check number",'C',"@! XXXXXXXX",8,[lfvPrnChkRG() WHEN lfwOldVal()],[LCRPCHKFRM],[LCRPCHKTO]

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

*!************************************************************************
*!
*!      FUNCTION lfInsVdChr
*!
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

*:***************************************************************************
* 
*  FUNCTION lfNxtChkUpd
*
*:***************************************************************************
FUNCTION lfNxtChkUpd
IF !EOF(lcRpTargt) AND &lcRpTargt..cPageNo <> lcChkPage
  lcNxtChkNo = PADL(INT(VAL(lcNxtChkNo)) + 1,8,'0')
  lcChkPage = &lcRpTargt..cPageNo 
ENDIF

*!*************************************************************
*! Name      : lfGetNetSm                              
*! Developer : Haytham El_Sheltawi
*! Date      : 01/19/1997
*! Purpose   : Function to get the Currency symbol of the Bank Checking
*!             account currency
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : APCHKPRV.RPR
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  Currency symbol of the Bank Checking 
*!                       account currency
*!*************************************************************
*! Example            :  =lfGetNetSm()
*!*************************************************************
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

RETURN IIF(lnCurSEl <> 0 , laCurrSmbl(lnCurSEl + 1) , '')

*!*************************************************************
*! Name      : lfSetCurSm                              
*! Developer : Haytham El_Sheltawi
*! Date      : 01/19/1997
*! Purpose   : Function to reset the Currency symbol
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : lfFooter() , APCHKPRDD.FRX , APCHKPRDL.FRX , APCHKPRWL.FRX
*!*************************************************************
*! Passed Parameters  : Currency code
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfSetCurSm()
*!*************************************************************
FUNCTION lfSetCurSm

PARAMETER lcCrCod

PRIVATE lcExactSt , lnCurSEl

lcExactSt = SET('EXACT')
SET EXACT ON
lnCurSEl = ASCAN(laCurrSmbl , lcCrCod)

SET CURRENCY TO IIF(lnCurSEl <> 0 , ALLTRIM(laCurrSmbl(lnCurSEl + 1)) , '')

SET EXACT &lcExactSt

RETURN ''

*!*************************************************************
*! Name      : lfDefNxtChk
*! Developer : Ahmed Ibrahim
*! Date      : 03/08/1999
*! Purpose   : Function to Get next check number
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

*!*************************************************************
*! Name      : lfAdvPyCur
*! Developer : Amin Khodary 
*! Date      : 09/08/1999
*! Purpose   : Function to get the currency code,unit, rate if adv. pay. 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : APCHKPRV.PRG in case of avanced payment.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  lfAdvPyCur
*!*************************************************************
*!B802573,1 AKA 09/05/99 

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

