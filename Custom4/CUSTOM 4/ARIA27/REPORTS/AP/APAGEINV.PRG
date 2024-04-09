*B600490,1 Mlk          Adjusting the Messages in this report.
*B500790,1 MAN 06/21/95 Changing the numeric fields picture in the FRX
*B500790,1              to print the commas   
*E300296,1 M.H 01/02/96 Add the currency to the AP reports.
*B601015,1 Hesham El-Sheltawi 04/01/96
*B601015,1 Adjust the report to age the payables in a way to match
*B601015,1 the trial balance in a specific period
*B601013,1 RENEE 04/02/96. Correct the spelling of 'Curruncy' to 'Currency'
*B601013,1                 in APAGECD.FRX, APAGECS.FRX, APAGEDD.FRX, APAGEDS.FRX
*B601236,1 M.H 08/28/96 Considre the debit memo in case of the option include payment is not checked.
*B601589,1 HS  01/23/97 Add just the approved adjustment to lnTotPaymnt
*B601416,1 HS    01/26/97 Change the calling of gfGetExSin so if the Invoice 
*B601416,1                currency is the base currency I make it the To  
*B601416,1                currency and if not the payment currency is 
*B601416,1                the To currency 
*B601856,1 HS  09/16/97 Changed the function lfSumALL to reset the variable
*B601856,1              lnTotPaymnt with 0 if the Invoice is not paid
*B601913,1 HS  11/30/97 Made some changes in the report APAGEDS.FRX :
*B601913,1              1) {Before (ldRpC4F) , APINVHDR.cVendCode [Group]}
*B601913,1                 Field [I made it a Calculate (Sum) field and
*B601913,1                 Reset (APINVHDR.cVendCode [Group])].
*B601913,1              2) {Before (ldRpC4F) , [Summary]} Field [I made the
*B601913,1                 Reset (End of Report)].
*E301301,1 AKA 08/04/99 Print aged payable based on base currency 
*E301352,1 ABD 12/06/99 Add field APINVHDR.dPostDate to the advanced options of the report.
*E301352,1 ABD          Note: The modification done In Files Syrepuvr.DBF & Sydreprt.DBF 
*B802942,1 SSE 02/22/2000 Add the A/P Account which is found in ADV1
*B603522,1 SSE 03/19/2000 Convert all objects in FRX to TRANSPARENT
*B803339,1 AMH 06/13/2000 To supres the vendor balance if he has zero balance.
*B603724,1 AKA 07/02/2000 fix the problem in option include payment
*B603679,1 KHM 07/04/2000 Changing the way of printing the report when 
*B603679,1                choosing 'N' in the include payments option by 
*B603679,1                printing from a temporary file instead of using 
*B603679,1                the master file in order to be able to do not 
*B603679,1                print the invoices with 0 amount.
*B603759,1 AKA 19/07/2000 Fix error Numeric overflow in case there is a Non Manual Check paymanet.
*B603905,1 AAN 19/09/2000 Fix error change the posting date in option grid from "is" to "Less or equal"
*B803711,1 SSE 01/10/2000 Fix bug of displaying void invoices (all modifications is in Report header Sydreprt).
*B604013,1 SSE 11/14/2000 Fix bug of displaying void invoices when Aging date is less than Void date.
*B604068,1 AAN 13/10/2000 Fix bug of not allowing to print all currencies in case the user selectes FORIGN CURRENCY option
*B604217,1 MHM 02/22/2001 Fix bug of The Aged Pyable report does not give same total in both versions Graphics & Text.
*B604217,1 MHM            The modificactions have been done in FRX (APAGEDD , APAGEDS ) 
*B605931,1 ALB 07/02/2002 Return Original amount instat of Equevlant 
*B606436,1 ALB 09/04/2002 Return Original amount instat of Equevlant if the Payment Diff. than Invoice Currency
*B606839,1 KHM 04/06/2003 Fix the bug of including the voided payments that are greater than
*B606839,1                the aging date when include payment option is No.
*B607137,1 ALB 05/05/2003 Fix bug if there is no exchange rate in given date
*B607184,1 ALB 05/06/2003 Fix bug in the Advanced Payment 
*B607288,1 ALB 05/20/2003 Fix bug if the payment diff than invoice currency and with diff Exchange Rate
*B607318,1 ALB 06/24/2003 Fix bug that FRX not print the last 2 char. from right (Apagedd.frx & Apagecd.frx)
*B119374,1 ALB 08/27/2003 Fix Relation Exp. between APDIST and APPAYMNT files
*B607464,1 ALB 09/11/2003 Add Thermometer when collecting the data from the invoice file
*B122247,1 NNA 04/21/2004 Fix bug of that the report give an incorrect result for invoices approved in different currency
*B123314,1 MHM 07/12/2004 Fix bug of that the report give an incorrect result for invoices payed in different currency 
*B124337,1 MHM 09/21/2004 Fix bug of that the report give an incorrect result for invoices created in forign and payed in different currency 
*B126840,1 MHM 03/16/2005 Fix bug of Variable llInvDate not found (no need for this variable)
*B608290,1 TMI 09/25/2007 Enhance the report by replacing the calling to the global function with a variable 
*B608634,1 WAM 07/28/2008 Fix paid amount calculation for invoices paid in foreign currency [T20080708.0003]
*****************************************************************************************************************

*B604013,1 Add condition to check for date of voiding [Begin]
*-- lcRpExp checks each void invoice by seeking in ApDist (which is opened in descending order
*-- with another alias) for its transaction date whether it's bigger than aging date.
lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),""," AND ") + [IIF(cInvStat='V',;
                    IIF(SEEK(cInvNo + cVendCode,'APDISTDS') AND ;
                    DTOS(ApDistDs.dApdTrDat) > DTOS(ldRpCurDat),.T.,.F.),.T.)]
*B604013,1 Add condition to check for date of voiding [End]

*B608290,1 TMI [Start] as there is only one value for the setup variable LLMULCURR we can call the global function gfGetMemvar 
*                      only once and use the return value in variable llUseMulCr instead
llUseMulCr = gfGetMemVar('LLMULCURR')
*B608290,1 TMI [End  ] 

SELECT APINVHDR
*E300296,1 M.H 12/18/95 Use the new index in the apinvhdr.
SET ORDER TO CURVENINV
*E300296,1 M.H End.

SET ORDER TO TAG VENDINV
SET ORDER TO TAG VENCODE IN APVENDOR
SET RELATION TO cvendcode INTO APVENDOR ADDITIVE

*B604068,1 AAN 12/10/2000 Determine whether the Grand total should be printed or not [Start].
llShowGT = IIF(lcRpCurr="F",IIF(EMPTY(laogfxflt[1,6]),.F.,.T.),.T.)
*B604068,1 AAN 12/10/2000 Determine whether the Grand total should be printed or not [End].
lcExSin1 = ' '
lcExSin2 = ' '
IF !llRpConPay
  SET ORDER TO TAG INVVEND IN APDIST
  SET RELATION TO CINVNO+CVENDCODE INTO APDIST ADDITIVE
  SELECT APDIST
  SET ORDER TO TAG TYPMETHDOC IN APPAYMNT

  *B119374,1 ALB Fix Relation Exp. between APDIST and APPAYMNT files [BEGIN]
  *B603759,1 AKA 19/07/2000 [Begin]
  * The old relation line was not retrieved the APPAYMNT records if the payment is
  * Non Manual Check.
  *SET RELATION TO "P"+APDIST.CAPDTRTYP+PADL(ALLTRIM(APDIST.CAPDREF),8,"0")+;
                   APDIST.CBNKCODE+APDIST.CCHKACCT INTO APPAYMNT ADDITIVE

  *SET RELATION TO "P"+APDIST.CAPDTRTYP+PADR(ALLTRIM(APDIST.CAPDREF),8," ")+;
                   APDIST.CBNKCODE+APDIST.CCHKACCT INTO APPAYMNT ADDITIVE                   
  SET RELATION TO "P"+APDIST.CAPDTRTYP+PADR(RTRIM(APDIST.CAPDREF),8," ")+;
                   APDIST.CBNKCODE+APDIST.CCHKACCT INTO APPAYMNT ADDITIVE                   

  *B603759,1 AKA 19/07/2000 [End]                   
  *B119374,1 ALB Fix Relation Exp. between APDIST and APPAYMNT files [END]
  lnTotPaymnt = 0  
  llCanPrint  = .F.
  
  *B603679,1 KHM 07/04/2000 (Begin) Using a temporary file instead of 
  *B603679,1                printing from the master file.
  TmpApInvHd = gfTempName()
  *-- This function is used to get all the AP invoices according to the
  *-- selected criteria and insert it in a temporary file to be used
  *-- in the print process.
  =lfColData()
  RETURN
  *B603679,1 KHM 07/04/2000 (End)
ENDIF
SELECT APINVHDR

*E301301,1 AKA (Start)
*If user wants to display all invoice regardless of the currency code, change 
* the index to be Vendor Code  + Currency + invoice No.

=lfCreatNdx()
*E301301,1 AKA (End)

DO gfDispRe WITH EVAL('lcRpForm'),'FOR '+lcRpExp+ ' AND' + IIF(EMPTY(lcRpRefnce),".T.",' LIKE("'+STRTRAN(lcRpRefnce,' ','?')+'",APINVHDR.cinvref)')
SET RELATION TO  
SELECT APDIST
SET RELATION TO

*!**************************************************************************
*!
*!      Function: lfRepShow
*!
*!**************************************************************************
*
FUNCTION lfRepShow

*E300296,1 M.H 01/02/96 Add the currency to the AP reports.

laOGFxFlt[1,6]= gcBaseCurr
*MAN
*laOGObjCnt[3] = gfGetMemVar('LLMULCURR')
laOGObjCnt[11] = gfGetMemVar('LLMULCURR')
=lfOGShowGet("lnRepCurr")
*E300296,1 End

*!**************************************************************************
*!
*!      Function: lfvCurDat
*!
*!**************************************************************************
* 
FUNCTION lfvCurDat

lcRpVar=SYS(18)
IF EMPTY(ldRpCurDat)
  *** You have to enter the ð.
  =gfModalGen("TRM04066B00000","DIALOG",'report date')
  &lcRpVar=lcOldVal
  SHOW GET (lcRpVar)
  _CUROBJ=_CUROBJ
ELSE
 *B601015,1 Hesham (Start)
 *B601015,1 if the Aging date is less than the system date
 *B601015,1 default include payments to .F.
 IF VARREAD()="LDRPCURDAT" AND ldRpCurDat < gdSysDate
   llRpConPay = .F.
   =lfOgShowGet("llRpConPay")   
   =lfvConPaymnt() 
 ENDIF
 *B601015,1 Hesham (End)
ldRpC1F=ldRpCurDat - lnRpPrd1
ldRpC1T=ldRpCurDat 
ldRpC2F=ldRpCurDat - lnRpPrd2
ldRpC2T=ldRpC1F - 1
ldRpC3F=ldRpCurDat - lnRpPrd3
ldRpC3T=ldRpC2F - 1
ldRpC4F=ldRpCurDat - lnRpPrd4
ldRpC4T=ldRpC3F - 1
*B601015,1 Hesham (Start) 
*B601015,1 Remove calling of the valid function of the include
*B601015,1 payments because the hidden filter had been changed
* =lfvConPaymnt() 
*B601015,1 Hesham (End)
ENDIF 

*!**************************************************************************
*!
*!      Function: lfSumALL
*!
*!**************************************************************************
*123314,1 MHM 09/21/2004 we comment this function to write it in another way to handle 
*123314,1 MHM 09/21/2004 FORGIN cases
*FUNCTION lfSumALL
FUNCTION OldlfSumALL
*-MHM 09/21/2004 

PARAMETERS lcVend,lcInv
*B601236,1 M.H Begin.
IF apinvhdr.ninvpaid + apinvhdr.ninvdistk + apinvhdr.ninvadj  = 0
  llCanPrint  = .T.
  
  *B601856,1 Add this line to reset the variable lnTotPaymnt with 0 [Begin]
  lnTotPaymnt = 0
  *B601856,1 Add this line to reset the variable lnTotPaymnt with 0 [End]
  RETURN 0
ENDIF
*B601236,1 M.H End.

*B601015,1 Hesham (Start)
*ldComDate={}
lnTotPaymnt = 0
llCanPrint  = .F.
*DO CASE
*  CASE BETWEEN(APINVHDR.DINVDUDAT,ldRpC1F,ldRpC1T)
*    ldComDate=ldRpC1T
*  CASE BETWEEN(APINVHDR.DINVDUDAT,ldRpC2F,ldRpC2T)
*   ldComDate=ldRpC2T
*  CASE BETWEEN(APINVHDR.DINVDUDAT,ldRpC3F,ldRpC3T)
*   ldComDate=ldRpC3T
*  CASE BETWEEN(APINVHDR.DINVDUDAT,ldRpC4F,ldRpC4T)
*   ldComDate=ldRpC4T
*ENDCASE 
lnRPAlias=SELECT()
SELECT APDIST
*SUM REST nAPdAmnt WHILE CINVNO+CVENDCODE = lcInv+lcVend;
      FOR capdtrtyp $ "MNHP" AND capdactID = "A";
      AND dAPDtrdat > ldComDate TO lnTotPaymnt

*E300296,1 M.H 05/30/96 Add the currency to the AP reports.
IF gfGetMemVar('LLMULCURR')
  SEEK lcInv+lcVend
  SCAN REST WHILE CINVNO+CVENDCODE = lcInv+lcVend;
        FOR ((capdtrtyp $ "MNHP" AND capdactID $ "CSJ") .OR. ( capdtrtyp = "A" AND nApdAmnt < 0));
        AND dAPDtrdat > ldRpCurDat;
        AND cApdStat  <> 'V'
     DO CASE
       CASE APDIST.CAPDACTID = 'C'
         lcExSin2 = ' '
         *B606436,1 ALB Return Original amount instat of Equevlant if the Payment Diff. 
         *B606436,1 than Invoice Currency [Begin]
         IF APINVHDR.CCURRCODE = APPAYMNT.CCURRCODE
           lnTotPaymnt = lnTotPaymnt-ROUND(APDIST.NAPDAMNT,2)
         ELSE
           *B124337,1 MHM 09/21/2004 for invoices created in forign and  payed in different currency [Start]
           IF (APINVHDR.CCURRCODE <> GcBaseCurr) AND (APPAYMNT.CCURRCODE <> GcBaseCurr)
             lcExSin1   = gfGetExSin(@lcExSin2,GcBaseCurr,APPAYMNT.CCURRCODE)
             lnExRate   = APDIST.NEXRATE           
             lnTotPaymnt = lnTotPaymnt-ROUND(APDIST.NAPDAMNT &lcRateSign nExRate,2)
           ELSE      
           *B124337,1 [End] 
           
           lcExSin1   = gfGetExSin(@lcExSin2,APINVHDR.CCURRCODE,APPAYMNT.CCURRCODE)
           *--under test MHM
           *B123314,1 MHM 12/07/2004 Comment this line to get Correct Signin Case Forign(Invoice) to Base(Paying) [Start]
           IF APINVHDR.CCURRCODE = GcBaseCurr
             lcExSin1   = IIF(lcExSin1 = '*' , '/' , '*')
             lcExSin2   = IIF(lcExSin2 = '*' , '/' , '*')
           ENDIF
           *lcExSin1   = IIF(lcExSin1 = '*' , '/' , '*')
           *lcExSin2   = IIF(lcExSin2 = '*' , '/' , '*')
           *B123314,1 MHM 12/07/2004 []

           lnUnit     = APINVHDR.NCURRUNIT
           lcInvCurr  = APINVHDR.CCURRCODE
           lcPayCurr  = APPAYMNT.CCURRCODE
           ldExRateDt = APPAYMNT.DPAYDATE

           *B122247,1 NNA 04/21/2004 (Begin) Get the Ex_rate from the APDIST File insteed of the apinvhdr file
           *B122247,1 NNA             because the Apdist File Have the Ex_rate From any Foreign Currency to the base
           *B122247,1 NNA             currency without looking to the invoice Currency                    
           *lnExRate   = APINVHDR.NEXRATE
           *B123314,1 MHM 07/12/2004  Get exchange rate accourding to payment[Start]
           *lnExRate   = APDIST.NEXRATE           
           IF APINVHDR.CCURRCODE = APPAYMNT.CCURRCODE
             lnExRate   = APDIST.NEXRATE           
           ELSE
             lcExSin1   = IIF(lcExSin1 = '*' , '/' , '*')
             lcExSin2   = IIF(lcExSin2 = '*' , '/' , '*')
             lnExRate   = APINVHDR.NEXRATE
           ENDIF   
           *B123314,1 MHM [ENd]
           
           *B122247,1 NNA (End)

           *B607137,1 ALB Fix bug if there is no exchange rate in given date [Begin]
           lnExRate = IIF(lnExRate = 0 ,lfGetLstRat(lcInvCurr,lcPayCurr,ldExRateDt),lnExRate)
           *B607137,1 ALB Fix bug if there is no exchange rate in given date [End]
           
           lnTotPaymnt = lnTotPaymnt-ROUND(APDIST.NAPDAMNT &lcExSin1 lnExRate &lcExSin2 lnUnit,2)
           *B124337,1 MHM 09/21/2004 Fix bug of that the report give an incorrect result for invoicescreated in forignand  payed in different currency 
           ENDIF
           *B124337,1 [End]
           
         ENDIF
         *B601416,1 Change the calling of gfGetExSin so if the [BEGIN]
         * Invoice currency is the base currency I make it the To currency 
         * and if not the payment currency is the To currency 
         *lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.CCURRCODE,APPAYMNT.CCURRCODE)
         *B601416,1 IF Statment to check if the Invoice currency is the 
         * same as the base currency
*--Alb   IF APINVHDR.CCURRCODE = gcBaseCurr;
           lcExSin1 = gfGetExSin(@lcExSin2 , APPAYMNT.CCURRCODE , APINVHDR.CCURRCODE);
         ELSE;
           lcExSin1 = gfGetExSin(@lcExSin2,APINVHDR.CCURRCODE,APPAYMNT.CCURRCODE);
           lcExSin1 = IIF(lcExSin1 = '*' , '/' , '*');
           lcExSin2 = IIF(lcExSin2 = '*' , '/' , '*');
         ENDIF;
         *B601416,1 Change the calling of gfGetExSin [End]
         

         *B601416,1 Change this line [Begin]
         *lnTotPaymnt = lnTotPaymnt-ROUND(APDIST.NAPDAMNT &lcExSin2 APPAYMNT.NEXRATE &lcExSin1 APPAYMNT.NCURRUNIT,2)

		 *B605931,1 ALBERT Change this line since that these FRX call the global function[Begin]
         *lnTotPaymnt = lnTotPaymnt-ROUND(APDIST.NAPDAMNT &lcExSin1 APPAYMNT.NEXRATE &lcExSin2 APPAYMNT.NCURRUNIT,2)
*--alb    lnTotPaymnt = lnTotPaymnt-ROUND(APDIST.NAPDAMNT,2)
         *B605931,1 ALBERT Change this line [End]
         
         *B601416,1 Change this line [End]
         *B606436,1 ALB Return Original amount instat of Equevlant if the Payment Diff. 
         *B606436,1 than Invoice Currency [End]
         
       *B601589,1 Change this line to check if the adjustment is a approved
       *          adjustment or a calculated adjustment [Begin]
       *CASE APDIST.CAPDACTID $ 'SJ'
       *  lnTotPaymnt = lnTotPaymnt - APDIST.nApdAmnt
       CASE APDIST.CAPDACTID $ 'S'
         lnTotPaymnt = lnTotPaymnt - APDIST.nApdAmnt

       *B607288,1 ALB Fix bug if the payment diff than invoice currency and with diff Exchange Rate [Begin]
       *CASE APDIST.CAPDACTID $ 'J' .AND. APDIST.nApdLinNo <> 1
       * lnTotPaymnt = lnTotPaymnt - APDIST.nApdAmnt
         CASE APDIST.CAPDACTID $ 'J' AND APINVHDR.CCURRCODE <> APPAYMNT.CCURRCODE ;
              AND APDIST.cApdTrTyp <> "A"
           lcExSin1   = gfGetExSin(@lcExSin2,APINVHDR.CCURRCODE,APPAYMNT.CCURRCODE)
           lcExSin1   = IIF(lcExSin1 = '*' , '/' , '*')
           lcExSin2   = IIF(lcExSin2 = '*' , '/' , '*')
           lnUnit     = APINVHDR.NCURRUNIT
           lcInvCurr  = APINVHDR.CCURRCODE
           lcPayCurr  = APPAYMNT.CCURRCODE
           ldExRateDt = APPAYMNT.DPAYDATE

           *B122247,1 NNA 04/21/2004 (Begin) Get the Ex_rate from the APDIST File insteed of the apinvhdr file
           *B122247,1 NNA             because the Apdist File Have the Ex_rate From any Foreign Currency to the base
           *B122247,1 NNA             currency without looking to the invoice Currency                    
           *lnExRate   = APINVHDR.NEXRATE

           *B123314,1 MHM 07/12/2004  Get exchange rate accourding to payment[Start]
           *lnExRate   = APDIST.NEXRATE           
           IF APINVHDR.CCURRCODE = APPAYMNT.CCURRCODE
             lnExRate   = APDIST.NEXRATE           
           ELSE
             lnExRate   = APINVHDR.NEXRATE
           ENDIF   
           *B123314,1 MHM [End]
           
           *B122247,1 NNA (End)
           
           lnExRate = IIF(lnExRate = 0 ,lfGetLstRat(lcInvCurr,lcPayCurr,ldExRateDt),lnExRate)
           lnTotPaymnt = lnTotPaymnt-ROUND(APDIST.NAPDAMNT &lcExSin1 lnExRate &lcExSin2 lnUnit,2)
         
       *B601589,1 Change this line [End]
       *CASE APDIST.cApdTrTyp = "A"
       CASE APDIST.cApdTrTyp = "A" AND APDIST.CAPDACTID $ 'A'
       *B607288,1 ALB Fix bug if the payment diff than invoice currency and with diff Exchange Rate [End]
           lnTotPaymnt = lnTotPaymnt+APDIST.NAPDAMNT
     ENDCASE     
  ENDSCAN
ELSE
*B607288,1 ALB Fix bug if the payment diff than invoice currency and with diff Exchange Rate [Begin]
*B601236,1 M.H End.
*  SUM REST nAPdAmnt WHILE CINVNO+CVENDCODE = lcInv+lcVend;
       FOR capdtrtyp $ "MNHP" AND capdactID = "A";
       AND dAPDtrdat > ldRpCurDat AND cApdStat  <> 'V' TO lnTotPaymnt

  SUM REST nAPdAmnt WHILE CINVNO+CVENDCODE = lcInv+lcVend;
      FOR ((capdtrtyp $ "MNHP" AND capdactID = "A") .OR. ( capdtrtyp = "A" AND nApdAmnt < 0));
      AND dAPDtrdat > ldRpCurDat AND cApdStat  <> 'V' TO lnTotPaymnt 
*B601236,1 M.H End.
*B607288,1 ALB Fix bug if the payment diff than invoice currency and with diff Exchange Rate [End]
ENDIF

*B601236,1 M.H Begin.
IF apinvhdr.nInvAmnt > 0 AND lnTotPaymnt <> apinvhdr.ninvpaid + apinvhdr.ninvdistk + apinvhdr.ninvadj
  SELECT APDIST
  lcOldTag = SET("ORDER")
  SET ORDER TO PAYMNTS
  SEEK "A"+REPL(" ",FSIZE("CBNKCODE"))+REPL(" ",FSIZE("CCHKACCT"))+lcInv
*---Alb1710
*  SUM REST nAPdAmnt WHILE CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF="A"+REPL(" ",FSIZE("CBNKCODE"))+;
                          REPL(" ",FSIZE("CCHKACCT"))+lcInv ;
                      FOR cVendCode = lcVend ;
                      AND dAPDtrdat > ldRpCurDat ;
                      AND cApdStat  <> 'V';
                      AND nApdAmnt > 0 TO lnTotDebit
  SUM REST nAPdAmnt WHILE CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF="A"+REPL(" ",FSIZE("CBNKCODE"))+;
                          REPL(" ",FSIZE("CCHKACCT"))+lcInv ;
                      FOR cVendCode = lcVend ;
                      AND dAPDtrdat > ldRpCurDat ;
                      AND cApdStat  <> 'V';
                      AND (cApdActId = 'A' AND nApdAmnt > 0);
                      TO lnTotDebit
 *--Alb1710
  lnTotPaymnt = lnTotPaymnt + lnTotDebit
  SET ORDER TO &lcOldTag
ENDIF
*B601236,1 M.H End.


*E300296,1 M.H 05/30/96 End.
*B601015,1 Hesham ( End )
SELECT (lnRpAlias)
GO RECNO()
llCanPrint  = (apinvhdr.ninvamnt - apinvhdr.ninvpaid - apinvhdr.ninvdistk - apinvhdr.ninvadj+lnTotPaymnt <> 0)
RETURN IIF(llCanPrint,lnTotPaymnt,0)

*!**************************************************************************
*!
*!      Function: lfSumPaymnt
*!
*!**************************************************************************
*
FUNCTION lfSumPaymnt
PARAMETERS lcVend,lcInv
ldComDate={}
lnTotPaymnt = 0
llCanPrint  = .F.
DO CASE
  CASE BETWEEN(APINVHDR.DINVDUDAT,ldRpC1F,ldRpC1T)
    ldComDate=ldRpC1T
  CASE BETWEEN(APINVHDR.DINVDUDAT,ldRpC2F,ldRpC2T)
    ldComDate=ldRpC2T
  CASE BETWEEN(APINVHDR.DINVDUDAT,ldRpC3F,ldRpC3T)
    ldComDate=ldRpC3T
  CASE BETWEEN(APINVHDR.DINVDUDAT,ldRpC4F,ldRpC4T)
    ldComDate=ldRpC4T
ENDCASE 
lnRPAlias=SELECT()
SELECT APDIST
SUM REST nAPdAmnt WHILE CINVNO+CVENDCODE = lcInv+lcVend;
    FOR capdtrtyp $ "MNHP" AND capdactID = "A";
    AND dAPDtrdat > ldComDate TO lnTotPaymnt 
SELECT (lnRpAlias)
GO RECNO()
llCanPrint  = (apinvhdr.ninvamnt - apinvhdr.ninvpaid - apinvhdr.ninvdistk - apinvhdr.ninvadj+lnTotPaymnt <> 0)
RETURN ''

*!**************************************************************************
*!
*!      Function: lfvConPaymnt
*!
*!**************************************************************************
*
FUNCTION lfvConPaymnt
*B803339,1   AMH 06/13/2000 To subres the vendor balance if zero (Start)
*IF !llRpConPay
*  lcRpForm = IIF(lcRpCType='S',"APAGEDS","APAGEDD")
*B601015,1 Hesham (Start)
*B601015,1 removing part of the hidden filter instead of changing
*B601015,1 it
*  laOGHDFlt[2,1] = "APINVHDR.DINVDUDAT"
*  laOGHDFlt[2,3] = "D"
*  laOGHDFlt[2,5] = "Between"
*  laOGHDFlt[2,6] = DTOC(ldRpCurDat-lnRpPrd4)+lcElmSep+DTOC(ldRpCurDat)
*  laOGHDFlt[2,7] = "V"
*  laOGHDFlt[2,1] = "APINVHDR.DINVDUDAT"
*  laOGHDFlt[2,3] = "D"
*  laOGHDFlt[2,5] = "Between"
*  laOGHDFlt[2,6] = ""
*  laOGHDFlt[2,7] = "V"
*ELSE 
*  lcRpForm = IIF(lcRpCType='S',"APAGECS","APAGECD")
*  laOGHDFlt[2,1] = "ABS(apinvhdr.ninvamnt)"
*  laOGHDFlt[2,3] = "N"
*  laOGHDFlt[2,5] = "Greater Than"  
*  laOGHDFlt[2,6] = "ABS(apinvhdr.ninvpaid) + ABS(apinvhdr.ninvadj) + ABS(apinvhdr.ninvdistk)"  
*  laOGHDFlt[2,7] = "E"
*ENDIF


IF !llRpConPay
  lcRpForm = IIF(lcRpCType='S',"APAGEDS","APAGEDD")
  *B603724,1 AKA   Add this part of hidding filter if include payment is No (Start)
  laOGHDFlt[2,1] = "APINVHDR.DINVDUDAT"
  laOGHDFlt[2,3] = "D"
  laOGHDFlt[2,5] = "Between"
  laOGHDFlt[2,6] = ""
  laOGHDFlt[2,7] = "V"
  *B603724,1 AKA (End)
ELSE 
  lcRpForm = IIF(lcRpCType='S',"APAGECS","APAGECD")
  *B603724,1 AKA   Add this part of hidding filter if include payment is yes (Start)
  *lcRpForm = IIF(lcRpCType='S',"APAGECS","APAGECD")  && Remark By Alb
  laOGHDFlt[2,1] = "ABS(apinvhdr.ninvamnt)"
  laOGHDFlt[2,3] = "N"
  laOGHDFlt[2,5] = "Greater Than"  
  laOGHDFlt[2,6] = "ABS(apinvhdr.ninvpaid) + ABS(apinvhdr.ninvadj) + ABS(apinvhdr.ninvdistk)"  
  laOGHDFlt[2,7] = "E"
  *B603724,1 AKA (End)
ENDIF
*B803339,1   AMH 06/13/2000 To subres the vendor balance if zero (End)

*!**************************************************************************
*!
*!      Function: lfvRepForm
*!
*!**************************************************************************
*
FUNCTION lfvRepForm
IF !llRpConPay
  lcRpForm = IIF(lcRpCType='S',"APAGEDS","APAGEDD")
ELSE
  lcRpForm = IIF(lcRpCType='S',"APAGECS","APAGECD")
ENDIF  
*!**************************************************************************
*!
*!      Function: lfvPrd
*!
*!**************************************************************************
* 
FUNCTION lfvPrd

*B600490,1  Malak Hanna  {Begin}
*B600490,1  Adjusting the Messages in this report.

DO CASE

  *B126840,1 MHM 03/16/2005 Fix bug of Variable llInvDate not found (no need for this variable)[Start]
  *CASE lnRpPrd1 <= 0 AND !llInvDate
  CASE lnRpPrd1 <= 0 
  *B126840,1 MHM 03/16/2005 [End]
  
    ** MESSAGE : " ð should be greater than ð."
    **           "           ® Ok ¯           "
    =gfModalGen("TRM04072B00000","DIALOG",'No. of days for the 1st period|zero')

  *B126840,1 MHM 03/16/2005 Fix bug of Variable llInvDate not found (no need for this variable)[Start]
  *CASE lnRpPrd1 >= lnRpPrd2 AND !llInvDate
  CASE lnRpPrd1 >= lnRpPrd2 
  *B126840,1 MHM 03/16/2005 [End]
  
    ** MESSAGE : " ð should be greater than ð."
    **           "           ® Ok ¯           "
    =gfModalGen("TRM04072B00000","DIALOG",'No. of days for the 2nd period|no. of days for the 1st period')

  *B126840,1 MHM 03/16/2005 Fix bug of Variable llInvDate not found (no need for this variable)[Start]
  *CASE lnRpPrd2 >= lnRpPrd3 AND !llInvDate
  CASE lnRpPrd2 >= lnRpPrd3 
  *B126840,1 MHM 03/16/2005 [End]
    ** MESSAGE : " ð should be greater than ð."
    **           "           ® Ok ¯           "
    =gfModalGen("TRM04072B00000","DIALOG",'No. of days for the 3rd period|no. of days for the 2nd period')

  *B126840,1 MHM 03/16/2005 Fix bug of Variable llInvDate not found (no need for this variable)[Start]
  *CASE lnRpPrd3 >= lnRpPrd4 AND !llInvDate
  CASE lnRpPrd3 >= lnRpPrd4 
  *B126840,1 MHM 03/16/2005 [End]
  
    ** MESSAGE : " ð should be greater than ð."
    **           "           ® Ok ¯           "
    =gfModalGen("TRM04072B00000","DIALOG",'No. of days for the 4th period|no. of days for the 3rd period')


  OTHERWISE
    =lfvCurDat()
    RETURN .T. 
ENDCASE 

*B600490,1  Malak Hanna  {End}
lcRpVar=SYS(18)
&lcRpVar=lcOldVal
SHOW GET (lcRpVar)
_CUROBJ = _CUROBJ

*!**************************************************************************
*!
*!      Function: lfvCurDisp
*!
*!**************************************************************************
*E300296,1 M.H 01/02/96 Add the currency to the AP reports.
*
FUNCTION lfvCurDisp
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

*E301301,1 AKA (Start)
* If forgin currency you must be sure invoice currency has value.
IF lcRpCurr = 'F' .AND. EMPTY(laOGFxFlt[1,6])
  laOGFxFlt[1,6]= gcBaseCurr
ENDIF
SHOW GET laOGFxFlt[1,6]
*E301301,1 AKA (End)


*!*************************************************************
*! Name      : lfwCurCode
*! Developer : Mohamed Hassan
*! Date      : 12/17/1995
*! Purpose   : This function called from the currency field to
*!             validate the currency.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
*B300296,1 New when function to save the old currency code.
FUNCTION lfwCurCode

lcOldCurr = laOGFxFlt[1,6]

*!*************************************************************
*! Name      : lfvCurCode
*! Developer : Mohamed Hassan
*! Date      : 12/17/1995
*! Purpose   : This function called from the currency field to
*!             validate the currency.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
*B300296,1 New valid function to validate the currency code.
FUNCTION lfvCurCode
*E301301,1 AKA (Start)
* Allow blank curreny in case user have no selected forign currency.
*B604068,1 AAN 12/10/2000 Add new cond. in if cond. to allow user not enter currency code[Start].
*IF EMPTY(laOGFxFlt[1,6])   && .AND. lcRpCurr <> 'F' 
IF EMPTY(laOGFxFlt[1,6])  
*B604068,1 AAN 12/10/2000 Add new cond. in if cond. to allow user not enter currency code[End].
  llAllCurr  = .T.
  RETURN
ENDIF
llAllCurr  = .F.
*E301301,1 AKA (End)


IF !SEEK(laOGFxFlt[1,6],'SYCCURR') .OR. ATC("?",laOGFxFlt[1,6]) > 0
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
    laOGFxFlt[1,6] = lcOldCurr
  ELSE
    laOGFxFlt[1,6] = laTemp[1]
  ENDIF
ENDIF

SHOW GET laOGFxFlt[1,6]

SELECT APINVHDR



*!**************************************************************************
*!
*!      Function: lfClearRep
*!
*!**************************************************************************
*E301301,1 AKA 
FUNCTION lfClearRep
IF FILE(gcWorkDir +lcApInvHdr+ '.CDX') 
  SELECT APINVHDR
  CLOSE INDEX
  ERASE (gcWorkDir +lcApInvHdr+ '.CDX')
ENDIF


*!**************************************************************************
*!
*!      Function: lfCreatNdx
*!
*!**************************************************************************
*E301301,1 AKA
FUNCTION lfCreatNdx
SELECT APINVHDR
IF EMPTY(laOGFxFlt[1,6]) .AND. lcRpCurr <> 'F' 
  * If index name is already defined 
  IF TYPE("lcApInvHdr") <> 'U'
    * If index file is already creadted 
    IF !FILE(gcWorkDir +lcApInvHdr+ '.CDX') 
      lcApInvHdr = gfTempName()        
      INDEX ON cvendcode+ccurrcode+cinvno  TAG cVenCurTag OF (gcWorkDir +  lcApInvHdr + '.CDX')    
    ELSE
      SET ORDER TO TAG cVenCurTag OF (gcWorkDir +  lcApInvHdr + '.CDX')    
    ENDIF
  ELSE    && ELASE OF  "If index name is already defined "
    lcApInvHdr = gfTempName()          
    INDEX ON cvendcode+ccurrcode+cinvno  TAG cVenCurTag OF (gcWorkDir +  lcApInvHdr + '.CDX')    
  ENDIF  && ENDIF OF  "If index name is already defined "
ELSE  
  SET ORDER TO CURVENINV
ENDIF
  

*!**************************************************************************
*! Name      : lfvApAcCod
*! Developer : Sameh Saiid Ezzat
*! Date      : 02/22/2000
*! Purpose   : This function is to validate the accounts from 
*!             the chart of account of the active company or the
*!             chart of account of another company.
*!**************************************************************************
*! Called from : The Option Grid
*!**************************************************************************
*! Example   :  =lfvApAcCod()
*!**************************************************************************
*
FUNCTION lfvApAcCod
PRIVATE lcSavAlias,lcFieldCont

lcSavAlias  = ALIAS()  && Variable to save the selected alias.
lcFieldCont = EVALUATE(SYS(18))   && Assign the content of the field to the variable.

*** Variable hold an empty account to compair with. ***
lcEmptyAcs = REPLICATE('0',lnApsAcLen)

*Prevent executing the browse if the account code is empty.
IF !EMPTY(STRTRAN(lcFieldCont,"-",""))

  IF llApGlLink .AND. lcFieldCont <> lcEmptyAcs

    SELECT('lcLinkChar')
    SET ORDER TO TAG ACCTCODE

    IF !SEEK(lcFieldCont) .OR. ATC('?',lcFieldCont) > 0
      DIMENSION laTemp[2]
      laTemp = ''
      lcSavBrFld=lcBrfields
      lcSavTitle=lcFile_Ttl

      lcBrfields="CACCTCODE :H= 'Account Code',;
                  CACCNLDES :H= 'Account Description'"

      lcFile_Ttl="Chart of accounts"
        
      =gfbrows(' ','CACCTCODE,CACCNLDES','laTemp')

      lcFile_Ttl=lcSavTitle
      lcBrfields=lcSavBrFld

      IF !EMPTY(laTemp[1])
        lcFieldCont = ALLTRIM(laTemp[1])
      ELSE
        lcFieldCont = REPLICATE('0',lnApsAcLen)  
      ENDIF
    ENDIF  
 
    IF !EMPTY(lcSavAlias)
      SELECT(lcSavAlias)
    ENDIF  
    lcVarName = SYS(18)
    
    *Blank the account code if the value equal 0.
    &lcVarName= IIF(VAL(lcFieldCont) = 0 , "" , lcFieldCont)

  ENDIF
ENDIF

*Blank the account code if the value equal 0 and there is no GL link.
IF VAL(lcFieldCont) = 0 .AND. llApGlLink 
  lcVarName  = SYS(18)
  &lcVarName = ""
ENDIF

IF !EMPTY(lcSavAlias)
  SELECT(lcSavAlias)
ENDIF  
*-- End of lfvApAcCod.


*!**************************************************************************
*! Name      : lfColData
*! Developer : Khalid Mohi El-Din
*! Date      : 07/07/2000
*! Purpose   : Is used to get all the AP invoices according to the
*!             selected criteria and insert it in a temporary file to 
*!             be used in the print process.
*!**************************************************************************
*! Example   :  =lfColData()
*!**************************************************************************
*!B603679,1 KHM 07/04/2000 
*!**************************************************************************
FUNCTION lfColData
*B607464,1 ALB Add Thermometer when collecting the data from the invoice file [BEGIN]
PRIVATE lnTotRec , lnCurRec
*B607464,1 ALB Add Thermometer when collecting the data from the invoice file [END]
*-- Creating the temporary file by copying the structure of APINVHDR and add
*-- two new fields nTotPay to hold the total payable invoices, and lCanPrint
*-- to determine whether to print the objects in the FRX or not.
SELECT APINVHDR
=AFIELD(laApInvHdr)
lnFileStru = ALEN(laApInvHdr,1)
DIMENSION laApInvHdr[lnFileStru+2,4]
laApInvHdr[lnFileStru+1,1] = 'nTotPay'
laApInvHdr[lnFileStru+1,2] = 'N'
laApInvHdr[lnFileStru+1,3] = 14
laApInvHdr[lnFileStru+1,4] = 2
laApInvHdr[lnFileStru+2,1] = 'lCanPrint'
laApInvHdr[lnFileStru+2,2] = 'L'
laApInvHdr[lnFileStru+2,3] = 0
laApInvHdr[lnFileStru+2,4] = 0

CREATE TABLE (gcWorkDir+TmpApInvHd) FROM ARRAY laApInvHdr
=gfOpenFile(gcWorkDir+TmpApInvHd,' ','EX')
*-- Setting the index 
IF EMPTY(laOGFxFlt[1,6]) .AND. lcRpCurr <> 'F' 
  INDEX ON cVendCode+cCurrCode+cInvNo  TAG (TmpApInvHd) OF (TmpApInvHd)
ELSE
  INDEX ON cCurrCode+cVendCode+cInvNo  TAG (TmpApInvHd) OF (TmpApInvHd)
ENDIF

*-- The FOR condition of the SCAN command in the APINVHDR. Where it holds
*-- the selected criteria.
lcForExp = 'FOR '+lcRpExp+ '.AND.' + IIF(EMPTY(lcRpRefnce),".T.",' LIKE("'+STRTRAN(lcRpRefnce,' ','?')+'",APINVHDR.cinvref)')

SELECT APINVHDR

*B607464,1 ALB Add Thermometer when collecting the data from the invoice file [BEGIN]
SET ORDER TO
lnTotRec = RECCOUNT()
lnCurRec = 0
*B607464,1 ALB Add Thermometer when collecting the data from the invoice file [END]

SCAN &lcForExp
  *B607464,1 ALB Add Thermometer when collecting the data from the invoice file [BEGIN]
  *WAIT WINDOW 'Payable invoice # : '+ cInvNo NOWAIT
  lnCurRec = recno()
  =gfThermo(lnTotRec , lnCurRec , 'Checking AP invoices... ')
  *B607464,1 ALB Add Thermometer when collecting the data from the invoice file [END]
  *-- Calculating the Total payable.
  
  *B122247,1 NNA 04/21/2004 (Begin) input a Date's Condition to Know if the aging date that input in
  *B122247,1 NNA             Option Grid is > the Payment date from the Apdist file or Not
  IF ApDist.dAPDtrdat <= ldRpCurDat
  *B122247,1 NNA (End)
    lnAmtToPay = nInvAmnt-nInvPaid-nInvDistk-nInvAdj
  *B122247,1 NNA 04/21/2004 (Begin) 
  *-- if the aging date < Payment date I get the lnAmtToPay by Decreasing the invoice amount
  *-- with any changed on the invoice but multiply the Ex_rate in the ApinvHdr file to Get
  *-- the Paid Amount by the Base Currency that Because I get lnTotPaymnt From the (lfSumALL) Function
  *-- By the Base Currency , in this case the Two Amount will be equal and lost them with Each +/- 
  *-- so I get the invoice amount as net
  ELSE
    *B123314,1 MHM 12/07/2004 Get correct sign in case of multi curr.[Start]
    *lnAmtToPay = nInvAmnt- ((nInvPaid+nInvDistk+nInvAdj) * nExRate)
    lnAmtToPay = nInvAmnt-nInvPaid-nInvDistk-nInvAdj
    *B123314,1 MHM [End]
  ENDIF
  *B122247,1 NNA (End)
  
  *IF cvenpmeth <> 'M'
    *B606839,1 KHM 04/06/2003 (Begin) Deduct all the voided payments that are less than the 
    *B606839,1                aging date by adding a new function lfViodPayt.
    lnAmtToPay = lnAmtToPay - lfViodPayt(cVendCode,cInvNo)
    *B606839,1 KHM 04/06/2003 (End)
  *ENDIF

  *-- Calling lfSumAll() to get the open amount of the invoice within
  *-- the date that was entered in the Option Grid.
  lnAmtToPay = lnAmtToPay + lfSumall(cVendCode,cInvNo)
  
  *-- Check if there is an open amount within the selected date then add
  *-- a new record to the temporary file.
  IF lnAmtToPay <> 0
     SCATTER MEMVAR MEMO
     INSERT INTO &TmpApInvHd FROM MEMVAR
     REPLACE &TmpApInvHd..nTotPay WITH lnAmtToPay,;
             &TmpApInvHd..lCanPrint WITH llCanPrint
      
  ENDIF 
  
ENDSCAN
*B607464,1 ALB Add Thermometer when collecting the data from the invoice file [BEGIN]
IF lnTotRec > lnCurRec 
  lnCurRec = lnTotRec 
  =gfThermo(lnTotRec , lnCurRec , 'Checking AP invoices...')
ENDIF
*B607464,1 ALB Add Thermometer when collecting the data from the invoice file [END]

WAIT CLEAR
  
*-- Check if the temporary file was empty then display a notification
*-- message that no record was selected.
SELECT &TmpApInvHd
LOCATE
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF
nn = gfAmntDisp(nTotPay, lcRpCurr , ldRpExDate , lcRpTmpNam)
SET RELATION TO cvendcode        INTO APVENDOR ADDITIVE
SET RELATION TO CINVNO+CVENDCODE INTO APDIST ADDITIVE
DO gfDispRe WITH EVAL('lcRpForm')
SET RELATION TO  
SELECT APDIST
SET RELATION TO

USE IN (TmpApInvHd)

*-- Delete the temporary file with its related index.
ERASE(gcWorkDir+TmpApInvHd+'.DBF')
ERASE(gcWorkDir+TmpApInvHd+'.CDX')  
RETURN

*khalid
*!**************************************************************************
*! Name      : lfViodPayt
*! Developer : Khalid Mohi El-Din
*! Date      : 04/06/2003
*! Purpose   : To calculate the voided payments that are less or equal to the aging date.
*!**************************************************************************
*! Example   :  =lfColData()
*!**************************************************************************
*B606839,1 KHM 04/06/2003 Added
*!**************************************************************************
FUNCTION lfViodPayt
PARAMETERS lcVend,lcInv

lnTotPaymnt = 0
llCanPrint  = .F.
lnRPAlias = SELECT()
SELECT APDIST

SEEK lcInv+lcVend

*B607184,1 ALB Fix bug in the Advanced Payment [Begin]
*SUM REST nAPdAmnt WHILE cInvNo+cVendCode+cApdTrTyp = lcInv+lcVend;
      FOR capdtrtyp $ "MNHP" AND cApdActID = "A" ;
      AND dAPDtrdat <= ldRpCurDat;
      AND cApdStat  = 'V' TO lnTotPaymnt
IF APPAYMNT.lPayAdvAn 
  SUM REST nAPdAmnt WHILE cInvNo+cVendCode+cApdTrTyp = lcInv+lcVend;
        FOR ( capdtrtyp $ "MNHP" AND cApdActID = "A" .AND. nAPdAmnt < 0 ) ;
        AND dAPDtrdat <= ldRpCurDat ;
        AND cApdStat  = 'V' TO lnTotPaymnt
ELSE
  SUM REST nAPdAmnt WHILE cInvNo+cVendCode+cApdTrTyp = lcInv+lcVend;
        FOR ( capdtrtyp $ "MNHP" AND cApdActID = "A" ) ;
        AND dAPDtrdat <= ldRpCurDat ;
        AND cApdStat  = 'V' TO lnTotPaymnt

ENDIF      
*B607184,1 ALB Fix bug in the Advanced Payment [End]

SELECT (lnRpAlias)
GO RECNO()
RETURN lnTotPaymnt


*!**************************************************************************
*! Name      : lfGetLstRat
*! Developer : Albert Raif (ALB)
*! Date      : 05/05/2003
*! Purpose   : Return the Last exchange rate before the given date if there is no 
*!             Rate in the given date
*!********************************************************************************
*! Example   :  =lfGetLstRat()
*!**************************************************************************
*B607137,1 ALB 05/05/2003 Added
*!**************************************************************************

FUNCTION lfGetLstRat
PARAMETER lcToCurr, lcFromCurr, ldExRate
lcOldAlias = ALIAS()
IF !USED('Sycexch')
  =gfOpenFile(gcSysHome+'Sycexch','','SH')
ENDIF

SELECT Sycexch.nexrate, MAX(Sycexch.dratedate);
 FROM Sycexch;
 WHERE Sycexch.dratedate <= ldExRate ;
   AND Sycexch.cbasecurr = lcFromCurr;
   AND Sycexch.ccurrcode = lcToCurr  ;
 INTO ARRAY laExRate
 
 SELECT (lcOldAlias)
 RETURN IIF(_TALLY >=1 ,laExRate[1,1],1)
 
*!**************************************************************************
*! Name      :lfSumALL
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/02/2005
*! Purpose   : Get amount in correct way for All payment cases
*!********************************************************************************
*! Example   :  =lfGetLstRat()
*!**************************************************************************
*!123314,1 MHM
*! this function modified in the way to always get the transaction to Base currancy
*! then if invoice currancy is forign then get it back to Invoice currancy
*!
*!**************************************************************************
*
FUNCTION lfSumALL
PARAMETERS lcVend,lcInv

IF apinvhdr.ninvpaid + apinvhdr.ninvdistk + apinvhdr.ninvadj  = 0
  llCanPrint  = .T.
  lnTotPaymnt = 0
  RETURN 0
ENDIF

lnTotPaymnt = 0
llCanPrint  = .F.

llGetEq = .F.

lnRPAlias=SELECT()
SELECT APDIST

*B608290,1 TMI [Start] use the variable "llUseMulCr" instead of calling a global function each time
*IF gfGetMemVar('LLMULCURR')
IF llUseMulCr
*B608290,1 TMI [End  ] 
  SEEK lcInv+lcVend
  SCAN REST WHILE CINVNO+CVENDCODE = lcInv+lcVend;
        FOR ((capdtrtyp $ "MNHP" AND capdactID $ "CSJ") .OR. ( capdtrtyp = "A" AND nApdAmnt < 0));
        AND dAPDtrdat > ldRpCurDat;
        AND cApdStat  <> 'V'
     DO CASE
       CASE APDIST.CAPDACTID = 'C'
         lcExSin2 = ' '

         IF APINVHDR.CCURRCODE = APPAYMNT.CCURRCODE 
           IF APINVHDR.CCURRCODE = GcBaseCurr
             lnTotPaymnt = lnTotPaymnt-ROUND(APDIST.NAPDAMNT,2)
           ELSE
             lcExSin1   = gfGetExSin(@lcExSin2,APDIST.CCURRCODE,GcBaseCurr)
             lnExRate   = APDIST.NEXRATE           
             lnTotPaymnt = lnTotPaymnt - ROUND(APDIST.NAPDAMNT &lcExSin1 lnExRate ,2)
             llGetEq = .T.
           ENDIF
         ELSE

           llGetEq = .T.
           IF (APINVHDR.CCURRCODE <> GcBaseCurr) AND (APPAYMNT.CCURRCODE <> GcBaseCurr)
             lcExSin1   = gfGetExSin(@lcExSin2,APDIST.CCURRCODE,GcBaseCurr)
             lnExRate   = APDIST.NEXRATE           
             lnTotPaymnt = lnTotPaymnt - ROUND(APDIST.NAPDAMNT &lcExSin1 lnExRate ,2)
           ELSE      
           
             lcExSin1   = gfGetExSin(@lcExSin2,APINVHDR.CCURRCODE,APPAYMNT.CCURRCODE)

             lnUnit     = APINVHDR.NCURRUNIT
             lcInvCurr  = APINVHDR.CCURRCODE
             lcPayCurr  = APPAYMNT.CCURRCODE
             ldExRateDt = APPAYMNT.DPAYDATE

             lnExRate   = APDIST.NEXRATE           
             lnExRate = IIF(lnExRate = 0 ,lfGetLstRat(lcInvCurr,lcPayCurr,ldExRateDt),lnExRate)

             *B608634,1 WAM 07/28/2008 Fix paid amount calculation for invoices paid in foreign currency [T20080708.0003]
             *lnTotPaymnt = lnTotPaymnt-ROUND(APDIST.NAPDAMNT &lcExSin1 lnExRate &lcExSin2 lnUnit,2)
             lnTotPaymnt = lnTotPaymnt-ROUND(APDIST.NEQVAMNT,2)
             *B608634,1 WAM 07/28/2008 (End)
           
           ENDIF
           
         ENDIF
       CASE APDIST.CAPDACTID $ 'S'

         IF APINVHDR.CCURRCODE = GcBaseCurr
           lnTotPaymnt = lnTotPaymnt - APDIST.nApdAmnt
         ELSE
           lcExSin1   = gfGetExSin(@lcExSin2,APDIST.CCURRCODE,GcBaseCurr)
           lnExRate   = APDIST.NEXRATE           
           lnTotPaymnt = lnTotPaymnt - ROUND(APDIST.NAPDAMNT &lcExSin1 lnExRate ,2)
           llGetEq = .T.
         ENDIF
         
         CASE APDIST.CAPDACTID $ 'J' AND APDIST.cApdTrTyp <> "A"

           lcExSin1   = gfGetExSin(@lcExSin2,APINVHDR.CCURRCODE,APPAYMNT.CCURRCODE)
           lcExSin1   = IIF(lcExSin1 = '*' , '/' , '*')
           lcExSin2   = IIF(lcExSin2 = '*' , '/' , '*')
           lnUnit     = APINVHDR.NCURRUNIT
           lcInvCurr  = APINVHDR.CCURRCODE
           lcPayCurr  = APPAYMNT.CCURRCODE
           ldExRateDt = APPAYMNT.DPAYDATE

           lnExRate   = APDIST.NEXRATE           

           lcExSin1   = gfGetExSin(@lcExSin2,APDIST.CCURRCODE,GcBaseCurr)
           lnExRate   = APDIST.NEXRATE           
           lnTotPaymnt = lnTotPaymnt - ROUND(APDIST.NAPDAMNT &lcExSin1 lnExRate ,2)

       CASE APDIST.cApdTrTyp = "A" AND APDIST.CAPDACTID $ 'A'

           lnTotPaymnt = lnTotPaymnt+APDIST.NAPDAMNT
     ENDCASE     
  ENDSCAN

  IF llGetEq 
    lcExSin2 = ' '
    *lcExSin1   = gfGetExSin(@lcExSin2,GcBaseCurr,APINVHDR.CCURRCODE)
    lcExSin1   = gfGetExSin(@lcExSin2,APINVHDR.CCURRCODE)
    lcExSin1   = IIF(lcExSin1 = '*' , '/' , '*')
    lcExSin2   = IIF(lcExSin2 = '*' , '/' , '*')
    lnExRate   = APINVHDR.NEXRATE           
    lnTotPaymnt= ROUND(lnTotPaymnt &lcExSin1 lnExRate ,2)
  ENDIF  
  
ELSE

  SUM REST nAPdAmnt WHILE CINVNO+CVENDCODE = lcInv+lcVend;
      FOR ((capdtrtyp $ "MNHP" AND capdactID = "A") .OR. ( capdtrtyp = "A" AND nApdAmnt < 0));
      AND dAPDtrdat > ldRpCurDat AND cApdStat  <> 'V' TO lnTotPaymnt 

ENDIF

IF apinvhdr.nInvAmnt > 0 AND lnTotPaymnt <> apinvhdr.ninvpaid + apinvhdr.ninvdistk + apinvhdr.ninvadj
  SELECT APDIST
  lcOldTag = SET("ORDER")
  SET ORDER TO PAYMNTS
  SEEK "A"+REPL(" ",FSIZE("CBNKCODE"))+REPL(" ",FSIZE("CCHKACCT"))+lcInv
  SUM REST nAPdAmnt WHILE CAPDTRTYP+CBNKCODE+CCHKACCT+CAPDREF="A"+REPL(" ",FSIZE("CBNKCODE"))+;
                          REPL(" ",FSIZE("CCHKACCT"))+lcInv ;
                      FOR cVendCode = lcVend ;
                      AND dAPDtrdat > ldRpCurDat ;
                      AND cApdStat  <> 'V';
                      AND (cApdActId = 'A' AND nApdAmnt > 0);
                      TO lnTotDebit

  lnTotPaymnt = lnTotPaymnt + lnTotDebit
  SET ORDER TO &lcOldTag
ENDIF

SELECT (lnRpAlias)
GO RECNO()
llCanPrint  = (apinvhdr.ninvamnt - apinvhdr.ninvpaid - apinvhdr.ninvdistk - apinvhdr.ninvadj+lnTotPaymnt <> 0)
RETURN IIF(llCanPrint,lnTotPaymnt,0)

 