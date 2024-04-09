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
*B600289 MALAK 03/26/95. Change the number of lines per stub in laser and dot matrix form.
*B800058,1 MAN 08/03/95  Increase the dim. of the laFieldstr array by one and add 
*B800058,1               the cPageNo field structure
*E300266,1 MAN   08/07/95 Added in output report fields COUTADDR4,COUTADDR5
*B800101,1 M.H 09/11/95  Print the stub lines at the end of the check for 
*B800101,1 M.H 09/11/95  the DOS,WINDOW laser form.
*B600710,1 MAN 10/02/95  Fix Reprinting checks
*B600710,1               - Fix The next check number
*B600710,1               - Allow the user to print 1 or more from the stub checks
*E300296,1 M.H 12/17/95  Add the currency to the AP reports.
*E300316,1 M.H 12/17/95  Get the equation signs from the currency file.
*B800411,1 M.H 12/25/95  Change calling the condition of update the invoice from "IIF(!llTestChk .AND. llPrintChk .AND. !lcPrintMod='V',lfUpInv(),'')" 
*B800411,1                                                                        to "IIF(!llTestChk .AND. llPrintChk .AND. lcPrintMod='V',lfUpInv(),'')"
*B800411,1 M.H 12/25/95   Unlock the APBANKS record after the preview.
*B800415,1 ORABY 01/05/96 Fix the following bugs in the check printing
*B800415,1                  1.The windows laser format was not updating the files.
*B800415,1                  2.The program was printing two invoices for the same vendor in two checks.
*B800415,1                  3.In the laser forms the system acumalate the invoices of previous checks on the bottom stub.
*B800415,1                  4.The chk # is not printed correctely in the bottom stub in the laser form (prints chk# + 1).
*B800415,1                  5.In the case of a mulipule pages chks the files were not updated with the right chk #.
*B800415,1                  6.The invoices were not sorted in the reprint as they are in the normal chk printing.
*B800415,1                  7.The program print chks for vendors with negative amounts (Debit memoes).
*B600970,1 M.H 02/06/96   Update the last check #,last payment date fields.
*B601006,1 ORABY 03/21/96 Under WIN, in case of dot matrix checks print do not send formfeed after eaach check.
*B601016,1 Hesham El-Sheltawi 04/01/96
*B601016,1 adding hidden filter to ignore printing voided invoices
*C100591,1 Hesham El-Sheltawi 06/03/96
*C100591,1 reprint range of checks instead of one check at atime
*B601091,1 Hesham El-Sheltawi 06/04/96
*B601091,1 call the gfDispRe with .T. the last parameter
*B601091,1 to force printing to file first before the print
*B601091,1 to ignore any errors can be happed while printing
*B601091,1 Hesham El-Sheltawi 06/15/96
*B601091,1 add the function lfInsVdChr to make the lines
*B601091,1 that hold the VOID sign in the dotmatrix report

*B601091,1 change the dotmatrix report form APCHKPDD.FRX 
*B601091,1 to avoid all the fox report form command error
*B601129,1 Hesham El-Sheltawi 06/23/96 update all the invoices
*B601129,1 that has been reprinted in the apinvhdr file with
*B601129,1 the last check number and date
*E300406,1 Hesham El-Sheltawi 07/09/96
*E300406,1 check if the is any custom forms in the report directory
*E300406,1 then use this custom form instead of the standered one
*B601197,1 M.H 08/12/96 Variable lnAprPayB,lnAprDisB,lnAprAdjB,lnApr1099B NOT found.
*B601411,1 HS-Haytham El-Sheltawi 11/13/96 Using gfRemit instead of lfRemit 
*B601411,1                                 and gfGetAdr instead of lfGetAdr
*B601417,1 HS-Haytham El-Sheltawi 11/14/96 Made some changes in the Report APCHKPDL
*B601417,1                                 [Using gfGetAdr instead of geting 
*B601417,1                                 the Address from the file.]
*B601431,1 HS-Haytham El-Sheltawi 11/27/96 Made some changes in the Report APCHKPDD
*B601431,1                                 [Calling lfIncNxtChk just one time]
*C100728,1 M.H 12/09/96 Add the posting date to the APINVHDR file.
*B601416,1 HS    01/06/97 Change the calling of gfGetExSin so if the Invoice 
*B601416,1                currency is the base currency I make it the To  
*B601416,1                currency and if not the Approve currency is 
*B601416,1                the To currency 
*B601526,1  HS  01/19/97  1)Print the net amount and the check amount in the
*B601526,1                  checking account cuurency and print the currency
*B601526,1                  symbol for all the ammounts in the report
*B601526,1                2)Add the new field [nInvFAAp] in the APINVHDR file 
*B601526,1                  [the approved amount in the approve currency] 
*B601526,1                3)Replace the calculation using lcExSin3 and
*B601526,1                lcExSin4 with the new field  
*B601526,1                 4)Fill the field [APDIST.nApdLinNo] with 1 in the 
*B601526,1                   case of calculated adjustment to be able to 
*B601526,1                   know the calculated adjustment from the 
*B601526,1                   approved one
*B601621,1  HS  03/27/97  Change the condition on the total approved payment
*B601621,1                for the vendor and make it (not < 0).
*B601621,1                and change the varible lnTotInvApv and make it
*B601621,1                the sum of the new field nInvFAAp to make the
*B601621,1                calculated amounts with the same currency
*B601555,1  HS  03/30/97  Use the long describtion of the currency in the
*B601555,1                amount printed in string instaed of the word dollar
*E300643,1  HS 04/21/97 1)Make some changes for we have change the file
*E300643,1              SYCCODES name to CODES and make it a data file
*E300643,1              [Change its dir. from SYSFILES to DBFS]
*E300643,1              2)Make some changes for we have changed the function
*E300643,1              [gfCodDes]
*E300643,1              3)Make some changes for we have changed the Index
*E300643,1              tag CODES
*E300683,6 RENEE 06/15/97 Call screen from disk instead of applications 
*B801546,1 AMM   04/22/98 Increase the size of invoice field to hold 12 chrs 
*B801546,1 AMM            instead of 8 chrs in .frx
*B601957,1  HS   07/06/98 Add the paid 1099 amount, invoice discount
*B601957,1                and invoice adjustment in the distribution file
*B601957,1                in the debit memo case as well as the invoice
*B601957,1                case.
*B602081,1 AMM   09/24/98 1- Fix the bugs in Vendor Validations
*B602081,1 AMM            2- Display the session number in the title bar of OG
*B602081,1 AMM            3- Adjust the text and grophics mode POPUP.
*B602081,1 AMM            4- Delete duplicated records in SYREPUVR
*B602486,1 AMM   01/25/99 Fix the bug of not printing Address3&4&5 in some 
*B602486,1 AMM            cases (did in the .FRX)
*E301077,79 AMM 03/04/99 Enhance opening and closing files.
*E300789,4 AMM 03/07/99 Remove field cComp_id from files Accod, FisHD, FsPrd, FsHld, Codes
*B602731,1  HS  04/14/99 Fix the problem "Invalid path or file name" which
*B602731,1               happens when the user attempts to change the
*B602731,1               Report Form - when using customized checks -.
*B802151,1 Amin (AKA) 06/16/99 Using word VOID instead of VOIDCHK.BMP in check print report (lazer) 
*B802573,1 AKA 09/05/99 Replace AP invoice header (APINVHDR) and AP distribution file (APDIST)
*B802573,1              with Currency code, exchange rate and currency unit to void error
*B802573,1              'Numeric overfolw' while aprove debit memo.
*B802856,1 AMM 12/08/99 Fix the bug of not printing decimal fraction in amount description in laser form
*B604519,1 SSE 06/10/2000 Fix bug of not calculating correctly in 1st stub in case of Reprint checks.
*B604519,1                All modifications is in FRX.
*B604991,1 SSE 10/10/2001 Fix bug in Stub of printing amount in number and words for check which has more
*B604991,1                Than 1 page, it should display asteriks.
*B606044,1 ALB 10/03/2002 Fix payment method when saving setting in OG
*B606346,1 ALB 10/30/2002 Print only invoices that has date less or equal to the check date in OG
*B606042,1 ALB 11/07/2002 Print only invoices that has POST date less or equal the check date in OG
*C200445,1 ALB 11/21/2002 Increase number of lines in ap check to 30 lines instate of 11 lines
*C102793,1 ALB 01/26/2003 Replace the "Adjustment" field is printed in the check stubs. 
*C102793,1                with the invoice Reference entered through the payable invoice screen.
*C200537,1 ALB 05/29/2003 Change stander check layout using alena check then add total discount 
*C200537,1                and factor name. the change made in FRX
*B128400,1 MHM 15/06/2005 Prevent user from overwrite if two session opened
*B608942,1 TMI 07/22/2009 if a record is used by a user check that this user is in SYUSTATC of both A27 and A4XP [T20090224.0001]
*B609042,1 TMI 10/13/2009 Refresh the OG buttons after printing a custom report [T20090930.0021]
*B609103,1 HES 12/03/2009 Standard AP Printed Check form for MBI customers [T20091023.0007]
*B609201,1 WAM 04/07/2010 Run custom checks under local drive for SaaS environment [T20100406.0016]
************************************************************************************************************************************************************
*
*B601526,1 Add this lines [Begin]
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
*B601526,1 Add this lines [END]

*** Check if user wants report to printer or file 
llPrintChk = IIF(UPPER(gcDevice)='PRINTER' OR UPPER(gcDevice)='FILE',.T.,.F.)
lcFisYear  = ''      && fiscal year
lcFisPrd   = ''      && fiscal period
lcLineTwo  = ''      && holds the total check amount (uesd in FRX)
lcInvRemit = ''      && invoice remit to
lcFactor   = ''      && factor code
lcTFactor  = ''      && 'factor code'
ldRPChkDat = ldChkDat
*B606044,1 Fix payment method when saving setting in OG [Begin]
lcRpExp = lcRpExp + IIF(lcRpVenPay = 'A','',' AND APINVHDR.CVENPMETH = lcRpVenPay')
*B606044,1 Fix payment method when saving setting in OG [end]

*B606346,1 ALB Print only invoices that has date less or equal the check date in OG [Begin]
lcRpExp = lcRpExp + ' AND APINVHDR.DINVDATE <= ldChkDat '
*B606346,1 ALB Print only invoices that has date less or equal the check date in OG [end]

*B606042,1 ALB Print only invoices that has POST date less or equal the check date in OG [BEGIN]
lcRpExp = lcRpExp + ' AND APINVHDR.DPOSTDATE <= ldChkDat'
*B606042,1 ALB Print only invoices that has POST date less or equal the check date in OG [END]

*BEGIN B800101,1 M.H 

*C200445,1 ALB Increase number of lines in ap check to 20 lines instate of 11 lines [Begin]
=gfDoTriger('APCHKPRV',PADR('CHNGLINE',10))
*DIMENSION laFooter[11,6]  && Creat a new array to hold the footer records.
DIMENSION laFooter[lnRpStub,6]  && Creat a new array to hold the footer records.
*C200445,1 ALB Increase number of lines in ap check to 20 lines instate of 11 lines [end]

laFooter  = ''
lnFooter  = 0 
lcVendor  = '' && Variable to hold the vendor code.
*B601091,1 Hesham El-Sheltawi (Start)
lcPageNo  = ''
*B601091,1 Hesham El-Sheltawi (End)
*END B800101,1 M.H 
*B800415,1  If the vendor address is changed the invoices should be printed in a new chk.
lcAddress  = '' && Variable to hold the invoice address.
*B602081,1 AMM cursor name that holding the Void bitmap

*B802151,1 AKA [Start] Although the standard Laser form stopped using the BMP
*B802151,1     We didn't comment out the code that handels the BMP for compatibality
*B802151,1     with any custom form. Added Error handling to maintain the case where
*B802151,1     there is no installed application that handles BMP
PRIVATE lcCurErHnd
lcCurErHnd = ON("ERROR")
ON ERROR STORE .T. TO llDumErHnd
*B802151,1 AKA [End]

lcVoid = gfTempName()
*B602081,1 AMM Create the cursor.
CREATE CURSOR (lcVoid) (gPic G)
APPEND BLANK
APPEND GENERAL gPIC FROM ( gcBmpHome+'VOIDCHK.BMP')
*B602081,1 AMM end
ON ERROR &lcCurErHnd
*B802151,1 AKA Return to system error handler


IF llTestChk            && In case of allignment test.
  IF lfLokBank()        && If could lock the bank record
    SELECT (lcRpTargt)
    *B601091,1 Hesham El-Sheltawi (Start)
    *B601091,1 call the gfDispRe with .T. the last parameter
    *B601091,1 to force printing to file first before the print
    *B601091,1 to ignore any errors can be happed while printing
*    DO gfDispRe WITH EVAL('lcRepForm')
    DO gfDispRe WITH EVAL('lcRepForm'),.f.,.f.,'R',.T.    
    *B601091,1 Hesham El-Sheltawi (End)    
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
*B600710,1 MAN 10/02/95
lnPrnCh  =0 && the payment group number being printed ... used in reprinting only

IF ! lfReValidate()    && Check if the data validated or not
  RETURN
ENDIF

lcPayPrd   = "NVNHPAY"+ALLTRIM(STR(VAL(lcFisPrd)))



*B802573,1 AKA [Start]
* Declare some varibales will be used as a global in case of adv. pay.
* AP Invoice header fields  (Print checks currency)
lnCurrUnit  = 1            && Holds currency unit of printed checks
lnExRate    = 1   		   && Holds currency rate of printed checks
lcCurrCode  = gcBaseCurr   && Holds currency code of printed checks
lnEqvAmnt   = lnPaymnt    && Holds eqv. amount
*B802573,1 AKA [End]

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
      *B802573,1 AKA [Strart]
      =lfAdvPyCur()   && Get currency code, rate and unit of advanced payment
      *B802573,1 AKA [End]
      =lfAdvPay()     && Fill the data into files
      SELECT (lcRpTargt)

    *B601091,1 Hesham El-Sheltawi (Start)
    *B601091,1 call the gfDispRe with .T. the last parameter
    *B601091,1 to force printing to file first before the print
    *B601091,1 to ignore any errors can be happed while printing
    *DO gfDispRe WITH EVAL('lcRepForm')
    DO gfDispRe WITH EVAL('lcRepForm'),.f.,.f.,'R',.T.    
    *B601091,1 Hesham El-Sheltawi (End)    

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
    *B800415,1 Add order field invoice number so that the invoices are sorted in the print as the reprint
    *SELECT  &lcRpFields, 000000 AS 'nNoOfInv', '    ' AS 'cPageNo';
    FROM  &lcRpFiles ;
    WHERE &lcPrExp   ;
    ORDER BY CVENDCODE,CADDRESS;
    INTO DBF (gcWorkDir+lcRpTargt)
    
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
** Mohamed Hassan **          
         *B601006,1 (s) In case of DOTMATIX printing, advance after seach check by line feed not form feed
         lcPadvance = _PADVANCE
*         _PADVANCE  = IIF(lcRpName ='1','LINEFEED','FORMFEED')
*         _PADVANCE  = IIF(lcRpName ='1',IIF(_DOS,'FORMFEED','LINEFEED'),'FORMFEED')
         _PADVANCE  = IIF(lcRpName ='1','LINEFEED','FORMFEED')
         _PLENGTH   = 42
         *B601006,1 (F) 
         *B601091,1 Hesham El-Sheltawi (Start)
         *B601091,1 call the gfDispRe with .T. the last parameter
         *B601091,1 to force printing to file first before the print
         *B601091,1 to ignore any errors can be happed while printing
*         DO gfDispRe WITH EVAL('lcRepForm')
          *B601091,1 Hesham El-Sheltawi (Start)
          *B601091,1 Loop through the Invoices that is going to
          *B601091,1 printed to update before printing in the
          *B601091,1 approved invoices case
          IF llPrintChk AND (lcRpName='2' AND _WINDOWS) &&lcRePForm = 'APCHKPWL'
            GO TOP
            lcTmpChkNo=lcNxtChkNo
            lcChkPage = cPageNo
            *LOCATE FOR lfUpInv() OR lfNxtChkUpd() AND .F.
            SCAN
              =lfNxtChkUpd()            
              =lfUpInv()
            ENDSCAN
            lcNxtChkNo = lcTmpChkNo
          ENDIF  
          *B601091,1 Hesham El-Sheltawi (End)
		    *-- Amin, 06/16/2005 Variable holds .T. is the printing process was ok.
			lPrntCompl = .T.
			*-- Amin, 06/16/2005 Variable holds .T. is the printing process was ok.
          
         DO gfDispRe WITH EVAL('lcRepForm'),.f.,.f.,'R',.T.    
         *B601091,1 Hesham El-Sheltawi (End)    

         *B601006,1  Return _PADVANCE to its original value
         _PADVANCE = lcPadvance
** Mohamed Hassan **
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
** Mohamed Hassan **      
      *B601006,1 (s) In case of DOTMATIX printing, advance after seach check by line feed not form feed
      lcPadvance = _PADVANCE
*         _PADVANCE  = IIF(lcRpName ='1','LINEFEED','FORMFEED')
*         _PADVANCE  = IIF(lcRpName ='1',IIF(_DOS,'FORMFEED','LINEFEED'),'FORMFEED')
         _PADVANCE  = IIF(lcRpName ='1','LINEFEED','FORMFEED')         
*         _PLENGTH   = 42         
      *B601006,1 (F) 
      *B601091,1 Hesham El-Sheltawi (Start)
      *B601091,1 call the gfDispRe with .T. the last parameter
      *B601091,1 to force printing to file first before the print
      *B601091,1 to ignore any errors can be happed while printing
      *DO gfDispRe WITH EVAL('lcRepForm')
      DO gfDispRe WITH EVAL('lcRepForm'),.f.,.f.,'R',.T.    
      *B601091,1 Hesham El-Sheltawi (End)    

      *B601006,1  Return _PADVANCE to its original value
      _PADVANCE = lcPadvance
** Mohamed Hassan **
      IF llPrintChk          && in case output device = 'Printer' or 'File'
        =lfRePrnUpd()        && update master file.
*C100591,1 Remark disabling the objects of the next check number
*C100591,1 and the banck code & bank account        
*        laOGObjCnt[10] = .F. && disable the next check number in OG.
*        laOGObjCnt[9] = .F. && disable the next check number in OG.
*        lcRePrnChk = ''      
*        lcRpBnkCod = ''
*        lcRpChkAct = ''
*        lcNxtChkNo = ''
        STORE '' TO lcRpChkFrm,lcRpChkTo
*        SHOW GET lcRePrnChk        
        SHOW GET lcRpBnkCod
        SHOW GET lcRpChkAct
*        SHOW GET lcNxtChkNo DISABLE
*C100591,1 (End)        
      ENDIF  
      =lfUnLokBank()         && unlock the bank record
    ENDIF

ENDCASE

SHOW GET lcNxtChkNo     && Refresh the next check number

*TMI B609042,1 10/13/2009 [start] refresh og buttons
SHOW GET pbOption
SHOW GET pbAdvanced
SHOW GET pbRun
SHOW GET pbOGReset
SHOW GET pbOGpreview
SHOW GET pbOGSave
SHOW GET pbOGDel
SHOW GET pbClose
*TMI B609042,1 10/13/2009 [end ] refresh og buttons



*-- Amin, 06/16/2005 Dispaly Print is Completed message if lPrntCompl was True
*!*	IF llPrintChk AND 
*!*	  *** Printing is completed.
*!*	  =gfModalGen("INM04124B00000","DIALOG")  
*!*	ENDIF
IF llPrintChk AND lPrntCompl 
  *** Printing is completed.
  =gfModalGen("INM04124B00000","DIALOG")  
ENDIF
*-- Amin, 06/16/2005 Dispaly Print is Completed message if lPrntCompl was True


*B601526,1 Add this lines [Begin]
SET CURRENCY TO lcSavCurSm
*B601526,1 Add this lines [END]
*B602081,1 AMM Close the cursor


IF USED(lcVoid)
  USE IN (lcVoid)
ENDIF
*B602081,1 AMM end

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

    *B601621,1 Change this line to use the new field nInvFAAp to make the
    *          calculated amounts with the same currency [Begin]
    *lnTotInvApv = lnTotInvApv + NINVAMTAP
    lnTotInvApv = lnTotInvApv + nInvFAAp
    *B601621,1 Change this line [End]
    
    *** lnRpStub nubmer of lines per page 12 for DOS, 14 for WINDOWS
    REPLACE nNoOfInv WITH CEILING(lnCount/lnRpStub)
  ELSE 
    *** Check if the approval payment is less than 1 

    *B601621,1 Change this line to make the condition (The total approved
    *           payments < 0) [Begin]
    *IF lnTotInvApv <= 0 
    IF lnTotInvApv < 0 
    *B601621,1 Change this line [End]

       IF llPrintChk
         *** unlock all locked records.
         =lfClearLok()
       ENDIF  

      *B601621,1 Change this line to change the message [Begin]
      *  *** Approval payment for vendor ð must be grater than zero.
      *=gfModalGen("INM04084B00000","DIALOG",ALLTRIM(SUBSTR(lcGroup,1,8)))

      *** Message :"Approved payment for vendor ð can not be lees than zero"
      *** Choice  :"                         < OK >                        "
      =gfModalGen("INM04162B00000","DIALOG",ALLTRIM(SUBSTR(lcGroup,1,8)))
      *B601621,1 Change this line to change the message [End]

      RETURN .F.
    ENDIF  
    
    REPLACE nNoOfInv WITH 0     && Indicate to End Of Group.
    lnCount     = 0

    *B601621,1 Change this line to use the new field nInvFAAp to make the
    *          calculated amounts with the same currency [Begin]
    *lnTotInvApv = NINVAMTAP
    lnTotInvApv = nInvFAAp
    *B601621,1 Change this line [End]
    
    lcGroup     = CVendCode + CAddress
  ENDIF
  lnCount     = lnCount     + 1
  SELECT APINVHDR
  *** If device = 'File' Or 'Printer' and can lock cureent record.

  **B128400,1 change to local function to modify it and change overwrtie to be retry [Start]
  *IF llPrintChk AND !gfObj_Lock(.T.)

  IF llPrintChk AND !lfObj_Lock(.T.)
  **B128400,1 [End]
    *** Invoice ð for vendor ð is being edited. 
    *** Check printing is canceled.
    *-- Amin
    *=gfModalGen("INM04082B00000","DIALOG",ALLTRIM(APINVHDR.CINVNO)+'|'+ALLTRIM(CVendCode))
    *-- Amin
    SELECT (lcRpTargt)
    
    **B128400,1 don't clear lock in nthis case[Start]
    *=lfClearLok()
    lcInvNo = APINVHDR.CINVNO
    lcVendCode = APINVHDR.CVendCode
    SCAN REST 
	  SELECT APINVHDR
	  IF APINVHDR.CINVNO+APINVHDR.CVendCode <> lcInvNo +lcVendCode 
	    =gfObj_Lock(.F.) 
	  ENDIF 
	ENDSCAN
    **B128400,1 [End]
    
    RETURN .F.
  ENDIF

  SELECT (lcRpTargt)
  SKIP -1
  *B800415,1 (Start) Stop printing chks for vendors with negative amounts

  *B601621,1 Change this line to make the condition (The total approved
  *           payments < 0) [Begin]
  *IF BOF() AND lnTotInvApv <= 0 
  IF BOF() AND lnTotInvApv < 0 
  *B601621,1 Change this line [End]

    IF llPrintChk
      *** unlock all locked records.
      =lfClearLok()
    ENDIF  

    *B601621,1 Change this line to change the message [Begin]
    *  *** Approval payment for vendor ð must be grater than zero.
    *=gfModalGen("INM04084B00000","DIALOG",ALLTRIM(SUBSTR(lcGroup,1,8)))

    *** Message :"Approved payment for vendor ð can not be lees than zero"
    *** Choice  :"                         < OK >                        "
    =gfModalGen("INM04162B00000","DIALOG",ALLTRIM(SUBSTR(lcGroup,1,8)))
    *B601621,1 Change this line to change the message [End]

    RETURN .F.
  ENDIF  
  *B800415,1 (END)
ENDDO

SELECT (lcRpTargt)
*B800415,1 (Start) Make sure that invoices for the same vendor+address is printed on the same chk.
*REPLACE ALL cPageNo WITH  PADL(CEILING(RECNO()/lnRpStub),4)
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
  *B800415,1 Field to hold the page number in the printing sequance, to be used by the chk form to advance pages.
  REPLACE cPageNo WITH PADL(lnPgCntr,4)
  lcGroup = CVendCode + CAddress
ENDSCAN
*B800415,1 (End)
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
*B600710,1 MAN 10/02/95 Rewrote the function
FUNCTION lfUpInv

IF llEndGrp
  *B800415,1 (Start) Change the method for calculating the chk # in order to update the files
  *lnNoOfChks = APCHECKS.NCHKNXTPN + CEILING(lnNoOfIvs/lnRpStub)
  *REPLACE APCHECKS.NCHKNXTPN WITH INT(VAL(lcNxtChkNo)) + MAX(&lcRpTargt..nNoOfInv,1) + IIF (&lcRpTargt..nNoOfInv < 1, 0, 1)
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
  *B800415,1 (End)
  
  
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

  *E300296,1 M.H Add the currency to the AP module.
  *E300296,1 M.H Check the Checking account currency rate with gcBaseCurr with the proceed date.
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
  *E300296,1 M.H End.
  
  
  SCAN REST 
    lnStubCnt = lnStubCnt + 1

    *E300316,1 M.H Get the equation signs. (Begin)
    *E300316,1 to exchange currency from invoice currency to company base currency.
    lcExSin2 = ' '
    lcExSin1 = gfGetExSin(@lcExSin2,&lcRpTargt..cCurrCode)

    *E300316,1 to exchange currency from invoice currency to approved currency.
    lcExSin4 = ' '

    *B601526,1 Ther is no need for this lines now [Begin]

    *B601416,1 Change the calling of gfGetExSin so if the [BEGIN]
    * Invoice currency is the base currency I make it the To currency 
    * and if not the Approve currency is the To currency 
    *lcExSin3 = gfGetExSin(@lcExSin4,&lcRpTargt..cCurrCode,&lcRpTargt..cAprCurCode)
    *B601416,1 IF Statment to check if the Invoice currency is the 
    * same as the base currency
     *IF &lcRpTargt..cCurrCode = gcBaseCurr 
     *  lcExSin3 = gfGetExSin(@lcExSin4,&lcRpTargt..cAprCurCode,&lcRpTargt..cCurrCode)
     *  lcExSin3 = IIF(lcExSin3 = '*' , '/' , '*')
     *  lcExSin4 = IIF(lcExSin4 = '*' , '/' , '*')
     *ELSE   && Else
     *  lcExSin3 = gfGetExSin(@lcExSin4,&lcRpTargt..cCurrCode,&lcRpTargt..cAprCurCode)
     *ENDIF  && End of IF 
    *B601416,1 Change the calling of gfGetExSin [END]

    *B601526,1 Ther is no need for this lines now [End]

    *E300316,1 to exchange currency from approved currency to company base currency.
    lcExSin6 = ' '
    lcExSin5 = gfGetExSin(@lcExSin6,&lcRpTargt..cAprCurCode)
    *E300316,1 (End)


    *E300296,1 M.H Add the currency to the AP module.
    *Calculate the amount to pay,Adjustment,Discount,1099 with the currency rate / unit.
    
    *B601197,1 M.H Begin.

    *B601526,1 Change this line (Replace the calculation using lcExSin3  
    *          and lcExSin4 with the new field ) [Begin]
    *lnAprPayB  = ROUND(&lcRpTargt..NINVAMTAP &lcExSin3 &lcRpTargt..NAPREXRAT &lcExSin4 &lcRpTargt..NAPRCURUNT &lcExSin5 lnChkExRat &lcExSin6 lnChkExUnt,2)
    lnAprPayB  = ROUND(&lcRpTargt..NINVFAAP &lcExSin5 lnChkExRat &lcExSin6 lnChkExUnt,2)
    *B601526,1 Change this line [End]
    
    lnAprDisB  = ROUND(&lcRpTargt..NINVDISAP &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT,2)
    lnAprAdjB  = ROUND(&lcRpTargt..NINVADJAP &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT,2)
    lnApr1099B = ROUND(&lcRpTargt..NINVA1099 &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT,2)
    *B601197,1 M.H End.
    *E300316,1 (End)

    *E300316,1 M.H Begin.
*    REPLACE APPAYMNT.NPAYAMNT  WITH APPAYMNT.NPAYAMNT  + &lcRpTargt..NINVAMTAP;
             APPAYMNT.NPAYDISC  WITH APPAYMNT.NPAYDISC  + &lcRpTargt..NINVDISAP;
             APPAYMNT.NPAYADJ   WITH APPAYMNT.NPAYADJ   + &lcRpTargt..NINVADJAP;
             APPAYMNT.NINV1099A WITH APPAYMNT.NINV1099A + &lcRpTargt..NINVA1099

    *B601526,1 Change this line (Replace the calculation using lcExSin3  
    *          and lcExSin4 with the new field ) [Begin]
    *REPLACE APPAYMNT.NPAYAMNT  WITH APPAYMNT.NPAYAMNT  + ROUND(&lcRpTargt..NINVAMTAP &lcExSin3 &lcRpTargt..NAPREXRAT &lcExSin4 &lcRpTargt..NAPRCURUNT,2);
    *        APPAYMNT.NPAYDISC  WITH APPAYMNT.NPAYDISC  + lnAprDisB;
    *        APPAYMNT.NPAYADJ   WITH APPAYMNT.NPAYADJ   + lnAprAdjB;
    *        APPAYMNT.NINV1099A WITH APPAYMNT.NINV1099A + lnApr1099B

    REPLACE APPAYMNT.NPAYAMNT  WITH APPAYMNT.NPAYAMNT  + ROUND(&lcRpTargt..NINVFAAP,2);
            APPAYMNT.NPAYDISC  WITH APPAYMNT.NPAYDISC  + lnAprDisB;
            APPAYMNT.NPAYADJ   WITH APPAYMNT.NPAYADJ   + lnAprAdjB;
            APPAYMNT.NINV1099A WITH APPAYMNT.NINV1099A + lnApr1099B

    *B601526,1 Change this line [End]
            
    *E300316,1 Hesham El-Sheltawi (Start)
    REPLACE APPAYMNT.CCURRCODE  WITH APCHECKS.CCURRCODE ;
            APPAYMNT.NEXRATE   WITH lnChkExRat ;
            APPAYMNT.NCURRUNIT WITH lnChkExUnt
    *E300316,1 (END)
    =gfAdd_Info('APPAYMNT')  && Add the audit information to the record.

*    REPLACE APVENDOR.NVENLPAYA WITH APVENDOR.NVENLPAYA + &lcRpTargt..NINVAMTAP  ;
             APVENDOR.NVEN1099B WITH APVENDOR.NVEN1099B + &lcRpTargt..NINVA1099  ;
             APVENDOR.NVENCPAY  WITH APVENDOR.NVENCPAY  + &lcRpTargt..NINVAMTAP  ;
             APVENDOR.NVENBAL   WITH APVENDOR.NVENBAL   - (&lcRpTargt..NINVAMTAP + &lcRpTargt..NINVDISAP + &lcRpTargt..NINVADJAP) ;
             APVENDOR.NVENOPNDR WITH APVENDOR.NVENOPNDR + IIF(&lcRpTargt..NINVAMNT < 0,&lcRpTargt..NINVAMTAP + &lcRpTargt..NINVDISAP+&lcRpTargt..NINVADJAP,0)

    REPLACE APVENDOR.NVENLPAYA WITH APVENDOR.NVENLPAYA + lnAprPayB ;
            APVENDOR.NVEN1099B WITH APVENDOR.NVEN1099B + lnApr1099B;
            APVENDOR.NVENCPAY  WITH APVENDOR.NVENCPAY  + lnAprPayB ;
            APVENDOR.NVENBAL   WITH APVENDOR.NVENBAL   - ROUND((&lcRpTargt..NINVAMTAP+&lcRpTargt..NINVDISAP+&lcRpTargt..NINVADJAP) &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT,2);
            APVENDOR.NVENOPNDR WITH APVENDOR.NVENOPNDR + ROUND(IIF(&lcRpTargt..NINVAMNT < 0,&lcRpTargt..NINVAMTAP + &lcRpTargt..NINVDISAP + &lcRpTargt..NINVADJAP &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT,0),2)

    =gfAdd_Info('APVENDOR')  && Add the audit information to the record.
                      
*   REPLACE APCHECKS.NCHKLPAMT WITH APCHECKS.NCHKLPAMT+&lcRpTargt..NINVAMTAP

    *B601526,1 Change this line (Replace the calculation using lcExSin3  
    *          and lcExSin4 with the new field ) [Begin]
    *REPLACE APCHECKS.NCHKLMAMT WITH APCHECKS.NCHKLMAMT + ROUND(&lcRpTargt..NINVAMTAP &lcExSin3 &lcRpTargt..NAPREXRAT &lcExSin4 &lcRpTargt..NAPRCURUNT,2)
    REPLACE APCHECKS.NCHKLMAMT WITH APCHECKS.NCHKLMAMT + ROUND(&lcRpTargt..NINVFAAP,2)
    *B601526,1 Change this line [End]
    
    =gfAdd_Info('APCHECKS')  && Add the audit information to the record.

*    REPLACE APVENHST.NVNHDISTKN WITH APVENHST.NVNHDISTKN + &lcRpTargt..NINVDISAP;
             APVENHST.NVNHTOTPA  WITH APVENHST.NVNHTOTPA  + &lcRpTargt..NINVAMTAP;
             APVENHST.NVNHADJ    WITH APVENHST.NVNHADJ    + &lcRpTargt..NINVADJAP;
             APVENHST.NVNHPCHKP  WITH APVENHST.NVNHPCHKP  + &lcRpTargt..NINVAMTAP;
             APVENHST.&lcPayPrd  WITH APVENHST.&lcPayPrd  + &lcRpTargt..NINVAMTAP

    REPLACE APVENHST.NVNHDISTKN WITH APVENHST.NVNHDISTKN + lnAprDisB;
            APVENHST.NVNHTOTPA  WITH APVENHST.NVNHTOTPA  + lnAprPayB;
            APVENHST.NVNHADJ    WITH APVENHST.NVNHADJ    + lnAprAdjB;
            APVENHST.NVNHMCHKP  WITH APVENHST.NVNHMCHKP  + lnAprPayB;
            APVENHST.&lcPayPrd  WITH APVENHST.&lcPayPrd  + lnAprPayB 

    =gfAdd_Info('APVENHST')  && Add the audit information to the record.

     
    SELECT APDIST 
    APPEND BLANK
*    REPLACE CVENDCODE  WITH &lcRpTargt..CVENDCODE ;
             CINVNO     WITH &lcRpTargt..CINVNO    ; 
             CAPDTRTYP  WITH 'P'                   ;
             DAPDTRDAT  WITH ldChkDat            ;
             LAPDPOST   WITH  .F.                  ;
             CAPDSTAT   WITH ''                    ;
             CAPDREF    WITH lcChkNo               ;
             CSTUBCHK   WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0');
             CAPDGLACT  WITH &lcRpTargt..CAPACCT    ;
             CAPDACTID  WITH 'A'                    ;
             CBATCHNO   WITH ''                     ;
             CTRNSLEDN  WITH ''                     ;
             CFISFYEAR  WITH lcFisYear              ;
             CFSPPRDID  WITH lcFisPrd               ;
             CAPSESSNO  WITH lcSession              ;
             CTAXCODE   WITH ''                     ;
             CBNKCODE   WITH lcRpBnkCod             ;
             CCHKACCT   WITH lcRpChkact             ;
             NAPDAMNT   WITH &lcRpTargt..NINVAMTAP + &lcRpTargt..NINVDISAP + &lcRpTargt..NINVADJAP

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

    *B601957,1 Change this line to add the paid 1099 amount in distribution
    *file in the debit memo case as well as the invoice case [Begin]
    
    *IF &lcRpTargt..NINVA1099 > 0
    
    *B601957,1 If there is a paid 1099 amount
    IF &lcRpTargt..NINVA1099 <> 0
    
    *B601957,1 Change this line to add the paid 1099 amount [End]
      
      APPEND BLANK 
*      REPLACE CVENDCODE   WITH &lcRpTargt..CVENDCODE;
               CINVNO      WITH &lcRpTargt..CINVNO;
               CAPDTRTYP   WITH 'P';
               DAPDTRDAT   WITH ldChkDat;
               LAPDPOST    WITH .F.;
               CAPDSTAT    WITH 'V';
               CAPDREF     WITH lcChkNo;
               CSTUBCHK    WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0') ;
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
               CCHKACCT    WITH lcRpChkact

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

    *B601526,1 Change this line (Replace the calculation using lcExSin3  
    *          and lcExSin4 with the new field ) [Begin]
    *REPLACE CVENDCODE   WITH &lcRpTargt..CVENDCODE;
    *        CINVNO      WITH &lcRpTargt..CINVNO;
    *        CAPDTRTYP   WITH 'P';
    *        DAPDTRDAT   WITH ldChkDat;
    *        LAPDPOST    WITH .F.;
    *        CAPDSTAT    WITH '';
    *        CAPDREF     WITH lcChkNo;
    *        CSTUBCHK    WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0');
    *        CAPDGLACT   WITH &lcRpTargt..CCHKGLACC;
    *        NAPDAMNT    WITH -&lcRpTargt..NINVAMTAP &lcExSin3 &lcRpTargt..NAPREXRAT &lcExSin4 &lcRpTargt..NAPRCURUNT;
    *        CAPDACTID   WITH 'C';
    *        CBATCHNO    WITH '';
    *        CTRNSLEDN   WITH '';
    *        CFISFYEAR   WITH lcFisYear;
    *        CFSPPRDID   WITH lcFisPrd;
    *        CAPSESSNO   WITH lcSession;
    *        CTAXCODE    WITH '';
    *        CBNKCODE    WITH lcRpBnkCod;
    *        CCHKACCT    WITH lcRpChkact;
    *        CCURRCODE   WITH &lcRpTargt..CAPRCURCOD;
    *        NEXRATE     WITH lnChkExRat;
    *        NCURRUNIT   WITH lnChkExUnt;
    *        NEQVAMNT    WITH -lnAprPayB

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

    *B601526,1 Change this line [End]
    
    =gfAdd_Info('APDIST')
      
    *B601957,1 Change this line to add the invoice discount in distribution
    *file in the debit memo case as well as the invoice case [Begin]
    
    *IF &lcRpTargt..NINVDISAP > 0    
    
    *B601957,1 If there is a discount
    IF &lcRpTargt..NINVDISAP <> 0    
    
    *B601957,1 Change this line to add the invoice discount [End]
      
      APPEND BLANK
*      REPLACE CVENDCODE WITH &lcRpTargt..CVENDCODE;
               CINVNO    WITH &lcRpTargt..CINVNO;
               CAPDTRTYP WITH 'P';
               DAPDTRDAT WITH ldChkDat;
               LAPDPOST  WITH .F.;
               CAPDSTAT  WITH '';
               CAPDREF   WITH lcChkNo;
               CSTUBCHK  WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0') ;
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
               CAPDGLACT WITH IIF(!EMPTY(APCHECKS.CDISCACCT),APCHECKS.CDISCACCT,IIF(SEEK(&lcRpTargt..CDIVISION,'APDIV') AND !EMPTY(APDIV.CDISCACCT),APDIV.CDISCACCT,APSETUP.CDISCACCT)) 

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

    *B601957,1 Change this line to add the invoice adjustment in distribution
    *file in the debit memo case as well as the invoice case [Begin]
    
    *IF &lcRpTargt..NINVADJAP > 0    
    
    *B601957,1 If there is an adjustment
    IF &lcRpTargt..NINVADJAP <> 0
    
    *B601957,1 Change this line to add the invoice adjustment [End]
      
      APPEND BLANK 
*      REPLACE CVENDCODE WITH &lcRpTargt..CVENDCODE ;
               CINVNO    WITH &lcRpTargt..CINVNO    ; 
               CAPDTRTYP WITH 'P'                   ;
               DAPDTRDAT WITH ldChkDat            ;
               LAPDPOST  WITH .F.                   ;
               CAPDSTAT  WITH ''                    ;
               CAPDREF   WITH lcChkNo               ;
               CSTUBCHK  WITH PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0') ;
               NAPDAMNT  WITH -&lcRpTargt..NINVADJAP;
               CAPDACTID WITH 'J'                   ;
               CBATCHNO  WITH ''                    ;
               CTRNSLEDN WITH ''                    ;
               CFISFYEAR WITH lcFisYear             ;
               CFSPPRDID WITH lcFisPrd              ;
               CAPSESSNO WITH lcSession             ;
               CTAXCODE  WITH ''                    ;
               CBNKCODE  WITH lcRpBnkCod            ;
               CCHKACCT  WITH lcRpChkact            ;
               CAPDGLACT WITH IIF(!EMPTY(APCHECKS.CADJACCT ),APCHECKS.CADJACCT,IIF(SEEK(&lcRpTargt..CDIVISION,'APDIV') AND !EMPTY(APDIV.CADJACCT ),APDIV.CADJACCT ,APSETUP.CADJACCT))

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

    *E300316,1 Used the variables hold signs in the equation. (Begin)
    lnExchDiff = lnAprPayB - ROUND(&lcRpTargt..NINVAMTAP &lcExSin1 &lcRpTargt..NEXRATE &lcExSin2 &lcRpTargt..NCURRUNIT,2)
    *E300296,1 Add the currency to the AP module.
    * Add the Eq. lnExchDiff
    * Add the base Currency,Rate = 1,Unit= 1
    IF lnExchDiff <> 0
      APPEND BLANK

*B601526,1 Change this line [Begin]
*          (Fill the line number with 1 to know the calculated adjustment
*          from the approved adjustment) 
*      REPLACE CVENDCODE WITH &lcRpTargt..CVENDCODE;
*              CINVNO    WITH &lcRpTargt..CINVNO; 
*              CAPDTRTYP WITH 'P';
*              DAPDTRDAT WITH ldChkDat;
*              LAPDPOST  WITH .F.;
*              CAPDSTAT  WITH '';
*              CAPDREF   WITH lcChkNo;
*              CSTUBCHK  WITH PADL(lnStubChkNo+CEILING(lnStubCnt/lnRpStub),8,'0');
*              NAPDAMNT  WITH lnExchDiff;
*              CAPDACTID WITH 'J';
*             CBATCHNO  WITH '';
*              CTRNSLEDN WITH '';
*             CFISFYEAR WITH lcFisYear;
*             CFSPPRDID WITH lcFisPrd;
*             CAPSESSNO WITH lcSession;
*             CTAXCODE  WITH '';
*             CBNKCODE  WITH lcRpBnkCod;
*             CCHKACCT  WITH lcRpChkact;
*             CAPDGLACT WITH lcExDifAcc;
*             CCURRCODE WITH gcBaseCurr;
*             NEXRATE   WITH 1;
*             NCURRUNIT WITH 1;
*             NEQVAMNT  WITH lnExchDiff

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
             
*B601526,1 Change this line [End]

      =gfAdd_Info('APDIST')  && Add the audit information to the record.
    ENDIF

*   REPLACE APINVHDR.NINVPAID  WITH APINVHDR.NINVPAID  + APINVHDR.NINVAMTAP;
            APINVHDR.NINVDISTK WITH APINVHDR.NINVDISTK + APINVHDR.NINVDISAP;
            APINVHDR.NINVADJ   WITH APINVHDR.NINVADJ   + APINVHDR.NINVADJAP;
            APINVHDR.NINV1099A WITH APINVHDR.NINV1099A + APINVHDR.NINVA1099;
            APINVHDR.CBNKCODE  WITH '' ;
            APINVHDR.CCHKACCT  WITH '' ;
            APINVHDR.CCHKGLACC WITH '' ;
            APINVHDR.NINVAMTAP WITH 0  ;
            APINVHDR.NINVDISAP WITH 0  ;
            APINVHDR.NINVADJAP WITH 0  ;
            APINVHDR.NINVA1099 WITH 0  ;
*B600970,1 M.H 02/06/96 Update the last check #,last payment date fields. 
*    REPLACE APINVHDR.NINVPAID   WITH &lcRpTargt..NINVPAID  + &lcRpTargt..NINVAMTAP;
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
            APINVHDR.NAPRCURUNT WITH 0

    *B601526,1 Add the new field (nInvFAAp) [Begin]
    *REPLACE APINVHDR.NINVPAID   WITH &lcRpTargt..NINVPAID  + &lcRpTargt..NINVAMTAP;
    *        APINVHDR.NINVDISTK  WITH &lcRpTargt..NINVDISTK + &lcRpTargt..NINVDISAP;
    *        APINVHDR.NINVADJ    WITH &lcRpTargt..NINVADJ   + &lcRpTargt..NINVADJAP;
    *        APINVHDR.NINV1099A  WITH &lcRpTargt..NINV1099A + &lcRpTargt..NINVA1099;
    *        APINVHDR.CBNKCODE   WITH '';
    *        APINVHDR.CCHKACCT   WITH '';
    *        APINVHDR.CCHKGLACC  WITH '';
    *        APINVHDR.NINVAMTAP  WITH 0 ;
    *        APINVHDR.NINVDISAP  WITH 0 ;
    *        APINVHDR.NINVADJAP  WITH 0 ;
    *        APINVHDR.NINVA1099  WITH 0 ;
    *        APINVHDR.CAPRCURCOD WITH '';
    *        APINVHDR.NAPREXRAT  WITH 0 ;
    *        APINVHDR.NAPRCURUNT WITH 0 ;
    *        APINVHDR.DCHKDATE   WITH ldChkDat;
    *        APINVHDR.CCHKNO     WITH lcChkNo

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

    *B601526,1 Add the new field (nInvFAAp) [End]

*B600970,1 M.H End.

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
*B600710,1 MAN 10/02/95 the next check number.
*REPLACE APCHECKS.NCHKNXTPN WITH INT(VAL(lcNxtChkNo)) + 1
*lcNxtChkNo = PADL(APCHECKS.NCHKNXTPN,8,'0')
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
*C100591,1 Hesham El-Sheltawi (START)
*C100591,1 Summition the total for the reprinted check for all the stub check
*C100591,1 that belongs to the same master check and printed before the check
*C100591,1 that we are printing
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
*C100591,1 (END)
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

*B602081,1 AMM Display the session number in the title bar of the window.
*lcCurWind = WOUTPUT()
*ACTIVATE WINDOW GWDGRIDSET
*@ IIF(_DOS,1,2),IIF(_DOS,50,55) SAY 'Session : ' + lcSession 
*ACTIVATE WINDOW (lcCurWind)
PRIVATE lcTitle
lcTitle = PROPER(ALLTRIM(WTITLE('GWDGRID')))
IF !('Session : ' $ lcTitle)
  MODIFY WINDOW GWDGRID TITLE lcTitle +PADL('Session : ' + lcSession,WCOLS())
ENDIF
*B602081,1 AMM end

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



*E300643,1 Change this lines for the changes we have made to SYCCODES [Begin]
*IF !USED('SYCCODES')
*  USE (gcSysHome+'SYCCODES') IN 0
IF !USED('CODES')
  USE (gcDataDir + 'CODES') IN 0
*E300643,1 Change this lines for the changes we have made to SYCCODES [End]

ENDIF
lcPrintMod = lcRpChkMod
lcOldMode = lcRpChkMod
STORE '' TO lcRpChkFrm,lcRpChkTo

*B609103,1 HES 12/03/2009 Standard AP Printed Check form for MBI customers [Start]
*!*	lcRepForm = IIF(lcRpName='1','APCHKPDD',IIF(_DOS,'APCHKPDL','APCHKPWL'))
*B609103,1 HES 12/03/2009 Standard AP Printed Check form for MBI customers [End]

*E300406,1 Hesham El-Sheltawi (Start)
*lcSetCom=SET('COMP')
*SET COMP ON
DIME laFrxFiles[1,4]
STORE '' TO laFrxFiles

*B609201,1 04/07/2010 WAM Run custom checks under local drive for SaaS environment
*=ADIR(laFrxFiles,gcRepHome+'*.FRX')

IF GCMULTIINST
  =ADIR(laFrxFiles,gcCRepHome+'*.FRX')
ELSE
  =ADIR(laFrxFiles,gcRepHome+'*.FRX')
ENDIF
*B609201,1 04/07/2010 WAM (End)

IF !EMPTY(laFrxFiles[1,1])
  IF ASCAN(laFrxFiles,STUFF(lcRepForm,1,4,'__'+gcAct_comp)+'.FRX')>0 
    
    *B602731,1 Change this line for we don't need the full path any more
    *          because the system will automatically check the reports
    *          root - because of the shared reports - and to prevent the
    *          problem "Invalid path or file name" which happens when the
    *          option grid attempts to check the report platform [Begin]
    *lcRepForm=gcRepHome+STUFF(lcRepForm,1,4,'__'+gcAct_comp)
    lcRepForm = STUFF(lcRepForm , 1 , 4 , '__' + gcAct_comp)
    *B602731,1 Change this line for we don't need the full path [End]
    
  ELSE  
    IF ASCAN(laFrxFiles,'__'+substr(lcRepForm,3)+'.FRX')>0 
      
      *B602731,1 Change this line for we don't need the full path any more
      *          because the system will automatically check the reports
      *          root - because of the shared reports - and to prevent the
      *          problem "Invalid path or file name" which happens when the
      *          option grid attempts to check the report platform [Begin]
      *lcRepForm=gcRepHome+'__'+substr(lcRepForm,3)
      lcRepForm = '__' + SUBSTR(lcRepForm , 3)
      *B602731,1 Change this line for we don't need the full path [End]
      
    ENDIF
  ENDIF
ENDIF  
*SET COMP &lcSetCom
*E300406,1 Hesham El-Sheltawi (End)
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
  *C100591,1 Hesham El-Sheltawi (Start)
  *C100591,1 if the bank code changed then initialize the reprint
  *C100591,1 check range variables
  IF lcPrintMod = 'R' 
    STORE '' TO lcRpChkFrm,lcRpChkTo
  ENDIF
  *C100591,1 Hesham El-Sheltawi (End)    
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
IF '?' $ lcRpBnkCod .OR. !SEEK(lcRpBnkCod)
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
    *C100591,1 Hesham El-Sheltawi (Start)
    *C100591,1 if the bank code changed then initialize the reprint
    *C100591,1 check range variables
    IF lcPrintMod = 'R' 
      STORE '' TO lcRpChkFrm,lcRpChkTo
    ENDIF
    *C100591,1 Hesham El-Sheltawi (End)    
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
  *C100591,1 Hesham El-Sheltawi (Start)
  *C100591,1 if the bank code changed then initialize the reprint
  *C100591,1 check range variables
  IF lcPrintMod = 'R' 
    STORE '' TO lcRpChkFrm,lcRpChkTo
  ENDIF
  *C100591,1 Hesham El-Sheltawi (End)      
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
    *C100591,1 Hesham El-Sheltawi (Start)
    *C100591,1 if the bank code changed then initialize the reprint
    *C100591,1 check range variables
    IF lcPrintMod = 'R' 
      STORE '' TO lcRpChkFrm,lcRpChkTo
    ENDIF
    *C100591,1 Hesham El-Sheltawi (End)      
    lcNxtChkNo = ''
    lcRpGlAcct = ''
  ELSE
    lcNxtChkNo = PADL(APCHECKS.NChkNxtPn,8,'0')
    lcRpGlAcct = APCHECKS.CCHKGLACC
    *C100591,1 Hesham El-Sheltawi (Start)
    *C100591,1 if the bank code changed then initialize the reprint
    *C100591,1 check range variables
    IF lcPrintMod = 'R' 
      STORE '' TO lcRpChkFrm,lcRpChkTo
    ENDIF
    *C100591,1 Hesham El-Sheltawi (End)        
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

    *E300643,1 Change this lines for the changes we have made 
    *          to SYCCODES [Begin]
    *lcDivision = SYCCODES.cdiscrep
    *lcAdvDiv   = SYCCODES.cCode_No
    lcDivision = CODES.cdiscrep
    lcAdvDiv   = CODES.cCode_No
    *E300643,1 Change this lines [End]
    
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
  *C100591,1 (Start) Enable the Check Account and the Bank code objects
  *C100591,1 by remarking the initialization of the array
  *C100591,1 laOGObjCnt[8],laOGObjCnt[9]
*    laOGObjCnt[8 ] = .F.     
*    laOGObjCnt[9 ] = .F.     
  *C100591,1 (End)
*    laOGObjCnt[9] = .F.         
  
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
*C100591,1 Hesham (Start)
*C100591,1 change the checking on the from to variables
*  CASE lcPrintMod = 'R' .AND. EMPTY(lcRePrnChk)   
  CASE lcPrintMod = 'R' .AND. (EMPTY(lcRpChkFrm) OR EMPTY(lcRpChkTo))
*C100591,1 Hesham (End)  
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
*B601411,1 Replace [Using gfRemit instead of lfRemit] (Bigin) 
*lcInvRemit = lfRemit(lcInvRemit, .T. , lcRpVenCod, lcFactor,;
                @lcRemitStat, 'lcRem1', 'lcRem2', 'lcRem3', 'lcRem4',;
                2, 40, 'laRemitTo', @lcRemitTo, lnRemitLen)
lcInvRemit = gfRemit(lcInvRemit, .T. , lcRpVenCod, lcFactor,;
                @lcRemitStat, 'lcRem1', 'lcRem2', 'lcRem3', 'lcRem4', 'lcRem5', 'lcRem6',;
                2, 40, 'laRemitTo', @lcRemitTo, lnRemitLen)
*B601411,1 Replace  (End)
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
    *B601411,1 Replace [Using gfGetAdr instead of lfGetAdr] (Bigin)
    *lcRem2      = SYCFACT.cAddress1
    *lcRem3      = SYCFACT.cAddress2
    *lcRem4      = lfGetAdr('SYCFACT', 'CFACCODE')
    lcRem2      = gfGetAdr('SYCFACT', 'CFACCODE',.F.,.F.,1)
    lcRem3      = gfGetAdr('SYCFACT', 'CFACCODE',.F.,.F.,2)
    lcRem4      = gfGetAdr('SYCFACT', 'CFACCODE',.F.,.F.,3)
    *B601411,1 Replace  (End)
    *B601411,1 Add [Using gfGetAdr to get addres lines 4 and 5] (Bigin)
    lcRem5      = gfGetAdr('SYCFACT', 'CFACCODE',.F.,.F.,4)     
    lcRem6      = gfGetAdr('SYCFACT', 'CFACCODE',.F.,.F.,5)
    *B601411,1 Add (End)
  ENDIF  
ELSE
  *B601411,1 Replace [Adding the Varibles lcRem5,lcRem6] (Bigin)
  *STORE SPACE(40) TO lcRem1, lcRem2, lcRem3, lcRem4
  STORE SPACE(40) TO lcRem1, lcRem2, lcRem3, lcRem4, lcRem5, lcRem6
  *B601411,1 Replace (End)
ENDIF   
SHOW GET lcRem1
SHOW GET lcRem2 
SHOW GET lcRem3 
SHOW GET lcRem4 
*B601411,1 Add [Adding the Varibles lcRem5,lcRem6] (Bigin)
SHOW GET lcRem5 
SHOW GET lcRem6 
*B601411,1 Add (End)
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

*B800058,1 Increase the dim. of the laFieldstr array by one to add 
*B800058,1 the cPageNo field structure

*DIMENSION laFieldStr(lnFieldNo+2,4)

DIMENSION laFieldStr(lnFieldNo+3,4)
laFieldStr[lnFieldNo+1,1] = 'CADDRESS'
laFieldStr[lnFieldNo+1,2] = 'C'
laFieldStr[lnFieldNo+1,3] = 240
laFieldStr[lnFieldNo+1,4] = 0

laFieldStr[lnFieldNo+2,1] = 'nNoOfInv'
laFieldStr[lnFieldNo+2,2] = 'N'
laFieldStr[lnFieldNo+2,3] = 6
laFieldStr[lnFieldNo+2,4] = 0

*B800058,1 Add cPageNo field structure
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

*B800411,1 M.H 12/25/95  Unlock the APBANKS record after the preview.
*IF llPrintChk
*B800411,1 M.H End.
  SELECT APBANKS
  =SEEK(lcRpBnkCod)
  =gfObj_Lock(.F.)
*B800411,1 M.H 12/25/95  Unlock the APBANKS record after the preview.
*ENDIF
*B800411,1 M.H 12/25/95 End.

*!**************************************************************************
*!
*!      Function lfAdvPay
*!
*!**************************************************************************
* 
FUNCTION lfAdvPay   
SELECT (lcRpTargt)

APPEND BLANK
*B602081,1 AMM Update the field nInvFaap with the advanced amount paid.
*REPLACE CVENDCODE   WITH lcRpVenCod  ;
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
        nNoOfInv    WITH 0
        
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
*B602081,1 AMM  end
     
IF llPrintChk
  SELECT APINVHDR
  APPEND BLANK
*B600970,1 M.H 02/06/96 Update the last check #,last payment date fields.
*  REPLACE CVENDCODE   WITH lcRpVenCod  ;
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
           CVENPRIOR   WITH APVENDOR.CVENPRIOR

*C100728,1 M.H Begin.
*  REPLACE CVENDCODE   WITH lcRpVenCod  ;
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
          CCHKNO      WITH lcNxtChkNo

  IF TYPE('APINVHDR.DPOSTDATE') = 'D'

    *B601526,1 Add the new field (nInvFAAp) [Begin]
    *REPLACE CVENDCODE   WITH lcRpVenCod  ;
    *        CINVNO      WITH lcDebMemN   ;
    *        CDIVISION   WITH lcAdvDiv    ;
    *        DINVDATE    WITH ldChkDat    ;
    *        CINVREF     WITH lcRef       ;
    *        CINVREMIT   WITH lcInvRemit  ;
    *        CFACCODE    WITH lcFactor    ;
    *        COUTCOMP    WITH lcRem1      ;
    *        COUTADDR1   WITH lcRem2      ;
    *        COUTADDR2   WITH lcRem3      ;
    *        COUTADDR3   WITH lcRem4      ;
    *        COUTADDR4   WITH lcRem5      ;  
    *        COUTADDR5   WITH lcRem6      ;                
    *        NINVAMNT    WITH 0 - lnPaymnt;
    *        NINVDISOF   WITH 0           ;
    *        NINVAMTAP   WITH 0           ;
    *        NINVDISAP   WITH 0           ;
    *        NINVADJAP   WITH 0           ;
    *        NINVPAID    WITH 0           ;
    *        NINVDISTK   WITH 0           ;
    *        NINVADJ     WITH 0           ;
    *        NINVA1099   WITH 0           ;
    *        CVENPMETH   WITH 'P'         ;
    *        CTERMCODE   WITH ''          ;
    *        NTERDUED    WITH 0           ;
    *        NTERDISCD   WITH 0           ;
    *        NTERDISCR   WITH 0           ;
    *        DINVDUDAT   WITH ldChkDat    ;
    *        CBNKCODE    WITH ''          ;
    *        CCHKACCT    WITH ''          ;
    *        CCHKGLACC   WITH ''          ;
    *        CCHKNO      WITH ''          ;
    *        DCHKDATE    WITH {}          ;
    *        CINVSTAT    WITH 'A'         ;
    *        CVENCCVEN   WITH ''          ;
    *        CVENCCINV   WITH ''          ;
    *        CAPACCT     WITH lcApAcct    ;
    *        CFISFYEAR   WITH lcFisYear   ;
    *        CFSPPRDID   WITH lcFisPrd    ;
    *        CVENPRIOR   WITH APVENDOR.CVENPRIOR ;
    *        DCHKDATE    WITH ldChkDat    ;
    *        CCHKNO      WITH lcNxtChkNo  ;
    *        DPOSTDATE   WITH ldChkDat

*B802573,1 AKA [Start]

    *REPLACE CVENDCODE   WITH lcRpVenCod  ;
    *        CINVNO      WITH lcDebMemN   ;
    *        CDIVISION   WITH lcAdvDiv    ;
    *        DINVDATE    WITH ldChkDat    ;
    *        CINVREF     WITH lcRef       ;
    *        CINVREMIT   WITH lcInvRemit  ;
    *        CFACCODE    WITH lcFactor    ;
    *        COUTCOMP    WITH lcRem1      ;
    *        COUTADDR1   WITH lcRem2      ;
    *        COUTADDR2   WITH lcRem3      ;
    *        COUTADDR3   WITH lcRem4      ;
    *        COUTADDR4   WITH lcRem5      ;  
    *        COUTADDR5   WITH lcRem6      ;                
    *        NINVAMNT    WITH 0 - lnPaymnt;
    *        NINVDISOF   WITH 0           ;
    *        NINVAMTAP   WITH 0           ;
    *        NINVDISAP   WITH 0           ;
    *        NINVADJAP   WITH 0           ;
    *        NINVPAID    WITH 0           ;
    *        NINVDISTK   WITH 0           ;
    *        NINVADJ     WITH 0           ;
    *        NINVA1099   WITH 0           ;
    *        CVENPMETH   WITH 'P'         ;
    *        CTERMCODE   WITH ''          ;
    *        NTERDUED    WITH 0           ;
    *        NTERDISCD   WITH 0           ;
    *        NTERDISCR   WITH 0           ;
    *        DINVDUDAT   WITH ldChkDat    ;
    *        CBNKCODE    WITH ''          ;
    *        CCHKACCT    WITH ''          ;
    *        CCHKGLACC   WITH ''          ;
    *        CCHKNO      WITH ''          ;
    *        DCHKDATE    WITH {}          ;
    *        CINVSTAT    WITH 'A'         ;
    *        CVENCCVEN   WITH ''          ;
    *        CVENCCINV   WITH ''          ;
    *        CAPACCT     WITH lcApAcct    ;
    *        CFISFYEAR   WITH lcFisYear   ;
    *        CFSPPRDID   WITH lcFisPrd    ;
    *        CVENPRIOR   WITH APVENDOR.CVENPRIOR ;
    *        DCHKDATE    WITH ldChkDat    ;
    *        CCHKNO      WITH lcNxtChkNo  ;
    *        DPOSTDATE   WITH ldChkDat    ;
    *        nInvFAAp    WITH 0   
    *B601526,1 Add the new field (nInvFAAp) [End]
    
* The last three columns added by amin [cCurrCode,nExRate ,nCurrUnit]

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
            

*B802573,1 AKA [End]



  ELSE

    *B601526,1 Add the new field (nInvFAAp) [Begin]
    *REPLACE CVENDCODE   WITH lcRpVenCod  ;
    *        CINVNO      WITH lcDebMemN   ;
    *        CDIVISION   WITH lcAdvDiv    ;
    *        DINVDATE    WITH ldChkDat    ;
    *        CINVREF     WITH lcRef       ;
    *        CINVREMIT   WITH lcInvRemit  ;
    *        CFACCODE    WITH lcFactor    ;
    *        COUTCOMP    WITH lcRem1      ;
    *        COUTADDR1   WITH lcRem2      ;
    *        COUTADDR2   WITH lcRem3      ;
    *        COUTADDR3   WITH lcRem4      ;
    *        COUTADDR4   WITH lcRem5      ;  
    *        COUTADDR5   WITH lcRem6      ;                
    *        NINVAMNT    WITH 0 - lnPaymnt;
    *        NINVDISOF   WITH 0           ;
    *        NINVAMTAP   WITH 0           ;
    *        NINVDISAP   WITH 0           ;
    *        NINVADJAP   WITH 0           ;
    *        NINVPAID    WITH 0           ;
    *        NINVDISTK   WITH 0           ;
    *        NINVADJ     WITH 0           ;
    *        NINVA1099   WITH 0           ;
    *        CVENPMETH   WITH 'P'         ;
    *        CTERMCODE   WITH ''          ;
    *        NTERDUED    WITH 0           ;
    *        NTERDISCD   WITH 0           ;
    *        NTERDISCR   WITH 0           ;
    *        DINVDUDAT   WITH ldChkDat    ;
    *        CBNKCODE    WITH ''          ;
    *        CCHKACCT    WITH ''          ;
    *        CCHKGLACC   WITH ''          ;
    *        CCHKNO      WITH ''          ;
    *        DCHKDATE    WITH {}          ;
    *        CINVSTAT    WITH 'A'         ;
    *        CVENCCVEN   WITH ''          ;
    *        CVENCCINV   WITH ''          ;
    *        CAPACCT     WITH lcApAcct    ;
    *        CFISFYEAR   WITH lcFisYear   ;
    *        CFSPPRDID   WITH lcFisPrd    ;
    *        CVENPRIOR   WITH APVENDOR.CVENPRIOR ;
    *        DCHKDATE    WITH ldChkDat    ;
    *        CCHKNO      WITH lcNxtChkNo

    *B802573,1 AKA [Start]    
    *REPLACE CVENDCODE   WITH lcRpVenCod  ;
    *        CINVNO      WITH lcDebMemN   ;
    *        CDIVISION   WITH lcAdvDiv    ;
    *        DINVDATE    WITH ldChkDat    ;
    *        CINVREF     WITH lcRef       ;
    *        CINVREMIT   WITH lcInvRemit  ;
    *        CFACCODE    WITH lcFactor    ;
    *        COUTCOMP    WITH lcRem1      ;
    *        COUTADDR1   WITH lcRem2      ;
    *        COUTADDR2   WITH lcRem3      ;
    *        COUTADDR3   WITH lcRem4      ;
    *        COUTADDR4   WITH lcRem5      ;  
    *        COUTADDR5   WITH lcRem6      ;                
    *        NINVAMNT    WITH 0 - lnPaymnt;
    *        NINVDISOF   WITH 0           ;
    *        NINVAMTAP   WITH 0           ;
    *        NINVDISAP   WITH 0           ;
    *        NINVADJAP   WITH 0           ;
    *        NINVPAID    WITH 0           ;
    *        NINVDISTK   WITH 0           ;
    *        NINVADJ     WITH 0           ;
    *        NINVA1099   WITH 0           ;
    *        CVENPMETH   WITH 'P'         ;
    *        CTERMCODE   WITH ''          ;
    *        NTERDUED    WITH 0           ;
    *        NTERDISCD   WITH 0           ;
    *        NTERDISCR   WITH 0           ;
    *        DINVDUDAT   WITH ldChkDat    ;
    *        CBNKCODE    WITH ''          ;
    *        CCHKACCT    WITH ''          ;
    *        CCHKGLACC   WITH ''          ;
    *        CCHKNO      WITH ''          ;
    *        DCHKDATE    WITH {}          ;
    *        CINVSTAT    WITH 'A'         ;
    *        CVENCCVEN   WITH ''          ;
    *        CVENCCINV   WITH ''          ;
    *        CAPACCT     WITH lcApAcct    ;
    *        CFISFYEAR   WITH lcFisYear   ;
    *        CFSPPRDID   WITH lcFisPrd    ;
    *        CVENPRIOR   WITH APVENDOR.CVENPRIOR ;
    *        DCHKDATE    WITH ldChkDat    ;
    *        CCHKNO      WITH lcNxtChkNo  ;
    *        nInvFAAp    WITH 0   
    *B601526,1 Add the new field (nInvFAAp) [End]
    
    * The last three columns added by amin [cCurrCode,nExRate ,nCurrUnit]
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
            
            
   *B802573,1 AKA [End]



*C100728,1 M.H End.
  ENDIF
*B600970,1 M.H End.

    =gfAdd_Info()

  SELECT APPAYMNT
  APPEND BLANK
  
  *B802573,1 AKA [Start]  
  
  *REPLACE CPAYTYPE  WITH 'P'         ;
  *        CPAYDOCNO WITH lcNxtChkNo  ;
  *        CPAYMETH  WITH 'P'         ;
  *        CPAYSTAT  WITH ''          ;
  *        DPAYDATE  WITH ldChkDat    ;
  *        CFISFYEAR WITH lcFisYear   ;
  *        CFSPPRDID WITH lcFisPrd    ;
  *        DPAYVDATE WITH {}          ;
  *        CPAYCLNO  WITH lcRpVenCode ;
  *        CPAYCOMP  WITH lcRem1      ;
  *        NPAYAMNT  WITH lnPaymnt    ;
  *        NPAYDISC  WITH 0           ;
  *        NPAYADJ   WITH 0           ;
  *        LPAYADVAN WITH .T.         ;
  *        NINV1099A WITH ln1099Amnt  ;
  *        CBNKCODE  WITH lcRpBnkCod  ;
  *        CPAYRECST WITH 'O'         ;        
  *        CCHKACCT  WITH lcRpChkAct
  * The last three columns added by amin [cCurrCode,nExRate ,nCurrUnit]            
  
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
   *B802573,1 AKA [End]

          
    =gfAdd_Info()

  REPLACE APCHECKS.DCHKLPDAT WITH ldChkDat ;
          APCHECKS.NCHKLPAMT WITH lnPaymnt

  *B600710,1 MAN 10/02/95 Update the next check number
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
  *B800415,1 Solve the problem  of Varible lnStubChkNo not found in the advanced payment
  *PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0')
  *B802573,1 AKA [Start]  
  *REPLACE CVENDCODE   WITH lcRpVenCod ;
  *        CINVNO      WITH lcDebMemN  ;
  *        CAPDTRTYP   WITH 'P'        ;
  *        DAPDTRDAT   WITH ldChkDat   ;
  *        LAPDPOST    WITH .F.        ;
  *        CAPDSTAT    WITH ''         ;
  *        CAPDREF     WITH lcNxtChkNo ;
  *        CSTUBCHK    WITH lcNxtChkNo ;
  *        CAPDGLACT   WITH lcApAcct   ;
  *        NAPDAMNT    WITH lnPaymnt   ;
  *        CAPDACTID   WITH 'A'        ;
  *        CBATCHNO    WITH ''         ;
  *        CTRNSLEDN   WITH ''         ;
  *        CFISFYEAR   WITH lcFisYear  ;
  *        CFSPPRDID   WITH lcFisPrd   ;
  *        CAPSESSNO   WITH lcSession  ;
  *        CTAXCODE    WITH ''         ;
  *        CBNKCODE    WITH lcRpBnkCod ;
  *        CCHKACCT    WITH lcRpChkAct ;
  *        NAPDLINNO   WITH 0
  
  * The last three columns added by amin [cCurrCode,nExRate ,nCurrUnit,nEqvAmnt]  
  
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
  *B802573,1 AKA [End]
  
  =gfAdd_Info()
  *B800415,1 Solve the problem  of Varible lnStubChkNo not found in the advanced payment
  *PADL(lnStubChkNo + CEILING(lnStubCnt/lnRpStub),8,'0') 
  APPEND BLANK
  
  *B802573,1 AKA [Start]    
  *REPLACE CVENDCODE   WITH lcRpVenCod   ;
  *        CINVNO      WITH lcDebMemN    ;
  *        CAPDTRTYP   WITH 'P'          ;
  *        DAPDTRDAT   WITH ldChkDat     ;
  *        LAPDPOST    WITH .F.          ;
  *        CAPDSTAT    WITH ''           ;
  *        CAPDREF     WITH lcNxtChkNo   ;
  *        CSTUBCHK    WITH lcNxtChkNo   ;
  *        CAPDGLACT   WITH lcRpGlAcct   ;
  *        NAPDAMNT    WITH 0 - lnPaymnt ;
  *        CAPDACTID   WITH 'C'          ;
  *        CBATCHNO    WITH ''           ;
  *        CTRNSLEDN   WITH ''           ;
  *        CFISFYEAR   WITH lcFisYear    ;
  *        CFSPPRDID   WITH lcFisPrd     ;
  *        CAPSESSNO   WITH lcSession    ;
  *        CTAXCODE    WITH ''           ;
  *        CBNKCODE    WITH lcRpBnkCod   ;
  *        CCHKACCT    WITH lcRpChkAct   ;
  *        NAPDLINNO   WITH 1 
  
  * The last three columns added by amin [cCurrCode,nExRate ,nCurrUnit,nEqvAmnt]
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
  *B802573,1 AKA [End]		  
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
*B601411,1 Add [Adding the Varibles lcRem5,lcRem6] (Bigin)
lcRem5     = SPACE(40)        && Address 5
lcRem6     = SPACE(40)        && Address 6
*B601411,1 Add (End)
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

*B601411,1 Replace [Using gfGetAdr instead of lfGetAdr] (Bigin)
*lcRem2       = APVENDOR.cAddress1
*lcRem3       = APVENDOR.cAddress2
*lcRem4       = lfGetAdr('APVENDOR', 'VENCODE',lcRpVenCod,APVENDOR.cCont_Code)
lcRem2       = gfGetAdr('APVENDOR', 'VENCODE',lcRpVenCod,APVENDOR.cCont_Code,1)
lcRem3       = gfGetAdr('APVENDOR', 'VENCODE',lcRpVenCod,APVENDOR.cCont_Code,2)
lcRem4       = gfGetAdr('APVENDOR', 'VENCODE',lcRpVenCod,APVENDOR.cCont_Code,3)
*B601411,1 Replace (End)

*B601411,1 Add [Using gfGetAdr to get address lines 4,5] (Bigin)
lcRem5       = gfGetAdr('APVENDOR', 'VENCODE',lcRpVenCod,APVENDOR.cCont_Code,4)
lcRem6       = gfGetAdr('APVENDOR', 'VENCODE',lcRpVenCod,APVENDOR.cCont_Code,5)
*B601411,1 Add (End)

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

*E300643,1 Remove the SET FILTER to be befor the SET ORDER and make some
*          changes to the SET FILTER statment to use the Rushmore [Begin] 
*SET ORDER TO TAG CODES
*SET FILTER TO (CCOMP_ID+CRLTFIELD+CFLD_NAME = gcAct_Comp+'N'+'CDIVISION') OR cFld_Name ='N/A'
*E300789,4  AMM Adjust to fit the new structure
*SET FILTER TO (CCOMP_ID+CRLTFIELD+CFLD_NAME = gcAct_Comp + 'N' + 'CDIVISION');
              .OR. (CCOMP_ID+CRLTFIELD+CFLD_NAME = gcAct_Comp + 'N' + 'N/A')
SET FILTER TO (CDefCode+CRLTFIELD+CFLD_NAME = 'N'+'N' + 'CDIVISION');
              .OR. (cDefCode+CRLTFIELD+CFLD_NAME = 'N'+'N' + 'N/A')
*E300789,4  AMM end
LOCATE
SET ORDER TO TAG CODES
*E300643,1 Remove the SET FILTER to be befor the SET ORDER [End]

IF _WINDOWS

  *E300643,1 Change this line for the changes we have made 
  *          to SYCCODES [Begin]
  *DEFINE POPUP puDivision prompt field SYCCODES.cdiscrep scroll;
  *FROM 5.90,12.90 TO 10.20,44.81;
  *MESSAGE gfObj_msg()
  DEFINE POPUP puDivision prompt field CODES.cdiscrep scroll;
  FROM 5.90,12.90 TO 10.20,44.81;
  MESSAGE gfObj_msg()
  *E300643,1 Change this line [End]
  
  ON SELECTION POPUP puDivision DO lfvDivision
ENDIF  

*E300643,1 Change this line for the changes we have made to (gfCodDes) [Begin]
*lcDivision = gfCodDes(APVENDOR.cDivision)
lcDivision = gfCodDes(APVENDOR.cDivision , 'CDIVISION')
*E300643,1 Change this line for the changes we have made to (gfCodDes) [End]

*E300643,1 Change this line for the changes we have made to the Index
*          tag CODES [Begin]
*IF SEEK(gcAct_Comp + APVENDOR.cDivision)
*E300789,4  AMM Adjust to fit the new index
*IF SEEK(gcAct_Comp + APVENDOR.cDivision + 'N' + 'CDIVISION')
IF SEEK('N'+APVENDOR.cDivision + 'N' + 'CDIVISION')
*E300789,4  AMM end
*E300643,1 Change this line [End]

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

*E300683,6 Call .SPR from the REPORTS directory
*DO APCKADVP.SPR   && Run the advance payment screen 
DO (gcRepHome + gcAct_Appl + '\APCKADVP.SPR')   && Run the advance payment screen 
*E300683,6 end

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT SYCCODES
SELECT CODES
*E300643,1 Change this line for the changes we have made to SYCCODES [End]

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

lcRepForm = IIF(lcRpName ='1','APCHKPDD',IIF(_DOS,'APCHKPDL','APCHKPWL'))

*E300406,1 Hesham El-Sheltawi (Start)
*lcSetCom=SET('COMP')
*SET COMP ON
DIME laFrxFiles[1,4]
STORE '' TO laFrxFiles

*B609201,1 04/07/2010 WAM Run custom checks under local drive for SaaS environment
*=ADIR(laFrxFiles,gcRepHome+'*.FRX')

IF GCMULTIINST
  =ADIR(laFrxFiles,gcCRepHome+'*.FRX')
ELSE
  =ADIR(laFrxFiles,gcRepHome+'*.FRX')
ENDIF
*B609201,1 04/07/2010 WAM (End)

IF !EMPTY(laFrxFiles[1,1])
  IF ASCAN(laFrxFiles,STUFF(lcRepForm,1,4,'__'+gcAct_comp)+'.FRX')>0 
    
    *B602731,1 Change this line for we don't need the full path any more
    *          because the system will automatically check the reports
    *          root - because of the shared reports - and to prevent the
    *          problem "Invalid path or file name" which happens when the
    *          option grid attempts to check the report platform [Begin]
    *lcRepForm=gcRepHome+STUFF(lcRepForm,1,4,'__'+gcAct_comp)
    lcRepForm = STUFF(lcRepForm , 1 , 4 , '__' + gcAct_comp)
    *B602731,1 Change this line for we don't need the full path [End]
    
  ELSE  
    IF ASCAN(laFrxFiles,'__'+substr(lcRepForm,3)+'.FRX')>0 
      
      *B602731,1 Change this line for we don't need the full path any more
      *          because the system will automatically check the reports
      *          root - because of the shared reports - and to prevent the
      *          problem "Invalid path or file name" which happens when the
      *          option grid attempts to check the report platform [Begin]
      *lcRepForm=gcRepHome+'__'+substr(lcRepForm,3)
      lcRepForm = '__' + SUBSTR(lcRepForm , 3)
      *B602731,1 Change this line for we don't need the full path [End]
      
    ENDIF
  ENDIF
ENDIF  
*SET COMP &lcSetCom
*E300406,1 Hesham El-Sheltawi (End)
*B600289 MALAK 03/26/95. Change the number of lines per stub in laser and dot matrix form.
lnRpStub  = 11
*lnRpStub = IIF(lcRpName ='1',12,14)
*B602081,1 AMM Adjust the Mode of report (text or graphics)
=lfRepPltFr(lcRepForm)
*B602081,1 AMM end

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
*B600710,1 MAN Start
IF lcRePrnChk == lcOldVal
 RETURN
ELSE
 lcChkToPrn = '' 
ENDIF
*B600710,1 MAN End
 
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
*B600710,1 MAN Changing the browse fields  
*  lcBrFields = "cPayDocNo :H= 'Check Code',"     +;
               "dPayDate  :H= 'Check Date',"     +;
               "cPayClNo  :H= 'Vendor Code',"    +;
               "nPayAdj   :H= 'Adjust. amount'," +;
               "nPayDisc  :H= 'Disc. amount',"   +;
               "nPayAmnt  :H= 'Net amount',"     +;
               "nPayAdj   :H= 'Adjust. amount'," +;
               "cPayAdvan = IIF(lPayAdvan,'Yes','No '):H= 'Adv'," +;
               "cAdd_User :H= 'Printed by'," +;
               "dAdd_Date :H= 'On date'," +;
               "cAdd_Time :H= 'On time'"
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

*  =gfBrows([FOR CPAYTYPE+CPAYMETH='P'+'P'],'cPayDocNo','laTemp')
  =gfBrows([FOR CPAYTYPE+CPAYMETH='P'+'P' .AND. CPAYSTAT <> 'V'],'cPayDocNo','laTemp')
  
  IF EMPTY(laTemp[1])
    lcRePrnChk = lcOldVal
    _CUROBJ    = _CUROBJ
    *B600710,1 MAN Added Return
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

*!**************************************************************************
*!
*!      FUNCTION : lfSelectRec
*!
*!**************************************************************************
* *C100591,1 (Start) ReWrote in function called lfSelectRec
FUNCTION lfSlcRec

lcInvoice = ' '
*B600710,1 MAN if The checks to be reprinted is empty
IF EMPTY(lcChkToPrn)
  lcChkToPrn=lcRePrnChk
ENDIF
*B600710,1 MAN End
  
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
    *B600710,1 MAN 
    *REPLACE CADDRESS WITH UPPER(cOutComp+cOutAddr1+cOutAddr2+cOutAddr3+cOutAddr4+cOutAddr5);
            nNoOfInv WITH 1
    REPLACE CADDRESS WITH UPPER(cOutComp+cOutAddr1+cOutAddr2+cOutAddr3+cOutAddr4+cOutAddr5);
            nNoOfInv WITH CEILING(RECCOUNT()/lnRpStub);
            cPageNo  WITH PADL(CEILING(RECNO()/lnRpStub),4)

    lcInvoice = APDIST.CINVNO
  ENDIF

  SELECT (lcRpTargt)

  DO CASE
    CASE APDIST.CAPDACTID = 'C'

      *B601526,1 Change this line to use the new field (nInvFAAp) [Begin]
      *REPLACE NINVAMTAP    WITH -APDIST.NAPDAMNT 
      REPLACE NINVFAAP     WITH -APDIST.NAPDAMNT 
      *B601526,1 Change this line [End]
      
    CASE APDIST.CAPDACTID = 'B'
      REPLACE NINVA1099    WITH -APDIST.NAPDAMNT

    *B601526,1 Change this line [Begin]
    *          (To get the approved adjustment record and not the calculated one)
    *CASE APDIST.CAPDACTID = 'J'  
    CASE (APDIST.CAPDACTID = 'J' .AND. APDIST.NAPDLINNO <> 1) 
    *B601526,1 Change this line [End]

      REPLACE NINVADJAP    WITH -APDIST.NAPDAMNT
    CASE APDIST.CAPDACTID = 'S'
      REPLACE NINVDISAP    WITH -APDIST.NAPDAMNT
  ENDCASE
  SELECT APDIST
ENDSCAN

SELECT APDIST
SET RELATION TO 

SELECT (lcRpTargt)
*B600710,1 MAN Added the following IF
IF lcRePrnChk $ lcChkToPrn
  REPLACE nNoOfInv  WITH 0
ENDIF  
GO TOP
*B600710,1 MAN 
*REPLACE nNoOfInv WITH CEILING(RECCOUNT()/lnRpStub)
*C100591,1 (End)

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
*B800415,1 Get the lcOldPmnt value before the IF condition
lcOldPmnt = APPAYMNT.cPayDocNo

REPLACE APCHECKS.NCHKNXTPN WITH INT(VAL(lcNxtChkNo))
*B600710,1 MAN  Added the IF  
IF lcRePrnChk $ lcChkToPrn
  *B800415,1 Get the lcOldPmnt value before the IF condition
  lcOldPmnt = APPAYMNT.cPayDocNo
  SCATTER MEMVAR MEMO
  REPLACE CPAYSTAT  WITH 'V'
  =gfAdd_Info()
  APPEND BLANK
  GATHER MEMVAR MEMO
*B600710,1 MAN 
  *REPLACE CPAYDOCNO WITH lcNxtChkNo 
  REPLACE CPAYDOCNO WITH PADL(INT(VAL(lcNxtChkNo)) - 1,8,'0')

  =gfAdd_Info()
          
  REPLACE APCHECKS.DCHKLPDAT WITH APPAYMNT.DPAYDATE ;
          APCHECKS.NCHKLPAMT WITH APPAYMNT.NPAYAMNT

*B800415,1 Stop The replacemnt of the next chk to print 
*B600710,1 MAN 
*  REPLACE APCHECKS.NCHKNXTPN WITH INT(VAL(lcNxtChkNo))

*B600710,1 MAN 
  *REPLACE APVENDOR.DVENLPAYD WITH APPAYMNT.DPAYDATE ;
           APVENDOR.NVENLPAYA WITH APPAYMNT.NPAYAMNT ;
           APVENDOR.CVENLPAYN WITH lcNxtChkNo 
  
  REPLACE APVENDOR.DVENLPAYD WITH APPAYMNT.DPAYDATE ;
          APVENDOR.NVENLPAYA WITH APPAYMNT.NPAYAMNT ;
          APVENDOR.CVENLPAYN WITH PADL(INT(VAL(lcNxtChkNo)) - 1,8,'0')
ENDIF

SELECT APDIST
=SEEK(APPAYMNT.CPAYMETH+APPAYMNT.CBNKCODE+APPAYMNT.CCHKACCT+lcOldPmnt)

*B600710,1 MAN 
*REPLACE cApdRef    WITH lcNxtChkNo ,;
         cApSessNo  WITH lcSession   ;
 ALL FOR cApDtrTyp+cBnkCode+cChkAcct+cApdRef = ;
         APPAYMNT.CPAYMETH+APPAYMNT.CBNKCODE + ;
         APPAYMNT.CCHKACCT+lcOldPmnt

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
*B800101,1 Function to fill the array with data.
FUNCTION lfFooter
*B601091,1 Hesham El-Sheltawi (Start)
*IF &lcRpTargt..cvendcode = lcVendor .AND. lnFooter < lnRpStub .AND. &lcRpTargt..caddress = lcaddress 
IF &lcRpTargt..cvendcode = lcVendor .AND. lnFooter < lnRpStub .AND. &lcRpTargt..caddress = lcaddress AND &lcRpTargt..cPageNo = lcPageNo
*B601091,1 Hesham El-Sheltawi (End)
  *B800415,1 At the bottom stub make sure that the system does not print the invoices from previous chks.
  lcaddress= &lcRpTargt..caddress
  *B601091,1 Hesham El-Sheltawi (Start)  
  lcPageNo = &lcRpTargt..cPageNo  
  *B601091,1 Hesham El-Sheltawi (End)
  lnFooter = lnFooter + 1
  laFooter[lnFooter,1] = &lcRpTargt..cinvno
  laFooter[lnFooter,2] = DTOC(&lcRpTargt..dinvdate)

  *B601526,1 Change this lines [Begin]
  *laFooter[lnFooter,3] = STR(&lcRpTargt..ninvamnt,15,2)
  *laFooter[lnFooter,4] = STR(&lcRpTargt..ninvdisap,10,2)
  *laFooter[lnFooter,5] = STR(&lcRpTargt..ninvadjap,15,2)
  *laFooter[lnFooter,6] = STR(&lcRpTargt..ninvamtap,15,2)
  
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
  *B601526,1 Change this lines [End]
  
ELSE
  lnFooter = 1
  laFooter = ''
  lcVendor = &lcRpTargt..cvendcode
  *B800415,1 At the bottom stub make sure that the system does not print the invoices from previous chks.
  lcaddress = &lcRpTargt..caddress
  *B601091,1 Hesham El-Sheltawi (Start)
  lcPageNo = &lcRpTargt..cPageNo
  *B601091,1 Hesham El-Sheltawi (End)
  laFooter[lnFooter,1] = &lcRpTargt..cinvno
  laFooter[lnFooter,2] = DTOC(&lcRpTargt..dinvdate)

  *B601526,1 Change this lines [Begin]
  *laFooter[lnFooter,3] = STR(&lcRpTargt..ninvamnt,15,2)
  *laFooter[lnFooter,4] = STR(&lcRpTargt..ninvdisap,10,2)
  *laFooter[lnFooter,5] = STR(&lcRpTargt..ninvadjap,15,2)
  *laFooter[lnFooter,6] = STR(&lcRpTargt..ninvamtap,15,2)
  
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
  *B601526,1 Change this lines [End]
  
ENDIF  

*C102793,1 Replace the "Adjustment" field is printed in the check stubs. [Begin]
=gfDoTriger('APCHKPRV',PADR('REFRANCE',10))
*C102793,1 Replace the "Adjustment" field is printed in the check stubs. [End]

RETURN ''


*C100591,1 HESHAM EL-SHELTAWI (START)
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

      *B601526,1 Change this line to use the new field (nInvFAAp) [Begin]
      *REPLACE NINVAMTAP    WITH -APDIST.NAPDAMNT 
      REPLACE NINVFAAP     WITH -APDIST.NAPDAMNT 
      *B601526,1 Change this line [End]
      
    CASE APDIST.CAPDACTID = 'B'
      REPLACE NINVA1099    WITH -APDIST.NAPDAMNT

    *B601526,1 Change this line [End]
    *          (To get the approved adjustment record and not the calculated one)
    *CASE APDIST.CAPDACTID = 'J'
    CASE (APDIST.CAPDACTID = 'J' .AND. APDIST.NAPDLINNO <> 1)
    *B601526,1 Change this line [End]

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
  *B601129,1 Hesham El-Sheltawi (Start)
  *B601129,1 Replace the invoice header check no with the new check number
  *B601129,1 that this invoice is printed with
  IF SEEK(CINVNO+CVENDCODE,'APINVHDR')
    REPLACE APINVHDR.CCHKNO   WITH PADL(INT(VAL(lcNxtChkNo)) - lnNoOfChk - 1 + lnChkNo ,8,'0');
            APINVHDR.DCHKDATE WITH ldChkDat
  ENDIF
  *B601129,1 Hesham El-Sheltawi (End)  
  * if there is any other records within the range of the checks to be reprinted
  * then to to the next one to be updated 
  * Note that we seek for the next value to be updated and skip -1
  * because the Scan will Skip the record pointer after our skip -1
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
*C100591,1 HESHAM EL-SHELTAWI (END)

*B601091,1 Hesham El-Sheltawi (Start)
*B601091,1 add the next function to make the lines
*B601091,1 that hold the VOID sign in the dotmatrix report
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
*B601091,1 Hesham El-Sheltawi (End)

*B601091,1 Hesham El-Sheltawi (Start)
FUNCTION lfNxtChkUpd
IF !EOF(lcRpTargt) AND &lcRpTargt..cPageNo <> lcChkPage
  lcNxtChkNo = PADL(INT(VAL(lcNxtChkNo)) + 1,8,'0')
  lcChkPage = &lcRpTargt..cPageNo 
ENDIF
*B601091,1 Hesham El-Sheltawi (End)

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
*B601526,1 This function was added by HS for the bug
*!*************************************************************
*
FUNCTION lfGetNetSm

*B601555,1 Change this line to add new varibles [Begin]
*PRIVATE lcOldAlias , lcExactSt , lcCurrency , lnCurSEl
PRIVATE lcOldAlias , lcExactSt , lcCurrency , lnCurSEl , llUsed
*B601555,1 Change this line to add new varibles [End]
 
lcOldAlias = ALIAS()
lcExactSt = SET('EXACT')
SET EXACT ON
SELECT APCHECKS

lcCurrency = LOOKUP(cCurrCode , lcRpBnkCod + lcRpChkAct , cBnkCode , 'BANKCHECK')
lnCurSEl = ASCAN(laCurrSmbl , lcCurrency)

*B601555,1 Add this line to get the currency describtion [Begin]
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
*B601555,1 Add this line to get the currency describtion [End]

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
*B601526,1 This function was added by HS for the bug
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

*!*************************************************************
*! Name      : lfDefNxtChk
*! Developer : Ahmed Ibrahim
*! Date      : 03/08/1999
*! Purpose   : Function to Get next check number
*! Reference : *E301077,79 AMM
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
*! Reference : *E301077,79 AMM
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
        *IF ALLTRIM(cLok_User) = ALLTRIM(gcUser_ID)
        *  *!600396,1 Messaging the user that he cannot edit the same record
        *  *!600396,1 from more than one session and permit him from editing
        *  *!600396,1 the same record
        *  IF gfModalGen("INM00240B00006","ALERT")=2
        *    lLok_It    = .F.
        *    lRet_Flag  = .F.
        *  ELSE      
        *    lLok_It    = .T.
        *  ENDIF
        *ELSE  
          lnOldrpSt = SET('REPROCESS')
          SET REPROCESS TO 1

        *B608942,1 TMI 07/20/2009 07:23:22 PM [Start] check if the user recorded in cLok_user field lies in SYUSTATC file of either A27 or A4xp
        *IF SEEK ('INI'+'OLDVARS'+cLok_User,'syuStatc') 
          IF SEEK ('INI'+'OLDVARS'+cLok_User,'syuStatc') .OR. IIF(glRunFrmA4,SEEK('INI'+'OLDVARS'+cLok_User,'A4USTATC'),.F.)
          *B608942,1 TMI 07/20/2009 07:23:29 PM [End  ] 
            **B128400,1[Start]
            *IF RLOCK('syuStatc')
            *  UNLOCK IN  syuStatc 
            *  GO gnMyStRec IN syuStatc 
            *  =RLOCK('syuStatc')
            *  lLok_It    = .T.
            *ELSE
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
            **B128400,1[Start]
            *ENDIF
            **B128400,1[End]
          ELSE
            lLok_It    = .T. 
          ENDIF          

          SET REPROCESS TO  lnOldrpSt

        *ENDIF
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

