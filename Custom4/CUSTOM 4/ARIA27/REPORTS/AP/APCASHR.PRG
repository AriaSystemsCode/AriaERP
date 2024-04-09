*:************************************************************************
*: Report name   : APCASHR.RPR
*: Program desc. : Cash Requirment report.
*:                 This report show the company cash requirments 
*:                 to meet its dues during specified intervals.
*:         System: Accounts payable
*:      Developer: MALAK - Malak Hanna
*:************************************************************************
*: Calls : 
*:         Functions  : lfSummBand
*:                    : lfvRepDate
*:                    : lfvBucket
*:                    : lfvChngRep
*:                    : lfvDayBuck
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: N20,1  06/20/95 Malak Hanna
*: N20,1  Cash Requirment 
*:*************************************************************
*B500790,1 MAN 06/21/95 Changing the numeric fields picture in the FRX
*B500790,1              to print the commas   
*B300296,1 M.H 12/17/95 Add the currency to the report.
*B600491,1 MAN 01/17/95 Fixed the hidden filter to prevent displaying
*B600491,1 MAN          the open invoices.
*B600491,1 MAN          Fixed wrong totals in the multi buckets format.
*B601013,1 RENEE 04/02/96. Correct the spelling of 'Curruncy' to 'Currency'
*B601013,1                 in APCASHR.FRX
*E300400,1 Hesham El-Sheltawi 05/16/96
*E300400,1 add grand totals at then end of the report if the user select
*E300400,1 Multiple buckets = Yes 
*E300400,1 changed made to APCASHR2.FRX
*B802489,1 AKA 08/02/99 This report should not consider invoice with no balance
*B803069,1 SSE 03/06/2000 1) Fix bug of wrong Due Date of first line displayed
*B803069,1                2) Add one Option in OG to display Zero Due Date value or not
*CP101903,1  AMH 06/13/2000 Add Factor Option to Option Grid
*B604005,1 MHM 11/02/00  Fix bug of Numeric overflow in AP reports (modified in reports only)
*B604005,1 MHM           modify fieldopen balance in APCASHR,
*B604338,1 AAN 17/04/01  Resize in width of "Discount Offered" and "Discount Approved" fields. 
*********************************************************************************
SELECT APINVHDR

STORE 0.00 TO lnAmount1,  lnAmount2,  lnAmount3,  lnAmount4, ;
              lnAmount5,  lnAmount6,  lnAmount7,  lnAmount8, ;
              lnAmount9,  lnAmount10, lnAmount11, lnAmount12,;
              lnTotPaid,  lnTotDiscA, lnTotOpenB
llPrnFlag = .F.

*B300296,1 New varibale to hold the old currency.
lcOldCurr = ''
*B300296,1 End.

*B802489,1 AKA (Start)
PRIVATE  lcValidInv
lcValidInv = " (APINVHDR.nInvAmnt - (APINVHDR.nInvPaid+APINVHDR.nInvDisTk+APINVHDR.nInvAdj) <> 0  ) "
lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp)  , '' , ' AND ' ) + lcValidInv
*B802489,1 AKA (End)

IF lcRpForm = 'APCASHR2'   && Check if the report form is 'APCASHR2'
  SET ORDER TO TAG DueDate 
  *** Locate if there is any records matching the selected criteria.
  LOCATE FOR &lcRpExp
  IF !FOUND()
    *** There are no records to display...!
    =gfModalGen("INM00052B00000","DIALOG")
    RETURN
  ENDIF  

  *B803069,1 variable used to display 'Due By' date in FRX 'APCASHR2' [Begin]
  PRIVATE ldDispDate
  ldDispDate = dInvDuDat
  *B803069,1 variable used to display 'Due By' date in FRX 'APCASHR2' [End]

  ** Get the first invoice due date.
  ldTillDate = dInvDuDat + lnRpBuckt1
ELSE
  *lcRpForm = lcRpSortOp
  IF lcRpForm == 'APCASHR'   && Sort option ---> 'Vendor'
    SET ORDER TO TAG VendInv
  ELSE                       && Sort option ---> 'Due date'
    SET ORDER TO TAG DueDate 
  ENDIF
ENDIF
DO gfDispRe WITH EVAL('lcRpForm'),'FOR '+lcRpExp

SELECT APINVHDR
SET ORDER TO TAG DueDate 

*!*************************************************************
*! Name      : lfRepShow
*! Developer : Mohamed Hassan
*! Date      : 12/17/1995
*! Purpose   : This function called from the grid to enable or
*!             disable the currency code.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
*B300296,1 New show function to control the Enable,Disable the
*          currency code field.
FUNCTION lfRepShow

IF lcRpForm = 'APCASHR2'
  laOGFxFlt[1,6]= gcBaseCurr
  laOGObjCnt[4] = gfGetMemVar('LLMULCURR')
  =lfOGShowGet("laOGFxFlt[1,6]")
ELSE
  laOGFxFlt[1,6]= gcBaseCurr
  laOGObjCnt[9] = gfGetMemVar('LLMULCURR')
  =lfOGShowGet("laOGFxFlt[1,6]")
ENDIF

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

*!*************************************************************
*! Name      : lfSummBand
*! Developer : MALAK - Malak Hanna
*! Date      : 06/20/1995
*! Purpose   : This function called in the detail part, to 
*!             calculate totals in every bucket.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfSummBand()
*!*************************************************************
FUNCTION lfSummBand

DO CASE
  CASE dInvDuDat < ldRpRepDat
    lnAmount1 = lnAmount1 + nInvAmnt - nInvPaid - nInvAdj - nInvDisTk 
       
  CASE dInvDuDat < ldRpRepDat + lnRpBuckt1
    lnAmount3 = lnAmount3 + nInvAmnt - nInvPaid - nInvAdj - nInvDisTk 

  CASE dInvDuDat < ldRpRepDat + lnRpBuckt2
    lnAmount5 = lnAmount5 + nInvAmnt - nInvPaid - nInvAdj - nInvDisTk 

  CASE dInvDuDat < ldRpRepDat + lnRpBuckt3
    lnAmount7 = lnAmount7 + nInvAmnt - nInvPaid - nInvAdj - nInvDisTk 

  CASE dInvDuDat < ldRpRepDat + lnRpBuckt4
    lnAmount9  = lnAmount9 + nInvAmnt - nInvPaid - nInvAdj - nInvDisTk 

  OTHERWISE
    lnAmount11 = lnAmount11+ nInvAmnt - nInvPaid - nInvAdj - nInvDisTk 

ENDCASE   

DO CASE
  CASE dInvDate + nTerDiscd < ldRpRepDat
    lnAmount2 = lnAmount2 + MAX(nInvDisOf-nInvDisTk,0)
       
  CASE dInvDate + nTerDiscd < ldRpRepDat + lnRpBuckt1
    lnAmount4 = lnAmount4 + MAX(nInvDisOf - nInvDisTk,0)

  CASE dInvDate + nTerDiscd < ldRpRepDat + lnRpBuckt2
    lnAmount6 = lnAmount6 + MAX(nInvDisOf - nInvDisTk,0)

  CASE dInvDate + nTerDiscd < ldRpRepDat + lnRpBuckt3
    lnAmount8 = lnAmount8 + MAX(nInvDisOf - nInvDisTk,0)

  CASE dInvDate + nTerDiscd < ldRpRepDat + lnRpBuckt4
    lnAmount10 = lnAmount10 + MAX(nInvDisOf - nInvDisTk,0)

  OTHERWISE
    lnAmount12 = lnAmount12 + MAX(nInvDisOf - nInvDisTk,0)
ENDCASE   

RETURN ''

*!*************************************************************
*! Name      : lfvRepDate
*! Developer : MALAK - Malak Hanna
*! Date      : 06/20/1995
*! Purpose   : Function to validate the report date.
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvRepDate()
*!*************************************************************
FUNCTION lfvRepDate

** If report date is empty.
IF EMPTY(ldRpRepDat)
  *** You have to enter the ð.
  =gfModalGen("TRM04066B00000","DIALOG",'report date')
  ldRpRepDat = lcOldVal
  _CUROBJ     = _CUROBJ
ENDIF 

*!*************************************************************
*! Name      : lfvBucket
*! Developer : MALAK - Malak Hanna
*! Date      : 06/20/1995
*! Purpose   : This function called to validate the number 
*!             of days in every buckets. 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvBucket()
*!*************************************************************
FUNCTION lfvBucket

DO CASE
 CASE lnRpBuckt1 <= 0     && Case firest bucket less than or equal zero.
  ** MESSAGE : " ð should be greater than ð."
  **           "           ® Ok ¯           "
  =gfModalGen("TRM04072B00000","DIALOG",'No. of days for the 1st bucket|zero')

 CASE lnRpBuckt1 >= lnRpBuckt2  && Case 1st bucket grater than or equal 2nd bucket.
  ** MESSAGE : " ð should be greater than ð."
  **           "           ® Ok ¯           "
  =gfModalGen("TRM04072B00000","DIALOG",'No. of days for the 2nd bucket|no. of days for the 1st bucket')

 CASE lnRpBuckt2 >= lnRpBuckt3  && Case 2nd bucket grater than or equal 3rd bucket.
  ** MESSAGE : " ð should be greater than ð."
  **           "           ® Ok ¯           "
  =gfModalGen("TRM04072B00000","DIALOG",'No. of days for the 3rd bucket|no. of days for the 2nd bucket')

 CASE lnRpBuckt3 >= lnRpBuckt4  && Case 3rd bucket grater than or equal 4th bucket.
  ** MESSAGE : " ð should be greater than ð."
  **           "           ® Ok ¯           "
  =gfModalGen("TRM04072B00000","DIALOG",'No. of days for the 4th bucket|no. of days for the 3rd bucket')

 OTHERWISE
   RETURN .T. 
ENDCASE 

lcRpVar  = SYS(18)
&lcRpVar = lcOldVal
_CUROBJ  = _CUROBJ

*!*************************************************************
*! Name      : lfvChngRep
*! Developer : MALAK - Malak Hanna
*! Date      : 06/20/1995
*! Purpose   : Check if the report form is changed, And rebuild 
*!             the options grid if changed.
*!*************************************************************
*! Calls     : lfChangeGrid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvChngRep()
*!*************************************************************
FUNCTION lfvChngRep

** Rebuild the options grid only if the report form name is changed.
IF !(lcOldMode == lcRpForm)
  lcOldMode = lcRpForm
  =lfChangeGrid(ALLTR(lcRpForm))
ENDIF

*!*************************************************************
*! Name      : lfvDayBuck
*! Developer : MALAK - Malak Hanna
*! Date      : 06/20/1995
*! Purpose   : Function to validate the number on days in the bucket.
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvDayBuck()
*!*************************************************************
FUNCTION lfvDayBuck

** If the number of days in the bucket is less than or equal zero.
IF lnRpBuckt1 <= 0  
  ** MESSAGE : " ð should be greater than ð."
  **           "           ® Ok ¯           "
  =gfModalGen("TRM04072B00000","DIALOG",'Entered value|zero')
  lnRpBuckt1 = lcOldVal 
  _CUROBJ  = _CUROBJ
ENDIF

*!*************************************************************
*! Name      : lfCheckDate
*! Developer : MALAK - Malak Hanna
*! Date      : 06/20/1995
*! Purpose   : This function called in the detail part, to 
*!             calculate totals in every bucket internval.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfCheckDate()
*!*************************************************************
FUNCTION lfCheckDate

*B600491,1  MAN Start
IF llPrnFlag
  STORE 0.00 TO lnTotPaid,  lnTotDiscA, lnTotOpenB
ENDIF 
*B600491,1  MAN End  

IF dInvDuDat < ldTillDate
  *B500790,1  MAN 06/21/95 Changed the STR Command to TRANSFROM
  *--B604338,1 AAN  Resize in report "Discount Offered" and "Discount Approved" 2 Char. [Begin]
  *lcRetVal = DTOC(dInvDuDat) + ' ' + cVendCode + ' ' + cInvNo + ' '+;
             DTOC(dInvDate)  + ' ' + cVenPrior + ' ' + TRANSFORM(nInvAmnt,'@$ 9999,999,999.99') + ' '+;
             TRANSFORM(nInvDisOf,'@$ 999,999.99') + ' '+ TRANSFORM(nInvPaid + nInvDisTk + nInvAdj,'@$ 9999,999,999.99') +;
             ' ' + TRANSFORM(nInvAmtAp,'@$ 9999,999,999.99') + ' '+ TRANSFORM(nInvDisAp,'@$ 999,999.99') + ' '+TRANSFORM(nInvAmnt - (nInvPaid + nInvDisTk + nInvAdj),'@$ 9999,999,999.99')

  lcRetVal = DTOC(dInvDuDat) + ' ' + cVendCode + ' ' + cInvNo + ' '+;
             DTOC(dInvDate)  + ' ' + cVenPrior + ' ' + TRANSFORM(nInvAmnt,'@$ 9999,999,999.99') + ' '+;
             TRANSFORM(nInvDisOf,'@$ 9999999.99') + ' '+ TRANSFORM(nInvPaid + nInvDisTk + nInvAdj,'@$ 9999,999,999.99') +;
             ' ' + TRANSFORM(nInvAmtAp,'@$ 9999,999,999.99') + ' '+ TRANSFORM(nInvDisAp,'@$ 9999999.99') + ' '+TRANSFORM(nInvAmnt - (nInvPaid + nInvDisTk + nInvAdj),'@$ 9999,999,999.99')
  *--B604338,1 AAN  Resize in report "Discount Offered" and "Discount Approved" 2 Char. [End]

  lnTotPaid  = lnTotPaid  + nInvAmtAp
  lnTotDiscA = lnTotDiscA + nInvDisAp
  lnTotOpenB = lnTotOpenB + nInvAmnt - (nInvPaid + nInvDisTk + nInvAdj)
  llPrnFlag  = .F.
  
  *B803069,1 Flag to detect whether to print balance with Zero Value [Begin]
  llPstAmnt = (ABS(lnTotPaid) + ABS(lnTotDiscA) + ABS(lnTotOpenB)) <> 0 
  *B803069,1 Flag to detect whether to print balance with Zero Value [End]
  
  RETURN lcRetVal 
ELSE
  SKIP -1 IN APINVHDR
  ldTillDate = ldTillDate + lnRpBuckt1

  *B803069,1 Variable accumulated to be displayed in FRX "APCASHR2" [Begin.]
  ldDispDate = ldDispDate + lnRpBuckt1
  llPstAmnt = (ABS(lnTotPaid) + ABS(lnTotDiscA) + ABS(lnTotOpenB)) <> 0
  *B803069,1 Variable accumulated to be displayed in FRX "APCASHR2" [End.]
  
  llPrnFlag  = .T.
 *B600491,1  MAN Moved this statement outside the IF Condition
 *STORE 0.00 TO lnTotPaid,  lnTotDiscA, lnTotOpenB
  RETURN ''
ENDIF

*!**************************************************************************
*! Name      : lfvFactor
*! Developer : Ahmed Maher
*! Date      : 06/13/2000
*! Purpose   : This function is to validate the Factors
*!**************************************************************************
*! Called from : The Option Grid
*!**************************************************************************
*! Example   :  =lfvFactor()
*!**************************************************************************
**CP101903,1  AMH 06/13/2000 Add Factor Option to Option Grid
FUNCTION lfvFactor

PRIVATE lcFactVar
lcFactVar = VARREAD()

IF !SEEK(&lcFactVar.,"SYCFACT")
  &lcFactVar. = lfFactBrow()
ENDIF

*!**************************************************************************
*! Name      : lfFactBrow
*! Developer : Ahmed Maher
*! Date      : 06/13/2000
*! Purpose   : This function is to Browse the Factors
*!**************************************************************************
*! Called from : Validation of Factor
*!**************************************************************************
*! Example   :  =lfFactBrow()
*!**************************************************************************
**CP101903,1  AMH 06/13/2000 Add Factor Option to Option Grid
FUNCTION lfFactBrow
PRIVATE lcFields,lnCurAlias,lcBrFields,lcFile_Ttl
STORE SPACE(0) TO lcFields
PRIVATE laTemp
DIMENSION laTemp[1]
laTemp[1] = ""
lnCurAlias = SELECT(0)

lcFields    = "cFacCode"
lcBrFields  = " cFacCode:H='Factor', cFacComp :H='Factor/Company name' "
lcFile_Ttl  = 'Factors'
          
SELECT SYCFACT
=gfBrows("",lcFields,'laTemp')
SELECT(lnCurAlias)

RETURN  IIF(!EMPTY(ALLTRIM(laTemp[1]))  , laTemp[1] , "")

*!**************************************************************************
*! Name      : lfvSortBy
*! Developer : Ahmed Abdel Naby
*! Date      : 18/04/2001
*! Purpose   : This function is run form option grid with sort by.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   :  =lfvSortBy()
*!**************************************************************************
*B604338,1 AAN 18/04/01  To update option grid.
FUNCTION lfvSortBy

IF lcRpSortOp = "V"   
  * Sort by Vendor
  lcRpForm = "APCASHR"
ELSE
  * Sort by Due date  
  lcRpForm = "APCASHR1"
ENDIF