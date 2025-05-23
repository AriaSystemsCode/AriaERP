*:************************************************************************
*: Report name   : APCASHR.RPR
*: Program desc. : Cash Requirment report.
*:                 This report show the company cash requirments
*:                 to meet its dues during specified intervals.
*:         System: Accounts payable
*:      Developer: Tarek Mohamed Ibrahim
*T20141002.0017,task, adding/correcting the main fix # for this entity object TMI 10/12/2014 08:54 [Start] 
*:                 E606069,1 && ignore this line
*:                 E303069,1 
*T20141002.0017,task, adding/correcting the main fix # for this entity object TMI 10/12/2014 08:54 [End  ] 
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
*!* Modifications;
*B610057,1 SAB 08/25/2012 Fix Cash Requirment Open Balance [T20120731.0009]
*:*************************************************************
*N000682,1 MMT 02/05/2013 Globalization changes[Start]
#INCLUDE r:\aria4xp\reports\ap\apcashr.h
*N000682,1 MMT 02/05/2013 Globalization changes[End]
SELECT APINVHDR

STORE 0.00 TO lnAmount1,  lnAmount2,  lnAmount3,  lnAmount4, lnAmount5,  lnAmount6,  lnAmount7,  lnAmount8, ;
              lnAmount9,  lnAmount10, lnAmount11, lnAmount12, lnTotPaid,  lnTotDiscA, lnTotOpenB
llPrnFlag = .F.

lcOldCurr = ''

*B610057,1 SAB 08/25/2012 Fix Cash Requirment Open Balance [Start]
STORE '' TO lcAPINVHDR
*B610057,1 SAB 08/25/2012 Fix Cash Requirment Open Balance [End]

PRIVATE  lcValidInv
lcValidInv = " (APINVHDR.nInvAmnt - (APINVHDR.nInvPaid+APINVHDR.nInvDisTk+APINVHDR.nInvAdj) <> 0  ) "
lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp)  , '' , ' AND ' ) + lcValidInv

IF lcRpForm = 'APCASHR2'   && Check if the report form is 'APCASHR2'
  SET ORDER TO TAG DueDate
  *** Locate if there is any records matching the selected criteria.
  LOCATE FOR &lcRpExp
  IF !FOUND()
    *** There are no records to display...!
    =gfModalGen("INM00052B00000","DIALOG")
    RETURN
  ENDIF

  *B610057,1 SAB 08/25/2012 Fix Cash Requirment Open Balance [Start]
  *PRIVATE ldDispDate
  *ldDispDate = dInvDuDat

  *** Get the first invoice due date.
  *ldTillDate = dInvDuDat + lnRpBuckt1

  ldTillDate = dInvDuDat + lnRpBuckt1
  =lfCollData()
  SELECT (lcAPINVHDR)
  lcRpExp = '.T.'
  *B610057,1 SAB 08/25/2012 Fix Cash Requirment Open Balance [End]
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

*!*****************************************************************************************
*! Name      : lfRepShow
*! Developer : Saber A.Razek (SAB)
*! Date      : 04/26/2012
*! Purpose   : This function called from the grid to enable or disable the currency code.
*!*****************************************************************************************
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

*!*****************************************************************************************
*! Name      : lfwCurCode
*! Developer : Saber A.Razek (SAB)
*! Date      : 04/26/2012
*! Purpose   : This function called from the currency field to validate the currency.
*!*****************************************************************************************
*New when function to save the old currency code.
FUNCTION lfwCurCode

lcOldCurr = laOGFxFlt[1,6]

*!*****************************************************************************************
*! Name      : lfvCurCode
*! Developer : Saber A.Razek (SAB)
*! Date      : 04/26/2012
*! Purpose   : This function called from the currency field to validate the currency.
*!*****************************************************************************************
*New valid function to validate the currency code.
FUNCTION lfvCurCode

IF !SEEK(laOGFxFlt[1,6],'SYCCURR') .OR. ATC("?",laOGFxFlt[1,6]) > 0
  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp     = ''
  lcSavBrFld = lcBrFields
  lcSavTitle = lcFile_Ttl

  *N000682 ,1 Thabet Handle globalization issues [Start]
  *lcFile_Ttl = "Currency"
  *lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
               "CCURRDESC :R :H= 'Description',  " +;
               "CCURRSMBL :R :H= 'Symbol'"
  lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CURRENCY,oAriaApplication.GetHeaderText("LANG_CURRENCY",AHEADERFILE))
  lcBrFields = "CCURRCODE :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CURRENCY_CODE,oAriaApplication.GetHeaderText("LANG_CURRENCY_CODE",AHEADERFILE))+"'," +;
               "CCURRDESC :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DESCRIPTION,oAriaApplication.GetHeaderText("LANG_DESCRIPTION",AHEADERFILE))+"',  " +;
               "CCURRSMBL :R :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SYMBOL,oAriaApplication.GetHeaderText("LANG_SYMBOL",AHEADERFILE))+"'"
  *N000682 ,1 Thabet Handle globalization issues [END]
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

*!*****************************************************************************************
*! Name      : lfSummBand
*! Developer : Saber A.Razek (SAB)
*! Date      : 04/26/2012
*! Purpose   : This function called in the detail part, to calculate totals in every bucket.
*!*****************************************************************************************
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

*!*****************************************************************************************
*! Name      : lfvRepDate
*! Developer : Saber A.Razek (SAB)
*! Date      : 04/26/2012
*! Purpose   : Function to validate the report date.
*!*****************************************************************************************
FUNCTION lfvRepDate

** If report date is empty.
IF EMPTY(ldRpRepDat)
  *** You have to enter the �.

  *N000682 ,1 Thabet Handle globalization issues [Start]
  *=gfModalGen("TRM04066B00000","DIALOG",'report date')
  =gfModalGen("TRM04066B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_REPORT_DATE,oAriaApplication.GetHeaderText("LANG_REPORT_DATE",AHEADERFILE)))
  *N000682 ,1 Thabet Handle globalization issues [END]

  ldRpRepDat = lcOldVal
  &&SAB_CUROBJ     = _CUROBJ
ENDIF

*!*****************************************************************************************
*! Name      : lfvBucket
*! Developer : Saber A.Razek (SAB)
*! Date      : 04/26/2012
*! Purpose   : This function called to validate the number of days in every buckets.
*!*****************************************************************************************
FUNCTION lfvBucket

*B610057,1 SAB 08/25/2012 Fix Cash Requirment Open Balance [Start]
IF lcRpForm = 'APCASHR2'
  RETURN .T.
ENDIF
*B610057,1 SAB 08/25/2012 Fix Cash Requirment Open Balance [End]
DO CASE
 CASE lnRpBuckt1 <= 0     && Case firest bucket less than or equal zero.
  ** MESSAGE : " � should be greater than �."
  **           "           � Ok �           "

   *N000682 ,1 Thabet Handle globalization issues [Start]
   *=gfModalGen("TRM04072B00000","DIALOG",'No. of days for the 1st bucket|zero')
   =gfModalGen("TRM04072B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_NO_OF_1,oAriaApplication.GetHeaderText("LANG_NO_OF_1",AHEADERFILE)))
   *N000682 ,1 Thabet Handle globalization issues [END]

 CASE lnRpBuckt1 >= lnRpBuckt2  && Case 1st bucket grater than or equal 2nd bucket.
  ** MESSAGE : " � should be greater than �."
  **           "           � Ok �           "


  *N000682 ,1 Thabet Handle globalization issues [Start]
  *=gfModalGen("TRM04072B00000","DIALOG",'No. of days for the 2nd bucket|no. of days for the 1st bucket')
  =gfModalGen("TRM04072B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_NO_OF_2,oAriaApplication.GetHeaderText("LANG_NO_OF_2",AHEADERFILE)))
  *N000682 ,1 Thabet Handle globalization issues [END]

 CASE lnRpBuckt2 >= lnRpBuckt3  && Case 2nd bucket grater than or equal 3rd bucket.
  ** MESSAGE : " � should be greater than �."
  **           "           � Ok �           "

   *N000682 ,1 Thabet Handle globalization issues [Start]
   *=gfModalGen("TRM04072B00000","DIALOG",'No. of days for the 3rd bucket|no. of days for the 2nd bucket')
   =gfModalGen("TRM04072B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_NO_OF_3,oAriaApplication.GetHeaderText("LANG_NO_OF_3",AHEADERFILE)))
   *N000682 ,1 Thabet Handle globalization issues [END]

 CASE lnRpBuckt3 >= lnRpBuckt4  && Case 3rd bucket grater than or equal 4th bucket.
  ** MESSAGE : " � should be greater than �."
  **           "           � Ok �           "

   *N000682 ,1 Thabet Handle globalization issues [Start]
   *=gfModalGen("TRM04072B00000","DIALOG",'No. of days for the 4th bucket|no. of days for the 3rd bucket')
   =gfModalGen("TRM04072B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_NO_OF_4,oAriaApplication.GetHeaderText("LANG_NO_OF_4",AHEADERFILE)))
   *N000682 ,1 Thabet Handle globalization issues [END]

 OTHERWISE
   RETURN .T.
ENDCASE

lcRpVar  = SYS(18)
&lcRpVar = lcOldVal
&&SAB_CUROBJ  = _CUROBJ

*!*****************************************************************************************
*! Name      : lfvChngRep
*! Developer : Saber A.Razek (SAB)
*! Date      : 04/26/2012
*! Purpose   : Check if the report form is changed, And rebuild the options grid if changed.
*!*****************************************************************************************
FUNCTION lfvChngRep

** Rebuild the options grid only if the report form name is changed.
IF !(lcOldMode == lcRpForm)
  lcOldMode = lcRpForm
  =lfChangeGrid(ALLTR(lcRpForm))
ENDIF

*!*****************************************************************************************
*! Name      : lfvDayBuck
*! Developer : Saber A.Razek (SAB)
*! Date      : 04/26/2012
*! Purpose   : Function to validate the number on days in the bucket.
*!*****************************************************************************************
FUNCTION lfvDayBuck

** If the number of days in the bucket is less than or equal zero.
IF lnRpBuckt1 <= 0
  ** MESSAGE : " � should be greater than �."
  **           "           � Ok �           "

   *N000682 ,1 Thabet Handle globalization issues [Start]
   *=gfModalGen("TRM04072B00000","DIALOG",'Entered value|zero')
   =gfModalGen("TRM04072B00000","DIALOG",;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ENTERED_VALUE_ZERO,oAriaApplication.GetHeaderText("LANG_ENTERED_VALUE_ZERO",AHEADERFILE)))
   *N000682 ,1 Thabet Handle globalization issues [END]
  lnRpBuckt1 = lcOldVal
  &&SAB_CUROBJ  = _CUROBJ
ENDIF

*B610057,1 SAB 08/25/2012 Fix Cash Requirment Open Balance [Start]
*!*	****************************************************************************************
*!*	*! Name      : lfCheckDate
*!*	*! Developer : Saber A.Razek (SAB)
*!*	*! Date      : 04/26/2012
*!*	*! Purpose   : This function called in the detail part, to calculate totals in every bucket internval.
*!*	****************************************************************************************
*!*	FUNCTION lfCheckDate

*!*	IF llPrnFlag
*!*	  STORE 0.00 TO lnTotPaid,  lnTotDiscA, lnTotOpenB
*!*	ENDIF

*!*	IF dInvDuDat < ldTillDate
*!*	  lcRetVal = DTOC(dInvDuDat) + ' ' + cVendCode + ' ' + cInvNo + ' '+;
*!*	             DTOC(dInvDate)  + ' ' + cVenPrior + ' ' + TRANSFORM(nInvAmnt,'@$ 9999,999,999.99') + ' '+;
*!*	             TRANSFORM(nInvDisOf,'@$ 9999999.99') + ' '+ TRANSFORM(nInvPaid + nInvDisTk + nInvAdj,'@$ 9999,999,999.99') +;
*!*	             ' ' + TRANSFORM(nInvAmtAp,'@$ 9999,999,999.99') + ' '+ TRANSFORM(nInvDisAp,'@$ 9999999.99') + ' '+TRANSFORM(nInvAmnt - (nInvPaid + nInvDisTk + nInvAdj),'@$ 9999,999,999.99')
*!*	  lnTotPaid  = lnTotPaid  + nInvAmtAp
*!*	  lnTotDiscA = lnTotDiscA + nInvDisAp
*!*	  lnTotOpenB = lnTotOpenB + nInvAmnt - (nInvPaid + nInvDisTk + nInvAdj)
*!*	  llPrnFlag  = .F.

*!*	  llPstAmnt = (ABS(lnTotPaid) + ABS(lnTotDiscA) + ABS(lnTotOpenB)) <> 0
*!*	
*!*	  RETURN lcRetVal
*!*	ELSE
*!*	  SKIP -1 IN APINVHDR
*!*	
*!*	  ldTillDate = ldTillDate + lnRpBuckt1

*!*	  ldDispDate = ldDispDate + lnRpBuckt1
*!*	  llPstAmnt = (ABS(lnTotPaid) + ABS(lnTotDiscA) + ABS(lnTotOpenB)) <> 0
*!*	
*!*	  llPrnFlag  = .T.
*!*	
*!*	  RETURN ''
*!*	ENDIF
*B610057,1 SAB 08/25/2012 Fix Cash Requirment Open Balance [End]

*!*****************************************************************************************
*! Name      : lfvFactor
*! Developer : Saber A.Razek (SAB)
*! Date      : 04/26/2012
*! Purpose   : This function is to validate the Factors
*!*****************************************************************************************
FUNCTION lfvFactor

PRIVATE lcFactVar
lcFactVar = VARREAD()

IF !SEEK(&lcFactVar.,"SYCFACT")
  &lcFactVar. = lfFactBrow()
ENDIF

*!*****************************************************************************************
*! Name      : lfFactBrow
*! Developer : Saber A.Razek (SAB)
*! Date      : 04/26/2012
*! Purpose   : This function is to Browse the Factors
*!*****************************************************************************************
FUNCTION lfFactBrow
PRIVATE lcFields,lnCurAlias,lcBrFields,lcFile_Ttl
STORE SPACE(0) TO lcFields
PRIVATE laTemp
DIMENSION laTemp[1]
laTemp[1] = ""
lnCurAlias = SELECT(0)

lcFields    = "cFacCode"

* N000682 ,1 Thabet Handle globalization issues [Start]
*lcBrFields  = " cFacCode:H='Factor', cFacComp :H='Factor/Company name' "
*lcFile_Ttl  = 'Factors'
lcBrFields  = "cFacCode:H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_FACTOR,oAriaApplication.GetHeaderText("LANG_FACTOR",AHEADERFILE))+;
              "', cFacComp :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_FACTOR_COMPANY_NAME,oAriaApplication.GetHeaderText("LANG_FACTOR_COMPANY_NAME",AHEADERFILE))+"'"
lcFile_Ttl  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Factors,oAriaApplication.GetHeaderText("LANG_Factors",AHEADERFILE))
* N000682 ,1 Thabet Handle globalization issues [END]


SELECT SYCFACT
=gfBrows("",lcFields,'laTemp')
SELECT(lnCurAlias)

RETURN  IIF(!EMPTY(ALLTRIM(laTemp[1]))  , laTemp[1] , "")

*!*****************************************************************************************
*! Name      : lfvSortBy
*! Developer : Saber A.Razek (SAB)
*! Date      : 04/26/2012
*! Purpose   : This function is run form option grid with sort by.
*!*****************************************************************************************
FUNCTION lfvSortBy

IF lcRpSortOp = "V"
  * Sort by Vendor
  lcRpForm = "APCASHR"
ELSE
  * Sort by Due date
  lcRpForm = "APCASHR1"
ENDIF

*B610057,1 SAB 08/25/2012 Fix Cash Requirment Open Balance [Start]
*!*****************************************************************************************
*! Name      : lfGetRepForm
*! Developer : Saber A.Razek (SAB)
*! Date      : 08/15/2012
*! Purpose   : Get the report form
*!*****************************************************************************************
FUNCTION lfGetForm

DO CASE
CASE llUseMBuck
  * Use Multiple Buckets = "Yes"
  lcRpForm = "APCASHR2"
  *N000682,1 MMT 03/17/2013 Fix issue of testing globlaization Phase#2[Start]
  *lcBcktTitle = 'Bucket Size'
  lcBcktTitle = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BUCKETSIZE,oAriaApplication.GetHeaderText("LANG_BUCKETSIZE",AHEADERFILE))
  *N000682,1 MMT 03/17/2013 Fix issue of testing globlaization Phase#2[End]
CASE !llUseMBuck AND lcRpSortOp = 'V'
  * Use Multiple Buckets = "No" and Sort by = "Vendor"
  lcRpForm = "APCASHR"
  *N000682,1 MMT 03/17/2013 Fix issue of testing globlaization Phase#2[Start]
  *lcBcktTitle = 'Days for 1st Bucket'
  lcBcktTitle = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DAY1BUCKET,oAriaApplication.GetHeaderText("LANG_DAY1BUCKET",AHEADERFILE))
  *N000682,1 MMT 03/17/2013 Fix issue of testing globlaization Phase#2[End]
CASE !llUseMBuck AND lcRpSortOp = 'D'
  * Use Multiple Buckets = "No" and Sort by = "Due Date"
  lcRpForm = "APCASHR1"
  *N000682,1 MMT 03/17/2013 Fix issue of testing globlaization Phase#2[Start]  
  *lcBcktTitle = 'Days for 1st Bucket'
  lcBcktTitle = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DAY1BUCKET,oAriaApplication.GetHeaderText("LANG_DAY1BUCKET",AHEADERFILE))
  *N000682,1 MMT 03/17/2013 Fix issue of testing globlaization Phase#2[END]  
ENDCASE

=ClearRead()

ENDFUNC


*!*****************************************************************************************
*! Name      : lfwRepWhen
*! Developer : Saber A.Razek (SAB)
*! Date      : 08/26/2012
*! Purpose   : the report when function
*!*****************************************************************************************
FUNCTION lfwRepWhen
*N000682,1 MMT 03/17/2013 Fix issue of testing globlaization Phase#2[Start]
lcBcktTitle = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DAY1BUCKET,oAriaApplication.GetHeaderText("LANG_DAY1BUCKET",AHEADERFILE))
*N000682,1 MMT 03/17/2013 Fix issue of testing globlaization Phase#2[End]
ENDFUNC

*!*****************************************************************************************
*! Name      : lfCollData
*! Developer : Saber A.Razek (SAB)
*! Date      : 08/26/2012
*! Purpose   : Collect data in APCASHR2 case
*!*****************************************************************************************
FUNCTION lfCollData

IF USED(lcAPINVHDR)
  USE IN (lcAPINVHDR)
ENDIF

lcAPINVHDR = gfTempName()
SELECT APINVHDR
lnFieldCnt = AFIELDS(laFileStru)

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,18]
laFileStru[lnNewFld,1] = 'dDueByDat'
laFileStru[lnNewFld,2] = 'D'
laFileStru[lnNewFld,3] = 8
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,18]
laFileStru[lnNewFld,1] = 'nTotPaid'
laFileStru[lnNewFld,2] = 'N'
laFileStru[lnNewFld,3] = 15
laFileStru[lnNewFld,4] = 2

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,18]
laFileStru[lnNewFld,1] = 'nTotDisc'
laFileStru[lnNewFld,2] = 'N'
laFileStru[lnNewFld,3] = 15
laFileStru[lnNewFld,4] = 2

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,18]
laFileStru[lnNewFld,1] = 'nTotOpen'
laFileStru[lnNewFld,2] = 'N'
laFileStru[lnNewFld,3] = 15
laFileStru[lnNewFld,4] = 2

FOR lnCount = 1 TO ALEN(laFileStru,1)
  FOR lnItmNo = 7 TO 16
    STORE '' TO laFileStru[lnCount, lnItmNo]
  ENDFOR
  STORE 0 TO laFileStru[lnCount,17],laFileStru[lnCount,18]
ENDFOR

=gfCrtTmp(lcAPINVHDR, @laFileStru, 'DTOS(DINVDUDAT)',lcAPINVHDR,.F.)   && DUEDATE && DTOS(DINVDUDAT)

ldDueByDat = ldTillDate
STORE 0.00 TO lnTotalPaid, lnTotalDisc, lnTotalOpen
SELECT APINVHDR
SCAN FOR &lcRpExp.
  IF !(APINVHDR.dInvDuDat < ldDueByDat)
    SELECT (lcAPINVHDR)
    APPEND BLANK
    REPLACE dDueByDat WITH ldDueByDat,;
            nTotPaid  WITH lnTotalPaid,;
            nTotDisc  WITH lnTotalDisc,;
            nTotOpen  WITH lnTotalOpen,;
            dInvDuDat WITH ldDueByDat

    STORE 0.00 TO lnTotalPaid, lnTotalDisc, lnTotalOpen
    ldDueByDat = ldDueByDat + lnRpBuckt1
  ENDIF

  IF BETWEEN(APINVHDR.dInvDuDat, ldDueByDat - lnRpBuckt1, ldDueByDat)
    SELECT APINVHDR
    SCATTER MEMO MEMVAR
    SELECT (lcAPINVHDR)
    APPEND BLANK
    GATHER MEMO MEMVAR
    lnTotalPaid = lnTotalPaid + nInvAmtAp
    lnTotalDisc = lnTotalDisc + nInvDisAp
    lnTotalOpen = lnTotalOpen + nInvAmnt - (nInvPaid + nInvDisTk + nInvAdj)
  ELSE
    SELECT APINVHDR
    SKIP -1
    SELECT (lcAPINVHDR)
  ENDIF

ENDSCAN

SELECT (lcAPINVHDR)
APPEND BLANK
REPLACE dDueByDat WITH ldDueByDat,;
        nTotPaid  WITH lnTotalPaid,;
        nTotDisc  WITH lnTotalDisc,;
        nTotOpen  WITH lnTotalOpen,;
        dInvDuDat WITH ldDueByDat

STORE 0.00 TO lnTotalPaid, lnTotalDisc, lnTotalOpen

ENDFUNC

*B610057,1 SAB 08/25/2012 Fix Cash Requirment Open Balance [End]
