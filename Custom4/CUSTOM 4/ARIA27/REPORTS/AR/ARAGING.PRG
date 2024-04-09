*:***************************************************************************
*: Program file  : ARAGING.PRG
*: Program desc. : A/R aging report
*: For Report    : Araging.FRX ,Araginh.FRX
*: System        : Aria Advantage Series.
*: Module        : Account Rec. (AR)
*: Developer     : Samah Wilson Kirollos (SWK)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfItemMask,gfPhoneTem,gfTempName,gfGetAdr,gfDispRe ,
*:               : gfRltFld, gfCodDes,gfADel , gfGetMemVar,gfOptMsg,gfBrows.
*:               : lfGetLogo,lfAdrShift,lfSolSpAdr,lfHeadVar,lfGetNotes,
*:               : lfNoteHead,lfNoteData,lfEndGroup,lfwRepWhen,lfFormName,
*:               : lfvOptMsg,lfwOldVal,lfvOrder,lfClearRep.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: This Program is due to E300880 ...
*:***************************************************************************
*: Modifactions
*B602103 MAB {Mohamed Badran} at 10/26/1998 - Fix Separte Bugs in Report.
*B801811,1 MAB {Mohamed Badran} at 11/11/1998 - Fix single companies bugs.
*B801901 MAB {Mohamed Badran} at 01/24/1999 - Fix Numeric overflow bug.
*B801901 MAB      Note : This bug is fixed by Samah, just I rewrite it,
*B801901 MAB             make the width 6 decimals and add another fix to it.                     
*B801903 MAB {Mohamed Badran} at 01/24/1999 - Add Codes before description
*B801903                                      in lfGetDesc Function.
*E301126 SAMEH at 02/22/1999 enh. made to print POST DATED CHECKS in 
*E301126       both ARGING,ARGINH (Dos Frx) report 
*E301126 Enh. is made in both ARAGING/ARAGINH (Dos Frx)
*E301126 in ARAGING (Detail/Summary) , adding in the 2nd group band another 
*E301126 field in the GROUP BY , this field is made in the lcTmpTran file
*E301126 where the GROUP BY is made , this field consists of 1 char. to hold
*E301126 whether it's INVOICE or CHECKS , also there is some fields in 
*E301126 report are transferred to VARIABLES to hold in its expressions 
*E301126 more lines than the fields in rep can do , these VAR are : lnAgeCu,
*E301126 lnAge30,lnAge60,lnAge90,lnAge120,lnCred30,lnCred60,lnCred90,
*E301126 lnCred120,lnNet30,lnNet60,lnNet90,lnNet120,lnOver30,lnOver60,
*E301126 lnOver90,lnOver120,lnApCre,lnAr,lnAgeBl
*E301126 also in these VARIABLES , 0.0001 is added to TOTAL in case of  
*E301126 INVOICES just to make the total appear as 0.00 (if the total of 
*E301126 invoice is 0) , it will be ignored in CHECKS (Blank If Zero)
*:E301126 ----------------------------------------------------------------
*E301126 in case of ARAGINH (SHORT report) , there is a function called
*E301126 lfPostChq() , called from the Detail Band in the report this
*E301126 function gets total post dated checks in case of not multi 
*E301126 currency company
*B802095 Sameh 04/01/99 Correcting spelling mistake in Option Grid 
*B802095                (Acount) became (Account)
*B802102,1 MAB 04/01/99 Structual CDX not found.
*B802051,1 MAB 04/01/99 Fix report printing (Print company Name,
*B802051,1              Maximize numeric fields width in short form)
*B802124,1 MAB 04/18/1999 Match data collection in Short form with 
*B802124,1                detail/summary form, fix in this program
*B802124,1                and in ARAGING.FRX
*E301214,1 MAB 05/02/1999 Optimize report performance in both .Prg and Forms .
*E301238,1 MAB 05/23/1999 Call gfCodDes with new .T. parameter return
*E301238,1                Code + Description if Editable code,
*E301238,1                else return Description only.
*E301239,1 SSE 05/23/1999 Make window .FRX for the report,things was done in .FRXs.
*B802825,1 RAMY 12/09/1999 Add one char to the field of OpenCr in araginh.frx
*E301214,1 MAB 05/30/1999 New Program code instead of last one 
*B603053,1 MAB 07/05/1999 Calculate equav. Base rate and unit from transaction file.
*B603241,1 SSE 10/27/1999 Modify Grand Total columns in Short format to be 99999999.99
*B802723,1 MAB 11/07/1999 Do not print zero transactions.
*B803041,1 ADEL 02/13/2000 Fix the bug that report is the same whether Factored or Non-Factored
*B803041,1                 by chenge LnotChFlt field to .F. in SYDREPUVR file only.
*E301429,1 NAD 06/18/2000  Add record to syrepuvr to ask the user to print
*E301429,1                 decimals or not.  
*B802892,1 BWA 08/03/2000 Fix the bug of the open credit column prints stars.[FRX]
*B603353,1 BWA 08/08/2000 Show the key off transactions under there title.
*B603353,1                [PRG & FRX > In field print Key Off in the Detail.
*B603898,1 BWA 09/17/2000 Fix the bug of missing and duplicate data in the short format.
*B603934,1 BWA 10/01/2000 Fix the bug of Total balance in Detail report doesn't match in short report.
*E500378,1 BWA 10/04/2000 Add 2 filters to the criteria of the report for the customer status & balance.
*C102178,1 BWA 01/23/2000 1) Change the selection grid for the customer to be mover to let the user select more than one choice.
*C102178,1                2) Print the notepad of the invoices only and this task customize for USA office.
*B604159,1 BWA 01/29/2001 The address is not printed completed.[FIX IN FRX]
*B604516,1 NAD 06/05/2001 ERR Cannot find HPBXLB.DRV in windows 2000 Fix in the frx by making TAG1 AND TAG2 Empty
*B803975,1 BWA 08/09/2001 Fix the bug of making the field of the Balance amount to be accurate to the value entered in it.
*C200240,1 SSE 10/28/2001 Custom add Region mover to AR Aging for MBI England.
*B605247,1 SSE 12/19/2001 Fix bug of adding the posting date in option grid.
*B605385,1 NAD 12/19/2001 Fix bug of adding the posting date in option grid To the ARHIST File
*C605955,1 ABD 05/26/2002 Fix bug that the araging should filter on the invhdr.rep1 and invhdr.rep2 not 
*C605955,1 ABD            The sales rep of the customer file 
*B605712,1 BWA 09/01/2002 Disable the filter of the Customer Notepad in case Print by Summery.
*B605499,1 SSE 10/21/2002 Fix bug of always printing portait even when selecting landscape.
*B605499,1                All modifications is in FRX ARAGING.FRX (PRG for documentation only)
*C200419,1 SSE 10/28/2001 Custom add Region , division mover to AR Aging for RON England.
*B606993,1 NAD 02/23/2003 Aged Accounts Receivable report not showing keyed off.        
*B606890,1 KHM 05/19/2003 Fix the problem of sorting by terms. The program sorts by 
*B606890,1                term code instead of description.
*B607360,1 KHM 06/26/2003 Fix the bug of not including the material invoice that keyed off.
*B607446,1 KHM 07/27/2003 Fix the bug of not displaying the data in short report and sort by term
*B120173,1 MMM 11/11/2003 Fix the bug of printing sub-totals as *** when running the report in 
*                         Short form and in Graphic [Fix in the FRX]. 
*B123160,1 BWA 06/10/2004 Fix the bug of in rare cases the transaction numbers same to each other.
*:***************************************************************************

*=================================  Begin ===============================*
*=========================  Report Program Code =========================*
*========================================================================*
*-- lcStTime   : Variable to hold the start Time
*-- llCurrGrp  : Flag .T. if multi currency and print in foreign and not sort by currency.
*-- lcReplExpr : Replace key field expression instead of use index command on huge file
*-- lcAcctFilt : Variable to hold filter on account
*-- lcSorted   : Printed Sort By expression
*-- lcGroup    : variable hold Short Form groups.
*-- lcInnGrp   :
*-- lcCodeDesc :
lcStTime   = TIME()
llCurrGrp  = llMultCur AND (lcRpCurr = 'F') AND (lcRpAgSort !='C')
STORE '' TO lcAcctFilt , lcSorted , lcGroup , lcInnGrp , lcCodeDesc
lcReplExpr = lfGetReplc()  && Get Sort by value to update cTempKey with it.

*C200419,1 This trigger is used also by RON20 (England).
*C200240,1 Check Region if changed. [Begin]
=gfDoTriger('ARAGING',PADR('CHECKREG',10))
*C200240,1 Check Region if changed. [End]

*C605955,1 ABD -  Fix bug that the araging should filter on the invhdr.rep1 and invhdr.rep2 not 
*C605955,1 ABD - The sales rep of the customer file. [Begin]
llWorkFile   = .F.
lcSalsFile   = ''
lcRpSlsExp   = [.T.]
*-- Get the sales Rep Position.
lnRpSlsPos   = lfItmPos("SALESREP.REPCODE")
IF lnRpSlsPos > 0 .AND. laOGFxFlt[lnRpSlsPos,7] = 'R'
  lcSalsFile = laOGFxFlt[lnRpSlsPos,6]
  llWorkFile = !EMPTY(lcSalsFile) AND USED(lcSalsFile) AND RECCOUNT(lcSalsFile) > 0
  IF llWorkFile
    lcRpSlsExp = [ llWorkFile .AND. (SEEK(InvHdr.Rep1,lcSalsFile) .OR. SEEK(InvHdr.Rep2,lcSalsFile))]
  ENDIF

  *- Cut the lcRpExp
  IF AT('In List',laOgFxFlt[lnRpSlsPos,5]) <> 0 
    lnSlsPost = AT('INLIST(SALESREP.REPCODE',lcRpExp)
  ENDIF

  IF lnSlsPost > 0
    lnPos1     = AT('AND' , SUBSTR(lcRpExp,lnSlsPost))
    IF lnPos1 > 0
      lcSlsPost = STRTRAN(lcRpExp , SUBSTR(lcRpExp , lnSlsPost , lnPos1-1) , " .T. ")
    ELSE
      lcSlsPost = STRTRAN(lcRpExp , SUBSTR(lcRpExp , lnSlsPost , LEN(lcRpExp)) , ' .T. ')
    ENDIF
  ENDIF
ENDIF
*C605955,1 ABD - [End]


*-- if user change filter criteria then you must collect data again
IF llOGFltCh
  *-- if you have previous data clear work files then recreate it. [begin]
  IF !USED(lcTempAge) OR (RECCOUNT(lcTempAge) > 0)
    =lfWorkFile()
  ENDIF
  *-- if you have previous data clear work files then recreate it. [End  ]

  *-- llChSortBy : Flag if .T. indicate that user change sort by, but here false it
  *                Avoid Replacing Key field with proper sort by values in if...endif block.
  *                i.e. do not enter reindex block because we do it with data collection.
  llChSortBy = .F. 
  
  =lfCollect()               && Collect Aging data.
  
ENDIF  && if user change filter criteria then you must collect data again.

*-- Asking if no records (Display message) otherwise print report [Begin.]
IF RECCOUNT(lcTempAge) = 0
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF
*-- Asking if no records (Display message) otherwise print report [End.]

*-- Filter Temporary Account File [Begin]

*-- if user want to show due past only.
IF llRPShowPa
  lcAcctFilt = [nAges > 0]
ENDIF  && end if user want to show due past only

*-- if user want balance debits only.
IF lcRpBalanc = 'D'
  lcAcctFilt = IIF(EMPTY(lcAcctFilt) , '' , lcAcctFilt + [ AND ]) + [nNetBal >= lnRpBalanc]
ENDIF  && end if user want balance debits only.

*-- if user want balance credits only.
IF lcRpBalanc = 'C'
  lcAcctFilt = IIF(EMPTY(lcAcctFilt) , '' , lcAcctFilt + [ AND ]) +  [nNetBal <= lnRpBalanc]
ENDIF  && end if user want balance credits only.

*-- if user print short form suppress zero lines because this form print totals.
IF lcRpReport = 'H'
  lcAcctFilt = IIF(EMPTY(lcAcctFilt) , '' , lcAcctFilt + [ AND ]) + [nNetBal <> 0]
ENDIF  && end if user print short form suppress zero lines.

*-- if user want Balance greater than in case balance = both only.
*E500378,1 Add filter in case user select balance = both get a new filter 
*E500378,1 balance greater than for the net balance of the customer.[START]
IF lcRpBalanc = 'B' AND lnRpBalanc <> 0

  *B803975,1 BWA 08/09/2001 Fix the bug of making the field of the Balance amount to be accurate to the value entered in it.[START]
  *lcAcctFilt = IIF(EMPTY(lcAcctFilt) , '' , lcAcctFilt + [ AND ]) + [nNetBal > lnRpBalanc]
  IF lnRpBalanc < 0
    lcAcctFilt = IIF(EMPTY(lcAcctFilt) , '' , lcAcctFilt + [ AND ]) + [nNetBal < lnRpBalanc]
  ELSE
    lcAcctFilt = IIF(EMPTY(lcAcctFilt) , '' , lcAcctFilt + [ AND ]) + [nNetBal > lnRpBalanc]
  ENDIF
  *B803975,1 BWA 08/09/2001 .[END]

ENDIF
*E500378,1 [END]

*-- if you have totals filter.
IF !EMPTY(lcAcctFilt)

  SELECT (lcTempAcc)
  SET FILTER TO &lcAcctFilt

  GO TOP
  *-- Asking if no records (Display message) otherwise print report [Begin.]
  IF EOF()
    *---Text : 'No Record Selected for the report..!'
    =gfModalGen('TRM00052B00000','DIALOG')
    RETURN
  ENDIF
  *-- Asking if no records (Display message) otherwise print report [End.]

ENDIF  && end if you have totals filter
*-- Filter Temporary Account File [End  ]

*=================================  Begin ===============================*
*==========================  Have data section ==========================*
*========================================================================*
SELECT (lcTempAge)
*-- Reindexing file if user change sort by [Begin.]
IF llChSortBy
  llChSortBy = .F.  && Avoid Replacing Key field with proper sort by values again.
  REPLACE ALL cTempKey WITH EVALUATE(lcReplExpr) && Replace key field with new values.
ENDIF
*-- Reindexing file if user change sort by [End.]

*-- if print short form.
IF lcRpReport = 'H'

  *-- Create Another alias form the same line descending order to print total line 
  *-- from withen detail band of form, and this technique have the ability to print
  *-- both transaction lines and post dated cheques from one line in this band, avoiding
  *-- waste of papers.
  IF !USED(lcTempRev)
    DO lpAnothAls
  ENDIF
  
  *-- Set relation with reverse file.
  SELECT (lcTempAge)
  SET RELATION TO EVALUATE(lcReplExpr) INTO (lcTempRev)
  
  *-- lcTermData : Printed in short form if line figure transaction totals
  *-- lcPostText : Printed in short form if line figure Post dated totals
  *-- lcCurrSec  : Printed in short form currency section.

  *-- if multi currency company.
  IF llMultCur
    lcCurrSec = [cCurrCode + '-' + IIF(SEEK(cCurrCode,'SycCurr'),PADR(SycCurr.CCURRDESC,9),SPACE(9))]

  ELSE  && else single currency company.
  
    lcCurrSec = [PADR(TRANSFORM(IIF(EMPTY(CUSTOMER.PHONE2), ]      +;
                [CUSTOMER.PHONE1,CUSTOMER.PHONE2),gfPhoneTem()),13)]
  ENDIF  && end if multi currency company.

  
  *E301238,1 Call gfCodDes with new .T. parameter [Begin]
  *lcTermData = [IIF(RECNO() <> RECNO(lcTempRev) OR ]                      +;
  *             [(lnGrpAge=0 AND lnGrpCre=0),'',]                          +;
  *             [PADR(CUSTOMER.BTNAME,10) + ' ' + ]                        +;
  *             lcCurrSec                                                  +;
  *             [Customer.SALESREP + ' ' + ]                               +;
  *             [cTermCode + ' ' +]                                        +;
  *             [PADR(gfCoddes(cTermCode,'CTERMCODE',.T.),25))]

  lcTermData = [IIF(RECNO() <> RECNO(lcTempRev) OR ]                      +;
               [(lnGrpAge=0 AND lnGrpCre=0),'',]                          +;
               [PADR(CUSTOMER.BTNAME,10) + ' ' + ]                        +;
               lcCurrSec + [ + ' ' +]                                     +;
               [Customer.SALESREP + ' ' + ]                               +;
               [PADR(gfCoddes(cTermCode,'CTERMCODE',.T.),25))]
  *E301238,1 Call gfCodDes with new .T. parameter [End  ]

  lcPostText = [IIF(RECNO() <> RECNO(lcTempRev) OR lnGrpPost=0 ,'',]      +;
               [PADR(CUSTOMER.BTNAME,10) + ]                              +;
               [',   Totals Post Dated Cheques = ' + ]                    +;
               [TRANSFORM(lnGrpPost,"99999999.99"))]

ELSE  && else print Detail or Summary forms.

  *-- Prepair to deal with Age Credits (Yes / No)
  llPrnCrdDt = llRpCrdDet AND llRPAgeCrd     && Print credit detail and age credits.
  
  *-- if user want to print new page per account
  IF llRpAccPag
    IF !USED(lcOGTmpForm)

      *-- To Fix structural CDX not found bug until making global fix [Begin] 
      lcSvErrHan = ON('ERROR')
      ON ERROR
      *-- To Fix structural CDX not found bug until making global fix [END  ] 
      
      USE &gcWorkDir.&lcOGTmpForm..FRX IN 0 ORDER 0 EXCLUSIVE

      *-- To Fix structural CDX not found bug until making global fix [Begin] 
      ON ERROR &lcSvErrHan
      *-- To Fix structural CDX not found bug until making global fix [End  ] 

    ENDIF

    *-- Replace pagebreak with .T. to print new page per account group.
    SELECT (lcOGTmpForm)
    LOCATE FOR OBJTYPE = 9 AND OBJCODE = 3 AND 'ACCOUNT' $ UPPER(ALLTRIM(EXPR))
    REPLACE PAGEBREAK WITH .T.
    USE IN (lcOGTmpForm)  && Close file.
  ENDIF

ENDIF  && end if print short form.

*-- Set relation between Temporary file and customer files.
SELECT (lcTempAge)
SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER ,;
                      ACCOUNT INTO (lcTempAcc) ADDITIVE

*-- Evaluate Aging headers, (Report aging titles). [Begin]
*-- If Age by date
IF lcAgeType = 'D'

  lcAgeHd30  = 'Over ' + ALLTRIM(STR(lnRPDay1))
  lcAgeHd60  = 'Over ' + ALLTRIM(STR(lnRPDay2))
  lcAgeHd90  = 'Over ' + ALLTRIM(STR(lnRPDay3))
  lcAgeHd120 = 'Over ' + ALLTRIM(STR(lnRPDay4))

ELSE  && Else Age by terms

  lcAgeHd30  = '1 - ' + ALLTRIM(STR(lnRPDay1))
  lcAgeHd60  = ALLTRIM(STR(lnRPDay1+1)) + ' - ' + ALLTRIM(STR(lnRPDay2))
  lcAgeHd90  = ALLTRIM(STR(lnRPDay2+1)) + ' - ' + ALLTRIM(STR(lnRPDay3))
  lcAgeHd120 = 'Over ' + ALLTRIM(STR(lnRPDay3+1))

ENDIF  && end If Age by date.
*-- Evaluate Aging headers, (Report aging titles). [End  ]

*-- Calculate spent time in collecting data.
lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcTempAcc))) + ' Customer(s), ' + ALLTRIM(STR(RECCOUNT())) + ' Transaction(s) in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT

*========================================================================*
*==========================  Have data section ==========================*
*=================================   End  ===============================*


*-- Print Code

*B603898,1 BWA 09/17/2000 Fix the bug of missing and duplicate data in the short format.[START]
IF lcRpReport = 'H'            && In case the user select the short report.
  REPLACE ALL &lcTempAge..cTranType WITH "" FOR &lcTempAge..cTranType = 'H'
ENDIF
*B603898,1 [END]

*C102178,1 BWA 01/23/2000 2) Print the notepad of the invoices only and this task customize for USA office.[START]
STORE SPACE(0) TO lcAcntPad
llRpInvNot = IIF(TYPE('llRpInvNot') = 'U', .F. , llRpInvNot)

IF llRpInvNot
  SELECT (lcTempAge)
  GO TOP
  SCAN
    IF llRPCusNot AND !EMPTY(lcAcntPad)
      IF lcAcntPad # &lcTempAge..ACCOUNT
        SKIP - 1
        REPLACE &lcTempAge..llFlgPad WITH .T.
        SKIP
      ENDIF
    ENDIF
    lcAcntPad = &lcTempAge..ACCOUNT
    IF &lcTempAge..TRANTYPE = "1" AND  SEEK('C'+ &lcTempAge..TRAN,'NOTEPAD') .AND.;
       !EMPTY(ALLTRIM(NOTEPAD.mNotes)) .AND. LEFT(ALLTRIM(STRTRAN(NOTEPAD.mNotes,CHR(13)+CHR(10),' ')),1) <> '*'
      REPLACE &lcTempAge..mNotes WITH NOTEPAD.mNotes
    ENDIF
  ENDSCAN
  IF EOF(lcTempAge)
    SKIP - 1
    REPLACE &lcTempAge..llFlgPad WITH .T.
  ENDIF
ENDIF
*C102178,1 [END]

GO TOP  && Refresh relation

IF EMPTY(lcAcctFilt)
  DO gfDispRe WITH EVALUATE('lcRpName')  && No totals filter.
ELSE
  DO gfDispRe WITH EVALUATE('lcRpName'), 'FOR !EOF(lcTempAcc)'  && Have totals filter.
ENDIF  

WAIT CLEAR

*-- Release relation.
SET RELATION TO

*-- Close reverse file (if print short form only)
IF USED(lcTempRev)
  USE IN (lcTempRev)
ENDIF
*-- end of report code.
*E301214,1 New Program code instead of last one [End  ]
*========================================================================*
*=========================  Report Program Code =========================*
*=================================   End  ===============================*


*========================================================================*
*==========================  Functions Section ==========================*
*========================================================================*
*!*************************************************************
*! Name      : lpAnothAls
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Open temporary cursor in another alias with reverse sorting 
*!           : which help us detect last line in each group with out using 
*!           : .FRX group to gain suppress line if blank which work only in 
*!           : .FRX detail band, also print Transaction line and post dated line
*!           : from the same .Frx detail line which save paper space.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : DO lpAnothAls
*!*************************************************************
*!Due to E301214,1
*
PROCEDURE lpAnothAls
PRIVATE lcFullPath , lcCurDir
lcFullPath = SET('FULLPATH')
SET FULLPATH ON 
lcCurDir   = DBF()  && Get Cursor full path.
SET FULLPATH &lcFullPath
  
USE (lcCurDir) IN 0 AGAIN ALIAS (lcTempRev)  && Use the same file in another alias
SELECT (lcTempRev)
SET ORDER TO TAG (lcTempRev)  && Set Descending order.
*-- end of lpAnothAls.

*!*************************************************************
*! Name        : lfpostChq
*! Developer   : Sameh Saiid (SSE)
*! Date        : 02/22/1999
*! Purpose     : get total POST DATED CHECKS for each ACCOUNT
*! Called From : SHORT report (ARAGINH)----->DOS FRX 
*!*************************************************************
*! Example     : lfPostChq()
*!*************************************************************
*!Due to E301214,1 this function is no longer in use.
FUNCTION lfPostChq

SELECT (lcPrtFile)
  lcAccount  = ACCOUNT  
    SELECT POSTDCHQ
    IF SEEK(&lcPrtFile..ACCOUNT)
      SUM amount REST WHILE account = lcAccount TO lnPDChcks
    ELSE
      lnPDChcks = 0.00
    ENDIF

RETURN lnPDChcks

*!*************************************************************
*! Name      : lfvAcct
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : Validate the acount from the option grid
*!*************************************************************
*! Example     : = lfvAcct()
*!*************************************************************
FUNCTION lfvAcct
PRIVATE lcObjNam , lcObjVal , llObjRet

lcObjNam = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*-- IF The user want to Browse or if the Account he entered is not in the file
IF !EMPTY(lcObjVal) .AND. !SEEK('M'+lcObjVal , 'CUSTOMER')
  llBrowse = .T.
  xAccount = lcObjVal
  DO CUSBROWM WITH xAccount
  lcObjVal = xAccount
  llBrowse = .F.
ENDIF    && End of IF
&lcObjNam = lcObjVal
*!*************************************************************
*! Name      : lfvReport
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : Called from the report form to enable 
*!             and disable some other fields in the option grid
*!*************************************************************
*! Example     : = lfvReport()
*!*************************************************************
*!Due to E301214,1 Clear Read to use suppress facility.
*
FUNCTION lfvReport
IF lcRpReport = 'H'
  lcRPName   = 'ARAGINH'
  = lfRepPltFr(lcRPName)
  
  *E301214,1 [Begin]
  *-- Do not print Page/Account
  *-- Do not print Credit detail
  *-- Do not print Customer Note Pad
  STORE .F. TO llRpAccPag , llRpCrdDet , llRpCusNot
  *E301214,1 [End  ]
  
ELSE  

  *-- if last run form was short Restore defaults.
  IF lcRpName = 'ARAGINH'
  
    lcRpName = 'ARAGING'
    = lfRepPltFr(lcRPName)

    *E301214,1 [Begin]
    *-- if detail form do default to print Credit detail, else do not print credit detail.
    STORE (lcRpReport $ 'DS') TO llRpCrdDet , llRpAgeCrd

    *E301214,1 [End  ]
  
  ENDIF  

ENDIF

*E301214,1 Activate suppress expressions [Begin]
CLEAR READ
*E301214,1 Activate suppress expressions [End  ]
*-- 

*!*************************************************************
*! Name      : lfwGrid
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : Called from the when function of the option grid
*!*************************************************************
*! Example     : = lfwGrid()
*!*************************************************************
*!Due to E301214,1 Create arrays hold temporary files structures, then create them.
*
FUNCTION lfwGrid
*E301214,1 if first time run report [Begin]
*E301214,1 lcBuild is defined in reports generator as numeric field and after first run gone to
*E301214,1 be character.
IF TYPE('lcBuild') = 'N'
  lcBuild = 'OK'
  lcPrtFac = 'FACTORED AND NON FACTORED INVOICES'

  *E301214,1 Create Temporary cursor first run. [Begin]
  =lfFilTrnAr()  && Fill arrays then create files.
  *E301214,1 Create Temporary cursor first run. [End  ]

ENDIF  

*-- Disable Age Credits if user does not want to include credit detail [Begin]
= !llRpCrdDet AND lfvIncDet()
*-- Disable Age Credits if user does not want to include credit detail [End  ]

*E301214,1 if first time run report [End  ]


*!*************************************************************
*! Name      : lfvInv
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : Called from the option grid to set the variable
*!             of factored invoices
*!*************************************************************
*! Example     : = lfvInv()
*!*************************************************************
FUNCTION lfvInv

lcRPSpeFac = IIF((lcRPFactor = 'F'),lcRPSpeFac,SPACE(05))
CLEAR READ

*DO CASE
*  CASE lcRPFactor = 'F'
*    lcPrtFac = 'FACTORED INVOICES'
*  CASE lcRPFactor = 'N'
*    lcPrtFac = 'NON FACTORED INVOICES'
*  CASE lcRPFactor = 'B'
*    lcPrtFac = 'FACTORED AND NON FACTORED INVOICES'
*ENDCASE
*--

*!*************************************************************
*! Name      : lfvBalance
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : Called from the option grid to set the variable
*!             of balance
*!*************************************************************
*! Example     : = lfvBalance()
*!*************************************************************
FUNCTION lfvBalance

DO CASE
  CASE lcRPBalanc = 'C'
    lcHedBal  = 'Under'   
    lnRpBalanc = ABS(lnRPBalanc) * -1
  CASE lcRPBalanc = 'D'
    lcHedBal  = 'Over'
    lnRpBalanc = ABS(lnRPBalanc)

  *E500378,1 Add new header to the new filter .[START]
  CASE lcRPBalanc = 'B'
    lcHedBal  = 'Balance Greater than'

    *B803975,1 BWA 08/09/2001 Fix the bug of making the field of the Balance amount to be accurate to the value entered in it.[START]
    *lnRpBalanc = ABS(lnRPBalanc)
    IF lnRpBalanc < 0
      lnRpBalanc = ABS(lnRPBalanc) * -1
    ELSE
      lnRpBalanc = ABS(lnRPBalanc)
    ENDIF
    *B803975,1 BWA 08/09/2001 .[END]

  *E500378,1 [END]

  OTHERWISE
    lcHedBal  = SPACE(10)
    lnRpBalanc = 0
ENDCASE
CLEAR READ  
*-- 

*!*************************************************************
*! Name      : lfvFactor
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : to validate a specific factor
*!*************************************************************
*! Example     : = lfvFactor()
*!*************************************************************
FUNCTION lfvFactor
PRIVATE lnAlias

lnAlias = SELECT()
SELECT CUSTOMER
IF !EMPTY(lcRPSpeFac) .AND. !SEEK ('F'+lcRPSpeFac)
  llNoThing = lfGetFac (lcRPSpeFac)
ENDIF
SELECT(lnAlias)
*!*************************************************************
*! Name      : lfvSalRep
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : to validate the salesrep field from the option grid
*!*************************************************************
*! Example     : = lfvSalRep()
*!*************************************************************
FUNCTION lfvSalRep
PRIVATE lcObjNam , lcObjVal , llObjRet

SET ORDER TO TAG SALESREP IN SALESREP
lcObjNam = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*-- IF The user want to Browse or if the Account he entered is not in the file
IF !EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'SALESREP')
  llBrowse = .T.
  xAccount = lcObjVal
  DO REPCHK WITH xAccount
  lcObjVal = xAccount
  llBrowse = .F.
ENDIF    && End of IF
&lcObjNam = lcObjVal
*!*************************************************************
*! Name      : llPrtSub
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : Called from the report the check when to print the subtotals
*!*************************************************************
*! Example     : = llPrtSub()
*!*************************************************************
*!Due to E301214,1 this function is no longer in use.
FUNCTION llPrtSub

IF EOF()
  llPrintSub = .T.
ENDIF
RETURN SPACE(01)  
*!*************************************************************
*! Name      : lfvDay1
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : to validate the number of days for the 1st bucket
*!*************************************************************
*! Example     : = lfvDay1()
*!*************************************************************
FUNCTION lfvDay1

IF lnRPDay1 <= 0
  =gfModalGen('TRM40132B40011','ALERT')
  lnRPDay1 = lnOldDays
ENDIF
IF lnRPDay1 >= lnRPDay2
  =gfModalGen('TRM40133B40011','ALERT','2nd|1st')
  lnRPDay1 = lnOldDays
ENDIF

*!*************************************************************
*! Name      : lfwDays
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the old value of days buckets in a variable
*!*************************************************************
*! Example     : = lfwDays()
*!*************************************************************
FUNCTION lfwDays

lnOldDays = EVALUATE(SYS(18))
*!*************************************************************
*! Name      : lfvDay2
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : to validate the number of days for the 2nd bucket
*!*************************************************************
*! Example     : = lfvDay2()
*!*************************************************************
FUNCTION lfvDay2

IF lnRPDay2 <= lnRPDay1
  =gfModalGen('TRM40133B40011','ALERT','2nd|1st')
  lnRPDay2 = lnOldDays
ENDIF
IF lnRPDay2 >= lnRPDay3
  =gfModalGen('TRM40133B40011','ALERT','3rd|2nd')
  lnRPDay2 = lnOldDays
ENDIF
*!*************************************************************
*! Name      : lfvDay3
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : to validate the number of days for the 3rd bucket
*!*************************************************************
*! Example     : = lfvDay3()
*!*************************************************************
FUNCTION lfvDay3

IF lnRPDay3 <= lnRPDay2
  =gfModalGen('TRM40133B40011','ALERT','3rd|2nd')
  lnRPDay3 = lnOldDays
ENDIF
IF lnRPDay3 >= lnRPDay4
  =gfModalGen('TRM40133B40011','ALERT','3rd|2nd')
  lnRPDay3 = lnOldDays
ENDIF
*!*************************************************************
*! Name      : lfvDay4
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : to validate the number of days for the 4th bucket
*!*************************************************************
*! Example     : = lfvDay4()
*!*************************************************************
FUNCTION lfvDay4

IF lnRPDay4 <= lnRPDay3
  =gfModalGen('TRM40133B40011','ALERT','4th|3rd')
  lnRPDay4 = lnOldDays
ENDIF

*!*************************************************************
*! Name      : lfGetDesc
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the description of codes
*!*************************************************************
*! Example     : = lfGetDesc()
*!*************************************************************
*B801903 MAB 01/24/1999 Add code before description.
*!Due to E301214,1 this function is no longer in use.
*
FUNCTION lfGetDesc
PRIVATE lcCodeDesc

lcCodeDesc = SPACE(01)
DO CASE
  CASE lcGroup = 'cTermCode'

    *B801903 Add code before description.
    *lcCodeDesc = gfCoddes(cTermCode,'CTERMCODE')
    lcCodeDesc = cTermCode + ' ' + PADR(gfCoddes(cTermCode,'CTERMCODE'),23)
  
  CASE lcGroup = 'CDIVISION'

    *B801903 Add code before description.
    *lcCodeDesc = gfCoddes(CDIVISION,'CDIVISION')
    lcCodeDesc = CDIVISION + ' ' + PADR(gfCoddes(CDIVISION,'CDIVISION'),23)

  CASE lcGroup = 'cCurrCode'
    =SEEK(cCurrCode,'SYCCURR')

    *B801903 Add code before description.
    *lcCodeDesc = SYCCURR.CCURRDESC
    lcCodeDesc = cCurrCode + ' ' + PADR(SYCCURR.CCURRDESC,23)
    
  CASE lcGroup = 'Salesrep'  
    =SEEK(Salesrep,'Salesrep')
    lcCodeDesc = Salesrep.Name
    
  CASE lcGroup = 'Region'  

    *B801903 Add code before description.
    *lcCodeDesc = gfCoddes(REGION,'REGION')
    lcCodeDesc = REGION + ' ' + PADR(gfCoddes(REGION,'REGION'),23)

  CASE lcRpAgSort = 'U'
    *B602103 I do not know why she change group [Begin]
    *lcGroup = 'cCont_Code'
    *B602103 I do not know why she change group [End]

    =SEEK(cCont_Code,'SYCINT')

    *B801903 Add code before description.
    *lcCodeDesc = SYCINT.CCONT_DESC
    lcCodeDesc = cCont_Code + ' ' + PADR(SYCINT.CCONT_DESC,23)

ENDCASE
RETURN lcCodeDesc
*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the description of Curr.
*!*************************************************************
*! Example     : = lfvCurDisp()
*!*************************************************************
FUNCTION lfvCurDisp

=gfRepCur(.T., @lcRpCurr,@ldRpExDate ,lcRpTmpNam)
*-- end of lfvCurDisp.

*!*************************************************************
*! Name      : lfGetRate
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the rate of Curr. in case of using multi curr.
*!*************************************************************
*! Example     : = lfGetRate()
*!*************************************************************
*!Due to E301214,1 this function is no longer in use.
FUNCTION lfGetRate
PARAMETERS lcfCurr
PRIVATE lcfCurr,lnfRate

lnfRate = 0
DO CASE
  CASE lcRpCurr = 'D'
    lnfRate = gfchkrate('',lcfCurr,ldEXDate,.F.,gcAct_Comp,.T.)
  CASE lcRpCurr = 'U'
    =SEEK(lcfCurr,lcRpTmpCur)
    lnfRate = &lcRpTmpCur..nExRate
ENDCASE
lnfRate = IIF(lnfRate = 0,1,lnfRate)
RETURN lnfRate

*!*************************************************************
*! Name      : lfRepName
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get salesrep name
*!*************************************************************
*! Example     : = lfRepName()
*!*************************************************************
*!Due to E301214,1 this function is no longer in use.
FUNCTION lfRepName
PARAMETER lcRepCode
PRIVATE lcRepCode,lcRepName
lcRepName = IIF(SEEK(lcRepCode,'SALESREP'),SALESREP.NAME,SPACE(24))
RETURN lcRepName

*!*************************************************************
*! Name      : lfSumCre
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the total credit for all the report
*!*************************************************************
*! Example     : = lfSumCre()
*!*************************************************************
*!Due to E301214,1 this function is no longer in use.
FUNCTION lfSumCre
PARAMETER lcAccount,lcCurr
PRIVATE lnCredit,lnAlias,lcCurr

lnAlias = SELECT()
lnCredit = 0
IF SEEK(&lcPrtFile..ACCOUNT,'CREDIT')
  SELECT CREDIT
  SCAN WHILE (ACCOUNT = &lcPrtFile..ACCOUNT) FOR IIF(!EMPTY(lcCurr),CCURRCODE=lcCurr,.T.)
    IF llMultCur AND lcRpCurr <> 'F'
      lcSign   = gfGetExSin("",ALLTRIM(CREDIT.CCURRCODE))
      lcSign   = IIF(EMPTY(lcSign),'*',lcSign)
      lnRate   = IIF(lcRpCurr = 'O',nExRate,lfGetRate(CREDIT.CCURRCODE))
      lnAmount = IIF(lcSign='/',AMOUNT/lnRate/nCurrUnit,AMOUNT*lnRate/nCurrUnit)
    ELSE
      lnAmount = AMOUNT
    ENDIF  
    lnCredit   = lnCredit + lnAmount
  ENDSCAN
ENDIF  
IF SEEK(&lcPrtFile..ACCOUNT,'ARHIST') AND llRPKeyOff
  SELECT ARHIST
  
  *B802124,1 MANMAB Include date range in scan filter [Begin]
  *SCAN WHILE (ACCOUNT = &lcPrtFile..ACCOUNT);
   FOR TRANTYPE $ '0456' AND IIF(!EMPTY(lcCurr),CCURRCODE=lcCurr,.T.);
   AND IIF(!EMPTY(lcRpGlAcc),cArGlAcc = lcRpGlAcc,.T.)

  SCAN WHILE (ACCOUNT = &lcPrtFile..ACCOUNT);
   FOR (HISTDATE > ldRpEndDat) .AND. (TRANDATE <= ldRpEndDat);
   and TRANTYPE $ '0456' AND IIF(!EMPTY(lcCurr),CCURRCODE=lcCurr,.T.);
   AND IIF(!EMPTY(lcRpGlAcc),cArGlAcc = lcRpGlAcc,.T.)
  *B802124,1 Include date range in scan filter [End  ]

    IF llMultCur AND lcRpCurr <> 'F'
      lcSign   = gfGetExSin("",ALLTRIM(ARHIST.CCURRCODE))
      lcSign   = IIF(EMPTY(lcSign),'*',lcSign)
      lnRate   = IIF(lcRpCurr = 'O',nExRate,lfGetRate(ARHIST.CCURRCODE))
      lnAmount = IIF(lcSign='/',AMOUNT/lnRate/nCurrUnit,AMOUNT*lnRate/nCurrUnit)
    ELSE
      lnAmount = AMOUNT
    ENDIF  
    lnCredit   = lnCredit + lnAmount
  ENDSCAN
ENDIF  
SELECT(lnAlias)
RETURN lnCredit

*!*************************************************************
*! Name      : lfCharge
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the total charge back for all the report
*!*************************************************************
*! Example     : = lfCharge()
*!*************************************************************
*!Due to E301214,1 this function is no longer in use.
FUNCTION lfCharge
PARAMETER lcAccount,lcCurr
PRIVATE lnDebit,lnAlias,lnAR,lcCurr

lnAlias = SELECT()
lnDebit = 0
IF SEEK(&lcPrtFile..ACCOUNT,'DEBIT')
  SELECT DEBIT
  SCAN WHILE (ACCOUNT = &lcPrtFile..ACCOUNT);
       FOR (IIF(!EMPTY(lcCurr),CCURRCODE=lcCurr,.T.) AND TRANTYPE <> '3')
    IF llMultCur AND lcRpCurr <> 'F'
      lcSign   = gfGetExSin("",ALLTRIM(DEBIT.CCURRCODE))
      lcSign   = IIF(EMPTY(lcSign),'*',lcSign)
      lnRate   = IIF(lcRpCurr = 'O',nExRate,lfGetRate(DEBIT.CCURRCODE))
      lnAmount = IIF(lcSign='/',AMOUNT/lnRate/nCurrUnit,AMOUNT*lnRate/nCurrUnit)
    ELSE
      lnAmount = AMOUNT
    ENDIF  
    lnDebit   = lnDebit + lnAmount
  ENDSCAN
ENDIF  
IF SEEK(&lcPrtFile..ACCOUNT,'ARHIST') AND llRPKeyOff
  SELECT ARHIST
  SCAN WHILE (ACCOUNT = &lcPrtFile..ACCOUNT);
       FOR TRANTYPE $ '12' AND IIF(!EMPTY(lcCurr),CCURRCODE=lcCurr,.T.);
       AND IIF(!EMPTY(lcRpGlAcc),cArGlAcc = lcRpGlAcc,.T.)
    IF llMultCur AND lcRpCurr <> 'F'
      lcSign   = gfGetExSin("",ALLTRIM(ARHIST.CCURRCODE))
      lcSign   = IIF(EMPTY(lcSign),'*',lcSign)
      lnRate   = IIF(lcRpCurr = 'O',nExRate,lfGetRate(ARHIST.CCURRCODE))
      lnAmount = IIF(lcSign='/',AMOUNT/lnRate/nCurrUnit,AMOUNT*lnRate/nCurrUnit)
    ELSE
      lnAmount = AMOUNT
    ENDIF  
    lnDebit   = lnDebit + lnAmount
  ENDSCAN
ENDIF  
SELECT(lnAlias)
RETURN lnDebit

*!*************************************************************
*! Name      : lfSumCharge
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the total charge back for all the report
*!*************************************************************
*! Example     : = lfSumCharge()
*!*************************************************************
*!Due to E301214,1 this function is no longer in use.
FUNCTION lfSumCharge
PRIVATE lnDebit,lnAlias,lnRecNo

lnAlias = SELECT()
SELECT(lcPrtFile)
SET RELATION OFF INTO (lcTmpTran)
lnRecNo = RECNO()
SUM CHGBACK TO lnDebit
SELECT(lnAlias)
RETURN lnDebit

*!*************************************************************
*! Name      : lfCharge
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the total charge back for all the report
*!*************************************************************
*! Example     : = lfCharge()
*!*************************************************************
*!Due to E301214,1 this function is no longer in use.
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

FOR lnCount = 1 TO 5
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*!*************************************************************
*! Name      : lfGetAdd
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the address of the customer
*!*************************************************************
*! Example     : = lfGetAdd()
*!*************************************************************
*!Due to E301214,1 this function is no longer in use.
FUNCTION lfGetAdd
laSoldTo[1] = gfGetAdr(lcPrtFile , '' , '' , '' , 1 , 1)
laSoldTo[2] = gfGetAdr(lcPrtFile , '' , '' , '' , 2 , 1)
laSoldTo[3] = gfGetAdr(lcPrtFile , '' , '' , '' , 3 , 1)
laSoldTo[4] = gfGetAdr(lcPrtFile , '' , '' , '' , 4 , 1)
laSoldTo[5] = gfGetAdr(lcPrtFile , '' , '' , '' , 5 , 1)
=lfAdrShift('laSoldTo')
RETURN ''

*!*************************************************************
*! Name      : lfGetNotes
*! Developer : Samah Wilson (SWK)
*! Date      : 06/30/1998
*! Purpose   : get the notes of the customer
*!*************************************************************
*! Example     : = lfGetNotes()
*!*************************************************************
FUNCTION lfGetNotes

lcNotes = [IIF(llRPCusNot .AND.]+;
          [SEEK('A'+ ACCOUNT,'NOTEPAD') .AND.]+;
          [!EMPTY(NOTEPAD.mNotes),]+; 
          [ALLTRIM(NOTEPAD.mNotes) + ] +;
          [IIF(!CHR(13) $ RIGHT(NOTEPAD.mNotes,2), CHR(13),'') , '')]
*REPLACE &lcPrtFile..LLOK_STAT WITH .T.
RETURN EVALUATE(lcNotes)
*!*************************************************************

*!*************************************************************
*! Name      : lfDefCurr
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/11/1998
*! Purpose   : Return Default currency value.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : currency default value.
*!*************************************************************
*! Example     : = lfDefCurr()
*!*************************************************************
*!B801811,1 MAB
*!
FUNCTION lfDefCurr

RETURN IIF(llMultCur,'F','O')
*-- end of lfDefCurr.

*!*************************************************************
*! Name      : lfFillSort
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/11/1998
*! Purpose   : Fill sort by arrays.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfFillSort()
*!*************************************************************
*!B801811,1 MAB
*!
FUNCTION lfFillSort
PRIVATE lnRows
lnRows = IIF(llMultCur,7,6)
DIMENSION laSortDesc[lnRows,1],laSortVal[lnRows,1]

laSortDesc[1,1] = 'Account'
laSortDesc[2,1] = 'Terms'
laSortDesc[3,1] = 'Salesrep'
laSortDesc[4,1] = 'Division'
laSortDesc[5,1] = 'Region'
laSortDesc[6,1] = 'Country'

laSortVal[1,1] = 'A'
laSortVal[2,1] = 'T'
laSortVal[3,1] = 'S'
laSortVal[4,1] = 'D'
laSortVal[5,1] = 'R'
laSortVal[6,1] = 'U'

*-- if company support multi currency add Currency to Sort By POPUP.
IF llMultCur
  =AINS(laSortDesc,5)
  =AINS(laSortVal,5)
  laSortDesc[5,1] = 'Currency'
  laSortVal[5,1]  = 'C'
ENDIF  && end if company support multi currency add Currency to Sort By POPUP.
*-- end of lfFillSort.

*!*************************************************************
*! Name      : lfFilTrnAr
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Create temporary cursor structure.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfWorkFile
*!*************************************************************
*! Called from : OG When function.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfFilTrnAr()
*!*************************************************************
*!Due to E301214,1
*
FUNCTION lfFilTrnAr
PRIVATE laTempTran , laTempCust , lcExcStat
lcExcStat = SET('EXACT')
SET EXACT ON

*-- laTempStru : Array hold structure of temporary transaction + Customer fields.
*-- laTempTran : Array hold structure of transaction fields.
*-- laTempCust : Array hold structure of Customer fields.

*C102178,1 BWA 01/23/2000 2) Print the notepad of the invoices only and this task customize for USA office.[START]
*Modify the dimension to take 2 more fields
*DIMENSION laTempStru[36,4] , laTempTran[1,4] , laTempCust[1,4]

*B606890,1 KHM 05/19/2003 (Begin) Increase laTempStru to add a new field to hold codes descrp.
DIMENSION laTempStru[39,4] , laTempTran[1,4] , laTempCust[1,4]
*B606890,1 KHM 05/19/2003 (End)

*C102178,1 [END]

STORE '' TO laTempStru,laTempTran
PRIVATE lnFileCnt , lnFldRow

*-- Fields from Customer File.
SELECT CUSTOMER
= AFIELDS(laTempCust)
laTempStru[1,1]  = 'ACCOUNT'
laTempStru[2,1]  = 'CFACCODE'
laTempStru[3,1]  = 'SALESREP'
laTempStru[4,1]  = 'CURRENT'
laTempStru[5,1]  = 'AGE30'
laTempStru[6,1]  = 'AGE60'
laTempStru[7,1]  = 'AGE90'
laTempStru[8,1]  = 'AGE120'
laTempStru[9,1]  = 'TOTAGE'
laTempStru[10,1] = 'OPENCR'
laTempStru[11,1] = 'CHGBACK'
laTempStru[12,1] = 'NETBAL'
laTempStru[13,1] = 'REGION'
laTempStru[14,1] = 'CTERMCODE'
laTempStru[15,1] = 'CDIVISION'
laTempStru[16,1] = 'CCURRCODE'
laTempStru[17,1] = 'CCONT_CODE'

*-- Loop to get other dimensions of Customer included fields (Like master file)
FOR lnFileCnt = 1 TO 17
  lnFldRow = ASCAN(laTempCust,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempCust,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTempCust[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTempCust[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTempCust[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of Customer included fields (Like master file)

*-- Fields from Debit , Credit , and History Files
SELECT ARHIST
= AFIELDS(laTempTran)
laTempStru[18,1] = 'TRANTYPE'
laTempStru[19,1] = 'TRANCODE'
laTempStru[20,1] = 'TRAN'
laTempStru[21,1] = 'BATCH'
laTempStru[22,1] = 'CINSTALNO'
laTempStru[23,1] = 'TRANDATE'
laTempStru[24,1] = 'DPOSTDATE'
laTempStru[25,1] = 'CHGBK_DATE'
laTempStru[26,1] = 'DESC'
laTempStru[27,1] = 'REFERENCE'
laTempStru[28,1] = 'AMOUNT'
laTempStru[29,1] = 'DUEDATE'
laTempStru[30,1] = 'CADJACCT'
laTempStru[31,1] = 'CARGLACC'
laTempStru[32,1] = 'NCURRUNIT'
laTempStru[33,1] = 'NEXRATE'

*-- Loop to get other dimensions of transaction included fields (Like master file)
FOR lnFileCnt = 18 TO 33
  lnFldRow = ASCAN(laTempTran,laTempStru[lnFileCnt,1])
  IF lnFldRow > 0
    lnFldRow = ASUBSCRIPT(laTempTran,lnFldRow,1)
    laTempStru[lnFileCnt , 2 ] = laTempTran[lnFldRow , 2 ]
    laTempStru[lnFileCnt , 3 ] = laTempTran[lnFldRow , 3 ]
    laTempStru[lnFileCnt , 4 ] = laTempTran[lnFldRow , 4 ]
  ENDIF
ENDFOR  && end Loop to get other dimensions of transaction included fields (Like master file)

*-- Add Fields from PostDchq File.
*-- cTranType field : Added to indicate that post dated if it's "P" else normal transaction.
laTempStru[34 ,1] = 'CTRANTYPE'
laTempStru[34 ,2] = 'C'
laTempStru[34 ,3] = 1
laTempStru[34 ,4] = 0

laTempStru[35 ,1] = 'CHEQUENO'
laTempStru[35 ,2] = 'C'
laTempStru[35 ,3] = 10
laTempStru[35 ,4] = 0

*-- Temporary Key Field.

laTempStru[36 ,1] = 'CTEMPKEY'
laTempStru[36 ,2] = 'C'
*B607446,1 KHM 07/27/2003 (Begin) Increase the width of the field.
*laTempStru[36 ,3] = 15
laTempStru[36 ,3]  = 50
*B607446,1 KHM 07/27/2003 (End)

laTempStru[36 ,4] = 0

*C102178,1 BWA 01/23/2000 2) Print the notepad of the invoices only and this task customize for USA office.[START]
*-- Field for the MNOTES.
laTempStru[37 ,1] = 'mNotes'
laTempStru[37 ,2] = 'M'
laTempStru[37 ,3] = 10
laTempStru[37 ,4] = 0

*-- Field for the MNOTES.
laTempStru[38 ,1] = 'llFlgPad'
laTempStru[38 ,2] = 'L'
laTempStru[38 ,3] = 1
laTempStru[38 ,4] = 0
*C102178,1 [END]

*B606890,1 KHM 05/19/2003 (Begin) Adding the codes description field.
laTempStru[39 ,1] = 'cCodeDescr'
laTempStru[39 ,2] = 'C'
laTempStru[39 ,3] = 30
laTempStru[39 ,4] = 0
*B606890,1 KHM 05/19/2003 (End)

SET EXACT &lcExcStat  && Restore exact setting.

=lfWorkFile()  && Create temporary files.
*-- end of lfFilTrnAr.

*!*************************************************************
*! Name      : lfWorkFile
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/99
*! Purpose   : Create work cursors.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfOpenFile
*!*************************************************************
*! Called from : lfFilTrnAr , Report code.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfWorkFile()
*!*************************************************************
*!Due to E301214,1
*
FUNCTION lfWorkFile

*-- if company use multi currency.
IF llMultCur

  *-- Open Currency file.
  IF !USED('SYCCURR')
    =gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  ENDIF  

ELSE  && else company use single currency format.
  
  *-- Open Post dated cheques file.
  IF !USED('POSTDCHQ')
    USE (gcDataDir+'POSTDCHQ') ORDER TAG POSTDCHQ IN 0 SHARED
  ENDIF

ENDIF  && end if company use multi currency.

SET ORDER TO CUSTOMER IN CUSTOMER
SET ORDER TO TAG Ccontcode IN SYCINT

*-- First Close all temporary files if it was opened before [Begin]
IF USED(lcTempAge)
  USE IN (lcTempAge)
ENDIF

IF USED(lcTempRev)
  USE IN (lcTempRev)
ENDIF

IF USED(lcTempAcc)
  USE IN (lcTempAcc)
ENDIF
*-- First Close all temporary files if it was opened before [End  ]

*-- Create Totals file [Begin]
*-- nAges   : Numeric Field Hold Totals for Age30 + Age60 + Age90 + Age120 (Past Due Ages only)
*-- nNetBal : Numeric Field Hold Net balance for account in base currency.
CREATE CURSOR (lcTempAcc) (Account C(5) , nAges N(11,2) , nNetBal N(11,2))
SELECT (lcTempAcc)

*-- Fix Cursor bug [Begin]
ZAP
*-- Fix Cursor bug [End  ]
INDEX ON ACCOUNT TAG (lcTempAcc) OF (gcWorkDir+lcTempAcc+'.CDX')
*-- Create Totals file [End  ]

*-- Create transaction lines file [Begin]
CREATE CURSOR (lcTempAge) ;
   FROM ARRAY laTempStru
SELECT (lcTempAge)

*-- Fix Cursor bug [Begin]
ZAP
*-- Fix Cursor bug [End  ]

*-- First  index used in reverse sorting in short form to detect end of group .
*-- Second index used in printing according sort by .
INDEX ON cTempKey TAG (lcTempRev) OF (gcWorkDir+lcTempAge+'.CDX') DESCENDING
INDEX ON cTempKey TAG (lcTempAge) OF (gcWorkDir+lcTempAge+'.CDX') 
*-- Create transaction lines file [End  ]
*-- end of lfWorkFile.

*!*************************************************************
*! Name      : lfvSortBy
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 04/05/99
*! Purpose   : change index flag to reindex temp cursor.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvSortBy()
*!*************************************************************
FUNCTION lfvSortBy
llChSortBy = .T.  && Detect that user change sorting method.
*-- end of lfvSortBy.

*!*************************************************************
*! Name      : lfGetReplc
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/04/99
*! Purpose   : Get Replaced expression.(According to sort by options)
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : lcExpr ---> which means (Sort by expression) 
*!*************************************************************
*! Example   : =lfGetReplc()
*!*************************************************************
*!Due to E301214,1
*
FUNCTION lfGetReplc
PRIVATE lcExpr
DO CASE
  *-- Sort by account
  CASE lcRpAgSort = 'A'
    lcGroup  = []  && Used in short form only.
    lcInnGrp = [Account]
    lcExpr   = [ACCOUNT + cTranType]
    lcSorted = 'Account'
    
  *-- Sort by Terms
    CASE lcRpAgSort = 'T'
    lcGroup  = [cTermCode]
    lcInnGrp = [cTermCode + Account]

    *E301238,1 Call gfCodDes with new .T. parameter [Begin]
    *lcCodeDesc = [cTermCode + '-' + gfCoddes(cTermCode,'CTERMCODE') + ' Totals ==>']
    lcCodeDesc = [gfCoddes(cTermCode,'CTERMCODE',.T.) + ' Totals ==>']
    *E301238,1 Call gfCodDes with new .T. parameter [End  ]

    *B606890,1 KHM 05/19/2003 (Begin) Sorting by codes description instead of cTermCode.
    *lcExpr   = [CTERMCODE + Account + cTranType]
    lcExpr   = [cCodeDescr + Account + cTranType]
    *B606890,1 KHM 05/19/2003 (End)
    lcSorted = 'Term Code'
    
  *-- Sort by division
  CASE lcRpAgSort = 'D'
    lcGroup  = [CDIVISION]
    lcInnGrp = [CDIVISION + Account]

    *E301238,1 Call gfCodDes with new .T. parameter [Begin]
    *lcCodeDesc = [CDIVISION + '-' + gfCoddes(CDIVISION,'CDIVISION') + ' Totals ==>']
    lcCodeDesc = [gfCoddes(CDIVISION,'CDIVISION',.T.) + ' Totals ==>']
    *E301238,1 Call gfCodDes with new .T. parameter [End  ]

    lcExpr   = [CDIVISION + Account + cTranType]
    lcSorted = 'Division'
    
  *-- Sort by currency
  CASE lcRpAgSort = 'C'
    lcGroup  = [cCurrCode]
    lcInnGrp = [cCurrCode + Account]
    lcCodeDesc = [cCurrCode + '-' + IIF(SEEK(cCurrCode,'SYCCURR'),SYCCURR.CCURRDESC,SPACE(30)) + ' Totals ==>']

    lcExpr   = [CCURRCODE + Account + cTranType]
    lcSorted = 'Currency'
    
  *-- Sort by sales rep.
  CASE lcRpAgSort = 'S'
    lcGroup  = [Salesrep]
    lcInnGrp = [Salesrep + Account]
    lcCodeDesc = [IIF(SEEK(Salesrep,'Salesrep'),Salesrep.Name,SPACE(24)) + ' Totals ==>']

    lcExpr   = [Salesrep + Account + cTranType]
    lcSorted = 'Sales Representative'

  *-- Sort by region
  CASE lcRpAgSort = 'R'
    lcGroup  = [Region]
    lcInnGrp = [Region + Account]

    *E301238,1 Call gfCodDes with new .T. parameter [Begin]
    *lcCodeDesc = [REGION + '-' + gfCoddes(REGION,'REGION') + ' Totals ==>']
    lcCodeDesc = [gfCoddes(REGION,'REGION',.T.) + ' Totals ==>']
    *E301238,1 Call gfCodDes with new .T. parameter [End  ]

    lcExpr   = [Region + Account + cTranType]
    lcSorted = 'Region'
    
  *-- Sort by country.
  CASE lcRpAgSort = 'U'
    lcGroup  = [cCont_Code]
    lcInnGrp = [cCont_Code + Account]
    lcCodeDesc = [cCont_Code + '-' + IIF(SEEK(cCont_Code,'SYCINT'),SYCINT.CCONT_DESC,SPACE(30)) + ' Totals ==>']

    lcExpr   = [cCont_Code + Account + cTranType]
    lcSorted = 'Country'

ENDCASE

*-- if multi currency and print in foreign and not sort by currency.
*-- include currency in your groups, and index filter.
IF llCurrGrp AND (LEFT(lcExpr,9) != 'CCURRCODE')
  lcExpr   = lcExpr   + [ + CCURRCODE]
  lcGroup  = lcGroup  + [ + CCURRCODE]
  lcInnGrp = lcInnGrp + [ + CCURRCODE]
ENDIF  && end if multi currency and print in foreign and not sort by currency.

RETURN lcExpr
*-- end of lfGetReplc

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/04/99
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*! Example   : =lfCollTime()
*!*************************************************************
*!Due to E301214,1
*
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- end of lfCollTime.

*!*************************************************************
*! Name      : lfvEndDate
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Valid function for Ending period.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfOGShowGet
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : .T.
*!*************************************************************
*! Example   : =lfvEndDate()
*!*************************************************************
*!Due to E301214,1
*
FUNCTION lfvEndDate
llRpKeyOff = (ldRpEndDat < gdSysDate)  && Key off value (Yes or No)
=lfOGShowGet('llRpKeyOff')
RETURN .T.
*-- end of lfvEndDate.

*!*************************************************************
*! Name      : lfvIncDet
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Valid function for Include credit detail.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfOGShowGet
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : .T.
*!*************************************************************
*! Example   : =lfvEndDate()
*!*************************************************************
*!Due to E301214,1
*
FUNCTION lfvIncDet
lnAgeCrd = ASCAN(laOGObjType,'llRpAgeCrd')
IF lnAgeCrd != 0
  lnAgeCrd = ASUBSCRIPT(laOGObjType,lnAgeCrd,1)
  STORE llRpCrdDet TO laOGObjCnt[lnAgeCrd] , llRpAgeCrd
  = lfOGShowGet('llRpAgeCrd')
ENDIF
*-- end of lfvIncDet.

*!*************************************************************
*! Name      : lfCollect
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Collect new data in two temporary cursor One is Transaction cursor
*!           : and another if for totals in base currency to handle due past only
*!           : and Over/Under Filters...
*!           : Note, Technique used here is to scan transcation files to get there transcations
*!           :       If achive customer filter in customer file.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfInsPstCh , lfInsDebit , lfInsCredt , lfInsHisto
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : DO lfCollect
*!*************************************************************
*!Due to E301214,1
*
FUNCTION lfCollect
*-- Define collect variables [Begin]
*-- lcFacFlt : Variable Hold Factor/Non-Factor/Both Filter.
*-- lcDebFlt : Variable Hold Debit main Filter.
*-- lcCreFlt : Variable Hold Credit main Filter.
*-- lcHstFlt : Variable Hold History main Filter.
PRIVATE lcDebFlt, lcCreFlt, lcHstFlt , lcFacFlt, lcGlAccFlt
STORE '' TO lcDebFlt, lcCreFlt, lcHstFlt, lcFacFlt
*-- Define collect variables [End  ]

*-- if print Factored only
IF lcRpFactor = 'F'

  lcFacFlt = IIF(EMPTY(lcRPSpeFac),[!EMPTY(cFacCode)],[cFacCode=lcRPSpeFac])

  lcDebFlt = [TRANDATE<=ldRpEndDat AND ]                          +;
             [IIF( TRANTYPE='3',CHGBK_DATE<=ldRpEndDat,.T.)]

  lcCreFlt = [TRANDATE<=ldRPEndDat AND ]                          +;
             [IIF( TRANTYPE='6',CREDT_DATE<=ldRPEndDat,.T.)]

ELSE && Factored (and/or) non Factored

  IF lcRpFactor='N'
    lcFacFlt = [EMPTY(cFacCode)]
  ENDIF

  lcDebFlt = [IIF( TRANTYPE='3',]                                +; 
             [TRANDATE<=ldRpEndDat AND CHGBK_DATE<=ldRpEndDat ,] +;
             [TRANDATE<=ldRpEndDat )]

  lcCreFlt = [IIF( TRANTYPE='6',]                                +;
             [TRANDATE<=ldRPEndDat AND CREDT_DATE<=ldRPEndDat ,] +;
             [TRANDATE<=ldRPEndDat )]

ENDIF  && end if print Factored only.

*C200419,1 Add the custom division to filter. [Begin]
=gfDoTriger('ARAGING',PADR('COMPFILT',10))
*C200419,1 Add the custom division to filter. [End]

*B605247,1 Add the check for posting date in case of invoice. [Begin]
*C200419,1 SSE Commented out. [Begin]
*IF !EMPTY(ldPostDate)
*  lcDebFlt = lcDebFlt + IIF(EMPTY(lcDebFlt),'',[ AND ]) + [IIF(TranType='1',IIF(SEEK(Tran,'InvHdr'),ldPostDate >= InvHdr.dPostDate,.F.),.T.)]
*  lcCreFlt = lcCreFlt + IIF(EMPTY(lcCreFlt),'',[ AND ]) + [IIF(TranType='0',IIF(SEEK(Tran,'RetHdr'),ldPostDate >= RetHdr.dPostDate,.F.),.T.)]
*ENDIF
*C200419,1 SSE Commented out. [End]
*B605247,1 Add the check for posting date in case of invoice. [End]

lcHstFlt = [HISTDATE > ldRPEndDat AND TRANDATE <= ldRPEndDat]  && History (Keyed) filter

*C200419,1 Add the custom division to filter. [Begin]
IF !EMPTY(ldPostDate)
  lcDebFlt = lcDebFlt + IIF(EMPTY(lcDebFlt),'',[ AND ]) + [IIF(TranType='1',ldPostDate >= dPostDate,.T.)]
  lcCreFlt = lcCreFlt + IIF(EMPTY(lcCreFlt),'',[ AND ]) + [IIF(TranType='0',ldPostDate >= dPostDate,.T.)]
  lcHstFlt = lcHstFlt + IIF(EMPTY(lcHstFlt),'',[ AND ]) + [IIF(TranType $ '01',ldPostDate >= dPostDate,.T.)]
ENDIF
*C200419,1 Add the custom division to filter. [End]

*B605385,1 NAD (Start)
*B606993,1 NAD (sTART) Commented out
*lcHstFlt = lcHstFlt + IIF(EMPTY(lcHstFlt),'',[ AND ]) + [ldPostDate >= dPostDate]
*B606993,1 NAD (END) Commented out
*B605385,1 NAD (END) 
*-- Add Factor Filter to Main Files Filter.
IF !EMPTY(lcFacFlt)
  lcDebFlt = lcDebFlt + [ AND ] + lcFacFlt
  lcCreFlt = lcCreFlt + [ AND ] + lcFacFlt
  lcHstFlt = lcHstFlt + [ AND ] + lcFacFlt
ENDIF  

*-- Add GlAccount Filter to Main Files Filter.
IF !EMPTY(STRTRAN(lcRpGlAcc,'-',''))
  lcDebFlt = lcDebFlt + [ AND cArGlAcc = lcRpGlAcc]
  lcCreFlt = lcCreFlt + [ AND cArGlAcc = lcRpGlAcc]
  lcHstFlt = lcHstFlt + [ AND cArGlAcc = lcRpGlAcc]
ENDIF

*-- Declare Customer file memory variables [Begin]
SELECT CUSTOMER 
GO TOP
SCATTER MEMVAR MEMO BLANK  && Define aging variables
*-- Declare Customer file memory variables [End  ]

*E500378,1 BWA 10/04/2000 Add 2 filters to the criteria of the report for the customer status & balance.[START]

*C102178,1 BWA 01/23/2000 1) Change the selection grid for the customer to be mover to let the user select more than one choice.[START]
*lcRpExp = lcRpExp + ' AND ' + IIF(lcRpStat = 'L' , " CUSTOMER.STATUS $ 'AHX' " , " CUSTOMER.STATUS = lcRpStat" )
lcRpExp = lcRpExp + ' AND ' + " CUSTOMER.STATUS $ lcRpStatFr "
*C102178,1 [END]

*E500378,1 [END]

*C200419,1 This trigger is used also by RON20 (England).
*C200240,1 Add the custom region to lcRpExp. [Begin]
=gfDoTriger('ARAGING',PADR('COMPLEXP',10))
*C200240,1 Add the custom region to lcRpExp. [End]

*-- insert post date cheques Transactions if not multi currency company.
IF !llMultCur
  =lfInsPstCh()
ENDIF  

*-- Insert Debit Transactions ...
=lfInsDebit()

*-- Insert Credit Transactions ...
=lfInsCredt()

*-- If user want to include key off transactions.
IF llRpKeyOff
  *-- Insert keyed Transactions ...
  =lfInsHisto()
ENDIF  
*-- end of lfCollect.

*!*************************************************************
*! Name      : lfInsPstCh
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Insert post dated cheques records to Transaction File.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpInsRecrd
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfInsPstCh()
*!*************************************************************
*!Due to E301214,1
*
FUNCTION lfInsPstCh
*-- Set relation with customer file.
SELECT POSTDCHQ
SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER  && Relation with customer.

*-- Scan All post dated checks achieve customer filter.
SCAN FOR account+DTOS(paydate) = '' AND CUSTOMER.TYPE = 'M' AND &lcRpExp AND ;
         Amount <> 0

  SCATTER MEMVAR
  m.TranDate  = POSTDCHQ.PAYDATE       && check pay date
  m.Desc      = "PostDated Checks"     && Description
  m.TranType  = ' '                    && Empty transaction
  m.cTranType = 'P'  && This field Added to indicate that post dated if it's "P" else normal transaction.
  
  DO lpInsRecrd  && Insert New Record in Transaction Temporary File.

ENDSCAN  && end Scan All post dated checks achieve customer filter.

SET RELATION TO  && Rest relation.
*-- end of lfInsPstCh.

*!*************************************************************
*! Name      : lfInsDebit
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Insert Debit records to Transaction and total Files.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpDebDeal , lpInsRecrd
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfInsDebit()
*!*************************************************************
*!Due to E301214,1
*
FUNCTION lfInsDebit
*-- Set relation with customer file.
SELECT DEBIT
SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER  && Relation with customer.

*C605955,1 ABD - Set relation with InvHdr file. [Begin]
*B123160,1 BWA 06/10/2004 Change the index to be able to seek with the account and the transaction No#.[START]
*SET RELATION TO Tran INTO Invhdr ADDITIVE
PRIVATE lcOrdInvH , lcOldFile
lcOldFile = SELECT()
SELECT INVHDR
lcOrdInvH = ORDER('INVHDR')
SET ORDER TO TAG INVHDRA TO INVHDR
SELECT(lcOldFile)
SET RELATION TO ACCOUNT + TRAN INTO Invhdr ADDITIVE
*B123160,1 BWA 06/10/2004.[END]
*C605955,1 ABD - [End]

*-- Scan Debit records achieve both customer and debit filters.

*B802723,1 Do not zero amount
*SCAN FOR account+tran+cinstalno+DTOS(trandate) = '' AND &lcDebFlt AND ;
         CUSTOMER.TYPE = 'M' AND &lcRpExp

*C605955,1 ABD -  Valid the sales rep from the Invhdr file. [Begin]
*SCAN FOR account+tran+cinstalno+DTOS(trandate) = '' AND &lcDebFlt AND ;
         CUSTOMER.TYPE = 'M' AND &lcRpExp AND Amount <> 0
SCAN FOR account+tran+cinstalno+DTOS(trandate) = '' AND &lcDebFlt AND ;
         CUSTOMER.TYPE = 'M' AND &lcRpExp AND Amount <> 0 .AND. &lcRpSlsExp
  *C605955,1 ABD -  [End]

  *-- if user does not want to include charge back transaction and it is Charge back.
  IF (!llRPPrnCh .AND. TranType = '3' )
    LOOP
  ENDIF

  SCATTER MEMVAR
  DO lpDebDeal   && Calculate Ages and fill memory variables (Totage and NetBal)
  DO lpInsRecrd  && Insert New Record in Transaction Temporary File and Totals Temporary file.

ENDSCAN  && end Scan Debit records achieve both customer and debit filters.

*B123160,1 BWA 06/10/2004 Restore the original values.[START]
lcOldFile = SELECT()
SELECT INVHDR
SET ORDER TO TAG &lcOrdInvH TO INVHDR
SELECT(lcOldFile)
*B123160,1 BWA 06/10/2004.[END]

SET RELATION TO  && Rest relation.
*-- end of lfInsDebit.

*!*************************************************************
*! Name      : lfInsCredt
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Insert Credit records to Transaction and total Files.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpCreDeal , lpInsRecrd
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfInsCredt()
*!*************************************************************
*!Due to E301214,1
*
FUNCTION lfInsCredt
*-- Set relation with customer file.
SELECT CREDIT
SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER  && Relation with customer.

*C605955,1 ABD - Set relation with InvHdr file. [Begin]
*B123160,1 BWA 06/10/2004 Change the index to be able to seek with the account and the transaction No#.[START]
*SET RELATION TO Tran INTO Invhdr ADDITIVE
PRIVATE lcOrdInvH , lcOldFile
lcOldFile = SELECT()
SELECT INVHDR
lcOrdInvH = ORDER('INVHDR')
SET ORDER TO TAG INVHDRA TO INVHDR
SELECT(lcOldFile)
SET RELATION TO ACCOUNT + TRAN INTO Invhdr ADDITIVE
*B123160,1 BWA 06/10/2004.[END]
*C605955,1 ABD - [End]

*-- Scan Credit records achieve both customer and Credit filters.

*B802723,1 Do not zero amount
*SCAN FOR account+tran+DTOS(trandate) = '' AND &lcCreFlt AND ;
         CUSTOMER.TYPE = 'M' AND &lcRpExp
*C605955,1 ABD -  Valid the sales rep from the Invhdr file. [Begin]
*SCAN FOR account+tran+DTOS(trandate) = '' AND &lcCreFlt AND ;
         CUSTOMER.TYPE = 'M' AND &lcRpExp AND Amount <> 0
SCAN FOR account+tran+DTOS(trandate) = '' AND &lcCreFlt AND ;
         CUSTOMER.TYPE = 'M' AND &lcRpExp AND Amount <> 0 .AND. &lcRpSlsExp
  *C605955,1 ABD -  [End]

  SCATTER MEMVAR
  DO lpCreDeal   && Calculate Ages and fill memory variables (OpenCr and NetBal)
  DO lpInsRecrd  && Insert New Record in Transaction Temporary File and Totals Temporary file.


ENDSCAN  && end Scan Credit records achieve both customer and Credit filters.

*B123160,1 BWA 06/10/2004 Restore the original values.[START]
lcOldFile = SELECT()
SELECT INVHDR
SET ORDER TO TAG &lcOrdInvH TO INVHDR
SELECT(lcOldFile)
*B123160,1 BWA 06/10/2004.[START]

SET RELATION TO  && Rest relation.

*-- end of lfInsCredt.

*!*************************************************************
*! Name      : lfInsHisto
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Insert History records to Transaction and total Files.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpDebDeal , lpCreDeal , lpInsRecrd
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfInsHisto()
*!*************************************************************
*!Due to E301214,1
*
FUNCTION lfInsHisto
*-- Set relation with customer file.
SELECT ARHIST
SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER  && Relation with customer.

*C605955,1 ABD - Set relation with InvHdr file. [Begin]
*B123160,1 BWA 06/10/2004 Change the index to be able to seek with the account and the transaction No#.[START]
*SET RELATION TO Tran INTO Invhdr ADDITIVE
PRIVATE lcOrdInvH , lcOldFile
lcOldFile = SELECT()
SELECT INVHDR
lcOrdInvH = ORDER('INVHDR')
SET ORDER TO TAG INVHDRA TO INVHDR
SELECT(lcOldFile)
SET RELATION TO ACCOUNT + TRAN INTO Invhdr ADDITIVE
*B123160,1 BWA 06/10/2004.[END]
*C605955,1 ABD - [End]


*-- Scan History records achieve both customer and History filters.

*B802723,1 Do not zero amount
*SCAN FOR account+tran+cinstalno = '' AND &lcHstFlt AND ;
         CUSTOMER.TYPE = 'M' AND &lcRpExp
*C605955,1 ABD -  Valid the sales rep from the Invhdr file. [Begin]
*SCAN FOR account+tran+cinstalno = '' AND &lcHstFlt AND ;
         CUSTOMER.TYPE = 'M' AND &lcRpExp AND Amount <> 0
SCAN FOR account+tran+cinstalno = '' AND &lcHstFlt AND ;
         CUSTOMER.TYPE = 'M' AND &lcRpExp AND Amount <> 0 .AND. &lcRpSlsExp
  *C605955,1 ABD -  [End]
  
  *-- IF user does not want to include charge back transaction and it is Charge back (Type=3),
  *-- OR charge back date greater than ending period OR it is credit on account transaction
  *-- (Type=6) AND credit date greater than ending period, then
  *-- Cancel this transaction [i.e. LOOP it]
  
  *B607360,1 KHM 06/26/2003 (Begin) Include the material invoice (TranType="M").
  *IF (!llRPPrnCh .AND. TranType = '3' )            OR ;
     (TRANTYPE = '3' .AND. CHGBK_DATE>ldRPEndDat)  OR ;
     (TRANTYPE = '6' .AND. CREDT_DATE>ldRPEndDat)  OR ;
     !(TRANTYPE $ "0123456")
  IF (!llRPPrnCh .AND. TranType = '3' )            OR ;
     (TRANTYPE = '3' .AND. CHGBK_DATE>ldRPEndDat)  OR ;
     (TRANTYPE = '6' .AND. CREDT_DATE>ldRPEndDat)  OR ;
     !(TRANTYPE $ "0123456M")
  *B607360,1 KHM 06/26/2003 (End)
    LOOP
  ENDIF

  SCATTER MEMVAR

  *====> Debit Transaction  <====*
  *-- if Invoice(Type=1), Debit Adjustment(Type=2) OR Charge back(Type=3) .

  *B607360,1 KHM 06/26/2003 (Begin) Include the material invoice (TranType="M").
  *IF TRANTYPE $ '123'
  IF TRANTYPE $ '123M'
  *B607360,1 KHM 06/26/2003 (End)
    DO lpDebDeal    && Calculate Ages and fill memory variables (Totage and NetBal)
  ENDIF

  *====> Credit Transaction  <====*
  *-- if Return Invoice(Type=0), Payment(Type=4), Credit Adjustment(Type=5)
  *-- , OR Credit on account(Type=6).
  IF TRANTYPE $ '0456'
    DO lpCreDeal   && Calculate Ages and fill memory variables (OpenCr and NetBal)
  ENDIF

  *B603353,1 BWA 08/08/2000 Show the key off transactions under there title.[START]
  *B603934,1 BWA 10/01/2000 Fix the bug of Total balance in Detail report doesn't match in short report.[START]
  *m.cTranType = 'H'  && This field Added to indicate history transactions.
  IF lcRpReport = 'D'            && In case the user select the detail report.
    m.cTranType = 'H'  && This field Added to indicate history transactions.
  ENDIF
  *B603934,1 [END]

  *B603353,1 [END]
  
  DO lpInsRecrd  && Insert New Record in Transaction Temporary File and Totals Temporary file.

ENDSCAN  && end Scan History records achieve both customer and History filters.

*B123160,1 BWA 06/10/2004 Restore the original values.[START]
lcOldFile = SELECT()
SELECT INVHDR
SET ORDER TO TAG &lcOrdInvH TO INVHDR
SELECT(lcOldFile)
*B123160,1 BWA 06/10/2004.[END]

SET RELATION TO  && Rest relation.

*--End of lfInsHisto.
*!*************************************************************
*! Name      : lpDebDeal
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Age debits according to aging type (By Date OR By Terms) also fill memory variables.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfUpdtAge
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : DO lpDebDeal
*!*************************************************************
*!Due to E301214,1
*
PROCEDURE lpDebDeal

*-- if Age By Date
IF lcAgeType = 'D'
  =lfUpdtAge('D',ldRpEndDat - TRANDATE)  && Update Date ages

ELSE  && else if Age by Terms

   ldDueDate = IIF(EMPTY(DUEDATE),TRANDATE+lnRPDay1,DUEDATE)
  =lfUpdtAge('D',ldRpEndDat - ldDueDate)  && Update Term ages
ENDIF  && end if Age By Date

*-- if charge back Charge back equal transaction amount
IF TRANTYPE= '3'
  m.ChgBack = Amount
ENDIF

STORE AMOUNT TO m.TotAge , m.NetBal  && Save total age and net balance per transaction

*-- if it is invoice and find it.
IF TRANTYPE = '1' AND SEEK(DEBIT.TRAN,'INVHDR')
  m.cDivision = INVHDR.CDIVISION  && invoice division.
ENDIF  
*-- end of lpDebDeal.

*!*************************************************************
*! Name      : lpCreDeal
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Age Credits also fill memory variables.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfUpdtAge
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : DO lpCreDeal
*!*************************************************************
*!Due to E301214,1
*
PROCEDURE lpCreDeal
=lfUpdtAge('C',gdSysDate - TRANDATE) && Update Date ages
STORE AMOUNT TO m.OpenCr , m.NetBal  && Save open credit and net balance per transaction

*-- if return invoice and return merchandise module installed and find this return.
IF TRANTYPE = '0' AND ('RM' $ gcCmpModules) AND SEEK(CREDIT.TRAN,'RETHDR')
  m.cDivision = RETHDR.CDIVISION  && Return division.
ENDIF  
*-- end of lpCreDeal.

*!*************************************************************
*! Name      : lfUpdtAge
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Age Credits also fill memory variables.
*! Note      : I use Age fields (Current, Age30 , 60 , 90 , 120) to age both
*!           : by date or by terms to save disk usage and also simplify print
*!           : forms to print from one field instead of use command like 
*!           : IIF(lcAgeType='D',Age30,TerAge30) , and we must all know that two situations
*!           : does not occur at the same time.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : lpDebDeal , lpCreDeal
*!*************************************************************
*! Passed Parameters  : 1- Age Debits(D)/Credits(C)
*!                    : 2- Age Days
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfUpdtAge()
*!*************************************************************
*!Due to E301214,1
*
FUNCTION lfUpdtAge
PARAMETERS lcUpdtTyp , lnAgeDays

*-- Intializing variables that calculates data from transaction files. [begin]
STORE 0.00 TO m.Current , m.Age30 , m.Age60 , m.Age90 , m.Age120 ,;
              m.OpenCr,m.ChgBack , m.Totage , m.NetBal , m.Age00
STORE '' TO m.Region , m.cDivision , m.SalesRep , m.cTermCode , m.cPastOnly
*-- Intializing variables that calculates data from transaction files. [End  ]

*-- if no parameter passed (i.e. want to intializing only) then return
*-- does occur in this program but it may be.
IF TYPE('lcUpdtTyp') $ 'UL'
  RETURN
ENDIF

*-- lcAges : Varaible Hold Field description (Age00, 30 , 60 , 90 , and 120)
*-- if Debit / History
IF lcUpdtTyp = 'D'

  *-- if Age By Date.
  IF lcAgeType = 'D'
    lcAges = 'm.Age'                                                    +;
      IIF(lnAgeDays >= lnRpDay4,'120',IIF(lnAgeDays >= lnRpDay3 ,'90'   ,;
      IIF(lnAgeDays >= lnRpDay2 ,'60' ,IIF(lnAgeDays >= lnRpDay1 ,'30','00')))) 


  ELSE  && else if Age By Terms.

    lcAges = 'm.Age'                                                      +;
      IIF(lnAgeDays >= lnRpDay3+1,'120',IIF(lnAgeDays >= lnRpDay2+1 ,'90' ,;
      IIF(lnAgeDays >= lnRpDay1+1 ,'60' ,IIF(lnAgeDays >= 1 ,'30','00')))) 
  
  ENDIF  && end if Age By Date.

ELSE  && else if Credit / History

  lcAges = 'm.Age'                                                      +;
    IIF(lnAgeDays >= 120,'120',IIF(lnAgeDays >= 90 ,'90' ,;
    IIF(lnAgeDays >= 60 ,'60' ,IIF(lnAgeDays >= 30 ,'30','00')))) 


ENDIF  && end if Debit / History

&lcAges   = Amount       && Fill Age variable
m.Current = IIF(m.Age00 = 0 , 0 , m.Age00)  && because there is no Age00 field (it's Current)
*-- end of lfUpdtAge.

*!*************************************************************
*! Name      : lpInsRecrd
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 02/05/1999
*! Purpose   : Insert both transaction and total records.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfBaseVal
*!*************************************************************
*! Called from : lpDebDeal , lpCreDeal
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : DO lpInsRecrd
*!*************************************************************
*!Due to E301214,1
*
PROCEDURE lpInsRecrd
PRIVATE lnPastAges , lnNetBal
STORE 0 TO lnPastAges , lnNetBal

*-- Evaluate Customer file memory variables. [Begin]
m.Region     = Customer.Region 
m.cTermCode  = Customer.cTermCode
m.cCont_Code = Customer.cCont_Code
m.SalesRep   = Customer.SalesRep
*-- Evaluate Customer file memory variables. [Begin]

*B606890,1 KHM 05/19/2003 (Begin) Getting the code's description
m.cCodeDescr = gfCoddes(m.cTermCode,'CTERMCODE',.T.)
*B606890,1 KHM 05/19/2003 (End)

INSERT INTO (lcTempAge) FROM MEMVAR         && Insert Transaction Record
SELECT (lcTempAge)
REPLACE cTempKey WITH EVALUATE(lcReplExpr)  && Replace key field with approparate value.

*-- if Debit, Credit, or History record only (Not Post dated checks record).
IF EMPTY(cTranType)

  *-- if Multi currency and currency does not equal base currency.
  IF llMultCur AND (cCurrCode <> gcBaseCurr)

    = lfBaseVal(@lnPastAges , @lnNetBal)  && Evaluate Past Ages and net balance in base currency.

  ELSE  && else if Single currency or currency equal base currency.

    lnPastAges = IIF(TotAge > 0 AND Current = 0,TotAge,0)  && Past age
    lnNetBal   = NetBal                                    && Net balance

  ENDIF  && end if Multi currency and currency does not equal base currency.

  *-- if Find this account in Temporary totals file.
  IF SEEK(Account,lcTempAcc)

    *-- Accomulate both past due ages and net balance in total file.
    REPLACE &lcTempAcc..nAges WITH   &lcTempAcc..nAges + lnPastAges ,;
            &lcTempAcc..nNetBal WITH &lcTempAcc..nNetBal + lnNetBal

  ELSE  && else if this account not found before.

    *-- Add new record in temporary total files.
    WAIT WINDOW 'Collect data for Customer ' + Account NOWAIT
    m.Account = Account
    m.nAges   = lnPastAges
    m.nNetBal = lnNetBal
    INSERT INTO (lcTempAcc) FROM MEMVAR  && insert new record in account totals file.
  
  ENDIF  && end if Find this account in Temporary totals file.

ENDIF  && end if Debit, Credit, or History record.
*-- end of lpInsRecrd.

*!*************************************************************
*! Name      : lfBaseVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 02/17/98
*! Purpose   : Calculate Currency unit, rate, signs and Evaluate amount values
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfGetExSin,gfChkRate
*!*************************************************************
*! Passed Parameters  : 1- First  number (Due Ages Amount)
*! By Reference       : 2- Second number (Net Balance Amount)
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfBaseVal()
*!*************************************************************
*
FUNCTION lfBaseVal
PARAMETERS lnBaseVal1 , lnBaseVal2
PRIVATE lcAExRSin , lcAUntSin , lnCurrRate , lnCurrUnit

lcAUntSin   = '*'        && Variable to hold unit sign.
lcAExRSin   = '*'        && Variable to hold exchange rate sign.

lcAExRSin  = gfGetExSin(@lcAUntSin, cCurrCode)  && Currency sign.

*B603053,1 Calculate equav. Base rate and unit from transaction file [Begin]
*lnCurrRate = gfChkRate('lnCurrUnit',cCurrCode,TranDate,.F.,gcAct_comp,.F.)  && Currency rate.
lnCurrRate  = nExRate          && Variable to hold current rate
lnCurrUnit  = nCurrUnit        && Variable to hold current unit
*B603053,1 Calculate equav. Base rate and unit from transaction file [End  ]

lnBaseVal1 = IIF(TotAge > 0 AND Current = 0,TotAge,0) &lcAExRSin lnCurrRate &lcAUntSin lnCurrUnit
lnBaseVal2 = NetBal &lcAExRSin lnCurrRate &lcAUntSin lnCurrUnit
*-- end of lfBaseVal.

*!*************************************************************
*! Name      : lfPrnCurDt
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 03/20/1999
*! Purpose   : Print under Current field in Detail form .FRX
*!           : and this is because Expression is too long and .FRX expression does not fit it. 
*!*************************************************************
*! Called from : ARAGREPD.FRX
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfPrnCurDt()
*!*************************************************************
*!
FUNCTION lfPrnCurDt
PRIVATE lcPrintVal
lcPrintVal = ''
*-- if user want to age credits.
IF llRpAgeCrd

  *-- if (User Want to Print Credit details OR it's +Ve Transaction) AND
  *--    Current Current not equal zero.
  IF (llRpCrdDet OR NetBal >= 0) AND current <> 0
    *E301429,1 (Start) Print decimals Depends on the user input.
    *lcPrintVal = TRANSFORM(IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=gcBaseCurr),Current,gfAmntDisp(Current,lcRpCurr,ldRpExDate,lcRpTmpNam)),'99999999.99')
    IF llRpDec
      lcPrintVal = TRANSFORM(IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=gcBaseCurr),Current,gfAmntDisp(Current,lcRpCurr,ldRpExDate,lcRpTmpNam)),'99999999.99')
    ElSE
      lcPrintVal = TRANSFORM(IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=gcBaseCurr),Current,gfAmntDisp(Current,lcRpCurr,ldRpExDate,lcRpTmpNam)),'99999999999')
    ENDIF
    *E301429,1 (End) 
  ENDIF

ELSE  && User does not want to Age Credits

  *-- if it is credit transaction.
  IF OpenCr < 0

    *-- if Include credit detail.
    IF llRpCrdDet
      *E301429,1 (Start) Print decimals Depends on the user input
      *lcPrintVal = TRANSFORM(IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=gcBaseCurr),OpenCr,gfAmntDisp(OpenCr,lcRpCurr,ldRpExDate,lcRpTmpNam)),'99999999.99')
      IF llRpDec
        lcPrintVal = TRANSFORM(IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=gcBaseCurr),OpenCr,gfAmntDisp(OpenCr,lcRpCurr,ldRpExDate,lcRpTmpNam)),'99999999.99')
      ElSE
        lcPrintVal = TRANSFORM(IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=gcBaseCurr),OpenCr,gfAmntDisp(OpenCr,lcRpCurr,ldRpExDate,lcRpTmpNam)),'99999999999')
      ENDIF
      *E301429,1 (End) 
    ENDIF  
  
  ELSE  && else it is debit transaction, in current field.
    *E301429,1 (Start) Print decimals Depends on the user input 
    *lcPrintVal = TRANSFORM(IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=gcBaseCurr),Current,gfAmntDisp(Current,lcRpCurr,ldRpExDate,lcRpTmpNam)),'99999999.99')
    IF llRpDec 
      lcPrintVal = TRANSFORM(IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=gcBaseCurr),Current,gfAmntDisp(Current,lcRpCurr,ldRpExDate,lcRpTmpNam)),'99999999.99')
    ElSE
      lcPrintVal = TRANSFORM(IIF(!llMultCur OR (lcRpCurr = 'F') OR (CCURRCODE=gcBaseCurr),Current,gfAmntDisp(Current,lcRpCurr,ldRpExDate,lcRpTmpNam)),'99999999999')
    ENDIF
    *E301429,1 (End) 
  ENDIF        && end if it is credit transaction.

ENDIF          && end if user want to age credits.

*-- if Value of return value = 0. [Begin]
IF !EMPTY(lcPrintVal) AND VAL(lcPrintVal) = 0
  lcPrintVal = ''
ENDIF
*-- if Value of return value = 0. [End  ]

RETURN lcPrintVal
*-- end of lfPrnCurDt.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 03/20/1999
*! Purpose   : Close any opened files if user press OG <Close> Button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
*!
FUNCTION lfClearRep
*-- Rise llOGFltCh flag to recollect data next time preview or run because this fn called 
*-- if user press <Reset> or Clear Read.
llOGFltCh = .T.  

*-- Close post dated checks file.
IF USED('POSTDCHQ')
  USE IN POSTDCHQ
ENDIF

*-- Close Currency file.
IF USED('SYCCURR')
  USE IN SYCCURR
ENDIF

*-- Close Temporary Cursors [Begin]
IF USED(lcTempAge)
  USE IN (lcTempAge)
ENDIF

IF USED(lcTempRev)
  USE IN (lcTempRev)
ENDIF

IF USED(lcTempAcc)
  USE IN (lcTempAcc)
ENDIF
*-- Close Temporary Cursors [End  ]
*-- end of lfClearRep.

*C102178,1 BWA 01/23/2000 1) Change the selection grid for the customer to be mover to let the user select more than one choice.[START]
*!*************************************************************
*! Name      : lfvTerm
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 01/21/2000
*! Purpose   : Function to fill the lcRpStatFr variable for the filter.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvTerm()
*!*************************************************************
FUNCTION lfvTerm

STORE .F. TO llFlgMovr
STORE .T. TO llogfltch           && Rise this flag to recollect the data.
STORE SPACE(0) TO lcRpStatFr

= gfMover(@laRpSource,@laRpTarget,'Customer Status ',.T.,'')

IF ASCAN(laRpTarget,"Active") > 0       && Case for the Active customer
 lcRpStatFr = "A"
 llFlgMovr = .T.
ENDIF

IF ASCAN(laRpTarget,"Hold") > 0         && Case for the Hold customer
 lcRpStatFr = lcRpStatFr + "H"
 llFlgMovr = .T.
ENDIF

IF ASCAN(laRpTarget,"Cancelled") > 0    && Case for Cancelled the customer
  lcRpStatFr = lcRpStatFr + "X"
  llFlgMovr = .T.
ENDIF

IF !llFlgMovr
  lcRpStatFr = "AHX"
ENDIF

*--End of lfvTerm.
*!*************************************************************
*! Name      : lfFillARY
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Function to fill Terms,Division,Region,Ship via,
*!           : Special Instruction,Customer classification.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfFillAll()
*!*************************************************************
FUNCTION lfFillARY

DIMENSION laRpSource[3],laRpTarget[1,1]
STORE '' TO laRpSource,laRpTarget

laRpSource[1] = 'Active'
laRpSource[2] = 'Hold'
laRpSource[3] = 'Cancelled'

*--End of lfFillARY.
*C102178,1 [END]
*:*************************************************************
*: Name      : lfItmPos
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 05/26/2002
*: Purpose   : Evaluate fixed filter position within array.
*:*************************************************************
*: Called from : Araging.prg
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : Position.
*:*************************************************************
*: Example     : = lfItmPos()
*:*************************************************************
*!C605955,1 ABD [Begin]
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*C605955,1 ABD - [End]
*-- End of lfItmPos.
*:*************************************************************
*: Name      : lfSeTSRep 
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 05/26/2002
*: Purpose   : Go top in Sales Rep file.
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : Position.
*:*************************************************************
*: Example     : = lfSetSRep()
*:*************************************************************
*!C605955,1 ABD [Begin]
FUNCTION lfSetSRep
PARAMETERS OpGrdParm
DO CASE
  CASE OpGrdParm = 'S'
   SELECT SalesRep
   SET ORDER TO TAG SalesRep
   GO TOP
  CASE OpGrdParm = 'R'
    SELECT SalesRep 
    SET ORDER TO 
ENDCASE

*-- End of lfSetSRep.
*:*************************************************************
