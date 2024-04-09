*:**************************************************************************
*: Program file  : ARCSTMDL.PRG
*: Program desc. : Customer Statement form [ Based on Form 'A']
*: For Report    : ArCStmdl.FRX
*: Date          : 05/01/2003
*: System        : Aria Advantage Series.
*: Module        : Account Receivable (AR)
*: Developer     : Abdou Elgendy (ABD)
*: Reference     : B607215,1
*:**************************************************************************
*: Procedures    : lpFillStru , lpCreatFil , lpCollect , lpInsDebit  ,
*:                 lpDebDeal  , lpInsCredt , lpCreDeal , lpInsRecrd  , 
*:                 lpCurrChng , lpChSortBy , lpApplyAge , lpGroupChg , 
*:                 lpApplyRel , lpCollhist
*:
*: Functions     : lfwRepWhen , lfItmPos   , lfVarPos   , lfvAcct    , lfvDate , 
*:                 lfAdrShift , lfvCurDisp , lfvPeriod  , lfvTran    , 
*:                 lfGetSign  , lfIntializ , lfClearRep , lfUpdtAge  , 
*:                 lfGetReplc , lfGetAddr  , lfEndGroup , lfOverMsg  , 
*:                 lfCollTime , lfAmntDisp , lfBreakRel , lfvInvoice ,
*:                 lfvRemit   , lfvFactor  , lfCutDate  , lfupdhdate ,
*:                 lpGetHAmnt , lpHstDeal  , lfUpdtHAge , lpInshstRc ,
*:                 lpGetAmnth , lfVoidRcrd , lfCurrAmt  , lfCurCdAmt
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*: Notes : var lcFormType must be defined in SyFrmCdD file in memo field 
*:         called mFrmSpSets it takes one of the following values 
*:         A --> Report will collect data for Form A or any typical form to it
*:         B --> Report will collect data for Form B or any typical form to it
*:         C --> This is kept for Future use
*:        '' --> It may be used in case of @SAY
*:**************************************************************************
*: Modification : -
*:C037891,1 BWA 03/24/2004 Add new colums to the Layout.
*:B123679,1 BWA 08/02/2004 Fix the bug of calculating transactions from ARHIST.
*:B124598,1 BWA 04/10/2004 Fix the bug of calculating transactions from ARHIST.
*:**************************************************************************
*:
lcStTime = TIME()

PRIVATE lcAliasB
DIMENSION laFactTo[6,1]
laFactTo = ''
lcFacName = ''
lcPrnComp = IIF(TYPE('llPrntComp') = 'L' , IIF(llPrntComp , 'Y' , 'N') , 'Y')
llPrntComp = lcPrnComp  = "Y"
llLogo = IIF(SEEK('*' + 'LOGO' , 'OBJLINK') .AND. SEEK(OBJLINK.cObject_ID ,;
             'OBJECTS') , .T. , .F.)        && Flag to know if we are to print the Company Logo
lcTaxRef = gfGetMemVar('M_TAX_REFE')        
=SEEK(gcAct_comp,'SycComp')
lcCompFax = TRANSFORM(SycComp.CCOM_FAX , gfPhoneTem())  && Fax No. Pic


*-- Checking if Current and all Aging not False (at least one is selected)
IF !llRpCurrnt AND !llRp30 AND !llRp60 AND !llRp90 AND !llRp120
  *-- Message <You have to select at least one balance period.>
  *-- Buttons <                     OK                      >
  =gfModalGen('TRM40136B40011','ALERT')
  RETURN
ENDIF

*-- Restore statement messages from memo file. [Begin]
IF !FILE('&gcDataDir.ARSTMSG.MEM')
  *-- Message <Statement Message is Empty>
  *-- Buttons <  <Proceed>     <Cancel>  >
  IF gfModalGen('QRM40157B40003','DIALOG') = 2
    RETURN
  ELSE
    STORE SPACE(60) TO MADL1 , MADL2 , MADL3 , MADL4, MADL5 ,;
                       M120L1, M120L2, M120L3, M90L1, M90L2 ,;
                       M90L3 , M60L1 , M60L2 , M60L3, M30L1 ,;
                       M30L2 , M30L3
    SAVE ALL LIKE M* TO (gcDataDir+'ARSTMSG')                       
    llOGFltCh = .T.
  ENDIF
ELSE
  RESTORE ADDITIVE FROM (gcDataDir+'ARSTMSG')
ENDIF  
*-- Restore statement messages from memo file. [End]

IF lcRpRemit='F' AND EMPTY(lcRpFactor)
  *-- Message <You have to enter a factor code.>
  *-- Buttons <              OK                >
  =gfModalGen('TRM00250B40011','ALERT','factor code')
  RETURN
ENDIF

IF llRpRemit
  =lfGetFac()
ELSE
  lcFacName = lcName
  =ACOPY(laCompany,laFactTo)
ENDIF

*-- if Filter Expression changed (ReCollect data again)
IF llOGFltCh 
   
   *-- Function to update the cut off date.
   = lfupdhdate ()
  *-- If Arrays is not formed yet or User presses Reset (Array will become empty)
  IF TYPE("laAccStru[1]") <> 'C' OR EMPTY(laAccStru[1])
    DO lpFillStru
  ENDIF

  *-- If Temp file is not used or has records inside
  IF !USED(lcTmpTrans) OR RECCOUNT(lcTmpTrans) > 0
    DO lpCreatFil
  ENDIF

  PRIVATE lcDebitFlt
  lcDebitFlt = ''

  lcRpExp = STRTRAN(lcRpExp,"DEBIT.","")
  lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),'',[ AND ]) + ;
            [Customer.Prnt_Statm<>'N']




  DO CASE 
    CASE lcRpRemit = 'F'
      lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),'',[ AND ]) + [cFacCode = lcRpFactor]
    CASE lcRpRemit = 'H'
      lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),'',[ AND ]) + [EMPTY(cFacCode)]
  ENDCASE

  *-- If user wants to Print Only Open Invoices
  IF llRpInvoie
    lcDebitFlt = lcDebitFlt + IIF(EMPTY(lcDebitFlt),"",[ AND ]) + [TranType = '1']
  ENDIF
  
  *-- If Print ChargeBack is NO
  IF !llRpPrnCha
    lcDebitFlt = lcDebitFlt + IIF(EMPTY(lcDebitFlt),"",[ AND ]) + [TranType <> '3']
  ENDIF
  lcDebitFlt = ALLTRIM(lcDebitFlt)
  
  *-- check for Date used in Aging
  ldRpHDate = IIF(EMPTY(ldRpHDate),gdSysDate,ldRpHDate)
  
  *-- Re-Initialize Age , currency variable
  lcOldAgeFl = ""
  lcSavedCur = ""
  
  *-- Get Sort By Expression.
  lcReplExpr = lfGetReplc()

  DO lpCollect   && Collect Data
ENDIF
*-- Endif of Filter Expression changed

*C037891,1 BWA 03/24/2004 Function to calculate the Running balance field.
=lfCalRnBal()
*C037891,1 BWA 03/24/2004.

IF lcFormType = "B" AND lcRepMode = "Text"
  lcAliasB = SELECT(0)
  IF USED(lcTmpTrn_A)
    USE IN (lcTmpTrn_A)
  ENDIF
  USE (lcTmpTrans) IN 0 AGAIN ALIAS (lcTmpTrn_A)
  SELECT(lcAliasB)
ENDIF

DO lpApplyAge  && Apply Aging criteria
*-- If End of File encountered (no Records to Display)
SELECT (lcTmpAcct)
LOCATE
*-- If End of File encountered (no Records to Display)
IF EOF()
  *-- Message <There are no records to display>
  *-- Buttons <               OK              >
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN	         && in case of @SAY
  RETURN
ENDIF
DO lpChSortBy  && Apply changing Sort by
DO lpGroupChg  && Apply Group change
DO lpCurrChng  && Apply Cuurency Change

lcOldAgeFl  = lcAgeFlt     && Store Current Age Filter
lcSavedCur  = lcRpCurr     && Store Current Currency type

*-- Apply Relation
SELECT (lcTmpTrans)
LOCATE
SELECT (lcTmpAcct)
LOCATE
DO lpApplyRel  && Apply Relations...

*-- Calculate spent time in collecting data.
lnInterval = lfCollTime(lcStTime,TIME())  && Calculate collecting data spent time.
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcTmpAcct))) + ;
            ' Customer(s), ' + ALLTRIM(STR(RECCOUNT())) + ;
            ' Transaction(s) in ' + ALLTRIM(STR(lnInterval,6,2)) + ;
            ' Seconds...' NOWAIT

*-- Call the Optional Program
=lfOptProg()
*-- If Report type is @SAY 
lcPrgName  = lcFormName
llIsAparel = .F.
IF llIsAparel
  DO EVALUATE('lcPrgName')
  DO ENDREPORT
ELSE    && Else Report is FRX
  STORE '' TO lcOverMsg1, lcOverMsg2, lcOverMsg3
  STORE .T. TO llGrpChang
  IF lcFormType = "A"
    =lfCutDate()
  ENDIF
  DO gfDispRe WITH EVAL('lcFormName')  
ENDIF

*-- EndIf of Report type is @SAY 
*:**************************************************************************
*: Name      : lfGroupTot
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Indicate end of group 
*:**************************************************************************
*: Called from : .FRX group footer (Report Form A)
*:**************************************************************************
*: Example     : = lfGroupTot()
*:**************************************************************************
*
FUNCTION lfGroupTot
*-- This Function is called in every page of Form A to detect if group
*-- is changed
PRIVATE lcAlias , lnGroupNo , lcCurrCode
lcAlias = ALIAS()      && Save Current Alias

SELECT (lcTmpGroup)
lnGroupNo  = nGroup
lcCurrCode = EVAL(lcTmpTrans+'.cCurrCode')
SKIP 1            && Move to next record
IF lnGroupNo >= nGroup OR EOF()
  llGrpChang = .T.
ELSE
  IF lcCurrCode == EVAL(lcTmpTrans+'.cCurrCode')
    llGrpChang = .F.
  ELSE
    llGrpChang = .T.
  ENDIF  
ENDIF 
SKIP -1           && Move to previous record

SELECT (lcAlias)     && Restore Old Alias
RETURN ''
*-- End of lfGroupTot.

*:**************************************************************************
*: Name      : lfwRepWhen
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Report When Function
*:**************************************************************************
*: Example   : = lfwRepWhen()
*:**************************************************************************
*
FUNCTION lfwRepWhen
PRIVATE lnTranDate
lnTranDate = lfItmPos('DEBIT.TRANDATE')   && get Tran. Date Fixed filter Position

*-- If Trans. Date found in Fixed filter option
IF lnTranDate > 0
  *-- Replace TO DATE in Transaction date with Current Date (System Date)
  *-- If system uses 4 Char. for Century 
  IF "ON" $ SET('CENTURY')
    laOGFxFlt[lnTranDate,6] = '  /  /    |' + DTOC(gdSysDate)
  ELSE    && Else system uses 2 char. for Century
    laOGFxFlt[lnTranDate,6] = '  /  /  |' + DTOC(gdSysDate)
  ENDIF
  *-- EndIf of system uses 4 Char. for Century 
ENDIF
*-- Endif of Trans. Date found in Fixed filter option 
llRpRemit = (lcRpRemit = 'F')
*-- check for single currency to open Post Dated Cheques file
IF !llMulCurr AND !USED('POSTDCHQ')
  llOpnPdChq = gfOpenFile(gcDataDir+'PostDChq','PostDChq','SH')
ENDIF

*-- Enable Currency Push Button in Case of Multi Currency
laOGObjCnt[lfVarPos('lnRpRepCur')] = llMulCurr
=lfOGShowGet('lnRpRepCur')

*-- Disable only open Invoice in case of not debit
laOGObjCnt[lfVarPos('llRpInvoie')] = IIF(lcRpTrans = 'D',.T.,.F.)
=lfOGShowGet('llRpInvoie')

*-- Disable only [Account has total credit balance] in case of not debit
laOGObjCnt[lfVarPos('llRpPrnBal')] = IIF(lcRpTrans <> 'D',.T.,.F.)
=lfOGShowGet('llRpPrnBal')

*-- Disable ChargeBack Option in case of Credit
laOGObjCnt[lfVarPos('llRpPrnCha')] = IIF(lcRpTrans $ 'DB',.T.,.F.)
=lfOGShowGet('llRpPrnCha')
=lfvInvoice()

*-- End of lfwRepWhen.
*:**************************************************************************
*: Name      : lfItmPos
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : To get the position of the fixed filter in OG
*:**************************************************************************
*: Called from : OG When Function 
*:**************************************************************************
*: Example   : = lfItmPos()
*:**************************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- End of lfItmPos.
*:**************************************************************************
*: Name      : lfVarPos
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : To get the position of the Variable in OG
*:**************************************************************************
*: Called from : OG when function
*:**************************************************************************
*: Example   : = lfVarPos()
*:**************************************************************************
*
FUNCTION lfVarPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos
lnItmPos = ASCAN(laOGObjType,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGObjType,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- End of lfVarPos.
*:**************************************************************************
*: Name      : lfvAcct
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Validate the enterd account
*:**************************************************************************
*: Example   : = lfvAcct()
*:**************************************************************************
*
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
*-- End of lfvAcct.

*:**************************************************************************
*: Name      : lfvDate
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Validate the enterd date
*:**************************************************************************
*: Example   : = lfvDate()
*:**************************************************************************
*
FUNCTION lfvDate
PRIVATE lcObjNam , lcObjVal , llObjRet

lcObjNam = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

IF !EMPTY(lcObjVal) .AND.  lcObjVal > gdSysDate
  =gfModalGen('TRM40135B40011','ALERT')
  lcObjVal = gdSysDate
ENDIF
&lcObjNam = lcObjVal
ldRpHDate = IIF(EMPTY(ldRpHDate),lcObjVal,MAX(ldRpHDate,lcObjVal))

*-- End of lfvDate.
*:**************************************************************************
*: Name      : lfAdrShift
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Shift the add. if there is any empty one
*:**************************************************************************
*: Example   : = lfAdrShift()
*:**************************************************************************
*
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

FOR lnCount = 1 TO ALEN(&lcArrayNam.,1)
  *-- IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*-- FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam.,1)
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*-- End of lfAdrShift.
*:**************************************************************************
*: Name      : lfvCurDisp
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Display the Currency Transform Screen
*:**************************************************************************
*: Called from : Called from the currency button
*:**************************************************************************
*: Example   : = lfvCurDisp()
*:**************************************************************************
*
FUNCTION lfvCurDisp
=gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpCur)

*-- End of lfvCurDisp.
*:**************************************************************************
*: Name      : lfvPeriod
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : To be sure that there is only one perid is set to  'Yes'
*:**************************************************************************
*: Example   : = lfvPeriod()
*:**************************************************************************
*
FUNCTION lfvPeriod
PARAMETER lnCondition

DO CASE
  CASE lnCondition = 1
    STORE .F. TO llRp30 , llRp60 , llRp90 , llRp120
  CASE lnCondition = 2
    STORE .F. TO llRpCurrnt , llRp60 , llRp90 , llRp120
  CASE lnCondition = 3
    STORE .F. TO llRpCurrnt , llRp30 , llRp90 , llRp120
  CASE lnCondition = 4
    STORE .F. TO llRpCurrnt , llRp30 , llRp60 , llRp120
  CASE lnCondition = 5
    STORE .F. TO llRpCurrnt , llRp30 , llRp60 , llRp90
ENDCASE
=lfOGShowGet('llRpCurrnt')
=lfOGShowGet('llRp30')
=lfOGShowGet('llRp60')
=lfOGShowGet('llRp90')
=lfOGShowGet('llRp120')

*-- End of lfvPeriod.
*:**************************************************************************
*: Name      : lfvTran
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Disable the invoice option in case of not debit
*:**************************************************************************
*: Example   : = lfvTran()
*:**************************************************************************
*:
FUNCTION lfvTran
*-- Disable only open Invoice in case of not debit
laOGObjCnt[lfVarPos('llRpInvoie')] = IIF(lcRpTrans = 'D',.T.,.F.)
=lfOGShowGet('llRpInvoie')

*-- Disable ChargeBack Option in case of Credit
laOGObjCnt[lfVarPos('llRpPrnCha')] = IIF(lcRpTrans $ 'DB',.T.,.F.)
=lfOGShowGet('llRpPrnCha')

*-- Disable only [Account has total credit balance] in case of not debit
laOGObjCnt[lfVarPos('llRpPrnBal')] = IIF(lcRpTrans <> 'D',.T.,.F.)
=lfOGShowGet('llRpPrnBal')

*-- End of lfvTran.
*:**************************************************************************
*: Name      : lfGetSign
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Get the symbol of the used curr.
*:**************************************************************************
*: Example   : = lfGetSign()
*:**************************************************************************
*
FUNCTION lfGetSign
PRIVATE lcSign

lcSign = SPACE(3)
lcSign = IIF(SEEK(&lcTmpTrans..cCurrCode,'SYCCURR'),SYCCURR.cCurrSmbl,lcSign)
RETURN IIF(lcCurrPost="LEFT",lcSign + "  Amount" ,"Amount  " + lcSign)

*-- End of lfGetSign
*:**************************************************************************
*: Name      : lfIntializ
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : To initialize all variables defined in Syrepuvr
*:**************************************************************************
*: Example   : = lfIntializ()
*:**************************************************************************
*
FUNCTION lfIntializ

*-- No of lines displayed in FRX
lnRpLines = 11

*-- Array to hold the account Sold To Address and Company Address
DIMENSION laSoldTo [5,1] , laCompany[6,1] , laSortDesc[4,1] , laSortVal[4,1] , laSetups[2,2]
laSoldTo   = ''
laCompany  = ''

*-- Get the address of the active company
=SEEK(gcAct_comp,'SycComp')
lcName  = SycComp.cCom_Name
lcPhone = SycComp.cCom_Phon

=gfGetAdr('SycComp' , '' , '' , '' , @laCompany, 1)
laCompany[6,1] = "Phone # : " + TRANSFORM(lcPhone , gfPhoneTem())
=lfAdrShift('laCompany')

=SEEK(SycComp.cCont_Code,'SycInt')
lcCountry  = SycInt.cPart4Lab
lcZip      = SycInt.cPart5Lab
lcCurrPost = SycInt.cCurrency
laSortDesc[1] = 'Account'
laSortDesc[2] = lcCountry
laSortDesc[3] = lcZip
laSortDesc[4] = 'Region'

laSortVal[1] = 'A'
laSortVal[2] = 'C'
laSortVal[3] = 'Z'
laSortVal[4] = 'R'

*-- Check if the system is setup to use multi currency
laSetups[1,1] = 'llMulCurr'
laSetups[2,1] = 'XAGINGTYPE'
=gfGetMemVar(@laSetups)
llMulCurr = laSetups[1,2]
lcAgeType = laSetups[2,2]

*-- Heading displayed in Option Grid according to Age Type (Date or Terms)
IF lcAgeType = 'D'
  lcHead1  = ' 30 Days+'
  lcHead2  = ' 60 Days+'
  lcHead3  = ' 90 Days+'
  lcHead4  = '120 Days+'
ELSE
  lcHead1  = ' ( +1 - 30) Days'
  lcHead2  = ' (+31 - 60) Days'
  lcHead3  = ' (+61 - 90) Days'
  lcHead4  = '(+91 - 120) Days'
ENDIF

*-- Temporay files names
lcTmpTrans = gfTempName()
lcTmpAcct  = gfTempName()
lcTmpGroup = gfTempName()
lcTmpLoop  = gfTempName()
lcTmpCurrc = gfTempName()
lcTmpAcCur = gfTempName()

*C037891,1 BWA 03/24/2004 Add 2 variables for the Invhdr and Rethdr files to open them.
lcInvHdr   = gfTempName()
lcRethdr   = gfTempName()

*-- Open the INVHDR.DBF to get the TAX_AMT value.
IF !USED(lcInvHdr)
  =gfOpenFile(gcDataDir+"InvHdr","Invhdra",'SH', @lcInvHdr, .T.)
ENDIF

*-- Open the RETHDR.DBF to get the TAX_AMT value.
IF !USED(lcRethdr)
  =gfOpenFile(gcDataDir+"RetHdr","Rethdra",'SH', @lcRethdr, .T.)
ENDIF
*C037891,1 BWA 03/24/2004.

*-- End of lfIntializ.
*:**************************************************************************
*: Name      : lfClearRep
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Clear the Report variables
*:**************************************************************************
*: Example   : =lfClearRep()
*:**************************************************************************
*
FUNCTION lfClearRep
llOGFltCh = .T.
IF llOpnPdChq
  USE IN PostDChq
ENDIF

*-- Close Temporary Cursors [Begin]
IF USED(lcTmpTrans)
  USE IN (lcTmpTrans)
  IF !TYPE('lcTmpTrn_A') = "U"
    IF USED(lcTmpTrn_A)
      USE IN (lcTmpTrn_A)
    ENDIF
  ENDIF
  ERASE (gcWorkDir+lcTmpTrans+".DBF")
  ERASE (gcWorkDir+lcTmpTrans+".CDX")
ENDIF

IF USED(lcTmpAcct)
  USE IN (lcTmpAcct)
  ERASE (gcWorkDir+lcTmpAcct+".DBF")
  ERASE (gcWorkDir+lcTmpAcct+".CDX")
ENDIF

IF USED(lcTmpGroup)
  USE IN (lcTmpGroup)
  ERASE (gcWorkDir+lcTmpGroup+".DBF")
  ERASE (gcWorkDir+lcTmpGroup+".CDX")
ENDIF

IF USED(lcTmpLoop)
  USE IN (lcTmpLoop)
  ERASE (gcWorkDir+lcTmpLoop+".DBF")
  ERASE (gcWorkDir+lcTmpLoop+".CDX")
ENDIF

IF USED(lcTmpCurrc)
  USE IN (lcTmpCurrc)
  ERASE (gcWorkDir+lcTmpCurrc+".DBF")
  ERASE (gcWorkDir+lcTmpCurrc+".CDX")
ENDIF

IF USED(lcTmpAcCur)
  USE IN (lcTmpAcCur)
  ERASE (gcWorkDir+lcTmpAcCur+".DBF")
  ERASE (gcWorkDir+lcTmpAcCur+".CDX")
ENDIF

*-- Close Temporary Cursors [End  ]
*-- End of lfClearRep.
*:**************************************************************************
*: Name      : lpFillStru
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : To fill Structures of all Arrays creating Temp cursors
*:**************************************************************************
*: Example   : DO lpFillStru
*:**************************************************************************
*
PROCEDURE lpFillStru
PRIVATE lnTranFile
*-- Prepare Arrays

*C037891,1 BWA 03/24/2004 Add 2 more fields to show them in the layout.
*DIMENSION laAgeStru[11,4],laAccStru[16,4],laTranStru[18,4]
DIMENSION laAgeStru[11,4],laAccStru[16,4],laTranStru[20,4]
*C037891,1 BWA 03/24/2004.

*-- Age Array Structure
laAgeStru[1,1] = "Current"
laAgeStru[2,1] = "Age30"
laAgeStru[3,1] = "Age60"
laAgeStru[4,1] = "Age90"
laAgeStru[5,1] = "Age120"
laAgeStru[6,1] = "OpenCr"
laAgeStru[7,1] = "TotAge"
laAgeStru[8,1] = "NetBal"

FOR lnI = 1 TO 8
  laAgeStru[lnI,2] = 'N'
  laAgeStru[lnI,3] = 20
  laAgeStru[lnI,4] = 2
ENDFOR

laAgeStru[9,1] = "cAddress4"
laAgeStru[9,2] = "C"
laAgeStru[9,3] = 30
laAgeStru[9,4] = 0

laAgeStru[10,1] = "cAddress5"
laAgeStru[10,2] = "C"
laAgeStru[10,3] = 30
laAgeStru[10,4] = 0

laAgeStru[11,1] = "Region"
laAgeStru[11,2] = "C"
laAgeStru[11,3] = 6
laAgeStru[11,4] = 0

*-- Account Array Structure
laAccStru[1,1] = "cGroupKey"
laAccStru[1,2] = "C"
laAccStru[1,3] = 35
laAccStru[1,4] = 0

laAccStru[2,1] = "Account"
laAccStru[2,2] = "C"
laAccStru[2,3] = 5
laAccStru[2,4] = 0

laAccStru[3,1] = "BtName"
laAccStru[3,2] = "C"
laAccStru[3,3] = 30
laAccStru[3,4] = 0

laAccStru[4,1] = "nPostDChq"
laAccStru[4,2] = "N"
laAccStru[4,3] = 11
laAccStru[4,4] = 2

laAccStru[5,1] = "cFacCode"
laAccStru[5,2] = "C"
laAccStru[5,3] = 6
laAccStru[5,4] = 0

*-- Complete account Structure.
=ACOPY(laAgeStru,laAccStru,1,-1,21)  && (7Rows * 4Col + 1)

*-- Temp. Transaction file.
laTranStru[1,1] = "Account"
laTranStru[1,2] = "C"
laTranStru[1,3] = 5
laTranStru[1,4] = 0

laTranStru[2,1] = "TranType"
laTranStru[2,2] = "C"
laTranStru[2,3] = 1
laTranStru[2,4] = 0

laTranStru[3,1] = "TranCode"
laTranStru[3,2] = "C"
laTranStru[3,3] = 6
laTranStru[3,4] = 0

laTranStru[4,1] = "Tran"
laTranStru[4,2] = "C"
laTranStru[4,3] = 6
laTranStru[4,4] = 0

laTranStru[5,1] = "cInsTalno"
laTranStru[5,2] = "C"
laTranStru[5,3] = 3
laTranStru[5,4] = 0

laTranStru[6,1] = "TranDate"
laTranStru[6,2] = "D"
laTranStru[6,3] = 8
laTranStru[6,4] = 0

laTranStru[7,1] = "Desc"
laTranStru[7,2] = "C"
laTranStru[7,3] = 20
laTranStru[7,4] = 0

laTranStru[8,1] = "Reference"
laTranStru[8,2] = "C"
laTranStru[8,3] = 30
laTranStru[8,4] = 0

laTranStru[9,1] = "Amount"
laTranStru[9,2] = "N"
laTranStru[9,3] = 11
laTranStru[9,4] = 2

laTranStru[10,1] = "DueDate"
laTranStru[10,2] = "D"
laTranStru[10,3] = 8
laTranStru[10,4] = 0

laTranStru[11,1] = "cCurrCode"
laTranStru[11,2] = "C"
laTranStru[11,3] = 3
laTranStru[11,4] = 0

laTranStru[12,1] = "cGroupKey"
laTranStru[12,2] = "C"
laTranStru[12,3] = 35
laTranStru[12,4] = 0

laTranStru[13,1] = "nGroup"
laTranStru[13,2] = "N"
laTranStru[13,3] = 6
laTranStru[13,4] = 0

laTranStru[14,1] = "nPosition"
laTranStru[14,2] = "N"
laTranStru[14,3] = 2
laTranStru[14,4] = 0

laTranStru[15,1] = "nHstAmount"
laTranStru[15,2] = "N"
laTranStru[15,3] = 11
laTranStru[15,4] = 2

laTranStru[16,1] = "nORATE"
laTranStru[16,2] = "N"
laTranStru[16,3] = 15
laTranStru[16,4] = 6

laTranStru[17,1] = "Store"
laTranStru[17,2] = "C"
laTranStru[17,3] = 8
laTranStru[17,4] = 0

laTranStru[18,1] = "History"
laTranStru[18,2] = "C"
laTranStru[18,3] = 6
laTranStru[18,4] = 0

*C037891,1 BWA 03/24/2004 Add Running balance field and Vat amount.
laTranStru[19,1] = "RunBalanc"
laTranStru[19,2] = "N"
laTranStru[19,3] = 11
laTranStru[19,4] = 2

laTranStru[20,1] = "VatAmnt"
laTranStru[20,2] = "N"
laTranStru[20,3] = 11
laTranStru[20,4] = 2
*C037891,1 BWA 03/24/2004.

lnTranFile = ALEN(laTranStru,1)*ALEN(laTranStru,2)

*-- Copy empty structure only.
DIMENSION laTranStru[ALEN(laTranStru,1)+ALEN(laAgeStru,1),4]
=ACOPY(laAgeStru,laTranStru,1,-1,lnTranFile+1)
*-- End of lpFillStru.

*:**************************************************************************
*: Name      : lpCreatFil
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Create work File.
*:**************************************************************************
*: Example   : DO lpCreatFil
*:**************************************************************************
*
PROCEDURE lpCreatFil

IF lcFormType = "A"
  
  *-- Temp. Loop File.
  CREATE CURSOR (lcTmpLoop) (cLoop C(1))
  ZAP
  INDEX ON cLoop TAG (lcTmpLoop) OF (gcWorkDir+lcTmpLoop+".CDX")
  INSERT INTO (lcTmpLoop) (cLoop) VALUES ("A")
  INSERT INTO (lcTmpLoop) (cLoop) VALUES ("B")

  *-- Temp. Group File.
  CREATE CURSOR (lcTmpGroup) (cGroupKey C(35) , nGroup N(6,0) , nPosition N(2,0) ,;
                              Account C(5) , cAddress4 C(30) , cAddress5 C(30) , Region C(6))
  ZAP
  INDEX ON cGroupKey + STR(nGroup,6) TAG (lcTmpGroup) OF ;
           (gcWorkDir+lcTmpGroup+".CDX")
  SET ORDER TO (lcTmpGroup) DESCENDING
ENDIF

*-- Temp. Account File.
CREATE CURSOR (lcTmpAcct) FROM ARRAY laAccStru
ZAP
INDEX ON cGroupKey TAG (lcTmpAcct) OF (gcWorkDir+lcTmpAcct+".CDX")

*-- Temp Transaction File
CREATE TABLE (lcTmpTrans) FROM ARRAY laTranStru
ZAP
*-- 

*INDEX ON cGroupKey + STR(nGroup,6)+ History + IIF(llMulCurr AND lcRpCurr="F",cCurrCode,"")  + DTOS(TranDate) + Tran ;
      TAG (lcTmpTrans) OF (gcWorkDir+lcTmpTrans+".CDX")
INDEX ON cGroupKey + STR(nGroup,6) + IIF(llMulCurr AND lcRpCurr="F",cCurrCode,"")  + History + DTOS(TranDate) + Tran ;
      TAG (lcTmpTrans) OF (gcWorkDir+lcTmpTrans+".CDX")      

*-- Temp. Currency File
*-- If Multi currency create this file
IF llMulCurr
  CREATE CURSOR (lcTmpCurrc) (cCurrCode C(3) , nDRate N(15,6) , nURate N(15,6) )
  INDEX ON cCurrCode TAG (lcTmpCurrc) OF (gcWorkDir+lcTmpCurrc+".CDX")
  ZAP
  
  CREATE CURSOR (lcTmpAcCur) (cGroupKey C(35) , Account C(5) , cAddress4 C(30) ,;
                              cAddress5 C(30) , Region C(6) , cCurrCode C(3) ,;
                              Current N(20,2) , Age30 N(20,2) , Age60 N(20,2) , ;
                              Age90 N(20,2) , Age120 N(20,2))
  INDEX ON cGroupKey + cCurrCode TAG (lcTmpAcCur) OF (gcWorkDir+lcTmpAcCur+".CDX")
  ZAP
ENDIF

*-- End of lpCreatFil.
*:**************************************************************************
*: Name      : lpCollect
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Collecting Data for Report 
*:**************************************************************************
*: Example   : DO lpCollect
*:**************************************************************************
*
PROCEDURE lpCollect
*-- Intialize variables that calculates data from transaction files. [begin]
STORE 0.00 TO m.Current , m.Age30 , m.Age60 , m.Age90 , m.Age120 , ;
              m.OpenCr , m.Totage , m.NetBal , m.Age00 ,;
              m.nGroup , m.nPostDChq , m.nHstAmount ,;
              m.cAddress4 , m.cAddress5 , m.Region

PRIVATE lnCurrent , lnAge30 , lnAge60 , lnAge90 , lnAge120 , ;
        lnOpenCr , lnTotAge , lnNetBal , lnAge00 , lnAmount

STORE 0 TO lnCurrent , lnAge30 , lnAge60 , lnAge90 , lnAge120 , ;
           lnOpenCr , lnTotAge , lnNetBal , lnAge00 , lnAmount
*-- Intialize variables that calculates data from transaction files. [End]

*-- Insert Debit Transactions ...
IF lcRpTrans $ 'DB'
  DO lpInsDebit
ENDIF  

*-- Insert Credit Transactions ...
IF lcRpTrans $ 'CB'
  DO lpInsCredt
ENDIF  

*-- function to Collect the data from History File.
DO lpCollhist


*-- End of lpCollect.
*:**************************************************************************
*: Name      : lpInsDebit
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Scan Debit for Records matching Criteria
*:**************************************************************************
*: Called from : lpCollect
*:**************************************************************************
*: Example   : DO lpInsDebit
*:**************************************************************************
*
PROCEDURE lpInsDebit
PRIVATE lcScanExpr , lcInstalNo

*-- Set relation with customer file.
SELECT DEBIT
SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER  && Relation with customer.
STORE '' TO m.cAddress4 , m.cAddress5 , m.Region

IF EMPTY(lcDebitFlt)
  lcScanExpr = ALLTRIM(lcRpExp)
ELSE
  lcScanExpr = ALLTRIM(lcRpExp + IIF(EMPTY(lcRpExp),"",[ AND ]) + lcDebitFlt)
ENDIF

*-- Scan around Debit file for records matching criteria
SCAN FOR &lcScanExpr
  m.cAddress4 = Customer.cAddress4 
  m.cAddress5 = Customer.cAddress5
  m.Region    = Customer.Region
  lcInstalNo = cInstalNo
  lnAmount = 0

  *C037891,1 BWA 03/24/2004 Variable hold the val amount.
  m.VatAmnt = 0
  *C037891,1 BWA 03/24/2004.

  SCATTER MEMVAR

  *-- Function to get the current amount until this date.
  = lfCurrAmt()
  
  **-- If Company Multi Currency and display by Equivalent Amount
  IF llMulCurr AND Amount <> 0 AND (cCurrCode <> gcBaseCurr)
    lnAmount = ROUND(lfAmntDisp(M.Amount,"O",ldRpExDate,lcRpTmpCur),2)
    
    *-- Checking for the occurence of this currency with "O" type
    IF !SEEK(cCurrCode,lcTmpCurrc)
      INSERT INTO (lcTmpCurrc) (cCurrCode) VALUES (Debit.cCurrCode)
    ENDIF
    
    m.nORate = lfAmntDisp(1,"O",ldRpExDate,lcRpTmpCur)
  ELSE
    lnAmount = Amount
    m.nORate = 1
  ENDIF

  *C037891,1 BWA 03/24/2004 Get the value of the vat amount from the Invhdr file.
  *--Fill the vat amount field with the value from Rethdr.TaxAmt in case Credit Memo.
  IF DEBIT.TRANTYPE = "1"
	 lcCrdtm = DEBIT.ACCOUNT + DEBIT.TRAN
	 IF SEEK(lcCrdtm,(lcInvHdr))
	   m.VatAmnt = &lcInvHdr..Tax_Amt
	 ENDIF
  ENDIF
  *C037891,1 BWA 03/24/2004. 
  
  DO lpDebDeal   && Calculate Ages and fill memory variables.
  DO lpInsRecrd  && Insert New Record in Transaction Temporary File and Totals Temporary file.
  
ENDSCAN  && end Scan Debit records achieve both customer and debit filters.
*-- EndScan around Debit file for records matching criteria

SET RELATION TO  && Rest relation.

*-- End of lpInsDebit.
*:**************************************************************************
*: Name      : lpDebDeal
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : To Calculate the Debit Aging 
*:**************************************************************************
*: Called from : lpInsDebit
*:**************************************************************************
*: Example   : DO lpDebDeal
*:**************************************************************************
*
PROCEDURE lpDebDeal

*-- if Age By Date
IF lcAgeType = 'D'
  =lfUpdtAge('D',ldRpHDate - TranDate)  && Update Date ages
ELSE  && else if Age by Terms
   ldDueDate = IIF(EMPTY(DueDate),TranDate+30,DueDate)
  =lfUpdtAge('D',ldRpHDate - ldDueDate)  && Update Term ages
ENDIF  && end if Age By Date

*-- Save total age and net balance per transaction
STORE m.Amount TO m.TotAge , m.NetBal

STORE lnAmount TO lnTotAge , lnNetBal

*-- End of lpDebDeal.
*:**************************************************************************
*: Name      : lpInsCredt
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Scan Credit for records matching criteria
*:**************************************************************************
*: Called from : lpCollect
*:**************************************************************************
*: Example   : DO lpInsCredt
*:**************************************************************************
*
PROCEDURE lpInsCredt
PRIVATE lcInstalNo
*-- Set relation with customer file.
SELECT CREDIT
SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER  && Relation with customer.
STORE '' TO m.cAddress4 , m.cAddress5 , m.Region

*-- Scan around Credit file for records matcjing criteria
SCAN FOR &lcRpExp
  lnAmount = 0
  m.cAddress4 = Customer.cAddress4 
  m.cAddress5 = Customer.cAddress5
  m.Region    = Customer.Region

  *C037891,1 BWA 03/24/2004 Variable hold the vat amount.
  m.VatAmnt = 0
  *C037891,1 BWA 03/24/2004.

  SCATTER MEMVAR
   
  lcInstalNo = ''
  
  *-- Function to get the current amount until this date.
  =lfCurCdAmt()
  
  IF llMulCurr AND M.Amount <> 0 AND (cCurrCode <> gcBaseCurr)
    
    lnAmount = ROUND(lfAmntDisp(M.Amount,"O",ldRpExDate,lcRpTmpCur),2)

    *-- Checking for the occurence of this currency with "O" type
    IF !SEEK(cCurrCode,lcTmpCurrc)
      INSERT INTO (lcTmpCurrc) (cCurrCode) VALUES (Credit.cCurrCode)
    ENDIF

    m.nORate = lfAmntDisp(1,"O",ldRpExDate,lcRpTmpCur)    
  ELSE
    lnAmount = M.Amount
    m.nORate = 1
  ENDIF  

  *C037891,1 BWA 03/24/2004 Get the value of the vat amount from the Rethdr file.
  *--Fill the vat amount field with the value from Rethdr.TaxAmt in case Credit Memo.
  IF CREDIT.TRANTYPE = "0"
	 lcCrdtm = CREDIT.ACCOUNT + CREDIT.TRAN
	 IF SEEK(lcCrdtm,(lcRetHdr))
	   m.VatAmnt = &lcRetHdr..Tax_Amt * -1
	 ENDIF
  ENDIF
  *C037891,1 BWA 03/24/2004.

  DO lpCreDeal   && Calculate Ages and fill memory variables (OpenCr and NetBal)
  DO lpInsRecrd  && Insert New Record in Transaction Temporary File and Totals Temporary file.

ENDSCAN  && end Scan Credit records achieve both customer and Credit filters.
*-- EndScan around Credit file for records matcjing criteria

SET RELATION TO  && Rest relation.

*-- End of lpInsCredt.
*:**************************************************************************
*: Name      : lpCreDeal
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : To Calculate the Credit Aging
*:**************************************************************************
*: Called from : lpInsCredt
*:**************************************************************************
*: Example   : DO lpCreDeal
*:**************************************************************************
*
PROCEDURE lpCreDeal

=lfUpdtAge('C',gdSysDate - TranDate) && Update Date ages

*-- Save open credit and net balance per transaction
STORE m.Amount TO m.OpenCr , m.NetBal

STORE lnAmount TO lnOpenCr , lnNetBal

*-- End of lpCreDeal.
*:**************************************************************************
*: Name      : lfUpdtAge
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Calculate the Aging 
*:**************************************************************************
*: Called from : lpCreDeal OR lpDebDeal
*:**************************************************************************
*: Passed Parameters : lcUpdtTyp --> 'D' for Debit , 'C' for Credit
*:                     lnAgeDays --> No of Days left  
*:**************************************************************************
*: Example   : =lfUpdtAge('C',gdSysDate - TranDate)
*:**************************************************************************
*
FUNCTION lfUpdtAge
PARAMETERS lcUpdtTyp , lnAgeDays

*-- Intialize variables that calculate data from transaction files. [begin]
STORE 0.00 TO m.Current , m.Age30 , m.Age60 , m.Age90 , m.Age120 , ;
              m.OpenCr , m.Totage , m.NetBal , m.Age00 ,;
              m.nGroup , m.nPostDChq

STORE 0 TO lnCurrent , lnAge30 , lnAge60 , lnAge90 , lnAge120 , ;
           lnOpenCr , lnTotAge , lnNetBal , lnAge00
*-- Intialize variables that calculates data from transaction files. [End]

*-- if no parameter passed (i.e. want to intializing only) then return...
*-- does occur in this program but it may be.
IF TYPE('lcUpdtTyp') $ 'UL'
  RETURN
ENDIF

*-- lcAges : Varaible Hold Field description (Age00, 30 , 60 , 90 , and 120)
*-- if Debit
IF lcUpdtTyp = 'D'

  *-- if Age By Date.
  IF lcAgeType = 'D'
    lcAges = 'm.Age'                                                    +;
      IIF(lnAgeDays >= 120,'120',IIF(lnAgeDays >= 90 ,'90'   ,;
      IIF(lnAgeDays >= 60 ,'60' ,IIF(lnAgeDays >= 30 ,'30','00')))) 
  ELSE  && else if Age By Terms.
    lcAges = 'm.Age'                                                      +;
      IIF(lnAgeDays >= 91,'120',IIF(lnAgeDays >= 61 ,'90' ,;
      IIF(lnAgeDays >= 31 ,'60' ,IIF(lnAgeDays >= 1 ,'30','00'))))   
  ENDIF  && end if Age By Date.

ELSE  && else if Credit
  lcAges = 'm.Age'                                                      +;
    IIF(lnAgeDays >= 120,'120',IIF(lnAgeDays >= 90 ,'90' ,;
    IIF(lnAgeDays >= 60 ,'60' ,IIF(lnAgeDays >= 30 ,'30','00')))) 
ENDIF  && end if Debit

&lcAges   = m.Amount       && Fill Age variable
m.Current = m.Age00

PRIVATE lcTotAccAg         && Fill the Accumulated Age Variable in lcTmpAcct
lcTotAccAg  = STRTRAN(lcAges,"m.","ln")
&lcTotAccAg = lnAmount
lnCurrent   = lnAge00

*-- End of lfUpdtAge.
*:**************************************************************************
*: Name      : lpGetAmnt
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : To Calculate the amount from History file
*:**************************************************************************
*: Called from : lpInsDebit OR lpInsCredt
*:**************************************************************************
*: Example   : DO lpGetAmnt
*:**************************************************************************
*
PROCEDURE lpGetAmnt 
PARAMETER lcInstalNo

*-- If there is Payment in AR History file
m.nHstAmount = 0
IF SEEK(m.Account + m.Tran + lcInstalNo,'ArHist') 
  m.nHstAmount = ArHist.OpenAmt + ArHist.Amount
ELSE
  m.nHstAmount = m.Amount
ENDIF
*-- EndIf of Payment in AR History file

*-- End of lpGetAmnt.  
*:**************************************************************************
*: Name      : lpInsRecrd
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : To Calculate the Credit Aging 
*:**************************************************************************
*: Called from : lpInsDebit OR lpInsCredt
*:**************************************************************************
*: Example   : DO lpInsRecrd
*:**************************************************************************
*
PROCEDURE lpInsRecrd

m.cGroupKey = EVALUATE(lcReplExpr)

*-- negative number is assigned to nGroup field in Transaction file 
*-- because index is sorted by nGroup so when we add the incremented 
*-- positive numbers for Group Position in Group change proc. with 
*-- SCAN/ENDSCAN the line remains in its position 
*-- AND
*-- m.nGroup must take value -1 to get the nGroup field begin with -1 not 0 
*-- to treat the case of the instalment of the invoices.

m.nGroup = -1
m.nGroup = -99999

INSERT INTO (lcTmpTrans) FROM MEMVAR         && Insert Transaction Record

*-- If cGroupKey with this currency is found in Temp Acc Currency file
IF llMulCurr
  IF SEEK(&lcTmpTrans..cGroupKey+&lcTmpTrans..cCurrCode,lcTmpAcCur)
    SELECT (lcTmpAcCur)
    REPLACE Current WITH Current + &lcTmpTrans..Current ,;
            Age30   WITH Age30   + &lcTmpTrans..Age30   ,;
            Age60   WITH Age60   + &lcTmpTrans..Age60   ,;
            Age90   WITH Age90   + &lcTmpTrans..Age90   ,;
            Age120  WITH Age120  + &lcTmpTrans..Age120
  ELSE

    INSERT INTO (lcTmpAcCur) (cGroupKey , Account , cAddress4 , cAddress5 , Region ,;
                              cCurrCode , Current , Age30 , Age60 , Age90 , Age120) ;
           VALUES (&lcTmpTrans..cGroupKey , &lcTmpTrans..Account , &lcTmpTrans..cAddress4 , ;
                   &lcTmpTrans..cAddress5 , &lcTmpTrans..Region , &lcTmpTrans..cCurrCode,;
                   &lcTmpTrans..Current , &lcTmpTrans..Age30 ,&lcTmpTrans..Age60 , ;
                   &lcTmpTrans..Age90 , &lcTmpTrans..Age120)
  ENDIF
ENDIF


*-- if Find this account in Temporary totals file.
IF SEEK(m.cGroupKey,lcTmpAcct)
  SELECT (lcTmpAcct)
  REPLACE Current WITH Current + lnCurrent ,;
            Age30 WITH Age30   + lnAge30   ,;
            Age60 WITH Age60   + lnAge60   ,;
            Age90 WITH Age90   + lnAge90   ,;
           Age120 WITH Age120  + lnAge120  ,;
           TotAge WITH TotAge  + lnTotAge  ,;
           OpenCr WITH OpenCr  + lnOpenCr  ,;
           NetBal WITH NetBal  + lnNetBal

ELSE  && else if this account not found before.

  *-- Add new record in temporary total files.
  WAIT WINDOW 'Collect data for Customer ' + Account NOWAIT

  *-- Adding postDated Cheques to lcTmpAcct file in single currency comp.
  IF !llMulCurr AND SEEK(m.Account,'PostDChq')
    SELECT PostDChq
    SUM Amount REST WHILE Account = m.Account TO m.nPostDChq
  ENDIF

  INSERT INTO (lcTmpAcct) (Current, Age30, Age60, Age90, Age120, OpenCr,;
               TotAge, NetBal, cGroupKey, Account, BtName, cAddress4,;
               cAddress5, Region, nPostDchq, cFacCode) VALUES ;
              (lnCurrent, lnAge30, lnAge60, lnAge90, lnAge120, lnOpenCr,;
               lnTotAge, lnNetBal, m.cGroupKey, m.Account, Customer.BtName,;
               Customer.cAddress4, Customer.cAddress5, Customer.Region,;
               m.nPostDchq, Customer.cFacCode)
ENDIF  && End if Find this account in Temporary totals file.

*-- End of lpInsRecrd.
*:**************************************************************************
*: Name      : lfGetReplc
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Get Replaced expression.
*:**************************************************************************
*: Called from : Report code section.
*:**************************************************************************
*: Returns   : lcExpr ---> which means (Sort by expression) 
*:**************************************************************************
*: Example   : =lfGetReplc()
*:**************************************************************************
*
FUNCTION lfGetReplc
PRIVATE lcExpr

DO CASE
  *-- Sort By Account
  CASE lcRpSort='A'
    lcExpr   = [Account]

  *-- Sort by Country
  CASE lcRpSort = 'C'
    lcExpr   = [cAddress4+Account]
    
  *-- Sort by Zip Code
  CASE lcRpSort = 'Z'
    lcExpr   = [cAddress5+Account]
    
  *-- Sort by Region
  CASE lcRpSort = 'R'
    lcExpr   = [Region+Account]
ENDCASE

RETURN lcExpr

*-- End of lfGetReplc
*:**************************************************************************
*: Name      : lfGetAddr
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : get the address of the customer
*:**************************************************************************
*: Example   : = lfGetAddr()
*:**************************************************************************
*
FUNCTION lfGetAddr

laSoldTo = ''

IF SEEK('M'+Account,'Customer')
  = gfGetAdr('Customer', '' , '' , '' , @laSoldTo , 1)
ENDIF

=lfAdrShift('laSoldTo')
RETURN ''

*-- End of lfGetAddr
*:**************************************************************************
*: Name      : lfGetFac
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : get the address of the Factor
*:**************************************************************************
*: Example   : = lfGetFac()
*:**************************************************************************
*:
FUNCTION lfGetFac
PARAMETER llDummy

laFactTo = ''
lcFacName = ''

IF lcRpRemit='F' AND !EMPTY(lcRpFactor) AND SEEK(lcRpFactor,'SYCFACT')
  lcFacName = SYCFACT.cfaccomp
  =gfGetAdr('SycFact', '' , '' , '' , @laFactTo , 1)
  =lfAdrShift('laFactTo')
ENDIF

*-- End of lfGetFac
*:**************************************************************************
*: Name      : lfEndGroup
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Indicate end of group 
*:**************************************************************************
*: Called from : .FRX group footer (Report Form B)
*:**************************************************************************
*: Example     : = lfEndGroup()
*:**************************************************************************
*
FUNCTION lfEndGroup

llEndGrp = .T.

RETURN ''

*-- End of lfEndGroup.
*:**************************************************************************
*: Name      : lfOverMsg
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Evaluate which over message to print.
*:**************************************************************************
*: Passed Parameters  : All parameters are called By Reference :
*:                    : OverMsg1, OverMsg 2, OverMsg3
*:**************************************************************************
*: Returns            : Null, But fill Over Messages.
*:**************************************************************************
*: Example   : = lfOverMsg(@lcOverMsg1 , lcOverMsg2 , lcOverMsg3)
*:**************************************************************************
*
FUNCTION lfOverMsg
PARAMETERS lcOver1 , lcOver2 , lcOver3
PRIVATE lcFileTot
STORE '' TO lcOver1 , lcOver2 , lcOver3

IF llMulCurr AND lcRpCurr="F" AND SEEK(&lcTmpTrans..cGroupKey + &lcTmpTrans..cCurrCode,lcTmpAcCur)
  lcFileTot = lcTmpAcCur
ELSE
  lcFileTot = lcTmpAcct
ENDIF

DO CASE
  CASE &lcFileTot..Age120 > 0
    lcOver1 = m120L1
    lcOver2 = m120L2
    lcOver3 = m120L3
  CASE &lcFileTot..Age90  > 0
    lcOver1 = m90L1
    lcOver2 = m90L2
    lcOver3 = m90L3
  CASE &lcFileTot..Age60  > 0
    lcOver1 = m60L1
    lcOver2 = m60L2
    lcOver3 = m60L3
  CASE &lcFileTot..Age30  > 0
    lcOver1 = m30L1
    lcOver2 = m30L2
    lcOver3 = m30L3
ENDCASE

RETURN ''

*-- End of lfOverMsg.
*:**************************************************************************
*: Name      : lfCollTime
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : To return the time of collecting data
*:**************************************************************************
*: Example   : = lfCollTime(lcStTime,lcEdTime)
*:**************************************************************************
*
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)

*-- End of lfCollTime.
*:**************************************************************************
*: Name      : lpCurrChng
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : To Apply the Currency Change
*:**************************************************************************
*: Example   : DO lpCurrChng
*:**************************************************************************
*
PROCEDURE lpCurrChng
PRIVATE lnAlias , lnAmount
lnAlias = SELECT()      && Save current Table

*-- If Currency is not Foreign Or Original
IF (lcRpCurr $ "DU") AND !(lcRpCurr == lcSavedCur)
  
  SELECT (lcTmpCurrc)
  *-- Scan Loop around Temp Original Currency File 
  SCAN
    lnAmount = lfAmntDisp(1,lcRpCurr,ldRpExDate,lcRpTmpCur)
    REPLACE nDRate WITH lnAmount , nURate WITH lnAmount            
  ENDSCAN  
  *-- EndScan Loop around Temp Original Currency File 
ENDIF

SELECT (lnAlias)         && Restore Old Table

*-- End of lpCurrChng
*:**************************************************************************
*: Name      : lpChSortBy
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : To Apply Changing Sort by	
*:**************************************************************************
*: Example   : DO lpChSortBy
*:**************************************************************************
*
PROCEDURE lpChSortBy
PRIVATE lnAlias , lcSortExpr , lcSortVal
*-- lcSortExpr -->  Variable to check for Sort Expression
lnAlias = SELECT()      && Save current Table

SELECT (lcTmpAcct)
LOCATE
lcSortExpr = lfGetReplc()   && cGroupkey Evaluation for first record
lcSortVal  = EVALUATE(lcSortExpr)

IF !SEEK(lcSortVal,lcTmpAcct)

  SELECT (lcTmpAcct)
  REPLACE ALL cGroupKey WITH EVALUATE(lcSortExpr)
  
  IF lcFormType = "A"
    SELECT (lcTmpGroup)
    REPLACE ALL cGroupKey WITH EVALUATE(lcSortExpr)
  ENDIF    

  SELECT (lcTmpTrans)
  REPLACE ALL cGroupKey WITH EVALUATE(lcSortExpr)
  
  *-- If Company is Multi Currency
  IF llMulCurr
    SELECT (lcTmpAcCur)
    REPLACE ALL cGroupKey WITH EVALUATE(lcSortExpr)
  ENDIF  
  *-- Endif of Company is Multi Currency.
ENDIF

SELECT (lnAlias)         && Restore Old Table

*-- End of lpChSortBy
*:**************************************************************************
*: Name      : lpApplyAge
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : To Apply the Aging Filter 
*:**************************************************************************
*: Example   : DO lpApplyAge
*:**************************************************************************
*
PROCEDURE lpApplyAge
lcAgeFlt = ''

*-- Account has Total Credit Balance
IF !llRpPrnBal
  IF llRpPrZero
    lcAgeFlt = IIF(EMPTY(lcAgeFlt),"",[ AND ]) + [NetBal >= 0]
  ELSE
    lcAgeFlt = IIF(EMPTY(lcAgeFlt),"",[ AND ]) + [NetBal > 0]
  ENDIF  
ENDIF

IF !llRpPrZero AND !("NetBal" $ lcAgeFlt)
  lcAgeFlt = lcAgeFlt + IIF(EMPTY(lcAgeFlt),"",[ AND ]) + [NetBal <> 0]
ENDIF  
  
*-- Add a filter due to the choice of aging
IF llRp30 OR llRp60 OR llRp90 OR llRp120
  lcAgeFlt = lcAgeFlt + IIF(EMPTY(lcAgeFlt),"",[ AND ])
  DO CASE
    CASE llRp30
      lcAgeFlt = lcAgeFlt + [((Age30 > 0) OR (Age60 > 0) OR (Age90 > 0) OR (Age120 > 0))]
    CASE llRp60
      lcAgeFlt = lcAgeFlt + [((Age60 > 0) OR (Age90 > 0) OR (Age120 > 0))]
    CASE llRp90
      lcAgeFlt = lcAgeFlt + [((Age90 > 0) OR (Age120 > 0))]
    CASE llRp120
      lcAgeFlt = lcAgeFlt + [(Age120 > 0)]
  ENDCASE
ENDIF  

*-- If user Changed Age Filter
IF !(lcAgeFlt == lcOldAgeFl)
  SELECT (lcTmpAcct)
  IF !EMPTY(lcAgeFlt)
    SET FILTER TO &lcAgeFlt
  ENDIF
ENDIF
*-- EndIf of user Changed Age Filter 

*-- End of lpApplyAge.
*:**************************************************************************
*: Name      : lpGroupChg
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : To Apply the Aging Filter 
*:**************************************************************************
*: Example   : DO lpGroupChg
*:**************************************************************************
*
PROCEDURE lpGroupChg

*-- If Form is A and user changed Filter or currency is changed 
IF (lcFormType = "A") AND (!(lcAgeFlt == lcOldAgeFl) OR ;
   (!(lcSavedCur == lcRpCurr) AND (lcRpCurr = "F" OR lcSavedCur = "F")) OR ;
   EMPTY(lcSavedCur))

  PRIVATE lcCurrCode , lcGroupKey , lnGroup , lnPosition , ;
          lcScanCond , lnCount , lcOldCurr , lcDateType
          
  *-- This function used to break all the relations before recollecting.
  =lfBreakRel()
    
  SELECT (lcTmpTrans)
  IF !llOGFltCh
    DELETE FOR EMPTY(Account)  
    PACK

    *-- This Replace to make sure that the added line will not move to 
    *-- End of Group according to the Index

    *B605492,1 Replace nGroup with -99999. [Begin]
    *REPLACE ALL nGroup WITH -1  
    REPLACE ALL nGroup WITH -99999  
    *B605492,1 Replace nGroup with -99999. [End]

  ENDIF

  *-- Scan loop around Temp Account file
  SELECT (lcTmpAcct)

  lcScanCond = IIF(EMPTY(lcAgeFlt),".T.",lcAgeFlt)
  SCAN FOR &lcScanCond
    lcGroupKey = cGroupKey   && Save current cGroupKey
      
    *-- If cGroupKey is found in lcTmpTrans file
    IF SEEK(lcGroupKey,lcTmpTrans)
      SELECT (lcTmpTrans)
      lcCurrCode = cCurrCode   && Save current cCurrCode within same cGroupKey
    
      lnPosition = 0
      lnGroup    = 1
    
      *-- Scan loop around Transaction file for same cGroupKey
      SCAN REST WHILE cGroupKey = lcGroupKey                    
        lnPosition = lnPosition + 1

        *-- If position exceeded number of lines in page(lnRpLines) OR 
        *-- Multi Currency and Display Type is Foreign And currency is changed
        IF llMulCurr AND lcRpCurr = "F" AND !(cCurrCode == lcCurrCode) OR ;
          lnPosition > lnRpLines
    
          *-- in Order to keep the Detail band in FORM A with fixed size we
          *-- have to add empty no of lines in transaction file
          *-- If no of lines less than the required lines for each Page (lnRpLines)
          IF lnPosition < lnRpLines
            SCATTER MEMVAR BLANK         
            m.cGroupKey = lcGroupKey
            m.nGroup    = lnGroup
            m.cCurrCode = lcCurrCode
            m.TranDate  = {09/09/9999}
            
            *sameh SSE
            m.History = 'zzzzzz'
            *sameh SSE

            lcOldCurr = cCurrCode
            *-- For Loop till the end of the required lines
            FOR lnCount = lnPosition TO lnRpLines
              m.nPosition = lnCount
              *-- add empty records till the end of the required lines for each page
              INSERT INTO (lcTmpTrans) FROM MEMVAR    
            ENDFOR
            SKIP
          ENDIF
          *-- Endif of no of lines less than required lines
                    
          lcCurrCode = cCurrCode    && Save current cCurrCode within same cGroupKey
          STORE 1 TO lnPosition     && Reset lnPosition 

          lnGroup = lnGroup + 1        
          *-- Add new group no AND Reset Position To 1               
          REPLACE nGroup     WITH lnGroup ,;        
                  nPosition  WITH lnPosition
      
          INSERT INTO (lcTmpGroup) (cGroupKey , nGroup , nPosition,;
                                    Account , cAddress4 , cAddress5 , Region);
                 VALUES (lcGroupKey , lnGroup , lnPosition ,;
                         &lcTmpAcct..Account , &lcTmpAcct..caddress4 ,;
                         &lcTmpAcct..caddress5 , &lcTmpAcct..Region) 

        ELSE       && Position is less than number of lines
 
          *-- Add current Group no AND Add current Position no
          REPLACE nGroup     WITH lnGroup ,;        
                  nPosition  WITH lnPosition        

          *-- Get Current cGroupKey in Temp Group File to update its position
          IF SEEK(lcGroupKey,lcTmpGroup)
            REPLACE &lcTmpGroup..nPosition WITH lnPosition  
          ELSE
            INSERT INTO (lcTmpGroup) (cGroupKey , nGroup , nPosition,;
                                      Account , cAddress4 , cAddress5 , Region);
                   VALUES (lcGroupKey , lnGroup , lnPosition ,;
                           &lcTmpAcct..Account , &lcTmpAcct..caddress4 , ;
                           &lcTmpAcct..caddress5 , &lcTmpAcct..Region) 
          ENDIF

          lcOldCurr = cCurrCode
        ENDIF
        *-- EndIf of position exceeded number of lines in page(lnRpLines)
      
      ENDSCAN
      *-- EndScan loop around Transaction file for same cGroupKey

      *-- If no of lines less than the required lines for each Page (lnRpLines)
      IF lnPosition < lnRpLines
        SCATTER MEMVAR BLANK         
        m.cGroupKey = lcGroupKey
        m.nGroup    = lnGroup
        m.cCurrCode = lcOldCurr
        m.TranDate  = {09/09/9999}

        *-- For Loop till the end of the required lines
        FOR lnCount = lnPosition + 1 TO lnRpLines
          m.nPosition = lnCount
              
          *-- add empty records till the end of the required lines for each page
          m.History = 'zzzzzz'
          INSERT INTO (lcTmpTrans) FROM MEMVAR    
        ENDFOR
        SKIP
      ENDIF
      *-- Endif of no of lines less than required lines
      
    ENDIF
    *-- Endif of cGroupKey is found in lcTmpTrans File
  ENDSCAN
  *-- EndScan loop around Temp Account file
ENDIF
*-- If Form is A and user changed Filter or currency is changed 

*-- End of lpGroupChg  
*:**************************************************************************
*: Name      : lpApplyRel
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : To Apply the Relation before displaying of Report
*:**************************************************************************
*: Example   : DO lpApplyRel
*:**************************************************************************
*
PROCEDURE lpApplyRel

*-- Setting Proper Relation before Displaying the Report
lnAlias = SELECT()      && Save current Table

**************** Form A Relations ****************************
**************** ---------------- ***************************
*-- Forming the following Relations
*   lcTmpAcct
*           |__ 
*              lcTmpGroup
*                       |__
*                       |__lcTmpLoop
*                                  |__
*                                     lcTmpTrans
*                                               |__
*                                                  lcTmpCurrc
**************************************************************


**************** Form B Relations ****************************
**************** ---------------- ****************************
*   lcTmpAcct
*           |__
*              lcTmpTrans
*                       |__
*                          lcTmpCurrc
**************************************************************

SELECT (lcTmpAcct)

DO CASE

  *-- If Report is Form A
  CASE lcFormType = "A"
    SET RELATION TO cGroupKey INTO (lcTmpGroup)

    SELECT (lcTmpGroup)
    SET ORDER TO (lcTmpGroup) ASCENDING
    SET RELATION TO "" INTO (lcTmpLoop)

    SELECT (lcTmpLoop)
    SET RELATION TO &lcTmpGroup..cGroupKey + STR(&lcTmpGroup..nGroup,6) INTO (lcTmpTrans)
  
    IF llMulCurr
      SELECT (lcTmpTrans)
      SET RELATION TO &lcTmpTrans..cCurrCode INTO (lcTmpCurrc)
    ENDIF    

    SELECT (lcTmpAcct)
    SET SKIP TO (lcTmpGroup) , (lcTmpLoop) , (lcTmpTrans)

  *-- If Report is Form B
  CASE lcFormType = "B"
    SET RELATION TO cGroupKey INTO (lcTmpTrans)
    SET SKIP TO (lcTmpTrans)

    IF llMulCurr
      SELECT (lcTmpTrans)
      SET RELATION TO &lcTmpTrans..cCurrCode INTO (lcTmpCurrc)
    ENDIF    
  
  OTHERWISE                  && in Case of @SAY
  
ENDCASE

SELECT (lnAlias)         && Restore Old Table
*-- End of lpApplyRel.

*:**************************************************************************
*: Name      : lfAmntDisp
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Display Foreign Currency in Equivalent Base currency
*:**************************************************************************
*: Example   : =lfAmntDisp
*:**************************************************************************
*: Notes     : This function is a exact copy from gfAmntDisp()
*:**************************************************************************
*
FUNCTION lfAmntDisp
PARAMETER lnAmount,lcRpDispCur,ldExRateDt,lcTmepFile,llAprvCurr,lcGetFile

PRIVATE lnAmount,lcRpDispCur,ldExRateDt,lcTmepFil,llAprvCurr,lcExSin1,lcExSin2,lnSavAlias

lnAmount    = IIF(TYPE('lnAmount') = 'N',lnAmount,0)
lcRpDispCur = IIF(TYPE('lcRpDispCur') ='C',lcRpDispCur,'')
ldExRateDt  = IIF(TYPE('ldExRateDt') = 'D',ldExRateDt,{})
lcTmepFile  = IIF(TYPE('lcTmepFile') = 'C',lcTmepFile,'')
llAprvCurr  = IIF(TYPE('llAprvCurr') = 'L',llAprvCurr,.F.)

lcExSin1    = ''       && Variable to hold the first sign in the equation.
lcExSin2    = ''       && Variable to hold the second sign in the equation.

lnSavAlias  = SELECT(0)  && Variable to save the alias.
lcGetFile   = IIF(TYPE('lcGetFile')$"UL",'',lcGetFile)

DO CASE
  CASE lcRpDispCur = 'F'

  CASE lcRpDispCur = 'O'
    IF EMPTY(lcGetFile)
      lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
    ELSE
      lcCurrCode = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,&lcGetFile..CCURRCODE)
    ENDIF  

    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)

    lnExRate = 0

    IF EMPTY(lcGetFile)
      lnUnit = NCURRUNIT

      lnUnit = IIF(lnUnit = 0 , 1 , lnUnit)

      lnExRate = IIF(llAprvCurr , gfChkRate('lnUnit' , lcCurrCode , DINVDATE , .F.) , NEXRATE)
    ELSE
      lnUnit = &lcGetFile..NCURRUNIT
      lnExRate = IIF(llAprvCurr , gfChkRate('lnUnit' , lcCurrCode , &lcGetFile..DINVDATE , .F.) , &lcGetFile..NEXRATE)
    ENDIF  

    lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
    lnUnit = IIF(lnExRate <> 0 , lnUnit , 1)

    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,6)

  CASE lcRpDispCur = 'D'
    lnExRate   = 0
    lnUnit     = 0
    
    IF EMPTY(lcGetFile)
      lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
    ELSE
      lcCurrCode = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,&lcGetFile..CCURRCODE)
    ENDIF
      
    IF lcCurrCode = gcBaseCurr
      lnExRate = 1
      lnUnit   = 1
    ELSE
      lnExRate   = gfChkRate('lnUnit',lcCurrCode,ldExRateDt,.F.)
    ENDIF

    lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
    lnUnit = IIF(lnExRate <> 0 , lnUnit , 1)

    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,6)

  CASE lcRpDispCur = 'U'

    lnExRate   = 0
    lnUnit     = 0

    IF EMPTY(lcGetFile)
      lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
    ELSE
      lcCurrCode = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,&lcGetFile..CCURRCODE)
    ENDIF  

    IF lcCurrCode = gcBaseCurr
      lnExRate = 1
      lnUnit   = 1
    ELSE
      IF SEEK(lcCurrCode,lcTmepFile)
        lnExRate = &lcTmepFile..NEXRATE
        lnUnit   = &lcTmepFile..NCURRUNIT
      ENDIF
    ENDIF

    lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
    lnUnit = IIF(lnExRate <> 0 , lnUnit , 1)

    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
    
    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,6)
ENDCASE

SELECT (lnSavAlias)

RETURN lnAmount

*-- End of lfAmntDisp.
*:**************************************************************************
*: Name      : lfBreakRel
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : BREAK ALL THE RELATIONS.
*:**************************************************************************
*: Example   : =lfBreakRel()
*:**************************************************************************
*
FUNCTION lfBreakRel

SELECT (lcTmpTrans)
SET RELATION TO

SELECT (lcTmpLoop)
SET RELATION TO

*-- We have to delete all records in Temp Group file to fill it with 
*-- the new Groups number
SELECT (lcTmpGroup)
SET RELATION TO
ZAP
  
SELECT (lcTmpAcct)
SET RELATION TO

*-- END OF lfBreakRel.
*:**************************************************************************
*: Name      : lfvInvoice
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Validation of Only open invoice option.
*:**************************************************************************
*: Example   : =lfvInvoice()
*:**************************************************************************
*:
FUNCTION lfvInvoice

IF llRpInvoie
  llRpPrnCha = .F.
ENDIF

*-- Disable ChargeBack Option in case of Only Open invoice YES
laOGObjCnt[lfVarPos('llRpPrnCha')] = !llRpInvoie
=lfOGShowGet('llRpPrnCha')

*-- End of lfvInvoice.
*:**************************************************************************
*: Name      : lfvRemit
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Validation for the new Remit. 
*:**************************************************************************
*: Example   : =lfvRemit()
*:**************************************************************************
*:
FUNCTION lfvRemit
CLEAR READ
IF lcRpRemit <> 'F'
  lcRpFactor = ''
  llRpRemit = .F.
ELSE
  llRpRemit = .T.
ENDIF

*-- End of lfvRemit.
*:**************************************************************************
*: Name      : lfvFactor
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Validation for the new factor field.
*:**************************************************************************
*: Example   : =lfvFactor()
*:**************************************************************************
*:
FUNCTION lfvFactor

IF !EMPTY(lcRpFactor) AND !SEEK(lcRpFactor,'SycFact')
  PRIVATE lcBrFields , lcAlias
  lcAlias = ALIAS()
  lcBrFields  = [cFacCode:H='Factor',cFacComp:H='Name',cFacCont:H='Contact',cPhoneNo :P= gfPhoneTem() :H='Phone']
  SELECT SycFact
  lcRpFactor = IIF(ARIABROW('',"Factors",gnBrFSRow1, gnBrFSCol1,;
                  gnBrFSRow2, gnBrFSCol2,'','','cFacCode','laBrowArr'),;
                  SycFact.cFacCode,SPACE(6))
  SELECT (lcAlias)
ENDIF

*-- End of lfvFactor.
*:**************************************************************************
*: Name      : lfCutDate
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Get Cut off date.
*:**************************************************************************
*: Example   : =lfCutDate()
*:**************************************************************************
*:
FUNCTION lfCutDate
PRIVATE lnTranDate
lnTranDate = lfItmPos('DEBIT.TRANDATE')   && get Tran. Date Fixed filter Position
ldCutDate = {}

*-- If Trans. Date found in Fixed filter option
IF lnTranDate > 0
  ldCutDate = CTOD(SUBSTR(laOGFxFlt[lnTranDate,6],12,10))
ENDIF
ldCutDate = IIF(EMPTY(ldCutDate),gdsysDate,ldCutDate)

*-- End of lfCutDate.
*:**************************************************************************
*: Name      : lfupdhdate
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : update the cut off date.
*:**************************************************************************
*: Example   : =lfupdhdate()
*:**************************************************************************
*:
FUNCTION lfupdhdate

PRIVATE lnCutofDat
lnCutofDat = lfItmPos('DEBIT.TRANDATE')   && get Tran. Date Fixed filter Position

*-- If Trans. Date found in Fixed filter option
IF lnCutofDat > 0
  ldRpHDate = IIF(EMPTY(laOGFxFlt[lnCutofDat,6]),ldRpHDate,CTOD(SUBSTR(laOGFxFlt[lnCutofDat,6],12,10)))
ENDIF
ldCutDate = IIF(EMPTY(ldCutDate),gdsysDate,ldCutDate)

*-- End of lfupdhdate.
*:*************************************************************
*: Name      : lpCollhist
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : function to Collect the data from History File.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : DO lpCollhist
*:*************************************************************
*:
PROCEDURE lpCollhist

PRIVATE lcInstalNo , lnPrvsAlis , lcOldOrder

lnPrvsAlis = SELECT()

lcTempHist = gfTempName()
SELECT ARHIST
lcOldOrder = ORDER()
SET ORDER TO Arhistht
= AFIELDS(laFileStru)
= gfCrtTmp(lcTempHist,@laFileStru,[Account],lcTempHist)

lcTmpInve = gfTempName()

CREATE TABLE (lcTmpInve) (Tran C(6) , TranType C(1))
INDEX ON TRAN+TranType  TAG (lcTmpInve)
*-- Set relation with customer file.
SELECT ARHIST
SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER  && Relation with customer.
STORE '' TO m.cAddress4 , m.cAddress5 , m.Region



SELECT (lcTmpTrans)
lcOldOrdr = ORDER()
INDEX ON Account+Tran TAG 'lcTmpTran1' ADDITIVE
SET ORDER TO lcTmpTran1
*-- Add New Variable to know it the file at EOF.
STORE .F. TO llEofTran 
STORE .T. TO llFirstTim
IF EOF()
  llEofTran = .T.
ENDIF
SELECT ARHIST

llLoop  = .F.
*-- Scan around Credit file for records matcjing criteria
ldStartDat = ldRpHDate - Day(ldRpHDate) + 1
SCAN FOR &lcRpExp 
  llLoop  = .F.
  IF BETWEEN(HistDate,ldStartDat,ldRpHDate)
    IF (Trantype $ '158' .AND. SEEK(Account+Tran,'DEBIT'))
      SELECT DEBIT
      LOCATE REST WHILE Account+Tran = ARHIST.ACCOUNT +ARHIST.Tran FOR Trantype = ARHIST.Trantype
      IF FOUND()
        llLoop = .T.
      ENDIF
      SELECT ARHIST
    ENDIF
    
    IF SEEK(Account+Tran,'CREDIT')
      llLoop  = .T.
    ENDIF   
    IF llLoop 
      LOOP
    ENDIF
    
    SCATTER MEMVAR MEMO
    INSERT INTO (lcTempHist) FROM MEMVAR
    REPLACE &lcTempHist..Cowner WITH ''
  ELSE
   IF BETWEEN(Trandate,ldStartDat,ldRpHDate)
     SCATTER MEMVAR MEMO
     INSERT INTO (lcTempHist) FROM MEMVAR
     REPLACE &lcTempHist..Cowner WITH 'ARHIST'
   ENDIF
  ENDIF
ENDSCAN



*-- Return to old order.

SET ORDER TO &lcOldOrder
SET RELATION TO  && Rest relation.

SELECT (lcTempHist)
SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER  && Relation with customer.
LOCATE
IF EOF()
  SELECT (lcTmpTrans)
  SET ORDER TO &lcOldOrdr
  SELECT (lnPrvsAlis)
  RETURN
ENDIF

SCAN
  lnAmount = 0
  m.cAddress4 = Customer.cAddress4 
  m.cAddress5 = Customer.cAddress5
  m.Region    = Customer.Region
  
  SCATTER MEMVAR
  *-- Don't collect any data for Void or return record.
  IF M.Trantype $ 'IR'
    LOOP
  ELSE
    IF lfVoidRcrd ()
      LOOP
    ENDIF
  ENDIF
  
  *-- calculate the recored one time for evry invoice.
  IF M.Trantype $ '169'
    IF SEEK(M.tran+IIF(M.TranType='6','9',M.TranType),lcTmpInve)
      LOOP
    ELSE
      INSERT INTO (lcTmpInve) FROM MEMVAR
    ENDIF
  ENDIF
  
  *-- Function to get the amount.
  IF ALLTRIM(Cowner) = 'ARHIST'
    IF m.TranType = '3' .AND. lfVoidDbt()
      LOOP
    ELSE
      DO lpHistAmnt
    ENDIF
  ELSE
    IF m.TranType # '2' 
      DO lpGetAmnth
    ENDIF
  ENDIF
    
  lcInstalNo = ''
  IF ALLTRIM(Cowner) # 'ARHIST' .AND. M.TranType = '1'
    DO lpGetHAmnt WITH lcInstalNo    && Calculate the amount from History file
  ELSE
    *-- Get the Credit Adjustment.
    IF Trantype = '7'
      m.nHstAmount = Amount
        ELSE
      m.nHstAmount = m.Amount
    ENDIF
    *-- 
    IF ALLTRIM(Cowner) # 'ARHIST'
      *-- This Function to check if Type '9'  credit payment is paid or not
     IF !(M.Trantype $ '98') .OR. lfVoidPay (M.Trantype)
       m.Amount = 0
     ENDIF 
    ENDIF
  ENDIF

  IF llMulCurr AND M.Amount <> 0 AND (cCurrCode <> gcBaseCurr)
    
    lnAmount = ROUND(lfAmntDisp(M.Amount,"O",ldRpExDate,lcRpTmpCur),2)

    *-- Checking for the occurence of this currency with "O" type
    IF !SEEK(cCurrCode,lcTmpCurrc)
      INSERT INTO (lcTmpCurrc) (cCurrCode) VALUES (&lcTempHist..cCurrCode)
    ENDIF

    m.nORate = lfAmntDisp(1,"O",ldRpExDate,lcRpTmpCur)
  ELSE
    lnAmount = M.Amount
    m.nORate = 1
  ENDIF  

  DO lpHstDeal   && Calculate Ages and fill memory variables (OpenCr and NetBal)
  DO lpInshstRc  && Insert New Record in Transaction Temporary File and Totals Temporary file.

ENDSCAN  && end Scan Credit records achieve both customer and Credit filters.
*-- EndScan around Credit file for records matcjing criteria

SET RELATION TO  && Rest relation.
*-- Close the temp Files.
IF USED(lcTmpInve)
  USE IN (lcTmpInve)
  ERASE (gcWorkDir+lcTmpInve+".DBF")
  ERASE (gcWorkDir+lcTmpInve+".CDX")
  ERASE (gcWorkDir+lcTmpInve+".FPT")
ENDIF
*-- Close the temp Files.
IF USED(lcTempHist)
  USE IN (lcTempHist)
  ERASE (gcWorkDir+lcTempHist+".DBF")
  ERASE (gcWorkDir+lcTempHist+".CDX")
  ERASE (gcWorkDir+lcTempHist+".FPT")
ENDIF

SELECT (lcTmpTrans)
SET ORDER TO &lcOldOrdr

SELECT (lnPrvsAlis)
*-- End OF lpCollhist
*:**************************************************************************
*: Name      : lpGetHAmnt
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : To Calculate the amount from History file
*:**************************************************************************
*: Called from : lpCollhist.
*:**************************************************************************
*: Example   : DO lpGetHAmnt
*:**************************************************************************
*:
PROCEDURE lpGetHAmnt
PARAMETER lcInstalNo
PRIVATE lnOldAlias , lnAmount , lnInvRec , ldStartDat
STORE 0 TO lnPaidBef , lnInvRec
lnOldAlias = SELECT(0)
*-- If there is Payment in AR History file
IF SEEK(m.Account + m.Tran + lcInstalNo,'ArHist') 
  ldStartDat = ldRpHDate - Day(ldRpHDate) + 1
  STORE 0 TO m.nHstAmount , M.Amount
  
  llOpenInh =gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
  M.Amount = IIF(SEEK(M.Tran,'InvHdr'),Invhdr.totalchg,0)
  IF llOpenInh
    USE IN INVHDR
  ENDIF
  *-- get all record for this account & transaction.
  SELECT ARHIST
  SCAN REST WHILE account+tran+cinstalno = m.Account + m.Tran
    IF histdate <= ldRpHDate
      m.nHstAmount = m.nHstAmount + ArHist.Amount
    ENDIF
    
    IF histdate <= ldStartDat
      lnPaidBef = lnPaidBef + ArHist.Amount
    ENDIF
    lnInvRec = lnInvRec + 1
  ENDSCAN
  M.Amount = M.Amount - m.nHstAmount
  
  *-- Get the Paid amount at before this month.
  IF M.Amount = 0 .AND. lnPaidBef # 0
    m.nHstAmount = m.nHstAmount - lnPaidBef
  ENDIF

  IF M.Trantype = '1' .AND. lnInvRec > 1 .AND. M.Amount # 0
    M.Reference  = 'PART PAID'
  ENDIF
ENDIF

SELECT (lnOldAlias)
*-- EndIf of Payment in AR History file

*-- End of lpGetHAmnt.
*:**************************************************************************
*: Name      : lpHstDeal
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 03/25/2003
*: Purpose   : To Calculate the Credit Aging
*:**************************************************************************
*: Called from : lpInsCredt
*:**************************************************************************
*: Example   : DO lpHstDeal
*:**************************************************************************
*:
PROCEDURE lpHstDeal

=lfUpdtHAge('C',gdSysDate - TranDate) && Update Date ages

*-- Save open credit and net balance per transaction
STORE m.Amount TO m.NetBal
IF !(Trantype $ '18')
  STORE m.Amount TO m.OpenCr
ENDIF
STORE lnAmount TO lnOpenCr , lnNetBal

*-- End of lpHstDeal.
*:**************************************************************************
*: Name      : lfUpdtHAge
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Calculate the Aging 
*:**************************************************************************
*: Called from : lpHstDeal
*:**************************************************************************
*: Passed Parameters : lcUpdtTyp --> 'D' for Debit , 'C' for Credit
*:                     lnAgeDays --> No of Days left  
*:**************************************************************************
*: Example   : =lfUpdtHAge('C',gdSysDate - TranDate)
*:**************************************************************************
*:
FUNCTION lfUpdtHAge
PARAMETERS lcUpdtTyp , lnAgeDays

*-- Intialize variables that calculate data from transaction files. [begin]
STORE 0.00 TO m.Current , m.Age30 , m.Age60 , m.Age90 , m.Age120 , ;
              m.OpenCr , m.Totage , m.NetBal , m.Age00 ,;
              m.nGroup , m.nPostDChq

STORE 0 TO lnCurrent , lnAge30 , lnAge60 , lnAge90 , lnAge120 , ;
           lnOpenCr , lnTotAge , lnNetBal , lnAge00
*-- Intialize variables that calculates data from transaction files. [End]

*-- if no parameter passed (i.e. want to intializing only) then return...
*-- does occur in this program but it may be.
IF TYPE('lcUpdtTyp') $ 'UL'
  RETURN
ENDIF

*-- lcAges : Varaible Hold Field description (Age00, 30 , 60 , 90 , and 120)
*-- if Debit
IF lcUpdtTyp = 'D'

  *-- if Age By Date.
  IF lcAgeType = 'D'
    lcAges = 'm.Age'                                                    +;
      IIF(lnAgeDays >= 120,'120',IIF(lnAgeDays >= 90 ,'90'   ,;
      IIF(lnAgeDays >= 60 ,'60' ,IIF(lnAgeDays >= 30 ,'30','00')))) 
  ELSE  && else if Age By Terms.
    lcAges = 'm.Age'                                                      +;
      IIF(lnAgeDays >= 91,'120',IIF(lnAgeDays >= 61 ,'90' ,;
      IIF(lnAgeDays >= 31 ,'60' ,IIF(lnAgeDays >= 1 ,'30','00'))))   
  ENDIF  && end if Age By Date.

ELSE  && else if Credit
  lcAges = 'm.Age'                                                      +;
    IIF(lnAgeDays >= 120,'120',IIF(lnAgeDays >= 90 ,'90' ,;
    IIF(lnAgeDays >= 60 ,'60' ,IIF(lnAgeDays >= 30 ,'30','00')))) 
ENDIF  && end if Debit

&lcAges   = m.Amount       && Fill Age variable
m.Current = m.Age00

PRIVATE lcTotAccAg         && Fill the Accumulated Age Variable in lcTmpAcct
lcTotAccAg  = STRTRAN(lcAges,"m.","ln")
&lcTotAccAg = lnAmount
lnCurrent   = lnAge00

*-- End of lfUpdtHAge.
*:**************************************************************************
*: Name      : lpInshstRc
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : To Calculate the Credit Aging 
*:**************************************************************************
*: Called from : lpInsDebit OR lpInsCredt
*:**************************************************************************
*: Example   : DO lpInshstRc
*:**************************************************************************
*:
PROCEDURE lpInshstRc

m.cGroupKey = EVALUATE(lcReplExpr)

*-- negative number is assigned to nGroup field in Transaction file 
*-- because index is sorted by nGroup so when we add the incremented 
*-- positive numbers for Group Position in Group change proc. with 
*-- SCAN/ENDSCAN the line remains in its position 
*-- AND
*-- m.nGroup must take value -1 to get the nGroup field begin with -1 not 0 
*-- to treat the case of the instalment of the invoices.
m.nGroup = -1
m.nGroup = -99999


*-- Check if this invoice is still open or not if still open update the current record
*-- that came from the Debit file.

llNewRecd = .T.

lnPrvAls = SELECT(0)
IF SEEK(M.Account+M.Tran,lcTmpTrans)
  SELECT (lcTmpTrans)
  LOCATE REST WHILE Account + Tran = M.Account+M.Tran For TranType = M.TranType
  IF FOUND()
    llNewRecd = .F.
  ENDIF
ENDIF

IF llNewRecd
  IF EMPTY(M.Desc)
    M.Desc = IIF(EMPTY(TRANCODE),M.Desc,gfCodDes(M.TRANCODE,'TRANCODE'))
  ENDIF
  INSERT INTO (lcTmpTrans) FROM MEMVAR         && Insert Transaction Record
ELSE
  IF EMPTY(&lcTmpTrans..Desc)
    M.Desc = IIF(EMPTY(TRANCODE),M.Desc,gfCodDes(M.TRANCODE,'TRANCODE'))
  ENDIF
  SELECT (lcTmpTrans)
  REPLACE nHstAmount WITH m.nHstAmount,;
          Reference  WITH IIF(M.Trantype = '1','PART PAID',Reference) ,;
          Desc       WITH M.Desc
  SELECT(lnPrvAls)
  RETURN
ENDIF

SELECT(lnPrvAls)

*-- Don't update header file in case not Invoice.
IF M.Trantype # '1'
  IF !llEofTran .AND. !llFirstTim
    RETURN
  ENDIF
  llFirstTim = .F.
ENDIF

*-- If cGroupKey with this currency is found in Temp Acc Currency file
IF llMulCurr
  IF SEEK(&lcTmpTrans..cGroupKey+&lcTmpTrans..cCurrCode,lcTmpAcCur)
    SELECT (lcTmpAcCur)
    REPLACE Current WITH Current + &lcTmpTrans..Current ,;
            Age30   WITH Age30   + &lcTmpTrans..Age30   ,;
            Age60   WITH Age60   + &lcTmpTrans..Age60   ,;
            Age90   WITH Age90   + &lcTmpTrans..Age90   ,;
            Age120  WITH Age120  + &lcTmpTrans..Age120
  ELSE

    INSERT INTO (lcTmpAcCur) (cGroupKey , Account , cAddress4 , cAddress5 , Region ,;
                              cCurrCode , Current , Age30 , Age60 , Age90 , Age120) ;
           VALUES (&lcTmpTrans..cGroupKey , &lcTmpTrans..Account , &lcTmpTrans..cAddress4 , ;
                   &lcTmpTrans..cAddress5 , &lcTmpTrans..Region , &lcTmpTrans..cCurrCode,;
                   &lcTmpTrans..Current , &lcTmpTrans..Age30 ,&lcTmpTrans..Age60 , ;
                   &lcTmpTrans..Age90 , &lcTmpTrans..Age120)
  ENDIF
ENDIF

*-- if Find this account in Temporary totals file.
IF SEEK(m.cGroupKey,lcTmpAcct)
  SELECT (lcTmpAcct)
  REPLACE Current WITH Current + lnCurrent ,;
            Age30 WITH Age30   + lnAge30   ,;
            Age60 WITH Age60   + lnAge60   ,;
            Age90 WITH Age90   + lnAge90   ,;
           Age120 WITH Age120  + lnAge120  ,;
           TotAge WITH TotAge  + lnTotAge  ,;
           OpenCr WITH OpenCr  + lnOpenCr  ,;
           NetBal WITH NetBal  + lnNetBal

ELSE  && else if this account not found before.

  *-- Add new record in temporary total files.
  WAIT WINDOW 'Collect data for Customer ' + Account NOWAIT

  *-- Adding postDated Cheques to lcTmpAcct file in single currency comp.
  IF !llMulCurr AND SEEK(m.Account,'PostDChq')
    SELECT PostDChq
    SUM Amount REST WHILE Account = m.Account TO m.nPostDChq
  ENDIF

  INSERT INTO (lcTmpAcct) (Current, Age30, Age60, Age90, Age120, OpenCr,;
               TotAge, NetBal, cGroupKey, Account, BtName, cAddress4,;
               cAddress5, Region, nPostDchq, cFacCode) VALUES ;
              (lnCurrent, lnAge30, lnAge60, lnAge90, lnAge120, lnOpenCr,;
               lnTotAge, lnNetBal, m.cGroupKey, m.Account, Customer.BtName,;
               Customer.cAddress4, Customer.cAddress5, Customer.Region,;
               m.nPostDchq, Customer.cFacCode)
ENDIF  && End if Find this account in Temporary totals file.

*-- End of lpInshstRc.
*:**************************************************************************
*: Name      : lpGetAmnth
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : To Calculate the Credit Aging 
*:**************************************************************************
*: Called from : lpInsDebit OR lpInsCredt
*:**************************************************************************
*: Example   : DO lpInshstRc
*:**************************************************************************
*:
PROCEDURE lpGetAmnth
PRIVATE lnAlias

lnAlias = SELECT (0)
*-- seek to know if this invoice is complete paid or no.

IF !SEEK(M.Account+M.Tran,'DEBIT') .AND. !(M.TranType $ '4508')
  IF M.TranType = '3'
    IF SEEK(m.Account + m.Tran,'ArHist')
      SELECT ARHIST
      m.Amount = 0
      SCAN REST WHILE  account+tran+cinstalno = m.Account + m.Tran ;
        FOR TranType = '3'
        m.Amount = m.Amount + Arhist.Amount 
      ENDSCAN
    ENDIF  
  ELSE
    m.Amount = 0
  ENDIF
ENDIF

SELECT (lnAlias)
*- End OF lpGetAmnth
*:*************************************************************
*: Name      : lfVoidRcrd
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Function to check if this invoice is void or not.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfVoidRcrd ()
*:*************************************************************
*:
FUNCTION lfVoidRcrd
PRIVATE llVoidRcrd , lnAOldAls

lnAOldAls = SELECT (0)
llVoidRcrd = .F.

IF SEEK(m.Account + m.Tran,'ArHist') 
  *-- get all record for this account & transaction.
  SELECT ARHIST
  SCAN REST WHILE account+tran+cinstalno = m.Account + m.Tran
    IF Trantype $ 'IR'
      llVoidRcrd = .T.
    ENDIF
  ENDSCAN
ENDIF

SELECT(lnAOldAls)

RETURN llVoidRcrd

*-- End OF lfVoidRcrd.
*:*************************************************************
*: Name      : lfCurrAmt
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Function to get the current amount until our date.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfCurrAmt ()
*:*************************************************************
*:
FUNCTION lfCurrAmt
PRIVATE lnPrvAlis , llFound , ldStartDat, lnPaidAmt

lnPrvAlis = SELECT (0)
*-- If there is Payment in AR History file
llFound =.F.
ldStartDat = ldRpHDate - Day(ldRpHDate) + 1
IF Debit.TranType = '3'
  IF SEEK(m.Account + m.Tran + lcInstalNo,'ArHist') 
    SELECT ArHist
    LOCATE REST WHILE account + tran + cinstalno = m.Account + m.Tran ;
    FOR TranType = '3' .AND. BETWEEN(HistDate,ldStartDat,ldRpHDate)
    IF Found()
      STORE 0 TO m.nHstAmount , m.Amount
      m.Amount = ArHist.OpenAmt + ArHist.Amount
      m.nHstAmount = m.Amount
      SCAN REST WHILE account+tran+cinstalno = m.Account + m.Tran + lcInstalNo ;
        FOR BETWEEN(HistDate,ldStartDat,ldRpHDate) .AND. TranType = '3'
        llFound = .T.
        m.Amount = m.Amount - Amount 
      ENDSCAN
      IF llFound
        m.nHstAmount = m.nHstAmount - M.Amount
      ENDIF
    ELSE
      m.nHstAmount = M.Amount
    ENDIF  
  ELSE
    m.nHstAmount = m.Amount
  ENDIF
ELSE
  IF SEEK(m.Account + m.Tran + lcInstalNo,'ArHist') 
    STORE 0 TO m.nHstAmount , m.Amount , lnInvRec,lnPaidAmt
    SELECT ArHist
    m.Amount = ArHist.OpenAmt + ArHist.Amount
    m.nHstAmount = m.Amount
    SCAN REST WHILE account+tran+cinstalno = m.Account + m.Tran + lcInstalNo ;
      FOR HistDate <= ldRpHDate
      llFound = .T.
      m.Amount = m.Amount - Amount
      lnInvRec = lnInvRec + 1
      
      IF BETWEEN(HistDate,ldStartDat,ldRpHDate)
        lnPaidAmt = lnPaidAmt + Amount
      ENDIF
    ENDSCAN
    IF llFound
      IF lnPaidAmt = 0
        m.nHstAmount = m.Amount
      ELSE
        m.nHstAmount = m.nHstAmount - M.Amount
      ENDIF
    ENDIF
    
    
    IF M.Trantype = '1' .AND. lnInvRec >= 1 .AND. M.Amount # 0
      M.Reference  = 'PART PAID'
    ENDIF
    
  ELSE
    m.nHstAmount = m.Amount
  ENDIF
ENDIF
*-- EndIf of Payment in AR History file


SELECT (lnPrvAlis)
*-- End OF lfCurrAmt
*:**************************************************************************
*: Name      : lpHistAmnt
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : To Calculate the Hist amount
*:**************************************************************************
*: Called from : lpInsDebit OR lpInsCredt
*:**************************************************************************
*: Example   : DO lpHistAmnt
*:**************************************************************************
*:
PROCEDURE lpHistAmnt
PRIVATE lnAlias , llFound

llFound =.F.
lnAlias = SELECT (0)
*-- seek to know if this invoice is complete paid or no.

IF !(M.TranType $ '45') .AND. SEEK(m.Account + m.Tran,'ArHist')
  
  STORE 0 TO m.nHstAmount , m.Amount
  SELECT ArHist
  m.Amount = ArHist.OpenAmt + ArHist.Amount
  m.nHstAmount = m.Amount
  SCAN REST WHILE account+tran+cinstalno = m.Account + m.Tran;
    FOR HistDate <= ldRpHDate
    llFound = .T.
    m.Amount = m.Amount - Amount 
  ENDSCAN
  IF llFound
    m.nHstAmount = m.nHstAmount - M.Amount
  ENDIF
ELSE
  m.nHstAmount = m.Amount
ENDIF

SELECT (lnAlias)
*- End OF lpHistAmnt
*:**************************************************************************
*: Name      : lfVoidDbt
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Check for the debit on account if paid.
*:**************************************************************************
*: Called from : lpInsDebit OR lpInsCredt
*:**************************************************************************
*: Example   : = lfVoidDbt ()
*:**************************************************************************
*:
FUNCTION lfVoidDbt
PRIVATE llLoopDbt
llLoopDbt = .F.
lnAlias = SELECT (0)

SELECT ArHist
IF SEEK(m.Account + m.Tran,'ArHist')
  LOCATE REST WHILE  account+tran+cinstalno = m.Account + m.Tran ;
   FOR TranType = '8'
  IF FOUND()
    llLoopDbt = .T.
  ENDIF
ENDIF

SELECT(lnAlias)
RETURN llLoopDbt
*-- End OF lfVoidDbt
*:**************************************************************************
*: Name      : lfVoidPay
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Check for the debit on account if paid.
*:**************************************************************************
*: Called from : lpInsDebit OR lpInsCredt
*:**************************************************************************
*: Example   : = lfVoidDbt ()
*:**************************************************************************
*:
FUNCTION lfVoidPay
Parameter lcParamter

IF Type('lcParamter') = 'U'
  RETURN
ENDIF

PRIVATE llLoopDbt
llLoopDbt = .F.
lnAlias = SELECT (0)
SELECT ArHist

DO CASE 
  CASE  lcParamter = '9'
    IF SEEK(m.Account + m.Tran,'ArHist')
      LOCATE REST WHILE  account+tran+cinstalno = m.Account + m.Tran ;
       FOR TranType = '6' .AND. BETWEEN(HistDate,ldStartDat,ldRpHDate)
      IF FOUND()
        llLoopDbt = .T.
      ENDIF
    ENDIF

  CASE  lcParamter = '8'
    IF SEEK(m.Account + m.Tran,'ArHist')
      LOCATE REST WHILE  account+tran+cinstalno = m.Account + m.Tran ;
       FOR TranType = '3' .AND. BETWEEN(HistDate,ldStartDat,ldRpHDate)
      IF FOUND()
        llLoopDbt = .T.
      ENDIF
    ENDIF
ENDCASE

SELECT(lnAlias)

RETURN llLoopDbt
*-- End OF lfVoidPay
*:*************************************************************
*: Name      : 
*: Developer : Abdou Elgendy (ABD)
*: Date      : 05/01/2003
*: Purpose   : Function to get the current amount until our date.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = ()
*:*************************************************************
*:
FUNCTION lfCurCdAmt
PRIVATE lnPrvAlis , llFound , ldStartDat , lnPaidAmt

lnPrvAlis = SELECT (0)
*-- If there is Payment in AR History file
llFound =.F.
ldStartDat = ldRpHDate - Day(ldRpHDate) + 1
IF SEEK(m.Account + m.Tran + lcInstalNo,'ArHist')

  *--bas
  *STORE 0 TO m.nHstAmount , m.Amount , lnPaidAmt
  STORE 0 TO m.nHstAmount , lnPaidAmt
  *--bas

  SELECT ArHist

  *--bas
  *m.Amount = Credit.Amount + ArHist.Amount
  *m.nHstAmount = m.Amount
  *--bas

  *--bas
  *SCAN REST WHILE account+tran+cinstalno = m.Account + m.Tran + lcInstalNo ;
    FOR HistDate <= ldRpHDate

  SCAN REST WHILE account+tran+cinstalno = m.Account + m.Tran + lcInstalNo ;
    FOR BETWEEN(HistDate,ldStartDat,ldRpHDate)

    *B124598,1 BWA 04/10/2004 Initial variable hold the total amount.[START]
    STORE 0 TO lnBalance
    *B124598,1 BWA 04/10/2004.[END]

    *B123679,1 BWA 08/02/2004 Fix the bug of calculating transactions from ARHIST.[START]
    IF TranType = '9'
      llFound = .T.
      IF Amount > 0
        m.Amount = m.Amount - Amount

        IF BETWEEN(HistDate,ldStartDat,ldRpHDate)
          lnPaidAmt = lnPaidAmt + Amount
        ENDIF
      ENDIF
    ELSE
      llFound = .T.

      *B124598,1 BWA 04/10/2004 Add the amounts to the new variable.[START]
      *m.Amount = m.Amount - Amount
      lnBalance = lnBalance + Amount
      *B124598,1 BWA 04/10/2004.[END]

      IF BETWEEN(HistDate,ldStartDat,ldRpHDate)
        lnPaidAmt = lnPaidAmt + Amount
      ENDIF
    ENDIF
  ENDSCAN
  *B123679,1 BWA 08/02/2004.[END]

  *B124598,1 BWA 04/10/2004 Add the amounts to the new variable.[START]
  *IF llFound
  *  IF lnPaidAmt = 0
  *    m.nHstAmount = m.Amount
  *  ELSE
  *    m.nHstAmount = m.nHstAmount - M.Amount
  *  ENDIF
  *ENDIF

  IF llFound
    IF lnPaidAmt = 0
      m.nHstAmount = lnBalance
    ELSE
      m.nHstAmount = m.nHstAmount - lnBalance
    ENDIF
  ENDIF
  *B124598,1 BWA 04/10/2004.[END]

ELSE
  m.nHstAmount = m.Amount
ENDIF

*-- EndIf of Payment in AR History file

SELECT (lnPrvAlis)
*-- End OF lfCurCdAmt
*:**************************************************************************
*: Name      : lfCalRnBal
*: Developer : BASSEM RAAFAT ERNEST(BWA) *C037891,1 
*: Date      : 03/24/2004
*: Purpose   : Calculate the running balance field.
*:**************************************************************************
*: Called from : Report code section.
*:**************************************************************************
*: Returns   : None.
*:**************************************************************************
*: Example   : =lfCalRnBal()
*:**************************************************************************
FUNCTION lfCalRnBal

PRIVATE lcAliasTT  , lcOrdValue , lcAccChg , lnRunBalanc , llFrsTime
STORE SPACE(5) TO lcAccChg
lcAliasTT = ALIAS()      && Save Current Alias TT >>TempTransaction

SELECT (lcTmpTrans)
lcOrdValue = EVAL(KEY())

llFrsTime = .T.
lnRunBalanc = 0

SCAN
  IF llFrsTime
    lcAccChg = &lcTmpTrans..Account
    llFrsTime = .F.
  ENDIF

  IF lcAccChg <> &lcTmpTrans..Account
  	lnRunBalanc = 0
  	lnRunBalanc = &lcTmpTrans..Amount
  ELSE
	lnRunBalanc = lnRunBalanc + &lcTmpTrans..Amount
  ENDIF
  
  REPLACE &lcTmpTrans..RunBalanc WITH lnRunBalanc
  lcAccChg = &lcTmpTrans..Account
ENDSCAN

=SEEK(lcOrdValue,lcTmpTrans)
SELECT (lcAliasTT)
*--End of lfCalRnBal.
*C037891,1 BWA 03/24/2004.