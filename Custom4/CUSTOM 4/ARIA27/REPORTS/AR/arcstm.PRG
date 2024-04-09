*:**************************************************************************
*: Program file  : ARCSTM.PRG
*: Program desc. : Customer Statement form
*: For Report    : ArCStmA.FRX,ArCStmB.FRX
*: System        : Aria Advantage Series.
*: Module        : Account Receivable (AR)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Reference     : N000130               
*:**************************************************************************
*: Procedures    : lpFillStru , lpCreatFil , lpCollect , lpInsDebit , 
*:                 lpDebDeal , lpInsCredt , lpCreDeal , lpInsRecrd , 
*:                 lpCurrChng , lpChSortBy , lpApplyAge , lpGroupChg , 
*:                 lpApplyRel
*:
*: Functions     : lfwRepWhen , lfItmPos , lfVarPos , lfvAcct , lfvDate , 
*:                 lfAdrShift , lfvCurDisp , lfvPeriod , lfvTran , 
*:                 lfGetSign , lfIntializ , lfClearRep , lfUpdtAge , 
*:                 lfGetReplc , lfGetAddr , lfEndGroup , lfOverMsg , 
*:                 lfCollTime , lfAmntDisp , lfBreakRel , lfvInvoice
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
*:B603779,1 SSE 07/27/2000 Convert Custom Report Frx to work with the new Customer Statement for Chinese Native
*:B603780,1 SSE 07/27/2000 Convert Custom Report Prg to work with the new Customer Statement for JovaniSET defa TO w:\aria27
*:B603781,1 SSE 07/27/2000 Convert Custom Report Prg/Frx to work with the new Customer Statement for KOBE
*:B603782,1 SSE 07/27/2000 Convert Custom Report Prg to work with the new Customer Statement for OLSEN
*:B603783,1 SSE 07/27/2000 Convert Custom Report Prg to work with the new Customer Statement for WearWolf
*:B604009,1 SSE 11/02/2000 Adjust Form A Paper size to be letter 8 1/2 * 11 and also adjust position of
*:B604009,1                Some fields in FRX (All modifications in FRX). 
*:B604065,1 SSE 12/12/2000 Adding an option to detect whether to print company name (Yes/No) to
*:B604065,1                Used in Custom report programs.
*:B604171,1 SSE 02/04/2000 Fix bug disabling Charge Back when user selects Only open invoices YES.
*:B604171,1                Also Change the name of Form from lcRpName to lcFormName .
*:B804080,1 HBG 25/03/2001 The customer address will be printed under the customer name
*:B804080,1                even if remit to factor is yes ,and the factor addres must be 
*:B804080,1                printed as in 26
*:C102280,1 BWA 04/05/2000 Convert the customer Statment form 'B' to text format.
*:B604447,1 BWA 05/03/2001 Variable ( lcTmpTrn_A ) not found.
*:E301635,1 AME 06/20/2001 Enable Printing logo in customer statments
*:C200241,1 SSE 10/28/2001 Custom add Region mover to Customer statement for MBI England.
*:B605492,1 SSE 02/24/2002 Fix bug of not printing more than 9 pages Form A.
*:B605923,1 SSE 05/08/2002 Fix bug of adding option either to print account with Zero balance (Yes/No).
*:B606365,1 SSE 09/23/2002 Fix bug of Subscript out of bound. (All modifications is in SyRepuvr)
*:B606179,1 SSE 10/15/2002 Fix bug of adding a new option (House or Factor) instead of Remit to Factor
*:B606337,1 SSE 10/21/2002 Fix bug of displaying Cut off date on Form A.
*:B606951,1 WAB 02/18/2003 Add the ability to Print BOTH (Factor and non factored Transaction )  
*:C200496,1 ABD 04/09/2003 Add a trigger for custom David luke to collect data from the arhist file.
*:B607254,1 AKA 05/04/2003 Remove the filer of Factor formthe Credit file. This is temporary fix until we fix the bug # 607231
*:B607100,1 ABD 10/07/2003 Fix bug that If the system is setup with multiple divisions the customer statment 
*:B607100,1 ABD            Still prints the company name insted of the division.  
*:B119521,1 ABD 12/25/2003 If we select "NO" the system should print only statements 
*:B119521,1 ABD            For accounts that have a positive or negative balance.
*:B122284,1 NNA 04/05/2004 Fix bug of that the acct. and balance column not matching specially if the Debit 
*:B122284,1 NNA            transaction No. = the credit memo Trans. No.
*:B123396,1 BWA 06/24/2004 Fix the bug of Cut off date always print with century off.
*:B124289,1 BWA 09/15/2004 Fix the bug of the Amount column doesn't match the balance column.
*:B123952,1 BWA 11/18/2004 Fix the bug of the filter "Acc has total credit bal" doesn't work properly
*:B129391,1 EIH 10/04/2005 Fix the bug of invalid Amount in case of multi currency.
*:B608529,1 Mos 04/22/2008 fix bug of wrong factor address  ticket# "T20080328.0010"  
*:********************************************************************************************************
lcStTime = TIME()


*:B607254,1 AKA 05/04/2003 Remove the filer of Factor formthe Credit file. This is temporary fix until we fix the bug # 607231
PRIVATE  lcDBExp, lcCRExp
STORE "" TO  lcDBExp, lcCRExp
*:B607254,1 AKA 05/04/2003 Remove the filer of Factor formthe Credit file. This is temporary fix until we fix the bug # 607231

*C102280,1 BWA 04/05/2000 Variable hold the alias.[START]
PRIVATE lcAliasB
*C102280,1 [END]

*B804080,1 HBG 25/03/2001 Array to print Factor Address [Begin]

*B606179,1 Increase the length of the array. [Begin]
*DIMENSION laFactTo[5,1]
DIMENSION laFactTo[6,1]
*B606179,1 Increase the length of the array. [End]

laFactTo = ''
lcFacName = ''
*B804080,1 [End]

*B607100,1 ABD - Define the division array. [Begin]
DECLARE laDivLName[1,2]
laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'
lcDivLName =''
*B607100,1 ABD - [End]

*B604065,1 print Company name or not (not used in From A,B only in Custom Program) [Begin]
lcPrnComp = IIF(TYPE('llPrntComp') = 'L' , IIF(llPrntComp , 'Y' , 'N') , 'Y')
llPrntComp = lcPrnComp  = "Y"
*B604065,1 print Company name or not (not used in From A,B only in Custom Program) [End]


*E301635,1 AME [Start] Define variable lllogo to indecate whether there is logo or not.
llLogo = IIF(SEEK('*' + 'LOGO' , 'OBJLINK') .AND. SEEK(OBJLINK.cObject_ID ,;
             'OBJECTS') , .T. , .F.)        && Flag to know if we are to print the Company Logo
lcTaxRef = gfGetMemVar('M_TAX_REFE')        
=SEEK(gcAct_comp,'SycComp')
lcCompFax = TRANSFORM(SycComp.CCOM_FAX , gfPhoneTem())  && Fax No. Pic
*E301635,1 AME [End]

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

    *SSE Sameh 
    llOGFltCh = .T.

  ENDIF
ELSE
  RESTORE ADDITIVE FROM (gcDataDir+'ARSTMSG')
ENDIF  
*-- Restore statement messages from memo file. [End]

*B606179,1 Check if factor code is empty. [Begin]
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
*B606179,1 Check if factor code is empty. [End] 

*C200241,1 Check Region if changed. [Begin]
=gfDoTriger('ARCSTM',PADR('CHECKREG',10))
*C200241,1 Check Region if changed. [End]

*-- if Filter Expression changed (ReCollect data again)
IF llOGFltCh 

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
   
  *C200241,1 Add the custom region to lcRpExp. [Begin]
  =gfDoTriger('ARCSTM',PADR('COMPLEXP',10))
  *C200241,1 Add the custom region to lcRpExp. [End]

  *B606179,1 Check if Remit to is Factor 'F'. [Begin]

  *B606951,1 WAB (Start) if the User select Both , keep the lcRpExp as is
  *IF lcRpRemit = 'F'
  *  lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),'',[ AND ]) + [cFacCode = lcRpFactor]
  *ELSE
  *  *-- Allow non factored transaction.    
  *  lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),'',[ AND ]) + [EMPTY(cFacCode)]
  *ENDIF
  DO CASE 
    *STORE "" TO  lcDBExp, lcCRExp
    CASE lcRpRemit = 'F'
      *lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),'',[ AND ]) + [cFacCode = lcRpFactor]
      lcRpExp = lcRpExp    
    CASE lcRpRemit = 'H'
      *lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),'',[ AND ]) + [EMPTY(cFacCode)]
      lcRpExp = lcRpExp 
  ENDCASE

  *B606951,1 WAB (End)
  
  *B606179,1 Check if Remit to is Factor 'F'. [End]
  
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

*C102280,1 BWA 04/05/2000 Reuse the transaction file the get the recno.[START]
IF lcFormType = "B" AND lcRepMode = "Text"
  lcAliasB = SELECT(0)
  IF USED(lcTmpTrn_A)
    USE IN (lcTmpTrn_A)
  ENDIF
  USE (lcTmpTrans) IN 0 AGAIN ALIAS (lcTmpTrn_A)
  SELECT(lcAliasB)
ENDIF
*C102280,1 [END]

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
llIsAparel = lfIsApparl(@lcPrgName)
IF llIsAparel
  DO EVALUATE('lcPrgName')
  DO ENDREPORT
ELSE    && Else Report is FRX
  STORE '' TO lcOverMsg1, lcOverMsg2, lcOverMsg3
  STORE .T. TO llGrpChang
  
  *B606337,1 Get the Cut off date if user changed it. [Begin]
  IF lcFormType = "A"
    =lfCutDate()
  ENDIF  
  *B606337,1 Get the Cut off date if user changed it. [End]

  *B604171,1 Change Form name from lcRpName to lcFormName [Begin]
  *DO gfDispRe WITH EVAL('lcRpName')
  DO gfDispRe WITH EVAL('lcFormName')  
  *B604171,1 Change Form name from lcRpName to lcFormName [End]    
ENDIF
*-- EndIf of Report type is @SAY 

*!**************************************************************************
*! Name      : lfGroupTot
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/2000
*! Purpose   : Indicate end of group 
*!**************************************************************************
*! Called from : .FRX group footer (Report Form A)
*!**************************************************************************
*! Example     : = lfGroupTot()
*!**************************************************************************
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

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/2000
*! Purpose   : Report When Function
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
PRIVATE lnTranDate , lcSycint
lnTranDate = lfItmPos('DEBIT.TRANDATE')   && get Tran. Date Fixed filter Position

*B123396,1 BWA 06/24/2004 Fix the bug of Cut off date always print with century off.[START]
lcSycint = ALIAS()      && Save Current Alias

IF !USED('SYCINT')
  =gfOpenFile(gcSysHome+'SYCINT',gcSysHome+'ccontcode','SH')
ENDIF

SELECT SYCINT
SEEK(ALLTRIM(gcContCode))
lcCentVal = ALLTRIM(LOOKUP(SYCINT.cCentury,gcContCode,SYCINT.cCont_Code,'Ccontcode'))
SET CENTURY &lcCentVal

SELECT (lcSycint)     && Restore Old Alias
*B123396,1 BWA 06/24/2004.[END]

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

*B606179,1 Assign the Remit to factor variable. [Begin]
llRpRemit = (lcRpRemit = 'F')
*B606179,1 Assign the Remit to factor variable. [End]

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

*B604171,1 Checking if Only open invoice is YES from Saved filter [Begin]
=lfvInvoice()
*B604171,1 [End]
*-- End of lfwRepWhen.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/1999
*! Purpose   : To get the position of the fixed filter in OG
*!**************************************************************************
*! Called from : OG When Function 
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
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

*!**************************************************************************
*! Name      : lfVarPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/1999
*! Purpose   : To get the position of the Variable in OG
*!**************************************************************************
*! Called from : OG when function
*!**************************************************************************
*! Example   : = lfVarPos()
*!**************************************************************************
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

*!**************************************************************************
*! Name      : lfvAcct
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/1999
*! Purpose   : Validate the enterd account
*!**************************************************************************
*! Example   : = lfvAcct()
*!**************************************************************************
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

*!**************************************************************************
*! Name      : lfvDate
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/1999
*! Purpose   : Validate the enterd date
*!**************************************************************************
*! Example   : = lfvDate()
*!**************************************************************************
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

*!**************************************************************************
*! Name      : lfAdrShift
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/1999
*! Purpose   : Shift the add. if there is any empty one
*!**************************************************************************
*! Example   : = lfAdrShift()
*!**************************************************************************
*
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

FOR lnCount = 1 TO ALEN(&lcArrayNam.,1)
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam.,1)
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- End of lfAdrShift.

*!**************************************************************************
*! Name      : lfvCurDisp
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/1999
*! Purpose   : Display the Currency Transform Screen
*!**************************************************************************
*! Called from : Called from the currency button
*!**************************************************************************
*! Example   : = lfvCurDisp()
*!**************************************************************************
*
FUNCTION lfvCurDisp
=gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpCur)
*-- End of lfvCurDisp.

*!**************************************************************************
*! Name      : lfvPeriod
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/1999
*! Purpose   : To be sure that there is only one perid is set to  'Yes'
*!**************************************************************************
*! Example   : = lfvPeriod()
*!**************************************************************************
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

*!**************************************************************************
*! Name      : lfvTran
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/1999
*! Purpose   : Disable the invoice option in case of not debit
*!**************************************************************************
*! Example   : = lfvTran()
*!**************************************************************************
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

*!**************************************************************************
*! Name      : lfGetSign
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/1999
*! Purpose   : Get the symbol of the used curr.
*!**************************************************************************
*! Example   : = lfGetSign()
*!**************************************************************************
*
FUNCTION lfGetSign
PRIVATE lcSign

lcSign = SPACE(3)
lcSign = IIF(SEEK(&lcTmpTrans..cCurrCode,'SYCCURR'),SYCCURR.cCurrSmbl,lcSign)
RETURN IIF(lcCurrPost="LEFT",lcSign + "  Amount" ,"Amount  " + lcSign)
*-- End of lfGetSign

*!**************************************************************************
*! Name      : lfIntializ
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/2000
*! Purpose   : To initialize all variables defined in Syrepuvr
*!**************************************************************************
*! Example   : = lfIntializ()
*!**************************************************************************
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
*-- End of lfIntializ.

*!**************************************************************************
*! Name      : lfClearRep
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/2000
*! Purpose   : Clear the Report variables
*!**************************************************************************
*! Example   : =lfClearRep()
*!**************************************************************************
*
FUNCTION lfClearRep
llOGFltCh = .T.
IF llOpnPdChq
  USE IN PostDChq
ENDIF

*-- Close Temporary Cursors [Begin]
IF USED(lcTmpTrans)
  USE IN (lcTmpTrans)

  *C102280,1 BWA 04/05/2000 Use the temp. File. [START]

  *B604447,1 BWA 05/03/2001 Variable ( lcTmpTrn_A ) not found.[START]
  *USE IN (lcTmpTrn_A)
  IF !TYPE('lcTmpTrn_A') = "U"
    IF USED(lcTmpTrn_A)
      USE IN (lcTmpTrn_A)
    ENDIF
  ENDIF
  *B604447,1 [END]
  *C102280,1 [END]

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

*!**************************************************************************
*! Name      : lpFillStru
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/2000
*! Purpose   : To fill Structures of all Arrays creating Temp cursors
*!**************************************************************************
*! Example   : DO lpFillStru
*!**************************************************************************
*
PROCEDURE lpFillStru
PRIVATE lnTranFile
*-- Prepare Arrays
DIMENSION laAgeStru[11,4],laAccStru[16,4],laTranStru[17,4]

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

lnTranFile = ALEN(laTranStru,1)*ALEN(laTranStru,2)

*-- Copy empty structure only.
DIMENSION laTranStru[ALEN(laTranStru,1)+ALEN(laAgeStru,1),4]
=ACOPY(laAgeStru,laTranStru,1,-1,lnTranFile+1)
*-- End of lpFillStru.

*!**************************************************************************
*! Name      : lpCreatFil
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/2000
*! Purpose   : Create work File.
*!**************************************************************************
*! Example   : DO lpCreatFil
*!**************************************************************************
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
INDEX ON cGroupKey + STR(nGroup,6) + IIF(llMulCurr AND lcRpCurr="F",cCurrCode,"") + DTOS(TranDate) + Tran ;
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

*!**************************************************************************
*! Name      : lpCollect
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/2000
*! Purpose   : Collecting Data for Report 
*!**************************************************************************
*! Example   : DO lpCollect
*!**************************************************************************
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

*C200496,1 ABD - Add a trigger for custom David luke to collect data from the arhist file. [Begin]
*=gfDoTriger('ARCSTM',PADR('COLLHIST',10))
*C200496,1 ABD [END]
*-- End of lpCollect.

*!**************************************************************************
*! Name      : lpInsDebit
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/2000
*! Purpose   : Scan Debit for Records matching Criteria
*!**************************************************************************
*! Called from : lpCollect
*!**************************************************************************
*! Example   : DO lpInsDebit
*!**************************************************************************
*
PROCEDURE lpInsDebit
PRIVATE lcScanExpr , lcInstalNo

*-- Set relation with customer file.
SELECT DEBIT
SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER  && Relation with customer.
STORE '' TO m.cAddress4 , m.cAddress5 , m.Region

STORE lcRpExp TO lcDBExp 

*:B607254,1 AKA 05/04/2003 Remove the filer of Factor formthe Credit file. This is temporary fix until we fix the bug # 607231
  DO CASE 
    CASE lcRpRemit = 'F'
      lcDBExp = lcRpExp + IIF(EMPTY(lcRpExp),'',[ AND ]) + [cFacCode = lcRpFactor]
    CASE lcRpRemit = 'H'
      lcDBExp = lcRpExp + IIF(EMPTY(lcRpExp),'',[ AND ]) + [EMPTY(cFacCode)]
  ENDCASE

IF EMPTY(lcDebitFlt)
  lcScanExpr = ALLTRIM(lcDBExp)
ELSE
  lcScanExpr = ALLTRIM(lcDBExp + IIF(EMPTY(lcDBExp),"",[ AND ]) + lcDebitFlt)
ENDIF

*IF EMPTY(lcDebitFlt)
*  lcScanExpr = ALLTRIM(lcRpExp)
*ELSE
*  lcScanExpr = ALLTRIM(lcRpExp + IIF(EMPTY(lcRpExp),"",[ AND ]) + lcDebitFlt)
*ENDIF
*:B607254,1 AKA 05/04/2003 Remove the filer of Factor formthe Credit file. This is temporary fix until we fix the bug # 607231

*-- Scan around Debit file for records matching criteria
SCAN FOR &lcScanExpr
  m.cAddress4 = Customer.cAddress4 
  m.cAddress5 = Customer.cAddress5
  m.Region    = Customer.Region
  lcInstalNo = cInstalNo
  lnAmount = 0
  SCATTER MEMVAR

  *-- If Company Multi Currency and display by Equivalent Amount
  IF llMulCurr AND Amount <> 0 AND (cCurrCode <> gcBaseCurr)

    lnAmount = ROUND(lfAmntDisp(Amount,"O",ldRpExDate,lcRpTmpCur),2)
    
    *-- Checking for the occurence of this currency with "O" type
    IF !SEEK(cCurrCode,lcTmpCurrc)
      INSERT INTO (lcTmpCurrc) (cCurrCode) VALUES (Debit.cCurrCode)
    ENDIF
    
    m.nORate = lfAmntDisp(1,"O",ldRpExDate,lcRpTmpCur)
  ELSE
    lnAmount = Amount
    m.nORate = 1
  ENDIF
  *B129391,1 EIH 10/04/2005 Fix the bug of invalid Amount in case of multi currency[Begin].
  IF llMulCurr AND (lnRpRepCur = 1 OR lnRpRepCur = 0 )
    lnAmount = Amount
  ENDIF  
  *B129391,1 EIH 10/04/2005 [End].
  
  DO lpGetAmnt WITH lcInstalNo    && Calculate the amount from History file
  DO lpDebDeal   && Calculate Ages and fill memory variables.
  DO lpInsRecrd  && Insert New Record in Transaction Temporary File and Totals Temporary file.

ENDSCAN  && end Scan Debit records achieve both customer and debit filters.
*-- EndScan around Debit file for records matching criteria

SET RELATION TO  && Rest relation.
*-- End of lpInsDebit.

*!**************************************************************************
*! Name      : lpDebDeal
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/2000
*! Purpose   : To Calculate the Debit Aging 
*!**************************************************************************
*! Called from : lpInsDebit
*!**************************************************************************
*! Example   : DO lpDebDeal
*!**************************************************************************
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

*!**************************************************************************
*! Name      : lpInsCredt
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/2000
*! Purpose   : Scan Credit for records matching criteria
*!**************************************************************************
*! Called from : lpCollect
*!**************************************************************************
*! Example   : DO lpInsCredt
*!**************************************************************************
*
PROCEDURE lpInsCredt
PRIVATE lcInstalNo
*-- Set relation with customer file.
SELECT CREDIT
SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER  && Relation with customer.
STORE '' TO m.cAddress4 , m.cAddress5 , m.Region

*:B607254,1 AKA 05/04/2003 Remove the filer of Factor formthe Credit file. This is temporary fix until we fix the bug # 607231
STORE lcRpExp TO lcCRExp 
DO CASE  
  CASE lcRpRemit = 'F'
    lcCRExp = lcRpExp 
  CASE lcRpRemit = 'H'
    lcCRExp = lcRpExp + IIF(EMPTY(lcRpExp),'',[ AND ]) + [EMPTY(cFacCode)]
ENDCASE 

*:B607254,1 AKA 05/04/2003 Remove the filer of Factor formthe Credit file. This is temporary fix until we fix the bug # 607231


*-- Scan around Credit file for records matcjing criteria
*:B607254,1 AKA 05/04/2003 Remove the filer of Factor formthe Credit file. This is temporary fix until we fix the bug # 607231
*SCAN FOR &lcRpExp
SCAN FOR &lcCRExp 
*:B607254,1 AKA 05/04/2003 Remove the filer of Factor formthe Credit file. This is temporary fix until we fix the bug # 607231
  lnAmount = 0
  m.cAddress4 = Customer.cAddress4 
  m.cAddress5 = Customer.cAddress5
  m.Region    = Customer.Region

  SCATTER MEMVAR
  IF llMulCurr AND Amount <> 0 AND (cCurrCode <> gcBaseCurr)
    
    lnAmount = ROUND(lfAmntDisp(Amount,"O",ldRpExDate,lcRpTmpCur),2)

    *-- Checking for the occurence of this currency with "O" type
    IF !SEEK(cCurrCode,lcTmpCurrc)
      INSERT INTO (lcTmpCurrc) (cCurrCode) VALUES (Credit.cCurrCode)
    ENDIF

    m.nORate = lfAmntDisp(1,"O",ldRpExDate,lcRpTmpCur)    
  ELSE
    lnAmount = Amount
    m.nORate = 1
  ENDIF  

  lcInstalNo = ''
  DO lpGetAmnt WITH lcInstalNo    && Calculate the amount from History file
  DO lpCreDeal   && Calculate Ages and fill memory variables (OpenCr and NetBal)
  DO lpInsRecrd  && Insert New Record in Transaction Temporary File and Totals Temporary file.

ENDSCAN  && end Scan Credit records achieve both customer and Credit filters.
*-- EndScan around Credit file for records matcjing criteria

SET RELATION TO  && Rest relation.
*-- End of lpInsCredt.

*!**************************************************************************
*! Name      : lpCreDeal
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/2000
*! Purpose   : To Calculate the Credit Aging
*!**************************************************************************
*! Called from : lpInsCredt
*!**************************************************************************
*! Example   : DO lpCreDeal
*!**************************************************************************
*
PROCEDURE lpCreDeal
=lfUpdtAge('C',gdSysDate - TranDate) && Update Date ages

*-- Save open credit and net balance per transaction
STORE m.Amount TO m.OpenCr , m.NetBal

STORE lnAmount TO lnOpenCr , lnNetBal

*-- End of lpCreDeal.

*!**************************************************************************
*! Name      : lfUpdtAge
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/2000
*! Purpose   : Calculate the Aging 
*!**************************************************************************
*! Called from : lpCreDeal OR lpDebDeal
*!**************************************************************************
*! Passed Parameters : lcUpdtTyp --> 'D' for Debit , 'C' for Credit
*!                     lnAgeDays --> No of Days left  
*!**************************************************************************
*! Example   : =lfUpdtAge('C',gdSysDate - TranDate)
*!**************************************************************************
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

*!**************************************************************************
*! Name      : lpGetAmnt
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/2000
*! Purpose   : To Calculate the amount from History file
*!**************************************************************************
*! Called from : lpInsDebit OR lpInsCredt
*!**************************************************************************
*! Example   : DO lpGetAmnt
*!**************************************************************************
*
PROCEDURE lpGetAmnt 
PARAMETER lcInstalNo
*-- If there is Payment in AR History file
*B122284,1 NNA 04/05/2004 (Begin) equalize the nHstamount with the Debit (open) amount from the debit file
*m.nHstAmount = 0
m.nHstAmount = lnAmount
*B122284,1 NNA (End)

IF SEEK(m.Account + m.Tran + lcInstalNo,'ArHist') 
  *B122284,1 NNA 05/04/2004 (Begin) Search with Trantype if the TranNo the same
  lcOldAlis = ALIAS()
  SELECT ArHist
  llFound = .F.
  SCAN REST WHILE Account+Tran+cInstalno = m.Account + m.Tran + lcInstalNo
    *-- [Debit on account] save as tranType (3) in the debit file and tranType (8) in the Arhist file
    *-- [Credit on account] save as tranType (6) in the Credit file and tranType (9) in the Arhist file  
    *B124289,1 BWA 09/15/2004 Fix the bug of the Amount column doesn't match the balance column.[START]
    *IF ArHist.TranType = m.TranType .OR. (m.TranType = '3' .AND. ArHist.TranType = '8') .OR. ;
                                       (m.TranType = '6' .AND. ArHist.TranType = '9')   
    IF ArHist.TranType = m.TranType
    *B124289,1 BWA 09/15/2004 [END]
      llFound=.T.
      *-- Get the nHstAmount by add the debit/Credit amount (From the debit/Credit File) to the ArHist.amount
      *-- for the same Account + TranCD + Instalment#No To get the total amount for the invoice or the instalment
      *m.nHstAmount = ArHist.OpenAmt + ArHist.Amount
      m.nHstAmount = m.nHstAmount + ArHist.Amount
    ELSE
      IF llFound=.F.
        m.nHstAmount = m.Amount
      ENDIF
    ENDIF
  ENDSCAN
  SELECT &lcOldAlis
  *B122284,1 NNA (End)
ELSE
    m.nHstAmount = m.Amount
ENDIF
*-- EndIf of Payment in AR History file
*-- End of lpGetAmnt.  


*!**************************************************************************
*! Name      : lpInsRecrd
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/2000
*! Purpose   : To Calculate the Credit Aging 
*!**************************************************************************
*! Called from : lpInsDebit OR lpInsCredt
*!**************************************************************************
*! Example   : DO lpInsRecrd
*!**************************************************************************
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

*B605492,1 Replace nGroup with -99999. [Begin]
m.nGroup = -1
m.nGroup = -99999
*B605492,1 Replace nGroup with -99999. [End]
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

*!**************************************************************************
*! Name      : lfGetReplc
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/19/2000
*! Purpose   : Get Replaced expression.
*!**************************************************************************
*! Called from : Report code section.
*!**************************************************************************
*! Returns   : lcExpr ---> which means (Sort by expression) 
*!**************************************************************************
*! Example   : =lfGetReplc()
*!**************************************************************************
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

*!**************************************************************************
*! Name      : lfGetAddr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/1999
*! Purpose   : get the address of the customer
*!**************************************************************************
*! Example   : = lfGetAddr()
*!**************************************************************************
*
FUNCTION lfGetAddr

laSoldTo = ''

*B804080,1 HBG 25/03/2001 Print the customer address always if it is FORM 'B' [Begin]
*IF llRpRemit AND !EMPTY(CFACCODE)

*B606179,1 SSE Commented out. [Begin]
*B606179,1 Always get the customer Bill To address.
*IF llRpRemit AND !EMPTY(CFACCODE) AND lcFormType <> "B"
*B804080,1 [End]
*  IF SEEK(CFACCODE,'SYCFACT')
*    = gfGetAdr('SYCFACT', '' , '' , '' , @laSoldTo , 1)
*  ENDIF
*ELSE
*B606179,1 SSE Commented out. [End]

  IF SEEK('M'+Account,'Customer')
    = gfGetAdr('Customer', '' , '' , '' , @laSoldTo , 1)
  ENDIF

  *B607100,1 ABD - Call function to get the division long name. [Begin]
  =gfRltFld(Customer.cDivision , @laDivLName , 'CDIVISION')
  *B607100,1 ABD - [End ]
  
*B606179,1 SSE Commented out. [Begin]
*ENDIF
*B606179,1 SSE Commented out. [End]

=lfAdrShift('laSoldTo')
RETURN ''
*-- End of lfGetAddr

*!**************************************************************************
*! Name      : lfGetFac
*! Developer : Hend Ghanem (HBG)
*! Date      : 25/03/2001
*! Purpose   : get the address of the Factor
*!**************************************************************************
*! Example   : = lfGetFac()
*!**************************************************************************
*B804080,1 
FUNCTION lfGetFac
PARAMETER llDummy

laFactTo = ''
lcFacName = ''

*B606179,1 Change the type of Remit to factor in option grid. [Begin]
*IF llRpRemit AND !EMPTY(cFacCode) AND SEEK(CFACCODE,'SYCFACT')
IF lcRpRemit='F' AND !EMPTY(lcRpFactor) AND SEEK(lcRpFactor,'SYCFACT')
*B606179,1 Change the type of Remit to factor in option grid. [End]

  lcFacName = SYCFACT.cfaccomp
  =gfGetAdr('SycFact', '' , '' , '' , @laFactTo , 1)
  =lfAdrShift('laFactTo')
ENDIF


*!**************************************************************************
*! Name      : lfEndGroup
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/2000
*! Purpose   : Indicate end of group 
*!**************************************************************************
*! Called from : .FRX group footer (Report Form B)
*!**************************************************************************
*! Example     : = lfEndGroup()
*!**************************************************************************
*
FUNCTION lfEndGroup
llEndGrp = .T.
RETURN ''
*-- End of lfEndGroup.

*!**************************************************************************
*! Name      : lfOverMsg
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/1999
*! Purpose   : Evaluate which over message to print.
*!**************************************************************************
*! Passed Parameters  : All parameters are called By Reference :
*!                    : OverMsg1, OverMsg 2, OverMsg3
*!**************************************************************************
*! Returns            : Null, But fill Over Messages.
*!**************************************************************************
*! Example   : = lfOverMsg(@lcOverMsg1 , lcOverMsg2 , lcOverMsg3)
*!**************************************************************************
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

*!**************************************************************************
*! Name      : lfCollTime
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/1999
*! Purpose   : To return the time of collecting data
*!**************************************************************************
*! Example   : = lfCollTime(lcStTime,lcEdTime)
*!**************************************************************************
*
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- End of lfCollTime.

*!**************************************************************************
*! Name      : lpCurrChng
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/1999
*! Purpose   : To Apply the Currency Change
*!**************************************************************************
*! Example   : DO lpCurrChng
*!**************************************************************************
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

*!**************************************************************************
*! Name      : lpChSortBy
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/1999
*! Purpose   : To Apply Changing Sort by	
*!**************************************************************************
*! Example   : DO lpChSortBy
*!**************************************************************************
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

*!**************************************************************************
*! Name      : lpApplyAge
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/1999
*! Purpose   : To Apply the Aging Filter 
*!**************************************************************************
*! Example   : DO lpApplyAge
*!**************************************************************************
*
PROCEDURE lpApplyAge
lcAgeFlt = ''

*-- Account has Total Credit Balance
IF !llRpPrnBal
  
  *B605923,1 Add Zero balance to the filter. [Begin]
  *lcAgeFlt = IIF(EMPTY(lcAgeFlt),"",[ AND ]) + [NetBal >= 0]
  *B119521,1 ABD - If we select "NO" the system should print only ststements 
  *B119521,1 ABD - For accounts that have a positive or negative balance.[Begin]
  *IF llRpPrZero
  *  lcAgeFlt = IIF(EMPTY(lcAgeFlt),"",[ AND ]) + [NetBal >= 0]
  *ELSE

  *B123952,1 BWA 11/18/2004 Fix the bug of the filter "Acc has total credit bal" doesn't work properly.[START]
  *IF !llRpPrZero
  *  lcAgeFlt = IIF(EMPTY(lcAgeFlt),"",[ AND ]) + [NetBal <> 0]
  *  *B119521,1 ABD [End]
  *ENDIF  
  **B605923,1 Add Zero balance to the filter. [End]

  IF llRpPrZero
    lcAgeFlt = IIF(EMPTY(lcAgeFlt),"",[ AND ]) + [NetBal >= 0]
  ELSE
    lcAgeFlt = IIF(EMPTY(lcAgeFlt),"",[ AND ]) + [NetBal > 0]
  ENDIF
ELSE
  IF !llRpPrZero
    lcAgeFlt = IIF(EMPTY(lcAgeFlt),"",[ AND ]) + [NetBal <> 0]
  ENDIF
    *B123952,1 BWA 11/18/2004.[END]
ENDIF

*B123952,1 BWA 11/18/2004 Comment this lines.[START]
**B605923,1 Add Zero balance to the filter. [Begin]
*IF !llRpPrZero AND !("NetBal" $ lcAgeFlt)
*  lcAgeFlt = lcAgeFlt + IIF(EMPTY(lcAgeFlt),"",[ AND ]) + [NetBal <> 0]
*ENDIF  
**B605923,1 Add Zero balance to the filter. [End]
*B123952,1 BWA 11/18/2004.[END]
 
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

*!**************************************************************************
*! Name      : lpGroupChg
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/1999
*! Purpose   : To Apply the Aging Filter 
*!**************************************************************************
*! Example   : DO lpGroupChg
*!**************************************************************************
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
          INSERT INTO (lcTmpTrans) FROM MEMVAR    
        ENDFOR
        SKIP
      ENDIF
      *-- Endif of no of lines less than required lines
      
    ENDIF
    *-- Endif of cGroupKey is found in lcTmpTrans File
  ENDSCAN
  *-- EndScan loop around Temp Account file
  *SET DATE TO &lcDateType
ENDIF
*-- If Form is A and user changed Filter or currency is changed 

*-- End of lpGroupChg  

*!**************************************************************************
*! Name      : lpApplyRel
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/1999
*! Purpose   : To Apply the Relation before displaying of Report
*!**************************************************************************
*! Example   : DO lpApplyRel
*!**************************************************************************
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

*!**************************************************************************
*! Name      : lfAmntDisp
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/2000
*! Purpose   : Display Foreign Currency in Equivalent Base currency
*!**************************************************************************
*! Example   : =lfAmntDisp
*!**************************************************************************
*! Notes     : This function is a exact copy from gfAmntDisp()
*!**************************************************************************
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

      *C200417,1 RAE [start]
      lnUnit = IIF(lnUnit = 0 , 1 , lnUnit)
      *C200417,1 RAE [end]

      lnExRate = IIF(llAprvCurr , gfChkRate('lnUnit' , lcCurrCode , DINVDATE , .F.) , NEXRATE)
    ELSE
      lnUnit = &lcGetFile..NCURRUNIT
      lnExRate = IIF(llAprvCurr , gfChkRate('lnUnit' , lcCurrCode , &lcGetFile..DINVDATE , .F.) , &lcGetFile..NEXRATE)
    ENDIF  
    
    *B129391,1 EIH 10/04/2005 Fix the bug of invalid Amount in case of multi currency[Begin].
    *lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
    lnExRate = IIF((lnRpRepCur = 1 OR lnRpRepCur = 0) ,1,IIF( lnExRate <> 0 , lnExRate , 1))
    *B129391,1 EIH 10/04/2005 Fix the bug of invalid Amount in case of multi currency[Begin].
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
    
    *lnAmount   = lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit
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
    
    *lnAmount   = lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit
    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,6)
ENDCASE

SELECT (lnSavAlias)

RETURN lnAmount
*-- End of lfAmntDisp.

*!**************************************************************************
*! Name      : lfBreakRel
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/18/2000
*! Purpose   : BREAK ALL THE RELATIONS.
*!**************************************************************************
*! Example   : =lfBreakRel()
*!**************************************************************************
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

*!**************************************************************************
*! Name      : lfvInvoice
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 02/04/2000
*! Purpose   : Validation of Only open invoice option.
*!**************************************************************************
*! Example   : =lfvInvoice()
*!**************************************************************************
*604171,1 Disable charge back if only open invoice is Yes
FUNCTION lfvInvoice

IF llRpInvoie
  llRpPrnCha = .F.
ENDIF

*-- Disable ChargeBack Option in case of Only Open invoice YES
laOGObjCnt[lfVarPos('llRpPrnCha')] = !llRpInvoie
=lfOGShowGet('llRpPrnCha')
*-- End of lfvInvoice.

*!**************************************************************************
*! Name      : lfvRemit
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 02/04/2000
*! Purpose   : Validation for the new Remit. 
*!**************************************************************************
*! Example   : =lfvRemit()
*!**************************************************************************
*B606179,1 
FUNCTION lfvRemit
CLEAR READ
*B606951,1 WAB (Start) - in case of Both and House let the llRpRemit = .F.
*IF lcRpRemit = 'H'
IF lcRpRemit <> 'F'
*B606951,1 WAB (END)
  lcRpFactor = ''
  llRpRemit = .F.
ELSE
  llRpRemit = .T.
ENDIF
*-- End of lfvRemit.

*!**************************************************************************
*! Name      : lfvFactor
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 02/04/2000
*! Purpose   : Validation for the new factor field.
*!**************************************************************************
*! Example   : =lfvFactor()
*!**************************************************************************
*B606179,1 
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

*!**************************************************************************
*! Name      : lfCutDate
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 02/04/2000
*! Purpose   : Get Cut off date.
*!**************************************************************************
*! Example   : =lfCutDate()
*!**************************************************************************
*B606337,1
FUNCTION lfCutDate
PRIVATE lnTranDate
lnTranDate = lfItmPos('DEBIT.TRANDATE')   && get Tran. Date Fixed filter Position
ldCutDate = {}

*B123396,1 BWA 06/24/2004 Set the century.[START]
SET CENTURY &lcCentVal
*B123396,1 BWA 06/24/2004.[END]

*-- If Trans. Date found in Fixed filter option
IF lnTranDate > 0
  *-- Replace TO DATE in Transaction date with Current Date (System Date)
  *-- If system uses 4 Char. for Century 
  IF "ON" $ SET('CENTURY')
    ldCutDate = CTOD(SUBSTR(laOGFxFlt[lnTranDate,6],12,10))
  ELSE    && Else system uses 2 char. for Century
    ldCutDate = CTOD(SUBSTR(laOGFxFlt[lnTranDate,6],12,8))
  ENDIF
  *-- EndIf of system uses 4 Char. for Century 
ENDIF
ldCutDate = IIF(EMPTY(ldCutDate),gdsysDate,ldCutDate)
*-- End of lfCutDate.

*B608529 Mos 04/22/2008 fix bug of wrong factor address[Start]
*!**************************************************************************
*! Name      : lfGroupTot
*! Developer : Mostafa Eid
*! Date      : 04/22/2008
*! Purpose   : get the factor address
*!**************************************************************************
FUNCTION lfGetRemAdd
IF lcRpRemit = 'B' 
  IF !EMPTY(EVALUATE(lcTmpAcct+'.cFacCode')) AND SEEK(EVALUATE(lcTmpAcct+'.cFacCode'),'SYCFACT')  &&Change the type of Remit to factor in option grid. 
    lcFacName = SYCFACT.cfaccomp
    =gfGetAdr('SycFact', '' , '' , '' , @laFactTo , 1)
    =lfAdrShift('laFactTo')
  ELSE
    lcFacName = lcName
    =ACOPY(laCompany,laFactTo)
  ENDIF 
ENDIF
RETURN ''  
*B608529 Mos 04/22/2008 fix bug of wrong factor address [End]