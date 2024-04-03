*:************************************************************************
*: Program file  	  : ARCHADJ.PRG
*: Program desc. 	  : CASH & ADJUSTMENTS JOURNAL REPORT
*: System        	  : Aria 4XP
*: Module        	  : ACCOUNT RECEIVABLE (AR)
*: Developer     	  : BASSEM RAAFAT ERNEST(BWA)
*: Date                : 02/05/2006
*: Tracking Job Number : N000540
*: Notes               : This program is built on the standard program AR930
*:************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : lfwOGWhen(), lfvCAB(), lfvCashTyp(), lfsrvSty()
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO ARCHADJ
*:************************************************************************
*: MODIFICATIONS:
*:T20060908.0022 - AYM 12/18/2006 :DO NOT USE LCRPEXP BUILD NEW EXPRESSIONS
*:B608149,1 WAM 07/03/2007 Browse batches from ARHIST file as well
*:B608350,1 MMT 11/13/2007 fix bug of not converting amount to base currency[T20071102.0002]
*:E302635,1 HES 09/01/2009 Option Grid Confusing Issue [T20081119.0017]
*:B611113,1 MMT 02/10/2016 Add Sort by Batch# in Batch browser[P20150112.0001 - Issue#51]
*:B611533,1 SAH 02/21/2018 modify report program to include Trantype '3' same it checks Trantype '2' in the filter expressions [T20180219.0005]
*:E611972,1 MMT 02/19/2020 Add code to collect date from new GL tables [GL Enhancement]
*:************************************************************************
*  TRAN CODES             FILE-ID
*  0 = CREDIT MEMOS       CREDIT
*  1 = INVOICES           DEBIT
*  3 = CHARGEBACK IN      DEBIT
*  4 = PAYMENT            CREDIT
*  5 = CREDIT ADJUSTMENT  CREDIT
*  2 = DEBIT ADJUSTMENT   DEBIT
*  6 = CREDIT ON ACCOUNT  CREDIT
*  7 = ALLOWANCE          CREDIT
*  8 = CHR.BCK            ARHIST
*  9 = CREDIT ON ACCT     ARHIST
*!*	_SCREEN.Visible = .T.
*!*	ACTIVATE WINDOW TRACE
*!*	SUSPEND
#INCLUDE R:\Aria4xp\reports\ar\archadj.H
*:T20060908.0022 - AYM 12/18/2006 :DO NOT USE LCRPEXP BUILD NEW EXPRESSIONS  --BEGIN
*:E611972,1 MMT 02/19/2020 Add code to collect date from new GL tables [GL Enhancement][Start]
IF llPrintMod
  lfReconWithGL()
  RETURN 
ENDIF
*:E611972,1 MMT 02/19/2020 Add code to collect date from new GL tables [GL Enhancement][End]
If llOgFltCh
  llDontPrn=.F.
  *--Define New Variable To Hold The Desc Reason.
  Store '' To lcTranCods

  *  BEGIN TRANSACTION
  Store ' .T. ' To lcHstExp,lcDbExp,lcCrExp

  * Customer Filter
  lcCusFltr= ''
  lcCusFltr= lfCheckFilter(1, 'CUSTOMER.ACCOUNT')
  llCusFltr   = !Empty(lcCusFltr) And Used(lcCusFltr) And Reccount(lcCusFltr) > 0
  If llCusFltr
    Select (lcCusFltr)
    Index On ACCOUNT Tag (lcCusFltr)
    lcHstExp=lcHstExp+" AND SEEK( CUSTOMER.ACCOUNT,'"+lcCusFltr+"') "
    lcDbExp=lcDbExp+" AND SEEK( CUSTOMER.ACCOUNT,'"+lcCusFltr+"') "
    lcCrExp=lcCrExp+" AND SEEK( CUSTOMER.ACCOUNT,'"+lcCusFltr+"') "
  Else
    If Type("lcCusFltr") = "C" And Used(lcCusFltr)
      Use In (lcCusFltr)
    Endif
    lcCusFltr= ''
  Endif

  * SalesRep Filter
  lcRepFltr= ''
  lcRepFltr= lfCheckFilter(1, 'CUSTOMER.SALESREP')
  llRepFltr   = !Empty(lcRepFltr) And Used(lcRepFltr) And Reccount(lcRepFltr) > 0
  If llRepFltr
    Select (lcRepFltr)
    Index On REPCODE Tag (lcRepFltr)
    lcHstExp=lcHstExp+" AND (SEEK( CUSTOMER.SALESREP,'"+lcRepFltr+"') .OR. SEEK( CUSTOMER.REP2,'"+lcRepFltr+"'))"
    lcDbExp=lcDbExp+" AND (SEEK( CUSTOMER.SALESREP,'"+lcRepFltr+"') .OR. SEEK( CUSTOMER.REP2,'"+lcRepFltr+"'))"
    lcCrExp=lcCrExp+" AND (SEEK( CUSTOMER.SALESREP,'"+lcRepFltr+"') .OR. SEEK( CUSTOMER.REP2,'"+lcRepFltr+"'))"

  Else
    If Type("lcRepFltr") = "C" And Used(lcRepFltr)
      Use In (lcRepFltr)
    Endif
    lcRepFltr= ''
  Endif
  * BATCH Filter
  lcBATFltr= ''
  lcBATFltr= lfCheckFilter(1, 'CREDIT.BATCH')
  llBATFltr   = !Empty(lcBATFltr) And Used(lcBATFltr) And Reccount(lcBATFltr) > 0
  If llBATFltr
    Select (lcBATFltr)
    Index On Batch Tag (lcBATFltr)
    lcHstExp=lcHstExp+" AND SEEK( BATCH,'"+lcBATFltr+"') "
    lcDbExp=lcDbExp+" AND SEEK( BATCH,'"+lcBATFltr+"') "
    lcCrExp=lcCrExp+" AND SEEK (BATCH,'"+lcBATFltr+"') "

  Else
    If Type("lcBATFltr") = "C" And Used(lcBATFltr)
      Use In (lcBATFltr)
    Endif
    lcBATFltr= ''
  Endif

  * Check if there is a filter on DEBIT REASON
  lcCurName = lfCheckFilter(1, "IIF(TRANTYPE='2',DEBIT.TRANCODE,'')")
  lcDBT   = loOgScroll.gfTempName()
  llDBT   = !Empty(lcCurName ) And lfStr2Curs(lcCurName ,lcDBT   ,"CDEB")
  If llDBT
    Select (lcDBT)
    Index On CDEB Tag (lcDBT )
    *B611533,1 SAH 02/21/2018 modify report program to include Trantype '3' same it checks Trantype '2' in the filter expressions [begin]
    *lcHstExp=lcHstExp+" AND IIF(TRANTYPE='2'  ,SEEK (TRANCODE,'"+lcDBT   +"')"+", .T. )"
    *lcDbExp=lcDbExp+" AND IIF(TRANTYPE='2',SEEK (TRANCODE,  '"+lcDBT   +"')"+", .T. )"
        
    lcHstExp=lcHstExp+" AND IIF((TRANTYPE='2' or TRANTYPE='3') ,SEEK (TRANCODE,'"+lcDBT   +"')"+", .T. )"
    lcDbExp=lcDbExp+" AND IIF((TRANTYPE='2' or TRANTYPE='3') ,SEEK (TRANCODE,  '"+lcDBT   +"')"+", .T. )"
    *B611533,1 SAH 02/21/2018 modify report program to include Trantype '3' same it checks Trantype '2' in the filter expressions [end]

  Endif

  *Check if there is a filter on CREDIT  REASON
  lcCurName = lfCheckFilter(1, "IIF(TRANTYPE='5',CREDIT.CCREDITCOD,'')")
  lcCRDT   = loOgScroll.gfTempName()
  llCRDT   = !Empty(lcCurName ) And lfStr2Curs(lcCurName ,lcCRDT,"CCRDT")
  If llCRDT
    Select (lcCRDT)
    Index On CCRDT Tag (lcCRDT)
    lcHstExp=lcHstExp+" AND IIF(TRANTYPE $ '57',SEEK (TRANCODE,'"+lcCRDT+"')"+", .T. )"
    lcCrExp=lcCrExp+" AND IIF(TRANTYPE $ '5'   ,SEEK (CCREDITCOD,'"+lcCRDT+"')"+", .T. )"

  Endif

  * Check if there is a filter on PAYMENT TYPE
  lcCurName = lfCheckFilter(1, "IIF(TRANTYPE='4',CREDIT.CARPTYPE,'')")
  lcCRTYP   = loOgScroll.gfTempName()
  llCTYP   = !Empty(lcCurName ) And lfStr2Curs(lcCurName ,lcCRTYP,"CCTYP")
  If llCTYP
    Select (lcCRTYP )
    Index On CCTYP Tag (lcCRTYP )
    lcHstExp=lcHstExp +" AND IIF(TRANTYPE ='4',SEEK (CARPTYPE,'" + lcCRTYP  +"')"+", .T. )"
    lcCrExp=lcCrExp   +" AND IIF(TRANTYPE ='4',SEEK (CARPTYPE,'" + lcCRTYP  +"')"+", .T. )"
  Endif

  lnDatePos = lfItmPos('CREDIT.TRANDATE')
  If lnDatePos > 0
    If  !Empty(laOGFxFlt[lnDatePos ,6])
      ldStrtDate = Ctod(Substr(laOGFxFlt[lnDatePos ,6],1, Atc('|',laOGFxFlt[lnDatePos ,6])-1))
      ldEndDate  = Ctod(Substr(laOGFxFlt[lnDatePos ,6],   Atc('|',laOGFxFlt[lnDatePos ,6])+1))
      lcHstExp=lcHstExp+" AND BETWEEN(TRANDATE,ldStrtDate,ldEndDate) "
      lcCrExp=lcCrExp+" AND BETWEEN(TRANDATE,ldStrtDate,ldEndDate) "
      lcDbExp=lcDbExp+" AND BETWEEN(TRANDATE,ldStrtDate,ldEndDate) "

    Endif
  Endif
  *:T20060908.0022 - AYM 12/18/2006 :DO NOT USE LCRPEXP BUILD NEW EXPRESSIONS  --END

  *-- Add to the filter condition if the Amount <> 0.
  lcRpFlt = " .AND. AMOUNT <> 0 "

  *:E302635,1 HES 09/01/2009 Handle the New Option "Select Debit or Credit" [Start]
  *-- Add the filter of cash & Adj type
  *!*	DO CASE
  *!*	  CASE lcrpCAB  = 'C'                                 &&-CASH ONLY
  *!*	    lcRpFlt = lcRpFlt +" .AND.TRANTYPE $'4' "
  *!*	  CASE lcrpCAB  = 'A'                                 &&-ADJUSTMENTS ONLY
  *!*	    lcRpFlt = lcRpFlt +" .AND.TRANTYPE $'257' "
  *!*	  CASE lcrpCAB  = 'B'
  *!*	    lcRpFlt = lcRpFlt +" .AND.TRANTYPE $'2457'"
  *!*	ENDCASE

  *-- Add the filter of Debit or Cridit type
  Do Case
  Case lcRpCAB = 'A' And lcrpDbOrCr = 'C'      &&-CRIDIT ADJUSTMENTS ONLY
    lcRpFlt = lcRpFlt +" .AND.TRANTYPE $ '57' "

  Case lcRpCAB = 'A' And lcrpDbOrCr = 'D'      &&-DEBIT ADJUSTMENTS ONLY
    *:B611533,1 SAH 02/21/2018 modify report program to include Trantype '3' same it checks Trantype '2' in the filter expressions [begin]
    *lcRpFlt = lcRpFlt +" .AND.TRANTYPE $ '2'"
    lcRpFlt = lcRpFlt +" .AND.TRANTYPE $ '23'"
    *:B611533,1 SAH 02/21/2018 modify report program to include Trantype '3' same it checks Trantype '2' in the filter expressions [end]

  Case lcRpCAB = 'A' And lcrpDbOrCr = 'B'      &&-DEBIT & CRIDIT ADJUSTMENTS ONLY
    *:B611533,1 SAH 02/21/2018 modify report program to include Trantype '3' same it checks Trantype '2' in the filter expressions [begin]
    *lcRpFlt = lcRpFlt +" .AND.TRANTYPE $ '257'"
    lcRpFlt = lcRpFlt +" .AND.TRANTYPE $ '2357'"
    *:B611533,1 SAH 02/21/2018 modify report program to include Trantype '3' same it checks Trantype '2' in the filter expressions [end]
  Case lcRpCAB = 'C'                           &&-CASH ONLY
    lcRpFlt = lcRpFlt +" .AND.TRANTYPE $ '4'"

  Case lcRpCAB = 'B' And lcrpDbOrCr = 'B'      &&-CASH & Adjustment
    *:B611533,1 SAH 02/21/2018 modify report program to include Trantype '3' same it checks Trantype '2' in the filter expressions [begin]
    *lcRpFlt = lcRpFlt +" .AND.TRANTYPE $ '2457'"
    lcRpFlt = lcRpFlt +" .AND.TRANTYPE $ '23457'"
    *:B611533,1 SAH 02/21/2018 modify report program to include Trantype '3' same it checks Trantype '2' in the filter expressions [end]

  Case lcRpCAB = 'B' And lcrpDbOrCr = 'C'      &&-CASH & CRIDIT ADJUSTMENTS ONLY
    lcRpFlt = lcRpFlt +" .AND.TRANTYPE $ '457'"

  Case lcRpCAB = 'B' And lcrpDbOrCr = 'D'      &&-CASH & DEBIT ADJUSTMENTS ONLY
    *:B611533,1 SAH 02/21/2018 modify report program to include Trantype '3' same it checks Trantype '2' in the filter expressions [begin]
    *lcRpFlt = lcRpFlt +" .AND.TRANTYPE $ '24'"
    lcRpFlt = lcRpFlt +" .AND.TRANTYPE $ '234'"
    *:B611533,1 SAH 02/21/2018 modify report program to include Trantype '3' same it checks Trantype '2' in the filter expressions [end]
  Endcase
  *:E302635,1 HES 09/01/2009 Handle the New Option "Select Debit or Credit [END]

  *-- Add the filter of Factored & Nofactored accounts
  Do Case
  Case lcrpFAcct='F'
    lcRpFlt = lcRpFlt + " .AND. !EMPTY(Customer.cFacCode) "
  Case lcrpFAcct='N'
    lcRpFlt = lcRpFlt + " .AND. EMPTY(Customer.cFacCode) "
  Endcase

  *--Define Currency Filter.
  lcCurrFilt = ""

  *-- if Print in Forign currency (Filter on selected currency)
  If llRpForCur
    lcCurrFilt = [ AND cCurrCode = lcRpCurr]
    lcHstExp= lcHstExp+ lcCurrFilt
    lcCrExp=  lcCrExp+ lcCurrFilt
    lcDbExp=  lcDbExp+ lcCurrFilt

  Endif


  *--------------------------------------------
  * SELECT RECORDS FROM ARHIST FILE
  *--------------------------------------------
  Select ARHIST
  Set Relation To 'M'+ACCOUNT Into CUSTOMER

  lcHstExp= lcHstExp+ lcRpFlt
  lcHstExp= lcHstExp+ " .AND. !(TRANTYPE $ '89') "
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *WAIT WINDOW Lang_SELECTING_AR_HISTORY_RECORDS NOWAIT
  Wait Window Iif(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_SELECTING_AR_HISTORY_RECORDS,oAriaApplication.GetHeaderText("Lang_SELECTING_AR_HISTORY_RECORDS",AHEADERFILE)) Nowait
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *:T20060908.0022 - AYM 12/18/2006 :DO NOT USE LCRPEXP BUILD NEW EXPRESSIONS  -- BEGIN
  If Used(TRANFILE)
    Use In (TRANFILE)
  Endif
  *:T20060908.0022 - AYM 12/18/2006 :DO NOT USE LCRPEXP BUILD NEW EXPRESSIONS  -- END
  Copy All To (oAriaApplication.Workdir+TRANFILE) For &lcHstExp

  Select ARHIST
  Set Relation To
  =gfOpentable(oAriaApplication.Workdir+TRANFILE,'','EX')

  *---------------------------------------
  * SELECT RECORDS FROM OPEN CREDIT FILE
  *---------------------------------------
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *WAIT WINDOW LANG_SELECTING_CREDIT_RECORDS NOWAIT
  Wait Window Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SELECTING_CREDIT_RECORDS,oAriaApplication.GetHeaderText("LANG_SELECTING_CREDIT_RECORDS",AHEADERFILE)) Nowait
  *N000682,1 11/20/2012 MMT Globlization changes[End]



  lcCrExp= lcCrExp+ lcRpFlt
  If lcRpCAB $ 'CB' .And. lcrpCshT<>'B'
    Do Case
    Case lcrpCshT = 'A'
      lcCrExp= lcCrExp+ ".AND.IIF(TranType='4',!lNonAR,.T.)"
    Case lcrpCshT = 'N'
      lcCrExp= lcCrExp+ ".AND.IIF(TranType='4',lNonAR,.T.)"
    Endcase
  Endif

  Select CREDIT
  Set Relation To 'M'+ACCOUNT Into CUSTOMER
  Copy All To (oAriaApplication.Workdir+WORKFILE) For &lcCrExp


  Select CREDIT
  Set Relation To

  *--Add credit records to the main temporary file.
  =gfOpentable(oAriaApplication.Workdir+WORKFILE,'','EX')
  Locate
  Scan
    Scatter Memvar Memo
    m.TranCode = m.cCreditCod
    Insert Into (TRANFILE) From Memvar
  Endscan
  Use

  *--------------------------------------------
  * COPY DEBIT DATA IF PROCESSING ADJUSTMENTS
  *--------------------------------------------
  If lcRpCAB <> 'C'

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *WAIT WINDOW LANG_SELECTING_DEBITS_FOR_REPORT NOWAIT
    Wait Window Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SELECTING_DEBITS_FOR_REPORT,oAriaApplication.GetHeaderText("LANG_SELECTING_DEBITS_FOR_REPORT",AHEADERFILE)) Nowait
    *N000682,1 11/20/2012 MMT Globlization changes[End]

    Select DEBIT
    lcDbExp= lcDbExp+ lcRpFlt

    Set Relation To 'M'+ACCOUNT Into CUSTOMER
    Copy All To (oAriaApplication.Workdir+WORKFILE) For &lcDbExp
    Select DEBIT
    Set Relation To
    Select &TRANFILE
    Append From (oAriaApplication.Workdir+WORKFILE)
  Endif

  Select (TRANFILE)
  Locate
  If Eof()
    llDontPrn=.T.
    *Message 'There are no records to display...!'
    =gfModalGen('TRM00052B00000','DIALOG')
    Return
  Endif

  XTITLE   = ''
  If lnDatePos > 0
    lnDatePos = Asubscript(laOGFxFlt,lnDatePos,1)

    Do Case
    Case Empty(Ctod(Substr(laOGFxFlt[lnDatePos,6],1,10))) .And. Empty(Ctod(Substr(laOGFxFlt[lnDatePos,6],12,21)))
      XTITLE   = ''

    Case Empty(Ctod(Substr(laOGFxFlt[lnDatePos,6],1,10))) .And. !Empty(Ctod(Substr(laOGFxFlt[lnDatePos,6],12,21)))
      Hdata     = Substr(laOGFxFlt[lnDatePos,6],12,21)
      XTITLE    = "PERIOD: To &Hdata "

    Case !Empty(Ctod(laOGFxFlt[lnDatePos,6]))
      Ldata    = Substr(laOGFxFlt[lnDatePos,6],1,10)
      Hdata    = Substr(laOGFxFlt[lnDatePos,6],12,21)
      XTITLE   = "PERIOD FROM: &Ldata To &Hdata "
    Endcase
  Endif

  Do Case
    *--CUSTOMER
  Case lcRpSRTCD = 'C'
    XSORT   = 'ACCOUNT+TRANTYPE+TRANCODE+TRAN'
    HBREAK2 = 'ACCOUNT'
    HBREAK1 = 'TRANTYPE+TRANCODE'
    XTITLE  = XTITLE + " (SORT: CUSTOMER)"

    *--TRANSACTION NUMBER
  Case lcRpSRTCD = 'T'
    XSORT   = 'TRANTYPE+TRANCODE+DTOS(TRANDATE)+TRAN'
    HBREAK2 = 'TRANTYPE'
    HBREAK1 = 'TRANCODE'
    XTITLE  = XTITLE + " (SORT: TRANSACTION)"

    *--BATCH NUMBER
  Case lcRpSRTCD = 'B'
    XSORT   = 'BATCH+TRANTYPE+TRANCODE+TRAN'
    HBREAK2 = 'BATCH'
    HBREAK1 = 'TRANTYPE+TRANCODE'
    XTITL  = XTITLE + " (SORT: BATCH)"

    *--REASON CODE
  Case lcRpSRTCD = 'R'
    XSORT   = 'TRANCODE+TRANTYPE+TRAN'
    HBREAK2 = 'TRANCODE'
    HBREAK1 = 'TRANCODE+TRANTYPE'
    XTITLE  = XTITLE + " (SORT: REASON CODE)"
  Endcase

  Z = Ltrim(Str(Reccount(),7))
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *WAIT WINDOW Lang_SORTING_Z_RECORDS NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  Set Talk On
  Index On &XSORT Tag &TRANFILE

  Set Talk Off
  Set Order To Tag &TRANFILE

  lcTmpTrn = loOgScroll.gfTempName()
  lcTmp2DB = loOgScroll.gfTempName()
  =lfAdjustCRSettings()

  Select (TRANFILE)
  =Afields(laFileStru)

  =lfAddField("laFileStru", "Reason" ,"C",24,0,.T.,.F.,"","","","","","","","","","",0,0)
  =lfAddField("laFileStru", "CustName" ,"C",20,0,.T.,.F.,"","","","","","","","","","",0,0)

  Dimension laIndx[1,2]
  laIndx[1,1] = XSORT
  laIndx[1,2] = (lcTmp2DB)

  =gfCrtTmp(lcTmp2DB,@laFileStru,@laIndx,lcTmp2DB,.F.)

  Select (lcTmp2DB)
  Copy All To (oAriaApplication.Workdir + "TRANFILE.DBF")

  Select (TRANFILE)
  Scan
    Scatter Memvar Memo
    *:B608350,1 MMT 11/13/2007 fix bug of not converting amount to base currency[Start]
    If llMultCurr And !llRpForCur
      m.Amount = lfBaseAmt()
    Endif
    *:B608350,1 MMT 11/13/2007 fix bug of not converting amount to base currency[End]

    Select (lcTmp2DB)
    Append Blank
    Gather Memvar Memo
    Replace &lcTmp2DB..CustName With Iif(Seek('M' + &TRANFILE..ACCOUNT , 'CUSTOMER') , Left(CUSTOMER.BTNAME ,20) , "")
    *:B611533,1 SAH 02/21/2018 modify report program to include Trantype '3' same it checks Trantype '2' in the filter expressions [begin]
    *IF TRANTYPE = '2'
    If TRANTYPE = '2' Or TRANTYPE = '3'
      *:B611533,1 SAH 02/21/2018 modify report program to include Trantype '3' same it checks Trantype '2' in the filter expressions [end]
      lcDesc = GFCODDES(&TRANFILE..TranCode,'TRANCODE')
      lcDesc = Iif( Alltrim(lcDesc) = "N/A" , "" , lcDesc)
      Replace &lcTmp2DB..Reason With lcDesc
    Else
      lcDesc2 = GFCODDES(TranCode,'CCREDITCOD')
      lcDesc2 = Iif( Alltrim(lcDesc2) = "N/A" , "" , lcDesc2)
      Replace &lcTmp2DB..Reason With lcDesc2
    Endif

  Endscan
  Select (lcTmp2DB)
  Locate
  Copy All To (oAriaApplication.Workdir + lcTmpTrn + ".DBF")

  *:T20060908.0022 - AYM 12/18/2006 :DO NOT USE LCRPEXP BUILD NEW EXPRESSIONS  --BEGIN
  *gfDispRe
  If Used(lcTmpTrn )
    Use In (lcTmpTrn )
  Endif

  =gfDispRe()
Else
  If llDontPrn
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    Return
  Else
    =gfDispRe()
  Endif
Endif
*:T20060908.0022 - AYM 12/18/2006 :DO NOT USE LCRPEXP BUILD NEW EXPRESSIONS  --END




*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 02/05/2006
*! Purpose   : The when function of the option grid.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOGWhen()
*!*************************************************************
Function lfwOGWhen

*--Fill Currency arrays.
=lfFillCurr()

*--Get position of Sales Rep.
lnSlRepPos = lfItmPos('CUSTOMER.SALESREP')

*-- Get the position of the print cost setting in the array to enable or
*-- disable it.
lnPos= Asubscript(laOgObjType,Ascan(laOgObjType,Upper('lcrpCshT')),1)
laOGObjCnt[lnPos] = (lcRpCAB<>'A')
lcrpCshT = 'B'
= lfOGShowGet('lcrpCshT')

*-- Company use Multi Currency.
If llMultCurr
  lnCurrPos  = lfGetVarPos("lcRpCurr")

  *-- if it is User Saved Filter and your saving is to print by
  *-- equavelent in base amount.
  If !(lnOGSeting = 1 Or llRpForCur)
    =lfvForCurr()
  Endif

Else  && Company use single currency
  *-- Rest Currency variables.
  lcRpCurr   = ""
  llRpForCur = .F.
Endif

*--Define Variable sort With the Defa value.
lcRpSRTCD = "T"

*--End of lfwOGWhen.
*!*************************************************************
*! Name      : lfvCAB
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 02/05/2006
*! Purpose   : Valid function of CASH & ADJUSTMENTS setting in the
*!             option grid to enable or disable Cash type setting
*!             (AR or NON AR or BOTH)
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfOGShowGet()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvCAB()
*!*************************************************************
Function lfvCAB
Private lnArreyLen

*:E302635,1 HES 09/01/2009 Make a problem while trying to supress the "Select Debit or Credit" Option [Start]
*!*	lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,UPPER('lcrpCshT')),1)
*!*	laOGObjCnt[lnPos] = (lcrpCAB<>'A')
*!*	lcrpCshT = 'B'

*!*	= lfOGShowGet('lcrpCshT')
*:E302635,1 HES 09/01/2009 Make a problem while trying to supress the "Select Debit or Credit" Option [End]

lnArreyLen = Alen(laDataVal,1)

*--Add a new option of sorting in case of adjustment only.
If lcRpCAB = "A"
  Dimension laDataDes[4,1],laDataVal[4,1]
  *N000682,1 MMT 02/10/2013 Globalization changes[Start]
  *!*	  laDataDes[1] = "Transaction"
  *!*	  laDataDes[2] = "Customer"
  *!*	  laDataDes[3] = "Batch"
  *!*	  laDataDes[4] = "Reson Code"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *laDataDes[1] = LANG_TRANS
  laDataDes[1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TRANS,oAriaApplication.GetHeaderText("LANG_TRANS",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *laDataDes[2] = LANG_CUSTOMER
  laDataDes[2] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CUSTOMER,oAriaApplication.GetHeaderText("LANG_CUSTOMER",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *laDataDes[3] = LANG_BATCH
  laDataDes[3] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BATCH,oAriaApplication.GetHeaderText("LANG_BATCH",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *laDataDes[4] = LANG_REASONCODE
  laDataDes[4] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_REASONCODE,oAriaApplication.GetHeaderText("LANG_REASONCODE",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 02/10/2013 Globalization changes[End]
  laDataVal[1] = "T"
  laDataVal[2] = "C"
  laDataVal[3] = "B"
  laDataVal[4] = "R"
Else
  Dimension laDataDes[3,1],laDataVal[3,1]
  *N000682,1 MMT 02/10/2013 Globalization changes[Start]
  *!*	  laDataDes[1] = "Transaction"
  *!*	  laDataDes[2] = "Customer"
  *!*	  laDataDes[3] = "Batch"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *laDataDes[1] = LANG_TRANS
  laDataDes[1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TRANS,oAriaApplication.GetHeaderText("LANG_TRANS",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *laDataDes[2] = LANG_CUSTOMER
  laDataDes[2] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CUSTOMER,oAriaApplication.GetHeaderText("LANG_CUSTOMER",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *laDataDes[3] = LANG_BATCH
  laDataDes[3] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BATCH,oAriaApplication.GetHeaderText("LANG_BATCH",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 02/10/2013 Globalization changes[End]
  laDataVal[1] = "T"
  laDataVal[2] = "C"
  laDataVal[3] = "B"
Endif

If lnArreyLen != Alen(laDataVal,1)
  lcRpSRTCD = "T"
  *:E302635,1 HES 09/01/2009
  *!*	  CLEAR READ
  CLEARREAD()
  *:E302635,1 HES 09/01/2009
Endif

*:E302635,1 HES 09/01/2009 Refresh OG [Start]
=CLEARREAD()
*:E302635,1 HES 09/01/2009 Refresh OG [End]

*--End of lfvCAB.
*!*************************************************************
*! Name      : lfvCashTyp
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 02/05/2006
*! Purpose   : To clear the selection of the in range function of the account
*!*************************************************************
*! Called from : Option Grid from two setting (Cash Type, Account Type)
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvCashTyp()
*!*************************************************************
Function lfvCashTyp

*-- Variable To Clear Selection From The InRang If Disable The Bottom.
llClear = .T.

*-- Get the position of the Customer in the array to enable or
*-- disable it.
If Ascan(laOGFxFlt,"CUSTOMER.ACCOUNT") # 0
  lnSrcLoc   = Asubscript(laOGFxFlt,Ascan(laOGFxFlt,"CUSTOMER.ACCOUNT"),1)
  *--Enable the object.
  laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGFxFlt,1) - ALEN(laOGFxFlt,1)+ lnSrcLoc] =  (lcrpCshT#"N")

  *--Refresh the source location setting in the option grid
  = lfOGShowGet('laOGFxFlt[' + Alltrim(Str(lnSrcLoc)) + ',6]')
Endif

*--Enable or Disbale SalesRep.
If Ascan(laOGFxFlt,"CUSTOMER.SALESREP") # 0
  lnSrcLoc   = Asubscript(laOGFxFlt,Ascan(laOGFxFlt,"CUSTOMER.SALESREP"),1)
  laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGFxFlt,1) - ALEN(laOGFxFlt,1)+ lnSrcLoc] =  (lcrpCshT#"N")
  = lfOGShowGet('laOGFxFlt[' + Alltrim(Str(lnSrcLoc)) + ',6]')
Endif

*--End of lfvCashTyp.
*!*************************************************************
*! Name      : lfsrvSty
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 02/05/2006
*! Purpose   : The reset function of the in range function of the account
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfsrvSty()
*!*************************************************************
Function lfsrvSty
Parameters lcParm

If lcParm = 'R'  && Reset code
  llClear = .F.
Endif

*--End of lfsrvSty.
*!*************************************************************
*! Name      : lfFillCurr
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 02/05/2006
*! Purpose   : Fill Currency arrays
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfFillCurr()
*!*************************************************************
Function lfFillCurr

llMultCurr = gfGetMemVar('llMulCurr')
If llMultCurr
  *-- Fill Currency arrays [Begin]
  Dimension laCurrVal[1,1]
  If !Used('SYCCURR')
    llOpenCurr = gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  Else
    Select SYCCURR
    Set Order To CCURRCODE  && To VALIDATE currency code.
  Endif

  Select Distinct CCURRCODE From SYCCURR Order By CCURRCODE Into Array laCurrVal
  Dimension laCurrDesc[ALEN(laCurrVal,1),1]

  For lnI = 1 To Alen(laCurrVal,1)
    = Seek(Alltrim(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = Padr(laCurrVal[lnI,1],3)
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + Alltrim(CCURRDESC)
  Endfor
Endif

*--End of lfFillCurr.
*!*************************************************************
*! Name      : lfGetVarPos
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 02/05/2006
*! Purpose   : Evaluate variable position.
*!*************************************************************
*! Called from : PRG
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : lcVarName
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfItmPos()
*!*************************************************************
Function lfGetVarPos
Parameters lcVarName
Private lnVarPos

lnVarPos = Ascan(laOgObjType,Upper(lcVarName))
If lnVarPos > 0
  lnVarPos = Asubscript(laOgObjType,lnVarPos,1)
Endif
Return lnVarPos

*--End of lfItmPos.
*!*************************************************************
*! Name      : lfvForCurr
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 02/05/2006
*! Purpose   : Print in Foreign currency (Y/N)
*!*************************************************************
*! Called from : PRG
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvForCurr()
*!*************************************************************
Function lfvForCurr
*-- Disable if print in base currency else enable it.
lcRpCurr = gcBaseCurr
laOGObjCnt[lnCurrPos] = llRpForCur
=lfOGShowGet("lcRpCurr")

*--End of lfvForCurr.
*!*************************************************************
*! Name      : lfAddCurr
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 02/05/2006
*! Purpose   : Add Currency filter
*!*************************************************************
*! Called from : PRG
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfAddCurr()
*!*************************************************************
Function lfAddCurr

XFILTER = Alltrim(XFILTER)
If !Empty(XFILTER)
  XFILTER = XFILTER + [ AND ]
Endif
XFILTER = XFILTER + lcCurrFilt

*--End of lfAddCurr.
*!*************************************************************
*! Name      : lfBaseAmt
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 02/05/2006
*! Purpose   : Compute base amount
*!*************************************************************
*! Called from : PRG
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfBaseAmt()
*!*************************************************************
Function lfBaseAmt

Private lnBaseAmt
lnBaseAmt = Amount
*-- if Multi currency and user want to print in base currency and
*-- currency not the base currency.
If llMultCurr And !llRpForCur And (CCURRCODE != gcBaseCurr)
  lnBaseAmt = gfAmntDisp(lnBaseAmt,"O",TranDate)
Endif
Return lnBaseAmt

*--End of lfBaseAmt.
*!***********************************************************************
*! Name      :  lfFilSortb
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 02/05/2006
*! Purpose   : Add a new option of sorting in case of adjustment only.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfFilSortb()
*!*************************************************************
Function lfFilSortb

Dimension laDataDes[3,1],laDataVal[3,1]
*N000682,1 MMT 02/10/2013 Globalization changes[Start]
*!*	laDataDes[1] = "Transaction"
*!*	laDataDes[2] = "Customer"
*!*	laDataDes[3] = "Batch"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laDataDes[1] = LANG_TRANS
laDataDes[1] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TRANS,oAriaApplication.GetHeaderText("LANG_TRANS",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laDataDes[2] = LANG_CUSTOMER
laDataDes[2] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CUSTOMER,oAriaApplication.GetHeaderText("LANG_CUSTOMER",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laDataDes[3] = LANG_BATCH
laDataDes[3] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BATCH,oAriaApplication.GetHeaderText("LANG_BATCH",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 02/10/2013 Globalization changes[End]
laDataVal[1] = "T"
laDataVal[2] = "C"
laDataVal[3] = "B"

*-- End Of lfFilSortb.
*!**************************************************************************
*! Name      : lfSeTSRep
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 02/05/2006
*! Purpose   : Go top in Sales Rep file.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : OpGrdParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfSerSRep()
*!*************************************************************
Function lfSetSRep
Parameters OpGrdParm

Do Case
Case OpGrdParm = 'S'
  Select SalesRep
  Set Order To Tag SalesRep
  Locate
Case OpGrdParm = 'R'
  Select SalesRep
  Set Order To
Endcase
*-- End of lfSetSRep.

*!**************************************************************************
*! Name      : lfSetBatch
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/29/2006
*! Purpose   : Get Bacth Numbers frrom Debit and Credit Files
*!*************************************************************
*! Example   : =lfSetBatch()
*!*************************************************************
Function lfSetBatch
Parameters OpGrdParm

Do Case
Case OpGrdParm = 'S'
  If !Used(lcBatchFile)
    *B608149,1 WAM 07/03/2007 Browse batches from ARHIST file as well
    *SELECT Distinct Batch, Desc, TranDate ;
    FROM Credit ;
    WHERE !EMPTY(Batch) ;
    UNION (SELECT Distinct Batch, Desc, TranDate ;
    FROM Debit ;
    WHERE !EMPTY(Batch)) ;
    INTO CURSOR &lcBatchFile

    Select Distinct Batch, Desc, TranDate ;
      FROM CREDIT ;
      WHERE !Empty(Batch) ;
      UNION (Select Distinct Batch, Desc, TranDate ;
      FROM DEBIT ;
      WHERE !Empty(Batch)) ;
      UNION (Select Distinct Batch, Desc, TranDate ;
      FROM ARHIST ;
      WHERE !Empty(Batch)) ;
      INTO Cursor &lcBatchFile
    *B608149,1 WAM 07/03/2007 (End)
    *B611113,1 MMT 02/10/2016 Add Sort by Batch# in Batch browser[P20150112.0001 - Issue#51][Start]
    Select(lcBatchFile)
    Index On Batch Tag (lcBatchFile)
    *B611113,1 MMT 02/10/2016 Add Sort by Batch# in Batch browser[P20150112.0001 - Issue#51][End]
    Select (lcBatchFile)
    Locate
  Endif
Endcase

*-- End of lfSetSRep.

*!***************************************************************************
*! Name      : lfItmPos
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 02/05/2006
*! Purpose   : to get the position of the fixed filter in OG
*!*************************************************************
*! Called from : OG When Function
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : lcItmInFlt
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfItmPos()
*!*************************************************************
Function lfItmPos
Parameters lcItmInFlt
Private lnItmPos

lnItmPos = Ascan(laOGFxFlt,lcItmInFlt)
If lnItmPos > 0
  lnItmPos = Asubscript(laOGFxFlt,lnItmPos,1)
Endif
Return lnItmPos

*-- End of lfItmPos.
*!*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 02/05/2006
*! Purpose   : To set the report data files and parameters
*!*************************************************************
*! Called from : ARCHADJ.PRG
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfAdjustCRSettings()
*!*************************************************************
Function lfAdjustCRSettings

loOgScroll.lcOGLastForm = "ARCHADJ"

Dimension loOgScroll.laCRTables[1]
loOgScroll.laCRTables[1] = oAriaApplication.Workdir + lcTmpTrn + ".DBF"

Dimension loOgScroll.laCRParams[6,2]
loOgScroll.laCRParams[1,1] = 'ReportName'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loOgScroll.laCRParams[1,2] = Lang_CASH_ADJUSTMENTS_JOURNAL_REPORT
loOgScroll.laCRParams[1,2] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_CASH_ADJUSTMENTS_JOURNAL_REPORT,oAriaApplication.GetHeaderText("Lang_CASH_ADJUSTMENTS_JOURNAL_REPORT",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


loOgScroll.laCRParams[2,1] =  'Layout'
If lcrpDS = 'S'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loOgScroll.laCRParams[2,2] = LANG_SUMMARY_FORMAT
  loOgScroll.laCRParams[2,2] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SUMMARY_FORMAT,oAriaApplication.GetHeaderText("LANG_SUMMARY_FORMAT",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

Else
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loOgScroll.laCRParams[2,2] = LANG_DETAIL_FORMAT
  loOgScroll.laCRParams[2,2] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DETAIL_FORMAT,oAriaApplication.GetHeaderText("LANG_DETAIL_FORMAT",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

Endif

loOgScroll.laCRParams[3,1] = 'SortBy'
Do Case
Case lcRpSRTCD = 'C'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loOgScroll.laCRParams[3,2] = LANG_CUSTOMER
  loOgScroll.laCRParams[3,2] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CUSTOMER,oAriaApplication.GetHeaderText("LANG_CUSTOMER",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]


Case lcRpSRTCD = 'T'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *	loOgScroll.laCRParams[3,2] = LANG_NOOFTRANS
  loOgScroll.laCRParams[3,2] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_NOOFTRANS,oAriaApplication.GetHeaderText("LANG_NOOFTRANS",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]


Case lcRpSRTCD = 'B'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *	loOgScroll.laCRParams[3,2] = LANG_NOOFBATCH
  loOgScroll.laCRParams[3,2] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_NOOFBATCH,oAriaApplication.GetHeaderText("LANG_NOOFBATCH",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]


Case lcRpSRTCD = 'R'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *	loOgScroll.laCRParams[3,2] = LANG_REASONCODE
  loOgScroll.laCRParams[3,2] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_REASONCODE,oAriaApplication.GetHeaderText("LANG_REASONCODE",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]


Endcase

loOgScroll.laCRParams[4,1] =  'OpTitle'
loOgScroll.laCRParams[4,2] = XTITLE

loOgScroll.laCRParams[5,1] = 'DetOrSum'
loOgScroll.laCRParams[5,2] = lcrpDS

loOgScroll.laCRParams[6,1] = 'PrintDecimal'
loOgScroll.laCRParams[6,2] = Iif(llRpDec, 1, 0)

*--End of lfAdjustCRSettings.
*!**************************************************************************************
*! Name      : lfAddField
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 18/01/2006
*! Purpose   : Add fields to the array of file structure.
*!**************************************************************************************
*! Called from :
*!**************************************************************************************
*! Passed Parameters : 1) Array name.
*!  				 : 2) lcFldName -- Field Name
*!                   : 3) lcFldType -- Field Type
*!                   : 		                       1) C = Characters
*!                   : 							   2) Y = Currency
*!                   : 							   3) D = Date
*!                   : 							   4) T = DateTime
*!                   : 							   5) B = Double
*!                   : 							   6) F = Float
*!                   : 							   7) G = General
*!                   : 							   8) I = Integer
*!                   : 							   9) L = Logical
*!                   : 						      10) M = Memo
*!                   : 						      11) N = Numeric
*!                   : 						      12) Q = Varbinary
*!                   : 						      13) V = Varchar and Varchar (Binary)
*!                   : 						      14) W = Blob
*!                   : 4) lnFldLen  -- Field width                        >> Numeric
*!                   : 5) lnFldDec  -- Field Decimal places               >> Numeric
*!                   : 6) ln5       -- Null values allowed                >> Logical
*!                   : 7) ln6       -- Code page translation not allowed  >> Logical
*!                   : 8) ln7       -- Field validation expression        >> Character
*!                   : 9) ln8       -- Field validation text
*!                   :10) ln9       -- Field default value
*!                   :11) ln10      -- Table validation expression
*!                   :12) ln11      -- Table validation text
*!                   :13) ln12      -- Long table name
*!                   :14) ln13      -- Insert trigger expression
*!                   :15) ln14      -- Update trigger expression
*!                   :16) ln15      -- Delete trigger expression
*!                   :17) ln16      -- Table comment
*!                   :18) ln17      -- NextValue for autoincrementing     >> Numeric
*!                   :19) ln18      -- Step for autoincrementing          >> Numeric
*!**************************************************************************************
*! Return      : None
*!**************************************************************************************
*! Example     :
*! =lfAddField("laFileStru", "Status" ,"C",3,0,.F.,.F.,"","","","","","","","","","",0,0)
*!
*! FOR lnCrtTmp = 1 TO 8
*!   lcNumQty = ALLTRIM(STR(lnCrtTmp+8))
*!   =lfAddField("laFileStru", "Qty"  + lcNumQty ,"N",6,0,.F.,.F.,"","","","","","","","","","",0,0)
*! ENDFOR
*!**************************************************************************************
Function lfAddField
Parameters lcStruArry , lcFldName , lcFldType , lnFldLen , lnFldDec ,ln5,ln6,ln7,ln8,ln9,ln10,ln11,ln12,ln13,ln14,ln15,ln16,ln17,ln18

lnFldPos  = Alen(&lcStruArry,1) + Iif(Type('&lcStruArry') = 'L', 0 , 1 )
Dimension &lcStruArry[lnFldPos , 18]
&lcStruArry[lnFldPos , 1]	= lcFldName
&lcStruArry[lnFldPos , 2]	= lcFldType
&lcStruArry[lnFldPos , 3]	= lnFldLen
&lcStruArry[lnFldPos , 4]	= lnFldDec

For lnCount = 5 To 18
  lcParam = 'ln' + Alltrim(Str(lnCount))
  If lnCount = 5 Or lnCount = 6
    &lcStruArry[lnFldPos, lnCount] = &lcParam
  Endif
  If Between(lnCount , 7 , 16)
    &lcStruArry[lnFldPos ,lnCount] = &lcParam
  Endif
  If lnCount = 17 Or lnCount = 18
    &lcStruArry[lnFldPos , lnCount] = &lcParam
  Endif
Endfor

*--End of lfAddField.






*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************
Function lfCheckFilter
Lparameters lnArrayType, lcFilter
Local lcReturn, lnPos
Do Case
Case lnArrayType = 1
  lnPos = Ascan(loOgScroll.laOGFxFlt,lcFilter)
  If lnPos > 0
    lnPos    = Asubscript(loOgScroll.laOGFxFlt,lnPos,1)
    lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]
  Else
    lcReturn = ""
  Endif
Case lnArrayType = 2
  lnPos = Ascan(loOgScroll.laOGHDFlt,lcFilter)
  If lnPos > 0
    lnPos    = Asubscript(loOgScroll.laOGHDFlt,lnPos,1)
    lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]
  Else
    lcReturn = ""
  Endif
Case lnArrayType = 3
  lnPos = Ascan(loOgScroll.laOGvrFlt,lcFilter)
  If lnPos > 0
    lnPos    = Asubscript(loOgScroll.laOGvrFlt,lnPos,1)
    lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]
  Else
    lcReturn = ""
  Endif
Otherwise
  lcReturn = ""
Endcase

Return lcReturn

*!*************************************************************
*! Name      : lfStr2Curs
*! Developer : Ayman Mahmoud Ahmed(AYM)
*! Date      : 02/11/2005
*! Purpose   : Create cursor from string filters
*!*************************************************************
*! Passed Parameters  : File Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
Function lfStr2Curs
Parameters lcString , lccursor , lcFieldsName

Create Cursor (lccursor) (&lcFieldsName. C(6))
Do While At('|',lcString)> 0
  lcFieldsValue  = Substr(lcString,1,At('|',lcString)-1)
  lcString = Substr(lcString,At('|',lcString)+1)
  Select (lccursor)
  Append Blank
  Replace &lcFieldsName. With lcFieldsValue
Enddo
Select (lccursor)
Append Blank
Replace &lcFieldsName. With lcString

*!*************************************************************
*! Name      : lfRefOG
*! Developer : HESHAM ELMASRY(HERS)
*! Date      : 09/01/2009
*! Purpose   : REfresh Option Grid
*!*************************************************************
*! Passed Parameters  : File Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
Function lfRefOG

=CLEARREAD()
* End of lfRefOG()
*:E611972,1 MMT 02/19/2020 Add code to collect date from new GL tables [GL Enhancement][Start]
*!***********************************************************************
*! Name      : lfReconWithGL
*: Developer : Mariam Mazhar(MMT)
*: Date      : 02/19/2020
*! Purpose   : collect data from New GL Tables
*!*************************************************************
FUNCTION lfReconWithGL
lcRpCurr = 'O'
lcRpDbOrCr = 'B'
lcRpCOB ='B' &&Data
lcRpCAB ='B' &&Data
lcRpDS ='D' &&Report
llRpDec = .T.  &&Print Decimals
lcrpCshT = 'B'
lnPeriodRow = ASCAN(loogscroll.laogfxflt, "lcPrdVar")
lnPeriodRow= ASUBSCRIPT(loogscroll.laogfxflt, lnPeriodRow, 1)
lcPeriod = ''
STORE '' TO lcPerFrom ,lcPerTo  
IF !EMPTY(loogscroll.laogfxflt[lnPeriodRow,6])
  SELECT (loogscroll.laogfxflt[lnPeriodRow,6])
  LOCATE 
  IF !EOF()
    lcPeriod =loogscroll.laogfxflt[lnPeriodRow,6]
    LOCATE
    lcPerFrom = STRTRAN(EVALUATE(lcPeriod +'.Keyexp') ,"-","")   && Remove the hyphen "-"
    GOTO bottom
    lcPerTo   = STRTRAN(EVALUATE(lcPeriod +'.Keyexp'),"-","")  && Remove the hyphen "-"
  ENDIF
ENDIF
ldFromDate = {}
ldToDate = {}
IF !EMPTY(lcPerFrom) OR !EMPTY(lcPerTo)
  ldFromDate = IIF(SEEK(lcPerFrom,'FsPrd'),FsPrd.dFsppBgDt,DTOC({}))
  ldToDate = IIF(SEEK(lcPerTo,'FsPrd'),FsPrd.dFsppEnDt,DTOC({}))
ENDIF  
XTITLE   = ''
IF !EMPTY(ldFromDate) AND !EMPTY(ldToDate)
  Ldata    = DTOC(ldFromDate)
  Hdata    = DTOC(ldToDate)
  XTITLE   = "PERIOD FROM: &Ldata To &Hdata "
Endif

  Do Case
    *--CUSTOMER
  Case lcRpSRTCD = 'C'
    XSORT   = 'ACCOUNT+TRANTYPE+TRANCODE+TRAN'
    XTITLE  = XTITLE + " (SORT: CUSTOMER)"

    *--TRANSACTION NUMBER
  Case lcRpSRTCD = 'T'
    XSORT   = 'TRANTYPE+TRANCODE+DTOS(TRANDATE)+TRAN'
    XTITLE  = XTITLE + " (SORT: TRANSACTION)"

    *--BATCH NUMBER
  Case lcRpSRTCD = 'B'
    XSORT   = 'BATCH+TRANTYPE+TRANCODE+TRAN'
    XTITL  = XTITLE + " (SORT: BATCH)"

    *--REASON CODE
  Case lcRpSRTCD = 'G'
    XSORT   = 'CACCTCODE+TRANTYPE+TRANCODE+TRAN'
    XTITLE  = XTITLE + " (SORT: GL Account)"
  Endcase
If llOgFltCh
  lfCreateTmp()
  llDontPrn=.F.
  lfCollectData()
ENDIF  
lfAdjustCRSettingsGL()
IF USED(lcTmpTrn)
  SELECT (lcTmpTrn)
  LOCATE 
  IF EOF()
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF
ENDIF
IF !USED(lcTmpTrn) AND FILE(oAriaApplication.Workdir + lcTmpTrn + ".DBF")
  USE (oAriaApplication.Workdir + lcTmpTrn + ".DBF") IN 0
  SELECT (lcTmpTrn)
  LOCATE 
  IF EOF()
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF
ENDIF
IF USED(lcTmpTrn)
  USE IN (lcTmpTrn)
ENDIF
=gfDispRe()
*!***********************************************************************
*! Name      : lfCollectData 
*: Developer : Mariam Mazhar(MMT)
*: Date      : 02/19/2020
*! Purpose   : collect data from New GL Tables
*!*************************************************************
FUNCTION  lfCollectData 
  *--Define New Variable To Hold The Desc Reason.
  Store '' To lcTranCods 

  *  BEGIN TRANSACTION
  lnGLRow = ASCAN(loogscroll.laogfxflt, "lcRpGlAcc")
  lnGLRow= ASUBSCRIPT(loogscroll.laogfxflt, lnGLRow, 1)
  lcGlCondFileUse= ''
  IF !EMPTY(loogscroll.laogfxflt[lnGLRow,6])
    SELECT (loogscroll.laogfxflt[lnGLRow,6])
    LOCATE 
    IF !EOF()
      lcGlCondFileUse=loogscroll.laogfxflt[lnGLRow,6]
    ENDIF
  ENDIF
 
  IF !USED('GLTRNHD')
	 =gfOpenTable('GLTRNHD','GLTRNHD')
  ENDIF
  IF !USED('GLTRNDT')
	 =gfOpenTable('GLTRNDT','GLTRNDT')
  ENDIF
  SET ORDER TO CRTRAN  IN Credit   && TRANTYPE+TRAN
  SET ORDER TO  DRTRAN  IN DEBIT  && TRANTYPE+TRAN+CINSTALNO

  IF !EMPTY(lcGlCondFileUse) AND USED(lcGlCondFileUse)
    SELECT GLTRNDT
    =gfSetOrder('GLTRNDTAC')
    SELECT (lcGlCondFileUse)
    LOCATE
  SCAN 
    IF gfSeek(&lcGlCondFileUse..cAcctCode,'GLTRNDT')
      SELECT 'GLTRNDT'
      SCAN REST WHILE CACCTCODE =&lcGlCondFileUse..cAcctCode FOR  ;
                    IIF(!EMPTY(ldFromDate) AND !EMPTY(ldToDate),BETWEEN(tran_date , ldFromDate , ldToDate),.T.) AND;
                     ((INLIST(CATG_KEY ,'009','010','002') AND EMPTY(CGLREF)) OR (!EMPTY(CGLREF) AND TRAN_TYPE ="KO" AND CATG_KEY ='001')) .AND. NAMOUNT <> 0 
            
        =gfSeek(GLTRNDT.CGLBATCHNO+GLTRNDT.CGLTRANNO,'GLTRNHD')
        lcTrnTyp = ''
        lcFileSrc = ''
        DO CASE 
          CASE GLTRNDT.TRAN_TYPE ="CR"
            lcTrnTyp ='4'
            lcFileSrc = 'CREDIT'
          CASE GLTRNDT.TRAN_TYPE ="CA"
            lcTrnTyp ='57'
            lcFileSrc = 'CREDIT'
          CASE GLTRNDT.TRAN_TYPE ="DA"
   	     lcTrnTyp ='23'          
            lcFileSrc = 'DEBIT'   
          CASE GLTRNDT.TRAN_TYPE ="KO"
            IF GLTRNDT.nglAmount > 0
              lcTrnTyp ='3'              
              lcFileSrc = 'DEBIT'    
            ENDIF          
          OTHERWISE 
            lcTrnTyp = ''
        ENDCASE 
        IF EMPTY(lcTrnTyp)
          LOOP 
        ENDIF
        =gfSeek('M'+SUBSTR(GLTRNHD.CCONT_ID,1,5),'CUSTOMER','CUSTOMER')
        IF lcrpFAcct='F' AND EMPTY(Customer.cFacCode)
          LOOP  
        ENDIF
        IF lcrpFAcct='N' AND !EMPTY(Customer.cFacCode)
          LOOP  
        ENDIF
        IF !EMPTY(lcFileSrc)
        SELECT (lcFileSrc)
        llTransFound = .F.
        IF LEN(lcTrnTyp)>1
          IF gfSeek (SUBSTR(lcTrnTyp,1,1)+SUBSTR(GLTRNDT.CTRAN_NO,1,6)) or gfSeek (SUBSTR(lcTrnTyp,2,1)+SUBSTR(GLTRNDT.CTRAN_NO,1,6)) 
            llTransFound = .T.
            SCATTER MEMO MEMVAR 
            m.Amount = gltrndt.nglamount
            IF UPPER(lcFileSrc)=='CREDIT'
              m.trancode = m.ccreditcod 
            ENDIF  
            m.cAcctCode = GLTRNDT.cAcctCode 
            m.Amount = lfBaseAmt()
            m.CustName =LEFT(CUSTOMER.BTNAME ,20) 
            If m.TRANTYPE = '2' Or m.TRANTYPE = '3'
              lcDesc = GFCODDES(m.TranCode,'TRANCODE')
              lcDesc = Iif( Alltrim(lcDesc) = "N/A" , "" , lcDesc)
              m.Reason =lcDesc
   		    Else
		        lcDesc2 = GFCODDES(m.TranCode,'CCREDITCOD')
    			  lcDesc2 = Iif( Alltrim(lcDesc2) = "N/A" , "" , lcDesc2)
		         m.Reason = lcDesc2
		    Endif
            INSERT INTO (lcTmpTrn) FROM MEMVAR 
          ENDIF
        ELSE
          IF gfSeek(lcTrnTyp+SUBSTR(GLTRNDT.CTRAN_NO,1,6)) 
            llTransFound = .T.
		        SCATTER MEMO MEMVAR 
            m.Amount = gltrndt.nglamount
            IF UPPER(lcFileSrc)=='CREDIT'
              m.trancode = m.ccreditcod 
            ENDIF 
            m.cAcctCode = GLTRNDT.cAcctCode 
            m.Amount = lfBaseAmt()
            m.CustName =LEFT(CUSTOMER.BTNAME ,20) 
            If m.TRANTYPE = '2' Or m.TRANTYPE = '3'
              lcDesc = GFCODDES(m.TranCode,'TRANCODE')
              lcDesc = Iif( Alltrim(lcDesc) = "N/A" , "" , lcDesc)
              m.Reason =lcDesc
		    Else
		      lcDesc2 = GFCODDES(m.TranCode,'CCREDITCOD')
			  lcDesc2 = Iif( Alltrim(lcDesc2) = "N/A" , "" , lcDesc2)
		       m.Reason = lcDesc2
		    ENDIF
            INSERT INTO (lcTmpTrn) FROM MEMVAR 
          ENDIF  
        ENDIF 
        ENDIF
        SELECT ARHIST 
        =gfSetOrder('ARHISTT')   && ACCOUNT+TRAN+CINSTALNO)
        =gfSeek(SUBSTR(GLTRNHD.CCONT_ID,1,5)+SUBSTR(GLTRNDT.CTRAN_NO,1,6))
        SCAN REST WHILE  ACCOUNT+TRAN+CINSTALNO = SUBSTR(GLTRNHD.CCONT_ID,1,5)+SUBSTR(GLTRNDT.CTRAN_NO,1,6) FOR TRANTYPE $ lcTrnTyp
    		  SCATTER MEMO MEMVAR 
          m.cAcctCode = GLTRNDT.cAcctCode 
          m.Amount = lfBaseAmt()
          m.Amount = gltrndt.nglamount
          m.CustName =LEFT(CUSTOMER.BTNAME ,20) 
          If m.TRANTYPE = '2' Or m.TRANTYPE = '3'
            lcDesc = GFCODDES(m.TranCode,'TRANCODE')
            lcDesc = Iif( Alltrim(lcDesc) = "N/A" , "" , lcDesc)
            m.Reason =lcDesc
    	    Else
    	      lcDesc2 = GFCODDES(m.TranCode,'CCREDITCOD')
    		  lcDesc2 = Iif( Alltrim(lcDesc2) = "N/A" , "" , lcDesc2)
    	       m.Reason = lcDesc2
    	    ENDIF
          INSERT INTO (lcTmpTrn) FROM MEMVAR 
        ENDSCAN   
      ENDSCAN
    ENDIF
  ENDSCAN
ELSE

   SELECT GLTRNDT
  =gfSQLRUN("Select * FROM GLTRNDT WHERE NAMOUNT <> 0 and "+;
                  " (((CATG_KEY IN ('009','010','002') AND CGLREF='')) OR (CGLREF <> '' AND TRAN_TYPE ='KO' AND CATG_KEY ='001')) "+;
                 IIF(!EMPTY(ldFromDate) AND !EMPTY(ldToDate)," AND tran_date BETWEEN ?ldFromDate AND ?ldToDate ",''),'GLTRNDT')
  SELECT GLTRNDT
  SCAN FOR (EMPTY(CGLREF) OR (!EMPTY(CGLREF) AND TRAN_TYPE ="KO"))
         =gfSeek(GLTRNDT.CGLBATCHNO+GLTRNDT.CGLTRANNO,'GLTRNHD')
        lcTrnTyp = ''
        lcFileSrc = ''
        DO CASE 
          CASE GLTRNDT.TRAN_TYPE ="CR"
            lcTrnTyp ='4'
            lcFileSrc = 'CREDIT'
          CASE GLTRNDT.TRAN_TYPE ="CA"
            lcTrnTyp ='57'
            lcFileSrc = 'CREDIT'
          CASE GLTRNDT.TRAN_TYPE ="DA"
          lcTrnTyp ='23'          
            lcFileSrc = 'DEBIT'  
          CASE GLTRNDT.TRAN_TYPE ="KO"
             IF GLTRNDT.nglAmount > 0
                lcTrnTyp ='3'              
                lcFileSrc = 'DEBIT'    
             ENDIF          
         OTHERWISE 
            lcTrnTyp = ''
        ENDCASE 
        IF EMPTY(lcTrnTyp)
          LOOP 
        ENDIF
        =gfSeek('M'+SUBSTR(GLTRNHD.CCONT_ID,1,5),'CUSTOMER','CUSTOMER')
        IF lcrpFAcct='F' AND EMPTY(Customer.cFacCode)
          LOOP  
        ENDIF
        IF lcrpFAcct='N' AND !EMPTY(Customer.cFacCode)
          LOOP  
        ENDIF
        IF !EMPTY(lcFileSrc)
        SELECT (lcFileSrc)
        llTransFound = .F.
        IF LEN(lcTrnTyp)>1
          IF gfSeek (SUBSTR(lcTrnTyp,1,1)+SUBSTR(GLTRNDT.CTRAN_NO,1,6)) or gfSeek (SUBSTR(lcTrnTyp,2,1)+SUBSTR(GLTRNDT.CTRAN_NO,1,6)) 
            llTransFound = .T.
            SCATTER MEMO MEMVAR 
            m.Amount = gltrndt.nglamount
            IF UPPER(lcFileSrc)=='CREDIT'
              m.trancode = m.ccreditcod 
            ENDIF  
            m.cAcctCode = GLTRNDT.cAcctCode 
            m.Amount = lfBaseAmt()
            m.CustName =LEFT(CUSTOMER.BTNAME ,20) 
            If m.TRANTYPE = '2' Or m.TRANTYPE = '3'
              lcDesc = GFCODDES(m.TranCode,'TRANCODE')
              lcDesc = Iif( Alltrim(lcDesc) = "N/A" , "" , lcDesc)
              m.Reason =lcDesc
            Else
              lcDesc2 = GFCODDES(m.TranCode,'CCREDITCOD')
              lcDesc2 = Iif( Alltrim(lcDesc2) = "N/A" , "" , lcDesc2)
              m.Reason = lcDesc2
            Endif
            INSERT INTO (lcTmpTrn) FROM MEMVAR 
          ENDIF
        ELSE
          IF gfSeek(lcTrnTyp+SUBSTR(GLTRNDT.CTRAN_NO,1,6)) 
            llTransFound = .T.
            SCATTER MEMO MEMVAR 
            m.Amount = gltrndt.nglamount
            IF UPPER(lcFileSrc)=='CREDIT'
              m.trancode = m.ccreditcod 
            ENDIF 
            m.cAcctCode = GLTRNDT.cAcctCode 
            m.Amount = lfBaseAmt()
            m.CustName =LEFT(CUSTOMER.BTNAME ,20) 
            If m.TRANTYPE = '2' Or m.TRANTYPE = '3'
              lcDesc = GFCODDES(m.TranCode,'TRANCODE')
              lcDesc = Iif( Alltrim(lcDesc) = "N/A" , "" , lcDesc)
              m.Reason =lcDesc
        Else
          lcDesc2 = GFCODDES(m.TranCode,'CCREDITCOD')
        lcDesc2 = Iif( Alltrim(lcDesc2) = "N/A" , "" , lcDesc2)
           m.Reason = lcDesc2
        ENDIF
            INSERT INTO (lcTmpTrn) FROM MEMVAR 
          ENDIF  
        ENDIF 
        ENDIF
        SELECT ARHIST 
        =gfSetOrder('ARHISTT')   && ACCOUNT+TRAN+CINSTALNO)
        =gfSeek(SUBSTR(GLTRNHD.CCONT_ID,1,5)+SUBSTR(GLTRNDT.CTRAN_NO,1,6))
        SCAN REST WHILE  ACCOUNT+TRAN+CINSTALNO = SUBSTR(GLTRNHD.CCONT_ID,1,5)+SUBSTR(GLTRNDT.CTRAN_NO,1,6) FOR TRANTYPE $ lcTrnTyp
          SCATTER MEMO MEMVAR 
          m.Amount = gltrndt.nglamount
          m.cAcctCode = GLTRNDT.cAcctCode 
          m.Amount = lfBaseAmt()
          m.CustName =LEFT(CUSTOMER.BTNAME ,20) 
          If m.TRANTYPE = '2' Or m.TRANTYPE = '3'
            lcDesc = GFCODDES(m.TranCode,'TRANCODE')
            lcDesc = Iif( Alltrim(lcDesc) = "N/A" , "" , lcDesc)
            m.Reason =lcDesc
          Else
            lcDesc2 = GFCODDES(m.TranCode,'CCREDITCOD')
          lcDesc2 = Iif( Alltrim(lcDesc2) = "N/A" , "" , lcDesc2)
             m.Reason = lcDesc2
          ENDIF
          INSERT INTO (lcTmpTrn) FROM MEMVAR 
        ENDSCAN   
      ENDSCAN
ENDIF
*!***********************************************************************
*! Name      : lfCreateTmp
*: Developer : Mariam Mazhar(MMT)
*: Date      : 02/19/2020
*! Purpose   : Create Temp File
*!*************************************************************
FUNCTION lfCreateTmp
SELECT ARHIST
=Afields(laFileStru)
=lfAddField("laFileStru", "Reason" ,"C",24,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "CustName" ,"C",20,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "cAcctCode" ,"C",24,0,.T.,.F.,"","","","","","","","","","",0,0)


Dimension laIndx[1,2]
laIndx[1,1] = XSORT
laIndx[1,2] = (lcTmpTrn)

=gfCrtTmp(lcTmpTrn,@laFileStru,@laIndx,lcTmpTrn,.F.)


*!*************************************************************
*! Name      : lfAdjustCRSettingsGL
*: Developer : Mariam Mazhar(MMT)
*: Date      : 02/19/2020
*! Purpose   : To set the report data files and parameters
*!*************************************************************
*! Called from : ARCHADJ.PRG
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfAdjustCRSettings()
*!*************************************************************
FUNCTION lfAdjustCRSettingsGL

loOgScroll.lcOGLastForm = "ARCHADJ"

Dimension loOgScroll.laCRTables[1]
loOgScroll.laCRTables[1] = oAriaApplication.Workdir + lcTmpTrn + ".DBF"
Dimension loOgScroll.laCRParams[6,2]
loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_CASH_ADJUSTMENTS_JOURNAL_REPORT,oAriaApplication.GetHeaderText("Lang_CASH_ADJUSTMENTS_JOURNAL_REPORT",AHEADERFILE))
loOgScroll.laCRParams[2,1] =  'Layout'
If lcrpDS = 'S'
  loOgScroll.laCRParams[2,2] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SUMMARY_FORMAT,oAriaApplication.GetHeaderText("LANG_SUMMARY_FORMAT",AHEADERFILE))
Else
  loOgScroll.laCRParams[2,2] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DETAIL_FORMAT,oAriaApplication.GetHeaderText("LANG_DETAIL_FORMAT",AHEADERFILE))
Endif

loOgScroll.laCRParams[3,1] = 'SortBy'
Do Case
Case lcRpSRTCD = 'C'
  loOgScroll.laCRParams[3,2] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CUSTOMER,oAriaApplication.GetHeaderText("LANG_CUSTOMER",AHEADERFILE))
Case lcRpSRTCD = 'T'
  loOgScroll.laCRParams[3,2] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_NOOFTRANS,oAriaApplication.GetHeaderText("LANG_NOOFTRANS",AHEADERFILE))
Case lcRpSRTCD = 'B'
  loOgScroll.laCRParams[3,2] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_NOOFBATCH,oAriaApplication.GetHeaderText("LANG_NOOFBATCH",AHEADERFILE))
Case lcRpSRTCD = 'G'
  loOgScroll.laCRParams[3,2] = Iif(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Account_Code,oAriaApplication.GetHeaderText("Lang_Account_Code",AHEADERFILE))
ENDCASE 

loOgScroll.laCRParams[4,1] =  'OpTitle'
loOgScroll.laCRParams[4,2] = XTITLE   

loOgScroll.laCRParams[5,1] = 'DetOrSum'
loOgScroll.laCRParams[5,2] = lcrpDS

loOgScroll.laCRParams[6,1] = 'PrintDecimal'
loOgScroll.laCRParams[6,2] = Iif(llRpDec, 1, 0)
*!***********************************************************************
*! Name      : lfvReconcile
*: Developer : Mariam Mazhar(MMT)
*: Date      : 02/19/2020
*! Purpose   : Validate 'Reconcile with GL' option
*!*************************************************************
FUNCTION lfvReconcile

 
lcSessValue = lcSession
IF llPrintMod <> llOldMode
  llOldMode= llPrintMod 
  

  STORE '' TO loogScroll.aRepVrFlt, loogScroll.laOGFxFlt,loogScroll.laOGVRFlt,loogScroll.laOGHDFlt
  loogScroll.lcOGPrgName = IIF(llPrintMod ,'ARCADJR','ARCHADJ')
  lnRemoteRslt = loogScroll.SQLExecute("SYDREPRT","Select * from SYDREPRT where cRep_ID='"+PADR(ALLTRIM(loogScroll.lcOGPrgName),loogScroll.lnRepIDLen)+"'",;
                                    '',"SYDREPRT","",oAriaApplication.cAria4SysFiles,;
                                    3,"",loogScroll.Parent.DataSessionID)
  IF (lnRemoteRslt < 1) OR EOF("SYDREPRT")
    RETURN .F.
  ENDIF

  SELECT SYDREPRT
  loogScroll.lnRepIDLen = LEN(cRep_ID)
  WITH loogScroll
    STORE PADR(ALLTRIM(.lcOGPrgName),.lnRepIDLen) TO .lcOGPrgName, .lcOGRepID, .lcOGManRep
    IF (.lcOGPrgName != .lcOGRepID)
      USE IN SYDREPRT
    lnRemoteRslt = This.SQLExecute("SYDREPRT","Select * from SYDREPRT where  cRep_ID='"+PADR(ALLTRIM(.lcOGPrgName),.lnRepIDLen)+"'",;
                                       '',"SYDREPRT","",oAriaApplication.cAria4SysFiles,;
                                        3,"",loogScroll.Parent.DataSessionID)
      IF (lnRemoteRslt < 1) OR EOF("SYDREPRT")
      .lcOGPrgName =  .lcOGRepID
        lnRemoteRslt = This.SQLExecute("SYDREPRT","Select * from SYDREPRT where  'cRep_ID='"+PADR(ALLTRIM(This.lcOGPrgName),This.lnRepIDLen)+"'",;
                                    '',"SYDREPRT","",oAriaApplication.cAria4SysFiles,;
                                    3,"",loogScroll.Parent.DataSessionID)
      ENDIF
    ENDIF
  ENDWITH
  SELECT SYDREPRT
  LOCATE
  loogScroll.gcAct_Appl = cApp_ID
  IF !llFromWhen
    lcSession = lcSessValue
    loogScroll.InitRepHeader()
  ENDIF
  lcSession = lcSessValue
  loOGScroll.GetRepVar()
  lcSession = lcSessValue
  WITH loOGScroll.OuterContainer.InnerContainer
    DO WHILE .ControlCount > 0 AND !('PRINTMOD' $ UPPER(.Controls(.ControlCount).Name))
     .RemoveObject(.Controls(.ControlCount).Name)
    ENDDO
  ENDWITH
  lcSession = lcSessValue
  loOGScroll.defineobjects (.T.)
  lcSession = lcSessValue
  =loOGScroll.RefreshScroll()
  loogScroll.lcOGPrgName = 'ARCHADJ'
  loogScroll.lcOGManRep= 'ARCHADJ'
  IF llPrintMod 
    lcRpForm = "ARCHADJ"
    loOgScroll.lcOGLastForm = "ARCHADJ"
  
    IF !USED('FSPRD')
	  =gfOpenTable('FSPRD','COMFYRPRDI','SH')   && CFISFYEAR+CFSPPRDID)
	ENDIF
	lcAcMask   = SPACE(0)         && Account mask
	lnAcLen    = 0                && Account Length
	lcGlFld = SPACE(0)         && Chart of account browse fields.
	lcAcntFld  = SPACE(0)         && Account field that will be validated.
	lcAcntDesF = SPACE(0)         && Account Description.
    llNoThing  = lfSetGLMsk()
    lfFilSortb()

  ELSE
    lcRpForm = "ARCHADJ"
    loOgScroll.lcOGLastForm = "ARCHADJ"

  ENDIF
  lcRpSRTCD = 'T'
  loOgScroll.lcRpFrxMod = "Graphics"
  lcRpFrxMod = loOgScroll.lcRpFrxMod
  loogscroll.Parent.cntVariables.lcRepMode = loOgScroll.lcRpFrxMod
  loogscroll.Parent.ogtoolbar.cntPrint.cboPrintOutput.displayValue   = loogscroll.Parent.cntVariables.lcRepMode
  loogscroll.Parent.ogtoolbar.cntPrint.cboPrintOutput.ENABLED=.F.  

ENDIF

*!*************************************************************
*! Name      : lfSetGLMsk
*! Developer : Mariam Mazhar(MMT)
*! Date      : 02/19/2020
*! Purpose   : Set GL Mask
*!*************************************************************
FUNCTION lfSetGLMsk
PRIVATE lnAlias

lnAlias    = SELECT(0)
llGlLink   = (ALLTRIM(gfGetMemVar('M_LINK_GL',oAriaApplication.ActiveCompanyID)) = 'Y')
lcLinkWith = (ALLTRIM(gfGetMemVar('M_GL_VERS',oAriaApplication.ActiveCompanyID)))
IF llGlLink
  IF lcLinkWith $ "AO"
    USE (oAriaApplication.SysPath + "SYCCOMP") IN 0 AGAIN ALIAS CompFile ORDER CCOMP_ID
    llNoThing  = SEEK(oAriaApplication.ActiveCompanyID, "CompFile")
    lcPthToUse = oAriaApplication.DataDir
    IF !EMPTY(CompFile.cCompPrnt)
      lcPrntComp = CompFile.cCompPrnt
      llNoThing  = SEEK(lcPrntComp, "CompFile")
      lcPthToUse = ALLTRIM(CompFile.cCom_DDir)
    ENDIF
    USE IN CompFile
    USE (lcPthToUse + "ACCOD") IN 0 AGAIN ALIAS CodeStr ORDER AccSegNo
    SELECT CodeStr
    GOTO TOP
    lcRep     = IIF(lcLinkWith = "A", "9", "X")
    lcAcMask  = "X" + SUBSTR(STRTRAN(ALLTRIM(cAcsMask),"#",lcRep),2)
    USE IN CodeStr
    IF lcLinkWith = "A" AND !USED('lcLinkChar')
      USE (lcPthToUse + "GLACCHAR") IN 0 ORDER ACCTCODE AGAIN ALIAS lcLinkChar
    ENDIF
    lnAcLen    = LEN(ALLTRIM(lcAcMask))
 
    lcGlFld    = "cAcctCode: 25 :H='"+;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Account_Code,oAriaApplication.GetHeaderText("Lang_Account_Code",AHEADERFILE))+;
    "',"+"cAccNlDes:65:H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Description,oAriaApplication.GetHeaderText("Lang_Description",AHEADERFILE))+"'"
 
    lcAcntFld  = "cAcctCode"
    lcAcntDesF = "cAccNlDes"
  ELSE
    lcLinkComp = ALLTRIM(gfGetMemVar('M_GL_CO'  , oAriaApplication.ActiveCompanyID))
    lcSBTGLDir = ALLTRIM(gfGetMemVar('M_SYS_DIR', oAriaApplication.ActiveCompanyID))
    lcAcntChrt = lcSBTGLDir + "\GLDATA\GLACNT" + lcLinkComp + ".DBF"
    lcAcntStrc = lcSBTGLDir + "\GLDATA\GLSTRU" + lcLinkComp + ".DBF"
    lcAcMask   = SPACE(0)
    USE (lcAcntStrc) IN 0 AGAIN ALIAS AcntStrc ORDER SegID
    SELECT AcntStrc
    SCAN FOR SegLen > 0
      lcAcMask = lcAcMask + IIF(EMPTY(lcAcMask),"","-") + ALLTRIM(SegMask)
    ENDSCAN
    USE IN AcntStrc
    IF !USED("lcLinkChar")
      USE (lcAcntChrt) IN 0 AGAIN ALIAS lcLinkChar ORDER GlAcnt
    ENDIF
    lnAcLen    = LEN(ALLTRIM(lcAcMask))
    lcGlFld = "glAcnt: 25 :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Account_Code,oAriaApplication.GetHeaderText("Lang_Account_Code",AHEADERFILE))+;
              "',"+"glDesc:53:H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Description,oAriaApplication.GetHeaderText("Lang_Description",AHEADERFILE))+"'"


    lcAcntFld  = "glAcnt"
    lcAcntDesF = "glDesc"
  ENDIF
ENDIF
SELECT (lnAlias)
*:E611972,1 MMT 02/19/2020 Add code to collect date from new GL tables [GL Enhancement][End]