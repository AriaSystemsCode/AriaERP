*:************************************************************************
*: Program file  : ARACTSUM.Prg
*: Program desc. : Activity Summary Report
*: System        : Aria4xp
*: Module        : AR
*: Developer     : AYMAN MAHMOUD AHMED (AYM)
*: Date          : 09/27/2006
*: Reference     : T20060908.0019 (N000555)
*:************************************************************************
*: Calls :
*:    Procedures : CALCULATE()
*:    Functions  : lfwOGWhen(), lfvDateRng(), lfvpbDateOk(), lfvSelBy()
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO ARACTSUM
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
*:***************************************************************************
*: Modifications :
*: B608493,1 WAM 03/26/2008 get returns when RM modules is installed T20080317.0011
*: B608508,1 WAM 04/08/2008 Credit Adjustment created from key off screen are shown as ending balance T20080402.0013
*: B609632,1 MMT 06/27/2011 Activity Summary rep. Apply the Factored/Non Factored filter on Returns[T20110428.0020]
*: B609751,1 MMT 11/27/2011 Apply Factored Filter on Credit/Debit files.[T20110428.0020]
*: B609751,1 MMT 11/27/2011 Fix bug of not including return from ARHIST file[T20110728.0001]
*: B609828,1 SAB 02/12/2012 Some records are not included in Activity Summary report if transtype = '3' [T20120118.0006]
*: B610793,1 MMT 08/05/2014 Activity Summary report does not show accounts has activity(Open Balance)[T20140709.0093]
*: B610889,1 MMT 10/22/2014 Activity summary report does not export end balance column to EXCEL[T20141021.0021]
*:***************************************************************************

*-- If select by month and month value not entered, or select by date and
*-- date values not entered, so there is no records to display.
#INCLUDE R:\ARIA4XP\REPORTS\AR\aractsum.H

IF  lcrpSelBy = 'D'
  lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVHDR.INVDATE'),1)
  LDATE = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,ATC('|',laOGFxFlt[lnDatePos,6])-1))
  HDATE = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],  ATC('|',laOGFxFlt[lnDatePos,6])+1))
	IF EMPTY(lDate) .OR. EMPTY(hDate)
*!*			*-- Message 'There are no records to display...!'
*!*			=gfModalGen('TRM00052B00000','DIALOG')
	*-- Message < You have to type a full month range. >
  *-- Buttons <                  OK                  >
   *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM000000B00000","DIALOG",'','',LANG_FULLDATERANGE)
=gfModalGen("TRM000000B00000","DIALOG",'','',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_FULLDATERANGE,oAriaApplication.GetHeaderText("LANG_FULLDATERANGE",AHEADERFILE)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

		
		
		RETURN
	ELSE
      ENDING = LDATE - 1

	ENDIF
ENDIF

IF lcrpSelBy = 'M'
  IF EMPTY(lcMonth)
*!*	    *-- Message 'There are no records to display...!'
*!*	    =gfModalGen('TRM00052B00000','DIALOG')
    *-- Message < You have to type a full month range. >
  *-- Buttons <                  OK                  >
   *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM000000B00000","DIALOG",'','',LANG_TYPEMONTH)
=gfModalGen("TRM000000B00000","DIALOG",'','',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_TYPEMONTH,oAriaApplication.GetHeaderText("LANG_TYPEMONTH",AHEADERFILE)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

     RETURN
  ELSE
    IF lcrpSelBy = 'M' .AND. EMPTY(lcYear)
      lcYear=STR(YEAR(DATE()),4)
    ENDIF
    LcX    = lcMONTH+'/'+'01'+'/'+PADL(lcYEAR,4,'0')
		LDATE  = CTOD(LcX)
		NDATE  = LDATE+32
		HDATE  = NDATE-(DAY(NDATE))
		ENDING = LDATE-(DAY(LDATE))
  ENDIF
ENDIF

IF llOgFltCh
 llDonprnt=.F.
****INIT. TOTALS***
STORE 0.00 TO XTOTOPEN,XTOTSALES,XTOTPAY,XTOTRET, XTOTFALL,XTOTDALL, ;
              XTOTMALL,XTOTBAL

STORE '' TO lcCurrency
IF llMultCurr
  lcCurrency= lfCheckFilter(1, 'INVHDR.CCURRCODE')
  llCurFltr   = !EMPTY(lcCurrency) AND USED(lcCurrency) AND RECCOUNT(lcCurrency) > 0
  IF llCurFltr
    SELECT (lcCurrency)
    INDEX ON CCURRCODE TAG (lcCurrency)
  ELSE
    IF TYPE("lcCurrency") = "C" AND USED(lcCurrency)
      USE IN (lcCurrency)
    ENDIF
    lcCurrency= ''
  ENDIF
ENDIF

lcseek=" .T. "
lcCurFltr= lfCheckFilter(1, 'CUSTOMER.ACCOUNT')
llCurFltr   = !EMPTY(lcCurFltr) AND USED(lcCurFltr) AND RECCOUNT(lcCurFltr) > 0
IF llCurFltr
  SELECT (lcCurFltr)
  INDEX ON ACCOUNT TAG (lcCurFltr)
  lcseek=lcseek+" AND SEEK(CUSTOMER.ACCOUNT ,'"+lcCurFltr+"')"
ELSE
  IF TYPE("lcCurFltr") = "C" AND USED(lcCurFltr)
    USE IN (lcCurFltr)
  ENDIF
  lcCurFltr= ''
ENDIF




*-- Array to get the Division long name
DIMENSION laType[1,2]
lcType = SPACE(1)
laType[1,1] = 'ALLOW_TYPE'
laType[1,2] = 'lcType'

XTIME  = SUBSTR(TIME(),1,5)

llFound =.F.             && Indicate if there are records to print or not.


*N000682,1 MMT 02/10/2013 Globalization changes[Start]
*XTITLE='PERIOD :'+ DTOC(LDATE) + ' --- ' + DTOC(HDATE)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*XTITLE=LANG_PERIOD+ DTOC(LDATE) + ' --- ' + DTOC(HDATE)
XTITLE=IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PERIOD,oAriaApplication.GetHeaderText("LANG_PERIOD",AHEADERFILE))+ DTOC(LDATE) + ' --- ' + DTOC(HDATE)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 MMT 02/10/2013 Globalization changes[End]


 lcWorkfile =loOgScroll.gfTempName()

 =lfBuildTmp()

 =gfOpenTABLE(oAriaApplication.DATADIR+'CUSTOMER',oAriaApplication.DATADIR+'CUSTOMER','SH')
 =gfOpenTABLE(oAriaApplication.DATADIR+'DEBIT',oAriaApplication.DATADIR+'DEBIT','SH')
 =gfOpenTABLE(oAriaApplication.DATADIR+'CREDIT',oAriaApplication.DATADIR+'CREDIT','SH')
 =gfOpenTABLE(oAriaApplication.DATADIR+'ARHIST',oAriaApplication.DATADIR+'ARHISTHT','SH')
 =gfOpenTABLE(oAriaApplication.DATADIR+'INVHDR',oAriaApplication.DATADIR+'INVHDRA','SH')

*B608493,1 WAM 03/26/2008 get returns when RM modules is installed
* IF 'MA' $ oAriaApplication.CompanyInstalledModules
 IF 'RM' $ oAriaApplication.CompanyInstalledModules
*B608493,1 WAM 03/26/2008 (End)
   =gfOpenTABLE(oAriaApplication.DATADIR+'RETHDR',oAriaApplication.DATADIR+'RETHDRA','SH')
 ENDIF
 =gfOpenTABLE(oAriaApplication.DATADIR+'SYCCURR',oAriaApplication.DATADIR+'CCURRCODE','SH')

SELE CUSTOMER
=GFSEEK('M')
SCAN REST WHILE  TYPE='M' FOR &lcseek
  STORE 0 TO XOPENBAL,XSALES,XPAY,XRET,XFALL,XDALL,XMALL,XBAL,XOPENDB,XOPENCR
  XACCOUNT=ACCOUNT
  *-- PROCEDURE TO CALCULATE OPENBALANCE AND CALCULATE PAYMENT AND ALLOWANCE
  =CALCULATE()
  *-----------O P E N  B A L .-----------
  XOPENBAL = XOPENDB + XOPENCR
  *-------------S A L E S----------------
  SELE INVHDR
  =GFSEEK(XACCOUNT)
  IF llRpVodInv      && Variable if the user wanted to Include voided invoices or not.
    XSALES = 0

    *B608508,1 WAM 04/08/2008 Consider empty currency as base currency
    *SCAN REST WHILE ACCOUNT=XACCOUNT ;
              FOR  IIF(lcRpFact ='F' , !EMPTY(InvHdr.cFacCode) ,;
                   IIF(lcRpFact ='N' , EMPTY(InvHdr.cFacCode),.T.));
                  .AND.   IIF(!EMPTY(lcCurrency),SEEK(INVHDR.CCURRCODE ,lcCurrency),INVHDR.CCURRCODE=ALLTRIM(gcBaseCurr))
    SCAN REST WHILE ACCOUNT=XACCOUNT ;
              FOR  IIF(lcRpFact ='F' , !EMPTY(InvHdr.cFacCode) ,;
                   IIF(lcRpFact ='N' , EMPTY(InvHdr.cFacCode),.T.));
                  .AND.   IIF(!EMPTY(lcCurrency),SEEK(INVHDR.CCURRCODE ,lcCurrency),EMPTY(INVHDR.CCURRCODE) OR INVHDR.CCURRCODE=ALLTRIM(gcBaseCurr))
    *B608508,1 WAM 04/08/2008 (End)

      DO CASE
        CASE STATUS <> 'V' AND BETWEEN(InvDate,LDATE,HDATE)
          XSALES = XSALES + IIF(llMultCurr,gfAmntDisp(TotalChg, lcRpCurr , ldRpExDate , lcRpTmpNam),TotalChg)

        CASE STATUS = 'V'  AND BETWEEN(InvDate,LDATE,HDATE)  AND !BETWEEN(vDate,LDATE,HDATE)
          XSALES = XSALES + IIF(llMultCurr,gfAmntDisp(vTotalChg, lcRpCurr , ldRpExDate , lcRpTmpNam),vTotalChg)

        CASE STATUS = 'V'  AND !BETWEEN(InvDate,LDATE,HDATE) AND BETWEEN(vDate,LDATE,HDATE)
          XSALES = XSALES - IIF(llMultCurr,gfAmntDisp(vTotalChg, lcRpCurr , ldRpExDate , lcRpTmpNam),vTotalChg)

      ENDCASE
    ENDSCAN
  ELSE
    *B608508,1 WAM 04/08/2008 Consider empty currency as base currency
    *SUM REST TOTALCHG TO XSALES WHILE (XACCOUNT=ACCOUNT) ;
    FOR BETWEEN(INVDATE,LDATE,HDATE) AND ;
       IIF(lcRpFact = 'F' , !EMPTY(InvHdr.cFacCode) ,;
           IIF(lcRpFact = 'N' , EMPTY(InvHdr.cFacCode),.T.));
          .AND. IIF(!EMPTY(lcCurrency),SEEK(INVHDR.CCURRCODE ,lcCurrency),INVHDR.CCURRCODE=ALLTRIM(gcBaseCurr))
    SUM REST TOTALCHG TO XSALES WHILE (XACCOUNT=ACCOUNT) ;
    FOR BETWEEN(INVDATE,LDATE,HDATE) AND ;
       IIF(lcRpFact = 'F' , !EMPTY(InvHdr.cFacCode) ,;
           IIF(lcRpFact = 'N' , EMPTY(InvHdr.cFacCode),.T.));
          .AND. IIF(!EMPTY(lcCurrency),SEEK(INVHDR.CCURRCODE ,lcCurrency),EMPTY(INVHDR.CCURRCODE) OR INVHDR.CCURRCODE=ALLTRIM(gcBaseCurr))
    *B608508,1 WAM 04/08/2008 (End)
ENDIF

  *--------R E T U R N S----------------

  *B608493,1 WAM 03/26/2008 get returns when RM modules is installed
  *IF 'MA' $ oAriaApplication.CompanyInstalledModules
  IF 'RM' $ oAriaApplication.CompanyInstalledModules
  *B608493,1 WAM 03/26/2008 (ENd)

    SELE RETHDR
    =GFSEEK(XACCOUNT)

    *B608508,1 WAM 04/08/2008 Consider empty currency as base currency
    *SUM REST IIF(llMultCurr,gfAmntDisp(TOTCREDIT, lcRpCurr , ldRpExDate , lcRpTmpNam),TOTCREDIT);
             TO XRET WHILE XACCOUNT=ACCOUNT ;
             FOR   BETWEEN(CRDATE,LDATE,HDATE);
             .AND. IIF(!EMPTY(lcCurrency),SEEK(RETHDR.CCURRCODE ,lcCurrency),RETHDR.CCURRCODE=ALLTRIM(gcBaseCurr))
    *: B609632,1 MMT 06/27/2011 Activity Summary rep. Apply the Factored/Non Factored filter on Returns[T20110428.0020][START]
*!*	    SUM REST IIF(llMultCurr,gfAmntDisp(TOTCREDIT, lcRpCurr , ldRpExDate , lcRpTmpNam),TOTCREDIT);
*!*	             TO XRET WHILE XACCOUNT=ACCOUNT ;
*!*	             FOR   BETWEEN(CRDATE,LDATE,HDATE);
*!*	             .AND. IIF(!EMPTY(lcCurrency),SEEK(RETHDR.CCURRCODE ,lcCurrency),EMPTY(RETHDR.CCURRCODE) OR RETHDR.CCURRCODE=ALLTRIM(gcBaseCurr))
    SUM REST IIF(llMultCurr,gfAmntDisp(TOTCREDIT, lcRpCurr , ldRpExDate , lcRpTmpNam),TOTCREDIT);
             TO XRET WHILE XACCOUNT=ACCOUNT ;
             FOR   BETWEEN(CRDATE,LDATE,HDATE);
             .AND. IIF(!EMPTY(lcCurrency),SEEK(RETHDR.CCURRCODE ,lcCurrency),EMPTY(RETHDR.CCURRCODE) OR RETHDR.CCURRCODE=ALLTRIM(gcBaseCurr)) AND ;
             IIF(lcRpFact = 'F' , !EMPTY(RETHDR.cFacCode) ,IIF(lcRpFact = 'N' , EMPTY(RetHdr.cFacCode),.T.))
    *: B609632,1 MMT 06/27/2011 Activity Summary rep. Apply the Factored/Non Factored filter on Returns[T20110428.0020][END]
    *B608508,1 WAM 04/08/2008 (End)

    XRET=(-1*XRET)
  ENDIF
  *--------B A L A N C E------------------
  XBAL=(XOPENBAL+XSALES+XPAY+XRET+XFALL+XDALL+XMALL)
  *----- T O T A L S -------
  XTOTOPEN = XTOTOPEN  + XOPENBAL
  XTOTSALES= XTOTSALES + XSALES
  XTOTPAY  = XTOTPAY   + XPAY
  XTOTRET  = XTOTRET   + XRET
  XTOTFALL = XTOTFALL  + XFALL
  XTOTDALL = XTOTDALL  + XDALL
  XTOTMALL = XTOTMALL  + XMALL
  XTOTBAL  = XTOTBAL   + XBAL
  *-- Prevent printing customers with no activities.
   m.account= CUSTOMER.ACCOUNT
   m.acc_name= CUSTOMER.BTNAME
   m.op_bal= XOPENBAL
   m.sales= XSALES
   m.paym= XPAY
   m.return=XRET
   m.freight= XFALL
   m.disc= XDALL
   m.misc= XMALL
   *: B610889,1 MMT 10/22/2014 Activity summary report does not export end balance column to EXCEL[T20141021.0021][Start]
   m.End_Bal = m.op_bal + m.sales + m.paym + m.return + m.freight + m.disc + m.misc
   *: B610889,1 MMT 10/22/2014 Activity summary report does not export end balance column to EXCEL[T20141021.0021][End]
   *: B609751,1 MMT 11/27/2011 Apply Factored Filter on Credit/Debit files.[Start]
   *llNoact=IIF(m.op_bal=0 And  m.sales=0 And m.paym=0 And m.return=0 And m.freight=0 And m.disc=0 And m.misc=0 ,.t.,.f.)
   *: B610793,1 MMT 08/05/2014 Activity Summary report does not show accounts has activity(Open Balance)[T20140709.0093][Start]
   *llNoact=IIF(m.sales=0 And m.paym=0 And m.return=0 And m.freight=0 And m.disc=0 And m.misc=0 ,.t.,.f.)
   llNoact=IIF(m.op_bal=0 And  m.sales=0 And m.paym=0 And m.return=0 And m.freight=0 And m.disc=0 And m.misc=0 ,.t.,.f.)
   *: B610793,1 MMT 08/05/2014 Activity Summary report does not show accounts has activity(Open Balance)[T20140709.0093][End]
   *: B609751,1 MMT 11/27/2011 Apply Factored Filter on Credit/Debit files.[End]
   IF !llrpnact
     IF  !llNoact
       INSERT INTO  (lcWorkfile ) FROM MEMVAR
     ENDIF
   ELSE
     INSERT INTO  (lcWorkfile ) FROM MEMVAR
   ENDIF
ENDSCAN

SELECT (lcWorkfile )
IF !RECCOUNT()>0
  llDonprnt=.T.
  *-- Message : There are no records to display...!
  *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF

INDEX ON ACCOUNT TAG (lcWorkfile )

=lfAdjustCRSettings()
IF USED(lcWorkfile )
    USE IN (lcWorkfile )
ENDIF

=gfDispRe()
ELSE
  IF llDonprnt
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ELSE
    =gfDispRe()
  ENDIF
ENDIF  &&FILTER CHANGE

*!*************************************************************
*! Name      : Calculate
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/27/98
*! Purpose   : Calculate THE OPEN BALANCE, PAYMENT AND ALLOWANCE
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : gfRltFld()
*!*************************************************************
*! Called from : ARACTSUM.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =CALCULATE()
*!*************************************************************

PROCEDURE CALCULATE
*------------------------------- D E B I T ----------------------------------*
SELECT DEBIT
=GFSEEK(XACCOUNT)
*B608508,1 WAM 04/08/2008 Consider empty currency as base currency
*SCAN REST WHILE ACCOUNT=XACCOUNT FOR  IIF(!EMPTY(lcCurrency),SEEK(DEBIT.CCURRCODE ,lcCurrency),DEBIT.CCURRCODE=ALLTRIM(gcBaseCurr))
*: B609751,1 MMT 11/27/2011 Apply Factored Filter on Credit/Debit files.[Start]
*SCAN REST WHILE ACCOUNT=XACCOUNT FOR  IIF(!EMPTY(lcCurrency),SEEK(DEBIT.CCURRCODE ,lcCurrency),EMPTY(DEBIT.CCURRCODE) OR DEBIT.CCURRCODE=ALLTRIM(gcBaseCurr))
SCAN REST WHILE ACCOUNT=XACCOUNT FOR  IIF(!EMPTY(lcCurrency),SEEK(DEBIT.CCURRCODE ,lcCurrency),EMPTY(DEBIT.CCURRCODE) OR DEBIT.CCURRCODE=ALLTRIM(gcBaseCurr)) AND ;
                                      IIF(lcRpFact ='F' , !EMPTY(DEBIT.cFacCode) ,IIF(lcRpFact ='N' , EMPTY(DEBIT.cFacCode),.T.))
*: B609751,1 MMT 11/27/2011 Apply Factored Filter on Credit/Debit files.[End]
*B608508,1 WAM 04/08/2008 (End)

  *-- OPEN BALANCE --*
  XDFOR = IIF( TRANTYPE='3','TRANDATE<=ENDING .AND. CHGBK_DATE<=ENDING',;
                             'TRANDATE<=ENDING')
  IF &XDFOR

    XOPENDB = XOPENDB + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)

  ENDIF

  *---- ADJUSTMENT ---*
  IF (TRANTYPE = '2' AND BETWEEN(TRANDATE,LDATE,HDATE)) OR ;
    (TRANTYPE = '3' AND EMPTY(ChgBk_Date) AND BETWEEN(TRANDATE,LDATE,HDATE))

    lcType = SPACE(1)
    =gfRltFld(DEBIT.TRANCODE , @laType , 'TRANCODE')
    DO CASE
      CASE lcType = 'F'
        XFALL=XFALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)


      CASE lcType = 'D'
        XDALL=XDALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)

      OTHERWISE
        XMALL=XMALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)

    ENDCASE
  ENDIF
ENDSCAN

*-------------------------------- C R E D I T -------------------------------*
SELECT CREDIT
=GFSEEK( XACCOUNT)
*B608508,1 WAM 04/08/2008 Consider empty currency as base currency
*SCAN REST WHILE ACCOUNT=XACCOUNT FOR IIF(!EMPTY(lcCurrency),SEEK(CREDIT.CCURRCODE ,lcCurrency),CREDIT.CCURRCODE=ALLTRIM(gcBaseCurr))
*: B609751,1 MMT 11/27/2011 Apply Factored Filter on Credit/Debit files.[Start]
*SCAN REST WHILE ACCOUNT=XACCOUNT FOR IIF(!EMPTY(lcCurrency),SEEK(CREDIT.CCURRCODE ,lcCurrency),EMPTY(CREDIT.CCURRCODE) OR CREDIT.CCURRCODE=ALLTRIM(gcBaseCurr))
SCAN REST WHILE ACCOUNT=XACCOUNT FOR IIF(!EMPTY(lcCurrency),SEEK(CREDIT.CCURRCODE ,lcCurrency),EMPTY(CREDIT.CCURRCODE) OR CREDIT.CCURRCODE=ALLTRIM(gcBaseCurr)) AND ;
                                     IIF(lcRpFact ='F' , !EMPTY(CREDIT.cFacCode) ,IIF(lcRpFact ='N' , EMPTY(CREDIT.cFacCode),.T.))
*: B609751,1 MMT 11/27/2011 Apply Factored Filter on Credit/Debit files.[End]
*B608508,1 WAM 04/08/2008 (End)

  *-- OPEN BALANCE --*
  XCFOR = IIF( TRANTYPE='6','TRANDATE<=ENDING .AND. CREDT_DATE<=ENDING',;
                            'TRANDATE<=ENDING')
  IF &XCFOR
    XOPENCR=XOPENCR + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)

  ENDIF
  *---- PAYMENTS ----*
  IF (TRANTYPE='4' .AND. BETWEEN(TRANDATE,LDATE,HDATE))
    XPAY=XPAY + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)

  ENDIF

  *---- ADJUSTMENT ---*
  IF (TRANTYPE = '5' AND BETWEEN(TRANDATE,LDATE,HDATE)) OR ;
    (TRANTYPE = '6' AND EMPTY(Credt_Date) AND BETWEEN(TRANDATE,LDATE,HDATE))


    lcType = SPACE(1)
    =gfRltFld(CREDIT.cCreditCod , @laType , 'CCREDITCOD')
    DO CASE
      CASE lcType = 'F'
        XFALL=XFALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)


      CASE lcType = 'D'
        XDALL=XDALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)

      OTHERWISE
        XMALL=XMALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)


    ENDCASE
  ENDIF
ENDSCAN

*-------------------------------- A R H I S T -------------------------------*

SELE ARHIST
=GFSEEK (XACCOUNT)

*B608508,1 WAM 04/08/2008 Consider empty currency as base currency
*SCAN REST  WHILE ACCOUNT=XACCOUNT FOR IIF(!EMPTY(lcCurrency),SEEK(ARHIST.CCURRCODE ,lcCurrency),ARHIST.CCURRCODE=ALLTRIM(gcBaseCurr))
*: B609751,1 MMT 11/27/2011 Apply Factored Filter on Credit/Debit files.[Start]
*SCAN REST  WHILE ACCOUNT=XACCOUNT FOR IIF(!EMPTY(lcCurrency),SEEK(ARHIST.CCURRCODE ,lcCurrency),EMPTY(ARHIST.CCURRCODE) OR ARHIST.CCURRCODE=ALLTRIM(gcBaseCurr))
SCAN REST  WHILE ACCOUNT=XACCOUNT FOR IIF(!EMPTY(lcCurrency),SEEK(ARHIST.CCURRCODE ,lcCurrency),EMPTY(ARHIST.CCURRCODE) OR ARHIST.CCURRCODE=ALLTRIM(gcBaseCurr)) AND ;
                                      IIF(lcRpFact ='F' , !EMPTY(ARHIST.cFacCode) ,IIF(lcRpFact ='N' , EMPTY(ARHIST.cFacCode),.T.))
*: B609751,1 MMT 11/27/2011 Apply Factored Filter on Credit/Debit files.[End]
*B608508,1 WAM 04/08/2008 (End)

  *-- PAYMENTS --*
  IF (TRANTYPE='4'.AND.BETWEEN(TRANDATE,LDATE,HDATE))
    XPAY=XPAY + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)

  ENDIF

  *---- ADJUSTMENT ---*
  *: B609828,1 SAB 02/12/2012 Some records are not included in Activity Summary report if transtype = '3' [Start]
  *IF ((TRANTYPE='2'.OR.TRANTYPE='5'.OR.TRANTYPE='7').AND. ;
      BETWEEN(TRANDATE,LDATE,HDATE))
  IF ((TRANTYPE='2'.OR. (TRANTYPE = '3' AND EMPTY(ChgBk_Date)) .OR. TRANTYPE='5'.OR.TRANTYPE='7').AND. ;
      BETWEEN(TRANDATE,LDATE,HDATE))
  *: B609828,1 SAB 02/12/2012 Some records are not included in Activity Summary report if transtype = '3' [End]

    lcType = SPACE(1)
    =gfRltFld(ARHIST.TRANCODE , @laType , IIF(TRANTYPE='2','TRANCODE','CCREDITCOD')  )
    DO CASE
      CASE lcType = 'F'
        XFALL=XFALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)


      CASE lcType = 'D'
        XDALL=XDALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)


      OTHERWISE
        XMALL=XMALL + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)


    ENDCASE
  ENDIF

  *-- OPEN BALANCE --*
  IF (HISTDATE > ENDING) .AND. (TRANDATE <= ENDING)

    *** THE PERIOD CLOSING DATE
    IF TRANTYPE = '3' .AND. CHGBK_DATE>ENDING
      LOOP
    ENDIF

    IF TRANTYPE = '6' .AND. CREDT_DATE>ENDING
      LOOP
    ENDIF
    *: B609751,1 MMT 11/27/2011 Fix bug of not including return from ARHIST file[Start]
    *IF INLIST(TRANTYPE,'1','2','3')
    IF INLIST(TRANTYPE,'1','2','3','R')
    *: B609751,1 MMT 11/27/2011 Fix bug of not including return from ARHIST file[End]
      XOPENDB = XOPENDB + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)

    ENDIF
    IF INLIST(TRANTYPE,'0','4','5','6')
      XOPENCR = XOPENCR + IIF(llMultCurr,gfAmntDisp(AMOUNT, lcRpCurr , ldRpExDate , lcRpTmpNam),AMOUNT)

    ENDIF
  ENDIF
ENDSCAN

*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/27/98
*! Purpose   : The when function of the option grid
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : gfTempName()
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfwOGWhen()
*!*************************************************************
FUNCTION lfwOGWhen

*-- If select by date disable the month item, else enable it.
lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LCMONTH'),1)
laOGObjCnt[lnPos] = lcrpSelBy # 'D'
= lfOGShowGet('LCMONTH')

*-- If select by date disable the year item, else enable it.
lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LCYEAR'),1)
laOGObjCnt[lnPos] = lcrpSelBy # 'D'
= lfOGShowGet('LCYEAR')

*-- If select by date Enable the date item, else disable it.
lnDatInvPo = lfItmPos('INVHDR.INVDATE')
lnDisInvDt = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,"laOGFxFlt[" + ;
                   ALLTRIM(STR(lnDatInvPo)) + ",6]"),1)
laOGObjCnt[lnDisInvDt] = lcrpSelBy = 'D'
= lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnDatInvPo)) + ',6]')

*B603591,1 BWA 07/19/2000 Initialize a variable to hold the cusrrency code.[START]
*lcCurrency = gcBaseCurr
*B603591,1 [END]




*!*************************************************************
*! Name      : lfvpbDateOk
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 12/27/1998
*! Purpose   : Validate The select by (date/month) setting in the
*!             option grid
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbDateOk()
*!*************************************************************
FUNCTION lfvSelBy

lnDatInvPo = lfItmPos('INVHDR.INVDATE')

IF lcrpSelBy = 'D'   && Select by date
  *-- Enable Date & disable month and year
  STORE SPACE(0) TO lcmonth, lcYear
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LCMONTH'),1)
  laOGObjCnt[lnPos] = .F.
  = lfOGShowGet('LCMONTH')
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LCYEAR'),1)
  laOGObjCnt[lnPos] = .F.
  = lfOGShowGet('LCYEAR')

  lnDisInvDt = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,"laOGFxFlt[" + ;
                   ALLTRIM(STR(lnDatInvPo)) + ",6]"),1)
  laOGObjCnt[lnDisInvDt] = .T.
  = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnDatInvPo)) + ',6]')
ELSE                 && lcRpSelBy = 'M' Select by month
  *-- Enable month and year & disable date
  STORE {} TO lDate, hDate

  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LCMONTH'),1)
  laOGObjCnt[lnPos] = .T.
  = lfOGShowGet('LCMONTH')
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LCYEAR'),1)
  laOGObjCnt[lnPos] = .T.
  = lfOGShowGet('LCYEAR')
  lnDisInvDt = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,"laOGFxFlt[" + ;
                   ALLTRIM(STR(lnDatInvPo)) + ",6]"),1)
  laOGObjCnt[lnDisInvDt] = .F.
  = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnDatInvPo)) + ',6]')

ENDIF
*!***************************************************************************
*! Name      : lfItmPos
*! Developer : Albert Raif (ALB)
*! Date      : 06/09/2003
*! Purpose   : to get the position of the fixed filter in OG
*!***************************************************************************
*! Called from : OG When Function
*!***************************************************************************
*! Example   : = lfItmPos()
*!***************************************************************************
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

*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : AYMAN MAHMOUD AHMED (SMM)
*! Date      : 06/26/2006
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[1]
DIMENSION loOgScroll.laCRParams[3,2]

loOgScroll.lcOGLastForm ='ARACTSUM'
loOGScroll.cCROrientation='L'
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcWorkfile + ".DBF"


loOgScroll.laCRParams[1,1] = 'ReportName'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loOgScroll.laCRParams[1,2]=  Lang_ACTIVITY_SUMMARY_REPORT
loOgScroll.laCRParams[1,2]=  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_ACTIVITY_SUMMARY_REPORT,oAriaApplication.GetHeaderText("Lang_ACTIVITY_SUMMARY_REPORT",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


loOgScroll.laCRParams[2,1] = 'LCDEC'
loOgScroll.laCRParams[2,2]= IIF(LLRPDEC,'Y','N')

loOgScroll.laCRParams[3,1] = 'PERIOD'
loOgScroll.laCRParams[3,2]= XTITLE



*************************************************************
*! Name      : lfBuildTmp
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 05/30/2006
*! Purpose   :
*!*************************************************************
FUNCTION lfBuildTmp
*: B610889,1 MMT 10/22/2014 Activity summary report does not export end balance column to EXCEL[T20141021.0021][Start]
*DIMENSION laTempStru[9,18] ,laTempCOM[1,18]
DIMENSION laTempStru[10,18] ,laTempCOM[1,18]
*: B610889,1 MMT 10/22/2014 Activity summary report does not export end balance column to EXCEL[T20141021.0021][End]
PRIVATE lnFileCnt , lnFldRow
STORE '' TO laTempStru,laTempCOM
lcExcStat = SET('EXACT')
SET EXACT ON
laTempStru[1,1] = 'ACCOUNT'
laTempStru[1,2] = 'C'
laTempStru[1,3] = 6
laTempStru[1,4] = 0

laTempStru[2,1] = 'ACC_NAME'
laTempStru[2,2] = 'C'
laTempStru[2,3] = 30
laTempStru[2,4] = 0

laTempStru[3,1] = 'OP_BAL'
laTempStru[3,2] = 'N'
laTempStru[3,3] = 13
laTempStru[3,4] = 2


laTempStru[4,1] = 'SALES'
laTempStru[4,2] = 'N'
laTempStru[4,3] = 13
laTempStru[4,4] = 2

laTempStru[5,1] = 'PAYM'
laTempStru[5,2] = 'N'
laTempStru[5,3] = 13
laTempStru[5,4] = 2

laTempStru[6,1] = 'RETURN'
laTempStru[6,2] = 'N'
laTempStru[6,3] = 13
laTempStru[6,4] = 2

laTempStru[7,1] = 'FREIGHT'
laTempStru[7,2] = 'N'
laTempStru[7,3] = 13
laTempStru[7,4] = 2

laTempStru[8,1] = 'DISC'
laTempStru[8,2] = 'N'
laTempStru[8,3] = 13
laTempStru[8,4] = 2

laTempStru[9,1] = 'MISC'
laTempStru[9,2] = 'N'
laTempStru[9,3] = 13
laTempStru[9,4] = 2

*: B610889,1 MMT 10/22/2014 Activity summary report does not export end balance column to EXCEL[T20141021.0021][Start]
laTempStru[10,1]="End_Bal"
laTempStru[10,2] = 'N'
laTempStru[10,3] = 14
laTempStru[10,4] = 2
*: B610889,1 MMT 10/22/2014 Activity summary report does not export end balance column to EXCEL[T20141021.0021][End]

=gfCrtTmp(lcWorkfile ,@laTempstru,,"",.f.)
SET EXACT &lcExcStat





*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter
LOCAL lcReturn, lnPOS
DO CASE
  CASE lnArrayType = 1
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  CASE lnArrayType = 3
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  OTHERWISE
    lcReturn = ""
ENDCASE

RETURN lcReturn




