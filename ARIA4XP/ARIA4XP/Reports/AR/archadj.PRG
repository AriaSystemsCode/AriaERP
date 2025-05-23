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

*:T20060908.0022 - AYM 12/18/2006 :DO NOT USE LCRPEXP BUILD NEW EXPRESSIONS  --BEGIN
IF llOgFltCh
	llDontPrn=.F.
*--Define New Variable To Hold The Desc Reason.
STORE '' To lcTranCods 

*  BEGIN TRANSACTION 
STORE ' .T. ' TO lcHstExp,lcDbExp,lcCrExp

* Customer Filter
	lcCusFltr= ''
	lcCusFltr= lfCheckFilter(1, 'CUSTOMER.ACCOUNT')
	llCusFltr   = !EMPTY(lcCusFltr) AND USED(lcCusFltr) AND RECCOUNT(lcCusFltr) > 0
	IF llCusFltr   
	  SELECT (lcCusFltr)
	  INDEX ON ACCOUNT TAG (lcCusFltr)
	  lcHstExp=lcHstExp+" AND SEEK( CUSTOMER.ACCOUNT,'"+lcCusFltr+"') "
 	  lcDbExp=lcDbExp+" AND SEEK( CUSTOMER.ACCOUNT,'"+lcCusFltr+"') "
 	  lcCrExp=lcCrExp+" AND SEEK( CUSTOMER.ACCOUNT,'"+lcCusFltr+"') "
	ELSE
	  IF TYPE("lcCusFltr") = "C" AND USED(lcCusFltr)
	    USE IN (lcCusFltr)
	  ENDIF
	  lcCusFltr= ''
	ENDIF

	* SalesRep Filter
	lcRepFltr= ''
	lcRepFltr= lfCheckFilter(1, 'CUSTOMER.SALESREP')
	llRepFltr   = !EMPTY(lcRepFltr) AND USED(lcRepFltr) AND RECCOUNT(lcRepFltr) > 0
	IF llRepFltr   
	  SELECT (lcRepFltr)
	  INDEX ON REPCODE TAG (lcRepFltr)
	  lcHstExp=lcHstExp+" AND (SEEK( CUSTOMER.SALESREP,'"+lcRepFltr+"') .OR. SEEK( CUSTOMER.REP2,'"+lcRepFltr+"'))"
	  lcDbExp=lcDbExp+" AND (SEEK( CUSTOMER.SALESREP,'"+lcRepFltr+"') .OR. SEEK( CUSTOMER.REP2,'"+lcRepFltr+"'))"
	  lcCrExp=lcCrExp+" AND (SEEK( CUSTOMER.SALESREP,'"+lcRepFltr+"') .OR. SEEK( CUSTOMER.REP2,'"+lcRepFltr+"'))"

	ELSE
	  IF TYPE("lcRepFltr") = "C" AND USED(lcRepFltr)
	    USE IN (lcRepFltr)
	  ENDIF
	  lcRepFltr= ''
	ENDIF
* BATCH Filter
	lcBATFltr= ''
	lcBATFltr= lfCheckFilter(1, 'CREDIT.BATCH')
	llBATFltr   = !EMPTY(lcBATFltr) AND USED(lcBATFltr) AND RECCOUNT(lcBATFltr) > 0
	IF llBATFltr   
	  SELECT (lcBATFltr)
	  INDEX ON BATCH TAG (lcBATFltr)
	  lcHstExp=lcHstExp+" AND SEEK( BATCH,'"+lcBATFltr+"') "
	  lcDbExp=lcDbExp+" AND SEEK( BATCH,'"+lcBATFltr+"') "
	  lcCrExp=lcCrExp+" AND SEEK (BATCH,'"+lcBATFltr+"') "

	ELSE
	  IF TYPE("lcBATFltr") = "C" AND USED(lcBATFltr)
	    USE IN (lcBATFltr)
	  ENDIF
	  lcBATFltr= ''
	ENDIF

	* Check if there is a filter on DEBIT REASON
	lcCurName = lfCheckFilter(1, "IIF(TRANTYPE='2',DEBIT.TRANCODE,'')")  
	lcDBT   = loOgScroll.gfTempName()
	llDBT   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcDBT   ,"CDEB")
	IF llDBT   
	  SELECT (lcDBT)
	  INDEX on CDEB TAG (lcDBT )
    lcHstExp=lcHstExp+" AND IIF(TRANTYPE='2',SEEK (TRANCODE,'"+lcDBT   +"')"+", .T. )"
	  lcDbExp=lcDbExp+" AND IIF(TRANTYPE='2',SEEK (TRANCODE,  '"+lcDBT   +"')"+", .T. )"

	ENDIF

  *Check if there is a filter on CREDIT  REASON
	lcCurName = lfCheckFilter(1, "IIF(TRANTYPE='5',CREDIT.CCREDITCOD,'')")  
	lcCRDT   = loOgScroll.gfTempName()
	llCRDT   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcCRDT,"CCRDT")
	IF llCRDT   
	  SELECT (lcCRDT)
	  INDEX on CCRDT TAG (lcCRDT)
    lcHstExp=lcHstExp+" AND IIF(TRANTYPE $ '57',SEEK (TRANCODE,'"+lcCRDT+"')"+", .T. )"
	  lcCrExp=lcCrExp+" AND IIF(TRANTYPE $ '5'   ,SEEK (CCREDITCOD,'"+lcCRDT+"')"+", .T. )"

	ENDIF

* Check if there is a filter on PAYMENT TYPE
	lcCurName = lfCheckFilter(1, "IIF(TRANTYPE='4',CREDIT.CARPTYPE,'')")  
	lcCRTYP   = loOgScroll.gfTempName()
	llCTYP   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcCRTYP,"CCTYP")
	IF llCTYP   	
	  SELECT (lcCRTYP )
	  INDEX on CCTYP TAG (lcCRTYP )
    lcHstExp=lcHstExp +" AND IIF(TRANTYPE ='4',SEEK (CARPTYPE,'" + lcCRTYP  +"')"+", .T. )"
	  lcCrExp=lcCrExp   +" AND IIF(TRANTYPE ='4',SEEK (CARPTYPE,'" + lcCRTYP  +"')"+", .T. )"
	ENDIF

lnDatePos = lfItmPos('CREDIT.TRANDATE')
IF lnDatePos > 0
	IF  !EMPTY(laOGFxFlt[lnDatePos ,6])
		  ldStrtDate = CTOD(SUBSTR(laOGFxFlt[lnDatePos ,6],1, ATC('|',laOGFxFlt[lnDatePos ,6])-1))
		  ldEndDate  = CTOD(SUBSTR(laOGFxFlt[lnDatePos ,6],   ATC('|',laOGFxFlt[lnDatePos ,6])+1))
		  lcHstExp=lcHstExp+" AND BETWEEN(TRANDATE,ldStrtDate,ldEndDate) "
		  lcCrExp=lcCrExp+" AND BETWEEN(TRANDATE,ldStrtDate,ldEndDate) "
		  lcDbExp=lcDbExp+" AND BETWEEN(TRANDATE,ldStrtDate,ldEndDate) "
		  
		ENDIF 
ENDIF 
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
DO CASE
  CASE lcRpCAB = 'A' AND lcrpDbOrCr = 'C'      &&-CRIDIT ADJUSTMENTS ONLY
    lcRpFlt = lcRpFlt +" .AND.TRANTYPE $ '57' "
    
  CASE lcRpCAB = 'A' AND lcrpDbOrCr = 'D'      &&-DEBIT ADJUSTMENTS ONLY
    lcRpFlt = lcRpFlt +" .AND.TRANTYPE $ '2'"
    
  CASE lcRpCAB = 'A' AND lcrpDbOrCr = 'B'      &&-DEBIT & CRIDIT ADJUSTMENTS ONLY
    lcRpFlt = lcRpFlt +" .AND.TRANTYPE $ '257'"
    
  CASE lcRpCAB = 'C'                           &&-CASH ONLY
    lcRpFlt = lcRpFlt +" .AND.TRANTYPE $ '4'"   
    
  CASE lcRpCAB = 'B' AND lcrpDbOrCr = 'B'      &&-CASH & Adjustment
    lcRpFlt = lcRpFlt +" .AND.TRANTYPE $ '2457'" 
    
  CASE lcRpCAB = 'B' AND lcrpDbOrCr = 'C'      &&-CASH & CRIDIT ADJUSTMENTS ONLY
    lcRpFlt = lcRpFlt +" .AND.TRANTYPE $ '457'" 
               
  CASE lcRpCAB = 'B' AND lcrpDbOrCr = 'D'      &&-CASH & DEBIT ADJUSTMENTS ONLY
    lcRpFlt = lcRpFlt +" .AND.TRANTYPE $ '24'"       
ENDCASE
*:E302635,1 HES 09/01/2009 Handle the New Option "Select Debit or Credit [END]

*-- Add the filter of Factored & Nofactored accounts
DO CASE
  CASE lcrpFAcct='F'
    lcRpFlt = lcRpFlt + " .AND. !EMPTY(Customer.cFacCode) "
  CASE lcrpFAcct='N'
    lcRpFlt = lcRpFlt + " .AND. EMPTY(Customer.cFacCode) "
ENDCASE

*--Define Currency Filter.
lcCurrFilt = ""

*-- if Print in Forign currency (Filter on selected currency)
IF llRpForCur
  lcCurrFilt = [ AND cCurrCode = lcRpCurr]
  lcHstExp= lcHstExp+ lcCurrFilt
  lcCrExp=  lcCrExp+ lcCurrFilt
  lcDbExp=  lcDbExp+ lcCurrFilt

ENDIF 


*--------------------------------------------
* SELECT RECORDS FROM ARHIST FILE
*--------------------------------------------
SELECT ARHIST
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER

lcHstExp= lcHstExp+ lcRpFlt
lcHstExp= lcHstExp+ " .AND. !(TRANTYPE $ '89') "
WAIT WINDOW 'SELECTING A/R HISTORY RECORDS ...' NOWAIT
*:T20060908.0022 - AYM 12/18/2006 :DO NOT USE LCRPEXP BUILD NEW EXPRESSIONS  -- BEGIN
IF USED(TRANFILE)
	USE IN (TRANFILE)
ENDIF
*:T20060908.0022 - AYM 12/18/2006 :DO NOT USE LCRPEXP BUILD NEW EXPRESSIONS  -- END
COPY ALL TO (oAriaApplication.Workdir+TRANFILE) FOR &lcHstExp

SELECT ARHIST
SET RELATION TO
=gfOpentable(oAriaApplication.Workdir+TRANFILE,'','EX')

*---------------------------------------
* SELECT RECORDS FROM OPEN CREDIT FILE
*---------------------------------------
WAIT WINDOW 'SELECTING CREDIT RECORDS ...' NOWAIT


lcCrExp= lcCrExp+ lcRpFlt
IF lcrpCAB $ 'CB' .AND. lcrpCshT<>'B'
  DO CASE
    CASE lcrpCshT = 'A'
      lcCrExp= lcCrExp+ ".AND.IIF(TranType='4',!lNonAR,.T.)"
    CASE lcrpCshT = 'N'
      lcCrExp= lcCrExp+ ".AND.IIF(TranType='4',lNonAR,.T.)"
  ENDCASE
ENDIF

SELECT CREDIT
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER
COPY ALL TO (oAriaApplication.WorkDir+WORKFILE) FOR &lcCrExp


SELECT CREDIT
SET RELATION TO

*--Add credit records to the main temporary file.
=gfOpenTABLE(oAriaApplication.WorkDir+WORKFILE,'','EX')
LOCATE
SCAN
  SCATTER MEMVAR MEMO
  m.TranCode = m.cCreditCod
  INSERT INTO (TRANFILE) FROM MEMVAR
ENDSCAN
USE

*--------------------------------------------
* COPY DEBIT DATA IF PROCESSING ADJUSTMENTS
*--------------------------------------------
IF lcrpCAB <> 'C'
  
  WAIT WINDOW 'SELECTING DEBITS FOR REPORT ...' NOWAIT
  SELECT DEBIT
  lcDbExp= lcDbExp+ lcRpFlt

  SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER
  COPY ALL TO (oAriaApplication.WorkDir+WORKFILE) FOR &lcDbExp
  SELECT DEBIT
  SET RELATION TO
  SELECT &TRANFILE
  APPEND FROM (oAriaApplication.WorkDir+WORKFILE)
ENDIF

SELECT (TRANFILE)
LOCATE
IF EOF()
  llDontPrn=.T.
  *Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF

XTITLE   = ''
IF lnDatePos > 0
  lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)

  DO CASE
    CASE EMPTY(CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,10))) .AND. EMPTY(CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],12,21)))
        XTITLE   = ''
      
    CASE EMPTY(CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,10))) .AND. !EMPTY(CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],12,21)))
        Hdata     = SUBSTR(laOGFxFlt[lnDatePos,6],12,21)
        XTITLE    = "PERIOD: To &Hdata "
        
    CASE !EMPTY(CTOD(laOGFxFlt[lnDatePos,6]))
        Ldata    = SUBSTR(laOGFxFlt[lnDatePos,6],1,10)
        Hdata    = SUBSTR(laOGFxFlt[lnDatePos,6],12,21)
        XTITLE   = "PERIOD FROM: &Ldata To &Hdata "
  ENDCASE
ENDIF

DO CASE
  *--CUSTOMER
  CASE lcRpSRTCD = 'C'
    XSORT   = 'ACCOUNT+TRANTYPE+TRANCODE+TRAN'
    HBREAK2 = 'ACCOUNT'
    HBREAK1 = 'TRANTYPE+TRANCODE'
    XTITLE  = XTITLE + " (SORT: CUSTOMER)"

  *--TRANSACTION NUMBER
  CASE lcRpSRTCD = 'T'
    XSORT   = 'TRANTYPE+TRANCODE+DTOS(TRANDATE)+TRAN'
    HBREAK2 = 'TRANTYPE'
    HBREAK1 = 'TRANCODE'
    XTITLE  = XTITLE + " (SORT: TRANSACTION)"
  
  *--BATCH NUMBER
  CASE lcRpSRTCD = 'B'
    XSORT   = 'BATCH+TRANTYPE+TRANCODE+TRAN'
    HBREAK2 = 'BATCH'
    HBREAK1 = 'TRANTYPE+TRANCODE'
    XTITL  = XTITLE + " (SORT: BATCH)"
  
  *--REASON CODE
  CASE lcRpSRTCD = 'R'
    XSORT   = 'TRANCODE+TRANTYPE+TRAN'
    HBREAK2 = 'TRANCODE'
    HBREAK1 = 'TRANCODE+TRANTYPE'
    XTITLE  = XTITLE + " (SORT: REASON CODE)"
ENDCASE

Z = LTRIM(STR(RECCOUNT(),7))
WAIT WINDOW 'SORTING &Z RECORDS ...' NOWAIT
SET TALK ON
INDEX ON &XSORT TAG &TRANFILE

SET TALK OFF
SET ORDER TO TAG &TRANFILE

lcTmpTrn = loOgScroll.gfTempName()
lcTmp2DB = loOgScroll.gfTempName()
=lfAdjustCRSettings()

SELECT (TRANFILE)
=AFIELDS(laFileStru)

=lfAddField("laFileStru", "Reason" ,"C",24,0,.T.,.F.,"","","","","","","","","","",0,0)
=lfAddField("laFileStru", "CustName" ,"C",20,0,.T.,.F.,"","","","","","","","","","",0,0)

DIMENSION laIndx[1,2]
laIndx[1,1] = XSORT
laIndx[1,2] = (lcTmp2DB)

=gfCrtTmp(lcTmp2DB,@laFileStru,@laIndx,lcTmp2DB,.F.)

SELECT (lcTmp2DB)
COPY ALL TO (oAriaApplication.WorkDir + "TRANFILE.DBF")

SELECT (TRANFILE)
SCAN
  SCATTER MEMVAR MEMO
  *:B608350,1 MMT 11/13/2007 fix bug of not converting amount to base currency[Start]
  IF llMultCurr AND !llRpForCur
    m.Amount = lfBaseAmt()
  ENDIF 
  *:B608350,1 MMT 11/13/2007 fix bug of not converting amount to base currency[End]
  
  SELECT (lcTmp2DB)
  APPEND BLANK
  GATHER MEMVAR MEMO
  REPLACE &lcTmp2DB..CustName WITH IIF(SEEK('M' + &TRANFILE..ACCOUNT , 'CUSTOMER') , LEFT(CUSTOMER.BTNAME ,20) , "")
 
  IF TRANTYPE = '2'
    lcDesc = GFCODDES(&TRANFILE..TRANCODE,'TRANCODE')
    lcDesc = IIF( ALLTRIM(lcDesc) = "N/A" , "" , lcDesc)
    REPLACE &lcTmp2DB..Reason WITH lcDesc
  ELSE
    lcDesc2 = GFCODDES(TRANCODE,'CCREDITCOD')
    lcDesc2 = IIF( ALLTRIM(lcDesc2) = "N/A" , "" , lcDesc2)
    REPLACE &lcTmp2DB..Reason WITH lcDesc2
  ENDIF

ENDSCAN

SELECT (lcTmp2DB)
LOCATE
COPY ALL TO (oAriaApplication.WorkDir + lcTmpTrn + ".DBF")

*:T20060908.0022 - AYM 12/18/2006 :DO NOT USE LCRPEXP BUILD NEW EXPRESSIONS  --BEGIN
*gfDispRe
IF USED(lcTmpTrn )
	    USE IN (lcTmpTrn )
ENDIF

	=gfDispRe()
ELSE
  IF llDontPrn
    *-- Message : There are no records to display...!
    *--                < Ok > 
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ELSE
    =gfDispRe()
  ENDIF  
ENDIF  
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
FUNCTION lfwOGWhen

*--Fill Currency arrays.
=lfFillCurr()

*--Get position of Sales Rep.
lnSlRepPos = lfItmPos('CUSTOMER.SALESREP')

*-- Get the position of the print cost setting in the array to enable or 
*-- disable it.
lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,UPPER('lcrpCshT')),1)
laOGObjCnt[lnPos] = (lcrpCAB<>'A')
lcrpCshT = 'B'
= lfOGShowGet('lcrpCshT')

*-- Company use Multi Currency.
IF llMultCurr
  lnCurrPos  = lfGetVarPos("lcRpCurr")

  *-- if it is User Saved Filter and your saving is to print by
  *-- equavelent in base amount.
  IF !(lnOGSeting = 1 OR llRpForCur)
    =lfvForCurr()
  ENDIF

ELSE  && Company use single currency
  *-- Rest Currency variables.
  lcRpCurr   = ""
  llRpForCur = .F.  
ENDIF  

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
FUNCTION lfvCAB
PRIVATE lnArreyLen

*:E302635,1 HES 09/01/2009 Make a problem while trying to supress the "Select Debit or Credit" Option [Start] 
*!*	lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,UPPER('lcrpCshT')),1)
*!*	laOGObjCnt[lnPos] = (lcrpCAB<>'A')
*!*	lcrpCshT = 'B'

*!*	= lfOGShowGet('lcrpCshT')
*:E302635,1 HES 09/01/2009 Make a problem while trying to supress the "Select Debit or Credit" Option [End]

lnArreyLen = ALEN(laDataVal,1)

*--Add a new option of sorting in case of adjustment only.
IF lcrpCAB = "A"
  DIMENSION laDataDes[4,1],laDataVal[4,1]
  laDataDes[1] = "Transaction"
  laDataDes[2] = "Customer"
  laDataDes[3] = "Batch"
  laDataDes[4] = "Reson Code"
  laDataVal[1] = "T"
  laDataVal[2] = "C"
  laDataVal[3] = "B"
  laDataVal[4] = "R"
ELSE
  DIMENSION laDataDes[3,1],laDataVal[3,1]
  laDataDes[1] = "Transaction"
  laDataDes[2] = "Customer"
  laDataDes[3] = "Batch"
  laDataVal[1] = "T"
  laDataVal[2] = "C"
  laDataVal[3] = "B"
ENDIF

IF lnArreyLen != ALEN(laDataVal,1)
  lcRpSRTCD = "T"
  *:E302635,1 HES 09/01/2009
*!*	  CLEAR READ
  CLEARREAD()
  *:E302635,1 HES 09/01/2009
ENDIF  

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
FUNCTION lfvCashTyp

*-- Variable To Clear Selection From The InRang If Disable The Bottom.
llClear = .T.

*-- Get the position of the Customer in the array to enable or 
*-- disable it.
IF ASCAN(laOGFxFlt,"CUSTOMER.ACCOUNT") # 0
  lnSrcLoc   = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,"CUSTOMER.ACCOUNT"),1)
  *--Enable the object.
  laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGFxFlt,1) - ALEN(laOGFxFlt,1)+ lnSrcLoc] =  (lcrpCshT#"N")
  
  *--Refresh the source location setting in the option grid
  = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnSrcLoc)) + ',6]')
ENDIF

*--Enable or Disbale SalesRep.
IF ASCAN(laOGFxFlt,"CUSTOMER.SALESREP") # 0
  lnSrcLoc   = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,"CUSTOMER.SALESREP"),1)
  laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGFxFlt,1) - ALEN(laOGFxFlt,1)+ lnSrcLoc] =  (lcrpCshT#"N")
  = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnSrcLoc)) + ',6]')
ENDIF

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
FUNCTION lfsrvSty
PARAMETERS lcParm

IF lcParm = 'R'  && Reset code
  llClear = .F.
ENDIF

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
FUNCTION lfFillCurr

llMultCurr = gfGetMemVar('llMulCurr')
IF llMultCurr
  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
  IF !USED('SYCCURR')
    llOpenCurr = gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  ELSE
    SELECT SYCCURR
    SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
ENDIF

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
FUNCTION lfGetVarPos
PARAMETERS lcVarName
PRIVATE lnVarPos

lnVarPos = ASCAN(laOGObjType,UPPER(lcVarName))
IF lnVarPos > 0
  lnVarPos = ASUBSCRIPT(laOGObjType,lnVarPos,1)
ENDIF
RETURN lnVarPos

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
FUNCTION lfvForCurr
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
FUNCTION lfAddCurr

XFILTER = ALLTRIM(XFILTER)
IF !EMPTY(XFILTER)
  XFILTER = XFILTER + [ AND ]
ENDIF
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
FUNCTION lfBaseAmt

PRIVATE lnBaseAmt
lnBaseAmt = AMOUNT
*-- if Multi currency and user want to print in base currency and 
*-- currency not the base currency.
IF llMultCurr AND !llRpForCur AND (cCurrCode != gcBaseCurr)
  lnBaseAmt = gfAmntDisp(lnBaseAmt,"O",TranDate)
ENDIF
RETURN lnBaseAmt

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

DIMENSION laDataDes[3,1],laDataVal[3,1]
laDataDes[1] = "Transaction"
laDataDes[2] = "Customer"
laDataDes[3] = "Batch"

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
FUNCTION lfSetSRep
PARAMETERS OpGrdParm

DO CASE
  CASE OpGrdParm = 'S'
   SELECT SalesRep
   SET ORDER TO TAG SalesRep
   LOCATE
  CASE OpGrdParm = 'R'
    SELECT SalesRep 
    SET ORDER TO 
ENDCASE
*-- End of lfSetSRep.

*!**************************************************************************
*! Name      : lfSetBatch
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 05/29/2006
*! Purpose   : Get Bacth Numbers frrom Debit and Credit Files
*!*************************************************************
*! Example   : =lfSetBatch()
*!*************************************************************
FUNCTION lfSetBatch
PARAMETERS OpGrdParm

DO CASE
  CASE OpGrdParm = 'S'
    IF !USED(lcBatchFile)
      *B608149,1 WAM 07/03/2007 Browse batches from ARHIST file as well 
      *SELECT Distinct Batch, Desc, TranDate ;
        FROM Credit ;
       WHERE !EMPTY(Batch) ;
       UNION (SELECT Distinct Batch, Desc, TranDate ;
                FROM Debit ;
               WHERE !EMPTY(Batch)) ;
        INTO CURSOR &lcBatchFile
      
      SELECT Distinct Batch, Desc, TranDate ;
        FROM Credit ;
       WHERE !EMPTY(Batch) ;
       UNION (SELECT Distinct Batch, Desc, TranDate ;
                FROM Debit ;
               WHERE !EMPTY(Batch)) ;
       UNION (SELECT Distinct Batch, Desc, TranDate ;
                FROM ARHIST ;
               WHERE !EMPTY(Batch)) ;
        INTO CURSOR &lcBatchFile
      *B608149,1 WAM 07/03/2007 (End)
      SELECT (lcBatchFile)
      LOCATE
    ENDIF
ENDCASE

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
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

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
FUNCTION lfAdjustCRSettings

loOgScroll.lcOGLastForm = "ARCHADJ"

DIMENSION loOgScroll.laCRTables[1]
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir + lcTmpTrn + ".DBF"

DIMENSION loOgScroll.laCRParams[6,2]
loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2] = 'CASH & ADJUSTMENTS JOURNAL REPORT'

loOgScroll.laCRParams[2,1] = 'Layout'
IF lcrpDS = 'S'
  loOgScroll.laCRParams[2,2] = 'Summary Format'
ELSE
  loOgScroll.laCRParams[2,2] = 'Detail Format'
ENDIF

loOgScroll.laCRParams[3,1] = 'SortBy'
DO CASE
  CASE lcRpSRTCD = 'C'
    loOgScroll.laCRParams[3,2] = 'Customer'
    
  CASE lcRpSRTCD = 'T'
	loOgScroll.laCRParams[3,2] = 'Transaction Number'

  CASE lcRpSRTCD = 'B'
	loOgScroll.laCRParams[3,2] = 'Batch Number'

  CASE lcRpSRTCD = 'R'
	loOgScroll.laCRParams[3,2] = 'Reason Code'

ENDCASE

loOgScroll.laCRParams[4,1] = 'OpTitle'
loOgScroll.laCRParams[4,2] = XTITLE

loOgScroll.laCRParams[5,1] = 'DetOrSum'
loOgScroll.laCRParams[5,2] = lcrpDS

loOgScroll.laCRParams[6,1] = 'PrintDecimal'
loOgScroll.laCRParams[6,2] = IIF(llRpDec, 1, 0)

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
FUNCTION lfAddField
PARAMETERS lcStruArry , lcFldName , lcFldType , lnFldLen , lnFldDec ,ln5,ln6,ln7,ln8,ln9,ln10,ln11,ln12,ln13,ln14,ln15,ln16,ln17,ln18

lnFldPos  = ALEN(&lcStruArry,1) + IIF(TYPE('&lcStruArry') = 'L', 0 , 1 )
DIMENSION &lcStruArry[lnFldPos , 18]
&lcStruArry[lnFldPos , 1]	= lcFldName
&lcStruArry[lnFldPos , 2]	= lcFldType
&lcStruArry[lnFldPos , 3]	= lnFldLen
&lcStruArry[lnFldPos , 4]	= lnFldDec

FOR lnCount = 5 TO 18
  lcParam = 'ln' + ALLTRIM(STR(lnCount))
  IF lnCount = 5 OR lnCount = 6
    &lcStruArry[lnFldPos, lnCount] = &lcParam
  ENDIF
  IF BETWEEN(lnCount , 7 , 16)
    &lcStruArry[lnFldPos ,lnCount] = &lcParam
  ENDIF
  IF lnCount = 17 OR lnCount = 18
    &lcStruArry[lnFldPos , lnCount] = &lcParam
  ENDIF
ENDFOR 

*--End of lfAddField.






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
FUNCTION lfStr2Curs 
PARAMETERS lcString , lccursor , lcFieldsName

CREATE CURSOR (lcCursor) (&lcFieldsName. C(6))
DO WHILE AT('|',lcString)> 0
  lcFieldsValue  = SUBSTR(lcString,1,AT('|',lcString)-1)
  lcString = SUBSTR(lcString,AT('|',lcString)+1)
  SELECT (lcCursor)
  APPEND BLANK
  REPLACE &lcFieldsName. WITH lcFieldsValue
ENDDO
SELECT (lcCursor)
APPEND BLANK
REPLACE &lcFieldsName. WITH lcString

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
FUNCTION lfRefOG

=CLEARREAD()
* End of lfRefOG()