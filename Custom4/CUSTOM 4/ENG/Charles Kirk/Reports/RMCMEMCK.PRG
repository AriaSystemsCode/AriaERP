*C201597,1 SAB 11/03/2013 Modify Order Confirmation Form CK and Credit Memo Form CK to Run from RB [T20120824.0005]
*C201597,1 SAB 11/03/2013 Modify Order Confirmation Form CK and Credit Memo Form CK to Run from RB [T20120824.0005][Start]
IF TYPE('lcXMLFileName') = 'O'
  =gfOpenTable('SCALE','SCALE','SH','SCALE')
  =gfOpenTable('CUSTOMER','CUSTOMER','SH','CUSTOMER')
  =gfOpenTable('NOTEPAD','NOTEPAD','SH','NOTEPAD')
  =gfOpenTable('WAREHOUS','WAREHOUS','SH','WAREHOUS')
  =gfOpenTable('SYCCOMP','CCOMP_ID','SH','SYCCOMP')
  =gfOpenTable('CODES','CODES','SH','CODES')
  =gfOpenTable('OBJECTS','OBJECTID','SH','OBJECTS')
  =gfOpenTable('OBJLINK','OBJLNKTY','SH','OBJLINK')
  =gfOpenTable('SYCINT','CCONTCODE','SH','SYCINT')
  =gfOpenTable('RETHDR','RETHDR','SH','RETHDR')
  =gfOpenTable('RETLINE','RETLINE','SH','RETLINE')
  =gfOpenTable('STYLE','STYLE','SH','STYLE')
ENDIF
*C201597,1 SAB 11/03/2013 Modify Order Confirmation Form CK and Credit Memo Form CK to Run from RB [T20120824.0005][End]
lcTax_Rate = gfgetmemvar('M_TAX_RATE', oariaapplication.activecompanyid)
LNTAXRAT = 0
DIMENSION LATAXRAT[ 1, 2]
LATAXRAT[ 1, 1] = 'NTAXRATE'
LATAXRAT[ 1, 2] = 'lnTaxRat'
= lfbuildtmp()
SELECT (lcTaxAble)
INDEX ON CRMEMO TAG (lcTaxAble)
SELECT (lcRetLine)
SCAN
	LCCRMEM = &lcRetline..CRMEMO
	lcSQL = "SELECT ACCOUNT, STORE FROM RETHDR WHERE CRMEMO = '" + lcCrMem + "'"
    llsqlerro = lorethdr.sqlrun(lcsql, "TRetHdr", .T.)
    IF llsqlerro
		LCSTORE = tRETHDR.STORE
		lcAccount = tRETHDR.Account
	ELSE
		*C201597,1 SAB 11/03/2013 Modify Order Confirmation Form CK and Credit Memo Form CK to Run from RB [T20120824.0005][Start]
		*WAIT WINDOW 'SQL Error ' + lcCrMem
		IF TYPE('lcXMLFileName') <> 'O'
		  WAIT WINDOW 'SQL Error ' + lcCrMem
		ELSE
		  *loProgress.Percent = Percent of processing done
		  loProgress.DESCRIPTION = 'SQL Error ' + lcCrMem
		  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
		ENDIF
		*C201597,1 SAB 11/03/2013 Modify Order Confirmation Form CK and Credit Memo Form CK to Run from RB [T20120824.0005][End]
	ENDIF
	*C201597,1 SAB 11/03/2013 Modify Order Confirmation Form CK and Credit Memo Form CK to Run from RB [T20120824.0005][Start]
	*WAIT WINDOW NOWAIT 'Selecting Records For The Report ...'+lcCrMem
	IF TYPE('lcXMLFileName') <> 'O'
	  WAIT WINDOW NOWAIT 'Selecting Records For The Report ...'+lcCrMem
	ELSE
	  *loProgress.Percent = Percent of processing done
	  loProgress.DESCRIPTION = 'Selecting Records For The Report ...'+lcCrMem
	  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
	ENDIF
	*C201597,1 SAB 11/03/2013 Modify Order Confirmation Form CK and Credit Memo Form CK to Run from RB [T20120824.0005][End]
	= GFRLTFLD(STYLE.CTAXCODE,@LATAXRAT,'CTAXCODE')
*	LNTAXABLE = 0
	SELECT (LCTAXABLE)
	IF SEEK(LCCRMEM)
		REPLACE &lcTaxable..TrdeAmt WITH &lcTaxable..TrdeAmt + &lcRetline..TRDE_AMT
		FOR LNLOP = 1 TO SCALE.CNT
			LCTAX = '&lcRetline..QTY'+ALLTRIM(STR(LNLOP))
			IF BETWEEN(LNLOP,STYLE.NTAXBREAK,8) .AND. LFCUSTEXMT() .AND. LNTAXRAT>0
				IF EVALUATE(LCTAX)>0
					REPLACE &lcTaxable..Taxable WITH &lcTaxable..Taxable + (EVAL(lcTax) * &lcRetline..PRICE) , &lcTaxable..PRICE   WITH &lcTaxable..PRICE + (EVAL(lcTax) * &lcRetline..GROS_PRICE)
				ENDIF
			ELSE
				REPLACE &lcTaxable..NonTax WITH &lcTaxable..NonTax + (EVAL(lcTax) * &lcRetline..PRICE) 
            ENDIF
		ENDFOR
	ELSE
		APPEND BLANK
		REPLACE &lcTaxable..CRMEMO  WITH &lcRetline..CRMEMO , &lcTaxable..TrdeAmt WITH &lcRetline..TRDE_AMT
		FOR LNLOP = 1 TO SCALE.CNT
			LCTAX = '&lcRetline..QTY'+ALLTRIM(STR(LNLOP))
			IF BETWEEN(LNLOP,STYLE.NTAXBREAK,8) .AND. LFCUSTEXMT() .AND. LNTAXRAT>0
				IF EVALUATE(LCTAX)>0
					REPLACE &lcTaxable..Taxable WITH &lcTaxable..Taxable + (EVAL(lcTax) * &lcRetline..PRICE) , &lcTaxable..PRICE   WITH &lcTaxable..PRICE + (EVAL(lcTax) * &lcRetline..GROS_PRICE)
				ENDIF
			ELSE
				REPLACE &lcTaxable..NonTax WITH &lcTaxable..NonTax + (EVAL(lcTax) * &lcRetline..PRICE) 
            ENDIF
		ENDFOR
	ENDIF
ENDSCAN
SELECT RETHDR
SET RELATION ADDITIVE TO crmemo INTO (lcTaxAble)

******

FUNCTION  LFCUSTEXMT
	PRIVATE LLRETURN, LCALIASX1
	LLRETURN = .F.
	LCALIASX1 = SELECT(0)
	SELECT CUSTOMER
	LCKEYX1 = EVALUATE(KEY())
	IF SEEK(IIF(lcStore=SPACE(8),'M'+lcAccount,'S'+lcAccount+lcStore))
		LLRETURN = .NOT. CUSTOMER.LVATEXEM
	ENDIF
	SELECT CUSTOMER
	= SEEK(LCKEYX1)
	SELECT (LCALIASX1)
	RETURN LLRETURN
ENDFUNC

************

PROCEDURE lfBuildTmp
	DIMENSION latempstru[5, 18]
	STORE '' TO latempstru
	STORE 0 TO lnindex
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'CRMEMO'
	latempstru[lnindex, 2] = 'C'
	latempstru[lnindex, 3] = 6
	latempstru[lnindex, 4] = 0
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'TAXABLE'
	latempstru[lnindex, 2] = 'N'
	latempstru[lnindex, 3] = 12
	latempstru[lnindex, 4] = 2
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'NONTAX'
	latempstru[lnindex, 2] = 'N'
	latempstru[lnindex, 3] = 12
	latempstru[lnindex, 4] = 2
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'PRICE'
	latempstru[lnindex, 2] = 'N'
	latempstru[lnindex, 3] = 9
	latempstru[lnindex, 4] = 2
	lnindex = lnindex+1
	latempstru[lnindex, 1] = 'TRDEAMT'
	latempstru[lnindex, 2] = 'N'
	latempstru[lnindex, 3] = 7
	latempstru[lnindex, 4] = 2
	= gfcrttmp(lcTaxAble, @latempstru, , "", .F.)
ENDPROC