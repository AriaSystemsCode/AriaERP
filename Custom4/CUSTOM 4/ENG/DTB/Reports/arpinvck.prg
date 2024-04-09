*B610050,1 MMT 08/16/2012 Fix bug of error while preview invoice Form CK[T20120730.0001]
*** 
*** ReFox XI+  #UK115113  Chris  Aria Systems Ltd [VFP90]
***
 PRIVATE lcteom, lneomday, lntdaysdue, ldduedate
 lcteom = ''
 lntdaysdue = 0
 lneomday = 20
 DIMENSION latermfld[ 3, 2]
 *B610050,1 MMT 08/16/2012 Fix bug of error while preview invoice Form CK[T20120730.0001][Start]
 * latermfld[ 1, 1] = 'EOM'
 latermfld[ 1, 1] = 'EOM '
 *B610050,1 MMT 08/16/2012 Fix bug of error while preview invoice Form CK[T20120730.0001][End]
 latermfld[ 1, 2] = 'lcTEOM'
 latermfld[ 2, 1] = 'NTERDUED'
 latermfld[ 2, 2] = 'lnTDaysDue'
 latermfld[ 3, 1] = 'EOMDAY'
 latermfld[ 3, 2] = 'lnEomDay'
 STORE 0 TO lnclrlngl, lnclrposgl, lnstylngl, lnstyposgl
 lccompphone = TRANSFORM(lccompphon, '@R '+lcphonpict)
 STORE 0 TO lnscale

 DIMENSION laitemseg[1]
* MAH Add to Support Request Builder
* = gfitemmask(@laitemseg)
IF TYPE('oAriaEnvironment') == 'O'
  LOCAL loItemMask
  loItemMask = CREATEOBJECT("GetItemMask")
  loItemMask.Do(@laitemseg)
ELSE
   = gfitemmask(@laitemseg)
ENDIF
* MAH Add to Support Request Builder


 FOR lncount = 1 TO ALEN(laitemseg, 1)
    IF laitemseg(lncount, 1)='C'
       lnclrlngl = LEN(laitemseg(lncount, 3))
       lnclrposgl = laitemseg(lncount, 4)
       EXIT
    ENDIF
 ENDFOR

* MAH Add to Support Request Builder
IF TYPE('oAriaEnvironment') <> 'O'
  * MAH Add to Support Request Builder
   DIMENSION laitemseg[1]
   = gfitemmask(@laitemseg)
  * MAH Add to Support Request Builder
ENDIF
* MAH Add to Support Request Builder
 FOR lncount = 1 TO ALEN(laitemseg, 1)
    IF laitemseg(lncount, 1)='F'
       lnstylngl = LEN(laitemseg(lncount, 3))
       lnstyposgl = laitemseg(lncount, 4)
       EXIT
    ENDIF
 ENDFOR
 lctax_rate = gfgetmemvar('M_TAX_RATE', gcact_comp)
 lntaxrat = 0
 DIMENSION lataxrat[1, 2]
 lataxrat[1, 1] = 'NTAXRATE'
 lataxrat[1, 2] = 'lnTaxRat'
 lcrpprst = IIF(oariaapplication.processid='ARPINV', IIF(lcrpprst='N', SPACE(1), lcrpprst), "")
 IF  .NOT. USED('DEBIT')
    = gfopenfile(gcdatadir+'DEBIT','DEBIT','SH')
 ENDIF
 IF  .NOT. USED('ORDDSGN')
	= gfopentable('ORDDSGN','ORDLINE','SH')
 ELSE
	SELECT invline
	SET RELATION OFF INTO ORDDSGN
 ENDIF
 IF  .NOT. USED('ARTWRKDS')
	= gfopentable('ARTWRKDS','ARTWRKDS','SH')
 ELSE
	SELECT ORDDSGN
	SET RELATION OFF INTO ARTWRKDS
 ENDIF
 SELECT invhdr
 LOCATE FOR &lcrpexp
 IF  .NOT. FOUND()
  * MAH Add to Support Request Builder
  IF TYPE('oAriaEnvironment') <> 'O'
  * MAH Add to Support Request Builder
    = gfmodalgen('TRM00052B00000', 'DIALOG')
  * MAH Add to Support Request Builder
  ENDIF
  * MAH Add to Support Request Builder
    llnorec = .T.
    SET DEVICE TO SCREEN
    llarpinv = .F.
    RETURN
 ENDIF
 lcasexp = IIF(EMPTY(lcrpexp), '.T.', lcrpexp)
 SET DEVICE TO PRINTER
 PRIVATE lnconsline
 lnconsline = 0
 lctmpordln = gftempname()
 lctaxable = gftempname()
 lcstylindx = gftempname()
 invline = gftempname()
 IF  .NOT. USED(lctmpordln)
    = gfopenfile(gcdatadir+"OrdLine", "OrdLine", 'SH', @lctmpordln)
 ENDIF
 lnchrgtax = 0
 lnchgnontx = 0
 lctmpinvch = gftempname()
 IF  .NOT. USED(lctmpinvch)
    = gfopenfile(gcdatadir+"invchrg", "invchrg", 'SH', @lctmpinvch)
 ENDIF
 lcinvchgno = ''
 SELECT invline
 = AFIELDS(lafilestru)
 CREATE TABLE (gcworkdir+lcstylindx) FROM ARRAY lafilestru
 INDEX ON invoice+STR(lineno, 6) TAG invline OF (gcworkdir+invline+'.CDX')
 CREATE TABLE (gcworkdir+lctaxable) (invoice C (6), taxable N (12, 2), nontax N (12, 2), price N (9, 2), ntaxchrg N (14, 2), nnontaxchg N (14, 2))
 INDEX ON invoice TAG taxplat OF (gcworkdir+lctaxable+'.CDX')
 SELECT invhdr
 SET RELATION ADDITIVE TO invhdr.invoice INTO consinvl
 SCAN FOR &lcasexp
    lcinvcee = invhdr.invoice
    IF invhdr.consol='Y'
       lnconsline = 0
       SELECT consinvl
       SCAN REST WHILE invoice+store+order+style+STR(lineno, 6)=lcinvcee
          WAIT WINDOW NOWAIT 'Selecting Records For The Report ...'+lcinvcee
          SCATTER MEMO MEMVAR
          lnconsline = lnconsline+1
          m.lineno = lnconsline
          lnrecnost = 0
          lnrecnost = RECNO('STYLE')
          = SEEK(consinvl.style, 'STYLE')
          = SEEK('S'+style.scale, 'SCALE')
          lnscale = scale.cnt
          = SEEK(consinvl.style+lcinvcee, 'INVLINE', 'INVLINES')
          m.desc1 = invline.desc1
          lntaxrat = 0
          = gfrltfld(style.ctaxcode, @lataxrat, 'CTAXCODE')
          lntaxbreak = style.ntaxbreak
          IF BETWEEN(lnrecnost, 1, RECCOUNT('STYLE'))
             GOTO lnrecnost IN style
          ENDIF
          SELECT (lcstylindx)
          APPEND BLANK
          GATHER MEMO MEMVAR
          SELECT (lctaxable)
          IF SEEK(lcinvcee)
             FOR lnlop = 1 TO lnscale
                lctax = "CONSINVL.QTY"+ALLTRIM(STR(lnlop))
                llexemted = lfcustexmt()
                IF BETWEEN(lnlop, lntaxbreak, 8) .AND. llexemted .AND. lntaxrat>0
                   IF EVALUATE(lctax)>0
                      REPLACE &lctaxable..taxable WITH &lctaxable..taxable + (EVAL(lctax) * consinvl.price) , &lctaxable..price   WITH &lctaxable..price + (EVAL(lctax) * consinvl.price)
                   ENDIF
                ELSE
                   REPLACE &lctaxable..nontax WITH &lctaxable..nontax + ( EVAL(lctax) * consinvl.price)
                ENDIF
             ENDFOR
          ELSE
             APPEND BLANK
             REPLACE &lctaxable..invoice WITH m.invoice
             FOR lnlop = 1 TO lnscale
                lctax = "CONSINVL.QTY"+ALLTRIM(STR(lnlop))
                llexemted = lfcustexmt()
                IF BETWEEN(lnlop, lntaxbreak, 8) .AND. llexemted .AND. lntaxrat>0
                   IF EVALUATE(lctax)>0
                      REPLACE &lctaxable..taxable WITH &lctaxable..taxable + (EVAL(lctax) * consinvl.price) , &lctaxable..price   WITH &lctaxable..price + (EVAL(lctax) * consinvl.price)
                   ENDIF
                ELSE
                   REPLACE &lctaxable..nontax WITH &lctaxable..nontax + (EVAL(lctax) * consinvl.price)
                ENDIF
             ENDFOR
          ENDIF
       ENDSCAN
    ELSE
       SELECT invline
       SCAN REST WHILE invoice+STR(lineno, 6)=lcinvcee
		  = gfrltfld(invhdr.ctermcode,@latermfld,'CTERMCODE')
		  lneomday = IIF(TYPE('lnEOMDay')<>'N' .OR. lneomday=0, 20, lneomday)
		  ldduedate = IIF(lcteom<>'Y', invhdr.shipdate+lntdaysdue, CTOD('01'+SUBSTR(DTOC(GOMONTH(invhdr.shipdate, IIF(DAY(invhdr.shipdate)>lneomday, 2, 1))), 3))-1+lntdaysdue)
		  UPDATE invhdr SET duedate=ldduedate WHERE invoice=invline.invoice AND duedate<>ldduedate
		  UPDATE debit SET duedate=ldduedate WHERE tran=invline.invoice AND duedate<>ldduedate AND account=invhdr.account
          WAIT WINDOW NOWAIT 'Selecting Records For The Report ...'+lcinvcee
          SCATTER MEMO MEMVAR
          SELECT (lcstylindx)
          APPEND BLANK
          GATHER MEMO MEMVAR
          = gfrltfld(style.ctaxcode, @lataxrat, 'CTAXCODE')
          SELECT (lctaxable)
          lntaxable = 0
          IF SEEK(lcinvcee)
             FOR lnlop = 1 TO scale.cnt
                lctax = "INVLINE.QTY"+ALLTRIM(STR(lnlop))
                IF BETWEEN(lnlop, style.ntaxbreak, 8) .AND. IIF(customer.type="M",  .NOT. customer.lvatexem, lfcustexmt()) .AND. lntaxrat>0
                   IF EVALUATE(lctax)>0
                      REPLACE &lctaxable..taxable WITH &lctaxable..taxable + (EVAL(lctax) * invline.price) , &lctaxable..price   WITH &lctaxable..price + (EVAL(lctax) * invline.gros_price)
                   ENDIF
                ELSE
                   REPLACE &lctaxable..nontax WITH &lctaxable..nontax + (EVAL(lctax) * invline.price)
                ENDIF
             ENDFOR
          ELSE
             APPEND BLANK
             REPLACE &lctaxable..invoice WITH m.invoice
             FOR lnlop = 1 TO scale.cnt
                lctax = "INVLINE.QTY"+ALLTRIM(STR(lnlop))
                IF BETWEEN(lnlop, style.ntaxbreak, 8) .AND. IIF(customer.type="M",  .NOT. customer.lvatexem, lfcustexmt()) .AND. lntaxrat>0
                   IF EVALUATE(lctax)>0
                      REPLACE &lctaxable..taxable WITH &lctaxable..taxable + (EVAL(lctax) * invline.price) , &lctaxable..price   WITH &lctaxable..price + (EVAL(lctax) * invline.gros_price)
                   ENDIF
                ELSE
                   REPLACE &lctaxable..nontax WITH &lctaxable..nontax + (EVAL(lctax) * invline.price)
                ENDIF
             ENDFOR
          ENDIF
       ENDSCAN
    ENDIF
    lnchrgtax = 0
    lnchgnontx = 0
    IF lcinvchgno<>lcinvcee
       lcinvchgno = lcinvcee
       SELECT (lctmpinvch)
       IF SEEK(lcinvcee)
          SCAN REST WHILE invoice+cstore+cchrgcode=lcinvcee
             IF ntaxrate>0
                lnchrgtax = lnchrgtax+nchrgamnt
             ELSE
                lnchgnontx = lnchgnontx+nchrgamnt
             ENDIF
          ENDSCAN
          SELECT (lctaxable)
          IF SEEK(lcinvcee)
             REPLACE &lctaxable..nTaxChrg WITH &lctaxable..nTaxChrg + lnchrgtax   , &lctaxable..nNonTaxChg WITH &lctaxable..nNonTaxChg + lnchgnontx
          ELSE
             APPEND BLANK
             REPLACE &lctaxable..invoice WITH lcinvcee , &lctaxable..nTaxChrg WITH &lctaxable..nTaxChrg + lnchrgtax   , &lctaxable..nNonTaxChg WITH &lctaxable..nNonTaxChg + lnchgnontx
          ENDIF
       ENDIF
    ENDIF
*!*	    IF SEEK(lcInvcee,(lcTmpInvCh)) AND SEEK(lcInvcee,(lcTaxable)) AND !&lcTaxable..llFlag
*!*	       SELECT (lctmpinvch)
*!*	       SCAN REST WHILE invoice+cstore+cchrgcode=lcinvcee
*!*	          SELECT (lctaxable)
*!*	          REPLACE llflag WITH .T.
*!*	          IF &lcTmpInvCh..nTaxRate > 0
*!*	             REPLACE nTaxChrg WITH nTaxChrg + &lcTmpInvCh..nChrgAmnt
*!*	          ELSE
*!*	             REPLACE nNonTaxChg WITH nNonTaxChg + &lcTmpInvCh..nChrgAmnt
*!*	          ENDIF
*!*	       ENDSCAN
*!*	    ENDIF
 ENDSCAN
 
 
 
 SELECT invhdr
 SET RELATION OFF INTO (lctmpdbt)
 SET RELATION OFF INTO customer
 SET RELATION OFF INTO ordhdr
 SET RELATION OFF INTO consinvl
 SELECT (lctmpdbt)
 SET RELATION TO
 SELECT invline
 SET RELATION OFF INTO style
 SET RELATION OFF INTO spck_lin
 SELECT style
 SET RELATION OFF INTO scale
 SELECT invline
 CLOSE INDEXES
 USE IN invline
 USE IN (lcstylindx)
 USE (gcworkdir+lcstylindx) ALIAS invline IN 0
 SELECT invline
 INDEX ON invoice+style+STR(lineno, 6) TAG invline OF (gcworkdir+lcstylindx+'.CDX')
 SELECT invhdr
 IF llprntinst .OR. llrpinvnot
    SET RELATION TO '' INTO (lctmpdbt)
    SELECT (lctmpdbt)
    SET RELATION ADDITIVE TO IIF(cfile_num='1', invhdr.invoice, '*') INTO invline
 ELSE
    SET RELATION ADDITIVE TO invhdr.invoice INTO invline
 ENDIF
 SELECT invline
 LOCATE
 SET RELATION ADDITIVE TO IIF( .NOT. EMPTY(invline.altstyle), invline.altstyle, invline.style) INTO style
 SET RELATION ADDITIVE TO "S"+invline.account+invline.style INTO spck_lin
 SELECT style
 SET RELATION ADDITIVE TO 'S'+scale INTO scale
 SELECT invhdr
 SET RELATION ADDITIVE TO IIF(EMPTY(store) .OR. store="********", 'M'+account, 'S'+account+store) INTO customer
 SET RELATION ADDITIVE TO 'O'+invhdr.order INTO ordhdr
 SET RELATION ADDITIVE TO 'C'+invhdr.invoice INTO notepad
 SET RELATION ADDITIVE TO invhdr.invoice INTO (lctaxable)
 IF llprntinst .OR. llrpinvnot
    SET SKIP TO (lctmpdbt), invline
 ELSE
    SET SKIP TO invline
 ENDIF
 SELECT invline
 SET RELATION TO order+STR(lineno, 6)+STR(lineno, 6) INTO ORDDSGN ADDITIVE
 SELECT ORDDSGN
 SET RELATION TO CDESIGNID INTO ARTWRKDS ADDITIVE
 lctermcode = ''
 lntotalchg = 0
 lntax_amt = 0
 ldinvdate = {}
 SELECT invhdr
 DO gfdispre WITH EVALUATE('lcFormName'), 'FOR '+lcrpexp
 SET DEVICE TO SCREEN
 llarpinv = .F.
 WAIT CLEAR
 
 * MAH Add to Support Request Builder
IF TYPE('oAriaEnvironment') <> 'O'
* MAH Add to Support Request Builder
 SELECT invline
 CLOSE INDEXES
 USE IN invline
 = gfopenfile(gcdatadir+"InvLine", "InvLine", 'SH')
 = lfbastoclr(lctmpordln, 'F')
 = lfbastoclr(lcstylindx, 'F')
 = lfbastoclr(lctaxable, 'F')
 = lfbastoclr(invline, 'F')
 = lfbastoclr(lctmpinvch, 'F')
* MAH Add to Support Request Builder
ENDIF
* MAH Add to Support Request Builder

ENDPROC
**
FUNCTION lfCustExmt
 PRIVATE llreturn, lcaliasx1
 llreturn = .F.
 lcaliasx1 = SELECT(0)
 SELECT customer
 lckeyx1 = EVALUATE(KEY())
 IF SEEK(IIF(m.store=SPACE(8), 'M'+m.account, 'S'+m.account+m.store))
    llreturn =  .NOT. customer.lvatexem
 ENDIF
 SELECT customer
 = SEEK(lckeyx1)
 SELECT (lcaliasx1)
 RETURN llreturn

 
ENDFUNC
**
PROCEDURE lfBasToClr
 PARAMETER lcfilname, lctypfun
 IF lctypfun="F"
    IF USED(lcfilname)
       SELECT (lcfilname)
       USE
    ENDIF
 ELSE
    FOR lnlop = 1 TO ALEN(lcfilname, 1)
       IF USED(lcfilname(lnlop))
          SELECT (lcfilname(lnlop))
          USE
       ENDIF
    ENDFOR
 ENDIF
ENDPROC
**
FUNCTION lfGetDisc
 PRIVATE latrltfld, lcpyterm, lcret, lnterdiscd, lnterdiscr, lnterdscd1, lnterdscr1, lnslct, lni, lnk
 lnslct = SELECT()
 STORE 0 TO lnterdiscd, lnterdiscr, lnterdscd1, lnterdscr1
 STORE '' TO lcdiscnt1, lcdiscnt2, lcnotet1, lcnotet2, lcnotet3, lcnotet4
 IF  .NOT. llendgroup
    RETURN ''
 ENDIF
 PRIVATE lastring, lavars
 DIMENSION lastring[2], lavars[2]
 STORE '' TO lastring, lavars
 lavars[1] = "lcDiscnt1"
 lavars[2] = "lcDiscnt2"
 IF llrpprsdc
    DIMENSION latrltfld[4, 2]
    latrltfld[1, 1] = 'NTERDISCD'
    latrltfld[1, 2] = 'lnTerDiscD'
    latrltfld[2, 1] = 'NTERDISCR'
    latrltfld[2, 2] = 'lnTerDiscR'
    latrltfld[3, 1] = 'NTERDISCD1'
    latrltfld[3, 2] = 'lnTerDscD1'
    latrltfld[4, 1] = 'NTERDISCR1'
    latrltfld[4, 2] = 'lnTerDscR1'
    lcalias = ALIAS()
    SELECT codes
    SET ORDER TO Codes
    IF SEEK('N'+lctermcode+'Y'+'CTERMCODE')
       SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name='N'+lctermcode+'Y'+'CTERMCODE'
          IF crltd_nam=PADR('NTERDISCD', 10)
             lnterdiscd = VAL(crltd_vlu)
          ENDIF
          IF crltd_nam=PADR('NTERDISCR', 10)
             lnterdiscr = VAL(crltd_vlu)
          ENDIF
          IF crltd_nam='NTERDISCD1'
             lnterdscd1 = VAL(crltd_vlu)
          ENDIF
          IF crltd_nam='NTERDISCR1'
             lnterdscr1 = VAL(crltd_vlu)
          ENDIF
       ENDSCAN
    ENDIF
    SELECT (lcalias)
    IF TYPE('lnTerDiscD')='C'
       lnterdiscd = 0
    ENDIF
    IF lnterdiscd<>0 .AND. lnterdiscr<>0
       lastring[1] = "If paid by "+DTOC(ldinvdate+lnterdiscd)+" you may deduct "+ALLTRIM(STR((lntotalchg-lntax_amt)*lnterdiscr/100, 10, 2))
    ENDIF
    IF TYPE('lnTerDscD1')='C'
       lnterdscd1 = 0
    ENDIF
    IF lnterdscd1<>0 .AND. lnterdscr1<>0
       lastring[2] = "If paid by "+DTOC(ldinvdate+lnterdscd1)+" you may deduct "+ALLTRIM(STR((lntotalchg-lntax_amt)*lnterdscr1/100, 10, 2))
    ENDIF
    = lfrmvgap()
 ENDIF
 SELECT notepad
 IF SEEK("TBANK", "NOTEPAD")
    DIMENSION lastring[4], lavars[4]
    STORE '' TO lastring, lavars
    lavars[1] = "lcNoteT1"
    lavars[2] = "lcNoteT2"
    lavars[3] = "lcNoteT3"
    lavars[4] = "lcNoteT4"
    lastring[1] = ALLTRIM(MLINE(mnotes, 1))
    lastring[2] = ALLTRIM(MLINE(mnotes, 2))
    lastring[3] = ALLTRIM(MLINE(mnotes, 3))
    lastring[4] = ALLTRIM(MLINE(mnotes, 4))
    = lfrmvgap()
 ENDIF
 SELECT (lnslct)
 RETURN ''
ENDFUNC
**
PROCEDURE lfRmvGap
 lnk = ALEN(lavars, 1)
 FOR lni = ALEN(lavars, 1) TO 1 STEP -1
    IF  .NOT. EMPTY(lastring(lni))
       &lavars[lnk] = lastring[lni]
       lnk = lnk-1
    ENDIF
 ENDFOR
ENDPROC
**
*** 
*** ReFox - all is not lost 
***
