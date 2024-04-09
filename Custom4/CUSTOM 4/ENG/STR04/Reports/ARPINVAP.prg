*** 
*B609539,1 MMT 02/28/2011 Custom Invoice Form AP does not filter by print status[T20110214.0014]
***
 *B609539,1 MMT 02/28/2011 Custom Invoice Form AP does not filter by print status[Start]
 lcRpPrSt = IIF(oAriaApplication.ProcessID = 'ARPINV',IIF(lcRpPrSt ='N',SPACE(1),lcRpPrSt),"")
 *B609539,1 MMT 02/28/2011 Custom Invoice Form AP does not filter by print status[End]
 STORE 0 TO lnclrlngl, lnclrposgl, lnstylngl, lnstyposgl
 STORE 0 TO lnscale
 DIMENSION laitemseg[1]
 = gfitemmask(@laitemseg)
 FOR lncount = 1 TO ALEN(laitemseg, 1)
    IF laitemseg(lncount, 1)='C'
       lnclrlngl = LEN(laitemseg(lncount, 3))
       lnclrposgl = laitemseg(lncount, 4)
       EXIT
    ENDIF
 ENDFOR
 DIMENSION laitemseg[1]
 = gfitemmask(@laitemseg)
 FOR lncount = 1 TO ALEN(laitemseg, 1)
    IF laitemseg(lncount, 1)='F'
       lnstylngl = LEN(laitemseg(lncount, 3))
       lnstyposgl = laitemseg(lncount, 4)
       EXIT
    ENDIF
 ENDFOR
 lctax_rate = gfgetmemvar('M_TAX_RATE', oariaapplication.activecompanyid)
 lcemail = gfgetmemvar('E_MAIL', oariaapplication.activecompanyid)
 lcwebsite = gfgetmemvar('WEBSITE', oariaapplication.activecompanyid)
 lccoregno = gfgetmemvar('COREGNO', oariaapplication.activecompanyid)
 lcspcinst = gfcoddes(invhdr.spcinst, 'SPCINST')
 lntaxrat = 0
 DIMENSION lataxrat[1, 2]
 lataxrat[1, 1] = 'NTAXRATE'
 lataxrat[1, 2] = 'lnTaxRat'
 SELECT invhdr
 LOCATE FOR &lcrpexp
 IF  .NOT. FOUND()
    = gfmodalgen('TRM00052B00000', 'DIALOG')
    llnorec = .T.
    SET DEVICE TO SCREEN
    llarpinv = .F.
	*B609539,1 MMT 02/28/2011 Custom Invoice Form AP does not filter by print status[Start]
    lcRpPrSt = IIF(lcRpPrSt =SPACE(1),'N',lcRpPrSt)
    *B609539,1 MMT 02/28/2011 Custom Invoice Form AP does not filter by print status[End]
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
    = gfopenfile(gcdatadir+'OrdLine', 'OrdLine', 'SH', @lctmpordln, .T.)
 ENDIF
 lnchrgtax = 0
 lnchgnontx = 0
 lctmpinvch = gftempname()
 IF  .NOT. USED(lctmpinvch)
    = gfopenfile(gcdatadir+'invchrg', 'invchrg', 'SH', @lctmpinvch)
 ENDIF
 lcinvchgno = ''
 SELECT invline
 = AFIELDS(lafilestru)
 CREATE TABLE (gcworkdir+lcstylindx) FROM ARRAY lafilestru
 INDEX ON invoice+STR(lineno, 6) TAG invline OF (gcworkdir+invline+'.CDX')
 CREATE TABLE (gcworkdir+lctaxable) (invoice C (6), taxable N (12, 2), nontax N (12, 2), price N (9, 2))
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
                lctax = 'CONSINVL.QTY'+ALLTRIM(STR(lnlop))
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
                lctax = 'CONSINVL.QTY'+ALLTRIM(STR(lnlop))
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
                lctax = 'INVLINE.QTY'+ALLTRIM(STR(lnlop))
                IF BETWEEN(lnlop, style.ntaxbreak, 8) .AND. IIF(customer.type='M',  .NOT. customer.lvatexem, lfcustexmt()) .AND. lntaxrat>0
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
                lctax = 'INVLINE.QTY'+ALLTRIM(STR(lnlop))
                IF BETWEEN(lnlop, style.ntaxbreak, 8) .AND. IIF(customer.type='M',  .NOT. customer.lvatexem, lfcustexmt()) .AND. lntaxrat>0
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
             REPLACE &lctaxable..taxable WITH &lctaxable..taxable + lnchrgtax , &lctaxable..price   WITH &lctaxable..price + lnchrgtax   , &lctaxable..nontax  WITH &lctaxable..nontax + lnchgnontx
          ELSE
             APPEND BLANK
             REPLACE &lctaxable..invoice WITH lcinvcee , &lctaxable..taxable WITH &lctaxable..taxable + lnchrgtax , &lctaxable..price   WITH &lctaxable..price + lnchrgtax   , &lctaxable..nontax  WITH &lctaxable..nontax + lnchgnontx
          ENDIF
       ENDIF
    ENDIF
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
 SET RELATION ADDITIVE TO 'S'+invline.account+invline.style INTO spck_lin
 SELECT style
 SET RELATION ADDITIVE TO 'S'+scale INTO scale
 SELECT invhdr
 SET RELATION ADDITIVE TO IIF(EMPTY(store) .OR. store='********', 'M'+account, 'S'+account+store) INTO customer
 SET RELATION ADDITIVE TO 'O'+invhdr.order INTO ordhdr
 SET RELATION ADDITIVE TO 'C'+invhdr.invoice INTO notepad
 SET RELATION ADDITIVE TO invhdr.invoice INTO (lctaxable)
 IF llprntinst .OR. llrpinvnot
    SET SKIP TO (lctmpdbt), invline
 ELSE
    SET SKIP TO invline
 ENDIF
 SELECT invhdr
 DO gfdispre WITH EVALUATE('lcFormName'), 'FOR '+lcrpexp
 *B609539,1 MMT 02/28/2011 Custom Invoice Form AP does not filter by print status[Start]
 lcRpPrSt = IIF(lcRpPrSt =SPACE(1),'N',lcRpPrSt)
 *B609539,1 MMT 02/28/2011 Custom Invoice Form AP does not filter by print status[End]
 SET DEVICE TO SCREEN
 llarpinv = .F.
 WAIT CLEAR
 SELECT invline
 CLOSE INDEXES
 USE IN invline
 = gfopenfile(gcdatadir+'InvLine', 'InvLine', 'SH')
 = lfbastoclr(lctmpordln, 'F')
 = lfbastoclr(lcstylindx, 'F')
 = lfbastoclr(lctaxable, 'F')
 = lfbastoclr(invline, 'F')
 = lfbastoclr(lctmpinvch, 'F')
ENDPROC
**
FUNCTION LFCUSTEXMT
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
PROCEDURE LFBASTOCLR
 PARAMETER lcfilname, lctypfun
 IF lctypfun='F'
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
FUNCTION LFGETDISC
 PRIVATE latrltfld, lcpyterm, lcret, lnterdiscd, lnterdiscr, lnterdscd1, lnterdscr1, lnslct, lni, lnk, lnalias
 lnslct = SELECT()
 STORE 0 TO lnterdiscd, lnterdiscr, lnterdscd1, lnterdscr1
 STORE '' TO lcdiscnt1, lcdiscnt2, lcnotet1, lcnotet2, lcnotet3, lcnotet4, lcnotet5, lcnotet6, lcnotet7
 FOR lni = 1 TO 15
    lci = ALLTRIM(STR(lni))
    STORE SPACE(0) TO lcnotet&lci
 ENDFOR
 IF  .NOT. llendgroup
    RETURN ''
 ENDIF
 PRIVATE lastring, lavars
 DIMENSION lastring[2], lavars[2]
 STORE '' TO lastring, lavars
 lavars[1] = 'lcDiscnt1'
 lavars[2] = 'lcDiscnt2'
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
    lnalias = ALIAS()
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
    SELECT (lnalias)
    IF TYPE('lnTerDiscD')='C'
       lnterdiscd = 0
    ENDIF
    IF lnterdiscd<>0 .AND. lnterdiscr<>0
       lastring[1] = 'If paid by '+DTOC(invhdr.invdate+lnterdiscd)+' you may deduct '+ALLTRIM(STR((invhdr.shipamt)*lnterdiscr/100, 10, 2))
    ENDIF
    IF lnterdscd1<>0 .AND. lnterdscr1<>0
       lastring[2] = 'If paid by '+DTOC(invhdr.invdate+lnterdscd1)+' you may deduct '+ALLTRIM(STR((invhdr.shipamt)*lnterdscr1/100, 10, 2))
    ENDIF
    = lfrmvgap()
 ENDIF
 SELECT notepad
 IF SEEK('T'+customer.usr_dfnd10, 'NOTEPAD')
    DIMENSION lastring[15], lavars[15]
    STORE '' TO lastring, lavars
    FOR lni = 1 TO 15
       lci = ALLTRIM(STR(lni))
       lavars[lni] = 'lcNoteT'+lci
       lastring[lni] = ALLTRIM(MLINE(mnotes, lni))
    ENDFOR
    = lfrmvgap()
 ENDIF
 SELECT (lnslct)
 RETURN ''
ENDFUNC
**
PROCEDURE LFRMVGAP
 lnk = ALEN(lavars, 1)
 FOR lni = ALEN(lavars, 1) TO 1 STEP -1
    IF  .NOT. EMPTY(lastring(lni))
       &lavars[lnk] = lastring[lni]
       lnk = lnk-1
    ENDIF
 ENDFOR
ENDPROC
**
