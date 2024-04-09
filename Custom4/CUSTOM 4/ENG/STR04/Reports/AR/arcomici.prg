*** 
*** ReFox XI+  #KO744164  Mariam  Mariam [VFP90]
***
llcomplete = .T.
SET DATE BRITISH
SET CENTURY ON
STORE 0 TO lnhandle, lninvoices, lncredits
= gfopentable('syccomp')
= gfopentable('invline')
= gfopentable('style', 'style')
= gfopentable('apvendor', 'vencode')
= gfopentable('customer', 'customer', 'SH', 'customer')
SELECT invline
SET ORDER TO invline
SET RELATION TO style INTO style
SELECT style
SET RELATION TO vendor INTO apvendor
SELECT syccomp
SET ORDER TO ccomp_id
= SEEK(oariaapplication.activecompanyid)
lctaxrefds = ALLTRIM(gfgetmemvar('M_TAX_REFE'))
llinvselect = .F.
lcinvsel = ''
lninvoice = ASCAN(loogscroll.laogfxflt, "INVHDR.INVOICE")
IF lninvoice > 0
 lninvoice = ASUBSCRIPT(loogscroll.laogfxflt, lninvoice, 1)
 lcinvsel = IIF(!EMPTY(loogscroll.laogfxflt(lninvoice, 6)), loogscroll.laogfxflt(lninvoice, 6), '')
 IF !EMPTY(lcinvsel) .AND. USED(lcinvsel)
  SELECT (lcinvsel)
  LOCATE
  IF !EOF()
   llinvselect = .T.
  ENDIF
 ENDIF
ENDIF
IF llinvselect
 lcdatafile = gftempname()
 lctryfile = gftempname()
 = lfbuildtmp()
 SELECT (lcinvsel)
 LOCATE
 lcinvoice = &lcinvsel..invoice
 lnunit = 0
 lnnexrate = 0
 lneurexrate = gfchkrate('lnUnit', 'EUR', DATE(), .F.)
 SCAN
  IF gfseek(&lcinvsel..invoice,'InvHdr')
   SELECT invhdr
   IF gfseek(IIF(EMPTY(invhdr.store), 'M', 'S') + invhdr.account + ALLTRIM(invhdr.store), 'customer')
    IF gfseek(invhdr.invoice, 'invline')
     SELECT invline
     SCAN FOR totqty > 0 WHILE invoice = invhdr.invoice
      lnnexrate = IIF(style.cpricecur <> 'EUR', gfchkrate('lnUnit', style.cpricecur, DATE(), .F.), lneurexrate)
      SELECT (lcdatafile)
      APPEND BLANK
      REPLACE rtype WITH '1', invoice WITH lcinvoice, vendor WITH '', ccomcode WITH style.ccomcode, qty WITH invline.totqty, amount WITH ROUND((style.totcost * lneurexrate), 2), caddress6 WITH ALLTRIM(UPPER(apvendor.caddress6)), content WITH ALLTRIM(style.content1) + ' ' + ALLTRIM(style.content2)
      IF llrpprvend
       = lfcountries()
      ENDIF
     ENDSCAN
    ENDIF
   ENDIF
  ENDIF
 ENDSCAN
 llendinv = .F.
 SELECT (lcdatafile)
 SET RELATION TO invoice INTO invhdr
 SET RELATION ADDITIVE TO vendor INTO apvendor
 SELECT invhdr
 SET RELATION TO IIF(EMPTY(invhdr.store), 'M', 'S') + invhdr.account + ALLTRIM(invhdr.store) INTO customer
 SELECT (lcdatafile)
 DO gfdispre WITH EVALUATE('lcRpName')
 COPY TO ADDBS(oariaapplication.workdir) + 'lcDataFile'
ENDIF
ENDPROC
**
PROCEDURE lfwrepwhen
**
** ReFox - this procedure is empty **
**
ENDPROC
**
FUNCTION lfdate
PARAMETER lddate
RETURN STRTRAN(DTOC(lddate), '/', '.')
ENDFUNC
**
FUNCTION lfsqlerror
PARAMETER lcsql
lnfilehandle = FCREATE(ADDBS(oariaapplication.workdir) + 'arexpkwf.sql')
FPUTS(lnfilehandle, lcsql)
FCLOSE(lnfilehandle)
RETURN .F.
ENDFUNC
**
PROCEDURE lfCountries
PRIVATE lncuralias
lncuralias = SELECT(0)
SELECT (lctryfile)
LOCATE FOR ALLTRIM(UPPER(caddress6)) = ALLTRIM(UPPER(apvendor.caddress6))
IF !FOUND()
 APPEND BLANK
 REPLACE vendor WITH style.vendor, caddress6 WITH ALLTRIM(UPPER(apvendor.caddress6))
 SELECT (lcdatafile)
 APPEND BLANK
 REPLACE rtype WITH '2', invoice WITH invline.invoice, vendor WITH style.vendor
ENDIF
SELECT (lncuralias)
ENDPROC
**
PROCEDURE lfBuildTmp
DIMENSION latempstru[8, 18]
STORE '' TO latempstru
STORE 1 TO lnindex
latempstru[lnindex, 1] = 'rtype'
latempstru[lnindex, 2] = 'C'
latempstru[lnindex, 3] = 1
latempstru[lnindex, 4] = 0
lnindex = lnindex + 1
latempstru[lnindex, 1] = 'invoice'
latempstru[lnindex, 2] = 'C'
latempstru[lnindex, 3] = 6
latempstru[lnindex, 4] = 0
lnindex = lnindex + 1
latempstru[lnindex, 1] = 'vendor'
latempstru[lnindex, 2] = 'C'
latempstru[lnindex, 3] = 8
latempstru[lnindex, 4] = 0
lnindex = lnindex + 1
latempstru[lnindex, 1] = 'ccomcode'
latempstru[lnindex, 2] = 'C'
latempstru[lnindex, 3] = 20
latempstru[lnindex, 4] = 0
lnindex = lnindex + 1
latempstru[lnindex, 1] = 'Qty'
latempstru[lnindex, 2] = 'N'
latempstru[lnindex, 3] = 6
latempstru[lnindex, 4] = 0
lnindex = lnindex + 1
latempstru[lnindex, 1] = 'amount'
latempstru[lnindex, 2] = 'N'
latempstru[lnindex, 3] = 12
latempstru[lnindex, 4] = 2
lnindex = lnindex + 1
latempstru[lnindex, 1] = 'caddress6'
latempstru[lnindex, 2] = 'C'
latempstru[lnindex, 3] = 20
latempstru[lnindex, 4] = 0
lnindex = lnindex + 1
latempstru[lnindex, 1] = 'content'
latempstru[lnindex, 2] = 'C'
latempstru[lnindex, 3] = 40
latempstru[lnindex, 4] = 0
= gfcrttmp(lcdatafile, @latempstru, "INVOICE+rtype+vendor+ccomcode+caddress6", lcdatafile, .T.)
DIMENSION latempstru[2, 18]
STORE '' TO latempstru
STORE 1 TO lnindex
latempstru[lnindex, 1] = 'vendor'
latempstru[lnindex, 2] = 'C'
latempstru[lnindex, 3] = 6
latempstru[lnindex, 4] = 0
lnindex = lnindex + 1
latempstru[lnindex, 1] = 'caddress6'
latempstru[lnindex, 2] = 'C'
latempstru[lnindex, 3] = 20
latempstru[lnindex, 4] = 0
= gfcrttmp(lctryfile, @latempstru, , "", .T.)
ENDPROC
**
FUNCTION lfendgroup
llendinv = .T.
RETURN ''
ENDFUNC
**
PROCEDURE lfStartVend
DIMENSION lavendor[6, 1]
lavendor = ''
IF rtype = '2'
 lavendor[1] = apvendor.caddress1
 lavendor[2] = apvendor.caddress2
 lavendor[3] = apvendor.caddress3
 lavendor[4] = apvendor.caddress4
 lavendor[5] = apvendor.caddress5
 lavendor[5] = apvendor.caddress6
 = lfadrshift('laVendor')
ENDIF
ENDPROC
**
FUNCTION lfStartgroup
llendinv = .F.
DIMENSION lashipto[6, 1]
lashipto = ''
DIMENSION lasoldto[6, 1]
lasoldto = ''
lasoldto[1] = gfgetadr('CUSTOMER', '', '', '', 1, '2')
lasoldto[2] = gfgetadr('CUSTOMER', '', '', '', 2, '2')
lasoldto[3] = gfgetadr('CUSTOMER', '', '', '', 3, '2')
lasoldto[4] = gfgetadr('CUSTOMER', '', '', '', 4, '2')
lasoldto[5] = gfgetadr('CUSTOMER', '', '', '', 5, '2')
lasoldto[6] = gfgetadr('CUSTOMER', '', '', '', 6, '2')
= lfadrshift('laSoldTo')
lashipto[1] = 'VIM bv'
lashipto[2] = 'Alan Paine'
lashipto[3] = 'Kaldenkerkerweg 95B'
lashipto[4] = '5932 DA Tegelen'
lashipto[5] = 'The Netherlands'
lashipto[6] = ''
= lfadrshift('laShipTo')
RETURN ''
ENDFUNC
**
PROCEDURE lfAdrShift
PARAMETER lcarraynam
FOR lncount = 1 TO 6
 IF TYPE(lcarraynam + "[" + STR(lncount , 1) + "]") = "C" .AND. EMPTY(&lcarraynam.[lnCount])
  =ADEL(&lcarraynam , lncount)
  lncount = lncount - 1
 ENDIF
ENDFOR
FOR lncount = 1 TO ALEN(&lcarraynam)
 IF TYPE(lcarraynam + "[" + STR(lncount, 1) + "]") <> "C"
  &lcarraynam.[lnCount] = ''
 ENDIF
ENDFOR
ENDPROC
**
*** 
*** ReFox - all is not lost 
***
