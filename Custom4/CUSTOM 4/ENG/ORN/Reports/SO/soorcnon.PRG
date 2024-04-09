*C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[T20121230.0001]
*** 
*** Order Confirmation for Orn Clothing
***
 *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[Start]
 IF TYPE('lcXMLFileName') = 'C'
   oAriaEnvironment.REPORT.ccrpapersize = 'A4'
   oAriaEnvironment.REPORT.ccrorientation = 'P'
   = lfevalsegs()
 ELSE
 *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[END]
   loogscroll.ccrpapersize = 'A4'
   loogscroll.ccrorientation = 'P'
   IF loogscroll.llogfltch
      = lfevalsegs()
   ENDIF
 *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[Start]   
 ENDIF
 *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[END]
 PRIVATE lntargtfld, lcordkey, lnmaxsize, lcrltcolor, lcfabcolor
 STORE '' TO lctargtfl, lcemail, lcwebsite, lcpaspar, lcusername, lcuserphone
 STORE 1 TO lntargtfld, lnsorcefld
 lcrltcolor = SPACE(0)
 lcfabcolor = SPACE(0)
 lcpaspar = ' '
 lnmaxsize = 16
 lcemail = gfgetmemvar('M_CEMAIL')
 lcwebsite = gfgetmemvar('M_CWEBSITE')
 IF  .NOT. USED('SYUUSER_A')
    = gfopentable('SYUUSER', 'CUSER_ID', 'SH', 'SYUUSER_A')
 ENDIF
 IF  .NOT. USED('CONTACT')
    *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[Start]
    *= gfopentable(oariaapplication.datadir+'CONTACT', 'CONTACT', 'SH')
    = gfopentable('CONTACT', 'CONTACT', 'SH')
    *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[End]
 ENDIF
 = lfcrttmp()
 DIMENSION lalngdesc[1, 2]
 lalngdesc[1, 1] = 'CLRLNAME'
 lalngdesc[1, 2] = 'lcRltColor'
 SELECT ordhdr
 SET RELATION ADDITIVE TO order INTO (lcempltmp)
 SET RELATION ADDITIVE TO cordtype+order INTO (lctargtfl)
 SELECT (lctargtfl)
 SET FILTER TO totqty<>0
 SET RELATION ADDITIVE TO 'S'+SUBSTR(style, 1, lnmajorlen) INTO objlink_a
 SET RELATION ADDITIVE TO 'S'+scale INTO scale
 SET RELATION ADDITIVE TO style INTO style
 lcordkey = ''
 lccompdat = {}
 lnstylelen = lnmajorlen+lncolorlen+1
 lcindexkey = 'CORDTYPE + ORDER + STORE + SUBSTR(Style,1,lnStyleLen)'
 = lfaccmulte()
 = lfcoldata()
 SELECT lctemppik
 SET RELATION TO
 SELECT (lctempord)
 SET RELATION TO
 lcskipexpr  = [&lcTargtFl]
 SELECT ordhdr
 SET SKIP TO &lcskipexpr
 lcrpexp = lcrpexp+' AND !EOF(lcTempOrd)'
 *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[Start]
 IF TYPE('lcXMLFileName') = 'C'
   oAriaEnvironment.REPORT.OGLastForm= lcFormName
   oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm,'FOR '+lcrpexp)
   loProgress.Percent = 1.0
   loProgress.DESCRIPTION = "Printing Report..."
   loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
 ELSE
 *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[END]
    DO gfdispre WITH EVALUATE('lcFormName'), 'FOR '+lcrpexp
 *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[Start]
 ENDIF
 *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[END]
 llsalsord = .F.
 IF USED('CONTACT')
    gfclosetable('CONTACT')
 ENDIF
 RETURN
 
**
PROCEDURE lfcrttmp
 *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[Start]
 IF TYPE('lcXMLFileName') = 'C'
   lctargtfl = gftempname()
 ELSE
 *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[End]
   lctargtfl = loogscroll.gftempname()
 *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[Start]
 ENDIF
 *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[End]
 IF USED(lctargtfl) .AND. RECCOUNT(lctargtfl)>0
    USE IN (lctargtfl)
 ENDIF
 IF  .NOT. USED(lctargtfl)
    SELECT ordline
    = AFIELDS(latmpstru)
    lntmpstru = ALEN(latmpstru, 1)
    DIMENSION latmpstru[lntmpstru+17, 18]
    lni = 0
    lni = lni+1
    latmpstru[lntmpstru+lni, 1] = 'cSzesDesc'
    latmpstru[lntmpstru+lni, 2] = 'C'
    latmpstru[lntmpstru+lni, 3] = 105
    latmpstru[lntmpstru+lni, 4] = 0
    lni = lni+1
    latmpstru[lntmpstru+lni, 1] = 'cSzesVal'
    latmpstru[lntmpstru+lni, 2] = 'C'
    latmpstru[lntmpstru+lni, 3] = 105
    latmpstru[lntmpstru+lni, 4] = 0
    lni = lni+1
    latmpstru[lntmpstru+lni, 1] = 'cSzesQty'
    latmpstru[lntmpstru+lni, 2] = 'C'
    latmpstru[lntmpstru+lni, 3] = 105
    latmpstru[lntmpstru+lni, 4] = 0
    lni = lni+1
    latmpstru[lntmpstru+lni, 1] = 'nSzesTotal'
    latmpstru[lntmpstru+lni, 2] = 'N'
    latmpstru[lntmpstru+lni, 3] = 8
    latmpstru[lntmpstru+lni, 4] = 0
    lni = lni+1
    latmpstru[lntmpstru+lni, 1] = 'nSzToVl'
    latmpstru[lntmpstru+lni, 2] = 'N'
    latmpstru[lntmpstru+lni, 3] = 8
    latmpstru[lntmpstru+lni, 4] = 2
    lni = lni+1
    latmpstru[lntmpstru+lni, 1] = 'cColorDes'
    latmpstru[lntmpstru+lni, 2] = 'C'
    latmpstru[lntmpstru+lni, 3] = 50
    latmpstru[lntmpstru+lni, 4] = 0
    lni = lni+1
    latmpstru[lntmpstru+lni, 1] = 'cPURDESC'
    latmpstru[lntmpstru+lni, 2] = 'C'
    latmpstru[lntmpstru+lni, 3] = 30
    latmpstru[lntmpstru+lni, 4] = 0
    lni = lni+1
    latmpstru[lntmpstru+lni, 1] = 'cFABCOLOR'
    latmpstru[lntmpstru+lni, 2] = 'C'
    latmpstru[lntmpstru+lni, 3] = 50
    latmpstru[lntmpstru+lni, 4] = 0
    lni = lni+1
    latmpstru[lntmpstru+lni, 1] = 'lFIRSTREC'
    latmpstru[lntmpstru+lni, 2] = 'L'
    latmpstru[lntmpstru+lni, 3] = 1
    latmpstru[lntmpstru+lni, 4] = 0
    lni = lni+1
    latmpstru[lntmpstru+lni, 1] = 'StyGrop'
    latmpstru[lntmpstru+lni, 2] = 'C'
    latmpstru[lntmpstru+lni, 3] = 6
    latmpstru[lntmpstru+lni, 4] = 0
    lni = lni+1
    latmpstru[lntmpstru+lni, 1] = 'COLOR'
    latmpstru[lntmpstru+lni, 2] = 'C'
    latmpstru[lntmpstru+lni, 3] = 30
    latmpstru[lntmpstru+lni, 4] = 0
    lni = lni+1
    latmpstru[lntmpstru+lni, 1] = 'CDIM1'
    latmpstru[lntmpstru+lni, 2] = 'C'
    latmpstru[lntmpstru+lni, 3] = 5
    latmpstru[lntmpstru+lni, 4] = 0
    lni = lni+1
    latmpstru[lntmpstru+lni, 1] = 'INDEXDIM'
    latmpstru[lntmpstru+lni, 2] = 'C'
    latmpstru[lntmpstru+lni, 3] = 2
    latmpstru[lntmpstru+lni, 4] = 0
    lni = lni+1
    latmpstru[lntmpstru+lni, 1] = 'llPRNTSIZ'
    latmpstru[lntmpstru+lni, 2] = 'L'
    latmpstru[lntmpstru+lni, 3] = 1
    latmpstru[lntmpstru+lni, 4] = 0
    lni = lni+1
    latmpstru[lntmpstru+lni, 1] = 'lineType'
    latmpstru[lntmpstru+lni, 2] = 'C'
    latmpstru[lntmpstru+lni, 3] = 1
    latmpstru[lntmpstru+lni, 4] = 0
    lni = lni+1
    latmpstru[lntmpstru+lni, 1] = 'nextPO'
    latmpstru[lntmpstru+lni, 2] = 'C'
    latmpstru[lntmpstru+lni, 3] = 6
    latmpstru[lntmpstru+lni, 4] = 0
    lni = lni+1
    latmpstru[lntmpstru+lni, 1] = 'poDate'
    latmpstru[lntmpstru+lni, 2] = 'D'
    latmpstru[lntmpstru+lni, 3] = 8
    latmpstru[lntmpstru+lni, 4] = 0
    FOR lni = 1 TO ALEN(latmpstru, 1)-lntmpstru
       STORE .F. TO latmpstru[lntmpstru+lni, 5], latmpstru[lntmpstru+lni, 6]
       STORE '' TO latmpstru[lntmpstru+lni, 7], latmpstru[lntmpstru+lni, 8], latmpstru[lntmpstru+lni, 9], latmpstru[lntmpstru+lni, 10], latmpstru[lntmpstru+lni, 11], latmpstru[lntmpstru+lni, 12], latmpstru[lntmpstru+lni, 13], latmpstru[lntmpstru+lni, 14], latmpstru[lntmpstru+lni, 15], latmpstru[lntmpstru+lni, 16]
       STORE 0 TO latmpstru[lntmpstru+lni, 17], latmpstru[lntmpstru+lni, 18]
    ENDFOR
    = gfcrttmp(lctargtfl, @latmpstru)
 ENDIF
 SELECT (lctargtfl)
 IF lcrpsortby='S'
    INDEX ON cordtype+order+store+lineType+employee+style TAG (lctargtfl)
    SET ORDER TO (lctargtfl)
 ELSE
    INDEX ON cordtype+order+store+lineType+employee+STR(lineno, 6) TAG (lctargtfl)
    SET ORDER TO (lctargtfl)
 ENDIF
 IF USED(lcempltmp) .AND. RECCOUNT(lcempltmp)>0
    USE IN (lcempltmp)
 ENDIF
 IF  .NOT. USED(lcempltmp)
    lni = 1
    DIMENSION latempstru9[lni, 4]
    latempstru9[lni, 1] = 'ORDER'
    latempstru9[lni, 2] = 'C'
    latempstru9[lni, 3] = 6
    latempstru9[lni, 4] = 0
    lni = ALEN(latempstru9, 1)+1
    DIMENSION latempstru9[lni, 4]
    latempstru9[lni, 1] = 'EMPLCODE '
    latempstru9[lni, 2] = 'C'
    latempstru9[lni, 3] = 12
    latempstru9[lni, 4] = 0
    lni = ALEN(latempstru9, 1)+1
    DIMENSION latempstru9[lni, 4]
    latempstru9[lni, 1] = 'EMPLNAME'
    latempstru9[lni, 2] = 'C'
    latempstru9[lni, 3] = 30
    latempstru9[lni, 4] = 0
    lni = ALEN(latempstru9, 1)+1
    DIMENSION latempstru9[lni, 4]
    latempstru9[lni, 1] = 'EMPLSITE'
    latempstru9[lni, 2] = 'C'
    latempstru9[lni, 3] = 10
    latempstru9[lni, 4] = 0
    = gfcrttmp(lcempltmp, @latempstru9)
    SELECT (lcempltmp)
    INDEX ON order+emplcode TAG (lcempltmp)
 ENDIF
ENDPROC
**
FUNCTION lfevalsegs
 PARAMETER lcreturn
 *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[Start]
 IF TYPE('lcXMLFileName') = 'C'
   lcnonmajtl = ''
   lcnonmajpi = ''
   DIMENSION lamajsegs[1, 1]
   LOCAL loItemMask
   loItemMask = CREATEOBJECT("GetItemMask")
   lnmajseg = loItemMask.Do('SM')
   = loItemMask.Do(@lamajsegs)
 ELSE
 *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[End]
   lnmajseg = gfitemmask('SM')
   DIMENSION lamajsegs[1, 1]
   = gfitemmask(@lamajsegs)
   lcnonmajtl = ''
   lcnonmajpi = ''
   lnmajseg = gfitemmask('SM')
   DIMENSION lamajsegs[1, 1]
   = gfitemmask(@lamajsegs)
 *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[Start]
 ENDIF
 *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[End]
 FOR lni = lnmajseg+1 TO ALEN(lamajsegs, 1)
    IF lamajsegs(lni, 1)='C'
       lcfree_clr = lamajsegs(lni, 1)
       lnnonmajst = lamajsegs(lni, 4)
       lcnonmajpi = IIF(EMPTY(lcnonmajpi) .OR. lamajsegs(lni, 1)='C', lamajsegs(lni, 3), lcnonmajpi+lamajsegs(lni-1, 6)+lamajsegs(lni, 3))
       lcnonmajtl = IIF(EMPTY(lcnonmajtl) .OR. lamajsegs(lni, 1)='C', PADR(lamajsegs(lni, 2), LEN(lamajsegs(lni, 3))), lcnonmajtl+lamajsegs(lni-1, 6)+PADR(lamajsegs(lni, 2), LEN(lamajsegs(lni, 3))))
       EXIT
    ENDIF
 ENDFOR
 STORE LEN(lcnonmajpi) TO lnfreelen, lncolorlen
 lccolortt = 'Only These '+ALLTRIM(lcnonmajtl)+'s.'
 lcreturn = .T.
 RETURN lcreturn
ENDFUNC
**
FUNCTION lfgthedvar
 PARAMETER lcreturn, lcparm
 lcalias = ALIAS()
 llendgroup = .F.
 = lfgetlspadr()
 SELECT (lcalias)
 RETURN ''
ENDFUNC
**
PROCEDURE lfgetlspadr
 lnsavalias = SELECT(0)
 lcstore = &lctargtfl..STORE
 = lfgetcodes('CDIVISION', ordhdr.cdivision, 'DIVLNAME  ', 'lcDivLName')
 lcshipvia = gfcoddes(ordhdr.shipvia, 'SHIPVIA')
 lcseason = gfcoddes(ordhdr.season, 'SEASON')
 lcspcinst = gfcoddes(ordhdr.spcinst, 'SPCINST')
 lcterms = gfcoddes(ordhdr.ctermcode, 'CTERMCODE')
 lcusername = IIF(SEEK(ordhdr.cadd_user, 'SYUUSER_A'), syuuser_a.cusr_name, '')
 lcuserphone = syuuser_a.cusr_phon
 SELECT customer
 IF ordhdr.multi='Y'
    = SEEK('S' + &lctargtfl..account + &lctargtfl..STORE , "CUSTOMER")
    IF ALLTRIM(ordhdr.shipvia)='*'
       lcshipvia = gfcoddes(customer.shipvia, 'SHIPVIA')
    ENDIF
 ENDIF
 lcsoltname = btname
 lcshptname = IIF(ordhdr.alt_shpto, ordhdr.stname, IIF(EMPTY(dba), stname, dba))
 lccontactn = ordhdr.cordcontac
 lcuserphon = IIF(SEEK(gcuser_id, 'SYUUSER_A'), syuuser_a.cusr_phon, '')
 lasoldto[1] = gfgetadr('CUSTOMER', '', '', '', 1, '2')
 lasoldto[2] = gfgetadr('CUSTOMER', '', '', '', 2, '2')
 lasoldto[3] = gfgetadr('CUSTOMER', '', '', '', 3, '2')
 lasoldto[4] = gfgetadr('CUSTOMER', '', '', '', 4, '2')
 lasoldto[5] = gfgetadr('CUSTOMER', '', '', '', 5, '2')
 = lfadrshift('laSoldTo')
 IF ordhdr.alt_shpto
    SELECT ordhdr
    lcshptname = stname
    lashipto[1] = caddress1
    lashipto[2] = caddress2
    lashipto[3] = caddress3
    lashipto[4] = caddress4
    lashipto[5] = caddress5
 ELSE
    IF  .NOT. EMPTY(customer.dist_ctr)
       lccurrkey = 'S'+customer.account+customer.store
       = SEEK('S'+customer.account+customer.dist_ctr, 'CUSTOMER')
       lcstore = lcstore+'  Dist. Center : '+customer.store
    ENDIF
    lcshptname = IIF(EMPTY(dba), stname, dba)
    lashipto[1] = gfgetadr('CUSTOMER', '', '', '', 1)
    lashipto[2] = gfgetadr('CUSTOMER', '', '', '', 2)
    lashipto[3] = gfgetadr('CUSTOMER', '', '', '', 3)
    lashipto[4] = gfgetadr('CUSTOMER', '', '', '', 4)
    lashipto[5] = gfgetadr('CUSTOMER', '', '', '', 5)
    IF TYPE('lcCurrKey')='C'
       = SEEK(lccurrkey, 'CUSTOMER')
    ENDIF
 ENDIF
 = lfadrshift('laShipTo')
 SELECT (lnsavalias)
ENDPROC
**
PROCEDURE lfaccmulte
 PRIVATE lnslct
 lnslct = SELECT()
 SELECT (lctempord)
 *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[Start]
 IF TYPE('lcXMLFileName') = 'C'
   lcnewtmp =  gftempname()
   COPY TO (oAriaEnvironment.DataDir+lcnewtmp) STRUCTURE
   SELECT 0
   USE EXCLUSIVE (oAriaEnvironment.datadir+lcnewtmp)   
 ELSE
 *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[End]
   lcnewtmp = loogscroll.gftempname()
   COPY TO (oariaapplication.datadir+lcnewtmp) STRUCTURE   
   SELECT 0
   USE EXCLUSIVE (oariaapplication.datadir+lcnewtmp)
 *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[Start]
 ENDIF
 *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[End]

 INDEX ON cordtype + ORDER + STORE + STYLE + DTOS(COMPLETE) + STR(price,12,2) TAG &lcnewtmp 
 SELECT (lctempord)
 LOCATE
 SCAN
    SCATTER MEMO MEMVAR
    IF  .NOT. SEEK(cordtype+order+store+style+DTOS(complete)+STR(price, 12, 2), lcnewtmp)
       *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[Start]
       IF TYPE('lcXMLFileName') = 'C'
         INSERT INTO (oAriaEnvironment.datadir+lcnewtmp) FROM MEMVAR
       ELSE  
       *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[END]
         INSERT INTO (oariaapplication.datadir+lcnewtmp) FROM MEMVAR
       *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[Start]
       ENDIF
       *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[END]
    ELSE
       SELECT (lcnewtmp)
       REPLACE qty1 WITH qty1+m.qty1, qty2 WITH qty2+m.qty2, qty3 WITH qty3+m.qty3, qty4 WITH qty4+m.qty4, qty5 WITH qty5+m.qty5, qty6 WITH qty6+m.qty6, qty7 WITH qty7+m.qty7, qty8 WITH qty8+m.qty8, totqty with totqty+m.totqty
       REPLACE pik1 WITH pik1+m.pik1, pik2 WITH pik2+m.pik2, pik3 WITH pik3+m.pik3, pik4 WITH pik4+m.pik4, pik5 WITH pik5+m.pik5, pik6 WITH pik6+m.pik6, pik7 WITH pik7+m.pik7, pik8 WITH pik8+m.pik8, totpik with totpik+m.totpik
    ENDIF
 ENDSCAN
 SELECT (lctempord)
 ZAP
 USE IN &lcnewtmp
 *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[Start]
 IF TYPE('lcXMLFileName') = 'C'
   APPEND FROM (oAriaEnvironment.datadir+lcnewtmp)
   ERASE (oAriaEnvironment.datadir+lcnewtmp+'.DBF')
   ERASE (oAriaEnvironment.datadir+lcnewtmp+'.CDX')
   ERASE (oAriaEnvironment.datadir+lcnewtmp+'.FPT')
 ELSE
 *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[End]
   APPEND FROM (oariaapplication.datadir+lcnewtmp)
   ERASE (oariaapplication.datadir+lcnewtmp+'.DBF')
   ERASE (oariaapplication.datadir+lcnewtmp+'.CDX')
   ERASE (oariaapplication.datadir+lcnewtmp+'.FPT')
 *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[Start]
 ENDIF
 *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[End]
 SELECT (lnslct)
ENDPROC
**
PROCEDURE lfcoldata
 PRIVATE lcemplcode, lcemplname, lcemplsite, lnoldalias, lccolordsc, lcdimdesc, lcstyle, lcclrhld, lccolormaj
 STORE '' TO lcemplcode, lcemplname, lcemplsite, lccolordsc, lcdimdesc, lcstyle, lcclrhld
 STORE 0 TO lnoldalias
 SELECT (lctempord)
 GOTO TOP
 lcpurdesc = ''
 SCAN
    IF  .NOT. (SUBSTR(EVALUATE(lctempord+'.Style'), lnnonmajst, lncolorlen)==lcclrhld)
       lcclrhld = SUBSTR(EVALUATE(lctempord+'.Style'), lnnonmajst, lncolorlen)
       lcdimdesc = scale.cdim1
    ENDIF
    IF  .NOT. (style.cstymajor==lcstyle)
       lcstyle = style.cstymajor
       lcdimdesc = scale.cdim1
    ENDIF
    IF lntargtfld=1
       lcdimdesc = scale.cdim1
    ENDIF
    SCATTER MEMO MEMVAR
    = lfgetcodes('COLOR', SUBSTR(EVALUATE(lctempord+'.Style'), lnnonmajst, lncolorlen), 'CLRLNAME', 'lcRltColor')
    lccolordsc = gfcoddes(SUBSTR(EVALUATE(lctempord+'.Style'), lnnonmajst, lncolorlen), "COLOR")
    IF !EMPTY(&lctempord..employee)
       SELECT contact
       gfsetorder('CONTACT')
       lcemplcode = &lctempord..employee
       IF gfseek('C'+ PADR(&lctempord..account,8)+&lctempord..STORE,'CONTACT')
          LOCATE  REST WHILE cconttype+ccont_id+STORE+contact  = 'C'+ PADR(&lctempord..account,8)+&lctempord..STORE  FOR ccntctcode= &lctempord..employee
          IF FOUND()
             lcemplname = contact
             lcemplsite = site_no
          ELSE
             STORE '' TO lcemplname, lcemplsite
          ENDIF
       ELSE
          STORE '' TO lcemplname, lcemplsite
       ENDIF
       lnoldalias = SELECT(0)
       IF !SEEK(&lctempord..ORDER+&lctempord..employee,lcempltmp)
          SELECT (lcempltmp)
          APPEND BLANK
          REPLACE ORDER     WITH &lctempord..ORDER		, emplcode  WITH lcemplcode 				, emplname  WITH lcemplname 				, emplsite  WITH lcemplsite 
       ENDIF
       SELECT (lnoldalias)
    ENDIF
    IF lcordkey<>EVALUATE(lcindexkey) .OR. lccompdat<>EVALUATE(lctempord+'.COMPLETE')
       STORE 1 TO lntargtfld
       SELECT (lctargtfl)
       APPEND BLANK
       GATHER MEMO MEMVAR
       REPLACE ccolordes WITH lcrltcolor, cpurdesc WITH lcpurdesc, cfabcolor WITH lcfabcolor, lfirstrec WITH .T., llprntsiz WITH .T., stygrop WITH style.cstygroup, color WITH lccolordsc, cdim1 WITH lcdimdesc, lineType with '1'
       lnrecno = RECNO()
       LOCATE
       GOTO lnrecno
    ENDIF
    FOR lncount = 1 TO scale.cnt
       lcszqty = '.QTY'+ALLTRIM(STR(lncount))
       lcszdes = '.SZ'+ALLTRIM(STR(lncount))
       IF lntargtfld>lnmaxsize .OR.  .NOT. (scale.cdim1==lcdimdesc)
          IF lntargtfld=1 .OR.  .NOT. (scale.cdim1==lcdimdesc)
             lcdimdesc = scale.cdim1
          ENDIF
          STORE 1 TO lntargtfld
          SELECT (lctargtfl)
          APPEND BLANK
          GATHER MEMO MEMVAR
          REPLACE ccolordes WITH lcrltcolor, cpurdesc WITH lcpurdesc, cfabcolor WITH lcfabcolor, lfirstrec WITH .F., llprntsiz WITH .T., stygrop WITH style.cstygroup, color WITH lccolordsc, cdim1 WITH lcdimdesc, lineType with '1'
          lnrecno = RECNO()
          LOCATE
          GOTO lnrecno
       ENDIF
       lnszqty = EVALUATE(lctempord+lcszqty)
       IF lnszqty>0
          SELECT (lctargtfl)
          REPLACE cszesqty WITH RTRIM(cszesqty)+STR(lnszqty, 5), cszesdesc WITH RTRIM(cszesdesc)+PADL(ALLTRIM(EVALUATE('SCALE'+lcszdes)), 5, ' '), cszesval WITH RTRIM(cszesval)+STR(ROUND(EVALUATE(lctempord+'.PRICE'), 2), 6, 2), nszestotal WITH nszestotal+lnszqty, nsztovl WITH nsztovl+(lnszqty*EVALUATE(lctempord+'.PRICE'))
          lntargtfld = lntargtfld+1
       ENDIF
    ENDFOR
    SELECT (lctempord)
    STORE '' TO lcfabcolor, lcpurdesc
    lcordkey = EVALUATE(lcindexkey)
    lccompdat = EVALUATE(lctempord+'.COMPLETE')
 ENDSCAN
 SELECT (lctempord)
 SET RELATION TO
 SELECT cordtype, order, store, style, desc1, scale, price, sum(qty1) as sumqty1,  sum(qty2) as sumqty2, sum(qty3) as sumqty3, sum(qty4) as sumqty4, sum(qty5) as sumqty5, sum(qty6) as sumqty6, sum(qty7) as sumqty7, sum(qty8) as sumqty8, ;
 	sum(pik1) as sumpik1,  sum(pik2) as sumpik2, sum(pik3) as sumpik3, sum(pik4) as sumpik4, sum(pik5) as sumpik5, sum(pik6) as sumpik6, sum(pik7) as sumpik7, sum(pik8) as sumpik8 from (lctempord) where totpik<totqty ;
 	group by cordtype, order, store, style, desc1, scale, price order by cordtype, order, store, style, desc1, scale, price into cursor lcTempPik
 select lcTempPik
 if reccount()>0
   
	 IF  .NOT. USED('POSHDR')
     *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[Start]
     *= gfopentable(oariaapplication.datadir+'POSHDR', 'POSHDR', 'SH')
	    = gfopentable('POSHDR', 'POSHDR', 'SH')
     *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[End]
	 ENDIF
	 IF  .NOT. USED('POSLN')
      *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[Start]
      *= gfopentable(oariaapplication.datadir+'POSLN', 'POSLNS', 'SH')
	    = gfopentable('POSLN', 'POSLNS', 'SH')
      *C201546,1 MMT 12/31/2012 Modify Custom order confirmation form ON to work from RB[End]
	 ENDIF
	 select lcTempPik
	 SET RELATION ADDITIVE TO 'S'+scale INTO scale
	 SET RELATION ADDITIVE TO style INTO style
	 STORE '' TO lccolordsc, lcdimdesc, lcstyle, lcclrhld, lcpurdesc, lcordkey
	 SCAN
	    IF  .NOT. (SUBSTR(lcTempPik.Style, lnnonmajst, lncolorlen)==lcclrhld)
	       lcclrhld = SUBSTR(lcTempPik.Style, lnnonmajst, lncolorlen)
	       lcdimdesc = scale.cdim1
	    ENDIF
	    IF  .NOT. (style.cstymajor==lcstyle)
	       lcstyle = style.cstymajor
	       lcdimdesc = scale.cdim1
	    ENDIF
	    IF lntargtfld=1
	       lcdimdesc = scale.cdim1
	    ENDIF
	    SCATTER MEMO MEMVAR
	    = lfgetcodes('COLOR', SUBSTR(lcTempPik.Style, lnnonmajst, lncolorlen), 'CLRLNAME', 'lcRltColor')
	    lccolordsc = gfcoddes(SUBSTR(lcTempPik.Style, lnnonmajst, lncolorlen), "COLOR")
	    store space(0) to lcopenpo
	    store {} to ldavalbl
	    IF lcordkey<>EVALUATE(lcindexkey)
	       STORE 1 TO lntargtfld
	       SELECT (lctargtfl)
	       APPEND BLANK
	       GATHER MEMO MEMVAR
	       REPLACE ccolordes WITH lcrltcolor, cpurdesc WITH lcpurdesc, cfabcolor WITH lcfabcolor, lfirstrec WITH .T., llprntsiz WITH .T., stygrop WITH style.cstygroup, color WITH lccolordsc, cdim1 WITH lcdimdesc, lineType with '2'
		       IF lfopenpo()
		          REPLACE &lctargtfl..nextpo WITH lcopenpo , &lctargtfl..podate WITH ldavalbl
		       ENDIF
	       lnrecno = RECNO()
	       LOCATE
	       GOTO lnrecno
	    ENDIF
	    FOR lncount = 1 TO scale.cnt
	       lcszI = ALLTRIM(STR(lncount))
	       lcszdes = '.SZ'+ALLTRIM(STR(lncount))
	       IF lntargtfld>lnmaxsize .OR.  .NOT. (scale.cdim1==lcdimdesc)
	          IF lntargtfld=1 .OR.  .NOT. (scale.cdim1==lcdimdesc)
	             lcdimdesc = scale.cdim1
	          ENDIF
	          STORE 1 TO lntargtfld
	          SELECT (lctargtfl)
	          APPEND BLANK
	          GATHER MEMO MEMVAR
	          REPLACE ccolordes WITH lcrltcolor, cpurdesc WITH lcpurdesc, cfabcolor WITH lcfabcolor, lfirstrec WITH .F., llprntsiz WITH .T., stygrop WITH style.cstygroup, color WITH lccolordsc, cdim1 WITH lcdimdesc, lineType with '2'
		       IF lfopenpo()
		          REPLACE &lctargtfl..nextpo WITH lcopenpo , &lctargtfl..podate WITH ldavalbl
		       ENDIF
	          lnrecno = RECNO()
	          LOCATE
	          GOTO lnrecno
	       ENDIF
	       lnszqty = EVALUATE('lctempPIK.sumqty'+lcszI)
	       lnszpik = EVALUATE('lctempPIK.sumpik'+lcszI)
	       IF lnszpik<lnszqty
	          SELECT (lctargtfl)
	          REPLACE cszesqty WITH RTRIM(cszesqty)+STR(lnszqty-lnszpik, 5), cszesdesc WITH RTRIM(cszesdesc)+PADL(ALLTRIM(EVALUATE('SCALE'+lcszdes)), 5, ' '), cszesval WITH RTRIM(cszesval)+STR(ROUND(lctempPik.PRICE, 2), 6, 2), nszestotal WITH nszestotal+lnszqty-lnszpik, nsztovl WITH nsztovl+((lnszqty-lnszpik)*lctemppik.PRICE)
	          lntargtfld = lntargtfld+1
	       ENDIF
	    ENDFOR
	    SELECT lcTempPik
	    STORE '' TO lcfabcolor, lcpurdesc
	    lcordkey = EVALUATE(lcindexkey)
	 ENDSCAN
 endif	
ENDPROC
**
PROCEDURE lfgetcodes
 PARAMETER lcfld, lcvalue, lcfldname, lcreturn
 lcalias = ALIAS()
 SELECT codes
 SET ORDER TO Codes
 IF SEEK('N'+lcvalue+'Y'+lcfld)
    SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name='N'+lcvalue+'Y'+lcfld
       IF crltd_nam=lcfldname
          &lcreturn = crltd_vlu
       ENDIF
    ENDSCAN
 ENDIF
 SELECT (lcalias)
ENDPROC
**
*** 
**
FUNCTION lfopenpo
 PRIVATE lcalaspo, lcevalkypo, lcstycheck
 STORE SPACE(0) TO lcopenpo, lcstycheck
 STORE {} TO ldavalbl
 STORE .F. TO llrtrnvl
 lcalaspo = SELECT(0)
 SELECT posln
 lcevalkypo = EVALUATE(KEY())
 if gfseek('0001'+lctempPIK.STYLE,'POSLN','POSLNS')
	 SCAN REST WHILE cinvtype+STYLE+cbusdocu+cstytype+po+STR(LINENO,6)+trancd  = '0001'+lctempPIK.STYLE FOR  gfseek(cbusdocu+cstytype+po,'POSHDR','POSHDR') AND poshdr.STATUS $ "HO"
	    lcstycheck = posln.style
	    SKIP
	    IF lcstycheck==posln.style
	       IF trancd=ALLTRIM(STR(1))
	          SKIP -1
	       ENDIF
	    ELSE
	       SKIP -1
	       lcopenpo = poshdr.po
	       ldavalbl = poshdr.available+5
	       llrtrnvl = .T.
	       EXIT
	    ENDIF
	 ENDSCAN
 endif
 = SEEK(lcevalkypo)
 SELECT (lcalaspo)
 RETURN llrtrnvl
ENDFUNC
***
