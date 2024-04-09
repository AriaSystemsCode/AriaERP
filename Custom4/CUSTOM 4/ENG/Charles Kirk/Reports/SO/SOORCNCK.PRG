*C201597,1 SAB 11/03/2013 Modify Order Confirmation Form CK and Credit Memo Form CK to Run from RB [T20120824.0005]
*B610704,1 TMI 03/24/2014  - SO - error emailing order confirmations [T20140321.0008]

*** 
*** ReFox XII  #UK122952  David Stephenson  Aria Systems Ltd [VFP90]
***
 *C201597,1 SAB 11/03/2013 Modify Order Confirmation Form CK and Credit Memo Form CK to Run from RB [T20120824.0005][Start]
 *loogscroll.ccrpapersize = 'A4'
 *loogscroll.ccrorientation = 'P'
 IF TYPE('lcXMLFileName') <> 'O'
   loogscroll.ccrpapersize = 'A4'
   loogscroll.ccrorientation = 'P'
 ELSE
   oAriaEnvironment.Report.cCrPaperSize   = 'A4'
   oAriaEnvironment.Report.cCROrientation = 'P'
   
   =gfOpenTable('ORDHDR','ORDHDR','SH','ORDHDR')
   =gfOpenTable('ORDLINE','ORDLINST','SH','ORDLINE')
   =gfOpenTable('STYLE','STYLE','SH','STYLE')
   =gfOpenTable('SCALE','SCALE','SH','SCALE')
   =gfOpenTable('CUSTOMER','CUSTOMER','SH','CUSTOMER')
   =gfOpenTable('NOTEPAD','NOTEPAD','SH','NOTEPAD')
   =gfOpenTable('WAREHOUS','WAREHOUS','SH','WAREHOUS')
   =gfOpenTable('SYCCOMP','CCOMP_ID','SH','SYCCOMP')
   =gfOpenTable('CODES','CODES','SH','CODES')
   =gfOpenTable('OBJECTS','OBJECTID','SH','OBJECTS')
   =gfOpenTable('OBJLINK','OBJLNKTY','SH','OBJLINK')
   =gfOpenTable('OBJECTS','OBJECTID','SH','OBJECTS_A')
   =gfOpenTable('OBJLINK','OBJLNKTY','SH','OBJLINK_A')
   =gfOpenTable('SYCINT','CCONTCODE','SH','SYCINT') 
 ENDIF
 *C201597,1 SAB 11/03/2013 Modify Order Confirmation Form CK and Credit Memo Form CK to Run from RB [T20120824.0005][End]
 STORE 0 TO lncolorlen, lnnonmajst
 = lfevalsegs()
 PRIVATE lntargtfld, lcordkey, lnmaxsize, lcrltcolor, lcfabcolor
 STORE '' TO lctargtfl, lcemail, lcwebsite, lcpaspar, lcusername, lcuserphone
 STORE 1 TO lntargtfld, lnsorcefld
 lcrltcolor = SPACE(0)
 lcfabcolor = SPACE(0)
 lcpaspar = ' '
 lnmaxsize = 16
 lcemail = gfgetmemvar('M_CEMAIL')
 lcwebsite = gfgetmemvar('M_CWEBSITE')
 *B610704,1 TMI 03/24/2014 17:38 [Start] open the SYUUSER with another alias
 *IF  .NOT. USED('SYUUSER')
 *   = gfopentable('SYUUSER', 'CUSER_ID', 'SH')
 IF  .NOT. USED('SYUUSER_A')
    = gfopentable('SYUUSER', 'CUSER_ID', 'SH','SYUUSER_A')
    *B610704,1 TMI 03/24/2014 17:39 [End  ] 
 ENDIF
 IF  .NOT. USED('NOTEPAD')
    = gfopentable('NOTEPAD', 'NOTEPAD', 'SH')
 ENDIF
 IF  .NOT. USED('NOTELABEL')
    = gfopentable('NOTEPAD', 'NOTEPAD', 'SH', 'NOTELABEL')
 ENDIF
 IF  .NOT. USED('CONTACT')
    = gfopentable('CONTACT', 'CONTACT', 'SH')
 ENDIF
 *C201597,1 SAB 11/03/2013 Modify Order Confirmation Form CK and Credit Memo Form CK to Run from RB [T20120824.0005][Start]
 *lctargtfl = loogscroll.gftempname()
 *lcempltmp = loogscroll.gftempname()
 IF TYPE('lcXMLFileName') <> 'O'
   lctargtfl = loogscroll.gftempname()
   lcempltmp = loogscroll.gftempname()
 ELSE
   lctargtfl = gfTempName()
   lcempltmp = gfTempName()   
 ENDIF
 *C201597,1 SAB 11/03/2013 Modify Order Confirmation Form CK and Credit Memo Form CK to Run from RB [T20120824.0005][End]
 = lfcrttmp()
 IF  .NOT. USED('ORDDSGN')
    = gfopentable('ORDDSGN', 'ORDLINE', 'SH')
 ELSE
    SELECT (lctargtfl)
    SET RELATION OFF INTO orddsgn
 ENDIF
 IF  .NOT. USED('ARTWRKDS')
    = gfopentable('ARTWRKDS', 'ARTWRKDS', 'SH')
 ELSE
    SELECT orddsgn
    SET RELATION OFF INTO artwrkds
 ENDIF
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
 SET RELATION ADDITIVE TO order+STR(lineno, 6) INTO orddsgn
 SELECT orddsgn
 SET RELATION ADDITIVE TO cdesignid INTO artwrkds
 SELECT style
 SET RELATION ADDITIVE TO 'F'+cstymajor INTO notepad
 SET RELATION ADDITIVE TO 'T'+label_code INTO notelabel
 lcordkey = ''
 lccompdat = {}
 lnstylelen = lnmajorlen+lncolorlen+1
 lcindexkey = 'CORDTYPE + ORDER + STORE + SUBSTR(Style,1,lnStyleLen)'
 = lfaccmulte()
 = lfcoldata()
 SELECT (lctempord)
 SET RELATION TO
 IF llprtpick
    DELETE FROM (lctargtfl) WHERE EMPTY(piktkt) .OR. (piktkt='******')
    DELETE FROM (lctempord) WHERE EMPTY(piktkt) .OR. (piktkt='******')
 ENDIF
 lcskipexpr  = [&lcTargtFl]
 SELECT ordhdr
 SET SKIP TO &lcskipexpr
 lcrpexp = lcrpexp+' AND !EOF(lcTempOrd)'
 *C201597,1 SAB 11/03/2013 Modify Order Confirmation Form CK and Credit Memo Form CK to Run from RB [T20120824.0005][Start]
 *DO gfdispre WITH EVALUATE('lcFormName'), 'FOR '+lcrpexp
 IF TYPE('lcXMLFileName') <> 'O'
   DO gfdispre WITH EVALUATE('lcFormName'), 'FOR '+lcrpexp
 ELSE
   oAriaEnvironment.Report.cCROrientation = 'P'
   oAriaEnvironment.REPORT.OGLastForm = 'SOORCNCK'
   loProgress.DESCRIPTION = "Printing Report..."
   loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
   PRIVATE loProxy
   loProxy = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
   oAriaEnvironment.REPORT.PRINT(oAriaEnvironment.REPORT.OGLastForm)
 ENDIF
 *C201597,1 SAB 11/03/2013 Modify Order Confirmation Form CK and Credit Memo Form CK to Run from RB [T20120824.0005][End]
 llsalsord = .F.
 IF  .NOT. USED('CONTACT')
    gfclosetable('CONTACT')
 ENDIF
 RETURN
 
**
PROCEDURE lfcrttmp
 IF USED(lctargtfl) .AND. RECCOUNT(lctargtfl)>0
    USE IN (lctargtfl)
 ENDIF
 IF  .NOT. USED(lctargtfl)
    SELECT ordline
    = AFIELDS(latmpstru)
    lntmpstru = ALEN(latmpstru, 1)
    DIMENSION latmpstru[lntmpstru+16, 18]
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
    latmpstru[lntmpstru+lni, 1] = 'cPickQty'
    latmpstru[lntmpstru+lni, 2] = 'C'
    latmpstru[lntmpstru+lni, 3] = 105
    latmpstru[lntmpstru+lni, 4] = 0
    lni = lni+1
    latmpstru[lntmpstru+lni, 1] = 'nSzesTotal'
    latmpstru[lntmpstru+lni, 2] = 'N'
    latmpstru[lntmpstru+lni, 3] = 8
    latmpstru[lntmpstru+lni, 4] = 0
    lni = lni+1
    latmpstru[lntmpstru+lni, 1] = 'nPickTotal'
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
    FOR lni = 1 TO ALEN(latmpstru, 1)-lntmpstru
       STORE .F. TO latmpstru[lntmpstru+lni, 5], latmpstru[lntmpstru+lni, 6]
       STORE '' TO latmpstru[lntmpstru+lni, 7], latmpstru[lntmpstru+lni, 8], latmpstru[lntmpstru+lni, 9], latmpstru[lntmpstru+lni, 10], latmpstru[lntmpstru+lni, 11], latmpstru[lntmpstru+lni, 12], latmpstru[lntmpstru+lni, 13], latmpstru[lntmpstru+lni, 14], latmpstru[lntmpstru+lni, 15], latmpstru[lntmpstru+lni, 16]
       STORE 0 TO latmpstru[lntmpstru+lni, 17], latmpstru[lntmpstru+lni, 18]
    ENDFOR
    = gfcrttmp(lctargtfl, @latmpstru)
 ENDIF
 SELECT (lctargtfl)
 IF lcrpsortby='S'
    INDEX ON cordtype+order+store+employee+style TAG (lctargtfl)
    SET ORDER TO (lctargtfl)
 ELSE
    INDEX ON cordtype+order+store+employee+STR(lineno, 6) TAG (lctargtfl)
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
 *C201597,1 SAB 11/03/2013 Modify Order Confirmation Form CK and Credit Memo Form CK to Run from RB [T20120824.0005][Start]
 *lnmajseg = gfitemmask('SM')
 lnmajseg = 0
 IF TYPE('lcXMLFileName') <> 'O'
   lnmajseg = gfitemmask('SM')
 ELSE 
   LOCAL loItemMask
   loItemMask = CREATEOBJECT("GetItemMask")
   lnmajseg = loItemMask.Do('SM')
 ENDIF
 *C201597,1 SAB 11/03/2013 Modify Order Confirmation Form CK and Credit Memo Form CK to Run from RB [T20120824.0005][End]
 DIMENSION lamajsegs[1, 1]
 *C201597,1 SAB 11/03/2013 Modify Order Confirmation Form CK and Credit Memo Form CK to Run from RB [T20120824.0005][Start]
 *= gfitemmask(@lamajsegs)
 IF TYPE('lcXMLFileName') <> 'O'
   = gfitemmask(@lamajsegs)
 ELSE 
   LOCAL loItemMask
   loItemMask = CREATEOBJECT("GetItemMask")
   loItemMask.Do(@lamajsegs)
 ENDIF
 *C201597,1 SAB 11/03/2013 Modify Order Confirmation Form CK and Credit Memo Form CK to Run from RB [T20120824.0005][End]
 lcnonmajtl = ''
 lcnonmajpi = ''
 *C201597,1 SAB 11/03/2013 Modify Order Confirmation Form CK and Credit Memo Form CK to Run from RB [T20120824.0005][Start]
 *lnmajseg = gfitemmask('SM')
 IF TYPE('lcXMLFileName') <> 'O'
   lnmajseg = gfitemmask('SM')
 ELSE 
   LOCAL loItemMask
   loItemMask = CREATEOBJECT("GetItemMask")
   lnmajseg = loItemMask.Do('SM')
 ENDIF
 *C201597,1 SAB 11/03/2013 Modify Order Confirmation Form CK and Credit Memo Form CK to Run from RB [T20120824.0005][End]
 DIMENSION lamajsegs[1, 1]
 *C201597,1 SAB 11/03/2013 Modify Order Confirmation Form CK and Credit Memo Form CK to Run from RB [T20120824.0005][Start]
 *= gfitemmask(@lamajsegs)
 IF TYPE('lcXMLFileName') <> 'O'
   = gfitemmask(@lamajsegs)
 ELSE 
   LOCAL loItemMask
   loItemMask = CREATEOBJECT("GetItemMask")
   loItemMask.Do(@lamajsegs)
 ENDIF
 *C201597,1 SAB 11/03/2013 Modify Order Confirmation Form CK and Credit Memo Form CK to Run from RB [T20120824.0005][End]
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
 *B610704,1 TMI 03/24/2014 17:39 [Start] open the SYUUSER with another alias
 *lcusername = IIF(SEEK(ordhdr.cadd_user, 'SYUUSER'), syuuser.cusr_name, '')
 *lcuserphone = IIF(SEEK(ordhdr.cadd_user, 'SYUUSER'), syuuser.cusr_phon, '')
 lcusername = IIF(SEEK(ordhdr.cadd_user, 'SYUUSER_A'), SYUUSER_A.cusr_name, '')
 lcuserphone = IIF(SEEK(ordhdr.cadd_user, 'SYUUSER_A'), SYUUSER_A.cusr_phon, '')
 *B610704,1 TMI 03/24/2014 17:39 [End  ] 
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
 *B610704,1 TMI 03/24/2014 17:39 [Start]  open the SYUUSER with another alias
 *lcuserphon = IIF(SEEK(gcuser_id, 'SYUUSER'), syuuser.cusr_phon, '')
 lcuserphon = IIF(SEEK(gcuser_id, 'SYUUSER_A'), SYUUSER_A.cusr_phon, '')
 *B610704,1 TMI 03/24/2014 17:39 [End  ] 
 lasoldto[1] = gfgetadr('CUSTOMER', '', '', '', 1, '2')
 lasoldto[2] = gfgetadr('CUSTOMER', '', '', '', 2, '2')
 lasoldto[3] = gfgetadr('CUSTOMER', '', '', '', 3, '2')
 lasoldto[4] = gfgetadr('CUSTOMER', '', '', '', 4, '2')
 lasoldto[5] = gfgetadr('CUSTOMER', '', '', '', 5, '2')
 = lfadrshift('laSoldTo')
 lcsoldphone = customer.phone1
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
 lcshipphone = customer.phone1
 SELECT (lnsavalias)
ENDPROC
**
PROCEDURE lfaccmulte
 PRIVATE lnslct
 lnslct = SELECT()
 SELECT (lctempord)
 *C201597,1 SAB 11/03/2013 Modify Order Confirmation Form CK and Credit Memo Form CK to Run from RB [T20120824.0005][Start]
 *lcnewtmp = loogscroll.gftempname()
 IF TYPE('lcXMLFileName') <> 'O'
   lcnewtmp = loogscroll.gftempname()
 ELSE
   lcnewtmp = gfTempName()
 ENDIF
 *C201597,1 SAB 11/03/2013 Modify Order Confirmation Form CK and Credit Memo Form CK to Run from RB [T20120824.0005][End]
 COPY TO (oariaapplication.datadir+lcnewtmp) STRUCTURE
 SELECT 0
 USE EXCLUSIVE (oariaapplication.datadir+lcnewtmp)
 INDEX ON cordtype + ORDER + STORE + STYLE + DTOS(COMPLETE) + STR(price,12,2) TAG &lcnewtmp 
 SELECT (lctempord)
 LOCATE
 SCAN
    SCATTER MEMO MEMVAR
    IF  .NOT. SEEK(cordtype+order+store+style+DTOS(complete)+STR(price, 12, 2), lcnewtmp)
       INSERT INTO (oariaapplication.datadir+lcnewtmp) FROM MEMVAR
    ELSE
       SELECT (lcnewtmp)
       REPLACE qty1 WITH qty1+m.qty1, qty2 WITH qty2+m.qty2, qty3 WITH qty3+m.qty3, qty4 WITH qty4+m.qty4, qty5 WITH qty5+m.qty5, qty6 WITH qty6+m.qty6, qty7 WITH qty7+m.qty7, qty8 WITH qty8+m.qty8
    ENDIF
 ENDSCAN
 SELECT (lctempord)
 ZAP
 USE IN &lcnewtmp
 APPEND FROM (oariaapplication.datadir+lcnewtmp)
 ERASE (oariaapplication.datadir+lcnewtmp+'.DBF')
 ERASE (oariaapplication.datadir+lcnewtmp+'.CDX')
 ERASE (oariaapplication.datadir+lcnewtmp+'.FPT')
 SELECT (lnslct)
ENDPROC
**
PROCEDURE lfcoldata
 PRIVATE lcemplcode, lcemplname, lcemplsite, lnoldalias, lccolordsc, lcdimdesc, lcstyle, lcclrhld, lccolormaj, lcpicktkt, lcpikdate
 STORE '' TO lcemplcode, lcemplname, lcemplsite, lccolordsc, lcdimdesc, lcstyle, lcclrhld, lcpicktkt
 STORE 0 TO lnoldalias
 STORE {} TO lcpikdate
 SELECT (lctempord)
 GOTO TOP
 lcpurdesc = ''
 SCAN
    IF llprtpick
       IF EMPTY(&lctempord..piktkt) OR &lctempord..piktkt='******'
          LOOP
       ENDIF
    ENDIF
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
       SET ORDER TO CONTACT
       lcemplcode = &lctempord..employee
       IF SEEK('C'+ PADR(&lctempord..account,8)+&lctempord..STORE,'CONTACT')
          LOCATE  REST WHILE cconttype+ccont_id+STORE+contact  = 'C'+ PADR(&lctempord..account,8)+&lctempord..STORE  FOR ccontcode = &lctempord..employee
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
       REPLACE ccolordes WITH lcrltcolor, cpurdesc WITH lcpurdesc, cfabcolor WITH lcfabcolor, lfirstrec WITH .T., llprntsiz WITH .T., stygrop WITH style.cstygroup, color WITH lccolordsc, cdim1 WITH lcdimdesc
       lnrecno = RECNO()
       LOCATE
       GOTO lnrecno
    ENDIF
    FOR lncount = 1 TO scale.cnt
       lcszqty = '.QTY'+ALLTRIM(STR(lncount))
       lcpkqty = '.Pik'+ALLTRIM(STR(lncount))
       lcszdes = '.SZ'+ALLTRIM(STR(lncount))
       IF lntargtfld>lnmaxsize .OR.  .NOT. (scale.cdim1==lcdimdesc)
          IF lntargtfld=1 .OR.  .NOT. (scale.cdim1==lcdimdesc)
             lcdimdesc = scale.cdim1
          ENDIF
          STORE 1 TO lntargtfld
          SELECT (lctargtfl)
          APPEND BLANK
          GATHER MEMO MEMVAR
          REPLACE ccolordes WITH lcrltcolor, cpurdesc WITH lcpurdesc, cfabcolor WITH lcfabcolor, lfirstrec WITH .F., llprntsiz WITH .T., stygrop WITH style.cstygroup, color WITH lccolordsc, cdim1 WITH lcdimdesc
          lnrecno = RECNO()
          LOCATE
          GOTO lnrecno
       ENDIF
       lnszqty = EVALUATE(lctempord+lcszqty)
       lnpkqty = EVALUATE(lctempord+lcpkqty)
       IF lnszqty>0
          SELECT (lctargtfl)
          REPLACE cszesqty WITH RTRIM(cszesqty)+STR(lnszqty, 5), cpickqty WITH RTRIM(cpickqty)+STR(lnpkqty, 5), cszesdesc WITH RTRIM(cszesdesc)+PADL(ALLTRIM(EVALUATE('SCALE'+lcszdes)), 5, ' '), cszesval WITH RTRIM(cszesval)+STR(ROUND(EVALUATE(lctempord+'.PRICE'), 2), 6, 2), nszestotal WITH nszestotal+lnszqty, npicktotal WITH npicktotal+lnpkqty, nsztovl WITH nsztovl+(lnszqty*EVALUATE(lctempord+'.PRICE'))
          lntargtfld = lntargtfld+1
       ENDIF
    ENDFOR
    SELECT (lctempord)
    STORE '' TO lcfabcolor, lcpurdesc
    lcordkey = EVALUATE(lcindexkey)
    lccompdat = EVALUATE(lctempord+'.COMPLETE')
 ENDSCAN
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
FUNCTION lffillarry
 PARAMETER lcalias, lcarray, lnlen
 PRIVATE lni, lncount, lcarraycnt, lnmemoset
 lnmemoset = SET('MEMOWIDTH')
 SET MEMOWIDTH TO 97
 lncount = 1
 FOR lni = 1 TO MEMLINES(EVALUATE(lcalias+'.MNOTES'))
    IF  .NOT. EMPTY(MLINE(EVALUATE(lcalias+'.MNOTES'), lni))
       lcarraycnt = lcarray+'[lnCount]'
       IF (LEN(&lcarraycnt)+LEN(ALLTRIM(MLINE(EVALUATE(lcalias+'.MNOTES'),lni)))<110
          &lcarraycnt. =&lcarraycnt+ALLTRIM(MLINE(EVALUATE(lcalias+'.MNOTES'),lni))
       ELSE
          lncount = lncount+1
          lcarraycnt = lcarray+'[lnCount]'
          &lcarraycnt. = ALLTRIM(MLINE(EVALUATE(lcalias+'.MNOTES'),lni))
       ENDIF
       IF lncount=lnlen
          EXIT
       ENDIF
    ELSE
       EXIT
    ENDIF
 ENDFOR
 SET MEMOWIDTH TO lnmemoset
 RETURN ''
ENDFUNC
**
FUNCTION lfgetstynotes
 PRIVATE lni, lncount, lcarraycnt, lnmemoset
 IF TYPE("lastynotes", 1)<>'A'
    DIMENSION lastynotes[6]
 ENDIF
 STORE SPACE(0) TO lastynotes
 lnmemoset = SET('MEMOWIDTH')
 SET MEMOWIDTH TO 97
 lncount = 1
 FOR lni = 1 TO MEMLINES(notepad.mnotes)
    IF  .NOT. EMPTY(MLINE(notepad.mnotes, lni))
       IF (LEN(lastynotes(lncount))+LEN(ALLTRIM(MLINE(notepad.mnotes, lni))))<110
          lastynotes[lncount] = lastynotes(lncount)+' '+ALLTRIM(MLINE(notepad.mnotes, lni))
       ELSE
          lncount = lncount+1
          IF lncount>4
             EXIT
          ENDIF
          lastynotes[lncount] = ALLTRIM(MLINE(notepad.mnotes, lni))
       ENDIF
       IF lncount>4
          EXIT
       ENDIF
    ELSE
       EXIT
    ENDIF
 ENDFOR
 IF LEN(ALLTRIM(lastynotes(lncount)))>0
    lncount = lncount+1
 ENDIF
 IF lncount>6
    RETURN ''
 ENDIF
 FOR lni = 1 TO MEMLINES(notelabel.mnotes)
    IF  .NOT. EMPTY(MLINE(notelabel.mnotes, lni))
       IF (LEN(lastynotes(lncount))+LEN(ALLTRIM(MLINE(notelabel.mnotes, lni))))<110
          lastynotes[lncount] = lastynotes(lncount)+' '+ALLTRIM(MLINE(notelabel.mnotes, lni))
       ELSE
          lncount = lncount+1
          IF lncount>6
             RETURN ''
          ENDIF
          lastynotes[lncount] = ALLTRIM(MLINE(notelabel.mnotes, lni))
       ENDIF
       IF lncount>6
          EXIT
       ENDIF
    ELSE
       EXIT
    ENDIF
 ENDFOR
 SET MEMOWIDTH TO lnmemoset
 RETURN ''
ENDFUNC
**
*** 
*** ReFox - all is not lost 
***
