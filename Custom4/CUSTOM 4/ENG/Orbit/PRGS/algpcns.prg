*** 
*** ReFox XI+  #WA047676  Mariam  Mariam [VFP90]
***
DO FORM oariaapplication.screenhome+"\AL\alscnPk.scx"
ENDPROC
**
PROCEDURE lfInitForm
PARAMETER loformset
IF TYPE('loFormSet.lFrstTime') = 'U'
 loformset.addproperty('lFrstTime', .F.)
ENDIF
IF !loformset.lfrsttime
 loformset.lcscanned = gftempname()
 loformset.lctmppktk = gftempname()
 loformset.lctmpcur = gftempname()
 loformset.lcpcklin = gftempname()
 loformset.lcctnhdr = gftempname()
 loformset.lcctndtl = gftempname()
 loformset.lctmpdetfl = gftempname()
 loformset.lctmasnshp = gftempname()
 loformset.lcscafile = gftempname()
 loformset.lccartonsz = gftempname()
 loformset.lcprnasnshp = gftempname()
 loformset.lcasnlabel = gftempname()
 loformset.lcinvhdr = gftempname()
 loformset.lcinvline = gftempname()
 loformset.lcconsinvh = gftempname()
 loformset.lcconsinvd = gftempname()
 loformset.lcupsbox = gftempname()
 loformset.lcpackhdr = gftempname()
 loformset.lcinsthdr = gftempname()
 loformset.lcinstlin = gftempname()
 loformset.lcappcrdt = gftempname()
 loformset.lctempstk = gftempname()
 loformset.lcpackline = gftempname()
 loformset.lcordcanln = gftempname()
 loformset.lccrttmp = gftempname()
 loformset.lcupstmp = gftempname()
ENDIF
loformset.ariaform1.caption = 'Scan Pick Tickets'
= gfopentable('PIKTKT', 'PIKTKT')
= gfopentable('PACK_HDR', 'PACK_HDR')
= gfopentable('SeTups', 'MODVAR')
= gfopentable('Ordline', 'Ordline')
= gfopentable('Customer', 'Customer')
= gfopentable('Ordhdr', 'Ordhdr')
= gfopentable('ICISTRU', 'SEGNO')
= gfopentable('SyUUser', 'CUSER_ID', 'SH', 'USers')
= gfopentable('CUSTDEPT', 'CUSTDEPT', 'SH')
loformset.lcmask = gfitemmask("PM", '', '0001')
loformset.llextsizsc = gfgetmemvar('M_USEEXSSC')
loformset.llupcinst = ('UP' $ oariaapplication.companyinstalledmodules)
STORE 0 TO loformset.lnsizepos, loformset.lnsizelen
IF loformset.llextsizsc
 DIMENSION lastyseg[1, 1]
 STORE "" TO lastyseg
 = gfitemmask(@lastyseg)
 FOR lncnt = 1 TO ALEN(lastyseg, 1)
  IF lastyseg(lncnt, 1) = "S"
   loformset.lcsizesep = ALLTRIM(lastyseg(lncnt - 1, 6))
   loformset.lnsizepos = lastyseg(lncnt, 4) - IIF(!EMPTY(loformset.lcsizesep), 1, 0)
   loformset.lnsizelen = LEN(lastyseg(lncnt, 3)) + IIF(!EMPTY(loformset.lcsizesep), 1, 0)
  ENDIF
 ENDFOR
ENDIF
loformset.changemode("A")
IF !loformset.lfrsttime
 loformset.lfrsttime = .T.
 DIMENSION lafilestruct[1, 4]
 lafilestruct[1, 1] = 'ScPiktkt'
 lafilestruct[1, 2] = 'C'
 lafilestruct[1, 3] = 6
 lafilestruct[1, 4] = 0
 gfcrttmp(loformset.lcscanned, @lafilestruct, "ScPiktkt", loformset.lcscanned, .F.)
 DIMENSION lapkstruct[6, 4]
 lapkstruct[1, 1] = 'ACCOUNT'
 lapkstruct[1, 2] = 'C'
 lapkstruct[1, 3] = 5
 lapkstruct[1, 4] = 0
 lapkstruct[2, 1] = 'STORE'
 lapkstruct[2, 2] = 'C'
 lapkstruct[2, 3] = 8
 lapkstruct[2, 4] = 0
 lapkstruct[3, 1] = 'ORDER'
 lapkstruct[3, 2] = 'C'
 lapkstruct[3, 3] = 6
 lapkstruct[3, 4] = 0
 lapkstruct[4, 1] = 'PIKTKT'
 lapkstruct[4, 2] = 'C'
 lapkstruct[4, 3] = 6
 lapkstruct[4, 4] = 0
 lapkstruct[5, 1] = 'NCARTON'
 lapkstruct[5, 2] = 'N'
 lapkstruct[5, 3] = 6
 lapkstruct[5, 4] = 0
 lapkstruct[6, 1] = 'PIKQTY'
 lapkstruct[6, 2] = "N"
 lapkstruct[6, 3] = 6
 lapkstruct[6, 4] = 0
 gfcrttmp(loformset.lctmppktk, @lapkstruct, "ACCOUNT+STORE+ORDER+PIKTKT", loformset.lctmppktk, .F.)
ELSE
 SELECT (loformset.lctmppktk)
 ZAP
 SELECT (loformset.lcscanned)
 ZAP
ENDIF
lfaddcontrolsrc(loformset)
ENDPROC
**
PROCEDURE lfChngeMode
PARAMETER loformset
ENDPROC
**
FUNCTION lfvPiktkt
PARAMETER lcpiktkt, loformset
lnslct = SELECT()
llcontinue = .F.
IF !EMPTY(lcpiktkt)
 IF gfseek(lcpiktkt, 'PIKTKT')
  IF piktkt.status $ 'C'
   = gfmodalgen('INM00000B00000', .F., .F., .F., 'This Pick Ticket is completed, Can not be scanned')
   loformset.ariaform1.txtpiktkt.value = ''
   SELECT (lnslct)
   RETURN 0
  ELSE
   IF SEEK(lcpiktkt, loformset.lcscanned) .OR. piktkt.scanned
    lcmsg = 'Pick Ticket &lcPiktkt has already been scanned - please scan another Pick Ticket or Save '
    = gfmodalgen('INM00000B00000', .F., .F., .F., lcmsg)
    loformset.ariaform1.txtpiktkt.value = ''
    SELECT (lnslct)
    RETURN 0
   ENDIF
   IF gfseek(piktkt.order + piktkt.store, 'PACK_HDR')
    SELECT pack_hdr
    LOCATE REST FOR piktkt = piktkt.piktkt WHILE order + store + pack_no = piktkt.order + piktkt.store
    IF FOUND()
     SELECT (loformset.lcscanned)
     APPEND BLANK
     REPLACE scpiktkt WITH piktkt.piktkt
     lcmsg = 'Pick Ticket &lcPiktkt  has a related pack list created - please scan another Pick Ticket or Save '
     = gfmodalgen('INM00000B00000', .F., .F., .F., lcmsg)
     loformset.ariaform1.txtpiktkt.value = ''
     SELECT (lnslct)
     RETURN 0
    ENDIF
   ENDIF
  ENDIF
 ENDIF
ELSE
 RETURN
ENDIF
IF !llcontinue
 llcontinue = gfseek(lcpiktkt, 'PIKTKT') .AND. piktkt.status $ 'PO'
ENDIF
IF llcontinue
 GOTO TOP IN (loformset.lctmppktk)
 IF !EOF(loformset.lctmppktk)
  lctmppktk = loformset.lctmppktk
  IF piktkt.account <> &lctmppktk..account .OR. piktkt.STORE <> &lctmppktk..STORE
   WAIT WINDOW NOWAIT 'The piktkt &lcPiktkt has different Account/Store, can not accept' TIMEOUT 2
   SELECT (lnslct)
   RETURN 0
  ENDIF
 ENDIF
ENDIF
IF !llcontinue .AND. !EMPTY(lcpiktkt)
 llcontinue = lfpiktktbr()
ENDIF
lctmppktk = loformset.lctmppktk
IF llcontinue
 IF SEEK(piktkt.account + piktkt.store + piktkt.order + piktkt.piktkt, lctmppktk)
  WAIT WINDOW NOWAIT 'The piktkt &lcPiktkt is already scanned in the current batch.' TIMEOUT 2
 ELSE
  SELECT &lctmppktk 
  LOCATE
  IF EOF()
   llscanok = .T.
  ELSE
   llscanok = ( piktkt.account+piktkt.STORE = &lctmppktk..account+&lctmppktk..STORE )
  ENDIF
  IF llscanok
   APPEND BLANK
   REPLACE account WITH piktkt.account, store WITH piktkt.store, order WITH piktkt.order, piktkt WITH piktkt.piktkt, ncarton WITH 1
   loformset.ariaform1.txtcartons.value = 1
   = gfseek('O' + order, 'ORDLINE')
   SELECT ordline
   SUM totqty TO lntotpik  REST WHILE cordtype+ORDER+STR(LINENO,6) = 'O'+ &lctmppktk..ORDER  FOR piktkt = &lctmppktk..piktkt
   SELECT &lctmppktk 
   REPLACE pikqty WITH lntotpik
   loformset.ariaform1.grdpiktkt.refresh()
   loformset.ariaform1.cmdfinish.enabled = .T.
   loformset.ariaform1.txtpiktkt.value = ''
   = gfseek("M" + piktkt.account, 'Customer', 'Customer')
   loformset.ariaform1.cboshipvia.value = customer.shipvia
   RETURN 0
  ENDIF
 ENDIF
ENDIF
ENDFUNC
**
FUNCTION lfPiktktBr
PRIVATE lcbrfields, lcfile_ttl, labrow, lcfields, llreturn, lcbrfields, lnslct, lcforcond
lnslct = SELECT()
DIMENSION labrow[7]
STORE SPACE(0) TO labrow
lcfields = 'PikTkt,Order,Store,Date,Account'
lcbrfields = "PikTkt:H='PikTkt'  ," + "Account:H='Account'," + "Store :H='Store'   ," + "Order :H='Order#'  ," + "Date:H='PikDate'    "
lcfile_ttl = 'Pick Tickets'
lcpiktktfile = loformset.lctmppktk
GOTO TOP IN (loformset.lctmppktk)
IF EOF()
 lcforcond = ''
ELSE
 lcforcond = &lcpiktktfile..account + &lcpiktktfile..STORE
ENDIF
SELECT piktkt
= gfseek("")
LOCATE
IF !EMPTY(lcforcond)
 lcbrcond = "'' FOR PikTkt.Account + PikTkt.STORE = LCFORCond AND PikTkt.Status $ 'PO' AND PIKTKT.SCANNED=.F."
ELSE
 lcbrcond = "'' FOR PikTkt.Status $ 'PO' AND PIKTKT.SCANNED=.F."
ENDIF
llreturn = ariabrow(lcbrcond, lcfile_ttl, gnbrfsrow1, gnbrfscol1, gnbrfsrow2, gnbrfscol2, .F., .F., lcfields, 'laBrow', .F., 'PikTkt', .F.)
loformset.ariaform1.txtpiktkt.value = labrow(1)
SELECT (lnslct)
RETURN llreturn
ENDFUNC
**
PROCEDURE lfAddControlSrc
PARAMETER loformset
loformset.ariaform1.grdpiktkt.recordsource = ''
loformset.ariaform1.grdpiktkt.recordsource = loformset.lctmppktk
loformset.ariaform1.grdpiktkt.column1.controlsource = loformset.lctmppktk + '.Account'
loformset.ariaform1.grdpiktkt.column2.controlsource = loformset.lctmppktk + '.Store'
loformset.ariaform1.grdpiktkt.column3.controlsource = loformset.lctmppktk + '.Order'
loformset.ariaform1.grdpiktkt.column4.controlsource = loformset.lctmppktk + '.PIKTKT'
loformset.ariaform1.grdpiktkt.column5.controlsource = loformset.lctmppktk + '.PIKQTY'
loformset.ariaform1.grdpiktkt.column6.controlsource = loformset.lctmppktk + '.NCARTON'
ENDPROC
**
PROCEDURE lfUndo
PARAMETER loformset
loformset.preferencename = ""
lfinitform(loformset)
IF TYPE("loFormset.ariaForm1.ariaPAGEFRAME1.page4.cntInvoicesummary.ChargesFile") = 'U'
 loformset.ariaform1.ariapageframe1.page4.cntinvoicesummary.addproperty('ChargesFile', "")
ENDIF
ENDPROC
**
PROCEDURE lfSavePktkt
PARAMETER loformset
lctmppktk = loformset.lctmppktk
SELECT &lctmppktk
LOCATE
SCAN
 =gfseek(&lctmppktk..piktkt,'PIKTKT')
 SELECT piktkt
 gfreplace("SCANNED WITH .T.")
ENDSCAN
SELECT piktkt
= gftableupdate()
= lfcrtpack(loformset)
= lfupdent()
GO TOP IN &lctmppktk
=gfseek('M'+&lctmppktk..account,'CUSTOMER')
IF customer.consol = 'N'
 = lfcrtinvs()
ENDIF
SELECT ordline
= gftableupdate()
SELECT piktkt
= gftableupdate()
= lfprnpack()
loformset.clearmemory()
ENDPROC
**
FUNCTION LFBEFORSAV
PARAMETER loformset
lctmppktk = loformset.lctmppktk
SELECT &lctmppktk
LOCATE
SCAN
 =gfseek('O'+&lctmppktk..ORDER,'ORDHDR')
 PRIVATE lnsyuserre, lclok_user
 lclok_user = ""
 IF ordhdr.llok_stat
  lnsyuserre = IIF(RECNO('Users') > RECCOUNT('Users'), 0, RECNO('Users'))
  IF gfseek(ordhdr.clok_user, "USers")
   lclok_user = users.cuser_id
   lclok_user = IIF(EMPTY(lclok_user), oariaapplication.user_id, lclok_user)
  ENDIF
  IF gfmodalgen('INM40182B00000', 'ALERT', 'Order #' + ordhdr.order + '|' + lclok_user) = 1
   RETURN .F.
  ENDIF
 ENDIF
 lcorderno = ordhdr.order
 IF ordhdr.status = 'C'
  = gfmodalgen('TRM40109B00000', 'ALERT', lcorderno)
  RETURN .F.
 ENDIF
 IF ordhdr.status = 'B'
  = gfmodalgen('TRM40155B00000', 'ALERT', lcorderno)
  RETURN .F.
 ENDIF
 IF !gfseek('M' + ordhdr.account, 'Customer')
  = gfmodalgen('TRM40112B00000', 'ALERT', 'Account ' + ordhdr.account)
  RETURN .F.
 ENDIF
 IF customer.status <> 'A'
  = gfmodalgen('TRM40113B00000', 'ALERT', 'Account ' + ordhdr.account)
  RETURN .F.
 ENDIF
 lcstoreno = &lctmppktk..STORE
 IF !EMPTY(lcstoreno) .AND. !gfseek('S' + ordhdr.account + lcstoreno, 'Customer')
  = gfmodalgen('TRM40112B00000', 'ALERT', 'Store ' + ALLTRIM(lcstoreno))
  RETURN .F.
 ENDIF
 IF customer.status <> 'A'
  = gfmodalgen('TRM40113B00000', 'ALERT', 'Store ' + ALLTRIM(lcstoreno))
  RETURN .F.
 ENDIF
 lcorderno = &lctmppktk..ORDER 
 IF !gfseek('O' + lcorderno, 'ordline')
  = gfmodalgen('TRM40114B00000', 'ALERT', lcorderno)
  RETURN .F.
 ENDIF
 IF ordhdr.bulk = 'Y' .AND. gfgetmemvar('M_INVBULK') = 'N'
  = gfmodalgen('TRM40151B00000', 'ALERT', lcorderno)
  RETURN (.F.)
 ENDIF
 IF ordhdr.status = 'H'
  IF gfgetmemvar('M_INVHOLD') = 'Y'
   IF gfmodalgen('QRM40111B40003', 'ALERT', lcorderno + '| ') = 2
    RETURN (.F.)
   ENDIF
  ELSE
   = gfmodalgen('TRM40111B00000', 'ALERT', lcorderno + '| Cannot Ship.')
   RETURN (.F.)
  ENDIF
 ENDIF
ENDSCAN
ENDFUNC
**
PROCEDURE LFCRTPACK
PARAMETER loformset
PRIVATE lastru, lctmpcur, lcpcklin, lcsvpklnor, lntotpik, lni
= gfopentable('INVHDR', 'INVHDR', 'SH')
= gfopentable('INVLINE', 'INVLINE', 'SH')
= gfopentable('Style', 'Style', 'SH')
= gfopentable('Scale', 'Scale', 'SH')
= gfopentable('STYDYE', 'STYDYE', 'SH')
IF !USED('PACK_HDR')
 = gfopentable('PACK_HDR', 'PACK_HDR', 'SH')
ENDIF
SELECT pack_hdr
= gfsetorder('PACK_HDR')
IF !USED('PACK_LIN')
 = gfopentable('PACK_LIN', 'PACK_LIN', 'SH')
ENDIF
lctmpcur = loformset.lctmpcur
lcpcklin = loformset.lcpcklin
SELECT &lctmppktk
LOCATE
=gfseek('M'+&lctmppktk..account,'CUSTOMER')
SCAN
 WAIT WINDOW NOWAIT 'produce and print a Packing List ' + piktkt
 =gfseek('O'+&lctmppktk..ORDER,'ORDHDR')
 =gfseek('O'+&lctmppktk..ORDER,'ORDLINE') 
 lcsvpklnor = ORDER('PACK_LIN')
 SELECT pack_lin
 = gfsetorder('PACKSTYLE')
 SELECT pack_hdr
 IF !gfseek(&lctmppktk..piktkt,'PACK_HDR')
  gfappend("")
 ENDIF
 gfreplace([PACK_NO    WITH &lcTmpPkTk..PIKTKT,]+ [PIKTKT     WITH &lcTmpPkTk..PIKTKT,]+ [ORDER      WITH &lcTmpPkTk..ORDER,]+ [ACCOUNT    WITH &lcTmpPkTk..ACCOUNT,]+ [STORE      WITH &lcTmpPkTk..STORE,]+ [TOT_CART   WITH 1,]+ [SHIPVIA    WITH ORDHDR.SHIPVIA,]+ [CTOSTORCN  WITH 'S',]+ [CWARECODE  WITH ORDHDR.CWARECODE,]+ [CONSGMENT  WITH '',]+ [WEIGHTDL   WITH 0 ,]+ [CCRTNVLTYP WITH '',]+ [PICKEDBY   WITH '',]+ [CHECKEDBY  WITH '',]+ [LCARRCTNID WITH .F.,]+ [DSHIPDATE  WITH  oAriaApplication.SystemDate])
 gfreplace('NOOFCARTON WITH loFormSet.ariaform1.txtCartons.VAlue')
 gfreplace('CONSGMENT  WITH loFormSet.ariaform1.txtCongNo.value')
 gfreplace('PICKEDBY WITH loFormSet.ariaform1.cboPick.Displayvalue,' + 'CHECKEDBY WITH loFormSet.ariaform1.cboPack.Displayvalue,' + 'CARRIER WITH loFormSet.ariaform1.cboShipVia.Displayvalue')
 = gfadd_info('PACK_HDR')
 = gfreplace("")
 IF gfseek(&lctmppktk..piktkt,'PACK_LIN')
  SELECT pack_lin
  SCAN REST WHILE pack_no = &lctmppktk..piktkt
   gfdelete()
  ENDSCAN
 ENDIF
 SELECT ordline
 = AFIELDS(lastru)
 = gfcrttmp(lctmpcur, @lastru)
 SELECT pack_lin
 = AFIELDS(lastru)
 = gfcrttmp(lcpcklin, @lastru, "STR(NO_CART,4)+STYLE+STR(NORDLINENO,6)", lcpcklin)
 SELECT ordline
 lntotpik = 0
 SCAN REST WHILE cordtype+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+&lctmppktk..ORDER  FOR piktkt  = &lctmppktk..piktkt .AND.  account = &lctmppktk..account .AND.  STORE   = &lctmppktk..STORE
  = gfseek(style, 'Style', 'Style')
  gfreplace('NPCK1 WITH PIK1,' + 'NPCK2 WITH PIK2,' + 'NPCK3 WITH PIK3,' + 'NPCK4 WITH PIK4,' + 'NPCK5 WITH PIK5,' + 'NPCK6 WITH PIK6,' + 'NPCK7 WITH PIK7,' + 'NPCK8 WITH PIK8,' + 'NPWGHT WITH STYLE.NSTYWEIGHT*TOTPIK')
  SCATTER MEMVAR
  INSERT INTO &lctmpcur FROM MEMVAR
  lntotpik = lntotpik + m.totpik
 ENDSCAN
 lncrtqty = INT(lntotpik/&lctmppktk..ncarton)
 SELECT &lctmpcur
 GOTO TOP
 DO WHILE lntotpik>0
  WAIT WINDOW NOWAIT 'Creating cartons distribution for packing list : ' + &lctmppktk..piktkt 
  lnsz = 1
  FOR lncarton = 1 TO &lctmppktk..ncarton
   FOR lni = 1 TO lncrtqty
    SELECT &lctmpcur
    lcsz = STR(lnsz, 1)
    DO WHILE EVALUATE(lctmpcur+'.PIK'+lcsz)=0
     lnsz = lnsz + 1
     IF lnsz > 8
      lnsz = 1
      SKIP IN &lctmpcur
      IF EOF(lctmpcur)
       GO TOP IN &lctmpcur
      ENDIF
     ENDIF
     lcsz = STR(lnsz, 1)
    ENDDO
    IF !SEEK(STR(lncarton,4)+&lctmpcur..STYLE+STR(&lctmpcur..LINENO,6),lcpcklin)
     =gfseek(&lctmpcur..STYLE , 'STYLE' )
     =gfseek(&lctmpcur..STYLE+&lctmpcur..cwarecode , 'STYDYE' )
     SELECT pack_hdr
     gfreplace('NLASTLNO WITH NLASTLNO+1')
     SELECT &lcpcklin
     APPEND BLANK
     REPLACE pack_no    WITH &lctmppktk..piktkt, line_no    WITH pack_hdr.nlastlno, STYLE      WITH &lctmpcur..STYLE, no_cart    WITH lncarton, nordlineno WITH &lctmpcur..LINENO 
     = gfadd_info(lcpcklin)
    ENDIF
    REPLACE &lctmpcur..pik&lcsz WITH &lctmpcur..pik&lcsz - 1 
    REPLACE &lcpcklin..qty&lcsz WITH &lcpcklin..qty&lcsz + 1
    lntotpik = lntotpik - 1
    IF lntotpik = 0
     EXIT
    ENDIF
   ENDFOR
   IF lntotpik = 0
    EXIT
   ENDIF
  ENDFOR
 ENDDO
 SELECT &lcpcklin
 GOTO TOP
 SCAN
  = gfseek(style, 'Style', 'Style')
  REPLACE totqty WITH qty1 + qty2 + qty3 + qty4 + qty5 + qty6 + qty7 + qty8, weight WITH style.nstyweight * totqty
  SCATTER MEMO MEMVAR
  SELECT pack_lin
  APPEND BLANK
  GATHER MEMO MEMVAR
  gfreplace('')
 ENDSCAN
 DIMENSION lasums[3]
 lasums = 0
 SELECT SUM(totqty), SUM(weight), MAX(no_cart) FROM (lcpcklin) INTO ARRAY lasums
 SELECT pack_hdr
 gfreplace('TOT_PCS WITH LASUMS(1),' + 'TOT_WGHT WITH LASUMS(2),' + 'NLASTCART WITH LASUMS(3)')
 SELECT pack_lin
 = lfsavcartn()
ENDSCAN
USE IN (lctmpcur)
USE IN (lcpcklin)
SELECT pack_lin
= gftableupdate()
SELECT pack_hdr
= gftableupdate()
SELECT style
= gftableupdate()
SELECT stydye
= gftableupdate()
SELECT asn_ship
= gftableupdate()
ENDPROC
**
PROCEDURE LFSAVCARTN
PRIVATE lcscfields, ladata, lnln
lcscfields = 'Pack_No, Order, Piktkt, Account, Store, Note, ShipVia,' + 'Tot_Wght, Tot_Cart, Tot_Pcs, Sp_Inst1, Sp_Inst2, LStandCtn,' + 'CToStorCn, CPkChCode, CPkDsCode'
lnln = OCCURS(',', lcscfields) + 1
DIMENSION ladata[lnln]
ladata[1] = &lctmppktk..piktkt
ladata[2] = &lctmppktk..ORDER
ladata[3] = &lctmppktk..piktkt
ladata[4] = &lctmppktk..account
ladata[5] = &lctmppktk..STORE
lcctnhdr = loformset.lcctnhdr
lcctndtl = loformset.lcctndtl
lctmpdetfl = loformset.lctmpdetfl
lctmasnshp = loformset.lctmasnshp
lcscafile = loformset.lcscafile
lccartonsz = loformset.lccartonsz
lcprnasnshp = loformset.lcprnasnshp
lcasnlabel = loformset.lcasnlabel
lcsndport = "COM2"
= lfcrtndtl()
IF !USED('WAREHOUS')
 = gfopentable('WAREHOUS', 'WAREHOUS', 'SH')
ENDIF
IF !USED('SPCK_LIN')
 = gfopentable('SPCK_LIN', 'SPCKLINS', 'SH')
ENDIF
llediacc = .F.
lledisys = ('AS' $ oariaapplication.companyinstalledmodules)
IF lledisys
 IF !USED('EDIACPRT')
  = gfopentable('EDIACPRT', 'ACCFACT', 'SH')
 ENDIF
 IF !USED('EDIPH')
  = gfopentable('EDIPH', 'PARTNER', 'SH')
 ENDIF
 llediacc = lledisys .AND. gfseek('A' + ladata(4), 'EDIACPRT') .AND. gfseek(ediacprt.cpartcode, 'EDIPH')
ENDIF
DIMENSION laflstru[1, 4]
laflstru[1, 1] = 'SZCODE'
laflstru[1, 2] = 'C'
laflstru[1, 3] = 1
laflstru[1, 4] = 0
= gfcrttmp(lccartonsz, @laflstru, 'SZCODE', lccartonsz)
FOR lni = 1 TO 8
 INSERT INTO (lccartonsz) (szcode) VALUES (STR(lni, 1))
ENDFOR
STORE 0 TO lnnonmajst, lncolorlen, lnmajseg, lnmajlen
STORE '' TO lcfree_clr
= lfevalsegs()
= gfopentable('Asn_Ship', 'Asn_Ship', 'SH')
SELECT 'Asn_Ship'
IF !USED(lctmasnshp)
 COPY TO (oariaapplication.workdir + lctmasnshp) STRUCTURE
 = gfopenfile(oariaapplication.workdir + lctmasnshp, '', 'EX')
 INDEX ON STR(cart_no, 6) TAG (lctmasnshp)
ENDIF
= gfopentable(oariaapplication.syspath + 'SYCASNLB', 'ASNlbl', 'SH')
= gfopentable(oariaapplication.syspath + 'SYCASNHD', 'VerPrt', 'SH')
= gfopentable(oariaapplication.syspath + 'SYCASNDT', 'CVer', 'SH')
SELECT sycasnhd
= gfseek("")
LOCATE FOR sycasnhd.ldetlabel = .T.
IF FOUND()
 lldetlabel = .T.
ENDIF
llupcinst = ('UP' $ oariaapplication.companyinstalledmodules)
lcdetlball = ''
IF llupcinst
 = gfopentable('STYLEUPC', 'STYLEUPC', 'SH')
ENDIF
PRIVATE lncartons, lncarref, lnactalias, lcdetorder
lnactalias = SELECT(0)
STORE SPACE(0) TO lasource, latarget
lncartons = 0
GOTO BOTTOM IN (lcctndtl)
lncartons = &lcctndtl..cart_no
IF lncartons <> 0
 DIMENSION lasource[lncartons], latarget[1]
ENDIF
DIMENSION latarget[1]
lncarref = 0
lcdetorder = ORDER(lcctndtl)
SET ORDER IN (lcctndtl) TO (lcctndtl)
FOR lncarref = 1 TO lncartons
 = SEEK(STR(lncarref, 4), lcctndtl) .AND. lfvlblinfo(lncarref)
 lasource[lncarref] =  "Carton # " + PADL(ALLTRIM(STR(&lcctndtl..cart_no)),4) 
 IF SEEK(STR(lncarref, 6), lctmasnshp)
  SELECT (lctmasnshp)
  SCATTER MEMO MEMVAR
  IF gfseek(&lctmasnshp..bol_no+&lctmasnshp..pack_no+STR(&lctmasnshp..cart_no,6),"Asn_Ship")
   SELECT asn_ship
   GATHER MEMO MEMVAR
   = gfreplace('')
  ELSE
   INSERT INTO Asn_Ship FROM MEMVAR
   = gfreplace("")
  ENDIF
 ENDIF
ENDFOR
IF USED(lcctnhdr)
 USE IN &lcctnhdr
ENDIF
ERASE (oariaapplication.workdir + lcctnhdr + '.DBF')
ERASE (oariaapplication.workdir + lcctnhdr + '.CDX')
IF USED(lcctndtl)
 USE IN &lcctndtl
ENDIF
ERASE (oariaapplication.workdir + lcctndtl + '.DBF')
ERASE (oariaapplication.workdir + lcctndtl + '.CDX')
IF USED(lctmpdetfl)
 USE IN &lctmpdetfl
ENDIF
ERASE (oariaapplication.workdir + lctmpdetfl + '.DBF')
ERASE (oariaapplication.workdir + lctmpdetfl + '.CDX')
IF USED(lctmasnshp)
 USE IN &lctmasnshp
ENDIF
ERASE (oariaapplication.workdir + lctmasnshp + '.DBF')
ERASE (oariaapplication.workdir + lctmasnshp + '.CDX')
IF USED(lccartonsz)
 USE IN &lccartonsz
ENDIF
ERASE (oariaapplication.workdir + lccartonsz + '.DBF')
ERASE (oariaapplication.workdir + lccartonsz + '.CDX')
SELECT (lnactalias)
ENDPROC
**
PROCEDURE LFCRTNDTL
PRIVATE lncuralias, lntotqty
lncuralias = SELECT(0)
DIMENSION lafilestru[5, 4]
lafilestru[1, 1] = 'Cart_No'
lafilestru[1, 2] = 'N'
lafilestru[1, 3] = 4
lafilestru[1, 4] = 0
lafilestru[2, 1] = 'Pal_No'
lafilestru[2, 2] = 'N'
lafilestru[2, 3] = 4
lafilestru[2, 4] = 0
lafilestru[3, 1] = 'TotPcs'
lafilestru[3, 2] = 'N'
lafilestru[3, 3] = 7
lafilestru[3, 4] = 0
lafilestru[4, 1] = 'TotWgh'
lafilestru[4, 2] = 'N'
lafilestru[4, 3] = 9
lafilestru[4, 4] = 2
lafilestru[5, 1] = 'Empty'
lafilestru[5, 2] = 'C'
lafilestru[5, 3] = 1
lafilestru[5, 4] = 0
lnfilestru = ALEN(lafilestru, 1)
DIMENSION lafilestru[lnfilestru + 1, 4]
lafilestru[lnfilestru + 1, 1] = 'cCrtnVlTyp'
lafilestru[lnfilestru + 1, 2] = 'C'
lafilestru[lnfilestru + 1, 3] = 6
lafilestru[lnfilestru + 1, 4] = 0
lnfilestru = ALEN(lafilestru, 1)
DIMENSION lafilestru[lnfilestru + 1, 4]
lafilestru[lnfilestru + 1, 1] = 'CCARRCTNID'
lafilestru[lnfilestru + 1, 2] = 'C'
lafilestru[lnfilestru + 1, 3] = 25
lafilestru[lnfilestru + 1, 4] = 0
DIMENSION laindx[2, 2]
laindx[1, 1] = 'STR(Cart_No,4)+STR(Pal_No,4)'
laindx[1, 2] = lcctnhdr
laindx[2, 1] = 'Empty+STR(Cart_No,4)+STR(Pal_No,4)'
laindx[2, 2] = 'EMPTY'
= gfcrttmp(lcctnhdr, @lafilestru, @laindx)
SET ORDER IN (lcctnhdr) TO lcCtnHdr
lni = 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Cart_No'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 4
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Style'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 19
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'nOrdLineNo'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Size1'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 5
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Size2'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 5
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Size3'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 5
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Size4'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 5
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Size5'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 5
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Size6'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 5
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Size7'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 5
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Size8'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 5
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Qty1'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Qty2'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Qty3'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Qty4'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Qty5'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Qty6'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Qty7'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Qty8'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Weight1'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 9
lafilestru[lni, 4] = 2
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Weight2'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 9
lafilestru[lni, 4] = 2
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Weight3'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 9
lafilestru[lni, 4] = 2
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Weight4'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 9
lafilestru[lni, 4] = 2
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Weight5'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 9
lafilestru[lni, 4] = 2
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Weight6'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 9
lafilestru[lni, 4] = 2
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Weight7'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 9
lafilestru[lni, 4] = 2
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Weight8'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 9
lafilestru[lni, 4] = 2
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'nStep'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PackLineNo'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'OrgWgh'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 5
lafilestru[lni, 4] = 2
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'SzCnt'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cStatus'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Br1'
lafilestru[lni, 2] = 'L'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Br2'
lafilestru[lni, 2] = 'L'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Br3'
lafilestru[lni, 2] = 'L'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Br4'
lafilestru[lni, 2] = 'L'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Br5'
lafilestru[lni, 2] = 'L'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Br6'
lafilestru[lni, 2] = 'L'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Br7'
lafilestru[lni, 2] = 'L'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Br8'
lafilestru[lni, 2] = 'L'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
DIMENSION laindx[2, 2]
laindx[1, 1] = 'STR(Cart_No,4)+Style+STR(nOrdLineNo,6)'
laindx[1, 2] = lcctndtl
laindx[2, 1] = 'cStatus'
laindx[2, 2] = 'Status'
= gfcrttmp(lcctndtl, @lafilestru, @laindx)
SET ORDER IN (lcctndtl) TO lcCtnDtl
SELECT pack_lin
=gfseek(&lctmppktk..piktkt,'PACK_LIN')
SCAN REST WHILE pack_no+STR(line_no,6)+STYLE+cpackcolor =  &lctmppktk..piktkt
 SCATTER MEMVAR
 INSERT INTO &lcctndtl FROM MEMVAR
 = gfseek(pack_lin.style, 'STYLE')
 = gfseek('S' + style.scale, 'SCALE')
 SELECT &lcctndtl
 REPLACE cart_no WITH no_cart, size1 WITH scale.sz1, size2 WITH scale.sz2, size3 WITH scale.sz3, size4 WITH scale.sz4, size5 WITH scale.sz5, size6 WITH scale.sz6, size7 WITH scale.sz7, size8 WITH scale.sz8, weight1 WITH style.nstyweight, weight2 WITH style.nstyweight, weight3 WITH style.nstyweight, weight4 WITH style.nstyweight, weight5 WITH style.nstyweight, weight6 WITH style.nstyweight, weight7 WITH style.nstyweight, weight8 WITH style.nstyweight, orgwgh WITH style.nstyweight, szcnt WITH scale.cnt, cstatus WITH 'M', br1 WITH qty1 > 0, br2 WITH qty2 > 0, br3 WITH qty3 > 0, br4 WITH qty4 > 0, br5 WITH qty5 > 0, br6 WITH qty6 > 0, br7 WITH qty7 > 0, br8 WITH qty8 > 0
 lntotqty = qty1 + qty2 + qty3 + qty4 + qty5 + qty6 + qty7 + qty8
 IF !SEEK(STR(&lcctndtl..cart_no,4),lcctnhdr)
  INSERT INTO &lcctnhdr (cart_no) VALUES (&lcctndtl..cart_no)
 ENDIF
 SELECT &lcctnhdr
 REPLACE totpcs WITH totpcs + lntotqty, totwgh WITH totwgh + lntotqty * style.nstyweight
ENDSCAN
ENDPROC
**
FUNCTION LFEVALSEGS
lcnonmajpi = ''
lnmajseg = gfitemmask('SM')
lnmajlen = LEN(gfitemmask('PM'))
DIMENSION lamajsegs[1, 1]
= gfitemmask(@lamajsegs)
FOR lni = lnmajseg + 1 TO ALEN(lamajsegs, 1)
 IF lamajsegs(lni, 1) $ 'CF'
  lcfree_clr = lamajsegs(lni, 1)
  lnnonmajst = IIF(lnnonmajst = 0 .OR. lamajsegs(lni, 1) = 'C', lamajsegs(lni, 4), lnnonmajst)
  lcnonmajpi = IIF(EMPTY(lcnonmajpi) .OR. lamajsegs(lni, 1) = 'C', lamajsegs(lni, 3), lcnonmajpi + lamajsegs(lni - 1, 6) + lamajsegs(lni, 3))
 ENDIF
 IF lamajsegs(lni, 1) = 'C' .OR. (!EMPTY(lcfree_clr) .AND. lamajsegs(lni, 1) <> 'F')
  EXIT
 ENDIF
ENDFOR
lncolorlen = LEN(lcnonmajpi)
RETURN ''
ENDFUNC
**
PROCEDURE LFVLBLINFO
PARAMETER lncarton, llprint
PRIVATE lncarton, lcuccver, lcstyle, lccolor, lcsize, llprint
DIMENSION lalblinfo[1, 6]
STORE '' TO lalblinfo, lcstyle, lccolor, lcsize, lccrtnsku, lccrtnupc
= lfgetstysz()
m.bol_no = ''
m.cpro_no = ''
m.cart_no = lncarton
lcwarecode = ordhdr.cwarecode
= gfseek(lcwarecode, 'Warehous')
m.vnd_name = warehous.cdesc
m.vnd_addr1 = warehous.caddress1
m.vnd_addr2 = warehous.caddress2
m.vnd_city = warehous.caddress3
m.vnd_state = warehous.caddress4
m.vnd_zip = warehous.caddress5
SELECT customer
= gfseek(IIF(EMPTY(ladata(5)), 'M' + ladata(4), 'S' + ladata(4) + ladata(5)), 'Customer')
lcdistctr = customer.dist_ctr
STORE ladata(5) TO m.cststore, m.store
STORE customer.stname TO m.cstname, m.shp_name
STORE customer.caddress1 TO m.cstaddr1, m.shp_addr1
STORE customer.caddress2 TO m.cstaddr2, m.shp_addr2
STORE customer.caddress3 TO m.cstcity, m.shp_city
STORE customer.caddress4 TO m.cststate, m.shp_state
STORE customer.caddress5 TO m.cstzip, m.shp_zip
= gfseek('M' + ladata(4), 'Customer')
m.int_vend = customer.ccusvend
IF gfseek('S' + ladata(4) + lcdistctr, 'Customer')
 m.store = lcdistctr
 m.shp_name = customer.stname
 m.shp_addr1 = customer.caddress1
 m.shp_addr2 = customer.caddress2
 m.shp_city = customer.caddress3
 m.shp_state = customer.caddress4
 m.shp_zip = customer.caddress5
ENDIF
m.shipvia = ordhdr.shipvia
lcwrname = lcwarecode
SELECT ordhdr
gfsetorder('ORDHDR')
= gfseek('O' + ladata(2))
lcorstore = ordhdr.store
lcuccver = ''
IF lledisys .AND. llediacc
 lcuccver = IIF(lndrctto = 1, ediph.casnlbl1, ediph.casnlbl2)
 m.lplt = ediph.lpltshp
ENDIF
lcuccver = IIF(EMPTY(lcuccver), 'XXX', lcuccver)
m.custpo = ordhdr.custpo
m.dept = ordhdr.dept
m.note1 = ordhdr.note1
m.note2 = ordhdr.note2
m.int_vend = IIF(EMPTY(ordhdr.int_vend), m.int_vend, ordhdr.int_vend)
m.cancelled = ordhdr.complete
m.event_cod = ordhdr.event_cod
lcmanufid = gfgetmemvar('XMANUFID')
m.manuf_id = PADL(ALLTRIM(lcmanufid), 7, '0')
m.ucc9 = RIGHT(PADL(ALLTRIM(ladata(1)), 6, '0'), 5) + PADL(lncarton, 4, '0')
m.ucc_check = lfcheckno('000' + m.manuf_id + m.ucc9)
m.asn_ver = lcuccver
IF (lcfree_clr <> 'C') .OR. EMPTY(lccolor) .OR. ('MIXED' $ lccolor)
 DIMENSION lacoddesc[1, 3]
 lacoddesc = ''
 lacoddesc[1, 1] = m.shipvia
 lacoddesc[1, 2] = 'SHIPVIA'
 m.cclrdesc = lccolor
ELSE
 DIMENSION lacoddesc[2, 3]
 lacoddesc = ''
 lacoddesc[1, 1] = m.shipvia
 lacoddesc[2,1] = SUBSTR(&lcctndtl..STYLE,lnnonmajst,lncolorlen)
 lacoddesc[1, 2] = 'SHIPVIA'
 lacoddesc[2, 2] = 'COLOR'
 = gfcoddes(@lacoddesc)
 m.cclrdesc = lacoddesc(2, 1)
ENDIF
= gfcoddes(@lacoddesc)
m.pack_no = IIF(EMPTY(ladata(1)), ladata(3), ladata(1))
m.carrier = lacoddesc(1, 3)
m.bol_no = ''
m.style = lcstyle
= SEEK(STR(m.cart_no, 4), lcctnhdr)
m.totqty    = &lcctnhdr..totpcs
m.csizedesc = lcsize
m.cupc = lccrtnupc
m.pack_id = lccrtnsku
lcdetorder = ORDER(lcctnhdr)
USE (oariaapplication.workdir + lcctnhdr) AGAIN ALIAS (lctmpdetfl) ORDER (lcdetorder) IN 0
GOTO BOTTOM IN (lctmpdetfl)
m.cartons = &lctmpdetfl..cart_no
USE IN (lctmpdetfl)
SELECT (lctmasnshp)
IF SEEK(STR(m.cart_no, 6), lctmasnshp)
 GATHER MEMO MEMVAR
ELSE
 INSERT INTO (lctmasnshp) FROM MEMVAR
ENDIF
= gfadd_info(lctmasnshp)
IF TYPE('laLblInfo[1,4]') = 'C' .AND. !EMPTY(lalblinfo(1, 4))
 SAVE TO MEMO mlblinfo ALL LIKE laLblInfo
ENDIF
IF llprint
 SELECT sycasnhd
 = gfseek("")
 GOTO TOP
 LOCATE FOR cver = lcuccver .AND. ctype = 'Y'
 IF FOUND()
  SELECT (lctmasnshp)
  = lfvsullbl(loformset, lcuccver, ALLTRIM(STR(m.cart_no)))
 ELSE
  LOCATE FOR cver = lcuccver .AND. ctype = 'N'
  IF FOUND()
   SELECT (lctmasnshp)
   = lfprintlbl(loformset, lcuccver)
  ENDIF
 ENDIF
 IF loformset.llntfound
  RETURN
 ENDIF
ENDIF
SELECT (lcpcklin)
ENDPROC
**
PROCEDURE LFGETSTYSZ
PRIVATE lccurralis, lnrecpoint, lndetpoint
lccurralis = SELECT(0)
SELECT (lcctndtl)
lnrecpoint = RECNO()
lndetpoint = RECNO(lccartonsz)
lcsetskip = SET('SKIP')
SET SKIP TO
SEEK STR(lncarton, 4) 
lcstyle = SUBSTR(style, 1, lnmajlen)
lcupdsty = style
IF lcfree_clr = 'C'
 lccolor = SUBSTR(style, lnnonmajst, lncolorlen)
ELSE
 lccolor = SPACE(6)
ENDIF
lnlblinfo = 0
DIMENSION lalblinfo[1, 6]
lccrtnsku = ''
lccrtnupc = ''
lcsize = ''
SCAN REST WHILE STR(cart_no, 4) = STR(lncarton, 4)
 lcstyle = IIF(lcstyle <> SUBSTR(style, 1, lnmajlen), SPACE(lnmajlen), lcstyle)
 lcupdsty = IIF(lcupdsty <> style, SPACE(19), lcupdsty)
 IF lcfree_clr = 'C'
  lccolor = IIF(EMPTY(lcstyle), SPACE(lncolorlen), IIF(lccolor <> SUBSTR(style, lnnonmajst, lncolorlen), 'MIXED', lccolor))
 ENDIF
 FOR lnszno1 = 1 TO 8
  lcszno = STR(lnszno1, 1)
  IF br&lcszno
   lcsku = SPACE(16)
   IF gfseek('S'+ladata[4] + &lcctndtl..STYLE, 'Spck_Lin')
    SELECT spck_lin
    LOCATE REST  WHILE TYPE + account + STYLE = 'S' + ladata[4] + &lcctndtl..STYLE FOR qty&lcszno = 1
    IF FOUND()
     lcsku = PADR(pack_id, 16)
    ELSE
     =gfseek('S' + ladata[4] + &lcctndtl..STYLE, 'Spck_Lin')
     LOCATE REST WHILE TYPE + account + STYLE = 'S' + ladata[4] + &lcctndtl..STYLE FOR totqty = 0
     IF FOUND()
      lcsku = PADR(pack_id, 16)
     ENDIF
    ENDIF
   ENDIF
   lcupc = SPACE(13)
   IF llupcinst AND gfseek(&lcctndtl..STYLE+lcszno,'StyleUpc')
    lcupc = styleupc.cupcnum1 + styleupc.cupcnum2 + styleupc.cupcnum3
   ENDIF
   SELECT (lcctndtl)
   lnlblinfo = lnlblinfo + 1
   DIMENSION lalblinfo[lnlblinfo, 6]
   lalblinfo[lnlblinfo, 1] = lcsku
   lalblinfo[lnlblinfo,2] = qty&lcszno
   lalblinfo[lnlblinfo,3] = SIZE&lcszno
   lalblinfo[lnlblinfo, 4] = style
   lalblinfo[lnlblinfo, 5] = lcupc
   lalblinfo[lnlblinfo, 6] = lnszno1
   lcsize    = IIF(SIZE&lcszno <> lcsize   ,'MIXED',SIZE&lcszno)
   lccrtnsku = IIF(lcsku <> lccrtnsku, SPACE(16), lcsku)
   lccrtnupc = IIF(lcupc <> lccrtnupc, SPACE(13), lcupc)
  ENDIF
 ENDFOR
ENDSCAN
lcsize = IIF(EMPTY(lcstyle), SPACE(5), lcsize)
IF !EMPTY(lcsetskip)
 SET SKIP TO (lcsetskip)
ENDIF
IF BETWEEN(lnrecpoint, 1, RECCOUNT())
 GOTO lnrecpoint
ENDIF
IF BETWEEN(lndetpoint, 1, RECCOUNT(lccartonsz))
 GOTO lndetpoint IN (lccartonsz)
ENDIF
SELECT (lccurralis)
ENDPROC
**
FUNCTION LFCHECKNO
PARAMETER lcuccno
PRIVATE lnchkdigit, lnsumodd, lnsumeven, lncount
STORE 0 TO lnsumodd, lnsumeven, lnchkdigit
FOR lncount = 1 TO 9
 lnsumodd = lnsumodd + VAL(SUBSTR(lcuccno, lncount * 2 - 1, 1))
 lnsumeven = lnsumeven + VAL(SUBSTR(lcuccno, lncount * 2, 1))
ENDFOR
lnsumodd = lnsumodd + VAL(SUBSTR(lcuccno, 19, 1))
lnchkdigit = MOD(lnsumodd * 3 + lnsumeven, 10)
RETURN (IIF(lnchkdigit = 0, '0', STR(INT(10 - lnchkdigit), 1)))
ENDFUNC
**
PROCEDURE lfVsulLbl
PARAMETER loformset, lcversion, lccartons
STORE 0 TO lnchoicedtl
lcaccount = PADR(ladata(4), 5)
IF llediacc
 IF gfseek('A' + lcaccount, 'EDIACPRT') .AND. gfseek(ediacprt.cpartcode, 'EDIPH') .AND. ediph.ldtlbl
  loformset.lldetlabel = .T.
  loformset.lcdetailvr = ediph.cdtlbl
 ELSE
  loformset.lldetlabel = .F.
 ENDIF
ELSE
 STORE '' TO loformset.lcdetailvr, loformset.lcdetlball
 DIMENSION laversn[1]
 lnvercount = lfgetverxx(loformset)
 loformset.lldetlabel = (lnvercount >= 1)
ENDIF
IF !llediacc .AND. lnvercount >= 1
 = lfselever(loformset, laversn)
ENDIF
STORE .F. TO llprintlbl
IF loformset.lldetlabel
 IF EMPTY(loformset.lcdetlball)
  lnchoicedtl = gfmodalgen('TRM44105B40016', 'DIALOG', lccartons)
  DO CASE
   CASE lnchoicedtl = 1
    llprintlbl = .T.
   CASE lnchoicedtl = 2
    llprintlbl = .T.
    loformset.lcdetlball = "Y"
   CASE lnchoicedtl = 3
    llprintlbl = .F.
   CASE lnchoicedtl = 4
    llprintlbl = .F.
    loformset.lcdetlball = "N"
  ENDCASE
 ELSE
  llprintlbl = (loformset.lcdetlball = "Y")
 ENDIF
ENDIF
SELECT (lctmasnshp)
lnedirecno = RECNO()
COPY TO (oariaapplication.workdir + lcprnasnshp)
SELECT 0
USE EXCLUSIVE (oariaapplication.workdir + lcprnasnshp)
INDEX ON bol_no + pack_no + STR(cart_no, 6) + asn_ver TAG (lcprnasnshp)
USE IN (lcprnasnshp)
tcediact = lcaccount
tcedishp = lcprnasnshp
tcedicmp = oariaapplication.activecompanyid
tcediprtnm = lcsndport
tcedibolno = bol_no
tcedipckno = pack_no
tnedicrtno = lccartons
tcediver = lcversion
tledidetlb = llprintlbl
tcedidetvr = IIF(llprintlbl, loformset.lcdetailvr, '')
lcedipath = ALLTRIM(UPPER(SUBSTR(oariaapplication.syspath, 1, AT('\', oariaapplication.syspath, 2))))
lcedipath = SUBSTR(lcedipath, 1, LEN(lcedipath) - 1)
lcaria4xppath = ALLTRIM(UPPER(SUBSTR(oariaapplication.applicationhome, 1, AT('\', oariaapplication.applicationhome, 2))))
lcaria4xppath = SUBSTR(lcaria4xppath, 1, LEN(lcaria4xppath) - 1)
IF (oariaapplication.classdir + 'EDI.VCX' $ SET('classlib'))
 RELEASE CLASSLIB (oariaapplication.classdir + 'EDI.VCX')
ENDIF
lcoldapphome = oariaapplication.applicationhome
lcoldapprep = oariaapplication.reporthome
lcoldappbitmaphome = oariaapplication.bitmaphome
lcoldappcls = oariaapplication.classdir
lcoldappscx = oariaapplication.screenhome
lcedipath = UPPER(SUBSTR(oariaapplication.syspath, 1, AT('\', oariaapplication.syspath, 2)))
oariaapplication.applicationhome = lcedipath + 'PRGS\'
oariaapplication.reporthome = lcedipath + 'REPORTS\'
oariaapplication.bitmaphome = lcedipath + 'BMPs\'
oariaapplication.classdir = lcedipath + 'CLASSES\'
oariaapplication.screenhome = lcedipath + 'SCREENS\'
SET CLASSLIB TO (oariaapplication.classdir + 'EDI.VCX') ADDITIVE
SET PROCEDURE TO (oariaapplication.applicationhome + 'EDIGLOBL.FXP') ADDITIVE
oprnlabel = CREATEOBJECT('PrnLabel', .T., ALLTRIM(lcaria4xppath), ALLTRIM(tcediact), ALLTRIM(tcedishp), ALLTRIM(tcediprtnm), ALLTRIM(tcedibolno), ALLTRIM(tcedipckno), tnedicrtno)
oprnlabel.do(.T., , ALLTRIM(lcaria4xppath), ALLTRIM(tcediact), ALLTRIM(tcedishp), ALLTRIM(tcediprtnm), ALLTRIM(tcedibolno), ALLTRIM(tcedipckno), tnedicrtno, ALLTRIM(tcediver), tledidetlb, IIF(tledidetlb, ALLTRIM(tcedidetvr), ''))
RELEASE oprnlabel
oariaapplication.applicationhome = lcoldapphome
oariaapplication.reporthome = lcoldapprep
oariaapplication.bitmaphome = lcoldappbitmaphome
oariaapplication.classdir = lcoldappcls
oariaapplication.screenhome = lcoldappscx
IF USED(lcprnasnshp)
 USE IN (lcprnasnshp)
 ERASE (oariaapplication.workdir + lcprnasnshp + '.*')
ENDIF
SELECT (lctmasnshp)
GOTO lnedirecno
WAIT CLEAR
ENDPROC
**
PROCEDURE lfSeleVer
LPARAMETERS loformset, laversn
PRIVATE lnprvalias
lnprvalias = SELECT(0)
DO FORM (oariaapplication.screenhome+'ALDtVer.SCX') WITH loformset, laversn
SELECT (lnprvalias)
ENDPROC
**
FUNCTION lfGetVerXX
LPARAMETERS loformset
PRIVATE lnprvalias, lnreturnvr
lnreturnvr = 0
lnprvalias = SELECT(0)
SELECT sycasnlb
= gfseek("")
SELECT DISTINCT cver FROM sycasnlb WHERE LEFT(cver, 2) = 'XX' AND cver <> 'XXX' INTO ARRAY laversn
lnreturnvr = ALEN(laversn, 1)
IF lnreturnvr >= 1
 loformset.lcdetailvr = laversn(1)
ENDIF
SELECT (lnprvalias)
RETURN lnreturnvr
ENDFUNC
**
PROCEDURE lfPrintLbl
LPARAMETERS loformset, lcversion
PRIVATE lncuralias, lnhandle, lcoutfile, lcstring, lcdata
lncuralias = SELECT(0)
lcstring = SPACE(40)
SELECT sycasnlb
IF !gfseek(lcversion + 'H', 'SYCASNLB') .AND. gfmodalgen('INM44068B00000', 'DIALOG') = 1
 loformset.llntfound = .T.
 RETURN
ENDIF
lcoutfile = oariaapplication.workdir + lcasnlabel + ".TXT"
lnhandle = FCREATE(lcoutfile, 0)
= FSEEK(lnhandle, 0, 2)
SCAN WHILE cver + ceditype = lcversion + 'H'
 STORE data TO lcdata
 lcstring = &lcdata
 = FPUTS(lnhandle, lcstring)
ENDSCAN
lcstring = SPACE(3)
= FPUTS(lnhandle, lcstring)
SELECT (loformset.lctmasnshp)
SCATTER MEMVAR
mucb = PADL(ALLTRIM(lfmanufid(loformset, m.bol_no)), 7, '0') + PADL(ALLTRIM(m.bol_no), 9, '0')
mucb = mucb + lfcheckdgt(mucb, 'E')
DIMENSION ladrltfld[1, 2]
ladrltfld[1, 1] = 'DIVLNAME  '
ladrltfld[1, 2] = 'lcDivLName'
lcdivlname = ''
= gfrltfld(ordhdr.cdivision, @ladrltfld, 'CDIVISION ')
m.divlname = lcdivlname
= gfseek(ordhdr.account + ordhdr.dept, 'CUSTDEPT')
m.deptdesc = custdept.cdeptdesc
= gfseek(ALLTRIM(style), 'STYLE')
m.stydesc = style.desc
= SEEK(STR(EVALUATE(lctmasnshp + '.cart_no'), 4), lcctnhdr)
m.weight = EVALUATE(lcctnhdr + '.TotWgh')
SELECT (lctmasnshp)
m.date = oariaapplication.systemdate
m.order = PADR(ladata(2), 6)
m.account = PADR(ladata(4), 5)
m.pattern = style.pattern
STORE '' TO m.sizedesc1, m.sizedesc2, m.sizedesc3, m.sizedesc4, m.sizedesc5, m.sizedesc6, m.sizedesc7, m.sizedesc8
STORE '' TO m.sizesku1, m.sizesku2, m.sizesku3, m.sizesku4, m.sizesku5, m.sizesku6, m.sizesku7, m.sizesku8
STORE 0 TO m.sizeqty1, m.sizeqty2, m.sizeqty3, m.sizeqty4, m.sizeqty5, m.sizeqty6, m.sizeqty7, m.sizeqty8
SELECT (lcctndtl)
lnstyrec = RECNO(lcctndtl)
SET RELATION TO
IF loformset.llextsizsc
 SELECT (lcctndtl)
 IF SEEK(STR(EVALUATE(lctmasnshp + '.cart_no'), 4) + ALLTRIM(EVALUATE(lctmasnshp + '.Style')))
  lnsizecount = 0
  lccurrsty = SUBSTR(style, 1, loformset.lnsizepos - IIF(EMPTY(loformset.lcsizesep), 1, 2)) + dyelot
  SCAN REST WHILE STR(cart_no, 4) + style + dyelot + STR(nordlineno, 6) = STR(EVALUATE(lctmasnshp + '.cart_no'), 4) + lccurrsty .AND. lnsizecount < 8
   gfseek('S' + SUBSTR(style, loformset.lnsizepos, loformset.lnsizelen), 'Scale')
   lcsizeno = EVALUATE(lcctndtl + '.cSizeNo')
   IF EVALUATE(lcctndtl + '.Qty') <> 0
    lnsizecount = lnsizecount + 1
    lcsizecount = STR(lnsizecount, 1)
    m.sizedesc&lcsizecount = ALLTRIM(SCALE.sz&lccount)
    m.sizeqty&lcsizecount  = EVALUATE(lcctndtl+'.Qty')
    SELECT spck_lin
    gfsetorder('Spklnstcn')
    gfseek('S' + m.account + EVALUATE(lcctndtl + '.style') + EVALUATE(lcctndtl + '.Dyelot'), 'SPCK_LIN')
    LOCATE REST WHILE TYPE+account+STYLE+dyelot+pack_id = 'S'+m.account+EVALUATE(lcctndtl+'.style')+EVALUATE(lcctndtl+'.Dyelot') FOR qty&lcsizeno= 1
    IF FOUND()
     m.sizesku&lcsizecount = ALLTRIM(spck_lin.pack_id)
    ENDIF
   ENDIF
  ENDSCAN
 ENDIF
ELSE
 SELECT (lctmasnshp)
 gfseek('S' + style.scale, 'SCALE')
 IF SEEK(STR(cart_no, 4) + ALLTRIM(style), lcctndtl)
  lccurrsty = ALLTRIM(EVALUATE(lcctndtl + '.Style')) + EVALUATE(lcctndtl + '.Dyelot')
  SELECT (lcctndtl)
  lcsizeno = EVALUATE(lcctndtl + '.cSizeNo')
  lnsizecount = VAL(lcsizeno)
  SCAN REST WHILE STR(cart_no, 4) + style + dyelot + STR(nordlineno, 6) = STR(EVALUATE(loformset.lctmasnshp + '.cart_no'), 4) + lccurrsty .AND. lnsizecount < 8
   lcsizeno = EVALUATE(lcctndtl + '.cSizeNo')
   lnsizecount = VAL(lcsizeno)
   IF EVALUATE(lcctndtl + '.Qty') <> 0
    m.sizedesc&lcsizeno= ALLTRIM(SCALE.sz&lcsizeno)
    m.sizeqty&lcsizeno= EVALUATE(lcctndtl+'.Qty')
    SELECT spck_lin
    gfsetorder('Spklnstcn')
    gfseek('S' + m.account + EVALUATE(lcctndtl + '.style') + EVALUATE(lcctndtl + '.Dyelot'), 'SPCK_LIN')
    LOCATE REST WHILE TYPE+account+STYLE+dyelot+pack_id = 'S'+m.account+EVALUATE(lcctndtl+'.style')+EVALUATE(lcctndtl+'.Dyelot') FOR qty&lcsizeno= 1
    IF FOUND()
     m.sizesku&lcsizeno = ALLTRIM(spck_lin.pack_id)
    ENDIF
   ENDIF
  ENDSCAN
 ENDIF
ENDIF
SELECT (lctmasnshp)
tmp_addr = ALLTRIM(vnd_city) + ",  " + ALLTRIM(vnd_state) + "   " + ALLTRIM(vnd_zip)
SELECT sycasnlb
gfseek(lcversion + 'L', 'SYCASNLB')
SCAN WHILE cver + ceditype = lcversion + 'L'
 STORE data TO lcdata
 lcstring = &lcdata
 = FPUTS(lnhandle, lcstring)
ENDSCAN
lcstring = SPACE(3)
= FPUTS(lnhandle, lcstring)
IF llediacc
 IF gfseek('A' + m.account, 'EDIACPRT') .AND. gfseek(ediacprt.cpartcode, 'EDIPH') .AND. ediph.ldtlbl
  loformset.lldetlabel = .T.
  loformset.lcdetailvr = ediph.cdtlbl
 ELSE
  loformset.lldetlabel = .F.
 ENDIF
ELSE
 STORE '' TO loformset.lcdetailvr, loformset.lcdetlball
 DIMENSION laversn[1]
 lnvercount = lfgetverxx(loformset)
 loformset.lldetlabel = (lnvercount >= 1)
ENDIF
IF loformset.lldetlabel
 PRIVATE lnchoice, llprintlbl, lnmajorlen, lnclrlen, lnclrpos, llusecolor, lcupcstyle, lcgencolor, lcgensty
 IF EMPTY(loformset.lcdetlball)
  lnchoice = gfmodalgen('TRM44105B40016', 'DIALOG', ALLTRIM(STR(EVALUATE(lctmasnshp + '.Cart_No'), 4)))
  DO CASE
   CASE lnchoice = 1
    llprintlbl = .T.
   CASE lnchoice = 2
    llprintlbl = .T.
    loformset.lcdetlball = "Y"
   CASE lnchoice = 3
    llprintlbl = .F.
   CASE lnchoice = 4
    llprintlbl = .F.
    loformset.lcdetlball = "N"
  ENDCASE
 ELSE
  llprintlbl = (loformset.lcdetlball = "Y")
 ENDIF
 IF llprintlbl
  IF !llediacc .AND. lnvercount >= 1
   = lfselever(loformset, laversn)
  ENDIF
  STORE '' TO mdstyle1, mdstyle2, mdstyle3, mdstyle4, mdstyle5, mdstyle6, mdstyle7, mdstyle8, mdstyle9, mdstyle10, mdstyle11, mdstyle12, mdstyle13, mdstyle14, mdstyle15, mdstyle16, mdstyle17, mdstyle18, mdstyle19, mdstyle20
  STORE '' TO mdstymaj1, mdstymaj2, mdstymaj3, mdstymaj4, mdstymaj5, mdstymaj6, mdstymaj7, mdstymaj8, mdstymaj9, mdstymaj10, mdstymaj11, mdstymaj12, mdstymaj13, mdstymaj14, mdstymaj15, mdstymaj16, mdstymaj17, mdstymaj18, mdstymaj19, mdstymaj20
  STORE '' TO mdcolor1, mdcolor2, mdcolor3, mdcolor4, mdcolor5, mdcolor6, mdcolor7, mdcolor8, mdcolor9, mdcolor10, mdcolor11, mdcolor12, mdcolor13, mdcolor14, mdcolor15, mdcolor16, mdcolor17, mdcolor18, mdcolor19, mdcolor20
  STORE '' TO mdsku1, mdsku2, mdsku3, mdsku4, mdsku5, mdsku6, mdsku7, mdsku8, mdsku9, mdsku10, mdsku11, mdsku12, mdsku13, mdsku14, mdsku15, mdsku16, mdsku17, mdsku18, mdsku19, mdsku20
  STORE '' TO mdstyupc1, mdstyupc2, mdstyupc3, mdstyupc4, mdstyupc5, mdstyupc6, mdstyupc7, mdstyupc8, mdstyupc9, mdstyupc10, mdstyupc11, mdstyupc12, mdstyupc13, mdstyupc14, mdstyupc15, mdstyupc16, mdstyupc17, mdstyupc18, mdstyupc19, mdstyupc20
  STORE '' TO mdsizdes1, mdsizdes2, mdsizdes3, mdsizdes4, mdsizdes5, mdsizdes6, mdsizdes7, mdsizdes8, mdsizdes9, mdsizdes10, mdsizdes11, mdsizdes12, mdsizdes13, mdsizdes14, mdsizdes15, mdsizdes16, mdsizdes17, mdsizdes18, mdsizdes19, mdsizdes20
  STORE 0 TO mdqty1, mdqty2, mdqty3, mdqty4, mdqty5, mdqty6, mdqty7, mdqty8, mdqty9, mdqty10, mdqty11, mdqty12, mdqty13, mdqty14, mdqty15, mdqty16, mdqty17, mdqty18, mdqty19, mdqty20
  lnmajorlen = LEN(loformset.lcmask)
  STORE 0 TO lnclrlen, lnclrpos
  STORE .F. TO llusecolor
  DIMENSION laitemseg[1]
  = gfitemmask(@laitemseg)
  FOR lncount = 1 TO ALEN(laitemseg, 1)
   IF laitemseg(lncount, 1) = 'C'
    llusecolor = .T.
    lnclrlen = LEN(laitemseg(lncount, 3))
    lnclrpos = laitemseg(lncount, 4)
    EXIT
   ENDIF
  ENDFOR
  lcgencolor = PADR(gfgetmemvar("MCLRASSCOD", oariaapplication.activecompanyid), 6)
  SELECT (lcctndtl)
  IF SEEK(STR(EVALUATE(lctmasnshp + '.Cart_No'), 4))
   lnsizecount = 0
   SCAN REST WHILE STR(cart_no, 4) = STR(EVALUATE(lctmasnshp + '.Cart_No'), 4) .AND. lnsizecount < 20
    gfseek(EVALUATE(lcctndtl + '.Style'), 'STYLE')
    gfseek('S' + style.scale, 'SCALE')
    IF loformset.llupcinst
     lcupcstyle = EVALUATE(lcctndtl + '.Style')
     IF llusecolor
      lcgensty = SUBSTR(lcupcstyle, 1, lnclrpos - 1) + lcgencolor + SUBSTR(lcupcstyle, lnclrpos + lnclrlen)
     ENDIF
    ENDIF
    lccount = EVALUATE(lcctndtl + '.cSizeNo')
    IF lnsizecount < 20 .AND. EVALUATE(lcctndtl + '.Qty') <> 0
     lnsizecount = lnsizecount + 1
     lcsizecount = ALLTRIM(STR(lnsizecount, 2))
     mdstyle&lcsizecount  = EVALUATE(lcctndtl+'.Style')
     mdstymaj&lcsizecount = LEFT(EVALUATE(lcctndtl+'.Style'), lnmajorlen)
     mdsizdes&lcsizecount = ALLTRIM(SCALE.sz&lccount)
     mdqty&lcsizecount    = EVALUATE(lcctndtl+'.Qty')
     IF llusecolor
      mdcolor&lcsizecount = SUBSTR( EVALUATE(lcctndtl+'.Style') , lnclrpos , lnclrlen)
     ENDIF
     IF loformset.llupcinst
      SELECT 'STYLEUPC'
      gfsetorder('STYLEUPC')
      IF gfseek(lcupcstyle + lccount, 'StyleUPC')
       mdstyupc&lcsizecount = styleupc.cupcnum1 + styleupc.cupcnum2 + styleupc.cupcnum3
      ELSE
       IF llusecolor .AND. gfseek(lcgensty + lccount, 'StyleUPC')
        mdstyupc&lcsizecount = styleupc.cupcnum1 + styleupc.cupcnum2 + styleupc.cupcnum3
       ENDIF
      ENDIF
     ENDIF
     SELECT spck_lin
     gfsetorder('Spklnstcn')
     gfseek('S' + m.account + EVALUATE(lcctndtl + '.Style') + EVALUATE(lcctndtl + '.Dyelot'), 'SPCK_LIN')
     LOCATE REST WHILE TYPE + account + STYLE + pack_id = 'S' + m.account + EVALUATE(lcctndtl+'.Style')+EVALUATE(lcctndtl+'.Dyelot') FOR qty&lccount = 1
     IF FOUND()
      mdsku&lcsizecount = ALLTRIM(spck_lin.pack_id)
     ENDIF
    ENDIF
   ENDSCAN
  ENDIF
  SELECT sycasnlb
  gfseek(loformset.lcdetailvr + 'H', 'SYCASNLB')
  SCAN WHILE cver + ceditype = loformset.lcdetailvr + 'H'
   STORE data TO lcdata
   lcstring = &lcdata
   = FPUTS(lnhandle, lcstring)
  ENDSCAN
  lcstring = SPACE(3)
  = FPUTS(lnhandle, lcstring)
  gfseek(loformset.lcdetailvr + 'L', 'SYCASNLB')
  SCAN WHILE cver + ceditype = loformset.lcdetailvr + 'L'
   STORE data TO lcdata
   lcstring = &lcdata
   = FPUTS(lnhandle, lcstring)
  ENDSCAN
 ENDIF
ENDIF
= FCLOSE(lnhandle)
lccommand = "TYPE " + lcoutfile + " > " + lcsndport
! &lccommand
WAIT CLEAR
SELECT (lcctndtl)
IF lnstyrec <= RECCOUNT(lcctndtl)
 GOTO lnstyrec
ENDIF
= RLOCK(lcctndtl)
UNLOCK IN (lcctndtl)
SELECT (lncuralias)
RETURN
ENDPROC
**
FUNCTION lfManufID
LPARAMETERS loformset, lcbolno
PRIVATE lcmanuf_id, larltdfld, mucclevel
lcmanuf_id = loformset.lcmanufid
mucclevel = gfgetmemvar('M_UCCDIV', oariaapplication.activecompanyid)
IF mucclevel = 'N'
 DIMENSION larltdfld[1, 2]
 STORE '' TO larltdfld, lcupcman
 larltdfld[1, 1] = "CUPCMAN"
 larltdfld[1, 2] = 'LCUPCMAN'
 = gfrltfld(ordhdr.cdivision, @larltdfld, 'CDIVISION')
 lcmanuf_id = IIF(EMPTY(lcupcman), lcmanuf_id, lcupcman)
ENDIF
RETURN ALLTRIM(lcmanuf_id)
ENDFUNC
**
FUNCTION lfCheckDgt
PARAMETER lcuccno, lctype
PRIVATE lnchkdigit, lnsumodd, lnsumeven, lncount
STORE 0 TO lnchkdigit, lnsumodd, lnsumeven, lntop
lntop = LEN(lcuccno)
FOR lncount = 1 TO lntop STEP 2
 lnsumodd = lnsumodd + VAL(SUBSTR(lcuccno, lncount, 1))
 lnsumeven = lnsumeven + VAL(SUBSTR(lcuccno, lncount + 1, 1))
ENDFOR
IF lctype = 'O'
 lnchkdigit = MOD(lnsumodd * 3 + lnsumeven, 10)
ELSE
 lnchkdigit = MOD(lnsumodd + lnsumeven * 3, 10)
ENDIF
RETURN (IIF(lnchkdigit = 0, '0', STR(INT(10 - lnchkdigit), 1)))
ENDFUNC
**
PROCEDURE LFUPDENT
IF !USED('STYHIST')
 = gfopentable('STYHIST', 'STYHIST', 'SH')
ENDIF
IF !USED('Contact')
 = gfopentable('Contact', 'Contact', 'SH')
ENDIF
IF !USED('UNIFORM')
 = gfopentable('UNIFORM', 'UNIFORM', 'SH')
ENDIF
STORE 0 TO lnclrpos, lnclrlen, lnmajorlen
= lfgetclrd()
SELECT &lctmppktk
LOCATE
=gfseek('M'+&lctmppktk..account,'CUSTOMER')
SCAN
 =gfseek('O'+&lctmppktk..ORDER,'ORDHDR')
 =gfseek('O'+&lctmppktk..ORDER,'ORDLINE') 
 SELECT ordline
 SCAN REST WHILE cordtype+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+&lctmppktk..ORDER  FOR piktkt = &lctmppktk..piktkt
  SELECT ordline
  IF !EMPTY(employee)
   WAIT WINDOW NOWAIT 'Update entitlments used field for order ' + &lctmppktk..ORDER 
   =gfseek("C"+PADR(&lctmppktk..account,8)+PADR(&lctmppktk..STORE,8),'Contact')
   SELECT contact
   LOCATE REST WHILE cconttype+ccont_id+STORE+contact= "C"+PADR(&lctmppktk..account,8)+PADR(&lctmppktk..STORE,8)  FOR ccntctcode = ordline.employee
   SELECT ordline
   lcsty = PADR(SUBSTR(ordline.style, 1, lnmajorlen), 19)
   lcclr = PADR(SUBSTR(ordline.style, lnclrpos, lnclrlen), 6)
   lnupdfctr = 1
   lnentlmnt = 0
   IF !EMPTY(contact.ucode)
    = gfseek(contact.ucode + lcsty + lcclr, 'UNIFORM')
    lnentlmnt = uniform.entitlemnt
    DO CASE
     CASE uniform.type = 'V'
      lnupdfctr = ordline.price
     CASE uniform.type = 'P'
      lnupdfctr = uniform.pntsval
    ENDCASE
   ENDIF
   SELECT styhist
   IF !gfseek(&lctmppktk..account+&lctmppktk..STORE+ordline.employee+lcsty+lcclr , 'STYHIST' )
    APPEND BLANK
    REPLACE account    WITH &lctmppktk..account , STORE      WITH &lctmppktk..STORE, employee   WITH ordline.employee, cstymajor  WITH lcsty , COLOR     WITH lcclr , entitlemnt WITH lnentlmnt
    = gfreplace("")
   ENDIF
   gfreplace("NUSED WITH NUSED+ORDLINE.TOTPIK*LNUPDFCTR")
   gfreplace('DLASTORD WITH ORDHDR.ENTERED,' + 'ORDER WITH ORDHDR.ORDER')
   = gfadd_info('STYHIST')
   IF !EMPTY(contact.ucode)
    = gfseek(contact.ucode, 'UNIFORM')
    SELECT uniform
    SCAN REST WHILE ucode + cstymajor + color = contact.ucode
     IF !gfseek(&lctmppktk..account+&lctmppktk..STORE+ordline.employee+cstymajor+COLOR , 'STYHIST' )
      SELECT styhist
      APPEND BLANK
      REPLACE account    WITH &lctmppktk..account   STORE      WITH &lctmppktk..STORE     employee   WITH ordline.employee      cstymajor  WITH uniform.cstymajor     COLOR     WITH uniform.COLOR        entitlemnt WITH uniform.entitlemnt
      = gfreplace("")
      = gfadd_info('STYHIST')
     ENDIF
    ENDSCAN
   ENDIF
  ENDIF
 ENDSCAN
ENDSCAN
SELECT styhist
gftableupdate()
ENDPROC
**
PROCEDURE LFGETCLRD
DIMENSION laitemseg[1]
PRIVATE lncount
lcoldselec = SELECT()
= gfitemmask(@laitemseg)
FOR lncount = 1 TO ALEN(laitemseg, 1)
 DO CASE
  CASE laitemseg(lncount, 1) = 'C'
   lnclrlen = LEN(laitemseg(lncount, 3))
   lnclrpos = laitemseg(lncount, 4)
   lcclrspr = ALLTRIM(laitemseg(lncount, 6))
 ENDCASE
ENDFOR
lnmajorlen = LEN(gfitemmask('PM'))
SELECT (lcoldselec)
ENDPROC
**
PROCEDURE LFCRTINVS
DIMENSION lasetups[26, 2]
llconsinv = .F.
DIMENSION latrltfld[6, 2]
STORE 0.00  TO lndiscpcnt
lccrttmp = ''
lcupstmp = ''
DIMENSION ladisrltfl[1, 2]
STORE 0 TO lndisc_pcn
STORE '' TO lcglsessio, lctaxname
STORE 0 TO lnordmark, lninvmark, lntottax
STORE 0 TO lnoldchrg
STORE {} TO lddefinvda, lddefpstda
STORE 0 TO lncartons, lnstylngth
STORE '' TO lctaxtitle, lctaxbreak, lcsession
STORE .F. TO lliscanada, llisenglan, llmulcurr, lleditexrt, lasetups
STORE '' TO lcinvline, lcinvhdr, lcpackhdr, lcupsbox, lcengchrg, lcflfields, lcscfields, lcstyhdr, lcemptysty, lcinstlin, lcinsthdr, lcappcrdt, lcmjrmsk, lcstymjr, lcconsinvh, lcconsinvd
lctempstk = ''
STORE '' TO lcpackline
STORE '' TO lcordcanln
STORE '' TO lcteom, lctcod
STORE 0 TO lntdaysdue, lnterdiscr
STORE 20 TO lneomday
STORE .F. TO m.lupsins, m.lnewline, m.lpacked, m.lbackord
STORE 0 TO m.ntaxrate, m.nchrgtax, m.nmerchtax
lcorder = SPACE(6)
DIMENSION laengstyta[1, 2]
STORE 0 TO lntaxrate
STORE '' TO laengstyta
PRIVATE lcupdpik
STORE SPACE(0) TO lcupdpik
lcupdpik = gfgetmemvar('M_MPKTKIN', oariaapplication.activecompanyid)
SELECT ordline
SCATTER BLANK MEMO MEMVAR
SELECT invhdr
SCATTER BLANK MEMVAR
lcmjrmsk = gfitemmask('PM')
lcemptysty = PADR(STRTRAN(gfitemmask('PI'), 'X', ' '), 19)
lcstyhdr = gfitemmask('HI')
lcstymjr = gfitemmask('HM')
lnstylngth = LEN(lcstyhdr)
lcscfields = 'Order,CustPo,PikTkt,Account,Store,InvDate,cWareCode,Season,                Comm1,Comm2,Invoice,Flag,cDivision,CCURRCODE'
lcflfields = 'SHIPDATE,DUEDATE,APPROVAL,cTermCode,SHIPVIA,SPCINST,NCHRGTAX,' + 'DEPT,NOTE1,NOTE2,REP1,REP2,lUpsIns,Cod_Flag,NEXRATE,cFacCode,' + 'CARTONS,WEIGHT,CODTAG,COD_AMT,TRDE_DISC,SHIPAMT,DISCPCNT,DISCOUNT,' + 'FREIGHT,INSUR,COD,TAX_AMT,TOTALCHG,NCHARGES,NCURRUNIT,SHIP,' + 'nPstAmt,cTaxRule,TAX_RATE,nPstRate,nHstRate,nHstAmt,BOL_NO,APPRAMT,DPOSTDATE,CCODTRCKNO,STATUS'
lcsession = gfsequence('CSESSION')
lcglsessio = gfsequence('GLSESSION')
lasetups[1, 1] = 'M_PACK'
lasetups[2, 1] = 'M_STY_COM'
lasetups[3, 1] = 'M_LN_NOTE'
lasetups[4, 1] = 'M_LINK_GL'
lasetups[5, 1] = 'M_WareHouse'
lasetups[6, 1] = 'M_GenOrNum'
lasetups[7, 1] = 'M_COST_METH'
lasetups[8, 1] = 'M_DYELOT'
lasetups[9, 1] = 'M_TAX'
lasetups[10, 1] = 'M_TAX_RATE'
lasetups[11, 1] = 'M_TAX_METH'
lasetups[12, 1] = 'M_UPC_USE'
lasetups[13, 1] = 'M_DIV_LINK'
lasetups[14, 1] = 'M_REP_COMM'
lasetups[15, 1] = 'M_UPSBOX'
lasetups[16, 1] = 'XAGINGTYPE'
lasetups[17, 1] = 'XPOSTFINV'
lasetups[18, 1] = 'XUPSFROM'
lasetups[19, 1] = 'M_CRDT_LMT'
lasetups[20, 1] = 'M_EDTPRICE'
lasetups[21, 1] = 'M_INVBULK'
lasetups[22, 1] = 'M_INVHOLD'
lasetups[23, 1] = 'M_BACKORD'
lasetups[24, 1] = 'M_TAX_DESC'
lasetups[26, 1] = 'M_HST_RATE'
= gfgetmemvar(@lasetups, oariaapplication.activecompanyid)
lctaxname = IIF(EMPTY(lasetups(24, 2)), 'G.S.T. Tax', lasetups(24, 2))
lcinvhdr = loformset.lcinvhdr
lcinvline = loformset.lcinvline
lcconsinvh = loformset.lcconsinvh
lcconsinvd = loformset.lcconsinvd
lcupsbox = loformset.lcupsbox
lcpackhdr = loformset.lcpackhdr
lcinsthdr = loformset.lcinsthdr
lcinstlin = loformset.lcinstlin
lcappcrdt = loformset.lcappcrdt
lctempstk = loformset.lctempstk
lcpackline = loformset.lcpackline
lcordcanln = loformset.lcordcanln
lccrttmp = loformset.lccrttmp
lcupstmp = loformset.lcupstmp
lliscanada = IIF(UPPER(ALLTRIM(oariaapplication.defaultcountry)) = 'CANADA', .T., .F.)
IF UPPER(ALLTRIM(oariaapplication.defaultcountry)) = 'ENG'
 DIMENSION laengstyta[1, 2]
 laengstyta[1, 1] = 'NTAXRATE'
 laengstyta[1, 2] = 'lnTaxRate'
 STORE '' TO lctaxtitle, lctaxbreak
 llisenglan = .T.
 lcengchrg = gftempname()
 lasetups[15, 2] = 'N'
ELSE
 llisenglan = .F.
ENDIF
llmulcurr = gfgetmemvar('llMulCurr', oariaapplication.activecompanyid)
lleditexrt = gfgetmemvar('LLEDITEXRA', oariaapplication.activecompanyid)
latrltfld[1, 1] = 'NTERDISCR'
latrltfld[1, 2] = 'lnTerDiscR'
latrltfld[2, 1] = 'EOM'
latrltfld[2, 2] = 'lcTEOM'
latrltfld[3, 1] = 'NTERDUED'
latrltfld[3, 2] = 'lnTDaysDue'
latrltfld[4, 1] = 'CODYN'
latrltfld[4, 2] = 'lcTCod'
latrltfld[5, 1] = 'LINSTALLM'
latrltfld[5, 2] = 'llInstTerm'
latrltfld[6, 1] = 'EOMDAY'
latrltfld[6, 2] = 'lnEomDay'
IF !USED('WAREHOUS')
 = gfopentable('WAREHOUS', 'WAREHOUS', 'SH')
ENDIF
IF 'CR' $ oariaapplication.companyinstalledmodules
 STORE .T. TO llcrfirst
 = gfopentable('ORDCHARG', 'ORDER', 'SH')
ENDIF
llinvchrg = llisenglan .AND. gfopentable('InvChrg', 'InvChrg', 'SH')
IF 'AL' $ oariaapplication.companyinstalledmodules
 llpiktkt = gfopentable('PIKTKT', 'Ordpik', 'SH')
 SELECT piktkt
 = gfseek("")
 SET FILTER TO status <> 'X' .AND. status <> 'C'
ENDIF
= gfopentable('Customer', 'Customer', 'SH', "DistCntr")
llopnfiles = .T.
= lfcrattemp()
SELECT &lctmppktk
LOCATE
=gfseek('M'+&lctmppktk..account,'CUSTOMER')
SCAN
 WAIT WINDOW NOWAIT 'Preparing files to create an invoice for piktkt : '+ &lctmppktk..piktkt
 =gfseek('O'+&lctmppktk..ORDER,'ORDHDR')    
 =lfgetorder(&lctmppktk..ORDER,&lctmppktk..STORE,.T.,&lctmppktk..piktkt,&lctmppktk..piktkt)
ENDSCAN
loformset.lformhastriggers = .T.
IF ASCAN(loformset.laevnttrig, 'INVCHARG') = 0
 DIMENSION loformset.laevnttrig[ALEN(loformset.laevnttrig, 1) + 1, 4]
 loformset.laevnttrig[ALEN(loformset.laevnttrig, 1), 1] = PADR('INVCHARG', 10)
 loformset.laevnttrig[ALEN(loformset.laevnttrig, 1), 2] = "PANMAIN"
 loformset.laevnttrig[ALEN(loformset.laevnttrig, 1), 3] = "'INVCHARG'"
 loformset.laevnttrig[ALEN(loformset.laevnttrig, 1), 4] = "C"
ENDIF
IF ASCAN(loformset.laevnttrig, 'ALSAVINV') = 0
 DIMENSION loformset.laevnttrig[ALEN(loformset.laevnttrig, 1) + 1, 4]
 loformset.laevnttrig[ALEN(loformset.laevnttrig, 1), 1] = PADR('ALSAVINV', 10)
 loformset.laevnttrig[ALEN(loformset.laevnttrig, 1), 2] = "bn4main"
 loformset.laevnttrig[ALEN(loformset.laevnttrig, 1), 3] = "'ALSAVINV'"
 loformset.laevnttrig[ALEN(loformset.laevnttrig, 1), 4] = "C"
ENDIF
IF ASCAN(loformset.laevnttrig, 'DLARBIN') = 0
 DIMENSION loformset.laevnttrig[ALEN(loformset.laevnttrig, 1) + 1, 4]
 loformset.laevnttrig[ALEN(loformset.laevnttrig, 1), 1] = PADR('DLARBIN', 10)
 loformset.laevnttrig[ALEN(loformset.laevnttrig, 1), 2] = "bn4main"
 loformset.laevnttrig[ALEN(loformset.laevnttrig, 1), 3] = "'DLARBIN'"
 loformset.laevnttrig[ALEN(loformset.laevnttrig, 1), 4] = "C"
ENDIF
IF ASCAN(loformset.laevnttrig, 'TRNHIST') = 0
 DIMENSION loformset.laevnttrig[ALEN(loformset.laevnttrig, 1) + 1, 4]
 loformset.laevnttrig[ALEN(loformset.laevnttrig, 1), 1] = PADR('TRNHIST', 10)
 loformset.laevnttrig[ALEN(loformset.laevnttrig, 1), 2] = "DIRMAIN"
 loformset.laevnttrig[ALEN(loformset.laevnttrig, 1), 3] = "'TRNHIST'"
 loformset.laevnttrig[ALEN(loformset.laevnttrig, 1), 4] = "C"
ENDIF
IF ASCAN(loformset.laevnttrig, 'GFSTYCRL') = 0
 DIMENSION loformset.laevnttrig[ALEN(loformset.laevnttrig, 1) + 1, 4]
 loformset.laevnttrig[ALEN(loformset.laevnttrig, 1), 1] = PADR('GFSTYCRL', 10)
 loformset.laevnttrig[ALEN(loformset.laevnttrig, 1), 2] = "bn4main"
 loformset.laevnttrig[ALEN(loformset.laevnttrig, 1), 3] = "'GFSTYCRL'"
 loformset.laevnttrig[ALEN(loformset.laevnttrig, 1), 4] = "C"
ENDIF
SELECT &lcinvhdr
SET ORDER TO
SET DELETED OFF
GOTO TOP
SCAN
 IF consol <> 'Y' .AND. SEEK(account + order + store + piktkt + 'Y' + SPACE(10), lcinvline)
  SELECT (lcinvline)
  LOCATE REST FOR account+ORDER+STORE+piktkt+'Y'+SPACE(10) = &lcinvhdr..account+ &lcinvhdr..ORDER+&lcinvhdr..STORE+&lcinvhdr..piktkt+'Y'+SPACE(10)  AND totqty <> 0
  IF FOUND()
   = gfmodalgen('TRM40153B00000', 'ALERT')
   llcsave = .F.
   EXIT
  ENDIF
  SELECT (lcinvhdr)
 ENDIF
 IF consol = 'Y' .OR. EMPTY(flag)
  llsaveinv = .F.
  DO gfchksavinv IN (oariaapplication.applicationhome+'AR\ARINV.PRG') WITH account, order, store, piktkt, lcinvhdr, IIF(USED(lcinsthdr), lcinsthdr, ''), IIF(USED(lcappcrdt), lcappcrdt, ''), IIF(USED(lcupsbox), lcupsbox, ''), 'llSaveInv', .F., .F., '', loformset
  IF !llsaveinv
   DELETE
  ENDIF
 ENDIF
 SELECT &lcinvhdr
 REPLACE dpostdate WITH oariaapplication.systemdate
 DIMENSION ladata[5]
 ladata[1] = order
 ladata[3] = piktkt
 ladata[5] = store
 m.ncharges = 0
 = lfupdirchg()
ENDSCAN
SELECT &lcinvhdr
SET DELETED ON
GOTO TOP
loformset.ariaform1.ariapageframe1.page4.cntinvoicesummary.chargesfile = lcengchrg
loformset.lcinvhdr = lcinvhdr
loformset.lcinvline = lcinvline
lcoldname = loformset.name
loformset.name = 'AWRARIINV'
DO gpsaveinv IN (oariaapplication.applicationhome+'AR\ARINV.PRG') WITH lcinvhdr, lcinvline, IIF(USED(lcupsbox), lcupsbox, ''), IIF(USED(lcengchrg), lcengchrg, ''), IIF(USED(lcinsthdr), lcinsthdr, ''), IIF(USED(lcinstlin), lcinstlin, ''), IIF(USED(lcordcanln), lcordcanln, ''), IIF(USED(lcappcrdt), lcappcrdt, ''), lcglsessio, 'laInv', .F., .F., loformset
IF ASCAN(loformset.laEvntTrig , PADR('INVCHARG',10)) <> 0
  loformset.mDoTrigger(PADR('INVCHARG',10)) 
ENDIF 
loformset.name = lcoldname
= gfclosetable('DistCntr')
DO lpreleaslk
IF USED(lcinvhdr)
 USE IN (lcinvhdr)
ENDIF
ERASE (oariaapplication.workdir + lcinvhdr + '.DBF')
ERASE (oariaapplication.workdir + lcinvhdr + '.CDX')
IF USED(lcinvline)
 USE IN (lcinvline)
ENDIF
ERASE (oariaapplication.workdir + lcinvline + '.DBF')
ERASE (oariaapplication.workdir + lcinvline + '.CDX')
ERASE (oariaapplication.workdir + lcinvline + '.FPT')
IF USED(lcupsbox)
 USE IN (lcupsbox)
ENDIF
ERASE (oariaapplication.workdir + lcupsbox + '.DBF')
ERASE (oariaapplication.workdir + lcupsbox + '.CDX')
IF USED(lcinsthdr)
 USE IN (lcinsthdr)
ENDIF
ERASE (oariaapplication.workdir + lcinsthdr + '.DBF')
ERASE (oariaapplication.workdir + lcinsthdr + '.CDX')
IF USED(lcinstlin)
 USE IN (lcinstlin)
ENDIF
ERASE (oariaapplication.workdir + lcinstlin + '.DBF')
ERASE (oariaapplication.workdir + lcinstlin + '.CDX')
IF USED(lcappcrdt)
 USE IN (lcappcrdt)
ENDIF
ERASE (oariaapplication.workdir + lcappcrdt + '.DBF')
ERASE (oariaapplication.workdir + lcappcrdt + '.CDX')
IF llisenglan .AND. USED(lcengchrg)
 USE IN (lcengchrg)
ENDIF
ERASE (oariaapplication.workdir + lcengchrg + '.DBF')
ERASE (oariaapplication.workdir + lcengchrg + '.CDX')
IF USED(lcordcanln)
 USE IN (lcordcanln)
ENDIF
ERASE (oariaapplication.workdir + lcordcanln + '.DBF')
ERASE (oariaapplication.workdir + lcordcanln + '.CDX')
IF USED(lcconsinvh)
 USE IN (lcconsinvh)
ENDIF
ERASE (oariaapplication.workdir + lcconsinvh + '.DBF')
ERASE (oariaapplication.workdir + lcconsinvh + '.CDX')
IF USED(lcconsinvd)
 USE IN (lcconsinvd)
ENDIF
ERASE (oariaapplication.workdir + lcconsinvd + '.DBF')
ERASE (oariaapplication.workdir + lcconsinvd + '.CDX')
ERASE (oariaapplication.workdir + lcconsinvd + '.FPT')
IF USED(lctempstk)
 USE IN (lctempstk)
ENDIF
ERASE (oariaapplication.workdir + lctempstk + '.DBF')
ERASE (oariaapplication.workdir + lctempstk + '.CDX')
IF 'AL' $ oariaapplication.companyinstalledmodules
 SELECT piktkt
 SET FILTER TO
ENDIF
SELECT invhdr
= gftableupdate()
SELECT invline
= gftableupdate()
SELECT customer
= gftableupdate()
SELECT style
= gftableupdate()
SELECT stydye
= gftableupdate()
SELECT ordhdr
= gftableupdate()
SELECT ordline
= gftableupdate()
TRY
 IF USED('STYINVJL')
  SELECT styinvjl
  TABLEUPDATE(.T., .T.)
 ENDIF
CATCH
ENDTRY
IF USED('InvChrg')
 SELECT invchrg
 = gftableupdate()
ENDIF
ENDPROC
**
PROCEDURE LFCRATTEMP
PRIVATE lafilestru, lnfilestru, laindex
SELECT invhdr
= AFIELDS(lafilestru)
lnfilestru = ALEN(lafilestru, 1)
DIMENSION lafilestru[lnfilestru + 17, 18]
lafilestru[lnfilestru + 1, 1] = 'cSelect'
lafilestru[lnfilestru + 1, 2] = 'C'
lafilestru[lnfilestru + 1, 3] = 1
lafilestru[lnfilestru + 1, 4] = 0
lafilestru[lnfilestru + 2, 1] = 'Picked'
lafilestru[lnfilestru + 2, 2] = 'N'
lafilestru[lnfilestru + 2, 3] = 7
lafilestru[lnfilestru + 2, 4] = 0
lafilestru[lnfilestru + 3, 1] = 'lUpsIns'
lafilestru[lnfilestru + 3, 2] = 'L'
lafilestru[lnfilestru + 3, 3] = 1
lafilestru[lnfilestru + 3, 4] = 0
lafilestru[lnfilestru + 4, 1] = 'nSteps'
lafilestru[lnfilestru + 4, 2] = 'N'
lafilestru[lnfilestru + 4, 3] = 6
lafilestru[lnfilestru + 4, 4] = 0
lafilestru[lnfilestru + 5, 1] = 'nChrgTax'
lafilestru[lnfilestru + 5, 2] = 'N'
lafilestru[lnfilestru + 5, 3] = 13
lafilestru[lnfilestru + 5, 4] = 2
lafilestru[lnfilestru + 6, 1] = 'nMerchTax'
lafilestru[lnfilestru + 6, 2] = 'N'
lafilestru[lnfilestru + 6, 3] = 13
lafilestru[lnfilestru + 6, 4] = 5
lafilestru[lnfilestru + 7, 1] = 'lCompUps'
lafilestru[lnfilestru + 7, 2] = 'L'
lafilestru[lnfilestru + 7, 3] = 1
lafilestru[lnfilestru + 7, 4] = 0
lafilestru[lnfilestru + 8, 1] = 'LastLine'
lafilestru[lnfilestru + 8, 2] = 'N'
lafilestru[lnfilestru + 8, 3] = 6
lafilestru[lnfilestru + 8, 4] = 0
lafilestru[lnfilestru + 9, 1] = 'LKEYOFF'
lafilestru[lnfilestru + 9, 2] = 'L'
lafilestru[lnfilestru + 9, 3] = 1
lafilestru[lnfilestru + 9, 4] = 0
lafilestru[lnfilestru + 10, 1] = 'NTAXDUE'
lafilestru[lnfilestru + 10, 2] = 'N'
lafilestru[lnfilestru + 10, 3] = 17
lafilestru[lnfilestru + 10, 4] = 6
lafilestru[lnfilestru + 11, 1] = 'NCARTONS'
lafilestru[lnfilestru + 11, 2] = 'N'
lafilestru[lnfilestru + 11, 3] = 11
lafilestru[lnfilestru + 11, 4] = 5
lafilestru[lnfilestru + 12, 1] = 'Ordered'
lafilestru[lnfilestru + 12, 2] = 'N'
lafilestru[lnfilestru + 12, 3] = 7
lafilestru[lnfilestru + 12, 4] = 0
lafilestru[lnfilestru + 13, 1] = 'cConStore'
lafilestru[lnfilestru + 13, 2] = 'C'
lafilestru[lnfilestru + 13, 3] = 8
lafilestru[lnfilestru + 13, 4] = 0
lafilestru[lnfilestru + 14, 1] = 'NTrueShip'
lafilestru[lnfilestru + 14, 2] = 'N'
lafilestru[lnfilestru + 14, 3] = 17
lafilestru[lnfilestru + 14, 4] = 5
lafilestru[lnfilestru + 15, 1] = 'NTrueDscnt'
lafilestru[lnfilestru + 15, 2] = 'N'
lafilestru[lnfilestru + 15, 3] = 17
lafilestru[lnfilestru + 15, 4] = 5
lafilestru[lnfilestru + 16, 1] = 'NTrueChrg'
lafilestru[lnfilestru + 16, 2] = 'N'
lafilestru[lnfilestru + 16, 3] = 17
lafilestru[lnfilestru + 16, 4] = 5
lafilestru[lnfilestru + 17, 1] = 'NTruTaxRat'
lafilestru[lnfilestru + 17, 2] = 'N'
lafilestru[lnfilestru + 17, 3] = 17
lafilestru[lnfilestru + 17, 4] = 5
FOR lnc = lnfilestru TO lnfilestru + 17
 STORE ' ' TO lafilestru[lnc, 7], lafilestru[lnc, 8], lafilestru[lnc, 9], lafilestru[lnc, 10], lafilestru[lnc, 11], lafilestru[lnc, 12], lafilestru[lnc, 13], lafilestru[lnc, 14], lafilestru[lnc, 15], lafilestru[lnc, 16]
 STORE 0 TO lafilestru[lnc, 17], lafilestru[lnc, 18]
ENDFOR
DIMENSION laindex[3, 2]
laindex[1, 1] = 'Account+Order+Store+PikTkt+CDIVISION'
laindex[1, 2] = lcinvhdr
laindex[2, 1] = 'cSelect+Account+Order+Store+PikTkt+CDIVISION'
laindex[2, 2] = 'Select'
laindex[3, 1] = 'Consol+Account+cDivision+cCurrCode+CDIVISION'
laindex[3, 2] = 'Consol'
= gfcrttmp(lcinvhdr, @lafilestru, @laindex)
= gfcrttmp(lcconsinvh, @lafilestru, 'Consol+Account+cDivision+cCurrCode', lcconsinvh)
SELECT (lcinvhdr)
SET ORDER TO (lcinvhdr)
SCATTER MEMVAR FIELDS &lcflfields BLANK
SCATTER FIELDS &lcscfields TO ladata BLANK
SELECT ordline
= AFIELDS(lafilestru)
lnfilestru = ALEN(lafilestru, 1)
DIMENSION lafilestru[lnfilestru + 13, 18]
lafilestru[lnfilestru + 1, 1] = 'LNEWLINE'
lafilestru[lnfilestru + 1, 2] = 'L'
lafilestru[lnfilestru + 1, 3] = 0
lafilestru[lnfilestru + 1, 4] = 0
lafilestru[lnfilestru + 2, 1] = 'LPACKED'
lafilestru[lnfilestru + 2, 2] = 'L'
lafilestru[lnfilestru + 2, 3] = 0
lafilestru[lnfilestru + 2, 4] = 0
lafilestru[lnfilestru + 3, 1] = 'LBACKORD'
lafilestru[lnfilestru + 3, 2] = 'L'
lafilestru[lnfilestru + 3, 3] = 0
lafilestru[lnfilestru + 3, 4] = 0
lafilestru[lnfilestru + 4, 1] = 'nSteps'
lafilestru[lnfilestru + 4, 2] = 'N'
lafilestru[lnfilestru + 4, 3] = 6
lafilestru[lnfilestru + 4, 4] = 0
lafilestru[lnfilestru + 5, 1] = 'nTaxRate'
lafilestru[lnfilestru + 5, 2] = 'N'
lafilestru[lnfilestru + 5, 3] = 10
lafilestru[lnfilestru + 5, 4] = 2
lafilestru[lnfilestru + 6, 1] = 'cCurrCode'
lafilestru[lnfilestru + 6, 2] = 'C'
lafilestru[lnfilestru + 6, 3] = 3
lafilestru[lnfilestru + 6, 4] = 0
lafilestru[lnfilestru + 7, 1] = 'cDivision'
lafilestru[lnfilestru + 7, 2] = 'C'
lafilestru[lnfilestru + 7, 3] = 6
lafilestru[lnfilestru + 7, 4] = 0
lafilestru[lnfilestru + 8, 1] = 'Consol'
lafilestru[lnfilestru + 8, 2] = 'C'
lafilestru[lnfilestru + 8, 3] = 1
lafilestru[lnfilestru + 8, 4] = 0
lafilestru[lnfilestru + 9, 1] = 'nNetAmnt'
lafilestru[lnfilestru + 9, 2] = 'N'
lafilestru[lnfilestru + 9, 3] = 18
lafilestru[lnfilestru + 9, 4] = 10
lafilestru[lnfilestru + 10, 1] = 'nGrosAmnt'
lafilestru[lnfilestru + 10, 2] = 'N'
lafilestru[lnfilestru + 10, 3] = 18
lafilestru[lnfilestru + 10, 4] = 10
lafilestru[lnfilestru + 11, 1] = 'LTAXABLE'
lafilestru[lnfilestru + 11, 2] = 'L'
lafilestru[lnfilestru + 11, 3] = 0
lafilestru[lnfilestru + 11, 4] = 0
lafilestru[lnfilestru + 12, 1] = 'cDyeFlag'
lafilestru[lnfilestru + 12, 2] = 'C'
lafilestru[lnfilestru + 12, 3] = 1
lafilestru[lnfilestru + 12, 4] = 0
lafilestru[lnfilestru + 13, 1] = 'cConStore'
lafilestru[lnfilestru + 13, 2] = 'C'
lafilestru[lnfilestru + 13, 3] = 8
lafilestru[lnfilestru + 13, 4] = 0
FOR lnc = lnfilestru TO lnfilestru + 13
 STORE ' ' TO lafilestru[lnc, 7], lafilestru[lnc, 8], lafilestru[lnc, 9], lafilestru[lnc, 10], lafilestru[lnc, 11], lafilestru[lnc, 12], lafilestru[lnc, 13], lafilestru[lnc, 14], lafilestru[lnc, 15], lafilestru[lnc, 16]
 STORE 0 TO lafilestru[lnc, 17], lafilestru[lnc, 18]
ENDFOR
DIMENSION laindex[4, 2]
laindex[1, 1] = 'Account+Order+Store+PikTkt+STR(LineNo,6)'
laindex[1, 2] = lcinvline
laindex[2, 1] = 'Account+Order+Store+PikTkt+Style'
laindex[2, 2] = 'Styles'
laindex[3, 1] = 'Consol+Account+cDivision+cCurrCode+Style'
laindex[3, 2] = 'Consol'
laindex[4, 1] = 'Account+Order+Store+PikTkt+cDyeFlag+Dyelot'
laindex[4, 2] = 'Dyelot'
= gfcrttmp(lcinvline, @lafilestru, @laindex)
= gfcrttmp(lcconsinvd, @lafilestru, 'Consol+Account+cDivision+cCurrCode+cWareCode+Style', lcconsinvd)
SELECT (lcinvline)
SET ORDER TO (lcinvline)
SCATTER BLANK MEMO MEMVAR
IF lasetups(15, 2) = 'Y'
 = gfopentable('UPSBOX', 'UPSBOX', 'SH')
 SELECT upsbox
 = AFIELDS(lafilestru)
 = gfclosetable('UPSBOX')
 lnfilestru = ALEN(lafilestru, 1)
 DIMENSION lafilestru[lnfilestru + 2, 18]
 lafilestru[lnfilestru + 1, 1] = 'Order'
 lafilestru[lnfilestru + 1, 2] = 'C'
 lafilestru[lnfilestru + 1, 3] = 6
 lafilestru[lnfilestru + 1, 4] = 0
 lafilestru[lnfilestru + 2, 1] = 'PikTkt'
 lafilestru[lnfilestru + 2, 2] = 'C'
 lafilestru[lnfilestru + 2, 3] = 6
 lafilestru[lnfilestru + 2, 4] = 0
 FOR lnc = lnfilestru TO lnfilestru + 2
  STORE ' ' TO lafilestru[lnc, 7], lafilestru[lnc, 8], lafilestru[lnc, 9], lafilestru[lnc, 10], lafilestru[lnc, 11], lafilestru[lnc, 12], lafilestru[lnc, 13], lafilestru[lnc, 14], lafilestru[lnc, 15], lafilestru[lnc, 16]
  STORE 0 TO lafilestru[lnc, 17], lafilestru[lnc, 18]
 ENDFOR
 = gfcrttmp(lcupsbox, @lafilestru, 'Order+Store+PikTkt+STR(CARTONS,5)', lcupsbox)
ENDIF
IF llisenglan
 IF !USED('InvChrg')
  = gfopenatable('InvChrg', 'InvChrg', 'SH')
 ENDIF
 SELECT invchrg
 = AFIELDS(lafilestru)
 = gfcrttmp(lcengchrg, @lafilestru, 'Order+cStore+PikTkt+cchrgcode', lcengchrg)
ENDIF
= gfcrttmp(lcinsthdr, '(Order C(6),Store C(8),PikTkt C(6), cInstmType C(1), nInstmFreq N(3),nInstIAmnt N(11,2),dInstmStDt D, nNoInstm N(3), cInstmRef C(30),nInstIPcnt N(6,2),nInstmAmnt N(10,2))', 'Order+Store+PikTkt', lcinsthdr)
= gfcrttmp(lcinstlin, '(Order C(6),Store C(8),PikTkt C(6), cInstalNo C(3),nInstmAmnt N(10,2),DueDate D,nInstmPcnt N(6,2),cInstmNote C(30))', 'Order+Store+PikTkt+cInstalNo', lcinstlin)
= gfcrttmp(lcordcanln, '(cOrdType C(1),Order C(6),LineNo N(6),Qty1 N(6), Qty2 N(6),Qty3 N(6),Qty4 N(6),Qty5 N(6),Qty6 N(6),Qty7 N(6), Qty8 N(6),TotQty N(7),Cancelled D,cCancReson C(6),Price N(12,2))', 'CORDTYPE+ORDER+STR(LINENO,6)', lcordcanln)
= gfcrttmp(lcpackline, '(Pack_No C(6),Order C(6),nOrdLineNo N(6),Qty1 N(6), Qty2 N(6),Qty3 N(6),Qty4 N(6),Qty5 N(6),Qty6 N(6),Qty7 N(6), Qty8 N(6),TotQty N(7))', 'PACK_NO+ORDER+STR(nOrdLineNo,6)', lcpackline)
= gfcrttmp(lctempstk, '(Style C(19)    , Qty1 N(10)     , Qty2 N(10)     , Qty3 N(10) ,  Qty4 N(10)     , Qty5 N(10)     , Qty6 N(10)     , Qty7 N(10) ,  Qty8 N(10)     , TotQty N(10)   , cWareCode C(6) , Dyelot c(10))', 'Style + cWareCode + Dyelot', lctempstk)
ENDPROC
**
FUNCTION LFGETORDER
PARAMETER lcorderno, lcstoreno, llpicked, lcpikfrom, lcpikto, llscope
PRIVATE ldduedate, lcstore, lccustlink, lccustsale, lcsalerep1, lcsalerep2, lccurrcode, lnexrate, lncurrunit, llpacked, lclinescon, lntaxqty, lntaxrate, llbackord
PRIVATE lnallcartn
lnallcartn = 0
IF 'AL' $ oariaapplication.companyinstalledmodules
 IF llopnfiles
  = gfopentable('PACK_HDR', 'Orderpck', 'SH')
  = gfopentable('PACK_LIN', 'PACK_LIN', 'SH')
 ENDIF
ENDIF
IF !llscope
 IF llopnfiles
  = gfopentable('STYLE', 'STYLE', 'SH')
  = gfopentable('STYDYE', 'STYDYE', 'SH')
  = gfopentable('SCALE', 'SCALE', 'SH')
 ENDIF
 SELECT style
 SELECT (lcinvline)
ENDIF
IF llpicked
 lclinescon = 'Picked .AND. TotPik > 0'
 DO CASE
  CASE !EMPTY(lcpikfrom) .AND. EMPTY(lcpikto)
   lclinescon = lclinescon + 'AND PikTkt >= lcPikFrom'
  CASE EMPTY(lcpikfrom) .AND. !EMPTY(lcpikto)
   lclinescon = lclinescon + 'AND PikTkt <= lcPikTo'
  CASE !EMPTY(lcpikfrom) .AND. !EMPTY(lcpikto)
   lclinescon = lclinescon + 'AND BETWEEN(PikTkt,lcPikFrom,lcPikTo)'
 ENDCASE
 DO CASE
  CASE llscope .AND. !EMPTY(ldfrompick) .AND. EMPTY(ldtopick)
   lclinescon = lclinescon + 'AND pikdate >= ldFromPick'
  CASE llscope .AND. EMPTY(ldfrompick) .AND. !EMPTY(ldtopick)
   lclinescon = lclinescon + 'AND pikdate <= ldToPick'
  CASE llscope .AND. !EMPTY(ldfrompick) .AND. !EMPTY(ldtopick)
   lclinescon = lclinescon + 'AND BETWEEN(pikdate,ldFromPick,ldToPick)'
 ENDCASE
ELSE
 lclinescon = 'TotQty >0'
ENDIF
IF llscope .AND. !EMPTY(lastytarge)
 lclinescon = lclinescon + ' AND ASCAN(laStyTarge,ALLTRIM(SUBSTR(Style,1,LEN(lcMjrMsk))))>0'
ENDIF
= gfrltfld(ordhdr.ctermcode, @latrltfld, 'CTERMCODE')
lneomday = IIF(TYPE('lnEOMDay') <> 'N' .OR. lneomday = 0, 20, lneomday - 1)
lddefinvda = oariaapplication.systemdate
IF llisenglan
 ldduedate = IIF(lcteom <> 'Y', lddefinvda + lntdaysdue, CTOD('01' + SUBSTR(DTOC(GOMONTH(lddefinvda, 1)), 3)) - 1 + lntdaysdue)
ELSE
 ldduedate = IIF(lcteom <> 'Y', lddefinvda + lntdaysdue, GOMONTH(CTOD(SUBSTR(DTOC(lddefinvda), 1, 3) + '10' + SUBSTR(DTOC(lddefinvda), 6, 5)), IIF(DAY(lddefinvda) > lneomday, 2, 1)) + lntdaysdue)
ENDIF
lccurrcode = IIF(EMPTY(ordhdr.ccurrcode), oariaapplication.basecurrency, ordhdr.ccurrcode)
STORE 1 TO lnexrate, lncurrunit
IF lccurrcode <> oariaapplication.basecurrency
 lnexrate = gfchkrate('lnCurrUnit', lccurrcode, lddefinvda, .T., .F., .F., lleditexrt)
 IF lnexrate = 0
  IF lleditexrt
   = gfmodalgen('INM00262B00000', 'ALERT', ALLTRIM(lccurrcode) + '|' + ALLTRIM(oariaapplication.basecurrency) + '|' + DTOC(lddefinvda))
  ELSE
   lccurrcode = oariaapplication.basecurrency
   STORE 1 TO lnexrate, lncurrunit
  ENDIF
 ENDIF
ENDIF
= gfseek('M' + ordhdr.account, 'Customer')
IF EMPTY(ordhdr.store)
 lcphone = customer.phone1
ENDIF
llbackord = INLIST(IIF(EMPTY(customer.cbackord), lasetups(23, 2), customer.cbackord), 'A', 'I')
SELECT ordline
gfsetorder('Ordlinst')
= gfseek('O' + lcorderno + ALLTRIM(lcstoreno))
DO WHILE cordtype+order+store+style+STR(lineno, 6)='O'+lcorderno+ALLTRIM(lcstoreno)
 lcstore = store
 = IIF(EMPTY(store), gfseek('M' + account, 'Customer'), gfseek('S' + account + store, 'Customer'))
 lcsalerep1 = ordhdr.rep1
 lncomm1 = ordhdr.comm1
 lcsalerep2 = ordhdr.rep2
 lncomm2 = ordhdr.comm2
 IF ordhdr.multi = 'Y' .AND. EMPTY(ordhdr.rep1) .AND. EMPTY(ordhdr.rep2)
  lcsalerep1 = customer.salesrep
  lncomm1 = customer.comm
  lcsalerep2 = customer.rep2
  lncomm2 = customer.comm2
 ENDIF
 SCAN REST FOR EVALUATE(lclinescon) WHILE cordtype + order + store + style + STR(lineno, 6) = 'O' + lcorderno + lcstore
  llpacked = .F.
  lcseekkey = IIF(EMPTY(ordline.piktkt), 'OrdLine.Order+OrdLine.Store', 'OrdLine.Order+OrdLine.Store+OrdLine.PikTkt')
  IF 'AL' $ oariaapplication.companyinstalledmodules AND  gfseek(&lcseekkey,'Pack_Hdr') AND  gfseek(pack_hdr.pack_no,'Pack_Lin')
   IF !SEEK(IIF(!EMPTY(ordline.piktkt), ordline.piktkt, pack_hdr.pack_no) + ordline.order, lcpackline)
    = lfpacklin(ordline.order, IIF(!EMPTY(ordline.piktkt), ordline.piktkt, pack_hdr.pack_no))
   ENDIF
   llpacked = SEEK(IIF(!EMPTY(ordline.piktkt), ordline.piktkt, pack_hdr.pack_no) + ordline.order + STR(ordline.lineno, 6), lcpackline)
   SELECT ordline
  ENDIF
  SCATTER MEMO MEMVAR
  m.gros_price = IIF(m.gros_price = 0, m.price, m.gros_price)
  = gfseek(m.style, 'Style')
  STORE 0 TO lntaxqty, lntaxrate
  IF llisenglan .AND. lasetups(9, 2) = 'Y' .AND. !customer.lvatexem .AND. style.ntaxbreak <> 0
   = gfrltfld(style.ctaxcode, @laengstyta, 'CTAXCODE')
  ENDIF
  m.ltaxable = style.ltaxable
  IF llpacked
   m.pik1 = &lcpackline..qty1
   m.pik2 = &lcpackline..qty2
   m.pik3 = &lcpackline..qty3
   m.pik4 = &lcpackline..qty4
   m.pik5 = &lcpackline..qty5
   m.pik6 = &lcpackline..qty6
   m.pik7 = &lcpackline..qty7
   m.pik8 = &lcpackline..qty8
   m.totpik = &lcpackline..totqty
   IF !EMPTY(m.prepak)
    = gfseek('P' + m.scale + m.prepak, 'Scale')
    IF scale.pp1 / scale.pptot * m.totpik <> m.pik1 .OR. scale.pp2 / scale.pptot * m.totpik <> m.pik2 .OR. scale.pp3 / scale.pptot * m.totpik <> m.pik3 .OR. scale.pp4 / scale.pptot * m.totpik <> m.pik4 .OR. scale.pp5 / scale.pptot * m.totpik <> m.pik5 .OR. scale.pp6 / scale.pptot * m.totpik <> m.pik6 .OR. scale.pp7 / scale.pptot * m.totpik <> m.pik7 .OR. scale.pp8 / scale.pptot * m.totpik <> m.pik8
     m.prepak = ''
     m.ppqty = 0
    ELSE
     m.ppqty = IIF(scale.pptot = 0, 0, m.totpik / scale.pptot)
    ENDIF
    = gfseek('S' + m.scale, 'Scale')
   ENDIF
  ENDIF
  STORE 0 TO lntotpck
  STORE .F. TO llpikqty
  lntotpck = ordline.npck1 + ordline.npck2 + ordline.npck3 + ordline.npck4 + ordline.npck5 + ordline.npck6 + ordline.npck7 + ordline.npck8
  llpikqty = llpacked .AND. lntotpck > 0
  lcinvno = ''
  IF !SEEK(m.account + m.order + m.store + m.piktkt, lcinvhdr)
  ENDIF
  INSERT INTO (lcinvline) (invoice, order, account, lineno, store, piktkt, style, dyelot, note_mem, comm1, comm2, book1, book2, book3, book4, book5, book6, book7, book8, totbook, flag, pik1, pik2, pik3, pik4, pik5, pik6, pik7, pik8, totpik, qty1, qty2, qty3, qty4, qty5, qty6, qty7, qty8, totqty, price, pack_id, gros_price, disc_pcnt, lpacked, lbackord, ntaxrate, group, prepak, desc1, season, ppqty, scale, cwarecode, consol, cdivision, ccurrcode, ltaxable, cdyeflag, cwarecode, gl_sales) VALUES (lcinvno, m.order, m.account, m.lineno, m.store, m.piktkt, m.style, m.dyelot, m.note_mem, m.comm1, m.comm2, m.qty1, m.qty2, m.qty3, m.qty4, m.qty5, m.qty6, m.qty7, m.qty8, m.totqty, IIF(llpicked, 'B', ' '), m.pik1, m.pik2, m.pik3, m.pik4, m.pik5, m.pik6, m.pik7, m.pik8, m.totpik, IIF(llpicked .OR. llpikqty, m.pik1, m.qty1), IIF(llpicked .OR. llpikqty, m.pik2, m.qty2), IIF(llpicked .OR. llpikqty, m.pik3, m.qty3), IIF(llpicked .OR. llpikqty, m.pik4, m.qty4), IIF(llpicked .OR. llpikqty, m.pik5, m.qty5), IIF(llpicked .OR. llpikqty, m.pik6, m.qty6), IIF(llpicked .OR. llpikqty, m.pik7, m.qty7), IIF(llpicked .OR. llpikqty, m.pik8, m.qty8), IIF(llpicked .OR. llpikqty, m.totpik, m.totqty), m.price, m.pack_id, m.gros_price, m.disc_pcnt, llpacked, llbackord, lntaxrate, m.group, m.prepak, m.desc1, m.season, m.ppqty, m.scale, m.cwarecode, 'N', ordhdr.cdivision, ordhdr.ccurrcode, m.ltaxable, style.cdye_flg, m.cwarecode, ordline.gl_sales)
  SELECT (lcinvline)
  IF INLIST(lasetups(7, 2), 'F', 'L')
  ENDIF
  = gfseek(m.style, 'Style')
  IF llisenglan .AND. lasetups(9, 2) = 'Y' .AND. !customer.lvatexem .AND. style.ntaxbreak <> 0
   FOR lncount = style.ntaxbreak TO 8
    lntaxqty = lntaxqty + EVALUATE(lcinvline + '.Qty' + STR(lncount, 1))
   ENDFOR
  ENDIF
  SCATTER MEMVAR FIELDS qty1, qty2, qty3, qty4, qty5, qty6, qty7, qty8, totqty
  SELECT (lcinvhdr)
  llinvpck = !EMPTY(m.piktkt) .AND. SEEK(m.order + m.store + m.piktkt, 'Pack_Hdr')
  IF !SEEK(m.account + m.order + m.store + m.piktkt)
   APPEND BLANK
   REPLACE cselect WITH '»', invoice WITH lcinvno, order WITH m.order, store WITH m.store, piktkt WITH m.piktkt, custpo WITH IIF(EMPTY(m.custpo), ordhdr.custpo, m.custpo), dist_ctr WITH customer.dist_ctr, account WITH ordhdr.account, ctermcode WITH ordhdr.ctermcode, spcinst WITH ordhdr.spcinst, lupsins WITH (ordhdr.cinsur = 'Y'), rep1 WITH lcsalerep1, comm1 WITH lncomm1, rep2 WITH lcsalerep2, comm2 WITH lncomm2, note1 WITH ordhdr.note1, note2 WITH ordhdr.note2, lcompups WITH .T., consol WITH 'N'
   REPLACE lastline WITH ordhdr.lastline
   IF llinvpck .AND. !EMPTY(pack_hdr.ship_date)
    IF llisenglan
     ldduedate = IIF(lcteom <> 'Y', pack_hdr.ship_date + lntdaysdue, CTOD('01' + SUBSTR(DTOC(GOMONTH(pack_hdr.ship_date, 1)), 3)) - 1 + lntdaysdue)
    ELSE
     ldduedate = IIF(lcteom <> 'Y', pack_hdr.ship_date + lntdaysdue, GOMONTH(CTOD(SUBSTR(DTOC(pack_hdr.ship_date), 1, 3) + '10' + SUBSTR(DTOC(pack_hdr.ship_date), 6, 5)), IIF(DAY(pack_hdr.ship_date) > lneomday, 2, 1)) + lntdaysdue)
    ENDIF
   ENDIF
   REPLACE discpcnt WITH ordhdr.disc, invdate WITH lddefinvda, shipdate WITH IIF(llinvpck .AND. !EMPTY(pack_hdr.ship_date), pack_hdr.ship_date, lddefinvda), dpostdate WITH lddefpstda, duedate WITH ldduedate, dept WITH ordhdr.dept, cfaccode WITH ordhdr.cfaccode, approval WITH ordhdr.approval, appramt WITH ordhdr.appramt, season WITH ordhdr.season, cdivision WITH ordhdr.cdivision, upszone WITH customer.upszone, phone WITH IIF(EMPTY(ordhdr.store), lcphone, customer.phone1), cwarecode WITH IIF(llpicked .AND. gfseek(m.order + m.piktkt, 'PikTkt'), piktkt.cwarecode, ordhdr.cwarecode), trde_disc WITH lnterdiscr, tax_rate WITH IIF(lasetups(9, 2) <> 'Y', 0, IIF(lliscanada, lasetups(10, 2), customer.ntaxrate)), npstrate WITH IIF(lasetups(9, 2) = 'Y' .AND. lliscanada, customer.ntaxrate, 0), ctaxrule WITH IIF(lasetups(9, 2) = 'Y' .AND. lliscanada, customer.ctaxrule, ''), cod_flag WITH IIF(lctcod = 'Y', 'Y', 'N'), status WITH ordhdr.status, ccurrcode WITH lccurrcode, nexrate WITH lnexrate, ncurrunit WITH lncurrunit, dadd_date WITH oariaapplication.systemdate, cadd_time WITH TIME(), cadd_user WITH oariaapplication.user_id
   REPLACE nhstrate WITH IIF(lasetups(9, 2) <> 'Y' .OR. !lliscanada, 0, lasetups(26, 2))
   IF llinvpck
    REPLACE weight WITH pack_hdr.tot_wght, ncartons WITH pack_hdr.tot_cart, cartons WITH pack_hdr.tot_cart
    REPLACE bol_no WITH pack_hdr.bill_ladg
   ENDIF
  ENDIF
  lncartons = ncartons + IIF(style.qty_ctn > 0, m.totqty / style.qty_ctn, 0)
  lnallcartn = IIF(CEILING(lncartons) = 0, 1, CEILING(lncartons))
  REPLACE ordered   WITH ordered  + &lcinvline..totbook , ship      WITH ship     + m.totqty , shipamt   WITH shipamt  + m.totqty*m.price , discount  WITH -shipamt * discpcnt/100, weight    WITH weight   + IIF(llinvpck,0,m.totqty*STYLE.nstyweight) , ncartons  WITH IIF(llinvpck,ncartons,lncartons) , cartons   WITH IIF(llinvpck,cartons,lnallcartn) , picked    WITH picked  + m.totpik , shipvia   WITH IIF(customer.nbrkweight <> 0 AND weight > customer.nbrkweight,customer.caltshpvia, IIF(llinvpck,IIF(ALLTRIM(pack_hdr.shipvia)='*',customer.shipvia,pack_hdr.shipvia),IIF(ALLTRIM(ordhdr.shipvia)='*',customer.shipvia,ordhdr.shipvia))), nmerchtax WITH nmerchtax + lntaxqty * m.price * lntaxrate/100 ,  tax_amt   WITH nmerchtax*(100-discpcnt)/100 , cod_amt   WITH IIF(cod_flag='Y' AND llisengland,shipamt+tax_amt+discount,0) , totalchg  WITH shipamt+tax_amt+discount, ntaxdue   WITH ntaxdue + IIF(m.ltaxable,m.totqty*m.price,0)
  IF llisenglan
   REPLACE ntrueship WITH ntrueship + (m.totqty * m.price), ntruedscnt WITH -ntrueship * discpcnt / 100, ntruechrg WITH ntrueship + nmerchtax + ntruedscnt
  ENDIF
 ENDSCAN
ENDDO
IF !llscope .AND. EOF(lcinvhdr)
 = gfmodalgen('TRM40128B00000', 'ALERT')
 RETURN (.F.)
ENDIF
RETURN
ENDFUNC
**
PROCEDURE LFUPDIRCHG
PRIVATE lnslct, lctaxcode, lcglaccnt, larltfld, lntaxrate
lnslct = SELECT()
DIMENSION larltfld[2, 2], larltfld2[1, 2]
larltfld[1, 1] = 'CTAXCODE'
larltfld[1, 2] = 'lcTaxCode'
larltfld[2, 1] = 'CFRGTACNT'
larltfld[2, 2] = 'lcGlAccnt'
larltfld2[1, 1] = 'NTAXRATE'
larltfld2[1, 2] = 'lnTaxRate'
IF !USED('ORDCHG')
 = gfopentable('ORDCHG', 'ORDCHG', 'SH')
ENDIF
IF gfseek(ladata(1), 'ORDCHG') .AND. !ordchg.invoiced
 SELECT ordchg
 SCAN REST WHILE order + cordchg = ladata(1)
  IF !SEEK(ladata(1) + ladata(5) + ladata(3) + ordchg.cordchg, lcengchrg)
   lcglaccnt = ' '
   lctaxcode = ' '
   = gfrltfld(ordchg.cordchg, @larltfld, 'CCHRGCODE')
   lntaxrate = 0
   = gfrltfld(lctaxcode, @larltfld2, 'CTAXCODE')
   SELECT &lcengchrg
   APPEND BLANK
   REPLACE order WITH ladata(1), piktkt WITH ladata(3), cchrgcode WITH ordchg.cordchg, nchrgamnt WITH ordchg.nordchg, cfrgtacnt WITH lcglaccnt, ntaxrate WITH lntaxrate
   REPLACE cstore WITH ladata(5)
   m.ncharges = m.ncharges + ordchg.nordchg
   m.tax_amt = m.tax_amt + nchrgamnt * ntaxrate
  ENDIF
 ENDSCAN
 = lfvcharges()
ENDIF
SELECT (lnslct)
ENDPROC
**
PROCEDURE LFVCHARGES
PRIVATE lnocharges, lnochrgtax
lascrmode = .F.
DIMENSION lascrmode[4]
lascrmode[4] = .T.
lnocharges = m.ncharges
lnochrgtax = m.nchrgtax
DO gpcharges WITH lcengchrg, SPACE(6), ladata(4), ladata(1), ladata(5), ladata(3), m.trde_disc, 'm.nCharges', 'm.nChrgTax'
SELECT (lcinvhdr)
lcoldhdorder = ORDER()
lnrecno = RECNO()
SET ORDER TO 'CONSOL'
IF SEEK('Y' + account + cdivision + ccurrcode)
 lccurrkey = account + cdivision + ccurrcode
 lnchrgtax = 0
 lntax_amt = 0
 = SEEK('N' + lccurrkey)
 SCAN REST WHILE consol + account + cdivision + ccurrcode = 'N' + lccurrkey
  lndiscpcnt = discpcnt
  lntrde_dis = trde_disc
  lcchrgkey = order + store + piktkt
  SELECT (lcengchrg)
  = SEEK(lcchrgkey)
  SUM nchrgamnt * ntaxrate / 100 TO lninvchrgt REST WHILE order + cstore + piktkt + cchrgcode = lcchrgkey
  lnchrgtax = lnchrgtax + lninvchrgt
  lntax_amt  = lntax_amt + lninvchrgtax + &lcinvhdr..nmerchtax*(100-lndiscpcnt)/100
 ENDSCAN
 = SEEK('Y' + lccurrkey)
 = RLOCK()
 REPLACE ncharges WITH ncharges - lnocharges + m.ncharges, nchrgtax WITH lnchrgtax, tax_amt WITH lntax_amt, cod_amt WITH IIF(cod_flag = 'Y', shipamt + ncharges + tax_amt + discount, 0), totalchg WITH shipamt + ncharges + tax_amt + discount
 UNLOCK
ENDIF
SELECT (lcinvhdr)
SET ORDER TO (lcoldhdorder)
GOTO lnrecno
= RLOCK()
m.tax_amt = m.nchrgtax + nmerchtax * (100 - m.discpcnt) / 100
REPLACE ncharges WITH m.ncharges, nchrgtax WITH m.nchrgtax, tax_amt WITH m.tax_amt, cod_amt WITH IIF(cod_flag = 'Y', shipamt + ncharges + tax_amt + discount, 0), totalchg WITH shipamt + ncharges + tax_amt + discount
UNLOCK
m.cod_amt = cod_amt
m.totalchg = totalchg
ENDPROC
**
PROCEDURE GPCHARGES
PARAMETER lcchrgfile, lcinvoice, lcaccount, lcorderno, lcstore, lcpiktkt, lntrddsc, lctotchrg, lctottax
PRIVATE lnalias, lacharges, lnchrgmark, lntotchrg, lntottax, llcomptax, lachrltfld
PRIVATE lnoldchrg
STORE 0 TO lnoldchrg
lnalias = SELECT()
llvatexmp = .F.
IF !EMPTY(store)
 lncusrec = RECNO('CUSTOMER')
 = gfseek('S' + account + store, 'CUSTOMER')
 llvatexmp = customer.lvatexem
 GOTO lncusrec IN customer
ELSE
 llvatexmp = customer.lvatexem
ENDIF
llcomptax = (gfgetmemvar('M_TAX', gcact_comp) = 'Y') .AND. !llvatexmp
llgllink = (gfgetmemvar('M_LINK_GL', gcact_comp) = 'Y')
lnchrgmark = 0
DIMENSION lalcodes[1, 10], lacharges[1, 2]
STORE '' TO lacharges
lntotchrg = &lctotchrg
= gfopentable('InvChrg', 'InvChrg', 'SH')
SELECT (lcchrgfile)
IF lascrmode(4)
 = SEEK(lcorderno + lcstore + lcpiktkt)
 SUM nchrgamnt * (1 - lntrddsc / 100) * ntaxrate / 100 TO lntottax REST WHILE order + cstore + piktkt + cchrgcode = lcorderno + lcstore + lcpiktkt
 = SEEK(lcorderno + lcstore + lcpiktkt)
ELSE
 = SEEK(lcinvoice + lcstore)
 SUM nchrgamnt * (1 - lntrddsc / 100) * ntaxrate / 100 TO lntottax REST WHILE invoice + cstore + cchrgcode = lcinvoice + lcstore
 = SEEK(lcinvoice + lcstore)
ENDIF
llnewchrg = .F.
SCATTER MEMVAR
IF lascrmode(4)
 SELECT (lcchrgfile)
 = SEEK(IIF(lascrmode(2), lcinvoice, lcorderno + lcstore + lcpiktkt))
 SUM nchrgamnt * (1 - lntrddsc / 100) * ntaxrate / 100 TO lntottax REST WHILE order + cstore + piktkt + cchrgcode = lcorderno + lcstore + lcpiktkt
 &lctotchrg = lntotchrg
 &lctottax  = lntottax
ENDIF
SELECT (lnalias)
ENDPROC
**
PROCEDURE LPRELEASLK
IF RECCOUNT(lcinvhdr) = 0
 RETURN
ENDIF
PRIVATE lcalias, lninvhdrrc, lnordhdrrc, lcordhdrtg, lcsetdelet
lcalias = ALIAS()
lcsetdelet = SET('DELETE')
SET DELETED OFF
lninvhdrrc = RECNO(lcinvhdr)
lnordhdrrc = RECNO('OrdHdr')
lcordhdrtg = ORDER('OrdHdr')
SET ORDER IN ordhdr TO OrdHdr
SELECT (lcinvhdr)
SCAN FOR !EMPTY(order)
 IF gfseek('O' + order, 'OrdHdr') .AND. ordhdr.llok_stat
  SELECT ordhdr
  = RLOCK()
  REPLACE llok_stat WITH .F., clok_user WITH '', dlok_date WITH {}, clok_time WITH ''
  UNLOCK
 ENDIF
ENDSCAN
SET ORDER IN ordhdr TO (lcordhdrtg)
IF BETWEEN(lnordhdrrc, 1, RECCOUNT('OrdHdr'))
 GOTO lnordhdrrc IN ordhdr
ENDIF
IF BETWEEN(lninvhdrrc, 1, RECCOUNT('InvHdr'))
 GOTO lninvhdrrc IN invhdr
ENDIF
SET DELETE &lcsetdelet.
IF !EMPTY(lcalias)
 SELECT (lcalias)
ENDIF
ENDPROC
**
PROCEDURE LFPRNPACK
PRIVATE lnslct, lcs
lnslct = SELECT()
lcs = IIF(RECCOUNT(lctmppktk) > 1, 's', '')
IF gfmodalgen('INM00000B00006',.F.,.F.,.F.,'Do you want to print the created Packing list&lcS') = 1
 SELECT rep_object
 RESTORE FROM MEMO mrepfxflt ADDITIVE
 lctmppktk = loformset.lctmppktk
 llmorethan140 = .F.
 SELECT (lctmppktk)
 LOCATE
 STORE '' TO lcpik
 IF RECCOUNT(lctmppktk) > 2
  IF RECCOUNT(lctmppktk) <= 27
   SELECT &lctmppktk
   LOCATE
   lncnt = RECCOUNT()
   lcpik = &lctmppktk..piktkt +"' OR INLIST(PACK_NO"
   SCAN FOR RECNO() > 1 .AND. RECNO() < lncnt
    lcpik = lcpik + ",'"+&lctmppktk..piktkt+"'"
   ENDSCAN
   GOTO BOTTOM
   lcpik = lcpik + ") OR PAcK_NO ='"+&lctmppktk..piktkt+""
  ELSE
   SELECT &lctmppktk
   LOCATE
   lcinlist = ''
   lncntlst = 1
   lncnt = RECCOUNT()
   IF lncnt > 140
    llmorethan140 = .T.
   ENDIF
   lcpik = &lctmppktk..piktkt +"'" 
   SCAN FOR RECNO() > 1 .AND. RECNO() < lncnt
    IF !EMPTY(lcinlist)
     lcevalexp = STRTRAN(lcinlist, 'PACK_NO', 'PIKTKT')
     IF EVALUATE(lcevalexp)
      LOOP
     ENDIF
    ENDIF
    lcinlist = lcinlist +  IIF(!EMPTY(lcinlist)," OR ","")+"BETWEEN(PACK_NO,'"+&lctmppktk..piktkt+"','"
    lcpack = 	&lctmppktk..piktkt 
    lnpack = VAL(&lctmppktk..piktkt)
    lnnextpack = lnpack + 1
    lcnextpack = PADL(ALLTRIM(STR(lnnextpack)), 6, '0')
    lncurrrec = RECNO()
    LOCATE FOR piktkt = lcnextpack
    IF FOUND()
     FOR a = lnnextpack + 1 TO 999999
      lcnewnextpack = PADL(ALLTRIM(STR(a)), 6, '0')
      LOCATE FOR piktkt = lcnewnextpack
      IF FOUND()
       lcnextpack = lcnewnextpack
      ELSE
       lcnextpack = PADL(ALLTRIM(STR(a - 1)), 6, '0')
       EXIT
      ENDIF
     ENDFOR
    ELSE
     lcnextpack = lcpack
    ENDIF
    IF BETWEEN(lncurrrec, 1, RECCOUNT())
     GOTO lncurrrec
    ENDIF
    lcinlist = lcinlist + lcnextpack + "')"
   ENDSCAN
   lcpik = lcpik + " OR " + lcinlist
   SELECT &lctmppktk
   GOTO BOTTOM
   lcpik = lcpik + " OR PAcK_NO ='"+&lctmppktk..piktkt+""              
  ENDIF
 ELSE
  IF RECCOUNT(lctmppktk) = 2
   SELECT &lctmppktk
   LOCATE
   lcpik = &lctmppktk..piktkt +"' OR PACK_NO = '"
   SELECT &lctmppktk
   GOTO BOTTOM
   lcpik = lcpik + &lctmppktk..piktkt 
  ELSE
   GOTO TOP
   lcpik = &lctmppktk..piktkt 
  ENDIF
 ENDIF
 SELECT rep_object
 lnseapos = ASCAN(laogfxflt, "PACK_HDR.PACK_NO")
 IF lnseapos > 0
  lnseapos = ASUBSCRIPT(laogfxflt, lnseapos, 1)
  laogfxflt[lnseapos, 6] = "lcPik"
 ENDIF
 SAVE TO MEMO mrepfxflt
 IF llmorethan140
  = SYS(3055, 2040)
 ENDIF
 loformset.print("P")
 IF llmorethan140
  = SYS(3055, 320)
 ENDIF
 IF USED(laogfxflt(1, 6))
  USE IN (laogfxflt(1, 6))
 ENDIF
ENDIF
SELECT (lnslct)
loformset.preferencename = ""
lfinitform(loformset)
IF TYPE("loFormset.ariaForm1.ariaPAGEFRAME1.page4.cntInvoicesummary.ChargesFile") = 'U'
 loformset.ariaform1.ariapageframe1.page4.cntinvoicesummary.addproperty('ChargesFile', "")
ENDIF
ENDPROC
**
PROCEDURE LFPACKLIN
PARAMETER lcorder, lcpackno
WAIT WINDOW NOWAIT 'Computing packed quantity...'
SELECT pack_lin
= gfseek(lcpackno)
SCAN REST WHILE pack_no + STR(line_no, 3) + style + cpackcolor = lcpackno
 IF !SEEK(lcpackno + lcorder + STR(nordlineno, 6), lcpackline)
  INSERT INTO (lcpackline) (order, pack_no, nordlineno) VALUES (lcorder, lcpackno, pack_lin.nordlineno)
 ENDIF
 SELECT (lcpackline)
 REPLACE qty1 WITH qty1 + pack_lin.qty1, qty2 WITH qty2 + pack_lin.qty2, qty3 WITH qty3 + pack_lin.qty3, qty4 WITH qty4 + pack_lin.qty4, qty5 WITH qty5 + pack_lin.qty5, qty6 WITH qty6 + pack_lin.qty6, qty7 WITH qty7 + pack_lin.qty7, qty8 WITH qty8 + pack_lin.qty8, totqty WITH totqty + pack_lin.totqty
ENDSCAN
WAIT CLEAR
ENDPROC
**
FUNCTION LfVCNFRM
PARAMETER loformset
IF lfbeforsav(loformset)
 IF gfmodalgen('INM00000B00006', .F., .F., .F., 'Are you sure you want to Save') = 1
  IF EMPTY(loformset.ariaform1.txtcongno.value)
   = gfmodalgen('INM00000B00000', .F., .F., .F., 'No consignment no has been entered - please update the no and then save the batch')
   loformset.ariaform1.txtcongno.setfocus()
   RETURN .F.
  ENDIF
  RETURN .T.
 ELSE
  RETURN .F.
 ENDIF
ELSE
 RETURN .F.
ENDIF
ENDFUNC
**
PROCEDURE lfCrtFl
lcpacks = gftempname()
DIMENSION laarrstr[2, 4]
laarrstr[1, 1] = 'KEYEXP'
laarrstr[1, 2] = 'C'
laarrstr[1, 3] = 6
laarrstr[1, 4] = 0
laarrstr[2, 1] = 'Pack_no'
laarrstr[2, 2] = 'C'
laarrstr[2, 3] = 6
laarrstr[2, 4] = 0
gfcrttmp(lcpacks, @laarrstr, "KEYEXP", lcpacks, .F.)
lcvaluestoconvert = lcpiktkts
IF !EMPTY(lcpiktkts)
 lnstart = 1
 lnend = AT(',', lcvaluestoconvert)
 DO WHILE lnend<>0
  SELECT (lcpacks)
  APPEND BLANK
  REPLACE pack_no WITH SUBSTR(lcvaluestoconvert, lnstart, lnend - 1), keyexp WITH SUBSTR(lcvaluestoconvert, lnstart, lnend - 1)
  lcvaluestoconvert = STUFF(lcvaluestoconvert, lnstart, lnend, "")
  lnend = AT(',', lcvaluestoconvert)
 ENDDO
 IF lnend = 0
  SELECT (lcpacks)
  APPEND BLANK
  REPLACE pack_no WITH lcvaluestoconvert, keyexp WITH lcvaluestoconvert
 ENDIF
ENDIF
SCAN
 SELECT (lcpacks)
 APPEND BLANK
 REPLACE pack_no WITH &lctmppktk..piktkt, keyexp WITH  &lctmppktk..piktkt
ENDSCAN
lnseapos = ASCAN(loogscroll.laogfxflt, "PACK_HDR.PACK_NO")
IF lnseapos > 0
 lnseapos = ASUBSCRIPT(loogscroll.laogfxflt, lnseapos, 1)
 loogscroll.laogfxflt[lnseapos, 6] = lcpacks
ENDIF
ENDPROC
**
PROCEDURE LFVCRT
PARAMETER loformset
lctmppktk = loformset.lctmppktk
lncrt = loformset.ariaform1.txtcartons.value
SELECT &lctmppktk
lnrecn = RECNO()
LOCATE
IF !EOF()
 SCAN
  IF ncarton > MIN(lncrt, pikqty)
   REPLACE ncarton WITH MIN(lncrt, pikqty)
  ENDIF
 ENDSCAN
ENDIF
IF BETWEEN(lnrecn, 1, RECCOUNT())
 GOTO lnrecn
ENDIF
ENDPROC
**
*** 
*** ReFox - all is not lost 
***
