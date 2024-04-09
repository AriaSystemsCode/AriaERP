*B611917,1 MMT 2021/08/12 Custom Automatic PL takes time to save in case of Non-EDI Account PL[T20210809.0001]
*** 
*** ReFox XI+  #OE577650  Mariam  Mariam [VFP90]
***
IF oariaapplication.multiinst
 = gfcallform('ALGAPLJ', 'AL')
ELSE
 DO FORM (oariaapplication.screenhome+'AL\ALGAPLJ.SCX')
ENDIF
ENDPROC
**
PROCEDURE lfFormInit
PARAMETER loformset
STORE 0 TO loformset.lnfrom, loformset.lnto, loformset.lnctnpal, loformset.lnpackwgh
loformset.llupdtpktk = .F.
IF FILE(oariaapplication.datadir + 'UpPkTk.MEM')
 RESTORE FROM (oariaapplication.datadir + 'UpPkTk.MEM') ADDITIVE
 loformset.llupdtpktk = llupdtpktk
ENDIF
STORE '' TO loformset.cpckdscode, loformset.cpckchcode, loformset.lcwarecode
loformset.llcomp = .F.
loformset.lldyelot = .F.
STORE 0 TO loformset.lndrctto
STORE 0 TO loformset.lnctntyp, loformset.lnmaxctn
loformset.lcbol = ''
loformset.llalwforce = lfsuppforc(loformset)
loformset.ariabrfields.edtbrowsefields.value = "Order:H='Order',Account:H= 'Account'," + "Status:H='Status',TOT_wght:H='Tot. Weight'," + "tot_cart:H='Tot. Carton',Tot_pcs:H='Tot. Pcs',cwarecode:H='Warehouse'"
= gfopentable('TMPL_LIN', 'TMPL_LIN')
= gfopentable('TMPL_HDR', 'TMPL_hDr')
= gfopentable('OrdHdr', 'OrdHdr')
= gfopentable('PIKTKT', 'PIKTKT')
= gfopentable('Pack_Hdr', 'Pack_Hdr')
= gfopentable('Customer', 'Customer')
= gfopentable('SCALE', 'SCALE')
= gfopentable('StyLE', 'StyLE')
= gfopentable('Pack_Lin', 'Pack_Lin')
= gfopentable('Pikline', 'Pikline')
= gfopentable('Ordline', 'Ordline')
= gfopentable('SPck_Hdr', 'SPCK_HDRVR')
= gfopentable('SPck_lin', 'SPCK_LINVR')
loformset.lcpcklin = gftempname()
loformset.lcpack_lin = gftempname()
loformset.lcctnhdr = gftempname()
loformset.lcstores = gftempname()
loformset.lcctndtl = gftempname()
loformset.lctmppck = gftempname()
loformset.lctmpidx = gftempname()
loformset.lctmpupln = gftempname()
loformset.lcsumpck = gftempname()
loformset.lcpakindxst = gftempname()
loformset.lcpakindxln = gftempname()
loformset.llcanprnlb = gfuserpriv('AL', 'ALPLIST', 'PRNPACKING')
loformset.lcerorfil = gftempname()
loformset.llasnsys = ('AS' $ oariaapplication.companyinstalledmodules)
loformset.llalomodul = ('AL' $ oariaapplication.companyinstalledmodules)
loformset.lldyelot = ALLTRIM(gfgetmemvar('M_DYELOT', oariaapplication.activecompanyid)) = 'Y'
loformset.lctmpupln = gftempname()
loformset.llextsizsc = gfgetmemvar('M_USEEXSSC', oariaapplication.activecompanyid)
loformset.lcsizesep = ''
STORE 0 TO lnsizepos, lnsizelen
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
loformset.lcstypic = gfitemmask("PI")
loformset.lcstyttl = gfitemmask("HI")
loformset.lnstylewid = LEN(loformset.lcstypic)
loformset.lctmasnshp = gftempname()
IF loformset.llcanprnlb
 = gfopentable('Asn_Ship', 'Asn_Ship')
 IF !USED(loformset.lctmasnshp)
  SELECT 'Asn_Ship'
  COPY TO (oariaapplication.workdir + loformset.lctmasnshp) STRUCTURE
  = gfopenfile(oariaapplication.workdir + loformset.lctmasnshp, '', 'EX')
  SELECT (loformset.lctmasnshp)
  INDEX ON STR(cart_no, 6) TAG (loformset.lctmasnshp)
 ENDIF
 = gfopentable(oariaapplication.syspath + 'SYCASNLB', 'ASNlbl')
 lldetlabel = gfseek("XX1" + "H", "SYCASNLB") .AND. gfseek("XX1" + "L", "SYCASNLB")
 IF lldetlabel
  loformset.llupcinst = ('UP' $ oariaapplication.companyinstalledmodules)
  loformset.lcdetlball = ""
  IF loformset.llupcinst
   = gfopentable('STYLEUPC', 'STYLEUPC')
  ENDIF
 ENDIF
ENDIF
loformset.lledisys = ('AS' $ oariaapplication.companyinstalledmodules)
IF loformset.lledisys
 = gfopentable('EDIAcPrt', 'ACCFACT')
 = gfopentable('EDIPH', 'PARTNER')
 = gfopentable('EDICRTSQ', 'EDICRTSQ')
 = gfopentable('BOL_HDR', 'BOL_HDR')
 = gfopentable('BOL_LIN', 'BOL_LIN')
ENDIF
WITH loformset
 .cbrowsetabledbengine = "SQL"
 .nworkarea = 'TMPL_HDR'
 .dataenvironment.initialselectedalias = 'TMPL_HDR'
 .cbrowsefilename = "TMPL_HDR"
 .cbrowseindexexpression = "ORDER"
 .cbrowseindexfields = "ORDER"
 .cbrowseindexname = "TMPL_HDR"
 .cbrowsealiasname = "TMPL_HDR"
 .cbrowsetablename = "TMPL_HDR"
 .cbrowsefilter = ""
 .browsetitle = "Templates"
ENDWITH
ENDPROC
**
PROCEDURE lfvOrderNo
LPARAMETERS loformset, lcorderno, lcaccount, llbrowse
PRIVATE lncuralias, lcordhdrtg, lcorder
lncuralias = SELECT(0)
lcordhdrtg = ORDER('OrdHdr')
SELECT ordhdr
= gfsetorder('OrdHdr')
loformset.ariaform1.pgfpacking.header.grdheadr.recordsource = ''
loformset.ariaform1.pgfpacking.detail.grddetail.recordsource = ''
loformset.ariaform1.pgfpacking.cartoninfo.grdcartonh.recordsource = ''
loformset.ariaform1.pgfpacking.cartoninfo.grdcartond.recordsource = ''
IF !((EMPTY(lcorderno) .OR. LASTKEY() <> 13) .AND. !llbrowse)
 IF (!EMPTY(lcorderno) .AND. !gfseek('O' + lcorderno, 'OrdHdr')) .OR. llbrowse
  lcorder = lcorderno
  = lfordbrow(@lcorder, lcaccount)
  lcorderno = lcorder
  loformset.ariaform1.kborderno.keytextbox.value = lcorderno
  llbrowse = .F.
 ENDIF
ENDIF
IF !EMPTY(lcorderno) .AND. gfseek('O' + lcorderno, 'OrdHdr')
 lcalias = SELECT(0)
 SELECT pack_hdr
 lctmpkey = pack_no
 lcordr = ORDER()
 = gfsetorder('OrderPck')
 llpacked = gfseek(ordhdr.order + ordhdr.store)
 = gfsetorder(lcordr)
 = gfseek(lctmpkey)
 = gfseek('O' + lcorderno, 'OrdHdr')
 SELECT (lcalias)
 DO CASE
  CASE ordhdr.status = 'C' .AND. llpacked
   = gfmodalgen("INM000000B00000", "DIALOG", '', '', 'This order is completed and packed, cannot pack.')
   loformset.ariaform1.kborderno.keytextbox.value = SPACE(6)
   llpacked = .F.
  CASE ordhdr.status = 'X'
   = gfmodalgen("INM44050B00000", "Dialog", "canceled")
   loformset.ariaform1.kborderno.keytextbox.value = SPACE(6)
  CASE ordhdr.bulk = 'Y'
   = gfmodalgen("INM44050B00000", "Dialog", "bulk")
   loformset.ariaform1.kborderno.keytextbox.value = SPACE(6)
  OTHERWISE
   IF !gfseek(lcorderno, 'TMPL_LIN')
    = gfmodalgen('INM00000B00000', .F., .F., .F., 'No tamplate saved for this order, can not proceed.')
    SELECT ordhdr
    = gfsetorder(lcordhdrtg)
    loformset.ariaform1.kborderno.keytextbox.value = SPACE(6)
    SELECT (lncuralias)
    RETURN
   ENDIF
   = gfseek(lcorderno, 'TMPL_HDR')
   llnothing = lfcrtuncmp(loformset)
   loformset.llcomp = (ordhdr.status = 'C' .AND. !llpacked)
   = lfgetdata(loformset, lcorderno)
   SELECT piktkt
   = gfsetorder('ORDPIK')
   IF gfseek(lcorderno, 'PikTkt')
    lnalias = SELECT(0)
    SELECT piktkt
    LOCATE REST FOR status $ 'PO' WHILE order = lcorderno
    IF FOUND()
     llhaspik = .T.
    ELSE
     llhaspik = .F.
    ENDIF
    SELECT (lnalias)
   ELSE
    llhaspik = .F.
   ENDIF
   = lfvnewpack(loformset)
   lcpckhdrord = ORDER('pack_hdr')
   SELECT pack_hdr
   = gfsetorder('ORDERPCK')
   IF !gfseek(lcorderno, 'PACK_HDR')
    = gfsetorder(lcpckhdrord)
    loformset.changemode('A')
   ELSE
    = gfsetorder(lcpckhdrord)
    loformset.changemode('V')
   ENDIF
 ENDCASE
ENDIF
SELECT ordhdr
gfsetorder(lcordhdrtg)
SELECT (lncuralias)
ENDPROC
**
PROCEDURE lfGetData
LPARAMETERS loformset, lcorderno
PRIVATE lcpiktkt, lcstore
llnothing = gfseek('O' + lcorderno, 'OrdHdr')
WITH loformset.ariaform1
 STORE IIF(!EMPTY(lcorderno), ordhdr.account, SPACE(6)) TO .kbaccount.keytextbox.value, lcaccount
 loformset.llediacc = loformset.lledisys .AND. gfseek('A' + lcaccount, 'EDIACPRT') .AND. gfseek(ediacprt.cpartcode, 'EDIPH')
 STORE loformset.lledisys .AND. loformset.llediacc .AND. ediacprt.lpkchrdes TO llpcdstat
 STORE loformset.lledisys .AND. loformset.llediacc .AND. ediph.lpltshp TO llpalstat
 lcstore = ordhdr.store
 lccuspo = ordhdr.custpo
 lcdept = ordhdr.dept
 .txtcustpo.value = ordhdr.custpo
 loformset.lcwarecode = ordhdr.cwarecode
 IF loformset.llediacc
  loformset.cpckchcode = ediacprt.cpckchcode
  loformset.cpckdscode = ediacprt.cpckdscode
 ELSE
  loformset.lnctntyp = 2
  loformset.lndrctto = 1
 ENDIF
ENDWITH
ENDPROC
**
PROCEDURE lfvNewPack
LPARAMETERS loformset
PRIVATE lctag, lncuralias
STORE .T. TO loformset.llcupdate, loformset.llanyupd, loformset.llnew
lcaccount = ordhdr.account
lcstore = ordhdr.store
IF ALLTRIM(ordhdr.shipvia) = '*' .AND. gfseek('S' + lcaccount + lcstore, 'Customer')
 lcshipvia = customer.shipvia
ENDIF
lcpckhdrord = ORDER('PACK_HDR')
SELECT pack_hdr
= gfsetorder('ORDERPCK')
= lfgtordsto(loformset)
lcstores = loformset.lcstores
PRIVATE lctmplord
lctmplord = ORDER('TMPL_LIN')
SELECT tmpl_lin
= gfsetorder('TMPL_LINS')
SELECT &lcstores
GOTO TOP
SCAN
 WAIT WINDOW NOWAIT 'Collecting data for store '+&lcstores..STORE + ' D.C '+ &lcstores..dist_ctr
 = lfvselordl(loformset)
ENDSCAN
SELECT &lcstores
GOTO TOP
WAIT CLEAR
SELECT pack_hdr
= gfsetorder(lcpckhdrord)
SELECT tmpl_lin
= gfsetorder(lctmplord)
lcpcklin = loformset.lcpcklin
SELECT (lcpcklin)
GOTO TOP
= RLOCK(lcpcklin)
UNLOCK IN (lcpcklin)
lctmppck = loformset.lctmppck
IF !USED(lctmppck)
 IF gfgetmemvar('M_ORDSTUTS', oariaapplication.activecompanyid) = 'L'
  USE (oariaapplication.workdir + lcpcklin) AGAIN ALIAS (lctmppck) ORDER (loformset.lcpakindxln) IN 0
 ELSE
  USE (oariaapplication.workdir + lcpcklin) AGAIN ALIAS (lctmppck) ORDER (loformset.lcpakindxst) IN 0
 ENDIF
ENDIF
lfdtlbrow(loformset)
ENDPROC
**
PROCEDURE lfGtOrdSto
PARAMETER loformset
PRIVATE lcsvord, lcgtstore, lnstoordrd, lcsvrel, lcsvpkord
lnstoordrd = 0
lcsvord = ORDER('ORDLINE')
lcsvpkord = ORDER('PIKLINE')
SELECT ordline
= gfsetorder("ORDLINST")
SELECT pikline
= gfsetorder("PIKLINEO")
SELECT pikline
lcsvrel = SET('RELATION')
SET RELATION TO
GOTO TOP
lcgtstore = ' '
lcgtpiktkt = '  '
lcorderno = loformset.ariaform1.kborderno.keytextbox.value
= gfseek('O' + lcorderno, 'ORDLINE')
SELECT ordline
lcstores = loformset.lcstores
SELECT * FROM Ordline WHERE cordtype + order + store + style + STR(lineno, 6) = 'O' + lcorderno ORDER BY store, piktkt, style, lineno INTO CURSOR 'TmpLines'
SELECT 'TmpLines'
LOCATE
SCAN
 IF lcgtstore <> tmplines.store .OR. lcgtpiktkt <> tmplines.piktkt
  lcgtpiktkt = tmplines.piktkt
  lcgtstore = tmplines.store
  = gfseek('S' + tmplines.account + tmplines.store, 'CUSTOMER')
  = gfseek(lcorderno + tmplines.store + IIF(!EMPTY(ALLTRIM(lcgtpiktkt)), lcgtpiktkt, ''), 'PACK_HDR')
  INSERT INTO (loformset.lcstores) (dist_ctr, store, piktkt, totord, totpik, pack_no, totpqty, weight, cartons, bol_no) VALUES (customer.dist_ctr, tmplines.store, tmplines.piktkt, tmplines.totbook, tmplines.totpik, pack_hdr.pack_no, pack_hdr.tot_pcs, pack_hdr.tot_wght, pack_hdr.tot_cart, pack_hdr.bill_ladg)
  IF gfseek(tmplines.order + STR(tmplines.lineno, 6), 'PIKLINE')
   SELECT pikline
   SCAN REST FOR gfseek(pikline.piktkt, 'PIKTKT', 'PIKTKT') .AND. piktkt.status <> 'X' WHILE order + STR(lineno, 6) = tmplines.order + STR(tmplines.lineno, 6)
    SELECT (loformset.lcstores)
    REPLACE piktkt WITH pikline.piktkt, totpik WITH pikline.totpik
    EXIT
   ENDSCAN
  ENDIF
 ELSE
  REPLACE &lcstores..totord WITH &lcstores..totord + tmplines.totbook, &lcstores..totpik WITH &lcstores..totpik +  IIF(gfseek(tmplines.ORDER+STR(tmplines.LINENO,6),'PIKLINE'),pikline.totpik,tmplines.totpik)
 ENDIF
ENDSCAN
GO TOP IN &lcstores
= lfwstrsbr(loformset)
SELECT pikline
= gfsetorder(lcsvpkord)
SELECT ordline
= gfsetorder(lcsvord)
ENDPROC
**
PROCEDURE lfCrtUnCmp
PARAMETER loformset
= lfcrselfiles(loformset)
= lfcrctnfiles(loformset)
= lfcrselsum(loformset)
lfwstrsbr(loformset)
ENDPROC
**
PROCEDURE lfCrSelFiles
PARAMETER loformset
PRIVATE lncuralias, lni
lncuralias = SELECT(0)
lni = 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cSelect1'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cSelect2'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PIKTKT'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cSelect3'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cSelect4'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cSelect5'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cSelect6'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cSelect7'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cSelect8'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Style'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 19
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Scale'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 3
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'SzCnt'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cSize1'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 5
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cSize2'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 5
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cSize3'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 5
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cSize4'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 5
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cSize5'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 5
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cSize6'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 5
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cSize7'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 5
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cSize8'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 5
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'OrgPWgh'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'OrdQty1'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'OrdQty2'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'OrdQty3'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'OrdQty4'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'OrdQty5'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'OrdQty6'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'OrdQty7'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'OrdQty8'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'OrdTotQty'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 7
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'AvlQty1'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'AvlQty2'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'AvlQty3'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'AvlQty4'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'AvlQty5'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'AvlQty6'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'AvlQty7'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'AvlQty8'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'AvlTotQty'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 7
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'OQty1'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'OQty2'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'OQty3'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'OQty4'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'OQty5'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'OQty6'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'OQty7'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'OQty8'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'OTotQty'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 7
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'CtnQty1'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'CtnQty2'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'CtnQty3'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'CtnQty4'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'CtnQty5'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'CtnQty6'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'CtnQty7'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'CtnQty8'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'CtnTotQty'
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
lafilestru[lni, 1] = 'TotWeight'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 10
lafilestru[lni, 4] = 2
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PQty1'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PQty2'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PQty3'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PQty4'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PQty5'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PQty6'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PQty7'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PQty8'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PTotQty'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'StyWgh'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 5
lafilestru[lni, 4] = 2
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'OrgStyWgh'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 5
lafilestru[lni, 4] = 2
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PWgh1'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 9
lafilestru[lni, 4] = 2
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PWgh2'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 9
lafilestru[lni, 4] = 2
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PWgh3'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 9
lafilestru[lni, 4] = 2
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PWgh4'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 9
lafilestru[lni, 4] = 2
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PWgh5'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 9
lafilestru[lni, 4] = 2
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PWgh6'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 9
lafilestru[lni, 4] = 2
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PWgh7'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 9
lafilestru[lni, 4] = 2
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PWgh8'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 9
lafilestru[lni, 4] = 2
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PTotWgh'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 9
lafilestru[lni, 4] = 2
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'LPicked'
lafilestru[lni, 2] = 'L'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'nStep'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 2
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'nOrdLineNo'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Selected'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'OrgTotOrd'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 7
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'nDiff1'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'nDiff2'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'nDiff3'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'nDiff4'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'nDiff5'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'nDiff6'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'nDiff7'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'nDiff8'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'nTotDiff'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 7
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PACK_ID'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 16
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cPkVersion'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 4
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cPkColor'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cPckSize'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 3
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'STORE'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 8
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'llPack'
lafilestru[lni, 2] = 'L'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'lRange'
lafilestru[lni, 2] = 'L'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'UpOqty1'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'UpOqty2'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'UpOqty3'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'UpOqty4'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'UpOqty5'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'UpOqty6'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'UpOqty7'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'UpOqty8'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'UOTotQty'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 7
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cNoSize'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'llFrst'
lafilestru[lni, 2] = 'L'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
DIMENSION laindx[7, 2]
laindx[1, 1] = "Style+STR(nOrdLineNo,6)"
laindx[1, 2] = loformset.lcpcklin
laindx[2, 1] = 'STORE+PIKTKT'
laindx[2, 2] = 'Store'
laindx[3, 1] = "IIF(OQty1+OQty2+OQty3+OQty4+OQty5+OQty6+OQty7+OQty8>0,'Y','N')"
laindx[3, 2] = 'Opened'
laindx[4, 1] = "IIF(PQty1+PQty2+PQty3+PQty4+PQty5+PQty6+PQty7+PQty8=0,'Y','N')"
laindx[4, 2] = 'NoPacked'
laindx[5, 1] = "STR(nOrdLineNo,6)+Style"
laindx[5, 2] = loformset.lctmpidx
laindx[6, 1] = "PACK_ID+cPkColor+cPckSize+cPkVersion+STR(nOrdLineNo,6)+Style"
laindx[6, 2] = loformset.lcpakindxln
laindx[7, 1] = "PACK_ID+cPkColor+cPckSize+cPkVersion+Style+STR(nOrdLineNo,6)"
laindx[7, 2] = loformset.lcpakindxst
= gfcrttmp(loformset.lcpcklin, @lafilestru, @laindx)
SET ORDER IN (loformset.lcpcklin) TO (loformset.lcpakindxln)
SELECT pack_lin
= AFIELDS(lafilestru)
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 18]
lafilestru[lni, 1] = 'STORE'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 8
lafilestru[lni, 4] = 0
STORE ' ' TO lafilestru[lni, 7], lafilestru[lni, 8], lafilestru[lni, 9], lafilestru[lni, 10], lafilestru[lni, 11], lafilestru[lni, 12], lafilestru[lni, 13], lafilestru[lni, 14], lafilestru[lni, 15], lafilestru[lni, 16]
STORE 0 TO lafilestru[lni, 17], lafilestru[lni, 18]
DIMENSION laindx[1, 2]
laindx[1, 1] = "PACK_ID+cPkColor+cPckSize+cPkVersion+Style+STR(nOrdLineNo,6)"
laindx[1, 2] = loformset.lcpack_lin
= gfcrttmp(loformset.lcpack_lin, @lafilestru, @laindx)
SET ORDER IN (loformset.lcpcklin) TO (loformset.lcpakindxln)
lni = 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'lSelect'
lafilestru[lni, 2] = 'L'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'STORE'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 8
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'DIST_CTR'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 8
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PIKTKT'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'TOTORD'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 8
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'TOTPIK'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 8
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Cartons'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 8
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'Weight'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 9
lafilestru[lni, 4] = 2
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'TOTPQty'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 8
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PACK_NO'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'BOL_NO'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
DIMENSION laindx[1, 2]
laindx[1, 1] = "DIST_CTR+STORE"
laindx[1, 2] = loformset.lcstores
= gfcrttmp(loformset.lcstores, @lafilestru, @laindx)
SET ORDER IN (loformset.lcstores) TO (loformset.lcstores)
SELECT (lncuralias)
ENDPROC
**
PROCEDURE lfCrCtnFiles
PARAMETER loformset
PRIVATE lncuralias
lncuralias = SELECT(0)
DIMENSION lafilestru[5, 4]
lafilestru[01, 1] = 'Cart_No'
lafilestru[01, 2] = 'N'
lafilestru[01, 3] = 4
lafilestru[01, 4] = 0
lafilestru[02, 1] = 'Pal_No'
lafilestru[02, 2] = 'N'
lafilestru[02, 3] = 4
lafilestru[02, 4] = 0
lafilestru[03, 1] = 'TotPcs'
lafilestru[03, 2] = 'N'
lafilestru[03, 3] = 7
lafilestru[03, 4] = 0
lafilestru[04, 1] = 'TotWgh'
lafilestru[04, 2] = 'N'
lafilestru[04, 3] = 9
lafilestru[04, 4] = 2
lafilestru[05, 1] = 'Empty'
lafilestru[05, 2] = 'C'
lafilestru[05, 3] = 1
lafilestru[05, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'STORE'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 8
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PIKTKT'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
DIMENSION laindx[3, 2]
laindx[1, 1] = "STR(Cart_No,4)+STR(Pal_No,4)"
laindx[1, 2] = loformset.lcctnhdr
laindx[2, 1] = "Empty+STR(Cart_No,4)+STR(Pal_No,4)"
laindx[2, 2] = "EMPTY"
laindx[3, 1] = "STORE+PIKTKT+STR(Cart_No,4)+STR(Pal_No,4)"
laindx[3, 2] = "STORE"
= gfcrttmp(loformset.lcctnhdr, @lafilestru, @laindx)
SET ORDER IN (loformset.lcctnhdr) TO (loformset.lcctnhdr)
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
lafilestru[lni, 1] = 'TotQty'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 7
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
lafilestru[lni, 1] = 'TotWeight'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 10
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
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'STORE'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 8
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PACK_ID'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 16
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cPkColor'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cPckSize'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 3
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cPkVersion'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 4
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'NPACKNO'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cNoSize'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PIKTKT'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
DIMENSION laindx[2, 2]
laindx[1, 1] = "STR(Cart_No,4)+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+Style+STR(nOrdLineNo,6)"
laindx[1, 2] = loformset.lcctndtl
laindx[2, 1] = "cStatus"
laindx[2, 2] = "Status"
= gfcrttmp(loformset.lcctndtl, @lafilestru, @laindx)
SET ORDER IN (loformset.lcctndtl) TO (loformset.lcctndtl)
SELECT (lncuralias)
ENDPROC
**
PROCEDURE lfCrSelSum
PARAMETER loformset
PRIVATE lncuralias, lni
lncuralias = SELECT(0)
lni = 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'lSelect'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'OTotQty'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 7
lafilestru[lni, 4] = 2
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'CtnTotQty'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PTotQty'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 2
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PTotWgh'
lafilestru[lni, 2] = 'N'
lafilestru[lni, 3] = 9
lafilestru[lni, 4] = 2
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'PACK_ID'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 19
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cPkVersion'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 4
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cPkColor'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 6
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'cPckSize'
lafilestru[lni, 2] = 'C'
lafilestru[lni, 3] = 3
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'llPack'
lafilestru[lni, 2] = 'L'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
lni = ALEN(lafilestru, 1) + 1
DIMENSION lafilestru[lni, 4]
lafilestru[lni, 1] = 'lRange'
lafilestru[lni, 2] = 'L'
lafilestru[lni, 3] = 1
lafilestru[lni, 4] = 0
DIMENSION laindx[1, 2]
laindx[1, 1] = "PACK_ID+cPkColor+cPckSize+cPkVersion"
laindx[1, 2] = loformset.lcsumpck
= gfcrttmp(loformset.lcsumpck, @lafilestru, @laindx)
SET ORDER IN (loformset.lcsumpck) TO (loformset.lcsumpck)
SELECT (lncuralias)
ENDPROC
**
PROCEDURE lfwStrsBr
PARAMETER loformset
loformset.ariaform1.pgfpacking.header.grdheadr.recordsource = ''
loformset.ariaform1.pgfpacking.header.grdheadr.recordsource = loformset.lcstores
loformset.ariaform1.pgfpacking.header.grdheadr.column11.controlsource = loformset.lcstores + '.lSelect'
IF TYPE('loFormSet.ariaform1.pgfPacking.header.grdHeadr.Column11.Ariacheckbox1') <> 'O'
 loformset.ariaform1.pgfpacking.header.grdheadr.column11.addobject('Ariacheckbox1', 'Ariacheckbox')
ENDIF
loformset.ariaform1.pgfpacking.header.grdheadr.column11.currentcontrol = 'Ariacheckbox1'
loformset.ariaform1.pgfpacking.header.grdheadr.column1.controlsource = loformset.lcstores + '.Store'
loformset.ariaform1.pgfpacking.header.grdheadr.column2.controlsource = loformset.lcstores + '.PikTkt'
loformset.ariaform1.pgfpacking.header.grdheadr.column3.controlsource = loformset.lcstores + '.TOTORD'
loformset.ariaform1.pgfpacking.header.grdheadr.column4.controlsource = loformset.lcstores + '.TOTPIK'
loformset.ariaform1.pgfpacking.header.grdheadr.column5.controlsource = loformset.lcstores + '.Cartons'
loformset.ariaform1.pgfpacking.header.grdheadr.column6.controlsource = loformset.lcstores + '.Weight'
loformset.ariaform1.pgfpacking.header.grdheadr.column7.controlsource = loformset.lcstores + '.TOTPQty'
loformset.ariaform1.pgfpacking.header.grdheadr.column8.controlsource = loformset.lcstores + '.PACK_NO'
loformset.ariaform1.pgfpacking.header.grdheadr.column9.controlsource = loformset.lcstores + '.BOL_NO'
loformset.ariaform1.pgfpacking.header.grdheadr.column10.controlsource = loformset.lcstores + '.DIST_CTR'
loformset.ariaform1.pgfpacking.header.grdheadr.readonly = .T.
loformset.ariaform1.pgfpacking.header.grdheadr.column11.readonly = .F.
loformset.ariaform1.pgfpacking.header.grdheadr.refresh
lfwstrsbr1(loformset)
ENDPROC
**
PROCEDURE lfvSelOrdL
PARAMETER loformset
PRIVATE lnalias, lni, lnj, lcsize, lnqty, lcstyle, lcstydesc, lncontinue, lnctnqty, lnctnwgh, lnstyordlin, llstyfound, lnrempqty
lcctnhdr = loformset.lcctnhdr
lcstores = loformset.lcstores
lcctndtl = loformset.lcctndtl
llstyfound = .F.
STORE 0 TO lncontinue, lnctnqty, lnctnwgh, lni, lnj, lnstyordlin, lnrempqty
SELECT &lcctnhdr
SET FILTER TO STORE = &lcstores..STORE AND piktkt =&lcstores..piktkt
GOTO TOP
SELECT &lcctndtl
SET FILTER TO STORE = &lcstores..STORE AND piktkt =&lcstores..piktkt
GOTO TOP
loformset.llanyupd = .F.
lnalias = ALIAS()
lcorder = loformset.ariaform1.kborderno.keytextbox.value
SELECT ordline
= gfsetorder('Ordlinst')
llnothing = gfseek('O'+lcorder +&lcstores..STORE,'OrdLine')
STORE SPACE(0) TO lcstyle, lcstydesc
lcexp = IIF(!EMPTY(&lcstores..piktkt),"WHILE cOrdType+Order+Store='O'+lcorder+&lcStores..STORE FOR PikTkt=&lcStores..PIKTKT AND Picked", "WHILE cOrdType+Order+Store='O'+lcorder+&lcStores..STORE")
llfrmpikln = .F.
IF !EMPTY(&lcstores..piktkt)
 SELECT piktkt
 = gfsetorder('PIKTKT')
 IF gfseek(&lcstores..piktkt) AND STATUS = 'C'
  llfrmpikln = .T.
  lcexp = "WHILE PikTkt+Order+Store=&lcStores..PIKTKT+lcorder+&lcStores..STORE FOR Picked "
 ENDIF
ENDIF
IF llfrmpikln
 SELECT "PikLine"
 =gfseek(&lcstores..piktkt+lcorder)
ELSE
 SELECT "OrdLine"
 =gfseek('O'+lcorder+&lcstores..STORE)
ENDIF
lcpcklin = loformset.lcpcklin
SCAN REST &lcexp
 = gfseek(style, 'Style')
 = gfseek('S' + style.scale, 'Scale')
 llfrstrecord = .T.
 FOR lniscl = 1 TO scale.cnt
  lcsz = STR(lniscl, 1)
  IF IIF(llfrmpikln,pikline.qty&lcsz.,ordline.qty&lcsz.) = 0
   LOOP
  ENDIF
  SELECT (lcpcklin)
  APPEND BLANK
  REPLACE llfrst WITH llfrstrecord
  llfrstrecord = .F.
  REPLACE piktkt WITH IIF(llfrmpikln, pikline.piktkt, ordline.piktkt)
  REPLACE STYLE      WITH IIF(llfrmpikln,pikline.STYLE,ordline.STYLE), SCALE      WITH SCALE.SCALE, szcnt      WITH SCALE.CNT, stywgh     WITH STYLE.nstyweight, orgstywgh  WITH STYLE.nstyweight, nordlineno WITH IIF(llfrmpikln,pikline.LINENO,ordline.LINENO), lpicked    WITH IIF(llfrmpikln,pikline.picked,ordline.picked), csize1     WITH SCALE.sz1, csize2     WITH SCALE.sz2, csize3     WITH SCALE.sz3, csize4     WITH SCALE.sz4, csize5     WITH SCALE.sz5, csize6     WITH SCALE.sz6, csize7     WITH SCALE.sz7, csize8     WITH SCALE.sz8, avlqty1    WITH IIF(EMPTY(&lcstores..piktkt),ordline.qty1, IIF(llfrmpikln,pikline.pik1,ordline.pik1)), avlqty2    WITH IIF(EMPTY(&lcstores..piktkt),ordline.qty2, IIF(llfrmpikln,pikline.pik2,ordline.pik2)), avlqty3    WITH IIF(EMPTY(&lcstores..piktkt),ordline.qty3, IIF(llfrmpikln,pikline.pik3,ordline.pik3)), avlqty4    WITH IIF(EMPTY(&lcstores..piktkt),ordline.qty4, IIF(llfrmpikln,pikline.pik4,ordline.pik4)), avlqty5    WITH IIF(EMPTY(&lcstores..piktkt),ordline.qty5, IIF(llfrmpikln,pikline.pik5,ordline.pik5)), avlqty6    WITH IIF(EMPTY(&lcstores..piktkt),ordline.qty6, IIF(llfrmpikln,pikline.pik6,ordline.pik6)), avlqty7    WITH IIF(EMPTY(&lcstores..piktkt),ordline.qty7, IIF(llfrmpikln,pikline.pik7,ordline.pik7)), avlqty8    WITH IIF(EMPTY(&lcstores..piktkt),ordline.qty8, IIF(llfrmpikln,pikline.pik8,ordline.pik8)), ordqty1    WITH IIF(llfrmpikln,pikline.qty1,ordline.qty1), ordqty2    WITH IIF(llfrmpikln,pikline.qty2,ordline.qty2), ordqty3    WITH IIF(llfrmpikln,pikline.qty3,ordline.qty3), ordqty4    WITH IIF(llfrmpikln,pikline.qty4,ordline.qty4), ordqty5    WITH IIF(llfrmpikln,pikline.qty5,ordline.qty5), ordqty6    WITH IIF(llfrmpikln,pikline.qty6,ordline.qty6), ordqty7    WITH IIF(llfrmpikln,pikline.qty7,ordline.qty7), ordqty8    WITH IIF(llfrmpikln,pikline.qty8,ordline.qty8), STORE      WITH &lcstores..STORE          
  REPLACE avltotqty WITH (avlqty1 + avlqty2 + avlqty3 + avlqty4 + avlqty5 + avlqty6 + avlqty7 + avlqty8), ordtotqty WITH (ordqty1 + ordqty2 + ordqty3 + ordqty4 + ordqty5 + ordqty6 + ordqty7 + ordqty8)
  REPLACE oqty1 WITH MAX(0, avlqty1 - ordline.npck1), oqty2 WITH MAX(0, avlqty2 - ordline.npck2), oqty3 WITH MAX(0, avlqty3 - ordline.npck3), oqty4 WITH MAX(0, avlqty4 - ordline.npck4), oqty5 WITH MAX(0, avlqty5 - ordline.npck5), oqty6 WITH MAX(0, avlqty6 - ordline.npck6), oqty7 WITH MAX(0, avlqty7 - ordline.npck7), oqty8 WITH MAX(0, avlqty8 - ordline.npck8), ototqty WITH MAX(0, oqty1 + oqty2 + oqty3 + oqty4 + oqty5 + oqty6 + oqty7 + oqty8), pqty1 WITH ordline.npck1, pqty2 WITH ordline.npck2, pqty3 WITH ordline.npck3, pqty4 WITH ordline.npck4, pqty5 WITH ordline.npck5, pqty6 WITH ordline.npck6, pqty7 WITH ordline.npck7, pqty8 WITH ordline.npck8
  REPLACE ptotqty WITH (pqty1 + pqty2 + pqty3 + pqty4 + pqty5 + pqty6 + pqty7 + pqty8), ototqty WITH (oqty1 + oqty2 + oqty3 + oqty4 + oqty5 + oqty6 + oqty7 + oqty8)
  REPLACE upoqty1 WITH MAX(0, avlqty1 - ordline.npck1), upoqty2 WITH MAX(0, avlqty2 - ordline.npck2), upoqty3 WITH MAX(0, avlqty3 - ordline.npck3), upoqty4 WITH MAX(0, avlqty4 - ordline.npck4), upoqty5 WITH MAX(0, avlqty5 - ordline.npck5), upoqty6 WITH MAX(0, avlqty6 - ordline.npck6), upoqty7 WITH MAX(0, avlqty7 - ordline.npck7), upoqty8 WITH MAX(0, avlqty8 - ordline.npck8), uototqty WITH MAX(0, upoqty1 + upoqty2 + upoqty3 + upoqty4 + upoqty5 + upoqty6 + upoqty7 + upoqty8)
  IF (ordline.npck1 + ordline.npck2 + ordline.npck3 + ordline.npck4 + ordline.npck5 + ordline.npck6 + ordline.npck7 + ordline.npck8) <> 0
   REPLACE pwgh1 WITH (ordline.npwght / (ordline.npck1 + ordline.npck2 + ordline.npck3 + ordline.npck4 + ordline.npck5 + ordline.npck6 + ordline.npck7 + ordline.npck8)) * pqty1, pwgh2 WITH (ordline.npwght / (ordline.npck1 + ordline.npck2 + ordline.npck3 + ordline.npck4 + ordline.npck5 + ordline.npck6 + ordline.npck7 + ordline.npck8)) * pqty2, pwgh3 WITH (ordline.npwght / (ordline.npck1 + ordline.npck2 + ordline.npck3 + ordline.npck4 + ordline.npck5 + ordline.npck6 + ordline.npck7 + ordline.npck8)) * pqty3, pwgh4 WITH (ordline.npwght / (ordline.npck1 + ordline.npck2 + ordline.npck3 + ordline.npck4 + ordline.npck5 + ordline.npck6 + ordline.npck7 + ordline.npck8)) * pqty4, pwgh5 WITH (ordline.npwght / (ordline.npck1 + ordline.npck2 + ordline.npck3 + ordline.npck4 + ordline.npck5 + ordline.npck6 + ordline.npck7 + ordline.npck8)) * pqty5, pwgh6 WITH (ordline.npwght / (ordline.npck1 + ordline.npck2 + ordline.npck3 + ordline.npck4 + ordline.npck5 + ordline.npck6 + ordline.npck7 + ordline.npck8)) * pqty6, pwgh7 WITH (ordline.npwght / (ordline.npck1 + ordline.npck2 + ordline.npck3 + ordline.npck4 + ordline.npck5 + ordline.npck6 + ordline.npck7 + ordline.npck8)) * pqty7, pwgh8 WITH (ordline.npwght / (ordline.npck1 + ordline.npck2 + ordline.npck3 + ordline.npck4 + ordline.npck5 + ordline.npck6 + ordline.npck7 + ordline.npck8)) * pqty8
  ELSE
   REPLACE pwgh1 WITH 0, pwgh2 WITH 0, pwgh3 WITH 0, pwgh4 WITH 0, pwgh5 WITH 0, pwgh6 WITH 0, pwgh7 WITH 0, pwgh8 WITH 0
  ENDIF
  REPLACE cnosize WITH lcsz
  REPLACE ptotwgh WITH (pwgh1 + pwgh2 + pwgh3 + pwgh4 + pwgh5 + pwgh6 + pwgh7 + pwgh8)
  REPLACE pack_id WITH IIF(llfrmpikln, pikline.pack_id, ordline.pack_id), cpkcolor WITH IIF(llfrmpikln, pikline.cpkcolor, ordline.cpkcolor), cpcksize WITH IIF(llfrmpikln, pikline.cpcksize, ordline.cpcksize), cpkversion WITH IIF(llfrmpikln, pikline.cpkversion, ordline.cpkversion)
  REPLACE lrange WITH IIF(llfrmpikln, pikline.lrange, ordline.lrange), llpack WITH !EMPTY(pack_id)
  IF gfseek(lcorder + pack_id + cpkcolor + cpcksize + cpkversion + style, 'TMPL_LIN')
   REPLACE ctnqty1 WITH tmpl_lin.qty1, ctnqty2 WITH tmpl_lin.qty2, ctnqty3 WITH tmpl_lin.qty3, ctnqty4 WITH tmpl_lin.qty4, ctnqty5 WITH tmpl_lin.qty5, ctnqty6 WITH tmpl_lin.qty6, ctnqty7 WITH tmpl_lin.qty7, ctnqty8 WITH tmpl_lin.qty8, ctntotqty WITH ctnqty1 + ctnqty2 + ctnqty3 + ctnqty4 + ctnqty5 + ctnqty6 + ctnqty7 + ctnqty8
   REPLACE weight1 WITH tmpl_lin.weight, weight2 WITH tmpl_lin.weight, weight3 WITH tmpl_lin.weight, weight4 WITH tmpl_lin.weight, weight5 WITH tmpl_lin.weight, weight6 WITH tmpl_lin.weight, weight7 WITH tmpl_lin.weight, weight8 WITH tmpl_lin.weight
  ENDIF
  IF lrange
   = gfseek(lcorder + pack_id + cpkcolor + cpcksize + cpkversion, 'TMPL_LIN')
   REPLACE weight1 WITH tmpl_lin.weight, weight2 WITH tmpl_lin.weight, weight3 WITH tmpl_lin.weight, weight4 WITH tmpl_lin.weight, weight5 WITH tmpl_lin.weight, weight6 WITH tmpl_lin.weight, weight7 WITH tmpl_lin.weight, weight8 WITH tmpl_lin.weight
  ENDIF
  loformset.llanyrec = .T.
 ENDFOR
ENDSCAN
SELECT (lcpcklin)
lcpack_id = ""
SCAN FOR lrange
 lnpckedqty = 0
 IF lcpack_id <> pack_id
  lcpack_id = pack_id
  IF gfseek(lcorder + pack_id + cpkcolor + cpcksize + cpkversion, 'TMPL_LIN')
   lnpckedqty = tmpl_lin.totqty
   lnweight = tmpl_lin.weight
  ENDIF
 ENDIF
 IF lnpckedqty > 0
  = gfseek(style, 'STYLE') .AND. gfseek('S' + style.scale, 'SCALE')
  IF lnpckedqty < ototqty
   lnpkqty = lnpckedqty / scale.cnt
   lnmod = MOD(lnpkqty, 1)
   lleven = .F.
   FOR lni = 1 TO scale.cnt
    lci = STR(lni, 1)
    REPLACE ctnqty&lci WITH IIF(!lleven,lnpkqty-lnmod,lnpkqty+lnmod)
    lleven = !lleven
   ENDFOR
  ELSE
   FOR lni = 1 TO scale.cnt
    lci = STR(lni, 1)
    REPLACE ctnqty&lci WITH oqty&lci
   ENDFOR
  ENDIF
  REPLACE ctntotqty WITH ctnqty1 + ctnqty2 + ctnqty3 + ctnqty4 + ctnqty5 + ctnqty6 + ctnqty7 + ctnqty8
 ENDIF
ENDSCAN
SELECT pack_lin
lctag = ORDER()
= gfsetorder('PackStyle')
IF !EMPTY(&lcstores..pack_no)  
 IF gfseek(&lcstores..pack_no,'Pack_Lin')
  SCAN REST WHILE pack_no = &lcstores..pack_no
   llstyfound = .F.
   lnrempqty1 = pack_lin.qty1
   lnrempqty2 = pack_lin.qty2
   lnrempqty3 = pack_lin.qty3
   lnrempqty4 = pack_lin.qty4
   lnrempqty5 = pack_lin.qty5
   lnrempqty6 = pack_lin.qty6
   lnrempqty7 = pack_lin.qty7
   lnrempqty8 = pack_lin.qty8
   lcsearexp = "'O'+lcorder+&lcStores..STORE+Pack_Lin.Style+STR(Pack_Lin.nOrdLineNo,6)"
   IF gfseek(&lcsearexp.,'Ordline')
    SELECT ordline
    lnpacklin = 0
    SCAN REST WHILE cordtype+ORDER+STORE+STYLE+STR(LINENO,6) =  &lcsearexp. 
     lnstyordlin = ordline.lineno
     llnothing = SEEK('O'+lcorder+&lcstores..STORE+pack_lin.STYLE+STR(lnstyordlin,6),'Ordline')
     SET ORDER IN (loformset.lcpcklin) TO (loformset.lcpakindxst)
     llnothing = SEEK(pack_id + cpkcolor + cpcksize + cpkversion + pack_lin.style + STR(lnstyordlin, 6), lcpcklin)
     IF (EMPTY(&lcstores..piktkt) AND !ordline.picked) OR (!EMPTY(&lcstores..piktkt) AND IIF(llfrmpikln,!ordline.picked,ordline.picked))
      SELECT (lcctndtl)
      lnpacklin = lnpacklin + 1
      = gfseek(pack_lin.style, 'Style')
      = gfseek("S" + style.scale, 'Scale')
      llfirstrec = .T.
      FOR lni = 1 TO scale.cnt
       lci = STR(lni, 1)
       IF pack_lin.qty&lci. = 0
        LOOP
       ENDIF
       APPEND BLANK
       REPLACE piktkt WITH &lcstores..piktkt               
       REPLACE STYLE      WITH pack_lin.STYLE, STORE      WITH &lcstores..STORE, szcnt      WITH &lcpcklin..szcnt, cstatus    WITH "A", nordlineno WITH lnstyordlin, packlineno WITH lnpacklin, cart_no    WITH pack_lin.no_cart, qty1       WITH pack_lin.qty1, qty2       WITH pack_lin.qty2, qty3       WITH pack_lin.qty3, qty4       WITH pack_lin.qty4, qty5       WITH pack_lin.qty5, qty6       WITH pack_lin.qty6, qty7       WITH pack_lin.qty7, qty8       WITH pack_lin.qty8, totqty     WITH qty1+qty2+qty3+qty4+qty5+qty6+qty7+qty8
       REPLACE size1      WITH IIF(qty1>0,&lcpcklin..csize1,size1), size2      WITH IIF(qty2>0,&lcpcklin..csize2,size2), size3      WITH IIF(qty3>0,&lcpcklin..csize3,size3), size4      WITH IIF(qty4>0,&lcpcklin..csize4,size4), size5      WITH IIF(qty5>0,&lcpcklin..csize5,size5), size6      WITH IIF(qty6>0,&lcpcklin..csize6,size6), size7      WITH IIF(qty7>0,&lcpcklin..csize7,size7), size8      WITH IIF(qty8>0,&lcpcklin..csize8,size8)
       REPLACE br1        WITH !EMPTY(qty1), br2        WITH !EMPTY(qty2), br3        WITH !EMPTY(qty3), br4        WITH !EMPTY(qty4), br5        WITH !EMPTY(qty5), br6        WITH !EMPTY(qty6), br7        WITH !EMPTY(qty7), br8        WITH !EMPTY(qty8), weight1    WITH qty1*(pack_lin.weight/pack_lin.totqty), weight2    WITH qty2*(pack_lin.weight/pack_lin.totqty), weight3    WITH qty3*(pack_lin.weight/pack_lin.totqty), weight4    WITH qty4*(pack_lin.weight/pack_lin.totqty), weight5    WITH qty5*(pack_lin.weight/pack_lin.totqty), weight6    WITH qty6*(pack_lin.weight/pack_lin.totqty), weight7    WITH qty7*(pack_lin.weight/pack_lin.totqty), weight8    WITH qty8*(pack_lin.weight/pack_lin.totqty), orgwgh     WITH &lcpcklin..orgstywgh
       REPLACE totweight WITH (weight1 + weight2 + weight3 + weight4 + weight5 + weight6 + weight7 + weight8)
       REPLACE cnosize WITH lci
      ENDFOR
      IF SEEK(STR(pack_lin.no_cart, 4), lcctnhdr)
       SELECT (lcctnhdr)
       REPLACE totpcs WITH totpcs +  &lcctndtl..qty1+&lcctndtl..qty2+ &lcctndtl..qty3+&lcctndtl..qty4+ &lcctndtl..qty5+&lcctndtl..qty6+ &lcctndtl..qty7+&lcctndtl..qty8, totwgh WITH totwgh +  &lcctndtl..weight1+&lcctndtl..weight2+ &lcctndtl..weight3+&lcctndtl..weight4+ &lcctndtl..weight5+&lcctndtl..weight6+ &lcctndtl..weight7+&lcctndtl..weight8
      ELSE
       IF (&lcctndtl..qty1+&lcctndtl..qty2+ &lcctndtl..qty3+&lcctndtl..qty4+ &lcctndtl..qty5+&lcctndtl..qty6+ &lcctndtl..qty7+&lcctndtl..qty8) > 0
        loformset.lnmaxctn = MAX(loformset.lnmaxctn, pack_lin.no_cart)
        INSERT INTO (lcctnhdr) (cart_no,pal_no,totpcs,totwgh,EMPTY, STORE,piktkt) VALUES (pack_lin.no_cart,pack_lin.npltno, &lcctndtl..qty1+&lcctndtl..qty2+ &lcctndtl..qty3+&lcctndtl..qty4+ &lcctndtl..qty5+&lcctndtl..qty6+ &lcctndtl..qty7+&lcctndtl..qty8, &lcctndtl..weight1+&lcctndtl..weight2+ &lcctndtl..weight3+&lcctndtl..weight4+ &lcctndtl..weight5+&lcctndtl..weight6+ &lcctndtl..weight7+&lcctndtl..weight8,'N', &lcstores..STORE, &lcstores..piktkt)
       ENDIF
      ENDIF
     ENDIF
    ENDSCAN
   ENDIF
  ENDSCAN
 ENDIF
ELSE
 STORE 1 TO loformset.lnfrom, loformset.lnto
ENDIF
SELECT pack_lin
= gfsetorder(lctag)
ENDPROC
**
PROCEDURE lfwStrsBr1
PARAMETER loformset
PRIVATE lnalias, laselct, lcopntmpl
lcstores = loformset.lcstores
lcorder = loformset.ariaform1.kborderno.keytextbox.value
lnalias = SELECT(0)
SELECT (lcstores)
lnstbrrec = RECNO()
DIMENSION laselct[1]
laselct = .F.
SELECT COUNT(lselect) FROM (lcstores) WHERE !EMPTY(lcstores) INTO ARRAY laselct
loformset.ariaform1.pgfpacking.header.cmdgen.enabled = !EMPTY(laselct(1)) .AND. loformset.activemode $ 'EA'
loformset.ariaform1.pgfpacking.header.cmdtemp.enabled = !EMPTY(lcorder)
SELECT (lcstores)
SELECT (lnalias)
ENDPROC
**
PROCEDURE lfDtlBrow
PARAMETER loformset
PRIVATE lncuralias
lncuralias = SELECT(0)
lcpcklin = loformset.lcpcklin
lctmppck = loformset.lctmppck
IF !USED(lctmppck)
 IF gfgetmemvar('M_ORDSTUTS', oariaapplication.activecompanyid) = 'L'
  USE (oariaapplication.workdir + lcpcklin) AGAIN ALIAS (lctmppck) ORDER (loformset.lcpakindxln) IN 0
 ELSE
  USE (oariaapplication.workdir + lcpcklin) AGAIN ALIAS (lctmppck) ORDER (loformset.lcpakindxst) IN 0
 ENDIF
ENDIF
lctemfile = lctmppck
SELECT (lctemfile)
lndtbrrec = RECNO()
WITH loformset.ariaform1.pgfpacking.detail.grddetail
 .recordsource = ''
 SELECT (loformset.lctmppck)
 LOCATE
 IF EOF()
  RETURN
 ENDIF
 .recordsource = loformset.lctmppck
 .column2.controlsource = 'ThisFormset.lfGetDetPack()'
 .column2.header1.caption = 'Pack_Id-Color-Size-Version'
 .column2.visible = .T.
 .column3.controlsource = 'ThisFormSet.lfIsRange()'
 .column3.header1.caption = 'Range'
 .column3.width = 60
 .column3.visible = .T.
 .column4.controlsource = loformset.lctmppck + '.Style'
 .column4.header1.caption = loformset.lcstyttl
 .column4.width = 180
 .column4.visible = .T.
 .column5.controlsource = "THISFormSet.lfGetSz()"
 .column5.header1.caption = 'Size'
 .column5.visible = .T.
 .column6.controlsource = "THISFormSet.lfGetOQty()"
 .column6.header1.caption = "O.Qty."
 .column6.visible = .T.
 .column7.controlsource = "THISFormSet.lfGetCtnQty()"
 .column7.header1.caption = "Qty.\Ctn"
 .column7.visible = .T.
 .column8.controlsource = "THISFormSet.lfGetWghUnt()"
 .column8.header1.caption = 'Wgh.\Unt'
 .column8.visible = .T.
 .column9.controlsource = "THISFormSet.lfGetPQty()"
 .column9.header1.caption = 'P.Qty.'
 .column9.visible = .T.
 .column10.controlsource = "THISFormSet.lfGetPWgh()"
 .column10.header1.caption = 'P.Wgh.'
 .column10.visible = .T.
 .readonly = .T.
ENDWITH
ENDPROC
**
FUNCTION lfGetPackId
PARAMETER loformset
RETURN EVALUATE(loformset.lctmppck + '.Pack_Id') + '-' + EVALUATE(loformset.lctmppck + '.cPkColor') + '-' + lfgetgmsz(EVALUATE(loformset.lctmppck + '.cPckSize')) + '-' + EVALUATE(loformset.lctmppck + '.cPkVersion')
ENDFUNC
**
FUNCTION lfGetGmSz
PARAMETER lcpacksize
PRIVATE lcnombr
IF !EMPTY(lcpacksize)
 = gfseek('S' + LEFT(lcpacksize, 1), 'SCALE')
 lcnombr = RIGHT(lcpacksize, 1)
 lclocsize = EVALUATE('SCALE.SZ' + lcnombr)
ELSE
 lclocsize = '*****'
ENDIF
RETURN lclocsize
ENDFUNC
**
FUNCTION lfIsRange
PARAMETER loformset
RETURN IIF(EVALUATE(loformset.lctmppck + '.llPack'), IIF(EVALUATE(loformset.lctmppck + '.lRange'), 'YES', 'NO'), '')
ENDFUNC
**
PROCEDURE lfOrdBrow
LPARAMETERS lcorder, lcaccount
PRIVATE lcfields, labrowarr, lncuralias, lccurtag, llreturn, lctag, lcbrfields
DIMENSION labrowarr[1]
STORE SPACE(0) TO lcfields, labrowarr, lcbrfields
lncuralias = SELECT(0)
SELECT ordhdr
SET RELATION ADDITIVE TO 'M' + ordhdr.account INTO customer
= gfseek('O')
LOCATE
lctag = ORDER('OrdHdr')
lcbrfields = [Order:H="Order#",status:3:H="Status",lcSesDesc=gfCodDes(Season,'SEASON'):H="Season",lcDivDesc=gfCodDes(cDivision,'CDIVISION'):H="Division",] + 'CustPo=IIF(multipo,"*Multi_PO*",custpo):H="Cust. P.O#",' + [ACCOUNT:H="Acct",store=IIF(MULTI='Y',"*Multi*",STORE):H="Store",Customer.stname] + ':15:H="Name",Open:H="Open.Qty.",OpenAmt:H="Open.Amt.",Ship:H="Ship.Qty.",Shipamt:H="Ship.Amt.",' + 'start:H="Start",Complete:H="Complete",' + 'Note1:6:H="Notes"'
DO CASE
 CASE !EMPTY(lcaccount)
  = gfsetorder('OrdAcct')
  lcacc = lcaccount
  lcorder = IIF(ariabrow("lcAcc+'O'", "Orders", gnbrfsrow1, gnbrfscol1, gnbrfsrow2, gnbrfscol2, '', '', 'Order', 'laBrowArr'), ordhdr.order, SPACE(6))
 CASE EMPTY(lcaccount)
  gfsetorder('OrdHdr')
  lcorder = IIF(ariabrow("'O'", "Orders", gnbrfsrow1, gnbrfscol1, gnbrfsrow2, gnbrfscol2, '', '', 'Order', 'laBrowArr'), ordhdr.order, SPACE(6))
ENDCASE
SELECT ordhdr
SET RELATION TO
gfsetorder(lctag)
SELECT (lncuralias)
ENDPROC
**
PROCEDURE lfvAccount
LPARAMETERS loformset, lcaccount, llbrowse
IF llbrowse .OR. (!EMPTY(lcaccount) .AND. !gfseek('M' + lcaccount, 'Customer'))
 DO cusbrowm WITH lcaccount
ENDIF
loformset.ariaform1.kbaccount.keytextbox.value = lcaccount
loformset.ariaform1.txtcustname.value = IIF(!EMPTY(lcaccount), customer.stname, '')
RETURN
ENDPROC
**
PROCEDURE lfActFold
LPARAMETERS loformset, lnactfolder
lcstores = loformset.lcstores
lctmppck = loformset.lctmppck
lcctnhdr = loformset.lcctnhdr
IF loformset.activemode $ 'S'
 RETURN
ENDIF
STORE IIF((loformset.activemode $ 'AE' .OR. loformset.llnew) .AND. lnactfolder = 1, .T., .F.) TO llinststat, llshipstat
= lfwinhdrst(loformset)
DO CASE
 CASE lnactfolder = 2
  IF USED(lctmppck)
   SELECT &lctmppck
   SET FILTER TO STORE = &lcstores..STORE  AND piktkt = &lcstores..piktkt
   GOTO TOP
  ENDIF
  = lfdtlbrow(loformset)
 CASE lnactfolder = 3
  IF USED(lcctnhdr)
   SELECT (lcctnhdr)
   SET FILTER TO STORE = &lcstores..STORE  AND piktkt = &lcstores..piktkt
   GOTO TOP
  ENDIF
  WITH loformset.ariaform1.pgfpacking.cartoninfo.grdcartonh
   .recordsource = ''
   SELECT (loformset.lcctnhdr)
   .recordsource = loformset.lcctnhdr
   .column1.controlsource = loformset.lcctnhdr + '.Cart_No'
   .column1.header1.caption = 'Cart.#'
   .column1.header1.alignment = 1
   .columns(1).alignment = 1
   .columns(1).width = 40
   .column2.visible = .F.
   .column3.controlsource = loformset.lcctnhdr + '.TotPcs'
   .column3.header1.caption = 'Tot.Pcs'
   .column3.header1.alignment = 1
   .columns(3).alignment = 1
   .columns(3).width = 70
   .column4.controlsource = loformset.lcctnhdr + '.TotWgh'
   .column4.header1.alignment = 1
   .column4.header1.caption = 'Tot.Wgh'
   .columns(4).alignment = 1
   .columns(4).width = 80
   .column5.visible = .F.
   .setall('ReadOnly', .T., 'COLUMN')
  ENDWITH
  WITH loformset.ariaform1.pgfpacking.cartoninfo.grdcartond
   .recordsource = ''
   SELECT (loformset.lcctndtl)
   .recordsource = loformset.lcctndtl
   .column6.visible = .F.
   .column1.controlsource = loformset.lcctndtl + '.Style'
   .column1.header1.caption = loformset.lcstyttl
   .columns(1).width = 120
   .column1.columnorder = 2
   .column2.visible = .F.
   .column3.controlsource = "THISFormSet.lfGetSzdet()"
   .columns(3).width = 40
   .column3.columnorder = 4
   .column4.controlsource = "THISFormSet.lfGetQtyDet()"
   .columns(4).width = 40
   .column4.header1.alignment = 1
   .columns(4).alignment = 1
   .column4.columnorder = 5
   .column5.controlsource = "THISFormSet.lfGetWghDET()"
   .columns(5).width = 90
   .column5.header1.alignment = 1
   .columns(5).alignment = 1
   .column5.columnorder = 6
   .setall('ReadOnly', .T., 'COLUMN')
  ENDWITH
  = lfwctnhdrbrp(loformset)
ENDCASE
ENDPROC
**
PROCEDURE lfWinHdrSt
LPARAMETERS loformset
IF loformset.activemode $ 'AE'
 WITH loformset.ariaform1
  STORE .F. TO .kborderno.enabled, .kbaccount.enabled, .txtcustname.enabled, .txtcustpo.enabled
 ENDWITH
ENDIF
ENDPROC
**
PROCEDURE lfwCtnHdrBrP
LPARAMETERS loformset
LOCAL lnalias
lnalias = SELECT(0)
lcctnhdr = loformset.lcctnhdr
SELECT (loformset.lcctndtl)
SET KEY TO STR(EVALUATE(loformset.lcctnhdr + '.Cart_No'), 4)
= SEEK(STR(EVALUATE(loformset.lcctnhdr + '.Cart_No'), 4))
loformset.ariaform1.pgfpacking.cartoninfo.grdcartond.refresh
SELECT (loformset.lcctnhdr)
SELECT (lnalias)
ENDPROC
**
FUNCTION lfGetSz
PARAMETER loformset
lcsizeno = EVALUATE(loformset.lctmppck + '.cNoSize')
IF !EMPTY(lcsizeno)
 RETURN EVALUATE(loformset.lctmppck + '.cSize' + lcsizeno)
ELSE
 RETURN ''
ENDIF
ENDFUNC
**
FUNCTION lfGetOQty
PARAMETER loformset
lcsizeno = EVALUATE(loformset.lctmppck + '.cNoSize')
IF !EMPTY(lcsizeno)
 RETURN EVALUATE(loformset.lctmppck + '.OQty' + lcsizeno)
ELSE
 RETURN 0
ENDIF
ENDFUNC
**
FUNCTION lfGetCtnQty
PARAMETER loformset
lcsizeno = EVALUATE(loformset.lctmppck + '.cNoSize')
IF !EMPTY(lcsizeno)
 RETURN EVALUATE(loformset.lctmppck + '.CtnQty' + lcsizeno)
ELSE
 RETURN 0
ENDIF
ENDFUNC
**
FUNCTION lfGetWghUnt
PARAMETER loformset
lcsizeno = EVALUATE(loformset.lctmppck + '.cNoSize')
IF !EMPTY(lcsizeno)
 RETURN EVALUATE(loformset.lctmppck + '.Weight' + lcsizeno)
ELSE
 RETURN 0
ENDIF
ENDFUNC
**
FUNCTION lfGetPQty
PARAMETER loformset
lcsizeno = EVALUATE(loformset.lctmppck + '.cNoSize')
IF !EMPTY(lcsizeno)
 RETURN EVALUATE(loformset.lctmppck + '.PQty' + lcsizeno)
ELSE
 RETURN 0
ENDIF
ENDFUNC
**
FUNCTION lfGetPWgh
PARAMETER loformset
lcsizeno = EVALUATE(loformset.lctmppck + '.cNoSize')
IF !EMPTY(lcsizeno)
 RETURN EVALUATE(loformset.lctmppck + '.PWgh' + lcsizeno)
ELSE
 RETURN 0
ENDIF
ENDFUNC
**
FUNCTION lfGetQtyDet
PARAMETER loformset
lcsizeno = EVALUATE(loformset.lcctndtl + '.cNoSize')
IF !EMPTY(lcsizeno)
 RETURN EVALUATE(loformset.lcctndtl + '.Qty' + lcsizeno)
ELSE
 RETURN 0
ENDIF
ENDFUNC
**
FUNCTION lfGetWghDET
PARAMETER loformset
lcsizeno = EVALUATE(loformset.lcctndtl + '.cNoSize')
IF !EMPTY(lcsizeno)
 RETURN EVALUATE(loformset.lcctndtl + '.Weight' + lcsizeno)
ELSE
 RETURN 0
ENDIF
ENDFUNC
**
FUNCTION lfGetSzdet
PARAMETER loformset
lcsizeno = EVALUATE(loformset.lcctndtl + '.cNoSize')
IF !EMPTY(lcsizeno)
 RETURN EVALUATE(loformset.lcctndtl + '.Size' + lcsizeno)
ELSE
 RETURN ''
ENDIF
ENDFUNC
**
PROCEDURE lfvSelSto
PARAMETER loformset, lcseltyp
lcstores = loformset.lcstores
PRIVATE lnalias, lnrecno
lnalias = SELECT()
SELECT &lcstores
lnrecno = RECNO(lcstores)
DO CASE
 CASE lcseltyp = 'S'
  IF EMPTY(pack_no)
   REPLACE lselect WITH .T.
  ELSE
   REPLACE lselect WITH .F.
  ENDIF
 CASE lcseltyp = 'A'
  GOTO TOP
  REPLACE lselect WITH .T. FOR EMPTY(pack_no)
 CASE lcseltyp = 'N'
  GOTO TOP
  REPLACE lselect WITH .F. ALL
ENDCASE
IF BETWEEN(lnrecno, 1, RECCOUNT(lcstores))
 GOTO (lnrecno) IN (lcstores)
ENDIF
= lfwstrsbr(loformset)
SELECT (lnalias)
ENDPROC
**
PROCEDURE lfOpnTmpl
PARAMETER loformset
PRIVATE lcorder
lcorder = "'" + ALLTRIM(loformset.ariaform1.kborderno.keytextbox.value) + "'"
oariaapplication.doprogram("AWRALAUPLJ", lcorder, .F., 'AL')
ENDPROC
**
PROCEDURE lfvGenPL
PARAMETER loformset
PRIVATE lccurs, lci, lnsvrec, lni, lncartons, lnj, lncrtsum, lccurrsty
lcstores = loformset.lcstores
lcerorfil = loformset.lcerorfil
lcaccount = loformset.ariaform1.kbaccount.keytextbox.value
IF loformset.activemode $ 'SV'
 RETURN
ENDIF
lcorder = loformset.ariaform1.kborderno.keytextbox.value
= gfseek('O' + lcorder, 'ORDHDR')
IF ordhdr.status = 'C'
 = gfmodalgen('INM00000B00000', .F., .F., .F., 'Order is invoiced, can not generate!')
 RETURN
ENDIF
SELECT &lcstores
lnsvrec = RECNO(lcstores)
LOCATE FOR lselect
IF !FOUND()
 = gfmodalgen('INM00000B00000', .F., .F., .F., 'No Store selected.')
 RETURN
ENDIF
lccurs = gftempname()
lcpcklin = loformset.lcpcklin
lcctnhdr = loformset.lcctnhdr
lcctndtl = loformset.lcctndtl
SELECT tmpl_hdr
= gfseek(lcorder, 'TMPL_HDR')
SELECT &lcpcklin 
lcrelation = SET('RELATION')
lcskip = SET('SKIP')
lnorder = ORDER(lcpcklin)
SET RELATION TO
SET ORDER TO (loformset.lcpakindxst)
lncrthord = ORDER(lcctnhdr)
SET ORDER IN (lcctnhdr) TO STORE
lncrtdord = ORDER(lcctndtl)
SET ORDER IN (lcctndtl) TO (lcctndtl)
DIMENSION laerrarr[1, 4]
laerrarr[1, 1] = 'cError'
laerrarr[1, 2] = 'C'
laerrarr[1, 3] = 80
laerrarr[1, 4] = 0
= gfcrttmp(loformset.lcerorfil, @laerrarr)
llgenrted = .F.
= lfchkerrs(loformset)
llnoerr = (RECCOUNT(lcerorfil) = 0)
llcontinue = .T.
IF RECCOUNT(lcerorfil) > 0
 = lferrdsply(loformset)
 IF llcontinue .AND. tmpl_hdr.llckonerr
  = gfmodalgen('INM00000B00000', .F., .F., .F., 'Order is locked on errors, can not create P/L for all selected stores')
 ENDIF
ENDIF
IF llcontinue .AND. (!tmpl_hdr.llckonerr .OR. (tmpl_hdr.llckonerr .AND. llnoerr))
 SELECT &lcstores
 GOTO TOP
 SCAN FOR lselect
  lncrtsum = 0
  SELECT &lcpcklin    
  SET FILTER TO
  GOTO TOP
  DIMENSION lacarstru[3, 4]
  lacarstru[1, 1] = 'NCARTON'
  lacarstru[1, 2] = 'N'
  lacarstru[1, 3] = 4
  lacarstru[1, 4] = 0
  lacarstru[2, 1] = 'NCOUNT'
  lacarstru[2, 2] = 'N'
  lacarstru[2, 3] = 4
  lacarstru[2, 4] = 0
  lacarstru[3, 1] = 'NFROM'
  lacarstru[3, 2] = 'N'
  lacarstru[3, 3] = 4
  lacarstru[3, 4] = 0
  = gfcrttmp(lccurs, @lacarstru, 'NCARTON', lccurs)
  = lfctnorg(loformset)
  WAIT WINDOW NOWAIT 'Generating Cartons for store '+&lcstores..STORE + ' '+IIF(!EMPTY(&lcstores..dist_ctr),'D.C. ' +&lcstores..dist_ctr , '')
  SELECT &lcpcklin
  SET FILTER TO
  SET FILTER TO STORE = &lcstores..STORE AND NOT lrange AND piktkt = &lcstores..piktkt
  GOTO TOP
  = gfseek(lcorder, 'TMPL_LIN')
  SELECT tmpl_lin
  SCAN REST FOR tmpl_lin.totqty > 0 WHILE order + STR(no_cart, 4) + pack_id + cpkcolor + cpksize + cpkversion + style = lcorder
   lnlinpk = 1
   lccurrsty = tmpl_lin.pack_id + tmpl_lin.cpkcolor + tmpl_lin.cpksize + tmpl_lin.cpkversion + tmpl_lin.style
   IF SEEK(lccurrsty, lcpcklin)
    SELECT &lcpcklin
    SCAN REST WHILE pack_id + cpkcolor + cpcksize + cpkversion + style + STR(nordlineno, 6) = lccurrsty
     DIMENSION laqty[8, 3]
     laqty = 0
     lncartons = 0
     FOR lni = 1 TO 8
      lci = STR(lni, 1)
      IF &lcpcklin..cnosize <> lci
       LOOP
      ENDIF
      laqty[lni,1] = &lcpcklin..avlqty&lci
      laqty[lni,2] = tmpl_lin.qty&lci
      laqty[lni,3] = IIF( tmpl_lin.qty&lci>0 , &lcpcklin..avlqty&lci/tmpl_lin.qty&lci , 0 )
      IF laqty(lni, 3) > lncartons
       lncartons = CEILING(laqty(lni, 3))
      ENDIF
     ENDFOR
     SELECT &lcpcklin
     lcsz = &lcpcklin..cnosize
     REPLACE pqty&lcsz.    WITH oqty&lcsz., ptotqty WITH pqty1+pqty2+pqty3+pqty4+pqty5+pqty6+pqty7+pqty8, pwgh&lcsz.   WITH pqty&lcsz.* weight&lcsz., ptotwgh WITH pwgh1+pwgh2+pwgh3+pwgh4+pwgh5+pwgh6+pwgh7+pwgh8           
     = gfseek(tmpl_lin.style, 'STYLE')
     = gfseek('S' + style.scale, 'SCALE')
     = gfseek(tmpl_lin.ncarton, lccurs)
     lnpackno = 0
     IF !EMPTY(&lcpcklin..pack_id)            
      lcsvord = ORDER('SPCK_LIN')
      SELECT spck_lin
      = gfsetorder('SPCK_LINVR')
      SELECT &lcpcklin
      IF gfseek('P' + lcaccount + pack_id + cpkcolor + cpcksize + cpkversion + style, 'SPCK_LIN') .OR. gfseek('P*****' + pack_id + cpkcolor + cpcksize + cpkversion + style, 'SPCK_LIN')
       lnpackno = pqty&lcsz./spck_lin.qty&lcsz.
      ENDIF
      SELECT spck_lin
      = gfsetorder(lcsvord)
      SELECT &lcpcklin
     ENDIF
     FOR lncrtn = 1 TO lncartons
      IF MIN(laqty[VAL(lcsz),1],tmpl_lin.qty&lcsz.) = 0
       LOOP
      ENDIF
      SELECT (lcctndtl)
      APPEND BLANK
      REPLACE cnosize WITH &lcpcklin..cnosize
      REPLACE piktkt WITH &lcstores..piktkt
      REPLACE cart_no WITH lncrtn+&lccurs..nfrom, STORE      WITH &lcstores..STORE, nordlineno WITH &lcpcklin..nordlineno, STYLE      WITH tmpl_lin.STYLE, szcnt      WITH SCALE.CNT, SIZE&lcsz.     WITH SCALE.sz&lcsz.
      REPLACE qty&lcsz.       WITH IIF(laqty[VAL(lcsz),1] > 0 , MIN(laqty[VAL(lcsz),1],tmpl_lin.qty&lcsz.) , 0 ), totqty     WITH qty1+qty2+qty3+qty4+qty5+qty6+qty7+qty8                    
      REPLACE weight&lcsz.    WITH IIF(qty&lcsz. > 0 , qty&lcsz. *tmpl_lin.weight , 0), totweight  WITH weight1+weight2+weight3+weight4+ weight5+weight6+weight7+weight8                              
      REPLACE br1 WITH !EMPTY(qty1), br2 WITH !EMPTY(qty2), br3 WITH !EMPTY(qty3), br4 WITH !EMPTY(qty4), br5 WITH !EMPTY(qty5), br6 WITH !EMPTY(qty6), br7 WITH !EMPTY(qty7), br8 WITH !EMPTY(qty8)
      IF !EMPTY(&lcpcklin..pack_id)
       REPLACE pack_id WITH spck_lin.pack_id, cpkcolor WITH spck_lin.cpkcolor, cpcksize WITH spck_lin.cpcksize, cpkversion WITH spck_lin.cpkversion, npackno WITH lnpackno
      ENDIF
      FOR lnj = 1 TO 8
       lcj = STR(lnj, 1)
       laqty[lnj,1] = MAX( laqty[lnj,1] - qty&lcj , 0 )
      ENDFOR
      IF !SEEK(&lcstores..STORE+&lcstores..piktkt+STR(lncrtn+&lccurs..nfrom,4),lcctnhdr)
       INSERT INTO (lcctnhdr) (cart_no,pal_no,EMPTY,STORE,piktkt) VALUES (lncrtn+&lccurs..nfrom,0, 'N',&lcstores..STORE,&lcstores..piktkt)
       lncrtsum = lncrtsum + 1
      ENDIF
      REPLACE &lcctnhdr..totpcs WITH &lcctnhdr..totpcs + &lcctndtl..totqty, &lcctnhdr..totwgh WITH &lcctnhdr..totwgh + &lcctndtl..totweight
      loformset.lnpackwgh = loformset.lnpackwgh + &lcctndtl..totweight
      SELECT &lcstores
      REPLACE totpqty WITH totpqty + &lcctndtl..totqty , weight  WITH weight  + &lcctndtl..totweight
     ENDFOR
    ENDSCAN
    lnlinpk = lnlinpk + 1
   ENDIF
  ENDSCAN
  = lfupdrngpk(loformset)
  SELECT &lcstores
  REPLACE cartons WITH lncrtsum, pack_no WITH "######", lselect WITH .F.
  llgenrted = .T.
 ENDSCAN
ENDIF
IF llgenrted
 = lfwstrsbr(loformset)
 = gfmodalgen('INM00000B00000', .F., .F., .F., 'Packing Lists are generated.')
 LOCATE FOR totord <> totpqty .AND. !EMPTY(pack_no)
 IF FOUND()
  = gfmodalgen('INM00000B00000', .F., .F., .F., 'Total Packed Qty not equal to Total OrdQty for some stores.')
 ENDIF
ENDIF
SELECT &lcpcklin
SET RELATION TO &lcrelation
SET SKIP TO &lcskip
SET ORDER TO (lnorder)
SET ORDER TO &lncrthord IN (lcctnhdr)
IF USED(lccurs)
 USE IN &lccurs 
ENDIF
IF BETWEEN(lnsvrec, 1, RECCOUNT(lcstores))
 GOTO (lnsvrec) IN (lcstores)
ENDIF
ENDPROC
**
PROCEDURE lfChkErrs
PARAMETER loformset
PRIVATE lncnt, lncartons, lnalias, lcsvorder, lcsvorder2, lni, lci, lccurrsty
lnalias = SELECT()
lcorder = loformset.ariaform1.kborderno.keytextbox.value
lcpcklin = loformset.lcpcklin
lcstores = loformset.lcstores
lcsvorder = ORDER(lcpcklin)
lcsvorder2 = ORDER('TMPL_LIN')
SET ORDER IN (lcpcklin) TO STORE
SELECT tmpl_lin
= gfsetorder('TMPL_LINS')
lcerorfil = loformset.lcerorfil
SELECT &lcpcklin
SET FILTER TO
GOTO TOP
SELECT &lcstores
GOTO TOP
SCAN FOR lselect
 lcmissed = ' '
 lladd = .F.
 SELECT (lcpcklin)
 IF SEEK(&lcstores..STORE,lcpcklin)
  SCAN REST WHILE STORE = &lcstores..STORE
   IF lrange
    LOOP
   ENDIF
   lccurrsty = lcorder + pack_id + cpkcolor + cpcksize + cpkversion + style
   IF !gfseek(lcorder + pack_id + cpkcolor + cpcksize + cpkversion + style, 'TMPL_LIN') .AND. !(style $ lcmissed)
    INSERT INTO (lcerorfil) VALUES (&lcstores..STORE+'      | '+&lcpcklin..STYLE+'  | MISSED')
    lcmissed = lcmissed + &lcpcklin..STYLE + '|'
    lladd = .T.
    LOOP
   ENDIF
   SELECT tmpl_lin
   SCAN REST WHILE order + pack_id + cpkcolor + cpksize + cpkversion + style = lccurrsty
    FOR lni = 1 TO 8
     lci = STR(lni, 1)
     IF tmpl_lin.qty&lci > 0 .AND.  &lcpcklin..avlqty&lci > tmpl_lin.qty&lci .AND.  MOD( &lcpcklin..avlqty&lci , tmpl_lin.qty&lci ) > 0
      INSERT INTO (lcerorfil) VALUES (&lcstores..STORE+'    | '   + &lcpcklin..STYLE+'|'    + &lcpcklin..csize&lci+'|  '+ ALLT(STR(tmpl_lin.ncarton))+ '  | Division '+ 'O.Qty='+LTRIM(STR(&lcpcklin..avlqty&lci))+ ', Qty/Ctn='+LTRIM(STR(tmpl_lin.qty&lci)))
      lladd = .T.
     ENDIF
    ENDFOR
   ENDSCAN
  ENDSCAN
 ENDIF
 IF lladd
  INSERT INTO (lcerorfil) VALUES (REPLICATE('=', 80))
 ENDIF
ENDSCAN
SET ORDER TO &lcsvorder  IN (lcpcklin)
SELECT tmpl_lin
= gfsetorder(lcsvorder2)
SELECT (lnalias)
ENDPROC
**
PROCEDURE lfErrDsply
PARAMETER loformset
lcerorfil = loformset.lcerorfil
PRIVATE lcerrosrc, lcnote, lcprocstat
lcerrosrc = (oariaapplication.workdir + 'Error.TXT')
lcnote = IIF(tmpl_hdr.llckonerr, 'Order is locked on errors', '')
llprocstat = !tmpl_hdr.llckonerr
IF oariaapplication.multiinst
 lcparmlst = "loFormset,lcNote,llProcStat ,lcErroSrc"
 = gfcallform('ALERRORS', 'AL', lcparmlst)
ELSE
 DO FORM (oariaapplication.screenhome+'AL\ALERRORS.scx') WITH loformset, lcnote, llprocstat, lcerrosrc
ENDIF
COPY TO (lcerrosrc) SDF
USE IN (lcerorfil)
ENDPROC
**
PROCEDURE lfvErrSrc
PARAMETER lobranchform
PRIVATE lcret, lcsavdef, lcfullpath
lcfullpath = SET('FULLPATH')
lcsavdef = FULLPATH('')
SET DEFAULT TO (oariaapplication.workdir)
lcret = GETFILE('TXT', 'Select File To Save.')
IF !EMPTY(lcret)
 lcerrosrc = lcret
ENDIF
lobranchform.ariaform1.txtflpth.value = lcerrosrc
SET DEFAULT  TO &lcsavdef
SET FULLPATH &lcfullpath
ENDPROC
**
PROCEDURE lfCtnOrg
PARAMETER loformset
lcpcklin = loformset.lcpcklin
lcstores = loformset.lcstores
PRIVATE lncnt, lncartons, lnalias, lccurrsty, lcoldsty, laavlqty, lcsto
lnalias = SELECT()
lcorder = loformset.ariaform1.kborderno.keytextbox.value
STORE ' ' TO lccurrsty, lcoldsty
DIMENSION laavlqty[9]
SELECT tmpl_lin
= gfseek(lcorder, 'TMPL_LIN')
SCAN REST FOR tmpl_lin.totqty > 0 WHILE order + STR(no_cart, 4) + pack_id + cpkcolor + cpksize + cpkversion + style = lcorder
 lccurrsty = pack_id + cpkcolor + cpksize + cpkversion + style
 lccurrpck = pack_id + cpkcolor + cpksize + cpkversion
 IF lcoldsty <> lccurrsty
  laavlqty = 0
  lcoldsty = lccurrsty
  lcsto = &lcstores..STORE
  SELECT &lcpcklin
  = SEEK(lccurrsty, lcpcklin)
  lcpckorsty = IIF(&lcpcklin..lrange,lccurrpck,lccurrsty)
  lncntsz =&lcpcklin..szcnt
  FOR lna = 1 TO lncntsz
   = SEEK(lccurrsty, lcpcklin)
   DIMENSION laszsum[1]
   STORE 0 TO laszsum
   lca = STR(lna, 1)
   SELECT SUM(avlqty&lca.) FROM &lcpcklin WHERE pack_id+cpkcolor+cpcksize+cpkversion+STYLE = lcpckorsty .AND. STORE = lcsto  AND cnosize = lca INTO ARRAY laszsum
   IF !ISNULL(laszsum(1))
    laavlqty[lna] = laszsum(1)
    laavlqty[9] = laavlqty(9) + laszsum(1)
   ENDIF
  ENDFOR
 ENDIF
 IF laavlqty(9) > 0
  lnpsum = IIF(tmpl_lin.qty1 > 0, laavlqty(1), 0) + IIF(tmpl_lin.qty2 > 0, laavlqty(2), 0) + IIF(tmpl_lin.qty3 > 0, laavlqty(3), 0) + IIF(tmpl_lin.qty4 > 0, laavlqty(4), 0) + IIF(tmpl_lin.qty5 > 0, laavlqty(5), 0) + IIF(tmpl_lin.qty6 > 0, laavlqty(6), 0) + IIF(tmpl_lin.qty7 > 0, laavlqty(7), 0) + IIF(tmpl_lin.qty8 > 0, laavlqty(8), 0)
  lncartons = CEILING(lnpsum / tmpl_lin.totqty)
  IF &lcpcklin..lrange
   lncartons = CEILING(laavlqty(9) / tmpl_lin.totqty)
  ENDIF
  IF !SEEK(tmpl_lin.ncarton, lccurs)
   INSERT INTO (lccurs) (ncarton) VALUES (tmpl_lin.ncarton)
  ENDIF
  SELECT &lccurs
  REPLACE ncount WITH MAX(ncount, lncartons)
 ENDIF
ENDSCAN
SELECT (lccurs)
SET ORDER TO
GOTO TOP
lncnt = 0
SCAN
 REPLACE nfrom WITH lncnt
 lncnt = lncnt + ncount
ENDSCAN
SET ORDER IN (lccurs) TO (lccurs)
LOCATE
SELECT (lnalias)
ENDPROC
**
PROCEDURE lfUpdRngPk
PARAMETER loformset
lctmpupln = loformset.lctmpupln
lcctndtl = loformset.lcctndtl
lcstores = loformset.lcstores
IF USED(lctmpupln)
 USE IN (lctmpupln)
ENDIF
DIMENSION lauplnstr[19, 4]
lauplnstr[1, 1] = 'Style'
lauplnstr[1, 2] = 'C'
lauplnstr[1, 3] = 19
lauplnstr[1, 4] = 0
lauplnstr[2, 1] = 'Qty1'
lauplnstr[2, 2] = 'N'
lauplnstr[2, 3] = 6
lauplnstr[2, 4] = 0
lauplnstr[3, 1] = 'Qty2'
lauplnstr[3, 2] = 'N'
lauplnstr[3, 3] = 6
lauplnstr[3, 4] = 0
lauplnstr[4, 1] = 'Qty3'
lauplnstr[4, 2] = 'N'
lauplnstr[4, 3] = 6
lauplnstr[4, 4] = 0
lauplnstr[5, 1] = 'Qty4'
lauplnstr[5, 2] = 'N'
lauplnstr[5, 3] = 6
lauplnstr[5, 4] = 0
lauplnstr[6, 1] = 'Qty5'
lauplnstr[6, 2] = 'N'
lauplnstr[6, 3] = 6
lauplnstr[6, 4] = 0
lauplnstr[7, 1] = 'Qty6'
lauplnstr[7, 2] = 'N'
lauplnstr[7, 3] = 6
lauplnstr[7, 4] = 0
lauplnstr[8, 1] = 'Qty7'
lauplnstr[8, 2] = 'N'
lauplnstr[8, 3] = 6
lauplnstr[8, 4] = 0
lauplnstr[9, 1] = 'Qty8'
lauplnstr[9, 2] = 'N'
lauplnstr[9, 3] = 6
lauplnstr[9, 4] = 0
lauplnstr[10, 1] = 'TotQty'
lauplnstr[10, 2] = 'N'
lauplnstr[10, 3] = 7
lauplnstr[10, 4] = 0
lauplnstr[11, 1] = 'CartNo'
lauplnstr[11, 2] = 'N'
lauplnstr[11, 3] = 6
lauplnstr[11, 4] = 0
lauplnstr[12, 1] = 'nOrdLineNo'
lauplnstr[12, 2] = 'N'
lauplnstr[12, 3] = 6
lauplnstr[12, 4] = 0
lauplnstr[13, 1] = 'Store'
lauplnstr[13, 2] = 'C'
lauplnstr[13, 3] = 8
lauplnstr[13, 4] = 0
lauplnstr[14, 1] = 'PACK_ID'
lauplnstr[14, 2] = 'C'
lauplnstr[14, 3] = 16
lauplnstr[14, 4] = 0
lauplnstr[15, 1] = 'CPKCOLOR'
lauplnstr[15, 2] = 'C'
lauplnstr[15, 3] = 6
lauplnstr[15, 4] = 0
lauplnstr[16, 1] = 'CPCKSIZE'
lauplnstr[16, 2] = 'C'
lauplnstr[16, 3] = 3
lauplnstr[16, 4] = 0
lauplnstr[17, 1] = 'CPKVERSION'
lauplnstr[17, 2] = 'C'
lauplnstr[17, 3] = 4
lauplnstr[17, 4] = 0
lauplnstr[18, 1] = 'cNoSize'
lauplnstr[18, 2] = 'C'
lauplnstr[18, 3] = 1
lauplnstr[18, 4] = 0
lauplnstr[19, 1] = 'PIKTKT'
lauplnstr[19, 2] = 'C'
lauplnstr[19, 3] = 6
lauplnstr[19, 4] = 0
= gfcrttmp(loformset.lctmpupln, @lauplnstr)
SELECT (lcctndtl)
GOTO BOTTOM
lnlastcrt = &lcctndtl..cart_no
SELECT tmpl_lin
= gfsetorder('TMPL_LINS')
lncartno = 0
lcorder = loformset.ariaform1.kborderno.keytextbox.value
SELECT &lcpcklin
SET FILTER TO
SET FILTER TO STORE = &lcstores..STORE AND lrange AND piktkt  = &lcstores..piktkt 
GOTO TOP
lnremqty = 0
lcpack_id = ""
lnpckedqty = 0
SCAN FOR uototqty > 0
 IF lcpack_id <> pack_id
  IF !EMPTY(lcpack_id)
   lncartno = 0
  ENDIF
  lcpack_id = pack_id
  IF gfseek(lcorder + pack_id + cpkcolor + cpcksize + cpkversion, 'TMPL_LIN')
   PRIVATE lnsz
   FOR lnsz = 1 TO 8
    IF EVALUATE('TMPL_LIN.QTY' + STR(lnsz, 1)) > 0
     lnpckedqty = EVALUATE('TMPL_LIN.QTY' + STR(lnsz, 1))
     EXIT
    ENDIF
   ENDFOR
   lnremqty = 0
  ENDIF
 ENDIF
 lni = 1
 DIMENSION laqty[8]
 STORE 0 TO laqty
 = gfseek(style, 'STYLE') .AND. gfseek('S' + style.scale, 'SCALE')
 lnreqqty = IIF(lnremqty = 0, lnpckedqty, lnremqty)
 lnstillreq = lnreqqty
 DO WHILE lni<=scale.cnt .AND. uototqty>0
  lci = STR(lni, 1)
  laqty[lni] = MIN(upoqty&lci,lnreqqty)   
  lnreqqty = MAX(lnreqqty - laqty(lni), 0)
  lnstillreq = lnreqqty
  REPLACE uototqty   WITH uototqty   - laqty[lni], upoqty&lci WITH upoqty&lci - laqty[lni]
  IF lnreqqty = 0
   lncartno = lncartno + 1
   INSERT INTO (lctmpupln) (STYLE,nordlineno,STORE,pack_id,cpkcolor,cpcksize,cpkversion,qty1,qty2,qty3,qty4,qty5,qty6,qty7,qty8,cartno,totqty,cnosize,piktkt) VALUES  (&lcpcklin..STYLE,&lcpcklin..nordlineno,&lcpcklin..STORE,&lcpcklin..pack_id,&lcpcklin..cpkcolor, &lcpcklin..cpcksize,&lcpcklin..cpkversion,laqty[1],laqty[2],laqty[3], laqty[4],laqty[5],laqty[6],laqty[7],laqty[8],lncartno,laqty[1]+laqty[2]+laqty[3]+ laqty[4]+laqty[5]+laqty[6]+laqty[7]+laqty[8],lci,&lcpcklin..piktkt)
   lnreqqty = lnpckedqty
   lnremqty = 0
   STORE 0 TO laqty
   IF upoqty&lci = 0
    lni = lni + 1
   ENDIF
  ELSE
   lni = lni + 1
  ENDIF
 ENDDO
 IF lnstillreq <> 0
  lncartno = lncartno + 1
  INSERT INTO (lctmpupln) (STYLE,nordlineno,STORE,pack_id,cpkcolor,cpcksize,cpkversion,qty1,qty2,qty3,qty4,qty5,qty6,qty7,qty8,cartno,totqty,piktkt) VALUES  (&lcpcklin..STYLE,&lcpcklin..nordlineno,&lcpcklin..STORE,&lcpcklin..pack_id,&lcpcklin..cpkcolor, &lcpcklin..cpcksize,&lcpcklin..cpkversion,laqty[1],laqty[2],laqty[3], laqty[4],laqty[5],laqty[6],laqty[7],laqty[8],lncartno,laqty[1]+laqty[2]+laqty[3]+ laqty[4]+laqty[5]+laqty[6]+laqty[7]+laqty[8],&lcpcklin..piktkt)
  lncartno = lncartno - 1
  lnremqty = lnstillreq
 ENDIF
ENDSCAN
= lfupdctn(loformset)
SELECT tmpl_lin
= gfsetorder('TMPL_LIN')
ENDPROC
**
PROCEDURE lfUpdCtn
PARAMETER loformset
lctmpupln = loformset.lctmpupln
lcpcklin = loformset.lcpcklin
lcctndtl = loformset.lcctndtl
lcstores = loformset.lcstores
lcctnhdr = loformset.lcctnhdr
lcorder = loformset.ariaform1.kborderno.keytextbox.value
SELECT (lctmpupln)
SCAN
 lccurrsty = pack_id + cpkcolor + cpcksize + cpkversion + style
 = SEEK(lccurrsty, lcpcklin)
 SELECT &lcpcklin
 REPLACE pqty1 WITH oqty1, pqty2 WITH oqty2, pqty3 WITH oqty3, pqty4 WITH oqty4, pqty5 WITH oqty5, pqty6 WITH oqty6, pqty7 WITH oqty7, pqty8 WITH oqty8, ptotqty WITH pqty1 + pqty2 + pqty3 + pqty4 + pqty5 + pqty6 + pqty7 + pqty8, pwgh1 WITH pqty1 * weight1, pwgh2 WITH pqty2 * weight2, pwgh3 WITH pqty3 * weight3, pwgh4 WITH pqty4 * weight4, pwgh5 WITH pqty5 * weight5, pwgh6 WITH pqty6 * weight6, pwgh7 WITH pqty7 * weight7, pwgh8 WITH pqty8 * weight8, ptotwgh WITH pwgh1 + pwgh2 + pwgh3 + pwgh4 + pwgh5 + pwgh6 + pwgh7 + pwgh8
 =gfseek(&lctmpupln..STYLE,'STYLE') AND gfseek('S'+STYLE.SCALE,'SCALE')
 = gfseek(lcorder + pack_id + cpkcolor + cpcksize + cpkversion, 'TMPL_LIN')
 = SEEK(tmpl_lin.ncarton, lccurs)
 lnlastcrt = &lccurs..nfrom
 SELECT (lcctndtl)
 APPEND BLANK
 REPLACE piktkt WITH &lctmpupln..piktkt    
 REPLACE cnosize WITH   &lctmpupln..cnosize
 REPLACE cart_no    WITH &lctmpupln..cartno+lnlastcrt , STORE      WITH &lctmpupln..STORE, nordlineno WITH &lctmpupln..nordlineno, STYLE      WITH &lctmpupln..STYLE, szcnt      WITH SCALE.CNT, size1      WITH SCALE.sz1, size2      WITH SCALE.sz2, size3      WITH SCALE.sz3, size4      WITH SCALE.sz4, size5      WITH SCALE.sz5, size6      WITH SCALE.sz6, size7      WITH SCALE.sz7, size8      WITH SCALE.sz8
 REPLACE qty1       WITH &lctmpupln..qty1, qty2       WITH &lctmpupln..qty2, qty3       WITH &lctmpupln..qty3, qty4       WITH &lctmpupln..qty4, qty5       WITH &lctmpupln..qty5, qty6       WITH &lctmpupln..qty6, qty7       WITH &lctmpupln..qty7, qty8       WITH &lctmpupln..qty8, totqty     WITH qty1+qty2+qty3+qty4+qty5+qty6+qty7+qty8
 REPLACE weight1    WITH IIF(qty1 > 0 , qty1*&lcpcklin..weight1 , 0 ), weight2    WITH IIF(qty2 > 0 , qty2*&lcpcklin..weight2 , 0 ), weight3    WITH IIF(qty3 > 0 , qty3*&lcpcklin..weight3 , 0 ), weight4    WITH IIF(qty4 > 0 , qty4*&lcpcklin..weight4 , 0 ), weight5    WITH IIF(qty5 > 0 , qty5*&lcpcklin..weight5 , 0 ), weight6    WITH IIF(qty6 > 0 , qty6*&lcpcklin..weight6 , 0 ), weight7    WITH IIF(qty7 > 0 , qty7*&lcpcklin..weight7 , 0 ), weight8    WITH IIF(qty8 > 0 , qty8*&lcpcklin..weight8 , 0 ), totweight  WITH weight1+weight2+weight3+weight4+weight5+weight6+weight7+weight8
 REPLACE br1 WITH !EMPTY(qty1), br2 WITH !EMPTY(qty2), br3 WITH !EMPTY(qty3), br4 WITH !EMPTY(qty4), br5 WITH !EMPTY(qty5), br6 WITH !EMPTY(qty6), br7 WITH !EMPTY(qty7), br8 WITH !EMPTY(qty8)
 REPLACE pack_id    WITH &lctmpupln..pack_id, cpkcolor   WITH &lctmpupln..cpkcolor, cpcksize   WITH &lctmpupln..cpcksize, cpkversion WITH &lctmpupln..cpkversion, npackno    WITH 0  
 IF !SEEK(&lcstores..STORE+&lcstores..piktkt+STR(&lctmpupln..cartno+lnlastcrt,4),lcctnhdr)
  INSERT INTO (lcctnhdr) (cart_no,pal_no,EMPTY,STORE,piktkt) VALUES (&lctmpupln..cartno+lnlastcrt,0,'N',&lcstores..STORE,&lcstores..piktkt)          
  lncrtsum = lncrtsum + 1
 ENDIF
 REPLACE &lcctnhdr..totpcs WITH &lcctnhdr..totpcs + &lcctndtl..totqty, &lcctnhdr..totwgh WITH &lcctnhdr..totwgh + &lcctndtl..totweight
 loformset.lnpackwgh = loformset.lnpackwgh + &lcctndtl..totweight
 SELECT &lcstores
 REPLACE totpqty WITH totpqty + &lcctndtl..totqty , weight  WITH weight  + &lcctndtl..totweight
ENDSCAN
ENDPROC
**
FUNCTION lfSavScrPack
PARAMETER loformset
lcorderno = loformset.ariaform1.kborderno.keytextbox.value
lcstores = loformset.lcstores
= gfseek('O' + lcorderno, 'Ordhdr', 'Ordhdr')
PRIVATE lapcks, lni
IF ordhdr.status = 'C'
 = gfmodalgen('INM00000B00000', .F., .F., .F., 'Order is invoiced, can not save!')
 RETURN .F.
ENDIF
SELECT &lcstores
GOTO TOP
lni = 0
SCAN FOR pack_no = '######'
 STORE 0 TO m.tot_wght, m.tot_cart, m.tot_pcs
 STORE .F. TO m.lstandctn
 STORE '' TO m.ctostorcn
 WAIT WINDOW NOWAIT 'Generating P/L for store '+&lcstores..STORE + ' D.C '+ &lcstores..dist_ctr
 lcpacknum = &lcstores..pack_no
 DO lfsavepack WITH loformset
 IF !EMPTY(lcpacknum)
  lni = lni + 1
  DIMENSION lapcks[lni]
  lapcks[lni] = lcpacknum
 ENDIF
ENDSCAN
WAIT WINDOW NOWAIT ''
IF lni > 0
 = gfmodalgen('INM00000B00000', .F., .F., .F., 'Packing lists from ' + lapcks(1) + ' to ' + lapcks(lni) + ' are generated.')
 RETURN .T.
ELSE
 = gfmodalgen('INM00000B00000', .F., .F., .F., 'No packing lists generated, cannot save.')
 RETURN .F.
ENDIF
ENDFUNC
**
FUNCTION lfSavePack
PARAMETER loformset
lcorderno = loformset.ariaform1.kborderno.keytextbox.value
lcaccount = loformset.ariaform1.kbaccount.keytextbox.value
lcstores = loformset.lcstores
lcctnhdr = loformset.lcctnhdr
lcctndtl = loformset.lcctndtl
PRIVATE llreturn, lncuralias, lcpckhdtag, lncount, lnbookqty, lnbookamt, lnopenqty, lnopenamt, lncount, lni, llstydyopn, lncurrecno, lcsvord
lcpcklin = loformset.lcpcklin
llstydyopn = .F.
STORE 0 TO lnbookqty, lnbookamt, lnrelcho, lnopenqty, lnopenamt
lncount = 0
lncuralias = SELECT(0)
lcdelstat = SET("DELETED")
PRIVATE lnbookdiff, lnqtydiff, lnbkamtdif, lnqyamtdif, lnorgbook, lnorgqty
STORE 0 TO lnbookdiff, lnqtydiff, lnbkamtdif, lnqyamtdif, lnorgbook, lnorgqty
SELECT pack_lin
= gfsetorder('PackStyle')
SELECT (lcpcklin)
SET RELATION TO
SET FILTER TO STORE =&lcstores..STORE AND piktkt =&lcstores..piktkt 
GOTO TOP
SELECT (lcctndtl)
lcctdtrel = SET("RELATION")
SET RELATION TO
SET FILTER TO STORE = &lcstores..STORE AND piktkt =&lcstores..piktkt 
GOTO TOP
SELECT (lcctndtl)
SET FILTER TO STORE = &lcstores..STORE
GOTO TOP
IF EOF() .OR. BOF() .OR. DELETED()
 llnothing = gfmodalgen("INM44035B00000", "Dialog")
 RETURN .F.
 llreturn = .F.
ELSE
 IF loformset.llupdtpktk
  = gfopentable(oariaapplication.datadir + "StyDye", oariaapplication.datadir + 'StyDye')
  IF lfnosuffic(loformset)
   RETURN
  ENDIF
 ENDIF
 IF !EMPTY(&lcstores..piktkt)
  SELECT (lcpcklin)
  lctag = ORDER(lcpcklin)
  SET ORDER TO NoPacked
  IF SEEK('Y', lcpcklin)
   IF lnrelcho = 0
    lnrelcho = gfmodalgen("QRM44039B44006", "Dialog")
   ENDIF
  ENDIF
  SET ORDER IN (lcpcklin) TO lcTag
 ENDIF
 IF lnrelcho <> 2
  SELECT (lcpcklin)
  SCAN
   llnothing  = gfseek('O'+lcorderno+&lcstores..STORE+&lcpcklin..STYLE+STR(nordlineno,6),'OrdLine')
   IF llnothing AND &lcpcklin..nstep = 0      
    IF !loformset.llcomp
     SELECT ordline
     lcsz =&lcpcklin..cnosize                     
     REPLACE npck&lcsz.   WITH &lcpcklin..pqty&lcsz., npwght       WITH npwght + &lcpcklin..pwgh&lcsz.                             
     IF !EMPTY(&lcpcklin..pack_id)            
      lcsvord = ORDER('SPCK_LIN')
      SELECT spck_lin
      = gfsetorder('SPCK_LINVR')
      SELECT ordline
      IF gfseek('P' + ordline.account + ordline.pack_id + ordline.cpkcolor + ordline.cpcksize + ordline.cpkversion + ordline.style, 'SPCK_LIN') .OR. gfseek('P' + '*****' + ordline.pack_id + ordline.cpkcolor + ordline.cpcksize + ordline.cpkversion + ordline.style, 'SPCK_LIN')
       REPLACE ordline.npkpack WITH (npck1 + npck2 + npck3 + npck4 + npck5 + npck6 + npck7 + npck8) / spck_lin.totqty
      ENDIF
      SELECT spck_lin
      = gfsetorder(lcsvord)
      SELECT ordline
      = gfreplace("")
     ENDIF
    ENDIF
    IF loformset.llupdtpktk
     lcsz =&lcpcklin..cnosize   
     SELECT (lcpcklin)
     REPLACE ndiff&lcsz. WITH pqty&lcsz.- ordline.pik&lcsz. 
     IF !loformset.llcomp
      SELECT ordline
      REPLACE pik1 WITH npck1, pik2 WITH npck2, pik3 WITH npck3, pik4 WITH npck4, pik5 WITH npck5, pik6 WITH npck6, pik7 WITH npck7, pik8 WITH npck8, totpik WITH pik1 + pik2 + pik3 + pik4 + pik5 + pik6 + pik7 + pik8
     ENDIF
    ENDIF
    IF !EMPTY(&lcstores..piktkt)
     IF lnrelcho = 1 OR  (lnrelcho = 3  AND  !( EMPTY(&lcpcklin..cselect1) OR EMPTY(&lcpcklin..cselect2) OR  EMPTY(&lcpcklin..cselect3) OR EMPTY(&lcpcklin..cselect4) OR  EMPTY(&lcpcklin..cselect5) OR EMPTY(&lcpcklin..cselect6) OR  EMPTY(&lcpcklin..cselect7) OR EMPTY(&lcpcklin..cselect8) ) )
      IF !loformset.llcomp
       lcsz =&lcpcklin..cnosize   
       REPLACE pik&lcsz.   WITH &lcpcklin..pqty&lcsz., totpik WITH totpik+ &lcpcklin..pqty&lcsz., piktkt  WITH IIF(totpik=0,'',piktkt), pikdate WITH IIF(totpik=0,{},pikdate), picked  WITH IIF(totpik=0,.F.,picked)              
      ENDIF
     ENDIF
    ENDIF
    SELECT (lcpcklin)
    REPLACE &lcpcklin..nstep WITH 1
   ENDIF
   IF &lcpcklin..nstep = 1      
    IF !loformset.llcomp
     SELECT style
     lcsz =&lcpcklin..cnosize   
     IF loformset.llupdtpktk AND !EMPTY(&lcstores..piktkt) AND gfseek(&lcpcklin..STYLE,'Style')
      REPLACE alo&lcsz.   WITH alo&lcsz. + &lcpcklin..ndiff&lcsz., totalo WITH alo1+alo2+alo3+alo4+alo5+alo6+alo7+alo8
     ENDIF
     REPLACE ord&lcsz.   WITH IIF(&lcpcklin..pqty&lcsz.>ordline.qty&lcsz.,ord&lcsz.+(&lcpcklin..pqty&lcsz.-ordline.qty&lcsz.),ord&lcsz.), totord WITH ord1+ord2+ord3+ord4+ord5+ord6+ord7+ord8
    ENDIF
    = gfreplace("")
    SELECT (lcpcklin)
    REPLACE &lcpcklin..nstep WITH 2
   ENDIF
   IF !loformset.llcomp
    llstydyopn = gfopentable(oariaapplication.datadir + "StyDye", oariaapplication.datadir + 'StyDye')
    lcsz =&lcpcklin..cnosize  
    IF &lcpcklin..nstep = 2      
     IF !EMPTY(&lcstores..piktkt)
      IF gfseek(ordline.style + loformset.lcwarecode, 'StyDye')
       SELECT stydye
       IF loformset.llupdtpktk
        REPLACE alo&lcsz. WITH alo&lcsz. + &lcpcklin..ndiff&lcsz., 
        REPLACE stydye.totalo WITH stydye.alo1 + stydye.alo2 + stydye.alo3 + stydye.alo4 + stydye.alo5 + stydye.alo6 + stydye.alo7 + stydye.alo8
       ENDIF
       REPLACE ord&lcsz. WITH ord&lcsz. + MAX(&lcpcklin..pqty&lcsz.-ordline.qty&lcsz.,0), totord WITH ord1+ord2+ord3+ord4+ord5+ord6+ord7+ord8                     
       = gfreplace("")
      ENDIF
     ENDIF
     SELECT (lcpcklin)
     REPLACE &lcpcklin..nstep WITH 3
    ENDIF
   ENDIF
   IF !EMPTY(&lcstores..piktkt)
    lcsz =&lcpcklin..cnosize  
    IF &lcpcklin..nstep = 3      
     IF loformset.lldyelot .AND. style.cdye_flg = 'Y' .AND. SEEK(ordline.style + loformset.lcwarecode + ordline.dyelot, 'StyDye')
      IF !loformset.llcomp
       SELECT stydye
       IF loformset.llupdtpktk
        REPLACE alo&lcsz. WITH alo&lcsz. + &lcpcklin..ndiff&lcsz.
        REPLACE stydye.totalo WITH stydye.alo1 + stydye.alo2 + stydye.alo3 + stydye.alo4 + stydye.alo5 + stydye.alo6 + stydye.alo7 + stydye.alo8
       ENDIF
       REPLACE ord&lcsz. WITH ord&lcsz. + MAX(&lcpcklin..pqty&lcsz.-ordline.qty&lcsz.,0), totord WITH ord1+ord2+ord3+ord4+ord5+ord6+ord7+ord8
       = gfreplace("")
      ENDIF
     ENDIF
     SELECT (lcpcklin)
     REPLACE &lcpcklin..nstep WITH 4
    ENDIF
   ENDIF
  ENDSCAN
 ENDIF
 IF USED("StyDye")
  SELECT "StyDye"
  = gftableupdate()
 ENDIF
 IF llstydyopn
  SELECT "StyDye"
  = gfclosetable("StyDye")
 ENDIF
 IF lnrelcho <> 2
  SELECT (lcctnhdr)
  lccthdind = ORDER(lcctnhdr)
  SET ORDER IN (lcctnhdr) TO EMPTY
  IF SEEK('Y', lcctnhdr)
   lnrecno = RECNO()
   SKIP -1
   lnlastctn = &lcctnhdr..cart_no
   GOTO lnrecno
   SCAN REST FOR empty = 'Y'
    lnpackctn = lnpackctn - 1
   ENDSCAN
  ELSE
   GOTO BOTTOM
   lnlastctn = &lcctnhdr..cart_no
  ENDIF
  SET ORDER IN (lcctnhdr) TO lcCtHdInd
  SELECT pack_hdr
  IF !gfseek(lcpacknum, 'Pack_Hdr')
   IF EMPTY(&lcstores..piktkt)
    = gfseek('O' + lcorderno, 'ORDHDR')
    lcpacknum = gfsequence('PIKTKT', '', '', ordhdr.cdivision)
    DO WHILE gfseek(lcpacknum, 'PACK_HDR')
     lcpacknum = gfsequence('PIKTKT', '', '', ordhdr.cdivision)
    ENDDO
    INSERT INTO PACK_HDR (pack_no) VALUES (lcpacknum)
    SELECT pack_hdr
    = gfreplace("")
    REPLACE &lcstores..pack_no WITH lcpacknum
   ELSE
    lcpacknum = &lcstores..piktkt
    INSERT INTO PACK_HDR (pack_no) VALUES (lcpacknum)
    SELECT pack_hdr
    REPLACE account WITH lcaccount, ORDER WITH lcorderno, STORE WITH &lcstores..STORE, shipvia WITH ordhdr.shipvia
    = gfreplace("")
   ENDIF
   SELECT pack_hdr
  ENDIF
  lnlastno = pack_hdr.nlastlno
  SELECT &lcctnhdr
  SCAN
   m.tot_wght = m.tot_wght + totwgh
   m.tot_cart = m.tot_cart + 1
   m.tot_pcs = m.tot_pcs + totpcs
  ENDSCAN
  m.account = lcaccount
  m.order = lcorderno
  m.STORE = &lcstores..STORE
  m.shipvia = ordhdr.shipvia
  m.lstandctn = IIF(loformset.lnctntyp = 1, .T., .F.)
  m.ctostorcn = IIF(loformset.lndrctto = 1, 'S', 'C')
  SELECT pack_hdr
  GATHER MEMO MEMVAR
  REPLACE cwarecode WITH loformset.lcwarecode, bill_ladg WITH ""
  = gfadd_info('Pack_Hdr')
  = gfreplace("")
 ENDIF
 IF lnrelcho <> 2
  lcsetdele = SET('DELETE')
  SET DELETED ON
  SELECT edicrtsq
  = gfsetorder('PCKCRTSQ')
  IF gfseek(lcpacknum, 'EDICRTSQ')
   SELECT edicrtsq
   SCAN REST WHILE pack_no + STR(cart_no, 6) = lcpacknum
    gfdelete()
   ENDSCAN
  ENDIF
  SET DELETE &lcsetdele 
  SET DELETED OFF
  SELECT (lcctndtl)
  SET FILTER TO STORE = &lcstores..STORE AND piktkt =&lcstores..piktkt
  SET ORDER TO 0
  = gfseek('O' + lcorderno, 'OrdHdr')
  SCAN FOR cstatus <> 'S'
   IF &lcctndtl..nstep = 0   
    SELECT pack_lin
    IF gfseek(lcpacknum+STR(&lcctndtl..cart_no,4)+&lcctndtl..STYLE,'Pack_Lin')
     IF DELETED('Pack_Lin')
      LOCATE REST FOR pack_no+STR(no_cart,4)+STYLE =  lcpacknum+STR(&lcctndtl..cart_no,4)+ &lcctndtl..STYLE AND !DELETED()
     ENDIF
    ENDIF
    IF !FOUND('Pack_Lin') .AND. !DELETED(lcctndtl)
     lnlastno = lnlastno + 1
     APPEND BLANK
     REPLACE line_no WITH lnlastno
     = gfreplace("")
     SELECT (lcctndtl)
     REPLACE packlineno WITH lnlastno
    ENDIF
    lcsz =&lcctndtl..cnosize 
    llnothing = SEEK(STR(&lcctndtl..cart_no,4),lcctnhdr)
    SELECT pack_lin
    REPLACE pack_no    WITH lcpacknum, no_cart    WITH &lcctndtl..cart_no, npltno     WITH &lcctnhdr..pal_no, STYLE      WITH &lcctndtl..STYLE, nordlineno WITH &lcctndtl..nordlineno, qty&lcsz.  WITH IIF(!DELETED(lcctndtl),&lcctndtl..qty&lcsz.,0), totqty     WITH qty1+qty2+qty3+qty4+qty5+qty6+qty7+qty8
    REPLACE pack_id    WITH &lcctndtl..pack_id, cpkcolor   WITH &lcctndtl..cpkcolor, cpcksize   WITH &lcctndtl..cpcksize, cpkversion WITH &lcctndtl..cpkversion, npackno    WITH &lcctndtl..npackno    
    REPLACE weight     WITH weight + IIF(!DELETED(lcctndtl), &lcctndtl..weight&lcsz.,MAX(weight-&lcctndtl..weight&lcsz.+(&lcctndtl..qty&lcsz.*&lcctndtl..orgwgh),0))
    = gfadd_info('Pack_Lin')
    = gfreplace("")
    IF pack_lin.totqty = 0
     gfdelete()
    ENDIF
    SELECT (lcctndtl)
    REPLACE &lcctndtl..nstep WITH 1
   ENDIF
   IF lnrelcho <> 2
    IF !loformset.llcomp
     SELECT ordline
     SET ORDER IN (lcpcklin) TO lcPckLin
     =SEEK(&lcctndtl..STYLE+STR(&lcctndtl..nordlineno,6),lcpcklin)
     =gfseek('O'+lcorderno +&lcstores..STORE+&lcctndtl..STYLE+STR(&lcctndtl..nordlineno,6),'OrdLine')
     lnorgbook = totbook
     lnorgqty = totqty
     lcsz =&lcctndtl..cnosize 
     REPLACE book&lcsz.    WITH book&lcsz. +MAX(&lcpcklin..ordqty&lcsz. -ordline.qty&lcsz. ,0), totbook WITH book1+book2+book3+book4+book5+book6+book7+book8
     lnbookqty = lnbookqty + totbook
     lnbookamt = lnbookamt + (totbook * price)
     REPLACE qty&lcsz.    WITH &lcpcklin..ordqty&lcsz., totqty  WITH qty1+qty2+qty3+qty4+qty5+qty6+qty7+qty8
     lnbookdiff = lnbookdiff + (totbook - lnorgbook)
     lnbkamtdif = lnbkamtdif + ((totbook - lnorgbook) * price)
     lnqtydiff = lnqtydiff + (totqty - lnorgqty)
     lnqyamtdif = lnqyamtdif + ((totqty - lnorgqty) * price)
    ENDIF
   ENDIF
  ENDSCAN
  SELECT pack_lin
  lcsetdel = SET('DELETE')
  SET DELETED ON
  = gfseek(lcpacknum)
  lni = 0
  llfirst = .T.
  lnlastcrt = pack_lin.no_cart
  llsamecrt = .F.
  SCAN REST WHILE pack_no + STR(no_cart, 4) + style = lcpacknum
   IF !llfirst
    llsamecrt = (pack_lin.no_cart = lnlastcrt)
    IF !llsamecrt
     lnlastcrt = pack_lin.no_cart
    ENDIF
   ENDIF
   IF llfirst .OR. !llsamecrt
    lni = lni + 1
   ENDIF
   REPLACE pack_lin.no_cart WITH lni
   = gfreplace("")
   llfirst = .F.
  ENDSCAN
  SELECT pack_lin
  = gfseek(lcpacknum)
  SCAN REST WHILE pack_no + STR(no_cart, 4) + style = lcpacknum
   SELECT edicrtsq
   = gfsetorder('PCKCRTSQ')
   SELECT pack_lin
   IF !gfseek(lcpacknum + STR(pack_lin.no_cart, 6), 'EDICRTSQ')
    IF gfseek('A' + lcaccount, 'EDIACPRT')
     lnucc9 = IIF(EMPTY(ediacprt.ucc9), 0, EVALUATE(ediacprt.ucc9)) + 1
     SELECT edicrtsq
     = gfsetorder("EDICRTSQ")
     llfound = gfseek(lcaccount + PADL(lnucc9, 9, '0'), 'EDICRTSQ')
     DO WHILE llfound
      lnucc9 = lnucc9 + 1
      llfound = gfseek(lcaccount + PADL(lnucc9, 9, '0'), 'EDICRTSQ')
     ENDDO
     INSERT INTO EDICRTSQ (pack_no, account, cart_no, ucc9) VALUES (lcpacknum, lcaccount, pack_lin.no_cart, PADL(lnucc9, 9, '0'))
     REPLACE ediacprt.ucc9 WITH PADL(lnucc9, 9, '0')
     = gfreplace("")
    ELSE
     SELECT edicrtsq
     *B611917,1 MMT 2021/08/12 Custom Automatic PL takes time to save in case of Non-EDI Account PL[T20210809.0001][Start]
     SET ORDER TO EDICRTSQ  DESCENDING  && ACCOUNT+UCC9 
*!*	     = gfseek(lcaccount)
*!*	     SELECT MAX(edicrtsq.ucc9) FROM EDICRTSQ WHERE edicrtsq.account = lcaccount INTO CURSOR lcMaxUcc
*!*	     SELECT lcmaxucc
*!*	     LOCATE
     *IF EOF()
     IF !gfseek(lcaccount)
     *B611917,1 MMT 2021/08/12 Custom Automatic PL takes time to save in case of Non-EDI Account PL[T20210809.0001][End]     
      lcucc9 = '000000001'
     ELSE
      *B611917,1 MMT 2021/08/12 Custom Automatic PL takes time to save in case of Non-EDI Account PL[T20210809.0001][Start]
      *lcucc9 = PADL(EVALUATE(lcmaxucc.max_ucc9) + 1, 9, '0')
      lcucc9 = PADL(EVALUATE(edicrtsq.ucc9) + 1, 9, '0')
      *B611917,1 MMT 2021/08/12 Custom Automatic PL takes time to save in case of Non-EDI Account PL[T20210809.0001][End]
     ENDIF
     INSERT INTO EDICRTSQ (pack_no, account, cart_no, ucc9) VALUES (lcpacknum, lcaccount, pack_lin.no_cart, lcucc9)
     = gfreplace("")
    ENDIF
   ENDIF
  ENDSCAN
  lnlastctn = lni
  SELECT (lcctndtl)
  GOTO TOP
  lni = 0
  llfirst = .T.
  lnlastcrt = &lcctndtl..cart_no
  llsamecrt = .F.
  SCAN
   IF !llfirst
    llsamecrt = (&lcctndtl..cart_no = lnlastcrt)
    IF !llsamecrt
     lnlastcrt = &lcctndtl..cart_no
    ENDIF
   ENDIF
   IF llfirst .OR. !llsamecrt
    lni = lni + 1
   ENDIF
   REPLACE &lcctndtl..cart_no WITH lni
   llfirst = .F.
  ENDSCAN
  SET DELETE &lcsetdel
  SELECT pack_hdr
  REPLACE nlastlno WITH MAX(nlastlno, lnlastno), nlastcart WITH lnlastctn
  = gfreplace('')
 ENDIF
 IF lnrelcho <> 2
  IF !(lnbookdiff = 0 .AND. lnqtydiff = 0 .AND. lnbkamtdif = 0 .AND. lnqyamtdif = 0) .AND. gfseek('O' + pack_hdr.order, 'OrdHdr')
   IF !loformset.llcomp
    SELECT ordhdr
    = RLOCK()
    REPLACE book WITH book + lnbookdiff, bookamt WITH bookamt + lnbkamtdif, open WITH open + lnqtydiff, openamt WITH openamt + lnqyamtdif
    UNLOCK
    = gfadd_info("OrdHdr")
    = gfreplace("")
   ENDIF
  ENDIF
 ENDIF
ENDIF
SET DELETED &lcdelstat
IF lnrelcho <> 2
 llreturn = .T.
ELSE
 llreturn = .F.
ENDIF
SELECT (lcctndtl)
SET RELATION TO &lcctdtrel.
llcsave = llreturn
SELECT pack_lin
= gftableupdate()
SELECT pack_hdr
= gftableupdate()
SELECT ordhdr
= gftableupdate()
SELECT ordline
= gftableupdate()
SELECT style
= gftableupdate()
IF USED('EDICRTSQ')
 SELECT edicrtsq
 = gftableupdate()
ENDIF
= lfordhdlck(.F., loformset)
SELECT (lncuralias)
RETURN llreturn
ENDFUNC
**
FUNCTION lfNoSuffic
PARAMETER loformset
lcpcklin = loformset.lcpcklin
lcstores = loformset.lcstores
lcorderno = loformset.ariaform1.kborderno.keytextbox.value
PRIVATE lcmessage, lni, lci, lnchoice, llexitloop, llret2main
STORE .F. TO llexitloop, llret2main
SELECT (lcpcklin)
SCAN
 llnothing = gfseek('O'+lcorderno +&lcstores..STORE+&lcpcklin..STYLE+STR(nordlineno,6),'OrdLine')
 IF llnothing
  lni = 0
  FOR lni = 1 TO 8
   lci = STR(lni, 1)
   IF (pqty&lci > ordline.pik&lci) AND  gfseek(ordline.STYLE+ordline.cwarecode+ordline.dyelot,'StyDye') AND  pqty&lci > (stydye.stk&lci - stydye.alo&lci)
    lcmessage = "Order : " + lcorderno + ", Style : " + ordline.style + ", does not have available quantity."
    IF loformset.llalwforce
     lcmessage = lcmessage + "Do you want to force allocation?"
     lnchoice = gfmodalgen("INM00000B44002", "Dialog", "", "", lcmessage)
     DO CASE
      CASE lnchoice = 1
      CASE lnchoice = 2
       llexitloop = .T.
      CASE lnchoice = 3
       STORE .T. TO llexitloop, llret2main
     ENDCASE
    ELSE
     lcmessage = lcmessage + "Save without update pick Quantity?"
     lnchoice = gfmodalgen("INM00000B44009", "Dialog", "", "", lcmessage)
     IF lnchoice = 1
      loformset.llupdtpktk = .F.
      llexitloop = .T.
     ELSE
      STORE .T. TO llexitloop, llret2main
     ENDIF
    ENDIF
    IF llexitloop
     EXIT
    ENDIF
   ENDIF
  ENDFOR
  IF llexitloop
   EXIT
  ENDIF
 ENDIF
ENDSCAN
RETURN llret2main
ENDFUNC
**
FUNCTION lfSuppForc
PARAMETER loformset
PRIVATE llalwforce
llalwforce = .T.
IF gfgetmemvar('M_FORCEALO', oariaapplication.activecompanyid) <> "Y"
 IF gfgetmemvar('M_FORCEALO', oariaapplication.activecompanyid) = "N"
  llalwforce = .F.
 ELSE
  llalwforce = gfuserpriv('AL', 'ALAUTAL', 'FORCING')
 ENDIF
ENDIF
RETURN llalwforce
ENDFUNC
**
FUNCTION lfChkOrdLok
LPARAMETERS loformset
PRIVATE llgoon, lncuralias, lncurrecno, lcorderno, lcstore, lcoldord
llgoon = .T.
lcorderno = loformset.ariaform1.kborderno.keytextbox.value
lncuralias = SELECT(0)
lcoldhord = ORDER('ORDHDR')
SELECT ordhdr
gfsetorder('ORDHDR')
IF gfseek('O' + lcorderno)
 SELECT ordhdr
 llgoon = gfobj_lock(.T.)
ENDIF
gfsetorder(lcoldhord)
SELECT (lncuralias)
RETURN llgoon
ENDFUNC
**
FUNCTION lfOrdHdLck
PARAMETER lllock, loformset
lcorder = loformset.ariaform1.kborderno.keytextbox.value
PRIVATE lnalias, lcsvord, llgoon
llgoon = .T.
lnalias = SELECT()
lcsvord = ORDER('ORDHDR')
SELECT ordhdr
= gfsetorder('ORDHDR')
= gfseek('O' + lcorder, 'ORDHDR')
= gfobj_lock(lllock)
= gfsetorder(lcsvord)
SELECT (lnalias)
RETURN llgoon
ENDFUNC
**
*** 
*** ReFox - all is not lost 
***
