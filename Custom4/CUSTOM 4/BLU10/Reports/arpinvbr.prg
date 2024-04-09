****************************************************************************
*: Program file : ARPINVZ.PRG    
*: DESC : PRINT INVOICES - 66 LINE PAGE, 8 1/2" x 11" (For Blue Berry)
*: A copy from invoice form D with some changes.   
*: System   : Aria4XP
*: Module   : Accounts Receivable
*: DATE     : 02/07/2007
*: Developer: Mariam Mazhar[MMT] C200744 - T20070108.0123
*:************************************************************************
*: Calls : 
*:         Functions  : gfGetMemVar()
*:                    : gfGetZone()
*:************************************************************************
*:Modifications :
*: B608185,1 WLD 07/30/2007 Update PRINTED FLAG
*:************************************************************************
 lcrpprst = IIF(lcrpprst='N', SPACE(1), lcrpprst)
 SELECT invhdr
 LOCATE FOR &lcrpexp
 IF  .NOT. FOUND()
    WAIT WINDOW "No Records Selected"
    llnorec = .T.
    SET DEVICE TO SCREEN
    RETURN
 ENDIF
 lcinvlines = loogscroll.gftempname()
 = lfcreatetemp()
 WAIT WINDOW NOWAIT 'PRINTING INVOICES.....'
 xtax = IIF(gfgetmemvar("M_TAX", oariaapplication.activecompanyid)='Y', .T., .F.)
 lcxtax_desc = gfgetmemvar('M_TAX_DESC', oariaapplication.activecompanyid)
 xtax_rate = gfgetmemvar('M_TAX_RATE', oariaapplication.activecompanyid)
 xtax_meth = gfgetmemvar('M_TAX_METH', oariaapplication.activecompanyid)
 lctaxrefr = gfgetmemvar('M_TAX_REFE', oariaapplication.activecompanyid)
 llnote = llrpinvnot
 RELEASE ALL LIKE 'M_*'
 IF  .NOT. USED('CONSINVH')
    = gfopentable(oariaapplication.datadir+'CONSINVH', oariaapplication.datadir+'CONSINVH', 'SH')
 ENDIF
 SELECT invhdr
 SCAN FOR  &lcrpexp
    STORE '' TO m.invoice, m.account, m.store, m.custpo, m.pterms, m.pshipvia, m.pspcinst, m.phone, m.xtax_desc, m.ctaxrefr, m.hasnot, m.mnothd, m.note1, m.note2, m.xbtname, m.ccaddress1, m.ccaddress2, m.ccaddress3, m.xfname, m.xfaddr1, m.xfaddr2, m.xfaddr3, m.xstname, m.xsaddr1, m.xsaddr2, m.xsaddr3
    STORE 0 TO m.cntgrp, lncounter
    STORE {} TO m.invdate, m.shipdate
    STORE 0 TO m.tax_rate, m.tax_amt, m.npstrate, m.npstamt, m.fr_ins_cod, m.totalchg, m.discount
    m.invoice = invhdr.invoice
    m.account = invhdr.account
    m.store = invhdr.store
    m.invdate = invhdr.invdate
    m.custpo = invhdr.custpo
    m.pterms = gfcoddes(invhdr.ctermcode, 'CTERMCODE')
    m.pshipvia = gfcoddes(invhdr.shipvia, 'SHIPVIA')
    m.pspcinst = gfcoddes(invhdr.spcinst, 'SPCINST')
    m.rep1 = invhdr.rep1
    m.shipdate = invhdr.shipdate
    m.phone = lacompadd(5)
    m.note1 = invhdr.note1
    m.note2 = invhdr.note2
    IF xtax .AND. xtax_meth='M'
       xstring_rate = invhdr.tax_rate
       m.xtax_desc = lcxtax_desc
       IF  .NOT. EMPTY(lctaxrefr)
          m.ctaxrefr = lctaxrefr
       ENDIF
       m.tax_rate = xstring_rate
       m.tax_amt = invhdr.tax_amt
       IF invhdr.npstamt<>0
          m.npstrate = invhdr.npstrate
          m.npstamt = invhdr.npstamt
       ENDIF
    ENDIF
    IF xtax .AND. xtax_meth='A'
       xstring_rate = invhdr.tax_rate
       m.xtax_desc = lcxtax_desc
       IF  .NOT. EMPTY(lctaxrefr)
          m.ctaxrefr = lctaxrefr
       ENDIF
       m.tax_rate = xstring_rate
       m.tax_amt = invhdr.tax_amt
       IF invhdr.npstamt<>0
          m.npstrate = invhdr.npstrate
          m.npstamt = invhdr.npstamt
       ENDIF
    ENDIF
    IF  .NOT. (xtax .AND. xtax_meth='M') .AND.  .NOT. (xtax .AND. xtax_meth='A')
       m.xtax_desc = ''
       m.tax_rate = 0
       m.tax_amt = 0
       m.npstrate = 0
       m.npstamt = 0
       m.ctaxrefr = ''
    ENDIF
    wkamt = freight+insur+cod
    IF wkamt<>0
       m.fr_ins_cod = wkamt
    ENDIF
    m.totalchg = invhdr.totalchg
    m.discount = invhdr.discount
    IF llnote
       SELECT notepad
       IF SEEK('C'+m.invoice, "NOTEPAD") .AND.  .NOT. EMPTY(notepad.mnotes)
          m.mnothd = notepad.mnotes
          m.hasnot = 'Y'
       ELSE
          m.mnothd = ''
          m.hasnot = 'N'
       ENDIF
       m.note1 = invhdr.note1
       m.note2 = invhdr.note2
    ELSE
       m.hasnot = 'N'
       m.mnothd = ''
    ENDIF
    SELECT customer
    = SEEK(IIF(m.store=SPACE(8), 'M'+m.account, 'S'+m.account+m.store))
    = lfsolspadr()
    m.xbtname = customer.btname
    m.ccaddress1 = lasoldto(1)
    m.ccaddress2 = lasoldto(2)
    m.ccaddress3 = TRIM(lasoldto(3))+' '+TRIM(lasoldto(4))+' '+lasoldto(5)
    IF LEN(TRIM(m.ccaddress2))=0
       m.ccaddress2 = m.ccaddress3
       m.ccaddress3 = ''
    ENDIF
    STORE ' ' TO m.xfname, m.xfaddr1, m.xfaddr2, m.xfaddr3
    IF llprnfact .AND.  .NOT. EMPTY(invhdr.cfaccode)
       m.xfname = lcfacname
       m.xfaddr1 = lafactor(1)
       m.xfaddr2 = lafactor(2)
       m.xfaddr3 = TRIM(lafactor(3))+' '+lafactor(4)+' '+lafactor(5)
       IF m.xfaddr2=' '
          m.xfaddr2 = m.xfaddr3
          m.xfaddr3 = ' '
       ENDIF
    ENDIF
    SELECT ordhdr
    = SEEK('O'+invhdr.order, 'ORDHDR', 'ORDHDR')
    IF alt_shpto
       m.xsaddr1 = caddress1
       m.xsaddr2 = caddress2
       m.xsaddr3 = TRIM(caddress3)+' '+TRIM(caddress4)+' '+caddress5
       IF LEN(TRIM(m.xsaddr2))=0
          m.xsaddr2 = m.xsaddr3
          m.xsaddr3 = ''
       ENDIF
    ELSE
       IF invhdr.consol='Y'
          SELECT consinvh
          = SEEK(m.invoice)
          SELECT customer
          = SEEK('S'+m.account+consinvh.store)
       ENDIF
       SELECT customer
       IF  .NOT. EMPTY(dist_ctr)
          lcdist = dist_ctr
          = SEEK('S'+m.account+lcdist)
       ENDIF
       m.xstname = IIF(EMPTY(dba), stname, dba)
       m.xsaddr1 = caddress1
       m.xsaddr2 = caddress2
       m.xsaddr3 = TRIM(caddress3)+' '+TRIM(caddress4)+' '+caddress5
       IF LEN(TRIM(m.xsaddr2))=0
          m.xsaddr2 = m.xsaddr3
          m.xsaddr3 = ''
       ENDIF
    ENDIF
    m.xstname = IIF(EMPTY(customer.dba), customer.stname, customer.dba)
    m.xsaddr1 = lashipto(1)
    m.xsaddr2 = lashipto(2)
    m.xsaddr3 = TRIM(lashipto(3))+' '+TRIM(lashipto(4))+' '+lashipto(5)
    IF LEN(TRIM(m.xsaddr2))=0
       m.xsaddr2 = m.xsaddr3
       m.xsaddr3 = ''
    ENDIF
    SELECT invline
    IF SEEK(invhdr.invoice)
       SCAN REST FOR totqty<>0 WHILE invoice+STR(lineno, 6)=invhdr.invoice
          = SEEK(invline.style, 'Style', 'Style')
          = SEEK("S"+style.scale, "SCALE")
          m.style = invline.style
          m.totqty = invline.totqty
          m.price = invline.price
          m.stydesc = style.desc
          FOR lncount = 1 TO scale.cnt
             lcszcnt = ALLTRIM(STR(lncount))
             m.sz = SCALE.sz&lcszcnt. 
             m.qty= invline.qty&lcszcnt.
             IF m.qty<>0
                SELECT (lcinvlines)
                APPEND BLANK
                GATHER MEMO MEMVAR
                lncounter = lncounter+1
                IF lncounter>17
                   m.cntgrp = m.cntgrp+1
                   lncounter = 0
                ENDIF
             ENDIF
          ENDFOR
       ENDSCAN
    ENDIF
 ENDSCAN
 IF RECCOUNT(lcinvlines)=0
    = gfmodalgen('TRM00052B40011', 'ALERT')
    IF USED(lcinvlines)
       SELECT (lcinvlines)
       USE
    ENDIF
    RETURN
 ENDIF
 DIMENSION loogscroll.lacrparams[2, 2]
 loogscroll.lacrparams[1, 1] = 'XTAX'
 loogscroll.lacrparams[1, 2] = IIF(xtax, 1, 0)
 loogscroll.lacrparams[2, 1] = 'XTAX_METH'
 loogscroll.lacrparams[2, 2] = xtax_meth
 DIMENSION loogscroll.lacrtables[1]
 loogscroll.lacrtables[1] = oariaapplication.workdir+lcinvlines+".DBF"
 PRIVATE lctempals
 lctempals = loogscroll.gftempname()
 SELECT DISTINCT invoice FROM (lcinvlines) INTO CURSOR (lctempals)
 SELECT (lcinvlines)
 USE
 gfdispre()
 lldummy = loogscroll.ll2printer .AND. lfprtflag()
 RETURN
ENDPROC
**
*!*************************************************************
*! Name      : lfCreateTemp
*! Developer : Mariam Mazhar 
*! Date      : 02/07/2007
*! Purpose   : PRINT THE FOOTER OF THE INVOICE
*!*************************************************************

FUNCTION lfCreateTemp
 DIMENSION laheader[43, 4]
 STORE '' TO laheader
 laheader[1, 1] = 'INVOICE'
 laheader[1, 2] = 'C'
 laheader[1, 3] = 6
 laheader[1, 4] = 0
 laheader[2, 1] = 'NOTE1'
 laheader[2, 2] = 'C'
 laheader[2, 3] = 30
 laheader[2, 4] = 0
 laheader[3, 1] = 'NOTE2'
 laheader[3, 2] = 'C'
 laheader[3, 3] = 30
 laheader[3, 4] = 0
 laheader[4, 1] = 'ACCOUNT'
 laheader[4, 2] = 'C'
 laheader[4, 3] = 5
 laheader[4, 4] = 0
 laheader[5, 1] = 'STORE'
 laheader[5, 2] = 'C'
 laheader[5, 3] = 8
 laheader[5, 4] = 0
 laheader[6, 1] = 'XBTNAME'
 laheader[6, 2] = 'C'
 laheader[6, 3] = 30
 laheader[6, 4] = 0
 laheader[7, 1] = 'CCADDRESS1'
 laheader[7, 2] = 'C'
 laheader[7, 3] = 30
 laheader[7, 4] = 0
 laheader[8, 1] = 'CCADDRESS2'
 laheader[8, 2] = 'C'
 laheader[8, 3] = 100
 laheader[8, 4] = 0
 laheader[9, 1] = 'CCADDRESS3'
 laheader[9, 2] = 'C'
 laheader[9, 3] = 30
 laheader[9, 4] = 0
 laheader[10, 1] = 'XFNAME'
 laheader[10, 2] = 'C'
 laheader[10, 3] = 10
 laheader[10, 4] = 0
 laheader[11, 1] = 'XFADDR1'
 laheader[11, 2] = 'C'
 laheader[11, 3] = 30
 laheader[11, 4] = 0
 laheader[12, 1] = 'XFADDR2'
 laheader[12, 2] = 'C'
 laheader[12, 3] = 30
 laheader[12, 4] = 0
 laheader[13, 1] = 'XFADDR3'
 laheader[13, 2] = 'C'
 laheader[13, 3] = 100
 laheader[13, 4] = 0
 laheader[14, 1] = 'INVDATE'
 laheader[14, 2] = 'D'
 laheader[14, 3] = 8
 laheader[14, 4] = 0
 laheader[15, 1] = 'XSTNAME'
 laheader[15, 2] = 'C'
 laheader[15, 3] = 30
 laheader[15, 4] = 0
 laheader[16, 1] = 'XSADDR1'
 laheader[16, 2] = 'C'
 laheader[16, 3] = 30
 laheader[16, 4] = 0
 laheader[17, 1] = 'XSADDR2'
 laheader[17, 2] = 'C'
 laheader[17, 3] = 30
 laheader[17, 4] = 0
 laheader[18, 1] = 'XSADDR3'
 laheader[18, 2] = 'C'
 laheader[18, 3] = 100
 laheader[18, 4] = 0
 laheader[19, 1] = 'CUSTPO'
 laheader[19, 2] = 'C'
 laheader[19, 3] = 15
 laheader[19, 4] = 0
 laheader[20, 1] = 'PTERMS'
 laheader[20, 2] = 'C'
 laheader[20, 3] = 30
 laheader[20, 4] = 0
 laheader[21, 1] = 'PSHIPVIA'
 laheader[21, 2] = 'C'
 laheader[21, 3] = 30
 laheader[21, 4] = 0
 laheader[22, 1] = 'PSPCINST'
 laheader[22, 2] = 'C'
 laheader[22, 3] = 30
 laheader[22, 4] = 0
 laheader[23, 1] = 'REP1'
 laheader[23, 2] = 'C'
 laheader[23, 3] = 3
 laheader[23, 4] = 0
 laheader[24, 1] = 'PHONE'
 laheader[24, 2] = 'C'
 laheader[24, 3] = 20
 laheader[24, 4] = 0
 laheader[25, 1] = 'FR_INS_COD'
 laheader[25, 2] = 'N'
 laheader[25, 3] = 10
 laheader[25, 4] = 2
 laheader[26, 1] = 'TOTALCHG'
 laheader[26, 2] = 'N'
 laheader[26, 3] = 14
 laheader[26, 4] = 2
 laheader[27, 1] = 'nPstRate'
 laheader[27, 2] = 'N'
 laheader[27, 3] = 10
 laheader[27, 4] = 2
 laheader[28, 1] = 'nPstAmt'
 laheader[28, 2] = 'N'
 laheader[28, 3] = 13
 laheader[28, 4] = 2
 laheader[29, 1] = 'MnotHd'
 laheader[29, 2] = 'M'
 laheader[29, 3] = 10
 laheader[29, 4] = 0
 laheader[30, 1] = 'TAX_AMT'
 laheader[30, 2] = 'N'
 laheader[30, 3] = 10
 laheader[30, 4] = 2
 laheader[31, 1] = 'TAX_RATE'
 laheader[31, 2] = 'N'
 laheader[31, 3] = 15
 laheader[31, 4] = 2
 laheader[32, 1] = 'Discount'
 laheader[32, 2] = 'N'
 laheader[32, 3] = 13
 laheader[32, 4] = 2
 laheader[33, 1] = 'hasnot'
 laheader[33, 2] = 'C'
 laheader[33, 3] = 1
 laheader[33, 4] = 0
 laheader[34, 1] = 'Style'
 laheader[34, 2] = 'C'
 laheader[34, 3] = 19
 laheader[34, 4] = 0
 laheader[35, 1] = 'SZ'
 laheader[35, 2] = 'C'
 laheader[35, 3] = 5
 laheader[35, 4] = 0
 laheader[36, 1] = 'QTY'
 laheader[36, 2] = 'N'
 laheader[36, 3] = 6
 laheader[36, 4] = 0
 laheader[37, 1] = 'TOTQTY'
 laheader[37, 2] = 'N'
 laheader[37, 3] = 10
 laheader[37, 4] = 0
 laheader[38, 1] = 'PRICE'
 laheader[38, 2] = 'N'
 laheader[38, 3] = 12
 laheader[38, 4] = 2
 laheader[39, 1] = 'STYDESC'
 laheader[39, 2] = 'C'
 laheader[39, 3] = 30
 laheader[39, 4] = 0
 laheader[40, 1] = 'ShipDate'
 laheader[40, 2] = 'D'
 laheader[40, 3] = 8
 laheader[40, 4] = 0
 laheader[41, 1] = 'XTAX_DESC'
 laheader[41, 2] = 'C'
 laheader[41, 3] = 30
 laheader[41, 4] = 0
 laheader[42, 1] = 'cTaxRefr'
 laheader[42, 2] = 'C'
 laheader[42, 3] = 30
 laheader[42, 4] = 0
 laheader[43, 1] = 'CntGrp'
 laheader[43, 2] = 'N'
 laheader[43, 3] = 10
 laheader[43, 4] = 0
 gfcrttmp(lcinvlines, @laheader, "Invoice", lcinvlines, .F.)

**
*!*************************************************************
*! Name      : lfPrtFlag
*! Developer : Walid Hamed (WLD)
*! Date      : 07/30/2007
*! Purpose   : Update PRINTED FLAG
*!*************************************************************
*! Passed Parameters  : File Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*: B608185,1 WLD 07/30/2007 Update PRINTED FLAG
*!*************************************************************
FUNCTION lfPrtFlag
 PRIVATE lnoldals, lcinvno
 lnoldals = SELECT(0)
 SELECT (lctempals)
 SCAN
    lcinvno = invoice
    SELECT invhdr
    LOCATE FOR invoice=lcinvno
    IF FOUND()
       REPLACE prtflag WITH "P"
    ENDIF
 ENDSCAN
 USE IN (lctempals)
 SELECT (lnoldals)
