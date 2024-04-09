 lndatepos = ASUBSCRIPT(laogfxflt, ASCAN(laogfxflt, 'REPCOMM.DATE'), 1)
 ldate = CTOD(SUBSTR(laogfxflt(lndatepos,6), 1, ATC('|', laogfxflt(lndatepos,6))-1))
 hdate = CTOD(SUBSTR(laogfxflt(lndatepos,6), ATC('|', laogfxflt(lndatepos,6))+1))
 lnreppos = ASUBSCRIPT(laogfxflt, ASCAN(laogfxflt, 'SALESREP.REPCODE'), 1)
 hsalesrep = laogfxflt(lnreppos,6)
 xstatus = IIF(lcrpstatus='B', 'OH', lcrpstatus)
 DIMENSION xtotal( 2, 3)
 xtotal = 0
 lngrossamt = 0
 comfilter = 'AMOUNT<>0 .AND.BETWEEN(DATE,LDATE,HDATE).AND.             STATUS $ ALLTRIM(XSTATUS)'
 lcfilter = IIF( .NOT. EMPTY(hsalesrep), 'REPCODE = HSALESREP', '.T.')
 IF llmultcurr
    lccurfiltr = laogfxflt(lncurrpos,6)
    IF lncurrpos>0
       lncurrpos1 = AT('AND', SUBSTR(lcrpexp, lncurrpos))
       IF lncurrpos1>0
          lcfilter = IIF(EMPTY(lccurfiltr), lcfilter, lcfilter+' AND '+SUBSTR(lcrpexp, lncurrpos, lncurrpos1-1))
       ELSE
          lncurrpos1 = LEN(lcrpexp)
          lcfilter = IIF(EMPTY(lccurfiltr), lcfilter, lcfilter+' AND '+SUBSTR(lcrpexp, lncurrpos, lncurrpos1))
       ENDIF
    ENDIF
 ENDIF
 fromdate = DTOC(ldate)
 thrudate = DTOC(hdate)
 PERIOD   = 'PERIOD: &FROMDATE - &THRUDATE'
 WAIT WINDOW NOWAIT 'Locating records in file....'
 SELECT repcomm
 LOCATE FOR &COMFILTER AND &lcFilter
 IF EOF('REPCOMM')
    = gfmodalgen(.F.,.F.,.F.,.F.,'There are no records to display...! ')
    SET DEVICE TO SCREEN
    lclastexpr = lcrpexp
    RETURN
 ELSE
    IF llclearfn .OR. llogfltch
       IF USED(reptemp)
          USE IN (reptemp)
       ENDIF
       COPY REST TO (gcWorkDir+RepTemp) FOR &ComFilter AND &lcFilter
       USE IN 0 (gcworkdir+reptemp)
       SELECT (reptemp)
       llclearfn = .F.
    ELSE
       SELECT (reptemp)
       GOTO TOP
    ENDIF
 ENDIF
 = gfopenfile(gcdatadir+'Credit',gcdatadir+'Crtran','SH')
 = gfopenfile(gcdatadir+'ARHIST',gcdatadir+'Arhistt','SH')
 SELECT (reptemp)
 SET RELATION TO
 SET RELATION TO trantype+tran INTO credit ADDITIVE
 LOCATE
 a = 'SRREPSH                                        SALESREP COMMISSION WORKSHEET REPORT'
 b = '  DATE   TRAN#  .....DESC...... NAME....   STORE#    ORDER# CUST PO    PCNT  TRD DSC  MER DSC   GROSS AMT.     NET-SHIP COMMISSION'
 choice = lcrpsortby
 IF llmultcurr .AND. lcrpcurr='F'
    DO CASE
       CASE choice='I'
          sortfield = 'REPCODE+CCURRCODE+TRAN+STR(RECNO(),7)'
       CASE choice='O'
          sortfield = 'REPCODE+CCURRCODE+ORDER+TRAN+STR(RECNO(),7)'
       CASE choice='C'
          sortfield = 'REPCODE+CCURRCODE+CUSTPO+TRAN+STR(RECNO(),7)'
       CASE choice='D'
          sortfield = 'REPCODE+CCURRCODE+DTOS(DATE)+TRAN+STR(RECNO(),7)'
    ENDCASE
 ELSE
    DO CASE
       CASE choice='I'
          sortfield = 'REPCODE+TRAN+STR(RECNO(),7)'
       CASE choice='O'
          sortfield = 'REPCODE+ORDER+TRAN+STR(RECNO(),7)'
       CASE choice='C'
          sortfield = 'REPCODE+CUSTPO+TRAN+STR(RECNO(),7)'
       CASE choice='D'
          sortfield = 'REPCODE+DTOS(DATE)+TRAN+STR(RECNO(),7)'
    ENDCASE
 ENDIF
 DO CASE
    CASE lnrpdeclno=0
       lcshpformt = '@Z 99999999999'
       lcamtformt = '@Z 999999999'
    CASE lnrpdeclno=1
       lcshpformt = '@Z 999999999.9'
       lcamtformt = '@Z 9999999.9'
    OTHERWISE
       lcshpformt = '@Z 99999999.99'
       lcamtformt = '@Z 999999.99'
 ENDCASE
 WAIT WINDOW NOWAIT 'Sorting '+LTRIM(STR(RECCOUNT(), 9))+' Commission records ...'
 SELECT (reptemp)
 INDEX ON &SORTFIELD TAG &REPTEMP
 IF hsalesrep=' '
    SELECT salesrep
    GOTO TOP
    hsalesrep = CHR(254)
 ENDIF
 SELECT &RepTemp
 iocom = SELECT()
 lcrepcurr = ccurrcode
 row = 99
 newrep = .T.
 pageno = 0
 SET DEVICE TO PRINTER
 STORE 0 TO lnnetship, lnamount
 DO WHILE .T.
    SELECT salesrep
    xsalesrep = repcode
    lcsalename = name
    IF EOF() .OR. repcode>hsalesrep
       EXIT
    ENDIF
    IF newrep
       IF (hsalesrep==xsalesrep) .OR. EMPTY(hsalesrep)
          WAIT WINDOW NOWAIT 'Sales commissiom for sales representative '+xsalesrep
       ENDIF
       STORE .F. TO gotcom, gotord, ptotal1, ptotal2
       row = 99
       xtotal = 0.00 
       CURFILE = '&REPTEMP'
       SELECT &REPTEMP
       SEEK xsalesrep
       gotcom = IIF(FOUND(), .T., .F.)
       IF  .NOT. gotcom
          SELECT salesrep
          SKIP
          LOOP
       ENDIF
    ENDIF
    newrep = .F.
    IF row>=45
       r_title = 'SALESREP COMMISSION WORKSHEET'
       xtitle = period
       pageno = pageno+1
       DO rpt_hdr WITH 'SRREPSH', xtitle, r_width
       row = 5
       newpage = .T.
    ENDIF
    DO WHILE gotcom
       SELECT &REPTEMP
       IF llmultcurr .AND. lcrepcurr<>ccurrcode .AND. lcrpcurr='F'
          = lfsubtot()
          lcrepcurr = ccurrcode
       ENDIF
       IF newpage
          newpage = .F.
          @ row, 01 SAY 'SALESREP: '+xsalesrep+'  '+salesrep.name
          STORE 0.00  TO tshipamt, tcommdue, tbakord
          row = row+2
          @ row, 00 SAY b
          row = row+2
       ENDIF
       SELECT &REPTEMP
       IF EOF() .OR. (repcode<>xsalesrep)
          gotcom = .F.
          ptotal1 = .T.
          newpage = .T.
          EXIT
       ENDIF
       IF row>55
          EXIT
       ENDIF
       SELECT &RepTemp
       CURFILE = '&REPTEMP'
       xtermspct = 00.00 
       SELECT invhdr
       SEEK &REPTEMP..Tran
       lndisc = 00.00 
       IF FOUND() AND &REPTEMP..TranType $ '16'
          lndisc = discpcnt
          IF &REPTEMP..TRANTYPE='1'  AND InvHdr.Consol = "Y" 
             SELECT consinvh
             SEEK(InvHdr.Invoice+&RepTemp..Store+&RepTemp..Order)
             xtermspct = consinvh.trde_disc
             SELECT invhdr
             xnetship = consinvh.shipamt+consinvh.discount
             lngrossamt = consinvh.shipamt
          ENDIF
          IF &REPTEMP..TRANTYPE='1'  AND InvHdr.Consol <> "Y" 
             xtermspct = invhdr.trde_disc
             xnetship = IIF(invhdr.status='V', invhdr.vshipamt+invhdr.vdiscount, invhdr.shipamt+invhdr.discount)
             lngrossamt = IIF(invhdr.status='V', invhdr.vshipamt, invhdr.shipamt)
          ENDIF
          IF &REPTEMP..TRANTYPE='6'  AND InvHdr.Consol = "Y" 
             SELECT consinvh
             SEEK(InvHdr.Invoice+&RepTemp..Store+&RepTemp..Order)
             xtermspct = consinvh.trde_disc
             SELECT invhdr
             xnetship = (consinvh.shipamt+consinvh.discount)*-1
             lngrossamt = (consinvh.shipamt)*-1
          ENDIF
          IF &REPTEMP..TRANTYPE='6'  AND InvHdr.Consol <> "Y" 
             xtermspct = invhdr.trde_disc
             xnetship = (invhdr.vshipamt+invhdr.vdiscount)*-1
             lngrossamt = (invhdr.vshipamt)*-1
          ENDIF
          IF xtermspct<>0
             xnetship = xnetship-(xnetship*xtermspct/100)
          ENDIF
       ELSE
          IF ('RM' $ gcCmpModules) AND SEEK(&REPTEMP->TRAN,'RETHDR')
             xnetship = IIF(rethdr.status='V', rethdr.vamount, rethdr.amount)
             lndals = SELECT()
             SELECT credit
             lcoldord = ORDER()
             SET ORDER TO CREDIT
             IF SEEK(&REPTEMP..Account+&REPTEMP..TRAN,'Credit')
                xnetship = xnetship+credit.dsc_amt
             ENDIF
             SET ORDER TO &lcOldOrd
             SELECT (lndals)
             IF SEEK(&REPTEMP..Account+&REPTEMP..TRAN,'ARHIST') AND ARHIST.TRANTYPE = '0'
                xnetship = xnetship+arhist.dsc_amt
             ENDIF
             XNETSHIP = XNETSHIP * IIF(&REPTEMP..TRANTYPE='5',-1,1)
             IF &REPTEMP..TranType = '5' 
                lngrossamt = IIF(rethdr.status='V', rethdr.vgross_amt, rethdr.gross_amt)*-1
             ELSE
                lngrossamt = IIF(rethdr.status='V', rethdr.vgross_amt, rethdr.gross_amt)
             ENDIF
          ELSE
             IF &REPTEMP..TranType = '5'
                XNETSHIP = &REPTEMP->NORG_AMNT
             ELSE
                XNETSHIP = IIF(&REPTEMP->COMMPCNT<>0, (&REPTEMP->AMOUNT/(&REPTEMP->COMMPCNT/100)),0)
             ENDIF
             IF &REPTEMP..TranType = '5'
                lngrossamt = credit.amount
             ENDIF
             lnalias = ALIAS()
             SELECT (reptemp)
             FOR lncount = 1 TO FCOUNT()
                IF FIELD(lncount)='ORGNL_CADJ' .AND. orgnl_cadj<>0
                   xnetship = orgnl_cadj
                   EXIT
                ENDIF
             ENDFOR
             SELECT (lnalias)
          ENDIF
       ENDIF
       SELECT customer
       SEEK 'M'+&REPTEMP->ACCOUNT
       xname = IIF(FOUND(), SUBSTR(btname, 1, 15), '')
       SELECT &REPTEMP
       lnnetship = IIF(llmultcurr, lfbaseamt(xnetship), xnetship)
       lnamount = IIF(llmultcurr, lfbaseamt(nforamnt), amount)
       @ row, 00 SAY date
       @ row, 09 SAY tran
       @ row, 16 SAY SUBSTR(desc, 1, 13)
       @ row, 32 SAY SUBSTR(xname, 1, 9)
       @ row, 43 SAY store
       @ row, 53 SAY order
       @ row, 60 SAY LEFT(ALLTRIM(custpo), 14)
       @ row, 70 SAY commpcnt PICTURE '@Z 99.99'
       @ row, 79 SAY xtermspct PICTURE '@Z 99.99'
       @ row, 87 SAY lndisc PICTURE '@ 99.99%'
       @ row, 96 SAY lngrossamt PICTURE '@Z 9999999.99'
       @ row, 108 SAY lnnetship PICTURE lcshpformt
       @ row, 121 SAY lnamount PICTURE lcamtformt
       xtotal( 1, 1) = xtotal(1,1)+lnnetship
       xtotal( 1, 2) = xtotal(1,2)+lngrossamt
       xtotal( 1, 3) = xtotal(1,3)+lnamount
       row = row+1
       SELECT &REPTEMP
       SKIP
    ENDDO
    IF llmultcurr .AND. lcrpcurr='F' .AND.  .NOT. gotcom .AND.  .NOT. EOF()
       = lfsubtot()
       lcrepcurr = ccurrcode
    ENDIF
    IF ptotal1 .AND. lcrpcurr<>'F'
       ptotal1 = .F.
       row = row+1
       @ row, 020 SAY xsalesrep+' TOTALS ------------------------------------------------->'
       @ row, 95 SAY xtotal(1,2) PICTURE '99999999.99'
       DO CASE
          CASE lnrpdeclno=0
             @ row, 107 SAY xtotal(1,1) PICTURE '@Z 99999999999'
             @ row, 120 SAY xtotal(1,3) PICTURE '@Z 999999999'
          CASE lnrpdeclno=1
             @ row, 107 SAY xtotal(1,1) PICTURE '@Z 999999999.9'
             @ row, 120 SAY xtotal(1,3) PICTURE '@Z 9999999.9'
          OTHERWISE
             @ row, 107 SAY xtotal(1,1) PICTURE '@Z 99999999.99'
             @ row, 120 SAY xtotal(1,3) PICTURE '@Z 999999.99'
       ENDCASE
       row = row+1
       @ row, 00 SAY REPLICATE('-', 132)
       row = row+2
    ENDIF
    IF  .NOT. gotcom
       newrep = .T.
       SELECT salesrep
       SKIP
    ENDIF
 ENDDO
 DO endreport
 SET DEVICE TO SCREEN
 lclastexpr = lcrpexp
 RETURN
*
PROCEDURE lfwrepwhen
 r_width = 'W'
 lndatepos = ASUBSCRIPT(laogfxflt, ASCAN(laogfxflt, 'REPCOMM.DATE'), 1)
 IF EMPTY(laogfxflt(lndatepos,6))
    laogfxflt[ lndatepos, 6] = DTOC(DATE()-DAY(DATE())-(DAY(DATE()-DAY(DATE()))-1))+'|'+DTOC(DATE()-DAY(DATE()))
 ENDIF
 IF llmultcurr
    lncurrpos = lfitmpos('REPCOMM.CCURRCODE')
 ELSE
    lcrpcurr = 'O'
 ENDIF
*
PROCEDURE lfwoldval
 laoldval = EVALUATE(SYS(18))
*
PROCEDURE lfvrepcode
 PRIVATE lcvar, lcobj, latemp
 lcvar = SYS(18)
 lcobj = EVALUATE(SYS(18))
 IF  .NOT. EMPTY(lcobj) .AND. ('?'$lcobj .OR.  .NOT. SEEK(lcobj, 'SALESREP'))
    SELECT salesrep
    DIMENSION latemp[ 1]
    latemp = ''
    lcbrfields = "REPCODE   :R :H= 'Code' , "+"NAME      :R :H= 'Name' ,"+"cAddress6 :R :H= 'Country' ,"+"PHONE     :R :H= 'Phone' ,"+"BALANCE   :R :H= 'Balance' "
    lcfile_ttl = 'Sales Representative ...'
    = gfbrows('','REPCODE','laTemp')
    IF  .NOT. EMPTY(latemp(1))
       lcobj = latemp(1)
    ELSE
       lcobj = laoldval
    ENDIF
 ENDIF
 &lcVar = lcObj      
*
PROCEDURE lfvdate
 IF EMPTY(EVALUATE(SYS(18)))
    WAIT WINDOW NOWAIT 'You must fill period range...'
    lccurrobj = SYS(18)
    &lcCurrObj = laOldVal
    SHOW GET lccurrobj
    _CUROBJ = _CUROBJ
 ENDIF
*
PROCEDURE lfclearrep
 llclearfn = .T.
 IF USED(reptemp)
    USE IN (reptemp)
    ERASE (gcworkdir+reptemp+'.DBF')
    ERASE (gcworkdir+reptemp+'.CDX')
 ENDIF
 IF llopencomp .AND. USED('SYCCOMP')
    USE IN syccomp
 ENDIF
 IF llmultcurr
    SET CURRENCY TO lccurrsymb
    SET CURRENCY &lcCurAlign
    IF llopenint .AND. USED('SYCINT')
       USE IN sycint
    ENDIF
    IF llopencurr .AND. USED('SYCCURR')
       USE IN syccurr
    ENDIF
    IF llopenexch .AND. USED('SYCEXCH')
       USE IN sycexch
    ENDIF
 ENDIF
*
PROCEDURE lffillvars
 IF  .NOT. USED('SYCCOMP')
    USE &gcSysHome.SYCCOMP ORDER TAG cComp_ID IN 0
    llopencomp = .T.
 ENDIF
 IF llmultcurr
    IF  .NOT. USED('SYCINT')
       USE IN 0 (gcsyshome+'SYCINT.DBF')
       llopenint = .T.
    ENDIF
    IF  .NOT. USED('SYCEXCH')
       USE IN 0 (gcsyshome+'SYCEXCH.DBF') ORDER Currency
       llopenexch = .T.
    ENDIF
    DIMENSION lacurrval[ 1, 1]
    IF  .NOT. USED('SYCCURR')
       llopencurr = gfopenfile(gcsyshome+'SYCCURR',gcsyshome+'Ccurrcode','SH')
    ELSE
       SELECT syccurr
       SET ORDER TO CCURRCODE
    ENDIF
    SELECT DISTINCT ccurrcode FROM SYCCURR ORDER BY ccurrcode INTO ARRAY lacurrval
    DIMENSION lacurrdesc[ ALEN(lacurrval, 1), 1]
    FOR lni = 1 TO ALEN(lacurrval, 1)
       = SEEK(ALLTRIM(lacurrval(lni,1)))
       lacurrval[ lni, 1] = PADR(lacurrval(lni,1), 3)
       lacurrdesc[ lni, 1] = ccurrcode+' - '+ALLTRIM(ccurrdesc)
    ENDFOR
 ENDIF
*
FUNCTION lfitmpos
 PARAMETER lcitminflt
 PRIVATE lnitmpos
 lnitmpos = ASCAN(laogfxflt, lcitminflt)
 IF lnitmpos>0
    lnitmpos = ASUBSCRIPT(laogfxflt, lnitmpos, 1)
 ENDIF
 RETURN lnitmpos
*
FUNCTION lfbaseamt
 PARAMETER lnamntcurr
 PRIVATE lnbaseamt
 lnbaseamt = lnamntcurr
 IF llmultcurr .AND. lcrpcurr<>'F'
    lnbaseamt = gfamntdisp(lnbaseamt,lcrpcurr,ldrpexdate,lcrptmpnam)
 ENDIF
 RETURN lnbaseamt
*
PROCEDURE lfsubtot
 @ row, 00 SAY REPLICATE('-', 132)
 row = row+1
 @ row, 00 SAY ' *  SUB TOTAL *            CURRENCY    '+lcrepcurr+'    '+lcsalename
 @ row, 95 SAY xtotal(1,2) PICTURE '99999999.99'
 DO CASE
    CASE lnrpdeclno=0
       @ row, 107 SAY xtotal(1,1) PICTURE '@Z 999999999999'
       @ row, 120 SAY xtotal(1,3) PICTURE '@Z 9999999999'
    CASE lnrpdeclno=1
       @ row, 107 SAY xtotal(1,1) PICTURE '@Z 9999999999.9'
       @ row, 120 SAY xtotal(1,3) PICTURE '@Z 99999999.9'
    OTHERWISE
       @ row, 107 SAY xtotal(1,1) PICTURE '@Z 999999999.99'
       @ row, 120 SAY xtotal(1,3) PICTURE '@Z 9999999.99'
 ENDCASE
 row = row+1
 @ row, 00 SAY REPLICATE('-', 132)
 row = row+2
 xtotal = 0.00 
