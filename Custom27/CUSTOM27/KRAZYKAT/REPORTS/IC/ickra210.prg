 lcsttime = TIME()
 lcrpexp = STRTRAN(lcrpexp, 'STYLE.', '')
 SELECT style
 IF llogfltch
    IF USED(lctempsty) .AND. RECCOUNT(lctempsty)>0
       SELECT (lctempsty)
       ZAP
    ENDIF
    SELECT style
    SET RELATION TO 'S'+scale INTO scale ADDITIVE
    SCAN FOR &lcRpExp
       STORE 0 TO lnots1, lnots2, lnots3, lnots4, lnots5, lnots6, lnots7, lnots8, lntotots
       FOR lncounter = 1 TO scale.cnt
          lccounter = STR(lncounter, 1)
          lnOTS&lcCounter = Stk&lcCounter + IIF(lcRpCutSol = 'W', Wip&lcCounter,Plan&lcCounter) - Ord&lcCounter
          lnTotOTS = lnTotOTS + lnOTS&lcCounter
       ENDFOR
       DO CASE
          CASE lcrpsign='P' .AND. lntotots<lnrpmini
             LOOP
          CASE lcrpsign='N' .AND. lntotots>lnrpmini
             LOOP
          CASE lcrpsign='B' .AND. lntotots=0
             LOOP
       ENDCASE
       SCATTER MEMO MEMVAR
       INSERT INTO (lctempsty) FROM MEMVAR
    ENDSCAN
    SET RELATION OFF INTO scale
 ENDIF
 SELECT (lctempsty)
 llprint = .F.
 GOTO TOP
 IF EOF()
    = gfmodalgen('TRM00052B00000','DIALOG')
    SET DEVICE TO SCREEN
    RETURN
 ELSE
    SELECT ordline
    SET RELATION TO cordtype+order INTO ordhdr ADDITIVE
    SELECT posln
    SET RELATION TO cstytype+po INTO poshdr ADDITIVE
    CLEAR TYPEAHEAD
    SET DEVICE TO PRINTER
    DO lprpprint
    SELECT ordline
    SET RELATION OFF INTO ordhdr
    SELECT posln
    SET RELATION OFF INTO poshdr
    lcedtime = TIME()
    lninterval = lfcolltime(lcsttime,lcedtime)
    WAIT WINDOW TIMEOUT 2 'Selected records in '+ALLTRIM(STR(lninterval, 6, 2))+' Seconds...'
    IF llprint
       DO endreport
    ELSE
       = gfmodalgen('TRM00000B00000',.F.,.F.,.F.,'No records have been printed!')
    ENDIF
    IF lcrpformat='L' .AND. gcdevice='P'
       @ PROW(),PCOL() SAY [&l0O]
    ENDIF
    SET DEVICE TO SCREEN
 ENDIF
*
PROCEDURE lprpprint
 r_title = 'Customized Open To Sell Summary'
 r_width = 'W'
 row = 99
 pageno = 0
 lngrandtot = 0
 SELECT (lctempsty)
 GOTO TOP
 IF lcrpformat='L' .AND. gcdevice='P'
    @ PROW(),PCOL() SAY [&l1O&l14]
 ENDIF
 DO WHILE  .NOT. EOF() .AND. INKEY()<>32
    lcstyle = SUBSTR(style, 1, lnmajlen)
    llfirsttim = .T.
    lntotal = 0
    lngrouptot = 0
    lcgroup = cstygroup
    lcpattern = pattern
    IF lcrpsort<>'S'
       = lfheader()
       lngrouptot = 0
    ELSE
       = IIF(row>=IIF(lcrpformat='L', 40, 55), lfheader(), .F.)
    ENDIF
    lcscale = ' '
    DO WHILE IIF(lcrpsort='S',  .NOT. EOF(), IIF(lcrpsort='P', pattern+style=lcpattern .AND.  .NOT. EOF(), cstygroup+pattern+style=lcgroup .AND.  .NOT. EOF()))
       lcstyle = SUBSTR(style, 1, lnmajlen)
       lcscale = ' '
       lntotal = 0
       lcpattern = pattern
       lcgroup = cstygroup
       llfirsttim = .T.
       SCAN REST WHILE IIF(lcrpsort='S', style=lcstyle, IIF(lcrpsort='P', pattern+style=lcpattern+lcstyle, cstygroup+pattern+style=lcgroup+lcpattern+lcstyle))
          WAIT WINDOW NOWAIT 'Printing '+lcmajttl+' '+style+' Space Bar to abort'
          = IIF(row>=IIF(lcrpformat='L', 40, 55), lfheader(), .F.)
          STORE 0 TO lntotots, lntotstk
          FOR lni = 1 TO 8
             lci = STR(lni, 1)
             lnOTS&lcI = 0
          ENDFOR
          = SEEK('S'+scale, 'Scale')
          FOR lni = 1 TO scale.cnt
             lci = STR(lni, 1)
             lnOTS&lcI = Stk&lcI + IIF(lcRpCutSol='W',Wip&lcI,Plan&lcI) - Ord&lcI
             lnTotOTS  = lnTotOTS + lnOTS&lcI
          ENDFOR
          DO CASE
             CASE lcrpsign='P' .AND. lntotots<lnrpmini
                LOOP
             CASE lcrpsign='N' .AND. lntotots>lnrpmini
                LOOP
             CASE lcrpsign='B' .AND. lntotots=0
                LOOP
          ENDCASE
          IF llfirsttim
             row = row+1
             = IIF(row>=IIF(lcrpformat='L', 40, 55), lfheader(), .F.)
             @ row, 00 SAY '**  '+SUBSTR(style, 1, lnmajlen)
             IF SEEK('F'+lcstyle, 'NotePad')
                DO lpprnnotpd
             ENDIF
             llfirsttim = .F.
          ENDIF
          llprint = .T.
          IF scale<>lcscale
             lncol = 38
             FOR lni = 1 TO scale.cnt
                lci = STR(lni, 1)
                @ Row,lnCol SAY PADL(ALLTRIM(Scale.Sz&lcI),5)
                lncol = lncol+7
             ENDFOR
             row = row+1
             = IIF(row>=IIF(lcrpformat='L', 40, 55), lfheader(), .F.)
             lcscale = scale
          ENDIF
          @ row, 00 SAY SUBSTR(style, lnclrpo, lncolorlen)
          @ row, 07 SAY PADR(gfcoddes(SUBSTR(style, lnclrpo, lncolorlen),'Color'), 15)
          @ row, 23 SAY cstygroup
          @ row, 28 SAY pricea PICTURE '9999.99'
          lncol = 37
          FOR lni = 1 TO scale.cnt
             lci = STR(lni, 1)
             @ Row,lnCol SAY lnOTS&lcI PICTURE "999999"
             lncol = lncol+7
          ENDFOR
          @ row, 94 SAY lntotots PICTURE '99999999'
          lntotal = lntotal+lntotots
          lngrandtot = lngrandtot+lntotots
          lngrouptot = lngrouptot+lntotots
          lntotstk = lfgettotst()
          lntotstk = totstk-lntotstk
          IF lntotstk>0
             row = row+1
             = IIF(row>=IIF(lcrpformat='L', 40, 55), lfheader(), .F.)
             @ row, 102 SAY 'STOCK'
             @ row, 110 SAY lntotstk PICTURE '9999999'
          ENDIF
          DO lppravlqty
          row = row+1
          = IIF(row>=IIF(lcrpformat='L', 40, 55), lfheader(), .F.)
       ENDSCAN
       IF lntotal<>0
          = IIF(row>=IIF(lcrpformat='L', 40, 55), lfheader(), .F.)
          @ row, 00 SAY REPLICATE('-', 131)
          row = row+1
          = IIF(row>=IIF(lcrpformat='L', 40, 55), lfheader(), .F.)
          @ row, 00 SAY '** '+UPPER(lcmajttl)+' TOTAL **'
          @ row, 94 SAY lntotal PICTURE '99999999'
          row = row+1
          = IIF(row>=IIF(lcrpformat='L', 40, 55), lfheader(), .F.)
          @ row, 00 SAY REPLICATE('-', 131)
          row = row+1
          = IIF(row>=IIF(lcrpformat='L', 40, 55), lfheader(), .F.)
       ENDIF
    ENDDO
    IF lcrpsort$'GP'
       = IIF(row>=IIF(lcrpformat='L', 39, 54), lfheader(), .F.)
       @ row, 00 SAY REPLICATE('-', 131)
       row = row+1
       = IIF(row>=IIF(lcrpformat='L', 40, 55), lfheader(), .F.)
       @ row, 00 SAY IIF(lcrpsort='G', 'STYLE GROUP TOTAL ---- '+cstygroup+'-'+gfcoddes(cstygroup,'CSTYGROUP'), 'PATTERN TOTAL ---- '+lcpattern)
       @ row, 94 SAY lngrouptot PICTURE '99999999'
       row = row+1
       = IIF(row>=IIF(lcrpformat='L', 40, 55), lfheader(), .F.)
       @ row, 00 SAY REPLICATE('-', 131)
       row = row+1
       = IIF(row>=IIF(lcrpformat='L', 40, 55), lfheader(), .F.)
    ENDIF
    SELECT (lctempsty)
 ENDDO
 row = row+1
 = IIF(row>=IIF(lcrpformat='L', 40, 55), lfheader(), .F.)
 @ row, 00 SAY REPLICATE('=', 131)
 row = row+1
 = IIF(row>=IIF(lcrpformat='L', 40, 55), lfheader(), .F.)
 @ row, 00 SAY '** GRAND TOTAL **'
 @ row, 94 SAY lngrandtot PICTURE '99999999'
 row = row+1
 = IIF(row>=IIF(lcrpformat='L', 40, 55), lfheader(), .F.)
 @ row, 00 SAY REPLICATE('=', 131)
*
PROCEDURE lpprnhdr
 pageno = pageno+1
 DO rpt_hdr WITH 'ICKRA210', lcrpoptitl, r_width
 row = 5
 @ row, 00 SAY PADR(lcnonmajt, 8)+' ....Descr....   Price    Sz1    Sz2    Sz3    Sz4    Sz5    Sz6    Sz7    Sz8   Net OTS'
 row = row+1
 @ row, 00 SAY REPLICATE('-', 114)
 row = row+1
*
PROCEDURE lpprnnotpd
 PRIVATE lnalias, lnoldmemw, lnnoteline
 lnalias = SELECT()
 lnoldmemw = SET('MEMOWIDTH')
 lnnoteline = 1
 SET MEMOWIDTH TO 72
 SELECT notepad
 DO WHILE lnnoteline<=3
    IF row>=IIF(lcrpformat='L', 40, 55)
       DO lpprnhdr
    ENDIF
    @ row, 36 SAY MLINE(mnotes, lnnoteline)
    row = row+1
    lnnoteline = lnnoteline+1
 ENDDO
 row = row+1
 IF row>=IIF(lcrpformat='L', 40, 55)
    DO lpprnhdr
 ENDIF
 SET MEMOWIDTH TO lnoldmemw
 SELECT (lnalias)
*
FUNCTION lfgettotst
 PRIVATE lnalias
 lnalias = SELECT(0)
 lnqty = 0
 IF SEEK (&lcTempSty..Style,'OrdLine')
    SELECT ordline
    SCAN REST WHILE Style + DTOS(Complete) + cOrdtype + Order + Store + STR(LineNo,6) = &lcTempSty..Style
       IF ordhdr.status$'OH'
          lnqty = lnqty+totqty
       ENDIF
    ENDSCAN
 ENDIF
 SELECT (lnalias)
 RETURN (lnqty)
*
PROCEDURE lppravlqty
 PRIVATE lnalias
 lnalias = SELECT(0)
 IF SEEK (&lcTempSty..Style,'PosLn')
    CREATE CURSOR (lcpotemp) (available D, ntotqty N (7))
    SELECT (lcpotemp)
    ZAP
    INDEX ON DTOS(available) TAG (lcpotemp) OF (gcworkdir+lcpotemp)
    SELECT posln
    SCAN REST WHILE Style + cStyType + Po + STR(Lineno,6) +  TranCd + STR(RECNO(),7) = &lcTempSty..Style
       IF poshdr.status$'OH' .AND.  .NOT. (trancd$'36')
          SELECT (lcpotemp)
          IF SEEK(DTOS(poshdr.available))
             REPLACE ntotqty WITH ntotqty+IIF(posln.trancd='1', posln.totqty, -posln.totqty)
          ELSE
             INSERT INTO (lcpotemp) (available, ntotqty) VALUE (poshdr.available, IIF(posln.trancd='1', posln.totqty, -posln.totqty))
          ENDIF
       ENDIF
    ENDSCAN
    SELECT (lcpotemp)
    SCAN
       IF lntotstk<0
          lntotstk = lntotstk+ntotqty
          REPLACE ntotqty WITH lntotstk
          IF ntotqty<0
             LOOP
          ENDIF
       ENDIF
       row = row+1
       IF row>=IIF(lcrpformat='L', 40, 55)
          DO lpprnhdr
       ENDIF
       @ row, 102 SAY available
       @ row, 110 SAY ntotqty PICTURE '9999999'
    ENDSCAN
 ENDIF
 IF lntotstk<0
    row = row+1
    IF row>=IIF(lcrpformat='L', 40, 55)
       DO lpprnhdr
    ENDIF
    @ row, 102 SAY 'OVERSOLD'
 ENDIF
 SELECT (lnalias)
*
PROCEDURE lfwrepwhen
 lnstatpos = lfitmpos('STYLE.STATUS')
 laogfxflt[ lnstatpos, 6] = 'A'
 SELECT style
 IF  .NOT. USED(lctempsty)
    COPY TO (gcworkdir+lctempsty) STRUCTURE
    = gfopenfile(gcworkdir+lctempsty,' ','EX')
    INDEX ON cstygroup+pattern+style TAG StyGrp
    INDEX ON pattern+style TAG pattern
    INDEX ON style TAG STYLE
 ENDIF
 = gfopenfile(gcsyshome+'SYCCOMP',gcsyshome+'Ccomp_id','SH')
 = SEEK(gcact_comp, 'SycComp')
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
FUNCTION lfnonmaj
 lnmajseg = gfitemmask('SM')
 DIMENSION lamajseg[ 1, 1]
 = gfitemmask(@lamajseg)
 llstopconc = .F.
 FOR lni = lnmajseg+1 TO ALEN(lamajseg, 1)
    lnnonmajpo = IIF(lnnonmajpo=0, lamajseg(lni,4), lnnonmajpo)
    IF lamajseg(lni,1)='F' .AND.  .NOT. llstopconc
       lcfreeclr = IIF(EMPTY(lcfreeclr), lamajseg(lni,1), lcfreeclr)
       lcnonmajpi = IIF(EMPTY(lcnonmajpi), lamajseg(lni,3), lcnonmajpi+lamajseg(lni-1,6)+lamajseg(lni,3))
       lcnonmajt = IIF(EMPTY(lcnonmajt), PADR(lamajseg(lni,2), LEN(lamajseg(lni,3))), lcnonmajt+lamajseg(lni-1,6)+PADR(lamajseg(lni,2), LEN(lamajseg(lni,3))))
    ENDIF
    IF lamajseg(lni,1)='C' .OR. ( .NOT. EMPTY(lcfreeclr) .AND. lamajseg(lni,1)<>'F')
       IF lamajseg(lni,1)='C'
          lnclrpo = lamajseg(lni,4)
          lcfreeclr = lamajseg(lni,1)
          lcnonmajpi = lamajseg(lni,3)
          lcnonmajt = PADR(lamajseg(lni,2), LEN(lamajseg(lni,3)))
          EXIT
       ELSE
          llstopconc = .T.
       ENDIF
    ENDIF
 ENDFOR
 STORE LEN(lcnonmajpi) TO lnfreelen, lncolorlen
 lccolortt = 'Only This '+ALLTRIM(lcnonmajt)
 RETURN ''
*
PROCEDURE lfvstyle
 lcstyle = VARREAD()
 lctag = ORDER('STYLE')
 SET ORDER TO cStyle IN style
 IF LASTKEY()=13 .AND.  .NOT. MDOWN()
    IF SEEK(&lcStyle.,'Style') 
       &lcStyle = STYLE.cStyMajor
    ELSE
       &lcStyle = gfStyBrw('M',"","",.F.)
    ENDIF
 ELSE
    &lcStyle = ''
 ENDIF
 SET ORDER TO lcTag IN style
*
PROCEDURE lfvfabric
 lcfabobj = VARREAD()
 lcFab    = &lcFabObj
 llusebyme = .F.
 IF  .NOT. USED('FABRIC')
    llusebyme = .T.
    USE IN 0 SHARED (gcdatadir+'FABRIC')
 ENDIF
 lctag = ORDER('FABRIC')
 SET ORDER TO FABRIC IN fabric
 IF LASTKEY()=13 .AND.  .NOT. MDOWN()
    IF SEEK(lcfab, 'FABRIC')
       &lcFabObj = FABRIC.Fabric
    ELSE
       = fabrow(@lcfab,'*')
       &lcFabObj = lcFab
    ENDIF
 ELSE
    &lcFabObj = ''
 ENDIF
 SET ORDER TO FABRIC IN fabric
 IF llusebyme
    USE IN fabric
 ENDIF
*
PROCEDURE lfsrvsty
 PARAMETER lcparm
 DO CASE
    CASE lcparm='S'
       USE IN 0 (gcdatadir+'Style') AGAIN ALIAS style_x ORDER Style
       SELECT style
       SET ORDER TO Cstyle
       SET RELATION TO style.style INTO style_x
       GOTO TOP IN style
    CASE lcparm='R'
       USE IN style_x
       SELECT style
       SET ORDER TO STYLE
       llclearsty = .F.
 ENDCASE
*
FUNCTION lfstysum
 PARAMETER lcsty, lccomp, lnaddtovar
 PRIVATE lnstyrec
 lntotcomp = 0
 IF RECCOUNT('STYLE')<>0
    lnstyrec = RECNO('STYLE')
    SELECT style_x
    SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) == ALLTRIM(lcSty)
    SELECT style
    IF BETWEEN(lnstyrec, 1, RECCOUNT())
       GOTO lnstyrec
    ENDIF
    DO CASE
       CASE lnaddtovar=1
          lno_t_s = lntotcomp
       CASE lnaddtovar=2
          lno_t_s = lno_t_s+lntotcomp
       CASE lnaddtovar=3
          lno_t_s = lno_t_s-lntotcomp
    ENDCASE
 ENDIF
 RETURN INT(lntotcomp)
*
FUNCTION lfcolltime
 PARAMETER lcstart, lcend
 lnsthour = IIF(VAL(LEFT(lcstart, 2))=0, VAL(LEFT(lcstart, 2))+24, VAL(LEFT(lcstart, 2)))
 lnendhour = IIF(VAL(LEFT(lcend, 2))=0, VAL(LEFT(lcend, 2))+24, VAL(LEFT(lcend, 2)))
 lnstart = 3600*lnsthour+60*VAL(SUBSTR(lcstart, 4, 2))+VAL(RIGHT(lcstart, 2))
 lnend = 3600*lnendhour+60*VAL(SUBSTR(lcend, 4, 2))+VAL(RIGHT(lcend, 2))
 RETURN (lnend-lnstart)
*
PROCEDURE lfvsign
 CLEAR READ
*
PROCEDURE lfvsortby
 lcsortn = IIF(lcrpsort='S', 'Style', IIF(lcrpsort='G', 'StyGrp', 'Pattern'))
 SELECT (lctempsty)
 SET ORDER TO TAG &lcSortN
*
PROCEDURE lfheader
 pageno = pageno+1
 lcrpoptitl = TRIM(lcrpoptitl)
 r_title = TRIM(r_title)
 x1 = ((130-(LEN(TRIM(syccomp.ccom_name))))/2)
 x2 = ((130-(LEN(r_title)))/2)
 x3 = ((130-(LEN(lcrpoptitl)))/2)
 @ 01, 000 SAY 'ICKRA2100'
 @ 01, x1 SAY syccomp.ccom_name
 @ 01, 120 SAY DATE()
 @ 01, 129 SAY '~'
 @ 02, 000 SAY TIME()
 @ 02, x2 SAY r_title
 @ 02, 120 SAY 'PAGE#'
 @ 02, 126 SAY STR(pageno, 4)
 @ 03, x3 SAY lcrpoptitl
 IF lcrpsort<>'S'
    IF lcrpsort='G'
       @ 04, 00 SAY 'STYLE GROUP:'+cstygroup+'-'+gfcoddes(cstygroup,'CSTYGROUP')
       @ 05, 00 SAY REPLICATE('*', 132)
    ELSE
       @ 04, 00 SAY 'STYLE PATTERN:'+lcpattern
       @ 05, 00 SAY REPLICATE('*', 132)
    ENDIF
    row = 6
 ELSE
    @ 04, 00 SAY REPLICATE('*', 132)
    row = 5
 ENDIF
 @ row, 00 SAY UPPER(ALLTRIM(lcnonmajt))+SPACE(2)+UPPER(ALLTRIM(lcnonmajt))+' DESCR.... GROUP   PRICE    SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8   NET OTS'
 row = row+1
 @ row, 00 SAY REPLICATE('-', 131)
 row = row+1
