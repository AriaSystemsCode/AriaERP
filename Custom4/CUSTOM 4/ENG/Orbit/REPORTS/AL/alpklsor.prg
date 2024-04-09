*** 
*** ReFox XII  #UK122952  David Stephenson  Aria Systems Ltd [VFP90]
***
 STORE SPACE(0) TO lcdelnot1, lcdelnot2, lcdelnot3, lcdelnot4
 STORE 0 TO lnclrlngl, lnclrposgl, lnstylngl, lnstyposgl, lnscalngl, lnscaposgl
 = lfchkstrct()
 llprntmsg = .F.
 lcemail = gfgetmemvar('M_CEMAIL', oariaapplication.activecompanyid)
 llmscale = gfgetmemvar('M_USEEXSSC')
 IF  .NOT. USED('contact')
    = gfopentable('contact', 'contact', 'SH')
 ENDIF
 IF  .NOT. USED('POSHDR')
    = gfopentable('POSHDR', 'POSHDR', 'SH')
 ENDIF
 IF  .NOT. USED('POSLN')
    = gfopentable('POSLN', 'POSLNS', 'SH')
 ELSE
    SELECT posln
    = gfsetorder('POSLNS')
 ENDIF
 IF  .NOT. USED('PIKTKT')
    = gfopentable('PIKTKT', 'PIKTKT', 'SH')
 ENDIF
 IF  .NOT. USED('INVHDR')
    = gfopentable('INVHDR', 'INVHDR', 'SH')
 ENDIF
 PRIVATE lcalasphon, lcphonuser
 lcalasphon = SELECT(0)
 SELECT (lccompinfo)
 lcphoncomp = ccom_phon 
*T20130604.0003 - AL - error emailing Pack List [start] open in another alias to keep indexes
* = gfopentable('SYUUSER', 'CUSER_ID', "SH")
IF !USED('SYUUSER_A')
   = gfopenTable(oariaapplication.syspath+'SYUUSER', 'CUSER_ID', "SH","SYUUSER_A")
ENDIF   
*T20130604.0003 - AL - error emailing Pack List [end  ] open in another alias to keep indexes
 
*T20130604.0003 - AL - error emailing Pack List [start] use the syuuser_A alias instead
* SELECT syuuser
 SELECT syuuser_A
*T20130604.0003 - AL - error emailing Pack List [end  ] 

 gfseek(oariaapplication.user_id)
 lcphonuser = cusr_phon
 SELECT (lcalasphon)
 DIMENSION lafilestruct[1, 4]
 lafilestruct[1, 1] = 'Pack_No'
 lafilestruct[1, 2] = 'C'
 lafilestruct[1, 3] = 6
 lafilestruct[1, 4] = 0
 = gfcrttmp(lcpckprtup, @lafilestruct, "Pack_No", 'Invoice', .T.)
 lfgetallordl()
 = lfprtpack()
 SELECT (lcpckprtup)
 SCAN
    IF lopack_hdr.SEEK(EVAL(lcpckprtup+'.Pack_No')) .AND. EMPTY(&lctemppack_hdr..dshipdate)
       lopack_hdr.replace("DSHIPDATE WITH oAriaApplication.SystemDate")
    ENDIF
 ENDSCAN
 lopack_hdr.tableupdate()
 USE IN (lcpckprtup)
 = lfbastoclr(lcadstygrp, 'F')
 RETURN
ENDPROC
**
PROCEDURE lfbastoclr
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
FUNCTION lfdelphon
 PARAMETER lcreturn
 IF ASCAN(lacompadd, "Phone# : ")>0
    lnpos = ASUBSCRIPT(lacompadd, ASCAN(lacompadd, "Phone# : "), 1)
    lacompadd[lnpos, 1] = SPACE(0)
 ENDIF
 RETURN ""
ENDFUNC
**
PROCEDURE lfgrpsetes
 PARAMETER llreturn
 IF lcrpselcby="P"
    llendgroup = .F.
    RETURN
 ENDIF
ENDPROC
**
FUNCTION lfnonmjdes
 PARAMETER llreturn
 PRIVATE lni, lctemp, lcstyle, lcnonmjdes, lnalias
 STORE '' TO lctemp, lcnonmjdes, lnalias
 lnalias = SELECT()
 SELECT (lcpacktmp)
 lcstyle = &lcpaklntmp..STYLE
 lni = 0
 FOR lni = lnmajseg+1 TO ALEN(lamajsegs, 1)
    lctemp = ''
    DO CASE
       CASE lamajsegs(lni, 1)$"FOTQ"
          IF SEEK(STR(lni, 1)+SUBSTR(lcstyle, lamajsegs(lni, 4), LEN(lamajsegs(lni, 3))), "ICSEGVAL")
             lctemp = ALLTRIM(icsegval.cisgvalsd)
          ENDIF
       CASE lamajsegs(lni, 1)$"ZCDG"
          DO CASE
             CASE lamajsegs(lni, 1)="Z"
                lccodeexpr = "SEASON"
             CASE lamajsegs(lni, 1)="C"
                lccodeexpr = "COLOR"
             CASE lamajsegs(lni, 1)="D"
                lccodeexpr = "CDIVISION"
             OTHERWISE
                lccodeexpr = "CSTYGROUP"
          ENDCASE
          lctemp = ALLTRIM(gfcoddes(SUBSTR(lcstyle, lamajsegs(lni, 4), LEN(lamajsegs(lni, 3))), lccodeexpr, .F.))
       OTHERWISE
          IF SEEK("S"+SUBSTR(lcstyle, lamajsegs(lni, 4), LEN(lamajsegs(lni, 3))), lcscalefile)
             lctemp = ALLTRIM(&lcscalefile..cscl_desc)
          ENDIF
    ENDCASE
    lcnonmjdes = IIF(EMPTY(lcnonmjdes), lctemp, lcnonmjdes+IIF(EMPTY(lctemp), '', '-')+lctemp)
 ENDFOR
 lcstyle = IIF(lnextscpos=0, lcstyle, LEFT(lcstyle, LEN(lcstyle)-lnextsclen))
 lcstyleexp = lcstyle+' '+lcnonmjdes
 SELECT (lnalias)
 RETURN ''
ENDFUNC
**
FUNCTION lfspckln16
*!*	 PARAMETER llreturn
 PRIVATE lnSpckLn, lncount, lccount, lcalias
 STORE '' TO lcstyls, lncount, lccount
 DIMENSION latemp[16], laspcktemp[16]
 STORE "" TO laspcktemp, latemp
 lcalias = SELECT(0)
 IF !lospck_lin.SEEK( 'S' + &lcPakLnTmp..account + &lcPakLnTmp..STYLE)
	 SELECT (lcalias)
	 RETURN ''
 ENDIF	
 IF &lctempspck_lin..totqty = 0
	 laspcktemp[1] ='SKU #:' + &lctempspck_lin..pack_id
	 SELECT (lcalias)
	 RETURN ''
 ENDIF

 =SEEK('S'+ left(&lcPakLnTmp..ScalDL,2),lcscalefile)
 =lospck_lin.SEEK( 'S' + &lcPakLnTmp..account + left(&lcPakLnTmp..STYLE,lnscaposgl-1)+&lcscalefile..scale)
 lnSpckLn=recno(lctempspck_lin)
 FOR lncount = 1 TO 8
	lccount = STR(lncount, 1)
    IF  .NOT. EMPTY(EVALUATE(lcPakLnTmp+'.Qty'+lccount))
    	select (lctempspck_lin)
    	goto lnSpckLn
    	scan rest for type+account+style='S' + &lcPakLnTmp..account + left(&lcPakLnTmp..STYLE,lnscaposgl-1)+&lcscalefile..scale
			IF !EMPTY(&lctempspck_lin..qty&lccount)
				latemp[lncount] = &lcscalefile..sz&lccount + ':' + &lctempspck_lin..pack_id
			ENDIF
		endscan
    ENDIF
 ENDFOR
 SKIP 1 IN (lcscalefile)
 IF  !EOF(lcscalefile) .AND. (LEFT(&lcscalefile..scale,2)==LEFT(EVAL(lcPakLnTmp+'.ScalDL'),2))
	 =lospck_lin.SEEK( 'S' + &lcPakLnTmp..account + left(&lcPakLnTmp..STYLE,lnscaposgl-1)+&lcscalefile..scale)
	 lnSpckLn=recno(lctempspck_lin)
	 FOR lncount = 9 TO 16
		lccount = alltrim(STR(lncount))
		lci=alltrim(STR(lncount-8))
	    IF  .NOT. EMPTY(EVALUATE(lcPakLnTmp+'.Qty'+lccount))
	    	select (lctempspck_lin)
	    	goto lnSpckLn
	    	scan rest for type+account+style='S' + &lcPakLnTmp..account + left(&lcPakLnTmp..STYLE,lnscaposgl-1)+&lcscalefile..scale
				IF !EMPTY(&lctempspck_lin..qty&lci)
					latemp[lncount] = alltrim(&lcscalefile..sz&lci)+ ' : ' + alltrim(&lctempspck_lin..pack_id)
				ENDIF
			endscan
	    ENDIF
	 ENDFOR
 endif
 lnntpty = 1
 FOR i = 1 TO 16
    IF  .NOT. EMPTY(latemp(i))
    	lcI=alltrim(str(lnntpty))
       repl sku&lcI with latemp(i) in (lcPakLnTmp)
       lnntpty = lnntpty+1
    ENDIF
 ENDFOR
 SELECT (lcalias)
 RETURN ''
ENDFUNC
**
FUNCTION lfdelnote
 PARAMETER lcreturn
 STORE SPACE(0) TO lcdelnot1, lcdelnot2, lcdelnot3, lcdelnot4
 PRIVATE lcalasdelv
 lcalasdelv = SELECT(0)
 lnmemoset = SET('MEMOWIDTH')
 SET MEMOWIDTH TO 100
 IF lonotepad.seek("T"+"DELIVERY")
    lcdelnot1 = MLINE(&lctempnotepad..mnotes,1)
    lcdelnot2 = MLINE(&lctempnotepad..mnotes,2)
 ENDIF
 IF lonotepad.seek("T"+"DELIVERY1")
    lcdelnot3 = MLINE(&lctempnotepad..mnotes,1)
    lcdelnot4 = MLINE(&lctempnotepad..mnotes,2)
 ENDIF
 SELECT (lcalasdelv)
 SET MEMOWIDTH TO lnmemoset
 RETURN ''
ENDFUNC
**
PROCEDURE lfprtpack
 PRIVATE lnolsals, lcoldpktmp, lcevlkydl, lcpack_no, lltofollow, lcpochk, lcpochk2, lcopenpo
 PRIVATE lchldscal, lckey, lcstyclr, lcvalstclr, lcscalval, lccdm1val, lcnewpack, ldavalbl
 PRIVATE lcchngsty, lcaccprnds
 STORE SPACE(0) TO lchldscal, lckey, lcstyclr, lcvalstclr, lcscalval, lccdm1val
 STORE SPACE(0) TO lcpack_no, lcnewpack, lcopenpo
 STORE {} TO ldavalbl
 lnolsals = SELECT(0)
 STORE 0 TO lnvldlnno, lnnewln
 lcoldpktmp = lcpaklntmp
 SELECT posln
 SELECT (lcpacktmp)
 SET RELATION TO
 = lfcreattmp()
 = lfadmsdrec()
 SELECT (lclinfile)
 LOCATE
 lcevlkydl = EVALUATE(KEY())
 SCAN
    lopack_hdr.seek(pack_no)
    =SEEK('M' + &lctemppack_hdr..account,lccustomer)  
    lltofollow = &lccustomer..ctofollow
    =SEEK(IIF(EMPTY(&lctemppack_hdr..STORE) , 'M' + &lctemppack_hdr..account ,'S' + &lctemppack_hdr..account + &lctemppack_hdr..STORE),lccustomer)
    WAIT WINDOW NOWAIT 'Selecting Records For The Report ...'+pack_no
    lcvalstclr = SUBSTR(style, lnstyposgl, lnstylngl)+"-"+SUBSTR(style, lnclrposgl, lnclrlngl)
    lnnewln = nordlineno
    SCATTER MEMO MEMVAR
    lckey = SUBSTR(style, lnscaposgl, 2)
    IF  .NOT. (lckey$lchldscal)
       = lfgetsizes()
       lchldscal = lchldscal+IIF(EMPTY(lchldscal), "", ",")+lckey
    ENDIF
    IF lcstyclr<>lcvalstclr .OR. lcnewpack<>pack_no .OR. lnvldlnno<>lnnewln
       = lfinsertrc()
    ELSE
       = lfnsrtfnd()
    ENDIF
    lcstyclr = lcvalstclr
    lnvldlnno = lnnewln
 ENDSCAN
 SELECT (lcadstygrp)
 LOCATE
 lcpaklntmp = lcadstygrp
 = lfupdgroup()
 SELECT (lcpacktmp)
 SET RELATION TO IIF(EMPTY(STORE),'M','S') + account + STORE INTO &lccustomer , "O" + ORDER + STORE     INTO &lcordlntmp, "B" + ORDER             INTO &lcnotepad  , "O" + ORDER             INTO &lcordhdr  , invoice                 INTO &lcinvlntmp
 SELECT (lcpaklntmp)
 SET RELATION TO pack_no INTO (lcpacktmp)
 DELETE ALL FOR totqty=0
 LOCATE
 DELETE ALL FOR EMPTY(folowrec) .AND. line_no=0
 llstrttof = .F.
 STORE SPACE(0) TO lcpochk, lcpochk2, lcnewpckno, lcchngsty, lcaccprnds
 SCAN
    IF lcpochk2<>pack_no .AND.  .NOT. EMPTY(lcpochk2) .AND. llrpprnnot
       SKIP -1
       REPLACE &lcpaklntmp..llprnntpd WITH .T.
       SKIP 1
    ENDIF
    IF lcpochk2<>pack_no
       llstrttof = .F.
    ENDIF
    IF (lcchngsty<>SUBSTR(style, lnstyposgl, lnstylngl) .AND.  .NOT. EMPTY(lcchngsty)) .OR. (lcchngsty==SUBSTR(style, lnstyposgl, lnstylngl) .AND.  .NOT. EMPTY(lcchngsty) .AND. (lcaccprnds<>account))
       SKIP -1
       IF account<>"ZZZZZ"
          REPLACE &lcpaklntmp..llprnsdc WITH .T.
       ENDIF
       REPLACE &lcpaklntmp..llprnlin WITH .T.
       SKIP 1
    ENDIF
    IF lopack_hdr.SEEK(&lcpaklntmp..pack_no) AND SEEK("O" + &lctemppack_hdr..ORDER + STR(nordlineno,6) , lcordlntmp,lcordlntmp)
       REPLACE cpackcolor WITH &lcordlntmp..employee
       && dragons
*!*	       IF lostyle.SEEK( &lcpaklntmp..STYLE)
*!*	          REPLACE styldesc WITH &lctempstyle..desc1, llprnsdc WITH .T.
*!*	       ENDIF
       REPLACE styldesc WITH &lcordlntmp..desc1, llprnsdc WITH .T.
       && dragons
    ENDIF
    IF lcpochk<>pack_no .AND. account="ZZZZZ"
       REPLACE &lcpaklntmp..prntofol WITH .T.
    ENDIF
    IF account="ZZZZZ" .AND. totqty>0
       REPLACE &lcpaklntmp..llprnlin WITH .T.
    ENDIF
    IF &lcpaklntmp..prntofol   
       llstrttof = &lcpaklntmp..prntofol  
    ENDIF
    IF llstrttof AND &lcadstygrp..cfollow   
    ENDIF
    IF llrpprnnot .AND. pack_no # lcpochk2     AND lopack_hdr.SEEK(lcpochk2)           AND lonotepad.SEEK("B"+&lctemppack_hdr..ORDER)
       SKIP -1
       REPLACE llsmpad WITH .T.            , notesm  WITH &lctempnotepad..mnotes
       SKIP
    ENDIF
    for lni=1 to 16
    	lcI=alltrim(str(lni))
    	repl &lcpaklntmp..sku&lci with ''
    endfor
    =lfspckln16()
    lcpochk = IIF(account="ZZZZZ", pack_no, "")
    lcpochk2 = pack_no
    lcchngsty = SUBSTR(style, lnstyposgl, lnstylngl)
    lcaccprnds = account
 ENDSCAN
 IF (lcchngsty<>SUBSTR(style, lnstyposgl, lnstylngl) .AND.  .NOT. EMPTY(lcchngsty)) .OR. (lcchngsty==SUBSTR(style, lnstyposgl, lnstylngl) .AND.  .NOT. EMPTY(lcchngsty) .AND. (lcaccprnds<>account))
    SKIP -1
    IF account<>"ZZZZZ"
       REPLACE &lcpaklntmp..llprnsdc WITH .T.
    ENDIF
    REPLACE &lcpaklntmp..llprnlin WITH .T.
    SKIP 1
 ENDIF
 GOTO BOTTOM
 IF !&lcpaklntmp..llprnsdc
    REPLACE &lcpaklntmp..llprnsdc WITH .T.
 ENDIF
 IF llrpprnnot
    GOTO BOTTOM
    IF llrpprnnot .AND. lopack_hdr.SEEK(pack_no) .AND. lonotepad.SEEK("B"+&lctemppack_hdr..ORDER)
       REPLACE llsmpad WITH .T.            , notesm  WITH ALLTRIM(&lctempnotepad..mnotes)
    ENDIF
    REPLACE &lcpaklntmp..llprnntpd WITH .T.
 ENDIF
 LOCATE
 loogscroll.ccrorientation = 'P'
 INDEX ON pack_no+cpackcolor+folowrec+cgroupkey+cdelivery+SUBSTR(style, lnstyposgl, lnstylngl)+ALLTRIM(STR(line_no))+ALLTRIM(STR(indxdm2)) TAG 'EmpIndx' ADDITIVE
 SET ORDER TO EmpIndx
 lcdltset = SET("Deleted")
 SET DELETED ON
 lclcdim1 = SPACE(5)
 lcstyflw = SPACE(lnscaposgl-1)
 lcpacknum = SPACE(6)
 lnrecnumber = 0
 SCAN FOR cfollow .AND. totqty>0 .AND.  .NOT. DELETED()
    IF (style<>lcstyflw .AND. IIF( .NOT. EMPTY(lcdim1), lcdim1<>lclcdim1, .T.)) .OR. pack_no<>lcpacknum
       lclcdim1 = lcdim1
       lnrecnumber = RECNO()
       lcstyflw = SUBSTR(style, lnstyposgl, lnscaposgl-1)
       lcpacknum = pack_no
       STORE 0 TO lnqty1, lnqty2, lnqty3, lnqty4, lnqty5, lnqty6, lnqty7, lnqty8, lnqty9, lnqty10, lnqty11, lnqty12, lnqty13, lnqty14, lnqty15, lnqty16, lntotqty
       SCAN FOR cfollow .AND. style=lcstyflw .AND. totqty>0 .AND. lcdim1=lclcdim1 .AND. pack_no=lcpacknum .AND.  .NOT. DELETED()
          FOR lnt = 1 TO 16
             lct = ALLTRIM(STR(lnt))
             lnqty&lct = lnqty&lct + qty&lct.
             lntotqty = lntotqty + qty&lct.
          ENDFOR
          IF RECNO()<>lnrecnumber
             DELETE
          ENDIF
       ENDSCAN
       IF BETWEEN(lnrecnumber, 1, RECCOUNT())
          GOTO lnrecnumber
          FOR lnt = 1 TO 16
             lct = ALLTRIM(STR(lnt))
             REPLACE qty&lct. WITH lnqty&lct
          ENDFOR
          REPLACE totqty WITH lntotqty
       ENDIF
    ENDIF
 ENDSCAN
 LOCATE
 llprnttot = .F.
 lcpack = ""
 lncntr = 0
 SCAN
    IF lcpack<>pack_no
       lcpack = pack_no
       lcemploye = cpackcolor
       lnrecno = RECNO()
       APPEND BLANK
       REPLACE pack_no WITH lcpack, account WITH 'ZZZZZ'
       IF  .NOT. EMPTY(lcemploye)
          REPLACE cpackcolor WITH 'ZZZZZZZZZZ'
       ELSE
          REPLACE folowrec WITH 'YYYY'
       ENDIF
       GOTO lnrecno
       SCAN REST FOR folowrec='ZZZZ' .AND. cpackcolor<>'ZZZZZZZZZZ' .AND.  .NOT. EMPTY(cpackcolor) WHILE pack_no=lcpack
          lnrecno = RECNO()
          SCATTER MEMO MEMVAR
          IF lncntr=0
             m.prntofol = .T.
             lncntr = lncntr+1
          ENDIF
          m.cpackcolor = 'ZZZZZZZZZZ'
          APPEND BLANK
          GATHER MEMO MEMVAR
          GOTO lnrecno
       ENDSCAN
    ENDIF
 ENDSCAN
 SET ORDER TO EmpIndx
 LOCATE
 DO gfdispre WITH EVALUATE('lcFormName')
 llalpaklst = .F.
 WAIT CLEAR
 SET DEVICE TO SCREEN
 lcpaklntmp = lcoldpktmp
 SELECT (lclinfile)
 SET RELATION TO
 DELETE ALL FOR cowner=="BBBBVVVVDDDDRRRR"
 SELECT (lnolsals)
ENDPROC
**
PROCEDURE lfgetsizes
 PRIVATE lcalias
 lcalias = ALIAS()
 PRIVATE lni, lnhdr, lnscalrec, lncontsrt
 STORE 0 TO lni, lnhdr, lnscalrec, lncontsrt
 SELECT (lctempscale)
 lnscalrec = IIF(EOF(lctempscale), 0, RECNO(lctempscale))
 LOCATE
 IF loscale.seek("S"+SUBSTR(style, lnscaposgl, 2))
    lncontsrt = 1
    SCAN FOR type+scale+prepak="S"+lckey
       SCATTER MEMO MEMVAR
       SELECT (lctmpsizes)
       SET ORDER TO (lctmpsizes)
       IF SEEK(lckey + &lctempscale..cdim1 , lctmpsizes)
          IF &lctmpsizes..cdim1 == &lctempscale..cdim1
             FOR lncrttmp = 1 TO 8
                lcnumsiz = "Sz"+ALLTRIM(STR(lncrttmp+8))
                lcsizfld = "Sz"+ALLTRIM(STR(lncrttmp))
                IF !EMPTY(&lctempscale..&lcsizfld)
                   REPLACE &lctmpsizes..&lcnumsiz WITH &lctempscale..&lcsizfld , &lctmpsizes..cdim1     WITH &lctempscale..cdim1
                ENDIF
             ENDFOR
          ELSE
             lncontsrt = lncontsrt+1
             APPEND BLANK
             GATHER MEMO MEMVAR
             REPLACE &lctmpsizes..scalfld WITH LEFT(&lctempscale..SCALE,2) , &lctmpsizes..cdim1   WITH &lctempscale..cdim1         , &lctmpsizes..indxdm2 WITH lncontsrt
             REPLACE orgscl WITH  &lctempscale..SCALE
          ENDIF
       ELSE
          APPEND BLANK
          GATHER MEMO MEMVAR
          REPLACE &lctmpsizes..scalfld WITH LEFT(&lctempscale..SCALE,2) , &lctmpsizes..indxdm2 WITH 1
          REPLACE orgscl WITH  &lctempscale..SCALE
       ENDIF
    ENDSCAN
 ENDIF
 SELECT (lctmpsizes)
 SET ORDER TO SortScal
 REPLACE &lctmpsizes..llprnsdc WITH .T.
 SELECT (lctempscale)
 IF lnscalrec<>0
    GOTO lnscalrec IN (lctempscale)
 ENDIF
 SELECT (lcalias)
ENDPROC
**
PROCEDURE lfinsertrc
 PRIVATE lnprvals, lchlddim, lckeyrec, lcoldordr, lcoldordr2
 STORE SPACE(0) TO lchlddim, lckeyrec, lcoldordr, lcoldordr2
 PRIVATE llrtrnvl
 STORE .F. TO llrtrnvl
 lnprvals = SELECT(0)
 = lopack_hdr.seek(m.pack_no)
 lcpack_no = &lctemppack_hdr..pack_no
 IF lcnewpack<>lcpack_no
    lcnewpack = lcpack_no
 ENDIF
 =SEEK(IIF(EMPTY(&lctemppack_hdr..STORE) , 'M' + &lctemppack_hdr..account ,'S' + &lctemppack_hdr..account + &lctemppack_hdr..STORE),lccustomer)
 = SEEK(style, lcstylefile)
 =SEEK('S'+&lcstylefile..SCALE,lctempscale)
 SELECT (lcordlntmp)
 lcoldordr = ORDER()
 SET ORDER TO (lcordlnindtmp)
 SELECT (lcadstygrp)
 SET RELATION TO "O" + &lctemppack_hdr..ORDER + STORE + STYLE INTO (lcordlntmp)
 lcoldordr2 = ORDER()
 SET ORDER TO lcStyFond
 =lopack_hdr.SEEK(EVAL(lclinfile+'.Pack_No')) AND SEEK("O" + &lctemppack_hdr..ORDER + STR(&lclinfile..nordlineno,6) , lcordlntmp,lcordlntmp)
 IF SEEK(EVAL(lclinfile+'.PACK_NO') + SUBSTR(EVAL(lclinfile+'.STYLE'),lnstyposgl,lnclrposgl+lnclrlngl-1)+EVAL(lcordlntmp+'.EMPLOYEE')) AND !EMPTY(&lctempscale..cdim1)
    = lfnsrtfnd()
    SELECT (lcadstygrp)
    SET ORDER TO TAG &lcoldordr2
    RETURN
 ENDIF
 SET ORDER TO TAG &lcoldordr2
 APPEND BLANK
 REPLACE STYLE     WITH &lclinfile..STYLE , styldesc  WITH &lcstylefile..desc1       , scaldl    WITH &lcstylefile..SCALE       , lcdim1    WITH &lctempscale..cdim1       , pack_no   WITH lcpack_no         , llprnscal WITH .T.               , cgroupkey WITH 'zzzzzz'
 REPLACE nordlineno WITH &lclinfile..nordlineno
 lnordlinno = &lclinfile..nordlineno
 =SEEK( LEFT(&lcstylefile..SCALE,2)+&lcadstygrp..lcdim1 , lctmpsizes , lctmpsizes )
 FOR lncrttmp = 1 TO 16
    lcnumsiz = "Sz"+ALLTRIM(STR(lncrttmp))+"dl"
    lcsizfld = "Sz"+ALLTRIM(STR(lncrttmp))
    IF !EMPTY(&lctmpsizes..&lcsizfld)
       REPLACE &lcadstygrp..&lcnumsiz WITH &lctmpsizes..&lcsizfld
    ENDIF
 ENDFOR
 GATHER MEMO MEMVAR FIELDS EXCEPT Qty*
 FOR lnscl = 1 TO 1
    lcsizscl = "Sz"+ALLTRIM(STR(lnscl))
    FOR lnall = 1 TO 16
       lcadsscl = "Sz"+ALLTRIM(STR(lnall))+"DL"
       IF &lctempscale..&lcsizscl == &lcadstygrp..&lcadsscl
          FOR lnfill = lnscl TO 8
             lcsizscl = "Sz"+ALLTRIM(STR(lnfill))
             lcqtyval = "Qty"+ALLTRIM(STR(lnfill))
             lcadsscl = "Qty"+ALLTRIM(STR(lnfill+lnall-1))
             IF !EMPTY(&lctempscale..&lcsizscl)
                REPLACE &lcadstygrp..&lcadsscl WITH m.&lcqtyval
             ENDIF
          ENDFOR
          EXIT
       ENDIF
    ENDFOR
 ENDFOR
 IF lopack_hdr.seek(m.pack_no)
    =SEEK(IIF(EMPTY(&lctemppack_hdr..STORE),'M','S') + &lctemppack_hdr..account + &lctemppack_hdr..STORE,lccustomer)
    REPLACE &lcadstygrp..account   WITH &lctemppack_hdr..account , &lcadstygrp..STORE     WITH &lctemppack_hdr..STORE   , &lcadstygrp..cdelivery WITH IIF(&lccustomer..lldelivery,'Y','N')
 ENDIF
 lchlddim = &lcadstygrp..lcdim1
 lckeyrec = EVALUATE(KEY())
 IF SEEK(SUBSTR(style, lnscaposgl, 2), lctmpsizes)
    SELECT (lctmpsizes)
    SCAN REST WHILE scalfld+ALLTRIM(STR(indxdm2))=SUBSTR(style, lnscaposgl, 2)
       IF lchlddim == &lctmpsizes..cdim1
          = SEEK(lckeyrec, lcadstygrp)
          REPLACE &lcadstygrp..indxdm2  WITH &lctmpsizes..indxdm2  , &lcadstygrp..llprnsdc WITH &lctmpsizes..llprnsdc
          IF &lctmpsizes..indxdm2 == 1
             REPLACE &lcadstygrp..llprnlin WITH .T.
             IF lfopenpo()
                REPLACE &lcadstygrp..ponofolo WITH lcopenpo , &lcadstygrp..datavlbl WITH ldavalbl
             ENDIF
          ELSE
             REPLACE &lcadstygrp..llprnlin WITH .F.
          ENDIF
          FOR lncrttmp = 1 TO 16
             lcnumsiz = "Sz"+ALLTRIM(STR(lncrttmp))+"dl"
             lcsizfld = "Sz"+ALLTRIM(STR(lncrttmp))
             IF !EMPTY(&lctmpsizes..&lcsizfld)
                REPLACE &lcadstygrp..&lcnumsiz WITH &lctmpsizes..&lcsizfld
             ENDIF
          ENDFOR
       ELSE
          SELECT (lcadstygrp)
          SCATTER MEMO MEMVAR
          m.STYLE = SUBSTR(m.STYLE,1,lnscaposgl-1)+&lctmpsizes..orgscl
          APPEND BLANK
          GATHER MEMO MEMVAR FIELDS EXCEPT Qty*, TotQty
          REPLACE cfollow WITH .F.
          REPLACE &lcadstygrp..account  WITH &lctemppack_hdr..account      , &lcadstygrp..folowrec WITH SPACE(0)              , &lcadstygrp..lcdim1   WITH &lctmpsizes..cdim1    , &lcadstygrp..indxdm2  WITH &lctmpsizes..indxdm2  , &lcadstygrp..llprnsdc WITH &lctmpsizes..llprnsdc
          IF &lctmpsizes..indxdm2 == 1
             REPLACE &lcadstygrp..llprnlin WITH .T.
             IF lfopenpo()
                REPLACE &lcadstygrp..ponofolo WITH lcopenpo , &lcadstygrp..datavlbl WITH ldavalbl
             ENDIF
          ELSE
             REPLACE &lcadstygrp..llprnlin WITH .F.
          ENDIF
          FOR lncrttmp = 1 TO 16
             lcnumsiz = "Sz"+ALLTRIM(STR(lncrttmp))+"dl"
             lcsizfld = "Sz"+ALLTRIM(STR(lncrttmp))
             IF !EMPTY(&lctmpsizes..&lcsizfld)
                REPLACE &lcadstygrp..&lcnumsiz WITH &lctmpsizes..&lcsizfld
             ENDIF
          ENDFOR
       ENDIF
       IF lltofollow
          SELECT (lcadstygrp)
          SCATTER MEMO MEMVAR
          APPEND BLANK
          GATHER MEMO MEMVAR FIELDS EXCEPT Qty*, TotQty
          REPLACE &lcadstygrp..account  WITH "ZZZZZ" , &lcadstygrp..cfollow  WITH .T.     , &lcadstygrp..folowrec WITH "ZZZZ"
          IF lchlddim == &lctmpsizes..cdim1
             =SEEK("O" + &lctemppack_hdr..ORDER + STORE + STYLE+STR(nordlineno,6) , lcordlntmp)
             PRIVATE lcsizscl, lcadsscl
             FOR lnscl = 1 TO 1
                lcsizscl = "Sz"+ALLTRIM(STR(lnscl))
                FOR lnall = 1 TO 16
                   lcadsscl = "Sz"+ALLTRIM(STR(lnall))+"DL"
                   IF &lctempscale..&lcsizscl == &lcadstygrp..&lcadsscl
                      FOR lnfill = lnscl TO 8
                         lcsizscl = "Sz"+ALLTRIM(STR(lnfill))
                         lcqtyord = "Qty"+ALLTRIM(STR(lnfill))
                         lcpikqty = "Pik"+ALLTRIM(STR(lnfill))
                         lcadsscl = "Qty"+ALLTRIM(STR(lnfill+lnall-1))
                         IF !EMPTY(&lctempscale..&lcsizscl) AND (&lcordlntmp..&lcqtyord - &lcordlntmp..&lcpikqty) # 0
                            REPLACE &lcadstygrp..&lcadsscl WITH (&lcordlntmp..&lcqtyord - &lcordlntmp..&lcpikqty)
                         ENDIF
                      ENDFOR
                      EXIT
                   ENDIF
                ENDFOR
             ENDFOR
             REPLACE &lcadstygrp..totqty WITH &lcadstygrp..totqty + (&lcordlntmp..totqty - &lcordlntmp..totpik)
          ENDIF
       ENDIF
    ENDSCAN
 ENDIF
 SELECT (lcordlntmp)
 SET ORDER TO (lcoldordr)
 lcfolval = SUBSTR(lckeyrec, LEN(lckeyrec)-4, 4)
 SELECT (lcadstygrp)
 lcordrtag = ORDER()
 SET ORDER TO lcFrstRec
 lckeyrec = LEFT(lckeyrec, LEN(lckeyrec)-5)+SUBSTR(style, lnstyposgl, lnclrposgl+lnclrlngl-1)+lcfolval+"1"
 = SEEK(lckeyrec, lcadstygrp)
 lcorder = piktkt.order
 lcorder = ''
 IF gfseek(&lclinfile..pack_no,'PIKTKT')
    lcorder = piktkt.order
 ELSE
    lcorder = &lctemppack_hdr..ORDER
 ENDIF
 IF SEEK('O'+lcorder, lcordlntmp)
    SELECT (lcordlntmp)
    LOCATE REST WHILE cordtype+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+lcorder FOR STYLE = &lcadstygrp..STYLE AND nordlineno = lnordlinno 
    IF FOUND()
       REPLACE &lcadstygrp..price WITH &lcordlntmp..price
    ENDIF
 ENDIF
 SELECT (lcadstygrp)
 SET ORDER TO TAG &lcordrtag
 SELECT (lclinfile)
 SELECT (lnprvals)
ENDPROC
**
PROCEDURE lfupdgroup
 PRIVATE lnprvals
 lnprvals = SELECT(0)
 SELECT (lcpaklntmp)
 LOCATE
 lngroupkey = 1
 STORE pack_no TO lcpack_no, lcoldpiktk
 STORE '' TO lccurtpktk
 lcoldkey = account+cdelivery+store
 SCAN
    lopack_hdr.seek(pack_no)
    IF ((lcoldkey # account + cdelivery + STORE) .OR. (lccurtpktk # pack_no .AND. cdelivery = 'N'))  AND &lcadstygrp..account <> "ZZZZZ"
       lngroupkey = lngroupkey+1
       SELECT (lctmpgroup)
       APPEND BLANK
       REPLACE pack_no    WITH &lcpaklntmp..pack_no            , cgroupkey  WITH ALLTRIM(STR(lngroupkey))        , weightdl   WITH weightdl   + &lctemppack_hdr..weightdl  , noofcarton WITH noofcarton + &lctemppack_hdr..noofcarton, tot_pcs    WITH tot_pcs    + &lctemppack_hdr..tot_pcs   , dprintdate WITH &lctemppack_hdr..dshipdate             , consgment  WITH &lctemppack_hdr..consgment
       SELECT (lcpaklntmp)
    ENDIF
    REPLACE cgroupkey WITH ALLTRIM(STR(lngroupkey))
    IF lfremtoflw()
       REPLACE &lctmpgroup..llremantto WITH .T.
    ENDIF
    IF (lcoldpiktk<>pack_no .AND. lnoldgroup=lngroupkey) .OR. EOF(lctmpgroup)
       IF EOF(lctmpgroup)
          SELECT (lctmpgroup)
          APPEND BLANK
          REPLACE pack_no    WITH &lcpaklntmp..pack_no             , cgroupkey  WITH ALLTRIM(STR(lngroupkey))         , weightdl   WITH weightdl   + &lctemppack_hdr..weightdl   , noofcarton WITH noofcarton + &lctemppack_hdr..noofcarton , tot_pcs    WITH tot_pcs    + &lctemppack_hdr..tot_pcs    , dprintdate WITH &lctemppack_hdr..dshipdate               , consgment  WITH &lctemppack_hdr..consgment
          SELECT (lcpaklntmp)
       ELSE
          REPLACE &lctmpgroup..pack_no    WITH '******', &lctmpgroup..weightdl   WITH &lctmpgroup..weightdl   + &lctemppack_hdr..weightdl  , &lctmpgroup..noofcarton WITH &lctmpgroup..noofcarton + &lctemppack_hdr..noofcarton, &lctmpgroup..tot_pcs    WITH &lctmpgroup..tot_pcs    + &lctemppack_hdr..tot_pcs
       ENDIF
       IF lfremtoflw()
          REPLACE &lctmpgroup..llremantto WITH .T.
       ENDIF
    ENDIF
    lcoldkey = account+cdelivery+store
    STORE pack_no TO lcoldpiktk, lccurtpktk
    lnoldgroup = lngroupkey
    IF llrpprnnot .AND. pack_no # lcpack_no   AND lopack_hdr.SEEK(lcpack_no,'Pack_hdr')         AND lonotepad.SEEK("B"+&lctemppack_hdr..ORDER)
       lcpack_no = pack_no
       SKIP -1
       REPLACE llsmpad WITH .T.            , notesm  WITH &lctempnotepad..mnotes
       SKIP
    ENDIF
 ENDSCAN
 SELECT (lnprvals)
ENDPROC
**
PROCEDURE lfupdatprt
 IF oariaapplication.gcdevice<>'SCREEN'
    IF  .NOT. SEEK(pack_no, lcpckprtup)
       INSERT INTO (lcpckprtup) (pack_no) VALUES (&lctemppack_hdr..pack_no)
    ENDIF
 ENDIF
ENDPROC
**
FUNCTION lfremtoflw
 PRIVATE lnalias, llremains
 lnalias = SELECT(0)
 llremains = .F.
 IF SEEK('O'+&lctemppack_hdr..ORDER,lcordhdr)
    IF &lcordhdr..OPEN+&lcordhdr..ship > &lctemppack_hdr..tot_pcs
       llremains = .T.
    ENDIF
 ENDIF
 SELECT (lnalias)
 RETURN llremains
ENDFUNC
**
PROCEDURE lfaddfield
 PARAMETER lcstruarry, lcfldname, lcfldtype, lnfldlen, lnflddec
 lnfldpos  = ALEN(&lcstruarry,1) + IIF(TYPE('&lcStruArry') = 'L', 0 , 1 )
 DIMENSION &lcstruarry[lnfldpos , 18]
 &lcstruarry[lnfldpos , 1]	= lcfldname
 &lcstruarry[lnfldpos , 2]	= lcfldtype
 &lcstruarry[lnfldpos , 3]	= lnfldlen
 &lcstruarry[lnfldpos , 4]	= lnflddec
ENDPROC
**
PROCEDURE lfchkstrct
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
 DIMENSION laitemseg[1]
 = gfitemmask(@laitemseg)
 FOR lncount = 1 TO ALEN(laitemseg, 1)
    IF laitemseg(lncount, 1)='S'
       lnscalngl = LEN(laitemseg(lncount, 3))
       lnscaposgl = laitemseg(lncount, 4)
       EXIT
    ENDIF
 ENDFOR
ENDPROC
**
FUNCTION lfopenpo
 PRIVATE lcalaspo, lcevalkypo, lcstycheck
 STORE SPACE(0) TO lcopenpo, lcstycheck
 STORE {} TO ldavalbl
 STORE .F. TO llrtrnvl
 lcalaspo = SELECT(0)
 SELECT posln
 lcevalkypo = EVALUATE(KEY())
 =gfseek("0001"+&lcadstygrp..STYLE+'PP')
 SCAN REST WHILE cinvtype+STYLE+cbusdocu+cstytype+po+STR(LINENO,6)+trancd= "0001"+&lcadstygrp..STYLE+'PP' FOR gfseek('PP'+posln.po,'POSHDR') AND poshdr.STATUS = "O"
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
 = SEEK(lcevalkypo)
 SELECT (lcalaspo)
 RETURN llrtrnvl
ENDFUNC
**
PROCEDURE lfcreattmp
 SELECT (lclinfile)
 = AFIELDS(latmpstru)
 lntmpstru = ALEN(latmpstru, 1)
 = lfaddfield("laTmpStru", "StyGrop", "C", 6, 0)
 = lfaddfield("laTmpStru", "llSmPad", "L", 1, 0)
 = lfaddfield("laTmpStru", "NoteSm", "M", 10, 0)
 = lfaddfield("laTmpStru", "StylDesc", "C", 60, 0)
 = lfaddfield("laTmpStru", "ScalDL", "C", 3, 0)
 FOR lncrttmp = 1 TO 16
    lcnumsiz = ALLTRIM(STR(lncrttmp))
    = lfaddfield("laTmpStru", "Sz"+lcnumsiz+"DL", "C", 5, 0)
 ENDFOR
 FOR lncrttmp = 1 TO 8
    lcnumqty = ALLTRIM(STR(lncrttmp+8))
    = lfaddfield("laTmpStru", "Qty"+lcnumqty, "N", 6, 0)
 ENDFOR
 = lfaddfield("laTmpStru", "llPrnScal", "L", 1, 0)
 = lfaddfield("laTmpStru", "Price", "N", 7, 2)
 = lfaddfield("laTmpStru", "ACCOUNT", "C", 5, 0)
 = lfaddfield("laTmpStru", "STORE", "C", 8, 0)
 = lfaddfield("laTmpStru", "CDelivery", "C", 1, 0)
 = lfaddfield("laTmpStru", "CGroupKey", "C", 6, 0)
 = lfaddfield("laTmpStru", "Carrier", "C", 40, 0)
 = lfaddfield("laTmpStru", "llPrnSDc", "L", 1, 0)
 = lfaddfield("laTmpStru", "lcDim1", "C", 5, 0)
 = lfaddfield("laTmpStru", "IndxDm2", "N", 3, 0)
 = lfaddfield("laTmpStru", "llPrnLin", "L", 1, 0)
 = lfaddfield("laTmpStru", "Cfollow", "L", 1, 0)
 = lfaddfield("laTmpStru", "FolowRec", "C", 4, 0)
 = lfaddfield("laTmpStru", "PrnToFol", "L", 1, 0)
 = lfaddfield("laTmpStru", "Ponofolo", "C", 6, 0)
 = lfaddfield("laTmpStru", "DatAvlbl", "D", 8, 0)
 = lfaddfield("laTmpStru", "llPrnNtPd", "L", 1, 0)
 FOR lncrttmp = 1 TO 16
    lcnumsiz = ALLTRIM(STR(lncrttmp))
    = lfaddfield("laTmpStru", "SKU"+lcnumsiz, "C", 25, 0)
 ENDFOR
 FOR lni = 1 TO ALEN(latmpstru, 1)
    STORE '' TO latmpstru[lni, 7], latmpstru[lni, 8], latmpstru[lni, 9], latmpstru[lni, 10], latmpstru[lni, 11], latmpstru[lni, 12], latmpstru[lni, 13], latmpstru[lni, 14], latmpstru[lni, 15], latmpstru[lni, 16]
    STORE 0 TO latmpstru[lni, 17], latmpstru[lni, 18]
 ENDFOR
 = gfcrttmp(lcadstygrp, @latmpstru, "PACK_NO + Account + Store + cGroupkey  + CDelivery + ALLTRIM(STR(Line_No)) + FolowRec + ALLTRIM(STR(IndxDm2))", lcadstygrp, .T.)
 SELECT (lcadstygrp)
 INDEX ON pack_no+SUBSTR(style, lnstyposgl, lnclrposgl+lnclrlngl-1)+cpackcolor TAG lcstyfond ADDITIVE
 INDEX ON pack_no+account+store+cgroupkey+cdelivery+ALLTRIM(STR(line_no))+SUBSTR(style, lnstyposgl, lnclrposgl+lnclrlngl-1)+folowrec+ALLTRIM(STR(indxdm2)) TAG lcfrstrec ADDITIVE
 DIMENSION lafilestr[8, 4]
 lafilestr[1, 1] = 'CGroupKey'
 lafilestr[1, 2] = 'C'
 lafilestr[1, 3] = 6
 lafilestr[1, 4] = 0
 lafilestr[2, 1] = 'PACK_NO'
 lafilestr[2, 2] = 'C'
 lafilestr[2, 3] = 6
 lafilestr[2, 4] = 0
 lafilestr[3, 1] = 'weightdl'
 lafilestr[3, 2] = 'N'
 lafilestr[3, 3] = 13
 lafilestr[3, 4] = 2
 lafilestr[4, 1] = 'noofcarton'
 lafilestr[4, 2] = 'N'
 lafilestr[4, 3] = 8
 lafilestr[4, 4] = 0
 lafilestr[5, 1] = 'TOT_PCS'
 lafilestr[5, 2] = 'N'
 lafilestr[5, 3] = 8
 lafilestr[5, 4] = 0
 lafilestr[6, 1] = 'DPrintDate'
 lafilestr[6, 2] = 'D'
 lafilestr[6, 3] = 8
 lafilestr[6, 4] = 0
 lafilestr[7, 1] = 'consgment'
 lafilestr[7, 2] = 'C'
 lafilestr[7, 3] = 20
 lafilestr[7, 4] = 0
 lafilestr[8, 1] = 'llRemantTo'
 lafilestr[8, 2] = 'L'
 lafilestr[8, 3] = 1
 lafilestr[8, 4] = 0
 = gfcrttmp(lctmpgroup, @lafilestr, "CGroupKey", lctmpgroup, .T.)
 DIMENSION lafilestrucu[21, 4]
 lafilestrucu[1, 1] = 'ScalFld'
 lafilestrucu[1, 2] = 'C'
 lafilestrucu[1, 3] = 2
 lafilestrucu[1, 4] = 0
 lafilestrucu[2, 1] = 'IndxDm2'
 lafilestrucu[2, 2] = 'N'
 lafilestrucu[2, 3] = 3
 lafilestrucu[2, 4] = 0
 lafilestrucu[3, 1] = 'cDim1'
 lafilestrucu[3, 2] = 'C'
 lafilestrucu[3, 3] = 5
 lafilestrucu[3, 4] = 0
 lafilestrucu[4, 1] = 'llPrnSDc'
 lafilestrucu[4, 2] = 'L'
 lafilestrucu[4, 3] = 1
 lafilestrucu[4, 4] = 0
 lafilestrucu[5, 1] = 'Sz1'
 lafilestrucu[5, 2] = 'C'
 lafilestrucu[5, 3] = 5
 lafilestrucu[5, 4] = 0
 lafilestrucu[6, 1] = 'Sz2'
 lafilestrucu[6, 2] = 'C'
 lafilestrucu[6, 3] = 5
 lafilestrucu[6, 4] = 0
 lafilestrucu[7, 1] = 'Sz3'
 lafilestrucu[7, 2] = 'C'
 lafilestrucu[7, 3] = 5
 lafilestrucu[7, 4] = 0
 lafilestrucu[8, 1] = 'Sz4'
 lafilestrucu[8, 2] = 'C'
 lafilestrucu[8, 3] = 5
 lafilestrucu[8, 4] = 0
 lafilestrucu[9, 1] = 'Sz5'
 lafilestrucu[9, 2] = 'C'
 lafilestrucu[9, 3] = 5
 lafilestrucu[9, 4] = 0
 lafilestrucu[10, 1] = 'Sz6'
 lafilestrucu[10, 2] = 'C'
 lafilestrucu[10, 3] = 5
 lafilestrucu[10, 4] = 0
 lafilestrucu[11, 1] = 'Sz7'
 lafilestrucu[11, 2] = 'C'
 lafilestrucu[11, 3] = 5
 lafilestrucu[11, 4] = 0
 lafilestrucu[12, 1] = 'Sz8'
 lafilestrucu[12, 2] = 'C'
 lafilestrucu[12, 3] = 5
 lafilestrucu[12, 4] = 0
 lafilestrucu[13, 1] = 'Sz9'
 lafilestrucu[13, 2] = 'C'
 lafilestrucu[13, 3] = 5
 lafilestrucu[13, 4] = 0
 lafilestrucu[14, 1] = 'Sz10'
 lafilestrucu[14, 2] = 'C'
 lafilestrucu[14, 3] = 5
 lafilestrucu[14, 4] = 0
 lafilestrucu[15, 1] = 'Sz11'
 lafilestrucu[15, 2] = 'C'
 lafilestrucu[15, 3] = 5
 lafilestrucu[15, 4] = 0
 lafilestrucu[16, 1] = 'Sz12'
 lafilestrucu[16, 2] = 'C'
 lafilestrucu[16, 3] = 5
 lafilestrucu[16, 4] = 0
 lafilestrucu[17, 1] = 'Sz13'
 lafilestrucu[17, 2] = 'C'
 lafilestrucu[17, 3] = 5
 lafilestrucu[17, 4] = 0
 lafilestrucu[18, 1] = 'Sz14'
 lafilestrucu[18, 2] = 'C'
 lafilestrucu[18, 3] = 5
 lafilestrucu[18, 4] = 0
 lafilestrucu[19, 1] = 'Sz15'
 lafilestrucu[19, 2] = 'C'
 lafilestrucu[19, 3] = 5
 lafilestrucu[19, 4] = 0
 lafilestrucu[20, 1] = 'Sz16'
 lafilestrucu[20, 2] = 'C'
 lafilestrucu[20, 3] = 5
 lafilestrucu[20, 4] = 0
 lafilestrucu[21, 1] = 'OrgScl'
 lafilestrucu[21, 2] = 'C'
 lafilestrucu[21, 3] = 3
 lafilestrucu[21, 4] = 0
 = gfcrttmp(lctmpsizes, @lafilestrucu, "ScalFld + cDim1", lctmpsizes, .T.)
 SELECT (lctmpsizes)
 INDEX ON scalfld+ALLTRIM(STR(indxdm2)) TAG sortscal
ENDPROC
**
PROCEDURE lfnsrtfnd
 SELECT (lclinfile)
 lcscalval = IIF(SEEK("S" + SUBSTR(STYLE , lnscaposgl , lnscalngl) , lctempscale) , &lctempscale..SCALE , "")
 lccdm1val = &lctempscale..cdim1
 SELECT (lcadstygrp)
 lcoldordr = ORDER()
 SET ORDER TO lcStyFond
 LOCATE
 = SEEK(EVALUATE(lclinfile+'.PACK_NO')+SUBSTR(EVALUATE(lclinfile+'.STYLE'), lnstyposgl, lnstylngl))
 SET ORDER TO TAG &lcoldordr
 SET ORDER TO lcAdStyGrp
 = SEEK(EVALUATE(lclinfile+'.PACK_NO')+account+store+cgroupkey+cdelivery)
 lcfullindex = ' PACK_NO + Account + Store + cGroupkey  + CDelivery + ALLTRIM(STR(Line_No)) + SUBSTR(STYLE,lnStyPosGl,lnStyLnGl) + ALLTRIM(STR(IndxDm2))'
 SCAN REST FOR pack_no==EVALUATE(lclinfile+'.PACK_NO') WHILE lcfullindex=""
    IF SUBSTR(STYLE,lnstyposgl,lnclrposgl+lnclrlngl-1) == SUBSTR(EVAL(lclinfile+'.STYLE'),lnstyposgl,lnclrposgl+lnclrlngl-1) AND lcdim1 == lccdm1val  AND &lcadstygrp..account <> "ZZZZZ"
       FOR lnscl = 1 TO 1
          lcsizscl = "Sz"+ALLTRIM(STR(lnscl))
          FOR lnall = 1 TO 16
             lcadsscl = "Sz"+ALLTRIM(STR(lnall))+"DL"
             IF &lctempscale..&lcsizscl == &lcadstygrp..&lcadsscl
                FOR lnfill = lnscl TO 8
                   lcsizscl = "Qty"+ALLTRIM(STR(lnfill))
                   lcadsscl = "Qty"+ALLTRIM(STR(lnfill+lnall-1))
                   IF &lclinfile..&lcsizscl # 0
                      REPLACE &lcadstygrp..&lcadsscl WITH &lcadstygrp..&lcadsscl + &lclinfile..&lcsizscl
                   ENDIF
                ENDFOR
                EXIT
             ENDIF
          ENDFOR
       ENDFOR
       REPLACE &lcadstygrp..totqty    WITH &lcadstygrp..totqty + &lclinfile..totqty  &lcadstygrp..line_no   WITH &lclinfile..line_no                       &lcadstygrp..llprnscal WITH .T.                                       &lcadstygrp..llprnlin  WITH .T.
       REPLACE &lcadstygrp..nordlineno WITH &lclinfile..nordlineno 
    ENDIF
    IF lltofollow AND SUBSTR(STYLE,lnstyposgl,lnclrposgl+lnclrlngl-1) == SUBSTR(EVAL(lclinfile+'.STYLE'),lnstyposgl,lnclrposgl+lnclrlngl-1)   AND lcdim1 == lccdm1val                  AND &lcadstygrp..account == "ZZZZZ"      AND  &lcadstygrp..cfollow                 AND &lcadstygrp..folowrec == "ZZZZ"
       =SEEK("O" + &lctemppack_hdr..ORDER + STORE + &lclinfile..STYLE+STR(&lclinfile..nordlineno,6) , lcordlntmp)
       IF lfopenpo()
          REPLACE &lcadstygrp..ponofolo WITH lcopenpo , &lcadstygrp..datavlbl WITH ldavalbl
       ENDIF
       PRIVATE lcsizscl, lcadsscl
       FOR lnscl = 1 TO 1
          lcsizscl = "Sz"+ALLTRIM(STR(lnscl))
          FOR lnall = 1 TO 16
             lcadsscl = "Sz"+ALLTRIM(STR(lnall))+"DL"
             IF &lctempscale..&lcsizscl == &lcadstygrp..&lcadsscl
                FOR lnfill = lnscl TO 8
                   lcsizscl = "Sz"+ALLTRIM(STR(lnfill))
                   lcqtyord = "Qty"+ALLTRIM(STR(lnfill))
                   lcpikqty = "Pik"+ALLTRIM(STR(lnfill))
                   lcadsscl = "Qty"+ALLTRIM(STR(lnfill+lnall-1))
                   IF !EMPTY(&lctempscale..&lcsizscl) AND (&lcordlntmp..&lcqtyord - &lcordlntmp..&lcpikqty) # 0
                      REPLACE &lcadstygrp..&lcadsscl WITH &lcadstygrp..&lcadsscl + (&lcordlntmp..&lcqtyord - &lcordlntmp..&lcpikqty)
                   ENDIF
                ENDFOR
                EXIT
             ENDIF
          ENDFOR
       ENDFOR
       REPLACE &lcadstygrp..totqty WITH &lcadstygrp..totqty + (&lcordlntmp..totqty - &lcordlntmp..totpik)
    ENDIF
 ENDSCAN
ENDPROC
**
PROCEDURE lfadmsdrec
 PRIVATE lcalias, lcpochk
 lcalias = SELECT(0)
 SELECT (lclinfile)
 llshpd = .F.
 STORE SPACE(0) TO lcpochk
 SCAN
    lcpack_no = &lclinfile..pack_no
    IF lcpochk<>pack_no
       lckeymain = EVALUATE(KEY())
       =lopack_hdr.SEEK(&lclinfile..pack_no)
       llshpd = .F.
       SELECT invline
       lcord = ORDER()
       SET ORDER TO INVLINEO
       IF SEEK(&lctemppack_hdr..ORDER)
          SET ORDER TO &lcord
          llshpd = .T.
       ENDIF
       IF SEEK("O" + &lctemppack_hdr..ORDER ,lcordlntmp) AND !llshpd
          SELECT (lcordlntmp)
          SCAN REST WHILE cordtype + ORDER + STR(LINENO,6) = "O" + &lctemppack_hdr..ORDER
             IF EMPTY(&lcordlntmp..piktkt)
                SCATTER MEMO MEMVAR
                SELECT (lclinfile)
                APPEND BLANK
                GATHER MEMVAR FIELDS LIKE Qty*, TotQty, Style
                REPLACE pack_no WITH lcpack_no, cowner WITH "BBBBVVVVDDDDRRRR"
                REPLACE nordlineno WITH &lcordlntmp..LINENO
                IF !SEEK(&lcordlntmp..STYLE,lcstylefile)
                   lostyle.SEEK(&lcordlntmp..STYLE)
                   SELECT (lctempstyle)
                   SCATTER MEMO MEMVAR
                   INSERT INTO (lcstylefile) FROM MEMVAR
                ENDIF
                SELECT (lcordlntmp)
             ENDIF
          ENDSCAN
          SELECT (lclinfile)
          = SEEK(lckeymain)
       ENDIF
    ENDIF
    lcpochk = &lctemppack_hdr..pack_no
 ENDSCAN
 SELECT (lcalias)
ENDPROC
**
FUNCTION lfgetempname
 lnalias = ALIAS()
 lcempname = ''
 SELECT contact
 =gfseek('C'+PADR(&lcpaklntmp..account,8)+PADR(&lcpaklntmp..STORE,8),'contact') 
 LOCATE REST WHILE cconttype+ccont_id+STORE+contact = 'C'+PADR(&lcpaklntmp..account,8)+PADR(&lcpaklntmp..STORE,8) FOR ccntctcode = ALLTRIM(&lcpaklntmp..cpackcolor)
 IF FOUND()
    lcempname = contact.contact
 ENDIF
 SELECT (lnalias)
 RETURN lcempname
ENDFUNC
**
FUNCTION lfgetucode
 lnalias = ALIAS()
 lcucode = ''
 SELECT contact
 =gfseek('C'+PADR(&lcpaklntmp..account,8)+PADR(&lcpaklntmp..STORE,8),'contact') 
 LOCATE REST WHILE cconttype+ccont_id+STORE+contact = 'C'+PADR(&lcpaklntmp..account,8)+PADR(&lcpaklntmp..STORE,8) FOR ccntctcode = ALLTRIM(&lcpaklntmp..cpackcolor)
 IF FOUND()
    lcucode = contact.ucode
 ENDIF
 SELECT (lnalias)
 RETURN lcucode
ENDFUNC
**
FUNCTION lfprnmsg
 lnalias = ALIAS()
 lcpackno =&lcpaklntmp..pack_no 
 lopack_hdr.seek(lcpackno)
 SELECT (lcpaklntmp)
 lnrcno = RECNO()
 LOCATE
 LOCATE FOR pack_no=lcpackno .AND. account="ZZZZZ" .AND. cfollow=.T. .AND. folowrec="ZZZZ" .AND. (EMPTY(ponofolo) .OR. EMPTY(datavlbl))
 IF FOUND()
    llprntmsg = .T.
 ELSE
    llprntmsg = .F.
 ENDIF
 IF BETWEEN(lnrcno, 1, RECCOUNT())
    GOTO lnrcno
 ENDIF
 SELECT (lnalias)
 RETURN ''
ENDFUNC
**
PROCEDURE lfgetallordl
 SELECT (lcordlntmp)
 DELETE ALL
 SELECT (lcpacktmp)
 LOCATE
 SCAN
    loordline.SEEK('O'+ &lcpacktmp..ORDER + &lcpacktmp..STORE)
    SELECT (lctempordline)
    SCAN REST WHILE cordtype+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+ &lcpacktmp..ORDER + &lcpacktmp..STORE 
       SCATTER MEMO MEMVAR
       INSERT INTO (lcordlntmp) FROM MEMVAR
    ENDSCAN
 ENDSCAN
 SELECT (lcordlntmp)
 SCAN
    lostyle.seek(style)
    SELECT (lcstylefile)
    IF !SEEK(&lctempstyle..STYLE,lcstylefile)
       SELECT (lctempstyle)
       SCATTER MEMO MEMVAR
       INSERT INTO (lcstylefile) FROM MEMVAR
    ENDIF
 ENDSCAN
ENDPROC
**
PROCEDURE lfgrprest
 llendgroup = .F.
ENDPROC
**
PROCEDURE lfchknext
 llprnttot = .F.
 lcpacknum = pack_no
 lnoldrec = RECNO(lcpaklntmp)
 IF  .NOT. EOF() .AND. (cpackcolor<>"ZZZZZZZZZZ" .OR. EMPTY(cpackcolor))
    SKIP 1 IN (lcpaklntmp)
    IF  .NOT. EOF() .AND. (lcpacknum=pack_no .AND. cpackcolor="ZZZZZZZZZZ")
       llprnttot = .T.
    ELSE
       IF EOF() .OR. ( .NOT. EOF() .AND. (lcpacknum<>pack_no))
          llprnttot = .T.
       ENDIF
    ENDIF
 ELSE
    IF EOF() .AND. (cpackcolor<>"ZZZZZZZZZZ" .OR. EMPTY(cpackcolor))
       llprnttot = .T.
    ELSE
       IF cpackcolor="ZZZZZZZZZZ"
          llprnttot = .F.
       ENDIF
    ENDIF
 ENDIF
 IF BETWEEN(lnoldrec, 1, RECCOUNT(lcpaklntmp))
    GOTO lnoldrec IN (lcpaklntmp)
 ENDIF
ENDPROC
**
*** 
*** ReFox - all is not lost 
***
PROCEDURE errHandler
   PARAMETER merror, mess, mess1, mprog, mlineno
   CLEAR
   wait window 'Error number: ' + LTRIM(STR(merror))
   wait window 'Error message: ' + mess
   wait window 'Line of code with error: ' + mess1
   wait window 'Line number of error: ' + LTRIM(STR(mlineno))
   wait window 'Program with error: ' + mprog
ENDPROC
 
