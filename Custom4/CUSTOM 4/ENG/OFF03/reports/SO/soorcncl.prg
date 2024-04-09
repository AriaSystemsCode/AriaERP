*!*	 2012-09-17 Change lPacks to nPackQty to allow for different Pack Sizes
 PRIVATE lntargtfld, lcordkey, lnmaxsize, lcrltcolor, lcfabcolor
 STORE '' TO lctargtfl, lcemail, lcwebsite, lcpaspar, lcusername, lcuserphone, lcStyle
 STORE 1 TO lntargtfld, lnsorcefld
*!*	 STORE .F. TO m.lPacks
 STORE 0 TO lnclrlngl, lnclrposgl, lnstylngl, lnstyposgl, lnscalngl, lnscaposgl, m.nPackQty
 = lfchkstrct()
 IF  .NOT. USED('SYUUSER')
    = gfopentable('SYUUSER', 'CUSER_ID', 'SH')
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
 lctargtfl = loogscroll.gftempname()
 lctempSz = loogscroll.gftempname()
 = lfcrttmp()
 SELECT ordhdr
 SET RELATION ADDITIVE TO cordtype+order INTO (lctargtfl)
 SELECT (lctargtfl)
 IF lcrpbook<>'Y'
    SET FILTER TO totqty<>0
 ENDIF
 SET RELATION ADDITIVE TO 'S'+SUBSTR(style, 1, lnmajorlen) INTO objlink_a
 SET RELATION ADDITIVE TO 'S'+scale INTO scale
 SET RELATION ADDITIVE TO style INTO style
 SET RELATION ADDITIVE TO styleID INTO (lcTempSz)
 SELECT objlink_a
 SET RELATION TO
 SET RELATION ADDITIVE TO cobject_id INTO objects_a
 SELECT style
 SET RELATION TO 'F'+cstymajor INTO notepad ADDITIVE
 lcindexkey = 'CORDTYPE + ORDER + STORE + SUBSTR(Style,1,lnStyleLen)'
 = lfaccmulte()
 = lfcoldata()
 DIMENSION laPackNotes[1]
 = lfGetPackNotes()
 SELECT (lctargtfl)
 IF lcrpsortby='S'
    INDEX ON cordtype+order+store+employee+style TAG (lctargtfl)
    SET ORDER TO (lctargtfl)
 ELSE
    INDEX ON cordtype+order+store+employee+STR(lineno, 6) TAG (lctargtfl)
    SET ORDER TO (lctargtfl)
 ENDIF
 SELECT (lctempord)
 SET RELATION TO
 lcskipexpr  = [&lcTargtFl]
 SELECT ordhdr
 SET SKIP TO &lcskipexpr
 lcrpexp = lcrpexp+' AND !EOF(lcTargtFl)'
 DO gfdispre WITH EVALUATE('lcFormName'), 'FOR '+lcrpexp
 llsalsord = .F.
 RETURN
ENDPROC
**
PROCEDURE lfCrtTmp
 IF USED(lctargtfl) .AND. RECCOUNT(lctargtfl)>0
    USE IN (lctargtfl)
 ENDIF
 IF  .NOT. USED(lctargtfl)
    SELECT ordline
	 = AFIELDS(lafstru)
	 lnfstru = alen(lafstru, 1)
	 DIMENSION lafstru[ lnfstru+18, 18]
	 lafstru[ lnfstru+1, 1] = 'qty9'
	 lafstru[ lnfstru+1, 2] = 'N'
	 lafstru[ lnfstru+1, 3] = 5
	 lafstru[ lnfstru+1, 4] = 0
	 lafstru[ lnfstru+2, 1] = 'qty10'
	 lafstru[ lnfstru+2, 2] = 'N'
	 lafstru[ lnfstru+2, 3] = 5
	 lafstru[ lnfstru+2, 4] = 0
	 lafstru[ lnfstru+3, 1] = 'qty11'
	 lafstru[ lnfstru+3, 2] = 'N'
	 lafstru[ lnfstru+3, 3] = 5
	 lafstru[ lnfstru+3, 4] = 0
	 lafstru[ lnfstru+4, 1] = 'qty12'
	 lafstru[ lnfstru+4, 2] = 'N'
	 lafstru[ lnfstru+4, 3] = 5
	 lafstru[ lnfstru+4, 4] = 0
	 lafstru[ lnfstru+5, 1] = 'qty13'
	 lafstru[ lnfstru+5, 2] = 'N'
	 lafstru[ lnfstru+5, 3] = 5
	 lafstru[ lnfstru+5, 4] = 0
	 lafstru[ lnfstru+6, 1] = 'qty14'
	 lafstru[ lnfstru+6, 2] = 'N'
	 lafstru[ lnfstru+6, 3] = 5
	 lafstru[ lnfstru+6, 4] = 0
	 lafstru[ lnfstru+7, 1] = 'qty15'
	 lafstru[ lnfstru+7, 2] = 'N'
	 lafstru[ lnfstru+7, 3] = 5
	 lafstru[ lnfstru+7, 4] = 0
	 lafstru[ lnfstru+8, 1] = 'qty16'
	 lafstru[ lnfstru+8, 2] = 'N'
	 lafstru[ lnfstru+8, 3] = 5
	 lafstru[ lnfstru+8, 4] = 0
	 lafstru[ lnfstru+9, 1] = 'book9'
	 lafstru[ lnfstru+9, 2] = 'N'
	 lafstru[ lnfstru+9, 3] = 5
	 lafstru[ lnfstru+9, 4] = 0
	 lafstru[ lnfstru+10, 1] = 'book10'
	 lafstru[ lnfstru+10, 2] = 'N'
	 lafstru[ lnfstru+10, 3] = 5
	 lafstru[ lnfstru+10, 4] = 0
	 lafstru[ lnfstru+11, 1] = 'book11'
	 lafstru[ lnfstru+11, 2] = 'N'
	 lafstru[ lnfstru+11, 3] = 5
	 lafstru[ lnfstru+11, 4] = 0
	 lafstru[ lnfstru+12, 1] = 'book12'
	 lafstru[ lnfstru+12, 2] = 'N'
	 lafstru[ lnfstru+12, 3] = 5
	 lafstru[ lnfstru+12, 4] = 0
	 lafstru[ lnfstru+13, 1] = 'book13'
	 lafstru[ lnfstru+13, 2] = 'N'
	 lafstru[ lnfstru+13, 3] = 5
	 lafstru[ lnfstru+13, 4] = 0
	 lafstru[ lnfstru+14, 1] = 'book14'
	 lafstru[ lnfstru+14, 2] = 'N'
	 lafstru[ lnfstru+14, 3] = 5
	 lafstru[ lnfstru+14, 4] = 0
	 lafstru[ lnfstru+15, 1] = 'book15'
	 lafstru[ lnfstru+15, 2] = 'N'
	 lafstru[ lnfstru+15, 3] = 5
	 lafstru[ lnfstru+15, 4] = 0
	 lafstru[ lnfstru+16, 1] = 'book16'
	 lafstru[ lnfstru+16, 2] = 'N'
	 lafstru[ lnfstru+16, 3] = 5
	 lafstru[ lnfstru+16, 4] = 0
	 lafstru[ lnfstru+17, 1] = 'Styleid'
	 lafstru[ lnfstru+17, 2] = 'C'
	 lafstru[ lnfstru+17, 3] = 22
	 lafstru[ lnfstru+17, 4] = 0
	 lafstru[ lnfstru+18, 1] = 'nPackQty'
	 lafstru[ lnfstru+18, 2] = 'N'
	 lafstru[ lnfstru+18, 3] = 4
	 lafstru[ lnfstru+18, 4] = 0
	 FOR linOloop = 1 to 18
	 	 FOR lnIloop = 5 to 16
			 lafstru[ lnfstru+linOloop, lnIloop] = ' '
		 ENDFOR
		 lafstru[ lnfstru+linOloop, 17] = 0
		 lafstru[ lnfstru+linOloop, 18] = 0
	 ENDFOR
    = gfcrttmp(lctargtfl, @lafstru, 'cordtype+order+store+styleID', lctargtfl)
 ENDIF
 DIMENSION lafstru[17, 4]
 STORE SPACE(0) TO lafstru
 lafstru[1, 1] = 'Styleid'
 lafstru[1, 2] = 'C'
 lafstru[1, 3] = 22
 lafstru[1, 4] = 0
 lafstru[2, 1] = 'sz1'
 lafstru[2, 2] = 'C'
 lafstru[2, 3] = 6
 lafstru[2, 4] = 0
 lafstru[3, 1] = 'sz2'
 lafstru[3, 2] = 'C'
 lafstru[3, 3] = 6
 lafstru[3, 4] = 0
 lafstru[4, 1] = 'sz3'
 lafstru[4, 2] = 'C'
 lafstru[4, 3] = 6
 lafstru[4, 4] = 0
 lafstru[5, 1] = 'sz4'
 lafstru[5, 2] = 'C'
 lafstru[5, 3] = 6
 lafstru[5, 4] = 0
 lafstru[6, 1] = 'sz5'
 lafstru[6, 2] = 'C'
 lafstru[6, 3] = 6
 lafstru[6, 4] = 0
 lafstru[7, 1] = 'sz6'
 lafstru[7, 2] = 'C'
 lafstru[7, 3] = 6
 lafstru[7, 4] = 0
 lafstru[8, 1] = 'sz7'
 lafstru[8, 2] = 'C'
 lafstru[8, 3] = 6
 lafstru[8, 4] = 0
 lafstru[9, 1] = 'sz8'
 lafstru[9, 2] = 'C'
 lafstru[9, 3] = 6
 lafstru[9, 4] = 0
 lafstru[10, 1] = 'sz9'
 lafstru[10, 2] = 'C'
 lafstru[10, 3] = 6
 lafstru[10, 4] = 0
 lafstru[11, 1] = 'sz10'
 lafstru[11, 2] = 'C'
 lafstru[11, 3] = 6
 lafstru[11, 4] = 0
 lafstru[12, 1] = 'sz11'
 lafstru[12, 2] = 'C'
 lafstru[12, 3] = 6
 lafstru[12, 4] = 0
 lafstru[13, 1] = 'sz12'
 lafstru[13, 2] = 'C'
 lafstru[13, 3] = 6
 lafstru[13, 4] = 0
 lafstru[14, 1] = 'sz13'	
 lafstru[14, 2] = 'C'
 lafstru[14, 3] = 6
 lafstru[14, 4] = 0
 lafstru[15, 1] = 'sz14'
 lafstru[15, 2] = 'C'
 lafstru[15, 3] = 6
 lafstru[15, 4] = 0
 lafstru[16, 1] = 'sz15'
 lafstru[16, 2] = 'C'
 lafstru[16, 3] = 6
 lafstru[16, 4] = 0
 lafstru[17, 1] = 'sz16'
 lafstru[17, 2] = 'C'
 lafstru[17, 3] = 6
 lafstru[17, 4] = 0
 gfcrttmp(lctempSz, @lafstru, 'styleid', lctempSz, .F.)
ENDPROC
**
**
PROCEDURE lfAccmulte
 PRIVATE lnslct
 lnslct = SELECT()
 SELECT (lctempord)
 lcnewtmp = loogscroll.gftempname()
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
       REPLACE totqty WITH totqty+m.qty1+m.qty2+m.qty3+m.qty4+m.qty5+m.qty6+m.qty7+m.qty8
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
PROCEDURE lfColData
 SELECT (lctempord)
 SCAN
	STORE 0 TO m.nPackQty
   SCATTER MEMO MEMVAR
   lcstyle=LEFT(&lctempord..Style, lnscaposgl-1)
   lnln = IIF(lfGetExt(&lctempord..style)=1, 0, 8)
   IF lnln>0
   		FOR lni = 1 TO 8
        	lnt = lni+lnln
        	lct = ALLTRIM(STR(lnt))
            lci = ALLTRIM(STR(lni))
            m.qty&lct = m.qty&lci
            m.qty&lci = 0
            m.book&lct = m.book&lci
            m.book&lci = 0
        ENDFOR
   ELSE
   		FOR lni = 9 TO 16
            lci = ALLTRIM(STR(lni))
            m.qty&lci = 0
            m.book&lci = 0
        ENDFOR
   ENDIF
   m.styleID=lcStyle
   IF  .NOT. SEEK(&lctempord..cordtype+&lctempord..order+&lctempord..store+m.styleID, lctargtfl)
       INSERT INTO (lctargtfl) FROM MEMVAR
   ELSE
   		FOR lni = 1 TO 16
            lci = ALLTRIM(STR(lni))
            REPL qty&lci WITH qty&lci+m.qty&lci, totqty WITH totqty+m.qty&lci IN (lctargtfl)
            m.qty&lci = 0
            REPL book&lci WITH book&lci+m.book&lci, totbook WITH totbook+m.book&lci IN (lctargtfl)
            m.book&lci = 0
        ENDFOR
   ENDIF
 ENDSCAN
ENDPROC
*****
*****
FUNCTION lffillarry
	PARAMETER lcalias, lcarray, lnlen
	PRIVATE lni, lncount, lcarraycnt, lnmemoset
	lnmemoset = SET('MEMOWIDTH')
	SET MEMOWIDTH TO 97
	lncount = 1
	FOR lni = 1 TO MEMLINES(EVALUATE(lcalias+'.MNOTES'))
		IF  .NOT. EMPTY(MLINE(EVALUATE(lcalias+'.MNOTES'), lni))
			lcarraycnt = lcarray+'[lnCount]'
			IF (LEN(&lcArrayCnt)+LEN(ALLTRIM(MLINE(EVALUATE(lcAlias+'.MNOTES'),lnI)))<110
				&lcArrayCnt. =&lcArrayCnt+ALLTRIM(MLINE(EVALUATE(lcAlias+'.MNOTES'),lnI))
			ELSE
				lncount = lncount+1
				lcarraycnt = lcarray+'[lnCount]'
				&lcArrayCnt. = ALLTRIM(MLINE(EVALUATE(lcAlias+'.MNOTES'),lnI))
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

FUNCTION lfGetStyNotes
	PRIVATE lni, lncount, lcarraycnt, lnmemoset
    IF TYPE("lastynotes",1) <> 'A'
		DIMENSION lastynotes[6]
	ENDIF
	STORE SPACE(0) TO lastynotes
	lnmemoset = SET('MEMOWIDTH')
	SET MEMOWIDTH TO 97
	lncount = 1
*!*		= lffillarry('NOTEPAD','laStyNotes',6)
	FOR lni = 1 TO MEMLINES(NOTEPAD.MNOTES)
		IF  .NOT. EMPTY(MLINE(NOTEPAD.MNOTES, lni))
			IF (LEN(lastynotes[lncount])+LEN(ALLTRIM(MLINE(NOTEPAD.MNOTES,lnI))))<110
				lastynotes[lncount]=lastynotes[lncount]+' '+ALLTRIM(MLINE(NOTEPAD.MNOTES,lnI))
			ELSE
				lncount = lncount+1
				IF lncount>4
					EXIT
				ENDIF
				lastynotes[lncount]=ALLTRIM(MLINE(NOTEPAD.MNOTES,lnI))
			ENDIF
			IF lncount>4
				EXIT
			ENDIF
		ELSE
			EXIT
		ENDIF
	ENDFOR
	IF LEN(ALLTRIM(lastynotes[lncount]))>0
		lncount = lncount+1
	ENDIF
	IF lncount>6
		RETURN ''
	ENDIF
	FOR lni = 1 TO MEMLINES(NOTELABEL.MNOTES)
		IF  .NOT. EMPTY(MLINE(NOTELABEL.MNOTES, lni))
			IF (LEN(lastynotes[lncount])+LEN(ALLTRIM(MLINE(NOTELABEL.MNOTES,lnI))))<110
				lastynotes[lncount]=lastynotes[lncount]+' '+ALLTRIM(MLINE(NOTELABEL.MNOTES,lnI))
			ELSE
				lncount = lncount+1
				IF lncount>6
					RETURN ''
				ENDIF
				lastynotes[lncount]=ALLTRIM(MLINE(NOTELABEL.MNOTES,lnI))
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
*****
*****
PROCEDURE lfChkStrct
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
*****
*****
FUNCTION lfGetExt
 LPARAMETERS lcStyleP
 lnalias = SELECT()
 lcStyle=LEFT(lcStyleP, lnscaposgl-1)
 lcScale=RIGHT(lcStyleP, lnscalngl)
*!*	 IF lcscale='Z01'
*!*	     m.nPackQty=12
*!*	     lcStyle=lcStyle+"_PACKS"
*!*	 ELSE
*!*	     m.lPacks=.F.
*!*	     lcStyle=lcStyle+"_PAIRS"
*!*	 ENDIF
 STORE 0 TO lncnt, lnpos, m.nPackQty
 DO CASE
 	CASE lcScale='Z01'
	     m.nPackQty=12
	     lcStyle=lcStyle+"_PACK12"
		SELECT (lcTempSz)
		IF !SEEK(lcStyle)
            SELECT (lcTempSz)
            APPEND BLANK
			REPL styleID WITH lcStyle IN (lcTempSz)
			SELECT scale
			SEEK ('S'+lcscale) 
			FOR lni = 1 TO 8
				lci = ALLTRIM(STR(lni))
				REPL sz&lci WITH SCALE.sz&lci IN (lcTempSz)
			ENDFOR
		ENDIF
		lnpos=1
 	CASE lcScale='Z04'
	     m.nPackQty=8
	     lcStyle=lcStyle+"_PACK8"
		SELECT (lcTempSz)
		IF !SEEK(lcStyle)
            SELECT (lcTempSz)
            APPEND BLANK
			REPL styleID WITH lcStyle IN (lcTempSz)
			SELECT scale
			SEEK ('S'+lcscale) 
			FOR lni = 1 TO 8
				lci = ALLTRIM(STR(lni))
				REPL sz&lci WITH SCALE.sz&lci IN (lcTempSz)
			ENDFOR
		ENDIF
		lnpos=1
 	CASE LEFT(lcScale,1)='Z'
	     lcStyle=lcStyle+"_PAIRS"
		SELECT (lcTempSz)
		IF !SEEK(lcStyle)
            SELECT (lcTempSz)
            APPEND BLANK
			REPL styleID WITH lcStyle IN (lcTempSz)
        ENDIF
		SELECT scale
		SEEK ('S'+'Z02') 
		SCAN REST WHILE type+scale='S'+ALLTRIM(LEFT(lcscale, 2))
			lncnt = lncnt+1
			IF lncnt=3
				EXIT
			ENDIF
			IF lncnt=1
				FOR lni = 1 TO 8
					lci = ALLTRIM(STR(lni))
					REPL sz&lci WITH SCALE.sz&lci IN (lcTempSz)
				ENDFOR
				IF lcScale=scale.scale
					lnpos=1
				ENDIF
			ELSE
				FOR lni = 1 TO 8
					lci = ALLTRIM(STR(lni))
					lcj = ALLTRIM(STR(lni+8))
					REPL sz&lcj WITH SCALE.sz&lci IN (lcTempSz)
				ENDFOR
				IF lcScale=scale.scale
					lnpos=2
				ENDIF
			ENDIF
		ENDSCAN
 	OTHERWISE
		SELECT (lcTempSz)
		IF !SEEK(lcStyle)
            SELECT (lcTempSz)
            APPEND BLANK
			REPL styleID WITH lcStyle IN (lcTempSz)
        ENDIF
		SELECT scale
		SEEK ('S'+ALLTRIM(LEFT(lcscale, 2))) 
		SCAN REST WHILE type+scale='S'+ALLTRIM(LEFT(lcscale, 2))
			lncnt = lncnt+1
			IF lncnt=3
				EXIT
			ENDIF
			IF lncnt=1
				FOR lni = 1 TO 8
					lci = ALLTRIM(STR(lni))
					REPL sz&lci WITH SCALE.sz&lci IN (lcTempSz)
				ENDFOR
				IF lcScale=scale.scale
					lnpos=1
				ENDIF
			ELSE
				FOR lni = 1 TO 8
					lci = ALLTRIM(STR(lni))
					lcj = ALLTRIM(STR(lni+8))
					REPL sz&lcj WITH SCALE.sz&lci IN (lcTempSz)
				ENDFOR
				IF lcScale=scale.scale
					lnpos=2
				ENDIF
			ENDIF
		ENDSCAN
 ENDCASE
 SELECT (lnalias)
 RETURN lnPos
ENDFUNC
*****
*****
FUNCTION lfGetPackNotes
	IF gfseek('T'+'PACKS','notelabel')
		lnRows=ALINES(laPackNotes, notelabel.mnotes)
		IF lnRows<5
			DIMENSION laPackNotes[5]
		ENDIF
	ENDIF
ENDFUNC
*****
*****
FUNCTION lfClearPackNotes
	DIMENSION laPackNotes[5]
	STORE SPACE(0) TO laPackNotes
ENDFUNC
*****
*****
