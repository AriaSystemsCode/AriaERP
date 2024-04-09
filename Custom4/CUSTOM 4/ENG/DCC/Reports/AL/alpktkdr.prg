*B611082,1 MMT 12/15/2015 Custom Picking ticket form DR is not including allocated lines as non-stock lines[T20151113.0005]
*** 
*** ReFox XII  #UK122952  David Stephenson  Aria Systems Ltd [VFP90]
***
 llalpktk = .F.
 IF llRpBlkSum AND llRpBatch
    lnpospick = ASCAN(loogscroll.laogfxflt, "PICKPACK.CPICKBY")
    IF lnpospick>0
       lnpospick = ASUBSCRIPT(loogscroll.laogfxflt, lnpospick, 1)
       lcpickby = IIF( .NOT. EMPTY(loogscroll.laogfxflt(lnpospick, 6)), loogscroll.laogfxflt(lnpospick, 6), '')
       IF EMPTY(lcpickby)
          = gfmodalgen('TRM00000B00000', .F., .F., .F., "You have to Assign the Batch")
          llPrinter=.F.
          RETURN
       ENDIF
    ENDIF
 ENDIF
 PRIVATE lcalasdl, lcorddl, lckeydl, lcoldname, lcphonuser
 PRIVATE lchldscal, lckey, lcstyclr, lcvalstclr, lcscalval, lccdm1val, lcopenpo, ldavalbl
 STORE SPACE(0) TO lchldscal, lckey, lcstyclr, lcvalstclr, lcscalval, lccdm1val
 STORE SPACE(0) TO lcopenpo
 STORE 0 TO lnclrlngl, lnclrposgl, lnstylngl, lnstyposgl, lnscalngl, lnscaposgl
 STORE {} TO ldavalbl
 lcalasdl = SELECT(0)
 lcorddl = ORDER()
 lckeydl = EVALUATE(KEY())
 = lfchkstrct()
 lnstyclrcnt = 0
 lnlastline = 0
 lnlastlinx = 0
 IF  .NOT. USED('POSHDR')
    = gfopentable(oariaapplication.datadir+'POSHDR', 'POSHDR', 'SH')
 ENDIF
 IF  .NOT. USED('CONTACT')
    = gfopentable(oariaapplication.datadir+'CONTACT', 'CONTACT', 'SH')
 ENDIF
 IF  .NOT. USED('POSLN')
    = gfopentable(oariaapplication.datadir+'POSLN', 'POSLNS', 'SH')
 ENDIF
 SELECT (lctmpordl)
 lcoldname = lctmpordl
 = lfcreattmp()
 SELECT (lctmpordl)
 LOCATE
 STORE '' TO lcorder
 STORE '' TO lcstore
 SCAN
    IF lcorder = &lctmpordl..ORDER AND lcstore = &lctmpordl..STORE
       LOOP
    ENDIF
    lcorder = &lctmpordl..ORDER
    lcstore = &lctmpordl..STORE
    SELECT ordline
    lcoldorder = ORDER()
    gfsetorder("ORDLINST")
    = gfseek('O'+lcorder+lcstore, 'ORDLINE', 'ORDLINST')
    SCAN REST FOR totqty<>0 WHILE cordtype+order+store='O'+lcorder+lcstore
       *B611082,1 MMT 12/15/2015 Custom Picking ticket form DR is not including allocated lines as non-stock lines[T20151113.0005][Start]
       *IF  .NOT. EMPTY(piktkt)
       IF  !EMPTY(ALLTRIM(piktkt)) AND  piktkt <> '******'
       *B611082,1 MMT 12/15/2015 Custom Picking ticket form DR is not including allocated lines as non-stock lines[T20151113.0005][End]
          LOOP
       ELSE
          lcpiktkt = &lctmpordl..piktkt
       ENDIF
       SCATTER MEMO MEMVAR
       
       *B611082,1 MMT 12/15/2015 Custom Picking ticket form DR is not including allocated lines as non-stock lines[T20151113.0005][Start]
       IF m.piktkt = '******'
          FOR nCnt = 1 TO 8
            cCnt = STR(nCnt,1)
            m.PIK&cCnt. = 0
          ENDFOR
          m.totpik = 0
          m.PikDate = {}
          m.picked =.F.
       ENDIF
       *B611082,1 MMT 12/15/2015 Custom Picking ticket form DR is not including allocated lines as non-stock lines[T20151113.0005][End]
       
       IF  .NOT. SEEK(lcpiktkt+lcorder+'D'+STR(m.lineno, 6), lctmpordl, lctmpordl)
          m.piktkt = lcpiktkt
          m.cgrupdetal = 'D'
          INSERT INTO (lctmpordl) FROM MEMVAR
       ENDIF
*!*	       m.piktkt = lcpiktkt
*!*	       m.cgrupdetal = 'D'
*!*	       INSERT INTO (lctmpordl) FROM MEMVAR
    ENDSCAN
    SELECT ordline
    SET ORDER TO &lcoldorder
    SELECT (lctmpordl)
 ENDSCAN
 LOCATE
 SCAN
    IF lineno=0 .AND. EMPTY(style)
       LOOP
    ENDIF
    lckey = SUBSTR(style, lnscaposgl, 2)
    IF  .NOT. (lckey$lchldscal)
       = lfgetsizes()
       lchldscal = lchldscal+IIF(EMPTY(lchldscal), "", ",")+lckey
    ENDIF
    SCATTER MEMO MEMVAR
    SELECT (lcadstygrp)
    APPEND BLANK
    GATHER MEMO MEMVAR
    REPLACE account WITH customer.account, cgroupkey WITH 'zzzzzz', stygrop WITH style.cstygroup, styloc WITH style.location, cstymajor WITH style.cstymajor, cconslpikt WITH 'Picking Tickets:  '+piktkt.piktkt, status WITH piktkt.status, cdelivery WITH IIF(customer.lldelivery, 'Y', 'N')
 ENDSCAN
 SELECT (lcadstygrp)
 INDEX ON piktkt+account+store+cdelivery+cgroupkey+ctype+stygrop+style+cgrupdetal+STR(lineno, 6) TAG lcgroup ADDITIVE
 SET ORDER TO lcGroup
 SELECT (lctmpordl)
 SET RELATION TO
 lctmpordl = lcadstygrp
 SELECT (lctmpordl)
 SCAN FOR  .NOT. EOF()
    lcempl = employee
    SELECT contact
    lccont = ""
    gfseek('C'+PADR(&lctmpordl..account,8)+&lctmpordl..STORE )
    SCAN REST WHILE cconttype+ccont_id+STORE+contact = 'C'+PADR(&lctmpordl..account,8)+ &lctmpordl..STORE FOR contact.ccntctcode = lcempl
       lccont = ALLTRIM(contact.contact)
    ENDSCAN
    SELECT (lctmpordl)
    IF  .NOT. EMPTY(lccont)
       REPLACE contact WITH lccont
    ENDIF
 ENDSCAN
 LOCATE
 lcoldpiktk = ''
 SCAN
    = lfgetfit()
    IF  .NOT. SEEK('zzzzzz'+piktkt, lctmpgroup)
       SELECT (lctmpgroup)
       APPEND BLANK
       REPLACE piktkt WITH &lctmpordl..piktkt, cgroupkey WITH 'zzzzzz'
       SELECT (lctmpordl)
       STORE piktkt TO lcoldpiktk
    ENDIF
 ENDSCAN
 SELECT (lctmpgroup)
 LOCATE
 lcfrmpktk = piktkt
 GOTO BOTTOM
 lctopktk = piktkt
 lntotpktk = RECCOUNT(lctmpgroup)
 lcbnstygrp = gftempname()
 lcreptsty = gftempname()
 = lfcrttmp()
 SELECT (lcbnstygrp)
 DELETE ALL
 SELECT (lctmpordl)
 SCAN
    SCATTER MEMO MEMVAR
    IF  .NOT. (OCCURS(',', m.cconslpikt)>0)
       SELECT (lcreptsty)
       APPEND BLANK
       GATHER MEMO MEMVAR
    ENDIF
    =SEEK('S'+&lctmpordl..SCALE,'SCALE')
    m.cdim1 = scale.cdim1
    m.ctype = 'B'
    SELECT (lcbnstygrp)
    APPEND BLANK
    GATHER MEMO MEMVAR
 ENDSCAN
 lctmpordl = lcbnstygrp
 SELECT (lctmpordl)
 INDEX ON piktkt+account+store+cdelivery+cgroupkey+ctype+clocation+stygrop+style+cgrupdetal+STR(lineno, 6) TAG lcgroup ADDITIVE
 SET ORDER TO lcGroup
 LOCATE
 lctmppktkt = ''
 SELECT (lctmpfit)
 llcheckfit = .F.
 LOCATE
 SCAN
    IF lctmppktkt<>piktkt
       lccursty = style
       lccurpiktkt = piktkt
       LOCATE FOR style=lccursty .AND. piktkt=lccurpiktkt .AND.  .NOT. EMPTY(sz1dl)
       SCATTER MEMO MEMVAR
       m.ctype = 'C'
       INSERT INTO (lctmpordl) FROM MEMVAR
       lcrecnum = RECNO()
       lcemploy = ''
       SCAN REST WHILE piktkt=lccurpiktkt
          IF lcemploy<>employee
             SCATTER MEMO MEMVAR
             m.ctype = 'D'
             INSERT INTO (lctmpordl) FROM MEMVAR
             lcemploy = employee
          ENDIF
       ENDSCAN
       GOTO lcrecnum
       lctmppktkt = piktkt
    ENDIF
    SCATTER MEMO MEMVAR
    INSERT INTO (lctmpordl) FROM MEMVAR
    llcheckfit = .T.
 ENDSCAN
 SELECT (lctmpordl)
 SET RELATION TO ORDER + piktkt INTO &lctmpordh
 SET RELATION ADDITIVE TO piktkt INTO piktkt
 SET RELATION ADDITIVE TO 'O'+order INTO ordhdr
 SET RELATION ADDITIVE TO cgroupkey+piktkt INTO (lctmpgroup)
 IF llrpordlnt
    SET RELATION ADDITIVE TO 'O'+order+STR(lineno, 6) INTO ordline
 ENDIF
 SET RELATION ADDITIVE TO style INTO style
 SET RELATION ADDITIVE TO 'S'+scale INTO scale
 SELECT piktkt
 SET RELATION TO cwarecode INTO warehous
 SET RELATION ADDITIVE TO IIF(EMPTY(store), 'M'+account, 'S'+account+store) INTO customer
 SELECT (lctmpordl)
 LOCATE
 SET ORDER TO lcGroup
 STORE ' ' TO lckey
 SCAN
    IF lckey = &lctmpordl..sz1dl
       REPLACE lprnthdr WITH .F.
    ELSE
       REPLACE lprnthdr WITH .T.
    ENDIF
    lckey = &lctmpordl..sz1dl
 ENDSCAN
 LOCATE
 STORE ' ' TO lckey
 SCAN FOR ctype='F'
    IF lckey = &lctmpordl..STYLE+&lctmpordl..SCALE+&lctmpordl..piktkt
       STORE 0 TO lnqty1, lnqty2, lnqty3, lnqty4, lnqty5, lnqty6, lnqty7, lnqty8, lnqty9, lnqty10, lnqty11, lnqty12, lnqty13, lnqty14, lnqty15, lnqty16, lntotqty
       FOR i = 1 TO 16
          lcci = ALLTRIM(STR(i))
          lnqty&lcci = qty&lcci
       ENDFOR
       lntotqty = totqty
       SKIP -1
       FOR i = 1 TO 16
          lcci = ALLTRIM(STR(i))
          IF llogfltch
             REPLACE qty&lcci WITH qty&lcci+lnqty&lcci 
          ENDIF
       ENDFOR
       IF llogfltch
          REPLACE totqty WITH totqty+lntotqty
       ENDIF
       SKIP
       DELETE
    ENDIF
    lckey = &lctmpordl..STYLE+&lctmpordl..SCALE+&lctmpordl..piktkt
 ENDSCAN
 LOCATE
 ord_name = ''
 pik_name = ''
 = fullname()
 SELECT (lcordhdr)
 DELETE FOR EMPTY(order)
 SELECT (lctmpordl)
 SET ORDER TO lcPrIdx
 LOCATE
 llusebin = .F.
 = lfuptotpik()
 DO gfdispre WITH EVALUATE('lcFormName')
 DELETE FOR EMPTY(pikdate)
 IF llrpblksum
    SELECT (lctmpfit)
    ZAP
    INDEX ON clocation+LEFT(style, lnscaposgl+1)+ctype+cdim1 TAG lctmpfit
    SELECT (lcbnstygrp)
    SCAN FOR ctype='B'
       = lfgetsmfit()
    ENDSCAN
    SELECT DISTINCT SUBSTR(style, 1, lnclrposgl+lnstylngl-1) FROM (lctmpfit) INTO CURSOR 'STYLECOLOR'
    lnstyclrcnt = RECCOUNT('STYLECOLOR')
    SELECT (lcbnstygrp)
    lcrpfrmid = lcformbulk
    = gfcrtfrm(lcrpfrmid, "", llogrefform)
    = lfreppltfr(lcrpfrmid)
*!*	    IF llRpBatch
*!*	    	SELECT distinct piktkt FROM (lctmpgroup) INTO CURSOR lcBthPik
*!*	    ENDIF
    SELECT (lctmpfit)
    SET RELATION ADDITIVE TO 'S'+scale INTO scale
    LOCATE
    IF  .NOT. EOF()
       llusebin = .F.
       = lfuptotpik()
	    IF llRpBatch
			 IF  .NOT. USED('PICKPACK')
			    = gfopentable('PICKPACK', 'PICKPACK')
			 ENDIF
			 STORE 0 TO lnBthCnt
	    	SELECT distinct piktkt FROM (lctmpgroup) INTO CURSOR lcBthPik
			 lctimenow = gfgettime()
			 lcScanbatch = gfsequence('SCANBATCH') 
			 SELECT lcBthPik
			 SCAN
			 	IF gfSeek(lcBthPik.piktkt, 'pickpack') AND !pickpack.LCOMPPICK
			 		=gfSeek('O'+pickpack.order,'ORDHDR')
		          SELECT pickpack
		          = gfreplace("CPICKBY   with lcPickBy")
		          = gfreplace("CPSTATUS  with 'Issued'")
		          = gfreplace("DPICKBY   with oAriaApplication.systemdate")
		          = gfreplace("CPICKTIME with lcTimeNow")
		          = gfreplace("SCANBATCH with lcScanbatch")
		          = gfreplace("CCONTREF with ordhdr.ccontref")
		          lnBthCnt=lnBthCnt+1
			 	ENDIF
			 ENDSCAN
			 IF lnBthCnt>0
				 SELECT pickpack
				 = gftableupdate()
			 ELSE
			 	STORE SPACE(0) TO lcScanbatch, lcPickBy
			 ENDIF
	    ENDIF
       SELECT (lctmpfit)
       = gfdispre(lcrpfrmid)
    ENDIF
 ENDIF
 lcformname = 'ALPKTKDR'
 lctmpordl = lcoldname
 SELECT (lcadstygrp)
 SET RELATION TO
 SELECT (lcalasdl)
 SET ORDER TO TAG &lcorddl
 = SEEK(lckeydl)
 = lfbastoclr(lcadstygrp, 'F')
 
**
PROCEDURE lfcreattmp
 SELECT (lctmpordl)
 = AFIELDS(latmpstru)
 lntmpstru = ALEN(latmpstru, 1)
 DIMENSION latmpstru[lntmpstru+38, 18]
 lni = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'StyGrop'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 6
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'StyLoc'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 6
 latmpstru[lntmpstru+lni, 4] = 0
 latmpstru[lntmpstru+3, 1] = 'Account'
 latmpstru[lntmpstru+3, 2] = 'C'
 latmpstru[lntmpstru+3, 3] = 5
 latmpstru[lntmpstru+3, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'CGroupKey'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 6
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Contact'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 30
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'nTotMajPik'
 latmpstru[lntmpstru+lni, 2] = 'N'
 latmpstru[lntmpstru+lni, 3] = 9
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'cDelivery'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 6
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'cConslPikt'
 latmpstru[lntmpstru+lni, 2] = 'M'
 latmpstru[lntmpstru+lni, 3] = 10
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'cStyMajor'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 19
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'cLocation'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 10
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'cType'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 1
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Ponofolo'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 6
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'DatAvlbl'
 latmpstru[lntmpstru+lni, 2] = 'D'
 latmpstru[lntmpstru+lni, 3] = 8
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Status'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 1
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'cdim1'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 5
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Sz1DL'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 5
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Sz2DL'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 5
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Sz3DL'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 5
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Sz4DL'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 5
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Sz5DL'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 5
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Sz6DL'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 5
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Sz7DL'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 5
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Sz8DL'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 5
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Sz9DL'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 5
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Sz10DL'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 5
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Sz11DL'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 5
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Sz12DL'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 5
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Sz13DL'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 5
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Sz14DL'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 5
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Sz15DL'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 5
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Sz16DL'
 latmpstru[lntmpstru+lni, 2] = 'C'
 latmpstru[lntmpstru+lni, 3] = 5
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Qty9'
 latmpstru[lntmpstru+lni, 2] = 'N'
 latmpstru[lntmpstru+lni, 3] = 6
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Qty10'
 latmpstru[lntmpstru+lni, 2] = 'N'
 latmpstru[lntmpstru+lni, 3] = 6
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Qty11'
 latmpstru[lntmpstru+lni, 2] = 'N'
 latmpstru[lntmpstru+lni, 3] = 6
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Qty12'
 latmpstru[lntmpstru+lni, 2] = 'N'
 latmpstru[lntmpstru+lni, 3] = 6
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Qty13'
 latmpstru[lntmpstru+lni, 2] = 'N'
 latmpstru[lntmpstru+lni, 3] = 6
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Qty14'
 latmpstru[lntmpstru+lni, 2] = 'N'
 latmpstru[lntmpstru+lni, 3] = 6
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Qty15'
 latmpstru[lntmpstru+lni, 2] = 'N'
 latmpstru[lntmpstru+lni, 3] = 6
 latmpstru[lntmpstru+lni, 4] = 0
 lni = lni+1
 latmpstru[lntmpstru+lni, 1] = 'Qty16'
 latmpstru[lntmpstru+lni, 2] = 'N'
 latmpstru[lntmpstru+lni, 3] = 6
 latmpstru[lntmpstru+lni, 4] = 0
 FOR lni = 1 TO ALEN(latmpstru, 1)-lntmpstru
    STORE .F. TO latmpstru[lntmpstru+lni, 5], latmpstru[lntmpstru+lni, 6]
    STORE '' TO latmpstru[lntmpstru+lni, 7], latmpstru[lntmpstru+lni, 8], latmpstru[lntmpstru+lni, 9], latmpstru[lntmpstru+lni, 10], latmpstru[lntmpstru+lni, 11], latmpstru[lntmpstru+lni, 12], latmpstru[lntmpstru+lni, 13], latmpstru[lntmpstru+lni, 14], latmpstru[lntmpstru+lni, 15], latmpstru[lntmpstru+lni, 16]
    STORE 0 TO latmpstru[lntmpstru+lni, 17], latmpstru[lntmpstru+lni, 18]
 ENDFOR
 lcadstygrp = gftempname()
 CREATE TABLE (oariaapplication.workdir+lcadstygrp) FROM ARRAY latmpstru
 INDEX ON account+store+cstymajor TAG lcgroup1
 INDEX ON account+store+style TAG lcgroup2 ADDITIVE
 DO CASE
    CASE lcrpprgrs="O"
       IF lcrpprgra='A'
          INDEX ON status+account+store+cdelivery+cgroupkey+piktkt+ctype+stygrop+style+cgrupdetal+STR(lineno, 6) TAG lcgroup ADDITIVE
       ELSE
          INDEX ON piktkt+account+store+cdelivery+cgroupkey+ctype+stygrop+style+cgrupdetal+STR(lineno, 6) TAG lcgroup ADDITIVE
       ENDIF
    CASE lcrpprgrs="P"
       INDEX ON status+piktkt+account+store+cdelivery+cgroupkey+ctype+stygrop+style+cgrupdetal+STR(lineno, 6) TAG lcgroup ADDITIVE
 ENDCASE
 SET ORDER TO lcGroup1
 CREATE TABLE (oariaapplication.workdir+lctmpfit) FROM ARRAY latmpstru
 INDEX ON piktkt+LEFT(style, lnscaposgl+1)+cdim1 TAG lctmpfit
 CREATE TABLE (oariaapplication.workdir+lctmpgroup) (cgroupkey C (6), piktkt C (6))
 INDEX ON cgroupkey+piktkt TAG (lctmpgroup)
 CREATE TABLE (oariaapplication.workdir+lctmpsizes) (scalfld C (2), indxdm2 N (3), cdim1 C (5), llprnsdc L, sz1 C (5), sz2 C (5), sz3 C (5), sz4 C (5), sz5 C (5), sz6 C (5), sz7 C (5), sz8 C (5), sz9 C (5), sz10 C (5), sz11 C (5), sz12 C (5), sz13 C (5), sz14 C (5), sz15 C (5), sz16 C (5))
 INDEX ON scalfld+ALLTRIM(STR(indxdm2)) TAG sortscal
 INDEX ON scalfld+cdim1 TAG (lctmpsizes) ADDITIVE
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
 FOR lnph = 1 TO ALEN(lasoldto, 1)
    IF 'Phone#'$lasoldto(lnph)
       lasoldto[lnph, 1] = SPACE(0)
    ENDIF
 ENDFOR
 FOR lnph = 1 TO ALEN(lashipto, 1)
    IF 'Phone#'$lashipto(lnph)
       lashipto[lnph, 1] = SPACE(0)
    ENDIF
 ENDFOR
 RETURN ""
ENDFUNC
**
PROCEDURE lfusebin
 PRIVATE lcrepsty, lccurpktkt, lcrepbin
 IF  .NOT. USED('PKBINLOC')
    = gfopentable('PKBINLOC', 'PKLINE', 'SH')
 ELSE
    SELECT pkbinloc
    gfsetorder('PKLINE')
 ENDIF
 = lfcrttmp()
 lcrepsty = ''
 lccurpktkt = ''
 lcrepbin = ''
 SELECT (lctmpordl)
 SCAN
    SCATTER MEMO MEMVAR
    lcbnpiktkt = piktkt
    IF  .NOT. (OCCURS(',', m.cconslpikt)>0)
       INSERT INTO (lcreptsty) FROM MEMVAR
    ENDIF
    SELECT pkbinloc
    IF gfseek(lcbnpiktkt+&lctmpordl..cwarecode+STR(&lctmpordl..LINENO,6)+&lctmpordl..STYLE)
       SCAN REST WHILE piktkt+cwarecode+STR(lineno, 6)+style+clocation=ALLTRIM(lcbnpiktkt)
          IF !(pkbinloc.STYLE = &lctmpordl..STYLE) OR &lctmpordl..totpik = 0
             LOOP
          ENDIF
          FOR lnci = 1 TO 8
             lcci = ALLTRIM(STR(lnci, 1))
             m.pik&lcci  =  qty&lcci 
          ENDFOR
          m.clocation = clocation
          m.cwarecode = cwarecode
          IF  .NOT. SEEK(m.account+m.store+cwarecode+clocation+style+piktkt, lcbnstygrp)
             =SEEK('S'+&lctmpordl..SCALE,'SCALE')
             m.cdim1 = scale.cdim1
             SELECT (lcbnstygrp)
             APPEND BLANK
             m.ctype = 'B'
             GATHER MEMO MEMVAR
          ELSE
             SELECT (lcbnstygrp)
             REPLACE pik1 WITH pik1+m.pik1, pik2 WITH pik2+m.pik2, pik3 WITH pik3+m.pik3, pik4 WITH pik4+m.pik4, pik5 WITH pik5+m.pik5, pik6 WITH pik6+m.pik6, pik7 WITH pik7+m.pik7, pik8 WITH pik8+m.pik8
          ENDIF
       ENDSCAN
    ENDIF
 ENDSCAN
 SELECT pkbinloc
 gfsetorder('PKBINPKT')
 lctmpordl = lcbnstygrp
 SELECT (lctmpordl)
 INDEX ON piktkt+account+store+cdelivery+cgroupkey+ctype+clocation+stygrop+style+cgrupdetal+STR(lineno, 6) TAG lcgroup ADDITIVE
 SET ORDER TO lcGroup
 LOCATE
ENDPROC
**
PROCEDURE lfcrttmp
 SELECT (lctmpordl)
 = AFIELDS(latmpstru)
 lntmpstru = ALEN(latmpstru, 1)
 DIMENSION latmpstru[ALEN(latmpstru, 1)+1, 18]
 latmpstru[ALEN(latmpstru, 1), 1] = 'LPrntHdr'
 latmpstru[ALEN(latmpstru, 1), 2] = 'L'
 latmpstru[ALEN(latmpstru, 1), 3] = 1
 latmpstru[ALEN(latmpstru, 1), 4] = 0
 FOR lni = 1 TO ALEN(latmpstru, 1)-lntmpstru
    STORE .F. TO latmpstru[lntmpstru+lni, 5], latmpstru[lntmpstru+lni, 6]
    STORE '' TO latmpstru[lntmpstru+lni, 7], latmpstru[lntmpstru+lni, 8], latmpstru[lntmpstru+lni, 9], latmpstru[lntmpstru+lni, 10], latmpstru[lntmpstru+lni, 11], latmpstru[lntmpstru+lni, 12], latmpstru[lntmpstru+lni, 13], latmpstru[lntmpstru+lni, 14], latmpstru[lntmpstru+lni, 15], latmpstru[lntmpstru+lni, 16]
    STORE 0 TO latmpstru[lntmpstru+lni, 17], latmpstru[lntmpstru+lni, 18]
 ENDFOR
 lcbnstygrp = gftempname()
 CREATE TABLE (oariaapplication.workdir+lcbnstygrp) FROM ARRAY latmpstru
 INDEX ON clocation+account+store+cstymajor TAG lcgroup1
 INDEX ON clocation+account+store+style TAG lcgroup2 ADDITIVE
 INDEX ON piktkt+employee+ctype+style+cgrupdetal+STR(lineno, 6) TAG lcpridx ADDITIVE
 DO CASE
    CASE lcrpprgrs="O"
       IF lcrpprgra='A'
          INDEX ON status+account+store+cdelivery+cgroupkey+piktkt+ctype+clocation+stygrop+style+cgrupdetal+STR(lineno, 6) TAG lcgroup ADDITIVE
       ELSE
          INDEX ON status+piktkt+account+store+cdelivery+cgroupkey+ctype+clocation+stygrop+style+cgrupdetal+STR(lineno, 6) TAG lcgroup ADDITIVE
       ENDIF
    CASE lcrpprgrs="P"
       INDEX ON piktkt+account+store+cdelivery+cgroupkey+ctype+clocation+stygrop+style+cgrupdetal+STR(lineno, 6) TAG lcgroup ADDITIVE
 ENDCASE
 INDEX ON cwarecode+clocation+style TAG lcgroup3
 SET ORDER TO lcGroup3
 lcreptsty = gftempname()
 CREATE TABLE (oariaapplication.workdir+lcreptsty) FROM ARRAY latmpstru
 INDEX ON account+store+piktkt+style TAG lcreptsty ADDITIVE
ENDPROC
**
PROCEDURE lfaddfield
 PARAMETER lcstruarry, lcfldname, lcfldtype, lnfldlen, lnflddec
 lnfldpos  = ALEN(&lcstruarry,1) + IIF(TYPE('&lcStruArry') = 'L', 0 , 1 )
 DIMENSION &lcstruarry[lnfldpos , 4]
 &lcstruarry[lnfldpos , 1]	= lcfldname
 &lcstruarry[lnfldpos , 2]	= lcfldtype
 &lcstruarry[lnfldpos , 3]	= lnfldlen
 &lcstruarry[lnfldpos , 4]	= lnflddec
ENDPROC
**
PROCEDURE lfgetsizes
 PRIVATE lcalias
 LOCAL lnszcnt
 lnszcnt = 0
 lcalias = ALIAS()
 PRIVATE lni, lnhdr, lnscalrec, lncontsrt
 STORE 0 TO lni, lnhdr, lnscalrec, lncontsrt
 SELECT scale
 lnscalrec = IIF(EOF('SCALE'), 0, RECNO('SCALE'))
 LOCATE
 IF SEEK("S" + SUBSTR(&lctmpordl..STYLE , lnscaposgl , 2))
    lncontsrt = 1
    SCAN FOR type+scale+prepak="S"+lckey
       SCATTER MEMO MEMVAR
       SELECT (lctmpsizes)
       SET ORDER TO (lctmpsizes)
       IF SEEK(lckey+cdim1, lctmpsizes)
          IF &lctmpsizes..cdim1 == SCALE.cdim1
             lnszcnt = lnszcnt+scale.cnt
             IF lnszcnt>16
                LOOP
             ENDIF
             FOR lncrttmp = 1 TO 8
                lcnumsiz = "Sz"+ALLTRIM(STR(lncrttmp+8))
                lcsizfld = "Sz"+ALLTRIM(STR(lncrttmp))
                IF !EMPTY(SCALE.&lcsizfld)
                   REPLACE &lctmpsizes..&lcnumsiz WITH SCALE.&lcsizfld , &lctmpsizes..cdim1     WITH SCALE.cdim1
                ENDIF
             ENDFOR
          ELSE
             lncontsrt = lncontsrt+1
             APPEND BLANK
             GATHER MEMO MEMVAR
             REPLACE &lctmpsizes..scalfld WITH LEFT(SCALE.SCALE,2) , &lctmpsizes..cdim1   WITH SCALE.cdim1         , &lctmpsizes..indxdm2 WITH lncontsrt
             lnszcnt = scale.cnt
          ENDIF
       ELSE
          APPEND BLANK
          GATHER MEMO MEMVAR
          REPLACE &lctmpsizes..scalfld WITH LEFT(SCALE.SCALE,2) , &lctmpsizes..indxdm2 WITH 1
          lnszcnt = scale.cnt
       ENDIF
    ENDSCAN
 ENDIF
 SELECT (lctmpsizes)
 SET ORDER TO SortScal
 REPLACE &lctmpsizes..llprnsdc WITH .T.
 SELECT scale
 IF lnscalrec<>0
    GOTO lnscalrec IN scale
 ENDIF
 SELECT (lcalias)
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
 gfseek('0001'+&lctmpfit..STYLE,'POSLN','POSLNS')
 SCAN REST WHILE cinvtype+STYLE+cbusdocu+cstytype+po+STR(LINENO,6)+trancd  = '0001'+&lctmpfit..STYLE FOR  gfseek(cbusdocu+cstytype+po,'POSHDR','POSHDR') AND poshdr.STATUS $ "HO"
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
PROCEDURE lfgetfit
 PRIVATE lnqty1, lnqty2, lnqty3, lnqty4, lnqty5, lnqty6, lnqty7, lnqty8, lntotqty
 STORE 0 TO lnqty1, lnqty2, lnqty3, lnqty4, lnqty5, lnqty6, lnqty7, lnqty8, lntotqty
 SELECT (lctmpordl)
 FOR lnci = 1 TO 8
    lcci = ALLTRIM(STR(lnci, 1))
    IF !EMPTY(&lctmpordl..qty&lcci) AND (&lctmpordl..qty&lcci > &lctmpordl..pik&lcci ) 
       lnqty&lcci  =  (&lctmpordl..qty&lcci - &lctmpordl..pik&lcci ) 
       lntotqty    = lntotqty +lnqty&lcci
    ENDIF
 ENDFOR
 IF  .NOT. EMPTY(lntotqty)
    = SEEK('S'+scale, 'SCALE')
    IF  .NOT. SEEK(piktkt+LEFT(style, lnscaposgl+1)+scale.cdim1, lctmpfit)
       SCATTER MEMO MEMVAR
       SELECT (lctmpfit)
       APPEND BLANK
       m.ctype = 'F'
       =SEEK( LEFT(&lctmpordl..SCALE,2)+SCALE.cdim1 , lctmpsizes , lctmpsizes )
       lnci2 = 0
       FOR lnci = 1 TO 16
          lcci = ALLTRIM(STR(lnci))
          IF (SCALE.sz1 <> &lctmpsizes..sz&lcci)
             LOOP
          ELSE
             lnci2 = lnci-1
             EXIT
          ENDIF
       ENDFOR
       FOR lnci = 1 TO 8
          lcci = ALLTRIM(STR(lnci, 1))
          m.qty&lcci  = 0
       ENDFOR
       FOR lnci = 1 TO 8
          lcci = ALLTRIM(STR(lnci, 1))
          lcci3 = ALLTRIM(STR(lnci+lnci2))
          m.qty&lcci3  = lnqty&lcci  
       ENDFOR
       m.totqty = lntotqty
       m.cdim1 = scale.cdim1
       GATHER MEMO MEMVAR
       IF lfopenpo()
          REPLACE &lctmpfit..ponofolo WITH lcopenpo , &lctmpfit..datavlbl WITH ldavalbl
       ENDIF
       =SEEK( LEFT(&lctmpordl..SCALE,2)+SCALE.cdim1 , lctmpsizes , lctmpsizes )
       FOR lncrttmp = 1 TO 16
          lcnumsiz = "Sz"+ALLTRIM(STR(lncrttmp))+"dl"
          lcsizfld = "Sz"+ALLTRIM(STR(lncrttmp))
          IF !EMPTY(&lctmpsizes..&lcsizfld)
             REPLACE &lctmpfit..&lcnumsiz WITH &lctmpsizes..&lcsizfld
          ENDIF
       ENDFOR
    ELSE
       SELECT (lctmpfit)
       LOCAL lni, lci
       SCATTER FIELDS sz1dl, sz2dl, sz3dl, sz4dl, sz5dl, sz6dl, sz7dl, sz8dl, sz9dl, sz10dl, sz11dl, sz12dl, sz13dl, sz14dl, sz15dl, sz16dl TO laszdl 
       FOR lni = 1 TO scale.cnt
          lci = STR(lni, 1)
          lcszpos = ALLT(STR(ASCAN(laszdl,SCALE.sz&lci)))
          IF lcszpos<>'0'
             REPLACE qty&lcszpos WITH qty&lcszpos + lnqty&lci  totqty      WITH totqty + lnqty&lci
          ENDIF
       ENDFOR
    ENDIF
 ENDIF
 SELECT (lctmpordl)
ENDPROC
**
FUNCTION lfvostat
 PARAMETER lcdummy
 CLEAR READ
 lcdummy = .T.
 RETURN lcdummy
ENDFUNC
**
PROCEDURE lfgetsmfit
 PRIVATE lnqty1, lnqty2, lnqty3, lnqty4, lnqty5, lnqty6, lnqty7, lnqty8, lntotqty
 STORE 0 TO lnqty1, lnqty2, lnqty3, lnqty4, lnqty5, lnqty6, lnqty7, lnqty8, lntotqty
 SELECT (lcbnstygrp)
 FOR lnci = 1 TO 8
    lcci = ALLTRIM(STR(lnci, 1))
    IF !EMPTY(&lcbnstygrp..pik&lcci) 
       lnqty&lcci  =  &lcbnstygrp..pik&lcci 
       lntotqty    = lntotqty +lnqty&lcci
    ENDIF
 ENDFOR
 IF  .NOT. EMPTY(lntotqty)
    IF  .NOT. SEEK(clocation+LEFT(style, lnscaposgl+1), lctmpfit)
       SCATTER MEMO MEMVAR
       SELECT (lctmpfit)
       APPEND BLANK
       m.ctype = 'A'
       GATHER MEMO MEMVAR
       =SEEK( LEFT(&lcbnstygrp..SCALE,2) , lctmpsizes )
       SELECT (lctmpsizes)
       LOCAT REST WHILE scalfld+ALLTRIM(STR(indxdm2))=LEFT(&lcbnstygrp..SCALE,2) FOR cdim1 = &lcbnstygrp..cdim1
       SELECT (lctmpfit)
       FOR lncrttmp = 1 TO 16
          lcnumsiz = "Sz"+ALLTRIM(STR(lncrttmp))+"dl"
          lcsizfld = "Sz"+ALLTRIM(STR(lncrttmp))
          IF !EMPTY(&lctmpsizes..&lcsizfld)
             REPLACE &lctmpfit..&lcnumsiz WITH &lctmpsizes..&lcsizfld
          ENDIF
       ENDFOR
    ENDIF
    SELECT (lcbnstygrp)
    = SEEK('S'+scale, 'SCALE')
    IF  .NOT. SEEK(clocation+LEFT(style, lnscaposgl+1)+'F'+scale.cdim1, lctmpfit)
       SCATTER MEMO MEMVAR
       SELECT (lctmpfit)
       APPEND BLANK
       m.ctype = 'F'
       =SEEK( LEFT(&lctmpordl..SCALE,2) , lctmpsizes )
       SELECT (lctmpsizes)
       LOCAT REST WHILE scalfld+ALLTRIM(STR(indxdm2))=LEFT(&lctmpordl..SCALE,2) FOR cdim1 = &lctmpordl..cdim1
       SELECT (lctmpfit)
       FOR lncrttmp = 1 TO 16
          lcnumsiz = "Sz"+ALLTRIM(STR(lncrttmp))+"dl"
          lcsizfld = "Sz"+ALLTRIM(STR(lncrttmp))
          IF !EMPTY(&lctmpsizes..&lcsizfld)
             m.&lcnumsiz = &lctmpsizes..&lcsizfld
          ENDIF
       ENDFOR
       lnci2 = 0
       FOR lnci = 1 TO 16
          lcci = ALLTRIM(STR(lnci))
          IF (SCALE.sz1 <> &lctmpsizes..sz&lcci)
             LOOP
          ELSE
             lnci2 = lnci-1
             EXIT
          ENDIF
       ENDFOR
       FOR lnci = 1 TO 8
          lcci = ALLTRIM(STR(lnci, 1))
          m.qty&lcci  = 0
       ENDFOR
       FOR lnci = 1 TO 8
          lcci = ALLTRIM(STR(lnci, 1))
          lcci3 = ALLTRIM(STR(lnci+lnci2))
          m.qty&lcci3  = lnqty&lcci  
       ENDFOR
       m.totqty = lntotqty
       m.cdim1 = scale.cdim1
       GATHER MEMO MEMVAR
    ELSE
       =SEEK( LEFT(&lctmpordl..SCALE,2) , lctmpsizes )
       SELECT (lctmpsizes)
       LOCAT REST WHILE scalfld+ALLTRIM(STR(indxdm2))=LEFT(&lctmpordl..SCALE,2) FOR cdim1 = &lctmpordl..cdim1
       SELECT (lctmpfit)
       FOR lncrttmp = 1 TO 16
          lcnumsiz = "Sz"+ALLTRIM(STR(lncrttmp))+"dl"
          lcsizfld = "Sz"+ALLTRIM(STR(lncrttmp))
          IF !EMPTY(&lctmpsizes..&lcsizfld)
             REPLACE &lctmpfit..&lcnumsiz WITH &lctmpsizes..&lcsizfld
          ENDIF
       ENDFOR
       lnci2 = 0
       FOR lnci = 1 TO 16
          lcci = ALLTRIM(STR(lnci))
          IF (SCALE.sz1 <> &lctmpsizes..sz&lcci)
             LOOP
          ELSE
             lnci2 = lnci-1
             EXIT
          ENDIF
       ENDFOR
       SELECT (lctmpfit)
       FOR lnci = 1 TO 8
          lcci = ALLTRIM(STR(lnci, 1))
          lcci3 = ALLTRIM(STR(lnci+lnci2))
          REPL qty&lcci3 WITH qty&lcci3+lnqty&lcci  
       ENDFOR
       REPLACE totqty WITH lntotqty+totqty
    ENDIF
 ENDIF
 SELECT (lcbnstygrp)
ENDPROC
**
FUNCTION lfgttotpk
 PARAMETER lnreturn, lcemp
 lnreturn = 0
 lnalias = ALIAS()
 STORE '' TO lcpiktkt
 lcpiktkt = &lctmpordl..piktkt
 SELECT piktkt
 = SEEK(lcpiktkt)
 IF piktkt.status='O'
    IF SEEK('O'+piktkt.order, 'ORDLINE')
       SELECT ordline
       SCAN REST FOR piktkt=lcpiktkt .AND. IIF( .NOT. EMPTY(lcemp), employee=lcemp, .T.) WHILE cordtype+order+STR(lineno, 6)='O'+piktkt.order
          lnreturn = lnreturn+totpik
       ENDSCAN
    ENDIF
 ELSE
    IF SEEK(lcpiktkt, 'PIKLINE')
       SELECT pikline
       SCAN REST WHILE piktkt+order+STR(lineno, 6)=lcpiktkt
          lnreturn = lnreturn+totpik
       ENDSCAN
    ENDIF
 ENDIF
 SELECT (lnalias)
 RETURN lnreturn
ENDFUNC
**
FUNCTION lfgttotpk2
 PARAMETER lnreturn
 lnreturn = 0
 lnalias = ALIAS()
 SELECT piktkt
 =SEEK(&lctmpordl..piktkt)
 IF piktkt.status='O'
    IF SEEK('O'+&lctmpordl..ORDER,'ORDLINE')
       SELECT ordline
       SCAN REST WHILE cordtype+ORDER+STR(LINENO,6) = 'O' + &lctmpordl..ORDER
          IF &lctmpordl..piktkt <> ordline.piktkt
             LOOP
          ENDIF
          lnreturn = lnreturn+totpik
       ENDSCAN
    ENDIF
 ELSE
    IF SEEK(&lctmpordl..piktkt,'PIKLINE')
       SELECT pikline
       SCAN REST WHILE piktkt+ORDER+STR(LINENO,6) = &lctmpordl..piktkt
          lnreturn = lnreturn+totpik
       ENDSCAN
    ENDIF
 ENDIF
 SELECT (lnalias)
 RETURN lnreturn
ENDFUNC
**
FUNCTION lfgttotpk3
 PARAMETER lnreturn
 lnreturn = 0
 lnalias = ALIAS()
 SELECT pkbinloc
 lcoldord = ORDER()
 = gfsetorder('Pkbinpkt')
 =gfseek(&lctmpordl..piktkt)
 SCAN REST WHILE piktkt+cwarecode+clocation+STYLE = &lctmpordl..piktkt
    lnreturn = lnreturn+totqty
 ENDSCAN
 SELECT pkbinloc
 gfsetorder(lcoldord)
 SELECT (lnalias)
 RETURN lnreturn
ENDFUNC
**
FUNCTION lfchkshpad
 LOCAL lashipto, lnslct
 lnslct = SELECT(0)
 DIMENSION lashipto[5]
 lashipto = ''
 LOCAL lcshptname
 lcshptname = ''
 lnlastline = 0
 lnlastlinx = 0
 SELECT (lcordhdr)
 =SEEK('O'+ &lctmpordl..ORDER)
 IF &lcordhdr..alt_shpto
    SELECT (lcordhdr)
    lcshptname = stname
    lashipto[1] = caddress1
    lashipto[2] = caddress2
    lashipto[3] = caddress3
    lashipto[4] = caddress4
    lashipto[5] = caddress5
 ELSE
    =SEEK(IIF(!EMPTY(&lctmpordl..STORE),'S','M')+ &lcordhdr..account+&lcordhdr..STORE,'Customer' ) 
    lcshptname = IIF( .NOT. EMPTY(lcshptname), lcshptname, customer.stname)
    lashipto[1] = IIF( .NOT. EMPTY(lashipto(1)), lashipto(1), customer.caddress1)
    lashipto[2] = IIF( .NOT. EMPTY(lashipto(2)), lashipto(2), customer.caddress2)
    lashipto[3] = IIF( .NOT. EMPTY(lashipto(3)), lashipto(3), customer.caddress3)
    lashipto[4] = IIF( .NOT. EMPTY(lashipto(4)), lashipto(4), customer.caddress4)
    lashipto[5] = IIF( .NOT. EMPTY(lashipto(5)), lashipto(5), customer.caddress5)
 ENDIF
 lcret = ALLTRIM(lcshptname)+IIF( .NOT. EMPTY(lashipto(1)), ", ", '')+ALLTRIM(lashipto(1))+IIF( .NOT. EMPTY(lashipto(2)), ", ", '')+ALLTRIM(lashipto(2))+IIF( .NOT. EMPTY(lashipto(3)), ", ", '')+ALLTRIM(lashipto(3))+IIF( .NOT. EMPTY(lashipto(4)), ", ", '')+ALLTRIM(lashipto(4))+IIF( .NOT. EMPTY(lashipto(5)), ", ", '')+ALLTRIM(lashipto(5))
 SELECT (lnslct)
 RETURN lcret
ENDFUNC
**
FUNCTION lfdelphon
 PARAMETER lcreturn
 FOR lnph = 1 TO ALEN(lasoldto, 1)
    IF 'Phone#'$lasoldto(lnph)
       lasoldto[lnph, 1] = SPACE(0)
    ENDIF
 ENDFOR
 FOR lnph = 1 TO ALEN(lashipto, 1)
    IF 'Phone#'$lashipto(lnph)
       lashipto[lnph, 1] = SPACE(0)
    ENDIF
 ENDFOR
 RETURN ""
ENDFUNC
**
PROCEDURE fullname
 lcals = ALIAS()
 IF  .NOT. USED('SYUUSER_F')
    = gfopentable('SYUUSER', 'CUSER_ID', 'SH', 'SYUUSER_F')
 ENDIF
 SELECT syuuser_f
 gfseek(ordhdr.cadd_user, 'SYUUSER_F', 'CUSER_ID')
 ord_name = syuuser_f.cusr_name
 gfseek(piktkt.cadd_user, 'SYUUSER_F', 'CUSER_ID')
 pik_name = syuuser_f.cusr_name
 SELECT (lcals)
ENDPROC
**
FUNCTION lflstln
 lctickempl = &lctmpordl..piktkt+&lctmpordl..employee
 lnlastline = 0
 lnlastlinx = 0
 SELECT (lctmpordl)
 lcalias = ALIAS()
 lnrecno = RECNO()
 SCAN FOR piktkt+employee=lctickempl .AND. ctype="B"
    lnlastline = lineno
 ENDSCAN
 LOCATE
 SCAN FOR piktkt+employee=lctickempl .AND. ctype="F"
    lnlastlinx = lineno
 ENDSCAN
 IF BETWEEN(lnrecno, 1, RECCOUNT())
    GOTO lnrecno
 ENDIF
 SELECT (lcalias)
 RETURN ''
ENDFUNC
**
FUNCTION lfgttotval
 PARAMETER lnreturn
 lnreturn = 0
 lnalias = ALIAS()
 STORE '' TO lcpiktkt
 lcpiktkt = &lctmpordl..piktkt
 SELECT piktkt
 LOCATE
 = SEEK(lcpiktkt)
 IF piktkt.status='O'
    IF SEEK('O'+piktkt.order, 'ORDLINE')
       SELECT ordline
       SCAN REST FOR piktkt=lcpiktkt WHILE cordtype+order+STR(lineno, 6)='O'+piktkt.order
          lnreturn = lnreturn+totpik*price
       ENDSCAN
    ENDIF
 ELSE
    IF SEEK(lcpiktkt, 'PIKLINE')
       SELECT pikline
       SCAN REST WHILE piktkt+order+STR(lineno, 6)=lcpiktkt
          lnreturn = lnreturn+totpik*price
       ENDSCAN
    ENDIF
 ENDIF
 SELECT (lnalias)
 RETURN lnreturn
ENDFUNC
**
PROCEDURE lfuptotpik
 lncnt = IIF(RECCOUNT()>0, RECCOUNT(), 1)
 DIMENSION lasty[1, lncnt]
 SCAN FOR  .NOT. EOF()
    lcstymaj = SUBSTR(style, lnstyposgl, lnstylngl)
    lcpktkt = piktkt
    lcemp = employee
    lnstytot = 0
    IF ASCAN(lasty, piktkt+lcemp+lcstymaj)=0
       lasty[1, RECNO()] = piktkt+lcemp+lcstymaj
       lnrecn = RECNO()
       SUM totpik TO lnstytot FOR piktkt+employee+ctype+style=lcpktkt+lcemp+'B'+lcstymaj
       LOCATE FOR piktkt+employee+ctype+style=lcpktkt+lcemp+'B'+lcstymaj
       REPLACE ntotmajpik WITH lnstytot FOR piktkt+employee+ctype+style=lcpktkt+lcemp+'B'+lcstymaj
       GOTO lnrecn
    ELSE
       LOOP
    ENDIF
 ENDSCAN
ENDPROC
**
*** 
*** ReFox - all is not lost 
***
