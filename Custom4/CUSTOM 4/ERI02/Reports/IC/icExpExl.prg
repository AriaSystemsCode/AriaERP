*:***************************************************************************
*: Program file  : ICEXPEXL.PRG
*: Program desc. : Custom Production Position report 
*: System        : Aria Advantage Series.
*: Module        : IC
*: Developer     : Mariam Mazhar(MMT)
*: Date          : 04/30/2015
*: Reference     : C201671{T20150406.0014}
*:***************************************************************************
*B611049,1 MMT 08/30/2015 Issue3 and 4:Incorrect calculations in Custom Production position report[T20150406.0014]
*C201843,1 MMT 07/12/2016 Modify Production Position report filtering by style group[T20160601.0055]
*C201843,2 MMT 07/17/2016 Style filter needs to be adjusted for the style group filter change[T20160601.0055]
*C201843,3 MMT 07/21/2016 Style filter needs to be adjusted for the style group filter change[T20160601.0055]
*C201864,1 MMT 09/06/2016 Change Type Filter to INLIST in Custom Production Position[P20160628.0001]
*:***************************************************************************
#include  r:\aria4xp\reports\ic\icstkjl.h
ldWipDateStart ={}
ldWipDateEnd={}

ldTRNSDateStart={}
ldTRNSDateEnd={}

ldORDDateStart={}
ldORDDateEnd={}
lnDatePos = ASCAN(loogscroll.laOGFxFlt,'INVTADJ.DATE')
IF lnDatePos > 0
  lnDatePos = ASUBSCRIPT(loogscroll.laOGFxFlt,lnDatePos,1)
  IF !EMPTY(loogscroll.laOGFxFlt[lnDatePos,6])
    lnSepPos=AT('|',loogscroll.laOGFxFlt[lnDatePos,6])
    ldWipDateStart = CTOD(SUBSTR(loogscroll.laOGFxFlt[lnDatePos,6],1,lnSepPos-1))
    ldWipDateEnd   = CTOD(SUBSTR(loogscroll.laOGFxFlt[lnDatePos,6],lnSepPos+1))
  ENDIF
ENDIF

lnDatePos = ASCAN(loogscroll.laOGFxFlt,'INVTADJ.TRANDATE')
IF lnDatePos > 0
  lnDatePos = ASUBSCRIPT(loogscroll.laOGFxFlt,lnDatePos,1)
  IF !EMPTY(loogscroll.laOGFxFlt[lnDatePos,6])
    lnSepPos=AT('|',loogscroll.laOGFxFlt[lnDatePos,6])
    ldTRNSDateStart= CTOD(SUBSTR(loogscroll.laOGFxFlt[lnDatePos,6],1,lnSepPos-1))
    ldTRNSDateend= CTOD(SUBSTR(loogscroll.laOGFxFlt[lnDatePos,6],lnSepPos+1))
  ENDIF
ENDIF

lnDatePos = ASCAN(loogscroll.laOGFxFlt,'INVTADJ.ORDDATE')
IF lnDatePos > 0
  lnDatePos = ASUBSCRIPT(loogscroll.laOGFxFlt,lnDatePos,1)
  IF !EMPTY(loogscroll.laOGFxFlt[lnDatePos,6])
    lnSepPos=AT('|',loogscroll.laOGFxFlt[lnDatePos,6])
    ldORDDateStart= CTOD(SUBSTR(loogscroll.laOGFxFlt[lnDatePos,6],1,lnSepPos-1))
    ldORDDateEnd= CTOD(SUBSTR(loogscroll.laOGFxFlt[lnDatePos,6],lnSepPos+1))
  ENDIF
ENDIF

*C201864,1 MMT 09/06/2016 Change Type Filter to INLIST in Custom Production Position[P20160628.0001][Start]
lcGroupString = ''
lnGrpPos = ASCAN(loogscroll.laOGFxFlt,'STYLE.CSTYGROUP')
IF lnGrpPos > 0
  IF USED(lcGroupCursor)
    SELECT (lcGroupCursor)
    ZAP
  ENDIF
  lnGrpPos = ASUBSCRIPT(loogscroll.laOGFxFlt,lnGrpPos,1)
  lcGroups= loOgScroll.laOgFxFlt[lnGrpPos,6]
  lcGroupString = STRTRAN(lcGroups,'|',',')
  IF !EMPTY(lcGroups)
    DIMENSION laTempacstru[1,4]
    laTempacstru[1,1]='CSTYGROUP'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]= 6
    laTempacstru[1,4]= 0
    =gfCrtTmp(lcGroupCursor,@laTempacstru,"CSTYGROUP",lcGroupCursor,.T.)
    lnStart=1
    lnEnd=AT('|',lcGroups)
    DO WHILE lnEnd <> 0
      SELECT(lcGroupCursor)
      APPEND BLANK
      REPLACE CSTYGROUP WITH SUBSTR(lcGroups,lnStart,lnEnd-1)
      lcGroups= STUFF(lcGroups,lnStart,lnEnd,"")
      lnEnd=AT('|',lcGroups)
    ENDDO
    IF lnEnd = 0
      SELECT(lcGroupCursor)
      APPEND BLANK
      REPLACE CSTYGROUP WITH lcGroups
    ENDIF
  ENDIF  
ENDIF
*C201864,1 MMT 09/06/2016 Change Type Filter to INLIST in Custom Production Position[P20160628.0001][End]


IF USED("curselect_x")
  USE IN curselect_x
ENDIF

CREATE CURSOR curselect_x (style C (7))
SELECT curselect_x
INDEX ON style TAG thestyle


IF USED("CURPOWIP")
  USE IN curpowip
ENDIF
IF USED("CURSTYLE")
  USE IN curstyle
ENDIF
IF USED("CURWIPTRN")
  USE IN curwiptrn
ENDIF
IF USED("CURORDERS")
  USE IN curorders
ENDIF
SELECT curselect_x
ZAP
llStyleSelected = .F.
lcFirstSty = ''
lcLastSty = ''
lnOrdPos = ASCAN(loOGScroll.laogFxflt,'STYLE.CSTYMAJOR')
IF lnOrdPos > 0
  lnOrdPos = ASUBSCRIPT(loOGScroll.laogFxflt,lnOrdPos,1)
  lcOrders = loOGScroll.laogFxflt[lnOrdPos,6]
  IF !EMPTY(lcOrders)
    IF USED(lcOrders)
      SELECT(lcOrders)
      GO BOTTOM 
      lcLastSty = &lcOrders..cStyMajor
      LOCATE 
      lcFirstSty = &lcOrders..cStyMajor
      SCAN 
        llStyleSelected = .T.
        INSERT INTO 'curselect_x' VALUES (&lcOrders..cStyMajor)
      ENDSCAN
    ENDIF
  ENDIF
ENDIF

lbodies = ""
IF lcRpRepF  = "S"
     lbodies = " and right(alltrim(LEFT(b.style,12)),1) <> 'B'"
ELSE
  IF lcRpRepF  = "B"
   lbodies = " and right(alltrim(LEFT(b.style,12)),1) = 'B'"
  ENDIF
ENDIF
lhatbag = ''
*C201843,1 MMT 07/12/2016 Modify Production Position report filtering by style group[T20160601.0055][Start]
*!*	IF lcRpType = "H"
*!*	     lhatbag = " and LEFT(b.style,1) = '1' "
*!*	ELSE
*!*	  IF lcRpType = 'B'
*!*	    lhatbag = " and LEFT(b.style,1) = '2' "
*!*	  ENDIF  
*!*	ENDIF
*C201864,1 MMT 09/06/2016 Change Type Filter to INLIST in Custom Production Position[P20160628.0001][Start]
*!*	IF lcRpType = "H"
*!*	   lhatbag = " and IIF(SEEK(b.Style,'STYLE','STYLE'),STYLE.cStyGroup  = 'H     ',.F.) "
*!*	ELSE
*!*	  IF lcRpType = 'B'
*!*	    lhatbag = " and IIF(SEEK(b.Style,'STYLE','STYLE'),STYLE.cStyGroup = 'B     ',.F.) "
*!*	  ENDIF  
*!*	ENDIF
IF !EMPTY(lcGroupCursor) AND USED(lcGroupCursor) AND RECCOUNT(lcGroupCursor)>0
  lhatbag = " and IIF(SEEK(b.Style,'STYLE','STYLE'),SEEK(STYLE.cStyGroup,'"+lcGroupCursor+"'),.F.)"
ENDIF
*C201864,1 MMT 09/06/2016 Change Type Filter to INLIST in Custom Production Position[P20160628.0001][End]
*C201843,1 MMT 07/12/2016 Modify Production Position report filtering by style group[T20160601.0055][End]
lselstyle = ""
IF llStyleSelected 
*!*	     IF  .NOT. EMPTY(lcFirstSty)
*!*	          lselstyle = " And b.style >= '" + ALLTRIM(lcFirstSty) + "'"
*!*	     ENDIF
*!*	     IF  .NOT. EMPTY(lcLastSty)
*!*	          lselstyle = lselstyle + " And b.style <= '" + ALLTRIM(lcLastSty) + "'"
*!*	     ENDIF
   lselstyle = " AND SEEK(PADR(SUBSTR(b.STYLE,1,12),19),lcOrders) "   
ENDIF



llSeason =.F.
lcSeaonsString = ''
lcSeaonsString2 = ''
lnPosSEAON = ASCAN(loOgScroll.laOgFXFlt,"STYLE.SEASON")
IF lnPosSEAON > 0
  IF USED(lcSEASON)
    SELECT (lcSEASON)
    ZAP
  ENDIF
  lnPosSEAON  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosSEAON,1)
  lcSeaonsString= loOgScroll.laOgFxFlt[lnPosSEAON ,6]
  lcSeaonsString2 = lcSeaonsString
  IF !EMPTY(lcSeaonsString)
    llSeason =.T.
   * lcStyColorCursor = loOgScroll.gfTempName()
    DIMENSION laTempacstru[1,4]
    laTempacstru[1,1]='Season'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]= 6
    laTempacstru[1,4]= 0
    =gfCrtTmp(lcSEASON,@laTempacstru,"Season",lcSEASON,.T.)
    lnStart=1
    lnEnd=AT('|',lcSeaonsString)
    DO WHILE lnEnd <> 0
      SELECT(lcSEASON)
      APPEND BLANK
      REPLACE Season WITH SUBSTR(lcSeaonsString,lnStart,lnEnd-1)
      lcSeaonsString = STUFF(lcSeaonsString,lnStart,lnEnd,"")
      lnEnd=AT('|',lcSeaonsString)
    ENDDO
    IF lnEnd = 0
      SELECT(lcSEASON)
      APPEND BLANK
      REPLACE Season WITH lcSeaonsString
    ENDIF
  ENDIF   
ENDIF   
lselseas = ''

IF llSeason 
  lselseas = ' And SEEK(a.season,lcSeason)'
	 DIMENSION laSeasons[1]
     laSeasons = ''
     =GFSUBSTR(lcSeaonsString2 ,@laSeasons,"|")
*!*	     lselseas = ' And INLIST(a.season'
*!*	     FOR i = 1 TO ALEN(laSeasons,1)
*!*	       lselseas = lselseas + ",'" + laSeasons[i] + "' "
*!*	     ENDFOR
*!*	     IF  .NOT. EMPTY(lselseas)
*!*	          lselseas = lselseas + ")"
*!*	     ENDIF
ENDIF
WAIT WINDOW NOWAIT "Please Wait"
 
lcSelect ="SELECT a.cbusdocu,a.nstyorder,a.status,a.receive,a.cstytype, a.po, a.available, a.vendor, b.* "+;
          " FROM poshdr a, posln b  WHERE a.cbusdocu+a.cstytype + a.po " + ;
          " = b.cbusdocu+b.CSTYTYPE+b.PO"+; 
          " and a.status <> 'X'"
          
SELECT POSLN 
=gfSqlRun(lcSelect ,'POSLN',.T.,'POSLN_A')
***
SELECT b.cstytype, b.po, b.available, b.vendor,b.style, SUM(b.totqty) AS TOTQTY ,0000000000 as need,0000000000 as needtr, 0000000000  as ordcur, 00000000000 as ordcurfu   ;
FROM  posln_A b  WHERE  (b.nstyorder > b.receive OR (RIGHT(ALLTRIM(LEFT(b.style,7)),1)= "B") AND LEFT(b.style,1) = '2');
and b.status <> 'X' and (b.status <> 'C' OR (RIGHT(ALLTRIM(LEFT(b.style,7)),1)= "B") AND LEFT(b.style,1) = '2')and b.trancd = '1' &lselstyle &lbodies;
&lhatbag  group BY 1,2,3,4,5 INTO CURSOR curpowip readwrite
****
*!*	SELECT cstytype, po, available, vendor,style, SUM(totqty) AS TOTQTY FROM POSLN_A   group BY 1,2,3,4,5 INTO CURSOR curpowip readwrite
*!*	ALTER TABLE curpowip ADD COLUMN need 'N' (5, 0) ADD COLUMN needtr 'N' (5, 0) ADD COLUMN ordcur 'N' (5, 0) ADD COLUMN ordcurfu 'N' (8, 0)

SELECT distinct a.style, a.totstk, a.totwip, a.totintrn, a.desc, a.totord, a.season,00000 as saks FROM style a, curpowip b WHERE a.style = b.style &lselseas into CURSOR;
curstyle READWRITE
SELECT curstyle
*!*	SET RELATION TO LEFT(style, 7) INTO curselect_x
*!*	IF thisform.optiongroup2.value = 1
*!*	     DELETE IN curstyle FOR  .NOT. EMPTY(curselect_x.style)
*!*	ELSE
*!*	     DELETE IN curstyle FOR EMPTY(curselect_x.style)
*!*	ENDIF
*!*	SET RELATION TO
SELECT a.cbusdocu,a.cstytype, a.po, a.trancd, a.shipno, a.totqty, a.date, a.dadd_date, a.style FROM posln_A a, curpowip b WHERE a.cstytype + a.po =  ;
       b.cstytype + b.po AND a.style = b.style AND a.trancd <> '1' INTO CURSOR curwiptrn READWRITE
       
SELECT curstyle
ALTER TABLE curstyle ADD COLUMN need 'N' (10, 0) ADD COLUMN needtr 'N' (10, 0) ADD COLUMN ordcur 'N' (10, 0) ADD COLUMN licqty 'N' (10,  ;
      0) ADD COLUMN ordcurfu 'N' (12, 0)
      
SELECT curstyle
INDEX ON style TAG style
SELECT style
SET ORDER TO style


lselstyle = STRTRAN(lselstyle, "b.", "a.")
lbodies = STRTRAN(lbodies, "b.", "a.")
lhatbag = STRTRAN(lhatbag, "b.", "a.")
lselseas = STRTRAN(lselseas, "b.", "a.")

SELECT a.start, a.complete,a.style, a.order, b.account, a.custpo, SUM(a.totqty) as totqty  FROM ordline a, ordhdr b  WHERE a.cordtype+a.order = b.cordtype;
+ b.order and b.status <> "X" AND b.cordtype <> "T" and a.totqty <> 0  &lselstyle  &lbodies &lhatbag   &lselseas  group BY 1,2,3,4,5,6 INTO CURSOR curorders;
READWRITE
SELECT curstyle
SET ORDER TO style
SELECT curorders
GOTO TOP
DO WHILE  .NOT. EOF()
     SELECT curselect_x
*!*	     IF thisform.optiongroup2.value = 2
*!*	          IF  .NOT. SEEK(LEFT(curorders.style, 7))
*!*	               SELECT curorders
*!*	               SKIP
*!*	               LOOP
*!*	          ENDIF
*!*	     ELSE
*!*	          IF SEEK(LEFT(curorders.style, 7))
*!*	               SELECT curorders
*!*	               SKIP
*!*	               LOOP
*!*	          ENDIF
*!*	     ENDIF
     SELECT curstyle
     IF  .NOT. SEEK(curorders.style)
          SELECT style
          IF (curorders.start >= ldRpCutDat .AND. curorders.start <= ldORDDateStart) .OR. (curorders.start >=  ;
             ldORDDateStart .AND. curorders.start <= ldORDDateEnd)
          ELSE
               SELECT curorders
               SKIP
               LOOP
          ENDIF
          = SEEK(curorders.style)
          SELECT curstyle
          APPEND BLANK
          REPLACE curstyle.style WITH style.style, curstyle.totstk WITH style.totstk, curstyle.totwip WITH style.totwip,  ;
                  curstyle.totintrn WITH style.totintrn, curstyle.totord WITH style.totord, curstyle.desc WITH style.desc,  ;
                  curstyle.season WITH style.season
     ENDIF
     SELECT curorders
     SKIP
ENDDO

SELECT curstyle
= doxl(ldWipDateStart , ldWipDateEnd,ldTRNSDateStart, ldTRNSDateEnd, ldORDDateStart, ldORDDateEnd, ldRpCutDat, VAL(lcRPMinLIC),  ;
       VAL(lcRPMinHnd))



*!*************************************************************
*! Name      : lfWRunGrid
*! Developer : Mariam Mazhar(MMT)
*! Date      : 04/30/2015
*! Purpose   : When Function
*!*************************************************************
FUNCTION lfWRunGrid
=gfOpenTable("POSLN","POSLN")
=gfOpenTable("POSHDR","POSHDR")
=gfOpenTable("ORDLINE","ORDLINE")
=gfOpenTable("ORDHDR","ORDHDR")
=gfOpenTable('Customer','Customer')
=gfOpenTable('shpmthdr','shpmthdr')
=gfOpenTable('stydye','stydye')
=gfOpenTable('Codes','Codes')
*B611049,1 MMT 08/30/2015 Issue3 and 4:Incorrect calculations in Custom Production position report[T20150406.0014][Start]
*USE (oAriaApplication.ReportHome+"IC\SHIPVIA.DBF") SHARED ORDER SHIPVIA
IF !USED('SHIPVIA')
  USE (oAriaApplication.ReportHome+"IC\SHIPVIA.DBF") SHARED ORDER SHIPVIA IN 0
ENDIF
*B611049,1 MMT 08/30/2015 Issue3 and 4:Incorrect calculations in Custom Production position report[T20150406.0014][End]
*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Mariam Mazhar(MMT)
*! Date      : 04/30/2015
*! Purpose   : Rise change style flag, in range browse screen.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    
    lnPosSEAON = ASCAN(loOgScroll.laOgFXFlt,"STYLE.SEASON")
    IF lnPosSEAON > 0
      IF USED(lcSEASON)
        SELECT (lcSEASON)
        ZAP
      ENDIF
	  lnPosSEAON  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosSEAON,1)
	  lcSeaonsString= loOgScroll.laOgFxFlt[lnPosSEAON ,6]
	  IF !EMPTY(lcSeaonsString)
	   * lcStyColorCursor = loOgScroll.gfTempName()
	    DIMENSION laTempacstru[1,4]
	    laTempacstru[1,1]='Season'
	    laTempacstru[1,2]='C'
	    laTempacstru[1,3]= 6
	    laTempacstru[1,4]= 0
	    =gfCrtTmp(lcSEASON,@laTempacstru,"Season",lcSEASON,.T.)
	    lnStart=1
	    lnEnd=AT('|',lcSeaonsString)
	    DO WHILE lnEnd <> 0
	      SELECT(lcSEASON)
	      APPEND BLANK
	      REPLACE Season WITH SUBSTR(lcSeaonsString,lnStart,lnEnd-1)
	      lcSeaonsString = STUFF(lcSeaonsString,lnStart,lnEnd,"")
	      lnEnd=AT('|',lcSeaonsString)
	    ENDDO
	    IF lnEnd = 0
	      SELECT(lcSEASON)
	      APPEND BLANK
	      REPLACE Season WITH lcSeaonsString
	    ENDIF
      ENDIF   
    ENDIF   
    *C201864,1 MMT 09/06/2016 Change Type Filter to INLIST in Custom Production Position[P20160628.0001][Start]
	lnGrpPos = ASCAN(loogscroll.laOGFxFlt,'STYLE.CSTYGROUP')
	IF lnGrpPos > 0
	  IF USED(lcGroupCursor)
	    SELECT (lcGroupCursor)
	    ZAP
	  ENDIF
	  lnGrpPos = ASUBSCRIPT(loogscroll.laOGFxFlt,lnGrpPos,1)
	  lcGroups= loOgScroll.laOgFxFlt[lnGrpPos,6]
	  IF !EMPTY(lcGroups)
	    DIMENSION laTempacstru[1,4]
	    laTempacstru[1,1]='CSTYGROUP'
	    laTempacstru[1,2]='C'
	    laTempacstru[1,3]= 6
	    laTempacstru[1,4]= 0
	    =gfCrtTmp(lcGroupCursor,@laTempacstru,"CSTYGROUP",lcGroupCursor,.T.)
	    lnStart=1
	    lnEnd=AT('|',lcGroups)
	    DO WHILE lnEnd <> 0
	      SELECT(lcGroupCursor)
	      APPEND BLANK
	      REPLACE CSTYGROUP WITH SUBSTR(lcGroups,lnStart,lnEnd-1)
	      lcGroups= STUFF(lcGroups,lnStart,lnEnd,"")
	      lnEnd=AT('|',lcGroups)
	    ENDDO
	    IF lnEnd = 0
	      SELECT(lcGroupCursor)
	      APPEND BLANK
	      REPLACE CSTYGROUP WITH lcGroups
	    ENDIF
	  ENDIF  
	ENDIF
	*C201864,1 MMT 09/06/2016 Change Type Filter to INLIST in Custom Production Position[P20160628.0001][End]



    SELECT STYLE
    *C201864,1 MMT 09/06/2016 Change Type Filter to INLIST in Custom Production Position[P20160628.0001][Start]
    *IF lcRpRepF <> 'A' OR lcRpType <> 'A' OR (USED(lcSEASON) AND RECCOUNT()> 0)    
    IF lcRpRepF <> 'A' OR (USED(lcGroupCursor) AND RECCOUNT(lcGroupCursor)> 0) OR (USED(lcSEASON) AND RECCOUNT(lcSEASON)> 0)
    *C201864,1 MMT 09/06/2016 Change Type Filter to INLIST in Custom Production Position[P20160628.0001][End]
      *C201843,2 MMT 07/17/2016 Style filter needs to be adjusted for the style group filter change[T20160601.0055][Start]
      *SET FILTER TO IIF(lcRpRepF <> 'A',IIF(lcRpRepF = "B", RIGHT(ALLTRIM(LEFT(style,12)),1) = "B",RIGHT(ALLTRIM(LEFT(style,12)),1) <> 'B'),.T.) AND ;
                    IIF(lcRpType <> 'A',LEFT(style,1) = IIF(lcRpType= "H", "1", "2") ,.T.) AND IIF (USED(lcSEASON) AND RECCOUNT()> 0 , SEEK(Style.season,lcSEASON),.T.)
      *C201864,1 MMT 09/06/2016 Change Type Filter to INLIST in Custom Production Position[P20160628.0001][Start]                     
*!*	      SET FILTER TO IIF(lcRpRepF <> 'A',IIF(lcRpRepF = "B", RIGHT(ALLTRIM(LEFT(style,12)),1) = "B",RIGHT(ALLTRIM(LEFT(style,12)),1) <> 'B'),.T.) AND ;
*!*	                   IIF(lcRpType <> 'A',IIF(lcRpType ='H',STYLE.cStyGroup  = 'H     ',STYLE.cStyGroup  = 'B     ') ,.T.) AND IIF (USED(lcSEASON) AND RECCOUNT()> 0 , SEEK(Style.season,lcSEASON),.T.)
      SET FILTER TO IIF(lcRpRepF <> 'A',IIF(lcRpRepF = "B", RIGHT(ALLTRIM(LEFT(style,12)),1) = "B",RIGHT(ALLTRIM(LEFT(style,12)),1) <> 'B'),.T.) AND ;
                    IIF(USED(lcGroupCursor) AND RECCOUNT(lcGroupCursor)> 0,SEEK(STYLE.cStyGroup,lcGroupCursor) ,.T.) AND;
                    IIF (USED(lcSEASON) AND RECCOUNT(lcSEASON)> 0 , SEEK(Style.season,lcSEASON),.T.)
      *C201864,1 MMT 09/06/2016 Change Type Filter to INLIST in Custom Production Position[P20160628.0001][End]
      *C201843,2 MMT 07/17/2016 Style filter needs to be adjusted for the style group filter change[T20160601.0055][End]              
    ENDIF 
    *lcSEASON   
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    SET FILTER TO 
ENDCASE
*-- end of lfsrvSty.
*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Mariam Mazhar(MMT)
*! Date      : 04/30/2015
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
FUNCTION lfEvalSegs

lnMajSeg   = gfItemMask('SM')  && No. of major segments.
lcMajPict  = gfItemMask("PM")

*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

lnMajorLen = LEN(lcMajPict)
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_ONLYTHESE,oAriaApplication.GetHeaderText("Lang_ONLYTHESE",AHEADERFILE))+' ' + ALLTRIM(lcNonMajTl) + 's'
*-- Define Mover for Inventory adjustments arrays. [end  ]
RETURN ''
*-- end of lfEvalSegs.
*!*************************************************************
*! Name      : lfReSetSty
*! Developer : Mariam Mazhar(MMT)
*! Date      : 04/30/2015
*! Purpose   : Reset style range
*!*************************************************************
FUNCTION  lfReSetSty

lnOrdPos = ASCAN(loOGScroll.laogFxflt,'STYLE.CSTYMAJOR')
IF lnOrdPos > 0
  lnOrdPos = ASUBSCRIPT(loOGScroll.laogFxflt,lnOrdPos,1)
  lcOrders = loOGScroll.laogFxflt[lnOrdPos,6]
  IF !EMPTY(lcOrders)
    IF USED(lcOrders)
      SELECT(lcOrders)
      ZAP
    ENDIF
  ENDIF
ENDIF

*!*************************************************************
*! Name      : doxl
*! Developer : Mariam Mazhar(MMT)
*! Date      : 04/30/2015
*! Purpose   : Export to excel
*!*************************************************************
FUNCTION doxl
PARAMETER pswdate, pewdate, pstdate, petdate, psodate, peodate, pcutoff, plicmin, pohmin
SET CENTURY OFF
PRIVATE lo, llicqty, lprior, lfuture, llastcol
lprior = CTOD("01/01/1901")
lfuture = CTOD("12/31/2199")
*ALTER TABLE curstyle ADD COLUMN saks 'N' (5, 0)
lappended = .F.
SELECT curstyle
INDEX ON style + season TAG repkey
SELECT curwiptrn
INDEX ON style TAG style
INDEX ON po + style TAG postyle
SELECT curorders
INDEX ON style TAG style
SELECT curpowip
INDEX ON style TAG style
SELECT customer
=gfSetOrder('customer')
SELECT poshdr
=gfSetOrder('POSHDR')
SELECT shpmthdr
=gfSetOrder('shpmthdr')
SELECT stydye
=gfSetOrder('STYDYE')
SELECT codes
=gfSetOrder('CODES')
SELECT shipvia
SET ORDER TO shipvia
SELECT style
GOTO TOP
DO WHILE  .NOT. EOF()
     SELECT curselect_x
*!*	     IF lmainform.optiongroup2.value = 2
*!*	          IF  .NOT. SEEK(LEFT(style.style, 7))
*!*	               SELECT style
*!*	               SKIP
*!*	               LOOP
*!*	          ENDIF
*!*	     ELSE
*!*	          IF SEEK(LEFT(style.style, 7))
*!*	               SELECT style
*!*	               SKIP
*!*	               LOOP
*!*	          ENDIF
*!*	     ENDIF
     SELECT style
     SELECT curstyle
     IF SEEK(style.style)
          SELECT style
          SKIP
          LOOP
     ENDIF
*!*	     IF lmainform.optiongroup2.value = 1
*!*	          IF  .NOT. EMPTY(lmainform.text7.value)
*!*	               IF style.style < ALLTRIM(lmainform.text7.value)
*!*	                    SELECT style
*!*	                    SKIP
*!*	                    LOOP
*!*	               ENDIF
*!*	          ENDIF
*!*	          IF  .NOT. EMPTY(lmainform.text8.value)
*!*	               IF style.style > ALLTRIM(lmainform.text8.value)
*!*	                    SELECT style
*!*	                    SKIP
*!*	                    LOOP
*!*	               ENDIF
*!*	          ENDIF
*!*	     ENDIF
     IF lcRpRepF <> "A"
          IF lcRpRepF = "B"
               IF RIGHT(ALLTRIM(LEFT(style.style, 7)), 1) <> "B"
                    SELECT style
                    SKIP
                    LOOP
               ENDIF
          ENDIF
          IF lcRpRepF  = "S"
               IF RIGHT(ALLTRIM(LEFT(style.style, 7)), 1) = "B"
                    SELECT style
                    SKIP
                    LOOP
               ENDIF
          ENDIF
     ENDIF
     *C201843,3 MMT 07/21/2016 Style filter needs to be adjusted for the style group filter change[T20160601.0055][Start]
     if llStyleSelected
       if !Seek(STYLE.CSTYMAJOR,lcOrders)
         SELECT style
         SKIP
         LOOP
       ENDIF
     ENDIF
     *C201843,3 MMT 07/21/2016 Style filter needs to be adjusted for the style group filter change[T20160601.0055][End]
     *C201864,1 MMT 09/06/2016 Change Type Filter to INLIST in Custom Production Position[P20160628.0001][Start]
*!*	     IF lcRpType <> 'A'
*!*	          IF lcRpType= 'H'
*!*	               *C201843,3 MMT 07/21/2016 Style filter needs to be adjusted for the style group filter change[T20160601.0055][Start]
*!*	               *IF LEFT(style.style, 1) <> "1"
*!*	               IF STYLE.cStyGroup  <> 'H     ' &&LEFT(style.style, 1) <> "1"
*!*	               *C201843,3 MMT 07/21/2016 Style filter needs to be adjusted for the style group filter change[T20160601.0055][End]
*!*	                    SELECT style
*!*	                    SKIP
*!*	                    LOOP
*!*	               ENDIF
*!*	          ENDIF
*!*	          IF lcRpType = 'B'
*!*	               *C201843,3 MMT 07/21/2016 Style filter needs to be adjusted for the style group filter change[T20160601.0055][Start]
*!*	               *IF LEFT(style.style, 1) <> "2"
*!*	               IF STYLE.cStyGroup  <> 'B     '
*!*	               *C201843,3 MMT 07/21/2016 Style filter needs to be adjusted for the style group filter change[T20160601.0055][End]
*!*	                    SELECT style
*!*	                    SKIP
*!*	                    LOOP
*!*	               ENDIF
*!*	          ENDIF
*!*	     ENDIF
     IF !EMPTY(lcGroupCursor) AND USED(lcGroupCursor) AND RECCOUNT(lcGroupCursor)> 0
       IF !SEEK(STYLE.cStyGroup,lcGroupCursor)
         SELECT style
         SKIP
         LOOP
       ENDIF
     ENDIF
     *C201864,1 MMT 09/06/2016 Change Type Filter to INLIST in Custom Production Position[P20160628.0001][End]
     lhit = .F.
     IF  .NOT. EMPTY(lselseas) AND !SEEK(style.season,lcseason)
         SELECT style
         SKIP
         LOOP
     ENDIF
     SELECT stydye
     IF SEEK(style.style + "LIC   ") .OR. (LEFT(style.style, 1) = '2' .AND. RIGHT(ALLTRIM(LEFT(style.style, 7)), 1) = "B")
          IF LEFT(style.style, 1) = '2' .AND. RIGHT(ALLTRIM(LEFT(style.style, 7)), 1) = "B"
               SELECT style
               IF (style.totstk < 0 .OR. style.totstk >= plicmin) .OR. (style.totstk > pohmin)
                    SELECT curstyle
                    lappended = .T.
                    APPEND BLANK
                    REPLACE curstyle.style WITH style.style, curstyle.totstk WITH style.totstk, curstyle.totwip WITH style.totwip,  ;
                            curstyle.totintrn WITH style.totintrn, curstyle.totord WITH style.totord, curstyle.desc WITH style.desc,  ;
                            curstyle.season WITH style.season
               ENDIF
          ELSE
               IF (stydye.totstk < 0 .OR. stydye.totstk >= plicmin) .OR. (style.totstk > pohmin)
                    SELECT curstyle
                    lappended = .T.
                    APPEND BLANK
                    REPLACE curstyle.style WITH style.style, curstyle.totstk WITH style.totstk, curstyle.totwip WITH style.totwip,  ;
                            curstyle.totintrn WITH style.totintrn, curstyle.totord WITH style.totord, curstyle.desc WITH style.desc,  ;
                            curstyle.season WITH style.season
               ENDIF
          ENDIF
          SELECT style
          SKIP
          LOOP
     ENDIF
     SELECT style
     SKIP
ENDDO
IF llRpDec   
     SELECT curwiptrn
     SET ORDER TO STYLE
     SELECT curstyle
     GOTO TOP
     DO WHILE  .NOT. EOF()
          SELECT curwiptrn
          SET KEY TO curstyle.style
          GOTO TOP
          lkeep = .F.
          DO WHILE  .NOT. EOF()
               IF curwiptrn.trancd < '3'
                    SELECT curwiptrn
                    SKIP
                    LOOP
               ENDIF
               lkeep = .T.
               EXIT
          ENDDO
          SELECT curstyle
          IF  .NOT. lkeep
               DELETE IN curstyle
          ENDIF
          IF  .NOT. EOF()
               SKIP
          ENDIF
     ENDDO
ENDIF
SELECT curpowip
REPLACE need WITH totqty ALL
GOTO TOP
DO WHILE  .NOT. EOF()
     SELECT curwiptrn
     SET ORDER TO postyle
     SET KEY TO curpowip.po + curpowip.style
     GOTO TOP
     DO WHILE  .NOT. EOF()
          IF curwiptrn.trancd = '2'
               REPLACE curpowip.totqty WITH curpowip.totqty - curwiptrn.totqty, curpowip.need WITH curpowip.need - curwiptrn.totqty
          ENDIF
          SKIP
     ENDDO
     SELECT curpowip
     SKIP
ENDDO
lkeep = .F.
SELECT curstyle
GOTO TOP
DO WHILE  .NOT. EOF()
     IF LEFT(curstyle.style, 1) = '2' .AND. RIGHT(ALLTRIM(LEFT(curstyle.style, 7)), 1) = 'B'
          SELECT style
          SET KEY TO
          IF SEEK(curstyle.style, "style", "style")
               llicqty = style.totstk
               REPLACE curstyle.licqty WITH curstyle.licqty + style.totstk
          ELSE
               llicqty = 0
          ENDIF
          SELECT curstyle
     ELSE
          SELECT stydye
          IF SEEK(curstyle.style + "LIC   ")
               llicqty = stydye.totstk
               REPLACE curstyle.licqty WITH curstyle.licqty + stydye.totstk
          ELSE
               llicqty = 0
          ENDIF
     ENDIF
     SELECT curstyle
     IF llicqty >= plicmin
          SELECT curstyle
          SKIP
          LOOP
     ENDIF
     IF curstyle.totstk >= pohmin
          SKIP
          LOOP
     ENDIF
     lkeep = .F.
     SELECT curpowip
     SET KEY TO curstyle.style
     GOTO TOP
     DO WHILE  .NOT. EOF()
          SELECT curpowip
          IF curpowip.totqty = 0
               SKIP
               LOOP
          ENDIF
          IF curpowip.available < pcutoff
               SKIP
               LOOP
          ENDIF
          lkeep = .T.
          SELECT curpowip
          SKIP
     ENDDO
     SELECT curstyle
     IF lkeep
          SELECT curstyle
          SKIP
          LOOP
     ENDIF
     SELECT curwiptrn
     SET ORDER TO style
     SET KEY TO curstyle.style
     DO WHILE  .NOT. EOF()
          lkeep = .F.
          IF curwiptrn.trancd = '3'
               SELECT shpmthdr
               *B611049,1 MMT 08/30/2015 Issue3 and 4:Incorrect calculations in Custom Production position report[T20150406.0014][Start]
               *IF gfSEEK(curwiptrn.shipno)               
               IF gfSEEK("PP"+curwiptrn.shipno)
               *B611049,1 MMT 08/30/2015 Issue3 and 4:Incorrect calculations in Custom Production position report[T20150406.0014][End]
                    IF shpmthdr.eta < pcutoff
                         SELECT curwiptrn
                         SKIP
                         LOOP
                    ENDIF
               ENDIF
          ENDIF
          SELECT poshdr
          =gfSEEK(curwiptrn.cbusDocu+ curwiptrn.cstytype + curwiptrn.po )
          IF poshdr.available < pcutoff
               SKIP
               LOOP
          ENDIF
          lkeep = .T.
          EXIT
     ENDDO
     IF lkeep
          SELECT curstyle
          SKIP
          LOOP
     ENDIF
     lkeep = .F.
     SELECT curorders
     SET KEY TO curstyle.style
     GOTO TOP
     DO WHILE  .NOT. EOF()
          lkeep = .F.
          IF curorders.start < pcutoff
               SKIP
               LOOP
          ENDIF
          lkeep = .T.
          EXIT
     ENDDO
     SELECT curstyle
     IF  .NOT. lkeep
          IF RIGHT(ALLTRIM(LEFT(curstyle.style, 7)), 1) = "B" .AND. curstyle.licqty <> 0
          ELSE
               DELETE
          ENDIF
     ENDIF
     SKIP
ENDDO
SELECT curstyle
COUNT TO x
IF x = 0
     *= MESSAGEBOX("no records selected", 0, "ERIC JAVITS INC")
     =gfModalGen('TRM00052B40011','ALERT')
     RETURN
ENDIF
lo = CREATEOBJECT("excel.application")
lo.workbooks.add()
*
*lo.activeworkbook.styles("Normal").font.name = "courrier new"
lo.activeworkbook.styles("Normal").font.name = "Calibri"
* 
lo.activeworkbook.styles("Normal").font.size = 7
lo.cells(7, 1).value = "Position Report run on " + DTOC(DATE()) + "  " + TIME()
lo.cells(8, 2).value = "Open wip Range: " + DTOC(pswdate) + " to " + DTOC(pewdate)
lo.cells(9, 2).value = "Transit Range: " + DTOC(pstdate) + " to " + DTOC(petdate)
lo.cells(10, 2).value = "Open Order Range: " + DTOC(psodate) + " to " + DTOC(peodate)
*!*	IF lmainform.optiongroup2.value = 1
*!*	     lo.cells(11, 2).value = "From Style: " + IIF(EMPTY(lmainform.text7.value), "Begining", lmainform.text7.value) + " To " +  ;
*!*	             IIF(EMPTY(lmainform.text8.value), "End", lmainform.text8.value) + "  WITH EXCLUSIONS"
*!*	ELSE
 lo.cells(11, 2).value = "Styles selected individualy from list"
*!*	ENDIF
lseasstring = ''
IF llSeason 
FOR i = 1 TO ALEN(laseasons,1)
  lseasstring = lseasstring + " " + laSeasons(i)
ENDFOR
ENDIF
lo.cells(12, 2).value = IIF(lcRpRepF='B', "Bodies Only", IIF(lcRpRepF='S', "Styles Only",  ;
        "Bodies and Styles"))
*C201864,1 MMT 09/06/2016 Change Type Filter to INLIST in Custom Production Position[P20160628.0001][Start]        
*!*	lo.cells(13, 2).value = "Seasons: " + IIF(EMPTY(ALLTRIM(lseasstring)),'Any Season',lseasstring) + "      Type: " + IIF(lcRpType = "A", "Hats and Bags",  ;
*!*	        IIF(lcRpType = 'H','Hats','Bags'))
lo.cells(13, 2).value = "Seasons: " + IIF(EMPTY(ALLTRIM(lseasstring)),'Any Season',lseasstring) + "      Type: " + IIF(EMPTY(lcGroupCursor) OR !USED(lcGroupCursor) OR RECCOUNT(lcGroupCursor)= 0 , "Hats and Bags",  ;
        lcGroupString)
*C201864,1 MMT 09/06/2016 Change Type Filter to INLIST in Custom Production Position[P20160628.0001][End]
lo.cells(14, 2).value = IIF(llRpDec, "Only Styles with Transit Quantities", "")
lo.cells(15, 2).value = "Cutoff date: " + DTOC(pcutoff) + "  Minimum LIC " + STR(plicmin) + "    Minimum On Hand: " + STR(pohmin)
lo.range(getrange(7, 1, 15, 2)).select
lo.selection.font.size = 7
lo.selection.font.bold = .T.
lo.cells(16, 2).value = 'CODES '
lo.cells(17, 3).value = 'W = Wip'
lo.cells(18, 3).value = 'O = Orders'
lo.cells(19, 3).value = 'SO = SAKS Orders'
lo.cells(20, 3).value = 'X = Non Transit Transaction code (Damaged, etc)'
lo.cells(21, 2).value = 'TRANSIT CODES'
lo.cells(22, 3).value = 'TA = TRANSIT BY AIR'
lo.cells(23, 3).value = 'TB = TRANSIT BY BOAT'
lo.cells(24, 3).value = 'TF = TRANSIT BY FORCE'
lo.cells(25, 3).value = 'TU = TRANSIT CODE NOT FOUND OR UNDEFINED'
lo.cells(26, 2).value = 'FORMULAS'
lo.cells(27, 3).value =  ;
        'OTS/NN = (LIC Quantity + Total In transit for the style) – Open orders inccured during the date range provided'
lo.cells(28, 3).value = 'current Orders  = total open orders for the date range provided '
lo.cells(29, 2).value = "NOTE: In columns E, G, I and J, for Bag Bodies, Total Stock is used instead of LIC Quantity"
lo.range(getrange(16, 1, 29, 3)).select
lo.selection.font.size = 7
lo.selection.font.bold = .T.
lo.activewindow.selectedsheets.hpagebreaks.add(lo.rows(31))
WITH lo.activesheet.pagesetup
     .printtitlerows = "$1:$1"
     .printtitlecolumns = ""
     .leftheader = ""
     .centerheader = ""
     .rightheader = ""
     .leftfooter = ""
     .CenterFooter = "Page &P of &N"
     .rightfooter = ""
     .leftmargin = lo.inchestopoints(0.75 )
     .rightmargin = lo.inchestopoints(0.75 )
     .topmargin = lo.inchestopoints(0.3 )
     .bottommargin = lo.inchestopoints(0.4 )
     .headermargin = lo.inchestopoints(0.5 )
     .footermargin = lo.inchestopoints(0.0 )
     .printheadings = .F.
     .printgridlines = .T.
     .centerhorizontally = .F.
     .centervertically = .F.
     .orientation = 2
     .draft = .F.
     IF lcRpFormat = 'L'
          .papersize = 5
     ELSE
          .papersize = 1
     ENDIF
     .firstpagenumber = -4105
     .zoom = .F.
     .fittopageswide = 1
     .fittopagestall = .F.
ENDWITH
WITH lo.activeworkbook.styles("Normal").font
     *mt
*     .name = "candara"
	  .name = "Calibri"
     *mt
     .size = 10
     .bold = .F.
     .italic = .F.
     .underline = -4142
     .strikethrough = .F.
     .colorindex = -4105
ENDWITH
lo.columns(1).columnwidth = 6.5 
lo.columns(2).columnwidth = 12.5 
lo.columns(3).columnwidth = 6.6 
lo.columns(4).columnwidth = 6.6 
lo.columns(5).columnwidth = 7
lo.columns(7).columnwidth = 7
lo.columns(8).columnwidth = 7
lo.columns(9).columnwidth = 7
lo.columns(10).columnwidth = 8
lo.columns(11).columnwidth = 7
FOR i = 12 TO 45
     lo.columns(i).columnwidth = 6.2 
ENDFOR
lo.columns(12).columnwidth = 7.2 
CREATE CURSOR currep (date D (8), label C (20), wip N (10), ta N (10, 0), tb N (10, 0), tf N (10, 0), tu N (10, 0), other N (10, 0), orders  ;
       N (8, 0), saks N (10, 0), comment M (4))
SELECT currep
INDEX ON date TAG date
lrow = 32
= fillrow(lo, 1, 1, "Style", "Color", "WIP", "WIP - trans", "LIC", "Trans", "WIP + LIC", "Seas")
lo.rows(1).font.bold = .T.
lo.rows(1).wraptext = .T.
lo.cells(1, 9) = "LIC + TRS"
*mt
*lo.cells(1, 10) = "OTS/NN"
lo.cells(1, 10) = "NN"
*mt
lo.cells(1, 11) = "Tot Orders"
lo.cells(1, 12) = "Current Orders"
*MT
lo.cells(1, 13) = "OTS"
*MT
lo.rows(1).font.bold = .T.
lo.rows(1).wraptext = .T.
loldstyle = "xyzzy"
llastcol = 0
llastsaks = 0
IF llRpShow  
     SELECT style, SUM(totqty) AS tordqty FROM curorders WHERE BETWEEN(start, psodate, peodate) GROUP BY 1 INTO CURSOR BSONE
     INDEX ON style TAG bsone
     SELECT curstyle
     SET RELATION TO style INTO bsone
     REPLACE ordcur WITH bsone.tordqty ALL
     DELETE FOR  .NOT. licqty + totintrn - ordcur < 0
     REPLACE ordcur WITH 0 ALL
     USE IN bsone
ENDIF
lrow = lrow + 1
SELECT curstyle
GOTO TOP
IF EOF()
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN 
ENDIF 
DO WHILE  .NOT. EOF()
     lhit = .F.
     IF  llSeason  AND .NOT. EMPTY(lselseas) AND !SEEK(curstyle.season,lcSeason)
*!*	          FOR i = 1 TO ALEN(laSeasons,1)
*!*	             IF laSeasons(i) = curstyle.season
*!*	               lhit = .T.
*!*	               EXIT
*!*	             ENDIF
*!*	          ENDFOR
*!*	          IF  .NOT. lhit
               SELECT curstyle
               SKIP
               LOOP
*!*	          ENDIF
     ENDIF
     llicqty = curstyle.licqty
     SELECT currep
     ZAP
     IF LEFT(curstyle.style, 7) <> loldstyle
          IF LEFT(loldstyle, 1) <> LEFT(curstyle.style, 1)
               IF loldstyle <> 'xyzzy'
                    lo.activewindow.selectedsheets.hpagebreaks.add(lo.rows(lrow))
               ENDIF
          ELSE
               IF loldstyle <> 'xyzzy'
                    lrow = lrow + 2
               ENDIF
          ENDIF
          loldstyle = LEFT(curstyle.style, 7)
          lo.cells(lrow, 1).value = LEFT(curstyle.style, 7)
          lo.cells(lrow, 2).value = curstyle.desc
          loldstyle = LEFT(curstyle.style, 7)
     ENDIF
     lrow = lrow + 1
     lo.rows(lrow).font.bold = .T.
     SELECT codes
     IF SEEK("N" + SUBSTR(curstyle.style, 14, 6) + "N" + "COLOR")
          lcolor = ALLTRIM(codes.cdiscrep)
     ELSE
          lcolor = SUBSTR(curstyle.style, 14, 6)
     ENDIF
     lo.cells(lrow, 2).value = lcolor
     lo.cells(lrow, 3).value = curstyle.totwip
     lo.cells(lrow, 4).value = curstyle.totwip - curstyle.totintrn
     lo.cells(lrow, 5).value = llicqty
     lo.cells(lrow, 6).value = curstyle.totintrn
     lo.cells(lrow, 7).value = curstyle.totwip + llicqty
     lo.cells(lrow, 8).value = curstyle.season
     lo.cells(lrow, 9).value = llicqty + curstyle.totintrn
     *MT
     *lo.cells(lrow, 13).value = curstyle.Totstk+curstyle.totwip -curstyle.TotOrd
     *B611049,1 MMT 08/30/2015 Issue3 and 4:Incorrect calculations in Custom Production position report[T20150406.0014][Start]
     *lo.cells(lrow, 13).value = llicqty+curstyle.totwip+ curstyle.totintrn -curstyle.TotOrd
     lo.cells(lrow, 13).value = llicqty+curstyle.totwip  -curstyle.TotOrd
     *B611049,1 MMT 08/30/2015 Issue3 and 4:Incorrect calculations in Custom Production position report[T20150406.0014][End]
     *MT
     lneedcol = 0
     ltrneedcol = 0
     ltotordcol = 0
     SELECT currep
     APPEND BLANK
     REPLACE date WITH lprior, label WITH "prior"
     APPEND BLANK
     REPLACE date WITH lfuture, label WITH "Future"
     SELECT curpowip
     SET ORDER TO style
     SET KEY TO curstyle.style
     GOTO TOP
     DO WHILE  .NOT. EOF()
          SELECT curwiptrn
          SET ORDER TO postyle
          SET KEY TO curpowip.po + curpowip.style
          GOTO TOP
          DO WHILE  .NOT. EOF()
               IF curwiptrn.trancd = '2'
                    SKIP
                    LOOP
               ENDIF
               IF curwiptrn.trancd = '3'
                    REPLACE curpowip.totqty WITH curpowip.totqty - curwiptrn.totqty
                    SELECT shpmthdr
                    *B611049,1 MMT 08/30/2015 Issue3 and 4:Incorrect calculations in Custom Production position report[T20150406.0014][Start]
*!*	                    IF  .NOT. EMPTY(curwiptrn.shipno) .AND. gfSEEK(curwiptrn.shipno) .AND.  .NOT. EMPTY(shpmthdr.eta)
*!*	                         lthedate = TTOD(shpmthdr.eta)
                    IF  .NOT. EMPTY(curwiptrn.shipno) .AND. gfSEEK("PP"+curwiptrn.shipno) .AND.  .NOT. EMPTY(shpmthdr.eta)
                         lthedate = shpmthdr.eta
                    *B611049,1 MMT 08/30/2015 Issue3 and 4:Incorrect calculations in Custom Production position report[T20150406.0014][End]     
                         lshipcode = shpmthdr.shipvia
                         SELECT shipvia
                         IF SEEK(shpmthdr.shipvia)
                              lshipvia = shipvia.type
                         ELSE
                              lshipvia = "U"
                         ENDIF
                         lnoeta = .F.
                    ELSE
                         lthedate = TTOD(curpowip.available)
                         lshipvia = "U"
                         lshipcode = 'N/A'
                         lnoeta = .T.
                    ENDIF
                    DO CASE
                         CASE lthedate < pstdate
                              SELECT currep
                              = SEEK(lprior)
                         CASE lthedate > petdate
                              SELECT currep
                              = SEEK(lfuture)
                         OTHERWISE
                              SELECT currep
                              IF  .NOT. SEEK(lthedate)
                                   APPEND BLANK
                                   REPLACE date WITH lthedate, label WITH LEFT(DTOC(lthedate), 5)
                              ENDIF
                    ENDCASE
                    DO CASE
                         CASE lshipvia = 'A'
                              REPLACE currep.ta WITH currep.ta + curwiptrn.totqty
                         CASE lshipvia = 'B'
                              REPLACE currep.tb WITH currep.tb + curwiptrn.totqty
                         CASE lshipvia = 'F'
                              REPLACE currep.tf WITH currep.tf + curwiptrn.totqty
                         OTHERWISE
                              REPLACE currep.tu WITH currep.tu + curwiptrn.totqty
                    ENDCASE
                    REPLACE currep.comment WITH ALLTRIM(currep.comment) + "Transit PO " + curwiptrn.po + IIF(lnoeta,  ;
                            " Available DATE(ETA Not found): ", " ETA: ") + DTOC(lthedate) + " SHIPVIA: " + lshipcode + CHR(10)
                    SELECT curwiptrn
                    SKIP
                    LOOP
               ENDIF
               DO CASE
                    CASE curpowip.available < pstdate
                         SELECT currep
                         = SEEK(lprior)
                    CASE curpowip.available > petdate
                         SELECT currep
                         = SEEK(lfuture)
                    OTHERWISE
                         SELECT currep
                         IF  .NOT. SEEK(TTOD(curpowip.available))
                              APPEND BLANK
                              REPLACE date WITH lthedate, label WITH LEFT(DTOC(TTOD(curpowip.available)), 5)
                         ENDIF
               ENDCASE
               REPLACE currep.other WITH currep.other + curwiptrn.totqty, currep.comment WITH ALLTRIM(currep.comment) + "other PO# " +  ;
                       curwiptrn.po + " PO Available: " + DTOC(TTOD(curpowip.available)) + CHR(10)
               SELECT curwiptrn
               SKIP
          ENDDO
          SELECT curpowip
          DO CASE
               CASE curpowip.available < pswdate
                    SELECT currep
                    = SEEK(lprior)
               CASE curpowip.available > pewdate
                    SELECT currep
                    = SEEK(lfuture)
               OTHERWISE
                    SELECT currep
                    IF  .NOT. SEEK(TTOD(curpowip.available))
                         APPEND BLANK
                         REPLACE date WITH TTOD(curpowip.available), label WITH LEFT(DTOC(TTOD(curpowip.available)), 5)
                    ENDIF
                    SELECT curstyle
                    REPLACE curstyle.need WITH curstyle.need + curpowip.need
          ENDCASE
          REPLACE currep.wip WITH currep.wip + curpowip.totqty, currep.comment WITH ALLTRIM(currep.comment) + "WIP PO: " +  ;
                  curpowip.po + " Vendor: " + curpowip.vendor + " Available: " + DTOC(TTOD(curpowip.available)) + CHR(10)
          SELECT curpowip
          SKIP
     ENDDO
     SELECT curorders
     SET KEY TO curstyle.style
     GOTO TOP
     DO WHILE  .NOT. EOF()
          SELECT customer
          SEEK "M" + curorders.account 
          SELECT curorders
          lthedate = orddate(curorders.start, psodate, peodate)
          DO CASE
               CASE lthedate < psodate
                    SELECT currep
                    = SEEK(lprior)
               CASE lthedate > peodate
                    SELECT currep
                    = SEEK(lfuture)
                    REPLACE curstyle.ordcurfu WITH curstyle.ordcurfu + curorders.totqty
               OTHERWISE
                    SELECT currep
                    IF  .NOT. SEEK(lthedate)
                         APPEND BLANK
                         REPLACE date WITH lthedate, label WITH LEFT(DTOC(lthedate), 5)
                    ENDIF
                    SELECT curstyle
                    REPLACE curstyle.need WITH curstyle.need - curorders.totqty
                    REPLACE curstyle.ordcur WITH curstyle.ordcur + curorders.totqty
          ENDCASE
          REPLACE currep.orders WITH currep.orders + curorders.totqty, currep.comment WITH ALLTRIM(currep.comment) + "Customer: " +  ;
                  curorders.account + " " + customer.btname + " " + "Order: " + curorders.order + "  Po: " + curorders.custpo +  ;
                  " Start: " + DTOC(curorders.start) + " Complete: " + DTOC(curorders.complete) + " " + "Open Qty: " +  ;
                  ALLTRIM(STR(curorders.totqty)) + CHR(10)
          IF curorders.account = 'SAK01'
               REPLACE curstyle.saks WITH curstyle.saks + curorders.totqty
          ENDIF
          SELECT curorders
          SKIP
     ENDDO
     *MT
     *lcol = 13
     lcol = 14
     *MT
     SELECT currep
     GOTO TOP
     DO WHILE  .NOT. EOF()
          lo.cells(lrow, lcol).value = "'" + currep.label
          lstring = IIF(currep.wip <> 0, "W: " + TRANSFORM(currep.wip, "9999"), "") + CHR(10) + IIF(currep.ta <> 0, "TA:" +  ;
                    TRANSFORM(currep.ta, "9999") + CHR(10), "") + IIF(currep.tb <> 0, "TB:" + TRANSFORM(currep.tb, "9999") + CHR(10),  ;
                    "") + IIF(currep.tf <> 0, "Tf:" + TRANSFORM(currep.tf, "9999") + CHR(10), "") + IIF(currep.tu <> 0, "Tu:" +  ;
                    TRANSFORM(currep.tu, "9999") + CHR(10), "") + IIF(currep.other <> 0, "X: " + TRANSFORM(currep.other, "9999"), "") +  ;
                    CHR(10) + IIF(currep.orders <> 0, "O:" + TRANSFORM(currep.orders, '9999'), "")
          lo.cells(lrow + 1, lcol).value = lstring
          IF  .NOT. EMPTY(currep.comment)
               lo.cells(lrow + 1, lcol).select
               lo.selection.addcomment
               lo.selection.comment.visible = .F.
               lo.selection.comment.text(currep.comment)
               lo.cells(lrow + 1, lcol).comment.shape.scaleheight(2.72 , 0, 2)
               lo.cells(lrow + 1, lcol).comment.shape.scalewidth(5.27 , 0, 0)
               lo.cells(lrow + 1, lcol).comment.shape.scalewidth(1.28 , 0, 2)
          ENDIF
          llastcol = IIF(lcol > llastcol, lcol, llastcol)
          SELECT currep
          lcol = lcol + 1
          SKIP
     ENDDO
     lo.cells(lrow, 10).value = llicqty + curstyle.totintrn - curstyle.ordcur
     lo.cells(lrow, 11).value = curstyle.totord
     lo.cells(lrow, 12).value = curstyle.ordcur
     lo.sheets("sheet2").select
     IF curstyle.saks <> 0
       lo.cells(lrow, 1).value = curstyle.saks
     ENDIF
     SELECT currep
     ZAP
     SELECT curorders
     GOTO TOP
     DO WHILE  .NOT. EOF()
          IF curorders.account <> 'SAK01'
               SKIP
               LOOP
          ENDIF
          IF curorders.start <= peodate
               SKIP
               LOOP
          ENDIF
          lthedate = orddate(curorders.start, peodate, lfuture, .T.)
          SELECT currep
          IF  .NOT. SEEK(lthedate)
               APPEND BLANK
               REPLACE currep.date WITH lthedate, currep.label WITH LEFT(DTOC(lthedate), 5)
          ENDIF
          REPLACE currep.saks WITH currep.saks + curorders.totqty, currep.comment WITH " Start: " + DTOC(curorders.start) +  ;
                  "   Complete: " + DTOC(curorders.complete) + " " + "Open Qty: " + ALLTRIM(STR(curorders.totqty)) + CHR(10)
          SELECT curorders
          SKIP
     ENDDO
     SELECT currep
     GOTO TOP
     lcol = 1
     DO WHILE  .NOT. EOF()
          lcol = lcol + 1
          lo.cells(lrow, lcol).value = "'" + currep.label
          lo.cells(lrow + 1, lcol).value = "SO: " + ALLTRIM(STR(currep.saks))
          IF  .NOT. EMPTY(currep.comment)
               lo.cells(lrow + 1, lcol).select
               lo.selection.addcomment
               lo.selection.comment.visible = .F.
               lo.selection.comment.text(currep.comment)
          ENDIF
          SELECT currep
          SKIP
     ENDDO
     IF lcol > llastsaks
          llastsaks = lcol
     ENDIF
     lo.sheets("sheet1").select
     lrow = lrow + 1
     SELECT curstyle
     SKIP
ENDDO
lo.sheets("sheet2").select

lo.range(getrange(1, 1, lrow + 1, llastsaks)).select
lo.selection.copy
lo.sheets("sheet1").select
lo.cells(1, llastcol + 1).select
lo.activesheet.paste
WAIT CLEAR
lo.columns(llastcol + 1).select
WITH lo.selection.interior
     .colorindex = 6
     .pattern = 1
ENDWITH
lo.columns("E:E").select
WITH lo.selection.interior
     .colorindex = 4
     .pattern = 1
ENDWITH
lo.columns("J:J").select
WITH lo.selection.interior
     .colorindex = 8
     .pattern = 1
ENDWITH
lo.columns("L:L").select
WITH lo.selection.interior
     .colorindex = 3
     .pattern = 1
ENDWITH
lo.range(getrange(1, llastcol + 1, 32, llastcol + 1)).select
lo.selection.interior.colorindex = -4142
lo.cells(1, llastcol + 1).value = 'SAKS'
lo.cells(1, llastcol + 1).font.bold = .T.
labsolutelast = llastcol + 1 + llastsaks - 1



lo.activesheet.pagesetup.printarea = "$A:$" + getcolval(labsolutelast)
lo.cells(1, 1).select
lo.visible = .T.
RETURN
ENDPROC
**
*!*************************************************************
*! Name      : orddate
*! Developer : Mariam Mazhar(MMT)
*! Date      : 04/30/2015
*! Purpose   : order date
*!*************************************************************
FUNCTION orddate
PARAMETER pdate, pbottom, ptop, psaks
LOCAL ldate, ltestdate
ldate = DTOS(pdate)
lday = VAL(RIGHT(ldate, 2))
lmo = VAL(SUBSTR(ldate, 5, 2))
lyear = VAL(LEFT(ldate, 4))
lsub = 0
IF lday > 15
     lmo = lmo + 1
     IF lmo > 12
          lyear = lyear + 1
          lmo = 1
     ENDIF
     lday = 1
     lsub = 1
ELSE
     lday = 15
ENDIF
ltestdate = CTOD(PADL(ALLTRIM(STR(lmo)), 2, '0') + "/" + PADL(ALLTRIM(STR(lday)), 2, '0') + "/" + PADR(ALLTRIM(STR(lyear)), 4, '0')) -  ;
            lsub
IF psaks
     RETURN ltestdate
ENDIF
IF pdate < pbottom
     ltestdate = lprior
ELSE
     IF ltestdate > ptop
          IF pdate <= ptop
               ltestdate = ptop
          ELSE
               ltestdate = lfuture
          ENDIF
     ENDIF
ENDIF
RETURN ltestdate
ENDFUNC
*!*************************************************************
*! Name      : Drawbox
*! Developer : Mariam Mazhar(MMT)
*! Date      : 04/30/2015
*! Purpose   : Drawbox
*!*************************************************************
PROCEDURE Drawbox
PARAMETER psheet, pulr, pulc, plrr, plrc
LOCAL lrow, lcol
lcol = pulc
FOR lrow = pulr TO plrr
     psheet.cells(lrow, lcol).borders(7).linestyle = 1
     psheet.cells(lrow, lcol).borders(7).weight = -4138
ENDFOR
lrow = pulr
FOR lcol = pulc TO plrc
     psheet.cells(lrow, lcol).borders(8).linestyle = 1
     psheet.cells(lrow, lcol).borders(8).weight = -4138
ENDFOR
lcol = plrc
FOR lrow = pulr TO plrr
     psheet.cells(lrow, lcol).borders(10).linestyle = 1
     psheet.cells(lrow, lcol).borders(10).weight = -4138
ENDFOR
lrow = plrr
FOR lcol = pulc TO plrc
     psheet.cells(lrow, lcol).borders(9).linestyle = 1
     psheet.cells(lrow, lcol).borders(9).weight = -4138
ENDFOR
RETURN
ENDPROC
**
PROCEDURE Makevisible
PARAMETER psheet
psheet.visible = -1
RETURN
ENDPROC
**
PROCEDURE Hideit
PARAMETER psheet
psheet.visible = 0
RETURN
ENDPROC
**
PROCEDURE procrange
PARAMETER psheet, pulr, pulc, plrr, plrc, pproperty, pvalue1, pnoeval
LOCAL lrange
lrange = '"' + getcolval(pulc) + ALLTRIM(STR(pulr)) + ':' + getcolval(plrc) + ALLTRIM(STR(plrr)) + '"'
IF  .NOT. pnoeval
     lcommand = "pSheet.range(" + lrange + ")." + pproperty + " = " + STR(EVALUATE(pvalue1))
ELSE
     lcommand = "pSheet.range(" + lrange + ")." + pproperty + " = " + pvalue1
ENDIF
= &lcommand
ENDPROC
**
*!*************************************************************
*! Name      : getcolval
*! Developer : Mariam Mazhar(MMT)
*! Date      : 04/30/2015
*! Purpose   : get col. value
*!*************************************************************
FUNCTION getcolval
PARAMETER pcol
LOCAL ret, ladj
ladj = pcol
lret = ""
DO WHILE ladj<>0
     lret = CHR(MOD(ladj - 1, 26) + 65) + lret
     ladj = INT((ladj - 1) / 26)
ENDDO
RETURN lret
ENDFUNC
**
*!*************************************************************
*! Name      : GetRange
*! Developer : Mariam Mazhar(MMT)
*! Date      : 04/30/2015
*! Purpose   : get range
*!*************************************************************
FUNCTION GetRange
PARAMETER pulr, pulc, plrr, plrc
RETURN getcolval(pulc) + ALLTRIM(STR(pulr)) + ":" + getcolval(plrc) + ALLTRIM(STR(plrr))
ENDFUNC
**
*!*************************************************************
*! Name      : mergecells
*! Developer : Mariam Mazhar(MMT)
*! Date      : 04/30/2015
*! Purpose   : Merge cells
*!*************************************************************
PROCEDURE mergecells
PARAMETER lexcel, pulr, pulc, plrr, plrc
lexcel.range(getrange(pulr, pulc, plrr, plrc)).mergecells = .T.
RETURN
ENDPROC
**
*!*************************************************************
*! Name      : Underrow
*! Developer : Mariam Mazhar(MMT)
*! Date      : 04/30/2015
*! Purpose   : Underrow
*!*************************************************************
FUNCTION Underrow
PARAMETER pe, prow, pncol, pscol, pweight
LOCAL i, lweight
IF TYPE("pWeight") <> "C" .OR. EMPTY(pweight)
     lweight = 2
ELSE
     IF UPPER(LEFT(pweight, 1)) = "T"
          lweight = 2
     ELSE
          IF UPPER(LEFT(pweight, 1)) = "H"
               lweight = 4
          ELSE
               lweight = -4138
          ENDIF
     ENDIF
ENDIF
FOR i = pscol TO pscol + pncol - 1
     pe.cells(prow, i).borders(9).linestyle = 1
     pe.cells(prow, i).borders(9).weight = lweight
ENDFOR
RETURN .T.
ENDFUNC
**
*!*************************************************************
*! Name      : Fillrow
*! Developer : Mariam Mazhar(MMT)
*! Date      : 04/30/2015
*! Purpose   : Fillrow
*!*************************************************************
PROCEDURE Fillrow
PARAMETER pe, prow, pstart, ps1, ps2, ps3, ps4, ps5, ps6, ps7, ps8, ps9, ps10, ps11, ps12
LOCAL i
i = 1
j = pstart
DO WHILE .T.
     IF i > 12
          EXIT
     ENDIF
     lfield = "ps" + ALLTRIM(STR(i))
     IF INLIST(TYPE(lfield), "C", "N", "D")
          IF TYPE(lfield) = "C" .OR. TYPE(lfield) = "D"
               IF  .NOT. EMPTY(EVALUATE(lfield))
                    pe.cells(prow, j).value = EVALUATE(lfield)
               ENDIF
          ELSE
               pe.cells(prow, j).value = EVALUATE(lfield)
          ENDIF
     ELSE
          EXIT
     ENDIF
     j = j + 1
     i = i + 1
ENDDO
RETURN
ENDPROC
**
*!*************************************************************
*! Name      : FillCrow
*! Developer : Mariam Mazhar(MMT)
*! Date      : 04/30/2015
*! Purpose   : FillCrow
*!*************************************************************
PROCEDURE FillCrow
PARAMETER pe, prow, pstart, ps1, ps2, ps3, ps4, ps5, ps6, ps7, ps8, ps9, ps10, ps11, ps12
LOCAL i
i = 1
j = pstart
DO WHILE .T.
     IF i > 12
          EXIT
     ENDIF
     lfield = "ps" + ALLTRIM(STR(i))
     DO CASE
          CASE TYPE(lfield) = "D"
               pe.cells(prow, j).value = "'" + DTOC(EVALUATE(lfield))
          CASE TYPE(lfield) = "N"
               pe.cells(prow, j).value = "'" + STR(EVALUATE(lfield))
          CASE TYPE(lfield) = "C"
               pe.cells(prow, j).value = "'" + EVALUATE(lfield)
          OTHERWISE
               EXIT
     ENDCASE
     j = j + 1
     i = i + 1
ENDDO
RETURN
ENDPROC
**
*!*************************************************************
*! Name      : setwidth
*! Developer : Mariam Mazhar(MMT)
*! Date      : 04/30/2015
*! Purpose   : setwidth
*!*************************************************************
PROCEDURE setwidth
PARAMETER pe, pstart, pn1, pn2, pn3, pn4, pn5, pn6, pn7, pn8, pn9, pn10, pn11, pn12
LOCAL i
i = 1
j = pstart
DO WHILE .T.
     IF i > 12
          EXIT
     ENDIF
     lfield = "pn" + ALLTRIM(STR(i))
     IF TYPE(lfield) = "N"
          IF EVALUATE(lfield) < 0
               LOOP
          ENDIF
          pe.columns(j).columnwidth = EVALUATE(lfield)
     ELSE
          EXIT
     ENDIF
     j = j + 1
     i = i + 1
ENDDO
RETURN
ENDPROC
