*:***************************************************************************
*: Program file  : ICSTLB
*: Program desc. : Print Style Labels
*: System        : Aria Advantage Series ARIA4XP.
*: Module        : INVENTORY CONTROL (IC)
*: Developer     : Walid Hamed (WLD)
*! Date          : 02/19/2007
*: Reference     : N000589 - Convert Style Label  from A27 to A4XP
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : gflfAdjSeg(),gfItemMask(),lfvFabric(),lfvStyle()
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : .... Changing the STYLE.DESC from 18 char. to 11 char. in LABEL
*:***************************************************************************
*: Example : DO ICSTLB
*:***************************************************************************
*N000682,1 MMT 02/11/2013 Globalization changes[Start]
#include r:\aria4xp\reports\ic\icstlb.h
*N000682,1 MMT 02/11/2013 Globalization changes[End]
  *....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*...
  *STY:XXXXXXXXXXXX XXXXXXXXXXXXXXXXXX
  *SIZ:XXX XXX XXX XXX XXX XXX XXX XXX
  *CLR:XXXXXX XXXXXX XXXXXX XXXXXX
  *FAB:XXXXXXXXXXXXXXXXXXXX

IF llOgFltCh
  lcStyLbl   = loOGScroll.gfTempName()
  =gfOpenTable('Style','Style','SH','Style')
  IF ('MA' $ oAriaApplication.CompanyInstalledModules)
    =gfOpenTable('Item','cStyle','SH','Item')
  ENDIF

  CREATE TABLE (oAriaApplication.WorkDir+lcStyLbl) (STYLE C(10),StyDesc c(20),CSIZE C(50),;
    CLR1 C(10),CLR2 C(10),CLR3 C(10),CLR4 C(10),CLR5 C(10),CLR6 C(10),CLR7 C(10),CLR8 C(10),;
    cFab c(20))

*-------------

*Creting filter exp   .. begin
lcseek=" .T. "
lcCurFltr= lfCheckFilter(1, 'STYLE.CSTYMAJOR')
llCurFltr   = !EMPTY(lcCurFltr) AND USED(lcCurFltr) AND RECCOUNT(lcCurFltr) > 0
IF llCurFltr
  SELECT (lcCurFltr)
  INDEX ON CSTYMAJOR TAG (lcCurFltr)
  lcseek=lcseek+" AND SEEK(STYLE.CSTYMAJOR ,'"+lcCurFltr+"')"
ELSE
  IF TYPE("lcCurFltr") = "C" AND USED(lcCurFltr)
    USE IN (lcCurFltr)
  ENDIF
  lcCurFltr= ''
ENDIF


lcCurFltr= lfCheckFilter(1, 'STYLE.FABRIC')
llCurFltr   = !EMPTY(lcCurFltr) AND USED(lcCurFltr) AND RECCOUNT(lcCurFltr) > 0
IF llCurFltr
  SELECT (lcCurFltr)
  INDEX ON CSTYMAJOR TAG (lcCurFltr)
  lcseek=lcseek+" AND SEEK(PADR(STYLE.FABRIC,19),'"+lcCurFltr+"')"
ELSE
  IF TYPE("lcCurFltr") = "C" AND USED(lcCurFltr)
    USE IN (lcCurFltr)
  ENDIF
  lcCurFltr= ''
ENDIF


* Check if there is a filter on Style CDIVISION
lcCurName = lfCheckFilter(1, 'STYLE.CDIVISION')
lcDiv   = loOgScroll.gfTempName()
llDiv   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcDiv ,"CDivision")
IF llDiv
  SELECT (lcDiv)
  INDEX on CDivision TAG (lcDiv)
  lcseek=lcseek+" AND SEEK(STYLE.CDIVISION,'"+lcDiv+"')"
ENDIF

* Check if there is a filter on Style SEASON
lcCurName = lfCheckFilter(1, 'STYLE.SEASON')
lcSea  = loOgScroll.gfTempName()
llSea   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcSea  ,"SEASON")
IF llSea
  SELECT (lcSea  )
  INDEX on SEASON TAG (lcSea  )
    lcseek=lcseek+" AND SEEK(STYLE.SEASON,'"+lcSea+"')"
ENDIF

* Check if there is a filter on Style Group
lcCurName = lfCheckFilter(1, 'STYLE.CSTYGROUP')
lcGrp  = loOgScroll.gfTempName()
llGrp   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcGrp  ,"Group")
IF llGrp
  SELECT (lcGrp  )
  INDEX on Group TAG (lcGrp  )
  lcseek=lcseek+" AND SEEK(STYLE.CSTYGROUP,'"+lcGrp+"')"
ENDIF


* Check if there is a filter on Style COLOR
lcCurName = lfCheckFilter(1, 'SUBSTR(STYLE.STYLE,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3]))')

lcCol  = loOgScroll.gfTempName()
llCol   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcCol  ,"Color")
IF llCol
  SELECT (lcCol  )
  INDEX on Color TAG (lcCol  )
  lcseek=lcseek+" AND SEEK(SUBSTR(STYLE.STYLE,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3])),'"+lcCol  +"')"
ENDIF

* PATTERN Filter
lcPattern= lfCheckFilter(1, 'STYLE.PATTERN')
llPattern=IIF(EMPTY(lcPattern),.f.,.t.)
IF llPattern
  lcSeek=lcSeek +" AND STYLE.PATTERN='"+lcPattern+"'"
ENDIF
lcRpStatus = lfCheckFilter(1, 'STYLE.STATUS')

lcSeek= lcSeek+ IIF(lcRpStatus<>' ',' .AND. Style.Status $ lcRpStatus','')

  SELECT STYLE
  LOCATE FOR &lcSeek
  IF !FOUND()
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF

  DO lpPrnLbl
  DIMENSION loOgScroll.laCRTables[1]
  DIMENSION loOgScroll.laCRParams[1,2]


  loOgScroll.laCRParams[1,1] = 'ReportName'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *loOgScroll.laCRParams[1,2]= Lang_STYLE_LABEL
  loOgScroll.laCRParams[1,2]= IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_STYLE_LABEL,oAriaApplication.GetHeaderText("Lang_STYLE_LABEL",AHEADERFILE))
  *N000682,1 11/20/2012 MMT Globlization changes[End]


  loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcStyLbl + ".DBF"
  SELECT (lcStyLbl)
  IF !RECCOUNT()>0
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF

  IF USED(lcStyLbl)
    USE IN (lcStyLbl)
  ENDIF
  =gfDispRe()
ELSE
    =gfDispRe()
ENDIF  &&FILTER CHANGE
*-- end of program code.

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Walid Hamed (WLD)
*! Date      : 02/20/2007
*! Purpose   : Validate Style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     :
*!*************************************************************

FUNCTION lfvStyle
  PRIVATE lcStyFld,lcStyle,lnSelcFile,lcCusTag
  lcStyFld   = VARREAD()
  lcStyle    = EVAL(lcStyFld)

  lnSelcFile = SELECT(0)
  SELECT STYLE
  lcCusTag  = ORDER('style')
  =gfSetOrder('Cstyle')

  IF !EMPTY(lcStyle)
    IF ('?' $ lcStyle .OR. !GSEEK(lcStyle,'Style'))
      lcStyle = gfStyBrw('M',"","",.F.)
    ELSE
      &lcStyFld = lcStyle
    ENDIF
  ENDIF
  &lcStyFld = lcStyle
  =gfSetOrder(lcCusTag)
  SELECT (lnSelcFile)

  *!*************************************************************
  *! Name      : lfAdjSeg
  *! Developer : Walid Hamed (WLD)
  *! Date      : 02/20/2007
  *! Purpose   : Get the style code segments information.
  *!*************************************************************
  *! Called from : option grid of POPRLB.PRG
  *!*************************************************************
  *! Calls       : None
  *!*************************************************************
  *! Passed Parameters : None.
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfAdjSeg()
  *!*************************************************************

FUNCTION lfAdjSeg

  STORE 0 TO lnFPos , lnDPos , lnZPos   , lnGPos , lnCPos , lnOPos , lnTPos , ;
    lnQPos , lnSPos , lnMajPos
  STORE 0 TO lnMajLen
  *-- laMajSeg array holds the style code segments data
  *-- laMajSeg[x,1] Holds segment type
  *-- laMajSeg[x,2] Holds segment title
  *-- laMajSeg[x,3] Holds segment picture
  *-- laMajSeg[x,4] Holds segment Starting position
  *-- laMajSeg[x,5] Holds segment description
  *-- laMajSeg[x,6] Holds segment separator
  *-- laMajSeg[x,7] Holds (.T./.F.) segment end major.


  DIMENSION laMajSeg[1,1]
  = gfItemMask(@laMajSeg)
  lnMajSeg  = gfItemMask('SM')  && No. of major segments.
  *--Get Major Length

  FOR lnC = 1 TO ALEN(laMajSeg,1)
    *-- If the style major consists of one segment, don't display it,
    *-- display the style major instead (style major will browse from the
    *-- style file directly)
    IF lnC = 1 .AND. lnMajSeg = 1
      LOOP
    ENDIF
    *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
*!*	    DO CASE
*!*	      CASE laMajSeg[lnC,1] = 'F'
*!*	        *-- If there are more than one "FREE" segment , get first one only
*!*	        lnFPos = IIF(lnFPos = 0, lnC , lnFPos)
*!*	        IF EMPTY(laMajSeg[lnC,5])
*!*	          laMajSeg[lnC,5] = 'Style'
*!*	        ENDIF
*!*	      CASE laMajSeg[lnC,1] = 'D'
*!*	        lnDPos = lnC
*!*	        IF EMPTY(laMajSeg[lnC,5])
*!*	          laMajSeg[lnC,5] = 'Division'
*!*	        ENDIF
*!*	      CASE laMajSeg[lnC,1] = 'Z'
*!*	        lnZPos = lnC
*!*	        IF EMPTY(laMajSeg[lnC,5])
*!*	          laMajSeg[lnC,5] = 'Season'
*!*	        ENDIF
*!*	      CASE laMajSeg[lnC,1] = 'G'
*!*	        lnGPos = lnC
*!*	        IF EMPTY(laMajSeg[lnC,5])
*!*	          laMajSeg[lnC,5] = 'Style Group'
*!*	        ENDIF
*!*	      CASE laMajSeg[lnC,1] = 'C'
*!*	        lnCPos   = lnC
*!*	        lnClrPos = laMajSeg[lnC,4]
*!*	        lnClrPic = LEN(laMajSeg[lnC,3])
*!*	        laMajSeg[lnC,5] = 'Only These Colors'

*!*	      CASE laMajSeg[lnC,1] = 'O'
*!*	        *-- If there are more than one "OTHER" segment , get first one only
*!*	        lnOPos = IIF(lnOPos = 0, lnC , lnOPos)
*!*	        IF EMPTY(laMajSeg[lnC,5])
*!*	          laMajSeg[lnC,5] = 'Other'
*!*	        ENDIF
*!*	      CASE laMajSeg[lnC,1] = 'T'
*!*	        lnTPos = lnC
*!*	        IF EMPTY(laMajSeg[lnC,5])
*!*	          laMajSeg[lnC,5] = 'Make'
*!*	        ENDIF
*!*	      CASE laMajSeg[lnC,1] = 'Q'
*!*	        lnQPos = lnC
*!*	        IF EMPTY(laMajSeg[lnC,5])
*!*	          laMajSeg[lnC,5] = 'Quality'
*!*	        ENDIF
*!*	      CASE laMajSeg[lnC,1] = 'S'
*!*	        lnSPos = lnC
*!*	        IF EMPTY(laMajSeg[lnC,5])
*!*	          laMajSeg[lnC,5] = 'Scale'
*!*	        ENDIF
*!*	    ENDCASE
    DO CASE
      CASE laMajSeg[lnC,1] = 'F'
        *-- If there are more than one "FREE" segment , get first one only
        lnFPos = IIF(lnFPos = 0, lnC , lnFPos)
        IF EMPTY(laMajSeg[lnC,5])
          laMajSeg[lnC,5] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_STYLE,oAriaApplication.GetHeaderText("LANG_STYLE",AHEADERFILE))
        ENDIF
      CASE laMajSeg[lnC,1] = 'D'
        lnDPos = lnC
        IF EMPTY(laMajSeg[lnC,5])
          laMajSeg[lnC,5] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DIVISION,oAriaApplication.GetHeaderText("LANG_DIVISION",AHEADERFILE))
        ENDIF
      CASE laMajSeg[lnC,1] = 'Z'
        lnZPos = lnC
        IF EMPTY(laMajSeg[lnC,5])
          laMajSeg[lnC,5] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SEASON,oAriaApplication.GetHeaderText("LANG_SEASON",AHEADERFILE))
        ENDIF
      CASE laMajSeg[lnC,1] = 'G'
        lnGPos = lnC
        IF EMPTY(laMajSeg[lnC,5])
          laMajSeg[lnC,5] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_STYLEGROUP,oAriaApplication.GetHeaderText("LANG_STYLEGROUP",AHEADERFILE))
        ENDIF
      CASE laMajSeg[lnC,1] = 'C'
        lnCPos   = lnC
        lnClrPos = laMajSeg[lnC,4]
        lnClrPic = LEN(laMajSeg[lnC,3])
        laMajSeg[lnC,5] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ONLYCOLOR,oAriaApplication.GetHeaderText("LANG_ONLYCOLOR",AHEADERFILE))

      CASE laMajSeg[lnC,1] = 'O'
        *-- If there are more than one "OTHER" segment , get first one only
        lnOPos = IIF(lnOPos = 0, lnC , lnOPos)
        IF EMPTY(laMajSeg[lnC,5])
          laMajSeg[lnC,5] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_OTHER,oAriaApplication.GetHeaderText("LANG_OTHER",AHEADERFILE))
        ENDIF
      CASE laMajSeg[lnC,1] = 'T'
        lnTPos = lnC
        IF EMPTY(laMajSeg[lnC,5])
          laMajSeg[lnC,5] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MAKE,oAriaApplication.GetHeaderText("LANG_MAKE",AHEADERFILE))
        ENDIF
      CASE laMajSeg[lnC,1] = 'Q'
        lnQPos = lnC
        IF EMPTY(laMajSeg[lnC,5])
          laMajSeg[lnC,5] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_QUALITY,oAriaApplication.GetHeaderText("LANG_QUALITY",AHEADERFILE))
        ENDIF
      CASE laMajSeg[lnC,1] = 'S'
        lnSPos = lnC
        IF EMPTY(laMajSeg[lnC,5])
          laMajSeg[lnC,5] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SCALE,oAriaApplication.GetHeaderText("LANG_SCALE",AHEADERFILE)) 
        ENDIF
    ENDCASE
    *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[End]
  ENDFOR

  *-- Compute Variables titles. [begin]
  *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[Start]
  *lcStyGrp  = lcStyMajor + ' Group'
  lcStyGrp  = lcStyMajor + ' '+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GROUP,oAriaApplication.GetHeaderText("LANG_GROUP",AHEADERFILE))
  *N000682,1 MMT 03/14/2013 Fix issues of Globalization Testing Phase#2[End]
  *-- Compute Variables titles. [end]

  *!*************************************************************
  *! Name      : lpPrnLbl
  *! Developer : Walid Hamed (WLD)
  *! Date      : 02/20/2007
  *! Purpose   : Print label loop
  *!*************************************************************
  *! Called from : Main program
  *!*************************************************************
  *! Calls       : None
  *!*************************************************************
  *! Passed Parameters : None.
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : DO lpPrnLbl
  *!*************************************************************
  *
PROCEDURE lpPrnLbl

  lnMajorLen = LEN(SUBSTR(lcStylePic,4))
  STORE 0 TO lnClrCnt
  XPRESTYLE = SPACE(lnMajorLen)
  SELECT STYLE
  SET FILTER TO &lcSeek
  SCAN
    IF XPRESTYLE <> PADR(STYLE.CSTYMAJOR,lnMajorLen)     &&Previous Style Different
      STORE 1 TO lnClrCnt
      STORE '' TO xScale
      SELECT (lcStyLbl)
      APPEND BLANK
      REPLACE STYLE WITH PADR(STYLE.CSTYMAJOR,lnMajorLen), StyDesc WITH  STYLE.DESC
      =GfSEEK('S'+STYLE.SCALE,'SCALE')
      xScale = ALLTRIM(SCALE.SZ1) + ' ' + ALLTRIM(SCALE.SZ2) + ' ' + ALLTRIM(SCALE.SZ3) + ' ' + ALLTRIM(SCALE.SZ4)+' '+;
        ALLTRIM(SCALE.SZ5) + ' ' + ALLTRIM(SCALE.SZ6) + ' ' + ALLTRIM(SCALE.SZ7) + ' ' + ALLTRIM(SCALE.SZ8)
      IF LEN(xScale)> 30
        xScale = PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,4),3,' ') +  ' ' + PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,4),3,' ') + ' ' +;
          PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,4),3,' ') + ' ' + PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,4),3,' ') + ' ' + ;
          PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,4),3,' ') + ' ' + PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,4),3,' ') + ' ' + ;
          PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,4),3,' ') + ' ' + PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,4),3,' ')
      ENDIF
      REPLACE CSIZE WITH xScale
      lcStyClr = 'CLR'+ALLTRIM(STR(lnclrCnt,2))
      REPLACE &lcStyClr. WITH SUBSTR(STYLE.STYLE,lnClrPos,lnClrPic)  IN (lcStyLbl)
      IF ('MA' $ oAriaApplication.CompanyInstalledModules)
       IF gfSeek('0002'+STYLE.Fabric,'Item') AND !EMPTY(Item.DESC)
          REPLACE CFab WITH ALLTRIM(Item.DESC)
        endif
      ENDIF
    ELSE
      IF EMPTY(&lcStyLbl..CLR8)
        lcStyClr = 'CLR'+ALLTRIM(STR(lnclrCnt,2))
        REPLACE &lcStyClr. WITH SUBSTR(STYLE.STYLE,lnClrPos,lnClrPic)  IN (lcStyLbl)
      ELSE
        lnclrCnt = 1
        SELECT (lcStyLbl)
        APPEND BLANK
        REPLACE STYLE WITH PADR(STYLE.CSTYMAJOR,lnMajorLen), StyDesc WITH  SUBSTR(STYLE.DESC,1,11)
        REPLACE CSIZE WITH xScale
        lcStyClr = 'CLR'+ALLTRIM(STR(lnclrCnt,2))
        REPLACE &lcStyClr. WITH SUBSTR(STYLE.STYLE,lnClrPos,lnClrPic)  IN (lcStyLbl)
      IF ('MA' $ oAriaApplication.CompanyInstalledModules)
         IF gfSeek('0002'+STYLE.Fabric,'Item') AND !EMPTY(Item.DESC)
          REPLACE CFab WITH ALLTRIM(Item.DESC)
        endif
      ENDIF
      ENDIF
    ENDIF
    lnClrCnt = lnClrCnt + 1
    XPRESTYLE = PADR(STYLE.CSTYMAJOR,lnMajorLen)
  ENDSCAN
  *-- end of lpPrnLbl.
  *:****************************************************************
  *: Name        : lfsrvSty
  *: Developer   : Walid Hamed (WLD)
  *: Date        : 02/20/2007
  *: Purpose     : Rise change style flag, in range browse screen.
  *:****************************************************************
  *: Calls       : None.
  *:****************************************************************
  *: Called from : Option Grid
  *:****************************************************************
  *: Passed Parameters  : None
  *:****************************************************************
  *: Returns     : None
  *:****************************************************************
  *: Example     : =lfsrvSty()
  *:****************************************************************
  *: Note        : SRV symbol is [S,Set -- R,Reset -- V,Valid]
  *:****************************************************************
  *
FUNCTION lfSRVSty
  PARAMETERS lcParm

  DO CASE
    CASE lcParm = 'S'  && Set code
      *-- open this file STYLE in another alias STYLE_X to set order to Style Major
      *-- unique index.
      SELECT STYLE
      =gfSetOrder('Cstyle')
      SET RELATION TO STYLE.STYLE INTO STYLE_X
      GO TOP IN STYLE
      llChStyle = .T.
    CASE lcParm = 'R'  && Reset code
      SELECT STYLE
      SET RELATION TO
      =gfSetOrder('STYLE')
      llClearSty = .F.
  ENDCASE

  *-- End of lfsrvSty.
  *:****************************************************************
  *: Name        : lfStySum
  *: Developer   : Walid Hamed (WLD)
  *: Date        : 02/20/2007
  *: Purpose     : sum a specific field for the current style in style file
  *:****************************************************************
  *: Calls       : None.
  *:****************************************************************
  *: Called from : Option Grid,style browse calculated fields.
  *:****************************************************************
  *: Passed Parameters  : None
  *:****************************************************************
  *: Returns     : Calculated field value.
  *:****************************************************************
  *: Example     : =lfStySum()
  *:****************************************************************
  *
FUNCTION lfStySum
  PARAMETERS lcSty,lccomp,lnAddToVar

  PRIVATE lnStyRec
  lnStyRec = IIF(BETWEEN(RECNO('STYLE'),1,RECCOUNT('STYLE')),RECNO('STYLE'),1)
  lnTotcomp = 0
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE STYLE = ALLTRIM(lcSty)
  SELECT STYLE
  GO lnStyRec
  DO CASE
    CASE lnAddToVar = 1
      lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
  RETURN INT(lnTotcomp)

  *-- End of lfStySum.
  *:****************************************************************
  *: Name        : lfwOGWhen
  *: Developer   : Walid Hamed (WLD)
  *: Date        : 02/20/2007
  *: Purpose     : Get Fabric code
  *:****************************************************************
  *: Calls       : None.
  *:****************************************************************
  *: Called from : Option Grid
  *:****************************************************************
  *: Passed Parameters  : None
  *:****************************************************************
  *: Returns     : Fabric Code field value.
  *:****************************************************************
  *: Example     : =lfwOGWhen()
  *:****************************************************************
  *
FUNCTION lfwOGWhen
IF !llFirst
	lcSqlStatment   = "SELECT ItemLoc.STYLE,ItemLoc.TOTWIP,ItemLoc.TOTSTK,ItemLoc.TOTORD,WIP1,DYELOT,ITEM.CSTYMAJOR   FROM ItemLoc(INDEX = STYDYE),ITEM(INDEX = CSTYLE) WHERE ITEMLOC.dyelot ='' AND ITEMLOC.STYLE = ITEM.STYLE AND ITEMLOC.CINVTYPE = ITEM.CINVTYPE "
	lcSqlStatment   = lcSqlStatment   +" AND ITEM.CINVTYPE = 0002"
	lcCursorLoc = loOgScroll.gfTempName()
	lcTable ='ItemLoc'
	lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursorLoc,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
	                                      'BROWSE',SET("DATASESSION"))

	IF lnConnectionHandlar >= 1
	  SELECT(lcCursorLoc)
	  lnBuffering = CURSORGETPROP("Buffering",lcCursorLoc)
	   =CURSORSETPROP("Buffering",3,lcCursorLoc)
	  SELECT (lcCursorLoc)
	  INDEX ON CSTYMAJOR TAG &lcCursorLoc
	  SET ORDER TO TAG &lcCursorLoc
	  SET RELATION TO
	  LOCATE
	ENDIF
	llFirst = .T.
ENDIF

*!*************************************************************
*! Name      : lfFabSum
*! Developer : Walid Hamed (WLD)
*! Date      : 02/19/2007
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
*--
LOCAL lnAlias
lnAlias = SELECT()
*--

lnTotcomp = 0
SELECT(lcCursorLoc)
IF RECCOUNT() != 0
  lnFabRec = RECNO('ITEM')
  SELECT(lcCursorLoc)
  LOCATE
  IF SEEK(lcFab)
    SUM &lcCOMP TO lnTotcomp WHILE cstymajor=lcFab AND DYELOT =''
  ENDIF
  SELECT ITEM
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF

SELECT(lnAlias)

RETURN INT(lnTotcomp)
*-- end of lfFabSum.
*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter
LOCAL lcReturn, lnPOS
DO CASE
  CASE lnArrayType = 1
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  CASE lnArrayType = 3
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  OTHERWISE
    lcReturn = ""
ENDCASE

RETURN lcReturn
*!*************************************************************
*! Name      : lfStr2Curs
*! Developer : Ayman Mahmoud Ahmed(AYM)
*! Date      : 02/11/2005
*! Purpose   : Create cursor from string filters
*!*************************************************************
*! Passed Parameters  : File Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfStr2Curs
PARAMETERS lcString , lccursor , lcFieldsName

CREATE CURSOR (lcCursor) (&lcFieldsName. C(6))
DO WHILE AT('|',lcString)> 0
  lcFieldsValue  = SUBSTR(lcString,1,AT('|',lcString)-1)
  lcString = SUBSTR(lcString,AT('|',lcString)+1)
  SELECT (lcCursor)
  APPEND BLANK
  REPLACE &lcFieldsName. WITH lcFieldsValue
ENDDO
SELECT (lcCursor)
APPEND BLANK
REPLACE &lcFieldsName. WITH lcString
