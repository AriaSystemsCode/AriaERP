*:************************************************************************
*: Program file  : SOSRORD.Prg
*: Program desc. : STYLE REORDER REPORT
*: System        : Aria Advantage Series VER. 4XP
*: Module        : SO
*: Developer     : Mariam Mazhar (MMT)
*: Date          : 25/07/2006
*: Reference     : N037672
*:************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfsrvSty(), lfSRVFab(), lfFabSum(), lfwOGWhen(), 
*:                 lfPrtHdr(), lfPrtCTot(), lfPrtSTot()
*:************************************************************************
*: MOdification:
*!* B609356,1 SMA 07/26/2010 Fix bug of creating empty *.cdx files [T20091027.0093]
*:************************************************************************
lcStTime = TIME()
lnMajSize  = LEN(lcMaj)                   && Length of the major
lcStyTitle = gfItemMask('HI')             && Style title
loOgScroll.cCRorientation = 'L'
IF loOgScroll.llOGFltCh && OG Filters changed
  WAIT WINDOW "Collecting Data......." NOWAIT 
  lfCrtTemp()
  lfCollect()
ELSE
  IF FILE(oAriaApplication.WorkDir +  lcRpTmp + ".DBF") AND !USED(lcRpTmp)
    USE oAriaApplication.WorkDir +  lcRpTmp + ".DBF" IN 0 
  ENDIF 
ENDIF 


DIMENSION loOgScroll.laCRParams[6,2]

loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2] = 'STYLE REORDER REPORT'

loOgScroll.laCRParams[2,1] = 'lcStyTitle'
loOgScroll.laCRParams[2,2] = lcStyTitle

loOgScroll.laCRParams[3,1] = 'lcRPT_TYPE'
loOgScroll.laCRParams[3,2] = lcRPT_TYPE

loOgScroll.laCRParams[4,1] = 'lcRpDeciml'
loOgScroll.laCRParams[4,2] = IIF(lcRpDeciml = "Y",1,0)

loOgScroll.laCRParams[5,1] = 'lnMajSize'
loOgScroll.laCRParams[5,2] = lnMajSize

loOgScroll.laCRParams[6,1] = 'llRpROrd'
loOgScroll.laCRParams[6,2] = IIF(llRpROrd,1,0)

SELECT (lcRpTmp)
IF RECCOUNT(lcRpTmp) = 0
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF
DIMENSION LOogsCROLL.laCRTables[1]
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcRpTmp + ".DBF"
lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcRpTmp))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' NOWAIT
USE IN (lcRpTmp)
gfDispRe()
RETURN 

*!*************************************************************
*! Name      : lfFabSum
*! Developer : MAriam Mazhar[MMT]
*! Date      : 06/11/2006
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
LPARAMETERS lcFab, lcComp
LOCAL lnTotcomp,  lnAlias
lnTotcomp = 0
  IF SEEK(ALLTRIM(lcFab), lcTmpFab)
    SUM &lcTmpFab..&lcCOMP. TO lnTotcomp WHILE &lcTmpFab..Fabric = lcFab
  ENDIF
RETURN lnTotcomp
*!*************************************************************
*! Name      : lfCreateIndecies
*! Developer : Mariam Mazhar [MMT]
*! Date      : 06/11/2006
*! Purpose   : Create Indecies for a cursor
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
FUNCTION lfCreateIndecies
LPARAMETERS lcCursor, lcIndex, lcTages

LOCAL lnOldBuffMode, lcIndex1, lcTages1, lcIndExp

*--If Query Successfully executed, Create Indexes if needed for the result cursor
lnOldBuffMode = CURSORGETPROP("Buffering", lcCursor)
=CURSORSETPROP("Buffering", 3, lcCursor)

lcTages1 = lcTages
lcIndex1 = lcIndex
SELECT (lcCursor)
DO WHILE AT("|", lcIndex1,1) <> 0
  lcIndex  = SUBSTR(lcIndex1, 1, AT("|", lcIndex1, 1) - 1)
  lcIndex1 = STRTRAN(lcIndex1, lcIndex + "|", "", 1, 1)
  lcTages  = SUBSTR(lcTages1, 1, AT("|", lcTages1, 1) - 1)
  lcTages1 = STRTRAN(lcTages1, lcTages + "|", "", 1, 1)
  *B609356,1 SMA 07/26/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
  *INDEX ON &lcIndex. TAG (lcTages) OF (lcCursor)
  INDEX ON &lcIndex. TAG (lcTages)
  *B609356,1 SMA 07/26/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
ENDDO
=CURSORSETPROP("Buffering", IIF(TYPE("lnBuffMode") = 'N', lnBuffMode, lnOldBuffMode), lcCursor)

RETURN .T.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar (MMT)
*! Date      : 07/11/2006
*! Purpose   : When function of OG
*!*************************************************************
*! Called from : OG read cycle
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
*lfwOGWhen() 
FUNCTION lfwOGWhen
IF !llFrstTime
  loDBFStyle    = CreateObject("RemoteTable","Style","Style",'Style',SET("DATASESSION"))&&,"",.T.)
  loDBFOrdline  = CreateObject("RemoteTable","Ordline","Ordline",'Ordline',SET("DATASESSION"))&&,"",.T.)
  loDBFordhdr   = CreateObject("RemoteTable","ordhdr","ordhdr",'ordhdr',SET("DATASESSION"))&&,"",.T.)
  loDBFinvline  = CreateObject("RemoteTable","invline","invline",'invline',SET("DATASESSION"))&&,"",.T.)
  loDBFCust     = CreateObject("RemoteTable","Customer","Customer",'Customer',SET("DATASESSION"))&&,"",.T.)
    
    
  lcSqlStat1 = "SELECT ITEMLOC.Style,ITEMLOC.TOTWIP, ITEMLOC.TOTSTK, ITEMLOC.TOTORD, ITEM.CSTYMAJOR AS FABRIC FROM ITEM INNER JOIN ITEMLOC ON ITEM.STYLE = ITEMLOC.STYLE AND ITEM.CINVTYPE = ITEMLOC.CINVTYPE AND ITEMLOC.DYELOT = '' WHERE ITEM.CINVTYPE = '0002'"
  lnResult1 = loOGScroll.ORDA.SQLRun(lcSqlStat1, lcTmpFab, , oAriaApplication.ActiveCompanyConStr, 3, "BROWSE", SET("Datasession"))
  llFrstTime = .T.
  IF lnResult1 >= 1
    =lfCreateIndecies(lcTmpFab, "Fabric|Style", "lcFabDye|'Style'")
  ENDIF
ENDIF 

*!*************************************************************
*! Name      : lfAdjSeg
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 12/29/98
*! Purpose   : Get the style code segments information.
*!*************************************************************
*! Called from : SOSRORD.PRG
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

STORE 0 TO lnFPos, lnDPos, lnZPos, lnGPos, lnCPos, lnOPos, lnTPos, ;
           lnQPos, lnSPos
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
FOR lnC = 1 TO ALEN(laMajSeg,1)
  *-- If the style major consists of one segment, don't display it, 
  *-- display the style major instead (style major will browse from the 
  *-- style file directly)
  IF lnC = 1 .AND. lnMajSeg = 1 
    LOOP
  ENDIF
  DO CASE
    CASE laMajSeg[lnC,1] = 'F'
      *-- If there are more than one "FREE" segment , get first one only
      lnFPos = IIF(lnFPos = 0, lnC , lnFPos)
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Style'
      ENDIF
    CASE laMajSeg[lnC,1] = 'D'
      lnDPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Division'
      ENDIF
    CASE laMajSeg[lnC,1] = 'Z'
      lnZPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Season'
      ENDIF
    CASE laMajSeg[lnC,1] = 'G'
      lnGPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Style Group'
      ENDIF
    CASE laMajSeg[lnC,1] = 'C'
      lnCPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Color'
      ENDIF
    CASE laMajSeg[lnC,1] = 'O'
      *-- If there are more than one "OTHER" segment , get first one only
      lnOPos = IIF(lnOPos = 0, lnC , lnOPos)
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Other'
      ENDIF
    CASE laMajSeg[lnC,1] = 'T'
      lnTPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Make'
      ENDIF
    CASE laMajSeg[lnC,1] = 'Q'
      lnQPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Quality'
      ENDIF
    CASE laMajSeg[lnC,1] = 'S'
      lnSPos = lnC
       IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Scale'
      ENDIF
  ENDCASE

ENDFOR
*!**************************************************************************
*! Name      : lfvEdiOrd
*! Developer : Mariam Mazhar (MMT)
*! Date      : 25/07/2006
*! Purpose   : to validate (Print Orders/Edi Orders) popup in OG 
*!**************************************************************************
*! Example   : =lfvEdiOrd()
*!**************************************************************************
FUNCTION lfvEdiOrd

lcRpEdiFlt = ""
IF 'EB' $ oAriaApplication.CompanyInstalledModules AND lcRpEdiPrn <> "B"
  lcRpEdiFlt = IIF(lcRpEdiPrn="O",[!OrdHdr.lEdiOrder],[OrdHdr.lEdiOrder])
ENDIF
llClearOrd = .T.

*!**************************************************************************
*! Name      : lfCrtTemp
*! Developer : Mariam Mazhar (MMT)
*! Date      : 25/07/2006
*! Purpose   : to Create temp. file
*!**************************************************************************
FUNCTION lfCrtTemp

DIMENSION laTempStru[18,4]
laTempStru = ''
laTempStru [1,1] = 'Type'
laTempStru [1,2] = 'C'
laTempStru [1,3] = 1
laTempStru [1,4] = 0

laTempStru [2,1] = 'Style'
laTempStru [2,2] = 'C'
laTempStru [2,3] = 19
laTempStru [2,4] = 0

laTempStru [3,1] = 'Account'
laTempStru [3,2] = 'C'
laTempStru [3,3] = 5
laTempStru [3,4] = 0

laTempStru [4,1] = 'Cust_Name'
laTempStru [4,2] = 'C'
laTempStru [4,3] = 30
laTempStru [4,4] = 0

laTempStru [5,1] = 'Order'
laTempStru [5,2] = 'C'
laTempStru [5,3] = 6
laTempStru [5,4] = 0

laTempStru [6,1] = 'Entered'
laTempStru [6,2] = 'D'
laTempStru [6,3] = 8
laTempStru [6,4] = 0

laTempStru [7,1] = 'Qty1'
laTempStru [7,2] = 'N'
laTempStru [7,3] = 6
laTempStru [7,4] = 0

laTempStru [8,1] = 'Qty2'
laTempStru [8,2] = 'N'
laTempStru [8,3] = 6
laTempStru [8,4] = 0

laTempStru [9,1] = 'Qty3'
laTempStru [9,2] = 'N'
laTempStru [9,3] = 6
laTempStru [9,4] = 0

laTempStru [10,1] = 'Qty4'
laTempStru [10,2] = 'N'
laTempStru [10,3] = 6
laTempStru [10,4] = 0

laTempStru [11,1] = 'Qty5'
laTempStru [11,2] = 'N'
laTempStru [11,3] = 6
laTempStru [11,4] = 0

laTempStru [12,1] = 'Qty6'
laTempStru [12,2] = 'N'
laTempStru [12,3] = 6
laTempStru [12,4] = 0

laTempStru [13,1] = 'Qty7'
laTempStru [13,2] = 'N'
laTempStru [13,3] = 6
laTempStru [13,4] = 0

laTempStru [14,1] = 'Qty8'
laTempStru [14,2] = 'N'
laTempStru [14,3] = 6
laTempStru [14,4] = 0

laTempStru [15,1] = 'toQty'
laTempStru [15,2] = 'N'
laTempStru [15,3] = 7
laTempStru [15,4] = 0

laTempStru [16,1] = 'Price'
laTempStru [16,2] = 'N'
laTempStru [16,3] = 7
laTempStru [16,4] = 2

laTempStru [17,1] = 'XDesc'
laTempStru [17,2] = 'C'
laTempStru [17,3] = 30
laTempStru [17,4] = 0

laTempStru [18,1] = 'PriceA'
laTempStru [18,2] = 'N'
laTempStru [18,3] = 7
laTempStru [18,4] = 2

 = gfCrtTmp(lcRpTmp,@laTempStru,"Style" ,lcRpTmp,.F.)
*!**************************************************************************
*! Name      : lfCollect
*! Developer : Mariam Mazhar (MMT)
*! Date      : 25/07/2006
*! Purpose   : to Collect data
*!**************************************************************************
FUNCTION lfCollect

lodbfStyle.SetOrder("Style")
lluse_ext_scale = gfGetMemVar('M_USEEXSSC')

llUseStyle = .F.
llUseDiv   = .F.
llUseSea   = .F.
llUseGrp   = .F.
llUseFab   = .F.
llUseClr   = .F.
llUseScale = .F.

IF lluse_ext_scale 
  lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'STYLE.SCALE'),1)
  IF lnPosition > 0
	  lcScaleFile = LOOGSCROLL.laOGFxFlt[lnPosition,6]
	  llUseScale = IIF(!EMPTY(lcScaleFile) .AND. USED(lcScaleFile) .AND. RECCOUNT(lcScaleFile)>0,.T.,.F.)
	ENDIF
	IF llUseScale  
	  SELECT(lcScaleFile)
	  LOCATE 
	  IF EOF()
	    llUseScale  = .F.
	  ENDIF 
	ENDIF 
ENDIF 

*--Style major
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(loOGScroll.laOGFxFlt,'STYLE.CSTYMAJOR'),1)
IF lnPosition > 0
  lcStylFile = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  llUseStyle = IIF(!EMPTY(lcStylFile) .AND. USED(lcStylFile) .AND. RECCOUNT(lcStylFile)>0,.T.,.F.)
ENDIF
IF llUseStyle 
  SELECT(lcStylFile)
  LOCATE 
  IF EOF()
    llUseStyle = .F.
  ENDIF 
ENDIF 

*---Division Filter
lnDivPos = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(loOGScroll.laOGFXFlt,'STYLE.CDIVISION'),1)
IF lnDivPos  > 0
  lcDivStr = LOOGSCROLL.laOGFXFlt[lnDivPos,6]
  lcDivFile = loOGScroll.gfTempName()
  llUseDiv = IIF(LEN(lcDivStr)>0,.T.,.F.) AND lfConvertToCursor(lcDivStr,'CDIVISION',lcDivFile)
ENDIF

*---Season Filter
lnSeaPos = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(loOGScroll.laOGFXFlt,'STYLE.SEASON'),1)
IF lnSeaPos > 0
  lcSeaStr = LOOGSCROLL.laOGFXFlt[lnSeaPos,6]
  lcSeaFile = loOGScroll.gfTempName()
  llUseSea = IIF(LEN(lcSeaStr)>0,.T.,.F.) AND lfConvertToCursor(lcSeaStr,'SEASON',lcSeaFile)
ENDIF

*--Style Group
lnGrpPos = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(loOGScroll.laOGFXFlt,'STYLE.CSTYGROUP'),1)
IF lnGrpPos > 0
  lcGrpStr = LOOGSCROLL.laOGFXFlt[lnGrpPos,6]
  lcGrpFile = loOGScroll.gfTempName()
  llUseGrp = IIF(LEN(lcGrpStr)>0,.T.,.F.) AND lfConvertToCursor(lcGrpStr,'CSTYGRP',lcGrpFile)
ENDIF

*--Style Fabric
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(loOGScroll.laOGFXFlt,'STYLE.FABRIC'),1)
IF lnPosition > 0
  lcFabFile = LOOGSCROLL.laOGFXFlt[lnPosition,6]
  llUseFab = IIF(!EMPTY(lcFabFile) .AND. USED(lcFabFile) .AND. RECCOUNT(lcFabFile)>0,.T.,.F.)
ENDIF
IF llUseFab 
  SELECT(lcFabFile)
  LOCATE 
  IF EOF()
    llUseFab = .F.
  ENDIF 
ENDIF 


*--Style Color
lnClrPos = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(loOGScroll.laOGVRFlt,'SUBSTR(STYLE.STYLE,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3]))'),1)
IF lnClrPos > 0
  lcClrStr = LOOGSCROLL.laOGVRFlt[lnClrPos,6]
  lcClrFile = loOGScroll.gfTempName()
  llUseClr = IIF(LEN(lcClrStr)>0,.T.,.F.) AND lfConvertToCursor(lcClrStr,'CSTYCLR',lcClrFile)
ENDIF

*--Pattern
lcPattStr = ""
lnPattPos = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(loOGScroll.laOGVRFlt,'STYLE.PATTERN'),1)
IF lnPattPos > 0
  lcPattStr = LOOGSCROLL.laOGVRFlt[lnPattPos,6]
ENDIF

lcStatStr  = ""
lnStPos = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(loOGScroll.laOGVRFlt,'STYLE.STATUS'),1)
IF lnStPos > 0
  IF !EMPTY(LOOGSCROLL.laOGVRFlt[lnStPos ,6])
    lcStatStr = "INLIST(Style.Status,'"+STRTRAN(LOOGSCROLL.laOGVRFlt[lnStPos ,6],"|","','")+"')"
  ENDIF   
ENDIF
IF llUseStyle
  SELECT(lcStylFile)
  SCAN 
    =loDBFStyle.Seek(&lcStylFile..CSTYMAJOR,'CSTYLE')
    SELECT STYLE
    SCAN REST WHILE CSTYMAJOR = &lcStylFile..CSTYMAJOR FOR IIF(llUseClr,SEEK(SUBSTR(STYLE.STYLE,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3])),lcClrFile),.T.);
                    AND IIF(llUseDiv,SEEK(STYLE.CDIVISION,lcDivFile),.T.);
                    AND IIF(llUseSea,SEEK(STYLE.SEASON,lcSeaFile),.T.) ;
                    AND IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.);
                    AND IIF(llUseFab,SEEK(STYLE.FABRIC,lcFabFile),.T.) ;
                    AND IIF(!EMPTY(lcPattStr),STYLE.PATTERN = lcPattStr,.T.);
                    AND IIF(!EMPTY(lcStatStr),eval(lcStatStr),.T.) AND IIF(llUseScale AND lluse_ext_scale ,SEEK(Style.Scale,lcScaleFile),.T.)
      IF lodbfORDLINE.SEEK(STYLE.STYLE,'ORDLINES')
        SELECT ORDLINE
        SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = STYLE.STYLE ;
                  FOR lodbfORDHDR.Seek(ORDLINE.cOrdType+ORDLINE.Order) AND (ORDHDR.STATUS # 'X' .AND. ORDHDR.CREORDER = 'Y') ;
                  AND IIF(!EMPTY(lcRpEdiFlt),EVALUATE(lcRpEdiFlt),.T.)
          SCATTER MEMO MEMVAR  
          m.Xdesc = Style.Desc 
          m.priceA = Style.priceA
          m.Type = 'O'
          m.toQty = ORDLINE.totqty
          m.Cust_Name = IIF(loDBFCust.SEEK(IIF(EMPTY(ORDHDR.STORE),'M'+ ORDHDR.ACCOUNT,'S'+ORDHDR.ACCOUNT+ORDHDR.STORE),'CUSTOMER'),SUBSTR(CUSTOMER.STNAME,1,7),"")
  	      m.ENTERED  = ORDHDR->ENTERED
      	  INSERT INTO (lcRpTmp) FROM MEMVAR 
        ENDSCAN                     
      ENDIF 
      IF lodbfINVLINE.seek(STYLE.STYLE,"INVLINES")
        SELECT INVLINE
		    SCAN REST WHILE STYLE+INVOICE+STR(LINENO,6) =  STYLE.STYLE  FOR ;
                        lodbfORDHDR.Seek("O"+INVLINE.Order) AND (ORDHDR.CREORDER = 'Y') ;
                        AND  IIF(!EMPTY(lcRpEdiFlt),EVALUATE(lcRpEdiFlt),.T.)
                        *ORDHDR.STATUS # 'X' .AND. 
           SCATTER MEMO MEMVAR   
           m.Xdesc = Style.Desc
           m.priceA = Style.priceA           
           m.Type = 'I'
           m.toQty = INVLINE.totqty
           m.Cust_Name = IIF(loDBFCust.SEEK(IIF(EMPTY(INVLINE.STORE),'M'+ INVLINE.ACCOUNT,'S'+INVLINE.ACCOUNT+INVLINE.STORE),'CUSTOMER'),SUBSTR(CUSTOMER.STNAME,1,7),"")
           m.ENTERED  = INVDATE
           INSERT INTO (lcRpTmp) FROM MEMVAR 
         ENDSCAN 
      ENDIF 
      IF !SEEK(Style.Style,lcRpTmp) AND !llRpROrd
        SELECT(lcRpTmp)
		APPEND BLANK       
        REPLACE Style  WITH  Style.Style
        REPLACE Xdesc  WITH  Style.Desc
        REPLACE price  WITH  Style.priceA
        REPLACE Entered WITH {}
      ENDIF 

    ENDSCAN  
  ENDSCAN 
ELSE 

    SELECT STYLE
    lodbfStyle.SetOrder("Style")
    LOCATE 
    SCAN FOR IIF(llUseClr,SEEK(SUBSTR(STYLE.STYLE,laMajSeg[lnCPos,4],LEN(laMajSeg[lnCPos,3])),lcClrFile),.T.);
                    AND IIF(llUseDiv,SEEK(STYLE.CDIVISION,lcDivFile),.T.);
                    AND IIF(llUseSea,SEEK(STYLE.SEASON,lcSeaFile),.T.) ;
                    AND IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.);
                    AND IIF(llUseFab,SEEK(STYLE.FABRIC,lcFabFile),.T.) ;
                    AND IIF(!EMPTY(lcPattStr),STYLE.PATTERN = lcPattStr,.T.);
                    AND IIF(!EMPTY(lcStatStr),eval(lcStatStr),.T.)  AND IIF(llUseScale AND lluse_ext_scale ,SEEK(Style.Scale,lcScaleFile),.T.)
      IF lodbfORDLINE.SEEK(STYLE.STYLE,'ORDLINES')
        SELECT ORDLINE
        SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = STYLE.STYLE ;
                  FOR lodbfORDHDR.Seek(ORDLINE.cOrdType+ORDLINE.Order) AND (ORDHDR.STATUS # 'X' .AND. ORDHDR.CREORDER = 'Y') ;
                  and IIF(!EMPTY(lcRpEdiFlt),EVALUATE(lcRpEdiFlt),.T.)
          SCATTER MEMO MEMVAR  
          m.Xdesc 	  = Style.Desc 
          m.priceA 	  = Style.priceA
          m.Type 	  = 'O'
          m.toQty 	  = ORDLINE.totqty
          m.Cust_Name = IIF(loDBFCust.SEEK(IIF(EMPTY(ORDHDR.STORE),'M'+ ORDHDR.ACCOUNT,'S'+ORDHDR.ACCOUNT+ORDHDR.STORE),'CUSTOMER'),SUBSTR(CUSTOMER.STNAME,1,7),"")
          m.ENTERED   = ORDHDR->ENTERED
          INSERT INTO (lcRpTmp) FROM MEMVAR 
        ENDSCAN
      ENDIF 
      IF lodbfINVLINE.seek(STYLE.STYLE,"INVLINES")
        SELECT INVLINE
        SCAN REST WHILE STYLE+INVOICE+STR(LINENO,6) =  STYLE.STYLE  FOR ;
                        lodbfORDHDR.Seek("O"+INVLINE.Order) AND (ORDHDR.CREORDER = 'Y');
                        AND  IIF(!EMPTY(lcRpEdiFlt),EVALUATE(lcRpEdiFlt),.T.)
                        *ORDHDR.STATUS # 'X' .AND. 
           SCATTER MEMO MEMVAR   
           m.Xdesc = Style.Desc
           m.priceA = Style.priceA           
           m.Type = 'I'
           m.toQty = INVLINE.totqty
           m.Cust_Name = IIF(loDBFCust.SEEK(IIF(EMPTY(INVLINE.STORE),'M'+ INVLINE.ACCOUNT,'S'+INVLINE.ACCOUNT+INVLINE.STORE),'CUSTOMER'),SUBSTR(CUSTOMER.STNAME,1,7),"")
           m.ENTERED  = INVDATE
           INSERT INTO (lcRpTmp) FROM MEMVAR 
         ENDSCAN 
      ENDIF 
      IF !SEEK(Style.Style,lcRpTmp) AND !llRpROrd
        SELECT(lcRpTmp)
		APPEND BLANK       
        REPLACE Style  WITH  Style.Style
        REPLACE Xdesc  WITH  Style.Desc
        REPLACE price  WITH  Style.priceA
        REPLACE Entered WITH {}
      ENDIF 
    ENDSCAN  
ENDIF 
lodbfStyle.SetOrder("CStyle")
*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 06/22/2006
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName 

DO CASE 
  
CASE   ALLTRIM(lcFieldName) = 'SEASON'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CDIVISION'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0
  
CASE   ALLTRIM(lcFieldName) = 'CSTYGRP'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CSTYCLR'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

ENDCASE 
 = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1 
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"") 
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO 
  IF lnEnd = 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH lcValuesToConvert 
  ENDIF 
ENDIF 
RETURN .T.
*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mariam Mazhar (MMT)
*! Date      : 07/26/2006
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
