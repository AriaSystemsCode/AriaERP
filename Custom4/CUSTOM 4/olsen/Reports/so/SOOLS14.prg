*:***************************************************************************
*: Program file  : SOOLS14
*: Program desc. : Wholesale Sales report for OLS10
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Mariam Mazhar(MMT)
*! Date          : 05/07/2014
*! Entry No.     : C201615[T20140430.0015]
*!***************************************************************************
*: Modifications:
*!***************************************************************************
LDATE = {}
HDATE = {}
lnDatePos  = ASUBSCRIPT(loogscroll.laOGFxFlt,ASCAN(loogscroll.laOGFxFlt,'INVHDR.INVDATE'),1)
IF lnDatePos > 0
  LDATE = CTOD(SUBSTR(loogscroll.laOGFxFlt[lnDatePos,6],1,ATC('|',loogscroll.laOGFxFlt[lnDatePos,6])-1))
  HDATE = CTOD(SUBSTR(loogscroll.laOGFxFlt[lnDatePos,6],  ATC('|',loogscroll.laOGFxFlt[lnDatePos,6])+1))
ENDIF

IF loogscroll.llOGFltCh 
  lfCrtTemp()
  lfCollectData()
ENDIF
IF !USED(lcTmpData) AND FILE(oAriaApplication.WorkDir + lcTmpData+ ".DBF")
  USE (oAriaApplication.WorkDir + lcTmpData+ ".DBF") IN 0 
ENDIF

SELECT (lcTmpData)
LOCATE
IF EOF()
  = gfModalGen('TRM00052B40011','ALERT')
  RETURN .F.
ENDIF
USE IN (lcTmpData)
  DIMENSION loOGScroll.laCRParams[3,2]
loOGScroll.laCRParams[2,1] = "Start"
loOGScroll.laCRParams[3,1] = "End"
loOGScroll.laCRParams[2,2] = DTOC(LDATE)
loOGScroll.laCRParams[3,2] = DTOC(HDATE)
loOGScroll.laCRParams[1,1] = 'ReportName'
loOGScroll.laCRParams[1,2] = 'Wholesale Sales Report'
loOGScroll.cCRPaperSize = 'LETTER'

DIMENSION loOgScroll.lacrTABLES[1]  && array For Temp Table & pathes
loOgScroll.lacrTABLES[1]= oAriaApplication.WorkDir+lcTmpData+'.DBF'

= gfDispRe(lcRpName)

*!*************************************************************
*! Name      : lfCrtTemp
*! Developer : Mariam Mazhar(MMT)
*! Date      : 05/07/2014
*! Purpose   : Create Temp.
*!*************************************************************
FUNCTION lfCrtTemp

IF USED(lcTmpData)
  USE IN (lcTmpData)
ENDIF
  
DIMENSION laDataTmp[9,4]
laDataTmp[1,1] = 'REP1'
laDataTmp[1,2] = 'C'
laDataTmp[1,3] = 3
laDataTmp[1,4] = 0

laDataTmp[2,1] = 'SEASON'
laDataTmp[2,2] = 'C'
laDataTmp[2,3] = 6
laDataTmp[2,4] = 0

laDataTmp[3,1] = 'ShipAmt'
laDataTmp[3,2] = 'N'
laDataTmp[3,3] = 14
laDataTmp[3,4] = 2

laDataTmp[4,1] = 'Discount'
laDataTmp[4,2] = 'N'
laDataTmp[4,3] = 13
laDataTmp[4,4] = 2

laDataTmp[5,1] = 'nPstAmt'
laDataTmp[5,2] = 'N'
laDataTmp[5,3] = 13
laDataTmp[5,4] = 2

laDataTmp[6,1] = 'Tax_Amt'
laDataTmp[6,2] = 'N'
laDataTmp[6,3] = 13
laDataTmp[6,4] = 2

laDataTmp[7,1] = 'TotalChg'
laDataTmp[7,2] = 'N'
laDataTmp[7,3] = 14
laDataTmp[7,4] = 2

laDataTmp[8,1] = 'CodFRINSUM'
laDataTmp[8,2] = 'N'
laDataTmp[8,3] = 14
laDataTmp[8,4] = 2

laDataTmp[9,1] = 'ntotcost'
laDataTmp[9,2] = 'N'
laDataTmp[9,3] = 14
laDataTmp[9,4] = 2

=gfCrtTmp(lcTmpData,@laDataTmp,"REP1+SEASON",lcTmpData,.F.)

*!*************************************************************
*! Name      : lfCollectData
*! Developer : Mariam Mazhar(MMT)
*! Date      : 05/07/2014
*! Purpose   : Collect Data
*!*************************************************************
FUNCTION lfCollectData


lcCursorSls = ''
llSelSls = .F.
lnPosSls = ASCAN(loOgScroll.laOgVRFlt,"INVHDR.REP1")
IF lnPosSls > 0
  lnPosSls = ASUBSCRIPT(loOGScroll.laOgVRFlt,lnPosSls,1)
  lcCursorSls= loOgScroll.laOgVRFlt[lnPosSls,6]
  IF !EMPTY(lcCursorSls)
    SELECT(lcCursorSls)
    LOCATE
    IF !EOF()
      llSelSls = .T.
    ENDIF
  ENDIF
ENDIF
lcSeasonCursor = ''
llSeason = .F.
lnPosSeason = ASCAN(loOgScroll.laOgVRFlt,"INVHDR.SEASON")
IF lnPosSeason > 0
  lnPosSeason = ASUBSCRIPT(loOGScroll.laOgVRFlt,lnPosSeason,1)
  lcSeasons= loOgScroll.laOgVRFlt[lnPosSeason,6]
  IF !EMPTY(lcSeasons)
    llSeason =.T.
    lcSeasonCursor = loOgScroll.gfTempName()
    DIMENSION laTempacstru[1,4]
    laTempacstru[1,1]='Season'
    laTempacstru[1,2]='C'
    laTempacstru[1,3]= 6
    laTempacstru[1,4]= 0
    =gfCrtTmp(lcSeasonCursor,@laTempacstru,"SEASON",lcSeasonCursor,.T.)
    lnStart=1
    lnEnd=AT('|',lcSeasons)
    DO WHILE lnEnd <> 0
      SELECT(lcSeasonCursor)
      APPEND BLANK
      REPLACE SEASON WITH SUBSTR(lcSeasons,lnStart,lnEnd-1)
      lcSeasons = STUFF(lcSeasons ,lnStart,lnEnd,"")
      lnEnd=AT('|',lcSeasons)
    ENDDO
    IF lnEnd = 0
      SELECT(lcSeasonCursor)
      APPEND BLANK
      REPLACE SEASON WITH lcSeasons
    ENDIF
  ENDIF  
ENDIF

SELECT INVHDR
=gfSeek('')
LOCATE
SCAN
  IF llSeason AND !SEEK(INVHDR.SEASON,lcSeasonCursor)
    LOOP 
  ENDIF
  IF llSelSls AND !SEEK(INVHDR.REP1,lcCursorSls)
    LOOP 
  ENDIF
  IF !EMPTY(LDATE) AND !EMPTY(HDATE) AND !BETWEEN(INVHDR.INVDATE,LDATE,HDATE)
    LOOP 
  ENDIF 
  IF EMPTY(ALLTRIM(Rep1))
    LOOP 
  ENDIF 
  WAIT WINDOW 'Collecting Data....' NOWAIT
  IF !SEEK(INVHDR.REP1+INVHDR.SEASON,lcTmpData)
    INSERT INTO (lcTmpData) (Rep1,SEASON) VALUES (INVHDR.REP1,INVHDR.SEASON)
  ENDIF 
  lnCost = 0
  =gfSeek(INVHDR.INVOICE,'InvLine')
  SELECT InvLine
  SUM REST (Cost*TotQty) TO lnCost WHILE INVOICE = INVHDR.INVOICE
  SELECT (lcTmpData)
  REPLACE ShipAmt WITH ShipAmt+ INVHDR.ShipAmt,;
		  Discount with Discount+ INVHDR.Discount,;
		  nPstAmt with nPstAmt + INVHDR.nPstAmt ,;
		  Tax_Amt WITH Tax_Amt+  INVHDR.Tax_Amt,;
		  TotalChg WITH TotalChg +  INVHDR.TotalChg,;
          CodFRINSUM WITH CodFRINSUM +  ( INVHDR.Cod+ INVHDR.Freight+ INVHDR.Insur),;
		  ntotcost  WITH ntotcost+ lnCost
   SELECT INVHDR 
ENDSCAN 


*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar(MMT)
*! Date      : 05/07/2014
*! Purpose   : When function
*!*************************************************************
FUNCTION lfwRepWhen
IF !USED('INVHDR')
  =gfOpenTable('INVHDR','INVHDR')
ENDIF
IF !USED('INVLINE')
  =gfOpenTable('INVLINE','INVLINE')
ENDIF
