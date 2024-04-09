*:***************************************************************************
*: Program file  : SOOLS14
*: Program desc. : Wholesale Sales report for OLS10
*: System        : Aria Advantage Series.
*: Module        : RM 
*: Developer     : Mariam Mazhar(MMT)
*! Date          : 05/14/2014
*! Entry No.     : C201617[T20140430.0015]
*!***************************************************************************
*: Modifications:
*!***************************************************************************
ldLdate = {}
ldHdate = {}
lnDatePos  = ASUBSCRIPT(loogscroll.laOGFxFlt,ASCAN(loogscroll.laOGFxFlt,'RETHDR.CRDATE'),1)
IF lnDatePos > 0
  ldLdate = CTOD(SUBSTR(loogscroll.laOGFxFlt[lnDatePos,6],1,ATC('|',loogscroll.laOGFxFlt[lnDatePos,6])-1))
  ldHdate = CTOD(SUBSTR(loogscroll.laOGFxFlt[lnDatePos,6],  ATC('|',loogscroll.laOGFxFlt[lnDatePos,6])+1))
ENDIF


IF loogscroll.llOGFltCh 
  lfCrtTemp()
  lfCollectData()
ENDIF
IF !USED(lcRpMTmp) AND FILE(oAriaApplication.WorkDir + lcRpMTmp+ ".DBF")
  USE (oAriaApplication.WorkDir + lcRpMTmp+ ".DBF") IN 0 
ENDIF

SELECT (lcRpMTmp)
LOCATE
IF EOF()
  = gfModalGen('TRM00052B40011','ALERT')
  RETURN .F.
ENDIF
SET STEP ON 
USE IN (lcRpMTmp)
DIMENSION loOGScroll.laCRParams[3,2]
loOGScroll.laCRParams[2,1] = "Start"
loOGScroll.laCRParams[3,1] = "End"
loOGScroll.laCRParams[2,2] = DTOC(ldLdate)
loOGScroll.laCRParams[3,2] = DTOC(ldHdate)
loOGScroll.laCRParams[1,1] = 'ReportName'
loOGScroll.laCRParams[1,2] = 'Wholesale Sales Report'
loOGScroll.cCRPaperSize = 'LETTER'

DIMENSION loOgScroll.lacrTABLES[1]  && array For Temp Table & pathes
loOgScroll.lacrTABLES[1]= oAriaApplication.WorkDir+lcRpMTmp+'.DBF'

= gfDispRe(lcRpName)
*!*************************************************************
*! Name      : lfCrtTemp
*! Developer : Mariam Mazhar(MMT)
*! Date      : 05/14/2014
*! Purpose   : Create Temp.
*!*************************************************************
FUNCTION lfCrtTemp
IF USED(lcRpMTmp)
  USE IN (lcRpMTmp)
ENDIF
SELECT retline 
lnFldCnt=AFIELDS(laDataTmp)
DIMENSION laDataTmp[lnFldCnt+5,ALEN(laDataTmp,2)]
laDataTmp[lnFldCnt+1,1]= 'Tax_Amt'
laDataTmp[lnFldCnt+1,2]= 'N'
laDataTmp[lnFldCnt+1,3]= 13
laDataTmp[lnFldCnt+1,4]= 2 

laDataTmp[lnFldCnt+2,1]= 'hAmount'
laDataTmp[lnFldCnt+2,2]= 'N'
laDataTmp[lnFldCnt+2,3]= 14
laDataTmp[lnFldCnt+2,4]= 2 

laDataTmp[lnFldCnt+3,1]= 'nPstAmt'
laDataTmp[lnFldCnt+3,2]= 'N'
laDataTmp[lnFldCnt+3,3]= 13
laDataTmp[lnFldCnt+3,4]= 2 

laDataTmp[lnFldCnt+4,1]= 'Other'
laDataTmp[lnFldCnt+4,2]= 'N'
laDataTmp[lnFldCnt+4,3]= 13
laDataTmp[lnFldCnt+4,4]= 2 

laDataTmp[lnFldCnt+5,1]= 'TotCost'
laDataTmp[lnFldCnt+5,2]= 'N'
laDataTmp[lnFldCnt+5,3]= 13
laDataTmp[lnFldCnt+5,4]= 2
FOR lnCount = 1 TO 5
  STORE '' TO laDataTmp[lnFldCnt+lnCount,7],laDataTmp[lnFldCnt+lnCount,8],laDataTmp[lnFldCnt+lnCount,9],;
              laDataTmp[lnFldCnt+lnCount,10],laDataTmp[lnFldCnt+lnCount,11],laDataTmp[lnFldCnt+lnCount,12],;
              laDataTmp[lnFldCnt+lnCount,13],laDataTmp[lnFldCnt+lnCount,14],laDataTmp[lnFldCnt+lnCount,15],;
              laDataTmp[lnFldCnt+lnCount,16]
  STORE 0 TO  laDataTmp[lnFldCnt+lnCount,17],laDataTmp[lnFldCnt+lnCount,18]
ENDFOR
=gfCrtTmp(lcRpMTmp,@laDataTmp,"Gl_Sales+REASON+STR(RECNO(),3)",lcRpMTmp,.F.)

*!*************************************************************
*! Name      : lfOgWhen
*! Developer : Mariam Mazhar(MMT)
*! Date      : 05/07/2014
*! Purpose   : When function
*!*************************************************************
FUNCTION lfOgWhen
=gfOpenTable('rethdr','rethdr')
=gfOpenTable('retline','retline')
=gfOpenTable('Style','Style')

*!*************************************************************
*! Name      : lfCollectData
*! Developer : Mariam Mazhar(MMT)
*! Date      : 05/07/2014
*! Purpose   : Collect Data
*!*************************************************************
FUNCTION lfCollectData
lcCursorSls = ''
llSelSls = .F.
lnPosSls = ASCAN(loOgScroll.laOgFxFlt,"SALESREP.REPCODE")
IF lnPosSls > 0
  lnPosSls = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosSls,1)
  lcCursorSls= loOgScroll.laOgFxFlt[lnPosSls,6]
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
lnPosSeason = ASCAN(loOgScroll.laOgFxFlt,"STYLE.SEASON")
IF lnPosSeason > 0
  lnPosSeason = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosSeason,1)
  lcSeasons= loOgScroll.laOgFxFlt[lnPosSeason,6]
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
lcFilter = " Rethdr.SalesRep1 <> ''"
DO CASE
  CASE EMPTY(ldLdate) .AND. !EMPTY(ldHdate)
    lcFilter = lcFilter + " AND RetHdr.crDate <= ?m.ldHdate AND RetHdr.Status <> 'V' "
  CASE !EMPTY(ldLdate) .AND. EMPTY(ldHdate)
    lcFilter = lcFilter + " AND RetHdr.crDate >= ?m.ldLdate .AND.RetHdr.Status <> 'V' "
  CASE !EMPTY(ldLdate) .AND. !EMPTY(ldHdate)
    IF (ldHdate) >= (ldLdate)
      lcFilter = lcFilter + " AND RetHdr.crDate BETWEEN ?m.ldLdate AND ?m.ldHdate AND RetHdr.Status <> 'V'"
    ENDIF
  CASE EMPTY(ldLdate) .AND. EMPTY(ldHdate)
    lcFilter = lcFilter + " AND RetHdr.Status <> 'V'"
ENDCASE              

SELECT Rethdr
=gfSqlRun("Select * from Rethdr Where "+lcFilter,'Rethdr',.F.,'Rethdr')
SELECT Rethdr
LOCATE 
SCAN FOR IIF(llSelSls ,SEEK(SalesRep1,lcCursorSls),.T.)
  SELECT RETLINE
  =gfSeek(Rethdr.crmemo)
  SCAN REST WHILE CRMEMO+STYLE+CRET_LINNO+CRET_TRNCD = Rethdr.crmemo FOR cret_trncd = "2" 
    IF !gfSeek(RETLINE.Style,'Style')
      LOOP 
    ENDIF 
    IF llSeason and !SEEK(Style.Season,lcSeasonCursor)
      LOOP
    ENDIF
    WAIT WINDOW  'Style  :  ' +   RETLINE.STYLE  NOWAIT
    SCATTER MEMVAR MEMO
    m.TotCost = Style.TotCost
    m.Tax_Amt = Rethdr.Tax_Amt
    m.HAmount = Rethdr.Amount
    m.nPstAmt = Rethdr.nPstAmt
    m.Other = Rethdr.Other
    SELECT (lcRpMTmp)
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE REASON   WITH Style.Season
    REPLACE Gl_Sales WITH Rethdr.SalesRep1
    SELECT RetLine
  ENDSCAN
ENDSCAN
SELECT (lcRpMTmp)
LOCATE
DELETE FOR HAMOUNT=0 OR Price=0 OR TotQty =0
