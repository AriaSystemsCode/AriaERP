*!***************************************************************************************************************************************
*! Name      : ALPKTKTMO.PRG
*! Developer : Mariam Mazhar (MMT)
*! Date      : 11/26/2008
*! Purpose   : Custom PIKTKT Form For Memo (C201063){T20080908.0001}
*!***************************************************************************************************************************************
*! Modifications:
*! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[T20080908.0001]
*!***************************************************************************************************************************************
SELECT (lcTmpOrdL)
IF EOF()
  RETURN 
ENDIF 
lcTempShp = loogscroll.gfTempName()
lcCartTemp = loogscroll.gfTempName()
lcStylePik = loogscroll.gfTempName()
lcPIKStyle = loogscroll.gfTempName()
lcScaleTable = loogscroll.gfTempName()
lcTtktwght = loogscroll.gfTempName() 
lcTtkStywght = loogscroll.gfTempName() 
STORE 0 TO lnClrLen,LNCLRSTRT,lnSclLen ,lnSclPos ,lnTotQty 
  
DIMENSION laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  DO CASE
  CASE laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    LNCLRSTRT = laItemSeg[lnCount,4]
    lcClrSpr = ALLT(laItemSeg[lnCount,6])    
     
  CASE  laItemSeg[lnCount,1]='S'
    lnSclLen = LEN(laItemSeg[lnCount,3])
    lnSclPos = laItemSeg[lnCount,4]
    
  ENDCASE  
ENDFOR
lnMajLen = LEN(gfItemMask("PM","",'0001'))


lnStyClrLen  = lnClrPosM1+  lnClrLnM1-1
lfCreateTmp()

SELECT  (lcTempShp)




sELECT (lcTmpOrdL)
lcOldOrder = ORDER()
INDEX on PIKTKT+Style TAG 'PKSTYLE'
SET ORDER to  'PKSTYLE'
lcPikSty = SPACE(lnStyClrLen)
SCAN
  IF lcPikSty  =PIKTKT+substr(Style,1,lnClrPosM1 +  lnClrLnM1-1)
    DELETE 
  ELSE
    lcPikSty  =PIKTKT+substr(Style,1,lnClrPosM1 +  lnClrLnM1-1)
  ENDIF 
ENDSCAN 
SET ORDER TO (lcOldOrder)

SELECT (lcTempShp)
SET ORDER TO 'PStyle'

SELECT (lcTmpOrdL)
SET RELATION TO PIKTKT+substr(Style,1,lnClrPosM1 +  lnClrLnM1-1) INTO (lcTempShp) ADDITIVE 
SET RELATION TO PIKTKT+substr(Style,1,lnClrPosM1 +  lnClrLnM1-1)  INTO (lcScaleTable) ADDITIVE 


SELECT (lcTmpOrdL)
SET SKIP TO (lcTempShp)
LOCATE 
lnGrsWeigt = 0
lnCubicSz = 0
lnSzCnt = 0
= gfDispRe()
IF loOgScroll.ll2Printer=.T.  
  llPrinter = .T.
ENDIF   
sELECT (lcTmpOrdL)
SET SKIP to
RECALL ALL 
llOGFltCh = .T.
llAlpktk = .F.

*:**************************************************************************
*:* Name        : lfCreateTmp
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 07/21/2008
*:* Purpose     : Create Temp. Files
*:*************************************************************************** 
FUNCTION lfCreateTmp

DIMENSION laTempFileStr[5,4]
laTempFileStr[1,1] = 'Style'
laTempFileStr[1,2] = 'C'
laTempFileStr[1,3] = lnStyClrLen
laTempFileStr[1,4] = 0

laTempFileStr[2,1] = 'PIKTKT'
laTempFileStr[2,2] = 'C'
laTempFileStr[2,3] = 6
laTempFileStr[2,4] = 0

laTempFileStr[3,1] = 'nSclCnt'
laTempFileStr[3,2] = 'N'
laTempFileStr[3,3] = 5
laTempFileStr[3,4] = 0


laTempFileStr[4,1] = 'DESC1'
laTempFileStr[4,2] = 'C'
laTempFileStr[4,3] = 60
laTempFileStr[4,4] = 0

laTempFileStr[5,1] = 'nTotQty'
laTempFileStr[5,2] = 'N'
laTempFileStr[5,3] = 7
laTempFileStr[5,4] = 0

=gfCrtTmp(lcPIKStyle ,@laTempFileStr,[Style+PIKTKT],lcPIKStyle )



SELECT (lcPIKStyle)
INDEX on PIKTKT+Style TAG 'POSTYLE'
SET ORDER TO (lcPIKStyle)
SELECT (lcTmpOrdL)
SCAN
  IF !SEEK(SUBSTR(Style,1,lnStyClrLen)+PIKTKT,lcPIKStyle)  
    m.Style = SUBSTR(Style,1,lnStyClrLen)
    m.PIKTKT = PIKTKT
    INSERT INTO (lcPIKStyle) FROM MEMVAR 
  ENDIF 
ENDSCAN 

lnMaxScl = 0
IF !USED('Style_A')
  =gfOpenTable('Style','Style','SH','Style_A')
ENDIF 
IF !USED('Scale')
  =gfOpenTable('Scale','Scale')
ENDIF 

*Scan to collect upper grid info. and get Max scale count used.
SELECT (lcPIKStyle)
SET ORDER TO 'POSTYLE'
LOCATE 
lnPOCnt = 0
SCAN 
  lnPIKCnt = 0
  lcCurrStyleMaj = Style
  lcCurrPIKTKT = PIKTKT
  =gfSeek(ALLTRIM(lcCurrStyleMaj) ,'Style_A','Style')
  SELECT 'Style_A'
  REPLACE DESC1 WITH Style_A.Desc1 IN (lcPIKStyle)
  SCAN REST WHILE Style =ALLTRIM(lcCurrStyleMaj)
    SELECT (lcTmpOrdL)
    LOCATE FOR pIKTKT = lcCurrPIKTKT  AND style = Style_A.Style 
    IF Found()
      =gfSeek("S"+Style_A.Scale,'Scale')    
      lnPikCnt = lnPikCnt + Scale.cnt 
      REPLACE  nTotQty WITH nTotQty + &lcTmpOrdL..totqty IN (lcPIKStyle)
    ENDIF 
  ENDSCAN
  REPLACE nSclCnt WITH lnPikCnt in (lcPIKStyle)
   
  IF lnPikCnt  > lnMaxScl
    lnMaxScl = lnPikCnt  
  ENDIF 
ENDSCAN 


*Scan to get the scale sizes used for each style color PO
IF lnMaxScl = 0
  RETURN .F.
ENDIF 
DIMENSION laSclFile[lnMaxScl +2 ,4]

laSclFile[1,1] = 'Style'
laSclFile[1,2] = 'C'
laSclFile[1,3] = lnStyClrLen
laSclFile[1,4] = 0

laSclFile[2,1] = 'PIKTKT'
laSclFile[2,2] = 'C'
laSclFile[2,3] = 6
laSclFile[2,4] = 0

lnCnt = 3

FOR lnI = 1 TO lnMaxScl 
  laSclFile[lnCnt ,1] = 'Scl'+ALLTRIM(STR(lnI))
  laSclFile[lnCnt ,2] = 'C'
  laSclFile[lnCnt ,3] = 5
  laSclFile[lnCnt ,4] = 0
  lnCnt = lnCnt + 1
ENDFOR 

=gfCrtTmp(lcScaleTable ,@laSclFile,[PIKTKT+Style],lcScaleTable)


SELECT (lcPIKStyle)
SET ORDER TO 'POSTYLE'
LOCATE 
SCAN 
  IF SEEK(Style+PIKTKT,lcScaleTable )
    LOOP 
  ENDIF 
  lcCurrStyleMaj = Style
  lcCurrPIKTKT   = PIKTKT
  =gfSeek(ALLTRIM(lcCurrStyleMaj) ,'Style_A','Style')
  SELECT 'Style_A'
  m.Style  = lcCurrStyleMaj
  m.PIKTKT = lcCurrPIKTKT  
  lnCnter = 1
  
  *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[Start]
  FOR lnO = 1 TO lnMaxScl 
    lcO = ALLTRIM(STR(lnO))
    STORE '' TO m.Scl&lcO
  ENDFOR 
  *C201063,2 MMT 12/22/2008 Fix bugs reported by customer[End]
  
  SCAN REST WHILE Style =ALLTRIM(lcCurrStyleMaj)
    SELECT (lcTmpOrdL)
    LOCATE FOR pIKTKT = lcCurrPIKTKT  AND style = Style_A.Style 
    IF Found() 
      =gfSeek("S"+Style_A.Scale,'Scale')    
      FOR lnI = 1 TO Scale.cnt 
        lcI = ALLTRIM(STR(lnI))
        lcCnter = ALLTRIM(STR(lnCnter))
        m.Scl&lcCnter  = Scale.Sz&lcI. 
        lnCnter = lnCnter + 1
      ENDFOR 
    ENDIF 
   
  ENDSCAN
  IF !SEEK(m.PIKTKT+m.Style,lcScaleTable )
     INSERT  INTO (lcScaleTable ) FROM MEMVAR 
  ELSE
     SELECT(lcScaleTable)
     GATHER MEMO MEMVAR  
  ENDIF 
ENDSCAN 

SELECT (lcPIKStyle)
LOCATE 

lfCrtTempFl()

  *Fill Temp File from SQL Table
IF !USED('POCRTNMF')
  =gfOpenTable('POCRTNMF','pocrtnmf')
ENDIF 
IF !USED('STYLE_A')
  =gfOpenTable('STYLE','STYLE','SH','STYLE_A')
ENDIF 
SELECT 'POCRTNMF'
gfSetOrder('POPIKTKT')


lcTempLPO = loogscroll.gfTempName()
DIMENSION laFilStr[4,4]
laFilStr[1,1] = 'PIKTKT'
laFilStr[1,2] = 'C'
laFilStr[1,3] = 6
laFilStr[1,4] = 0

laFilStr[2,1] = 'Style'
laFilStr[2,2] = 'C'
laFilStr[2,3] = 19
laFilStr[2,4] = 0

laFilStr[3,1] = 'NlineNo'
laFilStr[3,2] = 'N'
laFilStr[3,3] = 4
laFilStr[3,4] = 0

laFilStr[4,1] = 'PO'
laFilStr[4,2] = 'C'
laFilStr[4,3] = 6
laFilStr[4,4] = 0

=gfCrtTmp(lcTempLPO ,@laFilStr,[Style+PIKTKT+PO+STR(NlineNo,4)],lcTempLPO )



SELECT (lcPIKStyle)
lcPitkt = ''

*! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[Start]
lcTmpPOCRTNMF = loogScroll.gfTempName()
*! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[End]

lnCnt = 0
SCAN 
  IF lcPitkt <>  &lcPIKStyle..PIKTKT
    lnCnt = 0
  ENDIF 
  
  SELECT 'POCRTNMF'
  m.sTYLE = &lcPIKStyle..Style
  M.piktkt = &lcPIKStyle..piktkt
  m.nNoSz = &lcPIKStyle..nSclCnt
  m.SzCnt = -1 
  m.SzOrder = ""

  *! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[Start]
  *IF gfSEEK(&lcPIKStyle..piktkt)
  *SELECT 'POCRTNMF'
  *lcSqlStatement, lcAlias, llNoIndex, lcTempAlias
  IF gfSqlRun("Select * From POCRTNMF Where PIKTKT = '"+&lcPIKStyle..piktkt+"'",'POCRTNMF',.T.,lcTmpPOCRTNMF)
    SELECT (lcTmpPOCRTNMF)
    lnBuffering = CURSORGETPROP("Buffering",lcTmpPOCRTNMF)
    =CURSORSETPROP("Buffering",3,lcTmpPOCRTNMF)
    INDEX on PIKTKT+Style TAG  (lcTmpPOCRTNMF)
  *! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[End]
    
    lnCurrLnNo = -1
    lcPONo = '*****'
    DIMENSION laCartons[1]
    
    STORE '' TO laCartons
      SCAN REST WHILE PIKTKT = &lcPIKStyle..piktkt FOR Style =ALLTRIM(&lcPIKStyle..Style) 
        IF SEEK(PADR(m.sTYLE,19) +m.Piktkt+PO+STR(nlineno,4),lcTempLPO)
          *lnCurrLnNo = nlineno AND lcPONo = PO
          LOOP 
        ELSE
          *! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[Start]
          *INSERT INTO (lcTempLPO) VALUES (m.Piktkt,m.sTYLE,POCRTNMF.nlineno,POCRTNMF.PO)
          INSERT INTO (lcTempLPO) VALUES (m.Piktkt,m.sTYLE,&lcTmpPOCRTNMF..nlineno,&lcTmpPOCRTNMF..PO)
          *! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[End]
          
          DIMENSION laCartons[1]
          STORE '' TO laCartons
        ENDIF 
        lnCurrLnNo = nlineno
        lcPONo = PO
        lnCurrRec = RECNO()
        =SEEK(&lcPIKStyle..piktkt)
        lnCrtCnt = 0 
        lcCurrCrt = '  '
        lnSclCnt = 1
        FOR lnI = 1 TO lnMaxScl 
          lcI = ALLTRIM(STR(lnI))
          STORE 0 TO m.Qty&lcI.
        ENDFOR 
        STORE 0 TO m.TOTQTY
        
        *! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[Start]
        *lcStyle = POCRTNMF.Style
        lcStyle = &lcTmpPOCRTNMF..Style
        *! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[End]
        
        SCAN REST WHILE PIKTKT = &lcPIKStyle..piktkt FOR nLINENO = lnCurrLnNo AND lcPONo = PO AND sTYLE =ALLTRIM(&lcPIKStyle..Style) 
         
         *! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[Start]
         *IF lcStyle = POCRTNMF.Style 
          IF lcStyle = &lcTmpPOCRTNMF..Style 
         *! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[End]
            lnSclCnt = 1
          ENDIF 
          
          *! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[Start]
          *m.CCRTNVLTYP = POCRTNMF.CCARTONTYP
          m.CCRTNVLTYP = &lcTmpPOCRTNMF..CCARTONTYP
          *! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[End]
          IF ASCAN(laCartons,CCARTONNo ,1) = 0
            lnCrtCnt = lnCrtCnt + 1
          ENDIF 
          m.PO = PO
          *! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[Start]
*!*	          m.LineNo = POCRTNMF.NlineNo
*!*	          m.ngrsWght= POCRTNMF.Weight
*!*	          =gfSeek(POCRTNMF.Style,'STYLE_A')
          m.LineNo = &lcTmpPOCRTNMF..NlineNo
          m.ngrsWght= &lcTmpPOCRTNMF..Weight
          =gfSeek(&lcTmpPOCRTNMF..Style,'STYLE_A')
         *! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[End]
         
          =gfSeek('S'+Style_A.Scale,'Scale') 
          SELECT Style_A
          =gfSeek(ALLTRIM(&lcPikStyle..Style),'STYLE_A')
          lnSclCnt = 1
          SCAN REST WHILE Style = ALLTRIM(&lcPikStyle..Style) 
             SELECT (lcTmpOrdL)
		     LOCATE FOR pIKTKT = &lcPIKStyle..PIKTKT AND style = Style_A.Style 
			 IF !Found()  
			   LOOP 
			 ENDIF 
           
            =gfSeek('S'+Style_A.Scale,'Scale') 
            
            *! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[Start]
            *IF Style_A.Style = POCRTNMF.Style
            IF Style_A.Style = &lcTmpPOCRTNMF..Style
            *! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[End]
            
              FOR lnZ = 1 TO Scale.cnt
                lcSclCnt =ALLTRIM(STR(lnSclCnt))
                lcZ = STR(lnZ,1)
                
                *! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[Start]
                *m.Qty&lcSclCnt. =  POCRTNMF.Qty&lcZ.                
                m.Qty&lcSclCnt. =  &lcTmpPOCRTNMF..Qty&lcZ.
                *! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[End]
                
                m.TOTQTY = m.TOTQTY + m.Qty&lcSclCnt. 
                lnSclCnt = lnSclCnt +  1 
              ENDFOR 
              EXIT 
            ELSE
              lnSclCnt = lnSclCnt + Scale.cnt
            ENDIF  
          ENDSCAN  
          
          *! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[Start]
          *SELECT 'POCRTNMF'           
          SELECT (lcTmpPOCRTNMF)
          *! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[End]
          
          IF EMPTY(laCartons[1])
            laCartons[1] = CCARTONNo 
          ELSE
            DIMENSION laCartons[ALEN(laCartons,1)+1]
            laCartons[ALEN(laCartons,1)] = CCARTONNo 
          ENDIF
          
          *! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[Start]
          *lcStyle = POCRTNMF.Style           
          lcStyle = &lcTmpPOCRTNMF..Style 
          *! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[End]
          *****
          DIMENSION laCartRelated[1,2]
          laCartRelated[1,1] = 'NCRTCAPCTY'
          laCartRelated[1,2] = 'lnCapcty'
          sTORE 0 TO lnCapcty
          
          *! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[Start]
          *llNoThing = gfRltFld(POCRTNMF.CCartontyp, @laCartRelated, "CCRTNVLTYP")          
          llNoThing = gfRltFld(&lcTmpPOCRTNMF..CCartontyp, @laCartRelated, "CCRTNVLTYP")
          *! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[End]

          *****
          m.nCrtSz = lnCapcty && POCRTNMF.nlength * POCRTNMF.ndepth * VAL(POCRTNMF.width)
        ENDSCAN 
        m.NCrtTot = lnCrtCnt 

    	IF lnCnt = 0
    	  lnCnt = 1
    	ENDIF 
 		m.cCrtStr = ALLTRIM(STR(lnCnt)) +"-"+ ALLTRIM(STR(lnCnt  +m.NCrtTot - 1))
 		lnCnt = lnCnt + m.NCrtTot
		
        IF !SEEK(m.Piktkt+m.Style+m.CCRTNVLTYP+m.PO+STR(m.LINENO,6),lcTempShp)
          INSERT INTO (lcTempShp) FROM MEMVAR 
        ENDIF 
        
        *! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[Start]
        *SELECT 'POCRTNMF'
        SELECT(lcTmpPOCRTNMF)
        *! C201063,3 MMT 01/01/2009 Fix bug of wrong styles order in Piktkt Form[End]
        
        IF BETWEEN(lnCurrRec,1,RECCOUNT())
          GO RECORD lnCurrRec
        ENDIF 
      ENDSCAN 
    ENDIF
    lcPitkt =  &lcPIKStyle..PIKTKT
  ENDSCAN 

*SzCnt


DIMENSION laPkWght[3,4]

laPkWght[1,1] = 'Piktkt'
laPkWght[1,2] = "C"
laPkWght[1,3] = 6
laPkWght[1,4] = 0

laPkWght[2,1] = 'nGrsWght'
laPkWght[2,2] = 'N'
laPkWght[2,3] = 11
laPkWght[2,4] = 2

laPkWght[3,1] = 'nCubSz'
laPkWght[3,2] = 'N'
laPkWght[3,3] = 11
laPkWght[3,4] = 3

=gfCrtTmp(lcTtktwght ,@laPkWght,[PIKTKT],lcTtktwght )


DIMENSION laPkStWght[4,4]

laPkStWght[1,1] = 'Piktkt'
laPkStWght[1,2] = "C"
laPkStWght[1,3] = 6
laPkStWght[1,4] = 0

laPkStWght[2,1] = 'nGrsWght'
laPkStWght[2,2] = 'N'
laPkStWght[2,3] = 11
laPkStWght[2,4] = 2

laPkStWght[3,1] = 'StYle'
laPkStWght[3,2] = 'C'
laPkStWght[3,3] = 19
laPkStWght[3,4] = 0

laPkStWght[4,1] = 'NoCartNo'
laPkStWght[4,2] = 'N'
laPkStWght[4,3] = 7
laPkStWght[4,4] = 0

=gfCrtTmp(lcTtkStywght ,@laPkStWght,[PIKTKT+Style],lcTtkStywght )


lnCntin = 1
SELECT (lcTempShp)
SET ORDER TO 

SCAN
  IF SEEK(PIKTKT,lcTtktwght)
     LOOP 
  ENDIF 
  lcRecNum = RECNO()
  lcPiktk = Piktkt
  lnTotWght = 0
  lnnCubSz = 0
  SUM  NCrtTot * NgrsWght,NCrtTot * nCrtSz FOR Piktkt = lcPiktk TO lnTotWght,lnnCubSz
  INSERT INTO (lcTtktwght) VALUES (lcPiktk ,lnTotWght,lnnCubSz)
  SELECT (lcTempShp)
  IF BETWEEN(lcRecNum ,1,RECCOUNT())
    GO RECORD lcRecNum 
  ENDIF 
ENDSCAN 

SELECT (lcTempShp)
SET ORDER TO 

SCAN
  IF SEEK(PIKTKT+PADR(Style,19),lcTtkStywght )
     LOOP 
  ENDIF 
  lcRecNum = RECNO()
  lcPiktk = Piktkt
  lcStyle = Style
  lnTotWght = 0
  lnNoCart = 0
  SUM  NCrtTot * NgrsWght,NCrtTot  FOR Piktkt = lcPiktk AND Style = lcStyle  TO lnTotWght,lnNoCart 
  INSERT INTO (lcTtkStywght ) VALUES (lcPiktk,lnTotWght,lcStyle, lnNoCart )
  SELECT (lcTempShp)
  IF BETWEEN(lcRecNum ,1,RECCOUNT())
    GO RECORD lcRecNum 
  ENDIF 
ENDSCAN 




SELECT (lcTempShp)
LOCATE 

lcDeleT = SET("Deleted")
SET DELETED OFF 
SCAN FOR SzCnt = -1 
  lnCurRec = RECNO()
  lnRCnt =  CEILING(nNoSz/16)
  lnStartCnt = 0
  REPLACE SzCnt  WITH 0,;
    	  SzOrder WITH 	PADL(ALLTRIM(STR(lnCntin)),7,'0')
  
  lnQTY = 0
  FOR lnY = 1 TO MIN(16,nNoSz )
    lcY = ALLTRIM(STR(lnY ))
    lnQTY = lnQTY +  Qty&lcY.
  ENDFOR
   
  IF lnQTY = 0
    DELETE 
  ELSE
    REPLACE TotQty WITH   lnQTY * NCrtTot
  ENDIF 
  
  FOR lnW = 1 TO lnRCnt-1 
    SCATTER MEMO MEMVAR
    m.SzCnt = lnStartCnt + 16
    m.SzOrder = PADL(ALLTRIM(STR(lnCntin)),7,'0')
    lnStartCnt = lnStartCnt + 16
    lnQTY = 0
    FOR lnC  = lnStartCnt+1 TO MIN(lnStartCnt +16-1,nNoSz )
      lcC = ALLTRIM(STR(lnC ))
      lnQTY = lnQTY +  Qty&lcC.
    ENDFOR
    
    
    IF lnQTY > 0
      m.TotQty = lnQTY * m.NCrtTot
      APPEND BLANK 
      GATHER MEMO MEMVAR  
    ENDIF   
  ENDFOR  
  IF BETWEEN(lnCurRec ,1,RECCOUNT())
    GO RECORD lnCurRec 
  ENDIF 
  lnCntin = lnCntin + 1
ENDSCAN 
SET DELETED &lcDeleT 
  
*!*************************************************************
*! Name      : lfCrtTempFl
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/10/2008
*! Purpose   : Create Temp. File
*!*************************************************************
FUNCTION lfCrtTempFl
DIMENSION laFilStru[lnMaxScl +14,4]
laFilStru[1,1] = 'PIKTKT'
laFilStru[1,2] = 'C'
laFilStru[1,3] = 6
laFilStru[1,4] = 0

laFilStru[2,1] = 'Style'
laFilStru[2,2] = 'C'
laFilStru[2,3] = lnStyClrLen
laFilStru[2,4] = 0

laFilStru[3,1] = 'CCRTNVLTYP'
laFilStru[3,2] = 'C'
laFilStru[3,3] = 6
laFilStru[3,4] = 0

laFilStru[4,1] = 'NCrtTot'
laFilStru[4,2] = 'N'
laFilStru[4,3] = 6
laFilStru[4,4] = 0

laFilStru[5,1] = 'TOTQty'
laFilStru[5,2] = 'N'
laFilStru[5,3] = 9
laFilStru[5,4] = 0

laFilStru[6,1] = 'LINENO'
laFilStru[6,2] = 'N'
laFilStru[6,3] = 6
laFilStru[6,4] = 0

laFilStru[7,1] = 'Cstatus'
laFilStru[7,2] = 'C'
laFilStru[7,3] = 1
laFilStru[7,4] = 0

laFilStru[8,1] = 'NgrsWght'
laFilStru[8,2] = 'N'
laFilStru[8,3] = 8
laFilStru[8,4] = 2


laFilStru[9,1] = 'cCrtStr'
laFilStru[9,2] = 'C'
laFilStru[9,3] = 20
laFilStru[9,4] = 0

laFilStru[11,1] = 'PO'
laFilStru[11,2] = 'C'
laFilStru[11,3] = 6
laFilStru[11,4] = 0

laFilStru[10,1] = 'nCrtSz'
laFilStru[10,2] = 'N'
laFilStru[10,3] = 10
laFilStru[10,4] = 4

laFilStru[12,1] = 'nNoSz'
laFilStru[12,2] = 'N'
laFilStru[12,3] = 5
laFilStru[12,4] = 0

laFilStru[13,1] = 'SzCnt'
laFilStru[13,2] = 'N'
laFilStru[13,3] = 5
laFilStru[13,4] = 0

laFilStru[14,1] = 'SzOrder'
laFilStru[14,2] = 'C'
laFilStru[14,3] = 7
laFilStru[14,4] = 0
lnCnt = 15

FOR lnI = 1 TO lnMaxScl 
  laFilStru[lnCnt ,1] = 'Qty'+ALLTRIM(STR(lnI))
  laFilStru[lnCnt ,2] = 'N'
  laFilStru[lnCnt ,3] = 7
  laFilStru[lnCnt ,4] = 0
  lnCnt = lnCnt + 1
ENDFOR 
=gfCrtTmp(lcTempShp,@laFilStru,[PIKtKT+Style+PO+STR(LINENO,6)],lcTempShp)  
SELECT (lcTempShp)
INDEX on PIKtKT+Style+SzOrder TAG 'PStyle'
SET ORDER TO (lcTempShp)
*!*************************************************************
*! Name      : lfCalcTotQty
*: Developer : Mariam Mazhar[MMT]
*: Date      : 11/10/2008
*! Purpose   : Calc. Piktkt totQty
*!*************************************************************
FUNCTION lfCalcTotQty
lcStyle = &lcTempShp..Style
lcPiktkt = &lcTempShp..PIKTKT
lcAlias =SELECT(0)
SELECT (lcTempShp)
lnRecNo = RECNO()
=SEEK(lcPiktkt+lcStyle)
lnTotQty = 0
SCAN REST WHILE PIKtKT+Style+STR(LINENO,6) =  lcPiktkt+lcStyle
 lnTotQty = lnTotQty + TOTQTY
ENDSCAN 
IF BETWEEN(lnRecNo ,1,RECCOUNT(lcTempShp))
  GO RECORD lnRecNo IN (lcTempShp)
ENDIF 
SELECT(lcAlias )
RETURN lnTotQty

FUNCTION lfClcWght
lcPiktkt = &lcTempShp..PIKTKT
lnGrsWeigt = 0
lnCubicSz = 0
IF SEEK(lcPiktkt ,lcTtktwght)
  lnGrsWeigt = &lcTtktwght..nGrsWght
  lnCubicSz =  &lcTtktwght..nCubSz
ENDIF 
RETURN .T.  


FUNCTION lfGetStyWght
lcStyle = &lcTempShp..Style
lcPiktkt = &lcTempShp..PIKTKT
lnWghtSty = 0
IF SEEK(lcPiktkt+PADR(lcStyle,19),lcTtkStywght )
   lnWghtSty = &lcTtkStywght..nGrsWght
ENDIF 
RETURN lnWghtSty 


FUNCTION lfGetNoCrt
lcStyle = &lcTempShp..Style
lcPiktkt = &lcTempShp..PIKTKT
lnCrtSty = 0
IF SEEK(lcPiktkt+PADR(lcStyle,19),lcTtkStywght )
  lnCrtSty = &lcTtkStywght..NoCartNo
ENDIF 
RETURN lnCrtSty 