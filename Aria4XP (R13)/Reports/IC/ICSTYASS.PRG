*!*************************************************************
*! Name      : ICSTYASS.PRG Report
*! Developer : Hesham Elmasry(HES)
*! Date      : 08/11/2009
*!*************************************************************
DIMENSION laStyStru[1]
lcStyMajLen =  gfItemMask(@laStyStru)
lnStyLen  = LEN(laStyStru[1,3])
lnColrPos = laStyStru[2,4]
lnColrLen = LEN(laStyStru[2,3])

loogScroll.cCROrientation = 'P'

IF llOgFltCh
  IF !lfColData()
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN 
  ENDIF 
ENDIF 

SELECT(lcPriTemp)
=gfDispRe()

*!*************************************************************
*! Name      : lfWgrid
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 12/04/2003
*! Purpose   : 
*!*************************************************************
FUNCTION lfWgrid

IF !USED('STYINVJL') 
  =gfOpenTABLE(oAriaApplication.DATADIR+'STYINVJL',oAriaApplication.DATADIR+'STYINVJL','SH')
ENDIF

IF !USED('INVTADJ') 
  =gfOpenTABLE(oAriaApplication.DATADIR+'INVTADJ',oAriaApplication.DATADIR+'INVTADJ','SH')
ENDIF

IF !USED('STYLE') 
  =gfOpenTABLE(oAriaApplication.DATADIR+'STYLE',oAriaApplication.DATADIR+'STYLE','SH')
ENDIF

IF !USED('SCALE') 
  =gfOpenTABLE(oAriaApplication.DATADIR+'SCALE',oAriaApplication.DATADIR+'SCALE','SH')
ENDIF

IF !USED('STYDYE') 
  =gfOpenTABLE('STYDYE','STYDYEW','SH')
ENDIF


IF !USED('WAREHOUS')
  =gfOpenTable('WAREHOUS','WAREHOUS')
ENDIF 
SELECT 'WAREHOUS'
=gfSeek('')
DIMENSION laLocVal[1]
DIMENSION laLocDesc[1]
laLocDesc = ''
laLocVal = ''
SELECT CWARECODE  FROM 'WAREHOUS' ORDER BY CWARECODE INTO ARRAY laLocVal
SELECT cdesc FROM 'WAREHOUS' ORDER BY CWARECODE INTO ARRAY laLocDesc

FUNCTION lfDefloc 
SELECT 'WAREHOUS'
LOCATE FOR ldefware 
IF FOUND()
  lcDefloc = CWARECODE
ELSE
  lcDefloc = ''
ENDIF 
RETURN   lcDefloc

*!*************************************************************
*! Name      : lfCreatExp
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 12/04/2003
*! Purpose   : Return the selected status in the ReadBox
*!*************************************************************
FUNCTION lfCreatExp

*************************************************************
*! Name      : lfCheckFilter
*! Developer : Hesham Elmasry
*! Date      : 10/07/2009
*! Purpose   : Return the select values from filters
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
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]  
    ENDIF
  CASE lnArrayType = 3  
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)  
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6] 
    ENDIF
ENDCASE

RETURN lcReturn 

* End of lfCheckFilter

*************************************************************
*! Name      : lfColData
*! Developer : Hesham Elmasry
*! Date      : 10/07/2009
*! Purpose   : Colecting data
*!*************************************************************
FUNCTION lfColData

=lcCrtTemp() && Create the primary temp

PRIVATE     lcStyTmp, lcSeason, lcDivision, lcStyGroup, ldAdjDat, ldLAdjDat, ldHAdjDat
STORE '' TO lcStyTmp, lcSeason, lcDivision, lcStyGroup, ldAdjDat, ldLAdjDat, ldHAdjDat

ldAdjDat  = lfCheckFilter(1,"INVTADJ.DATE      ")
ldLAdjDat = CTOD(SUBSTR(ldAdjDat,1,  ATC('|',ldAdjDat)-1))
ldHAdjDat = CTOD(SUBSTR(ldAdjDat,    ATC('|',ldAdjDat)+1))

IF EMPTY(ldLAdjDat)
  ldLAdjDat = ldHAdjDat 
ENDIF

lnStSel    = 0
lcStyTmp   = lfCheckFilter(1,"STYLE.STYLE")
lcSeason   = lfCheckFilter(1,"STYLE.SEASON   ")
lcDivision = lfCheckFilter(1,"STYLE.CDIVISION ")
lcStyGroup = lfCheckFilter(1,"STYLE.CSTYGROUP ")

IF !EMPTY(lcStyTmp)
  SELECT(lcStyTmp)
  COUNT TO lnStSel for !DELETED()
ENDIF

lcStyFlExp  = " IIF(lnStSel > 0        , SEEK(STYLE.STYLE, lcStyTmp)  ,  .T.) AND " +;
              " IIF(!EMPTY(lcSeason)   , STYLE.SEASON    $ lcSeason   ,  .T.) AND " +;
              " IIF(!EMPTY(lcDivision) , STYLE.CDIVISION $ lcDivision ,  .T.) AND " +;
              " IIF(!EMPTY(lcStyGroup) , STYLE.CSTYGROUP $ lcStyGroup ,  .T.)     "

&& To be sure that we select just Assorted style             
*-SAB 08-03-2012 [Start]
*lcInvJlExp = " IIF(!EMPTY(lcRpWareH)  , CWARECODE = PADR(lcRpWareH,6)                  ,  .T.) AND                   " +;
             " IIF(!EMPTY(ldAdjDat)   , BETWEEN(Dtrdate, ldLAdjDat, ldHAdjDat) ,  .T.) AND                   " +;
             " CTRTYPE = '1' AND ((ALLTRIM(Reference) = 'Style Assortment BreakDown'   AND CIRTYPE = 'I') OR " +;
             "                    (ALLTRIM(Reference) = 'Style Assortment Creation'    AND CIRTYPE = 'R'))   "

*-SAB Fix issue 9 and 10 [Start]
*lcInvJlExp = " IIF(!EMPTY(lcRpWareH)  , CWARECODE = PADR(lcRpWareH,6)                  ,  .T.) AND                   " +;
             " IIF(!EMPTY(ldAdjDat)   , BETWEEN(Dtrdate, ldLAdjDat, ldHAdjDat) ,  .T.) AND                   " +;
             " CTRTYPE = '1' AND ((ALLTRIM(Reference) = 'Style Assortment BreakDown'   AND CIRTYPE = 'I') OR " +;
             "                    (ALLTRIM(Reference) = 'Style Assortment Transfer'    AND CIRTYPE = 'R'))   "
lcInvJlExp = " IIF(!EMPTY(lcRpWareH)  , CWARECODE = PADR(lcRpWareH,6)                  ,  .T.) AND                   " +;
             " IIF(!EMPTY(ldAdjDat)   , BETWEEN(Dtrdate, ldLAdjDat, ldHAdjDat) ,  .T.) AND                   " +;
             " CTRTYPE = '1' AND ((ALLTRIM(Reference) = 'Style Assortment BreakDown'   AND CIRTYPE = 'I') OR " +;
             "                    (ALLTRIM(Reference) = 'Style Assortment Creation'    AND CIRTYPE = 'R'))   "
*-SAB Fix issue 9 and 10 [End]
*-SAB 08-03-2012 [End]
SELECT STYLE
SET RELATION TO 'S'+SCALE INTO SCALE             


*-SAB 08-03-2012 [Start]
*!*	Select STYDYE 
*!*	=gfSeek(PADR(lcRpWareH,6))             
*!*	SCAN REST WHILE cwarecode+style+dyelot = PADR(lcRpWareH,6) FOR IIF(lnStSel > 0, SEEK(STYDYE.STYLE,lcStyTmp),.T.)
*!*	  SELECT STYLE
*!*	  =gfSeek(STYDYE.STYLE)
*!*	  IF !EVALUATE(lcStyFlExp)
*!*	    LOOP 
*!*	  ENDIF 
*!*	  lnRecNo = RECNO()
*!*	  SELECT STYINVJL
*!*	  lcStyleCurr = STYLE.STYLE
*!*	  =gfSeek(lcStyleCurr)
*!*	  SCAN REST WHILE STYLE = lcStyleCurr For &lcInvJlExp
*!*	    
*!*	    =gfSeek(lcStyleCurr,'STYLE','STYLE')
*!*	    lnRecNum = RECNO()
*!*	    lcStyle    = STYLE
*!*	    lcSession  = cSession
*!*	    lcWareCode = cWareCode
*!*	    SCATTER MEMVAR MEMO
*!*	    m.Stymajor  = SUBSTR(lcStyle, 1, lnStyLen)
*!*	    m.Color     = SUBSTR(lcStyle, lnColrPos, lnColrLen)
*!*	    m.cReasDesc = gfCodDes(PADR(cadjreason,6),'CADJREASON')
*!*	    SELECT STYLE
*!*	    m.SCALE = STYLE.SCALE
*!*	    SELECT INVTADJ
*!*	    gfSeek(lcStyle)
*!*	    LOCATE REST WHILE Style = lcStyle FOR cSession = lcSession
*!*	    IF FOUND()
*!*	      WAIT WINDOW NOWAIT "Collecting Data for Style : "+lcStyle    
*!*	      STORE 0 TO m.TotOrg, m.TotRem
*!*	      FOR lnY = 1 TO 8
*!*	        lcY = ALLTRIM(STR(lnY))
*!*	        m.Org&lcY = OldQty&lcY
*!*	        m.TotOrg  = m.TotOrg  + m.Org&lcY
*!*	        m.Rem&lcY = m.Org&lcY + Adj&lcY
*!*	        m.TotRem  = m.TotRem  + m.Rem&lcY
*!*	      ENDFOR 
*!*	      
*!*	      SELECT(lcPriTemp)
*!*	      APPEND BLANK 
*!*	      m.Cirtype   = 'I' && To Put data on Temp file with a specific technique (Assorted = 'I', Childs = 'R')
*!*	      GATHER MEMVAR MEMO
*!*	      
*!*	      && to prevent adding wrong data while adding Breakdown styles
*!*	      FOR lnI = 1 TO 8
*!*	        lcI = ALLTRIM(STR(lnI))
*!*	        m.Rem&lcI = 0
*!*	        m.Org&lcI = 0
*!*	      ENDFOR 
*!*	        
*!*	      SELECT STYINVJL
*!*	      
*!*	      *-SAB 08-03-2012 [Start]
*!*	      *SCAN FOR CSESSION = lcSession AND ;
*!*	               IIF(ALLTRIM(reference) = 'Style Assortment Creation', CirType = 'I',CirType = 'R')
*!*	      SCAN FOR CSESSION = lcSession AND ;
*!*	               IIF(ALLTRIM(reference) = 'Style Assortment Transfer', CirType = 'I',CirType = 'R')
*!*	      *-SAB 08-03-2012 [End]
*!*	        SCATTER MEMVAR MEMO  
*!*	        m.Stymajor  = SUBSTR(STYLE, 1, lnStyLen)
*!*	        m.Color     = SUBSTR(STYLE, lnColrPos, lnColrLen)        
*!*	        m.cReasDesc = gfCodDes(PADR(cadjreason,6),'CADJREASON')
*!*	        
*!*	        SELECT STYLE 
*!*	        gfSeek(STYINVJL.STYLE)
*!*	        m.SCALE = STYLE.SCALE
*!*	        
*!*	        SELECT (lcPriTemp)
*!*	        APPEND BLANK 
*!*	        m.Cirtype   = 'R' && To Put data on Temp file with a specific technique (Assorted = 'I', Childs = 'R')
*!*	        GATHER MEMVAR MEMO
*!*	      ENDSCAN 
*!*	    ENDIF 
*!*	    IF BETWEEN(lnRecNum,1,RECCOUNT())
*!*	       GO RECORD lnRecNum 
*!*	    ENDIF 
*!*	  ENDSCAN 
*!*	*!*	  SELECT STYLE 
*!*	*!*	  GOTO lnRecNo
*!*	ENDSCAN

IF lnStSel > 0
  SELECT (lcStyTmp)
  SCAN
    Select STYDYE
    =gfSeek(PADR(lcRpWareH,6)+PADR(&lcStyTmp..STYLE, 19))
    SCAN REST WHILE cwarecode+style+dyelot = PADR(lcRpWareH,6)+PADR(&lcStyTmp..STYLE, 19)
      SELECT STYLE
      =gfSeek(STYDYE.STYLE)
      IF !EVALUATE(lcStyFlExp)
        LOOP 
      ENDIF 
      lnRecNo = RECNO()
      SELECT STYINVJL
      lcStyleCurr = STYLE.STYLE
      =gfSeek(lcStyleCurr)
      SCAN REST WHILE STYLE = lcStyleCurr For &lcInvJlExp
        
        =gfSeek(lcStyleCurr,'STYLE','STYLE')
        lnRecNum = RECNO()
        lcStyle    = STYLE
        lcSession  = cSession
        lcWareCode = cWareCode
        SCATTER MEMVAR MEMO
        m.Stymajor  = SUBSTR(lcStyle, 1, lnStyLen)
        m.Color     = SUBSTR(lcStyle, lnColrPos, lnColrLen)
        m.cReasDesc = gfCodDes(PADR(cadjreason,6),'CADJREASON')
        SELECT STYLE
        m.SCALE = STYLE.SCALE
        SELECT INVTADJ
        gfSeek(lcStyle)
        LOCATE REST WHILE Style = lcStyle FOR cSession = lcSession
        IF FOUND()
          WAIT WINDOW NOWAIT "Collecting Data for Style : "+lcStyle    
          STORE 0 TO m.TotOrg, m.TotRem
          FOR lnY = 1 TO 8
            lcY = ALLTRIM(STR(lnY))
            m.Org&lcY = OldQty&lcY
            m.TotOrg  = m.TotOrg  + m.Org&lcY
            m.Rem&lcY = m.Org&lcY + Adj&lcY
            m.TotRem  = m.TotRem  + m.Rem&lcY
          ENDFOR 
          
          SELECT(lcPriTemp)
          APPEND BLANK 
          m.Cirtype   = 'I' && To Put data on Temp file with a specific technique (Assorted = 'I', Childs = 'R')
          GATHER MEMVAR MEMO
          
          && to prevent adding wrong data while adding Breakdown styles
          FOR lnI = 1 TO 8
            lcI = ALLTRIM(STR(lnI))
            m.Rem&lcI = 0
            m.Org&lcI = 0
          ENDFOR 
            
          SELECT STYINVJL
                  
          *-SAB Fix issue 9 and 10 [Start]
          *SCAN FOR CSESSION = lcSession AND IIF(ALLTRIM(reference) = 'Style Assortment Transfer', CirType = 'I',CirType = 'R')        
          SCAN FOR CSESSION = lcSession AND IIF(ALLTRIM(reference) = 'Style Assortment Creation', CirType = 'I',CirType = 'R')
          *-SAB Fix issue 9 and 10 [End]
            SCATTER MEMVAR MEMO  
            m.Stymajor  = SUBSTR(STYLE, 1, lnStyLen)
            m.Color     = SUBSTR(STYLE, lnColrPos, lnColrLen)        
            m.cReasDesc = gfCodDes(PADR(cadjreason,6),'CADJREASON')
            
            SELECT STYLE 
            gfSeek(STYINVJL.STYLE)
            m.SCALE = STYLE.SCALE
            
            SELECT (lcPriTemp)
            APPEND BLANK 
            m.Cirtype   = 'R' && To Put data on Temp file with a specific technique (Assorted = 'I', Childs = 'R')
            GATHER MEMVAR MEMO
          ENDSCAN 
        ENDIF 
        IF BETWEEN(lnRecNum,1,RECCOUNT())
          GO RECORD lnRecNum 
        ENDIF 
      ENDSCAN 
    *!*	  SELECT STYLE 
    *!*	  GOTO lnRecNo
    ENDSCAN
  ENDSCAN
ELSE
  Select STYDYE
  =gfSeek(PADR(lcRpWareH,6))             
  SCAN REST WHILE cwarecode+style+dyelot = PADR(lcRpWareH,6) FOR IIF(lnStSel > 0, SEEK(STYDYE.STYLE,lcStyTmp),.T.)
    SELECT STYLE
    =gfSeek(STYDYE.STYLE)
    IF !EVALUATE(lcStyFlExp)
      LOOP 
    ENDIF 
    lnRecNo = RECNO()
    SELECT STYINVJL
    lcStyleCurr = STYLE.STYLE
    =gfSeek(lcStyleCurr)
    SCAN REST WHILE STYLE = lcStyleCurr For &lcInvJlExp
      
      =gfSeek(lcStyleCurr,'STYLE','STYLE')
      lnRecNum = RECNO()
      lcStyle    = STYLE
      lcSession  = cSession
      lcWareCode = cWareCode
      SCATTER MEMVAR MEMO
      m.Stymajor  = SUBSTR(lcStyle, 1, lnStyLen)
      m.Color     = SUBSTR(lcStyle, lnColrPos, lnColrLen)
      m.cReasDesc = gfCodDes(PADR(cadjreason,6),'CADJREASON')
      SELECT STYLE
      m.SCALE = STYLE.SCALE
      SELECT INVTADJ
      gfSeek(lcStyle)
      LOCATE REST WHILE Style = lcStyle FOR cSession = lcSession
      IF FOUND()
        WAIT WINDOW NOWAIT "Collecting Data for Style : "+lcStyle    
        STORE 0 TO m.TotOrg, m.TotRem
        FOR lnY = 1 TO 8
          lcY = ALLTRIM(STR(lnY))
          m.Org&lcY = OldQty&lcY
          m.TotOrg  = m.TotOrg  + m.Org&lcY
          m.Rem&lcY = m.Org&lcY + Adj&lcY
          m.TotRem  = m.TotRem  + m.Rem&lcY
        ENDFOR 
        
        SELECT(lcPriTemp)
        APPEND BLANK 
        m.Cirtype   = 'I' && To Put data on Temp file with a specific technique (Assorted = 'I', Childs = 'R')
        GATHER MEMVAR MEMO
        
        && to prevent adding wrong data while adding Breakdown styles
        FOR lnI = 1 TO 8
          lcI = ALLTRIM(STR(lnI))
          m.Rem&lcI = 0
          m.Org&lcI = 0
        ENDFOR 
          
        SELECT STYINVJL
        
        *-SAB Fix issue 9 and 10 [Start]
        *SCAN FOR CSESSION = lcSession AND IIF(ALLTRIM(reference) = 'Style Assortment Transfer', CirType = 'I',CirType = 'R')        
        SCAN FOR CSESSION = lcSession AND IIF(ALLTRIM(reference) = 'Style Assortment Creation', CirType = 'I',CirType = 'R')
        *-SAB Fix issue 9 and 10 [End]
          SCATTER MEMVAR MEMO  
          m.Stymajor  = SUBSTR(STYLE, 1, lnStyLen)
          m.Color     = SUBSTR(STYLE, lnColrPos, lnColrLen)        
          m.cReasDesc = gfCodDes(PADR(cadjreason,6),'CADJREASON')
          
          SELECT STYLE 
          gfSeek(STYINVJL.STYLE)
          m.SCALE = STYLE.SCALE
          
          SELECT (lcPriTemp)
          APPEND BLANK 
          m.Cirtype   = 'R' && To Put data on Temp file with a specific technique (Assorted = 'I', Childs = 'R')
          GATHER MEMVAR MEMO
        ENDSCAN 
      ENDIF 
      IF BETWEEN(lnRecNum,1,RECCOUNT())
         GO RECORD lnRecNum 
      ENDIF 
    ENDSCAN 
  *!*	  SELECT STYLE 
  *!*	  GOTO lnRecNo
  ENDSCAN
ENDIF
*-SAB 08-03-2012 [End]

lnRecCount = 0 
SELECT(lcPriTemp)
COUNT TO lnRecCount FOR !DELETED()
IF lnRecCount > 0
  SELECT(lcPriTemp)
  SET RELATION TO 'S'+SCALE INTO SCALE
  RETURN .T.
ELSE
  RETURN .F.
ENDIF 

* End of lfColData()

*************************************************************
*! Name      : lcCrtTemp
*! Developer : Hesham Elmasry
*! Date      : 08/11/2009
*! Purpose   : Creating Primary Temp File
*!*************************************************************
FUNCTION lcCrtTemp

*========= Primary Temp ==========
IF USED(lcPriTemp) AND RECCOUNT(lcPriTemp) > 0
  SELECT (lcPriTemp)
  USE IN (lcPriTemp)
ENDIF
*-- Create File
IF !USED(lcPriTemp)  
  SELECT STYINVJL
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+6,18]
  
  laFileStru[lnFileStru+1,1] = 'STYMAJOR'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = lnStyLen
  laFileStru[lnFileStru+1,4] = 0
    
  laFileStru[lnFileStru+2,1] = 'COLOR'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = lnColrLen
  laFileStru[lnFileStru+2,4] = 0   
  
  laFileStru[lnFileStru+3,1] = 'TotOrg'
  laFileStru[lnFileStru+3,2] = 'N'
  laFileStru[lnFileStru+3,3] = 7
  laFileStru[lnFileStru+3,4] = 0  
  
  laFileStru[lnFileStru+4,1] = 'TotRem'
  laFileStru[lnFileStru+4,2] = 'N'
  laFileStru[lnFileStru+4,3] = 7
  laFileStru[lnFileStru+4,4] = 0        
  
  laFileStru[lnFileStru+5,1] = 'cReasDesc'
  laFileStru[lnFileStru+5,2] = 'C'
  laFileStru[lnFileStru+5,3] = 30
  laFileStru[lnFileStru+5,4] = 0      
  
  laFileStru[lnFileStru+6,1] = 'SCALE'
  laFileStru[lnFileStru+6,2] = 'C'
  laFileStru[lnFileStru+6,3] = 3
  laFileStru[lnFileStru+6,4] = 0      
  
*!*	  lnFileStru = ALEN(laFileStru,1)
*!*	  DIMENSION laFileStru[lnFileStru+8,18]
*!*	  FOR lnx = 1 TO 8 
*!*	    lcX = ALLTRIM(STR(lnX))
*!*	    laFileStru[lnFileStru+lnx,1] = 'Sz'+lcX
*!*	    laFileStru[lnFileStru+lnx,2] = 'C'
*!*	    laFileStru[lnFileStru+lnx,3] = 5
*!*	    laFileStru[lnFileStru+lnx,4] = 0
*!*	  ENDFOR
    
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+8,18]
  FOR lnx = 1 TO 8 
    lcX = ALLTRIM(STR(lnX))
    laFileStru[lnFileStru+lnx,1] = 'Org'+lcX
    laFileStru[lnFileStru+lnx,2] = 'N'
    laFileStru[lnFileStru+lnx,3] = 7
    laFileStru[lnFileStru+lnx,4] = 0
  ENDFOR 
  
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+8,18]
  FOR lnx = 1 TO 8
    lcX = ALLTRIM(STR(lnX))
    laFileStru[lnFileStru+lnx,1] = 'Rem'+lcX
    laFileStru[lnFileStru+lnx,2] = 'N'
    laFileStru[lnFileStru+lnx,3] = 7
    laFileStru[lnFileStru+lnx,4] = 0
  ENDFOR
  
  FOR lnCount = 1 TO ALEN(laFileStru,1)
    STORE '' TO laFileStru[lnCount,7],laFileStru[lnCount,8],laFileStru[lnCount,9],;
      laFileStru[lnCount,10],laFileStru[lnCount,11],laFileStru[lnCount,12],;
      laFileStru[lnCount,13],laFileStru[lnCount,14],laFileStru[lnCount,15],;
      laFileStru[lnCount,16]
    STORE 0  TO laFileStru[lnCount,17],laFileStru[lnCount,18]
  ENDFOR  
  
  DIMENSION laIndex[1,2]
  laIndex[1,1] = 'CSESSION+CIRTYPE+STYLE'
  laIndex[1,2] = 'SESNTYP'
  
  =gfCrtTmp(lcPriTemp,@laFileStru,@laIndex)
ENDIF

* End of lcCrtTemp()





