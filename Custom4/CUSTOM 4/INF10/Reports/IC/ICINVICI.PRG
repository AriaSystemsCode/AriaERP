*:***************************************************************************
*: Program file  : ICINVICI.PRG
*: Program desc. : INVENTORY REPORT FOR INF10
*: For Report    : (ICINVICI.FRX)
*: System        : Aria Advantage Series.4XP
*: Module        : Inventory Control (IC)
*: Developer     : Mariam Mazhar (MMT) 23/08/2006
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfDispRe,lfSeTSRep,lfGetMem,lfRankFile
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ICINVICI
*:***************************************************************************
*: This Report Program is due to C120869 ...
*:***************************************************************************
*: Modifications :
*! B608234,1 WAM 08/23/2007 Fix syle filter problem
*:***************************************************************************
lcStTime = TIME()
lcMaj         = gfItemMask('PM')     && Get the major pict. of the style
lnMajSize     = LEN(lcMaj)           && Length of the major

loogScroll.cCROrientation = 'P'
STORE '' To m.Style , m.Desc ,m.SCALE
STORE 0 TO m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.TotQty,m.price,m.Amount

IF loOgScroll.llOGFltCh && OG Filters changed
  WAIT WINDOW "Collecting Data......." NOWAIT 
  IF USED(TMPINVFILE)
    USE IN (TMPINVFILE)
  ENDIF
  =lfCrtTmp()
  lfCollect()
ENDIF 

SELECT (TMPINVFILE)

*-- Calculate spent time in collecting data.
DO gfDispRe WITH EVAL('lcRpForm')
*-- end of report code.


*!*  SELECT INVLINE
*!*  SET RELATION TO INVOICE INTO INVHDR
*!*  SET RELATION TO STYLE INTO STYLE ADDI
*!*  SELECT STYLE
*!*  SET RELATION TO 'S' + SCALE INTO SCALE
*!*  SELECT INVLINE

*!*  *-- Scan all invoices in selected critria.
*!*  SCAN FOR &lcRpExp
*!*    =lfGetMem()  && Fill Memory variables with its corresponding values.

*!*    *-- if you find this style in temp. file
*!*    IF SEEK(INVLINE.STYLE,TMPINVFILE)

*!*      SELECT (TMPINVFILE)
*!*      REPLACE Amount    WITH Amount    + m.Amount ,;
*!*              Qty1      WITH QTY1      + m.QTY1,;
*!*              Qty2      WITH QTY2      + m.QTY2,;
*!*              Qty3      WITH QTY3      + m.QTY3,;
*!*              Qty4      WITH QTY4      + m.QTY4,;
*!*              Qty5      WITH QTY5      + m.QTY5,;
*!*              Qty6      WITH QTY6      + m.QTY6,;
*!*              Qty7      WITH QTY7      + m.QTY7,;
*!*              Qty8      WITH QTY8      + m.QTY8,;
*!*              TotQty    WITH TotQTY    + m.TotQTY,;
*!*              SCALE     WITH m.Scale  
*!*    ELSE  && Add new style record.
*!*      IF !EMPTY(m.Style)
*!*        INSERT INTO (TMPINVFILE) FROM MEMVAR
*!*      ENDIF
*!*    ENDIF
*!*    
*!*  ENDSCAN



*-- Start of function and procedure section.
*!*************************************************************
*! Name      : lfCrtTmp
*! Developer : Mariam Mazhar(MMT)
*! Date      : 08/23/2006
*! Purpose   : Create temporary cursor.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCrtTmp()
*!*************************************************************
*
FUNCTION lfCrtTmp

DIMENSION laTempStru[14,4]
laTempStru = ''

laTempStru [1,1] = 'STYLE'
laTempStru [1,2] = 'C'
laTempStru [1,3] = 19
laTempStru [1,4] = 0

laTempStru [2,1] = 'DESC'
laTempStru [2,2] = 'C'
laTempStru [2,3] = 20
laTempStru [2,4] = 0

laTempStru [3,1] = 'QTY1'
laTempStru [3,2] = 'N'
laTempStru [3,3] = 6
laTempStru [3,4] = 0

laTempStru [4,1] = 'QTY2'
laTempStru [4,2] = 'N'
laTempStru [4,3] = 6
laTempStru [4,4] = 0

laTempStru [5,1] = 'QTY3'
laTempStru [5,2] = 'N'
laTempStru [5,3] = 6
laTempStru [5,4] = 0

laTempStru [6,1] = 'QTY4'
laTempStru [6,2] = 'N'
laTempStru [6,3] = 6
laTempStru [6,4] = 0

laTempStru [7,1] = 'QTY5'
laTempStru [7,2] = 'N'
laTempStru [7,3] = 6
laTempStru [7,4] = 0

laTempStru [8,1] = 'QTY6'
laTempStru [8,2] = 'N'
laTempStru [8,3] = 6
laTempStru [8,4] = 0

laTempStru [9,1] = 'QTY7'
laTempStru [9,2] = 'N'
laTempStru [9,3] = 6
laTempStru [9,4] = 0

laTempStru [10,1] = 'QTY8'
laTempStru [10,2] = 'N'
laTempStru [10,3] = 6
laTempStru [10,4] = 0


laTempStru [11,1] = 'TotQTY'
laTempStru [11,2] = 'N'
laTempStru [11,3] = 6
laTempStru [11,4] = 0

laTempStru [12,1] = 'Price'
laTempStru [12,2] = 'N'
laTempStru [12,3] = 12
laTempStru [12,4] = 2

laTempStru [13,1] = 'Amount'
laTempStru [13,2] = 'N'
laTempStru [13,3] = 14
laTempStru [13,4] = 2

laTempStru [14,1] = 'SCALE'
laTempStru [14,2] = 'C'
laTempStru [14,3] = 3
laTempStru [14,4] = 0


 = gfCrtTmp(TMPINVFILE,@laTempStru ,"STYLE" ,TMPINVFILE,.T.)

*CREATE CURSOR (TMPINVFILE) (STYLE C(19),DESC C(20), QTY1 N(6), QTY2 N(6), QTY3 N(6);
                           , QTY4 N(6), QTY5 N(6), QTY6 N(6), QTY7 N(6), QTY8 N(6);
                           , TotQTY N(6), Price N(12,2), Amount N(14,2), SCALE C(3))   

*INDEX ON STYLE TAG (TMPINVFILE) OF (TMPINVFILE)

*-- end of lfColData.
*!*************************************************************
*! Name      : lfGetMem
*! Developer : Mariam Mazhar(MMT)
*! Date      : 08/23/2006
*! Purpose   : Fill RANK_FILE memory variables with its corresponding data.
*!*************************************************************
*! Called from : Program code,
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetMem()
*!*************************************************************
*
FUNCTION lfGetMem

SCATT MEMVAR MEMO
m.Desc = Style.Desc
m.price = STYLE.PRICEA
m.Amount = m.neqvamnt
*!**************************************************************************
*! Name      : lfNonMaj
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/25/2004
*! Purpose   : To get the style nonmajor segment structure
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : gfItemMask()
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfNonMaj()
*!**************************************************************************
*
FUNCTION lfNonMaj
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc  
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'  
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT  
    ELSE      
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.      
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]
RETURN ''
*-- End of lfNonMaj.

*!**************************************************************************
*! Name      : lfStySum
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/25/2004
*! Purpose   : sum a specific field for the current style in style file
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!**************************************************************************
*! Passed Parameters  : lcSty,lccomp,lnAddToVar
*!**************************************************************************
*! Returns            : Calculated field value.
*!**************************************************************************
*! Example   : =lfStySum()
*!**************************************************************************
* 
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) == ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  

  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- End of lfStySum.


*!**************************************************************************
*! Name      : lfGetScale
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/25/2004
*! Purpose   : get the Scale sizes
*!**************************************************************************
*! Called from : FRX
*!**************************************************************************
*! Example   : =lfGetScale()
*!**************************************************************************
*
FUNCTION lfGetScale
PARAMETERS lcScale
PRIVATE lcOldAlias,lnX,lcString,lcScale,lcZ
lcOldAlias = ALIAS()

loDBFScale.Seek('S'+lcScale)
SELECT SCALE
SEEK 'S'+lcScale
lnX      = 1
lcString = ''
IF FOUND() 
  DO WHILE lnX <= CNT
    lcZ = STR(lnX,1)
    lcString = lcString + PADL(ALLTRIM(SZ&lcZ),5,' ') + IIF(lnX=CNT,'','  ')
    lnX= lnX + 1
  ENDDO
ELSE
  lcString = '* * * E R R O R * * *'
ENDIF
IF LEN(TRIM(lcOldAlias)) > 0
  SELECT (lcOldAlias)
ENDIF
RETURN(lcString)
*-- End of lfGetScale.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mariam Mazhar (MMT)
*! Date      : 23/08/2006
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwOGWhen
loDBFStyle   = CreateObject("RemoteTable","Style","CStyle",'Style',SET("DATASESSION"))&&,"",.T.)
loDBFINVHDR  = CreateObject("RemoteTable",'INVHDR','INVHDR','INVHDR',SET("DATASESSION"))
loDBFINVLINE = CreateObject("RemoteTable",'INVLINE','INVLINE','INVLINE',SET("DATASESSION"))
loDBFScale   = CreateObject("RemoteTable",'Scale','Scale','Scale',SET("DATASESSION"))

*!*************************************************************
*! Name      : lfCollect
*! Developer : Mariam Mazhar (MMT)
*! Date      : 23/08/2006
*! Purpose   : function to collect data
*!*************************************************************
FUNCTION lfCollect

llUseDiv   = .F.
llUseGrp   = .F.
llUseStyle = .F.
llUseSea   = .F.

lcDivs = ''
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,'INVHDR.CDIVISION'),1)
IF lnPosition > 0
  lcDivs 	= LOOGSCROLL.laOGFxFlt[lnPosition,6]
  lcDivFile = loogscroll.gfTempName()
  llUseDiv  = IIF(LEN(lcDivs)>0,.T.,.F.) and lfConvertToCursor(lcDivs,'CDIVISION',lcDivFile)
ENDIF

*---Season Filter
lnSeaPos = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(loOGScroll.laOGFXFlt,'INVHDR.SEASON'),1)
IF lnSeaPos > 0
  lcSeaStr = LOOGSCROLL.laOGFXFlt[lnSeaPos,6]
  lcSeaFile = loOGScroll.gfTempName()
  llUseSea = IIF(LEN(lcSeaStr)>0,.T.,.F.) AND lfConvertToCursor(lcSeaStr,'SEASON',lcSeaFile)
ENDIF

*STYLE.CSTYGROUP
lnGrpPos = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(loOGScroll.laOGVRFlt,'STYLE.CSTYGROUP'),1)
IF lnGrpPos > 0
  lcGrpStr = LOOGSCROLL.laOGVRFlt[lnGrpPos,6]
  lcGrpFile = loOGScroll.gfTempName()
  llUseGrp = IIF(LEN(lcGrpStr)>0,.T.,.F.) AND lfConvertToCursor(lcGrpStr,'CSTYGRP',lcGrpFile)
ENDIF
*--Style
lcStylFile = ''
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(loOGScroll.laOGVRFlt,'STYLE.CSTYMAJOR'),1)
IF lnPosition > 0
  lcStylFile = LOOGSCROLL.laOGVRFlt[lnPosition,6]
  llUseStyle = IIF(!EMPTY(lcStylFile) .AND. USED(lcStylFile) .AND. RECCOUNT(lcStylFile)>0,.T.,.F.)
ENDIF
IF llUseStyle 
  SELECT(lcStylFile)
  LOCATE 
  IF EOF()
    llUseStyle = .F.
  ENDIF 
ENDIF 

ldCompDateS = {}
ldCompDateE = {}
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(LOOGSCROLL.laOGFXFlt,'INVHDR.INVDATE'),1)
IF lnPosition > 0 .AND. !EMPTY(LOOGSCROLL.laOGFXFlt[lnPosition,6])
 ldCompDateS = IIF(EMPTY(SUBSTR(laOGFXFlt[lnPosition,6],1,10)),DTOC(CTOD("")),SUBSTR(laOGFXFlt[lnPosition,6],1,10))
 ldCompDateE = IIF(EMPTY(SUBSTR(laOGFXFlt[lnPosition,6],12,21)),DTOC(CTOD("")),SUBSTR(laOGFXFlt[lnPosition,6],12,21))
ENDIF
*B608234,1 WAM 08/23/2007 Fix syle filter problem
loDBFINVLINE.SETORDER("INVLINES")
*B608234,1 WAM 08/23/2007 (End)

IF llUseStyle 
  SELECT(lcStylFile)
  SCAN 
    IF loDBFINVLINE.Seek(SUBSTR(&lcStylFile..cstymajor,1,lnMajSize),'INVLINES')
	  SELECT INVLINE
	    SCAN REST WHILE STYLE+INVOICE+STR(LINENO,6) = SUBSTR(&lcStylFile..cstymajor,1,lnMajSize) FOR ;
                      loDBFINVHDR.Seek(INVLINE.INVOICE,'INVHDR') AND INVHDR.STATUS <> 'V' AND loDBFStyle.seek(&lcStylFile..cstymajor,'CStyle') AND IIF(llUseDiv,SEEK(INVHDR.CDIVISION,lcDivFile),.T.);
                      AND IIF(llUseSea,SEEK(INVHDR.SEASON,lcSeaFile),.T.) AND IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.);
                      AND IIF(!EMPTY(ldCompDateS) AND !EMPTY(ldCompDateE),BETWEEN(INVHDR.INVDATE,CTOD(ldCompDateS),CTOD(ldCompDateE)),.T.)
        =lfGetMem()  && Fill Memory variables with its corresponding values
        IF SEEK(INVLINE.STYLE,TMPINVFILE)
          SELECT (TMPINVFILE)
          REPLACE Amount    WITH Amount    + m.Amount ,;
                  Qty1      WITH QTY1      + m.QTY1,;
                  Qty2      WITH QTY2      + m.QTY2,;
                  Qty3      WITH QTY3      + m.QTY3,;
                  Qty4      WITH QTY4      + m.QTY4,;
                  Qty5      WITH QTY5      + m.QTY5,;
                  Qty6      WITH QTY6      + m.QTY6,;
                  Qty7      WITH QTY7      + m.QTY7,;
                  Qty8      WITH QTY8      + m.QTY8,;
                  TotQty    WITH TotQTY    + m.TotQTY,;
                  SCALE     WITH m.Scale  
          ELSE  && Add new style record.
            IF !EMPTY(m.Style)
            INSERT INTO (TMPINVFILE) FROM MEMVAR
          ENDIF
        ENDIF
      ENDSCAN                 
    ENDIF 
  ENDSCAN
ELSE
  SELECT INVLINE
  SCAN FOR loDBFINVHDR.Seek(INVLINE.INVOICE,'INVHDR') AND INVHDR.STATUS <> 'V' ;
           AND loDBFStyle.seek(INVLINE.Style,'Style') AND IIF(llUseDiv,SEEK(INVHDR.CDIVISION,lcDivFile),.T.);
           AND IIF(llUseSea,SEEK(INVHDR.SEASON,lcSeaFile),.T.) AND IIF(llUseGrp,SEEK(STYLE.CSTYGROUP,lcGrpFile),.T.);
           AND IIF(!EMPTY(ldCompDateS) AND !EMPTY(ldCompDateE),BETWEEN(INVHDR.INVDATE,CTOD(ldCompDateS),CTOD(ldCompDateE)),.T.)
    =lfGetMem()  && Fill Memory variables with its corresponding values
    IF SEEK(INVLINE.STYLE,TMPINVFILE)
      SELECT (TMPINVFILE)
      REPLACE Amount    WITH Amount    + m.Amount ,;
              Qty1      WITH QTY1      + m.QTY1,;
              Qty2      WITH QTY2      + m.QTY2,;
              Qty3      WITH QTY3      + m.QTY3,;
              Qty4      WITH QTY4      + m.QTY4,;
              Qty5      WITH QTY5      + m.QTY5,;
              Qty6      WITH QTY6      + m.QTY6,;
              Qty7      WITH QTY7      + m.QTY7,;
              Qty8      WITH QTY8      + m.QTY8,;
              TotQty    WITH TotQTY    + m.TotQTY,;
              SCALE     WITH m.Scale  
     ELSE  && Add new style record.
       IF !EMPTY(m.Style)
         INSERT INTO (TMPINVFILE) FROM MEMVAR
       ENDIF
     ENDIF
  ENDSCAN          
ENDIF 
*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 08/23/2006
*! Purpose   : Convert a list of values into a cursor
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

ENDCASE 
 = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1 
  lnEnd=AT('|',lcValuesToConvert)
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
