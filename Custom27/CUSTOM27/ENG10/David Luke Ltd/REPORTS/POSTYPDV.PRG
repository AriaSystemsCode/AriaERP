*:***************************************************************************
*: Program file  : POSTYPDV
*: Program desc. : Print PO for David Luke
*: For Report    : POSTYPDV.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : PO
*: Date          : 02/10/2003
*: Developer     : AHMED MAHER (AMH)
*: Reference     : C200478,1
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfGetAdr()
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************

*lcEmail    = gfGetMemVar('M_CEMAIL')   && Var. to hold the company email
*lcWebsite  = gfGetMemVar('M_CWEBSITE') && Var. to hold the company website
lnExtWidth = gfGetMemVar('M_EXTWIDTH') && Var. to hold the scale width
*lcScalePos = gfTempName()              && Var. to hold the scale postion map.

SELECT PosHdr
SET SKIP TO
SELECT PosLn
SET SKIP TO
SET ORDER TO PosLn
SET RELATION OFF INTO STYLE
SET RELATION OFF INTO OBJLINK

PRIVATE lcRltcolor,lnColorStr,lnColorLen,lnSizeStr
lcRltColor = SPACE(0)
STORE 0 TO lnColorStr,lnColorLen,lnSizeStr

DECLARE laLngDesc[1,2]
laLngDesc[1,1] = 'CLRLNAME'
laLngDesc[1,2] = 'lcRltColor'

*-- Get Color Segment.
=lfGetColor()

*-- Creating the temporary file

DIMENSION laTags[2,2]
laTags[1,1] = 'po+cwarecode+style+cpos'
laTags[2,1] = 'po+cwarecode+cstygroup+style+cpos'

laTags[1,2] = 'TmpTag1'
laTags[2,2] = 'TmpTag2'

IF USED(lcTmpPoLns)
  USE IN (lcTmpPoLns)
  USE (gcWorkDir+lcTmpPoLns) IN 0 EXCLUSIVE
  SELECT (lcTmpPoLns)
  ZAP
ELSE
  SELECT PosLn
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+49,4]

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'QTY9'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'QTY10'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'QTY11'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'QTY12'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'QTY13'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'QTY14'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'QTY15'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'QTY16'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ1'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ2'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ3'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ4'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ5'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ6'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ7'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ8'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ9'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ10'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ11'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ12'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ13'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ14'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ15'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'SZ16'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'NPRICE1'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 13
  laFileStru[lnFileStru,4] = 3

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'NPRICE2'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 13
  laFileStru[lnFileStru,4] = 3

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'NPRICE3'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 13
  laFileStru[lnFileStru,4] = 3

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'NPRICE4'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 13
  laFileStru[lnFileStru,4] = 3

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'NPRICE5'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 13
  laFileStru[lnFileStru,4] = 3

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'NPRICE6'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 13
  laFileStru[lnFileStru,4] = 3

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'NPRICE7'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 13
  laFileStru[lnFileStru,4] = 3

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'NPRICE8'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 13
  laFileStru[lnFileStru,4] = 3

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'NPRICE9'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 13
  laFileStru[lnFileStru,4] = 3

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'NPRICE10'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 13
  laFileStru[lnFileStru,4] = 3

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'NPRICE11'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 13
  laFileStru[lnFileStru,4] = 3

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'NPRICE12'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 13
  laFileStru[lnFileStru,4] = 3

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'NPRICE13'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 13
  laFileStru[lnFileStru,4] = 3

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'NPRICE14'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 13
  laFileStru[lnFileStru,4] = 3

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'NPRICE15'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 13
  laFileStru[lnFileStru,4] = 3

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'NPRICE16'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 13
  laFileStru[lnFileStru,4] = 3

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'cNotePad'
  laFileStru[lnFileStru,2] = 'M'
  laFileStru[lnFileStru,3] = 0
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'cColor'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 20
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'cPurCode'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 20
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'cFabDesc'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 20
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'cFabClr'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 20
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'cFabWeight'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 15
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'cFabPatt'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 10
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'cStyGroup'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'cPos'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0
  
  =gfCrtTmp(lcTmpPolns,@laFileStru,@laTags)
ENDIF

SELECT (lcTmpPolns)
IF llRpSrtGrp
  SET ORDER TO TAG TmpTag2
ELSE
  SET ORDER TO TAG TmpTag1
ENDIF

*-- Create the scale map.
*=lfMapScale()

*--- Opend needed files.
PRIVATE llOpenFab,llOpenBom
STORE .F. TO llOpenFab,llOpenBomL,llOpenBom
IF !USED('FABRIC')
  =gfOpenFile(gcDataDir+'FABRIC','FABRIC','SH')
  llOpenFab = .T.
ENDIF
IF !USED('BOMLINE')
  =gfOpenFile(gcDataDir+'BOMLINE','BOMLINE','SH')
  llOpenBomL = .T.
ENDIF
IF !USED('BOM')
  =gfOpenFile(gcDataDir+'BOM','BOM','SH')
  llOpenBom = .T.
ENDIF

STORE " " TO lcLastSty,lcLastWar,lcLastAcc,lcLastStr
SELECT PosHdr
SCAN FOR &lcRpExp
  IF SEEK(cStyType+Po,'PosLn')
    SELECT PosLn
    lcStyle = SPACE(19)
    lnPos = 0
    lnCnt = 0
    SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = ;
                    PosHdr.cStyType+PosHdr.PO FOR TranCd = '1' AND TotQty <>0
      *IF SEEK(SCALE,lcScalePos)
      *  SELECT (lcScalePos)
      *  SCAN REST WHILE SCALE = POSLN.SCALE
      *    lnSt  = ST
      *    lnEnd = END
      *    lnCnt = FROM - 1
      *    =SEEK('S'+SCALE,'SCALE')
      *    =SEEK(POSLN.STYLE,'STYLE')
      *    IF !SEEK(POSLN.PO+POSLN.CWARECODE+IIF(llRpSrtGrp,STYLE.CSTYGROUP,'')+;
      *            STUFF(POSLN.STYLE,lnSizeStr,3,'***')+POS,lcTmpPolns)
      *      SELECT POSLN
      *      SCATTER MEMVAR MEMO
      *      STORE 0 TO M.QTY1,M.QTY2,M.QTY3,M.QTY4,M.QTY5,M.QTY6,M.QTY7,M.QTY8
      *      m.Style = STUFF(m.Style,lnSizeStr,3,'***')
      *      INSERT INTO (lcTmpPolns) FROM MEMVAR
      *      SELECT (lcTmpPolns)
      *      REPLACE CPOS      WITH EVALUATE(lcScalePos+'.POS'),;
      *              CSTYGROUP WITH STYLE.CSTYGROUP
      *      =lfReplace()
      *    ENDIF
      *    SELECT (lcTmpPolns)
      *    FOR lnI = lnSt TO lnEnd
      *      lcI = ALLTRIM(STR(lnI,2,0))
      *      lnCnt = lnCnt + 1
      *      lcCnt = STR(lnCnt,1)
      *      REPLACE ('SZ'    +lcI) WITH EVALUATE('SCALE.SZ' +lcCnt),;
      *              ('QTY'   +lcI) WITH EVALUATE('POSLN.QTY'+lcCnt),;
      *              ('NPRICE'+lcI) WITH IIF(lcRpPrice='C',IIF(llrpfc,Posln.ncost1,Posln.NeCost1),;
      *                                  IIF(lcRpPrice='P',ROUND(IIF(lcRpForm='N' .AND. llrpPdic ,;
      *                                     (1-lnDisc/100),1)*Style.nSugRetPri,2),0))
      *    ENDFOR
      *    lcLastSty = STUFF(POSLN.Style,lnSizeStr,3,'***')
      *    lcLastWar = POSLN.cWareCode
      *    lcLastAcc = POSLN.Account
      *    lcLastStr = POSLN.Store
      *  ENDSCAN
      *ENDIF
      IF STUFF(STYLE,lnSizeStr,3,'***') # lcStyle
        lcStyle = STUFF(STYLE,lnSizeStr,3,'***')
        lnPos = 0
        lnCnt = 0
      ENDIF
      =SEEK('S'+SCALE,'SCALE')
      =SEEK(STYLE,'STYLE')
      IF !SEEK(PO+CWARECODE+IIF(llRpSrtGrp,STYLE.CSTYGROUP,'')+;
                  STUFF(STYLE,lnSizeStr,3,'***')+STR(lnPos,5,0),lcTmpPolns)
        =lfAddNew()
      ENDIF
      SELECT (lcTmpPolns)
      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        IF EVALUATE('POSLN.QTY'+lcI) = 0
          LOOP
        ENDIF
        lnCnt = lnCnt + 1
        lcCnt = ALLTRIM(STR(lnCnt,2,0))
        IF lnCnt = 17
          SELECT POSLN
          lnPos = lnPos + 1
          lnCnt = 1
          lcCnt = '1'
          =lfAddNew()
          SELECT (lcTmpPolns)
        ENDIF
        REPLACE ('SZ'    +lcCnt) WITH EVALUATE('SCALE.SZ' +lcI),;
                ('QTY'   +lcCnt) WITH EVALUATE('POSLN.QTY'+lcI),;
                ('NPRICE'+lcCnt) WITH IIF(lcRpPrice='C',IIF(llrpfc,Posln.ncost1,Posln.NeCost1),;
                                      IIF(lcRpPrice='P',ROUND(IIF(lcRpForm='N' .AND. llrpPdic ,;
                                          (1-lnDisc/100),1)*Style.nSugRetPri,2),0))
      ENDFOR
      IF lnCnt = 16
        lnPos = lnPos + 1
        lnCnt = 0
      ENDIF
      lcLastSty = STUFF(POSLN.Style,lnSizeStr,3,'***')
      lcLastWar = POSLN.cWareCode
      lcLastAcc = POSLN.Account
      lcLastStr = POSLN.Store
    ENDSCAN
    SELECT (lcTmpPolns)
    APPEND BLANK
    REPLACE cStyType  WITH PosHdr.cStyType      ,;
            Po        WITH PosHdr.PO            ,;
            Style     WITH SUBSTR(lcLastSty,1,lnMajSize)+"-ZZZZZ*",;
            cStyGroup WITH 'ZZZZZZ',;
            cPos      WITH 'ZZZZZ',;
            cWareCode WITH lcLastWar,;
            Account   WITH lcLastAcc,;
            Store     WITH lcLastStr,;
            Lineno    WITH 999999
    SELECT (lcTmpPolns)
    IF llRpPrtPn .AND. SEEK('P'+POSHDR.PO,'NOTEPAD')
      REPLACE cNotePad WITH NotePad.mNotes
    ENDIF
  ENDIF
ENDSCAN

IF llOpenFab
  USE IN FABRIC
ENDIF
IF llOpenBomL
  USE IN BOMLINE
ENDIF
IF llOpenBom
  USE IN BOM
ENDIF
*USE IN (lcScalePos)

SELECT PosLn
SET RELATION TO
SELECT PosHdr
SET RELATION TO

SELECT (lcTmpPolns)
GOTO TOP
SET RELATION TO cStyType+PO INTO PosHdr ADDITIVE
SET RELATION TO SUBSTR(STYLE,1,lnMajSize) INTO STYLE ADDITIVE
SET RELATION TO 'S'+SUBSTR(style,1,lnMajSize) INTO Objlink ADDITIVE

SELECT PosHdr
SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE
SET RELATION TO 'P'+POSHDR.PO INTO &TMPObjlink ADDITIVE

SELECT (lcTmpPolns)
lcRpExp = ".T."

*-- End Of Option Program.

*!*************************************************************
*! Name      : lfReplace
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/10/2003
*! Purpose   : Fill spicial handling fields
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfReplace()
*!*************************************************************
*
FUNCTION lfReplace

=gfRltFld(SUBSTR(Style,lnColorStr,lnColorLen) , @laLngDesc , 'COLOR     ')
REPLACE cColor WITH lcRltColor

IF !EMPTY(STYLE.FABRIC) .AND. SEEK(STYLE.FABRIC,'FABRIC') .AND. FABRIC.CPURCODE = '000271'
  IF SEEK('I1'+PO+STR(LINENO,6),'BOMLINE')
    SELECT BOMLINE
    LOCATE REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
                      'I1'+EVALUATE(lcTmpPolns+'.PO+STR('+lcTmpPolns+'.LINENO,6)');
                  FOR STYLE = POSLN.STYLE .AND. ITEM  = FABRIC.FABRIC
    IF FOUND()
      =gfRltFld(ICLR , @laLngDesc , 'COLOR     ')
    ELSE
      STORE SPACE(0) TO lcRltColor
    ENDIF
  ELSE
    IF SEEK(STYLE.CSTYMAJOR,'BOM')
      SELECT BOM
      LOCATE REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = STYLE.CSTYMAJOR;
                    FOR ITEM = FABRIC.FABRIC .AND. LIKE(STRTRAN(CITMMASK,'*','?'),STYLE.STYLE)
      IF FOUND()
        =gfRltFld(IIF(ICLR=REPLICATE('*',lnColorLen),SUBSTR(STYLE.STYLE,lnColorStr,lnColorLen),;
                      ICLR) , @laLngDesc , 'COLOR     ')
      ELSE
        STORE SPACE(0) TO lcRltColor
      ENDIF
    ELSE
      STORE SPACE(0) TO lcRltcolor
    ENDIF
  ENDIF
  SELECT (lcTmpPolns)
  REPLACE cPurCode   WITH gfCodDes(FABRIC.CPURCODE, 'CPURCODE'),;
          cFabDesc   WITH FABRIC.DESC,;
          cFabClr    WITH lcRltColor,;
          cFabWeight WITH FABRIC.CFABWEIGHT,;
          cFabPatt   WITH FABRIC.PATTERN
ENDIF
*-- end of lfReplace.

*!*************************************************************
*! Name      : lfMapScale
*! Developer : AHMED MAHER (AMH)
*! Date      : 02/10/2003
*! Purpose   : Map the scale postion
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfMapScale()
*!*************************************************************
*
FUNCTION lfMapScale

PRIVATE lcCurScale,lnPosCnt,lnLstSz,lnScaleCnt
CREATE CURSOR (lcScalePos) (SCALE C(3),POS C(5),ST N(2,0),END N(2,0),FROM N(1,0))
INDEX ON SCALE TAG (lcScalePos) OF (lcScalePos)

lcCurScale = SPACE(lnExtWidth)
STORE 0 TO lnPosCnt,lnLstSz,lnScaleCnt

SELECT SCALE
=SEEK('S')
SCAN REST WHILE TYPE+SCALE+PREPAK = 'S'
  IF SUBSTR(SCALE,1,lnExtWidth) # lcCurScale
    lcCurScale = SUBSTR(SCALE,1,lnExtWidth)
    STORE 0 TO lnPosCnt,lnLstSz
  ENDIF
  lnScaleCnt = CNT
  IF lnLstSz+CNT > 16
    INSERT INTO (lcScalePos) (SCALE,POS,ST,END,FROM);
                      VALUES (SCALE.SCALE,SUBSTR(SCALE.SCALE,1,lnExtWidth)+;
                              STRTRAN(STR(lnPosCnt,5-lnExtWidth,0),' ','0'),;
                              lnLstSz+1,16,1)
    lnPosCnt   = lnPosCnt + 1
    lnScaleCnt = lnLstSz+CNT-16
    lnLstSz    = 0
  ENDIF
  INSERT INTO (lcScalePos) (SCALE,POS,ST,END,FROM);
                    VALUES (SCALE.SCALE,SUBSTR(SCALE.SCALE,1,lnExtWidth)+;
                            STRTRAN(STR(lnPosCnt,5-lnExtWidth,0),' ','0'),;
                            lnLstSz+1,lnLstSz+lnScaleCnt,1+SCALE.CNT-lnScaleCnt)
  lnLstSz = lnLstSz + lnScaleCnt
  IF lnLstSz = 16
    lnPosCnt = lnPosCnt + 1
    lnLstSz  = 0
  ENDIF
ENDSCAN
*-- end of lfMapScale.

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Ahmed Maher
*! Date      : 02/17/2003
*! Purpose   : Get the color information.
*!*************************************************************
*! Calls     : gfItemMask
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfGetColor()
*!*************************************************************
*
FUNCTION lfGetColor

*-- Array hold the segmants specifications of the style code structure.
DECLARE laStySeg[1,1]

*-- Count of the major part.
lnMjorCnt  = gfItemMask("SM")
  
*-- Fill an array with the segments strucure, & loop in it to 
*-- know if there a color segment in the style code strucure.
=gfItemMask(@laStySeg)
FOR lnCnt = lnMjorCnt + 1 TO ALEN(laStySeg,1)
  IF laStySeg[lnCnt , 1] = "C"
    *-- Flag to know if there is color in the style code strucure.
    llColorExt = .T.
    *-- Var. hold the start position of the color segment in the style code strucure.
    lnColorStr = laStySeg[lnCnt , 4]
    *-- Var. hold the color segment lenght in the style code strucure.
    lnColorLen = LEN(laStySeg[lnCnt , 3])
  ENDIF
  IF laStySeg[lnCnt , 1] = "S"
    lnSizeStr = laStySeg[lnCnt , 4]
  ENDIF
ENDFOR

*!*************************************************************
*! Name      : lfAddNew
*! Developer : Ahmed Maher
*! Date      : 02/17/2003
*! Purpose   : Add new record in the temp file
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfAddNew()
*!*************************************************************
*
FUNCTION lfAddNew

SCATTER MEMVAR MEMO
STORE 0 TO M.QTY1,M.QTY2,M.QTY3,M.QTY4,M.QTY5,M.QTY6,M.QTY7,M.QTY8
m.Style = STUFF(m.Style,lnSizeStr,3,'***')
INSERT INTO (lcTmpPolns) FROM MEMVAR
SELECT (lcTmpPolns)
REPLACE CPOS      WITH STR(lnPos,5,0),;
        CSTYGROUP WITH STYLE.CSTYGROUP
=lfReplace()