*:***************************************************************************
*: Program file  : POSTYPCL
*: Program desc. : Print PO for Collage UK Ltd
*: For Report    : POSTYPCL.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : PO
*: Date          : 03/21/2002
*: Developer     : AHMED MAHER (AMH)
*: Reference     : C200307,1
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfGetAdr()
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*Modifications:
*B606040,1 KHM 05/29/2002 Print the PO note pad even if there is no style code
*B606040,1                notes.
*:***************************************************************************

SELECT PosHdr
SET SKIP TO
SELECT PosLn
SET SKIP TO
SET ORDER TO PosLn
SET RELATION OFF INTO STYLE
SET RELATION OFF INTO OBJLINK

*-- Creating the temporary file
IF USED(lcTmpPoLns)
  USE IN (lcTmpPoLns)
  USE (gcWorkDir+lcTmpPoLns) IN 0 ORDER TAG (lcTmpPoLns) EXCLUSIVE
  SELECT (lcTmpPoLns)
  ZAP
ELSE
  SELECT PosLn
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+42,4]

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
  laFileStru[lnFileStru,1] = 'cNotePad'
  laFileStru[lnFileStru,2] = 'M'
  laFileStru[lnFileStru,3] = 0
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'cCompos'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 60
  laFileStru[lnFileStru,4] = 0

  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'cCustName'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 30
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'cCustPo'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 15
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'cOrder'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'cWidth'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 6
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'cFabDesc'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 20
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'cFabPatt'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 10
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'nUntQty'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 7
  laFileStru[lnFileStru,4] = 3
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'cFabVend'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 30
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'nReq_Qty'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 12
  laFileStru[lnFileStru,4] = 3
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'nUsed_Qty'
  laFileStru[lnFileStru,2] = 'N'
  laFileStru[lnFileStru,3] = 12
  laFileStru[lnFileStru,4] = 3
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'cCatalog'
  laFileStru[lnFileStru,2] = 'M'
  laFileStru[lnFileStru,3] = 0
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'cColor'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 20
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'cDim1'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 5
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'dProduct'
  laFileStru[lnFileStru,2] = 'D'
  laFileStru[lnFileStru,3] = 8
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'cDesc'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 20
  laFileStru[lnFileStru,4] = 0
  
  lnFileStru = lnFileStru+1
  laFileStru[lnFileStru,1] = 'cPrice'
  laFileStru[lnFileStru,2] = 'C'
  laFileStru[lnFileStru,3] = 17
  laFileStru[lnFileStru,4] = 0
  
  =gfCrtTmp(lcTmpPolns,@laFileStru,'po+cwarecode+SUBSTR(style,1,16)+cDim1',lcTmpPolns)
ENDIF

*--- Opend needed files.
PRIVATE llOpenFab,llOpenCut,llOpenOrdH,llOpenOrdL,llOpenCtkt,llOpenCdNt,llOpenSCdN,llOpenBom
STORE .F. TO llOpenFab,llOpenCut,llOpenOrdH,llOpenOrdL,llOpenCtkt,llOpenCdNt,llOpenSCdN,llOpenBom
IF !USED('FABRIC')
  =gfOpenFile(gcDataDir+'FABRIC','FABRIC','SH')
  llOpenFab = .T.
ENDIF
IF !USED('CUTPICK')
  =gfOpenFile(gcDataDir+'CUTPICK','CUTPKORD','SH')
  llOpenCut = .T.
ENDIF
IF !USED('ORDHDR')
  =gfOpenFile(gcDataDir+'ORDHDR','ORDHDR','SH')
  llOpenOrdH = .T.
ENDIF
IF !USED('ORDLINE')
  =gfOpenFile(gcDataDir+'ORDLINE','ORDLINE','SH')
  llOpenOrdL = .T.
ENDIF
IF !USED('CTKTBOM')
  =gfOpenFile(gcDataDir+'CTKTBOM','CTKTBOM','SH')
  llOpenCtkt = .T.
ENDIF
IF !USED('STYLENOT')
  =gfOpenFile(gcDataDir+'STYLENOT','STYLENOT','SH')
  llOpenCdNt = .T.
ENDIF
IF !USED('STYCDNOT')
  =gfOpenFile(gcDataDir+'STYCDNOT','STYCDNOT','SH')
  llOpenSCdN = .T.
ENDIF
IF !USED('BOM')
  =gfOpenFile(gcDataDir+'BOM','BOM','SH')
  llOpenBom = .T.
ENDIF

DECLARE laSizes[16,2]
STORE " " TO lcLastSty,lcLastWar,lcLastAcc,lcLastStr
lnExtWidth = gfGetMemVar('M_EXTWIDTH')
STORE 0 TO lnCost1
SELECT PosHdr
SCAN FOR &lcRpExp
  IF SEEK(cStyType+Po,'PosLn')
    SELECT PosLn
    STORE SPACE(3) TO lcCatalog,lcClrCd
    SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = ;
                    PosHdr.cStyType+PosHdr.PO FOR TranCd = '1' AND TotQty <>0
      IF SEEK(PO+CWARECODE+SUBSTR(STYLE,1,16)+SCALE.CDIM1,lcTmpPolns)
        SELECT (lcTmpPolns)
        lnScalePos = ASCAN(laSizes,POSLN.SCALE)
        lnScalePos = IIF(lnScalePos<>0,ASUBSCRIPT(laSizes,lnScalePos,1),0)
        IF lnScalePos <> 0
          FOR lnI = lnScalePos TO MIN(lnScalePos+SCALE.CNT-1,16)
            lcI = ALLTRIM(STR(lnI,2))
            lcJ = STR(lnI-lnScalePos+1,1)
            REPLACE QTY&lcI. WITH POSLN.QTY&lcJ.
            REPLACE TOTQTY   WITH TOTQTY+POSLN.QTY&lcJ.
          ENDFOR
        ENDIF
      ELSE
        SCATTER MEMVAR MEMO
        =lfGetSizes()
        INSERT INTO (lcTmpPolns) FROM MEMVAR
        SELECT (lcTmpPolns)
        FOR lnI = 1 TO 8
          lcI = STR(lnI,1)
          REPLACE QTY&lcI. WITH 0
        ENDFOR
        REPLACE TOTQTY WITH 0
        lnScalePos = ASCAN(laSizes,POSLN.SCALE)
        lnScalePos = IIF(lnScalePos<>0,ASUBSCRIPT(laSizes,lnScalePos,1),0)
        IF lnScalePos <> 0
          FOR lnI = lnScalePos TO MIN(lnScalePos+SCALE.CNT-1,16)
            lcI = ALLTRIM(STR(lnI,2))
            lcJ = STR(lnI-lnScalePos+1,1)
            REPLACE QTY&lcI. WITH POSLN.QTY&lcJ.
            REPLACE TOTQTY   WITH TOTQTY+POSLN.QTY&lcJ.
          ENDFOR
        ENDIF
        FOR lnI = 1 TO 16
          lcI = ALLTRIM(STR(lnI,2))
          REPLACE SZ&lcI. WITH laSizes[lnI,1]
        ENDFOR
        IF SUBSTR(STYLE,14,3) # lcClrCd
          =SEEK('2'+PO+STR(LINENO,6),'CUTPICK')
          =SEEK('O'+CUTPICK.ORDER+CUTPICK.CORDLINE,'ORDLINE')
          lcClrCd = SUBSTR(STYLE,14,3)
          lcCatalog = ORDLINE.NOTE_MEM
        ENDIF
        REPLACE cDim1     WITH SCALE.CDIM1,;
                cCatalog  WITH lcCatalog,;
                cColor    WITH gfCodDes(SUBSTR(STYLE,14,3), 'COLOR')
        SELECT POSLN
      ENDIF
      lcLastSty = Style
      lcLastWar = cWareCode
      lcLastAcc = Account
      lcLastStr = Store
      lnCost1   = NCOST1
    ENDSCAN
    SELECT (lcTmpPolns)
    APPEND BLANK
    REPLACE cStyType  WITH PosHdr.cStyType      ,;
            Po        WITH PosHdr.PO            ,;
            Style     WITH SUBSTR(lcLastSty,1,lnMajSize)+"-ZZZZZ*",;
            cWareCode WITH lcLastWar,;
            Account   WITH lcLastAcc,;
            Store     WITH lcLastStr,;
            nCost1    WITH lnCost1,;
            Lineno    WITH 999999
    IF SEEK(PADR(SUBSTR(lcLastSty,1,lnMajSize),19),'STYCDNOT')
      SELECT STYCDNOT
      lnNoteCnt = 0
      SCAN REST WHILE CSTYMAJOR+CODE = PADR(SUBSTR(lcLastSty,1,lnMajSize),19)
        =SEEK(CODE,'STYLENOT')
        lnNoteCnt = lnNoteCnt + 1
        SELECT (lcTmpPolns)
        REPLACE cNotePad WITH IIF(lnNoteCnt=1,'',cNotePad+CHR(13)) + ALLTRIM(STR(lnNoteCnt));
                              + '. ' + STYLENOT.CNOTE
      ENDSCAN
    *B606040,1 KHM 05/29/2002 (Begin) Printing the PO note pad even if there
    *B606040,1                   no style coded notes.
    ENDIF
    *B606040,1 KHM 05/29/2002 (End)
      SELECT (lcTmpPolns)
      IF llRpPrtPn .AND. SEEK('P'+POSHDR.PO,'NOTEPAD')
        *B606040,1 KHM 05/29/2002 (Begin
        *REPLACE cNotePad WITH cNotePad+CHR(13)+CHR(13)+CHR(13)
        REPLACE cNotePad WITH cNotePad+CHR(13)+;
                IIF(SEEK(PADR(SUBSTR(lcLastSty,1,lnMajSize),19),'STYCDNOT'),;
                CHR(13)+CHR(13),'')
        *B606040,1 KHM 05/29/2002 (End)        
        
        REPLACE cNotePad WITH cNotePad+NotePad.mNotes
      ENDIF
    *B606040,1 KHM 05/29/2002 (Begin) Commented out
    *ENDIF
    *B606040,1 KHM 05/29/2002 (End)
    SELECT (lcTmpPolns)
    =lfReplace()
  ENDIF
ENDSCAN

IF llOpenFab
  USE IN FABRIC
ENDIF
IF llOpenCut
  USE IN CUTPICK
ENDIF
IF llOpenOrdH
  USE IN ORDHDR
ENDIF
IF llOpenOrdL
  USE IN ORDLINE
ENDIF
IF llOpenCtkt
  USE IN CTKTBOM
ENDIF
IF llOpenCdNt
  USE IN STYLENOT
ENDIF
IF llOpenSCdN
  USE IN STYCDNOT
ENDIF
IF llOpenBom
  USE IN BOM
ENDIF

SELECT PosLn
SET RELATION TO
SELECT PosHdr
SET RELATION TO

SELECT (lcTmpPolns)
GOTO TOP
SET RELATION TO cStyType+PO INTO PosHdr ADDITIVE
SET RELATION TO STYLE INTO STYLE ADDITIVE
SET RELATION TO 'S'+SUBSTR(style,1,lnMajSize) INTO Objlink ADDITIVE

SELECT PosHdr
SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE
SET RELATION TO 'P'+POSHDR.PO INTO &TMPObjlink ADDITIVE

SELECT (lcTmpPolns)
lcRpExp = ".T."

*-- End Of Option Program.

*!*************************************************************
*! Name      : lfGetSizes
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/21/2002
*! Purpose   : Get the sizes of the current style
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetSizes()
*!*************************************************************
*
FUNCTION lfGetSizes

PRIVATE lnSizeCnt, lnAlias, lnRecNo, lcDim1
lnSizeCnt = 0
laSizes = SPACE(0)
lnAlias = SELECT(0)
SELECT SCALE
lnRecNo = RECNO()
IF SEEK('S'+POSLN.SCALE)
  lcDim1 = CDIM1
  =SEEK('S'+SUBSTR(POSLN.SCALE,1,lnExtWidth))
  LOCATE REST WHILE Type+Scale+Prepak = 'S'+SUBSTR(POSLN.SCALE,1,lnExtWidth) FOR CDIM1 = lcDim1
  SCAN REST WHILE Type+Scale+Prepak = 'S'+SUBSTR(POSLN.SCALE,1,lnExtWidth) FOR CDIM1 = lcDim1
    FOR lnI = lnSizeCnt+1 TO MIN(lnSizeCnt+CNT,16)
      lcI = STR(lnI-lnSizeCnt,1)
      laSizes[lnI,1] = EVALUATE('SZ'+lcI)
      laSizes[lnI,2] = SCALE
    ENDFOR
    IF lnSizeCnt+CNT >= 16
      EXIT
    ENDIF
    lnSizeCnt = MIN(lnSizeCnt+CNT,16)
  ENDSCAN
ENDIF
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF
SELECT (lnAlias)
*-- end of lfGetSizes.

*!*************************************************************
*! Name      : lfReplace
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/21/2002
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

PRIVATE lnUntQty,lnReq_Qty,lnUsed_Qty,lcOrder,ldProduct
STORE 0 TO lnUntQty,lnReq_Qty,lnUsed_Qty
STORE {} TO ldProduct
=SEEK(POSHDR.PO)
=SEEK(SUBSTR(STYLE,1,lnMajSize),'STYLE')
=SEEK(STYLE.FABRIC,'FABRIC')
=SEEK(FABRIC.VENDOR,'APVENDOR')
=SEEK('2'+PO,'CUTPICK')
=SEEK('O'+CUTPICK.ORDER,'ORDHDR')
=SEEK('M'+ORDHDR.ACCOUNT,'CUSTOMER')
=SEEK('O'+CUTPICK.ORDER+CUTPICK.CORDLINE,'ORDLINE')
IF POSHDR.STATUS = 'H'
  IF SEEK(SUBSTR(STYLE,1,lnMajSize),'BOM')
    SELECT BOM
    LOCATE REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = SUBSTR(STYLE,1,lnMajSize);
                FOR ITEM = STYLE.FABRIC .AND. CCATGTYP = 'F'
    IF FOUND()
      lnUntQty = BOM.NBOMTOTQTY
      lnReq_Qty = lnUntQty * POSHDR.OPEN
    ENDIF
    SELECT (lcTmpPolns)
  ENDIF
ELSE
  SELECT CTKTBOM
  lcOrder = SET('ORDER')
  SET ORDER TO CTKTYP
  IF SEEK('I'+PO+STYLE.FABRIC)
    lnUntQty = UNTQTY
    ldProduct = DATE
    SUM REST WHILE cimtyp+cuttkt+item+iclr+mfgcode+dyelot = 'I'+PO+STYLE.FABRIC;
        REQ_QTY,USED_QTY TO lnReq_Qty,lnUsed_Qty
  ENDIF
  SET ORDER TO &lcOrder.
  SELECT (lcTmpPolns)
ENDIF
REPLACE REST WHILE po+cwarecode+SUBSTR(style,1,16)+cDim1 = POSHDR.PO;
        cCompos   WITH FABRIC.CONTENT,;
        cCustName WITH CUSTOMER.BTNAME,;
        cCustPo   WITH ORDHDR.CUSTPO,;
        cOrder    WITH CUTPICK.ORDER,;
        cWidth    WITH FABRIC.WIDTH,;
        cFabDesc  WITH FABRIC.DESC,;
        cFabPatt  WITH FABRIC.PATTERN,;
        nUntQty   WITH lnUntQty,;
        cFabVend  WITH APVENDOR.CVENCOMP,;
        nReq_Qty  WITH lnReq_Qty,;
        nUsed_Qty WITH lnUsed_Qty,;
        dProduct  WITH ldProduct,;
        cDesc     WITH STYLE.DESC,;
        cPrice    WITH ALLTRIM(STR(nCost1,13,3)) + ' ' + Poshdr.cpricecur
*-- end of lfReplace.