*:***************************************************************************
*: Program file  : POSTYPEF
*: Program desc. : Print PO for Elien Fisher
*: For Report    : POSTYPEF.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : PO
*: Developer     : AHMED MAHER (AMH)
*: Reference     : C101937,1
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfGetAdr()
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*:Modifications:
*:***************************************************************************

SELECT PosHdr
SET SKIP TO
SELECT PosLn
SET SKIP TO
SET ORDER TO PosLn

*-- Creating the temporary file
lcTmpPolns = gfTempName()
SELECT PosLn
=AFIELDS(laFStru)
DIMENSION laFStru[ALEN(laFStru,1)+1,4]
laFStru[ALEN(laFStru,1),1] = 'cNotePad'
laFStru[ALEN(laFStru,1),2] = 'M'
laFStru[ALEN(laFStru,1),3] = 0
laFStru[ALEN(laFStru,1),4] = 0
=gfCrtTmp(lcTmpPolns,@laFStru,'cstytype+po+style+STR(lineno,6)+trancd',lcTmpPolns)

STORE " " TO lcStyle,lcLastSty
SELECT PosHdr
SCAN FOR &lcRpExp
  lcStyle = " "
  IF SEEK(cStyType+Po,'PosLn')
    SELECT PosLn
    SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = ;
                    PosHdr.cStyType+PosHdr.PO FOR TranCd = '1' AND TotQty <>0
      SCATTER MEMVAR MEMO
      INSERT INTO (lcTmpPolns) FROM MEMVAR
      IF SUBSTR(POSLN.STYLE , 1 , lnMajSize) <> lcStyle AND ;
         llRpPrtSn AND SEEK('F'+STYLE.cStyMajor,'Notepad') 
         SCATTER MEMVAR MEMO
         INSERT INTO (lcTmpPolns) FROM MEMVAR
         SELECT (lcTmpPolns)
         FOR lnCnt = 1 TO 8
           lcCnt = STR(lnCnt,1)
           REPLACE Qty&lcCnt WITH 0
         ENDFOR
         REPLACE TotQty WITH 0,;
              cNotePad WITH NotePad.mNotes
         lcStyle = SUBSTR(POSLN.STYLE , 1 , lnMajSize)
      ENDIF   
      lcLastSty = Style
    ENDSCAN
    SELECT (lcTmpPolns)
    APPEND BLANK
    REPLACE cStyType WITH PosHdr.cStyType      ,;
            Po       WITH PosHdr.PO            ,;
            Style    WITH SUBSTR(lcLastSty,1,lnMajSize)+"-ZZZZZ*",;
            LineNo   WITH 999999,;
            cNotePad WITH IIF(llRpPrtPn AND SEEK('P' + POSHDR.PO , 'NOTEPAD'),NotePad.mNotes,'')
  ENDIF
ENDSCAN

SELECT PosLn
SET RELATION TO
SELECT PosHdr
SET RELATION TO

SELECT (lcTmpPolns)
GOTO TOP
SET RELATION TO cStyType+PO INTO PosHdr ADDITIVE
SET RELATION TO STYLE INTO STYLE ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SET RELATION TO 'S'+SUBSTR(Posln.style,1,lnMajSize) INTO Objlink ADDITIVE

SELECT PosHdr
SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE
SET RELATION TO 'P'+POSHDR.PO INTO &TMPObjlink ADDITIVE

SELECT (lcTmpPolns)
lcRpExp = ".T."

*!*************************************************************
*! Name      : lfGetLngD
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/19/2000
*! Purpose   : to get the color long description.
*!*************************************************************
*! Called from : POSTYPEF.FRX
*!*************************************************************
*! Calls       : gfRltFld()
*!*************************************************************
*! Passed Parameters : lcReturn
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetLngD()
*!*************************************************************
FUNCTION lfGetLngD
PARAMETERS lcReturn
DIMENSION laClrLName[1,2]
PRIVATE lnAlias
lnAlias = SELECT(0)

laClrLName[1,1] = 'CLRLNAME'      && Array to get the Division long name
laClrLName[1,2] = 'lcReturn'
lcColor = SUBSTR(Style,lnCStrt,lnCLengt)
=gfRltFld(lcColor, @laClrLName , 'COLOR')
SELECT(lnAlias )


*!*************************************************************
*! Name      : lfgetYld
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/21/2000
*! Purpose   : to get the yield value
*!*************************************************************
*! Called from : POSTYPEF.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lnRetVal,llCFrmFRX
*!*************************************************************
*! Return      : Yeild value
*!*************************************************************
*! Example     : = lfgetYld()
*!*************************************************************
FUNCTION lfgetYld
PARAMETERS lnRetVal
lnRetVal = 0
lcColor = SUBSTR(Style.Style,lnCStrt,lnCLengt)
IF SEEK('3' + PADR(Style.FABRIC,19)+lcColor+LEFT(STYLE.STYLE,lnMajLen) , 'BOM')
  lnRetVal = BOM.nBOMTotQty
ELSE
  IF SEEK('3' + PADR(Style.FABRIC,19)+REPLICATE('*',6)+LEFT(STYLE.STYLE,lnMajLen) , 'BOM')
    lnRetVal = BOM.nBOMTotQty
  ENDIF
ENDIF
RETURN ''

*!*************************************************************
*! Name      : lfOpenBom
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/21/2000
*! Purpose   : Open Bom
*!*************************************************************
*! Called from : POSTYPEF.FRX
*!*************************************************************
*! Calls       : gfOpenFile()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfOpenBom()
*!*************************************************************

FUNCTION lfOpenBom
PARAMETER lcDummy

llrPrtCs = .F.
lnAlias = SELECT(0)
IF !USED('BOM')
  =gfOpenFile(gcDataDir+'BOM','BOMITEM','SH')
ENDIF
SELECT (lnAlias)
RETURN ''

*!*************************************************************
*! Name      : lfClsBom
*! Developer : AHMED MAHER (AMH)
*! Date      : 09/21/2000
*! Purpose   : Close Bom
*!*************************************************************
*! Called from : POSTYPEF.FRX
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClsBom()
*!*************************************************************

FUNCTION lfClsBom
PARAMETERS lcDummy

IF USED('BOM')
  USE IN BOM
ENDIF
RETURN ''