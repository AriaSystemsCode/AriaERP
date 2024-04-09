*:***************************************************************************
*: Program file  : POSTYPM1.PRG
*: Program desc. : CUSTOMIZED PURCHASE ORDER FOR MBI.
*: Date          : 12/23/2001
*: System        : Aria Advantage Series.
*: Module        : STYLE PURCHASE ORDER (PO)
*: Developer     : (AMH) Ahmed Maher
*: Tracking Job Number: C200251
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO POSTYPMB
*:***************************************************************************
*: Modifications :
*: C200290,1 AMH 02/06/2002 Don't print the primary fabric in BOM report, 
*: C200290,1                and print the sizes desctiption.
*:***************************************************************************

PRIVATE lnColorStr, lnColorLen
STORE 0 TO lnColorStr, lnColorLen

*--Section break the relation and skip to.
SELECT PosHdr
SET SKIP TO

*-- Creating the temporary file and its index.
SELECT PosLn
=AFIELDS(laFStru)
DIMENSION laFStru[ALEN(laFStru,1)+1,4]
laFStru[ALEN(laFStru,1),1] = 'cStyMajor'
laFStru[ALEN(laFStru,1),2] = 'C'
laFStru[ALEN(laFStru,1),3] = 19
laFStru[ALEN(laFStru,1),4] = 0

DIMENSION laFStru[ALEN(laFStru,1)+1,4]
laFStru[ALEN(laFStru,1),1] = 'cDesc'
laFStru[ALEN(laFStru,1),2] = 'C'
laFStru[ALEN(laFStru,1),3] = 60
laFStru[ALEN(laFStru,1),4] = 0

DIMENSION laFStru[ALEN(laFStru,1)+1,4]
laFStru[ALEN(laFStru,1),1] = 'cUsr_Name'
laFStru[ALEN(laFStru,1),2] = 'C'
laFStru[ALEN(laFStru,1),3] = 35
laFStru[ALEN(laFStru,1),4] = 0

DIMENSION laFStru[ALEN(laFStru,1)+1,4]
laFStru[ALEN(laFStru,1),1] = 'cFabVend'
laFStru[ALEN(laFStru,1),2] = 'C'
laFStru[ALEN(laFStru,1),3] = 30
laFStru[ALEN(laFStru,1),4] = 0

DIMENSION laFStru[ALEN(laFStru,1)+1,4]
laFStru[ALEN(laFStru,1),1] = 'cStyFab'
laFStru[ALEN(laFStru,1),2] = 'C'
laFStru[ALEN(laFStru,1),3] = 7
laFStru[ALEN(laFStru,1),4] = 0

DIMENSION laFStru[ALEN(laFStru,1)+1,4]
laFStru[ALEN(laFStru,1),1] = 'nFabCost'
laFStru[ALEN(laFStru,1),2] = 'N'
laFStru[ALEN(laFStru,1),3] = 11
laFStru[ALEN(laFStru,1),4] = 3

DIMENSION laFStru[ALEN(laFStru,1)+1,4]
laFStru[ALEN(laFStru,1),1] = 'nIssued'
laFStru[ALEN(laFStru,1),2] = 'N'
laFStru[ALEN(laFStru,1),3] = 12
laFStru[ALEN(laFStru,1),4] = 3

DIMENSION laFStru[ALEN(laFStru,1)+1,4]
laFStru[ALEN(laFStru,1),1] = 'cType'
laFStru[ALEN(laFStru,1),2] = 'C'
laFStru[ALEN(laFStru,1),3] = 1
laFStru[ALEN(laFStru,1),4] = 0

DIMENSION laFStru[ALEN(laFStru,1)+1,4]
laFStru[ALEN(laFStru,1),1] = 'cObject_id'
laFStru[ALEN(laFStru,1),2] = 'C'
laFStru[ALEN(laFStru,1),3] = 10
laFStru[ALEN(laFStru,1),4] = 0

DIMENSION laFStru[ALEN(laFStru,1)+1,4]
laFStru[ALEN(laFStru,1),1] = 'cColor'
laFStru[ALEN(laFStru,1),2] = 'C'
laFStru[ALEN(laFStru,1),3] = 6
laFStru[ALEN(laFStru,1),4] = 0

=gfCrtTmp(lcTmpPolns,@laFStru,'cstytype+PO+cType+Style',lcTmpPolns)

IF llrPrtCs .AND. lcRpForm='P'
  SELECT (lcCostF)
  ZAP
  INDEX ON CUTTKT+Typ+cCatgTyp+MfgCode+Item+IClr TAG lcCostF
  SET ORDER TO TAG lcCostF
ENDIF

*--Section of open needed files.
PRIVATE llOpenFab,llOpenBom,llOpenCBom
STORE .F. TO llOpenFab,llOpenBom,llOpenCBom
IF !USED('FABRIC')
  llOpenFab = gfOpenFile(gcDataDir+'FABRIC','FABRIC','SH')
ENDIF
IF !USED('BOM')
  llOpenBom = gfOpenFile(gcDataDir+'BOM','BOM','SH')
ENDIF
IF !USED('CTKTBOM')
  llOpenCBom = gfOpenFile(gcDataDir+'CTKTBOM','CTKTYP','SH')
ENDIF

*-- Get Color Segment.
=lfGetColor()

*--Section of collecting the data.
SELECT PosHdr
SCAN FOR &lcRpExp
  STORE SPACE(0) TO lcStyMajor,lcDesc,lcUsr_Name,lcFabVend,lcStyFab
  STORE 0        TO lnFabCost
  IF SEEK(cStyType+Po,'PosLn')
    *-- Add Objects.
    IF SEEK('S'+SUBSTR(Posln.style,1,lnMajSize),'Objlink')
      SELECT OBJLINK
      SCAN REST WHILE cObjLnkTyp+cObjLink = 'S'+SUBSTR(Posln.style,1,lnMajSize)
        SELECT (lcTmpPolns)
        APPEND BLANK
        REPLACE cStyType WITH PosHdr.cStyType;
                Po       WITH PosHdr.PO;
                cType    WITH 'Z';
                cObject_id WITH OBJLINK.COBJECT_ID
      ENDSCAN
    ENDIF
    *-- Add Detail lines.
    SELECT PosLn
    IF SEEK(STYLE,'STYLE')
      lcStyMajor = STYLE.CSTYMAJOR
      lcDesc     = STYLE.DESC1
      lcStyFab   = STYLE.FABRIC
      IF SEEK(STYLE.FABRIC,'FABRIC') .AND. SEEK(FABRIC.VENDOR,'APVENDOR')
        lcFabVend = APVENDOR.CVENCOMP
      ENDIF
      IF SEEK(STYLE.CSTYMAJOR,'BOM')
        SELECT BOM
        LOCATE REST WHILE cItmMajor+Typ+cItmMask+MfgCode+Item+Iclr = STYLE.CSTYMAJOR;
                    FOR PADR(ITEM,7) = STYLE.FABRIC .AND. CCATGTYP = 'F'
        IF FOUND()
          lnFabCost = NBOMTOTQTY
        ENDIF
        SELECT POSLN
      ENDIF
    ENDIF
    IF SEEK(POSHDR.CADD_USER,'SYUUSER')
      lcUsr_Name = SYUUSER.CUSR_NAME
    ENDIF
    SCAN REST WHILE cstytype+PO+cWareCode+Account+Store+Style+TranCd = ;
                    PosHdr.cStyType+PosHdr.PO FOR TRANCD = '1'
      SCATTER MEMVAR
      m.cStyMajor = lcStyMajor
      m.cDesc     = lcDesc
      m.cUsr_Name = lcUsr_Name
      m.cFabVend  = lcFabVend
      m.cStyFab   = lcStyFab
      m.nFabCost  = lnFabCost
      m.nIssued   = lfGetIssue()
      m.cType     = 'D'
      m.cColor    = SUBSTR(m.Style,lnColorStr,lnColorLen)
      IF SEEK(m.cStyType+m.Po+m.cType+SUBSTR(m.Style,1,lnColorStr+lnColorLen-1),lcTmpPolns)
        SELECT (lcTmpPoLns)
        FOR lnI = 1 TO 8
          lcI = STR(lnI,1)
          REPLACE ('QTY'+lcI) WITH EVALUATE('QTY'+lcI) + EVALUATE('m.Qty'+lcI)
        ENDFOR
        REPLACE TOTQTY WITH TOTQTY + m.TotQty
      ELSE
        INSERT INTO (lcTmpPolns) FROM MEMVAR
      ENDIF
    ENDSCAN
    m.cStyMajor = lcStyMajor
    m.cDesc     = lcDesc
    m.cUsr_Name = lcUsr_Name
    m.cFabVend  = lcFabVend
    m.cStyFab   = lcStyFab
    m.nFabCost  = lnFabCost
    m.nIssued   = 0
    m.cType     = 'E'
    m.TotQty    = 0
    INSERT INTO (lcTmpPolns) FROM MEMVAR
  ENDIF
  IF llRpPrtPn .AND. SEEK('P' + POSHDR.PO , 'NOTEPAD')
    SELECT (lcTmpPolns)
    APPEND BLANK
    REPLACE cStyType WITH PosHdr.cStyType;
            Po       WITH PosHdr.PO;
            cType    WITH 'N'
  ENDIF
  IF llrPrtCs .AND. lcRpForm='P' .AND. SEEK('I'+POSHDR.PO,'CTKTBOM')
    SELECT CTKTBOM
    
    *C200290,1 AMH Don't print the primary fabric in BOM report [Start]
    *SCAN WHILE cImTyp+CutTkt = 'I'+POSHDR.PO FOR cCatgTyp $ 'FTS'
    SCAN WHILE cImTyp+CutTkt = 'I'+POSHDR.PO FOR cCatgTyp $ 'FTS' .AND.;
         IIF(CCATGTYP = 'F' .AND. PADR(ITEM,7)==lcStyFab,.F.,.T.)
    *C200290,1 AMH [End]
    
      SCATTER MEMVAR
      =SEEK(SUBSTR(ITEM,1,7),'FABRIC')
      m.mfgCode  = PADR(STR(999999-VAL(FABRIC.CPURCODE),6,0),6,'0')
      m.cOprCode = FABRIC.CPURCODE
      INSERT INTO (lcCostF) FROM MEMVAR 
    ENDSCAN
  ENDIF
ENDSCAN

*--Section of close opend files.
IF llOpenFab
  USE IN FABRIC
ENDIF
IF llOpenBom
  USE IN BOM
ENDIF
IF llOpenCBom
  USE IN CTKTBOM
ENDIF

*--Section of relations.
SELECT PosLn
SET RELATION TO
SELECT PosHdr
SET RELATION TO

SELECT (lcTmpPolns)
SET RELATION TO cStyType+PO INTO PosHdr ADDITIVE
SET RELATION TO STYLE INTO STYLE ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SET RELATION TO cObject_id INTO Objects ADDITIVE

SELECT PosHdr
SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE
SET RELATION TO 'P'+PO INTO NOTEPAD ADDITIVE
SET RELATION TO CSTYTYPE+PO INTO POSLN ADDITIVE

*--Section of company information.
DECLARE laCompAdd[7]
laCompAdd[7] = ''
FOR lnI = 1 TO 7
  IF EMPTY(laCompAdd[lnI])
    laCompAdd[lnI] = 'Fax# : ' + TRANSFORM(lcCompFax,lcPhonPict)
    EXIT
  ENDIF
ENDFOR

*--Section of runing the report.
SELECT (lcTmpPolns)
GOTO TOP
lcRpExp = '.T.'

*!*************************************************************
*! Name      : lfGetIssue
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/06/2002
*! Purpose   : Function to get the issued qty.
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : issued qty
*!*************************************************************
*! Example     : = lfGetIssue()
*!*************************************************************
FUNCTION lfGetIssue

PRIVATE lcStyle,lcColor,lnAlias,lcPoslnKey,lnTotQty,lnStyQty,lcOrder
lcStyle = SUBSTR(m.Style,1,lnMajSize)
lcColor = SUBSTR(m.Style,lnColorStr,lnColorLen)
lnReturn = 0
IF SEEK(lcStyle,'BOM') .AND. !EMPTY(lcStyFab)
  lnAlias = SELECT(0)
  SELECT BOM
  LOCATE REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = lcStyle;
              FOR (SUBSTR(citmmask,lnColorStr,lnColorLen) = lcColor .OR. SUBSTR(citmmask,lnColorStr,lnColorLen) = '******') .AND.;
                  PADR(ITEM,7) = lcStyFab .AND. CCATGTYP = 'F'
  IF FOUND()
    IF ICLR <> '******' .AND. SUBSTR(CITMMASK,lnColorStr,lnColorLen) = '******'
      SELECT POSLN
      lcPoslnKey = EVALUATE(KEY())
      SEEK POSHDR.CSTYTYPE+POSHDR.PO
      SUM TOTQTY REST WHILE cstytype+PO+cWareCode+Account+Store+Style+TranCd =;
                            POSHDR.CSTYTYPE+POSHDR.PO TO lnTotqty
      SEEK POSHDR.CSTYTYPE+POSHDR.PO
      SUM TOTQTY REST WHILE cstytype+PO+cWareCode+Account+Store+Style+TranCd =;
                            POSHDR.CSTYTYPE+POSHDR.PO FOR STYLE = SUBSTR(m.Style,1,lnColorStr+lnColorLen-1) TO lnStyqty
      SEEK lcPoslnKey
    ELSE
      STORE 1 TO lnTotQty,lnStyQty
    ENDIF
    SELECT CTKTBOM
    lcOrder = SET('ORDER')
    SET ORDER TO TAG CTKTYP
    IF SEEK('I'+POSHDR.PO+BOM.ITEM+IIF(BOM.ICLR='******',lcColor,BOM.ICLR),'CTKTBOM')
      lnReturn = ISSUE_QTY * lnStyQty / lntotQty
    ENDIF
    SET ORDER TO &lcOrder.
  ENDIF
ENDIF
RETURN lnReturn
*-- End of lfGetIssue.

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Ahmed Maher
*! Date      : 01/22/2002
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
ENDFOR
*--end of lfGetColor.