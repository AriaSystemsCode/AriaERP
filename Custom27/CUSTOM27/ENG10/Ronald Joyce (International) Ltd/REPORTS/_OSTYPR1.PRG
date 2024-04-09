*:***************************************************************************
*: Program file  : POSTYPR1
*: Program desc. : Print PO for RJ
*: For Report    : POSTYPR1.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : PO
*: Developer     : AHMED MAHER (AMH)
*: Reference     : C200243,1
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Modifications :
*: B605159,1 AMH 11/26/2001 Make some modifications in the PO form for RJ.
*:***************************************************************************

IF lcRpRjR1 = 'J'
  RETURN
ENDIF

PRIVATE lnI, lnColorStr, lnColorLen, lcPrmFab, lnFabCount, llOpenBom, llOpenFab
STORE .F. TO llOpenBom, llOpenFab
STORE 0 TO lnColorStr, lnColorLen
SELECT PosHdr
SET SKIP TO
SELECT PosLn
SET SKIP TO
SET ORDER TO PosLn

*-- Creating the temporary file
lcTmpPolns = gfTempName()
SELECT PosLn
=AFIELDS(laFStru)

DIMENSION laFStru[ALEN(laFStru,1)+10,4]

laFStru[ALEN(laFStru,1),1] = 'cNotePad'
laFStru[ALEN(laFStru,1),2] = 'M'
laFStru[ALEN(laFStru,1),3] = 0
laFStru[ALEN(laFStru,1),4] = 0

laFStru[ALEN(laFStru,1)-1,1] = 'cFabVend'
laFStru[ALEN(laFStru,1)-1,2] = 'C'
laFStru[ALEN(laFStru,1)-1,3] = 30
laFStru[ALEN(laFStru,1)-1,4] = 0

laFStru[ALEN(laFStru,1)-2,1] = 'cFabClr'
laFStru[ALEN(laFStru,1)-2,2] = 'C'
laFStru[ALEN(laFStru,1)-2,3] = 30
laFStru[ALEN(laFStru,1)-2,4] = 0

laFStru[ALEN(laFStru,1)-3,1] = 'cFabWdth'
laFStru[ALEN(laFStru,1)-3,2] = 'C'
laFStru[ALEN(laFStru,1)-3,3] = 6
laFStru[ALEN(laFStru,1)-3,4] = 0

laFStru[ALEN(laFStru,1)-4,1] = 'cFabPrice'
laFStru[ALEN(laFStru,1)-4,2] = 'C'
laFStru[ALEN(laFStru,1)-4,3] = 14
laFStru[ALEN(laFStru,1)-4,4] = 0

laFStru[ALEN(laFStru,1)-5,1] = 'cFabCont'
laFStru[ALEN(laFStru,1)-5,2] = 'C'
laFStru[ALEN(laFStru,1)-5,3] = 20
laFStru[ALEN(laFStru,1)-5,4] = 0

laFStru[ALEN(laFStru,1)-6,1] = 'cType'
laFStru[ALEN(laFStru,1)-6,2] = 'C'
laFStru[ALEN(laFStru,1)-6,3] = 1
laFStru[ALEN(laFStru,1)-6,4] = 0

laFStru[ALEN(laFStru,1)-7,1] = 'cObject_id'
laFStru[ALEN(laFStru,1)-7,2] = 'C'
laFStru[ALEN(laFStru,1)-7,3] = 10
laFStru[ALEN(laFStru,1)-7,4] = 0

laFStru[ALEN(laFStru,1)-8,1] = 'cFabOrder'
laFStru[ALEN(laFStru,1)-8,2] = 'C'
laFStru[ALEN(laFStru,1)-8,3] = 11
laFStru[ALEN(laFStru,1)-8,4] = 0

laFStru[ALEN(laFStru,1)-9,1] = 'cFabDesc'
laFStru[ALEN(laFStru,1)-9,2] = 'C'
laFStru[ALEN(laFStru,1)-9,3] = 20
laFStru[ALEN(laFStru,1)-9,4] = 0

=gfCrtTmp(lcTmpPolns,@laFStru,'cstytype+po+cType+cFabOrder',lcTmpPolns)

*-- Get Color Segment.
=lfGetColor()

*-- Open the bom & fabric files.
IF !USED('BOM')
  =gfOpenFile(gcDataDir+'BOM','BOM','SH')
  llOpenBom = .T.
ENDIF
IF !USED('FABRIC')
  =gfOpenFile(gcDataDir+'FABRIC','FABRIC','SH')
  llOpenFab = .T.
ENDIF

*-- Collecting Data.
SELECT PosHdr
SCAN FOR &lcRpExp
  WAIT WINDOW "PO# : " + PosHdr.PO NOWAIT
  IF SEEK(cStyType+Po,'PosLn')
    *-- Add Fabric Section.
    =lfGetType('F','3',8)
    *-- Add Lining Section.
    =lfGetType('L','2',4)
    *-- Add Trim Section.
    =lfGetType('T','4',8)
    *-- Add Footer.
    =lfAddHd()
    REPLACE cType    WITH 'X'
    *-- Add Po Notes.
    IF llRpPrtSn .AND. SEEK('F' + SUBSTR(Posln.style,1,lnMajSize) , 'NOTEPAD')
      =lfAddHd()
      REPLACE cType    WITH 'Y',;
              cNotePad WITH NotePad.mNotes
    ENDIF
    *-- Add Objects.
    IF llRpPic .AND. SEEK('S'+SUBSTR(Posln.style,1,lnMajSize),'Objlink')
      =lfAddHd()
      REPLACE cType      WITH 'Z';
              cObject_id WITH OBJLINK.COBJECT_ID
    ENDIF
  ENDIF
ENDSCAN

*-- Close the Bom file.
IF llOpenBom
  USE IN BOM
ENDIF
IF llOpenFab
  USE IN FABRIC
ENDIF

*-- Set relations between tables.
SELECT PosLn
SET RELATION TO
SELECT PosHdr
SET RELATION TO

SELECT (lcTmpPolns)
LOCATE
SET RELATION TO cStyType+PO INTO PosHdr ADDITIVE
SET RELATION TO STYLE INTO STYLE ADDITIVE
SET RELATION TO cObject_id INTO Objects ADDITIVE

SELECT PosHdr
SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE

SELECT (lcTmpPolns)
lcRpExp = ".T."

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Ahmed Maher
*! Date      : 11/05/2001
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

*!*************************************************************
*! Name      : lfGetType
*! Developer : Ahmed Maher
*! Date      : 11/06/2001
*! Purpose   : Get the Fabric/Lining/Trim information.
*!*************************************************************
*! Parameters: lcType     : (F = Fabric,L = Lining,T = Trim)
*!             lcCostType : the field typ in bom file
*!             lnRecCount : the maximum number of records to be collected
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfGetType()
*!*************************************************************
*
FUNCTION lfGetType
PARAMETERS lcType,lcCostType,lnRecCount

STORE 0 TO lnFabCount, lnFabRec
SELECT POSLN
IF SEEK(STYLE,'STYLE') .AND. SEEK(STYLE.CSTYMAJOR+lcCostType,'BOM')
  lcPrmFab = STYLE.FABRIC
  SELECT BOM
  LOCATE REST WHILE cItmMajor+Typ+cItmMask+MfgCode+Item+Iclr = STYLE.CSTYMAJOR+lcCostType;
              FOR LEFT(ITEM,7) = lcPrmFab .AND. (SUBSTR(CITMMASK,lnColorStr,lnColorLen)=;
                  SUBSTR(STYLE.STYLE,lnColorStr,lnColorLen) .OR.;
                  SUBSTR(CITMMASK,lnColorStr,lnColorLen)=REPLICATE('*',lnColorLen))
  IF FOUND()
    lnFabRec = RECNO()
    =SEEK(lcPrmFab,'FABRIC')
    =lfAddHd()
    
    *B605159,1 AMH Print nfabcost instead of costbuy & vendor name instead of vendor code [Start]
    *REPLACE cType     WITH lcType,;
            CFABVEND  WITH FABRIC.VENDOR,;
            CFABDESC  WITH FABRIC.DESC,;
            CFABCLR   WITH gfCodDes(IIF(BOM.ICLR = '******',SUBSTR(POSLN.STYLE,lnColorStr,lnColorLen),BOM.ICLR),'COLOR'),;
            CFABWDTH  WITH FABRIC.WIDTH,;
            CFABPRICE WITH FABRIC.CPRICECUR+' '+STR(FABRIC.COSTBUY,10,3),;
            CFABCONT  WITH FABRIC.CONTENT
    =SEEK(FABRIC.VENDOR,'APVENDOR')
    REPLACE cType     WITH lcType,;
            CFABVEND  WITH APVENDOR.CVENCOMP,;
            CFABDESC  WITH FABRIC.DESC,;
            CFABCLR   WITH gfCodDes(IIF(BOM.ICLR = '******',SUBSTR(POSLN.STYLE,lnColorStr,lnColorLen),BOM.ICLR),'COLOR'),;
            CFABWDTH  WITH FABRIC.WIDTH,;
            CFABPRICE WITH IIF(FABRIC.NFABCOST=0,'',FABRIC.CPRICECUR+' '+STR(FABRIC.NFABCOST,10,3)),;
            CFABCONT  WITH FABRIC.CONTENT
    *B605159,1 AMH [End]
    
    lnFabCount = 1
  ENDIF
  =SEEK(STYLE.CSTYMAJOR+lcCostType,'BOM')
  SELECT BOM
  SCAN REST WHILE cItmMajor+Typ+cItmMask+MfgCode+Item+Iclr = STYLE.CSTYMAJOR+lcCostType;
            FOR SUBSTR(CITMMASK,lnColorStr,lnColorLen)=SUBSTR(STYLE.STYLE,lnColorStr,lnColorLen) .OR.;
                SUBSTR(CITMMASK,lnColorStr,lnColorLen)=REPLICATE('*',lnColorLen)
    IF lnFabRec # 0 .AND. RECNO() = lnFabRec
      LOOP
    ENDIF
    =SEEK(LEFT(ITEM,7),'FABRIC')
    =lfAddHd()
    lnFabCount = lnFabCount + 1
    
    *B605159,1 AMH Print nfabcost instead of costbuy & vendor name instead of vendor code [Start]
    REPLACE cType     WITH lcType,;
            CFABVEND  WITH FABRIC.VENDOR,;
            CFABDESC  WITH FABRIC.DESC,;
            CFABCLR   WITH gfCodDes(IIF(BOM.ICLR = '******',SUBSTR(POSLN.STYLE,lnColorStr,lnColorLen),BOM.ICLR),'COLOR'),;
            CFABWDTH  WITH FABRIC.WIDTH,;
            CFABPRICE WITH FABRIC.CPRICECUR+' '+STR(FABRIC.COSTBUY,10,3),;
            CFABCONT  WITH FABRIC.CONTENT,;
            CFABORDER WITH 'z'+STR(lnFabCount,1)
    =SEEK(FABRIC.VENDOR,'APVENDOR')
    REPLACE cType     WITH lcType,;
            CFABVEND  WITH APVENDOR.CVENCOMP,;
            CFABDESC  WITH FABRIC.DESC,;
            CFABCLR   WITH gfCodDes(IIF(BOM.ICLR = '******',SUBSTR(POSLN.STYLE,lnColorStr,lnColorLen),BOM.ICLR),'COLOR'),;
            CFABWDTH  WITH FABRIC.WIDTH,;
            CFABPRICE WITH IIF(FABRIC.NFABCOST=0,'',FABRIC.CPRICECUR+' '+STR(FABRIC.NFABCOST,10,3)),;
            CFABCONT  WITH FABRIC.CONTENT,;
            CFABORDER WITH 'z'+STR(lnFabCount,1)
    *B605159,1 AMH [End]
    
    IF lnFabCount = lnRecCount
      EXIT
    ENDIF
  ENDSCAN
ENDIF
IF lnFabCount < lnRecCount
  PRIVATE lnI
  FOR lnI = lnFabCount + 1 TO lnRecCount
    =lfAddHd()
    REPLACE cType     WITH lcType,;
            CFABORDER WITH 'z'+STR(lnI,1)
  ENDFOR
ENDIF
*--end of lfGetType.

*!*************************************************************
*! Name      : lfAddHd
*! Developer : Ahmed Maher
*! Date      : 11/06/2001
*! Purpose   : Add the Hader information.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfAddHd()
*!*************************************************************
*
FUNCTION lfAddHd

SELECT POSLN
SCATTER MEMVAR MEMO
INSERT INTO (lcTmpPolns) FROM MEMVAR
SELECT (lcTmpPolns)
*--end of lfAddHd.

*!*************************************************************
*! Name      : lfvRjR1
*! Developer : Ahmed Maher
*! Date      : 11/06/2001
*! Purpose   : Print Bulk/Sample Purchase Order.
*!*************************************************************
*! Parameters: lcDummy
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfvRjR1()
*!*************************************************************
*
FUNCTION lfvRjR1
PARAMETERS lcDummy

lcDummy = .T.
lcFormName = 'POSTYPR' + lcRpRjR1
RETURN .T.
*--end of lfvRjR1.