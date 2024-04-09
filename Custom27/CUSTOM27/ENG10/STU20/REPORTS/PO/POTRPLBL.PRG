****************************************************************************
*: Program file      : POTRPLBL.PRG   (C#200208)
*: Program desc.     : TRIM PACK & LABELLING INSTRUCTIONS
*: System            : Aria Apparel System (A27).
*: Module            : Style Purchase Order (PO)
*: Developer         : (AMH) AHMED MAHER
*: Date              : 07/24/2001
*:**************************************************************************
*:Modifications :
*:****************************************************************************************
*: C200360,1 AMH 06/25/2002 Add new option to print tim pack , labelling instructions or both 
*: C200360,1 AMH            and print trim pack for hold PO.
*:****************************************************************************************

*-- Create temp File
lnMajorLen = LEN(gfItemMask("PM"))
STORE SPACE(0) TO lcPrmFab, lcFabType
*STORE .F. TO llAddPo
lcTmpFile  = gfTempName()

*C200360,1 AMH Add color Field [Start]
*CREATE CURSOR (lcTmpFile);
   (CPO C(6),CTYPE C(1),CSTYLE C(19),CCONSINFO1 C(30),CITEM C(7),CWIDTH C(6),CDESC C(20),;
    CCONTENT C(20),NUNTQTY N(7,3),NREQ_QTY N(12,3),CVENSTY C(19),CORDER C(6),CPRMFAB C(60),;
    CFABTYPE C(60))
CREATE CURSOR (lcTmpFile);
   (CPO C(6),CTYPE C(1),CSTYLE C(19),CCONSINFO1 C(30),CITEM C(7),CWIDTH C(6),CDESC C(20),;
    CCONTENT C(20),NUNTQTY N(7,3),NREQ_QTY N(12,3),CVENSTY C(19),CORDER C(6),CPRMFAB C(60),;
    CFABTYPE C(60),ICLR C(6))
*INDEX ON CPO+CTYPE TAG (lcTmpFile)  OF (lcTmpFile)
INDEX ON CPO+CTYPE+CITEM+ICLR TAG (lcTmpFile)  OF (lcTmpFile)

*--Get color segment information.
STORE 0 TO lnClrSrt,lnClrEnd
STORE 0 TO lnSizePos,lnSizeLen
llExtSizSc = gfGetMemVar('M_USEEXSSC',gcAct_Comp)

=lfGetColor()
llOpenBom = .F.
IF !USED('BOM')
  =gfOpenFile(gcDataDir+'BOM','BOM','SH')
  llOpenBom = .T.
ENDIF
*C200360,1 AMH [End]

SELECT (lcTmpFile)
SET ORDER TO (lcTmpFile)

*-- To get the selected PO if any.
lcPoFile = ''
llUsePo = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POSHDR.PO'),1)
IF lnPosition > 0
  lcPoFile = laOGFxFlt[lnPosition,6]
  llUsePo = IIF(!EMPTY(lcPoFile) .AND. USED(lcPoFile) .AND. RECCOUNT(lcPoFile)>0,.T.,.F.)
ENDIF

* -- If the user select Po.
IF llUsePo
  SELECT (lcPoFile)
ELSE
  SELECT POSHDR
ENDIF
LOCATE
SCAN
  IF llUsePo
    =SEEK('P'+PO,'POSHDR')
  ENDIF
  =SEEK('P'+PO,'POSLN')
  =SEEK(POSLN.STYLE,'STYLE')
  =SEEK(STYLE.FABRIC,'FABRIC')
  lcPrmFab = FABRIC.CONTENT
  =SEEK('2'+PO,'CUTPICK')
  IF SEEK('I'+PO,'CTKTBOM')
    SELECT CTKTBOM
    SCAN REST WHILE Cimtyp+Cuttkt+Item+Iclr+Mfgcode+Dyelot = 'I'+;
              EVALUATE(IIF(llUsePo,lcPoFile,'POSHDR')+'.PO') FOR CCATGTYP = 'T'
      *llAddPo = .T.
      =SEEK(PADR(ITEM,7)+ICLR,'FABRIC')
      lcFabType = IIF(ALLTRIM(gfCodDes(FABRIC.ITEM_TYPE,'ITEM_TYPE '))=='999999',FABRIC.CONTENT,lcFabType)
  
      *C200360,1 AMH Print Trim Pack only if use choice to print Trim Pack or Both [Start]
      IF lcRpPrint $ 'TB'
      *C200360,1 AMH [End]
  
      SELECT (lcTmpFile)
      APPEND BLANK
      REPLACE CPO        WITH POSHDR.PO,;
              CTYPE      WITH '1',;
              CSTYLE     WITH SUBSTR(POSLN.STYLE,1,lnMajorLen),;
              CCONSINFO1 WITH STYLE.CCONSINFO1,;
              CITEM      WITH CTKTBOM.ITEM,;
              CWIDTH     WITH CTKTBOM.WIDTH,;
              CDESC      WITH CTKTBOM.DESC,;
              CCONTENT   WITH FABRIC.CONTENT,;
              NUNTQTY    WITH CTKTBOM.UNTQTY,;
              NREQ_QTY   WITH CTKTBOM.REQ_QTY
      REPLACE CVENSTY    WITH STYLE.CVENSTY,;
              CORDER     WITH CUTPICK.ORDER,;
              CPRMFAB    WITH lcPrmFab
  
      *C200360,1 AMH Ending the if statment for printing the Trim Pack [Start]
      ENDIF
      *C200360,1 AMH [End]
      
    ENDSCAN
  
  *C200360,1 AMH Print trim pack in case of hold PO [Start]
  ELSE
    SELECT POSLN
    SCAN REST WHILE cStyType+Po+Style+STR(LineNo,6)+Trancd = 'P'+POSHDR.PO FOR TRANCD = '1'
      IF SEEK(PADR(SUBSTR(STYLE,1,lnMajorLen),19),'BOM')
        SELECT BOM
        SCAN REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr =;
                        PADR(SUBSTR(POSLN.STYLE,1,lnMajorLen),19);
                    FOR LIKE(STRTRAN(cItmMask,'*','?'),POSLN.STYLE) AND cCatgTyp = 'T'
          lcColor = IIF(ICLR='******',SUBSTR(POSLN.STYLE,lnClrSrt,lnClrEnd),ICLR)
          IF SEEK(POSLN.PO+'1'+PADR(ITEM,7)+lcColor,lcTmpFile)
            SELECT (lcTmpFile)
            lnTotQty = lfTotQty()
            REPLACE NUNTQTY    WITH IIF(NREQ_QTY+(lnTotQty*NUNTQTY)=0,0,;
                                        NUNTQTY*(NREQ_QTY+(BOM.NBOMTOTQTY*lnTotQty))/;
                                        (NREQ_QTY+(lnTotQty*NUNTQTY))),;
                    NREQ_QTY   WITH NREQ_QTY + (BOM.NBOMTOTQTY*lnTotQty)
          ELSE
            =SEEK(PADR(ITEM,7)+lcColor,'FABRIC')
            lcFabType = IIF(ALLTRIM(gfCodDes(FABRIC.ITEM_TYPE,'ITEM_TYPE '))=='999999',FABRIC.CONTENT,lcFabType)
            IF lcRpPrint $ 'TB'
              SELECT (lcTmpFile)
              APPEND BLANK
              REPLACE CPO        WITH POSHDR.PO,;
                      CTYPE      WITH '1',;
                      CSTYLE     WITH SUBSTR(POSLN.STYLE,1,lnMajorLen),;
                      CCONSINFO1 WITH STYLE.CCONSINFO1,;
                      CITEM      WITH BOM.ITEM,;
                      CWIDTH     WITH FABRIC.WIDTH,;
                      CDESC      WITH BOM.DESC,;
                      CCONTENT   WITH FABRIC.CONTENT,;
                      NUNTQTY    WITH BOM.NBOMTOTQTY,;
                      NREQ_QTY   WITH BOM.NBOMTOTQTY*lfTotQty()
              REPLACE CVENSTY    WITH STYLE.CVENSTY,;
                      CORDER     WITH CUTPICK.ORDER,;
                      CPRMFAB    WITH lcPrmFab,;
                      ICLR       WITH lcColor
            ENDIF
          ENDIF
        ENDSCAN
      ENDIF
    ENDSCAN
  *C200360,1 AMH [End]
  
  ENDIF
  
  *C200360,1 AMH Print Labelling ins. only if use choice to print Labelling ins. or Both [Start]
  IF lcRpPrint $ 'LB'
  *C200360,1 AMH [End]
  
  SELECT (lcTmpFile)
  APPEND BLANK
  REPLACE CPO        WITH POSHDR.PO,;
          CTYPE      WITH '2',;
          CVENSTY    WITH STYLE.CVENSTY,;
          CORDER     WITH CUTPICK.ORDER,;
          CPRMFAB    WITH lcPrmFab,;
          CFABTYPE   WITH lcFabType
  
  *C200360,1 AMH Ending the if statment for printing the labelling ins. [Start]
  ENDIF
  *C200360,1 AMH [End]
  
ENDSCAN

SELECT(lcTmpFile)
LOCATE
IF EOF()
  SET DEVICE TO SCREEN
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF
SET RELATION TO 'P'+CPO INTO LBLINST
*=SEEK(gcAct_Comp,'SYCCOMP')

*--- Start printing report
DO gfDispRe WITH EVAL('lcFormName')
SET DEVICE TO SCREEN

IF USED(lcTmpFile)
  USE IN (lcTmpFile)
ENDIF

*C200360,1 AMH Close the Bom file [Start]
IF USED('BOM') .AND. llOpenBom
  USE IN BOM
ENDIF
*C200360,1 AMH [End]

*!**********************************************************************
*! Name      : lfGetColor
*! Developer : AHMED MAHER (AMH)
*! Date      : 06/24/2002
*! Purpose   : Get the color length and width.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetColor()
*!*************************************************************
*C200360,1 AMH
FUNCTION lfGetColor

DIME laMajSeg[1,1]
=gfItemMask(@laMajSeg)
FOR lnCnt=1 TO ALEN(laMajSeg,1)
  *--Check for existance of color segment in style structure.
  IF laMajSeg[lnCnt,1]='C'
    *--Get the color length and width.
    lnClrSrt = laMajSeg[lnCnt,4]
    lnClrEnd = LEN(laMajSeg[lnCnt,3])
  ENDIF
  
  *--Check for existance of color segment in style structure.
  IF llExtSizSc .AND. laMajSeg[lnCnt,1] = 'S'
    *--Get the size length and width.
    lnSizePos = laMajSeg[lnCnt,4]
    lnSizeLen = LEN(laMajSeg[lnCnt,3])
  ENDIF
ENDFOR
RETURN

*!**********************************************************************
*! Name      : lfTotQty
*! Developer : AHMED MAHER (AMH)
*! Date      : 06/24/2002
*! Purpose   : Get the quantity req. by size.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfTotQty()
*!*************************************************************
*C200360,1 AMH
FUNCTION lfTotQty

lnTotQty = 0
IF BOM.LBASONSIZ .AND. !EMPTY(BOM.MSIZES)
  lnMemWidth = SET('MEMOWIDTH')
  SET MEMOWIDTH TO 40
  FOR lnJ = 1 TO MEMLINES(BOM.MSIZES)
    lnStrSize = AT('~',MLINE(BOM.MSIZES,lnJ))
    IF lnStrSize > 0 .AND. LEN(MLINE(BOM.MSIZES,lnJ)) > lnStrSize .AND.;
       IIF(llExtSizSc,SUBSTR(POSLN.STYLE,lnSizePos,lnSizeLen)=;
           SUBSTR(MLINE(BOM.MSIZES,lnJ),1,lnStrSize-1),.T.)
      FOR lnI = lnStrSize+1 TO LEN(MLINE(BOM.MSIZES,lnJ))
        lcI = SUBSTR(MLINE(BOM.MSIZES,lnJ),lnI,1)
        IF VAL(lcI) > 0
          lnTotQty = lnTotQty + EVALUATE('POSLN.QTY'+lcI)
        ENDIF
      ENDFOR
    ENDIF
  ENDFOR
  SET MEMOWIDTH TO lnMemWidth
ELSE
  lnTotQty = POSLN.TOTQTY
ENDIF
RETURN lnTotQty