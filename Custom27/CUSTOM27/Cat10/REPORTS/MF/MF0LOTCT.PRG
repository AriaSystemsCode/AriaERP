*:***********************************************************************
*: PROG         : MF0LOTCT.PRG
*: DATE         : 11/29/2001
*: Developer    : AHMED MAHER (AMH)
*: DESC         : Zero lot Cutting Ticket for Cathy Daneils (Cust. Prog. # 102484)
*:***********************************************************************
*: Calls        : 
*:         Functions  : 
*:         Procedures : 
*:***********************************************************************
*: Example      : DO MF0LOTCT
*:***********************************************************************
* Modifications :
* B605242,1 AMH 12/19/2002 Fix some Problems in the Zero Lot report and CT form.
* B605387,1 AMH 01/17/2002 Changing the printing of fabric description.
* B605388,1 AMH 01/17/2002 Changing the printing of fabric color.
* B605413,1 AMH 01/24/2002 Fix the problem of not getting the Yardes in case
* B605413,1                of having a fabric color that does not match
* B605413,1                a style color in the CT line section.
* B605448,1 AMH 01/30/2002 Print Color long description in a 
* B605448,1                condition of fabric's clr. starts with number.
* B605496,1 AMH 02/07/2002 Print fabric.content in the desc. of the trim section.
* C200293,1 AMH 02/24/2002 Print the sectioned marker data.
* B605790,1 AMH 04/03/2002 Sorting fabrics according to desc.
* B606481,1 AMH 10/20/2002 Fix bug of not get the correct marker data.
*:***********************************************************************

*-- To get the selected style if any.
lcStylFile = ''
llUseStyle = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.STYLE'),1)
IF lnPosition > 0
  lcStylFile = laOGFxFlt[lnPosition,6]
  llUseStyle = IIF(!EMPTY(lcStylFile) .AND. USED(lcStylFile) .AND. RECCOUNT(lcStylFile)>0,.T.,.F.)
ENDIF

*-- To Create cursors.
STORE SPACE(0) TO lcTmpCtktH,lcTmpCtktL,lcTmpCtktB
=lfCreatCur('H','CUTTKT')
=lfCreatCur('L','CUTTKT+STYLE+DYELOT+TRANCD')
=lfCreatCur('B','CIMTYP+CUTTKT+TYP+ITEM+ICLR+MFGCODE+DYELOT')

*-- Create C/Ts.
lcCuttkt = '000000'
IF !llUseStyle
  lcStyAlias = gfTempName()
  = gfOpenFile(gcDataDir+'STYLE','CSTYLE','SH', @lcStyAlias , .T.)
  lcStylFile = lcStyAlias
ENDIF
SELECT (lcStylFile)
lcStyMajor = SPACE(19)
SCAN
  lcStyMajor = SUBSTR(CSTYMAJOR,1,12)
  WAIT WINDOW 'Creating cutting ticket for style : ' + ALLTRIM(CSTYMAJOR) + ' ...' NOWAIT
  IF SEEK(SUBSTR(CSTYMAJOR,1,12),'STYLE') .AND. SEEK(CSTYMAJOR,'BOM')
    lcCuttkt = PADL(ALLTRIM(STR(VAL(lcCuttkt)+1,6,0)),6,'0')
    SELECT (lcTmpCtktH)
    APPEND BLANK
    REPLACE CUTTKT    WITH lcCuttkt,;
            STYLE     WITH STYLE.CSTYMAJOR,;
            SEASON    WITH STYLE.SEASON,;
            CDIVISION WITH STYLE.CDIVISION,;
            COMPLETE  WITH gdSysDate,;
            NLOT_NO   WITH 0
    SELECT STYLE
    DECLARE laQty[8]
    STORE 0 TO laQty
    =SEEK('S'+SCALE,'SCALE')
    DO CASE
      CASE SCALE.CNT = 1
        laQty[1] = 12
      CASE SCALE.CNT = 2
        laQty[1] = 6
        laQty[2] = 6
      OTHERWISE
        laQty[1] = 2
        laQty[2] = 6
        laQty[3] = 4
    ENDCASE
    IF SEEK('P'+SCALE+PREPAK,'SCALE') .AND. SCALE.PPTOT # 0 .AND. MOD(12,SCALE.PPTOT) = 0
      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        laQty[lnI] = EVALUATE('SCALE.PP'+lcI) * 12 / SCALE.PPTOT
      ENDFOR
    ENDIF
    SCAN REST WHILE STYLE = lcStyMajor
      SELECT (lcTmpCtktL)
      APPEND BLANK
      REPLACE CUTTKT WITH lcCuttkt,;
              STYLE  WITH STYLE.STYLE,;
              TRANCD WITH '1'
      FOR lnI = 1 TO 8
        lcI = STR(lnI,1)
        REPLACE QTY&lcI. WITH laQty[lnI]
        REPLACE TOTQTY   WITH TOTQTY + laQty[lnI]
      ENDFOR
    ENDSCAN
  ENDIF
ENDSCAN
WAIT CLEAR

IF !llUseStyle
  USE IN (lcStyAlias)
ENDIF

*-- Check to see if there are any cutting ticket created to print.
SELECT (lcTmpCtktH)
LOCATE
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  USE IN (lcTmpCtktH)
  USE IN (lcTmpCtktL)
  USE IN (lcTmpCtktB)
  llPoDisp = .F.
  RETURN
ENDIF

*-- Create CtktBom records.
SELECT (lcTmpCtktB)
INDEX ON cimtyp+cuttkt+item+iclr+mfgcode+dyelot TAG cTktyp OF (lcTmpCtktB)
SELECT (lcTmpCtktL)
lcCuttkt = SPACE(6)
SCAN
  IF lcCuttkt # CUTTKT
    WAIT WINDOW 'Creating cutting ticket cost sheet for style : ' + ALLTRIM(SUBSTR(STYLE,1,12)) + ' ...' NOWAIT
    lcCuttkt = CUTTKT
  ENDIF
  IF SEEK(SUBSTR(STYLE,1,12),'BOM')
    IF SEEK('M'+CUTTKT,lcTmpCtktB)
      SELECT (lcTmpCtktB)
      REPLACE REST WHILE cImTyp+Cuttkt+Typ+Item+Iclr+MfgCode+Dyelot =;
                         'M'+EVALUATE(lcTmpCtktL+'.CUTTKT') FLAG WITH SPACE(1)
    ENDIF
    SELECT BOM
    SCAN REST WHILE CITMMAJOR+TYP+CITMMASK+MFGCODE+ITEM+ICLR = ;
                    SUBSTR(EVALUATE(lcTmpCtktL+'.STYLE'),1,12) ;
              FOR SUBSTR(CITMMASK,14,6) = SUBSTR(EVALUATE(lcTmpCtktL+'.STYLE'),14,6) .OR. ;
                  SUBSTR(CITMMASK,14,6) = '******'
      
      *B605242,1 AMH Get the correct key [Start]
      *lcBomKey = 'M'+EVALUATE(lcTmpCtktL+'.CUTTKT')+TYP+ITEM+ICLR+MFGCODE+EVALUATE(lcTmpCtktL+'.DYELOT')
      lcBomKey = 'M'+EVALUATE(lcTmpCtktL+'.CUTTKT')+ITEM+IIF(ICLR='******',SUBSTR(EVALUATE(lcTmpCtktL+'.STYLE'),14,6),ICLR)+MFGCODE+EVALUATE(lcTmpCtktL+'.DYELOT')
      *B605242,1 AMH [End]
      
      SELECT (lcTmpCtktB)
      IF !SEEK(lcBomKey)
        APPEND BLANK
        REPLACE CUTTKT   WITH EVALUATE(lcTmpCtktL+'.CUTTKT'),;
                CIMTYP   WITH 'M',;
                CCATGTYP WITH BOM.CCATGTYP,;
                TYP      WITH BOM.TYP,;
                ITEM     WITH BOM.ITEM,;
                ICLR     WITH IIF(BOM.ICLR='******',SUBSTR(EVALUATE(lcTmpCtktL+'.STYLE'),14,6),BOM.ICLR),;
                CMARKER  WITH BOM.CMARKER,;
                DESC     WITH BOM.DESC,;
                MFGCODE  WITH BOM.MFGCODE,;
                UOM      WITH BOM.UOM,;
                UNTCOST  WITH BOM.UNTCOST,;
                DATE     WITH gdSysDate
      ENDIF
      IF FLAG <> 'Y'
        *IF BOM.CCATGTYP = 'T' .AND. BOM.LBASONSIZ
        IF BOM.CCATGTYP = 'T' .AND. !EMPTY(ALLTRIM(BOM.MSIZES))
          lnStart = AT('~',ALLTRIM(BOM.MSIZES)) + 1
          FOR lnI = 1 TO 8
            lcI = STR(lnI,1)
            REPLACE PIECES WITH PIECES + IIF(lcI$SUBSTR(ALLTRIM(BOM.MSIZES),lnStart),;
                                             EVALUATE(lcTmpCtktL+'.QTY'+lcI),0)
          ENDFOR
        ELSE
          REPLACE PIECES WITH PIECES + EVALUATE(lcTmpCtktL+'.TOTQTY')
        ENDIF
        REPLACE FLAG WITH 'Y'
      ENDIF
      IF BOM.CCATGTYP <> 'T'
        REPLACE REQ_QTY   WITH REQ_QTY + (EVALUATE(lcTmpCtktL+'.TOTQTY') * BOM.NBOMTOTQTY)
        REPLACE UNTQTY    WITH IIF(PIECES<>0,REQ_QTY/PIECES,0),;
                ISSUE_QTY WITH REQ_QTY,;
                USED_QTY  WITH REQ_QTY
      ELSE
        *IF BOM.LBASONSIZ
        IF !EMPTY(ALLTRIM(BOM.MSIZES))
          lnStart = AT('~',ALLTRIM(BOM.MSIZES)) + 1
          FOR lnI = 1 TO 8
            lcI = STR(lnI,1)
            IF lcI $ SUBSTR(ALLTRIM(BOM.MSIZES),lnStart)
              REPLACE REQ_QTY&lcI. WITH EVALUATE('REQ_QTY'+lcI) +;
                                        (EVALUATE(lcTmpCtktL+'.QTY'+lcI) * BOM.NBOMTOTQTY),;
                      REQ_QTY      WITH REQ_QTY + (EVALUATE(lcTmpCtktL+'.QTY'+lcI) * BOM.NBOMTOTQTY)
            ENDIF
            REPLACE ISS_QTY&lcI.  WITH EVALUATE('REQ_QTY'+lcI),;
                    USED_QTY&lcI. WITH EVALUATE('REQ_QTY'+lcI)
          ENDFOR
        ELSE
          REPLACE REQ_QTY WITH REQ_QTY + (EVALUATE(lcTmpCtktL+'.TOTQTY') * BOM.NBOMTOTQTY)
        ENDIF
        REPLACE ISSUE_QTY WITH REQ_QTY,;
                USED_QTY  WITH REQ_QTY,;
                UNTQTY    WITH IIF(PIECES<>0,REQ_QTY/PIECES,0)
      ENDIF
    ENDSCAN
  ENDIF
ENDSCAN
WAIT CLEAR

PRIVATE lcCarLbl,lcCareDsc1,lcCareDsc2,lcCareDsc3,lcCareDsc4,lcCarDesc5

PRIVATE lcPrmFab, lcPrmClr
STORE SPACE(0) TO lcPrmFab, lcPrmClr

lcCarlbl = ''
lcCareDsc1 = ''
lcCareDsc2 = ''
lcCareDsc3 = ''
lcCareDsc4 = ''
lcCareDsc5 = ''

PRIVATE lcRltColor
lcRltColor = SPACE(0)

DECLARE laCare1[1,2]
DECLARE laCare2[1,2]
DECLARE laCare3[1,2]
DECLARE laCare4[1,2]
DECLARE laCare5[1,2]

DECLARE laColor[1,2]
laColor[1,1] = 'CNRFCODE'
laColor[1,2] = 'lcRltColor'

*B605387,1 AMH Add array to get the longe description of color [Start]
DECLARE laLngDesc[1,2]
laLngDesc[1,1] = 'CLRLNAME'
laLngDesc[1,2] = 'lcRltColor'
*B605387,1 AMH [End]

laCare1[1,1] = 'CDESC1'
laCare1[1,2] = 'lcCareDsc1'
laCare2[1,1] = 'CDESC2'
laCare2[1,2] = 'lcCareDsc2'
laCare3[1,1] = 'CDESC3'
laCare3[1,2] = 'lcCareDsc3'
laCare4[1,1] = 'CDESC4'
laCare4[1,2] = 'lcCareDsc4'
laCare5[1,1] = 'CDESC5'
laCare5[1,2] = 'lcCareDsc5'

TmpCtTkt = gfTempName()

*B605242,1 AMH Change some fields width [Start]
*CREATE TABLE (gcWorkDir + TmpCtTkt);
       (cType C(2), cCutTkt C(6), cDivDesc C(30), cSewAt C(30), cMajor C(12),;
        cSeasDesc C(4), cSeasCod C(6), cCutAt C(30), cPackNo C(6), dComplete D,;
        cSalDesc1 C(50), cSalDesc2 C(50), cSalDesc3 C(50), cSalDesc4 C(50), cSalDesc5 C(50),;
        cCarLbl C(3), cCarDsc1 C(30), cCarDsc2 C(30), cCarDsc3 C(30), cCarDsc4 C(30), cCarDsc5 C(30),;
        cClrDesc C(30), nYards N(10), nPly N(3), cScale C(1), nQty1 N(6),;
        nQty2 N(6), nQty3 N(6),nQty4 N(6), nQty5 N(6), nQty6 N(6),;
        nQty7 N(6), nQty8 N(6), nTotQty N(7),cLineType C(10),nPieces N(10),;
        cItem C(30), cVendor C(30), cClr_Ref C(21), cFabDesc C(96), nUntQty N(7,3),;
        cUomUse C(3), nNeed N(8), cUomBuy C(3), nNum N(1,0), cSubNum C(1),;
        cFabDesc1 C(20), cFabDesc2 C(41), cPattern C(10), cMarker C(10), mNotes M, nLot_No N(4,0), cRev_No C(2),;
        cFabric C(7), cPosition C(10), cItem_Type C(6), cMarker2 C(10))

*B605387,1 AMH increasing the width of field cfabdesc to 112 [Start]
*CREATE TABLE (gcWorkDir + TmpCtTkt);
       (cType C(2), cCutTkt C(6), cDivDesc C(30), cSewAt C(30), cMajor C(12),;
        cSeasDesc C(4), cSeasCod C(6), cCutAt C(30), cPackNo C(6), dComplete D,;
        cSalDesc1 C(50), cSalDesc2 C(50), cSalDesc3 C(50), cSalDesc4 C(50), cSalDesc5 C(50),;
        cCarLbl C(3), cCarDsc1 C(50), cCarDsc2 C(50), cCarDsc3 C(50), cCarDsc4 C(50), cCarDsc5 C(50),;
        cClrDesc C(30), nYards N(10), nPly N(10,0), cScale C(1), nQty1 N(6),;
        nQty2 N(6), nQty3 N(6),nQty4 N(6), nQty5 N(6), nQty6 N(6),;
        nQty7 N(6), nQty8 N(6), nTotQty N(7),cLineType C(10),nPieces N(10),;
        cItem C(30), cVendor C(30), cClr_Ref C(21), cFabDesc C(96), nUntQty N(7,3),;
        cUomUse C(3), nNeed N(10,2), cUomBuy C(3), nNum N(1,0), cSubNum C(1),;
        cFabDesc1 C(20), cFabDesc2 C(41), cPattern C(10), cMarker C(10), mNotes M, nLot_No N(4,0), cRev_No C(2),;
        cFabric C(7), cPosition C(10), cItem_Type C(6), cMarker2 C(10))

*B605496,1 AMH Increase the cfabdesc2 width to 48 chr. [Start]
*CREATE TABLE (gcWorkDir + TmpCtTkt);
       (cType C(2), cCutTkt C(6), cDivDesc C(30), cSewAt C(30), cMajor C(12),;
        cSeasDesc C(4), cSeasCod C(6), cCutAt C(30), cPackNo C(6), dComplete D,;
        cSalDesc1 C(50), cSalDesc2 C(50), cSalDesc3 C(50), cSalDesc4 C(50), cSalDesc5 C(50),;
        cCarLbl C(3), cCarDsc1 C(50), cCarDsc2 C(50), cCarDsc3 C(50), cCarDsc4 C(50), cCarDsc5 C(50),;
        cClrDesc C(30), nYards N(10), nPly N(10,0), cScale C(1), nQty1 N(6),;
        nQty2 N(6), nQty3 N(6),nQty4 N(6), nQty5 N(6), nQty6 N(6),;
        nQty7 N(6), nQty8 N(6), nTotQty N(7),cLineType C(10),nPieces N(10),;
        cItem C(30), cVendor C(30), cClr_Ref C(21), cFabDesc C(112), nUntQty N(7,3),;
        cUomUse C(3), nNeed N(10,2), cUomBuy C(3), nNum N(1,0), cSubNum C(1),;
        cFabDesc1 C(20), cFabDesc2 C(41), cPattern C(10), cMarker C(10), mNotes M, nLot_No N(4,0), cRev_No C(2),;
        cFabric C(7), cPosition C(10), cItem_Type C(6), cMarker2 C(10))

*B605790,1 AMH Add New Field to sort fabrics by desc. [Start]
*CREATE TABLE (gcWorkDir + TmpCtTkt);
       (cType C(2), cCutTkt C(6), cDivDesc C(30), cSewAt C(30), cMajor C(12),;
        cSeasDesc C(4), cSeasCod C(6), cCutAt C(30), cPackNo C(6), dComplete D,;
        cSalDesc1 C(50), cSalDesc2 C(50), cSalDesc3 C(50), cSalDesc4 C(50), cSalDesc5 C(50),;
        cCarLbl C(3), cCarDsc1 C(50), cCarDsc2 C(50), cCarDsc3 C(50), cCarDsc4 C(50), cCarDsc5 C(50),;
        cClrDesc C(30), nYards N(10), nPly N(10,0), cScale C(1), nQty1 N(6),;
        nQty2 N(6), nQty3 N(6),nQty4 N(6), nQty5 N(6), nQty6 N(6),;
        nQty7 N(6), nQty8 N(6), nTotQty N(7),cLineType C(10),nPieces N(10),;
        cItem C(30), cVendor C(30), cClr_Ref C(21), cFabDesc C(112), nUntQty N(7,3),;
        cUomUse C(3), nNeed N(10,3), cUomBuy C(3), nNum N(1,0), cSubNum C(1),;
        cFabDesc1 C(20), cFabDesc2 C(48), cPattern C(10), cMarker C(10), mNotes M, nLot_No N(4,0), cRev_No C(2),;
        cFabric C(7), cPosition C(10), cItem_Type C(6), cMarker2 C(10))

*B606481,1 AMH Add nreplendec field [Start]
*CREATE TABLE (gcWorkDir + TmpCtTkt);
       (cType C(2), cCutTkt C(6), cDivDesc C(30), cSewAt C(30), cMajor C(12),;
        cSeasDesc C(4), cSeasCod C(6), cCutAt C(30), cPackNo C(6), dComplete D,;
        cSalDesc1 C(50), cSalDesc2 C(50), cSalDesc3 C(50), cSalDesc4 C(50), cSalDesc5 C(50),;
        cCarLbl C(3), cCarDsc1 C(50), cCarDsc2 C(50), cCarDsc3 C(50), cCarDsc4 C(50), cCarDsc5 C(50),;
        cClrDesc C(30), nYards N(10), nPly N(10,0), cScale C(1), nQty1 N(6),;
        nQty2 N(6), nQty3 N(6),nQty4 N(6), nQty5 N(6), nQty6 N(6),;
        nQty7 N(6), nQty8 N(6), nTotQty N(7),cLineType C(10),nPieces N(10),;
        cItem C(30), cVendor C(30), cClr_Ref C(21), cFabDesc C(112), nUntQty N(7,3),;
        cUomUse C(3), nNeed N(10,3), cUomBuy C(3), nNum N(1,0), cSubNum C(1),;
        cFabDesc1 C(20), cFabDesc2 C(48), cPattern C(10), cMarker C(10), mNotes M, nLot_No N(4,0), cRev_No C(2),;
        cFabric C(7), cPosition C(10), cItem_Type C(6), cMarker2 C(10), cSortDesc C(21))
CREATE TABLE (gcWorkDir + TmpCtTkt);
       (cType C(2), cCutTkt C(6), cDivDesc C(30), cSewAt C(30), cMajor C(12),;
        cSeasDesc C(4), cSeasCod C(6), cCutAt C(30), cPackNo C(6), dComplete D,;
        cSalDesc1 C(50), cSalDesc2 C(50), cSalDesc3 C(50), cSalDesc4 C(50), cSalDesc5 C(50),;
        cCarLbl C(3), cCarDsc1 C(50), cCarDsc2 C(50), cCarDsc3 C(50), cCarDsc4 C(50), cCarDsc5 C(50),;
        cClrDesc C(30), nYards N(10), nPly N(10,0), cScale C(1), nQty1 N(6),;
        nQty2 N(6), nQty3 N(6),nQty4 N(6), nQty5 N(6), nQty6 N(6),;
        nQty7 N(6), nQty8 N(6), nTotQty N(7),cLineType C(10),nPieces N(10),;
        cItem C(30), cVendor C(30), cClr_Ref C(21), cFabDesc C(112), nUntQty N(7,3),;
        cUomUse C(3), nNeed N(10,3), cUomBuy C(3), nNum N(1,0), cSubNum C(1),;
        cFabDesc1 C(20), cFabDesc2 C(48), cPattern C(10), cMarker C(10), mNotes M, nLot_No N(4,0), cRev_No C(2),;
        cFabric C(7), cPosition C(10), cItem_Type C(6), cMarker2 C(10), cSortDesc C(21),nRepLenDec N(9,3))
*B606481,1 AMH [End]

*B605790,1 AMH [End]

*B605496,1 AMH [End]

*B605387,1 AMH [End]

*B605242,1 AMH [End]

SELECT Style
SET ORDER TO cStyle
SELECT (lcTmpCtktB)
SET ORDER TO cTktyp
SET ORDER TO BOMITEM IN BOM

*C200293,1 AMH open PDMMRKSH & PDMMRKSL files [Start]
=gfOpenFile(gcDataDir+'PDMMRKSH','PDMMRKSH','SH')
=gfOpenFile(gcDataDir+'PDMMRKSL','PDMMRKSL','SH')
=gfOpenFile(gcDataDir+'PDMMARK','CMARKER','SH')
=gfOpenFile(gcDataDir+'PDMCUTMS','PDMCUTMS','SH')
*C200293,1 AMH [End]

WAIT WINDOW 'Please Stand By. Selecting records ....' NOWAIT

SELECT (lcTmpCtktH)
SCAN
  WAIT WINDOW "Select Cut ticket for style : " + ALLTRIM(STYLE) + ' ...' NOWAIT
  
  =SEEK(Style, "Style")
  lcPrmFab = STYLE.FABRIC
  lcPrmMrk = ''
  
  *-- Function to get the cut at and sew at names.
  STORE '' TO lcCutAt,lcSewAt,lcCarlbl,lcCareDsc1,lcCareDsc2,lcCareDsc3,;
              lcCareDsc4,lcCareDsc5
  =lfChkVend(@lcCutAt,@lcSewAt)  

  *-- Get season and division description
  lcSeasDesc  = SUBSTR(gfCodDes(SEASON , 'SEASON'),1,4)
  lcDivDesc = gfCodDes(CDIVISION , 'CDIVISION')

  *-- Get care label descriptions.
  =lfGetCare(@lcCarlbl , @lcCareDsc1 , @lcCareDsc2 , @lcCareDsc3 , @lcCareDsc4 , @lcCareDsc5)
  SELECT (TmpCtTkt)
  lnMemoWd = SET('MEMOWIDTH')
  SET MEMOWIDTH TO 50
  lcMajor    = SUBSTR(Style.cStyMajor,1,12)
  lcSeasCod  = EVALUATE(lcTmpCtktH+'.Season')
  lcPack     = EVALUATE(lcTmpCtktH+'.cPack')
  ldComplete = EVALUATE(lcTmpCtktH+'.Complete')
  lcSalDesc1 = MLINE(Style.mSalesDesc,1)
  lcSalDesc2 = MLINE(Style.mSalesDesc,2)
  lcSalDesc3 = MLINE(Style.mSalesDesc,3)
  lcSalDesc4 = MLINE(Style.mSalesDesc,4)
  lcSalDesc5 = MLINE(Style.mSalesDesc,5)  
  SET MEMOWIDTH TO lnMemoWd
  *-- Get BOM Lines
  =lfGetBom()
  *-- Get Cut Ticket Lines
  =lfGetCTLin()
  *-- Get Cut Ticket Cut With.
  INSERT INTO (TmpCtTkt) (cType,cCutTkt,cMajor,cPattern,cRev_No) VALUES;
                       ('F',EVALUATE(lcTmpCtktH+'.CutTkt'),SUBSTR(Style.cStyMajor,1,12),;
                       SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),;
                       RIGHT(ALLTRIM(STYLE.PATTERN),2))
  = lfReplace()
  INSERT INTO (TmpCtTkt) (cType,cCutTkt,cPattern) VALUES;
                         ('I',EVALUATE(lcTmpCtktH+'.CutTkt'),SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2))
  = lfReplace()
  
  *C200293,1 AMH Print sectioned marker data [Start]
  =lfGetSecLn()
  *C200293,1 AMH [End]
  
  *-- Get Cut Ticket NotePad
  IF SEEK('I'+EVALUATE(lcTmpCtktH+'.CUTTKT'),'NOTEPAD')
    INSERT INTO (TmpCtTkt);
         (cType,cCutTkt,mNotes) VALUES;
         ('N',EVALUATE(lcTmpCtktH+'.CutTkt'),Notepad.MNotes)
    = lfReplace()
  ENDIF
ENDSCAN
WAIT CLEAR

*C200293,1 AMH Close PDMMRKSH & PDMMRKSL files [Start]
USE IN PDMMRKSH
USE IN PDMMRKSL
USE IN PDMMARK
USE IN PDMCUTMS
*C200293,1 AMH [End]

SELECT (TmpCtTkt)
LOCATE
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  USE IN (TmpCtTkt)
  IF FILE(gcWorkDir  + TmpCtTkt  + '.DBF')
    ERASE (gcWorkDir + TmpCtTkt  + '.DBF')
  ENDIF
  llPoDisp = .F.
  SELECT (lcMainF)
  RETURN
ENDIF

USE IN (TmpCtTkt)

lcRpDtDir = gcDataDir
lcRpWrDir = gcWorkDir
lcRpSysDir = gcSysHome
lcRpRpDir = gcRepHome
lcRpImDir = gcImagDir
lcRpComp = gcAct_Comp
lcRpDevice = gcDevice
lcRpDefPth = gcDef_Path
lcRpTmpFle = TmpCtTkt
lcConnStr  = ""
SAVE TO (gcWorkDir+lcTmpMemo+'.MEM') ALL LIKE l?RP* 
lcCommLine = (gcWorkDir+lcTmpMemo+'.MEM')
lcLib=SYS(2004)+"foxtools.fll"
IF FILE(lcLib)
  SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
  SW_HIDE = 0
  lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")
  =EVALUATE("CALLFN("+STR(lnFnWinExec)+;
   ",gcRepHome+'MF\'+[MFPCTCA.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
   llPoDisp = .F.
   RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")
ELSE
  WAIT "LIBRARY NOT FOUND" WINDOW
  RETURN .F.
ENDIF

*:****************************************************************
*: Name        : lfgetcare
*: Developer   : AHMED MAHER (AMH)
*: Date        : 12/02/2001
*: Purpose     : Get the care label descriptions.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : 
*:****************************************************************
*: Example     : =lfgetcare()
*:****************************************************************
*
FUNCTION lfgetcare
PARAMETERS lccarlbl , lcCareDsc1 , lcCareDsc2 , lcCareDsc3 , lcCareDsc4 , lcCareDsc5
IF SEEK(Style, "Style") 
  lccarlbl=Style.cCarLbl
  IF !EMPTY(Style.cCarLbl)
    = gfRltFld(STYLE.cCarLbl , @laCare1 , 'CCARLBL')
    = gfRltFld(STYLE.cCarLbl , @laCare2 , 'CCARLBL')
    = gfRltFld(STYLE.cCarLbl , @laCare3 , 'CCARLBL')
    = gfRltFld(STYLE.cCarLbl , @laCare4 , 'CCARLBL')
    = gfRltFld(STYLE.cCarLbl , @laCare5 , 'CCARLBL')
  ENDIF
ENDIF
*--End oflfGetcare.

*:****************************************************************
*: Name        : lfChkVend
*: Developer   : AHMED MAHER (AMH)
*: Date        : 12/02/2001
*: Purpose     : Get Cut at & Sew at.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : 
*:****************************************************************
*: Example     : =lfChkVend()
*:****************************************************************
*
FUNCTION lfChkVend
PARAMETERS lcCutAt,lcSewAt
PRIVATE lnAlias
lnAlias = SELECT()
SELECT MFGOPRHD

*NEW AHMED [START]
*IF SEEK('M'+EVALUATE(lcTmpCtktH+'.CUTTKT')+'1 ') .AND. !lInHouse               
*  IF SEEK(cContCode,'ApVendor')
*    lcCutAt = ApVendor.cVenComp
*  ENDIF
*ENDIF
*IF SEEK('M'+EVALUATE(lcTmpCtktH+'.CUTTKT')+'2 ') .AND. !lInHouse               
*  IF SEEK(cContCode,'ApVendor')
*    lcSewAt = ALLTRIM(ApVendor.cVenComp)
*  ENDIF
*ENDIF
lnCount = 0
IF SEEK('M'+CUTTKTH.CUTTKT)
  SCAN REST WHILE Cimtyp+cTktNo+cOperSeq = 'M'+CUTTKTH.CUTTKT FOR !lInHouse
    lnCount = lnCount + 1
    IF lnCount = 1
      IF SEEK(cContCode,'ApVendor')
        lcCutAt = ApVendor.cVenComp
      ENDIF
    ENDIF
    IF lnCount = 2
      IF SEEK(cContCode,'ApVendor')
        lcSewAt = ALLTRIM(ApVendor.cVenComp)
      ENDIF
      EXIT
    ENDIF
  ENDSCAN
ENDIF
*NEW AHMED [END]

SELECT (lnAlias)
*-- End of lfChkVend.

*:****************************************************************
*: Name        : lfGetCTLin
*: Developer   : AHMED MAHER (AMH)
*: Date        : 12/02/2001
*: Purpose     : Function to get Cut Ticket Lines
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : 
*:****************************************************************
*: Example     : =lfGetCTLin()
*:****************************************************************
*
FUNCTION lfGetCTLin

*NEW AHMED [START]
*PRIVATE lcScale
PRIVATE lcScale, lcStyOrd
lcStyOrd = ORDER('Style')
SET ORDER TO Style IN Style
*NEW AHMED [END]

lcScale = ""
IF SEEK(EVALUATE(lcTmpCtktH+'.CutTkt'),lcTmpCtktL)
  SELECT (lcTmpCtktL)
  SCAN REST WHILE CutTkt+Style+Dyelot+TranCd = EVALUATE(lcTmpCtktH+'.CutTkt') FOR TranCd = '1'
    *-- Get Color Description
    lcColor = SUBSTR(Style,14,6)
    lcColorDes = gfCodDes(lcColor, 'COLOR')
    
    *B605388,1 AMH Changing the printing of fabric color [Start]
    *IF SEEK(CutTktL.Style,'Style')
    *  lcColorDes = ALLTRIM(lcColorDes) + IIF(!EMPTY(Style.Location)," "+ALLTRIM(Style.Location),'')
    *ENDIF
    lcClr_Ref = ''
    lnI = 0
    FOR lnI = 1 TO 6
      IF BETWEEN(ASC(SUBSTR(lcColor,lnI,1)),48,57)
        lcClr_Ref = lcClr_Ref + SUBSTR(lcColor,lnI,1)
      ENDIF
    ENDFOR
    lcColorDes = ALLTRIM(lcColorDes) + ' ' + IIF(VAL(lcClr_Ref) = 0,'',ALLTRIM(STR(VAL(lcClr_Ref),6,0)))
    *B605388,1 AMH [End]
    
    SCATTER MEMVAR MEMO
    SELECT (TmpCtTkt)  
    INSERT INTO (TmpCtTkt) FROM MEMVAR
    REPLACE cType    WITH '5',;
    		cCutTkt  WITH EVALUATE(lcTmpCtktH+'.CutTkt'),;
            cClrDesc WITH lcColorDes,;
            cPattern WITH SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),;
            cRev_No  WITH RIGHT(ALLTRIM(STYLE.PATTERN),2),;
            cMarker  WITH lcPrmMrk
    = lfReplace()
    FOR lnCountr = 1 TO 8
      lcCountr = STR(lnCountr,1)
      REPLACE nQty&lcCountr. WITH m.Qty&lcCountr
    ENDFOR
    lcScale = Style.Scale
    REPLACE nTotQty   WITH nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8,;
            cLineType WITH "PROPOSED";
            cScale    WITH lcScale
    *-- Function to get the yards qty.cc
    lnPieces = 0
    lnYards  = lfGetYrds()
    REPLACE nYards WITH lnYards
    REPLACE nPieces WITH lnPieces
    
    *B605413,1 AMH Save the correct marker of this line [Start]
    REPLACE cMarker WITH lcPrmMrk
    *B605413,1 AMH [End]
    
    *-- Insert a record for the Actual.
  INSERT INTO (TmpCtTkt);
       (cType,cCutTkt,cLineType,cScale,cPattern,cRev_No,cMarker) VALUES;
       ('5',EVALUATE(lcTmpCtktH+'.CutTkt'),"ACTUAL",lcScale,SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),RIGHT(ALLTRIM(STYLE.PATTERN),2),lcPrmMrk)
    = lfReplace()
    SELECT (lcTmpCtktL)
  ENDSCAN
ENDIF

*NEW AHMED [START]
SET ORDER TO lcStyOrd IN Style
*NEW AHMED [START]

*--End of lfGetCTLin.

******************************************************************
*: Name        : lfGetYrds
*: Developer   : AHMED MAHER (AMH)
*: Date        : 12/02/2001
*: Purpose     : Get yPerD.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : 
*:****************************************************************
*: Example     : =lfGetYrds()
*:****************************************************************
*
FUNCTION lfGetYrds

*B605413,1 AMH Add variable to save the C/T color [Start]
*PRIVATE lnRetValue, lnAlias
PRIVATE lnRetValue, lnAlias, lcColor, lcOrder
lcColor = SUBSTR(STYLE,14,6)
*B605413,1 AMH [End]

lnAlias = SELECT(0)
lnRetValue = 0

*B605413,1 AMH Get the SELF color from BOM file [Start]
SELECT BOM
lcOrder = SET('ORDER')
SET ORDER TO TAG BOM
IF SEEK(SUBSTR(STYLE,1,12))
  LOCATE REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = SUBSTR(STYLE,1,12);
              FOR ALLTRIM(UPPER(DESC))='SELF'
  IF FOUND()
    DO CASE
      CASE SUBSTR(CITMMASK,14,6) = '******' .AND. ICLR # '******'  && Constant color case.
        lcColor = ICLR
      CASE SUBSTR(CITMMASK,14,6) # '******'                        && Differant colors case.
        =SEEK(SUBSTR(STYLE,1,12))
        LOCATE REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = SUBSTR(STYLE,1,12);
                    FOR SUBSTR(CITMMASK,14,6) = lcColor .AND. ALLTRIM(UPPER(DESC))='SELF'
        IF FOUND()
          lcColor = ICLR
        ENDIF
    ENDCASE
  ENDIF
ENDIF
SET ORDER TO &lcOrder.
*IF SEEK('M'+EVALUATE(lcTmpCtktH+'.CUTTKT')+PADR(lcPrmFab,19)+SUBSTR(EVALUATE(lcTmpCtktL+'.Style'),14,6),lcTmpCtktB)
IF SEEK('M'+EVALUATE(lcTmpCtktH+'.CUTTKT')+PADR(lcPrmFab,19)+lcColor,lcTmpCtktB)
   SELECT (lcTmpCtktB)
   lnPieces   = Pieces
   lnUntQty   = UntQty
   lnRetValue = Pieces * lnUntQty
   lcPrmMrk   = CMARKER
ENDIF
*B605413,1 AMH [End]

SELECT(lnAlias)
RETURN lnRetValue

******************************************************************
*: Name        : lfGetBom
*: Developer   : Ahmed Maher (AMH)
*: Date        : 12/02/2001
*: Purpose     : Get Bom Lines.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : 
*:****************************************************************
*: Example     : =lfGetBom()
*:****************************************************************
*
FUNCTION lfGetBom

PRIVATE nConv,lcItem, lcVendor, lcClr_Des, lcFabDesc, lcUomUse, lcUomBuy, lnPieces, lnUntQty, lnNeed, lcClr_Ref
STORE '' to lcItem, lcClr_Des, lcFabDesc, lcUomUse, lcUomBuy, lcClr_Ref, lcVendor
STORE 0 to nConv , lnPieces, lnUntQty, lnNeed
llPrmFound = .F.
SELECT FABRIC
lcOrder = SET('ORDER')
SET ORDER TO FABRIC
IF SEEK('M'+EVALUATE(lcTmpCtktH+'.CutTkt'),lcTmpCtktB)
  SELECT (lcTmpCtktB)
  lnNum = 1
  lnFrsRec = RECNO()
  LOCATE REST WHILE cImtyp+Cuttkt+Item+Iclr+Mfgcode+Dyelot = 'M'+EVALUATE(lcTmpCtktH+'.CUTTKT');
         FOR CCATGTYP = 'F'
  IF FOUND()
    lnSubNum = 65
    lnSubNum1 = 65
    lnFabRec = RECNO()
    IF !EMPTY(lcPrmFab)
      =SEEK('M'+EVALUATE(lcTmpCtktH+'.CUTTKT')+PADR(lcPrmFab,19))
      LOCATE REST WHILE cImtyp+Cuttkt+Item+Iclr+Mfgcode+Dyelot = 'M'+EVALUATE(lcTmpCtktH+'.CUTTKT')+PADR(lcPrmFab,19);
             FOR CCATGTYP = 'F' .AND. SUBSTR(ITEM,1,1) <> 'L'
      IF FOUND()
        lcPrmClr = ''
        
        *new ahmed [start]
        *SCAN REST WHILE cImtyp+Cuttkt+Item+Iclr+Mfgcode+Dyelot = 'M'+EVALUATE(lcTmpCtktH+'.CUTTKT')+PADR(lcPrmFab,19);
             FOR CCATGTYP = 'F' .AND. SUBSTR(ITEM,1,1) <> 'L'
        SCAN REST WHILE cImtyp+Cuttkt+Item+Iclr+Mfgcode+Dyelot = 'M'+EVALUATE(lcTmpCtktH+'.CUTTKT')+PADR(lcPrmFab,19);
             FOR CCATGTYP = 'F' .AND. ALLTRIM(UPPER(DESC))='SELF' .AND. SUBSTR(ITEM,1,1) <> 'L'
        *new ahmed [end]
        
          IF SEEK(PADR(ITEM,7)+ICLR,'FABRIC')
            lcClr_des = gfCodDes(EVALUATE(lcTmpCtktB+'.Iclr') , 'COLOR     ')
            
            *NEW AHMED [START]
            *= gfRltFld(EVALUATE(lcTmpCtktB+'.ICLR') , @laColor , 'COLOR     ')
            *lcClr_Ref = SUBSTR(UPPER(TRIM(lcClr_Des)+' '+TRIM(lcRltColor)),1,21)
            
            *B605388,1 AMH Changing the printing of fabric color [Start]
            *lcClr_Ref = SUBSTR(UPPER(TRIM(lcClr_Des)+' '+TRIM(Fabric.cClr_Ref)),1,21)
            lcClr_Ref = ''
            lnI = 0
            FOR lnI = 1 TO 6
              IF BETWEEN(ASC(SUBSTR(FABRIC.COLOR,lnI,1)),48,57)
                lcClr_Ref = lcClr_Ref + SUBSTR(FABRIC.COLOR,lnI,1)
              ENDIF
            ENDFOR
            lcClr_Ref = SUBSTR(UPPER(ALLTRIM(lcClr_Des)+' ' + IIF(VAL(lcClr_Ref)=0,'',ALLTRIM(STR(VAL(lcClr_Ref),6,0)))),1,21)
            *B605388,1 AMH [End]
            
            *NEW AHMED [END]
            
            lcPrmClr = IIF(EMPTY(lcPrmClr),EVALUATE(lcTmpCtktB+'.ICLR'),lcPrmClr)
            lcCurClr = EVALUATE(lcTmpCtktB+'.ICLR')
            llPrmFound = .T.
            lcMarker = CMARKER
            lcPrmMrk = lcMarker
            IF lcPrmClr = lcCurClr
              
              *NEW AHMED [START]
              *INSERT INTO (TmpCtTkt);
                          (cType,cCuttkt,nNum,cSubNum,cFabDesc,cFabDesc1) VALUES;
                          ('2',EVALUATE(lcTmpCtktB+'.CUTTKT'),lnNum,CHR(lnSubNum1),SUBSTR(UPPER(TRIM(FABRIC.CONTENT))+;
                          IIF(EMPTY(FABRIC.CONTENT),'',' ')+UPPER(TRIM(FABRIC.DESC))+;
                          IIF(EMPTY(FABRIC.DESC),'',' ')+IIF(EMPTY(FABRIC.PATTERN),'',;
                          'PATTERN #'+UPPER(TRIM(FABRIC.PATTERN))),1,96),'SELF')
              
              *B605387,1 AMH Changing the printing of fabric description [Start]
              *INSERT INTO (TmpCtTkt);
                          (cType,cCuttkt,nNum,cSubNum,cFabDesc,cFabDesc1) VALUES;
                          ('2',EVALUATE(lcTmpCtktB+'.CUTTKT'),lnNum,CHR(lnSubNum1),SUBSTR(UPPER(TRIM(FABRIC.CONTENT))+;
                          IIF(EMPTY(FABRIC.CONTENT),'',' ')+IIF(EMPTY(Fabric.cOldDesc),UPPER(ALLTRIM(FABRIC.DESC)),UPPER(ALLTRIM(FABRIC.cOldDesc)))+;
                          IIF(EMPTY(FABRIC.DESC) .AND. EMPTY(Fabric.cOldDesc),'',' ')+IIF(EMPTY(SUBSTR(FABRIC.CITEM,13)),'','PATTERN #'+UPPER(ALLTRIM(SUBSTR(FABRIC.CITEM,13)))+' ')+;
                          IIF(!EMPTY(FABRIC.cOldPatt),UPPER(ALLTRIM(FABRIC.COLDPATT)),UPPER(ALLTRIM(FABRIC.PATTERN))),1,96),'SELF')
              
              *B605448,1 AMH Print Color long description in a condition of fabric's clr [Start]
              *= gfRltFld(EVALUATE(lcTmpCtktB+'.ICLR') , @laLngDesc , 'COLOR     ')
              IF BETWEEN(ASC(LEFT(EVALUATE(lcTmpCtktB+'.ICLR'),1)),48,57)
                = gfRltFld(EVALUATE(lcTmpCtktB+'.ICLR') , @laLngDesc , 'COLOR     ')
              ELSE
                lcRltColor = ''
              ENDIF
              *B605448,1 AMH [End]
              
              *B605790,1 AMH Add New Field to sort fabrics by desc. [Start]
              *INSERT INTO (TmpCtTkt);
                          (cType,cCuttkt,nNum,cSubNum,cFabDesc,cFabDesc1) VALUES;
                          ('2',EVALUATE(lcTmpCtktB+'.CUTTKT'),lnNum,CHR(lnSubNum1),SUBSTR(UPPER(TRIM(Fabric.Desc))+;
                          IIF(EMPTY(Fabric.Desc),'',' ')+UPPER(ALLTRIM(Fabric.Content))+;
                          IIF(EMPTY(Fabric.Content),'',' ')+UPPER(ALLTRIM(Fabric.cFabWeight))+;
                          IIF(EMPTY(Fabric.cFabWeight),'',' ')+UPPER(ALLTRIM(Fabric.Width))+;
                          IIF(EMPTY(Fabric.Width),'',' ')+SUBSTR(UPPER(ALLTRIM(lcRltColor)),1,30)+;
                          IIF(EMPTY(lcRltColor),'',' ')+UPPER(ALLTRIM(Fabric.Pattern)),1,112),'SELF')
              INSERT INTO (TmpCtTkt);
                          (cType,cCuttkt,nNum,cSubNum,cFabDesc,cFabDesc1,cSortDesc) VALUES;
                          ('2',EVALUATE(lcTmpCtktB+'.CUTTKT'),lnNum,CHR(lnSubNum1),SUBSTR(UPPER(TRIM(Fabric.Desc))+;
                          IIF(EMPTY(Fabric.Desc),'',' ')+UPPER(ALLTRIM(Fabric.Content))+;
                          IIF(EMPTY(Fabric.Content),'',' ')+UPPER(ALLTRIM(Fabric.cFabWeight))+;
                          IIF(EMPTY(Fabric.cFabWeight),'',' ')+UPPER(ALLTRIM(Fabric.Width))+;
                          IIF(EMPTY(Fabric.Width),'',' ')+SUBSTR(UPPER(ALLTRIM(lcRltColor)),1,30)+;
                          IIF(EMPTY(lcRltColor),'',' ')+UPPER(ALLTRIM(Fabric.Pattern)),1,112),'SELF','1SELF')
              *B605790,1 AMH [End]
              
              *B605387,1 AMH [End]
              
              *NEW AHMED [END]
              
              = lfReplace()
              
              *B605790,1 AMH Add New Field to sort fabrics by desc. [Start]
              *INSERT INTO (TmpCtTkt);
                          (cType,cCuttkt,nNum,cSubNum,cFabDesc1,cPattern,cRev_No,cMarker,;
                          cClr_Ref,nPieces,nUntQty,cFabric) VALUES;
                          ('2M',EVALUATE(lcTmpCtktB+'.CUTTKT'),lnNum,CHR(lnSubNum1),'SELF',;
                           SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),;
                           RIGHT(ALLTRIM(STYLE.PATTERN),2),lcMarker,lcClr_Ref,EVALUATE(lcTmpCtktB+'.PIECES'),EVALUATE(lcTmpCtktB+'.UNTQTY'),"")
              INSERT INTO (TmpCtTkt);
                          (cType,cCuttkt,nNum,cSubNum,cFabDesc1,cSortDesc,cPattern,cRev_No,cMarker,;
                          cClr_Ref,nPieces,nUntQty,cFabric) VALUES;
                          ('2M',EVALUATE(lcTmpCtktB+'.CUTTKT'),lnNum,CHR(lnSubNum1),'SELF','1SELF',;
                           SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),;
                           RIGHT(ALLTRIM(STYLE.PATTERN),2),lcMarker,lcClr_Ref,EVALUATE(lcTmpCtktB+'.PIECES'),EVALUATE(lcTmpCtktB+'.UNTQTY'),"")
              *B605790,1 AMH [End]
              
              = lfReplace()
              lnSubNum1 = lnSubNum1 + 1
            ELSE
              
              *B605790,1 AMH Add New Field to sort fabrics by desc. [Start]
              *INSERT INTO (TmpCtTkt);
                          (cType,cCuttkt,cPattern,cRev_No,cMarker,nNum,cSubNum,cClr_Ref,nPieces,nUntQty,cFabric) VALUES;
                          ('2M',EVALUATE(lcTmpCtktB+'.CUTTKT'),SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),;
                           RIGHT(ALLTRIM(STYLE.PATTERN),2),lcMarker,lnNum,' ',lcClr_Ref,EVALUATE(lcTmpCtktB+'.PIECES'),EVALUATE(lcTmpCtktB+'.UNTQTY'),"")
              INSERT INTO (TmpCtTkt);
                          (cType,cCuttkt,cPattern,cRev_No,cMarker,nNum,cSubNum,cClr_Ref,nPieces,nUntQty,cFabric,cSortDesc) VALUES;
                          ('2M',EVALUATE(lcTmpCtktB+'.CUTTKT'),SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),;
                           RIGHT(ALLTRIM(STYLE.PATTERN),2),lcMarker,lnNum,' ',lcClr_Ref,EVALUATE(lcTmpCtktB+'.PIECES'),EVALUATE(lcTmpCtktB+'.UNTQTY'),"",'1SELF')
              *B605790,1 AMH [End]
              
              = lfReplace()
            ENDIF
            lnSubNum = lnSubNum + 1
          ENDIF
        ENDSCAN
      ENDIF
    ENDIF
    GOTO lnFabRec
    lcNewItem = SPACE(19)
    lcNewDesc = SPACE(20)
    SCAN REST WHILE cImtyp+Cuttkt+Item+Iclr+Mfgcode+Dyelot = 'M'+EVALUATE(lcTmpCtktH+'.CUTTKT');
              FOR CCATGTYP = 'F' .AND. SUBSTR(ITEM,1,1) <> 'L'
      llNewItem = ((ITEM<>PADR(lcPrmFab,19)) .OR. ALLTRIM(UPPER(DESC))<>'SELF') .AND. (ITEM+DESC<>lcNewItem+lcNewDesc)
      lcNewItem = ITEM
      lcNewDesc = DESC
      IF SEEK(PADR(ITEM,7)+ICLR,'FABRIC')
        lcClr_des = gfCodDes(EVALUATE(lcTmpCtktB+'.Iclr') , 'COLOR     ')
        
        *NEW AHMED [START]
        *= gfRltFld(EVALUATE(lcTmpCtktB+'.ICLR') , @laColor , 'COLOR     ')
        *lcClr_Ref = SUBSTR(UPPER(TRIM(lcClr_Des)+' '+TRIM(lcRltColor)),1,21)
        
        *B605388,1 AMH Changing the printing of fabric color [Start]
        *lcClr_Ref = SUBSTR(UPPER(TRIM(lcClr_Des)+' '+TRIM(Fabric.cClr_Ref)),1,21)
        lcClr_Ref = ''
        lnI = 0
        FOR lnI = 1 TO 6
          IF BETWEEN(ASC(SUBSTR(FABRIC.COLOR,lnI,1)),48,57)
            lcClr_Ref = lcClr_Ref + SUBSTR(FABRIC.COLOR,lnI,1)
          ENDIF
        ENDFOR
        lcClr_Ref = SUBSTR(UPPER(ALLTRIM(lcClr_Des)+' ' + IIF(VAL(lcClr_Ref) = 0,'',ALLTRIM(STR(VAL(lcClr_Ref),6,0)))),1,21)
        *B605388,1 AMH [End]
        
        lcMarker = CMARKER
        *NEW AHMED [END]
        
        IF llNewItem
          
          *NEW AHMED [START]
          *lcMarker = CMARKER
          *INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,nNum,cSubNum,cFabDesc,cFabDesc1) VALUES;
                      ('2',EVALUATE(lcTmpCtktB+'.CUTTKT'),lnNum,CHR(lnSubNum1),SUBSTR(UPPER(TRIM(FABRIC.CONTENT))+;
                      IIF(EMPTY(FABRIC.CONTENT),'',' ')+UPPER(TRIM(FABRIC.DESC))+;
                      IIF(EMPTY(FABRIC.DESC),'',' ')+IIF(EMPTY(FABRIC.PATTERN),'',;
                      'PATTERN #'+UPPER(TRIM(FABRIC.PATTERN))),1,96),;
                      UPPER(SUBSTR(ALLTRIM(EVALUATE(lcTmpCtktB+'.DESC')),1,20)))
          
          *B605387,1 AMH Changing the printing of fabric description [Start]
          *INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,nNum,cSubNum,cFabDesc,cFabDesc1) VALUES;
                      ('2',EVALUATE(lcTmpCtktB+'.CUTTKT'),lnNum,CHR(lnSubNum1),SUBSTR(UPPER(TRIM(FABRIC.CONTENT))+;
                      IIF(EMPTY(FABRIC.CONTENT),'',' ')+IIF(EMPTY(Fabric.cOldDesc),UPPER(TRIM(FABRIC.DESC)),UPPER(ALLTRIM(FABRIC.cOldDesc)))+;
                      IIF(EMPTY(FABRIC.DESC) .AND. EMPTY(Fabric.cOldDesc),'',' ')+IIF(EMPTY(SUBSTR(FABRIC.CITEM,13)),'','PATTERN #'+UPPER(ALLTRIM(SUBSTR(FABRIC.CITEM,13)))+' ')+;
                      IIF(!EMPTY(FABRIC.cOldPatt),UPPER(ALLTRIM(FABRIC.COLDPATT)),UPPER(ALLTRIM(FABRIC.PATTERN))),1,96),UPPER(SUBSTR(ALLTRIM(CTKTBOM.DESC),1,20)))
          
          *B605448,1 AMH Print Color long description in a condition of fabric's clr [Start]
          *= gfRltFld(EVALUATE(lcTmpCtktB+'.ICLR') , @laLngDesc , 'COLOR     ')
          IF BETWEEN(ASC(LEFT(EVALUATE(lcTmpCtktB+'.ICLR'),1)),48,57)
            = gfRltFld(EVALUATE(lcTmpCtktB+'.ICLR') , @laLngDesc , 'COLOR     ')
          ELSE
            lcRltColor = ''
          ENDIF
          *B605448,1 AMH [End]
          
          *B605790,1 AMH Add New Field to sort fabrics by desc. [Start]
          *INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,nNum,cSubNum,cFabDesc,cFabDesc1) VALUES;
                      ('2',EVALUATE(lcTmpCtktB+'.CUTTKT'),lnNum,CHR(lnSubNum1),SUBSTR(UPPER(TRIM(Fabric.Desc))+;
                      IIF(EMPTY(Fabric.Desc),'',' ')+UPPER(ALLTRIM(Fabric.Content))+;
                      IIF(EMPTY(Fabric.Content),'',' ')+UPPER(ALLTRIM(Fabric.cFabWeight))+;
                      IIF(EMPTY(Fabric.cFabWeight),'',' ')+UPPER(ALLTRIM(Fabric.Width))+;
                      IIF(EMPTY(Fabric.Width),'',' ')+SUBSTR(UPPER(ALLTRIM(lcRltColor)),1,30)+;
                      IIF(EMPTY(lcRltColor),'',' ')+UPPER(ALLTRIM(Fabric.Pattern)),1,112),UPPER(SUBSTR(ALLTRIM(EVALUATE(lcTmpCtktB+'.DESC')),1,20)))
          INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,nNum,cSubNum,cFabDesc,cFabDesc1,cSortDesc) VALUES;
                      ('2',EVALUATE(lcTmpCtktB+'.CUTTKT'),lnNum,CHR(lnSubNum1),SUBSTR(UPPER(TRIM(Fabric.Desc))+;
                      IIF(EMPTY(Fabric.Desc),'',' ')+UPPER(ALLTRIM(Fabric.Content))+;
                      IIF(EMPTY(Fabric.Content),'',' ')+UPPER(ALLTRIM(Fabric.cFabWeight))+;
                      IIF(EMPTY(Fabric.cFabWeight),'',' ')+UPPER(ALLTRIM(Fabric.Width))+;
                      IIF(EMPTY(Fabric.Width),'',' ')+SUBSTR(UPPER(ALLTRIM(lcRltColor)),1,30)+;
                      IIF(EMPTY(lcRltColor),'',' ')+UPPER(ALLTRIM(Fabric.Pattern)),1,112),;
                      UPPER(SUBSTR(ALLTRIM(EVALUATE(lcTmpCtktB+'.DESC')),1,20)),;
                      lfGetSort()+UPPER(SUBSTR(ALLTRIM(EVALUATE(lcTmpCtktB+'.DESC')),1,20)))
          *B605790,1 AMH [End]
          
          *B605387,1 AMH [End]
          
          *NEW AHMED [END]
          
          = lfReplace()
          
          *B605790,1 AMH Add New Field to sort fabrics by desc. [Start]
          *INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,nNum,cSubNum,cFabDesc1,cPattern,cRev_No,cMarker,;
                      cClr_Ref,nPieces,nUntQty,cFabric) VALUES;
                      ('2M',EVALUATE(lcTmpCtktH+'.CUTTKT'),lnNum,CHR(lnSubNum1),UPPER(SUBSTR(ALLTRIM(EVALUATE(lcTmpCtktB+'.DESC')),1,9)),;
                       SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),;
                       RIGHT(ALLTRIM(STYLE.PATTERN),2),lcMarker,lcClr_Ref,EVALUATE(lcTmpCtktB+'.PIECES'),EVALUATE(lcTmpCtktB+'.UNTQTY'),SUBSTR(EVALUATE(lcTmpCtktB+'.ITEM'),1,7))
          INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,nNum,cSubNum,cFabDesc1,cSortDesc,cPattern,cRev_No,cMarker,;
                      cClr_Ref,nPieces,nUntQty,cFabric) VALUES;
                      ('2M',EVALUATE(lcTmpCtktH+'.CUTTKT'),lnNum,CHR(lnSubNum1),UPPER(SUBSTR(ALLTRIM(EVALUATE(lcTmpCtktB+'.DESC')),1,9)),;
                       lfGetSort()+UPPER(SUBSTR(ALLTRIM(EVALUATE(lcTmpCtktB+'.DESC')),1,9)),SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),;
                       RIGHT(ALLTRIM(STYLE.PATTERN),2),lcMarker,lcClr_Ref,EVALUATE(lcTmpCtktB+'.PIECES'),EVALUATE(lcTmpCtktB+'.UNTQTY'),SUBSTR(EVALUATE(lcTmpCtktB+'.ITEM'),1,7))
          *B605790,1 AMH [End]
          
          = lfReplace()
          lnSubNum1 = lnSubNum1 + 1
        ELSE
          
          *NEW AHMED [START]
          IF ITEM<>PADR(lcPrmFab,19) .OR. ALLTRIM(UPPER(DESC))<>'SELF'
          *NEW AHMED [END]
          
          *B605790,1 AMH Add New Field to sort fabrics by desc. [Start]
          *INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,cPattern,cRev_No,cMarker,nNum,cSubNum,cClr_Ref,nPieces,nUntQty,cFabric) VALUES;
                      ('2M',EVALUATE(lcTmpCtktB+'.CUTTKT'),SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),;
                       RIGHT(ALLTRIM(STYLE.PATTERN),2),lcMarker,lnNum,' ',lcClr_Ref,EVALUATE(lcTmpCtktB+'.PIECES'),EVALUATE(lcTmpCtktB+'.UNTQTY'),SUBSTR(EVALUATE(lcTmpCtktB+'.ITEM'),1,7))
          INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,cPattern,cRev_No,cMarker,nNum,cSubNum,cClr_Ref,nPieces,nUntQty,cFabric,cSortDesc) VALUES;
                      ('2M',EVALUATE(lcTmpCtktB+'.CUTTKT'),SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),;
                       RIGHT(ALLTRIM(STYLE.PATTERN),2),lcMarker,lnNum,' ',lcClr_Ref,EVALUATE(lcTmpCtktB+'.PIECES'),EVALUATE(lcTmpCtktB+'.UNTQTY'),SUBSTR(EVALUATE(lcTmpCtktB+'.ITEM'),1,7),;
                       lfGetSort()+UPPER(SUBSTR(ALLTRIM(EVALUATE(lcTmpCtktB+'.DESC')),1,9)))
          *B605790,1 AMH [End]
          
          = lfReplace()
          
          *NEW AHMED [START]
          ENDIF
          *NEW AHMED [END]
          
        ENDIF
        lnSubNum = lnSubNum + 1
      ENDIF
    ENDSCAN
    lnNum = lnNum + 1
  ENDIF
  GOTO lnFrsRec
  LOCATE REST WHILE cImtyp+Cuttkt+Item+Iclr+Mfgcode+Dyelot = 'M'+EVALUATE(lcTmpCtktH+'.CUTTKT');
         FOR CCATGTYP = 'F' .AND. SUBSTR(ITEM,1,1) = 'L'
  IF FOUND()
    STORE 65 TO lnSubNum , lnSubNum1
    lcNewItem = ''
    SCAN REST WHILE cImtyp+Cuttkt+Item+Iclr+Mfgcode+Dyelot = 'M'+EVALUATE(lcTmpCtktH+'.CUTTKT')+'L';
              FOR CCATGTYP = 'F'
      llNewItem = IIF(lcNewItem=ITEM,.F.,.T.)
      lcNewItem = ITEM
      IF SEEK(PADR(ITEM,7)+ICLR,'FABRIC')
        =SEEK(FABRIC.VENDOR,'APVENDOR')
        lcClr_des = gfCodDes(EVALUATE(lcTmpCtktB+'.Iclr') , 'COLOR     ')
        
        *NEW AHMED [START]
        *= gfRltFld(EVALUATE(lcTmpCtktB+'.ICLR') , @laColor , 'COLOR     ')
        *lcClr_Ref = SUBSTR(UPPER(TRIM(lcClr_Des)+' '+TRIM(lcRltColor)),1,21)
        lcClr_Ref = SUBSTR(UPPER(TRIM(lcClr_Des)+' '+TRIM(Fabric.cClr_Ref)),1,21)
        lcMarker = CMARKER
        *NEW AHMED [END]
        
        IF llNewItem
          
          *NEW AHMED [START]
          *lcMarker = CMARKER
          *INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,nNum,cSubNum,cFabDesc,cVendor) VALUES;
                      ('3',EVALUATE(lcTmpCtktB+'.CUTTKT'),lnNum,CHR(lnSubNum1),SUBSTR(UPPER(TRIM(FABRIC.CONTENT))+;
                      IIF(EMPTY(FABRIC.CONTENT),'',' ')+UPPER(TRIM(FABRIC.DESC))+IIF(EMPTY(FABRIC.DESC),'',' ')+;
                      UPPER(TRIM(FABRIC.PATTERN)),1,96),APVENDOR.CSHRTNAM)
          INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,nNum,cSubNum,cFabDesc,cVendor) VALUES;
                      ('3',EVALUATE(lcTmpCtktB+'.CUTTKT'),lnNum,CHR(lnSubNum1),SUBSTR(UPPER(TRIM(FABRIC.CONTENT))+;
                      IIF(EMPTY(FABRIC.CONTENT),'',' ')+IIF(EMPTY(Fabric.cOldDesc),UPPER(TRIM(FABRIC.DESC)),UPPER(TRIM(FABRIC.cOldDesc)))+;
                      IIF(EMPTY(FABRIC.DESC) .AND. EMPTY(Fabric.cOldDesc),'',' ')+IIF(!EMPTY(FABRIC.cOldPatt),UPPER(ALLTRIM(FABRIC.COLDPATT)),UPPER(ALLTRIM(FABRIC.PATTERN))),1,96),APVENDOR.CSHRTNAM)
          *NEW AHMED [END]
          
          = lfReplace()
          INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,nNum,cSubNum,cFabDesc1,cPattern,cRev_No,cMarker,;
                      cClr_Ref,nPieces,nUntQty,cfabric) VALUES;
                      ('3M',EVALUATE(lcTmpCtktB+'.CUTTKT'),lnNum,CHR(lnSubNum),APVENDOR.CSHRTNAM,;
                       SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),;
                       RIGHT(ALLTRIM(STYLE.PATTERN),2),lcMarker,lcClr_Ref,EVALUATE(lcTmpCtktB+'.PIECES'),EVALUATE(lcTmpCtktB+'.UNTQTY'),SUBSTR(EVALUATE(lcTmpCtktB+'.ITEM'),1,7))
          = lfReplace()
          lnSubNum1 = lnSubNum1 + 1
        ELSE
          INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,cPattern,cRev_No,cMarker,nNum,cSubNum,cClr_Ref,nPieces,nUntQty,cVendor,cFabric) VALUES;
                      ('3M',EVALUATE(lcTmpCtktB+'.CUTTKT'),SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),;
                       RIGHT(ALLTRIM(STYLE.PATTERN),2),lcMarker,lnNum,CHR(lnSubNum),;
                      lcClr_Ref,EVALUATE(lcTmpCtktB+'.PIECES'),EVALUATE(lcTmpCtktB+'.UNTQTY'),APVENDOR.CSHRTNAM,SUBSTR(EVALUATE(lcTmpCtktB+'.ITEM'),1,7))
          = lfReplace()
        ENDIF
        lnSubNum = lnSubNum + 1
      ENDIF
    ENDSCAN
    lnNum = lnNum + 1
  ENDIF
  GOTO lnFrsRec
  LOCATE REST WHILE cImtyp+Cuttkt+Item+Iclr+Mfgcode+Dyelot = 'M'+EVALUATE(lcTmpCtktH+'.CUTTKT');
         FOR CCATGTYP = 'T'
  IF FOUND()
    SCAN REST WHILE cImtyp+Cuttkt+Item+Iclr+Mfgcode+Dyelot = 'M'+EVALUATE(lcTmpCtktH+'.CUTTKT');
              FOR CCATGTYP = 'T'
      SELECT FABRIC
      IF SEEK(PADR(EVALUATE(lcTmpCtktB+'.ITEM'),7)+EVALUATE(lcTmpCtktB+'.ICLR')) .AND. LLTKT_PRT = 'Y'
        =SEEK(VENDOR,'APVENDOR')
        
        *NEW AHMED [START]
        *IF EVALUATE(lcTmpCtktB+'.ISSUE_QTY') <> 0
        IF EVALUATE(lcTmpCtktB+'.ISSUE_QTY') <> 0 .OR. EVALUATE(lcTmpCtktB+'.REQ_QTY') <> 0
          lcClr_des = gfCodDes(EVALUATE(lcTmpCtktB+'.Iclr') , 'COLOR     ')
          *= gfRltFld(EVALUATE(lcTmpCtktB+'.ICLR') , @laColor , 'COLOR     ')
        *NEW AHMED [END]
        
          lcItem = gfCodDes(ITEM_TYPE , 'ITEM_TYPE ')
          lcVendor = APVENDOR.CSHRTNAM
          
          *NEW AHMED [START]
          *lcClr_Ref = SUBSTR(UPPER(TRIM(lcClr_Des)+' '+TRIM(lcRltColor)),1,14)
          lcClr_Ref = SUBSTR(UPPER(TRIM(lcClr_Des)+' '+ALLTRIM(Fabric.cClr_Ref)),1,14)
          *NEW AHMED [END]
          
          *B605496,1 AMH Print fabric.content in the desc. of the trim section [Start]
          *lcFabDesc = SUBSTR(LTRIM(UPPER(CFABSHDESC)),1,41)
          lcFabDesc = SUBSTR(UPPER(CONTENT),1,48)
          *B605496,1 AMH [End]
          
          lnPieces = EVALUATE(lcTmpCtktB+'.PIECES')
          lnUntQty = EVALUATE(lcTmpCtktB+'.UNTQTY')
          lcUomUse = UOMUSE
          
          *NEW AHMED [START]
          *lnNeed = IIF(CTKTUOM$'U ',EVALUATE(lcTmpCtktB+'.ISSUE_QTY'),EVALUATE(lcTmpCtktB+'.ISSUE_QTY')/CONV)
          lnNeed = IIF(EVALUATE(lcTmpCtktB+'.ISSUE_QTY')<>0,EVALUATE(lcTmpCtktB+'.ISSUE_QTY'),EVALUATE(lcTmpCtktB+'.REQ_QTY'))
          lnNeed = IIF(CTKTUOM$'U ' .OR. CONV=0,lnNeed,lnNeed/CONV)
          *NEW AHMED [END]
          
          lcUomBuy = IIF(CTKTUOM$'U ',UOMUSE,UOMBUY)
          INSERT INTO (TmpCtTkt);
               (cType,cCuttkt,nNum,cItem,cVendor,cClr_Ref,cFabDesc2,nPieces,nUntQty,;
                cUomUse,nNeed,cUomBuy,cItem_Type) VALUES;
               ('4',EVALUATE(lcTmpCtktB+'.CUTTKT'),lnNum,lcItem,lcVendor,lcCLr_Ref,lcFabDesc,lnPieces,lnUntQty,;
                lcUomUse,lnNeed,lcUomBuy,FABRIC.ITEM_TYPE)
          = lfReplace()
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
ENDIF
SELECT FABRIC
SET ORDER TO &lcOrder.
*--End of lfGetBom.

******************************************************************
*: Name        : lfReplace
*: Developer   : Ahmed Maher (AMH)
*: Date        : 12/02/2001
*: Purpose     : Replace cutting ticket header data
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : 
*:****************************************************************
*: Example     : =lfReplace()
*:****************************************************************
*
FUNCTION lfReplace

lnAlias = SELECT(0)
SELECT (TmpCtTkt)
REPLACE cMajor    WITH lcMajor;
        cSeasCod  WITH lcSeasCod;
        cPackNo   WITH lcPack;
        dComplete WITH ldComplete;
        cSalDesc1 WITH lcSalDesc1;
        cSalDesc2 WITH lcSalDesc2;
        cSalDesc3 WITH lcSalDesc3;
        cSalDesc4 WITH lcSalDesc4;
        cSalDesc5 WITH lcSalDesc5;
        cDivDesc  WITH lcDivDesc;
        cSewAt    WITH lcSewAt;
        cSeasDesc WITH lcSeasDesc;
        cCutAt    WITH lcCutAt;
        cCarLbl   WITH ALLTRIM(lcCarlbl);
        cCarDsc1  WITH lcCareDsc1;
        cCarDsc2  WITH lcCareDsc2;
        cCarDsc3  WITH lcCareDsc3;
        cCarDsc4  WITH lcCareDsc4;
        cCarDsc5  WITH lcCareDsc5;
        nLot_No   WITH EVALUATE(lcTmpCtktH+'.NLOT_NO')
SELECT (lnAlias)
*-- end of lfReplace.

*!*************************************************************
*! Name      : lfsrStyle
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/29/2001
*! Purpose   : Rise change order of style file in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : lcParm
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrStyle()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrStyle
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    SET ORDER TO Cstyle IN STYLE
    GOTO TOP
  CASE lcParm = 'R'
    SET ORDER TO Style IN STYLE
    GOTO TOP
ENDCASE
*-- End of lfsrStyle.

*!*************************************************************
*! Name      : lfCreatCur
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/29/2001
*! Purpose   : Create Cursor for cuttkth, cuttktl, ctktbom
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : lcFile (H=CUTTKTH,L=CUTTKTL,B=CTKTBOM)
*!                      lcIndexKey = the index key 
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfCreatCur()
*!*************************************************************
FUNCTION lfCreatCur
PARAMETERS lcFile,lcIndexKey

lcTmpCtkt&lcFile. = gfTempName()
DECLARE laCtkt&lcFile.Str[1,4]

DO CASE
  CASE lcFile = 'H'
    SELECT CUTTKTH
  CASE lcFile = 'L'
    SELECT CUTTKTL
  CASE lcFile = 'B'
    SELECT CTKTBOM
ENDCASE
=AFIELDS(laCtkt&lcFile.Str)
CREATE CURSOR (lcTmpCtkt&lcFile.) FROM ARRAY laCtkt&lcFile.Str
INDEX ON &lcIndexKey. TAG (lcTmpCtkt&lcFile.) OF (lcTmpCtkt&lcFile.)
SELECT (lcTmpCtkt&lcFile.)
SET ORDER TO (lcTmpCtkt&lcFile.)
*-- End of lfCreatCur.

*C200293,1 AMH Function to get the sectioned marker data [Start]
*:****************************************************************
*: Name        : lfGetSecLn
*: Developer   : AHMED MAHER (AMH)
*: Date        : 02/24/2002
*: Purpose     : Function to get the sectioned marker data
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : 
*:****************************************************************
*: Example     : =lfGetSecLn()
*:****************************************************************
*
FUNCTION lfGetSecLn

PRIVATE lnAlias,lcOrder
lnAlias = SELECT(0)

m.Cuttkt = EVALUATE(lcTmpCtktH+'.CUTTKT')
IF !EMPTY(lcPrmMrk) .AND. SEEK(lcPrmMrk,'PDMMARK') .AND. SEEK('M'+m.Cuttkt+lcPrmFab,lcTmpCtktB)
  SELECT (lcTmpCtktB)
  lcOrder = SET('ORDER')
  SET ORDER TO TAG CTKTYP
  SCAN REST WHILE cimtyp+cuttkt+item+iclr+mfgcode+dyelot = 'M'+m.Cuttkt+lcPrmFab;
            FOR CMARKER = lcPrmMrk
    m.Color  = ICLR
    lcClr_Ref = ''
    lnI = 0
    FOR lnI = 1 TO 6
      IF BETWEEN(ASC(SUBSTR(ICLR,lnI,1)),48,57)
        lcClr_Ref = lcClr_Ref + SUBSTR(ICLR,lnI,1)
      ENDIF
    ENDFOR
    lcClr_Ref = IIF(VAL(lcClr_Ref)=0,'',ALLTRIM(STR(VAL(lcClr_Ref),6,0)))
    IF SEEK(lcPrmMrk,'PDMMRKSH')
      SELECT PDMMRKSH
      SCAN REST WHILE CKEY_FLD+CSECTNO = lcPrmMrk
        SCATTER MEMVAR MEMO
        m.mPattPiece = lfGetPiece()
        
        *B606481,1 AMH Get the correct replen [Start]
        *INSERT INTO (TmpCtTkt) (cType,cCutTkt,cMarker,cItem_Type,cClr_Ref,cUomUse,nQty1,nNeed,cFabDesc,nPly,cLineType) VALUES;
                               ('M',m.Cuttkt,m.cKey_Fld,lcClr_Ref,;
                                PDMMARK.cRepLenFra,m.cSectNo,m.nNum_Rep,m.nTot_Len,;
                                m.mPattPiece,0,m.Color)
        INSERT INTO (TmpCtTkt) (cType,cCutTkt,cMarker,cItem_Type,nRepLenDec,cUomUse,nQty1,nNeed,cFabDesc,nPly,cLineType) VALUES;
                               ('M',m.Cuttkt,m.cKey_Fld,lcClr_Ref,;
                                PDMMARK.nRepLenDec,m.cSectNo,m.nNum_Rep,m.nTot_Len,;
                                m.mPattPiece,0,m.Color)
        *B606481,1 AMH [End]
        
        =lfReplace()
      ENDSCAN
    ENDIF
  ENDSCAN
  SELECT (lcTmpCtktB)
  SET ORDER TO &lcOrder.
ENDIF
SELECT (lnAlias)
*-- end of lfGetSecLn.

*!*************************************************************
*! Name      : lfGetPiece
*! Developer : Ahmed Maher (AMH)
*! Date      : 02/24/2002
*! Purpose   : Get the pieces assigned to section.
*!*************************************************************
*! Example   : = lfGetPiece()
*!*************************************************************
*
FUNCTION lfGetPiece

PRIVATE lnAlias
lcPieces = ''
lnAlias = SELECT(0)
=SEEK(SUBSTR(EVALUATE(lcTmpCtktH+'.STYLE'),1,12),'STYLE')
=SEEK('S'+STYLE.SCALE,'SCALE')
IF SEEK(CKEY_FLD+CSECTNO,'PDMMRKSL')
  SELECT PDMMRKSL
  SCAN REST WHILE CKEY_FLD+CSECTNO+CCUTMCODE = PDMMRKSH.CKEY_FLD+PDMMRKSH.CSECTNO
    =SEEK(CPATERN_NO+CCUTMCODE,'PDMCUTMS')
    lcSize = STR(NSIZE,1)
    lcPieces = lcPieces+IIF(EMPTY(lcPieces),'','/')+ALLTRIM(STR(PIECES,6))+;
               IIF(EMPTY(EVALUATE('SCALE.SZ'+lcSize)),'',' ')+ALLTRIM(EVALUATE('SCALE.SZ'+lcSize))+;
               IIF(EMPTY(PDMCUTMS.CDESC),'',' ')+ALLTRIM(PDMCUTMS.CDESC)
  ENDSCAN
ENDIF
SELECT (lnAlias)
RETURN lcPieces
*-- end of lfGetPiece.
*C200293,1 AMH [End]

*B605790,1 AMH Function to get the sorting of fabrics by desc. [Start]
*:****************************************************************
*: Name        : lfGetSort
*: Developer   : AHMED MAHER (AMH)
*: Date        : 04/03/2002
*: Purpose     : Function to get the sorting of fabrics by desc.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : 
*:****************************************************************
*: Example     : =lfGetSort()
*:****************************************************************
*
FUNCTION lfGetSort

DO CASE
  CASE UPPER(ALLTRIM(EVALUATE(lcTmpCtktB+'.DESC'))) = 'SELF'
    RETURN '1'
  CASE UPPER(ALLTRIM(EVALUATE(lcTmpCtktB+'.DESC'))) = 'COMBO'
    RETURN '2'
  CASE UPPER(ALLTRIM(EVALUATE(lcTmpCtktB+'.DESC'))) = 'BINDING'
    RETURN '3'
  CASE UPPER(ALLTRIM(EVALUATE(lcTmpCtktB+'.DESC'))) = 'RIB SPRIAL'
    RETURN '4'
  OTHERWISE
    RETURN '5'
ENDCASE
*-- end of lfGetSort.
*B605790,1 AMH [End]