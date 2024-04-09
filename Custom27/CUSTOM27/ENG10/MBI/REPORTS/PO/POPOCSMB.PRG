****************************************************************************
*: Program file      : POPOCSMB.PRG   (C#200216)
*: Program desc.     : CUSTOMER COSTING SHEET REPORT FOR MBI
*: System            : Aria Apparel System (A27).
*: Module            : Style Purchase Order (PO)
*: Developer         : (AMH) AHMED MAHER
*: Date              : 08/07/2001
*:**************************************************************************
*

SELECT STYLE
SET ORDER TO TAG STYLE
lnMajorLen = LEN(gfItemMask("PM"))
lnMjorCnt  = gfItemMask("SM")
DECLARE laStySeg[1,1]
STORE 0 TO lnSizePos, lnSizeLen
=gfItemMask(@laStySeg)
FOR lnCnt = lnMjorCnt + 1 TO ALEN(laStySeg,1)
  IF laStySeg[lnCnt , 1] = "S"
    lnSizePos  = laStySeg[lnCnt , 4]
    lnSizeLen  = LEN(laStySeg[lnCnt , 3])
  ENDIF
ENDFOR

DECLARE laTRltFld[1,2]
laTRltFld[1,1] = 'NTERDISCR'
laTRltFld[1,2] = 'lnTerDiscR'
lnTerDiscr = 0

*-- Create temp File
lcTmpFile  = gfTempName()
CREATE CURSOR (lcTmpFile);
   ( cPo C(6), cCustName C(30), cStyle C(19), cVenSty C(19), cDesc C(20), cItemType C(30), cVendor C(6),;
    cItem C(7), cWidth c(6), nUntCost N(11,3), nUntQty N(7,3), cSizes C(11), nAvrSelPrc N(14,3), nDiscount N(14,3),;
    nStyOrder N(7,0), nReceived N(7,0), nMerchndis N(5,2), nPayment N(5,2))
INDEX ON cPo TAG (lcTmpFile) OF (lcTmpFile)
SELECT (lcTmpFile)
SET ORDER TO (lcTmpFile)

*-- To get the selected style if any.
lcStylFile = ''
llUseStyle = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POSLN.STYLE'),1)
IF lnPosition > 0
  lcStylFile = laOGFxFlt[lnPosition,6]
  llUseStyle = IIF(!EMPTY(lcStylFile) .AND. USED(lcStylFile) .AND. RECCOUNT(lcStylFile)>0,.T.,.F.)
ENDIF

*-- To get the selected Customer if any.
lcCustFile = ''
llUseCust  = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.ACCOUNT'),1)
IF lnPosition > 0
  lcCustFile = laOGFxFlt[lnPosition,6]
  llUseCust = IIF(!EMPTY(lcCustFile) .AND. USED(lcCustFile) .AND. RECCOUNT(lcCustFile)>0,.T.,.F.)
ENDIF

*-- To get the selected vendors if any
lcPoFile = ''
llUsePo = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POSHDR.PO'),1)
IF lnPosition > 0
  lcPoFile = laOGFxFlt[lnPosition,6]
  llUsePo = IIF(!EMPTY(lcPoFile) .AND. USED(lcPoFile) .AND. RECCOUNT(lcPoFile)>0,.T.,.F.)
ENDIF

* -- If the user selects Pos
IF llUsePo
  SELECT (lcPoFile)
  LOCATE
  SCAN
    =SEEK('P'+PO,'POSHDR')
    =SEEK('P'+PO,'POSLN')
    =SEEK(POSLN.STYLE,'STYLE')
    =SEEK('2'+POSHDR.PO,'CUTPICK')
    =SEEK('O'+CUTPICK.ORDER,'ORDHDR')
    =SEEK('M'+ORDHDR.ACCOUNT,'CUSTOMER')
    IF llUseStyle .AND. !SEEK(SUBSTR(POSLN.STYLE,1,lnMajorLen),lcStylFile)
      LOOP
    ENDIF
    IF llUseCust .AND. !SEEK(ORDHDR.ACCOUNT,lcCustFile)
      LOOP
    ENDIF
    =lfReplace()
  ENDSCAN
ELSE
  IF llUseStyle
    SET ORDER TO POSLNS IN POSLN
    SELECT (lcStylFile)
    LOCATE
    SCAN
      IF SEEK(SUBSTR(CSTYMAJOR,1,lnMajorLen),'POSLN')
        SELECT POSLN
        lcPo = SPACE(6)
        *C200216,4 AMH Save all Pos number in lcPo [Start]
        *SCAN REST WHILE Style+cStyType+Po+STR(LineNo,6)+Trancd =;
                        SUBSTR(EVALUATE(lcStylFile+'.CSTYMAJOR'),1,lnMajorLen) FOR PO <> lcPo
          *lcPo = PO
        SCAN REST WHILE Style+cStyType+Po+STR(LineNo,6)+Trancd =;
                        SUBSTR(EVALUATE(lcStylFile+'.CSTYMAJOR'),1,lnMajorLen) FOR !(PO $ lcPo)
          lcPo = lcPo + '|' + PO
        *C200216,4 AMH [End]
          =SEEK('P'+PO,'POSHDR')
          =SEEK('2'+POSHDR.PO,'CUTPICK')
          =SEEK('O'+CUTPICK.ORDER,'ORDHDR')
          =SEEK('M'+ORDHDR.ACCOUNT,'CUSTOMER')
          IF llUseCust .AND. !SEEK(ORDHDR.ACCOUNT,lcCustFile)
            LOOP
          ENDIF
          =lfReplace()
        ENDSCAN
      ENDIF
    ENDSCAN
    SET ORDER TO POSLN IN POSLN
  ELSE
    IF llUseCust
      SET ORDER TO ORDACCT IN ORDHDR
      SET ORDER TO CUTORD IN CUTPICK
      SELECT (lcCustFile)
      LOCATE
      SCAN
        IF SEEK(ACCOUNT,'ORDHDR')
          SELECT ORDHDR
          SCAN REST WHILE account+cordtype+order = EVALUATE(lcCustFile+'.ACCOUNT')
            IF SEEK('2'+ORDER,'CUTPICK')
              =SEEK('P'+CUTPICK.CTKTNO,'POSHDR')
              =SEEK('P'+CUTPICK.CTKTNO,'POSLN')
              =SEEK(POSLN.STYLE,'STYLE')
              =SEEK('M'+ORDHDR.ACCOUNT,'CUSTOMER')
              =lfReplace()
            ENDIF
          ENDSCAN
        ENDIF
      ENDSCAN
      SET ORDER TO ORDHDR IN ORDHDR
      SET ORDER TO CUTPKORD IN CUTPICK
    ELSE
      SELECT POSHDR
      LOCATE
      SCAN
        =SEEK('P'+PO,'POSLN')
        =SEEK(POSLN.STYLE,'STYLE')
        =SEEK('2'+POSHDR.PO,'CUTPICK')
        =SEEK('O'+CUTPICK.ORDER,'ORDHDR')
        =SEEK('M'+ORDHDR.ACCOUNT,'CUSTOMER')
        =lfReplace()
      ENDSCAN
    ENDIF
  ENDIF
ENDIF

SELECT(lcTmpFile)
GO BOTTOM
IF EOF()
  SET DEVICE TO SCREEN
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF
=SEEK(gcBaseCurr,'SycCurr')

*--- Start printing report
DO gfDispRe WITH EVAL('lcFormName')
SET DEVICE TO SCREEN

IF USED(lcTmpFile)
  USE IN (lcTmpFile)
ENDIF

*!*************************************************************
*! Name      : lfReplace
*! Developer : (AMH) Ahmed Maher
*! Date      : 08/08/2001
*! Purpose   : Add new record to lctmpfile
*!*************************************************************
*! Example   : = lfReplace()
*!*************************************************************
FUNCTION lfReplace

PRIVATE lcOrder, lnRecNo
IF SEEK('I'+POSHDR.PO,'CTKTBOM')
  SELECT CTKTBOM
  SCAN REST WHILE cImTyp+Cuttkt+Item+Iclr+MfgCode+Dyelot = 'I'+POSHDR.PO
    =SEEK(SUBSTR(ITEM,1,7)+ICLR,'FABRIC')
    SELECT (lcTmpFile)
    APPEND BLANK
    *REPLACE CCUSTNAME WITH CUSTOMER.BTNAME,;
            CPO       WITH POSHDR.PO,;
            CSTYLE    WITH STYLE.CSTYMAJOR,;
            CVENSTY   WITH STYLE.CVENSTY,;
            CDESC     WITH STYLE.DESC,;
            CITEMTYPE WITH gfCodDes(FABRIC.ITEM_TYPE,'ITEM_TYPE '),;
            CVENDOR   WITH FABRIC.VENDOR,;
            CITEM     WITH CTKTBOM.ITEM,;
            CWIDTH    WITH CTKTBOM.WIDTH,;
            NUNTCOST  WITH CTKTBOM.UNTCOST,;
            NUNTQTY   WITH CTKTBOM.UNTQTY
    REPLACE CCUSTNAME WITH CUSTOMER.BTNAME,;
            CPO       WITH POSHDR.PO,;
            CSTYLE    WITH STYLE.CSTYMAJOR,;
            CVENSTY   WITH STYLE.CVENSTY,;
            CDESC     WITH STYLE.DESC,;
            CITEMTYPE WITH IIF(CTKTBOM.typ='1','MAKING PRICE',IIF(CTKTBOM.Typ$'45',ALLT(cTktBom.Desc),gfCodDes(FABRIC.ITEM_TYPE,'ITEM_TYPE '))),;
            CVENDOR   WITH FABRIC.VENDOR,;
            CITEM     WITH CTKTBOM.ITEM,;
            CWIDTH    WITH CTKTBOM.WIDTH,;
            NUNTCOST  WITH CTKTBOM.UNTCOST,;
            NUNTQTY   WITH CTKTBOM.UNTQTY
  ENDSCAN
  SELECT POSLN
  lnRecNo = RECNO()
  lcOrder = SET('ORDER')
  SET ORDER TO POSLN
  lcStyle = SPACE(19)
  STORE 0 TO lnFrsSz, lnLstSz, lnBase, lnRecvQty
  STORE SPACE(5) TO lcFrsSz, lcLstSz

  *C200216,5 AMH Scan for budget records only [Start]
  *SCAN REST WHILE cStyType+Po+Style+STR(LineNo,6)+Trancd= 'P'+POSHDR.PO FOR trancd $ '12'
  SCAN REST WHILE cStyType+Po+Style+STR(LineNo,6)+Trancd= 'P'+POSHDR.PO FOR trancd = '1'
    *IF TRANCD = '1'
  *C200216,5 AMH [End]
      =SEEK('S'+SCALE,'SCALE')
      IF lcStyle = STUFF(STYLE,lnSizePos,lnSizeLen,REPLICATE('*',lnSizeLen))
        lnBase = lnBase + 8
      ELSE
        lnBase = 0
      ENDIF
      lcStyle = STUFF(STYLE,lnSizePos,lnSizeLen,REPLICATE('*',lnSizeLen))
      FOR lnI = 1+lnBase TO SCALE.CNT+lnBase
        lcI = STR(lnI-lnBase,1)
        IF EVALUATE('QTY'+lcI) > 0
          IF lnFrsSz = 0 .OR. lnI < lnFrsSz
            lcFrsSz = EVALUATE('SCALE.SZ'+lcI)
            lnFrsSz = lnI
          ENDIF
          IF lnLstSz = 0 .OR. lnI > lnLstSz
            lcLstSz = EVALUATE('SCALE.SZ'+lcI)
            lnLstSz = lnI
          ENDIF
        ENDIF
      ENDFOR
    *C200216,5 AMH Comment the next lines [Start]
    *ELSE
      *lnRecvQty = lnRecvQty + TOTQTY
    *ENDIF
    *C200216,5 AMH [End]
  ENDSCAN
  SET ORDER TO &lcOrder.
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GOTO lnRecNo
  ENDIF
  IF !EMPTY(lcFrsSz)
    SELECT (lcTmpFile)
    REPLACE CSIZES WITH ALLTRIM(lcFrsSz)+' - '+ALLTRIM(lcLstSz)
  ENDIF
  lnAvSelPrc = 0
  SELECT CUTPICK
  lcOrder = SET('ORDER')
  SET ORDER TO CUTPKORD
  AVERAGE ORDLINE.PRICE REST WHILE Trancd+Ctktno+CtktLineno+Order+Style+CordLine =;
                        '2'+POSHDR.PO FOR SEEK('O'+ORDER+CORDLINE,'ORDLINE') TO lnAvSelPrc
  SET ORDER TO &lcOrder.
  
  *C200216,5 AMH Calculate Shiped Qantity [Start]
  IF SEEK(ORDHDR.ORDER,'INVLINE')
    SELECT INVLINE
    lcInvoice = SPACE(6)
    SCAN REST WHILE Order+STR(LineNo,6)+Invoice = ORDHDR.ORDER
      lnRecvQty = lnRecvQty + TOTQTY
      IF !(INVOICE $ lcInvoice)
        IF SEEK(ACCOUNT+INVOICE,'RETLINE')
          SELECT RETLINE
          SCAN REST WHILE Account+Invoice+Style+Crmemo = INVLINE.ACCOUNT+INVLINE.INVOICE
            lnRecvQty = lnRecvqty - TOTQTY
          ENDSCAN
        ENDIF
        lcInvoice = lcInvoice + INVLINE.INVOICE + "|"
      ENDIF
    ENDSCAN
  ENDIF
  *C200216,5 AMH [End]
  SELECT (lcTmpFile)
  REPLACE NAVRSELPRC WITH lnAvSelPrc
  =gfRltFld(OrdHdr.cTermCode,@laTRltFld,'CTERMCODE')
  lnOrDisc = (lnAvSelPrc*OrdHdr.Disc/100)
  lnOrDisc = lnOrDisc +((lnAvSelPrc-lnOrDisc) *lnTerDiscR/100)
  REPLACE NDISCOUNT  WITH lnOrDisc
  REPLACE NMERCHNDIS WITH ORDHDR.DISC
  REPLACE NPAYMENT   WITH lnTerDiscR
  REPLACE NSTYORDER  WITH POSHDR.NSTYORDER
  REPLACE NRECEIVED  WITH lnRecvQty
ENDIF
*-- end of lfReplace.

*:----------------------------------------------------------------------------
*: Program file        : lfsrvSty.
*: Program description : Rise change style flag, in range browse screen..
*: Developer Name      : (AMH) Ahmed Maher
*:----------------------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------------------
*: Example             : =lfsrvSty()
*:----------------------------------------------------------------------------
*!
FUNCTION lfSRVSty
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
ENDCASE
*-- end of lfsrvSty.

*:----------------------------------------------------------------------------
*: Program file        : lfStySum.
*: Program description : sum a specific field for the current style in style file.
*: Developer Name      : (AMH) Ahmed Maher
*:----------------------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------------------
*: Example             : =lfStySum()
*:----------------------------------------------------------------------------
*!
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(RECNO('STYLE') <= RECCOUNT('STYLE'),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
DO CASE
  CASE lnAddToVar = 1
  	lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
  	lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
  	lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE
RETURN INT(lnTotcomp)
*-- end of lfStySum.