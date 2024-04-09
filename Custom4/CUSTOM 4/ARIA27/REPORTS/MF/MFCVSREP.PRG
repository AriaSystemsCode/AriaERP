*:***************************************************************************
*: Program file  : MFCVSREP (C102748,1)
*: Program desc. : Custom value statement report
*: For Report    : MFCVSREP.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : Manufactoring
*: Developer     : AHMED MAHER (AMH)
*:***************************************************************************
*: Calls : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO MFCVSREP
*:***************************************************************************

*-- Get the selected C/T if any
lcCutFile = ''
llCutRang = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.CUTTKT'),1)
IF lnPosition > 0
  lcCutFile = laOGFxFlt[lnPosition,6]
  llCutRang = IIF(!EMPTY(lcCutFile) .AND. USED(lcCutFile) .AND. RECCOUNT(lcCutFile)>0,.T.,.F.)
ENDIF

*-- Get the selected Style if any
lcStyle = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.STYLE'),1)
IF lnPosition > 0
  lcStyle = laOGFxFlt[lnPosition,6]
ENDIF

*-- Get the selected Vendor if any
lcVendor = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'APVENDOR.CVENDCODE'),1)
IF lnPosition > 0
  lcVendor = laOGFxFlt[lnPosition,6]
ENDIF

DECLARE laVenAddrs[7]
STORE SPACE(30) TO laVenAddrs
IF !EMPTY(lcVendor) .AND. SEEK(lcVendor,'APVENDOR')
  laVenAddrs[1] = APVENDOR.CVENCOMP
  laVenAddrs[2] = APVENDOR.CADDRESS1
  laVenAddrs[3] = APVENDOR.CADDRESS2
  laVenAddrs[4] = APVENDOR.CADDRESS3
  laVenAddrs[5] = APVENDOR.CADDRESS4
  laVenAddrs[6] = APVENDOR.CADDRESS5
  laVenAddrs[7] = APVENDOR.CADDRESS6
  =lfShiftArr()
ENDIF

lcTmpCurs = gfTempName()
CREATE CURSOR (lcTmpCurs) (cCuttkt C(6),cStyle C(19),cDesc C(20),cContant C(60),cScl_Desc C(10);
                           ,nStyWeight N(5,2),cType C(1),cItem_Type C(6),cCostStat C(1),;
                           nUnitCost N(15,7))
INDEX ON CCUTTKT+CSTYLE+CTYPE+CITEM_TYPE+CCOSTSTAT TAG (lcTmpCurs) OF (lcTmpCurs)
IF llCutRang
  SELECT (lcCutFile)
  SCAN
    IF SEEK('M1'+CUTTKT,'BOMLINE')
      =lfScan(CUTTKT)
    ENDIF
  ENDSCAN
ELSE
  IF EMPTY(lcStyle)
    IF SEEK('M1','BOMLINE')
      =lfScan('')
    ENDIF
  ELSE
    IF SEEK(lcStyle,'CUTTKTL')
      SELECT CUTTKTL
      SCAN REST WHILE style+cuttkt+trancd = lcStyle FOR TRANCD = '1'
        IF SEEK('M1'+CUTTKT+STR(LINENO,6),'BOMLINE')
          =lfScan(CUTTKT+STR(LINENO,6))
        ENDIF
      ENDSCAN
    ENDIF
  ENDIF
ENDIF
IF RECCOUNT(lcTmpCurs) = 0
  =gfModalGen('TRM00052B00000','DIALOG' )
  USE IN (lcTmpCurs)
  RETURN
ENDIF
SELECT (lcTmpCurs)
DO gfDispRe WITH 'MFCVSREP'
USE IN (lcTmpCurs)

*!*************************************************************
*! Name      : lfvStyle
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/17/2002
*! Purpose   : Validate the style field.
*!*************************************************************
*! Called from : O.G.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStyle()
*!*************************************************************

FUNCTION lfvStyle

PRIVATE lcVar,lcValue
lcVar = SYS(18)
lcValue = EVALUATE(lcVar)
IF !SEEK(lcValue,'STYLE')
  lcValue = gfStyBrw('I','','',.F.)
  &lcVar. = lcValue
ENDIF

*!*************************************************************
*! Name      : lfvVendor
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/17/2002
*! Purpose   : Validate the Vendor field.
*!*************************************************************
*! Called from : O.G.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVendor()
*!*************************************************************

FUNCTION lfvVendor

PRIVATE lcVar,lcValue,lnAlias
lnAlias = SELECT(0)
lcVar = SYS(18)
lcValue = EVALUATE(lcVar)
IF !SEEK(lcValue,'APVENDOR')
  SELECT APVENDOR
  lcBrFields = "CVENDCODE :H='Vendor',"+;
               "CVENCOMP  :H='Name',"+;
               "cPhoneNo  :H='Phone',"+;
               "cVenOurAc :H='Our Account' "
    
  lcForExpr = " 'C' $ cVenSupTyp "
  IF gfBrows([FOR &lcForExpr],'cVendCode', 'laTempArr','Vendors')
    lcValue = APVENDOR.cVendCode
  ELSE
    lcValue = SPACE(8)
  ENDIF
  &lcVar. = lcValue
  SELECT (lnAlias)
ENDIF

*!*************************************************************
*! Name      : lfScan
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/17/2002
*! Purpose   : Scan the BomLine file
*!*************************************************************
*! Passed Parameters : lcKey
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfScan()
*!*************************************************************

FUNCTION lfScan
PARAMETERS lcKey

PRIVATE lcNewPage
SELECT BOMLINE
lcNewPage = CTKTNO+STYLE
lcContant = ''
SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
                'M1'+lcKey;
            FOR CCATGTYP $ 'FTM' .AND. CCOSTSTAT $ '1456'
  IF CTKTNO+STYLE # lcNewPage .AND. SEEK(lcNewPage,lcTmpCurs)
    =lfAddFrt()
    lcNewPage = CTKTNO+STYLE
  ENDIF
  =SEEK(STYLE,'STYLE')
  =SEEK(STYLE.FABRIC,'FABRIC')
  lcContant = FABRIC.CONTENT
  IF CCATGTYP $ 'FT'
    =SEEK(PADR(ITEM,7),'FABRIC')
  ENDIF
  =SEEK('S'+STYLE.SCALE,'SCALE')
  SELECT (lcTmpCurs)
  APPEND BLANK
  REPLACE CCUTTKT    WITH BOMLINE.CTKTNO,;
          CSTYLE     WITH BOMLINE.STYLE,;
          CDESC      WITH STYLE.DESC,;
          CCONTANT   WITH lcContant,;
          CSCL_DESC  WITH SCALE.CSCL_DESC,;
          NSTYWEIGHT WITH STYLE.NSTYWEIGHT,;
          CTYPE      WITH IIF(BOMLINE.CCATGTYP = 'F','1',;
                          IIF(BOMLINE.CCATGTYP = 'T','2','3')),;
          CITEM_TYPE WITH IIF(BOMLINE.CCATGTYP='M',BOMLINE.MFGCODE,FABRIC.ITEM_TYPE),;
          CCOSTSTAT  WITH BOMLINE.CCOSTSTAT,;
          NUNITCOST  WITH BOMLINE.UNITCOST
ENDSCAN
IF SEEK(lcNewPage,lcTmpCurs)
  =lfAddFrt()
ENDIF

*!*************************************************************
*! Name      : lfAddFrt
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/20/2002
*! Purpose   : Add the freight cost
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAddFrt()
*!*************************************************************

FUNCTION lfAddFrt

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCurs)
APPEND BLANK
REPLACE CCUTTKT    WITH SUBSTR(lcNewPage,1,6),;
        CSTYLE     WITH SUBSTR(lcNewPage,7),;
        CDESC      WITH STYLE.DESC,;
        CCONTANT   WITH lcContant,;
        CSCL_DESC  WITH SCALE.CSCL_DESC,;
        NSTYWEIGHT WITH STYLE.NSTYWEIGHT,;
        CTYPE      WITH '4',;
        CITEM_TYPE WITH 'FREIGHT',;
        CCOSTSTAT  WITH '6',;
        NUNITCOST  WITH lnRpFrtChr
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfShiftArr
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/20/2002
*! Purpose   : Shift Array elements
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfShiftArr()
*!*************************************************************

FUNCTION lfShiftArr

FOR lnI = 2 TO 6
  IF EMPTY(laVenAddrs[lnI])
    FOR lnJ = lnI + 1 TO 7
      laVenAddrs[lnJ-1] = laVenAddrs[lnJ]
    ENDFOR
  ENDIF
ENDFOR