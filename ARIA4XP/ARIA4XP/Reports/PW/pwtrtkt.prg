*:***************************************************************************
*: Program file  : PWTRTKR
*: Program desc. : Tracking Tickets
*: System        : Aria Advantage Series ARIA4XP.
*: Module        : PW
*: Developer     : Mahmoud Said (MAH)
*! Date          : 05/07/2012
*: Reference     : N000681 - Convert Convert Tracking Tickets Report to A27 to A4XP
*:***************************************************************************
*B609914,1 MMT 05/13/2012 PW bugs fixes for demo 05/13/2012[No Changes done just attached report to entry]
*:***************************************************************************
IF llOgFltCh
  STORE 0 TO lnClrLen ,lnClrPos

  DECLARE laItemSeg[1]
  PRIVATE lnCount 
  =gfItemMask(@laItemSeg)
  FOR lnCount = 1 TO ALEN(laItemSeg, 1)
    IF laItemSeg[lnCount, 1]='C'
      lnClrLen = LEN(laItemSeg[lnCount, 3])
      lnClrPos = laItemSeg[lnCount, 4]
      lcClrSpr = ALLT(laItemSeg[lnCount, 6])
      EXIT
    ENDIF
  ENDFOR

  
  WAIT WINDOW "Collecting Data......." NOWAIT 
 
  LOCAL lnSelect
  lnSelect = SELECT()

  LOCAL llUseBundle
  llUseBundle = gfGetMemVar('LUSEBUNDLE')

  LOCAL lcSqlStatment, lnConnectionHandlar, lcPWCursor
  lcPWCursor = loOGScroll.gfTempName()

  lcSqlStatment = "SELECT " + ;
    "PWCTKBOM.CUTTKT AS CutTicket , " + ;
    "PWCTKBOM.STYLE AS Style, " + ;
    "PWCTKBOM.MFGCODE AS MFGOpCode, " + ;
    "PWCTKBOM.COPRCODE AS DetOpCode, " + ;
    "PWCTKBOM.NGENNO AS GenSeq, " + ;
    "PWOPERAT.CCOPON_DES AS CouponDesc, " + ;
    "POSHDR.ENTERED AS Issued, " + ;
    "(SELECT TOP 1 NLOTQTY1 FROM MFGOPRDT WHERE MFGOPRDT.CIMTYP = 'M' AND MFGOPRDT.CTKTNO = PWCTKBOM.CUTTKT AND MFGOPRDT.ITEM = PWCTKBOM.STYLE AND MFGOPRDT.TRANCD = '1') AS LotQty1, " + ;
    "(SELECT TOP 1 NLOTQTY2 FROM MFGOPRDT WHERE MFGOPRDT.CIMTYP = 'M' AND MFGOPRDT.CTKTNO = PWCTKBOM.CUTTKT AND MFGOPRDT.ITEM = PWCTKBOM.STYLE AND MFGOPRDT.TRANCD = '1') AS LotQty2, " + ;
    "(SELECT TOP 1 NLOTQTY3 FROM MFGOPRDT WHERE MFGOPRDT.CIMTYP = 'M' AND MFGOPRDT.CTKTNO = PWCTKBOM.CUTTKT AND MFGOPRDT.ITEM = PWCTKBOM.STYLE AND MFGOPRDT.TRANCD = '1') AS LotQty3, " + ;
    "(SELECT TOP 1 NLOTQTY4 FROM MFGOPRDT WHERE MFGOPRDT.CIMTYP = 'M' AND MFGOPRDT.CTKTNO = PWCTKBOM.CUTTKT AND MFGOPRDT.ITEM = PWCTKBOM.STYLE AND MFGOPRDT.TRANCD = '1') AS LotQty4, " + ;
    "(SELECT TOP 1 NLOTQTY5 FROM MFGOPRDT WHERE MFGOPRDT.CIMTYP = 'M' AND MFGOPRDT.CTKTNO = PWCTKBOM.CUTTKT AND MFGOPRDT.ITEM = PWCTKBOM.STYLE AND MFGOPRDT.TRANCD = '1') AS LotQty5, " + ;
    "(SELECT TOP 1 NLOTQTY6 FROM MFGOPRDT WHERE MFGOPRDT.CIMTYP = 'M' AND MFGOPRDT.CTKTNO = PWCTKBOM.CUTTKT AND MFGOPRDT.ITEM = PWCTKBOM.STYLE AND MFGOPRDT.TRANCD = '1') AS LotQty6, " + ;
    "(SELECT TOP 1 NLOTQTY7 FROM MFGOPRDT WHERE MFGOPRDT.CIMTYP = 'M' AND MFGOPRDT.CTKTNO = PWCTKBOM.CUTTKT AND MFGOPRDT.ITEM = PWCTKBOM.STYLE AND MFGOPRDT.TRANCD = '1') AS LotQty7, " + ;
    "(SELECT TOP 1 NLOTQTY8 FROM MFGOPRDT WHERE MFGOPRDT.CIMTYP = 'M' AND MFGOPRDT.CTKTNO = PWCTKBOM.CUTTKT AND MFGOPRDT.ITEM = PWCTKBOM.STYLE AND MFGOPRDT.TRANCD = '1') AS LotQty8, " + ;
    "(SELECT TOP 1 DUEDATE FROM MFGOPRDT WHERE MFGOPRDT.CIMTYP = 'M' AND MFGOPRDT.CTKTNO = PWCTKBOM.CUTTKT AND MFGOPRDT.ITEM = PWCTKBOM.STYLE AND MFGOPRDT.TRANCD = '1') AS DueDate, " + ;
    "PWCTKBOM.CBUNDLE AS Bundle "

  IF llUseBundle
    lcSqlStatment = lcSqlStatment + ;
      ", PWBUNDL.NBQTY AS BundleQty, " + ;
      "PWBUNDL.LOTSIZE AS BundleSize, " + ;
      "PWBUNDL.CLOTNO AS LotNo "
  ENDIF

  lcSqlStatment = lcSqlStatment + ;
    " FROM " + ;
    "PWCTKBOM JOIN " + ;
    "PWOPERAT ON (PWCTKBOM.MFGCODE = PWOPERAT.MFGCODE AND PWCTKBOM.COPRCODE = PWOPERAT.COPRCODE) JOIN " + ;
    "POSHDR ON (POSHDR.CBUSDOCU = 'P' AND POSHDR.CSTYTYPE = 'U' AND PWCTKBOM.CUTTKT = POSHDR.PO) JOIN " + ;
    "MFGOPRHD ON (PWCTKBOM.CUTTKT = MFGOPRHD.CTKTNO AND PWCTKBOM.MFGCODE = MFGOPRHD.COPRCODE AND MFGOPRHD.CIMTYP = 'M')";

  IF llUseBundle
    lcSqlStatment = lcSqlStatment + ;
      " JOIN PWBUNDL ON " +;
      "(PWCTKBOM.CBUNDLE = PWBUNDL.CBUNDLE AND PWCTKBOM.CUTTKT = PWBUNDL.CUTTKT AND TRANTYPE = 'B' AND PWBUNDL.CIMTYP = 'M')"
  ENDIF

  *-- Set filter
  LOCAL llAddWhere, lnPos, lcFilter, lnCounter

  *-- Cut Ticket
  lnPos = ASUBSCRIPT(loOGScroll.laOGFXFlt, ASCAN(loOGScroll.laOGFXFlt,'POSHDR.PO'), 1)

  IF lnPos > 0
    lcFilter = loOGScroll.laOGFXFlt[lnPos, 6]

    IF !EMPTY(lcFilter) .AND. RECCOUNT(lcFilter) > 0
      IF !llAddWhere
        lcSqlStatment = lcSqlStatment  + " WHERE ("
        llAddWhere = .T.
      ENDIF

      SELECT(lcFilter)
      LOCATE
      lcSqlStatment = lcSqlStatment + "(PWCTKBOM.CUTTKT = '" + keyexp + "')";

      lnCounter = 0
      SCAN
        IF lnCounter > 0
          lcSqlStatment = lcSqlStatment + " OR (PWCTKBOM.CUTTKT = '" + keyexp + "')"
        ENDIF
        lnCounter = lnCounter + 1
      ENDSCAN

      lcSqlStatment = lcSqlStatment + ")"
    ENDIF
  ENDIF

  *-- Enter Date
  lnPos = ASUBSCRIPT(loOGScroll.laOGFXFlt, ASCAN(loOGScroll.laOGFXFlt,'POSHDR.ENTERED'), 1)

  IF !EMPTY(laOGFXFlt[lnPos,6])
    IF !llAddWhere
      lcSqlStatment = lcSqlStatment  + " WHERE "
      llAddWhere = .T.
    ELSE
      lcSqlStatment = lcSqlStatment  + " AND "
    ENDIF

    dEntDate1  = CTOD(SUBSTR(loOGScroll.laOGFXFlt[lnPos,6],1,10))
    dEntDate2 = CTOD(SUBSTR(loOGScroll.laOGFXFlt[lnPos,6],12,21))

    lcSqlStatment = lcSqlStatment + " POSHDR.ENTERED >= ?dEntDate1 AND POSHDR.ENTERED <= ?dEntDate2 "
  ENDIF

  *-- Complete Date
  lnPos = ASUBSCRIPT(loOGScroll.laOGFXFlt, ASCAN(loOGScroll.laOGFXFlt,'POSHDR.COMPLETE'), 1)

  IF !EMPTY(laOGFXFlt[lnPos,6])
    IF !llAddWhere
      lcSqlStatment = lcSqlStatment  + " WHERE "
      llAddWhere = .T.
    ELSE
      lcSqlStatment = lcSqlStatment  + " AND "
    ENDIF

    dCmpDate1  = CTOD(SUBSTR(loOGScroll.laOGFXFlt[lnPos,6],1,10))
    dCmpDate2 = CTOD(SUBSTR(loOGScroll.laOGFXFlt[lnPos,6],12,21))

    lcSqlStatment = lcSqlStatment + " POSHDR.COMPLETE >= ?dCmpDate1 AND POSHDR.COMPLETE <= ?dCmpDate2 "
  ENDIF

  *-- Status
  lnPos = ASUBSCRIPT(loOGScroll.laOGFXFlt, ASCAN(loOGScroll.laOGFXFlt,'POSHDR.STATUS'), 1)

  IF !EMPTY(laOGFXFlt[lnPos,6])
    IF !llAddWhere
      lcSqlStatment = lcSqlStatment  + " WHERE "
      llAddWhere = .T.
    ELSE
      lcSqlStatment = lcSqlStatment  + " AND "
    ENDIF

    lcSqlStatment = lcSqlStatment + " POSHDR.STATUS IN ('" + STRTRAN(laOGFXFlt[lnPos,6], '|', "', '") + "') "
  ENDIF

  *-- Style
  lnPos = ASUBSCRIPT(loOGScroll.laOGFXFlt, ASCAN(loOGScroll.laOGFXFlt,'POSHDR.STYLE'), 1)

  IF lnPos > 0
    lcFilter = loOGScroll.laOGFXFlt[lnPos, 6]

    IF !EMPTY(lcFilter) .AND. RECCOUNT(lcFilter) > 0
      IF !llAddWhere
        lcSqlStatment = lcSqlStatment  + " WHERE ("
        llAddWhere = .T.
      ELSE
        lcSqlStatment = lcSqlStatment  + " AND ("
      ENDIF

      SELECT(lcFilter)
      LOCATE
      lcSqlStatment = lcSqlStatment + "(PWCTKBOM.STYLE = '" + keyexp + "')";

      lnCounter = 0
      SCAN
        IF lnCounter > 0
          lcSqlStatment = lcSqlStatment + " OR (PWCTKBOM.STYLE = '" + keyexp + "')"
        ENDIF
        lnCounter = lnCounter + 1
      ENDSCAN

      lcSqlStatment = lcSqlStatment + ")"
    ENDIF
  ENDIF

  *-- Detail Operation
  lnPos = ASUBSCRIPT(loOGScroll.laOGFXFlt, ASCAN(loOGScroll.laOGFXFlt,'PWOPERAT.MFGCODE + PWOPERAT.COPRCODE'), 1)

  IF lnPos > 0
    lcFilter = loOGScroll.laOGFXFlt[lnPos, 6]

    IF !EMPTY(lcFilter) .AND. RECCOUNT(lcFilter) > 0
      IF !llAddWhere
        lcSqlStatment = lcSqlStatment  + " WHERE ("
        llAddWhere = .T.
      ELSE
        lcSqlStatment = lcSqlStatment  + " AND ("
      ENDIF

      SELECT(lcFilter)
      LOCATE
      lcSqlStatment = lcSqlStatment + "(PWCTKBOM.MFGCODE = '" + SUBSTR(keyexp, 1, 6) + "' AND PWCTKBOM.COPRCODE = '" + SUBSTR(keyexp, 7) + "')";

      lnCounter = 0
      SCAN
        IF lnCounter > 0
          lcSqlStatment = lcSqlStatment + " OR (PWCTKBOM.MFGCODE = '" + SUBSTR(keyexp, 1, 6) + "' AND PWCTKBOM.COPRCODE = '" + SUBSTR(keyexp, 7) + "')"
        ENDIF
        lnCounter = lnCounter + 1
      ENDSCAN

      lcSqlStatment = lcSqlStatment + ")"
    ENDIF
  ENDIF

  lcSqlStatment = lcSqlStatment + " ORDER BY PWCTKBOM.CUTTKT , PWCTKBOM.STYLE , CAST(MFGOPRHD.COPERSEQ as int)"

  lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment, lcPWCursor, "", oAriaApplication.ActiveCompanyConStr, 3, 'BROWSE', SET("DATASESSION"))
  
  WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcPWCursor))) + ' Records.' NOWAIT

  *-- Loop to fill descriptions
  WAIT WINDOW 'Creating temp. Tracking Tickets file.' NOWAIT 
    
  lcPWTable = loOGScroll.gfTempName()

  CREATE TABLE (oAriaApplication.WorkDir + lcPWTable) (UseBundle L NULL, CutTicket c(6) NULL, STYLE c(19) NULL, MFGOpCode c(6) NULL, DetOpCode c(6) NULL, GenSeq c(10) NULL, ;
    CouponDesc c(6) NULL, MFGOpDisc c(30) NULL, Issued DATE NULL, StyleDesc c(20) NULL, SizeTitle c(5) NULL, ;
    LotQty numeric(6) NULL, DueDate DATE NULL,  Color c(6) NULL, Bundle c(6) NULL, LotNo c(6) NULL, SizeNo c(1) NULL)

  SELECT (lcPWCursor)
  LOCATE

  SCAN
    SELECT (lcPWTable)
    IF llUseBundle
      APPEND BLANK

      REPLACE UseBundle WITH .T.
      REPLACE CutTicket WITH &lcPWCursor..CutTicket
      REPLACE Style WITH &lcPWCursor..Style
      REPLACE MFGOpCode WITH &lcPWCursor..MFGOpCode
      REPLACE DetOpCode WITH &lcPWCursor..DetOpCode
      REPLACE GenSeq WITH &lcPWCursor..GenSeq
      REPLACE CouponDesc WITH &lcPWCursor..CouponDesc
      REPLACE Issued WITH &lcPWCursor..Issued
      REPLACE LotQty WITH &lcPWCursor..BundleQty
      REPLACE DueDate WITH &lcPWCursor..DueDate
      REPLACE Bundle WITH &lcPWCursor..Bundle
      REPLACE LotNo WITH &lcPWCursor..LotNo
      
      = SEEK(Style, "STYLE")
      REPLACE StyleDesc WITH  STYLE.Desc
      
      REPLACE COLOR WITH SUBSTR(STYLE.Style, lnClrPos, lnClrLen)
      
      = SEEK('N' + MFGOpCode + 'N' + 'MFGCODE', "CODES")
      REPLACE MFGOpDisc WITH CODES.CDISCREP

      = SEEK('S' + STYLE.SCALE, "SCALE")
      
      REPLACE SizeNo WITH IIF(ALLTRIM(&lcPWCursor..BundleSize) == ALLTRIM(SCALE.SZ1), '1', ;
                          IIF(ALLTRIM(&lcPWCursor..BundleSize) == ALLTRIM(SCALE.SZ2), '2', ;
                          IIF(ALLTRIM(&lcPWCursor..BundleSize) == ALLTRIM(SCALE.SZ3), '3', ;
                          IIF(ALLTRIM(&lcPWCursor..BundleSize) == ALLTRIM(SCALE.SZ4), '4', ;
                          IIF(ALLTRIM(&lcPWCursor..BundleSize) == ALLTRIM(SCALE.SZ5), '5', ;
                          IIF(ALLTRIM(&lcPWCursor..BundleSize) == ALLTRIM(SCALE.SZ6), '6', ;
                          IIF(ALLTRIM(&lcPWCursor..BundleSize) == ALLTRIM(SCALE.SZ7), '7', '8')))))))

      REPLACE SizeTitle WITH EVALUATE("SCALE.SZ" + SizeNo)

    ELSE
      LOCAL lnIndex
      FOR lnIndex = 1 TO 8
        IF EVALUATE(lcPWCursor + ".LotQty" + ALLTRIM(STR(lnIndex))) > 0
          APPEND BLANK

          REPLACE UseBundle WITH .F.
          REPLACE CutTicket WITH &lcPWCursor..CutTicket
          REPLACE Style WITH &lcPWCursor..Style
          REPLACE MFGOpCode WITH &lcPWCursor..MFGOpCode
          REPLACE DetOpCode WITH &lcPWCursor..DetOpCode
          REPLACE GenSeq WITH &lcPWCursor..GenSeq
          REPLACE CouponDesc WITH &lcPWCursor..CouponDesc
          REPLACE Issued WITH &lcPWCursor..Issued
          REPLACE LotQty WITH EVALUATE(lcPWCursor + ".LotQty" + ALLTRIM(STR(lnIndex)))
          REPLACE DueDate WITH &lcPWCursor..DueDate
          
          = SEEK(Style, "STYLE")
          REPLACE StyleDesc WITH  STYLE.Desc
      
          REPLACE COLOR WITH SUBSTR(STYLE.Style, lnClrPos, lnClrLen)
      
          = SEEK('N' + MFGOpCode + 'N' + 'MFGCODE', "CODES")
          REPLACE MFGOpDisc WITH CODES.CDISCREP

          = SEEK('S' + STYLE.SCALE, "SCALE")
          REPLACE SizeTitle WITH EVALUATE("SCALE.SZ" + ALLTRIM(STR(lnIndex)))
        ENDIF
      ENDFOR
    ENDIF

    SELECT (lcPWCursor)
  ENDSCAN


  *-- Set Report Data Source
  DIMENSION loOGScroll.laCRTables[1]
  loOGScroll.laCRTables[1] = oAriaApplication.WorkDir + lcPWTable + ".DBF"

  WAIT CLEAR
  
  SELECT (lcPWTable)
  IF !RECCOUNT() > 0
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF

  IF USED(lcPWTable)
    USE IN (lcPWTable)
  ENDIF

  =gfDispRe()
ELSE
  lcPWTable = loOGScroll.gfTempName()
  
  USE loOGScroll.laCRTables[1] IN 0 ALIAS (lcPWTable) AGAIN
  
  SELECT (lcPWTable)
  IF !RECCOUNT() > 0
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF

  IF USED(lcPWTable)
    USE IN (lcPWTable)
  ENDIF

  =gfDispRe()
ENDIF

*:****************************************************************
*: Name        : lfwOGWhen
*: Developer   : Mahmoud Said (MAH)
*: Date        : 02/20/2007
*: Purpose     : Intialize the report
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Called from : Option Grid
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : 
*:****************************************************************
*: Example     : =lfwOGWhen()
*:****************************************************************
*
FUNCTION lfwOGWhen 
DECLARE loOgScroll.laRepModes[1]
loOgScroll.laRepModes[1] = "WINDOWS"
RETURN .T.
