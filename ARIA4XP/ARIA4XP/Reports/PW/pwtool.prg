*:***************************************************************************
*: Program file  : HRTOOL
*: Program desc. : Tool
*: System        : Aria Advantage Series ARIA4XP.
*: Module        : HR
*: Developer     : Mahmoud Said (MAH)
*! Date          : 05/07/2012
*: Reference     : N000681 - Convert Tool Report to A27 to A4XP
*:***************************************************************************

IF llOgFltCh
  WAIT WINDOW "Collecting Data......." NOWAIT 
  
  DIMENSION loOgScroll.laCRParams[1,2]

  loOgScroll.laCRParams[1,1] = 'ReportName'
  loOgScroll.laCRParams[1,2] = 'Tools Report'
  
  LOCAL lnSelect
  lnSelect = SELECT()

  LOCAL lcSqlStatment, lcSqlWhere, lnConnectionHandlar, lcToolCursor, lcToolUnitCursor
  lcToolCursor       = loOGScroll.gfTempName()
  lcToolUnitCursor    = loOGScroll.gfTempName()
  
  lcSqlStatment = "SELECT PWTOOLH.CTOLGRPID, PWTOOLH.CTOLGRPDEC " + ;
                  ",PWTOOLH.CTOLGRPTYP, PWTOOLH.NTOLGRPQTY" + ;
                  ",PWTOOLH.NTOLGRPCST, PWTOOLH.CPLANT_ID, PEPLANT.CPNAME"

  lcSqlStatment = lcSqlStatment + " FROM PWTOOLH LEFT JOIN PEPLANT ON (PWTOOLH.CPLANT_ID = PEPLANT.CPLANT_ID) "

  *-- Set filter
  LOCAL llAddWhere, lnPos, lcFilter, lnCounter

  lcSqlWhere = ""
  
  *-- Plant
  lnPos = ASUBSCRIPT(loOGScroll.laOGFXFlt, ASCAN(loOGScroll.laOGFXFlt,'PWTOOLH.CPLANT_ID'), 1)

  IF lnPos > 0
    lcFilter = loOGScroll.laOGFXFlt[lnPos, 6]

    IF !EMPTY(lcFilter) .AND. RECCOUNT(lcFilter) > 0
      IF !llAddWhere
        lcSqlWhere = lcSqlWhere + " WHERE ("
        llAddWhere = .T.
      ENDIF

      SELECT(lcFilter)
      LOCATE
      lcSqlWhere = lcSqlWhere + "(PWTOOLH.CPLANT_ID = '" + keyexp + "')";

      lnCounter = 0
      SCAN
        IF lnCounter > 0
          lcSqlWhere = lcSqlWhere + " OR (PWTOOLH.CPLANT_ID = '" + keyexp + "')"
        ENDIF
        lnCounter = lnCounter + 1
      ENDSCAN

      lcSqlWhere = lcSqlWhere + ")"
    ENDIF
  ENDIF
      
  *-- Tool Group
  lnPos = ASUBSCRIPT(loOGScroll.laOGFXFlt, ASCAN(loOGScroll.laOGFXFlt,'PWTOOLH.CTOLGRPID'), 1)

  IF lnPos > 0
    lcFilter = loOGScroll.laOGFXFlt[lnPos, 6]

    IF !EMPTY(lcFilter) .AND. RECCOUNT(lcFilter) > 0
      IF !llAddWhere
        lcSqlWhere = lcSqlWhere + " WHERE ("
        llAddWhere = .T.
      ELSE
        lcSqlWhere = lcSqlWhere + " AND ("
      ENDIF

      SELECT(lcFilter)
      LOCATE
      lcSqlWhere = lcSqlWhere + "(PWTOOLH.CTOLGRPID = '" + keyexp + "')";

      lnCounter = 0
      SCAN
        IF lnCounter > 0
          lcSqlWhere = lcSqlWhere + " OR (PWTOOLH.CTOLGRPID = '" + keyexp + "')"
        ENDIF
        lnCounter = lnCounter + 1
      ENDSCAN

      lcSqlWhere = lcSqlWhere + ")"
    ENDIF
  ENDIF

  *-- Tool Group Type
  IF ALLTRIM(lcRpToolGroupType) <> "B"
    IF !llAddWhere
      lcSqlWhere = lcSqlWhere + " WHERE "
      llAddWhere = .T.
    ELSE
      lcSqlWhere = lcSqlWhere + " AND "
    ENDIF
    
    lcSqlWhere = lcSqlWhere + "PWTOOLH.CTOLGRPTYP = '" + ALLTRIM(lcRpToolGroupType) + "'"
  ENDIF

  lcSqlStatment = lcSqlStatment + lcSqlWhere + " ORDER BY PWTOOLH.CTOLGRPID"

  lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment, lcToolCursor, "", oAriaApplication.ActiveCompanyConStr, 3, 'BROWSE', SET("DATASESSION"))
                
  lcSqlStatment = "SELECT CTOLGRPID, CTOOL_ID, " + ;
                  "CTOOL_DESC, CTOLGRPTYP, CPRIORITY, " + ;
                  "DTOOLPUR, CTOOL_EFF, NTOOLLIF, " + ;
                  "NTOOLCST, NTOOLUSD"
             
  lcSqlStatment = lcSqlStatment +;
                  " FROM PWTOLDET " + ;
                  "WHERE CTOLGRPID IN (SELECT CTOLGRPID FROM PWTOOLH " + lcSqlWhere + ") "

  IF ALLTRIM(lcRpToolCodeType) <> "B"
    lcSqlStatment = lcSqlStatment + " AND " + " CTOLGRPTYP = '" + ALLTRIM(lcRpToolCodeType) + "'"
  ENDIF

  lcSqlStatment = lcSqlStatment + "ORDER BY CTOOL_ID"
  
  lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment, lcToolUnitCursor, "", oAriaApplication.ActiveCompanyConStr, 3, 'BROWSE', SET("DATASESSION"))
  
  WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcToolCursor))) + ' Records.' NOWAIT


  *-- Loop to fill addtional infotmation to Tool
  WAIT WINDOW 'Creating temp. Tool file.' NOWAIT 
  
  lcToolTable = loOGScroll.gfTempName()

  CREATE TABLE (oAriaApplication.WorkDir + lcToolTable) ;
               (CTOLGRPID C(6) NULL, CTOLGRPDEC C(30) NULL, CTOLGRPTYP C(1) NULL, NTOLGRPCST NUMBER(7, 2) NULL, ;
                NTOLGRPQTY NUMBER(4, 0) NULL, CPLANT_ID C(6) NULL, CPNAME C(30) NULL, LPUNITS L NULL)

  SELECT (lcToolCursor)
  LOCATE

  SCAN
    SELECT (lcToolTable)
    APPEND BLANK
    
    REPLACE CTOLGRPID  WITH &lcToolCursor..CTOLGRPID
    REPLACE CTOLGRPDEC WITH &lcToolCursor..CTOLGRPDEC
    REPLACE CTOLGRPTYP WITH &lcToolCursor..CTOLGRPTYP
    REPLACE NTOLGRPQTY WITH &lcToolCursor..NTOLGRPQTY
    REPLACE NTOLGRPCST WITH &lcToolCursor..NTOLGRPCST
    REPLACE CPLANT_ID  WITH &lcToolCursor..CPLANT_ID
    REPLACE CPNAME     WITH &lcToolCursor..CPNAME

    REPLACE LPUNITS    WITH llRpPrintToolUnits

    SELECT (lcToolCursor)
  ENDSCAN


  *-- Loop to fill addtional infotmation to Tool Unit
  WAIT WINDOW 'Creating temp. Tool Unit file.' NOWAIT 
  
  lcToolUnitTable = loOGScroll.gfTempName()

  CREATE TABLE (oAriaApplication.WorkDir + lcToolUnitTable) ;
               (CTOLGRPID C(6) NULL, CTOOL_ID C(6) NULL, ;
                CTOOL_DESC C(30) NULL, CTOLGRPTYP C(1) NULL, CPRIORITY C(1) NULL, DTOOLPUR DATE NULL, ;
                CTOOL_EFF NUMBER(3, 0) NULL, NTOOLLIF NUMBER(7, 2) NULL, NTOOLCST NUMBER(6, 2) NULL, NTOOLUSD NUMBER(7, 2) NULL)

  SELECT (lcToolUnitCursor)
  LOCATE

  SCAN
    SELECT (lcToolUnitTable)
    APPEND BLANK
    
    REPLACE CTOLGRPID  WITH &lcToolUnitCursor..CTOLGRPID
    REPLACE CTOOL_ID   WITH &lcToolUnitCursor..CTOOL_ID
    REPLACE CTOOL_DESC WITH &lcToolUnitCursor..CTOOL_DESC
    REPLACE CTOLGRPTYP WITH &lcToolUnitCursor..CTOLGRPTYP 
    REPLACE CPRIORITY  WITH &lcToolUnitCursor..CPRIORITY
    REPLACE DTOOLPUR   WITH &lcToolUnitCursor..DTOOLPUR
    REPLACE CTOOL_EFF  WITH &lcToolUnitCursor..CTOOL_EFF
    REPLACE NTOOLLIF   WITH &lcToolUnitCursor..NTOOLLIF
    REPLACE NTOOLCST   WITH &lcToolUnitCursor..NTOOLCST
    REPLACE NTOOLUSD   WITH &lcToolUnitCursor..NTOOLUSD

    SELECT (lcToolUnitCursor)
  ENDSCAN

  *-- Set Report Data Source
  DIMENSION loOGScroll.laCRTables[2]
  loOGScroll.laCRTables[1] = oAriaApplication.WorkDir + lcToolTable + ".DBF"
  loOGScroll.laCRTables[2] = oAriaApplication.WorkDir + lcToolUnitTable + ".DBF"

  DIMENSION loOGScroll.laCRTablesSubReport[2]
  loOGScroll.laCRTablesSubReport[1] = ""
  loOGScroll.laCRTablesSubReport[2] = "ToolUnit"
  
  WAIT CLEAR
  
  SELECT (lcToolTable)
  IF !RECCOUNT() > 0
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF

  IF USED(lcToolTable)
    USE IN (lcToolTable)
  ENDIF

  IF USED(lcToolUnitTable)
    USE IN (lcToolUnitTable)
  ENDIF

  =gfDispRe()
ELSE
  lcToolTable = loOGScroll.gfTempName()
  
  USE loOGScroll.laCRTables[1] IN 0 ALIAS (lcToolTable) AGAIN
  
  SELECT (lcToolTable)
  IF !RECCOUNT() > 0
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF

  IF USED(lcToolTable)
    USE IN (lcToolTable)
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


*:****************************************************************
*: Name        : lfGetPlantName
*: Developer   : Mahmoud Said (MAH)
*: Date        : 02/20/2007
*: Purpose     : Get Sift Plant Name
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Called from : Option Grid
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : 
*:****************************************************************
*: Example     : =lfGetPlantName()
*:****************************************************************
*
FUNCTION lfGetPlantName

IF TYPE('ctolgrpid') = 'C' .AND. !EMPTY(ctolgrpid) .AND. TYPE('cplant_id') = 'C' .AND. !EMPTY(cplant_id) .AND. !ISNULL(cplant_id)
  LOCAL lcSqlStatment, lnConnectionHandlar, lcPlantCursor
  lcPlantCursor = loOGScroll.gfTempName()

  lcSqlStatment = "SELECT cpname AS [Name] "

  lcSqlStatment = lcSqlStatment + " FROM PEPLANT "
  
  lcSqlStatment = lcSqlStatment + " WHERE cplant_id = '" + cplant_id + "'"
  
  lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment, lcPlantCursor, "", oAriaApplication.ActiveCompanyConStr, 3, 'BROWSE', SET("DATASESSION"))
  
  IF RECCOUNT(lcPlantCursor) > 0
    RETURN RTRIM(&lcPlantCursor..Name)
  ELSE
    RETURN ''
  ENDIF
ELSE
  RETURN ''
ENDIF