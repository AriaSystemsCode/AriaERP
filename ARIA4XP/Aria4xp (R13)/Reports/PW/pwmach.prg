*:***************************************************************************
*: Program file  : PWMACH
*: Program desc. : Machine
*: System        : Aria Advantage Series ARIA4XP.
*: Module        : PW
*: Developer     : Mahmoud Said (MAH)
*! Date          : 05/07/2012
*: Reference     : N000681 - Convert Machine Report to A27 to A4XP
*:***************************************************************************
#INclude r:\aria4xp\reports\pw\pwmach.h
IF llOgFltCh

* N000682 ,1 Thabet Handle globalization issues [Start]
*  WAIT WINDOW "Collecting Data......." NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Collecting_Data NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Collecting_Data,oAriaApplication.GetHeaderText("LANG_Collecting_Data",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

  * N000682 ,1 Thabet Handle globalization issues [END]

  DIMENSION loOgScroll.laCRParams[1,2]

  loOgScroll.laCRParams[1,1] = 'ReportName'
  * N000682 ,1 Thabet Handle globalization issues [Start]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loOgScroll.laCRParams[1,2] = LANG_Machine_Report
loOgScroll.laCRParams[1,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Machine_Report,oAriaApplication.GetHeaderText("LANG_Machine_Report",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  * N000682 ,1 Thabet Handle globalization issues [END]

  LOCAL lnSelect
  lnSelect = SELECT()

  LOCAL lcSqlStatment, lcSqlWhere, lnConnectionHandlar, lcMachineCursor
  lcMachineCursor       = loOGScroll.gfTempName()

  lcSqlStatment = "SELECT PWMACHIN.CMACHINEID, " + ;
                  "PWMACHIN.CWORKCENT, " + ;
                  "PWMACHIN.CDESC AS MCDESC, " + ;
                  "PWMACHIN.DDATEPUR, " + ;
                  "PWMACHIN.NMCH_EFF, " + ;
                  "PWMACHIN.NSETUPCST, " + ;
                  "PWMACHIN.NRUNCST, " + ;
                  "PWMACHIN.CMCH_TYP, " + ;
                  "PWMACHIN.NPRIORITY, " + ;
                  "PWMACHIN.DLSTMNT, " + ;
                  "PWMACHIN.NTOTUSG, " + ;
                  "PEWCNTR.CDESC AS WCDESC "

  lcSqlStatment = lcSqlStatment + " FROM PWMACHIN LEFT JOIN PEWCNTR ON (PWMACHIN.CWORKCENT = PEWCNTR.CWORKCENT) "

  *-- Set filter
  LOCAL llAddWhere, lnPos, lcFilter, lnCounter

  lcSqlWhere = ""

  *-- Machines
  lnPos = ASUBSCRIPT(loOGScroll.laOGFXFlt, ASCAN(loOGScroll.laOGFXFlt,'PWMACHIN.CMACHINEID'), 1)

  IF lnPos > 0
    lcFilter = loOGScroll.laOGFXFlt[lnPos, 6]

    IF !EMPTY(lcFilter) .AND. RECCOUNT(lcFilter) > 0
      IF !llAddWhere
        lcSqlWhere = lcSqlWhere + " WHERE ("
        llAddWhere = .T.
      ENDIF

      SELECT(lcFilter)
      LOCATE
      lcSqlWhere = lcSqlWhere + "(PWMACHIN.CMACHINEID = '" + keyexp + "')";

      lnCounter = 0
      SCAN
        IF lnCounter > 0
          lcSqlWhere = lcSqlWhere + " OR (PWMACHIN.CMACHINEID = '" + keyexp + "')"
        ENDIF
        lnCounter = lnCounter + 1
      ENDSCAN

      lcSqlWhere = lcSqlWhere + ")"
    ENDIF
  ENDIF

  *-- Work Center
  lnPos = ASUBSCRIPT(loOGScroll.laOGFXFlt, ASCAN(loOGScroll.laOGFXFlt,'PWMACHIN.CWORKCENT'), 1)

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
      lcSqlWhere = lcSqlWhere + "(PWMACHIN.CWORKCENT = '" + keyexp + "')";

      lnCounter = 0
      SCAN
        IF lnCounter > 0
          lcSqlWhere = lcSqlWhere + " OR (PWMACHIN.CWORKCENT = '" + keyexp + "')"
        ENDIF
        lnCounter = lnCounter + 1
      ENDSCAN

      lcSqlWhere = lcSqlWhere + ")"
    ENDIF
  ENDIF

  *-- Machine Type
  IF ALLTRIM(lcRpMachineType) <> "B"
    IF !llAddWhere
      lcSqlWhere = lcSqlWhere + " WHERE "
      llAddWhere = .T.
    ELSE
      lcSqlWhere = lcSqlWhere + " AND "
    ENDIF

    lcSqlWhere = lcSqlWhere + "PWMACHIN.CMCH_TYP = '" + ALLTRIM(lcRpMachineType) + "'"
  ENDIF

  lcSqlStatment = lcSqlStatment + lcSqlWhere + " ORDER BY PWMACHIN.CMACHINEID"

  lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment, lcMachineCursor, "", oAriaApplication.ActiveCompanyConStr, 3, 'BROWSE', SET("DATASESSION"))

  * N000682 ,1 Thabet Handle globalization issues [Start]
*  WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcMachineCursor))) + ' Records.' NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW 'LANG_Selected' + ALLTRIM(STR(RECCOUNT(lcMachineCursor))) + LANG_Records NOWAIT
WAIT WINDOW 'LANG_Selected' + ALLTRIM(STR(RECCOUNT(lcMachineCursor))) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Records,oAriaApplication.GetHeaderText("LANG_Records",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

  * N000682 ,1 Thabet Handle globalization issues [END]

  *-- Loop to fill addtional infotmation to Machine

  * N000682 ,1 Thabet Handle globalization issues [Start]
*  WAIT WINDOW 'Creating temp. Machine file.' NOWAIT
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Creating_temp_Machine NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Creating_temp_Machine,oAriaApplication.GetHeaderText("LANG_Creating_temp_Machine",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

  * N000682 ,1 Thabet Handle globalization issues [END]

  lcMachineTable = loOGScroll.gfTempName()

  CREATE TABLE (oAriaApplication.WorkDir + lcMachineTable) ;
               (CMACHINEID C(6) NULL, CWORKCENT c(6) NULL, MCDESC C(20) NULL, DDATEPUR DATE NULL, ;
                NMCH_EFF NUMBER(6, 2) NULL, NSETUPCST NUMBER(6, 2) NULL, NRUNCST NUMBER(6, 2) NULL, ;
                CMCH_TYP C(1) NULL, NPRIORITY NUMBER(1, 0) NULL, DLSTMNT DATE NULL, NTOTUSG NUMBER(6, 2) NULL, WCDESC C(20) NULL)

  SELECT (lcMachineCursor)
  LOCATE

  SCAN
    SELECT (lcMachineTable)
    APPEND BLANK

    REPLACE CMACHINEID WITH &lcMachineCursor..CMACHINEID
    REPLACE CWORKCENT  WITH &lcMachineCursor..CWORKCENT
    REPLACE MCDESC     WITH &lcMachineCursor..MCDESC
    REPLACE DDATEPUR   WITH &lcMachineCursor..DDATEPUR
    REPLACE NMCH_EFF   WITH &lcMachineCursor..NMCH_EFF
    REPLACE NSETUPCST  WITH &lcMachineCursor..NSETUPCST
    REPLACE NRUNCST    WITH &lcMachineCursor..NRUNCST
    REPLACE CMCH_TYP   WITH &lcMachineCursor..CMCH_TYP
    REPLACE NPRIORITY  WITH &lcMachineCursor..NPRIORITY
    REPLACE DLSTMNT    WITH &lcMachineCursor..DLSTMNT
    REPLACE NTOTUSG    WITH &lcMachineCursor..NTOTUSG
    REPLACE WCDESC     WITH &lcMachineCursor..WCDESC

    SELECT (lcMachineCursor)
  ENDSCAN

  *-- Set Report Data Source
  DIMENSION loOGScroll.laCRTables[1]
  loOGScroll.laCRTables[1] = oAriaApplication.WorkDir + lcMachineTable + ".DBF"

  DIMENSION loOGScroll.laCRTablesSubReport[1]
  loOGScroll.laCRTablesSubReport[1] = ""

  WAIT CLEAR

  SELECT (lcMachineTable)
  IF !RECCOUNT() > 0
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF

  IF USED(lcMachineTable)
    USE IN (lcMachineTable)
  ENDIF

  =gfDispRe()
ELSE
  lcMachineTable = loOGScroll.gfTempName()

  USE loOGScroll.laCRTables[1] IN 0 ALIAS (lcMachineTable) AGAIN

  SELECT (lcMachineTable)
  IF !RECCOUNT() > 0
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF

  IF USED(lcMachineTable)
    USE IN (lcMachineTable)
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
*: Name        : lfGetWorkCenterName
*: Developer   : Mahmoud Said (MAH)
*: Date        : 02/20/2007
*: Purpose     : Get Work Center Name
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Called from : Option Grid
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     :
*:****************************************************************
*: Example     : =lfGetWorkCenterName()
*:****************************************************************
*
FUNCTION lfGetWorkCenterName

IF TYPE('cmachineid') = 'C' .AND. !EMPTY(cmachineid) .AND. TYPE('cworkcent') = 'C' .AND. !EMPTY(cworkcent) .AND. !ISNULL(cworkcent)
  LOCAL lcSqlStatment, lnConnectionHandlar, lcCursor
  lcCursor = loOGScroll.gfTempName()

  lcSqlStatment = "SELECT cdesc AS [Name] "

  lcSqlStatment = lcSqlStatment + " FROM pewcntr "

  lcSqlStatment = lcSqlStatment + " WHERE cworkcent = '" + cworkcent + "'"

  lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment, lcCursor, "", oAriaApplication.ActiveCompanyConStr, 3, 'BROWSE', SET("DATASESSION"))

  IF RECCOUNT(lcCursor) > 0
    RETURN RTRIM(&lcCursor..Name)
  ELSE
    RETURN ''
  ENDIF
ELSE
  RETURN ''
ENDIF
