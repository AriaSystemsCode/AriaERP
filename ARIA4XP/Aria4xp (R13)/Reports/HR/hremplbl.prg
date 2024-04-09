*:***************************************************************************
*: Program file  : HREMPLBL
*: Program desc. : Employee Lable
*: System        : Aria Advantage Series ARIA4XP.
*: Module        : HR
*: Developer     : Mahmoud Said (MAH)
*! Date          : 05/07/2012
*: Reference     : N000681 - Convert Employee Lable Report to A27 to A4XP
*:***************************************************************************
#Include r:\aria4xp\reports\hr\hremplbl.h
IF llOgFltCh
* N000682 ,1 Thabet Handle globalization issues [Start]
*  WAIT WINDOW "Collecting Data......." NOWAIT
   *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Collect_Data NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Collect_Data,oAriaApplication.GetHeaderText("LANG_Collect_Data",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

* N000682 ,1 Thabet Handle globalization issues [END]
  LOCAL lnSelect
  lnSelect = SELECT()

  LOCAL lcSqlStatment, lnConnectionHandlar, lcEmployeeCursor
  lcEmployeeCursor = loOGScroll.gfTempName()

  lcSqlStatment = "SELECT " + ;
    "cperson_id AS PERSONID , " + ;
    "cname AS [NAME] "


  lcSqlStatment = lcSqlStatment + " FROM PEPERSON "

  *-- Set filter
  LOCAL llAddWhere, lnPos, lcFilter, lnCounter

  *-- Employee
  lnPos = ASUBSCRIPT(loOGScroll.laOGFXFlt, ASCAN(loOGScroll.laOGFXFlt,'PEPERSON.CPERSON_ID'), 1)

  IF lnPos > 0
    lcFilter = loOGScroll.laOGFXFlt[lnPos, 6]

    IF !EMPTY(lcFilter) .AND. RECCOUNT(lcFilter) > 0
      IF !llAddWhere
        lcSqlStatment = lcSqlStatment  + " WHERE ("
        llAddWhere = .T.
      ENDIF

      SELECT(lcFilter)
      LOCATE
      lcSqlStatment = lcSqlStatment + "(PEPERSON.CPERSON_ID = '" + keyexp + "')";

      lnCounter = 0
      SCAN
        IF lnCounter > 0
          lcSqlStatment = lcSqlStatment + " OR (PEPERSON.CPERSON_ID = '" + keyexp + "')"
        ENDIF
        lnCounter = lnCounter + 1
      ENDSCAN

      lcSqlStatment = lcSqlStatment + ")"
    ENDIF
  ENDIF

  lcSqlStatment = lcSqlStatment + " ORDER BY cperson_id"

  lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment, lcEmployeeCursor, "", oAriaApplication.ActiveCompanyConStr, 3, 'BROWSE', SET("DATASESSION"))

* N000682 ,1 Thabet Handle globalization issues [Start]
*  WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcEmployeeCursor))) + ' Records.' NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Collect_Data + ALLTRIM(STR(RECCOUNT(lcEmployeeCursor))) + LANG_Records  NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Collect_Data,oAriaApplication.GetHeaderText("LANG_Collect_Data",AHEADERFILE)) + ALLTRIM(STR(RECCOUNT(lcEmployeeCursor))) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Records,oAriaApplication.GetHeaderText("LANG_Records",AHEADERFILE))  NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

* N000682 ,1 Thabet Handle globalization issues [END]

  *-- Loop to fill descriptions
* N000682 ,1 Thabet Handle globalization issues [Start]
*  WAIT WINDOW 'Creating temp. Employees file.' NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Creating_temp_Employee NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Creating_temp_Employee,oAriaApplication.GetHeaderText("LANG_Creating_temp_Employee",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

* N000682 ,1 Thabet Handle globalization issues [END]
  lcEmployeeTable = loOGScroll.gfTempName()

  CREATE TABLE (oAriaApplication.WorkDir + lcEmployeeTable) (PERSONID C(8) NULL, NAME c(30) NULL)

  SELECT (lcEmployeeCursor)
  LOCATE

  SCAN
    SELECT (lcEmployeeTable)
    APPEND BLANK

    REPLACE PERSONID WITH &lcEmployeeCursor..PERSONID
    REPLACE NAME WITH &lcEmployeeCursor..NAME

    SELECT (lcEmployeeCursor)
  ENDSCAN

  *-- Set Report Data Source
  DIMENSION loOGScroll.laCRTables[1]
  loOGScroll.laCRTables[1] = oAriaApplication.WorkDir + lcEmployeeTable + ".DBF"

  WAIT CLEAR

  SELECT (lcEmployeeTable)
  IF !RECCOUNT() > 0
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF

  IF USED(lcEmployeeTable)
    USE IN (lcEmployeeTable)
  ENDIF

  =gfDispRe()
ELSE
  lcEmployeeTable = loOGScroll.gfTempName()

  USE loOGScroll.laCRTables[1] IN 0 ALIAS (lcEmployeeTable) AGAIN

  SELECT (lcEmployeeTable)
  IF !RECCOUNT() > 0
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF

  IF USED(lcEmployeeTable)
    USE IN (lcEmployeeTable)
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
RETURN .T.

*:****************************************************************
*: Name        : lfvFormat
*: Developer   : Mahmoud Said (MAH)
*: Date        : 02/20/2007
*: Purpose     : Get Report Form
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Called from : Option Grid
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     :
*:****************************************************************
*: Example     : =lfvFormat()
*:****************************************************************
*
FUNCTION lfvFormat
lcRpForm = lcFormat
loOGScroll.CheckPlatform()

*:****************************************************************
*: Name        : lfGetDepartment
*: Developer   : Mahmoud Said (MAH)
*: Date        : 02/20/2007
*: Purpose     : Get Employee Department
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Called from : Option Grid
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     :
*:****************************************************************
*: Example     : =lfGetDepartment()
*:****************************************************************
*
FUNCTION lfGetDepartment

IF TYPE('cperson_id') = 'C' .AND. !EMPTY(cperson_id) .AND. TYPE('cdeptid') = 'C' .AND. !EMPTY(cdeptid) .AND. !ISNULL(cdeptid)
  LOCAL lcSqlStatment, lnConnectionHandlar, lcDepartmentCursor
  lcDepartmentCursor = loOGScroll.gfTempName()

  lcSqlStatment = "SELECT cdeptname AS Department "

  lcSqlStatment = lcSqlStatment + " FROM PEDEPART "

  lcSqlStatment = lcSqlStatment + " WHERE cdeptid = '" + cdeptid + "'"

  lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment, lcDepartmentCursor, "", oAriaApplication.ActiveCompanyConStr, 3, 'BROWSE', SET("DATASESSION"))

  IF RECCOUNT(lcDepartmentCursor) > 0
    RETURN RTRIM(&lcDepartmentCursor..Department)
  ELSE
    RETURN ''
  ENDIF
ELSE
  RETURN ''
ENDIF

*:****************************************************************
*: Name        : lfGetPlant
*: Developer   : Mahmoud Said (MAH)
*: Date        : 02/20/2007
*: Purpose     : Get Employee Plant
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Called from : Option Grid
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     :
*:****************************************************************
*: Example     : =lfGetPlant()
*:****************************************************************
*
FUNCTION lfGetPlant

IF TYPE('cperson_id') = 'C' .AND. !EMPTY(cperson_id) .AND. TYPE('cplant_id') = 'C' .AND. !EMPTY(cplant_id) .AND. !ISNULL(cplant_id)
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

