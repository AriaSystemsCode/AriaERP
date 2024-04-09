*:***************************************************************************
*: Program file  : HRSHIFT
*: Program desc. : Department
*: System        : Aria Advantage Series ARIA4XP.
*: Module        : HR
*: Developer     : Mahmoud Said (MAH)
*! Date          : 05/07/2012
*: Reference     : N000681 - Convert Shift Report to A27 to A4XP
*:***************************************************************************

IF llOgFltCh
  WAIT WINDOW "Collecting Data......." NOWAIT 
  
  DIMENSION loOgScroll.laCRParams[1,2]

  loOgScroll.laCRParams[1,1] = 'ReportName'
  loOgScroll.laCRParams[1,2] = 'Shift Report'
  
  LOCAL lnSelect
  lnSelect = SELECT()

  LOCAL lcSqlStatment, lcSqlWhere, lnConnectionHandlar, lcShiftCursor, lcEmployeeCursor
  lcShiftCursor       = loOGScroll.gfTempName()
  lcEmployeeCursor    = loOGScroll.gfTempName()
  
  lcSqlStatment = "SELECT PESHIFT.CSHIFT_ID, PESHIFT.CSHIFT_STR " + ;
                  " ,PESHIFT.CSHIFT_FNS, PESHIFT.CPLANT_ID, PEPLANT.CPNAME "

  lcSqlStatment = lcSqlStatment + " FROM PESHIFT LEFT JOIN PEPLANT ON (PESHIFT.CPLANT_ID = PEPLANT.CPLANT_ID) "

  *-- Set filter
  LOCAL llAddWhere, lnPos, lcFilter, lnCounter

  lcSqlWhere = ""
  
  *-- Plant
  lnPos = ASUBSCRIPT(loOGScroll.laOGFXFlt, ASCAN(loOGScroll.laOGFXFlt,'PESHIFT.CPLANT_ID'), 1)

  IF lnPos > 0
    lcFilter = loOGScroll.laOGFXFlt[lnPos, 6]

    IF !EMPTY(lcFilter) .AND. RECCOUNT(lcFilter) > 0
      IF !llAddWhere
        lcSqlWhere = lcSqlWhere + " WHERE ("
        llAddWhere = .T.
      ENDIF

      SELECT(lcFilter)
      LOCATE
      lcSqlWhere = lcSqlWhere + "(PESHIFT.CPLANT_ID = '" + keyexp + "')";

      lnCounter = 0
      SCAN
        IF lnCounter > 0
          lcSqlWhere = lcSqlWhere + " OR (PESHIFT.CPLANT_ID = '" + keyexp + "')"
        ENDIF
        lnCounter = lnCounter + 1
      ENDSCAN

      lcSqlWhere = lcSqlWhere + ")"
    ENDIF
  ENDIF
      
  *-- Shift
  lnPos = ASUBSCRIPT(loOGScroll.laOGFXFlt, ASCAN(loOGScroll.laOGFXFlt,'PESHIFT.CSHIFT_ID'), 1)

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
      lcSqlWhere = lcSqlWhere + "(PESHIFT.CSHIFT_ID = '" + keyexp + "')";

      lnCounter = 0
      SCAN
        IF lnCounter > 0
          lcSqlWhere = lcSqlWhere + " OR (PESHIFT.CSHIFT_ID = '" + keyexp + "')"
        ENDIF
        lnCounter = lnCounter + 1
      ENDSCAN

      lcSqlWhere = lcSqlWhere + ")"
    ENDIF
  ENDIF

  lcSqlStatment = lcSqlStatment + lcSqlWhere + " ORDER BY CSHIFT_ID"

  lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment, lcShiftCursor, "", oAriaApplication.ActiveCompanyConStr, 3, 'BROWSE', SET("DATASESSION"))
                
  lcSqlStatment = "SELECT EMPSHIFT.CPERSON_ID, PEPERSON.CNAME, EMPSHIFT.CSHIFT_ID, EMPSHIFT.CSHIFT_STR, EMPSHIFT.CSHIFT_FNS, EMPSHIFT.CPLANT_ID " + ;
                  "FROM PEPERSON LEFT JOIN EMPSHIFT ON (EMPSHIFT.CPERSON_ID = PEPERSON.CPERSON_ID) " + ;
                  "WHERE EMPSHIFT.CSHIFT_ID IN (SELECT CSHIFT_ID FROM PESHIFT " + lcSqlWhere + ") ORDER BY PEPERSON.CPERSON_ID"
  
  lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment, lcEmployeeCursor, "", oAriaApplication.ActiveCompanyConStr, 3, 'BROWSE', SET("DATASESSION"))
  
  WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcShiftCursor))) + ' Records.' NOWAIT


  *-- Loop to fill addtional infotmation to Shift
  WAIT WINDOW 'Creating temp. Department file.' NOWAIT 
  
  lcShiftTable = loOGScroll.gfTempName()

  CREATE TABLE (oAriaApplication.WorkDir + lcShiftTable) ;
               (CSHIFT_ID C(6) NULL, CSHIFT_STR C(8) NULL, CSHIFT_FNS C(8) NULL, CPLANT_ID C(6) NULL, CPNAME C(30) NULL, LPEMP L NULL)

  SELECT (lcShiftCursor)
  LOCATE

  SCAN
    SELECT (lcShiftTable)
    APPEND BLANK
    
    REPLACE CSHIFT_ID  WITH &lcShiftCursor..CSHIFT_ID
    REPLACE CSHIFT_STR WITH &lcShiftCursor..CSHIFT_STR
    REPLACE CSHIFT_FNS WITH &lcShiftCursor..CSHIFT_FNS
    REPLACE CPLANT_ID  WITH &lcShiftCursor..CPLANT_ID
    REPLACE CPNAME     WITH &lcShiftCursor..CPNAME

    REPLACE LPEMP      WITH llRpPrintEmps

    SELECT (lcShiftCursor)
  ENDSCAN


  *-- Loop to fill addtional infotmation to Employee
  WAIT WINDOW 'Creating temp. Employee file.' NOWAIT 
  
  lcEmployeeTable = loOGScroll.gfTempName()
                  
  CREATE TABLE (oAriaApplication.WorkDir + lcEmployeeTable) ;
               (CPERSON_ID C(8) NULL, CNAME C(30) NULL, ;
                CSHIFT_ID C(6) NULL, CSHIFT_STR C(8) NULL, CSHIFT_FNS C(8) NULL, CPLANT_ID C(6) NULL)

  SELECT (lcEmployeeCursor)
  LOCATE

  SCAN
    SELECT (lcEmployeeTable)
    APPEND BLANK
    
    REPLACE CPERSON_ID WITH &lcEmployeeCursor..CPERSON_ID
    REPLACE CNAME      WITH &lcEmployeeCursor..CNAME
    REPLACE CSHIFT_ID  WITH &lcEmployeeCursor..CSHIFT_ID
    REPLACE CSHIFT_STR WITH &lcEmployeeCursor..CSHIFT_STR
    REPLACE CSHIFT_FNS WITH &lcEmployeeCursor..CSHIFT_FNS
    REPLACE CPLANT_ID  WITH &lcEmployeeCursor..CPLANT_ID
    
    
    
    SELECT (lcEmployeeCursor)
  ENDSCAN

  *-- Set Report Data Source
  DIMENSION loOGScroll.laCRTables[2]
  loOGScroll.laCRTables[1] = oAriaApplication.WorkDir + lcShiftTable + ".DBF"
  loOGScroll.laCRTables[2] = oAriaApplication.WorkDir + lcEmployeeTable + ".DBF"

  DIMENSION loOGScroll.laCRTablesSubReport[2]
  loOGScroll.laCRTablesSubReport[1] = ""
  loOGScroll.laCRTablesSubReport[2] = "EmpShift"
  
  WAIT CLEAR
  
  SELECT (lcShiftTable)
  IF !RECCOUNT() > 0
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF

  IF USED(lcShiftTable)
    USE IN (lcShiftTable)
  ENDIF

  IF USED(lcEmployeeTable)
    USE IN (lcEmployeeTable)
  ENDIF

  =gfDispRe()
ELSE
  lcShiftTable = loOGScroll.gfTempName()
  
  USE loOGScroll.laCRTables[1] IN 0 ALIAS (lcShiftTable) AGAIN
  
  SELECT (lcShiftTable)
  IF !RECCOUNT() > 0
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF

  IF USED(lcShiftTable)
    USE IN (lcShiftTable)
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

IF TYPE('cshift_id') = 'C' .AND. !EMPTY(cshift_id) .AND. TYPE('cplant_id') = 'C' .AND. !EMPTY(cplant_id) .AND. !ISNULL(cplant_id)
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



