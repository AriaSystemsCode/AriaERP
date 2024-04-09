*:***************************************************************************
*: Program file  : HREMP
*: Program desc. : Employee
*: System        : Aria Advantage Series ARIA4XP.
*: Module        : HR
*: Developer     : Mahmoud Said (MAH)
*! Date          : 05/07/2012
*: Reference     : N000681 - Convert Employee Report to A27 to A4XP
*:***************************************************************************

IF llOgFltCh
  WAIT WINDOW "Collecting Data......." NOWAIT 
  
  DIMENSION loOgScroll.laCRParams[1,2]

  loOgScroll.laCRParams[1,1] = 'ReportName'
  loOgScroll.laCRParams[1,2] = 'Employee Report'
  
  LOCAL lnSelect
  lnSelect = SELECT()

  LOCAL lcSqlStatment, lcSqlWhere, lnConnectionHandlar, lcEmployeeCursor, lcQulificationCursor, lcShiftCursor
  lcEmployeeCursor     = loOGScroll.gfTempName()
  lcQulificationCursor = loOGScroll.gfTempName()
  lcShiftCursor        = loOGScroll.gfTempName()

  lcSqlStatment = "SELECT CPERSON_ID ,CNAME , CPLANT_ID , CDEPTID , CPASPORTNO , " + ;
                  " CSOCIALNO , DBIRTHDATE , DSERVDATE , DTERMDATE , CACTFORCST , CSTATUS , " + ;
                  " CCATEGORY , CADDRESS1 , CADDRESS2 , CADDRESS3 , CADDRESS4 ,CADDRESS5 , " + ;
                  " CADDRESS6 , CE_MAIL , PHONE1 , PHONE2 , CMOBILE , CEMG_NAME , CEMG_ADD1 , " + ;
                  " CEMG_ADD2 , CEMG_ADD3 , CEMG_ADD4 , CEMG_ADD5 , CEMG_ADD6 , CEMG_MAIL , " + ;
                  " CEMG_PHON1 , CEMG_PHON2 , CEMG_MOBIL , CPAY_TYPE , CPAYFREQ , NRATE , " + ;
                  " NHOURRATE , NSALARY , NOVERTRATE , CCOSTCENT , CWORKCENT  "


  lcSqlStatment = lcSqlStatment + " FROM PEPERSON "

  *-- Set filter
  LOCAL llAddWhere, lnPos, lcFilter, lnCounter

  lcSqlWhere = ""
  
  *-- Plant
  lnPos = ASUBSCRIPT(loOGScroll.laOGFXFlt, ASCAN(loOGScroll.laOGFXFlt,'PEPERSON.CPLANT_ID'), 1)

  IF lnPos > 0
    lcFilter = loOGScroll.laOGFXFlt[lnPos, 6]

    IF !EMPTY(lcFilter) .AND. RECCOUNT(lcFilter) > 0
      IF !llAddWhere
        lcSqlWhere = lcSqlWhere + " WHERE ("
        llAddWhere = .T.
      ENDIF

      SELECT(lcFilter)
      LOCATE
      lcSqlWhere = lcSqlWhere + "(PEPERSON.CPLANT_ID = '" + keyexp + "')";

      lnCounter = 0
      SCAN
        IF lnCounter > 0
          lcSqlWhere = lcSqlWhere + " OR (PEPERSON.CPLANT_ID = '" + keyexp + "')"
        ENDIF
        lnCounter = lnCounter + 1
      ENDSCAN

      lcSqlWhere = lcSqlWhere + ")"
    ENDIF
  ENDIF
  
  *-- Employee
  lnPos = ASUBSCRIPT(loOGScroll.laOGFXFlt, ASCAN(loOGScroll.laOGFXFlt,'PEPERSON.CPERSON_ID'), 1)

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
      lcSqlWhere = lcSqlWhere + "(PEPERSON.CPERSON_ID = '" + keyexp + "')";

      lnCounter = 0
      SCAN
        IF lnCounter > 0
          lcSqlWhere = lcSqlWhere + " OR (PEPERSON.CPERSON_ID = '" + keyexp + "')"
        ENDIF
        lnCounter = lnCounter + 1
      ENDSCAN

      lcSqlWhere = lcSqlWhere + ")"
    ENDIF
  ENDIF

  *-- Department
  lnPos = ASUBSCRIPT(loOGScroll.laOGFXFlt, ASCAN(loOGScroll.laOGFXFlt,'PEPERSON.CDEPTID'), 1)

  IF lnPos > 0
    lcFilter = loOGScroll.laOGFXFlt[lnPos, 6]

    IF !EMPTY(lcFilter) .AND. RECCOUNT(lcFilter) > 0
      IF !llAddWhere
        lcSqlWhere = lcSqlWhere  + " WHERE ("
        llAddWhere = .T.
      ELSE
        lcSqlWhere = lcSqlWhere  + " AND ("
      ENDIF

      SELECT(lcFilter)
      LOCATE
      lcSqlWhere = lcSqlWhere + "(PEPERSON.CDEPTID = '" + keyexp + "')";

      lnCounter = 0
      SCAN
        IF lnCounter > 0
          lcSqlWhere = lcSqlWhere + " OR (PEPERSON.CDEPTID = '" + keyexp + "')"
        ENDIF
        lnCounter = lnCounter + 1
      ENDSCAN

      lcSqlWhere = lcSqlWhere + ")"
    ENDIF
  ENDIF

  *-- Type
  IF ALLTRIM(lcRpType) <> "B"
    IF !llAddWhere
      lcSqlWhere = lcSqlWhere + " WHERE "
      llAddWhere = .T.
    ELSE
      lcSqlWhere = lcSqlWhere + " AND "
    ENDIF
    
    lcSqlWhere = lcSqlWhere + "PEPERSON.CEMP_TYPE = '" + ALLTRIM(lcRpType) + "'"
  ENDIF

  *-- Category
  IF ALLTRIM(lcRpCategory) <> "B"
    IF !llAddWhere
      lcSqlWhere = lcSqlWhere + " WHERE "
      llAddWhere = .T.
    ELSE
      lcSqlWhere = lcSqlWhere + " AND "
    ENDIF
    
    lcSqlWhere = lcSqlWhere + "PEPERSON.CCATEGORY = '" + ALLTRIM(lcRpCategory) + "'"
  ENDIF
  
  *-- Status
  IF ALLTRIM(lcRpStatus) <> "E"
    IF !llAddWhere
      lcSqlWhere = lcSqlWhere + " WHERE "
      llAddWhere = .T.
    ELSE
      lcSqlWhere = lcSqlWhere + " AND "
    ENDIF
    
    lcSqlWhere = lcSqlWhere + "PEPERSON.CSTATUS = '" + ALLTRIM(lcRpStatus) + "'"
  ENDIF
  
  lcSqlStatment = lcSqlStatment + lcSqlWhere  + " ORDER BY PEPERSON.CPERSON_ID"

  lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment, lcEmployeeCursor, "", oAriaApplication.ActiveCompanyConStr, 3, 'BROWSE', SET("DATASESSION"))
  
  lcSqlStatment = "SELECT CPERSON_ID, CQULFCODE, NPERCENT FROM QUALIFY " +;
                  "WHERE CPERSON_ID IN (SELECT CPERSON_ID FROM PEPERSON " + lcSqlWhere + ") ORDER BY CQULFCODE"
  lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment, lcQulificationCursor, "", oAriaApplication.ActiveCompanyConStr, 3, 'BROWSE', SET("DATASESSION"))
  
  lcSqlStatment = "SELECT CPERSON_ID, CSHIFT_ID, CSHIFT_STR, CSHIFT_FNS FROM EMPSHIFT " + ;
                  "WHERE CPERSON_ID IN (SELECT CPERSON_ID FROM PEPERSON " + lcSqlWhere + ") ORDER BY CSHIFT_ID"
  lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment, lcShiftCursor, "", oAriaApplication.ActiveCompanyConStr, 3, 'BROWSE', SET("DATASESSION"))
  
  WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcEmployeeCursor))) + ' Records.' NOWAIT


  *-- Loop to fill addtional infotmation to employee
  WAIT WINDOW 'Creating temp. Employees file.' NOWAIT 
  
  lcEmployeeTable = loOGScroll.gfTempName()

  CREATE TABLE (oAriaApplication.WorkDir + lcEmployeeTable) ;
               (CPERSON_ID C(8) NULL, CNAME c(30) NULL, CPLANT_ID C(6) NULL, CDEPTID C(6) NULL, CPASPORTNO C(20) NULL, CSOCIALNO C(10) NULL, DBIRTHDATE Date NULL, ;
                DSERVDATE Date  NULL, DTERMDATE Date  NULL, CACTFORCST C(1)  NULL, CSTATUS C(1)  NULL, CCATEGORY C(1)  NULL, ;
                CADDRESS1 C(30)  NULL, CADDRESS2 C(30)  NULL, CADDRESS3 C(30)  NULL, CADDRESS4 C(30)  NULL, CADDRESS5 C(30)  NULL, CADDRESS6 C(30)  NULL, ;
                CE_MAIL C(30)  NULL, PHONE1 C(16)  NULL, PHONE2 C(16)  NULL, CMOBILE C(16)  NULL, CEMG_NAME C(30)  NULL, ;
                CEMG_ADD1 C(30)  NULL, CEMG_ADD2 C(30)  NULL, CEMG_ADD3 C(30)  NULL, CEMG_ADD4 C(30)  NULL, CEMG_ADD5 C(30)  NULL, CEMG_ADD6 C(30)  NULL, ;
                CEMG_MAIL C(30)  NULL, CEMG_PHON1 C(16)  NULL, CEMG_PHON2 C(16)  NULL, CEMG_MOBIL C(16)  NULL, ;
                CPAY_TYPE C(1)  NULL, CPAYFREQ C(1)  NULL, NRATE number(8,2)  NULL, NHOURRATE number(8,2)  NULL, NSALARY number(8,2)  NULL, NOVERTRATE number(6,2)  NULL, ;
                CCOSTCENT C(6)  NULL, CWORKCENT C(6)  NULL, ;
                LPADDRESS L  NULL, LPEMG L  NULL, LPQUAL L  NULL, LPPAYROLL L  NULL, LPSHIFT L  NULL)

  SELECT (lcEmployeeCursor)
  LOCATE

  SCAN
    SELECT (lcEmployeeTable)
    APPEND BLANK
    
    REPLACE CPERSON_ID WITH &lcEmployeeCursor..CPERSON_ID
    REPLACE CNAME      WITH &lcEmployeeCursor..CNAME
    REPLACE CPLANT_ID  WITH &lcEmployeeCursor..CPLANT_ID
    REPLACE CDEPTID    WITH &lcEmployeeCursor..CDEPTID
    REPLACE CPASPORTNO WITH &lcEmployeeCursor..CPASPORTNO
    REPLACE CSOCIALNO  WITH &lcEmployeeCursor..CSOCIALNO
    REPLACE DBIRTHDATE WITH &lcEmployeeCursor..DBIRTHDATE
    REPLACE DSERVDATE  WITH &lcEmployeeCursor..DSERVDATE
    REPLACE DTERMDATE  WITH &lcEmployeeCursor..DTERMDATE
    REPLACE CACTFORCST WITH &lcEmployeeCursor..CACTFORCST
    REPLACE CSTATUS    WITH &lcEmployeeCursor..CSTATUS
    REPLACE CCATEGORY  WITH &lcEmployeeCursor..CCATEGORY
    REPLACE CADDRESS1  WITH &lcEmployeeCursor..CADDRESS1
    REPLACE CADDRESS2  WITH &lcEmployeeCursor..CADDRESS2
    REPLACE CADDRESS3  WITH &lcEmployeeCursor..CADDRESS3
    REPLACE CADDRESS4  WITH &lcEmployeeCursor..CADDRESS4
    REPLACE CADDRESS5  WITH &lcEmployeeCursor..CADDRESS5
    REPLACE CADDRESS6  WITH &lcEmployeeCursor..CADDRESS6
    REPLACE CE_MAIL    WITH &lcEmployeeCursor..CEMG_MAIL
    REPLACE PHONE1     WITH &lcEmployeeCursor..CEMG_PHON1
    REPLACE PHONE2     WITH &lcEmployeeCursor..CEMG_PHON2
    REPLACE CMOBILE    WITH &lcEmployeeCursor..CMOBILE
    REPLACE CEMG_NAME  WITH &lcEmployeeCursor..CEMG_NAME
    REPLACE CEMG_ADD1  WITH &lcEmployeeCursor..CEMG_ADD1
    REPLACE CEMG_ADD2  WITH &lcEmployeeCursor..CEMG_ADD2
    REPLACE CEMG_ADD3  WITH &lcEmployeeCursor..CEMG_ADD3
    REPLACE CEMG_ADD4  WITH &lcEmployeeCursor..CEMG_ADD4
    REPLACE CEMG_ADD5  WITH &lcEmployeeCursor..CEMG_ADD5
    REPLACE CEMG_ADD6  WITH &lcEmployeeCursor..CEMG_ADD6
    REPLACE CEMG_MAIL  WITH &lcEmployeeCursor..CEMG_MAIL
    REPLACE CEMG_PHON1 WITH &lcEmployeeCursor..CEMG_PHON1
    REPLACE CEMG_PHON2 WITH &lcEmployeeCursor..CEMG_PHON2
    REPLACE CEMG_MOBIL WITH &lcEmployeeCursor..CEMG_MOBIL
    REPLACE CPAY_TYPE  WITH &lcEmployeeCursor..CPAY_TYPE
    REPLACE CPAYFREQ   WITH &lcEmployeeCursor..CPAYFREQ
    REPLACE NRATE      WITH &lcEmployeeCursor..NRATE
    REPLACE NHOURRATE  WITH &lcEmployeeCursor..NHOURRATE
    REPLACE NSALARY    WITH &lcEmployeeCursor..NSALARY
    REPLACE NOVERTRATE WITH &lcEmployeeCursor..NOVERTRATE
    REPLACE CCOSTCENT  WITH &lcEmployeeCursor..CCOSTCENT


    REPLACE LPADDRESS WITH llRpPrintAdd
    REPLACE LPEMG     WITH llRpPrintEmergency
    REPLACE LPQUAL    WITH llRpPrintQualification
    REPLACE LPPAYROLL WITH llRpPrintPayroll
    REPLACE LPSHIFT   WITH llRpPrintShifts

    SELECT (lcEmployeeCursor)
  ENDSCAN


  *-- Loop to fill addtional infotmation to Qulification
  WAIT WINDOW 'Creating temp. Qulification file.' NOWAIT 
  
  lcQualificationTable = loOGScroll.gfTempName()

  CREATE TABLE (oAriaApplication.WorkDir + lcQualificationTable) ;
               (CPERSON_ID C(8)  NULL, CQULFCODE C(6)  NULL, NPERCENT number(5, 2)  NULL, CQULFNAME C(30)  NULL)

  SELECT (lcQulificationCursor)
  LOCATE

  SCAN
    SELECT (lcQualificationTable)
    APPEND BLANK
    
    REPLACE CPERSON_ID WITH &lcQulificationCursor..CPERSON_ID
    REPLACE CQULFCODE  WITH &lcQulificationCursor..CQULFCODE
    REPLACE NPERCENT   WITH &lcQulificationCursor..NPERCENT
    
    = SEEK('N' + CQULFCODE + 'N' + 'CQULFCODE', "CODES")
    REPLACE CQULFNAME WITH CODES.CDISCREP

    SELECT (lcQulificationCursor)
  ENDSCAN

  *-- Loop to fill addtional infotmation to Shift
  WAIT WINDOW 'Creating temp. Shift file.' NOWAIT 
  
  lcShiftTable = loOGScroll.gfTempName()

  CREATE TABLE (oAriaApplication.WorkDir + lcShiftTable) ;
               (CPERSON_ID C(8)  NULL, CSHIFT_ID C(6)  NULL, CSHIFT_STR C(8)  NULL, CSHIFT_FNS C(8)  NULL)

  SELECT (lcShiftCursor)
  LOCATE

  SCAN
    SELECT (lcShiftTable)
    APPEND BLANK
    
    REPLACE CPERSON_ID WITH &lcShiftCursor..CPERSON_ID
    REPLACE CSHIFT_ID  WITH &lcShiftCursor..CSHIFT_ID
    REPLACE CSHIFT_STR WITH &lcShiftCursor..CSHIFT_STR
    REPLACE CSHIFT_FNS WITH &lcShiftCursor..CSHIFT_FNS

    SELECT (lcShiftCursor)
  ENDSCAN

  *-- Set Report Data Source
  DIMENSION loOGScroll.laCRTables[3]
  loOGScroll.laCRTables[1] = oAriaApplication.WorkDir + lcEmployeeTable + ".DBF"
  loOGScroll.laCRTables[2] = oAriaApplication.WorkDir + lcQualificationTable + ".DBF"
  loOGScroll.laCRTables[3] = oAriaApplication.WorkDir + lcShiftTable + ".DBF"

  DIMENSION loOGScroll.laCRTablesSubReport[3]
  loOGScroll.laCRTablesSubReport[1] = ""
  loOGScroll.laCRTablesSubReport[2] = "QUALIFY"
  loOGScroll.laCRTablesSubReport[3] = "EMPSHIFT"
  
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

  IF USED(lcQualificationTable)
    USE IN (lcQualificationTable)
  ENDIF

  IF USED(lcShiftTable)
    USE IN (lcShiftTable)
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
DECLARE loOgScroll.laRepModes[1]
loOgScroll.laRepModes[1] = "WINDOWS"
RETURN .T.

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


