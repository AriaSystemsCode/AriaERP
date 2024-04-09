*:***************************************************************************
*: Program file  : HRPLANT
*: Program desc. : Plant
*: System        : Aria Advantage Series ARIA4XP.
*: Module        : HR
*: Developer     : Mahmoud Said (MAH)
*! Date          : 05/07/2012
*: Reference     : N000681 - Convert Plant Report to A27 to A4XP
*:***************************************************************************

IF llOgFltCh
  WAIT WINDOW "Collecting Data......." NOWAIT 
  
  DIMENSION loOgScroll.laCRParams[1,2]

  loOgScroll.laCRParams[1,1] = 'ReportName'
  loOgScroll.laCRParams[1,2] = 'Plant Report'
  
  LOCAL lnSelect
  lnSelect = SELECT()

  LOCAL lcSqlStatment, lcSqlWhere, lnConnectionHandlar, lcPlantCursor, lcEmployeeCursor, lcShiftCursor, lcWorkCenterCursor
  lcPlantCursor       = loOGScroll.gfTempName()
  lcEmployeeCursor    = loOGScroll.gfTempName()
  lcShiftCursor       = loOGScroll.gfTempName()
  lcWorkCenterCursor  = loOGScroll.gfTempName()
  
  lcSqlStatment = "SELECT CPLANT_ID, CPNAME " + ;
                  ",CPHONE, CPHONE2" + ;
                  ",CPADDRESS1, CPADDRESS2, CPADDRESS3, CPADDRESS4, CPADDRESS5, CPADDRESS6, CE_MAIL "

  lcSqlStatment = lcSqlStatment + " FROM PEPLANT "

  *-- Set filter
  LOCAL llAddWhere, lnPos, lcFilter, lnCounter

  lcSqlWhere = ""
  
  *-- Plant
  lnPos = ASUBSCRIPT(loOGScroll.laOGFXFlt, ASCAN(loOGScroll.laOGFXFlt,'PEPLANT.CPLANT_ID'), 1)

  IF lnPos > 0
    lcFilter = loOGScroll.laOGFXFlt[lnPos, 6]

    IF !EMPTY(lcFilter) .AND. RECCOUNT(lcFilter) > 0
      IF !llAddWhere
        lcSqlWhere = lcSqlWhere + " WHERE ("
        llAddWhere = .T.
      ENDIF

      SELECT(lcFilter)
      LOCATE
      lcSqlWhere = lcSqlWhere + "(PEPLANT.CPLANT_ID = '" + keyexp + "')";

      lnCounter = 0
      SCAN
        IF lnCounter > 0
          lcSqlWhere = lcSqlWhere + " OR (PEPLANT.CPLANT_ID = '" + keyexp + "')"
        ENDIF
        lnCounter = lnCounter + 1
      ENDSCAN

      lcSqlWhere = lcSqlWhere + ")"
    ENDIF
  ENDIF
      
  lcSqlStatment = lcSqlStatment + lcSqlWhere + " ORDER BY CPLANT_ID"

  lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment, lcPlantCursor, "", oAriaApplication.ActiveCompanyConStr, 3, 'BROWSE', SET("DATASESSION"))
  
  lcSqlStatment = "SELECT CPERSON_ID, CNAME, CPLANT_ID FROM PEPERSON " + ;
                  "WHERE CPLANT_ID IN (SELECT CPLANT_ID FROM PEPLANT " + lcSqlWhere + ") ORDER BY CPERSON_ID"
  lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment, lcEmployeeCursor, "", oAriaApplication.ActiveCompanyConStr, 3, 'BROWSE', SET("DATASESSION"))
  
  lcSqlStatment = "SELECT CSHIFT_ID, CSHIFT_STR, CSHIFT_FNS, CPLANT_ID " + ;
                  "FROM PESHIFT " + ;
                  "WHERE CPLANT_ID IN (SELECT CPLANT_ID FROM PEPLANT " + lcSqlWhere + ") ORDER BY CSHIFT_ID"
  lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment, lcShiftCursor, "", oAriaApplication.ActiveCompanyConStr, 3, 'BROWSE', SET("DATASESSION"))
  
  lcSqlStatment = "SELECT CWORKCENT, CDESC, CWCNTR_TYP, CWCNTR_BS, CWCNTR_AVL, NOVR_RATE, NSTAND_RAT,CPLANT_ID " + ;
                  "FROM PEWCNTR " + ;
                  "WHERE CPLANT_ID IN (SELECT CPLANT_ID FROM PEPLANT " + lcSqlWhere + ") ORDER BY CWORKCENT"
  lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment, lcWorkCenterCursor, "", oAriaApplication.ActiveCompanyConStr, 3, 'BROWSE', SET("DATASESSION"))

  WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcPlantCursor))) + ' Records.' NOWAIT


  *-- Loop to fill addtional infotmation to Plant
  WAIT WINDOW 'Creating temp. Plant file.' NOWAIT 
  
  lcPlantTable = loOGScroll.gfTempName()

  CREATE TABLE (oAriaApplication.WorkDir + lcPlantTable) ;
               (CPLANT_ID C(6) NULL, CPNAME c(30) NULL, CPHONE C(16) NULL, CPHONE2 C(16) NULL, ;
                CPADDRESS1 C(30) NULL, CPADDRESS2 C(30)  NULL, CPADDRESS3 C(30)  NULL, ;
                CPADDRESS4 C(30) NULL, CPADDRESS5 C(30)  NULL, CPADDRESS6 C(30)  NULL, CE_MAIL C(30)  NULL, ;
                LPEMP L  NULL, LPSHIFT L NULL, LPWRKCNT L NULL)

  SELECT (lcPlantCursor)
  LOCATE

  SCAN
    SELECT (lcPlantTable)
    APPEND BLANK
    
    REPLACE CPLANT_ID  WITH &lcPlantCursor..CPLANT_ID
    REPLACE CPNAME     WITH &lcPlantCursor..CPNAME
    REPLACE CPHONE     WITH &lcPlantCursor..CPHONE
    REPLACE CPHONE2    WITH &lcPlantCursor..CPHONE2
    REPLACE CPADDRESS1 WITH &lcPlantCursor..CPADDRESS1
    REPLACE CPADDRESS2 WITH &lcPlantCursor..CPADDRESS2
    REPLACE CPADDRESS3 WITH &lcPlantCursor..CPADDRESS3
    REPLACE CPADDRESS4 WITH &lcPlantCursor..CPADDRESS4
    REPLACE CPADDRESS5 WITH &lcPlantCursor..CPADDRESS5
    REPLACE CPADDRESS6 WITH &lcPlantCursor..CPADDRESS6
    REPLACE CE_MAIL    WITH &lcPlantCursor..CE_MAIL

    REPLACE LPEMP      WITH llRpPrintEmps
    REPLACE LPSHIFT    WITH llRpPrintShifts
    REPLACE LPWRKCNT   WITH llRpPrintWorkCents

    SELECT (lcPlantCursor)
  ENDSCAN


  *-- Loop to fill addtional infotmation to Employee
  WAIT WINDOW 'Creating temp. Employee file.' NOWAIT 
  
  lcEmployeeTable = loOGScroll.gfTempName()
                  
  lcSqlStatment = "SELECT CPERSON_ID, CNAME, CPLANT_ID FROM PEPERSON " + ;
                  "WHERE CPLANT_ID IN (SELECT CPLANT_ID FROM PEPLANT " + lcSqlWhere + ") ORDER BY CPERSON_ID"

  CREATE TABLE (oAriaApplication.WorkDir + lcEmployeeTable) ;
               (CPERSON_ID C(8) NULL, CNAME C(30) NULL, CPLANT_ID C(6) NULL)

  SELECT (lcEmployeeCursor)
  LOCATE

  SCAN
    SELECT (lcEmployeeTable)
    APPEND BLANK
    
    REPLACE CPERSON_ID WITH &lcEmployeeCursor..CPERSON_ID
    REPLACE CNAME      WITH &lcEmployeeCursor..CNAME
    REPLACE CPLANT_ID  WITH &lcEmployeeCursor..CPLANT_ID
    
    SELECT (lcEmployeeCursor)
  ENDSCAN

  *-- Loop to fill addtional infotmation to Shift
  WAIT WINDOW 'Creating temp. Shift file.' NOWAIT 
  
  lcShiftTable = loOGScroll.gfTempName()

  CREATE TABLE (oAriaApplication.WorkDir + lcShiftTable) ;
               (CSHIFT_ID C(6) NULL, CSHIFT_STR C(8)  NULL, CSHIFT_FNS C(8)  NULL, CPLANT_ID C(6)  NULL)

  SELECT (lcShiftCursor)
  LOCATE

  SCAN
    SELECT (lcShiftTable)
    APPEND BLANK
    
    REPLACE CSHIFT_ID  WITH &lcShiftCursor..CSHIFT_ID
    REPLACE CSHIFT_STR WITH &lcShiftCursor..CSHIFT_STR
    REPLACE CSHIFT_FNS WITH &lcShiftCursor..CSHIFT_FNS
    REPLACE CPLANT_ID  WITH &lcShiftCursor..CPLANT_ID

    SELECT (lcShiftCursor)
  ENDSCAN

  *-- Loop to fill addtional infotmation to Work Center
  WAIT WINDOW 'Creating temp. Work Center file.' NOWAIT 
  
  lcWorkCenterTable = loOGScroll.gfTempName()

  CREATE TABLE (oAriaApplication.WorkDir + lcWorkCenterTable) ;
               (CWORKCENT C(6) NULL, CDESC C(20)  NULL, CWCNTR_TYP C(1)  NULL, CWCNTR_BS C(1)  NULL, ;
                CWCNTR_AVL NUMBER(6, 0) NULL, NOVR_RATE NUMBER(5, 2) NULL, NSTAND_RAT NUMBER(5, 2) NULL, CPLANT_ID C(6) NULL)

  SELECT (lcWorkCenterCursor)
  LOCATE

  SCAN
    SELECT (lcWorkCenterTable)
    APPEND BLANK
    
    REPLACE CWORKCENT  WITH &lcWorkCenterCursor..CWORKCENT
    REPLACE CDESC      WITH &lcWorkCenterCursor..CDESC 
    REPLACE CWCNTR_TYP WITH &lcWorkCenterCursor..CWCNTR_TYP
    REPLACE CWCNTR_BS  WITH &lcWorkCenterCursor..CWCNTR_BS
    REPLACE CWCNTR_AVL WITH &lcWorkCenterCursor..CWCNTR_AVL
    REPLACE NOVR_RATE  WITH &lcWorkCenterCursor..NOVR_RATE
    REPLACE NSTAND_RAT WITH &lcWorkCenterCursor..NSTAND_RAT
    REPLACE CPLANT_ID  WITH &lcWorkCenterCursor..CPLANT_ID

    SELECT (lcWorkCenterCursor)
  ENDSCAN

  *-- Set Report Data Source
  DIMENSION loOGScroll.laCRTables[4]
  loOGScroll.laCRTables[1] = oAriaApplication.WorkDir + lcPlantTable + ".DBF"
  loOGScroll.laCRTables[2] = oAriaApplication.WorkDir + lcEmployeeTable + ".DBF"
  loOGScroll.laCRTables[3] = oAriaApplication.WorkDir + lcShiftTable + ".DBF"
  loOGScroll.laCRTables[4] = oAriaApplication.WorkDir + lcWorkCenterTable + ".DBF"

  DIMENSION loOGScroll.laCRTablesSubReport[4]
  loOGScroll.laCRTablesSubReport[1] = ""
  loOGScroll.laCRTablesSubReport[2] = "Employees"
  loOGScroll.laCRTablesSubReport[3] = "Shifts"
  loOGScroll.laCRTablesSubReport[4] = "WorkCenter"
  
  WAIT CLEAR
  
  SELECT (lcPlantTable)
  IF !RECCOUNT() > 0
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF

  IF USED(lcPlantTable)
    USE IN (lcPlantTable)
  ENDIF

  IF USED(lcEmployeeTable)
    USE IN (lcEmployeeTable)
  ENDIF

  IF USED(lcShiftTable)
    USE IN (lcShiftTable)
  ENDIF

  IF USED(lcWorkCenterTable)
    USE IN (lcWorkCenterTable)
  ENDIF

  =gfDispRe()
ELSE
  lcPlantTable = loOGScroll.gfTempName()
  
  USE loOGScroll.laCRTables[1] IN 0 ALIAS (lcPlantTable) AGAIN
  
  SELECT (lcPlantTable)
  IF !RECCOUNT() > 0
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF

  IF USED(lcPlantTable)
    USE IN (lcPlantTable)
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