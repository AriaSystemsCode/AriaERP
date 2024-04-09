*:***************************************************************************
*: Program file  : HRPLANT
*: Program desc. : Plant
*: System        : Aria Advantage Series ARIA4XP.
*: Module        : HR
*: Developer     : Mahmoud Said (MAH)
*! Date          : 05/07/2012
*: Reference     : N000681 - Convert Plant Report to A27 to A4XP
*:***************************************************************************
#INCLUDE r:\aria4xp\reports\hr\hrplant.h
IF llOgFltCh
* N000682 ,1 Thabet Handle globalization issues [Start]
*  WAIT WINDOW "Collecting Data......." NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Collect_Data NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Collect_Data,oAriaApplication.GetHeaderText("LANG_Collect_Data",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

* N000682 ,1 Thabet Handle globalization issues [END]
  DIMENSION loOgScroll.laCRParams[1,2]

  loOgScroll.laCRParams[1,1] = 'ReportName'
  * N000682 ,1 Thabet Handle globalization issues [Start]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loOgScroll.laCRParams[1,2] = LANG_Plant_Report
loOgScroll.laCRParams[1,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Plant_Report,oAriaApplication.GetHeaderText("LANG_Plant_Report",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  * N000682 ,1 Thabet Handle globalization issues [END]

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
* N000682 ,1 Thabet Handle globalization issues [Start]
*  WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcPlantCursor))) + ' Records.' NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Selected + ALLTRIM(STR(RECCOUNT(lcPlantCursor))) + LANG_Records NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Selected,oAriaApplication.GetHeaderText("LANG_Selected",AHEADERFILE)) + ALLTRIM(STR(RECCOUNT(lcPlantCursor))) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Records,oAriaApplication.GetHeaderText("LANG_Records",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

* N000682 ,1 Thabet Handle globalization issues [END]

  *-- Loop to fill addtional infotmation to Plant
* N000682 ,1 Thabet Handle globalization issues [Start]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Creating_temp_Plant NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Creating_temp_Plant,oAriaApplication.GetHeaderText("LANG_Creating_temp_Plant",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

* WAIT WINDOW 'Creating temp. Plant file.' NOWAIT
* N000682 ,1 Thabet Handle globalization issues [END]
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
* N000682 ,1 Thabet Handle globalization issues [Start]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Creating_temp_Employee NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Creating_temp_Employee,oAriaApplication.GetHeaderText("LANG_Creating_temp_Employee",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

*  WAIT WINDOW 'Creating temp. Employee file.' NOWAIT
* N000682 ,1 Thabet Handle globalization issues [END]
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
* N000682 ,1 Thabet Handle globalization issues [Start]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Creating_temp_Shift NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Creating_temp_Shift,oAriaApplication.GetHeaderText("LANG_Creating_temp_Shift",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

*  WAIT WINDOW 'Creating temp. Shift file.' NOWAIT
* N000682 ,1 Thabet Handle globalization issues [END]

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
  * N000682 ,1 Thabet Handle globalization issues [Start]
  *WAIT WINDOW 'Creating temp. Work Center file.' NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Creating_temp_Work_Center NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Creating_temp_Work_Center,oAriaApplication.GetHeaderText("LANG_Creating_temp_Work_Center",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

  * N000682 ,1 Thabet Handle globalization issues [END]

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
