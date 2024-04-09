*:***************************************************************************
*: Program file  : HRWRKCNT
*: Program desc. : Work Center
*: System        : Aria Advantage Series ARIA4XP.
*: Module        : HR
*: Developer     : Mahmoud Said (MAH)
*! Date          : 05/07/2012
*: Reference     : N000681 - Convert Work Center Report to A27 to A4XP
*:***************************************************************************
#INCLUDE r:\aria4xp\reports\hr\hrwrkcnt.h
IF llOgFltCh
* N000682 ,1 Thabet Handle globalization issues [Start]
  *WAIT WINDOW "Collecting Data......." NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Collect_Data NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Collect_Data,oAriaApplication.GetHeaderText("LANG_Collect_Data",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

* N000682 ,1 Thabet Handle globalization issues [END]
  DIMENSION loOgScroll.laCRParams[1,2]

  loOgScroll.laCRParams[1,1] = 'ReportName'
  * N000682 ,1 Thabet Handle globalization issues [Start]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loOgScroll.laCRParams[1,2] = LANG_Work_Center_Report
loOgScroll.laCRParams[1,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Work_Center_Report,oAriaApplication.GetHeaderText("LANG_Work_Center_Report",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  * N000682 ,1 Thabet Handle globalization issues [END]

  LOCAL lnSelect
  lnSelect = SELECT()

  LOCAL lcSqlStatment, lcSqlWhere, lnConnectionHandlar, lcWorkCenterCursor, lcMachineCursor, lcEmployeeCursor
  lcWorkCenterCursor   = loOGScroll.gfTempName()
  lcMachineCursor     = loOGScroll.gfTempName()
  lcEmployeeCursor     = loOGScroll.gfTempName()

  lcSqlStatment = "SELECT PEWCNTR.CWORKCENT, PEWCNTR.CDESC " + ;
                  ",PEWCNTR.CWCNTR_TYP, PEWCNTR.CWCNTR_BS" + ;
                  ",PEWCNTR.CWCNTR_LOD,PEWCNTR.CWCNTR_AVL" + ;
                  ",PEWCNTR.NMAN_MCH,PEWCNTR.NOVR_RATE" + ;
                  ",PEWCNTR.NSTAND_RAT,PEWCNTR.CPLANT_ID, PEPLANT.CPNAME "

  lcSqlStatment = lcSqlStatment + " FROM PEWCNTR LEFT JOIN PEPLANT ON (PEWCNTR.CPLANT_ID = PEPLANT.CPLANT_ID) "

  *-- Set filter
  LOCAL llAddWhere, lnPos, lcFilter, lnCounter

  lcSqlWhere = ""

  *-- Plant
  lnPos = ASUBSCRIPT(loOGScroll.laOGFXFlt, ASCAN(loOGScroll.laOGFXFlt,'PEWCNTR.CPLANT_ID'), 1)

  IF lnPos > 0
    lcFilter = loOGScroll.laOGFXFlt[lnPos, 6]

    IF !EMPTY(lcFilter) .AND. RECCOUNT(lcFilter) > 0
      IF !llAddWhere
        lcSqlWhere = lcSqlWhere + " WHERE ("
        llAddWhere = .T.
      ENDIF

      SELECT(lcFilter)
      LOCATE
      lcSqlWhere = lcSqlWhere + "(PEWCNTR.CPLANT_ID = '" + keyexp + "')";

      lnCounter = 0
      SCAN
        IF lnCounter > 0
          lcSqlWhere = lcSqlWhere + " OR (PEWCNTR.CPLANT_ID = '" + keyexp + "')"
        ENDIF
        lnCounter = lnCounter + 1
      ENDSCAN

      lcSqlWhere = lcSqlWhere + ")"
    ENDIF
  ENDIF

  *-- Work Center
  lnPos = ASUBSCRIPT(loOGScroll.laOGFXFlt, ASCAN(loOGScroll.laOGFXFlt,'PEWCNTR.CWORKCENT'), 1)

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
      lcSqlWhere = lcSqlWhere + "(PEWCNTR.CWORKCENT = '" + keyexp + "')";

      lnCounter = 0
      SCAN
        IF lnCounter > 0
          lcSqlWhere = lcSqlWhere + " OR (PEWCNTR.CWORKCENT = '" + keyexp + "')"
        ENDIF
        lnCounter = lnCounter + 1
      ENDSCAN

      lcSqlWhere = lcSqlWhere + ")"
    ENDIF
  ENDIF

  *-- Type
  IF ALLTRIM(lcRpType) <> "B"
    IF !llAddWhere
      lcSqlWhere = lcSqlWhere  + " WHERE "
      llAddWhere = .T.
    ELSE
      lcSqlWhere = lcSqlWhere  + " AND "
    ENDIF

    lcSqlWhere = lcSqlWhere + "PEWCNTR.CWCNTR_TYP = '" + ALLTRIM(lcRpType) + "'"
  ENDIF

  *-- Bapacity Based
  IF ALLTRIM(lcRpCapacityBased) <> "B"
    IF !llAddWhere
      lcSqlWhere = lcSqlWhere  + " WHERE "
      llAddWhere = .T.
    ELSE
      lcSqlWhere = lcSqlWhere  + " AND "
    ENDIF

     lcSqlWhere = lcSqlWhere + "PEWCNTR.CWCNTR_BS = '" + ALLTRIM(lcRpCapacityBased) + "'"
  ENDIF

  lcSqlStatment = lcSqlStatment + lcSqlWhere  + " ORDER BY CWORKCENT"

  lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment, lcWorkCenterCursor, "", oAriaApplication.ActiveCompanyConStr, 3, 'BROWSE', SET("DATASESSION"))

  lcSqlStatment = "SELECT CMACHINEID, CDESC," + ;
                  " CWORKCENT FROM PWMACHIN " +;
                  "WHERE CWORKCENT IN (SELECT CWORKCENT FROM PEWCNTR " + lcSqlWhere + ") ORDER BY CMACHINEID"
  lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment, lcMachineCursor, "", oAriaApplication.ActiveCompanyConStr, 3, 'BROWSE', SET("DATASESSION"))

  lcSqlStatment = "SELECT CPERSON_ID, CNAME, CWORKCENT " + ;
                  "FROM PEPERSON " + ;
                  "WHERE CWORKCENT IN (SELECT CWORKCENT FROM PEWCNTR " + lcSqlWhere + ") ORDER BY CPERSON_ID"
  lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment, lcEmployeeCursor, "", oAriaApplication.ActiveCompanyConStr, 3, 'BROWSE', SET("DATASESSION"))
* N000682 ,1 Thabet Handle globalization issues [Start]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Selected + ALLTRIM(STR(RECCOUNT(lcWorkCenterCursor))) + LANG_Records NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Selected,oAriaApplication.GetHeaderText("LANG_Selected",AHEADERFILE)) + ALLTRIM(STR(RECCOUNT(lcWorkCenterCursor))) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Records,oAriaApplication.GetHeaderText("LANG_Records",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

* WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcWorkCenterCursor))) + ' Records.' NOWAIT
* N000682 ,1 Thabet Handle globalization issues [END]

  *-- Loop to fill addtional infotmation to Work Center
 * N000682 ,1 Thabet Handle globalization issues [Start]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Creating_temp_Work NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Creating_temp_Work,oAriaApplication.GetHeaderText("LANG_Creating_temp_Work",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

* WAIT WINDOW 'Creating temp. Work Center file.' NOWAIT
* N000682 ,1 Thabet Handle globalization issues [END]
  lcWorkCenterTable = loOGScroll.gfTempName()

  CREATE TABLE (oAriaApplication.WorkDir + lcWorkCenterTable) ;
               (CWORKCENT C(6) NULL, CDESC c(20) NULL, CWCNTR_TYP C(1) NULL, CWCNTR_BS C(1) NULL, CWCNTR_LOD NUMBER(2, 0) NULL, ;
                CWCNTR_AVL NUMBER(6, 0) NULL, NMAN_MCH NUMBER(5, 2) NULL, ;
                NOVR_RATE NUMBER(5, 2) NULL, NSTAND_RAT NUMBER(5, 2) NULL, CPLANT_ID C(6) NULL, CPNAME C(30)  NULL, ;
                LPMACHINE L  NULL, LPEMP L NULL)

  SELECT (lcWorkCenterCursor)
  LOCATE

  SCAN
    SELECT (lcWorkCenterTable)
    APPEND BLANK

    REPLACE CWORKCENT  WITH &lcWorkCenterCursor..CWORKCENT
    REPLACE CDESC      WITH &lcWorkCenterCursor..CDESC
    REPLACE CWCNTR_TYP WITH &lcWorkCenterCursor..CWCNTR_TYP
    REPLACE CWCNTR_BS  WITH &lcWorkCenterCursor..CWCNTR_BS
    REPLACE CWCNTR_LOD WITH &lcWorkCenterCursor..CWCNTR_LOD
    REPLACE CWCNTR_AVL WITH &lcWorkCenterCursor..CWCNTR_AVL
    REPLACE NMAN_MCH   WITH &lcWorkCenterCursor..NMAN_MCH
    REPLACE NOVR_RATE  WITH &lcWorkCenterCursor..NOVR_RATE
    REPLACE NSTAND_RAT WITH &lcWorkCenterCursor..NSTAND_RAT
    REPLACE CPLANT_ID  WITH &lcWorkCenterCursor..CPLANT_ID
    REPLACE CPNAME     WITH &lcWorkCenterCursor..CPNAME

    REPLACE LPMACHINE WITH llRpPrintMachines
    REPLACE LPEMP     WITH llRpPrintHumans

    SELECT (lcWorkCenterCursor)
  ENDSCAN


  *-- Loop to fill addtional infotmation to Machine
* N000682 ,1 Thabet Handle globalization issues [Start]
*  WAIT WINDOW 'Creating temp. Machine file.' NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Creating_temp_Machine_file NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Creating_temp_Machine_file,oAriaApplication.GetHeaderText("LANG_Creating_temp_Machine_file",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

* N000682 ,1 Thabet Handle globalization issues [Start]
  lcMachineTable = loOGScroll.gfTempName()

  CREATE TABLE (oAriaApplication.WorkDir + lcMachineTable) ;
               (CMACHINEID C(6) NULL, CDESC C(20) NULL, CWORKCENT C(6) NULL)

  SELECT (lcMachineCursor)
  LOCATE

  SCAN
    SELECT (lcMachineTable)
    APPEND BLANK

    REPLACE CMACHINEID WITH &lcMachineCursor..CMACHINEID
    REPLACE CDESC      WITH &lcMachineCursor..CDESC
    REPLACE CWORKCENT  WITH &lcMachineCursor..CWORKCENT

    SELECT (lcMachineCursor)
  ENDSCAN

  *-- Loop to fill addtional infotmation to Employee
* N000682 ,1 Thabet Handle globalization issues [Start]
*  WAIT WINDOW 'Creating temp. Employee file.' NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Creating_temp_Employee_file NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Creating_temp_Employee_file,oAriaApplication.GetHeaderText("LANG_Creating_temp_Employee_file",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

* N000682 ,1 Thabet Handle globalization issues [END]
  lcEmployeeTable = loOGScroll.gfTempName()

  CREATE TABLE (oAriaApplication.WorkDir + lcEmployeeTable) ;
               (CPERSON_ID C(8) NULL, CNAME C(30)  NULL, CWORKCENT C(6)  NULL)

  SELECT (lcEmployeeCursor)
  LOCATE

  SCAN
    SELECT (lcEmployeeTable)
    APPEND BLANK

    REPLACE CPERSON_ID WITH &lcEmployeeCursor..CPERSON_ID
    REPLACE CNAME      WITH &lcEmployeeCursor..CNAME
    REPLACE CWORKCENT  WITH &lcEmployeeCursor..CWORKCENT

    SELECT (lcEmployeeCursor)
  ENDSCAN

  *-- Set Report Data Source
  DIMENSION loOGScroll.laCRTables[3]
  loOGScroll.laCRTables[1] = oAriaApplication.WorkDir + lcWorkCenterTable + ".DBF"
  loOGScroll.laCRTables[2] = oAriaApplication.WorkDir + lcMachineTable + ".DBF"
  loOGScroll.laCRTables[3] = oAriaApplication.WorkDir + lcEmployeeTable + ".DBF"

  DIMENSION loOGScroll.laCRTablesSubReport[3]
  loOGScroll.laCRTablesSubReport[1] = ""
  loOGScroll.laCRTablesSubReport[2] = "Machine"
  loOGScroll.laCRTablesSubReport[3] = "Employee"

  WAIT CLEAR

  SELECT (lcWorkCenterTable)
  IF !RECCOUNT() > 0
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF

  IF USED(lcWorkCenterTable)
    USE IN (lcWorkCenterTable)
  ENDIF

  IF USED(lcMachineTable)
    USE IN (lcMachineTable)
  ENDIF

  IF USED(lcEmployeeTable)
    USE IN (lcEmployeeTable)
  ENDIF

  =gfDispRe()
ELSE
  lcWorkCenterTable = loOGScroll.gfTempName()

  USE loOGScroll.laCRTables[1] IN 0 ALIAS (lcWorkCenterTable) AGAIN

  SELECT (lcWorkCenterTable)
  IF !RECCOUNT() > 0
    *-- Message : There are no records to display...!
    *--                < Ok >
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ENDIF

  IF USED(lcWorkCenterTable)
    USE IN (lcWorkCenterTable)
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
