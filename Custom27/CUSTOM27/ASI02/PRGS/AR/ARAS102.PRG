*!********************************************************************
*: Program file  : ARAS102.PRG  C#200165
*: Program desc. : Forecasting.
*: For screen    : None.
*:         System: ARIA APPAREL SERIES
*:         Module: AR - Account recievable
*:      Developer: Ahmed Salah Shalaby - (SSH)
*!********************************************************************
*: Calls         : 
*!********************************************************************
*: Passed Parameters  :
*!********************************************************************
*: Example            : DO 
*!********************************************************************
* Modifications
***************************************************************************

*--- Variable Initialization
DIMENSION laData[6], laScrMode[4], laKeyField[1,4]
lcTmpQuery = ""
ldTmpStar  = {}
ldLstDay   = {}
ldfrsDay   = {}
lcFrstWD   = ""
lcLastWD   = ""
lcTmpCust  = ""
lcOldVal   = ""
lcScFields = ""
STORE '' TO laData
DIMENSION laTarget[1]
laTarget[1] = ""
DIMENSION laSource[1]
llModCond =  "laScrMode[3].OR.laScrMode[4]"
*--- Initial Browse title.
FOR lnInde = 1 TO 7
 lcIndex = STR(lnInde,1)
 llDay&lcIndex = .T.
 lnTotal&lcIndex = 0
ENDFOR
llCondition = "(lnCurrWek>1 .AND. lnCurrWek<laData[6])"
llCond1 = "llDay1 .AND. EVAL(llModCond) .OR. EVAL(llCondition)"
llCond2 = "llDay2 .AND. EVAL(llModCond) .OR. EVAL(llCondition)"
llCond3 = "llDay3 .AND. EVAL(llModCond) .OR. EVAL(llCondition)"
llCond4 = "llDay4 .AND. EVAL(llModCond) .OR. EVAL(llCondition)"
llCond5 = "llDay5 .AND. EVAL(llModCond) .OR. EVAL(llCondition)"
llCond6 = "llDay6 .AND. EVAL(llModCond) .OR. EVAL(llCondition)"
llCond7 = "llDay7 .AND. EVAL(llModCond) .OR. EVAL(llCondition)"

lnWeekTot = 0
lnCurrWek = 1
lcWDay1 = "Monday"
lcWDay2 = "Tuesday"
lcWDay3 = "Wednesday"
lcWDay4 = "Thursday"
lcWDay5 = "Friday"
lcWDay6 = "Saturday"
lcWDay7 = "Sunday"

lcDDay1 = "Sunday"
lcDDay2 = "Monday"
lcDDay3 = "Tuesday"
lcDDay4 = "Wednesday"
lcDDay5 = "Thursday"
lcDDay6 = "Friday"
lcDDay7 = "Saturday"

lcLWeKDay  = ""
*--- Screen Mode Array
STORE .F. TO laScrMode
lcDescForm = ""
laScrMode[1] = .T.
lcMtBrowTt = "Week#   "+STR(lnCurrWek,3)
laKeyField[1,1] = "laData[1]"
laKeyField[1,2] =.F.
laKeyField[1,3] = ""
laKeyField[1,4] = 1

IF !gfSetup()
  RETURN
ENDIF

lcDescForm = REPLICATE("X",40)
laDefProc[9]  = .F.  && Disable the control panel save proc.  (lpSavScr)
laDefProc[7]  = .F.  && Disable the control panel delete proc.(lpDelScr)
lcScFields = "CFRCCODE,CFRCDESC,DFRCFROM,DFRCTO,NFRCTOTL,NFRCNOWK"
SELECT ForCastH

IF !WEXIST(gcBaseWind)
  lcTmpQuery = gfTempName()
  lcTmpCust  = gfTempName()
  lcDymmy    = lfCretBr()
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
ELSE
  IF WEXIST("gwcContrl1")
    RELEASE WINDOW gwcContrl1
  ENDIF
ENDIF
DO (gcScrDir + gcWinAppl + '\ARASFOR.SPR')
POP KEY
RELEASE WINDOW (lcMtBrowTt)
RELEASE PAD _Option OF _MSYSMENU
RELEASE BAR 099 OF P01PU01
RELEASE BAR 100 OF P01PU01

*!*************************************************************
*! Name      : lfActsBrow  C#200165
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 02/15/01
*! Purpose   : Activate the browse.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfActsBrow()
*!*************************************************************
FUNCTION lfActsBrow

SELECT &lcTmpQuery
lcBrfield1 = [cMark   :02 :H = ''            :R ,] +;
             [Account :10 :H = "Customer"    :R ,] +;
             [Day1    :10 :V = lfvQty("1");
                            :P = '99999999.99':E = '':F ;
                            :H = lcWDay1 :W = EVAL(llCond1),]+;
             [Day2    :10 :V = lfvQty("2");
                            :P = '99999999.99':E = '':F ;
                            :H = lcWDay2 :W = EVAL(llCond2),]+;
             [Day3    :10 :V = lfvQty("3");
                            :P = '99999999.99';
                            :E = '':F ;
                            :H = lcWDay3 :W = EVAL(llCond3),]+;
             [Day4    :10 :V = lfvQty("4");
                            :P = '99999999.99':E = '':F ;
                            :H = lcWDay4 :W = EVAL(llCond4),]+;
             [Day5    :10 :V = lfvQty("5");
                            :P = '99999999.99';
                            :E = '':F ;
                            :H = lcWDay5 :W = EVAL(llCond5),]+;
             [Day6    :10 :V = lfvQty("6");
                          :P = '99999999.99':E = '':F ;
                          :H = lcWDay6 :W = EVAL(llCond6),]+;
             [Day7    :10 :V = lfvQty("7");
                            :P = '99999999.99':E = '':F ;
                            :H = lcWDay7 :W = EVAL(llCond7),]+;
             [TotQty  :10   :H = "Total"        :R]

BROWSE FIELDS &lcBrField1;
       WHEN lfwBrows();
       FOR ALLTRIM(STR(lnCurrWek,3)) == ALLTRIM(WEEK) .AND. !llDel;
       VALID :F lfvBrowse() ;
       WINDOW ARASFOR2 IN WINDOW (gcBaseWind);
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOMENU;
       NOWAIT;
       SAVE;
       TITLE ALLTRIM(lcMtBrowTt)

*!*************************************************************
*! Name      : lfUnTrap  C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/15/01
*! Purpose   : UnTrap function for Browse
*!           : Activate screen function....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfTrap()
*!*************************************************************
FUNCTION lfUnTrap

* -- Clear Trap
IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF

*-- If TOP window is not one of the browses window                                        
IF ALLTRIM(WONTOP()) # ALLTRIM((lcMtBrowTt))
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL CTRL+TAB
  ON KEY LABEL CTRL+ENTER
  ON KEY LABEL CTRL+HOME
  ON KEY LABEL CTRL+END
  ON KEY LABEL CTRL+W
  ON KEY LABEL ENTER
ENDIF
*-- end of lfUnTrap.

*!*************************************************************
*! Name      : lfKey  C#200165
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 02/15/01
*! Purpose   : Screen ICINVLK When Function.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfKey()
*!*************************************************************
*!
FUNCTION lfKey
PRIVATE lcTempAct1
PUSH KEY
IF laScrMode[2] .OR. laScrMode[3]
    SHOW GET laData[3] DISABLE
    SHOW GET laData[4] DISABLE
    SHOW GET laData[1] DISABLE
ENDIF
SHOW GET pbUSRFIELD DISABLE
DEFINE BAR 099 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 100 OF P01PU01 PROMPT lcMtBrowTt KEY ALT+B
ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcMtBrowTt)

*!*************************************************************
*! Name      : lfTrap C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/15/01
*! Purpose   : Trap function for Browse....
*!           : Deactivate screen function.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpTab,lpBackTab
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfTrap()
*!*************************************************************
FUNCTION lfTrap

*-- if TOP window is one of the browse window
IF ALLTRIM(WONTOP()) = ALLTRIM((lcMtBrowTt))
  glFromBrow = .T.
  ON KEY LABEL CTRL+ENTER lnDummy = 1		&& Do nothing
  ON KEY LABEL CTRL+HOME  lnDummy = 1		&& Do nothing
  ON KEY LABEL CTRL+W     lnDummy = 1		&& Do nothing
  ON KEY LABEL CTRL+END   lnDummy = 1		&& Do nothing
  ON KEY LABEL TAB        DO lpTab
  ON KEY LABEL CTRL+TAB   DO lpTab
  ON KEY LABEL BACKTAB    DO lpBackTab
ENDIF
*-- end of lfTrap.


*!*************************************************************
*! Name      : lpTab  C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/15/01
*! Purpose   : Trapping TAB order for browse window
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpTab
*!*************************************************************
PROCEDURE lpTab

ON KEY LABEL TAB
ON KEY LABEL CTRL+TAB
*-- Go to next window
IF ALLTRIM(WONTOP()) = ALLTRIM((lcMtBrowTt))
   ACTIVATE WINDOW ARASFOR3
  _CUROBJ = OBJNUM(lcDirseek)
ELSE
   ACTIVATE WINDOW (lcMtBrowTt)
  _CUROBJ = OBJNUM(ibBBrow)
ENDIF
*-- end of lpTab.

*!*************************************************************
*! Name      : lpBackTab  C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/15/01
*! Purpose   : Trapping BACKTAB order for browse window
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpBackTab
*!*************************************************************
PROCEDURE lpBackTab

ON KEY LABEL BACKTAB
IF ALLTRIM(WONTOP()) = ALLTRIM((lcMtBrowTt))
  *-- Go to previous window
  ACTIVATE WINDOW ARASFOR1		&&Activate popup windows
  IF laScrMode[1]
    _CUROBJ = OBJNUM(laData[1])
  ELSE
    _CUROBJ = OBJNUM(laData[2])
  ENDIF
ELSE
   ACTIVATE WINDOW (lcMtBrowTt)
  _CUROBJ = OBJNUM(ibBBrow) && Activate control panel window
ENDIF  
*-- end of lpBackTab.

*!*************************************************************
*! Name      : lfCretBr  C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/15/01
*! Purpose   : Create tempfile.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCretBr()
*!*************************************************************
*!
FUNCTION lfCretBr

CREATE TABLE gcWorkDir+(lcTmpQuery);
             (cMark C(1)   , cSel C(1)   ,Account C(5),Week C(3),;
              Day1  N(15,2), Day2 N(15,2),Day3 N(15,2) , Day4 N(15,2),;
              Day5  N(15,2), Day6 N(15,2),Day7 N(15,2) , Day8 N(15,2) , TotQty N(15,2) , llDel L)
SELECT(lcTmpQuery)
INDEX ON Account+Week TAG (lcTmpQuery)


*!*************************************************************
*! Name      : lfwBrows  C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/15/01
*! Purpose   : When Browse Temp. sequence file fn.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfShowBtns
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwBrows()
*!*************************************************************
*!
FUNCTION lfwBrows

SELECT (lcTmpQuery)
lnNewRec = RECNO()
REPLACE ALL cMark WITH ' '
GO lnNewRec
IF llDel
  LOCATE FOR !llDel
ENDIF
REPLACE cMark WITH '>'
SHOW WINDOW (lcMtBrowTt) REFRESH

*-- end of lfwBrows.

*!*************************************************************
*! Name      : lfvBrowse C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/15/01
*! Purpose   : Valid Browse Temp. sequence file fn.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvBrowse()
*!*************************************************************
*!
FUNCTION lfvBrowse

IF ALLTRIM(WONTOP()) # ALLTRIM((lcMtBrowTt))
  glFromBrow = .T.
  = gfStopBrow()
ENDIF
*-- end of lfvBrowse.


*!*************************************************************
*! Name      : lpShow C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/15/01
*! Purpose   : Show function.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lpShow()
*!*************************************************************
*!
FUNCTION lpShow


DO CASE
  *--- Case Select Mode Screen
  CASE laScrMode[1]
    SELECT (lcTmpQuery)
    DELETE ALL
    =lfRest()
  *--- Case View Mode Screen
  CASE laScrMode[2]
    =lfEnbDis("DISABLE")
    =lfGetData(laData[1])
    SHOW GET laData[3] DISABLE
    SHOW GET laData[4] DISABLE
    SHOW GET laData[1] DISABLE
    SHOW WINDOW (lcMtBrowTt) REFRESH
   *--- Case Edit Mode Screen
  CASE laScrMode[3]
    =lfEnbDis("ENABLE")
    IF !EOF(lcTmpQuery)
      SHOW GET pbdelCst ENABLE
    ELSE
      SHOW GET pbdelCst DISABLE
    ENDIF
    SHOW GET laData[1] DISABLE
  *--- Case Add New Mode Screen
  CASE laScrMode[4]
    =lfEnbDis("ENABLE",'4')
    IF !EOF(lcTmpQuery)
      SHOW GET pbdelCst ENABLE
    ELSE
      SHOW GET pbdelCst DISABLE
    ENDIF
ENDCASE
SELECT (lcTmpQuery)
GO TOP
IF EOF()
  SHOW GET pbdelCst DISABLE
  SHOW GET pbPreCst DISABLE
  SHOW GET pbNxtCst DISABLE
ENDIF
IF laData[6] = 1
  SHOW GET pbPreCst DISABLE
  SHOW GET pbNxtCst DISABLE
ENDIF
SHOW GET pbUSRFIELD DISABLE


*!*************************************************************
*! Name      : lfvData1 C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/15/01
*! Purpose   : Valid forecasting code.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvData1()
*!*************************************************************
*!
FUNCTION lfvData1
PARAMETER lcBrow

PRIVATE lnOldAls , lnvAction

lnOldAls = SELECT(0)
lnvAction = 7
IF TYPE("lcBrow") = "C" .OR. "?" $ ALLTRIM(laData[1])
  lnvAction = 1
ELSE
  IF !EMPTY(laData[1]) 
    lnvAction = IIF(!SEEK(laData[1],'forcasth'),gfModalGen('INM00001B00001',.F., "Forecast code: "+laData[1]),4)
  ENDIF
ENDIF
=lfCase(lnvAction)
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfCase C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/15/01
*! Purpose   : Valid forecasting code.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCase()
*!*************************************************************
*!
FUNCTION lfCase
PARAMETER lnAction

DO CASE
  CASE lnAction = 0  &&  Invalid input
    _CUROBJ = OBJNUM(laData[1])
  CASE lnAction = 1  &&  Browse
    =gfCPBrows()
  CASE lnAction = 2  &&  Add
    laScrMode = .F.
    llCUpDate = .T.
    laScrMode[4] = .T.
    SHOW GETS
  CASE lnAction = 3  &&  Reenter
    laData[1] = ''
    SHOW GET laData[1]
    _CUROBJ = OBJNUM(laData[1])
  CASE lnAction = 4  &&  Found In DBF
    =lfGetData(laData[1])
    laScrMode = .F.
    laScrMode[2] = .T.
    SHOW GETS
ENDCASE

*!*************************************************************
*! Name      : lfvData3 C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/15/01
*! Purpose   : Valid firest Date .
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvData3()
*!*************************************************************
*!
FUNCTION lfvData3

llCUpDate = (lcOldVal<>laData[3])
IF laData[3] = {}
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Invalid date.')
  _CUROBJ = OBJNUM(laData[3])
ELSE
  IF laData[4] <> {}
    =lfVData4()
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfVData4 C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/15/01
*! Purpose   : Valid second Date .
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfVData4()
*!*************************************************************
*!
FUNCTION lfVData4
PRIVATE lnOldAls

lnOldAls = SELECT(0)
IF laData[3] > laData[4] .OR. laData[4] = {}
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Invalid date range')
  laData[4] = {}
  _CUROBJ = OBJNUM(laData[4])
ELSE
  laData[6] = CEILING(((laData[4]-ladata[3])+1)/7)
  IF (laScrMode[3] .OR.  laScrMode[4]) .AND. laData[6] <> 0
    SHOW GET pbAddCst ENABLE 
  ENDIF
  IF TYPE("lcOldVal") = "D"
    llCUpDate = (lcOldVal<>laData[4])
  ENDIF
  =lfColmSta()
  =lfClmStat("B")
  =lfActsBrow()
  SHOW GET laData[6]
ENDIF
SELECT(lnOldAls)


*!*************************************************************
*! Name      : lfEnbDis C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/15/01
*! Purpose   : Enable/Disable Function.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfEnbDis()
*!*************************************************************
*!
FUNCTION lfEnbDis
PARAMETER lcStat,lcFrom


IF TYPE("lcFrom") <> "C"
  SHOW GET pbAddCst &lcStat
ELSE
  SHOW GET pbAddCst DISABLE
ENDIF
IF lcStat = "ENABLE"
  SHOW GET pbdelCst ENABLE
  IF lnCurrWek <> 1
    SHOW GET pbPreCst ENABLE
  ELSE
    SHOW GET pbPreCst DISABLE
  ENDIF
  IF lnCurrWek <> laData[6]
    SHOW GET pbNxtCst ENABLE
  ELSE
    SHOW GET pbNxtCst DISABLE
  ENDIF
ELSE
  SHOW GET pbNxtCst &lcStat
  SHOW GET pbPreCst &lcStat
ENDIF

*!*************************************************************
*! Name      : lfWOld C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/15/01
*! Purpose   : When Function to get the old value.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfWOld()
*!*************************************************************
*!
FUNCTION lfWOld
lcOldVal = EVAL(VARREAD())


*!*************************************************************
*! Name      : lfvQty C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/15/01
*! Purpose   : Valid qty function.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvQty()
*!*************************************************************
*!
FUNCTION lfvQty
PARAMETER lcCurInd

PRIVATE lnOldAls
IF Day&lcCurInd < 0
  REPLACE Day&lcCurInd WITH 0
  RETURN
ENDIF
lnOldAls = SELECT(0)
SELECT (lcTmpQuery)
lnOldQty = TOTQTY
REPLACE TOTQTY WITH 0
FOR lnIndex = 1 TO 7 
  lcQtyNo = STR(lnIndex,1)
  REPLACE TOTQTY WITH TOTQTY+Day&lcQtyNo
ENDFOR
lnTotal&lcCurInd = lnTotal&lcCurInd + (IIF(lnOldQty<>0,TOTQTY-lnOldQty,TOTQTY))
lnWeekTot = lnWeekTot + (IIF(lnOldQty<>0,TOTQTY-lnOldQty,TOTQTY))
laData[5] = laData[5]+(IIF(lnOldQty<>0,TOTQTY-lnOldQty,TOTQTY))
SHOW GET laData[5]
=lfRefresh()
SELECT(lnOldAls)


*!*************************************************************
*! Name      : lfvAddCst C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/15/01
*! Purpose   : Button Add customer valid function.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvAddCst()
*!*************************************************************
*!
FUNCTION lfvAddCst

PRIVATE lnOldAls
lnOldAls = SELECT(0)
=gfOpenFile(gcdataDir+"CUSTOMER","CUSTOMER","SH")
DIMENSION laTarget[1]
SELECT CUSTOMER
SELECT DISTINCT ACCOUNT+" - "+ALLTRIM(bTName) FROM CUSTOMER ;
                WHERE type+account+store = "M" .AND. !(Status $ "H|X") ;
                INTO ARRAY laSource
= gfMover(@laSource,@laTarget,'Select Customer(s).',.T.,'')
FOR lnIndex = 1 TO ALEN(laTarget,1)
  IF !EMPTY(PADL(laTarget[lnIndex],5)) .AND. !SEEK(PADL(laTarget[lnIndex],5),lcTmpQuery)
    SELECT (lcTmpQuery)
    FOR lnWekInd = 1 TO laData[6]
      APPEND BLANK
      REPLACE Account WITH PADL(laTarget[lnIndex],5),;
              Week    WITH PADL(STR(lnWekInd,3),3)
    ENDFOR
  ELSE
    SELECT (lcTmpQuery)
    IF !EMPTY(PADL(laTarget[lnIndex],5)) .AND. llDel
      lnTempWeek = 1
      SCAN REST WHILE Account+Week = PADL(laTarget[lnIndex],5)
        BLANK
        REPLACE Account WITH PADL(laTarget[lnIndex],5),;
                Week    WITH PADL(STR(lnTempWeek,3),3),;
                llDel WITH .F.
        lnTempWeek = lnTempWeek + 1
      ENDSCAN
    ENDIF
  ENDIF
  laTarget[lnIndex] = ""
ENDFOR
SELECT (lcTmpQuery)
LOCATE FOR !lldel
IF FOUND()
  SHOW GET pbdelCst ENABLE
  SHOW GET laData[3] DISABLE
  SHOW GET laData[4] DISABLE
  DO CASE
    CASE laData[6] = 1
      SHOW GET pbNxtCst DISABLE
      SHOW GET pbPreCst DISABLE
    CASE  lnCurrWek = 1
      SHOW GET pbNxtCst ENABLE
      SHOW GET pbPreCst DISABLE
    CASE  laData[6] = lnCurrWek
      SHOW GET pbNxtCst DISABLE
      SHOW GET pbPreCst ENABLE
    OTHERWISE
      SHOW GET pbPreCst ENABLE
      SHOW GET pbNxtCst ENABLE
  ENDCASE
ENDIF
SHOW WINDOW (lcMtBrowTt) REFRESH
llCUpDate = .T.
SELECT(lnOldAls)


*!*************************************************************
*! Name      : lfvDSeek C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/15/01
*! Purpose   : Direct seek funciton.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvDSeek()
*!*************************************************************
*!
FUNCTION lfvDSeek
PRIVATE lnOldAls

IF !EMPTY(lcDirseek)
  lnOldAls = SELECT(0)
  SELECT(lcTmpQuery)
  =SEEK(ALLTRIM(lcDirseek))
  SET CONFIRM ON
  SHOW WINDOW (lcMtBrowTt) REFRESH
  _CUROBJ = OBJNUM(ibNTrap)
  lcDirseek = ""
  SELECT(lnOldAls)
ENDIF


*!*************************************************************
*! Name      : lfvDelCst C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/15/01
*! Purpose   : Delete customer function.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvDelCst()
*!*************************************************************
*!
FUNCTION lfvDelCst
PRIVATE lnOldAls

lnOldAls = SELECT(0)
SELECT (lcTmpQuery)
=lfwBrows()
IF cMark = '>'
  SHOW WINDOW (lcMtBrowTt) REFRESH
  lcCust2Del = Account
  lcWek2Del  = ALLTRIM(Week)
  LOCATE FOR !lldel
  IF FOUND()
  *--- B# 00002  
  *--- M# 00002
    IF gfModalGen('INM00000B00002',.F.,.F.,.F., "Are you sure you want to delete Customer: "+lcCust2Del) = 1
      =SEEK(lcCust2Del)
      LOCATE REST WHILE Account = lcCust2Del FOR ALLTRIM(Week) = ALLTRIM(lcWek2Del)
      FOR lnInde = 1 TO 7
       lcIndex = STR(lnInde,1)
       lnTotal&lcIndex = lnTotal&lcIndex - Day&lcIndex
      ENDFOR
      GO TOP
      =SEEK(lcCust2Del)
      SCAN REST WHILE Account+Week = lcCust2Del
        laData[5] = laData[5] - TOTQTY
        IF ALLTRIM(Week) = ALLTRIM(lcWek2Del)
          lnWeekTot = lnWeekTot - TOTQTY
        ENDIF    
        REPLACE llDel WITH .T.
        *BLANK
      ENDSCAN
      SHOW GET laData[5]
      =lfRefresh()
      SHOW WINDOW (lcMtBrowTt) REFRESH
      SELECT(lnOldAls)
     _CUROBJ = OBJNUM(ibNTrap)
    ENDIF
    SELECT (lcTmpQuery)
    GO TOP
    LOCATE FOR !EMPTY(Account) .AND. !llDel
    IF !FOUND()
      SHOW GET pbdelCst DISABLE
      SHOW GET pbPreCst DISABLE
      SHOW GET pbNxtCst DISABLE
    ELSE
      SHOW GET pbdelCst ENABLE
       DO CASE
          CASE laData[6] = 1
            SHOW GET pbNxtCst DISABLE
            SHOW GET pbPreCst DISABLE
          CASE  lnCurrWek = 1
            SHOW GET pbNxtCst ENABLE
            SHOW GET pbPreCst DISABLE
          CASE  laData[6] = lnCurrWek
            SHOW GET pbNxtCst DISABLE
            SHOW GET pbPreCst ENABLE
          OTHERWISE
            SHOW GET pbPreCst ENABLE
            SHOW GET pbNxtCst ENABLE
        ENDCASE
    ENDIF
  ENDIF
ENDIF
llCUpDate = .T.
SELECT(lnOldAls)


*!*************************************************************
*! Name      : lfRest C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/15/01
*! Purpose   : Reset variable and DBF .
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfRest()
*!*************************************************************
*!
FUNCTION lfRest
PRIVATE lnOldAls

lnOldAls = SELECT(0)
lnWeekTot = 0
lnCurrWek = 1
lcMtBrowTt = "Week#   "+STR(lnCurrWek,3)
=lfActsBrow()
FOR lnInde = 1 TO 7
 lcIndex = STR(lnInde,1)
 llDay&lcIndex = .T.
 lnTotal&lcIndex = 0
ENDFOR
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lpSavScr C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/15/01
*! Purpose   : Save function .
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lpSavScr()
*!*************************************************************
*!
FUNCTION lpSavScr
PRIVATE lnOldAls

IF laData[3] = {} .OR. laData[4] = {}
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Cannot save forecast with out date range, this process will be ignored.')
  RETURN
ENDIF
lnOldAls = SELECT(0)
SELECT ForCastH
IF !SEEK(laData[1])
  APPEND BLANK
ENDIF
=RLOCK()
GATHER FROM laData FIELDS &lcScFields
=gfAdd_Info()                     && Add the add user,time and date
=gfObj_Lock(.F.)
UNLOCK
SELECT forcastd
SET ORDER TO Forcode
=RLOCK()
SELECT (lcTmpQuery)
GO TOP
DO WHILE !EOF()
  lcFrcCode = laData[1]
  lcCurAct  = Account
  lnTotAct  = 0
  =SEEK(lcCurAct)
  SCAN REST WHILE Account+Week = lcCurAct FOR !DELETED()
    SCATTER MEMVAR MEMO
    SELECT forcastd
    IF !SEEK(laData[1]+m.Account)
      APPEND BLANK
      REPLACE cWeekNo  WITH PADL(ALLTRIM(m.Week),2),;
              Account  WITH m.Account
    ELSE
      LOCATE REST WHILE cFrcCode+Account = laData[1]+m.Account ;
                  FOR ALLTRIM(cWeekNo)=ALLTRIM(m.Week)
      IF !FOUND()
        APPEND BLANK
        REPLACE cWeekNo  WITH PADL(ALLTRIM(m.Week),2),;
                Account  WITH m.Account
      ENDIF
    ENDIF
    IF m.llDel
      DELETE
      SELECT frcscust
      IF SEEK(lcCurAct)
        BLANK
        DELETE
      ENDIF
    ELSE
      REPLACE cFrcCode WITH laData[1],;
              nFrcAmt1 WITH m.Day1,;
              nFrcAmt2 WITH m.Day2,;
              nFrcAmt3 WITH m.Day3,;          
              nFrcAmt4 WITH m.Day4,;
              nFrcAmt5 WITH m.Day5,;
              nFrcAmt6 WITH m.Day6,;
              nFrcAmt7 WITH m.Day7,;
              nFrcWTot WITH m.TotQty
      lnTotAct = lnTotAct + m.TotQty
    ENDIF
  ENDSCAN
  SELECT forcastd
  SET ORDER TO Foracct
  SELECT frcscust
  =RLOCK()
  IF !SEEK(lcCurAct) .AND. SEEK(lcCurAct,'forcastd')
    APPEND BLANK
  ENDIF
  IF SEEK(lcCurAct,'forcastd')
    REPLACE Account  WITH lcCurAct,;
            cFrcCode WITH laData[1],;
            nFrcaTot WITH lnTotAct
  ENDIF
  SELECT forcastd
  SET ORDER TO Forcode
  SELECT (lcTmpQuery)
ENDDO
SELECT forcastd
UNLOCK
SELECT frcscust
UNLOCK
laUsrFields = ''
=lfRest()
SELECT(lnOldAls)


*!*************************************************************
*! Name      : lfvNave C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/15/01
*! Purpose   : Navigator function .
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvNave()
*!*************************************************************
*!
FUNCTION lfvNave
PARAMETER lcnxtActn

lnWeekTot =  0
IF lcnxtActn = "N"
  SHOW GET pbPreCst ENABLE
  IF lnCurrWek < laData[6]
    lnCurrWek = lnCurrWek + 1
    ldfrsDay   = ldfrsDay + 7
    ldLstDay  = ldLstDay + 7
    FOR lnIndex = 1 TO 7
      lcQtyNo = STR(lnIndex,1)
      lnTotal&lcQtyNo = 0
    ENDFOR
    FOR lnIndex = 1 TO 7
      lcQtyNo = STR(lnIndex,1)
      SUM ALL Day&lcQtyNo TO lnTotal&lcQtyNo FOR ALLTRIM(WEEK) = ALLTRIM(STR(lnCurrWek,3)) .AND. !llDel
      lnWeekTot =  lnWeekTot +lnTotal&lcQtyNo
    ENDFOR
    IF lnCurrWek = laData[6]
      SHOW GET pbNxtCst DISABLE
      =lfClmStat("E")
    ENDIF
  ENDIF
ELSE
  SHOW GET pbNxtCst ENABLE
  IF lnCurrWek > 1
    lnCurrWek = lnCurrWek - 1
    ldfrsDay   = ldfrsDay - 7
    ldLstDay  = ldLstDay - 7
    IF lnCurrWek = 1
      SHOW GET pbPreCst DISABLE
      =lfClmStat("B")
    ENDIF
    FOR lnIndex = 1 TO 7
      lcQtyNo = STR(lnIndex,1)
      lnTotal&lcQtyNo = 0
    ENDFOR
    FOR lnIndex = 1 TO 7
      lcQtyNo = STR(lnIndex,1)
      SUM ALL Day&lcQtyNo TO lnTotal&lcQtyNo FOR ALLTRIM(WEEK) = ALLTRIM(STR(lnCurrWek,3))  .AND. !llDel
      lnWeekTot =  lnWeekTot +lnTotal&lcQtyNo
    ENDFOR
  ENDIF
ENDIF
=lfRefresh()
lcTmpTit = "From:  "+DTOC(ldfrsDay) + "  To:  "+DTOC(ldLstDay)
IF lnCurrWek = 1
  lcTmpTit = "From:  "+DTOC(laData[3]) + "  To:  "+DTOC(ldLstDay)
ENDIF
IF lnCurrWek = laData[6]
  lcTmpTit = "From:  "+DTOC(ldfrsDay) + "  To:  "+DTOC(laData[4])
ENDIF
lcMtBrowTt = "Week#   "+STR(lnCurrWek,3)+"     "+lcTmpTit
SELECT (lcTmpQuery)
=lfActsBrow()


*!*************************************************************
*! Name      : lfGetData C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/15/01
*! Purpose   : Get data function .
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGetData()
*!*************************************************************
*!
FUNCTION lfGetData
PARAMETER lcCode

SELECT (lcTmpQuery)
DELETE ALL
=lfRest()
SELECT forcasth
=SEEK(laData[1])
SCATTER FIELDS &lcScFields MEMO TO laData
SELECT forcastd
SET ORDER TO Forcode
lnWeekTot = 0
=SEEK(laData[1])
FOR lnIndex = 1 TO 7
  lcQtyNo = STR(lnIndex,1)
  =SEEK(laData[1])
  SUM REST nFrcAmt&lcQtyNo TO lnTotal&lcQtyNo ;
      WHILE cfrccode+account = laData[1] ;
      FOR ALLTRIM(cWeekNo) = '1'
  lnWeekTot =  lnWeekTot +lnTotal&lcQtyNo
ENDFOR
=SEEK(laData[1])
SCAN REST WHILE cfrccode+account = laData[1]
  SCATTER MEMVAR MEMO
  SELECT (lcTmpQuery)
  IF !SEEK(m.Account+ALLTRIM(m.cWeekNo))
    APPEND BLANK
  ENDIF
  REPLACE Account  WITH m.Account,;
          Week     WITH m.cWeekNo,;
          Day1     WITH m.nFrcAmt1,;
          Day2     WITH m.nFrcAmt2,;
          Day3     WITH m.nFrcAmt3,;
          Day4     WITH m.nFrcAmt4,;
          Day5     WITH m.nFrcAmt5,;
          Day6     WITH m.nFrcAmt6,;
          Day7     WITH m.nFrcAmt7,;
          TotQty   WITH m.nFrcWTot
ENDSCAN
=lfVData4()


*!*************************************************************
*! Name      : lpDelScr C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/15/01
*! Purpose   : Delete forecusting.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lpDelScr()
*!*************************************************************
*!
FUNCTION lpDelScr
PRIVATE  lnOldAls

lnOldAls = SELECT(0)
SELECT ForCastH
SET ORDER TO ForCastH
IF SEEK(laData[1])
  BLANK
  DELETE
ENDIF
SELECT forcastd
SET ORDER TO Forcode
IF SEEK(laData[1])
  DELETE REST WHILE cfrccode+account = laData[1]
  FLUSH
ENDIF
SELECT frcscust
SET ORDER TO Forcode
IF SEEK(laData[1])
  DELETE REST WHILE cfrccode+account = laData[1]
ENDIF
laScrMode = .F.
laScrMode[1] = .T.
SHOW GETS
SELECT(lnOldAls)


*!*************************************************************
*! Name      : lfColmSta C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/15/01
*! Purpose   : Browse column status (Enable/Disable).
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfColmSta()
*!*************************************************************
*!
FUNCTION lfColmSta
PRIVATE lnOldAls, lnFWkDay , lnLWkDay , lnTotDay ,lnDIndex , lnTotWeek

lnOldAls = SELECT(0)
lnTotWeek = 0
lnTotDay = (laData[4] - laData[3]) +1

lnTotDay = IIF(lnTotDay=0,1,lnTotDay)
DIMENSION laTotDay[lnTotDay]
laTotDay[1] = laData[3]
FOR lnDIndex = 2 TO lnTotDay
  laTotDay[lnDIndex] = laData[3]+(lnDIndex-1)
ENDFOR
IF DOW(laData[3]) <> 2  && Not Monday
  FOR lnInd = 1 TO 7
     ldTmpStar = laData[3] - lnInd
     IF DOW(ldTmpStar) = 2
       EXIT
     ENDIF
  ENDFOR
  lnTotWeek = lnTotWeek + 1
ELSE
  ldTmpStar = laData[3]
ENDIF

=lfGetWeks()
laData[6] = lnTotWeek


*!*************************************************************
*! Name      : lfGetWeks C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/15/01
*! Purpose   : Get weeks.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGetWeks()
*!*************************************************************
*!
FUNCTION lfGetWeks
PRIVATE llFirst

*--- Loop On the array to get the first Monday
llFirst = .T.
lcFrstWD = ""
lcLastWD = ""
lnTempIn = 0
*IF ALEN(laTotDay) < 8 .AND. DOW(laTotDay[1]) <> 2
*  FOR ln2Inde = 1 TO ALEN(laTotDay)
*    lc2Ndx = STR(DOW(laTotDay[ln2Inde]),1)
*    lcFrstWD = lcFrstWD + "|" +lcDDay&lc2Ndx
*  ENDFOR
*ENDIF

FOR lnWIndex = 1 TO ALEN(laTotDay)
  IF DOW(laTotDay[lnWIndex]) = 2 .AND. lnWIndex <> ALEN(laTotDay)
    lnTotWeek = lnTotWeek + 1
    IF lnWIndex = 1 .AND. DOW(laTotDay[lnWIndex]) = 2
      FOR ln2Inde = 1 TO ALEN(laTotDay)
        lc2Ndx = STR(DOW(laTotDay[ln2Inde]),1)
        lcFrstWD = lcFrstWD + "|" +lcDDay&lc2Ndx
      ENDFOR
    ENDIF
    llFirst = .F.
  ELSE
    IF llFirst .AND.  lnWIndex <> ALEN(laTotDay)
      lcTempChr = STR(DOW(laTotDay[lnWIndex]),1)
      lcFrstWD = lcFrstWD + "|" +lcDDay&lcTempChr
      lnTempIn = lnTempIn + 1
    ENDIF
  ENDIF
  IF lnWIndex = ALEN(laTotDay) .AND. DOW(laTotDay[lnWIndex]) = 2
    lnTotWeek = lnTotWeek + 1
  ENDIF
ENDFOR
*lcTmpTit = "From:  "+DTOC(ldTmpStar) + "  To:  "+DTOC((ldTmpStar+7)-1)
IF lnTotWeek = 1
 lcTmpTit = "From:  "+DTOC(laData[3]) + "  To:  "+DTOC(laData[4])
ELSE
 lcTmpTit = "From:  "+DTOC(laData[3]) + "  To:  "+DTOC((ldTmpStar+7)-1)
ENDIF
ldfrsDay = ldTmpStar
ldLstDay = (ldTmpStar+7)-1
lcMtBrowTt = "Week#   "+STR(lnCurrWek,3)+"     "+lcTmpTit

FOR lnWIndex = ALEN(laTotDay) TO 1 STEP -1
  lcTempChr = STR(DOW(laTotDay[lnWIndex]),1)
  lcLastWD = lcLastWD + "|" +lcDDay&lcTempChr
  IF DOW(laTotDay[lnWIndex]) = 2
    EXIT
  ENDIF
ENDFOR
IF lnTotWeek = 1
  lcFrstWD = lcLastWD
ENDIF


*!*************************************************************
*! Name      : lfClmStat C#200165
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 02/15/01
*! Purpose   : Column status.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfClmStat()
*!*************************************************************
*!
FUNCTION lfClmStat
PARAMETER lcFrstLst

FOR lnInde = 1 TO 7
  lcIndex = STR(lnInde,1)
  llDay&lcIndex = .T.
ENDFOR

FOR lnInde = 1 TO 7
  lcIndex = STR(lnInde,1)
  llDay&lcIndex = IIF( lcFrstLst = "B",(lcWDay&lcIndex$lcFrstWD),(lcWDay&lcIndex$lcLastWD))
ENDFOR