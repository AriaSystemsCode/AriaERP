***************************************************************
*: Program file  : ICADSCH.PRG
*: Program desc. : Adornment SCHEDULE
*: Date          : 07/25/99
*: Module        : Inventory Control
*:         System: Aria Apparel System
*:      Developer: Ashraf Sherif Mohammad (ASH)
*:*************************************************************
*: Calls 		 : ICADSCH.SPR
*:         Functions  : 
*:*************************************************************

EXTERNAL ARRAY laData 
DECLARE laCompAdd[6,1]
STORE SPACE(0) TO laCompAdd
laScrMode[1] = .T.
laDefProc[7]  = .F.              && Disable the control panel delete proc.(lpDelScr)
laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)
lnBrRecNo     = 0
IF !gfSetup()
  RETURN
ENDIF
lcshowmode = 'DISABLE'
lcDesCode = SPACE(6)
lcDesDesc = SPACE(30)
lcBrowcTtl = 'Adornment Schedule'

IF !WEXIST(gcBaseWind)
  lcTempFile = gfTempname()
  CREATE CURSOR &lcTempFile (ScDate D,DsnCode C(6),DsnDesc C(30),Type C(1))
  INDEX ON DTOS(ScDate)+DsnCode TAG &lcTempFile
  lcScFields = 'Date'
  SELECT ICDESIGN
  SET ORDER TO ICDESIGN
  SELECT ICADSCH
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK               
ENDIF
SELECT ICADSCH
SET ORDER TO AdorDate
PUSH KEY
DO (gcScrDir+gcWinAppl+"\MFADSCH.SPX")
ON KEY
POP KEY


*!*************************************************************
*! Name      : lpShow
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 07/25/99
*! Purpose   : Assigns enable/disable vals. depend on laScrMode
*!*************************************************************
*! Calls     : lfBrowse,lpCollData
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lpShow()
*!*************************************************************

FUNCTION lpShow
lcShowMode=IIF(laScrMode[2],'DISABLE','ENABLE')
DO CASE
  ***--- S E L E C T   M O D E ---***
  CASE laScrMode[1]  && Select mode. 
    SELECT ICADSCH
    SET ORDER TO ADORDATE
    SCATTER FIELDS &lcScFields MEMO TO laData BLANK
    SHOW GET PbNew     DISABLE
    SHOW GET PbRemv    DISABLE
    SHOW GET lcDesCode DISABLE
    lcDesCode = SPACE(6)
    lcDesDesc = SPACE(30)
    SELECT &lcTempFile
    ZAP
    =lfBrowse()
    
  ***--- V I E W   M O D E ---***
  CASE laScrMode[2]  && View mode.
    SELECT &lcTempFile
    ZAP
    DO lpCollData
    *=lfBrowse()
    SHOW GET PbNew     DISABLE
    SHOW GET PbRemv    DISABLE
    SHOW GET lcDesCode DISABLE

  ***--- E D I T   M O D E ---***
  CASE laScrMode[3]  && Edit mode. 
    SHOW GET lcDesCode DISABLE
    SHOW GET PbNew     ENABLE
    SHOW GET PbRemv    ENABLE

  ***--- A D D   M O D E ---***
  CASE laScrMode[4]  && Add mode. 
    SHOW GET lcDesCode ENABLE
    _CUROBJ = OBJNUM(lcDesCode)    
ENDCASE

*!*************************************************************
*! Name      : lfVDate
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 07/25/99
*! Purpose   : Validation function of date.
*!*************************************************************
*! Calls     : gfModalGen,lpCollData
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfVDate()
*!*************************************************************

FUNCTION lfVDate
SELECT ICADSCH
IF EMPTY(laData[1])
  RETURN
ENDIF
IF !EMPTY(laData[1]) AND !SEEK(DTOS(laData[1]))
  lnChoice = gfModalGen("QRM42186B42003","DIALOG",'') 
  DO CASE
    CASE lnChoice = 1
      laScrMode = .F.
      laScrMode[4] = .T.
      SHOW GETS
      SHOW GET PbNew DISABLE
  
    CASE lnChoice = 2
      DO lpBrowD
      IF EMPTY(laData[1])
        SET ORDER TO ADORDATE IN ICADSCH
        RETURN
      ENDIF
      SELECT &lcTempFile
      ZAP
      DO lpCollData
      laScrMode = .F.
      laScrMode[2] = .T.
      SHOW GETS
      SHOW GET lcDesCode DISABLE
    CASE lnChoice = 3
      _CUROBJ = OBJNUM(laData[1])
  ENDCASE
ELSE
  SELECT &lcTempFile
  ZAP
  DO lpCollData
  laScrMode = .F.
  laScrMode[2] = .T.
  SHOW GETS
  SHOW GET lcDesCode DISABLE
  
ENDIF
SELECT ICADSCH
SET ORDER TO ADORDATE IN ICADSCH

*!*************************************************************
*! Name      : lpCollData
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 07/25/99
*! Purpose   : Collect designs for the entered date.
*!*************************************************************
*! Calls     : lfBrowse
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfVDate()
*!*************************************************************

PROCEDURE lpCollData
PRIVATE lnCurAlias, lnCurTag

lnCurAlias = SELECT(0)
SELECT ICADSCH
lnCurTag = VAL(SYS(21))
SET ORDER TO TAG DateDesn
SEEK(DTOS(laData[1]))
SCAN WHILE Date = laData[1]
  SELECT &lcTempFile
  IF !SEEK(DTOS(laData[1])+ICADSCH.cDsgnCode)
    APPEND BLANK
    REPLACE ScDate   WITH ICADSCH.Date ,;
            DsnCode  WITH ICADSCH.cDsgnCode 
    IF SEEK(ICADSCH.cDsgnCode,'ICDESIGN')            
      REPLACE DsnDesc WITH ICDESIGN.cDsgnName
    ENDIF
  ENDIF
ENDSCAN
LOCATE FOR DATE = laData[1]  


  =lfBrowse()


SET ORDER TO (lnCurTag) IN ICADSCH
SELECT (lnCurAlias)
  
*!*************************************************************
*! Name      : lfBrowse
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 07/25/99
*! Purpose   : Showing the browse
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : lfwBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfBrowse()
*!*************************************************************

FUNCTION lfBrowse

SELECT (lcTempFile)
SET ORDER TO TAG (lcTempFile)    
GO TOP
lcDesCode = DsnCode
lcDesDesc = DsnDesc
lnBrRecNo = RECNO()
lcBrD1fld = [cMarker=IIF(lnBrRecNo=RECNO(),'',' '):1:H=' ':W=.F.,;
             DsnCode:6:H='Design',DsnDesc:30:H='Design Name']


*-- Showing the browes
BROWSE FIELDS &lcBrD1fld ;
       FOR    Type <> 'D';
       LOCK 0   ;
       NOAPPEND ;
       NOCLEAR  ;
  	   NODELETE ;
       NOMENU   ;
       NOEDIT   ;
  	   NOWAIT   ;
	   SAVE     ;
  	   TITLE lcBrowcTtl ;
       WINDOW ICADSCH1 IN WINDOW (GCBASEWIND);
       WHEN lfwBrow()

_CUROBJ = OBJNUM(pbNEW)
RETURN

*!*************************************************************
*! Name      : lfwBrow
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 07/25/99
*! Purpose   : Controlls moving in the browse getting information
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwBrow()
*!*************************************************************

FUNCTION lfwBrow
PRIVATE lnAlias

lnAlias   = SELECT()
lnBrRecNo = RECNO()
SELECT (lcTempFile)
lcDesCode = DsnCode
lcDesDesc = DsnDesc
lnTRecNo = RECNO()
LOCATE FOR TYPE <> 'D'
IF EOF()
  SHOW GET PbRemv DISABLE
ENDIF
IF BETWEEN(lnTRecNo,0,RECCOUNT())
  GOTO lnTRecNo
ENDIF

SHOW GET lcDesCode
=lfRefresh()
SHOW WINDOW (lcBrowcTtl) REFRESH SAME
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfReadAct
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 07/25/99
*! Purpose   : Activate read screen function.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfReadAct()
*!*************************************************************

FUNCTION lfReadAct

ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ESC

*!*************************************************************
*! Name      : lfTrapKy
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 07/25/99
*! Purpose   : Trap key for the browse.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfTrapKy()
*!*************************************************************

FUNCTION lfTrapKy
IF WONTOP() = lcBrowcTtl
  ON KEY LABEL TAB        DO lptab1
  ON KEY LABEL backtab    DO lpsfttp1
  ON KEY LABEL ESC        DO gfEscap
ENDIF  
RETURN .F.

*!*************************************************************
*! Name      : lptab1
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 07/25/99
*! Purpose   : Trap procedure for tab.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lptab1()
*!*************************************************************

PROCEDURE lptab1

IF WONTOP(lcBrowcTtl)
  SHOW GET IBBROW1 DISABLE
  ACTI WINDOW ICADSCH3
  _CUROBJ = OBJNUM(lcDesCode)
ENDIF  
RETURN 

*!*************************************************************
*! Name      : lpsfttp1
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 07/25/99
*! Purpose   : Trap key for shift tab.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lpsfttp1()
*!*************************************************************

PROCEDURE lpsfttp1

IF WONTOP(lcBrowcTtl)
  ACTI WINDOW ICADSCH3
  _CUROBJ = OBJNUM(PBNEW)
ENDIF
RETURN

*!*************************************************************
*! Name      : lpBrowD
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 07/25/99
*! Purpose   : Browse dates.
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : AriaBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lpBrowD()
*!*************************************************************

PROCEDURE lpBrowD
PRIVATE lcBrFields 
SELECT ICADSCH

lcBrowTitl = "Schedule Date Browse"
lcBrFields = [Date:8:H = "Schedule Date",cDsgnCode:6:H = "Design Code"]
IF AriaBrow("",lcBrowTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2,gnBrFSCol2,'','','Date','laData')
  laData[1] = ICADSCH.Date  
ELSE
  laData[1]= {}
  _CUROBJ = _CUROBJ
ENDIF
SHOW GET laData[1]
 
*!*************************************************************
*! Name      : lfVDesign
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 07/25/99
*! Purpose   : Design valid function.
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : gfModalGen,lfwBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfVDesign()
*!*************************************************************

FUNCTION lfVDesign
PRIVATE lcBrFields 
IF EMPTY(lcDesCode)
  RETURN
ENDIF
SET ORDER TO DATEDESN IN ICADSCH
SELECT (lcTempFile)
lnAdRec = RECNO()

IF SEEK(DTOS(laData[1])+lcDesCode) 
  LOCATE REST WHILE DTOS(ScDate)+DsnCode = DTOS(laData[1])+lcDesCode FOR Type <> 'D'
  IF FOUND()
    =gfModalGen("QRM42187B42000","DIALOG",'') 
    RETURN
  ENDIF
ENDIF

IF BETWEEN(lnAdRec,0,RECCOUNT())
  GOTO lnAdRec 
ENDIF
IF SEEK(lcDesCode,'ICDESIGN')
  lcDesDesc = ICDESIGN.cDsgnName
  SELECT (lcTempFile)
  APPEND BLANK
  REPLACE ScDate   WITH laData[1] ,;
          DsnCode  WITH lcDesCode ,;
          DsnDesc  WITH lcDesDesc ,;
          Type     WITH 'A'
ELSE
  DECLARE laTmpDat[1]
  laTmpDat[1] = ' '
  SELECT IcDesign
  lcBrowTitl = "Design Codes"
  lcBrFields = [cDsgnCode:6:H = "Design Code",cDsgnName:30:H = "Design Name"]
  IF AriaBrow("",lcBrowTitle,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2,gnBrFSCol2,'','','cDsgnCode','laTmpData')
    lcDesCode = ICDESIGN.cDsgnCode
    lcDesDesc = ICDESIGN.cDsgnName
    lnAdRec = RECNO()
    SELECT (lcTempFile)
    IF SEEK(DTOS(laData[1])+lcDesCode) 
      LOCATE REST WHILE DTOS(ScDate)+DsnCode = DTOS(laData[1])+lcDesCode FOR Type <> 'D'
      IF FOUND()
        =gfModalGen("QRM42187B42000","DIALOG",'') 
        RETURN
      ENDIF
    ENDIF
    IF BETWEEN(lnAdRec,0,RECCOUNT())
      GOTO lnAdRec 
    ENDIF
    APPEND BLANK
    REPLACE ScDate   WITH laData[1] ,;
            DsnCode  WITH lcDesCode ,;
            DsnDesc  WITH lcDesDesc ,;
            Type     WITH 'A'
  ELSE
    lcDesCode = SPACE(6)
    _CUROBJ = _CUROBJ
  ENDIF
ENDIF
SHOW GET lcDesCode   
=lfwBrow()
SHOW GET PbNew ENABLE
SET ORDER TO AdorDate IN ICADSCH

*!*************************************************************
*: Name       : lfvNew
*: Developer  : Ashraf Sherif Mohammad (ASH)
*: Date       : 07/25/1999
*: Purpose    : Valid function for the new button
*!*************************************************************
*: Calls      : lfRefresh.
*!*************************************************************
*: Parameters : None.
*!*************************************************************
*: Returns    : None
*!*************************************************************
*: Example    : =lfvNew()
*!*************************************************************

FUNCTION lfvNew
llCUpDate = .T.
lcDesCode = SPACE(6)
lcDesDesc = SPACE(30)
SHOW GET lcDesCode ENABLE
=lfRefresh()
_CUROBJ = OBJNUM(lcDesCode)

*!*************************************************************
*: Name       : lfVRemv
*: Developer  : Ashraf Sherif Mohammad (ASH)
*: Date       : 07/25/1999
*: Purpose    : Valid function for the remove button.
*!*************************************************************
*: Calls      : lfwBrow,gfModalGen
*!*************************************************************
*: Parameters : None.
*!*************************************************************
*: Returns    : None
*!*************************************************************
*: Example    : =lfVRemv()
*!*************************************************************

FUNCTION lfVRemv
SELECT (lcTempFile)
IF !EOF() AND !BOF() AND gfModalGen("QRM42029B42002","DIALOG",'') = 1
  llCUpDate = .T.
  REPLACE Type WITH 'D'
  LOCATE FOR TYPE <> 'D'
  *IF !BOF()
  *  SKIP -1
  *  IF BOF() AND !EOF() 
  *    SKIP
  *  ENDIF
  *ELSE
  *  IF !EOF() 
  *    SKIP
  *    IF EOF() AND !BOF()
  *      SKIP -1
  *    ENDIF
  *  ELSE
  *    LOCATE FOR TYPE <> 'D'
  *  ENDIF
  *ENDIF
  =lfwBrow()
ENDIF

*!*************************************************************
*: Name       : lpSavScr
*: Developer  : Ashraf Sherif Mohammad (ASH)
*: Date       : 07/25/1999
*: Purpose    : Local save function.
*!*************************************************************
*: Calls      : None.
*:       Called by : gfCPSave
*!*************************************************************
*: Parameters : None.
*!*************************************************************
*: Returns    : None
*!*************************************************************
*: Example    : DO lpSavScr
*!*************************************************************

PROCEDURE lpSavScr
SELECT (lcTempFile)
LOCATE FOR TYPE <> 'D'
IF !FOUND()
  llCSave = .F.
  =gfModalGen('QRM42013B00000','DIALOG','')
  RETURN
ENDIF
SET ORDER TO DATEDESN IN ICADSCH
SELECT (lcTempFile)

SCAN FOR Type = 'D'
  SELECT ICADSCH
  IF SEEK(DTOS(&lcTempFile..ScDate)+&lcTempFile..DsnCode)
    DELETE
  ENDIF
ENDSCAN
SCAN FOR Type = 'A'
  SELECT ICADSCH
  IF !SEEK(DTOS(&lcTempFile..ScDate)+&lcTempFile..DsnCode)
    APPEND BLANK
    REPLACE Date      WITH &lcTempFile..ScDate ,;
            cDsgnCode WITH &lcTempFile..DsnCode
  ENDIF  
  =gfAdd_Info('ICADSCH')         
ENDSCAN
SELECT ICADSCH
SET ORDER TO ADORDATE IN ICADSCH
SHOW GETS

*!*************************************************************
*: Name       : lpDelScr
*: Developer  : Ashraf Sherif Mohammad (ASH)
*: Date       : 07/25/1999
*: Purpose    : Local delete function.
*!*************************************************************
*: Calls      : None.
*!*************************************************************
*: Parameters : None.
*!*************************************************************
*: Returns    : None
*!*************************************************************
*: Example    : DO lpDelScr
*!*************************************************************

PROCEDURE lpDelScr
SELECT ICADSCH
SET ORDER TO DATEDESN
IF SEEK(DTOS(laData[1]))
  DELETE REST WHILE DATE = laData[1]
ENDIF
laScrMode    = .F.
laScrMode[1] = .T.
SET ORDER TO ADORDATE IN ICADSCH

