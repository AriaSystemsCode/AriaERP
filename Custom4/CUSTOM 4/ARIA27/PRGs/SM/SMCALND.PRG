*:************************************************************************
*: Program file  : SMCALND.PRG
*: Program desc. : Project management/Calendars  (N000341)
*: For screen    : SMCALNDR
*:         System: ARIA APPAREL SYSTEM
*:      Developer: HEND GHANEM (HBG)
*:************************************************************************
*: Calls         : SMCALNDR.SPX
*:                 SMCPYCAL.SPX      
*:*************************************************************************
*: Passed Parameters  : None
*:*************************************************************************
*: Example            : DO SMCALNDR
*:************************************************************************
*:E301869,2 AMH 12/30/2002 Create a production schedule project according to a setting.
*:************************************************************************
PRIVATE lcBrTtl, lnBrRecNo, lnNOfLines
PRIVATE ALL LIKE lcT*

STORE '' TO lgOldVal
lcBrTtl   = 'Holidays'
lcCalFlds = [cCal_ID : H = 'Calendar ID', cCal_Des : H = 'Description']
lcCalTtl  = 'Calendars'

lc_PMCalDt = ''
lcWinTitl  = ''
lnNameWdth = '35'  
STORE ' ' TO lcWinCh0,lcWinCh1,lcWinCh2

DECLARE laWeekEnds[7]
laWeekEnds = .F.

*--ToolBar external proc.
laDefProc[7]  = .F.              && Disable the control panel delete proc.(lpDelScr)
laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)

lcEnbCntrl = SCHEME(1,9)
lcDisCntrl = SCHEME(1,10)
lcSelCntrl = SCHEME(1,6)

STORE 0 TO lnNOfLines, lnBrRecNo, lnOldCalRec, lnCurCalRec
ldCurDate    = gdSysDate
STORE .F. TO llBrowse, glUpdated, llMouseClic


IF gcDatadir = "\"
  =gfOpenFile(gcSysHome+'SYUUSER','cUser_Id')
  =gfSysOpen(gcSysHome+'SYCINST')
  
  IF SEEK(gcUser_ID,'SYUUSER') AND !EMPTY(SYUUSER.cUsr_dcom)
    gcDataDir = ALLTRIM(SYCINST.cinsallcmp)+ALLTRIM(SYUUSER.cUsr_dcom)+"\"
  ELSE
    =gfModalgen("TRM00388B00000","DIALOG")
    RETURN
  ENDIF  
ENDIF

IF !gfSetup()
  RETURN
ENDIF  

IF !WEXIST(gcBaseWind)
  lc_PMCalDt = gfTempName()
  lcWinCh0   = gfTempName()
  lcWinCh1   = gfTempName()
  lcWinCh2   = gfTempName()
  
  lcScFields = 'cCal_id,cCal_des,cCal_wend'
  
  *-- Create a cursor with the same structure as PMCALDT 
  *-- adding two more fields for saving.
  SELECT PMCALDT
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru, 1)
  DIMENSION laFileStru(lnFileStru + 2, 4)
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'cStatus'
  laFileStru[lnFileStru ,2] = 'C'
  laFileStru[lnFileStru ,3] = 1
  laFileStru[lnFileStru ,4] = 0
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'nRecNo'
  laFileStru[lnFileStru ,2] = 'N'
  laFileStru[lnFileStru ,3] = 10
  laFileStru[lnFileStru ,4] = 0
  
  CREATE CURSOR (lc_PMCalDt) FROM ARRAY laFileStru
  INDEX ON cCal_ID TAG PMCALDT
  SCATTER MEMVAR MEMO BLANK
  
  SELECT PMCALHD
  llCpyCalnd = SEEK('')
  SET RELATION TO cCal_ID INTO PMCALDT

  GO TOP
  SCATTER MEMVAR MEMO BLANK
  lcCal_ID  = m.cCal_ID
  
ENDIF

lcWinTitl = 'Calender'
PUSH KEY
ON KEY LABEL ALT+H ACTIVATE WINDOW (lcBrTtl)

*E301869,2 AMH Call screen from the correct directory [Start]
*DO (gcScrDir +"SM\"+ '\SMCALND.SPR')
DO (gcScrDir +gcWinAppl + '\SMCALND.SPR')
*E301869,2 AMH [End]

POP KEY

SELECT PMCALHD
SET RELATION TO
IF USED(lc_PMCalDt)
  USE IN (lc_PMCalDt)
ENDIF

*!*************************************************************
*! Name      : lfActBrows
*! Developer : Hend Ghanem
*! Date      : 11/22/2001
*! Purpose   : Browse activation function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfActBrows()
*!*************************************************************
FUNCTION lfActBrows
PARAMETERS lcDetFile

lnCurAlias = select(0)
SELECT (lcDetFile)
lnBrRecNo = RECNO()
BROWSE FIELDS cMarker   = IIF(RECNO() = lnBrRecNo, '', ' ');
                        : H = ' '   :R:1:W = .F.,;
              nLineNo   : H = 'No.'  :R:3:W =.F.,;
              cCal_HNam : H = 'Holiday Name':R:&lnNameWdth,;                                                                          
              dCal_HFrm : H = 'From' :R,; 
              dCal_HTo  : H = 'To' :R;               
         WINDOW (lcWinCh1) IN WINDOW (gcBaseWind);
         FOR cCal_ID = lcCal_ID;
         WHEN lnBrRecNo <> RECNO() .AND. (laScrMode[1] .OR. laScrMode[2] .OR. lfVldEntry()) ;
              .AND. lfwBrwWhen(ALIAS(), (laScrMode[3] .OR. laScrMode[4]) .AND. !EOF());
         NOMENU;
         NOAPPEND;
         NODELETE;
         NOEDIT;
         NOWAIT;
         SAVE;
         NOCLEAR;
         TITLE lcBrTtl 
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfwBrwWhen
*! Developer : Hend Ghanem
*! Date      : 11/22/2001
*! Purpose   : WHEN function of the browse command
*!*************************************************************
*! Calls              : lfRefresh()
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  BROWSE ..  WHEN lfwBrwWhen()
*!*************************************************************
FUNCTION lfwBrwWhen
PARAMETERS lcTmpCalDt, llEnableAll
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)
SELECT (lcTmpCalDt)
SCATTER MEMVAR MEMO

IF llEnableAll
  SHOW GET m.cCal_HNam ENABLE 
  IF !EMPTY(m.cCal_HNam)
    SHOW GET m.dcal_hfrm ENABLE 
    SHOW GET m.dcal_hto  ENABLE 
    SHOW GET pbRemove    ENABLE
  ELSE
    SHOW GET m.dcal_hfrm DISABLE 
    SHOW GET m.dcal_hto  DISABLE
    SHOW GET pbRemove    DISABLE
  ENDIF
  SHOW GET pbNew       ENABLE
ELSE
  SHOW GET m.cCal_HNam DISABLE
  SHOW GET m.dcal_hfrm DISABLE 
  SHOW GET m.dcal_hto  DISABLE
  SHOW GET pbRemove    DISABLE
  IF laScrMode[3] OR laScrMode[4]
    SHOW GET pbNew       ENABLE 
  ENDIF  
ENDIF
=lfRefresh()
lnBrRecNo = RECNO()
SHOW WINDOW (lcBrTtl) REFRESH
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lpShow
*! Developer : Hend Ghanem
*! Date      : 11/22/2001
*! Purpose   : Adjusts display of objects according to the mode.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpShow()
*!*************************************************************
FUNCTION lpShow

DO CASE
  CASE laScrMode[1] && Select Mode
    SELECT PMCALHD
    SCATTER MEMVAR MEMO BLANK
    laWeekEnds   = .F.
    lcCal_ID     = m.cCal_ID
    lnNOfLines = 0
    SELECT (lc_PMCalDt)
    ZAP
    RELEASE WINDOW(lcBrTtl)
    =lfActBrows(lc_PMCalDt)
    SCATTER MEMVAR MEMO 
        
    SHOW GET ibTabs       ENABLE
    SHOW GET ibCal_ID     ENABLE
    SHOW GET lcCal_ID     ENABLE
    _CUROBJ = OBJNUM(lcCal_ID)
    =lfRefresh()       
  CASE laScrMode[2] 
    
    SELECT PMCALHD
    IF BETWEEN(RECNO(), 1, RECCOUNT())
      GO RECNO()
    ENDIF
    SCATTER MEMVAR MEMO   
    lcCal_ID   = m.cCal_ID
    SELECT PMCALDT
    lnNOfLines = IIF(EOF(), 0, 1)
    SCATTER MEMVAR MEMO
    
    laWeekEnds = .F.
    FOR lnCount = 1 TO LEN(ALLTRIM(m.cCal_WEnd))
      laWeekEnds[VAL(SUBSTR(m.cCal_WEnd, lnCount, 1))] = .T.
    ENDFOR  
    SHOW GET lcCal_ID    DISABLE
    SHOW GET m.cCal_Des  DISABLE
    SHOW GET pbCopyCal   DISABLE
    SHOW GET ibCal_ID    DISABLE
    SHOW GET m.cCal_HNam DISABLE
    SHOW GET m.dCal_HFrm DISABLE
    SHOW GET m.dCal_HTo  DISABLE
    SHOW GET pbNew       DISABLE
    SHOW GET pbRemove    DISABLE
    RELEASE WINDOW (lcBrTtl)
    =lfActBrows('PMCALDT') 
    =lfRefresh()
  CASE laScrMode[3] 
    
    llCUpDate = .T.      
    SELECT (lc_PMCalDt)
    ZAP

    *-- Collect path details into the cursor
    SELECT PMCALHD
    IF BETWEEN(RECNO(), 1, RECCOUNT())
      GO RECNO()
    ENDIF
    
    SELECT PMCALDT
    SCAN REST WHILE cCal_ID = lcCal_ID
      SCATTER MEMVAR MEMO
      m.nRecNo  = RECNO()
      m.cStatus = 'S'
      INSERT INTO (lc_PMCalDt) FROM MEMVAR
    ENDSCAN
    lnNOfLines = RECCOUNT(lc_PMCalDt)
    SELECT (lc_PMCalDt)
    GO TOP
    SCATTER MEMVAR MEMO
    SELECT PMCALHD
    SCATTER MEMVAR MEMO
    lcCal_ID   = m.cCal_ID
    
    SHOW GET ibCal_ID    DISABLE
    SHOW GET lcCal_ID    DISABLE
    IF llCpyCalnd
      SHOW GET pbCopyCal ENABLE
    ELSE
      SHOW GET pbCopyCal DISABLE      
    ENDIF  
    SHOW GET m.ccal_des    ENABLE
    SHOW GET laWeekEnds[1] ENABLE
    SHOW GET laWeekEnds[2] ENABLE
    SHOW GET laWeekEnds[3] ENABLE
    SHOW GET laWeekEnds[4] ENABLE
    SHOW GET laWeekEnds[5] ENABLE
    SHOW GET laWeekEnds[6] ENABLE
    SHOW GET laWeekEnds[7] ENABLE

    SHOW GET pbNew       ENABLE
    IF lnNOfLines > 0
      SHOW GET pbRemove    ENABLE
      SHOW GET m.cCal_HNam ENABLE
      SHOW GET m.dCal_HFrm ENABLE
      SHOW GET m.dCal_HTo  ENABLE
    ELSE
      SHOW GET pbRemove    DISABLE
      SHOW GET m.cCal_HNam DISABLE
      SHOW GET m.dCal_HFrm DISABLE
      SHOW GET m.dCal_HTo  DISABLE
    ENDIF
    RELEASE WINDOW (lcBrTtl)
    =lfActBrows(lc_PMCalDt) 
    =lfRefresh()
    
  CASE laScrMode[4] 
    llCUpDate = .T.          
    glUpdated    = .F.
    lnNOfLines = 0

    SHOW GET ibCal_ID   DISABLE
    SHOW GET lcCal_ID  DISABLE

    IF llCpyCalnd
      SHOW GET pbCopyCal ENABLE
    ELSE
      SHOW GET pbCopyCal DISABLE      
    ENDIF  
    SHOW GET m.ccal_des ENABLE
    SHOW GET laWeekEnds[1] ENABLE
    SHOW GET laWeekEnds[2] ENABLE
    SHOW GET laWeekEnds[3] ENABLE
    SHOW GET laWeekEnds[4] ENABLE
    SHOW GET laWeekEnds[5] ENABLE
    SHOW GET laWeekEnds[6] ENABLE
    SHOW GET laWeekEnds[7] ENABLE
    SHOW GET cCal_HNam  DISABLE
    SHOW GET dCal_Hfrm  DISABLE
    SHOW GET dCal_HTo   DISABLE        
    SHOW GET pbRemove   DISABLE
    SHOW GET pbNew      ENABLE
    SHOW GET pbBrowse   DISABLE
ENDCASE
=lfRefresh()
SELECT PMCALHD
  
*!*************************************************************
*! Name      : lfwOldVals
*! Developer : Hend Ghanem
*! Date      : 11/22/2001
*! Purpose   : When function of get fields.
*!             Stores a fields old value
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfwOldVals()
*!*************************************************************
FUNCTION lfwOldVals
lgOldVal = EVALUATE('m.' + SYS(18))

*!*************************************************************
*! Name      : lfwOldFVals
*! Developer : Hend Ghanem
*! Date      : 11/22/2001
*! Purpose   : When function of get fields.
*!             Stores a fields old value
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfwOldFVals()
*!*************************************************************
FUNCTION lfwOldFVals
lgOldVal = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfVldKey
*! Developer : Hend Ghanem
*! Date      : 11/22/2001
*! Purpose   : Validates a key field.
*!*************************************************************
*! Calls              :  ARIABROW()
*!*************************************************************
*! Passed Parameters  : lcSrchFile  : File to boe searched or browsed
*!                      lcSrchTag   : Tag to be used for seek or browse
*!                      lcBrFields  : Browse fields string
*!                      lcBrFlTtl   : Browse title
*!                      lcFldName   : return field name
*!                      lcNotFndMsg : message string to be displayed if the key
*!                                    is not found.
*!*************************************************************
*! Returns            :  .t. if the screen is to be refreshed
*!                       with new information, 
*!                       .f. otherwise.
*!*************************************************************
*! Example            :  lfVldKey('PMCALHD', 'PMCALHD',;
*!                                lcPathFlds, lcPathTtl,;
*!                                'cPath_ID')
*!*************************************************************
FUNCTION lfVldKey
PARAMETERS lcSrchFile, lcSrchTag, lcBrFields, lcBrFlTtl,;
           lcFldName, lcNotFndMsg, lcCurCont, lcBrowCond 
PRIVATE lcCurObj, lnCurObjLen, lnCurAlias, lnCurTag, lnCurRec, llShow

llShow     = .F.
lnCurAlias = SELECT(0)
SELECT (lcSrchFile)
lnCurTag   = VAL(SYS(21))
SET ORDER TO TAG (lcSrchTag)

lcCurObj     = 'm.' + SYS(18)
lcCurCont = IIF(EMPTY(lcCurCont), EVALUATE(lcCurObj), lcCurCont)
lnCurObjLen  = FSIZE(lcFldName, lcSrchFile)
lcBrowCond   = IIF(EMPTY(lcBrowCond), '', lcBrowCond)
lnCurRec     = RECNO()

IF llBrowse .OR. '?' $ lcCurCont
  GO TOP
  IF ARIABROW(lcBrowCond, lcBrFlTtl, ;
              gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2)
    &lcCurObj = EVALUATE(lcSrchFile + '.' + lcFldName)
    STORE .F. To laScrMode
    laScrMode[2] = .T.
    llShow     = .T.
  ELSE  
    IF BETWEEN(lnCurRec, 1, RECCOUNT())
      GO lnCurRec
    ENDIF   
    &lcCurObj = SPACE(lnCurObjLen)
    _CUROBJ   = _CUROBJ
  ENDIF  
ELSE
  IF !EMPTY(lcCurCont) 
    IF !SEEK(lcCurCont)  
      lnSoftSeek = RECNO(0)
      lnOption = gfModalgen("TRM38247B02004","DIALOG",lcNotFndMsg)
      DO CASE 
        CASE lnOption = 1
          IF BETWEEN(lnSoftSeek, 1, RECCOUNT())
            GO lnSoftSeek
          ELSE
            GO TOP
          ENDIF
          IF ARIABROW(lcBrowCond, lcBrFlTtl, ;
                          gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2)
            &lcCurObj = EVALUATE(lcSrchFile + '.' + lcFldName)
            STORE .F. To laScrMode
            laScrMode[2] = .T.
            llShow     = .T.
          ELSE   
            IF BETWEEN(lnCurRec, 1, RECCOUNT())
              GO lnCurRec
            ENDIF   
            &lcCurObj = SPACE(lnCurObjLen)
            _CUROBJ   = _CUROBJ
          ENDIF  
        CASE lnOption = 2
          STORE .F. To laScrMode
          laScrMode[4] = .T.
          llShow     = .T.
        OTHERWISE
          &lcCurObj = SPACE(lnCurObjLen)
          _CUROBJ   = _CUROBJ
      ENDCASE    
    ELSE
      STORE .F. TO laScrMode
      laScrMode[2] = .T.
      llShow     = .T.
    ENDIF  
  ENDIF
ENDIF     

llBrowse    = .F.

*-- Restore environment
SET ORDER TO (lnCurTag)
SELECT (lnCurAlias) 

RETURN llShow


*!*************************************************************
*! Name      : lfvCal_ID
*! Developer : Hend Ghanem
*! Date      : 11/22/2001
*! Purpose   : Validates a key field.
*!*************************************************************
*! Calls              :  lfVldKey()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvCal_ID()
*!*************************************************************
FUNCTION lfvCal_ID

lcCal_ID = PADR(ALLTRIM(lcCal_ID),4)
SHOW GET lcCal_ID
IF llBrowse .OR. !EMPTY(lcCal_ID) .AND. lcCal_ID <> lgOldVal
  =lfVldKey('PMCALHD', 'PMCALHD', @lcCalFlds, @lcCalTtl,;
            'cCal_ID', 'Calendar ID ' + ALLTRIM(lcCal_ID) + ;
             ' is not found in the calendars file.')
  SHOW GETS
ENDIF
            
*!*************************************************************
*! Name      : lfvDsc
*! Developer : Hend Ghanem
*! Date      : 11/22/2001
*! Purpose   : Validates free format string fields.
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvDsc()
*!*************************************************************
FUNCTION lfvDsc
IF m.ccal_des <> lgOldVal
  m.ccal_des = PADR(ALLTRIM(m.ccal_des), 50)
ENDIF

*!*************************************************************
*! Name      : lpDelScr
*! Developer : Hend Ghanem
*! Date      : 11/22/2001
*! Purpose   : Local Delete
*!*************************************************************
*! Calls     : lpShow()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDelete()
*!*************************************************************
FUNCTION lpDelScr
PRIVATE lnCurAlias, laDelRec

lnCurAlias = select(0)

*-- Delete lines first
SELECT PMCALHD
IF RLOCK()
  REPLACE llok_stat WITH .T.
  UNLOCK
  SELECT PMCALDT
  SCATTER MEMVAR MEMO BLANK
      
  *-- Scan through all the records of the same Path ID in the details file.
  SCAN FOR cCal_ID = lcCal_ID
    *-- for every line on the file, delete related predecessors.
    GATHER MEMVAR MEMO
    DELETE
  ENDSCAN
    
  SELECT PMCALHD
  SCATTER MEMVAR MEMO BLANK
  GATHER MEMVAR MEMO
  DELETE
    
  *-- Go back to 'S'elect mode
  STORE .F. TO laScrMode
  laScrMode[1] = .T.
  SHOW GETS
ENDIF  

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lpSavscr
*! Developer : Hend Ghanem
*! Date      : 11/22/2001
*! Purpose   : Local Save
*!*************************************************************
*! Calls     : lpShow()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpSavscr()
*!*************************************************************
FUNCTION lpSavscr
PRIVATE lcSetDel, lcSetExact

IF lfVldEntry()
  *-- Add audit information
  m.cCal_ID    = lcCal_ID
  m.cAdd_User  = gcUser_ID
  m.dAdd_Date  = ldCurDate
  m.cAdd_Time  = TIME()
  m.cCal_WEnd  = ''
  FOR lnCount = 1 TO 7 
    IF laWeekEnds[lnCount]
      m.cCal_WEnd  = m.cCal_WEnd + STR(lnCount,1)
    ENDIF
  ENDFOR
  
  *-- Save header information
  SELECT PMCALHD
  IF laScrMode[4]
    IF !SEEK(lcCal_ID)
      INSERT INTO PMCALHD (llok_stat) ;
            VALUES (.T.)
    ENDIF        
  ENDIF
  GATHER MEMVAR MEMO

  *-- Save details
  lcSetDel   = SET('DELETED')
  lcSetExact = SET('EXACT')

  SET DELETED OFF
  SET EXACT OFF

  SELECT (lc_PMCalDt)

  *-- Go through all the records of the temp cursor.
  SCAN
    DO CASE
      *-- Case of a new line
      CASE cStatus = 'A'        
        SCATTER MEMVAR MEMO

        SELECT PMCALDT
        IF SEEK(' ')
          =RLOCK()
          RECALL         
          GATHER MEMVAR MEMO
          UNLOCK
        ELSE
          INSERT INTO PMCALDT FROM MEMVAR
        ENDIF  

      *-- An old line that is modified in the current session.
      CASE cStatus = 'M'        
        *-- Add audit information
        SCATTER MEMVAR MEMO
        m.cAdd_User  = gcUser_ID
        m.dAdd_Date  = ldCurDate
        m.cAdd_Time  = TIME()
        SELECT PMCALDT
        IF BETWEEN(&lc_PMCalDt..nRecNo, 1, RECCOUNT())
          GO &lc_PMCalDt..nRecNo
        ENDIF  
        
        GATHER MEMVAR MEMO
          
      *-- An old line that is deleted in the current session.
      CASE cStatus = 'D'        
        SELECT PMCALDT
        IF BETWEEN(&lc_PMCalDt..nRecNo, 1, RECCOUNT())
          GO &lc_PMCalDt..nRecNo
        ENDIF  

        SCATTER MEMVAR MEMO BLANK
        =RLOCK() 
        GATHER MEMVAR MEMO
        DELETE
        UNLOCK
    ENDCASE  
  ENDSCAN

  SET DELETED &lcSetDel
  SET EXACT   &lcSetExact

  SELECT PMCALHD
  REPLACE llok_stat WITH .F.
  llCpyCalnd = .T.
  SHOW GETS
ENDIF

*!*************************************************************
*! Name      : lfvNew
*! Developer : Hend Ghanem
*! Date      : 11/22/2001
*! Purpose   : Valid function for pudh button < New > (pbNew)
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvNew()
*!*************************************************************
FUNCTION lfvNew

IF lfVldEntry()
  lnCurAlias = SELECT(0)
  SELECT (lc_PMCalDt)
  ACTIVATE WINDOW (lcWinCh2)
  *-- Save the current operation ID
  lnOldCalRec = RECNO()
  GO BOTTOM
  IF !EOF()
    SKIP
  ENDIF
  =lfwBrwWhen(lc_PMCalDt, .T.)
  SHOW GET m.cCal_HNam ENABLE
  SHOW GET m.dCal_HFrm ENABLE
  SHOW GET m.dCal_HTo  ENABLE
  _CUROBJ    = OBJNUM(m.cCal_HNam)
  SELECT (lnCurAlias)
ENDIF

*!*************************************************************
*! Name      : lfvRemove
*! Developer : Hend Ghanem
*! Date      : 11/22/2001
*! Purpose   : Valid function for pbRemove < Remove > (pbRemove)
*!             Removes a line from the browse.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRemove()
*!*************************************************************
FUNCTION lfvRemove
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)

IF gfModalgen("TRM38208B38006","DIALOG") = 1
  SELECT (lc_PMCalDt)
  lnNOfLines = lnNOfLines - 1
  REPLACE cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1)
  DELETE
  SKIP
  IF EOF() 
    IF lnNOfLines > 0
      SKIP -1
    ENDIF  
  ELSE
    lnRecNo = RECNO()
    lnCount = 0 
    GO TOP
    SCAN WHILE lnCount < lnNOfLines
      lnCount = lnCount + 1 
      REPLACE nLineNo WITH lnCount,;
              cStatus WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1)
    ENDSCAN
    IF BETWEEN(lnRecNo, 1, RECCOUNT())
      GO lnRecNo
    ENDIF  
  ENDIF  
  glUpdated   = .T.
  =lfwBrwWhen(lc_PMCalDt, !EOF())
ENDIF
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfReadAct
*! Developer : Hend Ghanem
*! Date      : 11/22/2001
*! Purpose   : READ Activate function of PRM500.SCX
*!*************************************************************
*! Calls              : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  =lfReadAct()
*!*************************************************************
FUNCTION lfReadAct
*-- Restore old key settings and clear the current settings
=gfClearKey()
ON KEY LABEL ALT+H ACTIVATE WINDOW (lcBrTtl)

*!*************************************************************
*! Name      : lfReadDeact
*! Developer : Hend Ghanem
*! Date      : 11/22/2001
*! Purpose   : READ Deactivate function of PRM500.SCX
*!*************************************************************
*! Calls     : lfTrapKey
*!*************************************************************
*! Returns            :  .f.
*!*************************************************************
*! Example            :  =lfReadDeact()
*!*************************************************************
FUNCTION lfReadDeact
*-- Key traps for the browse
IF WONTOP() = lcBrTtl
  ON KEY LABEL CTRL+Q     lnDummy = 1
  ON KEY LABEL CTRL+W     lnDummy = 1
  ON KEY LABEL CTRL+HOME  GO TOP
  ON KEY LABEL CTRL+END   GO BOTTOM
  ON KEY LABEL TAB        DO lpTab WITH lcWinCh2, OBJNUM(m.cCal_HNam)
  ON KEY LABEL BACKTAB    DO lpTab WITH lcWinCh0, ;
                          IIF(laScrMode[1] , OBJNUM(lcCal_ID),;
                                          OBJNUM(laWeekEnds[7])) 
  *ON KEY LABEL ESC        llDummy = lfTrapKey(lcWinCh2, OBJNUM(pbClose))
  IF laScrMode[1] OR laScrMode[2]  
    *ON KEY LABEL CTRL+ENTER llDummy = lfTrapKey(lcWinCh2, OBJNUM(pbClose))
  ELSE
    ON KEY LABEL ALT+W      llDummy = lfTrapKey(lcWinCh2, OBJNUM(pbNew))
    ON KEY LABEL ALT+V      llDummy = !EOF(lc_PMCalDt) .AND. lfvRemove()
    ON KEY LABEL ALT+O      llDummy = llCpyCalnd .AND. lfTrapKey(lcWinCh0, OBJNUM(pbCopyCal))
    ON KEY LABEL CTRL+ENTER llDummy = lfTrapKey(lcWinCh2, OBJNUM(pbAccept))
  ENDIF
ENDIF  
RETURN .F.

*!*************************************************************
*! Name      : lfTrapKey
*! Developer : Hend Ghanem
*! Date      : 11/22/2001
*! Purpose   : Trap of keys
*!*************************************************************
*! Passed Parameters  :  lcWindName : Window name to activate
*!                       lnObjName  : object to focus.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  llDummy = lfTrapKey('MFG10002', OBJNUM(pbNew)
*!*************************************************************
FUNCTION lfTrapKey
PARAMETERS lcWindName, lnObjNum
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = lnObjNum
KEYBOARD CHR(13) CLEAR

*!*************************************************************
*! Name      : lpTab
*! Developer : Hend Ghanem
*! Date      : 11/22/2001
*! Purpose   : Trap of tab keys
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  DO lpTab WITH 'MFG10002', OBJNUM(pbNew)
*!*************************************************************
PROCEDURE lpTab
PARAMETERS lcWindName, lnObjNum
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = lnObjNum

*!*************************************************************
*! Name      : lfvCalHNam
*! Developer : Hend Ghanem
*! Date      : 11/22/2001
*! Purpose   : Validates holiday name field
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvCalHNam()
*!*************************************************************
*!
FUNCTION lfvCalHNam

lnCurAlias = SELECT(0)
m.cCal_HNam   = PADR(ALLTRIM(m.cCal_HNam), 30)
IF EMPTY(m.cCal_HNam)  
  IF m.cCal_HNam = lgOldVal
    IF BETWEEN(lnOldCalRec, 1 , RECCOUNT(lc_PMCalDt))
      GO lnOldCalRec IN (lc_PMCalDt)
      KEYBOARD '{ALT+H}'
    ENDIF  
    =lfwBrwWhen(lc_PMCalDt, !EOF())
  ELSE
    m.cCal_HNam = lgOldVal
    _CUROBJ = _CUROBJ
  ENDIF    
ELSE
  IF m.cCal_HNam <> lgOldVal
    IF EOF(lc_PMCalDt)  
      lnNOfLines = lnNOfLines + 1
      INSERT INTO (lc_PMCalDt);
              (  cCal_ID  ,  cCal_HNam,  nLineNo    , cStatus,;
                 cAdd_User,  dAdd_Date, cAdd_Time);
        VALUES(lcCal_ID  , m.cCal_HNam, lnNOfLines, 'A'    ,;
                 gcUser_ID  , ldCurDate , TIME())
       m.nLineNo   = lnNOfLines 
       lnCurCalRec = RECNO(lc_PMCalDt)
       KEYBOARD '{ALT+H}'
       =lfwBrwWhen(lc_PMCalDt, .T.)   
    ELSE
      SELECT (lc_PMCalDt)
      REPLACE cCal_HNam WITH m.cCal_HNam,;
              cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1),;
              cAdd_User WITH gcUser_ID,;
              dAdd_Date WITH ldCurDate,;
              cAdd_Time WITH TIME()
      SHOW WINDOW (lcBrTtl) REFRESH SAME
    ENDIF 
    glUpdated = .T.
  ENDIF  
ENDIF
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvCalDate
*! Developer : Hend Ghanem
*! Date      : 11/22/2001
*! Purpose   : Validates holiday date fields
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvCalDate()
*!*************************************************************
FUNCTION lfvCalDate
PRIVATE lcCurObj, lnCurAlias
lcCurObj = 'm.'+SYS(18)
IF EVAL(lcCurObj) <> lgOldVal 
  lnCurAlias = select(0)
  SELECT (lc_PMCalDt)
  IF !EMPTY(m.dCal_HFrm)
    IF EMPTY(m.dCal_HTo)
      m.dCal_HTo = m.dCal_HFrm
      REPLACE dCal_HFrm WITH m.dCal_HFrm,;
              dCal_HTo  WITH m.dCal_HTo ,;
              cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1),;
              cAdd_User WITH gcUser_ID,;
              dAdd_Date WITH ldCurDate,;
              cAdd_Time WITH TIME()
      glUpdated = .T.
      SHOW WINDOW (lcBrTtl) REFRESH SAME  
    ELSE
      IF m.dCal_HFrm > m.dCal_HTo
        =gfModalgen("TRM38209B00000","DIALOG")
        &lcCurObj = lgOldVal
        _CUROBJ   = _CUROBJ
      ELSE
        REPLACE dCal_HFrm WITH m.dCal_HFrm,;
                dCal_HTo  WITH m.dCal_HTo ,;
                cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1),;
                cAdd_User WITH gcUser_ID,;
                dAdd_Date WITH ldCurDate,;
                cAdd_Time WITH TIME()
        glUpdated = .T.
        SHOW WINDOW (lcBrTtl) REFRESH SAME  
      ENDIF
    ENDIF  
  ELSE
    IF !EMPTY(m.dCal_HTo)
      m.dCal_HFrm = m.dCal_HTo
      SHOW GET m.dCal_HFrm
    ENDIF  
    REPLACE dCal_HFrm WITH m.dCal_HFrm,;
            dCal_HTo  WITH m.dCal_HTo ,;
            cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1),;
            cAdd_User WITH gcUser_ID,;
            dAdd_Date WITH ldCurDate,;
            cAdd_Time WITH TIME()
    glUpdated = .T.
    SHOW WINDOW (lcBrTtl) REFRESH SAME  
  ENDIF
  SELECT (lnCurAlias)    
ENDIF

*!*************************************************************
*! Name      : lfVldEntry
*! Developer : Hend Ghanem
*! Date      : 11/22/2001
*! Purpose   : Checks if a line entry is complete
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : .T. if the line entry is valid,
*!                      .F. otherwise
*!*************************************************************
*! Example            : IF lfVldEntry()
*!*************************************************************
FUNCTION lfVldEntry
PRIVATE llVldEntry
llVldEntry = .T.
IF !EOF(lc_PMCalDt)
  IF EMPTY(m.dcal_hfrm)
    IF EMPTY(m.dcal_hto)
      llVldEntry = gfModalgen("TRM38210B00000","DIALOG") = 0
      IF WONTOP() = lcBrTtl
        IF BETWEEN(lnCurCalRec, 1 , RECCOUNT())
          GO lnCurCalRec
        ENDIF  
      ENDIF  
      _CUROBJ = OBJNUM(m.dcal_hfrm)
    ELSE
      m.dcal_hfrm =  m.dcal_hto 
    ENDIF
  ELSE    
    IF EMPTY(m.dcal_hto)
      m.dcal_hto = m.dcal_hfrm
    ENDIF
  ENDIF    
  SHOW GET m.dcal_hfrm
  SHOW GET m.dcal_hto
ENDIF  
RETURN llVldEntry


*!*************************************************************
*! Name      : lfvCopyCal
*! Developer : Hend Ghanem
*! Date      : 11/22/2001
*! Purpose   : Valid function for push button pbCopyCal
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvCopyCal()
*!*************************************************************
FUNCTION lfvCopyCal
PRIVATE lnCurAlias, llAppend, llOverwrite

lnCurAlias  = SELECT(0)
STORE .F. TO llAppend, llOverwrite
lcCal_Dsc   = SPACE(50)
lcCpCal_id  = SPACE(4)
SELECT PMCALHD
SET FILTER TO cCal_ID <> lcCal_ID

PUSH KEY
=gfClearKey()

*E301869,2 AMH Call screen from the correct directory [Start]
*DO (gcScrDir +"SM\"+ '\SMCPYCAL.SPR')
DO (gcScrDir +gcWinAppl + '\SMCPYCAL.SPR')
*E301869,2 AMH [End]
POP KEY

SELECT PMCALHD
SET FILTER TO 

IF llOverwrite
  llAppend     = .T.
  SELECT (lc_PMCalDt)
  REPLACE ALL cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1)
  DELETE ALL
  lnNOfLines = 0
ENDIF
IF llAppend
  SELECT PMCALHD
  laWeekEnds  = .F.
  FOR lnCount = 1 TO LEN(ALLTRIM(PMCALHD.cCal_WEnd))
    laWeekEnds[VAL(SUBSTR(PMCALHD.cCal_WEnd, lnCount, 1))] = .T.
  ENDFOR  
  SELECT PMCALDT
  IF SEEK(lcCpCal_ID)
    glUpdated = .T.
    SCAN REST WHILE cCal_ID = lcCpCal_ID
      SCATTER MEMVAR MEMO
      lnNOfLines = lnNOfLines + 1
      INSERT INTO (lc_PMCalDt);
        (cCal_ID  ,  nLineNo    ,   cCal_HNam,  dCal_HFrm,   dCal_HTo,;
         cAdd_User, dAdd_Date   , cAdd_Time  , cStatus);
       VALUES;
        (lcCal_ID , lnNOfLines, m.cCal_HNam, m.dCal_HFrm, m.dCal_HTo,;
         gcUser_ID , ldCurDate   , TIME()     , 'A') 
    ENDSCAN
  ENDIF  
  GO TOP IN (lc_PMCalDt)
  =lfwBrwWhen(lc_PMCalDt, lnNOfLines > 0)
ENDIF  
SELECT PMCALHD
=SEEK(lcCal_ID)
SELECT (lnCurAlias)



*!*************************************************************
*! Name      : lfSetFlag
*! Developer : Hend Ghanem
*! Date      : 11/22/2001
*! Purpose   : Valid function for push button pbCopyCal
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfSetFlag(@llAppend)
*!*************************************************************
FUNCTION lfSetFlag
PARAMETERS llFlagToSet
IF EMPTY(lcCpCal_ID)
  =gfModalgen("TRM38213B00000","DIALOG")
  _CUROBJ = OBJNUM(lcCpCal_ID)
ELSE
  llFlagToSet = .T.
  CLEAR READ
ENDIF  


*!*************************************************************
*! Name      : lfvCpCalID
*! Developer : Hend Ghanem
*! Date      : 11/22/2001
*! Purpose   : Valid function for get field lcCpCal_ID 
*!             IN SMCPYCAL.SPR
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfvCpCalID()
*!*************************************************************
FUNCTION lfvCpCalID
lcCpCal_id = PADR(ALLTRIM(lcCpCal_id),4)
SHOW GET lcCpCal_id

IF llBrowse .OR. (!EMPTY(lcCpCal_id) .AND. !SEEK(lcCpCal_id,'PMCALHD') .AND. lcCpCal_id <> lgOldVal)
  llBrowse = .F.
  SELECT PMCALHD
  SET ORDER TO TAG PMCALHD
  IF ARIABROW('', lcCalTtl, ;
              gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2)
    STORE .F. To laScrMode
    laScrMode[4] = .T.
    lcCpCal_id = PMCALHD.cCal_ID
    lcCal_Dsc  = PMCALHD.cCal_des
    =lfRefresh()
  ELSE
    lcCpCal_id = lgOldVal
  ENDIF
ELSE
  
  *E301869,2 AMH Fix bug of wronge discription [Start]
  *=SEEK(ALLTRIM(lcCpCal_id),'PMCALHD')
  =SEEK(PADR(lcCpCal_id,4),'PMCALHD')
  *E301869,2 AMH [End]
  
  lcCal_Dsc  = PMCALHD.cCal_des
  =lfRefresh()
ENDIF