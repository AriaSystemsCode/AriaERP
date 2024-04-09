*:************************************************************************
*: Program file  : MFPATH.PRG
*: Program desc. : Template information    (N00341)
*: For screen    : MFPATH
*:         System: ARIA APPAREL SYSTEM
*:      Developer: HEND GHANEM
*:************************************************************************
*: Calls         : MFPATH.SPX
*:                 MFPATHA.SPX      
*:*************************************************************************
*: Passed Parameters  : None
*:*************************************************************************
*: Example            : DO MFPATH
*:************************************************************************
*: Modifications :
*E301869,2 AMH 11/19/2002 Create a production schedule project according to a setting.
*B038431,1 MHM 08/23/2004 Fix Bug Of variable not defined in multi session          
*:E302251,1 AMH 8/31/2005  Calling the Notify Seting screen.
*:************************************************************************
PRIVATE lcBrTtl, lnBrRecNo, lnNumOfLines, lnCurObj, lcFuncName 


STORE '' TO lcOldVal,lcFuncName,lcModal
lcBrTtl      = 'Tasks Data'
lcPathFlds   = [cPath_ID : H = 'Template ID', cPath_Dsc : H = 'Template Name']
lcOprFlds    = [cOprt_ID : H = 'Task ID', cOprt_Dsc : H = 'Task Name', cOprt_Res:H='Responsible', nEst_Dur : H = 'Duration']
lcPathTtl    = 'Project Templates'
lcCtgFlds    = [cOprt_Ctg: H = 'Category', cCtg_Dsc : H = 'Description']
lcCtgTtl     = 'Categories of Tasks'
STORE ' ' TO lcOprt_Ctg

lcPath_ID    = SPACE(4)
DECLARE laOperaton[1], laPredecsr[1], laUpdtMthd[1], laUsers[1],;
        laNotified[1], laCategory[1] 
laUpdtMthd[1] = 'Manual'

lcCalFlds    = [cCal_ID : H = 'Calendar ID', cCal_Des : H = 'Description']
lcCalTtl     = 'Calendars'

STORE '' TO lcmcpathid,lcmpathdsc,lcmpathcom,lc_PMPthDt,lc_PMPthRl

*--ToolBar external proc.
laDefProc[7]  = .F.              && Disable the control panel delete proc. (lpDelScr)
laDefProc[9]  = .F.              && Disable the control panel save proc.   (lpSavScr)
laDefProc[10] = .F.              && Disable the control panel save proc.   (lpClsScr)

lcEnbCntrl   = SCHEME(1,9)
lcDisCntrl   = SCHEME(1,10)
lcSelCntrl   = SCHEME(1,6)

STORE SPACE(10) TO lcoprt_res 
*-- Screen objects state of display
lcKeyStat   = 'ENABLE'
lcEditStat  = 'DISABLE'    
lcOperStat  = 'DISABLE'    

STORE 0 TO lnNOfLines, lnBrRecNo 
lnCurObj     = 1

ldCurDate    = gdSysDate
STORE .F. TO llBrowse, glUpdated,llEnbSave,llMousClic, llExitLoop
STORE .F. TO  llOper 

*B038431,1 MHM 08/23/2004 Define variable before GFSETUP to used in multi session[Start]          
lnOprWdth = ''
*B038431,1 MHM 08/23/2004 [End]          

*E302251,1 AMH Define variables for Nofity Setting Screen [Start]
lcBrUser   = "Users"
lcPMPTHNTF = gfTempName()
*E302251,1 AMH [End]

IF !gfSetup()
  RETURN
ENDIF  

lcUsedTemp = gfGetMemVar('M_SODEFTMP') + '|' +;
             gfGetMemVar('M_PODEFTMP') + '|' +;
             gfGetMemVar('M_MFDEFTMP')
m.cpath_id  = lcmcpathid
m.cpath_dsc = lcmpathdsc
m.mpath_com = lcmpathcom
IF !WEXIST(gcBaseWind)
  lc_PMPthDt = gfTempName()
  lc_PMPthRl = gfTempName()
  lcScFields = 'cpath_id,cpath_dsc,mpath_com'
  *-- Create a file with the same structure as PMPTHDT 
  *-- adding two more fields for saving.
  SELECT PMPTHDT
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

  CREATE CURSOR (lc_PMPthDt) FROM ARRAY laFileStru
  *-- Use index expressions with print sequence and category code fields
  INDEX ON cOprt_Ctg + cOprt_ID TAG PMPTHDT OF (lc_PMPthDt)
  INDEX ON cPath_ID + cCtg_Seq + cOprt_Seq TAG PMPTHDTS OF (lc_PMPthDt)

  SELECT PMPTHRL
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
  CREATE CURSOR (lc_PMPthRl) FROM ARRAY laFileStru
  *-- Use index expressions with category code fields
  INDEX ON cPrd_Ctg + cPrd_ID  TAG PMPTHRLP OF (lc_PMPthRl)
  INDEX ON cOprt_Ctg + cOprt_ID TAG PMPTHRL OF (lc_PMPthRl)
 
  SELECT cUser_ID ;
    FROM (gcSysHome + 'SYUUSER');
    INTO ARRAY laUsers;
   ORDER BY cUser_ID
  SELECT (lc_PMPthDt)
  *-- Use index expressions with category code fields
  SET RELATION TO cOprt_Ctg + cOprt_ID INTO (lc_PMPthRl)
  SELECT PMPTHDT
  *-- Use index expressions with category code fields
  SET RELATION TO cPath_ID + cOprt_Ctg + cOprt_ID INTO PMPTHRL
  SELECT PMPTHHD
  SET RELATION TO cPath_ID INTO PMPTHDT
  GO TOP
  SCATTER MEMVAR MEMO BLANK
  lnOprWdth = '35'  
  
  *E302251,1 AMH Create cursor to hold data of Nofity Setting Screen [Start]
  CREATE CURSOR (lcPMPTHNTF) (CPath_ID   C(4)  ,COprt_Ctg  C(3)  ,COprt_ID   C(5)  ,CUser_ID   C(10),LBfrStrt L,;
                              NBfrStrtDy N(3,0),LBfrCmplt  L     ,NBfrCmplDy N(3,0),LOnStrt    L    ,LOnCmplt L,;
                              LOnRedrct  L     ,NStrtDelay N(3,0),NCmplDelay N(3,0),CEMAIL_ADD C(60),cStatus  C(1))
  SELECT (lcPMPTHNTF)
  INDEX ON CPath_ID+COprt_Ctg+COprt_ID+CUser_ID TAG (lcPMPTHNTF) OF (lcPMPTHNTF)
  =gfOpenFile(gcDatadir+'PMPTHNTF','PMPTHNTF','SH')
  =gfOpenFile(gcDatadir+'PMPCTGNT','PMPCTGNT','SH')
  *E302251,1 AMH [End]
  
ELSE
  =lpShow()
ENDIF

PUSH KEY
=gfClearKey()
REGIONAL llExitLoop 
llExitLoop = .F.
DO WHILE !llExitLoop
  *-- Clear read as we branch to Operations screen
  ON KEY LABEL ESC DO  lpEscScr
  DO (gcScrDir +"MFPATH.SPX")
  llExitLoop = .T.  
  IF !llOper 
    EXIT
  ENDIF
  IF !EMPTY(lcFuncName)
    &lcFuncName
    lcFuncName = ' '
    lcKeyStat  = 'DISABLE'    
    lcEditStat = 'ENABLE'
    lcOperStat = 'ENABLE'
  ENDIF
ENDDO

POP KEY

IF glQuitting
  SELECT PMPTHHD
  SET RELATION TO
  IF USED(lc_PMPthDt)
    USE IN (lc_PMPthDt)
  ENDIF
  IF USED(lc_PMPthRl)  
    USE IN (lc_PMPthRl)  
  ENDIF
  
  *E302251,1 AMH Remove cursor of Nofity Setting Screen [Start]
  IF USED(lcPMPTHNTF)
    USE IN (lcPMPTHNTF)
  ENDIF
  *E302251,1 AMH [End]
  
ENDIF
  
*!*************************************************************
*! Name      : lpShow
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
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
  CASE laScrMode[1]
    
    *E302251,1 AMH Collecting Data for Nofity Setting Screen [Start]
    SELECT (lcPMPTHNTF)
    ZAP
    *E302251,1 AMH [End]
    
    llEnbSave = .F.
    SELECT PMPTHHD
    SCATTER MEMVAR MEMO BLANK
    SHOW GET ibPath_ID   ENABLE
    SHOW GET m.cpath_id  ENABLE
    SHOW GET m.mpath_com ENABLE  
    SHOW GET pbNotes     DISABLE
    SHOW GET pbOprs      DISABLE
    _CUROBJ = OBJNUM(m.cPath_ID)
    lcOperStat  = 'DISABLE'    
           
  CASE laScrMode[2]
    
    DECLARE laOperaton[1], laPredecsr[1], laNotified[1]
    STORE '' TO laOperaton, laPredecsr, laNotified
    
    *E302251,1 AMH Collecting Data for Nofity Setting Screen [Start]
    SELECT (lcPMPTHNTF)
    ZAP
    *E302251,1 AMH [End]
    
    SELECT PMPTHHD
    SCATTER MEMVAR MEMO   
    *-- Collect all path operations in an array. 
    SELECT cOprt_Ctg + '\' + cOprt_ID + ' ' +SUBSTR(cOprt_Dsc, 1, 18);
      FROM PMPTHDT;
     WHERE cpath_id+coprt_ctg+coprt_id = m.cPath_ID ;
      INTO ARRAY laOperaton
    =lfGetData()
    
    *E302251,1 AMH Collecting Data for Nofity Setting Screen [Start]
    SELECT PMPTHNTF
    IF SEEK(m.cPath_ID)
      SCAN REST WHILE CPATH_ID+COPRT_CTG+COPRT_ID+CUSER_ID = m.cPath_ID
        SCATTER MEMVAR
        m.cStatus = 'S'
        INSERT INTO (lcPMPTHNTF) FROM MEMVAR
      ENDSCAN
    ENDIF
    *E302251,1 AMH [End]
    
    SHOW GET ibPath_ID   DISABLE
    SHOW GET m.cpath_id  DISABLE
    SHOW GET m.cpath_dsc DISABLE
    SHOW GET pbNotes     ENABLE
    SHOW GET pbOprs      ENABLE
    lcOperStat  = 'ENABLE'    
    SHOW GET lcOprt_Res  DISABLE
    SHOW GET puUser      DISABLE
    
  CASE laScrMode[3]
    
    llCUpDate  = .T.
    llExitLoop = .F.
    llOper     = .F.
    IF !llEnbSave
      =lfGetData()
      SELECT PMPTHHD
      SCATTER MEMVAR MEMO
    ENDIF
    SHOW GET m.cpath_dsc ENABLE
    SHOW GET pbOprs      ENABLE
    lcOperStat  = 'ENABLE'  
    
    IF llEnbSave
      SHOW GET pbSav ENABLE  
      laCtrStat[11] = 'ENABLE'
    ENDIF
    
  CASE laScrMode[4]

    llCUpDate  = .T.

    glUpdated    = .T.
    
    IF !llEnbSave
      DECLARE laOperaton[1], laPredecsr[1], laNotified[1],;
              laCategory[1] 
      STORE '' TO laOperaton, laPredecsr, laNotified,;
                  laCategory
      lnNOfLines = 0

      SELECT (lc_PMPthRl)
      ZAP

      SELECT (lc_PMPthDt)
      ZAP
    ENDIF
      
    SHOW GET ibPath_ID   DISABLE
    SHOW GET m.cpath_id  DISABLE
    SHOW GET m.cpath_dsc ENABLE
    SHOW GET pbNotes     ENABLE
    SHOW GET pbOprs      ENABLE
    lcOperStat  = 'ENABLE'  
    
    IF llEnbSave
      SHOW GET pbSav ENABLE  
      laCtrStat[11] = 'ENABLE'
    ENDIF

ENDCASE
SHOW GET m.mpath_com 
lcPath_ID  = m.cPath_ID

lcmcpathid = m.cpath_id
lcmpathdsc = m.cpath_dsc
lcmpathcom = m.mpath_com  

SELECT PMPTHHD

*!*************************************************************
*! Name      : lfwOldVals
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : When function of get fields.
*!             Stores a fields old value
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfwOldVals()
*!*************************************************************
FUNCTION lfwOldVals
lcOldVal = EVALUATE('m.' + SYS(18))

*!*************************************************************
*! Name      : lfwOldFVal
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : When function of get fields.
*!             Stores a fields old value
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfwOldFVal()
*!*************************************************************
FUNCTION lfwOldFVal
lcOldVal = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfVldKey
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Validates a key field.
*!*************************************************************
*! Calls              :  ARIABROW()
*!*************************************************************
*! Passed Parameters  : lcSrchFile  : File to boe searched or browsed
*!                      lcSrchTag   : Tag to be used for seek or browse
*!                      lcBrFields  : Browse fields string
*!                      lcBrFlTtl   : Browse title
*!                      lcFldName   : return field name
*!                      lcNtFndMsg : message string to be displayed if the key
*!                                    is not found.
*!*************************************************************
*! Returns            :  .t. if the screen is to be refreshed
*!                       with new information, 
*!                       .f. otherwise.
*!*************************************************************
*! Example            :  lfVldKey('PMPTHHD', 'PMPTHHD',;
*!                                lcPathFlds, lcPathTtl,;
*!                                'cPath_ID')
*!*************************************************************
FUNCTION lfVldKey
PARAMETERS lcSrchFile, lcSrchTag, lcBrFields, lcBrFlTtl,;
           lcFldName, lcNtFndMsg, lcCurContent, lcBrowCond 
PRIVATE lcCurObj, lnCurObjLen, lnCurAlias, lnCurTag, lnCurRec, llShow

llShow     = .F.

lnCurAlias = SELECT(0)
SELECT (lcSrchFile)
lnCurTag   = VAL(SYS(21))
SET ORDER TO TAG (lcSrchTag)

lcCurObj     = 'm.' + SYS(18)
lcCurContent = IIF(EMPTY(lcCurContent), EVALUATE(lcCurObj), lcCurContent)
lnCurObjLen  = FSIZE(lcFldName, lcSrchFile)
lcBrowCond   = IIF(EMPTY(lcBrowCond), '', lcBrowCond)
lnCurRec     = RECNO()

IF llBrowse .OR. '?' $ lcCurContent
  GO TOP
  DIME laTemp[1]
  IF ARIABROW(lcBrowCond, lcBrFlTtl, ;
              gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,lcFldName,'laTemp')
    &lcCurObj = EVALUATE(lcSrchFile + '.' + lcFldName)
    STORE .F. TO laScrMode
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
  IF !EMPTY(lcCurContent) 
    IF !SEEK(lcCurContent)  
      lnSoftSeek = RECNO(0)
      lnOption = gfModalgen("TRM38247B02004","DIALOG",lcNtFndMsg)  
      DO CASE 
        CASE lnOption = 1
          IF BETWEEN(lnSoftSeek, 1, RECCOUNT())
            GO lnSoftSeek
          ELSE
            GO TOP
          ENDIF
          DIME laTemp[1]
          IF ARIABROW(lcBrowCond, lcBrFlTtl, ;
                          gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,lcFldName,'laTemp')
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
          STORE .F. TO laScrMode
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
*! Name      : lfvPath_ID
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Validates a key field.
*!*************************************************************
*! Calls              :  lfVldKey()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvPath_ID()
*!*************************************************************
FUNCTION lfvPath_ID
m.cPath_ID = PADR(ALLTRIM(m.cPath_ID),4)
IF llBrowse .OR. !EMPTY(m.cPath_ID) .AND. m.cPath_ID <> lcOldVal
  =lfVldKey('PMPTHHD', 'PMPTHHD', @lcPathFlds, @lcPathTtl,;
            'cPath_ID', 'Template code ' + ALLTRIM(m.cPath_ID) + ;
             ' is not found in the project Templates file.')
  SHOW GETS
ENDIF

lcmcpathid = m.cpath_id
            
*!*************************************************************
*! Name      : lfvPathDsc
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Validates a key field.
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvPathDsc()
*!*************************************************************
FUNCTION lfvPathDsc
IF m.cPath_Dsc <> lcOldVal
  m.cPath_Dsc = PADR(ALLTRIM(m.cPath_Dsc), 40)
ENDIF
lcmpathdsc = m.cpath_dsc
*!*************************************************************
*! Name      : lpDelScr
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Valid function for push button < Delete > (pbDelete).
*!             Saves current modifications to the file. 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpDelScr()
*!*************************************************************
FUNCTION lpDelScr
PRIVATE lnCurAlias, laDelRec

lnCurAlias = SELECT(0)

*-- Check if template used in setups
IF m.cpath_id $ lcUsedTemp
  =gfModalgen("TRM38255B00000","DIALOG")
  RETURN
ENDIF

*-- Delete lines first
SET ORDER TO PMPRPTH IN PMPRJHD
IF SEEK(lcPath_ID,'PMPRJHD')
  =gfModalgen("INM38239B00000","DIALOG")          
  RETURN
ENDIF

SELECT PMPTHHD
IF RLOCK()
  REPLACE llok_stat WITH .T.
  UNLOCK
  SELECT PMPTHDT
  SCATTER MEMVAR MEMO BLANK
      
  *-- Scan through all the records of the same Path ID in the details file.
  SCAN FOR cpath_id+coprt_ctg+coprt_id = lcPath_ID
    *-- Use category code field
    lcOprt_Ctg = cOprt_Ctg
    SELECT PMPTHRL
    *-- Use category code field
    SCAN FOR cpath_id+coprt_ctg+coprt_id = lcPath_ID + lcOprt_Ctg   
      SCATTER TO laDelRec MEMO BLANK
      GATHER FROM laDelRec MEMO
      DELETE
    ENDSCAN
    
    *E302251,1 AMH Delete Notify Setting Records [Start]
    SELECT PMPTHNTF
    IF SEEK(lcPath_ID+lcOprt_Ctg+PMPTHDT.cOprt_ID)
      DELETE REST WHILE cPath_ID+COprt_Ctg+COprt_ID+CUser_ID = lcPath_ID+lcOprt_Ctg+PMPTHDT.cOprt_ID
    ENDIF
    *E302251,1 AMH [End]
    
    SELECT PMPTHDT  
    *-- for every line on the file, delete related predecessors.
    GATHER MEMVAR MEMO
    DELETE
  ENDSCAN
    
  SELECT PMPTHHD
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
*! Name      : lpSavScr
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Valid function for push button < Accept > (pbAccept).
*!             Saves current modifications to the file. 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpSavScr()
*!*************************************************************
FUNCTION lpSavScr
PRIVATE lcSetDel, lcSetExact

GO TOP IN (lc_PMPthDt)
IF EOF(lc_PMPthDt)
  =gfModalgen("INM38217B00000","DIALOG")          
  _CUROBJ = OBJNUM(pbOprs)
  llCSave = .F.
  RETURN 
ELSE  
  *-- Add audit information
  m.cAdd_User  = gcUser_ID
  m.dAdd_Date  = ldCurDate
  m.cAdd_Time  = TIME()

  *-- Save header information
  SELECT PMPTHHD
  IF SEEK(m.cPath_Id)
    m.llok_stat = .T.
    GATHER MEMVAR MEMO
  ELSE
    INSERT INTO PMPTHHD (llok_stat) ;
                 VALUES (.T.)
    GATHER MEMVAR MEMO
  ENDIF    

  *-- Save details
  lcSetDel   = SET('DELETED')
  lcSetExact = SET('EXACT')

  SET DELETED OFF
  SET EXACT OFF

  SELECT (lc_PMPthDt)

  *-- Go through all the records of the temp cursor.
  SCAN
    DO CASE
      *-- Case of a new line
      CASE cStatus = 'A'        
        SCATTER MEMVAR MEMO

        SELECT PMPTHDT
        IF SEEK(' ')
          =RLOCK()
          RECALL         
          GATHER MEMVAR MEMO
          UNLOCK
        ELSE
          INSERT INTO PMPTHDT FROM MEMVAR
        ENDIF  

      *-- An old line that is modified in the current session.
      CASE cStatus = 'M'        
        *-- Add audit information
        SCATTER MEMVAR MEMO
        m.cAdd_User  = gcUser_ID
        m.dAdd_Date  = ldCurDate
        m.cAdd_Time  = TIME()
        SELECT PMPTHDT
        IF BETWEEN(&lc_PMPthDt..nRecNo, 1, RECCOUNT())
          GO &lc_PMPthDt..nRecNo
        ENDIF  
        
        GATHER MEMVAR MEMO
          
      *-- An old line that is deleted in the current session.
      CASE cStatus = 'D'        
        SELECT PMPTHDT
        IF BETWEEN(&lc_PMPthDt..nRecNo, 1, RECCOUNT())
          GO &lc_PMPthDt..nRecNo
        ENDIF  

        SCATTER MEMVAR MEMO BLANK
        =RLOCK() 
        GATHER MEMVAR MEMO
        DELETE
        UNLOCK
    ENDCASE  

    *-- Update related records for every line.
    SELECT (lc_PMPthRl)
    =SEEK(&lc_PMPthDt..cOprt_Ctg + &lc_PMPthDt..cOprt_ID)
    *-- Use category code field
    SCAN REST WHILE cOprt_Ctg + cOprt_ID = ;
               &lc_PMPthDt..cOprt_Ctg + &lc_PMPthDt..cOprt_ID

      DO CASE
        *-- Case of a new line
        CASE cStatus = 'A'        
          SCATTER MEMVAR MEMO

          SELECT PMPTHRL
          IF SEEK(' ')
            =RLOCK()
            RECALL         
            GATHER MEMVAR MEMO
            UNLOCK
          ELSE
            INSERT INTO PMPTHRL FROM MEMVAR
          ENDIF  

        *-- An old line that is modified in the current session.
        CASE cStatus = 'M'        
          *-- Add audit information
          SCATTER MEMVAR MEMO
          m.cAdd_User  = gcUser_ID
          m.dAdd_Date  = ldCurDate
          m.cAdd_Time  = TIME()
          SELECT PMPTHRL
          IF BETWEEN(&lc_PMPthRl..nRecNo, 1, RECCOUNT())
            GO &lc_PMPthRl..nRecNo
          ENDIF  

          GATHER MEMVAR MEMO
          
        *-- An old line that is deleted in the current session.
        CASE cStatus = 'D'        
          SELECT PMPTHRL
          IF BETWEEN(&lc_PMPthRl..nRecNo, 1, RECCOUNT())
            GO &lc_PMPthRl..nRecNo
          ENDIF  

          SCATTER MEMVAR MEMO BLANK
          GATHER MEMVAR MEMO
          DELETE
      ENDCASE  
    ENDSCAN  
    
    *E302251,1 AMH Update Nofity Setting for each operation [Start]
    SELECT (lcPMPTHNTF)
    IF SEEK(EVALUATE(lc_PMPthDt+".CPath_ID+"+lc_PMPthDt+".COprt_Ctg+"+lc_PMPthDt+".cOprt_ID"))
      SCAN REST WHILE CPath_ID+COprt_Ctg+COprt_ID+CUser_ID = EVALUATE(lc_PMPthDt+".CPath_ID+"+lc_PMPthDt+".COprt_Ctg+"+lc_PMPthDt+".cOprt_ID")
        DO CASE
          *-- Case of a new line
          CASE cStatus = 'A'        
            SCATTER MEMVAR

            SELECT PMPTHNTF
            IF SEEK(' ')
              =RLOCK()
              RECALL         
              GATHER MEMVAR
              UNLOCK
            ELSE
              INSERT INTO PMPTHNTF FROM MEMVAR
            ENDIF  

          *-- An old line that is modified in the current session.
          CASE cStatus = 'M'        
            * Add audit information
            SCATTER MEMVAR
            m.cAdd_User  = gcUser_ID
            m.dAdd_Date  = ldCurDate
            m.cAdd_Time  = TIME()
            SELECT PMPTHNTF
            IF SEEK(m.CPath_ID+m.COprt_Ctg+m.COprt_ID+m.CUser_ID)
              GATHER MEMVAR
            ENDIF  
          
          *-- An old line that is deleted in the current session.
          CASE cStatus = 'D'        
            SCATTER MEMVAR
            SELECT PMPTHNTF
            IF SEEK(m.CPath_ID+m.COprt_Ctg+m.COprt_ID+m.CUser_ID)
              DELETE
            ENDIF
        ENDCASE
      ENDSCAN
    ENDIF
    *E302251,1 AMH [End]
    
    SELECT (lc_PMPthDt)
  ENDSCAN

  SET DELETED &lcSetDel
  SET EXACT   &lcSetExact

  *-- Back to 'S'elect mode.
  SELECT PMPTHHD
  REPLACE llok_stat WITH .F.
  SHOW GETS           
ENDIF
SELECT (lc_PMPthDt)
ZAP
llExitLoop = .T.    

*!*************************************************************
*! Name      : lfvOperton
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Valid function for push button < Operations > (pbOprs)
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvOperton()
*!*************************************************************
FUNCTION lfvOperton
PRIVATE lcOprt_ID, lcOldOprt, lcOprt_Ctg, lcOldCtg

IF laScrMode[2]
  STORE 'DISABLE' TO lcDispStat, lcNewStat, lcRemStat 
  SELECT PMPTHHD
  IF BETWEEN(RECNO(), 1, RECCOUNT())
    GO RECNO()
  ENDIF  
  SELECT PMPTHDT
  lcNotfyMod = 'NOMODIFY'
ELSE
  STORE IIF(EOF(lc_PMPthDt), 'DISABLE', 'ENABLE') TO lcDispStat, lcRemStat
  lcNewStat  = 'ENABLE'
  SELECT (lc_PMPthDt)
  GO TOP
  lcNotfyMod = IIF(EMPTY(laUsers[1]), '', 'NOMODIFY')
ENDIF    

lcCurrCtg  = SPACE(3)
STORE cOprt_Ctg TO lcOprt_Ctg, lcOldCtg
lcCtg_Dsc  = LOOKUP(PMCTGHD.cCtg_Dsc, cOprt_Ctg,;
                   PMCTGHD.cOprt_Ctg,'PMCTGHD')

STORE cOprt_ID TO lcOprt_ID, lcOldOprt 
lcNotify    = IIF(!EMPTY(laUsers[1]), IIF(!EMPTY(ALLTRIM(mNotify)),;
                  STRTRAN(ALLTRIM(mNotify), '|',', ') + '.', ''),;
                  ALLTRIM(mNotify))
lcPredec    = lfGetPred()
cbShw2Cust  = lShw2Cust
m.cCal_Id   = cCal_ID
m.nEst_dur  = nEst_dur
m.cOprt_Dsc = cOprt_Dsc

IF EOF()
  STORE laUpdtMthd[1] TO lcUpdtMthd, puUpdtMthd
ELSE
  STORE cUpdtMthd TO lcUpdtMthd, puUpdtMthd
ENDIF  

lcNoteStat  = IIF(EOF(), 'DISABLE', 'ENABLE')  
lcNotfyStat = IIF(EOF() .OR. EMPTY(laUsers[1]), 'DISABLE', 'ENABLE')  
lcPredStat  = IIF(ALEN(laOperaton) > 1 , 'ENABLE', 'DISABLE')

ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTtl)
PUSH KEY
ON KEY LABEL ESC T=0
DO (gcScrDir +"MFPATHA.SPX")
ON KEY LABEL ESC DO  lpEscScr
POP KEY

=gfClearKey()

*!*************************************************************
*! Name      : lfActBrows
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Browse activation function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfActBrows()
*!*************************************************************
FUNCTION lfActBrows

lcKeyFld  = IIF(laScrMode[2], [KEY ']+ m.cPath_ID + ['], '')
lnBrRecNo = RECNO()
BROWSE FIELDS cMarker   = IIF(RECNO() = lnBrRecNo, '', ' ');
                        : H = ' '     :R:1:W = .F.,;
              cOprt_Ctg : H = 'Ctg.'  :R:6,;
              cOprt_ID  : H = 'Op.ID':R:8,;                                                                          
              cOprt_Dsc : H = 'Task Name' :R :&lnOprWdth,; 
              cOprt_Res : H = 'Responsible' :R:20,;                            
              nEst_Dur  : H = 'Dur  ' :R:3:P='999';
         WINDOW 'MFPATHA0' ;
         IN WINDOW 'MFPATHA' ;
         WHEN lfwBrwWhen(ALIAS(), !laScrMode[2]);
         NOMENU;
         NOAPPEND;
         NODELETE;
         NOEDIT;
         NOWAIT;
         SAVE;
         NOCLEAR;
         TITLE lcBrTtl &lcKeyFld

*!*************************************************************
*! Name      : lfwBrwWhen
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : WHEN function of the browse command
*!*************************************************************
*! Calls              : lfRefresh()
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  BROWSE ..  WHEN lfwBrwWhen()
*!*************************************************************
FUNCTION lfwBrwWhen
PARAMETERS lcPthDtFle, llEnableAl

SELECT (lcPthDtFle)
SET ORDER TO TAG PMPTHDTS

IF EMPTY(cOprt_Ctg)
  STORE ' ' TO lcCtg_Dsc, lcNotify, lcPredec 
ELSE
  lcOprt_Ctg = cOprt_Ctg
  lcCtg_Dsc  = LOOKUP(PMCTGHD.cCtg_Dsc, cOprt_Ctg,;
                      PMCTGHD.cOprt_Ctg,'PMCTGHD')
  
  lcOprt_ID  = cOprt_ID
  lcNotify   = IIF(!EMPTY(laUsers[1]), IIF(!EMPTY(ALLTRIM(mNotify)),;
                    STRTRAN(ALLTRIM(mNotify), '|',', ') + '.', ''),;
                    ALLTRIM(mNotify))
  lcPredec   = lfGetPred()
  m.cCal_Id   = cCal_ID
  m.nEst_dur  = nEst_dur
  m.cOprt_Dsc = cOprt_Dsc
ENDIF
IF EOF()
  STORE laUpdtMthd[1] TO lcUpdtMthd, puUpdtMthd
ELSE
  STORE cUpdtMthd TO lcUpdtMthd, puUpdtMthd
ENDIF  
cbShw2Cust = lShw2Cust

PuUser     = IIF(EMPTY(coprt_res) AND EMPTY(cGroup_Id),'User',;
             IIF(EMPTY(coprt_res),'Group of users','User'))
lcoprt_res = IIF(EMPTY(coprt_res),cGroup_id,coprt_res)

IF !EMPTY(lcOprt_Ctg)
  SHOW GET ibOprt_Ctg  DISABLE
  SHOW GET lcOprt_Ctg  DISABLE
  SHOW GET m.cOprt_Dsc DISABLE
  SHOW GET lcOprt_Res  DISABLE
  SHOW GET puUser      DISABLE
ENDIF
IF !EMPTY(lcOprt_id)
  SHOW GET ibOprt_ID  DISABLE
  SHOW GET lcOprt_id  DISABLE
ENDIF  
IF llEnableAl
  SHOW GET ibCal_ID    ENABLE
  SHOW GET m.cCal_ID   ENABLE
  SHOW GET m.cOprt_Dsc ENABLE
  SHOW GET lcoprt_res  ENABLE
  SHOW GET PuUser      ENABLE
  SHOW GET ibUpdtMthd  ENABLE
  SHOW GET puUpdtMthd  ENABLE
  SHOW GET cbShw2Cust  ENABLE
  SHOW GET m.nest_dur  ENABLE
  SHOW GET pbNew       ENABLE 
  SHOW GET pbRemove    ENABLE 
  SHOW GET pbNotes     ENABLE 
  SHOW GET pbSort      ENABLE 
  IF ALEN(laOperaton) > 1
    SHOW GET pbPredcsrs ENABLE
    SHOW GET lcPredec    ENABLE 
  ELSE
    SHOW GET pbPredcsrs DISABLE
    SHOW GET lcPredec    DISABLE
  ENDIF
  IF EMPTY(laUsers[1])
    SHOW GET pbNotify  DISABLE
    SHOW GET lcNotify   ENABLE
  ELSE
    SHOW GET pbNotify   ENABLE
    SHOW GET lcNotify   ENABLE 
  ENDIF  
ELSE
  SHOW GET ibCal_ID   DISABLE  
  SHOW GET m.cCal_ID    DISABLE  
  SHOW GET m.cOprt_Dsc  DISABLE  
  SHOW GET lcoprt_res DISABLE  
  SHOW GET PuUser     DISABLE  
  SHOW GET ibUpdtMthd DISABLE  
  SHOW GET puUpdtMthd DISABLE  
  SHOW GET cbShw2Cust DISABLE  
  SHOW GET m.nest_dur   DISABLE  
  SHOW GET pbRemove   DISABLE
  SHOW GET pbSort     DISABLE 
  IF laScrMode[3] OR laScrMode[4]
    SHOW GET pbNew       ENABLE 
  ENDIF  
  IF EOF()
    SHOW GET pbNotes     DISABLE
    SHOW GET pbNotify    DISABLE
    SHOW GET lcNotify    DISABLE
    SHOW GET pbPredcsrs DISABLE
    SHOW GET lcPredec    DISABLE
  ELSE
    SHOW GET pbNotes     ENABLE 
    IF EMPTY(laUsers[1])
      SHOW GET pbNotify    DISABLE
      SHOW GET lcNotify    DISABLE
    ELSE
      SHOW GET pbNotify    ENABLE
      SHOW GET lcNotify    ENABLE   
    ENDIF  
    
    IF ALEN(laOperaton) > 1
      SHOW GET pbPredcsrs ENABLE
      SHOW GET lcPredec    ENABLE 
    ELSE
      SHOW GET pbPredcsrs DISABLE
      SHOW GET lcPredec    DISABLE
    ENDIF
  ENDIF   
ENDIF


=lfRefresh()
lnBrRecNo = RECNO()
SHOW WINDOW (lcBrTtl) REFRESH SAME

*!*************************************************************
*! Name      : lfvNew
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Valid function for pudh button < New > (pbNew)
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvNew()
*!*************************************************************
FUNCTION lfvNew

lnCurAlias = SELECT(0)
SELECT (lc_PMPthDt)

STORE laUpdtMthd[1] TO lcUpdtMthd, puUpdtMthd
ACTIVATE WINDOW 'MFPATHA1'
*-- Save the current operation ID
lcOldOprt  = cOprt_ID
*-- Save tthe old category as well
lcOldCtg   = cOprt_Ctg

GO BOTTOM
lcOprt_id  = SPACE(5)
=lfwBrwWhen(lc_PMPthDt)

*-- Enable category fields, disable operation ID
lcOprt_Ctg = lcCurrCtg  
SHOW GET lcOprt_Ctg  ENABLE
SHOW GET ibOprt_Ctg  ENABLE
_CUROBJ    = OBJNUM(lcOprt_Ctg)
SELECT (lnCurAlias)


*!*************************************************************
*! Name      : lfvRemove
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
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
PRIVATE lnCurAlias, llMayDelte

lnCurAlias = SELECT(0)
SET ORDER TO TAG PMPTHRLP IN (lc_PMPthRl) 
*-- If deletion of the line is confirmed, continue
IF SEEK(lcOprt_Ctg + lcOprt_ID, lc_PMPthRl)
  llMayDelte = gfModalgen("TRM38234B38006","DIALOG",'Remove') = 1
ELSE
  llMayDelte = gfModalgen("TRM38208B38006","DIALOG") = 1                       
ENDIF                     
                         
IF llMayDelte
  SELECT (lc_PMPthRl)
  SCAN REST WHILE cPrd_Ctg + cPrd_ID = lcOprt_Ctg + lcOprt_ID
    REPLACE cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1)
    DELETE
  ENDSCAN
  SET ORDER TO TAG PMPTHRL
  SCAN FOR cOprt_Ctg + cOprt_ID = lcOprt_Ctg + lcOprt_ID
    REPLACE cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1)
    DELETE
  ENDSCAN
  
  *E302251,1 AMH Delete Notify Setting Records [Start]
  SELECT (lcPMPTHNTF)
  IF SEEK(lcPath_ID+lcOprt_Ctg+lcoprt_id)
    SCAN REST WHILE cPath_ID+COprt_Ctg+COprt_ID+CUser_ID = lcPath_ID+lcOprt_Ctg+lcoprt_id
      REPLACE cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1)
      DELETE
    ENDSCAN
  ENDIF
  *E302251,1 AMH [End]
  
  SELECT (lc_PMPthDt)
  *-- Adjust status of the line to 'D' for old entris,
  *-- 'S' for a new entry, then delete the record.
  *-- Save the removed category code
  lcRemCatg = cOprt_Ctg
  lnNOfLines = lnNOfLines - 1
  REPLACE cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1)
  DELETE
  IF !EOF()
    SKIP
  ENDIF
  IF EOF() .AND. lnNOfLines > 0
    SKIP -1
  ENDIF  
  *-- Remove the element from the operations array.
  lnElement = ASCAN(laOperaton, lcOprt_Ctg + '\' + lcOprt_ID)
  IF lnElement > 0
    IF ADEL(laOperaton, lnElement) > 0
      IF ALEN(laOperaton) > 1
        DIMENSION laOperaton[ALEN(laOperaton)-1]
      ELSE
        DIMENSION laOperaton[1]
        laOperaton = ''
      ENDIF  
    ENDIF
  ENDIF
    
  *-- If the category is not found anymore in the temporary
  *-- file, remove it from the categories array.
  IF lnNOfLines > 0
    lnRecNo = RECNO()
    SET ORDER TO TAG PMPTHDT
    IF !SEEK(lcRemCatg) 
       lnElement = ASCAN(laCategory,  lcRemCatg) 
      IF lnElement > 0 .AND. ADEL(laCategory, lnElement) > 0 
        IF ALEN(laCategory) > 1
          DIMENSION laCategory[ALEN(laCategory) -1]
        ELSE
           DIMENSION laCategory[1]
          laCategory = ''
        ENDIF
      ENDIF
    ENDIF  
    SET ORDER TO TAG PMPTHDTS    
    IF BETWEEN(lnRecNo, 1, RECCOUNT())
      GO lnRecNo
    ENDIF  
  ELSE
    lcOprt_Ctg = SPACE(3)
    lcOprt_ID  = SPACE(5)
    DIMENSION laCategory[1]
    laCategory = ''
  ENDIF  
  IF ALEN(laOperaton) < 2
    SHOW GET pbPredcsrs DISABLE
  ENDIF 
      
  glUpdated   = .T.
  =lfwBrwWhen(lc_PMPthDt, !EOF())
ENDIF
SET ORDER TO TAG PMPTHRL IN (lc_PMPthRl) 
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfReadAct
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : READ Activate function of MFPATHA.SCX
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

*E302251,1 AMH Activate Nofity Setting window [Start]
*ON KEY LABEL ESC T=0
ON KEY LABEL ALT+H ACTIVATE WINDOW (lcBrUser)
ON KEY LABEL ESC DO lfEsc
*E302251,1 AMH [End]

ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTtl)

*!*************************************************************
*! Name      : lfReadDect
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : READ Deactivate function of MFPATHA.SCX
*!*************************************************************
*! Calls     : lfTrapKey
*!*************************************************************
*! Returns            :  .f.
*!*************************************************************
*! Example            :  =()
*!*************************************************************
FUNCTION lfReadDect
*-- Key traps for the browse
IF WONTOP() = lcBrTtl
  ON KEY LABEL TAB        DO lpTab     WITH 'MFPATHA1', OBJNUM(lcOprt_id)
  ON KEY LABEL BACKTAB    DO lpTab     WITH 'MFPATHA1', OBJNUM(pbClose)
  ON KEY LABEL CTRL+ENTER llDummy = lfTrapKey('MFPATHA1', OBJNUM(pbClose))
  ON KEY LABEL ESC        llDummy = lfTrapKey('MFPATHA1', OBJNUM(pbClose))
  ON KEY LABEL CTRL+Q     lnDummy = 1
  ON KEY LABEL CTRL+W     lnDummy = 1
  ON KEY LABEL CTRL+HOME  GO TOP
  ON KEY LABEL CTRL+END   GO BOTTOM
  IF !laScrMode[2]
    ON KEY LABEL ALT+W    llDummy = lfTrapKey('MFPATHA1', OBJNUM(pbNew))
    ON KEY LABEL ALT+V    llDummy = !EOF(lc_PMPthDt) .AND. lfvRemove()
    ON KEY LABEL ALT+S    llDummy = !EOF(lc_PMPthDt) .AND. lfvShw2Cst()
    ON KEY LABEL ALT+T    llDummy = !EOF(lc_PMPthDt) .AND. lfvSort()
  ENDIF  
  ON KEY LABEL ALT+F      llDummy = !EOF(lc_PMPthDt) .AND. !EMPTY(laUsers[1]) .AND. ;
                                 lfTrapKey('MFPATHA1', OBJNUM(pbNotify))
  ON KEY LABEL ALT+P      llDummy = ALEN(laOperaton) > 1 .AND. ;
                                 lfTrapKey('MFPATHA1', OBJNUM(pbPredcsrs))
  ON KEY LABEL ALT+N      llDummy = !EOF(lc_PMPthDt) .AND. ;
                                 lfTrapKey('MFPATHA1', OBJNUM(pbNotes))
  ON KEY LABEL ALT+C      llDummy = lfTrapKey('MFPATHA1', OBJNUM(pbClose))
ENDIF  
RETURN .F.

*!*************************************************************
*! Name      : lfTrapKey
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
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
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
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
*! Name      : lfvOprtCtg
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Validates category code field
*!*************************************************************
*! Calls              :  lfVldKey()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvOprtCtg()
*!*************************************************************
FUNCTION lfvOprtCtg
PRIVATE lnCurAlias 
IF MDOWN() .AND. WONTOP() <> lcBrTtl
  RETURN
ENDIF  
lcOprt_Ctg   = PADR(ALLTRIM(lcOprt_Ctg), 3)
lnCurAlias   = SELECT(0)
llBrowse     = llBrowse .OR. '?' $ lcOprt_ID
FOR lnI = 1 To 4
  IF laScrMode[lnI]
    lnOldSctM = lnI
    EXIT
  ENDIF
ENDFOR

*-- If the entry is left empty, 
IF !llBrowse .AND. EMPTY(lcOprt_Ctg)  
  *-- Display the previous status
   SET ORDER TO TAG PMPTHDT IN (lc_PMPthDt)
  =lfwBrwWhen(lc_PMPthDt, SEEK(lcOldCtg + lcOldOprt, lc_PMPthDt))
ELSE  
  *-- If the category has been entered before, proceed
  *-- directly to the next object.
  IF !llBrowse .AND. ASCAN(laCategory, lcOprt_Ctg) > 0 
    SHOW GET lcOprt_Ctg DISABLE
    SHOW GET ibOprt_Ctg DISABLE
    SHOW GET lcOprt_ID  ENABLE
    SHOW GET ibOprt_ID  ENABLE
    _CUROBJ = OBJNUM(lcOprt_ID)     
  ELSE
    *-- If the category is changed, validate
    *-- If the category is valid,
     lcBasWnd = gcBaseWind  
     IF lfVldKey('PMCTGHD', 'PMCTGHD', @lcCtgFlds, @lcCtgTtl,;
                'cOprt_Ctg', 'Category code ' + ALLTRIM(lcOprt_Ctg) + ;
                ' is not found in the categories of operations file.');
      .AND. ( laScrMode[2] .OR. lfExtPrg('A',lcOprt_Ctg) )
      lcBaseFile = 'PMPTHHD'  
      lcCurrCtg = lcOprt_Ctg
      lcStamp =  PMPTHHD.cAdd_User+IIF(EMPTY(PMPTHHD.dAdd_Date),'',DTOC(PMPTHHD.dAdd_Date))+;
                     PMPTHHD.cAdd_Time
      gcBaseWind = lcBasWnd 
      lcBaseWind = gcBaseWind
      ACTIVATE WINDOW MFPATHA
      *-- Get the category description
      lcCtg_Dsc = PMCTGHD.cCtg_Dsc
      =lfRefresh()

      SELECT (lc_PMPthDt)
      *-- If the category has never been used on that path
      *-- before, call automatic operations addition function.
     
      IF !EMPTY(lcOprt_Ctg)
        SHOW GET ibOprt_Ctg DISABLE
        SHOW GET lcOprt_Ctg DISABLE
      ENDIF

      IF ASCAN(laCategory, lcOprt_Ctg) > 0 .OR. !lfAddOprs()  
        SHOW GET lcOprt_ID ENABLE
        SHOW GET ibOprt_ID ENABLE
        _CUROBJ = OBJNUM(lcOprt_ID)    
      ELSE
        SHOW GET lcOprt_ID   DISABLE
        SHOW GET ibOprt_ID   DISABLE
        SHOW GET m.cOprt_Dsc DISABLE
        SHOW GET lcOprt_Res  DISABLE
        SHOW GET puUser      DISABLE

        *-- Position the pointer on the first record of the selected
        *-- category.
        SELECT (lc_PMPthDt) 
        lnCurTag = VAL(SYS(21))
        SET ORDER TO TAG PMPTHDT
        SEEK lcOprt_Ctg
        SET ORDER TO (lnCurTag)
        =lfwBrwWhen(lc_PMPthDt, .T.)
        _CUROBJ = OBJNUM(pbNew)
      ENDIF     
    *-- Else, if an invalid entry,
    ELSE
      lcOprt_Ctg = SPACE(3)
      _CUROBJ    = _CUROBJ
    ENDIF
    SET ORDER TO TAG PMCTGDT IN PMCTGDT   
  ENDIF  
ENDIF

STORE .F. To laScrMode
laScrMode[lnOldSctM] = .T.
SELECT (lnCurAlias) 


*!*************************************************************
*! Name      : lfvOprt_ID
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Validates a key field.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvOprt_ID()
*!*************************************************************
FUNCTION lfvOprt_ID
PRIVATE lnCurAlias 

IF MDOWN() .AND. WONTOP() <> lcBrTtl
  RETURN
ENDIF  

lcOprt_ID    = PADR(ALLTRIM(lcOprt_ID), 5)
lnCurAlias   = SELECT(0)
llBrowse     = llBrowse .OR. '?' $ lcOprt_ID
 
*-- If the entry is left empty, 
IF !llBrowse .AND. EMPTY(lcOprt_ID)  
  *-- Display the previous status
  SET ORDER TO TAG PMPTHDT IN (lc_PMPthDt)
  =lfwBrwWhen(lc_PMPthDt, SEEK(lcOldCtg + lcOldOprt, lc_PMPthDt))
  IF EOF(lc_PMPthDt)
    lcOprt_Ctg = SPACE(3)
    SHOW GET lcOprt_Ctg
  ENDIF  
ELSE  
  *-- If not browsing, seek for the current operation
  *-- in the path operations temp file.
  SET ORDER TO TAG PMPTHDT IN (lc_PMPthDt)
  IF !llBrowse .AND. SEEK(lcOprt_Ctg + lcOprt_ID, lc_PMPthDt)
    SET ORDER TO TAG PMPTHDTS IN (lc_PMPthDt)
    =lfwBrwWhen(lc_PMPthDt, .T.)
  ELSE
    *-- If a valid operation, whether found, or added,
    FOR lnI = 1 To 4
      IF laScrMode[lnI]
        lnOldSctM = lnI
        EXIT
      ENDIF
    ENDFOR
    lcBasWnd = gcBaseWind  
    IF lfVldKey('PMCTGDT', 'PMCTGDT', @lcOprFlds, ;
                ALLTRIM(lcCtg_Dsc) + ' ' + 'Tasks',;
                'cOprt_ID', ;
                'Category\Task code ' + ALLTRIM(lcOprt_Ctg) + ;
                '\' + ALLTRIM(lcOprt_ID) + ;
                ' is not found in the categories of tasks file.',;
                lcOprt_Ctg + lcOprt_ID, [lcOprt_Ctg]) ;
        .AND. (laScrMode[2] .OR. lfExtPrg('E', lcOprt_Ctg, lcOprt_ID))
      lcBaseFile = 'PMPTHHD'  
      lcStamp    =  PMPTHHD.cAdd_User+IIF(EMPTY(PMPTHHD.dAdd_Date),'',DTOC(PMPTHHD.dAdd_Date))+;
                     PMPTHHD.cAdd_Time
      gcBaseWind = lcBasWnd 
      lcBaseWind = gcBaseWind
      STORE .F. TO laScrMode  
      laScrMode[lnOldSctM] = .T.
      lcCurrCtg = lcOprt_Ctg   
      SET ORDER TO TAG PMCTGDT IN PMCTGDT 
      IF !SEEK(lcOprt_Ctg + lcOprt_ID, lc_PMPthDt)
        SELECT PMCTGDT
        IF SEEK(lcOprt_Ctg + lcOprt_ID)
          SCATTER MEMVAR MEMO

          lcNewSeqNm = lfGetSeqNm()
          INSERT INTO (lc_PMPthDt) FROM MEMVAR
          SELECT (lc_PMPthDt) 
          REPLACE cPath_ID  WITH lcPath_ID,;
                  cCtg_Seq  WITH lcNewSeqNm,;
                  cStatus   WITH 'A'      ,;
                  cAdd_User WITH gcUser_ID  ,;
                  dAdd_Date WITH ldCurDate,;
                  cAdd_Time WITH TIME()   
          *-- Collect all predecessors of the curren
          glUpdated = .T.
          lnNOfLines = lnNOfLines + 1
          IF !EMPTY(laOperaton[1])
            DECLARE laOperaton[lnNOfLines] 
          ENDIF  
          laOperaton[ALEN(laOperaton)] = lcOprt_Ctg + '\' + lcOprt_ID + ;
                                         ' ' +SUBSTR(m.cOprt_Dsc, 1, 18)
          IF ALEN(laOperaton) > 1
            SHOW GET pbPredcsrs ENABLE
          ENDIF 
   
          *-- Add the new category to the categories array
          IF ASCAN(laCategory, lcOprt_Ctg) = 0 
            IF !EMPTY(laCategory[1])
              DIMENSION laCategory[ALEN(laCategory) + 1]
            ENDIF  
            laCategory[ALEN(laCategory)] = lcOprt_Ctg
          ENDIF
          
          *-- Position the record pointer in PMCTGRL file on the
          *-- current operation so as to collect its predecessors,
          *-- if any exist.
          IF SEEK(lcOprt_Ctg + m.cOprt_ID, 'PMCTGRL') 
            SELECT PMCTGRL
            SCAN REST WHILE cOprt_Ctg + cOprt_ID = lcOprt_Ctg + m.cOprt_ID;
                    FOR ASCAN(laOperaton, cOprt_Ctg + '\' + cPrd_ID ) > 0
              SCATTER MEMVAR MEMO
              INSERT INTO (lc_PMPthRl) FROM MEMVAR
              SELECT (lc_PMPthRl)
              REPLACE cPath_ID  WITH lcPath_ID,;
                        cPrd_Ctg  WITH cOprt_Ctg,;
                        cAdd_User WITH gcUser_ID,;
                        dAdd_Date WITH ldCurDate,;
                        cAdd_Time WITH TIME(),;
                        cStatus WITH 'A' 
            ENDSCAN           
          ENDIF    && ENDIF SEEK(lcOprt_Ctg + m.cOprt_ID, 'PMCTGRL') 
          
          *E302251,1 AMH Add Nofity Setting Records [Start]
          IF SEEK(lcOprt_Ctg + m.cOprt_ID, 'PMPCTGNT')
            SELECT PMPCTGNT
            SCAN REST WHILE cOprt_Ctg + cOprt_ID = lcOprt_Ctg + m.cOprt_ID
              SCATTER MEMVAR
              m.cPaht_ID = lcPath_ID
              m.cStatus  = 'A'
              INSERT INTO (lcPMPTHNTF) FROM MEMVAR
            ENDSCAN
          ENDIF
          *E302251,1 AMH [End]
          
        ENDIF  
      ENDIF  
      SELECT (lc_PMPthDt)       
      SET ORDER TO TAG PMPTHDTS
      STORE .F. TO laScrMode
      laScrMode[lnOldSctM] = .T.
      =lfwBrwWhen(lc_PMPthDt, .T.)      
    *-- Else, if an invalid entry,
    ELSE
      STORE .F. TO laScrMode
      laScrMode[lnOldSctM] = .T.
      SET ORDER TO TAG PMPTHDTS IN (lc_PMPthDt)       
      lcOprt_ID  = SPACE(5)
      _CUROBJ    = _CUROBJ
    ENDIF  
  ENDIF   && ENDIF !llBrowse .AND. SEEK(lcOprt_Ctg + lcOprt_ID, lc_PMPthDt)
ENDIF
SELECT (lnCurAlias) 

*!*************************************************************
*! Name      : lfvShw2Cst
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Validates Show to customer check box.(cbShw2Cust)
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvShw2Cst()
*!*************************************************************
FUNCTION lfvShw2Cst
REPLACE &lc_PMPthDt..lShw2Cust WITH !&lc_PMPthDt..lShw2Cust,;
        &lc_PMPthDt..cStatus   WITH SUBSTR('MMA', AT(&lc_PMPthDt..cStatus, 'SMA'), 1),;
        &lc_PMPthDt..cAdd_User WITH gcUser_ID,;
        &lc_PMPthDt..dAdd_Date WITH ldCurDate,;
        &lc_PMPthDt..cAdd_Time WITH TIME()
cbShw2Cust = &lc_PMPthDt..lShw2Cust
SHOW GET cbShw2Cust
glUpdated = .T.

*!*************************************************************
*! Name      : lfvDetails
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Validates detail fields
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvDetails()
*!*************************************************************
FUNCTION lfvDetails
PRIVATE lcCurObj

lcCurObj = SYS(18)

lcSetExact = SET('EXACT')
SET EXACT ON

*AMH
lcOldBrFld = lcBrFields
*AMH

IF lcCurObj = 'LCOPRT_RES'
  DO CASE
    CASE PuUser = 'User'
      SELECT SYUUSER
      lcFile  = 'SYUUSER'
      lcBrFields = [cUser_Id : H = 'User ID', cUsr_Name : H = 'User Name']  
      lcFldName  = 'cUser_Id'
      lcField    = 'COPRT_RES'
    CASE PuUser = 'Group of users'
      SELECT SYUGROUP
      lcFile  = 'SYUGROUP'
      lcBrFields = [cGroup_Id : H = 'Group ID', cGroup_Nam : H = 'Group Name'] 
      lcFldName  = 'cGroup_Id'
      lcField    = 'cGroup_Id'
  ENDCASE  
  IF !EMPTY(LCOPRT_RES) AND !SEEK(ALLTRIM(LCOPRT_RES),lcFile)
    DIMENSION laTemp[1]
    IF ARIABROW('','Responsible User',gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,lcFldName,'laTemp')
      LCOPRT_RES = laTemp[1]
    ELSE
      LCOPRT_RES = SPACE(10)
    ENDIF  
  ENDIF  
ELSE
  lcField = lcCurObj 
ENDIF

*AMH
lcBrFields = lcOldBrFld
*AMH

SET EXACT &lcSetExact

*E301869,2 AMH Fix bug of remove the user if move by tab through a task [Start]
*IF LCOPRT_RES <> lcOldVal
IF EVALUATE(lcCurObj) <> lcOldVal
*E301869,2 AMH [End]

  lnCurAlias = SELECT(0)
  SELECT (lc_PMPthDt)
  REPLACE COPRT_RES  WITH ' ',;
          CGROUP_ID  WITH ' ',;
          &lcField  WITH IIF(lcCurObj = 'LCOPRT_RES',ALLTRIM(LCOPRT_RES),EVALUATE('m.'+lcCurObj)),;
          cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1),;
          cAdd_User WITH gcUser_ID,;
          dAdd_Date WITH ldCurDate,;
          cAdd_Time WITH TIME()
  glUpdated = .T.
  SHOW WINDOW (lcBrTtl) REFRESH SAME  
  SELECT (lnCurAlias)
ENDIF

*!*************************************************************
*! Name      : lfvDuraton
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Validates duration fields
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvDuraton()
*!*************************************************************
FUNCTION lfvDuraton

IF lcOldVal <> m.nEst_Dur
  lnCurAlias = SELECT(0)
  SELECT (lc_PMPthDt)
  IF m.nEst_Dur < 0 .AND. gfModalgen("TRM38215B00000","DIALOG")  = 1 
    REPLACE nEst_Dur  WITH lcOldVal
  ELSE
    REPLACE nEst_Dur  WITH m.nEst_Dur,;    
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
*! Name      : lfvPredcsr
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Valid function for push button < Predecessors >
*!             (pbPredcsrs)
*!*************************************************************
*! Calls              : gfMover
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvPredecesrs()
*!*************************************************************
FUNCTION lfvPredcsr
PRIVATE laPredOprs, lcOldPred, lnCurAlias

lnCurAlias = SELECT(0)
lcSetProc = SET('PROCEDURE')
SET PROCEDURE TO

=ACOPY(laOperaton, laPredOprs)

lnElement = ASCAN(laPredOprs, lcOprt_Ctg + '\' + lcOprt_ID)

IF lnElement > 0
  IF ADEL(laPredOprs, lnElement) > 0
    IF ALEN(laPredOprs) > 1
      DIMENSION laPredOprs[ALEN(laPredOprs)-1]
    ELSE
      DIMENSION laPredOprs[1]
      laPredOprs = ''
    ENDIF  
  ENDIF
ENDIF  
 
IF laScrMode[2]
  SELECT cPrd_Ctg + '\' + cPrd_ID +  ' ' + SUBSTR(cOprt_Dsc, 1, 18);
    FROM PMPTHRL, PMPTHDT ;
   WHERE PMPTHRL.cPath_ID +  PMPTHRL.cOprt_Ctg +  PMPTHRL.cOprt_ID = ;
         lcPath_ID + lcOprt_Ctg + lcOprt_ID;
   .AND. PMPTHRL.cPath_ID +  PMPTHRL.cPrd_Ctg  +  PMPTHRL.cPrd_ID = ;   
         PMPTHDT.cPath_ID +  PMPTHDT.cOprt_Ctg + PMPTHDT.cOprt_Id   ;
    INTO ARRAY laPredecsr     

  IF _TALLY = 0
    DIMENSION laPredecsr[1]
    laPredecsr = ''
  ENDIF
  =gfMover(@laPredOprs,@laPredecsr,;
           ALLTRIM(lcOprt_Ctg) + '\' + ;
           ALLTRIM(lcOprt_ID) + ' ' + ALLTRIM(PMPTHDT.coprt_dsc) +;
           ' Predecessors')
ELSE
  SELECT cPrd_Ctg + '\' + cPrd_ID +  ' ' + SUBSTR(cOprt_Dsc, 1, 18);
    FROM (lc_PMPthRl), (lc_PMPthDt) ;
   WHERE &lc_PMPthRl..cOprt_Ctg + &lc_PMPthRl..cOprt_ID = ;
         lcOprt_Ctg + lcOprt_ID     ;
   .AND. &lc_PMPthRl..cPrd_Ctg  +  &lc_PMPthRl..cPrd_ID = ;   
         &lc_PMPthDt..cOprt_Ctg + &lc_PMPthDt..cOprt_Id   ;
    INTO ARRAY laPredecsr     

  IF _TALLY = 0
    DIMENSION laPredecsr[1]
    laPredecsr = ''
  ENDIF
  *-- Create an array to hold all the elements that succeed the
  *-- current operation in all avaialble paths.
  DIMENSION laPathElems[1]
  laPathElems = ''
  *-- Fill the array laPathElems[]
  SELECT (lc_PMPthDt)
  SET RELATION OFF INTO (lc_PMPthRl)
  SELECT (lc_PMPthRl)
  SET ORDER TO TAG PMPTHRLP 
  *-- If the current operation is found as a predecessor to
  *-- others, get all the operations on all available paths
  *-- into an array.
  lnCount = 0  
  IF SEEK(lcOprt_Ctg + lcOprt_ID)
    SCAN REST WHILE cPrd_Ctg + cPrd_ID = lcOprt_Ctg + lcOprt_ID
      lnCount = lnCount + 1  
      DIMENSION laPathElems[lnCount]
      laPathElems[lnCount] = cOprt_Ctg +  cOprt_ID
    ENDSCAN
 
    *-- Collect the rest of the operations in the array
    lnNxtCount = 0
    DO WHILE lnNxtCount < ALEN(laPathElems)
      lnNxtCount = lnNxtCount + 1 
      lcNxtOprt  = laPathElems[lnNxtCount]
      IF SEEK(lcNxtOprt)
        SCAN REST WHILE cPrd_Ctg + cPrd_ID = lcNxtOprt
          DIMENSION laPathElems[ALEN(laPathElems) + 1]
          laPathElems[ALEN(laPathElems)] = cOprt_Ctg + cOprt_ID
        ENDSCAN
      ENDIF     &&ENDIF SEEK(lcNxtOprt)
    ENDDO    &&ENDDO WHILE lnNxtCount < ALEN(laPathElems)
  ENDIF     &&ENDIF SEEK(lcOprt_Ctg + lcOprt_ID)  
  lcOldPred = lcPredec 
  *-- Adjust validation string
  
  lcVldPrd = 'lfVldPrd(SUBSTR(IIF(lsSource = 0,laSource[1],laSource[lsSource]), 1, 3),;
                       SUBSTR(IIF(lsSource = 0,laSource[1],laSource[lsSource]), 5, 5),;
                      _CUROBJ = OBJNUM(pbAll))'  

  =gfMover(@laPredOprs,@laPredecsr,;
           'Select Predecessors for '+ ALLTRIM(lcOprt_Ctg) + '\' + ;
           ALLTRIM(lcOprt_ID) + ' ' + ALLTRIM(&lc_PMPthDt..coprt_dsc),;
           .T.,@lcVldPrd,.T.,.F.)          
  SELECT (lc_PMPthRl)
  SET ORDER TO TAG PMPTHRL
  SELECT (lc_PMPthDt)
  SET RELATION TO cOprt_Ctg + cOprt_ID INTO (lc_PMPthRl)
  SELECT (lc_PMPthRl)
  IF !EMPTY(laPredecsr[1]) 
    lnDelElems = ALEN(laPredecsr)
    *-- Unformat the predecessors array elements
    FOR lnCount = 1 TO lnDelElems
      laPredecsr[lnCount] = SUBSTR(laPredecsr[lnCount], 1, 3) + ;
                             SUBSTR(laPredecsr[lnCount], 5, 5) 
    ENDFOR  
    
    IF SEEK(lcOprt_Ctg + lcOprt_ID)
      SCAN REST WHILE cOprt_Ctg + cOprt_ID = lcOprt_Ctg + lcOprt_ID      
        lnElement  = ASCAN(laPredecsr, cPrd_Ctg + cPrd_ID)
        IF lnElement > 0 
          =ADEL(laPredecsr, lnElement)
          lnDelElems = lnDelElems - 1
        ELSE
          REPLACE cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1)
          DELETE
        ENDIF
      ENDSCAN
      FOR lnCount = 1 TO lnDelElems
        INSERT INTO (lc_PMPthRl);
                   (cPath_ID , cOprt_Ctg, cOprt_ID , ;
                    cPrd_Ctg, cPrd_ID, ;
                    cAdd_User, dAdd_Date, cAdd_Time, cStatus);
             VALUES(lcPath_ID, lcOprt_Ctg, lcOprt_ID, ;
                    SUBSTR(laPredecsr[lnCount], 1, 3),SUBSTR(laPredecsr[lnCount], 4, 5),;
                    gcUser_ID  , ldCurDate, TIME()   , 'A')
      ENDFOR
    ELSE
      FOR lnCount = 1 TO ALEN(laPredecsr)
        INSERT INTO (lc_PMPthRl);
                   (cPath_ID , cOprt_Ctg, cOprt_ID , ;
                    cPrd_Ctg, cPrd_ID, ;
                    cAdd_User, dAdd_Date, cAdd_Time, cStatus);
             VALUES(lcPath_ID, lcOprt_Ctg, lcOprt_ID, ;
                    SUBSTR(laPredecsr[lnCount], 1, 3),SUBSTR(laPredecsr[lnCount], 4, 5),;
                    gcUser_ID  , ldCurDate, TIME()   , 'A')
      ENDFOR
    ENDIF
  ELSE
    IF SEEK(lcOprt_Ctg + lcOprt_ID)
      SCAN REST WHILE cOprt_Ctg + cOprt_ID = lcOprt_Ctg + lcOprt_ID      
        REPLACE cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1)
        DELETE
      ENDSCAN
    ENDIF  
  ENDIF  

  lcOldVal   = lcPredec
  lcPredec   = lfGetPred()
  IF lcOldVal <> lcPredec
    glUpdated  = .T.
    SHOW GET lcPredec
  ENDIF
ENDIF  
SET PROCEDURE TO &lcSetProc
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfGetPred
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Gets predecessors of an operation in a string.
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  A string of predecessors or  ''.
*!*************************************************************
*! Example            :  lfGetPred()
*!*************************************************************
FUNCTION lfGetPred
PRIVATE lcPrdStrng, lnCurAlias

lnCurAlias = SELECT(0)
lcPrdStrng = ''
IF laScrMode[2]
  SELECT PMPTHDT
  IF BETWEEN(RECNO(), 1, RECCOUNT())
    GO RECNO()
  ENDIF
  SELECT PMPTHRL
ELSE
  SELECT (lc_PMPthDt)
  IF BETWEEN(RECNO(), 1, RECCOUNT())
    GO RECNO()
  ENDIF
  SELECT (lc_PMPthRl)
ENDIF    

SCAN REST WHILE cpath_id+coprt_ctg+coprt_id = m.cpath_id+lcOprt_Ctg + lcOprt_ID
  lcPrdStrng = lcPrdStrng + ALLTRIM(cPrd_Ctg) + '\' + ALLTRIM(cPrd_ID) +', '
ENDSCAN

SELECT (lnCurAlias)
RETURN IIF(!EMPTY(lcPrdStrng),;
           SUBSTR(lcPrdStrng, 1, RAT(',', lcPrdStrng)-1) + '.', SPACE(40)) 
           
           
*!*************************************************************
*! Name      : lfvUpdMthd
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Update Method popup valid function.
*!*************************************************************
*! Calls              : gfActPop()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvUpdMthd()
*!*************************************************************
FUNCTION lfvUpdMthd

lcUpdtMthd = puUpdtMthd

REPLACE &lc_PMPthDt..cUpdtMthd WITH lcUpdtMthd,;
        &lc_PMPthDt..cStatus   WITH SUBSTR('MMA', AT(&lc_PMPthDt..cStatus, 'SMA'), 1),;
        &lc_PMPthDt..cAdd_User WITH gcUser_ID,;
        &lc_PMPthDt..dAdd_Date WITH ldCurDate,;
        &lc_PMPthDt..cAdd_Time WITH TIME()
glUpdated   = .T.

*!*************************************************************
*! Name      : lfvNotify
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Valid function for push button < Notify... >
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvNotify()
*!*************************************************************
FUNCTION lfvNotify
IF ! lcNotify == lcOldVal
  lnCurAlias = SELECT(0)
  SELECT (lc_PMPthDt)
  REPLACE mNotify WITH STRTRAN(ALLTRIM(lcNotify), ',','|'),;
          cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1),;
          cAdd_User WITH gcUser_ID,;
          dAdd_Date WITH ldCurDate,;
          cAdd_Time WITH TIME()
  SELECT (lnCurAlias)
  glUpdated   = .T.
ENDIF

*!*************************************************************
*! Name      : lfvNotfyBt
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Valid function for push button < Notify... >
*!*************************************************************
*! Calls              : gfMover
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvNotfyBt()
*!*************************************************************
FUNCTION lfvNotfyBt

lnCurAlias = SELECT(0)
lcSetProc = SET('PROCEDURE')
SET PROCEDURE TO

*E302251,1 AMH Calling the Notify Seting screen [Start]
*DIME laUsers[1]
*SELECT cUser_ID ;
*  FROM (gcSysHome + 'SYUUSER');
*  INTO ARRAY laUsers;
*  ORDER BY cUser_ID
*
*SELECT (lc_PMPthDt)
*=gfStr2Ar(ALLTRIM(mNotify), @laNotified, '|')
*IF laScrMode[2]
*  =gfMover(@laUsers,@laNotified,"Users to Notify",.F.)
*ELSE
*  =gfMover(@laUsers,@laNotified,"Select Users to Notify",.T.)
*  lcNotify   = ''
*  FOR lnCount = 1 TO ALEN(laNotified)
*    lcNotify = lcNotify + ALLTRIM(laNotified[lnCount]) + '|'
*  ENDFOR
*  IF !EMPTY(lcNotify)
*    lcNotify = SUBSTR(lcNotify, 1, RAT('|', lcNotify)-1) 
*  ENDIF
*  REPLACE mNotify WITH ALLTRIM(lcNotify),;
*          cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1),;
*          cAdd_User WITH gcUser_ID,;
*          dAdd_Date WITH ldCurDate,;
*          cAdd_Time WITH TIME()
*  lcNotify   = IIF(!EMPTY(lcNotify), STRTRAN(lcNotify, '|', ', ') + '.', '')
*  glUpdated  = .T.
*  SHOW GET lcNotify
*ENDIF

SELECT (lcPMPTHNTF)
SET KEY TO lcPath_ID+lcOprt_Ctg+lcoprt_id
LOCATE
lnBrRecNo = RECNO()
SCATTER MEMVAR

PUSH KEY
ON KEY LABEL ALT+H ACTIVATE WINDOW (lcBrUser)
ON KEY LABEL ESC DO lfEsc
DO (gcScrDir + 'SM\SMNTFSET.SPX')
POP KEY

IF !laScrMode[2]
  lcNotify   = ''
  SELECT (lcPMPTHNTF)
  SCAN FOR !EMPTY(cUser_ID)
    lcNotify = lcNotify + ALLTRIM(cUser_ID) + '|'
  ENDSCAN
  IF !EMPTY(lcNotify)
    lcNotify = SUBSTR(lcNotify, 1, RAT('|', lcNotify)-1) 
  ENDIF
  SELECT (lc_PMPthDt)
  REPLACE mNotify   WITH ALLTRIM(lcNotify),;
          cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1),;
          cAdd_User WITH gcUser_ID,;
          dAdd_Date WITH ldCurDate,;
          cAdd_Time WITH TIME()

  lcNotify   = IIF(!EMPTY(lcNotify), STRTRAN(lcNotify, '|', ', ') + '.', '')
  glUpdated  = .T.
  SHOW GET lcNotify
ENDIF

SELECT (lcPMPTHNTF)
SET KEY TO
*E302251,1 AMH [End]

SET PROCEDURE TO &lcSetProc
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvNotes
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Valid function for push button < Notes... >
*!*************************************************************
*! Calls              : gfZoom()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvNotes()
*!*************************************************************
FUNCTION lfvNotes
lnCurAlias = SELECT(0)
*-- If view mode, show notes from PMPTHDT
IF laScrMode[2]
  SELECT PMPTHDT
  =gfZoom('mOprt_Com')
ELSE
  SELECT (lc_PMPthDt)
  =gfZoom('mOprt_Com')
  REPLACE cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1),;
          cAdd_User WITH gcUser_ID,;
          dAdd_Date WITH ldCurDate,;
          cAdd_Time WITH TIME()
  glUpdated = .T.

ENDIF  
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfVldPrd
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Checks a predecessor whether it would create a 
*!             cyclic path or not.
*!*************************************************************
*! Called    : lfvMSource() in MAINPROC, from ARIAMOVR.SR
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  :  
*!*************************************************************
*! Returns            : .T. if the current operation may be 
*!                      selected as a predecessor, .f. otherwise.
*!*************************************************************
*! Example            :  IF lfVldPrd(..)....
*!*************************************************************
FUNCTION lfVldPrd
PARAMETERS lcArrCtg, lcArrOprt, llMoveAll

IF _CUROBJ = OBJNUM(lsTarget) .OR. (TYPE("lnMovmnts")="N" .AND. (lnMovmnts = 3 OR lnMovmnts = 4))
  RETURN 
ELSE
  SET PROCEDURE TO &lcSetProc
  lnRetVal = IIF(ASCAN(laPathElems, lcArrCtg + lcArrOprt) = 0, .T.,;
             IIF(gfModalgen("TRM38214B00000","DIALOG",ALLTRIM(lcArrCtg) + '\' + ALLTRIM(lcArrOprt)) = 1, .F., .T.))
  IF !EMPTY(laTarget)
    lcPrevOrd = ORDER(lc_PMPthRl)                               
    IF llMoveAll 
      SET ORDER TO PMPTHRL IN (lc_PMPthRl)
      IF lnRetVal AND (SEEK(lcOprt_ctg+lcOprt_ID,lc_PMPthRl) AND;
                       SEEK(&lc_PMPthRl..Cprd_Ctg+&lc_PMPthRl..Cprd_ID,lc_PMPthRl)) AND ;
                       (lcArrCtg = &lc_PMPthRl..Cprd_Ctg AND lcArrOprt = &lc_PMPthRl..Cprd_ID)
        =gfModalgen("TRM38241B00000","DIALOG",;
                     lcArrCtg+"|"+lcArrOprt+"|"+lcOprt_ctg+"|"+lcOprt_ID+"|"+ALLTRIM(m.cOprt_dsc))
        lnRetVal = .F.
      ELSE
        SET ORDER TO PMPTHRLP IN (lc_PMPthRl)
        IF lnRetVal AND SEEK(lcArrCtg+lcArrOprt,lc_PMPthRl) AND ASCAN(laTarget,&lc_PMPthRl..COprt_Ctg+'\'+&lc_PMPthRl..COprt_ID) <> 0)      
        =gfModalgen("TRM38241B00000","DIALOG",;
                     lcArrCtg+"|"+lcArrOprt+"|"+lcOprt_ctg+"|"+lcOprt_ID+"|"+ALLTRIM(m.cOprt_dsc))
          lnRetVal = .F.
        ELSE
          SET ORDER TO PMPTHRL IN (lc_PMPthRl)
          IF lnRetVal AND SEEK(lcArrCtg+lcArrOprt,lc_PMPthRl) AND ASCAN(laTarget,&lc_PMPthRl..CPrd_Ctg+'\'+&lc_PMPthRl..CPrd_ID) <> 0)
            =gfModalgen("TRM38242B00000","DIALOG",;
            lcOprt_ctg+"|"+lcOprt_ID+"|"+ALLTRIM(m.cOprt_dsc)+"|"+lcArrCtg+"|"+lcArrOprt)

            lnRetVal = .F.
          ENDIF
        ENDIF
      ENDIF 
    ELSE
      SET ORDER TO PMPTHRL IN (lc_PMPthRl)
      IF lnRetVal AND (SEEK(lcOprt_ctg+lcOprt_ID,lc_PMPthRl) AND;
                       SEEK(&lc_PMPthRl..Cprd_Ctg+&lc_PMPthRl..Cprd_ID,lc_PMPthRl)) AND ;
                       (lcArrCtg = &lc_PMPthRl..Cprd_Ctg AND lcArrOprt = &lc_PMPthRl..Cprd_ID)
        =gfModalgen("TRM38243B00000","DIALOG",;
        lcOprt_ctg+"|"+lcOprt_ID+"|"+ALLTRIM(m.cOprt_dsc))
        lnRetVal = .F.
      ELSE
        SET ORDER TO PMPTHRLP IN (lc_PMPthRl)
        IF lnRetVal AND SEEK(lcArrCtg+lcArrOprt,lc_PMPthRl) AND;
                        ASCAN(laTarget,&lc_PMPthRl..COprt_Ctg+'\'+&lc_PMPthRl..COprt_ID) <> 0)      
          =gfModalgen("TRM38243B00000","DIALOG",;
          lcOprt_ctg+"|"+lcOprt_ID+"|"+ALLTRIM(m.cOprt_dsc))
          lnRetVal = .F.
        ELSE
          SET ORDER TO PMPTHRL IN (lc_PMPthRl)
          IF lnRetVal AND SEEK(lcArrCtg+lcArrOprt,lc_PMPthRl) AND;
                      ASCAN(laTarget,&lc_PMPthRl..CPrd_Ctg+'\'+&lc_PMPthRl..CPrd_ID) <> 0)
            =gfModalgen("TRM38244B00000","DIALOG",;
             lcOprt_ctg+"|"+lcOprt_ID+"|"+ALLTRIM(m.cOprt_dsc))
             lnRetVal = .F.
          ENDIF
        ENDIF
      ENDIF            
    ENDIF
    SET ORDER TO &lcPrevOrd IN (lc_PMPthRl)             
  ENDIF
  SET PROCEDURE TO 
  RETURN lnRetVal
ENDIF  

*!*************************************************************
*! Name      : lfAddOprs
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Automatic addition of operations through a mover.
*!*************************************************************
*! Calls     : ARIAMOVR.PRG
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  .T. if operations are added to the file,
*!                       .F. otherwise.
*!*************************************************************
*! Example            :  =lfAddOprs()
*!*************************************************************
FUNCTION lfAddOprs
PRIVATE laAvailOprs, laSelOprs, llAddOprs
DECLARE laAvailOprs[1], laSelOprs[1]

lcSetProc = SET('PROCEDURE')
SET PROCEDURE TO

*-- Initialize variable to .F. (i.e. no operations are added)
llAddOprs = .F.
SET ORDER TO TAG PMCTGDT IN PMCTGDT

*-- If the category is not found, this means that it has just
*-- been deleted from the categories file from another station.
IF SEEK(lcOprt_Ctg, 'PMCTGDT')
  *-- Collect all the operations of the current category
  *-- from PMPTHDT into an array.
  SELECT cOprt_ID + ' ' + cOprt_Dsc ;
   FROM PMCTGDT;
   WHERE COPRT_CTG+COPRT_ID = lcOprt_Ctg;       
   ORDER BY cOprt_Seq; 
  INTO ARRAY laAvailOprs

  *-- Initialize selected operations button to ''
  laSelOprs = '' 

  *-- Call the mover screen with both array names, no valid function
  =gfMover(@laAvailOprs, @laSelOprs,"Select From Available tasks", .T.)
  *-- If one or more operations are selected, collect their data into
  *-- the temporary file.
  IF !EMPTY(laSelOprs[1])
    *-- Go through all the selected operations array rows.
    glUpdated   = .T.
    lcNewCtgSeq = lfGetSeqNm()
    FOR lnCount = 1 TO ALEN(laSelOprs,1)
      *-- Position the record pointer in PMCTGDT file on the
      *-- current operation so as to collect its data.
      IF SEEK(lcOprt_Ctg + SUBSTR(laSelOprs[lnCount], 1, 5), 'PMCTGDT') 
        SELECT PMCTGDT 
        SCATTER MEMVAR MEMO
        INSERT INTO (lc_PMPthDt) FROM MEMVAR
        SELECT (lc_PMPthDt)
        REPLACE cPath_ID  WITH lcPath_ID,;
                cCtg_Seq  WITH lcNewCtgSeq,;
                cAdd_User WITH gcUser_ID,;
                dAdd_Date WITH ldCurDate,;
                cAdd_Time WITH TIME(),;
                cStatus WITH 'A'

        *-- Add the new operations to the operations array
        lnNOfLines = lnNOfLines + 1
        IF !EMPTY(laOperaton[1])
          DECLARE laOperaton[lnNOfLines] 
        ENDIF  
        laOperaton[lnNOfLines] = lcOprt_Ctg + '\' + m.cOprt_ID + ;
                                     ' ' + SUBSTR(m.cOprt_Dsc, 1, 18)
        *-- Position the record pointer in PMCTGRL file on the
        *-- current operation so as to collect its predecessors,
        *-- if any exist.
        IF SEEK(lcOprt_Ctg + m.cOprt_ID, 'PMCTGRL') 
          SELECT PMCTGRL 
          SCAN REST WHILE cOprt_Ctg + cOprt_ID = lcOprt_Ctg + m.cOprt_ID;
              FOR ASCAN(laOperaton, cOprt_Ctg + '\' + cPrd_ID ) > 0
            SCATTER MEMVAR MEMO
            INSERT INTO (lc_PMPthRl) FROM MEMVAR
            SELECT (lc_PMPthRl)
            REPLACE cPath_ID  WITH lcPath_ID,;
                    cPrd_Ctg  WITH cOprt_Ctg,;
                    cAdd_User WITH gcUser_ID,;
                    dAdd_Date WITH ldCurDate,;
                    cAdd_Time WITH TIME(),;
                    cStatus WITH 'A' 
          ENDSCAN           
        ENDIF    && ENDIF SEEK(lcOprt_Ctg + m.cOprt_ID, 'PMCTGRL') 
        
        *E302251,1 AMH Add Nofity Setting Records [Start]
        IF SEEK(lcOprt_Ctg + m.cOprt_ID, 'PMPCTGNT')
          SELECT PMPCTGNT
          SCAN REST WHILE cOprt_Ctg + cOprt_ID = lcOprt_Ctg + m.cOprt_ID
            SCATTER MEMVAR
            m.cPaht_ID = lcPath_ID
            m.cStatus  = 'A'
            INSERT INTO (lcPMPTHNTF) FROM MEMVAR
          ENDSCAN
        ENDIF
        *E302251,1 AMH [End]
        
      ENDIF    && ENDIF SEEK(lcOprt_Ctg + SUBSTR(laSelOprs[lnCount]), 1, 5), 'PMCTGDT')         
    ENDFOR   && ENDFOR lnCount = 1 TO ALEN(laSelOprs,1)
    IF ALEN(laOperaton) > 1
      SHOW GET pbPredcsrs ENABLE
    ENDIF 
    llAddOprs = .T.

    *-- Add the new category to the categories array
    IF !EMPTY(laCategory[1])
      DIMENSION laCategory[ALEN(laCategory) + 1]
    ENDIF  
    laCategory[ALEN(laCategory)] = lcOprt_Ctg
  ENDIF    && ENDIF !EMTPY(laSelOprs[1])
  SELECT (lc_PMPthDt)
ELSE
  =gfModalgen("INM38218B00000","DIALOG",ALLTRIM(lcOprt_Ctg))          
ENDIF
SET PROCEDURE TO &lcSetProc
RETURN llAddOprs

*!*************************************************************
*! Name      : lfvSort
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Valid function for push button < Sort... >
*!*************************************************************
*! Calls     : MFPSORT.PRG
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvSort()
*!*************************************************************

FUNCTION lfvSort

PRIVATE puCatgs, puOprts, laCatgLine, laOperLine, ;
        lsCatgLine, lsOperLine, lcCategory, lcSeqCursr,;
        lcOldCatg, llSaveSort, llChngSort, lnCurAlias

DEFINE POPUP puCatgs MOVER SCROLL MARGIN MARK ''
DEFINE POPUP puOprts MOVER SCROLL MARGIN MARK ''

*-- Declare categories and operations arrays
DECLARE laCatgLine[1,2], laOperLine[1,3]

*-- Initialize categpries and operations list variables
STORE 1 TO lsCatgLine, lsOperLine

STORE .F. TO llSaveSort, llChngSort  

*-- Collect the operations' sequence of all operations into 
*-- a cursor so as to restore if cancelled.
lcSeqCursr = gfTempName()
SELECT RECNO() AS nRecNo, cStatus, cOprt_Seq, cCtg_Seq;
  FROM (lc_PMPthDt);
  INTO CURSOR (lcSeqCursr)
  
*-- Collect all categories in categories arrays
SELECT DISTINCT &lc_PMPthDt..cOprt_Ctg, PMCTGHD.cCtg_Dsc;
  FROM (lc_PMPthDt), PMCTGHD;
 WHERE &lc_PMPthDt..cOprt_Ctg = PMCTGHD.cOprt_Ctg;
  INTO ARRAY laCatgLine;
 ORDER BY &lc_PMPthDt..cCtg_Seq
lcOldCatg   = laCatgLine[1,1]
lcCategory  = laCatgLine[1,2]

*-- Collect the operations of the first category so as to be 
*-- displayed in the related operations array
SELECT RECNO(), cOprt_ID, cOprt_Dsc;
  FROM (lc_PMPthDt);
 WHERE cOprt_Ctg =  lcOldCatg;
  INTO ARRAY laOperLine;
 ORDER BY cOprt_Seq

*-- Define category's popup bars 
FOR lnCount = 1 to ALEN(laCatgLine,1)
  DEFINE BAR lnCount OF puCatgs ;
         PROMPT laCatgLine[lnCount,1] + ' ' + ;
                laCatgLine[lnCount,2] 
ENDFOR

*-- Define operations's popup bars 
FOR lnCount = 1 to ALEN(laOperLine,1)
  DEFINE BAR lnCount OF puOprts ;
         PROMPT laOperLine[lnCount,2] + ' ' + ;
                laOperLine[lnCount,3]
ENDFOR

lnCurAlias = SELECT(0)

PUSH KEY
IF TYPE("lc_PMPrjDt") = "C"
  SELECT (lc_PMPrjDt)
ELSE
  SELECT (lc_PMPthDt)
ENDIF
ON KEY LABEL ESC T=0
DO (gcScrDir+"MFPSORT.SPX")
ON KEY LABEL ESC DO  lpEscScr
POP KEY


*-- If the current sorting is to be saved,
IF llSaveSort
  *-- Save the last set of operations' sequence into the file, 
  FOR lnCount = 1 TO CNTBAR('puOprts')
    lnSourcRow = GETBAR('puOprts', lnCount)     
    IF lnSourcRow > 0
      GO laOperLine[lnSourcRow, 1]
      REPLACE cOprt_Seq WITH PADL(lnCount, 2, '0'),;
              cStatus WITH IIF(cStatus = 'S', 'M', cStatus)
    ENDIF  
  ENDFOR

  **-- Save categories new sequence into the file.
  IF llChngSort
    FOR lnCount = 1 TO CNTBAR('puCatgs')
      lnSourcRow = GETBAR('puCatgs', lnCount)     
      IF lnSourcRow > 0
         REPLACE cCtg_Seq  WITH PADL(lnCount, 2, '0');
                 cStatus   WITH IIF(cStatus = 'S', 'M', cStatus);
            FOR cOprt_Ctg = laCatgLine[lnSourcRow, 1]                 
      ENDIF  
    ENDFOR
  ENDIF
*-- Otherwise, restore the old operations' sort 
ELSE
  SELECT (lcSeqCursr)
  INDEX ON nRecNo TAG (lcSeqCursr) OF (lcSeqCursr)
  
  SELECT (lc_PMPthDt)
  SET RELATION TO RECNO() INTO (lcSeqCursr)
  REPLACE ALL cStatus   WITH &lcSeqCursr..cStatus,;
              cOprt_Seq WITH &lcSeqCursr..cOprt_Seq,;
              cCtg_Seq  WITH &lcSeqCursr..cCtg_Seq
ENDIF

*-- Restore environment
RELEASE POPUPS puCatgs, puOprts EXTENDED 
IF USED(lcSeqCursr)
  USE IN (lcSeqCursr)
ENDIF

SELECT (lc_PMPthDt)
GO TOP 
=lfwBrwWhen(lc_PMPthDt, .T.)

*!*************************************************************
*! Name      : lfwLsCatgs
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : When function of lsCatgLine
*!             Adjusts display of a categoy's related operations
*!             in the operations list.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwLsCatgs()
*!*************************************************************
FUNCTION lfwLsCatgs
IF CNTBAR('puCatgs') > 0
  *-- If a new category is selected, or the order of the category
  *-- is changed,
  lnBarNo  = GETBAR('puCatgs', lsCatgLine)
  llChngSort  = llChngSort  .OR. lnBarNo <> lsCatgLine
  IF lcOldCatg <> laCatgLine[lnBarNo, 1]
    lcOldCatg  = laCatgLine[lnBarNo, 1]
    lcCategory = laCatgLine[lnBarNo, 2]
        
    *-- Save the old category's operations order into the file, 
    FOR lnCount = 1 TO CNTBAR('puOprts')
      lnSourcRow = GETBAR('puOprts', lnCount)     
      IF lnSourcRow > 0
        GO laOperLine[lnSourcRow, 1]
        REPLACE cOprt_Seq WITH PADL(lnCount, 2, '0'),;
                cStatus   WITH IIF(cStatus = 'S', 'M', cStatus) 
      ENDIF  
    ENDFOR
    
    *-- Recolloect the new category's operations into the operation
    *-- lines array
    SELECT RECNO(), cOprt_ID, cOprt_Dsc;
      FROM (lc_PMPthDt);
     WHERE cOprt_Ctg =  lcOldCatg;
      INTO ARRAY laOperLine;
     ORDER BY cOprt_Seq

    *-- Release bars of the current operations file
    RELEASE BAR ALL OF puOprts
    
    *-- ReDefine operations's popup bars 
    FOR lnCount = 1 to ALEN(laOperLine,1)
      DEFINE BAR lnCount OF puOprts ;
             PROMPT laOperLine[lnCount,2] + ' ' + ;
                    laOperLine[lnCount,3]
    ENDFOR
    
    *-- Show operations list
    lsOperLine = 1
    SHOW GET lsOperLine 
    =lfRefresh()
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvOkSort
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Valid function for pbOk (push button OK)
*!             Confirms current sort of categories and operations
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvOkSort()
*!*************************************************************
FUNCTION lfvOkSort
llSaveSort = .T.

*!*************************************************************
*! Name      : lfGetSeqNm
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Calculates a category sequence number
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  A correct category sequence number
*!*************************************************************
*! Example            :  lfGetSeqNm()
*!*************************************************************
FUNCTION lfGetSeqNm
PRIVATE lnCurAlias, lnCurTag, lnCurRec, lcNewCtgSeq

lnCurAlias = SELECT(0)
SELECT (lc_PMPthDt)
lnCurRec = RECNO()
lnCurTag = VAL(SYS(21))
SET ORDER TO TAG PMPTHDT
IF SEEK(lcOprt_Ctg)
  lcNewCtgSeq = cCtg_Seq
ELSE  
  SET ORDER TO TAG PMPTHDTS
  IF SEEK(lcPath_ID + PMCTGDT.cCtg_Seq)
    SELECT MAX(cCtg_Seq) ;
      FROM (lc_PMPthDt);
      INTO ARRAY laTmpArry
    lcNewCtgSeq = IIF(_TALLY > 0, PADL(INT(VAL(laTmpArry))+1,  2, '0'), '01')
  ELSE
    lcNewCtgSeq = PMCTGDT.cCtg_Seq
  ENDIF
ENDIF
SET ORDER TO (lnCurTag)
IF BETWEEN(lnCurRec, 1, RECCOUNT())
  GO lnCurRec
ENDIF
SELECT (lnCurAlias)  
RETURN lcNewCtgSeq


*!*************************************************************
*! Name      : lfvCal_ID                            N49,1
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Validates get field cCal_ID
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
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)
SELECT (lc_PMPthDt)
llBrowse  = llBrowse .OR. '?' $ m.cCal_ID
lcSetExact = SET('EXACT')
SET EXACT ON

REPLACE cCal_ID WITH PADR(ALLTRIM(m.cCal_ID),4)
SHOW GET m.cCal_ID
SELECT PMCALHD
IF !llBrowse 
  IF !EMPTY(&lc_PMPthDt..cCal_ID) .AND. !SEEK(&lc_PMPthDt..cCal_ID, 'PMCALHD') 
    IF BETWEEN(RECNO(0), 1, RECCOUNT())
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF
    llBrowse = .T.    
  ELSE
    SELECT (lc_PMPthDt)
    REPLACE cStatus WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1),;
            cAdd_User WITH gcUser_ID,;
            dAdd_Date WITH ldCurDate,;
            cAdd_Time WITH TIME()
    glUpdated = .T.                    
  ENDIF
ELSE
  GO TOP  
ENDIF 
IF llBrowse  
  lcBrFields = lcCalFlds
  DIME laTemp[1]
  lcFldName = 'cCal_ID'
  IF ARIABROW('', @lcCalTtl,; 
              gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,lcFldName,'laTemp')
    m.cCal_ID = PMCALHD.cCal_ID             
    SELECT (lc_PMPthDt)
    REPLACE cCal_ID   WITH PMCALHD.cCal_ID,;
            cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1),;
            cAdd_User WITH gcUser_ID,;
            dAdd_Date WITH ldCurDate,;
            cAdd_Time WITH TIME()
    glUpdated = .T.        
  ELSE
    m.cCal_ID = lcOldVal
    SELECT (lc_PMPthDt)    
    REPLACE cCal_ID WITH lcOldVal
  ENDIF           
ENDIF
llBrowse = .F.
SET EXACT &lcSetExact
SELECT (lnCurAlias)            

*!*************************************************************
*! Name      : lfExtPrg                      N49,1
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Calling External program
*!*************************************************************
*! Calls              :  lfExtPrg()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfExtPrg()
*!*************************************************************
FUNCTION lfExtPrg
PARAMETERS lcMode, lcOprtCtg, lcNewOprt

llCatgrAdd = .F.
=gfStatic()
DO gcAppHome+"SM\"+"SMCATGR" WITH lcMode, lcOprtCtg, lcNewOprt
RETURN llCatgrAdd

*!*************************************************************
*! Name      : lpClsScr
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : local close 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpClsScr()
*!*************************************************************
FUNCTION lpClsScr

llExitLoop = .T.    

*!*************************************************************
*! Name      : lfClearRead
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Clears READ upon branching if in Add or Edit mode.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClearRead()
*!*************************************************************
FUNCTION lfClearRead

STORE .T. TO  llOper ,llExitLoop 

IF laScrMode[2]
  =lfvOperton()
ELSE
  lcFuncName = '=lfvOperton()'
  lnCurObj   = _CUROBJ
  CLEAR READ
ENDIF

*!*************************************************************
*! Name      : lfvClose
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : valid function of Close operation screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvClose()
*!*************************************************************
FUNCTION lfvClose

IF !laScrMode[2]
  llEnbSave = .T.
ENDIF

STORE .T. TO  llOper 
llExitLoop = .F.
=lpShow()

CLEAR READ

*!*************************************************************
*! Name      : lfvUser
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   : Valid function of user popup
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION lfvUser

lcOprt_Res = SPACE(10)

SHOW GET lcOprt_Res

=lfRefresh()

*!*************************************************************
*! Name      : lfGetData
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   : Get data of selected path
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION lfGetData

DECLARE laOperaton[1], laPredecsr[1], laCategory[1]
STORE '' TO laPredecsr, laOperaton, laCategory

*-- Collect path details into the cursor
SELECT (lc_PMPthDt)
ZAP

SELECT PMPTHHD
IF BETWEEN(RECNO(), 1, RECCOUNT())
  GO RECNO()
ENDIF
    
SELECT PMPTHDT
=SEEK(m.cPath_ID)
SCAN REST WHILE cpath_id+coprt_ctg+coprt_id = m.cPath_ID
  SCATTER MEMVAR MEMO
  m.nRecNo  = RECNO()
  m.cStatus = 'S'
  INSERT INTO (lc_PMPthDt) FROM MEMVAR
       
  LCOPRT_RES = IIF(EMPTY(m.cOprt_Res),m.cGroup_id,m.cOprt_Res)
                 
  *-- Include category code field
  laOperaton[ALEN(laOperaton)] = m.cOprt_Ctg + '\' + m.cOprt_ID + ;
                                ' ' +SUBSTR(m.cOprt_Dsc, 1, 18)
 
  DECLARE laOperaton[ALEN(laOperaton) + 1]
ENDSCAN
lnNOfLines = RECCOUNT(lc_PMPthDt)
IF ALEN(laOperaton) > 1
  DECLARE laOperaton[ALEN(laOperaton) - 1]
ENDIF

*-- Collect available categories in an array
SELECT DISTINCT cOprt_Ctg ;
  FROM (lc_PMPthDt);
  INTO ARRAY laCategory
      
*-- Collect predecessors from PMPTHRL into a cursor
SELECT (lc_PMPthRl)
ZAP
SELECT PMPTHRL
IF SEEK(m.cPath_ID)
  SCAN REST WHILE cpath_id+coprt_ctg+coprt_id = m.cPath_ID
    SCATTER MEMVAR MEMO
    m.nRecNo  = RECNO()
    m.cStatus = 'S'
    INSERT INTO (lc_PMPthRl) FROM MEMVAR
  ENDSCAN
ENDIF

*!*************************************************************
*! Name      : lpEscScr
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   :
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
PROCEDURE lpEscScr
=gfCPClose()

*-----
FUNCTION gfCPBrows

SELECT (lcBaseFile)
=gfBrows()
DO lpShow

*!*************************************************************
*! Name      : lfUserBrow
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Browse activation function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfUserBrow()
*!*************************************************************
*E302251,1 AMH
FUNCTION lfUserBrow

lnCurAlias = select(0)
SELECT (lcPMPTHNTF)
lnBrRecNo = RECNO()
BROWSE FIELDS cMarker = IIF(RECNO() = lnBrRecNo, '', ' ');
                         : H = ' '   :R:1 :W = .F.,;
              cUser_ID   : H = 'User':R:10,;
              nBfrStrtDy : H = 'Before Start With':R,;
              lOnStrt    : H = 'On Start':R,;
              nStrtDelay : H = 'On Start Delay Every' :R,;
              lOnRedrct  : H = 'On Redirect':R:10,;
              nBfrCmplDy : H = 'Before Completion With':R,;
              lOnCmplt   : H = 'On Completion':R,;
              nCmplDelay : H = 'On Completion Delay Every' :R;
         WINDOW SMNTFST1 IN WINDOW SMNTFSET;
         WHEN lfwBrow();
         NOMENU;
         NOAPPEND;
         NODELETE;
         NOEDIT;
         NOWAIT;
         SAVE;
         NOCLEAR;
         TITLE (lcBrUser)
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfReadDeact
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : READ Deactivate function of PRM500.SCX
*!*************************************************************
*! Calls     : lpTab
*!*************************************************************
*! Returns            :  .f.
*!*************************************************************
*! Example            :  =lfReadDeact()
*!*************************************************************
*E302251,1 AMH
FUNCTION lfReadDeact

*-- Key traps for the browse
IF WONTOP() = lcBrUser
  ON KEY LABEL CTRL+Q     lnDummy = 1
  ON KEY LABEL CTRL+W     lnDummy = 1
  ON KEY LABEL CTRL+HOME  GO TOP
  ON KEY LABEL CTRL+END   GO BOTTOM
  ON KEY LABEL TAB        DO lpTab WITH "SMNTFST2",OBJNUM(m.cUser_ID)
  ON KEY LABEL BACKTAB    DO lpTab WITH "SMNTFST2",OBJNUM(pbClose)
ENDIF  
RETURN .F.

*!*************************************************************
*! Name      : lfwBrow
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : When function of users browse for notify screen
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  lfwBrow()
*!*************************************************************
*E302251,1 AMH
FUNCTION lfwBrow
PARAMETERS llDontRef

SELECT (lcPMPTHNTF)
lnBrRecNo = RECNO()
SCATTER MEMVAR
=lfNtfRef()

IF !llDontRef
  SHOW WINDOW (lcBrUser) REFRESH SAME
ENDIF

*!*************************************************************
*! Name      : lfNtfRef
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Refresh function for notify screen
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  lfNtfRef()
*!*************************************************************
*E302251,1 AMH
FUNCTION lfNtfRef

IF laScrMode[2] .OR. EOF(lcPMPTHNTF)
  SHOW GET m.cUser_ID   DISABLE
  SHOW GET ibUser_ID    DISABLE
  SHOW GET m.cEmail_Add DISABLE
  SHOW GET m.lBfrStrt   DISABLE
  SHOW GET m.nBfrStrtDy DISABLE
  SHOW GET m.lBfrCmplt  DISABLE
  SHOW GET m.nBfrCmplDy DISABLE
  SHOW GET m.nStrtDelay DISABLE
  SHOW GET m.nCmplDelay DISABLE
  SHOW GET m.lOnStrt    DISABLE
  SHOW GET m.lOnCmplt   DISABLE
  SHOW GET m.lOnRedrct  DISABLE
  SHOW GET pbRemove     DISABLE
ELSE
  SHOW GET m.cUser_ID   ENABLE
  SHOW GET ibUser_ID    ENABLE
  SHOW GET m.cEmail_Add ENABLE
  SHOW GET m.lBfrStrt   ENABLE
  IF m.lBfrStrt
    SHOW GET m.nBfrStrtDy ENABLE
  ELSE
    SHOW GET m.nBfrStrtDy DISABLE
  ENDIF
  SHOW GET m.lBfrCmplt  ENABLE
  IF m.lBfrCmplt
    SHOW GET m.nBfrCmplDy ENABLE
  ELSE
    SHOW GET m.nBfrCmplDy DISABLE
  ENDIF
  SHOW GET m.nStrtDelay ENABLE
  SHOW GET m.nCmplDelay ENABLE
  SHOW GET m.lOnStrt    ENABLE
  SHOW GET m.lOnCmplt   ENABLE
  SHOW GET m.lOnRedrct  ENABLE
  SHOW GET pbRemove     ENABLE
ENDIF

IF laScrMode[2]
  SHOW GET pbNew        DISABLE
ELSE
  SHOW GET pbNew        ENABLE
ENDIF

SHOW WINDOW SMNTFST2 REFRESH

*!*************************************************************
*! Name      : lfvUser_ID
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates User_ID field
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvUser_ID()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvUser_ID

IF (PADR(m.cUser_ID,10) = PADR(lcOldVal,10)) .AND. !llBrowse
  RETURN .T.
ENDIF

PRIVATE lnCurAlias
lnCurAlias = SELECT(0)

lcOldBrFld = lcBrFields
SELECT SYUUSER
lcBrFields = [cUser_Id : H = 'User ID', cUsr_Name : H = 'User Name']  
IF (!EMPTY(m.cUser_ID) AND !SEEK(PADR(m.cUser_ID,10),'SYUUSER')) .OR. llBrowse
  llBrowse = .F.
  DIMENSION laTemp[2]
  IF ARIABROW('','Users',gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,'cUser_Id,cEmail_Add','laTemp')
    m.cUser_ID   = laTemp[1]
    m.cEmail_Add = laTemp[2]
  ELSE
    m.cUser_ID   = lcOldVal
  ENDIF
ELSE
  IF !EMPTY(m.cUser_ID)
    lnTmpRecNo = RECNO(lcPMPTHNTF)
    IF !SEEK(lcPath_ID+lcOprt_Ctg+lcoprt_id+m.cUser_ID,lcPMPTHNTF)
      m.cEmail_Add = SYUUSER.CEMAIL_ADD
    ELSE
      WAIT WINDOW "User Exist"
      m.cUser_ID = lcOldVal
    ENDIF
    GO lnTmpRecNo IN (lcPMPTHNTF)
  ENDIF
ENDIF  
lcBrFields = lcOldBrFld

SELECT (lcPMPTHNTF)
REPLACE cUser_ID   WITH m.cUser_ID,;
        cEmail_Add WITH m.cEmail_Add,;
        cStatus    WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1)
SHOW WINDOW (lcBrUser) REFRESH SAME
glUpdated = .T.
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvNewUser
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates New button of notify screen
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvNewUser()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvNewUser

SELECT (lcPMPTHNTF)
APPEND BLANK
REPLACE cStatus   WITH "A",;
        cPath_ID  WITH lcPath_ID,;
        COprt_Ctg WITH lcOprt_Ctg,;
        COprt_ID  WITH lcOprt_id
=lfwBrow(.T.)
_CUROBJ = OBJNUM(m.cUser_ID)

*!*************************************************************
*! Name      : lfEsc
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates ESC Key
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfEsc()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfEsc

IF WEXIST("SMNTFST2")
  ACTIVATE WINDOW SMNTFST2
ELSE
  ACTIVATE WINDOW MFPATHA1
ENDIF

_CUROBJ = OBJNUM(pbClose)
KEYBOARD CHR(13)

*!*************************************************************
*! Name      : lfvEmail
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates cEmail_Add field
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvEmail()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvEmail

=lfTmpUpdat("cEmail_Add")

*!*************************************************************
*! Name      : lfTmpUpdat
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Update field on lcPMPCTGNT
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : lcField
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfTmpUpdat()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfTmpUpdat
PARAMETERS lcField,lcValue

SELECT (lcPMPTHNTF)
REPLACE (lcField) WITH EVALUATE("m."+lcField),;
        cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1)
SHOW WINDOW (lcBrUser) REFRESH SAME
glUpdated = .T.

*!*************************************************************
*! Name      : lfvBfrStrt
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates lBfrStrt field
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvBfrStrt()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvBfrStrt

=lfTmpUpdat("lBfrStrt")
IF m.lBfrStrt
  m.nBfrStrtDy = 1
  =lfTmpUpdat("nBfrStrtDy")
  SHOW GET m.nBfrStrtDy ENABLE
ELSE
  m.nBfrStrtDy = 0
  =lfTmpUpdat("nBfrStrtDy")
  SHOW GET m.nBfrStrtDy DISABLE
ENDIF

*!*************************************************************
*! Name      : lfvBfrStDy
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates nBfrStrtDy field
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvBfrStDy()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvBfrStDy

=lfTmpUpdat("nBfrStrtDy")

*!*************************************************************
*! Name      : lfvBfrCmpl
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates lBfrCmplt field
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvBfrCmpl()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvBfrCmpl

=lfTmpUpdat("lBfrCmplt")
IF m.lBfrCmplt
  m.nBfrCmplDy = 1
  =lfTmpUpdat("nBfrCmplDy")
  SHOW GET m.nBfrCmplDy ENABLE
ELSE
  m.nBfrCmplDy = 0
  =lfTmpUpdat("nBfrCmplDy")
  SHOW GET m.nBfrCmplDy DISABLE
ENDIF

*!*************************************************************
*! Name      : lfvBfrCmDy
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates nBfrCmplDy field
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvBfrCmDy()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvBfrCmDy

=lfTmpUpdat("nBfrCmplDy")

*!*************************************************************
*! Name      : lfvStDelay
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates nStDelay field
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvStDelay()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvStDelay

=lfTmpUpdat("nStrtDelay")

*!*************************************************************
*! Name      : lfvCmDelay
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates nCmplDelay field
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvCmDelay()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvCmDelay

=lfTmpUpdat("nCmplDelay")

*!*************************************************************
*! Name      : lfvOnStrt
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates lOnStrt field
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvOnStrt()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvOnStrt

=lfTmpUpdat("lOnStrt")

*!*************************************************************
*! Name      : lfvOnCmplt
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates lOnCmplt field
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvOnCmplt()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvOnCmplt

=lfTmpUpdat("lOnCmplt")

*!*************************************************************
*! Name      : lfvOnRdrct
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates lOnRedrct field
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvOnRdrct()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvOnRdrct

=lfTmpUpdat("lOnRedrct")

*!*************************************************************
*! Name      : lfvRemUser
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/31/2005
*! Purpose   : Validates Remove button of notify screen
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvRemUser()
*!*************************************************************
*!Modifications:
*!*************************************************************
*E302251,1 AMH
FUNCTION lfvRemUser

lnCurAlias = SELECT(0)

IF gfModalgen("TRM38208B38006","DIALOG") = 1
  SELECT (lcPMPTHNTF)
  REPLACE cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1)
  DELETE
  LOCATE
  glUpdated   = .T.
  =lfwBrow()
ENDIF
SELECT (lnCurAlias)