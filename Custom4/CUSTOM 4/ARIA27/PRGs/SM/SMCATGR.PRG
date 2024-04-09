*:************************************************************************
*: Program file  : SMCATGR.PRG
*: Program desc. : Project management/Categories of Operations (N000341)
*: For screen    : MFCTGRY.SPR
*:         System: ARIA APPAREL SYSTEM
*:      Developer: HEND GHANEM
*:************************************************************************
*: Calls         : SMCATGR.SPX
*:*************************************************************************
*: Passed Parameters  : lcScrMode (optional)
*:                      lcOprt_Ctg
*:                      lcOprt_ID (optional)
*:*************************************************************************
*: Example            : DO SMCATGR
*:************************************************************************
*:E301869,2 AMH 12/30/2002 Create a production schedule project according to a setting.
*:E302251,1 AMH 8/23/2005  Calling the Notify Seting screen.
*:************************************************************************
PARAMETERS lcScrMode, lcOprt_Ctg, lcNewOprt

PRIVATE lnCalAlias, glUpdated, lnNOfLines, lcOprt_ID,;
        lcKeyStat, lcNewStat, lcAddStat

STORE '' TO lcOldOprt, lgOldVal,lcModal,lcNewStat, lcAddStat,lcNotfyStat
lcCtgFlds    = [cOprt_Ctg : H = 'Category', cCtg_Dsc : H = 'Description']
lcOprFlds    = [cOprt_ID : H = 'Task ID', cOprt_Dsc : H = 'Task Name', cOprt_Ctg : H='Category']
lcCtgTtl     = 'Categories of Tasks'
*-- Add calendars
lcCalFlds    = [cCal_ID : H = 'Calendar ID', cCal_Des : H = 'Description']
lcCalTtl     = 'Calendars'

STORE '' TO lc_PMCtgDt,lc_PMCtgRl
STORE '' To lcmoprtctg,lcmctg_dsc,lnOperLins,lcmccal_id,llShw2Cust,lcmoprtdsc,lnmest_dur,;
            lnUser,lnUpdtMthd            
STORE 0 TO lnShw2Cust
*--ToolBar external proc.
laDefProc[7]  = .F.              && Disable the control panel delete proc.(lpDelScr)
laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)

*-- Color variables
lcEnbCntrl   = SCHEME(1,9)
lcDisCntrl   = SCHEME(1,10)
lcSelCntrl   = SCHEME(1,6)

STORE ' ' TO lc_PMCtgDt,lc_PMCtgRl
STORE SPACE(10) TO lcoprt_res 
DECLARE laPredecsr[1], laUpdtMthd[1], laUsers[1],;
        laNotified[1], laOperLine[1,3]
STORE '' TO laPredecsr, laNotified, laOperLine
lnNOfLines  = 0
m.cOprt_Dsc = SPACE(40)
ldCurDate   = gdSysDate
lnCalAlias  = SELECT(0)

STORE .F. TO llBrowse, glUpdated,llPramPasd,llAddOprt

DEFINE POPUP puOprts MOVER SCROLL MARGIN MARK ''
lsOperLins  = 1

*** Get the name of the base window in the program if it call from 
*** another program (Project or Path program)
IF TYPE('lcScrMode') = 'C'
  lcProg_Nam  = 'AWRSMCATGR'
  gcBaseWind  = ALLTRIM(UPPER(IIF(' ' $ lcProg_Nam,;
                    SUBSTR(lcProg_Nam,1,ATC(' ',lcProg_Nam)-1),lcProg_Nam)))
  gnProgCopy  = 0
  FOR lnAscii = 65 TO 90
    gnProgCopy = gnProgCopy + 1
    gcBaseWind =STUFF(gcBaseWind,2,1,CHR(lnAscii))
    IF WVISIBLE(gcBaseWind)
      LOOP
    ELSE
      EXIT 
    ENDIF
  ENDFOR
  lcBaseWind  = gcBaseWind
  =gfStatic()
ENDIF

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

*E302251,1 AMH Define variables for Notify Setting Screen [Start]
lcBrUser   = "Users"
lcPMPCTGNT = gfTempName()
*E302251,1 AMH [End]

IF !gfSetup()
  RETURN
ELSE
  m.coprt_ctg = lcmoprtctg 
  m.cctg_dsc  = lcmctg_dsc
  lsOperLins  = lnOperLins
  m.ccal_id   = lcmccal_id
  m.coprt_dsc = lcmoprtdsc
  m.nest_dur  = lnmest_dur
  puUser      = lnUser
  puUpdtMthd  = lnUpdtMthd            
ENDIF  

IF TYPE('lcScrMode') = 'C'
  lcBaseFile = 'PMCTGHD'  
ENDIF  


IF !WEXIST(gcBaseWind)
  
  IF TYPE('lcScrMode') = 'C'
    STORE .F. TO laScrMode
    DO CASE 
      CASE lcScrMode = 'S'
        laScrMode[1] = .T.
      CASE lcScrMode = 'V'
        laScrMode[2] = .T.
      CASE lcScrMode = 'E'
        laScrMode[3] = .T.
      CASE lcScrMode = 'A'
        laScrMode[4] = .T.
    ENDCASE
  ENDIF  

  lc_PMCtgDt   = gfTempName()
  lc_PMCtgRl   = gfTempName()
  
  lcScFields = 'coprt_ctg,cctg_dsc,cctg_seq'
  
  *-- Create a file with the same structure as PMCTGDT 
  SELECT PMCTGDT
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

  CREATE CURSOR (lc_PMCtgDt) FROM ARRAY laFileStru
  INDEX ON cOprt_ID  TAG PMCTGDT  OF (lc_PMCtgDt)
  INDEX ON cOprt_Seq TAG PMCTGDTS OF (lc_PMCtgDt)

  SELECT PMCTGRL
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

  CREATE CURSOR (lc_PMCtgRl) FROM ARRAY laFileStru
  INDEX ON cPrd_ID  TAG PMCTGRLP OF (lc_PMCtgRl)
  INDEX ON cOprt_ID TAG PMCTGRL  OF (lc_PMCtgRl)

  SELECT cUser_ID ;
    FROM (gcSysHome + 'SYUUSER');
    INTO ARRAY laUsers;
    ORDER BY cUser_ID

  lcNotfyStat = IIF(EMPTY(laUsers[1]), '', 'NOMODIFY')

  *-- Check if the program is called from paths or projects
  *-- screen.
  *-- If called from either program, set the required
  *-- mode, whether Add, or Edit, initializing variables
  *-- as appropriate to the mode.
  
  IF TYPE('lcScrMode') = 'C' .AND. (laScrMode[4] OR laScrMode[3])
    IF EMPTY(lcOprt_Ctg) .OR. EMPTY(lcNewOprt) .AND. laScrMode[3]
      SELECT (lnCalAlias)
      RETURN .F.
    ENDIF
    llPramPasd = .T.          &&  A parameter is passed
    IF laScrMode[4]
      SELECT PMCTGDT
      SCATTER MEMVAR MEMO BLANK
      m.cOprt_Ctg   = lcOprt_Ctg
      lcOprt_ID     = SPACE(5)
      lcNewOprt     = ' '
      *- Get the maximum sequence number into an array element
      SELECT MAX(cCtg_Seq) FROM PMCTGHD INTO ARRAY laTmpArry
      lcctg_seq   = IIF(_TALLY > 0, PADL(INT(VAL(laTmpArry))+1, 2, '0'), '01')
      lcAddStat   = 'ENABLE'
      llAddOprt   = .F.
      m.cCtg_Dsc  = ''
    ELSE
      SELECT PMCTGDT
      IF SEEK(lcOprt_Ctg)
        SCAN REST WHILE COPRT_CTG+COPRT_SEQ = lcOprt_Ctg
          SCATTER MEMVAR MEMO
          m.nRecNo  = RECNO()
          m.cStatus = 'S'
          INSERT INTO (lc_PMCtgDt) FROM MEMVAR
          laOperLine[ALEN(laOperLine,1),1] = RECNO(lc_PMCtgDt)
          laOperLine[ALEN(laOperLine,1),2] = m.cOprt_ID    
          
          laOperLine[ALEN(laOperLine,1),3] = m.cOprt_Dsc + ' '+; 
              IIF(EMPTY(m.cOprt_Res) AND EMPTY(m.cGroup_id),SPACE(10),IIF(EMPTY(m.cOprt_Res),m.cGroup_id,m.cOprt_Res)) +;
              SPACE(9)+ STR(m.nEst_Dur,3)
          
          DECLARE laOperLine[ALEN(laOperLine,1) + 1,3]
        ENDSCAN
        llAddOprt     = .T.
      ENDIF  
      lcAddStat     = 'DISABLE'
      lnNOfLines = RECCOUNT(lc_PMCtgDt)
      IF ALEN(laOperLine, 1) > 1
        DECLARE laOperLine[ALEN(laOperLine,1) - 1,3]
      ENDIF
      =lfFillPop()
      
      *-- Collect predecessors from PMCTGRL into cursor 
      SELECT PMCTGRL
      IF SEEK(lcOprt_Ctg)
        SCAN REST WHILE cOprt_Ctg = lcOprt_Ctg
          m.nRecNo  = RECNO()
          m.cStatus = 'S'
          SCATTER MEMVAR MEMO
          INSERT INTO (lc_PMCtgRl) FROM MEMVAR
        ENDSCAN
      ENDIF
      
      SELECT PMCTGHD
      SEEK lcOprt_Ctg
      SCATTER MEMVAR MEMO
      lcCtg_Seq = m.cCtg_Seq
    ENDIF
    lcKeyStat     = 'DISABLE'
    lcNewStat     = 'ENABLE'    
          
  ELSE
    llPramPasd  = .F.          
    llAddOprt     = .F.  
    STORE .F. TO laScrMode
    laScrMode[1] = .T.
    lcNewOprt     = ' '
    lcOprt_Ctg    = SPACE(3)   
    lcOprt_ID     = SPACE(5)
    lcCtg_Seq     = SPACE(2)
    STORE 'Manual' TO laUpdtMthd[1], lcUpdtMthd, puUpdtMthd
    STORE 0 TO lnNOfLines  
    lcKeyStat     = 'ENABLE'
    STORE 'DISABLE' TO lcAddStat, lcNewStat 
    
    GO TOP
    SCATTER MEMVAR MEMO BLANK  
  ENDIF
  
  *E302251,1 AMH Create cursor to hold data of Notify Setting Screen [Start]
  CREATE CURSOR (lcPMPCTGNT) (COprt_Ctg  C(3)  ,COprt_ID   C(5)  ,CUser_ID   C(10),LBfrStrt L,NBfrStrtDy N(3,0),;
                              LBfrCmplt  L     ,NBfrCmplDy N(3,0),LOnStrt    L    ,LOnCmplt L,LOnRedrct  L,;
                              NStrtDelay N(3,0),NCmplDelay N(3,0),CEMAIL_ADD C(60),cStatus C(1))
  SELECT (lcPMPCTGNT)
  INDEX ON COprt_Ctg+COprt_ID+CUser_ID TAG (lcPMPCTGNT) OF (lcPMPCTGNT)
  =gfOpenFile(gcDatadir+'PMPCTGNT','PMPCTGNT','SH')
  *E302251,1 AMH [End]
  
ELSE
  =lpShow()
ENDIF

IF llPramPasd
  =lpShow()
ENDIF

PUSH KEY
ON KEY LABEL ESC DO  lpEscScr

*E301869,2 AMH Call screen from the correct directory [Start]
DO (gcScrDir+"SM\"+"SMCATGR.SPX")
*E301869,2 AMH [End]

POP KEY

RELEASE POPUP puOprts

IF glQuitting
  IF USED(lc_PMCtgDt)
    USE IN (lc_PMCtgDt)
  ENDIF
  IF USED(lc_PMCtgRl)  
    USE IN (lc_PMCtgRl)  
  ENDIF
  
  *E302251,1 AMH Remove cursor of Notify Setting Screen [Start]
  IF USED(lcPMPCTGNT)
    USE IN (lcPMPCTGNT)
  ENDIF
  *E302251,1 AMH [End]
  
ENDIF

SELECT (lnCalAlias)
RETURN llPramPasd 

*!*************************************************************
*! Name      : lfReadWhen
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   : Adjusts display of objects according to the mode.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfReadWhen()
*!*************************************************************
FUNCTION lfReadWhen
IF laScrMode[3]
  lcOprt_ID = lcNewOprt
  _CUROBJ   = OBJNUM(pbNew)
  KEYBOARD CHR(13)
ENDIF   
RETURN .T.
  
*!*************************************************************
*! Name      : lpShow
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
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
*!Modifications:
*!*************************************************************
FUNCTION lpShow

*-- Clear popup bars
RELEASE BAR ALL OF puOprts
DECLARE laPredecsr[1], laNotified[1], laOperLine[1,3]
STORE '' TO laPredecsr, laNotified, laOperLine
lsOperLins = 1

DO CASE
  CASE laScrMode[1]
    SELECT (lc_PMCtgDt)
    ZAP
    SELECT (lc_PMCtgRl)
    ZAP
    
    *E302251,1 AMH Collecting Data for Notify Setting Screen [Start]
    SELECT (lcPMPCTGNT)
    ZAP
    *E302251,1 AMH [End]
    
    SELECT cUser_ID ;
      FROM (gcSysHome + 'SYUUSER');
      INTO ARRAY laUsers;
      ORDER BY cUser_ID
    
    SELECT PMCTGHD
    SCATTER MEMVAR MEMO BLANK
    lcctg_seq  = m.cCtg_Seq 
    lcOprt_ID  = SPACE(5)

    
    SHOW GET ibOprt_Ctg  ENABLE
    SHOW GET m.cOprt_Ctg ENABLE
    SHOW GET m.cCtg_Dsc  DISABLE
    SHOW GET lcctg_seq   DISABLE
    SHOW GET lsOperLins ENABLE  
    =lfvOpertion()
    _CUROBJ = OBJNUM(m.cOprt_Ctg)
    
    IF TYPE('lcScrMode') = 'C'
      RELEASE WINDOW (gcBaseWind)
    ENDIF
    
  CASE laScrMode[2]
    
    SELECT (lc_PMCtgDt)
    ZAP
    
    *E302251,1 AMH Collecting Data for Notify Setting Screen [Start]
    SELECT (lcPMPCTGNT)
    ZAP
    *E302251,1 AMH [End]
    
    SELECT PMCTGHD
    SCATTER MEMVAR MEMO
    lcOprt_Ctg = m.cOprt_Ctg
    
    SELECT PMCTGDT
    SET ORDER TO PMCTGDTS
    IF SEEK(lcOprt_Ctg)
      SCAN REST WHILE cOprt_Ctg = lcOprt_Ctg
        SCATTER MEMVAR MEMO
        m.nRecNo  = RECNO()
        m.cStatus = 'S'
        INSERT INTO (lc_PMCtgDt) FROM MEMVAR
        laOperLine[ALEN(laOperLine,1),1] = RECNO(lc_PMCtgDt)
        laOperLine[ALEN(laOperLine,1),2] = m.cOprt_ID    
        laOperLine[ALEN(laOperLine,1),3] = m.cOprt_Dsc + ' '+; 
             IIF(EMPTY(m.cOprt_Res) AND EMPTY(m.cGroup_id),SPACE(10),IIF(EMPTY(m.cOprt_Res),m.cGroup_id,m.cOprt_Res)) +;
              SPACE(9)+ STR(m.nEst_Dur,3)
        DECLARE laOperLine[ALEN(laOperLine,1) + 1,3]
      ENDSCAN
      
      LCOPRT_RES = IIF(EMPTY(m.cOprt_Res),m.cGroup_id,m.cOprt_Res)
               
      
    ENDIF  
    
    *E302251,1 AMH Collecting Data for Notify Setting Screen [Start]
    SELECT PMPCTGNT
    IF SEEK(lcOprt_Ctg)
      SCAN REST WHILE COPRT_CTG+COPRT_ID+CUSER_ID = lcOprt_Ctg
        SCATTER MEMVAR
        m.cStatus = 'S'
        INSERT INTO (lcPMPCTGNT) FROM MEMVAR
      ENDSCAN
    ENDIF
    *E302251,1 AMH [End]
    
    lnNOfLines = RECCOUNT(lc_PMCtgDt)
    IF ALEN(laOperLine, 1) > 1
      DECLARE laOperLine[ALEN(laOperLine,1) - 1,3]
    ENDIF
    =lfFillPop()
    
    SELECT PMCTGHD
    lcctg_seq  = cCtg_Seq
    
    SHOW GET ibOprt_Ctg  DISABLE
    SHOW GET m.cOprt_Ctg DISABLE
    SHOW GET m.cCtg_Dsc  DISABLE
    SHOW GET lcctg_seq   DISABLE
    SHOW GET lcOprt_Res  DISABLE
    SHOW GET puUser      DISABLE

    GO TOP IN (lc_PMCtgDt)
    SHOW GET lsOperLins  ENABLE
    =lfvOpertion()

  CASE laScrMode[3]
    IF TYPE('lcScrMode') = 'C'
      lcStamp =  PMCTGHD.cAdd_User+IIF(EMPTY(PMCTGHD.dAdd_Date),'',DTOC(PMCTGHD.dAdd_Date))+;
                    PMCTGHD.cAdd_Time
    ENDIF  

    llCUpDate = .T.
    glUpdated    = .F.

    SELECT (lc_PMCtgRl)
    ZAP

    SELECT (lc_PMCtgDt)
    ZAP

    SELECT PMCTGHD
    SCATTER MEMVAR MEMO
    lcOprt_Ctg = m.cOprt_Ctg

    SELECT PMCTGDT
    IF SEEK(lcOprt_Ctg)
      SCAN REST WHILE cOprt_Ctg = lcOprt_Ctg
        SCATTER MEMVAR MEMO
        m.nRecNo  = RECNO()
        m.cStatus = 'S'
        INSERT INTO (lc_PMCtgDt) FROM MEMVAR
        laOperLine[ALEN(laOperLine,1),1] = RECNO(lc_PMCtgDt)
        laOperLine[ALEN(laOperLine,1),2] = m.cOprt_ID    
        laOperLine[ALEN(laOperLine,1),3] = m.cOprt_Dsc + ' '+; 
           IIF(EMPTY(m.cOprt_Res) AND EMPTY(m.cGroup_id),SPACE(10),IIF(EMPTY(m.cOprt_Res),m.cGroup_id,m.cOprt_Res)) +;
              SPACE(9)+ STR(m.nEst_Dur,3)
                                            
        DECLARE laOperLine[ALEN(laOperLine,1) + 1,3]
      ENDSCAN
    ENDIF  
    lnNOfLines = RECCOUNT(lc_PMCtgDt)
    IF ALEN(laOperLine, 1) > 1
      DECLARE laOperLine[ALEN(laOperLine,1) - 1,3]
    ENDIF
    =lfFillPop()
    
    *-- Collect predecessors from PMCTGRL
    *-- Collect details into the cursor
    SELECT PMCTGRL
    IF SEEK(lcOprt_Ctg)
      SCAN REST WHILE cOprt_Ctg = lcOprt_Ctg
        SCATTER MEMVAR MEMO
        m.nRecNo  = RECNO()
        m.cStatus = 'S'
        INSERT INTO (lc_PMCtgRl) FROM MEMVAR
      ENDSCAN
    ENDIF
    
    GO TOP IN (lc_PMCtgDt)
    SELECT PMCTGHD
    lcctg_seq  = cCtg_Seq    
    
    SHOW GET m.cCtg_Dsc  ENABLE
    SHOW GET lcctg_seq   ENABLE
    SHOW GET lsOperLins
    =lfvOpertion(!EOF(lc_PMCtgDt))

    SHOW GET pbSav ENABLE  
    laCtrStat[11] = 'ENABLE'
    
  CASE laScrMode[4]
    llCUpDate = .T.
    glUpdated    = .F.


    DECLARE laTmpArry[1]
    laTmpArry[1] = '01'
    *-- Get the maximum sequence number into an array element
    SELECT MAX(cCtg_Seq) FROM PMCTGHD INTO ARRAY laTmpArry
    lcctg_seq    = IIF(_TALLY > 0, PADL(INT(VAL(laTmpArry))+1, 2, '0'), '01')
    lnNOfLines = 0
    
    SHOW GET ibOprt_Ctg  DISABLE
    SHOW GET m.cOprt_Ctg DISABLE
    SHOW GET lcoprt_id   DISABLE
    SHOW GET ibCal_ID    DISABLE
    SHOW GET m.ccal_id   DISABLE
    SHOW GET lnShw2Cust  DISABLE
    SHOW GET m.coprt_dsc DISABLE
    SHOW GET m.nest_dur  DISABLE
    SHOW GET lcoprt_res  DISABLE
    SHOW GET PuUser      DISABLE
    SHOW GET puUpdtMthd  DISABLE
    SHOW GET puUser      DISABLE
    
    SHOW GET pbPredecsr  DISABLE
    SHOW GET lcPredec    DISABLE
    SHOW GET pbNotify    DISABLE
    SHOW GET lcNotify    DISABLE
    SHOW GET m.cCtg_Dsc  ENABLE
    SHOW GET lcctg_seq   ENABLE
    SHOW GET pbNew       ENABLE
    SHOW GET pbNotes     DISABLE
    SHOW GET pbRemove    DISABLE

    SHOW GET pbSav ENABLE  
    laCtrStat[11] = 'ENABLE'

ENDCASE
SELECT PMCTGHD

lcmoprtctg = m.coprt_ctg 
lcmctg_dsc = m.cctg_dsc 
lnOperLins = lsOperLins 
lcmccal_id = m.ccal_id   
lcmoprtdsc = m.coprt_dsc 
lnmest_dur = m.nest_dur  
lnUser     = puUser     
lnUpdtMthd = puUpdtMthd  
  

*!*************************************************************
*! Name      : lfvBrowse
*! Developer : RENEE - Renee Ezzat
*! Date      : 01/16/1996
*! Purpose   : Valid function for push button < Browse >
*!             Browses all paths.
*!*************************************************************
*! Calls              :  lfvOprtCtg()
*!*************************************************************
*! Passed Parameters  :   None
*!*************************************************************
*! Returns            :   None
*!*************************************************************
*! Example            :  =lfvBrowse()
*!*************************************************************
FUNCTION lfvBrowse
llBrowse = .T.
=lfvOprtCtg()
llBrowse = .F.

*!*************************************************************
*! Name      : lfwOldVals
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
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
*! Name      : lfwOldFVal
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   : When function of get fields.
*!             Stores a fields old value
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfwOldFVal()
*!*************************************************************
FUNCTION lfwOldFVal
lgOldVal = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfVldKey
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   : Validates a key field.
*!*************************************************************
*! Passed Parameters  : lcSrchFile  : File to boe searched or browsed
*!                      lcSrchTag   : Tag to be used for seek or browse
*!                      lcBrFields  : Browse fields string
*!                      lcBrFlTtl   : Browse title
*!                      lcFldName   : return field name
*!                      lcNtFndMsg : message string to be displayed if the key
*!                                    is not found.
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfVldKey('PMCTGHD', 'PMCTGHD',;
*!                                lcCtgFlds, lcCtgTtl,;
*!                                'cOprt_Ctg')
*!*************************************************************
FUNCTION lfVldKey
PARAMETERS lcSrchFile, lcSrchTag, lcBrFields, lcBrFlTtl, lcFldName, lcNtFndMsg
PRIVATE lcCurObj, lnCurObjLn, lnCurAlias, lnCurTag

lnCurAlias = SELECT(0)
SELECT (lcSrchFile)
lnCurTag   = VAL(SYS(21))
SET ORDER TO TAG (lcSrchTag)

lcCurObj     = 'm.' + SYS(18)
lcCurCont = EVALUATE(lcCurObj)
lnCurObjLn  = FSIZE(lcFldName, lcSrchFile)

IF llBrowse .OR. '?' $ lcCurCont
  GO TOP
  IF ARIABROW('', lcBrFlTtl, ;
              gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2)
    &lcCurObj = EVALUATE(lcSrchFile + '.' + lcFldName)
    STORE .F. TO laScrMode
    laScrMode[2] = .T.
  ELSE   
    &lcCurObj = SPACE(lnCurObjLn)
    _CUROBJ   = _CUROBJ
  ENDIF  
ELSE
  IF !EMPTY(lcCurCont) 
    IF !SEEK(lcCurCont)  
      lnSoftSeek = RECNO(0)
      lnOption = gfModalgen("TRM38247B02004","DIALOG",lcNtFndMsg)  
      DO CASE 
        CASE lnOption = 1
          IF BETWEEN(lnSoftSeek, 1, RECCOUNT())
            GO lnSoftSeek
          ELSE
            GO TOP
          ENDIF
          IF ARIABROW('', lcBrFlTtl, ;
                          gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2)
            &lcCurObj = EVALUATE(lcSrchFile + '.' + lcFldName)
            STORE .F. TO laScrMode
            laScrMode[2] = .T.
          ELSE   
            &lcCurObj = SPACE(lnCurObjLn)
            _CUROBJ   = _CUROBJ
          ENDIF  
        CASE lnOption = 2
          STORE .F. TO laScrMode
          laScrMode[4] = .T.
        OTHERWISE
          &lcCurObj = SPACE(lnCurObjLn)
          _CUROBJ   = _CUROBJ
      ENDCASE    
    ELSE
      STORE .F. TO laScrMode
      laScrMode[2] = .T.
    ENDIF  
  ENDIF
ENDIF

llBrowse    = .F.

*N Re Restore environment
SET ORDER TO (lnCurTag)
SELECT (lnCurAlias) 


*!*************************************************************
*! Name      : lfvOprtCtg
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   : Validates a key field.
*!*************************************************************
*! Calls              :  lfVldKey()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvOprtCtg()
*!*************************************************************
FUNCTION lfvOprtCtg

m.cOprt_Ctg = PADR(ALLTRIM(m.cOprt_Ctg),3)
IF llBrowse .OR. !EMPTY(m.cOprt_Ctg) .AND. m.cOprt_Ctg <> lgOldVal
  =lfVldKey('PMCTGHD', 'PMCTGHD', @lcCtgFlds, @lcCtgTtl, 'cOprt_Ctg', 'Category not found.')
  lcOprt_Ctg = m.cOprt_Ctg     
  IF laScrMode[2] OR laScrMode[4] 
    SHOW GETS
  ENDIF  
ENDIF

lcmoprtctg = m.coprt_ctg 
            
*!*************************************************************
*! Name      : lfvCtg_Dsc
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   : Validates a key field.
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvCtg_Dsc()
*!*************************************************************
FUNCTION lfvCtg_Dsc
IF m.cCtg_Dsc <> lgOldVal
  m.cCtg_Dsc = PADR(ALLTRIM(m.cCtg_Dsc), 40)
ENDIF
lcmctg_dsc = m.cctg_dsc 
*!*************************************************************
*! Name      : lpDelScr
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   : Local Delete
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

SELECT PMCTGHD
IF RLOCK()
  REPLACE llok_stat WITH .T.
  UNLOCK
  SELECT PMCTGDT
  SCATTER MEMVAR MEMO BLANK
      
  *-- Scan through all the records of the same Path ID in the details file.
  SCAN FOR coprt_ctg+coprt_id = lcOprt_Ctg
    lcOprt_ID = cOprt_ID
    SELECT PMCTGRL
    SCAN FOR cOprt_Ctg + cOprt_ID = lcOprt_Ctg + lcOprt_ID
      SCATTER TO laDelRec MEMO BLANK
      GATHER FROM laDelRec MEMO
      DELETE
    ENDSCAN
    
    *E302251,1 AMH Delete Notify Setting Records [Start]
    SELECT PMPCTGNT
    IF SEEK(lcOprt_Ctg+lcoprt_id)
      DELETE REST WHILE COprt_Ctg+COprt_ID+CUser_ID = lcOprt_Ctg+lcoprt_id
    ENDIF
    *E302251,1 AMH [End]
    
    SELECT PMCTGDT  
    *-- for every line on the file, delete related predecessors.
    GATHER MEMVAR MEMO
    DELETE
  ENDSCAN
    
  SELECT PMCTGHD
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
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   : Local Save
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpSavScr()
*!*************************************************************
FUNCTION lpSavScr
PRIVATE lcSetDel, lcSetExact

GO TOP IN (lc_PMCtgDt)
IF EOF(lc_PMCtgDt)
  = gfModalgen("TRM38235B00000","DIALOG")
  _CUROBJ = OBJNUM(pbNew)
  llCSave = .F.
  RETURN
ELSE
  *-- Add audit information
  m.cAdd_User  = gcUser_ID
  m.dAdd_Date  = ldCurDate
  m.cAdd_Time  = TIME()

  *-- Save header information
  SELECT PMCTGHD
  IF laScrMode[4]
    INSERT INTO PMCTGHD (cOprt_Ctg, llok_stat) ;
          VALUES (lcOprt_Ctg, .T.)
    STORE .T. TO laScrMode
    laScrMode[1] = .T.
  ELSE
    STORE .T. TO laScrMode
    laScrMode[2] = .T.
  ENDIF
  m.cCtg_Seq  = lcctg_seq        
  GATHER MEMVAR MEMO
  
  *-- Reorganize array elements according to their
  *-- delected sequence
  =lfArangPop('puOprts', @laOperLine)
  *-- Save the selected sequence to the file
  SELECT (lc_PMCtgDt)
  FOR lnPrntSeq = 1 TO ALEN(laOperLine, 1)
    GOTO laOperLine[lnPrntSeq, 1]
    REPLACE cCtg_Seq  WITH lcCtg_Seq,;
            cOprt_Seq WITH PADL(lnPrntSeq, 2, '0') ,;
            cStatus   WITH IIF(cStatus='S', 'M', cStatus)
  ENDFOR

  *-- Save details
  lcSetDel   = SET('DELETED')
  lcSetExact = SET('EXACT')

  SET DELETED OFF
  SET EXACT OFF

  GO TOP
  *-- Go through all the records of the temp cursor.
  SCAN
    DO CASE
      *-- Case of a new line
      CASE cStatus = 'A'        
        SCATTER MEMVAR MEMO

        SELECT PMCTGDT
        IF SEEK(' ')
          =RLOCK()
          RECALL         
          GATHER MEMVAR MEMO
          UNLOCK
        ELSE
          INSERT INTO PMCTGDT FROM MEMVAR
        ENDIF  

      *-- An old line that is modified in the current session.
      CASE cStatus = 'M'        
        *-- Add audit information
        SCATTER MEMVAR MEMO
        m.cAdd_User  = gcUser_ID
        m.dAdd_Date  = ldCurDate
        m.cAdd_Time  = TIME()
        SELECT PMCTGDT
        IF BETWEEN(&lc_PMCtgDt..nRecNo, 1, RECCOUNT())
          GO &lc_PMCtgDt..nRecNo
        ENDIF  
        GATHER MEMVAR MEMO
          
      *-- An old line that is deleted in the current session.
      CASE cStatus = 'D'        
        SELECT PMCTGDT
        IF BETWEEN(&lc_PMCtgDt..nRecNo, 1, RECCOUNT())
          GO &lc_PMCtgDt..nRecNo
        ENDIF  

        SCATTER MEMVAR MEMO BLANK
        =RLOCK() 
        GATHER MEMVAR MEMO
        DELETE
        UNLOCK
    ENDCASE  

    ** Update related records for every operation.
    SELECT (lc_PMCtgRl)
    IF SEEK(&lc_PMCtgDt..cOprt_ID)
      SCAN REST WHILE cOprt_ID = &lc_PMCtgDt..cOprt_ID
        DO CASE
          *-- Case of a new line
          CASE cStatus = 'A'        
            SCATTER MEMVAR MEMO

            SELECT PMCTGRL
            IF SEEK(' ')
              =RLOCK()
              RECALL         
              GATHER MEMVAR MEMO
              UNLOCK
            ELSE
              INSERT INTO PMCTGRL FROM MEMVAR
            ENDIF  

          *-- An old line that is modified in the current session.
          CASE cStatus = 'M'        
            * Add audit information
            SCATTER MEMVAR MEMO
            m.cAdd_User  = gcUser_ID
            m.dAdd_Date  = ldCurDate
            m.cAdd_Time  = TIME()
            SELECT PMCTGRL
            IF BETWEEN(&lc_PMCtgRl..nRecNo, 1, RECCOUNT())
              GO &lc_PMCtgRl..nRecNo
            ENDIF  

            GATHER MEMVAR MEMO
          
          *-- An old line that is deleted in the current session.
          CASE cStatus = 'D'        
            SELECT PMCTGRL
            IF BETWEEN(&lc_PMCtgRl..nRecNo, 1, RECCOUNT())
              GO &lc_PMCtgRl..nRecNo
            ENDIF  
            SCATTER MEMVAR MEMO BLANK
            GATHER MEMVAR MEMO
            DELETE
        ENDCASE  
      ENDSCAN  
    ENDIF  
    
    *E302251,1 AMH Update Notify Setting for each operation [Start]
    SELECT (lcPMPCTGNT)
    IF SEEK(EVALUATE(lc_PMCtgDt+".COprt_Ctg+"+lc_PMCtgDt+".cOprt_ID"))
      SCAN REST WHILE COprt_Ctg+cOprt_ID = EVALUATE(lc_PMCtgDt+".COprt_Ctg+"+lc_PMCtgDt+".cOprt_ID")
        DO CASE
          *-- Case of a new line
          CASE cStatus = 'A'        
            SCATTER MEMVAR

            SELECT PMPCTGNT
            IF SEEK(' ')
              =RLOCK()
              RECALL         
              GATHER MEMVAR
              UNLOCK
            ELSE
              INSERT INTO PMPCTGNT FROM MEMVAR
            ENDIF  

          *-- An old line that is modified in the current session.
          CASE cStatus = 'M'        
            * Add audit information
            SCATTER MEMVAR
            m.cAdd_User  = gcUser_ID
            m.dAdd_Date  = ldCurDate
            m.cAdd_Time  = TIME()
            SELECT PMPCTGNT
            IF SEEK(m.COprt_Ctg+m.COprt_ID+m.CUser_ID)
              GATHER MEMVAR
            ENDIF  
          
          *-- An old line that is deleted in the current session.
          CASE cStatus = 'D'        
            SCATTER MEMVAR
            SELECT PMPCTGNT
            IF SEEK(m.COprt_Ctg+m.COprt_ID+m.CUser_ID)
              DELETE
            ENDIF
        ENDCASE
      ENDSCAN
    ENDIF
    *E302251,1 AMH [End]
    
    SELECT (lc_PMCtgDt)
  ENDSCAN
  
  SET DELETED &lcSetDel
  SET EXACT   &lcSetExact

  *-- Back to 'S'elect mode.
  SELECT PMCTGHD
  REPLACE llok_stat WITH .F.
  IF llPramPasd 
    llCatgrAdd = .T.
    CLEAR READ
  ELSE
    SHOW GETS
  ENDIF  
ENDIF


*!*************************************************************
*! Name      : lfwLsOprts
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   : WHEN function of list lsOprts
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwLsOprts()
*!*************************************************************
FUNCTION lfwLsOprts

IF CNTBAR('puOprts') > 0
  GOTO laOperLine[GETBAR('puOprts', lsOperLins), 1] IN (lc_PMCtgDt)
  =lfvOpertion(!llAddOprt .AND. (laScrMode[3] OR laScrMode[4]) .OR.;
                  llAddOprt  .AND. &lc_PMCtgDt..cOprt_ID = lcNewOprt)
ELSE
  RETURN .F.
ENDIF

lnOperLins = lsOperLins 

*!*************************************************************
*! Name      : lfvNew
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
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

STORE laUpdtMthd[1] TO lcUpdtMthd, puUpdtMthd

*-- Save the current operation ID
SELECT (lc_PMCtgDt)
lcOldOprt  = cOprt_ID
GO BOTTOM
IF !EOF()
  SKIP
ENDIF
=lfvOpertion()
SHOW GET pbNew     DISABLE
IF !llAddOprt
  lcOprt_id  = SPACE(5)  
  SHOW GET lcOprt_ID ENABLE
ELSE
  SHOW GET lcOprt_ID DISABLE
  =lfvOprt_ID()
ENDIF
_CUROBJ    = OBJNUM(lcoprt_id)
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvRemove
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
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

SET ORDER TO TAG PMCTGRLP IN (lc_PMCtgRl) 
*-- If deletion of the line is confirmed, continue
IF SEEK(lcOprt_ID, lc_PMCtgRl)
  llMayDelte = gfModalgen("TRM38234B38006","DIALOG",'Remove') = 1
ELSE
  llMayDelte = gfModalgen("TRM38208B38006","DIALOG") = 1                       
ENDIF                       
                         
IF llMayDelte
  glUpdated   = .T.
  *-- Delete related lines in (lc_PMCtgDt) file
  SELECT (lc_PMCtgRl)
  IF SEEK(lcOprt_ID)
    SCAN REST WHILE cPrd_ID = lcOprt_ID
      REPLACE cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1)
      DELETE
    ENDSCAN
  ENDIF  
  SET ORDER TO TAG PMCTGRL
  SCAN FOR coprt_id = lcOprt_ID
    REPLACE cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1)
    DELETE
  ENDSCAN
  
  *E302251,1 AMH Delete Notify Setting Records [Start]
  SELECT (lcPMPCTGNT)
  IF SEEK(lcOprt_Ctg+lcoprt_id)
    SCAN REST WHILE COprt_Ctg+COprt_ID+CUser_ID = lcOprt_Ctg+lcoprt_id
      REPLACE cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1)
      DELETE
    ENDSCAN
  ENDIF
  *E302251,1 AMH [End]
  
  SELECT (lc_PMCtgDt)

  *-- Adjust status of the line to 'D' for old entris,
  *-- 'S' for a new entry, then delete the record.
  REPLACE cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1)
  DELETE
  LOCATE
  IF EOF()
    GO TOP
  ENDIF  

  lnNOfLines = lnNOfLines - 1
  *-- Remove the element from the list popup
  =lfArangPop('puOprts', @laOperLine)      
  RELEASE BAR ALL OF puOprts
  =ADEL(laOperLine , lsOperLins)
  IF lnNOfLines >= 1
    DIMENSION laOperLine[ALEN(laOperLine,1)-1, 3]
    =lfFillPop()
    IF lsOperLins > CNTBAR('puOprts')
      lsOperLins = CNTBAR('puOprts')
    ENDIF  
  ELSE
    lcOprt_ID = SPACE(5)
    DIMENSION laOperLine[1,3] 
    STORE '' TO laOperLine 
    lsOperLins  = 1
  ENDIF  
  =lfvOpertion(!EOF())
  IF CNTBAR('puOprts') > 0
    GOTO laOperLine[GETBAR('puOprts', lsOperLins), 1] IN (lc_PMCtgDt)
    =lfvOpertion(.T.)
  ELSE
    =lfvOpertion(.F.)
  ENDIF   
  SHOW GET lsOperLins
ENDIF
SET ORDER TO TAG PMCTGRL IN (lc_PMCtgRl) 
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvOprt_ID
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   : Validates a key field.
*!*************************************************************
*! Calls              : lfvOeprations 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvOprt_ID()
*!*************************************************************
FUNCTION lfvOprt_ID
PRIVATE lnCurAlias, lnRecNo0

lnCurAlias = SELECT(0)
SELECT (lc_PMCtgDt)

lcOprt_ID    = PADR(ALLTRIM(lcOprt_ID), 5)  
*-- If the entry is left empty, 
IF EMPTY(lcOprt_ID)
  *-- Display the previous status
  SET ORDER TO TAG PMCTGDT
  =lfvOpertion(SEEK(lcOldOprt))
*-- ELse, validate as follows
ELSE
  IF CNTBAR('puOprts') > 0
    SET MARK OF BAR GETBAR('puOprts', lsOperLins) OF puOprts TO .F.
  ENDIF  
  SET ORDER TO TAG PMCTGDT
  IF !llAddOprt .AND. SEEK(lcOprt_ID, lc_PMCtgDt) 
    lsOperLins = GETBAR('puOprts', ASUBSCRIPT(laOperLine, ASCAN(laOperLine, lcOprt_ID), 1))
  ELSE
    INSERT INTO (lc_PMCtgDt);
            (cOprt_Ctg , cOprt_ID , cStatus  , cAdd_User, dAdd_Date,;
             cAdd_Time, lShw2Cust, cUpdtMthd  );
     VALUES(lcOprt_Ctg, lcOprt_ID, 'A'      , gcUser_ID  , ldCurDate,;
               TIME()   , .T.      , laUpdtMthd[1])
    glUpdated = .T.
    *-- Add the new entry to the available predecessors.
    lnNOfLines = lnNOfLines + 1
    IF !EMPTY(laOperLine[1, 2])
      DECLARE laOperLine[lnNOfLines,3]
    ENDIF  
    laOperLine[ALEN(laOperLine,1),1] = RECNO(lc_PMCtgDt)
    laOperLine[ALEN(laOperLine,1),2] = lcOprt_ID
    laOperLine[ALEN(laOperLine,1),3] = ' '
                                         
    *-- Refresh popup
    DEFINE BAR CNTBAR('puOprts') + 1 OF puOprts ;
         PROMPT lcOprt_ID
    lsOperLins = CNTBAR('puOprts')
  ENDIF
  SHOW GET lsOperLins
  =lfvOpertion(.T.)) 
ENDIF
SET ORDER TO TAG PMCTGDTS
SELECT (lnCurAlias) 

*!*************************************************************
*! Name      : lfvShw2Cst
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   : Validates Show to customer check box.(lShw2Cust)
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

m.lShw2Cust = IIF(lnShw2Cust = 1,.T.,.F.)

REPLACE &lc_PMCtgDt..lShw2Cust WITH m.lShw2Cust,;
        &lc_PMCtgDt..cStatus   WITH SUBSTR('MMA', AT(&lc_PMCtgDt..cStatus, 'SMA'), 1),;
        &lc_PMCtgDt..cAdd_User WITH gcUser_ID,;
        &lc_PMCtgDt..dAdd_Date WITH ldCurDate,;
        &lc_PMCtgDt..cAdd_Time WITH TIME()
        
glUpdated = .T.


*!*************************************************************
*! Name      : lfvDetails
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
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
*!Modifications:
*!*************************************************************
FUNCTION lfvDetails
PRIVATE lcCurObj, lnCurAlias

lcCurFld   = SYS(18)
lcCurObj   = 'm.' + lcCurFld
lcSetExact = SET('EXACT')
SET EXACT ON

*AMH
lcOldBrFld = lcBrFields
*AMH

IF lcCurFld = 'LCOPRT_RES'
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
  &lcCurObj  = PADR(ALLTRIM(EVALUATE(lcCurObj)), FSIZE(lcCurFld, 'PMCTGDT'))
  lcField    = lcCurFld
ENDIF

*AMH
lcBrFields = lcOldBrFld
*AMH

SET EXACT &lcSetExact


IF LCOPRT_RES  <> lgOldVal
  lnCurAlias = SELECT(0)
  SELECT (lc_PMCtgDt)
  REPLACE COPRT_RES  WITH ' ',;
          CGROUP_ID  WITH ' '  
  REPLACE &lcField  WITH IIF(lcCurFld = 'LCOPRT_RES',LCOPRT_RES,EVALUATE(lcCurObj)),;
          cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1),;
          cAdd_User WITH gcUser_ID,;
          dAdd_Date WITH ldCurDate,;
          cAdd_Time WITH TIME()
  glUpdated = .T.
  lnBarNo = GETBAR('puOprts', lsOperLins)
  laOperLine[lnBarNo, 3] = m.cOprt_Dsc + ' '+ IIF(EMPTY(LCOPRT_RES),SPACE(10),LCOPRT_RES) +SPACE(9)+STR(m.nEst_Dur,3)


  DEFINE BAR lnBarNo OF puOprts;
         PROMPT laOperLine[lnBarNo, 2] + ' ' + laOperLine[lnBarNo, 3]
  SHOW GET lsOperLins       
  SELECT (lnCurAlias)
ENDIF

lcmoprtdsc = m.coprt_dsc 

*!*************************************************************
*! Name      : lfvDurtion
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   : Validates duration fields
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvDurtion()
*!*************************************************************
*!Modifications:
*!*************************************************************

FUNCTION lfvDurtion
lnCurAlias = SELECT(0)

SELECT (lc_PMCtgDt)
IF lgOldVal <> m.nEst_Dur
  IF m.nEst_Dur < 0 .AND. gfModalgen("TRM38215B00000","DIALOG") = 1 
    m.nEst_Dur = lgOldVal
    _CUROBJ = OBJNUM(m.nEst_Dur)
  ELSE
    REPLACE nEst_Dur  WITH m.nEst_Dur,;
            cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1),;
            cAdd_User WITH gcUser_ID,;
            dAdd_Date WITH ldCurDate,;
            cAdd_Time WITH TIME()
  lnBarNo = GETBAR('puOprts', lsOperLins)
  laOperLine[lnBarNo, 3] = m.cOprt_Dsc + ' ' + IIF(EMPTY(LCOPRT_RES),SPACE(10),LCOPRT_RES) +SPACE(9)+STR(m.nEst_Dur,3)
  DEFINE BAR lnBarNo OF puOprts;
         PROMPT laOperLine[lnBarNo, 2] + ' ' + laOperLine[lnBarNo, 3]
  SHOW GET lsOperLins       
    glUpdated = .T.
  ENDIF  
ENDIF
SELECT (lnCurAlias)
lnmest_dur = m.nest_dur  

*!*************************************************************
*! Name      : lfvPredcsr
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   : Valid function for push button < Predecessors >
*!             (pbPredecsr)
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

DECLARE laPredOprs[ALEN(laOperLine,1)]
FOR lnCount = 1 TO ALEN(laOperLine,1)
  laPredOprs[lnCount] = laOperLine[lnCount, 2] + ' ' + ;
                  SUBSTR(laOperLine[lnCount, 3], 1, 20)
ENDFOR                  

*-- Look for the current operations from the operations array
lnElement = ASCAN(laPredOprs, ALLTRIM(lcOprt_ID+ ' ' + SUBSTR(m.cOprt_Dsc, 1, 20)))
*-- If found, 
IF lnElement > 0
  *-- If the element is deleted succesfully, redimension the operations
  *-- array
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
  SELECT cPrd_ID + ' ' + SUBSTR(cOprt_Dsc,1, 20);
    FROM PMCTGRL, PMCTGDT;
     WHERE PMCTGRL.cOprt_Ctg + PMCTGRL.cOprt_ID = lcOprt_Ctg + lcOprt_ID;
      .AND. PMCTGRL.cOprt_Ctg + PMCTGRL.cPrd_ID  = PMCTGDT.cOprt_Ctg + PMCTGDT.cOprt_ID;
    INTO ARRAY laPredecsr 
  
  IF _TALLY = 0
    DIMENSION laPredecsr[1]
    laPredecsr = ''
  ENDIF
  =gfMover(@laPredOprs,@laPredecsr,;
           ALLTRIM(lcOprt_ID) + ' ' + ALLTRIM(m.coprt_dsc) +;
           ' Predecessors')
ELSE
  SELECT cPrd_ID + ' ' + SUBSTR(cOprt_Dsc,1, 20);
    FROM (lc_PMCtgRl), (lc_PMCtgDt) ;
      WHERE &lc_PMCtgRl..cOprt_Ctg + &lc_PMCtgRl..cOprt_ID = lcOprt_Ctg + lcOprt_ID;
   .AND. &lc_PMCtgRl..cOprt_Ctg + &lc_PMCtgRl..cPrd_ID = &lc_PMCtgDt..cOprt_Ctg + &lc_PMCtgDt..cOprt_ID;
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
  SELECT (lc_PMCtgRl)
  SET ORDER TO TAG PMCTGRLP 
  *-- If the current operation is found as a predecessor to
  *-- others, get all the operations on all available paths
  *-- into an array.
  lnCount = 0  
  IF SEEK(lcOprt_ID)
    SCAN REST WHILE cPrd_ID = lcOprt_ID
      lnCount = lnCount + 1  
      DIMENSION laPathElems[lnCount]
      laPathElems[lnCount] = cOprt_ID
    ENDSCAN
 
    *-- Collect the rest of the operations in the array
    lnNxtCount = 0
    DO WHILE lnNxtCount < ALEN(laPathElems)
      lnNxtCount = lnNxtCount + 1 
      lcNxtOprt  = laPathElems[lnNxtCount]
      IF SEEK(lcNxtOprt)
        SCAN REST WHILE cPrd_ID = lcNxtOprt
          DIMENSION laPathElems[ALEN(laPathElems) + 1]
          laPathElems[ALEN(laPathElems)] = cOprt_ID
        ENDSCAN
      ENDIF     &&ENDIF SEEK(lcNxtOprt)
    ENDDO    &&ENDDO WHILE lnNxtCount < ALEN(laPathElems)
  ENDIF     &&ENDIF SEEK(lcOprt_ID)  
  SET ORDER TO TAG PMCTGRL
  IF llAddOprt .AND. &lc_PMCtgDt..cOprt_ID <> lcNewOprt
    =gfMover(@laPredOprs,@laPredecsr,;
             ALLTRIM(lcOprt_ID) + ' ' + ALLTRIM(m.coprt_dsc) +;
             ' Predecessors')
  ELSE
    lcOldPred = lcPredec
    lcVldPrd = 'lfVldPrd(SUBSTR(IIF(lsSource = 0,laSource[1],laSource[lsSource]), 1, 5), _CUROBJ = OBJNUM(pbAll))'
    =gfMover(@laPredOprs, @laPredecsr,;
             'Select Predecessors for ' + ALLTRIM(lcOprt_ID) + ' ' + ALLTRIM(m.coprt_dsc),;
             .T.,@lcVldPrd,.T.,.T.)
    SELECT (lc_PMCtgRl)
    IF !EMPTY(laPredecsr[1]) 
      lnDelElems = ALEN(laPredecsr)
      *-- Unformat the predecessors array elements    
      FOR lnCount = 1 TO lnDelElems
        laPredecsr[lnCount] = SUBSTR(laPredecsr[lnCount], 1, 5)
      ENDFOR 

      IF SEEK(lcOprt_ID)
        SCAN REST WHILE cOprt_ID = lcOprt_ID
          lnelement  = ASCAN(laPredecsr, cPrd_ID)
          IF lnelement > 0 
            =ADEL(laPredecsr, lnElement)
            lnDelElems = lnDelElems - 1
          ELSE
            REPLACE cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1)
            DELETE
          ENDIF
        ENDSCAN
        FOR lnCount = 1 TO lnDelElems
          INSERT INTO (lc_PMCtgRl);
                     ( cOprt_Ctg, cOprt_ID,  cPrd_ID, ;
                       cAdd_User, dAdd_Date, cAdd_Time, cStatus);
               VALUES(lcOprt_Ctg, lcOprt_ID, laPredecsr[lnCount],;
                       gcUser_ID  , ldCurDate, TIME()   , 'A')
        ENDFOR
      ELSE
        FOR lnCount = 1 TO ALEN(laPredecsr)
          INSERT INTO (lc_PMCtgRl);
                     ( cOprt_Ctg, cOprt_ID,  cPrd_ID, ;
                       cAdd_User, dAdd_Date, cAdd_Time, cStatus);
               VALUES(lcOprt_Ctg, lcOprt_ID, laPredecsr[lnCount],;
                       gcUser_ID  , ldCurDate, TIME()   , 'A')
        ENDFOR
      ENDIF
    ELSE
      IF SEEK(lcOprt_ID)
        SCAN REST WHILE cOprt_ID = lcOprt_ID
          REPLACE cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1)
          DELETE
        ENDSCAN
      ENDIF  
    ENDIF  

    lcPredec   = lfGetPred()
    glUpdated  = .T.
    SHOW GET lcPredec
  ENDIF  
ENDIF  

SET PROCEDURE TO &lcSetProc
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfGetPred
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
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
PRIVATE lcPrdStrng, lcExpToSeek, lnCurAlias
lnCurAlias = SELECT(0)
lcPrdStrng = ''
IF laScrMode[2]
  SELECT PMCTGRL
  lcExpToSeek = lcOprt_Ctg + lcOprt_ID
  lcCond      = 'cOprt_Ctg + cOprt_ID'
ELSE
  SELECT (lc_PMCtgRl)
  lcExpToSeek = &lc_PMCtgDt..cOprt_ID  
  lcCond      = 'cOprt_ID'
ENDIF    
IF SEEK(lcExpToSeek)
  SCAN REST WHILE &lcCond = lcExpToSeek
    lcPrdStrng = lcPrdStrng + ALLTRIM(cPrd_ID) +', '
  ENDSCAN
ENDIF
SELECT (lnCurAlias)
RETURN IIF(!EMPTY(lcPrdStrng),;
           SUBSTR(lcPrdStrng, 1, RAT(',', lcPrdStrng)-1) + '.', SPACE(40)) 
           
           
*!*************************************************************
*! Name      : lfvUpdMthd
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
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

REPLACE &lc_PMCtgDt..cUpdtMthd WITH lcUpdtMthd,;
        &lc_PMCtgDt..cStatus   WITH SUBSTR('MMA', AT(&lc_PMCtgDt..cStatus, 'SMA'), 1),;
        &lc_PMCtgDt..cAdd_User WITH gcUser_ID,;
        &lc_PMCtgDt..dAdd_Date WITH ldCurDate,;
        &lc_PMCtgDt..cAdd_Time WITH TIME()
glUpdated   = .T.

lnUpdtMthd = puUpdtMthd  

*!*************************************************************
*! Name      : lfvNotify
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
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

IF !lcNotify == lgOldVal
  lnCurAlias = SELECT(0)
  SELECT (lc_PMCtgDt)
  REPLACE mNotify   WITH STRTRAN(ALLTRIM(lcNotify), ',','|'),;
          cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1),;
          cAdd_User WITH gcUser_ID,;
          dAdd_Date WITH ldCurDate,;
          cAdd_Time WITH TIME()
  glUpdated   = .T.
  SELECT(lnCurAlias)
ENDIF

*!*************************************************************
*! Name      : lfvNotfyBt
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
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

lcSetProc = SET('PROCEDURE')
SET PROCEDURE TO
lnCurAlias = SELECT(0)
SELECT (lc_PMCtgDt)

*E302251,1 AMH Calling the Notify Seting screen [Start]
*DIME laUsers[1]
*SELECT cUser_ID ;
*  FROM (gcSysHome + 'SYUUSER');
*  INTO ARRAY laUsers;
*  ORDER BY cUser_ID
*
*=gfStr2Ar(ALLTRIM(mNotify), @laNotified, '|')
*IF laScrMode[2] .OR. llAddOprt .AND. cOprt_ID <> lcNewOprt
*  =gfMover(@laUsers,@laNotified,"Users to Notify",.F.)
*ELSE
*  SELECT cUser_ID ;
*    FROM (gcSysHome + 'SYUUSER');
*    INTO ARRAY laUsers;
*    ORDER BY cUser_ID
*
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
*
*  lcNotify   = IIF(!EMPTY(lcNotify), STRTRAN(lcNotify, '|', ', ') + '.', '')
*  glUpdated  = .T.
*  SHOW GET lcNotify
*ENDIF
SELECT (lcPMPCTGNT)
SET KEY TO lcOprt_Ctg+lcoprt_id
LOCATE
lnBrRecNo = RECNO()
SCATTER MEMVAR

PUSH KEY
ON KEY LABEL ALT+H ACTIVATE WINDOW (lcBrUser)
ON KEY LABEL ESC DO lfEsc
DO (gcScrDir +gcWinAppl + '\SMNTFSET.SPX')
POP KEY

IF !laScrMode[2] .AND. !(llAddOprt .AND. cOprt_ID <> lcNewOprt)
  lcNotify   = ''
  SELECT (lcPMPCTGNT)
  SCAN FOR !EMPTY(cUser_ID)
    lcNotify = lcNotify + ALLTRIM(cUser_ID) + '|'
  ENDSCAN
  IF !EMPTY(lcNotify)
    lcNotify = SUBSTR(lcNotify, 1, RAT('|', lcNotify)-1) 
  ENDIF
  SELECT (lc_PMCtgDt)
  REPLACE mNotify   WITH ALLTRIM(lcNotify),;
          cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1),;
          cAdd_User WITH gcUser_ID,;
          dAdd_Date WITH ldCurDate,;
          cAdd_Time WITH TIME()

  lcNotify   = IIF(!EMPTY(lcNotify), STRTRAN(lcNotify, '|', ', ') + '.', '')
  glUpdated  = .T.
  SHOW GET lcNotify
ENDIF

SELECT (lcPMPCTGNT)
SET KEY TO
*E302251,1 AMH [End]

SET PROCEDURE TO &lcSetProc
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvNotes
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   : Valid function for push button < Notes... >
*!*************************************************************
*! Calls              : AriaZoom()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvNotes()
*!*************************************************************
FUNCTION lfvNotes
PRIVATE lnCurAlias
lnCurAlias = SELECT(0)

SELECT (lc_PMCtgDt)
=gfZoom('mOprt_Com')
REPLACE &lc_PMCtgDt..cStatus   WITH SUBSTR('MMA', AT(&lc_PMCtgDt..cStatus, 'SMA'), 1),;
        &lc_PMCtgDt..cAdd_User WITH gcUser_ID,;
        &lc_PMCtgDt..dAdd_Date WITH ldCurDate,;
        &lc_PMCtgDt..cAdd_Time WITH TIME()
SELECT (lnCurAlias)        
glUpdated = .T.

*!*************************************************************
*! Name      : lfVldPrd
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
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
PARAMETERS lcArrOprt, llMoveAll

IF _CUROBJ = OBJNUM(lsTarget) .OR. (TYPE("lnMovmnts")="N" .AND. (lnMovmnts = 3 OR lnMovmnts = 4))
  RETURN
ELSE
  SET PROCEDURE TO &lcSetProc
  lnRetVal = IIF(ASCAN(laPathElems, lcArrOprt) = 0, .T.,;
             IIF( gfModalgen("TRM38214B00000","DIALOG",ALLTRIM(lcArrOprt)) = 1,;
                               .F., .T.))

  IF !EMPTY(laTarget)
    lcPrevOrd = ORDER(lc_PMCtgRl)                               
    IF llMoveAll 
      SET ORDER TO PMCTGRL IN (lc_PMCtgRl)
      IF lnRetVal AND (SEEK(lcOprt_ID,lc_PMCtgRl) AND;
                       SEEK(&lc_PMCtgRl..Cprd_ID,lc_PMCtgRl)) AND lcArrOprt = &lc_PMCtgRl..Cprd_ID
        =gfModalgen("TRM38241B00000","DIALOG",;
                     ""+"|"+lcArrOprt+"|"+""+"|"+lcOprt_ID+"|"+ALLTRIM(m.cOprt_dsc))
        lnRetVal = .F.
      ELSE
        SET ORDER TO PMCTGRLP IN (lc_PMCtgRl)
        IF lnRetVal AND SEEK(lcArrOprt,lc_PMCtgRl) AND ASCAN(laTarget,&lc_PMCtgRl..COprt_ID) <> 0)      
        =gfModalgen("TRM38241B00000","DIALOG",;
                     ""+"|"+lcArrOprt+"|"+""+"|"+lcOprt_ID+"|"+ALLTRIM(m.cOprt_dsc))
          lnRetVal = .F.
        ELSE
          SET ORDER TO PMCTGRL IN (lc_PMCtgRl)
          IF lnRetVal AND SEEK(lcArrOprt,lc_PMCtgRl) AND ASCAN(laTarget,&lc_PMCtgRl..cPrd_ID) <> 0)
            =gfModalgen("TRM38242B00000","DIALOG",;
            ""+"|"+lcOprt_ID+"|"+ALLTRIM(m.cOprt_dsc)+"|"+""+"|"+lcArrOprt)
            lnRetVal = .F.
          ENDIF
        ENDIF
      ENDIF 
    ELSE
      SET ORDER TO PMCTGRL IN (lc_PMCtgRl)
      IF lnRetVal AND (SEEK(lcOprt_ID,lc_PMCtgRl) AND;
                       SEEK(&lc_PMCtgRl..Cprd_ID,lc_PMCtgRl)) AND lcArrOprt = &lc_PMCtgRl..Cprd_ID
        =gfModalgen("TRM38243B00000","DIALOG",;
         ""+"|"+lcOprt_ID+"|"+ALLTRIM(m.cOprt_dsc))
        lnRetVal = .F.
      ELSE
        SET ORDER TO PMCTGRLP IN (lc_PMCtgRl)
        IF lnRetVal AND SEEK(lcArrOprt,lc_PMCtgRl) AND ASCAN(laTarget,&lc_PMCtgRl..COprt_ID) <> 0)      
          =gfModalgen("TRM38243B00000","DIALOG",;
           ""+"|"+lcOprt_ID+"|"+ALLTRIM(m.cOprt_dsc))
          lnRetVal = .F.
        ELSE
          SET ORDER TO PMCTGRL IN (lc_PMCtgRl)
          IF lnRetVal AND SEEK(lcArrOprt,lc_PMCtgRl) AND ASCAN(laTarget,&lc_PMCtgRl..cPrd_ID) <> 0)
            =gfModalgen("TRM38244B00000","DIALOG",;
             ""+"|"+lcOprt_ID+"|"+ALLTRIM(m.cOprt_dsc))
            lnRetVal = .F.
          ENDIF
        ENDIF
      ENDIF            
    ENDIF
    SET ORDER TO &lcPrevOrd IN (lc_PMCtgRl)             
  ENDIF
  SET PROCEDURE TO 
  RETURN lnRetVal
ENDIF

*!*************************************************************
*! Name      : lfFillPop
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   : Fills popup bars from array
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfFillPop()
*!*************************************************************
FUNCTION lfFillPop
FOR lnCount = 1 to ALEN(laOperLine,1)
  DEFINE BAR lnCount OF puOprts ;
         PROMPT laOperLine[lnCount,2] + ' ' + laOperLine[lnCount,3]
ENDFOR

*!*************************************************************
*! Name      : lfvOpertion
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   : Valid function for push button < Operations > (pbOprs)
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvOpertion()
*!*************************************************************
FUNCTION lfvOpertion
PARAMETERS llEnablAll
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)
SELECT (lc_PMCtgDt)
SET ORDER TO TAG PMCTGDTS

SCATTER MEMVAR MEMO

IF EOF()
  STORE SPACE(40) TO lcNotify, lcPredec
  STORE laUpdtMthd[1] TO lcUpdtMthd, puUpdtMthd
ELSE  
  lcOprt_ID  = cOprt_ID

  lcNotify   = IIF(!EMPTY(laUsers[1]), IIF(!EMPTY(ALLTRIM(mNotify)),;
                    STRTRAN(ALLTRIM(mNotify), '|',', ') + '.', ''),;
                    ALLTRIM(mNotify))
  lcPredec   = lfGetPred()
  STORE cUpdtMthd TO lcUpdtMthd, puUpdtMthd
ENDIF

PuUser     = IIF(EMPTY(coprt_res) AND EMPTY(cGroup_Id),'User',;
             IIF(EMPTY(coprt_res),'Group of users','User'))
lcoprt_res = IIF(EMPTY(coprt_res),cGroup_id,coprt_res)
lnShw2Cust = IIF(m.lShw2Cust,1,0)

IF llEnablAll
  SHOW GET lcOprt_ID   DISABLE
  *-- Add calendars
  SHOW GET ibCal_ID   ENABLE
  SHOW GET m.cCal_ID  ENABLE
  SHOW GET lcoprt_res  ENABLE
  SHOW GET PuUser      ENABLE
  SHOW GET m.cOprt_dsc ENABLE 
  SHOW GET ibUpdtMthd  ENABLE 
  SHOW GET puUpdtMthd  ENABLE 
  SHOW GET lnShw2Cust  ENABLE 
  SHOW GET m.nest_dur  ENABLE 
  
  IF !llAddOprt
    SHOW GET pbNew       ENABLE 
    SHOW GET pbRemove    ENABLE 
  ENDIF
  SHOW GET pbNotes     ENABLE 
  IF ALEN(laOperLine, 1) > 1
    SHOW GET pbPredecsr ENABLE
    SHOW GET lcPredec    ENABLE 
  ELSE
    SHOW GET pbPredecsr DISABLE
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
  IF EOF(lc_PMCtgDt)
    SHOW GET pbNotes     DISABLE
    SHOW GET pbNotify    DISABLE
    SHOW GET lcNotify    DISABLE
    SHOW GET pbPredecsr DISABLE
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
    
    IF ALEN(laOperLine, 1) > 1
      SHOW GET pbPredecsr ENABLE
      SHOW GET lcPredec    ENABLE 
    ELSE
      SHOW GET pbPredecsr DISABLE
      SHOW GET lcPredec    DISABLE
    ENDIF
  ENDIF   
  
  SHOW GET lcOprt_ID     DISABLE  
  *-- Add calendars
  SHOW GET ibCal_ID      DISABLE  
  SHOW GET m.cCal_ID     DISABLE  
  SHOW GET m.cOprt_dsc   DISABLE
  SHOW GET lcoprt_res    DISABLE
  SHOW GET PuUser        DISABLE  
  SHOW GET ibUpdtMthd    DISABLE
  SHOW GET puUpdtMthd    DISABLE
  SHOW GET lnShw2Cust    DISABLE
  SHOW GET m.nest_dur    DISABLE
  SHOW GET pbRemove      DISABLE
  IF (laScrMode[3] OR laScrMode[4]) .AND. !llAddOprt
    SHOW GET pbNew      ENABLE 
  ELSE 
    SHOW GET pbNew       DISABLE
  ENDIF  
  
ENDIF


SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvCtg_Seq
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   : Valid function for category print sequence field
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvCtg_Seq()
*!*************************************************************
FUNCTION lfvCtg_Seq
IF lcctg_seq  <> lgOldVal
  lcctg_seq  = IIF(EMPTY(lcctg_seq), lgOldVal, PADL(ALLTRIM(lcctg_seq), 2, '0'))   
ENDIF


*!*************************************************************
*! Name      : lfvCal_ID                        
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   : Validates get field m.cCal_ID
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
llBrowse  = llBrowse .OR. '?' $ m.cCal_ID
lcSetExact = SET('EXACT')
SET EXACT ON
m.cCal_ID = PADR(ALLTRIM(m.cCal_ID),4)
SHOW GET m.cCal_ID

SELECT PMCALHD

IF !llBrowse 
  IF !EMPTY(m.cCal_ID) .AND. !SEEK(m.cCal_ID, 'PMCALHD') 
    IF BETWEEN(RECNO(0), 1, RECCOUNT())
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF
    llBrowse = .T.    
  ELSE
    REPLACE &lc_PMCtgDt..cCal_ID   WITH m.cCal_ID,;
            &lc_PMCtgDt..cStatus   WITH SUBSTR('MMA', AT(&lc_PMCtgDt..cStatus, 'SMA'), 1),;
            &lc_PMCtgDt..cAdd_User WITH gcUser_ID,;
            &lc_PMCtgDt..dAdd_Date WITH ldCurDate,;
            &lc_PMCtgDt..cAdd_Time WITH TIME()
    glUpdated = .T.                    
  ENDIF
ELSE
  GO TOP  
ENDIF 
IF llBrowse  
  DIMENSION laSelected[1]
  lcBrFields = lcCalFlds
  IF ARIABROW('', @lcCalTtl,; 
              gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,;
              .F.,.F.,"cCal_ID","laSelected")
    m.cCal_ID = laSelected[1]
    REPLACE &lc_PMCtgDt..cCal_ID   WITH m.cCal_ID,;
            &lc_PMCtgDt..cStatus   WITH SUBSTR('MMA', AT(&lc_PMCtgDt..cStatus, 'SMA'), 1),;
            &lc_PMCtgDt..cAdd_User WITH gcUser_ID,;
            &lc_PMCtgDt..dAdd_Date WITH ldCurDate,;
            &lc_PMCtgDt..cAdd_Time WITH TIME()
    glUpdated = .T.        
  ELSE
    m.cCal_ID = lgOldVal
  ENDIF           
ENDIF
llBrowse = .F.
SET EXACT &lcSetExact
lcmccal_id = m.ccal_id   
SELECT (lnCurAlias)            

*!*************************************************************
*! Name      : lfArangPop
*! Developer : Hend Ghanem
*! Date      : 11/28/2001
*! Purpose   : To rearrange array from any popup
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : Popup name
*!                      array name
*!                      array colum to use in sort
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*  Collect new arrangement of popup
FUNCTION lfArangPop
PARAMETERS lcPopNam,laArray

IF ALEN(laArray,2) <= 1 
  DECLARE laArray[CNTBAR(lcPopNam),1]

  FOR lnCount = 1 TO CNTBAR(lcPopNam)
    laArray[lnCount]=PRMBAR(lcPopNam,GETBAR(lcPopNam,lnCount))
  ENDFOR
ELSE  
  DECLARE laTmpAry[ALEN(laArray,1),ALEN(laArray,2)]
  =ACOPY(laArray,laTmpAry)
  FOR lnCount = 1 TO CNTBAR(lcPopNam)
    lnSourcRow = GETBAR(lcPopNam,lnCount)     
    IF lnSourcRow > 0
      FOR  lnColum = 1 TO ALEN(laTmpAry,2)
        laArray[lnCount,lnColum] = laTmpAry[lnSourcRow,lnColum] 
      ENDFOR
    ENDIF
  ENDFOR
ENDIF  

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
lnUser     = puUser     
=lfRefresh()

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

*!*************************************************************
*! Name      : lfUserBrow
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/23/2005
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
SELECT (lcPMPCTGNT)
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
         WHEN lfwBrwWhen();
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
*! Name      : lfReadAct
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/23/2005
*! Purpose   : READ Activate function of PRM500.SCX
*!*************************************************************
*! Calls              : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  =lfReadAct()
*!*************************************************************
*E302251,1 AMH
FUNCTION lfReadAct

*-- Restore old key settings and clear the current settings
=gfClearKey()
ON KEY LABEL ALT+H ACTIVATE WINDOW (lcBrUser)
ON KEY LABEL ESC DO lfEsc

*!*************************************************************
*! Name      : lfReadDeact
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/23/2005
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
  ON KEY LABEL TAB        DO lpTab WITH OBJNUM(m.cUser_ID)
  ON KEY LABEL BACKTAB    DO lpTab WITH OBJNUM(pbClose)
ENDIF  
RETURN .F.

*!*************************************************************
*! Name      : lpTab
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/23/2005
*! Purpose   : Trap of tab keys
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  DO lpTab WITH OBJNUM(pbNew)
*!*************************************************************
*E302251,1 AMH
PROCEDURE lpTab
PARAMETERS lnObjNum

ACTIVATE WINDOW SMNTFST2
_CUROBJ = lnObjNum

*!*************************************************************
*! Name      : lfwBrwWhen
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/25/2005
*! Purpose   : When function of users browse for notify screen
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  lfwBrwWhen()
*!*************************************************************
*E302251,1 AMH
FUNCTION lfwBrwWhen
PARAMETERS llDontRef

SELECT (lcPMPCTGNT)
lnBrRecNo = RECNO()
SCATTER MEMVAR
=lfNtfRef()

IF !llDontRef
  SHOW WINDOW (lcBrUser) REFRESH SAME
ENDIF

*!*************************************************************
*! Name      : lfNtfRef
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/25/2005
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

IF laScrMode[2] .OR. EOF(lcPMPCTGNT)
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
*! Date      : 08/25/2005
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

IF (PADR(m.cUser_ID,10) = PADR(lgOldVal,10)) .AND. !llBrowse
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
    m.cUser_ID   = lgOldVal
  ENDIF
ELSE
  IF !EMPTY(m.cUser_ID)
    lnTmpRecNo = RECNO(lcPMPCTGNT)
    IF !SEEK(lcOprt_Ctg+lcoprt_id+m.cUser_ID,lcPMPCTGNT)
      m.cEmail_Add = SYUUSER.CEMAIL_ADD
    ELSE
      WAIT WINDOW "User Exist"
      m.cUser_ID = lgOldVal
    ENDIF
    GO lnTmpRecNo IN (lcPMPCTGNT)
  ENDIF
ENDIF  
lcBrFields = lcOldBrFld

SELECT (lcPMPCTGNT)
REPLACE cUser_ID   WITH m.cUser_ID,;
        cEmail_Add WITH m.cEmail_Add,;
        cStatus    WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1)
SHOW WINDOW (lcBrUser) REFRESH SAME
glUpdated = .T.
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvNewUser
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/25/2005
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

SELECT (lcPMPCTGNT)
APPEND BLANK
REPLACE cStatus   WITH "A",;
        COprt_Ctg WITH lcOprt_Ctg,;
        COprt_ID  WITH lcoprt_id
=lfwBrwWhen(.T.)
_CUROBJ = OBJNUM(m.cUser_ID)

*!*************************************************************
*! Name      : lfEsc
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/25/2005
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

ACTIVATE WINDOW SMNTFST2
_CUROBJ = OBJNUM(pbClose)
KEYBOARD CHR(13)

*!*************************************************************
*! Name      : lfvEmail
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/29/2005
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
*! Date      : 08/29/2005
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

SELECT (lcPMPCTGNT)
REPLACE (lcField) WITH EVALUATE("m."+lcField),;
        cStatus   WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1)
SHOW WINDOW (lcBrUser) REFRESH SAME
glUpdated = .T.

*!*************************************************************
*! Name      : lfvBfrStrt
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/29/2005
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
*! Date      : 08/29/2005
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
*! Date      : 08/29/2005
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
*! Date      : 08/29/2005
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
*! Date      : 08/29/2005
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
*! Date      : 08/29/2005
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
*! Date      : 08/29/2005
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
*! Date      : 08/29/2005
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
*! Date      : 08/29/2005
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
*! Date      : 08/25/2005
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
  SELECT (lcPMPCTGNT)
  REPLACE cStatus WITH SUBSTR('DDS', AT(cStatus, 'SMA'), 1)
  DELETE
  LOCATE
  glUpdated   = .T.
  =lfwBrwWhen()
ENDIF
SELECT (lnCurAlias)
