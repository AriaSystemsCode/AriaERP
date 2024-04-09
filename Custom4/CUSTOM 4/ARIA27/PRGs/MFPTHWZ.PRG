 *:************************************************************************
*: Program file  : MFPTHWZ.PRG
*: Program desc. : Path Wizard  (N00341)
*: For screen    : MFPTHWIZ
*:               : MFPTHWZ1
*:         System: ARIA APPAREL SYSTEM
*:      Developer: HEND GHANEM
*:************************************************************************
*: Calls         : MFPTHWIZ.SPX
*:               : MFPTHWZ1.SPX   
*:*************************************************************************
*: Passed Parameters  : None
*:*************************************************************************
*: Example            : DO MFPTHWZ
*:************************************************************************

STORE '' TO lcOldVal

lcCtgFlds    = [cOprt_Ctg: H = 'Category', cCtg_Dsc : H = 'Description']
lcCtgTtl     = 'Categories of Tasks'
lcCalFlds    = [cCal_ID : H = 'Calendar ID', cCal_Des : H = 'Description']
lcCalTtl     = 'Calendars'
lnShw2Cust   = 1
lcPrompt = '\<Close'
lnOprSeq = 0
llEnBack = .F.
DECLARE laUpdtMthd[1], laUsers[1], laNotified[1] , laPredesor[1,2]
laUpdtMthd[1] = 'Manual'
STORE laUpdtMthd[1] TO lcUpdtMthd, puUpdtMthd
laPredesor[1,1] = ' '
laPredesor[1,2] = ' '
lnPred = 0
STORE ' ' TO lcoprt_res ,lcNotfyStat , lcNotfyMod , lcNotify
m.cCtg_dsc = ' '
*-- Screen objects state of display
lcKeyStat   = 'ENABLE'
lcEditStat  = 'DISABLE'    

ldCurDate    = gdSysDate

STORE .F. TO llBrowse, glUpdated 

IF !gfSetup()
  RETURN
ENDIF  

IF !WEXIST(gcBaseWind)
  
  lc_PMPthHd   = gfTempName()
  lc_PMPthDt   = gfTempName()
  lc_PMPthRl   = gfTempName()
  
  
  *-- Create a file with the same structure as PMPTHHD 
  *-- adding two more fields for saving.
  SELECT PMPTHHD
  =AFIELDS(laFileStru)
  CREATE CURSOR (lc_PMPthHd) FROM ARRAY laFileStru
  INDEX ON cpath_id TAG PMPTHHD OF (lc_PMPthHd)

  
  *-- Create a file with the same structure as PMPTHDT 
  *-- adding two more fields for saving.
  SELECT PMPTHDT
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru(lnFileStru + 2, 4)
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'cCtg_Dsc'
  laFileStru[lnFileStru ,2] = 'C'
  laFileStru[lnFileStru ,3] = 20
  laFileStru[lnFileStru ,4] = 0
  lnFileStru = lnFileStru + 1
  laFileStru[lnFileStru ,1] = 'lFirst'
  laFileStru[lnFileStru ,2] = 'L'
  laFileStru[lnFileStru ,3] = 1
  laFileStru[lnFileStru ,4] = 0

  CREATE CURSOR (lc_PMPthDt) FROM ARRAY laFileStru
  *-- Use index expressions with print sequence and category code fields
  INDEX ON cOprt_Seq + cOprt_Ctg + cOprt_ID TAG PMPTHDT OF (lc_PMPthDt)
  INDEX ON cPath_ID  + cCtg_Seq + cOprt_Seq TAG PMPTHDTS OF (lc_PMPthDt)
  INDEX ON cOprt_Ctg + cOprt_ID TAG PMPTHDTO OF (lc_PMPthDt)
  
  SELECT PMPTHRL
  =AFIELDS(laFileStru)
  
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

  SELECT (lc_PMPthDt)
  
  GO TOP
  SCATTER MEMVAR MEMO BLANK
  
  lnOprWdth = '35'  
ENDIF

lcNotfyStat = IIF(EOF() .OR. EMPTY(laUsers[1]), 'DISABLE', 'ENABLE')  
lcNotfyMod  = IIF(EMPTY(laUsers[1]),' ', 'NOMODIFY')
lcNotify    = IIF(!EMPTY(laUsers[1]), IIF(!EMPTY(ALLTRIM(mNotify)),;
                  STRTRAN(ALLTRIM(mNotify), '|',', ') + '.', ''),;
                  ALLTRIM(mNotify))

PUSH KEY
=gfClearKey()
lcExact = SET('EXACT')
SET EXACT ON
DO (gcScrDir +"MFPTHWIZ.SPX")
SET EXACT &lcExact 
IF USED(lc_PMPthHd)
  USE IN (lc_PMPthHd)
ENDIF
IF USED(lc_PMPthDt)
  USE IN (lc_PMPthDt)
ENDIF
IF USED(lc_PMPthRl)  
  USE IN (lc_PMPthRl)  
ENDIF

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
IF SEEK(m.cPath_ID,'PMPTHHD')
  = gfModalgen("TRM38248B00000","DIALOG")  
  m.cPath_ID = ' '
  m.cPath_Dsc = ' '
  SCATTER MEMVAR MEMO BLANK
  SHOW GET m.cPath_Dsc DISABLE
  SHOW GET m.mpath_com DISABLE
  SHOW GET pbNotes     DISABLE
  SHOW GET ibOprt_Ctg  DISABLE
  SHOW GET m.cOprt_Ctg DISABLE
  _CUROBJ = OBJNUM(m.cPath_ID)
  =lfShowGets('DISABLE')
  =lfRefresh()
ELSE
  IF m.cPath_ID <> lcOldVal .AND. !EMPTY(m.cPath_ID)
    INSERT INTO (lc_PMPthHd) (cPath_ID) VALUES (m.cPath_ID)
    lnOprSeq = 1
    INSERT INTO (lc_PMPthDt) (cPath_ID,lFirst,cOprt_seq);
                      VALUES (m.cPath_ID,.T.,IIF(lnOprSeq < 10,'0'+STR(lnOprSeq,1),STR(lnOprSeq,2)))
    STORE ' ' TO m.cPath_Dsc,m.mpath_com,m.cOprt_Ctg
    SHOW GET m.cPath_Dsc ENABLE
    SHOW GET m.mpath_com ENABLE
    SHOW GET pbNotes     ENABLE
    SHOW GET ibOprt_Ctg  ENABLE
    SHOW GET m.cOprt_Ctg ENABLE
    =lfShowGets('DISABLE')
  ELSE
    IF EMPTY(m.cPath_ID) AND SEEK(lcOldVal,lc_PMPthHd)
      DELETE
      SCATTER MEMVAR MEMO BLANK
      SHOW GET m.cPath_Dsc DISABLE
      SHOW GET m.mpath_com DISABLE
      SHOW GET pbNotes     DISABLE
      SHOW GET ibOprt_Ctg  DISABLE
      SHOW GET m.cOprt_Ctg DISABLE    
      =lfShowGets('DISABLE')
    ENDIF  
  ENDIF
  =lfRefresh()  
ENDIF
            
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
  IF SEEK(m.cPath_ID,lc_PMPthHd)
    REPLACE &lc_PMPthHd..cPath_Dsc WITH m.cPath_Dsc
  ENDIF 
ENDIF

*!*************************************************************
*! Name      : lfvPathCom
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
*! Example            :  lfvPathCom()
*!*************************************************************
FUNCTION lfvPathCom

IF SEEK(m.cPath_ID,lc_PMPthHd)
  REPLACE &lc_PMPthHd..mpath_com WITH m.mpath_com
ENDIF 

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
PARAMETER llBrowse
PRIVATE lnCurAlias 

lnCurAlias   = SELECT(0)
llBrowse     = llBrowse .OR. '?' $ m.cOprt_Ctg

IF llBrowse OR (m.cOprt_Ctg <> lcOldVal AND (!EMPTY(m.cOprt_Ctg) .AND. !SEEK(m.cOprt_Ctg,'PMCTGHD'))) 
  IF llBrowse
    llBrowse = .F.
  ENDIF
  SELECT PMCTGHD
  DIMENSION laTemp[3]
  lcFldName = 'cOprt_Ctg,cCtg_Dsc,cCtg_Seq'
  lcBrFields = [cOprt_Ctg: H = 'Category', cCtg_Dsc : H = 'Description']
  SET EXACT OFF
  IF ARIABROW('','Categories',gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,lcFldName,'laTemp')
    m.cOprt_Ctg = laTemp[1]
    m.cCtg_Dsc  = laTemp[2]
    m.cCtg_Seq  = laTemp[3]
    
    SET ORDER TO PMPTHDTS IN (lc_PMPthDt)
    SELECT (lc_PMPthDt)
    REPLACE &lc_PMPthDt..cOprt_Ctg WITH m.cOprt_Ctg,;
            &lc_PMPthDt..cCtg_Dsc  WITH m.cCtg_Dsc,;
            &lc_PMPthDt..cCtg_Seq  WITH m.cCtg_Seq

    STORE ' ' TO m.cOprt_id , m.ccal_id , m.ccal_id , lcoprt_res , m.cOprt_Dsc
    m.nest_dur = 0
    =lfShowGets('ENABLE')
  ELSE
    STORE ' ' TO m.cOprt_Ctg , m.cCtg_Dsc , m.cOprt_id , m.ccal_id , m.ccal_id , lcoprt_res , m.cOprt_Dsc
    m.nest_dur = 0
    =lfShowGets('DISABLE')
  ENDIF
  SET EXACT ON
ELSE
  IF m.cOprt_Ctg <> lcOldVal 
    m.cCtg_Dsc  = PMCTGHD.cCtg_Dsc
    m.cCtg_Seq  = PMCTGHD.cCtg_Seq
    SELECT (lc_PMPthDt)
    REPLACE &lc_PMPthDt..cOprt_Ctg WITH m.cOprt_Ctg,;
            &lc_PMPthDt..cCtg_Dsc  WITH m.cCtg_Dsc,;
            &lc_PMPthDt..cCtg_Seq  WITH m.cCtg_Seq
      
    STORE ' ' TO m.cOprt_id ,m.cOprt_Dsc , m.ccal_id , lcoprt_res , m.cOprt_Dsc
    m.nest_dur = 0
    =lfShowGets('ENABLE')
  ENDIF    
ENDIF


IF EMPTY(m.cOprt_Ctg) AND (m.cOprt_Ctg <> lcOldVal)
  SET ORDER TO PMPTHDT IN (lc_PMPthDt)
  IF SEEK(IIF(lnOprSeq < 10,'0'+STR(lnOprSeq,1),STR(lnOprSeq,2))+lcOldVal,lc_PMPthDt)
    REPLACE &lc_PMPthDt..cOprt_Ctg WITH " "
  ENDIF
  STORE ' ' TO m.cOprt_Ctg , m.cCtg_Dsc , m.cOprt_id , m.ccal_id , m.ccal_id , lcoprt_res , m.cOprt_Dsc
  m.nest_dur = 0
  =lfShowGets('DISABLE')
ENDIF  


=lfRefresh()

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
PARAMETER llBrowse
PRIVATE lnCurAlias 

lnCurAlias   = SELECT(0)
llBrowse     = llBrowse .OR. '?' $ m.cOprt_ID

IF llBrowse OR (m.cOprt_ID <> lcOldVal AND;
               (!EMPTY(m.cOprt_ID) AND !SEEK(m.cOprt_Ctg + m.cOprt_ID,'PMCTGDT')))
  IF llBrowse 
    llBrowse  = .F.
  ENDIF
  SELECT PMCTGDT
  DIMENSION laTemp[10]
  lcFldName = 'cOprt_ID,cOprt_Dsc,cOprt_seq,ccal_id,nest_dur,mNotify,coprt_res,;
               cGroup_ID,lshw2cust,cupdtmthd'
  lcOprt_Ctg = SUBSTR(m.cOprt_Ctg,1,3)
  lcBrFields = [cOprt_ID: H = 'Task ID', cOprt_Dsc : H = 'Description']
  SET EXACT OFF
  IF ARIABROW([lcOprt_Ctg],'Tasks',gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,;
               .F.,.F.,lcFldName,'laTemp')
    
    m.cOprt_ID  = laTemp[1]      
    m.cOprt_Dsc = laTemp[2]
    m.cOprt_seq = laTemp[3]
    m.ccal_id   = laTemp[4]
    m.nest_dur  = laTemp[5]
    lcNotify    = laTemp[6]
    lcoprt_res  = IIF(EMPTY(laTemp[7]),laTemp[8],laTemp[7])
    PuUser      = IIF(EMPTY(laTemp[7]) AND EMPTY(laTemp[8]),'User',;
                  IIF(EMPTY(laTemp[7]),'Group of users','User'))
    =gfStr2Ar(ALLTRIM(lcNotify), @laNotified, '|') 
    m.lshw2cust = laTemp[9]  
    lnShw2Cust  = IIF(m.lshw2cust,1,0) 
    m.cupdtmthd = laTemp[10]               
  ELSE
    =lfIntlzVar()
  ENDIF
  SET EXACT ON
ELSE
  IF !EMPTY(m.cOprt_ID) AND SEEK(m.cOprt_Ctg + m.cOprt_ID,'PMCTGDT')
    m.cOprt_Dsc = PMCTGDT.cOprt_Dsc
    m.cOprt_seq = PMCTGDT.cOprt_seq
    m.ccal_id   = PMCTGDT.ccal_id
    m.nest_dur  = PMCTGDT.nEst_dur
    lcNotify    = PMCTGDT.mNotify
    lcoprt_res  = IIF(EMPTY(PMCTGDT.coprt_res),PMCTGDT.cGroup_ID,PMCTGDT.coprt_res)
    PuUser      = IIF(EMPTY(PMCTGDT.coprt_res) AND EMPTY(PMCTGDT.cGroup_Id),'User',;
                  IIF(EMPTY(PMCTGDT.coprt_res),'Group of users','User'))
    =gfStr2Ar(ALLTRIM(lcNotify), @laNotified, '|')                                    
    m.lshw2cust = PMCTGDT.lshw2cust  
    lnShw2Cust  = IIF(m.lshw2cust,1,0)
    m.cupdtmthd = PMCTGDT.cupdtmthd             
  ENDIF   
ENDIF  

puUpdtMthd  = m.cupdtmthd

IF !EMPTY(m.cOprt_ID) AND (m.cOprt_ID <> lcOldVal)
  lcPrvOrd = ORDER(lc_PMPthDt)
  SET ORDER TO PMPTHDTO  IN (lc_PMPthDt)
  IF SEEK(m.cOprt_Ctg + m.cOprt_ID,lc_PMPthDt);
     AND &lc_PMPthDt..cOprt_Seq <> IIF(lnOprSeq < 10,'0'+STR(lnOprSeq,1),STR(lnOprSeq,2))

    = gfModalgen("TRM38250B00000","DIALOG")  
    =lfIntlzVar()
    SET ORDER TO (lcPrvOrd) IN (lc_PMPthDt)
    =lfShowGets('ENABLE')
    =lfRefresh()
    RETURN
  ENDIF
  SET ORDER TO (lcPrvOrd) IN (lc_PMPthDt)
   
  SELECT (lc_PMPthRl)
  SET ORDER TO TAG PMPTHRLP 
  *-- If the current operation is found as a predecessor to
  *-- others, get all the operations on all available paths
  *-- into an array.
  lnCount = 0  
  IF SEEK(m.cOprt_Ctg + m.cOprt_ID)
    lcOper = m.cOprt_Ctg + m.cOprt_ID
    = gfModalgen("TRM38249B00000","DIALOG",lcOper)
    =lfIntlzVar()
    =lfShowGets('ENABLE')
    =lfRefresh()
    RETURN
  ENDIF

  SET ORDER TO PMPTHDT IN (lc_PMPthDt)

  
  IF SEEK(IIF(lnOprSeq < 10,'0'+STR(lnOprSeq,1),STR(lnOprSeq,2))+m.cOprt_Ctg,lc_PMPthDt)
  
    IF EMPTY(&lc_PMPthDt..cOprt_ID)
      lnPred = lnPred + 1
      DIME laPredesor[lnPred,2]
      laPredesor[lnPred,1] = m.cOprt_ctg
      laPredesor[lnPred,2] = m.cOprt_ID  
      IF lnPred > 1
        INSERT INTO (lc_PMPthRl) (cpath_id,coprt_ctg,coprt_id,cprd_ctg,cprd_id);
                           VALUES (m.cpath_id,laPredesor[lnPred,1],laPredesor[lnPred,2],;
                          laPredesor[lnPred-1,1],laPredesor[lnPred-1,2])
      ENDIF
    ELSE
      SELECT (lc_PMPthRl)
      SET ORDER TO TAG PMPTHRL
      IF SEEK(m.cOprt_Ctg + lcOldVal)    
        REPLACE &lc_PMPthRl..coprt_ctg WITH m.cOprt_ctg,;
                &lc_PMPthRl..coprt_ID  WITH m.cOprt_ID
      ENDIF
    ENDIF

    SELECT (lc_PMPthDt)
    REPLACE &lc_PMPthDt..cOprt_ID  WITH m.cOprt_ID ,;
            &lc_PMPthDt..cOprt_Dsc WITH m.cOprt_Dsc,;
            &lc_PMPthDt..ccal_id   WITH m.ccal_id  ,; 
            &lc_PMPthDt..nest_dur  WITH m.nest_dur ,;             
            &lc_PMPthDt..mNotify   WITH lcNotify   ,;  
            &lc_PMPthDt..coprt_res WITH ''         ,;  
            &lc_PMPthDt..cGroup_Id WITH ''         ,;
            &lc_PMPthDt..lshw2cust WITH m.lshw2cust,;
            &lc_PMPthDt..cupdtmthd WITH m.cupdtmthd
    IF PuUser = 'User'
      REPLACE &lc_PMPthDt..coprt_res WITH lcoprt_res
    ELSE
      REPLACE &lc_PMPthDt..cGroup_Id WITH lcoprt_res    
    ENDIF            
                       
  ENDIF
ELSE
  IF EMPTY(m.cOprt_ID) AND (m.cOprt_ID <> lcOldVal)
    IF SEEK(IIF(lnOprSeq < 10,'0'+STR(lnOprSeq,1),STR(lnOprSeq,2))+m.cOprt_Ctg,lc_PMPthDt)
 
      lnPred = lnPred - 1
      SELECT (lc_PMPthRl)
      IF lnPred =< 1
        ZAP
      ELSE
        SET ORDER TO TAG PMPTHRL
        IF SEEK(m.cOprt_Ctg + lcOldVal)    
          REPLACE &lc_PMPthRl..coprt_ctg WITH '',;
                  &lc_PMPthRl..coprt_ID  WITH ''
        ENDIF
      ENDIF
      SELECT (lc_PMPthDt)
      REPLACE &lc_PMPthDt..cOprt_ID  WITH '',;
              &lc_PMPthDt..cOprt_Dsc WITH '',;
              &lc_PMPthDt..ccal_id   WITH '',; 
              &lc_PMPthDt..nest_dur  WITH 0 ,;             
              &lc_PMPthDt..mNotify   WITH '',;  
              &lc_PMPthDt..coprt_res WITH '',;  
              &lc_PMPthDt..cGroup_Id WITH '',;
              &lc_PMPthDt..lshw2cust WITH .F.,;
              &lc_PMPthDt..cupdtmthd WITH '',;
              &lc_PMPthDt..coprt_res WITH '',;
              &lc_PMPthDt..cGroup_Id WITH ''
       =lfIntlzVar()
    ENDIF
  ENDIF
ENDIF  
=lfShowGets('ENABLE')
=lfRefresh()

SELECT (lnCurAlias) 
*!*************************************************************
*! Name      : lfvOprtDsc
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
*! Example            :  lfvOprtDsc()
*!*************************************************************
FUNCTION lfvOprtDsc

IF m.cOprt_Dsc <> lcOldVal
  lnCurAlias = SELECT(0)
  SET ORDER TO PMPTHDT IN (lc_PMPthDt)
  IF SEEK(IIF(lnOprSeq < 10,'0'+STR(lnOprSeq,1),STR(lnOprSeq,2))+m.cOprt_Ctg+m.cOprt_ID,lc_PMPthDt)
    SELECT (lc_PMPthDt)
    REPLACE &lc_PMPthDt..cOprt_Dsc WITH m.cOprt_Dsc
  ENDIF
  SELECT (lnCurAlias)
  glUpdated   = .T.
ENDIF

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
PARAMETER llBrowse
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)

llBrowse     = llBrowse .OR. '?' $ m.ccal_id

IF llBrowse OR (m.ccal_id <> lcOldVal AND (!EMPTY(m.ccal_id) AND !SEEK(m.ccal_id,'PMCALHD'))) 
  IF llBrowse 
    llBrowse  = .F.
  ENDIF
  SELECT PMCALHD
  DIMENSION laTemp[2]
  lcFldName = 'ccal_id'
  lcBrFields = [cCal_ID : H = 'Calendar ID', cCal_Des : H = 'Description']
  SET EXACT OFF
  IF ARIABROW('','Calenders',gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,;
               .F.,.F.,lcFldName,'laTemp')
    m.ccal_id  = laTemp[1]      
  ELSE
    m.ccal_id  = ' '      
  ENDIF
  SET EXACT ON
ENDIF
SET ORDER TO PMPTHDT IN (lc_PMPthDt)
IF SEEK(IIF(lnOprSeq < 10,'0'+STR(lnOprSeq,1),STR(lnOprSeq,2))+m.cOprt_Ctg+m.cOprt_ID,lc_PMPthDt)
  SELECT (lc_PMPthDt)
  REPLACE &lc_PMPthDt..cCal_id WITH m.cCal_id
ENDIF
=lfRefresh()

SELECT (lnCurAlias) 

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
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)

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
  SET EXACT OFF
  IF ARIABROW('','Responsible User',gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,lcFldName,'laTemp')
    LCOPRT_RES = laTemp[1]
  ELSE
    LCOPRT_RES = ''
  ENDIF  
  SET EXACT ON
ENDIF
SET ORDER TO PMPTHDT IN (lc_PMPthDt)
IF SEEK(IIF(lnOprSeq < 10,'0'+STR(lnOprSeq,1),STR(lnOprSeq,2))+m.cOprt_Ctg+m.cOprt_ID,lc_PMPthDt)
  SELECT (lc_PMPthDt)
  REPLACE COPRT_RES  WITH ' ',;
          CGROUP_ID  WITH ' '  
  REPLACE &lcField   WITH ALLTRIM(LCOPRT_RES)
ENDIF

=lfRefresh()

SELECT (lnCurAlias) 

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
    m.nEst_Dur  = lcOldVal
  ENDIF  
  SELECT (lnCurAlias)
ENDIF
SET ORDER TO PMPTHDT IN (lc_PMPthDt)
IF SEEK(IIF(lnOprSeq < 10,'0'+STR(lnOprSeq,1),STR(lnOprSeq,2))+m.cOprt_Ctg+m.cOprt_ID,lc_PMPthDt)
  SELECT (lc_PMPthDt)
  REPLACE &lc_PMPthDt..nEst_Dur WITH m.nEst_Dur
ENDIF

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
SET ORDER TO PMPTHDT IN (lc_PMPthDt)
IF SEEK(IIF(lnOprSeq < 10,'0'+STR(lnOprSeq,1),STR(lnOprSeq,2))+m.cOprt_Ctg+m.cOprt_ID,lc_PMPthDt)
  SELECT (lc_PMPthDt)
  REPLACE &lc_PMPthDt..cUpdtMthd WITH lcUpdtMthd
ENDIF

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

IF !(lcNotify = lcOldVal)
  lnCurAlias = SELECT(0)
  SET ORDER TO PMPTHDT IN (lc_PMPthDt)
  IF SEEK(IIF(lnOprSeq < 10,'0'+STR(lnOprSeq,1),STR(lnOprSeq,2))+m.cOprt_Ctg+m.cOprt_ID,lc_PMPthDt)
    SELECT (lc_PMPthDt)
    REPLACE &lc_PMPthDt..mNotify WITH STRTRAN(ALLTRIM(lcNotify), ',','|')
  ENDIF
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

*-- Amin
IF EMPTY(laUsers[1])	
  DIME laUsers[1]
  laUsers[1]=''
ENDIF 
*-- Amin

SELECT (lc_PMPthDt)
=gfStr2Ar(ALLTRIM(mNotify), @laNotified, '|')
IF laScrMode[2]
  =gfMover(@laUsers,@laNotified,"Users to Notify",.F.)
ELSE
  =gfMover(@laUsers,@laNotified,"Select Users to Notify",.T.)
  lcNotify   = ''
  FOR lnCount = 1 TO ALEN(laNotified)
    lcNotify = lcNotify + ALLTRIM(laNotified[lnCount]) + '|'
  ENDFOR
  IF !EMPTY(lcNotify)
    lcNotify = SUBSTR(lcNotify, 1, RAT('|', lcNotify)-1) 
  ENDIF
  REPLACE mNotify WITH ALLTRIM(lcNotify)
  lcNotify   = IIF(!EMPTY(lcNotify), STRTRAN(lcNotify, '|', ', ') + '.', '')
  glUpdated  = .T.
  SHOW GET lcNotify
ENDIF
SET PROCEDURE TO &lcSetProc
SET ORDER TO PMPTHDT IN (lc_PMPthDt)
IF SEEK(IIF(lnOprSeq < 10,'0'+STR(lnOprSeq,1),STR(lnOprSeq,2))+m.cOprt_Ctg+m.cOprt_ID,lc_PMPthDt)
  SELECT (lc_PMPthDt)
  REPLACE &lc_PMPthDt..mNotify WITH ALLTRIM(lcNotify)
ENDIF

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

m.lShw2Cust  = IIF(lnShw2Cust = 1,.T.,.F.)

SET ORDER TO PMPTHDT IN (lc_PMPthDt)
IF SEEK(IIF(lnOprSeq < 10,'0'+STR(lnOprSeq,1),STR(lnOprSeq,2))+m.cOprt_Ctg+m.cOprt_ID,lc_PMPthDt)
  SELECT (lc_PMPthDt)
  REPLACE &lc_PMPthDt..lShw2Cust WITH m.lShw2Cust
ENDIF

glUpdated   = .T.



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

SELECT (lc_PMPthDt)
=gfZoom('m.mOprt_Com')

SET ORDER TO PMPTHDT IN (lc_PMPthDt)
IF SEEK(IIF(lnOprSeq < 10,'0'+STR(lnOprSeq,1),STR(lnOprSeq,2))+m.cOprt_Ctg+m.cOprt_ID,lc_PMPthDt)
  SELECT (lc_PMPthDt)
  REPLACE &lc_PMPthDt..mOprt_Com WITH m.mOprt_Com
ENDIF

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfShowGets
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
FUNCTION lfShowGets
PARAMETERS lcShwStat

SHOW GET ibOprt_ID    &lcShwStat
SHOW GET m.cOprt_id   &lcShwStat
SHOW GET m.cOprt_Dsc  &lcShwStat
SHOW GET ibCal_ID     &lcShwStat
SHOW GET m.ccal_id    &lcShwStat
SHOW GET puUser       &lcShwStat
SHOW GET lcoprt_res   &lcShwStat
SHOW GET lnShw2Cust   &lcShwStat
SHOW GET m.nest_dur   &lcShwStat
SHOW GET puUpdtMthd   &lcShwStat
SHOW GET pbNotify     &lcShwStat
SHOW GET lcNotify     &lcShwStat

SHOW GET pbCNote      &lcShwStat

lcPrompt = IIF(EMPTY(m.cPath_Id),'\<Close','\<Cancel')
SHOW GET pbCancel,1 PROMPT lcPrompt ENABLE
IF !EMPTY(m.cOprt_id)
  SHOW GET pbFinish    &lcShwStat 
  SHOW GET pbNext      &lcShwStat
ELSE
  SHOW GET pbFinish    DISABLE
  SHOW GET pbNext      DISABLE
ENDIF  

IF !llEnBack AND (EOF(lc_PMPthDt) OR &lc_PMPthDt..lFirst)
  SHOW GET pbBack  DISABLE
ELSE
  llEnBack = .T.
  SHOW GET pbBack  ENABLE  
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

lcOprt_Res = ' ' 

SHOW GET lcOprt_Res

=lfRefresh()


*!*************************************************************
*! Name      : lfvNext
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Validates Next in first screen.
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvNext()
*!*************************************************************
FUNCTION lfvNext

lcPath_id  = m.cPath_id
lcpath_dsc = m.cpath_dsc
*llBrowse = .F.


SELECT (lc_PMPthDt)
IF !EOF()
  SKIP 
ENDIF
lnOprSeq = lnOprSeq + 1  
IF !EOF()
  SCATTER MEMVAR MEMO 
  lcoprt_res = IIF(EMPTY(m.coprt_res),m.cGroup_Id,m.coprt_res)
  lcNotify   = m.MNotify
  SELECT cUser_ID ;
    FROM (gcSysHome + 'SYUUSER');
    INTO ARRAY laUsers;
   ORDER BY cUser_ID
  =gfStr2Ar(ALLTRIM(m.MNotify), @laNotified, '|')
  llNew = .F.
ELSE
  INSERT INTO (lc_PMPthDt) (cPath_ID,cOprt_seq) VALUES (m.cPath_ID,IIF(lnOprSeq < 10,'0'+STR(lnOprSeq,1),STR(lnOprSeq,2)))
  SCATTER MEMVAR MEMO
  lcoprt_res = ''
  lcNotify   = ''
  STORE '' TO laNotified
  SELECT cUser_ID ;
    FROM (gcSysHome + 'SYUUSER');
    INTO ARRAY laUsers;
    ORDER BY cUser_ID
  llNew = .T. 
  _CUROBJ = OBJNUM(m.cOprt_Ctg)
ENDIF


m.cPath_id  = lcPath_id
m.cpath_dsc = lcpath_dsc

SHOW GET m.cPath_id  DISABLE
SHOW GET m.cpath_dsc DISABLE
SHOW GET m.mpath_com DISABLE
SHOW GET pbNotes     DISABLE

IF llNew
  SHOW GET ibOprt_Ctg  ENABLE
  SHOW GET m.cOprt_Ctg ENABLE
  =lfShowGets('DISABLE')
ELSE
  SHOW GET ibOprt_Ctg  ENABLE
  SHOW GET m.cOprt_Ctg ENABLE
  =lfShowGets('ENABLE')
ENDIF

=lfRefresh()

*!*************************************************************
*! Name      : lfvfinish
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Validates Next in first screen.
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvfinish()
*!*************************************************************
FUNCTION lfvfinish

SELECT (lc_PMPthHd)
SCATTER MEMVAR MEMO
INSERT INTO PMPTHHD FROM MEMVAR

SELECT (lc_PMPthDt)
SCAN FOR !EMPTY(&lc_PMPthDt..coprt_id)
  SCATTER MEMVAR MEMO
  INSERT INTO PMPTHDT FROM MEMVAR
ENDSCAN

SELECT (lc_PMPthRl)
SCAN FOR !EMPTY(&lc_PMPthRl..coprt_id)
  SCATTER MEMVAR MEMO
  INSERT INTO PMPTHRL FROM MEMVAR
ENDSCAN

SELECT (lc_PMPthHd)
ZAP
SCATTER MEMVAR MEMO BLANK
SELECT (lc_PMPthDt)
ZAP
SCATTER MEMVAR MEMO BLANK
SELECT (lc_PMPthRl)
ZAP

lcoprt_res = ''
lcNotify   = ''
STORE '' TO laNotified
SELECT cUser_ID ;
  FROM (gcSysHome + 'SYUUSER');
  INTO ARRAY laUsers;
 ORDER BY cUser_ID

SHOW GET m.cPath_id  ENABLE
SHOW GET m.cpath_dsc ENABLE
SHOW GET m.mpath_com ENABLE
SHOW GET pbNotes     ENABLE
SHOW GET ibOprt_Ctg  DISABLE
SHOW GET m.cOprt_Ctg DISABLE
llEnBack = .F.
=lfShowGets('DISABLE')
=lfRefresh()


*!*************************************************************
*! Name      : lfvBack
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Validates Next in first screen.
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvBack()
*!*************************************************************
FUNCTION lfvBack

SELECT (lc_PMPthDt)
lnOprSeq = lnOprSeq - 1  

IF !BOF()
  SKIP -1
  SCATTER MEMVAR MEMO
  lcoprt_res = IIF(EMPTY(m.coprt_res),m.cGroup_Id,m.coprt_res)
  lcNotify   = m.MNotify
  =gfStr2Ar(ALLTRIM(m.MNotify), @laNotified, '|')
ENDIF

IF &lc_PMPthDt..lFirst
  llEnBack = .F.
ENDIF  

SHOW GET m.cPath_id  DISABLE
SHOW GET m.cpath_dsc DISABLE
SHOW GET m.mpath_com DISABLE
SHOW GET pbNotes     DISABLE
SHOW GET ibOprt_Ctg  ENABLE
SHOW GET m.cOprt_Ctg ENABLE
=lfShowGets('ENABLE')

*llBrowse = .F.

=lfRefresh()

*!*************************************************************
*! Name      : lfvCancel
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Validates Cancel
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfvCancel()
*!*************************************************************
FUNCTION lfvCancel

IF lcPrompt = '\<Close'
*--- SSH
  CLEAR READ
ELSE
  SELECT (lc_PMPthHd)
  ZAP
  SCATTER MEMVAR MEMO BLANK
  SELECT (lc_PMPthDt)
  ZAP
  SCATTER MEMVAR MEMO BLANK
  SELECT (lc_PMPthRl)
  ZAP
  llEnBack = .F.
ENDIF

lcoprt_res = ''
lcNotify   = ''
STORE '' TO laNotified
SELECT cUser_ID ;
  FROM (gcSysHome + 'SYUUSER');
  INTO ARRAY laUsers;
 ORDER BY cUser_ID

SHOW GET m.cPath_id  ENABLE
SHOW GET m.cpath_dsc DISABLE
SHOW GET m.mpath_com DISABLE
SHOW GET pbNotes     DISABLE
SHOW GET ibOprt_Ctg  DISABLE
SHOW GET m.cOprt_Ctg DISABLE
=lfShowGets('DISABLE')

=lfRefresh()

*!*************************************************************
*! Name      : lfIntlzVar
*! Developer : HEND GHANEM
*! Date      : 12/17/2001
*! Purpose   : Initialize Variables
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfIntlzVar()
*!*************************************************************
FUNCTION lfIntlzVar

STORE ' ' TO m.cOprt_id ,m.cOprt_Dsc, m.ccal_id , lcoprt_res,lcNotify
m.cupdtmthd = 'Manual'
m.lShw2Cust = .T.
lnShw2Cust  = 1
m.nest_dur = 0
PuUser = 'User'
STORE '' TO laNotified
SELECT cUser_ID ;
  FROM (gcSysHome + 'SYUUSER');
  INTO ARRAY laUsers;
  ORDER BY cUser_ID