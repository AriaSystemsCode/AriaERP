*:************************************************************************
*: Program file  : SMRPTMP.Prg
*: Program desc. : Report templates program. (N00341)
*: For screen    : SMRPTMP.Scx
*:         System: ARIA APPAREL SYSTEM
*:         Module: 
*:      Developer: HEND GHANEM
*********************************************************************************
*: Calls : 
*:         Functions  : lfvTmpCode
*:                    : lfBrwUnTrp
*:                    : lfBrowTrap
*:                    : lfDeAct
*:                    : lfvNew
*:                    : lfvPRMSh
*:                    : lfvModify
*:                    : lfvRemove
*:                    : lfvOk
*:                    : lfwBrows
*:                    : lfDispBrow
*:                    : lfvclose
*:                    : lfPRM1Sh
*:                    : lfvTmpCod
*:                    : lfvOper
*:                    : lfVldSlOpr
*:                    : gfMover
*********************************************************************************
*:E301869,2 AMH 12/30/2002 Create a production schedule project according to a setting.
*:************************************************************************

lcPRM400Ttl = "Report templates" && variable to hold screen title.
llInsert    = .F.                && variable to hold if insert or not.
lcTmpCode   = SPACE(3)           && variable to hold Template code.
lcDesc      = SPACE(30)          && variable to hold Description.
pbOper      = 1                  && variable to hold Operation.
lnBrRecNo   = 0                  && variable to hold browse record number.
lcBrowTitl  = 'Templates'        && variable to hold browse title.
lcFillMemo  = ' '                && variable to hold operation memo field string.   
lcWintitl   = 'Report template'  && variable to hold screen title.

DECLARE laAvailOprs[1], laSelOprs[1]
laAvailOprs = ' '  && Array to hold available operation.
laSelOprs   = ' '  && Array to hold selected operation.

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
  lcTempCurs = gfTempName()
  CREATE CURSOR (lcTempCurs) (Oper C(51)) 
  INDEX ON Oper TAG (lcTempCurs)

  SELECT PMCTGDT
  GO TOP 
  *-- program cannot be generated if there is no operation created.
  IF EMPTY(cOprt_id)
    =gfModalgen("TRM38226B00000","DIALOG",'creating a template')
    RETURN 
  ENDIF
  SELECT PMRPRTM
  GO TOP
ENDIF
PUSH KEY  

*-- trap tap and shift tab keys
=lfPushKey()

*E301869,2 AMH Call screen from the correct directory [Start]
*DO (gcScrDir+"SM\"+'SMRPTMP.SPR')
DO (gcScrDir+gcWinAppl+'\SMRPTMP.SPR')
*E301869,2 AMH [End]

POP KEY

*!*************************************************************
*! Name      : lfPRM1Sh
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : control push button status.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfPRM1Sh()
*!*************************************************************
FUNCTION lfPRM1Sh

*-- check if there is no records in PMRPRMTM file no need 
*-- to make to operation modify or remove.
IF EOF()
  SHOW GET pbModify DISABLE
  SHOW GET pbRemove DISABLE
ENDIF

*!*************************************************************
*! Name      : lfPushKey
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : trap keys.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfPushKey()
*!*************************************************************
FUNCTION lfPushKey 


ON KEY LABEL CTRL+W     lnDummy = 1
ON KEY LABEL CTRL+Q     lnDummy = 1
ON KEY LABEL CTRL+HOME  lnDummy = 1
ON KEY LABEL CTRL+END   lnDummy = 1
ON KEY LABEL ESC        DO lfvClose
ON KEY LABEL Ctrl+ENTER DO lfvclose


*!*************************************************************
*! Name      : lpTrap1
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : Tab trap.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lpTrap1()
*!*************************************************************
PROCEDURE lpTrap1

IF WONTOP(lcBrowTitl)
  ACTIVATE WINDOW SMRPTMP1
ENDIF


*!*************************************************************
*! Name      : lpTrap2
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : shift tab trap.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lpTrap2()
*!*************************************************************
PROCEDURE lpTrap2
 
IF WONTOP(lcBrowTitl)
  ACTIVATE WINDOW SMRPTMP1
  _CUROBJ = OBJNUM(pbClose)
ENDIF


*!*************************************************************
*! Name      : lfvclose
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : Validation on push button close.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvclose()
*!*************************************************************
FUNCTION lfvclose

ACTIVATE WINDOW SMRPTMP1
_CUROBJ = OBJNUM(pbClose)
CLEAR READ

*!*************************************************************
*! Name      : lfDispBrow
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : create report template browse.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfDispBrow()
*!*************************************************************
FUNCTION lfDispBrow

lnBrRecNo  = RECNO('PMRPRTM')
lcBrowTitl = 'Templates'
lcBrFields = "lcMarker=IIF(RECNO()=lnBrRecNo,'',' '):1:H=' ':W=.F.,"+;
             "cTmp_Code :R :H='Template Code':15,"+;
             "cTmp_Dsc  :R :H='Description':35"

BROWSE FIELDS &lcBrFields;
       WINDOW    SMRPTMP0;
       WHEN lfwBrows();
       IN WINDOW SMRPTMP;
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOMENU;
       NOWAIT;
       NOEDIT;
       SAVE;
       TITLE lcBrowTitl

*!*************************************************************
*! Name      : lfwBrows
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : Refresh record pointer.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfwBrows()
*!*************************************************************
FUNCTION lfwBrows

lnBrRecNo  = RECNO('PMRPRTM')
SHOW WINDOW (lcBrowTitl) REFRESH

IF _WINDOWS OR _MAC
  llTrap =  IIF (EMPTY(ON('KEY','TAB')),.T.,.F.)
  IF llTrap
    =lfBrowTrap()
  ENDIF  
ENDIF  

*!*************************************************************
*! Name      : lfvOk
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : validation on push button Ok.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvOk()
*!*************************************************************
FUNCTION lfvOk

*-- Privent user from saving template has no operation.
IF EMPTY(lcFillMemo)
  =gfModalgen("TRM38226B00000","DIALOG",'saving')
ELSE
  *-- Privent add same code more than one time. (Begin)
  IF llInsert
    IF SEEK(lcTmpCode)
      lnChoice = gfModalgen("TRM38227B38031","DIALOG",TRIM(lcTmpCode))
      IF lnChoice = 1
        llInsert=.F.
      ELSE
        _CUROBJ = OBJNUM(lcTmpCode)
        RETURN
      ENDIF  
    ENDIF
  ENDIF  
  *-- if inserted new record it means not update.
  IF llInsert
    APPEND BLANK
  ENDIF  
  REPLACE cTmp_Code WITH lcTmpCode,;
          cTmp_Dsc  WITH lcDesc,;
          mTmp_Oprt WITH lcFillMemo
  llInsert   = .F.
  lnBrRecNo  = RECNO('PMRPRTM')
  CLEAR READ
ENDIF

*!*************************************************************
*! Name      : lfvRemove
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : validation on push button remove.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvRemove()
*!*************************************************************
FUNCTION lfvRemove

lnChoice = gfModalgen("TRM38228B00030","DIALOG")

IF lnChoice = 1
  DELETE
ENDIF

GO TOP 
*-- check if there is no records in PMRPRTM file,
*-- no need from modify, or delete operation.
IF EMPTY(cTmp_Code)
  SHOW GET pbModify DISABLE
  SHOW GET pbRemove DISABLE
ENDIF

lnBrRecNo = RECNO('PMRPRTM')
SHOW WINDOW(lcBrowTitl) REFRESH

*!*************************************************************
*! Name      : lfvModify
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : validation on push button modify.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvModify()
*!*************************************************************
FUNCTION lfvModify

lcObjDisp  = 'DISABLE'
llObjDisp  = .F.
DIMENSION laSelOprs(1)
STORE '' TO laAvailOprs, laSelOprs
lcFillMemo = ""
lcWindTitl="  Modify Record "

lcTmpCode   = cTmp_Code 
lcDesc      = cTmp_Dsc

IF !EMPTY(mTmp_Oprt)
  =gfStr2Ar(ALLTRIM(mTmp_Oprt), @laSelOprs, '|')
  IF ALEN(laSelOprs) > 0
    FOR lnCount = 1 TO ALEN(laSelOprs)
      laSelOprs[lnCount] = SUBSTR(laSelOprs[lnCount], 1, 3) + '\' + ;
                           SUBSTR(laSelOprs[lnCount], 4, 5) + ' ' +;
                           LOOKUP(PMCTGDT.cOprt_Dsc, laSelOprs[lnCount],;
                           PMCTGDT.cOprt_Ctg, 'PMCTGDT')
    ENDFOR                              
  ENDIF  
ENDIF  

lcFillMemo = ''
FOR lnCount = 1 TO ALEN(laSelOprs)
  lcFillMemo = lcFillMemo + SUBSTR(laSelOprs[lnCount], 1, 3) + ;
                            SUBSTR(laSelOprs[lnCount], 5, 5) + '|' 
ENDFOR  
IF !EMPTY(lcFillMemo)
  lcFillMemo = SUBSTR(lcFillMemo , 1, LEN(lcFillMemo)-1)
ENDIF

POP KEY

*E301869,2 AMH Call screen from the correct directory [Start]
*DO (gcScrDir+"SM\"+'SMRPTMPA.SPR')
DO (gcScrDir+gcWinAppl+'\SMRPTMPA.SPR')
*E301869,2 AMH [End]

=lfPushKey()

SHOW WINDOW(lcBrowTitl) REFRESH

*!*************************************************************
*! Name      : lfvPRMSh
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : control foreign price branch screen.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvPRMSh()
*!*************************************************************
FUNCTION lfvPRMSh

IF llInsert
  SHOW GET lcTmpCode  ENABLE
  SHOW GET pbOk       DISABLE  
  SHOW GET lcDesc     DISABLE
  SHOW GET pbOper     DISABLE
ENDIF

*!*************************************************************
*! Name      : lfvNew
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : validation on push button insert.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvNew()
*!*************************************************************
FUNCTION lfvNew

lcObjDisp  = 'ENABLE'
llObjDisp  = .T.
lcFillMemo = ""
DIMENSION laSelOprs(1)
STORE '' TO laAvailOprs, laSelOprs
lcWindTitl =" Add a new record "
llInsert   = .T.
lcTmpCode = SPACE(3)
lcDesc    = SPACE(30)
POP KEY

*E301869,2 AMH Call screen from the correct directory [Start]
*DO (gcScrDir+"SM\"+'SMRPTMPA.SPR')
DO (gcScrDir+gcWinAppl+'\SMRPTMPA.SPR')
*E301869,2 AMH [End]

=lfPushKey()
SHOW WINDOW(lcBrowTitl) REFRESH

*-- if did not insert any record and ther is no records
*-- in the PMRPRTM file no need from modify or delete.
IF !EOF()
  SHOW GET pbModify ENABLE
  SHOW GET pbRemove ENABLE
  IF BETWEEN(lnBrRecNo, 1, RECCOUNT())
    GO lnBrRecNo
  ENDIF  
ENDIF
llInsert = .F.

*!*************************************************************
*! Name      : lfDeAct
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : control browse trap.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfDeAct()
*!*************************************************************
FUNCTION lfDeAct

*--  check if browse window is active.
ON KEY LABEL ALT+B
IF WONTOP(lcBrowTitl)
  lnBrRecNo  = RECNO('PMRPRTM')
  =lfBrowTrap()
ELSE
  =lfBrwUnTrp() 
ENDIF

RETURN .F.

*!*************************************************************
*! Name      : lfAct
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : control browse trap.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfAct()
*!*************************************************************
FUNCTION lfAct

=gfClearKey()
=lfBrwUnTrp() 
ON KEY LABEL ALT+B DO lpAct


*!*************************************************************
*! Name      : lpAct
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : control browse trap.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfAct()
*!*************************************************************
PROCEDURE lpAct

IF _WINDOWS OR _MAC
  =lFBROWTRAP() 
ENDIF  
SHOW WINDOW (lcBrowTitl)

*!*************************************************************
*! Name      : lfBrowTrap
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfBrowTrap()
*!*************************************************************
FUNCTION lfBrowTrap

ON KEY LABEL Alt+W       DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+M       DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+V       DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL Alt+C       DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL ALT+P       DO  lpDoTrap WITH LASTKEY()     
ON KEY LABEL ENTER       DO  lpDoTrap WITH LASTKEY()       
ON KEY LABEL TAB         DO  lpTrap1
ON KEY LABEL BACKTAB     DO  lpTrap2 

*!*************************************************************
*! Name      : lpDoTrap
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lpDoTrap()
*!*************************************************************
PROCEDURE lpDoTrap
PARAMETER lnLstKey

=lfBrwUnTrp()

IF !EOF('PMRPRTM')
  DO CASE
    CASE lnLstKey = 17
      =lfvNew()
    CASE INLIST(lnLstKey,50,13)
      =lfvModify()
    CASE lnLstKey = 47 
      =lfvRemove()
    CASE lnLstKey = 46
      =lfvClose()  
    CASE lnLstKey = 25  
      =lfvOper()
  ENDCASE
ENDIF  

=lfBrowTrap()

*!*************************************************************
*! Name      : lfBrwUnTrp
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : Release browse trap.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfBrwUnTrp()
*!*************************************************************
FUNCTION lfBrwUnTrp

ON KEY LABEL Alt+W
ON KEY LABEL Alt+V
ON KEY LABEL Alt+M
ON KEY LABEL ENTER
ON KEY LABEL Alt+C
ON KEY LABEL TAB    
ON KEY LABEL BACKTAB

*!*************************************************************
*! Name      : lfvTmpCod
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : Template Code validation.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvTmpCod()
*!*************************************************************
FUNCTION lfvTmpCod

IF SEEK(lcTmpCode)
  lnChoice = gfModalgen("TRM38227B00000","DIALOG",TRIM(lcTmpCode))
  lcTmpCode = SPACE(3)
ENDIF

IF EMPTY(lcTmpCode)
  lcDesc = SPACE(30)
  SHOW GET lcTmpCode ENABLE
  SHOW GET lcDesc    DISABLE
  SHOW GET pbOper    DISABLE
  SHOW GET pbOk      DISABLE
ELSE
  SHOW GET lcTmpCode ENABLE
  SHOW GET lcDesc    ENABLE
  SHOW GET pbOper    ENABLE
  SHOW GET pbOk      ENABLE
ENDIF

*!*************************************************************
*! Name      : lfvOper
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : Selecting operation.
*!*************************************************************
*! Calls     : gfStr2Ar, gfMover
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvOper()
*!*************************************************************
FUNCTION lfvOper
PARAMETERS llViewOnly
STORE '' TO laAvailOprs


SELECT cOprt_Ctg + '\' + cOPrt_ID + ' ' + cOPrt_Dsc;
       FROM PMCTGDT ORDER BY cCtg_SEQ,cOprt_Ctg,cOPrt_Seq,cOPrt_ID;
       INTO ARRAY laAvailOprs

IF llViewOnly
  IF !EMPTY(mTmp_Oprt)
    =gfStr2Ar(ALLTRIM(mTmp_Oprt), @laSelOprs, '|')
    IF ALEN(laSelOprs) > 0
      FOR lnCount = 1 TO ALEN(laSelOprs)
        laSelOprs[lnCount] = SUBSTR(laSelOprs[lnCount], 1, 3) + '\' + ;
                             SUBSTR(laSelOprs[lnCount], 4, 5) + ' ' +;
                             LOOKUP(PMCTGDT.cOprt_Dsc, laSelOprs[lnCount],;
                             PMCTGDT.cOprt_Ctg, 'PMCTGDT')
      ENDFOR                               
    ENDIF  
  ENDIF  

  lcCurAlias = SELECT(0)
  SELECT (lcTempCurs)
  ZAP
  FOR lnCount = 1 TO ALEN(laSelOprs)
    INSERT INTO (lcTempCurs) (Oper) VALUES (laSelOprs[lnCount])
  ENDFOR
  LOCATE
  SET ORDER TO (lcTempCurs)
  DIME laSelOprs[1] 
  laSelOprs = ''
  lnI = 1
  SCAN
    DIME laSelOprs[lnI] 
    laSelOprs[lnI] = &lcTempCurs..Oper
    lnI = lnI + 1
  ENDSCAN
  SELECT (lcCurAlias)
  =gfMover(@laAvailOprs, @laSelOprs,"Tasks")
ELSE
  lcVldSlOpr = 'lfVldSlOpr()'
  llMovAll = .T.
  lcCurAlias = SELECT(0)
  SELECT (lcTempCurs)
  ZAP
  FOR lnCount = 1 TO ALEN(laSelOprs)
    INSERT INTO (lcTempCurs) (Oper) VALUES (laSelOprs[lnCount])
  ENDFOR
  LOCATE
  SET ORDER TO (lcTempCurs)
  DIME laSelOprs[1] 
  laSelOprs = ''
  lnI = 1
  SCAN
    DIME laSelOprs[lnI] 
    laSelOprs[lnI] = &lcTempCurs..Oper
    lnI = lnI + 1
  ENDSCAN
  SELECT (lcCurAlias)

  =gfMover(@laAvailOprs, @laSelOprs,"Selecting Tasks",;
        .T.,@lcVldSlOpr,.T.,.F.)
  
  lcFillMemo = ''
  IF !EMPTY(laSelOprs)
    FOR lnCount = 1 TO ALEN(laSelOprs)
      lcFillMemo = lcFillMemo + SUBSTR(laSelOprs[lnCount], 1, 3) + ;
                                SUBSTR(laSelOprs[lnCount], 5, 5) + '|' 
    ENDFOR  
  ENDIF  
  
  IF !EMPTY(lcFillMemo)
    lcFillMemo = SUBSTR(lcFillMemo , 1, LEN(lcFillMemo)-1)
  ENDIF
ENDIF


*!*************************************************************
*! Name      : lfVldSlOpr
*! Developer : HEND GHANEM
*! Date      : 12/16/2001
*! Purpose   : grantee the selected operation.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfVldSlOpr()
*!*************************************************************
FUNCTION lfVldSlOpr

IF _CUROBJ = OBJNUM(lsTarget) .OR. (TYPE("lnMovmnts")="N" .AND. (lnMovmnts = 3 OR lnMovmnts = 4))
  IF TYPE("lnMovmnts")="N" .AND. lnMovmnts = 4
    FOR lnI = 1 TO ALEN(laSource,1)
      IF SUBSTR(laSource[lnI],1,1) <> '\'
        laSource[lnI] = '\'+laSource[lnI]
      ENDIF
    ENDFOR
  ENDIF
  llMovAll = .T.
  RETURN .T.
ENDIF

IF ALEN(laTarget) = 10 AND llMovAll 
  IF TYPE('lnMovmnts') = 'N' AND lnMovmnts = 2
    llMovAll = .F.
  ENDIF
  = gfModalgen("TRM38229B00030","DIALOG")
  RETURN .F.
ELSE
  RETURN llMovAll
ENDIF