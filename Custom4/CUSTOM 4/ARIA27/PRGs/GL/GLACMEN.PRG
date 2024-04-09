*:************************************************************************
*:
*: Procedure file: GLACMEN.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Yasser Saad Ibrahim
*:                 Malak Hanna Aziz    
*:      Copyright (c) 
*:  Last modified:  /  /
*:
*:  Procs & Fncts: 
*:               
*:      Documented 12/22/1993
*:************************************************************************
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*E300692,1 ESSMAT 06/30/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*E300789,6 MAB    03/07/99. Remove cComp_ID from files
*
DECLARE laSegSiz[1,3],laBalRecs[1],laFileStru[1],laArowFlag[6,4]

** laCtrStat  && To disable the browse pad in the menu

STORE 0         TO lnOldRecs , lnOldRow
STORE "DISABLE" TO lcCurObj  , lcObjUpDat , lcObjGener , laCtrStat

lnAction     =  1
lc_BlankDBF  = ""
llProcessing = .T.
laArowFlag   = .F.


IF !gfSetup()    
  RETURN
ENDIF  

lcFuncName   = IIF(_DOS,'lfDosRef','lfWinRef')
lcNormalCol  = SCHEME(5,1)

SELECT GLTYPES
GO TOP
IF EOF()
  =gfModalGen("TRM02038B00000","DIALOG")
  glQuitting  = .T.  
  RETURN 
ENDIF

SELECT GLSEGVAL

FOR lnCounter = 1 TO lnAcsNoSeg
  SEEK(STR(lnCounter,1))
  IF ! FOUND()
    =gfModalGen("TRM02199B00000","DIALOG")
    glQuitting  = .T.  
    RETURN 
  ENDIF
ENDFOR

*SELECT DISTINCT SYCFSPRD.CFISFYEAR, SYCFSPRD.CFSPPRDID;
       FROM &gcSysHome.SYCFSPRD;
       WHERE sycfsprd.ccomp_id=gcAct_Comp .AND.;
       VAL(CFISFYEAR) >= lnCurr_yer - 1;
       ORDER BY SYCFSPRD.CFISFYEAR, SYCFSPRD.CFSPPRDID;
       INTO ARRAY laBalRecs

*E300692,1 CHANGE FILE NAME AND PATH FROM SYCFSPRD TO FSPRD
*SELECT DISTINCT SYCFSPRD.CFISFYEAR, SYCFSPRD.CFSPPRDID;
       FROM &gcSysHome.SYCFSPRD;
       WHERE sycfsprd.ccomp_id=gcPrnt_Cmp .AND.;
       VAL(CFISFYEAR) >= lnCurr_yer - 1;
       ORDER BY SYCFSPRD.CFISFYEAR, SYCFSPRD.CFSPPRDID;
       INTO ARRAY laBalRecs

*E300789,6 FSPRD file does not have company Id field now [Begin
*SELECT DISTINCT FSPRD.CFISFYEAR, FSPRD.CFSPPRDID;
*       FROM &gcDataDir.FSPRD;
*       WHERE fsprd.ccomp_id=gcPrnt_Cmp .AND.;
*       VAL(CFISFYEAR) >= lnCurr_yer - 1;
*       ORDER BY FSPRD.CFISFYEAR, FSPRD.CFSPPRDID;
*       INTO ARRAY laBalRecs

SELECT DISTINCT FSPRD.CFISFYEAR, FSPRD.CFSPPRDID;
       FROM &gcDataDir.FSPRD;
       WHERE VAL(CFISFYEAR) >= lnCurr_yer - 1;
       ORDER BY FSPRD.CFISFYEAR, FSPRD.CFSPPRDID;
       INTO ARRAY laBalRecs

*E300789,6 FSPRD file does not have company Id field now [End

*E300692,1 end

*E300692,1 CHANGE FILE NAME AND PATH FROM SYCACCOD TO ACCOD
*SELECT DISTINCT ;
       NACSSIZE,CACSSHDES,CACSSHDES,nacssegno;
  FROM  &gcSysHome.SYCACCOD;
  WHERE !EMPTY(SYCACCOD.nacssize) .AND. ;
       SYCACCOD.cComp_Id = gcAct_Comp ;
 ORDER BY nacssegno ;
 INTO  ARRAY laSegSiz
 
 *E300789,6 ACCOD file does not have company Id field now [Begin
 *SELECT DISTINCT ;
 *      NACSSIZE,CACSSHDES,CACSSHDES,nacssegno;
 * FROM  &gcDataDir.ACCOD;
 * WHERE !EMPTY(ACCOD.nacssize) .AND. ;
 *      ACCOD.cComp_Id = gcAct_Comp ;
 *ORDER BY nacssegno ;
 *INTO  ARRAY laSegSiz

 SELECT DISTINCT ;
       NACSSIZE,CACSSHDES,CACSSHDES,nacssegno;
  FROM  &gcDataDir.ACCOD;
  WHERE !EMPTY(ACCOD.nacssize) ;
 ORDER BY nacssegno ;
 INTO  ARRAY laSegSiz

 *E300789,6 ACCOD file does not have company Id field now [End
*E300692,1 end      

lnSegNo  = ALEN(laSegSiz,1) 

FOR lnCount = 1 TO lnSegNo

  laSegSiz[lnCount,3] = "Sg"+ALLTRIM(STR(lnCount))
  lcExclAry = "laExcAry"+STR(lnCount,1)
  DECLARE &lcExclAry[1]
  &lcExclAry = " "
  laSegSiz[lnCount,2] =ALLTRIM(laSegSiz[lnCount,2])

  IF !USED(laSegSiz[lnCount,3])
	SELECT 0
	USE &gcDataDir.GLSEGVAL	AGAIN ALIAS &laSegSiz[lnCount,3] ORDER 1
    lcFilter = "CACSSEGNO='"+STR(lnCount,1)+"'"
	SET FILTER TO &lcFilter
  ENDIF
  
  lcList1 = "lcSegRng"+STR(lnCount,1)+"1"
  lcList2 = "lcSegRng"+STR(lnCount,1)+"2" 
  GO TOP
  &lcList1= ALLTRIM(csegvalue)
  GO BOTTOM  
  &lcList2= ALLTRIM(csegvalue)

  SELECT GLSEGVAL
  lcArrayName = "laSegment"+STR(lnCount,1)
  DECLARE &lcArrayName[1]
 
  SELECT ALLTRIM(cSegValue)+' '+cSeglnDes FROM &laSegSiz[lnCount,3];
     WHERE CACSSEGNO = STR(lnCount,1);
     ORDER BY  cSegValue ;
     INTO ARRAY &lcArrayName  

  laArowFlag[lnCount,1] = .F.
  laArowFlag[lnCount,4] = .F.

  IF ALEN(&lcArrayName)   = 1
    laArowFlag[lnCount,2] = .F.
    laArowFlag[lnCount,3] = .F.
  ELSE
    laArowFlag[lnCount,2] = .T.
    laArowFlag[lnCount,3] = .T.
  ENDIF 

ENDFOR


lc_TempFile = gfTempName()

SELECT GLACCHAR
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]

laFileStru[lnFileStru+1,1] = 'cInclude'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0

CREATE TABLE &gcWorkDir.&lc_TempFile FROM ARRAY laFileStru 

SELECT (lc_TempFile)
INDEX ON  cAcctCode TAG cAcctCode
SET ORDER TO TAG cAcctCode


PUSH KEY CLEAR
=lfMainScrKey()

*E300683,5 Call *.SPR from screens directory
* DO GLACMEN.SPR 
DO (gcScrDir + gcWinAppl + '\GLACMEN.SPR')
*E300683,5 end   
POP KEY

glQuitting  = .T.  

FOR lnCount = 1 TO lnSegNo
  IF USED(laSegSiz[lnCount,3])
	USE IN ALIAS(laSegSiz[lnCount,3])
  ENDIF
ENDFOR

IF USED(lc_TempFile)
  USE IN ALIAS(lc_TempFile)
ENDIF

ERASE &gcWorkDir.&lc_TempFile..DBF
ERASE &gcWorkDir.&lc_TempFile..FPT
ERASE &gcWorkDir.&lc_TempFile..CDX


*!**************************************************************************
*!
*!      Function: lfvRange
*!
*!**************************************************************************
*
FUNCTION lfvRange

llSegChang = .F.
=lfSegRan()

IF llSegChang 
  lcObjGener = 'ENABLE'
  SHOW GET pbGenerat &lcObjGener
ENDIF  

*!**************************************************************************
*!
*!      Function: lfReadWhen
*!
*!**************************************************************************
*
FUNCTION lfReadWhen

DO CASE
  CASE _DOS
  lnAcsSeg = lnAcsSegSz
  CASE _WINDOWS
  lnAcsSeg = lnAcsSegSz+2
ENDCASE

lcBrString = "cInclude:H='Inc':W=lfwInclud():P='@M û, ':3,cAcctcode:H=lcAcsegDes:R:W=.F."+;
             ":"+STR(lnAcsSeg,2)+",cAccnldes:H='Account Description':R:W=.F."

BROWSE FIELDS &lcBrString;
       NOAPPEND;
       NODELETE;
       NOCLEAR;
       NOMENU;
       LOCK 0;
       FREEZE cInclude;
       SAVE;
       NOWAIT;
       TITLE 'Accounts';
       WHEN  lfCheckBut();
       WINDOW GLACMEN2 IN WINDOW (gcBaseWind)

=lfCheckBut()


*!**************************************************************************
*!
*!      Function: lfwInclud
*!
*!**************************************************************************
*
FUNCTION lfwInclud

IF LASTKEY() = 13 
  IF lnOldRow = MROW(WONTOP())
    =gfDclick(' ')
  ELSE
    lnOldRow = MROW(WONTOP())
  ENDIF
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvGenerat
*!
*!**************************************************************************
*
FUNCTION lfvGenerat

DECLARE laFilter[6,2]


IF RECCOUNT(lc_TempFile) > 0 
  IF gfModalGen("TRM02208B00012","DIALOG") = 1
    SELECT (lc_TempFile)
    ZAP
    =lfReadWhen()
  ELSE
    RETURN  
  ENDIF
ENDIF

lnTotAcont = 1
lnTotRec   = 0
lnSavTime = SECONDS()

lc_BlankDBF = gfTempName()    
SELECT GLACCHAR
COPY STRUCTURE TO &gcWorkDir.&lc_BlankDBF
USE &gcWorkDir.&lc_BlankDBF IN 0 EXCLUSIVE
SELECT (lc_BlankDBF)

FOR lnCounter = 1 TO 40
  APPEND BLANK
ENDFOR

SELECT GLACCHAR
SET ORDER TO TAG ACCTCODE

=gfThermo(100,1,'Generating Account...',' ')
laFilter = .F.

FOR lnCount = 1 TO lnSegNo

  SELECT (laSegSiz[lnCount,3])
  lcFromSeg  = "lcSegRng"+STR(lnCount,1)+"1"
  lcTOSeg    = "lcSegRng"+STR(lnCount,1)+"2"
  lcExcAry   = "laExcAry"+STR(lnCount,1)
  lnSegTotal = 0

  GO TOP
  COUNT FOR cSegValue >= EVALUATE(lcFromSeg)  .AND. ;
            cSegValue <= EVALUATE(lcTOSeg)    .AND. ;
            ASCAN(&lcExcAry,RTRIM(cSegValue)) = 0 ;
            TO lnSegTotal
 
  lnTotAcont = lnTotAcont * IIF(lnSegTotal=0,1,lnSegTotal)
  GO TOP
  lcSegFilt =  [CACSSEGNO =']+STR(lnCount,1)+;
    [' AND BETWEEN(cSegValue,']+EVAL(lcFromSeg)+[',']+EVAL(lcToSeg)+[')]
    
    *[ AND ASCAN(']+lcExcAry+[',RTRIM(cSegValue)) = 0]
  SET FILTER TO &lcSegFilt

ENDFOR

lcEscStat = SET('ESCAPE')
lcOnEsc   = ON('ESCAPE')
SET ESCAPE ON 
ON ESCAPE llProcessing = .F.
llProcessing = .T.

SELECT (laSegSiz[1,3])

SCAN ALL FOR ASCAN(laExcAry1,RTRIM(cSegValue)) = 0 AND llProcessing

IF lfTerminate() 
  EXIT
ENDIF

  SCATTER MEMVAR MEMO
  IF TYPE('laSegSiz[2,3]') = 'C'
    SELECT (laSegSiz[2,3])
    SCAN ALL FOR ASCAN(laExcAry2,RTRIM(cSegValue)) = 0 AND  llProcessing
      IF TYPE('laSegSiz[3,3]') = 'C'
        SELECT (laSegSiz[3,3])
        SCAN ALL FOR ASCAN(laExcAry3,RTRIM(cSegValue)) = 0 AND  llProcessing
          IF TYPE('laSegSiz[4,3]') = 'C'
            SELECT (laSegSiz[4,3])
            SCAN ALL FOR  ASCAN(laExcAry4,RTRIM(cSegValue)) = 0 AND llProcessing
              IF TYPE('laSegSiz[5,3]') = 'C'
                SELECT (laSegSiz[5,3])
                SCAN ALL FOR ASCAN(laExcAry5,RTRIM(cSegValue)) = 0 AND  llProcessing
                  IF TYPE('laSegSiz[6,3]') = 'C'
                    SELECT (laSegSiz[6,3])
                    SCAN ALL FOR ASCAN(laExcAry6,RTRIM(cSegValue)) = 0 AND  llProcessing
                      =lfUpdTemp() 
                      =lfTerminate() 
                    ENDSCAN && 6
                  ELSE   && 5
                    =lfUpdTemp()   
                    =lfTerminate() 
                  ENDIF  && 5
                ENDSCAN  && 5
              ELSE   && 4
                =lfUpdTemp()  
                =lfTerminate() 
              ENDIF  && 4
            ENDSCAN  && 4
          ELSE   && 3 
            =lfUpdTemp()   
            =lfTerminate() 
          ENDIF  && 3
        ENDSCAN  && 3
      ELSE  && 2
        =lfUpdTemp()   
        =lfTerminate() 
      ENDIF && 2
    ENDSCAN && 2
  ELSE  && 1
    =lfUpdTemp()   
    =lfTerminate() 
  ENDIF && 1
ENDSCAN && 1

IF USED(lc_BlankDBF)
  USE IN ALIAS(lc_BlankDBF)
ENDIF
ERASE &gcWorkDir.&lc_BlankDBF..DBF

FOR lnCount = 1 TO lnSegNo
  SELECT (laSegSiz[lnCount,3])
  lcFilter = "'"+STR(lnCount,1)+"'"
  SET FILTER TO CACSSEGNO=&lcFilter
ENDFOR


ON  ESCAPE &lcOnEsc
SET ESCAPE &lcEscStat

=gfThermo(100,100,'Generating Account...')

lcTotTime = STR(SECONDS()-lnSavTime,8,3)

*WAIT ALLTRIM(STR(lnTotRec))+" Accounts Generated in "+lcTotTime+" seconds " WINDOW NOWAIT
lcTmpStr = ALLTRIM(STR(lnTotRec)) + "|" + lcTotTime
=gfWait("02232","NOWAIT",lcTmpStr)

lcObjGener = 'DISABLE'
SHOW GET pbGenerat &lcObjGener
SELECT (lc_TempFile)
DELETE ALL FOR EMPTY(&lc_TempFile..cAcctcode)
GO TOP
=lfReadWhen()


*!**************************************************************************
*!
*!      Function: lfUpdTemp
*!
*!**************************************************************************
*
FUNCTION lfUpdTemp

lcOldAlias= ALIAS()

lcAccount = ""
lcLongDes = ""
lcSortDes = ""

FOR lnCount = 1 TO lnSegNo
**** error
  lcAccount = lcAccount + IIF(EMPTY(lcAccount),"","-") +;
              ALLTRIM(EVALUATE[laSegSiz[lnCount,3]+".cSegValue"])

  lcLongDes = lcLongDes + IIF(EMPTY(lcLongDes),"",IIF(EMPTY(EVALUATE[laSegSiz[lnCount,3]+'.cSegShDes']),"","-")) +;
              ALLTRIM(EVALUATE[laSegSiz[lnCount,3]+".cSeglnDes"])

  lcSortDes = lcLongDes + IIF(EMPTY(lcSortDes),"",IIF(EMPTY(EVALUATE[laSegSiz[lnCount,3]+'.cSegShDes']),"","-")) +;
              ALLTRIM(EVALUATE[laSegSiz[lnCount,3]+".cSegShDes"])

ENDFOR

IF rbAction = 1
  IF  !SEEK(lcAccount,'GLACCHAR')
    SELECT (lc_TempFile)
    GO TOP
    IF  RECCOUNT() = 0 .OR. !EMPTY(&lc_TempFile..cAcctcode)
      APPEND FROM &gcWorkDir.&lc_BlankDBF
      GO TOP
    ENDIF
    
    REPLACE  &lc_TempFile..cInclude   WITH "û"      ;
             &lc_TempFile..cAcctcode  WITH lcAccount ;
             &lc_TempFile..cAccnsdes  WITH lcSortDes ; 
             &lc_TempFile..cAccnldes  WITH lcLongDes ; 
             &lc_TempFile..lLok_Stat  WITH .F.       ; 
             &lc_TempFile..cLok_User  WITH ''        ; 
             &lc_TempFile..dLok_Date  WITH {}        ; 
             &lc_TempFile..cLok_Time  WITH ''

    GATHER MEMVAR MEMO              
    lnTotRec   = lnTotRec   + 1
    lnCurrunt  = RECNO(lc_TempFile) 
    =gfThermo(lnTotAcont,lnCurrunt,'Generating Account:',lcAccount)
  ELSE
    lnTotAcont = lnTotAcont - 1 
  ENDIF  
ELSE
  IF  SEEK(lcAccount,'GLACCHAR')
    SELECT (lc_TempFile)
    GO TOP
    IF  RECCOUNT() = 0 .OR. !EMPTY(&lc_TempFile..cAcctcode)
      APPEND FROM &gcWorkDir.&lc_BlankDBF
      GO TOP
    ENDIF
    
    REPLACE  &lc_TempFile..cInclude   WITH "û"      ;
             &lc_TempFile..cAcctcode  WITH lcAccount ;
             &lc_TempFile..cAccnldes  WITH lcLongDes
    lnTotRec   = lnTotRec   + 1
    lnCurrunt  = RECNO(lc_TempFile) 
    =gfThermo(lnTotAcont,lnCurrunt,'Generating Account...',lcAccount)
  ELSE
    lnTotAcont = lnTotAcont - 1 
  ENDIF  
ENDIF  

SELECT &lcOldAlias

*!**************************************************************************
*!
*!      Function: lfvMUpdate
*!
*!**************************************************************************
*
FUNCTION lfvMUpdate

lnCurrunt = 0 
SELECT (lc_TempFile)

COUNT FOR !EMPTY(cInclude) TO lnTotAcont

IF lnTotAcont = 0 
  =gfModalGen("TRM02103B00000","DIALOG",IIF(rbAction = 1,'add','delete'))   
  _CUROBJ=OBJNUM(pbSelOne)
  RETURN
ENDIF

lcEscStat = SET('ESCAPE')
lcOnEsc   = ON('ESCAPE')
SET ESCAPE ON 
ON ESCAPE llProcessing = .F.

llProcessing = .T.

*** Adding Accounts
IF rbAction = 1
  lcTime = gfGetTime() 
  ldDate = DATE()
  SELECT (lc_TempFile)
  REPLACE ALL &lc_TempFile..cAdd_User WITH gcUser_Id  ;
              &lc_TempFile..dAdd_Date WITH ldDate     ;
              &lc_TempFile..cAdd_Time WITH lcTime     

  lc_Balance = gfTempName()
  SELECT GLACBALS
  COPY STRUCTURE TO &gcWorkDir.&lc_Balance
  SELECT GLACBALS

  FOR lnCount = 1 TO ALEN(laBalRecs,1)
    INSERT INTO &gcWorkDir.&lc_Balance ;
       (cAcctCode,cFisfYear,cFspPrdid,nacbptddr,nacbptdcr,;
        nacbytddr,nacbytdcr,nacbopbal,nacbclbal,cAdd_User,;
        dAdd_Date,cAdd_Time);
        VALUES ('00',laBalRecs[lnCount,1],laBalRecs[lnCount,2],;
        0,0,0,0,0,0,gcUser_Id,ldDate,lcTime)
  ENDFOR

  SELECT (lc_TempFile)
   
  SCAN FOR !EMPTY(cInclude)
    IF lfTerminate() 
      EXIT
    ENDIF

    lnCurrunt = lnCurrunt + 1
    lcAccount = cAcctCode
    =gfThermo(lnTotAcont,lnCurrunt,'Updating Account...',lcAccount)

    IF !SEEK(lcAccount,'GLACCHAR')
      SCATTER MEMVAR MEMO
      SELECT GLACCHAR
      APPEND BLANK
      GATHER MEMVAR MEMO

      SELECT (lc_Balance)
      REPLACE ALL &lc_Balance..cAcctCode With &lc_TempFile..cAcctCode 
      SELECT GLACBALS
      APPEND FROM &gcWorkDir.&lc_Balance 
      SELECT (lc_TempFile)
      DELETE
    ENDIF
    SELECT (lc_TempFile)
  ENDSCAN

  IF USED(lc_Balance)
    USE IN ALIAS(lc_Balance)
  ENDIF
  ERASE &gcWorkdir.&lc_Balance..DBF

*** Deleting Accounts
ELSE

  SCAN FOR !EMPTY(cInclude)

    IF lfTerminate() 
      EXIT
    ENDIF
    lnCurrunt = lnCurrunt + 1
    lcAccount = cAcctCode
    =gfThermo(lnTotAcont,lnCurrunt,'Deleting Account...',lcAccount)
 
    SELECT GLACCHAR
    SET ORDER TO TAG ACCTCODE
    SEEK(lcAccount)

    IF gfObj_Lock(.T.)
      SELECT (lc_TempFile)
      IF lfDelAcnt(lcAccount)
        DELETE
      ENDIF
    ENDIF
   SELECT (lc_TempFile)
  ENDSCAN
ENDIF  

ON  ESCAPE &lcOnEsc
SET ESCAPE &lcEscStat

IF lnCurrunt <> lnTotAcont
  =gfThermo(100,100,' ',' ')
ENDIF
  
SELECT (lc_TempFile)
PACK
GO TOP 
=lfReadWhen()
lcObjGener = 'DISABLE'
SHOW GET pbGenerat &lcObjGener


*!**************************************************************************
*!
*!      Procedure:  lpTab
*!
*!**************************************************************************
*
PROCEDURE lpTab 
           
IF WONTOP('Accounts')
  ACTIVATE WINDOW GLACMEN3
  IF lcObjUpDat = 'ENABLE'
    _CUROBJ=OBJNUM(pbSelOne)
  ELSE  
    _CUROBJ=OBJNUM(pbClose)
  ENDIF  
ELSE
  IF WONTOP('GLACMEN3')
    IF _CUROBJ = OBJNUM(pbClose)
      ACTIVATE WINDOW GLACMEN1
    ELSE  
      _CUROBJ= _CUROBJ + 1 
    ENDIF
  ELSE  

    IF _CUROBJ=OBJNUM(pbRange) AND lcObjGener='DISABLE' AND lcObjUpDat='DISABLE'
      ACTIVATE WINDOW ACCOUNTS
    ELSE  
      IF _CUROBJ = OBJNUM(pbUpDate) 
        IF lcObjGener = 'DISABLE'
          ACTIVATE WINDOW ACCOUNTS
        ELSE
          _CUROBJ= _CUROBJ + 1
        ENDIF  
      ELSE  
        IF _CUROBJ = OBJNUM(pbGenerat) 
          ACTIVATE WINDOW ACCOUNTS
        ELSE
          _CUROBJ= _CUROBJ + 1
        ENDIF
      ENDIF  
    ENDIF
  ENDIF  
ENDIF

   
*!**************************************************************************
*!
*!      Procedure:  lpShiftTab
*!
*!**************************************************************************
*
PROCEDURE lpShiftTab 

IF WONTOP('Accounts')
  ACTIVATE WINDOW GLACMEN1
  IF lcObjGener='DISABLE' AND lcObjUpDat='DISABLE'  
    _CUROBJ = OBJNUM(pbRange)
  ELSE
    IF lcObjGener='ENABLE' 
      _CUROBJ = OBJNUM(pbGenerat) 
    ELSE  
      IF lcObjUpDat='ENABLE' 
        _CUROBJ = OBJNUM(pbUpDate)
      ELSE
        _CUROBJ = OBJNUM(pbRange)  
      ENDIF
    ENDIF   
  ENDIF  
ELSE
  IF WONTOP('GLACMEN1')
    IF _CUROBJ = OBJNUM(rbAction) AND rbAction = 1
      ACTIVATE WINDOW GLACMEN3
      _CUROBJ=OBJNUM(pbClose)
    ELSE  
      IF _CUROBJ = OBJNUM(pbGenerat) AND lcObjUpDat='DISABLE' 
        _CUROBJ  = OBJNUM(pbRange)
      ELSE 
        _CUROBJ= _CUROBJ - 1
      ENDIF
    ENDIF   
  ELSE  

    IF (_CUROBJ = OBJNUM(pbClose) AND lcObjUpDat = 'DISABLE') ;
              OR _CUROBJ = OBJNUM(pbSelOne)
      ACTIVATE WINDOW ACCOUNTS
    ELSE  
      _CUROBJ= _CUROBJ - 1
    ENDIF  

  ENDIF  
ENDIF



*!**************************************************************************
*!
*!      PROCEDURE: lpEscap
*!
*!**************************************************************************
*
PROCEDURE lpEscap

_CUROBJ = OBJNUM(pbClose)
KEYBOARD CHR(17)+CHR(13)



*!**************************************************************************
*!
*!      Function: lfvAction
*!
*!**************************************************************************
*
FUNCTION lfvAction

IF rbAction = lnAction
  RETURN
ELSE
  rbAction = lnAction 
  SHOW GET   rbAction
ENDIF

IF RECCOUNT(lc_TempFile) > 0 
  IF gfModalGen("TRM02207B00012","DIALOG") = 1
    SELECT (lc_TempFile)
    ZAP
    =lfReadWhen()
  ELSE
    RETURN  
  ENDIF
ENDIF


rbAction = IIF(rbAction=1,2,1)
lnAction = rbAction 
SHOW GET   rbAction

IF RECCOUNT(lc_TempFile) > 0 
  lcObjUpDat = 'ENABLE'
ELSE  
  lcObjUpDat = 'DISABLE'
ENDIF 

IF rbAction = 1 
  SHOW GET pbUpdate,1 PROMPT 'Add to \<Master File'      &lcObjUpDat
ELSE  
  SHOW GET pbUpdate,1 PROMPT 'D\<elete from Master File' &lcObjUpDat
ENDIF  


*!**************************************************************************
*!
*!      Function:  lfCheckBut
*!
*!**************************************************************************
*
FUNCTION  lfCheckBut

SELECT (lc_TempFile)

IF RECCOUNT() > 0 
  IF !EMPTY(cInclude)
    SHOW GET pbSelOne,1 PROMPT '\<Unselect' ENABLE
  ELSE                                   
    SHOW GET pbSelOne,1 PROMPT '\<Select' ENABLE
  ENDIF
  lcObjUpDat = 'ENABLE'
ELSE
  lcObjUpDat = 'DISABLE'
ENDIF  

SHOW GET pbSelOne  &lcObjUpDat
SHOW GET pbSelAll  &lcObjUpDat
SHOW GET pbSelNon  &lcObjUpDat
SHOW GET pbInvert  &lcObjUpDat
SHOW GET pbUpdate  &lcObjUpDat
SHOW WINDOW ('Accounts') REFRESH


*!**************************************************************************
*!					
*!      Function:  lfvSelOne
*!
*!**************************************************************************
*
FUNCTION  lfvSelOne

SELECT (lc_TempFile)
REPLACE cInclude WITH IIF(EMPTY(cInclude),'û','  ')
=lfCheckBut()


*!**************************************************************************
*!
*!      Function:  lfvSelAll 
*!
*!**************************************************************************
*

FUNCTION  lfvSelAll 

SELECT (lc_TempFile)
lnRecNo = RECNO()
REPLACE ALL cInclude WITH 'û'

IF lnRecNo > RECCOUNT()
  GO BOTTOM
ELSE 
  GOTO lnRecNo
ENDIF  

=lfCheckBut()


*!**************************************************************************
*!
*!      Function:  lfvSelNon 
*!
*!**************************************************************************
*
FUNCTION  lfvSelNon 

SELECT (lc_TempFile)
lnRecNo = RECNO()
REPLACE ALL cInclude WITH '  '

IF lnRecNo > RECCOUNT()
  GO BOTTOM
ELSE 
  GOTO lnRecNo
ENDIF

=lfCheckBut()

*!**************************************************************************
*!
*!      Function:  lfvInvert 
*!
*!**************************************************************************
*
FUNCTION  lfvInvert 

SELECT (lc_TempFile)
lnRecNo = RECNO()
REPLACE ALL cInclude WITH IIF(EMPTY(cInclude),'û','  ')

IF lnRecNo > RECCOUNT()
  GO BOTTOM
ELSE 
  GOTO lnRecNo
ENDIF

=lfCheckBut()


*!**************************************************************************
*!
*!      Function: lfChildScrKey
*!
*!**************************************************************************
*
FUNCTION lfChildScrKey

ON KEY LABEL UPARROW  DO lpMoveUp
ON KEY LABEL DNARROW  DO lpMoveDn
ON KEY LABEL ESC      DO lpEscRange
ON KEY LABEL HOME     lnDummy = 1 
ON KEY LABEL END      lnDummy = 1 


*!**************************************************************************
*!
*!      Function: lfMainScrKey
*!
*!**************************************************************************
*
FUNCTION lfMainScrKey

ON KEY LABEL TAB        DO lpTab 
ON KEY LABEL RIGHTARROW DO lpTab
ON KEY LABEL LEFTARROW  DO lpShiftTab
ON KEY LABEL BACKTAB    DO lpShiftTab 
ON KEY LABEL ESC        DO lpEscap
ON KEY LABEL Ctrl+ENTER lnDumi = 1
ON KEY LABEL CTRL+W     lnDumi = 1
ON KEY LABEL CTRL+Q     lnDumi = 1
ON KEY LABEL CTRL+END   lnDumi = 1


*!**************************************************************************
*!
*!      Function: lfNextValid
*!
*!**************************************************************************
*
FUNCTION lfNextValid
PARAMETERS lcArray,lnArrayPos,lcCurObj

IF lnArrayPos = 0 .OR. lnArrayPos > ALEN(&lcArray)
  RETURN .F.
ENDIF

lcOtherObj   = IIF(SUBSTR(lcCurObj,10,1)='1',;
                   SUBSTR(lcCurObj,1,9)+'2' ,;
                   SUBSTR(lcCurObj,1,9)+'1')

lcNewValue   = SUBSTR(&lcArray[lnArrayPos],1,AT(' ',&lcArray[lnArrayPos]))                    

IF SUBSTR(lcCurObj,10,1)= '1' 
  IF VAL(EVALUATE(lcOtherObj)) >= VAL(lcNewValue)
    RETURN .T.
  ENDIF
ELSE  
  IF VAL(EVALUATE(lcOtherObj)) <= VAL(lcNewValue)
    RETURN .T.
  ENDIF
ENDIF

RETURN .F.


*!**************************************************************************
*!
*!      PROCEDURE:  lpMoveUp
*!
*!**************************************************************************
*
PROCEDURE lpMoveUp

CLEAR TYPEAHEAD
ON KEY LABEL UPARROW  lnDumi = 1
ON KEY LABEL DNARROW  lnDumi = 1


lcCurObj = SYS(18)
IF SUBSTR(lcCurObj,1,8) <> 'IBSEGRNG'
  =lfChildScrKey()
  RETURN
ENDIF

lcArrayNam = 'laSegment' + SUBSTR(lcCurObj,9,1)
lcSayField = 'lc'+ SUBSTR(lcCurObj,3)
lnWhereVal =  ASCAN(&lcArrayNam,EVALUATE(lcSayField))

IF lnWhereVal = 1 
  =lfChildScrKey()
  RETURN
ENDIF

IF lnWhereVal <> 1 AND lfNextValid(lcArrayNam,lnWhereVal-1,lcSayField)
  lnWhereVal   = lnWhereVal - 1  
  lnArrayCol   = IIF(SUBSTR(lcCurObj,10,1)='1',2,4)
  lnArrayRow   = INT(VAL(SUBSTR(lcCurObj,9 ,1)))
  &lcSayField  = SUBSTR(&lcArrayNam[lnWhereVal],1,AT(' ',&lcArrayNam[lnWhereVal]))
  lcSeglndes   = SUBSTR(&lcArrayNam[lnWhereVal],AT(' ',&lcArrayNam[lnWhereVal]))
  laArowFlag[lnArrayRow,lnArrayCol] = .T.  
  lcInvisable  = 'ibMove'+STR(lnArrayRow,1)+STR(lnArrayCol,1)
  SHOW GET &lcInvisable ENABLE COLOR ,,,,,,,,&lcNormalCol,&lcNormalCol
 
  IF lnWhereVal = 1
    lnArrayCol = IIF(SUBSTR(lcCurObj,10,1)='1',1,3)
    lnArrayRow = INT(VAL(SUBSTR(lcCurObj,9 ,1)))
    laArowFlag[lnArrayRow,lnArrayCol] = .F.
    lcInvisable  = 'ibMove'+STR(lnArrayRow,1)+STR(lnArrayCol,1)
    SHOW GET &lcInvisable DISABLE COLOR ,,,,,,,,&lcNormalCol,&lcNormalCol
  ENDIF  

  SHOW GETS OFF
  SHOW GET lcSeglndes DISABLE
ENDIF

=lfChildScrKey()


*!**************************************************************************
*!
*!      PROCEDURE:  lpMoveDn
*!
*!**************************************************************************
*
PROCEDURE lpMoveDn

CLEAR TYPEAHEAD
ON KEY LABEL DNARROW  lnDumi = 1
ON KEY LABEL UPARROW  lnDumi = 1



lcCurObj = SYS(18)
IF SUBSTR(lcCurObj,1,8) <> 'IBSEGRNG'
  =lfChildScrKey()
  RETURN
ENDIF

lcArrayNam = 'laSegment' + SUBSTR(lcCurObj,9,1)
lcSayField = 'lc'+ SUBSTR(lcCurObj,3)
lnWhereVal =  ASCAN(&lcArrayNam,EVALUATE(lcSayField))

IF lnWhereVal = ALEN(&lcArrayNam,1)
  =lfChildScrKey()
  RETURN
ENDIF

IF lnWhereVal <> ALEN(&lcArrayNam,1) AND ;
   lfNextValid(lcArrayNam,lnWhereVal+1,lcSayField)
  lnWhereVal  = lnWhereVal + 1
  lnArrayCol  = IIF(SUBSTR(lcCurObj,10,1)='1',1,3)
  lnArrayRow  = INT(VAL(SUBSTR(lcCurObj,9,1)))
  &lcSayField = SUBSTR(&lcArrayNam[lnWhereVal],1,AT(' ',&lcArrayNam[lnWhereVal]))
  lcSeglndes  = SUBSTR(&lcArrayNam[lnWhereVal],AT(' ',&lcArrayNam[lnWhereVal]))
  laArowFlag[lnArrayRow,lnArrayCol] = .T.
  lcInvisable  = 'ibMove'+STR(lnArrayRow,1)+STR(lnArrayCol,1)
  SHOW GET &lcInvisable ENABLE COLOR ,,,,,,,,&lcNormalCol,&lcNormalCol
 
  IF lnWhereVal = ALEN(&lcArrayNam,1)
    lnArrayCol = IIF(SUBSTR(lcCurObj,10,1)='1',2,4)
    lnArrayRow = INT(VAL(SUBSTR(lcCurObj,9,1)))
    laArowFlag[lnArrayRow,lnArrayCol] = .F.
    lcInvisable  = 'ibMove'+STR(lnArrayRow,1)+STR(lnArrayCol,1)
    SHOW GET &lcInvisable DISABLE COLOR ,,,,,,,,&lcNormalCol,&lcNormalCol
  ENDIF  

  SHOW GETS OFF
  SHOW GET lcSeglndes DISABLE
ENDIF

=lfChildScrKey()


*!**************************************************************************
*!
*!      Function: lfwMoveUp
*!
*!**************************************************************************
*
FUNCTION lfwMoveUp
PARAMETERS lcSayField

llDoItOnce = .T.
lcArrayNam = 'laSegment' + SUBSTR(lcSayField,9,1)
lnWhereVal = ASCAN(&lcArrayNam,EVALUATE(lcSayField))

IF lnWhereVal = 1 
  =lfChildScrKey()
  RETURN
ENDIF

lnMouseCol = MCOL()
lnMouseRow = MROW()

DO CASE
  CASE _DOS
    lnDeltaCol  = INT(lnMaxSiz/2)
    lnDeltaRow  = 0

  CASE _WINDOWS
    lnDeltaCol  = lnMaxSiz/2
    lnDeltaRow  = 0.6
ENDCASE  


DO WHILE  ( MDOWN() OR llDoItOnce ) ;
   AND lfNextValid(lcArrayNam,lnWhereVal-1,lcSayField) ;
   AND BETWEEN(MCOL(),lnMouseCol-lnDeltaCol,lnMouseCol+lnDeltaCol) ;
   AND BETWEEN(MROW(),lnMouseRow-lnDeltaRow,lnMouseRow+lnDeltaRow)

  llDoItOnce = .F.

  IF lnWhereVal <> 1
    lnArrayCol = IIF(SUBSTR(lcSayField,10,1)='1',2,4)
    lnArrayRow = INT(VAL(SUBSTR(lcSayField,9,1)))
    laArowFlag[lnArrayRow,lnArrayCol] = .T.
    lcInvisable  = 'ibMove'+STR(lnArrayRow,1)+STR(lnArrayCol,1)
    SHOW GET &lcInvisable ENABLE COLOR ,,,,,,,,&lcNormalCol,&lcNormalCol

    lnWhereVal= lnWhereVal - 1
    &lcSayField  = SUBSTR(&lcArrayNam[lnWhereVal],1,AT(' ',&lcArrayNam[lnWhereVal]))
    lcSeglndes   = SUBSTR(&lcArrayNam[lnWhereVal],AT(' ',&lcArrayNam[lnWhereVal]))
    SHOW GET lcSeglndes DISABLE
    =lfRefresh()

    DO CASE
      CASE _DOS
        lnDumyLoop = 500
      CASE _WINDOWS
        lnDumyLoop = 250
    ENDCASE 

    FOR lnDumDelay = 1 TO lnDumyLoop 
    ENDFOR
 
    IF lnWhereVal = 1
      lnArrayCol = IIF(SUBSTR(lcSayField,10,1)='1',1,3)
      lnArrayRow = INT(VAL(SUBSTR(lcSayField,9,1)))
      laArowFlag[lnArrayRow,lnArrayCol] = .F.
      lcInvisable  = 'ibMove'+STR(lnArrayRow,1)+STR(lnArrayCol,1)
      SHOW GET &lcInvisable DISABLE COLOR ,,,,,,,,&lcNormalCol,&lcNormalCol
      SHOW GETS OFF
    ENDIF  
  ENDIF    
  
ENDDO

lcObject = 'IB'+ SUBSTR(lcSayField,3)
_CUROBJ = OBJNUM(&lcObject)



*!**************************************************************************
*!
*!      Function: lfwMoveDn
*!
*!**************************************************************************
*
FUNCTION lfwMoveDn
PARAMETERS lcSayField
llDoItOnce = .T.

lcArrayNam = 'laSegment' + SUBSTR(lcSayField,9,1)
lnWhereVal =  ASCAN(&lcArrayNam,EVALUATE(lcSayField))

lnMouseCol = MCOL()
lnMouseRow = MROW()

DO CASE
  CASE _DOS
    lnDeltaCol  = INT(lnMaxSiz/2)
    lnDeltaRow  = 0

  CASE _WINDOWS
    lnDeltaCol  = lnMaxSiz/2
    lnDeltaRow  = 0.6
ENDCASE  


DO WHILE  ( MDOWN() OR llDoItOnce ) ;
   AND lfNextValid(lcArrayNam,lnWhereVal+1,lcSayField) ;
   AND BETWEEN(MCOL(),lnMouseCol-lnDeltaCol,lnMouseCol+lnDeltaCol) ;
   AND BETWEEN(MROW(),lnMouseRow-lnDeltaRow,lnMouseRow+lnDeltaRow)

  llDoItOnce = .F.
  
  IF lnWhereVal <> ALEN(&lcArrayNam,1)
    lnArrayCol = IIF(SUBSTR(lcSayField,10,1)='1',1,3)
    lnArrayRow = INT(VAL(SUBSTR(lcSayField,9,1)))
    laArowFlag[lnArrayRow,lnArrayCol] = .T.
    lcInvisable  = 'ibMove'+STR(lnArrayRow,1)+STR(lnArrayCol,1)
    SHOW GET &lcInvisable ENABLE COLOR ,,,,,,,,&lcNormalCol,&lcNormalCol

    lnWhereVal  = lnWhereVal + 1
    &lcSayField = SUBSTR(&lcArrayNam[lnWhereVal],1,AT(' ',&lcArrayNam[lnWhereVal]))
    lcSeglndes  = SUBSTR(&lcArrayNam[lnWhereVal],AT(' ',&lcArrayNam[lnWhereVal]))
    SHOW GET lcSeglndes DISABLE
    =lfRefresh()
     
    DO CASE
      CASE _DOS
        lnDumyLoop = 500
      CASE _WINDOWS
        lnDumyLoop = 250
    ENDCASE 

    FOR lnDumDelay = 1 TO lnDumyLoop 
    ENDFOR

    IF lnWhereVal = ALEN(&lcArrayNam,1)
      lnArrayCol  = IIF(SUBSTR(lcSayField,10,1)='1',2,4)
      lnArrayRow  = INT(VAL(SUBSTR(lcSayField,9,1)))
      laArowFlag[lnArrayRow,lnArrayCol] = .F.
      lcInvisable = 'ibMove'+STR(lnArrayRow,1)+STR(lnArrayCol,1)
      SHOW GET &lcInvisable DISABLE COLOR ,,,,,,,,&lcNormalCol,&lcNormalCol
      SHOW GETS OFF
    ENDIF  
  ENDIF    
ENDDO

lcObject = 'IB'+ SUBSTR(lcSayField,3)
_CUROBJ = OBJNUM(&lcObject)


*!**************************************************************************
*!
*!   PROCEDURE : lpEscRange
*!
*!**************************************************************************
*
PROCEDURE lpEscRange
_CUROBJ = OBJNUM(pbRCancel)
KEYBOARD "{ENTER}"


*!**************************************************************************
*!
*!      Function:  lfSegRan
*!
*!**************************************************************************
*
FUNCTION lfSegRan
ON KEY
=lfChildScrKey()

DECLARE laObjdisp[6]


lnMaxSiz   = 1
laObjdisp  = .F.

FOR lnCount = 1 TO lnSegNo
  lnMaxSiz = MAX(lnMaxSiz,laSegSiz[lnCount,1])

  IF lnMaxSiz %2 = 0 
    IF _DOS
      lnMaxSiz = lnMaxSiz + 1
    ENDIF  
  ENDIF

  laObjdisp[lnCount] = .T. 
ENDFOR  

lnHight  = (lnSegNo*3)+IIF(_DOS,4,15)
lnWidth  = (lnMaxSiz*3)+IIF(_DOS,32,29)

SELECT GLSEGVAL

  
*E300683,5 Call *.SPR from screens directory
* DO GLSEGRAN.SPR 
DO (gcScrDir + gcWinAppl + '\GLSEGRAN.SPR')
*E300683,5 end   
ON KEY
=lfMainScrKey()


*!**************************************************************************
*!
*!      Function: lfwSegment
*!
*!**************************************************************************
*
FUNCTION lfwSegment

FOR lnXCounter = 1 TO lnSegNo
  FOR lnYCounter = 1 TO 4
    lcEnbOrDis= IIF(laArowFlag[lnXCounter,lnYCounter],'ENABLE','DISABLE')
    lcInvisable  = 'ibMove'+STR(lnXCounter,1)+STR(lnYCounter,1)
    SHOW GET &lcInvisable &lcEnbOrDis COLOR ,,,,,,,,&lcNormalCol,&lcNormalCol  
  ENDFOR
ENDFOR  


*!**************************************************************************
*!
*!      Function: lfwSegRng
*!
*!**************************************************************************
*
FUNCTION lfwSegRng

lcCurObj = SYS(18)
lcArrayNam = 'laSegment' + SUBSTR(lcCurObj,9,1)
lcSayField = 'lc'+ SUBSTR(lcCurObj,3)
lnWhereVal =  ASCAN(&lcArrayNam,EVALUATE(lcSayField))
lcSeglndes   = SUBSTR(&lcArrayNam[lnWhereVal],AT(' ',&lcArrayNam[lnWhereVal]))
SHOW GET lcSeglndes DISABLE


*!**************************************************************************
*!
*!      Function: lfvCancRan
*!
*!**************************************************************************
*
FUNCTION lfvCancRan
llSegChang = .F.

*!**************************************************************************
*!
*!      Function: lfvOkRange
*!
*!**************************************************************************
*
FUNCTION lfvOkRange

llSegChange = .T.
CLEAR READ

*!**************************************************************************
*!
*!      Function: lfvExclud
*!
*!**************************************************************************
*
FUNCTION lfvExclud
DECLARE laSource[1]

lcObjNam   = SYS(18)
lnAliasNum = VAL(SUBSTR(lcObjNam,9,1))
lcFromSeg  = "lcSegRng"+STR(lnAliasNum,1)+"1"
lcTOSeg    = "lcSegRng"+STR(lnAliasNum,1)+"2"
lcExcAry   = "laExcAry"+STR(lnAliasNum,1)

SELECT ALLTRIM(cSegValue)+' '+cSeglnDes ;
       FROM &laSegsiz[lnAliasNum,3];
       WHERE cAcsSegNo = STR(lnAliasNum,1)   .AND.;
             cSegValue>= EVALUATE(lcFromSeg) .AND.;
             cSegValue<= EVALUATE(lcTOSeg);
       ORDER BY  cSegValue ;
       INTO ARRAY laSource   

lnAryCount = 1

FOR lnCount = 1 TO ALEN(&lcExcAry,1)
  lnElement = ASCAN(laSource,&lcExcAry[lnAryCount])
  IF lnElement = 0
    =ADEL(&lcExcAry,lnAryCount)
    IF ALEN(&lcExcAry,1) > 1    
      DIMENSION &lcExcAry[ALEN(&lcExcAry,1)-1]
    ENDIF  
  ELSE
    lnAryCount = lnAryCount + 1
  ENDIF
ENDFOR

=gfMover(@laSource,@&lcExcAry,"Exclude segment values",.T.)

RELEASE laSource

*!**************************************************************************
*!
*!      Function: lfTerminate
*!
*!**************************************************************************
*
FUNCTION lfTerminate

IF ! llProcessing 
  IF gfModalGen("TRM02209B02019",'ALART') = 1
    RETURN .T.
  ENDIF
ENDIF

llProcessing = .T.
RETURN .F.