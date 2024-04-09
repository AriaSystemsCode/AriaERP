*:************************************************************************
*:
*: Procedure file: SMDIALG.PRG 
*:                 (Dialogs)
*:
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: SYSTEM MANAGER
*:         Author: Reham Alallamy
*:      Copyright (c) 
*:************************************************************************
*: Last modified :
*E300683,4 AHMED 06/08/97 Add prgs directory path to the calling of programs
*E301077,78 Hesham 02/25/1999
*E301077,78 Controlling opening and clossing files.
*E301077,78 to make the program open the files when ever it need it
*E301312,1 Hesham 08/26/99
*E301312,1 Change the screen to be able to add customer messages and buttons

*:************************************************************************
EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc
DIMENSION laKeyField[3,4],laApp[1],laSeqNo[1,4]

laKeyField[1,1] = "laData[1]"
laKeyField[1,2] =.F.
laKeyField[1,3] = 'CAPPID'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[2]'
laKeyField[2,2] =.F.
laKeyField[2,3] = 'CAPPID'
laKeyField[2,4] = 2
laKeyField[3,1] = 'laData[3]'
laKeyField[3,2] =.T.
laKeyField[3,3] = 'CAPPID'
laKeyField[3,4] = 3

STORE SPACE(0) TO lctMessage,lctButton,lctWith,lctShow,laApp
lcApp_Id     = " "
lcApp_Desc   = " "
lcTmpObj1    = " "
lcTmpObj2    = " "
lcTmpObj3    = " "
lcTryStat    = " "
llBrowse     = .F.

laDefProc[9] = .F.     && Don't use the default save procedure

IF !gfSetup()    
  RETURN
ENDIF  

IF !WEXIST(lcBaseWind)
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  *E301077,78 Hesham (Start)
   llApplUse = gfSysOpen(gcSysHome+'sydappl')
  *E301077,78 Hesham (End)
  SELECT SydAppl
  *** Collect all the dialogs started no. for all modules. ***
  DECLARE laSeqNo[1,4]
  laSeqNo[1,1] = " "
  laSeqNo[1,2] = " "
  laSeqNo[1,3] = 0
  laSeqNo[1,4] = 0
  SELECT capp_id,capp_name,ndlgstrtn,ndlgmaxno ;
         FROM (gcSysHome+"sydAppl") ;
         WHERE cApp_Id <> "SY"   .AND. ;
               !EMPTY(ndlgstrtn) .AND. ;
               !EMPTY(ndlgmaxno) ;
         INTO ARRAY laSeqNo ;
         ORDER BY ndlgstrtn

  *E301312,1 Hesham (Start)
  *E301312,1 Change the screen to be able to add customer messages
  DECLARE laSeqNo[ALEN(laSeqNo,1)+1,ALEN(laSeqNo,2)] 
  =AINS('laSeqNo',1)
  laSeqNo[1,1] = "  "
  laSeqNo[1,2] = "Custom  "      
  laSeqNo[1,3] = 0
  laSeqNo[1,4] = 9999
  *E301312,1 Hesham (Start)


  SELECT cApp_Id+"  "+cApp_Name ;
         FROM (gcSysHome+'sydAppl') ;
         WHERE cApp_Id <> "SY" ;
         INTO ARRAY laApp ;
         ORDER BY cApp_Name

  *E301077,78 Hesham (Start)
   USE IN IIF(llApplUse,'sydappl',0)
  *E301077,78 Hesham (End)

  IF EMPTY(laApp[1,1])
    STORE " " TO lcApp_Id,lcApp_Desc,laApp
  ELSE
  *E301312,1 Hesham (Start)
  *E301312,1 Change the screen to be able to add customer messages
  DECLARE laApp[ALEN(laApp,1)+1,ALEN(laApp,2)] 
  =AINS('laApp',1)
  laApp[1,1] = "    Custom"
  *E301312,1 Hesham (Start)
    IF ASCAN(laApp,"SM") > 0
      lcApp_Id   = SUBSTR(laApp[ASCAN(laApp,"SM"),1],1,2)
      DO CASE
        CASE _DOS
          lcApp_Desc = laApp[ASCAN(laApp,"SM"),1]
        CASE _WINDOWS
          ibPopApp   = ASCAN(laApp,"SM")
      ENDCASE
      laData[1] = lcApp_Id
      laData[2] = 'B'
      laData[5] = 'S'
    ELSE
      lcApp_Id   = SUBSTR(laApp[1,1],1,2)
      DO CASE
        CASE _DOS
          lcApp_Desc = laApp[1,1]
        CASE _WINDOWS
          ibPopApp   = 1
      ENDCASE
      laData[1] = lcApp_Id
      laData[2] = 'B'
      laData[5] = 'A'
    ENDIF
  ENDIF
ELSE
  rbObjTyp  = AT(laData[2],"BM")
  DO CASE
    CASE laData[6] = "TR"
      rbMsgTyp = 1
    CASE laData[6] = "QR"
      rbMsgTyp = 2
    CASE laData[6] = "IN"
      rbMsgTyp = 3
  ENDCASE
ENDIF


lcMsg_Stat = IIF(laScrMode[3] .OR. laScrMode[4],;
             IIF(laData[2] = "B","DISABLE","ENABLE"),"DISABLE")
lcTryStat  = IIF(laScrMode[1] .OR. laScrMode[4],"DISABLE","ENABLE")

SELECT SYDDLOBJ

*E300683,4 Call *.SPR from screens directory
* DO SMDIALG.SPR 
DO (gcScrDir + gcWinAppl + '\SMDIALG.SPR')
*E300683,4 end  
*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow
EXTERNAL ARRAY laScrMode

DO CASE
  CASE laScrMode[1]
    rbObjTyp  = 1
    laData[2] = "B"
    
    rbMsgTyp  = 2
    laData[6] = "QR"
    
    laData[1] = lcApp_Id
    laData[5] = IIF(lcApp_Id = "SM","S","A")

  IF !EMPTY(lcApp_Id)
    laData[5] = IIF(laData[1] = "SM","S","A")
  ELSE
    laData[5] = 'C'
  ENDIF  
    
    SHOW GET pbTry DISABLE
    
  CASE laScrMode[2] .OR. laScrMode[3]
    SHOW GET pbTry ENABLE
    
    rbObjTyp  = AT(laData[2],"BM")
    lcApp_ID  = laData[1]
    IF ASCAN(laApp,laData[1]) > 0
      DO CASE
        CASE _DOS
          lcApp_Desc = laApp[ASCAN(laApp,laData[1]),1]
        CASE _WINDOWS
          ibPopApp   = ASCAN(laApp,laData[1])
      ENDCASE
      laData[1] = lcApp_Id
    ENDIF
    DO CASE
      CASE laData[6] = "TR"
        rbMsgTyp = 1
      CASE laData[6] = "QR"
        rbMsgTyp = 2
      CASE laData[6] = "IN"
        rbMsgTyp = 3
      OTHERWISE
        rbMsgTyp = 0
    ENDCASE
  CASE laScrMode[4]
    SHOW GET pbTry DISABLE
ENDCASE  

IF laScrMode[3] .OR. laScrMode[4]
  IF laData[2] = "B"
    rbMsgTyp = 0
    SHOW GET rbMsgTyp DISABLE
  ELSE
    SHOW GET rbMsgTyp ENABLE
  ENDIF
ELSE
  SHOW GET rbMsgTyp DISABLE
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_2
*!
*!**************************************************************************
*
FUNCTION lfvData_2

laData[2] = SUBSTR("BM",rbObjTyp,1)

IF laScrMode[3] .OR. laScrMode[4]
  IF laData[2] = "B"
    SHOW GET rbMsgTyp DISABLE
  ELSE
    SHOW GET rbMsgTyp ENABLE
  ENDIF
ELSE
  SHOW GET rbMsgTyp DISABLE
ENDIF

=lfRefresh() 

*!**************************************************************************
*!
*!      Function: lfvNew
*!
*!**************************************************************************
*
FUNCTION lfvNew

SELECT SYDDLOBJ
SET ORDER TO TAG CAPPID DESCENDING
IF SEEK(laData[1]+laData[2])
 *E301312,1 Hesham (Start)
 *E301312,1 Change the screen to be able to add customer messages
  *lnValue = VAL(syddlobj.cDlobjid) + 1  
  IF !EMPTY(laData[1])
    lnValue = VAL(syddlobj.cDlobjid) + 1
  ELSE
    lnValue = VAL(substr(syddlobj.cDlobjid,2)) + 1  
  ENDIF  
 *E301312,1 Hesham (eND)  
  lcExact = SET("EXACT")
  SET EXACT ON
  IF ASCAN(laSeqNo,laData[1]) > 0
    IF ASUBSCRIPT('laSeqNo',ASCAN(laSeqNo,laData[1]),1) > 0
      lnAryElm = ASUBSCRIPT('laSeqNo',ASCAN(laSeqNo,laData[1]),1)
      IF !BETWEEN(lnValue,laSeqNo[lnAryElm,3],;
                 laSeqNo[lnAryElm,3]+laSeqNo[lnAryElm,4])
        *** Message numbers for modul ð should be between ð and ð. ***
        *** <  Ok  > ***
        lcTmpStr     = ALLTRIM(laSeqNo[lnAryElm,2]) + "|" +;
                       ALLTRIM(STR(laSeqNo[lnAryElm,3])) + "|"+;
                       ALLTRIM(STR(laSeqNo[lnAryElm,3]+laSeqNo[lnAryElm,4]))
        =gfModalGen("INM00194B00000","DIALOG",lcTmpStr)
        laData[3]    = SPACE(3)
        _CUROBJ      = _CUROBJ
      ELSE
       *E301312,1 Hesham (Start)
       *E301312,1 Change the screen to be able to add customer messages
       *laData[3]    = RIGHT('0000'+ALLTRIM(STR(lnValue)),5)            
       IF !EMPTY(laData[1])
         laData[3]    = RIGHT('0000'+ALLTRIM(STR(lnValue)),5)                   
       ELSE
         laData[3]    = 'C'+PADL(ALLT(STR(lnvalue)),4,'0')
       ENDIF
       *E301312,1 Hesham (eND)      
        laScrMode    = .F.
        laScrMode[4] = .T.
        SHOW GETS
      ENDIF
    ENDIF
  ENDIF
ELSE
  lcExact = SET("EXACT")
  SET EXACT ON
  IF ASCAN(laSeqNo,laData[1]) > 0
    IF ASUBSCRIPT('laSeqNo',ASCAN(laSeqNo,laData[1]),1) > 0
        lnAryElm     = ASUBSCRIPT('laSeqNo',ASCAN(laSeqNo,laData[1]),1)    
        *E301312,1 Hesham (Start)
        *E301312,1 Change the screen to be able to add customer messages
        *lnValue      = laSeqNo[lnAryElm,3]        
        *laData[3]    = RIGHT('0000'+ALLTRIM(STR(lnValue)),5)                
        lnValue      = laSeqNo[lnAryElm,3]                
        IF EMPTY(laData[1])
          laData[3] = 'C0000'
        ELSE
 
          laData[3]    = RIGHT('0000'+ALLTRIM(STR(lnValue)),5)
        ENDIF  
        *E301312,1 Hesham (eND)        
          laScrMode    = .F.
      laScrMode[4] = .T.
      SHOW GETS
    ENDIF
  ENDIF
ENDIF

SET EXACT &lcExact

SET ORDER TO TAG CAPPID ASCENDING

*!**************************************************************************
*!
*!      Function: lfvData_3
*!
*!**************************************************************************
*
FUNCTION lfvData_3

IF llBrowse .OR. (!EMPTY(laData[3]) AND LASTKEY()=13)
  IF LEFT(laData[3],1) = "?" .OR. llBrowse
    lcTmpObj1 = laData[1]
    lcTmpObj2 = laData[2]
    lcTmpObj3 = laData[3]
    laData[1] = "?"
    laData[2] = " "
    laData[3] = " "
    =gfSeekRec()
    IF EMPTY(laData[2]) .AND. EMPTY(laData[3])
      laData[1] = lcTmpObj1
      laData[2] = lcTmpObj2
      laData[3] = " "
      _CUROBJ   = OBJNUM(laData[3])
    ENDIF
  ELSE
    laData[3] = RIGHT('0000'+ALLTRIM(laData[3]),5)
    =gfSeekRec()
    IF laScrMode[4]
      =lfvNew()
    ENDIF
  ENDIF
ENDIF  
llBrowse = .F.

*!**************************************************************************
*!
*!      Function: lfvData_6
*!
*!**************************************************************************
*
FUNCTION lfvData_6

DO CASE
  CASE rbMsgTyp = 1
    laData[6] = "TR"
  CASE rbMsgTyp = 2
    laData[6] = "QR"
  CASE rbMsgTyp = 3
    laData[6] = "IN"
ENDCASE

*!**************************************************************************
*!
*!      Function: lfvTry
*!
*!**************************************************************************
*
FUNCTION lfvTry

*E300683,4 Call *.SPR from screens directory
* DO SMTRYDG.SPR 
DO (gcScrDir + gcWinAppl + '\SMTRYDG.SPR')
*E300683,4 end   
*!**************************************************************************
*!
*!      Procedure: lpShowDlg
*!
*!**************************************************************************
*
PROCEDURE lpShowDlg

lcDvar = laData[3]

*!**************************************************************************
*!
*!      Procedure: lfvShow
*!
*!**************************************************************************
*
FUNCTION lfvShow

lcMtoDsp = "INM"+IIF(rbObjtyp=1,lcDvar,laData[3])+;
           "B"+IIF(rbObjtyp=1,laData[3],lcDvar)
=gfModalGen(lcMtoDsp)

*!**************************************************************************
*!
*!      Procedure: lfvAppId
*!
*!**************************************************************************
*
FUNCTION lfvAppId

*** Save the old application. ***
lcOldApp = lcApp_Id

DO CASE
  CASE _DOS
    lcApp_Id = SUBSTR(gfActPop(2,15,8,64,'laApp',1,1,@lcApp_Desc),1,2)
    =lfRefresh()
  CASE _WINDOWS
    lcApp_Id = SUBSTR(laApp[ibPopApp,1],1,2)
    SHOW GET ibPopApp
ENDCASE

*E301312,1 Hesham (Start)
*E301312,1 Change the screen to be able to add customer messages
IF ASCAN(laSeqNo,lcApp_Id) = 0
*IF !EMPTY(lcApp_Id) and ASCAN(laSeqNo,lcApp_Id) = 0
*E301312,1 Hesham (End)
  *** This modul has no message start no. or max. no. of messages. ***
  *** You have to enter this values before entering any messages. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00195B00000","DIALOG")
  lcApp_Id = lcOldApp
  DO CASE
    CASE _DOS
      lcApp_Desc = laApp[ASCAN(laApp,lcApp_Id),1]
      =lfRefresh()
    CASE _WINDOWS
      ibPopApp   = ASCAN(laApp,lcApp_Id)
      SHOW GET ibPopApp
  ENDCASE
ELSE
  laData[1] = lcApp_Id
  *E301312,1 Hesham (Start)
  *E301312,1 Change the screen to be able to add customer messages
  *laData[5] = IIF(laData[1] = "SM","S","A")  
  IF !EMPTY(lcApp_Id)
    laData[5] = IIF(laData[1] = "SM","S","A")
  ELSE
    laData[5] = 'C'
  ENDIF  
  *E301312,1 Hesham (End)  
ENDIF

*!**************************************************************************
*!
*!      Procedure lpSavScr
*!
*!**************************************************************************
*
PROCEDURE lpSavScr

EXTERNAL ARRAY laData,laScrMode

SELECT SYDDLOBJ

IF laScrMode[4]
  SET ORDER TO TAG CAPPID DESCENDING
  
  IF SEEK(laData[1]+laData[2])
   *E301312,1 Hesham (Start)
   *E301312,1 Change the screen to be able to add customer messages
   *lnValue = VAL(syddlobj.cDlobjid) + 1  
   IF !EMPTY(laData[1])
     lnValue = VAL(syddlobj.cDlobjid) + 1
   ELSE
     lnValue = VAL(substr(syddlobj.cDlobjid,2)) + 1  
   ENDIF  
   *E301312,1 Hesham (eND)  

    lcExact = SET("EXACT")
    SET EXACT ON
    IF ASCAN(laSeqNo,laData[1]) > 0
      IF ASUBSCRIPT('laSeqNo',ASCAN(laSeqNo,laData[1]),1) > 0
        lnAryElm = ASUBSCRIPT('laSeqNo',ASCAN(laSeqNo,laData[1]),1)
        IF !BETWEEN(lnValue,laSeqNo[lnAryElm,3],;
                   laSeqNo[lnAryElm,3]+laSeqNo[lnAryElm,4])
          *** Message numbers for modul ð should be between ð and ð. ***
          *** <  Ok  > ***
          lcTmpStr     = ALLTRIM(laSeqNo[lnAryElm,2]) + "|" +;
                         ALLTRIM(STR(laSeqNo[lnAryElm,3])) + "|"+;
                         ALLTRIM(STR(laSeqNo[lnAryElm,3]+laSeqNo[lnAryElm,4]))
          =gfModalGen("INM00194B00000","DIALOG",lcTmpStr)
          llCSave = .F.
          RETURN
        ELSE
         *E301312,1 Hesham (Start)
         *E301312,1 Change the screen to be able to add customer messages
         *laData[3]    = RIGHT('0000'+ALLTRIM(STR(lnValue)),5)            
         IF !EMPTY(laData[1])
           laData[3]    = RIGHT('0000'+ALLTRIM(STR(lnValue)),5)                   
         ELSE
           laData[3]    = 'C'+PADL(ALLT(STR(lnvalue)),4,'0')
         ENDIF
         *E301312,1 Hesham (eND)      
        ENDIF
      ENDIF
    ENDIF
  ELSE
    lcExact = SET("EXACT")
    SET EXACT ON
    IF ASCAN(laSeqNo,laData[1]) > 0
      IF ASUBSCRIPT('laSeqNo',ASCAN(laSeqNo,laData[1]),1) > 0
        lnAryElm     = ASUBSCRIPT('laSeqNo',ASCAN(laSeqNo,laData[1]),1)    
        *E301312,1 Hesham (Start)
        *E301312,1 Change the screen to be able to add customer messages
        *lnValue      = laSeqNo[lnAryElm,3]        
        *laData[3]    = RIGHT('0000'+ALLTRIM(STR(lnValue)),5)                
        lnValue      = laSeqNo[lnAryElm,3]                
        IF EMPTY(laData[1])
          laData[3] = 'C0000'
        ELSE
          laData[3]    = RIGHT('0000'+ALLTRIM(STR(lnValue)),5)
        ENDIF  
        *E301312,1 Hesham (eND)        
      ENDIF
    ENDIF
  ENDIF

  SET EXACT &lcExact
ENDIF

SET ORDER TO TAG CAPPID ASCENDING

IF laScrMode[4]
  APPEND BLANK
ENDIF
GATHER FROM laData FIELDS &lcScFields MEMO
=gfAdd_Info()

IF laScrMode[4]
  lcTmpStr = IIF(laData[2] = "M","message"+"|"+laData[3],;
                                 "button"+"|"+laData[3])
  =gfWait("00207","NOWAIT",lcTmpStr)
ENDIF
