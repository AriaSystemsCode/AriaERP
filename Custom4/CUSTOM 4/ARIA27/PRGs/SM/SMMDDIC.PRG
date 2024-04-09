*:************************************************************************
*:
*: Procedure file: SMMDDIC.PRG 
*:                 (Modules dictionary)
*:
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: SYSTEM MANAGER
*:         Author: Reham Aly Alallamy
*:      Copyright (c) 
*:************************************************************************
* Last modified:  /  /
*E300683,4 AHMED 06/08/97 Add prgs directory path to the calling of programs
*:************************************************************************
EXTERNAL ARRAY laData,laDefProc,laScrMode
DECLARE  laFiles[1,1],laKeyField[1,4],laNewFiles[1,1],laSeqNo[1,3]

lsFiles    = 1
lnOldStart = 0
lnOldMax   = 0

rbDfnBy    = IIF(EMPTY(GETENV('DEVELOP')),3,2)
llBrowse   = .F.

IF !gfSetup()    
  RETURN
ENDIF  

*** This settings are executated once (if the window dosen't exist')
IF !WEXIST(lcBaseWind)
  laDefProc[7]    = .F.     && Don't use the default delete procedure
  laDefProc[9]    = .F.     && Don't use the default save procedure
  
  laKeyField[1,1] = 'laData[1]'
  laKeyField[1,2] =.T.
  laKeyField[1,3] = 'CAPP_ID'
  laKeyField[1,4] = 1

  STORE SPACE(1) TO laFiles,laNewFiles,laSeqNo

  SELECT sydAppl
  SCATTER  FIELDS &lcScFields MEMO TO laData BLANK
ENDIF

lcMaxStat = IIF(EMPTY(laData[8]),"DISABLE",;
                IIF(laScrMode[1] .OR. laScrMode[2],"DISABLE","ENABLE"))

*E300683,4 Call *.SPR from screens directory
* DO SMMdDic.SPR     && Run the screen program
DO (gcScrDir + gcWinAppl + '\SMMdDic.SPR')
*E300683,4 end  
*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow
EXTERNAL  ARRAY laScrMode,laFiles,laNewFiles,laData

DO CASE
  CASE laScrMode[1]      && Screen select mode 
    lnOldStart   = 0
    lnOldMax     = 0
    DIMENSION laFiles[1,1],laSeqNo[1,3]
    laSeqNo[1,1] = 0
    laSeqNo[1,2] = 0
    laSeqNo[1,3] = " "
    laFiles      = " "
    rbDfnBy      = IIF(EMPTY(GETENV('DEVELOP')),3,2)
    laData[5]    = IIF(EMPTY(GETENV('DEVELOP')),"U","A")
    lsFiles      = 1
  CASE laScrMode[2]     && View mode
    rbDfnBy = AT(laData[5],'SAU')
    lsFiles = 1
    DECLARE laFiles[1,1]
    STORE SPACE(1) TO laFiles
    rbDfnBy =IIF(laData[5] $ "SAU",ATC(laData[5],"SAU"),1)
    *** Get files used in this appliction to be displayed on screen
    SELECT RTRIM(cFile_Nam)+SPACE(10-LEN(RTRIM(cFile_nam)))+;
           IIF(EMPTY(cFile_ttl),RTRIM(cFile_nam)+;
           SPACE(34-LEN(RTRIM(cFile_nam))),RTRIM(cFile_ttl));
           FROM (gcSysHome+"SydFiles");
           WHERE laData[1] $ mFile_App;
           INTO ARRAY laFiles;
           ORDER BY cFile_Nam
    SELECT SydAppl
    *** Flaging open first files. ***
    lnRecNo= IIF(RECNO() >RECCOUNT(),0,RECNO())
    =lfFlgFiles(@laFiles)
    IF lnRecNo<>0
      GO lnRecNo
    ENDIF      
  CASE (laScrMode[3] OR laScrMode[4])  && edit or add
    IF laScrMode[3]
      rbDfnBy = AT(laData[5],'SAU')
    ELSE
      laData[5] = SUBSTR('SAU',rbDfnBy,1)
    ENDIF
    *** Get files used in this appliction to be displayed on screen
    DECLARE laFiles[1,1]
    laFiles = ' '
    SELECT RTRIM(cFile_Nam)+SPACE(10-LEN(RTRIM(cFile_nam)))+;
           IIF(EMPTY(cFile_ttl),RTRIM(cFile_nam)+;
           SPACE(34-LEN(RTRIM(cFile_nam))),RTRIM(cFile_ttl));
           FROM (gcSysHome+"SydFiles");
           WHERE laData[1] $ mFile_App;
           INTO ARRAY laFiles;
           ORDER BY cFile_Nam
    *** Flaging open first files         
    =lfFlgFiles(@laFiles)
    SHOW GET lsFiles
ENDCASE

IF laScrMode[1]
  SHOW GET pbZoom DISABLE
ELSE
  SHOW GET pbZoom ENABLE
ENDIF  

IF laScrMode[3] .OR. laScrMode[4]
  SELECT SydAppl
  *** Flaging open first files. ***
  lnRecNo= IIF(RECNO() >RECCOUNT(),0,RECNO())
  
  *** Collect all the dialogs started no. for all modules. ***
  DECLARE laSeqNo[1,3]
  laSeqNo[1,1] = 0
  laSeqNo[1,2] = 0
  laSeqNo[1,3] = " "
  SELECT ndlgstrtn,ndlgmaxno,capp_name ;
         FROM (gcSysHome+"sydAppl") ;
         WHERE sydAppl.cApp_Id <> laData[1] ;
         INTO ARRAY laSeqNo ;
         ORDER BY ndlgstrtn
    
  IF lnRecNo<>0
    GO lnRecNo
  ENDIF      
ENDIF

lcMaxStat = IIF(EMPTY(laData[8]),"DISABLE",;
                IIF(laScrMode[1] .OR. laScrMode[2],"DISABLE","ENABLE"))

laData[9] = IIF(EMPTY(laData[8]),0,laData[9])
SHOW GET laData[9] &lcMaxStat

*!**************************************************************************
*!
*!      Function lfvData_1
*!
*!**************************************************************************
*
FUNCTION lfvData_1

IF llBrowse .OR. (!EMPTY(laData[1]) .AND. LASTKEY() = 13)
  =gfSeekRec() 
ENDIF
llBrowse = .F.

*!**************************************************************************
*!
*!      Function lfvData_5
*!
*!**************************************************************************
*
FUNCTION lfvData_5

laData[5] = SUBSTR('SAU',rbDfnBy,1)

*!**************************************************************************
*!
*!      Function lfwData_8
*!
*!**************************************************************************
*
FUNCTION lfwData_8

lnOldStart = laData[8]

*!**************************************************************************
*!
*!      Function lfvData_8
*!
*!**************************************************************************
*
FUNCTION lfvData_8

IF !EMPTY(laData[8]) .AND. LASTKEY() = 13
  IF laData[8] < 0
    *** Dialog start no. should be greater than zero. ***
    *** <  Ok  > ***
    =gfModalGen("TRM00234B00000","DIALOG","Dialog start no.")
    laData[8] = lnOldStart
    _CUROBJ   = _CUROBJ
  ELSE
    IF laData[8] >= laSeqNo[1,1] .OR. ;
       laData[8] <= laSeqNo[ALEN(laSeqNo,1),1]
      FOR lnCount = 1 TO ALEN(laSeqNo,1)
        IF BETWEEN(laData[8],laSeqNo[lnCount,1],;
                   laSeqNo[lnCount,1]+laSeqNo[lnCount,2])
          *** Messages start no. are overlapped for modul ð. *** 
          *** <  Ok  > ***
          =gfModalGen("TRM00193B00000","DIALOG",ALLTRIM(laSeqNo[lnCount,3]))
          laData[8] = lnOldStart
          _CUROBJ   = _CUROBJ
          EXIT
        ENDIF
      ENDFOR
      laData[9] = IIF(laData[8] <> lnOldStart , 0 , laData[9])
    ENDIF
  ENDIF
ELSE
  IF LASTKEY() <> 13
    laData[8] = lnOldStart
  ENDIF
ENDIF

SHOW GET laData[8]

IF EMPTY(laData[8])
  laData[9] = 0
  SHOW GET laData[9] DISABLE
ELSE
  SHOW GET laData[9] ENABLE
ENDIF

*!**************************************************************************
*!
*!      Function lfwData_9
*!
*!**************************************************************************
*
FUNCTION lfwData_9

lnOldMax = laData[9]

*!**************************************************************************
*!
*!      Function lfvData_9
*!
*!**************************************************************************
*
FUNCTION lfvData_9

IF !EMPTY(laData[9]) .AND. LASTKEY() = 13
  IF laData[9] < 0
    *** Maximum no. of dialogs should be greater than zero. ***
    *** <  Ok  > ***
    =gfModalGen("TRM00234B00000","DIALOG","Maximum no. of dialogs")
    laData[9] = lnOldMax
    _CUROBJ   = _CUROBJ
  ELSE
    FOR lnCount = 1 TO ALEN(laSeqNo,1)
      IF BETWEEN(laSeqNo[lnCount,1],laData[8],laData[8]+laData[9])
        *** Messages start no. are overlapped for modul ð. *** 
        *** <  Ok  > ***
        =gfModalGen("TRM00193B00000","DIALOG",ALLTRIM(laSeqNo[lnCount,3]))
        laData[9] = lnOldMax
        _CUROBJ   = _CUROBJ
        EXIT
      ENDIF
    ENDFOR
  ENDIF
ELSE
  IF LASTKEY() <> 13
    laData[9] = lnOldMax
  ENDIF
ENDIF

SHOW GET laData[9]

*!**************************************************************************
*!
*!      Function lfvMntFile
*!
*!**************************************************************************
*
FUNCTION lfvMntFile

DO CASE
  CASE pbFiles = 1    && Add file to files list
    *** Create array holding the unassigned files to application
    DECLARE laNewFiles[1,1]
    IF EMPTY(laFiles [1,1])
      SELECT RTRIM(cFile_Nam)+SPACE(10-LEN(RTRIM(cFile_nam)))+;
             IIF(EMPTY(cFile_ttl),RTRIM(cFile_nam)+;
             SPACE(34-LEN(RTRIM(cFile_nam))),RTRIM(cFile_ttl));
             FROM  (gcSysHome+"SydFiles");
             WHERE !EMPTY(cFile_Nam) .AND. !EMPTY(cFile_ttl);
             INTO  ARRAY laNewFiles ;
             ORDER BY cFile_Nam
    ELSE
      SELECT RTRIM(cFile_Nam)+SPACE(10-LEN(RTRIM(cFile_nam)))+;
             IIF(EMPTY(cFile_ttl),RTRIM(cFile_nam)+;
             SPACE(34-LEN(RTRIM(cFile_nam))),RTRIM(cFile_ttl));
             FROM (gcSysHome+"SydFiles");
             WHERE ASCAN(laFiles," "+cFile_Nam) = 0 .AND. ;
                   ASCAN(laFiles,""+cFile_Nam) = 0 .AND. ;
                   !EMPTY(cFile_Nam) .AND. ;
                   !EMPTY(cFile_ttl) ;
             INTO ARRAY laNewFiles ;
             ORDER BY cFile_Nam
    ENDIF
    SELECT sydAppl
    
    *** If there is avaialible files so display them
    IF !EMPTY(laNewFiles [1,1])
      STORE 0 TO lsFl_List,pbSelect
      *** File list
      *E300683,4 Call programs from PRGS directory
      *DO SMFlLst.PRG WITH .F.
      DO (gcAppHome + gcWinAppl + '\SMFlLst ') WITH .F.
      *E300683,4 end
      
      *** If a file was selected
      IF lsFl_List > 0  && Insert file from list
        IF !EMPTY(laFiles[1,1])
          DIMENSION laFiles[ALEN(laFiles,1)+1,1]
        ENDIF
        laFiles[ALEN(laFiles,1),1] = " "+laNewFiles[lsFl_List,1]
        lsFiles = ALEN(laFiles,1)
      ENDIF
    ENDIF
    
  CASE pbFiles = 2    && Remove file from files list
    IF lsFiles <> 0 AND !EMPTY(laFiles[1,1])
      *** "Are you sure you want to remove it"  
      IF  gfModalGen('QRM00007B00007','Dialog'," ")  = 1
        IF ALEN(laFiles,1) <> 1
          =ADEL(laFiles,lsFiles)
          DIMENSION laFiles[ALEN(laFiles,1)-1,1]
        ELSE
          laFiles = ' '  
        ENDIF
        lsFiles = 1
      ENDIF
    ELSE
      *** "You must mark a list item first"
      = gfModalGen('TRM00008B00000','Dialog'," ")
      RETURN
    ENDIF
    
  CASE pbFiles = 3    && Toggele the openning files status
    IF lsFiles <> 0 AND !EMPTY(laFiles[1,1])
      laFiles[lsFiles,1] = IIF(LEFT(laFiles[lsFiles,1],1)=" ",;
                           STUFF(laFiles[lsFiles,1],1,1,""),;
                           STUFF(laFiles[lsFiles,1],1,1," "))
    ELSE
      *** "You must mark a list item first"
      =gfModalGen('TRM00008B00000','Dialog'," ")
      RETURN
    ENDIF
ENDCASE

SHOW GET lsFiles

*!**************************************************************************
*!
*!      Function lfFlgFiles
*!
*!**************************************************************************
*
FUNCTION lfFlgFiles

PARAMETER laFile

EXTERNAL ARRAY laFile

*** Get files used in this appliction to be flaged when displayed
DECLARE laMstrFl[1,1]
SELECT cFile_Nam FROM (gcSysHome+"SydFiles");
       WHERE laData[1]+'$' $ mFile_App;
       INTO ARRAY laMstrFl 
       
*** Flag open first files
IF EMPTY(laMstrFl[1,1])
  FOR lnCount = 1 TO ALEN(laFile,1)
    laFile[lnCount,1]=" "+laFile[lnCount,1]
  ENDFOR
ELSE  
  FOR lnCount = 1 TO ALEN(laFile,1)
    laFile[lnCount,1]=IIF(ASCAN(laMstrFl,SUBSTR(laFile[lnCount,1],1,8))<>0,;
                      ""+laFile[lnCount,1]," "+laFile[lnCount,1])
  ENDFOR
ENDIF

*!**************************************************************************
*!
*!      Procedure lpSavScr
*!
*!**************************************************************************
*
PROCEDURE lpSavScr
EXTERNAL ARRAY laData,laScrMode

IF EMPTY(laData[2])
  *** You cannot save this module with empty title. ***
  *** <   Ok   > ***
  = gfModalGen("TRM00125B00000","DIALOG","module")
  llCSave = .F.    && Flag to say that no save happen.
  _CUROBJ = OBJNUM(laData[2])
  RETURN
ENDIF

IF EMPTY(laData[8]) .OR. EMPTY(laData[9])
  *** You cannot leave one of the dialogs start no. or ***
  *** its maximum no. for this module empty.  You have ***
  *** to fill or blank both of them...
  *** <   Ok   > ***
  = gfModalGen("TRM00238B00000","DIALOG")
  llCSave = .F.    && Flag to say that no save happen.
  _CUROBJ = IIF(EMPTY(laData[8]),OBJNUM(laData[8]),OBJNUM(laData[9]))
  RETURN
ENDIF

SELECT SydAppl

*** Saving application data
IF laScrMode[4]
  APPEND BLANK
ENDIF
GATHER FROM laData FIELDS &lcScFields MEMO 
=gfAdd_Info()

*** Erasing old appliction data from the master file
SELECT SydFiles
SCAN
  IF OCCURS(laData[1]+'$',mFile_App) <> 0
    REPLACE mFile_App WITH STUFF(mFile_App,ATC(laData[1],mFile_App),4,'')
  ELSE
    IF OCCURS(laData[1],mFile_App) <> 0
      REPLACE mFile_App WITH STUFF(mFile_App,ATC(laData[1],mFile_App),3,'')
    ENDIF
  ENDIF
  IF RIGHT(mFile_App,1)=","
    REPLACE mFile_App WITH LEFT(mFile_App,LEN(mFile_App)-1)
  ENDIF
ENDSCAN

*** Add the new appliction data to the file database
IF !EMPTY(laFiles[1,1])
  SELECT sydFiles
  SET ORDER TO 1
  FOR lnCount =1 TO ALEN(laFiles,1)
    IF SEEK (SUBSTR(laFiles[lnCount,1],2,8))
      IF EMPTY(ALLTRIM(mFile_App))
        REPLACE mFile_App WITH;
                laData[1]+IIF(LEFT(laFiles[lnCount,1],1)<>" ","$","")
      ELSE
        REPLACE mFile_App WITH mFile_App+;
                ","+laData[1]+IIF(LEFT(laFiles[lnCount,1],1)<>" ","$","")
      ENDIF    
    ENDIF
  ENDFOR
ENDIF

SELECT SydAppl

*!**************************************************************************
*!
*!      Procedure lpDelScr
*!
*!**************************************************************************
*
PROCEDURE lpDelScr

EXTERNAL ARRAY laData,laScrMode

laScrMode    =.F.
laScrMode[1] =.T.
*** Delete the appliction from the appliction data base
SCATTER MEMVAR MEMO BLANK
GATHER  MEMVAR MEMO 
DELETE

SELECT SydFiles
SET ORDER TO TAG cFile_Nam

*** Delete the appliction data from the files data base
FOR lnCount = 1 TO ALEN(laFiles,1)
  laFiles[lnCount,1] = SUBSTR(laFiles[lnCount,1],2,8)
  IF SEEK(laFiles[lnCount,1])
    IF OCCURS(laData[1]+'$',mFile_App) <> 0
      REPLACE mFile_App WITH STUFF(mFile_App,ATC(laData[1]+'$',;
              mFile_App),4,'')
    ELSE
      IF OCCURS(laData[1],mFile_App) <> 0
        REPLACE mFile_App WITH STUFF(mFile_App,ATC(laData[1],;
                mFile_App),3,'')
      ENDIF
    ENDIF
    IF RIGHT(mFile_App,1) = ','
      REPLACE mFile_App WITH LEFT(mFile_App, LEN(mFile_App) - 1)
    ENDIF
  ENDIF
ENDFOR

SELECT SydAppl
