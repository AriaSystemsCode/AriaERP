*:************************************************************************
*:
*: Procedure file: SMOBDIC.PRG 
*:                 (Module objects dictionary)
*:
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: SYSTEM MANAGER
*:         Author: Reham Aly Alallamy
*:      Copyright (c) 
*:************************************************************************
* Last modified:  /  /
*B601694,1 Reham On 04/06/97
*B601694,1 Fix the wrong saving of the base file under windows.
*E300683,4 AHMED 06/08/97 Add prgs directory path to the calling of programs
*E300693,1 Hesham El_Sheltawi 06/22/97
*E300693,1 Add new screen to the screen set to maintain the program report
*E300693,1 and it's filter
*E300704,1 Hesham El-Sheltawi 09/07/97
*E300704,1 ignore the validation of the base file of the program
*E300704,1 so we can use the control panel with programs without
*E300704,1 base file
*B601829,1 Hesham 07/31/97
*B601829,1 change the first column of the array that hold the files
*B601829,1 information to use the file name insted of the file title
*E038033,1 HMA 04/03/2004 Select only records belongs to A27(to split 
*E038033,1                A27 records of SYDREPRT File from A4XP records).
*:************************************************************************

EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc
*E300693,1 Hesham El_Sheltawi (Start)
*DECLARE laKeyField[2,4],laWndObj[4,3],;
        laFilLogFs[1,2],laFil_Indx[1,2],;
        laFilLog[1,1],laFilUsed[1,1],;
        laArrTmp1[1,1],laMainPrgs[1,2],;
        laArrTmp2[1],laApp[1,1]

DECLARE laKeyField[2,4],laWndObj[4,3],;
        laFilLogFs[1,2],laFil_Indx[1,2],;
        laFilLog[1,1],laFilUsed[1,1],;
        laArrTmp1[1,1],laMainPrgs[1,2],;
        laArrTmp2[1],laApp[1,1],laOGFxFlt[1,7],laOGVRFlt[1,7],LAOGFIELDN[1],LAOGFIELDH[1];
        ,laRepFltr[1,2],laRepVars[1,2]
*E300693,1 Hesham El_Sheltawi (End)
laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.F.
laKeyField[1,3] = 'CAPP_ID'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[2]'
laKeyField[2,2] =.T.
laKeyField[2,3] = 'CAPP_ID'
laKeyField[2,4] = 2

lcExprsion = " " 
lctMainPrg = "Program files and base file" && Hold the title of the child screen.
lctMainRp  = "Program Report and Filter" 
lcMain_Fil = " "                         && Hold the base file to display on the screen.
lcCurrTag  = " "
lcCurrFile = " "
lcLst_Fil  = " "
lcOldPrg   = " "
lcOld_10   = " "
lcMainStat = " "
puTag      = " "
puFilTag   = " "
lcTmpVlu   = " "

lcApp_Desc = " "
ibPopApp   = 1
llBrowse   = .F.
STORE " " TO laFilLogFs,laFil_Indx,laFilLog,laFilUsed,laArrTmp1,laMainPrgs,laOGFxFlt,laOGVRFlt,;
             LAOGFIELDH,LAOGFIELDN,lcOldRep,lcRepName,lcVarFltr,lcFixFltr,laRepFltr,laRepVars

rbPOption  = 1
rbObj_Typ  = 1
rbDfnBy    = IIF(EMPTY(GETENV('DEVELOP')),3,2)
rbModal    = 2

puSorcFil  = 1

lsSorcFil  = 1
lsTrgtFil  = 1

laDefProc[9]  = .F.

IF !gfSetup()
  RETURN
ENDIF  

laWndObj[1,1]  = gcBaseWind
laWndObj[1,2]  = 'IBPOPAPP'
laWndObj[1,3]  = 'PBMAINFIL'

laWndObj[2,1]  = 'GWCCONTRL1'
laWndObj[2,2]  = 'PBTOP'
laWndObj[2,3]  = 'PBCLS'

laWndObj[3,1]  = 'CWDSMPRGFL'
laWndObj[3,2]  = 'LSSORCFIL'
laWndObj[3,3]  = 'PBPGCLOSE'

laWndObj[4,1]  = 'CWDSMPRGRP'
laWndObj[4,2]  = 'PBRPCLOSE'
laWndObj[4,3]  = 'PBRPCLOSE'


IF !WEXIST(lcBaseWind)
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  
  SELECT cApp_Id+"  "+cApp_Name ;
         FROM (gcSysHome+'sydAppl') ;
         INTO ARRAY laApp ;
         ORDER BY cApp_Name

  IF EMPTY(laApp[1,1])
    *** No modules available. ***
    *** <  Ok  > ***
    =gfModalGen("TRM00189B00000","DIALOG","modules")
    glQuitting = .T.
    RETURN
    STORE " " TO lcApp_Id,lcApp_Desc,laApp,laData[1]
    ibPopApp = 1
  ELSE
    laData[1] = SUBSTR(laApp[1,1],1,2)
    DO CASE
      CASE _DOS
        lcApp_Desc = laApp[1,1]
      CASE _WINDOWS
        ibPopApp   = 1
    ENDCASE
  ENDIF

  *** Get all the files available in the system. ***
  SELECT DISTINCT cFile_nam,;
         IIF(EMPTY(cFile_ttl),;
         cFile_nam+SPACE(34-LEN(cFile_nam)),cFile_ttl);
         FROM (gcSysHome+"SYDFILES");
         ORDER BY cFile_ttl;
         INTO ARRAY laFilLogFs
  
  IF EMPTY(laFilLogFs[1,1])
    laFilLogFs = " "
  ELSE
    FOR lnCount = 1 TO ALEN(laFilLogFs,1)
      laFilLogFs[lnCount,1] = ALLTRIM(laFilLogFs[lnCount,1])
      laFilLogFs[lnCount,2] = ALLTRIM(laFilLogFs[lnCount,2])
    ENDFOR
  ENDIF
  
  DECLARE laFilLog[ALEN(laFilLogFs,1),2]
  =ACOPY(laFilLogFs,laFilLog)
  =gfADel(@laFilLog,1,2)

  DECLARE laFilUsed[1,1]
  laFilUsed = " "
  DEFINE POPUP puSorcFil  MARGIN RELATIVE SCROLL MARK CHR(16)
  FOR lnCount = 1 TO ALEN('laFilLog',1)
    DEFINE BAR lnCount OF puSorcFil PROMPT (ALLTRIM(laFilLog[lnCount,1]))
    IF ASCAN('laFilUsed',ALLTRIM(laFilLog[lnCount,1])) > 0
      SET SKIP OF BAR lnCount OF puSorcFil .T.
    ENDIF
  ENDFOR

  *** Get all the main programs available in the system. ***
  SELECT DISTINCT cApobjNam,IIF(!EMPTY(cPrglname),;
         PROPER(cPrglname),cApobjNam+SPACE(40-LEN(cApobjNam))) ;
         FROM  (gcSysHome+"sydObjct");
         WHERE cApobjNam <> laData[2];
         INTO ARRAY laMainPrgs;
         ORDER BY cPrglname

  DECLARE laSource[ALEN(laMainPrgs,1),2]
  =ACOPY(laMainPrgs,laSource)
  =gfADel(@laSource,1,2)

  IF _WINDOWS
    puTag = " "
    DEFINE POPUP puFilTag prompt field SYDINDEX.CFILE_TAG scroll;
    FROM 21.42,10.25 TO 26,22.63;
    MESSAGE gfObj_msg()
    ON SELECTION POPUP puFilTag DO lfvFil_Tag
  ENDIF
ENDIF

lcMainStat = IIF(laScrMode[1],"DISABLE",;
             IIF(laData[3] = "G","DISABLE","ENABLE"))

lcAcsStat  = IIF(laScrMode[1],"DISABLE",;
                 IIF(!laData[10] .AND. laData[3] = "P",;
                 "ENABLE","DISABLE"))

SELECT SYDINDEX
SET FILTER TO
*B601829,1 Hesham (Start)
*B601829,1 make the filter on the file name complete 8 char.
*B601829,1 so if the file name len less that 8 it get the right inf.
*SET FILTER TO ALLTRIM(cFile_nam) = ALLTRIM(lcLst_Fil)
SET FILTER TO cFile_nam = lcLst_Fil
*B601829,1 Hesham (Start)
LOCATE

SELECT SYDOBJCT

*E300683,4 Call *.SPR from screens directory
* DO SMObdic.SPR 
DO (gcScrDir + gcWinAppl + '\SMObdic.SPR')
*E300683,4 end  
SELECT SYDINDEX
SET FILTER TO

IF glQuitting
  RELEASE POPUP puFilTag
  IF WEXIST('CWDSMPRGFL')
    RELEASE WINDOW CWDSMPRGFL
  ENDIF
ENDIF
 
*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow
EXTERNAL ARRAY laData,laScrMode
PRIVATE lnSelAlias  && Define a variable to save the selected file in the work Aria
DO CASE
  CASE laScrMode[1]
    DO CASE
      CASE _DOS
        laData[1] = SUBSTR(lcApp_Desc,1,2)
      CASE _WINDOWS
        laData[1] = SUBSTR(laApp[ibPopApp,1],1,2)
    ENDCASE
    
    lcLst_Fil  = " "
    lsSorcFil  = 1
    lsTrgtFil  = 1
    rbObj_Typ  = 1
    rbPoption  = 1
    rbDfnBy    = IIF(EMPTY(GETENV('DEVELOP')),3,2)
    rbModal    = 2
    laData[3]  = 'P'
    laData[6]  = IIF(EMPTY(GETENV('DEVELOP')),'U','A')
    laData[8]  = .F.
    
    DECLARE laFil_Indx[1,2],laOGFxFlt[1,7],laOGVRFlt[1,7],LAOGFIELDN[1],LAOGFIELDH[1];
            ,laRepFltr[1,2],laRepVars[1,2]
    STORE '' TO laOGFxFlt,laOGVRFlt,LAOGFIELDN,LAOGFIELDH,laFil_Indx,lcVarFltr,lcFixFltr,;
                laRepFltr,laRepVars
    lcCurrTag  = " "
    
    DECLARE laFilUsed[1,1]
    laFilUsed  = " "
    lcCurrFile = " "

    lcExprsion = " "
    SHOW GET lsRepVar
    FOR lnCount = 1 TO ALEN('laFilLog',1)
      DEFINE BAR lnCount OF puSorcFil PROMPT (ALLTRIM(laFilLog[lnCount,1]))
      IF ASCAN('laFilUsed',ALLTRIM(laFilLog[lnCount,1])) > 0
        SET SKIP OF BAR lnCount OF puSorcFil .T.
      ENDIF
    ENDFOR
    
    lcMain_Fil = " "

  CASE laScrMode[2]
    *** Get all the main programs available in the system. ***
    DECLARE laMainPrgs[1,2],laSource[1,2]
    STORE " " TO laMainPrgs,laSource
    SELECT DISTINCT cApobjNam,IIF(!EMPTY(cPrglname),;
           PROPER(cPrglname),cApobjNam+SPACE(40-LEN(cApobjNam))) ;
           FROM  (gcSysHome+"sydObjct");
           WHERE cApobjNam <> laData[2] .AND. ;
                 cApp_Id   =  laData[1] ;
           INTO ARRAY laMainPrgs;
           ORDER BY cPrglname

    DECLARE laOGFxFlt[1,7],laOGVRFlt[1,7],LAOGFIELDN[1],LAOGFIELDH[1],laRepFltr[1,2],laRepVars[1,2]
    STORE '' TO laOGFxFlt,laOGVRFlt,LAOGFIELDN,LAOGFIELDH,lcVarFltr,lcFixFltr,laRepFltr,laRepVars
    DECLARE laSource[ALEN(laMainPrgs,1),2]
    

    *E038033,1 HMA 04/03/2004 Select only Records belong to ARIA27 [BEGIN]
    *IF SEEK(laData[19],'SYDREPRT')
    *  IF !EMPTY(SYDREPRT.MREPAVFLD) AND SYDREPRT.Crep_ID = laData[19] 
    *    RESTORE FROM MEMO SYDREPRT.MREPAVFLD ADDI    
    *  ENDIF  
    =SEEK(laData[19],'SYDREPRT')
    lnSelAlias=SELECT()
    SELECT SYDREPRT
    LOCATE WHILE SYDREPRT.Crep_ID = laData[19] FOR SYDREPRT.cVer<>"A40" 
    SELECT(lnSelAlias)
    IF FOUND('SYDREPRT')
      IF !EMPTY(SYDREPRT.MREPAVFLD) 
	    RESTORE FROM MEMO SYDREPRT.MREPAVFLD ADDI    
      ENDIF  
    ENDIF  
    *E038033,1 HMA [END]
        
    IF !EMPTY(sydObjct.MREPFXFLT)
      RESTORE FROM MEMO sydObjct.MREPFXFLT ADDI
    ENDIF
    IF !EMPTY(sydObjct.MREPVRFLT)  
      RESTORE FROM MEMO sydObjct.MREPVRFLT ADDI  
    ENDIF  
    IF !EMPTY(sydObjct.mrepfltr)
       RESTORE FROM MEMO sydObjct.mrepfltr ADDI
    ENDIF
    lcVarFltr = gfGenFlt('laOGVrFlt',.F.)
    lcFixFltr = gfGenFlt('laOGFxFlt',.F.)
    =lfInitRepVr()    
    SHOW GET lsRepVar      
    
    =ACOPY(laMainPrgs,laSource)
    =gfADel(@laSource,1,2)

    lsSorcFil  = 1
    lsTrgtFil  = 1
    
    rbModal   = IIF(SUBSTR(laData[12],3,1)="D",1,2)
    rbObj_Typ = AT(laData[3],'PCG')
    rbDfnBy   = AT(laData[6],'SAU')
    rbPoption = IIF(laData[8],2,1)
    
    *** Get the files names used by this program ***
    *** in the target array in the list...
    lcSavExct = SET("EXACT")
    SET EXACT ON

    DECLARE laFilUsed[1,1],laFil_Indx[1,2]
    IF !EMPTY(laData[14])
      DECLARE laArrTmp1[1,2]
      laArrTmp1 = " "
      =gfSubStr(laData[14],@laArrTmp1,",|")
      DECLARE laFilUsed[ALEN(laArrTmp1,1),1],laFil_Indx[ALEN(laArrTmp1,1),2]
      STORE " " TO laFilUsed,laFil_Indx
      FOR lnCount = 1 TO ALEN(laArrTmp1,1)
        IF ASCAN(laFilLogFs,PADR(laArrTmp1[lnCount,1],8)) > 0
       
          laFilUsed[lnCount,1]  = laFilLogFs[ASUBSCRIPT(laFilLogFs,;
                                  ASCAN(laFilLogFs,PADR(laArrTmp1[lnCount,1],8)),1),2]

        *B601829,1 Hesham (Start)
        *B601829,1 change the first column of the array that hold the files
        *B601829,1 information to use the file name insted of the file title
*          laFil_Indx[lnCount,1] = laFilLogFs[ASUBSCRIPT(laFilLogFs,;
                                  ASCAN(laFilLogFs,laArrTmp1[lnCount,1]),1),2]
          laFil_Indx[lnCount,1] = PADR(laArrTmp1[lnCount,1],8)
        *B601829,1 Hesham (End)                                  
          laFil_Indx[lnCount,2] = laArrTmp1[lnCount,2]
        ELSE
          laFilUsed[lnCount,1]  = " "
          laFil_Indx[lnCount,1] = " "
          laFil_Indx[lnCount,2] = " "
        ENDIF
      ENDFOR
    ELSE
      DECLARE laFilUsed[1,1],laFil_Indx[1,2]
      STORE " " TO laFilUsed,laFil_Indx
    ENDIF
    
    lcLst_Fil = laArrTmp1[1,1]
    SELECT sydIndex
    GO TOP
    IF EMPTY(laFil_Indx[1,2]) .OR. EOF()
      puTag      = " "
      lcCurrTag  = " "
      lcExprsion = " "
    ELSE
      puTag      = laFil_Indx[1,2]
      lcCurrTag  = laFil_Indx[1,2]
      LOCATE FOR ALLTRIM(cFile_nam) + ALLTRIM(cFile_tag) == ;
                 ALLTRIM(laArrTmp1[1,1]) + ALLTRIM(lcCurrTag)
      IF FOUND()
        lcExprsion = sydIndex.cIndx_exp
      ELSE
        lcExprsion = " "
      ENDIF
    ENDIF

    SHOW GET lcExprsion
    SELECT sydObjct

    *** Fill both lists :_ 
    *** First list with all the files used in the whole system. ***
    *** Second list with the files used in this program.
    FOR lnCount = 1 TO ALEN('laFilLog',1)
      DEFINE BAR lnCount OF puSorcFil PROMPT (ALLTRIM(laFilLog[lnCount,1]))
      IF ASCAN('laFilUsed',ALLTRIM(laFilLog[lnCount,1])) > 0
        SET SKIP OF BAR lnCount OF puSorcFil .T.
      ENDIF
    ENDFOR
    
    lcMain_Fil = IIF(ASCAN(laFilLogFs,laData[13]) > 0,;
                 laFilLogFs[ASUBSCRIPT(laFilLogFs,;
                 ASCAN(laFilLogFs,ALLTRIM(laData[13])),1),2]," ")
    lcCurrTag  = laFil_Indx[1,2]
    puTag      = laFil_Indx[1,2]
    
    SET EXACT &lcSavExct

  CASE laScrMode[3] .OR. laScrMode[4]
    IF ALEN('laFilUsed',1) = ALEN('laFilLog',1)  AND !EMPTY(laFilUsed[1,1]);
       OR EMPTY(laFilLog[1,1])
      SHOW GET lsSorcFil    DISABLE
      SHOW GET pbMove       DISABLE
      SHOW GET pbAll        DISABLE
    ELSE
      SHOW GET lsSorcFil    ENABLE
      SHOW GET pbMove       ENABLE
      SHOW GET pbAll        ENABLE
    ENDIF  
  
    IF EMPTY(laFilUsed[1,1])
      SHOW GET ibPopBasF DISABLE
      SHOW GET ibPopTag  DISABLE
      SHOW GET lsTrgtFil DISABLE
      SHOW GET pbRemove  DISABLE
      SHOW GET pbRAll    DISABLE
      lcMain_Fil = " "
      lcCurrTag  = " "
      puTag      = " "
    ELSE
      SHOW GET ibPopBasF ENABLE
      SHOW GET ibPopTag  ENABLE
      SHOW GET lsTrgtFil ENABLE
      SHOW GET pbRemove  ENABLE
      SHOW GET pbRAll    ENABLE
    ENDIF
    
    IF !EMPTY(laFilUsed[1,1])
      lcCurrFile = " "
      lcSavExact = SET("EXACT")
      SET EXACT ON
      IF ASCAN(laFilLogFs,laFilUsed[1,1]) > 0
        lcCurrFile = laFilLogFs[ASUBSCRIPT(laFilLogFs,;
                     ASCAN(laFilLogFs,laFilUsed[1,1]),1),1]
      ENDIF
      SET EXACT &lcSavExact
    ELSE
      lcCurrFile = " "
    ENDIF

    lcLst_Fil = lcCurrFile
    SELECT sydIndex
    GO TOP
    IF EMPTY(laFil_Indx[lsTrgtFil,2]) .OR. EOF()
      puTag      = " "
      lcCurrTag  = " "
      lcExprsion = " "
      SHOW GET ibPopTag DISABLE
    ELSE
      puTag      = laFil_Indx[lsTrgtFil,2]
      lcCurrTag  = laFil_Indx[lsTrgtFil,2]
      LOCATE FOR ALLTRIM(cFile_nam) + ALLTRIM(cFile_tag) == ;
                 ALLTRIM(lcCurrFile) + ALLTRIM(lcCurrTag)
      IF FOUND()
        lcExprsion = sydIndex.cIndx_exp
      ELSE
        lcExprsion = " "
      ENDIF
      SHOW GET ibPopTag ENABLE
    ENDIF

    SHOW GET lcExprsion
    SELECT sydObjct
    
    IF laScrMode[4]
      laData[3]  = SUBSTR('PCG',rbObj_Typ,1)
      laData[6]  = SUBSTR('SAU',rbDfnBy,1)
      laData[8]  = (rbPoption = 2)
      laData[10] = .F.
      laData[11] = .T.
      rbModal    = 2
    ENDIF
    IF rbModal = 1
      laData[11] = .F.
      SHOW GET laData[11] DISABLE
    ELSE
      SHOW GET laData[11] ENABLE
    ENDIF
ENDCASE

IF laScrMode[2] .OR. laScrMode[3]
  lcSavExact = SET("EXACT")
  SET EXACT OFF
  IF ASCAN(laApp,laData[1]) > 0
    DO CASE
      CASE _DOS
        lcApp_Desc = laApp[ASCAN(laApp,laData[1]),1]
      CASE _WINDOWS
        ibPopApp   = ASCAN(laApp,laData[1])
    ENDCASE
  ENDIF
  SET EXACT &lcSavExact
ENDIF

SHOW GET laData[10]
SHOW GET laData[11]
SHOW GET rbObj_Typ
SHOW GET rbDfnBy

IF !laScrMode[1] .AND. laData[3] = "P"
  IF laData[10]
    SHOW GET pbPAccess DISABLE
  ELSE
    SHOW GET pbPAccess ENABLE
  ENDIF
ELSE
  SHOW GET pbPAccess DISABLE
ENDIF

IF laData[3] = "G"
  SHOW GET rbModal    DISABLE
  SHOW GET laData[10] DISABLE
  SHOW GET laData[11] DISABLE
  SHOW GET laData[15] DISABLE
  SHOW GET pbMainFil  DISABLE
  SHOW GET pbMainRep  DISABLE  
  IF WVISIBLE('CWDSMPRGFL') 
    =gfChClose('CWDSMPRGFL')
  ENDIF
ELSE
  IF laScrMode[3] .AND. laScrMode[4]
    SHOW GET rbModal    ENABLE
    SHOW GET laData[10] ENABLE
    SHOW GET laData[11] ENABLE
    SHOW GET laData[15] ENABLE
  ENDIF
  IF !laScrMode[1]
    SHOW GET pbMainFil  ENABLE
    SHOW GET pbMainRep  ENABLE  
  ENDIF
ENDIF

SHOW GET ibPopBasF COLOR ,,,,,&lcSelCont,,,&lcEnbCont,&lcDisCont
SHOW GET ibPopTag  COLOR ,,,,,&lcSelCont,,,&lcEnbCont,&lcDisCont

*!**************************************************************************
*!
*!      Function lfvData_1 
*!
*!**************************************************************************
*
FUNCTION lfvData_1

DO CASE
  CASE _DOS
    laData[1] = SUBSTR(gfActPop(2,4,8,49,'laApp',1,1,@lcApp_Desc),1,2)
    =lfRefresh()
  CASE _WINDOWS
    laData[1] = SUBSTR(laApp[ibPopApp,1],1,2)
    SHOW GET ibPopApp
ENDCASE

*!**************************************************************************
*!
*!      Function lfvData_2 
*!
*!**************************************************************************
*
FUNCTION lfvData_2

EXTERNAL ARRAY laData
IF !EMPTY(laData[1]) .AND. !EMPTY(laData[2]) .AND. LASTKEY() = 13
  IF ALLTRIM(ladata[2]) = "?"
    *** Browse the segments according to the seg. no. that have "?". ***
    lcTmpVlu = laData[2]
    =gfBrows('laData[1]',lcScFields,'laData')
    IF lcTmpVlu <> laData[2]
      laScrMode    = .F.
      laScrMode[2] = .T.       && Go to view mode
      SHOW GETS
    ENDIF
  ELSE
    IF SEEK(laData[1]+laData[2])
      SCATTER FIELDS &lcScFields MEMO TO laData
      laScrMode    = .F.
      laScrMode[2] = .T.       && Go to view mode
      SHOW GETS
    ELSE
      IF glAutoAdd         && If automatic add, option = < add >
        lnOption = 2
      ELSE
        *** {Field} is not found in the data file. ***
        *** <Browse> <Add> <Reenter> ***
        lnOption = gfModalGen('QRM00001B00001','Dialog',"Field "+laData[1]+laData[2]) 
      ENDIF  
      DO CASE
        CASE lnOption = 1         && Browse.
          lcTmpVlu = laData[2]
          =gfBrows('laData[1]',lcScFields,'laData')
          IF lcTmpVlu <> laData[2]
            laScrMode    = .F.
            laScrMode[2] = .T.       && Go to view mode
            SHOW GETS
          ENDIF
       CASE lnOption = 2         && Add
         *** Go to add mode. ***
         laScrMode    = .F.
         laScrMode[4] = .T.
         SHOW GETS
       CASE lnOption = 3         && Reenter
         *** Go back to select mode again. ***
         laScrMode    = .F.
         laScrMode[1] = .T.
         laData[2]    = SPACE(10)
         SHOW GET laData[2]
         _CUROBJ      = OBJNUM(laData[2])
      ENDCASE
    ENDIF
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function lfvObj_Typ
*!
*!**************************************************************************
*
FUNCTION lfvObj_Typ

laData[3] = SUBSTR('PCG',rbObj_Typ,1)

IF laData[3] = 'P' .AND. !laData[11]
  SHOW GET pbPAccess ENABLE
ELSE
  SHOW GET pbPAccess DISABLE
ENDIF

IF laData[3] = "G"
  SHOW GET rbModal    DISABLE
  SHOW GET laData[10] DISABLE
  SHOW GET laData[11] DISABLE
  SHOW GET laData[15] DISABLE
  SHOW GET pbMainFil  DISABLE
  SHOW GET pbMainRep  DISABLE    
  IF WVISIBLE('CWDSMPRGFL') 
    =gfChClose('CWDSMPRGFL')
  ENDIF
  IF WVISIBLE('CWDSMPRGRP') 
    =gfChClose('CWDSMPRGRP')
  ENDIF
  
ELSE
  SHOW GET rbModal    ENABLE
  SHOW GET laData[10] ENABLE
  SHOW GET laData[11] ENABLE
  SHOW GET laData[15] ENABLE
  SHOW GET pbMainFil  ENABLE
  SHOW GET pbMainRep  ENABLE  
ENDIF

*!**************************************************************************
*!
*!      Function lfvData_6
*!
*!**************************************************************************
*
FUNCTION lfvData_6

laData[6] = SUBSTR('SAU',rbDfnBy,1)

*!**************************************************************************
*!
*!      Function lfvData_10
*!
*!**************************************************************************
*
FUNCTION lfvData_10

IF laData[10]
  SHOW GET pbPAccess DISABLE
ELSE
  IF laData[3] = "P"
    SHOW GET pbPAccess ENABLE
  ELSE
    SHOW GET pbPAccess DISABLE
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function lfvData_12
*!
*!**************************************************************************
*
FUNCTION lfvData_12

laData[12] = ""
laData[12] = IIF(laData[1]="SY","G","A");
             +"W"+;
             IIF(rbModal=1,"D","R")+;
             IIF(AT(".",laData[2]) > 1,;
             SUBSTR(laData[2],1,AT(".",laData[2])-1),;
             ALLTRIM(laData[2]))

IF rbModal = 1
  laData[11] = .F.
  SHOW GET laData[11] DISABLE
ELSE
  SHOW GET laData[11] ENABLE
ENDIF

*!**************************************************************************
*!
*!      Function lfvData_13
*!
*!**************************************************************************
*
FUNCTION lfvData_13

DO CASE
  CASE _DOS
    lcMain_Fil = gfActPop(0,24,6,59,'laFilUsed',1,1,@lcMain_Fil)

    laData[13] = IIF(ASCAN(laFilLogFs,lcMain_Fil) > 0 ,;
                     laFilLogFs[ASUBSCRIPT(laFilLogFs,;
                     ASCAN(laFilLogFs,lcMain_Fil),1),1]," ")
    =lfRefresh()
  CASE _WINDOWS
    *B601694,1 Reham On 04/06/97  ** Begin **
    *B601694,1 Fix the wrong saving of the base file under windows.
    *laData[13] = laFilLogFs[ibPopBasF,1]
    lcMain_Fil = laFilUsed[ibPopBasF,1]
    laData[13] = IIF(ASCAN(laFilLogFs,lcMain_Fil) > 0 ,;
                     laFilLogFs[ASUBSCRIPT(laFilLogFs,;
                     ASCAN(laFilLogFs,lcMain_Fil),1),1]," ")
    *B601694,1 Reham On 04/06/97  ** End   **
    SHOW GET ibPopBasF
ENDCASE

*!**************************************************************************
*!
*!      Function lfvData_17
*!
*!**************************************************************************
*
FUNCTION lfvData_17

lcOldVal   = laData[17]

GETEXPR "Get skip exprission" TO laData[17] DEFAULT laData[17]

laData[17] = IIF(EMPTY(laData[17]),lcOldVal,laData[17])

*!**************************************************************************
*!
*!      Function lfvData_18
*!
*!**************************************************************************
*
FUNCTION lfvData_18

lcOldVal   = laData[18]

GETEXPR "Get mark exprission" TO laData[18] DEFAULT laData[18]

laData[18] = IIF(EMPTY(laData[18]),lcOldVal,laData[18])

*!**************************************************************************
*!
*!      Function lfvFil_Tag
*!
*!**************************************************************************
*
FUNCTION lfvFil_Tag

DO CASE
  CASE _DOS
    lcCurrTag = gfActPop(11,15,16,27,'sydIndex','CFILE_TAG','CFILE_TAG','lcCurrTag')
    =lfRefresh()
  CASE _WINDOWS
    puTag     = SYDINDEX.CFILE_TAG
    lcCurrTag = SYDINDEX.CFILE_TAG
    SHOW GET puTag
ENDCASE

IF _WINDOWS
  DEACTIVATE POPUP puFilTag
ENDIF

laFil_Indx[lsTrgtFil,2] = lcCurrTag

lcExprsion = sydIndex.cIndx_exp

SHOW GET lcExprsion

*!**************************************************************************
*!
*!      Function lfvSorcFil
*!
*!**************************************************************************
*
FUNCTION lfvSorcFil

IF lsSorcFil <= ALEN('laFilLog',1) AND lsSorcFil <> 0
  SET SKIP OF BAR lsSorcFil OF puSorcFil .T.

  IF !EMPTY(laFilUsed[1,1]) 
    DIMENSION laFilUsed [ALEN(laFilUsed,1) +1,1]
    DIMENSION laFil_Indx[ALEN(laFil_Indx,1)+1,2]
  ENDIF
  laFilUsed[ALEN(laFilUsed,1),1]   = ALLTRIM(laFilLog[lsSorcFil,1])
*B601829,1 Hesham (Start)
*B601829,1 change the first column of the array that hold the files
*B601829,1 information to use the file name insted of the file title
*  laFil_Indx[ALEN(laFil_Indx,1),1] = ALLTRIM(laFilLog[lsSorcFil,1])
  laFil_Indx[ALEN(laFil_Indx,1),1] = PADR(laFilLogfs[lsSorcFil,1],8)

*  lcCurrFile = IIF(ASCAN(laFilLogFs,laFilUsed[ALEN(laFilUsed,1),1]) > 0 ,;
               laFilLogFs[ASUBSCRIPT(laFilLogFs,;
               ASCAN(laFilLogFs,laFilUsed[ALEN(laFilUsed,1),1]),1),1]," ")
lcCurrFile =  laFil_Indx[ALEN(laFil_Indx,1),1] 
*B601829,1 Hesham (End)  
  
  lcLst_Fil = lcCurrFile
  SELECT sydIndex
  GO TOP
  IF EOF()
    laFil_Indx[ALEN(laFil_Indx,1),2] = " "
    lcCurrTag                        = " "
    puTag                            = " "
    SHOW GET ibPopTag DISABLE
    SHOW GET puTag    DISABLE
  ELSE
    laFil_Indx[ALEN(laFil_Indx,1),2] = cFile_Tag
    lcCurrTag                        = cFile_Tag
    puTag                            = cFile_Tag
    SHOW GET ibPopTag ENABLE
    SHOW GET puTag    ENABLE
  ENDIF
  
  SELECT sydObjct
ENDIF  

lnStart  = lsSorcFil
lsSorcFil = 0

FOR lnCount = lnStart TO CNTBAR('puSorcFil')
  IF !SKPBAR('puSorcFil',lnCount)
    lsSorcFil = lnCount 
    EXIT
  ENDIF  
ENDFOR

IF lsSorcFil = 0
  FOR lnCount = 1 TO CNTBAR('puSorcFil')
    IF !SKPBAR('puSorcFil',lnCount)
      lsSource = lnCount 
      EXIT
    ENDIF  
  ENDFOR
ENDIF  

_CUROBJ = OBJNUM(lsSorcFil)
SHOW GETS

*!**************************************************************************
*!
*!      Function lfwTrgtFil
*!
*!**************************************************************************
*
FUNCTION lfwTrgtFil

IF !EMPTY(laFilUsed[lsTrgtFil,1])
*B601829,1 Hesham (Start)
*B601829,1 change the first column of the array that hold the files
*B601829,1 information to use the file name insted of the file title
*  lcCurrFile = " "
*  lcSavExact = SET("EXACT")
*  SET EXACT ON
*  IF ASCAN(laFilLogFs,ALLTRIM(laFilUsed[lsTrgtFil,1])) > 0
*    lcCurrFile = laFilLogFs[ASUBSCRIPT(laFilLogFs,;
                 ASCAN(laFilLogFs,ALLTRIM(laFilUsed[lsTrgtFil,1])),1),1]
*  ENDIF
*  SET EXACT &lcSavExact
   lcCurrFile = laFil_indx[lsTrgtFil,1]
*B601829,1 Hesham (End)   
ELSE
  lcCurrFile = " "
ENDIF

lcLst_Fil = lcCurrFile
SELECT sydIndex
GO TOP
IF EMPTY(laFil_Indx[lsTrgtFil,2]) .OR. EOF()
  puTag      = " "
  lcCurrTag  = " "
  lcExprsion = " "
  SHOW GET ibPopTag DISABLE
  SHOW GET puTag    DISABLE
ELSE
  puTag      = laFil_Indx[lsTrgtFil,2]
  lcCurrTag  = laFil_Indx[lsTrgtFil,2]
  lcExprsion = sydIndex.cIndx_exp
  SHOW GET ibPopTag ENABLE
  SHOW GET puTag    ENABLE
ENDIF

SHOW GET lcExprsion

SELECT sydObjct

IF _DOS
  =lfRefresh()
ENDIF

*!**************************************************************************
*!
*!      Function lfvTrgtFil
*!
*!**************************************************************************
*
FUNCTION lfvTrgtFil

IF laFilUsed[lsTrgtFil,1] = laData[13] .OR. ;
   laFilUsed[lsTrgtFil,1] = lcMain_Fil 
  *** You cannot remove your base file. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00130B00000","DIALOG")
  RETURN
ENDIF

IF lsTrgtFil <= ALEN('laFilUsed',1) AND lsTrgtFil <> 0
  lsSorcFil  = IIF(ASCAN('laFilLog',laFilUsed[lsTrgtFil,1]) > 0 ,;
                   ASUBSCRIPT('laFilLog',;
                   ASCAN('laFilLog',laFilUsed[lsTrgtFil,1]),1),1)
                   
  SET MARK OF POPUP puSorcFil .F.
  SET SKIP OF BAR lsSorcFil OF puSorcFil .F.

  =ADEL(laFilUsed,lsTrgtFil)
  =ADEL(laFil_Indx,lsTrgtFil)
  IF ALEN(laFilUsed,1) > 1
    DIMENSION laFilUsed [ALEN(laFilUsed,1) -1,1]
    DIMENSION laFil_Indx[ALEN(laFil_Indx,1)-1,2]
  ELSE
    STORE " " TO laFilUsed,laFil_Indx
  ENDIF  
ENDIF

lsTrgtFil = 1

_CUROBJ = OBJNUM(lsTrgtFil)

SHOW GETS

*!**************************************************************************
*!
*!      Function lfvUbdPjx
*!
*!**************************************************************************
*
FUNCTION lfvUbdPjx

lnOldrec  = RECNO()
lcPjxName = ' '
lcModId   = ' '
llUpDone  = .F.

*E300683,4 Call *.SPR from screens directory
* DO SMPJXUP.SPR 
DO (gcScrDir + gcWinAppl + '\SMPJXUP.SPR')
*E300683,4 end  
IF llUpDone
  SHOW GETS
ENDIF  

IF lnOldrec > 0 .AND. RECCOUNT() > 0
 GO lnOldrec
ELSE
 GO TOP
ENDIF

*!**************************************************************************
*!
*!      Function lfvSlctpjx
*!
*!**************************************************************************
*
FUNCTION lfvSlctpjx

lcPjxName=GETFILE('PJX',"Select project file to update from...")

IF !EMPTY(lcPjxName)
  lcModId    = UPPER(ALLTRIM(SUBSTR(lcPjxName,RAT('\',lcPjxName)+1,2)))
  lcCurrFile = ALIAS()
  llOpend    = .F.
  IF !USED('SYDAPPL')
    SELECT 0
    USE &gcSysHome.SYDAPPL ORDER 1
    llOpend  = .T.
  ENDIF
  
  IF SEEK(lcModId,'SYDAPPL')
    SHOW GETS WINDOW AWDSMPJXUP ENABLE
  ELSE
*    WAIT "NOT LINKED TO THE SYSTEM" WINDOW
    =gfWait("00210")
  ENDIF  
  IF llOpend 
    USE IN ALIAS('SYDAPPL')
  ENDIF
  SELECT (lcCurrFile)
ENDIF 

*!**************************************************************************
*!
*!      Function lfvUpd
*!
*!**************************************************************************
*
FUNCTION lfvUpd

lcCurrFile = ALIAS()
llOpen     = .F.

IF !USED('SYDOBJCT')
  SELECT 0
  USE &gcSysHome.SYDOBJCT ORDER 1
  llOpen   = .T.  
ENDIF

SELECT 0
USE &lcPjxName ALIAS PROJECT
lnTotRec   = RECCOUNT()
lnCurRec   = 0

SCAN FOR !(TYPE $"sH")
  lnCurRec  = RECNO()
  =gfThermo(lnTotRec,lnCurRec,"Updating from project...",lcPjxName )
  lcObjName = UPPER(ALLTRIM(SUBSTR(NAME,RAT('\',NAME)+1)))
  lcObjName = IIF(ASC(RIGHT(lcObjName,1))< 48,SUBSTR(lcObjName,1,;
                 LEN(lcObjName)-1),lcObjName)
  IF !SEEK(lcModId+lcObjName,'SYDOBJCT')
     INSERT INTO (gcSysHome+"SYDOBJCT") ;
                (cApp_Id,cApObjNam,cApObjTyp,cUpgrdlvl);
         VALUES (lcModId,lcObjName,PROJECT.TYPE,'S')  
    =gfAdd_Info('SYDOBJCT')
  ENDIF
  SELECT PROJECT
ENDSCAN

IF lnTotRec > lnCurRec
  =gfThermo(lnTotRec,lnTotRec,"Updating from project...",lcPjxName )
ENDIF

USE IN ALIAS ('PROJECT')

IF llOpen  
  USE IN ALIAS('SYDOBJCT')
ENDIF

llUpDone  = .T.

SELECT (lcCurrFile)

*!**************************************************************************
*!
*!      Function lfvPAccess
*!
*!**************************************************************************
*
FUNCTION lfvPAccess

DECLARE laArrTmp2[1]

laArrTmp2 = ' '
laTarget  = ' '
lcPrgList = ' '
lcOldPrgs = laData[9]
lcOldMors = laData[8]
llCancel  = .F.
rbPoption = IIF(laData[8],2,1)

=gfSubStr(laData[9],@laArrTmp2,'|')

DECLARE laTarget[ALEN(laArrTmp2,1)]
laTarget = " "

lcSavExac = SET("EXACT")
SET EXACT ON

FOR lnCount = 1 TO ALEN(laArrTmp2,1)
  IF ASCAN(laMainPrgs,laArrTmp2[lnCount]) > 0
    laTarget[lnCount] = laMainPrgs[ASUBSCRIPT(laMainPrgs,;
                           ASCAN(laMainPrgs,laArrTmp2[lnCount]),1),2]
  ELSE
    laTarget[lnCount] = " "
  ENDIF
ENDFOR

SET EXACT &lcSavExac

IF !EMPTY(laTarget[1]) 
  FOR lnCount = 1 TO ALEN(laTarget,1)
    lcPrgList  = lcPrgList+laTarget[lnCount]+CHR(13)+" "
  ENDFOR
ENDIF  

*E300683,4 Call *.SPR from screens directory
* DO SMPOPTN.SPR 
DO (gcScrDir + gcWinAppl + '\SMPOPTN.SPR')
*E300683,4 end  
IF llCancel
  laData[9] = lcOldPrgs
  laData[8]  = lcOldMors 
ENDIF

*!**************************************************************************
*!
*!      Function lfPopShow
*!
*!**************************************************************************
*
FUNCTION lfPopShow

IF laScrMode[2]
  SHOW GETS DISABLE ONLY
  SHOW GET  pbOk    ENABLE
ELSE
  SHOW GETS ENABLE ONLY
ENDIF

*!**************************************************************************
*!
*!      Function lfvPOption
*!
*!**************************************************************************
*
FUNCTION  lfvPOption

laData[8] = (rbPoption = 2)

*!**************************************************************************
*!
*!      Function lfvprgnams
*!
*!**************************************************************************
*
FUNCTION lfvprgnams

IF !EMPTY(laSource[1])
  =gfMover(@laSource,@laTarget,"Program list")

  laData[9] = ''
  lcPrgList  = ' '
  
  IF !EMPTY(laTarget[1])
    FOR lnCount = 1 TO ALEN(laTarget,1)
      lcPrgList  = lcPrgList+laTarget[lnCount]+CHR(13)+" "
      laData[9] = laData[9] + IIF(EMPTY(laData[9]),"","|") +;
                   ALLTRIM(laMainPrgs[ASUBSCRIPT(laMainPrgs,;
                   ASCAN(laMainPrgs,laTarget[lnCount]),1),1])
    ENDFOR
  ENDIF

  SHOW GETS WINDOW AWDSMPOPTN
ELSE
  *** There is no programs for this module except the current program. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00202B00000","DIALOG")
ENDIF

*!*************************************************************************
*!
*!              Function: lfvMovmnts
*!
*!*************************************************************************
*
FUNCTION lfvMovmnts

PARAMETERS lnMovmnts

DO CASE
  CASE lnMovmnts = 1
    _CUROBJ = OBJNUM(lsSorcFil)
    KEYBOARD "{ENTER}"
  CASE lnMovmnts = 2
    _CUROBJ = OBJNUM(lsTrgtFil)
    KEYBOARD "{ENTER}"
ENDCASE

*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!
*!**************************************************************************
*
PROCEDURE lpSavScr

EXTERNAL ARRAY laScrMode    

IF EMPTY(laData[7])
  *** You cannot save this record with empty logical program name. ***
  *** <   Ok   > ***
  = gfModalGen("TRM00117B00000","DIALOG")
  llCSave = .F.    && Flag to say that no save happen.
  _CUROBJ = OBJNUM(laData[7])
  RETURN
ENDIF

*E300704,1 Hesham El-Sheltawi (Start)
*E300704,1 ignore the validation of the base file of the program
*E300704,1 so we can use the control panel with programs without
*E300704,1 base file
*IF laData[3] = "P" .AND. rbModal = 2 .AND. EMPTY(laData[13])
  *** You have to select base file for this program. ***
  *** <  Ok  > ***
*  =gfModalGen("TRM00226B00000","DIALOG")
*  llCSave = .F.    && Flag to say that no save happen.
*  _CUROBJ = OBJNUM(ibPopBasF)
*  RETURN
*ENDIF
*E300704,1 Hesham El-Sheltawi (END)

IF !EMPTY(laFil_Indx[1,1])
  lcSavExact = SET("EXACT")
  SET EXACT ON
  STORE "" TO laData[14],lcFiles,lcTags
  FOR lnCount = 1 TO ALEN(laFil_Indx,1)
    IF ASCAN(laFilLogFs,laFil_Indx[lnCount,1]) > 0
*      lcFiles = lcFiles + IIF(EMPTY(lcFiles),"",",") +;
                ALLTRIM(laFilLogFs[ASUBSCRIPT(laFilLogFs,;
                ASCAN(laFilLogFs,laFil_Indx[lnCount,1]),1),1])
      lcFiles = lcFiles + IIF(EMPTY(lcFiles),"",",") +;
                ALLTRIM(laFil_Indx[lnCount,1]) 
      
      lcTags  = lcTags  + IIF(EMPTY(lcTags),"",",") +;
                IIF(EMPTY(laFil_Indx[lnCount,2]),;
                REPLICATE(CHR(219),10),ALLTRIM(laFil_Indx[lnCount,2]))
      
    ENDIF
  ENDFOR
  lcTags  = STRTRAN(lcTags,CHR(219),' ')
  SET EXACT &lcSavExact
  laData[14] = IIF(!EMPTY(lcFiles),lcFiles + "|" + lcTags,"")
ELSE
  laData[14] = ""
ENDIF

laData[12] = ""

laData[12] = IIF(laData[1]="SY","G","A")+"W"+;
             IIF(rbModal=1,"D","R")+;
             IIF(AT(".",laData[2]) > 1,;
             SUBSTR(laData[2],1,AT(".",laData[2])-1),;
             ALLTRIM(laData[2]))

SELECT SYDOBJCT
IF laScRMode[4]
  APPEND BLANK
ENDIF

GATHER FROM laData FIELDS &lcScFields MEMO

SAVE TO MEMO MREPFXFLT ALL LIKE laOGFxFlt
SAVE TO MEMO MREPVRFLT ALL LIKE laOGVrFlt
DIMENSION laRepFltr[1,2]
STORE '' TO laRepFltr
FOR lnCount = 1 TO ALEN(laRepVars,1)
  IF !EMPTY(laRepVars[lnCount,2])
    IF !EMPTY(laRepFltr[1,1])
      DIMENSION laRepFltr[ALEN(laRepFltr,1)+1,2]
    ENDIF
    laRepFltr[ALEN(laRepFltr,1),1] = laRepVars[lnCount,1]
    laRepFltr[ALEN(laRepFltr,1),2] = laRepVars[lnCount,2]    
  ENDIF
ENDFOR
SAVE TO MEMO MREPFLTR ALL LIKE laRepFltr
=gfAdd_Info()

*** Remove the initial program record from the static file to be recreated
SELECT SYUSTATC
IF SEEK("INI"+ALLTRIM(laData[12]))
  DELETE
ENDIF

SELECT SYDOBJCT

*!**************************************************************************
*!
*!      Function lfvData_15
*!
*!**************************************************************************
*
FUNCTION lfvData_15

laData[15] = IIF(!EMPTY(laData[16]),.T.,.F.)

DECLARE laLog_Tmp[1,1],laFis_Tmp[1,1]

*** Get the descriptions of all the valid entries in array. ***
lcAll_Fis = SUBSTR(laData[16],1,AT("|",laData[16])-1)
=gfSubStr(lcAll_Fis,@laFis_Tmp,',')

*** Get the codes of all the valid entries in array. ***
lcAll_Log = SUBSTR(laData[16],AT("|",laData[16])+1)
=gfSubStr(lcAll_Log,@laLog_Tmp,',')

*** Prepare an array for the list lines hold from both arrays. ***
*** The codes array and the descriptions array.
DECLARE laSubProc[ALEN(laFis_Tmp),1]
FOR lnCount = 1 TO ALEN(laSubProc,1)
  laSubProc[lnCount,1] = ALLTRIM(laFis_Tmp[lnCount,1]) + ;
                          SPACE(10 - LEN(ALLTRIM(laFis_Tmp[lnCount,1]))) + ;
                          "   "+ALLTRIM(laLog_Tmp[lnCount,1])
ENDFOR

*E300683,4 Call *.SPR from screens directory
* DO SMSUBPR.SPR 
DO (gcScrDir + gcWinAppl + '\SMSUBPR.SPR')
*E300683,4 end  
laData[15] = IIF(!EMPTY(laData[16]),.T.,.F.)

SHOW GET laData[15]

*!***************************************************************
*!
*!      Procedure: lpSubShow
*!
*!****************************************************************
*
*** Show procedure of the subprocess screen. ***

PROCEDURE lpSubShow

EXTERNAL ARRAY laScrMode,laData

IF laScrMode[2]
  SHOW GETS DISABLE ONLY
  SHOW GET  pbOk       ENABLE
  SHOW GET  lsSubProc ENABLE
ELSE
  SHOW GETS ENABLE ONLY
ENDIF

*!**************************************************************************
*!
*!      FUNCTION : lfwSubproc
*!
*!**************************************************************************
*
*** When function of the subprocess list. ***

FUNCTION lfwSubproc

IF ALEN(laFis_Tmp,1) = 1 .AND. EMPTY(laFis_Tmp[lsSubProc,1]) .AND. ;
   ALEN(laLog_Tmp,1) = 1 .AND. EMPTY(laLog_Tmp[lsSubProc,1])
   
  lsSubProc = 1
  _CUROBJ   = OBJNUM(pbNew)
  RETURN .F.
ENDIF

lcFisNam = laFis_Tmp[lsSubProc,1]
lcLogNam = laLog_Tmp[lsSubProc,1]

SHOW GET lcFisNam 
SHOW GET lcLogNam

*!**************************************************************************
*!
*!      FUNCTION : lfvFisNam
*!
*!**************************************************************************
*
*** Valid function of the lcFisNam variable the hold the prg object name. ***

FUNCTION lfvFisNam

IF !EMPTY(lcFisNam) .AND. LASTKEY() = 13
  laFis_Tmp [lsSubProc,1] = lcFisNam
  laSubProc [lsSubProc,1] = ALLTRIM(laFis_Tmp[lsSubProc,1]) + ;
                            SPACE(10 - LEN(ALLTRIM(laFis_Tmp[lsSubProc,1]))) + ;
                            "   "+ALLTRIM(laLog_Tmp[lsSubProc,1])
  SHOW GET pbRem     ENABLE
  SHOW GET pbNew     ENABLE
  SHOW GET lsSubProc
  _CUROBJ = OBJNUM(lcLogNam)
ENDIF

*!**************************************************************************
*!
*!      FUNCTION : lfvLogNam
*!
*!**************************************************************************
*
*** Valid function of the lcLogNam variable that hold the description. ***

FUNCTION lfvLogNam

IF !EMPTY(lcLogNam) .AND. LASTKEY() = 13
  laLog_Tmp [lsSubProc,1] = lcLogNam
  laSubProc [lsSubProc,1] = ALLTRIM(laFis_Tmp[lsSubProc,1]) + ;
                            SPACE(10 - LEN(ALLTRIM(laFis_Tmp[lsSubProc,1]))) + ;
                            "   "+ALLTRIM(laLog_Tmp[lsSubProc,1])
  SHOW GET pbRem     ENABLE
  SHOW GET pbNew     ENABLE
  SHOW GET lsSubProc
  _CUROBJ = OBJNUM(lsSubProc)
ENDIF

*!**************************************************************************
*!
*!      FUNCTION : lfvNew
*!
*!**************************************************************************
*
*** Valid function of the push button < NEW >. ***

FUNCTION lfvNew

IF ALEN(laFis_Tmp,1) = 1 .AND. EMPTY(laFis_Tmp[1,1]) .AND. ;
   ALEN(laLog_Tmp,1) = 1 .AND. EMPTY(laLog_Tmp[1,1])

  SHOW GET pbRem  DISABLE
  lsSubProc = 1
ELSE
  DECLARE laSubProc [ALEN(laSubProc,1)+1,1]
  DECLARE laFis_Tmp [ALEN(laFis_Tmp,1) +1,1]
  DECLARE laLog_Tmp [ALEN(laLog_Tmp,1) +1,1]
  SHOW GET pbRem  ENABLE
  lsSubProc = ALEN(laSubProc,1)
  STORE " " TO laSubProc [lsSubProc,1],;
               laFis_Tmp [lsSubProc,1],;
               laLog_Tmp [lsSubProc,1]
ENDIF

lcFisNam = SPACE(10)
lcLogNam = SPACE(30)

SHOW GET pbNew    DISABLE
SHOW GET lcFisNam
SHOW GET lcLogNam
SHOW GET lsSubProc

_CUROBJ = OBJNUM(lcFisNam)

*!**************************************************************************
*!
*!      FUNCTION : lfvRem
*!
*!**************************************************************************
*
*** Valid function of the push button < REM >. ***

FUNCTION lfvRem

IF gfModalGen("QRM00007B00007","ALERT") = 1
  IF ALEN(laFis_Tmp,1) = 1 .AND. ALEN(laLog_Tmp,1) = 1
    laFis_Tmp [1,1] = " "
    laLog_Tmp [1,1] = " "
    laSubProc [1,1] = " "
  ELSE
    =ADEL(laFis_Tmp,lsSubProc)
    DECLARE laFis_Tmp[ALEN(laFis_Tmp,1)-1,1]
  
    =ADEL(laLog_Tmp,lsSubProc)
    DECLARE laLog_Tmp[ALEN(laLog_Tmp,1)-1,1]
  
    =ADEL(laSubProc,lsSubProc)
    DECLARE laSubProc[ALEN(laSubProc,1)-1,1]
  ENDIF

  lsSubProc = 1

  lcFisNam = laFis_Tmp[lsSubProc,1]
  lcLogNam = laLog_Tmp[lsSubProc,1]

  SHOW GET lcFisNam 
  SHOW GET lcLogNam
  SHOW GET lsSubProc
  SHOW GET pbNew      ENABLE

  IF ALEN(laFis_Tmp,1) = 1 .AND. EMPTY(laFis_Tmp[1,1]) .AND. ;
     ALEN(laLog_Tmp,1) = 1 .AND. EMPTY(laLog_Tmp[1,1])
    SHOW GET pbRem DISABLE
  ELSE
    SHOW GET pbRem ENABLE
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      FUNCTION : lfvOk
*!
*!**************************************************************************
*
*** Valid function of the push button < OK >. ***

FUNCTION lfvOk

IF !laScrMode[2]
  IF !EMPTY(laFis_Tmp[1,1]) .AND. !EMPTY(laLog_Tmp[1,1])

    llEmpCod = .F.
    llEmpDes = .F.

    lcAll_Fis = ""

    FOR lnCount = 1 TO ALEN(laFis_Tmp,1)
      IF EMPTY(laFis_Tmp[lnCount,1])
        *** You cannot leave any empty physical name. ***
        *** You have to enter its value or remove this line. ***
        *** <  Ok  > ***
        = gfModalGen("TRM00123B00000","DIALOG","physical name")
        llEmpCod = .T.
        EXIT
      ENDIF

      lcAll_Fis = lcAll_Fis + ;
                  IIF(EMPTY(lcAll_Fis),"",",") + ;
                  ALLTRIM(laFis_Tmp[lnCount,1])
    ENDFOR

    IF llEmpCod
      _CUROBJ = OBJNUM(lsSubProc)
      RETURN
    ENDIF

    lcAll_Log = ""
    FOR lnCount = 1 TO ALEN(laLog_Tmp,1)
      IF EMPTY(laLog_Tmp[lnCount,1])
        *** You cannot leave any empty description. ***
        *** You have to enter its value or remove this line. ***
        *** <  Ok  > ***
        = gfModalGen("TRM00123B00000","DIALOG","description")
        llEmpDes = .T.
        EXIT
      ENDIF

      lcAll_Log = lcAll_Log + ;
                  IIF(EMPTY(lcAll_Log),"",",") + ;
                  ALLTRIM(laLog_Tmp[lnCount,1])
    ENDFOR

    IF llEmpDes
      _CUROBJ = OBJNUM(lsSubProc)
      RETURN
    ENDIF

    laData[15] = .T.
    laData[16] = lcAll_Fis + "|" + lcAll_Log
  ELSE
    laData[15] = .F.
    laData[16] = ""
  ENDIF
ENDIF

CLEAR READ

FUNCTION lfvReport
lcReport = 'APPYINVD'
SELECT SYDREPRT

  *E038033,1 HMA 04/03/2004 Select only Records belong to ARIA27 [BEGIN]

  = SEEK(lcReport)
  *IF SEEK(lcReport) 
  *RESTORE FROM MEMO MREPFXFLT ADDI
  *RESTORE FROM MEMO MREPVRFLT ADDI  
  *RESTORE FROM MEMO MREPAVFLD ADDI
  *=gfbfiltr('laOGVRFlt',@LAOGFIELDH,@LAOGFIELDN,'E',lcReport,'SYREPUVR')  
  LOCATE  WHILE cRep_id=lcReport FOR cVer<>"A40"
  RESTORE FROM MEMO MREPFXFLT ADDI
  RESTORE FROM MEMO MREPVRFLT ADDI  
  RESTORE FROM MEMO MREPAVFLD ADDI
  =gfbfiltr('laOGVRFlt',@LAOGFIELDH,@LAOGFIELDN,'E',lcReport,'SYREPUVR')

  *E038033,1 HMA [END]




FUNCTION lfwData_19
lcOldRep = laData[19]

FUNCTION lfvData_19
IF EMPTY(laData[19])
  DIMENSION laOGFxFlt[1,7],laOGVRFlt[1,7],laRepFltr[1,2],laRepVars[1,2]
  STORE '' TO laOGFxFlt,laOGVRFlt,laRepFltr,laRepVars
ELSE
  IF laData[19] <> lcOldRep OR llBrowse
     SELECT SYDREPRT
     IF !SEEK(laData[19]) OR llBrowse
       *E038033,1 HMA 04/03/2004 Select only Records belong to ARIA27 [BEGIN]

       *DIMEN laData1[1]
       *STORE '' TO laData1
       *PRIVATE lcBrFields,lcFile_ttl
       *lcFile_ttl    = "Reports information"
       *lcBrFields = [cRep_id:H='Report ID',cRep_Name:H='Report Name']
       *=gfBrows(.f.,'CREP_ID','laData1')
       *IF !EMPTY(laData1)
       *  laData[19]=laData1[1]
       *ELSE
       *  laData[19] = lcOldRep
       *ENDIF
       LOCATE WHILE cRep_id <> laData[19] FOR cVer<>"A40"
       DIMEN laData1[1]
       STORE '' TO laData1
       PRIVATE lcBrFields,lcFile_ttl
       lcFile_ttl    = "Reports information"
       lcBrFields = [cRep_id:H='Report ID',cRep_Name:H='Report Name']
       =gfBrows(.f.,'CREP_ID','laData1')
       IF !EMPTY(laData1)
         laData[19]=laData1[1]
       ELSE
         laData[19] = lcOldRep
       ENDIF
        
       *E038033,1 HMA [END]
     ENDIF
     IF laData[19] <> lcOldRep
       RESTORE FROM MEMO MREPFXFLT ADDI
       RESTORE FROM MEMO MREPVRFLT ADDI  
       RESTORE FROM MEMO MREPAVFLD ADDI
       lcVarFltr = gfGenFlt('laOGVrFlt',.F.)
       lcFixFltr = gfGenFlt('laOGFxFlt',.F.)
       =lfInitRepVr()
     ENDIF  
  ENDIF
ENDIF


FUNCTION lfvVarFlt
=gfbfiltr('laOGVRFlt',@LAOGFIELDH,@LAOGFIELDN,'E',laData[19],'SYREPUVR')
lcVarFltr = gfGenFlt('laOGVrFlt',.F.)
SHOW GET lcVarFltr

FUNCTION lfvFixFlt
=gfbfiltr('laOGFxFlt',@LAOGFIELDH,@LAOGFIELDN,'E',laData[19],'SYREPUVR')
lcFixFltr = gfGenFlt('laOGFxFlt',.F.)
SHOW GET lcFixFltr

FUNCTION lfvRepVar
IF lsRepVar > 0
  laRepVars[lsRepVar,2] = GetExpr(laRepVars[lsRepVar,2],'SYDFLFLD';
                        ,'Set Report Variable','LAOGFIELDH',@LAOGFIELDN,.f.,'U',.f.)

ENDIF

FUNCTION lfInitRepVr
SELECT UPPER(ALLT(MFLD_NAME)),'' FROM (gcSysHome+'SYREPUVR');
       WHERE CREP_ID = laData[19] AND lAskRunT;
       INTO ARRAY laRepVars
FOR lnCount = 1 TO ALEN(laRepVars,1)
  lnVarPos = ASCAN(laRepFltr,laRepVars[lnCount,1])
  IF lnVarPos > 0
    lnVarPos = ASUBSCRIPT(laRepFltr,lnVarPos,1)
    laRepVars[lnCount,2] = laRepFltr[lnVarPos,2]
  ENDIF
ENDFOR
SHOW GET lsRepVar