*:************************************************************************
*:
*: Procedure file: SMWRKST.PRG 
*:                 (Workstaions)
*:
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: SYSTEM MANAGER
*:         Author: Reham Aly Alallamy
*:      Copyright (c) 
*:************************************************************************
* Last modified:  /  /
*E300683,4 AHMED 06/04/97 Add prgs directory path to the calling of programs
*:************************************************************************

EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc
DECLARE laKeyField [1,4]
 
laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'CWRKST_ID'
laKeyField[1,4] = 1

laDefProc[9] = .F.

lcDrvStat = " "

llBrowse  = .F.

IF !gfSetup()    
  RETURN
ENDIF  

IF !WEXIST(lcBaseWind)
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
ENDIF

lcDrvStat = IIF(laScrMode[3] .OR. laScrMode[4],;
                IIF(laData[6],"ENABLE","DISABLE"),"DISABLE")

*E300683,4 Call *.SPR from screens directory
* DO SMWrkst.SPR 
DO (gcScrDir + gcWinAppl + '\SMWrkst.SPR')
*E300683,4 end    
*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow
EXTERNAL ARRAY laScrMode

IF laScrMode[1]
  laData[6] = .F.
  SHOW GET pbZoom DISABLE
ELSE
  SHOW GET pbZoom ENABLE
ENDIF

IF laScrMode[3] .OR. laScrMode[4]
  IF laData[6]
    SHOW GET pbGetDrv  ENABLE
    SHOW GET laData[7] ENABLE
  ELSE
    SHOW GET pbGetDrv  DISABLE
    SHOW GET laData[7] DISABLE
  ENDIF
ELSE
  SHOW GET pbGetDrv  DISABLE
  SHOW GET laData[7] DISABLE
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
*
FUNCTION lfvData_1
 
IF llBrowse .OR. (!EMPTY(laData[1]) AND LASTKEY()= 13)
  =gfSeekRec()
ENDIF
llBrowse = .F.

*!**************************************************************************
*!
*!      Function: lfvGetStn
*!
*!**************************************************************************
*
FUNCTION lfvGetStn

laData[1] = IIF(!EMPTY(gcStation),gcStation,SPACE(6))

IF !EMPTY(laData[1])
  SHOW GET laData[1]
  =gfSeekRec()
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_6
*!
*!**************************************************************************
*
FUNCTION lfvData_6

IF laData[6]
  SHOW GET pbGetDrv  ENABLE
  SHOW GET laData[7] ENABLE
ELSE
  laData[7] = ""
  SHOW GET pbGetDrv  DISABLE
  SHOW GET laData[7] DISABLE
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_7
*!
*!**************************************************************************
*
FUNCTION lfvData_7

lcOldDir  = laData[7]
IF !EMPTY(laData[7])
  laData[7] = GETDIR(laData[7],"Select the temp. path for the local drive.")
ELSE
  laData[7] = GETDIR('',"Select the temp. path for the local drive.")
ENDIF  

laData[7] = IIF(EMPTY(laData[7]),lcOldDir,laData[7])

SHOW GET laData[7]


*!********************************************************************
*!
*!              Procedure: lpSavScr
*!
*!********************************************************************
*
PROCEDURE lpSavScr

EXTERNAL ARRAY laScrMode,laData

IF laData[6] .AND. EMPTY(laData[7])
  *** You have to get path for local drive.  You cannot leave it empty. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00218B00000","DIALOG")
  llCSave = .F.    && Flag to say that no save happen.
  _CUROBJ = OBJNUM(pbGetDrv)
  RETURN
ENDIF

IF laScrMode[4]
  APPEND BLANK
ENDIF

GATHER FROM laData FIELDS &lcScFields MEMO 
=gfAdd_Info()
