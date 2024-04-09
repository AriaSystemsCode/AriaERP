*:************************************************************************
*:
*: Procedure file: SMINSTL.PRG 
*:                 (Installation)
*:
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: SYSTEM MANAGER
*:         Author: 
*:      Copyright (c) 
*:************************************************************************
* Last modified:  /  /
*E300683,4 AHMED 06/08/97 Add prgs directory path to the calling of programs
*:************************************************************************
*B601317,1 RENEE 11/12/96. Correct the spelling of 'country' in SMINSTL.SCX
EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc
DECLARE laCompInfo[2,1]

llOld_3    = .F.
lcOld_ID   = ""
lcOldColor = ""
lcOldCont  = ""
lcOldFil   = ""
lcDefStat  = IIF(_DOS,"DISABLE","ENABLE")

STORE " " TO lcSave1,lcSave2,lcSave3,lcSave4,lcSave5

llBrowse = .F.

IF !gfSetup()    
  RETURN
ENDIF  

SCATTER  FIELDS &lcScFields TO laData

lcComNam = LOOKUP(sycComp.cCom_name,laData[26],sycComp.cComp_id)

*E300683,4 Call *.SPR from screens directory
* DO SMInstl.SPR 
DO (gcScrDir + gcWinAppl + '\SMInstl.SPR')
*E300683,4 end   

DO gpDispStat
 
*!**************************************************************************
*!
*!      Function: lfShow
*!
*!**************************************************************************
*
FUNCTION lfShow

EXTERNAL ARRAY laData
DO CASE
  CASE laData[6] = "none"
    rbHours = 1
  CASE laData[6] = "24hr"
    rbHours = 2
  CASE laData[6] = "12hr"
    rbHours = 3
ENDCASE   

IF laData[2]
  SHOW GET laData[3] ENABLE
ELSE
  SHOW GET laData[3] DISABLE
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_2
*!
*!**************************************************************************
*
FUNCTION lfvData_2

IF laData[2]
  laData[3] = llOld_3
  SHOW GET laData[3] ENABLE
ELSE
  llOld_3   = laData[3]
  laData[3] = .F.
  SHOW GET laData[3] DISABLE
ENDIF

*!**************************************************************************
*!
*!      Function: lfvOpsDir()
*!
*!**************************************************************************
*
FUNCTION lfvOpsDir
PARAMETERS lnVarOfs,lnParOfs

EXTERNAL ARRAY laData
lcSave1 = laData[lnParOfs]
lcSave2 = laData[lnParOfs+1]
lcSave3 = laData[lnParOfs+2]
DO CASE
  CASE lnParOfs = 14
    *E300683,4 Call *.SPR from screens directory
    * DO SMdosop.SPR 
    DO (gcScrDir + gcWinAppl + '\SMdosop.SPR')
    *E300683,4 end   

  CASE lnParOfs = 17
    lcSave4 = laData[28]
    lcSave5 = laData[30]
    *E300683,4 Call *.SPR from screens directory
    * DO SMwinop.SPR 
    DO (gcScrDir + gcWinAppl + '\SMwinop.SPR')
    *E300683,4 end   
  CASE lnParOfs = 20
    *E300683,4 Call *.SPR from screens directory
    * DO SMunxop.SPR 
    DO (gcScrDir + gcWinAppl + '\SMunxop.SPR')
    *E300683,4 end   
  CASE lnParOfs = 23
    *E300683,4 Call *.SPR from screens directory
    * DO SMmacop.SPR 
    DO (gcScrDir + gcWinAppl + '\SMmacop.SPR')
    *E300683,4 end   
ENDCASE

IF lnVarOfs <> 10
  laData[lnVarOfs] =  !EMPTY(laData[lnParOfs])   .AND.;
                      !EMPTY(laData[lnParOfs+1]) .AND.;
                      !EMPTY(laData[lnParOfs+2])
ELSE
  laData[lnVarOfs] =  !EMPTY(laData[lnParOfs])   .AND.;
                      !EMPTY(laData[lnParOfs+1]) .AND.;
                      !EMPTY(laData[lnParOfs+2]) .AND.;
                      !EMPTY(laData[28]) .AND.;
                      !EMPTY(laData[30])
ENDIF

SHOW GET laData[lnVarOfs]
 
*!**************************************************************************
*!
*!      Function: lfvHours
*!
*!**************************************************************************
*
FUNCTION lfvHours

EXTERNAL ARRAY laData
DO CASE
  CASE rbHours = 1
    laData[6] = "none"
  CASE rbHours = 2
    laData[6] = "24hr"
  CASE rbHours = 3
    laData[6] = "12hr"
ENDCASE         
 
*!**************************************************************************
*!
*!      Function: lfvExit()
*!
*!**************************************************************************
*
FUNCTION lfvExit
PARAMETERS lnAryOfs

EXTERNAL ARRAY laData
laData[lnAryOfs]   = lcSave1
laData[lnAryOfs+1] = lcSave2
laData[lnAryOfs+2] = lcSave3     

IF lnAryOfs = 17
  laData[28] = lcSave4
  laData[30] = lcSave5
ENDIF

*!**************************************************************************
*!
*!      Function: lfvDirPath
*!
*!**************************************************************************
*
FUNCTION lfvDirPath
PARAMETERS 	lcVarNam

lcNewPath = GETDIR() 

IF !EMPTY(lcNewPath)
  &lcVarNam =lcNewPath + IIF(60 - LEN(lcNewPath) > 0 ,;
                             SPACE(60 - LEN(lcNewPath)) , '')
  =gfUpdate()
ENDIF

*&lcVarNam = IIF(!EMPTY(lcNewPath),lcNewPath,&lcVarNam)

SHOW GET &lcVarNam 

*!**************************************************************************
*!
*!      Function: lfvPExit
*!
*!**************************************************************************
*
Function lfvPExit

EXTERNAL ARRAY laData
GATHER FROM laData FIELDS &lcScFields MEMO
=gfAdd_Info()

gcAllCmp  = ALLTRIM(sycinst.cInsAllCmp)
gcIntCont = laData[31]

IF FILE (gcDef_path+"OLDPATH.DAT")
  lcSavSAf = SET ('SAFETY')
  SET TALK    OFF
  SET SAFETY  OFF
  SET CONSOLE OFF
  SET TEXTMERGE ON
  SET TEXTMERGE TO (gcDef_path+"OLDPATH.DAT")
  \\<<ALLTRIM(sycinst.cinsysfdr)>>
  \<<ALLTRIM(sycinst.cinsallcmp)>>
  \<<ALLTRIM(sycinst.cinsdospd)>>
  \<<ALLTRIM(sycinst.cinsdosrd)>>
  \<<ALLTRIM(sycinst.cinsdoswd)>>
  \<<ALLTRIM(sycinst.cinswinpd)>>
  \<<ALLTRIM(sycinst.cinswinrd)>>
  \<<ALLTRIM(sycinst.cinswinwd)>>
  \<<ALLTRIM(sycinst.cinswinbm)>>
  \<<ALLTRIM(sycinst.cinsrsrdr)>>
  SET TEXTMERGE TO
  SET TEXTMERGE OFF
  SET CONSOLE ON
  SET SAFETY &lcSavSAf
ENDIF
glQuitting = .T.

*!**************************************************************************
*!
*!      Function: lfvPCancel
*!
*!**************************************************************************
*
Function lfvPCancel

glQuitting = .T.

*!**************************************************************************
*!
*!      Function lfwCompId
*!
*!**************************************************************************
*
Function lfwCompId

lcOld_ID = laData[26]

*!**************************************************************************
*!
*!      Function lfvCompId
*!
*!**************************************************************************
*
Function lfvCompId

IF (!EMPTY(laData[26]) .AND. LASTKEY() = 13) .OR. llBrowse
  IF SEEK(laData[26],"SYCCOMP") .AND. !llBrowse
    lcComNam = sycComp.cCom_name
  ELSE
    lcSavAlias    = SELECT(0)
    laCompInfo[1] = laData[26]
    laCompInfo[2] = lcComNam
    lcOldBrow     = lcBrFields    
    lcBrFields    = "cComp_id :H='Company ID',cCom_name :H='Company Name'"

    SELECT SYCCOMP
   
    IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF
    
    =gfBrows(.F.,"cComp_id,cCom_name","laCompInfo")
    
    lcBrFields = lcOldBrow
    SELECT(lcSavAlias)
    
    IF laData[26] = laCompInfo[1] 
      laData[26] = lcOld_ID
    ELSE
      laData[26] = laCompInfo[1]
      lcComNam   = laCompInfo[2]
    ENDIF
  ENDIF  
ELSE
  *ymo
  IF lcOld_ID <> laData[26]
    laData[26] = "  "
    lcComNam   = SPACE(30)
  ENDIF
ENDIF

llBrowse = .F.
SHOW GETS

*!**************************************************************************
*!
*!      Function lfvDefBmp
*!
*!**************************************************************************
*
FUNCTION lfvDefBmp

lcOldFil   = laData[30]

laData[30] = gfChngWall(.T.)

laData[30] = IIF(EMPTY(laData[30]),lcOldFil,laData[30])

SHOW GET laData[30]



*!**************************************************************************
*!
*!      Function: lfwcountry
*!
*!**************************************************************************
*
FUNCTION lfwcountry
lcOldCont=laData[31]


*!**************************************************************************
*!
*!      Function: lfvcountry
*!
*!**************************************************************************
*
FUNCTION lfvcountry
IF (!EMPTY(laData[31]) AND !SEEK(laData[31],'SYCINT')) OR llBrowse 
  PRIVATE lcBrFields,lcFile_ttl,lcSelect
  lcSelect = SELECT()
  SELECT SYCINT
  lcBrFields=gfDbfField('SYCINT')
  DIMENSION laData1[1]
  STORE '' TO laData1
  lcFile_ttl    = "Internationals"
  =gfBrows(.F.,"cCont_Code","laData1")
  laData[31]=laData1[1]
  SELECT (lcSelect)
  SHOW GET laData[31]
ENDIF
IF EMPTY(laData[31])
  laData[31] = lcOldCont
ENDIF
llBrowse = .F.
=lfRefresh(WOUTPUT())

