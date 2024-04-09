*!********************************************************************
*: Program file  : SMCNMAN.PRG
*: Program desc. : MAIN PROGRAM TO CONVERTING FILES FROM ARIA20 TO ARIA26
*: For screen    : None.
*:         System: ARIA APPAREL SERIES
*:         Module: SM.
*:      Developer: Ahmed Salah Shalaby - (SSH)
*!********************************************************************
*: Calls         : lfConSetup()
*!********************************************************************
*: Passed Parameters  :
*!********************************************************************
*: Example            : DO CONVERT 
*!********************************************************************
* Modifications
*B#604140,1 SSH Alow the user to select one file to convert
***************************************************************************
lcErr=ON('ERROR')
ON ERROR DO lfErrHand WITH LINENO() , PROG()
lcErorFil  = gfTempName()
CREATE TABLE (gcWorkDir)+(lcErorFil) (cErro C(100),cLine C(10), cProgId C(10))
lcErroSrc  = gcWorkDir+"Error20.TXT"
lcOSource  = SPACE(1)   && Old surce directory.
lcSource   = SPACE(1)   && Surce directory.
lcOTarget  = SPACE(1)   && Old target directory.
lcTarget   = SPACE(1)   && Target directory.
lnThermSiz = 32        && The default size of the thermometers.
DECLARE laComp2Con[1,2]
*B#604140,1 SSH Alow the user to select one file to convert
DECLARE laSingFile[1]
laSingFile = ""
*B#604140,1 SSH Alow the user to select one file to convert
laComp2Con = SPACE(1)
lnCompNo   = 1
lcCurrFile = "Preparing files for conversion  "
lcOATherm  = SPACE(1)
lcFTherm   = SPACE(1)
lcOAPer    = "0  %"
lcFPer     = "0  %"
DO (gcScrDir +  gcAct_Appl + '\Smconvrt.SPR')
ON ERROR &lcErr
IF FILE(gcWorkDir+lcErorFil+".DBF")
  SELECT (lcErorFil)
  GO TOP
  IF FILE(gcWorkDir+lcErorFil+".DBF")
    SELECT (lcErorFil)
    GO TOP
    IF EOF()
      APPEND BLANK
      REPLACE cErro  WITH "Conversion process has been canceled."
    ENDIF
  ENDIF
  DO (gcScrDir +  gcAct_Appl + '\Smcneror.SPR')
  COPY TO (lcErroSrc) TYPE DELIMITED
  IF USED(lcErorFil)
    USE IN (lcErorFil)
  ENDIF
  ERASE (gcWorkDir)+(lcErorFil)+".DBF"
ENDIF

*!********************************************************************
*: Program file  : lfvSource
*: Program desc. : Valid source function
*: For screen    : None.
*:      Developer: Ahmed Salah Shalaby - (SSH)
*!********************************************************************
*: Passed Parameters  :
*!********************************************************************
*: Example            : =lfvSource()
*!********************************************************************
* Modifications
***************************************************************************
FUNCTION lfvSource

lcSource = GETDIR("","Select the source 2.0 'CONTROL.DBF' directory.")
IF !(lcOSource == lcSource)
  WAIT "Validating entery.. Please wait" WINDOW NOWAIT
  IF !EMPTY(lcSource) AND lfChkCon(lcSource,"Aria 2.0",.T.)
    lcStatus = IIF(EMPTY(lcTarget),"DISABLE","ENABLE")
    SHOW GET pbConvert &lcStatus
    =lfGetComp()
  ELSE
    IF !EMPTY(lcSource)
      lcOvrWrit = "Invalid Aria 2.0 directory."
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcOvrWrit)
    ENDIF
    lcSource = lcOSource
    _CUROBJ = _CUROBJ
  ENDIF  
  WAIT CLEAR 
  SHOW GET lcSource 
ENDIF

*!********************************************************************
*: Program file  : lfvTarget
*: Program desc. : Valid target
*: For screen    : None.
*:      Developer: Ahmed Salah Shalaby - (SSH)
*!********************************************************************
*: Passed Parameters  :
*!********************************************************************
*: Example            : =lfvTarget()
*!********************************************************************
* Modifications
***************************************************************************
FUNCTION lfvTarget

lcTarget = GETDIR("","Select the target 2.6 'CONTROL.DBF' directory.")
IF !(lcOTarget == lcTarget)
  WAIT "Validating entery.. Please wait" WINDOW NOWAIT
  IF !EMPTY(lcTarget) AND lfChkCon(lcTarget,"Aria 2.6",.F.)
    lcStatus = IIF(EMPTY(lcSource),"DISABLE","ENABLE")
    SHOW GET pbConvert &lcStatus
  ELSE
    IF !EMPTY(lcTarget)
      lcOvrWrit = "Invalid Aria 2.6 directory."
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcOvrWrit)
    ENDIF
    lcTarget = lcOTarget
    _CUROBJ = _CUROBJ
  ENDIF  
  WAIT CLEAR 
  SHOW GET lcTarget
ENDIF

*!********************************************************************
*: Program file  : lfChkCon
*: Program desc. : Fucntion to check the path
*: For screen    : None.
*:      Developer: Ahmed Salah Shalaby - (SSH)
*!********************************************************************
*: Passed Parameters  :
*!********************************************************************
*: Example            : =lfChkCon()
*!********************************************************************
* Modifications
***************************************************************************
FUNCTION lfChkCon
PARAMETERS lcPath, lcPathTitl, llIsA20
PRIVATE llRet

lcPath     = ALLTRIM(lcPath)
llContErr  = .F.
IF FILE(lcPath+"CONTROL.DBF")
  
  IF llIsA20
    IF !FILE (lcPath+"INIT.FXP")
      RETURN .F.
    ENDIF  
  ELSE
    IF !FILE (lcPath+"ARIA25.MPX")
      RETURN .F.
    ENDIF
  ENDIF
  
  IF ALLTRIM(lcSource) == ALLTRIM(lcTarget)
    llRet = .F.
  ELSE
    lcCurrField = ""
    llOpnCat    = .T.
    lcOldDef    = FULLPATH("")
    =gfOpenFile(lcPath+'CONTROL',lcPath+'CONTROL','SH')
    SCAN WHILE !llContErr
      lcCurrField = "DD"
      SET DEFAULT TO (lcPath + ALLTRIM(&lcCurrField))
      lcCurrField = "LB"
      SET DEFAULT TO (lcPath + ALLTRIM(&lcCurrField))
      IF llOpnCat AND !llContErr
        llOpnCat = .F.
        lcCatFile = lcPath + ALLTRIM(Control.LB) + "CATALOG"
        IF FILE(lcCatFile+".DBF")
          SELECT 0
          USE (lcCatFile) SHARED
          INDEX ON EXT+DATAFILE+FILE TAG CATALOG OF gcWorkDir+"tCATALOG"
          IF !SEEK("DBF")
            llContErr = .T.
          ENDIF
          USE IN Catalog
        ELSE
          llContErr = .T.
        ENDIF
      ENDIF  
    ENDSCAN
    SET DEFAULT TO (lcOldDef)
    IF !llContErr AND !EMPTY(lcTarget) AND !EMPTY(lcSource)
      lcToOpen = IIF (llIsA20,lcTarget,lcSource)
      *USE (lcToOpen+"Control.DBF") ORDER Control ALIAS SorCont IN 0
      =gfOpenFile(lcToOpen+'CONTROL',lcToOpen+'CONTROL','SH',"SorCont")
      SELECT SorCont
      lcSysCode = ALLTRIM(STR(VAL(SorCont.SysCode)))
      COUNT ALL FOR !DELETED() AND SEEK("0"+lcSysCode,"Control") ;
                    .AND. PADL(SYSCODE,1) = PADL(laComp2Con[lnCompNo],1) TO lnMaxComp
      USE IN SorCont
      IF lnMaxComp = 0
        llContErr = .T.
      ENDIF
    ENDIF  
    llRet = !llContErr
    USE IN CONTROL
  ENDIF  
ELSE
  llRet = .F.
ENDIF    

RETURN llRet

*!********************************************************************
*: Program file  : lfConvert
*: Program desc. : Convert function
*: For screen    : None.
*:      Developer: Ahmed Salah Shalaby - (SSH)
*!********************************************************************
*: Passed Parameters  :
*!********************************************************************
*: Example            : =lfConvert()
*!********************************************************************
* Modifications
***************************************************************************
FUNCTION lfConvert
PRIVATE lnCurTask,  lcOvrWrit

lcOvrWrit = "The conversion procces will " + ;
            "overwrite all data files in Aria 2.6 corresponding Company."
IF gfModalGen('INM00000B00012',.F.,.F.,.F.,lcOvrWrit) = 1
  lcTherm    = ""
  SHOW GET lcTherm
  = lfFixSequ()
*B#604140,1 SSH Alow the user to select one file to convert
  IF ALLTRIM(laSingFile[lnFileNo]) = "All"
    SELECT SYCONFIL
    SET FILTER TO
  ELSE
    SELECT SYCONFIL
    SET FILTER TO (ALLTRIM(cFileName) = ALLTRIM(laSingFile[lnFileNo]))
  ENDIF
  *--- SSH
  IF ALLTRIM(laSingFile[lnFileNo]) == "Fix-Fabric"
    SELECT SYCONFIL
    SET FILTER TO
    SET FILTER TO (ALLTRIM(cFileName) $ "ITEM,CTKTBOM,CUTTKTH,CUTTKTL,BOM")
  ENDIF
  *--- SSH
*B#604140,1 SSH Alow the user to select one file to convert
  DO (FULLPATH("")+ "PRGS\" + gcAct_Appl +"\SMCNDATA.FXP")
  CLEAR READ
  IF FILE(gcWorkDir+lcErorFil+".DBF")
    SELECT (lcErorFil)
    GO TOP
    IF EOF()
      APPEND BLANK
      REPLACE cErro  WITH "Conversion process completed successfully."
    ENDIF
  ENDIF
ENDIF

*!********************************************************************
*: Program file  : lfClose
*: Program desc. : Close function
*: For screen    : None.
*:      Developer: Ahmed Salah Shalaby - (SSH)
*!********************************************************************
*: Passed Parameters  :
*!********************************************************************
*: Example            : =lfClose()
*!********************************************************************
* Modifications
***************************************************************************
FUNCTION lfClose
ON ERROR &lcErr
CLEAR READ


*!********************************************************************
*: Program file  : lfUpdtTher
*: Program desc. : Update theremometer function.
*: For screen    : None.
*:      Developer: Ahmed Salah Shalaby - (SSH)
*!********************************************************************
*: Passed Parameters  :
*!********************************************************************
*: Example            : =lfUpdtTher()
*!********************************************************************
* Modifications
***************************************************************************
FUNCTION lfUpdtTher
PARAMETER lcCurr , lnFilNo
PRIVATE lnPersent, lnMax, lnAct

lnPersent = (lnFilNo/RECCOUNT("SYCONFIL")) *100
lcPerSign  = SPACE(1) + "%" + SPACE(1)
IF lnPersent > 0
  lcOATherm  = REPLICATE(CHR(219),lnFilNo*(lnThermSiz/RECCOUNT("SYCONFIL")))
ELSE
  lcOATherm  = ""
ENDIF
lcPersent  = ALLTRIM(STR(lnPerSent,6,2))+lcPerSign
SHOW GET lcOATherm
lcOAPer = lcPersent
=lfRefresh()

*!********************************************************************
*: Program file  : lfFixSequ
*: Program desc. : Function to fix sequence file .
*: For screen    : None.
*:      Developer: Ahmed Salah Shalaby - (SSH)
*!********************************************************************
*: Passed Parameters  :
*!********************************************************************
*: Example            : =lfFixSequ()
*!********************************************************************
* Modifications
***************************************************************************
FUNCTION lfFixSequ
PRIVATE lnAlias, lcLib , llFixSeq
llFixSeq = .F.
lnAlias = SELECT()

SELECT 0
*USE (lcSource + "Control") ORDER Control
=gfOpenFile(lcSource +'Control',lcSource +'Control','SH')
SELECT Control
lcData = ALLTRIM(DD)
=gfOpenFile(lcSource +lcData +'Sequence',"",'SH')
SELECT Sequence
llFixSeq = (TYPE("CRMEMO")="U")
= AFIELDS(laFields)

USE IN Sequence
IF ASCAN (laFields,"CREDITMEMO") > 0 .AND. llFixSeq
  lnNewCol = ALEN(laFields,1) + 1
  DIMENSION laFields[lnNewCol,4]
  laFields[lnNewCol,1] = "CRMEMO"
  laFields[lnNewCol,2] = "N"
  laFields[lnNewCol,3] = 6
  laFields[lnNewCol,4] = 0

  lcTempName = gfTempName()
  CREATE DBF  (lcSource+lcData+lcTempName) FROM ARRAY laFields
  APPEND FROM (lcSource+lcData+"Sequence")
  USE IN (lcTempName)
  ERASE  (lcSource+lcData+"Sequence"+".DBF")
  RENAME (lcSource+lcData+lcTempName+".DBF") TO ;
         (lcSource+lcData+"Sequence.DBF")
 
  *USE (lcSource + lcData + "Sequence") IN 0
  =gfOpenFile(lcSource +lcData +'Sequence',"",'SH')
  SELECT Sequence
  REPLACE ALL CrMemo WITH CreditMemo
  USE IN Sequence
ENDIF
USE IN CONTROL

SELECT (lnAlias)

*!********************************************************************
*: Program file  : lfDummy
*: Program desc. : Dummy function to return .F.
*: For screen    : None.
*:      Developer: Ahmed Salah Shalaby - (SSH)
*!********************************************************************
*: Passed Parameters  :
*!********************************************************************
*: Example            : =lfDummy()
*!********************************************************************
* Modifications
***************************************************************************
FUNCTION lfDummy

RETURN .F.


*!********************************************************************
*: Program file  : lfGetComp
*: Program desc. : Funciton to get the company
*: For screen    : None.
*:      Developer: Ahmed Salah Shalaby - (SSH)
*!********************************************************************
*: Passed Parameters  :
*!********************************************************************
*: Example            : =lfGetComp()
*!********************************************************************
* Modifications
***************************************************************************
FUNCTION lfGetComp

PRIVATE lnOldAls , llClsCnt , llByME
llClsCnt = .F.
lnOldAls = SELECT(0)
lcTempCon = gfTempName()
IF FILE(lcSource+"CONTROL.DBF") .AND. ;
  (!USED("CONTROL") .OR. (lcSource+"CONTROL.DBF")<>DBF("CONTROL"))
  *USE (lcSource+"CONTROL.DBF") ORDER Control ALIAS &lcTempCon IN 0
  =gfOpenFile(lcSource+'CONTROL',lcSource+'CONTROL','SH',lcTempCon)
  llClsCnt = .T.
ENDIF
SELECT (lcTempCon)
SELECT SysCode+" - "+ALLTRIM(Company);
 FROM &lcTempCon WHERE .T. INTO ARRAY laComp2Con;
 ORDER BY SysCode
IF _TALLY=0
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Source Control file is empty, no available company to convert.')
ELSE
  SHOW GET lnCompNo ENABLE
  =lfVCmpPop(1)
  USE IN IIF(llClsCnt,lcTempCon,0)
ENDIF
llByME = .F.
IF !USED("SYCONFIL")
  llByME = gfOpenFile(gcsyshome+"SYConFil","cFileNo","SH")
ENDIF
*B#604140,1 SSH Alow the user to select one file to convert
SELECT SYCONFIL
SET FILTER TO
SELECT cFileName FROM SYCONFIL WHERE .T. INTO ARRAY laSingFile;
       ORDER BY cFileName
DIMENSION laSingFile[ALEN(laSingFile)+2]
laSingFile[ALEN(laSingFile)-1] = "Fix-Fabric"
laSingFile[ALEN(laSingFile)] = "All"
lnFileNo = ALEN(laSingFile)

SHOW GET lnFileNo ENABLE
*B#604140,1 SSH Alow the user to select one file to convert
SELECT(lnOldAls)

*!********************************************************************
*: Program file  : lfVCmpPop
*: Program desc. : Funciton Valid the company popup.
*: For screen    : None.
*:      Developer: Ahmed Salah Shalaby - (SSH)
*!********************************************************************
*: Passed Parameters  :
*!********************************************************************
*: Example            : =lfVCmpPop()
*!********************************************************************
* Modifications
***************************************************************************
FUNCTION lfVCmpPop
PARAMETER lnComp

IF !EMPTY(laComp2Con[lnComp])
  SHOW GET pbTarget ENABLE
  lnCompNo = lnComp
ENDIF

*!********************************************************************
*: Program file  : lfUpdFTher
*: Program desc. : Funciton to update the thermometer.
*: For screen    : None.
*:      Developer: Ahmed Salah Shalaby - (SSH)
*!********************************************************************
*: Passed Parameters  :
*!********************************************************************
*: Example            : =lfUpdFTher()
*!********************************************************************
* Modifications
***************************************************************************
FUNCTION lfUpdFTher
PARAMETER lcMessg , lnRecNo , lcFilNam , lnTotRecNo
PRIVATE lnPer
lnPer = (lnRecNo/lnTotRecNo) *100
lcPerSign  = SPACE(1) + "%" + SPACE(1)
IF lnPer > 0
  lcFTherm  = REPLICATE(CHR(219),lnRecNo*(lnThermSiz/lnTotRecNo))
ELSE
  lcFTherm  = ""
ENDIF
lcPersent  = ALLTRIM(STR(lnPer,6,2))+lcPerSign
SHOW GET lcFTherm
lcCurrFile = lcMessg
SHOW GET lcCurrFile
lcFPer = lcPersent
=lfRefresh()

*!********************************************************************
*: Program file  : lfErrHand
*: Program desc. : Error handeler function.
*: For screen    : None.
*:      Developer: Ahmed Salah Shalaby - (SSH)
*!********************************************************************
*: Passed Parameters  :
*!********************************************************************
*: Example            : =lfErrHand()
*!********************************************************************
* Modifications
***************************************************************************
FUNCTION lfErrHand
PARAMETER lnLineNo , lcProgNam , lcMsage
PRIVATE lnOldAls

IF TYPE("lcMsage")="L"
  lcMsage = PADR(MESSAGE(),70)
ENDIF
lnOldAls = SELECT(0)
WAIT WINDOW "Error : "+ALLTRIM(PADR(MESSAGE(),70)) NOWAIT
SELECT (lcErorFil)
APPEND BLANK
*--- (cErro C(100) , cLine C(10) , cProgId C(10))
REPLACE cErro   WITH lcMsage,;
        cLine   WITH ALLTRIM(STR(lnLineNo,10)),;
        cProgId WITH lcProgNam
WAIT CLEAR
SELECT(lnOldAls)


*!********************************************************************
*: Program file  : lfErrHand
*: Program desc. : Get the error TXT file directory to save it
*: For screen    : None.
*:      Developer: Ahmed Salah Shalaby - (SSH)
*!********************************************************************
*: Passed Parameters  :
*!********************************************************************
*: Example            : =lfErrHand()
*!********************************************************************
* Modifications
***************************************************************************
FUNCTION lfvErrSrc

lcErroSrc = GETFILE('TXT', 'Log file: def.(Error20.txt) ', 'Select',1)
SHOW GET lcErroSrc DISABLE