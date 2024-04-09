SET DELETED ON
*-- BADRAN 07/01/2001 Open the SYDAPPL in menu case [Begin]
llOpenAppl = .F.
lcStartDir  = SET("DEFAULT")  && Badran at 09/30/2001 Save old work area.
STORE "" TO lcTarMnu, lcOrgMnu, lcTmpMnu
llEscDummy = .F.  && Dummy Esc Variable.
DECLARE laTechInfo[1,2]

*-- B605846,1 04/10/2002 Declare two new array to handling insert & remove sub menu [Start]
*-- laMnuInsrt = Array to hold the sub menu in the orginal menu
*--              to privant its child to be apple to insert into the target menu
*-- laMnuRemve = Array to hold the sub menu in the target menu
*--              to privant its child to be apple to remove from the target menu
DECLARE laMnuInsrt[1], laMnuRemve[1]
STORE SPACE(0) TO laMnuInsrt,laMnuRemve
*-- B605846,1 04/10/2002 Declare two new array to handling insert & remove sub menu [End]

laTechInfo = ""
*-- BADRAN 07/01/2001 Open the SYDAPPL in menu case [End  ]
STORE "DISABLE" TO lcInsPrpt, lcDelPrpt
STORE '' TO lcPrgDir,lcRepDir,lcScrDir,lcBmpDir,lcSysDirect,lcEntID,lcOutType,lcRootDir
=lfDfRunDir()
*B604754,1 HBG 07/08/2001 Eliminate SYSFILES dialog box selection [Begin]
*lcSysDirect = GETDIR('','Select System Files Directory')
*-- Badran 09/30/2001 Fix getting the sysfiles directory [Begin]
lcOldPath = SET("FULLPATH")
SET FULLPATH ON
lcCurrPath = ALLTRIM(FULLPATH(SET("DEFAULT")))
lcCurrPath = lcCurrPath + IIF(RIGHT(lcCurrPath,1)="\","","\")
*-- If you directly run from the system files directory.
IF FILE(lcCurrPath + "SYCINST.DBF")
  lcSysDirect = lcCurrPath
ELSE
  *-- If you run from the root of aria 27
  IF FILE(lcCurrPath + "SYSFILES\" + "SYCINST.DBF")
    lcSysDirect = lcCurrPath  + "SYSFILES\"
  ELSE  && else you run from any where on your disk drives.
    lcSysDirect = GETDIR('','Select System Files Directory')
  ENDIF
ENDIF
*-- Badran 09/30/2001 Fix getting the sysfiles directory [End  ]
*B604754,1 [End]
IF EMPTY(lcSysDirect)
  SET FULLPATH &lcOldPath.
  RETURN
ENDIF
IF FILE(lcSysDirect+'SYCINST.DBF')
  lcRootDir = SUBSTR(lcSysDirect,1,RAT('\',lcSysDirect,2))
  USE (lcSysDirect+'SYCINST.DBF') IN 0 AGAIN ALIAS SYCPATH
  IF TYPE('SYCPATH.CSCRDIR')='C' AND TYPE('SYCPATH.CINSWINPD')='C' AND;
     TYPE('SYCPATH.CINSWINRD')='C' AND TYPE('SYCPATH.CINSWINBM')='C'
    lcCurrDefa = SET("DEFAULT")
    lcError = ON("ERROR")
    llError = .F.
    ON ERROR llError = .T.
    lcPrgDir = ALLT(SYCPATH.CINSWINPD)
    SET DEFAULT TO (lcPrgDir)
    IF llError
      WAIT WINDOW lcSysDirect + " is a wrong Sysfiles Directory" TIMEOUT 2
      SET DEFAULT TO (lcCurrDefa)
      ON ERROR &lcError.
      USE IN SYCPATH
      RETURN
    ENDIF
    lcRepDir = ALLT(SYCPATH.CINSWINRD)
    SET DEFAULT TO (lcRepDir)
    IF llError
      WAIT WINDOW lcSysDirect + " is a wrong Sysfiles Directory" TIMEOUT 2
      SET DEFAULT TO (lcCurrDefa)
      ON ERROR &lcError.
      USE IN SYCPATH
      RETURN
    ENDIF
    lcScrDir = ALLT(SYCPATH.CSCRDIR)
    SET DEFAULT TO (lcScrDir)
    IF llError
      WAIT WINDOW lcSysDirect + " is a wrong Sysfiles Directory" TIMEOUT 2
      SET DEFAULT TO (lcCurrDefa)
      ON ERROR &lcError.
      USE IN SYCPATH
      RETURN
    ENDIF
    lcBmpDir = ALLT(SYCPATH.CINSWINBM)
    SET DEFAULT TO (lcBmpDir)
    IF llError
      WAIT WINDOW lcSysDirect + " is a wrong Sysfiles Directory" TIMEOUT 2
      SET DEFAULT TO (lcCurrDefa)
      ON ERROR &lcError.
      USE IN SYCPATH
      RETURN
    ENDIF
    ON ERROR &lcError.

  ELSE
    lcPrgDir = lcRootDir+'PRGS\'
    lcRepDir = lcRootDir+'REPORTS\'
    lcScrDir = lcRootDir+'SCREENS'
    lcBmpDir = lcRootDir+'BMPS\'
    USE IN SYCPATH
    RETURN
  ENDIF
  USE IN SYCPATH
ELSE
  lcSysDirect = ''
  RETURN
ENDIF
SET DEFAULT TO (lcRootDir)
DIMEN laObjType[13,4]
laObjType[1,1] = "Programs"
laObjType[1,2] = "PRG"
laObjType[1,3] = [lcPrgDir]
laObjType[1,4] = "PROGRAM"

laObjType[2,1] = "System File"
laObjType[2,2] = "DBF"
laObjType[2,3] = [lcSysDirect]
laObjType[2,4] = "SYSTEM"

laObjType[3,1] = "Bit Map & Icons"
laObjType[3,2] = "BMP,ICO"
laObjType[3,3] = [lcBmpDir]
laObjType[3,4] = "BITMAP"

laObjType[4,1] = "Report"
laObjType[4,2] = "FRX"
laObjType[4,3] = [lcRepDir]
laObjType[4,4] = "REPORTS"

laObjType[5,1] = "Label"
laObjType[5,2] = "LBX"
laObjType[5,3] = [lcRepDir]
laObjType[5,4] = "LABEL"

laObjType[6,1] = "Report Program"
laObjType[6,2] = "PRG"
laObjType[6,3] = [lcRepDir]
laObjType[6,4] = "REPORTPRG"

laObjType[7,1] = "Screen"
laObjType[7,2] = "SCX"
laObjType[7,3] = [lcScrDir]
laObjType[7,4] = "SCREENS"

laObjType[8,1] = "Screen Program"
laObjType[8,2] = "SPR"
laObjType[8,3] = [lcScrDir]
laObjType[8,4] = "SCREENPRG"

laObjType[9,1] = "Free File"
laObjType[9,2] = "*"
laObjType[9,3] = [lcRootDir]
laObjType[9,4] = "FREEFILE"

laObjType[10,1] = "Report Screen"
laObjType[10,2] = "SCX"
laObjType[10,3] = [lcRepDir]
laObjType[10,4] = "REPORTFR"

laObjType[11,1] = "Report Screen Program"
laObjType[11,2] = "SPR"
laObjType[11,3] = [lcRepDir]
laObjType[11,4] = "REPORTFP"

laObjType[12,1] = "Application To build"
laObjType[12,2] = "PJX"
laObjType[12,3] = [lcRootDir]
laObjType[12,4] = "XPROJECT"

laObjType[13,1] = "Main Program"
laObjType[13,2] = "PRG"
laObjType[13,3] = [lcRootDir]
laObjType[13,4] = "MAINCUSPRG"

lcSafety = SET('SAFETY')
SET SAFETY OFF
lcEntID = SYS(16)
lcEntID = IIF(ATC('\',lcEntID)>0,SUBSTR(lcEntID,RAT('\',lcEntID)+1),lcEntID)
lcOutType = UPPER(SUBSTR(lcEntID,8,1))
lcEntID = SUBSTR(lcEntID,1,7)
lcObjFile = lcEntID+'O'
USE (lcObjFile) IN 0 ORDER TAG Centry
SELECT (lcObjFile)
SCAN
  DO CASE
    CASE INLIST(cobjecttyp ,'PROGRAM','REPORTPRG','SCREENPRG','REPORTFP',"MAINCUSPRG")
      IF LDONTCOMP
         LOOP
      ENDIF
      lnPrgPos = ASCAN(laObjType,ALLT(&lcObjFile..COBJECTTYP))
      IF lnPrgPos>0
        lnPrgPos= ASUBSC(laObjType,lnPrgPos,1)
      ENDIF
      IF lnPrgPos>0
        IF COBJTYPE $ 'DW'
          lcSourDir = EVAL(laObjType[lnPrgPos,3])+IIF(EMPTY(&lcObjFile..CSOURCE),'',ALLT(&lcObjFile..CSOURCE)+'\')+ALLT(&lcObjFile..COBJNAME)
        ELSE
          lcSourDir = LEFT(EVAL(laObjType[lnPrgPos,3]),1)+SUBSTR(ALLT(CSOURCE),2)+ALLT(&lcObjFile..COBJNAME)
        ENDIF
        lcSourDir = IIF(lcOutType='C',STRTRAN(STRTRAN(lcSourDir,'.PRG','.FXP'),'.SPR','.SPX'),lcSourDir)
        IF &lcObjFile..CPROSSTYPE = 'D' OR CSOURCE <> CDISTEN
          IF COBJTYPE $ 'DW'
            lcDestDir = EVAL(laObjType[lnPrgPos,3])+IIF(EMPTY(&lcObjFile..CDISTEN),'',ALLT(&lcObjFile..CDISTEN)+'\')+ALLT(&lcObjFile..COBJNAME)
          ELSE
             lcDestDir = LEFT(EVAL(laObjType[lnPrgPos,3]),1)+SUBSTR(ALLT(CDISTEN),2)+ALLT(&lcObjFile..COBJNAME)
          ENDIF
           lcDestDir = IIF(lcOutType='C',STRTRAN(STRTRAN(lcDestDir ,'.PRG','.FXP'),'.SPR','.SPX'),lcDestDir)
          ERASE (lcDestDir)
        ENDIF
        IF &lcObjFile..CPROSSTYPE <> 'D'
          *-- Badran at 12/18/2001 Handle copying errors [Begin]
          *COPY MEMO &lcObjFile..mcontent TO (lcSourDir)
          lcComnd = [COPY MEMO ] + lcObjFile + [.mcontent TO ] + lcSourDir
          =lfCrtFile(lcComnd,"copying ",lcSourDir)
          *-- Badran at 12/18/2001 Handle copying errors [End  ]
        ENDIF
      ENDIF
    CASE INLIST(cobjecttyp ,'REPORTS','LABEL','SCREENS','REPORTFR')
      IF lcOutType='C' AND ALLT(cobjecttyp) $ 'SCREENS,REPORTFR'
        LOOP
      ENDIF
      lnPrgPos = ASCAN(laObjType,ALLT(&lcObjFile..COBJECTTYP))
      IF lnPrgPos>0
        lnPrgPos= ASUBSC(laObjType,lnPrgPos,1)
      ENDIF
      IF lnPrgPos>0
        IF COBJTYPE $ 'DW'
          lcSourDir = EVAL(laObjType[lnPrgPos,3])+IIF(EMPTY(&lcObjFile..CSOURCE),'',ALLT(&lcObjFile..CSOURCE)+'\')+ALLT(&lcObjFile..COBJNAME)
        ELSE
          lcSourDir = LEFT(EVAL(laObjType[lnPrgPos,3]),1)+SUBSTR(ALLT(CSOURCE),2)+ALLT(&lcObjFile..COBJNAME)
        ENDIF
        IF &lcObjFile..CPROSSTYPE = 'D' OR CSOURCE <> CDISTEN
          IF COBJTYPE $ 'DW'
            lcDestDir = EVAL(laObjType[lnPrgPos,3])+IIF(EMPTY(&lcObjFile..CDISTEN),'',ALLT(&lcObjFile..CDISTEN)+'\')+ALLT(&lcObjFile..COBJNAME)
          ELSE
             lcDestDir = LEFT(EVAL(laObjType[lnPrgPos,3]),1)+SUBSTR(ALLT(CDISTEN),2)+ALLT(&lcObjFile..COBJNAME)
          ENDIF
          ERASE (lcDestDir )
          ERASE (SUBSTR(lcDestDir ,1,LEN(lcDestDir)-1)+'T')
        ENDIF
        IF &lcObjFile..CPROSSTYPE <>'D'
          lcSorce = ALLT(&lcObjFile..MCONTENT)
          lcTargetDir = lcSourDir
          *-- Badran at 12/18/2001 Handle copying errors [Begin]
          *SELECT * FROM (lcSorce+'.'+laObjType[lnPrgPos,2]) INTO DBF (lcTargetDir)
          *USE
          lcComnd = [SELECT * FROM ] + lcSorce+'.'+laObjType[lnPrgPos,2] + [ INTO DBF ] + lcTargetDir
          llCrt =lfCrtFile(lcComnd,"copying ",lcTargetDir)
          IF llCrt
            USE
          ENDIF
          *-- Badran at 12/18/2001 Handle copying errors [End  ]
          IF USED(lcSorce)
            USE IN (lcSorce)
          ENDIF
        ENDIF
      ENDIF

    CASE cobjecttyp = 'SYSTEM'
      lcSource = ALLT(&lcObjFile..MCONTENT)
      lcTarget = STRTRAN(ALLT(&lcObjFile..COBJNAME),'.DBF')
      lcTag = ALLT(&lcObjFile..Ctag)
      USE (lcSource+'.DBF') IN 0 ORDER TAG &lcTag
      SELECT (lcSource)
      lcKeyExpr = KEY()
      lcKey = STRTRAN(KEY(),"ALLTRIM")
      lnKeyLen = LEN(EVAL(lcKey))
      lcValue = PADR(&lcObjFile..CKEY,lnKeyLen)
      USE (lcSysDirect+lcTarget+'.DBF') IN 0 AGAIN ALIAS SYSFILE
      SELECT SYSFILE
      lcOnError = ON('ERROR')
      llError = .F.
      ON ERROR llError = .T.
      SET ORDER TO TAG &lcTag
      ON ERROR &lcOnError
      IF llError
        WAIT WINDOW "TAG "+lcTag+' does not exist in file'+lcSysDirect+lcTarget+'.DBF'
        CLOSE DATA
        RETURN
      ENDIF
      IF KEY()<>lcKeyExpr &&OR LEN(EVAL(KEY()))<>lnKeyLen
        WAIT WINDOW "TAG "+lcTag+' does not exist in file'+lcSysDirect+lcTarget+'.DBF'
        CLOSE DATA
        RETURN
      ENDIF

      *-- BADRAN 07/05/2001 Add Information about entries [Begin]
      IF &lcObjFile..COBJNAME = "SYDFLFLD.DBF"
        =AddTecInfo("SYDFLFLD",&lcSource..cFile_Nam)
      ENDIF
      IF &lcObjFile..COBJNAME = "SYDINDEX.DBF"
        =AddTecInfo("SYDINDEX",&lcSource..cFile_Nam)
      ENDIF
      IF &lcObjFile..COBJNAME = "SYFRMCDD.DBF"
         USE (lcSysDirect+"SYFRMCDH.DBF") AGAIN ALIAS FORMHDR IN 0 ORDER Formcdhd SHARED
         IF SEEK(&lcSource..cFormMaj,"FORMHDR")
           lcFormDes = ALLTRIM(FORMHDR.cFormMjDes) + "|" + ALLTRIM(&lcSource..cFormCdDes)
           =AddTecInfo("SYFRMCDD",lcFormDes)
         ENDIF
         USE IN FORMHDR
      ENDIF
      IF &lcObjFile..COBJNAME = "SYCTRIGG.DBF"
        =AddTecInfo("SYCTRIGG",'You should zap the static file "SyuStatc" before running.')
      ENDIF
      *-- BADRAN 07/05/2001 Add Information about entries [End  ]

      IF &lcObjFile..COBJNAME = "SYCMENU.DBF"
        IF EMPTY(lcOrgMnu)
          USE (lcSysDirect+lcTarget+'.DBF') IN 0 AGAIN ALIAS SYSMENU
          SELECT SYSMENU
          SET ORDER TO TAG &lcTag
          lcTarMnu = gfTempName()
          lcOrgMnu = gfTempName()
          lcTmpMnu = gfTempName()
        ENDIF
        lnRecNo = 0
        SELECT *,RECNO() AS NPHISLOC FROM (lcSOURCE);
              WHERE &lcKey = lcValue;
              INTO DBF (lcTmpMnu)
        IF !USED(lcOrgMnu)
          COPY STRUCTURE TO (lcOrgMnu)
          USE (lcOrgMnu) IN 0 EXCLUSIVE
        ENDIF

        IF !USED(lcTarMnu)
          SELECT SYSFILE
          COPY STRUCTURE TO (lcTarMnu) WITH CDX
          USE (lcTarMnu) IN 0 ORDER Apppopbar EXCLUSIVE
        ENDIF

        SELECT (lcTmpMnu)
        SCAN
          REPLACE cOwner WITH "",;
                  NPHISLOC WITH 0
          lcMnuKeyVl = cApp_Id + CPAD_POS
          IF !SEEK(lcMnuKeyVl, lcTarMnu)
            SELECT (lcTarMnu)
            APPEND FROM FULLPATH(DBF("SYSFILE")) FOR cApp_Id + CPAD_POS = lcMnuKeyVl
            REPLACE ALL cOwner WITH "O" FOR cApp_Id + CPAD_POS = lcMnuKeyVl
          ENDIF
        ENDSCAN

        SELECT(lcOrgMnu)
        APPEND FROM (lcTmpMnu)

        *-- B605846,1 04/10/2002 Fill the laMnuInsrt array with sub menu if any [Start]
        IF CSUB_TYP = 'S'
          IF !EMPTY(laMnuInsrt[1])
            DECLARE laMnuInsrt[ALEN(laMnuInsrt)+1]
          ENDIF
          laMnuInsrt[ALEN(laMnuInsrt)] = CPROSS_ID
        ENDIF
        *-- B605846,1 04/10/2002 Fill the laMnuInsrt array with sub menu if any [End]

        USE IN (lcTmpMnu)
        ERASE (lcTmpMnu+'.DBF')
        ERASE (lcTmpMnu+'.FPT')
        IF USED(lcSource)
          USE IN (lcSource)
        ENDIF
      ELSE
        =SEEK(lcValue,'SYSFILE')
        DELETE WHILE &lcKey = lcValue &&FOR !SEEK(&lcKey,lcSource)
        SELECT (lcSource)
        GO TOP
        SCAN
          SCATT MEMVAR MEMO
          INSERT INTO SYSFILE FROM MEMVAR
       ENDSCAN
     ENDIF
     IF USED('SYSFILE')
       USE IN SYSFILE
     ENDIF
     IF USED(lcSource)
       USE IN (lcSource)
     ENDIF

    CASE ALLT(cobjecttyp) = 'XPROJECT'
      lcFileName = ALLT(&lcObjFile..COBJNAME)
      lcFileName = STRTRAN(lcFileName,'.PJX',IIF("AAS_WIN" $ lcFileName,'.EXE','.APP'))
       IF "AAS_WIN" $ lcFileName
          lcSourDir = lcRootDir+lcFileName
       ELSE
          lcSourDir = lcPrgDir+lcFileName
       ENDIF
        IF &lcObjFile..CPROSSTYPE <>'D'
          *-- Badran at 12/18/2001 Handle copying errors [Begin]
          *COPY MEMO &lcObjFile..mcontent TO (lcSourDir)
          lcComnd = [COPY MEMO ] + lcObjFile + [.mcontent TO ] + lcSourDir
          =lfCrtFile(lcComnd,"copying ",lcSourDir)
          *-- Badran at 12/18/2001 Handle copying errors [End  ]
        ENDIF
    CASE INLIST(ALLT(cobjecttyp),'BITMAP','FREEFILE')
      lnPrgPos = ASCAN(laObjType,ALLT(&lcObjFile..COBJECTTYP))
      IF lnPrgPos>0
        lnPrgPos= ASUBSC(laObjType,lnPrgPos,1)
      ENDIF
      IF lnPrgPos>0
        IF COBJTYPE $ 'DW'
            lcSourDir = EVAL(laObjType[lnPrgPos,3])+IIF(EMPTY(&lcObjFile..CSOURCE),'',ALLT(&lcObjFile..CSOURCE)+'\')+ALLT(&lcObjFile..COBJNAME)
        ELSE
          Loop
        ENDIF
        IF &lcObjFile..CPROSSTYPE = 'D' OR CSOURCE <> CDISTEN
          IF COBJTYPE $ 'DW'
            lcDestDir = EVAL(laObjType[lnPrgPos,3])+IIF(EMPTY(&lcObjFile..CDISTEN),'',ALLT(&lcObjFile..CDISTEN)+'\')+ALLT(&lcObjFile..COBJNAME)
          ELSE
             lcDestDir = LEFT(EVAL(laObjType[lnPrgPos,3]),1)+SUBSTR(ALLT(CDISTEN),2)+ALLT(&lcObjFile..COBJNAME)
          ENDIF
          ERASE (lcDestDir)
        ENDIF
        IF &lcObjFile..CPROSSTYPE <>'D'
          IF "AAS_WIN" $ ALLT(&lcObjFile..COBJNAME)
            COPY MEMO &lcObjFile..mcontent TO (STRTRAN(lcSourDir,'.PJX','.EXE'))
          ELSE
            COPY MEMO &lcObjFile..mcontent TO (STRTRAN(lcSourDir,'.PJX','.APP'))
          ENDIF
        ENDIF
      ENDIF
  ENDCASE
ENDSCAN
*-- BADRAN 07/01/2001 The Menu Handling code done once [Begin]
IF TYPE("lcOrgMnu") = "C" AND USED(lcOrgMnu)
  SELECT (lcOrgMnu)
  SET RELATION TO &lcOrgMnu..CAPP_ID + &lcOrgMnu..CPAD_POS INTO &lcTarMnu  ADDI
  LOCATE
  llMenuUpd = .F.
  PUSH KEY
  ON KEY LABEL ESC llEscDummy = .T.
  DO SUMENU.SPX
  POP KEY
  IF USED(lcOrgMnu)
    USE IN (lcOrgMnu)
  ENDIF
  ERASE (lcOrgMnu+'.DBF')
  ERASE (lcOrgMnu+'.FPT')
  IF USED(lcTarMnu)
    USE IN (lcTarMnu)
  ENDIF
  ERASE (lcTarMnu+'.DBF')
  ERASE (lcTarMnu+'.FPT')
ENDIF
*-- BADRAN 07/01/2001 The Menu Handling code done once [End  ]
SET DEFA TO (lcStartDir)
SET SAFETY &lcSafety
IF USED(lcObjFile)
  USE IN (lcObjFile)
ENDIF
IF USED("SYSFILE")
  USE IN SYSFILE
ENDIF
IF USED("SYSMENU")
  USE IN SYSMENU
ENDIF
IF TYPE("lcTarget")="C" AND USED(lcTarget) AND LEFT(lcTarget,1) = "X"
  lcTarFile = STRTRAN(FULLPATH(DBF(lcTarget)),".DBF")
  USE IN (lcTarget)
  ERASE (lcTarFile+".DBF")
  ERASE (lcTarFile+".CDX")
  ERASE (lcTarFile+".FPT")
ENDIF
IF USED(lcTarMnu) AND LEFT(lcTarMnu,1) = "X"
  lcTarFile = STRTRAN(FULLPATH(DBF(lcTarMnu)),".DBF")
  USE IN (lcTarMnu)
  ERASE (lcTarFile+".DBF")
  ERASE (lcTarFile+".CDX")
  ERASE (lcTarFile+".FPT")
ENDIF
*-- BADRAN 07/01/2001 if used SYDAPPL close it [Begin]
IF llOpenAppl
  USE IN SYDAPPL
ENDIF
=lfNotePad()
*-- Badran 10/16/2001 (Set of Path should be the last line in code.) [Begin]
SET FULLPATH &lcOldPath.
*-- Badran 10/16/2001 (Set of Path should be the last line in code.) [End  ]
*-- BADRAN 07/01/2001 if used SYDAPPL close it [End  ]

FUNCTION gfTempName
RETURN 'X'+SUBSTR(SYS(2015),4)
FUNCTION lfvOkSys
SELECT (lcOrgMnu)
lnActivRec = RECNO()
LOCATE FOR EMPTY(cOwner)
IF FOUND()
*-- BADRAN 07/01/2001 Give the user the posiblity to either continue or break the updating [Begin]
*  WAIT "You have to select a corresponding record for each record updated in sycmenu file" WINDOW
*  RETURN
  LOCATE FOR !EMPTY(cOwner)
  IF FOUND()
    lcMessage  = "Some of the new menu bar(s) are not inserted." +;
                 CHR(13) + "Are you sure you want to continue ?"
    lnMessType = 4
  ELSE
    lcMessage  = "You Should insert at least one of the new menu bar(s) in order to continue."
    lnMessType = 0
  ENDIF
  lnReturn = 0
  GO lnActivRec
  SHOW WINDOW New REFRESH SAME
  SHOW WINDOW Customer REFRESH SAME
  DO SUEXEMSG.SPX
  IF lnReturn = 0
    RETURN
  ENDIF
ENDIF
GO lnActivRec
SHOW WINDOW New REFRESH SAME
SHOW WINDOW Customer REFRESH SAME
*-- BADRAN 07/01/2001 Give the user the posiblity to either continue or break the updating [End  ]
DECLARE laUnique[1]
laUnique = ""
SELECT (lcTarMnu)
PACK
SELECT (lcOrgMnu)
PACK
LOCATE
* SCAN       && Badran 07/01/2001 Update the Added menu bars only.
SCAN FOR !EMPTY(cOwner)
  lcKeyValue = cApp_ID + cPad_Pos
  IF ASCAN(laUnique,lcKeyValue) = 0
    IF !EMPTY(laUnique)
      DECLARE laUnique[ALEN(laUnique) + 1]
    ENDIF
    laUnique[ALEN(laUnique)] = lcKeyValue
    SELECT SYSMENU
    DELETE FOR capp_id+cpad_pos+cpop_pos+cpop_levl+cbar_pos = lcKeyValue
    SELECT (lcTarMnu)
    SCAN FOR capp_id+cpad_pos+cpop_pos+cpop_levl+cbar_pos = lcKeyValue
      SCATTER MEMVAR MEMO
      m.cOwner = ""
      INSERT INTO SYSMENU FROM MEMVAR
    ENDSCAN
  ENDIF
ENDSCAN

*-- Write a menu Log Information [Begin]
=AddTecInfo("SYCMENU","")
*-- Write a menu Log Information [End  ]
llMenuUpd = .T.
CLEAR READ

FUNCTION lfwRdSys
*lcFieldBr = "CPAD_POS,CPOP_POS,CBAR_POS,CSUB_CTG,CSUB_TYP,CSUB_PRPT,CPROSS_ID,CPROCTYPE"
lcFieldBr  = "cPrompt = SPACE(VAL(ALLTRIM(CPOP_LEVL)) * 8) + CSUB_PRPT :H='Bar Prompt' :R :70"+;
             ",cBar = IIF(cOwner='O','   ','New') :H='Status'"

SELECT (lcTarMnu)
BROWSE FIELDS X=IIF(RECNO() = lnRecNo,'>',' ') :H="" ,&lcFieldBr;
       FREEZE X;
        WINDOW SUMENU3;
       IN WINDOW SUMENU ;
           WHEN lfwTarMnu() ;
           LOCK 0;
           NOAPPEND;
           NOCLEAR;
           NODELETE;
           NOWAIT;
           NOEDIT;
           SAVE;
           NOMENU;
           TITLE "Customer Menu Bars"

*-- B605846,1 04/10/2002 indent bar prompt as its level [Start]
*lcFieldBr  = "CSUB_PRPT :H='Bar Prompt' :R :50"
lcFieldBr  = "cPrompt = SPACE(VAL(ALLTRIM(CPOP_LEVL)) * 8) + CSUB_PRPT :H='Bar Prompt' :R :70"
*-- B605846,1 04/10/2002 indent bar prompt as its level [End]

*-- BADRAN 07/01/2001 Open SYDAPPL to check the existing of modules on it. [Begin]
IF USED("SYDAPPL")
  SET ORDER TO Capp_id IN SYDAPPL
ELSE
  llOpenAppl = .T.
  USE (lcSysDirect+"SYDAPPL") ORDER Capp_id IN 0 SHARED
ENDIF
*-- BADRAN 07/01/2001 Open SYDAPPL to check the existing of modules on it. [End  ]
SELECT (lcOrgMnu)
*-- BADRAN 07/01/2001 Add the SydAppl For condition to the Browse [Begin]
*-- i.e. Line # 3 in the Browse was added to prompt the user with his applications only.
BROWSE FIELDS CAPP_ID,&lcFieldBr WINDOW SUMENU2;
       IN WINDOW SUMENU ;
           FOR SEEK(CAPP_ID,"SYDAPPL") ;
           WHEN lfwOrgMnu();
           LOCK 0;
           NOAPPEND;
           NOCLEAR;
           NODELETE;
           NOWAIT;
           NOEDIT;
           SAVE;
           NOMENU;
           TITLE "New Menu Bar(s)"
*-- BADRAN 07/01/2001 Add the SydAppl For condition to the Browse [End  ]

FUNCTION lfwTarMnu

*-- B605846,1 04/10/2002 Disable the insert button for the sub menu child [Start]
*lcInsPrpt = IIF(EOF(lcOrgMnu) OR (cSub_Typ = "P") OR (&lcOrgMnu..cOwner = "A"),"DISABLE","ENABLE")
lcInsPrpt = IIF(EOF(lcOrgMnu) OR (cSub_Typ = "P") OR (EVALUATE(lcOrgMnu+'.cOwner') = "A");
                OR (ASCAN(laMnuInsrt,EVALUATE(lcOrgMnu+'.CMSTR_NAM')) # 0),"DISABLE","ENABLE")
*-- B605846,1 04/10/2002 Disable the insert button for the sub menu child [End]

SHOW GET pbInsert &lcInsPrpt
IF cSub_Typ = "P"
  REPLACE &lcOrgMnu..NPHISLOC WITH 0
ELSE
  REPLACE &lcOrgMnu..NPHISLOC WITH RECNO(lcTarMnu)
ENDIF

*-- B605846,1 04/15/2002 Disable the remove button for sub menu child [Start]
*IF &lcTarMnu..cOwner = "O"
IF (EVALUATE(lcTarMnu+'.cOwner') = "O") .OR. (ASCAN(laMnuRemve,EVALUATE(lcTarMnu+'.CMSTR_NAM')) # 0)
*-- B605846,1 04/15/2002 Disable the remove button for sub menu child [End]

  STORE "DISABLE" TO lcDelPrpt
ELSE
  STORE "ENABLE" TO lcDelPrpt
ENDIF
SHOW GET pbDelete &lcDelPrpt
*-- end of lfwTarMnu.

FUNCTION lfwOrgMnu

*-- B605846,1 04/10/2002 Disable the insert button for the sub menu child [Start]
lcInsPrpt = IIF((EVALUATE(lcTarMnu+'.cSub_Typ') = "P") OR (cOwner = "A");
                OR (ASCAN(laMnuInsrt,CMSTR_NAM) # 0),"DISABLE","ENABLE")
SHOW GET pbInsert &lcInsPrpt
*-- B605846,1 04/10/2002 Disable the insert button for the sub menu child [End]

lnRecNo = NPHISLOC
IF lnRecNo > 0
  GO lnRecNo IN (lcTarMnu)
ENDIF
SHOW WINDOW Customer REFRESH SAME
FUNCTION lfvSelSys
SELECT (lcOrgMnu)
lnRecNo = IIF(EMPTY(&lcTarMnu..CPOP_POS),0,RECNO(lcTarMnu))
REPLACE NPHISLOC WITH lnRecNo
SHOW WINDOW Customer REFRESH SAME
FUNCTION lfvCnSys
*-- BADRAN 07/02/2001 Check before cancel.
lnReturn   = 0
lcMessage  = "Your menu bars are not yet updated with the new menu bar(s)." +;
             CHR(13) + "Are you sure you want to cancel ?"
lnMessType = 4
DO SUEXEMSG.SPX
IF lnReturn = 0
  RETURN
ENDIF
llMenuUpd = .F.
CLEAR READ

*!*************************************************************
*! Name      : lfvInsert
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/02/2001
*! Purpose   : Insert New menu bar.
*!*************************************************************
*!
*E301602,1
FUNCTION lfvInsert
PRIVATE lnActAlias, lcKeyVal, lcPopLevel, llDummy, ;
        lcPopValue, lcBarValue, lcSubValue, lcActivKey
lnActAlias = SELECT(0)

SELECT (lcTarMnu)
lcActivKey = EVALUATE(KEY())
lcPopLevel = cPop_levl
lcKeyVal   = cApp_ID + cPad_Pos

SELECT (lcOrgMnu)

*-- B605846,1 04/10/2002 Handling the case of sub menu [Start]

*-- comment the next replace command to avoid changing the popup name in case of sub menu(s).
*-- The correct popup name will be saved after calling the lfGetSubMn function.
*-- cowner = 'B' to mark this record as (BEFORE INSERTED) record.
*REPLACE cOwner     WITH "A" ,;
*        cPop_Pos   WITH &lcTarMnu..cPop_Pos  ,;
*        cBar_Pos   WITH &lcTarMnu..cBar_Pos  ,;
*        cPop_levl  WITH &lcTarMnu..cPop_levl ,;
*        cmStr_Nam  WITH &lcTarMnu..cmStr_Nam ,;
*        cSub_Pos   WITH &lcTarMnu..cSub_Pos
REPLACE cOwner     WITH "B" ,;
        cPop_Pos   WITH &lcTarMnu..cPop_Pos  ,;
        cBar_Pos   WITH &lcTarMnu..cBar_Pos  ,;
        cPop_levl  WITH &lcTarMnu..cPop_levl ,;
        cmStr_Nam  WITH IIF(CSUB_TYP='S',CMSTR_NAM,&lcTarMnu..cmStr_Nam) ,;
        cSub_Pos   WITH &lcTarMnu..cSub_Pos

*-- lnElmentCnt = The number of sycmenu record(s) will be inserted.
*-- lnSubMnu    = The number of sub menu(s) will be inserted.
*-- lcPopupID   = The popup id of inserted sub menu.
PRIVATE lnElmntCnt, lnSubMnu, lcPopupID

*-- if sub menu
IF CSUB_TYP = 'S'
  *-- Check if there are sub-menu(s) before this sub-menu [Begin]
  SELECT (lcTarMnu)
  SEEK lcActivKey
  lcPopupID = ALLTRIM(CMSTR_NAM)+'01'  && initial value of popup name.
  SET ORDER TO Apppopbar DESCENDING    && descending the order to search for sub-menu(s) before.
  LOCATE REST FOR capp_id+cpad_pos+cpop_pos+cpop_levl+cbar_pos # lcActivKey .AND. CSUB_TYP = 'S'
  IF FOUND()  && There are sub-menu(s)
    lcPopupID = PADR(ALLTRIM(CMSTR_NAM),7)+PADL(ALLTRIM(STR(VAL(RIGHT(ALLTRIM(CPROSS_ID),2))+1,2,0)),2,'0')
  ENDIF
  SET ORDER TO Apppopbar ASCENDING
  SEEK lcActivKey
  *-- Check if there are sub-menu(s) before this sub-menu [End  ]

  SELECT (lcOrgMnu)
  lnSubMnu   = 1
  lnElmntCnt = 1
  =lfGetSubMn(lcPopupID)    && mark the sub menu child as inserted (cowner='B')

  *-- Assure that we are in the true record (May the relation was breaked.)
  SELECT (lcTarMnu)
  SEEK lcActivKey

  SELECT (lcOrgMnu)
  REPLACE CMSTR_NAM WITH EVALUATE(lcTarMnu+'.CMSTR_NAM')  && save the correct popup name
*-- not sub menu
ELSE
  lnSubMnu   = 1
  lnElmntCnt = 1
ENDIF
*-- B605846,1 04/10/2002 Handling the case of sub menu [End]

SHOW WINDOW New REFRESH SAME

lcTmpTargt = gfTempName()
SELECT * FROM (lcTarMnu) ;
  WHERE capp_id+cpad_pos+cpop_pos+cpop_levl+cbar_pos = lcKeyVal ;
  ORDER BY capp_id,cpad_pos,cpop_pos,cpop_levl,cbar_pos         ;
  INTO TABLE (lcTmpTargt)

SELECT (lcTmpTargt)

*B605846,1 Rearrange target menu records before adding the new record(s) [Begin]
LOCATE FOR capp_id+cpad_pos+cpop_pos+cpop_levl+cbar_pos = lcActivKey
SCAN REST WHILE cApp_ID + cPad_Pos = lcKeyVal

  *-- B605846,1 04/10/2002 Increment cPop_Pos by the number of menu elements added [Start]
  *lcPopValue = PADL(ALLTRIM(STR(INT(VAL(cPop_Pos)) + 1)),2,"0")
  lcPopValue = PADL(ALLTRIM(STR(INT(VAL(cPop_Pos)) + lnElmntCnt)),2,"0")
  *-- B605846,1 04/10/2002 Increment cPop_Pos by the number of menu elements added [End]

  lcSubValue = PADL(ALLTRIM(STR(INT(VAL(cSub_Pos)) + 1)),2,"0")
  REPLACE cPop_Pos WITH lcPopValue,;
          cSub_Pos WITH lcSubValue

  *-- B605846,1 04/10/2002 Increment the popup name of submenu & child(s) [Start]
  IF EVALUATE(lcOrgMnu+'.CSUB_TYP') = 'S'
    *-- If sub-menu
    IF CSUB_TYP = 'S'
      REPLACE CPROSS_ID WITH PADR(ALLTRIM(CMSTR_NAM),7)+PADL(ALLTRIM(STR(VAL(RIGHT(CPROSS_ID,2))+lnSubMnu,2,0)),2,'0')
    ENDIF

    *-- If this record parent is not the main menu pad,
    IF VAL(CPOP_LEVL) > 1 .AND. CMSTR_NAM > EVALUATE(lcOrgMnu+'.CMSTR_NAM')
      REPLACE CMSTR_NAM WITH PADR(ALLTRIM(CMSTR_NAM),7)+PADL(ALLTRIM(STR(VAL(RIGHT(CMSTR_NAM,2))+lnSubMnu,2,0)),2,'0')
    ENDIF

  ENDIF
  *-- B605846,1 04/10/2002 Increment the popup name of submenu & child(s) [End]

  *-- Increment only if it is in the same pop level

  *-- B605846,1 04/14/2002 Increment only if it is in the same pop level
  *--                        and it is a child of the same parent [Start]
  *IF cPop_levl = lcPopLevel
  IF cPop_levl = lcPopLevel .AND. CMSTR_NAM = EVALUATE(lcOrgMnu+'.CMSTR_NAM')
  *-- B605846,1 04/14/2002 Increment only if it is in the same pop level
  *--                        and it is a child of the same parent [End]

    lcBarValue = PADL(ALLTRIM(STR(INT(VAL(cBar_Pos)) + 1)),2,"0")
    REPLACE cBar_Pos WITH lcBarValue
  ENDIF
ENDSCAN
*B605846,1 Rearrange target menu records before adding the new record(s) [End  ]

SELECT (lcOrgMnu)

*-- B605846,1 04/10/2002 insert all marked record as inserted (cowner='B') [Start]
*SCATTER MEMVAR MEMO
*m.cOwner    = "N"
*INSERT INTO (lcTmpTargt) FROM MEMVAR
SCAN FOR COWNER = 'B'
  REPLACE COWNER WITH 'A'
  SCATTER MEMVAR MEMO
  m.cOwner    = "N"
  INSERT INTO (lcTmpTargt) FROM MEMVAR
  *-- Fill the laMnuRemve array with sub menu to privent its child to be apple to remove
  IF CSUB_TYP = 'S'
    IF !EMPTY(laMnuRemve[1])
      DECLARE laMnuRemve[ALEN(laMnuRemve)+1]
    ENDIF
    laMnuRemve[ALEN(laMnuRemve)] = CPROSS_ID
  ENDIF
ENDSCAN
LOCATE
*-- B605846,1 04/10/2002 insert all marked record as inserted (cowner='B') [End]

SELECT (lcTarMnu)
BLANK FOR capp_id+cpad_pos+cpop_pos+cpop_levl+cbar_pos = lcKeyVal
DELETE FOR EMPTY(capp_id)

SELECT (lcTmpTargt)
SCAN
  SCATTER MEMVAR MEMO
  INSERT INTO (lcTarMnu) FROM MEMVAR
ENDSCAN
llDummy = SEEK(lcActivKey,lcTarMnu)

IF USED(lcTmpTargt)
  lcTmpFile = STRTRAN(FULLPATH(DBF(lcTmpTargt)),".DBF")
  USE IN (lcTmpTargt)
  ERASE (lcTmpFile+".DBF")
  ERASE (lcTmpFile+".FPT")
ENDIF

SHOW WINDOW Customer REFRESH SAME

=lfwTarMnu()
SELECT (lnActAlias)
*-- end of lfvInsert.

*!*************************************************************
*! Name      : lfvDelete
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/02/2001
*! Purpose   : Remove the New menu bar.
*!*************************************************************
*!
*E301602,1
FUNCTION lfvDelete
PRIVATE lnActAlias, lcKeyVal, lcPopLevel, llDummy, ;
        lcPopValue, lcBarValue, lcSubValue, lcActivKey
lnActAlias = SELECT(0)

SELECT (lcTarMnu)
lcActivKey = EVALUATE(KEY())

lcPopLevel = cPop_levl
lcKeyVal   = cApp_ID + cPad_Pos

SELECT (lcOrgMnu)

*-- B605846,1 04/15/2002 Handling remove sub menu [Start]
*REPLACE cOwner     WITH ""
LOCATE FOR capp_id+cpad_pos+cpop_pos+cpop_levl+cbar_pos = lcActivKey

*-- lnElmentCnt = the number of sycmenu record will be remove
*-- lnSubMnu    = the number of sub menu will be remove
PRIVATE lnElmntCnt,lnSubMnu
lnElmntCnt = 1
lnSubMnu   = 0

*-- if sub menu
IF CSUB_TYP = 'S'
  lnSubMnu = 1
  =lfDelSubMn()
*-- if not sub menu
ELSE
  REPLACE cOwner     WITH ""
ENDIF
*-- B605846,1 04/15/2002 Handling remove sub menu [End]

SHOW WINDOW New REFRESH SAME

lcTmpTargt = gfTempName()

*-- B605846,1 04/15/2002 Shifting the target menu elements according to removed elements [Start]
*SELECT capp_id, cpad_pos, cpop_levl,;
*  IIF(cpop_levl = lcPopLevel, PADL(ALLTRIM(STR(INT(VAL(cBar_Pos)) - 1)),2,"0"),cbar_pos) AS cbar_pos,;
*  PADL(ALLTRIM(STR(INT(VAL(cPop_Pos)) - 1)),2,"0") AS cPop_Pos,;
*  PADL(ALLTRIM(STR(INT(VAL(cSub_Pos)) - 1)),2,"0") AS csub_pos,;
*  csub_ctg, cbarmodule, csub_typ,;
*  csub_prpt, cmstr_nam, cpross_id,;
*  cproctype, csub_hkey, csub_msg,;
*  cprocpath, ldefault, cupgrdlvl,;
*  cmenuparam, chlptopic, cadd_user,;
*  cadd_time, dadd_date, llok_stat,;
*  clok_user, dlok_date, clok_time, cowner;
* FROM (lcTarMnu);
* WHERE capp_id+cpad_pos+cpop_pos+cpop_levl+cbar_pos > lcActivKey AND ;
*   capp_id + cpad_pos = lcKeyVal ;
* ORDER BY capp_id, cpad_pos, cpop_levl, cbar_pos ;
* INTO TABLE (lcTmpTargt)
SELECT capp_id, cpad_pos, cpop_levl,;
  IIF(cpop_levl = lcPopLevel .AND. CMSTR_NAM = EVALUATE(lcOrgMnu+'.CMSTR_NAM'),;
      PADL(ALLTRIM(STR(INT(VAL(cBar_Pos)) - 1)),2,"0"),cbar_pos) AS cbar_pos,;
  PADL(ALLTRIM(STR(INT(VAL(cPop_Pos)) - lnElmntCnt)),2,"0") AS cPop_Pos,;
  PADL(ALLTRIM(STR(INT(VAL(cSub_Pos)) - 1)),2,"0") AS csub_pos,;
  csub_ctg, cbarmodule, csub_typ, csub_prpt,;
  IIF(EVALUATE(lcOrgMnu+'.CSUB_TYP') = 'S' .AND. VAL(CPOP_LEVL) > 1 .AND.;
      CMSTR_NAM > EVALUATE(lcOrgMnu+'.CMSTR_NAM'),;
      PADR(ALLTRIM(CMSTR_NAM),7)+PADL(ALLTRIM(STR(VAL(RIGHT(CMSTR_NAM,2))-lnSubMnu,2,0)),2,'0'),;
      CMSTR_NAM) AS cmstr_nam,;
  IIF(EVALUATE(lcOrgMnu+'.CSUB_TYP') = 'S' .AND. CSUB_TYP = 'S',;
      PADR(ALLTRIM(CMSTR_NAM),7)+PADL(ALLTRIM(STR(VAL(RIGHT(CPROSS_ID,2))-lnSubMnu,2,0)),2,'0'),;
      CPROSS_ID) AS cpross_id,;
  cproctype, csub_hkey, csub_msg,;
  cprocpath, ldefault, cupgrdlvl,;
  cmenuparam, chlptopic, cadd_user,;
  cadd_time, dadd_date, llok_stat,;
  clok_user, dlok_date, clok_time, cowner;
 FROM (lcTarMnu);
 WHERE capp_id+cpad_pos+cpop_pos+cpop_levl+cbar_pos > lcActivKey AND ;
   capp_id + cpad_pos = lcKeyVal ;
 ORDER BY capp_id, cpad_pos, cpop_levl, cbar_pos ;
 INTO TABLE (lcTmpTargt)
*-- B605846,1 04/15/2002 Shifting the target menu elements according to removed elements [End]

SELECT (lcTarMnu)
BLANK FOR capp_id+cpad_pos+cpop_pos+cpop_levl+cbar_pos >= lcActivKey AND ;
          capp_id + cpad_pos = lcKeyVal
DELETE FOR EMPTY(capp_id)

SELECT (lcTmpTargt)
SCAN
  SCATTER MEMVAR MEMO
  INSERT INTO (lcTarMnu) FROM MEMVAR
ENDSCAN
llDummy = SEEK(lcActivKey,lcTarMnu)

IF USED(lcTmpTargt)
  lcTmpFile = STRTRAN(FULLPATH(DBF(lcTmpTargt)),".DBF")
  USE IN (lcTmpTargt)
  ERASE (lcTmpFile+".DBF")
  ERASE (lcTmpFile+".FPT")
ENDIF

SHOW WINDOW Customer REFRESH SAME

=lfwTarMnu()
SELECT (lnActAlias)
*-- end of lfvDelete.

*!*************************************************************
*! Name      : lfDfRunDir
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 11/19/2000
*! Purpose   : Checks for current drive and if it is CD-ROM or Floopy drive
*!           : it will change the path to the directory of the running aria program.
*!*************************************************************
*!
*B604025,1
FUNCTION lfDfRunDir
PRIVATE lcSetLibr , lcPathSet , lcDfRunDir
*-- Set Libarary to Foxtools.
lcSetLibr = SET("LIBRARY")
IF !(SYS(2004)+"FOXTOOLS.FLL" $ lcSetLibr)
  SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
ENDIF
*-- Save full path setting
lcPathSet = SET("FULLPATH")
SET FULLPATH ON
lcDfRunDir = JUSTPATH(FULLPATH(SYS(16,0)))
SET DEFAULT TO &lcDfRunDir
*-- Restore Full path setting
SET FULLPATH &lcPathSet
*-- Release foxtools libarary.
IF !(SYS(2004)+"FOXTOOLS.FLL" $ lcSetLibr)
  RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")
ENDIF
*-- end of lfDfRunDir.
*!**************************************************************************
*! Function    : AddTecInfo
*! Author      : Mohamed Atia Badran (MAB)
*! Date        : 07/05/2001
*! Purpose     : Add the Technical Notes to Tech. Array
*!**************************************************************************
*
FUNCTION AddTecInfo
PARAMETERS lcFile, lcInfo
PRIVATE lnInfoPos, lcCurrInfo
lnInfoPos = ASCAN(laTechInfo,lcFile)
IF lnInfoPos = 0
  lnInfoPos = ALEN(laTechInfo,1)
  IF !EMPTY(laTechInfo[1,1])
    lnInfoPos = lnInfoPos + 1
    DECLARE laTechInfo[lnInfoPos,2]
  ENDIF
ELSE
  lnInfoPos = ASUBSCRIPT(laTechInfo,lnInfoPos,1)
ENDIF
IF lnInfoPos > 0
  lcInfo = ALLTRIM(lcInfo)
  lcCurrInfo = IIF(EMPTY(laTechInfo[lnInfoPos,2]),"",laTechInfo[lnInfoPos,2])
  IF ATC(lcInfo,lcCurrInfo) = 0
    laTechInfo[lnInfoPos,1] = lcFile
    laTechInfo[lnInfoPos,2] = lcCurrInfo + IIF(EMPTY(lcCurrInfo),"",", ") + lcInfo
  ENDIF
ENDIF
*-- end of AddTecInfo.

*!**************************************************************************
*! Function    : lfNotePad
*! Author      : Mohamed Atia Badran (MAB)
*! Date        : 07/05/2001
*! Purpose     : Write the entry Notepad file.
*!**************************************************************************
*
FUNCTION lfNotePad
PRIVATE llWrite, lnActAlias
llWrite = !EMPTY(laTechInfo)
IF llWrite
  IF FILE(lcEntID + ".TXT")
    ERASE (lcEntID + ".TXT")
  ENDIF
  lnHandle = FCREATE(lcEntID + ".TXT")
  IF lnHandle < 0
    RETURN
  ENDIF
  IF !EMPTY(laTechInfo)
    =FPUTS(lnHandle,"For the EXE file to complete updating your system correctly, please do the following")
    =FPUTS(lnHandle,"  ")
    lnInfo = 0
    FOR lnInfo = 1 TO ALEN(laTechInfo,1)
      IF laTechInfo[lnInfo,1] = "SYCMENU"
        lcStrExpr = STR(lnInfo,1) + "- " + "Inserted menu bar(s) was automatically added to your menu."
        =FPUTS(lnHandle,lcStrExpr)
        =FPUTS(lnHandle," ")
      ENDIF
      IF laTechInfo[lnInfo,1] = "SYDFLFLD"
        lcStrExpr = STR(lnInfo,1) + "- " + "Please update file structure for file(s) " + laTechInfo[lnInfo,2]
        =FPUTS(lnHandle,lcStrExpr)
        =lfStruStep(lnHandle,"Structures")
        =FPUTS(lnHandle," ")
      ENDIF

      IF laTechInfo[lnInfo,1] = "SYDINDEX"
        lcStrExpr = STR(lnInfo,1) + "- " + "Please update file index(s) for file(s) " + laTechInfo[lnInfo,2]
        =FPUTS(lnHandle,lcStrExpr)
        =lfStruStep(lnHandle,"Indices")
        =FPUTS(lnHandle," ")
      ENDIF

      IF laTechInfo[lnInfo,1] = "SYFRMCDD"
        lnPipePos = ATC("|",laTechInfo[lnInfo,2])
        lcMajForm = SUBSTR(laTechInfo[lnInfo,2],1,lnPipePos-1)
        lcCurForm = SUBSTR(laTechInfo[lnInfo,2],lnPipePos+1)
        lcStrExpr = STR(lnInfo,1) + "- " + 'This EXE file includes the "' + lcMajForm +;
                    '" form "' + lcCurForm + '".'
        =FPUTS(lnHandle,lcStrExpr)

        =FPUTS(lnHandle,"In order to activate this form, please do the following")
        =FPUTS(lnHandle,"  a. Run the System.")
        =FPUTS(lnHandle,"  b. Select the System Manager module.")
        =FPUTS(lnHandle,'  c. Run the Form Code screen by clicking the "File/Company Setup/Form Code" menu bar.')
        =FPUTS(lnHandle,"  d. Press the toolbar Edit button to change the active form.")
        =FPUTS(lnHandle,"  e. Select the " + '"' + lcMajForm + '"' +;
                    ' Major form From the grid then select the "' + lcCurForm + ;
                    '" from the drop down combo box.')
        =FPUTS(lnHandle,"  f. Save your selection by pressing the toolbar Save button.")
        =FPUTS(lnHandle," ")
      ENDIF

      IF laTechInfo[lnInfo,1] = "SYCTRIGG"
        lcStrExpr = STR(lnInfo,1) + "- " + laTechInfo[lnInfo,2]
        =FPUTS(lnHandle,lcStrExpr)
        =FPUTS(lnHandle,"Follow up the steps below to ZAP the Static file.")
        =FPUTS(lnHandle,"  a. Be sure that no other users in the system.")
        =FPUTS(lnHandle,"  b. Run the System.")
        =FPUTS(lnHandle,"  c. Select the System Manager module.")
        =FPUTS(lnHandle,'  d. Run the FoxPro Dot Prompt by clicking the "File/FoxPro Dot Prompt" menu bar.')
        =FPUTS(lnHandle,"  e. Issue the following commands.")
        =FPUTS(lnHandle,"     USE " + lcSysDirect + "SYUSTATC.DBF EXCLUSIVE")
        =FPUTS(lnHandle,"     ZAP")
        =FPUTS(lnHandle,"     The system will prompt you to sure that you want to zap the file, then answer by clicking Yes")
        =FPUTS(lnHandle,"     USE")
        =FPUTS(lnHandle,"     QUIT")
        =FPUTS(lnHandle,"  f. Re-Login again.")
        =FPUTS(lnHandle," ")
      ENDIF
    ENDFOR
    lcStrExpr = REPLICATE("-",70)
    =FPUTS(lnHandle,lcStrExpr)

  ENDIF

  =FCLOSE(lnHandle)

  lcNoteDir = FULLPATH(SET("DEFAULT"))
  lcNoteDir = lcNoteDir + IIF(RIGHT(lcNoteDir,1)="\","","\")
  lcNoteF   = lcNoteDir + lcEntID + ".TXT"

  lnReturn   = 0
  lcMessage  =  "In order to complete the update process correctly, " +;
                "it is highly recommended to read the technical information file." +;
                CHR(13) + "Would you like to read the technical information in " +;
                CHR(13) + "(" + lcNoteF + ") ?"
  lnMessType = 4
  lcNoPrompt = "\<Later"
  DO SUEXEMSG.SPX
  IF lnReturn = 1
    lcCommand = [!/N NOTEPAD ] + lcNoteF
    &lcCommand.
  ENDIF

ENDIF

*-- end of lfNotePad.
*!**************************************************************************
*! Function    : lfStruStep
*! Author      : Mohamed Atia Badran (MAB)
*! Date        : 07/09/2001
*! Purpose     : Add File Structure/File Indicies Steps
*!**************************************************************************
*
FUNCTION  lfStruStep
PARAMETERS lnHandle, lcStruType
        =FPUTS(lnHandle,"In order to update file " + lcStruType + ", please do the following")
        =FPUTS(lnHandle,"  a. Be sure that no other users in the system.")
        =FPUTS(lnHandle,"  b. Run the System.")
        =FPUTS(lnHandle,"  c. Select the System Manager module.")
        =FPUTS(lnHandle,'  d. Run the Maintain Dictionary screen by clicking the "Transaction/Maintain Dictionary" menu bar.')
        =FPUTS(lnHandle,"  e. Select the above mentioned file(s).")
        =FPUTS(lnHandle,'  f. Press the "Update file ' + lcStruType + '" button.')
        =FPUTS(lnHandle,'  g. The system will prompt you that updating finished, then you should press OK and close the screen.')
*-- end of lfStruStep.
*!**************************************************************************
*! Function    : lfCrtFile
*! Author      : Mohamed Atia Badran (MAB)
*! Date        : 12/18/2001
*! Purpose     : Create a file into disk
*!**************************************************************************
*
FUNCTION lfCrtFile
PARAMETERS lcCommExc, lcString, lcFile
PRIVATE lcError, llError, lcDir, lcLineFeed
lcError = ON("ERROR")
llError = .F.
ON ERROR llError = .T.
&lcCommExc.     && Execute the passed command.
IF llError
  lcLineFeed = CHR(13) + CHR(10)
  lcDir      = SUBSTR(lcFile,1,RAT("\",lcFile))
  lcMessage  = "Error While " + lcString + lcFile + lcLineFeed +;
               "May you have one of the following : " + lcLineFeed +;
               "1- File was marked as read only." + lcLineFeed +;
               "2- Folder not found in the disk."  + lcLineFeed +;
               "3- You didn't have write access to network drive."
  lnMessType = 0
  DO SUEXEMSG.SPX
ENDIF
ON ERROR &lcError.
RETURN !llError
*-- end of lfCrtFile.

*!**************************************************************************
*! Function    : lfGetSubMn
*! Author      : Ahmed Maher (AMH)
*! Date        : 04/10/2002
*! Purpose     : Mark the sub menu child as inserted
*!**************************************************************************
*! Due to entry : B605846,1
FUNCTION lfGetSubMn
PARAMETERS lcPopupID

PRIVATE lcPopupID,lcMstr_nam,lcPop_Levl
lcMstr_nam = CPROSS_ID
lcPop_Pos  = CPOP_POS
lcPop_Levl = PADL(ALLTRIM(STR(VAL(CPOP_LEVL)+1,2,0)),2,'0')

*-- Search for all sub menu child
*-- Scan the original menu table (lcOrgMnu)
SCAN REST FOR CMSTR_NAM = lcMstr_nam
  lnElmntCnt = lnElmntCnt + 1   && increment the marked lines by one
  lcPop_Pos  = PADL(ALLTRIM(STR(VAL(lcPop_Pos )+1,2,0)),2,'0')
  REPLACE cOwner     WITH "B",;
          cPop_Pos   WITH lcPop_Pos ,;
          cPop_levl  WITH lcPop_Levl,;
          cMstr_Nam  WITH IIF(CSUB_TYP='S',CMSTR_NAM,lcPopupID)

  *-- if the child of the sub menu itself if sub menu mark all its child
  IF CSUB_TYP = 'S'
    lnSubMnu = lnSubMnu + 1  && increment the number of inserted sub menu by one
    =lfGetSubMn(PADR(ALLTRIM(CMSTR_NAM),7)+;
                PADL(ALLTRIM(STR(VAL(RIGHT(ALLTRIM(lcPopupID),2))+1,2,0)),2,'0'))
    REPLACE CMSTR_NAM WITH lcPopupID
  ENDIF
ENDSCAN
LOCATE FOR CSUB_TYP = 'S' .AND. CPROSS_ID = lcMstr_nam

*-- remove this sub menu from laMnuInsrt
PRIVATE lnMnuInsrt
lnMnuInsrt = ASCAN(laMnuInsrt,CPROSS_ID)
IF lnMnuInsrt > 0
  =ADEL(laMnuInsrt,lnMnuInsrt)
  IF ALEN(laMnuInsrt) > 1
    DECLARE laMnuInsrt[ALEN(laMnuInsrt)-1]
  ELSE
    STORE SPACE(0) TO laMnuInsrt
  ENDIF
ENDIF
REPLACE CPROSS_ID WITH lcPopupID
*-- end of lfGetSubMn.

*!**************************************************************************
*! Function    : lfDelSubMn
*! Author      : Ahmed Maher (AMH)
*! Date        : 04/15/2002
*! Purpose     : Mark all sub menu child as deleted
*!**************************************************************************
*! Due to entry : B605846,1
*!
FUNCTION lfDelSubMn

PRIVATE lcMstr_nam
lcMstr_nam = CPROSS_ID

*-- Fill the laMnuInsrt Array with sub menu
IF !EMPTY(laMnuInsrt[1])
  DECLARE laMnuInsrt[ALEN(laMnuInsrt)+1]
ENDIF
laMnuInsrt[ALEN(laMnuInsrt)] = CPROSS_ID

*-- Search for all child of sub menu
SCAN REST FOR CMSTR_NAM = lcMstr_nam
  lnElmntCnt = lnElmntCnt + 1  && increment the number of removed record by one

  *-- if the child of sub menu is itself a sub menu mark its child also
  IF CSUB_TYP = 'S'
    lnSubMnu = lnSubMnu + 1    && increment the number of removed sub menu by one
    =lfDelSubMn()
  ELSE
    REPLACE cOwner WITH SPACE(0)

    *-- blank this child from the target menu
    SELECT (lcTarMnu)
    =SEEK(EVALUATE(lcOrgMnu+'.capp_id')+EVALUATE(lcOrgMnu+'.cpad_pos')+;
          EVALUATE(lcOrgMnu+'.cpop_pos')+EVALUATE(lcOrgMnu+'.cpop_levl')+;
          EVALUATE(lcOrgMnu+'.cbar_pos'))
    BLANK
    SELECT (lcOrgMnu)
  ENDIF
ENDSCAN
LOCATE FOR CSUB_TYP = 'S' .AND. CPROSS_ID = lcMstr_nam
REPLACE COWNER WITH SPACE(0)

*-- blank this sub menu from the target menu
SELECT (lcTarMnu)
=SEEK(EVALUATE(lcOrgMnu+'.capp_id')+EVALUATE(lcOrgMnu+'.cpad_pos')+;
      EVALUATE(lcOrgMnu+'.cpop_pos')+EVALUATE(lcOrgMnu+'.cpop_levl')+;
      EVALUATE(lcOrgMnu+'.cbar_pos'))

*-- remove the sub menu from array laMnuRemve
=ADEL(laMnuRemve,ASCAN(laMnuRemve,CPROSS_ID))
IF ALEN(laMnuRemve) > 1
  DECLARE laMnuRemve[ALEN(laMnuRemve)-1]
ELSE
  STORE SPACE(0) TO laMnuRemve
ENDIF
BLANK
SELECT (lcOrgMnu)
*-- end of lfDelSubMn.