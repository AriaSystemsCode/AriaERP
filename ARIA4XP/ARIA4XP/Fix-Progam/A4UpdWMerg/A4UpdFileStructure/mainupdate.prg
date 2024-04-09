LPARAMETERS lcAria4xpSqlDicPath,lcAria27SysFlsPath
_SCReen.Visible = .F. 
SET SAFETY OFF
SET CONFIRM OFF 

lnCntClient = lfGetNumOfClients()
IF lnCntClient > 1 OR lnCntClient = 0 
  IF FILE(UPPER(ADDBS(ALLTRIM(FULLPATH(''))))+'SqlDictPath.txt')
    ERASE (UPPER(ADDBS(ALLTRIM(FULLPATH(''))))+'SqlDictPath.txt')
  ENDIF
  IF FILE(UPPER(ADDBS(ALLTRIM(FULLPATH(''))))+"InstallLog.xml")
    ERASE (UPPER(ADDBS(ALLTRIM(FULLPATH(''))))+"InstallLog.xml")
  ENDIF
  WAIT WINDOW "To resume the installation, please click on the support message that pops up." 
  *MESSAGEBOX("Client Table has more than one record.Please contact Aria Support Team.")  
  RETURN 
ENDIF
*set step on 
STORE '' TO lcServer99,lcDB99,lcUser99,lcPassword99
IF FILE(UPPER(ADDBS(ALLTRIM(FULLPATH(''))))+"InstallLog.xml")
  XMLTOCURSOR(UPPER(ADDBS(ALLTRIM(FULLPATH(''))))+"InstallLog.xml","InstallLogCursor",512)
  lcAria4xpSqlDicPath = UPPER(ADDBS(ALLTRIM(InstallLogCursor.A4Path)))+'SQLDICTIONARY\'
  lcAria27SysFlsPath= UPPER(ADDBS(ALLTRIM(InstallLogCursor.Aria27Path)))+'SYSFILES\'
  lcServer99 = ALLTRIM(InstallLogCursor.Server99)
  lcDB99= ALLTRIM(InstallLogCursor.DB99)
  lcUser99= ALLTRIM(InstallLogCursor.User99)
  lcPassword99= ALLTRIM(InstallLogCursor.Password99)  
ENDIF

*!*  llSAASSing = .F.
IF FILE(UPPER(ADDBS(ALLTRIM(FULLPATH(''))))+'SqlDictPath.txt')
  lcAria4xpSqlDicPath= FILETOSTR(UPPER(ADDBS(ALLTRIM(FULLPATH(''))))+'SqlDictPath.txt')
*!*    IF UPPER(ADDBS(ALLTRIM(lcAria4xpSqlDicPath))) = UPPER(ADDBS(ALLTRIM(lcAria4xpSMSqlDicPath)))
*!*      llSAASSing = .T.
*!*    ENDIF
 * ERASE SqlDictPath.txt
ENDIF
*OR !FILE(ADDBS(lcAria4xpSqlDicPath)+"SYDFILES.DBF")
IF TYPE('lcAria4xpSqlDicPath') <> 'C' OR ;
  (TYPE('lcAria4xpSqlDicPath') = 'C' AND (EMPTY(lcAria4xpSqlDicPath) OR !DIRECTORY(lcAria4xpSqlDicPath)))
  
  lcAria4xpSqlDicPath = GETDIR("", "Select Aria4XP SQL Dictionary Directory")
  IF EMPTY(lcAria4xpSqlDicPath) OR !DIRECTORY(lcAria4xpSqlDicPath) OR IIF(FILE(ADDBS(lcAria4xpSqlDicPath)+ 'ARIA4XP.txt'),.F.,!FILE(ADDBS(lcAria4xpSqlDicPath)+"SYDFILES.DBF"))
    MESSAGEBOX("Invalid Aria4XP SQL Dictionary Directory")
    RETURN .F.
  ENDIF
ENDIF  

IF TYPE('lcAria27SysFlsPath') <> 'C' OR ;
  (TYPE('lcAria27SysFlsPath') = 'C' AND (EMPTY(lcAria27SysFlsPath) OR !DIRECTORY(lcAria27SysFlsPath) OR !FILE(ADDBS(lcAria27SysFlsPath)+"SYCCOMP.DBF")))
  lcAria27SysFlsPath= GETDIR("", "Select Aria27 Sysfiles Directory")
  
  IF EMPTY(lcAria27SysFlsPath) OR !DIRECTORY(lcAria27SysFlsPath) OR !FILE(ADDBS(lcAria27SysFlsPath)+"SYCCOMP.DBF")
    MESSAGEBOX("Invalid Aria27 Sysfiles Directory")
    RETURN .F.
  ENDIF
ENDIF  

lcAria4xpSqlDicPath = ADDBS(lcAria4xpSqlDicPath)
lcAria27SysFlsPath = ADDBS(lcAria27SysFlsPath)

lcSharedFolderSqlDict = UPPER(ADDBS(ALLTRIM(FULLPATH(''))))+'SQLDICTIONARY\'
lcAria4xpSqlDicPath = UPPER(ADDBS(ALLTRIM(lcAria4xpSqlDicPath)))

IF lcSharedFolderSqlDict  <> lcAria4xpSqlDicPath 
  lfCopyXXFiles(lcSharedFolderSqlDict,lcAria4xpSqlDicPath)
      
  IF !DIRECTORY(UPPER(ADDBS(ALLTRIM(FULLPATH(''))))+"MEDIA_SQLDICTIONARY\")
    MD (UPPER(ADDBS(ALLTRIM(FULLPATH(''))))+"MEDIA_SQLDICTIONARY\")
  ENDIF
  COPY FILE (lcSharedFolderSqlDict +'*.*') TO (UPPER(ADDBS(ALLTRIM(FULLPATH(''))))+"MEDIA_SQLDICTIONARY\"+'*.*')
  ERASE (lcSharedFolderSqlDict+'*.*')
  RD (lcSharedFolderSqlDict) 
  
ENDIF 



*MT
  use (lcAria27SysFlsPath+'SYCCOMP.DBF') in 0 Shared 
  Sele SYCCOMP
  lcCntCmp = ''
  SCAN for !DELETED()
    IF SYCCOMP.ccomp_id = '99' 
       if (EMPTY(SYCCOMP.cconserver) OR EMPTY(SYCCOMP.ccondbname))
        REPLACE ccondbname WITH lcDB99,;
            cconserver WITH lcServer99,;
                cconuserid WITH lcUser99,;
                cconpaswrd WITH lcPassword99,;
                CCONDriver WITH 'SQL'
       ENDIF
       Repl lrunfroma4 with .T.  
       *MMT
       lcSourceDbfs = (UPPER(ADDBS(ALLTRIM(FULLPATH(''))))+"\DEMODATABASE\")
       lcTargDbfs = ADDBS(ALLTRIM(SYccomp.ccom_ddir))
       LCaRIA27foldDBF = ADDBS(STRTRAN(UPPER(ALLTRIM(lcAria27SysFlsPath)),'\SYSFILES',''))+"DBFS\"
       IF !DIRECTORY(LCaRIA27foldDBF+"R12Old99")
         MD (LCaRIA27foldDBF+"R12Old99")
         COPY FILE (lcTargDbfs+"*.*") TO (LCaRIA27foldDBF+"R12Old99\"+"*.*")
         IF DIRECTORY(lcTargDbfs+"Objects")
           MD (LCaRIA27foldDBF+"R12Old99\Objects")
           COPY FILE (lcTargDbfs+"Objects\"+"*.*") TO (LCaRIA27foldDBF+"R12Old99\Objects\"+"*.*")
           DELETE FILE (lcTargDbfs+"Objects\"+"*.*") 
         ENDIF  
         COPY FILE (lcSourceDbfs+"*.*") to (lcTargDbfs+"*.*")
       ENDIF
       *MMT            
    ELSE
      if !lrunfroma4 
        lcCntCmp = lcCntCmp + IIF(!Empty(lcCntCmp),',','')+ SYCCOMP.ccomp_id   
      ENDIF  
    ENDIF
  ENDSCAN  
  use in SYCCOMP
*MT


lcShared27Sys = UPPER(ADDBS(ALLTRIM(FULLPATH(''))))+'SYSFILES\'
IF DIRECTORY(lcShared27Sys)
  lfCopyXXFiles(lcShared27Sys ,lcAria27SysFlsPath)
ELSE
  MESSAGEBOX('ARIA27 Dictionary source files are not exist. Please contact Aria Support Team')  
  RETURN .F.    
ENDIF  
IF !DIRECTORY(UPPER(ADDBS(ALLTRIM(FULLPATH(''))))+"MEDIA_SYSFILES\")
  MD (UPPER(ADDBS(ALLTRIM(FULLPATH(''))))+"MEDIA_SYSFILES\")
ENDIF
COPY FILE (lcShared27Sys +'*.*') TO (UPPER(ADDBS(ALLTRIM(FULLPATH(''))))+"MEDIA_SYSFILES\"+'*.*')
ERASE (lcShared27Sys +'*.*')
RD (lcShared27Sys)
*MT
lcAria27Path  = ADDBS(STRTRAN(UPPER(lcAria27SysFlsPath),'\SYSFILES',''))
*MT
DO CASE 
CASE  DIRECTORY(lcAria27Path) AND FILE(lcAria27Path+'A271stRun.exe')
  lcOldDefa = FULLPATH('')
  SET DEFAULT TO (lcAria27Path)
  DO (lcAria27Path+'A271stRun.exe') &&WITH lcAria27SysFlsPath
  SET DEFAULT TO (lcOldDefa)
  
CASE DIRECTORY(lcAria27Path) AND FILE(lcAria27Path+'A271stRun_cl.exe')
  lcOldDefa = FULLPATH('')
  SET DEFAULT TO (lcAria27Path)
  DO (lcAria27Path+'A271stRun_cl.exe') &&WITH lcAria27SysFlsPath
  SET DEFAULT TO (lcOldDefa)

OTHERWISE 
  MESSAGEBOX('ARIA27 Dictionary is not updated properly. Please contact Aria Support Team')  
  RETURN .F.
ENDCASE
*Merge Start
LOCAL lcSQLDICPATH
lcSQLDICPATH = lcAria4xpSqlDicPath  && In all cases you are multi-inst

IF FILE(lcSQLDICPATH + 'ARIA4XP.txt')
  LOCAL lnSelected ,lcSrc,lcTrg,lcMod ,lcProgram ,lbFound
  *lnSelected = SELECT()
*********************-- BADRAN 2011/04/07 Move ARIA4XP.txt under the SQLDictionary ************ END

  
  LOCAL LASQLDFILES, LNSQLDFILESCOUNT
  *MT
  *LNSQLDFILESCOUNT = 7
   LNSQLDFILESCOUNT = 8
  *MT
  DIMENSION LASQLDFILES[lnSQLDFilesCount, 3]
  
  LASQLDFILES[1, 1] = 'SYDFIELD.DBF'
  LASQLDFILES[1, 2] = 'SYDFIELD.CDX'
  LASQLDFILES[1, 3] = 'SYDFIELD.FPT'
  
  LASQLDFILES[2, 1] = 'SYDFILES.DBF'
  LASQLDFILES[2, 2] = 'SYDFILES.CDX'
  LASQLDFILES[2, 3] = 'SYDFILES.FPT'
  
  LASQLDFILES[3, 1] = 'SYDFLFLD.DBF'
  LASQLDFILES[3, 2] = 'SYDFLFLD.CDX'
  LASQLDFILES[3, 3] = ''
  
  LASQLDFILES[4, 1] = 'SYDINDEX.DBF'
  LASQLDFILES[4, 2] = 'SYDINDEX.CDX'
  LASQLDFILES[4, 3] = 'SYDINDEX.FPT'
   
  LASQLDFILES[5, 1] = 'SYDREPRT.DBF'
  LASQLDFILES[5, 2] = 'SYDREPRT.CDX'
  LASQLDFILES[5, 3] = 'SYDREPRT.FPT'
  
  LASQLDFILES[6, 1] = 'SYREPUVR.DBF'
  LASQLDFILES[6, 2] = 'SYREPUVR.CDX'
  LASQLDFILES[6, 3] = 'SYREPUVR.FPT'
  
  LASQLDFILES[7, 1] = 'SYDOBJCT.DBF'
  LASQLDFILES[7, 2] = 'SYDOBJCT.CDX'
  LASQLDFILES[7, 3] = 'SYDOBJCT.FPT'
  *MT
  LASQLDFILES[8, 1] = 'SYDFXPRG.DBF'
  LASQLDFILES[8, 2] = 'SYDFXPRG.CDX'
  LASQLDFILES[8, 3] = 'SYDFXPRG.FPT'  
  *MT
  LOCAL lcOldSetting
  lcOldSetting = SET('Cpdialog')
  SET CPDIALOG OFF
  
  LOCAL I
  FOR I = 1 TO LNSQLDFILESCOUNT
    LOCAL SYSFILE, XXSYSFILE
    SYSFILE = LASQLDFILES[I, 1]
    XXSYSFILE = STUFF(UPPER(SYSFILE), 1, 2, 'XX')
    WAIT WINDOW 'Updating SQLdictionary files, updating ' + SYSFILE + '.' NOWAIT
    DO CASE
    CASE FILE(lcSQLDICPATH + SYSFILE) AND FILE(lcSQLDICPATH + XXSYSFILE)
        USE  (lcSQLDICPATH + SYSFILE) ALIAS SYSTABLE IN 0 EXCLUSIVE
        SELECT SYSTABLE
        DELETE FROM SYSTABLE WHERE CUPGRDLVL = 'A' .OR. CUPGRDLVL = 'S'
        PACK  IN SYSTABLE
        APPEND FROM (lcSQLDICPATH + XXSYSFILE) FOR  CUPGRDLVL = 'A' .OR. CUPGRDLVL = 'S'
        USE
        ERASE (lcSQLDICPATH + XXSYSFILE)
        IF FILE(lcSQLDICPATH + STRTRAN(XXSYSFILE, '.DBF', '.FPT'))
          ERASE (lcSQLDICPATH + STRTRAN(XXSYSFILE , '.DBF' , '.FPT'))
        ENDIF
        IF FILE(lcSQLDICPATH + STRTRAN(XXSYSFILE, '.DBF', '.CDX'))
          ERASE (lcSQLDICPATH + STRTRAN(XXSYSFILE, '.DBF', '.CDX'))
        ENDIF
        
      CASE !FILE(lcSQLDICPATH + SYSFILE) AND FILE(lcSQLDICPATH + XXSYSFILE)
        RENAME  (lcSQLDICPATH + XXSYSFILE) TO (lcSQLDICPATH + SYSFILE)
        IF FILE(lcSQLDICPATH + STRTRAN(XXSYSFILE, '.DBF', '.FPT'))
      SYSFILE = LASQLDFILES[I, 3]
          RENAME (lcSQLDICPATH + STRTRAN(XXSYSFILE , '.DBF' ,'.FPT')) TO (lcSQLDICPATH + SYSFILE)
        ENDIF
        IF FILE(lcSQLDICPATH + STRTRAN(XXSYSFILE, '.DBF', '.CDX'))
          SYSFILE = LASQLDFILES[I, 2]
          RENAME (lcSQLDICPATH + STRTRAN(XXSYSFILE, '.DBF', '.CDX')) TO (lcSQLDICPATH + SYSFILE)
        ENDIF
        
        CASE FILE(lcSQLDICPATH + SYSFILE) AND !FILE(lcSQLDICPATH + XXSYSFILE)
        *-- This case should not happen
    ENDCASE
  ENDFOR

  *-- Handle SYSQLERR table after installtion
  DO CASE
    CASE FILE(lcSQLDICPATH + 'SYSQLERR.DBF') AND FILE(lcSQLDICPATH + 'XXSQLERR.DBF')
      SYSQLERR = 'SYSQLERR.DBF'
      USE  (lcSQLDICPATH + 'XXSQLERR.DBF') IN 0 EXCLUSIVE
      SELECT XXSQLERR
      DELETE ALL
      PACK
      APPEND FROM (lcSQLDICPATH + SYSQLERR)
      USE
      
      * Erase SYSQLERR files
      ERASE (lcSQLDICPATH + 'SYSQLERR.DBF')
      IF FILE(lcSQLDICPATH + 'SYSQLERR.CDX')
        ERASE (lcSQLDICPATH + 'SYSQLERR.CDX')
      ENDIF
      IF FILE(lcSQLDICPATH + 'SYSQLERR.FPT')
        ERASE (lcSQLDICPATH + 'SYSQLERR.FPT')
      ENDIF
      
      * Rename XXSQLERR to SYSQLERR
      RENAME (lcSQLDICPATH + 'XXSQLERR.DBF') TO (lcSQLDICPATH + 'SYSQLERR.DBF')
      IF FILE(lcSQLDICPATH + 'XXSQLERR.CDX')
        RENAME (lcSQLDICPATH + 'XXSQLERR.CDX') TO (lcSQLDICPATH + 'SYSQLERR.CDX')
      ENDIF
      IF FILE(lcSQLDICPATH + 'XXSQLERR.FPT')
        RENAME (lcSQLDICPATH + 'XXSQLERR.FPT') TO (lcSQLDICPATH + 'SYSQLERR.FPT')
      ENDIF
      
    CASE !FILE(lcSQLDICPATH + 'SYSQLERR.DBF') AND FILE(lcSQLDICPATH + 'XXSQLERR.DBF')
      RENAME (lcSQLDICPATH + 'XXSQLERR.DBF') TO (lcSQLDICPATH + 'SYSQLERR.DBF')
      
      IF FILE(lcSQLDICPATH + 'XXSQLERR.CDX')
        RENAME (lcSQLDICPATH + 'XXSQLERR.CDX') TO (lcSQLDICPATH + 'SYSQLERR.CDX')
      ENDIF
      IF FILE(lcSQLDICPATH + 'XXSQLERR.FPT')
        RENAME (lcSQLDICPATH + 'XXSQLERR.FPT') TO (lcSQLDICPATH + 'SYSQLERR.FPT')
      ENDIF
      
 *MT
 ENDCASE 
 
 IF  !FILE(lcSQLDICPATH + 'SYUSTATC.DBF') AND FILE(lcSQLDICPATH + 'XXUSTATC.DBF')
      * Rename XXSQLERR to SYSQLERR
      RENAME (lcSQLDICPATH + 'XXUSTATC.DBF') TO (lcSQLDICPATH + 'SYUSTATC.DBF')
      IF FILE(lcSQLDICPATH + 'XXUSTATC.CDX')
        RENAME (lcSQLDICPATH + 'XXUSTATC.CDX') TO (lcSQLDICPATH + 'SYUSTATC.CDX')
      ENDIF
      IF FILE(lcSQLDICPATH + 'XXUSTATC.FPT')
        RENAME (lcSQLDICPATH + 'XXUSTATC.FPT') TO (lcSQLDICPATH + 'SYUSTATC.FPT')
      ENDIF

 *MT     
ENDIF

  *! E039109,1 AKM 04/06/2005, Add setting of Aria 2.7 'LRunFromA4' field in 'SYDREPRT' and 'SYDOBJCT' to '.T.' for programs converted to Aria 4XP [Start]
  lcSrc=lcSQLDICPATH+'SYDOBJCT.DBF'
  lcTrg=lcAria27SysFlsPath+'SYDOBJCT.DBF'
  USE (lcSrc) IN 0 ALIAS Sorce SHARED
  USE (lcTrg) IN 0 ALIAS Trget SHARED
  SELECT Sorce
  SCAN
    lcMod = CAPP_ID
    IF  !INLIST(UPPER(lcMod),'SM','SY')
      lcProgram = capobjnam
      lbFound= SEEK (lcMod+lcProgram ,'Trget' ,'CAPP_ID')
      IF lbFound
        SELECT Trget
        REPLACE lRunFromA4 WITH .T.
        LOCATE
      ENDIF
    ENDIF
  ENDSCAN
  USE IN Sorce
  USE IN Trget
  lcSrc=lcSQLDICPATH+'SYDREPRT.DBF'
  lcTrg=lcAria27SysFlsPath+'SYDREPRT.DBF'
  USE (lcSrc) IN 0 ALIAS Sorce SHARED
  USE (lcTrg) IN 0 ALIAS Trget SHARED
  SELECT Sorce
  SCAN
    lcMod = CAPP_ID
    IF  !INLIST(UPPER(lcMod),'SM','SY')
      lcProgram = crep_id
      lbFound  =  SEEK (lcProgram ,'Trget' ,'crep_id')
      IF lbFound
        SELECT Trget
        REPLACE lRunFromA4 WITH .T.
        LOCATE
      ENDIF
    ENDIF
  ENDSCAN
  USE IN Sorce
  USE IN Trget
  *! E039109,1 AKM 04/06/2005, Add setting of Aria 2.7 'LRunFromA4' field in 'SYDREPRT' and 'SYDOBJCT' to '.T.' for programs converted to Aria 4XP [End]
  *! E302857,1 MMT 04/27/2011 Update File Structure for SQL Table[MEDIA][Start]
*!*    lcSQLDICPATH = ADDBS(lcSQLDICPATH)
*!*    IF USED('SYDFILES')
*!*      USE IN SYDFILES
*!*    ENDIF
*!*    USE lcSQLDICPATH +'SYDFILES' SHARED IN 0 ORDER 1 
*!*    USE lcSQLDICPATH +'SYDFILES' SHARED IN 0 ALIAS 'UPDATEFLS' AGAIN ORDER 1 
*!*    IF USED('SYDFLFLD')
*!*      USE IN 'SYDFLFLD' 
*!*    ENDIF
*!*    USE lcSQLDICPATH +'SYDFLFLD' SHARED IN 0 ORDER CFILE_NAM 
*!*    IF USED('SYDFIELD')
*!*      USE IN 'SYDFIELD'
*!*    ENDIF
*!*    USE lcSQLDICPATH +'SYDFIELD' SHARED IN 0 ORDER CFLD_NAME 
*!*    IF USED('SYDINDEX')
*!*      USE IN 'SYDINDEX'
*!*    ENDIF
*!*    USE lcSQLDICPATH +'SYDINDEX' SHARED IN 0 ORDER CFILE 
*!*    This.updatetablestructure (THIS.SysPath,.F.,'SYDFILES','SYDFLFLD' ,'SYDFIELD' ,'SYDINDEX','UPDATEFLS')
*!*    USE IN 'SYDFILES'
*!*    USE IN 'SYDFLFLD' 
*!*    USE IN 'SYDFIELD'
*!*    USE IN 'SYDINDEX'
*!*    USE IN 'UPDATEFLS'
*!*    IF FILE(lcSQLDICPATH +'SYDFXPRG.DBF')
*!*      USE lcSQLDICPATH +'SYDFXPRG.DBF' IN 0 order COBJECTID
*!*      SELECT SYDFXPRG
*!*      SCAN FOR !lprgrun
*!*        lcFlToRun =ADDBS(This.ApplicationHome)+'SM\'+ALLTRIM(SYDFXPRG.cobjectnam)+'.fxp'
*!*        IF FILE(lcFlToRun)
*!*          DO (lcFlToRun) WITH THIS.SysPath
*!*        ENDIF
*!*        REPLACE lprgrun WITH .T. IN SYDFXPRG
*!*      ENDSCAN
*!*      USE IN 'SYDFXPRG'
*!*    ENDIF
  IF FILE(UPPER(ADDBS(ALLTRIM(FULLPATH(''))))+"SCREENS\SO\sowizard.scx")
    USE (UPPER(ADDBS(ALLTRIM(FULLPATH(''))))+"SCREENS\SO\sowizard.scx") IN 0 SHARED AGAIN ALIAS SOWIZARDAls  
	  SELECT SOWIZARDAls
	  REPLACE ALL CLASSLOC WITH "" FOR "EDI" $ UPPER(CLASSLOC)
	  USE IN SOWIZARDAls
  ENDIF

  *! E302857,1 MMT 04/27/2011 Update File Structure for SQL Table[MEDIA][End]
  *********************-- BADRAN 2011/04/07 Move ARIA4XP.txt under the SQLDictionary ************ BEGIN
  *-- BADRAN 2011/04/07 ERASE (THIS.DefaultPath + 'ARIA4XP.txt')
  ERASE (lcSQLDICPATH + 'ARIA4XP.txt')
  *********************-- BADRAN 2011/04/07 Move ARIA4XP.txt under the SQLDictionary ************ END

  *! B999999,1 WSH 03/08/2005, Fis Bug of losting focus from login screen. [Start]
  *-- WAIT CLEAR
  *! B999999,1 WSH 03/08/2005, [End]
  SET CPDIALOG &lcOldSetting.
*  SELECT(lnSelected)
ELSE
   MESSAGEBOX("Aria4XP.txt file is not exist under Aria4XP SqlDictionary. Please contact Aria Support Team")
   RETURN .F.
ENDIF


*Merge End
SET DELETED ON 
SET TABLEPROMPT OFF
SET CPDIALOG OFF
SET NOTIFY OFF
SET NOTIFY CURSOR OFF
SET SAFETY OFF 
SET EXCLUSIVE OFF 


USE SHARED (lcAria4xpSqlDicPath +'SYDFIELD.DBF') ORDER CFLD_NAME IN 0
USE SHARED (lcAria4xpSqlDicPath +'SYDINDEX.DBF') ORDER CFILE IN 0
USE SHARED (lcAria4xpSqlDicPath +'SYDFILES.DBF') ORDER 1 IN 0
USE SHARED (lcAria4xpSqlDicPath +'SYDFLFLD.DBF') ORDER CFILE_NAM IN 0

 CREATE CURSOR UpdFiles (cfile_nam CHARACTER (30), cfile_ttl CHARACTER (34))
 SELECT updfiles
 INDEX ON cfile_nam TAG 'UpdFiles'
 
 SELECT SYDFILES
 SCAN FOR !DELETED()
   SELECT updfiles
   IF  .NOT. SEEK(PADR(SYDFILES.CFILE_NAM, 30), 'UpdFiles')
     APPEND BLANK
     REPLACE cfile_nam WITH SYDFILES.CFILE_NAM, cfile_ttl WITH SYDFILES.CFILE_NAM
   ENDIF
 ENDSCAN   

 DO lpupdatetablestructure IN modistruc.prg WITH lcAria27SysFlsPath , .F., 'SYDFILES', 'SYDFLFLD', 'SYDFIELD', 'SYDINDEX', 'UpdFiles'
 
*!*  *MT
*!*  if empty(lcCntCmp)
*!*  *MT 
*!*    MESSAGEBoX("Update file structure done successfully")
*!*  *MT
*!*  ELSE
*!*    MESSAGEBoX("Update file structure done successfully . Please run conversion program to update the companies: "+lcCntCmp)
*!*  ENDIF
*!*  *MT


*mt
FUNCTION lfGetNumOfClients
Local lnDispLogin
lnDispLogin = SQLGetprop(0,"DispLogin")
lnCountRecord = 0
Try
  loEnvObj = Createobject('Aria.Environment.AriaEnviromentVariables')
  If Type('loEnvObj') = 'O'
    lcsqlsysfilesconnectionstring = loEnvObj.aria50SystemFilesConnectionStringOdbc
    =SQLSetprop(0,"DispLogin",3)
    If !Empty(lcsqlsysfilesconnectionstring)
      lnConnHandle =Sqlstringconnect(lcsqlsysfilesconnectionstring)
      If lnConnHandle > 0
        Try
          lnRemResult = SQLExec(lnConnHandle,"Select COUNT(*) As ClCount from Clients ","Clients_A")
          If lnRemResult > 0
            lnCountRecord = Clients_A.ClCount
          Endif
        Catch
        Endtry
        =SQLDisconnect(lnConnHandle)
      Endif
    Endif
  Endif
Catch
Endtry
= SQLSetprop(0,"DispLogin",lnDispLogin)
Return lnCountRecord

FUNCTION lfCopyXXFiles
 LPARAMETERS lclocalsyspath, lcclientsyspath
 LOCAL lcsetofdefault, lnselect
 lnselect = SELECT(0)
 lcsetofdefault = FULLPATH('')
 SET DEFAULT TO (lclocalsyspath)
 LOCAL ladir, lnfls, lnflindex, lcleft
 llSysFl = .F.
 IF 'SYSFILES' $ UPPER(lcclientsyspath)
   llSysFl  = .T.
 ENDIF
 IF llSysFl  
   USE (lcclientsyspath+"sydappl.dbf") ORDER CAPP_ID IN 0
   SELECT sydappl
 Endif

 DIMENSION ladir[1, 5]
 lnfls = ADIR(ladir, '*.*', 'D')
 FOR lnflindex = 3 TO lnfls
    IF SUBSTR(ladir(lnflindex, 5), 5, 1)='D'
       LOOP
    ENDIF
    lcleft = UPPER(LEFT(ladir(lnflindex, 1), 2))
    IF (lcleft=="XX") .OR. IIF(llSysFl  ,SEEK(lcleft,'sydappl'),.F.)
       COPY FILE (lclocalsyspath+ladir(lnflindex, 1)) TO (lcclientsyspath+ladir(lnflindex, 1))
    ENDIF
 ENDFOR
 IF llSysFl  
   USE IN sydappl
*!*     IF FILE(lclocalsyspath+'A4sysupd.exe')
*!*       COPY FILE (lclocalsyspath+'A4sysupd.exe') TO (lcclientsyspath+'A4sysupd.exe') 
*!*     ENDIF
 ELSE 
   COPY FILE (lclocalsyspath+'ARIA4XP.TXT') TO (lcclientsyspath+'ARIA4XP.TXT')
 ENDIF
 SET DEFAULT TO (lcsetofdefault)
 SELECT (lnselect)
*mt