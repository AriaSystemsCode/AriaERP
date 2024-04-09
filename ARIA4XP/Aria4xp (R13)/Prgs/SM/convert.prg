*:************************************************************************
*: Program file  : CONVERT.PRG			
*: Program desc. : MAIN CONVERSION PROGRAM
*: For screen    : CONVERT.SCX
*:         System: ARIA 4.0 XP
*:         Module: SM and SYSTEM INFORMATION
*:      Developer: AHMED MAHER (AMH)
*:************************************************************************
*: Modifications :
*: E038125,1 AMH 06/02/2004 Use the correct Index to convert POSLN,
*: E038125,1                update LINENO in BOM to be unique and convert STYPRICE file.
*: E038137,1 AMH 06/06/2004 Support Custom Dictionary and add missing tables.
*: E038220,1 WSH 07/25/2004 Add UOM Table and change the manipulation of UOM fields.
*: E038594,1 AMH 10/05/2004 Enhance the conversion speed.
*: B038828,1 AMH 12/23/2004 Fix bug of conversion terminate if MA,MF modules not installed.
*: E038621,1 WSH 05/11/2005 Replace the field SycComp.lRunFromA4 with .T. to indicate that programs for
*:                          this company has been converted to run from Aria4 not from 27...
*: E039550,1 WSH 08/07/2005 Add Quantity Fields Totals to the Item File.
*: B131128,1 AMH 02/19/2006 Fix bug of Fabric file not found.
*: B608498,1 MHM Fix the bug of take only the first line in the cancelled Po's if the same line recived as cancelled many times
*: B608646,1 MMT 08/07/2008 Convert Fill Currency fields in BOM and Bomline Tables[T20080314.0004]
*! E302567,1 MMT 01/06/2009 Change file paths for SAAS[Task:T20081225.0022]
*! B609148,1 MMT 02/15/2010 Some Lines in POSLN Table are not converted to SQL[T20100129.001]
*! B609452,1 MMT 1/08/2010 Conversion program converts BOM.MSZCROSREF incorrectly for extended size scale company[T20100930.0003]
*! B609467,1 MMT 11/25/2010 Manufacture Module does not converted automatically using aria4 conversion[T20100111.0017]
*! E302821,1 TMI 12/19/2010 Resolve the problem of that compatibility level 80 by changing the code to support higher sqlserver releases
*! E303030,1 MAB 12/28/2011 Extend File/Field/Index to NN length instead of 8,10,10
*! B610199,1 HIA 01/16/2013 Aria4xp-SM - Release to GL screen freezes once release is complete [T20121213.0012]
*! B610744,1 MMT 06/12/2014 Fix issues in the Conversion program at GMA[T20131219.0020]
*:************************************************************************

#INCLUDE R:\aria4xp\prgs\sm\convert.h

*--You have to select the company before running the conversion,
*--since this company will be the target location for the conversion.
IF EMPTY(oAriaApplication.ActiveCompanyID)
  =gfModalGen('TRM00321B00000','DIALOG')
  RETURN
ENDIF

DO FORM (oAriaApplication.ScreenHome+"SM\CONVERT.SCX")
*--End...

*!*************************************************************
*! Name      : lfvCancel
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/15/2004
*! Purpose   : Valid Cancel button.
*!*************************************************************
FUNCTION lfvCancel
LPARAMETERS oFormSet

IF MOD(oFormSet.lnstep,5)-1>0
  *--Are you sure you want to cancel the conversion? Y/N
  IF gfModalGen('QRM00325B00006','DIALOG') = 2
    RETURN .F.
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvModSel
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/15/2004
*! Purpose   : Valid Selecting Modules.
*!*************************************************************
FUNCTION lfvModSel
LPARAMETERS lcSign,oFormSet

oFormSet.lnModuleCnt = oFormSet.lnModuleCnt &lcSign. 1
IF lcSign = '+'
  oFormSet.lcSelModules = oFormSet.lcSelModules + SYCNVMDL.CAPP_ID + '|'
  =lfDefneFls(SYCNVMDL.cApp_Id,(SYCNVMDL.cAutRef = 'Converting...' OR SYCNVMDL.cAutRef = 'Reconverting...'),oFormSet)
ELSE
  oFormSet.lcSelModules = STRTRAN(oFormSet.lcSelModules , SYCNVMDL.CAPP_ID + '|')
  =lfRemovFls(SYCNVMDL.cApp_Id,oFormSet)
ENDIF
=lfUpdCnvMdl(.T.,oFormSet)

*!*************************************************************
*! Name      : lfValdFl
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/15/2004
*! Purpose   : Valid add/remove files in mover.
*!*************************************************************
*! Passed Parameters  : lnOption-> option no. in mover.
*!*************************************************************
*! Returns            : .T. or .F. if not valid.
*!*************************************************************
*!*  FUNCTION lfValdFl
*!*  PARAMETERS lnOption,lnListIndex,oFormSet

*!*  *--You cannot remove this file(s), it was converting right now!
*!*  DO CASE
*!*    CASE lnOption=3
*!*      IF '(In-Work)' $ laTarget[lnListIndex]
*!*        =gfModalGen('TRM00327B00000','DIALOG')
*!*        RETURN .F.
*!*      ENDIF
*!*    CASE lnOption=4
*!*      llTremnte=.F.
*!*      FOR lnI=1 TO ALEN(laTarget,1)
*!*        IF '(In-Work)' $ laTarget[lnI]
*!*          llTremnte=.T.
*!*          EXIT
*!*        ENDIF
*!*      ENDFOR
*!*      IF llTremnte
*!*        =gfModalGen('TRM00327B00000','DIALOG')
*!*        RETURN .F.
*!*      ENDIF
*!*  ENDCASE

*!*************************************************************
*! Name      : lfvModDet
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/15/2004
*! Purpose   : Select files (Detail).
*!*************************************************************
FUNCTION lfvModDet
LPARAMETERS oFormSet

PRIVATE laSource[ALEN(oFormSet.laSource)],laTarget[ALEN(oFormSet.laTarget)]
=ACOPY(oFormSet.laSource,laSource)
=ACOPY(oFormSet.laTarget,laTarget)

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfMover(@laSource,@laTarget,LANG_CONVERT_SELFILES,.T.,.F.,.F.,.F.,oFormSet)
=gfMover(@laSource,@laTarget,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_SELFILES,oFormSet.GetHeaderText("LANG_CONVERT_SELFILES",oFormSet.HeaderAlias)),.T.,.F.,.F.,.F.,oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]


LOCAL lnMousePointer
lnMousePointer = oFormSet.AriaForm1.MousePointer
oFormSet.AriaForm1.MousePointer = 11
oFormSet.AriaForm1.LockScreen = .T.
LOCAL lnI,lnTarget
lnTarget = ALEN(laTarget)
DIMENSION oFormSet.laTarget[lnTarget]
=ACOPY(laTarget,oFormSet.laTarget)

oFormSet.lnModuleCnt = 0
oFormSet.lcSelModules = ''
SELECT SYCNVMDL
REPLACE ALL nFlag WITH 0, mfile_app WITH ''

IF !EMPTY(laTarget[1])
  FOR lnI = 1 TO lnTarget
    *E303030,1 BEGIN oAriaApplication.FieldW
*!*	    =lfAddFile(PADR(laTarget[lnI],10),oFormSet)
*!*	    =SEEK(PADR(laTarget[lnI],10),'SYCNVLIB')
*!*	    =lfCnvFlsCrl(PADR(SYCNVLIB.CFILE,8),SYCNVLIB.CFILE_TTL,.F.,('(In-Work)' $ laTarget[lnI]))

    =lfAddFile(PADR(laTarget[lnI],oAriaApplication.FieldW),oFormSet)
    =SEEK(PADR(laTarget[lnI],oAriaApplication.FieldW),'SYCNVLIB')
    =lfCnvFlsCrl(PADR(SYCNVLIB.CFILE,oAriaApplication.FileW),SYCNVLIB.CFILE_TTL,.F.,('(In-Work)' $ laTarget[lnI]))
    *E303030,1 END

  ENDFOR
ENDIF

SELECT SYCNVMDL
REPLACE FOR nFlag = 2 nFlag WITH IIF(lfSetModule(cApp_Id,oFormSet),1,2)
LOCATE
=lfUpdCnvMdl(.F.,oFormSet)
oFormSet.AriaForm1.LockScreen = .F.
oFormSet.AriaForm1.MousePointer = lnMousePointer
*PADR


*!*************************************************************
*! Name      : lfDefneFls
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/15/2004
*! Purpose   : Define target array.
*!*************************************************************
FUNCTION lfDefneFls
LPARAMETERS lcFileMod,llInWork,oFormSet,llChkSource

LOCAL llRet,lnFilePos,lnSourceLen,lcFileStatus,lcFile_App
SELECT SYCNVLIB
lcFile_App = ''
llRet = .F.
SCAN FOR lcFileMod $ mFile_app
  llRet = .T.

  *E303030,1 BEGIN
  *lnFilePos = ASCAN(oFormSet.laTarget,PADR(cFile,10))
  lnFilePos = ASCAN(oFormSet.laTarget,PADR(cFile,oAriaApplication.FieldW))
  *E303030,1 END oAriaApplication

  IF EMPTY(SYCNVMDL.mfile_app)
    IF llInWork
      lcFileStatus = ' (In-Work)'
    ELSE
      lcFileStatus = '.........'
    ENDIF

    IF SYCNVMDL.nFlag = 1
      IF lnFilePos = 0
        IF EMPTY(oFormSet.laTarget[1])
          *E303030,1 BEGIN
          *oFormSet.laTarget[1] = PADR(cFile,10)+lcFileStatus
          oFormSet.laTarget[1] = PADR(cFile,oAriaApplication.FieldW)+lcFileStatus
          *E303030,1 BEGIN oAriaApplication
        ELSE
          lnSourceLen = ALEN(oFormSet.laTarget)
          DIMENSION oFormSet.laTarget[lnSourceLen+1]
          *E303030,1 BEGIN
          *oFormSet.laTarget[lnSourceLen+1] = PADR(cFile,10)+lcFileStatus
          oFormSet.laTarget[lnSourceLen+1] = PADR(cFile,oAriaApplication.FieldW)+lcFileStatus
          *E303030,1 BEGIN oAriaApplication
        ENDIF
        *E303030,1 BEGIN
        *=lfCnvFlsCrl(PADR(cFile,8),SYCNVLIB.CFILE_TTL,.F.,llInWork)
        =lfCnvFlsCrl(PADR(cFile,oAriaApplication.FileW),SYCNVLIB.CFILE_TTL,.F.,llInWork)
        *E303030,1 BEGIN oAriaApplication
      ENDIF
      *E303030,1 BEGIN oAriaApplication
      *lcFile_App = lcFile_App + PADR(cFile,10) + ','
      lcFile_App = lcFile_App + PADR(cFile,oAriaApplication.FieldW) + ','
      *E303030,1 BEGIN oAriaApplication
    ENDIF
  ELSE
    *E303030,1 BEGIN oAriaApplication
    *IF PADR(cFile,10) $ SYCNVMDL.mfile_app
    IF PADR(cFile,oAriaApplication.FieldW) $ SYCNVMDL.mfile_app
    *E303030,1 BEGIN oAriaApplication
      IF llInWork
        lcFileStatus = ' (In-Work)'
      ELSE
        lcFileStatus = '.........'
      ENDIF

      IF lnFilePos = 0
        IF EMPTY(oFormSet.laTarget[1])
          *E303030,1 BEGIN oAriaApplication
          *oFormSet.laTarget[1] = PADR(cFile,10)+lcFileStatus
          oFormSet.laTarget[1] = PADR(cFile,oAriaApplication.FieldW)+lcFileStatus
          *E303030,1 BEGIN oAriaApplication
        ELSE
          lnSourceLen = ALEN(oFormSet.laTarget)
          DIMENSION oFormSet.laTarget[lnSourceLen+1]
          *E303030,1 BEGIN oAriaApplication
          *oFormSet.laTarget[lnSourceLen+1] = PADR(cFile,10)+lcFileStatus
          oFormSet.laTarget[lnSourceLen+1] = PADR(cFile,oAriaApplication.FieldW)+lcFileStatus
          *E303030,1 BEGIN oAriaApplication
        ENDIF
        *E303030,1 BEGIN
        *=lfCnvFlsCrl(PADR(cFile,8),SYCNVLIB.CFILE_TTL,.F.,llInWork)
        =lfCnvFlsCrl(PADR(cFile,oAriaApplication.FileW),SYCNVLIB.CFILE_TTL,.F.,llInWork)
        *E303030,1 BEGIN oAriaApplication
      ENDIF

    ELSE
      lcFileStatus = '.........'
    ENDIF
  ENDIF

  IF llChkSource
    *E303030,1 BEGIN
    *lnFilePos = ASCAN(oFormSet.laSource,PADR(cFile,10))
    lnFilePos = ASCAN(oFormSet.laSource,PADR(cFile,oAriaApplication.FieldW))
    *E303030,1 BEGIN oAriaApplication
    IF lnFilePos = 0
      IF EMPTY(oFormSet.laSource[1])
        *E303030,1 BEGIN
        *oFormSet.laSource[1] = PADR(cFile,10)+lcFileStatus
        oFormSet.laSource[1] = PADR(cFile,oAriaApplication.FieldW)+lcFileStatus
        *E303030,1 BEGIN oAriaApplication
      ELSE
        lnSourceLen = ALEN(oFormSet.laSource)
        DIMENSION oFormSet.laSource[lnSourceLen+1]
        *E303030,1 BEGIN
        *oFormSet.laSource[lnSourceLen+1] = PADR(cFile,10)+lcFileStatus
        oFormSet.laSource[lnSourceLen+1] = PADR(cFile,oAriaApplication.FieldW)+lcFileStatus
        *E303030,1 BEGIN oAriaApplication
      ENDIF
    ENDIF
  ENDIF
ENDSCAN

IF !EMPTY(lcFile_App)
  SELECT SYCNVMDL
  REPLACE mFile_app WITH lcFile_App
ENDIF
RETURN llRet
*PADR


*!*************************************************************
*! Name      : lfUpdError
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/18/2004
*! Purpose   : Save the occurs errors.
*!*************************************************************
FUNCTION lfUpdError
LPARAMETERS lcErrTxt,oFormSet,llNotDispMess,llNoErrOccur

*E038594,1 AMH Write any error occured directly to file converr.txt to help on fix conversion bugs [Start]
=STRTOFILE(CHR(13)+CHR(10)+lcErrTxt,oAriaApplication.WorkDir+"ConvErr.txt",1)
*E038594,1 AMH [End]

LOCAL lnAlias,llTerminate
lnAlias =SELECT(0)
llTerminate = .F.
IF !SEEK(ALLTRIM(lcErrTxt),oFormSet.lcTmpErr)
  INSERT INTO (oFormSet.lcTmpErr) ( cErrTxt ) VALUES (lcErrTxt)
  oFormSet.llErrOccur = oFormSet.llErrOccur OR !llNoErrOccur
ENDIF
IF !oFormSet.llIgnoreError AND !llNotDispMess
  *--lcErrTxt+'! The conversion will be terminated
  =gfModalGen('TRM00328B00000','DIALOG',lcErrTxt)
  llTerminate = .T.
ENDIF
SELECT (lnAlias)
RETURN llTerminate

*:*************************************************************
*: Name      : lfvSavLog
*: Developer : AHMED MAHER (AMH)
*: Date      : 03/18/2004
*: Purpose   : Save created log.
*:*************************************************************
FUNCTION lfvSavLog
LPARAMETERS llAutoSv,oFormSet

LOCAL lcLogFile,lnAlias
IF !llAutoSv
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcLogFile = GETFILE('TXT', LANG_CONVERT_LOGFILE, LANG_CONVERT_SELECT,1)
lcLogFile = GETFILE('TXT', IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_LOGFILE,oFormSet.GetHeaderText("LANG_CONVERT_LOGFILE",oFormSet.HeaderAlias)), IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_SELECT,oFormSet.GetHeaderText("LANG_CONVERT_SELECT",oFormSet.HeaderAlias)),1)
*N000682,1 11/20/2012 MMT Globlization changes[End]

ELSE
  lcLogFile = ALLT(FULLPATH(''))+"CnvLog.Sav"
ENDIF

IF EMPTY(lcLogFile)
  RETURN
ENDIF

IF 'UNTITLED' $ UPPER(lcLogFile)
  lcLogFile = STRTRAN(lcLogFile,"Untitled","CnvLog.txt")
ENDIF

lnAlias = SELECT(0)
SELECT (oFormSet.lcTmpErr)
SET ORDER TO
COPY TO (lcLogFile) TYPE DELIMITED
SELECT(lnAlias)
RETURN

*:*************************************************************
*: Name      : lfOpenRemt
*: Developer : AHMED MAHER (AMH)
*: Date      : 02/10/2004
*: Purpose   : Open Remote files.
*:*************************************************************
FUNCTION lfOpenRemt
LPARAMETERS lcTable,lcCursor,lcTagName,lcTagExp,oFormSet

LOCAL lnConnectionHandlar, lcSqlStatment, lnI, llFirstField
LOCAL ARRAY laFields[1]

oAriaApplication.RemoteCompanyData.mindexfields(lcTagExp,@laFields)
*E302821,1 TMI 12/26/2010 [Start] replace the hint: index=<lcTagName> BY with(index(<indexTag>))
*lcSqlStatment = "SELECT * FROM " + lcTable + "(index=" + lcTagName + ") WHERE "
lcSqlStatment = "SELECT * FROM " + lcTable + " WITH(index(" + lcTagName + ")) WHERE "
*E302821,1 TMI 12/26/2010 [End  ]

llFirstField = .T.
FOR lnI = 1 TO ALEN(laFields)
  lcSqlStatment = lcSqlStatment+IIF(llFirstField,""," AND ")+"["+LOWER(laFields[lnI])+"]=?m."+LOWER(laFields[lnI])
  llFirstField = .F.
ENDFOR

*E038137,1 Retriving data with native Fox command to give the conversion process more speed [Start]
*lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,lcCursor,lcTable,;
                      oAriaApplication.ActiveCompanyConStr,3,'SAVE',oFormSet.DataSessionID)
IF USED(lcCursor)
  USE IN (lcCursor)
ENDIF

lnConnectionHandlar = SQLEXEC(lnConnHand,lcSqlStatment,lcCursor)

*E038594,1 AMH Disconnect the connection after using it 1000 times [Start]
lnConnCnt = lnConnCnt + 1
*!*  IF lnConnCnt = 1000
*!*    lnConnectionHandlar = SQLDISCONNECT(lnConnHand)
*!*    =STRTOFILE(CHR(13)+CHR(10)+STR(lnConnCnt),oAriaApplication.WorkDir+"ConvConn.txt",1)
*!*    lnConnCnt = 0
*!*    lnConnHand = SQLSTRINGCONNECT(oAriaApplication.ActiveCompanyConStr)
*!*  ENDIF
*E038594,1 AMH [End]

*IF lnConnectionHandlar # 1
IF lnConnectionHandlar < 0
*E038137,1 [End]

  *E038594,1 AMH Disconnect the connection if error occured then try the same SQL statment with new one [Start]
  *RETURN .F.
  lnConnectionHandlar = SQLDISCONNECT(lnConnHand)
  =STRTOFILE(CHR(13)+CHR(10)+STR(lnConnCnt),oAriaApplication.WorkDir+"ConvConn.txt",1)
  lnConnCnt = 0
  lnConnHand = SQLSTRINGCONNECT(oAriaApplication.ActiveCompanyConStr)
  lnConnectionHandlar = SQLEXEC(lnConnHand,lcSqlStatment,lcCursor)
  lnConnCnt = lnConnCnt + 1
  IF lnConnectionHandlar < 0
    RETURN .F.
  ENDIF
  *E038594,1 AMH [End]

ENDIF

*:*************************************************************
*: Name      : lfUpdateRemt
*: Developer : AHMED MAHER (AMH)
*: Date      : 02/10/2004
*: Purpose   : Update Remote files.
*:*************************************************************
FUNCTION lfUpdateRemt
LPARAMETERS lcSqlCommand

*E038137,1 Update SQL table with native Fox command to give the conversion process more speed [Start]
*LOCAL lnConnectionHandlar, lcTranCode, llReturn, LcPrimaryKeyList, llFirstField, lnI
*LOCAL ARRAY laFields[1]
*
*llReturn   = .T.
*
*lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'',.T.)
*IF TYPE('lcTranCode') = 'N'
*  RETURN .F.
*ENDIF
*
*LcPrimaryKeyList = ""
*oAriaApplication.RemoteCompanyData.mindexfields(lcTagExp,@laFields)
*llFirstField = .T.
*FOR lnI = 1 TO ALEN(laFields)
*  LcPrimaryKeyList = LcPrimaryKeyList+IIF(llFirstField,"",",")+LOWER(laFields[lnI])
*  llFirstField = .F.
*ENDFOR
*
*lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(;
*                      lcTable,lcTranCode,oFormSet.DataSessionId,;
*                      LcPrimaryKeyList)
*
*IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
*  llReturn = .F.
*ENDIF
*
*lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode,.T.)
*IF lnConnectionHandlar # 1
*  llReturn = .F.
*ENDIF
*RETURN llReturn
LOCAL lnConnectionHandlar

lnConnectionHandlar = SQLEXEC(lnConnHand,lcSqlCommand)

*E038594,1 AMH Disconnect the connection after using it 1000 times [Start]
lnConnCnt = lnConnCnt + 1
*!*  IF lnConnCnt = 1000
*!*    lnConnectionHandlar = SQLDISCONNECT(lnConnHand)
*!*    =STRTOFILE(CHR(13)+CHR(10)+STR(lnConnCnt),oAriaApplication.WorkDir+"ConvConn.txt",1)
*!*    lnConnCnt = 0
*!*    lnConnHand = SQLSTRINGCONNECT(oAriaApplication.ActiveCompanyConStr)
*!*  ENDIF
*E038594,1 AMH [End]

IF lnConnectionHandlar < 0

  *E038594,1 AMH Disconnect the connection after error occured then try the same SQL statment with new one [Start]
  *RETURN .F.
  lnConnectionHandlar = SQLDISCONNECT(lnConnHand)
  =STRTOFILE(CHR(13)+CHR(10)+STR(lnConnCnt),oAriaApplication.WorkDir+"ConvConn.txt",1)
  lnConnCnt = 0
  lnConnHand = SQLSTRINGCONNECT(oAriaApplication.ActiveCompanyConStr)
  lnConnectionHandlar = SQLEXEC(lnConnHand,lcSqlCommand)
  lnConnCnt = lnConnCnt + 1
  IF lnConnectionHandlar < 0
    RETURN .F.
  ENDIF
  *E038594,1 AMH [End]

ELSE
  RETURN .T.
ENDIF
*E038137,1 [End]

*:*************************************************************
*: Name      : lfConvFile
*: Developer : AHMED MAHER (AMH)
*: Date      : 02/12/2004
*: Purpose   : Convert files.
*:*************************************************************
FUNCTION lfConvFile
LPARAMETERS lcRemt_Name,lcFile_Name,lcSydIndex,oFormSet

LOCAL lcFlFlter,lcRunUpd,llDumy

*E038594,1 AMH Don't delete record from SQL table becuase it tacks a lot of time.
*E038594,1     we drop table before chech in its structure and indeces [Start]
*IF SYCNVFLS.NFLAG = 1
*  IF !lfDelFile(lcFile_Name)
*    IF lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+lcFile_Name+LANG_CONVERT_NODELETE,oFormSet)
*      RETURN .F.
*    ENDIF
*  ENDIF
*ENDIF
*E038594,1 AMH [End]

*E038137,1 Update SQL table with native Fox command to give the conversion process more speed [Start]
IF !lfSqlInsert(lcFile_Name)
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+lcFile_Name+LANG_CONVERT_NORETREI,oFormSet)
IF lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+lcFile_Name+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NORETREI,oFormSet.GetHeaderText("LANG_CONVERT_NORETREI",oFormSet.HeaderAlias)),oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

    RETURN .F.
  ENDIF
ENDIF
SELECT (lcSydIndex)
LOCATE FOR cFile_tag = SYCNVLIB.cFile_tag
IF !lfSqlUpdate(lcFile_Name,cindx_exp,ALLTRIM(cFile_tag))
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+lcFile_Name+LANG_CONVERT_NORETREI,oFormSet)
IF lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+lcFile_Name+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NORETREI,oFormSet.GetHeaderText("LANG_CONVERT_NORETREI",oFormSet.HeaderAlias)),oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

    RETURN .F.
  ENDIF
ENDIF
*E038137,1 [End]

*-1) Case of normal update.
IF SYCNVLIB.lExt_call
  *--Open Aria27 File.
  SELECT SYCNVLIB
  IF !gfOpenFile(oAriaApplication.DataDir+ALLTRIM(cFile_a26),'','SH')
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+ALLTRIM(SYCNVLIB.cFile_a26)+LANG_CONVERT_OPENFILE,oFormSet)
IF lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+ALLTRIM(SYCNVLIB.cFile_a26)+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_OPENFILE,oFormSet.GetHeaderText("LANG_CONVERT_OPENFILE",oFormSet.HeaderAlias)),oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

      RETURN .F.
    ENDIF
  ENDIF
  lcFlFlter = IIF(!EMPTY(SYCNVLIB.f_cond),ALLTRIM(SYCNVLIB.f_cond),".T.")
  SELECT (ALLTRIM(SYCNVLIB.cFile_a26))
  COUNT FOR &lcFlFlter. TO oFormSet.lnRecCnt
  oFormSet.lnCurRec = oFormSet.lnCurRec * oFormSet.lnRecCnt / 100

  *--C o v e r s i o n  e n j g e n.
  *--Scan on source file.
  SELECT (lcSydIndex)
  LOCATE FOR cFile_tag = SYCNVLIB.cFile_tag
  SELECT (ALLTRIM(SYCNVLIB.cFile_a26))
  SCAN FOR &lcFlFlter.
    SCATTER MEMVAR MEMO
    IF !lfUpdEngn(lcRemt_Name,lcFile_Name,EVALUATE(lcSydIndex+'.cindx_exp'),ALLTRIM(STR(RECNO())),oFormSet)
      RETURN .F.
    ENDIF
  ENDSCAN

  *--Close Files.
  IF USED(ALLTRIM(SYCNVLIB.cFile_a26))
    USE IN (ALLTRIM(SYCNVLIB.cFile_a26))
  ENDIF

  IF USED(lcRemt_Name)
    USE IN (lcRemt_Name)
  ENDIF

*-2)Case of UpNormal update.
ELSE
  lcRunUpd = SYCNVLIB.f_name
  IF !EMPTY(lcRunUpd)
    llDumy = &lcRunUpd.
    IF !llDumy
      RETURN .F.
    ENDIF
  ELSE
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+ALLTRIM(SYCNVLIB.cFile_a26)+LANG_CONVERT_NOUPDATE,oFormSet)
IF lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+ALLTRIM(SYCNVLIB.cFile_a26)+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOUPDATE,oFormSet.GetHeaderText("LANG_CONVERT_NOUPDATE",oFormSet.HeaderAlias)),oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

      RETURN .F.
    ENDIF
  ENDIF
ENDIF

*AMH Add the record count of SQL table in the conversion log [Start]
LOCAL lnConnectionHandlar
lnConnectionHandlar = SQLEXEC(lnConnHand,"SELECT COUNT(CADD_USER) AS SQLCNT FROM "+lcFile_Name,"SQLCNT")
IF lnConnectionHandlar > 0
  SELECT SQLCNT
  LOCATE
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfUpdError(LANG_CONVERT_FILE+lcFile_Name+" "+ALLTRIM(STR(SQLCNT))+LANG_CONVERT_RECORDS,oFormSet,.F.,.T.)
=lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+lcFile_Name+" "+ALLTRIM(STR(SQLCNT))+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_RECORDS,oFormSet.GetHeaderText("LANG_CONVERT_RECORDS",oFormSet.HeaderAlias)),oFormSet,.F.,.T.)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  USE IN SQLCNT
ENDIF
*AMH [End]

*E038137,1 Remove Update function [Start]
= SQLEXEC(lnConnHand,"DROP FUNCTION "+ALLTRIM(LOWER(lcFile_Name))+"fun")

*E038594,1 AMH Disconnect the connection after using it 1000 times [Start]
lnConnCnt = lnConnCnt + 1
*!*  IF lnConnCnt = 1000
*!*    lnConnectionHandlar = SQLDISCONNECT(lnConnHand)
*!*    =STRTOFILE(CHR(13)+CHR(10)+STR(lnConnCnt),oAriaApplication.WorkDir+"ConvConn.txt",1)
*!*    lnConnCnt = 0
*!*    lnConnHand = SQLSTRINGCONNECT(oAriaApplication.ActiveCompanyConStr)
*!*  ENDIF
*E038594,1 AMH [End]

*E038137,1 [End]

SELECT SYCNVLIB

*:*************************************************************
*: Name      : lfvProceed
*: Developer : AHMED MAHER (AMH)
*: Date      : 02/12/2004
*: Purpose   : Validate the proceed button.
*:*************************************************************
FUNCTION lfvProceed
LPARAMETERS oFormSet

*--Open Dictionary files.
LOCAL lnTableVal
lnTableVal = SET("TableValidate")
SET TABLEVALIDATE TO 0

LOCAL lcDictionaryDir
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
*lcDictionaryDir = STRTRAN(oAriaApplication.ProgramHome,"PRGS\","SQLDictionary\")
lcDictionaryDir = STRTRAN(oAriaApplication.ProgramHome,"PRGS\","SYSFILES\")
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[eND]
*! E302567,1 MMT 01/06/2009 Change file paths for SAAS[Start]
*!*	=gfOpenFile(lcDictionaryDir+'SYDFILES','Cfile_nam','SH')

*!*	*E038137,1 AMH Use cFlFld index when add new custom fields [Start]
*!*	*=gfOpenFile(lcDictionaryDir+'SYDFLFLD','Cfile_nam','SH')
*!*	=gfOpenFile(lcDictionaryDir+'SYDFLFLD','cFlFld','SH')
*!*	*E038137,1 AMH [End]

*!*	=gfOpenFile(lcDictionaryDir+'SYDFIELD','Cfld_name','SH')

*!*	*E038137,1 AMH Use cFile_Nam index when add new custom index [Start]
*!*	*=gfOpenFile(lcDictionaryDir+'SYDINDEX','Cfile','SH')
*!*	=gfOpenFile(lcDictionaryDir+'SYDINDEX','cFile_Nam','SH')
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
*!*	lnFilesTable =oAriaApplication.RemoteCompanyData.execute ("Select * from SYDFILES ",;
*!*		                                  '',"SYDFILES","",oAriaApplication.cAria4SysFiles,;
*!*		                                  3,"",oFormSet.DataSessionID)
lnFilesTable =oAriaApplication.RemoteCompanyData.execute ("Select * from SYDFILES where CVER='A40'",;
	                                  '',"SYDFILES","",oAriaApplication.cAria4SysFiles,;
	                                  3,"",oFormSet.DataSessionID)
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[END]	                                  
IF lnFilesTable > 0
  SELECT SYDFILES
  =CURSORSETPROP("Buffering",3,'SYDFILES')
  INDEX on CFILE_NAM TAG 'CFILE_NAM'
ELSE
  RETURN .F.
ENDIF  	

lnFLFLDTable =oAriaApplication.RemoteCompanyData.execute ("Select * from SYDFLFLD ",;
	                                  '',"SYDFLFLD","",oAriaApplication.cAria4SysFiles,;
	                                  3,"",oFormSet.DataSessionID)
IF lnFLFLDTable > 0
  SELECT SYDFLFLD
  =CURSORSETPROP("Buffering",3,'SYDFLFLD')
  INDEX on CFILE_NAM+CFLD_NAME TAG 'cFlFld'
ELSE
  RETURN .F.
ENDIF  	

lnSYDINDEXTable =oAriaApplication.RemoteCompanyData.execute ("Select * from SYDINDEX ",;
	                                  '',"SYDINDEX","",oAriaApplication.cAria4SysFiles,;
	                                  3,"",oFormSet.DataSessionID)
IF lnSYDINDEXTable > 0
  SELECT SYDINDEX
  =CURSORSETPROP("Buffering",3,'SYDINDEX')
  INDEX on CFILE_NAM TAG 'CFILE_NAM'
ELSE
  RETURN .F.
ENDIF  	
*! B610199,1 HIA 01/16/2013 Aria4xp-SM - Release to GL screen freezes once release is complete [T20121213.0012][Start]
*lnFieldTable =oAriaApplication.RemoteCompanyData.execute ("Select * from SYDFIELD",;
	                                  '',"SYDFIELD","",oAriaApplication.cAria4SysFiles,;
	                                  3,"",oFormSet.DataSessionID)
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
*!*	lnFieldTable =oAriaApplication.RemoteCompanyData.execute ("Select * from SYDFIELD",;
*!*		                                  '',"AYDFIELD","",oAriaApplication.cAria4SysFiles,;
*!*		                                  3,"",oFormSet.DataSessionID)	  
lnFieldTable =oAriaApplication.RemoteCompanyData.execute ("Select * from SYDFIELD where (CVER = 'A40' OR EMPTY(CVER))",;
	                                  '',"AYDFIELD","",oAriaApplication.cAria4SysFiles,;
	                                  3,"",oFormSet.DataSessionID)	  
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[enD]	                                                                  
*! B610199,1 HIA 01/16/2013 Aria4xp-SM - Release to GL screen freezes once release is complete [T20121213.0012][end]	                                  
IF lnFieldTable > 0
  *! B610199,1 HIA 01/16/2013 Aria4xp-SM - Release to GL screen freezes once release is complete [T20121213.0012][Start]
*!*	  SELECT SYDFIELD
*!*	  =CURSORSETPROP("Buffering",3,'SYDFIELD')
  SELECT AYDFIELD
  =CURSORSETPROP("Buffering",3,'AYDFIELD')  
  *! B610199,1 HIA 01/16/2013 Aria4xp-SM - Release to GL screen freezes once release is complete [T20121213.0012][END]
  INDEX on CFLD_NAME TAG 'CFLD_NAME'
ELSE
  RETURN .F.
ENDIF  	
*! E302567,1 MMT 01/06/2009 Change file paths for SAAS[End]
*E038137,1 AMH [End]

*E038594,1 AMH Open Style, Scale, Fabric and Codes files here to give conversion more speed [Start]
IF !gfOpenFile(oAriaApplication.DataDir+'STYLE','CSTYLE','SH')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfUpdError(LANG_CONVERT_NOTOPEN+'STYLE'+LANG_CONVERT_FILE2,oFormSet)
=lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'STYLE'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  RETURN .F.
ENDIF

IF !gfOpenFile(oAriaApplication.DataDir+'SCALE','SCALE','SH')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfUpdError(LANG_CONVERT_NOTOPEN+'SCALE'+LANG_CONVERT_FILE2,oFormSet)
=lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'SCALE'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  RETURN .F.
ENDIF

*B131128,1 AMH Fix bug of Fabric file not found [Start]
*IF !gfOpenFile(oAriaApplication.DataDir+'FABRIC','CFABRIC','SH')
IF FILE(oAriaApplication.DataDir+'FABRIC'+'.DBF') AND !gfOpenFile(oAriaApplication.DataDir+'FABRIC','CFABRIC','SH')
*B131128,1 AMH [End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfUpdError(LANG_CONVERT_NOTOPEN+'FABRIC'+LANG_CONVERT_FILE2,oFormSet)
=lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'FABRIC'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  RETURN .F.
ENDIF

IF !gfOpenFile(oAriaApplication.DataDir+'CODES','CCODE_NO','SH')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfUpdError(LANG_CONVERT_NOTOPEN+'CODES'+LANG_CONVERT_FILE2,oFormSet)
=lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'CODES'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  RETURN .F.
ENDIF
*E038594,1 AMH [End]
*E302821,1 T20101124.0009 TMI 12/19/2010 [Start] check if the connection string ok
=oAriaApplication.mReadConStr(oAriaApplication.ActiveCompanyId,.T.)
*E302821,1 T20101124.0009 TMI 12/19/2010 [End  ]

*--Create and convert files.
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_CONVERT_CREATE NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_CREATE,oFormSet.GetHeaderText("LANG_CONVERT_CREATE",oFormSet.HeaderAlias)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]


*--Create DataBase.
IF !oAriaApplication.RemoteCompanyData.mcreatedatabase(oAriaApplication.ActiveCompanyID)
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfUpdError(LANG_CONVERT_DATABASE,oFormSet)
=lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_DATABASE,oFormSet.GetHeaderText("LANG_CONVERT_DATABASE",oFormSet.HeaderAlias)),oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  RETURN
ENDIF

*-Create Tables/Indeces for each company module.
LOCAL lnI,lnRemResult,lcSydFlFld,lcSydField,lcSydIndex,laFiles
lcSydFiles = gfTempName()
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
*!*	lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from sydfield",'',"FOXFIELD","",;
*!*	              oAriaApplication.SystemConnectionString,3,"",oFormSet.DataSessionId)
lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from sydfield where (CVER='A27' OR EMPTY(CVER))",'',"FOXFIELD","",;
              oAriaApplication.SystemConnectionString,3,"",oFormSet.DataSessionId)
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[END]              
IF lnRemResult < 1
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfUpdError(LANG_CONVERT_DICTNARY,oFormSet)
=lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_DICTNARY,oFormSet.GetHeaderText("LANG_CONVERT_DICTNARY",oFormSet.HeaderAlias)),oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  RETURN
ENDIF

SELECT FOXFIELD
=CURSORSETPROP("Buffering",3)
INDEX ON cFld_Name TAG cFld_Name

DIMENSION laFileStru[1,18]
laFileStru[1,1] = 'CFILE_NAM'
laFileStru[1,2] = 'C'
*E303030,1 BEGIN
*laFileStru[1,3] = 8
laFileStru[1,3] = oAriaApplication.FileW
*E303030,1 END oAriaApplication
laFileStru[1,4] = 0

LOCAL lnI
FOR lnI = 7 TO 16
  STORE '' TO laFileStru[1,lnI]
ENDFOR
STORE 0 TO laFileStru[1,17],laFileStru[1,18]

=gfCrtTmp('CONVFILE',@laFileStru,'cfile_nam','cfile_nam')

LOCAL lnConnectionHandlar,llInWork,llGetStructure,llGetIndeces
lnConnectionHandlar = 0

*E038137,1 Get the total number of files will be converted [Start]
SELECT SYCNVFLS
GO BOTTOM

*E038220,1 WSH Add UOM Table to the total converted files [Start]
*oFormSet.lnOverCnt = nRecNo + 1
oFormSet.lnOverCnt = nRecNo + 2
*E038220,1 WSH [End]

*E038137,1 [End]

*E038137,1 Create connection to use while conversion [Start]
PRIVATE lnConnHand,lcSqlInsert,lcSqlUpdate,lcFile_Name,oFormSet,lcErrorMsg

*E038594,1 AMH Define variables to hold how match we use the current connection
*E038594,1     and hold the default UOM Relation code [Start]
PRIVATE lnConnCnt,lcDefUOMRel,lcDefUOM
lnConnCnt = 0
lcDefUOMRel = SPACE(6)
lcDefUOM = SPACE(6)

*-- Get the Material Code structure.
PRIVATE oGetItemMask,lcItemPic,laItemSeg
oGetItemMask = CREATEOBJECT('GetItemMask')
DIMENSION laItemSeg[1,1]
lcItemPic = ''

*-- Get use ex. size scale setup.
PRIVATE llSUseExSS
llSUseExSS = gfGetMemVar('M_USEEXSSC',oAriaApplication.ActiveCompanyID)
*E038594,1 AMH [End]

lnConnHand = SQLSTRINGCONNECT(oAriaApplication.ActiveCompanyConStr)
IF lnConnHand < 1
  RETURN .F.
ENDIF
STORE "" TO lcSqlInSert,lcSqlUpdate,lcFile_Name,lcErrorMsg
oFormSet = oFormSet
*--Save any errors ocurse while converting files.
lcErr = ON('ERROR')
ON ERROR llDumy = lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+lcFile_Name+;
                             IIF(EMPTY(lcErrorMsg),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_RECNO,oFormSet.GetHeaderText("LANG_CONVERT_RECNO",oFormSet.HeaderAlias))+ALLTRIM(STR(RECNO())),lcErrorMsg)+;
                             ' '+MESSAGE()+')',oFormSet)
*E038137,1 [End]

*! E038621,1 WSH 05/11/2005, Replace the field SycComp.lRunFromA4 with .T. to indicate that programs for
*!                           this company has been converted to run from Aria4 not from 27... [Start]
LOCAL loSycComp, lcSycComp, lcTranCode
loSycComp = CREATEOBJECT("RemoteTable", "SYCCOMP", "CCOMP_ID", .F., SET("Datasession"), .F., .T.)

IF TYPE("loSycComp") = 'O' AND loSycComp.SEEK(oAriaApplication.ActiveCompanyID)
  *-- Begin Updating Transaction
  lcTranCode = oAriaApplication.RemoteSystemData.BeginTran(oAriaApplication.SystemConnectionString, 3, '', loSycComp.llNative)

  *-- Check Result for Begin Transaction
  IF TYPE('lcTranCode') = 'N'
    loSycComp = .NULL.
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfUpdError(LANG_CONVERT_SYCCOMP, oFormSet)
=lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_SYCCOMP,oFormSet.GetHeaderText("LANG_CONVERT_SYCCOMP",oFormSet.HeaderAlias)), oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

    RETURN .F.
  ENDIF

  loSycComp.REPLACE("lRunFromA4 WITH .T.")
  loSycComp.TABLEUPDATE(lcTranCode)

  *-- Commit Changes and Check Result
  lnConnHandler = oAriaApplication.RemoteSystemData.CommitTran(lcTranCode, loSycComp.llNative)
  IF lnConnHandler <> 1
    =oAriaApplication.RemoteSystemData.RollBackTran(lcTranCode, loSycComp.llNative)
    loSycComp = .NULL.
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfUpdError(LANG_CONVERT_SYCCOMP, oFormSet)
=lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_SYCCOMP,oFormSet.GetHeaderText("LANG_CONVERT_SYCCOMP",oFormSet.HeaderAlias)), oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

    RETURN .F.
  ENDIF

  loSycComp = .NULL.
ELSE
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfUpdError(LANG_CONVERT_SYCCOMP, oFormSet)
=lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_SYCCOMP,oFormSet.GetHeaderText("LANG_CONVERT_SYCCOMP",oFormSet.HeaderAlias)), oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  RETURN .F.
ENDIF
*! E038621,1 WSH 05/11/2005, [End]

SELECT SYCNVMDL
lnI = 0
SCAN FOR nFlag > 0
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_CONVERT_PREPAR+cApp_Name NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_PREPAR,oFormSet.GetHeaderText("LANG_CONVERT_PREPAR",oFormSet.HeaderAlias))+cApp_Name NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

  llInWork  = (SYCNVMDL.cAutRef = 'Converting...' OR SYCNVMDL.cAutRef = 'Reconverting...')
  STORE .T. TO llGetStructure,llGetIndeces
  lnI = lnI + 1
  oFormSet.lnCurModule = lnI
  oFormSet.AriaForm1.cntStep4.txtModule.Value = ALLTRIM(CAPP_NAME)

  *-- Start Conversion.
  REPLACE cAutRef  WITH IIF(EMPTY(cAutRef),'Converting...','Reconverting...'),;
          dDate    WITH oAriaApplication.SystemDate,;
          cCnvTime WITH TIME()
  =lfUpdCnvMdl(.T.,oFormSet)

  DIMENSION laFiles[1]
  laFiles[1] = ''
  =gfSubStr(mFile_app,@laFiles,',')

  *E038220,1 WSH Add UOM Table After the INVTYPE file [Start]
  IF !SEEK('UOM ', 'CONVFILE')
    DIMENSION laFiles[ALEN(laFiles)+1]
    =AINS(laFiles,1)
    laFiles[1] = 'UOM'
    INSERT INTO SYCNVLIB (CFILE    ,CFILE_TTL        ,LEXT_CALL,F_NAME,CFILE_TAG) VALUES;
                         ('UOM','Units of Measurment',.F.      ,'lfCnvUOM(oFormSet)','UOMCODE')
    INSERT INTO SYCNVFLS (CFILE_NAM,CFILE_TTL        ,NFLAG) VALUES;
                         ('UOM','Units of Measurment',3)
  ENDIF
  *E038220,1 WSH [End]

  *--Add the INVTYPE file in case of not converted yet to be sure it is the first file converted.
  IF !SEEK('INVTYPE ','CONVFILE')
    DIMENSION laFiles[ALEN(laFiles)+1]
    =AINS(laFiles,1)
    laFiles[1]='INVTYPE'
    INSERT INTO SYCNVLIB (CFILE    ,CFILE_TTL        ,LEXT_CALL,F_NAME,CFILE_TAG) VALUES;
                         ('INVTYPE','Inventory Types',.F.      ,'lfCnvInvTp(oFormSet)','CINVTYPE')
    INSERT INTO SYCNVFLS (CFILE_NAM,CFILE_TTL        ,NFLAG) VALUES;
                         ('INVTYPE','Inventory Types',3)
  ENDIF

  oFormSet.lnFileCnt = ALEN(laFiles)
  oFormSet.AriaForm1.cntStep4.objModulePro.FloodPercent = 0
  FOR oFormSet.lnCurFile = 1 TO oFormSet.lnFileCnt

    *E038594,1 AMH Intialize the variable used to calculate the percentage done from converting the current file [Start]
    lnPercent = 0
    *E038594,1 AMH [End]

    *E303030,1 BEGIN
    *IF SEEK(PADR(laFiles[oFormSet.lnCurFile],8),'CONVFILE')
    IF SEEK(PADR(laFiles[oFormSet.lnCurFile],oAriaApplication.FileW),'CONVFILE')
    *E303030,1 BEGIN oAriaApplication
      oFormSet.lnRecCnt = 1
      oFormSet.lnCurRec = 1
      =lfRefPro(oFormSet)
      LOOP
    ENDIF

    *E303030,1 BEGIN
    *IF SEEK(PADR(laFiles[oFormSet.lnCurFile],8),"SYDFILES")
    IF SEEK(PADR(laFiles[oFormSet.lnCurFile],oAriaApplication.FileW),"SYDFILES")
    *E303030,1 BEGIN oAriaApplication

      SET KEY TO '' IN SYDFLFLD
      SET KEY TO '' IN SYDINDEX

      *E038137,1 AMH Support custom dictionary [Start]
      LOCAL lnAlias
      lnAlias = SELECT(0)
      *! B610744,1 MMT 06/12/2014 Fix issues in the Conversion program at GMA[T20131219.0020][Start]
      *lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from sydflfld where ALLTRIM(cfile_nam)+','$'"+;
                    SYDFILES.mFlDfData+"'+',' and cUpGrdLvl='U'",'',"FOXFLFLD","",oAriaApplication.SystemConnectionString,3,;
                    "",oFormSet.DataSessionId)
      lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from sydflfld where ALLTRIM(cfile_nam) $ '"+;
                    SYDFILES.mFlDfData+"' and cUpGrdLvl='U'",'',"FOXFLFLD","",oAriaApplication.SystemConnectionString,3,;
                    "",oFormSet.DataSessionId)                    
      *! B610744,1 MMT 06/12/2014 Fix issues in the Conversion program at GMA[T20131219.0020][END]                    
      IF lnRemResult < 1
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+ALLTRIM(laFiles[oFormSet.lnCurFile])+LANG_CONVERT_STRUCTUR,oFormSet)
IF lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+ALLTRIM(laFiles[oFormSet.lnCurFile])+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_STRUCTUR,oFormSet.GetHeaderText("LANG_CONVERT_STRUCTUR",oFormSet.HeaderAlias)),oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

          RETURN
        ELSE
          llGetStructure = .F.
        ENDIF
      ENDIF

      SELECT FOXFLFLD
      SCAN
        SCATTER MEMO MEMVAR
        *E303030,1 BEGIN
        *m.cFile_Nam = PADR(laFiles[oFormSet.lnCurFile],8)
        m.cFile_Nam = PADR(laFiles[oFormSet.lnCurFile],oAriaApplication.FileW)
        *E303030,1 BEGIN oAriaApplication
        IF !SEEK(m.cFile_Nam+m.cFld_Name,'SYDFLFLD') AND SEEK(m.cFld_Name,'FOXFIELD')
          INSERT INTO SYDFLFLD FROM MEMVAR
          *! B610199,1 HIA 01/16/2013 Aria4xp-SM - Release to GL screen freezes once release is complete [T20121213.0012][Start]
          *IF !SEEK(m.cFld_Name,'SYDFIELD')
          IF !SEEK(m.cFld_Name,'AYDFIELD')          
          *! B610199,1 HIA 01/16/2013 Aria4xp-SM - Release to GL screen freezes once release is complete [T20121213.0012][END]
            SELECT FOXFIELD
            SCATTER MEMO MEMVAR
            *! B610199,1 HIA 01/16/2013 Aria4xp-SM - Release to GL screen freezes once release is complete [T20121213.0012][Start]
            *INSERT INTO SYDFIELD FROM MEMVAR
            INSERT INTO AYDFIELD FROM MEMVAR            
            *! B610199,1 HIA 01/16/2013 Aria4xp-SM - Release to GL screen freezes once release is complete [T20121213.0012][END]
          ENDIF
        ENDIF
      ENDSCAN
      *! B610744,1 MMT 06/12/2014 Fix issues in the Conversion program at GMA[T20131219.0020][Start]
*!*	      lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from sydindex where cfile_nam$'"+;
*!*	                    SYDFILES.mFlDfData+"' and cUpGrdLvl='U'",'',"FOXINDEX","",oAriaApplication.SystemConnectionString,3,;
*!*	                    "",oFormSet.DataSessionId)
      lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from sydindex where cfile_nam $ '"+;
                    SYDFILES.mFlDfData+"' and cUpGrdLvl='U'",'',"FOXINDEX","",oAriaApplication.SystemConnectionString,3,;
                    "",oFormSet.DataSessionId)
	  *! B610744,1 MMT 06/12/2014 Fix issues in the Conversion program at GMA[T20131219.0020][End]                    
      IF lnRemResult < 1
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+ALLTRIM(laFiles[oFormSet.lnCurFile])+LANG_CONVERT_STRUCTUR,oFormSet)
IF lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+ALLTRIM(laFiles[oFormSet.lnCurFile])+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_STRUCTUR,oFormSet.GetHeaderText("LANG_CONVERT_STRUCTUR",oFormSet.HeaderAlias)),oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

          RETURN
        ELSE
          llGetStructure = .F.
        ENDIF
      ENDIF

      SELECT FOXINDEX
      SCAN
        SCATTER MEMO MEMVAR
        *E303030,1 BEGIN
        *m.cFile_Nam = PADR(laFiles[oFormSet.lnCurFile],8)
        m.cFile_Nam = PADR(laFiles[oFormSet.lnCurFile],oAriaApplication.FileW)
        *E303030,1 BEGIN oAriaApplication
        IF !SEEK(m.cFile_Nam+m.cFile_tag,'SYDINDEX')
          INSERT INTO SYDINDEX FROM MEMVAR
        ENDIF
      ENDSCAN
      SELECT (lnAlias)
      *E038137,1 AMH [End]

      *E303030,1 BEGIN
*!*	      =SEEK(PADR(laFiles[oFormSet.lnCurFile],8),'SYDFLFLD')
*!*	      =SEEK(PADR(laFiles[oFormSet.lnCurFile],8),'SYDINDEX')
*!*	
*!*	      SET KEY TO PADR(laFiles[oFormSet.lnCurFile],8) IN SYDFLFLD
*!*	      SET KEY TO PADR(laFiles[oFormSet.lnCurFile],8) IN SYDINDEX

      =SEEK(PADR(laFiles[oFormSet.lnCurFile],oAriaApplication.FileW),'SYDFLFLD')
      =SEEK(PADR(laFiles[oFormSet.lnCurFile],oAriaApplication.FileW),'SYDINDEX')

      SET KEY TO PADR(laFiles[oFormSet.lnCurFile],oAriaApplication.FileW) IN SYDFLFLD
      SET KEY TO PADR(laFiles[oFormSet.lnCurFile],oAriaApplication.FileW) IN SYDINDEX

      *E303030,1 BEGIN oAriaApplication
      lcSydFlFld = "SYDFLFLD"
      *! B610199,1 HIA 01/16/2013 Aria4xp-SM - Release to GL screen freezes once release is complete [T20121213.0012][Start]
      *lcSydField = "SYDFIELD"
      lcSydField = "AYDFIELD"      
      *! B610199,1 HIA 01/16/2013 Aria4xp-SM - Release to GL screen freezes once release is complete [T20121213.0012][END]
      lcSydIndex = "SYDINDEX"

    ELSE
      *E303030,1 BEGIN
*!*	      lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from sydflfld where cfile_nam='"+;
*!*	                    PADR(laFiles[oFormSet.lnCurFile],8)+"'",'',"FOXFLFLD","",oAriaApplication.SystemConnectionString,3,;
*!*	                    "",oFormSet.DataSessionId)
      lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from sydflfld where cfile_nam='"+;
                    PADR(laFiles[oFormSet.lnCurFile],oAriaApplication.FileW)+"'",'',"FOXFLFLD","",oAriaApplication.SystemConnectionString,3,;
                    "",oFormSet.DataSessionId)

      *E303030,1 BEGIN oAriaApplication
      IF lnRemResult < 1
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+ALLTRIM(laFiles[oFormSet.lnCurFile])+LANG_CONVERT_STRUCTUR,oFormSet)
IF lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+ALLTRIM(laFiles[oFormSet.lnCurFile])+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_STRUCTUR,oFormSet.GetHeaderText("LANG_CONVERT_STRUCTUR",oFormSet.HeaderAlias)),oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

          RETURN
        ELSE
          llGetStructure = .F.
        ENDIF
      ENDIF

      *E303030,1 BEGIN
*!*	      lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from sydindex where cfile_nam='"+;
*!*	                    PADR(laFiles[oFormSet.lnCurFile],8)+"'",'',"FOXINDEX","",oAriaApplication.SystemConnectionString,3,;
*!*	                    "",oFormSet.DataSessionId)
      lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from sydindex where cfile_nam='"+;
                    PADR(laFiles[oFormSet.lnCurFile],oAriaApplication.FileW)+"'",'',"FOXINDEX","",oAriaApplication.SystemConnectionString,3,;
                    "",oFormSet.DataSessionId)

      *E303030,1 BEGIN oAriaApplication
      IF lnRemResult < 1
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+ALLTRIM(laFiles[oFormSet.lnCurFile])+LANG_CONVERT_INDICES,oFormSet)
IF lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+ALLTRIM(laFiles[oFormSet.lnCurFile])+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_INDICES,oFormSet.GetHeaderText("LANG_CONVERT_INDICES",oFormSet.HeaderAlias)),oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

          RETURN
        ELSE
          llGetIndeces = .F.
        ENDIF
      ENDIF

      lcSydFlFld = "FOXFLFLD"
      lcSydField = "FOXFIELD"
      lcSydIndex = "FOXINDEX"
    ENDIF

    *E303030,1 BEGIN
    *IF !SEEK(PADR(laFiles[oFormSet.lnCurFile],10),'SYCNVLIB')
    IF !SEEK(PADR(laFiles[oFormSet.lnCurFile],oAriaApplication.FieldW),'SYCNVLIB')
    *E303030,1 BEGIN oAriaApplication
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+ALLTRIM(laFiles[oFormSet.lnCurFile])+LANG_CONVERT_CONVERTE,oFormSet)
IF lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+ALLTRIM(laFiles[oFormSet.lnCurFile])+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_CONVERTE,oFormSet.GetHeaderText("LANG_CONVERT_CONVERTE",oFormSet.HeaderAlias)),oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

        RETURN
      ELSE

        *E038137,1 Update the number of current file converted [Start]
        oFormSet.lnCurOver = oFormSet.lnCurOver + 1

        *E303030,1 BEGIN
        *INSERT INTO CONVFILE (CFILE_NAM) VALUES (PADR(lcFile_Name,8))
        INSERT INTO CONVFILE (CFILE_NAM) VALUES (PADR(lcFile_Name,oAriaApplication.FileW))
        *E303030,1 BEGIN oAriaApplication

        IF USED("FOXFLFLD")
          USE IN FOXFLFLD
        ENDIF

        IF USED("FOXINDEX")
          USE IN FOXINDEX
        ENDIF
        *E038137,1 [End]

        LOOP
      ENDIF
    ENDIF

    SELECT SYCNVLIB
    oFormSet.AriaForm1.cntStep4.txtFile.Value = ALLTRIM(cfile_ttl)
    oFormSet.AriaForm1.cntStep4.objFilePro.FloodPercent = 0

    *E038594,1 AMH Add line in the convtime file which help on calculate the conversion performance [Start]
    =STRTOFILE(CHR(13)+CHR(10)+cFile+STR(lnPercent)+' '+TIME(),oAriaApplication.WorkDir+"ConvTime.txt",1)
    lnPercent = lnPercent + 10
    *E038594,1 AMH [End]

    LOCAL lcRemt_Name,lcFile_ttl

    lcRemt_Name = gfTempName()
    lcFile_Name = ALLTRIM(cfile)
    lcFile_ttl  = ALLTRIM(cfile_ttl)
    oFormSet.lnRecCnt = 100
    oFormSet.lnCurRec = 0

    *E303030,1 BEGIN
    *=SEEK(PADR(cfile,8),'SYCNVFLS')
    =SEEK(PADR(cfile,oAriaApplication.FileW),'SYCNVFLS')
    *E303030,1 BEGIN oAriaApplication

    *--Create Table.
    IF llGetStructure AND llGetIndeces

      *E038594,1 AMH Drop table instaed of delete its records [Start]
      IF SYCNVFLS.NFLAG = 1
        IF !lfDelFile(lcFile_Name)
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+lcFile_Name+LANG_CONVERT_NODELETE,oFormSet)
IF lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+lcFile_Name+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NODELETE,oFormSet.GetHeaderText("LANG_CONVERT_NODELETE",oFormSet.HeaderAlias)),oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

            RETURN
          ELSE
            LOOP
          ENDIF
        ENDIF
      ENDIF
      *E038594,1 AMH [End]

      *AMH [Start]
      *lnConnectionHandlar = oAriaApplication.RemoteCompanyData.mcreatetable(;
                            oAriaApplication.ActiveCompanyID,lcFile_Name,;
                            lcFile_ttl,lcSydFlFld,lcSydField,lcSydIndex,;
                            oFormSet.DataSessionID,lnConnectionHandlar)
      lnConnectionHandlar = oAriaApplication.RemoteCompanyData.mcreatetable(;
                            oAriaApplication.ActiveCompanyID,lcFile_Name,;
                            lcFile_ttl,lcSydFlFld,lcSydField,lcSydIndex,;
                            oFormSet.DataSessionID,lnConnectionHandlar,(lcFile_Name$'INVTYPE|UOM'))
      *AMH [End]

      IF EMPTY(lnConnectionHandlar)
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+ALLTRIM(laFiles[oFormSet.lnCurFile])+LANG_CONVERT_UPDSTRU,oFormSet)
IF lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+ALLTRIM(laFiles[oFormSet.lnCurFile])+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_UPDSTRU,oFormSet.GetHeaderText("LANG_CONVERT_UPDSTRU",oFormSet.HeaderAlias)),oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

          RETURN
        ELSE
          LOOP
        ENDIF
      ENDIF
      WAIT CLEAR
      SELECT SYCNVLIB
    ENDIF

    *--Refrech Prograce Bars.
    oFormSet.lnCurRec = oFormSet.lnCurRec + 1
    =lfRefPro(oFormSet)

    *--Create Index.
    IF lnConnectionHandlar # -2 AND llGetIndeces
      lnConnectionHandlar = oAriaApplication.RemoteCompanyData.mcreateIndex(;
                            oAriaApplication.ActiveCompanyID,;
                            lcFile_Name,lcFile_ttl,lcSydIndex,;
                            oFormSet.DataSessionID,lnConnectionHandlar)
      IF EMPTY(lnConnectionHandlar)
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+ALLTRIM(laFiles[oFormSet.lnCurFile])+LANG_CONVERT_UPDINDEX,oFormSet)
IF lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+ALLTRIM(laFiles[oFormSet.lnCurFile])+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_UPDINDEX,oFormSet.GetHeaderText("LANG_CONVERT_UPDINDEX",oFormSet.HeaderAlias)),oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

          RETURN
        ELSE
          LOOP
        ENDIF
      ENDIF
      WAIT CLEAR
      SELECT SYCNVLIB
    ENDIF

    *--Refrech Prograce Bars.
    oFormSet.lnCurRec = oFormSet.lnCurRec + 1
    =lfRefPro(oFormSet)

    *--Converting files.
    IF !lfConvFile(lcRemt_Name,lcFile_Name,lcSydIndex,oFormSet)
      RETURN
    ENDIF

    *E038137,1 Update the number of current file converted [Start]
    oFormSet.lnCurOver = MIN(oFormSet.lnCurOver + 1,oFormSet.lnOverCnt)
    *E038137,1 [End]

    *E303030,1 BEGIN
    *INSERT INTO CONVFILE (CFILE_NAM) VALUES (PADR(lcFile_Name,8))
    INSERT INTO CONVFILE (CFILE_NAM) VALUES (PADR(lcFile_Name,oAriaApplication.FileW))
    *E303030,1 BEGIN oAriaApplication
    IF USED("FOXFLFLD")
      USE IN FOXFLFLD
    ENDIF

    IF USED("FOXINDEX")
      USE IN FOXINDEX
    ENDIF
  ENDFOR

  *-- Conversion Complete.
  SELECT SYCNVMDL
  REPLACE cAutRef  WITH IIF(cAutRef='Converting...','Converted','Reconverted'),;
          dDate    WITH oAriaApplication.SystemDate,;
          cCnvTime WITH TIME()
  =lfUpdCnvMdl(.T.,oFormSet)

  *E038137,1 Comment the next line since we will callculate the overall per total files converted [Start]
  *oFormSet.AriaForm1.cntStep4.objOverPro.FloodPercent = INT(oFormSet.lnCurModule / oFormSet.lnModuleCnt * 100)
  *E038137,1 [End]

ENDSCAN
WAIT CLEAR
USE IN FOXFIELD

*E038137,1 Clear the connection used for conversion [Start]
ON ERROR &lcErr.
=SQLDISCONNECT(lnConnHand)
=STRTOFILE(CHR(13)+CHR(10)+STR(lnConnCnt),oAriaApplication.WorkDir+"ConvConn.txt",1)
lnConnCnt = 0
*E038137,1 [End]
SET TABLEVALIDATE TO lnTableVal
*PADR

*:*************************************************************
*: Name      : lfRefPro
*: Developer : AHMED MAHER (AMH)
*: Date      : 02/12/2004
*: Purpose   : Refresh the Prograce bars.
*:*************************************************************
FUNCTION lfRefPro
LPARAMETERS oFormSet

WITH oFormSet.AriaForm1.cntStep4
  .objFilePro.FloodPercent   = INT(oFormSet.lnCurRec / oFormSet.lnRecCnt * 100)

  *E038594,1 AMH Add line in convtime file to help on calculate the conversion performance [Start]
  IF .objFilePro.FloodPercent >= lnPercent
    =STRTOFILE(CHR(13)+CHR(10)+SYCNVLIB.cFile+STR(lnPercent)+' '+TIME(),oAriaApplication.WorkDir+"ConvTime.txt",1)
    lnPercent = lnPercent + 10
  ENDIF
  *E038594,1 AMH [End]

  .objModulePro.FloodPercent = INT(((oFormSet.lnCurFile - 1) + (oFormSet.lnCurRec / oFormSet.lnRecCnt));
                                   / oFormSet.lnFileCnt * 100)

  *E038137,1 Update the number of current file converted [Start]
  *.objOverPro.FloodPercent   = INT(((oFormSet.lnCurModule - 1) +;
                                   (((oFormSet.lnCurFile - 1) + (oFormSet.lnCurRec / oFormSet.lnRecCnt));
                                    / oFormSet.lnFileCnt)) / oFormSet.lnModuleCnt * 100)
  .objOverPro.FloodPercent   = INT(((oFormSet.lnCurOver - 1) + (oFormSet.lnCurRec / oFormSet.lnRecCnt));
                                   / oFormSet.lnOverCnt * 100)
  *E038137,1 [End]

ENDWITH

*:*************************************************************
*: Name      : lfFormInit
*: Developer : AHMED MAHER (AMH)
*: Date      : 03/14/2004
*: Purpose   : Init of the conversion form
*:*************************************************************
FUNCTION lfFormInit
LPARAMETERS oFormSet

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_CONVERT_START NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_START,oFormSet.GetHeaderText("LANG_CONVERT_START",oFormSet.HeaderAlias)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]


*--Get the Company Modules.
LOCAL lnRemResult,lnModuleCnt,laModules,lnI
DIMENSION laModules[1]

lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from syccomp where cComp_ID='"+;
              oAriaApplication.ActiveCompanyID+"'",'',"SYCCOMP","",;
              oAriaApplication.SystemConnectionString,3,"",oFormSet.DataSessionId)
IF lnRemResult >= 1
  SELECT SYCCOMP
  LOCATE
  oFormSet.lcModules = STRTRAN(STRTRAN(STRTRAN(STRTRAN(mComp_mdl,'SY|'),'SY'),'SM|'),'SM')
  =gfSubStr(oFormSet.lcModules,@laModules,'|')
  USE IN SYCCOMP
ELSE
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00408B00000','DIALOG',LANG_CONVERT_MODULES)
=gfModalGen('TRM00408B00000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULES,oFormSet.GetHeaderText("LANG_CONVERT_MODULES",oFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  RETURN .F.
ENDIF

lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from sycnvmdl where cComp_ID='"+;
              oAriaApplication.ActiveCompanyID+"'",'',"SYCNVMDL","",;
              oAriaApplication.cAria4SysFiles,3,"",oFormSet.DataSessionId)
IF lnRemResult < 1
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *=gfModalGen('TRM00408B00000','DIALOG',LANG_CONVERT_MODULES)
  =gfModalGen('TRM00408B00000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULES,oFormSet.GetHeaderText("LANG_CONVERT_MODULES",oFormSet.HeaderAlias)))
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  RETURN .F.
ENDIF

SELECT SYCNVMDL
LOCAL lnBufMode
lnBufMode = CURSORGETPROP("Buffering")
=CURSORSETPROP("Buffering",3)
INDEX ON cApp_Id TAG SYCNVMDL
=CURSORSETPROP("Buffering",lnBufMode)

DIMENSION laFileStru[4,18]
laFileStru[1,1] = 'CFILE_NAM'
laFileStru[1,2] = 'C'
*E303030,1 BEGIN
*laFileStru[1,3] = 8
laFileStru[1,3] = oAriaApplication.FileW
*E303030,1 BEGIN oAriaApplication
laFileStru[1,4] = 0

laFileStru[2,1] = 'CFILE_TTL'
laFileStru[2,2] = 'C'
laFileStru[2,3] = 34
laFileStru[2,4] = 0

laFileStru[3,1] = 'NFLAG'
laFileStru[3,2] = 'N'
laFileStru[3,3] = 1
laFileStru[3,4] = 0

laFileStru[4,1] = 'NRECNO'
laFileStru[4,2] = 'N'
laFileStru[4,3] = 6
laFileStru[4,4] = 0

LOCAL lnI,lnJ
FOR lnJ = 1 TO 4
  FOR lnI = 7 TO 16
    STORE '' TO laFileStru[lnJ,lnI]
  ENDFOR
  STORE 0 TO laFileStru[lnJ,17],laFileStru[lnJ,18]
ENDFOR

=gfCrtTmp('SYCNVFLS',@laFileStru,'cfile_nam','cfile_nam')

*--Get the Conversion information.
lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from sycnvlib where ccnvver='A27'",'',"SYCNVLIB","",;
              oAriaApplication.cAria4SysFiles,3,"",oFormSet.DataSessionId)
IF lnRemResult < 1
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00408B00000','DIALOG',LANG_CONVERT_INFORMAT)
=gfModalGen('TRM00408B00000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_INFORMAT,oFormSet.GetHeaderText("LANG_CONVERT_INFORMAT",oFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  RETURN .F.
ENDIF

SELECT SYCNVLIB
LOCAL lnBufMode
lnBufMode = CURSORGETPROP("Buffering")
=CURSORSETPROP("Buffering",3)
INDEX ON cfile TAG SYCNVLIB
=CURSORSETPROP("Buffering",lnBufMode)

lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from sycnvlln where cver='A27'",'',"SYCNVLLN","",;
              oAriaApplication.cAria4SysFiles,3,"",oFormSet.DataSessionId)
IF lnRemResult < 1
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00408B00000','DIALOG',LANG_CONVERT_INFORMAT)
=gfModalGen('TRM00408B00000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_INFORMAT,oFormSet.GetHeaderText("LANG_CONVERT_INFORMAT",oFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  RETURN .F.
ENDIF

SELECT SYCNVLLN
LOCAL lnBufMode
lnBufMode = CURSORGETPROP("Buffering")
=CURSORSETPROP("Buffering",3)
INDEX ON cfile+cfld_name TAG SYCNVLLN
=CURSORSETPROP("Buffering",lnBufMode)

*--Get the custom conversion information.
=lfGetCustConv(oFormSet)

lnModuleCnt = ALEN(laModules,1)
oFormSet.lnModuleCnt = 0
oFormSet.lcSelModules = ''
FOR lnI = 1 TO lnModuleCnt
  IF !EMPTY(laModules[lnI])
    laModules[lnI] = SUBSTR(laModules[lnI],1,2)
    lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from sydappl where capp_id='"+laModules[lnI]+"'",;
                  '',"SYDAPPL","",oAriaApplication.SystemConnectionString,3,"",oFormSet.DataSessionId)
    IF lnRemResult < 1
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00408B00000','DIALOG',LANG_CONVERT_MODULES)
=gfModalGen('TRM00408B00000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULES,oFormSet.GetHeaderText("LANG_CONVERT_MODULES",oFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      RETURN .F.
    ENDIF

    SELECT SYCNVMDL
    IF !SEEK(SYDAPPL.cApp_Id)
      APPEND BLANK
      REPLACE cComp_ID  WITH oAriaApplication.ActiveCompanyID,;
              cApp_Id   WITH SYDAPPL.cApp_Id,;
              cApp_Name WITH SYDAPPL.cApp_Name,;
              dDate     WITH oAriaApplication.SystemDate,;
              cCnvTime  WITH TIME(),;
              nFlag     WITH 1,;
              mfile_app WITH ''
    ENDIF

    SELECT SYCNVLIB
    LOCATE FOR SYCNVMDL.cApp_ID $ mFile_App
    REPLACE SYCNVMDL.lFlag WITH FOUND()
    SELECT SYCNVMDL

    oFormSet.lnModuleCnt  = oFormSet.lnModuleCnt  + 1
    oFormSet.lcSelModules = oFormSet.lcSelModules + cApp_Id + '|'

    IF !lfDefneFls(cApp_Id,(cAutRef = 'Converting...' OR cAutRef = 'Reconverting...'),oFormSet,.T.)
      SELECT SYCNVMDL
      REPLACE nFlag WITH 0
    ENDIF
  ENDIF
ENDFOR

*--Check modules instalation.
SELECT SYCNVMDL
LOCATE
IF EOF()
  *--No modules was installed for this company, Unable to run the conversion.
  =gfModalGen('TRM00322B00000','DIALOG')
  RETURN .F.
ELSE
  LOCAL lnCnt
  lnCnt = 0
  SCAN
    lnCnt = lnCnt + 1
    REPLACE cEdt_Ver WITH STR(lnCnt,2,0)
  ENDSCAN
ENDIF
=lfUpdCnvMdl(.F.,oFormSet)

*--Errors file name.
oFormSet.lcTmpErr = gfTempName()

DIMENSION laFileStru[1,18]
laFileStru[1,1] = 'cErrTxt'
laFileStru[1,2] = 'C'
laFileStru[1,3] = 100
laFileStru[1,4] = 0

LOCAL lnI
FOR lnI = 7 TO 16
  STORE '' TO laFileStru[1,lnI]
ENDFOR
STORE 0 TO laFileStru[1,17],laFileStru[1,18]

=gfCrtTmp(oFormSet.lcTmpErr,@laFileStru,'cErrTxt',oFormSet.lcTmpErr)

WAIT CLEAR

WITH oFormSet.ariaForm1.cntStep2.grdModules
  .RecordSource = 'sycnvmdl'
  .Column1.ControlSource = "sycnvmdl.nflag"
  .Column2.ControlSource = "sycnvmdl.capp_id"
  .Column3.ControlSource = "sycnvmdl.capp_name"
  .Column4.ControlSource = "sycnvmdl.cautref"
  .Column5.ControlSource = "sycnvmdl.ddate"
  .Column6.ControlSource = "sycnvmdl.ccnvtime"

  *--Set dyenamic colors.
  .SetAll("Dynamicbackcolor", "", "Column")
  .SetAll("Dynamicbackcolor","IIF(MOD(VAL(SYCNVMDL.cEdt_Ver),2)=0,16769996,16777215)", "Column")
ENDWITH

WITH oFormSet.ariaForm1.cntStep3.grdFiles
  .RecordSource = 'SYCNVFLS'
  .Column1.ControlSource = "SYCNVFLS.CFILE_TTL"
  .Column2.ControlSource = "SYCNVFLS.NFLAG"

  *--Set dyenamic colors.
  .SetAll("Dynamicbackcolor", "", "Column")
  .SetAll("Dynamicbackcolor","IIF(MOD(SYCNVFLS.NRECNO,2)=0,16769996,16777215)", "Column")
ENDWITH

oFormSet.AriaForm1.cntStep5.lstLog.RowSource = oFormSet.lcTmpErr
=lfFormRef(oFormSet)
*MMT24
IF oAriaApplication.oActiveLang.lIs_RTL
  oFormSet.AriaForm1.cntStep4.objFilePro.Left = oFormSet.AriaForm1.Width - (oFormSet.AriaForm1.cntStep4.objFilePro.Left+oFormSet.AriaForm1.cntStep4.objFilePro.Width)
  oFormSet.AriaForm1.cntStep4.objModulePro.Left = oFormSet.AriaForm1.Width - (oFormSet.AriaForm1.cntStep4.objModulePro.Left+oFormSet.AriaForm1.cntStep4.objModulePro.Width)
  oFormSet.AriaForm1.cntStep4.objOverPro.Left = oFormSet.AriaForm1.Width - (oFormSet.AriaForm1.cntStep4.objOverPro.Left+oFormSet.AriaForm1.cntStep4.objOverPro.Width)
ENDIF  
*MMT24
*:*************************************************************
*: Name      : lfFormRef
*: Developer : AHMED MAHER (AMH)
*: Date      : 03/14/2004
*: Purpose   : Refresh of the conversion form
*:*************************************************************
FUNCTION lfFormRef
LPARAMETERS oFormSet

LOCAL lnI,lcI
WITH oFormSet.ariaForm1
  FOR lnI = 1 TO 5
    lcI = STR(lnI,1)
    .cntStep&lcI..Visible = (lnI=oFormSet.lnstep)
  ENDFOR
  .cntStep4.objFilePro.Visible = (oFormSet.lnstep=4)
  .cntStep4.objModulePro.Visible = (oFormSet.lnstep=4)
  .cntStep4.objOverPro.Visible = (oFormSet.lnstep=4)
  .cmdBack.Enabled = (MOD(oFormSet.lnstep,5)-1>0)
  .cmdNext.Enabled = (MOD(oFormSet.lnstep,5)>0)
  *N000682,1 MMT 11/22/2012 Globalization changes[Start]
*!*	  .cmdNext.Caption = IIF(oFormSet.lnstep=4,'Proceed >','Next >')
*!*	  .cmdCancel.Caption = IIF(oFormSet.lnstep=5,'Finished','Cancel')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.cmdNext.Caption = IIF(oFormSet.lnstep=4,LANG_CONVERT_PROCEED_CAPTION,LANG_CONVERT_NEXT_CAPTION)
.cmdNext.Caption = IIF(oFormSet.lnstep=4,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_PROCEED_CAPTION,oFormSet.GetHeaderText("LANG_CONVERT_PROCEED_CAPTION",oFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NEXT_CAPTION,oFormSet.GetHeaderText("LANG_CONVERT_NEXT_CAPTION",oFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*.cmdCancel.Caption = IIF(oFormSet.lnstep=5,LANG_CONVERT_FINISHED_CAPTION,LANG_CONVERT_CANCEL_CAPTION)
.cmdCancel.Caption = IIF(oFormSet.lnstep=5,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FINISHED_CAPTION,oFormSet.GetHeaderText("LANG_CONVERT_FINISHED_CAPTION",oFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_CANCEL_CAPTION,oFormSet.GetHeaderText("LANG_CONVERT_CANCEL_CAPTION",oFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 11/22/2012 Globalization changes[END]
ENDWITH

*:*************************************************************
*: Name      : lfStep
*: Developer : AHMED MAHER (AMH)
*: Date      : 03/14/2004
*: Purpose   : move next/back in the wizard
*:*************************************************************
FUNCTION lfStep
LPARAMETERS lcSign,oFormSet

oFormSet.lnstep = oFormSet.lnstep &lcSign. 1
IF oFormSet.lnstep = 3
  *--Check selecting of the modules.
  SELECT SYCNVMDL
  LOCATE FOR nFlag > 0
  IF !FOUND()
    *--Nothing selected, Cannot continue.
    =gfModalGen('TRM00324B00000','DIALOG','selected')
    oFormSet.lnstep = 2
    LOCATE
    RETURN
  ENDIF
  LOCATE

  LOCAL lnCnt
  SELECT SYCNVFLS
  lnCnt = 0
  SCAN
    lnCnt = lnCnt + 1
    REPLACE NRECNO WITH lnCnt
    *E303030,1 BEGIN
    *IF ASCAN(oFormSet.laTarget,PADR(CFILE_NAM,10)) = 0
    IF ASCAN(oFormSet.laTarget,PADR(CFILE_NAM,oAriaApplication.FieldW)) = 0
    *E303030,1 BEGIN oAriaApplication
      =lfCnvFlsCrl(CFILE_NAM,'',.T.)
    ENDIF
  ENDSCAN
  LOCATE
ENDIF

IF oFormSet.lnstep = 4
  *--Check if Need to Add material scale.
  IF SEEK('BOM     ','SYCNVFLS') OR SEEK('POSLN   ','SYCNVFLS') OR SEEK('ITEM    ','SYCNVFLS')
    =gfOpenFile(oAriaApplication.DataDir+'SCALE','SCALE','SH')
    IF SEEK('S*  ','SCALE')
      IF SCALE.CNT > 1
        *--Scale * must be for only one size, Cannot continue.
        =gfModalGen('TRM00410B00000','DIALOG')
        oFormSet.lnstep = 3
        USE IN SCALE
        RETURN
      ENDIF
    ELSE
      INSERT INTO SCALE (TYPE,SCALE,CSCL_DESC,CNT,SZ1) VALUES ('S','*  ','Nosize',1,'Size1')
    ENDIF
    USE IN SCALE
  ENDIF
ENDIF

IF oFormSet.lnstep = 5
  =lfvProceed(oFormSet)

  IF !oFormSet.llErrOccur
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfUpdError(SPACE(20)+LANG_CONVERT_COMPLETE,oFormSet,.T.)
=lfUpdError(SPACE(20)+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_COMPLETE,oFormSet.GetHeaderText("LANG_CONVERT_COMPLETE",oFormSet.HeaderAlias)),oFormSet,.T.)
*N000682,1 11/20/2012 MMT Globlization changes[End]

    oFormSet.AriaForm1.cntStep5.cmdSaveLog.Enabled = .F.
  ENDIF
  *--Auto save Conv log.
  =lfvSavLog(.T.,oFormSet)
ENDIF
=lfFormRef(oFormSet)

*:*************************************************************
*: Name      : lfFormDistroy
*: Developer : AHMED MAHER (AMH)
*: Date      : 03/14/2004
*: Purpose   : Distroy the conversion program.
*:*************************************************************
FUNCTION lfFormDistroy

*--Close files on exit.
IF USED('SYCNVMDL')
  USE IN SYCNVMDL
ENDIF

IF USED('SYCNVLIB')
  USE IN SYCNVLIB
ENDIF

IF USED('SYCNVLLN')
  USE IN SYCNVLLN
ENDIF

IF USED('SYDAPPL')
  USE IN SYDAPPL
ENDIF

*!*************************************************************
*! Name      : lfRemovFls
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/15/2004
*! Purpose   : Remove files from target array.
*!*************************************************************
FUNCTION lfRemovFls
LPARAMETERS lcFileMod,oFormSet

LOCAL llRet,lnFilePos,lnTargetLen,laModules,lnI,llRemove
SELECT SYCNVLIB
llRet = .F.
SCAN FOR lcFileMod $ mFile_app
  llRet = .T.
  *E303030,1 BEGIN
  *lnFilePos = ASCAN(oFormSet.laTarget,PADR(cFile,10))
  lnFilePos = ASCAN(oFormSet.laTarget,PADR(cFile,oAriaApplication.FieldW))
  *E303030,1 BEGIN oAriaApplication
  IF lnFilePos = 0
    LOOP
  ENDIF

  llRemove = .T.
  DIMENSION laModules[1]
  laModules[1] = ''
  =gfSubStr(mFile_app,@laModules,',')

  FOR lnI = 1 TO ALEN(laModules)
    IF !EMPTY(laModules[lnI]) AND laModules[lnI] $ oFormSet.lcSelModules
      llRemove = .F.
      EXIT
    ENDIF
  ENDFOR

  IF llRemove
    =ADEL(oFormSet.laTarget,lnFilePos)
    lnTargetLen = ALEN(oFormSet.laTarget)
    IF lnTargetLen > 1
      DIMENSION oFormSet.laTarget[lnTargetLen-1]
    ENDIF
    *E303030,1 BEGIN
    *=lfCnvFlsCrl(PADR(cFile,8),'',.T.)
    =lfCnvFlsCrl(PADR(cFile,oAriaApplication.FileW),'',.T.)
    *E303030,1 BEGIN oAriaApplication
  ENDIF
ENDSCAN

SELECT SYCNVMDL
REPLACE mfile_app WITH ''
RETURN llRet

*!*************************************************************
*! Name      : lfAddFile
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/16/2004
*! Purpose   : Add file to target array.
*!*************************************************************
FUNCTION lfAddFile
LPARAMETERS lcFile,oFormSet

LOCAL lnI

SELECT SYCNVLIB
IF !SEEK(lcFile)
  RETURN .F.
ENDIF

DIMENSION laModules[1]
laModules[1] = ''
=gfSubStr(mFile_app,@laModules,',')

FOR lnI = 1 TO ALEN(laModules)
  IF !EMPTY(laModules[lnI])
    IF !(laModules[lnI] $ oFormSet.lcSelModules + 'SM|SY')
      oFormSet.lnModuleCnt = oFormSet.lnModuleCnt + 1
      oFormSet.lcSelModules = oFormSet.lcSelModules + laModules[lnI] + '|'
      IF SEEK(laModules[lnI],'SYCNVMDL')
        SELECT SYCNVMDL
        REPLACE nFlag WITH 2,;
                mfile_app WITH lcFile + ','
      ENDIF
    ELSE
      IF SEEK(laModules[lnI],'SYCNVMDL') AND !(lcFile $ SYCNVMDL.mfile_app)
        SELECT SYCNVMDL
        REPLACE mfile_app WITH mfile_app + lcFile + ','
      ENDIF
    ENDIF
  ENDIF
ENDFOR

*!*************************************************************
*! Name      : lfSetModule
*! Developer : AHMED MAHER (AMH)
*! Date      : 03/18/2004
*! Purpose   : Set complitly selected modules.
*!*************************************************************
FUNCTION lfSetModule
LPARAMETERS lcModule,oFormSet

LOCAL lcFile_App,laFiles,lnI
SELECT SYCNVLIB
lcFile_App = ''
SCAN FOR lcModule $ mFile_app
  *E303030,1 BEGIN
  *lcFile_App = lcFile_App + PADR(cFile,10) + ','
  lcFile_App = lcFile_App + PADR(cFile,oAriaApplication.FieldW) + ','
  *E303030,1 BEGIN oAriaApplication
ENDSCAN

SELECT SYCNVMDL
DIMENSION laFiles[1]
laFiles[1] = ''
=gfSubStr(mFile_app,@laFiles,',')

FOR lnI = 1 TO ALEN(laFiles)
  IF !EMPTY(laFiles[lnI])
    *E303030,1 BEGIN
    *lcFile_App = STRTRAN(lcFile_App,PADR(laFiles[lnI],10)+',')
    lcFile_App = STRTRAN(lcFile_App,PADR(laFiles[lnI],oAriaApplication.FieldW)+',')
    *E303030,1 BEGIN oAriaApplication
  ENDIF
ENDFOR

RETURN EMPTY(lcFile_App)

*:*************************************************************
*: Name      : lfGetCustConv
*: Developer : AHMED MAHER (AMH)
*: Date      : 03/21/2004
*: Purpose   : Get the custom conversion information.
*:*************************************************************
FUNCTION lfGetCustConv
LPARAMETERS oFormSet

LOCAL lnRemResult
lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from syccnvlb where ccnvver='A27'",'',"SYCCNVLB","",;
              oAriaApplication.cAria4SysFiles,3,"",oFormSet.DataSessionId)
IF lnRemResult < 1
  RETURN .F.
ENDIF

lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from syccnvll where cver='A27'",'',"SYCCNVLL","",;
              oAriaApplication.cAria4SysFiles,3,"",oFormSet.DataSessionId)
IF lnRemResult < 1
  RETURN .F.
ENDIF

IF USED('SYCCNVLB') AND !EOF('SYCCNVLB')
  SELECT SYCCNVLB
  SCAN
    SCATTER MEMVAR MEMO
    IF SEEK(m.cFile,'SYCNVLIB')
      SELECT SYCNVLIB
      GATHER MEMVAR MEMO
    ELSE
      INSERT INTO SYCNVLIB FROM MEMVAR
    ENDIF
  ENDSCAN
ENDIF
IF USED('SYCCNVLL') AND !EOF('SYCCNVLL')
  SELECT SYCCNVLL
  SCAN
    SCATTER MEMVAR MEMO
    IF SEEK(m.cFile+m.cFld_Name,'SYCNVLLN')
      SELECT SYCNVLLN
      GATHER MEMVAR MEMO
    ELSE
      INSERT INTO SYCNVLLN FROM MEMVAR
    ENDIF
  ENDSCAN
ENDIF
IF USED('SYCCNVLL')
  USE IN SYCCNVLL
ENDIF
IF USED('SYCCNVLB')
  USE IN SYCCNVLB
ENDIF

*:*************************************************************
*: Name      : lfUpdEngn
*: Developer : AHMED MAHER (AMH)
*: Date      : 03/21/2004
*: Purpose   : Engin to update.
*:*************************************************************
FUNCTION lfUpdEngn
LPARAMETERS lcRemt_Name,lcFile_Name,lcindx_exp,lcRecNo,oFormSet

*--Seek for UpNormal fields update.
LOCAL lcAField,lcSField,lcStorVal,lcUpdFld
*E303030,1 BEGIN
*IF SEEK(PADR(lcFile_Name,10),'SYCNVLLN')
IF SEEK(PADR(lcFile_Name,oAriaApplication.FieldW),'SYCNVLLN')
*E303030,1 BEGIN
  SELECT SYCNVLLN
  *E303030,1 BEGIN oAriaApplication
  *SCAN WHILE cFile=PADR(lcFile_Name,10) FOR !EMPTY(SYCNVLLN.Content)
  SCAN WHILE cFile=PADR(lcFile_Name,oAriaApplication.FieldW) FOR !EMPTY(SYCNVLLN.Content)
  *E303030,1 BEGIN oAriaApplication
    lcAField = 'm.' + ALLTRIM(cFld_name)
    lcSField = ALLTRIM(Content)
    DO CASE
      CASE SYCNVLLN.cType='A'
        &lcAField. = EVALUATE(lcSField)
      CASE SYCNVLLN.cType='V'
        lcStorVal = IIF(TYPE(lcAField)='N',VAL(SYCNVLLN.Content),;
                    IIF(TYPE(lcAField)='L',(SYCNVLLN.Content='.T.'),;
                    IIF(TYPE(lcAField)='T',CTOD(SYCNVLLN.Content),SYCNVLLN.Content)))
        &lcAField. = lcStorVal
      CASE SYCNVLLN.cType='E'
        lcUpdFld = lcAField + " = " + lcSfield
        &lcUpdFld.
    ENDCASE
  ENDSCAN
ENDIF

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*RETURN lfUpdA4Files(lcRemt_Name,lcFile_Name,SYCNVLIB.cfile_tag,lcindx_exp,LANG_CONVERT_RECNO+lcRecNo,oFormSet)
RETURN lfUpdA4Files(lcRemt_Name,lcFile_Name,SYCNVLIB.cfile_tag,lcindx_exp,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_RECNO,oFormSet.GetHeaderText("LANG_CONVERT_RECNO",oFormSet.HeaderAlias))+lcRecNo,oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]


*:*************************************************************
*: Name      : lfUpdCnvMdl
*: Developer : AHMED MAHER (AMH)
*: Date      : 03/22/2004
*: Purpose   : Update the SYCNVMDL file.
*:*************************************************************
FUNCTION lfUpdCnvMdl
LPARAMETERS llCurrOnly,oFormSet

LOCAL lnAlias,lnChange,lnRemResult
lnAlias = SELECT(0)
SELECT SYCNVMDL

IF llCurrOnly
  lnChange = 1
ELSE
  lnChange = GETNEXTMODIFIED(0)
ENDIF

DO WHILE lnChange <> 0

  IF !llCurrOnly
    *--Go to modified record.
    GOTO lnChange
  ENDIF
  SCATTER MEMVAR MEMO

  DO CASE
    *--Update -----------------------------------------------------------------
    CASE RECNO()>0
      lnRemResult = oAriaApplication.remotesystemdata.execute("update sycnvmdl set ccomp_id=?m.ccomp_id, "+;
                    "capp_id=?m.capp_id, capp_name=?m.capp_name, cautref=?m.cautref, mfile_app=?m.mfile_app, "+;
                    "ddate=?m.ddate, ccnvtime=?m.ccnvtime, "+;
                    "nflag=?m.nflag where ccomp_id=?m.ccomp_id and capp_id=?m.capp_id",;
                    '',"","",oAriaApplication.SystemConnectionString,3,"",oFormSet.DataSessionId)

    *--Insert -----------------------------------------------------------------
    CASE RECNO()<0
      lnRemResult = oAriaApplication.remotesystemdata.execute("insert into sycnvmdl "+;
                    "(ccomp_id,capp_id,capp_name,cautref,mfile_app,"+IIF(EMPTY(m.ddate),"","ddate,")+;
                    "ccnvtime,nflag) values (?m.ccomp_id,?m.capp_id,?m.capp_name,?m.cautref,"+;
                    "?m.mfile_app,?m.ddate,?m.ccnvtime,?m.nflag)",'',"","",;
                    oAriaApplication.SystemConnectionString,3,"",oFormSet.DataSessionId)

  ENDCASE

  *--Next modified record.
  IF llCurrOnly
    lnChange = 0
  ELSE
    lnChange = GETNEXTMODIFIED(lnChange)
  ENDIF
ENDDO

=TABLEUPDATE(!llCurrOnly,.T.)
IF !llCurrOnly
  LOCATE
ENDIF

SELECT (lnAlias)

*:*************************************************************
*: Name      : lfCnvFlsCrl
*: Developer : AHMED MAHER (AMH)
*: Date      : 04/08/2004
*: Purpose   : Update the SYCNVFLS file.
*:*************************************************************
FUNCTION lfCnvFlsCrl
LPARAMETERS lcFile,lcFile_Ttl,llDelete,llInWork

LOCAL lnAlias
lnAlias = SELECT(0)

SELECT SYCNVFLS
IF SEEK(lcFile)
  IF llDelete
    DELETE
  ENDIF
ELSE
  IF !llDelete
    APPEND BLANK
    REPLACE CFILE_NAM WITH lcFile,;
            CFILE_TTL WITH lcFile_Ttl,;
            NFLAG     WITH IIF(llInWork,3,1)
  ENDIF
ENDIF

SELECT (lnAlias)

*:*************************************************************
*: Name      : lfSetAllFiles
*: Developer : AHMED MAHER (AMH)
*: Date      : 04/08/2004
*: Purpose   : Set all files delete/update/none
*:*************************************************************
FUNCTION lfSetAllFiles
LPARAMETERS lnSetAll

LOCAL lnAlias
lnAlias = SELECT(0)

SELECT SYCNVFLS
REPLACE ALL NFLAG WITH lnSetAll

SELECT (lnAlias)

*:*************************************************************
*: Name      : lfDelFile
*: Developer : AHMED MAHER (AMH)
*: Date      : 04/08/2004
*: Purpose   : Delete all record in file
*:*************************************************************
FUNCTION lfDelFile
LPARAMETERS lcTable

*E038594,1 AMH Drop table instead of delete its records [Start]
*LOCAL lcConStr,lnConnectionHandlar,lnCnnResult
*lcConStr = oAriaApplication.mreadconstr(oAriaApplication.ActiveCompanyID)
*
*IF EMPTY(lcConStr)
*  RETURN .F.
*ENDIF
*
**--Open connection or retrive existing one from connection collection.
*lnConnectionHandlar = oAriaApplication.RemoteCompanyData.oConnectionsClass.Open(3,lcConStr,'SAVE',.F.)
*
*IF lnConnectionHandlar < 1
*  RETURN .F.
*ENDIF
*
**--Run SQL Execute Command.
*lnCnnResult = SQLEXEC(lnConnectionHandlar,'DELETE FROM ['+LOWER(lcTable)+']')
*
*IF lnCnnResult < 0
*  RETURN .F.
*ENDIF
*
**--Comment.
*lnCnnResult = SQLEXEC(lnConnectionHandlar,"COMMIT")
*
*IF lnCnnResult < 0
*  RETURN .F.
*ENDIF
LOCAL lnCnnResult,lnAlias
lnAlias = SELECT(0)

*--Run SQL Execute Command.
lnCnnResult = SQLEXEC(lnConnHand,"select sysobjects.name from sysobjects where sysobjects.name = '"+;
                                 LOWER(lcTable)+"'","FILE"+lcTable)
lnConnCnt = lnConnCnt + 1
*!*  IF lnConnCnt = 1000
*!*    lnConnectionHandlar = SQLDISCONNECT(lnConnHand)
*!*    =STRTOFILE(CHR(13)+CHR(10)+STR(lnConnCnt),oAriaApplication.WorkDir+"ConvConn.txt",1)
*!*    lnConnCnt = 0
*!*    lnConnHand = SQLSTRINGCONNECT(oAriaApplication.ActiveCompanyConStr)
*!*  ENDIF

IF lnCnnResult < 0
  lnCnnResult = SQLDISCONNECT(lnConnHand)
  =STRTOFILE(CHR(13)+CHR(10)+STR(lnConnCnt),oAriaApplication.WorkDir+"ConvConn.txt",1)
  lnConnCnt = 0
  lnConnHand  = SQLSTRINGCONNECT(oAriaApplication.ActiveCompanyConStr)
  lnCnnResult = SQLEXEC(lnConnHand,"select sysobjects.name from sysobjects where sysobjects.name = '"+;
                                   LOWER(lcTable)+"'","FILE"+lcTable)
  lnConnCnt = lnConnCnt + 1
  IF lnCnnResult < 0
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
ENDIF

SELECT ("FILE"+lcTable)
LOCATE
IF EOF()
  RETURN
ELSE
  USE IN ("FILE"+lcTable)
  SELECT (lnAlias)
ENDIF

lnCnnResult = SQLEXEC(lnConnHand,'DROP TABLE ['+LOWER(lcTable)+']')
lnConnCnt = lnConnCnt + 1
*!*  IF lnConnCnt = 1000
*!*    lnConnectionHandlar = SQLDISCONNECT(lnConnHand)
*!*    =STRTOFILE(CHR(13)+CHR(10)+STR(lnConnCnt),oAriaApplication.WorkDir+"ConvConn.txt",1)
*!*    lnConnCnt = 0
*!*    lnConnHand = SQLSTRINGCONNECT(oAriaApplication.ActiveCompanyConStr)
*!*  ENDIF

IF lnCnnResult < 0
  lnCnnResult = SQLDISCONNECT(lnConnHand)
  =STRTOFILE(CHR(13)+CHR(10)+STR(lnConnCnt),oAriaApplication.WorkDir+"ConvConn.txt",1)
  lnConnCnt = 0
  lnConnHand  = SQLSTRINGCONNECT(oAriaApplication.ActiveCompanyConStr)
  lnCnnResult = SQLEXEC(lnConnHand,'DROP TABLE ['+LOWER(lcTable)+']')
  lnConnCnt = lnConnCnt + 1
  IF lnCnnResult < 0
    RETURN .F.
  ENDIF
ENDIF
*E038594,1 AMH [End]

*:*************************************************************
*: Name      : lfUpdA4Files
*: Developer : AHMED MAHER (AMH)
*: Date      : 04/08/2004
*: Purpose   : Update Aria 4.0 XP files.
*:*************************************************************
FUNCTION lfUpdA4Files

*E038220,1 WSH Add parameter to check for add data without check for existance [Start]
*LPARAMETERS lcRemt_Name,lcFile_Name,lcTag_Name,lcindx_exp,lcErrMsg,oFormSet
LPARAMETERS lcRemt_Name,lcFile_Name,lcTag_Name,lcindx_exp,lcErrMsg,oFormSet,llNewRec
*E038220,1 WSH [End]

*E038220,1 WSH llNewRec will be passed as a parameter [Start]
*LOCAL llNewRec
*llNewRec = .F.
*E038220,1 WSH [End]

lcErrorMsg = lcErrMsg

*E038220,1 WSH if llNewRec = .T., don't retrieve data, add recrod without checking [Start]
IF !llNewRec
*E038220,1 WSH [End]

*--Retreive Data from Remote Table.
IF !lfOpenRemt(lcFile_Name,lcRemt_Name,lcTag_Name,lcindx_exp,oFormSet)
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+lcFile_Name+lcErrMsg+LANG_CONVERT_NORETREI,oFormSet)
IF lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+lcFile_Name+lcErrMsg+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NORETREI,oFormSet.GetHeaderText("LANG_CONVERT_NORETREI",oFormSet.HeaderAlias)),oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

    lcErrorMsg = ""
    RETURN .F.
  ENDIF

  *E038594,1 AMH Don't resume while conversion cannot retreive data [Start]
  *--Refrech Prograce Bars.
  oFormSet.lnCurRec = oFormSet.lnCurRec + 0.98
  =lfRefPro(oFormSet)
  RETURN
  *E038594,1 AMH [End]

ENDIF

*E038220,1 WSH if llNewRec = .T., add without retrieving data [Start]
ENDIF
*E038220,1 WSH [End]

*--Update Remote Cursor.
SELECT(lcRemt_Name)
LOCATE
IF EOF()
  APPEND BLANK
  llNewRec = .T.
ENDIF
GATHER MEMVAR MEMO
SCATTER MEMVAR MEMO

IF llNewRec OR SYCNVFLS.NFLAG = 2

  *E038137,1 Update SQL table with native Fox command to give the conversion process more speed [Start]
  LOCAL lcSqlCommand

  *AMH 9/8/2004 CODE USED TO FIND ERRORS IN SQL STATMENT.
*!*    DO WHILE AT('?',lcSqlInsert) > 0
*!*      lnCurrPos = AT('?',lcSqlInsert)
*!*      lcTrim    = SUBSTR(lcSqlInsert,lnCurrPos)
*!*      lnLen     = AT(',',lcTrim)
*!*      lnLen     = IIF(lnLen=0,LEN(lcTrim),lnLen)-1
*!*      lcValue   = EVALUATE(SUBSTR(lcSqlInsert,lnCurrPos+1,lnLen-1))
*!*      DO CASE
*!*        CASE TYPE('lcValue') = 'C'
*!*          lcValue = "'" + lcValue + "'"
*!*        CASE TYPE('lcValue') = 'N'
*!*          lcValue = STR(lcValue,15,3)
*!*        CASE TYPE('lcValue') = 'D'
*!*          lcValue = DTOC(lcValue)
*!*        CASE TYPE('lcValue') = 'T'
*!*          lcValue = TTOC(lcValue)
*!*        CASE TYPE('lcValue') = 'L'
*!*          lcValue = IIF(lcValue,"1","0")
*!*      ENDCASE
*!*      lcSqlInsert = STUFF(lcSqlInsert,lnCurrPos,lnLen,lcValue)
*!*    ENDDO
  *AMH9/8/2004

  lcSqlCommand = IIF(llNewRec,lcSqlInsert,lcSqlUpdate)
  *--Update Data to the Remote Table.
  *IF !lfUpdateRemt(lcRemt_Name,lcindx_exp,oFormSet)
  IF !lfUpdateRemt(lcSqlCommand)
  *E038137,1 [End]

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+lcFile_Name+lcErrMsg+LANG_CONVERT_NOUPDATA,oFormSet)
IF lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+lcFile_Name+lcErrMsg+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOUPDATA,oFormSet.GetHeaderText("LANG_CONVERT_NOUPDATA",oFormSet.HeaderAlias)),oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

      lcErrorMsg = ""
      RETURN .F.
    ENDIF
  ENDIF
ENDIF

*--Release memory variables.
SELECT(lcRemt_Name)
SCATTER BLANK MEMVAR MEMO
lcErrorMsg = ""

*--Refrech Prograce Bars.
oFormSet.lnCurRec = oFormSet.lnCurRec + 0.98
=lfRefPro(oFormSet)

*:*************************************************************
*: Name      : lfSqlInsert
*: Developer : AHMED MAHER (AMH)
*: Date      : 06/17/2004
*: Purpose   : Create the Insert Sql statment.
*:*************************************************************
*E038137,1 AMH
FUNCTION lfSqlInsert
LPARAMETERS lcFile_Name

LOCAL lnAlias,lnConnectionHandler
lnAlias = SELECT(0)
lcSqlInsert = "insert into [" + LOWER(lcFile_Name) + "] "

lnConnectionHandler = SQLEXEC(lnConnHand,"select syscolumns.name from syscolumns,sysobjects where sysobjects.name = '"+LOWER(lcFile_Name)+"' and sysobjects.id = syscolumns.id","COL"+lcFile_Name)

*E038594,1 AMH Disconnect the connection after using it 1000 times [Start]
lnConnCnt = lnConnCnt + 1
*!*  IF lnConnCnt = 1000
*!*    lnConnectionHandlar = SQLDISCONNECT(lnConnHand)
*!*    =STRTOFILE(CHR(13)+CHR(10)+STR(lnConnCnt),oAriaApplication.WorkDir+"ConvConn.txt",1)
*!*    lnConnCnt = 0
*!*    lnConnHand = SQLSTRINGCONNECT(oAriaApplication.ActiveCompanyConStr)
*!*  ENDIF
*E038594,1 AMH [End]

IF lnConnectionHandler < 0

  *E038594,1 AMH Disconnect the connection after error occured then try with new one [Start]
  *RETURN .F.
  lnConnectionHandlar = SQLDISCONNECT(lnConnHand)
  =STRTOFILE(CHR(13)+CHR(10)+STR(lnConnCnt),oAriaApplication.WorkDir+"ConvConn.txt",1)
  lnConnCnt = 0
  lnConnHand = SQLSTRINGCONNECT(oAriaApplication.ActiveCompanyConStr)
  lnConnectionHandlar = SQLEXEC(lnConnHand,"select syscolumns.name from syscolumns,sysobjects where sysobjects.name = '"+LOWER(lcFile_Name)+"' and sysobjects.id = syscolumns.id","COL"+lcFile_Name)
  lnConnCnt = lnConnCnt + 1
  IF lnConnectionHandlar < 0
    RETURN .F.
  ENDIF
  *E038594,1 AMH [End]

ENDIF

LOCAL lcFields,lcValues,llFirstField
STORE "" TO lcFields,lcValues
SELECT ("COL"+lcFile_Name)
llFirstField = .T.
SCAN

  *AMH [Start]
  IF UPPER(ALLTRIM(name))=='REC_NO'
    LOOP
  ENDIF
  *AMH [End]

  lcFields = lcFields + IIF(llFirstField,"",",") + "[" + ALLTRIM(name) + "]"
  lcValues = lcValues + IIF(llFirstField,"",",") + "?m." + ALLTRIM(name)
  llFirstField = .F.
ENDSCAN
lcSqlInsert = lcSqlInsert + "(" + lcFields + ") values (" + lcValues + ")"

USE IN ("COL"+lcFile_Name)
SELECT (lnAlias)
RETURN .T.

*:*************************************************************
*: Name      : lfSqlUpdate
*: Developer : AHMED MAHER (AMH)
*: Date      : 06/17/2004
*: Purpose   : Create the Update Sql statment.
*:*************************************************************
*E038137,1 AMH
FUNCTION lfSqlUpdate
LPARAMETERS lcFile_Name,lcIndx_Exp,lcTagName

LOCAL lnAlias,lnConnectionHandler
lnAlias = SELECT(0)

*E038594,1 AMH Disconnect the connection after using it 1000 times [Start]
= SQLEXEC(lnConnHand,"DROP FUNCTION "+ALLTRIM(LOWER(lcFile_Name))+"fun")
lnConnCnt = lnConnCnt + 1
*!*  IF lnConnCnt = 1000
*!*    lnConnectionHandlar = SQLDISCONNECT(lnConnHand)
*!*    =STRTOFILE(CHR(13)+CHR(10)+STR(lnConnCnt),oAriaApplication.WorkDir+"ConvConn.txt",1)
*!*    lnConnCnt = 0
*!*    lnConnHand = SQLSTRINGCONNECT(oAriaApplication.ActiveCompanyConStr)
*!*  ENDIF
*E038594,1 AMH [End]

lnConnectionHandler = SQLEXEC(lnConnHand,"select syscolumns.name,syscolumns.usertype,syscolumns.prec,syscolumns.scale"+;
                                         " from syscolumns,sysobjects where sysobjects.name = '"+LOWER(lcFile_Name)+;
                                         "' and sysobjects.id = syscolumns.id","COL"+lcFile_Name)

*E038594,1 AMH Disconnect the connection after using it 1000 times [Start]
lnConnCnt = lnConnCnt + 1
*!*  IF lnConnCnt = 1000
*!*    lnConnectionHandlar = SQLDISCONNECT(lnConnHand)
*!*    =STRTOFILE(CHR(13)+CHR(10)+STR(lnConnCnt),oAriaApplication.WorkDir+"ConvConn.txt",1)
*!*    lnConnCnt = 0
*!*    lnConnHand = SQLSTRINGCONNECT(oAriaApplication.ActiveCompanyConStr)
*!*  ENDIF
*E038594,1 AMH [End]

IF lnConnectionHandler < 0

  *E038594,1 AMH Disconnect the connection after error occured then try with another new one [Start]
  *RETURN .F.
  lnConnectionHandlar = SQLDISCONNECT(lnConnHand)
  =STRTOFILE(CHR(13)+CHR(10)+STR(lnConnCnt),oAriaApplication.WorkDir+"ConvConn.txt",1)
  lnConnCnt = 0
  lnConnHand = SQLSTRINGCONNECT(oAriaApplication.ActiveCompanyConStr)
  lnConnectionHandler = SQLEXEC(lnConnHand,"select syscolumns.name,syscolumns.usertype,syscolumns.prec,syscolumns.scale"+;
                                           " from syscolumns,sysobjects where sysobjects.name = '"+LOWER(lcFile_Name)+;
                                           "' and sysobjects.id = syscolumns.id","COL"+lcFile_Name)
  lnConnCnt = lnConnCnt + 1
  IF lnConnectionHandlar < 0
    RETURN .F.
  ENDIF
  *E038594,1 AMH [End]

ENDIF
SELECT ("COL"+lcFile_Name)

LOCAL lcSqlCommand,lcWhereCond,llFirstField,lnI
LOCAL ARRAY laFields[1]

STORE "" TO lcSqlCommand,lcWhereCond,lcParameters,lcVarType,lcParaVal
oAriaApplication.RemoteCompanyData.mindexfields(lcindx_exp,@laFields)

lcSqlCommand = "CREATE FUNCTION "+ALLTRIM(LOWER(lcFile_Name))+"fun ("
llFirstField = .T.
FOR lnI = 1 TO ALEN(laFields)
  LOCATE FOR name = PADR(LOWER(laFields[lnI]),128)
  IF FOUND()
    DO CASE
      CASE usertype = 1
        lcVarType = "char(" + ALLTRIM(STR(prec)) + ")"
      CASE usertype = 10
        lcVarType = "numeric(" + ALLTRIM(STR(prec)) + "," + ALLTRIM(STR(scale)) + ")"
      CASE usertype = 12
        lcVarType = "datetime"
      CASE usertype = 16
        lcVarType = "bit"
      CASE usertype = 19
        lcVarType = "text"
    ENDCASE
    lcParameters = lcParameters+IIF(llFirstField,"",",")+"@"+LOWER(laFields[lnI])+"var " + lcVarType
    lcParaVal    = lcParaVal+IIF(llFirstField,"",",")+"?m."+LOWER(laFields[lnI])
    lcWhereCond  = lcWhereCond+IIF(llFirstField,""," and ")+"["+LOWER(laFields[lnI])+"] = @"+LOWER(laFields[lnI])+"var"
    llFirstField = .F.
  ENDIF
ENDFOR
*E302821,1 TMI 12/26/2010 [Start] replace the hint: index=<lcTagName> BY with(index(<indexTag>))
*lcSqlCommand = lcSqlCommand + lcParameters + ") RETURNS TABLE AS RETURN (SELECT * FROM " + " ["+LOWER(lcFile_Name) +;
                                             "] (index=" + lcTagName + ") WHERE " + lcWhereCond + ")"
lcSqlCommand = lcSqlCommand + lcParameters + ") RETURNS TABLE AS RETURN (SELECT * FROM " + " ["+LOWER(lcFile_Name) +;
                                             "] WITH(index(" + lcTagName + ")) WHERE " + lcWhereCond + ")"
*E302821,1 TMI 12/26/2010 [End  ]
lnConnectionHandler = SQLEXEC(lnConnHand,lcSqlCommand)

*E038594,1 AMH Disconnect the connection after using it 1000 times [Start]
lnConnCnt = lnConnCnt + 1
*!*  IF lnConnCnt = 1000
*!*    lnConnectionHandlar = SQLDISCONNECT(lnConnHand)
*!*    =STRTOFILE(CHR(13)+CHR(10)+STR(lnConnCnt),oAriaApplication.WorkDir+"ConvConn.txt",1)
*!*    lnConnCnt = 0
*!*    lnConnHand = SQLSTRINGCONNECT(oAriaApplication.ActiveCompanyConStr)
*!*  ENDIF
*E038594,1 AMH [End]

IF lnConnectionHandler < 0

  *E038594,1 AMH Disconnect the connection after error occured then try with new one [Start]
  *RETURN .F.
  lnConnectionHandlar = SQLDISCONNECT(lnConnHand)
  =STRTOFILE(CHR(13)+CHR(10)+STR(lnConnCnt),oAriaApplication.WorkDir+"ConvConn.txt",1)
  lnConnCnt = 0
  lnConnHand = SQLSTRINGCONNECT(oAriaApplication.ActiveCompanyConStr)
  lnConnectionHandler = SQLEXEC(lnConnHand,lcSqlCommand)
  lnConnCnt = lnConnCnt + 1
  IF lnConnectionHandlar < 0
    RETURN .F.
  ENDIF
  *E038594,1 AMH [End]

ENDIF

lcSqlUpdate = "update "+ALLTRIM(LOWER(lcFile_Name))+"fun("+lcParaVal+") set "
LOCAL lcFields
STORE "" TO lcFields
SELECT ("COL"+lcFile_Name)
llFirstField = .T.
SCAN

  *AMH [Start]
  IF UPPER(ALLTRIM(name))=='REC_NO'
    LOOP
  ENDIF
  *AMH [End]

  lcFields = lcFields + IIF(llFirstField,"",",") + "[" + ALLTRIM(name) + "]=?m." + ALLTRIM(name)
  llFirstField = .F.
ENDSCAN
lcSqlUpdate = lcSqlUpdate + lcFields

USE IN ("COL"+lcFile_Name)
SELECT (lnAlias)
RETURN .T.

***************************************************************
***                  Special Handling Files                 ***
***************************************************************

*:*************************************************************
*: Name      : lfConvBom
*: Developer : AHMED MAHER (AMH)
*: Date      : 02/10/2004
*: Purpose   : Convert BOMHEADR file.
*:*************************************************************
FUNCTION lfConvBom
LPARAMETERS oFormSet

LOCAL lcBomHeadr
lcBomHeadr    = gfTempName()

*E038594,1 AMH Don't Open the Style file here to give conversion more speed [Start]
*IF !gfOpenFile(oAriaApplication.DataDir+'STYLE','CSTYLE','SH')
*  =lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+'BOMHEADR'+LANG_CONVERT_NOTOPEN+'STYLE'+;
*              LANG_CONVERT_FILE2,oFormSet)
*  RETURN .F.
*ENDIF
*E038594,1 AMH [End]

IF !gfOpenFile(oAriaApplication.DataDir+'BOM','BOM','SH')
  =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'BOMHEADR'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'BOM'+;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]


  *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
  *RETURN .F.
  RETURN .T.
  *B038828,1 AMH [End]
ENDIF

SELECT DISTINCT CITMMAJOR,LMATERIAL FROM BOM INTO CURSOR BOMCOUNT
SELECT BOMCOUNT
COUNT TO oFormSet.lnRecCnt
oFormSet.lnCurRec = oFormSet.lnCurRec * oFormSet.lnRecCnt / 100
LOCAL lcType,lcCstShtTyp,lcCstSht_ID,lcItmMajor
STORE ' ' TO lcCstShtTyp,lcCstSht_ID,lcItmMajor

SELECT BOM
SCAN
  =SEEK(CITMMAJOR,'STYLE')
  lcType = TYP
  m.ccstshttyp = IIF(LMATERIAL,'T',IIF(STYLE.MAKE,'M','I'))
  m.ccstsht_id = 'DEFCST'
  m.citmmajor  = CITMMAJOR
  IF m.ccstshttyp + m.ccstsht_id + m.citmmajor <> PADR(lcCstShtTyp + lcCstSht_ID + lcItmMajor,26)
    IF !EMPTY(lcCstShtTyp + lcCstSht_ID + lcItmMajor)
      m.ccstshttyp = lcCstShtTyp
      m.ccstsht_id = lcCstSht_ID
      m.citmmajor  = lcItmMajor
      IF !lfUpdA4Files(lcBomHeadr,"BOMHEADR","BOMHEADR","CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID",;
         IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_STYMAJOR,oFormSet.GetHeaderText("LANG_CONVERT_STYMAJOR",oFormSet.HeaderAlias))+m.citmmajor+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_TYPE,oFormSet.GetHeaderText("LANG_CONVERT_TYPE",oFormSet.HeaderAlias))+m.ccstshttyp,oFormSet)
        RETURN .F.
      ENDIF
    ENDIF
    SELECT BOM
    m.ccstshttyp = IIF(LMATERIAL,'T',IIF(STYLE.MAKE,'M','I'))
    m.ccstsht_id = 'DEFCST'
    m.citmmajor  = CITMMAJOR
    STORE 0 TO m.ncost1,m.ncost2,m.ncost3,m.ncost4,m.ncost5,m.ncost6,m.ncost7,m.totcost
  ENDIF

  lcCstShtTyp  = m.ccstshttyp
  lcCstSht_ID  = m.ccstsht_id
  lcItmMajor   = m.citmmajor
  m.ccstshtdsc = 'Default cost sheet'
  m.ldefcstsht = .T.
  m.cstatus    = 'A'
  m.lbasonsiz  = LBASONSIZ
  m.ncost&lcType. = EVALUATE('m.ncost'+lcType) + TOTCOST
  m.totcost    = m.totcost + TOTCOST
  m.cinvtype   = IIF(LMATERIAL,'0002','0001')

  m.cEdit_User = IIF(TYPE('STYLE.cEdit_User')#'U',STYLE.cEdit_User,'')
  m.dEdit_Date = IIF(TYPE('STYLE.dEdit_Date')#'U',STYLE.dEdit_Date,'')
  m.cEdit_Time = IIF(TYPE('STYLE.cEdit_Time')#'U',STYLE.cEdit_Time,'')
  m.cEdt_Ver   = IIF(TYPE('STYLE.cEdt_Ver')#'U',STYLE.cEdt_Ver,'')

  m.cAdd_User = IIF(TYPE('STYLE.cAdd_User')#'U',STYLE.cAdd_User,'')
  m.dAdd_Date = IIF(TYPE('STYLE.dAdd_Date')#'U',STYLE.dAdd_Date,'')
  m.cAdd_Time = IIF(TYPE('STYLE.cAdd_Time')#'U',STYLE.cAdd_Time,'')
  m.cAdd_Ver  = IIF(TYPE('STYLE.cAdd_Ver')#'U',STYLE.cAdd_Ver,'')
ENDSCAN

IF !EMPTY(lcCstShtTyp + lcCstSht_ID + lcItmMajor)
  m.ccstshttyp = lcCstShtTyp
  m.ccstsht_id = lcCstSht_ID
  m.citmmajor  = lcItmMajor
  IF !lfUpdA4Files(lcBomHeadr,"BOMHEADR","BOMHEADR","CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID",;
     IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_STYMAJOR,oFormSet.GetHeaderText("LANG_CONVERT_STYMAJOR",oFormSet.HeaderAlias))+m.citmmajor+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_TYPE,oFormSet.GetHeaderText("LANG_CONVERT_TYPE",oFormSet.HeaderAlias))+m.ccstshttyp,oFormSet)
    RETURN .F.
  ENDIF
ENDIF

*E038594,1 AMH Don't close the Style file to give conversion more speed [Start]
*IF USED('Style')
*  USE IN Style
*ENDIF
*E038594,1 AMH [End]

IF USED('Bom')
  USE IN Bom
ENDIF

IF USED('BomCount')
  USE IN BomCount
ENDIF

IF USED(lcBomHeadr)
  USE IN (lcBomHeadr)
ENDIF

*:*************************************************************
*: Name      : lfCnvPoHd
*: Developer : AHMED MAHER (AMH)
*: Date      : 03/23/2004
*: Purpose   : Convert POSHDR file.
*:*************************************************************
FUNCTION lfCnvPoHd
LPARAMETERS oFormSet

LOCAL lcPosHdr,llUsePosLn,llUseCutTktL,llUseStyle,llUseFabric,llUseCutTktH,llUsePosHdr,llUsePofHdr,llUseMMfgOrdH,;
      llUseMaRwHdr

lcPosHdr = gfTempName()
STORE .F. to llUsePosLn,llUseCutTktL,llUseStyle,llUseFabric,llUseCutTktH,llUsePosHdr,llUsePofHdr,llUseMMfgOrdH,;
             llUseMaRwHdr

IF FILE(oAriaApplication.DataDir+'POSHDR'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'POSHDR','POSHDR','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'POSHDR'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'POSHDR'+;
       IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]


    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUsePosHdr = .T.
  ENDIF
ENDIF

IF FILE(oAriaApplication.DataDir+'CUTTKTH'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'CUTTKTH','CUTTKTH','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'POSHDR'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'CUTTKTH'+;
     IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUseCutTktH = .T.
  ENDIF
ENDIF

IF FILE(oAriaApplication.DataDir+'POFHDR'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'POFHDR','POFHDR','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'POSHDR'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'POFHDR'+;
       IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUsePofHdr = .T.
  ENDIF
ENDIF

IF FILE(oAriaApplication.DataDir+'MMFGORDH'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'MMFGORDH','MMFGORDH','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'POSHDR'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'MMFGORDH'+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUseMMfgOrdH = .T.
  ENDIF
ENDIF

IF FILE(oAriaApplication.DataDir+'MARWHDR'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'MARWHDR','MARWHDR','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'POSHDR'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'MARWHDR'+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUseMaRwHdr = .T.
  ENDIF
ENDIF

IF (llUseMMfgOrdH OR llUseMaRwHdr) AND FILE(oAriaApplication.DataDir+'FABRIC'+'.DBF')

  *E038594,1 AMH Don't Open the Fabric file here to give conversion more speed [Start]
  *IF !gfOpenFile(oAriaApplication.DataDir+'FABRIC','CFABRIC','SH')
  *  =lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+'POSHDR'+LANG_CONVERT_NOTOPEN+'FABRIC'+;
  *              LANG_CONVERT_FILE2,oFormSet)
  *  RETURN .F.
  *ELSE
    llUseFabric = .T.
  *ENDIF
  *E038594,1 AMH [End]

ENDIF

IF llUseCutTktH AND FILE(oAriaApplication.DataDir+'STYLE'+'.DBF')

  *E038594,1 AMH Don't Open the Style file here to give conversion more speed [Start]
  *IF !gfOpenFile(oAriaApplication.DataDir+'STYLE','CSTYLE','SH')
  *  =lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+'POSHDR'+LANG_CONVERT_NOTOPEN+'STYLE'+;
  *              LANG_CONVERT_FILE2,oFormSet)
  *  RETURN .F.
  *ELSE
    llUseStyle = .T.
  *ENDIF
  *E038594,1 AMH [End]

ENDIF

IF llUseCutTktH AND FILE(oAriaApplication.DataDir+'CUTTKTL'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'CUTTKTL','CUTTKTL','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'POSHDR'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'CUTTKTL'+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUseCutTktL = .T.
  ENDIF
ENDIF

IF llUsePosHdr AND FILE(oAriaApplication.DataDir+'POSLN'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'POSLN','POSLN','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'POSHDR'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'POSLN'+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUsePosLn = .T.
  ENDIF
ENDIF

IF !llUsePosHdr AND !llUseCutTktH AND !llUsePofHdr AND !llUseMMfgOrdD AND !llUseMaRwHdr

  *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
  *RETURN .F.
  RETURN .T.
  *B038828,1 AMH [End]

ENDIF

LOCAL lnPosRec,lnCutRec,lnPofRec,lnMfgRec,lnMarRec
STORE 0 TO lnPosRec,lnCutRec,lnPofRec,lnMfgRec,lnMarRec

IF llUsePosHdr
  SELECT POSHDR
  COUNT TO lnPosRec
ENDIF

IF llUseCutTktH
  SELECT CUTTKTH
  COUNT TO lnCutRec
ENDIF

IF llUsePofHdr
  SELECT POFHDR
  COUNT TO lnPofRec
ENDIF

IF llUseMMfgOrdH
  SELECT MMFGORDH
  COUNT TO lnMfgRec
ENDIF

IF llUseMaRwHdr
  SELECT MARWHDR
  COUNT TO lnMarRec
ENDIF

oFormSet.lnRecCnt = lnPosRec + lnCutRec + lnPofRec + lnMfgRec + lnMarRec
oFormSet.lnCurRec = oFormSet.lnCurRec * oFormSet.lnRecCnt / 100

IF llUsePosHdr
  SELECT POSHDR
  SCAN

    SCATTER MEMVAR MEMO

    *B038828,1 AMH Check if POSLN file is ussed [Start]
    *=SEEK(m.cStyType+m.PO,'PosLn')
    =IIF(llUsePosLn,SEEK(m.cStyType+m.PO,'PosLn'),.T.)
    *B038828,1 AMH [End]

    *--Retreive Data from POSHDR file.
    m.cbusdocu   = SUBSTR('PPPRCN',AT(m.cstytype,'PDARCN'),1)
    m.cstytype   = IIF(m.cstytype='R','P',m.cstytype)
    m.style      = ''
    m.cInvType   = ''
    m.season     = ''
    m.ini_comp   = {}
    m.pcs_canold = 0
    m.recflag    = ''
    m.pattern    = ''
    m.del_date   = {}
    m.tkttype    = ''
    m.bundle     = ''
    m.crequis    = ''
    m.ccont_code = ''
    m.crevno     = ''
    m.cmrp       = ''
    m.nicost6    = 0
    m.nicost7    = 0
    m.nact_cost6 = 0
    m.nact_cost7 = 0
    m.nlan_cost6 = 0
    m.nlan_cost7 = 0
    m.nfcost6    = 0
    m.nfcost7    = 0
    m.nfactcost6 = 0
    m.nfactcost7 = 0
    m.nflancost6 = 0
    m.nflancost7 = 0

    *B038828,1 AMH Check if POSLN file is ussed [Start]
    *m.cstygrade  = POSLN.CSTYGRADE
    m.cstygrade  = IIF(llUsePosLn,POSLN.CSTYGRADE,'1')
    *B038828,1 AMH [End]

    IF !lfUpdA4Files(lcPosHdr,"POSHDR","POSHDR","CBUSDOCU+CSTYTYPE+PO",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_WORKORD,oFormSet.GetHeaderText("LANG_CONVERT_WORKORD",oFormSet.HeaderAlias))+m.po+;
     IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_TYPE,oFormSet.GetHeaderText("LANG_CONVERT_TYPE",oFormSet.HeaderAlias))+m.cbusdocu+m.cstytype,oFormSet)
      RETURN .F.
    ENDIF
  ENDSCAN
ENDIF

IF llUseCutTktH
  SELECT CUTTKTH
  SCAN
    SCATTER MEMVAR MEMO
    =SEEK(m.style,'STYLE')
    m.lastline = 0

    *B038828,1 AMH Check if CUTTKTL file is ussed [Start]
    *IF SEEK(m.cuttkt,'CUTTKTL')
    IF llUseCutTktL AND SEEK(m.cuttkt,'CUTTKTL')
    *B038828,1 AMH [End]

      SELECT CUTTKTL
      SCAN REST WHILE CUTTKT+STYLE+DYELOT+TRANCD = m.cuttkt FOR TRANCD = '1'
        m.lastline = MAX(LINENO,m.lastline)
      ENDSCAN
    ENDIF

    *--Retreive Data from POSHDR file.
    m.cInvType   = '0001'
    m.Start      = m.Entered
    m.cbusdocu   = 'P'
    m.cstytype   = 'U'
    m.po         = m.cuttkt
    m.cpurcode   = STYLE.CPURCODE
    m.available  = m.complete
    m.rate       = 1
    m.pocancel   = oAriaApplication.SystemDate
    m.nicost1    = m.nest_cost1
    m.nicost2    = m.nest_cost2
    m.nicost3    = m.nest_cost3
    m.nicost4    = m.nest_cost4
    m.nicost5    = m.nest_cost5
    m.pototal    = m.nest_cost1 + m.nest_cost2 + m.nest_cost3 + m.nest_cost4 + m.nest_cost5
    m.nstyorder  = m.pcs_bud
    m.receive    = m.pcs_rec
    m.damage     = m.pcs_dam
    m.cancel     = m.pcs_can
    m.open       = m.pcs_opn
    m.ntot_cost  = m.nlan_cost1 + m.nlan_cost2 + m.nlan_cost3 + m.nlan_cost4 + m.nlan_cost5
    m.cpricecur  = oAriaApplication.BaseCurrency
    m.cdutycur   = oAriaApplication.BaseCurrency
    m.npricerat  = 1
    m.ndutyrat   = 1
    m.nfcost1    = m.nest_cost1
    m.nfcost2    = m.nest_cost2
    m.nfcost3    = m.nest_cost3
    m.nfcost4    = m.nest_cost4
    m.nfcost5    = m.nest_cost5
    m.nfactcost1 = m.nact_cost1
    m.nfactcost2 = m.nact_cost2
    m.nfactcost3 = m.nact_cost3
    m.nfactcost4 = m.nact_cost4
    m.nfactcost5 = m.nact_cost5
    m.nflancost1 = m.nlan_cost1
    m.nflancost2 = m.nlan_cost2
    m.nflancost3 = m.nlan_cost3
    m.nflancost4 = m.nlan_cost4
    m.nflancost5 = m.nlan_cost5
    m.ncurrunit  = 1
    m.ndcurunit  = 1
    m.nicost6    = 0
    m.nicost7    = 0
    m.nact_cost6 = 0
    m.nact_cost7 = 0
    m.nlan_cost6 = 0
    m.nlan_cost7 = 0
    m.nfcost6    = 0
    m.nfcost7    = 0
    m.nfactcost6 = 0
    m.nfactcost7 = 0
    m.nflancost6 = 0
    m.nflancost7 = 0
    m.stygrade   = STYLE.CSTYGRADE
    m.cinvtype   = '0001'

    IF !lfUpdA4Files(lcPosHdr,"POSHDR","POSHDR","CBUSDOCU+CSTYTYPE+PO",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_WORKORD,oFormSet.GetHeaderText("LANG_CONVERT_WORKORD",oFormSet.HeaderAlias))+m.po+;
     IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_TYPE,oFormSet.GetHeaderText("LANG_CONVERT_TYPE",oFormSet.HeaderAlias))+m.cbusdocu+m.cstytype,oFormSet)
      RETURN .F.
    ENDIF
  ENDSCAN
ENDIF

IF llUsePofHdr
  SELECT POFHDR
  SCAN
    SCATTER MEMVAR MEMO

    *--Retreive Data from POSHDR file.
    m.cbusdocu   = m.cmattype
    m.cstytype   = 'M'
    m.po         = m.pomat
    m.Status     = IIF(m.Status='L','S',m.Status)
    m.available  = m.complete
    m.nicost1    = m.necost1
    m.nicost2    = m.necost2
    m.nicost3    = m.necost3
    m.nicost4    = m.necost4
    m.nstyorder  = m.nfaborder
    m.receive    = m.nfbreceive
    m.damage     = m.nfabdamage
    m.cancel     = m.nfabcancel
    m.open       = m.npo_open
    m.nfactcost1 = m.nact_cost1
    m.nfactcost2 = m.nact_cost2
    m.nfactcost3 = m.nact_cost3
    m.nfactcost4 = m.nact_cost4
    m.nflancost1 = m.nlan_cost1
    m.nflancost2 = m.nlan_cost2
    m.nflancost3 = m.nlan_cost3
    m.nflancost4 = m.nlan_cost4
    m.nact_cost1 = m.neactcost1
    m.nact_cost2 = m.neactcost2
    m.nact_cost3 = m.neactcost3
    m.nact_cost4 = m.neactcost4
    m.nlan_cost1 = m.nelancost1
    m.nlan_cost2 = m.nelancost2
    m.nlan_cost3 = m.nelancost3
    m.nlan_cost4 = m.nelancost4
    m.ntot_cost  = m.nlan_cost1 + m.nlan_cost2 + m.nlan_cost3 + m.nlan_cost4
    m.lmultiware = .F.
    m.nfcost1    = m.ncost1
    m.nfcost2    = m.ncost2
    m.nfcost3    = m.ncost3
    m.nfcost4    = m.ncost4
    m.nicost6    = 0
    m.nicost7    = 0
    m.nact_cost6 = 0
    m.nact_cost7 = 0
    m.nlan_cost6 = 0
    m.nlan_cost7 = 0
    m.nfcost6    = 0
    m.nfcost7    = 0
    m.nfactcost6 = 0
    m.nfactcost7 = 0
    m.nflancost6 = 0
    m.nflancost7 = 0
    m.stygrade   = m.cfabgrade

    IF !lfUpdA4Files(lcPosHdr,"POSHDR","POSHDR","CBUSDOCU+CSTYTYPE+PO",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_WORKORD,oFormSet.GetHeaderText("LANG_CONVERT_WORKORD",oFormSet.HeaderAlias))+m.po+;
     IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_TYPE,oFormSet.GetHeaderText("LANG_CONVERT_TYPE",oFormSet.HeaderAlias))+m.cbusdocu+m.cstytype,oFormSet)
      RETURN .F.
    ENDIF
  ENDSCAN
ENDIF

IF llUseMMfgOrdH
  SELECT MMFGORDH
  SCAN
    SCATTER MEMVAR MEMO

    *B131128,1 AMH Fix bug of Fabric file not found [Start]
    *=SEEK(m.cfabric,'FABRIC')
    =llUseFabric AND SEEK(m.cfabric,'FABRIC')
    *B131128,1 AMH [End]

    *--Retreive Data from POSHDR file.
    m.cbusdocu   = 'P'
    m.cstytype   = 'F'
    m.po         = m.cmfgordno

    *B131128,1 AMH Fix bug of Fabric file not found [Start]
    *m.cpurcode   = FABRIC.CPURCODE
    m.cpurcode   = IIF(llUseFabric,FABRIC.CPURCODE,"")
    *B131128,1 AMH [End]

    m.available  = m.complete
    m.rate       = 1
    m.pocancel   = oAriaApplication.SystemDate
    m.nicost1    = m.nest_cost1
    m.nicost2    = m.nest_cost2
    m.nicost3    = m.nest_cost3
    m.nicost4    = m.nest_cost4
    m.pototal    = m.nest_cost1 + m.nest_cost2 + m.nest_cost3 + m.nest_cost4
    m.nstyorder  = m.nmmgbudget
    m.receive    = m.received
    m.damage     = m.damaged
    m.cancel     = m.canceled
    m.open       = m.nmmfgopen
    m.ntot_cost  = m.nlan_cost1 + m.nlan_cost2 + m.nlan_cost3 + m.nlan_cost4
    m.cpricecur  = oAriaApplication.BaseCurrency
    m.cdutycur   = oAriaApplication.BaseCurrency
    m.npricerat  = 1
    m.ndutyrat   = 1
    m.nfcost1    = m.nest_cost1
    m.nfcost2    = m.nest_cost2
    m.nfcost3    = m.nest_cost3
    m.nfcost4    = m.nest_cost4
    m.nfactcost1 = m.nact_cost1
    m.nfactcost2 = m.nact_cost2
    m.nfactcost3 = m.nact_cost3
    m.nfactcost4 = m.nact_cost4
    m.nflancost1 = m.nlan_cost1
    m.nflancost2 = m.nlan_cost2
    m.nflancost3 = m.nlan_cost3
    m.nflancost4 = m.nlan_cost4
    m.ncurrunit  = 1
    m.ndcurunit  = 1
    m.style      = m.cfabric
    m.cInvType   = '0002'
    m.nicost6    = 0
    m.nicost7    = 0
    m.nact_cost6 = 0
    m.nact_cost7 = 0
    m.nlan_cost6 = 0
    m.nlan_cost7 = 0
    m.nfcost6    = 0
    m.nfcost7    = 0
    m.nfactcost6 = 0
    m.nfactcost7 = 0
    m.nflancost6 = 0
    m.nflancost7 = 0

    *B131128,1 AMH Fix bug of Fabric file not found [Start]
    *m.stygrade   = FABRIC.CFABGRADE
    m.stygrade   = IIF(llUseFabric,FABRIC.CFABGRADE,"")
    *B131128,1 AMH [End]

    m.cinvtype   = '0002'

    IF !lfUpdA4Files(lcPosHdr,"POSHDR","POSHDR","CBUSDOCU+CSTYTYPE+PO",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_WORKORD,oFormSet.GetHeaderText("LANG_CONVERT_WORKORD",oFormSet.HeaderAlias))+m.po+;
     IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_TYPE,oFormSet.GetHeaderText("LANG_CONVERT_TYPE",oFormSet.HeaderAlias))+m.cbusdocu+m.cstytype,oFormSet)
      RETURN .F.
    ENDIF
  ENDSCAN
ENDIF

IF llUseMaRwHdr
  SELECT MARWHDR
  SCAN
    SCATTER MEMVAR MEMO

    *B131128,1 AMH Fix bug of Fabric file not found [Start]
    *=SEEK(m.cfabric,'FABRIC')
    =llUseFabric AND SEEK(m.cfabric,'FABRIC')
    *B131128,1 AMH [End]

    *--Retreive Data from POSHDR file.
    m.cbusdocu   = 'R'
    m.cstytype   = 'W'
    m.po         = m.cmfgrwnum
    m.vendor     = m.cvendcode

    *B131128,1 AMH Fix bug of Fabric file not found [Start]
    *m.cpurcode   = FABRIC.CPURCODE
    m.cpurcode   = IIF(llUseFabric,FABRIC.CPURCODE,"")
    *B131128,1 AMH [End]

    m.available  = m.complete
    m.rate       = 1
    m.pocancel   = oAriaApplication.SystemDate
    m.nicost1    = m.nest_cost1
    m.nicost2    = m.nest_cost2
    m.nicost3    = m.nest_cost3
    m.nicost4    = m.nest_cost4
    m.pototal    = m.nest_cost1 + m.nest_cost2 + m.nest_cost3 + m.nest_cost4
    m.nstyorder  = m.nmmgbudget
    m.receive    = m.received
    m.damage     = m.damaged
    m.cancel     = m.canceled
    m.open       = m.nmmfgopen
    m.ntot_cost  = m.nlan_cost1 + m.nlan_cost2 + m.nlan_cost3 + m.nlan_cost4
    m.cpricecur  = oAriaApplication.BaseCurrency
    m.cdutycur   = oAriaApplication.BaseCurrency
    m.npricerat  = 1
    m.ndutyrat   = 1
    m.nfcost1    = m.nest_cost1
    m.nfcost2    = m.nest_cost2
    m.nfcost3    = m.nest_cost3
    m.nfcost4    = m.nest_cost4
    m.nfactcost1 = m.nact_cost1
    m.nfactcost2 = m.nact_cost2
    m.nfactcost3 = m.nact_cost3
    m.nfactcost4 = m.nact_cost4
    m.nflancost1 = m.nlan_cost1
    m.nflancost2 = m.nlan_cost2
    m.nflancost3 = m.nlan_cost3
    m.nflancost4 = m.nlan_cost4
    m.ncurrunit  = 1
    m.ndcurunit  = 1
    m.style      = m.cfabric
    m.cInvType   = '0002'
    m.nicost6    = 0
    m.nicost7    = 0
    m.nact_cost6 = 0
    m.nact_cost7 = 0
    m.nlan_cost6 = 0
    m.nlan_cost7 = 0
    m.nfcost6    = 0
    m.nfcost7    = 0
    m.nfactcost6 = 0
    m.nfactcost7 = 0
    m.nflancost6 = 0
    m.nflancost7 = 0

    *B131128,1 AMH Fix bug of Fabric file not found [Start]
    *m.stygrade   = FABRIC.CFABGRADE
    m.stygrade   = IIF(llUseFabric,FABRIC.CFABGRADE,"")
    *B131128,1 AMH [End]

    m.cinvtype   = '0002'

    IF !lfUpdA4Files(lcPosHdr,"POSHDR","POSHDR","CBUSDOCU+CSTYTYPE+PO",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_WORKORD,oFormSet.GetHeaderText("LANG_CONVERT_WORKORD",oFormSet.HeaderAlias))+m.po+;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_TYPE,oFormSet.GetHeaderText("LANG_CONVERT_TYPE",oFormSet.HeaderAlias))+m.cbusdocu+m.cstytype,oFormSet)
      RETURN .F.
    ENDIF
  ENDSCAN
ENDIF

IF llUsePosHdr AND USED('POSHDR')
  USE IN POSHDR
ENDIF

IF llUseCutTktH AND USED('CUTTKTH')
  USE IN CUTTKTH
ENDIF

IF llUsePofHdr AND USED('POFHDR')
  USE IN POFHDR
ENDIF

IF llUseMMfgOrdH AND USED('MMFGORDH')
  USE IN MMFGORDH
ENDIF

IF llUseMaRwHdr AND USED('MARWHDR')
  USE IN MARWHDR
ENDIF

*E038594,1 AMH Don't close the Fabric file here to give conversion more speed [Start]
*IF llUseFabric AND USED('FABRIC')
*  USE IN FABRIC
*ENDIF
*E038594,1 AMH [End]

*E038594,1 AMH Don't close the Style file here to give conversion more speed [Start]
*IF llUseStyle AND USED('STYLE')
*  USE IN STYLE
*ENDIF
*E038594,1 AMH [End]

IF llUseCutTktL AND USED('CUTTKTL')
  USE IN CUTTKTL
ENDIF

IF llUsePosLn AND USED('POSLN')
  USE IN POSLN
ENDIF

IF USED(lcPosHdr)
  USE IN (lcPosHdr)
ENDIF

*:*************************************************************
*: Name      : lfCnvPoLn
*: Developer : AHMED MAHER (AMH)
*: Date      : 03/25/2004
*: Purpose   : Convert POSLN file.
*:*************************************************************
FUNCTION lfCnvPoLn
LPARAMETERS oFormSet

LOCAL lcPosLn,llUsePosLn,llUseCutTktL,llUsePofLn,llUseMMfgOrdD,llUseMaRwLin,llUseMatShpDt,llUseStyle,llUseMatInvJl,;
      llUseFabric,llUseCutTktH,llUsePosHdr,llUsePofHdr,llUseMMfgOrdH,llUseMaRwHdr,llUseBom

lcPosLn = gfTempName()
STORE .F. to llUsePosLn,llUseCutTktL,llUsePofLn,llUseMMfgOrdD,llUseMaRwLin,llUseMatShpDt,llUseStyle,llUseMatInvJl,;
             llUseFabric,llUseCutTktH,llUsePosHdr,llUsePofHdr,llUseMMfgOrdH,llUseMaRwHdr,llUseBom

IF FILE(oAriaApplication.DataDir+'POSLN'+'.DBF')

  *E038125,1 AMH Use the POSLNSH index since it is unique [Start]
  *IF !gfOpenFile(oAriaApplication.DataDir+'POSLN','POSLN','SH')
  IF !gfOpenFile(oAriaApplication.DataDir+'POSLN','POSREC','SH')
  *E038125,1 AMH [End]

    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'POSLN'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'POSLN'+;
        IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUsePosLn = .T.
  ENDIF
ENDIF

IF FILE(oAriaApplication.DataDir+'CUTTKTL'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'CUTTKTL','CUTLIN','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'POSLN'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'CUTTKTL'+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUseCutTktL = .T.
  ENDIF
ENDIF

IF FILE(oAriaApplication.DataDir+'POFLN'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'POFLN','POFLN','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'POSLN'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'POFLN'+;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUsePofLn = .T.
  ENDIF
ENDIF

IF FILE(oAriaApplication.DataDir+'MMFGORDD'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'MMFGORDD','MMFGORDDLN','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'POSLN'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'MMFGORDD'+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUseMMfgOrdD = .T.
  ENDIF
ENDIF

IF FILE(oAriaApplication.DataDir+'MARWLIN'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'MARWLIN','MARWLIN','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'POSLN'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'MARWLIN'+;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)

    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUseMaRwLin = .T.
  ENDIF
ENDIF

IF FILE(oAriaApplication.DataDir+'MATSHPDT'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'MATSHPDT','MATLN','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'POSLN'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'MATSHPDT'+;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUseMaTshpDt = .T.
  ENDIF
ENDIF

IF llUseCutTktL AND FILE(oAriaApplication.DataDir+'STYLE'+'.DBF')

  *E038594,1 AMH Don't Open the Style file here to give conversion more speed [Start]
  *IF !gfOpenFile(oAriaApplication.DataDir+'STYLE','STYLE','SH')
  *  =lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+'POSLN'+LANG_CONVERT_NOTOPEN+'STYLE'+;
  *              LANG_CONVERT_FILE2,oFormSet)
  *  RETURN .F.
  *ELSE
    llUseStyle = .T.
  *ENDIF
  SET ORDER TO STYLE IN STYLE
  *E038594,1 AMH [End]

ENDIF

IF (llUseMMfgOrdD OR llUseMaRwLin) AND FILE(oAriaApplication.DataDir+'MATINVJL'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'MATINVJL','MATINVJL','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'POSLN'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'MATINVJL'+;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)

    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUseMatInvJl = .T.
  ENDIF
ENDIF

IF (llUsePofLn OR llUseMMfgOrdD OR llUseMaRwLin OR llUseMatShpDt) AND FILE(oAriaApplication.DataDir+'FABRIC'+'.DBF')

  *E038594,1 AMH Don't Open the Fabric file here to give conversion more speed [Start]
  *IF !gfOpenFile(oAriaApplication.DataDir+'FABRIC','FABRIC','SH')
  *  =lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+'POSLN'+LANG_CONVERT_NOTOPEN+'FABRIC'+;
  *              LANG_CONVERT_FILE2,oFormSet)
  *  RETURN .F.
  *ELSE
    llUseFabric = .T.
  *ENDIF
  SET ORDER TO FABRIC IN FABRIC
  *E038594,1 AMH [End]

ENDIF

IF llUseCutTktL AND FILE(oAriaApplication.DataDir+'CUTTKTH'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'CUTTKTH','CUTTKTH','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'POSLN'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'CUTTKTH'+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUseCutTktH = .T.
  ENDIF
ENDIF

IF llUsePosLn AND FILE(oAriaApplication.DataDir+'POSHDR'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'POSHDR','POSHDR','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'POSLN'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'POSHDR'+;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUsePosHdr = .T.
  ENDIF
ENDIF

IF (llUsePofLn OR llUseMatShpDt) AND FILE(oAriaApplication.DataDir+'POFHDR'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'POFHDR','POFHDR','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'POSLN'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'POFHDR'+;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUsePofHdr = .T.
  ENDIF
ENDIF

IF llUseMMfgOrdD AND FILE(oAriaApplication.DataDir+'MMFGORDH'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'MMFGORDH','MMFGORDH','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'POSLN'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'MMFGORDH'+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUseMMfgOrdH = .T.
  ENDIF
ENDIF

IF llUseMaRwLin AND FILE(oAriaApplication.DataDir+'MARWHDR'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'MARWHDR','MARWHDR','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'POSLN'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'MARWHDR'+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUseMaRwHdr = .T.
  ENDIF
ENDIF

IF (llUsePosLn OR llUseCutTktL OR llUsePofLn OR llUseMMfgOrdD OR llUseMaRwLin OR llUseMatShpDt) AND FILE(oAriaApplication.DataDir+'BOM'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'BOM','BOM','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'POSLN'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'BOM'+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUseBom = .T.
  ENDIF
ENDIF

IF !llUseBom

  *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
  *RETURN .F.
  RETURN .T.
  *B038828,1 AMH [End]

ENDIF

LOCAL lnPosRec,lnCutRec,lnPofRec,lnMfgRec,lnMarRec,lnMshRec
STORE 0 TO lnPosRec,lnCutRec,lnPofRec,lnMfgRec,lnMarRec,lnMshRec

IF llUsePosLn
  SELECT POSLN
  COUNT TO lnPosRec
ENDIF

IF llUseCutTktL
  SELECT CUTTKTL
  COUNT TO lnCutRec
ENDIF

IF llUsePofLn
  SELECT POFLN
  COUNT TO lnPofRec
ENDIF

IF llUseMMfgOrdD
  SELECT MMFGORDD
  COUNT TO lnMfgRec
ENDIF

IF llUseMaRwLin
  SELECT MARWLIN
  COUNT TO lnMarRec
ENDIF

IF llUseMatShpdt
  SELECT MATSHPDT
  COUNT FOR TRANCD = '3' TO lnMshRec
ENDIF

oFormSet.lnRecCnt = lnPosRec + lnCutRec + lnPofRec + lnMfgRec + lnMarRec + lnMshRec
oFormSet.lnCurRec = oFormSet.lnCurRec * oFormSet.lnRecCnt / 100

*-- Get the style major length
LOCAL oGetItemMask,lcMjrPct,lnStyMajLen
oGetItemMask = CREATEOBJECT('GetItemMask')
lcMjrPct     = oGetItemMask.Do('PM')   &&Major picture
lnStyMajLen  = LEN(lcMjrPct)      &&Major Part lenth

IF llUsePosLn
  SELECT POSLN
  *! B609148,1 MMT 02/15/2010 Some Lines in POSLN Table are not converted to SQL[Start]
  lcPoLnOrder = ORDER('POSLN')
  SET ORDER TO
  *! B609148,1 MMT 02/15/2010 Some Lines in POSLN Table are not converted to SQL[End]
  SCAN
    SCATTER MEMVAR MEMO

    *B038828,1 AMH Check if POSHDR file is ussed [Start]
    *=SEEK(m.cStyType+m.PO,'POSHDR')
    =llUsePosHdr AND SEEK(m.cStyType+m.PO,'POSHDR')
    *B038828,1 AMH [End]

    m.ccstsht_id = IIF(SEEK(PADR(SUBSTR(m.style,1,lnStyMajLen),19),'BOM'),'DEFCST','')

    *--Retreive Data from POSLN file.
    m.cbusdocu   = SUBSTR('PPPRCN',AT(m.cstytype,'PDARCN'),1)
    m.cstytype   = IIF(m.cstytype='R','P',m.cstytype)
    m.nicost1    = m.necost1
    m.nicost2    = m.necost2
    m.nicost3    = m.necost3
    m.nicost4    = m.necost4
    m.nicost5    = m.necost5
    m.nact_cost1 = m.neactcost1
    m.nact_cost2 = m.neactcost2
    m.nact_cost3 = m.neactcost3
    m.nact_cost4 = m.neactcost4
    m.nact_cost5 = m.neactcost5
    m.nlan_cost1 = m.nelancost1
    m.nlan_cost2 = m.nelancost2
    m.nlan_cost3 = m.nelancost3
    m.nlan_cost4 = m.nelancost4
    m.nlan_cost5 = m.nelancost5
    m.nfcost1    = m.ncost1
    m.nfcost2    = m.ncost2
    m.nfcost3    = m.ncost3
    m.nfcost4    = m.ncost4
    m.nfcost5    = m.ncost5
    m.nfactcost1 = m.nact_cst1
    m.nfactcost2 = m.nact_cst2
    m.nfactcost3 = m.nact_cst3
    m.nfactcost4 = m.nact_cst4
    m.nfactcost5 = m.nact_cst5
    m.nflancost1 = m.nlan_cst1
    m.nflancost2 = m.nlan_cst2
    m.nflancost3 = m.nlan_cst3
    m.nflancost4 = m.nlan_cst4
    m.nflancost5 = m.nlan_cst5
    *B608498 Fix the bug of take only the ferist line in the cancelled Po's if the same line recived as cancelled many times
    IF m.CBUSDOCU = 'P' AND m.TRANCD = "5" AND  EMPTY(m.CRSESSION)
      m.CRSESSION = gfsequence('GLSESSION')
      REPLACE POSLN.CRSESSION WITH m.CRSESSION
    ENDIF
    *B608498 [End]

    *B038828,1 AMH Check if POSHDR file is ussed [Start]
    m.complete   = IIF(llUsePosHdr,POSHDR.COMPLETE,{})
    m.shipvia    = IIF(llUsePosHdr,POSHDR.SHIPVIA,'')
    *B038828,1 AMH [End]

    *E038220,1 WSH Remove UOMBuy and Conv fields from POSLN and Add Relation Code [Start]
    *m.uombuy     = 'EAC'
    *m.conv       = 1

    *E038594,1 AMH Use variable hold the default UOM relation code instead of get it from UOM table [Start]
    *m.cUOMCode   = lfGetUOMRel(oFormSet, '', '', 1)
    m.cUOMCode   = lcDefUOMRel
    *E038594,1 AMH [End]

    *E038220,1 WSH [End]

    m.cinvtype   = '0001'

    *E038125,1 AMH Use the POSLNSH index since it is unique [Start]
    *IF !lfUpdA4Files(lcPosLn,"POSLN","POSLN","CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD",LANG_CONVERT_WORKORD+m.po+;
   *LANG_CONVERT_TYPE+m.cbusdocu+m.cstytype+LANG_CONVERT_LINENO+STR(m.lineno,6),oFormSet)

    IF !lfUpdA4Files(lcPosLn,"POSLN","POSREC","CBUSDOCU+CSTYTYPE+PO+CRSESSION+SHIPNO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD+CSTYGRADE+CWARECODE",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_WORKORD,oFormSet.GetHeaderText("LANG_CONVERT_WORKORD",oFormSet.HeaderAlias))+m.po+;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_TYPE,oFormSet.GetHeaderText("LANG_CONVERT_TYPE",oFormSet.HeaderAlias))+m.cbusdocu+m.cstytype+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_LINENO,oFormSet.GetHeaderText("LANG_CONVERT_LINENO",oFormSet.HeaderAlias))+STR(m.lineno,6),oFormSet)
    *E038125,1 AMH [End]

      RETURN .F.
    ENDIF
  ENDSCAN
  *! B609148,1 MMT 02/15/2010 Some Lines in POSLN Table are not converted to SQL[Start]
  IF !EMPTY(lcPoLnOrder)
    SELECT POSLN
    SET ORDER TO (lcPoLnOrder)
  ENDIF
  *! B609148,1 MMT 02/15/2010 Some Lines in POSLN Table are not converted to SQL[End]
ENDIF

IF llUseCutTktL
  SELECT CUTTKTL
  SCAN
    SCATTER MEMVAR MEMO
    =SEEK(m.style,'STYLE')

    *B038828,1 AMH Check if CUTTKTH file is ussed [Start]
    *=SEEK(m.cuttkt,'CUTTKTH')
    =llUseCutTktH AND SEEK(m.cuttkt,'CUTTKTH')
    *B038828,1 AMH [End]

    m.ccstsht_id = IIF(SEEK(PADR(SUBSTR(m.style,1,lnStyMajLen),19),'BOM'),'DEFCST','')

    *--Retreive Data from POSLN file.
    m.cbusdocu   = 'P'
    m.cstytype   = 'U'
    m.po         = m.cuttkt
    m.trancd     = IIF(m.trancd > '2',STR(VAL(m.trancd)+1,1),m.trancd)
    m.scale      = STYLE.SCALE
    m.nicost1    = m.ncost1
    m.nicost2    = m.ncost2
    m.nicost3    = m.ncost3
    m.nicost4    = m.ncost4
    m.nicost5    = m.ncost5
    m.nact_cost1 = m.nact_cst1
    m.nact_cost2 = m.nact_cst2
    m.nact_cost3 = m.nact_cst3
    m.nact_cost4 = m.nact_cst4
    m.nact_cost5 = m.nact_cst5
    m.nlan_cost1 = m.nlan_cst1
    m.nlan_cost2 = m.nlan_cst2
    m.nlan_cost3 = m.nlan_cst3
    m.nlan_cost4 = m.nlan_cst4
    m.nlan_cost5 = m.nlan_cst5
    m.nlanprrat  = 1
    m.nlandurat  = 1
    m.nactprrat  = 1
    m.nfcost1    = m.ncost1
    m.nfcost2    = m.ncost2
    m.nfcost3    = m.ncost3
    m.nfcost4    = m.ncost4
    m.nfcost5    = m.ncost5
    m.nfactcost1 = m.nact_cst1
    m.nfactcost2 = m.nact_cst2
    m.nfactcost3 = m.nact_cst3
    m.nfactcost4 = m.nact_cst4
    m.nfactcost5 = m.nact_cst5
    m.nflancost1 = m.nlan_cst1
    m.nflancost2 = m.nlan_cst2
    m.nflancost3 = m.nlan_cst3
    m.nflancost4 = m.nlan_cst4
    m.nflancost5 = m.nlan_cst5

    *B038828,1 AMH Check if CUTTKTH file is ussed [Start]
    *m.complete   = CUTTKTH.COMPLETE
    m.complete   = IIF(llUseCutTktH,CUTTKTH.COMPLETE,{})
    *B038828,1 AMH [End]

    *E038220,1 WSH Remove UOMBuy and Conv fields from POSLN and Add Relation Code [Start]
    *m.uombuy     = 'EAC'
    *m.conv       = 1

    *E038594,1 AMH Use the variable hold default UOM relation code instead of get it from UOM table [Start]
    *m.cUOMCode   = lfGetUOMRel(oFormSet, '', '', 1)
    m.cUOMCode   = lcDefUOMRel
    *E038594,1 AMH [End]

    *E038220,1 WSH [End]

    m.cinvtype   = '0001'

	*khm1
	m.ShipNo = SPACE(6)
	*khm1

    IF !lfUpdA4Files(lcPosLn,"POSLN","POSREC","CBUSDOCU+CSTYTYPE+PO+CRSESSION+SHIPNO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD+CSTYGRADE+CWARECODE",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_WORKORD,oFormSet.GetHeaderText("LANG_CONVERT_WORKORD",oFormSet.HeaderAlias))+m.po+;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_TYPE,oFormSet.GetHeaderText("LANG_CONVERT_TYPE",oFormSet.HeaderAlias))+m.cbusdocu+m.cstytype+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_LINENO,oFormSet.GetHeaderText("LANG_CONVERT_LINENO",oFormSet.HeaderAlias))+STR(m.lineno,6),oFormSet)
      RETURN .F.
    ENDIF
  ENDSCAN
ENDIF

IF llUsePofLn
  SELECT POFLN
  SCAN
    SCATTER MEMVAR MEMO

    *B038828,1 AMH Check if POFHDR file is ussed [Start]
    *=SEEK(m.cMatType+m.pomat,'POFHDR')
    =llUsePofHdr AND SEEK(m.cMatType+m.pomat,'POFHDR')
    *B038828,1 AMH [End]

    *B131128,1 AMH Fix bug of Fabric file not found [Start]
    *=SEEK(m.fabric+m.color,'FABRIC')
    =llUseFabric AND SEEK(m.fabric+m.color,'FABRIC')
    *B131128,1 AMH [End]

    m.ccstsht_id = IIF(SEEK(PADR(m.fabric,19),'BOM'),'DEFCST','')

    *--Retreive Data from POSLN file.
    m.cbusdocu   = m.cmattype
    m.cstytype   = 'M'
    m.po         = m.pomat
    m.trancd     = IIF(m.trancd > '2',STR(VAL(m.trancd)+1,1),m.trancd)
    m.style      = lfUpdItem(m.fabric,m.color)
    m.scale      = '*'
    m.qty1       = m.nfabtotqty
    m.totqty     = m.nfabtotqty
    m.gros_price = m.ncost1
    m.nicost1    = m.necost1
    m.nicost2    = m.necost2
    m.nicost3    = m.necost3
    m.nicost4    = m.necost4
    m.retsty     = lfUpdItem(m.cfabric1,m.ccolor1)
    m.cstygrade  = m.cfabgrade
    m.nfcost1    = m.ncost1
    m.nfcost2    = m.ncost2
    m.nfcost3    = m.ncost3
    m.nfcost4    = m.ncost4
    m.nfactcost1 = m.nact_cost1
    m.nfactcost2 = m.nact_cost2
    m.nfactcost3 = m.nact_cost3
    m.nfactcost4 = m.nact_cost4
    m.nflancost1 = m.nlan_cost1
    m.nflancost2 = m.nlan_cost2
    m.nflancost3 = m.nlan_cost3
    m.nflancost4 = m.nlan_cost4
    m.nact_cost1 = m.neactcost1
    m.nact_cost2 = m.neactcost2
    m.nact_cost3 = m.neactcost3
    m.nact_cost4 = m.neactcost4
    m.nlan_cost1 = m.nelancost1
    m.nlan_cost2 = m.nelancost2
    m.nlan_cost3 = m.nelancost3
    m.nlan_cost4 = m.nelancost4
    m.complete   = m.ddelivdate

    *B038828,1 AMH Check if POFHDR file is ussed [Start]
    *m.shipvia    = POFHDR.SHIPVIA
    m.shipvia    = IIF(llUsePofHdr,POFHDR.SHIPVIA,'')
    *B038828,1 AMH [End]

    *E038220,1 WSH Remove UOMBuy and Conv fields from POSLN and Add Relation Code [Start]
    *m.uombuy     = FABRIC.UOMBUY
    *m.conv       = FABRIC.CONV

    *B131128,1 AMH Fix bug of Fabric file not found [Start]
    *m.cUOMCode   = lfGetUOMRel(oFormSet, FABRIC.UOMBuy, FABRIC.UOMUse, FABRIC.Conv)
    m.cUOMCode = IIF(llUseFabric,lfGetUOMRel(oFormSet,FABRIC.UOMBuy,FABRIC.UOMUse,FABRIC.Conv),"")
    *B131128,1 AMH [End]

    *E038220,1 WSH [End]

    m.cinvtype   = '0002'

    IF !lfUpdA4Files(lcPosLn,"POSLN","POSREC","CBUSDOCU+CSTYTYPE+PO+CRSESSION+SHIPNO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD+CSTYGRADE+CWARECODE",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_WORKORD,oFormSet.GetHeaderText("LANG_CONVERT_WORKORD",oFormSet.HeaderAlias))+m.po+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_TYPE,oFormSet.GetHeaderText("LANG_CONVERT_TYPE",oFormSet.HeaderAlias))+m.cbusdocu+m.cstytype+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_LINENO,oFormSet.GetHeaderText("LANG_CONVERT_LINENO",oFormSet.HeaderAlias))+STR(m.lineno,6),oFormSet)
      RETURN .F.
    ENDIF
  ENDSCAN
ENDIF

IF llUseMMfgOrdD
  SELECT MMFGORDD
  SCAN
    SCATTER MEMVAR MEMO

    *B038828,1 AMH Check if MMFGORDH file is ussed [Start]
    *=SEEK(m.cmfgordno,'MMFGORDH')
    =llUseMMfgOrdH AND SEEK(m.cmfgordno,'MMFGORDH')
    *B038828,1 AMH [End]

    *B131128,1 AMH Fix bug of Fabric file not found [Start]
    *=SEEK(m.cfabric+m.color,'FABRIC')
    =llUseFabric AND SEEK(m.cfabric+m.color,'FABRIC')
    *B131128,1 AMH [End]

    *B038828,1 AMH Check if MATINVJL file is ussed [Start]
    *=SEEK(m.cfabric+m.color+m.cwarecode+m.dyelot+m.crsession,'MATINVJL')
    =llUseMatInvJl AND SEEK(m.cfabric+m.color+m.cwarecode+m.dyelot+m.crsession,'MATINVJL')
    *B038828,1 AMH [End]

    m.ccstsht_id = IIF(SEEK(PADR(m.fabric,19),'BOM'),'DEFCST','')

    *--Retreive Data from POSLN file.
    m.cbusdocu   = 'P'
    m.cstytype   = 'F'
    m.po         = m.cmfgordno
    m.trancd     = IIF(m.trancd > '2',STR(VAL(m.trancd)+1,1),m.trancd)
    m.style      = lfUpdItem(m.cfabric,m.color)
    m.scale      = '*'
    m.qty1       = m.nmfgtotqty
    m.totqty     = m.nmfgtotqty
    m.date       = m.drecvdate
    m.nicost1    = m.ncost1
    m.nicost2    = m.ncost2
    m.nicost3    = m.ncost3
    m.nicost4    = m.ncost4
    m.retsty     = lfUpdItem(m.cfabric1,m.ccolor1)
    m.cstygrade  = m.cfabgrade
    m.nlanprrat  = 1
    m.nlandurat  = 1
    m.nactprrat  = 1

    *B038828,1 AMH Check if MATINVJL file is ussed [Start]
    *m.dpostdate  = MATINVJL.DPOSTDATE
    m.dpostdate  = IIF(llUseMatInvJl,MATINVJL.DPOSTDATE,{})
    *B038828,1 AMH [End]

    m.nfcost1    = m.ncost1
    m.nfcost2    = m.ncost2
    m.nfcost3    = m.ncost3
    m.nfcost4    = m.ncost4
    m.nfactcost1 = m.nact_cost1
    m.nfactcost2 = m.nact_cost2
    m.nfactcost3 = m.nact_cost3
    m.nfactcost4 = m.nact_cost4
    m.nflancost1 = m.nlan_cost1
    m.nflancost2 = m.nlan_cost2
    m.nflancost3 = m.nlan_cost3
    m.nflancost4 = m.nlan_cost4

    *B038828,1 AMH Check if MMFGORDH file is ussed [Start]
    *m.complete   = MMFGORDH.COMPLETE
    m.complete   = IIF(llUseMMfgOrdH,MMFGORDH.COMPLETE,{})
    *B038828,1 AMH [End]

    *E038220,1 WSH Remove UOMBuy and Conv fields from POSLN and Add Relation Code [Start]
    *m.uombuy     = FABRIC.UOMBUY
    *m.conv       = FABRIC.CONV

    *B131128,1 AMH Fix bug of Fabric file not found [Start]
    *m.cUOMCode   = lfGetUOMRel(oFormSet, FABRIC.UOMBuy, FABRIC.UOMUse, FABRIC.Conv)
    m.cUOMCode   = IIF(llUseFabric,lfGetUOMRel(oFormSet,FABRIC.UOMBuy,FABRIC.UOMUse,FABRIC.Conv),"")
    *B131128,1 AMH [End]

    *E038220,1 WSH [End]

	*khm1
	m.ShipNo = SPACE(6)
	*khm1

    m.cinvtype   = '0002'

    IF !lfUpdA4Files(lcPosLn,"POSLN","POSREC","CBUSDOCU+CSTYTYPE+PO+CRSESSION+SHIPNO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD+CSTYGRADE+CWARECODE",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_WORKORD,oFormSet.GetHeaderText("LANG_CONVERT_WORKORD",oFormSet.HeaderAlias))+m.po+;
        IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_TYPE,oFormSet.GetHeaderText("LANG_CONVERT_TYPE",oFormSet.HeaderAlias))+m.cbusdocu+m.cstytype+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_LINENO,oFormSet.GetHeaderText("LANG_CONVERT_LINENO",oFormSet.HeaderAlias))+STR(m.lineno,6),oFormSet)
      RETURN .F.
    ENDIF
  ENDSCAN
ENDIF

IF llUseMaRwLin
  SELECT MARWLIN
  SCAN
    SCATTER MEMVAR MEMO

    *B038828,1 AMH Check if MARWHDR file is ussed [Start]
    *=SEEK(m.cmfgordno,'MARWHDR')
    =llUseMaRwHdr AND SEEK(m.cmfgordno,'MARWHDR')
    *B038828,1 AMH [End]

    *B131128,1 AMH Fix bug of Fabric file not found [Start]
    *=SEEK(m.cfabric+m.color,'FABRIC')
    =llUseFabric AND SEEK(m.cfabric+m.color,'FABRIC')
    *B131128,1 AMH [End]

    *B038828,1 AMH Check if MATINVJL file is ussed [Start]
    *=SEEK(m.cfabric+m.color+m.cwarecode+m.dyelot+m.crsession,'MATINVJL')
    =llUseMatInvJl AND SEEK(m.cfabric+m.color+m.cwarecode+m.dyelot+m.crsession,'MATINVJL')
    *B038828,1 AMH [End]

    m.ccstsht_id = IIF(SEEK(PADR(m.fabric,19),'BOM'),'DEFCST','')

    *--Retreive Data from POSLN file.
    m.cbusdocu   = 'R'
    m.cstytype   = 'W'
    m.po         = m.cmfgrwnum
    LOCAL lcTranCd
    lcTranCd = ''
    DO CASE
      CASE m.trancd = '1'
        lcTranCd = '1'
      CASE m.trancd = '2'
        lcTranCd = '6'
      CASE m.trancd = '3'
        lcTranCd = '2'
      CASE m.trancd = '4'
        lcTranCd = '5'
      CASE m.trancd = '6'
        lcTranCd = '4'
    ENDCASE
    m.trancd     = lcTranCd
    m.style      = lfUpdItem(m.cfabric,m.color)
    m.scale      = '*'
    m.qty1       = m.nmfgtotqty
    m.totqty     = m.nmfgtotqty
    m.date       = m.drecvdate
    m.nicost1    = m.ncost1
    m.nicost2    = m.ncost2
    m.nicost3    = m.ncost3
    m.nicost4    = m.ncost4
    m.retsty     = lfUpdItem(m.cfabric1,m.ccolor1)
    m.cstygrade  = m.cfabgrade
    m.nlanprrat  = 1
    m.nlandurat  = 1
    m.nactprrat  = 1

    *B038828,1 AMH Check if MATINVJL file is ussed [Start]
    *m.dpostdate  = MATINVJL.DPOSTDATE
    m.dpostdate  = IIF(llUseMatInvJl,MATINVJL.DPOSTDATE,{})
    *B038828,1 AMH [End]

    m.nfcost1    = m.ncost1
    m.nfcost2    = m.ncost2
    m.nfcost3    = m.ncost3
    m.nfcost4    = m.ncost4
    m.nfactcost1 = m.nact_cost1
    m.nfactcost2 = m.nact_cost2
    m.nfactcost3 = m.nact_cost3
    m.nfactcost4 = m.nact_cost4
    m.nflancost1 = m.nlan_cost1
    m.nflancost2 = m.nlan_cost2
    m.nflancost3 = m.nlan_cost3
    m.nflancost4 = m.nlan_cost4

    *B038828,1 AMH Check if MARWHDR file is ussed [Start]
    *m.complete   = MARWHDR.COMPLETE
    m.complete   = IIF(llUseMaRwHdr,MARWHDR.COMPLETE,{})
    *B038828,1 AMH [End]

    *E038220,1 WSH Remove UOMBuy and Conv fields from POSLN and Add Relation Code [Start]
    *m.uombuy     = FABRIC.UOMBUY
    *m.conv       = FABRIC.CONV

    *B131128,1 AMH Fix bug of Fabric file not found [Start]
    *m.cUOMCode   = lfGetUOMRel(oFormSet, FABRIC.UOMBuy, FABRIC.UOMUse, FABRIC.Conv)
    m.cUOMCode   = IIF(llUseFabric,lfGetUOMRel(oFormSet,FABRIC.UOMBuy,FABRIC.UOMUse,FABRIC.Conv),"")
    *B131128,1 AMH [End]

    *E038220,1 WSH [End]

    m.cinvtype   = '0002'

    IF !lfUpdA4Files(lcPosLn,"POSLN","POSREC","CBUSDOCU+CSTYTYPE+PO+CRSESSION+SHIPNO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD+CSTYGRADE+CWARECODE",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_WORKORD,oFormSet.GetHeaderText("LANG_CONVERT_WORKORD",oFormSet.HeaderAlias))+m.po+;
      IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_TYPE,oFormSet.GetHeaderText("LANG_CONVERT_TYPE",oFormSet.HeaderAlias))+m.cbusdocu+m.cstytype+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_LINENO,oFormSet.GetHeaderText("LANG_CONVERT_LINENO",oFormSet.HeaderAlias))+STR(m.lineno,6),oFormSet)
      RETURN .F.
    ENDIF
  ENDSCAN
ENDIF

IF llUseMatShpDt
  SELECT MATSHPDT
  SCAN FOR TRANCD = '3'
    SCATTER MEMVAR MEMO

    *B038828,1 AMH Check if POFHDR file is ussed [Start]
    *=SEEK(m.cMatType+m.pomat,'POFHDR')
    =llUsePofHdr AND SEEK(m.cMatType+m.pomat,'POFHDR')
    *B038828,1 AMH [End]

    *B131128,1 AMH Fix bug of Fabric file not found [Start]
    *=SEEK(m.fabric+m.color,'FABRIC')
    =llUseFabric AND SEEK(m.fabric+m.color,'FABRIC')
    *B131128,1 AMH [End]

    m.ccstsht_id = IIF(SEEK(PADR(m.fabric,19),'BOM'),'DEFCST','')

    *--Retreive Data from POSLN file.
    m.cbusdocu   = m.cmattype
    m.cstytype   = 'M'
    m.po         = m.pomat
    m.style      = lfUpdItem(m.fabric,m.color)
    m.scale      = '*'
    m.qty1       = m.nfabtotqty
    m.totqty     = m.nfabtotqty
    m.gros_price = m.ncost1
    m.nicost1    = m.necost1
    m.nicost2    = m.necost2
    m.nicost3    = m.necost3
    m.nicost4    = m.necost4
    m.retsty     = lfUpdItem(m.cfabric1,m.ccolor1)
    m.cstygrade  = m.cfabgrade
    m.nfcost1    = m.ncost1
    m.nfcost2    = m.ncost2
    m.nfcost3    = m.ncost3
    m.nfcost4    = m.ncost4
    m.nfactcost1 = m.nact_cost1
    m.nfactcost2 = m.nact_cost2
    m.nfactcost3 = m.nact_cost3
    m.nfactcost4 = m.nact_cost4
    m.nflancost1 = m.nlan_cost1
    m.nflancost2 = m.nlan_cost2
    m.nflancost3 = m.nlan_cost3
    m.nflancost4 = m.nlan_cost4
    m.nact_cost1 = m.neactcost1
    m.nact_cost2 = m.neactcost2
    m.nact_cost3 = m.neactcost3
    m.nact_cost4 = m.neactcost4
    m.nlan_cost1 = m.nelancost1
    m.nlan_cost2 = m.nelancost2
    m.nlan_cost3 = m.nelancost3
    m.nlan_cost4 = m.nelancost4

    *B038828,1 AMH Check if POFHDR file is ussed [Start]
    *m.shipvia    = POFHDR.SHIPVIA
    m.shipvia    = IIF(llUsePofHdr,POFHDR.SHIPVIA,'')
    *B038828,1 AMH [End]

    *E038220,1 WSH Remove UOMBuy and Conv fields from POSLN and Add Relation Code [Start]
    *m.uombuy     = FABRIC.UOMBUY
    *m.conv       = FABRIC.CONV

    *B131128,1 AMH Fix bug of Fabric file not found [Start]
    *m.cUOMCode   = lfGetUOMRel(oFormSet, FABRIC.UOMBuy, FABRIC.UOMUse, FABRIC.Conv)
    m.cUOMCode   = IIF(llUseFabric,lfGetUOMRel(oFormSet,FABRIC.UOMBuy,FABRIC.UOMUse,FABRIC.Conv),"")
    *B131128,1 AMH [End]

    *E038220,1 WSH [End]

    m.cinvtype   = '0002'

    IF !lfUpdA4Files(lcPosLn,"POSLN","POSREC","CBUSDOCU+CSTYTYPE+PO+CRSESSION+SHIPNO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD+CSTYGRADE+CWARECODE",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_WORKORD,oFormSet.GetHeaderText("LANG_CONVERT_WORKORD",oFormSet.HeaderAlias))+m.po+;
          IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_TYPE,oFormSet.GetHeaderText("LANG_CONVERT_TYPE",oFormSet.HeaderAlias))+m.cbusdocu+m.cstytype+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_LINENO,oFormSet.GetHeaderText("LANG_CONVERT_LINENO",oFormSet.HeaderAlias))+STR(m.lineno,6),oFormSet)
      RETURN .F.
    ENDIF
  ENDSCAN
ENDIF

IF llUsePosLn AND USED('POSLN')
  USE IN POSLN
ENDIF

IF llUseCutTktL AND USED('CUTTKTL')
  USE IN CUTTKTL
ENDIF

IF llUsePofLn AND USED('POFLN')
  USE IN POFLN
ENDIF

IF llUseMMfgOrdD AND USED('MMFGORDD')
  USE IN MMFGORDD
ENDIF

IF llUseMaRwLin AND USED('MARWLIN')
  USE IN MARWLIN
ENDIF

IF llUseMatShpDt AND USED('MATSHPDT')
  USE IN MATSHPDT
ENDIF

*E038594,1 AMH Don't close the Fabric file here to give conversion more speed [Start]
*IF llUseFabric AND USED('FABRIC')
*  USE IN FABRIC
*ENDIF

*B131128,1 AMH Fix bug of Fabric file not found [Start]
*SET ORDER TO CFABRIC IN FABRIC
IF llUseFabric
  SET ORDER TO CFABRIC IN FABRIC
ENDIF
*B131128,1 AMH [End]

*E038594,1 AMH [End]

*E038594,1 AMH Don't close the Style file here to give conversion more speed [Start]
*IF llUseStyle AND USED('STYLE')
*  USE IN STYLE
*ENDIF
SET ORDER TO CSTYLE IN STYLE
*E038594,1 AMH [End]

IF llUseMatInvJl AND USED('MATINVJL')
  USE IN MATINVJL
ENDIF

IF llUsePosHdr AND USED('POSHDR')
  USE IN POSHDR
ENDIF

IF llUseCutTktH AND USED('CUTTKTH')
  USE IN CUTTKTH
ENDIF

IF llUsePofHdr AND USED('POFHDR')
  USE IN POFHDR
ENDIF

IF llUseMMfgOrdH AND USED('MMFGORDH')
  USE IN MMFGORDH
ENDIF

IF llUseMaRwHdr AND USED('MARWHDR')
  USE IN MARWHDR
ENDIF

IF USED('BOM')
  USE IN BOM
ENDIF

IF USED(lcPosLn)
  USE IN (lcPosLn)
ENDIF

*:*************************************************************
*: Name      : lfUpdItem
*: Developer : AHMED MAHER (AMH)
*: Date      : 04/11/2004
*: Purpose   : Update ITEM field with correct segment or fabric and color.
*:*************************************************************
FUNCTION lfUpdItem
LPARAMETERS lcFabric,lcColor

LOCAL lnAlias,lcRet
lnAlias = SELECT(0)

*E038594,1 AMH Get the material code structure once at the start of conversion procces [Start]
*-- Get the Material Code structure.
*LOCAL oGetItemMask,lcItemPic,lnI,lnRemItem,lcRemItem
*oGetItemMask = CREATEOBJECT('GetItemMask')
*LOCAL ARRAY laItemSeg[1,1]
*= oGetItemMask.Do(@laItemSeg,'','0002')
*lcItemPic = oGetItemMask.Do('PI','','0002')
LOCAL lnI,lnRemItem,lcRemItem
*E038594,1 AMH [End]

lcRet = STRTRAN(lcItemPic,'X','*')
lnRemItem = 7
lcRemItem = PADR(lcFabric,7)
FOR lnI = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnI,1] = 'C'
    lcRet = STUFF(lcRet,laItemSeg[lnI,4],LEN(laItemSeg[lnI,3]),PADR(lcColor,LEN(laItemSeg[lnI,3])))
  ENDIF

  IF laItemSeg[lnI,1] $ 'FO' AND lnRemItem > 0
    lcRet = STUFF(lcRet,laItemSeg[lnI,4],LEN(laItemSeg[lnI,3]),PADR(lcRemItem,LEN(laItemSeg[lnI,3])))
    lnRemItem = lnRemItem - LEN(laItemSeg[lnI,3])
    IF lnRemItem > 0
      lcRemItem = SUBSTR(lcRemItem,LEN(laItemSeg[lnI,3])+1)
    ENDIF
  ENDIF
ENDFOR

SELECT (lnAlias)
RETURN lcRet

*:*************************************************************
*: Name      : lfCnvShpHd
*: Developer : AHMED MAHER (AMH)
*: Date      : 04/04/2004
*: Purpose   : Convert SHPMTHDR file.
*:*************************************************************
FUNCTION lfCnvShpHd
LPARAMETERS oFormSet

LOCAL lcShpMtHdr,llUseShpMtHdr,llUseMatShpHd

lcShpMtHdr = gfTempName()
STORE .F. to llUseShpMtHdr,llUseMatShpHd

IF FILE(oAriaApplication.DataDir+'SHPMTHDR'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'SHPMTHDR','SHPMTHDR','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'SHPMTHDR'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'SHPMTHDR'+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUseShpMtHdr = .T.
  ENDIF
ENDIF

IF FILE(oAriaApplication.DataDir+'MATSHPHD'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'MATSHPHD','MATSHPHD','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'SHPMTHDR'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'MATSHPHD'+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUseMatShpHd = .T.
  ENDIF
ENDIF

IF !llUseShpMtHdr AND !llUseMatShpHd

  *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
  *RETURN .F.
  RETURN .T.
  *B038828,1 AMH [End]

ENDIF

LOCAL lnPosRec,lnMatRec
STORE 0 TO lnPosRec,lnMatRec

IF llUseShpMtHdr
  SELECT SHPMTHDR
  COUNT TO lnPosRec
ENDIF

IF llUseMatShpHd
  SELECT MATSHPHD
  COUNT TO lnMatRec
ENDIF

oFormSet.lnRecCnt = lnPosRec + lnMatRec
oFormSet.lnCurRec = oFormSet.lnCurRec * oFormSet.lnRecCnt / 100

IF llUseShpMtHdr
  SELECT SHPMTHDR
  SCAN
    SCATTER MEMVAR MEMO

    *--Retreive Data from SHPMTHDR file.
    m.cbusdocu   = 'P'
    m.cshptype   = 'P'
    m.totqtyhdr  = m.totqty

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF !lfUpdA4Files(lcShpMtHdr,"SHPMTHDR","SHPMTHDR","CBUSDOCU+CSHPTYPE+SHIPNO",LANG_CONVERT_SHIPMENT+m.shipno,oFormSet)
IF !lfUpdA4Files(lcShpMtHdr,"SHPMTHDR","SHPMTHDR","CBUSDOCU+CSHPTYPE+SHIPNO",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_SHIPMENT,oFormSet.GetHeaderText("LANG_CONVERT_SHIPMENT",oFormSet.HeaderAlias))+m.shipno,oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

      RETURN .F.
    ENDIF
  ENDSCAN
ENDIF

IF llUseMatShpHd
  SELECT MATSHPHD
  SCAN
    SCATTER MEMVAR MEMO

    *--Retreive Data from SHPMTHDR file.
    m.cbusdocu   = 'P'
    m.cshptype   = 'M'
    m.totqtyhdr  = m.nfabtotqty
    m.recv_stk   = m.recvstk
    m.recv_dam   = m.recvdam
    m.recv_can   = m.recvcan

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF !lfUpdA4Files(lcShpMtHdr,"SHPMTHDR","SHPMTHDR","CBUSDOCU+CSHPTYPE+SHIPNO",LANG_CONVERT_SHIPMENT+m.shipno,oFormSet)
IF !lfUpdA4Files(lcShpMtHdr,"SHPMTHDR","SHPMTHDR","CBUSDOCU+CSHPTYPE+SHIPNO",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_SHIPMENT,oFormSet.GetHeaderText("LANG_CONVERT_SHIPMENT",oFormSet.HeaderAlias))+m.shipno,oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

      RETURN .F.
    ENDIF
  ENDSCAN
ENDIF

IF llUseShpMtHdr AND USED('SHPMTHDR')
  USE IN SHPMTHDR
ENDIF

IF llUseMatShpHd AND USED('MATSHPHD')
  USE IN MATSHPHD
ENDIF

IF USED(lcShpMtHdr)
  USE IN (lcShpMtHdr)
ENDIF

*:*************************************************************
*: Name      : lfCnvInvTp
*: Developer : AHMED MAHER (AMH)
*: Date      : 04/05/2004
*: Purpose   : Convert INVTYPE file.
*:*************************************************************
FUNCTION lfCnvInvTp
LPARAMETERS oFormSet

LOCAL lcInvType
lcInvType = gfTempName()

oFormSet.lnRecCnt = 2
oFormSet.lnCurRec = oFormSet.lnCurRec * oFormSet.lnRecCnt / 100

*--Add style type--*
*--Retreive Data from INVTYPE file.
m.cinvtype  = '0001'
m.cdesc     = 'Style'
m.cstatus   = 'A'
m.citemstru = 'U'
m.citemtran = 'SBUM'

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF !lfUpdA4Files(lcInvType,"INVTYPE","CINVTYPE","CINVTYPE",LANG_CONVERT_TYPE+m.cinvtype,oFormSet)
IF !lfUpdA4Files(lcInvType,"INVTYPE","CINVTYPE","CINVTYPE",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_TYPE,oFormSet.GetHeaderText("LANG_CONVERT_TYPE",oFormSet.HeaderAlias))+m.cinvtype,oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  RETURN .F.
ENDIF

*--Add Material type--*
*--Add Material code structure if not exist.
=gfOpenFile(oAriaApplication.DataDir+'ICISTRU','SEGNO','SH')
IF !SEEK('M','ICISTRU')
  INSERT INTO ICISTRU (CITEMRECTY,CISEGNO,NISEGSIZE,CISEGSDES,CISEGSEPR,CISEGHEAD,CISEGTYPE,LSEGENDMAJ) VALUES;
                      ('M','1',12,'Fabric','-','Fabric      -Color','F',.T.)
  INSERT INTO ICISTRU (CITEMRECTY,CISEGNO,NISEGSIZE,CISEGSDES,CISEGTYPE,LSEGENDMAJ) VALUES;
                      ('M','2',6,'Color','C',.F.)
ENDIF
USE IN ICISTRU

*--Retreive Data from INVTYPE file.
m.cinvtype  = '0002'
m.cdesc     = 'Material'
m.cstatus   = 'A'
m.citemstru = 'M'
m.citemtran = 'BUM'

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF !lfUpdA4Files(lcInvType,"INVTYPE","CINVTYPE","CINVTYPE",LANG_CONVERT_TYPE+m.cinvtype,oFormSet)
IF !lfUpdA4Files(lcInvType,"INVTYPE","CINVTYPE","CINVTYPE",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_TYPE,oFormSet.GetHeaderText("LANG_CONVERT_TYPE",oFormSet.HeaderAlias))+m.cinvtype,oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  RETURN .F.
ENDIF

IF USED(lcInvType)
  USE IN (lcInvType)
ENDIF
= oGetItemMask.Do(@laItemSeg,'','0002')
lcItemPic = oGetItemMask.Do('PI','','0002')

*:*************************************************************
*: Name      : lfCnvItem
*: Developer : AHMED MAHER (AMH)
*: Date      : 04/04/2004
*: Purpose   : Convert ITEM file.
*:*************************************************************
FUNCTION lfCnvItem
LPARAMETERS oFormSet

LOCAL lcItem,llUseStyle,llUseFabric,llUseFabDye,llUsePofLn,llUseMMfgOrdH,llUseMMfgOrdD,llUseCodes
LOCAL llStyMark,lnTotCst,lnRotSub,lcCstTyp,lnTotWo

lcItem = gfTempName()
STORE .F. to llUseStyle,llUseFabric,llUseFabDye,llUsePofLn,llUseMMfgOrdD,llUseCodes

llStyMark  = gfGetMemVar('M_stymark')='T'

*!B999999,1 WSH 03/06/2005, Read Markup Setup from Material Setup. [Start]
LOCAL llFabMark

llFabMark  = gfGetMemVar('M_FABMARK')='T'
*!B999999,1 WSH 03/06/2005, [End]

*--Comment in phase 1 to be uncoment in phase 2 [Start]
*!*  IF FILE(oAriaApplication.DataDir+'STYLE'+'.DBF')
*!*    IF !gfOpenFile(oAriaApplication.DataDir+'STYLE','STYLE','SH')
*!*      =lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+'ITEM'+LANG_CONVERT_NOTOPEN+'STYLE'+;
*!*                  LANG_CONVERT_FILE2,oFormSet)
*!*      RETURN .F.
*!*    ELSE
*!*      llUseStyle = .T.
*!*    ENDIF
*!*  ENDIF
*-- [End]

IF FILE(oAriaApplication.DataDir+'FABRIC'+'.DBF')

  *E038594,1 AMH Don't Open the Fabric file here to give conversion more speed [Start]
  *IF !gfOpenFile(oAriaApplication.DataDir+'FABRIC','FABRIC','SH')
  *  =lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+'ITEM'+LANG_CONVERT_NOTOPEN+'FABRIC'+;
  *              LANG_CONVERT_FILE2,oFormSet)
  *  RETURN .F.
  *ELSE
    llUseFabric = .T.
  *ENDIF
  SET ORDER TO FABRIC IN FABRIC
  *E038594,1 AMH [End]

ENDIF

IF llUseFabric AND FILE(oAriaApplication.DataDir+'FABDYE'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'FABDYE','FABDYE','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'ITEM'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'FABDYE'+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUseFabDye = .T.
  ENDIF
ENDIF

IF llUseFabric AND FILE(oAriaApplication.DataDir+'POFLN'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'POFLN','POFLNF','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'ITEM'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'POFLN'+;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUsePofLn = .T.
  ENDIF
ENDIF

IF llUseFabric AND FILE(oAriaApplication.DataDir+'MMFGORDD'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'MMFGORDD','MMFGORDD','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'ITEM'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'MMFGORDD'+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUseMMfgOrdD = .T.
  ENDIF
ENDIF

IF llUseFabric AND FILE(oAriaApplication.DataDir+'MMFGORDH'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'MMFGORDH','MMFGORDD','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'ITEM'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'MMFGORDH'+;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUseMMfgOrdH = .T.
  ENDIF
ENDIF

IF llUseFabric AND FILE(oAriaApplication.DataDir+'CODES'+'.DBF')

  *E038594,1 AMH Don't Open the Codes file here to give conversion more speed [Start]
  *IF !gfOpenFile(oAriaApplication.DataDir+'CODES','CCODE_NO','SH')
  *  =lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+'ITEM'+LANG_CONVERT_NOTOPEN+'CODES'+;
  *              LANG_CONVERT_FILE2,oFormSet)
  *  RETURN .F.
  *ELSE
    llUseCodes = .T.
  *ENDIF
  *E038594,1 AMH [End]

ENDIF

IF !llUseStyle AND !llUseFabric

  *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
  *RETURN .F.
  RETURN .T.
  *B038828,1 AMH [End]

ENDIF

LOCAL lnStyRec,lnFabRec
STORE 0 TO lnStyRec,lnFabRec

IF llUseStyle
  SELECT STYLE
  COUNT TO lnStyRec
ENDIF

IF llUseFabric
  SELECT FABRIC
  COUNT TO lnFabRec
ENDIF

oFormSet.lnRecCnt = lnStyRec + lnFabRec
oFormSet.lnCurRec = oFormSet.lnCurRec * oFormSet.lnRecCnt / 100

IF llUseStyle
  SELECT STYLE
  SCAN
    SCATTER MEMVAR MEMO

    *--Retreive Data from ITEM file.
    ***--- fill this bluck in phase 2 [Start]
    ***--- [End]

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF !lfUpdA4Files(lcItem,"ITEM","STYLE","CINVTYPE+STYLE",LANG_CONVERT_ITEM+m.cinvtype+m.style,oFormSet)
IF !lfUpdA4Files(lcItem,"ITEM","STYLE","CINVTYPE+STYLE",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_ITEM,oFormSet.GetHeaderText("LANG_CONVERT_ITEM",oFormSet.HeaderAlias))+m.cinvtype+m.style,oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

      RETURN .F.
    ENDIF
  ENDSCAN
ENDIF

IF llUseFabric
  SELECT FABRIC
  SCAN
    SCATTER MEMVAR MEMO

    *B038828,1 AMH Check if FABDYE file is ussed [Start]
    *=SEEK(m.fabric+m.color,'FABDYE')
    =llUseFabDye AND SEEK(m.fabric+m.color,'FABDYE')
    *B038828,1 AMH [End]

    *E038594,1 AMH Remove nWo1,ntotwo from item table [Start]

    *E039550,1 WSH 08/07/2005 Remove Comments to add Quantity Fields Totals to the Item File. [Start]
    lnTotWo      = 0
    IF m.make
      IF SEEK(m.fabric,'MMFGORDH')
        SELECT MMFGORDH
        SCAN REST WHILE cfabric+cmfgordno = m.fabric
          IF SEEK(cmfgordno+cfabric+m.color,'MMFGORDD')
            SELECT MMFGORDD
            SCAN REST WHILE cmfgordno+cfabric+color+dyelot+trancd = MMFGORDH.CMFGORDNO+m.fabric+m.color FOR trancd = '1'
              lnTotWo = lnTotWo + nmfgtotqty
            ENDSCAN
          ENDIF
        ENDSCAN
      ENDIF
    ELSE
      IF llUsePofLn AND SEEK(m.fabric+m.color+'P','POFLN')
        SELECT POFLN
        SCAN REST WHILE fabric+color+cmattype+pomat+trancd = m.fabric+m.color+'P' FOR trancd = '1'
          lnTotWo = lnTotWo + nfabtotqty
        ENDSCAN
      ENDIF
    ENDIF
    *E039550,1 WSH 08/07/2005 [End]

    *E038594,1 AMH [End]

    *--Retreive Data from ITEM file.
    m.cinvtype   = '0002'
    m.style      = lfUpdItem(m.fabric,m.color)
    m.cstymajor  = m.fabric
    m.status     = 'A'
    m.lInvSty    = .T.
    m.cDivision  = IIF(SEEK('DCDIVISION ','CODES'),CODES.CCODE_NO,'')
    m.cstygroup  = IIF(SEEK('DCSTYGROUP ','CODES'),CODES.CCODE_NO,'')
    m.cDiscCode  = IIF(SEEK('DCDISCCODE ','CODES'),CODES.CCODE_NO,'')
    m.scale      = '*'
    m.desc1      = m.desc
    m.season     = IIF(SEEK('DSEASON    ','CODES'),CODES.CCODE_NO,'')
    m.cstygrade  = m.cfabgrade
    m.royalty    = IIF(SEEK('DROYALTY   ','CODES'),CODES.CCODE_NO,'')
    m.nicost1    = m.nfabcost
    m.nicost2    = m.nitem_tax
    m.nicost3    = m.nitemquota
    m.nicost4    = m.nitm_frt
    *khm1 Replace these fields with 0 because the are caculated in the style/material screen
    *m.nprcost2   = IIF(m.nmcost1=0,0,m.nmcost2/m.nmcost1*100)
    *m.nprcost3   = IIF(m.nmcost1=0,0,m.nmcost3/m.nmcost1*100)
    *m.nprcost4   = IIF(m.nmcost1=0,0,m.nmcost4/m.nmcost1*100)

    m.nprcost2   = 0
    m.nprcost3   = 0
    m.nprcost4   = 0
    *khm1

    m.totcost    = m.costbuy
    m.ave_cost   = m.nfave_cost
    m.location   = m.loc
    m.ord1       = m.nsellorder
    m.totord     = m.nsellorder
    m.wip1       = m.onorder
    m.totwip     = m.onorder
    m.stk1       = m.onhand
    m.totstk     = m.onhand
    m.shp1       = m.nsellship
    m.totshp     = m.nsellship
    m.pricea     = m.nsellprice
    m.priceb     = m.nsellprice
    m.pricec     = m.nsellprice
    lcCstTyp     = IIF(m.make,'m','i')
    lnTotCst     = EVALUATE('m.n'+lcCstTyp+'cost1+m.n'+lcCstTyp+'cost2+m.n'+lcCstTyp+'cost3+m.n'+lcCstTyp+'cost4')

    *!B999999,1 WSH 03/06/2005, Read Markup Setup from Material Setup. [Start]
    *lnRotSub     = IIF(llStyMark,ROUND(lnTotCst,2),ROUND(m.nsellprice,2))
    lnRotSub     = IIF(llFabMark,ROUND(lnTotCst,2),ROUND(m.nsellprice,2))
    *!B999999,1 WSH 03/06/2005, [End]

    *E038594,1 AMH Get the corrent value [Start]
    *m.marka      = MAX(MIN(IIF(m.nsellprice=0 OR lnTotCst=0 ,0,;
                            ((ROUND(m.nsellprice,2)-ROUND(lnTotCst,2)) / lnRotSub )*100 ),999999),-99999)
    m.marka      = MAX(MIN(IIF(m.nsellprice=0 OR lnTotCst=0 ,0,;
                            ((ROUND(m.nsellprice,2)-ROUND(lnTotCst,2)) / lnRotSub )*100 ),999),-99)
    *E038594,1 AMH [End]

    m.markb      = m.marka
    m.markc      = m.marka
    m.ncurrrate  = m.curr_rate
    m.nfrgnprice = m.frgn_price

    *B038828,1 AMH Check if FABDYE file is ussed [Start]
    *m.cdefware   = FABDYE.CWARECODE
    m.cdefware   = IIF(llUseFabDye,FABDYE.CWARECODE,'')
    *B038828,1 AMH [End]

    m.intrans1   = m.matotintr
    m.totintrn   = m.matotintr

    *E038594,1 AMH Remove this files from item table [Start]
    *m.nwo1       = lnTotWo
    *m.ntotwo     = lnTotWo
    *E038594,1 AMH [End]

    *E039550,1 WSH 08/07/2005 Add Quantity Fields in the Item File. [Start]
    m.ntotwo     = lnTotWo
    *E039550,1 WSH 08/07/2005 [End]

    m.cretsty    = lfUpdItem(m.cfabric1,m.ccolor1)
    m.cretsty2   = lfUpdItem(m.cfabric2,m.ccolor2)
    m.ldetcost   = m.make
    m.gros_price = IIF(m.make,m.nmcost1,m.nfabcost)
    m.nonret1    = m.onret
    m.ntotonret  = m.onret
    m.nreorder1  = m.reorder
    m.ntotreord  = m.reorder
    m.citemfld1  = m.width
    m.citemfld2  = m.cfabweight
    m.ncusage1   = m.nmatwip
    m.ntotcusa   = m.nmatwip
    m.nhusage1   = m.usage
    m.ntothusage = m.usage
    m.citemfld3  = m.content
    m.fabric     = ''

    *E038220,1 WSH Get UOM Relation Code from UOM File to add it to Item file [Start]
    m.CCONVBUY   = lfGetUOMRel(oFormSet, FABRIC.UOMBuy, FABRIC.UOMUse, FABRIC.Conv)
    m.CCONVSELL  = lfGetUOMRel(oFormSet, FABRIC.cSellUOM, FABRIC.UOMUse, FABRIC.nSellConv)
    *E038220,1 WSH [End]

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF !lfUpdA4Files(lcItem,"ITEM","STYLE","CINVTYPE+STYLE",LANG_CONVERT_ITEM+m.cinvtype+m.style,oFormSet)
IF !lfUpdA4Files(lcItem,"ITEM","STYLE","CINVTYPE+STYLE",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_ITEM,oFormSet.GetHeaderText("LANG_CONVERT_ITEM",oFormSet.HeaderAlias))+m.cinvtype+m.style,oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

      RETURN .F.
    ENDIF
  ENDSCAN

  *B131128,1 AMH Fix bug of Fabric file not found [Start]
  SET ORDER TO CFABRIC IN FABRIC
  *B131128,1 AMH [End]

ENDIF

*E038594,1 AMH Don't close the Style file here to give conversion more speed [Start]
*IF llUseStyle AND USED('STYLE')
*  USE IN STYLE
*ENDIF
*E038594,1 AMH [End]

*E038594,1 AMH Don't close the Fabric file here to give conversion more speed [Start]
*IF llUseFabric AND USED('FABRIC')
*  USE IN FABRIC
*ENDIF

*B131128,1 AMH Fix bug of Fabric file not found [Start]
*SET ORDER TO CFABRIC IN FABRIC
*B131128,1 AMH [End]

*E038594,1 AMH [End]

IF llUseFabDye AND USED('FABDYE')
  USE IN FABDYE
ENDIF

IF llUsePofLn AND USED('POFLN')
  USE IN POFLN
ENDIF

IF llUseMMfgOrdD AND USED('MMFGORDD')
  USE IN MMFGORDD
ENDIF

IF llUseMMfgOrdH AND USED('MMFGORDH')
  USE IN MMFGORDH
ENDIF

*E038594,1 AMH Don't close the codes file to give the conversion more speed [Start]
*IF llUseCodes AND USED('CODES')
*  USE IN CODES
*ENDIF
*E038594,1 AMH [End]

IF USED(lcItem)
  USE IN (lcItem)
ENDIF

*:*************************************************************
*: Name      : lfCnvItmLc
*: Developer : AHMED MAHER (AMH)
*: Date      : 04/13/2004
*: Purpose   : Convert ITEMLOC file.
*:*************************************************************
FUNCTION lfCnvItmLc
LPARAMETERS oFormSet

LOCAL lcItemLoc,llUseStyDye,llUseFabDye,llUseFabric,llUsePofLn,llUseMMfgOrdH,llUseMMfgOrdD,lnTotWo

lcItemLoc = gfTempName()
STORE .F. to llUseStyDye,llUseFabDye,llUseFabric,llUsePofLn,llUseMMfgOrdH,llUseMMfgOrdD

*--Comment in phase 1 to be uncoment in phase 2 [Start]
*!*  IF FILE(oAriaApplication.DataDir+'STYDYE'+'.DBF')
*!*    IF !gfOpenFile(oAriaApplication.DataDir+'STYDYE','STYDYE','SH')
*!*      =lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+'ITEMLOC'+LANG_CONVERT_NOTOPEN+'STYDYE'+;
*!*                  LANG_CONVERT_FILE2,oFormSet)
*!*      RETURN .F.
*!*    ELSE
*!*      llUseStyDye = .T.
*!*    ENDIF
*!*  ENDIF
*-- [End]

IF FILE(oAriaApplication.DataDir+'FABDYE'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'FABDYE','FABDYE','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'ITEMLOC'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'FABDYE'+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUseFabDye = .T.
  ENDIF
ENDIF

IF llUseFabDye AND FILE(oAriaApplication.DataDir+'FABRIC'+'.DBF')

  *E038594,1 AMH Don't Open the Fabric file here to give conversion more speed [Start]
  *IF !gfOpenFile(oAriaApplication.DataDir+'FABRIC','FABRIC','SH')
  *  =lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+'ITEMLOC'+LANG_CONVERT_NOTOPEN+'FABRIC'+;
  *              LANG_CONVERT_FILE2,oFormSet)
  *  RETURN .F.
  *ELSE
    llUseFabric = .T.
  *ENDIF
  SET ORDER TO FABRIC IN FABRIC
  *E038594,1 AMH [End]

ENDIF

IF llUseFabDye AND FILE(oAriaApplication.DataDir+'POFLN'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'POFLN','POFLNF','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'ITEMLOC'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'POFLN'+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)

    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUsePofLn = .T.
  ENDIF
ENDIF

IF llUseFabDye AND FILE(oAriaApplication.DataDir+'MMFGORDD'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'MMFGORDD','MMFGORDD','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'ITEMLOC'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'MMFGORDD'+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUseMMfgOrdD = .T.
  ENDIF
ENDIF

IF llUseFabDye AND FILE(oAriaApplication.DataDir+'MMFGORDH'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'MMFGORDH','MMFGORDH','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'ITEMLOC'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'MMFGORDH'+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUseMMfgOrdH = .T.
  ENDIF
ENDIF

IF !llUseStyDye AND !llUseFabDye

  *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
  *RETURN .F.
  RETURN .T.
  *B038828,1 AMH [End]

ENDIF

LOCAL lnStyRec,lnFabRec
STORE 0 TO lnStyRec,lnFabRec

IF llUseStyDye
  SELECT STYDYE
  COUNT TO lnStyRec
ENDIF

IF llUseFabDye
  SELECT FABDYE
  COUNT TO lnFabRec
ENDIF

oFormSet.lnRecCnt = lnStyRec + lnFabRec
oFormSet.lnCurRec = oFormSet.lnCurRec * oFormSet.lnRecCnt / 100

IF llUseStyDye
  SELECT STYDYE
  SCAN
    SCATTER MEMVAR MEMO

    *--Retreive Data from ITEMLOC file.
    ***--- fill this bluck in phase 2 [Start]
    ***--- [End]

    IF !lfUpdA4Files(lcItemLoc,"ITEMLOC","STYDYE","CINVTYPE+STYLE+CWARECODE+DYELOT",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_ITEM,oFormSet.GetHeaderText("LANG_CONVERT_ITEM",oFormSet.HeaderAlias))+m.cinvtype+;
        m.style+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_WARECODE,oFormSet.GetHeaderText("LANG_CONVERT_WARECODE",oFormSet.HeaderAlias))+m.cwarecode+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_DYELOT,oFormSet.GetHeaderText("LANG_CONVERT_DYELOT",oFormSet.HeaderAlias))+m.dyelot,oFormSet)
      RETURN .F.
    ENDIF
  ENDSCAN
ENDIF

IF llUseFabDye
  SELECT FABDYE
  SCAN
    SCATTER MEMVAR MEMO
    =SEEK(m.fabric+m.color,'FABRIC')
    lnTotWo      = 0
    IF FABRIC.MAKE

      *B038828,1 AMH Check if MMFGORDH file is ussed [Start]
      *IF SEEK(m.fabric,'MMFGORDH')
      IF llUseMMfgOrdH AND SEEK(m.fabric,'MMFGORDH')
      *B038828,1 AMH [End]

        SELECT MMFGORDH
        SCAN REST WHILE cfabric+cmfgordno = m.fabric FOR cwarecode = m.cwarecode

          *B038828,1 AMH Check if MMFGORDD file is ussed [Start]
          *IF SEEK(cmfgordno+cfabric+m.color,'MMFGORDD')
          IF llUseMMfgOrdD AND SEEK(cmfgordno+cfabric+m.color,'MMFGORDD')
          *B038828,1 AMH [End]

            SELECT MMFGORDD
            SCAN REST WHILE cmfgordno+cfabric+color+dyelot+trancd = MMFGORDH.CMFGORDNO+m.fabric+m.color+m.dyelot+'1'
              lnTotWo = lnTotWo + nmfgtotqty
            ENDSCAN
          ENDIF
        ENDSCAN
      ENDIF
    ELSE

      *B038828,1 AMH Check if POFLN file is ussed [Start]
      *IF SEEK(m.fabric+m.color+'P','POFLN')
      IF llUsePofLn AND SEEK(m.fabric+m.color+'P','POFLN')
      *B038828,1 AMH [End]

        SELECT POFLN
        SCAN REST WHILE fabric+color+cmattype+pomat+trancd = m.fabric+m.color+'P';
                    FOR dyelot = m.dyelot AND cwarecode = m.cwarecode AND trancd = '1'

          *E038594,1 AMH Get the corrent value [Start]
          *lnTotWo = lnTotWo + nfabtotqty
          lnTotWo = lnTotWo + (nfabtotqty*FABRIC.CONV)
          *E038594,1 AMH [End]

        ENDSCAN
      ENDIF
    ENDIF

    *--Retreive Data from ITEMLOC file.
    m.cinvtype   = '0002'
    m.style      = lfUpdItem(m.fabric,m.color)
    m.desc       = FABRIC.DESC
    m.stk1       = m.onhand
    m.totstk     = m.onhand
    m.shp1       = m.nsellship
    m.totshp     = m.nsellship
    m.ord1       = m.nsellorder
    m.totord     = m.nsellorder
    m.wip1       = m.onorder
    m.totwip     = m.onorder
    m.ave_cost   = m.nfave_cost
    m.intrans1   = m.matotintr
    m.totintrn   = m.matotintr
    m.nwo1       = lnTotWo
    m.ntotwo     = lnTotWo
    m.nhusage1   = m.usage
    m.ntothusage = m.usage
    m.ncusage1   = m.nmatwip
    m.ntotcusa   = m.nmatwip
    m.nonret1    = m.onret
    m.ntotonret  = m.onret

    IF !lfUpdA4Files(lcItemLoc,"ITEMLOC","STYDYE","CINVTYPE+STYLE+CWARECODE+DYELOT",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_ITEM,oFormSet.GetHeaderText("LANG_CONVERT_ITEM",oFormSet.HeaderAlias))+m.cinvtype+;
      m.style+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_WARECODE,oFormSet.GetHeaderText("LANG_CONVERT_WARECODE",oFormSet.HeaderAlias))+m.cwarecode+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_DYELOT,oFormSet.GetHeaderText("LANG_CONVERT_DYELOT",oFormSet.HeaderAlias))+m.dyelot,oFormSet)
      RETURN .F.
    ENDIF
  ENDSCAN
ENDIF

IF llUseStyDye AND USED('STYDYE')
  USE IN STYDYE
ENDIF

IF llUseFabDye AND USED('FABDYE')
  USE IN FABDYE
ENDIF

*E038594,1 AMH Don't close the Fabric file here to give conversion more speed [Start]
*IF llUseFabric AND USED('FABRIC')
*  USE IN FABRIC
*ENDIF

*B131128,1 AMH Fix bug of Fabric file not found [Start]
*SET ORDER TO CFABRIC IN FABRIC
IF llUseFabric
  SET ORDER TO CFABRIC IN FABRIC
ENDIF
*B131128,1 AMH [End]

*E038594,1 AMH [End]

IF llUsePofLn AND USED('POFLN')
  USE IN POFLN
ENDIF

IF llUseMMfgOrdD AND USED('MMFGORDD')
  USE IN MMFGORDD
ENDIF

IF llUseMMfgOrdH AND USED('MMFGORDH')
  USE IN MMFGORDH
ENDIF

IF USED(lcItemLoc)
  USE IN (lcItemLoc)
ENDIF

*:*************************************************************
*: Name      : lfCnvItmAd
*: Developer : AHMED MAHER (AMH)
*: Date      : 04/13/2004
*: Purpose   : Convert ITEMADJ file.
*:*************************************************************
FUNCTION lfCnvItmAd
LPARAMETERS oFormSet

LOCAL lcItemAdj,llUseInvtAdj,llUseFInvtAdj

lcItemAdj = gfTempName()
lcKeyVal  = gfTempName()
STORE .F. to llUseInvtAdj,llUseFInvtAdj

*--Comment in phase 1 to be uncoment in phase 2 [Start]
*!*  IF FILE(oAriaApplication.DataDir+'INVTADJ'+'.DBF')
*!*    IF !gfOpenFile(oAriaApplication.DataDir+'INVTADJ','INVTADJ','SH')
*!*      =lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+'ITEMADJ'+LANG_CONVERT_NOTOPEN+'INVTADJ'+;
*!*                  LANG_CONVERT_FILE2,oFormSet)
*!*      RETURN .F.
*!*    ELSE
*!*      llUseInvtAdj = .T.
*!*    ENDIF
*!*  ENDIF
*-- [End]

IF FILE(oAriaApplication.DataDir+'FINVTADJ'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'FINVTADJ','FINVTADJ','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'ITEMADJ'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'FINVTADJ'+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUseFInvtAdj = .T.
  ENDIF
ENDIF

IF !llUseInvtAdj AND !llUseFInvtAdj

  *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
  *RETURN .F.
  RETURN .T.
  *B038828,1 AMH [End]

ENDIF

LOCAL lnStyRec,lnFabRec
STORE 0 TO lnStyRec,lnFabRec

IF llUseInvtAdj
  SELECT INVTADJ
  COUNT TO lnStyRec
ENDIF

IF llUseFInvtAdj
  SELECT FINVTADJ
  COUNT TO lnFabRec
ENDIF

oFormSet.lnRecCnt = lnStyRec + lnFabRec
oFormSet.lnCurRec = oFormSet.lnCurRec * oFormSet.lnRecCnt / 100

IF llUseInvtAdj
  SELECT INVTADJ
  SCAN
    SCATTER MEMVAR MEMO

    *--Retreive Data from ITEMADJ file.
    ***--- fill this bluck in phase 2 [Start]
    ***--- [End]

    IF !lfUpdA4Files(lcItemAdj,"ITEMADJ","ITEMADJ","CSESSION+CINVTYPE+STYLE+CFROMWARE+DYELOT",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_ITEM,oFormSet.GetHeaderText("LANG_CONVERT_ITEM",oFormSet.HeaderAlias))+m.cinvtype+;
      m.style+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_WARECODE,oFormSet.GetHeaderText("LANG_CONVERT_WARECODE",oFormSet.HeaderAlias))+m.cfromware+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_DYELOT,oFormSet.GetHeaderText("LANG_CONVERT_DYELOT",oFormSet.HeaderAlias))+m.dyelot,oFormSet)
      RETURN .F.
    ENDIF
  ENDSCAN
ENDIF

IF llUseFInvtAdj
  SELECT FINVTADJ
  lnFlFld =AFIELDS(laFlFld)
  DIMENSION laFlFld[lnFlFld+1,18]
  laFlFld[lnFlFld+1,1] = "LineNo"
  laFlFld[lnFlFld+1,2] = "N"
  laFlFld[lnFlFld+1,3] = 6
  laFlFld[lnFlFld+1,4] = 0

  FOR lnI = 7 TO 16
    laFlFld[lnFlFld+1,lnI] = ""
  ENDFOR
  STORE 0 TO laFlFld[lnFlFld+1,17],laFlFld[lnFlFld+1,18]

  =gfCrtTmp(lcKeyVal,@laFlFld,"CTRN_SEQ+FABRIC+COLOR+CFROMWARE+DYELOT",lcKeyVal)
  SELECT FINVTADJ
  SCAN
    SCATTER MEMVAR MEMO
    SELECT (lcKeyVal)
    IF !SEEK(m.CTRN_SEQ+m.FABRIC+m.COLOR+m.CFROMWARE+m.DYELOT)
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE LineNo WITH 0
    ENDIF
    REPLACE LineNo WITH LineNo + 1
    SELECT FINVTADJ
    m.LineNo     = EVALUATE(lcKeyVal+'.LineNo')

    *--Retreive Data from ITEMADJ file.
    m.cinvtype   = '0002'
    m.style      = lfUpdItem(m.fabric,m.color)
    m.dpostdate  = m.date
    m.csession   = m.ctrn_seq
    m.unt_cost   = m.nfunitcost
    m.old_cost   = m.nfunitcost
    m.adj1       = m.nmtotadj
    m.totadj     = m.nmtotadj
    m.oldqty1    = m.oldqty
    m.totold     = m.oldqty
    m.noldto1    = m.noldtoqty
    m.ntotoldto  = m.noldtoqty

    IF !lfUpdA4Files(lcItemAdj,"ITEMADJ","ITEMADJ","CSESSION+CINVTYPE+STYLE+CFROMWARE+DYELOT+STR(LINENO,6)",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_ITEM,oFormSet.GetHeaderText("LANG_CONVERT_ITEM",oFormSet.HeaderAlias))+m.cinvtype+;
      m.style+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_WARECODE,oFormSet.GetHeaderText("LANG_CONVERT_WARECODE",oFormSet.HeaderAlias))+m.cfromware+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_DYELOT,oFormSet.GetHeaderText("LANG_CONVERT_DYELOT",oFormSet.HeaderAlias))+m.dyelot,oFormSet)
      RETURN .F.
    ENDIF
  ENDSCAN
ENDIF

IF llUseInvtAdj AND USED('INVTADJ')
  USE IN INVTADJ
ENDIF

IF llUseFInvtAdj AND USED('FINVTADJ')
  USE IN FINVTADJ
ENDIF

IF USED(lcItemAdj)
  USE IN (lcItemAdj)
ENDIF

IF USED(lcKeyVal)
  USE IN (lcKeyVal)
ENDIF

*:*************************************************************
*: Name      : lfCnvItmJr
*: Developer : AHMED MAHER (AMH)
*: Date      : 04/04/2004
*: Purpose   : Convert ITEMJRNL file.
*:*************************************************************
FUNCTION lfCnvItmJr
LPARAMETERS oFormSet

LOCAL lcItemJrnl,llUseStyInvJl,llUseMatInvJl,llUsePofLn,llUseMMfgOrdD,llUseMaRwLin,llUseFabric

lcItemJrnl = gfTempName()
lcKeyVal   = gfTempName()
STORE .F. to llUseStyInvJl,llUseMatInvJl,llUsePofLn,llUseMMfgOrdD,llUseMaRwLin,llUseFabric

*--Comment in phase 1 to be uncoment in phase 2 [Start]
*!*  IF FILE(oAriaApplication.DataDir+'STYINVJL'+'.DBF')
*!*    IF !gfOpenFile(oAriaApplication.DataDir+'STYINVJL','STYINVJL','SH')
*!*      =lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+'ITEMJRNL'+LANG_CONVERT_NOTOPEN+'STYINVJL'+;
*!*                  LANG_CONVERT_FILE2,oFormSet)
*!*      RETURN .F.
*!*    ELSE
*!*      llUseStyInvJl = .T.
*!*    ENDIF
*!*  ENDIF
*-- [End]

IF FILE(oAriaApplication.DataDir+'MATINVJL'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'MATINVJL','MATINVJL','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'ITEMJRNL'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'MATINVJL'+;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUseMatInvJl = .T.
  ENDIF
ENDIF

IF llUseMatInvJl AND FILE(oAriaApplication.DataDir+'POFLN'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'POFLN','POFREC','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'ITEMJRNL'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'POFLN'+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUsePofLn = .T.
  ENDIF
ENDIF

IF llUseMatInvJl AND FILE(oAriaApplication.DataDir+'MMFGORDD'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'MMFGORDD','MFGREC','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'ITEMJRNL'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'MFGREC'+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUseMMfgOrdD = .T.
  ENDIF
ENDIF

IF llUseMatInvJl AND FILE(oAriaApplication.DataDir+'MARWLIN'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'MARWLIN','MFGREC','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'ITEMJRNL'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'MFGREC'+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)

   *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
    *RETURN .F.
    RETURN .T.
    *B038828,1 AMH [End]

  ELSE
    llUseMaRwLin = .T.
  ENDIF
ENDIF

IF llUseMatInvJl AND FILE(oAriaApplication.DataDir+'FABRIC'+'.DBF')

  *E038594,1 AMH Don't Open the Fabric file here to give conversion more speed [Start]
  *IF !gfOpenFile(oAriaApplication.DataDir+'FABRIC','CFABRIC','SH')
  *  =lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+'ITEMJRNL'+LANG_CONVERT_NOTOPEN+'FABRIC'+;
  *              LANG_CONVERT_FILE2,oFormSet)
  *  RETURN .F.
  *ELSE
    llUseFabric = .T.
  *ENDIF
  *E038594,1 AMH [End]

ENDIF

IF !llUseStyInvJl AND !llUseMatInvJl

  *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
  *RETURN .F.
  RETURN .T.
  *B038828,1 AMH [End]

ENDIF

LOCAL lnStyRec,lnFabRec
STORE 0 TO lnStyRec,lnFabRec

IF llUseStyInvJl
  SELECT STYINVJL
  COUNT TO lnStyRec
ENDIF

IF llUseMatInvJl
  SELECT MATINVJL
  COUNT TO lnFabRec
ENDIF

oFormSet.lnRecCnt = lnStyRec + lnFabRec
oFormSet.lnCurRec = oFormSet.lnCurRec * oFormSet.lnRecCnt / 100

IF llUseStyInvJl
  SELECT STYINVJL
  SCAN
    SCATTER MEMVAR MEMO

    *--Retreive Data from ITEMJRNL file.
    ***--- fill this bluck in phase 2 [Start]
    ***--- [End]

    IF !lfUpdA4Files(lcItemJrnl,"ITEMJRNL","STYINVJL","CINVTYPE+STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_ITEM,oFormSet.GetHeaderText("LANG_CONVERT_ITEM",oFormSet.HeaderAlias))+m.cinvtype+;
     m.style+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_WARECODE,oFormSet.GetHeaderText("LANG_CONVERT_WARECODE",oFormSet.HeaderAlias))+m.cwarecode+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_DYELOT,oFormSet.GetHeaderText("LANG_CONVERT_DYELOT",oFormSet.HeaderAlias))+m.cdyelot,oFormSet)
      RETURN .F.
    ENDIF
  ENDSCAN
ENDIF

IF llUseMatInvJl
  SELECT MATINVJL
  lnFlFld =AFIELDS(laFlFld)
  DIMENSION laFlFld[lnFlFld+2,18]
  laFlFld[lnFlFld+1,1] = "nLineNo"
  laFlFld[lnFlFld+1,2] = "N"
  laFlFld[lnFlFld+1,3] = 6
  laFlFld[lnFlFld+1,4] = 0
  laFlFld[lnFlFld+2,1] = "cIrType"
  laFlFld[lnFlFld+2,2] = "C"
  laFlFld[lnFlFld+2,3] = 1
  laFlFld[lnFlFld+2,4] = 0

  FOR lnI = 7 TO 16
    laFlFld[lnFlFld+1,lnI] = ""
    laFlFld[lnFlFld+2,lnI] = ""
  ENDFOR
  STORE 0 TO laFlFld[lnFlFld+1,17],laFlFld[lnFlFld+1,18],laFlFld[lnFlFld+2,17],laFlFld[lnFlFld+2,18]

  =gfCrtTmp(lcKeyVal,@laFlFld,"CFABRIC+CCOLOR+CWARECODE+DTOS(DTRANDATE)+CTRN_SEQ+CIRTYPE+CTRAN+STR(LINENO,6)+CTRANTYPE",lcKeyVal)
  SELECT MATINVJL
  SCAN
    SCATTER MEMVAR MEMO
    SELECT (lcKeyVal)
    IF !SEEK(m.CFABRIC+m.CCOLOR+m.CWARECODE+DTOS(m.DTRANDATE)+m.CTRN_SEQ+IIF(m.nreceived=0,'I','R')+m.CTRAN+STR(m.LINENO,6)+m.CTRANTYPE)
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE nLineNo WITH 0 cIrType WITH IIF(m.nreceived=0,'I','R')
    ENDIF
    REPLACE nLineNo WITH nLineNo + 1
    SELECT MATINVJL
    m.nLineNo    = EVALUATE(lcKeyVal+'.nLineNo')

    =SEEK(m.cfabric,'FABRIC')
    m.cbusdocu = ''
    m.cstytype = ''
    m.ctrtype  = ''
    DO CASE
      CASE m.ctrantype = '1'
        IF FABRIC.MAKE

          *B038828,1 AMH Check if MMFGORDD file is ussed [Start]
          *IF SEEK(m.ctrn_seq+m.ctran,'MMFGORDD')
          IF llUseMMfgOrdD AND SEEK(m.ctrn_seq+m.ctran,'MMFGORDD')
          *B038828,1 AMH [End]

            SELECT MMFGORDD
            LOCATE REST WHILE crsession+cmfgordno+cfabric+color+trancd = m.ctrn_seq+m.ctran;
                          FOR cfabgrade = FABRIC.CFABGRADE AND IIF(EMPTY(cfabric1),cfabric=m.cfabric AND color=m.ccolor,;
                              cfabric1=m.cfabric AND ccolor1=m.ccolor)
            IF FOUND()
              m.ctrtype = '5'
              m.cbusdocu = 'P'
              m.cstytype = 'F'
            ENDIF
          ELSE

            *B038828,1 AMH Check if MARWLIN file is ussed [Start]
            *IF SEEK(m.ctrn_seq+m.ctran,'MARWLIN')
            IF llUseMaRwLin AND SEEK(m.ctrn_seq+m.ctran,'MARWLIN')
            *B038828,1 AMH [End]

              SELECT MARWLIN
              LOCATE REST WHILE crsession+cmfgrwnum+cfabric+color+trancd = m.ctrn_seq+m.ctran;
                            FOR cfabgrade = FABRIC.CFABGRADE AND IIF(EMPTY(cfabric1),cfabric=m.cfabric AND color=m.ccolor,;
                                cfabric1=m.cfabric AND ccolor1=m.ccolor)
              IF FOUND()
                m.ctrtype  = IIF(trancd='3','5','6')
                m.cbusdocu = 'R'
                m.cstytype = 'W'
              ENDIF
            ENDIF
          ENDIF
        ELSE

          *B038828,1 AMH Check if POFLN file is ussed [Start]
          *IF SEEK('P'+m.ctran+m.ctrn_seq,'POFLN')
          IF llUsePofLn AND SEEK('P'+m.ctran+m.ctrn_seq,'POFLN')
          *B038828,1 AMH [End]

            SELECT POFLN
            LOCATE REST WHILE cmattype+pomat+crsession+fabric+color+trancd = 'P'+m.ctran+m.ctrn_seq;
                          FOR cfabgrade = FABRIC.CFABGRADE AND IIF(EMPTY(cfabric1),fabric=m.cfabric AND color=m.ccolor,;
                              cfabric1=m.cfabric AND ccolor1=m.ccolor)
            IF FOUND()
              m.ctrtype  = '5'
              m.cbusdocu = 'P'
              m.cstytype = 'M'
            ENDIF
          ELSE

            *B038828,1 AMH Check if POFLN file is ussed [Start]
            *IF SEEK('R'+m.ctran+m.ctrn_seq,'POFLN')
            IF llUsePofLn AND SEEK('R'+m.ctran+m.ctrn_seq,'POFLN')
            *B038828,1 AMH [End]

              SELECT POFLN
              LOCATE REST WHILE cmattype+pomat+crsession+fabric+color+trancd = 'R'+m.ctran+m.ctrn_seq;
                            FOR cfabgrade = FABRIC.CFABGRADE AND IIF(EMPTY(cfabric1),fabric=m.cfabric AND color=m.ccolor,;
                                cfabric1=m.cfabric AND ccolor1=m.ccolor)
              IF FOUND()
                m.ctrtype  = '6'
                m.cbusdocu = 'R'
                m.cstytype = 'M'
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      CASE m.ctrantype = '2'
        m.ctrtype = '1'
      CASE m.ctrantype = '3'
        m.ctrtype = '2'
      CASE m.ctrantype = '4'
        m.ctrtype = '9'
        DO CASE
          CASE m.cimtyp = 'I'
            m.cbusdocu = 'P'
            m.cstytype = 'P'
          CASE m.cimtyp = 'M'
            m.cbusdocu = 'P'
            m.cstytype = 'U'
          CASE m.cimtyp = 'T'
            m.cbusdocu = 'P'
            m.cstytype = 'F'
          CASE m.cimtyp = 'D'
            m.cbusdocu = 'P'
            m.cstytype = 'D'
        ENDCASE
      CASE m.ctrantype = '5'
        m.ctrtype = IIF(m.nreceived=0,'3','4')
    ENDCASE

    *--Retreive Data from ITEMJRNL file.
    m.cinvtype   = '0002'
    m.csession   = m.ctrn_seq
    m.style      = lfUpdItem(m.cfabric,m.ccolor)
    m.dtrdate    = m.dtrandate
    m.ctrcode    = m.ctran
    m.ncost      = m.nunitcost
    m.cirtype    = IIF(m.nreceived=0,'I','R')
    m.nstk1      = m.nreceived - m.nissued
    m.ntotstk    = m.nreceived - m.nissued
    m.nprvsqty   = m.nmprvsqty
    m.cadjacct   = m.cglmatadj
    m.cicacnt    = m.cmicacct

    IF !lfUpdA4Files(lcItemJrnl,"ITEMJRNL","STYDATE","CINVTYPE+STYLE+CWARECODE+DTOS(DTRDATE)+CSESSION+CIRTYPE+CTRCODE+STR(LINENO,6)+CTRTYPE+STR(NLINENO,6)",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_ITEM,oFormSet.GetHeaderText("LANG_CONVERT_ITEM",oFormSet.HeaderAlias))+m.cinvtype+;
      m.style+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_WARECODE,oFormSet.GetHeaderText("LANG_CONVERT_WARECODE",oFormSet.HeaderAlias))+m.cwarecode+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_DYELOT,oFormSet.GetHeaderText("LANG_CONVERT_DYELOT",oFormSet.HeaderAlias))+m.cdyelot,oFormSet)
      RETURN .F.
    ENDIF
  ENDSCAN
ENDIF

IF llUseStyInvJl AND USED('STYINVJL')
  USE IN STYINVJL
ENDIF

IF llUseMatInvJl AND USED('MATINVJL')
  USE IN MATINVJL
ENDIF

IF llUsePofLn AND USED('POFLN')
  USE IN POFLN
ENDIF

IF llUseMMfgOrdD AND USED('MMFGORDD')
  USE IN MMFGORDD
ENDIF

IF llUseMaRwLin AND USED('MARWLIN')
  USE IN MARWLIN
ENDIF

*E038594,1 AMH Don't close the Fabric file here to give conversion more speed [Start]
*IF llUseFabric AND USED('FABRIC')
*  USE IN FABRIC
*ENDIF
*E038594,1 AMH [End]

IF USED(lcItemJrnl)
  USE IN (lcItemJrnl)
ENDIF

IF USED(lcKeyVal)
  USE IN (lcKeyVal)
ENDIF

*:*************************************************************
*: Name      : lfCnvSgVal
*: Developer : AHMED MAHER (AMH)
*: Date      : 04/14/2004
*: Purpose   : Convert ICSEGVAL file.
*:*************************************************************
FUNCTION lfCnvSgVal
LPARAMETERS oFormSet

LOCAL lcIcSegVal,llUseIcSegVal,llUseFabric

lcIcSegVal = gfTempName()
STORE .F. to llUseIcSegVal,llUseFabric

*--Comment in phase 1 to be uncoment in phase 2 [Start]
*!*  IF FILE(oAriaApplication.DataDir+'ICSEGVAL'+'.DBF')
*!*    IF !gfOpenFile(oAriaApplication.DataDir+'ICSEGVAL','SEGVAL','SH')
*!*      =lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+'ICSEGVAL'+LANG_CONVERT_NOTOPEN+'ICSEGVAL'+;
*!*                  LANG_CONVERT_FILE2,oFormSet)
*!*      RETURN .F.
*!*    ELSE
*!*      llUseIcSegVal = .T.
*!*    ENDIF
*!*  ENDIF
*-- [End]

IF FILE(oAriaApplication.DataDir+'FABRIC'+'.DBF')

  *E038594,1 AMH Don't Open the Fabric file here to give conversion more speed [Start]
  *IF !gfOpenFile(oAriaApplication.DataDir+'FABRIC','CFABRIC','SH')
  *  =lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+'ICSEGVAL'+LANG_CONVERT_NOTOPEN+'FABRIC'+;
  *              LANG_CONVERT_FILE2,oFormSet)
  *  RETURN .F.
  *ELSE
    llUseFabric = .T.
  *ENDIF
  *E038594,1 AMH [End]

ENDIF

IF !llUseIcSegVal AND !llUseFabric

  *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
  *RETURN .F.
  RETURN .T.
  *B038828,1 AMH [End]

ENDIF

LOCAL lnStyRec,lnFabRec
STORE 0 TO lnStyRec,lnFabRec

IF llUseIcSegVal
  SELECT ICSEGVAL
  COUNT TO lnStyRec
ENDIF

IF llUseFabric
  *-- Get the Material Code structure.
  LOCAL oGetItemMask,lnI,lnRemItem,lnSegCnt
  oGetItemMask = CREATEOBJECT('GetItemMask')
  LOCAL ARRAY laItemSeg[1,1],laUseSeg[1,3]
  = oGetItemMask.Do(@laItemSeg,'','0002')
  lnRemItem = 7
  lnSegCnt  = 1
  FOR lnI = 1 TO ALEN(laItemSeg,1)
    IF laItemSeg[lnI,1] $ 'FO' AND lnRemItem > 0
      IF EMPTY(laUseSeg[1,1])
        laUseSeg[1,1] = 1
      ELSE
        lnSegCnt = lnSegCnt + 1
        DIMENSION laUseSeg[lnSegCnt,2]
        laUseSeg[lnSegCnt,1] = laUseSeg[lnSegCnt-1,1] + laUseSeg[lnSegCnt-1,2]
      ENDIF
      laUseSeg[lnSegCnt,2] = MIN(lnRemItem,LEN(laItemSeg[lnI,3]))
      laUseSeg[lnSegCnt,3] = STR(lnI,1)
      lnRemItem = lnRemItem - LEN(laItemSeg[lnI,3])
    ENDIF
  ENDFOR

  SELECT FABRIC
  COUNT TO lnFabRec
  lnFabRec = lnFabRec * lnSegCnt
ENDIF

oFormSet.lnRecCnt = lnStyRec + lnFabRec
oFormSet.lnCurRec = oFormSet.lnCurRec * oFormSet.lnRecCnt / 100

IF llUseIcSegVal
  SELECT ICSEGVAL
  SCAN
    SCATTER MEMVAR MEMO

    *--Retreive Data from ICSEGVAL file.
    ***--- fill this bluck in phase 2 [Start]
    ***--- [End]

    IF !lfUpdA4Files(lcIcSegVal,"ICSEGVAL","SEGVAL","CINVTYPE+CISEGNO+CISEGVAL",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_SEGMENT,oFormSet.GetHeaderText("LANG_CONVERT_SEGMENT",oFormSet.HeaderAlias))+m.cinvtype+;
                     m.cisegno+m.cisegval,oFormSet)
      RETURN .F.
    ENDIF
  ENDSCAN
ENDIF

IF llUseFabric
  SELECT FABRIC
  SCAN
    SCATTER MEMVAR MEMO

    FOR lnI = 1 TO lnSegCnt
      *--Retreive Data from ICSEGVAL file.
      m.cisegno    = laUseSeg[lnI,3]
      m.cisegval   = SUBSTR(m.fabric,laUseSeg[lnI,1],laUseSeg[lnI,2])
      m.cisgvalsd  = m.desc
      m.cisgvalld  = m.desc
      m.cinvtype   = '0002'

      IF !lfUpdA4Files(lcIcSegVal,"ICSEGVAL","SEGVAL","CINVTYPE+CISEGNO+CISEGVAL",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_SEGMENT,oFormSet.GetHeaderText("LANG_CONVERT_SEGMENT",oFormSet.HeaderAlias))+m.cinvtype+;
                       m.cisegno+m.cisegval,oFormSet)
        RETURN .F.
      ENDIF
    ENDFOR
  ENDSCAN
ENDIF

IF llUseIcSegVal AND USED('ICSEGVAL')
  USE IN ICSEGVAL
ENDIF

*E038594,1 AMH Don't close the Fabric file here to give conversion more speed [Start]
*IF llUseFabric AND USED('FABRIC')
*  USE IN FABRIC
*ENDIF
*E038594,1 AMH [End]

IF USED(lcIcSegVal)
  USE IN (lcIcSegVal)
ENDIF

*:*************************************************************
*: Name      : lfCnvBomLn
*: Developer : AHMED MAHER (AMH)
*: Date      : 05/10/2005
*: Purpose   : Convert BOMLINE file.
*:*************************************************************
FUNCTION lfCnvBomLn
LPARAMETERS oFormSet

LOCAL lnLineNo,lcBomLine,llUseBomLine,lcKeyVal,lnFlFld

lnLineNo = 0
lcBomLine = gfTempName()
lcKeyVal  = gfTempName()
STORE .F. to llUseBomLine

IF FILE(oAriaApplication.DataDir+'BOMLINE'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'BOMLINE','','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'BOMLINE'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'BOMLINE'+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    RETURN .F.
  ELSE
    llUseBomLine = .T.
  ENDIF
ENDIF

IF !llUseBomLine
  RETURN .T.
ENDIF


*: B608646,1 MMT 08/07/2008 Convert Fill Currency fields in BOM and Bomline Tables[Start]
llUsePosHdrFile = .F.
IF FILE(oAriaApplication.DataDir+'POSHDR'+'.DBF')
  IF !gfOpenFile(oAriaApplication.DataDir+'POSHDR','POSHDR','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'POSHDR'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'POSHDR'+;
   IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
  ELSE
    llUsePosHdrFile = .T.
  ENDIF
ENDIF
*: B608646,1 MMT 08/07/2008 Convert Fill Currency fields in BOM and Bomline Tables[End]



SELECT BOMLINE
COUNT TO oFormSet.lnRecCnt
oFormSet.lnCurRec = oFormSet.lnCurRec * oFormSet.lnRecCnt / 100

SELECT BOMLINE
lnFlFld =AFIELDS(laFlFld)

*: B608646,1 MMT 08/07/2008 Convert Fill Currency fields in BOM and Bomline Tables[Start]
*DIMENSION laFlFld[lnFlFld+1,18]
DIMENSION laFlFld[lnFlFld+4,18]
*: B608646,1 MMT 08/07/2008 Convert Fill Currency fields in BOM and Bomline Tables[End]

laFlFld[lnFlFld+1,1] = "nLineNo"
laFlFld[lnFlFld+1,2] = "N"
laFlFld[lnFlFld+1,3] = 6
laFlFld[lnFlFld+1,4] = 0

*: B608646,1 MMT 08/07/2008 Convert Fill Currency fields in BOM and Bomline Tables[Start]
laFlFld[lnFlFld+2,1] = "nexrate"
laFlFld[lnFlFld+2,2] = "N"
laFlFld[lnFlFld+2,3] = 9
laFlFld[lnFlFld+2,4] = 4

laFlFld[lnFlFld+3,1] = "ncurrunit"
laFlFld[lnFlFld+3,2] = "N"
laFlFld[lnFlFld+3,3] = 4
laFlFld[lnFlFld+3,4] = 0

laFlFld[lnFlFld+4,1] = "ccurrcode"
laFlFld[lnFlFld+4,2] = "C"
laFlFld[lnFlFld+4,3] = 3
laFlFld[lnFlFld+4,4] = 0
*: B608646,1 MMT 08/07/2008 Convert Fill Currency fields in BOM and Bomline Tables[End]

*: B608646,1 MMT 08/07/2008 Convert Fill Currency fields in BOM and Bomline Tables[Start]
*!*	FOR lnI = 7 TO 16
*!*	  laFlFld[lnFlFld+1,lnI] = ""
*!*	ENDFOR
*!*	STORE 0 TO laFlFld[lnFlFld+1,17],laFlFld[lnFlFld+1,18]
FOR lnJ = 1 TO 4
  FOR lnI = 7 TO 16
    laFlFld[lnFlFld+lnJ ,lnI] = ""
  ENDFOR
  STORE 0 TO laFlFld[lnFlFld+lnJ ,17],laFlFld[lnFlFld+lnJ ,18]
ENDFOR
*: B608646,1 MMT 08/07/2008 Convert Fill Currency fields in BOM and Bomline Tables[End]

=gfCrtTmp(lcKeyVal,@laFlFld,"CIMTYP+CTYPE+CTKTNO+SHIPNO+STR(LINENO,6)+CBOMTYP+STYLE+SCLR+ITEM+ICLR+MFGCODE+CRSESSION+CSTYGRADE",lcKeyVal)
SELECT BOMLINE
SCAN
  SCATTER MEMVAR MEMO
  SELECT (lcKeyVal)
  IF !SEEK(m.cImTyp+m.cType+m.cTktNo+m.ShipNo+STR(m.LineNo,6)+m.cBomTyp+m.Style+m.SClr+m.Item+m.IClr+m.MfgCode+m.cRsession+m.cStyGrade)
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE nLineNo WITH 0
  ENDIF
  REPLACE nLineNo WITH nLineNo + 1

  *: B608646,1 MMT 08/07/2008 Convert Fill Currency fields in BOM and Bomline Tables[Start]
  IF (EMPTY(ccurrcode) OR ISNULL(ccurrcode))
    IF  m.cimtyp = 'I' AND m.ccatgtyp  $ 'PDM' AND llUsePosHdrFile
      IF SEEK('P'+m.Ctktno,'POSHDR')
        IF m.ccatgtyp  = 'P'
           REPLACE ccurrcode  WITH POSHDR.cpricecur,;
                   NCURRUNIT  WITH POSHDR.NCURRUNIT ,;
                   nExRate	  WITH POSHDR.NPRICERAT


        ELSE
          IF m.ccatgtyp  $ 'DM'
            REPLACE ccurrcode  WITH POSHDR.cdutycur,;
                   NCURRUNIT  WITH POSHDR.NDCURUNIT ,;
                   nExRate	  WITH POSHDR.NDUTYRAT
          ELSE
            REPLACE ccurrcode WITH oAriaApplication.BaseCurrency,;
                	NCURRUNIT WITH 1,;
               	    nExRate	  WITH 1
          ENDIF
        ENDIF
      ELSE
        REPLACE ccurrcode WITH oAriaApplication.BaseCurrency,;
                NCURRUNIT WITH 1,;
                nExRate	  WITH 1

      ENDIF
    ELSE
        REPLACE ccurrcode WITH oAriaApplication.BaseCurrency,;
                NCURRUNIT WITH 1,;
                nExRate	  WITH 1
    ENDIF
  ENDIF
  m.ccurrcode  =EVALUATE(lcKeyVal+'.ccurrcode')
  m.NCURRUNIT =EVALUATE(lcKeyVal+'.NCURRUNIT')
  m.nExRate	=EVALUATE(lcKeyVal+'.nExRate')
  *: B608646,1 MMT 08/07/2008 Convert Fill Currency fields in BOM and Bomline Tables[End]

  SELECT BOMLINE

  m.nLineNo    = EVALUATE(lcKeyVal+'.nLineNo')
  m.cCompSizes = lfGetCmpSz()
  m.cInvTypC   = IIF(CCATGTYP='S','0001',IIF(CCATGTYP$'FT','0002',''))
  m.cInvType   = IIF(CIMTYP='T','0002','0001')
  m.cSizes     = lfGetSzLn()
  m.Item       = lfGetItem('m.cCatgTyp$"FT"',m.Item,m.IClr)
  m.Style      = lfGetItem('m.cImTyp="T"',m.Style,m.SClr)

  IF !lfUpdA4Files(lcBomLine,"BOMLINE","BOMLINEU","CIMTYP+CTYPE+CTKTNO+SHIPNO+STR(LINENO,6)+CBOMTYP+CINVTYPE+STYLE+CINVTYPC+ITEM+MFGCODE+CRSESSION+CSTYGRADE+STR(NLINENO,6)",;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_RECNO,oFormSet.GetHeaderText("LANG_CONVERT_RECNO",oFormSet.HeaderAlias))+ALLTRIM(STR(RECNO())),oFormSet)
    RETURN .F.
  ENDIF
ENDSCAN

IF USED('BOMLINE')
  USE IN BOMLINE
ENDIF

IF USED(lcBomLine)
  USE IN (lcBomLine)
ENDIF

*! B609467,1 MMT 11/25/2010 Manufacture Module does not converted automatically using aria4 conversion[Start]
IF USED('POSHDR')
  USE IN POSHDR
ENDIF
*! B609467,1 MMT 11/25/2010 Manufacture Module does not converted automatically using aria4 conversion[End]

IF USED(lcKeyVal)
  USE IN (lcKeyVal)
ENDIF

***************************************************************
***                 Special Handling Fields                 ***
***************************************************************

*:*************************************************************
*: Name      : lfUpdCstyp
*: Developer : AHMED MAHER (AMH)
*: Date      : 03/21/2004
*: Purpose   : Update CCSTSHTTYP in the BOM file.
*:*************************************************************
FUNCTION lfUpdCstyp
LPARAMETERS lcCITMMAJOR

LOCAL lnAlias,lcRet
lnAlias = SELECT(0)

*E038594,1 AMH Don't Open the Style file here to give conversion more speed [Start]
*=gfOpenFile(oAriaApplication.DataDir+'STYLE','CSTYLE','SH')
*E038594,1 AMH [End]

SELECT STYLE
=SEEK(lcCITMMAJOR)
lcRet = IIF(MAKE,'M','I')

*E038594,1 AMH Done close style file to be used again [Start]
*USE IN STYLE
*E038594,1 AMH [End]

SELECT (lnAlias)
RETURN lcRet

*:*************************************************************
*: Name      : lfGetItem
*: Developer : AHMED MAHER (AMH)
*: Date      : 04/06/2004
*: Purpose   : Update ITEM in the BOM file.
*:*************************************************************
FUNCTION lfGetItem
LPARAMETERS lcCond,lcItem,lcColor

LOCAL lnAlias,lcRet
lnAlias = SELECT(0)

IF EVALUATE(lcCond)
  lcRet = lfUpdItem(lcItem,lcColor)
ELSE
  lcRet = lcItem
ENDIF

SELECT (lnAlias)
RETURN lcRet

*:*************************************************************
*: Name      : lfGetMask
*: Developer : AHMED MAHER (AMH)
*: Date      : 04/06/2004
*: Purpose   : Update CITMMASK in the BOM file.
*:*************************************************************
FUNCTION lfGetMask

LOCAL lnAlias,lcRet
lnAlias = SELECT(0)

IF m.lmaterial
  lcRet = lfUpdItem(m.citmmajor,m.citmmask)
ELSE
  lcRet = m.citmmask
ENDIF

SELECT (lnAlias)
RETURN lcRet

*:*************************************************************
*: Name      : lfGetSizes
*: Developer : AHMED MAHER (AMH)
*: Date      : 04/06/2004
*: Purpose   : Update MSIZES in the BOM file.
*:*************************************************************
FUNCTION lfGetSizes

*E038594,1 AMH Get the setup of use ex. size scale at the start of conversion procces [Start]
*LOCAL lnAlias,lcRet,llSUseExSS,lcMScale,lcSScale,lnI
LOCAL lnAlias,lcRet,lcMScale,lcSScale,lnI
*E038594,1 AMH [End]

lnAlias = SELECT(0)
lcRet = m.msizes

*E038594,1 AMH Get the setup of use ex. size scale at the start of conversion procces [Start]
*llSUseExSS = gfGetMemVar('M_USEEXSSC',oAriaApplication.ActiveCompanyID)
*E038594,1 AMH [End]

lcMScale   = '*  '

IF EMPTY(lcRet)
  IF m.lmaterial
    IF m.ccatgtyp $ 'FT'
      lcRet = lcMScale + '~1'
    ENDIF
  ELSE
    IF m.ccatgtyp $ 'FT'

      *E038594,1 AMH Don't Open the Style and Scale files here to give conversion more speed [Start]
      *=gfOpenFile(oAriaApplication.DataDir+'STYLE','CSTYLE','SH')
      *=gfOpenFile(oAriaApplication.DataDir+'SCALE','SCALE','SH')
      *E038594,1 AMH [End]

      SELECT STYLE
      =SEEK(m.citmmajor)
      lcSScale = STYLE.SCALE
      SELECT SCALE
      =SEEK('S'+lcSScale)
      IF llSUseExSS
        SET ORDER TO STYLE IN STYLE
        SELECT STYLE
        SCAN REST WHILE CSTYMAJOR = m.citmmajor
          lcSScale = SCALE
          =SEEK('S'+lcSScale,'SCALE')
          lcRet = lcSScale + '~1'
          FOR lnI = 2 TO SCALE.CNT
            lcRet = lcRet + ',' + STR(lnI,1)
          ENDFOR
          lcRet = lcRet + CHR(13)
        ENDSCAN

        *E038594,1 AMH Restore the old order of style file [Start]
        SET ORDER TO CSTYLE IN STYLE
        *E038594,1 AMH [End]

      ELSE
        lcRet = lcSScale + '~1'
        FOR lnI = 2 TO SCALE.CNT
          lcRet = lcRet + ',' + STR(lnI,1)
        ENDFOR
      ENDIF

      *E038594,1 AMH Don't close the style and scale file [Start]
      *USE IN SCALE
      *USE IN STYLE
      *E038594,1 AMH [End]

    ENDIF
  ENDIF
ENDIF

SELECT (lnAlias)
RETURN lcRet

*:*************************************************************
*: Name      : lfGetCrsRf
*: Developer : AHMED MAHER (AMH)
*: Date      : 04/07/2004
*: Purpose   : Update MSZCROSREF in the BOM file.
*:*************************************************************
FUNCTION lfGetCrsRf

*E038594,1 AMH Get the setup of use ex. size scale at the start of conversion procces [Start]
*LOCAL lnAlias,lcRet,llSUseExSS,lcMScale,lcSScale,lnI
LOCAL lnAlias,lcRet,lcMScale,lcSScale,lnI
*E038594,1 AMH [End]

lnAlias = SELECT(0)
lcRet = m.mszcrosref

*E038594,1 AMH Get the setup of use ex. size scale at the start of conversion procces [Start]
*llSUseExSS = gfGetMemVar('M_USEEXSSC',oAriaApplication.ActiveCompanyID)
*E038594,1 AMH [End]


lcMScale   = '*  '

IF EMPTY(lcRet)
  IF m.lmaterial
    IF m.ccatgtyp $ 'FT'
      lcRet = lcMScale + ',1~' + lcMScale + ',1' + CHR(13)
    ENDIF
  ELSE
    IF m.ccatgtyp $ 'FT'

      *E038594,1 AMH Don't Open the Style and Scale files here to give conversion more speed [Start]
      *=gfOpenFile(oAriaApplication.DataDir+'STYLE','CSTYLE','SH')
      *=gfOpenFile(oAriaApplication.DataDir+'SCALE','SCALE','SH')
      *E038594,1 AMH [End]

      SELECT STYLE
      =SEEK(m.citmmajor)
      lcSScale = STYLE.SCALE
      SELECT SCALE
      =SEEK('S'+lcSScale)
      IF llSUseExSS
        *! B609452,1 MMT 1/08/2010 Conversion program converts BOM.MSZCROSREF incorrectly for extended size scale company[Start]
*!*	        SET ORDER TO STYLE IN STYLE
*!*	        SELECT STYLE
*!*	        SCAN REST WHILE CSTYMAJOR = m.citmmajor
*!*	          lcSScale = SCALE
*!*	          =SEEK('S'+lcSScale,'SCALE')
*!*	          lcRet = lcSScale + ',1~' + lcMScale + ',1' + CHR(13)
*!*	          FOR lnI = 2 TO SCALE.CNT
*!*	            lcRet = lcRet + lcSScale + ',' + STR(lnI,1) + '~' + lcMScale + ',1' + CHR(13)
*!*	          ENDFOR
*!*	        ENDSCAN
        DIMENSION laSizesArr[1]
        laSizesArr = ''
        =gfSubStr(m.msizes ,@laSizesArr,CHR(13))
        FOR lnA =1 TO ALEN(laSizesArr,1)
          lnSclStrt = ATC('~',laSizesArr[lnA])
          IF lnSclStrt > 0
            lcScaleStr = SUBSTR(laSizesArr[lnA],lnSclStrt +1)
            lcSScale = SUBSTR(laSizesArr[lnA],1,lnSclStrt -1)
            DIMENSION laScalesArr[1]
            laScalesArr = ''
            =gfSubStr(lcScaleStr,@laScalesArr,',')
            FOR lnB = 1 TO ALEN(laScalesArr,1)
	          lcRet = lcRet +  lcSScale + ',' + laScalesArr[lnB] + '~' + lcMScale + ',1' + CHR(13)
            ENDFOR
          ENDIF
        ENDFOR
        *! B609452,1 MMT 1/08/2010 Conversion program converts BOM.MSZCROSREF incorrectly for extended size scale company[End]

        *E038594,1 AMH Restore the old order of style file [Start]
        SET ORDER TO CSTYLE IN STYLE
        *E038594,1 AMH [End]

      ELSE
*! B609452,1 MMT 1/08/2010 Conversion program converts BOM.MSZCROSREF incorrectly for extended size scale company[Start]
*!*	        lcRet = lcSScale + ',1~' + lcMScale + ',1' + CHR(13)
*!*	        FOR lnI = 2 TO SCALE.CNT
*!*	          lcRet = lcRet + lcSScale + ',' + STR(lnI,1) + '~' + lcMScale + ',1' + CHR(13)
*!*	        ENDFOR
 			 DIMENSION laSizesArr[1]
        laSizesArr = ''
        =gfSubStr(m.msizes ,@laSizesArr,CHR(13))
        lnSclStrt = ATC('~',laSizesArr[1])
				lcScaleStr = SUBSTR(laSizesArr[1],lnSclStrt +1)
				lcSScale = SUBSTR(laSizesArr[1],1,lnSclStrt -1)
			  DIMENSION laScalesArr[1]
        laScalesArr = ''
        =gfSubStr(lcScaleStr,@laScalesArr,',')
        FOR lnB = 1 TO ALEN(laScalesArr,1)
					lcRet = lcRet +  lcSScale + ',' + laScalesArr[lnB] + '~' + lcMScale + ',1' + CHR(13)
				ENDFOR
*! B609452,1 MMT 1/08/2010 Conversion program converts BOM.MSZCROSREF incorrectly for extended size scale company[End]
      ENDIF

      *E038594,1 AMH Don't close the style and scale file [Start]
      *USE IN SCALE
      *USE IN STYLE
      *E038594,1 AMH [End]

    ENDIF
  ENDIF
ENDIF

SELECT (lnAlias)
RETURN lcRet

*:*************************************************************
*: Name      : lfUpdLinNo
*: Developer : AHMED MAHER (AMH)
*: Date      : 03/21/2004
*: Purpose   : Update NLINENO in the BOM file.
*:*************************************************************
*: E038125,1 AMH
FUNCTION lfUpdLinNo
LPARAMETERS lcCITMMAJOR

LOCAL lnAlias,lnRecNo,lnLineNo

lnAlias = SELECT(0)
SELECT BOM
lnRecNo = RECNO()
lnLineNo = 0

IF SEEK(lcCITMMAJOR,'BOM','BOM')
  CALCULATE MAX(nLineNo) REST WHILE cItmMajor = lcCITMMAJOR TO lnLineNo
  GOTO lnRecNo
  REPLACE nLineNo WITH lnLineNo + 1
ENDIF

SELECT (lnAlias)
RETURN (lnLineNo+1)

*:*************************************************************
*: Name      : lfGetSzLn
*: Developer : AHMED MAHER (AMH)
*: Date      : 04/14/2004
*: Purpose   : Update CSIZES in the BOMLINE file.
*:*************************************************************
FUNCTION lfGetSzLn

LOCAL lnAlias,lcRet,lnI
lnAlias = SELECT(0)
lcRet = m.csizes

IF m.cimtyp = 'T'
  lcRet = '1'
ELSE
  IF lcRet = '12345678'

    *E038594,1 AMH Don't Open the Style and Scale files here to give conversion more speed [Start]
    *=gfOpenFile(oAriaApplication.DataDir+'STYLE','STYLE','SH')
    *=gfOpenFile(oAriaApplication.DataDir+'SCALE','SCALE','SH')
    SET ORDER TO STYLE IN STYLE
    *E038594,1 AMH [End]

    =SEEK(m.style,'STYLE')
    =SEEK('S'+STYLE.SCALE,'SCALE')
    lcRet = '1'
    FOR lnI = 2 TO SCALE.CNT
      lcRet = lcRet + STR(lnI,1)
    ENDFOR

    *E038594,1 AMH Don't close the style and scale file [Start]
    *USE IN SCALE
    *USE IN STYLE
    SET ORDER TO CSTYLE IN STYLE
    *E038594,1 AMH [End]

  ENDIF
ENDIF

SELECT (lnAlias)
RETURN lcRet

*:*************************************************************
*: Name      : lfGetCmpSz
*: Developer : AHMED MAHER (AMH)
*: Date      : 04/14/2004
*: Purpose   : Update CCOMPSIZES in the BOMLINE file.
*:*************************************************************
FUNCTION lfGetCmpSz

LOCAL lnAlias,lcRet,lnI
lnAlias = SELECT(0)
lcRet = m.ccompsizes

IF EMPTY(m.ccompsizes) AND m.ccatgtyp $ 'FT'

  *E038594,1 AMH Don't Open the Style and Scale files here to give conversion more speed [Start]
  *=gfOpenFile(oAriaApplication.DataDir+'STYLE','STYLE','SH')
  *=gfOpenFile(oAriaApplication.DataDir+'SCALE','SCALE','SH')
  SET ORDER TO STYLE IN STYLE
  *E038594,1 AMH [End]

  =SEEK(m.style,'STYLE')
  =SEEK('S'+STYLE.SCALE,'SCALE')
  lcRet = '1'
  FOR lnI = 2 TO SCALE.CNT
    IF STR(lnI,1) $ m.csizes
      lcRet = lcRet + '1'
    ENDIF
  ENDFOR

  *E038594,1 AMH Don't close the style and scale file [Start]
  *USE IN SCALE
  *USE IN STYLE
  SET ORDER TO CSTYLE IN STYLE
  *E038594,1 AMH [End]

ENDIF

SELECT (lnAlias)
RETURN lcRet

*:*************************************************************
*: Name      : lfCnvUOM
*: Developer : Wael M. Abo-Shawareb (WSH)
*: Date      : 07/25/2004
*: Purpose   : Update UOM file.
*:*************************************************************
*E038220,1 WSH
FUNCTION lfCnvUOM
LPARAMETERS oFormSet

PRIVATE lcUOMCode, lcUOM, lcDefUOM
lcUOM = gfTempName()

*E038594,1 AMH Don't Open the Codes file here to give conversion more speed [Start]
*--Open Codes file to Add UOM Codes.
*IF !gfOpenFile(oAriaApplication.DataDir+'CODES','CCODE_NO','SH')
*  =lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+'UOM'+LANG_CONVERT_NOTOPEN+'CODES'+;
*              LANG_CONVERT_FILE2,oFormSet)
*  RETURN .F.
*ENDIF
*E038594,1 AMH [End]

*E038594,1 AMH Don't Open the Fabric file here to give conversion more speed [Start]
*--Open Fabric File to read UOMs.
*IF !gfOpenFile(oAriaApplication.DataDir+'FABRIC','FABRIC','SH')
*  =lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+'UOM'+LANG_CONVERT_NOTOPEN+'Fabric'+;
*              LANG_CONVERT_FILE2,oFormSet)
*  RETURN .F.
*ENDIF

*B131128,1 AMH Fix bug of Fabric file not found [Start]
*SET ORDER TO FABRIC IN FABRIC
LOCAL llUseFabric
STORE .F. to llUseFabric
IF FILE(oAriaApplication.DataDir+'FABRIC'+'.DBF')
  llUseFabric = .T.
  SET ORDER TO FABRIC IN FABRIC
ENDIF
*B131128,1 AMH [End]

*E038594,1 AMH [End]

*--Open BOM File to read UOMs.
IF !gfOpenFile(oAriaApplication.DataDir+'BOM','BOMITEM','SH')
  =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'UOM'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'BOM'+;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
  *B038828,1 AMH Don't return faluse to privent terminate conversion [Start]
  *RETURN .F.
  RETURN .T.
  *B038828,1 AMH [End]

ENDIF

*--Get the Default UOM Code if found in CODES file.
lcDefUOM = IIF(SEEK('DCUNTOFMGR ', 'CODES'), CODES.cCode_No, SPACE(6))

*--Set Formset Properties
LOCAL lnFABRICRec, lnBOMRec
STORE 0 TO lnFABRICRec, lnBOMRec

*B131128,1 AMH Fix bug of Fabric file not found [Start]
*SELECT FABRIC
*COUNT TO lnFABRICRec
IF llUseFabric
  SELECT FABRIC
  COUNT TO lnFABRICRec
ENDIF
*B131128,1 AMH [End]

SELECT BOM
COUNT FOR cCatgTyp $ 'FT' TO lnBOMRec

oFormSet.lnRecCnt = (lnFABRICRec * 5) + (lnBOMRec * 2)
oFormSet.lnCurRec = oFormSet.lnCurRec * oFormSet.lnRecCnt / 100

*--Get the Highest Relation Code in UOM file
LOCAL lcStatement, lnConnectionHandlar


*E038594,1 AMH Use UOMFILE cursor to increase the conversion performance [Start]
*lcStatement = "SELECT TOP 1 CUOMCODE FROM UOM ORDER BY CUOMCODE DESC"
*lnConnectionHandlar = oAriaApplication.RemoteCompanyData.SQLRun(lcStatement, lcUOM, "UOM",;
                      oAriaApplication.ActiveCompanyConStr, 3, 'SAVE', oFormSet.DataSessionID)
lcStatement = "SELECT TOP 1 * FROM UOM ORDER BY CUOMCODE DESC"
lnConnectionHandlar = oAriaApplication.RemoteCompanyData.SQLRun(lcStatement, "UOMFILE", "UOM",;
                      oAriaApplication.ActiveCompanyConStr, 3, 'SAVE', oFormSet.DataSessionID)
*E038594,1 AMH [End]

IF lnConnectionHandlar # 1
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+'UOM'+LANG_CONVERT_NORETREI,oFormSet)
IF lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'UOM'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NORETREI,oFormSet.GetHeaderText("LANG_CONVERT_NORETREI",oFormSet.HeaderAlias)),oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

    RETURN .F.
  ENDIF
ENDIF

*--Set the Highest Relation Code in UOM file
*E038594,1 AMH Create index on the UOMFILE cursor [Start]
*lcUOMCode = EVALUATE(lcUOM + '.CUOMCODE')
=CURSORSETPROP("Buffering",3,"UOMFILE")
SELECT UOMFILE
INDEX ON CUOM_B+CUOM_V TAG UOMFILE
lcUOMCode = CUOMCODE
IF !EMPTY(lcDefUOM)
  lcDefUOMRel = lfGetUOMRel(oFormSet, '', '', 1)
ENDIF
*E038594,1 AMH [End]

*--Scan UOMs in Fabric file to update UOM file.

*B131128,1 AMH Fix bug of Fabric file not found [Start]
IF llUseFabric
*B131128,1 AMH [End]

SELECT FABRIC
SCAN
  *--Update UOM-Buy to UOM-Buy Relation
  IF !lfAddUOMRel(FABRIC.UOMBuy, FABRIC.UOMBuy, 1, oFormSet)
    RETURN .F.
  ENDIF

  *--Update UOM-Use to UOM-Use Relation.
  IF !lfAddUOMRel(FABRIC.UOMUse, FABRIC.UOMUse, 1, oFormSet)
    RETURN .F.
  ENDIF

  *--Update UOM-Sell to UOM-Sell Relation.
  IF !lfAddUOMRel(FABRIC.cSellUOM, FABRIC.cSellUOM, 1, oFormSet)
    RETURN .F.
  ENDIF

  *--Update UOM-Buy to UOM-Use Relation.
  IF !lfAddUOMRel(FABRIC.UOMBuy, FABRIC.UOMUse, FABRIC.Conv, oFormSet)
    RETURN .F.
  ENDIF

  *--Update UOM-Sell to UOM-Buy Relation.
  IF !lfAddUOMRel(FABRIC.cSellUOM, FABRIC.UOMUse, FABRIC.nSellConv, oFormSet)
    RETURN .F.
  ENDIF
ENDSCAN

*B131128,1 AMH Fix bug of Fabric file not found [Start]
ENDIF
*B131128,1 AMH [End]

*--Update UOM file from UOM Field in BOM file.
SELECT BOM
SCAN FOR cCatgTyp $ 'FT'
  *--Get UOM-Buy value from FABRIC file.

  *B131128,1 AMH Fix bug of Fabric file not found [Start]
  *SELECT FABRIC
  *=SEEK(SUBSTR(BOM.Item, 1, 7) + STRTRAN(BOM.IClr, '*', ''), 'FABRIC', 'FABRIC')

  *IF !FOUND()
  IF llUseFabric
    SELECT FABRIC
    =SEEK(SUBSTR(BOM.Item, 1, 7) + STRTRAN(BOM.IClr, '*', ''), 'FABRIC', 'FABRIC')
  ENDIF

  IF !llUseFabric OR !FOUND()
  *B131128,1 AMH [End]

    *--Refrech Progress Bars.
    oFormSet.lnCurRec = oFormSet.lnCurRec + (0.98 * 2 )
    =lfRefPro(oFormSet)
    LOOP
  ENDIF

  *--Update UOM "BOM" to UOM "BOM" Relation.
  IF !lfAddUOMRel(BOM.UOM, BOM.UOM, 1, oFormSet)
    RETURN .F.
  ENDIF

  *--Update UOM-Buy "FABRIC" to UOM "BOM" Relation.

  *B131128,1 AMH Fix bug of Fabric file not found [Start]
  IF llUseFabric
  *B131128,1 AMH [End]

  SELECT FABRIC
  SCAN REST WHILE FABRIC + COLOR = SUBSTR(BOM.Item, 1, 7) + STRTRAN(BOM.IClr, '*', '')
    IF !lfAddUOMRel(FABRIC.UOMBuy, BOM.UOM, FABRIC.Conv, oFormSet)
      RETURN .F.
    ENDIF
    oFormSet.lnCurRec = oFormSet.lnCurRec - 0.98
  ENDSCAN

  *B131128,1 AMH Fix bug of Fabric file not found [Start]
  ENDIF
  *B131128,1 AMH [End]

  *--Refrech Progress Bars.
  oFormSet.lnCurRec = oFormSet.lnCurRec + 0.98
  =lfRefPro(oFormSet)
ENDSCAN

*--Close Opened Files.

*E038594,1 AMH Don't close the codes file to give the conversion more speed [Start]
*USE IN CODES
*E038594,1 AMH Get the correct UOM code length [Start]

*E038594,1 AMH Don't close the Fabric file here to give conversion more speed [Start]
*USE IN FABRIC

*B131128,1 AMH Fix bug of Fabric file not found [Start]
*SET ORDER TO CFABRIC IN FABRIC
IF llUseFabric
  SET ORDER TO CFABRIC IN FABRIC
ENDIF
*B131128,1 AMH [End]

*E038594,1 AMH [End]

USE IN BOM
IF USED(lcUOM)
  USE IN (lcUOM)
ENDIF

RETURN .T.

*:*************************************************************
*: Name      : lfAddUOMRel
*: Developer : Wael M. Abo-Shawareb (WSH)
*: Date      : 07/26/2004
*: Purpose   : Add a Relation Code in UOM and add UOM Code to Codes file
*:*************************************************************
*E038220,1 WSH
FUNCTION lfAddUOMRel
LPARAMETERS lcUOM_B, lcUOM_V, lnConf, oFormSet

*E038594,1 AMH Get the correct UOM code length [Start]
*m.CUOM_B    = IIF(EMPTY(lcUOM_B), IIF(EMPTY(lcDefUOM), 'EAC   ', lcDefUOM), UPPER(lcUOM_B))
*m.CUOM_V    = IIF(EMPTY(lcUOM_V), IIF(EMPTY(lcDefUOM), 'EAC   ', lcDefUOM), UPPER(lcUOM_V))
m.CUOM_B    = IIF(EMPTY(lcUOM_B), IIF(EMPTY(lcDefUOM), 'EAC   ', lcDefUOM), PADR(UPPER(lcUOM_B),6))
m.CUOM_V    = IIF(EMPTY(lcUOM_V), IIF(EMPTY(lcDefUOM), 'EAC   ', lcDefUOM), PADR(UPPER(lcUOM_V),6))
*E038594,1 AMH [End]

m.NCONF     = IIF(lnConf = 0, 1, lnConf)
m.cAdd_User = oAriaApplication.User_ID
m.dAdd_Date = DATE()
m.cAdd_Time = gfGetTime()

*E038594,1 AMH Use the UOMFILE cursor to increate the conversion performance [Start]
IF SEEK(m.CUOM_B+m.CUOM_V,"UOMFILE")
  SELECT UOMFILE
  LOCATE REST WHILE CUOM_B+CUOM_V = m.CUOM_B+m.CUOM_V FOR NCONF = m.NCONF
  IF FOUND()
    *--Refrech Progress Bars.
    oFormSet.lnCurRec = oFormSet.lnCurRec + 0.98
    =lfRefPro(oFormSet)
    RETURN .T.
  ENDIF
ENDIF
*E038594,1 AMH [End]

*--Retreive Data from UOM Table.
IF !lfOpenRemt('UOM', lcUOM, 'UOM', 'CUOM_B+CUOM_V+NCONF', oFormSet)
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+'UOM'+LANG_CONVERT_NORETREI,oFormSet)
IF lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'UOM'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NORETREI,oFormSet.GetHeaderText("LANG_CONVERT_NORETREI",oFormSet.HeaderAlias)),oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]

    RETURN .F.
  ENDIF
ENDIF

*--Check if it has been added before to UOM, then add it to Codes and UOM.
IF EOF(lcUOM)
  lcUOMCode  = STRTRAN(STR(VAL(lcUOMCode) + 1, 6, 0), ' ', '0')
  m.CUOMCODE = lcUOMCode

  *--Update Codes File by UOM_B if not found.
  IF m.CUOM_B == m.CUOM_V AND !SEEK('NCUNTOFMGR ' + m.CUOM_B, 'CODES', 'CCODE_NO')
    INSERT INTO 'CODES' (cDefCode, cFld_Name, cCode_No, cDiscRep, lRltFields, cRltField) ;
           VALUES ('N', 'CUNTOFMGR', m.CUOM_B, m.CUOM_B, .F., 'N')
    =gfAdd_info('CODES')

    *--Add Default UOM Code if not added.
    IF EMPTY(lcDefUOM)
      INSERT INTO 'CODES' (cDefCode, cFld_Name, cCode_No, cDiscRep, lRltFields, cRltField) ;
             VALUES ('D', 'CUNTOFMGR', m.CUOM_B, lcUOM_B, .F., 'N')
      lcDefUOM = IIF(EMPTY(lcDefUOM), m.CUOM_B, lcDefUOM)

      *E038594,1 AMH Save the default UOM relation code in variable [Start]
      lcDefUOMRel = lcUOMCode
      *E038594,1 AMH [End]

      =gfAdd_info('CODES')
    ENDIF
  ENDIF

  *--Update UOM File with UOM Relation.
  IF !lfUpdA4Files(lcUOM,"UOM","UOM","CUOM_B+CUOM_V+NCONF","", oFormSet, .T.)
    RETURN .F.
  ENDIF
ELSE
  m.CUOMCODE = EVALUATE(lcUOM+'.CUOMCODE')
  *--Refrech Progress Bars.
  oFormSet.lnCurRec = oFormSet.lnCurRec + 0.98
  =lfRefPro(oFormSet)
ENDIF

*E038594,1 AMH Update the UOMFILE cursor with the new UOM relation [Start]
SELECT (lcUOM)
SCATTER MEMVAR
INSERT INTO UOMFILE FROM MEMVAR
SELECT uomfile
*E038594,1 AMH [End]

RETURN .T.

*:*************************************************************
*: Name      : lfGetUOMRel
*: Developer : Wael M. Abo-Shawareb (WSH)
*: Date      : 07/26/2004
*: Purpose   : Get UOM Relation Code from UOM file.
*:*************************************************************
*E038220,1 WSH
FUNCTION lfGetUOMRel
LPARAMETERS oFormSet, lcUOM_B, lcUOM_V, lnConf


*E038594,1 AMH Dont assigne value here to increate the conversion performance [Start]
*LOCAL lcUOM, lcUOMCode, lcDefUOM, llOpenCodes
LOCAL lcUOM, lcUOMCode, llOpenCodes
*lcUOM        = gfTempName()
*E038594,1 AMH [End]

lcUOMCode    = SPACE(6)
llOpenCodes  = .F.
llOpenFabric = .F.

*--Open Codes file if it is not opened to Get Default UOM Code
IF !USED('CODES')
  IF !gfOpenFile(oAriaApplication.DataDir+'CODES','CCODE_NO','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+ALLTRIM(SYCNVLIB.cFile)+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'CODES'+;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    RETURN SPACE(6)
  ELSE
    llOpenCodes = .T.
  ENDIF
ENDIF

*--Get the Default UOM Code if found in CODES file.
lcDefUOM = IIF(SEEK('DCUNTOFMGR ', 'CODES', 'CCODE_NO'), CODES.cCode_No, SPACE(6))

*--Retreive Data from UOM Table.
m.CUOM_B = IIF(EMPTY(lcUOM_B), IIF(EMPTY(lcDefUOM), 'EAC   ', lcDefUOM), UPPER(lcUOM_B))
m.CUOM_V = IIF(EMPTY(lcUOM_V), IIF(EMPTY(lcDefUOM), 'EAC   ', lcDefUOM), UPPER(lcUOM_V))
m.NCONF  = IIF(lnConf = 0, 1, lnConf)

*E038594,1 AMH Use the UOMFILE cursor to increate the conversion performance [Start]
IF SEEK(PADR(m.CUOM_B,6)+PADR(m.CUOM_V,6),"UOMFILE")
  SELECT UOMFILE
  LOCATE REST WHILE CUOM_B+CUOM_V = m.CUOM_B+m.CUOM_V FOR NCONF = m.NCONF
  IF FOUND()
    lcUOMCode = CUOMCODE
    RETURN lcUOMCode
  ENDIF
ENDIF
lcUOM        = gfTempName()
*E038594,1 AMH [End]

IF !lfOpenRemt('UOM', lcUOM, 'UOM', 'CUOM_B+CUOM_V+NCONF', oFormSet)
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF lfUpdError(LANG_CONVERT_MODULE+SYCNVMDL.CAPP_ID+LANG_CONVERT_FILE+ALLTRIM(SYCNVLIB.cFile)+LANG_CONVERT_NORETREI,oFormSet)
IF lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+ALLTRIM(SYCNVLIB.cFile)+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NORETREI,oFormSet.GetHeaderText("LANG_CONVERT_NORETREI",oFormSet.HeaderAlias)),oFormSet)
*N000682,1 11/20/2012 MMT Globlization changes[End]


    *E038594,1 AMH Don't close the codes file to give the conversion more speed [Start]
    *IF llOpenCodes
    *  USE IN CODES
    *ENDIF
    *E038594,1 AMH [End]

    RETURN SPACE(6)
  ENDIF
ENDIF

*--Check if it has been added before to UOM, then add it to Codes and UOM.
IF !EOF(lcUOM)
  lcUOMCode = EVALUATE(lcUOM + '.CUOMCODE')
ENDIF

USE IN (lcUOM)

*E038594,1 AMH Don't close the codes file to give the conversion more speed [Start]
*IF llOpenCodes
*  USE IN CODES
*ENDIF
*E038594,1 AMH [End]

RETURN lcUOMCode

*:*************************************************************
*: Name      : lfGetBOMUOMRel
*: Developer : Wael M. Abo-Shawareb (WSH)
*: Date      : 07/26/2004
*: Purpose   : Get UOM Relation Code from UOM file
*:             for the BOM & CTKTBOM files.
*:*************************************************************
*E038220,1 WSH
FUNCTION lfGetBOMUOMRel
LPARAMETERS oFormSet

LOCAL lcBOM_B, lcUOM_V, lnConf, llOpenFabric, lcRelCode
llOpenFabric = .F.
lcRelCode    = SPACE(6)

*B131128,1 AMH Fix bug of Fabric file not found [Start]
IF !FILE(oAriaApplication.DataDir+'FABRIC'+'.DBF')
  RETURN lcRelCode
ENDIF
*B131128,1 AMH [End]

*--Get UOM-Buy from FABRIC file - if it is not passed...
IF !USED('FABRIC')
  IF !gfOpenFile(oAriaApplication.DataDir+'FABRIC','FABRIC','SH')
    =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+ALLTRIM(SYCNVLIB.cFile)+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'FABRIC'+;
  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
    RETURN SPACE(6)
  ENDIF
  llOpenFabric = .T.

*E038594,1 AMH Use the correct order [Start]
ELSE
  SET ORDER TO FABRIC IN FABRIC
*E038594,1 AMH [End]

ENDIF

=SEEK(SUBSTR(m.Item, 1, 7) + STRTRAN(m.iClr, '*', ''), 'FABRIC', 'FABRIC')
lcUOM_B = FABRIC.UOMBuy
lcUOM_V = m.UOM
lnConf  = FABRIC.Conv

*--Get Relation Code
lcRelCode = lfGetUOMRel(oFormSet, lcUOM_B, lcUOM_V, lnConf)

*E038594,1 AMH Dont Close the fabric file to increate the conversion performance [Start]
*IF llOpenFabric
*  USE IN FABRIC
*ENDIF
SET ORDER TO CFABRIC IN FABRIC
*E038594,1 AMH [End]

RETURN lcRelCode

*:*************************************************************
*: Name      : lfCnvFCast
*: Developer : Hend Ghanem (HBG)
*: Date      : 06/27/2005
*: Purpose   : Convert FORCASTH file.
*:*************************************************************
FUNCTION lfCnvFCast
LPARAMETERS oFormSet

LOCAL lcForCastH
lcForCastH = gfTempName()

IF !gfOpenFile(oAriaApplication.DataDir+'FORCAST','FORCASTW','SH')
  =lfUpdError(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_MODULE,oFormSet.GetHeaderText("LANG_CONVERT_MODULE",oFormSet.HeaderAlias))+SYCNVMDL.CAPP_ID+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE,oFormSet.GetHeaderText("LANG_CONVERT_FILE",oFormSet.HeaderAlias))+'FORCASTH'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_NOTOPEN,oFormSet.GetHeaderText("LANG_CONVERT_NOTOPEN",oFormSet.HeaderAlias))+'FORCAST'+;
 IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_FILE2,oFormSet.GetHeaderText("LANG_CONVERT_FILE2",oFormSet.HeaderAlias)),oFormSet)
  RETURN .T.
ENDIF

SELECT DISTINCT NWEEK,NYEAR FROM FORCAST INTO CURSOR FCSTCOUNT
SELECT FCSTCOUNT
COUNT TO oFormSet.lnRecCnt
oFormSet.lnCurRec = oFormSet.lnCurRec * oFormSet.lnRecCnt / 100
LOCAL lnWeek , lnYear
STORE 0 TO lnWeek , lnYear

SELECT FORCAST
SCAN
  m.nWeek = nWeek
  m.nYear = nYear
  IF STR(m.nYear,4)+STR(m.nWeek,2) <> STR(lnYear,4)+STR(lnWeek,2)
    lnYear = m.nYear
    lnWeek = m.nWeek
    m.cEdit_User = IIF(TYPE('FORCAST.cEdit_User')#'U',FORCAST.cEdit_User,'')
	m.dEdit_Date = IIF(TYPE('FORCAST.dEdit_Date')#'U',FORCAST.dEdit_Date,'')
	m.cEdit_Time = IIF(TYPE('FORCAST.cEdit_Time')#'U',FORCAST.cEdit_Time,'')
	m.cEdt_Ver   = IIF(TYPE('FORCAST.cEdt_Ver')#'U',FORCAST.cEdt_Ver,'')
	
	m.cAdd_User = IIF(TYPE('FORCAST.cAdd_User')#'U',FORCAST.cAdd_User,'')
	m.dAdd_Date = IIF(TYPE('FORCAST.dAdd_Date')#'U',FORCAST.dAdd_Date,'')
	m.cAdd_Time = IIF(TYPE('FORCAST.cAdd_Time')#'U',FORCAST.cAdd_Time,'')
	m.cAdd_Ver  = IIF(TYPE('FORCAST.cAdd_Ver')#'U',FORCAST.cAdd_Ver,'')

    IF !lfUpdA4Files(lcForCastH,"FORCASTH","FORCASTH","STR(nYear,4)+STR(nWeek,2)",;
    IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_WEEK,oFormSet.GetHeaderText("LANG_CONVERT_WEEK",oFormSet.HeaderAlias))+STR(m.nWeek,2)+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CONVERT_YEAR,oFormSet.GetHeaderText("LANG_CONVERT_YEAR",oFormSet.HeaderAlias))+STR(m.nYear,4),oFormSet)
      RETURN .F.
    ENDIF
  ENDIF
ENDSCAN


IF USED('FORCAST')
  USE IN FORCAST
ENDIF

IF USED('FCSTCOUNT')
  USE IN FCSTCOUNT
ENDIF

IF USED(lcForCastH)
  USE IN (lcForCastH)
ENDIF


*: B608646,1 MMT 08/07/2008 Convert Fill Currency fields in BOM and Bomline Tables[Start]
*!*************************************************************
*! Name      : lfGetCurrCode
*! Developer : Mariam Mazhar (MMT)
*! Date      : 08/07/2008
*! Purpose   : Get Currecny for BOM file
*!*************************************************************
FUNCTION lfGetCurrCode

LOCAL lnAlias,lcRet
lnAlias = SELECT(0)
m.CCURRCODE = ''
lcRet = m.CCURRCODE

IF EMPTY(lcRet) OR ISNULL(lcRet)
  IF m.lmaterial
    IF m.ccatgtyp  $ 'PDM'
      SELECT Fabric
      =SEEK(m.citmmajor)
      IF m.ccatgtyp  = 'P'
        lcRet = Fabric.cpricecur
      ELSE
        IF m.ccatgtyp  $ 'MD'
          lcRet = Fabric.cdutycur
        ENDIF
	  ENDIF
	ELSE
     lcRet = oAriaApplication.BaseCurrency
    ENDIF
  ELSE
    IF m.ccatgtyp  $ 'PDM'
      SELECT STYLE
      =SEEK(m.citmmajor)
      IF m.ccatgtyp  = 'P'
        lcRet = Style.cpricecur
      ELSE
        IF m.ccatgtyp  $ 'MD'
          lcRet = style.cdutycur
        ENDIF
      ENDIF
    ELSE
     lcRet = oAriaApplication.BaseCurrency
    ENDIF
  ENDIF
ENDIF
SELECT (lnAlias)
RETURN lcRet


*!*************************************************************
*! Name      : lfGetExRate
*! Developer : Mariam Mazhar (MMT)
*! Date      : 08/07/2008
*! Purpose   : Get Currecny Ex. Rate for BOM file
*!*************************************************************
FUNCTION lfGetExRate
LOCAL lnAlias,lcRet
lnAlias = SELECT(0)
m.nExRate = 1
lcRet = m.nExRate

IF !EMPTY(m.CCURRCODE) AND !ISNULL(m.CCURRCODE)
  IF m.CCURRCODE <> oAriaApplication.BaseCurrency
    STORE 1 TO lnCurrUnit, lnExRate
    lnExRate  = gfChkRate('lnCurrUnit',m.CCURRCODE,oAriaApplication.SystemDate,.T.,oAriaApplication.ActiveCompanyID,.F.,.T.)
    lcRet = lnExRate
  ELSE
    lcRet = 1
  ENDIF
ELSE
  lcRet = 1
ENDIF
SELECT (lnAlias)
RETURN lcRet
*!*************************************************************
*! Name      : lfGetCurrUnt
*! Developer : Mariam Mazhar (MMT)
*! Date      : 08/07/2008
*! Purpose   : Get Currecny unit for BOM file
*!*************************************************************
FUNCTION lfGetCurrUnt

LOCAL lnAlias,lcRet
lnAlias = SELECT(0)
IF !EMPTY(m.CCURRCODE) AND !ISNULL(m.CCURRCODE)
  IF m.CCURRCODE <> oAriaApplication.BaseCurrency
    STORE 1 TO lnCurrUnit, lnExRate
    lnExRate  = gfChkRate('lnCurrUnit',m.CCURRCODE,oAriaApplication.SystemDate,.T.,oAriaApplication.ActiveCompanyID,.F.,.T.)
    lcRet = lnCurrUnit
  ELSE
    lcRet = 1
  ENDIF
ELSE
  lcRet = 1
ENDIF
SELECT (lnAlias)
RETURN lcRet
*: B608646,1 MMT 08/07/2008 Convert Fill Currency fields in BOM and Bomline Tables[End]
