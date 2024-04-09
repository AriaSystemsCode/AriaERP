*****************************************************************

*   lnError =    The numeric code of the number provided by ERROR()
*   lcmsg =     The error message provided by MESSAGE()
*   lckode =    The contents of the line of code which
*                 triggered the error as provided by MESSAGE(1)
*   lcmodul =   The name of the code module, SYS(16)
*   lnline =    The number of the line of code triggering the
*                 error, LINENO()
*   lcprint =    Current PRINTER setting as per SYS(102)
*   lckonsol =   Current CONSOLE setting as per SYS(100)
*   lcdevice =   Current DEVICE setting as per SYS(101)
*   keypress =  LASTKEY()
*   curr_dbf =  Selected .DBF when error occurred
*   getfield =  Field of GET...READ at time error occurred
*   def_drive = Default drive at time error occurred - SYS(5)
*   run_memry = Amount of DOS memory available for RUN command - SYS(12)
*   prnt_devic= Current SET PRINTER TO setting - SYS(6)
*   cur_direc = Current directory at time error occurred - SYS(2003)
*   top_win =   Top window at time error occurred
*   llocked =   Record or file lock status at time error occurred -
*                 SYS(2011)
*   lcmissing =  Missing file, window, etc.
*   curs_set =  Cursor setting at time error occurred.
*
*  In addition to this information, the routine saves the information
*  from LIST MEMORY and LIST STATUS is saved into the memo field.
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*E300615,1 Hesham El-Sheltawi 27/03/97
*E300615,1 change the name of the fields line_no to nline_no
*E300615,1 to prevent the conflict with the apparel fields
*E300615,1 to update the dictionary
*E300701,1 Hesham El-Sheltawi 09/07/97
*E300701,1 Make the error hand. get the screen name and the program
*E300701,1 name with the path then trancate the path from the result
*E300701,1 because in 27 version we run programs from on the disk
*E300701,1 directly, to make the error handler able to return to the
*E300701,1 right point in the program
*E300999,1 Hesham El-Sheltawi 11/20/1998
*E300999,1 Modify the error hand. to act in a smart way to recover
*E300999,1 the errors and make the user can continue regardles the
*E300999,1 error that occurs
*E300999,1 Add new functions to recover the dbf errors and try to
*E300999,1 fix the corrupted files
*E300797,1 Display the error message according to an environment
*E300797,1 Variable "ARIADEMO"
*E301077,78 Hesham 02/25/1999
*E301077,78 Controlling opening and clossing files.
*E301077,78 to make the program open the files when ever it need it
*B603471,1 KHM 02/22/2000 Fix the bug of not saving the error in the
*B603471,1                syuerr file because the file is closed.
*B603481,1 Hesham 02/27/2000 make the NO. Of record locking retries infenity
*B603686,1 Hesham 06/18/2000
*B603686,1 Control the numeric overflow error
*! B128052,1 MAH 06/21/2005 Enhance option grid startup.
*B609113,1 MMT 12/20/2009 make aria4xp error handler update syueror table [T20091120.0006]
* B609711,1 MAH 11/23/2011 Separate Business From Aria.exe Begin

PARAMETERS lnError, lcmsg, lckode, lcmodul, lnline, lcprint, ;
    lckonsol, lcdevice, keypress, curr_dbf, def_drive, ;
    run_memry, prnt_devic, cur_direc, top_win, llocked, ;
    lcmissing, curs_set,lcLpmHit

*! B128052,1 MAH 06/21/2005 If a file not uses inside the option grid (to enhance the performance) open it [BEGIN]
*IF TYPE('loOGScroll') = 'O' .AND. !ISNULL(loOGScroll)
*  IF lnError = 13
*    lcFileName = SUBSTR(lcmsg, LEN("Alias '") +1)
*    lcFileName = SUBSTR(lcFileName, 1, AT("'",lcFileName) -1)
*
*    LOCAL lnIndex
*    FOR lnIndex = 1 TO ALEN(loOGScroll.laSelFile, 1)
*      IF UPPER(ALLTRIM(loOGScroll.laSelFile[lnIndex, 1])) == UPPER(ALLTRIM(lcFileName))
*        loOGScroll.OpenTable(loOGScroll.laSelFile[lnIndex, 2], loOGScroll.laSelFile[lnIndex, 3], loOGScroll.laSelFile[lnIndex, 1])
*        RETRY
*      ENDIF
*    ENDFOR
*  ENDIF
*ENDIF
*! B128052,1 MAH 06/21/2005 [END]

* B609711,1 MAH 11/23/2011 Separate Business From Aria.exe Begin
IF .NOT. UPPER("ariaglb.fxp") $ SET("Procedure")
  LOCAL lcCurrentProcedure, lnPathStart, lnLenOfPath, lcCurPath

  lcCurrentProcedure = UPPER(SYS(16,1))
  lcCurrentProcedure = STRTRAN(lcCurrentProcedure, "\PRGS\SY", "")
  lnPathStart        = AT(":",lcCurrentProcedure)- 1
  lnLenOfPath        = RAT("\", lcCurrentProcedure) - (lnPathStart)
  lcCurPath          = SUBSTR(lcCurrentProcedure, lnPathStart, lnLenofPath)

  LOCAL lcAriaMainPath
  lcAriaMainPath = "SET PROCEDURE TO " + lcCurPath + "\PRGS\SY\ariaglb.fxp ADDITIVE "
  &lcAriaMainPath.
  RETRY
  EXIT
ENDIF
* B609711,1 MAH 11/23/2011 Separate Business From Aria.exe End

PRIVATE lcErrWoutPut,lnCounter,laErrTrac,lcErrProgm,lcErrScr,lcErrApp,;
            lcErrMainPrg,lcErrCallFun,lcErrWorkAr,lcErrCallPrg,oErrScr,;
            lcOrderTag,lnCounter,lcErrScrClass,lcToProc,lcErrProcess,;
            lcErrMsg,lnRemResult,llErrFile,llUsrErr,gcWorkDir



*********************************************
llUsrErr = .F.
testing=.F.
glMultiUsr=.T.
gcUser_ID = oAriaApplication.User_ID
gcWorkDir = oAriaApplication.WorkDir
lcContact="your system administrator."&&+" Phone: (202) 3357791"
lcPhone=""
*lcPhone="Phone : (202)3357791"
*B609113,1 MMT 12/20/2009 make aria4xp error handler update syueror table [Start]
IF !USED('SYUEROR')
 =gfOpenFile(oAriaApplication.SysPath+'SYUEROR')
 llUsrErr = .T.
ENDIF
llError = .F.
*B609113,1 MMT 12/20/2009 make aria4xp error handler update syueror table [End]
CLEAR TYPEAHEAD
*********************************************
*wait curr_dbf window
ON ERROR

DIMENSION laErrTrac[1,1]
STORE '' TO lcErrWoutPut,lnCounter,laErrTrac,lcErrProgm,lcErrScr,lcErrApp,;
            lcErrMainPrg,lcErrCallFun,lcErrWorkAr,lcErrCallPrg,oErrScr,;
            lcErrScrClass,lcToProc,lcErrProcess,lcErrMsg


*--  lcErrWoutPut       the window output name at the error point
*--  laErrTrac          array have the program path till the error occurs
*--  lcErrProgm         the program name '.fxp' where the error occurs
*--  lcErrScr           the screen name '.spx' where the error occurs
*--  lcErrCallFun       the function that call the place where the error occurs
*--  lcErrApp           the module name '.app' where the error occurs
*--  lcErrMainPrg       the main program name "function in aria3"  or Aria3 itself if empty
*--  lcErrWorkAr        work aria when the error occurs
*--get the current output window at the error point
*--if it is the control panel then get the previous window
*--to the controll panel because we can not depend on the
*--control panel to Know if we will have to terminate the
*--read or not

lcErrWorkAr = SELECT()
lcErrWoutPut=WOUTPUT()

IF lcErrWoutPut = 'GWDTHERMO'
  lcErrWoutPut = ''
  RELEASE WINDOW GWDTHERMO
ENDIF
lcErrWoutPut=IIF(lcErrWoutPut='GWCCONTRL1',WLAST(),lcErrWoutPut)
lcErrWoutPut=IIF(lcErrWoutPut='FNDATION','',lcErrWoutPut)


lnCounter=1
*--Initialize the array of the program path
*--to know the place of error and where to return
DO WHILE !EMPTY(SYS(16,lnCounter))
  DIMENSION laErrTrac[lnCounter,1]
  laErrTrac[lnCounter,1]=SYS(16,lnCounter)
  lnCounter=lnCounter+1
  lnCounter=lnCounter+IIF(EMPTY(SYS(16,lnCounter+2)),2,0)
ENDDO
*--get the screen that the error occurs at if exist
lnCounter =ALEN(laErrTrac,1)
llFound = .F.
lcErrorFormName = ""
DO WHILE !llFound AND lnCounter>0 AND laErrTrac[lnCounter,1]<>'ON...'
  IF LEFT(laErrTrac[lnCounter,1],10) = 'PROCEDURE ' && AND ;
     (UPPER(RIGHT(laErrTrac[lnCounter,1],4))='.SCT' OR UPPER(RIGHT(laErrTrac[lnCounter,1],4))='.VCT')
    lcErrScr=laErrTrac[lnCounter,1]
    lcErrScr = STRTRAN(lcErrScr,"PROCEDURE","")
    IF lcErrorFormName <> ALLTRIM(SUBSTR(lcErrScr,1,ATC(".",lcErrScr)-1))
       lcErrorFormName = UPPER(ALLTRIM(SUBSTR(lcErrScr,1,ATC(".",lcErrScr)-1)))
      *-- loop through all existance forms
      FOR lnFrmCnt = 1 TO _SCREEN.FormCount
        DO CASE
          *-- in case error occurs in a form or toolbar
          CASE TYPE('_SCREEN.Forms(lnFrmCnt)')="O" AND UPPER(_SCREEN.Forms(lnFrmCnt).Name) = lcErrorFormName
            llFound=.T.
            lcErrScr=laErrTrac[lnCounter,1]
            lcErrScrClass = _SCREEN.Forms(lnFrmCnt).BaseClass
            oErrScr = _SCREEN.Forms(lnFrmCnt)
            EXIT
          *-- in case error occurs in a formset
          CASE TYPE('_SCREEN.Forms(lnFrmCnt).PARENT')="O" AND UPPER(_SCREEN.Forms(lnFrmCnt).Parent.Name) = lcErrorFormName
            llFound=.T.
            lcErrScr=laErrTrac[lnCounter,1]
            lcErrScrClass = _SCREEN.Forms(lnFrmCnt).PARENT.BaseClass
            oErrScr = _SCREEN.Forms(lnFrmCnt).PARENT
            EXIT
        ENDCASE
      ENDFOR
    ENDIF
  ENDIF
  lnCounter=lnCounter-1
ENDDO


*--get the program that the error occurs at if exist
lnCounter=IIF(llFound,lnCounter,ALEN(laErrTrac,1))
llFound = .F.
DO WHILE !llFound AND lnCounter>0 AND laErrTrac[lnCounter,1]<>'ON...'
  IF LEFT(laErrTrac[lnCounter,1],10) <> 'PROCEDURE ' AND RIGHT(laErrTrac[lnCounter,1],4)='.FXP'
    IF ATC('\',laErrTrac[lnCounter,1]) >0
      lcErrProgm = SUBSTR(laErrTrac[lnCounter,1],RAT('\',laErrTrac[lnCounter,1])+1)
    ELSE
      lcErrProgm = laErrTrac[lnCounter,1]
    ENDIF
    lcErrProgm = IIF(RATC(".",lcErrProgm)>0,SUBSTR(lcErrProgm,RATC(".",lcErrProgm)+1),lcErrProgm)
    llFound=.T.
  ENDIF
  lnCounter=lnCounter-1
ENDDO
lcErrProgm = IIF(EMPTY(lcErrProgm) AND !EMPTY(lcErrScr),"DO",lcErrProgm)


*--get the Function that calls the place where error occurs at if exist
lnCounter=IIF(llFound,lnCounter,ALEN(laErrTrac,1)-IIF(EMPTY(lcErrScr),0,1))
llFound = .F.
DO WHILE !llFound AND lnCounter>0 AND laErrTrac[lnCounter,1]<>'ON...'
  IF LEFT(laErrTrac[lnCounter,1],10) = 'PROCEDURE ' AND ;
     (RIGHT(laErrTrac[lnCounter,1],4)='.APP' OR RIGHT(laErrTrac[lnCounter,1],4)='.EXE';
     OR RIGHT(laErrTrac[lnCounter,1],4)='.FXP') && OR RIGHT(laErrTrac[lnCounter,1],4)='.SCT';
     OR RIGHT(laErrTrac[lnCounter,1],4)='.VCT')
    lcErrCallFun=SUBSTR(laErrTrac[lnCounter,1],11,ATC(' ',laErrTrac[lnCounter,1],2)-11)
    lcErrCallFun= IIF(RATC(".",lcErrCallFun)>0,SUBSTR(lcErrCallFun,RATC(".",lcErrCallFun)+1),lcErrCallFun)
    llFound=.T.
  ENDIF
  lnCounter=lnCounter-1
ENDDO


*--get the Application "Module" that the error occurs at if exist
lnCounter=IIF(llFound,lnCounter,ALEN(laErrTrac,1))
llFound = .F.
lcErrCallPrg = ''
DO WHILE !llFound AND lnCounter>0 AND laErrTrac[lnCounter,1]<>'ON...'
  IF LEFT(laErrTrac[lnCounter,1],10) = 'PROCEDURE ' AND ;
     RIGHT(laErrTrac[lnCounter,1],4)='.APP'
    lcErrCallPrg=SUBSTR(laErrTrac[lnCounter,1],11,ATC(' ',laErrTrac[lnCounter,1],2)-11)
    llFound=.T.
  ENDIF
  lnCounter=lnCounter-1
ENDDO


llFound = .F.
lnCounter=IIF(llFound,lnCounter,ALEN(laErrTrac,1))
*-- get the module app that called the place error occured if exist
DO WHILE !llFound AND lnCounter>0
  IF LEFT(laErrTrac[lnCounter,1],10) <> 'PROCEDURE ' AND ;
     RIGHT(laErrTrac[lnCounter,1],4)='.APP'
    IF ATC('\',laErrTrac[lnCounter,1])>0
      lcErrApp=SUBSTR(laErrTrac[lnCounter,1],RAT('\',laErrTrac[lnCounter,1])+1)
    ELSE
      lcErrApp=laErrTrac[lnCounter,1]
    ENDIF
    llFound=.T.
  ENDIF
  lnCounter=lnCounter-1
ENDDO

*--get the main program of the error program
*--if empty then we will go to master
*--if not empty then it will have a function from
*--the aria3.prg that we can return to
lnCounter=IIF(llFound,lnCounter,ALEN(laErrTrac,1)-1)
llFound = .F.
DO WHILE !llFound AND lnCounter>0 AND laErrTrac[lnCounter,1]<>'ON...'
  IF LEFT(laErrTrac[lnCounter,1],10) = 'PROCEDURE ' AND ;
     (RIGHT(laErrTrac[lnCounter,1],4)='.APP' OR RIGHT(laErrTrac[lnCounter,1],4)='.EXE');
     AND ATC(':\',laErrTrac[lnCounter,1])>0
    lcErrMainPrg=SUBSTR(laErrTrac[lnCounter,1],11,ATC(' ',laErrTrac[lnCounter,1],2)-11)
    llFound=.T.
  ENDIF
  lnCounter=lnCounter-1
ENDDO

*-- Get Error Message and Display it
lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from SYCERROR WHERE nerror_no="+ALLTRIM(STR(lnError))+;
  							" OR nerror_no IN (3000,3001,3002) ORDER BY nerror_no",'',"SYCERROR","",oAriaApplication.SystemConnectionString,3,;
				  			"",SET("DATASESSION"))

llErrFile = USED("SYCERROR")
IF USED('SYCERROR')
  LOCATE FOR nError_No = lnError
  IF !FOUND()
    LOCATE FOR nError_No = 3000
  ENDIF
  lcErrProcess = MEXECUTE
  lcErrMsg = mmumsg
ELSE
  lcErrMsg = "Unable to find error description."+CHR(13)+CHR(10)+;
             "Please Contact: your system administrator."
  lcErrProcess = "lnOption=SHOWEROR()"+CHR(13)+CHR(10)
  lcErrProcess = lcErrProcess + ["END"] +CHR(13)+CHR(10)
  lcErrProcess = lcErrProcess +["PUSH BUTTON"]+CHR(13)+CHR(10)
  lcErrProcess = lcErrProcess +"\!\?\<Ok"+CHR(13)+CHR(10)
  lcErrProcess = lcErrProcess +["CONDITION 1"]+CHR(13)+CHR(10)
  lcErrProcess = lcErrProcess +"DO gpGo_Mastr"+CHR(13)+CHR(10)
ENDIF
lnNoLine=IIF(ATCLINE('"END"',lcErrProcess)>0,;
             ATCLINE('"END"',lcErrProcess)-1,MEMLINES(lcErrProcess))

*E301077,78 Hesham (Start)
*USE IN IIF(llErrFile,'SYCERROR',0)
*E301077,78 Hesham (End)
*-- Get Result from Error Alert window and execute the process needed
FOR lnCounter=1 TO lnNoLine
  lnOption=0
  lcComm=MLINE(lcErrProcess,lnCounter)
  IF !EMPTY(lcComm)
    &lcComm
    IF lnOption<>0
      IF ATCLINE('"PUSH BUTTON"',lcErrProcess)>0
      lnCondNo=ATCLINE('"CONDITION '+STR(lnOption,1,1)+'"',lcErrProcess)+1
      FOR lnProc=lnCondNo TO MEMLINES(lcErrProcess)
          IF '"CONDITION' $ UPPER(MLINE(lcErrProcess,lnProc)) OR;
           '"PUSH BUTTON"' $ UPPER(MLINE(lcErrProcess,lnProc)) OR;
           ["END"] $ UPPER(MLINE(lcErrProcess,lnProc))
            EXIT
          ELSE
           lcToProc=MLINE(lcErrProcess,lnProc)
           &lcToProc
          ENDIF
      ENDFOR
     ENDIF
    ENDIF
  ENDIF
ENDFOR












*!**********************************************************
*!              PROCEDURE gpGo_Mastr
*!  this procedure will be called if the error need to release the
*! form and return to the upper level calling program or function
*!**********************************************************
PROCEDURE gpGo_Mastr
WAIT "Setting Environment" WINDOW NOWAIT
DO gfsetenvmt

WAIT "Saving Error Log file." WINDOW NOWAIT

DO gfsaveErr

WAIT CLEAR

RELEASE lstext, lmtext
CLEAR TYPEAHEAD
POP KEY ALL
ON KEY
SELECT (lcErrWorkAr)
*-- release the form that the error occurs on and return to the suitable calling
*-- procedure
DO CASE
  CASE !EMPTY(lcErrScr) AND !EMPTY(lcErrProgm)
    *-- if error occurs in a form or formset then release it
    IF UPPER(lcErrScrClass) = "FORM" OR UPPER(lcErrScrClass)="FORMSET"
      IF TYPE('oErrScr') = "O"
        oErrScr.Release
      ENDIF
    ENDIF
    *-- return to the calling program
    lcReturnTo = 'TO '+lcErrProgm
  CASE !EMPTY(lcErrScr) AND !EMPTY(lcErrCallFun)
   *-- if error occurs in a form or formset then release it
    IF UPPER(lcErrScrClass) = "FORM" OR UPPER(lcErrScrClass)="FORMSET"
      IF TYPE('oErrScr') = "O"
        oErrScr.Release
      ENDIF
    ENDIF
    *-- return to the calling function
    lcReturnTo = 'TO '+lcErrCallFun
  CASE !EMPTY(lcErrScr) AND EMPTY(lcErrProgm)
    *-- if error occurs in a form or formset then release it
    IF UPPER(lcErrScrClass) = "FORM" OR UPPER(lcErrScrClass)="FORMSET"
      IF TYPE('oErrScr') = "O"
        oErrScr.Release
      ENDIF
    ENDIF
    *-- return to the applicatin program if exist otherwise return to the main program
    lcReturnTo = 'TO '+IIF(!EMPTY(lcErrApp),lcErrApp,lcErrMainPrg)
  CASE !EMPTY(lcErrCallPrg)
    lcReturnTo = 'TO '+lcErrCallPrg
  CASE !EMPTY(lcErrApp)
    lcReturnTo = 'TO '+lcErrMainPrg
  OTHERWISE
    llErrClrRd = .T.
    lcReturnTo = 'TO MASTER'
ENDCASE

SET EXACT OFF
DO ERORSET
*-- if the return to level canot be determined then return to master
IF ALLTRIM(lcReturnTo) == "TO" .OR. EMPTY(lcReturnTo)
  RETURN TO MASTER
ELSE
  RETURN &lcReturnTo
ENDIF



FUNCTION gfSetEnvmt
PRIVATE disk_space
STORE DISKSPACE() TO disk_space
SET CONSOLE OFF
PUBLIC lstext, lmtext
STORE gcWorkDir+ustring() + ".TXT" TO lstext
STORE gcWorkDir+ustring() + ".TXT" TO lmtext
lcPsetup = _PDSETUP
_PDSETUP = ''
LIST MEMORY TO (lmtext)
_PDSETUP = lcPsetup
ON KEY && Null out hot keys
RETURN
* End FUNCTION  gfSETENVMT



*!*****************************************************************************
*!
*!      Procedure: gfSaveErr
*!
*!      Called by: Trap_EROR.PRG
*!               : GO_MASTR           (procedure in FP25EROR.PRG)
*!
*!
*!           Uses: SYSDATA.DBF
*!               : FP25EROR.DBF
*!
*!*****************************************************************************

FUNCTION gfSaveErr
PRIVATE uname
uName=""
IF glmultiusr
  STORE gcUser_ID TO uname
ENDIF

* ==========================
* Store info to error file
* ==========================
IF !USED('SYUEROR')
  RETURN
ENDIF

SELECT SYUEROR
lnCounter=1
lcCallProg=""

FOR lnCounter =1 TO ALEN(laErrTrac,1)
  lcCallProg = lcCallProg + IIF(lnCounter=1,'',CHR(13)+CHR(10))+laErrTrac[lnCounter,1]
ENDFOR
IF !LLERROR
  SET CONSOLE OFF
  APPEND BLANK

  REPLACE er_no WITH lnerror, er_msg WITH lcmsg, ;
      kode WITH lckode, mmodul WITH lcmodul, ddate ;
      WITH DATE(), ttime WITH TIME(), nline_no WITH lnline;
      ProgCalls WITH lcCallProg, cSession WITH IIF(TYPE('lcSession')='C',lcSession,'')





  IF TYPE("UNAME") <> "U" .AND. ! EMPTY(uname)
      REPLACE lan_user WITH uname
  ELSE
      REPLACE lan_user WITH "NOT AVAILABL"
  ENDIF (TYPE("UNAME") <> "U" .AND. ! EMPTY(uname))
* ==================================================
*$ Put info from the files created with LIST MEMORY
*$ and LIST STATUS into the memo field and then
*$ erase the text files
* ==================================================
*  APPEND MEMO Status FROM (lstext)
*  ERASE (lstext)
  APPEND MEMO Memory FROM (lmtext)
  ERASE (lmtext)
  SET CONSOLE ON
ENDIF

RETURN
* End procedure save_it


*!*****************************************************************************
*!
*!      Procedure: gfSaveErr
*!
*!      Called by: Trap_EROR.PRG
*!               : GO_MASTR           (procedure in FP25EROR.PRG)
*!
*!
*!           Uses: SYSDATA.DBF
*!               : FP25EROR.DBF
*!
*!*****************************************************************************

FUNCTION gfSaveErrNo
PRIVATE uname
uName=""
IF glmultiusr
  STORE gcUser_ID TO uname
ENDIF
IF !USED('SYUEROR')
  RETURN
ENDIF

* ==========================
* Store info to error file
* ==========================
SELECT SYUEROR
lnCounter=1
lcCallProg=""
DO WHILE !EMPTY(SYS(16,lnCounter))
  lcCallProg = lcCallProg + SYS(16,lnCounter) + CHR(13)+CHR(10)
  lnCounter=lnCounter+1
ENDDO
IF !llError
  SET CONSOLE OFF
  APPEND BLANK

  REPLACE er_no WITH lnerror, er_msg WITH lcmsg, ;
      kode WITH lckode, mmodul WITH lcmodul, ddate ;
      WITH DATE(), ttime WITH TIME(), nline_no WITH lnline;
      ProgCalls WITH lcCallProg, cSession WITH IIF(TYPE('lcSession')='C',lcSession,'')

  IF TYPE("UNAME") <> "U" .AND. ! EMPTY(uname)
      REPLACE lan_user WITH uname
  ELSE
      REPLACE lan_user WITH "NOT AVAILABL"
  ENDIF (TYPE("UNAME") <> "U" .AND. ! EMPTY(uname))

  SET CONSOLE ON
ENDIF

RETURN
* End procedure save_it


*-- called when ever the error that occurs need to
*-- cancel the execution of the application
PROCEDURE kancel
 =qquit()
* End procedure KANCEL

*-- called when ever the error that occurs need to
*-- cancel the execution of the application
PROCEDURE qquit
ON SHUTDOWN
ON KEY && Null out hot keys
CLEAR WINDOWS
DEACTIVATE MENUS
DEACTIVATE POPUPS
CLEAR POPUPS
CLEAR PROMPT
SET REFRESH TO 0, 200
SET DEVICE TO SCREEN
CLOSE DATA
SET LIBRARY TO

*! B999999,1 MMT 03/03/2005 Erase all temp files on work directory [START]
gnUserLog = gfUserList(.T.)
IF gnUserLog > 0
  CLEAR EVENTS
  RETURN .T.
ENDIF

IF !EMPTY(gcCurResouce) AND FILE(gcCurResouce)
  SET RESOURCE TO (gcCurResouce)
ELSE
  SET RESOURCE OFF
ENDIF
lcDefault = SET("Default")
SET DEFAULT TO (oAriaApplication.WorkDir)
lnNumFiles = ADIR(laWorkFiles)
IF lnNumFiles > 0
  FOR lnCount = 1 TO lnNumFiles
    TRY
      IF !EMPTY(laWorkFiles[lnCount,1]) AND UPPER(SUBSTR(laWorkFiles[lnCount,1],1,1)) == 'X'
        ERASE (oAriaApplication.WorkDir+laWorkFiles[lnCount,1])
      ENDIF
    CATCH
    ENDTRY
  ENDFOR
ENDIF
SET DEFAULT TO &lcDefault
*! B999999,1 MMT 03/03/2005 Erase all temp files on work directory [End]

CLEAR READ ALL
CLEAR EVENTS
* End procedure qquit


*-- return a unique file name
PROCEDURE ustring
PARAMETERS sendback
STORE SUBSTR(SYS(3), 4, 4) + ;
SUBSTR(SYS(2015),7,4) TO sendback
RETURN sendback



*!*****************************************************************************
*!
*!      Procedure: gfGo_Again
*!
*!
*!          Calls: ERORSET            (procedure in FP25EROR.PRG)
*!
*!    Other Files: &PRNT_DEVIC
*!    restore the enviroment so we can do a retry of the command that made the
*!    error
*!*****************************************************************************
FUNCTION gfGo_Again
*******************************************************************************

IF USED("SYUEROR")
  SELECT SYUeror
  USE
ENDIF (USED("SYUEROR"))
IF USED("sycerror")
  USE IN sycerror
ENDIF

IF ! EMPTY(curr_dbf)
  llTrpErr = .F.
  ON ERROR llTrpErr = .T.
  SELECT (curr_dbf)
  *B603481,1 Due to error "record is out of range" I changed the following line to use
  *B603481,1 variables instead of using the functions !!
  *IF BETWEEN(RECNO(),1,RECCOUNT())
  PRIVATE lnCurDBRec,lnMaxDBRec
  lnCurDBRec = RECNO()
  lnMaxDBRec = RECCOUNT()
  IF BETWEEN(lnCurDBRec,1,lnMaxDBRec)
    GO lnCurDBRec
    *B603481,1 End
  ELSE
    GO TOP
  ENDIF
  ON ERROR
ENDIF ! empty(curr_dbf)


SET CONSOLE &lckonsol
SET DEVICE TO &lcdevice
SET PRINTER TO &prnt_devic
SET PRINT &lcprint
RELEASE lstext, lmtext
DO erorset
CLEAR TYPEAHEAD
SET CURSOR &curs_set
RETURN
* End procedure gfGo_Again

*!*****************************************************************************
*!
*!      Procedure: ERORSET
*!
*!      Called by: FP25EROR.PRG
*!               : GO_MASTR           (procedure in FP25EROR.PRG)
*!               : gfGo_Again           (procedure in FP25EROR.PRG)
*!
*!          Calls: Trap_Eror.PRG
*!        reset the on error command before returning from the error handler
*!*****************************************************************************
PROCEDURE erorset
*E301077,78 Hesham (Start)
USE IN IIF(llUsrErr AND USED('SYUEROR'),'SYUEROR',0)
*E301077,78 Hesham (End)
ON ERROR DO gfEHan WITH ERROR(), MESSAGE(), ;
    MESSAGE(1), SYS(16), LINENO(), SYS(102), ;
    SYS(100), SYS(101), LASTKEY(), ALIAS(), ;
    SYS(5), SYS(12), SYS(6), SYS(2003), ;
    WONTOP(), SYS(2011), SYS(2018), SET("CURSOR")
RELEASE lstext, lmtext
RETURN
* End procedure ERORSET


*-- send the error information to the printer
PROCEDURE to_prn
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
DO qquit
PRIVATE prnstatus
STORE 1 TO prnstatus
SELECT sycerror
LOCATE FOR nerror_no = 3001
IF FOUND()
  *-- LOOP till the printer status is ready
  DO WHILE SYS(13) # "READY" AND prnstatus=1
    prnstatus=SHOWEROR()
  ENDDO
  *-- if printer status is ready then print the error
  IF prnstatus=1 AND SYS(13) = "READY"
    SET CONSOLE OFF
    LIST STATUS TO PRINT
    LIST MEMORY TO PRINT
    SET CONSOLE ON
  *-- if the printer status is not ready then display the error on screen
  ELSE
    SET CONSOLE ON
    HIDE WINDOW ALL
    DISP STATUS
    DISP MEMORY
    SHOW WINDOW ALL
  ENDIF && prnstatus OR SYS(13) = "READY")
ENDIF
SET CURSOR ON
CLEAR
SET CLEAR OFF
RETURN
* End procedure TO_PRN

*-- to control the error number 91
FUNCTION lfTrap91
*-- it the command that issued the error contain RELEASE
*-- then save the error and continue execution
IF "RELEASE" $ lcKode
  DO gfSaveErrNo
  SET CONSOLE &lcKonsol
  SET DEVICE TO &lcDevice
  DO ERORSET
*-- if the command does not contain release then return to master
ELSE
   DO gpGo_Mastr
ENDIF

*-- incase there was an error occured in an index
FUNCTION gfIndexErr
DO gpGo_Mastr
RETURN




*!*****************************************************************************
*!
*!      Function : lfDbfFix
*!
*!      Called by:
*!               :
*!
*!          Calls: gpGo_Mastr
*!               : CHECKVAL
*!*****************************************************************************
*E300999,1 Hesham (Start)
*E300999,1 Function to Fix dbf if corrupted
*E301316,1 Walid Abou El-Magd Stop using this function and call gfFixDbf()
*E301316,1 I will just put a RETURN statment to do that .

FUNCTION lfDbfFix
PRIVATE lcOldOrder
lcOldOrder=""
DIMENSION laFixInfo[10]
laFixInfo=" "
laFixInfo[1]=" "
*-- Set the what to do parameter
laFixInfo[2]="F" &&-- AutoFix
*-- Set the detail parameter
laFixInfo[3]="F" &&-- Summary

*-- Set the backup parameter
laFixInfo[4]="A" &&-- Ask

*-- Set the last backup parameter
laFixInfo[5]="A" &&-- Ask

*-- If error from gfOpenFile() or gfSysOpen() or gfSetup()


IF TYPE("lcMacroSub")='C' AND "USE" $ UPPER(lcMacroSub)
  lcOldOrder=lcMacroSub
  lcMacroSub=UPPER(ALLTRIM(STRTRAN(UPPER(lcMacroSub),'USE ')))
  DO CASE
    *-- Call from gfOpenFile() or gfSysOpen()
    CASE "NFILE"$lcMacroSub
      laFixInfo[1]=NFILE
    *-- Call from gfOpenFile() or gfSetup()
    CASE "LCPATH"$lcMacroSub OR "LAFILENAME"$lcMacroSub
      laFixInfo[1]=EVAL(lcMacroSub)
  ENDCASE
  =gfFixDbf(@laFixInfo)
  IF UPPER(laFixInfo[10])='FIXED'
    DO erorset
    &lcOldOrder
  ELSE
    DO erorset
    DO gpGo_Mastr
  ENDIF
  RETURN

ELSE
  *--The system is unable to detect the name of corrupted file , Please run maintain dictionary to check for corrupted files.
  =gfModalGen('QRM00362B00000','Dialog')
  DO erorset
  DO gpGo_Mastr
ENDIF
RETURN



*B603686,1 Hesham (Start)
*B603686,1 Get numeric overflow Fields
FUNCTION lfGetOvrf
PRIVATE lnCount,lcVarN
SELECT (curr_dbf)
SCATT MEMVAR
lcmissing = "File Name = "+DBF()+CHR(13)+CHR(10)+;
            "Key Value = " + EVAL(KEY()) + CHR(13)+CHR(10)+;
            "Fields = "
FOR lnCount = 1 TO FCOUNT()
  IF TYPE(FIELD(lnCount)) = 'N'
    lcVarN = "M."+field(lnCount)
    IF LEFT(STR(&lcVarN),1) = "*"
      *lcmissing = lcmissing + " " + field(lnCount)
      lcmissing = lcmissing + field(lnCount) + CHR(13)+CHR(10)
    ENDIF
  ENDIF
ENDFOR


*B603686,1 Hesham (Start)
*B603686,1 Control the numeric overflow error
FUNCTION lfTrapOvrf
DO gfSaveErrNo
SET CONSOLE &lcKonsol
SET DEVICE TO &lcDevice
SELECT (curr_dbf)
SCATT MEMVAR
lnFldNo = 0
lcmissing = ""
FOR lnCount = 1 TO FCOUNT()
  IF TYPE(FIELD(lnCount)) = 'N'
    lcVarN = "M."+field(lnCount)
    IF LEFT(STR(&lcVarN),1) = "*"
      lnFldNo = lnFldNo + 1
      DIME laFldErr[lnFldNo,2]
      laFldErr[lnFldNo,1] = field(lnCount)
      laFldErr[lnFldNo,2] = lnCount
    ENDIF
  ENDIF
ENDFOR
IF lnFldNo > 0
  FOR lnCount = 1 TO lnFldNo
    REPLACE (FIELD(laFldErr[lnCount,2])) WITH 0
  ENDFOR
ENDIF
DO ERORSET



*-- Display the error message and the needed control buttons
FUNCTION ShowEror
_WRAP=.T.
_LMARGIN = 2
_RMARGIN = 55
_ALIGNMENT = "LEFT"
lcPushB=IIF(ATCLINE('"PUSH BUTTON"',lcErrProcess)>0,;
         MLINE(lcErrProcess,ATCLINE('"PUSH BUTTON"',;
         lcErrProcess)+1),"\!  \<OK  ")

lcMessage = lfGetMemLn(ALLT(lcErrMsg))+CHR(13)+CHR(13)+;
"Error #: "+ALLTRIM(STR(lnError))+CHR(13)+;
"Line # : "+ALLTRIM(STR(lnline))+CHR(13)+;
"Prog. : "+lcmodul+CHR(13)+;
"Message : "+lcmsg


DO FORM (oAriaApplication.ScreenHome+"SY\Errform.scx") WITH lcMessage,lcPushB TO lnOption
RETURN lnOption


*-- get message memo lines and call the replace macro substitution function
FUNCTION lfGetMemLn
PARAMETER lcMemo
PRIVATE lnCounter
lcRet=''
*-- loop through the memo lines
FOR lnCounter = 1 TO MEMLINES(lcMemo)
lcret=lcRet+lfGetMac(ALLTRIM(STRTRAN(MLINE(lcMemo,lnCounter),CHR(13)+CHR(10),' ',1)))+' '
ENDFOR
RETURN lcRet


*-- get macross in the message string and replace it with the values
FUNCTION lfGetMac
PARAMETER lcNewStr
lcMacCon=""
*-- if there was a macro substitution characten in the string passed
IF '&' $ lcNewStr
  IF OCCURS(' ',lcNewStr)>0
    *-- look through the macro subst. occurs in the string
    FOR I=1 TO OCCURS(' ',lcNewStr)
      IF ATC(' ',lcNewStr,I)>ATC('&',lcNewStr)
        lcMacCon=SUBSTR(lcNewStr,ATC('&',lcNewStr),;
        ATC(' ',lcNewStr,I)-ATC('&',lcNewStr))
        EXIT
      ELSE
        lcMacCon=SUBSTR(lcNewStr,ATC('&',lcNewStr))
      ENDIF
    ENDFOR
  ELSE
    lcMacCon=lcNewStr
  ENDIF
ENDIF
IF !EMPTY(lcMacCon)
  lcMacCon=SUBSTR(lcMacCon,2)
  lcNewStr=STRTRAN(lcNewStr,"&"+lcMacCon,&lcMacCon)
ENDIF
RETURN lcNewStr      		


