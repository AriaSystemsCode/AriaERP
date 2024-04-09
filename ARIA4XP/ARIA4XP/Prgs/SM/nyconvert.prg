*:************************************************************************
*: Program file  : CONVERT.PRG			
*: Program desc. : MAIN CONVERSION PROGRAM
*: For screen    : CONVERT.SCX
*:         System: ARIA 4.0 XP
*:         Module: SM and SYSTEM INFORMATION
*:      Developer: AHMED MAHER (AMH)
*:************************************************************************
*: Modification:
*!* B609356,1 SMA 07/25/2010 Fix bug of creating empty *.cdx files [T20091027.0093]
*:************************************************************************
*--You have to select the company before running the conversion,
*--since this company will be the target location for the conversion.
IF EMPTY(oAriaApplication.ActiveCompanyID)
  =gfModalGen('TRM00321B00000','DIALOG')
  RETURN
ENDIF

PRIVATE laSpcHand
DIMENSION laSpcHand[4,2]
laSpcHand[1,1] = 'BOM'
laSpcHand[2,1] = 'BOMHEADR'
laSpcHand[3,1] = 'POSHDR'
laSpcHand[4,1] = 'POSLN'


LOCAL lcDictionaryDir
lcDictionaryDir = GETDIR(CURDIR(),"Enter the SQL dictionary folder of Aria 4.0 XP","SQL dictionary folder",64)
IF EMPTY(lcDictionaryDir)
  MESSAGEBOX("No SQL dictionary are defined, unable to proceed!",16,_SCREEN.Caption)
  RETURN
ENDIF

IF !FILE(lcDictionaryDir+'SYDFILES.DBF') .OR. !FILE(lcDictionaryDir+'SYDFLFLD.DBF') .OR.;
   !FILE(lcDictionaryDir+'SYDFIELD.DBF')
  MESSAGEBOX("Invalid SQL dictionary, unable to proceed!",16,_SCREEN.Caption)
  RETURN
ENDIF

*--Open Dictionary files.
=gfOpenFile(lcDictionaryDir+'SYDFILES','Cfile_nam','SH')
=gfOpenFile(lcDictionaryDir+'SYDFLFLD','Cfile_nam','SH')
=gfOpenFile(lcDictionaryDir+'SYDFIELD','Cfld_name','SH')
=gfOpenFile(lcDictionaryDir+'SYDINDEX','CFILE','SH')

*--Create SQL files.
IF !lfCreateSQL()
  MESSAGEBOX("Cannot create SQL files, unable to proceed!",16,_SCREEN.Caption)
  RETURN
ENDIF

*--Convert POSHDR and POSLN file.
IF laSpcHand[3,2] .AND. laSpcHand[4,2] .AND. !lfConvPO()
  MESSAGEBOX("Cannot Convert work orders files, unable to continue!",16,_SCREEN.Caption)
  RETURN
ENDIF

*--Convert BOM file.
IF laSpcHand[1,2] .AND. laSpcHand[1,2] .AND. !lfConvBom()
  MESSAGEBOX("Cannot Convert BOM file, unable to continue!",16,_SCREEN.Caption)
  RETURN
ENDIF

*--Normal Convert files.
SELECT SYDFILES
SCAN FOR ASCAN(laSpcHand,ALLTRIM(cfile_nam))=0
  IF !lfNormConv()
    MESSAGEBOX("Cannot Convert "+ALLTRIM(cfile_ttl)+" file, unable to continue!",16,_SCREEN.Caption)
    RETURN
  ENDIF
ENDSCAN

MESSAGEBOX("Conversion completed successfully",64,_SCREEN.Caption)

IF .F.
WAIT WINDOW '     Starting conversion for Aria 2.7 ,please wait ...     ' NOWAIT

*--Open conversion files.
=gfOpenFile(gcSysHome+'SYCNVMDL','SYCNVMDL','SH')
=gfOpenFile(gcSysHome+'SYCNVLIB','SYCNVLIB','SH')
=gfOpenFile(gcSysHome+'SYCNVLLN','SYCNVLLN','SH')

*E300630 SSH (BEGIN)
*--If there is a custom files start to add it to the standard
*--(SYCNVLIB & SYCNVLLN) conversion files to be taken in action.
IF FILE(gcSysHome+'SYCCNVLB.DBF')
  =gfOpenFile(gcSysHome+'SYCCNVLB','','SH')  && Custom library
  GO TOP
ENDIF
IF FILE(gcSysHome+'SYCCNVLL.DBF')
  =gfOpenFile(gcSysHome+'SYCCNVLL','','SH')  && Custom Lib line
ENDIF

IF USED('SYCCNVLB') AND !EOF('SYCCNVLB')
  SELECT SYCCNVLB
  SCAN
    *E301240,1 SSH 25/05/99 Change the way of appending records from customer
    *E301240,1 SSH          lib. to standard one.
    *IF !SEEK(cFile,'SYCNVLIB') OR SYCNVLIB.F_Name <> SYCCNVLB.F_Name
    *  SCATTER MEMVAR MEMO
    *  SELECT SYCNVLIB
    *  IF SEEK(cFile,'SYCNVLIB')
    *    REPLACE SYCNVLIB.F_Name WITH M.F_Name
    *  ELSE
    *    APPEND BLANK
    *    GATHER MEMVAR MEMO
    *  ENDIF
    *ENDIF
    SCATTER MEMVAR MEMO
    IF SEEK(m.cFile,'SYCNVLIB')
      SELECT SYCNVLIB
      GATHER MEMVAR MEMO
    ELSE
      INSERT INTO SYCNVLIB FROM MEMVAR
    ENDIF
    *E301240,1 SSH(END)
  ENDSCAN
ENDIF
IF USED('SYCCNVLL') AND !EOF('SYCCNVLL')
  SELECT SYCCNVLL
  SCAN
    *E301240,1 SSH 25/05/99 Change the way of appending records from customer
    *E301240,1 SSH          lib. line to standard one.
    *IF !SEEK(cFile,'SYCNVLLN')
    SCATTER MEMVAR MEMO
    IF SEEK(m.cFile+m.cFld_Name,'SYCNVLLN')
      SELECT SYCNVLLN
      GATHER MEMVAR MEMO
    ELSE
      INSERT INTO SYCNVLLN FROM MEMVAR
    ENDIF
    *ENDIF  
    *E301240,1 SSH(END)
  ENDSCAN
ENDIF
IF USED('SYCCNVLL')
  USE IN SYCCNVLL
ENDIF
IF USED('SYCCNVLB')
  USE IN SYCCNVLB
ENDIF  
*E300630 SSH (END).


*--Create or update the installed modules file.
=gfOpenFile(gcSysHome+'SYCCOMP','Ccomp_id','SH')
=SEEK(gcAct_Comp)
=gfOpenFile(gcSysHome+'SYDAPPL','Capp_id','SH')
SELECT SYDAPPL
*E500259,1 Start, Added to pick the EDI module.
*SCAN FOR cApp_Id $ SYCCOMP.mComp_Mdl AND cApp_Id <> "SM" AND cModuleVer <> 'V'
SCAN FOR cApp_Id $ SYCCOMP.mComp_Mdl AND cApp_Id <> "SM"
*E500259,1 End.

  SELECT SYCNVMDL
  IF ! SEEK(gcAct_comp+SYDAPPL.cApp_Id)
    APPEND BLANK
    REPLACE cComp_ID  WITH gcAct_comp,;
            cApp_Id   WITH SYDAPPL.cApp_Id,;
            cApp_Name WITH SYDAPPL.cApp_Name
  ENDIF
  IF (cAutRef = 'Converting...' OR cAutRef = 'Reconverting...')
    REPLACE lFlag     WITH .F.
  ELSE
    REPLACE lFlag     WITH .T.,;
            mfile_app WITH ''
  ENDIF
ENDSCAN
*--System manager record.
*--To be the first module in module index list we put 'SY' as '00'
SELECT SYCNVMDL
IF SEEK(gcAct_comp+'SY')
  =RLOCK()
  REPLACE cApp_Id WITH '00'
  UNLOCK
ELSE
  IF ! SEEK(gcAct_comp+'00')
    APPEND BLANK
    REPLACE cComp_ID  WITH gcAct_comp,;
            cApp_Id   WITH '00',;
            cApp_Name WITH 'Main System'
  ENDIF
ENDIF
IF (cAutRef = 'Converting...' OR cAutRef = 'Reconverting...')
  REPLACE lFlag     WITH .F.
ELSE
  REPLACE lFlag     WITH .T.,;
          mfile_app WITH ''
ENDIF
SELECT SYCNVMDL
*E500259,1 Start.
*--Check if EDI Module Installed.
llEDI_Ins = .F.
IF SEEK(gcAct_comp+'EB')
  llEDI_Ins = .T.
ENDIF
*E500259,1 End.

SET KEY TO gcAct_Comp


*--Chck for available drives.
ibSrch = .F.
DIME laDrive[1]
lnOldDrv = 0
lcCurrDrv = FULLPATH('')
lcCurrErr = ON('ERROR')
ON ERROR llNoDrv = .T.
lcAllDrvs='CDEFGHIJKLMNOPQRSTUVWXYZ'
lnJ = 0
FOR lnI=1 TO 24
  llNoDrv = .F.
  lcChkDrv = SUBSTR(lcAllDrvs,lnI,1)+':\'
  SET DEFA TO (lcChkDrv)
  IF ! llNoDrv
     lnJ = lnJ + 1
     DIME laDrive[lnJ]
     laDrive[lnJ] = SUBSTR(lcAllDrvs,lnI,1)+':'
  ENDIF
ENDFOR
ON ERROR &lcCurrErr
SET DEFAULT TO (lcCurrDrv)


*--Initialize.
*--Screen locking setting.
lcMLock = SET('MULTILOCKS')
lnRepros= SET('REPROCESS')
SET MULTILOCKS ON
SET REPROCESS TO 1

lnScrCont  = 1   && Active screens no.
lnLastScnt = 6   && Total no. of screens.
ibSrch     = .F.
iberrlog   = 1
llTerminat = .F.
*B603386,1 Start.
llJrnAldyCk = .F.   && If journal option check already asked.
llBalJurl   = .F.   && Option to balance journal stock.
*B603386,1 End.

STORE '' TO lcConv,lcConv2,lcConv3,lcConv4     &&Bitmaps.
STORE ",RGB(192,192,192,192,192,192)" TO lcObColor,lcMFleColor

*--Errors file name.
lcTmpErr = gfTempName()
CREATE TABLE (gcWorkDir+lcTmpErr) (cErrTxt C(100))
INDEX ON cErrTxt TAG (lcTmpErr)


*--Screens Variables.
*--02 Window.
DECLARE laSComp[1],laASComp[1],laLstMod[2]
STORE '' TO laSComp,laASComp,lcSource,lcASource,lcOSource,lcOASource
STORE 1  TO lnSComp,lnASComp
llChkSpace = .T.
lcOldValue = ''
*--Source directories.
*--Aria 27 dir. 
*-gcSysHome
*-gcDataDir
*--Aria 26 dir.
*-lcA26Data
*-lcA26lib
*-lcA26Img
*--Adv1 dir.
*-lcAdvData
*-lcAdvSys
STORE '' TO lcA26Data,lcA26lib,lcA26Img,lcAdvData,lcAdvSys

*--03 Window.
lcDiamond = ""
*lcExpMdTxt= IIF(SYCNVMDL.lflag,lcDiamond,SPACE(1))+IIF(SYCNVMDL.capp_id='00','SY',SYCNVMDL.capp_id)+'-'+PADR( SYCNVMDL.capp_name,20)+'-'+ PADR(SYCNVMDL.cAutRef,15)+'-'+DTOC(SYCNVMDL.ddate)+'-'+SUBSTR(SYCNVMDL.cCnvTime,1,5)

*--04 Window.
DIME laCurrncy[1]
laCurrncy = ' '
lnCurrncy = 1
lcOldBasCur = ''
lcDefWipWH =  ''


*--The following files may be opened shared before run the conversion
*--This files will be converted and need to be opened exclusve and closed.
DIME laOpnWhnExt[9]
laOpnWhnExt[9] = .F.
laOpnWhnExt[1]=USED('OBJECTS')
laOpnWhnExt[2]=USED('OBJLINK')
laOpnWhnExt[3]=USED('FISHD')
laOpnWhnExt[4]=USED('FSPRD')
laOpnWhnExt[5]=USED('FSHLD')
laOpnWhnExt[6]=USED('SEQUENCE')
laOpnWhnExt[7]=USED('GL_LINK')
laOpnWhnExt[8]=USED('ACCOD')
laOpnWhnExt[9]=USED('CODES')
=gfOpenFile(gcDataDir+'CODES','Ccode_no','SH')

*--For New Codes.
*--Read need default codes used in some files update.
*-Inventory adjustment reason (CADJREASON).
*-Payment type (CARPTYPE).
*-Bank adjustment reason (CBNKADJ).
*-Discount code (CDISCCODE).
*-Purchase group (CPURCODE).
*-Credit Decline reason (DECL_CODE).
*-Royalty (ROYALTY).
SELECT CODES
*B802998,1 Start, Commented old and added new.
*lcGVr_AdjR = IIF(SEEK(SPACE(2)+'CADJREASON'),CODES.cCode_No,'')
*lcGVr_ARRf = IIF(!EMPTY(lcGVr_AdjR) AND ;
* SEEK(gcAct_Comp+'CADJREASON'+lcGVr_AdjR+SPACE(30)+'GLACCOUNT'),CODES.cRltd_Vlu,'') 
*lcGVr_PyTp = IIF(SEEK(SPACE(2)+'CARPTYPE  '),CODES.cCode_No,'')
*lcGVr_PTRf = IIF(!EMPTY(lcGVr_PyTp) AND ;
* SEEK(gcAct_Comp+'CARPTYPE  '+lcGVr_PyTp+SPACE(30)+'CARGLACC'),CODES.cRltd_Vlu,'') 
*lcGVr_BnkR = IIF(SEEK(SPACE(2)+'CBNKADJ   '),CODES.cCode_No,'')
*lcGVr_DsCd = IIF(SEEK(SPACE(2)+'CDISCCODE '),CODES.cCode_No,'')
*lcGVr_PrGp = IIF(SEEK(SPACE(2)+'CPURCODE  '),CODES.cCode_No,'')
*lcGVr_DclR = IIF(SEEK(SPACE(2)+'DECL_CODE '),CODES.cCode_No,'')
*lcGVr_Royl = IIF(SEEK(SPACE(2)+'ROYALTY   '),CODES.cCode_No,'')
*lcGVr_Term = IIF(SEEK(SPACE(2)+'CTERMCODE '),CODES.cCode_No,'')

*--Read conversion global variables.
SELECT CODES
*-Bank adjustment reason.
lcGVr_BnkR = IIF(SEEK('D'+'CBNKADJ   '),CODES.cCode_No,'')
*-Discount code.
lcGVr_DsCd = IIF(SEEK('D'+'CDISCCODE '),CODES.cCode_No,'')
*-Purchase group.
lcGVr_PrGp = IIF(SEEK('D'+'CPURCODE  '),CODES.cCode_No,'')
*-Credit Decline reason.
lcGVr_DclR = IIF(SEEK('D'+'DECL_CODE '),CODES.cCode_No,'')
*-Royalty code.
lcGVr_Royl = IIF(SEEK('D'+'ROYALTY   '),CODES.cCode_No,'')
*-Term code.
lcGVr_Term = IIF(SEEK('D'+'CTERMCODE '),CODES.cCode_No,'')

*--Default inventory adjustment reason and account.
DIME laDRltFld[1,2]
STORE '' TO lcGVr_AdjR,lcGVr_ARRf,laDRltFld
IF SEEK('D'+'CADJREASON')
  lcGVr_AdjR = CODES.cCode_No
  laDRltFld[1,1] = 'GLACCOUNT'
  laDRltFld[1,2] = 'lcGVr_ARRf'
  =gfRltFld(lcGVr_AdjR,@laDRltFld,'CADJREASON')
ENDIF

*--Default AR type, used in credit,arhist.
STORE '' TO lcGVr_PyTp,lcGVr_PTRf,laDRltFld
IF SEEK('D'+'CARPTYPE  ')
  lcGVr_PyTp = CODES.cCode_No
  laDRltFld[1,1] = 'CARGLACC'
  laDRltFld[1,2] = 'lcGVr_PTRf'
  =gfRltFld(lcGVr_PyTp,@laDRltFld,'CARPTYPE  ')
ENDIF

*--Default AR gl account, used in credit,debit,arhist.
STORE '' TO lcGVr_CrAr,lcGVr_CrAA,laDRltFld
IF SEEK('D'+'CCREDITCOD')
  lcGVr_CrAr = CODES.cCode_No
  laDRltFld[1,1] = 'CADJACCT'
  laDRltFld[1,2] = 'lcGVr_CrAA'
  =gfRltFld(lcGVr_CrAr,@laDRltFld,'CCREDITCOD')
ENDIF
STORE '' TO lcGVr_DbAr,lcGVr_DbAA,laDRltFld
IF SEEK('D'+'TRANCODE  ')
  lcGVr_DbAr = CODES.cCode_No
  laDRltFld[1,1] = 'CADJACCT'
  laDRltFld[1,2] = 'lcGVr_DbAA'
  =gfRltFld(lcGVr_DbAr,@laDRltFld,'TRANCODE  ')
ENDIF
*B802998,1 End.

WAIT CLEAR

*--Call Conversion screen.
DO (gcScrDir+"SM\CONVERT.SPR")


*--Restore old settings.
SET MULTILOCKS &lcMLock
SET REPROCESS TO lnRepros

*Close all previus selected files if opened.
IF USED('OBJECTS')
  USE IN OBJECTS
ENDIF  
IF USED('OBJLINK')
  USE IN OBJLINK
ENDIF  
IF USED('FISHD')
  USE IN FISHD
ENDIF  
IF USED('FSPRD')
  USE IN FSPRD
ENDIF  
IF USED('FSHLD')
  USE IN FSHLD
ENDIF  
IF USED('SEQUENCE')
  USE IN SEQUENCE
ENDIF  
IF USED('GL_LINK')
  USE IN GL_LINK
ENDIF  
IF USED('ACCOD')
  USE IN ACCOD
ENDIF  
IF USED('CODES')
  USE IN CODES
ENDIF  

=IIF(laOpnWhnExt[1],gfOpenFile(gcDataDir+'OBJECTS','Objectid','SH')  ,.F.)
=IIF(laOpnWhnExt[2],gfOpenFile(gcDataDir+'OBJLINK','Cobjlink','SH')  ,.F.)
=IIF(laOpnWhnExt[3],gfOpenFile(gcDataDir+'FISHD','Compfyear','SH')   ,.F.)
=IIF(laOpnWhnExt[4],gfOpenFile(gcDataDir+'FSPRD','comfyrprdi','SH')  ,.F.)
=IIF(laOpnWhnExt[5],gfOpenFile(gcDataDir+'FSHLD','comfyrhdat','SH')  ,.F.)
=IIF(laOpnWhnExt[6],gfOpenFile(gcDataDir+'SEQUENCE','Cseq_type','SH'),.F.)
=IIF(laOpnWhnExt[7],gfOpenFile(gcDataDir+'GL_LINK','Gl_link','SH')   ,.F.)
=IIF(laOpnWhnExt[8],gfOpenFile(gcDataDir+'ACCOD','Compid','SH')      ,.F.)
=IIF(laOpnWhnExt[9],gfOpenFile(gcDataDir+'CODES','Ccode_no','SH')    ,.F.)

*--Close files on exit.
USE IN (lcTmpErr)
ERASE (gcWorkDir+lcTmpErr+'.DBF')
ERASE (gcWorkDir+lcTmpErr+'.CDX')
USE IN SYCNVLLN
USE IN SYCNVLIB
USE IN SYCNVMDL
RETURN
*--End...
ENDIF


*!*************************************************************
*! Name      : lfvBack
*! Developer : Timour A. K.
*! Date      : 09/10/98
*! Purpose   : Valid Back button.
*!*************************************************************
*! Calls     : lfActivate()
*!*************************************************************
FUNCTION lfvBack

*--Reduce active screen no.
lnScrCont = lnScrCont - 1
=lfActivate()
RETURN


*!*************************************************************
*! Name      : lfvNext
*! Developer : Timour A. K.
*! Date      : 09/10/98
*! Purpose   : Valid Next button.
*!*************************************************************
*! Calls     : lfActivate()
*!*************************************************************
FUNCTION lfvNext

*--Chck Before going to next screen.
DO CASE
  CASE lnScrCont = 1
    *--Automatic search on aria.
    IF ibSrch AND lnDrive <> lnOldDrv
      WAIT WINDOW "Search on Aria's directory(s) ,please wait..." NOWAIT
      STORE ' ' TO lcSource,lcASource
      lcCurrDrv = FULLPATH('')
      lcChkDrv  = ALLT(laDrive[lnDrive])+'\'
      SET DEFA TO (lcChkDrv)
      =ADIR(laSDiry,lcChkDrv+"*.",'D')
      FOR lnI=1 TO ALEN(laSDiry,1) 
        IF "." $ laSDiry[lnI,1]
          LOOP
        ENDIF  
        lcChkSDir = lcChkDrv+laSDiry[lnI,1]
        SET DEFA TO (lcChkSDir)
        IF FILE('CONTROL.DBF') AND FILE('ARIA25.MPX')
          lcSource = FULLPATH('')
          lnASelect = SELECT()
          =gfOpenFile(lcSource +'CONTROL','CONTROL','SH')
          SELECT syscode FROM CONTROL INTO ARRAY laSComp
          lnSComp = 1
          USE IN CONTROL
          SELECT(lnASelect)          
        ENDIF

        IF FILE(FULLPATH('')+'SYSFILES\SYCACCOD.DBF')
          lcASource = FULLPATH('')
          lnASelect = SELECT()
          *--Open file with different alias since 27 already opened this file.
          USE (lcASource+'SYSFILES\syccomp') IN 0 ALIAS 'AdvComp' SHARED
          SELECT Ccomp_id FROM AdvComp ;
               WHERE 'AP' $ mComp_mdl OR 'GL' $ mComp_mdl ;
               INTO ARRAY laASComp ORDER BY 1
          lnASComp = 1
          USE IN AdvComp
          SELECT(lnASelect)
        ENDIF

        IF !EMPTY(lcSource) AND !EMPTY(lcASource)
          EXIT
        ENDIF
      ENDFOR 
      lnOldDrv = lnDrive
      SET DEFAULT TO (lcCurrDrv)      
      WAIT CLEAR      
    ENDIF

  CASE lnScrCont = 2 AND llChkSpace
    *--Check modules instalation.
    IF !SEEK(gcAct_Comp,'SYCNVMDL')
      *--No modules was installed for this company, Unable to run the conversion.
      =gfModalGen('TRM00322B00000','DIALOG')
      RETURN
    ENDIF

    *--Check for a Disk Space.
    WAIT WINDOW 'Checking disk space...' NOWAIT
    *lnAvlSpace = ROUND((DISKSPACE()/1024/1024),1)    && in MB.
    lcConsSet = SET('CONS')
    SET CONS OFF
    DIR LIKE *.XXX TO FILE (gcWorkDir+'Tmpspace.TXT')
    SET CONS &lcConsSet
    CREATE CURSOR TMPSPCE (mTxt M)
    APPEND BLANK
    APPEND MEMO TMPSPCE.mTxt FROM (gcWorkDir+'Tmpspace.TXT')
    lcFreeSpac = SUBSTR(MLINE(TMPSPCE.mTxt,;
     ATCLINE("BYTES REMAINING",UPPER(TMPSPCE.mTxt))),1,;
     ATC("BYTES REMAINING",MLINE(TMPSPCE.mTxt,ATCLINE("BYTES REMAINING",UPPER(TMPSPCE.mTxt))))-1)
    lnAvlSpace = ROUND((VAL(lcFreeSpac)/1024/1024),1)    && in MB.
    USE

    STORE '' TO lcA26Data,lcA26lib,lcA26Img,lcAdvData,lcAdvSys
    *--Check required space.
    lnReqSpace = 0
    *--For aria26.
    IF !EMPTY(lcSource)
      llOpend1 = gfOpenFile(lcSource+'CONTROL','CONTROL','SH')
      SEEK laSComp[lnSComp]
      *--Read directory.
      lcA26Data = lcSource+ALLTRIM(Dd)
      lcA26lib  = lcSource+ALLTRIM(Lb)
      lcA26Img  = lcSource+ALLTRIM(Id)
      lcCurrPath = FULLPATH('')
      SET DEFAULT TO (lcA26Data)
      =ADIR(laPathFls)
      SET DEFAULT TO (lcCurrPath)
      IF llOpend1
        USE IN CONTROL
      ENDIF
      lnTotByte = 0
      FOR lnI = 1 TO ALEN(laPathFls,1)
        lnTotByte = lnTotByte + laPathFls[lnI,2]
      ENDFOR
      lnReqSpace = ( 2 * ROUND(VAL(STR(lnTotByte/1024/1024,12,2)),1) ) && in MB.
    ENDIF

    *--For Adv1.
    IF !EMPTY(lcASource)
      IF !USED('AdvComp')
        USE (lcASource+'SYSFILES\syccomp') IN 0 ALIAS 'AdvComp' SHARED
      ENDIF
      SELECT AdvComp
      SET ORDER TO TAG Ccomp_id
      SEEK laASComp[lnASComp]
      *--Read directory.
      lcAdvData = ALLTRIM(cCom_DDir)
      lcAdvSys  = lcASource+'SYSFILES\'
      lcCurrPath = FULLPATH('')
      SET DEFAULT TO (lcAdvData)
      =ADIR(laPathFls)
      SET DEFAULT TO (lcCurrPath)
      USE IN AdvComp
 
      lnTotByte = 0
      FOR lnI = 1 TO ALEN(laPathFls,1)
        lnTotByte = lnTotByte + laPathFls[lnI,2]
      ENDFOR
      lnReqSpace = lnReqSpace + ( 2 * ROUND(VAL(STR(lnTotByte/1024/1024,12,2)),1) ) && in MB.
    ENDIF
    llChkSpace = .F.
    WAIT CLEAR 
    
    IF lnReqSpace > lnAvlSpace 
      lcReqSpace = ALLT(STR(lnReqSpace,10,1))
      lcAvlSpace = ALLT(STR(lnAvlSpace,10,1))
      *--The required disk space for the conversion is 9999.9 MB. ,
      *--The available disk space right now is 9999.9 MB. ,
      *--Cannot proceed.
      IF gfModalGen('TRM00323B00006','DIALOG',lcReqSpace+'|'+lcAvlSpace)=2
        RETURN
      ENDIF
    ENDIF

  CASE lnScrCont = 3
    *--Check selecting of the modules.
    SELECT SYCNVMDL
    LOCATE FOR lFlag
    IF !FOUND()
      *--Nothing selected, Cannot continue.
      =gfModalGen('TRM00324B00000','DIALOG','selected')
      RETURN
    ENDIF
    *--Fill Currency array.
    IF !EMPTY(lcSource)
      DIME laCurrncy[1]
      lnCurrncy = 1
      IF FILE(lcA26Data+'CODE.DBF') 
        SELECT 0
        USE (lcA26Data+'CODE.DBF') SHARED
        SET ORDER TO TAG CODE
        SEEK 'F'
        lnJ = 0
        SCAN WHILE CODE='F'
          lnJ = lnJ + 1
          DIME laCurrncy[lnJ]
          laCurrncy[lnJ] = SUBSTR(CODE,2,3)
        ENDSCAN 
        lnCurrncy = ASCAN(laCurrncy,SUBSTR(gcBaseCurr,1,1))
        lnCurrncy = IIF(lnCurrncy = 0 ,1, lnCurrncy)
        USE IN CODE
      ENDIF

      *B603386,1 Start.
      llJrnlFnd = .F.
      SELECT SYCNVMDL
      IF SEEK(gcAct_Comp+'IC') AND lFlag
        llJrnlFnd = EMPTY(mFile_App) OR OCCURS('STYINVJL',mFile_App)<>0
      ENDIF
      IF !llJrnlFnd AND SEEK(gcAct_Comp+'MA') AND lFlag
        llJrnlFnd = EMPTY(mFile_App) OR OCCURS('MATINVJL',mFile_App)<>0
      ENDIF
      IF !llJrnlFnd AND SEEK(gcAct_Comp+'PO') AND lFlag
        llJrnlFnd = EMPTY(mFile_App) OR OCCURS('STYINVJL',mFile_App)<>0
      ENDIF
      IF !llJrnlFnd AND SEEK(gcAct_Comp+'PS') AND lFlag
        llJrnlFnd = EMPTY(mFile_App) OR OCCURS('STYINVJL',mFile_App)<>0
      ENDIF

      IF !llJrnAldyCk AND llJrnlFnd
        llBalJurl = ( gfModalGen('QRM00367B00038','DIALOG') = 1 )
        llJrnAldyCk = .T.
      ENDIF
      *B603386,1 End.
      
    ENDIF


  CASE lnScrCont = 4
    *--Nothing to check.
     
  CASE lnScrCont = 5
    *--Run main converion program.
    DO (gcAppHome+"SM\CNVMAIN.FXP")

ENDCASE

*--Increase active screen no.
lnScrCont = lnScrCont + 1
=lfActivate()

*--Proceed with next.
DO CASE
  CASE lnScrCont = 2
     _CUROBJ = OBJNUM(pbSdir)

  CASE lnScrCont = 4
    *--Check the setup for first time.
    SELECT SYCNVMDL
    LOCATE FOR lFlag AND !lSetReq
    IF FOUND()
      SHOW GET pbNext,1 PROMPT '\<Next >' DISABLE
    ENDIF

  CASE lnScrCont = 6
    SHOW GET pbSvLog ENABLE
    IF RECCOUNT(lcTmpErr)=0
      iberrlog=1
      =lfUpdError(SPACE(29)+'The conversion was succesfuly completed.')
      SHOW GET pbErrtxt
      SHOW GET pbSvLog DISABLE
    ENDIF
    *--Auto save Conv log.
    =lfvSavLog(.T.)
    SELECT SYCNVLIB
    UNLOCK ALL
    SHOW GET pbErrtxt

ENDCASE
RETURN


*!*************************************************************
*! Name      : lfvCancel
*! Developer : Timour A. K.
*! Date      : 09/10/98
*! Purpose   : Valid Cancel button.
*!*************************************************************
FUNCTION lfvCancel

IF lnScrCont <> 1 AND lnScrCont <> lnLastScnt
  *--Are you sure you want to cancel the conversion? Y/N
  IF gfModalGen('QRM00325B00006','DIALOG') = 2
    RETURN
  ENDIF   
ENDIF
CLEAR READ
RETURN


*!*************************************************************
*! Name      : lfActivate
*! Developer : Timour A. K.
*! Date      : 09/10/98
*! Purpose   : Activate screen objects.
*!*************************************************************
FUNCTION lfActivate

SHOW GETS DISABLE
lcWActWind = 'Conv0'+STR(lnScrCont,1) 
SHOW WINDOW (lcWActWind) TOP
SHOW GETS WINDOW (lcWActWind) ENABLE ONLY

*FOR lnI=1 TO lnLastScnt
*  lcWWindNo = 'Conv0'+STR(lnI,1) 
*  IF lnI = lnScrCont
*    SHOW GETS WINDOW (lcWWindNo) ENABLE ONLY
*  ELSE
*    SHOW GETS WINDOW (lcWWindNo) DISABLE ONLY
*  ENDIF
*ENDFOR

lcBkState = IIF(lnScrCont=1,'DISABLE','ENABLE')
lcNxState = IIF(lnScrCont=2 AND EMPTY(lcSource+lcASource),'DISABLE','ENABLE')
SHOW GET pbCancel ENABLE
SHOW GET pbBack &lcBkState
SHOW GET pbNext &lcNxState
*--Main conversion screen.
IF lnScrCont=5
  SHOW GET pbNext,1 PROMPT '< \<Proceed >'
ELSE
  SHOW GET pbNext,1 PROMPT '\<Next >' &lcNxState
ENDIF
IF lnScrCont=lnLastScnt
  SHOW GET pbBack DISABLE
  SHOW GET pbNext DISABLE
  SHOW GET pbCancel,1 PROMPT '\<Finished'
ENDIF
RETURN


*!*************************************************************
*! Name      : lfvGetSor
*! Developer : Timour A. K.
*! Date      : 09/10/98
*! Purpose   : Get Aria26 scource directory.
*!*************************************************************
*! Called From  : Window (02)
*!*************************************************************
FUNCTION lfvGetSor

lcSource = GETDIR()
lcSource = ALLT(lcSource)
IF !EMPTY(lcSource) AND FILE(lcSource+'CONTROL.DBF')
  lnASelect = SELECT()
  llOpend1=gfOpenFile(lcSource +'CONTROL','CONTROL','SH')
  IF LEN(syscode) = 1
    *--Invalid Aria Version.
    =gfModalGen('INM00326B00000','DIALOG','Version')
    DECLARE laSComp[1]
    STORE ' ' TO laSComp,lcSource
  ELSE
    SELECT syscode FROM control INTO ARRAY laSComp
    lnSComp = 1
  ENDIF
  IF llOpend1
    USE IN CONTROL
  ENDIF
  SELECT(lnASelect)
ELSE
  IF !EMPTY(lcSource)
    *--Invalid Aria 2.6 Directory.
    =gfModalGen('INM00326B00000','DIALOG','2.6 Directory')
  ENDIF
  DECLARE laSComp[1]
  STORE ' ' TO laSComp,lcSource
ENDIF
lcNxState = IIF(EMPTY(lcSource+lcASource),'DISABLE','ENABLE')
SHOW GET pbNext &lcNxState
SHOW GET lnSComp
SHOW GET lcSource
IF !llChkSpace
  llChkSpace = (lcSource<>lcOSource)
ENDIF
RETURN


*!*************************************************************
*! Name      : lfvGetASor
*! Developer : Timour A. K.
*! Date      : 09/10/98
*! Purpose   : Get ADV1 scource directory.
*!*************************************************************
*! Called From  : Window (02)
*!*************************************************************
FUNCTION lfvGetASor
         
lcASource = GETDIR()
lcASource = ALLT(lcASource)
IF !EMPTY(lcASource) AND FILE(lcASource+'SYSFILES\SYCACCOD.DBF')
  lnASelect = SELECT()
  *--Open file with different alias since 27 already opened this file.
  USE (lcASource+'SYSFILES\syccomp') IN 0 ALIAS 'AdvComp' SHARED
  SELECT Ccomp_id FROM AdvComp ;
       WHERE 'AP' $ mComp_mdl OR 'GL' $ mComp_mdl ;
        INTO ARRAY laASComp ;
        ORDER BY 1
  lnASComp = 1
  USE IN AdvComp
  SELECT(lnASelect)
ELSE
  IF !EMPTY(lcASource)
    *--Invalid Aria Advantage Series Directory.
    =gfModalGen('INM00326B00000','DIALOG','Advantage Series Directory')
  ENDIF
  DECLARE laASComp[1]
  STORE ' ' TO laASComp,lcASource
ENDIF
lcNxState = IIF(EMPTY(lcSource+lcASource),'DISABLE','ENABLE')
SHOW GET pbNext &lcNxState
SHOW GET lnASComp
SHOW GET lcASource
IF !llChkSpace
  llChkSpace = (lcASource<>lcOASource)
ENDIF
RETURN


*!*************************************************************
*! Name      : lfOldValue
*! Developer : Timour A. K.
*! Date      : 09/10/98
*! Purpose   : Function to store old value of the current filed.
*!*************************************************************
FUNCTION lfOldValue

lcOldValue = EVALUATE(SYS(18))
RETURN


*:*************************************************************
*: Name      : lfThrmo
*: Developer : Timour A. K.
*: Date      : 09/10/98
*: Purpose   : Thermometers.
*:*************************************************************
FUNCTION lfThrmo
PARA lnCurrRc,lnMaxum,lcTrmNo

IF '4' $ lcTrmNo
    lcSpThrm = REPLICATE(CHR(219),CEILING((lnCurrRc*34)/lnMaxum))
    lcSThPrc = ALLT(STR((lnCurrRc/lnMaxum)*100,3))+'%'
    SHOW GET lcSpThrm 
    SHOW GET lcSThPrc 
    lnCurNum = lnCurNum + 1
ENDIF
IF '3' $ lcTrmNo
    lcD3Thrm  = REPLICATE(CHR(219),CEILING((lnCurrRc*34)/lnMaxum))
    lcD3ThPrc = ALLT(STR((lnCurrRc/lnMaxum)*100,3))+'%'
    SHOW GET lcD3Thrm
    SHOW GET lcD3ThPrc
    lnCurNm3 = lnCurNm3 + 1
ENDIF    
IF '2' $ lcTrmNo
    lcD2Thrm  = REPLICATE(CHR(219),CEILING((lnCurrRc*34)/lnMaxum))
    lcD2ThPrc = ALLT(STR((lnCurrRc/lnMaxum)*100,3))+'%'
    SHOW GET lcD2Thrm
    SHOW GET lcD2ThPrc
    lnCurNm2 = lnCurNm2 + 1
ENDIF
IF '1' $ lcTrmNo
  lcMFleColor= IIF(lcMFleColor=lcObColor,gcObjColor,lcObColor)
  SHOW GET lcMFleDesc COLOR &lcMFleColor

  lcD1Thrm  = REPLICATE(CHR(219),CEILING((lnCurrRc*34)/lnMaxum))
  lcD1ThPrc = ALLT(STR((lnCurrRc/lnMaxum)*100,3))+'%'
  SHOW GET lcD1Thrm
  SHOW GET lcD1ThPrc
  lnCurNm1 = lnCurNm1 + 1
ENDIF
RETURN


*!*************************************************************
*! Name      : lfvStart
*! Developer : Timour A. K.
*! Date      : 09/10/98
*! Purpose   : Start converting Setup.
*!*************************************************************
*! Called From  : Window (04)
*!*************************************************************
FUNCTION lfvStart

*--Run setup conversion program.
=CNVSETUP()
SHOW GET pbNext ENABLE
RETURN


*!*************************************************************
*! Name      : lfvModSel
*! Developer : Timour A. K.
*! Date      : 09/10/98
*! Purpose   : Valid Selecting Modules.
*!*************************************************************
*! Called From  : Window (03)
*!*************************************************************
FUNCTION lfvModSel

SELECT SYCNVMDL
REPLACE lflag WITH !lFlag
lcDetStat = IIF(! SYCNVMDL.lFlag ,'DISABLE','ENABLE')
SHOW GET pbMDet &lcDetStat
SHOW GET lsAllMod
RETURN

*!*************************************************************
*! Name      : lfValdFl
*! Developer : Timour A. K.
*! Date      : 09/10/98
*! Purpose   : Valid remove files in mover.
*!*************************************************************
*! Passed Parameters  : lnOption-> option no. in mover.
*!*************************************************************
*! Returns            : .T. or .F. if not valid.
*!*************************************************************
FUNCTION lfValdFl
PARAMETERS lnOption

*--You cannot remove this file(s), it was converting right now!
DO CASE
  CASE lnOption=3
  IF '(In-Work)' $ laTarget[lsTarget]
    =gfModalGen('TRM00327B00000','DIALOG')
    RETURN .F.
  ENDIF
  CASE lnOption=4
     llTremnte=.F. 
     FOR lnI=1 TO ALEN(laTarget,1)
       IF '(In-Work)' $ laTarget[lnI]
         llTremnte=.T. 
         EXIT
       ENDIF
     ENDFOR
     IF llTremnte
       =gfModalGen('TRM00327B00000','DIALOG')
       RETURN .F.
     ENDIF
ENDCASE  


*!*************************************************************
*! Name      : lfvModDet
*! Developer : Timour A. K.
*! Date      : 09/10/98
*! Purpose   : Select the selected Module files (Detail).
*!*************************************************************
*! Called From  : Window (03)
*!*************************************************************
FUNCTION lfvModDet

*--Selected module.
lcfileMod = IIF(SYCNVMDL.cApp_Id='00','SY',SYCNVMDL.cApp_Id)
llInWork  = (SYCNVMDL.cAutRef = 'Converting...' OR SYCNVMDL.cAutRef = 'Reconverting...')

*--Define target array.
DIME laSource[1],laTarget[1]
=lfDefneFls()

SELECT SYCNVMDL
=gfMover(@laSource,@laTarget,"Select "+ALLT(SYCNVMDL.cApp_name)+" files",.T.,'lfValdFl')

*--Save update files selection.
SELECT SYCNVMDL
IF EMPTY(laTarget[1])
  REPLACE lFlag     WITH .F.,;
          mfile_app WITH ''
  SHOW GET pbMDet DISABLE
  SHOW GET lsAllMod
ELSE
  REPLACE mfile_app WITH ''
  IF ALEN(laSource,1) <> ALEN(laTarget,1)
    FOR lnI=1 TO ALEN(laTarget,1)  
      REPLACE mfile_app WITH mfile_app +','+PADR(laTarget[lnI],10)
    ENDFOR
  ENDIF
ENDIF

IF llInWork
  llSelOk = .F.
  FOR lnI=1 TO ALEN(laTarget,1)
    IF !('(In-Work)' $ laTarget[lnI])
      llSelOk = .T.
      EXIT
    ENDIF
  ENDFOR
  IF ! llSelOk
    REPLACE lFlag WITH .F.
    SHOW GET pbMDet DISABLE
    SHOW GET lsAllMod
  ENDIF
ENDIF
RETURN


*!*************************************************************
*! Name      : lfDefneFls
*! Developer : Timour A. K.
*! Date      : 09/10/98
*! Purpose   : Define target array.
*!*************************************************************
*! Called From  : Window (03)
*!*************************************************************
FUNCTION lfDefneFls
*--Return target array.

SELECT PADR(cFile,10)+'.........' ;
       FROM SYCNVLIB ;
      WHERE lcfileMod $ mFile_app ;
       INTO ARRAY laSource ORDER BY 1

IF EMPTY(SYCNVMDL.mfile_app)
  =ACOPY(laSource,laTarget)
ELSE
  lnJ=0
  FOR lnI=1 TO ALEN(laSource,1)  
    IF PADR(laSource[lnI],10) $ SYCNVMDL.mfile_app
       lnJ = lnJ + 1
       DIME laTarget[lnJ]
       laTarget[lnJ] = laSource[lnI]
    ENDIF
  ENDFOR
ENDIF


*--If some one already convert chech available source.
IF llInWork
  SELECT SYCNVLIB
  FOR lnI=1 TO ALEN(laTarget,1)  
    =SEEK(PADR(laTarget[lnI],10))
    llCanLock = RLOCK()
    UNLOCK 
    IF ! llCanLock
      laTarget[lnI] = PADR(laTarget[lnI],10) + ' (In-Work)'
      lnSrcelm=ASCAN(laSource,PADR(laTarget[lnI],10)+'.........')
      laSource[lnSrcelm] = laTarget[lnI]
    ENDIF
  ENDFOR
ENDIF
RETURN


*!*************************************************************
*! Name      : lfUpdError
*! Developer : Timour A. K.
*! Date      : 09/10/98
*! Purpose   : Save the occurs errors.
*!*************************************************************
FUNCTION lfUpdError
PARA lcErrTxt
lnAlias =SELECT()
IF !SEEK(ALLT(lcErrTxt),lcTmpErr)
  INSERT INTO (lcTmpErr) ( cErrTxt ) VALUES (lcErrTxt)
ENDIF
IF iberrlog=2
  *--lcErrTxt+'! The conversion will be terminated
  =gfModalGen('TRM00328B00000','DIALOG',lcErrTxt)
  llTerminat = .T.
ENDIF
SELECT (lnAlias)
RETURN









*:*************************************************************
*: Name      : CnvSetup.prg
*: Developer : Timour A. K.
*: Date      : 09/10/98
*: Purpose   : Convert Setup.
*:*************************************************************
*: Called From  : Window (03) lfvStart()
*:*************************************************************
FUNCTION CnvSetup

llOpend1=gfOpenFile(gcSysHome+'SYCCONFG','Modvar','SH')
llOpend2=gfOpenFile(gcDataDir+'SETUPS'  ,'Modvar','SH')
llOpend3=.F.

*--Restore 2.6 Source Data if aria 2.6 need to be convert.
IF !EMPTY(lcSource)
  llOpend3 = gfOpenFile(lcSource+'CONTROL','CONTROL','SH')
  SEEK laSComp[lnSComp]
  *--Open 2.6 Setups file.
  SELECT 0
  USE (lcA26Data+'SETUPS') ALIAS 'A26Setup' SHARED
  SET ORDER TO TAG Setups

  *--Open help file for 2.6 control file setups.
  =gfOpenFile(gcSysHome+'SYCNTSTP','SYCNTSTP','SH')
ENDIF
SELECT SYCCOMP
=SEEK(gcAct_Comp)
*-Setupd modules.
lcCStpMdls = mModlSet
*-Installed modules.
lcICmModl  = mComp_mdl

*--Scan on 2.7 setups.
SELECT SYCCONFG
COUNT ALL TO lnMax
lnCurNum = 1
STORE ' ' TO lcSpThrm,lcSThPrc,lcStpMsg
SHOW GET lcSpThrm 
SHOW GET lcSThPrc 
SHOW GET lcStpMsg
SCAN
  *--Update setup.
  IF cApp_id='ÿÿ' OR ( SEEK(gcAct_Comp+cApp_id,'SYCNVMDL') AND SYCNVMDL.lFlag)
    =lfUpdCSetp(cApp_ID,cFld_Name)

    *--Updatwe company modules setup.
    IF cApp_id<>'ÿÿ' AND !( cApp_id $ lcCStpMdls ) 
       lcCStpMdls = lcCStpMdls+IIF(EMPTY(lcCStpMdls),'',',')+cApp_id
    ENDIF
  ENDIF

  =lfThrmo(lnCurNum,lnMax,'4')
ENDSCAN

*B802383,1 Start.
SELECT SYCNVMDL
IF SEEK(gcAct_comp+'AP') AND !( 'AP' $ lcCStpMdls ) 
  lcCStpMdls = lcCStpMdls+IIF(EMPTY(lcCStpMdls),'',',')+'AP'
ENDIF
IF SEEK(gcAct_comp+'GL') AND !( 'GL' $ lcCStpMdls ) 
  lcCStpMdls = lcCStpMdls+IIF(EMPTY(lcCStpMdls),'',',')+'GL'
ENDIF
*B802383,1 End.

SELECT SYCCOMP
=RLOCK()
REPLACE mModlSet WITH lcCStpMdls
UNLOCK
*--Activate new modules set.
gcCmpModules = SYCCOMP.mModlSet

*B802031,1 03/08/99 (Start)
*B802031,1 Added to update the multi currency setup if there is
*B802031,1 more than one currency in 2.6 depends on message Y/N if the user select 
*B802031,1 P/O or MA module in setup conversion.
llPOModStp = (SEEK(gcAct_Comp+'PO','SYCNVMDL') AND SYCNVMDL.lFlag)
llMAModStp = (SEEK(gcAct_Comp+'MA','SYCNVMDL') AND SYCNVMDL.lFlag)
*--Converting setup for PO or MA and have more than currency.
IF ( llPOModStp OR llMAModStp ) AND ALEN(laCurrncy,1) > 1
  llChgMCur  = .F.
  SELECT SETUPS
  LOCATE FOR CFLD_NAME = 'LLMULCURR'
  IF FOUND() AND (TYPE('EVAL(MLINE(MDATA_DEF,1))') = 'L') AND (EVAL(MLINE(MDATA_DEF,1)) = .F.)
    IF llPOModStp
      SELECT 0
      USE (lcA26Data+'POSHDR') ALIAS 'APOSHDR' SHARED
      LOCATE FOR CURRENCY <> laCurrncy[1] AND !EMPTY(CURRENCY)
      IF FOUND()
        llChgMCur = .T.
      ENDIF
      USE
    ENDIF
    
    IF llMAModStp AND !llChgMCur
      SELECT 0
      USE (lcA26Data+'POFHDR') ALIAS 'APOFHDR' SHARED
      LOCATE FOR CURRENCY <> laCurrncy[1] AND !EMPTY(CURRENCY)
      IF FOUND()
        llChgMCur = .T.
      ENDIF
      USE 
    ENDIF

  ENDIF
  *--More than one currency, ask for update to multi currency.
  IF llChgMCur
    *-- More that one currency found in Aria 2.6, Would you like to switch to use a multi currency in Aria 2.7 ?
    IF gfModalGen('QRM00347B00006','DIALOG') = 1
      SELECT SETUPS
      =RLOCK() 
      REPLACE mData_Def WITH ".T."
      UNLOCK
    ENDIF
  ENDIF
ENDIF
*B802031,1 03/08/99 (End).


lcStpMsg = 'Click Next to continue with the conversion.'
SHOW GET lcStpMsg 

*--Update setup done field.
SELECT SYCNVMDL
IF SEEK(gcAct_Comp)
  REPLACE REST lSetReq WITH .T. WHILE cComp_ID=gcAct_Comp FOR lFlag
ENDIF

IF USED('A26SETUP')
  USE IN A26SETUP
ENDIF
IF USED('SYCNTSTP')
  USE IN SYCNTSTP
ENDIF
IF llOpend1 AND USED('SYCCONFG')
  USE IN SYCCONFG
ENDIF
IF llOpend2 AND USED('SETUPS')
  USE IN SETUPS
ENDIF
IF llOpend3 AND USED('CONTROL')
  USE IN CONTROL
ENDIF
RETURN



*:*************************************************************
*: Name      : lfUpdCSetp
*: Developer : Timour A. K.
*: Date      : 09/10/98
*: Purpose   : Convert/Create and Update Setup.
*:*************************************************************
*: Called From  : CnvSetup.PRG
*:*************************************************************
*: Parameters : Module Id and Setup field name to convert. 
*:*************************************************************
FUNCTION lfUpdCSetp
PARAMETERS lcApp_ID,lcFld_Name

lcAlias = SELECT()
*--Get default form config.
SCATTER MEMVAR MEMO

*--Create Aria2.7 setup.
SELECT SETUPS
IF !SEEK(lcApp_ID+lcFld_Name)
  APPEND BLANK
ENDIF
lcOldDefa = MLINE(MDATA_DEF,1)
GATHER MEMVAR MEMO  
REPLACE cDefa_typ WITH 'V'

*--If Aria2.6 was converted.
IF !EMPTY(lcSource)
  *--If this setup in 26 control file.
  IF SEEK(lcFld_Name,'SYCNTSTP')
    lcCnrlFld = SYCNTSTP.cKey_fld
    lcCnrlVal = CONTROL.&lcCnrlFld
    *--Gl version field has a defferen when save.
    lcCnrlVal = IIF(lcFld_Name='M_GL_VERS ',SUBSTR(lcCnrlVal,1,1),lcCnrlVal)
    IF lcFld_Name='M_SYS_DIR '
      lcCnrlVal = IIF(CONTROL.GL_VERS='ARIAGL',' ',lcCnrlVal)  
    ENDIF
    IF TYPE('lcCnrlVal') <> 'C'
      lcCnrlVal = IIF(lcFld_Name='M_LIMPCOST',IIF(lcCnrlVal,'.T.','.F.'),ALLT(STR(lcCnrlVal,12,3)))
    ENDIF
    =RLOCK() 
    REPLACE mData_Def WITH lcCnrlVal
    UNLOCK


  *--If setup not in control ,check setups file.
  ELSE
    *--Overwrite by Aria2.6 setup if exist.
    =SEEK('M','A26SETUP')
    *--Put material dyelot use as style dyelot use in 2.6 (special case).
    lcSetngNam = IIF(lcFld_Name='M_MATDYE  ','M_DYELOT  ',lcFld_Name) 
  
    SELECT A26SETUP
    LOCATE REST WHILE Code = 'M' FOR MemVarName = lcSetngNam
    IF FOUND()
      *--Update 27 setup.
      SELECT SETUPS
      =RLOCK() 

      *B603926,1 swap the values to match Aria27 [Begin]
      *REPLACE mData_Def WITH A26SETUP.Content
      IF A26SETUP.MemVarName = "M_UCCDIV"
        REPLACE mData_Def WITH IIF(A26SETUP.Content = 'Y','N','Y')
      ELSE 
        REPLACE mData_Def WITH A26SETUP.Content
      ENDIF
      *B603926,1 swap the values to match Aria27 [End]

      UNLOCK
    ENDIF
  ENDIF
ELSE
  *--If aria27 not converted but default in this exp.
  SELECT SETUPS
  IF lcFld_Name='M_MATDYE  ' OR lcFld_Name='M_DYELOT  ' OR lcFld_Name='M_WAREHOUS'
    =RLOCK() 
    REPLACE mData_Def WITH 'N'
    UNLOCK
  ENDIF

ENDIF

*--Convert the multi currency setup.
IF lcFld_Name='LLMULCURR'
  SELECT SETUPS
  =RLOCK() 
  REPLACE mData_Def WITH IIF(lcOldDefa=".T.",".T.",".F.")
  UNLOCK
ENDIF

SELECT(lcAlias)
RETURN


*:*************************************************************
*: Name      : lfvSavLog
*: Developer : Timour A. K.
*: Date      : 09/10/98
*: Purpose   : Save created log.
*:*************************************************************
*! Called From  : Window (06)
*!*************************************************************
FUNCTION lfvSavLog
PARA llAutoSv

IF !llAutoSv
  lcLogFile = GETFILE('TXT', 'Log file: def.(CnvLog.txt) ', 'Select',1)
ELSE
  lcLogFile = ALLT(FULLPATH(''))+"CnvLog.Sav"
ENDIF
IF EMPTY(lcLogFile)
  RETURN
ENDIF  
IF 'UNTITLED' $ UPPER(lcLogFile)
  lcLogFile = STRTRAN(lcLogFile,"Untitled","CnvLog.txt")
ENDIF
lcAlias = SELECT()
SELECT (lcTmpErr)
SET ORDER TO 
COPY TO (lcLogFile) TYPE DELIMITED
SELECT(lcAlias)
RETURN

*:*************************************************************
*: Name      : lfCreateSQL
*: Developer : AHMED MAHER (AMH)
*: Date      : 02/10/2004
*: Purpose   : Create SQL Tables
*:*************************************************************
FUNCTION lfCreateSQL

WAIT WINDOW '     Create SQL tables ,please wait ...     ' NOWAIT
SELECT SYDFILES
SCAN
  WAIT WINDOW '     Create '+ALLTRIM(cfile_ttl)+' file,please wait ...     ' NOWAIT
  LOCAL lnConnectionHandlar, loRemote, lcSqlStatment, llFirstField

  LOCAL lnSpcHand
  lnSpcHand = ASCAN(laSpcHand,ALLTRIM(cfile_nam))
  IF lnSpcHand # 0
    lnSpcHand = ASUBSCRIPT(laSpcHand,lnSpcHand,1)
    laSpcHand[lnSpcHand,2] = .T.
  ENDIF
  
  lcSqlStatment = "create table "+LOWER(ALLTRIM(cfile_nam))+" ("
  IF SEEK(cfile_nam,'SYDFLFLD')
    SELECT SYDFLFLD
    llFirstField = .T.
    SCAN REST WHILE cfile_nam = SYDFILES.cfile_nam
      lcSqlStatment = lcSqlStatment + IIF(llFirstField,"",",") + "[" + LOWER(ALLTRIM(cfld_name)) + "] "
      llFirstField = .F.
      IF SEEK(cfld_name,'SYDFIELD')
        LOCAL lcFldType,lcFldWidth
        DO CASE
          CASE SYDFIELD.cdata_typ = 'C'
            lcFldType  = "char"
            lcFldWidth = "("+ALLTRIM(STR(SYDFIELD.nfld_wdth))+")"
          CASE SYDFIELD.cdata_typ = 'N'
            lcFldType  = "numeric"
            lcFldWidth = "("+ALLTRIM(STR(SYDFIELD.nfld_wdth))+","+ALLTRIM(STR(SYDFIELD.nfld_dec))+")"
          CASE SYDFIELD.cdata_typ = 'L'
            lcFldType  = "bit"
            lcFldWidth = ""
          CASE SYDFIELD.cdata_typ = 'D'
            lcFldType  = "datetime"
            lcFldWidth = ""
          CASE SYDFIELD.cdata_typ = 'M'
            lcFldType  = "text"
            lcFldWidth = ""
        ENDCASE
        lcSqlStatment = lcSqlStatment + lcFldType + lcFldWidth
      ENDIF
    ENDSCAN
  ENDIF
  lcSqlStatment = lcSqlStatment + IIF(llFirstField,"",",") + "[nrecno_key] numeric(9,0))"
  
   
  loRemote = CREATEOBJECT('remotedataaccess')
  lnConnectionHandlar = loRemote.sqlrun(lcSqlStatment,'','',oAriaApplication.ActiveCompanyConStr,3,'SAVE')
  
  IF lnConnectionHandlar # 1
  	
    =loRemote.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
    RETURN .F.
  ENDIF
  
  SELECT SYDFILES
  IF SEEK(cfile_nam,'SYDINDEX')
    SELECT SYDINDEX
    SCAN REST WHILE cfile_nam = SYDFILES.CFILE_NAM
      LOCAL lnI
      LOCAL ARRAY laFields[1]
      loRemote.mindexfields(cIndx_Exp,@laFields)
      lcSqlStatment = "CREATE NONCLUSTERED INDEX ["+LOWER(ALLTRIM(cfile_tag))+"] ON ["+LOWER(ALLTRIM(cfile_nam))+"] ("
      llFirstField = .T.
      FOR lnI = 1 TO ALEN(laFields)
        lcSqlStatment = lcSqlStatment+IIF(llFirstField,"",",")+"["+LOWER(laFields[lnI])+"] "  && +IIF(lAscend,"A","DE")+"SC"
        llFirstField = .F.
      ENDFOR
      lcSqlStatment = lcSqlStatment + ")"
      
      lnConnectionHandlar = loRemote.sqlrun(lcSqlStatment,'','',oAriaApplication.ActiveCompanyConStr,3,'SAVE')
      
      IF lnConnectionHandlar # 1
        =loRemote.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
        RETURN .F.
      ENDIF
    ENDSCAN
  ENDIF
  loRemote = NULL
ENDSCAN
WAIT WINDOW '     Create SQL tables done     ' NOWAIT
WAIT CLEAR

*:*************************************************************
*: Name      : lfConvBom
*: Developer : AHMED MAHER (AMH)
*: Date      : 02/10/2004
*: Purpose   : Convert BOM file.
*:*************************************************************
FUNCTION lfConvBom

WAIT WINDOW '     Convert BOM file,please wait ...     ' NOWAIT
PRIVATE loBomHeadrCon,lcBomHeadr,loBomCon,lcBom

lcBomHeadr    = gfTempName()
lcBom         = gfTempName()
loBomHeadrCon = NULL
loBomCon      = NULL

IF !lfOpenSql('loBomHeadrCon','BOMHEADR',lcBomHeadr) .OR. !lfOpenSql('loBomCon','BOM',lcBom) .OR.;
   !gfOpenFile(oAriaApplication.DataDir+'STYLE','CSTYLE','SH') .OR.;
   !gfOpenFile(oAriaApplication.DataDir+'FABRIC','CFABRIC','SH') .OR.;
   !gfOpenFile(oAriaApplication.DataDir+'BOM','BOM','SH')
  RETURN .F.
ENDIF

LOCAL lnBuffering
lnBuffering = CURSORGETPROP("Buffering",lcBomHeadr)
=CURSORSETPROP("Buffering",3,lcBomHeadr)
SELECT (lcBomHeadr)
*B609356,1 SMA 07/25/2010 remove of clause to prevent empty *.cdx files from creation.....[BEGIN]
*INDEX ON CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID TAG (lcBomHeadr) OF (lcBomHeadr)
INDEX ON CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID TAG (lcBomHeadr) 
*B609356,1 SMA 07/25/2010 remove of clause to prevent empty *.cdx files from creation.....[END]
=CURSORSETPROP("Buffering",lnBuffering,lcBomHeadr)

SELECT STYLE
SCAN
  WAIT WINDOW '     Converting cost sheet of style : '+ALLTRIM(CSTYMAJOR)+'     ' NOWAIT
  SELECT (lcBOMHEADR)
  APPEND BLANK
  REPLACE CITMMAJOR  WITH STYLE.CSTYMAJOR     ,;
          CCSTSHT_ID WITH ''                  ,;
          CCSTSHTTYP WITH 'I'                 ,;
          CCSTSHTDSC WITH 'Default cost sheet',;
          LDEFCSTSHT WITH .T.                 ,;
          CSTATUS    WITH 'A'                 ,;
          LBASONSIZ  WITH .F.
  
  REPLACE NCOST1  WITH 0,;
          NCOST2  WITH 0,;
          NCOST3  WITH 0,;
          NCOST4  WITH 0,;
          NCOST5  WITH 0,;
          NCOST6  WITH 0,;
          NCOST7  WITH 0,;
          TOTCOST WITH 0
  
  REPLACE CADD_USER WITH '',;
          CADD_TIME WITH '',;
          DADD_DATE WITH {},;
          LLOK_STAT WITH .F.,;
          CLOK_USER WITH '',;
          DLOK_DATE WITH {},;
          CLOK_TIME WITH ''
  
  APPEND BLANK
  REPLACE CITMMAJOR  WITH STYLE.CSTYMAJOR     ,;
          CCSTSHT_ID WITH ''                  ,;
          CCSTSHTTYP WITH 'M'                 ,;
          CCSTSHTDSC WITH 'Default cost sheet',;
          LDEFCSTSHT WITH .T.                 ,;
          CSTATUS    WITH 'A'                 ,;
          LBASONSIZ  WITH .F.
  
  REPLACE NCOST1  WITH 0,;
          NCOST2  WITH 0,;
          NCOST3  WITH 0,;
          NCOST4  WITH 0,;
          NCOST5  WITH 0,;
          NCOST6  WITH 0,;
          NCOST7  WITH 0,;
          TOTCOST WITH 0
  
  REPLACE CADD_USER WITH '',;
          CADD_TIME WITH '',;
          DADD_DATE WITH {},;
          LLOK_STAT WITH .F.,;
          CLOK_USER WITH '',;
          DLOK_DATE WITH {},;
          CLOK_TIME WITH ''
  
  IF SEEK(STYLE.CSTYMAJOR,'BOM')
    =SEEK(STYLE.CSTYMAJOR+IIF(STYLE.MAKE,'M','I'),lcBOMHEADR)
    REPLACE CCSTSHT_ID WITH 'DEFCST'
    SELECT BOM
    SCAN REST WHILE CITMMAJOR = STYLE.CSTYMAJOR
      SCATTER MEMO MEMVAR
      SELECT (lcBom)
      APPEND BLANK
      GATHER MEMO MEMVAR
      REPLACE CCSTSHT_ID WITH 'DEFCST',;
              CCSTSHTTYP WITH EVALUATE(lcBOMHEADR+'.CCSTSHTTYP')
      SELECT (lcBOMHEADR)
      REPLACE LBASONSIZ WITH BOM.LBASONSIZ
      REPLACE ('NCOST'+BOM.TYP) WITH EVALUATE('NCOST'+BOM.TYP) + BOM.TOTCOST,;
              TOTCOST           WITH TOTCOST + BOM.TOTCOST
    ENDSCAN
  ENDIF
ENDSCAN

SELECT FABRIC
SCAN FOR MAKE
  WAIT WINDOW '     Converting cost sheet of fabric : '+ALLTRIM(FABRIC)+'     ' NOWAIT
  SELECT (lcBOMHEADR)
  APPEND BLANK
  REPLACE CITMMAJOR  WITH FABRIC.FABRIC       ,;
          CCSTSHT_ID WITH ''                  ,;
          CCSTSHTTYP WITH 'T'                 ,;
          CCSTSHTDSC WITH 'Default cost sheet',;
          LDEFCSTSHT WITH .T.                 ,;
          CSTATUS    WITH 'A'                 ,;
          LBASONSIZ  WITH .F.
  
  REPLACE NCOST1  WITH 0,;
          NCOST2  WITH 0,;
          NCOST3  WITH 0,;
          NCOST4  WITH 0,;
          NCOST5  WITH 0,;
          NCOST6  WITH 0,;
          NCOST7  WITH 0,;
          TOTCOST WITH 0
  
  REPLACE CADD_USER WITH '',;
          CADD_TIME WITH '',;
          DADD_DATE WITH {},;
          LLOK_STAT WITH .F.,;
          CLOK_USER WITH '',;
          DLOK_DATE WITH {},;
          CLOK_TIME WITH ''
  
  IF SEEK(FABRIC.FABRIC,'BOM')
    SELECT BOM
    LOCATE REST WHILE CITMMAJOR = FABRIC.FABRIC FOR lmaterial
    IF FOUND()
      REPLACE (lcBOMHEADR+'.CCSTSHT_ID') WITH 'DEFCST'
      SCAN REST WHILE CITMMAJOR = FABRIC.FABRIC FOR lmaterial
        SCATTER MEMO MEMVAR
        SELECT (lcBom)
        APPEND BLANK
        GATHER MEMO MEMVAR
        REPLACE CCSTSHT_ID WITH 'DEFCST',;
                CCSTSHTTYP WITH EVALUATE(lcBOMHEADR+'.CCSTSHTTYP')
        SELECT (lcBOMHEADR)
        REPLACE ('NCOST'+BOM.TYP) WITH EVALUATE('NCOST'+BOM.TYP) + BOM.TOTCOST,;
                TOTCOST           WITH TOTCOST + BOM.TOTCOST
      ENDSCAN
    ENDIF
  ENDIF
ENDSCAN

WAIT WINDOW '     Update SQL files,please wait ...     ' NOWAIT

IF USED('Style')
  USE IN Style
ENDIF

IF USED('Fabric')
  USE IN Fabric
ENDIF

IF USED('Bom')
  USE IN Bom
ENDIF

IF !lfUpdateSql(loBomHeadrCon,lcBomHeadr,'CITMMAJOR,CCSTSHTTYP,CCSTSHT_ID') .OR.;
   !lfUpdateSql(loBomCon,lcBom,'CITMMAJOR,CCSTSHTTYP,CCSTSHT_ID,TYP,CITMMASK,MFGCODE,ITEM,ICLR')
  RETURN .F.
ENDIF
loBomHeadrCon = NULL
loBomCon      = NULL

WAIT WINDOW '     Convert BOM file done     ' NOWAIT

IF USED(lcBomHeadr)
  USE IN (lcBomHeadr)
ENDIF

IF USED(lcBom)
  USE IN (lcBom)
ENDIF

*:*************************************************************
*: Name      : lfOpenSql
*: Developer : AHMED MAHER (AMH)
*: Date      : 02/10/2004
*: Purpose   : Open SQL files.
*:*************************************************************
FUNCTION lfOpenSql
LPARAMETERS lcConObj,lcTable,lcCursor

LOCAL lnConnectionHandlar, lcSqlStatment

lcSqlStatment = "SELECT * FROM " + lcTable

&lcConObj. = CREATEOBJECT('remotedataaccess')
lnConnectionHandlar = &lcConObj..sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3,'SAVE')

IF lnConnectionHandlar # 1
  =&lcConObj..CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF

*:*************************************************************
*: Name      : lfUpdateSql
*: Developer : AHMED MAHER (AMH)
*: Date      : 02/10/2004
*: Purpose   : Update SQL files.
*:*************************************************************
FUNCTION lfUpdateSql
LPARAMETERS loConObj,lcTable,LcPrimaryKeyList

LOCAL lnConnectionHandlar, lcTranCode, llReturn

llReturn   = .T.

lcTranCode = loConObj.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  =loConObj.CheckRetResult("BeginTran",lcTranCode,.T.)
  IF BETWEEN(lnRecNo,1,RECCOUNT(lcTable))
    GOTO lnRecNo IN (lcTable)
  ENDIF
  RETURN .F.
ENDIF

lnConnectionHandlar = loConObj.sqlupdate(lcTable,lcTranCode,SET("Datasession"),LcPrimaryKeyList)
IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
  =loConObj.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
  llReturn = .F.
ENDIF

lnConnectionHandlar = loConObj.CommitTran(lcTranCode)
IF lnConnectionHandlar # 1
  =loConObj.CheckRetResult("CommitTran",lnConnectionHandlar,.T.)
  llReturn = .F.
ENDIF
RETURN llReturn

*:*************************************************************
*: Name      : lfConvPO
*: Developer : Khalid Mohi (KHM)
*: Date      : 02/11/2004
*: Purpose   : Convert POSHDR & POSLN files
*:*************************************************************
FUNCTION lfConvPO

WAIT WINDOW '     Convert POSHDR file,please wait ...     ' NOWAIT
PRIVATE loPosHdrCon,loPosLnCon, lcPosHdr, lcPosLn

lcPosHdr    = gfTempName()
lcPosln     = gfTempName()
loPosHdrCon = NULL
loPosLnCon  = NULL

IF !lfOpenSql('loPosHdrCon','POSHDR',lcPosHdr) .OR. !lfOpenSql('loPosLnCon','POSLN',lcPosLn) .OR.;
   !gfOpenFile(oAriaApplication.DataDir+'POSHDR','POSHDR','SH') .OR.;
   !gfOpenFile(oAriaApplication.DataDir+'POSLN','POSLN','SH') OR;
   !gfOpenFile(oAriaApplication.DataDir+'BOM','BOM','SH')
  RETURN .F.
ENDIF

*-- Get the style major length
LOCAL oGetItemMask
oGetItemMask = CREATEOBJECT('GetItemMask')
lcMjrPct     = oGetItemMask.Do('PM')   &&Major picture
lnStyMajLen  = LEN(lcMjrPct)      &&Major Part lenth

SELECT POSHDR
SEEK "P"
SCAN REST WHILE cStyType+PO = "P"
  WAIT WINDOW "PO # : " + PO NOWAIT
  SCATTER MEMVAR MEMO
  =SEEK(m.cStyType+m.PO,'PosLn')
  SELECT(lcPosHdr)
  APPEND BLANK
  GATHER MEMVAR MEMO
  REPLACE Style      WITH " ",;
          Season     WITH " ",;
          nICost6    WITH 0  ,;
          nICost7    WITH 0  ,;
          nAct_Cost6 WITH 0  ,;
          nAct_Cost7 WITH 0  ,;
          nLan_Cost6 WITH 0  ,;
          nLan_Cost7 WITH 0  ,;
          nFCost6    WITH 0  ,;
          nFCost7    WITH 0  ,;
          nFActCost6 WITH 0  ,;
          nFActCost7 WITH 0  ,;
          nFLanCost6 WITH 0  ,;
          nFLanCost7 WITH 0  ,;
          cBusDocu   WITH "P",;
          cStyGrade  WITH PosLn.cStyGrade
  REPLACE INI_COMP   WITH {},;
          PCS_CANOLD WITH 0,;
          RECFLAG    WITH '',;
          PATTERN    WITH '',;
          DEL_DATE   WITH {},;
          CTKTTYPE   WITH '',;
          CBUNDLE    WITH '',;
          CREQUIS    WITH '',;
          CCONT_CODE WITH '',;
          CREVNO     WITH '',;
          CMRP       WITH '',;
          LASSTMAT   WITH .F.
ENDSCAN

SELECT PosLn
SEEK "P"
SCAN REST WHILE cStyType+PO+Style+STR(LineNo,6)+TranCd = "P"
  WAIT WINDOW "Style : " + Style NOWAIT 
  SCATTER MEMVAR MEMO
  lcDefCst = IIF(SEEK(PADR(SUBSTR(m.Style,1,lnStyMajLen),19),"BOM"),"DEFCST", " " )
  =SEEK(m.cStyType+m.PO,'PosHdr')
  SELECT(lcPosLn)
  APPEND BLANK 
  GATHER MEMVAR MEMO
  FOR lnCntr = 1 TO 5
    lcCntr = STR(lnCntr,1)
    REPLACE ('nFCost'+lcCntr)    WITH EVALUATE('m.nCost'+lcCntr)    ,;
   		      ('nFActCost'+lcCntr) WITH EVALUATE('m.nAct_Cst'+lcCntr) ,;
            ('nFLanCost'+lcCntr) WITH EVALUATE('m.nLan_Cst'+lcCntr) ,;
            ('nICost'+lcCntr)    WITH EVALUATE('m.nECost'+lcCntr)   ,;
            ('nAct_Cost'+lcCntr) WITH EVALUATE('m.nEActCost'+lcCntr),;
            ('nLan_Cost'+lcCntr) WITH EVALUATE('m.nELanCost'+lcCntr)
  ENDFOR
  
  FOR lnCntr = 6 TO 7
    lcCntr = STR(lnCntr,1)
    REPLACE ('nFCost'+lcCntr)    WITH 0 ,;
            ('nFActCost'+lcCntr) WITH 0 ,;
            ('nFLanCost'+lcCntr) WITH 0 ,;
            ('nICost'+lcCntr)    WITH 0 ,;
            ('nAct_Cost'+lcCntr) WITH 0 ,;
            ('nLan_Cost'+lcCntr) WITH 0
  ENDFOR
  REPLACE Complete   WITH PosHdr.Complete,;
          ShipVia    WITH PosHdr.ShipVia,;
          cPacktype  WITH " " ,;
          cPackUOM   WITH " " ,;
          nInPackQty WITH 0 ,;
          nInPackHgt WITH 0 ,;
          nInPackWdt WITH 0 ,;
          nInPackLen WITH 0 ,;
          nInPackWgt WITH 0 ,;          
          nMsPackQty WITH 0 ,;
          nMsPackHgt WITH 0 ,;
          nMsPackWdt WITH 0 ,;
          nMsPackLen WITH 0 ,;
          nMsPackWgt WITH 0
  REPLACE UOMBuy     WITH "EAC",;
          Conv       WITH 1,;
          cBusDocu   WITH "P",;
          Group      WITH "A",;
          cCstSht_ID WITH lcDefCst          
  REPLACE PATTERN    WITH '',;
          WIDTH      WITH '',;
          LCANCEL    WITH .F.,;
          LAPPROVSHP WITH .F.,;
          CSHPBOOKIG WITH '',;
          LVEND_ACK  WITH .F.
ENDSCAN

WAIT WINDOW '     Update SQL files,please wait ...     ' NOWAIT

IF USED('PosHdr')
  USE IN PosHdr
ENDIF

IF USED('PosLn')
  USE IN PosLn
ENDIF

IF USED('Bom')
  USE IN Bom
ENDIF

IF !lfUpdateSql(loPosHdrCon,lcPosHdr,'cBusDocu,cStyType,PO') .OR.;
   !lfUpdateSql(loPosLnCon,lcPosLn,'cBusDocu,cStyType,PO,Style,LineNo,TranCd')
  RETURN .F.
ENDIF
loPosHdrCon = NULL
loPosLnCon  = NULL

WAIT WINDOW '     Convert work orders files has been done successfully    ' NOWAIT

IF USED(lcPosHdr)
  USE IN (lcPosHdr)
ENDIF

IF USED(lcPosLn)
  USE IN (lcPosLn)
ENDIF

*:*************************************************************
*: Name      : lfNormConv
*: Developer : AHMED MAHER (AMH)
*: Date      : 02/12/2004
*: Purpose   : Convert Normal files.
*:*************************************************************
FUNCTION lfNormConv

PRIVATE loSQLCon,lcSQL,lcFile_Name,lcFile_Title

lcSQL        = gfTempName()
loSQLCon     = NULL
lcFile_Name  = ALLTRIM(cfile_nam)
lcFile_Title = ALLTRIM(cfile_ttl)

IF !lfOpenSql('loSQLCon',lcFile_Name,lcSQL) .OR.;
   !gfOpenFile(oAriaApplication.DataDir+lcFile_Name,'','SH')
  RETURN .F.
ENDIF

SELECT (lcFile_Name)
SCAN
  WAIT WINDOW '     Convert "+lcFile_Title+" file,please wait ...     ' NOWAIT
  SCATTER MEMVAR MEMO
  m.nrecno_key = RECNO()
  SELECT(lcSQL)
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN

WAIT WINDOW '     Update SQL files,please wait ...     ' NOWAIT

IF USED(lcFile_Name)
  USE IN (lcFile_Name)
ENDIF

IF !lfUpdateSql(loSQLCon,lcSQL,'nrecno_key')
  RETURN .F.
ENDIF
loSQLCon = NULL

WAIT WINDOW '     Convert '+lcFile_Title+'file has been done successfully    ' NOWAIT

IF USED(lcSQL)
  USE IN (lcSQL)
ENDIF
