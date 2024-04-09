*:************************************************************************
*: Program file  : CONVERT.PRG			
*: Program desc. : MAIN CONVERSION PROGRAM
*: For screen    : CONVERT.SPR
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: SM and SYSTEM INFORMATION
*:      Developer: TAK - Timour Abdalla Khalil 
*:************************************************************************
*E300630,1 SSH 01/01/99 Add the ability to convert the custom DBF's too.
*B802031,1 TAK 03/08/99 Added to update the multi currency setup if there is
*B802031,1 TAK          more than one currency in 2.6 depends on message Y/N.
*E500259,1 TAK 04/09/99 Added to convert an EDI files if module was installed.
*E301240,1 SSH 25/05/99 Convert Eil100 dbf's.
*B802383,1 TAK 07/04/99 Record that AP and GL setup was converted in global variable.
*B603386,1 TAK 01/12/00 Optinal convert inventory journal files, or default it
*B603386,1 TAK          by physical inventory of the current stock.
*B802998,1 TAK 01/30/00 Read correct defaults from codes file these defaults used in some converted files.
*B603926,1 SSE 09/27/00 When converting the option U.C.C manufactured ID based (Company/Division) happens to be swapped
*:************************************************************************

*--You have to select the company before running the conversion,
*--since this company will be the target location for the conversion.
IF EMPTY(gcAct_Comp)
  =gfModalGen('TRM00321B00000','DIALOG')
  RETURN
ENDIF

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