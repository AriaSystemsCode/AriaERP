*:----------------------------------------------------------------
*: Program file        : SMWMAIN.PRG
*: Program description : Company Wizard
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : System Manager - (SM)
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : N000185
*:----------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------
*: Called From         : None.
*:----------------------------------------------------------------
*: Modifications       : 
*:B804025,1 SSH 25/03/2001 Add default codes and update sales rep.
*:E038033,1 HMA 04/03/2004 Select only records belongs to A27(to split 
*:E038033,1                A27 records of SYDREPRT File from A4XP records).
*:----------------------------------------------------------------
*:
PRIVATE lcRunFrom
lnOldAls   = SELECT(0)
lcNextActn = ''
lcWizHlep  = ""
lcOldGcDDir = gcDataDir
lcOldMod   = ''
lnScrNo = 0
lnCharNo = 0
DIMENSION laAllMod[1] , laAllComp[1]
DIMENSION laStyStr[1] , laStyDesc[3,2]
lnStyCod = 1
laStyStr   = ""
laStyDesc  = ''
laAllMod = " "
laAllComp = ""
lcSScrHlp = "Style code structure will be based on style - color"
lcTempDir = ''
lcDiamond = ""
llHComp   = 0
laAllComp = ""
SELECT cApp_Id FROM (gcSysHome+"SYDAPPL") ;
       WHERE cApp_Id <> "SM" .AND. ;
             cApp_Id <> "SY" ;
       INTO ARRAY laAllComp
IF !gfSetup()
  RETURN
ENDIF  
WAIT WINDOW "Start company wizard screens..." NOWAIT
IF USED("SYWCOMP")
  USE IN SYWCOMP
ENDIF
=gfOpenFile(gcsyshome+"SYWCOMP","Ccomp_id","SH")
=gfOpenFile(gcsyshome+"SYWHELP","Sywhelp","SH")
=gfOpenFile(gcsyshome+"SYWSEQNC","","SH")
=gfOpenFile(gcsyshome+"SYWSETP","","SH")
=gfOpenFile(gcsyshome+"SYWCONFG","","SH")
=gfOpenFile(gcsyshome+"SYWACHRT","Cidentify","SH")
*--- Open wizard file
SELECT SYWSEQNC
*--- This filter to avoid navigate throug the unnessesary screens
SET FILTER TO (EMPTY(ALLTRIM(lcFltExp)) .OR. EVAL(ALLTRIM(lcFltExp))) .AND. lInstall
llSkip = .T.
IF lfCheckRes()
  DO WHILE !EOF('SYWSEQNC') AND !BOF('SYWSEQNC') .AND. llHComp<>1
    DO CASE
      CASE EMPTY(lcNextActn)
        SELECT SYWSEQNC
        REPLACE cRunFrm WITH '1'
        lnScrNo = lnScrNo+1
        llSkip = .F.
        DO gcAppHome+"SM\SMCMWZ.FXP"
        glQuitting = .T.
        lcOldMod   = laData[15]
        llSkip = .T.
        SELECT SYWSEQNC
        REPLACE cRunFrm   WITH ' '
      CASE lcNextActn = 'N' .AND. !EOF('SYWSEQNC')    && Next
        SELECT SYWCOMP
        REPLACE cCom_DDir WITH gcsyshome
        SELECT SYWSEQNC
        IF llSkip
         SKIP
          IF !USED("SYWCONFG")
            =gfOpenFile(gcsyshome+"SYWCONFG","Nvarpos","SH")
          ENDIF
          SELECT SYWCONFG
          SET ORDER TO Nvarpos
          IF SYWSEQNC.cPrgType<>'O' .OR. (SYWSEQNC.cPrgType='O' .AND. SEEK(ALLTRIM(SYWSEQNC.cParam1)))
            lnScrNo = lnScrNo +1
          ENDIF
          SELECT SYWSEQNC
        ELSE
          llSkip = .T.
        ENDIF
        SELECT SYWSEQNC
        REPLACE cRunFrm WITH '1'
        =lfControl()
        glQuitting = .T.
        SELECT SYWSEQNC
        REPLACE cRunFrm   WITH ' '
      CASE lcNextActn = 'B' .AND. !BOF('SYWSEQNC')    && Back
        SELECT SYWCOMP
        REPLACE cCom_DDir WITH gcsyshome
        SELECT SYWSEQNC
        IF llSkip
          SKIP -1
          IF !USED("SYWCONFG")
            =gfOpenFile(gcsyshome+"SYWCONFG","Nvarpos","SH")
          ENDIF
          SELECT SYWCONFG
          SET ORDER TO Nvarpos
          IF SYWSEQNC.cPrgType<>'O' .OR. (SYWSEQNC.cPrgType='O' .AND. SEEK(ALLTRIM(SYWSEQNC.cParam1)))
            lnScrNo = lnScrNo - 1
          ENDIF
          SELECT SYWSEQNC
        ELSE
          llSkip = .T.
        ENDIF
        SELECT SYWSEQNC
        REPLACE cRunFrm WITH '1'
        IF ALLTRIM(cScrNam) = "SMCMWZ"
          DO gcAppHome+"SM\SMCMWZ.FXP"
          lcOldMod   = laData[15]
        ELSE
         =lfControl()
        ENDIF
        glQuitting = .T.
        SELECT SYWSEQNC
        REPLACE cRunFrm   WITH ' '
      CASE lcNextActn = 'U'  && Resume Later
        SELECT SYWSEQNC
        REPLACE cRunFrm   WITH '1'
        REPLACE nPageNo   WITH lnScrNo
        =lfEndSesin(lcNextActn)
        glQuitting = .T.
        EXIT
      CASE lcNextActn = 'C'  && Cancel
        =lfEndSesin(lcNextActn)
        EXIT
        glQuitting = .T.
     CASE lcNextActn = 'F'  && Finish
        =lfVFinish()
        glQuitting = .T.
    ENDCASE
  ENDDO
ENDIF
IF USED("SYWCOMP")
  USE IN SYWCOMP
ENDIF
IF USED("CODES")
  USE IN CODES
ENDIF
gcDataDir = lcOldGcDDir
=gfCPClose()
=gfStatic()
=GFCLEANUP()
*-- clear read 

*!*************************************************************
*! Name      : lfDoScreen  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Function to call programs and screens.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStatic
*!                          gpDoProg
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : 
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfDoScreen()
*!*************************************************************
*!
FUNCTION lfDoScreen

PRIVATE lnOldAls , lcTmpComp , lcTempActn
lnOldAls = SELECT(0)
lcTmpComp  = gcAct_Comp
gcAct_Comp = laData[1]
SELECT SYWCOMP

REPLACE cCom_DDir WITH gcsyshome
SELECT SYWSEQNC
lcCurrWind = gcBaseWind
=gfStatic()
lcWindow   = UPPER(lcBaseWind)
llSavFis   = .F.
glFirsTime = .T.
gcBaseWind ='AWR'+ALLTRIM(cScrNam)
lcRunFrom = gcAppHome+"SM\"+ALLTRIM(cScrNam)+".FXP"
IF !EMPTY(cParam1)
  DO &lcRunFrom WITH EVAL(ALLTRIM(cParam1))
ELSE
  DO &lcRunFrom
ENDIF
lcTempActn = lcNextActn
IF SEEK ('WIN'+lcWindow+UPPER(gcUser_Id)+gcStation,'syustatc')   
  ** Restore data for the company program from static file. **
  RESTORE FROM MEMO syustatc.mObj_Data  ADDITIVE
ENDIF  
glQuitting = .F.
gcBaseWind = lcCurrWind
SELECT SYWCOMP
=SEEK(laData[1])
REPLACE cCom_DDir WITH lcTempDir
gcAct_Comp = lcTmpComp
lcNextActn = lcTempActn
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfControl  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Function to contral if it going to run screen/Function/install module.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : lfSetModul
*!                          lfDoScreen
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfControl()
*!*************************************************************
*!
FUNCTION lfControl

PRIVATE lnOldSys
DO CASE
  CASE  ALLTRIM(cPrgType)=='F'   && Function or procedure.
    DO ALLTRIM(cScrNam)
  CASE  ALLTRIM(cPrgType)$'CSO'   && Module to install (Company Setup , Screen to set module , Option grid).
    =lfSetModul(ALLTRIM(cParam1))
  CASE  ALLTRIM(cPrgType)=='P'   && Calling program and screens.
    =lfDoScreen()
ENDCASE

*!*************************************************************
*! Name      : lfSetModul  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Function to Setup module.
*!*************************************************************
*! Calls     : Procedures : SYCMWZ.
*!             Functions  : gfStatic
*!                          lfDoScreen
*!*************************************************************
*! Passed Parameters  : Module Id  (lcMod2Set)
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfSetModul()
*!*************************************************************
*!
FUNCTION lfSetModul
PARAMETERS lcMod2Set,llGlobalSet

PRIVATE lcTempActn
lcOldActCm = gcAct_comp
lcOldDataD = gcDataDir
lcOldActMd = gcAct_Appl
gcAct_Appl = lcMod2Set
gcAct_comp = laData[1]
gcDataDir  = gfGetDataDir(ALLTRIM(laData[11]))
SELECT SYWCOMP
lnRecCmp = RECNO()

IF EMPTY(laData[14])
  lcInsMdl  = SYWCOMP.mcomp_mdl
ELSE
  IF SEEK(laData[14],"SYWCOMP")
    lcInsMdl  = SYWCOMP.mcomp_mdl
  ELSE
    lcInsMdl  = laData[15]
  ENDIF
ENDIF

SELECT SYDOBJCT
SET ORDER TO TAG CAPP_ID

** Check if the program of module setup is **
** exist in the module objects or not...
llGlobalSet = !SEEK(lcMod2Set+lcMod2Set+'SETUP','SYDOBJCT')
SELECT SYWCOMP
** Save the current environment in the static file. **
lcCurrWind = gcBaseWind
=gfStatic()
lcWindow   = UPPER(lcBaseWind)
glFirsTime = .T.

IF !llGlobalSet
  gcBaseWind ='AWD' + lcMod2Set + 'WSETUP'
  ** Call the module setup program  to add **
  ** the account code struc. for the added **
  ** comapny...
  lcDataDir = ALLTRIM(laData[11])
  lcDataDir = gfGetDataDir(ALLTRIM(laData[11]))
  *lcAppName  = gcAppHome +lcMod2Set + '.APP'
  lcRunFrom = gcAppHome+lcMod2Set+"\"
*--- SSH  SPLIT
  IF FILE(lcRunFrom+lcMod2Set+"WSETUP.FXP")
    ERASE lcRunFrom+lcMod2Set+"WSETUP.FXP"
    ERASE gcScrDir+lcMod2Set+"\"+lcMod2Set+"WSETUP.SPR"
  ENDIF
  DO gcAppHome+"SM\"+lcMod2Set+"WSETUP.FXP" WITH .T.,laData[1],lcDataDir,lcInsMdl
*--- SSH SPLIT
ELSE
  lcAppName  = gcAppHome +lcMod2Set + '.APP'
  lcDataDir = ALLTRIM(laData[11])
  lcDataDir = gfGetDataDir(ALLTRIM(laData[11]))
  IF lcMod2Set <> "ÿÿ"
    lcOldProc = SET("PROCEDURE")
    IF FILE(lcAppName)
      SET PROCEDURE TO &lcAppName
    ENDIF
  ELSE
    =gfOpenFile(gcsyshome+"SYCCOMP","Ccomp_id","SH")
    *lcAppName  = gcAppHome +'SM.APP'
    *SET PROCEDURE TO &lcAppName
  ENDIF
  DO SYCMWZ.FXP WITH lcMod2Set,gcdataDir,laData[1]
  IF lcMod2Set <> "ÿÿ"
    SET PROCEDURE TO &lcOldProc
  ELSE
    USE IN SYCCOMP
  ENDIF
ENDIF
lcSavExact = SET('EXACT')
SET EXACT ON
IF SEEK ('WIN'+lcWindow+UPPER(gcUser_Id)+gcStation,'syustatc')   
  ** Restore data for the company program from static file. **
  lcTempActn = lcNextActn
  RESTORE FROM MEMO syustatc.mObj_Data  ADDITIVE
  lcNextActn = lcTempActn
ENDIF  
SET EXACT &lcSavExact
gcBaseWind = lcCurrWind
glQuitting = .F.
SELECT SYWCOMP
llModSetUse = .T.
IF !llGlobalSet
  llModSetUse = USED("SYW"+lcMod2Set+'SET')
  IF !llModSetUse
    USE (gcSysHome+"SYW"+lcMod2Set+"SET") IN 0
  ENDIF  
  SELECT ("SYW"+lcMod2Set+'SET')
  llModuleIns = lSetDon
ELSE
  lcTmpSetAl = gfTempName()  
  SELECT 0
  USE (gcSysHome+'SYWSETP') AGAIN ALIAS &lcTmpSetAl
  SET ORDER TO TAG NVARPOS
  llModuleIns = SEEK(lcMod2Set)
  USE IN (lcTmpSetAl)
ENDIF  
SELECT SYWCOMP
=SEEK(laData[1])
lcTempModul = mModlSet
IF !(lcMod2Set $ lcTempModul)
  REPLACE mModlSet  WITH mModlSet+IIF(llModuleIns,IIF(EMPTY(mModlSet),'',',')+lcMod2Set,'')
ENDIF  
lcCompTMod  = mComp_Mdl
IF !(lcMod2Set $ lcCompTMod)
  REPLACE mComp_Mdl  WITH mComp_Mdl+IIF(llModuleIns,IIF(EMPTY(mComp_Mdl),'',',')+lcMod2Set,'')
ENDIF  
IF laData[1] = gcAct_Comp
  gcCmpModules = ALLTRIM(mModlSet)
ENDIF  
IF !llModSetUse
  USE IN ("SYW"+lcMod2Set+'SET')
ENDIF  
gcAct_Appl = lcOldActMd
gcAct_Comp = lcOldActCm
gcDataDir  = lcOldDataD

*!*************************************************************
*! Name      : lfEndSesin  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Funciton to end session in both resume later or cancel.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : lfRestDBF
*!                          lfClsDBF
*!*************************************************************
*! Passed Parameters  : Resume / cancel (lcResCan)
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfEndSesin()
*!*************************************************************
*!
FUNCTION lfEndSesin
PARAMETER lcResCan

PRIVATE lnOldAls 

lnOldAls = SELECT(0)
IF lcResCan = 'C'   && Called from Cancel.
  =lfRestDBF(.T.)   && Function to Reset DBFS
ENDIF
=lfClsDBF("SYWCOMP")
=lfClsDBF("SYWCONFG")
=lfClsDBF("SYWSEQNC")
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfClsDBF  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Function to close dbf if use.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : None.
*!*************************************************************
*! Passed Parameters  : Dbf name (lcDBD2Cls)
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfClsDBF()
*!*************************************************************
*!
FUNCTION lfClsDBF
PARAMETER lcDBD2Cls

IF USED(lcDBD2Cls)
  USE IN &lcDBD2Cls
ENDIF

*!*************************************************************
*! Name      : lfRestDBF  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Function to reset dbfs.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : None.
*!*************************************************************
*! Passed Parameters  : Close/reset only (llClose)
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfRestDBF()
*!*************************************************************
*!
FUNCTION lfRestDBF
PARAMETER llClose

PRIVATE lnOldAls
lnOldAls = SELECT(0)
*--- Rest wizard company file.
llDummy = llClose .AND. lfClsDBF("SYWCOMP")
=lfOpnDbf("SYWCOMP")
SELECT SYWCOMP
DELETE ALL
=lfOpnDbf("SYWCOMP")

*--- Rest main wizard files.
=lfOpnDbf("SYWSEQNC")
REPLACE ALL cRunFrm  WITH ' ',;
            lInstall WITH lDefault,;
            nPageNo  WITH 0
llDummy = llClose .AND. lfClsDBF("SYWSEQNC")
*--- Rest Fiscal calender files.
=lfOpnDbf("SYWFSHD")
REPLACE ALL lSelect   WITH .F.,;
            cFislHead WITH "Fis. year",;
            cFissHead WITH "Fis. year"
llDummy = llClose .AND. lfClsDBF("SYWFSHD")

=lfOpnDbf("SYWFSPD")
REPLACE ALL lSelect WITH .F.
llDummy = llClose .AND. lfClsDBF("SYWFSPD")

*--- Rest Scale file
=lfOpnDbf("sywscal")
REPLACE ALL lSelect WITH .T.
llDummy = llClose .AND. lfClsDBF("sywscal")
*--- Rest Extended size scale file
=lfOpnDbf("sywscalh")
REPLACE ALL lSelect WITH .T.
llDummy = llClose .AND. lfClsDBF("sywscalh")


*--- Rest style code structure files.
=lfOpnDbf("SYWSCOD")
REPLACE ALL lSelect WITH .F.
llDummy = llClose .AND. lfClsDBF("SYWSCOD")

*--- Rest codes files .
=lfOpnDbf("SYWCODE")
REPLACE ALL lSelect WITH .T.;
            lStart  WITH .F.
llDummy = llClose .AND. lfClsDBF("SYWCODE")

*--- Rest Customer files .
=lfOpnDbf("SYWCUST")
REPLACE ALL lSelect WITH .T.;
            lStart  WITH .F.
llDummy = llClose .AND. lfClsDBF("SYWCUST")

*--- Rest Acount Code Structure Files.
*--- "VIP!!" Reset Account Code Structure Files
=lfOpnDbf("SYWGLSGV")   && Segmanet value
REPLACE ALL lSelect WITH IIF(cidentify="1",.T.,.F.)
=lfOpnDbf("SYWGLCHR")   && Chart Of Account
REPLACE ALL lSelect WITH IIF(cidentify="1",.T.,.F.)
=lfOpnDbf("SYWGLTYP")
REPLACE ALL lSelect WITH IIF(cidentify="1",.T.,.F.)
=lfOpnDbf("SYWACBAL")
REPLACE ALL lSelect WITH IIF(cidentify="1",.T.,.F.)

=lfOpnDbf("SYWACHRT")
REPLACE ALL lSelect WITH IIF(cIdentify="1" ,.T.,.F.)

*--- Rest SYWSETP file.
=lfOpnDbf("SYWSETP")
DELETE ALL
llDummy = llClose .AND. lfClsDBF("SYWSETP")

*--- Rest Gl/Ap SYWSETP file.
=lfOpnDbf("SYWGLSET")
DELETE ALL
llDummy = llClose .AND. lfClsDBF("SYWGLSET")

=lfOpnDbf("SYWAPSET")
DELETE ALL
llDummy = llClose .AND. lfClsDBF("SYWAPSET")

=lfOpnDbf("SYWWAREH")
SELECT SYWWAREH
DELETE ALL
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfvIns  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Function to module to be installed.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : None.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : = lfvIns()
*!*************************************************************
*!
FUNCTION lfvIns

PRIVATE laTempArr
IF EMPTY(laData[14])
  ** If this company is a parent compnay. **
  SELECT "  "+cApp_Id+" "+cApp_name+" +"+IIF(lSetReq,"T","F") ;
         FROM (gcSysHome+"SYDAPPL") ;
         WHERE cApp_Id <> "SM" .AND. ;
               cApp_Id <> "SY" ;
         INTO ARRAY laAllMod
ELSE
  ** If this company is a child company. **
  SELECT "  "+cApp_Id+" "+cApp_name+" +"+IIF(lSetReq,"T","F") ;
         FROM (gcSysHome+"SYDAPPL") ;
         WHERE cApp_Id <> "SM" .AND. ;
               cApp_Id <> "SY" .AND. ;
               !lparntmdl ;
         INTO ARRAY laAllMod
ENDIF
=lfSelctdMd()
DIMENSION laTempArr[ALEN(laAllMod,1),4]
FOR lnC=1 TO ALEN(laAllMod,1)
  laTempArr[lnC,2] = laAllMod[lnC]
  laTempArr[lnC,3] = SUBSTR(laAllMod[lnC],3,2)
  laTempArr[lnC,4] = lnC
ENDFOR
FOR lnC=1 TO ALEN(laAllMod,1)
  IF AT(lcDiamond,laAllMod[lnC]) = 0
    laTempArr[lnC,1] = 0
  ENDIF
ENDFOR
FOR lnCnt = 1 TO ALEN(laTempArr,1)
  IF AT(lcDiamond,laTempArr[lnCnt,2]) = 0
    LOOP
  ENDIF
  lnCount2 = laTempArr[lnCnt,4]
  lcCurModul = SUBSTR(laAllMod[lnCount2],3,2)
  lcCurLogNm = SUBSTR(ALLTRIM(LEFT(laAllMod[lnCount2],;
                      LEN(laAllMod[lnCount2])-3)),6,30)
  ** Do you want to install files for module 'laAllMod[lnCount2]'? **
  ** <  Yes  > - <  No  > **
 *IF gfModalGen("QRM00179B00006","DIALOG",lcCurLogNm) = 1
    ** Function to install files for current module. **
    IF lfInstall(lcCurModul)
      IF AT(lcCurModul,lcOldMod) = 0
        lcOldMod   = lcOldMod + IIF(EMPTY(lcOldMod),'','|')+lcCurModul
      ENDIF
      laData[15] = laData[15] + IIF(EMPTY(laData[15]),'','|')+lcCurModul
      SELECT SYCCOMP
      REPLACE mComp_mdl WITH laData[15]
      laAllMod[lnCount2] = " "+SUBSTR(laAllMod[lnCount2],2,36)+;
                           "Installed    "+RIGHT(laAllMod[lnCount2],1)
            
      SELECT SYCCOMP
      IF !(lcCurModul $ mModlSet)
        REPLACE mModlSet  WITH mModlSet+IIF(EMPTY(mModlSet),'',',')+lcCurModul
      ENDIF  
      IF laData[1] = gcAct_Comp
        gcCmpModules = ALLTRIM(mModlSet)
      ENDIF
    ELSE
      ** Installing files for module [laAllMod[lnCount2] **
      ** not done successfully...
      ** <  Ok  > **
      =gfModalGen("TRM00180B00000","DIALOG",lcCurLogNm)
      laAllMod[lnCount2] = " " + SUBSTR(laAllMod[lnCount2],2)
    ENDIF
  *ENDIF
ENDFOR

*!*************************************************************
*! Name      : lfInstall  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Function to Install Module.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : lfver_upd
*!                        : lfCreatFil
*!                        : lfCoparStr
*!                        : lfGetFC
*!*************************************************************
*! Passed Parameters  : Module to install (lcMod2Ins)
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : = lfInstall()
*!*************************************************************
*!
FUNCTION lfInstall
PARAMETERS lcMod2Ins

lcUpGrdLvl = IIF(lcMod2Ins='SY','S','A')
** Get all the files related to this module. **
SELECT SYDFILES
DECLARE laMdlFiles[1,3]
laMdlFiles = " "
_TALLY     = 0
SELECT CFILE_NAM,CFILE_TTL,MFILE_APP ;
       FROM (gcSysHome+"SYDFILES") ;
       WHERE lcMod2Ins $ (mfile_app) .AND. ;
             LEFT(CFILE_NAM,2) <> "SY" AND CUPGRDLVL=lcUpGrdLvl;
       INTO ARRAY laMdlFiles       

lnRecords = _TALLY

IF _TALLY = 0
  ** There is no files to be installed for this module. **
  ** <  Ok  > **
  =gfModalGen("TRM00186B00000","DIALOG")
  RETURN .F.
ENDIF

DECLARE laFil2Upd[1]
laFil2Upd = " "

llYes2All = .T.
** Loop in the files related to the module **
** to be installed...
FOR lnCount5 = 1 TO ALEN(laMdlFiles,1)
  lcFileNam  = UPPER(ALLTRIM(laMdlFiles[lnCount5,1]))
  lcPathFile = ALLTRIM(laData[11])+lcFileNam
  =gfThermo(lnRecords,lnCount5,"Creating: ",laMdlFiles[lnCount5,2])
  lcTargDir = ALLTRIM(laData[11])
  lnSelected = lnCount5
  =lfver_upd(lcFileNam,.T.)
  LOOP
  ** Select the file structure in Array **
  DECLARE laFileStrc[1,5]
  laFileStrc = " "
  SELECT sydflfld.cfld_name,sydfield.cdata_typ, ;
         sydfield.nfld_wdth,sydfield.nfld_dec, ;
         sydflfld.nfld_pos ;
   FROM  (gcSyshome+"sydflfld"),(gcSyshome+"sydfield") ;
         ORDER BY sydflfld.nfld_pos ;
         GROUP BY sydField.cFld_Name ;
   WHERE UPPER(sydflfld.cfile_nam) = PADR(lcFileNam,8) .AND. ;
         sydfield.cfld_name = sydflfld.cfld_name ;
         INTO ARRAY laFileStrc
  =gfAdel(@laFileStrc,5,2)  && An additional colum holding the position 
                            && of each field has to be removed from the
                            && Array befor verification or building.
  
  ** If function called from add mode override the files. **
  ** If function called from edit mode check first. **
  IF llSavMod 
    ** Overwrite the file or create. **
    =lfCreatFil()
  ELSE
    IF !FILE(lcPathFile+".DBF")
      ** Create the file. **
      =lfCreatFil()
    ELSE
      IF AT(",",laMdlFiles[lnCount5,3]) = 0
        ** If not involved in any other module. overwrite it. **
        =lfCreatFil()
      ELSE
        ** If the file is involved in more than one modul **
        ** update its strucure not to overwrite it...
        =lfCoparStr()
      ENDIF
    ENDIF
  ENDIF
ENDFOR
=lfGetFC(lcMod2Ins,ALLTRIM(laData[11]),'A')
IF lcMod2Ins = "IC"
  lcItemFil = gcSysHome + "SYWSCOD"
ENDIF

** If general ledger modul, copy this files **
** to the same path from sysfiles dir. :_
** Cash flow items.  "GLCFITEM"
** Ratio groups.     "GLRACOD"
** Source journals.  "GLSUBJOR"
IF lcMod2Ins = "GL"
  lcItemFil = gcSysHome + "GLCFITEM"
  IF FILE(lcItemFil+".DBF") AND FILE(lcItemFil+".CDX") AND FILE(lcItemFil+".FPT")
    ** Copy Cash flow items. **
    COPY FILE (lcItemFil+".DBF") TO (ALLTRIM(laData[11])+"GLCFITEM.DBF")
    COPY FILE (lcItemFil+".CDX") TO (ALLTRIM(laData[11])+"GLCFITEM.CDX")
    COPY FILE (lcItemFil+".FPT") TO (ALLTRIM(laData[11])+"GLCFITEM.FPT")
  ENDIF
  lcRacFil = gcSysHome + "GLRACOD"
  IF FILE(lcRacFil+".DBF") AND FILE(lcRacFil+".CDX") AND FILE(lcRacFil+".FPT")
    ** Copy Ratio groups. **
    COPY FILE (lcRacFil+".DBF") TO (ALLTRIM(laData[11])+"GLRACOD.DBF")
    COPY FILE (lcRacFil+".CDX") TO (ALLTRIM(laData[11])+"GLRACOD.CDX")
    COPY FILE (lcRacFil+".FPT") TO (ALLTRIM(laData[11])+"GLRACOD.FPT")
  ENDIF
  lcSubFil = gcSysHome + "GLSUBJOR"
  IF FILE(lcSubFil+".DBF") AND FILE(lcSubFil+".CDX") AND FILE(lcSubFil+".FPT")
    ** Copy Source journals. **
    COPY FILE (lcSubFil+".DBF") TO (ALLTRIM(laData[11])+"GLSUBJOR.DBF")
    COPY FILE (lcSubFil+".CDX") TO (ALLTRIM(laData[11])+"GLSUBJOR.CDX")
    COPY FILE (lcSubFil+".FPT") TO (ALLTRIM(laData[11])+"GLSUBJOR.FPT")
  ENDIF
ENDIF

IF !EMPTY(laFil2Upd[1])
  DO (gcScrDir + gcWinAppl + '\SMDSPFL.SPR')
ENDIF

*!*************************************************************
*! Name      : lfGetFC  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Copy or remove the form codes from the DBFS form 
*!             code tables based on the form codes files in the 
*!             sysfiles directory.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : lfver_upd
*!                        : lfCreatFil
*!                        : lfCoparStr
*!                        : lfGetFC
*!*************************************************************
*! Passed Parameters : lcApp      Application Name
*!                     lnArrNum   Application number in the array
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example     : = lfGetFC("AR","W:\aria27\dbfs\",'A')
*!*************************************************************
*!
FUNCTION lfGetFC
PARAMETERS lcMod2Ins,lcDataDr,lcAddDel

IF FILE(lcDataDr+'FORMCDHD.DBF') .AND. ;
      FILE(lcDataDr+'FORMCDHD.DBF') 
  lnAlias = SELECT(0)
  lcFCDHD = gfTempName()
  lcFCDDT = gfTempName()
  USE (lcDataDr+'FORMCDHD.DBF') AGAIN ALIAS (lcFCDHD) ORDER TAG Formcdhd IN 0
  USE (lcDataDr+'FORMCDDT.DBF') AGAIN ALIAS (lcFCDDT) ORDER TAG Formcddt IN 0

  llOpSYREP = gfOpenFile(gcSysHome+'SYDREPRT','CREP_ID','SH')
  llOpFDT   = gfOpenFile(gcSysHome+'SYFRMCDD','FORMCDDT','SH')
  llOpFHD   = gfOpenFile(gcSysHome+'SYFRMCDH','FORMCDHD','SH')

  IF lcAddDel = 'A'      
    SELECT SYFRMCDD
    SET RELATION TO SYFRMCDD.cRep_Id INTO SYDREPRT ADDITIVE      
    SELECT SYFRMCDH
    SET RELATION TO SYFRMCDH.cformmaj INTO SYFrmCdD ADDITIVE      
    SCAN 
      llRecIns = .F.
      SELECT SYFRMCDD
      *E038033,1 HMA 04/03/2004 Select only Records belong to ARIA27 [BEGIN]
      
      *SCAN WHILE cformmaj = SYFRMCDH.cformmaj FOR lcMod2Ins $ UPPER(SYDREPRT.mCallMods)
      SCAN WHILE cformmaj = SYFRMCDH.cformmaj FOR lcMod2Ins $ UPPER(SYDREPRT.mCallMods) .AND. SYDREPRT.cVer<>"A40"
      
      *E038033,1 HMA  [END]
        SCATTER MEMVAR MEMO
        IF !SEEK(m.cformmaj+m.cformcode,lcFCDDT)
          INSERT INTO (lcFCDDT) FROM MEMVAR
        ELSE
          SELECT (lcFCDDT) 
          GATHER MEMVAR MEMO   
        ENDIF
        SELECT SYFRMCDD
        llRecIns = .T.
      ENDSCAN
      IF llRecIns 
        SELECT SYFRMCDH
        SCATTER MEMVAR MEMO
        IF !SEEK(m.cformmaj,lcFCDHD)
          INSERT INTO (lcFCDHD) FROM MEMVAR
        ELSE
          SELECT (lcFCDHD)
          GATHER MEMVAR MEMO FIELDS EXCEPT cCurForm, mFormSets
        ENDIF
      ENDIF  
      SELECT SYFRMCDH
    ENDSCAN
  ELSE
    SELECT (lcFCDDT)
    SET RELATION TO cRep_ID INTO SYDREPRT ADDITIVE      
    SELECT (lcFCDHD)
    SET RELATION TO cformmaj INTO (lcFCDDT) ADDITIVE      
    SET SKIP TO (lcFCDDT),SYDREPRT

    *E038033,1 HMA 04/03/2004 Select only Records belong to ARIA27 [BEGIN]

    *SCAN FOR lcMod2Ins $ UPPER(SYDREPRT.mCallMods)
    SCAN FOR lcMod2Ins $ UPPER(SYDREPRT.mCallMods) .AND. SYDREPRT.cVer<>"A40"
    
    *E038033,1 HMA  [END]
      llDelMod = .T.
      lcCalMod = STRTRAN(SYDREPRT.mCallMods,lcMod2Ins)
      lcCalMod = STRTRAN(lcCalMod,',')
      IF !EMPTY(lcCalMod)
        FOR lnI = 1 TO LEN(lcCalMod)/2
          IF SUBSTR(lcCalMod,1,2) $ lcOldMod
            llDelMod = .F.
            EXIT
          ELSE
            lcCalMod = SUBSTR(lcCalMod,3)
          ENDIF
        ENDFOR
      ENDIF
      IF llDelMod
        SELECT (lcFCDDT)
        DELETE
      ENDIF
    ENDSCAN
    DELETE FOR EOF(lcFCDDT)      
  ENDIF      
  IF llOpSYREP .AND. USED('SYDREPRT')
    USE IN SYDREPRT
  ENDIF
  USE IN (lcFcdHd)
  USE IN (lcFcdDt)
  IF llOpFHD .AND. USED('SYFRMCDH')
    USE IN SYFRMCDH
  ELSE 
    SELECT SYFRMCDH
    SET RELATION TO
  ENDIF
  IF llOpFDT .AND. USED('SYFRMCDD')
    USE IN SYFRMCDD
  ELSE
    SELECT SYFRMCDD
    SET RELATION TO
  ENDIF
  SELECT (lnAlias)
ENDIF

*!*************************************************************
*! Name      : lfCretComp  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Function to create company.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : lfInstall
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example     : = lfCretComp()
*!*************************************************************
*!
FUNCTION lfCretComp
PARAMETER llDontInst
PRIVATE lnOldAls

lnOldAls = SELECT(0)
=gfOpenFile(gcSysHome+"SYCCOMP",'CCOMP_ID','SH')
SELECT SYWCOMP
GO TOP
SCATTER MEMVAR MEMO
DELETE ALL
laData[11] = gcAllCmp+IIF(RIGHT(ALLTRIM(gcAllCmp),1)='\','','\')+;
                            UPPER(ALLTRIM(laData[1]))+'\'
laData[11] = gfGetDataDir(ALLTRIM(laData[11]))
gcDataDir  = laData[11]
m.cCom_Ddir = gcDataDir
IF llDontInst
  SELECT SYCCOMP
  APPEND BLANK
  GATHER FROM laData FIELDS &lcScFields MEMO  
ELSE
  INSERT INTO SYCCOMP FROM MEMVAR
ENDIF
USE IN SYCCOMP
USE IN SYWCOMP
=gfOpenFile(gcSysHome+"SYCCOMP",'CCOMP_ID','SH')
** See if this company or not in the company file **
** for defining the menu popup comany bar...
SELECT SYCCOMP
=SEEK(laData[1])
lnComRec = IIF(RECNO()>RECCOUNT(),0,RECNO())
GO TOP
IF EOF()
  llFirstComp = .T.
ELSE
  llFirstComp = .F.
ENDIF
IF lnComRec <> 0
  GOTO lnComRec
ENDIF  
=gfAdd_Info()

lcSavDef = FULLPATH(SET("DEFAULT"))
lcDbfPath = IIF(RIGHT(ALLTRIM(laData[11]),1)='\',;
              SUBSTR(ALLTRIM(laData[11]) , 1 , LEN(ALLTRIM(laData[11])) - 3),;
              SUBSTR(ALLTRIM(laData[11]) , 1 , LEN(ALLTRIM(laData[11])) - 2))
IF !gfValdPath(lcDbfPath)
  lcDbfPath = IIF(RIGHT(ALLTRIM(lcDbfPath),1)='\',;
              SUBSTR(ALLTRIM(lcDbfPath),1,LEN(ALLTRIM(lcDbfPath))-1),;
              ALLTRIM(lcDbfPath))
  =gfMkDir(lcDbfPath)
ENDIF
** If the path is not found, create it. **
IF !gfValdPath(laData[11])
  lcNewPath =IIF(RIGHT(ALLTRIM(laData[11]),1)='\',;
                 SUBSTR(ALLTRIM(laData[11]),1,;
                 LEN(ALLTRIM(laData[11]))-1),;
                 ALLTRIM(laData[11]))
  =gfMkDir(lcNewPath)
ENDIF
lcAliasSel = SELECT(0)
lcDymmy = lfInstall('SY')   
SELECT (lcAliasSel)  
SET DEFAULT TO &lcSavDef
** Add this company title in the menu **
** popup of the companies...
IF llFirstComp
  lnBarNo = CNTBAR('_COMPANIES')
ELSE
  lnBarNo = CNTBAR('_COMPANIES')+1
ENDIF
DEFINE BAR lnBarNo OF _COMPANIES PROMPT laData[1]+'-'+ALLTRIM(laData[2])
lcComp_Id = '"'+laData[1]+'"'
ON SELECTION BAR lnBarNo OF _COMPANIES ;
            DO gpChangCom WITH &lcComp_Id
glCmpCreat = .T.
SELECT SYCCOMP
IF lnComRec <> 0
  GOTO lnComRec
ENDIF  
SELECT(lnOldAls)


*!*************************************************************
*! Name      : lfCheckRes  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Function Check if resume previous installation process.
*!*************************************************************
*! Calls     : Procedures : SMCMWZ
*!             Functions  : lfRestDBF
*!                        : lfEndSesin
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example     : = lfCheckRes()
*!*************************************************************
*!
FUNCTION lfCheckRes

SELECT SYWSEQNC
SET ORDER TO Runfrm
IF SEEK('1')
  llSkip   = .F.
  lcTempKey = cScrSeq
  SELECT SYWSEQNC
  lnScrNo = nPageNo
  *IF ALLTRIM(cScrNam) <> "SMCMWZ"
  DO gcAppHome+"SM\SMCMWZ.FXP" WITH .T.
  *ENDIF
  lcOldMod   = laData[15]
  lnChoice = gfModalGen("QRM00377B00041","DIALOG",laData[1]+" - "+ALLTRIM(laData[2]))
  DO CASE
    CASE lnChoice = 1
      lcOldMod   = laData[15]
      lcNextActn = 'N'
      SELECT SYWSEQNC
      lnScrNo = nPageNo
      SET ORDER TO Csequanc
      SET FILTER TO (EMPTY(ALLTRIM(lcFltExp)) .OR. EVAL(ALLTRIM(lcFltExp))) .AND. lInstall
      llSkip = .F.
      IF ALLTRIM(cScrNam) = "SMCMWZ"
        lcNextActn = ""
      ENDIF
      =SEEK(lcTempKey)
    CASE lnChoice = 2
      =lfEndSesin("U")  && Call Resume Later Function.
      RETURN (.F.)
    CASE lnChoice = 3
      =lfRestDBF()      && Function to cancel sessin
      SELECT SYWCOMP
      GO TOP
      BLANK
      DELETE          
      lcNextActn = ''
      llSkip = .T.
      SELECT SYWSEQNC
      SET ORDER TO Csequanc
      GO TOP    
  ENDCASE
  SELECT SYWSEQNC
ELSE
  SET ORDER TO Csequanc
  GO TOP    
ENDIF

*--- PART 1 : Code for select module screens and Functions
*!*************************************************************
*! Name      : lfSelModul  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Function Select modules to install.
*!*************************************************************
*! Calls     : Procedures : SMCMWZ
*!             Functions  : lfMod2Inst.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfSelModul()
*!*************************************************************
*!
FUNCTION lfSelModul

PRIVATE lnOldAls
lnOldAls = SELECT(0)
lcTSelect   = "S\<elect"    && Hold the word "S\<elect"
lcTUnSelct  = "\<Unselect"  && Hold the word "\<Unselect"
lcDiamond   = ""
lcPrompt    = lcTSelect
lcPrompt  = IIF(LEFT(laAllMod[1,1],1) = " ",lcTSelect,lcTUnSelct)
=lfMod2Inst()
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfMod2Inst  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Function runc screen to select modules from.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfMod2Inst()
*!*************************************************************
*!
FUNCTION lfMod2Inst

** Collect all the available modules in the system. **
IF EMPTY(laAllMod)
  IF EMPTY(laData[14])
    ** If this company is a parent compnay. **
    SELECT "  "+cApp_Id+" "+cApp_name+" +"+IIF(lSetReq,"T","F") ;
           FROM (gcSysHome+"SYDAPPL") ;
           WHERE cApp_Id <> "SM" .AND. ;
                 cApp_Id <> "SY" ;
           INTO ARRAY laAllMod
  ELSE
    ** If this company is a child company. **
    SELECT "  "+cApp_Id+" "+cApp_name+" +"+IIF(lSetReq,"T","F") ;
           FROM (gcSysHome+"SYDAPPL") ;
           WHERE cApp_Id <> "SM" .AND. ;
                 cApp_Id <> "SY" .AND. ;
                 !lparntmdl ;
           INTO ARRAY laAllMod
  ENDIF
ENDIF
=lfSortMod()
IF _TALLY > 0
  ** Call the installtion and setup modules screen. **
  llSavMod  = .T.
  lcPrompt  = lcTSelect
  =lfSelctdMd()
  DO (gcScrDir + gcWinAppl + '\Smwzset.SPR')
ELSE
  ** There is no modules to install its files. **
  ** <  Ok  > **
  =gfModalGen("TRM00178B00000","DIALOG")
ENDIF

*!*************************************************************
*! Name      : lfSortMod  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Function Sort modules array.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfSortMod()
*!*************************************************************
*!
FUNCTION lfSortMod

IF SEEK("CM","SYDAPPL")
  lccmProm = "  "+sydappl.cApp_Id+" "+sydappl.cApp_name+" +"+IIF(sydappl.lSetReq,"T","F")
  lncmPos = ASCAN(laAllMod,lccmProm)
  IF lncmPos >0
    =ADEL(laAllMod,lncmPos)
    =AINS(laAllMod,1)
    laAllMod[1] = lccmProm
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvModules  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Valid function for the list that hold all the modules
*!             linked to this company in the SMINSET screen.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : lfSRelScr
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfvModules()
*!*************************************************************
*!
FUNCTION lfvModules
PRIVATE lnOldAls , lcNeedMod

IF lsModules = 0 .OR. lsModules > ALEN(laAllMod,1)
  RETURN
ENDIF
lnOldAls = SELECT(0)
IF LEFT(laAllMod[lsModules],1) = " "
  laAllMod[lsModules] = lcDiamond + SUBSTR(laAllMod[lsModules],2)
  *--- ALLTRIM(SUBSTR(laAllMod[lsModules],2))
  IF ALLTRIM(SUBSTR(laAllMod[lsModules],3,2)) = 'IC'
    =lfSRelScr("LFSSTYCOD",.T.)
  ENDIF
  IF ALLTRIM(SUBSTR(laAllMod[lsModules],3,2)) = 'GL'
    =lfSRelScr("SMWACOD",.T.)
  ENDIF
  IF ALLTRIM(SUBSTR(laAllMod[lsModules],3,2)) $ 'AR|SO|PS'
    =lfSRelScr("SMWCUST",.T.)
  ENDIF
ELSE
  laAllMod[lsModules] = " " + SUBSTR(laAllMod[lsModules],2)
  IF ALLTRIM(SUBSTR(laAllMod[lsModules],3,2)) = 'IC'
    =lfSRelScr("LFSSTYCOD",.F.)
  ENDIF
  IF ALLTRIM(SUBSTR(laAllMod[lsModules],3,2)) = 'GL'
    =lfSRelScr("SMWACOD",.F.)
  ENDIF
  IF ALLTRIM(SUBSTR(laAllMod[lsModules],3,2)) $ 'AR|SO|PS'
    =lfSRelScr("SMWCUST",.F.)
  ENDIF
ENDIF
_CUROBJ = _CUROBJ
SHOW GET lsModules
SELECT(lnOldAls)
=lfStatus()

*!*************************************************************
*! Name      : lfStatus  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Funciton to change the prompet of select button.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : lfSRelScr
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfStatus()
*!*************************************************************
*!
FUNCTION lfStatus

IF LEFT(laAllMod[lsModules],1) = " "
  lcPrompt = lcTSelect
  SHOW GET pbSel,1 PROMPT lcTSelect
ELSE
  lcPrompt = lcTUnSelct
  SHOW GET pbSel,1 PROMPT lcTUnSelct
ENDIF

*!*************************************************************
*! Name      : lfvAll  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Funciton Select all modules.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : lfSRelScr
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfvAll()
*!*************************************************************
*!
FUNCTION lfvAll

FOR lnCont1 = 1 TO ALEN(laAllMod,1)
  IF LEFT(laAllMod[lnCont1],1) = " "
    laAllMod[lnCont1] = lcDiamond + SUBSTR(laAllMod[lnCont1],2)
  ENDIF
ENDFOR
=lfSRelScr("LFSSTYCOD",.T.)
=lfSRelScr("SMWACOD",.T.)
=lfSRelScr("SMWCUST",.T.)
SHOW GET lsModules
=lfStatus()

*!*************************************************************
*! Name      : lfvNon  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Funciton Select no modules.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : lfSRelScr
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfvNon()
*!*************************************************************
*!
FUNCTION lfvNon

FOR lnCont1 = 1 TO ALEN(laAllMod,1)
  IF LEFT(laAllMod[lnCont1],1) = lcDiamond
    laAllMod[lnCont1] = " " + SUBSTR(laAllMod[lnCont1],2)
  ENDIF
ENDFOR
=lfSRelScr("LFSSTYCOD",.F.)
=lfSRelScr("SMWACOD",.F.)
=lfSRelScr("SMWCUST",.F.)
SHOW GET lsModules

=lfStatus()

*!*************************************************************
*! Name      : lfvInv  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Funciton invert Selecttion.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : lfSRelScr
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfvInv()
*!*************************************************************
*!
FUNCTION lfvInv

FOR lnCont1 = 1 TO ALEN(laAllMod,1)
  IF LEFT(laAllMod[lnCont1],1) = " "
    laAllMod[lnCont1] = lcDiamond + SUBSTR(laAllMod[lnCont1],2)
    IF ALLTRIM(SUBSTR(laAllMod[lsModules],2)) = 'IC'
      =lfSRelScr("LFSSTYCOD",.T.)
    ENDIF
    IF ALLTRIM(SUBSTR(laAllMod[lsModules],3,2)) = 'GL'
      =lfSRelScr("SMWACOD",.T.)
    ENDIF
    IF ALLTRIM(SUBSTR(laAllMod[lsModules],3,2)) $ 'AR|SO|PS'
      =lfSRelScr("SMWCUST",.T.)
    ENDIF
  ELSE
    laAllMod[lnCont1] = " " + SUBSTR(laAllMod[lnCont1],2)
    IF ALLTRIM(SUBSTR(laAllMod[lsModules],2)) = 'IC'
      =lfSRelScr("LFSSTYCOD",.F.)
    ENDIF
    IF ALLTRIM(SUBSTR(laAllMod[lsModules],3,2)) = 'GL'
      =lfSRelScr("SMWACOD",.F.)
    ENDIF
    IF ALLTRIM(SUBSTR(laAllMod[lsModules],3,2)) $ 'AR|SO|PS'
      =lfSRelScr("SMWCUST",.F.)
    ENDIF
  ENDIF
ENDFOR
SHOW GET lsModules
=lfStatus()

*!*************************************************************
*! Name      : lfvNext  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Valid Funciton for next button from select module screnn.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : lfSRelScr
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfvNext()
*!*************************************************************
*!
FUNCTION lfvNext

PRIVATE lnOldAls , lcOldRec , llSelect
llSelect = .F.
FOR lnAMInd = 1 TO ALEN(laAllMod,1)
  IF !llSelect
    llSelect = LEFT(laAllMod[lnAMInd],1) = lcDiamond
  ENDIF
ENDFOR
IF !llSelect
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'No modules selected , Cannot proceed.')
ELSE
  lnOldAls = SELECT(0)
  SELECT SYWSEQNC
  SET FILTER TO
  lcOldRec = cScrSeq
  *--- cscrnam+cparam1
  SET ORDER TO Cscrnam
  FOR lnIndex = 1 TO ALEN(laAllMod)
    SELECT SYWSEQNC
    =SEEK(PADR("SYCMWZ",10)+SUBSTR(laAllMod[lnIndex,1],3,2),'SYWSEQNC')
    lcNeedMod = lfGetReq(SUBSTR(laAllMod[lnIndex,1],3,2))
    IF EMPTY(lcNeedMod) .OR. (SUBSTR(laAllMod[lnIndex,1],1,1) <> "") .OR.lfCheckReq(lcNeedMod)
      REPLACE lInstall WITH SUBSTR(laAllMod[lnIndex,1],1,1) = ""
      IF lInstall
        SELECT SYWCOMP
        lcModule = SUBSTR(laAllMod[lnIndex,1],3,2)
        lcTempModul = mModlSet
        IF !(lcModule $ lcTempModul)
          REPLACE mModlSet  WITH mModlSet+IIF(EMPTY(mModlSet),'',',')+lcModule
          lcTempModul = mModlSet
        ENDIF  
        lcCompTMod  = mComp_Mdl
        IF !(lcModule $ lcCompTMod)
          REPLACE mComp_Mdl  WITH mComp_Mdl+IIF(EMPTY(mComp_Mdl),'',',')+lcModule
          lcCompTMod  = mComp_Mdl
        ENDIF  
      ENDIF
    ELSE
      RETURN
    ENDIF
  ENDFOR
  SELECT SYWSEQNC
  SET FILTER TO (EMPTY(ALLTRIM(lcFltExp)) .OR. EVAL(ALLTRIM(lcFltExp))) .AND. lInstall
  SET ORDER TO Csequanc
  =SEEK(lcOldRec)
  lcNextActn = 'N'
  CLEAR READ  
ENDIF

*!*************************************************************
*! Name      : lfSelctdMd  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Funciton to get the previously selected modules.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfSelctdMd()
*!*************************************************************
*!
FUNCTION lfSelctdMd

PRIVATE lnOldAls

lnOldAls = SELECT(0)
SELECT SYWSEQNC
SET FILTER TO
lcOldRec = cScrSeq
*--- cscrnam+cparam1
SET ORDER TO Cscrnam
SELECT SYWSEQNC
FOR lnIndex = 1 TO ALEN(laAllMod,1)
  IF SEEK(PADR("SYCMWZ",10)+SUBSTR(laAllMod[lnIndex,1],3,2),'SYWSEQNC');
     .AND. SYWSEQNC.lInstall
    IF LEFT(laAllMod[lnIndex],1) = " "
      laAllMod[lnIndex] = lcDiamond + SUBSTR(laAllMod[lnIndex],2)
    ENDIF
  ENDIF
ENDFOR
SELECT SYWSEQNC
SET FILTER TO (EMPTY(ALLTRIM(lcFltExp)) .OR. EVAL(ALLTRIM(lcFltExp))) .AND. lInstall
SET ORDER TO Csequanc
=SEEK(lcOldRec)
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfSRelScr  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Funciton to select the related screens to aspecific module.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfSRelScr()
*!*************************************************************
*!
FUNCTION lfSRelScr
PARAMETER lcScrName , llStats

SELECT SYWSEQNC
lcTempKey = EVAL(KEY())
SET FILTER TO
LOCATE FOR ALLTRIM(cScrNam) = lcScrName
REPLACE lInstall WITH llStats
SET FILTER TO (EMPTY(ALLTRIM(lcFltExp)) .OR. EVAL(ALLTRIM(lcFltExp))) .AND. lInstall
=SEEK(lcTempKey)
**---Part 1  End Of Select Modules Part

*--- Part 2 : Codes For Style codes stracture part**--------
*!*************************************************************
*! Name      : lfSStyCod  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Funciton to select style code structure.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfSStyCod()
*!*************************************************************
*!
FUNCTION lfSStyCod

STORE "" TO lcLong1 , lcLong2 , lcLong3 , lcSSca, lcHeader
=gfOpenFile(gcsyshome+"SYWSCOD","Segno","SH")
llMScale   = gfGetMemVar('M_USEEXSSC')
SELECT SYWSCOD
SELECT ciseghead,cItemrecty FROM SYWSCOD INTO ARRAY laStyStr ;
       WHERE !EMPTY(cExtSiz) .AND. IIF(llMScale,cExtSiz='Y',cExtSiz='N')
LOCATE FOR lSelect
IF !FOUND() .OR. cItemrecty = 'A'
  lnStyCod = 1
ELSE
  FOR lnInde = 1 TO ALEN(laStyStr,1)
    IF laStyStr[lnInde,1] = ciseghead
      lnStyCod = lnInde
    ENDIF
  ENDFOR
ENDIF
lcSStyle = "Style"
lcSColor = "Color"
=lfvStyCod()
DO (gcScrDir + gcWinAppl + '\SmSTYCOD.SPR')
USE IN SYWSCOD

*!*************************************************************
*! Name      : lfvStyCod  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Valid Funciton for style code popup.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfvStyCod()
*!*************************************************************
*!
FUNCTION lfvStyCod

SELECT SYWSCOD
FOR lnInde = 1 TO 3
  =SEEK(laStyStr[lnStyCod,2]+STR(lnInde,1))
  laStyDesc[lnInde,1] = cIsegsDes
  laStyDesc[lnInde,2] = cisegldes
  lcHeader = IIF(lnInde=1,cIsegHead,lcHeader)
ENDFOR
SHOW GET laStyDesc[1,1]  DISABLE
SHOW GET laStyDesc[1,2]  DISABLE
SHOW GET laStyDesc[2,1]  DISABLE
SHOW GET laStyDesc[2,2]  DISABLE
SHOW GET laStyDesc[3,1]  DISABLE
SHOW GET laStyDesc[3,2]  DISABLE
SHOW GET lcHeader DISABLE

*!*************************************************************
*! Name      : lfvsPre  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Style code screen previous button.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfvsPre()
*!*************************************************************
*!
FUNCTION lfvsPre

lcNextActn = 'B'
=lfUStyCod()

*!*************************************************************
*! Name      : lfvSnext  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Style code screen Next button.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfvSnext()
*!*************************************************************
*!
FUNCTION lfvSnext

lcNextActn = 'N'
=lfUStyCod()

*!*************************************************************
*! Name      : lfvsResu  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Style code screen resume Later button.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfvsResu()
*!*************************************************************
*!
FUNCTION lfvsResu

lcNextActn = 'U'
=lfUStyCod()

*!*************************************************************
*! Name      : lfUStyCod  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Function to update style code structure.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfUStyCod()
*!*************************************************************
*!
FUNCTION lfUStyCod

SELECT SYWSCOD
REPLACE ALL lSelect WITH .F.
=SEEK(laStyStr[lnStyCod,2]+"1")
REPLACE lSelect WITH .T.

*--- Part 2 : End style code structure.


*--- Part 4  : Global functions need to be local to void some unnessesary validations.
*!*************************************************************
*! Name      : gfGetMemVar
*! Developer : Hesham El-Sheltawi
*! Date      : 10/05/95
*! Purpose   : Return Variable(s) for company settings from SYCSETUP
*!*************************************************************
*! Parameters: lcArray   && variable to restore
*!                       && OR one dimension array to restore the variable(s)
*!                       name with the same variable name
*!                       && OR two dimension array to restore the variable(2)
*!                       in column 1 into variable names in column 2
*!             lcCompID  &&company id to get its settings
*!*************************************************************
*! Called by : 
*!*************************************************************
*! Returns            : VALUE OF VARIABLE OR no of variables restored
*!*************************************************************
*! Example   : lcVarName=gfGetMemVar('LLMULCURR ','01')
*!             WILL return from the sycsetup file the setting
*!             value for company 01 the variable called "LLMULCURR "
*!*************************************************************
*
FUNCTION gfGetMemVar
PARAMETERS lcArray,lcCompID

PRIVATE lnAliasNo,llCUsedBy,llArrayORvar,llTwoDimen,lcSetupTag,;
        lcConfgTag,lnRetCount,lcOnErr,llError,laVarArr,llUseSycC
PRIVATE  lcSetPath, llReUsedBy, lnCurTag, lnCurRec, llCompUsed
lnAliasNo=SELECT()
llUsedBy  = .F.
llCUsedBy = .F.
llSUsedBy = .F.
llUseSycC = .F.
lcCompID  = IIF(TYPE('lcCompId')<>'C' OR EMPTY(lcCompID),gcAct_Comp,lcCompID)
llReUsedBy = .F.
llArrayORvar = TYPE('lcArray[1]')='C'
llTwoDimen = IIF(llArrayORvar AND ALEN(lcArray,2)=2,IIF(TYPE('lcArray[1,2]')='L' OR TYPE(lcArray[1,1])='U','A','N'),'V' )
IF !llArrayORvar AND ',' $ lcArray
   DIMENSION laVarArr[1]
   =gfSubStr(lcArray,@laVarArr)
   DIMENSION lcArray[ALEN(laVarArr)]
   =ACOPY(laVarArr,lcArray)
   llArrayORvar = .T.
   llTwoDimen = 'V'
ENDIF
IF lcArray $ "LLHIST | M_Comp_Id"
  lcCompDir  = IIF(SEEK(lcCompID, 'SYCCOMP'), gfGetDataDir(ALLTRIM(SYCCOMP.cCom_DDir)), gcDataDir)
  IF USED("SETUPS")
    USE IN SETUPS
  ENDIF
  USE (lcCompDir+'SETUPS') IN 0
  SELECT SETUPS
  SET ORDER TO TAG VARNAME
ELSE
  IF !USED('SYWSETP')
    SELECT 0
    USE (gcSysHome+'SYWSETP') AGAIN
    llSUsedBy = .T.
  ELSE
    SELECT SYWSETP
  ENDIF
ENDIF
lcSetupTag =TAG()
SET ORDER TO TAG VARNAME
lcRetVal=''
lnRetCount=0
lcOnErr=ON('ERROR')
ON ERROR llError = .T.
IF !llArrayORvar
  IF SEEK(PADR(UPPER(lcArray),10))
    DO CASE
      CASE cDefa_Typ='V'
        lcRetVal = lfTrnsStr(STRTRAN(mData_Def,CHR(13)+CHR(10),''),cData_Typ)
      CASE cDefa_Typ='E'
       lcRetVal=EVAL(STRTRAN(mData_Def,CHR(13)+CHR(10),''))
    ENDCASE
  ELSE
    llUseSycC = .T.
    IF lcArray $ "LLHIST | M_Comp_Id"
      IF !USED('SYCCONFG')
        SELECT 0
        USE (gcSysHome+'SYCCONFG')
        llCUsedBy = .T.
      ELSE
        SELECT SYCCONFG
      ENDIF
      lcConfgTag=TAG()
      SET ORDER TO TAG VARNAME
    ELSE
      IF !USED('SYWCONFG')
        SELECT 0
        USE (gcSysHome+'SYWCONFG')
        llCUsedBy = .T.
      ELSE
        SELECT SYWCONFG
      ENDIF
      lcConfgTag=TAG()
      SET ORDER TO TAG VARNAME
    ENDIF
    IF SEEK(PADR(UPPER(lcArray),10))
      DO CASE
        CASE cDefa_Typ='V'
          lcRetVal=lfTrnsStr(STRTRAN(mData_Def,CHR(13)+CHR(10),''),cData_Typ)
        CASE cDefa_Typ='E'
         lcRetVal=EVAL(STRTRAN(mData_Def,CHR(13)+CHR(10),''))
      ENDCASE    
    ENDIF
  ENDIF
ELSE
  llUseSycC = .T.
  IF !USED('SYWCONFG')
    SELECT 0
    USE (gcSysHome+'SYWCONFG')
    llCUsedBy = .T.
  ELSE
    SELECT SYWCONFG
  ENDIF
  lcConfgTag=TAG()
  SET ORDER TO TAG VARNAME

  FOR lnCount = 1 TO ALEN(lcArray,1)
    llError = .F.
    lcRetVal=''
    SELECT SYWSETP
    IF SEEK(PADR(UPPER(IIF(llTwoDimen = 'V',lcArray[lnCount],lcArray[lnCount,1])),10))
      DO CASE
        CASE cDefa_Typ='V'
          lcRetVal=lfTrnsStr(STRTRAN(mData_Def,CHR(13)+CHR(10),''),cData_Typ)
        CASE cDefa_Typ='E'
         lcRetVal=EVAL(STRTRAN(mData_Def,CHR(13)+CHR(10),''))
      ENDCASE
    ELSE
      SELECT SYWCONFG
      IF SEEK(PADR(UPPER(IIF(llTwoDimen = 'V',lcArray[lnCount],lcArray[lnCount,1])),10))
        DO CASE
          CASE cDefa_Typ='V'
            lcRetVal=lfTrnsStr(STRTRAN(mData_Def,CHR(13)+CHR(10),''),cData_Typ)
          CASE cDefa_Typ='E'
           lcRetVal=EVAL(STRTRAN(mData_Def,CHR(13)+CHR(10),''))
        ENDCASE    
      ENDIF
    ENDIF  
    DO CASE
      CASE llTwoDimen = 'N'
        &lcArray[lnCount,2] = lcRetVal
      CASE llTwoDimen = 'V'
        &lcArray[lnCount] = lcRetVal
      CASE llTwoDimen = 'A'
        lcArray[lnCount,2] = lcRetVal
    ENDCASE
    lnRetCount=lnRetCount+IIF(!llError,1,0)    
  ENDFOR
ENDIF
ON ERROR &lcOnErr
IF llUseSycC
  SELECT SYWCONFG
  IF !EMPTY(lcConfgTag)
    SET ORDER TO TAG (lcConfgTag)
  ELSE
    SET ORDER TO
  ENDIF
  IF llcUsedBy
    IF lcArray $ "LLHIST | M_Comp_Id"
      USE IN SYCCONFG
    ELSE
      USE IN SYWCONFG
    ENDIF
  ENDIF
ENDIF  

SELECT SYWSETP
IF !EMPTY(lcSetupTag)
  SET ORDER TO TAG (lcSetupTag)
ELSE
  SET ORDER TO
ENDIF
IF llSUsedBy
  USE IN SYWSETP
ENDIF

IF llReUsedBy .AND. !EMPTY(lcSYWSETPDir)
  SELECT SYWSETP
  USE (lcSYWSETPDir) ORDER lnCurTag
  IF BETWEEN(lnCurRec, 1, RECCOUNT())
    GO lnCurRec
  ELSE
    GO TOP
  ENDIF  
ENDIF  

SELECT (lnAliasNo)
RETURN IIF(!llArrayORvar,lcRetVal,lnRetCount)


*!*************************************************************
*! Name      : lfOpnDbf  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Function Use dbf.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfOpnDbf()
*!*************************************************************
*!
FUNCTION lfOpnDbf
PARAMETER lcFil2Use , lcType

IF !USED(lcFil2Use)
  IF TYPE ("lcType") = 'C' .AND. lcType = "D"
    =gfOpenFile(gcDataDir+lcFil2Use,"","SH")
  ELSE
    =gfOpenFile(gcsyshome+lcFil2Use,"","SH")
  ENDIF
ENDIF
SELECT (lcFil2Use)

*!*************************************************************
*! Name      : lfGetReq  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Function to get required module for each module.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfGetReq()
*!*************************************************************
*!
FUNCTION lfGetReq
PARAMETER lcModule
PRIVATE lnOldAls , lcOldKey , lcAppKey , lcReq2Ins , lcToRet

lnOldAls = SELECT(0)
SELECT SYDAPPL
lcAppKey = EVAL(KEY())
lcOldKey = ORDER()
SET ORDER TO Capp_id
IF SEEK(lcModule)
  lcReq2Ins = mReqModul
  lcToRet = ALLTRIM(lcReq2Ins)
ENDIF
SET ORDER TO lcOldKey
=SEEK(lcAppKey)
SELECT(lnOldAls)
RETURN (lcToRet)


*!*************************************************************
*! Name      : lfCheckReq  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Function to check if required module selected.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfCheckReq()
*!*************************************************************
*!
FUNCTION lfCheckReq
PARAMETER lcRequird

PRIVATE lcOldAls , lnMIndex , llToRet , lcAllModul , llFundMod

*--- Get all module into string.
lcAllModul = ""
FOR lnAIndex = 1 TO ALEN(laAllMod,1)
  lcAllModul = lcAllModul + ALLTRIM(SUBSTR(laAllMod[lnAIndex],2,3))+"|"
ENDFOR
llToRet  = .T.
lnStartPo = 1
lcOldAls = SELECT(0)
lnReqNo  = OCCURS(',',lcRequird)+1
FOR  lnMIndex = 1 TO lnReqNo
  lcModId  = SUBSTR(lcRequird,lnStartPo,2)
  lnStartPo = lnStartPo + 3
  *--- ASCAN(laAllMod,)
  llFundMod = lcModId $ lcAllModul
  SELECT SYDAPPL
  lcAppKey = EVAL(KEY())
  lcOldKey = ORDER()
  SET ORDER TO Capp_id
  =SEEK(lcModId)
  lccmProm = lcDiamond+' '+sydappl.cApp_Id+" "+sydappl.cApp_name+" +"+IIF(sydappl.lSetReq,"T","F")
  lncmPos = ASCAN(laAllMod,lccmProm)
  llToRet = (lncmPos # 0)
  IF !llToRet
    =gfModalGen("TRM00374B00000","DIALOG",lcModId+"|"+ALLTRIM(SUBSTR(laAllMod[lnIndex],2,3)))
  ENDIF
ENDFOR
SELECT SYDAPPL
SET ORDER TO lcOldKey
=SEEK(lcAppKey)
llToRet = IIF(EMPTY(lcRequird),.T.,llToRet)
SELECT(lcOldAls)
RETURN (llToRet)
*--- lcMod2Chk

*!*************************************************************
*! Name      : lfUpdMast  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Function to update master files.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfUpdMast()
*!*************************************************************
*!
FUNCTION lfUpdMast
PARAMETER lcFrmFile , lcToFile , llCondtion , llAll

PRIVATE lnOldAls
lnOldAls = SELECT(0)
*--- NFILE,lcIndex,MODE,lcAliasNam,llForceOp
IF FILE(gcsyshome+lcFrmFile+".DBF") .AND. FILE(gcDataDir+lcToFile+".DBF")
  =gfOpenFile(gcdatadir+lcToFile,'','SH')
  =gfOpenFile(gcsyshome+lcFrmFile,"","SH")
  IF !llAll   && Not all the file
    SELECT &lcFrmFile
    SET FILTER TO lSelect
  ENDIF
  IF llCondtion  && There is Idintifier to identify each related group.
    LOCATE FOR lSelect
    lcCond = cIdentify
    SET FILTER TO 
    SET FILTER TO cIdentify = lcCond
  ENDIF
  DO CASE
    CASE  lcFrmFile = "SYWCUST"
      *--- Update Customer History File
      SELECT (lcFrmFile)
      GO TOP
      SCAN
        SCATTER MEMVAR MEMO
        WAIT WINDOW "Update Customer files"  NOWAIT
        INSERT INTO (lcToFile) FROM MEMVAR
        IF m.Type = 'M' .AND. FILE(gcDataDir+"ARCUSHST.DBF")
          =gfOpenFile(gcdatadir+"ARCUSHST",'Acthst','SH')
          SELECT ARCUSHST
          WAIT WINDOW "Update Customer history files" NOWAIT
          lcCurrYer = syccomp.cCurr_Yer
          FOR lnYerInd = -1 TO 1
            IF !SEEK(m.Account+STR(VAL(syccomp.cCurr_Yer)+lnYerInd,4))
              APPEND BLANK
              REPLACE Account   WITH m.Account,;
                      cFisfYear WITH STR(VAL(syccomp.cCurr_Yer)+lnYerInd,4)
            ENDIF
          ENDFOR
          USE IN ARCUSHST
        ENDIF
      ENDSCAN
    CASE  lcFrmFile = "SYWSCOD"
      WAIT WINDOW "Update style code structure files" NOWAIT
      SELECT (lcFrmFile)
      LOCATE FOR lSelect
      IF FOUND()
        lcStruId = cItemrecty
        SELECT (lcFrmFile)
        SET ORDER TO Segno
        LOCATE FOR citemrecty+cisegno = lcStruId .AND. cIsegType = "S"
        llExSiz = FOUND()
        =SEEK(lcStruId)
        SCAN REST WHILE citemrecty+cisegno = lcStruId
          SCATTER MEMVAR MEMO
          m.cItemrecty = 'U'
          INSERT INTO (lcToFile) FROM MEMVAR
        ENDSCAN
        IF llExSiz
          =gfOpenFile(gcsyshome+"SYWSETP","Nvarpos","SH")
          SELECT SYWSETP
          SET ORDER TO Varname
          =SEEK("M_USEEXSSC")
          REPLACE mData_Def WITH ".T."
          USE IN SYWSETP
        ENDIF
      ENDIF
    OTHERWISE
      WAIT WINDOW "Update "+lcFrmFile+" file"  NOWAIT
      SELECT &lcFrmFile
      SCAN
        SCAT MEMVAR MEMO
        INSERT INTO (lcToFile) FROM MEMVAR
      ENDSCAN
      IF lcFrmFile = "SYWSETP"
        =lfRstMemo()
      ENDIF
  ENDCASE
  USE IN &lcToFile
  USE IN &lcFrmFile
ENDIF
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfFinish  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Function to finish installaiotn process.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfFinish()
*!*************************************************************
*!
FUNCTION lfFinish


lcFHlp    = "Create Company and update files with default data "
DO (gcScrDir + gcWinAppl + '\smwfinsh.SPR')

*!*************************************************************
*! Name      : lfVFinish  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Valid Function for finish button.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfVFinish()
*!*************************************************************
*!
FUNCTION lfVFinish

=lfCretComp()
=lfvIns()
*--- Call Function to update default fiscal calender

IF USED("SYWCONFG")
  USE IN SYWCONFG
ENDIF
=gfOpenFile(gcSysHome+'SYWCONFG')
SELECT SYWCONFG
COPY TO (ALLT(laData[11])+'\SETUPS') FOR EMPTY(CAPP_ID) WITH CDX
SELECT SYCCOMP
=SEEK(laData[1])
lcCmopMdl = mComp_Mdl
IF "AP" $ lcCmopMdl
  WAIT WINDOW "Update account payable setup file" NOWAIT
  =lfOpnDbf("SYWAPSET")
  SELECT SYWAPSET
  COPY TO (ALLT(laData[11])+'\APSETUP') WITH CDX
ENDIF
IF "GL" $ lcCmopMdl
  WAIT WINDOW "Update General Ledger setup file" NOWAIT
  =lfOpnDbf("SYWGLSET")
  COPY TO (ALLT(laData[11])+'\GLSETUP') WITH CDX
ENDIF

*--- Parameter : From file , To File , Update all
=lfUpdMast("SYWCUST","CUSTOMER")
*:B#804025,1 SSH 25/03/2001 Change function name to add default codes.
*=lfUpdMast("SYWCODE","CODES")
=lfUpdCodes()
*:B#804025,1 SSH [END]

=lfUpdMast("SYWSCAL","SCALE")
=lfUpdMast("SYWSCALH","scalehd")
=lfUpdMast("SYWSETP","SETUPS",.F.,.T.)
=lfUpdMast("SYWSCOD","ICISTRU",.F.,.T.)
=lfUpdMast("sywaccod","accod",.T.)
=lfUpdRep()
IF lfUpCalndr()
  IF "GL" $ lcCmopMdl
    =lfUpdChr()
  ENDIF
ENDIF
=lfUpdMast("SYWWAREH","warehous",.F.,.T.)
** Do you want to set this company as a system default company. **
** <  Yes  > - <  Cancel  > **
IF gfModalGen("TRM00199B00006","DIALOG") = 1
  =gfOpenFile(gcSysHome+'SYCINST')
  REPLACE sycinst.cinsdfcom WITH laData[1]
  SELECT SYCCOMP
ENDIF

=gfWait("00209","NOWAIT")
HIDE MENU _MSYSMENU SAVE
RELEASE POPUPS ALL EXTENDED
SET SYSMENU TO
*** Call the menu builder to rebuild the intial menu. ***
=gfMenuBld("SY","I","NOSHOW")
*** Call the menu builder to rebuild the second menu. ***
=gfMenuBld("SY","S","NOSHOW")
*** Call the menu builder to rebuild the current module. ***
=gfMenuBld(gcAct_Appl,"A")
SHOW MENU _MSYSMENU
WAIT CLEAR

IF laData[1] = gcAct_Comp
  gcCom_Name = ALLTRIM(SYWCOMP.cCom_Name)
  gcDataDir  = gfGetDataDir(ALLTRIM(SYWCOMP.cCom_dDir))
  gcComp_Mdl = ALLTRIM(SYWCOMP.mComp_Mdl)
  gcComp_Lvl = IIF(EMPTY(SYWCOMP.ccompprnt),'P','C')
  gcPrnt_Cmp = IIF(!EMPTY(SYWCOMP.ccompprnt),;
                   SYWCOMP.ccompprnt,SYWCOMP.ccomp_Id)
  gcContCode = ALLTRIM(laData[8])
  =gfCompSets(ALLTRIM(SYWCOMP.ccont_code))
ENDIF  
lcNextActn = 'C'

*!*************************************************************
*! Name      : lfRstMemo  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Function to reset validation for each SYWSETP file record.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfRstMemo()
*!*************************************************************
*!
FUNCTION lfRstMemo
PRIVATE lnOldAls

lnOldAls = SELECT(0)

=gfOpenFile(gcSysHome+'SYCCONFG','Nvarpos','SH')
=gfOpenFile(gcDataDir+'SETUPS','Nvarpos','SH')
SELECT SETUPS
APPEND FROM gcSysHome+'SYCCONFG' FOR ALLTRIM(cFld_Name) $ "LLHIST||M_COMP_ID"
SET ORDER TO Nvarpos
SET RELATION TO SETUPS.capp_id+STR(SETUPS.nvarpos) INTO SYCCONFG ADDITIVE
SCAN
  WAIT WINDOW "Restore "+ALLTRIM(cFld_Head)+" validation" NOWAIT
  REPLACE mVald_Str WITH SYWCONFG.mVald_Str
ENDSCAN
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfGetHelp  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Function to get help.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfGetHelp()
*!*************************************************************
*!
FUNCTION lfGetHelp
PARAMETER lcBrowNam

PRIVATE lnOldAls
lcWizHlep  = ''
lnOldAls = SELECT(0)
SELECT SYWHELP
*--- cscrno+cobjname

IF TYPE("lcBrowNam") = 'C'
  IF SEEK(PADR(SYWSEQNC.cScrSeq,3)+lcBrowNam)
    lcWizHlep  = SYWHELP.mHelp
  ENDIF
ELSE
  IF SEEK(PADR(SYWSEQNC.cScrSeq,3)+VARREAD())
    lcWizHlep  = SYWHELP.mHelp
  ENDIF
ENDIF
SHOW GET lcWizHlep
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfEraTemp  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Function to erase temp files.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfEraTemp()
*!*************************************************************
*!
FUNCTION lfEraTemp
PARAMETER lcTmpName

IF USED(lcTmpName)
  USE IN &lcTmpName
ENDIF
IF FILE(gcWorkDir+lcTmpName+".DBF")
  ERASE &gcWorkDir.&lcTmpName..DBF
ENDIF
IF FILE(gcWorkDir+lcTmpName+".CDX")
    ERASE &gcWorkDir.&lcTmpName..CDX
ENDIF

*:----------------------------------------------------------------
*: Program file        : lfUpCalndr
*: Program description : Update calender files
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : N000185
*:----------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------
*: Called From         : None.
*:----------------------------------------------------------------
*:
FUNCTION lfUpCalndr
PRIVATE lnOldAls , lcCurrYer

lnOldAls  = SELECT(0)
lcCurrYer = lfGetCYer()
IF EMPTY(lcCurrYer)
  RETURN(.F.)
ELSE
  IF !USED("FISHD")
    =gfOpenFile(gcdatadir+"FISHD",'','SH')  
  ENDIF
  SELECT FISHD
  DELETE ALL
  =lfOpnDbf("SYWFSHD")
  SELECT SYWFSHD
  SET ORDER TO
  SCAN
    SCATTER MEMVAR MEMO
    =lfUpdYer("m.cFisyStat")
    INSERT INTO FISHD FROM MEMVAR
    SELECT FISHD
    lcOldDat = SET("DATE")
    SET DATE TO DMY
    REPLACE dFisbgDat WITH CTOD("01/01/"+cFisfYear),;
            dFisenDat WITH CTOD("31/12/"+cFisfYear)
    SET DATE TO &lcOldDat
  ENDSCAN
  IF !USED("FSPRD")
    =gfOpenFile(gcdatadir+"FSPRD",'','SH')  
  ENDIF
  SELECT FSPRD
  DELETE ALL
  =lfOpnDbf("SYWFSPD")
  SELECT SYWFSPD
  SET ORDER TO
  SCAN
    SCATTER MEMVAR MEMO
    =lfUpdYer("m.cFisfYear")
    INSERT INTO FSPRD FROM MEMVAR
    SELECT FSPRD
    lcOldDat = SET("DATE")
    SET DATE TO DMY
    REPLACE dfsppbgdt WITH CTOD("01/"+cfspprdid+"/"+cFisfYear)
    DO CASE
      CASE cfspprdid = "02"
        REPLACE dfsppendt WITH CTOD("28/"+cfspprdid+"/"+cFisfYear)
      CASE cfspprdid $ "07|08"
        REPLACE dfsppendt WITH CTOD("31/"+cfspprdid+"/"+cFisfYear)
      CASE cfspprdid $ "04|06"
          REPLACE dfsppendt WITH CTOD("30/"+cfspprdid+"/"+cFisfYear)        
      CASE MOD(VAL(cfspprdid),2) = 0
          REPLACE dfsppendt WITH CTOD("31/"+cfspprdid+"/"+cFisfYear)
      CASE MOD(VAL(cfspprdid),2) <> 0
          REPLACE dfsppendt WITH CTOD("30/"+cfspprdid+"/"+cFisfYear)
    ENDCASE
    SET DATE TO &lcOldDat
  ENDSCAN
  USE IN FSPRD
  USE IN FISHD
  USE IN SYWFSPD
  USE IN SYWFSHD
  SELECT(lnOldAls)
ENDIF

*:----------------------------------------------------------------
*: Program file        : lfGetCYer
*: Program description : Functino to get current year.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : N000185
*:----------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------
*: Called From         : None.
*:----------------------------------------------------------------
*:
FUNCTION lfGetCYer
PRIVATE lnOldAls
lnOldAls = SELECT(0)
SELECT SYCCOMP
=SEEK(laData[1])
RETURN cCurr_Yer
SELECT(lnOldAls)

*:----------------------------------------------------------------
*: Program file        : lfUpdYer
*: Program description : Function to Return the current year.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : N000185
*:----------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------
*: Called From         : None.
*:----------------------------------------------------------------
*:
FUNCTION lfUpdYer
PARAMETER lcToChk
DO CASE
  CASE &lcToChk = "P"
    m.cFisfYear = STR(VAL(lcCurrYer)-1,4)
  CASE &lcToChk = "C"
    m.cFisfYear =  lcCurrYer
  CASE &lcToChk = "N"
    m.cFisfYear =  STR(VAL(lcCurrYer)+1,4)
ENDCASE

*:----------------------------------------------------------------
*: Program file        : lfUpdBal
*: Program description : Function to update GL-balance .
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : N000185
*:----------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------
*: Called From         : None.
*:----------------------------------------------------------------
*:
FUNCTION lfUpdBal
PRIVATE lnOldAls
lnOldAls = SELECT(0)
IF FILE(gcdatadir+"GLACBALS.DBF")
  =lfOpnDbf("SYWACBAL")
  IF !USED("GLACBALS")
    =gfOpenFile(gcdatadir+"GLACBALS",'','SH')  
  ENDIF
  SELECT GLACBALS
  DELETE ALL
  SELECT SYWACBAL
  SCAN FOR cIdentify = lcChrId
    SCATTER MEMVAR MEMO
    lcCurrYer = lfGetCYer()
    =lfUpdYer("cFisfYear")
    INSERT INTO GLACBALS FROM MEMVAR
  ENDSCAN
  USE IN SYWACBAL
  USE IN GLACBALS
ENDIF
SELECT(lnOldAls)

*:----------------------------------------------------------------
*: Program file        : lfUpdChr
*: Program description : Function to update GL-balance .
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : N000185
*:----------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------
*: Called From         : None.
*:----------------------------------------------------------------
*:
FUNCTION lfUpdChr

PRIVATE lnOldAls , lcChrId

lnOldAls = SELECT(0)
lcChrId = lfGetChrId()
=lfOpnDbf("SYWGLCHR")
=lfOpnDbf("GLACCHAR","D")
SELECT SYWGLCHR
SCAN FOR cIdentify = lcChrId
  SCATTER MEMVAR MEMO
  WAIT WINDOW "Update GLACCHAR." NOWAIT 
  INSERT INTO GLACCHAR FROM MEMVAR
ENDSCAN
USE IN GLACCHAR
USE IN SYWGLCHR

=lfOpnDbf("SYWGLTYP")
=lfOpnDbf("GLTYPES","D")
SELECT SYWGLTYP
SCAN FOR cIdentify = lcChrId
  WAIT WINDOW "Update GLTYPES." NOWAIT 
  SCATTER MEMVAR MEMO
  INSERT INTO GLTYPES FROM MEMVAR
ENDSCAN
USE IN SYWGLTYP
USE IN GLTYPES

=lfOpnDbf("SYWGLSGV")
=lfOpnDbf("GLSEGVAL","D")
SELECT SYWGLSGV
SCAN FOR cIdentify = lcChrId
  WAIT WINDOW "Update GLSEGVAL." NOWAIT 
  SCATTER MEMVAR MEMO
  INSERT INTO GLSEGVAL FROM MEMVAR
ENDSCAN
USE IN GLSEGVAL
USE IN SYWGLSGV
*:B#804025,1 SSH 25/03/2001 Add default like code.
=lfOpnDbf("sywlink")
=lfOpnDbf("gl_link","D")
SELECT sywlink
SCAN FOR cIdentify = lcChrId
  WAIT WINDOW "Update gl_link." NOWAIT 
  SCATTER MEMVAR MEMO
  INSERT INTO gl_link FROM MEMVAR
ENDSCAN
*:B#804025,1 SSH [END]

=lfUpdBal()
SELECT(lnOldAls)

*:----------------------------------------------------------------
*: Program file        : lfGetChrId
*: Program description : Function to get chart of account ID.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : N000185
*:----------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------
*: Called From         : None.
*:----------------------------------------------------------------
*:
FUNCTION lfGetChrId
PRIVATE lnOldAls , lcReturn
lcReturn = "*"
lnOldAls = SELECT(0)
IF !USED("SYWACHRT")
   = gfOpenFile(gcSysHome+"SYWACHRT","Cidentify","SH")
ENDIF
SELECT SYWACHRT
LOCATE FOR lSelect
IF FOUND()
  lcReturn = cIdentify
ENDIF
REPLACE ALL lSelect WITH IIF(cIdentify="1" ,.T.,.F.)
SELECT(lnOldAls)
RETURN(lcReturn)

*:B#804025,1 SSH 25/03/2001 Add default repcode.
*:----------------------------------------------------------------
*: Program file        : lfUpdRep
*: Program description : Function to add default sales rep.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : N000185
*:----------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------
*: Called From         : None.
*:----------------------------------------------------------------
*:
FUNCTION lfUpdRep
PRIVATE lnOldAls
lnOldAls = SELECT(0)
IF FILE(gcdataDir+"salesrep.DBF")
  =lfOpnDbf("salesrep","D")
  SELECT salesrep
  APPEND BLANK
  REPLACE RepCode WITH "HSE";
          NAME    WITH "HOUSE ACCOUNT"
  SELECT SYCCOMP
  =SEEK(laData[1])
  SCATTER MEMVAR MEMO
  SELECT salesrep
  GATHER MEMVAR MEMO
ENDIF
SELECT(lnOldAls)
*:B#804025,1 SSH [END]

*:B#804025,1 SSH 25/03/2001 Add default repcode.
*:----------------------------------------------------------------
*: Program file        : lfUpdCodes
*: Program description : Function to add default codes.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : N000185
*:----------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------
*: Called From         : None.
*:----------------------------------------------------------------
*:
FUNCTION lfUpdCodes
PRIVATE lnOldAls
IF FILE(gcdataDir+"CODES.DBF")
  lnOldAls = SELECT(0)
  =gfOpenFile(gcdatadir+"CODES",'','SH')
  SET ORDER TO Ccode_no IN CODES
  SELECT SYWCODE
  SET FILTER TO lSelect
  SCAN
      lcTempCod = cFld_Name
      SCATTER MEMVAR MEMO
      WAIT WINDOW "Update SYWCODES for "+lcTempCod NOWAIT
      SELECT CODES
      IF !SEEK("D"+lcTempCod) .AND. m.cdefcode <> "D"
        APPEND BLANK
        GATHER MEMVAR MEMO
        REPLACE  cdefcode   WITH "D",;
                 lRltFields WITH .F.,;
                 crltField  WITH "N",;
                 crltd_Nam  WITH ""
      ENDIF
      INSERT INTO CODES FROM MEMVAR
    ENDSCAN
    SELECT SYWCODE
    SET FILTER TO
    REPLACE ALL lSelect WITH .T.
    SELECT(lnOldAls)
ENDIF