*:************************************************************
*: Program file  : SMGLREL
*: Program desc. : Release postings from ARIA or SBT subledger and
*:               : create unposted GL batches, and release AP to GL batches
*: For screen    : SMGLREL
*:        System : Aria Advantage Series.
*:        Module : Material (SM).
*:     Developer : Ahmed Amer (AHM)
*:************************************************************
*: Calls :
*:         Procedures : lpOpnFiles,lpLckFiles,lpRelPrint,lpAriaRel,
*:                    : lpSBTRel,lpPrdVal,lpAccVal,lpAcPrdVal,lpPostMark,
*:                    : APGLREL
*:         Functions  : lfvComp,lfvProceed,lfGetGLBt,lfGetGLTr,
*:                    : gfGetMemVar,gfModalGen,gfFLock,gfRLock,gfSequence,
*:                    : gfGetTime
*:************************************************************
*: Passed Parameters  : None
*:************************************************************
*: Example : DO SMGLREL
*:
*:************************************************************
*: Modifications :
*: E301024 : (AAMER) 09/30/98
*:          1- add entry number in GLDIST file to link between
*:             an entry in GLDIST, and a released line in GLTRNSDT.
*:          2- create batches and transaction No. to Year+Period+Source Module
*:             instead of Year+Module only in (Summary case).
*:             create batches and transaction No. to Year+Period+Source Module+session
*:             instead of Year+Module+session only in (Detail case).
*:*******************************************************************************************
*E301098,1 HESHAM 12/16/98 Get company data path using gfGetDataDir(..)
*E300789,1 Hesham 03/04/99
*E300789,1 remove field ccomp_id from files ACCOD,FISHD,FSPRD,FSHLD,CODES
*E300789,1 Change Any seeking in these files
*E301173,1 AHM As a result of removing gl linked company if we are linked to Aria
*B802195,1 AHM 04/26/99 Check if a module is installed and setup before
*B802195,1     opening a relevant file.
*B802411,1 AHN 07/21/1999
*B802792,1 WAB 01/05/2000 Fix the bug of 'GLDIST' file does not exist.
*B603350,1 BWA 01/23/2000 Fix the bug of After posting batches in GL
*B603350,1                WE get the following error message - File in Use.
*B603503,1 ADEL 03/09/2000 Don't release AP unless it is installed and linked to GL.
*B603504,1 ADEL 03/11/2000 - Make the screens of selecting modules for releasing work.
*B603504,1 				   - Release transactions by category key per module
*B803530,1 RAMY 07/31/2000 Print the posting register report
*B605290,1 MHM  12/30/2001 Fix the bug of Can't release the child AP entries to Parent compnay 
*B605723,1 Amin 03/21/2002 Stop line was added by Moh. Hassan since it was casue problem with if the customer has 
*                 Fiscal year different than fiscal calender. 
*B606514,1 RAE  10/24/2002 Increase the size of Tran# (GLDIST.Tran_no) to 6 character 
*                          in the SMRELPOD.FRX (Detail).
*C037437,1 MHM 02/05/2004 Update CTRNDESC with new value for Suzan
*B121766,1 MHM 03/17/2004 Update Gldist NL type with correct Module if 'MF' Not Installed
*B125320,1 NNA 01/03/2005 Fix bug happen in cause of B#121766 that by Replacing all the GL Trans.
*B125320,1 NNA            with 'PO' instead of the Transaction Module.
*B126253,1 NNA 01/26/2005 Fix bug that the Release to GL in some cases update the Source module 
*B126253,1 NNA            with one Chr. only like 'PO' saved as 'P'
*B125803,1 MHM 04/06/2005 Fix Bug of description not updated in Case of Detail and Same Account
*B608687,1 TMI 09/11/2008 Prevent the screen to show if no company is selected
*:************************************************************************************************
*--lccAccFile  Variable that hold chart of account file name
*--lcBtchFile  Variable that hold batch file name
*--lcHUnPFile  Variable that hold Header transactions file name
*--lcDUnPFile  Variable that hold Detail transactions file name
*--lcDistFile  Variable that hold distribution file name
*--lcFsPrd     Variable that hold Fiscal period file name
*--lcGLVer     Variable that hold the linked GL version (ARIA\SBT)
*--lcGLCo      Variable that hold the company ID which the
*--            system is linked to
*--lcComDesc   Variable that hold the descripton of the company
*--            that we release its transactions
*--lcxense  Variable that hold the suspense account
*--lcDataDir   Variable that hold the data path of the company we want to
*--            release its transactions
*--lcSBTSDir   Variable that hold the SBT System directory
*--lcGLCoDDir  Variable that hold the data directory of the company that
*--            G/L is linked to.
*--lcSequePth  Variable that hold the path that the sequenece file is opened
*--            from.
*--lcAPDisPth  Variable that hold the path that the APDIST file is opened
*--            from.
*--lcAPSetPth  Variable that hold the path that the APSETUP file is opened
*--            from
*--lcLnkChPth  Variable that hold the path that the lcLinkChar alias
*--            (which chart of account file is opened with) is opened from
*--lcSysDaAl   Variable that hold alias name that SYSDATA file is opened with
*--lcSequnAl   Variable that hold alias name that Sequence file is opened with
*--lcGLSetAl   Variable that hold alias name that GLSETUP file is opened with
*--lcAcCodAl   Variable that hold alias name that ACCOD file is opened with
*--lcAccHrAl   Variable that hold alias name that Chart of account file
*--            is opened with
*--lcFsPrdAl   Variable that hold alias name that Fiscal period file is
*--            opened with
*--lcGDistAl   Variable that hold alias name that GLDIST   file is opened with
*--lcBatchAl   Variable that hold alias name that GLBATCH  file is opened with
*--lcHTrnsAl   Variable that hold alias name that GLTrnsHd file is opened with
*--lcDTrnsAl   Variable that hold alias name that GLTrnsDt file is opened with
*--lcGSJorAl   Variable that hold alias name that GLSubJor file is opened with
*--lcInvHdAl   Variable that hold alias name that INVHDR   file is opened with
*--lcRetHdAl   Variable that hold alias name that INVDTL   file is opened with
*--lcCutTkAl   Variable that hold alias name that CUTTKT   file is opened with
*--lcPosHdAl   Variable that hold alias name that POSHDR   file is opened with
*--lcCredtAl   Variable that hold alias name that CREDIT   file is opened with
*--lcDebitAl   Variable that hold alias name that DEBIT    file is opened with
*--lcLinkChar  Variable that hold alias name that lcCAccFile file is opened with
*--lcDetail    Variable that hold Transfer type
*--lcBatchTmp  Variable that hold Batch tmep. file name
*--lcDUnPTmp   Variable that hold Transaction unposted header temp. file name
*--lcHUnPTmp   Variable that hold Transaction unposted detailed temp. file name
*--lcTmpFile   Variable that hold Temp. file name
*--lcCurProc   Variable that hold the name of the executing procedure to be
*--            used in case uncomplete session
*--lcAccMask   Variable that hold the system account mask
*--lcComp      Variable that hold the company ID
*--lcCurComp   Variable that hold the cuurent company after select another
*--            company from the Companies popup
*--lcBatchNo   Variable that hold the generated batch no
*--lcTranNo    Variable that hold the generated trans no
*--lcTmpBatch  Variable that hold Batch temp. file name which is used
*--            in APGLREL Program
*--lcTmpTrnHd  Variable that hold Transaction header temp. file name which is used
*--            in APGLREL Program
*--lcTmpTrnDt  Variable that hold Transaction detail temp. file name which is used
*--            in APGLREL Program
*--lcTmpAPDist Variable that hold APdist temp. file name which is used
*--            in APGLREL Program
*--lcSession   Variable that hold the session id for the uncompleted session
STORE SPACE(0) TO lccAccFile,lcBtchFile,lcHUnPFile,;
  lcDUnPFile,lcDistFile,lcFsPrd,lcGLVer,lcGLCo,;
  lcComDesc,lcSuspense,lcDataDir,lcComDesc,lcSequePth,;
  lcSBTSDir,lcGLCoDDir,lcAPDisPth,lcAPSetPth,lcLnkChPth,;
  lcSysDaAl,lcLinkChar,lcSequnAl,lcGLSetAl,lcAcCodAl,lcAccHrAl,;
  lcFsPrdAl,lcGDistAl,lcBatchAl,lcHTrnsAl,lcDTrnsAl,;
  lcGSJorAl,lcInvHdAl,lcRetHdAl,lcCutTkAl,lcPosHdAl,;
  lcCredtAl,lcDebitAl,lcDetail,lcBatchTmp,lcDUnPTmp,;
  lcHUnPTmp,lcTmpFile,lcCurProc,lcAccMask,lcComp,lcCurComp,;
  lcBatchNo,lcTranNo,lcTmpBatch,lcTmpTrnHd,lcTmpTrnDt,;
  lcTmpAPDist,lcSession

*-- llNoShow   Variable that prevent or enable the program go into    ;
lpshow once the PROGRAM is executed;
(.F. GO IN lpshow,.T. don't)
*-- llUnComChk Variable that showes if lfChkUnCmS function is called
*--            in order to make this check only one time
*-- llUnComFnd Variable that showes if uncomplete session is found
*-- llGoOn     Variable indecates to continuing the process of the program
*-- llAPRelCon Variable indecates to continuing the process of AP Release
*-- llCurrent  Variable that showes the Existance of unposted transactions
*--            in dist. file
*-- llNoGLRec  variable that show if there is no unposted records to be released
STORE .F. TO llNoShow,llUnComFnd,llUnComChk,llGoOn,llAPRelCon,llCurrent,;
  llNoGLRec,llNoAPRec,llUseByMe

*-- lnResp     Variable that hold the response of Process messages
*-- lnSessMin  Variable that hold the greatest session no. in GLDIST File
*-- lnSessMax  Variable that hold the lowest   session no. in GLDIST File
*-- lnUnCmSeRc Variable that hold the uncomplete session record no in the "uncmsess" file
*-- lnCurrent  Variable that hold Max. unposted transaction record # in dist. file
*--            when release begin.
*-- llUseByMe  Variable that indicates if the SYGLTRAN file was opened in this
*--            program or it was allready used before getting into the program
*--            inorder to close it if it is opened in the program


*B608687,1 TMI [Start] Select an active company before proceeding allow the variable lcCompDir to be populated
IF EMPTY(gcAct_Comp)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No active company is Selected !')
  RETURN
ENDIF
*B608687,1 TMI [End  ] 

STORE 0 TO lnResp,lnSessMin,lnSessMax,lnUnCmSeRc,lnCurrent

lcProgID  = PADR("RELTOGL",10)
lnSessNo  = gnProgCopy  && This is to hold the no of the opened session from global variable

lcUserID  = PADR(gcUser_id, 10)    && This is to hold the user id


*B802411,1 (Start)
PRIVATE lcTrans,laModSor,laModTar
DIMENSION laModSor[4],laModTar[4]
*B603504,1 (Begin) Remark the following lines to fill it later with only installed and linked modules.
*STORE "Material"  TO laModSor[1],laModTar[1]
*STORE "Inventory" TO laModSor[2],laModTar[2]
*STORE "A/R"       TO laModSor[3],laModTar[3]
*STORE "A/P"       TO laModSor[4],laModTar[4]
*-- Just initialize them
STORE '' TO laModSor,laModTar
STORE .F. TO llMatMod,llExclCon
*B603504,1 (Begin) Remark the following line as then variable is no longer used.
*llApLink   = .F.
llApInstal = .F.
*B603504,1 (End)

lcOkBmp    = gcBmpHome + "OK.BMP"
STORE '' TO lcTranTyp,lcTransNo
STORE {} TO ldTranDate
*B603504,1 (End)
STORE .T. TO llGLRel,llAPRel
cbSelMod = 1
lcTrans = ''
*B802411,1 (End)

*B802411,1 (Start)
*DIMENSION laVars[27]
DIMENSION laVars[32]
*B802411,1 (End)

laVars[01] = "lcComp"
laVars[02] = "lcDataDir"
laVars[03] = "lcComDesc"
laVars[04] = "lcGLVer"
laVars[05] = "lcGLCoDDir"
laVars[06] = "lcSBTSDir"
laVars[07] = "llGoOn"
laVars[08] = "lcSuspense"
laVars[09] = "lcAccMask"
laVars[10] = "llAPRelCon"
laVars[11] = "lnResp"
laVars[12] = "lnSessMin"
laVars[13] = "lnSessMax"
laVars[14] = "lnUnCmSeRc"
laVars[15] = "lcCAccFile"
laVars[16] = "lcBtchFile"
laVars[17] = "lcHUnPFile"
laVars[18] = "lcDUnPFile"
laVars[19] = "lcDistFile"
laVars[20] = "lcFsPrd"
laVars[21] = "lcCurComp"
laVars[22] = "lcDetail"
laVars[23] = "llCurrent"
laVars[24] = "lnCurrent"
laVars[25] = "lcBatchNO"
laVars[26] = "lcTranNO"
laVars[27] = "llUseByMe"
*B802411,1 (Start)
laVars[28] = "laModSor"
laVars[29] = "laModTar"
laVars[30] = "lcTrans"
laVars[31] = "llGLRel"
laVars[32] = "llAPRel"
*B802411,1 (End)

lcSysDaAl  = gfTempName()
lcLinkChar = gfTempName()
lcSequnAl  = gfTempName()
lcGLSetAl  = gfTempName()
lcAcCodAl  = gfTempName()
lcAcCHrAl  = gfTempName()
lcFsPrdAl  = gfTempName()
lcGDistAl  = gfTempName()
lcBatchAl  = gfTempName()
lcHTrnsAl  = gfTempName()
lcDTrnsAl  = gfTempName()
lcGSJorAl  = gfTempName()
lcInvHdAl  = gfTempName()
lcRetHdAl  = gfTempName()
lcCutTkAl  = gfTempName()
lcPosHdAl  = gfTempName()
lcCredtAl  = gfTempName()
lcDebitAl  = gfTempName()

*--laCompany  Array that hold the companies IDs and its descriptions
*--laSourceJr Array that used by Source Journal
DIMENSION laCompany[1]
DIMENSION laSourceJr[1,2]
STORE '' TO laCompany,laSourceJr

IF !gfSetup()
  RETURN
ENDIF

*--Filling laCompany array with companies IDs
SELECT SycComp.cComp_ID+"-"+SycComp.cCom_Name,SycComp.cComp_ID;
  FROM SycComp INTO ARRAY laCompany ;
  ORDER BY SycComp.cComp_ID

lcOrgEsc = ON("KEY","ESCAPE")
ON KEY LABEL ESCAPE DO lpEscKey
*-- calling the screen SMGLREL
DO (gcScrDir + gcWinAppl + '\SMGLREL.SPR')
ON KEY LABEL ESCAPE &lcOrgEsc.

*B603350,1 BWA 01/23/2000 Fix the bug of After posting batches in GL WE get error message - File in Use.
IF USED('lcLinkChar')
  SELECT lcLinkChar
  USE
ENDIF
*B603350,1 BWA 01/23/2000 [END]

*--E301077,82 (Start)
*IF llUseByMe
*  USE IN SYGLTRAN
*ENDIF
*--E301077,82 (End)

*!*************************************************************
*! Name      : lpShow
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Handling the screen mode
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpShow
*!*************************************************************

PROCEDURE lpShow

IF EMPTY(lcComp)
  lcComp    = gcAct_Comp
  lcCurComp = lcComp
ENDIF
IF EMPTY(lcComp)
  IF ASCAN(laCompany,'NOCOMP') = 0
    DIMENSION laCompany[ALEN(laCompany,1)+1,2]
    laCompany[ALEN(laCompany,1),1] = 'Select a company'
    laCompany[ALEN(laCompany,1),2] = 'NOCOMP'
    _CUROBJ = OBJNUM(puCompany)
  ENDIF
  puCompany = ASUBSCRIPT(laCompany,ASCAN(laCompany,'NOCOMP'),1)
ELSE
  puCompany = ASUBSCRIPT(laCompany,ASCAN(laCompany,lcComp),1)
  DIMENSION laSetups[3,2]
  laSetUps[1,1]  = 'M_GL_CO'
  laSetUps[2,1]  = 'M_GL_VERS'
  laSetUps[3,1]  = 'M_SYS_DIR'
  = gfGetMemVar(@laSetups,lcComp)
  lcGLCo  = laSetUps[1,2]
  *B603503,1 (Begin) If no modules linked set it to 'A' as AP may be linked.
  *lcGLVer = laSetUps[2,2]
  lcGLVer = IIF(EMPTY(laSetUps[2,2]),'A',laSetUps[2,2])
  *B603503,1 (End)
  *-- get the company Description
  IF SEEK(lcComp,'SycComp')
    *E301173,1 (Start)
    IF lcGLVer <> 'S'
      lcGLCo = IIF(EMPTY(SyCComp.cCompPrnt),lcComp,ALLTRIM(SyCComp.cCompPrnt))
    ENDIF
    *E301173,1 (Start)
    *E301098,1 Hesham (Start)
    *lcDataDir = ALLTRIM(SycComp.cCom_dDir)
    lcDataDir = gfGetDataDir(ALLTRIM(SycComp.cCom_dDir))
    *E301098,1 Hesham (End)
    lcComDesc = SycComp.Ccom_Name
  ELSE
    lcDataDir = ''
    lcComDesc = ''
  ENDIF

  *--E301077,82 (Start)
  *IF USED ('UNCMSESS')
  *  USE IN UNCMSESS
  *ENDIF
  *USE (lcDataDir+'UNCMSESS') IN 0 SHARED
  *B603504,1 (Begin) Avoid getting 'Alias already in use' error message when selecting
  *B603504,1 (Begin) any other company from companys' combo box.
  IF USED('UNCMSESS')
    USE IN UNCMSESS
  ENDIF
  *B603504,1 (End)
  =gfOpenFile(lcDataDir+'UNCMSESS')
  *--E301077,82 (End)
  IF !llUnComChk
    IF !lfChkUnCmS()
      DO CASE
        *B603503,1 (Begin) ARIA OR OTHER.
        *CASE lcGLVer = 'A'
      CASE lcGLVer <> 'S'
        *B603503,1 (End)
        *-- IF it is linked to ARIA
        lcCAccFile = 'GLAcChar'
        lcBtchFile = 'GLBatch'
        lcHUnPFile = 'GLTrnsHd'
        lcDUnPFile = 'GLTrnsDt'
        lcDistFile = 'GLDist'
        lcFsPrd    = 'FsPrd'
        *-- get the data directory of the G/L linked company
        IF SEEK(lcGLCo,'SycComp')
          *E301098,1 Hesham (Start)
          *lcGLCoDDir = ALLTRIM(SycComp.cCom_dDir)
          lcGLCoDDir = gfGetDataDir(ALLTRIM(SycComp.cCom_dDir))
          *E301098,1 Hesham (End)
        ELSE
          lcGLCoDDir = ''
        ENDIF
      CASE lcGLVer = 'S'
        *-- IF it is linked to SBT
        lcCAccFile =  "GLACNT" + lcGLCo    && Chart of account file name
        lcBtchFile =  "GLBTCH" + lcGLCo    && Batch file name
        lcHUnPFile =  "GLHUNP" + lcGLCo    && Header Unposted file name
        lcDUnPFile =  "GLDUNP" + lcGLCo    && Detailed Unposted file name
        lcDistFile =  "GLDIST"             && Distripution file
        lcFsPrd    =  "SYCDFIS"            && Detailed Fiscal file

        *-- get the system directory of SBT
        lcSBTSDir = laSetUps[3,2]

        *--E301077,82 (Start)
        *USE (lcSBTSDir+'SYSDATA') IN 0 AGAIN ALIAS (lcSysDaAl) SHARED
        =gfOpenFile(lcSBTSDir+'SYSDATA','','SH',@lcSysDaAl,.T.)

        *--E301077,82 (End)

        SELECT (lcSysDaAl)
        LOCATE FOR SYSID = "GL" + lcGLCo
        IF !FOUND()
          *--lcInfoMsg = 'Company not found !!!'
          =gfModalGen('INM00269B00000','DIALOG')
          llGoOn = .F.
        ELSE  &&FOUND
          *-- get the data directory of the G/L linked company
          lcGLCoDDir = ALLTRIM(SUBSTR(DRIVE,61,30))         && DATA DIRECTORY PATH
        ENDIF
      ENDCASE

      llUnComFnd = .F.
    ELSE
      llUnComFnd = .T.
      IF !EMPTY(lcCurProc)
        *-- Here it has to be checked if the company and the GL linked company
        *-- still exist or not
        IF ASCAN(laCompany,lcComp) = 0
          ** Message : ' Company x does not exist.'
          ** <OK>
          =gfModalGen('INM00315B00000','Dialog',lcComp)
        ELSE
          puCompany = ASUBSCRIPT(laCompany,ASCAN(laCompany,lcComp),1)
          llNoThing = lfvProceed()
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDIF
*B603504,1 (Begin) Fill the modules mover with all installed and linked modules.
IF !EMPTY(lcComp)
  =lfGetModul()
  *-- If no modules are linked, uncheck the 'Selceted modules'.
  IF EMPTY(laModSor)
    cbSelMod = 0
  ELSE
    cbSelMod = 1
  ENDIF
  SHOW GET cbSelMod
ENDIF
*B603504,1 (End)

*!*************************************************************
*! Name      : lfvComp
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat the selected company
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvComp()
*!*************************************************************

FUNCTION lfvComp

lcComp  = laCompany[puCompany,2]

*B603504,1 (Begin) Refresh company directories even if the selected company is the default one.
*IF !(lcComp == lcCurComp)
*B603504,1 (End)
DO CASE
  *B603503,1 (Begin) Aria or Other.
  *CASE lcGLVer = 'A'
CASE lcGLVer <> 'S'
  *B603503,1 (End)
  lcCAccFile = 'GLAcChar'
  lcBtchFile = 'GLBatch'
  lcHUnPFile = 'GLTrnsHd'
  lcDUnPFile = 'GLTrnsDt'
  lcDistFile = 'GLDist'
  lcFsPrd    = 'FsPrd'
CASE lcGLVer = 'S'
  lcCAccFile =  "GLACNT" + lcGLCo    && Chart of account file name
  lcBtchFile =  "GLBTCH" + lcGLCo    && Batch file name
  lcHUnPFile =  "GLHUNP" + lcGLCo    && Header Unposted file name
  lcDUnPFile =  "GLDUNP" + lcGLCo    && Detailed Unposted file name
  lcDistFile =  "GLDIST"             && Distripution file
  lcFsPrd    =  "SYCDFIS"            && Detailed Fiscal file
ENDCASE

lcFiles   = ''
llNoThing = lfUpdUnCmS("Close", SPACE(0))

llUnComChk = .F.
DO lpShow
*B603504,1 (Begin) Remark ENDIF
*ENDIF
*B603504,1 (End)

*!*************************************************************
*! Name      : lfvProceed
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Processing GL AND AP Release to GL Batches
*!*************************************************************
*! Calls     :
*!             Procedures : lpOpnFiles,lpLckFiles,lpRelPrint,lpAriaRel,
*:                        : lpSBTRel,lpPrdVal,lpAccVal,lpAcPrdVal,lpPostMark
*:                        : APGLREL
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvProceed()
*!*************************************************************

FUNCTION lfvProceed

*B603504,1 (Begin) Return if no modules are linked to GL.
IF EMPTY(laModSor)
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'No modules linked to GL!')
  RETURN
ENDIF
*B603504,1 (End)
*-- llApRelCon variable showes if we can continue in eleasing AP
*-- even releasing GL is canceled
IF !EMPTY(lcComp)
  *B603503,1 (Begin) Comment the IF, ELSE, and ENDIF
  *IF lcGLVer = 'O'
  ** Message : Release to G/L is not available if you are not linked to
  **           either Aria or SBT.
  ** <OK>
  *  =gfModalGen('INM00318B00000','Dialog')
  *  llGoOn = .F.
  *  llApRelCon = .F.
  *ELSE
  *B603503,1 (End)
  IF !llUnComFnd

    ** Check if the GL module is installed for the parent company
**MH
*    IF SEEK(lcComp,'SycComp')
     IF SEEK(lcGLCo,'SycComp')
**MH
      IF ('GL' $ SYCCOMP.MCOMP_MDL)
        llGoOn = .T.
      ELSE
        ** Message : ' The GL module is not installed for     '
        ** <OK>
        =gfModalGen('TRM04132B00000','Dialog',lcGLCo)
        llGoOn = .F.
      ENDIF
      IF ('AP' $ SYCCOMP.MMODLSET)
        llApRelCon = .T.
      ELSE
        llApRelCon = .F.
      ENDIF
    ELSE
      llGoOn = .F.
      llApRelCon = .F.
    ENDIF

    IF llGoOn
      llGL_Link  = ALLTRIM(UPPER(gfGetMemVar('M_Link_GL',lcComp)))   = 'Y'
      *B603503,1 (Begin) Return only if neither AP nor all other modules linked to GL.
      ** Check if the system is linked to GL
      *IF !llGL_Link
      llGLRel = llGL_Link AND !(lcGLVer = 'O')
      *B603504,1 (Begin) Remark the following lines as we checked AP link in lfGetModul.
      *=SEEK(lcGLCo,'SycComp')
      *lcGLCoDDir = gfGetDataDir(ALLTRIM(SycComp.cCom_dDir))
      *=gfOpenFile(lcGlCoDDir+'APSETUP')
      *GO TOP IN APSETUP
      *llAPRel = !EOF('APSETUP') AND (APSETUP.CAPSGLLINK = 'Y')

      *B603504,1 13/04/2000 (Begin) Remark the following line as llAPRel has been updated in the lfvSelMod.
      *llAPRel  = llApLink
      *B603504,1 (End)

      *B603504,1 (End)
      IF !llGL_Link AND !llAPRel
        *B603503,1 (End)

        IF lcGLVer = 'O'
          ** Message : Release to G/L is not available if you are not linked to
          **           either Aria or SBT.
          ** <OK>
          =gfModalGen('INM00318B00000','Dialog')
        ELSE
          *-- System has not been linked to gl_link yet
          *-- <OK>
          = gfModalGen("INM00292B00000","Dialog")
        ENDIF
        llGoOn = .F.
        llApRelCon = .F.
      ELSE
        llGoOn = .T.

        *--E301077,82 (Start)
        *IF USED('Sequence')
        *  lcSequePth = DBF('Sequence')
        *  USE IN Sequence
        *ENDIF
        *USE (lcGlCoDDir+'Sequence') IN 0 AGAIN SHARED
        =gfOpenFile(lcGlCoDDir+'Sequence')
        *--E301077,82 (End)

        lcSession  = gfsequence('CSESSION')
        lcFiles    = ''
        lnUnCmSeRc = 0
        llNoThing  = IIF(lnUnCmSeRc=0, lfAdUnCmSR(), .T.)

      ENDIF
    ENDIF
  ENDIF
  *B603503,1 (Begin) Comment endif
  *ENDIF
  *B603503,1 (End)
ENDIF

*B802411,1 (Start)
IF cbSelMod = 0
  llGoOn = .F.
  WAIT "No transaction has been selected." WINDOW
ENDIF
*B802411,1 (End)

*-- If linked to GL.
IF llGoOn
  *-- Open files
  DO lpOpnFiles
  IF llGoOn AND lcCurProc < '3'
    lcFiles = "lcBatchTmp," + lcBatchTmp + "," + ORDER(lcBatchTmp) + ";" + ;
      "lcHUnPTmp,"  + lcHUnPTmp  + "," + ORDER(lcHUnPTmp)  + ";" + ;
      "lcDUnPTmp,"  + lcDUnPTmp  + "," + ORDER(lcDUnPTmp)  + ";"
    lcCurProc = '1'
    llNoThing = lfUpdUnCmS("Open", lcCurProc)
  ENDIF
ENDIF
*B802411,1 (Start)
IF llGLRel
  *B802411,1 (End)

  *-- If not cancelled
  IF llGoOn
    *-- Call routine to lock source files
    DO lpLckFiles WITH "SOURCE"
  ENDIF

  *-- If not cancelled (if locks succeeded)
  IF llGoOn

    IF lcCurProc < '3'
      *-- See if there are any unposted transactions in current dist. file
      SELECT (lcGDistAl)
      SEEK " "
      SCAN WHILE &lcGDistAl..posted = " "
        *-- Search distribution file for unposted transactions.

        *-- Save maximum record number currently in file
        lnCurrent = IIF(RECNO(lcGDistAl) > lnCurrent, RECNO(lcGDistAl),lnCurrent)
        llCurrent = .T.
      ENDSCAN
    ENDIF

    *-- Release locks on Distribution file
    = gfFlock(lcGDistAl,.F.)
    IF lcCurProc < '3'
      *-- If no unposted transactions found in distribution file
      IF .NOT. llCurrent
        *-- Display alert

        *? CHR(7)
        *-- "No unreleased entries found in general ledger distribution file."
        *-- <OK>
        *= gfModalGen('INM00306B00000','Dialog')
        llNoGLRec = .T.

        *-- Set flag to cancel
        llGoOn = .F.
      ENDIF
    ENDIF

  ENDIF && llGoOn

  *-- If there is unposted transactions in distribution file.
  IF llGoOn
    *-- Initialize ledger posting balance
    lnBalance = 0.00

    IF lcCurProc < '2'

      *-- Print posting register and calculate ledger posting balance
      DO lpRelPrint

      IF llGoOn .AND. lnBalance <> 0.00
        *-- Out of balance, display alert
        *-- This Ledger posting is not balanced (Credits <> Debits).
        *-- The posting is out of balance by $ x . A balancing entry will
        *-- be posted to the Suspense Account ( x ) in this amount.
        *-- <OK>
        lcTmpStr = ALLTRIM(STR(lnBalance, 10, 2)) + "|" + ALLTRIM(lcSuspense)
        = gfModalGen('INM00307B00000','Dialog',lcTmpStr)
      ENDIF
    ENDIF

  ENDIF  && llGoOn

  IF lcCurProc < '2'
    *-- If not cancelled
    IF llGoOn
      *-- Confirm releasing
      *-- "Ready to release entries as a Summary/Detail posting
      *--  to the General Ledger ?"
      *-- <Proceed;\<Cancel
      lnResp   = 1
      lcTmpStr = IIF(lcDetail = "D", "detail", "summary")
      lnResp = gfModalGen('QRM00308B00012','Dialog',lcTmpStr)
      llGoOn = IIF(lnResp=1,.T.,.F.)
    ENDIF     && llGoOn

    IF llGoOn AND lcGLVer = 'S'
      *-- Call routine to validate GL account numbers
      DO lpAcPrdVal WITH lnCurrent
    ENDIF

    IF llGoOn AND lcCurProc < '3'
      lcFiles = "lcBatchTmp," + lcBatchTmp + "," + ORDER(lcBatchTmp) + ";" + ;
        "lcHUnPTmp,"  + lcHUnPTmp  + "," + ORDER(lcHUnPTmp)  + ";" + ;
        "lcDUnPTmp,"  + lcDUnPTmp  + "," + ORDER(lcDUnPTmp)  + ";"
      lcCurProc = '2'
      llNoThing = lfUpdUnCmS("Open", lcCurProc)
    ENDIF

  ENDIF

  *-- If not cancelled
  IF llGoOn
    *-- Call routine to lock source file ( Distribution file )
    DO lpLckFiles WITH "SOURCE"
  ENDIF

  *-- If not cancelled
  IF llGoOn AND lcGLVer = 'S'
    *-- Call routine to lock GL destination files
    DO lpLckFiles WITH "DESTINATION"
  ENDIF

  IF lcCurProc < '4'
    *-- If not cancelled
    *B603503,1 (Begin) Add AND llGLRel.
    *IF llGoOn
    IF llGoOn AND llGLRel
      *B603503,1 (End)
      IF lcGLVer = 'A'
        *B603504,1 (Begin) Don't release AR,MA.IC if the user selected to release AP only.
        *-- Call routine to post General Ledger entries
        *DO lpAriaRel WITH lnCurrent
        IF ALEN(laModTar,1)<>1 OR laModTar[1]<>'AP'
          DO lpAriaRel WITH lnCurrent
        ENDIF
        *B603504,1 (End)
      ELSE
        *-- Call routine to post General Ledger entries
        DO lpSBTRel WITH lnCurrent
      ENDIF
    ENDIF

    lcCurProc = '4'
    lcFiles = ''
    llNoThing = lfUpdUnCmS("Open", lcCurProc)
  ENDIF
  *B802411,1 (Start)
ENDIF
*B802411,1 (End)

lcOrgActCo  = gcAct_Comp
lcOrgDDir   = gcDataDir

*B802411,1 (Start)
*IF lcGLVer = 'A' AND llApRelCon

*B603503,1 (Begin) llAPRel shouldn't be .T. unless AP is linked to GL. Add llGoOn because if the use
*B603503,1         selects to Cancel the release not to Proceed then don't release AP also.
*IF llAPRel AND lcGLVer = 'A' AND llApRelCon
*--Add (OR llNoGlRec) condition as if llNoGlRec (no record to post in GLDIT) llGoOn will be .f.
*--Add llApInstal condition as if the user may press Proceed without browsing modules mover. In this case llAPRel will be .T.
*--even if AP not installed.
IF llAPRel AND llApInstal AND (llGoOn OR llNoGlRec)
  *B603503,1 (End)
  IF lcCurProc < '6'
    llNewSetUp = .F.
    gcAct_Comp = lcComp
    gcDataDir  = lcDataDir
    *-- Release AP to G/L Batches
    llFunCall = .T.
    DO (gcAppHome+"AP.APP") WITH 'APGLREL',.F.,.T.
  ENDIF
ENDIF
gcAct_Comp = lcOrgActCo
gcDataDir  = lcOrgDDir

*-- close the aliases that files are used with
*--E301077,82 (Start)
*DO lpClsAlias
*--E301077,82 (End)

*--Koko
lcFiles = ''
llNoThing = lfUpdUnCmS("Close", SPACE(0))
glQuitting = .T.
CLEAR READ

*!*************************************************************
*! Name      : lpOpnFiles
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Open necessary files
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpOpnFiles
*!*************************************************************

PROCEDURE lpOpnFiles

*B603503,1 (Begin) Aria or AP link.
*IF lcGLVer = 'A'
IF lcGLVer = 'A' OR llAPRel
  *B603503,1 (End)
  *--E301077,82 (Start)
  *USE (lcGlCoDDir+'GLSETUP') IN 0 AGAIN ALIAS (lcGLSetAl) SHARED
  =gfOpenFile(lcGlCoDDir+'GLSETUP','','SH',@lcGLSetAl,.T.)
  *--E301077,82 (End)

  SELECT (lcGLSetAl)
  ** Check if GL module has been setup for the company
  IF !&lcGLSetAl..LSETDON
    IF lcCurProc < '3'
      llGoOn = .F.
      ** Message : " The GL module has not been setup for  "
      ** Choices : <OK>
      =gfModalGen('TRM04133B00000','Dialog',lcGLCo)
    ENDIF
  ELSE
    IF lcCurProc < '3'
      lcSuspense = ALLTRIM(Csetsusmj)
    ENDIF

    *--E301077,82 (Start)
    *USE (lcGlCoDDir+'ACCOD') IN 0 AGAIN ALIAS (lcAcCodAl) SHARED
    =gfOpenFile(lcGlCoDDir+'ACCOD','','SH',@lcAcCodAl,.T.)
    *--E301077,82 (End)

    IF lcCurProc < '3'
      *--lcAccMask variable hold the account mask
      lcAccMask = " "
    ENDIF

    SELECT (lcAcCodAl)
    *E300789,1 Hesham (Start)
    *SET ORDER TO TAG COMPID
    SET ORDER TO TAG Accsegno
    *E300789,1 Hesham (End)
    IF lcCurProc < '3'
      *E300789,1 Hesham (Start)
      *IF !SEEK(lcGLCo)
      IF !SEEK("")
        *E300789,1 Hesham (End)
        *-- "Account Mask for Company x not found.
        *--  Unable to continue with release."
        *-- <OK>
        =gfModalGen('INM00309B00000','Dialog',lcGLCo)
        llGoOn = .F.
      ELSE
        lcAccMask  = &lcAcCodAl..Cacsmask
        lcAccMask  = SUBSTR(lcAccMask, LEN(lcSuspense) + 1)
        IF !EMPTY(lcSuspense)
          lcSuspense = lcSuspense + STRTRAN(lcAccMask, "#", "0")
        ENDIF
      ENDIF
    ENDIF

  ENDIF

ENDIF

IF llGoOn
  IF lcGLVer = 'S'
    SELECT (lcSysDaAl)
    SET FILTER TO LEFT(pass2, 1) <> "D" .AND. .NOT. DELETED()
    IF lcCurProc < '3'
      LOCATE FOR SYSID = "GL" + lcGLCo

      lcSuspense = SUBSTR(&lcSysDaAl..link, 36, 24)
    ENDIF
  ENDIF

  *--E301077,82 (Start)
  *USE (lcGlCoDDir+lcCAccFile) IN 0 AGAIN ALIAS (lcAccHrAl) SHARED
  =gfOpenFile(lcGlCoDDir+lcCAccFile,'','SH',@lcAccHrAl,.T.)
  *--E301077,82 (End)


  *-- Check existing of suspense account in chart of account file
  SELECT (lcAcCHrAl)
  *B603503,1 (Begin) Aria or Other.
  *lcAcChrTag = IIF(lcGLVer='A',"ACCTCODE","GLACNT")
  lcAcChrTag = IIF(lcGLVer<>'S',"ACCTCODE","GLACNT")
  *B603503,1 (End)
  SET ORDER TO TAG (lcAcChrTag)

  IF lcCurProc < '3'
    IF EMPTY(lcSuspense)
      *-- You have to setup the ð account major in the GL setup before ð.
      *-- <OK>
      =gfModalGen('INM00229B00000','DIALOG',"suspense|releasing")
      llGoOn     = .F.
      llApRelCon = .F.
    ELSE
      IF !SEEK(lcSuspense)
        *-- "Suspense account x not found in chart
        *--  of account file. Unable to continue with release."
        *-- <OK>
        =gfModalGen('INM00310B00000','DIALOG',lcSuspense)
        llGoOn     = .F.
        llApRelCon = .F.
      ENDIF
    ENDIF
  ENDIF
ENDIF

IF llGoOn
  *-- Open fiscal calendar detail file from linked company system directory

  *--E301077,82 (Start)
  *USE (IIF(lcGLVer='A',lcGlCoDDir,lcSBTSDir)+lcFsPrd) IN 0 AGAIN ;
  ALIAS (lcFsPrdAl) SHARED
*B603503,1 (Begin) Aria or Other.
*=gfOpenFile(IIF(lcGLVer='A',lcGlCoDDir,lcSBTSDir)+lcFsPrd,'','SH',@lcFsPrdAl,.T.)
=gfOpenFile(IIF(lcGLVer<>'S',lcGlCoDDir,lcSBTSDir)+lcFsPrd,'','SH',@lcFsPrdAl,.T.)
*B603503,1 (End)
*--E301077,82 (End)


SELECT (lcFsPrdAl)
*B603503,1 (Begin) Aria or Other.
*IF lcGLVer = 'A'
IF lcGLVer <> 'S'
  *B603503,1 (End)
  SET ORDER TO Comfyrprdi
ENDIF

*-- Set filter
SELECT (lcFsPrdAl)
*B603503,1 (Begin) Aria or Other.
*IF lcGLVer = 'A'
IF lcGLVer <> 'S'
  *B603503,1 (End)
  *--E301077,82 (Start)
  *SET FILTER TO .NOT. EMPTY(Ccomp_id + Cfisfyear + Cfspprdid) ;
  .AND. .NOT. DELETED()
SET FILTER TO .NOT. EMPTY(Cfisfyear + Cfspprdid) .AND. .NOT. DELETED()
*--E301077,82 (End)
ELSE
SET FILTER TO .NOT. EMPTY(compid + yearprd) .AND. .NOT. DELETED()
ENDIF

IF lcCurProc < '3'
  *-- Summary/detail transfer type
  lcDetail = ALLTRIM(UPPER(gfGetMemVar('M_POST_DET',lcComp)))
ENDIF

*-- Open Distribution File
*--E301077,82 (Start)
*IF .NOT. USED(lcGDistAl)
*  USE (lcDataDir+lcDistFile) IN 0 AGAIN ALIAS (lcGDistAl) SHARED
*  SELECT (lcGDistAl)
*  SET ORDER TO Gldistpo           && Posted Index
*ENDIF
*B802792,1 WAB - open the gldist file if the file exist but  if the gldist
*B802792,1 WAB - is not exist put .f. to relase flag (llglrel)
*=gfOpenFile(lcDataDir+lcDistFile,'Gldistpo','SH',@lcGDistAl,.T.)
*B603503,1 (Begin) Add ".DBF".
*IF FILE(lcDataDir+lcDistFile)
IF FILE(lcDataDir+lcDistFile+".DBF")
  *B603503,1 (End)
  =gfOpenFile(lcDataDir+lcDistFile,'Gldistpo','SH',@lcGDistAl,.T.)

  *B605723,1 Amin Stop the next line since it casues a problem if the customer has 
  * Fiscal year different than fiscal calender. [Begin]
  **MH**
  *=SEEK(' ')
  *REPLACE REST GLPERIOD WITH PADL(MONTH(Tran_date),2,'0') WHILE POSTED = ' '
  **MH**
  *B605723,1 Amin Stop the next line since it casues a problem if the customer has 
  * Fiscal year different than fiscal calender. [End]
  

ELSE
  llGLRel = .F.
ENDIF
*B802792,1 WAB - END

*--E301077,82 (End)


*-- Open batch status file for GL company
*--E301077,82 (End)
*USE (lcGLCoDDir+lcBtchFile) IN 0 AGAIN ALIAS (lcBatchAl) SHARED
=gfOpenFile(lcGLCoDDir+lcBtchFile,'','SH',@lcBatchAl,.T.)
*--E301077,82 (End)


*-- IF lcCurProc < '4'  This means that Release to G/L is finished and the
*-- next processing is AP release
IF lcCurProc < '4'
  *-- IF lcCurProc < '3'  This means that uncomplete session is dedected
  *--                    and temp file will be restored, so there is
  *--                    no need to be created
  IF lcCurProc < '3'
    *-- Get a batch temp. file name
    lcBatchTmp = gfTempName()
    SELECT (lcBatchAl)
    = AFIELDS(laFileStru)
    lnFileStru = ALEN(laFileStru,1)
    DIMENSION laFileStru[lnFileStru + 1, 4]
    lnFileStru = lnFileStru + 1
    laFileStru[lnFileStru ,1] = 'nStep'
    laFileStru[lnFileStru ,2] = 'N'
    laFileStru[lnFileStru ,3] = 1
    laFileStru[lnFileStru ,4] = 0
    CREATE TABLE (gcWorkDir + lcBatchTmp) FROM ARRAY laFileStru
    SELECT (lcBatchTmp)
    *B603503,1 (Begin) Aria or Other.
    *IF lcGLVer = 'A'
    IF lcGLVer <> 'S'
      *B603503,1 (End)
      INDEX ON cBatchNo TAG BATCHNO OF (gcWorkDir + lcBatchTmp)
    ENDIF
    USE
    USE (gcWorkDir + lcBatchTmp) SHARED
    *B603503,1 (Begin) Aria or Other.
    *IF lcGLVer = 'A'
    IF lcGLVer <> 'S'
      *B603503,1 (End)
      SET ORDER TO BATCHNO
    ENDIF
  ELSE
    *B603503,1 (Begin) Aria or Other.
    *IF lcGLVer = 'A'
    IF lcGLVer <> 'S'
      *B603503,1 (End)
      SELECT (lcBatchTmp)
      SET ORDER TO BATCHNO
    ENDIF
  ENDIF
ENDIF    &&IF lcCurProc < '4'


*-- Open unposted transaction detail file for GL company
*--E301077,82 (Start)
*USE (lcGlCoDDir+lcDUnPFile) IN 0 AGAIN ALIAS (lcDTrnsAl) SHARED
=gfOpenFile(lcGLCoDDir+lcDUnPFile,'','SH',@lcDTrnsAl,.T.)
*--E301077,82 (End)

*-- IF lcCurProc < '4'  This means that Release to G/L is finished and the
*-- next processing is AP release
IF lcCurProc < '4'
  *-- IF lcCurProc < '3'  This means that uncomplete session is dedected
  *--                    and temp file will be restored, so there is
  *--                    no need to be created
  IF lcCurProc < '3'
    *-- Get a detailed unposted temp. file name
    lcDUnPTmp = gfTempName()
    SELECT (lcDTrnsAl)
    = AFIELDS(laFileStru)
    lnFileStru = ALEN(laFileStru,1)
    DIMENSION laFileStru[lnFileStru + 1, 4]
    lnFileStru = lnFileStru + 1
    laFileStru[lnFileStru ,1] = 'nStep'
    laFileStru[lnFileStru ,2] = 'N'
    laFileStru[lnFileStru ,3] = 1
    laFileStru[lnFileStru ,4] = 0
    CREATE TABLE (gcWorkDir + lcDUnPTmp) FROM ARRAY laFileStru
    SELECT (lcDUnPTmp)
    USE
    USE (gcWorkDir + lcDUnPTmp) SHARED
  ENDIF
ENDIF

*-- Open unposted transaction header file for GL company
*--E301077,82 (Start)
*USE (lcGlCoDDir+lcHUnPFile) IN 0 AGAIN ALIAS (lcHTrnsAl) SHARED
=gfOpenFile(lcGlCoDDir+lcHUnPFile,'','SH',@lcHTrnsAl,.T.)
*--E301077,82 (End)

*-- IF lcCurProc < '4'  This means that Release to G/L is finished and the
*-- next processing is AP release
IF lcCurProc < '4'
  *-- IF lcCurProc < '3'  This means that uncomplete session is dedected
  *--                      and temp file will be restored, so there is
  *--                      no need to be created
  IF lcCurProc < '3'
    *-- Get a detailed unposted temp. file name
    lcHUnPTmp = gfTempName()
    SELECT (lcHTrnsAl)
    = AFIELDS(laFileStru)
    lnFileStru = ALEN(laFileStru,1)
    DIMENSION laFileStru[lnFileStru + 1, 4]
    lnFileStru = lnFileStru + 1
    laFileStru[lnFileStru ,1] = 'nStep'
    laFileStru[lnFileStru ,2] = 'N'
    laFileStru[lnFileStru ,3] = 1
    laFileStru[lnFileStru ,4] = 0
    CREATE TABLE (gcWorkDir + lcHUnPTmp) FROM ARRAY laFileStru
    SELECT(lcHUnPTmp)
    *B603503,1 (Begin) Aria or Other.
    *IF lcGLVer = 'A'
    IF lcGLVer <> 'S'
      *B603503,1 (End)
      INDEX ON cBatchNo + cTranNo TAG BATCHTRN OF (gcWorkDir + lcHUnPTmp)
    ENDIF
    USE
    USE (gcWorkDir + lcHUnPTmp) SHARED
    *B603503,1 (Begin) Aria or Other.
    *IF lcGLVer = 'A'
    IF lcGLVer <> 'S'
      *B603503,1 (End)
      SET ORDER TO BATCHTRN
    ENDIF
  ELSE
    *B603503,1 (Begin) Aria or Other.
    *IF lcGLVer = 'A'
    IF lcGLVer <> 'S'
      *B603503,1 (End)
      SELECT(lcHUnPTmp)
      SET ORDER TO TAG BATCHTRN
    ENDIF
  ENDIF
ENDIF

*B603503,1 (Begin) Aria or Other.
*IF lcGLVer = 'A'
IF lcGLVer <> 'S'
  *B603503,1 (End)
  *--E301077,82 (Start)
  *USE (lcGlCoDDir+'GLSUBJOR') IN 0 AGAIN ALIAS (lcGSJorAl) SHARED
  *SELECT (lcGSJorAl)
  *SET ORDER TO SRCJRNL
  =gfOpenFile(lcGlCoDDir+'GLSUBJOR','SRCJRNL','SH',@lcGSJorAl,.T.)
  *--E301077,82 (End)


  *-- create array used in assigning values to distribution file
  SELECT (lcGSJorAl)
  lnElements = 0			&& Initial value to redimension array laSourceJr
  SCAN
    lnOccur=0
    IF !EMPTY(&lcGSJorAl..mSJTrans)
      lcLine = ALLTRIM(&lcGSJorAl..mSJTrans)
      lnOccur = OCCUR('~',lcLine) + 1
      DECLARE laSourceJr[lnElements+lnOccur,2]
      FOR lnCount = 1 TO lnOccur
        laSourceJr[lnElements+lnCount,1]=;
          IIF(OCCUR('~',lcLine) != 0,SUBSTR(lcLine,1,ATC('~',lcLine,1)-1),lcLine)
        lcLine = SUBSTR(lcLine,ATC('~',lcLine,1) + 1)
        laSourceJr[lnElements+lnCount,2]=&lcGSJorAl..CSRCJRNL
      ENDFOR
    ENDIF
    lnElements=lnElements+lnOccur
  ENDSCAN

  lcSetPthSt = SET("FULLPATH")
  IF llAPRelCon
    *--E301077,82 (Start)
    SET FULLPATH ON
    *IF USED('APDIST')
    *  lcAPDisPth = DBF('APDIST')
    *  USE IN APDIST
    *ENDIF
    *USE (lcGlCoDDir+'APDIST') IN 0 AGAIN SHARED
    *SET ORDER TO Post IN APDIST

    *B605290,1 MHM  12/30/2001 Open right file"child company" [Start]
    *=gfOpenFile(lcGlCoDDir+'APDIST','Post')
    =gfOpenFile(lcDataDir+'APDIST','Post')
    *B605290,1 MHM  [End]
    
    *--E301077,82 (End)

    *--E301077,82 (Start)
    *IF USED('APSETUP')
    *  lcAPSetPth = DBF('APSETUP')
    *  USE IN APSETUP
    *ENDIF
    *USE (lcGlCoDDir+'APSETUP') IN 0 AGAIN SHARED
    =gfOpenFile(lcGlCoDDir+'APSETUP')
    *--E301077,82 (End)

  ENDIF

  *-- Using chart of Account with alias name "lcLinkChar" refers to
  *-- AP main program use Chart of Account with this Alias
  *--E301077,82 (Start)
  *IF USED('lcLinkChar')
  *  lcLnkChPth = DBF('lcLinkChar')
  *  USE IN lcLinkChar
  *ENDIF
  *USE (lcGlCoDDir+lcCAccFile) IN 0 AGAIN ALIAS lcLinkChar SHARED
  *SET ORDER TO AcctCode IN lcLinkChar
  =gfOpenFile(lcGlCoDDir+lcCAccFile,'AcctCode','SH',@lcLinkChar,.T.)
  *--E301077,82 (End)

  SET FULLPATH &lcSetPthSt.
ENDIF

*-- Open G/L transaction types file
*--E301077,82 (Start)
*IF !USED('SyGLTran')
*  llUseByMe = .T.
*  USE (gcSysHome+'SyGLTran') IN 0 SHARE
*  SET ORDER TO SyGLTran IN SyGLTran
*ELSE
*  SET ORDER TO SyGLTran IN SyGLTran
*ENDIF
=gfOpenFile(gcSysHome+'SyGLTran','SyGLTran')
*--E301077,82 (End)


*C037437,1 MHM 02/05/2004 Open Needed Files[End]
IF ASCAN(laEvntTrig,PADR('USEFILES',10))<>0
  =gfDoTriger(lcProgName,PADR('USEFILES',10))
ENDIF
*C037437,1 MHM 02/05/2004 [End]

ENDIF

*!*************************************************************
*! Name      : lpClsAlias
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Release aliases that files are used with.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpClsAlias
*!*************************************************************

PROCEDURE lpClsAlias

IF USED (lcSysDaAl)
  USE IN (lcSysDaAl)
ENDIF
IF USED (lcSequnAl)
  USE IN (lcSequnAl)
ENDIF
IF USED (lcGLSetAl)
  USE IN (lcGLSetAl)
ENDIF
IF USED (lcAcCodAl)
  USE IN (lcAcCodAl)
ENDIF
IF USED (lcAccHrAl)
  USE IN (lcAccHrAl)
ENDIF
IF USED (lcFsPrdAl)
  USE IN (lcFsPrdAl)
ENDIF
IF USED (lcGDistAl)
  USE IN (lcGDistAl)
ENDIF
IF USED (lcBatchAl)
  USE IN (lcBatchAl)
ENDIF
IF USED (lcHTrnsAl)
  USE IN (lcHTrnsAl)
ENDIF
IF USED (lcDTrnsAl)
  USE IN (lcDTrnsAl)
ENDIF
IF USED (lcGSJorAl)
  USE IN (lcGSJorAl)
ENDIF
IF USED (lcInvHdAl)
  USE IN (lcInvHdAl)
ENDIF
IF USED (lcRetHdAl)
  USE IN (lcRetHdAl)
ENDIF
IF USED (lcCutTkAl)
  USE IN (lcCutTkAl)
ENDIF
IF USED (lcPosHdAl)
  USE IN (lcPosHdAl)
ENDIF
IF USED (lcCredtAl)
  USE IN (lcCredtAl)
ENDIF
IF USED (lcDebitAl)
  USE IN (lcDebitAl)
ENDIF

*-- Open APDIST,APSETUP,lccAccFile Files from the original path
IF USED('Sequence')
  SELECT SEQUENCE
  USE
  IF !EMPTY(lcSequePth)
    USE(lcSequePth) SHARED
  ENDIF
ENDIF
*-- Open APDIST,APSETUP,lccAccFile Files from the original path
IF USED('APDIST')
  SELECT APDIST
  USE
  IF !EMPTY(lcAPDisPth)
    USE(lcAPDisPth) SHARED
  ENDIF
ENDIF

IF USED('APSETUP')
  SELECT APSETUP
  USE
  IF !EMPTY(lcAPSetPth)
    USE(lcAPSetPth) SHARED
  ENDIF
ENDIF

IF USED('lcLinkChar')
  SELECT lcLinkChar
  USE
  IF !EMPTY(lcLnkChPth)
    USE(lcLnkChPth) SHARED
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lpLckFiles
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Lock Files.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpLckFiles
*!*************************************************************

PROCEDURE lpLckFiles

PARAMETERS lcTask

PRIVATE lcLocFiles

*-- Attempt lock
DO CASE
CASE lcTask = "SOURCE"
  *-- Lock source files

  *-- Loop to acquire file lock on Distribution file
  *-- Attempt file locks
  llGoOn = gfFlock(lcGDistAl,.T.)
CASE lcTask = "DESTINATION"
  *-- Lock GL destination files; Batch, Header unposted and Detailed unposted
  lcLocFiles = lcBatchAl+','+lcDTrnsAl+','+lcHTrnsAl

  *-- Loop to acquire file lock Distribution files.

  *-- Release locks to prevent deadlock on retries
  llGoOn = gfFlock(lcLocFiles,.F.)

  *-- Attempt file locks
  llGoOn = gfFlock(lcLocFiles,.T.)

ENDCASE

*!*************************************************************
*! Name      : lpRelPrint
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Print posting register and calculate ledger posting balance.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpRelPrint
*!*************************************************************

PROCEDURE lpRelPrint

*-- Declare local variables private
PRIVATE lcHead1, lcHead2

IF lcCurProc < '2'
  *-- "Do you want to print a Posting Register?"
  *-- <Summary>;<Detail>;<No>
  lnResp = gfModalGen('QRM00311B00034','Dialog')
ENDIF

IF lnResp = 1 .OR. lnResp = 2
  *-- Print register, display processing message
  WAIT WINDOW "Preparing to print posting register..." NOWAIT

  *-- Setup report titles
  lcTitle = "ARIA" + IIF(lnResp = 1, " Summary", " Detail") + ;
    " Posting Register"
  lcHead1 = "Release from ARIA company " + lcComp + ;
    " to GL company " + lcGLCo
  lcHead2 = ""
ELSE
  *-- Don't print register, display processing message
  WAIT WINDOW 'Checking ledger posting balance...' NOWAIT
ENDIF

SELECT (lcAcCHrAl)
DO CASE
CASE (lnResp = 1) .OR. (lnResp = 3)
  *-- Summary Report or no report
  *-- Setup session list variables

  lnSessMin = 0
  lnSessMax = 0
  lcTmpFile = gfTempName()

  *-- IF lcCurProc < '2' This means that uncomplete session is dedected
  *--                  and there is no need to craete the temp file and
  *--                  fill it
  IF lcCurProc < '2'
    *-- Create temporary file
    CREATE TABLE (gcWorkDir+lcTmpFile) ;
      (GlAccount  C(24)   , ;
      nEqvAmnt   N(12, 2), ;
      GlFYear    C(4)    , ;
      GlPeriod   C(2))

    *-- Close and reopen temporary file with alias
    USE
    USE (gcWorkDir+lcTmpFile) EXCLUSIVE
    INDEX ON GlFYear + GlPeriod + GlAccount TAG (lcTmpFile) OF ;
      (gcWorkDir+lcTmpFile) COMPACT

    *-- Copy unposted records to temporary file
    SELECT (lcGDistAl)
    SCAN FOR Posted = " " .AND. RECNO(lcGDistAl) <= lnCurrent
      *-- Search for existing record
      SELECT (lcTmpFile)
      SEEK &lcGDistAl..GlFYear + &lcGDistAl..Glperiod + &lcGDistAl..GlAccount
      *-- If existing record not found
      IF .NOT. FOUND()
        *-- Add record to temporary file for report
        APPEND BLANK
        REPLACE GlAccount  WITH &lcGDistAl..GlAccount, ;
          GlFYear    WITH &lcGDistAl..GlFYear, ;
          GlPeriod   WITH &lcGDistAl..GlPeriod
      ENDIF
      *-- Update amount
      REPLACE nEqvAmnt  WITH nEqvAmnt + &lcGDistAl..nEqvAmnt
      =RLOCK()
      UNLOCK
      *-- Update session list variables
      lnSessMin = IIF(lnSessMin = 0, VAL(&lcGDistAl..GlSession), ;
        MIN(lnSessMin, VAL(&lcGDistAl..GlSession)))
      lnSessMax = MAX(lnSessMax, VAL(&lcGDistAl..GlSession))
    ENDSCAN
  ELSE
    SELECT(lcTmpFile)
    SET ORDER TO TAG (lcTmpFile)
  ENDIF
  *-- Update report header from session variables
  lcHead2 = "From Session " + LTRIM(STR(lnSessMin, 6, 0)) + ;
    "-" + LTRIM(STR(lnSessMax, 6, 0))
CASE lnResp = 2
  *-- Detail Report
  *-- Open invoice header file
  *--E301077,82 (Start)
  *USE (lcDataDir+'INVHDR') INDEX(lcDataDir+'INVHDR') ORDER INVHDR IN 0 ;
  ALIAS (lcInvHdAl) SHARED

*B802195 (Start)
*=gfOpenFile(lcDataDir+'INVHDR','INVHDR','SH',@lcInvHdAl,.T.)
IF ('AR' $ SYCCOMP.mModlSet)
  =gfOpenFile(lcDataDir+'INVHDR','INVHDR','SH',@lcInvHdAl,.T.)
ENDIF
*B802195 (End)
*--E301077,82 (End)


*-- Open return header file
*--E301077,82 (Start)
*USE (lcDataDir+'RETHDR') INDEX(lcDataDir+'RETHDR') ORDER RETHDR IN 0 ;
ALIAS (lcRetHdAl) SHARED

*B802195 (Start)
*=gfOpenFile(lcDataDir+'RETHDR','RETHDR','SH',@lcRetHdAl,.T.)
IF ('RM' $ SYCCOMP.mModlSet)
  =gfOpenFile(lcDataDir+'RETHDR','RETHDR','SH',@lcRetHdAl,.T.)
ENDIF
*B802195 (End)
*--E301077,82 (End)

*-- Open cutticket header file
*--E301077,82 (Start)
*USE (lcDataDir+'CUTTKTH') INDEX(lcDataDir+'CUTTKTH') ORDER CUTTKTH IN 0 ;
ALIAS (lcCutTkAl) SHARED

*B802195 (Start)
*=gfOpenFile(lcDataDir+'CUTTKTH','CUTTKTH','SH',@lcCutTkAl,.T.)
IF ('MF' $ SYCCOMP.mModlSet)
  =gfOpenFile(lcDataDir+'CUTTKTH','CUTTKTH','SH',@lcCutTkAl,.T.)
ENDIF
*B802195 (End)
*--E301077,82 (End)

*-- Open style purchase order header file
*--E301077,82 (Start)
*USE (lcDataDir+'POSHDR') INDEX(lcDataDir+'POSHDR') ORDER POSHDR IN 0 ;
ALIAS (lcPosHdAl) SHARED
*B802195 (Start)
*=gfOpenFile(lcDataDir+'POSHDR','POSHDR','SH',@lcPosHdAl,.T.)
IF ('PO' $ SYCCOMP.mModlSet) OR ('PS' $ SYCCOMP.mModlSet)
  =gfOpenFile(lcDataDir+'POSHDR','POSHDR','SH',@lcPosHdAl,.T.)
ENDIF
*B802195 (End)
*--E301077,82 (End)

*-- Open Credit file
*--E301077,82 (Start)
*USE (lcDataDir+'CREDIT') IN 0 ALIAS (lcCredtAl) SHARED
*B802195 (Start)
*=gfOpenFile(lcDataDir+'CREDIT','','SH',@lcCredtAl,.T.)
IF ('AR' $ SYCCOMP.mModlSet)
  =gfOpenFile(lcDataDir+'CREDIT','','SH',@lcCredtAl,.T.)
ENDIF
*B802195 (End)
*--E301077,82 (End)

*-- Open Debit file
*--E301077,82 (Start)
*USE (lcDataDir+'DEBIT') IN 0 ALIAS (lcDebitAl) SHARED
*B802195 (Start)
*=gfOpenFile(lcDataDir+'DEBIT','','SH',@lcDebitAl,.T.)
IF ('AR' $ SYCCOMP.mModlSet)
  =gfOpenFile(lcDataDir+'DEBIT','','SH',@lcDebitAl,.T.)
ENDIF
*B802195 (End)
*--E301077,82 (End)

*-- Select distribution file
SELECT (lcGDistAl)
*-- IF lcCurProc < '2' This means that uncomplete session is dedected
*--                  and there is no need to craete the temp file and
*--                  fill it
IF lcCurProc < '2'
  *-- Create the temporary file
  lcTmpFile = gfTempName()             && Temp. file name
  COPY TO (gcWorkDir+lcTmpFile) ;
    FIELDS ;
    &lcGDistAl..Tran_no, ;
    &lcGDistAl..GlAccount, ;
    &lcGDistAl..GlFYear, ;
    &lcGDistAl..GlPeriod, ;
    &lcGDistAl..nEqvAmnt, ;
    &lcGDistAl..GlSession, ;
    &lcGDistAl..Tran_Date, ;
    &lcGDistAl..Tran_Type ;
    FOR posted = " " .AND. RECNO(lcGDistAl) <= lnCurrent
  *-- Open temporary file
  USE (gcWorkDir+lcTmpFile) IN 0 EXCLUSIVE
ENDIF
ENDCASE

WAIT CLEAR

*-- Select report file
SELECT (lcTmpFile)

IF lnResp = 1 .OR. lnResp = 2
  *-- IF lcCurProc < '2' This means that uncomplete session is dedected
  *--                  and there is no need to create the index
  IF lcCurProc < '2'
    *-- Build index on account number and set relation to accounts file
    INDEX ON GlFYear + GlPeriod + GlAccount TAG (lcTmpFile) OF ;
      (gcWorkDir+lcTmpFile) COMPACT
  ELSE
    SET ORDER TO TAG (lcTmpFile)
  ENDIF

  R_WIDTH    = 'W'        && Report width
  XReport = 'NOTEPAD'
  *DO Sel_Dev
  *wait wind "selecting device to print..."
  IF XReport <> 'ABORT'
    *DO lpSPrint WITH lnResp
  ENDIF

ENDIF && lnResp = 1 .OR. lnResp = 2

*-- Select report file
SELECT (lcTmpFile)

*-B803530,1 Ramy call proc. to print the report [start]
IF lnResp = 1 .OR. lnResp = 2
 =lpPrnPost(lnResp)
ENDIF 
*-B803530,1 Ramy [end]
*-- Calculate balance
CALCULATE SUM (nEqvAmnt) TO lnBalance

*-- Close and delete temporary files
IF USED(lcTmpFile)
  USE IN (lcTmpFile)
ENDIF

ERASE gcWorkDir+lcTmpFile+".dbf"
ERASE gcWorkDir+lcTmpFile+".cdx"

*!*************************************************************
*! Name      : lpAriaRel
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Release GL entries to Aria general ledger..
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpAriaRel
*!*************************************************************

PROCEDURE lpAriaRel

PARAMETERS lnMaxRec

lnCurrent = lnMaxRec
*-- Set index order in chart of accounts file
SET ORDER TO TAG AcctCode IN (lcAcCHrAl)

*-- Declare local variables private
PRIVATE lcDocExp,  lcSumExp, lcNdxExp, lcYearPrd,  lcSess   , ;
  ldGlDate,  lnTDrTot, lnTCrTot,  lnBDrTot,  lnBCrTot , ;
  lcGlTDes,  lnAmount, lcGlAcnt, lcDocKey ,  lcSubDetl, ;
  lcErrMsg, ldPrdBDat, ldPrdEDat

IF lcCurProc <'3'

  *-- *E301024 Add source module to transaction records (Start)
  *-- AAMER 10/05/98

  *-- Create temp file for GLDist File and Build filtered index

  lcTmpFile   = gfTempName()

  *SELECT *,.F. AS Pass,RECNO() AS nRecNo FROM (lcGDistAl) ;
  *  INTO DBF (gcWorkDir+lcTmpFile) ;
  *  WHERE EMPTY(POSTED) ;
  *  AND   RECNO() <= lnMaxRec
  *DO CASE
  *  CASE lcDetail = "S"
  *    *-- Summary
  *    lcSumExp = [.t.]
  *    lcNdxExp = [GlFYear + GlPeriod + GlAccount]
  *    lcDocExp = [""]
  *    lcDocKey = ""
  * CASE lcDetail = "D"
  *    *-- Detail
  *    lcSumExp = lcTmpFile+[.GlAcntType = "C"]
  *    lcNdxExp = [GlFYear + GlPeriod + GlSession + IIF(] + lcSumExp + ;
  *               [, "C", "D") + GlAccount]
  *    *-- Set expression for document number lookup
  *    lcDocExp = lcTmpFile+[.Tran_no]
  *ENDCASE
  *B802411,1 (Start)
  IF EMPTY(lcTrans)
    *B802411,1 (End)
    SELECT *,.F. AS Pass,RECNO() AS nRecNo, lfSorcMod(&lcGDistAl..Tran_Type) AS Csrcmodul ;
      FROM (lcGDistAl) ;
      INTO DBF (gcWorkDir+lcTmpFile) ;
      WHERE EMPTY(POSTED) ;
      AND   RECNO() <= lnMaxRec
    *B802411,1 (Start)
  ELSE
    *B603504,1 (Begin) Select tranasctios by category key not transaction type.
    *B603504,1         Give attention to "013" as it is shared with MA and IC
    *SELECT *,.F. AS Pass,RECNO() AS nRecNo, lfSorcMod(&lcGDistAl..Tran_Type) AS Csrcmodul ;
    FROM (lcGDistAl) ;
    INTO DBF (gcWorkDir+lcTmpFile) ;
    WHERE EMPTY(POSTED) ;
    AND   RECNO() <= lnMaxRec ;
    AND   Tran_Type+Catg_Key $ lcTrans
  *--This condition made because Tran key "013" shares MA and IC.
  lcMatCon = ''
  *--If user selects MA or IC.
  IF !llExclCon
    *--IF he selects MA
    IF llMatMod
      lcMatCon = " AND IIF(Catg_Key = '013',LEFT(tran_type,1)='M',.T.) "
    ELSE
      *--IF he selects IC
      lcMatCon = " AND IIF(Catg_Key = '013',LEFT(tran_type,1)<>'M',.T.) "
    ENDIF
  ENDIF
  SELECT *,.F. AS Pass,RECNO() AS nRecNo, lfSorcMod(&lcGDistAl..Tran_Type) AS Csrcmodul ;
    FROM (lcGDistAl) ;
    INTO DBF (gcWorkDir+lcTmpFile) ;
    WHERE EMPTY(POSTED) ;
    AND   RECNO() <= lnMaxRec ;
    AND   Catg_Key $ lcTrans &lcMatCon
  *B603504,1 (End)
ENDIF
*B802411,1 (End)

*-- Setup summarization and index key expressions

DO CASE
CASE lcDetail = "S"
  *-- Summary
  lcSumExp = [.t.]
  lcNdxExp = [GlFYear + GlPeriod + Csrcmodul + GlAccount]
  lcDocExp = [""]
  lcDocKey = ""

CASE lcDetail = "D"
  *-- Detail
  lcSumExp = lcTmpFile+[.GlAcntType = "C"]
  lcNdxExp = [GlFYear + GlPeriod + Csrcmodul + GlSession + IIF(] + lcSumExp + ;
    [, "C", "D") + GlAccount]

  *-- Set expression for document number lookup
  lcDocExp = lcTmpFile+[.Tran_no]

ENDCASE
*-- E301024 Add source module to transaction records (End)

SELECT(lcTmpFile)
*-- E301024 Create index in lctmpfile on batch+tran (Start)
INDEX ON glbatch+cTrnsLedn TAG BatTrn OF &gcWorkDir.&lcTmpFile COMPACT
*-- E301024 Create index in lctmpfile on batch+tran (End)

INDEX ON &lcNdxExp + &lcDocExp TAG (lcTmpFile) OF ;
  &gcWorkDir.&lcTmpFile COMPACT ADDITIVE
*B603504,1 (Begin) Create a new index based on TranNo to get the tran_date for the current
*B603504,1         transction if it has any line having Tran_date otherwise browse a screen for
*B603504,1         the user to enable him to enter a date for the current transaction and upfdate
*B603504,1         all the lines ot it by this entedred date.
INDEX ON Tran_Type+tran_no TAG tran_no OF &gcWorkDir.&lcTmpFile COMPACT ADDITIVE
*B603504,1 (End)

USE
USE (gcWorkDir+lcTmpFile) SHARED
SET ORDER TO TAG (lcTmpFile)

IF llGoOn AND lcCurProc < '3'
  lcFiles = "lcBatchTmp," + lcBatchTmp + "," + ORDER(lcBatchTmp) + ";" + ;
    "lcHUnPTmp,"  + lcHUnPTmp  + "," + ORDER(lcHUnPTmp)  + ";" + ;
    "lcDUnPTmp,"  + lcDUnPTmp  + "," + ORDER(lcDUnPTmp)  + ";" + ;
    "lcTmpFile,"  + lcTmpFile  + "," + ORDER(lcTmpFile)  + ";"
  lcCurProc = '2'
  llNoThing = lfUpdUnCmS("Open", lcCurProc)
ENDIF

*-- Set variavles to get the transaction no. and the batch no.
lcTranNo  = 0
lcBatchNo = 0

*-- Post to GL files
GOTO TOP

lnRecCount = RECCOUNT()
lnCount    = 0
DO WHILE .NOT. EOF()  .AND. llGoOn

  *B603504,1 (Begin) Call lfGetPerd() to get the period and year if both or date are empty.
  *-- If the function change anything in the file, loop. Refer to documentation in the function.
  IF lfGetPerd()
    LOOP
  ENDIF
  *B603504,1 (End)


  *-- Get batch number
  lcBatchNo = lcBatchNo + 1
  *-- Check the Year / Period

  DO lpPrdVal WITH &lcTmpFile..GlFYear,&lcTmpFile..GlPeriod
  *-- Do not continue if the Year/Period is not valid
  IF !llGoOn
    EXIT
  ENDIF

  *-- Save year period number for batch
  lcYearPrd = &lcTmpFile..GlFYear + &lcTmpFile..GlPeriod

  *-- E301024 lcSorcMod variable that hold transaction source module (Start)
  lcSorcMod = &lcTmpFile..cSrcModul
  *-- E301024 lcSorcMod variable that hold transaction source module (End)

  *-- Get period begin and end date
  *--E301077,82 (Start)
  *IF SEEK(lcGLCo+lcYearPrd,lcFsPrdAl)
  IF SEEK(lcYearPrd,lcFsPrdAl)
    *--E301077,82 (End)
    ldPrdBDat = &lcFsPrdAl..dFsPpBgDt
    ldPrdEDat = &lcFsPrdAl..dFsPpEnDt
  ENDIF

  *-- Add batch status record to GLBTCH temporary file and
  *-- leave marked with object lock

  SELECT &lcBatchTmp
  APPEND BLANK
  REPLACE Cbatchno  WITH PADL(lcBatchNo,FSIZE('CBATCHNO'),'0'), ;
    Cbatdesc  WITH "Posting from ARIA, " + lcComDesc, ;
    Cbatpyr   WITH &lcTmpFile..GlFYear, ;
    Dbatpbeg  WITH ldPrdBDat, ;
    Dbatpend  WITH ldPrdEDat, ;
    Cbatrefer WITH "On "+ DTOC(DATE()), ;
    Cbattype  WITH "L", ;
    Cbatstat  WITH "U", ;
    Csrcmodul WITH &lcTmpFile..cSrcModul ,;
    Ccomp_id  WITH lcComp
  =RLOCK()
  UNLOCK

  *-- Process records for batch
  SELECT (lcTmpFile)

  *-- E301024 Add source module to transaction records (Start)
  *DO WHILE &lcTmpFile..GlFYear + &lcTmpFile..GlPeriod = lcYearPrd ;
  .AND. .NOT. EOF() .AND. llGoOn

DO WHILE &lcTmpFile..GlFYear + &lcTmpFile..GlPeriod + &lcTmpFile..cSrcModul = ;
    lcYearPrd + lcSorcMod ;
    .AND. .NOT. EOF() .AND. llGoOn


  *-- Setup session ID and date field values for GLHUNP update
  lcSess   = IIF(lcDetail = "D", &lcTmpFile..GlSession, "")
  ldGlDate = &lcTmpFile..Tran_date

  *-- Get transaction number
  lcTranNo = lcTranNo + 1
  *E301024 Decalre lnEntryNo to hold entery No. (Start)
  lnEntryNo = 0
  *E301024 Decalre lnEntryNo to hold entery No. (End)

  *-- Setup accumulator variables for batch
  lnTDrTot = 0
  lnTCrTot = 0

  *-- Assign lcCsrcjrnl with code in array laSourceJr
  *-- or default code if not found, Seeking is by
  *-- Tran_Type in temporary GLDist file.
  lcCsrcjrnl = 'GJ'
  IF !EMPTY(lcSess)
    lcSrExact = SET("EXACT")
    SET EXACT ON
    lnFnItem = CEILING(ASCAN(laSourceJr,&lcTmpFile..Tran_Type)/2)
    lcCsrcjrnl = IIF(lnFnItem = 0,'GJ',laSourceJr[lnFnItem,2])
    SET EXACT &lcSrExact
  ENDIF

  *-- Add transaction header record to GLHUNP
  SELECT &lcHUnPTmp
  APPEND BLANK
  REPLACE Cbatchno   WITH PADL(lcBatchNo,FSIZE('CBATCHNO'),'0'), ;
    Ctranno    WITH PADL(lcTranNo,FSIZE('CTRANNO'),'0'), ;
    Ctrndesc   WITH "Posting from ARIA, " + lcComDesc , ;
    Ctrnrefer  WITH "On "+ DTOC(DATE()), ;
    Dtrnpdate  WITH ldGlDate, ;
    Ctrnpyr    WITH SUBSTR(lcYearPrd,1,4), ;
    Ctrnpprd   WITH SUBSTR(lcYearPrd,5,2), ;
    Ctrnstat   WITH "U", ;
    Ctrntype   WITH "N", ;
    Csrcmodul  WITH &lcTmpFile..cSrcModul, ;
    Ccomp_id   WITH lcComp, ;
    Ctrnrever  WITH "N", ;
    Cstandard  WITH "Y", ;
    Csrcjrnl   WITH lcCsrcjrnl
  =RLOCK()
  UNLOCK

  *-- Process records for transaction
  SELECT (lcTmpFile)

  *-- E301024 Add source module to transaction records (Start)
  *DO WHILE &lcTmpFile..GlFYear + &lcTmpFile..GlPeriod = lcYearPrd ;
  .AND. IIF(EMPTY(lcSess), .T., (&lcTmpFile..GlSession = ;
  lcSess)) .AND. .NOT. EOF()

DO WHILE &lcTmpFile..GlFYear + &lcTmpFile..GlPeriod + ;
    &lcTmpFile..cSrcModul = lcYearPrd + lcSorcMod ;
    .AND. IIF(EMPTY(lcSess), .T., (&lcTmpFile..GlSession = ;
    lcSess)) .AND. .NOT. EOF()
  *-- E301024 Add source module to transaction records (End)

  *-- Setup account number
  lcGlAcnt = &lcTmpFile..GlAccount

  *-- Store gl account
  lcTGlAcnt = lcGlAcnt

  *-- Check this account
  DO lpAccVal WITH 'lcGlAcnt'

  IF ! llGoOn
    EXIT
  ENDIF

  *-- Setup subledger link field value for summary posting
  lcSubDetl = ""

  *-- Setup document summarization key value
  lcDocKey = &lcDocExp


  lcTmpStr = "Year/Period: " + ;
    LEFT(lcYearPrd, 4) + "/" + RIGHT(lcYearPrd, 2) + ;
    ", account " + ALLTRIM(lcGlAcnt)
  lnCount = lnCount + 1
  = gfThermo(lnReccount,lnCount,'Generating...',lcTmpStr)


  *-- Setup description
  lcGlTDes = "Transaction #: " + LTRIM(Tran_No)

  *-- If detail posting
  IF .NOT. (lcDetail = "S" .OR. (lcDetail = "D" ;
      .AND. &lcSumExp.))
    *-- Setup subledger link field value
    lcSubDetl = lcDocKey
  ENDIF

  *E301024 Increment lnEntryNo (Start)
  lnEntryNo = lnEntryNo + 1
  *E301024 Increment lnEntryNo (End)

  *-- Setup amount and move to next source record to be processed
  *-- Mark record with batch number and GL company number
  IF lcDetail = "S" .OR. (lcDetail = "D" .AND. &lcSumExp)
    *-- Summary, or detail and on control account

    *-- Transaction description should be blank for summarized totals

    *B125803,1 MHM 04/06/2005 dont update Desc. only in case of summrize [Start]
    IF lcDetail = "S"
    *B125803,1 MHM 04/06/2005  [End]

      lcGlTDes = " "

    *B125803,1 MHM 04/06/2005 dont update Desc. only in case of summrize [Start]
    ENDIF  
    *B125803,1 MHM 04/06/2005  [End]

    *-- Accumulate amount while on same period/account/session/control
    lnAmount = 0

    *-- E301024 Add source module to transaction records (Start)
    *SCAN WHILE GlFYear + GlPeriod = lcYearPrd .AND. GlAccount = lcTGlAcnt ;
    .AND. IIF(EMPTY(lcSess), .T., (&lcTmpFile..GlSession = ;
    lcSess)) .AND. &lcSumExp.

  SCAN WHILE GlFYear + GlPeriod + cSrcModul = lcYearPrd + lcSorcMod .AND. GlAccount = lcTGlAcnt ;
      .AND. IIF(EMPTY(lcSess), .T., (&lcTmpFile..GlSession = ;
      lcSess)) .AND. &lcSumExp.
    *-- E301024 Add source module to transaction records (End)

    *-- Update batch and company numbers
    *E301024 update glbatch, cTrnsLedn , nEntryNo (Start)
    *REPLACE glbatch   WITH PADL(lcBatchNo,FSIZE('GLBATCH'),'0')

    REPLACE glbatch   WITH PADL(lcBatchNo,FSIZE('GLBATCH'),'0') ,;
      nEntryNo  WITH lnEntryNo ,;
      cTrnsLedn WITH PADL(lcTranNo,FSIZE('cTrnsLedn'),'0')
    *E301024 update glbatch, cTrnsLedn , nEntryNo (End)

    =RLOCK()
    UNLOCK

    *-- Increment amount
    lnAmount = lnAmount + &lcTmpFile..nEqvamnt
  ENDSCAN
ELSE
  *-- Detail and not on control account

  *-- Accumulate amount while on same period/account/session/document
  lnAmount = 0

  *-- E301024 Add source module to transaction records (Start)
  *SCAN WHILE GlFYear + GlPeriod = lcYearPrd .AND. GlAccount = lcTGlAcnt ;
  .AND. IIF(EMPTY(lcSess), .T., (&lcTmpFile..GlSession = ;
  lcSess)) .AND. &lcDocExp. = lcDocKey

SCAN WHILE GlFYear + GlPeriod + cSrcModul = lcYearPrd + lcSorcMod .AND. GlAccount = lcTGlAcnt ;
    .AND. IIF(EMPTY(lcSess), .T., (&lcTmpFile..GlSession = ;
    lcSess)) .AND. &lcDocExp. = lcDocKey
  *-- E301024 Add source module to transaction records (End)

  *-- Update batch and company numbers

  *E301024 update glbatch, cTrnsLedn , nEntryNo (Start)
  *REPLACE glbatch   WITH PADL(lcBatchNo,FSIZE('GLBATCH'),'0')

  REPLACE glbatch   WITH PADL(lcBatchNo,FSIZE('GLBATCH'),'0') ,;
    nEntryNo  WITH lnEntryNo ,;
    cTrnsLedn WITH PADL(lcTranNo,FSIZE('cTrnsLedn'),'0')
  *E301024 update glbatch, cTrnsLedn , nEntryNo (End)
  =RLOCK()
  UNLOCK

  *-- Increment amount
  lnAmount = lnAmount + &lcTmpFile..nEqvAmnt

ENDSCAN
ENDIF && lcDetail = "N" .OR. etc.

*-- Add transaction detail record to GLDUNP
SELECT &lcDUnPTmp
APPEND BLANK
*E301024 also replace nEntryNo with lnEntryNo in transactions detail (Start)
*REPLACE Cacctcode  WITH lcGlAcnt, ;
Cbatchno   WITH PADL(lcBatchNo,FSIZE('CBATCHNO'),'0'), ;
Ctranno    WITH PADL(lcTranNo,FSIZE('CTRANNO'),'0'), ;
Ctrdtexp   WITH lcGlTDes, ;
Dtrnpdate  WITH ldGlDate, ;
Namount    WITH ABS(lnAmount), ;
Cdrorcr    WITH IIF(lnAmount < 0,'C','D'), ;
Ctrnpyr    WITH SUBSTR(lcYearPrd,1,4) , ;
Ctrnpprd   WITH SUBSTR(lcYearPrd,5,2)

REPLACE Cacctcode  WITH lcGlAcnt, ;
  Cbatchno   WITH PADL(lcBatchNo,FSIZE('CBATCHNO'),'0'), ;
  Ctranno    WITH PADL(lcTranNo,FSIZE('CTRANNO'),'0'), ;
  Ctrdtexp   WITH lcGlTDes, ;
  Dtrnpdate  WITH ldGlDate, ;
  Namount    WITH ABS(lnAmount), ;
  Cdrorcr    WITH IIF(lnAmount < 0,'C','D'), ;
  Ctrnpyr    WITH SUBSTR(lcYearPrd,1,4) , ;
  Ctrnpprd   WITH SUBSTR(lcYearPrd,5,2) ,;
  nEntryNo   WITH lnEntryNo
*E301024 also replace nEntryNo with lnEntryNo in transactions detail (End)
=RLOCK()
UNLOCK

*-- Increment debit and credit totals for transaction and batch
lnTDrTot = lnTDrTot + IIF(Cdrorcr = 'D', Namount, 0)
lnTCrTot = lnTCrTot + IIF(Cdrorcr = 'C', Namount, 0)

*-- Already on next detail record
SELECT (lcTmpFile)
ENDDO

IF WEXIST('gwdThermo')
  = gfThermo(lnReccount,lnReccount,'Generating...',lcTmpStr)
ENDIF

*-- If transaction is out of balance
IF lnTDrTot <> lnTCrTot .AND. llGoOn

  *-- Setup out of balance amount
  lnAmount = lnTCrTot - lnTDrTot

  *-- Display alert

  *-- "Batch xxx is out of balance by $ 99.99. An offsetting amount
  *--  posted to account xxxxxxx"
  *-- <OK>
  lcTmpStr = ALLTRIM(STR(lcBatchNo)) + "|" + LTRIM(STR(lnAmount, 10, 2)) +;
    "|" + ALLTRIM(lcSuspense)
  = gfModalGen("INM00305B00000","Dialog",lcTmpStr)

  *-- Add suspense transaction detail record to GLDUNP
  SELECT &lcDUnPTmp
  APPEND BLANK
  REPLACE Cacctcode  WITH lcSuspense, ;
    Cbatchno   WITH PADL(lcBatchNo,FSIZE('CBATCHNO'),'0'), ;
    Ctranno    WITH PADL(lcTranNo,FSIZE('CTRANNO'),'0'), ;
    Ctrdtexp   WITH "Suspense - Out of Balance", ;
    Dtrnpdate  WITH ldGlDate, ;
    Namount    WITH ABS(lnAmount), ;
    Cdrorcr    WITH IIF(lnAmount < 0, "C", "D"), ;
    Ctrnpyr    WITH SUBSTR(lcYearPrd,1,4) , ;
    Ctrnpprd   WITH SUBSTR(lcYearPrd,5,2)
  =RLOCK()
  UNLOCK
ENDIF
ENDDO
SELECT (lcTmpFile)
ENDDO
ELSE
SELECT (lcTmpFile)
SET ORDER TO TAG (lcTmpFile)
ENDIF

IF llGoOn AND lcCurProc < '3'
  *-- E301024 make no active index (Start)
  SELECT (lcTmpFile)
  SET ORDER TO 0
  *-- E301024 make no active index (End)

  lcCurProc = '3'
  llNoThing = lfUpdUnCmS("Open",lcCurProc)
ENDIF

*-- If not cancelled
IF llGoOn

  *-- Now update master files

  *-- Call routine to lock GL destination files
  DO lpLckFiles WITH "DESTINATION"

  *-- IF locking files success
  IF llGoOn

    *-- Set indeses and relations
    SELECT &lcHUnPTmp
    SET ORDER TO TAG BATCHTRN

    SELECT &lcBatchTmp
    SET ORDER TO TAG BATCHNO

    SELECT &lcDUnPTmp
    SET RELATION TO &lcDUnPTmp..cBatchNo + &lcDUnPTmp..cTranNo ;
      INTO &lcHUnPTmp ADDITIVE

    SELECT &lcHUnPTmp
    SET RELATION TO &lcHUnPTmp..cBatchNo INTO &lcBatchTmp ADDITIVE

    *-- Initialize batch and transaction numbers
    lcBatKey = ""
    lcTrnkey = ""
    *-- Go through header unposted file
    SELECT &lcDUnPTmp
    lnRecCount = RECCOUNT()
    lnCount    = 0
    SCAN

      *-- New batch
      IF lcBatKey <> &lcDUnPTmp..cBatchno
        lcBatKey = &lcDUnPTmp..cBatchno

        IF &lcDUnPTmp..nStep < 1
          *-- Get new bacth number
          lcBatchNo = gfSequence("cBatchNo",lcGLCo)
          *-- Update Distribution file with batch number and company ID
          *-- and mark this batch as posted.
          SELECT (lcTmpFile)

          REPLACE ALL GLBATCH WITH lcBatchNo, ;
            GLCOMP WITH lcGLCo,  ;
            POSTED WITH "X" ;
            FOR &lcTmpFile..GLBATCH = &lcDUnPTmp..cBatchno
          =RLOCK()
          UNLOCK
        ENDIF

        IF &lcBatchTmp..nStep < 1
          *-- Update batch master file
          SELECT &lcBatchTmp
          SCATTER MEMVAR MEMO
          m.cBatchno  = lcBatchNo
          m.nBatCnTot = 0
          m.nBatotDr  = 0
          m.nBatotCr  = 0
          m.cAdd_user = gcUser_Id
          m.dAdd_date = gdSysDate
          m.cAdd_time = gfGettime()

          SELECT (lcBatchAl)
          APPEND BLANK
          GATHER MEMVAR MEMO
          =RLOCK()
          UNLOCK

          SELECT(lcBatchTmp)
          REPLACE nStep WITH 1
          =RLOCK()
          UNLOCK
        ENDIF

        *-- Initialize batch totals
        lnBDrTot = 0
        lnBCrTot = 0
      ENDIF

      *-- New transaction
      IF lcTrnKey <> &lcDUnPTmp..cTranno
        lcTrnKey = &lcDUnPTmp..cTranno

        *-- Update header unposted master file
        IF &lcHUnPTmp..nStep<1

          *-- Get new transaction number
          lcTranNo = gfSequence('cTranNo',lcGLCo)

          lcTmpStr = "Batch (" +lcBatchNo+ ") Transaction (" +lcTranNo+ ")"
          lnCount = lnCount + 1
          = gfThermo(lnReccount,lnCount,'Releasing...',lcTmpStr)

          *E301024 update tempfile (gldist) with the generated tran. No(Start)
          SELECT (lcTmpFile)
          REPLACE ALL cTrnsLedn WITH lcTranNo ;
            FOR GLBatch   + cTrnsLedn = ;
            lcBatchNo + &lcDUnPTmp..cTranno
          *E301024 update tempfile (gldist) with the generated tran. No(End)

          SELECT &lcHUnPTmp
          SCATTER MEMVAR MEMO
          m.cBatchno  = lcBatchNo
          m.cTranno   = lcTranNo
          m.nTrnTotDr = 0
          m.nTrnTotCr = 0
          m.cAdd_user = gcUser_Id
          m.dAdd_date = gdSysDate
          m.cAdd_time = gfGettime()

          SELECT (lcHTrnsAl)
          APPEND BLANK
          GATHER MEMVAR MEMO
          =RLOCK()
          UNLOCK

          SELECT(lcHUnPTmp)
          REPLACE nStep WITH 1
          =RLOCK()
          UNLOCK
        ENDIF

        *-- Initialize transaction totals
        lnTDrTot = 0
        lnTCrTot = 0
      ENDIF

      *-- Accumulate batch and transaction totals
      DO CASE
      CASE &lcDUnPTmp..cDrorCr = 'D'
        lnTDrTot =  lnTDrTot + &lcDUnPTmp..nAmount
        lnBDrTot =  lnBDrTot + &lcDUnPTmp..nAmount
      CASE &lcDUnPTmp..cDrorCr = 'C'
        lnTCrTot =  lnTCrTot + &lcDUnPTmp..nAmount
        lnBCrTot =  lnBCrTot + &lcDUnPTmp..nAmount
      ENDCASE

      *-- Update detailed unposted master file
      IF &lcDUnPTmp..nStep<1
        SELECT &lcDUnPTmp
        SCATTER MEMVAR MEMO
        m.cBatchno  = lcBatchNo
        m.cTranno   = lcTranNo
        m.cAdd_user = gcUser_Id
        m.dAdd_date = gdSysDate
        m.cAdd_time = gfGettime()

        SELECT (lcDTrnsAl)
        APPEND BLANK
        GATHER MEMVAR MEMO
        =RLOCK()
        UNLOCK

        SELECT(lcDUnPTmp)
        REPLACE nStep WITH 1
        =RLOCK()
        UNLOCK

      ENDIF

      *-- Update header unposted file with transaction totals
      SELECT (lcHTrnsAl)
      REPLACE nTrnTotDr WITH lnTDrTot, ;
        nTrnTotCr WITH lnTCrTot
      =RLOCK()
      UNLOCK

      *-- Update batch file with batch totals
      SELECT (lcBatchAl)
      REPLACE nBatCnTot WITH lnBDrTot, ;
        nBatotDr  WITH lnBDrTot, ;
        nBatotCr  WITH lnBCrTot
      =RLOCK()
      UNLOCK

    ENDSCAN

    IF WEXIST('gwdThermo')
      = gfThermo(lnReccount,lnReccount,'Releasing...',lcTmpStr)
    ENDIF

    SELECT (lcTmpFile)
    lnRecCount = RECCOUNT()
    lnCount    = 0
    SCAN
      IF !&lcTmpFile..Pass
        SCATTER MEMVAR MEMO
        SELECT (lcGDistAl)
        GOTO nRecNo
        GATHER MEMVAR MEMO
        =RLOCK()
        UNLOCK
        SELECT (lcTmpFile)
        REPLACE Pass WITH .T.
        =RLOCK()
        UNLOCK
      ENDIF

      lnCount = lnCount + 1
      = gfThermo(lnReccount,lnCount,'Updating G/L Distribution...')

    ENDSCAN

    IF WEXIST('gwdThermo')
      = gfThermo(lnReccount,lnReccount,'Updating G/L Distribution...')
    ENDIF

  ENDIF &&IF llGoOn

  *-- Unlock master files
  lcLocFiles = lcBatchAl+','+lcDTrnsAl+','+lcHTrnsAl
  *-- Release locks to prevent deadlock on retries
  = gfFlock(lcLocFiles,.F.)

ENDIF

*-- Erase temporary files and indeses
USE IN &lcTmpFile
ERASE gcWorkDir+lcTmpFile+".dbf"
ERASE gcWorkDir+lcTmpFile+".cdx"

USE IN &lcBatchTmp
ERASE gcWorkDir+lcBatchTmp+".dbf"
ERASE gcWorkDir+lcBatchTmp+".cdx"

USE IN &lcHUnPTmp
ERASE gcWorkDir+lcHUnPTmp+".dbf"
ERASE gcWorkDir+lcHUnPTmp+".cdx"

USE IN &lcDUnPTmp
ERASE gcWorkDir+lcDUnPTmp+".dbf"
ERASE gcWorkDir+lcDUnPTmp+".cdx"

SELECT (lcGDistAl)
SET ORDER TO TAG GlDistPo IN (lcGDistAl)

*!*************************************************************
*! Name      : lpSBTRel
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Release GL entries to SBT general ledger..
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpSBTRel
*!*************************************************************

PROCEDURE lpSBTRel
PARAMETERS lnMaxRec

lnCurrent = lnMaxRec

*-- Set index order in chart of accounts file
SET ORDER TO TAG glacnt IN &lcAcCHrAl

*-- Declare local variables private
PRIVATE lcDocExp,  lcSumExp, lcNdxExp, lcYearPrd, lcSess, ;
  ldGlDate,  lnTDrTot, lnTCrTot, lnBDrTot,  lnBCrTot, ;
  lcGlTDes,  lnAmount, lcGlAcnt, lcDocKey,  lcSubDetl, tmpFile, ;
  lcErrMsg1, lcErrMsg2

IF lcCurProc <'3'
  *-- Create temp file for GLDist File and Build filtered index
  lcTmpFile   = gfTempName()

  *-- E301024 Add source module to transaction records (Start)
  *SELECT *,.F. AS Pass,RECNO() AS nRecNo FROM (lcGDistAl) ;
  INTO DBF (gcWorkDir+lcTmpFile) ;
  WHERE EMPTY(POSTED) ;
  AND   RECNO() <= lnMaxRec

**-- Setup summarization and index key expressions
*DO CASE
*  CASE lcDetail = "S"
*    *-- Summary
*    lcSumExp = [.T.]
*    lcNdxExp = [GlFYear + GlPeriod + GlAccount]
*    lcDocExp = [""]
*    lcDocKey = ""

*  CASE lcDetail = "D"
*    *-- Detail
*    lcSumExp = lcTmpFile + [.GlAcntType = "C"]
*    lcNdxExp = [GlFYear + GlPeriod + GlSession + IIF(] + lcSumExp + ;
*               [, "C", "D") + GlAccount]

*   *-- Set expression for document number lookup
*    lcDocExp = lcTmpFile + [.Tran_no]

*ENDCASE
SELECT *,.F. AS Pass,RECNO() AS nRecNo,lfSorcMod(&lcGDistAl..Tran_Type) AS Csrcmodul;
  FROM (lcGDistAl) ;
  INTO DBF (gcWorkDir+lcTmpFile) ;
  WHERE EMPTY(POSTED) ;
  AND   RECNO() <= lnMaxRec
*-- Setup summarization and index key expressions
DO CASE
CASE lcDetail = "S"
  *-- Summary
  lcSumExp = [.T.]
  lcNdxExp = [GlFYear + GlPeriod + cSrcModul + GlAccount]
  lcDocExp = [""]
  lcDocKey = ""

CASE lcDetail = "D"
  *-- Detail
  lcSumExp = lcTmpFile + [.GlAcntType = "C"]
  lcNdxExp = [GlFYear + GlPeriod + cSrcModul + GlSession + IIF(] + lcSumExp + ;
    [, "C", "D") + GlAccount]

  *-- Set expression for document number lookup
  lcDocExp = lcTmpFile + [.Tran_no]

ENDCASE
*-- E301024 Add source module to transaction records (End)

*-- Build filtered index
SELECT (lcTmpFile)
*-- E301024 Create index in lctmpfile on batch+tran
INDEX ON glbatch+cTrnsLedn TAG BatTrn OF &gcWorkDir.&lcTmpFile COMPACT

INDEX ON &lcNdxExp + &lcDocExp TAG (lcTmpFile) OF ;
  &gcWorkDir.&lcTmpFile COMPACT ADDITIVE
USE
USE (gcWorkDir+lcTmpFile) SHARED
SET ORDER TO TAG (lcTmpFile)

IF llGoOn AND lcCurProc < '3'
  lcFiles = "lcBatchTmp," + lcBatchTmp + "," + ORDER(lcBatchTmp) + ";" + ;
    "lcHUnPTmp,"  + lcHUnPTmp  + "," + ORDER(lcHUnPTmp)  + ";" + ;
    "lcDUnPTmp,"  + lcDUnPTmp  + "," + ORDER(lcDUnPTmp)  + ";" + ;
    "lcTmpFile,"  + lcTmpFile  + "," + ORDER(lcTmpFile)  + ";"
  lcCurProc = '2'
  llNoThing = lfUpdUnCmS("Open", lcCurProc)
ENDIF

*-- Post to GL files
GOTO TOP
lnRecCount = RECCOUNT()
lnCount    = 0
DO WHILE .NOT. EOF()
  *-- Save year period number for batch
  lcYearPrd = &lcTmpFile..GlFYear + &lcTmpFile..GlPeriod

  *-- E301024 lcSorcMod variable that hold transaction source module (Start)
  lcSorcMod = &lcTmpFile..cSrcModul
  *-- E301024 lcSorcMod variable that hold transaction source module (End)

  IF !&lcTmpFile..Pass
    *-- Get batch number
    lcBatchNo = lfGetGlBt(lcGLCo)

    *-- lfGetGlBt could not lock record in gldist file
    *-- so terminate the process
    IF !llGoOn
      *-- "cannot complete the process, record is used by another"
      *-- <OK>
      = gfModalGen("INM00304B00000","Dialog")
      EXIT
    ENDIF

  ENDIF

  *-- Add batch status record to GLBTCH temporary file and
  *-- leave marked with object lock
  SELECT (lcBatchTmp)
  APPEND BLANK
  REPLACE batchno  WITH lcBatchNo, ;
    bdesc    WITH "Posting from ARIA, " + lcComDesc, ;
    yearprd  WITH lcYearPrd, ;
    bref     WITH "", ;
    lowseq   WITH "", ;
    uppseq   WITH "", ;
    btype    WITH "Z", ;
    bstat    WITH "", ;
    ctrltot  WITH 0, ;
    drtotal  WITH 0, ;
    crtotal  WITH 0, ;
    stsuser  WITH gcUser_Id, ;
    stsdate  WITH gdSysDate, ;
    ststime  WITH gfGettime(), ;
    edtuser  WITH "", ;
    edtdate  WITH {""}, ;
    edttime  WITH "", ;
    auduser  WITH "", ;
    auddate  WITH {""}, ;
    audtime  WITH "", ;
    pstuser  WITH "", ;
    pstdate  WITH {""}, ;
    psttime  WITH "", ;
    post1    WITH "", ;
    post2    WITH "", ;
    post3    WITH "", ;
    post4    WITH "", ;
    post5    WITH "", ;
    post6    WITH "", ;
    post7    WITH "", ;
    post8    WITH "", ;
    adduser  WITH gcUser_Id, ;
    adddate  WITH gdSysDate, ;
    addtime  WITH gfGettime(), ;
    lckstat  WITH "L", ;
    lckuser  WITH gcUser_Id, ;
    lckdate  WITH gdSysDate, ;
    lcktime  WITH gfGettime()

  *-- Setup accumulator variables for batch
  lnBDrTot = 0
  lnBCrTot = 0
  lcLowSeq = SPACE(6)
  lcUppSeq = SPACE(6)

  *-- Process records for batch
  SELECT (lcTmpFile)

  *-- E301024 Add source module to transaction records (Start)
  *DO WHILE &lcTmpFile..GlFYear + &lcTmpFile..GlPeriod = lcYearPrd ;
  .AND. .NOT. EOF()

DO WHILE &lcTmpFile..GlFYear + &lcTmpFile..GlPeriod + ;
    &lcTmpFile..cSrcModul  = lcYearPrd + lcSorcMod ;
    .AND. .NOT. EOF()
  *-- E301024 Add source module to transaction records (End)

  *-- Setup session ID and date field values for GLHUNP update
  lcSess   = IIF(lcDetail = "D", &lcTmpFile..GlSession, "")
  ldGlDate = &lcTmpFile..tran_date

  IF !&lcTmpFile..Pass
    *-- Get transaction number
    lcTranNo = lfGetGlTr(lcGLCo)
    IF !llGoOn
      *-- "cannot complete the process, record is used by another"
      *-- <OK>
      = gfModalGen("INM00304B00000","Dialog")
      EXIT
    ENDIF
  ENDIF

  *-- Update upper and lower transaction numbers for batch record
  lcLowSeq = IIF(EMPTY(lcLowSeq), lcTranNo, MIN(lcTranNo, lcLowSeq))
  lcUppSeq = MAX(lcTranNo, lcUppSeq)

  *-- Setup accumulator variables for batch
  lnTDrTot = 0
  lnTCrTot = 0

  *E301024 here reintialize entry no.
  lnEntryNo = 0

  *-- Add transaction header record to GLHUNP
  SELECT &lcHUnPTmp
  APPEND BLANK
  REPLACE batchno  WITH lcBatchNo, ;
    glseqno  WITH lcTranNo, ;
    subno    WITH lcSess, ;
    gltdes   WITH "Posting from ARIA, " + lcComDesc , ;
    glrefr   WITH "", ;
    gldate   WITH ldGlDate, ;
    yearprd  WITH lcYearPrd, ;
    STATUS   WITH "U", ;
    trtype   WITH "Z", ;
    glmodule WITH "ARIA", ;
    glcomp   WITH lcComDesc, ;
    glrevrs  WITH "N", ;
    revper   WITH "", ;
    revyear  WITH "", ;
    adduser  WITH gcUser_Id, ;
    adddate  WITH gdSysDate, ;
    addtime  WITH gfGettime(), ;
    lckstat  WITH "", ;
    lckuser  WITH gcUser_Id, ;
    lckdate  WITH gdSysDate, ;
    lcktime  WITH gfGettime()

  *-- Process records for transaction
  SELECT &lcTmpFile

  *-- E301024 Add source module to transaction records (Start)
  *DO WHILE &lcTmpFile..GlFYear + &lcTmpFile..GlPeriod = lcYearPrd ;
  .AND. IIF(EMPTY(lcSess), .T., (&lcTmpFile..GlSession = ;
  lcSess)) .AND. .NOT. EOF()

DO WHILE &lcTmpFile..GlFYear + &lcTmpFile..GlPeriod + &lcTmpFile..cSrcModul = ;
    lcYearPrd + lcSorcMod ;
    .AND. IIF(EMPTY(lcSess), .T., (&lcTmpFile..GlSession = ;
    lcSess)) .AND. .NOT. EOF()
  *-- E301024 Add source module to transaction records (End)

  *-- Setup account number
  lcGlAcnt = &lcTmpFile..GlAccount

  *-- Setup subledger link field value for summary posting
  lcSubDetl = ""

  *-- Setup document summarization key value
  lcDocKey = &lcDocExp

  lcTmpStr ="Batch: " + ALLTRIM(lcBatchNo) + "Year/Period " + ;
    LEFT(lcYearPrd, 4) + "/" + RIGHT(lcYearPrd, 2) + ;
    ", account " + ALLTRIM(lcGlAcnt)

  lnCount = lnCount + 1
  = gfThermo(lnReccount,lnCount,'Generating...',lcTmpStr)

  *-- Setup description
  lcGlTDes = "Transaction #: " + LTRIM(Tran_No)

  *-- If detail posting
  IF .NOT. (lcDetail = "S" .OR. (lcDetail = "D" ;
      .AND. &lcSumExp.))
    *-- Setup subledger link field value
    lcSubDetl = lcDocKey
  ENDIF

  *-- Setup amount and move to next source record to be processed
  *-- Mark record with batch number and GL company number
  IF lcDetail = "S" .OR. (lcDetail = "D" .AND. &lcSumExp)
    *-- Summary, or detail and on control account

    *-- Transaction description should be blank for summarized totals
    lcGlTDes = " "

    *-- Accumulate amount while on same period/account/session/control
    lnAmount = 0


    *-- E301024 here we increment lnEntryNo
    lnEntryNo = lnEntryNo + 1

    *-- E301024 Add source module to transaction records (Start)
    *SCAN WHILE GlFYear + GlPeriod = lcYearPrd .AND. GlAccount = lcGlAcnt ;
    .AND. IIF(EMPTY(lcSess), .T., (&lcTmpFile..GlSession = ;
    lcSess)) .AND. &lcSumExp.

  SCAN WHILE GlFYear + GlPeriod + cSrcModul = lcYearPrd + lcSorcMod .AND. GlAccount = lcGlAcnt ;
      .AND. IIF(EMPTY(lcSess), .T., (&lcTmpFile..GlSession = ;
      lcSess)) .AND. &lcSumExp.
    *-- E301024 Add source module to transaction records (End)

    *-- Update batch and company numbers
    *-- E301024 here we updated entry No. and tran. No. fields (Start)
    *REPLACE glbatch WITH lcBatchNo, ;
    glcomp  WITH lcGLCo
  REPLACE glbatch WITH lcBatchNo, ;
    glcomp  WITH lcGLCo ,;
    nEntryNo  WITH lnEntryNo ,;
    cTrnsLedn WITH lcTranNo
  *-- E301024 here we updated entry No. and tran. No. fields (End)

  *-- Increment amount
  lnAmount = lnAmount + nEqvAmnt
ENDSCAN

ELSE
*-- Detail and not on control account

*-- Accumulate amount while on same period/account/session/document
lnAmount = 0

*-- E301024 Add source module to transaction records (Start)
*SCAN WHILE GlFYear + GlPeriod = lcYearPrd .AND. GlAccount = lcGlAcnt ;
.AND. IIF(EMPTY(lcSess), .T., (&lcTmpFile..GlSession = ;
lcSess)) .AND. &lcDocExp. = lcDocKey

SCAN WHILE GlFYear + GlPeriod + cSrcModul = lcYearPrd + lcSorcMod .AND. GlAccount = lcGlAcnt ;
    .AND. IIF(EMPTY(lcSess), .T., (&lcTmpFile..GlSession = ;
    lcSess)) .AND. &lcDocExp. = lcDocKey
  *-- E301024 Add source module to transaction records (End)

  *-- Update batch and company numbers
  *-- E301024 here we updated entry No. and tran. No. fields (Start)
  *REPLACE glbatch WITH lcBatchNo, ;
  glcomp  WITH lcGLCo , ;
  Posted  WITH 'X'

REPLACE glbatch WITH lcBatchNo, ;
  glcomp  WITH lcGLCo , ;
  Posted  WITH 'X' ,;
  nEntryNo  WITH lnEntryNo ,;
  cTrnsLedn WITH lcTranNo
*-- E301024 here we updated entry No. and tran. No. fields (End)

*-- Increment amount
lnAmount = lnAmount + nEqvAmnt
ENDSCAN

ENDIF && lcDetail = "N" .OR. etc.

*-- Search for account number in GLACNT
SELECT &lcAcCHrAl
SEEK lcGlAcnt

*-- If acount not found or deleted or inactive
IF .NOT. FOUND(lcAcCHrAl) .OR. DELETED(lcAcCHrAl) .OR. ;
    &lcAcCHrAl..glstat = "I"
  *-- Use suspense account
  lcGlAcnt = lcSuspense
ENDIF

*-- Add transaction detail record to GLDUNP
SELECT &lcDUnPTmp
APPEND BLANK
REPLACE glacnt   WITH lcGlAcnt, ;
  batchno  WITH lcBatchNo, ;
  glseqno  WITH lcTranNo, ;
  gltdes   WITH lcGlTDes, ;
  gldamnt  WITH IIF(lnAmount < 0, 0, ABS(lnAmount)), ;
  glcamnt  WITH IIF(lnAmount < 0, ABS(lnAmount), 0), ;
  credit   WITH (lnAmount < 0), ;
  subdetl  WITH lcSubDetl, ;
  adduser  WITH gcUser_Id, ;
  adddate  WITH gdSysDate, ;
  addtime  WITH gfGettime(), ;
  lckstat  WITH "", ;
  lckuser  WITH gcUser_Id, ;
  lckdate  WITH gdSysDate, ;
  lcktime  WITH gfGettime()

*-- Increment debit and credit totals for transaction and batch
lnTDrTot = lnTDrTot + &lcDUnPTmp..GlDAmnt
lnTCrTot = lnTCrTot + &lcDUnPTmp..GlCAmnt
lnBDrTot = lnBDrTot + &lcDUnPTmp..GlDAmnt
lnBCrTot = lnBCrTot + &lcDUnPTmp..GlCAmnt

*-- Already on next detail record
SELECT (lcTmpFile)
ENDDO && WHILE &lcTmpFile..GlFYear + &lcTmpFile..GlPeriod = etc.

IF WEXIST('gwdThermo')
  = gfThermo(lnReccount,lnReccount,'Generating...',lcTmpStr)
ENDIF

*-- If transaction is out of balance
IF lnTDrTot <> lnTCrTot
  *-- Setup out of balance amount
  lnAmount = lnTCrTot - lnTDrTot

  *-- Display alert
  *-- "Batch xxx is out of balance by $ 99.99. An offsetting amount
  *--  posted to account xxxxxxx"
  *-- <OK>
  lcTmpStr = ALLTRIM(STR(lcBatchNo)) + "|" + LTRIM(STR(lnAmount, 10, 2)) +;
    "|" + ALLTRIM(lcSuspense)
  = gfModalGen("INM00305B00000","Dialog",lcTmpStr)

  *-- Add suspense transaction detail record to GLDUNP
  SELECT &lcDUnPTmp
  APPEND BLANK
  REPLACE glacnt   WITH lcSuspense, ;
    batchno  WITH lcBatchNo, ;
    glseqno  WITH lcTranNo, ;
    gltdes   WITH "Suspense - Out of Balance", ;
    gldamnt  WITH IIF(lnAmount < 0, 0, ABS(lnAmount)), ;
    glcamnt  WITH IIF(lnAmount < 0, ABS(lnAmount), 0), ;
    credit   WITH (lnAmount < 0), ;
    adduser  WITH gcUser_Id, ;
    adddate  WITH gdSysDate, ;
    addtime  WITH gfGettime(), ;
    lckstat  WITH "", ;
    lckuser  WITH gcUser_Id, ;
    lckdate  WITH gdSysDate, ;
    lcktime  WITH gfGettime()

  *-- Update debit and credit totals for batch
  lnBDrTot = lnBDrTot + &lcDUnPTmp..gldamnt
  lnBCrTot = lnBCrTot + &lcDUnPTmp..glcamnt
ENDIF && lnTDrTot <> lnTCrTot
ENDDO && WHILE &lc_alias..glfyear + &lc_alias..glper = etc.

*-- Update batch status record with totals for batch and
*-- release object lock
SELECT (lcBatchTmp)
REPLACE lowseq   WITH lcLowSeq, ;
  uppseq   WITH lcUppSeq, ;
  bstat    WITH "U", ;
  ctrltot  WITH MIN(lnBDrTot, lnBCrTot), ;
  drtotal  WITH lnBDrTot, ;
  crtotal  WITH lnBCrTot, ;
  lckstat  WITH "", ;
  lckdate  WITH gdSysDate, ;
  lcktime  WITH gfGettime()
SELECT (lcTmpFile)
ENDDO && WHILE .NOT. EOF()
ELSE
SELECT (lcTmpFile)
SET ORDER TO TAG (lcTmpFile)
ENDIF

IF llGoOn AND lcCurProc < '3'
  lcCurProc = '3'
  llNoThing = lfUpdUnCmS("Open",lcCurProc)
ENDIF


IF llGoOn
  SELECT (lcBatchTmp)
  lnRecCount = RECCOUNT()
  lnCount    = 0
  SCAN
    IF nStep < 1
      SCATTER MEMVAR MEMO
      SELECT(lcBatchAl)
      APPEND BLANK
      GATHER MEMVAR MEMO
      SELECT (lcBatchTmp)
      REPLACE nStep WITH 1
    ENDIF
    lnCount = lnCount + 1
    = gfThermo(lnReccount,lnCount,'Updating master files...',"GLBATCH")
  ENDSCAN

  IF WEXIST('gwdThermo')
    = gfThermo(lnReccount,lnReccount,'Updating master files...',"GLBATCH")
  ENDIF

  SELECT (lcHUnPTmp)
  lnRecCount = RECCOUNT()
  lnCount    = 0
  SCAN
    IF nStep < 1
      SCATTER MEMVAR MEMO
      SELECT(lcHTrnsAl)
      APPEND BLANK
      GATHER MEMVAR MEMO
      SELECT (lcHUnPTmp)
      REPLACE nStep WITH 1
    ENDIF
    lnCount = lnCount + 1
    = gfThermo(lnReccount,lnCount,'Updating master files...',"GLTRNSHD")
  ENDSCAN

  IF WEXIST('gwdThermo')
    = gfThermo(lnReccount,lnReccount,'Updating master files...',"GLTRNSHD")
  ENDIF

  SELECT (lcDUnPTmp)
  lnRecCount = RECCOUNT()
  lnCount    = 0
  SCAN
    IF nStep < 1
      SCATTER MEMVAR MEMO
      SELECT(lcDTrnsAl)
      APPEND BLANK
      GATHER MEMVAR MEMO
      SELECT (lcDUnPTmp)
      REPLACE nStep WITH 1
    ENDIF
    lnCount = lnCount + 1
    = gfThermo(lnReccount,lnCount,'Updating master files...',"GLTRNSDT")
  ENDSCAN

  IF WEXIST('gwdThermo')
    = gfThermo(lnReccount,lnReccount,'Updating master files...',"GLTRNSDT")
  ENDIF

  SELECT (lcTmpFile)
  lnRecCount = RECCOUNT()
  lnCount    = 0
  SCAN
    IF !Pass
      SCATTER MEMVAR MEMO
      SELECT(lcGDistAl)
      APPEND BLANK
      GATHER MEMVAR MEMO
      SELECT (lcTmpFile)
      REPLACE Pass WITH .T.
    ENDIF
    lnCount = lnCount + 1
    = gfThermo(lnReccount,lnCount,'Updating master files...',"GLDIST")
  ENDSCAN

  IF WEXIST('gwdThermo')
    = gfThermo(lnReccount,lnReccount,'Updating master files...',"GLDIST")
  ENDIF

ENDIF

*-- Unlock master files
lcLocFiles = lcBatchAl+','+lcDTrnsAl+','+lcHTrnsAl
*-- Release locks to prevent deadlock on retries
= gfFlock(lcLocFiles,.F.)

*-- Erase temporary files and indeses
USE IN &lcTmpFile
ERASE gcWorkDir+lcTmpFile+".dbf"
ERASE gcWorkDir+lcTmpFile+".cdx"

USE IN &lcBatchTmp
ERASE gcWorkDir+lcBatchTmp+".dbf"
ERASE gcWorkDir+lcBatchTmp+".cdx"

USE IN &lcHUnPTmp
ERASE gcWorkDir+lcHUnPTmp+".dbf"
ERASE gcWorkDir+lcHUnPTmp+".cdx"

USE IN &lcDUnPTmp
ERASE gcWorkDir+lcDUnPTmp+".dbf"
ERASE gcWorkDir+lcDUnPTmp+".cdx"

SELECT &lcGDistAl
SET ORDER TO TAG GlDistPo IN &lcGDistAl

*!*************************************************************
*! Name      : lfGetGlBt
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Retrieves the next GL batch number from SYSDATA, increments
*!           : value in GL SYSDATA record with a check for rollover at
*!           : value of 999,999.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcCompId
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetGlBt(lcGlCo)
*!*************************************************************

FUNCTION lfGetGlBt

PARAMETERS lcCompId

*-- Declare local variables private
PRIVATE lcReturn, lcSAlias, lcXOrder, lcXFilt, lnXRecNo

*-- Initialize return variable
lcReturn = ""

*-- Save current work area
lcSAlias = ALIAS()

*-- Set record pointer to selected application and company in SYSDATA
SELECT (lcSysDaAl)
LOCATE FOR SysId = "GL" +  lcCompId

*-- See if GLBTCH is already open. If not, open it to be used for
*-- validating that batch number is not already used.
lnXRecNo = 0

*-- Save record pointer in GLBTCH
SELECT &lcBatchAl
lnXRecNo = IIF(EOF(), 0, RECNO())
SELECT (lcSysDaAl)

*-- Lock the GL SYSDATA record
llGoOn = gfRlock(lcSysDaAl,.T.)

IF llGoOn
  *-- Extract the next batch number from SYSDATA
  lcReturn = TRANSFORM(IIF(VAL(LEFT(&lcSysDaAl..str9, 6)) = 0, 1, ;
    VAL(LEFT(&lcSysDaAl..str9, 6))), "@L 999999")

  SELECT &lcBatchAl
  lcXOrder = SYS(22)
  lcXFilt  = SET("FILTER")
  *-- Clear filter
  SET FILTER TO
  *--  Set order to batchno
  SET ORDER TO TAG batchno IN &lcBatchAl

  *-- If the batch number exists in GLBTCH, then increment the batch
  *-- number until an unused number is found.
  DO WHILE SEEK(lcReturn,lcBatchAl)
    *-- Convert the batch number to a string
    lcReturn     = TRANSFORM(VAL(lcReturn) + 1, "@L 999999")
  ENDDO

  *-- No order set on entry
  IF EMPTY(lcXOrder)
    SET ORDER TO 0 IN &lcBatchAl
  ELSE
    SET ORDER TO TAG (lcXOrder) IN &lcBatchAl
  ENDIF

  *-- No filter set on entry
  IF EMPTY(lcXFilt)
    SET FILTER TO
  ELSE
    SET FILTER TO &lcXFilt
  ENDIF

  *-- Update SYSDATA with incremented next batch number
  SELECT (lcSysDaAl)
  REPLACE str9  WITH IIF(VAL(lcReturn) = 999999, "000001", ;
    TRANSFORM(VAL(lcReturn) + 1, "@L 999999")) + ;
    SUBSTR(&lcSysDaAl..str9, 7, 4)

  *-- Unlock the SYSDATA record
  = gfRlock(lcSysDaAl,.F.)

  SELECT &lcBatchAl
  IF lnXRecNo = 0
    GOTO BOTTOM
  ELSE
    GOTO lnXRecNo
  ENDIF

  *-- Restore prior alias
  IF .NOT. EMPTY(lcSAlias) .AND. USED(lcSAlias)
    SELECT (lcSAlias)
  ENDIF

ENDIF

RETURN lcReturn

*!*************************************************************
*! Name      : lfGetGlTr
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Retrieves the next GL transaction sequence number from SYSDATA,
*!           : Increments value in GL SYSDATA record with a check for rollover
*!           : at value of 999,999.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcCompId
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetGlTr(lcGlCo)
*!*************************************************************

FUNCTION lfGetGlTr

PARAMETERS lcCompId

*-- Declare local variables private
PRIVATE lcReturn, lcSAlias

*-- Initialize return variable
lcReturn = ""

*-- Save current work area
lcSAlias = ALIAS()

*-- Position the record pointer in SYSDATA on the correct GL company
SELECT (lcSysDaAl)
LOCATE FOR SysId = "GL" +  lcCompId

*-- Lock the GL SYSDATA record

llGoOn = gfRlock(lcSysDaAl,.T.)

IF llGoOn
  *-- Grab the next transaction sequence number from SYSDATA
  lcReturn = TRANSFORM(IIF(VAL(SUBSTR(&lcSysDaAl..str10, 5, 6)) = 0, ;
    1, VAL(SUBSTR(&lcSysDaAl..str10, 5, 6))), "@L 999999")

  *-- Update SYSDATA with incremented next transaction sequence number
  REPLACE str10 WITH LEFT(&lcSysDaAl..str10, 4) + ;
    IIF(VAL(lcReturn) = 999999, "000001", ;
    TRANSFORM(VAL(lcReturn) + 1, "@L 999999"))

  *-- Unlock the SYSDATA record
  = gfRlock(lcSysDaAl,.F.)

  *-- Restore prior alias
  IF .NOT. EMPTY(lcSAlias) .AND. USED(lcSAlias)
    SELECT (lcSAlias)
  ENDIF
ENDIF

RETURN lcReturn

*!*************************************************************
*! Name      : lpPrdVal
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validate transactions periods for all unposted
*!           : transactios in the distribution file. (Aria)
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpPrdVal
*!*************************************************************

PROCEDURE lpPrdVal

PARAMETERS lcFisYear, lcPeriod

*-- Declare local variables private
PRIVATE llError, lcErrMsg

*-- Set index orders fiscal calendar file
SET ORDER TO TAG ComFYrPrdI IN (lcFsPrdAl)

SELECT (lcFsPrdAl)
*--E301077,82 (Start)
*SEEK lcGLCo + lcFisYear + lcPeriod
SEEK lcFisYear + lcPeriod
*--E301077,82 (End)

llError = .F.

*-- Check for invalid, deleted, or locked posting period
DO CASE
CASE .NOT. FOUND(lcFsPrdAl)
  *-- If invalid, set flag to cancel and display alert
  llError  = .T.
  lcTmpStr =  lcPeriod + "/" + lcFisYear + ;
    " not found in fiscal calendar." + ;
    " Unable to continue with release."

CASE DELETED(lcFsPrdAl)
  *-- If deleted, set flag to cancel and display alert
  llError  = .T.
  lcTmpStr =  lcPeriod + "/" + lcFisYear + ;
    " is deleted in fiscal calendar." + ;
    " Unable to continue with release."

CASE &lcFsPrdAl..lFsPLocks
  *-- If period marked as locked,
  *-- set flag to cancel and display alert
  llError  = .T.
  lcTmpStr =  lcPeriod + "/" + lcFisYear + ;
    " is locked in fiscal calendar." + ;
    " You must unlock the period before postings ;
    can be released." + ;
    " Unable to continue with release."
ENDCASE

IF llError
  llGoOn = .F.
  *-- Posting period xxxx
  = gfModalGen("INM00312B00000","Dialog",lcTmpStr)
ENDIF

*-- Return delete status
SET DELETE ON

*-- Select Distribution file again
SELECT (lcGDistAl)

*!*************************************************************
*! Name      : lpAccVal
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validate General ledger accounts
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpAccVal WITH 'lcGlAcnt'
*!*************************************************************

PROCEDURE lpAccVal
PARAMETERS lcAccount

PRIVATE lcDelStat, lnResp

*-- Set index orders in chart of accounts
SET ORDER TO TAG AcctCode IN (lcAcCHrAl)
lcDelStat = SET('DELETE')
llDispMss = .F.
lnResp = 0
*-- Search for account number in GLACNT
IF !SEEK(&lcAccount,lcAcCHrAl)
  SET DELETE OFF
  IF !SEEK(&lcAccount,lcAcCHrAl)
    *-- Non Exist Account
    lcTmpStr = "Account " + ALLTRIM(&lcAccount)+ " not found in Chart of Accounts"
  ELSE
    *-- Deleted Account
    lcTmpStr = "Account " + ALLTRIM(&lcAccount) + " is deleted in Chart of Accounts"
  ENDIF
  llDispMss = .T.
ELSE
  IF &lcAcCHrAl..CSegActiv = "I"
    *-- Inactive Account
    lcTmpStr = "Account " + ALLTRIM(&lcAccount) + " is marked as Inactive in Chart of Accounts"
    llDispMss = .T.
  ENDIF
ENDIF

IF llDispMss
  *-- lcTmpStr+".Transfer to Suspense Account xxxx or cancel release to GL ?"
  *-- <Release>,<Cancel>
  lnResp = gfModalGen('QRM00313B00035','Dialog',lcTmpStr+'|'+ALLTRIM(lcSuspense))
  llGoOn = (lnResp <> 2)
  SET DELETE &lcDelStat
ENDIF

&lcAccount = IIF(lnResp=1,lcSuspense,&lcAccount)

*!*************************************************************
*! Name      : lpAcPrdVal
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validate General ledger accounts and periods for all unposted
*!           : transactios in the distribution file. (SBT)
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpAcPrdVal WITH lnCurrent
*!*************************************************************

PROCEDURE lpAcPrdVal

PARAMETERS lnMaxRec

*-- Declare local variables private
PRIVATE lcGlAcnt, lcFisYear, lcPeriod, llError, ;
  lcErrMsg1, lcErrMsg2, lcErrMsg3

*-- Set index orders in chart of accounts and fiscal calendar files
SET ORDER TO TAG glacnt   IN &lcAcCHrAl
SET ORDER TO TAG compid1  IN &lcFsPrdAl
SET ORDER TO TAG gldistpo IN &lcGDistAl

*-- Validate GL account numbers in source file
SELECT &lcGDistAl
SEEK " "
SCAN FOR RECNO(lcGDistAl) <= lnMaxRec  WHILE Posted = " " .AND. .NOT. EOF()

  *-- Save account number and posting year/period
  lcGlAcnt  = GlAccount
  lcFisYear = GlFYear
  lcPeriod  = GlPeriod

  *-- Display processing message
  lcErrMsg1 =  "Checking posting to account " + ALLTRIM(lcGlAcnt) + ;
    ", period " + lcFisYear + "/" + lcPeriod

  WAIT WIND lcErrMsg1 NOWAIT
  *-- Search for account number in GLACNT
  SELECT &lcAcCHrAl
  SET DELETE OFF
  SEEK lcGlAcnt
  *-- Check for invalid, deleted, or inactive account number
  llError = .F.
  DO CASE
  CASE .NOT. FOUND(lcAcCHrAl)
    *-- If invalid, display dialog
    llError = .T.
    *-- Non Exist Account
    lcTmpStr = "Account " + TRIM(lcGlAcnt) + " not found in Chart of Accounts"
  CASE DELETED(lcAcCHrAl)
    *-- If deleted, display dialog
    llError = .T.
    *-- Deleted Account
    lcTmpStr = "Account " + TRIM(lcGlAcnt) + " is deleted in Chart of Accounts"

  CASE &lcAcCHrAl..GlStat = "I"
    *-- If inactive, display dialog
    llError = .T.
    *-- Inactive Account
    lcTmpStr = "Account " + TRIM(lcGlAcnt) + " is marked as Inactive in Chart of Accounts"

  OTHERWISE
    *-- Passed all account number validations
    lnResp = 3

  ENDCASE

  IF llError
    *-- X.Transfer to Suspense Account xxxx or cancel release to GL ?'
    *-- <Release>,<Cancel>
    lnResp = gfModalGen('QRM00313B00035','Dialog',lcTmpStr+'|'+ALLTRIM(lcSuspense))
  ENDIF

  IF lnResp = 1 .OR. lnResp = 3
    *-- Search for posting period in SYCDFIS
    SELECT &lcFsPrdAl
    SEEK lcGLCo + lcFisYear + lcPeriod
    llError = .F.

    *-- Check for invalid, deleted, or locked posting period
    DO CASE
    CASE .NOT. FOUND(lcFsPrdAl)
      *-- If invalid, set flag to cancel and display alert
      llError = .T.
      lcTmpStr =  lcPeriod + "/" + lcFisYear + ;
        " not found in fiscal calendar." + ;
        "Unable to continue with release."
    CASE DELETED(lcFsPrdAl)
      *-- If deleted, set flag to cancel and display alert
      llError = .T.
      lcTmpStr = lcPeriod + "/" + lcFisYear + ;
        " is deleted in fiscal calendar." +;
        "Unable to continue with release."

    CASE &lcFsPrdAl..PermLck
      *-- If period marked as permanantly locked,
      *-- set flag to cancel and display alert
      llError = .T.
      lcTmpStr  = lcPeriod + "/" + lcFisYear + ;
        " is permanantly locked in fiscal calendar." +;
        "You must unlock the period before postings can be released." +;
        "Unable to continue with release."


    CASE &lcFsPrdAl..PLocked
      *-- If period marked as locked,
      *-- set flag to cancel and display alert
      llError = .T.
      lcErrMsg1 = "Posting period " + lcPeriod + "/" + ;
        lcFisYear + " is locked in fiscal calendar."
      lcErrMsg2 = "You must unlock the period before postings can be released."
      lcErrMsg3 = "Unable to continue with release."

      lcTmpStr  = lcPeriod + "/" + lcFisYear + ;
        " is locked in fiscal calendar." +;
        "You must unlock the period before postings can be released."+;
        "Unable to continue with release."

    OTHERWISE
      *-- Passed all posting period validations
      lnResp = 3
    ENDCASE

    WAIT CLEAR

    IF llError
      lnResp = 2
      *-- Posting period lcTmpStr
      *-- <OK>
      = gfModalGen("INM00312B00000","Dialog",lcTmpStr)
    ENDIF

  ENDIF && lnResp = 1 .OR. lnResp = 3
  *-- Return delete status
  SET DELETE ON

  *-- If Escape or Cancel
  IF lnResp = 0 .OR. lnResp = 2
    *-- Escape or Cancel, set flag and exit
    llGoOn = .F.
    EXIT
  ENDIF

  *-- Move to next record in source file
  SELECT &lcGDistAl

ENDSCAN
*!*************************************************************
*! Name      : lpPostMark
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Mark/unmark transaction records as posted.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : llMark   : Mark type
*!                               : .T. Marking records as posted.
*!                               : .F. Clearing batch numbers from
*!                                     transaction records.
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpPostMark WITH llGoOn
*!*************************************************************

PROCEDURE lpPostMark

PARAMETERS llMark

*-- If marking records
IF llMark
  *-- Display processing message
  WAIT WINDOW 'Marking transaction records as released...' NOWAIT

  *-- Mark unposted records in Distribution file
  SELECT &lcGDistAl
  SET ORDER TO 0 IN &lcGDistAl
  REPLACE ALL Posted WITH "X" FOR Posted = " " .AND. RECNO() <= lnCurrent

ELSE
  *-- If not marking records

  *-- Display processing message
  WAIT WINDOW 'Clearing batch numbers from transaction records...' NOWAIT

  *-- Clear GL batch number from unposted records in Distribution file
  SELECT &lcGDistAl
  SEEK " "
  REPLACE ALL GlBatch WITH "", ;
    GlComp  WITH "" ;
    WHILE Posted = " " FOR RECNO() <= lnCurrent
ENDIF && llMark

*!*************************************************************
*! Name      : lfChkUnCmS
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : Check if there is uncomplete session
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfChkUnCmS()
*!*************************************************************

FUNCTION lfChkUnCmS

PRIVATE lnAlias, llGoOut, lnReprocess
llGoOut    = .F.
llUnComChk = .T.
IF gfUnCompSession(lcProgID,lnSessNo,"Release to G/L")
  llGoOut   = .T.
  lcSession = UnCmSess.cSession
  lcCurProc = ALLTRIM(UnCmSess.cCurrObj)
ENDIF

RETURN llGoOut

*!*************************************************************
*! Name      : lfAdUnCmSR
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : Adding record in uncomplete session file
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfAdUnCmSR()
*!*************************************************************

FUNCTION lfAdUnCmSR
PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT UnCmSess
IF !SEEK('I')
  APPEND BLANK
ENDIF
lnUnCmSeRc = RECNO()
BLANK
REPLACE STATUS     WITH 'O'      ,;
  cUTranType WITH lcProgID ,;
  cUserId    WITH lcUserID ,;
  cSession   WITH lcSession,;
  cProgram   WITH lcProgID ,;
  cCurrScr   WITH lcProgID ,;
  cCurrObj   WITH VARREAD(),;
  dTranDate  WITH gdSysDate,;
  cTranTime  WITH TIME()

llNoThing = lfUpdUnCmS("Open", SPACE(0))
llNoThing  = RLOCK()

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfUpdUnCmS
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : Update the current object and the status of the session
*!           : and Update variables
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfUpdUnCmS()
*!*************************************************************

FUNCTION lfUpdUnCmS
PARAMETERS lcStatus, lcCurProc
PRIVATE lnAlias

llNoThing = IIF(lnUnCmSeRc=0, .T., gfSavSess(lcProgID, lcFiles, @laVars, lcSession))

IF lnUnCmSeRc <> 0
  lnAlias  = SELECT(0)
  lcStatus = UPPER(LEFT(lcStatus,1))
  SELECT UnCmSess
  GOTO lnUnCmSeRc
  REPLACE cCurrObj WITH lcCurProc ,;
    STATUS   WITH lcStatus
  IF STATUS $ "IC"
    UNLOCK
  ENDIF
  SELECT(lnAlias)
ENDIF

*!*************************************************************
*! Name      : lpClsScr
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   :
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lpClsScr()
*!*************************************************************

PROCEDURE lpClsScr

llNoThing = lfUpdUnCmS("Initia", SPACE(0))
*--E301077,82 (Start)
*IF USED ('UNCMSESS')
*  USE IN UNCMSESS
*ENDIF
*USE (gcDataDir+'UNCMSESS') IN 0 SHARED
*--E301077,82 (Start)

*!*************************************************************
*! Name      : lfvClose
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : Validate the close data process
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvClose()
*!*************************************************************

FUNCTION lfvClose

lcFiles   = ''
llNoThing = lfUpdUnCmS("Close", SPACE(0))
*--E301077,82 (Start)
*IF USED ('UNCMSESS')
*  USE IN UNCMSESS
*ENDIF
*IF !EMPTY(gcDataDir)
*  USE (gcDataDir+'UNCMSESS') IN 0 SHARED
*ENDIF
glQuitting = .T.
CLEAR READ

*!*************************************************************
*! Name      : lpEscKey
*! Developer : Ahmed Amer (AHM)
*! Date      : 09/10/97
*! Purpose   : Trap escape key
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : DO lpEscKey
*!*************************************************************

PROCEDURE lpEscKey

=lfvClose()

*!*************************************************************

FUNCTION lfSorcMod

PARAMETERS lcTranType

*B126253,1 NNA 01/26/2005 (Begin) make width of (lcReturn)=2 CHR. because in some cases if (lcTranType)
*B126253,1 NNA            get Empty in the first time then (lcReturn) will be one Chr. so the field of
*B126253,1 NNA            &lcTmpFile..cSrcModul Create with wdith = one CHR.
*lcReturn = ''
lcReturn = SPACE(2)
*B126253,1 NNA (End)

IF SEEK(lcTranType,'SyGLTran')
  lcReturn = SyGLTran.cSrcModul

  *B121766,1 MHM 03/17/2004 Update Gldist NL type with 'PO' correct Module if 'MF' Not Installed[Start]
  
  *B125320,1 NNA 01/03/2005 (BEGIN) return with 'PO' only if the trantype = 'NL' (that for 
  *B125320,1 NNA            'Manufacturing operation' cost items on a PO cost sheet) and 'MF' not installed
  *IF (OCCURS('MF',gcCmpModules)=0) 
  IF (OCCURS('MF',gcCmpModules)=0) AND lcTranType = 'NL'
    lcReturn = 'PO'
  ENDIF
  *B125320,1 NNA (End)
    
  *B121766,1 MHM 03/17/2004 [End]
ENDIF

RETURN (lcReturn)

*!*************************************************************

FUNCTION lfvSelMod


*B603504,1 (Begin) Return if no modules are linked to GL.
IF EMPTY(laModSor)
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'No modules linked to GL!')
  cbSelMod = 0
  SHOW GET cbSelMod
  RETURN
ENDIF
*B603504,1 (End)
= gfMover(@laModSor,@laModTar,"Transactions",.T.,.F.,.F.,.T.)
cbSelMod = IIF(ALEN(laModTar,1)=1 AND EMPTY(laModTar[1]),0,1)

SHOW GET cbSelMod
*B603504,1 (Begin) Remove IF AND ENDIF to get all transactions for selected modules.
*IF ALEN(laModTar,1)=1 AND !EMPTY(laModTar[1]) AND ALEN(laModTar,1) # 4
*B603504,1 (End)
IF ASCAN(laModTar,"Material") > 0
  *B603504,1 (Begin) Change the way of collecting to be selecting transactios by catg_key not tran_type+catg_key.
  *lcTrans = lcTrans + ",MP015" + ",MP016" + ",MA015" + ",MA016" + ",MO013"
  lcTrans = lcTrans + ",015,016,013,021"
  *B603504,1 (End)
ENDIF

IF ASCAN(laModTar,"Inventory") > 0
  *B603504,1 (Begin) Change the way of collecting to be selecting transactios by catg_key not tran_type+catg_key.
  *lcTrans = lcTrans + ",IN006" + ",IN008" + ",VI006" + ",VI008" +;
  ",RM006" + ",RM008" + ",VR006" + ",VR008" + ;
  ",IP006" + ",IP007" + ",IA006" + ",IA007" +;
  ",IA013" + ",PO006" + ",PO013" + ",CT006" +;
  ",CT013"
lcTrans = lcTrans + ",006,007,008,013,018,019,022,023,024,025,026"
*B603504,1 (End)
ENDIF

IF ASCAN(laModTar,"AR") > 0
  *B603504,1 (Begin) Change the way of collecting to be selecting transactios by catg_key not tran_type+catg_key.
  *lcTrans = lcTrans + ",IN004" + ",IN014" + ",IN003" + ",IN005" + ",IN001" + ;
  ",VI004" + ",VI014" + ",VI003" + ",VI005" + ",VI001" + ;
  ",RM004" + ",RM014" + ",RM020" + ",RM005" + ",RM001" + ;
  ",VR004" + ",VR014" + ",VR020" + ",VR005" + ",VR001" + ;
  ",CR001" + ",CR002" + ",CA009" + ",CA001" +            ;
  ",DA010" + ",DA001" + ;
  ",KO001"
lcTrans = lcTrans + ",001,002,003,004,005,009,010,014,020"
*B603504,1 (End)
ENDIF
llGLRel = !EMPTY(lcTrans)
llAPRel =  ASCAN(laModTar,"AP") > 0
*B603504,1 (Begin) Initialize three variables needed.
*--Are both MA and IC selected or unselected?
llExclCon = (ASCAN(laModTar,"Material") > 0 AND ASCAN(laModTar,"Inventory") > 0 ) OR (ASCAN(laModTar,"Material") = 0 AND ASCAN(laModTar,"Inventory") = 0 )
*--Is MA only selectd?
llMatMod = (ASCAN(laModTar,"Material") > 0 AND ASCAN(laModTar,"Inventory") = 0 )
*B603504,1 (End)

*B603504,1 (Begin) Remove ENDIF.
*ENDIF
*B603504,1 (End)

*!******************************************************************************************
*! Name      : lfGetModul
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/11/2000
*! Purpose   : Fill the modules mover with only installed and linked modules.
*!******************************************************************************************
*! Refer to  : B603504,1
*!******************************************************************************************
FUNCTION lfGetModul

PRIVATE lcAlias
lcAlias = ALIAS()

DIMENSION laModSor[4],laModTar[4]
STORE '' TO laModSor,laModTar
*B603504,1 (Begin) Remark the following line as then variable is no longer used.
*llApLink   = .F.
*B603504,1 (End)
*-- Are MA,AR,IC modules installed and linked to GL?
IF ALLTRIM(UPPER(gfGetMemVar('M_Link_GL',lcComp)))   = 'Y'
  IF ('MA' $ SYCCOMP.mModlSet)
    STORE "Material"  TO laModSor[1],laModTar[1]
  ENDIF
  IF  ('IC' $ SYCCOMP.mModlSet)
    STORE "Inventory" TO laModSor[2],laModTar[2]
  ENDIF
  IF  ('AR' $ SYCCOMP.mModlSet)
    STORE "AR"       TO laModSor[3],laModTar[3]
  ENDIF
ENDIF
*-- Is AP module installed and linked to GL?
IF  ('AP' $ SYCCOMP.mModlSet)
  *B603504,1 (Begin) Is AP module installed and linked to GL?
  llApInstal = .T.
  *B603504,1 (End)
  *--Make sure the comany APSETUP file is open and avoid getting 'Alias aleady in use' error mesaage.
  IF USED('APSETUP')
    USE IN APSETUP
  ENDIF
  =gfOpenFile(lcGlCoDDir+'APSETUP')
  GO TOP IN APSETUP
  *--Is AP installed?
  IF !EOF('APSETUP') AND (APSETUP.CAPSGLLINK = 'Y')
    STORE "AP"       TO laModSor[4],laModTar[4]
    *B603504,1 (Begin) Remark the following line as then variable is no longer used.
    *llApLink   = .F.
    *B603504,1 (End)
  ENDIF
ENDIF

*-- This loop is to delete any empty element and redimension bioth arrays.
lnDelNo = 0
FOR lnMod = 1 TO ALEN(laModTar,1)
  *-- If it's empty as no module or it's .f. as it has been previously deleted.
  IF EMPTY(laModTar[lnMod]) AND IIF(TYPE('laModTar[lnMod]')<>'L',.T.,laModTar[lnMod] <> .F.)
    =ADEL(laModTar,lnMod)
    =ADEL(laModSor,lnMod)
    lnDelNo = lnDelNo +1
  ENDIF
ENDFOR
*-- If no module linked then empty arrays and return
IF lnDelNo = 4
  STORE '' TO laModTar,laModSor
  RETURN
ENDIF
IF lnDelNo > 0
  *-- If the AP only is linked, the first elemnt will be empty.
  IF EMPTY(laModTar[1]) AND IIF(TYPE('laModTar[1]')<>'L',.T.,laModTar[1] <> .F.)
    =ADEL(laModTar,1)
    =ADEL(laModSor,1)
    lnDelNo = lnDelNo +1
  ENDIF
  DIMENSION laModTar[4-lnDelNo]
  DIMENSION laModSor[4-lnDelNo]
ENDIF
IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF

*!******************************************************************************************
*! Name      : lfGetPerd
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/11/2000
*! Purpose   : Get the period and year and fill them for the remaining records of the transaction.
*!******************************************************************************************
*! Refer to  : B603504,1
*!******************************************************************************************
FUNCTION lfGetPerd

*-- This function will return .F. if nothing for it to do.
IF !EMPTY(Tran_date) AND !EMPTY(GlFYear) AND !EMPTY(GlPeriod)
  RETURN .F.
ENDIF

PRIVATE lcAlias,lnRecNo,llUserDate,lcTranTyp
lcAlias = ALIAS()

*--For the screen
lcTranTyp  = TRAN_desc
lcTransNo  = Tran_No
llUserDate = .F.
IF EMPTY(Tran_date) OR EMPTY(GlFYear) OR EMPTY(GlPeriod)
  *-- Save the current record.
  *----------------------------------------------------------------------------------------------
  *-- Note : The program will never enter here unless date or year or period is empty. This means
  *--      : that it will enter here for the most top records in the file as it's indexed on Year+Period.
  *--      : So, after getting the valid date,year, and period and update the file with them the place of the
  *--      : record being updated will change. Therefor if this function change anything in the temp file
  *--      : it will return .T. to LOOP to get another record with date or year or period is empty. As for
  *--      : the record previously updated it will be manipulated when its place in the DO WHILE comes to it according
  *:       : to the index.
  *-----------------------------------------------------------------------------------------------
  *--Get recno of the next transaction record as it will be the current record if the file chnaged.
  lcKey = EVAL(KEY())
  lcTranKey = Tran_Type+Tran_no
  *--We look here for any record having date or year or period empty for any other transaction to get it's rec no to get it
  *--back if we update all the lines of the current transaction with the new date,year and period.
  LOCATE FOR (EMPTY(Tran_date) OR EMPTY(GlFYear) OR EMPTY(GlPeriod)) AND (Tran_Type+Tran_no <> lcTranKey)
  IF !FOUND()
    *--No records having empty date or year or period.
    =SEEK(lcKey)
    *--Get the next transaction rec no.
    LOCATE REST FOR Tran_Type+Tran_no <> lcTranKey
    IF EOF()
      =SEEK(lcKey)
    ENDIF
  ENDIF
  lnRecNo = RECNO()
  =SEEK(lcKey)
  IF EMPTY(Tran_date)
    *--Is there any date for this tranaction?
    *-- Initialize date.
    ldTranDate = Tran_date
    *--Change order to tran_no
    SET ORDER TO TAG tran_no
    =SEEK(lcTranKey)
    SCAN WHILE Tran_Type+Tran_no = lcTranKey FOR !EMPTY(Tran_date)
      ldTranDate = Tran_date
      IF !EMPTY(ldTranDate)
        EXIT
      ENDIF
    ENDSCAN
    =SEEK(lcTranKey)
    IF EMPTY(ldTranDate)
      DO (gcScrDir + gcWinAppl + '\SMGLDATE.SPR')
      llUserDate = !EMPTY(ldTranDate)
    ENDIF
    *-- If the user left the date empty.
    IF EMPTY(ldTranDate)
      *-- The user didn't enter a date. Return and let the program does it it has always done.
      SET ORDER TO TAG (lcTmpFile)
      GO IIF(BETWEEN(lnRecNo,1,RECCOUNT(lcTmpFile)),lnRecNo,RECNO())
      IF !EMPTY(lcAlias)
        SELECT (lcAlias)
      ENDIF
      RETURN .F.
    ENDIF
  ENDIF
  *-- IF there is a transaction date, valid it.
  STORE '' TO lcYear,lcPrd
  ldTranDate = IIF(!EMPTY(Tran_Date),Tran_Date,ldTranDate)
  *-- If it's not valid.
  llValid = CheckPrd(ldTranDate,'lcYear','lcPrd',Tran_Type, .T.)
  *--If original date or user date entered is invalid.
  IF !llValid AND !llUserDate
    DO (gcScrDir + gcWinAppl + '\SMGLDATE.SPR')
  ENDIF
  *-- If the original date is valid or the user entered a valid date.
  IF llValid OR (!EMPTY(ldTranDate) AND CheckPrd(ldTranDate,'lcYear','lcPrd',Tran_Type))
    *-- It's a valid date and we got its year and period.
    *--Verify order tran_no
    SET ORDER TO TAG tran_no
    =SEEK(lcTranKey)
    REPLACE REST TRAN_DATE  WITH ldTranDate,;
      GlFYear    WITH lcYear,;
      GlPeriod   WITH lcPrd;
      WHILE Tran_Type+Tran_no = lcTranKey
  ELSE
    RETURN .F.
  ENDIF
  SET ORDER TO TAG (lcTmpFile)
  GO IIF(BETWEEN(lnRecNo,1,RECCOUNT(lcTmpFile)),lnRecNo,RECNO())
ENDIF
IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF


*--------------------------------------------------------------
*! Name      : lpPrnPost
*! Developer : Ramy Mabrouk
*! Date      : 07/31/2000
*! Purpose   : Print the posting regester reports
*--------------------------------------------------------------
*! Calls     :
*!             Procedures : ....
*!             Functions  : gfTempName() , gfModalGen()
*--------------------------------------------------------------
*! Passed Parameters  : 1 -->> for summary report
*!                      2 -->> for detail report
*--------------------------------------------------------------
*! Returns            : None
*--------------------------------------------------------------
*! Example   : = lpPrnPost(1)
*--------------------------------------------------------------
*-B803530,1 Ramy

PROCEDURE lpPrnPost

PARAMETERS lnRepType

PRIVATE lcTranType, lcCustomer, lcRef


OGPlatForm   = 'DOS'
lcOGPlatForm = OGPlatForm
gcDevice     = "PRINTER"
*lcSetConsl   = SET('CONSOLE')
*SET CONSOLE OFF

*lcSetDevic = SET('DEVICE')
*SET DEVICE TO PRINTER

gcOutFile  = ''
gcOutFile  = gcWorkDir + gfTempName() + '.txt'
lcFrxNam   = IIF(lnRepType = 1 , 'SMRELPOS.FRX' , 'SMRELPOD.FRX')
lcCustomer = ''
lcRef      = ''
llDummy    = .T.

*-- Chart of account file
IF !USED(lcCAccFile)
  =gfOpenFile(lcGlCoDDir+lcCAccFile,'','SH')
ENDIF

SELE (lcTmpFile)
SET ORDER TO TAG Acctcode IN   &lcCAccFile
SET RELATION TO Glaccount INTO &lcCAccFile

IF lnRepType = 2
  *-- Invoice hedear file
  IF USED(lcInvHdAl)
    SET ORDER TO TAG invhdr  IN   (lcInvHdAl)
    SET RELATION TO Tran_No  INTO (lcInvHdAl) ADDITIVE
  ENDIF
  *-- Return hedear file
  IF USED(lcRetHdAl)
    SET ORDER TO TAG rethdr  IN   (lcRetHdAl)
    SET RELATION TO Tran_No  INTO (lcRetHdAl) ADDITIVE
  ENDIF
  *-- Cutticket hedear file
  IF USED(lcCutTkAl)
    SET ORDER TO TAG CUTTKTH IN   (lcCutTkAl)
    SET RELATION TO Tran_No  INTO (lcCutTkAl) ADDITIVE
  ENDIF

  *-- Style purchase order hedear file
  IF USED(lcPosHdAl)
    SET ORDER TO TAG POSHDR  IN   (lcPosHdAl)
    SET RELATION TO Tran_No  INTO (lcPosHdAl) ADDITIVE
  ENDIF
ENDIF



*--Message : Do you like to print or preview the posting register ?
*--Button  :                Print | Preview
lnReturn = gfModalGen('QRM00373B00040','Dialog')

IF lnReturn = 1
  gcDevice = "PRINTER"
  OGPlatForm = 'DOS'
  lldummy = pSetup(.T.)
ELSE
  gcDevice = "SCREEN"
  OGPlatForm = 'DOS'
ENDIF

IF LASTKEY()=27

    KEYBOARD "R"
 
ENDIF

IF !lldummy
 RETURN
ENDIF

SELECT (lcTmpFile)

DO (gcRepHome+"SMREPORT.APP") WITH "SMRELGL",.T.
CLEAR TYPEAHEAD

ERASE gcOutFile
gcOutFile = " "
*SET DEVICE TO &lcSetDevic
*SET CONSOLE &lcSetConsl


*--------------------------------------------------------------
*! Name      : lfGetType
*! Developer : Ramy Mabrouk
*! Date      : 07/31/2000
*! Purpose   : Get some information needed in the .frx
*--------------------------------------------------------------
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*--------------------------------------------------------------
*! Passed Parameters  : None
*--------------------------------------------------------------
*! Returns            : None
*--------------------------------------------------------------
*! Example   : = lfGetType()
*--------------------------------------------------------------
*-B803530,1 Ramy
FUNCTION lfGetType

DO CASE

CASE Tran_Type =  'IN'
  lcTranTYpe   =  'INVOICE             '
  lcCustomer   =  EVALUATE(lcInvHdAl+".Account")

CASE Tran_Type =  'VI'
  lcTranType   =  'VOID INVOICE        '
  lcCustomer   =  EVALUATE(lcInvHdAl+".Account")

CASE Tran_Type =  'CR'
  lcTranType   =  'CASH RECEIPT        '
  SELECT (lcCredtAl)
  LOCATE FOR Batch = &lcTmpFile->Tran_No
  lcCustomer   =  EVALUATE(lcCredtAl + ".Account")
  lcRef        =  EVALUATE(lcCredtAl + ".REFERENCE")
  SELECT &lcTmpFile

CASE Tran_Type =  'CA'
  lcTranType   =  'CREDIT ADJUSTMENT   '
  SELECT (lcCredtAl)
  LOCATE FOR TRAN = &lcTmpFile->Tran_No
  lcCustomer   =  EVALUATE(lcCredtAl + ".Account")
  lcRef        =  EVALUATE(lcCredtAl + ".REFERENCE")
  SELECT &lcTmpFile

CASE Tran_Type =  'DA'
  lcTranType   =  'DEBIT ADJUSTMENT    '
  SELECT (lcDebitAl)
  LOCATE FOR TRAN = &lctmpFile->Tran_No
  lcCustomer   =  EVALUATE(lcDebitAl + ".Account")
  lcRef        =  EVALUATE(lcDebitAl + ".REFERENCE")
  SELECT &lcTmpFile

CASE Tran_Type =  'RM'
  lcTranType   =  'RETURN MERCHANDISE  '
  lcCustomer   =  EVALUATE(lcRetHdAl+".Account")
  lcRef        =  EVALUATE(lcRetHdAl+".REFERENCE")

CASE Tran_Type =  'VR'
  lcTranType   =  'VOID RETURN         '
  lcCustomer   =  EVALUATE(lcRetHdAl+".Account")
  lcRef        =  EVALUATE(lcRetHdAl+".REFERENCE")

CASE Tran_Type =  'IP'
  lcTranType   =  'INVENTORY PHYSICAL  '

CASE Tran_Type =  'IA'
  lcTranType   =  'INVENTORY ADJUSTMENT'

CASE Tran_Type =  'PO'
  lcTranType   =  'P/O RECEIVING       '
  lcCustomer   =  EVALUATE(lcPosHdAl+".Account")

CASE Tran_Type =  'CT'
  lcTranType   =  'C/T RECEIVING       '
  *lcCustomer   =  EVALUATE(lcCutTkAl+".Account")

CASE Tran_Type =  'ZE'
  lcTranType   =  'ZERO OUT STOCK      '
ENDCASE

SELECT (lcTmpFile)