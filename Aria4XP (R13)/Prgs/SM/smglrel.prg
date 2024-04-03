*:************************************************************************
*:  Program File: ARIA4XP\PRGS\SM\SMGLREL.PRG
*:  Module      : System Manager
*:  Desc.       : Release to GL
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 11/14/2012
*:  Reference   : *E303295
*:
*: I kept everything in this program as it is since the screen is a modal one, also files are locked in the actual release time
*: I made the screen as Default data session one to see all defined variables
*:
*:************************************************************************
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

* Note :
* The files are being updated by this program are :
*                  GLDIST.DBF
*                  APDIST.DBF
*                  GLBATCH.DBF
*                  GLTRNSDT.DBF
*                  GLTRNSHD.DBF
************************************************************************************
* Modifications
*B610170,1 TMI 12/06/2012 [T20121206.0004] release the set procedure after closing the program
*E303295,4 TMI 12/08/2012 [Start] opne the
*B610232,1 HIA 02/10/2013 System Manager to Release to GL module program freeze up  [T20130118.0006]
*B610256,1 HIA 02/21/2013 System Manager to Release to GL module program freeze up  [T20130118.0006]
*N000682,1 04/16/2013 HES Globlization changes[Start]
*B610392,1 TMI 06/20/2013 when selecting AP the report should show AP trans. only(A4xp-R13) [T20130425.0011]
*B610791,1 TMI 08/04/2014 Allow showing the release report before the actual release process [T20140724.0006]
*B610869,1 TMI 10/01/2014 fix a bug when releasing to the GL [T20140930.0002 ]
*B610879,1 TMI 10/12/2014 use the seek function instead of the relation [T20140702.0003]
*E303557,1 MMT 03/09/2015 Don't close period has unreleased transactions[T20150227.0001]
*B610961,1 MMT 03/11/2015 Release to GL summary report prints account desc. incorrectly[T20141219.0004]
*E303598,1 MMT 08/27/2015 Show Transaction type in Release to GL Summary report[T20150716.0011]
*B611293,1 MHM 04/04/2017 Issue with releasing Gl [T20170327.0002]
*E303962,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL
*E303971,1 MMT 05/02/2018 Modify release to GL program to use new setups from GLSETUP[P20171130.0001]
*B611750,1 Es 3/21/2019 Void Credit memo does not add entries in GLDIST and Release to GL Includes APDIST 109 Payment records and moves to Suspense account because it has empty GL account.[T20190304.0004]
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001]
************************************************************************************
*E303557,1 MMT 03/09/2015 Don't close period has unreleased transactions[T20150227.0001][Start]
PARAMETERS llDoNotShowReleaseScreen
*E303557,1 MMT 03/09/2015 Don't close period has unreleased transactions[T20150227.0001][End]

*N000682,1 04/16/2013 HES Globlization changes[Start]
#INCLUDE R:\ARIA4XP\PRGS\SM\SMGLREL.H
*N000682,1 04/16/2013 HES Globlization changes[End  ]

*B610791,1 TMI 08/04/2014 13:55 [Start] define ap1 object
ap1 = CREATEOBJECT('ap')
*B610791,1 TMI 08/04/2014 13:55 [End  ]

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

*E303295 TMI 11/18/2012 [Start] define some variables
gcWorkDir  = oAriaApplication.WorkDir
gcDataDir  = oAriaApplication.DataDir
gdSysDate  = oAriaApplication.SystemDate
gcCom_Name = oAriaApplication.ActiveCompanyName
gcWinAppl  = oAriaApplication.activemoduleid
gcScrDir   = oAriaApplication.ScreenHome

lnSlct = SELECT(0)
=gfOpenFile(gcDataDir+'FISHD','COMPFYEAR','SH')
SELECT (lnSlct)
*E303295 TMI 11/18/2012 [End  ]


*E303295 TMI 11/18/2012 [Start] get the laEvntTrig array contents for this program
DIMENSION laEvntTrig[1]
laEvntTrig = ' '
*E303295 TMI 11/18/2012 [End  ]

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


*- Select an active company before proceeding allow the variable lcCompDir to be populated
IF EMPTY(oAriaApplication.ActiveCompanyID)
*N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No active company is Selected !')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_NoActive_Comp)

*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
	IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
		loFormSet.oProgress.Hide()
	ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]

	=gfModalGen('INM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_NoActive_Comp,loFormSet.GetHeaderText("LANG_NoActive_Comp",loFormSet.HeaderAlias)))
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
	IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
		loFormSet.oProgress.Show()
	ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]


*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 HES Globlization changes[End  ]
	RETURN
ENDIF

STORE 0 TO lnResp,lnSessMin,lnSessMax,lnUnCmSeRc,lnCurrent

lcProgID  = PADR("RELTOGL",10)
lnSessNo  = gnProgCopy  && This is to hold the no of the opened session from global variable

lcUserID  = PADR(oAriaApplication.User_ID, 10)    && This is to hold the user id

PRIVATE lcTrans,laModSor,laModTar
DIMENSION laModSor[4],laModTar[4]

*-- Just initialize them
STORE '' TO laModSor,laModTar
STORE .F. TO llMatMod,llExclCon

llApInstal = .F.

STORE '' TO lcTranTyp,lcTransNo
STORE {} TO ldTranDate
STORE .T. TO llGLRel,llAPRel
cbSelMod = 1
lcTrans = ''

lcSysDaAl  = gfTempName()
lcLinkChar = gfTempName()
lcSequnAl  = gfTempName()
lcGLSetAl  = gfTempName()
lcAcCodAl  = gfTempName()
lcAccHrAl  = gfTempName()
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
lcAPDIST   = gfTempName()

*--laCompany  Array that hold the companies IDs and its descriptions
*--laSourceJr Array that used by Source Journal
DIMENSION laCompany[1]
DIMENSION laSourceJr[1,2]
STORE '' TO laCompany,laSourceJr

IF USED('SYCCOMP')
	USE IN SycComp
ENDIF
=gfOpenFile(oAriaApplication.SysPath+'SYCCOMP','CCOMP_ID','SH')
*--Filling laCompany array with companies IDs
SELECT SycComp.cComp_ID+"-"+SycComp.cCom_Name,SycComp.cComp_ID;
FROM SycComp INTO ARRAY laCompany ;
ORDER BY SycComp.cComp_ID

lcOrgEsc = ON("KEY","ESCAPE")
*B610170,1 TMI 12/06/2012 [Start] save the set procedure
LOCAL lcProc
lcProc = SET("Procedure")
*B610170,1 TMI 12/06/2012 [End  ]

*-- calling the screen SMGLREL
lcRunScx = lfGetScx("SM\SMGLREL.scx")
*E303557,1 MMT 03/09/2015 Don't close period has unreleased transactions[T20150227.0001][Start]
IF llDoNotShowReleaseScreen
	DO FORM (lcRunScx) NOSHOW
ELSE
*E303557,1 MMT 03/09/2015 Don't close period has unreleased transactions[T20150227.0001][End]
	DO FORM (lcRunScx)
*E303557,1 MMT 03/09/2015 Don't close period has unreleased transactions[T20150227.0001][Start]
ENDIF
*E303557,1 MMT 03/09/2015 Don't close period has unreleased transactions[T20150227.0001][End]
IF USED('lcLinkChar')
	SELECT lcLinkChar
	USE
ENDIF

*- remove the created index (oAriaApplication.WorkDir+lcAPDIST+'.IDX')
IF FILE(oAriaApplication.WorkDir+lcAPDIST+'.IDX')
	IF USED('APDIST')
		USE IN APDIST
	ENDIF
	ERASE (oAriaApplication.WorkDir+lcAPDIST+'.IDX')
ENDIF
*B610170,1 TMI 12/06/2012 [Start] restore old procedure settings
*B610256,1 HIA 02/21/2013 System Manager to Release to GL module program freeze up  [T20130118.0006][Start]
*SET PROCEDURE TO (lcProc)
SET PROCEDURE TO &lcProc.
*B610256,1 HIA 02/21/2013 System Manager to Release to GL module program freeze up  [T20130118.0006][End]
*B610170,1 TMI 12/06/2012 [End  ]

************************************************************
*! Name      : lfGetScx
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : Get the scx path to run in SaaS environemt
************************************************************
FUNCTION lfGetScx
PARAMETERS lcScx
LOCAL lcRunScx
IF oAriaApplication.Multiinst AND FILE(oAriaApplication.clientscreenhome+lcScx)
	lcRunScx = oAriaApplication.clientscreenhome+lcScx
ELSE
	lcRunScx = oAriaApplication.ScreenHome+lcScx
ENDIF
RETURN lcRunScx
*- End of lfGetScx.
************************************************************
*! Name      : lfFormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/18/2012
*! Purpose   : init form method
************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet

loFormSet.ADDPROPERTY('oProgress' ,  NULL )

WITH loFormSet.Ariaform1

	.laCompany.VALUE = oAriaApplication.ActiveCompanyID+'-'+oAriaApplication.ActiveCompanyName

	.cbSelMod.CONTROLSOURCE = 'cbSelMod'
	IF TYPE('loFormset.Ariaform1.cbSelMod.OldValue') = 'U'
		.cbSelMod.ADDPROPERTY('OldValue',cbSelMod)
	ENDIF

*- initiate module array
	.laCompany.VALID()

	.REFRESH()
*E303557,1 MMT 03/09/2015 Don't close period has unreleased transactions[T20150227.0001][Start]
	IF llDoNotShowReleaseScreen
		lfvProceed(loFormSet)
	ENDIF
*E303557,1 MMT 03/09/2015 Don't close period has unreleased transactions[T20150227.0001][End]
ENDWITH

*- End of lfFormInit.

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
PRIVATE lnSlct
lnSlct = SELECT(0)


IF EMPTY(lcComp)
	lcComp    = oAriaApplication.ActiveCompanyID
	lcCurComp = lcComp
ENDIF
IF EMPTY(lcComp)
	IF ASCAN(laCompany,'NOCOMP') = 0
		DIMENSION laCompany[ALEN(laCompany,1)+1,2]
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	    laCompany[ALEN(laCompany,1),1] = 'Select a company'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laCompany[ALEN(laCompany,1),1] = LANG_Select_company
		laCompany[ALEN(laCompany,1),1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Select_company,loFormSet.GetHeaderText("LANG_Select_company",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[end]

		laCompany[ALEN(laCompany,1),2] = 'NOCOMP'
		_CUROBJ = OBJNUM(puCompany)
	ENDIF
	puCompany = ASUBSCRIPT(laCompany,ASCAN(laCompany,'NOCOMP'),1)
ELSE
	puCompany = ASUBSCRIPT(laCompany,ASCAN(laCompany,lcComp),1)
	DIMENSION laSetups[3,2]
	laSetups[1,1]  = 'M_GL_CO'
	laSetups[2,1]  = 'M_GL_VERS'
	laSetups[3,1]  = 'M_SYS_DIR'
	= gfGetMemVar(@laSetups,lcComp)
	lcGLCo  = laSetups[1,2]
	lcGLVer = IIF(EMPTY(laSetups[2,2]),'A',laSetups[2,2])
*-- get the company Description

	IF SEEK(lcComp,'SycComp')
		IF lcGLVer <> 'S'
			lcGLCo = IIF(EMPTY(SycComp.cCompPrnt),lcComp,ALLTRIM(SycComp.cCompPrnt))
		ENDIF

		lcDataDir = gfGetDataDir(ALLTRIM(SycComp.cCom_dDir))
		lcComDesc = SycComp.cCom_Name
	ELSE
		lcDataDir = ''
		lcComDesc = ''
	ENDIF

	IF USED('UNCMSESS')
		USE IN UnCmSess
	ENDIF

	IF !llUnComChk
		IF !lfChkUnCmS()
			DO CASE

			CASE lcGLVer <> 'S'
*-- IF it is linked to ARIA
				lccAccFile = 'GLAcChar'
				lcBtchFile = 'GLBatch'
				lcHUnPFile = 'GLTrnsHd'
				lcDUnPFile = 'GLTrnsDt'
				lcDistFile = 'GLDist'
				lcFsPrd    = 'FsPrd'
*-- get the data directory of the G/L linked company
				IF SEEK(lcGLCo,'SycComp')
					lcGLCoDDir = gfGetDataDir(ALLTRIM(SycComp.cCom_dDir))
				ELSE
					lcGLCoDDir = ''
				ENDIF
			CASE lcGLVer = 'S'
*-- IF it is linked to SBT
				lccAccFile =  "GLACNT" + lcGLCo    && Chart of account file name
				lcBtchFile =  "GLBTCH" + lcGLCo    && Batch file name
				lcHUnPFile =  "GLHUNP" + lcGLCo    && Header Unposted file name
				lcDUnPFile =  "GLDUNP" + lcGLCo    && Detailed Unposted file name
				lcDistFile =  "GLDIST"             && Distripution file
				lcFsPrd    =  "SYCDFIS"            && Detailed Fiscal file

*-- get the system directory of SBT
				lcSBTSDir = laSetups[3,2]

				=gfOpenFile(lcSBTSDir+'SYSDATA','','SH',@lcSysDaAl,.T.)

				SELECT (lcSysDaAl)
				LOCATE FOR SYSID = "GL" + lcGLCo
				IF !FOUND()
*--lcInfoMsg = 'Company not found !!!'
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
					IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
						loFormSet.oProgress.Hide()
					ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]

					=gfModalGen('INM00269B00000','DIALOG')
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
					IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
						loFormSet.oProgress.Show()
					ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]


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
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
					IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
						loFormSet.oProgress.Hide()
					ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]

					=gfModalGen('INM00315B00000','Dialog',lcComp)
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
					IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
						loFormSet.oProgress.Show()
					ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]


				ELSE
					puCompany = ASUBSCRIPT(laCompany,ASCAN(laCompany,lcComp),1)
					llNoThing = lfvProceed()
				ENDIF
			ENDIF
		ENDIF
	ENDIF
ENDIF

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

SELECT (lnSlct)

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
PARAMETERS loFormSet,loFld

lcComp = LEFT(loFld.VALUE,2)

DO CASE

CASE lcGLVer <> 'S'
	lccAccFile = 'GLAcChar'
	lcBtchFile = 'GLBatch'
	lcHUnPFile = 'GLTrnsHd'
	lcDUnPFile = 'GLTrnsDt'
	lcDistFile = 'GLDist'
	lcFsPrd    = 'FsPrd'
CASE lcGLVer = 'S'
	lccAccFile =  "GLACNT" + lcGLCo    && Chart of account file name
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

loFormSet.Ariaform1.cbSelMod.REFRESH()

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
PARAMETERS loFormSet

IF EMPTY(laModSor)
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'No modules linked to GL!')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00000B00000',.F.,.F.,.F.,LANG_No_modules_linked_to_GL)
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
	IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
		loFormSet.oProgress.Hide()
	ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]

	=gfModalGen('TRM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_No_modules_linked_to_GL,loFormSet.GetHeaderText("LANG_No_modules_linked_to_GL",loFormSet.HeaderAlias)))
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
	IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
		loFormSet.oProgress.Show()
	ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]


*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	RETURN
ENDIF

*-- llApRelCon variable showes if we can continue in eleasing AP
*-- even releasing GL is canceled
IF !EMPTY(lcComp)
	IF !llUnComFnd

** Check if the GL module is installed for the parent company
		IF SEEK(lcGLCo,'SycComp')
			IF ('GL' $ SycComp.MCOMP_MDL)
				llGoOn = .T.
			ELSE
** Message : ' The GL module is not installed for     '
** <OK>
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
				IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
					loFormSet.oProgress.Hide()
				ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]

				=gfModalGen('TRM04132B00000','Dialog',lcGLCo)
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
				IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
					loFormSet.oProgress.Show()
				ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]


				llGoOn = .F.
			ENDIF
			IF ('AP' $ SycComp.MMODLSET)
				llAPRelCon = .T.
			ELSE
				llAPRelCon = .F.
			ENDIF
		ELSE
			llGoOn = .F.
			llAPRelCon = .F.
		ENDIF

		IF llGoOn
			llGL_Link  = ALLTRIM(UPPER(gfGetMemVar('M_Link_GL',lcComp)))   = 'Y'
*- Return only if neither AP nor all other modules linked to GL.
** Check if the system is linked to GL
			llGLRel = llGL_Link AND !(lcGLVer = 'O')

			IF !llGL_Link AND !llAPRel

				IF lcGLVer = 'O'
** Message : Release to G/L is not available if you are not linked to
**           either Aria or SBT.
** <OK>
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
					IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
						loFormSet.oProgress.Hide()
					ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]

					=gfModalGen('INM00318B00000','Dialog')
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
					IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
						loFormSet.oProgress.Show()
					ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]


				ELSE
*-- System has not been linked to gl_link yet
*-- <OK>
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
					IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
						loFormSet.oProgress.Hide()
					ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]

					= gfModalGen("INM00292B00000","Dialog")
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
					IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
						loFormSet.oProgress.Show()
					ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]


				ENDIF
				llGoOn = .F.
				llAPRelCon = .F.
			ELSE
				llGoOn = .T.

				=gfOpenFile(lcGLCoDDir+'Sequence')

				lcSession  = gfsequence('CSESSION')
				lcFiles    = ''
				lnUnCmSeRc = 0
				llNoThing  = IIF(lnUnCmSeRc=0, lfAdUnCmSR(), .T.)

			ENDIF
		ENDIF
	ENDIF
ENDIF

IF cbSelMod = 0
	llGoOn = .F.
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  WAIT "No transaction has been selected." WINDOW
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT LANG_No_ransaction_has_been_selected WINDOW
	WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_No_ransaction_has_been_selected,loFormSet.GetHeaderText("LANG_No_ransaction_has_been_selected",loFormSet.HeaderAlias)) WINDOW
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
ENDIF

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


*B610791,1 TMI 08/03/2014 18:58 [Start]
*IF llGLRel
IF llGLRel OR (llAPRel AND llApInstal)
*B610791,1 TMI 08/03/2014 18:58 [End  ]

*-- If not cancelled
	IF llGoOn
*-- Call routine to lock source files
*B610087,1 SAB 09/17/2012 Fix problem of taking a long time when running GL realese [Start]
*- No need to lock GLDIST file as the program is working on a temp file
*    *B610232,1 HIA 02/10/2013 System Manager to Release to GL module program freeze up  [T20130118.0006][Start]
*DO lpLckFiles WITH "SOURCE"
		DO lpLckFiles WITH "SOURCE"
*B610232,1 HIA 02/10/2013 System Manager to Release to GL module program freeze up  [T20130118.0006][End]
*B610087,1 SAB 09/17/2012 Fix problem of taking a long time when running GL realese [End]
	ENDIF

*-- If not cancelled (if locks succeeded)
	IF llGoOn

		IF lcCurProc < '3'
*-- See if there are any unposted transactions in current dist. file
			SELECT (lcGDistAl)
*E303962,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
*SEEK " "
			=gfSEEK(" ")
*E303962,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]
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
				llGoOn = .F. OR llAPRel
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
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
				IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
					loFormSet.oProgress.Hide()
				ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]

				= gfModalGen('INM00307B00000','Dialog',lcTmpStr)
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
				IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
					loFormSet.oProgress.Show()
				ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]


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
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	      lcTmpStr = IIF(lcDetail = "D", "detail", "summary")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTmpStr = IIF(lcDetail = "D", LANG_detail, LANG_summary)
*E303971,1 MMT 05/02/2018 Modify release to GL program to use new setups from GLSETUP[P20171130.0001][Start]
*lcTmpStr = IIF(lcDetail = "D", IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_detail,loFormSet.GetHeaderText("LANG_detail",loFormSet.HeaderAlias)), IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_summary,loFormSet.GetHeaderText("LANG_summary",loFormSet.HeaderAlias)))
			lcTmpStr = IIF(!(&lcGLSetAl..nglsrelm=1 and &lcGLSetAl..NGLSRELacc = 2 and !EMPTY(&lcGLSetAl..mGLENTACC)), IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_detail,loFormSet.GetHeaderText("LANG_detail",loFormSet.HeaderAlias)), IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_summary,loFormSet.GetHeaderText("LANG_summary",loFormSet.HeaderAlias)))
*E303971,1 MMT 05/02/2018 Modify release to GL program to use new setups from GLSETUP[P20171130.0001][End]
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
			IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
				loFormSet.oProgress.Hide()
			ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]

			lnResp = gfModalGen('QRM00308B00012','Dialog',lcTmpStr)
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
			IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
				loFormSet.oProgress.Show()
			ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]


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
*B610232,1 HIA 02/10/2013 System Manager to Release to GL module program freeze up  [T20130118.0006][Start]
*DO lpLckFiles WITH "SOURCE"

*B610232,1 HIA 02/10/2013 System Manager to Release to GL module program freeze up  [T20130118.0006][End]
	ENDIF

*-- If not cancelled
	IF llGoOn AND lcGLVer = 'S'
*-- Call routine to lock GL destination files
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	    DO lpLckFiles WITH "DESTINATION"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*DO lpLckFiles WITH LANG_DESTINATION
		DO lpLckFiles WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DESTINATION,loFormSet.GetHeaderText("LANG_DESTINATION",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END]
	ENDIF

	IF lcCurProc < '4'
*-- If not cancelled
		IF llGoOn AND llGLRel
			IF lcGLVer = 'A'
*- Don't release AR,MA.IC if the user selected to release AP only.
*-- Call routine to post General Ledger entries
*DO lpAriaRel WITH lnCurrent
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF ALEN(laModTar,1)<>1 OR laModTar[1]<>LANG_AP
				IF ALEN(laModTar,1)<>1 OR laModTar[1]<>IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AP,loFormSet.GetHeaderText("LANG_AP",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

					DO lpAriaRel WITH lnCurrent
				ENDIF
			ELSE
*-- Call routine to post General Ledger entries
				DO lpSBTRel WITH lnCurrent
			ENDIF
		ENDIF

		lcCurProc = '4'
		lcFiles = ''
		llNoThing = lfUpdUnCmS("Open", lcCurProc)
	ENDIF
ENDIF

lcOrgActCo  = oAriaApplication.ActiveCompanyID
lcOrgDDir   = oAriaApplication.DataDir

IF llAPRel AND llApInstal AND llGoOn
	SET ORDER TO POST IN APDIST

	IF lcCurProc < '6'
		llNewSetUp = .F.
		oAriaApplication.ActiveCompanyID = lcComp
		oAriaApplication.DataDir  = lcDataDir
*-- Release AP to G/L Batches
		llFunCall = .T.

*E303295 TMI 11/18/2012 [Start]
*DO (gcAppHome+"AP.APP") WITH 'APGLREL',.F.,.T.
		glMapPath = .F.
		DO (oAriaApplication.ApplicationHome+'AP\APGLREL.FXP')
*E303295 TMI 11/18/2012 [End  ]
	ENDIF
ENDIF
oAriaApplication.ActiveCompanyID = lcOrgActCo
oAriaApplication.DataDir  = lcOrgDDir

lcFiles = ''
llNoThing = lfUpdUnCmS("Close", SPACE(0))
glQuitting = .T.
*=lfUpdateTables()
*=lfCloseTables()
loFormSet.RELEASE

*!*	************************************************************
*!*	*! Name      : lfUpdateTables
*!*	*! Developer : TMI - Tarek Mohamed Ibrahim
*!*	*! Date      : 11/18/2012
*!*	*! Purpose   : Update tables using gfTableUpdate
*!*	************************************************************
*!*	FUNCTION lfUpdateTables

*!*	SELECT GLDIST
*!*	gfTableUpdate()

*!*	SELECT APDIST
*!*	gfTableUpdate()

*!*	SELECT GLBATCH
*!*	gfTableUpdate()

*!*	SELECT GLTRNSDT
*!*	gfTableUpdate()

*!*	SELECT GLTRNSHD
*!*	gfTableUpdate()

*!*	SELECT SEQUENCE
*!*	gfTableUpdate()

*!*	*- End of lfUpdateTables.

************************************************************
*! Name      : lfClose
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/18/2012
*! Purpose   : close a table
************************************************************
FUNCTION lfClose
PARAMETERS lcTable,lcCloseOption
lcCloseOption = IIF(EMPTY(lcCloseOption),' ',lcCloseOption)

IF USED(lcTable)
	DO CASE
	CASE lcCloseOption = 'S'
		=gfCloseTable(lcTable)
	OTHERWISE
		USE IN (lcTable)
	ENDCASE
ENDIF
*- End of lfClose.

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

IF lcGLVer = 'A' OR llAPRel
	=gfOpenFile(lcGLCoDDir+'GLSETUP','','SH',@lcGLSetAl,.T.)

	SELECT (lcGLSetAl)
** Check if GL module has been setup for the company
	IF !&lcGLSetAl..LSETDON
		IF lcCurProc < '3'
			llGoOn = .F.
** Message : " The GL module has not been setup for  "
** Choices : <OK>
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
			IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
				loFormSet.oProgress.Hide()
			ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]

			=gfModalGen('TRM04133B00000','Dialog',lcGLCo)
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
			IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
				loFormSet.oProgress.Show()
			ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]


		ENDIF
	ELSE
		IF lcCurProc < '3'
			lcSuspense = ALLTRIM(Csetsusmj)
		ENDIF

		=gfOpenFile(lcGLCoDDir+'ACCOD','','SH',@lcAcCodAl,.T.)

		IF lcCurProc < '3'
*--lcAccMask variable hold the account mask
			lcAccMask = " "
		ENDIF

		SELECT (lcAcCodAl)
		SET ORDER TO TAG Accsegno
		IF lcCurProc < '3'
			IF !SEEK("")
*-- "Account Mask for Company x not found.
*--  Unable to continue with release."
*-- <OK>
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
				IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
					loFormSet.oProgress.Hide()
				ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]

				=gfModalGen('INM00309B00000','Dialog',lcGLCo)
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
				IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
					loFormSet.oProgress.Show()
				ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]


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

			lcSuspense = SUBSTR(&lcSysDaAl..LINK, 36, 24)
		ENDIF
	ENDIF

	=gfOpenFile(lcGLCoDDir+lccAccFile,'','SH',@lcAccHrAl,.T.)

*-- Check existing of suspense account in chart of account file
	SELECT (lcAccHrAl)
	lcAcChrTag = IIF(lcGLVer<>'S',"ACCTCODE","GLACNT")
	SET ORDER TO TAG (lcAcChrTag)

	IF lcCurProc < '3'
		IF EMPTY(lcSuspense)
*-- You have to setup the ð account major in the GL setup before ð.
*-- <OK>
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	      =gfModalGen('INM00229B00000','DIALOG',"suspense|releasing")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00229B00000','DIALOG',LANG_suspense_releasing)

*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
			IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
				loFormSet.oProgress.Hide()
			ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]

			=gfModalGen('INM00229B00000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_suspense,loFormSet.GetHeaderText("LANG_suspense",loFormSet.HeaderAlias))+"|"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_releasing,loFormSet.GetHeaderText("LANG_releasing",loFormSet.HeaderAlias)))
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
			IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
				loFormSet.oProgress.Show()
			ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]


*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
			llGoOn     = .F.
			llAPRelCon = .F.
		ELSE
			IF !SEEK(lcSuspense)
*-- "Suspense account x not found in chart
*--  of account file. Unable to continue with release."
*-- <OK>
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
				IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
					loFormSet.oProgress.Hide()
				ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]

				=gfModalGen('INM00310B00000','DIALOG',lcSuspense)
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
				IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
					loFormSet.oProgress.Show()
				ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]


				llGoOn     = .F.
				llAPRelCon = .F.
			ENDIF
		ENDIF
	ENDIF
ENDIF

IF llGoOn
*-- Open fiscal calendar detail file from linked company system directory

	=gfOpenFile(IIF(lcGLVer<>'S',lcGLCoDDir,lcSBTSDir)+lcFsPrd,'','SH',@lcFsPrdAl,.T.)

	SELECT (lcFsPrdAl)
	IF lcGLVer <> 'S'
		SET ORDER TO Comfyrprdi
	ENDIF

*-- Set filter
	SELECT (lcFsPrdAl)
	IF lcGLVer <> 'S'
		SET FILTER TO .NOT. EMPTY(Cfisfyear + Cfspprdid) .AND. .NOT. DELETED()
	ELSE
		SET FILTER TO .NOT. EMPTY(compid + yearprd) .AND. .NOT. DELETED()
	ENDIF

	IF lcCurProc < '3'
*-- Summary/detail transfer type
		lcDetail = ALLTRIM(UPPER(gfGetMemVar('M_POST_DET',lcComp)))
	ENDIF

*-- Open Distribution File
*E303962,1 MMT 07/25/2018 GLDIST Conversion to SQL[Start]
*IF FILE(lcDataDir+lcDistFile+".DBF")
	IF .T.
*E303962,1 MMT 07/25/2018 GLDIST Conversion to SQL[End]
*E303962,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
*!*	  =gfOpenFile(lcDataDir+lcDistFile,'Gldistpo','SH',@lcGDistAl,.T.)
		=gfOpenTable(lcDataDir+lcDistFile,'Gldistpo','SH',@lcGDistAl,.T.)
*E303962,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[END]
	ELSE
		llGLRel = .F.
	ENDIF

*-- Open batch status file for GL company
	=gfOpenFile(lcGLCoDDir+lcBtchFile,'','SH',@lcBatchAl,.T.)


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
*E303295 TMI 11/12/2012 [Start]
			lnOrgLen = lnFileStru
*E303295 TMI 11/12/2012 [End  ]
			DIMENSION laFileStru[lnFileStru + 1, 18]
			lnFileStru = lnFileStru + 1
			laFileStru[lnFileStru ,1] = 'nStep'
			laFileStru[lnFileStru ,2] = 'N'
			laFileStru[lnFileStru ,3] = 1
			laFileStru[lnFileStru ,4] = 0
*E303281 TMI 11/12/2012 [Start]
			FOR i= lnOrgLen+1 TO ALEN(laFileStru,1)
				STORE .F. TO laFileStru[i,5],laFileStru[i,6]
				STORE '' TO laFileStru[i,7],laFileStru[i,8],laFileStru[i,9],laFileStru[i,10],laFileStru[i,11],laFileStru[i,12],laFileStru[i,13],laFileStru[i,14],laFileStru[i,15],laFileStru[i,16]
				STORE 0 TO laFileStru[i,17],laFileStru[i,18]
			ENDFOR
*E303281 TMI 11/12/2012 [End  ]

			CREATE TABLE (oAriaApplication.WorkDir + lcBatchTmp) FROM ARRAY laFileStru
			SELECT (lcBatchTmp)
			IF lcGLVer <> 'S'
				INDEX ON cBatchNo TAG BATCHNO OF (oAriaApplication.WorkDir + lcBatchTmp)
			ENDIF
			USE
			USE (oAriaApplication.WorkDir + lcBatchTmp) SHARED
			IF lcGLVer <> 'S'
				SET ORDER TO BATCHNO
			ENDIF
		ELSE
			IF lcGLVer <> 'S'
				SELECT (lcBatchTmp)
				SET ORDER TO BATCHNO
			ENDIF
		ENDIF
	ENDIF    &&IF lcCurProc < '4'


*-- Open unposted transaction detail file for GL company
	=gfOpenFile(lcGLCoDDir+lcDUnPFile,'','SH',@lcDTrnsAl,.T.)

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
*E303295 TMI 11/12/2012 [Start]
			lnOrgLen = lnFileStru
*E303295 TMI 11/12/2012 [End  ]
			DIMENSION laFileStru[lnFileStru + 1, 18]
			lnFileStru = lnFileStru + 1
			laFileStru[lnFileStru ,1] = 'nStep'
			laFileStru[lnFileStru ,2] = 'N'
			laFileStru[lnFileStru ,3] = 1
			laFileStru[lnFileStru ,4] = 0
*E303295 TMI 11/12/2012 [Start]
			FOR i= lnOrgLen+1 TO ALEN(laFileStru,1)
				STORE .F. TO laFileStru[i,5],laFileStru[i,6]
				STORE '' TO laFileStru[i,7],laFileStru[i,8],laFileStru[i,9],laFileStru[i,10],laFileStru[i,11],laFileStru[i,12],laFileStru[i,13],laFileStru[i,14],laFileStru[i,15],laFileStru[i,16]
				STORE 0 TO laFileStru[i,17],laFileStru[i,18]
			ENDFOR
*E303281 TMI 11/12/2012 [End  ]
			CREATE TABLE (oAriaApplication.WorkDir + lcDUnPTmp) FROM ARRAY laFileStru
			SELECT (lcDUnPTmp)
			USE
			USE (oAriaApplication.WorkDir + lcDUnPTmp) SHARED
		ENDIF
	ENDIF

*-- Open unposted transaction header file for GL company
	=gfOpenFile(lcGLCoDDir+lcHUnPFile,'','SH',@lcHTrnsAl,.T.)

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
*E303295 TMI 11/12/2012 [Start]
			lnOrgLen = lnFileStru
*E303295 TMI 11/12/2012 [End  ]
			DIMENSION laFileStru[lnFileStru + 1, 18]
			lnFileStru = lnFileStru + 1
			laFileStru[lnFileStru ,1] = 'nStep'
			laFileStru[lnFileStru ,2] = 'N'
			laFileStru[lnFileStru ,3] = 1
			laFileStru[lnFileStru ,4] = 0
*E303295 TMI 11/12/2012 [Start]
			FOR i= lnOrgLen+1 TO ALEN(laFileStru,1)
				STORE .F. TO laFileStru[i,5],laFileStru[i,6]
				STORE '' TO laFileStru[i,7],laFileStru[i,8],laFileStru[i,9],laFileStru[i,10],laFileStru[i,11],laFileStru[i,12],laFileStru[i,13],laFileStru[i,14],laFileStru[i,15],laFileStru[i,16]
				STORE 0 TO laFileStru[i,17],laFileStru[i,18]
			ENDFOR
*E303281 TMI 11/12/2012 [End  ]

			CREATE TABLE (oAriaApplication.WorkDir + lcHUnPTmp) FROM ARRAY laFileStru
			SELECT(lcHUnPTmp)
			IF lcGLVer <> 'S'
				INDEX ON cBatchNo + cTranNo TAG BATCHTRN OF (oAriaApplication.WorkDir + lcHUnPTmp)
			ENDIF
			USE
			USE (oAriaApplication.WorkDir + lcHUnPTmp) SHARED
			IF lcGLVer <> 'S'
				SET ORDER TO BATCHTRN
			ENDIF
		ELSE
			IF lcGLVer <> 'S'
				SELECT(lcHUnPTmp)
				SET ORDER TO TAG BATCHTRN
			ENDIF
		ENDIF
	ENDIF

	IF lcGLVer <> 'S'
		=gfOpenFile(lcGLCoDDir+'GLSUBJOR','SRCJRNL','SH',@lcGSJorAl,.T.)

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
			SET FULLPATH ON

			=gfOpenFile(lcDataDir+'APDIST','Post')

			=gfOpenFile(lcGLCoDDir+'APSETUP','APSETUP','SH')

		ENDIF

*-- Using chart of Account with alias name "lcLinkChar" refers to
*-- AP main program use Chart of Account with this Alias
		=gfOpenFile(lcGLCoDDir+lccAccFile,'AcctCode','SH',@lcLinkChar,.T.)

		SET FULLPATH &lcSetPthSt.
	ENDIF

	gcSysHome = oAriaApplication.SysPath
*E303295 TMI 11/12/2012 [End  ]
	=gfOpenFile(gcSysHome+'SyGLTran','SyGLTran')

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
*B610961,1 MMT 03/11/2015 Release to GL summary report prints account desc. incorrectly[T20141219.0004][Start]
IF USED(lccAccFile)
	USE IN (lccAccFile)
ENDIF
*B610961,1 MMT 03/11/2015 Release to GL summary report prints account desc. incorrectly[T20141219.0004][End]
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
*E303962,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
*USE IN (lcGDistAl)
	=gfCloseTable(lcGDistAl)
*E303962,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]
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

*E303295 TMI 11/19/2012 [Start] close the sql opened tables here
=gfCloseTable('RETHDR')
=gfCloseTable('POSHDR')


*!*	lfClose('GLDIST')
*!*	lfClose('APDIST')
*!*	lfClose('GLBATCH')
*!*	lfClose('GLTRNSDT')
*!*	lfClose('GLTRNSHD')

*!*	lfClose('SEQUENCE')
*!*	lfClose('FISHD')
*!*	lfClose('SYDFLFLD')
*!*	lfClose('SYDFIELD')
*!*	lfClose('SYDFILES')
*!*	lfClose('SYCCOMP')
*!*	lfClose('SYSDATA')
*!*	lfClose('SEQUENCE')
*!*	lfClose('GLSETUP')
*!*	lfClose('ACCOD')
*!*	lfClose('GLSUBJOR')
*!*	lfClose('APDIST')
*!*	lfClose('APSETUP')
*!*	lfClose('SYGLTRAN')
*!*	lfClose('INVHDR')
*!*	lfClose('RETHDR','S')
*!*	lfClose('POSHDR','S')
*!*	lfClose('CREDIT')
*!*	lfClose('DEBIT')
*!*	lfClose('APSETUP')
*E303295 TMI 11/19/2012 [End  ]

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
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*CASE lcTask = LANG_DESTINATION
CASE lcTask = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DESTINATION,loFormSet.GetHeaderText("LANG_DESTINATION",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

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
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
	IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
		loFormSet.oProgress.Hide()
	ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]

	lnResp = gfModalGen('QRM00311B00034','Dialog')
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
	IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
		loFormSet.oProgress.Show()
	ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]


ENDIF

IF lnResp = 1 .OR. lnResp = 2
*-- Print register, display processing message
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  WAIT WINDOW "Preparing to print posting register..." NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Preparing_to_print_posting_register NOWAIT
	WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Preparing_to_print_posting_register,loFormSet.GetHeaderText("LANG_Preparing_to_print_posting_register",loFormSet.HeaderAlias)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
*-- Setup report titles
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  lcTitle = "ARIA" + IIF(lnResp = 1, " Summary", " Detail") + ;
*!*	    " Posting Register"
*!*	  lcHead1 = "Release from ARIA company " + lcComp + ;
*!*	    " to GL company " + lcGLCo
	lcTitle = "ARIA" + IIF(lnResp = 1, IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_summary,loFormSet.GetHeaderText("LANG_summary",loFormSet.HeaderAlias)), IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_detail,loFormSet.GetHeaderText("LANG_detail",loFormSet.HeaderAlias))) + ;
	IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Posting_Register,loFormSet.GetHeaderText("LANG_Posting_Register",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

	lcHead1 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Release_from_ARIA_company,loFormSet.GetHeaderText("LANG_Release_from_ARIA_company",loFormSet.HeaderAlias)) + lcComp + ;
	IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_to_GL_company,loFormSet.GetHeaderText("LANG_to_GL_company",loFormSet.HeaderAlias)) + lcGLCo
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	lcHead2 = ""
ELSE
*-- Don't print register, display processing message
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  WAIT WINDOW 'Checking ledger posting balance...' NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Checking_ledger_posting_balance NOWAIT
	WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Checking_ledger_posting_balance,loFormSet.GetHeaderText("LANG_Checking_ledger_posting_balance",loFormSet.HeaderAlias)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
ENDIF

SELECT (lcAccHrAl)
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
		CREATE TABLE (oAriaApplication.WorkDir+lcTmpFile) ;
		(GlAccount  C(24)   , ;
		nEqvAmnt   N(12, 2), ;
		GlFYear    C(4)    , ;
		GlPeriod   C(2))
*E303598,1 MMT 08/27/2015 Show Transaction type in Release to GL Summary report[T20150716.0011][Start]
		IF ASCAN(loFormSet.laEvntTrig, PADR('ALTRTABLE',10)) <> 0
			=loFormSet.mDoTrigger(PADR('ALTRTABLE',10))
		ENDIF
*E303598,1 MMT 08/27/2015 Show Transaction type in Release to GL Summary report[T20150716.0011][End]
*-- Close and reopen temporary file with alias
		USE
		USE (oAriaApplication.WorkDir+lcTmpFile) EXCLUSIVE
		INDEX ON GlFYear + GlPeriod + GlAccount TAG (lcTmpFile) OF ;
		(oAriaApplication.WorkDir+lcTmpFile) COMPACT

*-- Copy unposted records to temporary file
		SELECT (lcGDistAl)
*- for transactions not selected do not show them in the report
*E303295 TMI 11/19/2012 [Start]
*SCAN FOR Posted = " " .AND. RECNO(lcGDistAl) <= lnCurrent AND IIF(!EMPTY(lcTrans),CATG_KEY $ lcTrans,.T.)
		GO TOP
*E303962,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
*SEEK " "
		=gfSeek(" ")
*E303962,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]
*B610393,1 TMI 06/20/2013 [Start]
*SCAN REST WHILE Posted = " " FOR RECNO(lcGDistAl) <= lnCurrent AND IIF(!EMPTY(lcTrans),CATG_KEY $ lcTrans,.T.)
		SCAN REST WHILE posted = " " FOR RECNO(lcGDistAl) <= lnCurrent AND ;
			IIF(llAPRel AND EMPTY(lcTrans),.F.,;
			IIF(!EMPTY(lcTrans),CATG_KEY $ lcTrans,.T.))
*B610393,1 TMI 06/20/2013 [End  ]
*E303295 TMI 11/19/2012 [End  ]

*-- Search for existing record
			SELECT (lcTmpFile)
			SEEK &lcGDistAl..GlFYear + &lcGDistAl..GlPeriod + &lcGDistAl..GlAccount
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
*E303598,1 MMT 08/27/2015 Show Transaction type in Release to GL Summary report[T20150716.0011][Start]
			IF ASCAN(loFormSet.laEvntTrig, PADR('UPDTRNTYP',10)) <> 0
				=loFormSet.mDoTrigger(PADR('UPDTRNTYP',10))
			ENDIF
*E303598,1 MMT 08/27/2015 Show Transaction type in Release to GL Summary report[T20150716.0011][end]
			=RLOCK()
			UNLOCK
*-- Update session list variables
			lnSessMin = IIF(lnSessMin = 0, VAL(&lcGDistAl..GlSession), ;
			MIN(lnSessMin, VAL(&lcGDistAl..GlSession)))
			lnSessMax = MAX(lnSessMax, VAL(&lcGDistAl..GlSession))
		ENDSCAN

*-- add the APDIST unposted recored
		IF llAPRel
			SET STEP ON
			SELECT APDIST
			lcOrd = ORDER()
			SET ORDER TO POST
			LOCATE
* B611750,1 Es 3/21/2019 Void Credit memo does not add entries in GLDIST and Release to GL Includes APDIST 109 Payment records and moves to Suspense account because it has empty GL account.[Start]
* SCAN WHILE !LAPDPOST
			SCAN WHILE !LAPDPOST for capdactid<> 'B'
* B611750,1 Es 3/21/2019 Void Credit memo does not add entries in GLDIST and Release to GL Includes APDIST 109 Payment records and moves to Suspense account because it has empty GL account.[End]

				SELECT (lcTmpFile)
				SEEK APDIST.Cfisfyear + APDIST.Cfspprdid + APDIST.CAPDGLACT
*IF .NOT. FOUND()
*B611337 post to Gl ignore the records with no accounts [begin]
*-- If existing record not found
				IF .NOT. FOUND() AND !EMPTY(ALLTRIM(APDIST.CAPDGLACT))
*-- Add record to temporary file for report
					APPEND BLANK
					REPLACE GlAccount  WITH APDIST.CAPDGLACT ;
					GlFYear    WITH APDIST.Cfisfyear ;
					GlPeriod   WITH APDIST.Cfspprdid
				ENDIF
*-- Update amount
				REPLACE nEqvAmnt  WITH nEqvAmnt + APDIST.nEqvAmnt
*-- If existing record not found

*B611337 post to Gl ignore the records with no accounts [End]
				=RLOCK()
				UNLOCK
			ENDSCAN
			SET ORDER TO &lcOrd
		ENDIF
	ELSE
		SELECT(lcTmpFile)
		SET ORDER TO TAG (lcTmpFile)
	ENDIF
*-- Update report header from session variables
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  lcHead2 = "From Session " + LTRIM(STR(lnSessMin, 6, 0)) + ;
*!*	    "-" + LTRIM(STR(lnSessMax, 6, 0))
	lcHead2 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_From_Session,loFormSet.GetHeaderText("LANG_From_Session",loFormSet.HeaderAlias)) + LTRIM(STR(lnSessMin, 6, 0)) + ;
	"-" + LTRIM(STR(lnSessMax, 6, 0))
*N000682,1 04/16/2013 RAS Globlization changes[END ]
CASE lnResp = 2
*-- Detail Report
*-- Open invoice header file

	IF ('AR' $ SycComp.MMODLSET)
		=gfOpenFile(lcDataDir+'INVHDR','INVHDR','SH',@lcInvHdAl,.T.)
	ENDIF

*-- Open return header file
	IF ('RM' $ SycComp.MMODLSET)
		=gfOpenTable(lcDataDir+'RETHDR','RETHDR','SH',@lcRetHdAl,.T.)
	ENDIF

*-- Open cutticket header file
*!*	IF ('MF' $ SYCCOMP.mModlSet)
*!*	  =gfOpenFile(lcDataDir+'CUTTKTH','CUTTKTH','SH',@lcCutTkAl,.T.)
*!*	ENDIF

*-- Open style purchase order header file
*E303295,4 TMI 12/08/2012 [Start] allow to open POSHDR when mf is installed[Start]
*IF ('PO' $ SYCCOMP.mModlSet) OR ('PS' $ SYCCOMP.mModlSet)
	IF ('PO' $ SycComp.MMODLSET) OR ('MF' $ SycComp.MMODLSET) OR ('PS' $ SycComp.MMODLSET)
		=gfOpenTable(lcDataDir+'POSHDR','POSHDR','SH',@lcPosHdAl,.T.)
*E303295,4 TMI 12/08/2012 [End]
	ENDIF

*-- Open Credit file
	IF ('AR' $ SycComp.MMODLSET)
		=gfOpenFile(lcDataDir+'CREDIT','','SH',@lcCredtAl,.T.)
	ENDIF

*-- Open Debit file
	IF ('AR' $ SycComp.MMODLSET)
		=gfOpenFile(lcDataDir+'DEBIT','','SH',@lcDebitAl,.T.)
	ENDIF

*-- Select distribution file
	SELECT (lcGDistAl)
*-- IF lcCurProc < '2' This means that uncomplete session is dedected
*--                  and there is no need to craete the temp file and
*--                  fill it
	IF lcCurProc < '2'
*-- Create the temporary file
		lcTmpFile = gfTempName()             && Temp. file name
*- add a filter for transactions not selected to not show them in the report
*COPY TO (oAriaApplication.WorkDir+lcTmpFile) ;
*  FIELDS ;
*  &lcGDistAl..Tran_no, ;
*  &lcGDistAl..GlAccount, ;
*  &lcGDistAl..GlFYear, ;
*  &lcGDistAl..GlPeriod, ;
*  &lcGDistAl..nEqvAmnt, ;
*  &lcGDistAl..GlSession, ;
*  &lcGDistAl..Tran_Date, ;
*  &lcGDistAl..Tran_Type ;
*  FOR posted = " " .AND. RECNO(lcGDistAl) <= lnCurrent
		CREATE TABLE (oAriaApplication.WorkDir+lcTmpFile) ( ;
		Tran_no C(12),;
		GlAccount C(24), ;
		GlFYear C(4), ;
		GlPeriod C(2), ;
		nEqvAmnt N(15,2), ;
		GlSession C(8), ;
		Tran_Date D, ;
		Tran_Type C(2))
		SELECT (lcGDistAl)
*E303295 TMI 11/19/2012 [Start]

*SET ORDER TO post
		LOCATE
*E303962,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
*SEEK " "
		=gfSEEK(" ")
*E303962,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]
*E303295 TMI 11/19/2012 [End  ]
*- if nothing was selected then loop for all
*B610393,1 TMI 06/20/2013 [Start]
*SCAN REST WHILE posted = " " FOR RECNO(lcGDistAl) <= lnCurrent AND IIF(!EMPTY(lcTrans),CATG_KEY $ lcTrans,.T.)
		SCAN REST WHILE posted = " " FOR RECNO(lcGDistAl) <= lnCurrent AND ;
			IIF(llAPRel AND EMPTY(lcTrans),.F.,;
			IIF(!EMPTY(lcTrans),CATG_KEY $ lcTrans,.T.))
*B610393,1 TMI 06/20/2013 [End  ]
			SCATTER MEMVAR
			INSERT INTO (oAriaApplication.WorkDir+lcTmpFile) FROM MEMVAR
		ENDSCAN

		IF llAPRel
			SELECT APDIST
			lcOrd = ORDER()
			SET ORDER TO POST
			LOCATE
* B611750,1 Es 3/21/2019 Void Credit memo does not add entries in GLDIST and Release to GL Includes APDIST 109 Payment records and moves to Suspense account because it has empty GL account.[Start]
*SCAN WHILE !LAPDPOST
			SCAN WHILE !LAPDPOST for capdactid<> 'B'
* B611750,1 Es 3/21/2019 Void Credit memo does not add entries in GLDIST and Release to GL Includes APDIST 109 Payment records and moves to Suspense account because it has empty GL account.[End]

				SELECT (lcTmpFile)
*-- Add record to temporary file for report
				APPEND BLANK
				REPLACE Tran_no    WITH APDIST.CAPDREF   ;
				GlAccount  WITH APDIST.CAPDGLACT ;
				GlFYear    WITH APDIST.Cfisfyear ;
				GlPeriod   WITH APDIST.Cfspprdid ;
				nEqvAmnt   WITH APDIST.nEqvAmnt  ;
				GlSession  WITH APDIST.CAPSESSNO ;
				Tran_Date  WITH APDIST.DAPDTRDAT ;
				Tran_Type  WITH APDIST.CAPDTRTYP
				=RLOCK()
				UNLOCK
			ENDSCAN
			SET ORDER TO &lcOrd

*- Create the temp index to use it in setting the relation into the APDIST file
			SELECT APDIST

*B611293,1 MHM 04/04/2017 Issue with releasing Gl [T20170327.0002][Begin]
*INDEX ON CAPDGLACT+CAPSESSNO TO (oAriaApplication.WorkDir+lcAPDIST)
			INDEX ON CAPDGLACT+CAPSESSNO+CINVNO TO (oAriaApplication.WorkDir+lcAPDIST)
*B611293,1 MHM 04/04/2017 Issue with releasing Gl [T20170327.0002][END]

		ENDIF

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
		(oAriaApplication.WorkDir+lcTmpFile) COMPACT
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

IF lnResp = 1 .OR. lnResp = 2
	=lpPrnPost(lnResp)
ENDIF
*-- Calculate balance
CALCULATE SUM (nEqvAmnt) TO lnBalance

*-- Close and delete temporary files
IF USED(lcTmpFile)
	USE IN (lcTmpFile)
ENDIF

ERASE oAriaApplication.WorkDir+lcTmpFile+".dbf"
ERASE oAriaApplication.WorkDir+lcTmpFile+".cdx"

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
SET ORDER TO TAG AcctCode IN (lcAccHrAl)

*-- Declare local variables private
PRIVATE lcDocExp,  lcSumExp, lcNdxExp, lcYearPrd,  lcSess   , ;
ldGlDate,  lnTDrTot, lnTCrTot,  lnBDrTot,  lnBCrTot , ;
lcGlTDes,  lnAmount, lcGlAcnt, lcDocKey ,  lcSubDetl, ;
lcErrMsg, ldPrdBDat, ldPrdEDat

IF lcCurProc <'3'

*-- Create temp file for GLDist File and Build filtered index
	lcTmpFile   = gfTempName()
*E303962,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
	SELECT (lcGDistAl)
	=gfSeek(' ',lcGDistAl,'GLDISTPO')
*E303962,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]
	IF EMPTY(lcTrans)
		SELECT *,.F. AS PASS,RECNO() AS nRecNo, lfSorcMod(&lcGDistAl..Tran_Type) AS Csrcmodul ;
		FROM (lcGDistAl) ;
		INTO DBF (oAriaApplication.WorkDir+lcTmpFile) ;
		WHERE EMPTY(posted) ;
		AND   RECNO() <= lnMaxRec
	ELSE

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
		SELECT *,.F. AS PASS,RECNO() AS nRecNo, lfSorcMod(&lcGDistAl..Tran_Type) AS Csrcmodul ;
		FROM (lcGDistAl) ;
		INTO DBF (oAriaApplication.WorkDir+lcTmpFile) ;
		WHERE EMPTY(posted) ;
		AND   RECNO() <= lnMaxRec ;
		AND   CATG_KEY $ lcTrans &lcMatCon
	ENDIF

*-- Setup summarization and index key expressions
*E303971,1 MMT 05/02/2018 Modify release to GL program to use new setups from GLSETUP[P20171130.0001][Start]
*!*      DO CASE
*!*        CASE lcDetail = "S"
*!*          *-- Summary
*!*          lcSumExp = [.t.]
*!*          lcNdxExp = [GlFYear + GlPeriod + Csrcmodul + GlAccount]
*!*          lcDocExp = [""]
*!*          lcDocKey = ""

*!*        CASE lcDetail = "D"
*!*          *-- Detail
*!*          lcSumExp = lcTmpFile+[.GlAcntType = "C"]
*!*          lcNdxExp = [GlFYear + GlPeriod + Csrcmodul + GlSession + IIF(] + lcSumExp + ;
*!*            [, "C", "D") + GlAccount]

*!*          *-- Set expression for document number lookup
*!*          lcDocExp = lcTmpFile+[.Tran_no]

*!*      ENDCASE
	IF &lcGLSetAl..nGlSrbpw= 0
		REPLACE nGLSRBPW WITH 1 IN (lcGLSetAl)
		IF lcDetail = "S"
			REPLACE nGLsrelm WITH 1 ,;
			NGLSRELACC WITH 2 ,;
			mglentacc WITH '' IN (lcGLSetAl)
			IF !USED('syglcatg')
				=gfOpenTable(oAriaApplication.SysPath+'syglcatg',.F.,'SH')
			ENDIF
			SELECT 'syglcatg'
			=gfSeek('')
			LOCATE
			SCAN
				REPLACE mglentacc WITH mglentacc + '|' + syglcatg.CatgKey IN (lcGLSetAl)
			ENDSCAN
			=gfCloseTable('syglcatg')
		ELSE
			REPLACE nGLsrelm WITH 2 ,;
			NGLSRELACC WITH 1 ,;
			mglentacc WITH ''  IN (lcGLSetAl)
		ENDIF
		SELECT  (lcGLSetAl)
* =TABLEUPDATE(.T.)
	ENDIF
	lnNoOfPeriod = &lcGLSetAl..nGlSrbpw
	lnNoOfPeriod = lnNoOfPeriod - 1
	DO CASE
	CASE &lcGLSetAl..nglsrelm = 1 && Module
		lcNdxExp = [GlFYear + GlPeriod + Csrcmodul + GlAccount]
		lcDocExp = [""]
		lcDocKey = ""

	CASE &lcGLSetAl..nglsrelm = 2 && GL Session
		lcNdxExp = [GlFYear + GlPeriod + Csrcmodul + GlSession + GlAccount]
		lcDocExp = lcTmpFile+[.Tran_no]

	CASE &lcGLSetAl..nglsrelm = 3 && Tran Type
		lcNdxExp = [GlFYear + GlPeriod + Csrcmodul+ Tran_type + GlAccount]
		lcDocExp = lcTmpFile+[.Tran_no]

	ENDCASE
	IF &lcGLSetAl..NGLSRELACC = 1
		lcSumExp = lcTmpFile+[.GlAcntType = "C"]
	ELSE
		IF EMPTY(&lcGLSetAl..mGLEntAcc)
			lcDocExp = lcTmpFile+[.Tran_no]
		ENDIF
		lcSumExp = lcTmpFile+[.catg_key $ "]+&lcGLSetAl..mGLEntAcc+["]
	ENDIF
*E303971,1 MMT 05/02/2018 Modify release to GL program to use new setups from GLSETUP[P20171130.0001][End]
	SELECT(lcTmpFile)
*--  Create index in lctmpfile on batch+tran (Start)
	gcWorkDir = oAriaApplication.WorkDir
	INDEX ON glbatch+cTrnsLedn TAG BatTrn OF &gcWorkDir.&lcTmpFile COMPACT
*--

	INDEX ON &lcNdxExp + &lcDocExp TAG (lcTmpFile) OF ;
	&gcWorkDir.&lcTmpFile COMPACT ADDITIVE

	INDEX ON Tran_Type+Tran_no TAG Tran_no OF &gcWorkDir.&lcTmpFile COMPACT ADDITIVE

	USE
	USE (oAriaApplication.WorkDir+lcTmpFile) SHARED
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

*- Call lfGetPerd() to get the period and year if both or date are empty.
*-- If the function change anything in the file, loop. Refer to documentation in the function.
		IF lfGetPerd()
			LOOP
		ENDIF


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

*E303971,1 MMT 05/02/2018 Modify release to GL program to use new setups from GLSETUP[P20171130.0001][Start]
		lcYearLoop = &lcTmpFile..GlFYear
		lcPeriodLoop =  &lcTmpFile..GlPeriod
*E303971,1 MMT 05/02/2018 Modify release to GL program to use new setups from GLSETUP[P20171130.0001][End]

*--  lcSorcMod variable that hold transaction source module (Start)
		lcSorcMod = &lcTmpFile..Csrcmodul
*--

*-- Get period begin and end date
		IF SEEK(lcYearPrd,lcFsPrdAl)
			ldPrdBDat = &lcFsPrdAl..dFsPpBgDt
			ldPrdEDat = &lcFsPrdAl..dFsPpEnDt
		ENDIF

*-- Add batch status record to GLBTCH temporary file and
*-- leave marked with object lock

		SELECT &lcBatchTmp
		APPEND BLANK
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	    REPLACE Cbatchno  WITH PADL(lcBatchNo,FSIZE('CBATCHNO'),'0'), ;
*!*	      Cbatdesc  WITH "Posting from ARIA, " + lcComDesc, ;
*!*	      Cbatpyr   WITH &lcTmpFile..GlFYear, ;
*!*	      Dbatpbeg  WITH ldPrdBDat, ;
*!*	      Dbatpend  WITH ldPrdEDat, ;
*!*	      Cbatrefer WITH "On "+ DTOC(DATE()), ;
*!*	      Cbattype  WITH "L", ;
*!*	      Cbatstat  WITH "U", ;
*!*	      Csrcmodul WITH &lcTmpFile..cSrcModul ,;
*!*	      Ccomp_id  WITH lcComp
		REPLACE cBatchNo  WITH PADL(lcBatchNo,FSIZE('CBATCHNO'),'0'), ;
		Cbatdesc  WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Posting_from_ARIA,loFormSet.GetHeaderText("LANG_Posting_from_ARIA",loFormSet.HeaderAlias)) + lcComDesc, ;
		Cbatpyr   WITH &lcTmpFile..GlFYear, ;
		Dbatpbeg  WITH ldPrdBDat, ;
		Dbatpend  WITH ldPrdEDat, ;
		Cbatrefer WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_On,loFormSet.GetHeaderText("LANG_On",loFormSet.HeaderAlias))+ DTOC(DATE()), ;
		Cbattype  WITH "L", ;
		Cbatstat  WITH "U", ;
		Csrcmodul WITH &lcTmpFile..Csrcmodul ,;
		cComp_ID  WITH lcComp
*N000682,1 04/16/2013 RAS Globlization changes[END ]
		=RLOCK()
		UNLOCK

*-- Process records for batch
		SELECT (lcTmpFile)

*--  Add source module to transaction records (Start)
*E303971,1 MMT 05/02/2018 Modify release to GL program to use new setups from GLSETUP[P20171130.0001][Start]
*DO WHILE &lcTmpFile..GlFYear + &lcTmpFile..GlPeriod + &lcTmpFile..Csrcmodul = ;
lcYearPrd + lcSorcMod ;
.AND. .NOT. EOF() .AND. llGoOn
		DO WHILE &lcTmpFile..GlFYear + &lcTmpFile..GlPeriod + &lcTmpFile..Csrcmodul = ;
			lcYearLoop AND &lcTmpFile..Csrcmodul = lcSorcMod  AND BETWEEN(VAL(&lcTmpFile..GlPeriod) ,VAL(lcPeriodLoop) ,VAL(lcPeriodLoop) +lnNoOfPeriod);
			.AND. .NOT. EOF() .AND. llGoOn
*E303971,1 MMT 05/02/2018 Modify release to GL program to use new setups from GLSETUP[P20171130.0001][End]

*-- Setup session ID and date field values for GLHUNP update
*E303971,1 MMT 05/02/2018 Modify release to GL program to use new setups from GLSETUP[P20171130.0001][Start]
*lcSess   = IIF(lcDetail = "D", &lcTmpFile..GlSession, "")
			lcYearPrd = &lcTmpFile..GlFYear + &lcTmpFile..GlPeriod
			lcSess   = IIF(&lcGLSetAl..nglsrelm=2 , &lcTmpFile..GlSession,IIF(&lcGLSetAl..nglsrelm=3, &lcTmpFile..Tran_TYPE,''))
			IF SEEK(&lcTmpFile..GlFYear + &lcTmpFile..GlPeriod,lcFsPrdAl)
				ldPrdEDat= &lcFsPrdAl..dFsPpEnDt
			ENDIF
*E303971,1 MMT 05/02/2018 Modify release to GL program to use new setups from GLSETUP[P20171130.0001][End]

			ldGlDate = &lcTmpFile..Tran_Date

*-- Get transaction number
			lcTranNo = lcTranNo + 1
*- Decalre lnEntryNo to hold entery No. (Start)
			lnEntryNo = 0

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
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	      REPLACE Cbatchno   WITH PADL(lcBatchNo,FSIZE('CBATCHNO'),'0'), ;
*!*	        Ctranno    WITH PADL(lcTranNo,FSIZE('CTRANNO'),'0'), ;
*!*	        Ctrndesc   WITH "Posting from ARIA, " + lcComDesc , ;
*!*	        Ctrnrefer  WITH "On "+ DTOC(DATE()), ;
*!*	        Dtrnpdate  WITH ldGlDate, ;
*!*	        Ctrnpyr    WITH SUBSTR(lcYearPrd,1,4), ;
*!*	        Ctrnpprd   WITH SUBSTR(lcYearPrd,5,2), ;
*!*	        Ctrnstat   WITH "U", ;
*!*	        Ctrntype   WITH "N", ;
*!*	        Csrcmodul  WITH &lcTmpFile..cSrcModul, ;
*!*	        Ccomp_id   WITH lcComp, ;
*!*	        Ctrnrever  WITH "N", ;
*!*	        Cstandard  WITH "Y", ;
*!*	        Csrcjrnl   WITH lcCsrcjrnl
			REPLACE cBatchNo   WITH PADL(lcBatchNo,FSIZE('CBATCHNO'),'0'), ;
			cTranNo    WITH PADL(lcTranNo,FSIZE('CTRANNO'),'0'), ;
			Ctrndesc   WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Posting_from_ARIA,loFormSet.GetHeaderText("LANG_Posting_from_ARIA",loFormSet.HeaderAlias)) + lcComDesc , ;
			Ctrnrefer  WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_On,loFormSet.GetHeaderText("LANG_On",loFormSet.HeaderAlias))+ DTOC(DATE()), ;
			Dtrnpdate  WITH ldGlDate, ;
			Ctrnpyr    WITH SUBSTR(lcYearPrd,1,4), ;
			Ctrnpprd   WITH SUBSTR(lcYearPrd,5,2), ;
			Ctrnstat   WITH "U", ;
			Ctrntype   WITH "N", ;
			Csrcmodul  WITH &lcTmpFile..Csrcmodul, ;
			cComp_ID   WITH lcComp, ;
			Ctrnrever  WITH "N", ;
			Cstandard  WITH "Y", ;
			CSRCJRNL   WITH lcCsrcjrnl
*N000682,1 04/16/2013 RAS Globlization changes[END ]
			=RLOCK()
			UNLOCK

*-- Process records for transaction
			SELECT (lcTmpFile)

*-- - Add source module to transaction records (Start)
*E303971,1 MMT 05/02/2018 Modify release to GL program to use new setups from GLSETUP[P20171130.0001][Start]
*DO WHILE &lcTmpFile..GlFYear + &lcTmpFile..GlPeriod + ;
&lcTmpFile..Csrcmodul = lcYearPrd + lcSorcMod ;
.AND. IIF(EMPTY(lcSess), .T., (&lcTmpFile..GlSession = ;
lcSess)) .AND. .NOT. EOF()
			DO WHILE &lcTmpFile..GlFYear + &lcTmpFile..GlPeriod + &lcTmpFile..Csrcmodul = lcYearPrd + lcSorcMod AND ;
				IIF(EMPTY(lcSess), .T., IIF(&lcGLSetAl..nglsrelm=2,(GlSession = lcSess),(Tran_TYPE = lcSess))) AND !EOF()
*E303971,1 MMT 05/02/2018 Modify release to GL program to use new setups from GLSETUP[P20171130.0001][End]
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


*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	        lcTmpStr = "Year/Period: " + ;
*!*	          LEFT(lcYearPrd, 4) + "/" + RIGHT(lcYearPrd, 2) + ;
*!*	          ", account " + ALLTRIM(lcGlAcnt)
				lcTmpStr = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Year_Period,loFormSet.GetHeaderText("LANG_Year_Period",loFormSet.HeaderAlias)) + ;
				LEFT(lcYearPrd, 4) + "/" + RIGHT(lcYearPrd, 2) + ;
				IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_account,loFormSet.GetHeaderText("LANG_account",loFormSet.HeaderAlias)) + ALLTRIM(lcGlAcnt)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
				lnCount = lnCount + 1
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	        =gfThermo(lnReccount,lnCount,'Generating...',lcTmpStr)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfThermo(lnReccount,lnCount,LANG_Generating,lcTmpStr)
				=gfThermo(lnRecCount,lnCount,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Generating,loFormSet.GetHeaderText("LANG_Generating",loFormSet.HeaderAlias)),lcTmpStr)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END]

*-- Setup description
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	        lcGlTDes = "Transaction #: " + LTRIM(Tran_No)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcGlTDes = LANG_Transaction_No + LTRIM(Tran_No)
				lcGlTDes = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Transaction_No,loFormSet.GetHeaderText("LANG_Transaction_No",loFormSet.HeaderAlias)) + LTRIM(Tran_no)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]

*-- If detail posting
*E303971,1 MMT 05/02/2018 Modify release to GL program to use new setups from GLSETUP[P20171130.0001][Start]
*IF .NOT. (lcDetail = "S" .OR. (lcDetail = "D" ;
.AND. &lcSumExp.))
				IF NOT  ((&lcGLSetAl..NGLSRELACC = 1 OR (&lcGLSetAl..NGLSRELACC = 2)) AND &lcSumExp.)
*E303971,1 MMT 05/02/2018 Modify release to GL program to use new setups from GLSETUP[P20171130.0001][End]
*-- Setup subledger link field value
					lcSubDetl = lcDocKey
				ENDIF

				lnEntryNo = lnEntryNo + 1

*-- Setup amount and move to next source record to be processed
*-- Mark record with batch number and GL company number
*E303971,1 MMT 05/02/2018 Modify release to GL program to use new setups from GLSETUP[P20171130.0001][Start]
*IF lcDetail = "S" .OR. (lcDetail = "D" .AND. &lcSumExp)
				IF ((&lcGLSetAl..NGLSRELACC = 1 OR (&lcGLSetAl..NGLSRELACC = 2)) AND &lcSumExp.)
*E303971,1 MMT 05/02/2018 Modify release to GL program to use new setups from GLSETUP[P20171130.0001][End]
*-- Summary, or detail and on control account

*-- Transaction description should be blank for summarized totals

*- dont update Desc. only in case of summrize [Start]
*E303971,1 MMT 05/02/2018 Modify release to GL program to use new setups from GLSETUP[P20171130.0001][Start]
*IF lcDetail = "S"
					IF &lcGLSetAl..nGLSRELM = 1
*E303971,1 MMT 05/02/2018 Modify release to GL program to use new setups from GLSETUP[P20171130.0001][End]

						lcGlTDes = " "

					ENDIF

*-- Accumulate amount while on same period/account/session/control
					lnAmount = 0

*- Define a variable to hold the current transation #
					lcTrnNo = Tran_no
*E303971,1 MMT 05/02/2018 Modify release to GL program to use new setups from GLSETUP[P20171130.0001][Start]
*SCAN WHILE GlFYear + GlPeriod + Csrcmodul = lcYearPrd + lcSorcMod .AND. GlAccount = lcTGlAcnt ;
.AND. IIF(EMPTY(lcSess), .T., (&lcTmpFile..GlSession = ;
lcSess)) .AND. &lcSumExp.
					SCAN WHILE GlFYear + GlPeriod + Csrcmodul = lcYearPrd + lcSorcMod AND GlAccount = lcTGlAcnt ;
						.AND. IIF(EMPTY(lcSess), .T., IIF(&lcGLSetAl..nglsrelm=2,(&lcTmpFile..GlSession = ;
						lcSess),(&lcTmpFile..Tran_TYPE = lcSess))) .AND. &lcSumExp.
*E303971,1 MMT 05/02/2018 Modify release to GL program to use new setups from GLSETUP[P20171130.0001][End]

*-- Update batch and company numbers

						REPLACE glbatch   WITH PADL(lcBatchNo,FSIZE('GLBATCH'),'0') ,;
						nEntryNo  WITH lnEntryNo ,;
						cTrnsLedn WITH PADL(lcTranNo,FSIZE('cTrnsLedn'),'0')

						=RLOCK()
						UNLOCK

*-- Increment amount
						lnAmount = lnAmount + &lcTmpFile..nEqvAmnt

*- if tran_no is changed then empty the description
						IF lcTrnNo <> Tran_no
							lcGlTDes = ''
						ENDIF
						lcTrnNo = Tran_no

					ENDSCAN

				ELSE
*-- Detail and not on control account

*-- Accumulate amount while on same period/account/session/document
					lnAmount = 0

*E303971,1 MMT 05/02/2018 Modify release to GL program to use new setups from GLSETUP[P20171130.0001][Start]
*SCAN WHILE GlFYear + GlPeriod + Csrcmodul = lcYearPrd + lcSorcMod .AND. GlAccount = lcTGlAcnt ;
.AND. IIF(EMPTY(lcSess), .T., (&lcTmpFile..GlSession = ;
lcSess)) .AND. &lcDocExp. = lcDocKey
					SCAN WHILE GlFYear + GlPeriod + Csrcmodul = lcYearPrd + lcSorcMod AND GlAccount = lcTGlAcnt ;
						.AND. IIF(EMPTY(lcSess), .T., IIF(&lcGLSetAl..nglsrelm=2,(&lcTmpFile..GlSession = ;
						lcSess),(&lcTmpFile..Tran_TYPE = lcSess))) .AND. &lcDocExp. = lcDocKey

*E303971,1 MMT 05/02/2018 Modify release to GL program to use new setups from GLSETUP[P20171130.0001][End]

*-- Update batch and company numbers
						REPLACE glbatch   WITH PADL(lcBatchNo,FSIZE('GLBATCH'),'0') ,;
						nEntryNo  WITH lnEntryNo ,;
						cTrnsLedn WITH PADL(lcTranNo,FSIZE('cTrnsLedn'),'0')
						=RLOCK()
						UNLOCK

*-- Increment amount
						lnAmount = lnAmount + &lcTmpFile..nEqvAmnt

					ENDSCAN
				ENDIF && lcDetail = "N" .OR. etc.

*-- Add transaction detail record to GLDUNP
				SELECT &lcDUnPTmp
				APPEND BLANK
				REPLACE Cacctcode  WITH lcGlAcnt, ;
				cBatchNo   WITH PADL(lcBatchNo,FSIZE('CBATCHNO'),'0'), ;
				cTranNo    WITH PADL(lcTranNo,FSIZE('CTRANNO'),'0'), ;
				Ctrdtexp   WITH lcGlTDes, ;
				Dtrnpdate  WITH ldGlDate, ;
				Namount    WITH ABS(lnAmount), ;
				Cdrorcr    WITH IIF(lnAmount < 0,'C','D'), ;
				Ctrnpyr    WITH SUBSTR(lcYearPrd,1,4) , ;
				Ctrnpprd   WITH SUBSTR(lcYearPrd,5,2) ,;
				nEntryNo   WITH lnEntryNo

				=RLOCK()
				UNLOCK

*-- Increment debit and credit totals for transaction and batch
				lnTDrTot = lnTDrTot + IIF(Cdrorcr = 'D', Namount, 0)
				lnTCrTot = lnTCrTot + IIF(Cdrorcr = 'C', Namount, 0)

*-- Already on next detail record
				SELECT (lcTmpFile)
			ENDDO
*E303971,1 MMT 05/02/2018 Modify release to GL program to use new setups from GLSETUP[P20171130.0001][Start]
			Replace Dbatpend  WITH ldPrdEDat in &lcBatchTmp
*E303971,1 MMT 05/02/2018 Modify release to GL program to use new setups from GLSETUP[P20171130.0001][End]
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	      = gfThermo(lnReccount,lnReccount,'Generating...',lcTmpStr)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfThermo(lnReccount,lnReccount,LANG_Generating,lcTmpStr)
			= gfThermo(lnRecCount,lnRecCount,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Generating,loFormSet.GetHeaderText("LANG_Generating",loFormSet.HeaderAlias)),lcTmpStr)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]

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
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
				IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
					loFormSet.oProgress.Hide()
				ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]

				= gfModalGen("INM00305B00000","Dialog",lcTmpStr)
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
				IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
					loFormSet.oProgress.Show()
				ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]



*-- Add suspense transaction detail record to GLDUNP
				SELECT &lcDUnPTmp
				APPEND BLANK
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	        REPLACE Cacctcode  WITH lcSuspense, ;
*!*	          Cbatchno   WITH PADL(lcBatchNo,FSIZE('CBATCHNO'),'0'), ;
*!*	          Ctranno    WITH PADL(lcTranNo,FSIZE('CTRANNO'),'0'), ;
*!*	          Ctrdtexp   WITH "Suspense - Out of Balance", ;
*!*	          Dtrnpdate  WITH ldGlDate, ;
*!*	          Namount    WITH ABS(lnAmount), ;
*!*	          Cdrorcr    WITH IIF(lnAmount < 0, "C", "D"), ;
*!*	          Ctrnpyr    WITH SUBSTR(lcYearPrd,1,4) , ;
*!*	          Ctrnpprd   WITH SUBSTR(lcYearPrd,5,2)
				REPLACE Cacctcode  WITH lcSuspense, ;
				cBatchNo   WITH PADL(lcBatchNo,FSIZE('CBATCHNO'),'0'), ;
				cTranNo    WITH PADL(lcTranNo,FSIZE('CTRANNO'),'0'), ;
				Ctrdtexp   WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Suspense_Out_of_Balance,loFormSet.GetHeaderText("LANG_Suspense_Out_of_Balance",loFormSet.HeaderAlias)), ;
				Dtrnpdate  WITH ldGlDate, ;
				Namount    WITH ABS(lnAmount), ;
				Cdrorcr    WITH IIF(lnAmount < 0, "C", "D"), ;
				Ctrnpyr    WITH SUBSTR(lcYearPrd,1,4) , ;
				Ctrnpprd   WITH SUBSTR(lcYearPrd,5,2)
*N000682,1 04/16/2013 RAS Globlization changes[END ]
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
	SELECT (lcTmpFile)
	SET ORDER TO 0

	lcCurProc = '3'
	llNoThing = lfUpdUnCmS("Open",lcCurProc)
ENDIF

*-- If not cancelled
IF llGoOn

*-- Now update master files

*-- Call routine to lock GL destination files
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  DO lpLckFiles WITH "DESTINATION"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*DO lpLckFiles WITH LANG_DESTINATION
	DO lpLckFiles WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DESTINATION,loFormSet.GetHeaderText("LANG_DESTINATION",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]

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

*B610879,1 TMI 10/12/2014 16:56 [Start] use actual search instead of relation
			=SEEK(&lcDUnPTmp..cBatchNo + &lcDUnPTmp..cTranNo, lcHUnPTmp )
			=SEEK(&lcHUnPTmp..cBatchNo , lcBatchTmp )
*B610879,1 TMI 10/12/2014 16:56 [End  ]

*-- New batch
			IF lcBatKey <> &lcDUnPTmp..cBatchNo
				lcBatKey = &lcDUnPTmp..cBatchNo

				IF &lcDUnPTmp..nStep < 1
*-- Get new bacth number
					lcBatchNo = gfsequence("cBatchNo",lcGLCo)
*-- Update Distribution file with batch number and company ID
*-- and mark this batch as posted.
					SELECT (lcTmpFile)

					REPLACE ALL glbatch WITH lcBatchNo, ;
					GLCOMP WITH lcGLCo,  ;
					posted WITH "X" ;
					FOR &lcTmpFile..glbatch = &lcDUnPTmp..cBatchNo
					=RLOCK()
					UNLOCK
				ENDIF

				IF &lcBatchTmp..nStep < 1
*-- Update batch master file
					SELECT &lcBatchTmp
					SCATTER MEMVAR MEMO
					m.cBatchNo  = lcBatchNo
					m.nBatCnTot = 0
					m.nBatotDr  = 0
					m.nBatotCr  = 0
					m.cAdd_user = oAriaApplication.User_ID
					m.dAdd_date = oAriaApplication.SystemDate
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
			IF lcTrnkey <> &lcDUnPTmp..cTranNo
				lcTrnkey = &lcDUnPTmp..cTranNo

*-- Update header unposted master file
				IF &lcHUnPTmp..nStep<1

*-- Get new transaction number
					lcTranNo = gfsequence('cTranNo',lcGLCo)

*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	          lcTmpStr = "Batch (" +lcBatchNo+ ") Transaction (" +lcTranNo+ ")"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTmpStr = LANG_Batch +lcBatchNo+ LANG_Transaction +lcTranNo+ ")"
					lcTmpStr = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Batch,loFormSet.GetHeaderText("LANG_Batch",loFormSet.HeaderAlias)) +lcBatchNo+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Transaction,loFormSet.GetHeaderText("LANG_Transaction",loFormSet.HeaderAlias)) +lcTranNo+ ")"
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
					lnCount = lnCount + 1
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	          = gfThermo(lnReccount,lnCount,'Releasing...',lcTmpStr)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfThermo(lnReccount,lnCount,LANG_Releasing,lcTmpStr)
					= gfThermo(lnRecCount,lnCount,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_releasing,loFormSet.GetHeaderText("LANG_Releasing",loFormSet.HeaderAlias)),lcTmpStr)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]

					SELECT (lcTmpFile)
					REPLACE ALL cTrnsLedn WITH lcTranNo ;
					FOR glbatch   + cTrnsLedn = ;
					lcBatchNo + &lcDUnPTmp..cTranNo

					SELECT &lcHUnPTmp
					SCATTER MEMVAR MEMO
					m.cBatchNo  = lcBatchNo
					m.cTranNo   = lcTranNo
					m.nTrnTotDr = 0
					m.nTrnTotCr = 0
					m.cAdd_user = oAriaApplication.User_ID
					m.dAdd_date = oAriaApplication.SystemDate
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
			CASE &lcDUnPTmp..Cdrorcr = 'D'
				lnTDrTot =  lnTDrTot + &lcDUnPTmp..Namount
				lnBDrTot =  lnBDrTot + &lcDUnPTmp..Namount
			CASE &lcDUnPTmp..Cdrorcr = 'C'
				lnTCrTot =  lnTCrTot + &lcDUnPTmp..Namount
				lnBCrTot =  lnBCrTot + &lcDUnPTmp..Namount
			ENDCASE

*-- Update detailed unposted master file
			IF &lcDUnPTmp..nStep<1
				SELECT &lcDUnPTmp
				SCATTER MEMVAR MEMO
				m.cBatchNo  = lcBatchNo
				m.cTranNo   = lcTranNo
				m.cAdd_user = oAriaApplication.User_ID
				m.dAdd_date = oAriaApplication.SystemDate
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
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	    = gfThermo(lnReccount,lnReccount,'Releasing...',lcTmpStr)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfThermo(lnReccount,lnReccount,LANG_Releasing,lcTmpStr)
		= gfThermo(lnRecCount,lnRecCount,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_releasing,loFormSet.GetHeaderText("LANG_Releasing",loFormSet.HeaderAlias)),lcTmpStr)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]

		SELECT (lcTmpFile)
		lnRecCount = RECCOUNT()
		lnCount    = 0
		SCAN
			IF !&lcTmpFile..PASS
				SCATTER MEMVAR MEMO
				SELECT (lcGDistAl)
				GOTO nRecNo
				GATHER MEMVAR MEMO
*E303962,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
				=gfReplace('')
*E303962,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]
				=RLOCK()
				UNLOCK
				SELECT (lcTmpFile)
				REPLACE PASS WITH .T.
				=RLOCK()
				UNLOCK
			ENDIF

			lnCount = lnCount + 1
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	      = gfThermo(lnReccount,lnCount,'Updating G/L Distribution...')
*N000682,1 TMI 04/21/2013 [Start] add LANG_ as in the .h file
*= gfThermo(lnReccount,lnCount,Updating_GL_Distribution)
			= gfThermo(lnRecCount,lnCount,LANG_Updating_GL_Distribution)
*N000682,1 TMI 04/21/2013 [End  ]
*N000682,1 04/16/2013 RAS Globlization changes[END ]

		ENDSCAN
		SELECT (lcGDistAl)
		=gfTableUpdate()
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	    = gfThermo(lnReccount,lnReccount,'Updating G/L Distribution...')
*N000682,1 TMI 04/21/2013 [Start] add LANG_ as in the .h file
*= gfThermo(lnReccount,lnReccount,Updating_GL_Distribution)
		= gfThermo(lnRecCount,lnRecCount,LANG_Updating_GL_Distribution)
*N000682,1 TMI 04/21/2013 [End  ]
*N000682,1 04/16/2013 RAS Globlization changes[END ]

	ENDIF &&IF llGoOn

*-- Unlock master files
	lcLocFiles = lcBatchAl+','+lcDTrnsAl+','+lcHTrnsAl
*-- Release locks to prevent deadlock on retries
	= gfFlock(lcLocFiles,.F.)

ENDIF

*-- Erase temporary files and indeses
USE IN &lcTmpFile
ERASE oAriaApplication.WorkDir+lcTmpFile+".dbf"
ERASE oAriaApplication.WorkDir+lcTmpFile+".cdx"

USE IN &lcBatchTmp
ERASE oAriaApplication.WorkDir+lcBatchTmp+".dbf"
ERASE oAriaApplication.WorkDir+lcBatchTmp+".cdx"

USE IN &lcHUnPTmp
ERASE oAriaApplication.WorkDir+lcHUnPTmp+".dbf"
ERASE oAriaApplication.WorkDir+lcHUnPTmp+".cdx"

USE IN &lcDUnPTmp
ERASE oAriaApplication.WorkDir+lcDUnPTmp+".dbf"
ERASE oAriaApplication.WorkDir+lcDUnPTmp+".cdx"

SELECT (lcGDistAl)
*E303962,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
*!*	  SET ORDER TO TAG GlDistPo IN (lcGDistAl)
=gfSetOrder('GLDISTPO')
*E303962,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]



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
SET ORDER TO TAG glacnt IN &lcAccHrAl

*-- Declare local variables private
PRIVATE lcDocExp,  lcSumExp, lcNdxExp, lcYearPrd, lcSess, ;
ldGlDate,  lnTDrTot, lnTCrTot, lnBDrTot,  lnBCrTot, ;
lcGlTDes,  lnAmount, lcGlAcnt, lcDocKey,  lcSubDetl, TMPFILE, ;
lcErrMsg1, lcErrMsg2

IF lcCurProc <'3'
*-- Create temp file for GLDist File and Build filtered index
	lcTmpFile   = gfTempName()

	SELECT *,.F. AS PASS,RECNO() AS nRecNo,lfSorcMod(&lcGDistAl..Tran_Type) AS Csrcmodul;
	FROM (lcGDistAl) ;
	INTO DBF (oAriaApplication.WorkDir+lcTmpFile) ;
	WHERE EMPTY(posted) ;
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

*-- Build filtered index
	SELECT (lcTmpFile)
	gcWorkDir = oAriaApplication.WorkDir
	INDEX ON glbatch+cTrnsLedn TAG BatTrn OF &gcWorkDir.&lcTmpFile COMPACT

	INDEX ON &lcNdxExp + &lcDocExp TAG (lcTmpFile) OF ;
	&gcWorkDir.&lcTmpFile COMPACT ADDITIVE
	USE
	USE (oAriaApplication.WorkDir+lcTmpFile) SHARED
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

		lcSorcMod = &lcTmpFile..Csrcmodul

		IF !&lcTmpFile..PASS
*-- Get batch number
			lcBatchNo = lfGetGlBt(lcGLCo)

*-- lfGetGlBt could not lock record in gldist file
*-- so terminate the process
			IF !llGoOn
*-- "cannot complete the process, record is used by another"
*-- <OK>
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
				IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
					loFormSet.oProgress.Hide()
				ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]

				= gfModalGen("INM00304B00000","Dialog")
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
				IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
					loFormSet.oProgress.Show()
				ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]


				EXIT
			ENDIF

		ENDIF

*-- Add batch status record to GLBTCH temporary file and
*-- leave marked with object lock
		SELECT (lcBatchTmp)
		APPEND BLANK
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	    REPLACE batchno  WITH lcBatchNo, ;
*!*	      bdesc    WITH "Posting from ARIA, " + lcComDesc, ;
*!*	      yearprd  WITH lcYearPrd, ;
*!*	      bref     WITH "", ;
*!*	      lowseq   WITH "", ;
*!*	      uppseq   WITH "", ;
*!*	      btype    WITH "Z", ;
*!*	      bstat    WITH "", ;
*!*	      ctrltot  WITH 0, ;
*!*	      drtotal  WITH 0, ;
*!*	      crtotal  WITH 0, ;
*!*	      stsuser  WITH oAriaApplication.User_ID, ;
*!*	      stsdate  WITH oAriaApplication.SystemDate, ;
*!*	      ststime  WITH gfGettime(), ;
*!*	      edtuser  WITH "", ;
*!*	      edtdate  WITH {}, ;
*!*	      edttime  WITH "", ;
*!*	      auduser  WITH "", ;
*!*	      auddate  WITH {}, ;
*!*	      audtime  WITH "", ;
*!*	      pstuser  WITH "", ;
*!*	      pstdate  WITH {}, ;
*!*	      psttime  WITH "", ;
*!*	      post1    WITH "", ;
*!*	      post2    WITH "", ;
*!*	      post3    WITH "", ;
*!*	      post4    WITH "", ;
*!*	      post5    WITH "", ;
*!*	      post6    WITH "", ;
*!*	      post7    WITH "", ;
*!*	      post8    WITH "", ;
*!*	      adduser  WITH oAriaApplication.User_ID, ;
*!*	      adddate  WITH oAriaApplication.SystemDate, ;
*!*	      addtime  WITH gfGettime(), ;
*!*	      lckstat  WITH "L", ;
*!*	      lckuser  WITH oAriaApplication.User_ID, ;
*!*	      lckdate  WITH oAriaApplication.SystemDate, ;
*!*	      lcktime  WITH gfGettime()
		REPLACE BATCHNO  WITH lcBatchNo, ;
		bdesc    WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Posting_from_ARIA,loFormSet.GetHeaderText("LANG_Posting_from_ARIA",loFormSet.HeaderAlias)) + lcComDesc, ;
		yearprd  WITH lcYearPrd, ;
		bref     WITH "", ;
		lowseq   WITH "", ;
		uppseq   WITH "", ;
		btype    WITH "Z", ;
		bstat    WITH "", ;
		ctrltot  WITH 0, ;
		drtotal  WITH 0, ;
		crtotal  WITH 0, ;
		stsuser  WITH oAriaApplication.User_ID, ;
		stsdate  WITH oAriaApplication.SystemDate, ;
		ststime  WITH gfGettime(), ;
		edtuser  WITH "", ;
		edtdate  WITH {}, ;
		edttime  WITH "", ;
		auduser  WITH "", ;
		auddate  WITH {}, ;
		audtime  WITH "", ;
		pstuser  WITH "", ;
		pstdate  WITH {}, ;
		psttime  WITH "", ;
		post1    WITH "", ;
		post2    WITH "", ;
		post3    WITH "", ;
		post4    WITH "", ;
		post5    WITH "", ;
		post6    WITH "", ;
		post7    WITH "", ;
		post8    WITH "", ;
		adduser  WITH oAriaApplication.User_ID, ;
		adddate  WITH oAriaApplication.SystemDate, ;
		addtime  WITH gfGettime(), ;
		lckstat  WITH "L", ;
		lckuser  WITH oAriaApplication.User_ID, ;
		lckdate  WITH oAriaApplication.SystemDate, ;
		lcktime  WITH gfGettime()
*N000682,1 04/16/2013 RAS Globlization changes[END ]
*-- Setup accumulator variables for batch
		lnBDrTot = 0
		lnBCrTot = 0
		lcLowSeq = SPACE(6)
		lcUppSeq = SPACE(6)

*-- Process records for batch
		SELECT (lcTmpFile)

		DO WHILE &lcTmpFile..GlFYear + &lcTmpFile..GlPeriod + ;
			&lcTmpFile..Csrcmodul  = lcYearPrd + lcSorcMod ;
			.AND. .NOT. EOF()

*-- Setup session ID and date field values for GLHUNP update
			lcSess   = IIF(lcDetail = "D", &lcTmpFile..GlSession, "")
			ldGlDate = &lcTmpFile..Tran_Date

			IF !&lcTmpFile..PASS
*-- Get transaction number
				lcTranNo = lfGetGlTr(lcGLCo)
				IF !llGoOn
*-- "cannot complete the process, record is used by another"
*-- <OK>
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
					IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
						loFormSet.oProgress.Hide()
					ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]

					= gfModalGen("INM00304B00000","Dialog")
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
					IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
						loFormSet.oProgress.Show()
					ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]


					EXIT
				ENDIF
			ENDIF

*-- Update upper and lower transaction numbers for batch record
			lcLowSeq = IIF(EMPTY(lcLowSeq), lcTranNo, MIN(lcTranNo, lcLowSeq))
			lcUppSeq = MAX(lcTranNo, lcUppSeq)

*-- Setup accumulator variables for batch
			lnTDrTot = 0
			lnTCrTot = 0

			lnEntryNo = 0

*-- Add transaction header record to GLHUNP
			SELECT &lcHUnPTmp
			APPEND BLANK
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	      REPLACE batchno  WITH lcBatchNo, ;
*!*	        glseqno  WITH lcTranNo, ;
*!*	        subno    WITH lcSess, ;
*!*	        gltdes   WITH "Posting from ARIA, " + lcComDesc , ;
*!*	        glrefr   WITH "", ;
*!*	        gldate   WITH ldGlDate, ;
*!*	        yearprd  WITH lcYearPrd, ;
*!*	        STATUS   WITH "U", ;
*!*	        trtype   WITH "Z", ;
*!*	        glmodule WITH "ARIA", ;
*!*	        glcomp   WITH lcComDesc, ;
*!*	        glrevrs  WITH "N", ;
*!*	        revper   WITH "", ;
*!*	        revyear  WITH "", ;
*!*	        adduser  WITH oAriaApplication.User_ID, ;
*!*	        adddate  WITH oAriaApplication.SystemDate, ;
*!*	        addtime  WITH gfGettime(), ;
*!*	        lckstat  WITH "", ;
*!*	        lckuser  WITH oAriaApplication.User_ID, ;
*!*	        lckdate  WITH oAriaApplication.SystemDate, ;
*!*	        lcktime  WITH gfGettime()
			REPLACE BATCHNO  WITH lcBatchNo, ;
			glseqno  WITH lcTranNo, ;
			subno    WITH lcSess, ;
			gltdes   WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Posting_from_ARIA,loFormSet.GetHeaderText("LANG_Posting_from_ARIA",loFormSet.HeaderAlias)) + lcComDesc , ;
			glrefr   WITH "", ;
			gldate   WITH ldGlDate, ;
			yearprd  WITH lcYearPrd, ;
			STATUS   WITH "U", ;
			trtype   WITH "Z", ;
			glmodule WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ARIA,loFormSet.GetHeaderText("LANG_ARIA",loFormSet.HeaderAlias)), ;
			GLCOMP   WITH lcComDesc, ;
			glrevrs  WITH "N", ;
			revper   WITH "", ;
			revyear  WITH "", ;
			adduser  WITH oAriaApplication.User_ID, ;
			adddate  WITH oAriaApplication.SystemDate, ;
			addtime  WITH gfGettime(), ;
			lckstat  WITH "", ;
			lckuser  WITH oAriaApplication.User_ID, ;
			lckdate  WITH oAriaApplication.SystemDate, ;
			lcktime  WITH gfGettime()
*N000682,1 04/16/2013 RAS Globlization changes[END ]
*-- Process records for transaction
			SELECT &lcTmpFile

			DO WHILE &lcTmpFile..GlFYear + &lcTmpFile..GlPeriod + &lcTmpFile..Csrcmodul = ;
				lcYearPrd + lcSorcMod ;
				.AND. IIF(EMPTY(lcSess), .T., (&lcTmpFile..GlSession = ;
				lcSess)) .AND. .NOT. EOF()

*-- Setup account number
				lcGlAcnt = &lcTmpFile..GlAccount

*-- Setup subledger link field value for summary posting
				lcSubDetl = ""

*-- Setup document summarization key value
				lcDocKey = &lcDocExp

*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	        lcTmpStr ="Batch: " + ALLTRIM(lcBatchNo) + "Year/Period " + ;
*!*	          LEFT(lcYearPrd, 4) + "/" + RIGHT(lcYearPrd, 2) + ;
*!*	          ", account " + ALLTRIM(lcGlAcnt)
				lcTmpStr =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Batch1,loFormSet.GetHeaderText("LANG_Batch1",loFormSet.HeaderAlias)) + ALLTRIM(lcBatchNo) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Year_Period1,loFormSet.GetHeaderText("LANG_Year_Period1",loFormSet.HeaderAlias)) + ;
				LEFT(lcYearPrd, 4) + "/" + RIGHT(lcYearPrd, 2) + ;
				IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_account,loFormSet.GetHeaderText("LANG_account",loFormSet.HeaderAlias)) + ALLTRIM(lcGlAcnt)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]

				lnCount = lnCount + 1

*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	        = gfThermo(lnReccount,lnCount,'Generating...',lcTmpStr)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfThermo(lnReccount,lnCount,LANG_Generating,lcTmpStr)
				= gfThermo(lnRecCount,lnCount,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Generating,loFormSet.GetHeaderText("LANG_Generating",loFormSet.HeaderAlias)),lcTmpStr)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]

*-- Setup description
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	        lcGlTDes = "Transaction #: " + LTRIM(Tran_No)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcGlTDes = LANG_Transaction_No + LTRIM(Tran_No)
				lcGlTDes = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Transaction_No,loFormSet.GetHeaderText("LANG_Transaction_No",loFormSet.HeaderAlias)) + LTRIM(Tran_no)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
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


					lnEntryNo = lnEntryNo + 1

					SCAN WHILE GlFYear + GlPeriod + Csrcmodul = lcYearPrd + lcSorcMod .AND. GlAccount = lcGlAcnt ;
						.AND. IIF(EMPTY(lcSess), .T., (&lcTmpFile..GlSession = ;
						lcSess)) .AND. &lcSumExp.

						REPLACE glbatch WITH lcBatchNo, ;
						GLCOMP  WITH lcGLCo ,;
						nEntryNo  WITH lnEntryNo ,;
						cTrnsLedn WITH lcTranNo

*-- Increment amount
						lnAmount = lnAmount + nEqvAmnt
					ENDSCAN

				ELSE
*-- Detail and not on control account

*-- Accumulate amount while on same period/account/session/document
					lnAmount = 0

					SCAN WHILE GlFYear + GlPeriod + Csrcmodul = lcYearPrd + lcSorcMod .AND. GlAccount = lcGlAcnt ;
						.AND. IIF(EMPTY(lcSess), .T., (&lcTmpFile..GlSession = ;
						lcSess)) .AND. &lcDocExp. = lcDocKey

						REPLACE glbatch WITH lcBatchNo, ;
						GLCOMP  WITH lcGLCo , ;
						posted  WITH 'X' ,;
						nEntryNo  WITH lnEntryNo ,;
						cTrnsLedn WITH lcTranNo

*-- Increment amount
						lnAmount = lnAmount + nEqvAmnt
					ENDSCAN

				ENDIF && lcDetail = "N" .OR. etc.

*-- Search for account number in GLACNT
				SELECT &lcAccHrAl
				SEEK lcGlAcnt

*-- If acount not found or deleted or inactive
				IF .NOT. FOUND(lcAccHrAl) .OR. DELETED(lcAccHrAl) .OR. ;
					&lcAccHrAl..glstat = "I"
*-- Use suspense account
					lcGlAcnt = lcSuspense
				ENDIF

*-- Add transaction detail record to GLDUNP
				SELECT &lcDUnPTmp
				APPEND BLANK
				REPLACE glacnt   WITH lcGlAcnt, ;
				BATCHNO  WITH lcBatchNo, ;
				glseqno  WITH lcTranNo, ;
				gltdes   WITH lcGlTDes, ;
				gldamnt  WITH IIF(lnAmount < 0, 0, ABS(lnAmount)), ;
				glcamnt  WITH IIF(lnAmount < 0, ABS(lnAmount), 0), ;
				credit   WITH (lnAmount < 0), ;
				subdetl  WITH lcSubDetl, ;
				adduser  WITH oAriaApplication.User_ID, ;
				adddate  WITH oAriaApplication.SystemDate, ;
				addtime  WITH gfGettime(), ;
				lckstat  WITH "", ;
				lckuser  WITH oAriaApplication.User_ID, ;
				lckdate  WITH oAriaApplication.SystemDate, ;
				lcktime  WITH gfGettime()

*-- Increment debit and credit totals for transaction and batch
				lnTDrTot = lnTDrTot + &lcDUnPTmp..gldamnt
				lnTCrTot = lnTCrTot + &lcDUnPTmp..glcamnt
				lnBDrTot = lnBDrTot + &lcDUnPTmp..gldamnt
				lnBCrTot = lnBCrTot + &lcDUnPTmp..glcamnt

*-- Already on next detail record
				SELECT (lcTmpFile)
			ENDDO && WHILE &lcTmpFile..GlFYear + &lcTmpFile..GlPeriod = etc.
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	      = gfThermo(lnReccount,lnReccount,'Generating...',lcTmpStr)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfThermo(lnReccount,lnReccount,LANG_Generating,lcTmpStr)
			= gfThermo(lnRecCount,lnRecCount,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Generating,loFormSet.GetHeaderText("LANG_Generating",loFormSet.HeaderAlias)),lcTmpStr)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]

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
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
				IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
					loFormSet.oProgress.Hide()
				ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]

				= gfModalGen("INM00305B00000","Dialog",lcTmpStr)
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
				IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
					loFormSet.oProgress.Show()
				ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]



*-- Add suspense transaction detail record to GLDUNP
				SELECT &lcDUnPTmp
				APPEND BLANK
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	        REPLACE glacnt   WITH lcSuspense, ;
*!*	          batchno  WITH lcBatchNo, ;
*!*	          glseqno  WITH lcTranNo, ;
*!*	          gltdes   WITH "Suspense - Out of Balance", ;
*!*	          gldamnt  WITH IIF(lnAmount < 0, 0, ABS(lnAmount)), ;
*!*	          glcamnt  WITH IIF(lnAmount < 0, ABS(lnAmount), 0), ;
*!*	          credit   WITH (lnAmount < 0), ;
*!*	          adduser  WITH oAriaApplication.User_ID, ;
*!*	          adddate  WITH oAriaApplication.SystemDate, ;
*!*	          addtime  WITH gfGettime(), ;
*!*	          lckstat  WITH "", ;
*!*	          lckuser  WITH oAriaApplication.User_ID, ;
*!*	          lckdate  WITH oAriaApplication.SystemDate, ;
*!*	          lcktime  WITH gfGettime()
				REPLACE glacnt   WITH lcSuspense, ;
				BATCHNO  WITH lcBatchNo, ;
				glseqno  WITH lcTranNo, ;
				gltdes   WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Suspense_Out_of_Balance,loFormSet.GetHeaderText("LANG_Suspense_Out_of_Balance",loFormSet.HeaderAlias)), ;
				gldamnt  WITH IIF(lnAmount < 0, 0, ABS(lnAmount)), ;
				glcamnt  WITH IIF(lnAmount < 0, ABS(lnAmount), 0), ;
				credit   WITH (lnAmount < 0), ;
				adduser  WITH oAriaApplication.User_ID, ;
				adddate  WITH oAriaApplication.SystemDate, ;
				addtime  WITH gfGettime(), ;
				lckstat  WITH "", ;
				lckuser  WITH oAriaApplication.User_ID, ;
				lckdate  WITH oAriaApplication.SystemDate, ;
				lcktime  WITH gfGettime()
*N000682,1 04/16/2013 RAS Globlization changes[END ]
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
		lckdate  WITH oAriaApplication.SystemDate, ;
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
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	    = gfThermo(lnReccount,lnCount,'Updating master files...',"GLBATCH")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfThermo(lnReccount,lnCount,LANG_Updating_master_files,"GLBATCH")
		= gfThermo(lnRecCount,lnCount,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Updating_master_files,loFormSet.GetHeaderText("LANG_Updating_master_files",loFormSet.HeaderAlias)),"GLBATCH")
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	ENDSCAN
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  = gfThermo(lnReccount,lnReccount,'Updating master files...',"GLBATCH")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfThermo(lnReccount,lnReccount,LANG_Updating_master_files,"GLBATCH")
	= gfThermo(lnRecCount,lnRecCount,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Updating_master_files,loFormSet.GetHeaderText("LANG_Updating_master_files",loFormSet.HeaderAlias)),"GLBATCH")
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]

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
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	    = gfThermo(lnReccount,lnCount,'Updating master files...',"GLTRNSHD")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfThermo(lnReccount,lnCount,LANG_Updating_master_files,"GLTRNSHD")
		= gfThermo(lnRecCount,lnCount,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Updating_master_files,loFormSet.GetHeaderText("LANG_Updating_master_files",loFormSet.HeaderAlias)),"GLTRNSHD")
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	ENDSCAN
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  = gfThermo(lnReccount,lnReccount,'Updating master files...',"GLTRNSHD")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfThermo(lnReccount,lnReccount,LANG_Updating_master_files,"GLTRNSHD")
	= gfThermo(lnRecCount,lnRecCount,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Updating_master_files,loFormSet.GetHeaderText("LANG_Updating_master_files",loFormSet.HeaderAlias)),"GLTRNSHD")
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]

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
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	    = gfThermo(lnReccount,lnCount,'Updating master files...',"GLTRNSDT")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfThermo(lnReccount,lnCount,LANG_Updating_master_files,"GLTRNSDT")
		= gfThermo(lnRecCount,lnCount,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Updating_master_files,loFormSet.GetHeaderText("LANG_Updating_master_files",loFormSet.HeaderAlias)),"GLTRNSDT")
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	ENDSCAN
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  = gfThermo(lnReccount,lnReccount,'Updating master files...',"GLTRNSDT")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfThermo(lnReccount,lnReccount,LANG_Updating_master_files,"GLTRNSDT")
	= gfThermo(lnRecCount,lnRecCount,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Updating_master_files,loFormSet.GetHeaderText("LANG_Updating_master_files",loFormSet.HeaderAlias)),"GLTRNSDT")
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]

	SELECT (lcTmpFile)
	lnRecCount = RECCOUNT()
	lnCount    = 0
	SCAN
		IF !PASS
			SCATTER MEMVAR MEMO
			SELECT(lcGDistAl)
			APPEND BLANK
			GATHER MEMVAR MEMO
			SELECT (lcTmpFile)
			REPLACE PASS WITH .T.
		ENDIF
		lnCount = lnCount + 1
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	    = gfThermo(lnReccount,lnCount,'Updating master files...',"GLDIST")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfThermo(lnReccount,lnCount,LANG_Updating_master_files,"GLDIST")
		= gfThermo(lnRecCount,lnCount,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Updating_master_files,loFormSet.GetHeaderText("LANG_Updating_master_files",loFormSet.HeaderAlias)),"GLDIST")
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]

	ENDSCAN
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  = gfThermo(lnReccount,lnReccount,'Updating master files...',"GLDIST")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfThermo(lnReccount,lnReccount,LANG_Updating_master_files,"GLDIST")
	= gfThermo(lnRecCount,lnRecCount,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Updating_master_files,loFormSet.GetHeaderText("LANG_Updating_master_files",loFormSet.HeaderAlias)),"GLDIST")
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]

ENDIF

*-- Unlock master files
lcLocFiles = lcBatchAl+','+lcDTrnsAl+','+lcHTrnsAl
*-- Release locks to prevent deadlock on retries
= gfFlock(lcLocFiles,.F.)

*-- Erase temporary files and indeses
USE IN &lcTmpFile
ERASE oAriaApplication.WorkDir+lcTmpFile+".dbf"
ERASE oAriaApplication.WorkDir+lcTmpFile+".cdx"

USE IN &lcBatchTmp
ERASE oAriaApplication.WorkDir+lcBatchTmp+".dbf"
ERASE oAriaApplication.WorkDir+lcBatchTmp+".cdx"

USE IN &lcHUnPTmp
ERASE oAriaApplication.WorkDir+lcHUnPTmp+".dbf"
ERASE oAriaApplication.WorkDir+lcHUnPTmp+".cdx"

USE IN &lcDUnPTmp
ERASE oAriaApplication.WorkDir+lcDUnPTmp+".dbf"
ERASE oAriaApplication.WorkDir+lcDUnPTmp+".cdx"

SELECT &lcGDistAl
*E303962,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
*!*	    SET ORDER TO TAG GlDistPo IN &lcGDistAl
=gfSetOrder('GLDISTPO')
*E303962,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]


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
LOCATE FOR SYSID = "GL" +  lcCompId

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
	SET ORDER TO TAG BATCHNO IN &lcBatchAl

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
LOCATE FOR SYSID = "GL" +  lcCompId

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
SET ORDER TO TAG Comfyrprdi IN (lcFsPrdAl)

SELECT (lcFsPrdAl)
SEEK lcFisYear + lcPeriod

llError = .F.

*-- Check for invalid, deleted, or locked posting period
DO CASE
CASE .NOT. FOUND(lcFsPrdAl)
*-- If invalid, set flag to cancel and display alert
	llError  = .T.
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  lcTmpStr =  lcPeriod + "/" + lcFisYear + ;
*!*	    " not found in fiscal calendar." + ;
*!*	    " Unable to continue with release."
	lcTmpStr =  lcPeriod + "/" + lcFisYear + ;
	IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_not_found_in_fiscal_calendar,loFormSet.GetHeaderText("LANG_not_found_in_fiscal_calendar",loFormSet.HeaderAlias)) + ;
	IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Unable_to_continue_with_release,loFormSet.GetHeaderText("LANG_Unable_to_continue_with_release",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]


*N000682,1 04/16/2013 RAS Globlization changes[END ]

CASE DELETED(lcFsPrdAl)
*-- If deleted, set flag to cancel and display alert
	llError  = .T.
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  lcTmpStr =  lcPeriod + "/" + lcFisYear + ;
*!*	    " is deleted in fiscal calendar." + ;
*!*	    " Unable to continue with release."
	lcTmpStr =  lcPeriod + "/" + lcFisYear + ;
	IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_is_deleted_in_fiscal_calendar,loFormSet.GetHeaderText("LANG_is_deleted_in_fiscal_calendar",loFormSet.HeaderAlias))+ ;
	IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Unable_to_continue_with_release,loFormSet.GetHeaderText("LANG_Unable_to_continue_with_release",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]


*N000682,1 04/16/2013 RAS Globlization changes[END ]
CASE &lcFsPrdAl..lFsPLocks
*-- If period marked as locked,
*-- set flag to cancel and display alert
	llError  = .T.
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  lcTmpStr =  lcPeriod + "/" + lcFisYear + ;
*!*	    " is locked in fiscal calendar." + ;
*!*	    " You must unlock the period before postings     can be released." + ;
*!*	    " Unable to continue with release."
	lcTmpStr =  lcPeriod + "/" + lcFisYear + ;
	IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_is_locked_in_fiscal_calendar,loFormSet.GetHeaderText("LANG_is_locked_in_fiscal_calendar",loFormSet.HeaderAlias)) + ;
	IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_You_must_unlock_the_period_before_postings,loFormSet.GetHeaderText("LANG_You_must_unlock_the_period_before_postings",loFormSet.HeaderAlias)) + ;
	IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Unable_to_continue_with_release,loFormSet.GetHeaderText("LANG_Unable_to_continue_with_release",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]


*N000682,1 04/16/2013 RAS Globlization changes[END ]
ENDCASE

IF llError
	llGoOn = .F.
*-- Posting period xxxx
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
	IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
		loFormSet.oProgress.Hide()
	ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]

	= gfModalGen("INM00312B00000","Dialog",lcTmpStr)
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
	IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
		loFormSet.oProgress.Show()
	ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]


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
SET ORDER TO TAG AcctCode IN (lcAccHrAl)
lcDelStat = SET('DELETE')
llDispMss = .F.
lnResp = 0
*-- Search for account number in GLACNT
IF !SEEK(&lcAccount,lcAccHrAl)
	SET DELETE OFF
	IF !SEEK(&lcAccount,lcAccHrAl)
*-- Non Exist Account
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	    lcTmpStr = "Account " + ALLTRIM(&lcAccount)+ " not found in Chart of Accounts"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTmpStr = LANG_Account1 + ALLTRIM(&lcAccount)+ LANG_not_found_in_Chart_of_Accounts
		lcTmpStr = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Account1,loFormSet.GetHeaderText("LANG_Account1",loFormSet.HeaderAlias)) + ALLTRIM(&lcAccount)+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_not_found_in_Chart_of_Accounts,loFormSet.GetHeaderText("LANG_not_found_in_Chart_of_Accounts",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]

	ELSE
*-- Deleted Account
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	    lcTmpStr = "Account " + ALLTRIM(&lcAccount) + " is deleted in Chart of Accounts"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTmpStr = LANG_Account1+ ALLTRIM(&lcAccount) + LANG_is_deleted_in_Chart_of Accounts
*B610869,1 TMI 10/01/2014 16:13 [Start] fix a problem when releasing
*lcTmpStr = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Account1,loFormSet.GetHeaderText("LANG_Account1",loFormSet.HeaderAlias))+ ALLTRIM(&lcAccount);
*+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_is_deleted_in_Chart_of         ,loFormSet.GetHeaderText("LANG_is_deleted_in_Chart_of"         ,loFormSet.HeaderAlias)) Accounts
		lcTmpStr = IIF(oAriaApplication.oActivelang.cLang_ID = "EN","Account "   ,loFormSet.GetHeaderText("LANG_Account1",loFormSet.HeaderAlias))+ ALLTRIM(&lcAccount);
		+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_is_deleted_in_Chart_of_Accounts,loFormSet.GetHeaderText("LANG_is_deleted_in_Chart_of_Accounts",loFormSet.HeaderAlias))
*B610869,1 TMI 10/01/2014 16:13 [End  ]
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]

	ENDIF
	llDispMss = .T.
ELSE
	IF &lcAccHrAl..CSegActiv = "I"
*-- Inactive Account
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	    lcTmpStr = "Account " + ALLTRIM(&lcAccount) + " is marked as Inactive in Chart of Accounts"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTmpStr = LANG_Account1 + ALLTRIM(&lcAccount) + LANG_is_marked_as_Inactive_in_Chart_of_Accounts
		lcTmpStr = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Account1,loFormSet.GetHeaderText("LANG_Account1",loFormSet.HeaderAlias)) + ALLTRIM(&lcAccount) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_is_marked_as_Inactive_in_Chart_of_Accounts,loFormSet.GetHeaderText("LANG_is_marked_as_Inactive_in_Chart_of_Accounts",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
		llDispMss = .T.
	ENDIF
ENDIF

IF llDispMss
*-- lcTmpStr+".Transfer to Suspense Account xxxx or cancel release to GL ?"
*-- <Release>,<Cancel>
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
	IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
		loFormSet.oProgress.Hide()
	ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]

	lnResp = gfModalGen('QRM00313B00035','Dialog',lcTmpStr+'|'+ALLTRIM(lcSuspense))
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
	IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
		loFormSet.oProgress.Show()
	ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]


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
SET ORDER TO TAG glacnt   IN &lcAccHrAl
SET ORDER TO TAG compid1  IN &lcFsPrdAl

*E303962,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
*!*	    SET ORDER TO TAG GlDistPo IN &lcGDistAl
SELECT (lcGDistAl)
=gfSetOrder('GLDISTPO')
*E303962,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]


*-- Validate GL account numbers in source file
SELECT &lcGDistAl
*E303962,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
*SEEK " "
=gfSEEK(" ")
*E303962,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]
SCAN FOR RECNO(lcGDistAl) <= lnMaxRec  WHILE posted = " " .AND. .NOT. EOF()

*-- Save account number and posting year/period
	lcGlAcnt  = GlAccount
	lcFisYear = GlFYear
	lcPeriod  = GlPeriod

*-- Display processing message
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  lcErrMsg1 =  "Checking posting to account " + ALLTRIM(lcGlAcnt) + ;
*!*	    ", period " + lcFisYear + "/" + lcPeriod
	lcErrMsg1 =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Checking_posting_to_account,loFormSet.GetHeaderText("LANG_Checking_posting_to_account",loFormSet.HeaderAlias)) + ALLTRIM(lcGlAcnt) + ;
	IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_period,loFormSet.GetHeaderText("LANG_period",loFormSet.HeaderAlias)) + lcFisYear + "/" + lcPeriod
*N000682,1 11/20/2012 MMT Globlization changes[End]


*N000682,1 04/16/2013 RAS Globlization changes[END ]

	WAIT WIND lcErrMsg1 NOWAIT
*-- Search for account number in GLACNT
	SELECT &lcAccHrAl
	SET DELETE OFF
	SEEK lcGlAcnt
*-- Check for invalid, deleted, or inactive account number
	llError = .F.
	DO CASE
	CASE .NOT. FOUND(lcAccHrAl)
*-- If invalid, display dialog
		llError = .T.
*-- Non Exist Account
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	    lcTmpStr = "Account " + TRIM(lcGlAcnt) + " not found in Chart of Accounts"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTmpStr = LANG_Account1 + TRIM(lcGlAcnt) + LANG_not_found_in_Chart_of_Accounts
		lcTmpStr = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Account1,loFormSet.GetHeaderText("LANG_Account1",loFormSet.HeaderAlias)) + TRIM(lcGlAcnt) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_not_found_in_Chart_of_Accounts,loFormSet.GetHeaderText("LANG_not_found_in_Chart_of_Accounts",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	CASE DELETED(lcAccHrAl)
*-- If deleted, display dialog
		llError = .T.
*-- Deleted Account
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	    lcTmpStr = "Account " + TRIM(lcGlAcnt) + " is deleted in Chart of Accounts"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTmpStr = LANG_Account1 + TRIM(lcGlAcnt) + LANG_is_deleted_in_Chart_of_Accounts
		lcTmpStr = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Account1,loFormSet.GetHeaderText("LANG_Account1",loFormSet.HeaderAlias)) + TRIM(lcGlAcnt) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_is_deleted_in_Chart_of_Accounts,loFormSet.GetHeaderText("LANG_is_deleted_in_Chart_of_Accounts",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]

	CASE &lcAccHrAl..glstat = "I"
*-- If inactive, display dialog
		llError = .T.
*-- Inactive Account
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	    lcTmpStr = "Account " + TRIM(lcGlAcnt) + " is marked as Inactive in Chart of Accounts"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTmpStr =LANG_Account1 + TRIM(lcGlAcnt) + LANG_is_marked_as_Inactive_in_Chart_of_Accounts
		lcTmpStr =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Account1,loFormSet.GetHeaderText("LANG_Account1",loFormSet.HeaderAlias)) + TRIM(lcGlAcnt) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_is_marked_as_Inactive_in_Chart_of_Accounts,loFormSet.GetHeaderText("LANG_is_marked_as_Inactive_in_Chart_of_Accounts",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]

	OTHERWISE
*-- Passed all account number validations
		lnResp = 3

	ENDCASE

	IF llError
*-- X.Transfer to Suspense Account xxxx or cancel release to GL ?'
*-- <Release>,<Cancel>
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
		IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
			loFormSet.oProgress.Hide()
		ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]

		lnResp = gfModalGen('QRM00313B00035','Dialog',lcTmpStr+'|'+ALLTRIM(lcSuspense))
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
		IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
			loFormSet.oProgress.Show()
		ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]


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
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	      lcTmpStr =  lcPeriod + "/" + lcFisYear + ;
*!*	        " not found in fiscal calendar." + ;
*!*	        "Unable to continue with release."
			lcTmpStr =  lcPeriod + "/" + lcFisYear + ;
			IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_not_found_in_fiscal_calendar,loFormSet.GetHeaderText("LANG_not_found_in_fiscal_calendar",loFormSet.HeaderAlias))+ ;
			"Unable to continue with release."

*N000682,1 04/16/2013 RAS Globlization changes[END ]
		CASE DELETED(lcFsPrdAl)
*-- If deleted, set flag to cancel and display alert
			llError = .T.
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	      lcTmpStr = lcPeriod + "/" + lcFisYear + ;
*!*	        " is deleted in fiscal calendar." +;
*!*	        "Unable to continue with release."
			lcTmpStr = lcPeriod + "/" + lcFisYear + ;
			IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_is_deleted_in_fiscal_calendar,loFormSet.GetHeaderText("LANG_is_deleted_in_fiscal_calendar",loFormSet.HeaderAlias)) +;
			IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Unable_to_continue_with_release,loFormSet.GetHeaderText("LANG_Unable_to_continue_with_release",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]


*N000682,1 04/16/2013 RAS Globlization changes[END ]
		CASE &lcFsPrdAl..PermLck
*-- If period marked as permanantly locked,
*-- set flag to cancel and display alert
			llError = .T.
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	      lcTmpStr  = lcPeriod + "/" + lcFisYear + ;
*!*	        " is permanantly locked in fiscal calendar." +;
*!*	        "You must unlock the period before postings can be released." +;
*!*	        "Unable to continue with release."
			lcTmpStr  = lcPeriod + "/" + lcFisYear + ;
			IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_is_permanantly_locked_in_fiscal_calendar,loFormSet.GetHeaderText("LANG_is_permanantly_locked_in_fiscal_calendar",loFormSet.HeaderAlias)) +;
			IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_You_must_unlock_the_period_before_postings_can_be_released,loFormSet.GetHeaderText("LANG_You_must_unlock_the_period_before_postings_can_be_released",loFormSet.HeaderAlias))  +;
			IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Unable_to_continue_with_release,loFormSet.GetHeaderText("LANG_Unable_to_continue_with_release",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]

		CASE &lcFsPrdAl..PLocked
*-- If period marked as locked,
*-- set flag to cancel and display alert
			llError = .T.
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	      lcErrMsg1 = "Posting period " + lcPeriod + "/" + ;
*!*	        lcFisYear + " is locked in fiscal calendar."
*!*	      lcErrMsg2 = "You must unlock the period before postings can be released."
*!*	      lcErrMsg3 = "Unable to continue with release."

*!*	      lcTmpStr  = lcPeriod + "/" + lcFisYear + ;
*!*	        " is locked in fiscal calendar." +;
*!*	        "You must unlock the period before postings can be released."+;
*!*	        "Unable to continue with release."
			lcErrMsg1 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Posting_period,loFormSet.GetHeaderText("LANG_Posting_period",loFormSet.HeaderAlias)) + lcPeriod + "/" + ;
			lcFisYear + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_is_locked_in_fiscal_calendar,loFormSet.GetHeaderText("LANG_is_locked_in_fiscal_calendar",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcErrMsg2 = LANG_You_must_unlock_the_period_before_postings_can_be_released
			lcErrMsg2 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_You_must_unlock_the_period_before_postings_can_be_released,loFormSet.GetHeaderText("LANG_You_must_unlock_the_period_before_postings_can_be_released",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcErrMsg3 = LANG_Unable_to_continue_with_release
			lcErrMsg3 = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Unable_to_continue_with_release,loFormSet.GetHeaderText("LANG_Unable_to_continue_with_release",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]


			lcTmpStr  = lcPeriod + "/" + lcFisYear + ;
			IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_is_locked_in_fiscal_calendar,loFormSet.GetHeaderText("LANG_is_locked_in_fiscal_calendar",loFormSet.HeaderAlias)) +;
			IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_You_must_unlock_the_period_before_postings_can_be_released,loFormSet.GetHeaderText("LANG_You_must_unlock_the_period_before_postings_can_be_released",loFormSet.HeaderAlias)) +;
			IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Unable_to_continue_with_release,loFormSet.GetHeaderText("LANG_Unable_to_continue_with_release",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]


*N000682,1 04/16/2013 RAS Globlization changes[END ]
		OTHERWISE
*-- Passed all posting period validations
			lnResp = 3
		ENDCASE

		WAIT CLEAR

		IF llError
			lnResp = 2
*-- Posting period lcTmpStr
*-- <OK>
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
			IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
				loFormSet.oProgress.Hide()
			ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]

			= gfModalGen("INM00312B00000","Dialog",lcTmpStr)
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
			IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
				loFormSet.oProgress.Show()
			ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]


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
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  WAIT WINDOW 'Marking transaction records as released...' NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Marking_transaction_records_as_released NOWAIT
	WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Marking_transaction_records_as_released,loFormSet.GetHeaderText("LANG_Marking_transaction_records_as_released",loFormSet.HeaderAlias)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]

*-- Mark unposted records in Distribution file
	SELECT &lcGDistAl
	SET ORDER TO 0 IN &lcGDistAl
	REPLACE ALL posted WITH "X" FOR posted = " " .AND. RECNO() <= lnCurrent

ELSE
*-- If not marking records

*-- Display processing message
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  WAIT WINDOW 'Clearing batch numbers from transaction records...' NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Clearing_batch_numbers_from_transaction_records NOWAIT
	WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Clearing_batch_numbers_from_transaction_records,loFormSet.GetHeaderText("LANG_Clearing_batch_numbers_from_transaction_records",loFormSet.HeaderAlias)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]

*-- Clear GL batch number from unposted records in Distribution file
	SELECT &lcGDistAl
	SEEK " "
	REPLACE ALL glbatch WITH "", ;
	GLCOMP  WITH "" ;
	WHILE posted = " " FOR RECNO() <= lnCurrent
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

*E303295 TMI 11/18/2012 [Start]
IF .F. && gfUnCompSession(lcProgID,lnSessNo,"Release to G/L")
*E303295 TMI 11/18/2012 [End  ]
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
*E303295 TMI 11/18/2012 [Start] no need for this function
RETURN
*E303295 TMI 11/18/2012 [End  ]

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
dTranDate  WITH oAriaApplication.SystemDate,;
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
*E303295 TMI 11/12/2012 [Start] no need for this here
RETURN .T.
*E303295 TMI 11/12/2012 [End  ]
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
PARAMETERS loFormSet
lcFiles   = ''
llNoThing = lfUpdUnCmS("Close", SPACE(0))
glQuitting = .T.
loFormSet.RELEASE

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
lcReturn = SPACE(2)

IF SEEK(lcTranType,'SyGLTran')
	lcReturn = SyGLTran.Csrcmodul

	IF (OCCURS('MF',oAriaApplication.Companyinstalledmodules)=0) AND lcTranType = 'NL'
		lcReturn = 'PO'
	ENDIF
ENDIF

RETURN (lcReturn)

*!*************************************************************

FUNCTION lfvSelMod
PARAMETERS loFormSet,loFld

*- Return if no modules are linked to GL.
IF EMPTY(laModSor)
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'No modules linked to GL!')
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00000B00000',.F.,.F.,.F.,LANG_No_modules_linked_to_GL)
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
	IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
		loFormSet.oProgress.Hide()
	ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]

	=gfModalGen('TRM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_No_modules_linked_to_GL,loFormSet.GetHeaderText("LANG_No_modules_linked_to_GL",loFormSet.HeaderAlias)))
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
	IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
		loFormSet.oProgress.Show()
	ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]


*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	cbSelMod = 0

	loFormSet.Ariaform1.cbSelMod.REFRESH()
	RETURN
ENDIF

*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	= gfMover(@laModSor,@laModTar,"Transactions",.T.,.F.,.F.,.T.)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfMover(@laModSor,@laModTar,LANG_Transactions,.T.,.F.,.F.,.T.)
= gfMover(@laModSor,@laModTar,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Transactions,loFormSet.GetHeaderText("LANG_Transactions",loFormSet.HeaderAlias)),.T.,.F.,.F.,.T.)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
cbSelMod = IIF(ALEN(laModTar,1)=1 AND EMPTY(laModTar[1]),0,1)

SHOW GET cbSelMod
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	IF ASCAN(laModTar,"Material") > 0
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF ASCAN(laModTar,LANG_Material) > 0
IF ASCAN(laModTar,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Material,loFormSet.GetHeaderText("LANG_Material",loFormSet.HeaderAlias))) > 0
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	lcTrans = lcTrans + ",015,016,013,021"

ENDIF
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	IF ASCAN(laModTar,"Inventory") > 0
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF ASCAN(laModTar,LANG_Inventory) > 0
IF ASCAN(laModTar,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Inventory,loFormSet.GetHeaderText("LANG_Inventory",loFormSet.HeaderAlias))) > 0
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	lcTrans = lcTrans + ",006,007,008,013,018,019,022,023,024,025,026"

ENDIF
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	IF ASCAN(laModTar,"AR") > 0
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF ASCAN(laModTar,LANG_AR) > 0
IF ASCAN(laModTar,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AR,loFormSet.GetHeaderText("LANG_AR",loFormSet.HeaderAlias))) > 0
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	lcTrans = lcTrans + ",001,002,003,004,005,009,010,014,020"

ENDIF
llGLRel = !EMPTY(lcTrans)
llAPRel =  ASCAN(laModTar,"AP") > 0

*- Initialize three variables needed.
*--Are both MA and IC selected or unselected?
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	llExclCon = (ASCAN(laModTar,"Material") > 0 AND ASCAN(laModTar,"Inventory") > 0 ) OR (ASCAN(laModTar,"Material") = 0 AND ASCAN(laModTar,"Inventory") = 0 )
*!*	*--Is MA only selectd?
*!*	llMatMod = (ASCAN(laModTar,"Material") > 0 AND ASCAN(laModTar,"Inventory") = 0 )
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*llExclCon = (ASCAN(laModTar,LANG_Material) > 0 AND ASCAN(laModTar,LANG_Inventory) > 0 ) OR (ASCAN(laModTar,LANG_Material) = 0 AND ASCAN(laModTar,LANG_Inventory) = 0 )
llExclCon = (ASCAN(laModTar,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Material,loFormSet.GetHeaderText("LANG_Material",loFormSet.HeaderAlias))) > 0 AND ASCAN(laModTar,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Inventory,loFormSet.GetHeaderText("LANG_Inventory",loFormSet.HeaderAlias))) > 0 ) OR (ASCAN(laModTar,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Material,loFormSet.GetHeaderText("LANG_Material",loFormSet.HeaderAlias))) = 0 AND ASCAN(laModTar,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Inventory,loFormSet.GetHeaderText("LANG_Inventory",loFormSet.HeaderAlias))) = 0 )
*N000682,1 11/20/2012 MMT Globlization changes[End]

*--Is MA only selectd?
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*llMatMod = (ASCAN(laModTar,LANG_Material) > 0 AND ASCAN(laModTar,LANG_Inventory) = 0 )
llMatMod = (ASCAN(laModTar,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Material,loFormSet.GetHeaderText("LANG_Material",loFormSet.HeaderAlias))) > 0 AND ASCAN(laModTar,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Inventory,loFormSet.GetHeaderText("LANG_Inventory",loFormSet.HeaderAlias))) = 0 )
*N000682,1 11/20/2012 MMT Globlization changes[End]


*N000682,1 04/16/2013 RAS Globlization changes[END ]
loFormSet.Ariaform1.cbSelMod.REFRESH()

*!******************************************************************************************
*! Name      : lfGetModul
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/11/2000
*! Purpose   : Fill the modules mover with only installed and linked modules.
*!******************************************************************************************
FUNCTION lfGetModul

PRIVATE lcAlias
lcAlias = ALIAS()

DIMENSION laModSor[4],laModTar[4]
STORE '' TO laModSor,laModTar

*-- Are MA,AR,IC modules installed and linked to GL?
IF ALLTRIM(UPPER(gfGetMemVar('M_Link_GL',lcComp)))   = 'Y'
	IF ('MA' $ SycComp.MMODLSET)
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	    STORE "Material"  TO laModSor[1],laModTar[1]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*STORE LANG_Material  TO laModSor[1],laModTar[1]
		STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Material,loFormSet.GetHeaderText("LANG_Material",loFormSet.HeaderAlias))  TO laModSor[1],laModTar[1]
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	ENDIF
	IF  ('IC' $ SycComp.MMODLSET)
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	    STORE "Inventory" TO laModSor[2],laModTar[2]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*STORE LANG_Inventory TO laModSor[2],laModTar[2]
		STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Inventory,loFormSet.GetHeaderText("LANG_Inventory",loFormSet.HeaderAlias)) TO laModSor[2],laModTar[2]
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	ENDIF
	IF  ('AR' $ SycComp.MMODLSET)
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	    STORE "AR"       TO laModSor[3],laModTar[3]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*STORE LANG_AR      TO laModSor[3],laModTar[3]
		STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AR,loFormSet.GetHeaderText("LANG_AR",loFormSet.HeaderAlias))      TO laModSor[3],laModTar[3]
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	ENDIF
ENDIF
*-- Is AP module installed and linked to GL?
IF  ('AP' $ SycComp.MMODLSET)
*- Is AP module installed and linked to GL?
	llApInstal = .T.

*--Make sure the comany APSETUP file is open and avoid getting 'Alias aleady in use' error mesaage.
	IF USED('APSETUP')
		USE IN APSETUP
	ENDIF

	=gfOpenFile(lcGLCoDDir+'APSETUP','APSETUP','SH')

	GO TOP IN APSETUP
*--Is AP installed?
	IF !EOF('APSETUP') AND (APSETUP.CAPSGLLINK = 'Y')
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	    STORE "AP"       TO laModSor[4],laModTar[4]
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*STORE LANG_AP       TO laModSor[4],laModTar[4]
		STORE IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AP,loFormSet.GetHeaderText("LANG_AP",loFormSet.HeaderAlias))       TO laModSor[4],laModTar[4]
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]

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
FUNCTION lfGetPerd

*-- This function will return .F. if nothing for it to do.
IF !EMPTY(Tran_Date) AND !EMPTY(GlFYear) AND !EMPTY(GlPeriod)
	RETURN .F.
ENDIF

PRIVATE lcAlias,lnRecNo,llUserDate,lcTranTyp
lcAlias = ALIAS()

*--For the screen
lcTranTyp  = TRAN_desc
lcTransNo  = Tran_no
llUserDate = .F.
*E303295 TMI 11/19/2012 [Start]
SELECT &lcTmpFile
*E303295 TMI 11/19/2012 [End  ]
IF EMPTY(Tran_Date) OR EMPTY(GlFYear) OR EMPTY(GlPeriod)
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
	LOCATE FOR (EMPTY(Tran_Date) OR EMPTY(GlFYear) OR EMPTY(GlPeriod)) AND (Tran_Type+Tran_no <> lcTranKey)
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
	IF EMPTY(Tran_Date)
*--Is there any date for this tranaction?
*-- Initialize date.
		ldTranDate = Tran_Date
*--Change order to tran_no
		SET ORDER TO TAG Tran_no
		=SEEK(lcTranKey)
		SCAN WHILE Tran_Type+Tran_no = lcTranKey FOR !EMPTY(Tran_Date)
			ldTranDate = Tran_Date
			IF !EMPTY(ldTranDate)
				EXIT
			ENDIF
		ENDSCAN
		=SEEK(lcTranKey)
		IF EMPTY(ldTranDate)
			DO FORM (gcScrDir + gcWinAppl + '\SMGLDATE.scx')
			llUserDate = !EMPTY(ldTranDate)
		ENDIF
		SELECT &lcTmpFile

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
		DO FORM (gcScrDir + gcWinAppl + '\SMGLDATE.scx')
	ENDIF
	SELECT &lcTmpFile
*-- If the original date is valid or the user entered a valid date.
	IF llValid OR (!EMPTY(ldTranDate) AND CheckPrd(ldTranDate,'lcYear','lcPrd',Tran_Type))
*-- It's a valid date and we got its year and period.
*--Verify order tran_no
		SET ORDER TO TAG Tran_no
		=SEEK(lcTranKey)
		REPLACE REST Tran_Date  WITH ldTranDate,;
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

PROCEDURE lpPrnPost

PARAMETERS lnRepType

PRIVATE lcTranType, lcCustomer, lcRef


*!*	OGPlatForm   = 'DOS'
*!*	lcOGPlatForm = OGPlatForm
*!*	gcDevice     = "PRINTER"
*lcSetConsl   = SET('CONSOLE')
*SET CONSOLE OFF

*lcSetDevic = SET('DEVICE')
*SET DEVICE TO PRINTER

gcOutFile  = ''
gcOutFile  = oAriaApplication.WorkDir + gfTempName() + '.txt'
lcFrxNam   = IIF(lnRepType = 1 , 'SMRELPOS.FRX' , 'SMRELPOD.FRX')
lcCustomer = ''
lcRef      = ''
llDummy    = .T.

*-- Chart of account file
IF !USED(lccAccFile)
	=gfOpenFile(lcGLCoDDir+lccAccFile,'','SH')
ENDIF

SELE (lcTmpFile)
SET ORDER TO TAG AcctCode IN   &lccAccFile
SET RELATION TO GlAccount INTO &lccAccFile
IF llAPRel AND lnResp = 2
	SET ORDER TO (lcAPDIST) IN APDIST

*B611293,1 MHM 04/04/2017 Issue with releasing Gl [T20170327.0002][Begin]
*SET RELATION TO GLAccount+GlSession  INTO APDIST ADDITIVE
	SET RELATION TO GlAccount+GlSession+Tran_no  INTO APDIST ADDITIVE
*B611293,1 MHM 04/04/2017 Issue with releasing Gl [T20170327.0002][END]

ENDIF

IF lnRepType = 2
*-- Invoice hedear file
	IF USED(lcInvHdAl)
		SET ORDER TO TAG invhdr  IN   (lcInvHdAl)
		SET RELATION TO Tran_no  INTO (lcInvHdAl) ADDITIVE
	ENDIF
*-- Return hedear file
	IF USED(lcRetHdAl)
		SELECT (lcRetHdAl)
		gfSetOrder('rethdr')
*SET ORDER TO TAG rethdr  IN   (lcRetHdAl)
		SELECT (lcTmpFile)
		SET RELATION TO Tran_no  INTO (lcRetHdAl) ADDITIVE
	ENDIF

*E303295 TMI 11/18/2012 [Start] is included in the POSHDR table
**-- Cutticket hedear file
*IF USED(lcCutTkAl)
*  SET ORDER TO TAG CUTTKTH IN   (lcCutTkAl)
*  SET RELATION TO Tran_No  INTO (lcCutTkAl) ADDITIVE
*ENDIF
*E303295 TMI 11/18/2012 [End  ]

*-- Style purchase order hedear file
	IF USED(lcPosHdAl)
*SET ORDER TO TAG POSHDR  IN   (lcPosHdAl)
		SELECT (lcPosHdAl)
		gfSetOrder('POSHDR')
		SELECT (lcTmpFile)
		SET RELATION TO Tran_no  INTO (lcPosHdAl) ADDITIVE
	ENDIF
ENDIF

*--Message : Do you like to print or preview the posting register ?
*--Button  :                Print | Preview
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
	loFormSet.oProgress.Hide()
ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]

lnReturn = gfModalGen('QRM00373B00040','Dialog')
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
	loFormSet.oProgress.Show()
ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]



*!*	IF lnReturn = 1
*!*	  gcDevice = "PRINTER"
*!*	  OGPlatForm = 'DOS'
*!*	ELSE
*!*	  gcDevice = "SCREEN"
*!*	  OGPlatForm = 'DOS'
*!*	ENDIF

*!*	IF LASTKEY()=27

*!*	    KEYBOARD "R"
*!*
*!*	ENDIF

*!*	IF !lldummy
*!*	 RETURN
*!*	ENDIF

SELECT (lcTmpFile)
LOCATE
IF !EOF()
*E303557,1 MMT 03/09/2015 Don't close period has unreleased transactions[T20150227.0001][Start]
	IF !llDoNotShowReleaseScreen
*E303557,1 MMT 03/09/2015 Don't close period has unreleased transactions[T20150227.0001][End]
		loFormSet.Ariaform1.VISIBLE = .F.
*E303557,1 MMT 03/09/2015 Don't close period has unreleased transactions[T20150227.0001][Start]
	ENDIF
*E303557,1 MMT 03/09/2015 Don't close period has unreleased transactions[T20150227.0001][End]
	gcRepHome = oAriaApplication.ReportHome

	lcForm = gcRepHome+'SM\'+IIF(lnResp=1,'SMRELPOS','SMRELPOD')
*E303598,1 MMT 08/27/2015 Show Transaction type in Release to GL Summary report[T20150716.0011][Start]
	IF ASCAN(loFormSet.laEvntTrig, PADR('SELSUMFORM',10)) <> 0
		=loFormSet.mDoTrigger(PADR('SELSUMFORM',10))
	ENDIF
*E303598,1 MMT 08/27/2015 Show Transaction type in Release to GL Summary report[T20150716.0011][End]
	lcOutput = IIF(lnReturn=1,'TO PRINTER PROMPT NOCONSOLE','PREVIEW')
	KEYBOARD '{CTRL+F10}'
	REPORT FORM (lcForm) &lcOutput
	CLEAR TYPEAHEAD

	ERASE gcOutFile
	gcOutFile = " "
*E303557,1 MMT 03/09/2015 Don't close period has unreleased transactions[T20150227.0001][Start]
	IF !llDoNotShowReleaseScreen
*E303557,1 MMT 03/09/2015 Don't close period has unreleased transactions[T20150227.0001][End]
		loFormSet.Ariaform1.VISIBLE = .T.
*E303557,1 MMT 03/09/2015 Don't close period has unreleased transactions[T20150227.0001][Start]
	ENDIF
*E303557,1 MMT 03/09/2015 Don't close period has unreleased transactions[T20150227.0001][End]
ENDIF
IF llAPRel
	llNoGLRec = EOF()
ENDIF

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
FUNCTION lfGetType


*E303295 TMI 11/19/2012 [Start] Initialize variables
lcTranType   =  ' '
lcCustomer   =  ' '
lcRef        =  ' '
*E303295 TMI 11/19/2012 [End  ]

DO CASE

CASE Tran_Type =  'IN'
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  lcTranTYpe   =  'INVOICE             '
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTranTYpe   =  LANG_INVOICE
	lcTranType   =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVOICE,loFormSet.GetHeaderText("LANG_INVOICE",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	lcCustomer   =  EVALUATE(lcInvHdAl+".Account")

CASE Tran_Type =  'VI'
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  lcTranType   =  'VOID INVOICE        '
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTranType   =  LANG_VOID_INVOICE
	lcTranType   =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_VOID_INVOICE,loFormSet.GetHeaderText("LANG_VOID_INVOICE",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	lcCustomer   =  EVALUATE(lcInvHdAl+".Account")

CASE Tran_Type =  'CR'
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  lcTranType   =  'CASH RECEIPT        '
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTranType   =  LANG_CASH_RECEIPT
	lcTranType   =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CASH_RECEIPT,loFormSet.GetHeaderText("LANG_CASH_RECEIPT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	SELECT (lcCredtAl)
	LOCATE FOR BATCH = &lcTmpFile->Tran_no
	lcCustomer   =  EVALUATE(lcCredtAl + ".Account")
	lcRef        =  EVALUATE(lcCredtAl + ".REFERENCE")
	SELECT &lcTmpFile

CASE Tran_Type =  'CA'
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  lcTranType   =  'CREDIT ADJUSTMENT   '
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTranType   =  LANG_CREDIT_ADJUSTMENT
	lcTranType   =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CREDIT_ADJUSTMENT,loFormSet.GetHeaderText("LANG_CREDIT_ADJUSTMENT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	SELECT (lcCredtAl)
	LOCATE FOR TRAN = &lcTmpFile->Tran_no
	lcCustomer   =  EVALUATE(lcCredtAl + ".Account")
	lcRef        =  EVALUATE(lcCredtAl + ".REFERENCE")
	SELECT &lcTmpFile

CASE Tran_Type =  'DA'
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  lcTranType   =  'DEBIT ADJUSTMENT    '
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTranType   =  LANG_DEBIT_ADJUSTMENT
	lcTranType   =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DEBIT_ADJUSTMENT,loFormSet.GetHeaderText("LANG_DEBIT_ADJUSTMENT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[Start]
	SELECT (lcDebitAl)
	LOCATE FOR TRAN = &lcTmpFile->Tran_no
	lcCustomer   =  EVALUATE(lcDebitAl + ".Account")
	lcRef        =  EVALUATE(lcDebitAl + ".REFERENCE")
	SELECT &lcTmpFile

CASE Tran_Type =  'RM'
	gfSeek(&lcTmpFile..Tran_no,lcRetHdAl)
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  lcTranType   =  'RETURN MERCHANDISE  '
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTranType   =  LANG_RETURN_MERCHANDISE
	lcTranType   =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_RETURN_MERCHANDISE,loFormSet.GetHeaderText("LANG_RETURN_MERCHANDISE",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	lcCustomer   =  EVALUATE(lcRetHdAl+".Account")
	lcRef        =  EVALUATE(lcRetHdAl+".REFERENCE")

CASE Tran_Type =  'VR'
	gfSeek(&lcTmpFile..Tran_no,lcRetHdAl)
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  lcTranType   =  'VOID RETURN         '
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTranType   =  LANG_VOID_RETURN
	lcTranType   =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_VOID_RETURN,loFormSet.GetHeaderText("LANG_VOID_RETURN",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	lcCustomer   =  EVALUATE(lcRetHdAl+".Account")
	lcRef        =  EVALUATE(lcRetHdAl+".REFERENCE")

CASE Tran_Type =  'IP'
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  lcTranType   =  'INVENTORY PHYSICAL  '
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTranType   =  LANG_INVENTORY_PHYSICAL
	lcTranType   =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVENTORY_PHYSICAL,loFormSet.GetHeaderText("LANG_INVENTORY_PHYSICAL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]

CASE Tran_Type =  'IA'
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  lcTranType   =  'INVENTORY ADJUSTMENT'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTranType   =  LANG_INVENTORY_ADJUSTMENT
	lcTranType   =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVENTORY_ADJUSTMENT,loFormSet.GetHeaderText("LANG_INVENTORY_ADJUSTMENT",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]

CASE Tran_Type =  'PO'
	gfSeek('PP'+&lcTmpFile..Tran_no,lcPosHdAl)
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  lcTranType   =  'P/O RECEIVING       '
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTranType   =  LANG_PO_RECEIVING
	lcTranType   =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PO_RECEIVING,loFormSet.GetHeaderText("LANG_PO_RECEIVING",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	lcCustomer   =  EVALUATE(lcPosHdAl+".Account")

CASE Tran_Type =  'CT'
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  lcTranType   =  'C/T RECEIVING       '
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTranType   =  LANG_CT_RECEIVING
	lcTranType   =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CT_RECEIVING,loFormSet.GetHeaderText("LANG_CT_RECEIVING",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
*lcCustomer   =  EVALUATE(lcCutTkAl+".Account")

CASE Tran_Type =  'ZE'
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  lcTranType   =  'ZERO OUT STOCK      '
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTranType   =  LANG_ZERO_OUT_STOCK
	lcTranType   =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ZERO_OUT_STOCK,loFormSet.GetHeaderText("LANG_ZERO_OUT_STOCK",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]

*- Add the needed types to show the APDIST transactions
CASE Tran_Type = 'A '
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  lcTranTYpe   = 'DM application'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTranTYpe   = LANG_DM_application
	lcTranType   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DM_application,loFormSet.GetHeaderText("LANG_DM_application",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	lcCustomer   = APDIST.CVENDCODE

CASE Tran_Type = 'B '
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  lcTranTYpe   = 'Bank adjustment'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTranTYpe   = LANG_Bank_adjustment
	lcTranType   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Bank_adjustment,loFormSet.GetHeaderText("LANG_Bank_adjustment",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	lcCustomer   = APDIST.CVENDCODE

CASE Tran_Type = 'H '
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  lcTranTYpe   = 'Cash payments'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTranTYpe   = LANG_Cash_payments
	lcTranType   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Cash_payments,loFormSet.GetHeaderText("LANG_Cash_payments",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	lcCustomer   = APDIST.CVENDCODE
	lcRef        = APDIST.CINVNO

CASE Tran_Type = 'I '
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  lcTranTYpe   = 'AP Invoice'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTranTYpe   = LANG_AP_Invoice
	lcTranType   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_AP_Invoice,loFormSet.GetHeaderText("LANG_AP_Invoice",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	lcCustomer   = APDIST.CVENDCODE

CASE Tran_Type = 'M '
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  lcTranTYpe   = 'Manual checks'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTranTYpe   = LANG_Manual_checks
	lcTranType   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Manual_checks,loFormSet.GetHeaderText("LANG_Manual_checks",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	lcCustomer   = APDIST.CVENDCODE
	lcRef        = APDIST.CINVNO

CASE Tran_Type = 'N '
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  lcTranTYpe   = 'Non check payment'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTranTYpe   = LANG_Non_check_payment
	lcTranType   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Non_check_payment,loFormSet.GetHeaderText("LANG_Non_check_payment",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	lcCustomer   = APDIST.CVENDCODE
	lcRef        = APDIST.CINVNO

CASE Tran_Type = 'P '
*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	  lcTranTYpe   = 'Printed checks'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTranTYpe   = LANG_Printed_checks
	lcTranType   = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Printed_checks,loFormSet.GetHeaderText("LANG_Printed_checks",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]
	lcCustomer   = APDIST.CVENDCODE
	lcRef        = APDIST.CINVNO
ENDCASE

SELECT (lcTmpFile)

************************************************************
*! Name      : gfThermo
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 11/08/2012
*! Purpose   : Show the Progress bar
************************************************************
FUNCTION gfThermo
PARAMETERS lnTotRecs, lnThermRec,lcFirstLabel,lcSndLable

lcFirstLabel  = IIF(EMPTY(lcFirstLabel ), ' ' , lcFirstLabel )
lcSndLable    = IIF(EMPTY(lcSndLable   ), ' ' , lcSndLable   )
IF lnTotRecs = lnThermRec
	loFormSet.oProgress = NULL
	RETURN
ENDIF

IF ISNULL(loFormSet.oProgress)
	loFormSet.oProgress = NEWOBJECT('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
ENDIF

WITH loFormSet.oProgress
	.TotalProgress = lnTotRecs
	.lblFirstLabel.CAPTION = lcFirstLabel
	.lblSecondLabel.CAPTION = lcSndLable
	.CurrentProgress(lnThermRec)
	.SHOW()
ENDWITH
*- End of lfShowProgress	.

************************************************************
*! Name      : MSG
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/05/2012
*! Purpose   : MSG, can run only on my PC
************************************************************
FUNCTION msg
PARAMETERS llDoNotUseStep
IF SYS(0)='DEV4 # tarek'
	ON ERROR
	_SCREEN.VISIBLE=.T.
	IF !llDoNotUseStep
		IF FILE('C:\TEMP\X.X')
			SET STEP ON
		ENDIF
	ENDIF
ENDIF
*- End of MSG.


************************************************************
*! Name      : lfApAcs
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 12/06/2012
*! Purpose   :
*!***********************************************************
*! This function is to validate the accounts from the chart of account
*! of the active company or the chart of account of another company.
*!*************************************************************************
FUNCTION lfApAcs
PARAMETERS loFld,lcAcsDes,llBrowse, llValidAcs

** If the parameter is undefined.

lcAcsDes    = IIF(TYPE('lcAcsDes') $ 'UL','',lcAcsDes)

lcSavAlias  = ALIAS()  && Variable to save the selected alias.

llBrowse    = IIF(TYPE('llBrowse') $ 'U',.F.,llBrowse)

llValidAcs  = .T.

*B610170,1 TMI 12/06/2012 [Start] use the lofld instead
lcFieldCont = EVALUATE(SYS(18))   && Assign the content of the field to the variable.
lcFieldCont = loFld.KeyTextbox.VALUE
*B610170,1 TMI 12/06/2012 [End  ]

*** Variable hold an empty account to compair with. ***
*B610791,1 TMI 08/04/2014 12:16 [Start]
lnApsAcLen = LEN(ALLTRIM(ap1.lcApsAcMas))
llApGlLink = ap1.llApGlLink
*B610791,1 TMI 08/04/2014 12:16 [End  ]

lcEmptyAcs = REPLICATE('0',lnApsAcLen)

IF llApGlLink .AND. ( llBrowse .OR. lcFieldCont <> lcEmptyAcs)

	SELECT('lcLinkChar')
	SET ORDER TO TAG AcctCode

	IF llBrowse .OR. !SEEK(lcFieldCont) .OR. ATC('?',lcFieldCont) > 0
		llBrowse = .F.
		DIMENSION laTemp[2]
		laTemp = ''
*B610791,1 TMI 08/04/2014 12:42 [Start] comment this
*lcSavBrFld=lcBrfields
*lcSavTitle=lcFile_Ttl
*B610791,1 TMI 08/04/2014 12:42 [End  ]

*N000682,1 04/16/2013 RAS Globlization changes[Start]
*!*	    lcBrfields="CACCTCODE :H= 'Account Code',;
*!*	                CACCNLDES :H= 'Account Description'"

*!*	    lcFile_Ttl="Chart of accounts"
		lcBrfields="CACCTCODE :H= '"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Account_Code,loFormSet.GetHeaderText("LANG_Account_Code",loFormSet.HeaderAlias))+"',"+;
		"CACCNLDES :H='"+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Account_Description,loFormSet.GetHeaderText("LANG_Account_Description",loFormSet.HeaderAlias))+"'"
*N000682,1 11/20/2012 MMT Globlization changes[End]


*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFile_Ttl=LANG_Chart_of_accounts
		lcFile_Ttl=IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Chart_of_accounts,loFormSet.GetHeaderText("LANG_Chart_of_accounts",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 RAS Globlization changes[END ]

		=gfbrows(' ','CACCTCODE,CACCNLDES','laTemp')

*B610791,1 TMI 08/04/2014 12:42 [Start] comment this
*lcFile_Ttl=lcSavTitle
*lcBrfields=lcSavBrFld
*B610791,1 TMI 08/04/2014 12:42 [End  ]

		IF !EMPTY(laTemp[1])
			lcFieldCont = ALLTRIM(laTemp[1])
			lcAcsDes    = ALLTRIM(laTemp[2])
		ELSE
			lcFieldCont = REPLICATE('0',lnApsAcLen)
			lcAcsDes    = ' '
			llValidAcs  = .F.
		ENDIF
	ELSE
		lcAcsDes      = ALLTRIM(CACCNLDES)
	ENDIF

*B606903,1 KHM 02/22/2003 (Begin) Do not select the inactive account
	IF !EOF() AND CSegActiv = "I"
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
		IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
			loFormSet.oProgress.Hide()
		ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]

		=gfModalGen("TRM04202B00000","DIALOG",ALLTRIM(lcFieldCont))
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][Start]
		IF TYPE('loFormSet.oProgress') ='O' AND !ISNULL(loFormSet.oProgress)
			loFormSet.oProgress.Show()
		ENDIF
*B612177,1 Es 07/21/2020 The Release to GL messages displayed while release process are displayed behind the progress bar. [T20200616.0001][End]


		lcFieldCont = REPLICATE('0',lnApsAcLen)
		lcAcsDes    = ' '
		llValidAcs  = .F.
		_CUROBJ = _CUROBJ
	ENDIF
*B606903,1 KHM 02/22/2003 (End)

	IF !EMPTY(lcSavAlias)
		SELECT(lcSavAlias)
	ENDIF
*B610170,1 TMI 12/06/2012 [Start]
*lcVarName = SYS(18)
*&lcVarName= lcFieldCont
	loFld.KeyTextbox.VALUE = lcFieldCont
*B610170,1 TMI 12/06/2012 [End  ]
ENDIF

IF !EMPTY(lcSavAlias)
	SELECT(lcSavAlias)
ENDIF

RETURN llValidAcs
*- End of lfApAcs.
