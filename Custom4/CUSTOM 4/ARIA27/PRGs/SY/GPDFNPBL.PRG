*E300399,1 Hesham 05/26/96 Define variable to hold if the printing
*E300399,1 is auto compression or not the variable name is glPrtAComp
*B601081,1 Hesham 05/28/96 Define variable to hold The system files
*B601081,1 path in SYCINST variable name is gcOrgPath
*E300581,1 Hesham 02/26/96 
*E300581,1 Define Public variables used by the apparel system
*E300627,1 Hesham El-Sheltawi  03/31/97
*E300627,1 define variable to hold the installed modules for the active comp.
*E300713,1 Hesham 07/29/97
*E300713,1 define global variables for current year & period
*E300842,1 Hesham 03/23/98
*E300842,1 define variable to hold the current site id
*E300797,1 Hesham 11/25/98
*E300797,1 Add global variable use for toggling the error handler
*E301098,1 Hesham 12/16/98 Add global variable use for DataPath gcDataEnv

*!*************************************************************
*! Name      : gpDfnPbl
*! Developer : Yasser El Oraby
*! Date      : June 1995 
*! Purpose   : To Define Public Variables
*!*************************************************************
*! Called by : ARIA3.PRG
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO gpDfnPbl
*!*************************************************************
*

*** Public arrayes
PUBLIC ARRAY gaAct_Wind[1,4],gaMnu_Fl[1,6],gaBmpNams[1,2],gaProcess[1,2],gaUsr_task[1,7]
PUBLIC ARRAY gaMarkBars[1,3]

*** Public variables
PUBLIC gnMant_Bar,gnBar_No,gnMesgAdd,gnWinSepta,gnMaxButt,gnButtPos,gnOldFiles,gnMyStRec
PUBLIC gcDevice,gcRepHome,gcSysName,gcBmpHome,gcRsrcHome,gcUpgrdLvl,gcAriaVer,gcBaseWind
PUBLIC gcPop_Name,gcReadClr,gcMesg_Str,gcHomeWin,gcManager,gcOutFile,gcUserName
PUBLIC gcSysHome,gcAppHome,gcDataDir,gcWorkDir,gcAllCmp,gcColorSet,gcCom_Name
PUBLIC gcNextprog,gcPrgPram,gcUser_ID,gcStation,gcOld_Appl,gcApp_Name,gcAct_Appl,gcAct_Comp,gcAct_Pad
PUBLIC gcComp_Mdl,gcUser_Grp,gcComp_Lvl,gcPrnt_Cmp,gcUser_Mdl,gcDef_path,gcNextRepo,gcOldDbf
PUBLIC glEndprog,glQuitting,glFirsTime,glInitMenu,glRestore,glReLogIn,gcUser_Com,glToolActv,glDoprog,glQuitProg,glCancel
PUBLIC glSkip_Mnt,glNoLog,glMsgRem,glAutoAdd,glParntMdl,glUser_Tsk,glLog_Requ
PUBLIC gcGen_pd,gnPrint_Wd,glEscPrs,glPopAct,glChngPass,gnDateWdth,gcHelpTopc
PUBLIC gnTimeOut,glGo2Objct,gcUser_bmp,glChildTop,gnCompBar,glTooAlon,gnMsgRec
PUBLIC gcPlatform,glMapPath,gcAdmnPass,glCmpCreat,gcAct_Key,gnAct_pos,gcPrmtMdls,gcCompName,gcLicence
PUBLIC glMUpdate,gcPlatForm,gcIntCont,gcBaseCurr,glPrtAComp,gcOrgPath,gcPrnt_Port,glSys_Log
*E300627,1 define variable gcCmpModules to hold the installed modules for the
*E300627,1 active comp.
PUBLIC gc10CPInch,gc12CPInch,gc16CPInch,gcUser_lvl,gcCmpModules
*E300713,1 Hesham (Start)
*E300713,1 define global variables for current year & period
PUBLIC gcCurryear,gcCurrPrd
*E300713,1 Hesham (End)
*E300581,1 Hesham El-Sheltawi (Start)
*************************** Apparel Public Variables ***********************
PUBLIC CHOICE,DATA,QCLRNRM,QCLRSAY,QCLRMSG,QCLRMAG,QCLRREV,QCLRBLK,QPGUKEY
PUBLIC QPGDKEY,QDELKEY,QINSKEY,QESCKEY,QRETKEY,QUPKEY,QDWNKEY,QRGTKEY,QLFTKEY
PUBLIC QHMEKEY,QENDKEY,QFOXVER2,qTopMsg,qLstKey,S_CLOSEALL,S_NET,S_ENV,S_TIMEOUT
PUBLIC S_PRINTER,qBatchMod,qUserPrt,QREPO_SCR,QLASTROWY3,QPRDCNTRL,QBEGBALDT
PUBLIC gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,gnBrHSRow1,gnBrHSCol1
PUBLIC gnBrHSRow2,gnBrHSCol2,gcErrorTxt,glNewDevl,gcARPrmpt
*E300842,1 Hesham (Start)
*E300842,1 define variable to hold the current site id
*PUBLIC gcImagDir,gcLibDir,gcScrDir,gdSysDate
*E300797,1 Hesham (Start)
*E300797,1 Add global variable use for toggling the error handler
*PUBLIC gcImagDir,gcLibDir,gcScrDir,gdSysDate,gcCurSite
*E301098,1 Hesham (Start)
*E301098,1 Add global variable use for DataPath
*PUBLIC gcImagDir,gcLibDir,gcScrDir,gdSysDate,gcCurSite,glErrorHand
PUBLIC gcImagDir,gcLibDir,gcScrDir,gdSysDate,gcCurSite,glErrorHand,gcDataEnv
*E301098,1 Hesham (End)
*E300797,1 Hesham (End)
*E300842,1 Hesham (End)
*************************** Apparel Public Arrays ***********************
PUBLIC ARRAY QTEMPNAME(1),QRECNO(24),BP(10),gaMDIPrgs[1]
*E300581,1 Hesham El-Sheltawi (End)
