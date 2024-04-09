*!*************************************************************
*! Name      : gpRlsPbl
*! Developer : Yasser El Oraby
*! Date      : June 1995 
*! Purpose   : To Release Public Variables
*!*************************************************************
*! Called by : ARIA3.PRG
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO gpRlsPbl
*!*************************************************************
*E300260,4 ORABY 07/09/1995 Stop releasing variables used in ARIA APPAREL
*E300323,4 RENEE 12/21/1995 Stop releasing variables used in ARIA APPAREL,
*E300323,4                  called from gfModalGen()
*E300323,4                  gcSysName, gnMsgRec, gcMesg_Str.
*B600822,1 Hesham El-Sheltawi 07/21/96
*B600822,1 Do not release gcRsrcHome
*E300399,1 Hesham El_sheltawi 09/24/96
*E300399,1 Do not release variables that is needed in the Option grid
*E300399,1 so when running the option grid for apparel report it works
*E300399,1 with out errors
*E300627,1 Hesham El-Sheltawi  03/31/97
*E300627,1 Release the variable that hold the installed modules for the active comp.
*E300627,1 this variable name is gcCmpModules
*E300713,1 Hesham 07/29/97
*E300713,1 release global variables for current year & period
*E300842,1 Hesham 03/23/98
*E300842,1 release variable gcCurSite that hold the current site id
*E300797,1 Hesham 11/25/98
*E300797,1 release variable glErrorHand
*E301098,1 Hesham 12/16/98 remove global variable use for DataPath
*

*** RELEASE arrayes
RELEASE gaAct_Wind,gaMnu_Fl,gaBmpNams,gaProcess,gaUsr_task,gaMarkBars

*** RELEASE variables
RELEASE gnMant_Bar,gnBar_No,gnMesgAdd,gnWinSepta,gnMaxButt,gnButtPos,gnOldFiles,gnMyStRec
*E300323,4 Do not release gcSysName 
*RELEASE gcDevice,gcRepHome,gcSysName,gcBmpHome,gcRsrcHome,gcUpgrdLvl,gcAriaVer,gcBaseWind
*B600822,1 Hesham El-Sheltawi (Start)
*B600822,1 Do not release gcRsrcHome
*RELEASE gcDevice,gcRepHome,gcBmpHome,gcRsrcHome,gcUpgrdLvl,gcAriaVer,gcBaseWind
*E300399,1 Hesham El_sheltawi (Start)
*RELEASE gcDevice,gcRepHome,gcBmpHome,gcUpgrdLvl,gcAriaVer,gcBaseWind
RELEASE gcDevice,gcBmpHome,gcUpgrdLvl,gcAriaVer,gcBaseWind
*E300399,1 Hesham El_sheltawi (End)
*B600822,1 Hesham El-Sheltawi (End)
*E300323,4 Do not release gcMesg_Str.
*RELEASE gcPop_Name,gcReadClr,gcMesg_Str,gcHomeWin,gcManager,gcOutFile
*E300399,1 Hesham El_sheltawi (Start)
*RELEASE gcPop_Name,gcReadClr,gcHomeWin,gcManager,gcOutFile
RELEASE gcPop_Name,gcReadClr,gcHomeWin,gcManager
*E300399,1 Hesham El_sheltawi (End)
*E300323,4 end.
RELEASE gcAppHome,gcDataDir,gcAllCmp,gcColorSet,gcCom_Name
*E300581,1 HESHAM EL-SHELTAWI (START)
*RELEASE gcNextprog,gcOld_Appl,gcApp_Name,gcAct_Appl,gcAct_Pad
RELEASE gcNextprog,gcPrgPram,gcOld_Appl,gcApp_Name,gcAct_Pad
*E300581,1 HESHAM EL-SHELTAWI (END)
RELEASE gcComp_Mdl,gcUser_Grp,gcComp_Lvl,gcPrnt_Cmp,gcUser_Mdl,gcDef_path,gcNextRepo,gcOldDbf
RELEASE glEndprog,glQuitting,glFirsTime,glInitMenu,glRestore,glReLogIn,gcUser_Com,glToolActv,glDoprog,glQuitProg,glCancel
*E300581,1 HESHAM EL-SHELTAWI (Start)
*RELEASE glSkip_Mnt,glNoLog,glMsgRem,glAutoAdd,glParntMdl,glUser_Tsk,glLog_Requ
RELEASE glSkip_Mnt,glNoLog,glMsgRem,glAutoAdd,glParntMdl,glUser_Tsk
*E300581,1 HESHAM EL-SHELTAWI (END)
*E300399,1 Hesham El_sheltawi (Start)
*E300581,1 HESHAM EL-SHELTAWI (Start)
*RELEASE gcGen_pd,gnPrint_Wd,glEscPrs,glPopAct,glChngPass,gnDateWdth,gcHelpTopc
RELEASE glEscPrs,glPopAct,gnDateWdth,gcHelpTopc
*E300581,1 HESHAM EL-SHELTAWI (End)
*E300399,1 Hesham El_sheltawi (End)
*E300323,4 Do not release gnMsgRec.
*RELEASE gnTimeOut,glGo2Objct,gcUser_bmp,glChildTop,gnCompBar,glTooAlon,gnMsgRec
*E300581,1 HESHAM EL-SHELTAWI (Start)
*RELEASE gnTimeOut,glGo2Objct,gcUser_bmp,glChildTop,gnCompBar,glTooAlon
RELEASE gnTimeOut,glGo2Objct,gcUser_bmp,glChildTop,gnCompBar,gcCmpModules
*E300581,1 HESHAM EL-SHELTAWI (END)
*E300323,4 end.
RELEASE gcPlatform,glMapPath,gcAdmnPass,glCmpCreat,gcAct_Key,gnAct_pos,gcPrmtMdls,gcCompName,gcLicence
RELEASE glMUpdate
*E300260,4 These variables will be used in ARIA APPAREL so do not release them
*RELEASE gcUser_ID,gcStation,gcUserName,gcSysHome,gcAct_Comp
*E300713,1 Hesham (Start)
*E300713,1 release global variables for current year & period
*E300842,1 Hesham (Start)
*RELEASE gcCurrYear,gcCurrPrd
*E300797,1 Hesham (Start)
*RELEASE gcCurrYear,gcCurrPrd,gcCurSite
*E301098,1 Hesham (Start)
*E301098,1 remove variable gcDataEnv
*RELEASE gcCurrYear,gcCurrPrd,gcCurSite,glErrorHand
RELEASE gcCurrYear,gcCurrPrd,gcCurSite,glErrorHand,gcDataEnv
*E301098,1 Hesham (End)
*E300797,1 Hesham (End)
*E300842,1 Hesham (End)
*E300713,1 Hesham (End)
*E300323,4 Clear variable gcMesg_Str
gcMesg_Str = ''
*E300323,4 end.
