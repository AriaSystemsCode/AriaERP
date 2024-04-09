*:************************************************************************
*:  Program File: \ARIA4XP\PRGS\SM\SMCLOSE.PRG
*:  Module      : System Manager
*:  Desc.       : Close Year\Period
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 01/15/2013
*:  Reference   : **E303343,1**
*:************************************************************************
* modifications
*B610361,1 fix problems resulted from globalization TMI 06/06/13 [Start] comment and move down
*B610484,1 TMI 08/27/2013 fix some problems while closing a year [T20130802.0008] 
*B610489,1 TMI 09/01/2013 make the array 18 columns [T20130805.0033]
*B610674,1 TMI 02/07/2014 Modify the GLCLOSE.PRG program to not check the two fields CSETSUSMJ,CSETRETMJ
*B610674,1                when the company is not linked to GL while closing a period. [T20140117.0056] 
*E303557,1 MMT 03/09/2015 Don't close period has unreleased transactions[T20150227.0001]
*B611023,1 MMT 07/02/2015 Close Period screen gives error when user changes company[T20150630.0011]
*E303959,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL 
*B611581,1 MMT 07/18/2018 Can Close period because of un-released GL entries and company is not linked to GL[T20180510.0019]
*B612064,1 MMT 02/19/2020 Fix bug of not excluding 1099 records in APDIST while checking unreleased entries[T20200217.0003]
*B612515,1 MMT 01/24/2022 Show progress bar while closing and avoid ICSTYHST duplication[T20220105.0001]
*B612703,1 MMT 03/31/2024 Fix error while closing period if there is no GL setup[T-ERP-20230927.0002]
*:************************************************************************
PARAMETERS lcClose

#INCLUDE r:\aria4xp\screens\sm\smclose.h
 
*- Get the screen , call it
lcRunScx = lfGetScx("SM\SMCLOSE.scx")
DO FORM (lcRunScx) WITH lcClose

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
  lcRunScx = oAriaApplication.screenhome+lcScx
ENDIF
RETURN lcRunScx
 *- End of lfGetScx.

*!*************************************************************
*! Name      : lfFormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : called from the Screen init Method
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet

IF EMPTY(loFormSet.lcClose)
  RETURN .F.
ENDIF

loFormSet.AddProperty('lcProgName','SMCLOS'+loFormSet.lcClose)

*!*	*- Set functions to the APMAIN.FXP
lcPath = oAriaapplication.ApplicationHome
SET PROCEDURE TO (lcPath+'AP\APMAIN.FXP') ADDITIVE
SET PROCEDURE TO (lcPath+'GL\GL.FXP') ADDITIVE

*- Open tables
*=lfOpenPRGFILES(loFormSet.lcProgName)

*- Call to assign
*=lfGL(loFormset)

IF USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF
=gfOpenFile(oAriaApplication.SysPath+'SYCCOMP' ,'CCOMP_ID' ,'SH')
=gfOpenFile(oAriaApplication.SysPath+'SYDFIELD','CFLD_NAME','SH')


*- Define needed variables.
IF !lfDefineVars(loFormSet)
  RETURN .F.
ENDIF

*- control source
WITH loFormset.AriaForm1
  .laCompany.RowSource = 'ThisformSet.laCompany'
  .laCompany.ControlSource = 'ThisformSet.lcCompany'
  .Refresh()
ENDWITH

SELECT FISHD
LOCATE
lnCnt = 0
COUNT FOR CFISYSTAT = 'C' TO lnCnt
IF lnCnt<>1
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_SMCLOSE_No_current_year)  &&'No current year , or more than one, pls correct.'
=gfModalGen('INM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_No_current_year,loFormSet.GetHeaderText("LANG_SMCLOSE_No_current_year",loFormSet.HeaderAlias)))  &&'No current year , or more than one, pls correct.'
*N000682,1 11/20/2012 MMT Globlization changes[End]

  RETURN .F.
ENDIF
LOCATE

LOCATE FOR CFISYSTAT = 'C'
IF loFormSet.laCompany[loFormSet.puCompany,4] <> fishd.CFISFYEAR
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_SMCLOSE_Current_year_mismatch_error)  && 'Current year mismatch error.'
  *B612515,1 MMT 01/24/2022 Show progress bar while closing and avoid ICSTYHST duplication[T20220105.0001][Start]
  *=gfModalGen('INM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_Current_year_mismatch_error,loFormSet.GetHeaderText("LANG_SMCLOSE_Current_year_mismatch_error",loFormSet.HeaderAlias)))  && 'Current year mismatch error.'
  
*N000682,1 11/20/2012 MMT Globlization changes[End]

*!*	  RETURN .F.
  SELECT FSPRD
  =SEEK(fishd.CFISFYEAR,'FSPRD','COMFYRPRDI')
  LOCATE REST WHILE CFISFYEAR+CFSPPRDID =fishd.CFISFYEAR FOR !FSPRD.lfspclsds 
  IF FOUND()
    loFormSet.laCompany[loFormSet.puCompany,4] = fishd.CFISFYEAR
    loFormSet.laCompany[loFormSet.puCompany,3] = FSPRD.CFSPPRDID 
    SELECT SYCCOMP
    =SEEK(loFormSet.laCompany[loFormSet.puCompany,2],'SYCCOMP','CCOMP_ID')
    REPLACE ccurr_prd WITH FSPRD.CFSPPRDID ,;
		    ccurr_yer WITH fishd.CFISFYEAR IN Syccomp 
  ELSE
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_Current_year_mismatch_error,loFormSet.GetHeaderText("LANG_SMCLOSE_Current_year_mismatch_error",loFormSet.HeaderAlias)))  && 'Current year mismatch error.'  
    RETURN .F.
  ENDIF
  *B612515,1 MMT 01/24/2022 Show progress bar while closing and avoid ICSTYHST duplication[T20220105.0001][End]
ENDIF




*- End of lfFormInit.

************************************************************
*! Name      : lfSMClose
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/15/2013
*! Purpose   : close period / year program
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet

*- the screen set of variables
loFormSet.AddProperty('laVars[1,2]')
loFormSet.laVars[1,1] = 'lcDummy'  && just define it for the function does not crash when called

** Array declaration **
=lfAddProp('laCompany[1]',' ')
=lfAddProp('laFileOpen[9,3]',' ')
*=lfAddProp('laFileStru[1]','')
=lfAddProp('laSFileOpn[4,3]')

** Itializing the character variables **

=lfAddProp('lcCompany'  , ' ' , .T. )       && Variable to hold the company name.
=lfAddProp('puCompany'  , 0   , .T. )       && Variable to hold the popup selection.
*E303343,1 TMI 01/16/2013 [Start] comment this, not needed.
*=lfAddProp('laCompany'  , ' ' )       && Array intialization.
*E303343,1 TMI 01/16/2013 [End  ]
=lfAddProp('lcSavComp'  , ' ' )       && Variable to Save the Active Company.
=lfAddProp('lcSavData'  , ' ' )       && Variable to Save the Active Company Data Dir.
=lfAddProp('lcIndExp1'  , ' ' )       && Variable to hold the index Exp.
=lfAddProp('lcTagExp1'  , ' ' )       && Variable to hold the tag Exp.
=lfAddProp('lcIndExp2'  , ' ' )       && Variable to hold the index Exp.
=lfAddProp('lcTagExp2'  , ' ' )       && Variable to hold the tag Exp.
=lfAddProp('lcDfRetAct' , ' ' )       && Variable to hold the Retained Account.
=lfAddProp('lcSusActMj' , ' ' )       && Variable to hold the Suspence Account Major.
=lfAddProp('lcAcsMask'  , ' ' )       && Variable to hold the Account Mask.
=lfAddProp('lcAcsegDes' , ' ' )       && Variable to hold the Account Segment Discription.
=lfAddProp('lcDataDir'  , ' ' )       && Variable to store the company dir.
=lfAddProp('lcPath'     , ' ' )       && Variable to hold the path of the setup file.
=lfAddProp('lcTmpFisHD' , ' ' )       && Var to hold name of Fiscal year temp file.
=lfAddProp('lc_TempPR'  , ' ' )       && Var to hold name of Periods temp file.
=lfAddProp('lcClosStat' , 'N' )       && Var to hold the status of the closing date.
*lcComp_ID'  = oAriaApplication.prntcompanyid && Variable to hold the company ID, First initialization by the active company.

*- Add variables for Alias names for ACCOD, FISHD, FSPRD, FSHLD files
STORE '' TO lcAccCod, lcFisHd, lcFsPrd, lcFsHld

=lfAddProp('lcSpcChr'   , '|/-\')
=lfAddProp('lcNewYear'  , ''  )
=lfAddProp('lcNewNxtYr' , ''  )

** Variables already declared in the screen object file **
*E303343,1 TMI 01/15/2013 [Start]
*!*	lcTCloseU  = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding the word close in upper case.
*!*	lcTOpen    = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding the word open in lower case.
*!*	lcTLock    = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding the word lock in lower case.
*!*	lcTCloseP  = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding the title of the screen;
*!*	                           in case of closing period.
*!*	lcTCloseY  = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding the title of the screen;
*!*	                           in case of closing year & period.
*!*	lcTUnpostd = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding the word unposet single;
*!*	                           transaction.
*!*	lcTUnGenAl = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding the word ungenerated   ;
*!*	                           allocations transaction.
*!*	lcTUnGenRc = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding the word ungenerated   ;
*!*	                           recurrind transaction.
*!*	lcTColect  = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding the message collecting ;
*!*	                           companies information.
*!*	lcTUnPostB = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding the word unposet batches.
*!*	lcTGenRevT = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding the word Generating revers;
*!*	                           transactions.
*!*	lcTGenRevB = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding the word Generating revers;
*!*	                           batches.
*!*	lcTUpdMast = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding the word Updatind the  ;
*!*	                           master files.
*!*	lcTFiscal  = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding the title of the ficsacl;
*!*	                           header screen.
*!*	lcTAdjBal  = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding 'Adjusting balance...'.
*!*	lcTAdjPsYP = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding 'Adjusting posted year ;
*!*	                           & period...'
*!*	lcTBatHand = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding 'Batch handeling...'
*!*	lcTRclasfi = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding 'Reclacified from the  ;
*!*	                           batch file.'
*!*	lcTRPostBt = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding 'Repost batchs &     ;
*!*	                           transactions...'
*!*	lcTUpdBal  = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding 'Update balances...'
*!*	lcTGnClsEn = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding 'Generate closing      ;
*!*	                           entries...'
*!*	lcTCrClsEn = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding 'Creat closing entries.'
*!*	lcTYECNo   = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding 'YEC No. '
*!*	lcTPrior   = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding 'Prior year closing    ;
*!*	                           entries by '.
*!*	lcTClsEntN = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding 'Closing entry No. '.
*!*	lcTCompFil = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding 'Update the Company file'.
*!*	lctClsDone = ' '        && Variable is already declared in the screen ;
*!*	                           object file holding 'done'.
*E303343,1 TMI 01/15/2013 [End  ]

** Itializing the logical variables **

=lfAddProp('llOpenBy'   , .F. )       && Variable to hold if the selected file is;
                           open by my.
=lfAddProp('llSetDone'  , .F. )       && Variable to hold the variable of the lSetDon;
                            from the GLSETUP.DBF.
=lfAddProp('llSetUpMaj' , .F. )       && Variable to hold the variable of the CSETRETMJ;
                            from the GLSETUP.DBF.
=lfAddProp('llFileLock' , .F. )       && Variable to hold if there is a posible ability;
                            to lock the files.
=lfAddProp('llFirstTime', .T. )       && Variable to enter the FUNCTION open file if the;
                            the user work dirct without change the company.
=lfAddProp('llCopyStrut', .T. )       && Variable to Copy the files structure.
=lfAddProp('llDatChange', .F. )       && Flag if there is any change in the end date;
                           of the current year.
=lfAddProp('llPrdChange', .F. )       && Flag if there is any change in the Period.
=lfAddProp('llClosDate' , .F. )       && Flag to indicate if we are going to replace;
                            the batch end date with the closing date or not.
=lfAddProp('llCloseGL'  , .F. )       && Variable to indicate if the GL Module is installed.
=lfAddProp('llCloseAP'  , .F. )       && Variable to indicate if the AP Module is installed.

** Itializing the numeric variables **

=lfAddProp('lnAcsSegSz' , 0   )       && Variable to hold the Account Segment Size.
=lfAddProp('lnAcsNoSeg' , 0   )       && Variable to hold the Account No of Segments.
=lfAddProp('lnFrsSegSz' , 0   )       && Variable to hold the Account First Segment.
=lfAddProp('lnTotTrnDr' , 0   )       && This variable to make the summission of the;
                           total Debit of each Transaction.
=lfAddProp('lnTotTrnCr' , 0   )       && This variable to make the summission of the;
                           total Credit of each Transaction.
=lfAddProp('lnTotBatDr' , 0   )       && This variable to make the summission of the;
                           total Debit of each batch.
=lfAddProp('lnTotBatCr' , 0   )       && This variable to make the summission of the;
                           total Credit of each batch.
*=lfAddProp('lnOldComp'  , 0   )       && Variable to hold the array element of the Old;
                           Company ID.
=lfAddProp('lnTermCount', 0   )       && Variable to hold the thermo counter.

** Itializing the date variables **

=lfAddProp('ldFsppBgDt' , {}  )       && Variable to hold the transaction begin date.
=lfAddProp('ldFsppEnDt' , {}  )       && Variable to hold the transaction end date.
=lfAddProp('ldDatChange', {}  )       && Variable to hold if the date of the next year;
                           change.
=lfAddProp('ldTodayDate', oAriaApplication.SystemDate )  && Variable to hold todays date.
=lfAddProp('ldOldNxtDat', {}  )       && Variable to hold the end date of the old next;
                           year.

*!*	*E303343,1 TMI 01/15/2013 [Start] to be copied to the header file and commented out from here
*!*	lcTCloseU  = "Closing"
*!*	lcTOpen    = "open"
*!*	lcTLock    = "locked"
*!*	lcTUnPostd = "Unposted single transactions"
*!*	lcTUnGenAl = "Ungenerated allocation transactions"
*!*	lcTUnGenRc = "Ungenerated recurring transactions"
*!*	lcTCloseP  = "Closing period"
*!*	lcTCloseY  = "Closing year & period "
*!*	lcTColect  = "Collecting companies information...! "
*!*	lcTUnPostB = "Unposted batches"
*!*	lcTGenRevT = "Generating reverse transactions. "
*!*	lcTGenRevB = "Generating reverse batches. "
*!*	lcTUpdMast = "Updating master files. "
*!*	lcTFiscal  = "Fiscal year"
*!*	lcTAdjBal  = "Adjusting balance... "
*!*	lcTAdjPsYP = "Adjusting posted year & period... "
*!*	lcTBatHand = "Batch Handeling... "
*!*	lcTRclasfi = "Reclacified from the batch file. "
*!*	lcTRPostBt = "Repost batchs & transactions... "
*!*	lcTUpdBal  = "Update balances... "
*!*	lcTGnClsEn = "Generate closing entries... "
*!*	lcTCrClsEn = "Create closing entries... "
*!*	lcTYecNo   = "YEC No"
*!*	lcTPrior   = "Prior year closing entries by "
*!*	lcTClsEntn = "Closing entry No. "
*!*	lcTCompFil = "Update the company file... "
*!*	lcTCompAP  = "Upadte the AP files..."
*!*	lcTCmpChld = "Update the AP child companies files..."
*!*	lcTClsDone = "done"
*!*	*E303343,1 TMI 01/15/2013 [End  ]

** Checking for the closing PARAMETERS from the menu **
IF loFormSet.lcClose = 'P'
  ** The screen title is Closing period.
  *lcScrTitle' = LANG_SMCLOSE_lctCloseP
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.Ariaform1.Caption = LANG_SMCLOSE_lctCloseP
loFormSet.Ariaform1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctCloseP,loFormSet.GetHeaderText("LANG_SMCLOSE_lctCloseP",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ELSE
  ** The screen title is Closing year & period.
  *lcScrTitle' = LANG_SMCLOSE_lctCloseY
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.Ariaform1.Caption = LANG_SMCLOSE_lctCloseY
loFormSet.Ariaform1.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctCloseY,loFormSet.GetHeaderText("LANG_SMCLOSE_lctCloseY",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDIF

** MESSAGE : "We strongle recommend that a full system"
**           "backup is done immediatly               "
**           "prior to the closing processing.        "
** Choices : "       < Proceed >    ® Cancel ¯        "

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF gfModalGen("TRM00097B00012","ALERT",LANG_SMCLOSE_lctCloseL) = 2
IF gfModalGen("TRM00097B00012","ALERT",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctCloseL,loFormSet.GetHeaderText("LANG_SMCLOSE_lctCloseL",loFormSet.HeaderAlias))) = 2
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *glQuitting = .T.
  RETURN .F.
ENDIF

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT LANG_SMCLOSE_lcTColect WINDOW NOWAIT
WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTColect,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTColect",loFormSet.HeaderAlias)) WINDOW NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]


=lfAddProp('lcAccCod'     , 'ACCOD')
=lfAddProp('lcFisHd'      , 'FISHD')
=lfAddProp('lcFsPrd'      , 'FSPRD')
=lfAddProp('lcFsHld'      , 'FSHLD')

*- reset these 4 properties as variables in case if any is used below
lcAccCod = loFormSet.lcAccCod
lcFisHd = loFormSet.lcFisHd
lcFsPrd = loFormSet.lcFsPrd
lcFsHld = loFormSet.lcFsHld

WITH loFormSet
.laSFileOpn[1,1] = 'ACCOD'
.laSFileOpn[1,2] = '&lcAccCod'
.laSFileOpn[1,3] = 'ACCSEGNO'

.laSFileOpn[2,1] = 'FISHD'
.laSFileOpn[2,2] = '&lcFisHd'
.laSFileOpn[2,3] = 'COMPFYEAR'

.laSFileOpn[3,1] = 'FSPRD'
.laSFileOpn[3,2] = '&lcFsPrd'
.laSFileOpn[3,3] = 'COMFYRPRDI'

.laSFileOpn[4,1] = 'FSHLD'
.laSFileOpn[4,2] = '&lcFsHld'
.laSFileOpn[4,3] = 'COMFYRHDAT'
ENDWITH

=lfAddProp('lnCurr_Yer',0,.T.)
=lfAddProp('lcCurr_Yer','',.T.)

** Selecting all the company which have data in the GLSETUP file.
loFormSet.lcCompany = '  '

SELECT SYCCOMP
lnNoOfCmp = 0
DIME loFormSet.laCompany[1]
STORE ' ' TO loFormSet.laCompany
SCAN FOR !EMPTY(ALLTRIM(SYCCOMP.CCURR_YER))
  loFormSet.lcDataDir = gfGetDataDir(ALLTRIM(SYCCOMP.CCOM_DDIR))
  IF lfOpenSFils(loFormSet) .AND. SEEK(SYCCOMP.CCURR_YER,lcFisHd) .AND.;
     SEEK(SYCCOMP.CCURR_YER+SYCCOMP.CCURR_PRD,lcFsPrd)
     lnNoOfCmp = lnNoOfCmp + 1

     DIME loFormSet.laCompany[lnNoOfCmp,11]

     loFormSet.laCompany[lnNoOfCmp,1] = SYCCOMP.CCOMP_ID+' '+SYCCOMP.CCOM_NAME
     loFormSet.laCompany[lnNoOfCmp,2] = SYCCOMP.CCOMP_ID
     loFormSet.laCompany[lnNoOfCmp,3] = SYCCOMP.CCURR_PRD
     loFormSet.laCompany[lnNoOfCmp,4] = SYCCOMP.CCURR_YER
     loFormSet.laCompany[lnNoOfCmp,5] = &lcFisHd..CFISNOPRD
     loFormSet.laCompany[lnNoOfCmp,6] = &lcFisHd..DFISBGDAT
     loFormSet.laCompany[lnNoOfCmp,7] = &lcFisHd..DFISENDAT
     loFormSet.laCompany[lnNoOfCmp,8] = SYCCOMP.CCOM_DDIR
     loFormSet.laCompany[lnNoOfCmp,9] = SPACE(1)
     loFormSet.laCompany[lnNoOfCmp,10] = &lcFsPrd..DFSPPENDT

     loFormSet.laCompany[lnNoOfCmp,11] = &lcFsPrd..lFsPLockS

     IF SYCCOMP.CCOMP_ID == oAriaApplication.ActiveCompanyID
       loFormSet.lcCompany = SYCCOMP.CCOMP_ID
     ENDIF

  ENDIF
ENDSCAN

WAIT CLEAR

** Case there is no companys have the same account code structure.
IF EMPTY(loFormSet.laCompany)

  ** Message : "No companys available.  Either the company"
  **           "Fisical Period Not define or the General  "
  **           "Leadger Setup file is not found.          "
  ** Choice  : "                   ® Ok ¯                 "

  =gfModalGen("TRM00111B00000","Dialog")
  *glQuitting = .T.
  RETURN .F.

ELSE

   IF !EMPTY(oAriaApplication.prntcompanyid)
     FOR lnCount = 1 TO ALEN(loFormSet.laCompany,1)
       IF loFormSet.laCompany[lnCount,2] = oAriaApplication.prntcompanyid
         loFormSet.puCompany = lnCount
         EXIT
       ENDIF
     ENDFOR
   ELSE
     loFormSet.puCompany = 1
   ENDIF

  ** Assign the display variable of the popup in the screen by the;
     Active company.

  *loFormSet.lcCompany = ALLTRIM(loFormSet.laCompany[loFormSet.puCompany,1])

  ** Assign the begin date with default of the Active company.
  =lfAddProp('ldFsppBgDt' , IIF(loFormSet.puCompany=0,{},loFormSet.laCompany[loFormSet.puCompany,6]))

  ** Assign the period End date with default of the Active company.
  =lfAddProp('ldFsppEnDt' , IIF(loFormSet.puCompany=0,{},loFormSet.laCompany[loFormSet.puCompany,10]))

  ** Assign the data dir by the default of the Active company.
  =lfAddProp('lcDataDir' , oAriaApplication.DataDir)

  ** Create random names for the Temp files.
  =lfAddProp('lcTmpBatch'   , gfTempName() )&& Varible to hold the TEMP file Name for;
                                 Batch.
  =lfAddProp('lcTmpTrnHd'   , gfTempName() )&& Varible to hold the TEMP file Name for;
                                 Transaction Header.
  =lfAddProp('lcTmpTrnDt'   , gfTempName() )&& Varible to hold the TEMP file Name for;
                                 Transaction Detail.
  =lfAddProp('lcTmpFisHD'   , gfTempName() )&& Varible to hold the TEMP file Name for;
                                 Fiscal header.
  =lfAddProp('lc_TempPR'    , gfTempName() )&& Varible to hold the TEMP file Name for;
                                 Periods.
  =lfAddProp('lcPeriods'    , gfTempName() )&& Variable to hold name of period temp;
                                 cursor.
  =lfAddProp('lcTempBal'    , gfTempName() )&& Varriable to hold name of temp balance;
                                 file.
  =lfAddProp('lcTmpClsEnt'  , gfTempName() )&& Variable to holds the closing entries;
                                 generated during the closing session.
  =lfAddProp('lcTmpAcChar2' , gfTempName() )&& Alias name to be used when openning ;
                                 GLACCHAR file for the scond time.
  =lfAddProp('lcTmpReport'  , gfTempName() )&& Varible to hold the Cursor Name for;
                                 the closing report.

  ** Create random names for the company files which is going to be
  ** open by onther ALIAS name.

  =lfAddProp('lcAcChar' , gfTempName() )&& Variable to hold the GLACCHAR
  =lfAddProp('lcSetup'  , gfTempName() )&& Variable to hold the GLSETUP
  =lfAddProp('lcBatch'  , gfTempName() )&& Variable to hold the GLBATCH
  =lfAddProp('lcAutHd'  , gfTempName() )&& Variable to hold the GLAUTHD
  =lfAddProp('lcTrnsHd' , gfTempName() )&& Variable to hold the GLTRNSHD
  =lfAddProp('lcTrnsDt' , gfTempName() )&& Variable to hold the GLTRNSDT
  =lfAddProp('lcPTrnHd' , gfTempName() )&& Variable to hold the GLPTRNHD
  =lfAddProp('lcPTrnDt' , gfTempName() )&& Variable to hold the GLPTRNDT
  =lfAddProp('lcAcBals' , gfTempName() )&& Variable to hold the GLACBALS

ENDIF

** Array to hold information about the files to be opend
** File name, Alias name, Tag name

WITH loFormSet
.laFileOpen[1,1] = 'GLACCHAR'
*E303343,1 TMI 01/16/2013 [Start]
*.laFileOpen[1,2] = '&lcAcChar'
.laFileOpen[1,2] = loFormSet.lcAcChar
*E303343,1 TMI 01/16/2013 [End  ]
.laFileOpen[1,3] = 'ACCTCODE'

.laFileOpen[2,1] = 'GLSETUP'
*E303343,1 TMI 01/16/2013 [Start]
*.laFileOpen[2,2] = '&lcSetup'
.laFileOpen[2,2] = loFormSet.lcSetup
*E303343,1 TMI 01/16/2013 [End  ]
.laFileOpen[2,3] = ''

.laFileOpen[3,1] = 'GLBATCH'
*E303343,1 TMI 01/16/2013 [Start]
*.laFileOpen[3,2] = '&lcBatch'
.laFileOpen[3,2] = loFormSet.lcBatch
*E303343,1 TMI 01/16/2013 [End  ]
.laFileOpen[3,3] = 'BATSTAT'

.laFileOpen[4,1] = 'GLAUTHD'
*E303343,1 TMI 01/16/2013 [Start]
*.laFileOpen[4,2] = '&lcAutHd'
.laFileOpen[4,2] = loFormSet.lcAutHd
*E303343,1 TMI 01/16/2013 [End  ]
.laFileOpen[4,3] = 'TYPECODE'

.laFileOpen[5,1] = 'GLTRNSHD'
*E303343,1 TMI 01/16/2013 [Start]
*.laFileOpen[5,2] = '&lcTrnsHd'
.laFileOpen[5,2] = loFormSet.lcTrnsHd
*E303343,1 TMI 01/16/2013 [End  ]
.laFileOpen[5,3] = 'TRANSTAT'

.laFileOpen[6,1] = 'GLTRNSDT'
*E303343,1 TMI 01/16/2013 [Start]
*.laFileOpen[6,2] = '&lcTrnsDt'
.laFileOpen[6,2] = loFormSet.lcTrnsDt
*E303343,1 TMI 01/16/2013 [End  ]
.laFileOpen[6,3] = 'BATCHTRN'

.laFileOpen[7,1] = 'GLPTRNHD'
*E303343,1 TMI 01/16/2013 [Start]
*.laFileOpen[7,2] = '&lcPTrnHd'
.laFileOpen[7,2] = loFormSet.lcPTrnHd
*E303343,1 TMI 01/16/2013 [End  ]
.laFileOpen[7,3] = 'BATCHTRN'

.laFileOpen[8,1] = 'GLPTRNDT'
*E303343,1 TMI 01/16/2013 [Start]
*.laFileOpen[8,2] = '&lcPTrnDt'
.laFileOpen[8,2] = loFormSet.lcPTrnDt
*E303343,1 TMI 01/16/2013 [End  ]
.laFileOpen[8,3] = 'BATCHTRN'

.laFileOpen[9,1] = 'GLACBALS'
*E303343,1 TMI 01/16/2013 [Start]
*.laFileOpen[9,2] = '&lcAcBals'
.laFileOpen[9,2] = loFormSet.lcAcBals
*E303343,1 TMI 01/16/2013 [End  ]
.laFileOpen[9,3] = 'ACCYRPRD'
ENDWITH

*- End of lfDefineVars.
*DO (gcScrDir + gcWinAppl + '\SMCLOSE.SPR')
*glQuitting = .T.

*E303343,1 TMI 01/20/2013 [Start] open the files of the current company
SELECT syccomp
LOCATE FOR cComp_ID = oAriaApplication.ActiveCompanyID
loFormSet.lcDataDir = gfGetDataDir(ALLTRIM(SYCCOMP.CCOM_DDIR))
IF lfOpenSFils(loFormSet) .AND. SEEK(SYCCOMP.CCURR_YER,lcFisHd) .AND. SEEK(SYCCOMP.CCURR_YER+SYCCOMP.CCURR_PRD,lcFsPrd)
  ** // **
ELSE
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_SMCLOSE_Problems_opening_companyS_files)  &&"Problems opening company's files"
=gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_SMCLOSE_Problems_opening_companyS_files)  &&"Problems opening company's files"
*N000682,1 11/20/2012 MMT Globlization changes[End]

  RETURN .F.
ENDIF


*E303343,1 TMI 01/20/2013 [End  ]


************************************************************
*! Name      : lfAddProp
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/15/2013
*! Purpose   : Write a function that add a variable as a property to the formset, in the same time it adds it to a list of variables , this list will be defined
*            : as a two dimensions array to hold the variable name and its value
*            : Call a loop the defines list of variables from the array , and in the end of each function, define another function that updates any variable
*            : with its changes, consider the case of arrays
************************************************************
FUNCTION lfAddProp
PARAMETERS lcProp,lcVal,llDoNotAddToArray
LOCAL lnLen,llAdd
*- add the property
lcProp = UPPER(ALLTRIM(lcProp))
loFormSet.AddProperty(lcProp,lcVal)

llAdd = !llDoNotAddToArray
IF llAdd
  *- update the properties array laVars
  lnLen = ALEN(loFormSet.laVars,1)+1
  DIMENSION loFormSet.laVars[lnLen , 2]
  loFormSet.laVars[lnLen,1] = lcProp
  loFormSet.laVars[lnLen,2] = lcVal
ENDIF
*- End of lfAddProp.
************************************************************
*! Name      : lfGetVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/15/2013
*! Purpose   : Get the variables definition from the loformset.laVars array
** this block will be added as it is at the begining of each function in the program
************************************************************
FUNCTION lfGetVars
PARAMETERS loFormSet
LOCAL i
FOR i = 1 TO ALEN(loFormSet.laVars,1)
  lcVar = loFormSet.laVars[i,1]
  IF !'[' $ lcVar
    &lcVar = loFormSet.laVars[i,2]
  ELSE
    lcVar = SUBSTR(lcVar,AT('[',lcVar)-1)
    DIMENSION &lcVar.[ALEN(loFormSet.&lcVar,1),ALEN(loFormSet.&lcVar,2)]
    =ACOPY(loFormSet.&lcVar,&lcVar)
  ENDIF
ENDFOR
*- End of lfGetVars.
************************************************************
*! Name      : lfResetVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/15/2013
*! Purpose   : Reset variables values at the end of each function
************************************************************
FUNCTION lfResetVars
PARAMETERS loFormSet
LOCAL i
FOR i = 1 TO ALEN(loFormSet.laVars,1)
  lcVar = loFormSet.laVars[i,1]
  IF !'[' $ lcVar
    IF TYPE('loFormSet.&lcVar.')<>'U'
      loFormSet.&lcVar = &lcVar
      loFormSet.laVars[i,2] = &lcVar
    ENDIF
  ENDIF
*!*	    lcVar = SUBSTR(lcVar,AT('[',lcVar)-1)
*!*	    DIMENSION loFormSet.&lcVar.[ALEN(&lcVar,1),ALEN(&lcVar,2)]
*!*	    =ACOPY(&lcVar,loFormSet.&lcVar)
*!*	  ENDIF
ENDFOR
*- End of lfResetVars.


************************************************************
*! Name      : lfFormDestroy
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/15/2013
*! Purpose   : clean up files
************************************************************
FUNCTION lfFormDestroy
PARAMETERS loFormSet
** ERASE the Temp. file which hold the Batchs.
gcWorkDir = oAriaApplication.WorkDir
lcTmpBatch = loFormSet.lcTmpBatch
lcTmpTrnHd = loFormSet.lcTmpTrnHd
lcTmpTrnDt = loFormSet.lcTmpTrnDt
lcTmpFisHD = loFormSet.lcTmpFisHD
lcTmpReport = loFormSet.lcTmpReport
lc_TempPR = loFormSet.lc_TempPR
lc_TempPR = loFormSet.lc_TempPR
lcTmpClsEnt = loFormSet.lcTmpClsEnt

IF USED(lcTmpBatch)
  USE IN &lcTmpBatch
ENDIF
ERASE &gcWorkDir.&lcTmpBatch+'.DBF'
ERASE &gcWorkDir.&lcTmpBatch+'.CDX'
ERASE &gcWorkDir.&lcTmpBatch+'.FPT'

** ERASE the Temp. file which hold the Transactions Header.
IF USED(lcTmpTrnHd)
  USE IN &lcTmpTrnHd
ENDIF
ERASE &gcWorkDir.&lcTmpTrnHd+'.DBF'
ERASE &gcWorkDir.&lcTmpTrnHd+'.CDX'
ERASE &gcWorkDir.&lcTmpTrnHd+'.FPT'

** ERASE the Temp. file which hold the Transactions Details.
IF USED(lcTmpTrnDt)
  USE IN &lcTmpTrnDt
ENDIF
ERASE &gcWorkDir.&lcTmpTrnDt+'.DBF'
ERASE &gcWorkDir.&lcTmpTrnDt+'.CDX'
ERASE &gcWorkDir.&lcTmpTrnDt+'.FPT'

** Close the Temp file which hold the Fiscal Header.
IF USED(lcTmpFisHD)
  USE IN &lcTmpFisHD
ENDIF
ERASE &gcWorkDir.&lcTmpFisHD+'.DBF'
ERASE &gcWorkDir.&lcTmpFisHD+'.CDX'
ERASE &gcWorkDir.&lcTmpFisHD+'.FPT'

** Close the Temp file which hold the closing report.
IF USED(lcTmpReport)
  USE IN &lcTmpReport
ENDIF
ERASE &gcWorkDir.&lcTmpReport+'.DBF'

IF lcClose = 'Y'
  IF USED(lc_TempPR)
    USE IN &lc_TempPR
  ENDIF
  ERASE &gcWorkDir.&lc_TempPR+'.DBF'
  ERASE &gcWorkDir.&lc_TempPR+'.CDX'
  ERASE &gcWorkDir.&lc_TempPR+'.FPT'

  ** ERASE the Temp. file which hold the closing entries.
  IF USED(lcTmpClsEnt)
    USE IN &lcTmpClsEnt
  ENDIF
  ERASE &gcWorkDir.&lcTmpClsEnt+'.DBF'
  ERASE &gcWorkDir.&lcTmpClsEnt+'.FPT'
  ERASE &gcWorkDir.&lcTmpClsEnt+'.CDX'
ENDIF
*- End of lfFormDestroy.

*!*	*!**************************************************************************
*!*	*!
*!*	*!      Function:  lfwPopup
*!*	*!
*!*	*!**************************************************************************
*!*	*
*!*	FUNCTION lfwPopup

*!*	** This variable is to save the element of the array for not to open
*!*	** again the files of the selected company.
*!*	lnOldComp = puCompany
*!*	lcColor1  = SCHEME(1,6)
*!*	lcColor2  = SCHEME(1,2)

*!*	** Show get all hand made popups with the appropirate color. ***
*!*	SHOW GET ibPopCom COLOR ,,,,,&lcSelCont,,,&lcEnbCont,&lcDisCont

*!**************************************************************************
*!
*!      Function:  lfvPopup
*!
*!**************************************************************************
*
FUNCTION lfvPopup
PARAMETERS loFormSet,loFld

** If the old varaible is not equal the new one so we are going to
** check first if there is any processing done to this company befor
** or not if there is any, so we are going to have message indicates
** the type of processing which is done befor to this selected company.
** If not we are going to assigen the variables to this company.
o = loFormset.AriaForm1.laCompany
*E303343,1 TMI 01/15/2013 [Start]
*IF puCompany <> lnOldComp
IF o.Value <> o.OldValue
  *E303343,1 TMI 01/15/2013 [End  ]
  loFormSet.llCloseGL   = .F.   && Flag to indicate that the GL module is installed for this company.
  loFormSet.llCloseAP   = .F.   && Flag to indicate that the AP module is installed for this company.
  loFormSet.llCopyStrut = .T.
  ** If the company selected for the first time.
  loFormSet.llFirstTime = .F.

  ** Variable to hold the begining date of the current company.
  loFormSet.ldFsppBgdt  = loFormSet.laCompany[loFormSet.puCompany,6]

  ** Variable to hold the End date of the current company.
  loFormSet.ldFsppEndt  = loFormSet.laCompany[loFormSet.puCompany,10]

  ** Variable to hold the data directory of the current company.
  loFormSet.lcDataDir = oAriaApplication.DataDir

  ** Check if the GL Module exist for this company.
  =SEEK(loFormSet.laCompany[loFormSet.puCompany,2],'SYCCOMP')

  ** Check if the GL module is installed for this company.
  IF 'GL' $ SYCCOMP.MMODLSET
    loFormSet.llCloseGL = .T.  && Flag to indicate that the GL module is installed for this company.
    ** Function to check for the opening of the files.
    =lfOpenFiles(loFormSet)
  ENDIF

  ** Check if the AP module is installed for this company.
  IF 'AP' $ SYCCOMP.MMODLSET
    loFormSet.llCloseAP = .T.  && Flag to indicate that the AP module is installed for this company.
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function:  lfvClose
*!
*!**************************************************************************
*
FUNCTION lfvClose
PARAMETERS loFormSet

IF !EMPTY(loFormSet.lcCompany) AND !EMPTY(loFormSet.Ariaform1.laCompany.Value)
  IF loFormSet.laCompany[loFormSet.puCompany,9] = 'C'
    ** MESSAGE : "ð process is done."
    ** Choice  : "         ® Ok ¯         "
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00129B00000","Dialog",LANG_SMCLOSE_LCTCLOSEU+"|"+LANG_SMCLOSE_lctClsDone)
=gfModalGen("TRM00129B00000","Dialog",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_LCTCLOSEU,loFormSet.GetHeaderText("LANG_SMCLOSE_LCTCLOSEU",loFormSet.HeaderAlias))+"|"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctClsDone,loFormSet.GetHeaderText("LANG_SMCLOSE_lctClsDone",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  ELSE
    ** MESSAGE : "Closing process is canceled."
    ** Choice  : "          ® Ok ¯            "
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00103B00000","Dialog",LANG_SMCLOSE_lctCloseU)
=gfModalGen("TRM00103B00000","Dialog",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctCloseU,loFormSet.GetHeaderText("LANG_SMCLOSE_lctCloseU",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  ENDIF
ENDIF

** Close all open files.
FOR lnFCount = 1 TO ALEN(loFormSet.laFileOpen,1)
  IF USED(loFormSet.laFileOpen[lnFCount,2])
    USE IN (loFormSet.laFileOpen[lnFCount,2])
  ENDIF
ENDFOR

* Close fiscal calendar files
FOR lnFCount = 1 TO ALEN(loFormSet.laSFileOpn,1)
  IF USED(loFormSet.laSFileOpn[lnFCount,2])
    USE IN (loFormSet.laSFileOpn[lnFCount,2])
  ENDIF
ENDFOR
loFormSet.Release()

*!**************************************************************************
*!
*!      Function:  lfvProceed
*!
*!**************************************************************************
*
FUNCTION lfvProceed
PARAMETERS loFormSet
 
*- Do not proceed if no company is selected
IF EMPTY(loFormSet.lcCompany)
  RETURN
ENDIF

=lfOpenSFils(loFormSet)
SELECT (loFormSet.lcAccCod)

*- define the variables from the loFormSet.laVars array
LOCAL i
FOR i = 1 TO ALEN(loFormSet.laVars,1)
  lcVar  = loFormSet.laVars[i,1]
  IF !'[' $ lcVar    && do not include arrays in the redefinition process
    &lcVar = loFormSet.laVars[i,2]
  ENDIF
ENDFOR

*- define array to hold file structure
DIMENSION laFileStru[1]

*- define some other variables to be used within the function
gcWorkDir = oAriaApplication.WorkDir
oProgress = NULL

IF SEEK("")
  IF nAcsSegSz = 0
    ** MESSAGE : "You have to add the account code structure"
    **           "before entering this option !!!           "
    ** Choice  : "                   ® Ok ¯                 "

    =gfModalGen("TRM00227B00000","DIALOG")
    loFormSet.laCompany[loFormSet.puCompany,9] = 'F'
    RETURN
  ELSE
    ** Assign the Account Segment Size, No of Segments, Account Mask,
    ** Account Segment Discription and the Account First Segment Size.
    loFormSet.lnAcsSegSz = nAcsSegSz
    lnAcsNoSeg = nAcsNoSeg
    lcAcsMask  = "X"+SUBSTR(cAcsMask,2)
    lcAcsMask  = STRTRAN(lcAcsMask,'#','9')
    lcAcsegDes = cAcsegDes
    lnFrsSegSz = AT("-",CACSMASK)-1
  ENDIF
ENDIF

*E303343,1 TMI 01/21/2013 [Start] define loFormSet.lcAcsMask
loFormSet.lcAcsMask = lcAcsMask
*E303343,1 TMI 01/21/2013 [End ]

** Check if the GL Module exist for this company.
IF SEEK(loFormSet.laCompany[loFormSet.puCompany,2],'SYCCOMP')
  ** Check if the GL module is installed for this company.
  IF 'GL' $ SYCCOMP.MMODLSET
    llCloseGL = .T.  && Flag to indicate that the GL module is installed for this company.
  ENDIF
  ** Check if the AP module is installed for this company.
  IF 'AP' $ SYCCOMP.MMODLSET
    llCloseAP = .T.  && Flag to indicate that the AP module is installed for this company.
  ENDIF
ENDIF

** Checking about the company processing stat.
IF !EMPTY(loFormSet.laCompany[loFormSet.puCompany,9])
  DO CASE
    ** Closing processing is already done normaly.
    CASE loFormSet.laCompany[loFormSet.puCompany,9] = 'C'
      ** MESSAGE : "ð processing is done."
      ** Choice  : "        ® Ok ¯       "

      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00112B00000","Dialog",LANG_SMCLOSE_lctCloseU)
=gfModalGen("TRM00112B00000","Dialog",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctCloseU,loFormSet.GetHeaderText("LANG_SMCLOSE_lctCloseU",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]


    ** The company data files cannot be found during the Closing processing.
    CASE loFormSet.laCompany[loFormSet.puCompany,9] = 'F'
      ** MESSAGE : "The Company Files cannot be ð."
      ** Choice  : "           ® Ok ¯             "

      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00113B00000","Dialog",LANG_SMCLOSE_lctOpen)
=gfModalGen("TRM00113B00000","Dialog",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctOpen,loFormSet.GetHeaderText("LANG_SMCLOSE_lctOpen",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]


    ** The company files cannot be locked during the Closing processing.
    CASE loFormSet.laCompany[loFormSet.puCompany,9] = 'L'
      ** MESSAGE : "The Company Files cannot be ð."
      ** Choice  : "            ® Ok ¯            "

      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00113B00000","Dialog",LANG_SMCLOSE_lctLock)
=gfModalGen("TRM00113B00000","Dialog",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctLock,loFormSet.GetHeaderText("LANG_SMCLOSE_lctLock",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  ENDCASE
ELSE
  DO CASE
    CASE llCloseGL
      ** If its the first time selecting this company.
      IF llFirstTime
        llFirstTime = .F.
        ** Function to check for the opening of the files.
        IF !lfOpenFiles(loFormSet)
          RETURN
        ENDIF
      ENDIF

      ** Function to check for the retained earnings account.
      IF !lfRetEarAcc()
        RETURN
      ENDIF

      ** If this the first time creating this file.
      IF llCopyStrut
        llCopyStrut = .F.
        ** Creat temp batch file by coping the file structure to the temp file.
        IF !USED(lcTmpBatch)
          SELECT(lcBatch)
          COPY STRUCTURE TO &gcWorkDir.&lcTmpBatch CDX
          SELECT 0
          USE &gcWorkDir.&lcTmpBatch EXCLUSIVE ORDER TAG BATCHNO
        ELSE
          SELECT(lcTmpBatch)
          ZAP
        ENDIF

        IF !USED(lcTmpTrnHd)
          ** Creat temp posted transaction header file by coping the file;
             structure to the temp file.
          SELECT(lcPTrnHd)
          COPY STRUCTURE TO &gcWorkDir.&lcTmpTrnHd CDX

          SELECT 0
          USE &gcWorkDir.&lcTmpTrnHd EXCLUSIVE
          INDEX ON CSTANDARD+CTRNPYR+CTRNPPRD TAG STNPYRPRD
        ELSE
          SELECT(lcTmpTrnHd)
          ZAP
        ENDIF

        IF !USED(lcTmpTrnDt)
          ** Creat temp posted transaction details file by coping the file;
             structure to the temp file.
          SELECT(lcPTrnDt)
          COPY STRUCTURE TO &gcWorkDir.&lcTmpTrnDt CDX

          SELECT 0
          USE &gcWorkDir.&lcTmpTrnDt EXCLUSIVE
          INDEX ON CTRANNO TAG TRANSNO
        ELSE
          SELECT(lcTmpTrnDt)
          ZAP
        ENDIF

        IF !USED(lcTmpFisHD)
          ** Creat temp Fiscal Header file.
          SELECT (lcFisHd)
          SET ORDER TO TAG COMPFYEAR
          lcIndExp1= SYS(14,VAL(SYS(21)))
          lcTagExp1= ' TAG '+SYS(22) + IIF('DESC' $ SET('ORDER'),' DESC','')

          ** Add new field to the temp file.
          =AFIELDS(laFileStru)
          lnFileStru = ALEN(laFileStru,1)
          DIMENSION laFileStru[lnFileStru+1,18]
          laFileStru[lnFileStru+1,1] = 'CCURR_PRD'
          laFileStru[lnFileStru+1,2] = 'C'
          laFileStru[lnFileStru+1,3] = 2
          laFileStru[lnFileStru+1,4] = 0
          =lfArrayStru(@laFileStru,lnFileStru)

          ** Creat temp Fiscal Header file from the array.
          CREATE TABLE &gcWorkDir.&lcTmpFisHD FROM ARRAY laFileStru
          INDEX ON &lcIndExp1.&lcTagExp1
        ELSE
          SELECT(lcTmpFisHD)
          ZAP
        ENDIF

        IF !USED(lcTmpReport)
          CREATE TABLE &gcWorkDir.&lcTmpReport (CBATCHNO C(6),CTRANNO C(8),;
                 CDESCRIP C(50),COLDBATNO C(6),COLDTRNNO C(8))
        ELSE
          SELECT(lcTmpReport)
          ZAP
        ENDIF

        SELECT(lcBatch)
        SET RELATION TO

        SELECT(lcTrnsHd)
        SET RELATION TO

        SELECT(lcTrnsDt)
        SET RELATION TO

        SELECT(lcTmpBatch)
        SET RELATION TO

        SELECT(lcTmpTrnHd)
        SET RELATION TO

        SELECT(lcTmpTrnDt)
        SET RELATION TO

        SELECT(lcPTrnHD)
        SET RELATION TO

        SELECT(lcPTrnDt)
        SET RELATION TO

        SELECT(lcAcChar)
        SET RELATION TO
      ENDIF

      ** In this part we are going to check if the current period =
      ** no of period.
      ** If its equal we are going to give a message having 2 choices
      ** if the user select the first choice we are going to close
      ** the Period & Year.

      IF lcClose = 'P' .AND. loFormSet.laCompany[loFormSet.puCompany,3] = loFormSet.laCompany[loFormSet.puCompany,5]
        *B119546,1 ASH 09/02/2003 Change the message to be
        **MESSAGE: "'You are trying to close the last period in the current year. This action will close the whole fiscal year."

        **MESSAGE: "You are trying to close the last period in the  "
        **         "current year therefore you must close the period"
        **         "and the year ?                                  "
        **Choices: "            < Proceed >      ® Cancel ¯         "

        IF gfModalGen('TRM00098B00012','Dialog') = 2
          RETURN
        ELSE
          ** We are to close both the Year & Period.
          lcClose = 'Y'
        ENDIF
      ENDIF

      ** If the user want to close the current fisical year befor the
      ** last period.

      IF lcClose = 'Y' ;
        .AND. INT(VAL(loFormSet.laCompany[loFormSet.puCompany,3])) < INT(VAL(loFormSet.laCompany[loFormSet.puCompany,5]));
        .AND. loFormSet.laCompany[loFormSet.puCompany,4] <> STR(YEAR(loFormSet.laCompany[loFormSet.puCompany,10]),4)

        ** MESSAGE : "You are trying to close the current fisical year befor"
        **           "the last period. Since the closing date is not valid  "
        **           "to be the end date for the current fisical year.      "
        ** Choices : "               < Proceed >      ® Cancel ¯            "

        IF gfModalGen('TRM00456B00000','Dialog') = 1 && always 1
          RETURN
        ELSE
          ** Variable indicate that the closing date is invalid.
          lcClosStat  = 'I'
          ** Flag indicate that the closing date is changed.
          llDatChange = .F.
        ENDIF
      ENDIF

      IF lcClose = 'Y';
        .AND. INT(VAL(loFormSet.laCompany[loFormSet.puCompany,3])) < INT(VAL(loFormSet.laCompany[loFormSet.puCompany,5]));
        .AND. loFormSet.laCompany[loFormSet.puCompany,4] = STR(YEAR(loFormSet.laCompany[loFormSet.puCompany,10]),4)

        ** MESSAGE : "You are trying to close the current fisical year befor"
        **           "the last period. The current year end date will be    "
        **           "99/99/9999. All batches and transactions will be      "
        **           "reclassified to new batches and transactions according"
        **           "to the new defintion of the fisical year.             "
        ** Choices : "              < Proceed >      ® Cancel ¯             "

        IF gfModalGen('TRM00456B00000','Dialog') = 1 && always 1
          RETURN
        ELSE
          ** Variable indicate that the end date is valid.
          lcClosStat = 'V'
          ** Flag indicate that the end date is changed.
          llDatChange= .T.
        ENDIF
      ENDIF

      IF lcClose = 'Y' .AND. loFormSet.laCompany[loFormSet.puCompany,11]

        ** MESSAGE : "Period ð is locked. Cannot proceed with the closing"
        **           "process.                                           "
        ** Choices : "                        < Ok >                     " 
        =gfModalGen('TRM00278B00000' , 'Dialog' , ALLTRIM(loFormSet.laCompany[loFormSet.puCompany,3]))
        RETURN
      ENDIF    && End of IF
      *E303557,1 MMT 03/09/2015 Don't close period has unreleased transactions[T20150227.0001][Start]
      IF lcClose = 'P'
        *B611581,1 MMT 07/18/2018 Can Close period because of un-released GL entries and company is not linked to GL[T20180510.0019][Start]
        llLinkToGL = gfGetMemVar('M_LINK_GL',oAriaApplication.ActiveCompanyID) ='Y'
        llAPInstalled = .F.
        IF 'AP' $ oAriaApplication.CompanySetupModules
          =gfOpenTable('APSETUP','APSETUP')
          SELECT APSETUP
          =gfSeek('')
          LOCATE
          llAPInstalled = (APSETUP.CAPSGLLINK ='Y')
          =gfCloseTable('APSETUP')
        ENDIF 
        IF llLinkToGL 
        *B611581,1 MMT 07/18/2018 Can Close period because of un-released GL entries and company is not linked to GL[T20180510.0019][End]
          IF !USED('GLDISTCHECK')
          *E303959,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL [Start]
*!*	            USE (loFormSet.lcDataDir+"GLDIST") Shared again IN 0 ALIAS GLDISTCHECK ORDER GLDISTPO   && POSTED
             =gfOpenTable(oAriaApplication.DataDir+'GLDIST', oAriaApplication.DataDir+'GLDISTPO', 'SH','GLDISTCHECK')
          *E303959,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]
          ENDIF  
        *B611581,1 MMT 07/18/2018 Can Close period because of un-released GL entries and company is not linked to GL[T20180510.0019][Start]  
        ENDIF  
        IF llAPInstalled 
        *B611581,1 MMT 07/18/2018 Can Close period because of un-released GL entries and company is not linked to GL[T20180510.0019][End]
          IF !USED('APDISTCHECK')
            USE (loFormSet.lcDataDir+"APDIST") Shared again IN 0 ALIAS APDISTCHECK ORDER POST   && LAPDPOST
          ENDIF  
        *B611581,1 MMT 07/18/2018 Can Close period because of un-released GL entries and company is not linked to GL[T20180510.0019][Start]  
        ENDIF
        *B611581,1 MMT 07/18/2018 Can Close period because of un-released GL entries and company is not linked to GL[T20180510.0019][End]  
        llCheckAgain = .T.
        *B611581,1 MMT 07/18/2018 Can Close period because of un-released GL entries and company is not linked to GL[T20180510.0019][Start]  
        llCheckAgain = (llAPInstalled  OR llLinkToGL)
        *B611581,1 MMT 07/18/2018 Can Close period because of un-released GL entries and company is not linked to GL[T20180510.0019][End]  
        DO WHILE llCheckAgain
          llUnReleaseFound = .F.
          
          *B611581,1 MMT 07/18/2018 Can Close period because of un-released GL entries and company is not linked to GL[T20180510.0019][Start]  
          IF llLinkToGL
             IF !USED('GLDISTCHECK')
               =gfOpenTable(oAriaApplication.DataDir+'GLDIST', oAriaApplication.DataDir+'GLDISTPO', 'SH','GLDISTCHECK')
             ENDIF  
            *B611581,1 MMT 07/18/2018 Can Close period because of un-released GL entries and company is not linked to GL[T20180510.0019][END]  
            SELECT ('GLDISTCHECK')
            *E303959,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
*!*	          =SEEK(' ')
            =gfSEEK(' ')
            *E303959,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]
            LOCATE REST WHILE POSTED = ' ' FOR glperiod =ALLTRIM(loFormSet.laCompany[loFormSet.puCompany,3]) AND glfyear = ALLTRIM(loFormSet.laCompany[loFormSet.puCompany,4])
            IF FOUND()
              llUnReleaseFound = .T.
            ELSE
              *B611581,1 MMT 07/18/2018 Can Close period because of un-released GL entries and company is not linked to GL[T20180510.0019][Start]  
              IF llAPInstalled 
                IF !USED('APDISTCHECK')
                  USE (loFormSet.lcDataDir+"APDIST") Shared again IN 0 ALIAS APDISTCHECK ORDER POST   && LAPDPOST
                ENDIF 
                *B611581,1 MMT 07/18/2018 Can Close period because of un-released GL entries and company is not linked to GL[T20180510.0019][End]  
                SELECT APDISTCHECK 
                =SEEK(.F.)
                *B612064,1 MMT 02/19/2020 Fix bug of not excluding 1099 records in APDIST while checking unreleased entries[T20200217.0003][Start]
                *LOCATE REST WHILE LAPDPOST = .F. FOR cfisfyear = ALLTRIM(loFormSet.laCompany[loFormSet.puCompany,4]) AND cfspprdid = ALLTRIM(loFormSet.laCompany[loFormSet.puCompany,3])
                LOCATE REST WHILE LAPDPOST = .F. FOR CAPDACTID <>'B' AND cfisfyear = ALLTRIM(loFormSet.laCompany[loFormSet.puCompany,4]) AND cfspprdid = ALLTRIM(loFormSet.laCompany[loFormSet.puCompany,3])
                *B612064,1 MMT 02/19/2020 Fix bug of not excluding 1099 records in APDIST while checking unreleased entries[T20200217.0003][End]
                IF FOUND()
                  llUnReleaseFound = .T.
                ENDIF
              *B611581,1 MMT 07/18/2018 Can Close period because of un-released GL entries and company is not linked to GL[T20180510.0019][Start]  
              ENDIF
              *B611581,1 MMT 07/18/2018 Can Close period because of un-released GL entries and company is not linked to GL[T20180510.0019][End]  
            ENDIF
          *B611581,1 MMT 07/18/2018 Can Close period because of un-released GL entries and company is not linked to GL[T20180510.0019][Start]  
          ELSE
	        IF llAPInstalled  
              IF !USED('APDISTCHECK')
                USE (loFormSet.lcDataDir+"APDIST") Shared again IN 0 ALIAS APDISTCHECK ORDER POST   && LAPDPOST
              ENDIF 
	          SELECT APDISTCHECK 
	          =SEEK(.F.)
              *B612064,1 MMT 02/19/2020 Fix bug of not excluding 1099 records in APDIST while checking unreleased entries[T20200217.0003][Start]
	          *LOCATE REST WHILE LAPDPOST = .F. FOR cfisfyear = ALLTRIM(loFormSet.laCompany[loFormSet.puCompany,4]) AND cfspprdid = ALLTRIM(loFormSet.laCompany[loFormSet.puCompany,3])
	          LOCATE REST WHILE LAPDPOST = .F. FOR CAPDACTID <>'B' AND cfisfyear = ALLTRIM(loFormSet.laCompany[loFormSet.puCompany,4]) AND cfspprdid = ALLTRIM(loFormSet.laCompany[loFormSet.puCompany,3])
              *B612064,1 MMT 02/19/2020 Fix bug of not excluding 1099 records in APDIST while checking unreleased entries[T20200217.0003][End]	          
	          IF FOUND()
	            llUnReleaseFound = .T.
	          ENDIF
	        ENDIF
          ENDIF
          *B611581,1 MMT 07/18/2018 Can Close period because of un-released GL entries and company is not linked to GL[T20180510.0019][End]  
          
          IF llUnReleaseFound 
            IF gfModalGen('QRM00000B00012',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_UNRELEASEDTRANSACTIONS,loFormSet.GetHeaderText("LANG_UNRELEASEDTRANSACTIONS",loFormSet.HeaderAlias))) = 2
              llCheckAgain = .F.
              *B612064,1 MMT 02/19/2020 Fix bug of not excluding 1099 records in APDIST while checking unreleased entries[T20200217.0003][Start]
  			  IF llAPInstalled  
   			    IF USED('APDISTCHECK')
			      USE IN APDISTCHECK 	
			    ENDIF  
  			  ENDIF
			  IF llLinkToGL
			    IF USED('GLDISTCHECK')
			      SELECT ('GLDISTCHECK')
			      =gfCloseTable('GLDISTCHECK')
			    ENDIF  
			  ENDIF                
              *B612064,1 MMT 02/19/2020 Fix bug of not excluding 1099 records in APDIST while checking unreleased entries[T20200217.0003][End]
              RETURN 
            ELSE
              *B612064,1 MMT 02/19/2020 Fix bug of not excluding 1099 records in APDIST while checking unreleased entries[T20200217.0003][Start]
  			  IF llAPInstalled 
  			    IF USED('APDISTCHECK') 
  			      USE IN APDISTCHECK 	
			    ENDIF
  			  ENDIF
			  IF llLinkToGL
			    IF USED('GLDISTCHECK')
 			      SELECT ('GLDISTCHECK')
			      =gfCloseTable('GLDISTCHECK')
			    ENDIF  
			  ENDIF                
              *B612064,1 MMT 02/19/2020 Fix bug of not excluding 1099 records in APDIST while checking unreleased entries[T20200217.0003][End]

              loFormSet.Ariaform1.VISIBLE = .F.
              lcDataSessionNo = SET("Datasession") 
              =oAriaApplication.DoProgram('AWRSMGLREL','.T.',.F.,'SM')
              SET DATASESSION TO &lcDataSessionNo.
              loFormSet.Ariaform1.VISIBLE = .T.
            ENDIF
          ELSE
            llCheckAgain = .F.  
		  ENDIF
		ENDDO 
		
		*B611581,1 MMT 07/18/2018 Can Close period because of un-released GL entries and company is not linked to GL[T20180510.0019][Start]  
		IF llAPInstalled  
		*B611581,1 MMT 07/18/2018 Can Close period because of un-released GL entries and company is not linked to GL[T20180510.0019][End]  
  		  IF USED('APDISTCHECK')
            USE IN APDISTCHECK 	
          ENDIF  
        *B611581,1 MMT 07/18/2018 Can Close period because of un-released GL entries and company is not linked to GL[T20180510.0019][Start]  
        ENDIF
        *B611581,1 MMT 07/18/2018 Can Close period because of un-released GL entries and company is not linked to GL[T20180510.0019][End]  
        
        *E303959,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL [Start]	
*!*	        USE IN GLDISTCHECK
        *B611581,1 MMT 07/18/2018 Can Close period because of un-released GL entries and company is not linked to GL[T20180510.0019][Start]  
        IF llLinkToGL
        *B611581,1 MMT 07/18/2018 Can Close period because of un-released GL entries and company is not linked to GL[T20180510.0019][END]  
          IF USED('GLDISTCHECK')
            SELECT ('GLDISTCHECK')
            =gfCloseTable('GLDISTCHECK')
          ENDIF  
        *B611581,1 MMT 07/18/2018 Can Close period because of un-released GL entries and company is not linked to GL[T20180510.0019][Start]  
        ENDIF
        *B611581,1 MMT 07/18/2018 Can Close period because of un-released GL entries and company is not linked to GL[T20180510.0019][End]    
        *E303959,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL [End]
      ENDIF
      *E303557,1 MMT 03/09/2015 Don't close period has unreleased transactions[T20150227.0001][End]


      ** Check if there is any unposted batches.
      IF lfUnpBatch()
        ** Check if there is any unposted transactions.
        IF lfUnpSinTran()
          ** Check if there is any ungenerated allocations.
          IF lfUngenAllo()
            ** Check if there is any ungenrated recurrings.
            IF lfUngenRec()
              ** Build reverce transactions and update master files.
              IF lfUpdate()
                ** Closing year & period ***
                IF lcClose = 'Y'
                  ** If there is any problem in locking the file do not continue.
                  IF loFormSet.laCompany[loFormSet.puCompany,9] <> 'L'
                    ** Create new fiscal year & periods direct in the master files.

                    DO (oAriaApplication.applicationhome + 'SM\SMFISHD ')

                  ELSE
                    ** MESSAGE : "The Company Files cannot be ð."
                    ** Choice  : "            ® Ok ¯            "
                    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00113B00000","Dialog",LANG_SMCLOSE_lctLock)
=gfModalGen("TRM00113B00000","Dialog",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctLock,loFormSet.GetHeaderText("LANG_SMCLOSE_lctLock",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

                  ENDIF
 
                  ** If there is any problem in locking the file do not continue.
                  IF loFormSet.laCompany[loFormSet.puCompany,9] <> 'L'
                    ** Adjusting balnce file.
                    =lfAdjBalnc()
                  ELSE
                    ** MESSAGE : "The Company Files cannot be ð."
                    ** Choice  : "            ® Ok ¯            "
                    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00113B00000","Dialog",LANG_SMCLOSE_lctLock)
=gfModalGen("TRM00113B00000","Dialog",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctLock,loFormSet.GetHeaderText("LANG_SMCLOSE_lctLock",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

                  ENDIF

                  ** If there is any problem in locking the file do not continue.
                  IF loFormSet.laCompany[loFormSet.puCompany,9] <> 'L'
                    ** Calling this function if the end date of the curret
                    ** new year is changed or their is any modification in the
                    ** periods lenth.
                    IF llDatChange OR llPrdChange
                      ** Adjusting posted year and periods
                      =lfAdjPstYP()
                    ENDIF
                  ELSE
                    ** MESSAGE : "The Company Files cannot be ð."
                    ** Choice  : "            ® Ok ¯            "
                    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00113B00000","Dialog",LANG_SMCLOSE_lctLock)
=gfModalGen("TRM00113B00000","Dialog",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctLock,loFormSet.GetHeaderText("LANG_SMCLOSE_lctLock",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

                  ENDIF

                  ** If there is any problem in locking the file do not continue.
                  IF loFormSet.laCompany[loFormSet.puCompany,9] <> 'L'
                    ** Do not call this function if the closing stat is normal
                    ** and the end date of the new current year is greater than
                    ** old date of the current year.
                    IF lcClosStat <> 'N' AND ldDatChange < loFormSet.laCompany[loFormSet.puCompany,7]
                      ** Handel the over lap batches
                      =lfBatHandl()
                    ENDIF
                  ELSE
                    ** MESSAGE : "The Company Files cannot be ð."
                    ** Choice  : "            ® Ok ¯            "
                    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00113B00000","Dialog",LANG_SMCLOSE_lctLock)
=gfModalGen("TRM00113B00000","Dialog",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctLock,loFormSet.GetHeaderText("LANG_SMCLOSE_lctLock",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

                  ENDIF

                  ** If there is any problem in locking the file do not continue.
                  IF loFormSet.laCompany[loFormSet.puCompany,9] <> 'L'
                    DO CASE
                      ** Call the repost batchs & transaction if the close stat
                      ** is valid.
                      CASE lcClosStat = 'V'
                        ** Repost batchs & transactions
                        =lfRpstBtTr(loFormSet.laCompany[loFormSet.puCompany,10])

                        ** Call the repost batchs & transaction if there is any
                        ** change in the periods or the end date of the new current
                        ** year is less than the old current year.
                      CASE llPrdChange OR ldDatChange < loFormSet.laCompany[loFormSet.puCompany,7]
                        ** Repost batchs & transactions
                        =lfRpstBtTr(ldDatChange)
                    ENDCASE
                  ELSE
                    ** MESSAGE : "The Company Files cannot be ð."
                    ** Choice  : "            ® Ok ¯            "
                    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00113B00000","Dialog",LANG_SMCLOSE_lctLock)
=gfModalGen("TRM00113B00000","Dialog",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctLock,loFormSet.GetHeaderText("LANG_SMCLOSE_lctLock",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

                  ENDIF

                  ** If there is any problem in locking the file do not continue.
                  IF loFormSet.laCompany[loFormSet.puCompany,9] <> 'L'
                    ** Generate closing entries
                    =lfGpstClsEn()
                  ELSE
                    ** MESSAGE : "The Company Files cannot be ð."
                    ** Choice  : "            ® Ok ¯            "
                    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00113B00000","Dialog",LANG_SMCLOSE_lctLock)
=gfModalGen("TRM00113B00000","Dialog",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctLock,loFormSet.GetHeaderText("LANG_SMCLOSE_lctLock",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

                  ENDIF

                  ** If there is any problem in locking the file do not continue.
                  IF loFormSet.laCompany[loFormSet.puCompany,9] <> 'L'
                    ** Update the company file
                    =lfUpdComp()
                  ELSE
                    ** MESSAGE : "The Company Files cannot be ð."
                    ** Choice  : "            ® Ok ¯            "
                    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00113B00000","Dialog",LANG_SMCLOSE_lctLock)
=gfModalGen("TRM00113B00000","Dialog",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctLock,loFormSet.GetHeaderText("LANG_SMCLOSE_lctLock",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF

    OTHERWISE
      IF llFirstTime
        llFirstTime = .F.
      ENDIF

      ** If this the first time creating this file.
      IF llCopyStrut
        llCopyStrut = .F.
        IF !USED(lcTmpFisHD)
          ** Creat temp Fiscal Header file.
          SELECT (lcFisHd)
          SET ORDER TO TAG COMPFYEAR
          lcIndExp1= SYS(14,VAL(SYS(21)))
          lcTagExp1= ' TAG '+SYS(22) + IIF('DESC' $ SET('ORDER'),' DESC','')

          ** Add new field to the temp file.
          =AFIELDS(laFileStru)
          lnFileStru = ALEN(laFileStru,1)
          *B610489,1 TMI 09/01/2013 [Start] make the array 18 columns
          *DIMENSION laFileStru[lnFileStru+1,4]
          DIMENSION laFileStru[lnFileStru+1,18]
          *B610489,1 TMI 09/01/2013 [End  ] 
          laFileStru[lnFileStru+1,1] = 'CCURR_PRD'
          laFileStru[lnFileStru+1,2] = 'C'
          laFileStru[lnFileStru+1,3] = 2
          laFileStru[lnFileStru+1,4] = 0
          =lfArrayStru(@laFileStru,lnFileStru)

          ** Creat temp Fiscal Header file from the array.
          CREATE TABLE &gcWorkDir.&lcTmpFisHD FROM ARRAY laFileStru
          INDEX ON &lcIndExp1.&lcTagExp1
        ELSE
          SELECT(lcTmpFisHD)
          ZAP
        ENDIF
      ENDIF

      ** In this part we are going to check if the current period =
      ** no of period.
      ** If its equal we are going to give a message having 2 choices
      ** if the user select the first choice we are going to close
      ** the Period & Year.

      IF lcClose = 'P' .AND. loFormSet.laCompany[loFormSet.puCompany,3] = loFormSet.laCompany[loFormSet.puCompany,5]
        *B119546,1 ASH 09/02/2003 Change the message to be
        **MESSAGE: "'You are trying to close the last period in the current year. This action will close the whole fiscal year."

        **MESSAGE: "You are trying to close the last period in the  "
        **         "current year therefore you must close the period"
        **         "and the year ?                                  "
        **Choices: "            < Proceed >      ® Cancel ¯         "

        IF gfModalGen('TRM00098B00012','Dialog') = 2
          RETURN
        ELSE
          ** We are to close both the Year & Period.
          lcClose = 'Y'
        ENDIF
      ENDIF

      ** If the user want to close the current fisical year befor the
      ** last period.

      IF lcClose = 'Y' ;
        .AND. INT(VAL(loFormSet.laCompany[loFormSet.puCompany,3])) < INT(VAL(loFormSet.laCompany[loFormSet.puCompany,5]));
        .AND. loFormSet.laCompany[loFormSet.puCompany,4] <> STR(YEAR(loFormSet.laCompany[loFormSet.puCompany,10]),4)

        ** MESSAGE : "You are trying to close the current fisical year befor"
        **           "the last period. Since the closing date is not valid  "
        **           "to be the end date for the current fisical year.      "
        ** Choices : "               < Proceed >      ® Cancel ¯            "

        IF gfModalGen('TRM00456B00000','Dialog') = 1 && always 1
          RETURN
        ELSE
          ** Variable indicate that the closing date is invalid.
          lcClosStat  = 'I'
          ** Flag indicate that the closing date is changed.
          llDatChange = .F.
        ENDIF
      ENDIF

      IF lcClose = 'Y';
        .AND. INT(VAL(loFormSet.laCompany[loFormSet.puCompany,3])) < INT(VAL(loFormSet.laCompany[loFormSet.puCompany,5]));
        .AND. loFormSet.laCompany[loFormSet.puCompany,4] = STR(YEAR(loFormSet.laCompany[loFormSet.puCompany,10]),4)

        ** MESSAGE : "You are trying to close the current fisical year befor"
        **           "the last period. The current year end date will be    "
        **           "99/99/9999. All batches and transactions will be      "
        **           "reclassified to new batches and transactions according"
        **           "to the new defintion of the fisical year.             "
        ** Choices : "              < Proceed >      ® Cancel ¯             "

        IF gfModalGen('TRM00456B00000','Dialog') = 1 && always 1
          RETURN
        ELSE
          ** Variable indicate that the end date is valid.
          lcClosStat = 'V'
          ** Flag indicate that the end date is changed.
          llDatChange= .T.
        ENDIF
      ENDIF


      IF lcClose = 'Y' .AND. loFormSet.laCompany[loFormSet.puCompany,11]

        ** MESSAGE : "Period ð is locked. Cannot proceed with the closing"
        **           "process.                                           "
        ** Choices : "                        < Ok >                     "
        =gfModalGen('TRM00278B00000' , 'Dialog' , ALLTRIM(loFormSet.laCompany[loFormSet.puCompany,3]))
        RETURN
      ENDIF    && End of IF

      ** Closing year & period ***
      IF lcClose = 'Y'
        ** If there is any problem in locking the file do not continue.
        IF loFormSet.laCompany[loFormSet.puCompany,9] <> 'L'
          ** Create new fiscal year & periods direct in the master files.

          DO (oAriaApplication.applicationhome + 'SM\SMFISHD ')
        ELSE
          ** MESSAGE : "The Company Files cannot be ð."
          ** Choice  : "            ® Ok ¯            "
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00113B00000","Dialog",LANG_SMCLOSE_lctLock)
=gfModalGen("TRM00113B00000","Dialog",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctLock,loFormSet.GetHeaderText("LANG_SMCLOSE_lctLock",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

        ENDIF
        ** If there is any problem in locking the file do not continue.
        IF loFormSet.laCompany[loFormSet.puCompany,9] <> 'L'
          ** Update the company file
          =lfUpdComp()
        ELSE
          ** MESSAGE : "The Company Files cannot be ð."
          ** Choice  : "            ® Ok ¯            "
          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00113B00000","Dialog",LANG_SMCLOSE_lctLock)
=gfModalGen("TRM00113B00000","Dialog",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctLock,loFormSet.GetHeaderText("LANG_SMCLOSE_lctLock",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

        ENDIF
      ENDIF
      IF !llCloseGL .AND. lcClose = 'P'
        =lfApUpdate()
      ENDIF

  ENDCASE
ENDIF

*- Reset properties values
=lfResetVars(loFormSet)
loFormSet.Ariaform1.Refresh()

RETURN
*- End of lfvProceed.

************************************************************
*! Name      : lfArrayStru
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/15/2013
*! Purpose   : make the array structure compatible with the VFP9
************************************************************
FUNCTION lfArrayStru
PARAMETERS laFileStru,lnOrgLen
LOCAL i
FOR i= lnOrgLen+1 TO ALEN(laFileStru,1)
  STORE .F. TO laFileStru[i,5],laFileStru[i,6]
  STORE '' TO laFileStru[i,7],laFileStru[i,8],laFileStru[i,9],laFileStru[i,10],laFileStru[i,11],laFileStru[i,12],laFileStru[i,13],laFileStru[i,14],laFileStru[i,15],laFileStru[i,16]
  STORE 0 TO laFileStru[i,17],laFileStru[i,18]
ENDFOR


*- End of lfArrayStru.

*!**************************************************************************
*!
*!      Function:  lfOpenFiles
*!
*!**************************************************************************
*
** This function is to Open files & set order

FUNCTION lfOpenFiles
PARAMETERS loFormSet

** Flag indicate that the files of the selected company is opened.
llOpenFile = .T.

FOR lnCount = 1 TO ALEN(loFormSet.laFileOpen,1)
  IF FILE(loFormSet.lcDataDir+loFormSet.laFileOpen[lnCount,1]+'.DBF')
    IF USED(loFormSet.laFileOpen[lnCount,2])
      SELECT(loFormSet.laFileOpen[lnCount,2])
      USE
    ELSE
      SELECT 0
    ENDIF
    USE (loFormSet.lcDataDir+loFormSet.laFileOpen[lnCount,1]) AGAIN ALIAS(loFormSet.laFileOpen[lnCount,2])
    SET ORDER TO TAG (loFormSet.laFileOpen[lnCount,3])
  ELSE
    ** MESSAGE : "File ð not found in the directory of company ð."
    ** Choices : "                    ® Ok ¯                     "

*N000682,1 04/17/2013 RAS Globalization[START]
*!*	    =gfModalGen("TRM00110B00000","ALERT",'File '+loFormSet.laFileOpen[lnCount,1]+'|'+ALLTRIM(loFormSet.laCompany[loFormSet.puCompany,1])+'|'+LANG_SMCLOSE_lctCloseL)
 *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00110B00000","ALERT",LANG_File +loFormSet.laFileOpen[lnCount,1]+'|'+ALLTRIM(loFormSet.laCompany[loFormSet.puCompany,1])+'|'+LANG_SMCLOSE_lctCloseL)
=gfModalGen("TRM00110B00000","ALERT",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_File,loFormSet.GetHeaderText("LANG_File",loFormSet.HeaderAlias)) +loFormSet.laFileOpen[lnCount,1]+'|'+ALLTRIM(loFormSet.laCompany[loFormSet.puCompany,1])+'|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctCloseL,loFormSet.GetHeaderText("LANG_SMCLOSE_lctCloseL",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/17/2013 RAS Globalization[End  ]
    loFormSet.laCompany[loFormSet.puCompany,9] = 'F'
    llOpenFile = .F.
    EXIT
  ENDIF
ENDFOR

RETURN llOpenFile

*!**************************************************************************
*!
*!      Function:  lfRetEarAcc
*!
*!**************************************************************************
*
** Check that retained earnings account in the (GLSETUP.cSetRetmj)
** is setup or not and also found in the accounts file (GLACCHAR.DBF).

FUNCTION lfRetEarAcc

SELECT(lcSetup)
*B612703,1 MMT 03/31/2024 Fix error while closing period if there is no GL setup[T-ERP-20230927.0002][Start]
lcDfRetAct = ''
LOCATE 
IF !EOF() 
*B612703,1 MMT 03/31/2024 Fix error while closing period if there is no GL setup[T-ERP-20230927.0002][End]
GO 1

lcDfRetAct = ALLTRIM(&lcSetup..cSetRetMj)
*B612703,1 MMT 03/31/2024 Fix error while closing period if there is no GL setup[T-ERP-20230927.0002][Start]
ENDIF
*B612703,1 MMT 03/31/2024 Fix error while closing period if there is no GL setup[T-ERP-20230927.0002][End]
IF !EMPTY(lcDfRetAct)

  ** Prepare the default retained earnings account code according to the
  ** account code mask (lcAcsMask) as well as the field content.

  lcDfRetAct=lcDfRetAct+;
             STRTRAN(SUBSTR(ALLTRIM(loFormSet.lcAcsMask),LEN(lcDfRetAct)+1),'9','0')+;
             REPLICATE(' ',FSIZE('cAcctCode','&lcAcChar')-loFormSet.lnAcsSegSz)

  ** Seeking for the default retained earning account code into the chart
  ** of accounts file (GLACCHAR.DBF).

  SELECT(lcAcChar)

  IF !SEEK(lcDfRetAct)

    ** IF the retained earnings account code is not into the chart of
    ** Accounts file, present the following message.

    ** Message : "The default retained earnings account does not"
    **           "exist in the chart of accounts file.          "
    **           "You have to add the default retained earnings "
    **           "account before ð.                             "
    ** Choice  : "                      ® OK ¯                  "

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00228B00000','Dialog',LANG_SMCLOSE_lctCloseL)
    *B610674,1 TMI 02/07/2014 18:46 [Start] show this message only when the company is linked to GL
    IF gfGetMemVar('M_LINK_GL') == 'Y'
      *B610674,1 TMI 02/07/2014 18:46 [End  ] 
      =gfModalGen('TRM00228B00000','Dialog',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctCloseL,loFormSet.GetHeaderText("LANG_SMCLOSE_lctCloseL",loFormSet.HeaderAlias)))
      *N000682,1 11/20/2012 MMT Globlization changes[End]
      loFormSet.laCompany[loFormSet.puCompany,9] = 'F'
      RETURN .F.
      *B610674,1 TMI 02/07/2014 18:55 [Start] 
    ENDIF 
    *B610674,1 TMI 02/07/2014 18:55 [End  ] 
  ENDIF
ELSE

  ** If the field is empty, present the following message, and return 0

  ** Message : "You have to setup the ð account major "
  **           "in the GL setup before ð.             "
  ** Choice  : "                ® OK ¯                "
  *N000682,1 04/17/2013 RAS Globalization[START]
*!*	  lcMsgTxt = 'retained earnings'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcMsgTxt = LANG_retained_earnings
lcMsgTxt = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_retained_earnings,loFormSet.GetHeaderText("LANG_retained_earnings",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/17/2013 RAS Globalization[End  ]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00229B00000','Dialog',lcMsgTxt+"|"+LANG_SMCLOSE_lctCloseL)
  *B610674,1 TMI 02/07/2014 18:46 [Start] show this message only when the company is linked to GL
  IF gfGetMemVar('M_LINK_GL') == 'Y'
    *B610674,1 TMI 02/07/2014 18:46 [End  ] 
    =gfModalGen('TRM00229B00000','Dialog',lcMsgTxt+"|"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctCloseL,loFormSet.GetHeaderText("LANG_SMCLOSE_lctCloseL",loFormSet.HeaderAlias)))
    *N000682,1 11/20/2012 MMT Globlization changes[End]
  
    loFormSet.laCompany[loFormSet.puCompany,9] = 'F'
    RETURN .F.
    *B610674,1 TMI 02/07/2014 18:55 [Start] 
  ENDIF 
  *B610674,1 TMI 02/07/2014 18:55 [End  ] 
ENDIF

** Check that the suspence account (GLSETUP.cSetRetMj)
** is setup in the GlSetup file (GLSETUP.DBF), and found
** in the chart of accounts file (GLACCHAR.DBF)

lcSusActMj = ALLTRIM(&lcSetup..cSetSusMj)

** If the field is setuped.
IF !EMPTY(lcSusActMj)

  ** Prepare the suspence account code according to the account code
  ** mask (lcAcsMask) as well as the field content.

  lcSusActMj=lcSusActMj +;
             STRTRAN(SUBSTR(ALLTRIM(loFormSet.lcAcsMask),LEN(lcSusActMj)+1),'9','0')+;
             REPLICATE(' ',FSIZE('cAcctCode','&lcAcChar')-loFormSet.lnAcsSegSz)

  ** Look for the suspence account code into the chart
  ** of accounts file (GLACCHAR.DBF)

  SELECT(lcAcChar)
  IF !SEEK(lcSusActMj)

    ** If the suspence account code is not found in the chart of accounts
    ** file (GLACCHAR), present the following message and return 0.

    ** Message : "The default suspence account does not exist in the"
    **           "chart of accounts file. "
    **           "You have to add the default suspence "
    **           "account before ð.                    "
    ** Choice  : "                ® OK ¯               "

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00230B00000','Dialog',LANG_SMCLOSE_lctCloseL)
    *B610674,1 TMI 02/07/2014 18:46 [Start] show this message only when the company is linked to GL
    IF gfGetMemVar('M_LINK_GL') == 'Y'
      *B610674,1 TMI 02/07/2014 18:46 [End  ] 
      =gfModalGen('TRM00230B00000','Dialog',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctCloseL,loFormSet.GetHeaderText("LANG_SMCLOSE_lctCloseL",loFormSet.HeaderAlias)))
      *N000682,1 11/20/2012 MMT Globlization changes[End]
  
      loFormSet.laCompany[loFormSet.puCompany,9] = 'F'
      RETURN .F.
      *B610674,1 TMI 02/07/2014 18:55 [Start] 
    ENDIF 
    *B610674,1 TMI 02/07/2014 18:55 [End  ] 
  ENDIF
ELSE
  ** If the field is empty, present the following message, and return 0

  ** Message : "You have to setup the ð major in the "
  **           "GL setup before ð.                   "
  **           "                ® OK ¯               "
  *N000682,1 04/17/2013 RAS Globalization[START]
*!*	   lcMsgTxt = 'suspence'
   *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcMsgTxt = LANG_suspence
lcMsgTxt = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_suspence,loFormSet.GetHeaderText("LANG_suspence",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/17/2013 RAS Globalization[End  ]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('TRM00229B00000','Dialog',lcMsgTxt+"|"+LANG_SMCLOSE_lctCloseL)
    *B610674,1 TMI 02/07/2014 18:46 [Start] show this message only when the company is linked to GL
    IF gfGetMemVar('M_LINK_GL') == 'Y'
      *B610674,1 TMI 02/07/2014 18:46 [End  ] 
=gfModalGen('TRM00229B00000','Dialog',lcMsgTxt+"|"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctCloseL,loFormSet.GetHeaderText("LANG_SMCLOSE_lctCloseL",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  loFormSet.laCompany[loFormSet.puCompany,9] = 'F'
  RETURN .F.
  *B610674,1 TMI 02/07/2014 18:55 [Start] 
  ENDIF 
  *B610674,1 TMI 02/07/2014 18:55 [End  ] 
ENDIF

*!**************************************************************************
*!
*!      Function:  lfUnpBatch
*!
*!**************************************************************************
*
FUNCTION lfUnpBatch

** Check for the Unposted Batches.
lnChoice    = 1    && Variable to hold the Choice.
lnDlgOpTion = 1
llUnpBatch  = .T.  && Varible to Stop the Counter.

SELECT(lcBatch)

SCAN FOR &lcBatch..cBatStat $"AEHOU";
    .AND. (BETWEEN(&lcBatch..dBatPBeg,ldFsppBgDt,ldFsppEnDt);
    .OR.  BETWEEN(&lcBatch..dBatPEnd,ldFsppBgDt,ldFsppEnDt))

  ** MESSAGE : "The closing period has ð. You may cancel the period"
  **           "closing, display or print the unposted batches, or "
  **           "resume with the period closing.                    "
  ** Choices : "   ® Resume ¯  < Display >  < Print >  ® Cancel ¯  "
  lnChoice   = gfModalGen('QRM00101B00021','Dialog',;
LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctUnpostb,loFormSet.GetHeaderText("LANG_SMCLOSE_lctUnpostb",loFormSet.HeaderAlias)))+'|'+LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctUnpostb,loFormSet.GetHeaderText("LANG_SMCLOSE_lctUnpostb",loFormSet.HeaderAlias))),"lfvUnpBatch()")
*N000682,1 11/20/2012 MMT Globlization changes[End]

  lnDlgOpTion = lnChoice
  =IIF( lnChoice = 2 , lfvUnpBatch() , '' )
  llUnpBatch = lnChoice = 1
  loFormSet.laCompany[loFormSet.puCompany,9] = IIF(!llUnpBatch,'F',' ')
  EXIT
ENDSCAN

RETURN llUnpBatch

*!**************************************************************************
*!
*!      Function:  lfvUnpBatch
*!
*!**************************************************************************
FUNCTION lfvUnpBatch

DO CASE
  ** Display option.
  CASE lnDlgOpTion = 2

    lcTempBatch = gfTempName()

    SELECT *;
      FROM &lcBatch;
     WHERE cbatstat IN ('U','O','H','E','A');
       AND (BETWEEN(dbatpbeg,ldFsppBgDt,ldFsppEnDt);
        OR BETWEEN(dbatpend,ldFsppBgDt,ldFsppEnDt));
     ORDER BY CBATCHNO;
      INTO CURSOR &lcTempBatch

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFile_Ttl = LANG_SMCLOSE_lctUnpostb
lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctUnpostb,loFormSet.GetHeaderText("LANG_SMCLOSE_lctUnpostb",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    lcBrFields = gfDbfField('GLBATCH')

    SELECT(lcTempBatch)
    DIMENSION laTemp[1]
    =gfBrows(.F.,"cbatchno",'laTemp',lcFile_Ttl,.F.)

    USE IN &lcTempBatch

  ** Resume or cancel.
  OTHERWISE
    CLEAR READ
ENDCASE

*!**************************************************************************
*!
*!      Function:  lfUnpSinTran
*!
*!**************************************************************************
*
FUNCTION lfUnpSinTran

** Check for Unposted Single Transaction.

lnChoice    = 1    && Variable to hold the Choice.
lnDlgOpTion = 1
llUnpSinTrn = .T.  && Variable to stop the Counter.

SELECT(lcTrnsHd)

SCAN FOR  cTrnStat $ "AEHOU";
    .AND. cBatchNo = '000000';
    .AND. BETWEEN(dTrnPDate,ldFsppBgDt,ldFsppEnDt)

  ** MESSAGE : "The closing period has ð. You may cancel the period"
  **           "closing, display or print the Unposted Single      "
  **           "Transaction, or resume with the period closing.    "
  ** Choices : "   ® Resume ¯  < Display >  < Print >  ® Cancel ¯  "
  lnChoice = gfModalGen('QRM00101B00021','Dialog',;
LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctUnpostd,loFormSet.GetHeaderText("LANG_SMCLOSE_lctUnpostd",loFormSet.HeaderAlias)))+'|'+LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctUnpostd,loFormSet.GetHeaderText("LANG_SMCLOSE_lctUnpostd",loFormSet.HeaderAlias))),"lfvUnpSTrn()")
*N000682,1 11/20/2012 MMT Globlization changes[End]

  lnDlgOpTion = lnChoice
  =IIF( lnChoice = 2 , lfvUnpSTrn() , '' )
  llUnpSinTrn = lnChoice = 1
  loFormSet.laCompany[loFormSet.puCompany,9] = IIF(!llUnpSinTrn,'F',' ')
  EXIT
ENDSCAN

RETURN llUnpSinTrn

*!**************************************************************************
*!
*!      Function:  lfvUnpSTrn
*!
*!**************************************************************************
*
FUNCTION lfvUnpSTrn

DO CASE
  ** Display option.
  CASE lnDlgOpTion = 2

    lcTempTrn = gfTempName()

    SELECT *;
      FROM &lcTrnsHd;
     WHERE CBATCHNO = "000000";
       AND ctrnstat IN ('U','O','H','E','A');
       AND BETWEEN(dtrnpdate,ldFsppBgDt,ldFsppEnDt);
     ORDER BY CBATCHNO,CTRANNO;
      INTO CURSOR &lcTempTrn

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFile_Ttl = LANG_SMCLOSE_lctUnpostd
lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctUnpostd,loFormSet.GetHeaderText("LANG_SMCLOSE_lctUnpostd",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    lcBrFields = gfDbfField('GLTRNSHD')
    SELECT(lcTempTrn)

    DIMENSION laTemp[1]
    =gfBrows(.F.,"cbatchno",'laTemp',lcFile_Ttl,.F.)

    USE IN &lcTempTrn

  ** Resume or cancel.
  OTHERWISE

ENDCASE

*!**************************************************************************
*!
*!      Function:  lfUngenAllo
*!
*!**************************************************************************
*
FUNCTION lfUngenAllo

** Check for Ungenerated Allocation Transactions.

** This flag to allow me to exit whenever I found an ungenerated allocation.
lnChoice    = 1    && Variable to hold the Choice.
lnDlgOpTion = 1
llAllo_Flag = .T.  && Variable to stop the Counter.

********* Ungenerated Allocation Transactions *********
SELECT(lcAutHd)

IF SEEK('A')
  SCAN REST WHILE cAutType  = 'A';
              FOR NAUTFRQNO = 0

    ** MESSAGE : "The closing period has ð. You may cancel the period"
    **           "closing, display or print the ungenerated          "
    **           "Allocations Transactions, or resume with the period"
    **           "closing.                                           "
    ** Choices : "   ® Resume ¯  < Display >  < Print >  ® Cancel ¯  "
    lnChoice    = gfModalGen('QRM00101B00021','Dialog',;
LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctUngenAl,loFormSet.GetHeaderText("LANG_SMCLOSE_lctUngenAl",loFormSet.HeaderAlias)))+'|'+LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctUngenAl,loFormSet.GetHeaderText("LANG_SMCLOSE_lctUngenAl",loFormSet.HeaderAlias))),"lfvUngnAllo()")
*N000682,1 11/20/2012 MMT Globlization changes[End]

    lnDlgOpTion = lnChoice
    =IIF( lnChoice = 2 , lfvUngnAllo() , '' )
    llAllo_Flag = lnChoice = 1
    loFormSet.laCompany[loFormSet.puCompany,9] = IIF(!llAllo_Flag,'F',' ')
    EXIT
  ENDSCAN
ENDIF

RETURN llAllo_Flag

*!**************************************************************************
*!
*!      Function:  lfvUngnAllo
*!
*!**************************************************************************
*
FUNCTION lfvUngnAllo

DO CASE
  ** Display option.
  CASE lnDlgOpTion = 2

    lcTmpAutAlo = gfTempName()

    SELECT *;
      FROM &lcAutHd;
     WHERE NAUTFRQNO = 0;
       AND CAUTTYPE  = "A";
     ORDER BY CAUTTYPE,CAUTCODE;
      INTO CURSOR &lcTmpAutAlo

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFile_Ttl = LANG_SMCLOSE_lctUngenAl
lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctUngenAl,loFormSet.GetHeaderText("LANG_SMCLOSE_lctUngenAl",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    lcBrFields = gfDbfField('GLAUTHD')

    SELECT(lcTmpAutAlo)
    DIMENSION laTemp[1]
    =gfBrows(.F.,"cbatchno",'laTemp',lcFile_Ttl,.F.)

    USE IN &lcTmpAutAlo

  ** Resume or cancel option.
  OTHERWISE
    CLEAR READ
ENDCASE

*!**************************************************************************
*!
*!      Function:  lfUngenRec
*!
*!**************************************************************************
*
FUNCTION lfUngenRec

** Check for Ungenerated Recurring.

** This flag to allow me to exit whenever I found an ungenerated Recurring.
lnChoice    = 1    && Variable to hold the Choice
lnDlgOpTion = 1
llRecu_Flag = .T.  && Variable to stop the Counter

SELECT(lcAutHd)

IF SEEK('R')
  ********* Ungenerated Recurring Transactions *********
  SCAN REST WHILE cAutType = 'R';
        FOR (BETWEEN(dAutNGDat,ldFsppBgDt,ldFsppEnDt);
       .OR. BETWEEN(dAutEnDat,ldFsppBgDt,ldFsppEnDt))

    ** MESSAGE : "The closing period has ð. You may cancel the period"
    **           "closing, display or print the ungenerated Recurring"
    **           "Transactions, or resume with the period closing.   "
    ** Choices : "   ® Resume ¯  < Display >  < Print >  ® Cancel ¯  "
    lnChoice = gfModalGen('QRM00101B00021','Dialog',;
LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctUngenRc,loFormSet.GetHeaderText("LANG_SMCLOSE_lctUngenRc",loFormSet.HeaderAlias)))+'|'+LOWER(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctUngenRc,loFormSet.GetHeaderText("LANG_SMCLOSE_lctUngenRc",loFormSet.HeaderAlias))),"lfvUngnRec()")
*N000682,1 11/20/2012 MMT Globlization changes[End]

    lnDlgOpTion = lnChoice
    =IIF( lnChoice = 2 , lfvUngnRec() , '' )
    llRecu_Flag = lnChoice = 1
    loFormSet.laCompany[loFormSet.puCompany,9] = IIF(!llRecu_Flag,'F',' ')
    EXIT
  ENDSCAN
ENDIF

RETURN llRecu_Flag

*!**************************************************************************
*!
*!      Function:  lfvUngnRec
*!
*!**************************************************************************
*
FUNCTION lfvUngnRec

DO CASE
  ** Display option
  CASE lnDlgOpTion = 2

    lcTmpAutRec = gfTempName()

    SELECT *;
      FROM &lcAutHd;
     WHERE CAUTTYPE = "R";
     .AND. (BETWEEN(dautngdat,ldFsppBgDt,ldFsppEnDt);
      .OR. BETWEEN(dautendat,ldFsppBgDt,ldFsppEnDt));
     ORDER BY CAUTTYPE,CAUTCODE;
      INTO CURSOR &lcTmpAutRec


    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFile_Ttl = LANG_SMCLOSE_lctUngenRc
lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctUngenRc,loFormSet.GetHeaderText("LANG_SMCLOSE_lctUngenRc",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]


    SELECT(lcTmpAutRec)
    lcBrFields = gfDbfField('GLAUTHD')

    DIMENSION laTemp[1]
    =gfBrows(.F.,"cbatchno",'laTemp',lcFile_Ttl,.F.)

    USE IN &lcTmpAutRec

  ** Resume or cancel option
  OTHERWISE
    CLEAR READ
ENDCASE

*!**************************************************************************
*!
*!      Function:  lfUpdate
*!
*!**************************************************************************
*
FUNCTION lfUpdate

llRevers = .F.   && Flag to hold if there is any reversed transactions.

llUpdate = .T.

SELECT(lcPTrnHd)
** Locate for the transaction posting date less than the closing date.
LOCATE FOR dTrnPDate <= ldFsppEnDt .AND. cTrnRever = 'Y'

IF FOUND()
  llRevers = .T.
  ** Generate the reversing transactions for the transaction posted
  ** up to the period.

  SELECT(lcTmpBatch)
  ZAP

  SELECT(lcTmpTrnHd)
  ZAP

  SELECT(lcTmpTrnDt)
  ZAP

  ** Set relation between the posted transaction header & detail.
  SELECT(lcPTrnHd)
  SET RELATION TO CBATCHNO+CTRANNO INTO &lcPTrnDt ADDITIVE

  lnTotTrnCr = 0          && Variable to hold the transaction total Credit.
  lnTotTrnDr = 0          && Variable to hold the transaction total Debit.
  lnTranNo   = 0          && Variable to hold the transaction no.
  lnCurtrns  = 0          && Variable to hold the termometer counter.
  lnTotTrns  = RECCOUNT() && Variable to hold the No of REcords in the file.

  SCAN FOR dTrnPDate <= ldFsppEnDt .AND. cTrnRever = 'Y'
    lnCurtrns  = lnCurtrns + 1
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfThermo(lnTotTrns,lnCurtrns,LANG_SMCLOSE_lcTGenRevT,"")
=gfThermo(lnTotTrns,lnCurtrns,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTGenRevT,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTGenRevT",loFormSet.HeaderAlias)),"")
*N000682,1 11/20/2012 MMT Globlization changes[End]


    lnTranNo = lnTranNo + 1

    SELECT(lcPTrnDt)

    SCAN REST WHILE cBatchNo+cTranNo = ;
                    &lcPTrnHd..cBatchNo+&lcPTrnHd..cTranNo

      INSERT INTO &gcWorkDir.&lcTmpTrnDt;
             (CBATCHNO,CTRANNO,CACCTCODE,CTRDTEXP,CDRORCR,NAMOUNT,;
             DTRNPDATE,CTRNPYR,CTRNPPRD);
       VALUES('000000',PADL(lnTranNo,FSIZE('CTRANNO'),'0'),;
             &lcPTrnDt..CACCTCODE,&lcPTrnDt..CTRDTEXP,;
             IIF(&lcPTrnDt..CDRORCR ='D','C','D'),;
             &lcPTrnDt..NAMOUNT,&lcPTrnHd..DTRNREVDT,;
             &lcPTrnHd..CTRNREVYR,&lcPTrnHd..CTRNREVPR)

      DO CASE
        CASE &lcPTrnDt..CDRORCR ='D'
          lnTotTrnCr = lnTotTrnCr + &lcPTrnDt..NAMOUNT
        CASE &lcPTrnDt..CDRORCR ='C'
          lnTotTrnDr = lnTotTrnDr + &lcPTrnDt..NAMOUNT
      ENDCASE

      =gfAdd_Info('&lcTmpTrnDt')

      SELECT(lcPTrnHd)
    ENDSCAN

    INSERT INTO &gcWorkDir.&lcTmpTrnHD;
           (CBATCHNO,CTRANNO,CTRNDESC,CTRNREFER,DTRNPDATE,;
           CTRNPYR,CTRNPPRD,CTRNSTAT,CTRNTYPE,CTRNREVER,CSTANDARD,;
           CSRCJRNL,NTRNTOTDR,NTRNTOTCR,CSRCMODUL,CCOMP_ID);
    VALUES('000000',PADL(lnTranNo,FSIZE('CTRANNO'),'0'),;
           'Reversing transaction by '+oAriaApplication.User_ID,;
           'On '+DTOC(ldTodayDate),&lcPTrnHd..DTRNREVDT,&lcPTrnHd..CTRNREVYR,;
           &lcPTrnHd..CTRNREVPR,'U','N','N',&lcPTrnHd..CSTANDARD,;
           &lcPTrnHd..CSRCJRNL,lnTotTrnDr,lnTotTrnCr,&lcPTrnHd..cSrcModul,;
           loFormSet.laCompany[loFormSet.puCompany,2])

    lnTotTrnCr = 0
    lnTotTrnDr = 0

    =gfAdd_Info('&lcTmpTrnHD')

    SELECT(lcPTrnHd)
  ENDSCAN

  IF lnCurtrns  < lnTotTrns
    FOR lnCounter = lnCurtrns TO lnTotTrns
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfThermo(lnTotTrns,lnCounter,LANG_SMCLOSE_lcTGenRevT,"")
=gfThermo(lnTotTrns,lnCounter,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTGenRevT,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTGenRevT",loFormSet.HeaderAlias)),"")
*N000682,1 11/20/2012 MMT Globlization changes[End]

    ENDFOR
  ENDIF

  ** Generat batches from generated transactions.
  lcBatchKey = ' '  && Variable to store the KEY Exp. for comparison.
  lcCurntPrd = ' '  && Variable to hold the new current period.

  lnBatchNo  = 0    && Variable to hold the new Batch No.
  lnTotBatDr = 0    && Variable to hold the total Debit for each batch.
  lnTotBatCr = 0    && Variable to hold the total Credit for each batch.

  llFrstTime = .T.

  SELECT(lcTmpTrnDt)
  SET ORDER TO TAG TRANSNO

  SELECT(lcTmpTrnHD)
  SET ORDER TO STNPYRPRD
  SET RELATION TO CTRANNO INTO &lcTmpTrnDt ADDITIVE

  lnCurtrns  = 0
  lnTotTrns  = RECCOUNT()

  GO TOP
  SCAN
    IF !(CSTANDARD+CTRNPYR+CTRNPPRD == lcBatchKey)

      ** Assign the key to the new Exp. to compair with.
      lcBatchKey = &lcTmpTrnHD..CSTANDARD+;
                   &lcTmpTrnHD..CTRNPYR+;
                   &lcTmpTrnHD..CTRNPPRD

      lnCurtrns  = RECNO()

      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfThermo(lnTotTrns,lnCurtrns,LANG_SMCLOSE_lcTGenRevB,"")
=gfThermo(lnTotTrns,lnCurtrns,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTGenRevB,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTGenRevB",loFormSet.HeaderAlias)),"")
*N000682,1 11/20/2012 MMT Globlization changes[End]


      IF !llFrstTime

        SELECT(lcTmpBatch)

        REPLACE NBATCNTOT WITH lnTotBatDr,;
                NBATOTDR  WITH lnTotBatDr,;
                NBATOTCR  WITH lnTotBatCr

        =gfAdd_Info('&lcTmpBatch')

        lnTotBatDr = 0
        lnTotBatCr = 0

      ENDIF

      llFrstTime = .F.
      lnBatchNo  = lnBatchNo+1
	  =SEEK(&lcTmpTrnHD..CTRNPYR+&lcTmpTrnHD..CTRNPPRD,(lcFsPrd))

      *B610361,1 fix problems resulted from globalization TMI 06/06/13 [Start] comment and move down
      *INSERT INTO &gcWorkDir.&lcTmpBatch;
             (CBATCHNO,CBATSTAT,LBATIND,CBATTYPE,CBATPYR,DBATPBEG,;
              DBATPEND,CBATREFER,CBATDESC,CSRCMODUL,CCOMP_ID)
              *B610361,1 fix problems resulted from globalization TMI 06/06/13 [End  ] 

      *N000682,1 04/17/2013 RAS Globalization[START]
	
*!*	      VALUES(PADL(lnBatchNo,FSIZE('CBATCHNO'),'0'),'U',.T.,;
*!*	             IIF(&lcTmpTrnHD..CSTANDARD = 'Y','N','S'),;
*!*	             &lcTmpTrnHD..CTRNPYR, &lcFsPrd..dFsppbgdt, &lcFsPrd..dFsppendt, ;
*!*	             'On '+DTOC(ldTodayDate),'Reversing batch by '+oAriaApplication.User_ID,;
*!*	             &lcTmpTrnHD..cSrcModul,loFormSet.laCompany[loFormSet.puCompany,2])

       *B610361,1 fix problems resulted from globalization TMI 06/06/13 [Start] comment, add the above part
       *VALUES(PADL(lnBatchNo,FSIZE('CBATCHNO'),'0'),'U',.T.,;
       *      IIF(&lcTmpTrnHD..CSTANDARD = 'Y','N','S'),;
       *      &lcTmpTrnHD..CTRNPYR, &lcFsPrd..dFsppbgdt, &lcFsPrd..dFsppendt, ;
       *      'On '+DTOC(ldTodayDate),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Reversing_batch_by,loFormSet.GetHeaderText("LANG_Reversing_batch_by",loFormSet.HeaderAlias)) +oAriaApplication.User_ID,;
       *     &lcTmpTrnHD..cSrcModul,loFormSet.laCompany[loFormSet.puCompany,2])
       *B610361,1 fix problems resulted from globalization TMI 06/06/13 [End  ] 
             
      *N000682,1 04/17/2013 RAS Globalization[End  ]
      
      *B610361,1 fix problems resulted from globalization TMI 06/06/13 [Start] 
      INSERT INTO &gcWorkDir.&lcTmpBatch;
             (CBATCHNO,CBATSTAT,LBATIND,CBATTYPE,CBATPYR,DBATPBEG,;
              DBATPEND,CBATREFER,CBATDESC,CSRCMODUL,CCOMP_ID) ;
       VALUES(PADL(lnBatchNo,FSIZE('CBATCHNO'),'0'),'U',.T.,;
             IIF(&lcTmpTrnHD..CSTANDARD = 'Y','N','S'),;
             &lcTmpTrnHD..CTRNPYR, &lcFsPrd..dFsppbgdt, &lcFsPrd..dFsppendt, ;
             'On '+DTOC(ldTodayDate),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Reversing_batch_by,loFormSet.GetHeaderText("LANG_Reversing_batch_by",loFormSet.HeaderAlias)) +oAriaApplication.User_ID,;
            &lcTmpTrnHD..cSrcModul,loFormSet.laCompany[loFormSet.puCompany,2])
      *B610361,1 fix problems resulted from globalization TMI 06/06/13 [End  ] 
    ENDIF

    ** Calculate the total Debit & Credit for each batch.
    lnTotBatDr = lnTotBatDr + &lcTmpTrnHD..NTRNTOTDR
    lnTotBatCr = lnTotBatCr + &lcTmpTrnHD..NTRNTOTCR

    SELECT(lcTmpTrnDt)

    REPLACE REST WHILE CTRANNO = &lcTmpTrnHd..CTRANNO;
                       CBATCHNO WITH PADL(lnBatchNo,FSIZE('CBATCHNO'),'0')

    SELECT(lcTmpTrnHd)
    REPLACE CBATCHNO WITH PADL(lnBatchNo,FSIZE('CBATCHNO'),'0')
  ENDSCAN

  SELECT(lcTmpBatch)

  REPLACE NBATCNTOT WITH lnTotBatDr,;
          NBATOTDR  WITH lnTotBatDr,;
          NBATOTCR  WITH lnTotBatCr

  =gfAdd_Info('&lcTmpBatch')

  IF lnCurtrns  < lnTotTrns
    FOR lnCounter = lnCurtrns TO lnTotTrns
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfThermo(lnTotTrns,lnCounter,LANG_SMCLOSE_lcTGenRevB,"")
=gfThermo(lnTotTrns,lnCounter,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTGenRevB,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTGenRevB",loFormSet.HeaderAlias)),"")
*N000682,1 11/20/2012 MMT Globlization changes[End]

    ENDFOR
  ENDIF
ENDIF

** MESSAGE : "Remove VOID Batches & Transactions from the files ?"
** CHOICES : "  ® Remove ¯  < Dont Remove >  ® Cancel Closing ¯  "

lnChoice = gfModalGen('TRM00102B00022','Dialog','')

DO CASE
  CASE lnChoice = 1
    lcDelScop = "VP"
  CASE lnChoice = 2
    lcDelScop = "P"
  CASE lnChoice = 3
    ** Set relation off ***
    SELECT(lcPTrnHd)
    SET RELATION TO

    SELECT(lcTmpTrnHD)
    SET RELATION TO
    llUpdate = .F.
    loFormSet.laCompany[loFormSet.puCompany,9] = IIF(!llUpdate,'F',' ')
    RETURN llUpdate
ENDCASE

=SEEK(loFormSet.laCompany[loFormSet.puCompany,2],'SYCCOMP')

************************** Update master files *****************************

IF gfFLOCK("&lcTrnsDt.,&lcTrnsHd,&lcBatch.,&lcPtrnHd.,&lcFsPrd",.T.) ;
   .AND. gfRLOCK("SYCCOMP",.T.)
  IF lcClose = 'P'

    ** Update the current period of the selected company.
    SELECT SYCCOMP
    REPLACE CCURR_PRD WITH PADL(INT(VAL(CCURR_PRD))+1,2,'0')
    =gfAdd_Info('SYCCOMP')

    lcCurntPrd = PADL(INT(VAL(CCURR_PRD)),2,'0')
    ** Update fiscal period file.
    SELECT (lcFsPrd)
    SET ORDER TO TAG COMFYRPRDI
    SEEK loFormSet.laCompany[loFormSet.puCompany,4]+loFormSet.laCompany[loFormSet.puCompany,3]
    ** Close curren period and lock it if required in glsetup.
    REPLACE lFspLocks WITH lFspLocks .OR. &lcSetup..lSetLokPd,;
            lFspclsds WITH .T.
    =gfAdd_Info(lcFsPrd)
    ** Unlock next period to be the current.
    SKIP 1
    REPLACE lFspLocks WITH .F.
    =gfAdd_Info(lcFsPrd)
  ENDIF
  ** Remove all posted batches ( and voided if requird )
  ** posted in current period.
  SELECT(lcBatch)

  IF !EMPTY(STRTRAN(lcDelScop , 'P' , ''))
    DELETE FOR  cBatStat $ STRTRAN(lcDelScop , 'P' , '') .AND.;
               (BETWEEN(dBatPBeg,ldFsppBgDt,ldFsppEnDt) .OR.;
                BETWEEN(dBatPEnd,ldFsppBgDt,ldFsppEnDt))

  ENDIF    && End of IF

  ** Remove all posted transactions ( and voided if requird )
  ** posted in current period.

  SELECT(lcTrnsHd)
  SET ORDER TO TAG TRANSTAT
  SET RELATION TO CBATCHNO+CTRANNO INTO &lcTrnsDT ADDITIVE

  SCAN FOR cTrnStat $ lcDelScop ;
      .AND. cTrnPprd <= loFormSet.laCompany[loFormSet.puCompany,3]

*N000682,1 04/17/2013 RAS Globalization[START]
*!*	    =lfThermo('Remove Voided Batches & Transactions')
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfThermo( LANG_Remove_Voided_Batches_Transactions)
=lfThermo( IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Remove_Voided_Batches_Transactions,loFormSet.GetHeaderText("LANG_Remove_Voided_Batches_Transactions",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/17/2013 RAS Globalization[End  ]
    SELECT(lcTrnsDt)
    DELETE REST FOR cBatchNo+cTranNo = ;
                      &lcTrnsHd..cBatchNo+&lcTrnsHd..cTranNo
    SELECT(lcTrnsHd)
    DELETE
  ENDSCAN
  WAIT CLEAR
  ** If there is any generated revers transactions/batches
  ** Update posted transaction file and add the generated
  ** transactions/batches.

  IF llRevers

    ** Change the transations revers status to revesed.
    SELECT(lcPTrnHd)

    REPLACE FOR cTrnRever = 'Y' .AND.;
                dTrnPDate <= ldFsppEnDt;
                cTrnRever WITH 'R',;
                cComp_Id  WITH loFormSet.laCompany[loFormSet.puCompany,2],;
                cAdd_User WITH oAriaApplication.User_ID,;
                dAdd_Date WITH DATE(),;
                cAdd_Time WITH gfGetTime()


    SELECT(lcBatch)
    SET RELATION TO

    SELECT(lcTrnsHd)
    SET RELATION TO

    SELECT(lcTrnsDt)

    ** Update master batch/transactions header/transaction details.
    SET ORDER TO TAG BATCHNO  IN &lcTmpBatch
    SET ORDER TO TAG BATCHTRN IN &lcTmpTrnHd
    SET ORDER TO TAG BATCHTRN IN &lcTmpTrnDt

    SELECT(lcTmpBatch)
    SET RELATION TO

    SELECT(lcTmpTrnHd)
    SET RELATION TO
    SET RELATION TO &lcTmpTrnHd..cbatchno INTO &lcTmpBatch ADDITIVE

    SELECT(lcTmpTrnDt)
    SET RELATION TO &lcTmpTrnDt..cbatchno+&lcTmpTrnDt..ctranno INTO &lcTmpTrnHd ADDITIVE

    ** Set variables for the Thermometer.
    lnCurtrns  = 0
    lnTotTrns  = RECCOUNT()

    ** Variable to compair if there is a change in the batch No.
    lcBatchKey = ' '

    ** Variable to compair if there is a change in the Transaction No.
    lcTrnNoKey = ' '

    ** Variables to sum the total debit & credit per Batch.
    lnTotBatDr = 0
    lnTotBatCr = 0

    ** Variables to sum the total debit & credit per Transaction.
    lnTotTrnDr = 0
    lnTotTrnCr = 0

    SCAN
      ** Assign the thermometer variable with the Record no.
      lnCurtrns  = lnCurtrns + 1
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfThermo(lnTotTrns,lnCurtrns,LANG_SMCLOSE_lcTUpdMast,"")
=gfThermo(lnTotTrns,lnCurtrns,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTUpdMast,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTUpdMast",loFormSet.HeaderAlias)),"")
*N000682,1 11/20/2012 MMT Globlization changes[End]


      ** Compair if the batch no = the old batch or not. If not
      ** we are going to creat a new batch no.
      IF lcBatchKey <> cBatchNo

        lcBatchKey = cBatchNo

        ** Creating New batch No.

        lcBatchNo = gfSequence('CBATCHNO', loFormSet.laCompany[loFormSet.puCompany,2])

        SELECT(lcTmpBatch)
        SCATTER MEMVAR MEMO
        m.cBatchNo = lcBatchNo
        m.nBatCnTot= 0
        m.nBatotDr = 0
        m.nBatotCr = 0

        SELECT(lcBatch)
        APPEND BLANK
        GATHER MEMVAR MEMO
        =gfAdd_Info('&lcBatch')

        SELECT(lcTmpTrnDt)
        lnTotBatDr = 0
        lnTotBatCr = 0
      ENDIF

      ** Compair if the batch no = the old transaction or not.  If not
      ** we are going to creat a new transaction no.
      IF lcTrnNoKey <> cTranNo

        lcTrnNoKey = cTranNo

        ** Create New Transaction No.

        lcTranNo = gfSequence('CTRANNO' , loFormSet.laCompany[loFormSet.puCompany,2])

        SELECT(lcTmpTrnHd)
        SCATTER MEMVAR MEMO
        m.cBatchNo = lcBatchNo
        m.cTranNo  = lcTranNo
        m.nTrnTotDr= 0
        m.nTrnTotCr= 0

        SELECT(lcTrnsHd)
        APPEND BLANK
        GATHER MEMVAR MEMO
        =gfAdd_Info('&lcTrnsHd')

        SELECT(lcTmpTrnDt)
        lnTotTrnDr = 0
        lnTotTrnCr = 0
      ENDIF

      SELECT(lcTmpTrnDt)
      SCATTER MEMVAR MEMO
      m.cBatchNo = lcBatchNo
      m.cTranNo  = lcTranNo

      IF &lcTmpTrnDt..cDrorcr = 'D'
        lnTotTrnDr = lnTotTrnDr+nAmount
        lnTotBatDr = lnTotBatDr+nAmount
      ELSE
        lnTotTrnCr = lnTotTrnCr+nAmount
        lnTotBatCr = lnTotBatCr+nAmount
      ENDIF

      SELECT(lcTrnsDt)
      APPEND BLANK
      GATHER MEMVAR MEMO
      =gfAdd_Info('&lcTrnsDt')

      SELECT(lcTrnsHd)
      REPLACE nTrnTotDr WITH lnTotTrnDr,;
              nTrnTotCr WITH lnTotTrnCr

      SELECT(lcBatch)
      REPLACE nBatCnTot WITH lnTotBatDr,;
              nBatotDr  WITH lnTotBatDr,;
              nBatotCr  WITH lnTotBatCr

      SELECT(lcTmpTrnDt)
    ENDSCAN

    SELECT(lcTrnsHd)
    REPLACE nTrnTotDr WITH lnTotTrnDr,;
            nTrnTotCr WITH lnTotTrnCr

    SELECT(lcBatch)
    REPLACE nBatCnTot WITH lnTotBatDr,;
            nBatotDr  WITH lnTotBatDr,;
            nBatotCr  WITH lnTotBatCr

    IF lnCurtrns < lnTotTrns
      FOR lnCounter = lnCurtrns TO lnTotTrns
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfThermo(lnTotTrns,lnCounter,LANG_SMCLOSE_lcTUpdMast,"")
=gfThermo(lnTotTrns,lnCounter,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTUpdMast,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTUpdMast",loFormSet.HeaderAlias)),"")
*N000682,1 11/20/2012 MMT Globlization changes[End]

      ENDFOR
    ENDIF
  ENDIF

  ** Set the relation off ***
  SELECT(lcTrnsHd)
  SET RELATION TO

  SELECT(lcTrnsDt)
  SET RELATION TO
  *B612064,1 MMT 02/19/2020 Fix bug of not excluding 1099 records in APDIST while checking unreleased entries[Start]
  IF USED(lcTmpBatch)
  *B612064,1 MMT 02/19/2020 Fix bug of not excluding 1099 records in APDIST while checking unreleased entries[End]
  SELECT(lcTmpBatch)
  SET RELATION TO
  *B612064,1 MMT 02/19/2020 Fix bug of not excluding 1099 records in APDIST while checking unreleased entries[Start]
  ENDIF
  *B612064,1 MMT 02/19/2020 Fix bug of not excluding 1099 records in APDIST while checking unreleased entries[End]
  SELECT(lcPTrnHd)
  SET RELATION TO
  *B612064,1 MMT 02/19/2020 Fix bug of not excluding 1099 records in APDIST while checking unreleased entries[Start]
  IF USED(lcTmpTrnHd)
  *B612064,1 MMT 02/19/2020 Fix bug of not excluding 1099 records in APDIST while checking unreleased entries[End]
  SELECT(lcTmpTrnHd)
  SET RELATION TO
  *B612064,1 MMT 02/19/2020 Fix bug of not excluding 1099 records in APDIST while checking unreleased entries[Start]
  ENDIF
  IF USED(lcTmpTrnDt)
  *B612064,1 MMT 02/19/2020 Fix bug of not excluding 1099 records in APDIST while checking unreleased entries[End]
  SELECT(lcTmpTrnDt)
  SET RELATION TO
  *B612064,1 MMT 02/19/2020 Fix bug of not excluding 1099 records in APDIST while checking unreleased entries[Start]
  ENDIF
  *B612064,1 MMT 02/19/2020 Fix bug of not excluding 1099 records in APDIST while checking unreleased entries[End]

  ** Unlock the locked file
  =gfFLOCK("&lcTrnsDt.,&lcTrnsHd,&lcBatch.,&lcPtrnHd.,&lcFsPrd",.F.);
   .AND. gfRLOCK("SYCCOMP",.F.)
  ** MESSAGE : "Closing period processing is finished."
  **           "Current period is ð.                  "
  ** Choices : "                  ® Ok ¯              "
  IF lcClose = 'P'
    =gfModalGen('TRM00104B00000','Dialog',lcCurntPrd)
    loFormSet.laCompany[loFormSet.puCompany,9] = 'C'
  ENDIF
ELSE
  loFormSet.laCompany[loFormSet.puCompany,9] = 'L'
  =gfFLOCK("&lcTrnsDt.,&lcTrnsHd,&lcBatch.,&lcPtrnHd.,&lcFsPrd",.F.);
   .AND. gfRLOCK("SYCCOMP",.F.)
ENDIF

************** CLOSING YEAR & PERIOD **************

*!**************************************************************************
*!
*!      Function : lfAdjBalnc
*!
*!**************************************************************************
*
FUNCTION lfAdjBalnc

PRIVATE lcFisYear, lcLastYear, lcLastPrd

** Lock the bacht file to update.
IF gfFLOCK("&lcAcBals.",.T.)

  ** Variable to hold the new current year.
  lcFisYear = STR(INT(VAL(loFormSet.laCompany[loFormSet.puCompany,4])+1),4)

  ** if the closing date is 'V'alid or 'I'nvalid.
  IF lcClosStat $ 'IV'
    SELECT(lcAcBals)
    lcSavOrder = SET('ORDER')
    SET ORDER TO TAG FISFYEAR

    ** Delete all the balances for the posting period is greater
    ** than the closing period.
    lcPrdTDel = PADL(INT(VAL(loFormSet.laCompany[loFormSet.puCompany,3])+1),2,'0')
    IF SEEK (loFormSet.laCompany[loFormSet.puCompany,4]+lcPrdTDel)
      DELETE REST WHILE cfisfyear = loFormSet.laCompany[loFormSet.puCompany,4]
      SET ORDER TO &lcSavOrder
    ENDIF
  ENDIF

  ** If there is any change in the period dates or ranges.
  IF llPrdChange
    SELECT(lcAcBals)
    lcSavOrder = SET('ORDER')
    SET ORDER TO TAG FISFYEAR
    ** Seek for the first period.
    IF SEEK(lcFisYear+'01')
      ** Delete the rest of the periods.
      DELETE REST
    ENDIF

    SET ORDER TO &lcSavOrder

    ** Seek for the new 'C'urrent year in the fiscal header file to now
    ** the no of periods to recreate the balance of it.
    SELECT (lcFisHd)
    SET ORDER TO TAG COMPFYEAR
    LOCATE FOR cFisFYear + cFisYStat = lcFisYear + 'C'
    IF FOUND()
      ** Variable to hold the no of periods of the new next year.
      lnNoOfPrd = INT(VAL(&lcFisHd..cFisNoPrd))
      ** Creat a cursor that hold the no of periods.
      CREATE CURSOR &lcPeriods (CFSPPRDID C(2))

      FOR lnCount = 1 TO lnNoOfPrd
        INSERT INTO &lcPeriods;
         VALUES(PADL(lnCount,2,'0'))
      ENDFOR

      ** Add the new balances records of the new current year with
      ** initialization of the begining balance and closing balance
      ** for the accounts with type 'A','L','Q','Y' by the closing
      ** of the nest year last period.
      *B610484,1 TMI 08/27/2013 [Start] the oActiveLang is seen as an Alias in this context, change the code a bit to check a variable instead
      *SELECT &lcAcChar..CACCTCODE,;
      *       lcFisYear AS 'CFISFYEAR',;
      *       &lcPeriods..CFSPPRDID,;
      *       000000000000000.00 AS 'NACBPTDDR',;
      *       000000000000000.00 AS 'NACBPTDCR',;
      *       000000000000000.00 AS 'NACBYTDDR',;
      *       000000000000000.00 AS 'NACBYTDCR',;
      *       IIF(LEFT(&lcAcChar..ctypecode,1) $ 'ALQY',;
      *                &lcAcBals..NACBCLBAL,000000000000000.00 ) AS 'NACBOPBAL',;
      *       IIF(LEFT(&lcAcChar..ctypecode,1) $ 'ALQY',;
      *                &lcAcBals..NACBCLBAL,000000000000000.00 ) AS 'NACBCLBAL',;
      *       oAriaApplication.User_ID   AS 'CADD_USER',;
      *       DATE()      AS 'DADD_DATE',;
      *       gfGetTime() AS 'CADD_TIME',;
      *       lfThermo(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTAdjBal,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTAdjBal",loFormSet.HeaderAlias)));
      *  FROM &lcAcChar,&lcAcBals,&lcPeriods;
      * WHERE &lcAcBals..CACCTCODE = &lcAcChar..CACCTCODE;
      *   AND &lcAcBals..CFISFYEAR = loFormSet.laCompany[loFormSet.puCompany,4];
      *   AND &lcAcBals..CFSPPRDID = loFormSet.laCompany[loFormSet.puCompany,3];
      *  INTO DBF &gcWorkDir.&lcTempBal
      llEN = oAriaApplication.oActivelang.cLang_ID = "EN"
      SELECT &lcAcChar..CACCTCODE,;
             lcFisYear AS 'CFISFYEAR',;
             &lcPeriods..CFSPPRDID,;
             000000000000000.00 AS 'NACBPTDDR',;
             000000000000000.00 AS 'NACBPTDCR',;
             000000000000000.00 AS 'NACBYTDDR',;
             000000000000000.00 AS 'NACBYTDCR',;
             IIF(LEFT(&lcAcChar..ctypecode,1) $ 'ALQY',;
                      &lcAcBals..NACBCLBAL,000000000000000.00 ) AS 'NACBOPBAL',;
             IIF(LEFT(&lcAcChar..ctypecode,1) $ 'ALQY',;
                      &lcAcBals..NACBCLBAL,000000000000000.00 ) AS 'NACBCLBAL',;
             oAriaApplication.User_ID   AS 'CADD_USER',;
             DATE()      AS 'DADD_DATE',;
             gfGetTime() AS 'CADD_TIME',;
             lfThermo(IIF(llEN,LANG_SMCLOSE_lcTAdjBal,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTAdjBal",loFormSet.HeaderAlias)));
        FROM &lcAcChar,&lcAcBals,&lcPeriods;
       WHERE &lcAcBals..CACCTCODE = &lcAcChar..CACCTCODE;
         AND &lcAcBals..CFISFYEAR = loFormSet.laCompany[loFormSet.puCompany,4];
         AND &lcAcBals..CFSPPRDID = loFormSet.laCompany[loFormSet.puCompany,3];
        INTO DBF &gcWorkDir.&lcTempBal
      *B610484,1 TMI 08/27/2013 [End  ] 

      ** Append the balances in the balance file.
      SELECT(lcAcBals)
      APPEND FROM &gcWorkDir.&lcTempBal

      ** Close the Cursor.
      USE IN &lcPeriods
    ENDIF
  ENDIF

  ** Update the variable that hold the new current year to hold the
  ** new next year.
  lcFisYear = STR(INT(VAL(lcFisYear)+1),4)

  ** Seek for the new 'N'ext year in the fiscal header file to now
  ** the no of periods to recreate the balance of it.

  SELECT (lcFisHd)
  SET ORDER TO TAG COMPFYEAR
  LOCATE FOR cFisFYear + cFisYStat = lcFisYear + 'N'

  IF FOUND()
    ** Variable to hold the no of periods of the new next year.
    lnNoOfPrd = INT(VAL(&lcFisHd..cFisNoPrd))
    ** Creat a cursor that hold the no of periods.
    CREATE CURSOR &lcPeriods (CFSPPRDID C(2))

    FOR lnCount = 1 TO lnNoOfPrd
      INSERT INTO &lcPeriods;
       VALUES(PADL(lnCount,2,'0'))
    ENDFOR
    lnTermCount = 0
    ** Add the new balabnces records of the new next year with initialization
    ** of the begining balance and closing balance for the accounts with
    ** type 'A','L','Q','Y' by the closing of the nest year last period.

    IF llPrdChange
	
      *B610484,1 TMI 08/27/2013 [Start] the oActiveLang is seen as an Alias in this context, change the code a bit to check a variable instead
      *SELECT &lcAcChar..CACCTCODE,;
      *       lcFisYear AS 'CFISFYEAR',;
      *       &lcPeriods..CFSPPRDID,;
      *       000000000000000.00 AS 'NACBPTDDR',;
      *       000000000000000.00 AS 'NACBPTDCR',;
      *       000000000000000.00 AS 'NACBYTDDR',;
      *       000000000000000.00 AS 'NACBYTDCR',;
      *       IIF(LEFT(&lcAcChar..ctypecode,1) $ 'ALQY',;
      *                &lcAcBals..NACBCLBAL,000000000000000.00 ) AS 'NACBOPBAL',;
      *       IIF(LEFT(&lcAcChar..ctypecode,1) $ 'ALQY',;
      *                &lcAcBals..NACBCLBAL,000000000000000.00 ) AS 'NACBCLBAL',;
      *       oAriaApplication.User_ID   AS 'CADD_USER',;
      *       DATE()      AS 'DADD_DATE',;
      *       gfGetTime() AS 'CADD_TIME',;
      *       lfThermo(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTAdjBal,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTAdjBal",loFormSet.HeaderAlias)));
      *  FROM &lcAcChar,&lcAcBals,&lcPeriods;
      * WHERE &lcAcBals..CACCTCODE = &lcAcChar..CACCTCODE;
      *   AND &lcAcBals..CFISFYEAR = loFormSet.laCompany[loFormSet.puCompany,4];
      *   AND &lcAcBals..CFSPPRDID = loFormSet.laCompany[loFormSet.puCompany,3];
      *  INTO DBF &gcWorkDir.&lcTempBal
      llEN = oAriaApplication.oActivelang.cLang_ID = "EN"
      SELECT &lcAcChar..CACCTCODE,;
             lcFisYear AS 'CFISFYEAR',;
             &lcPeriods..CFSPPRDID,;
             000000000000000.00 AS 'NACBPTDDR',;
             000000000000000.00 AS 'NACBPTDCR',;
             000000000000000.00 AS 'NACBYTDDR',;
             000000000000000.00 AS 'NACBYTDCR',;
             IIF(LEFT(&lcAcChar..ctypecode,1) $ 'ALQY',;
                      &lcAcBals..NACBCLBAL,000000000000000.00 ) AS 'NACBOPBAL',;
             IIF(LEFT(&lcAcChar..ctypecode,1) $ 'ALQY',;
                      &lcAcBals..NACBCLBAL,000000000000000.00 ) AS 'NACBCLBAL',;
             oAriaApplication.User_ID   AS 'CADD_USER',;
             DATE()      AS 'DADD_DATE',;
             gfGetTime() AS 'CADD_TIME',;
             lfThermo(IIF(llEN,LANG_SMCLOSE_lcTAdjBal,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTAdjBal",loFormSet.HeaderAlias)));
        FROM &lcAcChar,&lcAcBals,&lcPeriods;
       WHERE &lcAcBals..CACCTCODE = &lcAcChar..CACCTCODE;
         AND &lcAcBals..CFISFYEAR = loFormSet.laCompany[loFormSet.puCompany,4];
         AND &lcAcBals..CFSPPRDID = loFormSet.laCompany[loFormSet.puCompany,3];
        INTO DBF &gcWorkDir.&lcTempBal
        *B610484,1 TMI 08/27/2013 [End  ] 

    ELSE
      lcLastYear = STR(INT(VAL(loFormSet.laCompany[loFormSet.puCompany,4])) + 1, 4)
      lcLastPrd  = &lcFisHd..cFisNoPrd

      *B610484,1 TMI 08/27/2013 [Start] the oActiveLang is seen as an Alias in this context, change the code a bit to check a variable instead
      *SELECT &lcAcChar..CACCTCODE,;
      *       lcFisYear AS 'CFISFYEAR',;
      *       &lcPeriods..CFSPPRDID,;
      *       000000000000000.00 AS 'NACBPTDDR',;
      *       000000000000000.00 AS 'NACBPTDCR',;
      *       000000000000000.00 AS 'NACBYTDDR',;
      *       000000000000000.00 AS 'NACBYTDCR',;
      *       IIF(LEFT(&lcAcChar..ctypecode,1) $ 'ALQY',;
      *                &lcAcBals..NACBCLBAL,000000000000000.00 ) AS 'NACBOPBAL',;
      *       IIF(LEFT(&lcAcChar..ctypecode,1) $ 'ALQY',;
      *                &lcAcBals..NACBCLBAL,000000000000000.00 ) AS 'NACBCLBAL',;
      *       oAriaApplication.User_ID   AS 'CADD_USER',;
      *       DATE()      AS 'DADD_DATE',;
      *       gfGetTime() AS 'CADD_TIME',;
      *       lfThermo(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTAdjBal,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTAdjBal",loFormSet.HeaderAlias)));
      *  FROM &lcAcChar,&lcAcBals,&lcPeriods;
      * WHERE &lcAcBals..CACCTCODE = &lcAcChar..CACCTCODE;
      *   AND &lcAcBals..CFISFYEAR = lcLastYear;
      *   AND &lcAcBals..CFSPPRDID = lcLastPrd;
      *  INTO DBF &gcWorkDir.&lcTempBal
      llEN = oAriaApplication.oActivelang.cLang_ID = "EN"
      SELECT &lcAcChar..CACCTCODE,;
             lcFisYear AS 'CFISFYEAR',;
             &lcPeriods..CFSPPRDID,;
             000000000000000.00 AS 'NACBPTDDR',;
             000000000000000.00 AS 'NACBPTDCR',;
             000000000000000.00 AS 'NACBYTDDR',;
             000000000000000.00 AS 'NACBYTDCR',;
             IIF(LEFT(&lcAcChar..ctypecode,1) $ 'ALQY',;
                      &lcAcBals..NACBCLBAL,000000000000000.00 ) AS 'NACBOPBAL',;
             IIF(LEFT(&lcAcChar..ctypecode,1) $ 'ALQY',;
                      &lcAcBals..NACBCLBAL,000000000000000.00 ) AS 'NACBCLBAL',;
             oAriaApplication.User_ID   AS 'CADD_USER',;
             DATE()      AS 'DADD_DATE',;
             gfGetTime() AS 'CADD_TIME',;
             lfThermo(IIF(llEN,LANG_SMCLOSE_lcTAdjBal,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTAdjBal",loFormSet.HeaderAlias)));
        FROM &lcAcChar,&lcAcBals,&lcPeriods;
       WHERE &lcAcBals..CACCTCODE = &lcAcChar..CACCTCODE;
         AND &lcAcBals..CFISFYEAR = lcLastYear;
         AND &lcAcBals..CFSPPRDID = lcLastPrd;
        INTO DBF &gcWorkDir.&lcTempBal
      *B610484,1 TMI 08/27/2013 [End  ] 
    ENDIF

    SELECT(lcAcBals)
    APPEND FROM &gcWorkDir.&lcTempBal

    IF USED(lcTempBal)
      USE IN &lcTempBal
    ENDIF
    ERASE &gcWorkDir.&lcTempBal+'.DBF'
    ** Close the Cursor.
    USE IN &lcPeriods
  ENDIF
  ** Unlock the batch file.
  =gfFLOCK("&lcAcBals.",.F.)
ELSE
  loFormSet.laCompany[loFormSet.puCompany,9] = 'L'
  ** Unlock the batch file.
  =gfFLOCK("&lcAcBals.",.F.)
ENDIF

WAIT CLEAR

*!**************************************************************************
*!
*!      Function : lfAdjPstYP
*!
*!**************************************************************************
*
FUNCTION lfAdjPstYP

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT LANG_SMCLOSE_lcTAdjPsYP WINDOW NOWAIT
WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTAdjPsYP,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTAdjPsYP",loFormSet.HeaderAlias)) WINDOW NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]


PRIVATE lcFisYear

lcFisYear = STR(INT(VAL(loFormSet.laCompany[loFormSet.puCompany,4])+1),4)  && New Current Year

** Lock the posted transaction header & details file, the transaction
** header & details file.

IF gfFLOCK("&lcPTrnHd.,&lcPTrnDt.,&lcTrnsHd.,&lcTrnsDt.",.T.)

  ** If the end date of the new current year is changed.

  IF lcClosStat = 'V'
    SELECT(lcPTrnHd)
    SET ORDER TO TAG POSTDATE
    SET RELATION TO cBatchNo + cTranNo INTO &lcPTrnDt ADDITIVE

    ** If the transaction date greater than the closing date.
    SELECT(lcPTrnHd)
    SCAN FOR dTrnPDate > loFormSet.laCompany[loFormSet.puCompany,10]
      SELECT (lcFsPrd)
      LOCATE FOR  cFisfYear =  lcFisYear;
             AND BETWEEN(&lcPTrnHd..dTrnPDate,dFsppBgDt,dFsppEnDt)
      IF FOUND()
        SELECT(lcPTrnDt)
        SELECT(lcPTrnHd)
        REPLACE cTrnPYr   WITH &lcFsPrd..cFisfYear,;
                cTrnPPrd  WITH &lcFsPrd..cFspprdid,;
                cAdd_User WITH oAriaApplication.User_ID,;
                dAdd_Date WITH DATE(),;
                cAdd_Time WITH gfGetTime()
      ENDIF
    ENDSCAN

    SELECT(lcPTrnHd)
    SET RELATION TO
    SET ORDER TO TAG BATCHTRN

    SELECT(lcTrnsHd)
    SET ORDER TO TAG POSTDATE
    SET RELATION TO cBatchNo + cTranNo INTO &lcTrnsDt ADDITIVE

    ** If the transaction posting date greater than the closing date.
    SELECT(lcTrnsHd)
    SCAN FOR dTrnPDate > loFormSet.laCompany[loFormSet.puCompany,10];
         AND cTrnStat <> 'V'
      SELECT (lcFsPrd)
      LOCATE FOR cFisfYear = lcFisYear;
             AND BETWEEN(&lcTrnsHd..dTrnPDate,dFsppBgDt,dFsppEnDt)

      IF FOUND()
        SELECT(lcTrnsDt)
        REPLACE REST WHILE cBatchNo + cTranNo = ;
                           &lcTrnsHd..cBatchNo + &lcTrnsHd..cTranNo;
                           &lcTrnsDt..cTrnPYr   WITH &lcFsPrd..cFisfYear,;
                           &lcTrnsDt..cTrnPPrd  WITH &lcFsPrd..cFspprdid,;
                           &lcTrnsDt..cAdd_User WITH oAriaApplication.User_ID,;
                           &lcTrnsDt..dAdd_Date WITH DATE(),;
                           &lcTrnsDt..cAdd_Time WITH gfGetTime()
        SELECT(lcTrnsHd)
		REPLACE cTrnPYr   WITH &lcFsPrd..cFisfYear,;
                cTrnPPrd  WITH &lcFsPrd..cFspprdid,;
                cAdd_User WITH oAriaApplication.User_ID,;
                dAdd_Date WITH DATE(),;
                cAdd_Time WITH gfGetTime()
      ENDIF
    ENDSCAN

    SELECT(lcTrnsHd)
    SET RELATION TO
    SET ORDER TO TAG BATCHTRN
  ENDIF

  ** Unlock the posted transaction header & details file, the transaction
  ** header & details file.
  =gfFLOCK("&lcPTrnHd.,&lcPTrnDt.,&lcTrnsHd.,&lcTrnsDt.",.F.)

ELSE
  ** Unlock the posted transaction header & details file, the transaction
  ** header & details file.
  =gfFLOCK("&lcPTrnHd.,&lcPTrnDt.,&lcTrnsHd.,&lcTrnsDt.",.F.)
  loFormSet.laCompany[loFormSet.puCompany,9] = 'L'
ENDIF

WAIT CLEAR

*!**************************************************************************
*!
*!      Function : lfBatHandl
*!
*!**************************************************************************
*
FUNCTION lfBatHandl

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_SMCLOSE_lcTBatHand NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTBatHand,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTBatHand",loFormSet.HeaderAlias)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]


SELECT (lcFisHd)
SET ORDER TO TAG COMPFYEAR

** Lock the batch, posted transaction header & details file, the
** transaction header & details file.

IF gfFLOCK("&lcBatch.,&lcPTrnHd.,&lcPTrnDt.,&lcTrnsHd.,&lcTrnsDt.",.T.)

  SELECT(lcTrnsHd)
  SET ORDER TO TAG BATCHTRN
  SET RELATION TO cBatchNo + cTranNo INTO &lcTrnsDt ADDITIVE

  SELECT(lcPTrnHd)
  SET ORDER TO TAG BATCHTRN
  SET RELATION TO cBatchNo + cTranNo INTO &lcPTrnDt ADDITIVE

  ** If the closing date is 'I'nvalid.
  IF lcClosStat = 'I'

    ** Assing all the transaction that have no batches and the
    ** transaction posting date is between the closing date and
    ** the old end date of the current year to status Invalid.

    SELECT(lcTrnsHd)
    REPLACE ALL FOR cBatchNo = '000000';
                AND BETWEEN(dTrnPDate,loFormSet.laCompany[loFormSet.puCompany,10],;
                                      loFormSet.laCompany[loFormSet.puCompany,7]);
                    cTrnStat  WITH 'I',;
                    cAdd_User WITH oAriaApplication.User_ID,;
                    dAdd_Date WITH DATE(),;
                    cAdd_Time WITH gfGetTime()

    ** Assing all the transaction that have no batches and the
    ** transaction posting date is between the closing date and
    ** the old end date of the current year to status Invalid.

    SELECT(lcPTrnHd)
    REPLACE ALL FOR cBatchNo = '000000';
                AND BETWEEN(dTrnPDate,loFormSet.laCompany[loFormSet.puCompany,10],;
                                      loFormSet.laCompany[loFormSet.puCompany,7]);
                    cTrnStat  WITH 'I',;
                    cAdd_User WITH oAriaApplication.User_ID,;
                    dAdd_Date WITH DATE(),;
                    cAdd_Time WITH gfGetTime()
  ENDIF

  SELECT(lcBatch)
  SET ORDER TO TAG BATCHNO

  SET RELATION TO cBatchNo INTO &lcTrnsHd ADDITIVE
  SET RELATION TO cBatchNo INTO &lcPTrnHd ADDITIVE

  ** If the closing date is 'V'alid or 'I'nvalid.
  IF lcClosStat $ 'VI'

    ** If the closing date is 'I'nvalid.
    IF lcClosStat = 'I'

      ** We are scan the batch file for the batch posting date is
      ** between the closing date & the old current year end date
      ** and the batch status is not equal to 'V'oid.
      SELECT(lcBatch)
      SCAN FOR BETWEEN(dBatPBeg,loFormSet.laCompany[loFormSet.puCompany,10],;
                                loFormSet.laCompany[loFormSet.puCompany,7]);
           AND !INLIST(cBatStat,'V','R')

        ** In the comming step we are going to check for the batch stat
        ** if 'P'osted we are going to put in the variable 'lc_TrnHd'
        ** the alias of the transaction header file and the in the
        ** variable 'lc_TrnDt' the alias of the transaction details file.
        ** If the batch stat not equal to 'P'osted we are going to put
        ** the transaction header.

        IF &lcBatch..cBatStat = 'P'
          lc_TrnHd = EVALUATE('lcPTrnHd')
          lc_TrnDt = EVALUATE('lcPTrnDt')
          llFromPost = .T.
        ELSE
          lc_TrnHd = EVALUATE('lcTrnsHd')
          lc_TrnDt = EVALUATE('lcTrnsDt')
          llFromPost = .F.
        ENDIF

        ** We are going to assign all the transactions that liy between
        ** the closing date and the end date of the closing year to status
        ** 'I'nvalid.

        SELECT(lc_TrnHd)
        REPLACE REST WHILE cBatchNo = &lcBatch..cBatchNo;
                       FOR !INLIST(cTrnStat,'R','V');
                           cTrnStat  WITH  'I',;
                           cAdd_User WITH oAriaApplication.User_ID,;
                           dAdd_Date WITH DATE(),;
                           cAdd_Time WITH gfGetTime()

        ** We are going to assign all the batchs that liy between the
        ** closing date and the end date of the closing year to status
        ** 'I'nvalid.

        SELECT(lcBatch)
        REPLACE cBatStat  WITH 'I',;
                cAdd_User WITH oAriaApplication.User_ID,;
                dAdd_Date WITH DATE(),;
                cAdd_Time WITH gfGetTime()
      ENDSCAN
    ENDIF
    ** We are going to handel the batch over laping.
    llClosDate = .T.
    IF USED(lcTmpBatch)
      SELECT(lcTmpBatch)
      ZAP
    ENDIF
    SELECT(lcBatch)
    DO lpOvrLapBt WITH loFormSet.laCompany[loFormSet.puCompany,10]
    APPEND FROM &gcWorkDir.&lcTmpBatch
  ENDIF
  ** The end date of the old current year is changed.
  IF llDatChange .AND. ldDatChange < loFormSet.laCompany[loFormSet.puCompany,7]
    ** We are going to handel the batch over laping.
    llClosDate = .F.
    IF USED(lcTmpBatch)
      SELECT(lcTmpBatch)
      ZAP
    ENDIF
    SELECT(lcBatch)
    DO lpOvrLapBt WITH ldDatChange
    APPEND FROM &gcWorkDir.&lcTmpBatch
  ENDIF

  SELECT(lcTrnsHd)
  SET RELATION TO

  SELECT(lcPTrnHd)
  SET RELATION TO

  =gfFLOCK("&lcBatch.,&lcPTrnHd.,&lcPTrnDt.,&lcTrnsHd.,&lcTrnsDt.",.F.)
ELSE
  loFormSet.laCompany[loFormSet.puCompany,9] = 'L'
  =gfFLOCK("&lcBatch.,&lcPTrnHd.,&lcPTrnDt.,&lcTrnsHd.,&lcTrnsDt.",.F.)
ENDIF

WAIT CLEAR

*!**************************************************************************
*!
*!      FUNCTION : lpOvrLapBt
*!
*!**************************************************************************
*
FUNCTION lpOvrLapBt
PARAMETERS ldEndDate

PRIVATE lnBeforTotBatDr,lnBeforTotBatCr,;
        lnAfterTotBatDr,lnAfterTotBatCr,;
        lcBefor_Bat_No,lcAfter_Bat_No,;
        lcFisYear,lcPostPr,llCreatTrn

lcCurArea = ALIAS()  && Store the selected work area.
llFromPost= .F.
lcFisYear = IIF(STR(YEAR(ldEndDate),4) < loFormSet.laCompany[loFormSet.puCompany,4],STR(YEAR(ldEndDate)+2,4),STR(YEAR(ldEndDate)+1,4))

** We are going to scan the batch file for the closing date or
** the end date of the old current closing year that came in the
** parameter is between batch posted begin date & posted end date
** and the batch status is not void.

SELECT(lcBatch)
SCAN FOR BETWEEN(ldEndDate,dBatPBeg,dBatPEnd) .AND. !INLIST(cBatStat,'R','V')

  lnBeforTotBatDr = 0   && Variable to hold the totals of befor debit batch.
  lnBeforTotBatCr = 0   && Variable to hold the totals of befor credit batch.
  lnAfterTotBatDr = 0   && Variable to hold the totals of after debit batch.
  lnAfterTotBatCr = 0   && Variable to hold the totals of after credit batch.

  lcBefor_Bat_No  = ' ' && Variable to hold the befor batch no.
  lcAfter_Bat_No  = ' ' && Variable to hold the after batch no.

  llCreatTrn      = .F. && Flag to indicate if the batch have Tran. or not.

  ** In the comming step we are going to check for the batch stat
  ** if 'P'osted we are going to put in the variable 'lc_TrnHd'
  ** the alias of the transaction header file and the in the
  ** variable 'lc_TrnDt' the alias of the transaction details file.
  ** If the batch stat not equal to 'P'osted we are going to put
  ** the transaction header.

  IF &lcBatch..cBatStat = 'P'
    lc_TrnHd = EVALUATE('lcPTrnHd')
    lc_TrnDt = EVALUATE('lcPTrnDt')
    llFromPost = .T.
  ELSE
    lc_TrnHd = EVALUATE('lcTrnsHd')
    lc_TrnDt = EVALUATE('lcTrnsDt')
    llFromPost = .F.
  ENDIF

  IF lcClosStat = 'I'

    ** We are going to scan either the posted transaction header or the
    ** transaction header file for the batch no of the transaction file
    ** is equal the batch no of the batch file and the the transaction
    ** status is not void.

    SELECT(lc_TrnHd)
    SCAN REST WHILE cBatchNo = &lcBatch..cBatchNo;
                FOR !INLIST(cTrnStat,'R','V')

      ** If the transaction posted date is greater than the date in the
      ** parameter variable we are going to assign this transaction as
      ** 'I'nvalid.
      SELECT(lc_TrnHd)
      IF dTrnPDate > ldEndDate
        REPLACE cTrnStat  WITH 'I',;
                cAdd_User WITH oAriaApplication.User_ID,;
                dAdd_Date WITH DATE(),;
                cAdd_Time WITH gfGetTime()
      ELSE
        ** We are going to seek in the 'SYCFSPRD' for the current
        ** selected company and the new current year.
        SELECT (lcFsPrd)
        LOCATE FOR cFisfYear =loFormSet.laCompany[loFormSet.puCompany,4] .AND.;
                  BETWEEN(&lc_TrnHd..dTrnPDate,dFsppBgDt,dFsppEnDt)

        IF FOUND()
          ** We are going to assign the period no to this variable.
          lcPostPr = &lcFsPrd..cFspprdid
        ENDIF

        IF EMPTY(lcBefor_Bat_No)
          ** Generate a new befor batch no for the transactions that
          ** liy befor the closing date.

          lcBefor_Bat_No = gfSequence('CBATCHNO' , loFormSet.laCompany[loFormSet.puCompany,2])

        ENDIF

        ** Generate the transaction no.

        lcTranNo = gfSequence('CTRANNO' , loFormSet.laCompany[loFormSet.puCompany,2])

        ** We are going to scan the transaction detail file for the batch
        ** & transaction no is equal the transation header batch &
        ** transation no.

        SELECT(lc_TrnDt)
        SCAN REST WHILE cBatchNo + cTranNo =;
                        &lc_TrnHd..cBatchNo + &lc_TrnHd..cTranNo

          ** We are are going to collect the data of the record in a
          ** memory variables and append blank in the same file to
          ** create a new transaction detail.

          SCATTER MEMVAR MEMO
          m.cBatchNo  = lcBefor_Bat_No
          m.cTranNo   = lcTranNo
          m.cTrnPYr   = lcFisYear
          m.cTrnPPrd  = lcPostPr
          m.cAdd_User = oAriaApplication.User_ID
          m.dAdd_Date = DATE()
          m.cAdd_Time = gfGetTime()

          lnRecNo = RECNO()
          APPEND BLANK
          GATHER MEMVAR MEMO
          GO lnRecNo
        ENDSCAN

        ** We are are going to collect the data of the record in a
        ** memory variables and append blank in the same file to
        ** create a new transaction header.

        SELECT(lc_TrnHd)
        SCATTER MEMVAR MEMO
        m.cBatchNo  = lcBefor_Bat_No
        m.cTranNo   = lcTranNo
        m.cTrnPYr   = lcFisYear
        m.cTrnPPrd  = lcPostPr
        m.cAdd_User = oAriaApplication.User_ID
        m.dAdd_Date = DATE()
        m.cAdd_Time = gfGetTime()

        lnRecNo = RECNO()
        APPEND BLANK
        GATHER MEMVAR MEMO
        GO lnRecNo

        SELECT(lcTmpReport)
        APPEND BLANK

        *N000682,1 04/17/2013 RAS Globalization[START]
*!*	        REPLACE CBATCHNO  WITH lcBefor_Bat_No,;
*!*	                CTRANNO   WITH lcTranNo,;
*!*	                CDESCRIP  WITH 'Reclacified from the '+;
*!*	                               IIF(llFromPost,;
*!*	                               'posted transaction header file',;
*!*	                               'transaction header file'),;
*!*	                COLDBATNO WITH &lc_TrnHd..CBatchNo,;
*!*	                COLDTRNNO WITH &lc_TrnHd..cTranNo

        REPLACE CBATCHNO  WITH lcBefor_Bat_No,;
                CTRANNO   WITH lcTranNo,;
                CDESCRIP  WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Reclacified_from_the,loFormSet.GetHeaderText("LANG_Reclacified_from_the",loFormSet.HeaderAlias))+;
                               IIF(llFromPost,;
                               IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_posted_transaction_header_file,loFormSet.GetHeaderText("LANG_posted_transaction_header_file",loFormSet.HeaderAlias)),;
                               IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_transaction_header_file,loFormSet.GetHeaderText("LANG_transaction_header_file",loFormSet.HeaderAlias))),;
                COLDBATNO WITH &lc_TrnHd..CBatchNo,;
                COLDTRNNO WITH &lc_TrnHd..cTranNo
        *N000682,1 04/17/2013 RAS Globalization[End  ]

        SELECT(lc_TrnHd)

        ** We are going to assign the old transaction that we create the
        ** new one from by status 'R'eclassefied.

        REPLACE cTrnStat  WITH 'R',;
                cAdd_User WITH oAriaApplication.User_ID,;
                dAdd_Date WITH DATE(),;
                cAdd_Time WITH gfGetTime()

        llCreatTrn = .T.  && Flag that the batch have Tran.

        ** Add the total debit for all the batch transaction.
        lnBeforTotBatDr = lnBeforTotBatDr + &lc_TrnHd..nTrnTotDr

        ** Add the total credit for all the batch transaction.
        lnBeforTotBatCr = lnBeforTotBatCr + &lc_TrnHd..nTrnTotCr
      ENDIF
    ENDSCAN

    ** Check if the batch status is 'P'osted and the batch no doesn't
    ** created so we are going to creat a new batch no.
    SELECT(lcBatch)

    IF cBatStat = 'E' .AND. EMPTY(lcBefor_Bat_No)
      ** Generate a new befor batch no for the transactions that liy
      ** befor the closing date.

      lcBefor_Bat_No = gfSequence('CBATCHNO' , loFormSet.laCompany[loFormSet.puCompany,2])

    ENDIF

    ** If there is no batch no we are not going to add anything in the
    ** batch file.

    IF EMPTY(lcBefor_Bat_No)
      REPLACE cBatStat  WITH 'I',;
              cAdd_User WITH oAriaApplication.User_ID,;
              dAdd_Date WITH DATE(),;
              cAdd_Time WITH gfGetTime()
    ELSE
      DO CASE
        ** In this case we are going to check if the batch is
        ** empty or out of balance.

        CASE lnBeforTotBatDr = 0 .AND. lnBeforTotBatCr = 0
          IF llCreatTrn
            lcBatStat = 'O'
          ELSE
            lcBatStat = 'E'
          ENDIF

        ** In this case we are going to check the batch status. If
        ** 'A'pproved, 'H'old, 'P'osted, samari'Z'ed OR 'U'nposted.
        CASE &lcBatch..CBATSTAT $ 'AHPZU'
          lcBatStat = &lcBatch..cBatStat

        ** In this case we are going to check the batch status. If
        ** 'O'ut of order.
        CASE &lcBatch..cBatStat = 'O'
          lcBatStat = IIF(lnBeforTotBatDr = lnBeforTotBatCr,'U','O')
      ENDCASE

      llCreatTrn = .F.

      SCATTER MEMVAR MEMO
      m.cBatchNo  = lcBefor_Bat_No
      m.cBatStat  = lcBatStat
      m.cBatPyr   = lcFisYear
      m.dBatPEnd  = ldEndDate
      m.nBatCnTot = IIF(lnBeforTotBatDr >= lnBeforTotBatCr,;
                        lnBeforTotBatDr,lnBeforTotBatCr)
      m.nBaTotDr  = lnBeforTotBatDr
      m.nBaTotCr  = lnBeforTotBatCr
      m.cAdd_User = oAriaApplication.User_ID
      m.dAdd_Date = DATE()
      m.cAdd_Time = gfGetTime()

      SELECT(lcTmpBatch)
      APPEND BLANK
      GATHER MEMVAR MEMO

      SELECT(lcTmpReport)
      APPEND BLANK
      REPLACE CBATCHNO  WITH lcBefor_Bat_No,;
              CTRANNO   WITH ' ',;
              CDESCRIP  WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTRclasfi,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTRclasfi",loFormSet.HeaderAlias)),;
              COLDBATNO WITH &lcBatch..CBatchNo,;
              COLDTRNNO WITH ' '

      SELECT(lcBatch)
      ** We are going to assign the old batch status with 'R'eclassified.
      REPLACE cBatStat  WITH 'R',;
              cAdd_User WITH oAriaApplication.User_ID,;
              dAdd_Date WITH DATE(),;
              cAdd_Time WITH gfGetTime()
    ENDIF
  ELSE

    ** We are going to scan either the posted or the unposted transaction
    ** header file for batch no equal the batch no of the batch file and
    ** the transaction status is not equal 'Void'.

    SELECT(lc_TrnHd)
    SCAN REST WHILE cBatchNo = &lcBatch..cBatchNo;
                FOR !INLIST(cTrnStat,'R','V')

      ** Generate a new transaction no.

      lcTranNo = gfSequence('CTRANNO' , loFormSet.laCompany[loFormSet.puCompany,2])

      IF &lc_TrnHd..dTrnPDate <= ldEndDate
        IF EMPTY(lcBefor_Bat_No)
          ** Generate a new befor batch no for the transactions that liy
          ** befor the closing date.
          llTrnBefor = .T.

          lcBefor_Bat_No = gfSequence('CBATCHNO' , loFormSet.laCompany[loFormSet.puCompany,2])

        ENDIF
      ELSE
        IF EMPTY(lcAfter_Bat_No)
          ** Generate a new after batch no for the transactions that liy
          ** after the closing date.
          llTrnBefor = .F.

          lcAfter_Bat_No = gfSequence('CBATCHNO' , loFormSet.laCompany[loFormSet.puCompany,2])

        ENDIF
      ENDIF

      ** We are going to seek in the 'SYCFSPRD' for the current selected
      ** company and the new current year.
      SELECT (lcFsPrd)
      LOCATE FOR cFisfYear = IIF(llTrnBefor,loFormSet.laCompany[loFormSet.puCompany,4],lcFisYear);
             AND BETWEEN(&lc_TrnHd..dTrnPDate,dFsppBgDt,dFsppEnDt)

      IF FOUND()
        ** We are going to assign the period no to this variable.
        lcPostPr = &lcFsPrd..cFspprdid
      ENDIF

      ** We are going to scan the detail file for the batch & transaction
      ** no equal the transaction header batch & transaction no.

      SELECT(lc_TrnDt)
      SCAN REST WHILE cBatchNo + cTranNo =;
                      &lc_TrnHd..cBatchNo + &lc_TrnHd..cTranNo

        ** We are going to store the data in a memory variables to
        ** append a new record in the same file.

        SCATTER MEMVAR MEMO
        m.cBatchNo  = IIF(llTrnBefor,lcBefor_Bat_No,lcAfter_Bat_No)
        m.cTranNo   = lcTranNo
        m.cTrnPYr   = IIF(llTrnBefor,lcFisYear,loFormSet.laCompany[loFormSet.puCompany,4])
        m.cTrnPPrd  = lcPostPr
        m.cAdd_User = oAriaApplication.User_ID
        m.dAdd_Date = DATE()
        m.cAdd_Time = gfGetTime()

        lnRecNo = RECNO()
        APPEND BLANK
        GATHER MEMVAR MEMO
        GO lnRecNo

      ENDSCAN

      ** We are going to store the data in a memory variables to
      ** append a new record in the same file.

      SELECT(lc_TrnHd)
      SCATTER MEMVAR MEMO
      m.cBatchNo  = IIF(llTrnBefor,lcBefor_Bat_No,lcAfter_Bat_No)
      m.cTranNo   = lcTranNo
      m.cTrnPYr   = IIF(llTrnBefor,lcFisYear,loFormSet.laCompany[loFormSet.puCompany,4])
      m.cTrnPPrd  = lcPostPr
      m.cAdd_User = oAriaApplication.User_ID
      m.dAdd_Date = DATE()
      m.cAdd_Time = gfGetTime()

      lnRecNo = RECNO()
      APPEND BLANK
      GATHER MEMVAR MEMO
      GO lnRecNo

      SELECT(lcTmpReport)
      APPEND BLANK

      *N000682,1 04/17/2013 RAS Globalization[START]

*!*	      REPLACE CBATCHNO  WITH IIF(llTrnBefor,lcBefor_Bat_No,lcAfter_Bat_No),;
*!*	              CTRANNO   WITH lcTranNo,;
*!*	              CDESCRIP  WITH 'Reclacified from the '+;
*!*	                             IIF(llFromPost,;
*!*	                             'posted transaction header file',;
*!*	                             'transaction header file'),;
*!*	              COLDBATNO WITH &lc_TrnHd..CBatchNo,;
*!*	              COLDTRNNO WITH &lc_TrnHd..cTranNo
*!*	
      REPLACE CBATCHNO  WITH IIF(llTrnBefor,lcBefor_Bat_No,lcAfter_Bat_No),;
              CTRANNO   WITH lcTranNo,;
              CDESCRIP  WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Reclacified_from_the,loFormSet.GetHeaderText("LANG_Reclacified_from_the",loFormSet.HeaderAlias))+;
                             IIF(llFromPost,;
                             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_posted_transaction_header_file,loFormSet.GetHeaderText("LANG_posted_transaction_header_file",loFormSet.HeaderAlias)),;
                             IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_transaction_header_file,loFormSet.GetHeaderText("LANG_transaction_header_file",loFormSet.HeaderAlias))),;
              COLDBATNO WITH &lc_TrnHd..CBatchNo,;
              COLDTRNNO WITH &lc_TrnHd..cTranNo
      *N000682,1 04/17/2013 RAS Globalization[End  ]

      SELECT(lc_TrnHd)

      ** Replace the status of the over lapping transaction with
      ** 'R'eclassified.

      REPLACE cTrnStat  WITH 'R',;
              cAdd_User WITH oAriaApplication.User_ID,;
              dAdd_Date WITH DATE(),;
              cAdd_Time WITH gfGetTime()

      llCreatTrn = .T.   && Flag to indicate that the batch have Tran.

      IF llTrnBefor
        ** Calculate the total debit & credit for the befor batchs.
        lnBeforTotBatDr = lnBeforTotBatDr + &lc_TrnHd..nTrnTotDr
        lnBeforTotBatCr = lnBeforTotBatCr + &lc_TrnHd..nTrnTotCr
      ELSE
        ** Calculate the total debit & credit for the after batchs.
        lnAfterTotBatDr = lnAfterTotBatDr + &lc_TrnHd..nTrnTotDr
        lnAfterTotBatCr = lnAfterTotBatCr + &lc_TrnHd..nTrnTotCr
      ENDIF
    ENDSCAN

    ** We are going to assign a new befor batch no if the batch status
    ** is 'P'osted & the variable of the befor batch no is empty or
    ** the variable of the befor batch no is empty.

    SELECT(lcBatch)
    IF cBatStat <> 'P' AND EMPTY(lcBefor_Bat_No)
      ** Generate a new befor batch no for the transactions that liy
      ** befor the closing date.

      lcBefor_Bat_No = gfSequence('CBATCHNO' , loFormSet.laCompany[loFormSet.puCompany,2])

    ENDIF

    ** If the befor batch no is generated so we are going to add a new
    ** befor batch.
    IF !EMPTY(lcBefor_Bat_No)
      DO CASE
        ** In this case we are going to check if the batch is
        ** empty or out of balance.

        CASE lnBeforTotBatDr = 0 .AND. lnBeforTotBatCr = 0
          IF llCreatTrn
            lcBatStat = 'O'
          ELSE
            lcBatStat = 'E'
          ENDIF

        ** In this case we are going to check the batch status. If
        ** 'A'pproved, 'H'old, 'P'osted, samari'Z'ed OR 'U'nposted.
        CASE &lcBatch..CBATSTAT $ 'AHPZU'
          lcBatStat = &lcBatch..cBatStat

        ** In this case we are going to check the batch status. if
        ** 'O'ut of order.
        CASE &lcBatch..cBatStat = 'O'
          lcBatStat = IIF(lnBeforTotBatDr = lnBeforTotBatCr,'U','O')
      ENDCASE

      SELECT(lcBatch)
      SCATTER MEMVAR MEMO
      m.cBatchNo  = lcBefor_Bat_No
      m.cBatStat  = lcBatStat
      m.cBatPyr   = loFormSet.laCompany[loFormSet.puCompany,4]
      m.dBatPEnd  = ldEndDate
      m.nBatCnTot = IIF(lnBeforTotBatDr >= lnBeforTotBatCr,;
                        lnBeforTotBatDr,lnBeforTotBatCr)
      m.nBaTotDr  = lnBeforTotBatDr
      m.nBaTotCr  = lnBeforTotBatCr
      m.cAdd_User = oAriaApplication.User_ID
      m.dAdd_Date = DATE()
      m.cAdd_Time = gfGetTime()

      SELECT(lcTmpBatch)
      APPEND BLANK
      GATHER MEMVAR MEMO

      SELECT(lcTmpReport)
      APPEND BLANK
      REPLACE CBATCHNO  WITH lcBefor_Bat_No,;
              CTRANNO   WITH ' ',;
              CDESCRIP  WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTRclasfi,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTRclasfi",loFormSet.HeaderAlias)),;
              COLDBATNO WITH &lcBatch..CBatchNo,;
              COLDTRNNO WITH ' '

      SELECT(lcBatch)
    ENDIF

    SELECT(lcBatch)
    IF cBatStat <> 'P' AND EMPTY(lcAfter_Bat_No)
      ** Generate a new befor batch no for the transactions that liy
      ** after the closing date.

      lcAfter_Bat_No = gfSequence('CBATCHNO' , loFormSet.laCompany[loFormSet.puCompany,2])

    ENDIF

    IF !EMPTY(lcAfter_Bat_No)
      DO CASE
        ** In this case we are going to check if the batch is
        ** empty or out of balance.
        CASE lnBeforTotBatDr = 0 .AND. lnBeforTotBatCr = 0
          IF llCreatTrn
            lcBatStat = 'O'
          ELSE
            lcBatStat = 'E'
          ENDIF

        ** In this case we are going to check the batch status. If
        ** 'A'pproved, 'H'old, 'P'osted, samari'Z'ed OR 'U'nposted.
        CASE &lcBatch..CBATSTAT $ 'AHPZU'
          lcBatStat = &lcBatch..cBatStat

        ** In this case we are going to check the batch status. if
        ** 'O'ut of order.
        CASE &lcBatch..cBatStat = 'O'
          lcBatStat = IIF(lnBeforTotBatDr = lnBeforTotBatCr,'U','O')
      ENDCASE

      llCreatTrn = .F.

      ** Store the batch information in memory variables to append
      ** a new batch in the same file.

      SELECT(lcBatch)
      SCATTER MEMVAR MEMO
      m.cBatchNo  = lcAfter_Bat_No
      m.cBatStat  = lcBatStat
      m.cBatPYr   = lcFisYear
      m.dBatPBeg  = ldEndDate + 1
      m.dBatPEnd  = IIF(llClosDate,loFormSet.laCompany[loFormSet.puCompany,7],ldOldNxtDat)
      m.nBatCNTot = IIF(lnAfterTotBatDr >= lnAfterTotBatCr,;
                       lnAfterTotBatDr,lnAfterTotBatCr)
      m.nBaTotDr  = lnAfterTotBatDr
      m.nBaTotCr  = lnAfterTotBatCr
      m.cAdd_User = oAriaApplication.User_ID
      m.dAdd_Date = DATE()
      m.cAdd_Time = gfGetTime()

      SELECT(lcTmpBatch)
      APPEND BLANK
      GATHER MEMVAR MEMO

      SELECT(lcTmpReport)
      APPEND BLANK
      REPLACE CBATCHNO  WITH lcAfter_Bat_No,;
              CTRANNO   WITH ' ',;
              CDESCRIP  WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTRclasfi,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTRclasfi",loFormSet.HeaderAlias)),;
              COLDBATNO WITH &lcBatch..CBatchNo,;
              COLDTRNNO WITH ' '

      SELECT(lcBatch)
    ENDIF

    llCreatTrn= .F.

    SELECT(lcBatch)
    ** Replace the old batch with status 'R'eclassified.
    REPLACE cBatStat  WITH 'R',;
            cComp_Id  WITH loFormSet.laCompany[loFormSet.puCompany,2],;
            cAdd_User WITH oAriaApplication.User_ID,;
            dAdd_Date WITH DATE(),;
            cAdd_Time WITH gfGetTime()
  ENDIF
ENDSCAN

SELECT(lcCurArea)  && Restore the calling work area.

*!**************************************************************************
*!
*!      Function : lfRpstBtTr
*!
*!**************************************************************************
*
FUNCTION lfRpstBtTr
PARAMETERS ldRePostDate

** We are going to call this function If the closing date of the last
** period change.

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_SMCLOSE_lcTRPostBt NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTRPostBt,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTRPostBt",loFormSet.HeaderAlias)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]


** Lock the balance, posted transaction details transaction details file.

IF gfFLOCK("&lcAcBals.,&lcPTrnDt.",.T.)

  SELECT(lcBatch)
  SET RELATION TO
  SET RELATION TO CBATCHNO INTO &lcPTrnDt ADDITIVE

  SELECT(lcPTrnDt)
  SET RELATION TO CACCTCODE+CTRNPYR+CTRNPPRD INTO &lcAcBals ADDITIVE
  SET RELATION TO CACCTCODE INTO &lcAcChar ADDITIVE
  SET RELATION TO CBATCHNO+CTRANNO INTO &lcPTrnHd ADDITIVE

  ** Scanning the batch file for the batch begin postin date greater
  ** than the reposting date and the batch status is equal 'P'osted.

  *E303343,1 TMI 01/20/2013 [Start]
  SELECT(lcBatch)
  lnTot = 100
  LOCATE
  COUNT TO lnTot FOR &lcBatch..dBatPBeg > ldRePostDate ;
       AND &lcBatch..cBatStat = 'P'
  lnCnt = 1

  *E303343,1 TMI 01/20/2013 [End  ]

  SELECT(lcBatch)
  SCAN FOR &lcBatch..dBatPBeg > ldRePostDate ;
       AND &lcBatch..cBatStat = 'P'

    *E303343,1 TMI 01/20/2013 [Start]
    lnCnt = lnCnt+1
    *E303343,1 TMI 01/20/2013 [End  ]

    ** If the batch type equal 'B'eginning balance batch(es).
    IF &lcBatch..cBatType = 'B'
      ** We are going to assign the post type with 'B'.
      lcPostType = 'B'
    ELSE   && If the batch type equal 'N'onbeginning balance batch(es).
      ** We are going to assign the post type with 'N'ormal.
      lcPostType = 'N'
    ENDIF

    ** We are going to scan the posted transaction detai file for the
    ** transaction batch no equal the batch no of the batch file.

    SELECT(lcPTrnDt)
    lnTermCount = 1
    SCAN REST WHILE cBatchNo = &lcBatch..cBatchNo
      DO lpUpd_Bal WITH lcPostType
    ENDSCAN
  ENDSCAN

  SELECT(lcBatch)
  SET RELATION TO

  ** We are going to assign the post type with 'T'ransactions.
  lcPostType = 'T'

  ** We are going to scan the transaction detail file for the transaction
  ** posted date greater than the reposting date and the batch no equal 0
  ** and the transaction stat equal 'P'osted.

  SELECT(lcPTrnDt)
  SCAN FOR dTrnPDate > ldRePostDate;
       AND cBatchNo = '000000';
       AND &lcPTrnHd..cTrnStat = 'P'
    DO lpUpd_Bal WITH lcPostType
  ENDSCAN

  SELECT(lcPTrnDt)
  SET RELATION TO

  =gfFLOCK("&lcAcBals.,&lcPTrnDt.",.F.)
ELSE
  loFormSet.laCompany[loFormSet.puCompany,9] = 'L'
  =gfFLOCK("&lcAcBals.,&lcPTrnDt.",.F.)
ENDIF

WAIT CLEAR

*!**************************************************************************
*!
*!      FUNCTION : lpUpd_Bal
*!
*!**************************************************************************
*
*  This FUNCTION performs the actual update of amounts to the
*  lcAcBals file from the posted transactions detais file.
*  parameter       : variable containing the posting type,
*                    whether : 'T' = transaction(s)
*                              'B' = beginning balance batch(es)
*                              'N' = nonbeginning balance batch(es)
*
FUNCTION lpUpd_Bal
PARAMETERS lcPostType

*E303343,1 TMI 01/20/2013 [Start] add an indicator while updating
*WAIT WINDOW NOWAIT LANG_SMCLOSE_lcTUpdBal
lnTermCount = lnTermCount + 1
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW NOWAIT LANG_SMCLOSE_lcTUpdBal + SUBSTR(lcSpcChr,MOD(lnTermCount,4)+1,1)
WAIT WINDOW NOWAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTUpdBal,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTUpdBal",loFormSet.HeaderAlias)) + SUBSTR(lcSpcChr,MOD(lnTermCount,4)+1,1)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*E303343,1 TMI 01/20/2013 [End  ]

PRIVATE lcAcctCode,lcFisFYear,lcAcBalsEx,lcCurArea

** Save the current work area.
lcCurArea = ALIAS()

SELECT(lcAcBals)

** Current index expression for tag 'ACCYRPRD' is :
** cAcctCode+cFisFYEar+cFspPrdId

lcAcBalsEx = SYS(14,VAL(SYS(21)))
lcAcctCode = &lcPTrnDt..cAcctCode
lcFisFYear = &lcPTrnDt..cTrnPYr

DO CASE

  ** Case posting transaction(s), or batch(es) of types 'N'ormal,
  ** 'S'tatistical, or sub'L'edger (non beginning).
  CASE lcPostType $ 'TN'
    ** If the entry is a debit :
    IF &lcPTrnDt..cDrOrCr = 'D'
      ** Update the current account code + year + period,

      ** Update period to date debit field (GLACBALS.nAcBPtdDr).
      ** Update year to date debit field (GLACBALS.nAcBYtdDr).
      ** Update closing balance field (GLACBALS.nAcBClBal).

      REPLACE &lcAcBals..nAcBPtdDr WITH &lcAcBals..nAcBPtdDr +;
                                                   &lcPTrnDt..nAmount,;
                         &lcAcBals..nAcBYtdDr WITH &lcAcBals..nAcBYtdDr +;
                                                   &lcPTrnDt..nAmount,;
                         &lcAcBals..nAcBClBal WITH &lcAcBals..nAcBClBal +;
                                                   &lcPTrnDt..nAmount,;
                         &lcAcBals..cAdd_User WITH oAriaApplication.User_ID,;
                         &lcAcBals..dAdd_Date WITH DATE(),;
                         &lcAcBals..cAdd_Time WITH gfGetTime()

      ** Update the following periods for the same account code
      ** and year.
      IF !EOF()
        SKIP
        ** Update year to date debit field (GLACBALS.nAcBYtdDr)
        ** Update openning balance field (GLACBALS.nAcBOpBal)
        ** Update closing balance field (GLACBALS.nAcBClBal)
        REPLACE REST WHILE &lcAcBalsEx. = lcAcctCode+lcFisfYear;
                           &lcAcBals..nAcBYtdDr WITH &lcAcBals..nAcBYtdDr +;
                                                     &lcPTrnDt..nAmount,;
                           &lcAcBals..nAcBOpBal WITH &lcAcBals..nAcBOpBal +;
                                                     &lcPTrnDt..nAmount,;
                           &lcAcBals..nAcBClBal WITH &lcAcBals..nAcBClBal +;
                                                     &lcPTrnDt..nAmount,;
                           &lcAcBals..cAdd_User WITH oAriaApplication.User_ID,;
                           &lcAcBals..dAdd_Date WITH DATE(),;
                           &lcAcBals..cAdd_Time WITH gfGetTime()

        ** Update the following years for the same account code
        ** if the current account type is 'A'ssets, 'L'iability,
        ** e'Q'uity, or statistical ('Y').

        IF LEFT(&lcAcChar..cTypeCode,1) $ 'ALQY'

          ** Update open balance field (GLACBALS.nAcBOpBal).
          ** Update closing balance field (GLACBALS.nAcBClBal).

          REPLACE REST WHILE &lcAcBalsEx. = lcAcctCode ;
                             &lcAcBals..nAcBOpBal WITH &lcAcBals..nAcBOpBal +;
                                                       &lcPTrnDt..nAmount,;
                             &lcAcBals..nAcBClBal WITH &lcAcBals..nAcBClBal +;
                                                       &lcPTrnDt..nAmount,;
                             &lcAcBals..cAdd_User WITH oAriaApplication.User_ID,;
                             &lcAcBals..dAdd_Date WITH DATE(),;
                             &lcAcBals..cAdd_Time WITH gfGetTime()
        ENDIF
      ENDIF
    ELSE    && If it is a credit.

      ** Update the current account code + year + period.
      ** Update period to date credit field (GLACBALS.nAcBPtdCr).
      ** Update year to date credit field (GLACBALS.nAcBYtdCr).
      ** Update closing balance field (GLACBALS.nAcBClBal).

      REPLACE &lcAcBals..nAcBPtdCr WITH &lcAcBals..nAcBPtdCr +;
                                        &lcPTrnDt..nAmount,;
              &lcAcBals..nAcBYtdCr WITH &lcAcBals..nAcBYtdCr +;
                                        &lcPTrnDt..nAmount,;
              &lcAcBals..nAcBClBal WITH &lcAcBals..nAcBClBal -;
                                        &lcPTrnDt..nAmount,;
              &lcAcBals..cAdd_User WITH oAriaApplication.User_ID,;
              &lcAcBals..dAdd_Date WITH DATE(),;
              &lcAcBals..cAdd_Time WITH gfGetTime()

      ** Update the following periods for the same account code
      ** and year
      IF !EOF()
        SKIP
        ** Update year to date debit field (GLACBALS.nAcBYtdDr)
        ** Update openning balance field (GLACBALS.nAcBOpBal)
        ** Update closing balance field (GLACBALS.nAcBClBal)

        REPLACE REST WHILE &lcAcBalsEx. = lcAcctCode+lcFisfYear;
                           &lcAcBals..nAcBYtdCr WITH &lcAcBals..nAcBYtdCr +;
                                                     &lcPTrnDt..nAmount,;
                           &lcAcBals..nAcBOpBal WITH &lcAcBals..nAcBOpBal -;
                                                     &lcPTrnDt..nAmount,;
                           &lcAcBals..nAcBClBal WITH &lcAcBals..nAcBClBal -;
                                                     &lcPTrnDt..nAmount,;
                           &lcAcBals..cAdd_User WITH oAriaApplication.User_ID,;
                           &lcAcBals..dAdd_Date WITH DATE(),;
                           &lcAcBals..cAdd_Time WITH gfGetTime()

        ** Update the following years for the same account code.
        ** if the current account type is 'A'ssets, 'L'iability,
        ** e'Q'uity, or statistical ('Y').

        IF LEFT(&lcAcChar..cTypeCode,1) $ 'ALQY'
          ** Update open balance field (GLACBALS.nAcBOpBal).
          ** Update closing balance field (GLACBALS.nAcBClBal).

          REPLACE REST WHILE &lcAcBalsEx. = lcAcctCode ;
                             &lcAcBals..nAcBOpBal WITH &lcAcBals..nAcBOpBal -;
                                                       &lcPTrnDt..nAmount,;
                             &lcAcBals..nAcBClBal WITH &lcAcBals..nAcBClBal -;
                                                       &lcPTrnDt..nAmount,;
                             &lcAcBals..cAdd_User WITH oAriaApplication.User_ID,;
                             &lcAcBals..dAdd_Date WITH DATE(),;
                             &lcAcBals..cAdd_Time WITH gfGetTime()
        ENDIF
      ENDIF
    ENDIF

  ** Case posting beginning balance batch(es)
  CASE lcPostType = 'B'
    ** If the entry is a debit.
    IF &lcPTrnDt..cDrOrCr = 'D'
      ** Update the current, and the following periods for the
      ** same year.

      ** Update opening balance field (GLACBALS.nAcBOpBal).
      ** Update closing balance field (GLACBALS.nAcBClBal).

      REPLACE REST WHILE &lcAcBalsEx. = lcAcctCode + lcFisfYear;
                         &lcAcBals..nAcBOpBal WITH &lcAcBals..nAcBOpBal +;
                                                   &lcPTrnDt..nAmount,;
                         &lcAcBals..nAcBClBal WITH &lcAcBals..nAcBClBal +;
                                                   &lcPTrnDt..nAmount,;
                         &lcAcBals..cAdd_User WITH oAriaApplication.User_ID,;
                         &lcAcBals..dAdd_Date WITH DATE(),;
                         &lcAcBals..cAdd_Time WITH gfGetTime()


      ** Update the following years for the same account code
      ** if the current account type is 'A'ssets, 'L'iability,
      ** e'Q'uity, or statistical ('Y')

      IF LEFT(&lcAcChar..cTypeCode,1) $ 'ALQY'
        ** Update open balance field (GLACBALS.nAcBOpBal).
        ** Update closing balance field (GLACBALS.nAcBClBal).

        REPLACE REST WHILE &lcAcBalsEx. = lcAcctCode ;
                           &lcAcBals..nAcBOpBal WITH &lcAcBals..nAcBOpBal +;
                                                     &lcPTrnDt..nAmount,;
                           &lcAcBals..nAcBClBal WITH &lcAcBals..nAcBClBal + ;
                                                     &lcPTrnDt..nAmount,;
                           &lcAcBals..cAdd_User WITH oAriaApplication.User_ID,;
                           &lcAcBals..dAdd_Date WITH DATE(),;
                           &lcAcBals..cAdd_Time WITH gfGetTime()
      ENDIF

    ELSE    && If the entry is a credit.

      ** Update the current, and the following periods for the
      ** same year
      ** Update opening balance field (GLACBALS.nAcBOpBal).
      ** Update closing balance field (GLACBALS.nAcBClBal).

      REPLACE REST WHILE &lcAcBalsEx. = lcAcctCode + lcFisFYear ;
                         &lcAcBals..nAcBOpBal WITH &lcAcBals..nAcBOpBal -;
                                                   &lcPTrnDt..nAmount,;
                         &lcAcBals..nAcBClBal WITH &lcAcBals..nAcBClBal -;
                                                   &lcPTrnDt..nAmount,;
                         &lcAcBals..cAdd_User WITH oAriaApplication.User_ID,;
                         &lcAcBals..dAdd_Date WITH DATE(),;
                         &lcAcBals..cAdd_Time WITH gfGetTime()

      ** Update the following years for the same account code
      ** if the current account type is 'A'ssets, 'L'iability,
      ** e'Q'uity, or statistical ('Y')

      IF LEFT(&lcAcChar..cTypeCode,1) $ 'ALQY'
        ** Update open balance field (GLACBALS.nAcBOpBal).
        ** Update closing balance field (GLACBALS.nAcBClBal).

        REPLACE REST WHILE &lcAcBalsEx. = lcAcctCode ;
                           &lcAcBals..nAcBOpBal WITH &lcAcBals..nAcBOpBal -;
                                                     &lcPTrnDt..nAmount,;
                           &lcAcBals..nAcBClBal WITH &lcAcBals..nAcBClBal - ;
                                                     &lcPTrnDt..nAmount,;
                           &lcAcBals..cAdd_User WITH oAriaApplication.User_ID,;
                           &lcAcBals..dAdd_Date WITH DATE(),;
                           &lcAcBals..cAdd_Time WITH gfGetTime()
      ENDIF
   	ENDIF
ENDCASE

** Restore the calling work area.
SELECT(lcCurArea)

*!**************************************************************************
*!
*!      Function : lfGpstClsEn
*!
*!**************************************************************************
*
FUNCTION lfGpstClsEn

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_SMCLOSE_lcTGnClsEn NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTGnClsEn,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTGnClsEn",loFormSet.HeaderAlias)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]


PRIVATE lcAcctCode,lcRetAcct,lcCostCntr,lcTmpCstCt,;
        lnStartPos,lnAmntWdth,lnAcctWdth

IF gfFLOCK("&lcTrnsDt.,&lcTrnsHd,&lcBatch.",.T.)

  ** field size of closing balance field in GLACBALS.
  lnAmntWdth = FSIZE('nAcbClBal','&lcAcBals')

  ** field size of account code field in GLACCHAR.
  lnAcctWdth = FSIZE('cAcctCode','&lcAcChar')

  ** Get unique names for the temporary files

  IF USED(lcTmpBatch)
    SELECT(lcTmpBatch)
    ZAP
  ENDIF

  IF USED(lcTmpTrnHd)
    SELECT(lcTmpTrnHd)
    ZAP
  ENDIF

  IF USED(lcTmpTrnDt)
    SELECT(lcTmpTrnDt)
    ZAP
  ENDIF

  ** Temporary files creation.

  ** 1.Closing entries file (lcTmpClsEnt) has the following file structure:
  **   field name    Type       Width      Decimal   Comment
  **   cCostCntr       C      lnAcctWdth     --      same as cAcctCode
  **   cAcctCode       C      lnAcctWdth     --
  **   nAmount         N      lnAmntWdth      2
  **   cDrOrCr         C          1          --      'D'ebit or 'C'redit

  CREATE TABLE &gcWorkDir.&lcTmpClsEnt;
               (cCostCntr C(lnAcctWdth),cAcctCode C(lnAcctWdth),;
                nAmount N(lnAmntWdth,2),cDrOrCr C(1))

  SELECT(lcTmpClsEnt)
  INDEX ON cCostCntr + cAcctCode TAG CostCtAcct
  SET ORDER TO TAG CostCtAcct

  ** The chart of accounts file need to be openned again in
  ** another work area, with master tag = 'ACCTCODE'.
  SELECT 0
  USE &lcDataDir.GLACCHAR AGAIN ALIAS &lcTmpAcChar2 ORDER ACCTCODE

  SELECT(lcAcBals)
  lcSavOrder = SET('ORDER')
  SET ORDER TO TAG ACCYRPRD

  SELECT(lcAcChar)
  ** Set a filter on accounts of types in 'E','I','S','T','C'.
  SET FILTER TO LEFT(&lcAcChar..cTypeCode,1) $ 'EISTC'

  ** Set a relation between GLACCHAR (parent) and GLACBALS (child)
  ** on : account code + Closing year + last period in the closing year.
  ** (1-1 relation).

  SET RELATION TO cAcctCode+loFormSet.laCompany[loFormSet.puCompany,4]+loFormSet.laCompany[loFormSet.puCompany,3];
             INTO &lcAcBals ADDITIVE

  ** Scan through all the accounts of types 'E', 'I', 'S', 'T', 'C'
  ** having a closing balance.

  SCAN FOR &lcAcBals..nAcbClBal <> 0

    ** Get the retained earnings account for the current account code.
    ** 1. Get the cost center for the current account.

    lcAcctCode = IIF(' ' $ &lcAcChar..cAcctCode,;
                    STRTRAN(&lcAcChar..cAcctCode,' ','-',1,1),;
                    PADR(&lcAcChar..cAcctCode,LEN(&lcAcChar..cAcctCode)+;
                    1,'-'))
    lnStartPos = AT('-',lcAcctCode)
    lcCostCntr = SUBSTR(lcAcctCode,lnStartPos+1,;
                     AT('-',lcAcctCode,&lcSetup..nSetCostC)-lnStartPos-1)

    ** 2. Then build the corresponding retained earnings account code.
    lcRetAccnt = lcDfRetAct
    lcRetAccnt = STUFF(lcRetAccnt,lnStartPos+1,LEN(lcCostCntr),lcCostCntr)

    ** 3. Check if the retained earnings account exist in the chart
    **    of accounts file, if not, build another one from the preceeding
    **    cost center, and look for it in the charts of accounts file,
    **    and so on.

    lcTmpCstCt = lcCostCntr

    DO WHILE !SEEK(lcRetAccnt,lcTmpAcChar2)
      lcTmpCstCt = SUBSTR(lcTmpCstCt,1,RAt('-',lcTmpCstCt)-1)
      lcRetAccnt = lcDfRetAct
      lcRetAccnt = STUFF(lcRetAccnt,lnStartPos+1,LEN(lcTmpCstCt),lcTmpCstCt)
    ENDDO

    ** 4. Create the closing entries temporary file (lcTmpClsEnt) as follows :

    ** 4.1 Prepare cost center variable.
    lcCostCntr = PADR(lcCostCntr,lnAcctWdth,' ')

    ** Append a new entry for the cost center value + current account code
    ** to the file as follows :
    ** - use the absolute value of lcAcBals.cAcBClBal field for the
    **   amount field ( for the closing period-year)
    ** - If lcAcBals.nAcBClBal is originally a Debit (i.e., its value
    **   is positive, store 'C'redit to cDrOrCr field, and vice versa.

    INSERT INTO &gcWorkDir.&lcTmpClsEnt;
           (cCostCntr,cAcctCode,nAmount,cDrOrCr);
     VALUES(lcCostCntr,&lcAcChar..cAcctCode,;
            ABS(&lcAcBals..nAcbClBAl),;
            IIF(&lcAcBals..nAcbClBAl > 0,'C','D'))

    ** Look for the cost center value + retained earnings account code
    ** If found, update it as follows, else append the current record
    ** to the file as shown.
    ** - use the absolute value of lcAcBals.cAcBClBal field for the
    **   amount field ( for the closing period-year)
    ** - If lcAcBals.nAcBClBal is originally a Debit (i.e., its value
    **   is positive, store 'D'ebit to cDrOrCr field, and vice versa.

    IF SEEK(lcCostCntr+lcRetAccnt,lcTmpClsEnt)
      lnAmount = IIF(&lcTmpClsEnt..cDrOrCr='D',;
                     &lcTmpClsEnt..nAmount,0 - &lcTmpClsEnt..nAmount)+;
                     &lcAcBals..nAcbClBAl
      REPLACE &lcTmpClsEnt..nAmount WITH ABS(lnAmount),;
              &lcTmpClsEnt..cDrOrCr WITH IIF(lnAmount > 0,'D','C')
    ELSE
      INSERT INTO &gcWorkDir.&lcTmpClsEnt;
             (cCostCntr,cAcctCode,nAmount,cDrOrCr);
       VALUES(lcCostCntr,lcRetAccnt,;
              ABS(&lcAcBals..nAcbClBAl),;
              IIF(&lcAcBals..nAcbClBAl > 0,'D','C'))
    ENDIF
  ENDSCAN

  ** Build the temporary files holding the closing batch, (lcTmpBatch)
  ** its transactions (lcTmpTrnHd), and their details (lcTmpTrnDt)
  ** by calling lfCreatCls function.

  IF RECCOUNT(lcTmpClsEnt) > 0
    =lfCreatCls()
    =gfFLOCK("&lcTrnsDt.,&lcTrnsHd,&lcBatch.",.F.)

    ** Prepare variables for the posting program. ***

    ** Variable to hold the numeric value of the new current year.
    loFormSet.lnCurr_Yer = INT(VAL(loFormSet.laCompany[loFormSet.puCompany,4])+1)

    ** Variable to hold the character value of the new current year.
    loFormSet.lcCurr_Yer = STR(loFormSet.lnCurr_Yer,4)

    ** Calling the posting function with type batch that need to be posted,
    ** the temp file name that hold the batch no,the program name that call
    ** the posting program,the active company,the data dir of the selected
    ** company.
     *E303343,1 TMI 01/16/2013 [Start]
     *DO (gcAppHome+"GL.APP") WITH "LFTBPOST WITH 'BATCH',lcTmpBatch,'CLOSING',' ',loFormSet.laCompany[loFormSet.puCompany,2],lcDataDir",'',"T"
     =lfOpenTbls4Post(loFormSet)

     =LFTBPOST(loFormSet,'BATCH',lcTmpBatch,'CLOSING',' ',loFormSet.laCompany[loFormSet.puCompany,2],lcDataDir,.F.)
     *E303343,1 TMI 01/16/2013 [End  ]

     =lfCloseTbls4Post(loFormSet)
  ENDIF

  SELECT(lcAcChar)
  SET RELATION TO
  SET FILTER TO

  IF USED(lcTmpAcChar2)
    USE IN &lcTmpAcChar2
  ENDIF

  SELECT(lcAcBals)
  SET ORDER TO &lcSavOrder
  =gfFLOCK("&lcTrnsDt.,&lcTrnsHd,&lcBatch.",.F.)
ELSE
  loFormSet.laCompany[loFormSet.puCompany,9] = 'L'
  =gfFLOCK("&lcTrnsDt.,&lcTrnsHd,&lcBatch.",.F.)
ENDIF

WAIT CLEAR
************************************************************
*! Name      : lfOpenTbls4Post
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/17/2013
*! Purpose   : Open the tables needed for the post
************************************************************
FUNCTION lfOpenTbls4Post
PARAMETERS loFormSet
LOCAL lcFiles,lcTags
LOCAL i,lcPath

DIMENSION laTbl4Post[1],laTag4Post[1]
STORE '' TO laTbl,laTag

*- add the following files to the arrays laTbl and laTag using the function lfAddFl
lnCnt = 1
lfAddFl('GLACCHAR','ACCTCODE'  )
lfAddFl('GLSEGVAL','ACSSEGVAL' )
lfAddFl('GLACBALS','ACCYRPRD'  )
lfAddFl('GLTYPES' ,'TYPECODE'  )
lfAddFl('GLTRNSHD','BATCHTRN'  )
lfAddFl('GLTRNSDT','BATCHTRN'  )
lfAddFl('GLPTRNDT','BATCHTRN'  )
lfAddFl('GLPTRNHD','BATCHTRN'  )
lfAddFl('GLSUBJOR','SRCJRNL'   )
lfAddFl('GLCFITEM','CFICODE'   )
lfAddFl('GLRACOD' ,'TYPRATIO'  )
lfAddFl('SYDAPPL' ,'CAPP_ID'   )
lfAddFl('GLAUTHD' ,'TYPECODE'  )
lfAddFl('GLAUTDT' ,'TYPCODACC' )
lfAddFl('GLSETUP' ,'GLSETUP'   )
lfAddFl('GLGRPHD' ,'GRPCODE'   )
lfAddFl('GLGRPDT' ,'ACCTCODE'  )
lfAddFl('GLBUDDT' ,'ACCTCODE'  )
lfAddFl('GLBUDHD' ,'BDCODYR'   )
lfAddFl('SYDFIELD','CFLD_NAME' )
lfAddFl('ACCOD'   ,'ACCSEGNO'  )
lfAddFl('FISHD'   ,'COMPFYEAR' )
lfAddFl('FSPRD'   ,'COMFYRPRDI')
lfAddFl('SYCCOMP' ,'CCOMP_ID'  )
lfAddFl('SYGLTRAN','SYGLTRAN'  )
lfAddFl('GLBATCH' ,'BATCHNO'   )

*- add the property if not found
IF TYPE('loFormSet.laOpenTbls4Post')='U'
  loFormSet.AddProperty('laOpenTbls4Post[1,3]')
ENDIF

lcPath = UPPER(ADDBS(ALLTRIM(loFormSet.laCompany[loFormSet.puCompany,8])))

DIMENSION loFormSet.laOpenTbls4Post[ALEN(laTbl4Post,1),6]
loFormSet.laOpenTbls4Post = .F.
FOR i = 1 TO ALEN(laTbl4Post,1)
  loFormSet.laOpenTbls4Post[i,1] = UPPER(laTbl4Post[i])
  loFormSet.laOpenTbls4Post[i,2] = UPPER(laTag4Post[i])
  IF USED(laTbl4Post[i]) AND LEFT(laTbl4Post[i],2)<>'SY' AND !lcPath $ DBF(laTbl4Post[i])
    loFormSet.laOpenTbls4Post[i,4] = .T.
    loFormSet.laOpenTbls4Post[i,5] = DBF(laTbl4Post[i])
    loFormSet.laOpenTbls4Post[i,6] = order(laTbl4Post[i])
    USE IN laTbl4Post[i]
  ENDIF
  WAIT WINDOW NOWAIT laTbl4Post[i]
  IF !USED(laTbl4Post[i])
    IF LEFT(laTbl4Post[i],2) = 'SY'
      =gfOpenFile(oAriaApplication.SysPath+laTbl4Post[i],laTag4Post[i],'SH')
    ELSE
      && open files with the buffering set.
      =gfOpenTable(lcPath+laTbl4Post[i],laTag4Post[i],'SH')
      *SELECT (laTbl[i])
      *CURSORSETPROP("Buffering",5)
    ENDIF
    loFormSet.laOpenTbls4Post[i,3] = .T.
  ENDIF
ENDFOR
*- End of lfOpenTbls4Post.

************************************************************
*! Name      : lfAddFl
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/17/2013
*! Purpose   : add files to the arrays laTbl4Post and laTag4Post using the function lfAddFl
************************************************************
FUNCTION lfAddFl
PARAMETERS lcFl,lcTag
DIMENSION laTbl4Post[lnCnt],laTag4Post[lnCnt]
laTbl4Post[lnCnt] = lcFl
laTag4Post[lnCnt] = lcTag
lnCnt = lnCnt + 1

*- End of lfAddFl.
************************************************************
*! Name      : lfCloseTbls4Post
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 01/17/2013
*! Purpose   : close the tables used specifically for the post process
************************************************************
FUNCTION lfCloseTbls4Post
PARAMETERS loFormSet
LOCAL i,lnSlct,lcTag
lnSlct = SELECT(0)
FOR i = 1 TO ALEN(loFormSet.laOpenTbls4Post,1)
  IF !EMPTY(loFormSet.laOpenTbls4Post[i,1]) AND loFormSet.laOpenTbls4Post[i,3]
    *B611023,1 MMT 07/02/2015 Close Period screen gives error when user changes company[T20150630.0011][Start]
    *USE IN (loFormSet.laOpenTbls4Post[i,1])
    IF (LEFT(loFormSet.laOpenTbls4Post[i,1],2) ='SY')
      USE IN (loFormSet.laOpenTbls4Post[i,1])
    ELSE
      =gfCloseTable(loFormSet.laOpenTbls4Post[i,1])
    ENDIF  
    *B611023,1 MMT 07/02/2015 Close Period screen gives error when user changes company[T20150630.0011][End]
    IF loFormSet.laOpenTbls4Post[i,4]
      *E303343,1 TMI 01/20/2013 [Start]
      *USE (loFormSet.laOpenTbls4Post[i,5]) IN 0
      *SELECT (loFormSet.laOpenTbls4Post[i,5])
      SELECT 0
      USE (loFormSet.laOpenTbls4Post[i,5])
      *E303343,1 TMI 01/20/2013 [End  ]
      lcTag = loFormSet.laOpenTbls4Post[i,6]
      SET ORDER TO &lcTag
    ENDIF
  ENDIF
ENDFOR
SELECT (lnSlct )
*- End of lfCloseTbls4Post.

*!**************************************************************************
*!
*!      Function : lfCreatCls
*!
*!**************************************************************************
*  Creates a closing entries batch ,its transactions, and transaction
*  details from the previously created temporary file lcTmpClsEnt.
FUNCTION lfCreatCls

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_SMCLOSE_lcTCrClsEn NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTCrClsEn,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTCrClsEn",loFormSet.HeaderAlias)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]


PRIVATE lcTmpCstCt,lcBatchNo,lcTranNo,lcClosNo,;
        lnTotBatDr,lnTotBatCr,lnTotTrnDr,lnTotTrnCr,;
        lcUserName,lnBtNoWdth,lnTrNoWdth

** lcTmpCstCt : Temporary cost center variable ( for search ).
** lcBatchNo  : Holds the created batch number.
** lcTranNo   : Holds the created transactions numbers.
** lcClosNo   : Holds the created closing entry number.
** lnTotBatDr : Created batch total debit.
** lnTotBatCr : Created batch total credit.
** lnTotTrnDr : Transaction total debit.
** lnTotTrnCr : Transaction total credit.

** Closing entries are created as follows :
** All of them are recorded in a single year end 'C'losing entries
** batch, under the same generated batch number.
** Whenever the cost center is changed in the closing entries file
** (lcTmpClsEnt), a new transaction is created and all the records
** belonging to the same cost center are considered the details of
** this transaction.

lcTmpCstCt = ' '
lcSJ_Def   = 'GJ' && Default source journal, to be corrected.
lcBatchNo  = ' '  && Variable to hold the batch sequence No.
lcTranNo   = ' '  && Variable to hold the Transaction sequence No.
lcClosNo   = ' '  && Variable to hold the Closing entries No.

** Get the current user name ***
=gfOpenFile(oAriaApplication.SysPath+'SYUUSER','CUSER_ID','SH')
lcUserName = LOOKUP(SYUUSER.cUsr_Name,oAriaApplication.User_ID,SYUUSER.cUser_ID,'cUser_ID')

lnBatchNo  = 0    && Temp Batch No.
lnTranNo   = 0    && Temp Transaction No.
lnClosNo   = 0    && Temp Closing entries No.
lnTotBatDr = 0    && Reset batch's total debit.
lnTotBatCr = 0    && Reset batch's total credit.
lnTotTrnDr = 0    && Reset Transaction total debit.
lnTotTrnCr = 0    && Reset Transaction total credit.

** Reset closing entry sequence number in SYCSEQUN file.
SELECT 0
lcSeqTmpNm = gfTempName()
USE (ALLTRIM(loFormSet.laCompany[loFormSet.puCompany,8]) + 'SEQUENCE') ;
     AGAIN ;
     ALIAS (lcSeqTmpNm) ;
     ORDER TAG CSEQ_TYPE

IF SEEK('CCLOSENT')
  DELETE
ENDIF

USE

SELECT(lcTmpClsEnt)
GO TOP

SCAN
  ** If the cost center value is changed, record a new transaction

  IF lcTmpCstCt <> &lcTmpClsEnt..cCostCntr
    ** Store the current cost center value.
    lcTmpCstCt = &lcTmpClsEnt..cCostCntr
    ** Reset total debit and credit totals
    lnTotTrnDr = 0
    lnTotTrnCr = 0
  ENDIF

  ** Process all the records having the same cost center value.

  SCAN REST WHILE &lcTmpClsEnt..cCostCntr = lcTmpCstCt

    ** Do not allow any line in the transaction details to have
    ** a zero amount, hence, filter out those records in the
    ** closing entries file with zero amounts.

    IF &lcTmpClsEnt..nAmount <> 0.00

      ** Append a new line in the temporary transactions details file.

      SELECT(lcTmpTrnDt)

      APPEND BLANK
      REPLACE cAcctCode WITH &lcTmpClsEnt..cAcctCode,;
              cDrOrCr   WITH &lcTmpClsEnt..cDrOrCr,;
              nAmount   WITH &lcTmpClsEnt..nAmount,;
              dTrnPDate WITH loFormSet.laCompany[loFormSet.puCompany,10],;
              cTrnPYr   WITH loFormSet.laCompany[loFormSet.puCompany,4],;
              cTrnPPrd  WITH loFormSet.laCompany[loFormSet.puCompany,3]

      IF &lcTmpTrnDt..cDrOrCr = 'D'
        lnTotTrnDr = lnTotTrnDr + &lcTmpTrnDt..nAmount
      ELSE
        lnTotTrnCr = lnTotTrnCr + &lcTmpTrnDt..nAmount
      ENDIF

      SELECT(lcTmpClsEnt)
    ENDIF
  ENDSCAN

  ** SCAN..ENDSCAN increments the record pointer automatically,
  ** so, go back one record so as not to lose the last one.

  SKIP -1

  ** Append a new line in the temporary transactions header file.

  ** Do not allow any line in the transaction header to have zero
  ** total debit or credit values.

  ** Checking total debit value is enough since both total
  ** values are equal.

  IF lnTotTrnDr <> 0

    ** Having checked that there are entries for this transaction
    ** and hence, a transaction header and a batch are to be created,
    ** proceed with creation of a batch number, a transaction number
    ** and a closing entry number.

    ** If a batch number hasn't already been created,
    ** generate a new batch number by calling the global function
    ** gfSequence, passing to it an ID : 'BATCH', and a default value
    ** (1) to be used if no batch numbers have been created before.
    ** This is then padded with zeroes from the left CLOSENT.

    IF lnBatchNo = 0
      lnBatchNo = lnBatchNo + 1
    ENDIF

    ** Generate a new transaction number by calling the global function
    ** gfSequence, passing to it an ID : 'TRANSACT', and a default value
    ** (1) to be used if no transaction numbers have been created before.
    ** This is then padded with zeroes from the left.

    lnTranNo = lnTranNo + 1

    ** Generate a new closing entry number by calling the global function
    ** gfSequence, passing to it an ID : 'CLOSENT', and a default value
    ** (1) to be used if no closing entry numbers have been created before.
    ** This is then padded with zeroes from the left.

    lnClosNo = lnClosNo + 1

    *B610484,1 TMI 08/27/2013 [Start] get the correct value of the variable gcAct_Appl
    gcAct_Appl =  oAriaApplication.activemoduleid
    *B610484,1 TMI 08/27/2013 [End  ] 

    SELECT(lcTmpTrnHd)

    APPEND BLANK
    REPLACE cBatchNo  WITH PADL(lnBatchNo,FSIZE('CBATCHNO'),'0'),;
            cTranNo   WITH PADL(lnTranNo,FSIZE('CTRANNO'),'0'),;
            cTrnDesc  WITH 'Created by '+lcUserName,;
            cTrnRefer WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTYECNo,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTYECNo",loFormSet.HeaderAlias))+' '+PADL(lnClosNo,FSIZE('CTRANNO'),'0'),;
            dTrnPDate WITH loFormSet.laCompany[loFormSet.puCompany,10],;
            cTrnPYr   WITH loFormSet.laCompany[loFormSet.puCompany,4],;
            cTrnPPrd  WITH loFormSet.laCompany[loFormSet.puCompany,3],;
            cTrnType  WITH 'Y',;
            cTrnStat  WITH 'Y',;
            cTrnRever WITH 'N',;
            nTrnTotDr WITH lnTotTrnDr,;
            nTrnTotCr WITH lnTotTrnCr,;
            cSrcModul WITH gcAct_Appl,;
            cStandard WITH 'Y',;
            cSrcJrnl  WITH lcSJ_Def

    SELECT(lcTmpTrnDt)

    REPLACE ALL FOR EMPTY(cBatchNo);
                cBatchNo WITH PADL(lnBatchNo,FSIZE('CBATCHNO'),'0'),;
                cTranNo  WITH PADL(lnTranNo,FSIZE('CTRANNO'),'0'),;
                cTrDtExp WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTClsEntN,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTClsEntN",loFormSet.HeaderAlias))+' '+;
                              PADL(lnClosNo,FSIZE('CTRANNO'),'0')
  ENDIF

  SELECT(lcTmpClsEnt)

  ** Update total debit and credit values for the batch
  lnTotBatDr = lnTotBatDr + lnTotTrnDr
  lnTotBatCr = lnTotBatCr + lnTotTrnCr

ENDSCAN

** Having finished creating a transaction header and details,
** append a new record in the batches file.
** Do not allow any batch to have zero total debit or credit
** values, If a batch has zero total debit and total credit,
** there is no need to proceed with files creation, appending
** to master files, or posting

** Checking total debit value is enough since both total
** values are equal.

IF lnTotBatDr <> 0

  *B610484,1 TMI 08/27/2013 [Start] get the correct value of the variable gcAct_Appl
  gcAct_Appl =  oAriaApplication.activemoduleid
  *B610484,1 TMI 08/27/2013 [End  ] 

  SELECT(lcTmpBatch)

  APPEND BLANK

  REPLACE cBatchNo  WITH PADL(lnBatchNo,FSIZE('CBATCHNO'),'0'),;
          lBatInd   WITH .F.,;
          cBatType  WITH 'C',;
          cBatStat  WITH 'Y',;
          cBatPYr   WITH loFormSet.laCompany[loFormSet.puCompany,4],;
          dBatPBeg  WITH loFormSet.laCompany[loFormSet.puCompany,10],;
          dBatPEnd  WITH loFormSet.laCompany[loFormSet.puCompany,10],;
          cBatRefer WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTYECNo,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTYECNo",loFormSet.HeaderAlias))+' '+PADL(lnClosNo,FSIZE('CTRANNO'),'0'),;
          cBatDesc  WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTPrior,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTPrior",loFormSet.HeaderAlias))+' '+lcUserName,;
          nBatCnTot WITH lnTotBatDr,;
          nBaTotDr  WITH lnTotBatDr,;
          nBaTotCr  WITH lnTotBatCr,;
          cSrcModul WITH gcAct_Appl

  SELECT(lcBatch)
  SET RELATION TO

  SELECT(lcTrnsHd)
  SET RELATION TO

  SELECT(lcTrnsDt)

  ** Update master batch/transactions header/transaction details
  SET ORDER TO TAG BATCHNO  IN &lcTmpBatch
  SET ORDER TO TAG BATCHTRN IN &lcTmpTrnHd
  SET ORDER TO TAG BATCHTRN IN &lcTmpTrnDt

  SELECT(lcTmpBatch)
  SET RELATION TO

  SELECT(lcTmpTrnHd)
  SET RELATION TO
  SET RELATION TO CBATCHNO INTO &lcTmpBatch ADDITIVE

  SELECT(lcTmpTrnDt)
  SET RELATION TO
  SET RELATION TO CBATCHNO + CTRANNO INTO &lcTmpTrnHd ADDITIVE

  ** Set variables for the Thermometer
  lnCurtrns  = 0
  lnTotTrns  = RECCOUNT()

  ** Variable to compair if there is a change in the batch No.
  lcBatchKey = ' '

  ** Variable to compair if there is a change in the Transaction No.
  lcTrnNoKey = ' '

  ** Variables to sum the total debit & credit per Batch.
  lnTotBatDr = 0
  lnTotBatCr = 0

  ** Variables to sum the total debit & credit per Transaction.
  lnTotTrnDr = 0
  lnTotTrnCr = 0

  SCAN
    ** Assign the thermometer variable with the Record no.
    lnCurtrns  = lnCurtrns + 1
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfThermo(lnTotTrns,lnCurtrns,LANG_SMCLOSE_lcTUpdMast,"")
=gfThermo(lnTotTrns,lnCurtrns,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTUpdMast,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTUpdMast",loFormSet.HeaderAlias)),"")
*N000682,1 11/20/2012 MMT Globlization changes[End]


    ** Compair if the batch no = the old batch or not.  If not
    ** we are going to creat a new batch no.
    IF lcBatchKey <> cBatchNo

      lcBatchKey = cBatchNo

      ** Creating New batch No.

      lcBatchNo = gfSequence('CBATCHNO' , loFormSet.laCompany[loFormSet.puCompany,2])

      SELECT(lcTmpBatch)
      SCATTER MEMVAR MEMO
      m.cBatchNo  = lcBatchNo
      m.nBatCnTot = 0
      m.nBatotDr  = 0
      m.nBatotCr  = 0
      m.cComp_Id  = loFormSet.laCompany[loFormSet.puCompany,2]
      m.cAdd_User = oAriaApplication.User_ID
      m.dAdd_Date = DATE()
      m.cAdd_Time = gfGetTime()

      SELECT(lcBatch)
      APPEND BLANK
      GATHER MEMVAR MEMO

      SELECT(lcTmpTrnDt)
      lnTotBatDr = 0
      lnTotBatCr = 0
    ENDIF

    ** Compair if the batch no = the old transaction or not.  If not
    ** we are going to creat a new transaction no.
    IF lcTrnNoKey <> cTranNo

      lcTrnNoKey = cTranNo

      ** Create New Transaction No.

      lcTranNo = gfSequence('CTRANNO' , loFormSet.laCompany[loFormSet.puCompany,2])
      ** Create New Closing entries No.

      lcClosNo = gfSequence('CCLOSENT', loFormSet.laCompany[loFormSet.puCompany,2],;
      						 '','','CTRANNO')
      SELECT(lcTmpTrnHd)
      SCATTER MEMVAR MEMO
      m.cBatchNo  = lcBatchNo
      m.cTranNo   = lcTranNo
      m.nTrnTotDr = 0
      m.nTrnTotCr = 0
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*m.cTrnRefer = LANG_SMCLOSE_lcTYECNo+' '+lcClosNo
m.cTrnRefer = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTYECNo,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTYECNo",loFormSet.HeaderAlias))+' '+lcClosNo
*N000682,1 11/20/2012 MMT Globlization changes[End]

      m.cComp_Id  = loFormSet.laCompany[loFormSet.puCompany,2]
      m.cAdd_User = oAriaApplication.User_ID
      m.dAdd_Date = DATE()
      m.cAdd_Time = gfGetTime()

      SELECT(lcTrnsHd)
      APPEND BLANK
      GATHER MEMVAR MEMO

      SELECT(lcTmpTrnDt)
      lnTotTrnDr = 0
      lnTotTrnCr = 0
    ENDIF

    SELECT(lcTmpTrnDt)
    SCATTER MEMVAR MEMO
    m.cBatchNo  = lcBatchNo
    m.cTranNo   = lcTranNo
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*m.cTrDtExp  = LANG_SMCLOSE_lcTClsEntN+' '+lcClosNo
m.cTrDtExp  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTClsEntN,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTClsEntN",loFormSet.HeaderAlias))+' '+lcClosNo
*N000682,1 11/20/2012 MMT Globlization changes[End]

    m.cAdd_User = oAriaApplication.User_ID
    m.dAdd_Date = DATE()
    m.cAdd_Time = gfGetTime()

    IF cDrorcr = 'D'
      lnTotTrnDr = lnTotTrnDr + nAmount
      lnTotBatDr = lnTotBatDr + nAmount
    ELSE
      lnTotTrnCr = lnTotTrnCr + nAmount
      lnTotBatCr = lnTotBatCr + nAmount
    ENDIF

    SELECT(lcTrnsDt)
    APPEND BLANK
    GATHER MEMVAR MEMO

    SELECT(lcTrnsHd)
    REPLACE nTrnTotDr WITH lnTotTrnDr,;
            nTrnTotCr WITH lnTotTrnCr

    SELECT(lcBatch)
    REPLACE nBatCnTot WITH lnTotBatDr,;
            nBatotDr  WITH lnTotBatDr,;
            nBatotCr  WITH lnTotBatCr,;
            cBatRefer WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTYECNo,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTYECNo",loFormSet.HeaderAlias))+' '+lcClosNo
*N000682,1 11/20/2012 MMT Globlization changes[End]


    SELECT(lcTmpTrnDt)

  ENDSCAN

  SELECT(lcTrnsHd)
  REPLACE nTrnTotDr WITH lnTotTrnDr,;
          nTrnTotCr WITH lnTotTrnCr

  SELECT(lcBatch)
  REPLACE nBatCnTot WITH lnTotBatDr,;
          nBatotDr  WITH lnTotBatDr,;
          nBatotCr  WITH lnTotBatCr,;
          cBatRefer WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTYECNo,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTYECNo",loFormSet.HeaderAlias))+' '+lcClosNo
*N000682,1 11/20/2012 MMT Globlization changes[End]


  IF lnCurtrns < lnTotTrns
    FOR lnCounter = lnCurtrns TO lnTotTrns
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfThermo(lnTotTrns,lnCounter,LANG_SMCLOSE_lcTUpdMast,"")
=gfThermo(lnTotTrns,lnCounter,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTUpdMast,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTUpdMast",loFormSet.HeaderAlias)),"")
*N000682,1 11/20/2012 MMT Globlization changes[End]

    ENDFOR
  ENDIF
ENDIF

** Set the relation off ***
SELECT(lcTrnsHd)
SET RELATION TO

SELECT(lcTmpBatch)
GO TOP
REPLACE cBatchNo WITH lcBatchNo
SET RELATION TO

SELECT(lcPTrnHd)
SET RELATION TO

SELECT(lcTmpTrnHD)
SET RELATION TO

SELECT(lcTmpTrnDt)
SET RELATION TO

SELECT(lcBatch)
SET RELATION TO

WAIT CLEAR

*!**************************************************************************
*!
*!      Function : lfUpdComp
*!
*!**************************************************************************
*
FUNCTION lfUpdComp

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_SMCLOSE_lcTCompFil NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTCompFil,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTCompFil",loFormSet.HeaderAlias)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]


SELECT SYCCOMP
=SEEK(loFormSet.laCompany[loFormSet.puCompany,2])
*E303343,1 TMI 01/17/2013 [Start] this line to resolve a strange locking problem in the &lcFsPrd file
IF EOF(lcFsPrd)
  GO BOTTOM in &lcFsPrd
ENDIF
*E303343,1 TMI 01/17/2013 [End  ]
IF gfRLOCK("SYCCOMP,&lcFsPrd",.T.)
  lcNewYear = STR(INT(VAL(cCurr_Yer)+1),4)
  REPLACE cCurr_Yer WITH lcNewYear,;
          cCurr_Prd WITH '01',;
          cAdd_User WITH oAriaApplication.User_ID,;
          dAdd_Date WITH DATE(),;
          cAdd_Time WITH gfGetTime()

  ** Update fiscal period file
  SELECT (lcFsPrd)
  =SEEK(loFormSet.laCompany[loFormSet.puCompany,4]+loFormSet.laCompany[loFormSet.puCompany,3])

  IF llCloseGL
    ** Close curren period and lock it if required in glsetup.
    REPLACE lFspLocks WITH lFspLocks .OR. &lcSetup..lSetLokPd,;
            lFspclsds WITH .T.
  ELSE
    ** Close curren period and lock it if required in glsetup.
    REPLACE lFspLocks WITH lFspLocks,;
            lFspclsds WITH .T.
  ENDIF
  =gfAdd_Info(lcFsPrd)

  ** Unlock next period to be the current
  SKIP 1
  REPLACE lFspLocks WITH .F.
  =gfAdd_Info(lcFsPrd)
  =gfRLOCK("SYCCOMP,&lcFsPrd",.F.)

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_SMCLOSE_lcTCompAP NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTCompAP,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTCompAP",loFormSet.HeaderAlias)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

  SELECT SYCCOMP
  IF 'AP' $ MMODLSET
    USE (lcDataDir+'APVENDOR') IN 0
    USE (lcDataDir+'APVENHST') IN 0 ORDER TAG VENDYEAR

	*B800692,1 M.H Begin.
	*    lcNewNxtYr= STR(INT(VAL(cCurr_Yer)+2),4)
	    lcNewNxtYr= STR(INT(VAL(cCurr_Yer)+1),4)
	*B800692,1 M.H End.

    SELECT APVENDOR
    SCAN
      SELECT APVENHST
      IF !SEEK(APVENDOR.CVENDCODE+lcNewNxtYr)
        APPEND BLANK
        REPLACE CVENDCODE WITH APVENDOR.CVENDCODE,;
                CFISFYEAR WITH lcNewNxtYr,;
                CADD_USER WITH oAriaApplication.User_ID,;
                DADD_DATE WITH DATE(),;
                CADD_TIME WITH gfGetTime()
      ENDIF
      SELECT APVENDOR
    ENDSCAN
    USE IN APVENDOR
    USE IN APVENHST
  ENDIF

  =lfUpdVend()  && Function to update the Child company files.

  ** MESSAGE : "Closing year & period processing is finished."
  **           "Current period is 01.  Current year is ð.    "
  ** Choices : "                     ® Ok ¯                  "
  =gfModalGen('TRM00115B00000','Dialog',lcNewYear)
  loFormSet.laCompany[loFormSet.puCompany,9] = 'C'
ELSE
  loFormSet.laCompany[loFormSet.puCompany,9] = 'L'
  =gfRLOCK("SYCCOMP,&lcFsPrd",.F.)
ENDIF

WAIT CLEAR

*!**************************************************************************
*!
*!      Function:  lfSetupChk
*!
*!**************************************************************************
*
FUNCTION lfSetupChk
PARAMETERS loFormSet,lcPath

** Varible to hold the TEMP file Name for the SETUP file
lcGLSetup = gfTempName()

** Trimming the parameter which hold the Company path
lcPath  = ALLTRIM(lcPath)

** Varibale to indicate whether the setup done in the GLSETUP file or not
llSetDone = .F.

** Checking if the file is exict in the company directory or not
** if the file exict then open by onther alias name

IF FILE(lcPath+'GLSETUP.DBF')
  SELECT 0
  USE &lcPath.GLSETUP AGAIN ALIAS('&lcGLSetup')
  llSetDone = lSetDon
ENDIF

IF USED(lcGLSetup)
  USE IN &lcGLSetup
ENDIF

RETURN (llSetDone)

*!**************************************************************************
*!
*!      Function:  lfThermo
*!
*!**************************************************************************
*
FUNCTION lfThermo
PARAMETERS lcMessage
lnTermCount = lnTermCount + 1
WAIT WINDOW lcMessage+'   '+SUBSTR(lcSpcChr,MOD(lnTermCount,4)+1,1) NOWAIT

*!**************************************************************************
*!
*!      Function: lfApUpdate
*!
*!**************************************************************************
*
FUNCTION lfApUpdate

SELECT SYCCOMP
=SEEK(loFormSet.laCompany[loFormSet.puCompany,2],'SYCCOMP')

************************** Update master files *****************************

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_SMCLOSE_lcTCompFil NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTCompFil,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTCompFil",loFormSet.HeaderAlias)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]


IF gfRLOCK("SYCCOMP",.T.)
  ** Update the current period of the selected company.
  SELECT SYCCOMP
  REPLACE CCURR_PRD WITH PADL(INT(VAL(CCURR_PRD))+1,2,'0')
  =gfAdd_Info('SYCCOMP')

  lcCurntPrd = PADL(INT(VAL(CCURR_PRD)),2,'0')
  ** Update fisical period file.
  SELECT (lcFsPrd)
  SET ORDER TO TAG COMFYRPRDI
  =SEEK(loFormSet.laCompany[loFormSet.puCompany,4]+loFormSet.laCompany[loFormSet.puCompany,3])
  ** Close curren period and lock it if required in glsetup.
  REPLACE lFspLocks WITH lFspLocks,;
          lFspclsds WITH .T.
  =gfAdd_Info(lcFsPrd)
  ** Unlock next period to be the current.
  SKIP 1
  REPLACE lFspLocks WITH .F.

  =gfAdd_Info(lcFsPrd)

  ** Unlock the locked file
  =gfRLOCK("SYCCOMP",.F.)
  ** MESSAGE : "Closing period processing is finished."
  **           "Current period is ð.                  "
  ** Choices : "                  ® Ok ¯              "
  IF lcClose = 'P'
    =gfModalGen('TRM00104B00000','Dialog',lcCurntPrd)
    loFormSet.laCompany[loFormSet.puCompany,9] = 'C'
  ENDIF
ELSE
  loFormSet.laCompany[loFormSet.puCompany,9] = 'L'
  =gfRLOCK("SYCCOMP",.F.)
ENDIF

WAIT CLEAR

************** CLOSING AP PERIOD **************

*!**************************************************************************
*!
*!      Function: lfUpdVend
*!
*!**************************************************************************
*
FUNCTION lfUpdVend

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_SMCLOSE_lcTCmpChld NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lcTCmpChld,loFormSet.GetHeaderText("LANG_SMCLOSE_lcTCmpChld",loFormSet.HeaderAlias)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]


SELECT SYCCOMP
SCAN FOR CCOMPPRNT = loFormSet.laCompany[loFormSet.puCompany,2] .AND. 'AP' $ MMODLSET
  lcApPath = gfgetdatadir(ALLTRIM(CCOM_DDIR))
  USE (lcApPath+'APVENDOR') IN 0
  USE (lcApPath+'APVENHST') IN 0 ORDER TAG VENDYEAR
  SELECT APVENDOR
  SCAN
    SELECT APVENHST
    IF !SEEK(APVENDOR.CVENDCODE+lcNewNxtYr)
      APPEND BLANK
      REPLACE CVENDCODE WITH APVENDOR.CVENDCODE,;
              CFISFYEAR WITH lcNewNxtYr,;
              CADD_USER WITH oAriaApplication.User_ID,;
              DADD_DATE WITH DATE(),;
              CADD_TIME WITH gfGetTime()
    ENDIF
    SELECT APVENDOR
  ENDSCAN
  USE IN APVENDOR
  USE IN APVENHST
ENDSCAN

WAIT CLEAR

*!**************************************************************************
*!
*!      Function:  lfOpenFiles
*!
*!**************************************************************************
*- Add file openings of ACCOD, FISHD, FSPRD
** This function is to Open files & set order
FUNCTION lfOpenSFils
PARAMETERS loFormSet,llOpenFile

** Flag indicate that the files of the selected company is opened.
llOpenFile = .T.

FOR lnCount = 1 TO ALEN(loFormSet.laSFileOpn,1)
  IF FILE(loFormSet.lcDataDir+loFormSet.laSFileOpn[lnCount,1]+'.DBF')
    IF USED(loFormSet.laSFileOpn[lnCount,2])
      SELECT(loFormSet.laSFileOpn[lnCount,2])
      USE
    ELSE
      SELECT 0
    ENDIF
    USE (loFormSet.lcDataDir+loFormSet.laSFileOpn[lnCount,1]) AGAIN ALIAS(loFormSet.laSFileOpn[lnCount,2])
    SET ORDER TO TAG (loFormSet.laSFileOpn[lnCount,3])
  ELSE
    ** MESSAGE : "File ð not found in the directory of company ð."
    ** Choices : "                    ® Ok ¯                     "
    IF loFormSet.puCompany <> 0
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00110B00000","ALERT",LANG_File+loFormSet.laSFileOpn[lnCount,1]+'|'+ALLTRIM(loFormSet.laCompany[loFormSet.puCompany,1])+'|'+LANG_SMCLOSE_lctCloseL)
=gfModalGen("TRM00110B00000","ALERT",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_File,loFormSet.GetHeaderText("LANG_File",loFormSet.HeaderAlias))+loFormSet.laSFileOpn[lnCount,1]+'|'+ALLTRIM(loFormSet.laCompany[loFormSet.puCompany,1])+'|'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SMCLOSE_lctCloseL,loFormSet.GetHeaderText("LANG_SMCLOSE_lctCloseL",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      loFormSet.laCompany[loFormSet.puCompany,9] = 'F'
    ENDIF
    llOpenFile = .F.
    EXIT
  ENDIF
ENDFOR

*- Close fiscal calendar files
IF !llOpenFile
  FOR lnFCount = 1 TO ALEN(loFormSet.laSFileOpn,1)
    IF USED(loFormSet.laSFileOpn[lnFCount,2])
      USE IN (loFormSet.laSFileOpn[lnFCount,2])
    ENDIF
  ENDFOR
ENDIF
RETURN llOpenFile


************************************************************
*!
*! Name      : lfChkCmpAc
*! Purpose   : checking account
*!
************************************************************
FUNCTION lfChkCmpAc
PARAMETERS loFormSet,lcComp_Id, lcAccCdStr
PRIVATE lnCurAlias, llRetVal, lcTempAcCd, lnCurRec, lcDataDir

lnCurAlias = SELECT()
lcTempAcCd = gfTempName()
llRetVal   = .F.

SELECT SYCCOMP
lnCurRec = RECNO()
SCAN FOR EMPTY(cCompPrnt) .AND. cComp_Id <> lcComp_ID .AND. !llRetVal
  lcDataDir = gfGetDataDir(ALLTRIM(SYCCOMP.CCOM_DDIR))
  IF FILE(lcDataDir + 'ACCOD.DBF')
    SELECT 0
    USE (lcDataDir + 'ACCOD') AGAIN ALIAS (lcTempAcCd) ORDER TAG ACCSEGNO
    GO TOP
    llRetVal = ALLTRIM(ACCOD.cAcsMask) = ALLTRIM(lcAccCdStr)
    USE IN (lcTempAcCd)
  ENDIF
ENDSCAN
IF BETWEEN(lnCurRec, 1, RECCOUNT())
  GO lnCurRec
ENDIF
SELECT (lnCurAlias)
RETURN llRetVal
*- End of lfChkCmpAc.

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
  oProgress = NULL
  RETURN
ENDIF

IF ISNULL(oProgress)
  oProgress = NEWOBJECT('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
ENDIF

WITH oProgress
  .TotalProgress = lnTotRecs
  .lblFirstLabel.CAPTION = lcFirstLabel
  .lblSecondLabel.CAPTION = lcSndLable
  .CurrentProgress(lnThermRec)
  .SHOW()
ENDWITH
*- End of lfShowProgress	.
