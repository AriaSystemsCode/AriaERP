*:************************************************************************
*:
*: Procedure file: SMCLOSE.PRG
*:
*:         System: S.M. System
*:         Author: Mohamed Hassan Mohamed
*:      Copyright (c)
*:  Last modified: 05/08/1994
*:
*:  Procs & Fncts: lfwPopup()
*:               : lfvPopup()
*:               : lfvClose()
*:               : lfvProceed()
*:               : lfOpenFiles()
*:               : lfRetEarAcc()
*:               : lfUnpBatch()
*:               : lfvUnpBatch()
*:               : lfUnpSinTran()
*:               : lfvUnpSTrn()
*:               : lfUngenAllo()
*:               : lfvUngnAllo()
*:               : lfUngenRec()
*:               : lfvUngnRec()
*:               : lfUpdate()
*:               : lfSetupChk()
*:               : lfAdjBalnc()
*:               : lfAdjPstYP()
*:               : lfBatHandl()
*:               : lfRpstBtTr()
*:               : lfGpstClsEn()
*:               : lfCreatCls()
*:               : lfUpdComp()
*:               :
*:      Documented  /  /   at   :
*:************************************************************************
*B600508,1 Calling the wrong message "00328" it should be "00229"
*B800692,1 M.H 07/31/96 The record created for the next year in the APVENHST file have a wrong year.
*E300431,1 M.H 07/31/96 Add thermometer to the closing period function.
*B800697,1 M.H 08/07/96 Fix the bug of assign the comapny position to the variable pucompany.
*B800698,1 M.H 08/07/96 The closing entry batch has a reference field,
*B800698,1 M.H 08/07/96 GLBATCH.cBatRefer : 'YEC No.  000000'
*B800698,1 M.H 08/07/96 change to 
*B800698,1 M.H 08/07/96 GLBATCH.cBatRefer : 'YEC No 00000002' 
*B800698,1 M.H 08/07/96 because it does not show the closing entry number, it does not fit the field.
*B800705,1 M.H 08/07/96 Remove the print button from SYDDLOBJ CAPP_ID = 'SM' CDLOBJTYP = 'B' CDLOBJID = '00021'
*B601460,1 RENEE 12/03/96 Fix the following problems that happen
*B601460,1 RENEE 12/03/96 when there are unposted batches, unposted 
*B601460,1 RENEE 12/03/96 transactions, unallocated transactions 
*B601460,1 RENEE 12/03/96 or ungenerated recurring transactions in a period that 
*B601460,1 RENEE 12/03/96 is being closed.
*B601460,1 RENEE 12/03/96 1. Syntax error when pressing 'Display'
*B601460,1 RENEE 12/03/96 2. The 'Resume' 'Display' 'Cancel' message 
*B601460,1 RENEE 12/03/96    appears again if you 'Cancel'
*B601460,1 RENEE 12/03/96 3. The text of the message is not appropriate to the
*B601460,1 RENEE 12/03/96    different cases, always displaying 'unposted batches'
*B601460,1 RENEE 12/03/96 4. Both < Resume > and < Cancel > buttons are 
*B601460,1 RENEE 12/03/96    activated with Ctrl+Enter
*B601646,1 RENEE 03/06/97 When closing year, create the new balances (GLACBALS)
*B601646,1                from the last closing balance of an account.
*E300663,1  HS 04/23/97 1)Change the calling of the function [gfSequence]
*E300663,1              for the changes we have made to that function.
*E300663,1              2)Make some changes for we have change the file
*E300663,1              SYCSEQUN name to SEQUENCE and make it a data file
*E300663,1              [Change its dir. from SYSFILES to DBFS]
*E300683,4 AHMED 06/08/97 Add prgs directory path to the calling of programs
*E300581,1 RENEE Add program paramemters
*E300692,5 RENEE 07/01/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,5 				   SYCFSPRD, SYCFSHLD
*B601842,1  HS   09/16/97 Prevent deleting posted Batches from the GLBATCH
*B601842,1                file.
*B801372,1  HS   11/25/97 Check if the current period is locked give the
*B801372,1                user a message and cancel the Closing process
*B801372,1                [I added one columns to the array laCompany to
*B801372,1                hold the current period lock status].
*B601960,1  HS   04/05/98 Remove the transactions that are posted before
*B601960,1                the current period from the GLTRNSHD file and
*B601960,1                the GLTRNSDT file.
*E301098,1 HESHAM 12/16/98 Get company data path using gfGetDataDir(..)
*E300789,1 Hesham 03/04/99
*E300789,1 remove field ccomp_id from files ACCOD,FISHD,FSPRD,FSHLD,CODES
*E300789,1 Change Any seeking in these files
*B602668,1 AMM 03/16/99 Fix the bug of 'Invalid subscript reference' when 
*B602668,1 AMM          entering the closing program without active company.
*B602728,1 RENEE 03/25/99 Fix creating balances with '****' opening and 
*B602728,1 				  closing balances at year end close
*B119546,1 ASH 09/02/2003 Change the incomprehensible message when closing final period of a year.
************************************************************************
* This parameter is comming from the menu to tell if its closing 
* This parameter is comming from the menu to tell if its closing 
* year & period or closing period.

PARAMETERS lcClose

lcClose = IIF(TYPE('lcClose')='C',lcClose,'')
IF EMPTY(lcClose)
  RETURN
ENDIF

** Array declaration **
*E300692,5 Add array laSFileOpn[3,3] fo hold fiscal caland Acc code files
*DECLARE laCompany[1],laFileOpen[9,3],laFileStru[1]
DECLARE laCompany[1],laFileOpen[9,3],laFileStru[1], laSFileOpn[4,3]
*E300692,5 end

** Itializing the character variables **

lcCompany  = ' '        && Variable to hold the company name.
laCompany  = ' '        && Array intialization.
laFileOpen = ' '        && Array intialization.
lcSavComp  = ' '        && Variable to Save the Active Company.
lcSavData  = ' '        && Variable to Save the Active Company Data Dir.
lcIndExp1  = ' '        && Variable to hold the index Exp.
lcTagExp1  = ' '        && Variable to hold the tag Exp.
lcIndExp2  = ' '        && Variable to hold the index Exp.
lcTagExp2  = ' '        && Variable to hold the tag Exp.
lcDfRetAct = ' '        && Variable to hold the Retained Account.
lcSusActMj = ' '        && Variable to hold the Suspence Account Major.
lcAcsMask  = ' '        && Variable to hold the Account Mask.
lcAcsegDes = ' '        && Variable to hold the Account Segment Discription.
lcDataDir  = ' '        && Variable to store the company dir.
lcPath     = ' '        && Variable to hold the path of the setup file.
lcTmpFisHD = ' '        && Var to hold name of Fiscal year temp file.
lc_TempPR  = ' '        && Var to hold name of Periods temp file.
lcClosStat = 'N'        && Var to hold the status of the closing date.
lcComp_ID  = gcPrnt_cmp && Variable to hold the company ID,;
                           First initialization by the active company.

*E300692,5 Add variables for Alias names for ACCOD, FISHD, FSPRD, FSHLD files
STORE '' TO lcAccCod, lcFisHd, lcFsPrd, lcFsHld
*E300692,5 end

lcSpcChr   = '|/-\'
lcNewYear  = ''
lcNewNxtYr = ''

** Variables already declared in the screen object file **

lcTCloseU  = ' '        && Variable is already declared in the screen ;
                           object file holding the word close in upper case.
lcTCloseL  = ' '        && Variable is already declared in the screen ;
                           object file holding the word close in lower case.
lcTOpen    = ' '        && Variable is already declared in the screen ;
                           object file holding the word open in lower case.
lcTLock    = ' '        && Variable is already declared in the screen ;
                           object file holding the word lock in lower case.
lcTCloseP  = ' '        && Variable is already declared in the screen ;
                           object file holding the title of the screen;
                           in case of closing period.
lcTCloseY  = ' '        && Variable is already declared in the screen ;
                           object file holding the title of the screen;
                           in case of closing year & period.
lcTUnpostd = ' '        && Variable is already declared in the screen ;
                           object file holding the word unposet single;
                           transaction.
lcTUnGenAl = ' '        && Variable is already declared in the screen ;
                           object file holding the word ungenerated   ;
                           allocations transaction.
lcTUnGenRc = ' '        && Variable is already declared in the screen ;
                           object file holding the word ungenerated   ;
                           recurrind transaction.
lcTColect  = ' '        && Variable is already declared in the screen ;
                           object file holding the message collecting ;
                           companies information.
lcTUnPostB = ' '        && Variable is already declared in the screen ;
                           object file holding the word unposet batches.
lcTGenRevT = ' '        && Variable is already declared in the screen ;
                           object file holding the word Generating revers;
                           transactions.
lcTGenRevB = ' '        && Variable is already declared in the screen ;
                           object file holding the word Generating revers;
                           batches.
lcTUpdMast = ' '        && Variable is already declared in the screen ;
                           object file holding the word Updatind the  ;
                           master files.
lcTFiscal  = ' '        && Variable is already declared in the screen ;
                           object file holding the title of the ficsacl;
                           header screen.
lcTAdjBal  = ' '        && Variable is already declared in the screen ;
                           object file holding 'Adjusting balance...'.
lcTAdjPsYP = ' '        && Variable is already declared in the screen ;
                           object file holding 'Adjusting posted year ;
                           & period...'
lcTBatHand = ' '        && Variable is already declared in the screen ;
                           object file holding 'Batch handeling...'
lcTRclasfi = ' '        && Variable is already declared in the screen ;
                           object file holding 'Reclacified from the  ;
                           batch file.'
lcTRPostBt = ' '        && Variable is already declared in the screen ;
                           object file holding 'Repost batchs &     ;
                           transactions...'
lcTUpdBal  = ' '        && Variable is already declared in the screen ;
                           object file holding 'Update balances...'
lcTGnClsEn = ' '        && Variable is already declared in the screen ;
                           object file holding 'Generate closing      ;
                           entries...'
lcTCrClsEn = ' '        && Variable is already declared in the screen ;
                           object file holding 'Creat closing entries.'
lcTYECNo   = ' '        && Variable is already declared in the screen ;
                           object file holding 'YEC No. '
lcTPrior   = ' '        && Variable is already declared in the screen ;
                           object file holding 'Prior year closing    ;
                           entries by '.
lcTClsEntN = ' '        && Variable is already declared in the screen ;
                           object file holding 'Closing entry No. '.
lcTCompFil = ' '        && Variable is already declared in the screen ;
                           object file holding 'Update the Company file'.
lctClsDone = ' '        && Variable is already declared in the screen ;
                           object file holding 'done'.

** Itializing the logical variables **

llOpenBy   = .F.        && Variable to hold if the selected file is;
                           open by my.
llSetDone  = .F.        && Variable to hold the variable of the lSetDon;
                            from the GLSETUP.DBF.
llSetUpMaj = .F.        && Variable to hold the variable of the CSETRETMJ;
                            from the GLSETUP.DBF.
llFileLock = .F.        && Variable to hold if there is a posible ability;
                            to lock the files.
llFirstTime= .T.        && Variable to enter the Procedure open file if the;
                            the user work dirct without change the company.
llCopyStrut= .T.        && Variable to Copy the files structure.
llDatChange= .F.        && Flag if there is any change in the end date;
                           of the current year.
llPrdChange= .F.        && Flag if there is any change in the Period.
llClosDate = .F.        && Flag to indicate if we are going to replace;
                            the batch end date with the closing date or not.
llCloseGL  = .F.        && Variable to indicate if the GL Module is installed.
llCloseAP  = .F.        && Variable to indicate if the AP Module is installed.

** Itializing the numeric variables **

lnAcsSegSz = 0          && Variable to hold the Account Segment Size.
lnAcsNoSeg = 0          && Variable to hold the Account No of Segments.
lnFrsSegSz = 0          && Variable to hold the Account First Segment.
lnTotTrnDr = 0          && This variable to make the summission of the;
                           total Debit of each Transaction.
lnTotTrnCr = 0          && This variable to make the summission of the;
                           total Credit of each Transaction.
lnTotBatDr = 0          && This variable to make the summission of the;
                           total Debit of each batch.
lnTotBatCr = 0          && This variable to make the summission of the;
                           total Credit of each batch.
lnOldComp  = 0          && Variable to hold the array element of the Old;
                           Company ID.
puCompany  = 0          && Variable to hold the popup selection.
lnTermCount= 0          && Variable to hold the thermo counter.

** Itializing the date variables **

ldFsppBgDt = {}         && Variable to hold the transaction begin date.
ldFsppEnDt = {}         && Variable to hold the transaction end date.
ldDatChange= {}         && Variable to hold if the date of the next year;
                           change.
ldTodayDate= gdSysDate  && Variable to hold todays date.
ldOldNxtDat= {}         && Variable to hold the end date of the old next;
                           year.

IF !gfSetup()
  RETURN
ENDIF

lcTCloseL  = "closing"
lcTCloseU  = "Closing"
lcTOpen    = "open"
lcTLock    = "locked"
*B601460,1 Add an 's' and remove an extra space
*lcTUnPostd = "Unposted Single Transaction "
lcTUnPostd = "Unposted single transactions"
*B601460,1 Remove an extra space
*lcTUnGenAl = "Ungenerated Allocations Transactions "
lcTUnGenAl = "Ungenerated allocation transactions"
*B601460,1 Remove an extra space
*lcTUnGenRc = "Ungenerated Recurring Transactions "
lcTUnGenRc = "Ungenerated recurring transactions"
*B601460,1 end.
lcTCloseP  = "Closing period"
lcTCloseY  = "Closing year & period "
lcTColect  = "Collecting companies information...! "
*B601460,1 Remove an extra space
*lcTUnPostB = "Unposted batches "
lcTUnPostB = "Unposted batches"
*B601460,1 end.
lcTGenRevT = "Generating reverse transactions. "
lcTGenRevB = "Generating reverse batches. "
lcTUpdMast = "Updating master files. "
lcTFiscal  = "Fiscal year"
lcTAdjBal  = "Adjusting balance... "
lcTAdjPsYP = "Adjusting posted year & period... "
lcTBatHand = "Batch Handeling... "
lcTRclasfi = "Reclacified from the batch file. "
lcTRPostBt = "Repost batchs & transactions... "
lcTUpdBal  = "Update balances... "
lcTGnClsEn = "Generate closing entries... "
lcTCrClsEn = "Create closing entries... "
*B800698,1 M.H Begin.
*lcTYecNo   = "YEC No. "
lcTYecNo   = "YEC No"
*B800698,1 M.H End.
lcTPrior   = "Prior year closing entries by "
lcTClsEntn = "Closing entry No. "
lcTCompFil = "Update the company file... "
lcTCompAP  = "Upadte the AP files..."
lcTCmpChld = "Update the AP child companies files..."
lcTClsDone = "done"

** Checking for the closing PARAMETERS from the menu **

IF lcClose = 'P'
  ** The screen title is Closing period.
  lcScrTitle = lctCloseP
ELSE
  ** The screen title is Closing year & period.
  lcScrTitle = lctCloseY
ENDIF

** MESSAGE : "We strongle recommend that a full system"
**           "backup is done immediatly               "
**           "prior to the closing processing.        "
** Choices : "       < Proceed >    ® Cancel ¯        "

IF gfModalGen("TRM00097B00012","ALERT",lctCloseL) = 2
  glQuitting = .T.
  RETURN
ENDIF

WAIT lcTColect WINDOW NOWAIT

*E300692,5 Add variables for ACCOD, FISHD, FSPRD files
lcAccCod     = 'ACCOD' && gfTempName()
lcFisHd      = 'FISHD' &&gfTempName()
lcFsPrd      = 'FSPRD' &&gfTempName()
lcFsHld      = 'FSHLD' &&gfTempName()
*E300692,5 end

*E300692,5 Open ACCOD, FISHD, FSPRD from the correct datadir
*E300692,5 Add elements to laSFileOpn
laSFileOpn[1,1] = 'ACCOD'
laSFileOpn[1,2] = '&lcAccCod'
laSFileOpn[1,3] = 'ACCSEGNO'

laSFileOpn[2,1] = 'FISHD'
laSFileOpn[2,2] = '&lcFisHd'
laSFileOpn[2,3] = 'COMPFYEAR'

laSFileOpn[3,1] = 'FSPRD'
laSFileOpn[3,2] = '&lcFsPrd'
laSFileOpn[3,3] = 'COMFYRPRDI'

laSFileOpn[4,1] = 'FSHLD'
laSFileOpn[4,2] = '&lcFsHld'
laSFileOpn[4,3] = 'COMFYRHDAT'

*E300692,5 Attempt to open ACCOD, FISHD, FSPRD files
*IF !lfOpenSFils()
*  glQuitting = .T.
*  RETURN
*ENDIF  
*E300692,5 end

** Selecting all the company which have data in the GLSETUP file. 
*E300692,5 Use FISHD, FSPRD instead of SYCFISHD, SYCFSPRD
*SELECT SYCCOMP.CCOMP_ID+' '+SYCCOMP.CCOM_NAME,;
       SYCCOMP.CCOMP_ID,SYCCOMP.CCURR_PRD,;
       SYCCOMP.CCURR_YER,SYCFISHD.CFISNOPRD,;
       SYCFISHD.DFISBGDAT,SYCFISHD.DFISENDAT,;
       SYCCOMP.CCOM_DDIR,SPACE(1) AS 'cCompStat',;
       SYCFSPRD.DFSPPENDT;
  FROM &gcSysHome.SYCCOMP,&gcSysHome.SYCFSPRD,;
       &gcSysHome.SYCFISHD;
 WHERE SYCCOMP.CCOMP_ID  + SYCCOMP.CCURR_YER =;
       SYCFISHD.CCOMP_ID + SYCFISHD.CFISFYEAR ;
   AND SYCCOMP.CCOMP_ID  + SYCCOMP.CCURR_YER  + SYCCOMP.CCURR_PRD=;
       SYCFSPRD.CCOMP_ID + SYCFSPRD.CFISFYEAR + SYCFSPRD.CFSPPRDID;
   AND (lfSetupChk(SYCCOMP.CCOM_DDIR) .OR. 'AP' $ SYCCOMP.MCOMP_MDL) ;
 GROUP BY SYCCOMP.CCOMP_ID;
  INTO ARRAY laCompany

SELECT SYCCOMP
lnNoOfCmp = 0
DIME laCompany[1]
STORE ' ' TO laCompany
SCAN FOR !EMPTY(ALLTRIM(SYCCOMP.CCURR_YER))
  *E301098,1 Hesham (Start)
  *lcDataDir = ALLTRIM(SYCCOMP.CCOM_DDIR)
  lcDataDir = gfGetDataDir(ALLTRIM(SYCCOMP.CCOM_DDIR))
  *E301098,1 Hesham (End)
  *E300789,1 Hesham (Start)
  *IF lfOpenSFils() .AND. SEEK(SYCCOMP.CCOMP_ID+SYCCOMP.CCURR_YER,lcFisHd) .AND.;
     SEEK(SYCCOMP.CCOMP_ID+SYCCOMP.CCURR_YER+SYCCOMP.CCURR_PRD,lcFsPrd)
  IF lfOpenSFils() .AND. SEEK(SYCCOMP.CCURR_YER,lcFisHd) .AND.;
     SEEK(SYCCOMP.CCURR_YER+SYCCOMP.CCURR_PRD,lcFsPrd)
  *E300789,1 Hesham (End)   
     lnNoOfCmp = lnNoOfCmp + 1
     
     *B801372,1 Change this line to add one columns to the array for the
     *current period lock status [Begin]
     *DIME laCompany[lnNoOfCmp,10]
     DIME laCompany[lnNoOfCmp,11]
     *B801372,1 Change this line to add one columns to the array [End]
     
     laCompany[lnNoOfCmp,1] = SYCCOMP.CCOMP_ID+' '+SYCCOMP.CCOM_NAME
     laCompany[lnNoOfCmp,2] = SYCCOMP.CCOMP_ID
     laCompany[lnNoOfCmp,3] = SYCCOMP.CCURR_PRD
     laCompany[lnNoOfCmp,4] = SYCCOMP.CCURR_YER 
     laCompany[lnNoOfCmp,5] = &lcFisHd..CFISNOPRD
     laCompany[lnNoOfCmp,6] = &lcFisHd..DFISBGDAT
     laCompany[lnNoOfCmp,7] = &lcFisHd..DFISENDAT
     laCompany[lnNoOfCmp,8] = SYCCOMP.CCOM_DDIR
     laCompany[lnNoOfCmp,9] = SPACE(1)
     laCompany[lnNoOfCmp,10] = &lcFsPrd..DFSPPENDT
     
     *B801372,1 Add this line to add one columns to the array for the
     *current period lock status [Begin]
     laCompany[lnNoOfCmp,11] = &lcFsPrd..lFsPLockS
     *B801372,1 Change this line to add one columns to the array [End]
     
  ENDIF   
ENDSCAN
*E300692,5 end

WAIT CLEAR

** Case there is no companys have the same account code structure.

*IF _TALLY = 0
IF EMPTY(laCompany)
  ** Message : "No companys available.  Either the company"
  **           "Fisical Period Not define or the General  "
  **           "Leadger Setup file is not found.          "
  ** Choice  : "                   ® Ok ¯                 "

  =gfModalGen("TRM00111B00000","Dialog")
  glQuitting = .T.
  RETURN
ELSE

*B800697,1 M.H Begin.
  ** Assign the popup to the Active company.
*  lnElement  = ASCAN(laCompany,gcPrnt_CMP)

  ** Get the no of the element selected.
*  puCompany  = IIF(lnElement > 0,ASUBSCRIPT(laCompany,lnElement,1),1)

   *B602668,1 AMM Check if there is active company select it, else select 
   *B602668,1 AMM first company
   IF !EMPTY(gcPrnt_cmp)
   *B602668,1 AMM end
     FOR lnCount = 1 TO ALEN(laCompany,1)
       IF laCompany[lnCount,2] = gcPrnt_CMP
         puCompany = lnCount
         EXIT
       ENDIF
     ENDFOR
   *B602668,1 AMM select first company.
   ELSE
     puCompany = 1
   ENDIF
   *B602668,1 AMM end
*B800697,1 M.H End.

  ** Assign the display variable of the popup in the screen by the;
     Active company.
  lcCompany  = ALLTRIM(laCompany[puCompany,1])

  ** Assign the begin date with default of the Active company.
  ldFsppBgDt = laCompany[puCompany,6]

  ** Assign the period End date with default of the Active company.
  ldFsppEnDt = laCompany[puCompany,10]

  ** Assign the data dir by the default of the Active company.
  lcDataDir  = ALLTRIM(laCompany[puCompany,8])

  ** Create random names for the Temp files.
  lcTmpBatch   = gfTempName() && Varible to hold the TEMP file Name for;
                                 Batch.
  lcTmpTrnHd   = gfTempName() && Varible to hold the TEMP file Name for;
                                 Transaction Header.
  lcTmpTrnDt   = gfTempName() && Varible to hold the TEMP file Name for;
                                 Transaction Detail.
  lcTmpFisHD   = gfTempName() && Varible to hold the TEMP file Name for;
                                 Fiscal header.
  lc_TempPR    = gfTempName() && Varible to hold the TEMP file Name for;
                                 Periods.
  lcPeriods    = gfTempName() && Variable to hold name of period temp;
                                 cursor.
  lcTempBal    = gfTempName() && Varriable to hold name of temp balance;
                                 file.
  lcTmpClsEnt  = gfTempName() && Variable to holds the closing entries;
                                 generated during the closing session.
  lcTmpAcChar2 = gfTempName() && Alias name to be used when openning ;
                                 GLACCHAR file for the scond time.
  lcTmpReport  = gfTempName() && Varible to hold the Cursor Name for;
                                 the closing report.
 
  ** Create random names for the company files which is going to be
  ** open by onther ALIAS name.
  
  lcAcChar = gfTempName() && Variable to hold the GLACCHAR
  lcSetup  = gfTempName() && Variable to hold the GLSETUP
  lcBatch  = gfTempName() && Variable to hold the GLBATCH
  lcAutHd  = gfTempName() && Variable to hold the GLAUTHD
  lcTrnsHd = gfTempName() && Variable to hold the GLTRNSHD
  lcTrnsDt = gfTempName() && Variable to hold the GLTRNSDT
  lcPTrnHd = gfTempName() && Variable to hold the GLPTRNHD
  lcPTrnDt = gfTempName() && Variable to hold the GLPTRNDT
  lcAcBals = gfTempName() && Variable to hold the GLACBALS

ENDIF

** Array to hold information about the files to be opend
** File name, Alias name, Tag name

laFileOpen[1,1] = 'GLACCHAR'
laFileOpen[1,2] = '&lcAcChar'
laFileOpen[1,3] = 'ACCTCODE'

laFileOpen[2,1] = 'GLSETUP'
laFileOpen[2,2] = '&lcSetup'
laFileOpen[2,3] = ''

laFileOpen[3,1] = 'GLBATCH'
laFileOpen[3,2] = '&lcBatch'
laFileOpen[3,3] = 'BATSTAT'

laFileOpen[4,1] = 'GLAUTHD'
laFileOpen[4,2] = '&lcAutHd'
laFileOpen[4,3] = 'TYPECODE'

laFileOpen[5,1] = 'GLTRNSHD'
laFileOpen[5,2] = '&lcTrnsHd'
laFileOpen[5,3] = 'TRANSTAT'

laFileOpen[6,1] = 'GLTRNSDT'
laFileOpen[6,2] = '&lcTrnsDt'
laFileOpen[6,3] = 'BATCHTRN'

laFileOpen[7,1] = 'GLPTRNHD'
laFileOpen[7,2] = '&lcPTrnHd'
laFileOpen[7,3] = 'BATCHTRN'

laFileOpen[8,1] = 'GLPTRNDT'
laFileOpen[8,2] = '&lcPTrnDt'
laFileOpen[8,3] = 'BATCHTRN'

laFileOpen[9,1] = 'GLACBALS'
laFileOpen[9,2] = '&lcAcBals'
laFileOpen[9,3] = 'ACCYRPRD'

*E300683,4 Call *.SPR from screens directory
* DO SMCLOSE.SPR  && Call the screen program.
DO (gcScrDir + gcWinAppl + '\SMCLOSE.SPR')
*E300683,4 end  
glQuitting = .T.

** ERASE the Temp. file which hold the Batchs.
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

*!**************************************************************************
*!
*!      Function:  lfwPopup
*!
*!**************************************************************************
*
FUNCTION lfwPopup

** This variable is to save the element of the array for not to open
** again the files of the selected company.
lnOldComp = puCompany
lcColor1  = SCHEME(1,6)
lcColor2  = SCHEME(1,2)

** Show get all hand made popups with the appropirate color. ***
SHOW GET ibPopCom COLOR ,,,,,&lcSelCont,,,&lcEnbCont,&lcDisCont

*!**************************************************************************
*!
*!      Function:  lfvPopup
*!
*!**************************************************************************
*
FUNCTION lfvPopup

** Calling (gfActPop)  to activate the popup to change the source
** company from the list popup menu. Return the Element No of the
** selected company returns 4 Numeric from lcCompany that contains.

IF _DOS
  ** In case of DOS we are using or POPUP.
  lnPopLen  = IIF(ALEN(laCompany,1) <= 3,ALEN(laCompany,1)+4,8) 
  lcComp_ID = gfActPop(2,3,lnPopLen,38,'laCompany',2,1,@lcCompany)

  lcColor1  = SCHEME(1,6)
  lcColor2  = SCHEME(1,2)

  ** Show get all hand made popups with the appropirate color. ***
  SHOW GET ibPopCom  COLOR ,,,,,&lcSelCont,,,&lcEnbCont,&lcDisCont
  SHOW GETS

  ** Assign to this variable the element number of the selected company.
*B800697,1 M.H Begin.
*  puCompany = ASUBSCRIPT(laCompany,ASCAN(laCompany,lcComp_ID),1)
   FOR lnCount = 1 TO ALEN(laCompany,1)
     IF laCompany[lnCount,2] = lcComp_ID
       puCompany = lnCount
       EXIT
     ENDIF
   ENDFOR
*B800697,1 M.H End.
ENDIF

** If the old varaible is not equal the new one so we are going to
** check first if there is any processing done to this company befor
** or not if there is any, so we are going to have message indicates
** the type of processing which is done befor to this selected company.
** If not we are going to assigen the variables to this company.

IF puCompany <> lnOldComp
  llCloseGL   = .F.   && Flag to indicate that the GL module is installed for this company.
  llCloseAP   = .F.   && Flag to indicate that the AP module is installed for this company.
  llCopyStrut = .T.
  ** If the company selected for the first time.
  llFirstTime = .F.
  
  ** Variable to hold the begining date of the current company.
  ldFsppBgdt  = laCompany[puCompany,6]
  
  ** Variable to hold the End date of the current company.
  ldFsppEndt  = laCompany[puCompany,10]
  
  ** Variable to hold the data directory of the current company.
  lcDataDir   = ALLTRIM(laCompany[puCompany,8])
  
  ** Check if the GL Module exist for this company.
  =SEEK(laCompany[puCompany,2],'SYCCOMP')

  ** Check if the GL module is installed for this company.  
  IF 'GL' $ SYCCOMP.MMODLSET
    llCloseGL = .T.  && Flag to indicate that the GL module is installed for this company.
    ** Function to check for the opening of the files.
    =lfOpenFiles()
  ENDIF
  
  ** Check if the AP module is installed for this company.  
  IF 'AP' $ SYCCOMP.MMODLSET
    llCloseAP = .T.  && Flag to indicate that the AP module is installed for this company.
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function:  lfvClose
*!
*!**************************************************************************
*
FUNCTION lfvClose

IF laCompany[puCompany,9] = 'C'
  ** MESSAGE : "ð process is done."
  ** Choice  : "         ® Ok ¯         "
  =gfModalGen("TRM00129B00000","Dialog",LCTCLOSEU+"|"+lctClsDone)
ELSE
  ** MESSAGE : "Closing process is canceled."
  ** Choice  : "          ® Ok ¯            "
  =gfModalGen("TRM00103B00000","Dialog",lctCloseU)
ENDIF  

** Close all open files.
FOR lnFCount = 1 TO ALEN(laFileOpen,1)
  IF USED(laFileOpen[lnFCount,2])
    USE IN &laFileOpen[lnFCount,2]
  ENDIF
ENDFOR

*E300692,5 Close fiscal calendar files
FOR lnFCount = 1 TO ALEN(laSFileOpn,1)
  IF USED(laSFileOpn[lnFCount,2])
    USE IN &laSFileOpn[lnFCount,2]
  ENDIF
ENDFOR
*E300692,1 end

*!**************************************************************************
*!
*!      Function:  lfvProceed
*!
*!**************************************************************************
*
FUNCTION lfvProceed
*E300692,5 Use ACCOD instead of SYCACCOD
*SELECT SYCACCOD
=lfOpenSFils()
SELECT (lcAccCod)

*E300692,5 end
*E300789,1 Hesham (Start)
*IF SEEK(laCompany[puCompany,2])
IF SEEK("")
*E300789,1 Hesham (End)
  IF nAcsSegSz = 0
    ** MESSAGE : "You have to add the account code structure"
    **           "before entering this option !!!           "
    ** Choice  : "                   ® Ok ¯                 "

    =gfModalGen("TRM00227B00000","DIALOG")
    laCompany[puCompany,9] = 'F'
    RETURN
  ELSE
    ** Assign the Account Segment Size, No of Segments, Account Mask,
    ** Account Segment Discription and the Account First Segment Size.
    lnAcsSegSz = nAcsSegSz
    lnAcsNoSeg = nAcsNoSeg
    lcAcsMask  = "X"+SUBSTR(cAcsMask,2)
    lcAcsMask  = STRTRAN(lcAcsMask,'#','9')
    lcAcsegDes = cAcsegDes
    lnFrsSegSz = AT("-",CACSMASK)-1
  ENDIF
ENDIF

** Check if the GL Module exist for this company.
IF SEEK(laCompany[puCompany,2],'SYCCOMP')
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
IF !EMPTY(laCompany[puCompany,9])
  DO CASE
    ** Closing processing is already done normaly.
    CASE laCompany[puCompany,9] = 'C'
      ** MESSAGE : "ð processing is done."
      ** Choice  : "        ® Ok ¯       "

      =gfModalGen("TRM00112B00000","Dialog",lctCloseU)

    ** The company data files cannot be found during the Closing processing.
    CASE laCompany[puCompany,9] = 'F'
      ** MESSAGE : "The Company Files cannot be ð."
      ** Choice  : "           ® Ok ¯             "

      =gfModalGen("TRM00113B00000","Dialog",lctOpen)

    ** The company files cannot be locked during the Closing processing.
    CASE laCompany[puCompany,9] = 'L'
      ** MESSAGE : "The Company Files cannot be ð."
      ** Choice  : "            ® Ok ¯            "

      =gfModalGen("TRM00113B00000","Dialog",lctLock)
  ENDCASE
ELSE
  DO CASE
    CASE llCloseGL
      ** If its the first time selecting this company.
      IF llFirstTime
        llFirstTime = .F.
        ** Function to check for the opening of the files.
        IF !lfOpenFiles()
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
          *E300692,5 Use FISHD instead of SYCFISHD
          *SELECT SYCFISHD
          SELECT (lcFisHd)
          *E300692,5 end
          SET ORDER TO TAG COMPFYEAR
          lcIndExp1= SYS(14,VAL(SYS(21)))
          lcTagExp1= ' TAG '+SYS(22) + IIF('DESC' $ SET('ORDER'),' DESC','')

          ** Add new field to the temp file.
          =AFIELDS(laFileStru)
          lnFileStru = ALEN(laFileStru,1)
          DIMENSION laFileStru[lnFileStru+1,4]
          laFileStru[lnFileStru+1,1] = 'CCURR_PRD'
          laFileStru[lnFileStru+1,2] = 'C'
          laFileStru[lnFileStru+1,3] = 2
          laFileStru[lnFileStru+1,4] = 0
    
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

      IF lcClose = 'P' .AND. laCompany[puCompany,3] = laCompany[puCompany,5]
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
        .AND. INT(VAL(laCompany[puCompany,3])) < INT(VAL(laCompany[puCompany,5]));
        .AND. laCompany[puCompany,4] <> STR(YEAR(laCompany[puCompany,10]),4)

        ** MESSAGE : "You are trying to close the current fisical year befor"
        **           "the last period. Since the closing date is not valid  "
        **           "to be the end date for the current fisical year.      "
        ** Choices : "               < Proceed >      ® Cancel ¯            "

        IF gfModalGen('TRM00099B00012','Dialog') = 2
          RETURN
        ELSE
          ** Variable indicate that the closing date is invalid.
          lcClosStat  = 'I'
          ** Flag indicate that the closing date is changed.
          llDatChange = .F.
        ENDIF
      ENDIF

      IF lcClose = 'Y';
        .AND. INT(VAL(laCompany[puCompany,3])) < INT(VAL(laCompany[puCompany,5]));
        .AND. laCompany[puCompany,4] = STR(YEAR(laCompany[puCompany,10]),4)

        ** MESSAGE : "You are trying to close the current fisical year befor"
        **           "the last period. The current year end date will be    "
        **           "99/99/9999. All batches and transactions will be      "
        **           "reclassified to new batches and transactions according"
        **           "to the new defintion of the fisical year.             "
        ** Choices : "              < Proceed >      ® Cancel ¯             "

        IF gfModalGen('TRM00100B00012','Dialog',DTOC(laCompany[puCompany,10])) = 2
          RETURN
        ELSE
          ** Variable indicate that the end date is valid.
          lcClosStat = 'V'
          ** Flag indicate that the end date is changed.
          llDatChange= .T.
        ENDIF
      ENDIF
      
      *B801372,1 Added this lines to check if the current Period is locked
      *give the user a message and cancel the Closing process [Begin]
      *B801372,1 IF The Current Period is locked
      IF lcClose = 'Y' .AND. laCompany[puCompany,11]
        
        ** MESSAGE : "Period ð is locked. Cannot proceed with the closing"
        **           "process.                                           "
        ** Choices : "                        < Ok >                     "
        =gfModalGen('TRM00278B00000' , 'Dialog' , ALLTRIM(laCompany[puCompany,3]))
        RETURN
      ENDIF    && End of IF
      *B801372,1 Added this lines [End]
      
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
                  IF laCompany[puCompany,9] <> 'L'
                    ** Create new fiscal year & periods direct in the master files.
                    *E300683,4 Call programs from PRGS directory
                    *DO SMFISHD
                    DO (gcAppHome + gcWinAppl + '\SMFISHD ')
                    *E300683,4 end

                  ELSE
                    ** MESSAGE : "The Company Files cannot be ð."
                    ** Choice  : "            ® Ok ¯            "
                    =gfModalGen("TRM00113B00000","Dialog",lctLock)
                  ENDIF

                  ** If there is any problem in locking the file do not continue.
                  IF laCompany[puCompany,9] <> 'L'
                    ** Adjusting balnce file.
                    =lfAdjBalnc()
                  ELSE
                    ** MESSAGE : "The Company Files cannot be ð."
                    ** Choice  : "            ® Ok ¯            "
                    =gfModalGen("TRM00113B00000","Dialog",lctLock)
                  ENDIF  

                  ** If there is any problem in locking the file do not continue.
                  IF laCompany[puCompany,9] <> 'L'
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
                    =gfModalGen("TRM00113B00000","Dialog",lctLock)
                  ENDIF

                  ** If there is any problem in locking the file do not continue.
                  IF laCompany[puCompany,9] <> 'L'
                    ** Do not call this function if the closing stat is normal
                    ** and the end date of the new current year is greater than
                    ** old date of the current year.
                    IF lcClosStat <> 'N' AND ldDatChange < laCompany[puCompany,7]
                      ** Handel the over lap batches
                      =lfBatHandl()
                    ENDIF  
                  ELSE
                    ** MESSAGE : "The Company Files cannot be ð."
                    ** Choice  : "            ® Ok ¯            "
                    =gfModalGen("TRM00113B00000","Dialog",lctLock)
                  ENDIF

                  ** If there is any problem in locking the file do not continue.
                  IF laCompany[puCompany,9] <> 'L'      
                    DO CASE
                      ** Call the repost batchs & transaction if the close stat
                      ** is valid.
                      CASE lcClosStat = 'V'
                        ** Repost batchs & transactions
                        =lfRpstBtTr(laCompany[puCompany,10])

                        ** Call the repost batchs & transaction if there is any 
                        ** change in the periods or the end date of the new current
                        ** year is less than the old current year.
                      CASE llPrdChange OR ldDatChange < laCompany[puCompany,7]
                        ** Repost batchs & transactions
                        =lfRpstBtTr(ldDatChange)
                    ENDCASE
                  ELSE
                    ** MESSAGE : "The Company Files cannot be ð."
                    ** Choice  : "            ® Ok ¯            "
                    =gfModalGen("TRM00113B00000","Dialog",lctLock)
                  ENDIF  

                  ** If there is any problem in locking the file do not continue.
                  IF laCompany[puCompany,9] <> 'L' 
                    ** Generate closing entries
                    =lfGpstClsEn()
                  ELSE
                    ** MESSAGE : "The Company Files cannot be ð."
                    ** Choice  : "            ® Ok ¯            "
                    =gfModalGen("TRM00113B00000","Dialog",lctLock)
                  ENDIF  

                  ** If there is any problem in locking the file do not continue.
                  IF laCompany[puCompany,9] <> 'L' 
                    ** Update the company file 
                    =lfUpdComp()
                  ELSE
                    ** MESSAGE : "The Company Files cannot be ð."
                    ** Choice  : "            ® Ok ¯            "
                    =gfModalGen("TRM00113B00000","Dialog",lctLock)
                  ENDIF  
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF

    *CASE llCloseAP
    OTHERWISE
      IF llFirstTime
        llFirstTime = .F.
      ENDIF  

      ** If this the first time creating this file.
      IF llCopyStrut
        llCopyStrut = .F.
        IF !USED(lcTmpFisHD)
          ** Creat temp Fiscal Header file.
          *E300692,5 Use FISHD instead of SYCFISHD
          *SELECT SYCFISHD
          SELECT (lcFisHd)
          *E300692,5 end
          SET ORDER TO TAG COMPFYEAR
          lcIndExp1= SYS(14,VAL(SYS(21)))
          lcTagExp1= ' TAG '+SYS(22) + IIF('DESC' $ SET('ORDER'),' DESC','')

          ** Add new field to the temp file.
          =AFIELDS(laFileStru)
          lnFileStru = ALEN(laFileStru,1)
          DIMENSION laFileStru[lnFileStru+1,4]
          laFileStru[lnFileStru+1,1] = 'CCURR_PRD'
          laFileStru[lnFileStru+1,2] = 'C'
          laFileStru[lnFileStru+1,3] = 2
          laFileStru[lnFileStru+1,4] = 0
    
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

      IF lcClose = 'P' .AND. laCompany[puCompany,3] = laCompany[puCompany,5]
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
        .AND. INT(VAL(laCompany[puCompany,3])) < INT(VAL(laCompany[puCompany,5]));
        .AND. laCompany[puCompany,4] <> STR(YEAR(laCompany[puCompany,10]),4)

        ** MESSAGE : "You are trying to close the current fisical year befor"
        **           "the last period. Since the closing date is not valid  "
        **           "to be the end date for the current fisical year.      "
        ** Choices : "               < Proceed >      ® Cancel ¯            "

        IF gfModalGen('TRM00099B00012','Dialog') = 2
          RETURN
        ELSE
          ** Variable indicate that the closing date is invalid.
          lcClosStat  = 'I'
          ** Flag indicate that the closing date is changed.
          llDatChange = .F.
        ENDIF
      ENDIF

      IF lcClose = 'Y';
        .AND. INT(VAL(laCompany[puCompany,3])) < INT(VAL(laCompany[puCompany,5]));
        .AND. laCompany[puCompany,4] = STR(YEAR(laCompany[puCompany,10]),4)

        ** MESSAGE : "You are trying to close the current fisical year befor"
        **           "the last period. The current year end date will be    "
        **           "99/99/9999. All batches and transactions will be      "
        **           "reclassified to new batches and transactions according"
        **           "to the new defintion of the fisical year.             "
        ** Choices : "              < Proceed >      ® Cancel ¯             "

        IF gfModalGen('TRM00100B00012','Dialog',DTOC(laCompany[puCompany,10])) = 2
          RETURN
        ELSE
          ** Variable indicate that the end date is valid.
          lcClosStat = 'V'
          ** Flag indicate that the end date is changed.
          llDatChange= .T.
        ENDIF
      ENDIF
      
      *B801372,1 Added this lines to check if the Current Period is locked
      *give the user a message and cancel the Closing process [Begin]
      *B801372,1 IF The Current Period is locked
      IF lcClose = 'Y' .AND. laCompany[puCompany,11]
        
        ** MESSAGE : "Period ð is locked. Cannot proceed with the closing"
        **           "process.                                           "
        ** Choices : "                        < Ok >                     "
        =gfModalGen('TRM00278B00000' , 'Dialog' , ALLTRIM(laCompany[puCompany,3]))
        RETURN
      ENDIF    && End of IF
      *B801372,1 Added this lines [End]
      
      ** Closing year & period ***
      IF lcClose = 'Y'
        ** If there is any problem in locking the file do not continue.
        IF laCompany[puCompany,9] <> 'L'
          ** Create new fiscal year & periods direct in the master files.
          *E300683,4 Call programs from PRGS directory
          *DO SMFISHD
          DO (gcAppHome + gcWinAppl + '\SMFISHD ')
          *E300683,4 end
        ELSE
          ** MESSAGE : "The Company Files cannot be ð."
          ** Choice  : "            ® Ok ¯            "
          =gfModalGen("TRM00113B00000","Dialog",lctLock)
        ENDIF
        ** If there is any problem in locking the file do not continue.
        IF laCompany[puCompany,9] <> 'L' 
          ** Update the company file 
          =lfUpdComp()
        ELSE
          ** MESSAGE : "The Company Files cannot be ð."
          ** Choice  : "            ® Ok ¯            "
          =gfModalGen("TRM00113B00000","Dialog",lctLock)
        ENDIF  
      ENDIF
      IF !llCloseGL .AND. lcClose = 'P'
        =lfApUpdate()
      ENDIF  
    
  ENDCASE
ENDIF  

*!**************************************************************************
*!
*!      Function:  lfOpenFiles
*!
*!**************************************************************************
*
** This function is to Open files & set order

FUNCTION lfOpenFiles

** Flag indicate that the files of the selected company is opened.
llOpenFile = .T.

FOR lnCount = 1 TO ALEN(laFileOpen,1)
  IF FILE(lcDataDir+laFileOpen[lnCount,1]+'.DBF')
    IF USED(laFileOpen[lnCount,2])
      SELECT(laFileOpen[lnCount,2])
      USE
    ELSE  
      SELECT 0
    ENDIF  
    USE &lcDataDir.&laFileOpen[lnCount,1] AGAIN ALIAS(laFileOpen[lnCount,2])
    SET ORDER TO TAG &laFileOpen[lnCount,3]
  ELSE
    ** MESSAGE : "File ð not found in the directory of company ð."
    ** Choices : "                    ® Ok ¯                     "

    =gfModalGen("TRM00110B00000","ALERT",'File '+laFileOpen[lnCount,1]+'|'+ALLTRIM(laCompany[puCompany,1])+'|'+lctCloseL)
    laCompany[puCompany,9] = 'F'
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
GO 1

lcDfRetAct = ALLTRIM(&lcSetup..cSetRetMj)

IF !EMPTY(lcDfRetAct)

  ** Prepare the default retained earnings account code according to the
  ** account code mask (lcAcsMask) as well as the field content.

  lcDfRetAct=lcDfRetAct+;
             STRTRAN(SUBSTR(ALLTRIM(lcAcsMask),LEN(lcDfRetAct)+1),'9','0')+;
             REPLICATE(' ',FSIZE('cAcctCode','&lcAcChar')-lnAcsSegSz)

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

    =gfModalGen('TRM00228B00000','Dialog',lctCloseL)
    laCompany[puCompany,9] = 'F'
    RETURN .F.
  ENDIF
ELSE

  ** If the field is empty, present the following message, and return 0

  ** Message : "You have to setup the ð account major " 
  **           "in the GL setup before ð.             " 
  ** Choice  : "                ® OK ¯                "   
  lcMsgTxt = 'retained earnings'
  =gfModalGen('TRM00229B00000','Dialog',lcMsgTxt+"|"+lctCloseL)
  laCompany[puCompany,9] = 'F'
  RETURN .F.
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
             STRTRAN(SUBSTR(ALLTRIM(lcAcsMask),LEN(lcSusActMj)+1),'9','0')+;
             REPLICATE(' ',FSIZE('cAcctCode','&lcAcChar')-lnAcsSegSz)

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

    =gfModalGen('TRM00230B00000','Dialog',lctCloseL)
    laCompany[puCompany,9] = 'F'
    RETURN .F.
  ENDIF
ELSE
  ** If the field is empty, present the following message, and return 0

  ** Message : "You have to setup the ð major in the "
  **           "GL setup before ð.                   " 
  **           "                ® OK ¯               "
*B600508,1 Calling the right message "00229" not "00328"
*  lcMsgTxt = 'suspence account'
   lcMsgTxt = 'suspence'
*  =gfModalGen('TRM00328B00000','Dialog',lcMsgTxt+"|"+lctCloseL)
  =gfModalGen('TRM00229B00000','Dialog',lcMsgTxt+"|"+lctCloseL)
  laCompany[puCompany,9] = 'F'
  RETURN .F.
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

  *B601460,1 Pass another parameter to gfModalGen
  *lnChoice   = gfModalGen('TRM00101B00021','Dialog',lctUnpostb,"lfvUnpBatch()")
  lnChoice   = gfModalGen('QRM00101B00021','Dialog',;
                          LOWER(lctUnpostb)+'|'+LOWER(lctUnpostb),"lfvUnpBatch()")
  *B601460,1 Better coding
  *llUnpBatch = IIF(lnChoice = 1,.T.,.F.)
  llUnpBatch = lnChoice = 1
  *B601460,1 end
  laCompany[puCompany,9] = IIF(!llUnpBatch,'F',' ')
  EXIT
ENDSCAN

RETURN llUnpBatch

*!**************************************************************************
*!
*!      Function:  lfvUnpBatch
*!
*!**************************************************************************
*
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

    lcSav_Ttl  = lcFile_Ttl
    lcFile_Ttl = lctUnpostb
    lcSavBrFlds= lcBrFields
    *B601460,1 Fill lcBrFields with the browse fields of GLBATCH file
    *B601460,1 Use global function gfDbfField() 
    *lcBrFields = ''
    lcBrFields = gfDbfField('GLBATCH')
    *B601460,1 end

    SELECT(lcTempBatch)
    DIMENSION laTemp[1] 
    =gfBrows(.F.,"cbatchno",'laTemp',lcFile_Ttl,.F.)

    lcBrFields = lcSavBrFlds

    lcFile_Ttl = lcSav_Ttl
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
  *B601460,1 Pass another parameter to gfModalGen
  *lnChoice = gfModalGen('TRM00101B00021','Dialog',lctUnpostd,"lfvUnpSTrn()")
  lnChoice = gfModalGen('QRM00101B00021','Dialog',;
                        LOWER(lctUnpostd)+'|'+LOWER(lctUnpostd),"lfvUnpSTrn()")
  *B601460,1 Comment the next paragraph so that the message is not displayed
  *B601460,1 twice.
  *IF lnChoice = 2 .OR. lnChoice = 3
  *  lnChoice = gfModalGen('TRM00101B00021','Dialog',lctUnpostd,"lfvUnpSTrn()")
  *ENDIF
  *llUnpSinTrn = IIF(lnChoice = 1,.T.,.F.)
  llUnpSinTrn = lnChoice = 1
  *B601460,1 end
  laCompany[puCompany,9] = IIF(!llUnpSinTrn,'F',' ')
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

    lcSav_ttl  = lcFile_Ttl

    lcFile_Ttl = lctUnpostd
    lcSavBrFlds= lcBrFields          
    *B601460,1 Fill lcBrFields with the browse fields of GLTRNSHD file
    *B601460,1 Use global function gfDbfField() 
    *lcBrFields = ''
    lcBrFields = gfDbfField('GLTRNSHD')
    *B601460,1 end    
    SELECT(lcTempTrn)

    DIMENSION laTemp[1]
    =gfBrows(.F.,"cbatchno",'laTemp',lcFile_Ttl,.F.)

    lcFile_Ttl = lcSav_ttl
    lcBrFields = lcSavBrFlds
 
    USE IN &lcTempTrn

  ** Resume or cancel.
  OTHERWISE
*    CLEAR READ

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
    *B601460,1 Pass another parameter to gfModalGen
    *lnChoice    = gfModalGen('TRM00101B00021','Dialog',lctUngenAl,"lfvUngnAllo()")
    lnChoice    = gfModalGen('QRM00101B00021','Dialog',;
                             LOWER(lctUngenAl)+'|'+LOWER(lctUngenAl),"lfvUngnAllo()")
    *B601460,1 Comment the next paragraph so that the message is not displayed
    *B601460,1 twice.
    *IF lnChoice = 2 .OR. lnChoice = 3
    *  lnChoice  = gfModalGen('TRM00101B00021','Dialog',lctUngenAl,"lfvUngnAllo()")
    *ENDIF
    *llAllo_Flag = IIF(lnChoice = 1,.T.,.F.)
    llAllo_Flag = lnChoice = 1
    *B601460,1 end
    laCompany[puCompany,9] = IIF(!llAllo_Flag,'F',' ')
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

    lcSav_ttl  = lcFile_Ttl
    lcSavBrFlds= lcBrFields          
    lcFile_Ttl = lctUngenAl
    *B601460,1 Fill lcBrFields with the browse fields of GLAUTHD file
    *B601460,1 Use global function gfDbfField() 
    *lcBrFields = ''
    lcBrFields = gfDbfField('GLAUTHD')
    *B601460,1 end    

    SELECT(lcTmpAutAlo)
    DIMENSION laTemp[1] 
    =gfBrows(.F.,"cbatchno",'laTemp',lcFile_Ttl,.F.)
    lcBrFields = lcSavBrFlds
    lcFile_Ttl = lcSav_ttl

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
    *B601460,1 Pass another parameter to gfModalGen
    *lnChoice = gfModalGen('TRM00101B00021','Dialog',lctUngenRc,"lfvUngnRec()")
    lnChoice = gfModalGen('QRM00101B00021','Dialog',;
                          LOWER(lctUngenRc)+'|'+LOWER(lctUngenRc),"lfvUngnRec()")
    *B601460,1 Comment the next paragraph so that the message is not displayed
    *B601460,1 twice.
    *IF lnChoice = 2 .OR. lnChoice = 3
    *  lnChoice  = gfModalGen('TRM00101B00021','Dialog',lctUngenRc,"lfvUngnRec()")
    *ENDIF
    *llRecu_Flag = IIF(lnChoice = 1,.T.,.F.)
    llRecu_Flag = lnChoice = 1
    *B601460,1 end
    laCompany[puCompany,9] = IIF(!llRecu_Flag,'F',' ')
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

    lcSav_ttl  = lcFile_Ttl

    lcFile_Ttl = lctUngenRc

    SELECT(lcTmpAutRec)

    *B601460,1 Fill lcBrFields with the browse fields of GLAUTHD file
    *B601460,1 Use global function gfDbfField() 
    lcSavBrFlds = lcBrFields
    lcBrFields = gfDbfField('GLAUTHD')
    *B601460,1 end    

    DIMENSION laTemp[1]
    =gfBrows(.F.,"cbatchno",'laTemp',lcFile_Ttl,.F.)

    lcFile_Ttl = lcSav_ttl
    *B601460,1 Restore lcBrFields
    lcBrFields = lcSavBrFlds 
    *B601460,1 end    

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
    =gfThermo(lnTotTrns,lnCurtrns,lcTGenRevT,"")

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
           'Reversing transaction by '+gcUser_Id,;
           'On '+DTOC(ldTodayDate),&lcPTrnHd..DTRNREVDT,&lcPTrnHd..CTRNREVYR,;
           &lcPTrnHd..CTRNREVPR,'U','N','N',&lcPTrnHd..CSTANDARD,;
           &lcPTrnHd..CSRCJRNL,lnTotTrnDr,lnTotTrnCr,&lcPTrnHd..cSrcModul,;
           laCompany[puCompany,2])

    lnTotTrnCr = 0
    lnTotTrnDr = 0

    =gfAdd_Info('&lcTmpTrnHD')

    SELECT(lcPTrnHd)
  ENDSCAN

  IF lnCurtrns  < lnTotTrns
    FOR lnCounter = lnCurtrns TO lnTotTrns
      =gfThermo(lnTotTrns,lnCounter,lcTGenRevT,"")
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

      =gfThermo(lnTotTrns,lnCurtrns,lcTGenRevB,"")

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
      *E300692,5 Use FSPRD instead of SYCFSPRD
      *=SEEK(laCompany[puCompany,2]+&lcTmpTrnHD..CTRNPYR+&lcTmpTrnHD..CTRNPPRD,'SYCFSPRD')
      *E300789,1 Hesham (Start)
      *=SEEK(laCompany[puCompany,2]+&lcTmpTrnHD..CTRNPYR+&lcTmpTrnHD..CTRNPPRD,(lcFsPrd))
	  =SEEK(&lcTmpTrnHD..CTRNPYR+&lcTmpTrnHD..CTRNPPRD,(lcFsPrd))
	  *E300789,1 Hesham (End)
	  *E300692,5 Use FSPRD instead of SYCFSPRD
      *INSERT INTO &gcWorkDir.&lcTmpBatch;
             (CBATCHNO,CBATSTAT,LBATIND,CBATTYPE,CBATPYR,DBATPBEG,;
              DBATPEND,CBATREFER,CBATDESC,CSRCMODUL,CCOMP_ID);
       VALUES(PADL(lnBatchNo,FSIZE('CBATCHNO'),'0'),'U',.T.,;
             IIF(&lcTmpTrnHD..CSTANDARD = 'Y','N','S'),;
             &lcTmpTrnHD..CTRNPYR,SYCFSPRD.dFsppbgdt,SYCFSPRD.dFsppendt, ;
             'On '+DTOC(ldTodayDate),'Reversing batch by '+gcUser_Id,;
             &lcTmpTrnHD..cSrcModul,laCompany[puCompany,2])
             
      INSERT INTO &gcWorkDir.&lcTmpBatch;
             (CBATCHNO,CBATSTAT,LBATIND,CBATTYPE,CBATPYR,DBATPBEG,;
              DBATPEND,CBATREFER,CBATDESC,CSRCMODUL,CCOMP_ID);
       VALUES(PADL(lnBatchNo,FSIZE('CBATCHNO'),'0'),'U',.T.,;
             IIF(&lcTmpTrnHD..CSTANDARD = 'Y','N','S'),;
             &lcTmpTrnHD..CTRNPYR, &lcFsPrd..dFsppbgdt, &lcFsPrd..dFsppendt, ;
             'On '+DTOC(ldTodayDate),'Reversing batch by '+gcUser_Id,;
             &lcTmpTrnHD..cSrcModul,laCompany[puCompany,2])             
      *E300692,5 end       
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
      =gfThermo(lnTotTrns,lnCounter,lcTGenRevB,"")
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
    laCompany[puCompany,9] = IIF(!llUpdate,'F',' ')
    RETURN llUpdate
ENDCASE

=SEEK(laCompany[puCompany,2],'SYCCOMP')

************************** Update master files *****************************

*E300692,5 Use FSPRD instead of SYCFSPRD
*IF gfFLOCK("&lcTrnsDt.,&lcTrnsHd,&lcBatch.,&lcPtrnHd.,SYCFSPRD",.T.) ;
   .AND. gfRLOCK("SYCCOMP",.T.)
IF gfFLOCK("&lcTrnsDt.,&lcTrnsHd,&lcBatch.,&lcPtrnHd.,&lcFsPrd",.T.) ;
   .AND. gfRLOCK("SYCCOMP",.T.)
*E300692,5 end   
  IF lcClose = 'P'

    ** Update the current period of the selected company.
    SELECT SYCCOMP
    REPLACE CCURR_PRD WITH PADL(INT(VAL(CCURR_PRD))+1,2,'0')
    =gfAdd_Info('SYCCOMP')

    lcCurntPrd = PADL(INT(VAL(CCURR_PRD)),2,'0')
    ** Update fiscal period file.
    *E300692,5 Use FSPRD instead of SYCFSPRD
    *SELECT SYCFSPRD
    SELECT (lcFsPrd)
    *E300692,5 end
    SET ORDER TO TAG COMFYRPRDI
    *E300789,1 Hesham (Start)
    *SEEK laCompany[puCompany,2]+laCompany[puCompany,4]+laCompany[puCompany,3]
    SEEK laCompany[puCompany,4]+laCompany[puCompany,3]    
    *E300789,1 Hesham (End)
    ** Close curren period and lock it if required in glsetup.
    REPLACE lFspLocks WITH lFspLocks .OR. &lcSetup..lSetLokPd,;
            lFspclsds WITH .T.
    *E300692,5 Use FSPRD instead of SYCFSPRD
    *=gfAdd_Info('SYCFSPRD')
    =gfAdd_Info(lcFsPrd)
    *E300692,5 end
    ** Unlock next period to be the current.
    SKIP 1
    REPLACE lFspLocks WITH .F.
    *E300692,5 Use FSPRD instead of SYCFSPRD
    *=gfAdd_Info('SYCFSPRD')
    =gfAdd_Info(lcFsPrd)
    *E300692,5 end
  ENDIF
  ** Remove all posted batches ( and voided if requird )
  ** posted in current period.
  SELECT(lcBatch)
  
  *B601842,1 Change this line to prevent deleting posted Batches from
  *the GLBATCH file [Begin]
  *DELETE FOR  cBatStat $ lcDelScop .AND.;
  *           (BETWEEN(dBatPBeg,ldFsppBgDt,ldFsppEnDt) .OR.;
  *            BETWEEN(dBatPEnd,ldFsppBgDt,ldFsppEnDt))
  *B601842,1 IF Statment to check if we are going to delete the Void Batches
  IF !EMPTY(STRTRAN(lcDelScop , 'P' , ''))
    DELETE FOR  cBatStat $ STRTRAN(lcDelScop , 'P' , '') .AND.;
               (BETWEEN(dBatPBeg,ldFsppBgDt,ldFsppEnDt) .OR.;
                BETWEEN(dBatPEnd,ldFsppBgDt,ldFsppEnDt))
  
  ENDIF    && End of IF
  *B601842,1 Change this line to prevent deleting posted Batches [End]

  ** Remove all posted transactions ( and voided if requird )
  ** posted in current period.
  SELECT(lcTrnsHd)
  SET ORDER TO TAG TRANSTAT
  SET RELATION TO CBATCHNO+CTRANNO INTO &lcTrnsDT ADDITIVE

  *B601960,1 Change these line to remove all posted transactions
  *(and voided if requird) posted in [or before] the current period [Begin]
  *SCAN FOR cTrnStat $ lcDelScop ;
  *    .AND. cTrnPprd = laCompany[puCompany,3]
  SCAN FOR cTrnStat $ lcDelScop ;
      .AND. cTrnPprd <= laCompany[puCompany,3]
  *B601960,1 Change these line to remove all posted transactions [End]

*E300431,1 M.H Begin.
    =lfThermo('Remove Voided Batches & Transactions')
*E300431,1 M.H End.
    SELECT(lcTrnsDt)
    DELETE REST FOR cBatchNo+cTranNo = ;
                      &lcTrnsHd..cBatchNo+&lcTrnsHd..cTranNo
    SELECT(lcTrnsHd)
    DELETE
  ENDSCAN
*E300431,1 M.H Begin.
  WAIT CLEAR
*E300431,1 M.H End.
  ** If there is any generated revers transactions/batches
  ** Update posted transaction file and add the generated
  ** transactions/batches.

  IF llRevers

    ** Change the transations revers status to revesed.
    SELECT(lcPTrnHd)
    
    *B601960,1 Change this line to change the For condition to make the
    *REPLACE statement work on all transactions that belongs to the current
    *period or before it and have reverse entry [Begin]
    
    *REPLACE FOR cTrnRever = 'Y' .AND.;
    *            cTrnPprd  = laCompany[puCompany,3];
    *            cTrnRever WITH 'R',;
    *            cComp_Id  WITH laCompany[puCompany,2],;
    *            cAdd_User WITH gcUser_ID,;
    *            dAdd_Date WITH DATE(),;
    *            cAdd_Time WITH gfGetTime()
    
    REPLACE FOR cTrnRever = 'Y' .AND.;
                dTrnPDate <= ldFsppEnDt;
                cTrnRever WITH 'R',;
                cComp_Id  WITH laCompany[puCompany,2],;
                cAdd_User WITH gcUser_ID,;
                dAdd_Date WITH DATE(),;
                cAdd_Time WITH gfGetTime()
    
    *B601960,1 Change this line to change the For condition [End]
    
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
      =gfThermo(lnTotTrns,lnCurtrns,lcTUpdMast,"")

      ** Compair if the batch no = the old batch or not. If not
      ** we are going to creat a new batch no.
      IF lcBatchKey <> cBatchNo

        lcBatchKey = cBatchNo

        ** Creating New batch No.

        *E300663,1 Change this line for the changes we have 
        *          made to (gfSequence) [Begin]
        *lcBatchNo = PADL(gfSequence("BATCH",1,laCompany[puCompany,2]),6,'0')
        lcBatchNo = gfSequence('CBATCHNO', laCompany[puCompany,2])
        *E300663,1 Change this line [End]
        
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

        *E300663,1 Change this line for the changes we have 
        *          made to (gfSequence) [Begin]
        *lcTranNo = PADL(gfSequence("TRANSACT",1,laCompany[puCompany,2]),8,'0')
        lcTranNo = gfSequence('CTRANNO' , laCompany[puCompany,2])
        *E300663,1 Change this line [End]
        
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
        =gfThermo(lnTotTrns,lnCounter,"lcTUpdMast","")
      ENDFOR
    ENDIF
  ENDIF

  ** Set the relation off ***
  SELECT(lcTrnsHd)
  SET RELATION TO

  SELECT(lcTrnsDt)
  SET RELATION TO

  SELECT(lcTmpBatch)
  SET RELATION TO

  SELECT(lcPTrnHd)
  SET RELATION TO

  SELECT(lcTmpTrnHd)
  SET RELATION TO

  SELECT(lcTmpTrnDt)
  SET RELATION TO

  ** Unlock the locked file
  *E300692,5 Use FSPRD instead of SYCFSPRD
  *=gfFLOCK("&lcTrnsDt.,&lcTrnsHd,&lcBatch.,&lcPtrnHd.,SYCFSPRD",.F.);
   .AND. gfRLOCK("SYCCOMP",.F.)
  =gfFLOCK("&lcTrnsDt.,&lcTrnsHd,&lcBatch.,&lcPtrnHd.,&lcFsPrd",.F.);
   .AND. gfRLOCK("SYCCOMP",.F.)
  *E300692,5 end
  ** MESSAGE : "Closing period processing is finished."
  **           "Current period is ð.                  "
  ** Choices : "                  ® Ok ¯              "
  IF lcClose = 'P'
    =gfModalGen('TRM00104B00000','Dialog',lcCurntPrd)
    laCompany[puCompany,9] = 'C'
  ENDIF  
ELSE
  laCompany[puCompany,9] = 'L'
  *E300692,5 Use FSPRD instead of SYCFSPRD
  *=gfFLOCK("&lcTrnsDt.,&lcTrnsHd,&lcBatch.,&lcPtrnHd.,SYCFSPRD",.F.);
   .AND. gfRLOCK("SYCCOMP",.F.)
  =gfFLOCK("&lcTrnsDt.,&lcTrnsHd,&lcBatch.,&lcPtrnHd.,&lcFsPrd",.F.);
   .AND. gfRLOCK("SYCCOMP",.F.)
  *E300692,5 end
ENDIF

************** CLOSING YEAR & PERIOD **************

*!**************************************************************************
*!
*!      Function : lfAdjBalnc
*!
*!**************************************************************************
*
FUNCTION lfAdjBalnc

*B601646,1 Add private variables lcLastYear, lcLastPrd
*PRIVATE lcFisYear
PRIVATE lcFisYear, lcLastYear, lcLastPrd
*B601646,1 end

** Lock the bacht file to update.
IF gfFLOCK("&lcAcBals.",.T.)

  ** Variable to hold the new current year.
  lcFisYear = STR(INT(VAL(laCompany[puCompany,4])+1),4)

  ** if the closing date is 'V'alid or 'I'nvalid.
  IF lcClosStat $ 'IV'
    SELECT(lcAcBals)
    lcSavOrder = SET('ORDER')
    SET ORDER TO TAG FISFYEAR

    ** Delete all the balances for the posting period is greater 
    ** than the closing period.
    lcPrdTDel = PADL(INT(VAL(laCompany[puCompany,3])+1),2,'0')
    IF SEEK (laCompany[puCompany,4]+lcPrdTDel)
      DELETE REST WHILE cfisfyear = laCompany[puCompany,4]
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
    *E300692,5 Use FISHD instead of SYCFISHD
    *SELECT SYCFISHD
    SELECT (lcFisHd)
    *E300692,5 end
    SET ORDER TO TAG COMPFYEAR
    *E300789,1 Hesham (Start)    
    *LOCATE FOR cComp_Id + cFisFYear + cFisYStat =;
               laCompany[puCompany,2] + lcFisYear + 'C'
    LOCATE FOR cFisFYear + cFisYStat = lcFisYear + 'C'
    *E300789,1 Hesham (End)           
    IF FOUND()               
      ** Variable to hold the no of periods of the new next year.
      *E300692,5 Use FISHD instead of SYCFISHD
      *lnNoOfPrd = INT(VAL(SYCFISHD.cFisNoPrd))
      lnNoOfPrd = INT(VAL(&lcFisHd..cFisNoPrd))
      *E300692,5 end
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

      *B602728,1 Create numeric fields with the actual field sizes to hold 
      *B602728,1 all possible balances.
      *SELECT &lcAcChar..CACCTCODE,;
             lcFisYear AS 'CFISFYEAR',;
             &lcPeriods..CFSPPRDID,;
             0.00 AS 'NACBPTDDR',;
             0.00 AS 'NACBPTDCR',;
             0.00 AS 'NACBYTDDR',;
             0.00 AS 'NACBYTDCR',;
             IIF(LEFT(&lcAcChar..ctypecode,1) $ 'ALQY',;
                      &lcAcBals..NACBCLBAL,0.00) AS 'NACBOPBAL',;
             IIF(LEFT(&lcAcChar..ctypecode,1) $ 'ALQY',;
                      &lcAcBals..NACBCLBAL,0.00) AS 'NACBCLBAL',;
             gcUser_ID   AS 'CADD_USER',;
             DATE()      AS 'DADD_DATE',;
             gfGetTime() AS 'CADD_TIME',;
             lfThermo(lcTAdjBal);
        FROM &lcAcChar,&lcAcBals,&lcPeriods;
       WHERE &lcAcBals..CACCTCODE = &lcAcChar..CACCTCODE;
         AND &lcAcBals..CFISFYEAR = laCompany[puCompany,4];
         AND &lcAcBals..CFSPPRDID = laCompany[puCompany,3];
        INTO DBF &gcWorkDir.&lcTempBal

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
             gcUser_ID   AS 'CADD_USER',;
             DATE()      AS 'DADD_DATE',;
             gfGetTime() AS 'CADD_TIME',;
             lfThermo(lcTAdjBal);
        FROM &lcAcChar,&lcAcBals,&lcPeriods;
       WHERE &lcAcBals..CACCTCODE = &lcAcChar..CACCTCODE;
         AND &lcAcBals..CFISFYEAR = laCompany[puCompany,4];
         AND &lcAcBals..CFSPPRDID = laCompany[puCompany,3];
        INTO DBF &gcWorkDir.&lcTempBal
      *B602728,1 end
      
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
  
  *E300692,5 Use FISHD instead of SYCFISHD
  *SELECT SYCFISHD
  SELECT (lcFisHd)
  *E300692,5 end
  SET ORDER TO TAG COMPFYEAR    
*E300789,1 Hesham (Start)
*  LOCATE FOR cComp_Id + cFisFYear + cFisYStat =;
             laCompany[puCompany,2] + lcFisYear + 'N'
  LOCATE FOR cFisFYear + cFisYStat = lcFisYear + 'N'
             
*E300789,1 Hesham (End)             
  IF FOUND()               
    ** Variable to hold the no of periods of the new next year.
    *E300692,5 Use FISHD instead of SYCFISHD
    *lnNoOfPrd = INT(VAL(SYCFISHD.cFisNoPrd))
    lnNoOfPrd = INT(VAL(&lcFisHd..cFisNoPrd))
    *E300692,5 end
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

    *B601646,1 Get closing balances of the last period in GLACBALS   
    *B601646,1 If closing year before the year ends normally, create 
    *B601646,1 the balance records as usual
    IF llPrdChange
    *B601646,1 end
      *B602728,1 Create numeric fields with the actual field sizes to hold 
      *B602728,1 all possible balances.
      *SELECT &lcAcChar..CACCTCODE,;
             lcFisYear AS 'CFISFYEAR',;
             &lcPeriods..CFSPPRDID,;
             0.00 AS 'NACBPTDDR',;
             0.00 AS 'NACBPTDCR',;
             0.00 AS 'NACBYTDDR',;
             0.00 AS 'NACBYTDCR',;
             IIF(LEFT(&lcAcChar..ctypecode,1) $ 'ALQY',;
                      &lcAcBals..NACBCLBAL,0.00) AS 'NACBOPBAL',;
             IIF(LEFT(&lcAcChar..ctypecode,1) $ 'ALQY',;
                      &lcAcBals..NACBCLBAL,0.00) AS 'NACBCLBAL',;
             gcUser_ID   AS 'CADD_USER',;
             DATE()      AS 'DADD_DATE',;
             gfGetTime() AS 'CADD_TIME',;
             lfThermo(lcTAdjBal);
        FROM &lcAcChar,&lcAcBals,&lcPeriods;
       WHERE &lcAcBals..CACCTCODE = &lcAcChar..CACCTCODE;
         AND &lcAcBals..CFISFYEAR = laCompany[puCompany,4];
         AND &lcAcBals..CFSPPRDID = laCompany[puCompany,3];
        INTO DBF &gcWorkDir.&lcTempBal
	
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
             gcUser_ID   AS 'CADD_USER',;
             DATE()      AS 'DADD_DATE',;
             gfGetTime() AS 'CADD_TIME',;
             lfThermo(lcTAdjBal);
        FROM &lcAcChar,&lcAcBals,&lcPeriods;
       WHERE &lcAcBals..CACCTCODE = &lcAcChar..CACCTCODE;
         AND &lcAcBals..CFISFYEAR = laCompany[puCompany,4];
         AND &lcAcBals..CFSPPRDID = laCompany[puCompany,3];
        INTO DBF &gcWorkDir.&lcTempBal
     *B602728,1 end
               
        *B601646,1 Otherwise, use existing numbers  
    ELSE
      *B601646,1 lcLastYear = Current year + 1
      lcLastYear = STR(INT(VAL(laCompany[puCompany,4])) + 1, 4) 
      *B601646,1 Last period 
      *E300692,5 Use FISHD instead of SYCFISHD
      *lcLastPrd  = SYCFISHD.cFisNoPrd
      lcLastPrd  = &lcFisHd..cFisNoPrd
      *E300692,5 end

      *B602728,1 Create numeric fields with the actual field sizes to hold 
      *B602728,1 all possible balances.
      *SELECT &lcAcChar..CACCTCODE,;
             lcFisYear AS 'CFISFYEAR',;
             &lcPeriods..CFSPPRDID,;
             0.00 AS 'NACBPTDDR',;
             0.00 AS 'NACBPTDCR',;
             0.00 AS 'NACBYTDDR',;
             0.00 AS 'NACBYTDCR',;
             IIF(LEFT(&lcAcChar..ctypecode,1) $ 'ALQY',;
                      &lcAcBals..NACBCLBAL,0.00) AS 'NACBOPBAL',;
             IIF(LEFT(&lcAcChar..ctypecode,1) $ 'ALQY',;
                      &lcAcBals..NACBCLBAL,0.00) AS 'NACBCLBAL',;
             gcUser_ID   AS 'CADD_USER',;
             DATE()      AS 'DADD_DATE',;
             gfGetTime() AS 'CADD_TIME',;
             lfThermo(lcTAdjBal);
        FROM &lcAcChar,&lcAcBals,&lcPeriods;
       WHERE &lcAcBals..CACCTCODE = &lcAcChar..CACCTCODE;
         AND &lcAcBals..CFISFYEAR = lcLastYear;
         AND &lcAcBals..CFSPPRDID = lcLastPrd;
        INTO DBF &gcWorkDir.&lcTempBal
	
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
             gcUser_ID   AS 'CADD_USER',;
             DATE()      AS 'DADD_DATE',;
             gfGetTime() AS 'CADD_TIME',;
             lfThermo(lcTAdjBal);
        FROM &lcAcChar,&lcAcBals,&lcPeriods;
       WHERE &lcAcBals..CACCTCODE = &lcAcChar..CACCTCODE;
         AND &lcAcBals..CFISFYEAR = lcLastYear;
         AND &lcAcBals..CFSPPRDID = lcLastPrd;
        INTO DBF &gcWorkDir.&lcTempBal
      *B602728,1 end
      *B601646,1 end
    ENDIF
    *B601646,1 end
 
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
  laCompany[puCompany,9] = 'L'
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

WAIT lcTAdjPsYP WINDOW NOWAIT

PRIVATE lcFisYear

lcFisYear = STR(INT(VAL(laCompany[puCompany,4])+1),4)  && New Current Year

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
    SCAN FOR dTrnPDate > laCompany[puCompany,10]
      *E300692,5 Use FSPRD instead of SYCFSPRD
      *SELECT SYCFSPRD
      SELECT (lcFsPrd)
      *E300692,5 end
      *E300789,1 Hesham (Start)
      *LOCATE FOR cComp_Id + cFisfYear = laCompany[puCompany,2] + lcFisYear;
             AND BETWEEN(&lcPTrnHd..dTrnPDate,dFsppBgDt,dFsppEnDt)
      LOCATE FOR  cFisfYear =  lcFisYear;
             AND BETWEEN(&lcPTrnHd..dTrnPDate,dFsppBgDt,dFsppEnDt)
      *E300789,1 Hesham (End) 
      IF FOUND()
        SELECT(lcPTrnDt)
        *E300692,5 Use FSPRD instead of SYCFSPRD
        *REPLACE REST WHILE cBatchNo + cTranNo = ;
                           &lcPTrnHd..cBatchNo + &lcPTrnHd..cTranNo;
                           cTrnPYr   WITH SYCFSPRD.cFisfYear,;
                           cTrnPPrd  WITH SYCFSPRD.cFspprdid,;
                           cAdd_User WITH gcUser_ID,;
                           dAdd_Date WITH DATE(),;
                           cAdd_Time WITH gfGetTime()
        REPLACE REST WHILE cBatchNo + cTranNo = ;
                           &lcPTrnHd..cBatchNo + &lcPTrnHd..cTranNo;
                           cTrnPYr   WITH &lcFsPrd..cFisfYear,;
                           cTrnPPrd  WITH &lcFsPrd..cFspprdid,;
                           cAdd_User WITH gcUser_ID,;
                           dAdd_Date WITH DATE(),;
                           cAdd_Time WITH gfGetTime()
        *E300692,5 end
        SELECT(lcPTrnHd)
        *E300692,5 Use FSPRD instead of SYCFSPRD
        *REPLACE cTrnPYr   WITH SYCFSPRD.cFisfYear,;
                cTrnPPrd  WITH SYCFSPRD.cFspprdid,;
                cAdd_User WITH gcUser_ID,;
                dAdd_Date WITH DATE(),;
                cAdd_Time WITH gfGetTime()
        REPLACE cTrnPYr   WITH &lcFsPrd..cFisfYear,;
                cTrnPPrd  WITH &lcFsPrd..cFspprdid,;
                cAdd_User WITH gcUser_ID,;
                dAdd_Date WITH DATE(),;
                cAdd_Time WITH gfGetTime()        
        *E300692,5 end        
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
    SCAN FOR dTrnPDate > laCompany[puCompany,10];
         AND cTrnStat <> 'V'
      *E300692,5 Use FSPRD instead of SYCFSPRD 
      *SELECT SYCFSPRD
      SELECT (lcFsPrd)
      *E300692,5 end
      *E300789,1 Hesham (Start)
      *LOCATE FOR cComp_Id + cFisfYear = laCompany[puCompany,2] + lcFisYear;
             AND BETWEEN(&lcTrnsHd..dTrnPDate,dFsppBgDt,dFsppEnDt)
      LOCATE FOR cFisfYear = lcFisYear;
             AND BETWEEN(&lcTrnsHd..dTrnPDate,dFsppBgDt,dFsppEnDt)

      *E300789,1 Hesham (End)       
      IF FOUND()
        SELECT(lcTrnsDt)
        *E300692,5 Use FSPRD instead of SYCFSPRD 
        *REPLACE REST WHILE cBatchNo + cTranNo = ;
                           &lcTrnsHd..cBatchNo + &lcTrnsHd..cTranNo;
                           &lcTrnsDt..cTrnPYr   WITH SYCFSPRD.cFisfYear,;
                           &lcTrnsDt..cTrnPPrd  WITH SYCFSPRD.cFspprdid,;
                           &lcTrnsDt..cAdd_User WITH gcUser_ID,;
                           &lcTrnsDt..dAdd_Date WITH DATE(),;
                           &lcTrnsDt..cAdd_Time WITH gfGetTime()
        REPLACE REST WHILE cBatchNo + cTranNo = ;
                           &lcTrnsHd..cBatchNo + &lcTrnsHd..cTranNo;
                           &lcTrnsDt..cTrnPYr   WITH &lcFsPrd..cFisfYear,;
                           &lcTrnsDt..cTrnPPrd  WITH &lcFsPrd..cFspprdid,;
                           &lcTrnsDt..cAdd_User WITH gcUser_ID,;
                           &lcTrnsDt..dAdd_Date WITH DATE(),;
                           &lcTrnsDt..cAdd_Time WITH gfGetTime()
        *E300692,5 end                   
        SELECT(lcTrnsHd)
        *E300692,5 Use FSPRD instead of SYCFSPRD 
        *REPLACE cTrnPYr   WITH SYCFSPRD.cFisfYear,;
                cTrnPPrd  WITH SYCFSPRD.cFspprdid,;
                cAdd_User WITH gcUser_ID,;
                dAdd_Date WITH DATE(),;
                cAdd_Time WITH gfGetTime()
		REPLACE cTrnPYr   WITH &lcFsPrd..cFisfYear,;
                cTrnPPrd  WITH &lcFsPrd..cFspprdid,;
                cAdd_User WITH gcUser_ID,;
                dAdd_Date WITH DATE(),;
                cAdd_Time WITH gfGetTime()                
        *E300692,5 end        
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
  laCompany[puCompany,9] = 'L'
ENDIF  

WAIT CLEAR

*!**************************************************************************
*!
*!      Function : lfBatHandl
*!
*!**************************************************************************
*
FUNCTION lfBatHandl

WAIT WINDOW lcTBatHand NOWAIT

*E300692,5 Use FISHD instead of SYCFISHD
*SELECT SYCFISHD
SELECT (lcFisHd)
*E300692,5 end
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
                AND BETWEEN(dTrnPDate,laCompany[puCompany,10],;
                                      laCompany[puCompany,7]);
                    cTrnStat  WITH 'I',;
                    cAdd_User WITH gcUser_ID,;
                    dAdd_Date WITH DATE(),;
                    cAdd_Time WITH gfGetTime()
    
    ** Assing all the transaction that have no batches and the
    ** transaction posting date is between the closing date and 
    ** the old end date of the current year to status Invalid.

    SELECT(lcPTrnHd)
    REPLACE ALL FOR cBatchNo = '000000';
                AND BETWEEN(dTrnPDate,laCompany[puCompany,10],;
                                      laCompany[puCompany,7]);
                    cTrnStat  WITH 'I',;
                    cAdd_User WITH gcUser_ID,;
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
      SCAN FOR BETWEEN(dBatPBeg,laCompany[puCompany,10],;
                                laCompany[puCompany,7]);
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
                           cAdd_User WITH gcUser_ID,;
                           dAdd_Date WITH DATE(),;
                           cAdd_Time WITH gfGetTime()
                         
        ** We are going to assign all the batchs that liy between the
        ** closing date and the end date of the closing year to status
        ** 'I'nvalid.

        SELECT(lcBatch)
        REPLACE cBatStat  WITH 'I',;
                cAdd_User WITH gcUser_ID,;
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
    DO lpOvrLapBt WITH laCompany[puCompany,10]
    APPEND FROM &gcWorkDir.&lcTmpBatch
  ENDIF
  ** The end date of the old current year is changed.
  IF llDatChange .AND. ldDatChange < laCompany[puCompany,7]
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
  laCompany[puCompany,9] = 'L'
  =gfFLOCK("&lcBatch.,&lcPTrnHd.,&lcPTrnDt.,&lcTrnsHd.,&lcTrnsDt.",.F.)
ENDIF  

WAIT CLEAR

*!**************************************************************************
*!
*!      Procedure : lpOvrLapBt
*!
*!**************************************************************************
*
PROCEDURE lpOvrLapBt

PARAMETERS ldEndDate

PRIVATE lnBeforTotBatDr,lnBeforTotBatCr,;
        lnAfterTotBatDr,lnAfterTotBatCr,;
        lcBefor_Bat_No,lcAfter_Bat_No,;
        lcFisYear,lcPostPr,llCreatTrn
        
lcCurArea = ALIAS()  && Store the selected work area.
llFromPost= .F.
lcFisYear = IIF(STR(YEAR(ldEndDate),4) < laCompany[puCompany,4],STR(YEAR(ldEndDate)+2,4),STR(YEAR(ldEndDate)+1,4))

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
                cAdd_User WITH gcUser_ID,;
                dAdd_Date WITH DATE(),;
                cAdd_Time WITH gfGetTime()
      ELSE
        ** We are going to seek in the 'SYCFSPRD' for the current
        ** selected company and the new current year.
        *E300692,5 Use FSPRD instead of SYCFSPRD 
        *SELECT SYCFSPRD
        SELECT (lcFsPrd)
        *E300692,5 end
        *E300789,1 Hesham (Start)
        *LOCATE FOR cComp_Id =laCompany[puCompany,2] .AND.;
                  cFisfYear =laCompany[puCompany,4] .AND.;
                  BETWEEN(&lc_TrnHd..dTrnPDate,dFsppBgDt,dFsppEnDt)
        LOCATE FOR cFisfYear =laCompany[puCompany,4] .AND.;
                  BETWEEN(&lc_TrnHd..dTrnPDate,dFsppBgDt,dFsppEnDt)

        *E300789,1 Hesham (End)
        IF FOUND()
          ** We are going to assign the period no to this variable.
          *E300692,5 Use FSPRD instead of SYCFSPRD 
          *lcPostPr = SYCFSPRD.cFspprdid
          lcPostPr = &lcFsPrd..cFspprdid
          *E300692,5 end
        ENDIF

        IF EMPTY(lcBefor_Bat_No)
          ** Generate a new befor batch no for the transactions that
          ** liy befor the closing date.

          *E300663,1 Change this line for the changes we have 
          *          made to (gfSequence) [Begin]
          *lcBefor_Bat_No = PADL(gfSequence("BATCH",1,laCompany[puCompany,2]),6,'0')
          lcBefor_Bat_No = gfSequence('CBATCHNO' , laCompany[puCompany,2])
          *E300663,1 Change this line [End]
          
        ENDIF  
        
        ** Generate the transaction no.

        *E300663,1 Change this line for the changes we have 
        *          made to (gfSequence) [Begin]
        *lcTranNo = PADL(gfSequence("TRANSACT",1,laCompany[puCompany,2]),8,'0')
        lcTranNo = gfSequence('CTRANNO' , laCompany[puCompany,2])
        *E300663,1 Change this line [End]
        
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
          m.cAdd_User = gcUser_ID
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
        m.cAdd_User = gcUser_ID
        m.dAdd_Date = DATE()
        m.cAdd_Time = gfGetTime()

        lnRecNo = RECNO()
        APPEND BLANK
        GATHER MEMVAR MEMO
        GO lnRecNo

        SELECT(lcTmpReport)
        APPEND BLANK
        REPLACE CBATCHNO  WITH lcBefor_Bat_No,; 
                CTRANNO   WITH lcTranNo,;
                CDESCRIP  WITH 'Reclacified from the '+;
                               IIF(llFromPost,;
                               'posted transaction header file',;
                               'transaction header file'),;
                COLDBATNO WITH &lc_TrnHd..CBatchNo,;
                COLDTRNNO WITH &lc_TrnHd..cTranNo

        SELECT(lc_TrnHd)
        
        ** We are going to assign the old transaction that we create the
        ** new one from by status 'R'eclassefied.
        
        REPLACE cTrnStat  WITH 'R',;
                cAdd_User WITH gcUser_ID,;
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

      *E300663,1 Change this line for the changes we have 
      *          made to (gfSequence) [Begin]
      *lcBefor_Bat_No = PADL(gfSequence("BATCH",1,laCompany[puCompany,2]),6,'0')
      lcBefor_Bat_No = gfSequence('CBATCHNO' , laCompany[puCompany,2])
      *E300663,1 Change this line [End]
      
    ENDIF
    
    ** If there is no batch no we are not going to add anything in the
    ** batch file.
    
    IF EMPTY(lcBefor_Bat_No)
      REPLACE cBatStat  WITH 'I',;
              cAdd_User WITH gcUser_ID,;
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
      m.cAdd_User = gcUser_ID
      m.dAdd_Date = DATE()
      m.cAdd_Time = gfGetTime()
      
      SELECT(lcTmpBatch)
      APPEND BLANK
      GATHER MEMVAR MEMO
      
      SELECT(lcTmpReport)
      APPEND BLANK
      REPLACE CBATCHNO  WITH lcBefor_Bat_No,; 
              CTRANNO   WITH ' ',;
              CDESCRIP  WITH lcTRclasfi,;
              COLDBATNO WITH &lcBatch..CBatchNo,;
              COLDTRNNO WITH ' '

      SELECT(lcBatch)
      ** We are going to assign the old batch status with 'R'eclassified.
      REPLACE cBatStat  WITH 'R',;
              cAdd_User WITH gcUser_ID,;
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

      *E300663,1 Change this line for the changes we have 
      *          made to (gfSequence) [Begin]
      *lcTranNo = PADL(gfSequence("TRANSACT",1,laCompany[puCompany,2]),8,'0')
      lcTranNo = gfSequence('CTRANNO' , laCompany[puCompany,2])
      *E300663,1 Change this line [End]
      
      IF &lc_TrnHd..dTrnPDate <= ldEndDate
        IF EMPTY(lcBefor_Bat_No)
          ** Generate a new befor batch no for the transactions that liy
          ** befor the closing date.
          llTrnBefor = .T.

          *E300663,1 Change this line for the changes we have 
          *          made to (gfSequence) [Begin]
          *lcBefor_Bat_No = PADL(gfSequence("BATCH",1,laCompany[puCompany,2]),6,'0')
          lcBefor_Bat_No = gfSequence('CBATCHNO' , laCompany[puCompany,2])
          *E300663,1 Change this line [End]
          
        ENDIF
      ELSE
        IF EMPTY(lcAfter_Bat_No)
          ** Generate a new after batch no for the transactions that liy
          ** after the closing date.
          llTrnBefor = .F.

          *E300663,1 Change this line for the changes we have 
          *          made to (gfSequence) [Begin]
          *lcAfter_Bat_No = PADL(gfSequence("BATCH",1,laCompany[puCompany,2]),6,'0')
          lcAfter_Bat_No = gfSequence('CBATCHNO' , laCompany[puCompany,2])
          *E300663,1 Change this line [End]
          
        ENDIF
      ENDIF

      ** We are going to seek in the 'SYCFSPRD' for the current selected
      ** company and the new current year.
      *E300692,5 Use FSPRD instead of SYCFSPRD 
      *SELECT SYCFSPRD
      SELECT (lcFsPrd)
      *E300692,5 end
      *E300789,1 Hesham (Start)
      *LOCATE FOR cComp_Id  = laCompany[puCompany,2];
             AND cFisfYear = IIF(llTrnBefor,laCompany[puCompany,4],lcFisYear);
             AND BETWEEN(&lc_TrnHd..dTrnPDate,dFsppBgDt,dFsppEnDt)
      LOCATE FOR cFisfYear = IIF(llTrnBefor,laCompany[puCompany,4],lcFisYear);
             AND BETWEEN(&lc_TrnHd..dTrnPDate,dFsppBgDt,dFsppEnDt)

      *E300789,1 Hesham (Start)
      IF FOUND()
        ** We are going to assign the period no to this variable.
        *E300692,5 Use FSPRD instead of SYCFSPRD 
        *lcPostPr = SYCFSPRD.cFspprdid
        lcPostPr = &lcFsPrd..cFspprdid
        *E300692,5 end
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
        m.cTrnPYr   = IIF(llTrnBefor,lcFisYear,laCompany[puCompany,4])
        m.cTrnPPrd  = lcPostPr
        m.cAdd_User = gcUser_ID
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
      m.cTrnPYr   = IIF(llTrnBefor,lcFisYear,laCompany[puCompany,4])
      m.cTrnPPrd  = lcPostPr
      m.cAdd_User = gcUser_ID
      m.dAdd_Date = DATE()
      m.cAdd_Time = gfGetTime()
    
      lnRecNo = RECNO()
      APPEND BLANK
      GATHER MEMVAR MEMO
      GO lnRecNo

      SELECT(lcTmpReport) 
      APPEND BLANK
      REPLACE CBATCHNO  WITH IIF(llTrnBefor,lcBefor_Bat_No,lcAfter_Bat_No),; 
              CTRANNO   WITH lcTranNo,;
              CDESCRIP  WITH 'Reclacified from the '+;
                             IIF(llFromPost,;
                             'posted transaction header file',;
                             'transaction header file'),;
              COLDBATNO WITH &lc_TrnHd..CBatchNo,;
              COLDTRNNO WITH &lc_TrnHd..cTranNo

      SELECT(lc_TrnHd)

      ** Replace the status of the over lapping transaction with 
      ** 'R'eclassified.
      
      REPLACE cTrnStat  WITH 'R',;
              cAdd_User WITH gcUser_ID,;
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

      *E300663,1 Change this line for the changes we have 
      *          made to (gfSequence) [Begin]
      *lcBefor_Bat_No = PADL(gfSequence("BATCH",1,laCompany[puCompany,2]),6,'0')
      lcBefor_Bat_No = gfSequence('CBATCHNO' , laCompany[puCompany,2])
      *E300663,1 Change this line [End]
      
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
      m.cBatPyr   = laCompany[puCompany,4]
      m.dBatPEnd  = ldEndDate
      m.nBatCnTot = IIF(lnBeforTotBatDr >= lnBeforTotBatCr,;
                        lnBeforTotBatDr,lnBeforTotBatCr)
      m.nBaTotDr  = lnBeforTotBatDr
      m.nBaTotCr  = lnBeforTotBatCr
      m.cAdd_User = gcUser_ID
      m.dAdd_Date = DATE()
      m.cAdd_Time = gfGetTime()
      
      SELECT(lcTmpBatch)
      APPEND BLANK
      GATHER MEMVAR MEMO

      SELECT(lcTmpReport) 
      APPEND BLANK
      REPLACE CBATCHNO  WITH lcBefor_Bat_No,; 
              CTRANNO   WITH ' ',;
              CDESCRIP  WITH lcTRclasfi,;
              COLDBATNO WITH &lcBatch..CBatchNo,;
              COLDTRNNO WITH ' '
              
      SELECT(lcBatch)
    ENDIF  

    SELECT(lcBatch)
    IF cBatStat <> 'P' AND EMPTY(lcAfter_Bat_No)
      ** Generate a new befor batch no for the transactions that liy
      ** after the closing date.

      *E300663,1 Change this line for the changes we have 
      *          made to (gfSequence) [Begin]
      *lcAfter_Bat_No = PADL(gfSequence("BATCH",1,laCompany[puCompany,2]),6,'0')
      lcAfter_Bat_No = gfSequence('CBATCHNO' , laCompany[puCompany,2])
      *E300663,1 Change this line [End]
      
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
      m.dBatPEnd  = IIF(llClosDate,laCompany[puCompany,7],ldOldNxtDat)
      m.nBatCNTot = IIF(lnAfterTotBatDr >= lnAfterTotBatCr,;
                       lnAfterTotBatDr,lnAfterTotBatCr)
      m.nBaTotDr  = lnAfterTotBatDr
      m.nBaTotCr  = lnAfterTotBatCr
      m.cAdd_User = gcUser_ID
      m.dAdd_Date = DATE()
      m.cAdd_Time = gfGetTime()

      SELECT(lcTmpBatch)
      APPEND BLANK
      GATHER MEMVAR MEMO

      SELECT(lcTmpReport) 
      APPEND BLANK
      REPLACE CBATCHNO  WITH lcAfter_Bat_No,; 
              CTRANNO   WITH ' ',;
              CDESCRIP  WITH lcTRclasfi,;
              COLDBATNO WITH &lcBatch..CBatchNo,;
              COLDTRNNO WITH ' '

      SELECT(lcBatch)
    ENDIF  

    llCreatTrn= .F.

    SELECT(lcBatch)
    ** Replace the old batch with status 'R'eclassified.
    REPLACE cBatStat  WITH 'R',;
            cComp_Id  WITH laCompany[puCompany,2],;
            cAdd_User WITH gcUser_ID,;
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

WAIT WINDOW lcTRPostBt NOWAIT

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
  
  SELECT(lcBatch)
  SCAN FOR &lcBatch..dBatPBeg > ldRePostDate ;
       AND &lcBatch..cBatStat = 'P'
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
  laCompany[puCompany,9] = 'L'
  =gfFLOCK("&lcAcBals.,&lcPTrnDt.",.F.)
ENDIF  

WAIT CLEAR

*!**************************************************************************
*!
*!      Procedure : lpUpd_Bal
*!
*!**************************************************************************
*
*  This procedure performs the actual update of amounts to the
*  lcAcBals file from the posted transactions detais file.
*  parameter       : variable containing the posting type,
*                    whether : 'T' = transaction(s)
*                              'B' = beginning balance batch(es)
*                              'N' = nonbeginning balance batch(es)
*
PROCEDURE lpUpd_Bal

PARAMETERS lcPostType

WAIT WINDOW lcTUpdBal NOWAIT

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
                         &lcAcBals..cAdd_User WITH gcUser_ID,;
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
                           &lcAcBals..cAdd_User WITH gcUser_ID,;
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
                             &lcAcBals..cAdd_User WITH gcUser_ID,;
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
              &lcAcBals..cAdd_User WITH gcUser_ID,;
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
                           &lcAcBals..cAdd_User WITH gcUser_ID,;
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
                             &lcAcBals..cAdd_User WITH gcUser_ID,;
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
                         &lcAcBals..cAdd_User WITH gcUser_ID,;
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
                           &lcAcBals..cAdd_User WITH gcUser_ID,;
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
                         &lcAcBals..cAdd_User WITH gcUser_ID,;
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
                           &lcAcBals..cAdd_User WITH gcUser_ID,;
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

WAIT WINDOW lcTGnClsEn NOWAIT

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

  SET RELATION TO cAcctCode+laCompany[puCompany,4]+laCompany[puCompany,3];
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
    lnCurr_Yer = INT(VAL(laCompany[puCompany,4])+1)
    
    ** Variable to hold the character value of the new current year.
    lcCurr_Yer = STR(lnCurr_Yer,4)

    ** Calling the posting function with type batch that need to be posted,
    ** the temp file name that hold the batch no,the program name that call
    ** the posting program,the active company,the data dir of the selected 
    ** company.

*    =lfTBPost('BATCH',lcTmpBatch,'CLOSING',' ',laCompany[puCompany,2],lcDataDir)
*    DO lfTBPost WITH 'BATCH',lcTmpBatch,'CLOSING',' ',laCompany[puCompany,2],lcDataDir IN (gcAppHome+"GL.APP")
     *E300581,1 Change program call to fill parameter
     
     DO (gcAppHome+"GL.APP") WITH "LFTBPOST WITH 'BATCH',lcTmpBatch,'CLOSING',' ',laCompany[puCompany,2],lcDataDir",'',"T"
 *    lcPram = ['BATCH',']+lcTmpBatch+[','CLOSING',' ',']+laCompany[puCompany,2]+[',']+lcDataDir+[']
*     DO (gcAppHome+"GL.APP") WITH ;
         "LFTBPOST",lcPram,'T'
     
*     DO (gcAppHome+"GL.APP") WITH ;
         "LFTBPOST","'BATCH',lcTmpBatch,'CLOSING',' ',laCompany[puCompany,2],lcDataDir","T"
     *E300581,1 end 
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
  laCompany[puCompany,9] = 'L'
  =gfFLOCK("&lcTrnsDt.,&lcTrnsHd,&lcBatch.",.F.)
ENDIF  

WAIT CLEAR

*!**************************************************************************
*!
*!      Function : lfCreatCls
*!
*!**************************************************************************
*  Creates a closing entries batch ,its transactions, and transaction
*  details from the previously created temporary file lcTmpClsEnt.
*
FUNCTION lfCreatCls

WAIT WINDOW lcTCrClsEn NOWAIT

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
lcUserName = LOOKUP(SYUUSER.cUsr_Name,gcUser_ID,SYUUSER.cUser_ID,'cUser_ID')

lnBatchNo  = 0    && Temp Batch No.
lnTranNo   = 0    && Temp Transaction No.
lnClosNo   = 0    && Temp Closing entries No.
lnTotBatDr = 0    && Reset batch's total debit.
lnTotBatCr = 0    && Reset batch's total credit.
lnTotTrnDr = 0    && Reset Transaction total debit.
lnTotTrnCr = 0    && Reset Transaction total credit.

** Reset closing entry sequence number in SYCSEQUN file.

*E300663,1 Remove this lines for the changes we have made to SYCSEQUN [Begin]
*IF !USED('SYCSEQUN')
*  SELECT 0
*  USE (gcSysHome+"SYCSEQUN")
*ELSE
*  SELECT SYCSEQUN
*ENDIF  
*
*SET ORDER TO TAG CSEQ_TYPE
*E300663,1 Remove this lines for the changes we have made to SYCSEQUN [End]

*E300663,1 Add this lines for the changes we have made to SYCSEQUN [Begin]
SELECT 0
lcSeqTmpNm = gfTempName()
USE (ALLTRIM(laCompany[puCompany,8]) + 'SEQUENCE') ;
     AGAIN ;
     ALIAS (lcSeqTmpNm) ;
     ORDER TAG CSEQ_TYPE
*E300663,1 Add this lines for the changes we have made to SYCSEQUN [End]

*E300663,1 Change this lines for the changes we have made to SYCSEQUN [Begin]
*IF SEEK('CLOSENT'+laCompany[puCompany,2])
*IF SEEK('CAUTCODE')
*E300663,1 Change this lines for the changes we have made to SYCSEQUN [End]
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
              dTrnPDate WITH laCompany[puCompany,10],;
              cTrnPYr   WITH laCompany[puCompany,4],;
              cTrnPPrd  WITH laCompany[puCompany,3]

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

    SELECT(lcTmpTrnHd)
 
    APPEND BLANK
    REPLACE cBatchNo  WITH PADL(lnBatchNo,FSIZE('CBATCHNO'),'0'),;
            cTranNo   WITH PADL(lnTranNo,FSIZE('CTRANNO'),'0'),;
            cTrnDesc  WITH 'Created by '+lcUserName,;
            cTrnRefer WITH lcTYECNo+' '+PADL(lnClosNo,FSIZE('CTRANNO'),'0'),;
            dTrnPDate WITH laCompany[puCompany,10],;
            cTrnPYr   WITH laCompany[puCompany,4],;
            cTrnPPrd  WITH laCompany[puCompany,3],;
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
                cTrDtExp WITH lcTClsEntN+' '+;
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

  SELECT(lcTmpBatch)

  APPEND BLANK

  REPLACE cBatchNo  WITH PADL(lnBatchNo,FSIZE('CBATCHNO'),'0'),;
          lBatInd   WITH .F.,;
          cBatType  WITH 'C',;
          cBatStat  WITH 'Y',;
          cBatPYr   WITH laCompany[puCompany,4],;
          dBatPBeg  WITH laCompany[puCompany,10],;
          dBatPEnd  WITH laCompany[puCompany,10],;
          cBatRefer WITH lcTYECNo+' '+PADL(lnClosNo,FSIZE('CTRANNO'),'0'),;
          cBatDesc  WITH lcTPrior+' '+lcUserName,;
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
    =gfThermo(lnTotTrns,lnCurtrns,lcTUpdMast,"")

    ** Compair if the batch no = the old batch or not.  If not
    ** we are going to creat a new batch no.
    IF lcBatchKey <> cBatchNo

      lcBatchKey = cBatchNo

      ** Creating New batch No.

      *E300663,1 Change this line for the changes we have 
      *          made to (gfSequence) [Begin]
      *lcBatchNo = PADL(gfSequence("BATCH",1,laCompany[puCompany,2]),6,'0')
      lcBatchNo = gfSequence('CBATCHNO' , laCompany[puCompany,2])
      *E300663,1 Change this line [End]
      
      SELECT(lcTmpBatch)
      SCATTER MEMVAR MEMO
      m.cBatchNo  = lcBatchNo
      m.nBatCnTot = 0
      m.nBatotDr  = 0
      m.nBatotCr  = 0
      m.cComp_Id  = laCompany[puCompany,2]
      m.cAdd_User = gcUser_ID
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

      *E300663,1 Change this line for the changes we have 
      *          made to (gfSequence) [Begin]
      *lcTranNo = PADL(gfSequence("TRANSACT",1,laCompany[puCompany,2]),8,'0')
      lcTranNo = gfSequence('CTRANNO' , laCompany[puCompany,2])
      *E300663,1 Change this line [End]
      
      ** Create New Closing entries No.

      *E300663,1 Change this line for the changes we have 
      *          made to (gfSequence) [Begin]
      *lcClosNo = PADL(gfSequence("CLOSENT",1,laCompany[puCompany,2]),8,'0')
      *lcClosNo = gfSequence('CAUTCODE' , laCompany[puCompany,2])
      lcClosNo = gfSequence('CCLOSENT', laCompany[puCompany,2],;
      						 '','','CTRANNO')
      *E300663,1 Change this line [End]
      
      SELECT(lcTmpTrnHd)
      SCATTER MEMVAR MEMO
      m.cBatchNo  = lcBatchNo
      m.cTranNo   = lcTranNo
      m.nTrnTotDr = 0
      m.nTrnTotCr = 0
      m.cTrnRefer = lcTYECNo+' '+lcClosNo
      m.cComp_Id  = laCompany[puCompany,2]
      m.cAdd_User = gcUser_ID
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
    m.cTrDtExp  = lcTClsEntN+' '+lcClosNo
    m.cAdd_User = gcUser_ID
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
            cBatRefer WITH lcTYECNo+' '+lcClosNo

    SELECT(lcTmpTrnDt)

  ENDSCAN

  SELECT(lcTrnsHd)
  REPLACE nTrnTotDr WITH lnTotTrnDr,;
          nTrnTotCr WITH lnTotTrnCr

  SELECT(lcBatch)
  REPLACE nBatCnTot WITH lnTotBatDr,;
          nBatotDr  WITH lnTotBatDr,;
          nBatotCr  WITH lnTotBatCr,;
          cBatRefer WITH lcTYECNo+' '+lcClosNo

  IF lnCurtrns < lnTotTrns
    FOR lnCounter = lnCurtrns TO lnTotTrns
      =gfThermo(lnTotTrns,lnCounter,"lcTUpdMast","")
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

WAIT WINDOW lcTCompFil NOWAIT

SELECT SYCCOMP
=SEEK(laCompany[puCompany,2])
*E300692,5 Use FSPRD instead of SYCFSPRD 
*IF gfRLOCK("SYCCOMP,SYCFSPRD",.T.)
IF gfRLOCK("SYCCOMP,&lcFsPrd",.T.)
*E300692,5 end
  lcNewYear = STR(INT(VAL(cCurr_Yer)+1),4)
  REPLACE cCurr_Yer WITH lcNewYear,;
          cCurr_Prd WITH '01',;
          cAdd_User WITH gcUser_ID,;
          dAdd_Date WITH DATE(),;
          cAdd_Time WITH gfGetTime()

  ** Update fiscal period file
  *E300692,5 Use FSPRD instead of SYCFSPRD 
  *SELECT SYCFSPRD
  SELECT (lcFsPrd)
  *E300692,5 end
  *E300789,1 Hesham (Start)
  *=SEEK(laCompany[puCompany,2]+laCompany[puCompany,4]+laCompany[puCompany,3])
  =SEEK(laCompany[puCompany,4]+laCompany[puCompany,3])
  *E300789,1 Hesham (End)

  IF llCloseGL
    ** Close curren period and lock it if required in glsetup.
    REPLACE lFspLocks WITH lFspLocks .OR. &lcSetup..lSetLokPd,;
            lFspclsds WITH .T.
  ELSE
    ** Close curren period and lock it if required in glsetup.
    REPLACE lFspLocks WITH lFspLocks,;
            lFspclsds WITH .T.
  ENDIF  
  *E300692,5 Use FSPRD instead of SYCFSPRD 
  *=gfAdd_Info('SYCFSPRD')
  =gfAdd_Info(lcFsPrd)
  *E300692,5 end

  ** Unlock next period to be the current
  SKIP 1
  REPLACE lFspLocks WITH .F.
  *E300692,5 Use FSPRD instead of SYCFSPRD 
  *=gfAdd_Info('SYCFSPRD')
  *=gfRLOCK("SYCCOMP,SYCFSPRD",.F.)
  =gfAdd_Info(lcFsPrd)
  =gfRLOCK("SYCCOMP,&lcFsPrd",.F.)
  *E300692,5 end
  
  WAIT WINDOW lcTCompAP NOWAIT
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
                CADD_USER WITH gcUser_ID,;
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
  laCompany[puCompany,9] = 'C'
ELSE
  laCompany[puCompany,9] = 'L'
  *E300692,5 Use FSPRD instead of SYCFSPRD 
  *=gfRLOCK("SYCCOMP,SYCFSPRD",.F.)
  =gfRLOCK("SYCCOMP,&lcFsPrd",.F.)
  *E300692,5 end
ENDIF

WAIT CLEAR

*!**************************************************************************
*!
*!      Function:  lfSetupChk
*!
*!**************************************************************************
*
FUNCTION lfSetupChk
PARAMETERS lcPath

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
*E300431,1 M.H Add a variable to hold the message to appear in the thermo.
PARAMETER lcMessage
*E300431,1 M.H End.
lnTermCount = lnTermCount + 1
*E300431,1 M.H Begin.
*WAIT WINDOW lcTAdjBal+'   '+SUBSTR(lcSpcChr,MOD(lnTermCount,4)+1,1) NOWAIT
WAIT WINDOW lcMessage+'   '+SUBSTR(lcSpcChr,MOD(lnTermCount,4)+1,1) NOWAIT
*E300431,1 M.H End.

*!**************************************************************************
*!
*!      Function: lfApUpdate
*!
*!**************************************************************************
*
FUNCTION lfApUpdate

SELECT SYCCOMP
=SEEK(laCompany[puCompany,2],'SYCCOMP')

************************** Update master files *****************************

WAIT WINDOW lcTCompFil NOWAIT

IF gfRLOCK("SYCCOMP",.T.)
  ** Update the current period of the selected company.
  SELECT SYCCOMP
  REPLACE CCURR_PRD WITH PADL(INT(VAL(CCURR_PRD))+1,2,'0')
  =gfAdd_Info('SYCCOMP')

  lcCurntPrd = PADL(INT(VAL(CCURR_PRD)),2,'0')
  ** Update fisical period file.
  *E300692,5 Use FSPRD instead of SYCFSPRD 
  *SELECT SYCFSPRD
  SELECT (lcFsPrd)
  *E300692,5 end
  SET ORDER TO TAG COMFYRPRDI
  *E300789,1 Hesham (Start)
  *=SEEK(laCompany[puCompany,2]+laCompany[puCompany,4]+laCompany[puCompany,3])
  =SEEK(laCompany[puCompany,4]+laCompany[puCompany,3])
  *E300789,1 Hesham (End)
  ** Close curren period and lock it if required in glsetup.
  REPLACE lFspLocks WITH lFspLocks,;
          lFspclsds WITH .T.
  *E300692,5 Use FSPRD instead of SYCFSPRD 
  *=gfAdd_Info('SYCFSPRD')
  =gfAdd_Info(lcFsPrd)
  *E300692,5 end
  ** Unlock next period to be the current.
  SKIP 1
  REPLACE lFspLocks WITH .F.
  
  *E300692,5 Use FSPRD instead of SYCFSPRD 
  *=gfAdd_Info('SYCFSPRD')
  =gfAdd_Info(lcFsPrd)
  *E300692,5 end
  
  ** Unlock the locked file
  =gfRLOCK("SYCCOMP",.F.)
  ** MESSAGE : "Closing period processing is finished."
  **           "Current period is ð.                  "
  ** Choices : "                  ® Ok ¯              "
  IF lcClose = 'P'
    =gfModalGen('TRM00104B00000','Dialog',lcCurntPrd)
    laCompany[puCompany,9] = 'C'
  ENDIF
ELSE
  laCompany[puCompany,9] = 'L'
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

WAIT WINDOW lcTCmpChld NOWAIT

SELECT SYCCOMP
SCAN FOR CCOMPPRNT = laCompany[puCompany,2] .AND. 'AP' $ MMODLSET
  *E301098,1 Hesham (Start)
  *lcApPath = ALLTRIM(CCOM_DDIR)
  lcApPath = gfgetdatadir(ALLTRIM(CCOM_DDIR))
  *E301098,1 Hesham (End)
  USE (lcApPath+'APVENDOR') IN 0
  USE (lcApPath+'APVENHST') IN 0 ORDER TAG VENDYEAR
  SELECT APVENDOR
  SCAN
    SELECT APVENHST
    IF !SEEK(APVENDOR.CVENDCODE+lcNewNxtYr)
      APPEND BLANK
      REPLACE CVENDCODE WITH APVENDOR.CVENDCODE,;
              CFISFYEAR WITH lcNewNxtYr,;
              CADD_USER WITH gcUser_ID,;
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
*E300692,5 Add file openings of ACCOD, FISHD, FSPRD
** This function is to Open files & set order
FUNCTION lfOpenSFils
PRIVATE llOpenFile

** Flag indicate that the files of the selected company is opened.
llOpenFile = .T.

FOR lnCount = 1 TO ALEN(laSFileOpn,1)
  IF FILE(lcDataDir+laSFileOpn[lnCount,1]+'.DBF')
    IF USED(laSFileOpn[lnCount,2])
      SELECT(laSFileOpn[lnCount,2])
      USE
    ELSE  
      SELECT 0
    ENDIF  
    USE &lcDataDir.&laSFileOpn[lnCount,1] AGAIN ALIAS(laSFileOpn[lnCount,2])
    SET ORDER TO TAG &laSFileOpn[lnCount,3]
  ELSE
    ** MESSAGE : "File ð not found in the directory of company ð."
    ** Choices : "                    ® Ok ¯                     "
    IF puCompany <> 0
      =gfModalGen("TRM00110B00000","ALERT",'File '+laSFileOpn[lnCount,1]+'|'+ALLTRIM(laCompany[puCompany,1])+'|'+lctCloseL)
      laCompany[puCompany,9] = 'F'
    ENDIF  
    llOpenFile = .F.
    EXIT
  ENDIF
ENDFOR

*E300692,1 Close fiscal calendar files
IF !llOpenFile
  FOR lnFCount = 1 TO ALEN(laSFileOpn,1)
    IF USED(laSFileOpn[lnFCount,2])
      USE IN &laSFileOpn[lnFCount,2]
    ENDIF
  ENDFOR
ENDIF
*E300692,1 end
RETURN llOpenFile





FUNCTION lfChkCmpAc
PARAMETERS lcComp_Id, lcAccCdStr
PRIVATE lnCurAlias, llRetVal, lcTempAcCd, lnCurRec, lcDataDir

lnCurAlias = SELECT()
lcTempAcCd = gfTempName()
llRetVal   = .F.

SELECT SYCCOMP
lnCurRec = RECNO()
SCAN FOR EMPTY(cCompPrnt) .AND. cComp_Id <> lcComp_ID .AND. !llRetVal
  *E301098,1 Hesham (Start)
  *lcDataDir = ALLTRIM(SYCCOMP.CCOM_DDIR)
  lcDataDir = gfGetDataDir(ALLTRIM(SYCCOMP.CCOM_DDIR))
  *E301098,1 Hesham (End)
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
  

