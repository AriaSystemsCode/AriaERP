*!************************************************************************
*!
*!      ALREPORT.PRG
*!
*!
*!************************************************************************
*E300406,1 Hesham El-Sheltawi 07/09/96
*E300406,1 change the functions that create the temprory frx to use and
*E300406,1 external frx beside any frx inside the project itself
*E300683,4 RENEE 06/15/97 Make the report programs run from inside the 
*E300683,4                module id dir. under, reports direct 
*E300693,1 Hesham El-Sheltawi 06/22/97
*E300693,1 Make all the module reports can be run directly from inside any
*E300693,1 program in the system through the toolbar
*E300981,1 RENEE 08/24/98 Set century off before running reports
*E301015,1 Hossam El Etreby Added Some Lines to run the At/Say Format beside
*          .FRX Format 09/16/1998
*B602079,1 MAN   09/13/98 Fix Running shared reports from their module directory
*E300871,4 RENEE 09/26/98 Call gfChkForm to check if the called report is a report 
*E300871,4	              or a form. If it is a form, restore its settings
*E301077,108 HDM 03/14/1999 Close SydReprt After program exit
*C101524,1 ADEL 05/13/1999 Add a parameter to the (gfChkForm) function to get 
*C101524,1                 the optional program if exists.
*C101524,1 				   Add lfUsrVldFn() function to SOREPORT
*B602924,1 BWA 05/24/99 Fix the bug of leaving temporary files when close the reports
*E301251,1 HDM 06/14/1999 Stop calling gfchkform in reports programs as it's called in OG Now
*MAB AT 08/16/2000 Add Function to open and close special files for a custom forms
*E038033,1 HMA 04/03/2004 Select only records belongs to A27(to split 
*E038033,1                A27 records of SYDREPRT File from A4XP records).
*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Task:T20081225.0020]
*!************************************************************************
*E300693,1 Hesham El-Sheltawi (Start)
*PARAMETERS lcRpPrg,llDoReport
PARAMETERS lcRpPrg,llDoReport,lcRepDir
*E300693,1 Hesham El-Sheltawi (End)

*E300981,1 Define private variable to hold the century setting
PRIVATE lcSetCent
*E300981,1 end

*MAB AT 08/16/2000 Add Function to open and close special files for a custom forms [Begin]
*PRIVATE llRepFile
PRIVATE llRepFile, laSpcFiles
DECLARE laSpcFiles[1]
laSpcFiles = ""
*MAB AT 08/16/2000 Add Function to open and close special files for a custom forms [End  ]
llRepFile = .F.

llDoReport=IIF(PARAM()=1,.F.,llDoReport)

*E300871,4 Call gfChkForm to check if the called report is a report 
*E300871,4 or a form.
*C101524,1 Add private variable lcOptProg
*PRIVATE lcFormName, lcFrmSpSets, laSettings, llFRXForm
*STORE '' TO lcFormName, lcFrmSpSets
*-- E301251,1 HDM[Start] Stop calling gfchkform in reports programs as it's called in OG Now
*PRIVATE lcFormName, lcFrmSpSets, laSettings, llFRXForm, lcOptProg
*STORE '' TO lcFormName, lcFrmSpSets, lcOptProg
*C101524,1 end
*llFRXForm = .F.
*DECLARE laSettings[1,2]

*E300871,4 If it is a form, restore its settings
*C101524,1 Add a parameter to get the optional progarm.
*IF gfChkForm(@lcRpPrg, @lcFormName, @laSettings, @lcFrmSpSets, @llFRXForm)
*IF gfChkForm(@lcRpPrg, @lcFormName, @laSettings, @lcFrmSpSets, @llFRXForm, @lcOptProg)
*C101524,1 end
  *-- Restore form settings
*  IF !EMPTY(laSettings[1,1])
*    FOR lnCount = 1 TO ALEN(laSettings,1)
*      &laSettings[lnCount, 1] = laSettings[lnCount, 2]
*    ENDFOR
*  ENDIF
*  IF !EMPTY(lcFrmSpSets)
*    DECLARE laSettings[1,2]
*    laSettings = ''
*	=gfSubstr(ALLTRIM(lcFrmSpSets), @laSettings,"|~")
*    FOR lnCount = 1 TO ALEN(laSettings,1)
*      &laSettings[lnCount, 1] = laSettings[lnCount, 2]
*    ENDFOR
*  ENDIF 
*ENDIF
*E300871,4 end

IF llDoReport
  lcRepAlias = SELECT()
  IF !USED('SYDREPRT')
    SELECT 0
    USE &gcSysHome.SYDREPRT ORDER 1
    *HDM E301077,108 [Begin]
    llRepFile = .T.
    *HDM E301077,108 [End]

  ELSE
    SELECT SYDREPRT 
    SET ORDER TO 1
  ENDIF
  lcSetRpPrg = lcRpPrg
   *E038033,1 HMA 04/03/2004 Select only Records belong to ARIA27 [BEGIN]
  
  *IF SEEK(PADR(lcRpPrg,8))
  *  lcSetRpPrg=ALLTRIM(IIF(!EMPTY(CMAINREPID),PADR(ALLTRIM(CMAINREPID),8),PADR(ALLTRIM(lcRpPrg),8)))
  = SEEK(PADR(lcRpPrg,8))
  LOCATE  WHILE cRep_id= PADR(lcRpPrg,8) FOR cVer<>"A40"
  lcSetRpPrg=ALLTRIM(IIF(!EMPTY(CMAINREPID),PADR(ALLTRIM(CMAINREPID),8),PADR(ALLTRIM(lcRpPrg),8)))
  
  *IF LAPPARELRP
  IF !llFRXForm .AND. LAPPARELRP
    =gfSetDevice()
  ENDIF
  *ENDIF

  *E038033,1 HMA [END] 
  SELECT (lcRepAlias)
  
  *E300683,4 check if the report is shared between modules or not
  *E300683,4 if shared then run it from the reports dir. else from
  *E300683,4 module dir under the reports dir
  lcFullSet = SET('FULLPATH')
  SET FULLPATH ON
  *B602079,1 Check for the existance of the report in its module directory
  *B602079,1 if it does not exist in the active module's directory
  *lcSetRpPrg = IIF(FILE(gcRepHome+lcSetRpPrg+'.FXP'),gcRepHome+lcSetRpPrg,gcRepHome+gcAct_Appl+'\'+lcSetRpPrg)
  
  *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Start]
  *lcSetRpPrg = IIF(FILE(gcRepHome+lcSetRpPrg+'.FXP'),;
                   gcRepHome+lcSetRpPrg,;
  				   IIF(FILE(gcRepHome+gcAct_Appl+'\'+lcSetRpPrg+'.FXP'),;
  				            gcRepHome+gcAct_Appl+'\'+lcSetRpPrg,;
  				            gcRepHome+LEFT(lcSetRpPrg,2)+'\'+lcSetRpPrg))
  IF (GCMULTIINST AND FILE(gcCRepHome+lcSetRpPrg+'.FXP')) OR ;
     (GCMULTIINST AND FILE(gcCRepHome+gcAct_Appl+'\'+lcSetRpPrg+'.FXP')) 

    lcSetRpPrg = IIF(FILE(gcCRepHome+lcSetRpPrg+'.FXP'),;
                   gcCRepHome+lcSetRpPrg,;
  				   IIF(FILE(gcCRepHome+gcAct_Appl+'\'+lcSetRpPrg+'.FXP'),;
  				            gcCRepHome+gcAct_Appl+'\'+lcSetRpPrg,;
  				            gcCRepHome+LEFT(lcSetRpPrg,2)+'\'+lcSetRpPrg))

  ELSE
  lcSetRpPrg = IIF(FILE(gcRepHome+lcSetRpPrg+'.FXP'),;
                   gcRepHome+lcSetRpPrg,;
  				   IIF(FILE(gcRepHome+gcAct_Appl+'\'+lcSetRpPrg+'.FXP'),;
  				            gcRepHome+gcAct_Appl+'\'+lcSetRpPrg,;
  				            gcRepHome+LEFT(lcSetRpPrg,2)+'\'+lcSetRpPrg))
  
  ENDIF 
  *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [End]
  
  *B602079,1 end
  SET FULL &lcFullSet
  *E300683,4 (End)

  *HDM -- Assign this variable to call Rpt_Hdr as 2.6 reports [begin]
  Qcompany = gcCom_Name
  *HDM -- Assign this variable to call Rpt_Hdr as 2.6 reports [end]

  *E300981,1 Set century off after saving the current century setting
  lcSetCent = SET('CENTURY')
  SET CENTURY OFF
  *E300981,1 end
  DO &lcSetRpPrg
  *E300981,1 Restore century setting
  SET CENTURY &lcSetCent
  *E300981,1 end
ELSE
  
  *E300683,4 Make all the variables that is used by the modules main progs. private
  PRIVATE  lcBrFields,lcBaseFile,laField_H,laField_N,lcFile_Ttl,lcSetProcd
  *E300683,4 (End)
  
  *B602924,1 BWA 05/24/99 Declare an array to hold the temporary files to delete them whem 
  *B602924,1 BWA 05/24/99 close the reports
  DECLARE laPrgTemps[1]
  *B602924,1 BWA [END]
  
  DECLARE laField_H[1,1],laField_N[1,1]


  STORE " " TO lcRpForm ,lcRpFiles,lcRpExpr,lcRpTargt,lcRpOrder,lcRpFields,lcRpExp
  STORE " " TO lcBrFields,lcBaseFile,laField_H,laField_N,lcSetProcd
  ********** Declaring the arrays needed for the incomestatment report
  ********** temprarory defined for now only
  DIMENSION laRpMaType(1,5),laRpSType(1,6),laRpCType(1,6),laRpEType(1,6);
            ,laRpIType(1,6),laRpTType(1,6)
  DIMENSION laRpCol(1,22)     && Column information
  DIMENSION laTempCol(1,22)   && Temp Column
  
  *-- HDM
  R_WIDTH = 'N'
  *-- HDM

  *llOGFltCh = .T.
  IF !USED('SYDREPRT')
    SELECT 0
    USE &gcSysHome.SYDREPRT ORDER 1
    *HDM E301077,108 [Begin]
    llRepFile = .T.
    *HDM E301077,108 [End]
  ELSE
    SELECT SYDREPRT 
    SET ORDER TO 1
  ENDIF  
  lcSetRpPrg = lcRpPrg
  *E038033,1 HMA 04/03/2004 Select only Records belong to ARIA27 [BEGIN]
  
  *IF SEEK(PADR(lcRpPrg,8))
  *  lcSetRpPrg=ALLTRIM(IIF(!EMPTY(CMAINREPID),PADR(ALLTRIM(CMAINREPID),8),PADR(ALLTRIM(lcRpPrg),8)))
  *ENDIF
  = SEEK(PADR(lcRpPrg,8))
  LOCATE  WHILE cRep_id= PADR(lcRpPrg,8) FOR cVer<>"A40"
  lcSetRpPrg=ALLTRIM(IIF(!EMPTY(CMAINREPID),PADR(ALLTRIM(CMAINREPID),8),PADR(ALLTRIM(lcRpPrg),8)))
  
  *E038033,1 HMA [END] 

  *E300683,4 check if the report is shared between modules or not
  *E300683,4 if shared then run it from the reports dir. else from
  *E300683,4 module dir under the reports dir  
  lcFullSet = SET('FULLPATH')
  SET FULLPATH ON
  *B602079,1 Check for the existance of the report in its module directory
  *B602079,1 if it does not exist in the active module's directory
  *lcSetRpPrg = IIF(FILE(gcRepHome+lcSetRpPrg+'.FXP'),gcRepHome+lcSetRpPrg,gcRepHome+gcAct_Appl+'\'+lcSetRpPrg)
  
  *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Start]
  *lcSetRpPrg = IIF(FILE(gcRepHome+lcSetRpPrg+'.FXP'),;
                   gcRepHome+lcSetRpPrg,;
  				   IIF(FILE(gcRepHome+gcAct_Appl+'\'+lcSetRpPrg+'.FXP'),;
  				            gcRepHome+gcAct_Appl+'\'+lcSetRpPrg,;
  				            gcRepHome+LEFT(lcSetRpPrg,2)+'\'+lcSetRpPrg))
  IF (GCMULTIINST AND FILE(gcCRepHome+lcSetRpPrg+'.FXP')) OR ;
     (GCMULTIINST AND FILE(gcCRepHome+gcAct_Appl+'\'+lcSetRpPrg+'.FXP')) 

    lcSetRpPrg = IIF(FILE(gcCRepHome+lcSetRpPrg+'.FXP'),;
                   gcCRepHome+lcSetRpPrg,;
  				   IIF(FILE(gcCRepHome+gcAct_Appl+'\'+lcSetRpPrg+'.FXP'),;
  				            gcCRepHome+gcAct_Appl+'\'+lcSetRpPrg,;
  				            gcCRepHome+LEFT(lcSetRpPrg,2)+'\'+lcSetRpPrg))

  ELSE
  lcSetRpPrg = IIF(FILE(gcRepHome+lcSetRpPrg+'.FXP'),;
                   gcRepHome+lcSetRpPrg,;
  				   IIF(FILE(gcRepHome+gcAct_Appl+'\'+lcSetRpPrg+'.FXP'),;
  				            gcRepHome+gcAct_Appl+'\'+lcSetRpPrg,;
  				            gcRepHome+LEFT(lcSetRpPrg,2)+'\'+lcSetRpPrg))
  
  ENDIF 
  *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [End]
  
  *B602079,1 end
  SET FULL &lcFullSet
  *E300683,4 (End)

  lcSetProcd=SET('PROCEDURE')
  lcOldErr=ON('ERROR')
  ON ERROR llDumErr = .T.
  *B602079,1 Set procedure to the prg instead of the obsolete '.RPR'
  *SET PROCEDURE TO (ALLTRIM(lcSetRpPrg)+'.RPR')
  SET PROCEDURE TO (ALLTRIM(lcSetRpPrg))
  *B602079,1 end
  ON ERROR &lcOldErr
  *E300693,1 Hesham El-Sheltawi (Start)
*  =gfOpGrid(lcRpPrg,.f.)  
  =gfOpGrid(lcRpPrg,.f.,lcRepDir)

  *HDM E301077,108 [Begin]
  IF llRepFile
    USE IN SYDREPRT
  ENDIF
  *HDM E301077,108 [End]

  *E300693,1 Hesham El-Sheltawi (End)

  
  SET PROCEDURE TO (lcSetProcd)
  *B602924,1 BWA Delete the temporary files when closing the reports 
  *B602924,1 BWA Clean temp Files
  
  IF TYPE('laPrgTemps')#'U'
    FOR lnCount = 1 TO ALEN(laPrgTemps,1)
      IF !EMPTY(laPrgTemps[lnCount,1])
        IF USED(laPrgTemps[lnCount,1])
          USE IN (laPrgTemps[lnCount,1])
        ENDIF
        ERASE (gcWorkDir+laPrgTemps[lnCount,1]+'.DBF')
        ERASE (gcWorkDir+laPrgTemps[lnCount,1]+'.FPT')
        ERASE (gcWorkDir+laPrgTemps[lnCount,1]+'.CDX')
        ERASE (gcWorkDir+laPrgTemps[lnCount,1]+'.TXT')
        ERASE (gcWorkDir+laPrgTemps[lnCount,1]+'.TMP')
      ENDIF
    ENDFOR
  ENDIF
  *B602924,1 BWA [END]

ENDIF

*!*********************************************************************************
*!
*!             FUNCTION : lfRpThermo
*!
*!*********************************************************************************
*
FUNCTION lfRpThermo
PARAMETERS lnRpTotal,lnRpCount,lcMessage
PRIVATE lcOldEsc
lcMessage=IIF(TYPE('lcMessage')='C',lcMessage,'Collecting Data')
&lnRpCount = &lnRpCount + 1
lcOldEsc=SET('ESCAPE')
SET ESCAPE OFF
IF &lnRpCount<=lnRpTotal
  =gfThermo(lnRpTotal,&lnRpCount,lcMessage,' ')
ENDIF  
SET ESCAPE &lcOldEsc


*!*********************************************************************************
*!
*!             FUNCTION : lfClosThrm
*!
*!*********************************************************************************
*
FUNCTION lfClosThrm
PARAMETERS lnRpTotal,lnRpCount,lcMessage,lnStep
lnStep=IIF(TYPE('lnStep')='N',lnStep,1)
lnStep=IIF(lnStep<1,1,lnStep)
lcMessage=IIF(TYPE('lcMessage')='C',lcMessage,'Collecting Data')
FOR lnCounter  = lnRpCount TO lnRpTotal STEP lnStep
  =gfThermo(lnRpTotal,lnCounter,lcMessage,' ')     
ENDFOR
IF WEXIST("gwdThermo")
    RELEASE WINDOW gwdThermo
ENDIF



*!************************************************************************
*!
*!      FUNCTION lfWaitMsg
*!
*!************************************************************************
FUNCTION lfWaitMsg

WAIT "Collecting data... " WINDOW NOWAIT

*!**************************************************************************
*!
*!      Function: lfClearRep
*!
*!**************************************************************************
* 
FUNCTION lfClearRep
*MAB AT 08/16/2000 Add Function to open and close special files for a custom forms [Begin]
=lfSpcClear()
*MAB AT 08/16/2000 Add Function to open and close special files for a custom forms [End  ]
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfGetForm						*E300871,4
*! Developer : Renee
*! Date      : 09/26/1998
*! Purpose   : Function to get if the current form is a @ ,  SAY report
*!*************************************************************
*! Called from : None
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example	      	 : lcFormName = lfGetForm()
*!*************************************************************
*
FUNCTION RemlfGetForm
RETURN lcFormName

*!*************************************************************
*! Name      : lfIsApparl						*E300871,4
*! Developer : Renee
*! Date      : 09/26/1998
*! Purpose   : Function to get if the current form is a @ ,  SAY report
*!*************************************************************
*! Called from : None
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example	      	 : lcFornName = gfGetForm()
*!*************************************************************
*
FUNCTION RemlfIsApparl
PARAMETERS lcForm
IF !llFRXForm
  lcForm = IIF(FILE(gcRepHome + lcForm + '.FXP'),;
                    gcRepHome + lcForm , gcRepHome + gcAct_Appl+'\'+  lcForm ) 
ENDIF
RETURN !llFRXForm

*!*************************************************************
*! Name      : lfOptProg						*C101389,1 ASH
*! Developer : Renee
*! Date      : 09/26/1998
*! Purpose   : Function to get if the current form is a @ ,  SAY report
*!*************************************************************
*! Called from : None
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Example	      	 : =lfOptProg
*!*************************************************************
*
FUNCTION remlfoptprog
PRIVATE lcProgToDo
*C101389,1 Add variable lcOptProg
IF !EMPTY(lcOptProg)
  lcProgToDo = ALLTRIM(lcOptProg)
  lcProgToDo = IIF(FILE(gcRepHome+lcProgToDo+'.FXP'),;
                   gcRepHome+lcProgToDo,;
  				   IIF(FILE(gcRepHome+gcAct_Appl+'\'+lcProgToDo+'.FXP'),;
  				            gcRepHome+gcAct_Appl+'\'+lcProgToDo,;
  				            gcRepHome+LEFT(lcProgToDo,2)+'\'+lcProgToDo))


  DO (lcProgToDo)
ENDIF  

*!*************************************************************
*! Name      : lfUsrVldFn
*! Developer : Bassem Rafaat (BWA)
*! Date      : 05/17/1999
*! Purpose   : Function to run any function withen its program
*!*************************************************************
*! Called from : Global in MA reports 
*!*************************************************************
*! Calls       : passed function as a parameter
*!*************************************************************
*! Passed Parameters : lcFncNam,lcFncLoc,lcParam
*!                     lcFncNam  : Variable hold the name function 
*!                     lcFncLoc  : Variable hold the function path
*!                     lcParam   : Variable hold the parameters 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfUsrVldFn()			
*!*************************************************************
*C101484,1 Add a function to enable calling user defined functions
*C101484,1 placed on disk.
*C101524,1 Add lfUsrVldFn() function to SOREPORT
FUNCTION remlfusrvldfn
PARAMETERS lcFncNam,lcFncLoc,lcParam
*-- lcRetrn variable to hold the return value
PRIVATE lcRetrn
IF !EMPTY(lcFncNam)
  lcRetrn  = SPACE(0)
  IF EMPTY(lcFncLoc) .AND. !EMPTY(lcOptProg)
    lcFncLoc = lcOptProg
  ENDIF
  IF !EMPTY(lcFncLoc)  
    lcFncLoc= IIF(FILE(gcRepHome+lcFncLoc+'.FXP'),;
                       gcRepHome+lcFncLoc,;
  	  			       IIF(FILE(gcRepHome+gcAct_Appl+'\'+lcFncLoc+'.FXP'),;
  				            gcRepHome+gcAct_Appl+'\'+lcFncLoc,;
  				            gcRepHome+LEFT(lcFncLoc,2)+'\'+lcFncLoc))

    IF EMPTY(lcParam)
      DO (lcFncNam) IN (lcFncLoc) WITH lcRetrn
    ELSE
      DO (lcFncNam) IN (lcFncLoc) WITH lcRetrn,&lcParam
    ENDIF
    RETURN lcRetrn
  ENDIF  
ENDIF

*!*************************************************************
*! Name      : lfSpcOpen
*! Developer : Mohamed Atia Badran
*! Date      : 08/16/2000
*! Purpose   : Open a special case tables
*!*************************************************************
*! Called from : Any Picket Ticket form
*!*************************************************************
*
FUNCTION lfSpcOpen
PARAMETERS lcFileName , lcIndTag , lcFullPath , lcSharing 
PRIVATE lcFullPath , lcFileName , lcIndTag , lcSharing , llCanOpen

IF (TYPE("lcFileName")$"UL") OR EMPTY(lcFileName)
  llCanOpen = .F.
ELSE

  PRIVATE lnFilePos
  llCanOpen = .T.
  lcFileName = UPPER(ALLTRIM(lcFileName))
  IF TYPE("lcIndTag")$"UL"
    lcIndTag = ""
  ENDIF

  IF TYPE("lcFullPath")$"UL" OR EMPTY(lcFullPath)
    lcFullPath = IIF(LEFT(lcFileName,2)="SY",gcSysHome,gcDataDir)
  ENDIF

  lcFullPath = ALLTRIM(lcFullPath)
  IF RIGHT(lcFullPath,1) <> "\"
    lcFullPath = lcFullPath + "\"
  ENDIF

  IF TYPE("lcSharing")$"UL"
    lcSharing = "SH"
  ENDIF

  = gfOpenFile(lcFullPath+lcFileName, lcIndTag , lcSharing)

  IF ASCAN(laSelFile,lcFileName) = 0

    IF (TYPE("laSpcFiles[1]") $ "UL") OR EMPTY(laSpcFiles[1])
      DECLARE laSpcFiles[1]
      lnFilePos = 1
    ELSE
      lnFilePos = ASCAN(laSpcFiles,lcFileName)
      IF lnFilePos = 0
        DECLARE laSpcFiles[ALEN(laSpcFiles) + 1]
        lnFilePos = ALEN(laSpcFiles)
      ENDIF
    ENDIF

    IF lnFilePos = ALEN(laSpcFiles,1)
      laSpcFiles[lnFilePos] = lcFileName
    ENDIF  

  ENDIF

ENDIF
RETURN llCanOpen
*-- end of lfSpcOpen.

*!*************************************************************
*! Name      : lfSpcClear
*! Developer : Mohamed Atia Badran
*! Date      : 08/16/2000
*! Purpose   : Close a special case tables
*!*************************************************************
*! Called from : Any Picket Ticket form
*!*************************************************************
*
FUNCTION lfSpcClear
IF TYPE("laSpcFiles[1]") $ "UL"
  RETURN
ENDIF

IF !EMPTY(laSpcFiles[1])
  PRIVATE lnI
  lnI = 0
  FOR lnI = 1 TO ALEN(laSpcFiles,1)
    IF USED(laSpcFiles[lnI])
      USE IN (laSpcFiles[lnI])
    ENDIF
  ENDFOR
ENDIF
*-- end of lfSpcClear.