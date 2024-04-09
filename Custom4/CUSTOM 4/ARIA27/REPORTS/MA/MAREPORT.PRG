*!************************************************************************
*!
*!      MAREPORT.PRG
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
*B602079,1 MAN   09/13/98 Fix Running shared reports from their module directory
*E300871,6 SAMEH 02/15/99 enhancement in all reports to check if the
*E300871,6 				  report is a report or a form. If it is a form, restore its 
*E300871,6 			      settings , depends on calling gfChkForm func. made by renee 
*B801962,1 RENEE 02/22/99 Add function lfGetVend to validate vendors from reports
*E301077,108 HDM 03/14/1999 Close SydReprt After program exit
*C101484,1 BASSEM 05/18/99 Call optional program for forms
*B602924,1 BWA 05/24/99 Fix the bug of leaving temporary files when close the reports
*E301251,1 HDM 06/14/1999 Stop calling gfchkform in reports programs as it's called in OG Now
*E038033,1 HMA 04/03/2004 Select only records belongs to A27(to split 
*E038033,1                A27 records of SYDREPRT File from A4XP records).
*B124255,1 NNA 11/03/2004 Fix bug that in the material PO screen if you used the print icon
*B124255,1 NNA            to print the PO it'll print properly but if you tried to print 
*B124255,1 NNA            it again it'll print all POs
*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Task:T20081225.0020]
*!************************************************************************
*E300693,1 Hesham El-Sheltawi (Start)
*PARAMETERS lcRpPrg,llDoReport
PARAMETERS lcRpPrg,llDoReport,lcRepDir
*E300693,1 Hesham El-Sheltawi (End)

*E300981,1 Define private variable to hold the century setting
PRIVATE lcSetCent
*E300981,1 end

*E301077,108 private flag[Begin]
PRIVATE llRepFile
llRepFile = .F.
*E301077,108 [End]

llDoReport=IIF(PARAM()=1,.F.,llDoReport)

*E300296,1 12/18/95 M.H Add the multi currency to the AP reports.
*E300296,1 Variable to hold if the company is using the multi currency or not.
llMultCurr = gfGetMemVar('LLMULCURR')
*E300296,1 End.
*-- E301251,1 HDM[Start] Stop calling gfchkform in reports programs as it's called in OG Now
*E300871,6 Call gfChkForm to check if the called report is a report 
*E300871,6 or a form.
*PRIVATE lcFormName, lcFrmSpSets, laSettings, llFRXForm
*C101484,1 Add lcOptProg variable
*STORE '' TO lcFormName, lcFrmSpSets
*STORE '' TO lcFormName, lcFrmSpSets, lcOptProg
*C101484,1 end
*llFRXForm = .F.
*DECLARE laSettings[1,2]

*E300871,6 If it is a form, restore its settings
*C101484,1 Return the optional parameter program name
*IF gfChkForm(@lcRpPrg, @lcFormName, @laSettings, @lcFrmSpSets, @llFRXForm)
*IF gfChkForm(@lcRpPrg, @lcFormName, @laSettings, @lcFrmSpSets, @llFRXForm, @lcOptProg)
*C101484,1 end
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
*E300871,6 end
*-- E301251,1 HDM[End]
IF llDoReport
  lcRepAlias = SELECT()
  IF !USED('SYDREPRT')
    SELECT 0
    USE &gcSysHome.SYDREPRT ORDER 1

    *E301077,108 [Begin]
    llRepFile = .T.
    *E301077,108 [End]

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
  *E300871,6 SAMEH 
  IF !llFRXForm .AND. LAPPARELRP
  *E300871,6 SAMEH end
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
  Qcompany = gcCom_Name
  
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
  R_WIDTH = 'N'
  *llOGFltCh = .T.
  IF !USED('SYDREPRT')
    SELECT 0
    USE &gcSysHome.SYDREPRT ORDER 1

    *E301077,108 [Begin]
    llRepFile = .T.
    *E301077,108 [End]

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

  *E301077,108 [Begin]
  IF llRepFile
    USE IN SYDREPRT
  ENDIF
  *E301077,108 [End]

  *E300693,1 Hesham El-Sheltawi (End)

  
  SET PROCEDURE TO (lcSetProcd)
  
  *B602924,1 BWA Delete the temporary files when closing the reports 
  *B602924,1 BWA Clean temp Files
  
  *B124255,1 NNA 11/03/2004 (Begin) If we are in the Material PO Screen 'Glquitting' Be .F. until
  *B124255,1 NNA            exit from the Screen so the Temp File 'lcNewMpos' must be open until
  *B124255,1 NNA            quitting the screen
  *IF TYPE('laPrgTemps')#'U' 
  IF TYPE('laPrgTemps')#'U' AND GLQUITTING
  *B124255,1 NNA (End)
  
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

*!*************************************************************
*! Name      : lfGetForm						*E300871,6
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
*! Name      : lfIsApparl						*E300871,6
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
*! Name      : lfOptProg					  *E300871,6
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
*-- Add variable lcOptProg
IF !EMPTY(lcOptProg)
  lcProgToDo = ALLTRIM(lcOptProg)
  lcProgToDo = IIF(FILE(gcRepHome+lcProgToDo+'.FXP'),;
                   gcRepHome+lcProgToDo,;
  				   IIF(FILE(gcRepHome+gcAct_Appl+'\'+lcProgToDo+'.FXP'),;
  				            gcRepHome+gcAct_Appl+'\'+lcProgToDo,;
  				            gcRepHome+LEFT(lcProgToDo,2)+'\'+lcProgToDo))


  DO (lcProgToDo)
ENDIF

*!**************************************************************************
*!
*!      Function: lfGetVend
*!
*!**************************************************************************
* 
*B801962,1 Add function lfGetVend to validate vendors from reports
FUNCTION lfGetVend

DECLARE laRpRetFld(1)
lcBrFields    = 'Cvendcode:H="Vendor code",Cvencomp:H="Company"'
laRpRetFld[1] = ''
lcRpCurFld    = SYS(18)
lcRpArea      = ALIAS()

SELECT APVENDOR
SET ORDER TO TAG VENCODE
IF !EMPTY(&lcRpCurFld) .AND. !SEEK(&lcRpCurFld)
  *B601173,1 Hesham El-Sheltawi (Start)
  *B601173,1 if there is  any near matching records
  *B601173,1 then go to the nearest record
  IF !EMPTY(&lcRpCurFld) AND RECNO(0)>0
    GO RECNO(0)
  ENDIF  
  *B601173,1 Hesham El-Sheltawi (End)  
  =gfBrows(.F.,'Cvendcode',"laRpRetFld",'Vendors',.F.)
  &lcRpCurFld = laRpRetFld[1]
  SHOW GET (lcRpCurFld)
ENDIF

SELECT (lcRpArea)


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
FUNCTION remlfusrvldfn
PARAMETERS lcFncNam,lcFncLoc,lcParam
*-- lcRetrn variable to hold the return value
PRIVATE lcRetrn
lcRetrn = SPACE(0)
DO (lcFncNam) IN (gcRepHome+gcAct_Appl+'\'+lcFncLoc) WITH lcRetrn,(lcParam)
RETURN lcRetrn