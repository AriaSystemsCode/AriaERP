*!************************************************************************
*!
*!      RMREPORT.PRG
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
*E300871,5 KHM   02/09/99 Call gfChkForm to check if the called report is a report 
*E300871,5	              or a form. If it is a form, restore its settings
*E301077,108 HDM 03/14/1999 Close SydReprt After program exit
*B602924,1 BWA 05/24/99 Fix the bug of leaving temporary files when close the reports
*E301251,1 HDM 06/14/1999 Stop calling gfchkform in reports programs as it's called in OG Now
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
*--HDM E301077,108 private flag[Begin]
PRIVATE llRepFile
llRepFile = .F.
*--HDM E301077,108 [End]

llDoReport=IIF(PARAM()=1,.F.,llDoReport)
*E301251,1[Start] Comment out these lines as these variables defined now by the option grid
*E300871,5 KHM 02/09/99 Call gfChkForm to check if the called report
*E300871,5              is a report or a form.
*PRIVATE lcFormName, lcFrmSpSets, laSettings, llFRXForm
*STORE '' TO lcFormName, lcFrmSpSets
*llFRXForm = .F.
*DECLARE laSettings[1,2]

*E300871,5 KHM 02/09/99 If it is a form, restore its settings
*IF gfChkForm(@lcRpPrg, @lcFormName, @laSettings, @lcFrmSpSets, @llFRXForm)
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
*E300871,5 KHM 02/09/99 (End)
*E301251,1[END]
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
  
  *E301081,1 MAB @X,Y SAY [BEGIN]
  *E300871,5 KHM 02/09/99 (Begin)
  *IF LAPPARELRP
  IF !llFRXForm .AND. LAPPARELRP
  *E300871,5 KHM 02/09/99 (End)
    =gfSetDevice()
  ENDIF
  *E301081,1 MAB @X,Y SAY [END]
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

  *E301081,1 MAB 7/14/98 Assign this variable to call Rpt_Hdr as 2.6 reports [begin]
  Qcompany = gcCom_Name
  *E301081,1 MAB 7/14/98 Assign this variable to call Rpt_Hdr as 2.6 reports [end]

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
  *E301081,1 MAB @X,Y SAY [BEGIN]
  R_WIDTH = 'N'
  *E301081,1 MAB @X,Y SAY [END]

 
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


*!*************************************************************
*! Name      : lfGetForm						*E300871,5
*! Developer : Khalid Mohi El-Din M.
*! Date      : 02/09/1999
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
FUNCTION RemlfGetForm
RETURN lcFormName

*!*************************************************************
*! Name      : lfIsApparl						*E300871,5
*! Developer : Khalid Mohi El-Din M.
*! Date      : 02/09/1999
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
FUNCTION RemlfIsApparl
PARAMETERS lcForm
IF !llFRXForm
  lcForm = IIF(FILE(gcRepHome + lcForm + '.FXP'),;
                    gcRepHome + lcForm , gcRepHome + gcAct_Appl+'\'+  lcForm ) 
ENDIF
RETURN !llFRXForm
