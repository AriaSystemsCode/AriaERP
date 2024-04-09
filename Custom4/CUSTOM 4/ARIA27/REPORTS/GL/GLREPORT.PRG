*!************************************************************************
*!
*!      GLREPORT.PRG
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
*B602079,1 MAN   09/13/98 Fix Running shared reports from their module directory
*E301077,72 MAB 03/06/1999 Open and close files (Report master file)
*E301170,1 MAB 03/15/1999 llOGFltCh Defined in OG program.
*B602924,1 BWA 05/24/99 Fix the bug of leaving temporary files when close the reports
*E038033,1 HMA 04/03/2004 Select only records belongs to A27(to split 
*E038033,1                A27 records of SYDREPRT File from A4XP records).
*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Task:T20081225.0020]
*!**************************************************************************
*E300693,1 Hesham El-Sheltawi (Start)
*PARAMETERS lcRpPrg,llDoReport
PARAMETERS lcRpPrg,llDoReport,lcRepDir
*E300693,1 Hesham El-Sheltawi (End)

*E301077,72 [Begin
PRIVATE llRepFile
llRepFile = .F.
*E301077,72 [End

llDoReport=IIF(PARAM()=1,.F.,llDoReport)

IF llDoReport
  lcRepAlias = SELECT()
  IF !USED('SYDREPRT')
    SELECT 0
    USE &gcSysHome.SYDREPRT 

    *E301077,72 [Begin
    llRepFile = .T.
    *E301077,72 [End

  ELSE
    SELECT SYDREPRT 
  ENDIF 
  SET ORDER TO TAG CREP_ID
  lcSetRpPrg = lcRpPrg
  *E038033,1 HMA 04/03/2004 Select only Records belong to ARIA27 [BEGIN]
  
  *IF SEEK(PADR(lcRpPrg,8))
  *  lcSetRpPrg=ALLTRIM(IIF(!EMPTY(CMAINREPID),PADR(ALLTRIM(CMAINREPID),8),PADR(ALLTRIM(lcRpPrg),8)))
  *ENDIF
  = SEEK(PADR(lcRpPrg,8))
  LOCATE WHILE cRep_id= PADR(lcRpPrg,8) FOR cVer<>"A40"
  lcSetRpPrg=ALLTRIM(IIF(!EMPTY(CMAINREPID),PADR(ALLTRIM(CMAINREPID),8),PADR(ALLTRIM(lcRpPrg),8)))
  
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

  DO &lcSetRpPrg
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
  DIMENSION laRpMaType(1,6),laRpSType(1,6),laRpCType(1,6),laRpEType(1,6);
            ,laRpIType(1,6),laRpTType(1,6)
  DIMENSION laRpAType(1,6),laRpLType(1,6),laRpQType(1,6)
  DIMENSION laRpCol(1,24)     && Column information
  DIMENSION laTempCol(1,23)   && Temp Column
  DIMENSION laDbfName(1),laRpMainTo[1,1],laRpSeg(1,2)
  DIMENSION laRpColInf(47,2),laSegInf[1,2],laRpSegV[1]

  *E301170,1 MAB 03/15/1999 llOGFltCh Defined in OG program.
  *llOGFltCh = .T.
  
  IF !USED('SYDREPRT')
    SELECT 0
    USE &gcSysHome.SYDREPRT 

    *E301077,72 [Begin
    llRepFile = .T.
    *E301077,72 [End

  ELSE
    SELECT SYDREPRT 
  ENDIF  
  SET ORDER TO TAG CREP_ID
  lcSetRpPrg = lcRpPrg
  *E038033,1 HMA 04/03/2004 Select only Records belong to ARIA27 [BEGIN]
  
  *IF SEEK(PADR(lcRpPrg,8))
  *  lcSetRpPrg=ALLTRIM(IIF(!EMPTY(CMAINREPID),PADR(ALLTRIM(CMAINREPID),8),PADR(ALLTRIM(lcRpPrg),8)))
  *ENDIF
  = SEEK(PADR(lcRpPrg,8))
  LOCATE WHILE cRep_id= PADR(lcRpPrg,8) FOR cVer<>"A40"
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
  
  *E301077,72 [Begin
  IF llRepFile
    USE IN SYDREPRT
  ENDIF
  *E301077,72 [End

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
