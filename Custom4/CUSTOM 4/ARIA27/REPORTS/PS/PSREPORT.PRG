*!************************************************************************
*!
*!      PSREPORT.PRG
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
*E300921,1 TAK 07/12/98 Added to not open the APSETUP file in Ap was Not Installed.
*E300981,1 RENEE 08/24/98 Set century off before running reports
*B602079,1 MAN   09/13/98 Fix Running shared reports from their module directory
*B602556,1 RENEE 02/17/99 Fix 'File is in use' bug in lfSetGLMsk()
*B801962,1 RENEE 02/22/99 Add function lfGetVend to validate vendors from reports
*E301077,108 HDM 03/14/1999 Close SydReprt After program exit
*E301170,1 MAB 03/15/1999 llOGFltCh Defined in OG program.
*B602929,1 AMM 05/23/1999 Adjust to work files
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


*E300697,1 YMA 07/13/97 Define the variables that will hold the 
*E300697,1              GL account mask and length.
lcAcMask   = SPACE(0)         && Account mask
lnAcLen    = 0                && Account Length
lcAcntBrwF = SPACE(0)         && Chart of account browse fields.
lcAcntFld  = SPACE(0)         && Account field that will be validated.
lcAcntDesF = SPACE(0)         && Account Description.
llNoThing  = lfSetGLMsk()
*E300697,1 YMA 07/13/97 End.

**E300921,1 start.
llMultCurr = gfGetMemVar('LLMULCURR')
llApLink   = (OCCURS('AP',gcCmpModules)<>0)
IF llApLink
  =gfOpenFile(gcDataDir+'APSETUP','','SH')
ENDIF
*E300921,1 end.


**E300921,1 start. added lcFile_Ttl used in reconcleation report.
*E300683,4 Make all the variables that is used by the modules main progs. private
PRIVATE  lcBrFields,lcBaseFile,laField_H,laField_N,lcFile_Ttl,lcSetProcd
STORE " " TO lcBrFields,lcBaseFile,laField_H,laField_N,lcFile_Ttl,lcSetProcd
*E300683,4 (End)

llDoReport=IIF(PARAM()=1,.F.,llDoReport)

*B602929,1 AMM start
*-- E301251,1 HDM[Start] Stop calling gfchkform in reports programs as it's called in OG Now
*PRIVATE lcFormName, lcFrmSpSets, laSettings, llFRXForm, lcOptProg
*STORE '' TO lcFormName, lcFrmSpSets, lcOptProg
*DECLARE laSettings[1,2]

*llFRXForm = .F.

*C101389,1 ASH If it is a form, restore its settings
*IF gfChkForm(@lcRpPrg, @lcFormName, @laSettings, @lcFrmSpSets, @llFRXForm, @lcOptProg)
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
*C101389,1 ASH end
*B602929,1 AMM end

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

  *B602929,1 AMM start
  IF !llFRXForm .AND. LAPPARELRP
    =gfSetDevice()
  ENDIF
  *B602929,1 AMM end
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
  
  *B602929,1 AMM Assign this variable to call Rpt_Hdr as 2.6 reports [begin]
  Qcompany = gcCom_Name
  *B602929,1 AMM end

  *E300981,1 Set century off after saving the current century setting
  lcSetCent = SET('CENTURY')
  SET CENTURY OFF
  *E300981,1 end
  DO &lcSetRpPrg
  *E300981,1 Restore century setting
  SET CENTURY &lcSetCent
  *E300981,1 end

ELSE

  *B602924,1 BWA 05/24/99 Declare an array to hold the temporary files to delete them whem 
  *B602924,1 BWA 05/24/99 close the reports
  DECLARE laPrgTemps[1]
  *B602924,1 BWA [END]
 
  DECLARE laField_H[1,1],laField_N[1,1]

  STORE " " TO lcRpForm ,lcRpFiles,lcRpExpr,lcRpTargt,lcRpOrder,lcRpFields,lcRpExp
  ********** Declaring the arrays needed for the incomestatment report
  ********** temprarory defined for now only
  DIMENSION laRpMaType(1,5),laRpSType(1,6),laRpCType(1,6),laRpEType(1,6);
            ,laRpIType(1,6),laRpTType(1,6)
  DIMENSION laRpCol(1,22)     && Column information
  DIMENSION laTempCol(1,22)   && Temp Column
  
  *E301170,1 MAB 03/15/1999 llOGFltCh Defined in OG program.
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
  = SEEK(PADR(lcRpPrg,8))
  LOCATE  WHILE cRep_id= PADR(lcRpPrg,8) FOR cVer<>"A40"
  lcSetRpPrg=ALLTRIM(IIF(!EMPTY(CMAINREPID),PADR(ALLTRIM(CMAINREPID),8),PADR(ALLTRIM(lcRpPrg),8)))
  *ENDIF

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
  *B602924,1 BWA  [END]

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


*!******************************************************
*!
*!      Function : lfSetGLMsk
*!
*!******************************************************
*! E300697,1 YMA 07/13/97
*! This function is to load the GL account mask and width
*! to variables that will be used in all the screens that
*! display any GL account.
*!******************************************************

FUNCTION lfSetGLMsk
PRIVATE lnAlias

lnAlias    = SELECT(0)
llGlLink   = (ALLTRIM(gfGetMemVar('M_LINK_GL',gcAct_Comp)) = 'Y')
lcLinkWith = (ALLTRIM(gfGetMemVar('M_GL_VERS',gcAct_Comp)))

IF llGlLink
  IF lcLinkWith $ "AO"
    USE (gcSyshome + "SYCCOMP") IN 0 AGAIN ALIAS CompFile ORDER CCOMP_ID
    llNoThing  = SEEK(gcAct_Comp, "CompFile")
    lcPthToUse = gcDataDir
    IF !EMPTY(CompFile.cCompPrnt)
      lcPrntComp = CompFile.cCompPrnt
      llNoThing  = SEEK(lcPrntComp, "CompFile")
      lcPthToUse = ALLTRIM(CompFile.cCom_DDir)
    ENDIF
    USE IN CompFile
    USE (lcPthToUse + "ACCOD") IN 0 AGAIN ALIAS CodeStr ORDER AccSegNo
    SELECT CodeStr
    GOTO TOP
    lcRep     = IIF(lcLinkWith = "A", "9", "X")
    lcAcMask  = "X" + SUBSTR(STRTRAN(ALLTRIM(cAcsMask),"#",lcRep),2)
    USE IN CodeStr
    IF lcLinkWith = "A" AND !USED('lcLinkChar')
      *B602556,1 If the file is already in use in alias GLACCHAR, it gives
      *B602556,1 a 'File is in use' message. 
      *USE (lcPthToUse + "GLACCHAR") IN 0 ORDER ACCTCODE ALIAS lcLinkChar
      USE (lcPthToUse + "GLACCHAR") IN 0 ORDER ACCTCODE AGAIN ALIAS lcLinkChar
      *B602556,1 end
    ENDIF
    lnAcLen    = LEN(ALLTRIM(lcAcMask))
    lcAcntBrwF = "cAcctCode:"+ALLTRIM(STR(lnAcLen))+":H='Account Code',"+;
                 "cAccNlDes:65:H='Description'"
    lcAcntFld  = "cAcctCode"
    lcAcntDesF = "cAccNlDes"
  ELSE
    lcLinkComp = ALLTRIM(gfGetMemVar('M_GL_CO'  , gcAct_Comp))
    lcSBTGLDir = ALLTRIM(gfGetMemVar('M_SYS_DIR', gcAct_Comp))
    lcAcntChrt = lcSBTGLDir + "\GLDATA\GLACNT" + lcLinkComp + ".DBF"
    lcAcntStrc = lcSBTGLDir + "\GLDATA\GLSTRU" + lcLinkComp + ".DBF"
    lcAcMask   = SPACE(0)
    USE (lcAcntStrc) IN 0 AGAIN ALIAS AcntStrc ORDER SegID
    SELECT AcntStrc
    SCAN FOR SegLen > 0
      lcAcMask = lcAcMask + IIF(EMPTY(lcAcMask),"","-") + ALLTRIM(SegMask)
    ENDSCAN
    USE IN AcntStrc
    IF !USED("lcLinkChar")
      *B602556,1 If the file is already in use in alias GLACCHAR, it gives
      *B602556,1 a 'File is in use' message. 
      *USE (lcAcntChrt) IN 0 ALIAS lcLinkChar ORDER GlAcnt
      USE (lcAcntChrt) IN 0 AGAIN ALIAS lcLinkChar ORDER GlAcnt
      *B602556,1 end
    ENDIF
    lnAcLen    = LEN(ALLTRIM(lcAcMask))
    lcAcntBrwF = "glAcnt:"+ALLTRIM(STR(lnAcLen))+":H='Account Code',"+;
                 "glDesc:53:H='Description'"
    lcAcntFld  = "glAcnt"
    lcAcntDesF = "glDesc"
  ENDIF
  
ENDIF

SELECT (lnAlias)


*!******************************************************
*!
*!      Function : lfvGLAccnt
*!
*!******************************************************
*! E300697,1 YMA 07/13/97
*! This is a global AR function to validate any enterd
*! GL account on any AR screen, it will update the object
*! that is called from, and refresh it on the screen.
*!******************************************************

FUNCTION lfvGLAccnt
PARAMETERS lcAccDscOb, lcRefFunNm
PRIVATE lnAlias, lcCurVar, lcCurVal  , lcEmpty
PRIVATE llEmpty, laTemp  , lcBrFields, lcFile_Ttl
PRIVATE lcToGet, lcNoThing

lnAlias    = SELECT(0)
lcNoThing  = SPACE(0)
lcAccDscOb = IIF(TYPE("lcAccDscOb")#"C", "lcNoThing", lcAccDscOb)
lcRefFunNm = IIF(TYPE("lcRefFunNm")#"C", SPACE(0)   , lcRefFunNm)
llFrmBrow  = IIF(TYPE("llFrmBrow" )#"L", .F.        , llFrmBrow )
lcCurVar   = SYS(18)
lcCurVal   = EVAL(lcCurVar)
lcEmpty    = STRTRAN(lcAcMask,"9",SPACE(1))
lcEmpty    = STRTRAN(lcEmpty ,"!",SPACE(1))
lcEmpty    = STRTRAN(lcEmpty ,"X",SPACE(1))
lcEmpty    = STRTRAN(lcEmpty ,"#",SPACE(1))
llEmpty    = EMPTY(lcCurVal) OR (lcCurVal = lcEmpty)
lcRetVal   = SPACE(0)

IF (!llEmpty AND !SEEK(lcCurVal, "lcLinkChar")) OR llFrmBrow
  SELECT lcLinkChar
  DIMENSION laTemp[2]
  laTemp     = SPACE(0)
  llFrmBrow  = .F.
  lcBrFields = lcAcntBrwF
  lcFile_Ttl = "Chart of Accounts"
  lcToGet    = lcAcntFld + "," + lcAcntDesF
  IF gfBrows(.F.,lcToGet,'laTemp')
    &lcCurVar   = laTemp[1]
    &lcAccDscOb = laTemp[2]
  ELSE
    &lcCurVar   = SPACE(lnAcLen)
    &lcAccDscOb = SPACE(0)
    _CUROBJ     = _CUROBJ
  ENDIF
  IF !EMPTY(lcRefFunNm)
    DO &lcRefFunNm
  ENDIF
  SHOW GET &lcCurVar
  SELECT(lnAlias)
ENDIF  

*!********************************************************************
*! Name      : gfAmntDisp
*! Developer : Mohamed Hassan
*! Date      : 12/26/95
*! Purpose   : Return the amount according to the display condition.
*!********************************************************************
*! Parameters: lnAmount     && The amount that you want to display.
*!           : lcRpDispCur  && The way to display the amount.
*!           : ldExRateDt   && If you are going to display the amount
*!           :                 with an exchange rate of a specific date.
*!           : lcTmepFile   && The temp file name that hold the temp. 
*!           :                 exchange rates.
*!           : llAprvCurr   && If you are using the Approved currency.
*!********************************************************************
*! Call      : From all the AP reports that is using the currency display
*!           : feature.
*!********************************************************************
*! Returns   : lnAmount
*!********************************************************************
*! Example   : gfAmntDisp(APINVHDR.NINVAMNT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.).
*!********************************************************************
*E300881     : MAB 06/10/1998 in Sales Journal Report.
*E300881     : Transfer this function from AP Module to AR Module.
*
FUNCTION gfAmntDisp
PARAMETER lnAmount,lcRpDispCur,ldExRateDt,lcTmepFile,llAprvCurr
PRIVATE lnAmount,lcRpDispCur,ldExRateDt,lcTmepFil,llAprvCurr,lcExSin1,lcExSin2,lcSavAlias

lnAmount    = IIF(TYPE('lnAmount') = 'N',lnAmount,0)
lcRpDispCur = IIF(TYPE('lcRpDispCur') ='C',lcRpDispCur,'')
ldExRateDt  = IIF(TYPE('ldExRateDt') = 'D',ldExRateDt,{})
lcTmepFile  = IIF(TYPE('lcTmepFile') = 'C',lcTmepFile,'')
llAprvCurr  = IIF(TYPE('llAprvCurr') = 'L',llAprvCurr,.F.)

lcExSin1    = ''       && Variable to hold the first sign in the equation.
lcExSin2    = ''       && Variable to hold the second sign in the equation.
lcSavAlias  = ALIAS()  && Variable to save the alias.

DO CASE
  CASE lcRpDispCur = 'F'

  CASE lcRpDispCur = 'O'
    lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)

    *B601526,1 Add this lines to get the Exchange rate and the number of units
    lnExRate = 0
    lnUnit = NCURRUNIT
    lnExRate = IIF(llAprvCurr , gfChkRate('lnUnit' , lcCurrCode , DINVDATE , .F.) , NEXRATE)
    lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
    lnUnit = IIF(lnExRate <> 0 , lnUnit , 1)
    *B601526,1 Add this lines [End]
    
    *B601526,1 Change this line [Begin]
    *lnAmount   = ROUND(lnAmount &lcExSin1 IIF(llAprvCurr,NAPREXRAT,NEXRATE) &lcExSin2 IIF(llAprvCurr,NAPRCURUNT,NCURRUNIT),2)
    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit , 2)
    *B601526,1 Change this line [End]

  CASE lcRpDispCur = 'D'
    lnExRate   = 0
    lnUnit     = 0
    lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
    IF lcCurrCode = gcBaseCurr
      lnExRate = 1
      lnUnit   = 1
    ELSE

      *B601526,1 Change this line [Begin]
      *lnExRate   = gfChkRate('lnUnit',CCURRCODE,ldExRateDt,.F.)
      lnExRate   = gfChkRate('lnUnit',lcCurrCode,ldExRateDt,.F.)
      *B601526,1 Change this line [Begin]

    ENDIF

    *B601526,1 Add this lines [Begin]
    lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
    lnUnit = IIF(lnExRate <> 0 , lnUnit , 1)
    *B601526,1 Add this lines [End]

    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,2)

  CASE lcRpDispCur = 'U'

    lnExRate   = 0
    lnUnit     = 0
    lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)

    IF lcCurrCode = gcBaseCurr
      lnExRate = 1
      lnUnit   = 1
    ELSE
      IF SEEK(lcCurrCode,lcTmepFile)
        lnExRate = &lcTmepFile..NEXRATE
        lnUnit   = &lcTmepFile..NCURRUNIT
      ENDIF
    ENDIF

    *B601526,1 Add this lines [Begin]
    lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
    lnUnit = IIF(lnExRate <> 0 , lnUnit , 1)
    *B601526,1 Add this lines [End]

    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,2)
ENDCASE

RETURN lnAmount
*-- end of gfAmntDisp.

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
*! Name      : lfGetForm						*C101389,1 ASH
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
*! Name      : lfIsApparl						*C101389,1 ASH
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
*! Example	      	 : IF lfIsApparl(lcFormName) 
*!*************************************************************
*
FUNCTION RemlfIsApparl
PARAMETERS lcForm
IF !llFRXForm
  lcForm = IIF(FILE(gcRepHome + lcForm + '.FXP'),;
                    gcRepHome + lcForm , gcRepHome + gcAct_Appl+'\'+  lcForm ) 
ENDIF
RETURN !llFRXForm
