*:************************************************************************
*: Program file  : CR.PRG
*: Program desc. : Customer Relationship Managment "CR"
*:
*:         System: Aria Apparel System
*:      Developer: Reham Al-Allamy
*:************************************************************************
*: Calls : 
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Modifications      :
*****************************************************************************
PARAMETERS lcProgName,lcPrgPram

EXTERNAL PROCEDURE GFOPGRID
lcPrgPram = IIF(TYPE('lcPrgPram')='C',lcPrgPram,'')
  #REGION 1
DECLARE   laData[1,1]     , laDataCopy[1,1] , laFld_msg[1,2]  , laScrMode[5,1] ,;
          laKeyField[3,1] , laDefProc[10,1] , laFileName[1,2] , laField_H[1,1] ,;
          laField_N[1,1]  , laWndObj[1,3]   , laFiltrExp[1,7] , laFixFltr[1,7] ,;
          laCtrStat[15]   , laWobjects[1,3] , laArrayes[1,2]  , laSubProc[1,2],laPrgTemps[1],;
          laUsrFields[1,7]

DECLARE   laOGFxFlt[1,7],laOGVrFlt[1,7],laRepFltr[1,2]
STORE '' TO laOGFxFlt,laOGVrFlt, lcReportID   ,laRepFltr

STORE .F. TO llAddRec     , llEditRec       , llDeleRec      

STORE  1  TO lnCurObj     , lnBar_No        , lnDataNo        , pbRight        ,;
             pbLeft       , pbButt
STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N        ,;
             laField_H    , laPrgNames      , laArrayes       , lcWindTitl       ,;
             lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields       ,;
             lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp          ,;
             lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr       ,;
             laWobjects   , lcButtNam       , lcCurr_yer      , lcCurr_prd       ,;
             laSubProc    , lcExDifAcc      , lcCompPrnt,laPrgTemps,lcMenUnProce ,;
             lcSydKey     , lcNoteType      , lcNoteKey,laUsrFields

STORE 0   TO lnCurr_yer   , lnCurr_prd

STORE {}  TO ldFisBgDat   , ldFisEnDat      , ldPyBgDate      , ldNyEnDate

  laCtrStat[1]  = "DISABLE"                && First button    
  laCtrStat[2]  = "DISABLE"                && Last button
  laCtrStat[3]  = "DISABLE"                && Next button
  laCtrStat[4]  = "DISABLE"                && Previous button   
  laCtrStat[5]  = "DISABLE"                && Copy button
  laCtrStat[6]  = "DISABLE"                && Past Button
  laCtrStat[7]  = "DISABLE"                && Edit button 
  laCtrStat[8]  = "DISABLE"                && Delete button
  laCtrStat[9]  = "DISABLE"                && Select button
  laCtrStat[10] = "ENABLE"                 && Browse button
  laCtrStat[11] = "DISABLE"                && Save button 
  laCtrStat[12] = "ENABLE"                 && Close/Cancel button 
  laCtrStat[13] = "DISABLE"
  laCtrStat[14] = "DISABLE"                 && User Defined fields button   
  laCtrStat[15]  = "DISABLE"                && Audit Trail button
  
  laScrMode     = .F.
  laScrMode[1]  = .T.
  laFld_msg     = SPACE (51)
  laDefProc     = .T.
  llNewSetup    = IIF(TYPE('llNewSetup') = 'U',.F.,llNewSetup)
  lcSelCont     = SCHEME(1,6) 
  lcEnbCont     = SCHEME(1,2) 
  lcDisCont     = SCHEME(1,10) 

  *** Colors used under Windows for objects' borders and
  *** windows backgrounds
  *** - Color used for top and left corners of an object
  lcTopLeft  = 'RGB(128,128,128,128,128,128)'      && Dark gray
  *** - Color used for bottom and right corners of an object
  lcBotRight = 'RGB(255,255,255,255,255,255)'      && White 
  *** - Background color of windows 
  lcBackGrnd = 'RGB(192,192,192,192,192,192)'      && Light gray

  lcHidObjNrm= SUBSTR(SCHEME(1,1),ATC('/',SCHEME(1,1))+1)
  lcHidObjNrm= lcHidObjNrm+'/'+STRTRAN(lcHidObjNrm,'*','+')
  lcHidObjNrm= IIF(_WINDOWS,',RGB(192,192,192,192,192,192),,,,,,,,RGB(192,192,192,192,192,192)',lcHidObjNrm)  

  lcHidObjDlg= SUBSTR(SCHEME(5,1),ATC('/',SCHEME(5,1))+1)
  lcHidObjDlg= lcHidObjDlg+'/'+STRTRAN(lcHidObjDlg,'*','+')
  lcHidObjDlg= IIF(_WINDOWS,',RGB(192,192,192,192,192,192),,,,,,,,RGB(192,192,192,192,192,192)',lcHidObjDlg)  

  lcBaseWind = " "
  lcLoclShow = "lpShow"

  llMultiRun = glMultiRun

  STORE .F. TO llUserSet,llUpdate,llNoContrl,llFromBrow,llDoLocal,glQuitting,;
               llCUpdate

  llglobShow = .T.

  lnDateWdth = IIF('ON'$SET('CENT'),10,8)
  lnRecNo    = 0
  IF _WINDOWS
    lcESC  = ON('KEY' , 'ESC')
    ON KEY LABEL ESC DO gfEscap
  ENDIF

  lcCtrW = ON('KEY' , 'CTRL+W')
  lcCtrQ = ON('KEY' , 'CTRL+Q')
  lcCtrE = ON('KEY' , 'CTRL+END')
  lcCtrH = ON('KEY' , 'CTRL+HOME')
  
  ON KEY LABEL CTRL+W           lnDummy = 1
  ON KEY LABEL Ctrl+Q           lnDummy = 1
  ON KEY LABEL CTRL+END         lnDummy = 1
  ON KEY LABEL CTRL+HOME        lnDummy = 1

  lcF3Key = ON('KEY' , 'F3')
  ON KEY LABEL F3               DO lfF3Key

lcPrgTmp = IIF(ATC(' ',lcProgName)>0,SUBSTR(lcProgName,1,ATC(' ',lcProgName)-1),lcProgName)
gcDosApps = gcAppHome+gcWinAppl+'\'

*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Start]
IF GCMULTIINST 
  gcCDosApps = gcCappHome+gcWinAppl+'\'
ENDIF 
*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [End]


SET FULLPATH ON
*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Start]
*llPrgFound = FILE(gcDosApps+lcPrgTmp +'.FXP') .OR. ;
   FILE(gcDosApps+lcPrgTmp +'.PRG') .OR. ;
   FILE(gcDosApps+lcPrgTmp +'.EXE')
llPrgFound = (GCMULTIINST AND (FILE(gcCDosApps +lcPrgTmp +'.FXP') .OR. ;
   FILE(gcCDosApps +lcPrgTmp +'.PRG') .OR. ;
   FILE(gcCDosApps +lcPrgTmp +'.EXE'))) OR ;
   FILE(gcDosApps+lcPrgTmp +'.FXP') .OR. ;
   FILE(gcDosApps+lcPrgTmp +'.PRG') .OR. ;
   FILE(gcDosApps+lcPrgTmp +'.EXE')
*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [End]   
   
IF !llPrgFound
  gcDosApps = gcAppHome
  
  *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Start]
  *llPrgFound = FILE(gcDosApps+lcPrgTmp +'.FXP') .OR. ;
     FILE(gcDosApps+lcPrgTmp +'.PRG') .OR. ;
     FILE(gcDosApps+lcPrgTmp +'.EXE')
  IF GCMULTIINST     
    gcCDosApps = gcCappHome
  ENDIF   
  llPrgFound = (GCMULTIINST AND (FILE(gcCDosApps +lcPrgTmp +'.FXP') .OR. ;
	 FILE(gcCDosApps +lcPrgTmp +'.PRG') .OR. ;
	 FILE(gcCDosApps +lcPrgTmp +'.EXE'))) OR ;
	 FILE(gcDosApps+lcPrgTmp +'.FXP') .OR. ;
     FILE(gcDosApps+lcPrgTmp +'.PRG') .OR. ;
     FILE(gcDosApps+lcPrgTmp +'.EXE')
     
  *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [End]     
  
ENDIF   
IF llPrgFound
  IF !EMPTY(lcPrgPram)
    *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Start]
    *DO (gcDosApps+lcPrgTmp) WITH &lcPrgPram
    IF (GCMULTIINST AND (FILE(gcCDosApps +lcPrgTmp +'.FXP') .OR. ;
	 FILE(gcCDosApps +lcPrgTmp +'.PRG') .OR. ;
	 FILE(gcCDosApps +lcPrgTmp +'.EXE')))
	 
 	  DO (gcCDosApps +lcPrgTmp) WITH &lcPrgPram
	ELSE
	  DO (gcDosApps+lcPrgTmp) WITH &lcPrgPram
    ENDIF
    *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [End]

  ELSE
    *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Start]
    *DO (gcDosApps+lcPrgTmp)
    IF (GCMULTIINST AND (FILE(gcCDosApps +lcPrgTmp +'.FXP') .OR. ;
   	   FILE(gcCDosApps +lcPrgTmp +'.PRG') .OR. ;
	   FILE(gcCDosApps +lcPrgTmp +'.EXE')))
	 
 	  DO (gcCDosApps +lcPrgTmp) 
	ELSE
	  DO (gcDosApps+lcPrgTmp) 
    ENDIF
    *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [End]

  ENDIF  
ELSE

  DO &lcProgName

  #REGION 1
ENDIF  
SET FULLPATH OFF

ON KEY LABEL ESC &lcESC
ON KEY LABEL F3 &lcF3Key

=gfCleanUp()



*!**************************************************************************
*!
*!      Function : lfEscap
*!
*!**************************************************************************
*
FUNCTION lfEscap

IF laScrMode[3] .OR. laScrMode[4]
  lcScMode =IIF(laScrMode[3],"editing !!","adding !!")
  WAIT "You cannot leave this file during "+lcScMode WINDOW NOWAIT
ELSE
  glQuitting = .T.   
  CLEAR READ
ENDIF  

*!**************************************************************************
*!
*!      Function: lfGetIndTag
*!
*!**************************************************************************
*
FUNCTION lfGetIndTag

lcTag = ' TAG '+SYS(22) + IIF('DESC' $ SET('ORDER'),' DESC','')
RETURN lcTag

*!**************************************************************************
*!
*!      Function : VALIDALL
*!
*!**************************************************************************
*
FUNCTION VALIDALL
PARAMETERS lcFuncName

DO (lcFuncName) IN TEST

*!**************************************************************************
*!
*!      Function: lfwActBrow
*!
*!**************************************************************************
* This function inactive lfvActBrow if the user didn't click left mouse
FUNCTION lfwActBrow

RETURN MDOWN()

*!**************************************************************************
*!
*!      Function: lfvActBrow
*!
*!**************************************************************************
* This function Activate the browse
FUNCTION lfvActBrow

PARAMETERS lcObjName

_CUROBJ  = OBJNUM(lcObjName)
llBrowse = .T.
KEYBOARD "{ENTER}"

*!*************************************************************
*! Name       : lfvLetters
*! Developer  : Reham Al-Allamy
*! Date       : 06/25/2001
*! Purpose    : Letter Id's Function
*!*************************************************************
*! Calls      : None
*!*************************************************************
*! Called From: Setup Screen
*!*************************************************************
*! Parameters : None
*!*************************************************************
*! Returns    :  None.
*!*************************************************************
*! Example    :  =lfvLetters()
*!*************************************************************
*
FUNCTION lfvLetters
PRIVATE lcValue , llOpenLet , lcAlias

lcValue   = EVALUATE(VARREAD())
llOpenLet = .F.

lcAlias = ALIAS()

IF !USED("LETTERS")
  =gfOpenFile(gcDataDir+'LETTERS',gcDataDir+'Cletterid','SH')
  llOpenLet = .T.
ENDIF

IF !SEEK(lcValue,"LETTERS")
  =GFVLFLD('LETTERS','cletterid',VARREAD(),'',.F.,.F.,[cletterid:H="Letter ID",cletshdes:H="Letter Description"],'1',.F.,.t.)
ENDIF

IF llOpenLet
  =gfCloseFile("LETTERS")
ENDIF

SELECT (lcAlias)


*!*************************************************************
*! Name       : lfvCusPass
*! Developer  : Reham Al-Allamy
*! Date       : 06/25/2001
*! Purpose    : Customer Password Function
*!*************************************************************
*! Calls      : None
*!*************************************************************
*! Called From: Setup Screen
*!*************************************************************
*! Parameters : None
*!*************************************************************
*! Returns    :  None.
*!*************************************************************
*! Example    :  =lfvCusPass()
*!*************************************************************
*
FUNCTION lfvCusPass
PRIVATE lcValue , llOpenFld , lcAlias , laField , lcKey , lcOrder

lcValue   = EVALUATE(VARREAD())
llOpenFld = .F.

lcAlias = ALIAS()

IF !USED("SYDFIELD")
  =gfOpenFile(gcSysHome+'SYDFIELD',gcSysHome+'Cfld_name','SH')
  llOpenFld = .T.
ELSE
  SELECT SYDFIELD
  lcOrder = ORDER()
  SET ORDER TO TAG Cfld_name
ENDIF

IF !SEEK(lcValue,"SYDFIELD")
  lcSaveBrow = lcBrFields
  lcBrFields = [cfld_name:H="Field Code",cfld_head:H="Field Header"]
  lcOld_ttl  = lcFile_ttl
  lcFile_ttl = "Users' Fields"
  DECLARE laField[1]
  laField[1] = M_CUSTPASS
  lcKey      = "USR_DFND"
  
  *-- Call the browse with the Fields file.
  =gfBrows("lcKey" , "cFld_Name" , "laField")
  lcBrFields = lcSaveBrow
  lcFile_ttl = lcOld_ttl
  M_CUSTPASS = IIF(M_CUSTPASS=laField[1] , SPACE(10) , laField[1])
ENDIF

IF !EMPTY(M_CUSTPASS) AND !EMPTY(M_CONFMAIL) AND (ALLTRIM(M_CUSTPASS) == ALLTRIM(M_CONFMAIL))
  *** "Confirmation e-mail address field must be different from the customer password field." ***
  *** <  Ok  > ***
  =gfModalGen("TRM24000B00000","Dialog")
  RETURN .F.
ENDIF

IF llOpenFld
  =gfCloseFile("SYDFIELD")
ELSE
  SELECT SYDFIELD
  SET ORDER TO &lcOrder
ENDIF
SELECT (lcAlias)

*!*************************************************************
*! Name       : lfvConMail
*! Developer  : Reham Al-Allamy
*! Date       : 06/25/2001
*! Purpose    : Customer Confirmation Mail
*!*************************************************************
*! Calls      : None
*!*************************************************************
*! Called From: Setup Screen
*!*************************************************************
*! Parameters : None
*!*************************************************************
*! Returns    :  None.
*!*************************************************************
*! Example    :  =lfvConMail()
*!*************************************************************
*
FUNCTION lfvConMail
PRIVATE lcValue , llOpenFld , lcAlias , laField , lcKey , lcOrder

lcValue   = EVALUATE(VARREAD())
llOpenFld = .F.

lcAlias = ALIAS()

IF !USED("SYDFIELD")
  =gfOpenFile(gcSysHome+'SYDFIELD',gcSysHome+'Cfld_name','SH')
  llOpenFld = .T.
ELSE
  SELECT SYDFIELD
  lcOrder = ORDER()
  SET ORDER TO TAG Cfld_name
ENDIF


IF !EMPTY(lcValue)
  IF !SEEK(lcValue,"SYDFIELD")
    lcSaveBrow = lcBrFields
    lcBrFields = [cfld_name:H="Field Code",cfld_head:H="Field Header"]
    lcOld_ttl  = lcFile_ttl
    lcFile_ttl = "Users' Fields"
    DECLARE laField[1]
    laField[1] = M_CONFMAIL
    lcKey      = "USR_DFND"
    
    *-- Call the browse with the Fields file.
    =gfBrows("lcKey" , "cFld_Name" , "laField")
    lcBrFields = lcSaveBrow
    lcFile_ttl = lcOld_ttl
    M_CONFMAIL = IIF(M_CONFMAIL = laField[1] , SPACE(10) , laField[1])
  ENDIF
  
  IF !EMPTY(M_CUSTPASS) AND !EMPTY(M_CONFMAIL) AND (ALLTRIM(M_CUSTPASS) == ALLTRIM(M_CONFMAIL))
    *** "Confirmation e-mail address field must be different from the customer password field." ***
    *** <  Ok  > ***
    =gfModalGen("TRM24000B00000","Dialog")
    RETURN .F.
  ENDIF
ENDIF

IF llOpenFld
  =gfCloseFile("SYDFIELD")
ELSE
  SELECT SYDFIELD
  SET ORDER TO &lcOrder
ENDIF
SELECT (lcAlias)

