*E300693,4  HS 01/26/98  Add some new arrays and variables needed for the
*E300693,4               Option Grid
*E301098,1  HESHAM 12/16/98 Get company data path using gfGetDataDir(..)
*E301077,14 Reham  12/30/98 Open SycComp in lfSetGLMsk() function before
*E301077,14                 calling gfGetMemVar()
*B602556,1 RENEE 02/17/99 Fix 'File is in use' bug in lfSetGlMsk()
*B602556,1 				  Remove SYCCOMP opening before gfGetMemvar()
*E301148,1 AMM 02/28/99 Enhance opening and closing files
*E301164,1 Hesham 03/08/99
*E301164,1 declare array to store all temprory files created by any program 
*E301176,1 HDM 03/21/1999 Define new Variables as The Notepad Controled Globally
*C101459,1 Hesham 03/24/99
*C101459,1 define array laUsrFields to hold user defined fields for the program base file
*E301297,1  HS  07/22/99  Implement the Object/Events triggers, Add a new
*E301297,1                function (gfDoTriger) to handle the Triggers that
*E301297,1                need to be executed when a certain event takes
*E301297,1                place from a certain screen (object).
*E301434,1 Ramy 06/28/2000 Trap the F3 key to clear the current get field
*B607038,1 ABD 07/30/2003 Add Valid Function to the new seting Credit Limit Aged Days.
*B038283,1 NNA 07/12/2004 fix bug that if the GL and AP modules not installed, then in Cash Receipt
*B038283,1 NNA            with Field [Bank GL Account] system Displays an error massage [Alias not Found ] and exit
*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Task:T20081225.0020]
*!***********************************************************************
*
PARAMETERS lcProgName,lcPrgPram

lcPrgPram = IIF(TYPE('lcPrgPram')='C',lcPrgPram,'')

#REGION 1
*E301164,1 Hesham (Start)
*E301164,1 declare array to store all temprory files created by any program 
*DECLARE   laData[1,1]     , laDataCopy[1,1] , laFld_msg[1,2]  , laScrMode[5,1] ,;
          laKeyField[3,1] , laDefProc[10,1] , laFileName[1,2] , laField_H[1,1] ,;
          laField_N[1,1]  , laWndObj[1,3]   , laFiltrExp[1,7] , laFixFltr[1,7] ,;
          laCtrStat[12]   , laWobjects[1,3] , laArrayes[1,2]  , laSubProc[1,2] ,;
          laGraphFlt[1,7]
*-HDM E301176,1 03/21/1999[Start] Changed laCtrStat[12] To laCtrStat[13]
*C101459,1 Hesham (Start)
*DECLARE   laData[1,1]     , laDataCopy[1,1] , laFld_msg[1,2]  , laScrMode[5,1] ,;
          laKeyField[3,1] , laDefProc[10,1] , laFileName[1,2] , laField_H[1,1] ,;
          laField_N[1,1]  , laWndObj[1,3]   , laFiltrExp[1,7] , laFixFltr[1,7] ,;
          laCtrStat[13]   , laWobjects[1,3] , laArrayes[1,2]  , laSubProc[1,2] ,;
          laGraphFlt[1,7],laPrgTemps[1]
          
*E301297,1 Change this line to add a new row to the array (laCtrStat) for
*          the Audit Trail button [Begin]
*DECLARE   laData[1,1]     , laDataCopy[1,1] , laFld_msg[1,2]  , laScrMode[5,1] ,;
*          laKeyField[3,1] , laDefProc[10,1] , laFileName[1,2] , laField_H[1,1] ,;
*          laField_N[1,1]  , laWndObj[1,3]   , laFiltrExp[1,7] , laFixFltr[1,7] ,;
*          laCtrStat[14]   , laWobjects[1,3] , laArrayes[1,2]  , laSubProc[1,2] ,;
*          laGraphFlt[1,7],laPrgTemps[1],laUsrFields[1,7]
DECLARE   laData[1,1]     , laDataCopy[1,1] , laFld_msg[1,2]  , laScrMode[5,1] ,;
          laKeyField[3,1] , laDefProc[10,1] , laFileName[1,2] , laField_H[1,1] ,;
          laField_N[1,1]  , laWndObj[1,3]   , laFiltrExp[1,7] , laFixFltr[1,7] ,;
          laCtrStat[15]   , laWobjects[1,3] , laArrayes[1,2]  , laSubProc[1,2] ,;
          laGraphFlt[1,7] , laPrgTemps[1]   , laUsrFields[1,7]

*E301297,1 Change this line to add a new row to the array (laCtrStat) [End]

*-HDM E301176,1 03/21/1999[End]
*C101459,1 Hesham (End)(BWA)
*E301164,1 Hesham (End)
STORE .F. TO llAddRec     , llEditRec       , llDeleRec       , llApGlLink,llApS1099

STORE  1  TO lnCurObj     , lnBar_No        , lnDataNo        , pbRight        ,;
             pbLeft       , pbButt
*E301164,1 Hesham (Start)
*E301164,1 declare array to store all temprory files created by any program 

*STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N      ,;
             laField_H    , laPrgNames      , laArrayes       , lcWindTitl     ,;
             lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields     ,;
             lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp        ,;
             lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr     ,;
             laWobjects   , lcButtNam       , lcCurr_yer      , lcCurr_prd     ,;
             laSubProc    , lcExDifAcc      , lcCompPrnt      , laGraphFlt
*-- HDM E301176,1 03/21/1999 [Start]Define new Variables as The Notepad Controled Globally
*C101459,1 Hesham (Start)
*STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N      ,;
             laField_H    , laPrgNames      , laArrayes       , lcWindTitl     ,;
             lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields     ,;
             lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp        ,;
             lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr     ,;
             laWobjects   , lcButtNam       , lcCurr_yer      , lcCurr_prd     ,;
             laSubProc    , lcExDifAcc      , lcCompPrnt      , laGraphFlt,laPrgTemps,lcMenUnProce

STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N      ,;
             laField_H    , laPrgNames      , laArrayes       , lcWindTitl     ,;
             lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields     ,;
             lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp        ,;
             lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr     ,;
             laWobjects   , lcButtNam       , lcCurr_yer      , lcCurr_prd     ,;
             laSubProc    , lcExDifAcc      , lcCompPrnt      , laGraphFlt,laPrgTemps,lcMenUnProce,;
             lcSydKey     , lcNoteType      , lcNoteKey       ,laUsrFields 
*C101459,1 Hesham (End)
*-- HDM E301176,1 03/21/1999 [End]

*E301164,1 Hesham (End)

STORE 0   TO lnCurr_yer   , lnCurr_prd

STORE {}  TO ldFisBgDat   , ldFisEnDat      , ldPyBgDate      , ldNyEnDate

*E300693,4 Add this lines to add some new arrays and variables needed for
*the Option Grid [Begin]
DECLARE laOGFxFlt[1,7] , laOGVrFlt[1,7] , laRepFltr[1,2]
STORE '' TO lcReportID , laOGFxFlt[1,7] , laOGVrFlt[1,7] , laRepFltr[1,2]
*E300693,4 Add this lines to add some new arrays and variables [End]

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

laCtrStat[13] = "DISABLE"                && Notepad Button

*C101459,1 Hesham (Start)
laCtrStat[14] = "DISABLE"                && User defined fields button 
*C101459,1 Hesham (End)

*E301297,1 Add this line to add a new row to the array (laCtrStat) for the
*          Audit Trail button [Begin]
laCtrStat[15]  = "DISABLE"                && Audit Trail button
*E301297,1 Add this line to add a new row to the array (laCtrStat) [End]

laScrMode     = .F.
laScrMode[1]  = .T.
laFld_msg     = SPACE (51)
laDefProc     = .T.
llNewSetup    = IIF(TYPE('llNewSetup') = 'U',.F.,llNewSetup)
lcSelCont     = SCHEME(1,6) 
lcEnbCont     = SCHEME(1,2) 
lcDisCont     = SCHEME(1,10) 


*E300697,1 YMA 07/13/97 Define the variables that will hold the 
*E300697,1              GL account mask and length.
lcAcMask   = SPACE(0)         && Account mask
lnAcLen    = 0                && Account Length
lcAcntBrwF = SPACE(0)         && Chart of account browse fields.
lcAcntFld  = SPACE(0)         && Account field that will be validated.
lcAcntDesF = SPACE(0)         && Account Description.
llNoThing  = lfSetGLMsk()
*E300697,1 YMA 07/13/97 End.

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
  *E301434,1 Ramy [start]
  *ON KEY LABEL ESC
  lcESC  = ON('KEY' , 'ESC')
  *E301434,1 Ramy [end]
  ON KEY LABEL ESC DO gfEscap
ENDIF

*E301434,1 Ramy [start]
lcCtrW = ON('KEY' , 'CTRL+W')
lcCtrQ = ON('KEY' , 'CTRL+Q')
lcCtrE = ON('KEY' , 'CTRL+END')
lcCtrH = ON('KEY' , 'CTRL+HOME')
*E301434,1 Ramy [end]

ON KEY LABEL CTRL+W           lnDummy = 1
ON KEY LABEL Ctrl+Q           lnDummy = 1
ON KEY LABEL CTRL+END         lnDummy = 1
ON KEY LABEL CTRL+HOME        lnDummy = 1

*E301434,1 Ramy Add this line to trap F3 key [start]
lcF3Key = ON('KEY' , 'F3')
ON KEY LABEL F3               DO lfF3Key
*E301434,1 Ramy [end]

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
llPrgFound =(GCMULTIINST AND (FILE(gcCDosApps+lcPrgTmp +'.FXP') .OR. ;
   FILE(gcCDosApps+lcPrgTmp +'.PRG') .OR. ;
   FILE(gcCDosApps+lcPrgTmp +'.EXE'))) OR ;
   FILE(gcDosApps+lcPrgTmp +'.FXP') .OR. ;
   FILE(gcDosApps+lcPrgTmp +'.PRG') .OR. ;
   FILE(gcDosApps+lcPrgTmp +'.EXE')
*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [End]

IF !llPrgFound
  gcDosApps  = gcAppHome
  
  *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Start]
  *llPrgFound = FILE(gcDosApps+lcPrgTmp +'.FXP') .OR. ;
               FILE(gcDosApps+lcPrgTmp +'.PRG') .OR. ;
               FILE(gcDosApps+lcPrgTmp +'.EXE')
  IF GCMULTIINST              
    gcCDosApps = gcCappHome         
  ENDIF   
  llPrgFound = (GCMULTIINST AND (FILE(gcCDosApps+lcPrgTmp +'.FXP') .OR. ;
	 FILE(gcCDosApps+lcPrgTmp +'.PRG') .OR. ;
	 FILE(gcCDosApps+lcPrgTmp +'.EXE'))) OR ;
	 FILE(gcDosApps+lcPrgTmp +'.FXP') .OR. ;
     FILE(gcDosApps+lcPrgTmp +'.PRG') .OR. ;
     FILE(gcDosApps+lcPrgTmp +'.EXE')
  *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [End]
  
ENDIF   

IF llPrgFound
  *E300581,1 Hesham El-Sheltawi (Start)
  *E300581,1 make the menu can call the program with paramters
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
  *E300581,1 Hesham El-Sheltawi (End)  
ELSE
  DO &lcProgName
  #REGION 1
ENDIF  


*E301148,1 Renee Close file
IF USED('lcLinkChar')
  USE IN lcLinkChar
ENDIF
*E301148,1 AMM end

SET FULLPATH OFF

*E301434,1 Ramy [start]
*ON KEY LABEL ESC 
ON KEY LABEL ESC &lcESC
ON KEY LABEL F3 &lcF3Key
*E301434,1 Ramy [end]

= gfCleanUp()


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
*  Author  : Yasser Saad Ibrahim
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

*********************************************************************

FUNCTION lfVldSets


*!******************************************************
*!
*!      Function : lfSetGLMsk
*!
*!******************************************************
*E300697,1 YMA 07/13/97
* This function is to load the GL account mask and width
* to variables that will be used in all the screens that
* display any GL account.
*E301077,14 Reham On 12/30/98
*E301077,14 Open SycComp before calling gfGetMemVar()
*!******************************************************

FUNCTION lfSetGLMsk
PRIVATE lnAlias

lnAlias    = SELECT(0)

*B602556,1 Do not use SYCCOMP
*E301077,14 Reham On 12/30/98   *** Begin ***
*E301077,14 Open the company file.
*=gfOpenFile(gcSysHome+'SycComp',gcSysHome+'cComp_Id','SH')
*E301077,14 Reham On 12/30/98   *** End   ***
*B602556,1 end

llGlLink   = (ALLTRIM(gfGetMemVar('M_LINK_GL',gcAct_Comp)) = 'Y')
lcLinkWith = (ALLTRIM(gfGetMemVar('M_GL_VERS',gcAct_Comp)))
*E301148,1 AMM Initialize flag
llOpAcCh = .F.
*E301148,1 AMM end

IF llGlLink
  IF lcLinkWith $ "AO"
    USE (gcSyshome + "SYCCOMP") IN 0 AGAIN ALIAS CompFile ORDER CCOMP_ID
    llNoThing  = SEEK(gcAct_Comp, "CompFile")
    lcPthToUse = gcDataDir
    IF !EMPTY(CompFile.cCompPrnt)
      lcPrntComp = CompFile.cCompPrnt
      llNoThing  = SEEK(lcPrntComp, "CompFile")
      *E301098,1 Hesham (Start)
      *lcPthToUse = ALLTRIM(CompFile.cCom_DDir)
      lcPthToUse = gfGetDataDir(ALLTRIM(CompFile.cCom_DDir))
      *E301098,1 Hesham (eND)
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
      *E301148,1 AMM Set the flag with .T. to close file when exiting
      llOpAcCh = .T.
      *E301148,1 AMM end
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
      *E301148,1 AMM Set the flag to .T. to close the file when exiting
      llOpAcCh = .T.
      *E301148,1 AMM end
    ENDIF
    lnAcLen    = LEN(ALLTRIM(lcAcMask))
    lcAcntBrwF = "glAcnt:"+ALLTRIM(STR(lnAcLen))+":H='Account Code',"+;
                 "glDesc:53:H='Description'"
    lcAcntFld  = "glAcnt"
    lcAcntDesF = "glDesc"
  ENDIF
  
ENDIF

*E301148,1 AMM Close file
*IF llOpAcCh 
*  USE IN lcLinkChar
*ENDIF
*E301148,1 AMM end

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

*B038283,1 NNA 07/12/2004 (Begin) Check If [lcLinkChar] file Used or not
IF USED('lcLinkChar')
*B038283,1 NNA (End)

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

*B038283,1 NNA (BEGIN)
ENDIF
*B038283,1 NNA (END)

*B607038,1 ABD - [Begin]
*:*************************************************************
*: Name      : lfvCRLMDay
*: Developer : Abdou Elgendy
*: Date      : 07/29/2003
*: Purpose   : Enable and Disable Credt limit aged days seting.
*: Refer to  : B607038,1 
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfvCRLMDay ()
*:*************************************************************
*:
FUNCTION lfvCRLMDay

IF M_CRDT_LMT = 'Y'
  = lfOGShowGet('M_CRLM_DAY',.T.)
ELSE
  = lfOGShowGet('M_CRLM_DAY',.F.)
ENDIF


*-- End OF lfvCRLMDay
*:*************************************************************
*: Name      : lfOGRShow
*: Developer : Abdou Elgendy
*: Date      : 07/29/2003
*: Purpose   : Show objecrts function.
*: Refer to  : B607038,1 
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfOGRShow ()
*:*************************************************************
*:
FUNCTION lfOGRShow

= lfOGShowGet('M_CRLM_DAY',(M_CRDT_LMT = 'Y'))

*-- End OF lfOGRShow
*B607038,1 ABD - [End]
*:*************************************************************