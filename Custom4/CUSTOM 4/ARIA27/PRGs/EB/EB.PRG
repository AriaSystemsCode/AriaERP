*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Task:T20081225.0020]
PARAMETERS lcProgName,lcPrgPram
lcPrgPram = IIF(TYPE('lcPrgPram')='C',lcPrgPram,'')
  #REGION 1
  DECLARE   laData[1,1]     , laDataCopy[1,1] , laFld_msg[1,2]  , laScrMode[5,1] ,;
            laKeyField[3,1] , laDefProc[10,1] , laFileName[1,2] , laField_H[1,1] ,;
            laField_N[1,1]  , laWndObj[1,3]   , laFiltrExp[1,7] , laFixFltr[1,7] ,;
            laCtrStat[15]   , laWobjects[1,3] , laArrayes[1,2]  , laSubProc[1,2] ,;
            laPrgTemps[1]   , laUsrFields[1,7]
  STORE .F. TO llAddRec     , llEditRec       , llDeleRec      

  STORE  1  TO lnCurObj     , lnBar_No        , lnDataNo        , pbRight        ,;
               pbLeft       , pbButt
  STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N      ,;
               laField_H    , laPrgNames      , laArrayes       , lcWindTitl     ,;
               lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields     ,;
               lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp        ,;
               lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr     ,;
               laWobjects   , lcButtNam       , lcCurr_yer      , lcCurr_prd     ,;
               laSubProc    , lcExDifAcc      , lcCompPrnt,laPrgTemps,lcMenUnProce,;
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
  laCtrStat[14] = "DISABLE"                && User Defined fields button   
  laCtrStat[15] = "DISABLE"                && Audit Trail button
  
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
    ON KEY LABEL ESC
    ON KEY LABEL ESC DO gfEscap
  ENDIF
  
  ON KEY LABEL CTRL+W           lnDummy = 1
  ON KEY LABEL Ctrl+Q           lnDummy = 1
  ON KEY LABEL CTRL+END         lnDummy = 1
  ON KEY LABEL CTRL+HOME        lnDummy = 1

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
  gcDosApps = gcAppHome
  
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
SET FULLPATH OFF
ON KEY LABEL ESC

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

*!*************************************************************
*! Name      : lfVldSets     
*! Developer : AHMED MOHAMMED
*! Date      : 02/12/1998
*! Purpose   : Called from SYCMSET.PRG, Checks for the validity
*!             of the entered values in the setups grid, before
*!             closing
*! REF       : *B601911,1
*!*************************************************************
*! Calls     : 
*!             Procedures  : None
*!             Functions   : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : .T. if all parameters are valid.
*!             .F. otherwise, and places the cursor on the 
*!             invalid object.
*!*************************************************************
*! Example   :  IF lfVldSets()
*!*************************************************************
FUNCTION lfVldSets

