*B601911,1 AMM 02/12/98 1- Add the lfvldSets() empty function for future use 
*B601911,1 AMM          and to prevent calling the function with the same name in the 
*B601911,1 AMM          SM module.
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
*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Task:T20081225.0020]
******************************************************************************

PARAMETERS lcProgName,lcPrgPram
lcPrgPram = IIF(TYPE('lcPrgPram')='C',lcPrgPram,'')
  #REGION 1

*E301164,1 Hesham (Start)
*E301164,1 declare array to store all temprory files created by any program 

*  DECLARE   laData[1,1]     , laDataCopy[1,1] , laFld_msg[1,2]  , laScrMode[5,1] ,;
            laKeyField[3,1] , laDefProc[10,1] , laFileName[1,2] , laField_H[1,1] ,;
            laField_N[1,1]  , laWndObj[1,3]   , laFiltrExp[1,7] , laFixFltr[1,7] ,;
            laCtrStat[12]   , laWobjects[1,3] , laArrayes[1,2]  , laSubProc[1,2]

*-HDM E301176,1 03/21/1999[Start] Changed laCtrStat[12] To laCtrStat[13]
*C101459,1 Hesham (Start)
*  DECLARE   laData[1,1]     , laDataCopy[1,1] , laFld_msg[1,2]  , laScrMode[5,1] ,;
            laKeyField[3,1] , laDefProc[10,1] , laFileName[1,2] , laField_H[1,1] ,;
            laField_N[1,1]  , laWndObj[1,3]   , laFiltrExp[1,7] , laFixFltr[1,7] ,;
            laCtrStat[13]   , laWobjects[1,3] , laArrayes[1,2]  , laSubProc[1,2],laPrgTemps[1]

  *E301297,1 Change this line to add a new row to the array (laCtrStat) for
  *          the Audit Trail button [Begin]
  *DECLARE   laData[1,1]     , laDataCopy[1,1] , laFld_msg[1,2]  , laScrMode[5,1] ,;
  *          laKeyField[3,1] , laDefProc[10,1] , laFileName[1,2] , laField_H[1,1] ,;
  *          laField_N[1,1]  , laWndObj[1,3]   , laFiltrExp[1,7] , laFixFltr[1,7] ,;
  *          laCtrStat[14]   , laWobjects[1,3] , laArrayes[1,2]  , laSubProc[1,2],laPrgTemps[1],;
  *          laUsrFields[1,7]
  DECLARE   laData[1,1]     , laDataCopy[1,1] , laFld_msg[1,2]  , laScrMode[5,1] ,;
            laKeyField[3,1] , laDefProc[10,1] , laFileName[1,2] , laField_H[1,1] ,;
            laField_N[1,1]  , laWndObj[1,3]   , laFiltrExp[1,7] , laFixFltr[1,7] ,;
            laCtrStat[15]   , laWobjects[1,3] , laArrayes[1,2]  , laSubProc[1,2] ,;
            laPrgTemps[1]   , laUsrFields[1,7]
  
  *E301297,1 Change this line to add a new row to the array (laCtrStat) [End]

*C101459,1 Hesham (End)


*-HDM E301176,1 03/21/1999[End]

*E301164,1 Hesham (End)

  STORE .F. TO llAddRec     , llEditRec       , llDeleRec      

  STORE  1  TO lnCurObj     , lnBar_No        , lnDataNo        , pbRight        ,;
               pbLeft       , pbButt

*E301164,1 Hesham (Start)
*E301164,1 declare array to store all temprory files created by any program 

*  STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N      ,;
               laField_H    , laPrgNames      , laArrayes       , lcWindTitl     ,;
               lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields     ,;
               lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp        ,;
               lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr     ,;
               laWobjects   , lcButtNam       , lcCurr_yer      , lcCurr_prd     ,;
               laSubProc    , lcExDifAcc      , lcCompPrnt
*-- HDM E301176,1 03/21/1999 [Start]Define new Variables as The Notepad Controled Globally

*  STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N      ,;
               laField_H    , laPrgNames      , laArrayes       , lcWindTitl     ,;
               lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields     ,;
               lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp        ,;
               lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr     ,;
               laWobjects   , lcButtNam       , lcCurr_yer      , lcCurr_prd     ,;
               laSubProc    , lcExDifAcc      , lcCompPrnt,laPrgTemps,lcMenUnProce
*C101459,1 Hesham (Start)

*  STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N      ,;
               laField_H    , laPrgNames      , laArrayes       , lcWindTitl     ,;
               lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields     ,;
               lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp        ,;
               lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr     ,;
               laWobjects   , lcButtNam       , lcCurr_yer      , lcCurr_prd     ,;
               laSubProc    , lcExDifAcc      , lcCompPrnt,laPrgTemps,lcMenUnProce,;
               lcSydKey     , lcNoteType      , lcNoteKey

  STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N      ,;
               laField_H    , laPrgNames      , laArrayes       , lcWindTitl     ,;
               lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields     ,;
               lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp        ,;
               lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr     ,;
               laWobjects   , lcButtNam       , lcCurr_yer      , lcCurr_prd     ,;
               laSubProc    , lcExDifAcc      , lcCompPrnt,laPrgTemps,lcMenUnProce,;
               lcSydKey     , lcNoteType      , lcNoteKey,laUsrFields

*C101459,1 Hesham (End)


*-- HDM E301176,1 03/21/1999 [End]

*E301164,1 Hesham (End)


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

  *C101459,1 Hesham (Start)
  laCtrStat[14] = "DISABLE"                 && User Defined fields button   
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

*E301434,1 Ramy [start]
*ON KEY LABEL ESC 
ON KEY LABEL ESC &lcESC
ON KEY LABEL F3 &lcF3Key
*E301434,1 Ramy [end]

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