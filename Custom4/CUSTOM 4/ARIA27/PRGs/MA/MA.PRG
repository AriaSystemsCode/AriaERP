*B601911,1 AMM 02/12/98 Add a function lfvChgDye() to validate the dyelot 
*B601911,1 AMM          setting.
*E300930,1 AMM 11/04/98 Add the validation of material cost method setting 
*E300930,1 AMM          in the setups screen
*E301098,1 HESHAM 12/16/98 Get company data path using gfGetDataDir(..)
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
*E500329,1 WAB 08/06/2000 Generate Sequential numbers when receiving by rolls
*E500330,1 WAB 08/06/2000 Generate sequential numbers when doing physical or inv adj
*E500349,1 WAB 08/20/2000 ability to enter manual material PO #  
*C200254,1 ABD 03/24/2002 ability to enter manual material rework order number.  
*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Task:T20081225.0020]
***************************************************************************
PARAMETERS lcProgName,lcprgpram
lcprgpram = IIF(TYPE('lcprgpram')='C',lcprgpram,'')
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

*  STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N       ,;
               laField_H    , laPrgNames      , laArrayes       , lcWindTitl      ,;
               lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields      ,;
               lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp         ,;
               lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr      ,;
               laWobjects   , lcButtNam       , lcCurr_yer      , lcCurr_prd      ,;
               laSubProc    , lcExDifAcc      , lcCompPrnt,laPrgTemps,lcMenUnProce,;
              lcSydKey     , lcNoteType      , lcNoteKey

  STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N       ,;
               laField_H    , laPrgNames      , laArrayes       , lcWindTitl      ,;
               lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields      ,;
               lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp         ,;
               lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr      ,;
               laWobjects   , lcButtNam       , lcCurr_yer      , lcCurr_prd      ,;
               laSubProc    , lcExDifAcc      , lcCompPrnt,laPrgTemps,lcMenUnProce,;
              lcSydKey     , lcNoteType      , lcNoteKey ,laUsrFields

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
  laCtrStat[13] = "DISABLE"                 && User Defined fields button   

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

*!**************************************************************************
*!
*!      Function: lfVldSets
*!
*!**************************************************************************


FUNCTION lfVldSets


*!**************************************************************************
*! Name      : lfvChgDye
*! Developer : Ahmed Mohammed
*! Date      : 02/12/1998
*! Purpose   : Validate the changing of dyelot setup from yes to No.
*! REF       : *B601911,1
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : gfModalGen(), gfTempName()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvChgDye()
*!**************************************************************************
FUNCTION lfvChgDye

IF M_MATDYE = 'N'
  lnAlias  = SELECT(0)
  lcTFab   = gfTempName()
  SELECT 0
  *E301098,1 Hesham (Start)
  *USE ALLTRIM(SyCCOMP.cCom_DDir)+'FABRIC.DBF' AGAIN ALIAS (lcTFab)
  USE gfGetDataDir(ALLTRIM(SyCCOMP.cCom_DDir))+'FABRIC.DBF' AGAIN ALIAS (lcTFab)
  *E301098,1 Hesham (End)
  *B601911,1 Search for any material using dyelots
  LOCATE FOR cDYE_Flg = 'Y' 
  *B601911,1 If any material using dyelots is found, do not allow
  *B601911,1 changing dyelot from Yes to No
  IF FOUND()
    *B601911,1 AMM the message is "Some Materials are in dyelots,You need to change"+ 
    *B601911,1 AMM "these items to no before you can change the system setup."    
    =gfModalGen("QRM00291B00000","DIALOG",'Materials|items') 
    M_MATDYE = 'Y'
    =lfOGShowGet('M_MATDYE',.T.)
  ENDIF
  USE IN (lcTFab)
  SELECT (lnAlias)
ENDIF

*!**************************************************************************
*! Name      : lfvmaCsM
*! Developer : Ahmed Mohammed
*! Date      : 11/04/1998
*! Purpose   : Validate the changing of material cost method
*! REF       : *E300930,1 AMM
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : lfOGShowGet()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvmaCsM()
*!**************************************************************************
FUNCTION lfvmaCsM
IF M_MATCSTMT = 'L'
  *E300930,1 AMM If material cost method is Lot, Enable the keep tracking 
  *E300930,1 AMM of rolls setting
  =lfOGShowGet('M_TRKROLLS',.T.)
ELSE
  M_TRKROLLS = 'N'
  =lfOGShowGet('M_TRKROLLS',.F.)
  *E500330,1,E500329,1 WAB - (START) If material cost method is !Lot,DISABLE generate manual Rol ID 
   M_GENROLID = 'N'
  =lfOGShowGet('M_GENROLID',.F.)
  *E500330,1,E500329,1 WAB - (END)
ENDIF

*!**************************************************************************
*! Name      : lfOGRShow
*! Developer : Ahmed Mohammed
*! Date      : 11/04/1998
*! Purpose   : Control enabling or disabling objects in the setting grid before 
*!             displaying (in the current module 'MA' ).
*! REF       : *E300930,1 AMM
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : lfOGShowGet()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfOGRShow()
*!**************************************************************************
FUNCTION lfOGRShow

=lfOGShowGet('M_TRKROLLS',M_MATCSTMT = 'L')
*E500330,1,E500329,1 WAB - (START) If Keep Tracking By Roll = Y ENABLE generate manual Rol ID 
=lfOGShowGet('M_GENROLID',M_TRKROLLS = 'Y')
*E500330,1,E500329,1 WAB - (END)

*!**************************************************************************
*! Name      : lfvGENMONU
*! Developer : Hossam El Etreby
*! Date      : 09/16/1999
*! Purpose   : Valid function of 'Manual Material Manufacturing order no.' 
*!             setting in the option grid.
*! REF       : E301234,1
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : gfModalGen()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvGENMONU()
*!**************************************************************************
FUNCTION lfvGENMONU
IF M_GENMONUM = "N"
  *"Please make sure that the generated sequence"+
  *"number will not overlap existing Cutting Ticket numbers ."
  =gfModalGen('INM00288B00000','DIALOG','Manufacturing Order')
ELSE
  *"Please note that you will not be able to "+
  *"assign a number that already exist ."
  =gfModalGen('INM00289B00000','DIALOG')
ENDIF



*!**************************************************************************
*! Name      : lfvGenRolId
*! Developer : WAB - Walid A. Wahab
*! Date      : 08/07/2000
*! Purpose   : Valid function of 'Manual Rolls ID .' 
*!             setting in the option grid.
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : gfModalGen()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvGenRolId()
*!**************************************************************************
*!E500329,1 & E500330,1
*!**************************************************************************
FUNCTION lfvGenRolId
IF M_GENROLID = "N"
  *"Please make sure that the generated sequence"+
  *"number will not overlap existing Rolls ID numbers ."
  =gfModalGen('INM00288B00000','DIALOG','Rolls Id')
ELSE
  *"Please note that you will not be able to "+
  *"assign a number that already exist ."
  =gfModalGen('INM00289B00000','DIALOG')
ENDIF

*!**************************************************************************
*! Name      : lfvGenRolId
*! Developer : WAB - Walid A. Wahab
*! Date      : 08/07/2000
*! Purpose   : Valid function of 'Keep Track of Mat. Rolls .' 
*!             setting in the option grid.
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvGenRolId()
*!**************************************************************************
*!E500329,1 & E500330,1
*!**************************************************************************
FUNCTION lfvTrkRolls
IF M_TRKROLLS = 'Y'
  *--If Keep Track of Mat. Rolls  . Enable the create manual Rol id
  =lfOGShowGet('M_GENROLID',.T.)
ELSE
   M_GENROLID = 'N'
  =lfOGShowGet('M_GENROLID',.F.)
ENDIF



*!**************************************************************************
*! Name      : lfvGenMAPo
*! Developer : WAB - Walid A. Wahab
*! Date      : 08/15/2000
*! Purpose   : Valid function of genereate material po manual 
*!             setting in the option grid.
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvGenMAPo()
*!**************************************************************************
*!*E500349,1 
*!**************************************************************************
FUNCTION lfvGenMAPo
IF M_GENMAPON = "N"
  *"Please make sure that the generated sequence"+
  *"number will not overlap existing Material PO numbers ."
  =gfModalGen('INM00288B00000','DIALOG','Material Purchase Order ')
ELSE
  *"Please note that you will not be able to "+
  *"assign a number that already exist ."
  =gfModalGen('INM00289B00000','DIALOG')
ENDIF

*:*************************************************************
*: Name       : lfvMFGOrdNo
*: Developer  : Abdou Elgendy [ABD]
*: Date       : 03/18/2001
*: Purpose    : Valid function of 'Manual Material Manufacturing 
*:            : Rework order no.' setting in the option grid. 
*:            : (key field)
*: REF        : 200254,1
*:*************************************************************
*: Calls      : 
*:            : Procedures : None
*:            :---------------------------
*:            : FUNCTION   : gfModalGen()
*:*************************************************************
*: Passed Parameters  :  None
*:*************************************************************
*: Returns            :  None
*:*************************************************************
*: Example            :  =lfvGENRWNU()
*:*************************************************************
*:
FUNCTION lfvGENRWNU
IF M_GENRWNUM = "N"
  *- Message Text   :- Please make sure that the generated sequence
  *- Message Text   :- number will not overlap existing rework order numbers .
  *- Message No.    :- 00288.
  *- Buttom Message :- Ok.
  *- Buttom Number  :- 00000.
  =gfModalGen('INM00288B00000','DIALOG','Rework Order')
ELSE
  *- Message Text   :- Please note that you will not be able to
  *- Message Text   :- assign a number that already exist.
  *- Message No.    :- 00289.
  *- Buttom Message :- Ok.
  *- Buttom Number  :- 00000.
  =gfModalGen('INM00289B00000','DIALOG')
ENDIF

*-- End of 
*:*************************************************************
