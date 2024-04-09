*E300296,1 M.H 10/10/95 Get the exchange diffrence account from the GL setup file if
*E300296,1 M.H          the company is linked to the GL module.
*B600922,1 M.H 01/09/96 Check if we are running the apsetup from the AP or SM.
*B601013,1 RENEE 04/08/96 If the company is not linked to a G/L, get the
*B601013,1                exchange difference account from the company setup
*B601013,1                instead of the APSETUP file.
*B601030,1 RENEE 04/08/96 gcPrnt_Cmp, gcComp_Lvl and gcComp_Mdl are not refreshed
*B601030,1                with the selected company's information, if the program
*B601030,1                is called from the SM, company information screen (calling
*B601030,1                APSETUP)
*N42,1      HS  01/29/97  Add the validation function for the new field
*N42,1                    C1099CODE [gfv1099C]
*B601598,1  HS  01/30/97  1)Fix the function [lfglacct]
*B601598,1                2)Add a new parameter to the function [lfglacct]
*B601598,1                 and changed the name of one parameter 
*E300643,1  HS 04/14/97 Make some changes for we have change the file
*E300643,1              SYCCODES name to CODES and make it a data file
*E300643,1              [Change its dir. from SYSFILES to DBFS]
*E300683,6 RENEE 06/04/97 - Call programs from disk instead of applications 
*E300683,6                                  - Add BMP variables
*E300693,1 Hesham El_Sheltawi 06/22/97
*E300693,1 add arrays and variables needed for printing the default printer
*E300693,1 for different screens
*E300692,1 ESSMAT 06/29/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*E300692,5 RENEE Open files from data directory for the parent company
*E300824,1 AMM 03/05/98 Add a new parameter to don't initialize the variables 
*E300824,1 AMM          if the AP application is executed only to run a 
*E300824,1 AMM          function included in it.
*B601993,1 RENEE 03/29/98. Fix the format of an empty exchange difference account
*E300861,1 AHM   05/31/98. Some adjustments for calling APGLREL from System
*E300861,1 				   Manager	
*E300921,1 TAK 07/12/98 Added to not open the APSETUP file in Ap was Not Installed.
*E301098,1 HESHAM 12/16/98 Get company data path using gfGetDataDir(..)
*B602556,1 RENEE 02/17/99 Fix 'File is in use' bug in lfSetGlMsk()
*E301148,1 AMM 02/28/99 Enhance opening and closing files
*E301164,1 Hesham 03/08/99
*E301164,1 declare array to store all temprory files created by any program 
*E301176,1 HDM 03/21/1999 Define new Variables as The Notepad Controled Globally
*C101459,1 Hesham 03/24/99
*C101459,1 define array laUsrFields to hold user defined fields for the program base file
*B603027,1 AKA 07/04/99 Fix the bug of expending the account code in case of bank setup has
*B603027,1              selected as safe.
*E301297,1  HS  07/22/99  Implement the Object/Events triggers, Add a new
*E301297,1                function (gfDoTriger) to handle the Triggers that
*E301297,1                need to be executed when a certain event takes
*E301297,1                place from a certain screen (object).
*E301434,1 Ramy 06/28/2000 Trap the F3 key to clear the current get field 
*E301467,1 TAK  09/17/2000 Add new PS setups and valid function for it, for Atomic integration.
*B604071,1 ABD  01/02/2001 Add new PS setups and valid function for it, For PS Default Location.
*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Task:T20081225.0020]
************************************************************************************
*:Ver 2.0

*E300824,1 AMM add parameter llJumpAll to optionally reassign variable values .
*PARAMETERS lcProgName,lcPrgPram
PARAMETERS lcProgName,lcPrgPram, llJumpAll
*E300824,1 AMM end
*E300692,5 Add private variable
PRIVATE lcPrntDDir
*E300692,5 end

lcPrgPram = IIF(TYPE('lcPrgPram')='C',lcPrgPram,'')
#REGION 1
*E300693,1 Hesham El_Sheltawi (Start)
*DECLARE   laData[1,1]     , laDataCopy[1,1] , laFld_msg[1,2]  , laScrMode[5,1] ,;
          laKeyField[3,1] , laDefProc[10,1] , laFileName[1,2] , laField_H[1,1] ,;
          laField_N[1,1]  , laWndObj[1,3]   , laFiltrExp[1,7] , laFixFltr[1,7] ,;
          laCtrStat[12]   , laWobjects[1,3] , laArrayes[1,2]  , laSubProc[1,2]
*E300824,1 AMM if the AP is called to run a certain function in it, this 
*E300824,1 AMM parameter llJumpAll is true so, initialization won't occurs.
IF !llJumpAll
*E300824,1 AMM end
*E301164,1 Hesham (Start)
*E301164,1 declare array to store all temprory files created by any program 

*  DECLARE   laData[1,1]     , laDataCopy[1,1] , laFld_msg[1,2]  , laScrMode[5,1] ,;
            laKeyField[3,1] , laDefProc[10,1] , laFileName[1,2] , laField_H[1,1] ,;
            laField_N[1,1]  , laWndObj[1,3]   , laFiltrExp[1,7] , laFixFltr[1,7] ,;
            laCtrStat[12]   , laWobjects[1,3] , laArrayes[1,2]  , laSubProc[1,2],;
            laOGFxFlt[1,7],laOGVrFlt[1,7],laRepFltr[1,2]
*-HDM E301176,1 03/21/1999[Start] Changed laCtrStat[12] To laCtrStat[13]
*C101459,1 Hesham (Start)


*  DECLARE   laData[1,1]     , laDataCopy[1,1] , laFld_msg[1,2]  , laScrMode[5,1] ,;
            laKeyField[3,1] , laDefProc[10,1] , laFileName[1,2] , laField_H[1,1] ,;
            laField_N[1,1]  , laWndObj[1,3]   , laFiltrExp[1,7] , laFixFltr[1,7] ,;
            laCtrStat[13]   , laWobjects[1,3] , laArrayes[1,2]  , laSubProc[1,2],;
            laOGFxFlt[1,7],laOGVrFlt[1,7],laRepFltr[1,2],laPrgTemps[1]

  *E301297,1 Change this line to add a new row to the array (laCtrStat) for
  *          the Audit Trail button [Begin]
  *DECLARE   laData[1,1]     , laDataCopy[1,1] , laFld_msg[1,2]  , laScrMode[5,1] ,;
  *          laKeyField[3,1] , laDefProc[10,1] , laFileName[1,2] , laField_H[1,1] ,;
  *          laField_N[1,1]  , laWndObj[1,3]   , laFiltrExp[1,7] , laFixFltr[1,7] ,;
  *          laCtrStat[14]   , laWobjects[1,3] , laArrayes[1,2]  , laSubProc[1,2],;
  *          laOGFxFlt[1,7],laOGVrFlt[1,7],laRepFltr[1,2],laPrgTemps[1],laUsrFields[1,7]
  DECLARE   laData[1,1]     , laDataCopy[1,1] , laFld_msg[1,2]  , laScrMode[5,1] ,;
            laKeyField[3,1] , laDefProc[10,1] , laFileName[1,2] , laField_H[1,1] ,;
            laField_N[1,1]  , laWndObj[1,3]   , laFiltrExp[1,7] , laFixFltr[1,7] ,;
            laCtrStat[15]   , laWobjects[1,3] , laArrayes[1,2]  , laSubProc[1,2] ,;
            laOGFxFlt[1,7]  , laOGVrFlt[1,7]  , laRepFltr[1,2]  , laPrgTemps[1]  ,;
            laUsrFields[1,7]
  
  *E301297,1 Change this line to add a new row to the array (laCtrStat) [End]

*C101459,1 Hesham (End)

*-HDM E301176,1 03/21/1999[End]

*E301164,1 Hesham (End)
 
  *E300693,1 Hesham El_Sheltawi (End)
  STORE .F. TO llAddRec     , llEditRec       , llDeleRec       , llApGlLink,llApS1099
  
  STORE  1  TO lnCurObj     , lnBar_No        , lnDataNo        , pbRight        ,;
               pbLeft       , pbButt
 
  *B601030,1 Add lcCompPrnt to hold the parent company of a selected company
  *E300693,1 Hesham El_Sheltawi (Start)
  *STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N      ,;
               laField_H    , laPrgNames      , laArrayes       , lcWindTitl     ,;
               lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields     ,;
               lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp        ,;
               lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr     ,;
               laWobjects   , lcButtNam       , lcCurr_yer      , lcCurr_prd     ,;
               laSubProc    , lcExDifAcc      , lcCompPrnt 

*E301164,1 Hesham (Start)
*E301164,1 declare array to store all temprory files created by any program 

*  STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N      ,;
               laField_H    , laPrgNames      , laArrayes       , lcWindTitl     ,;
               lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields     ,;
               lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp        ,;
               lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr     ,;
               laWobjects   , lcButtNam       , lcCurr_yer      , lcCurr_prd     ,;
               laSubProc    , lcExDifAcc      , lcCompPrnt ,laOGFxFlt,laOGVrFlt,;
               lcReportID   ,laRepFltr

*-- HDM E301176,1 03/21/1999 [Start]Define new Variables as The Notepad Controled Globally
*  STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N      ,;
               laField_H    , laPrgNames      , laArrayes       , lcWindTitl     ,;
               lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields     ,;
               lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp        ,;
               lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr     ,;
               laWobjects   , lcButtNam       , lcCurr_yer      , lcCurr_prd     ,;
               laSubProc    , lcExDifAcc      , lcCompPrnt ,laOGFxFlt,laOGVrFlt,;
               lcReportID   ,laRepFltr,laPrgTemps,lcMenUnProce


*C101459,1 Hesham (Start)


*  STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N      ,;
               laField_H    , laPrgNames      , laArrayes       , lcWindTitl     ,;
               lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields     ,;
               lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp        ,;
               lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr     ,;
               laWobjects   , lcButtNam       , lcCurr_yer      , lcCurr_prd     ,;
               laSubProc    , lcExDifAcc      , lcCompPrnt ,laOGFxFlt,laOGVrFlt,;
               lcReportID   ,laRepFltr,laPrgTemps,lcMenUnProce,;
               lcSydKey     , lcNoteType      , lcNoteKey

  STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N      ,;
               laField_H    , laPrgNames      , laArrayes       , lcWindTitl     ,;
               lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields     ,;
               lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp        ,;
               lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr     ,;
               laWobjects   , lcButtNam       , lcCurr_yer      , lcCurr_prd     ,;
               laSubProc    , lcExDifAcc      , lcCompPrnt ,laOGFxFlt,laOGVrFlt,;
               lcReportID   ,laRepFltr,laPrgTemps,lcMenUnProce,;
               lcSydKey     , lcNoteType      , lcNoteKey,laUsrFields

*C101459,1 Hesham (End)


*-- HDM E301176,1 03/21/1999 [End]


*E301164,1 Hesham (End)

  *E300693,1 Hesham El_Sheltawi (End)
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

*E300824,1 AMM end of '!llJumpAll'
ENDIF
*E300824,1 AMM end  

*E300697,1 YMA 07/13/97 Define the variables that will hold the 
*E300697,1              GL account mask and length.
lcAcMask   = SPACE(0)         && Account mask
lnAcLen    = 0                && Account Length
lcAcntBrwF = SPACE(0)         && Chart of account browse fields.
lcAcntFld  = SPACE(0)         && Account field that will be validated.
lcAcntDesF = SPACE(0)         && Account Description.
llNoThing  = lfSetGLMsk()
*E300697,1 YMA 07/13/97 End.

*E300921,1 Check if AP module is installed or no.
llApLink   = (OCCURS('AP',gcCmpModules)<>0)
*E300921,1 End.

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

*E300683,6 Add variables holding BMP names for all BMPS use in all AP screens
lcBtMpUp1 = gcBMPHome + "UP1.BMP"
lcBtMpDn1 = gcBMPHome + "DN1.BMP"
lcBtMpPrc = gcBMPHome + "PROCEED.BMP"
lcBtMpNew = gcBMPHome + "NEW.BMP"
lcBtMpRem = gcBMPHome + "REM.BMP"
lcBtMpCan = gcBMPHome + "CAN.BMP"
lcBtMpCls = gcBMPHome + "CLS.BMP"
lcBtMpOK  = gcBMPHome + "OK.BMP"
lcBtMpExt = gcBMPHome + "EXTKEY.BMP"
*E300683,6 end

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
  
*!*************************************************************************
*E300824,1 AMM start, Variable to check if Open APSETUP with myself.
llOpAPS = .F.
*E300824,1 AMM end

*E300921,1 Restore form APSETUP in Ap is installed else variables is .F..
IF llApLink
*E300921,1 End.
  IF !USED("APSETUP")
    *E300824,1 AMM If the file opened with myself make it true.
    llOpAPS = .T.
    *E300824,1 AMM end
    SELECT 0
    USE (gcDataDir+"APSETUP")
  ELSE 
    SELECT APSETUP
  ENDIF
  llApS1099=lApS1099

  *** This line is very important to refresh the station buffer with new 
  *** information from the disk
  llApGlLink = .F.
  GO TOP
  IF !EOF()
    GO 1
    llApGlLink = (CAPSGLLINK = 'Y')
  ENDIF  

ELSE
  llApS1099  =.F.
  llApGlLink =.F.
ENDIF



*B600422,1 Checking to close files opened by the prg.
llUsd_COMP = .T. 
IF !USED("SYCCOMP")
  *B600422,1 Checking to close files opened by the prg.
  llUsd_COMP = .F.
  SELECT 0
  USE (gcSyshome+"SYCCOMP")
ENDIF

*E300692,5 Open files after you get the parent company data directory
*B600422,1 Checking to close files opened by the prg.
*llUsd_FSHD = .T. 
*IF !USED("SYCFISHD")
*  *B600422,1 Checking to close files opened by the prg.
*  llUsd_FSHD = .F. 
*  SELECT 0
*  USE (gcSyshome+"SYCFISHD")
*ENDIF
*SELECT SYCFISHD
*SET ORDER TO COMPFYEAR
*E300692,5 end

*** In case the user uninstall the G/L from the current company 
*** we have to conferm that the link to the G/L is disconnected
SELECT SYCCOMP
SET ORDER TO CCOMP_ID

*B601030,1 Get the company information for gcAct_Comp from the 
*B601030,1 company files, instead of the global variables because
*B601030,1 the global variables are not changed so as to suit a selected
*B601030,1 company, if the AP.PRG is called from SM.PRG, company information
*B601030,1 screen
*llGlLink  = .F.
*IF gcComp_Lvl = "C"
*  IF SEEK(gcPrnt_Cmp,"SYCCOMP")
*    llGlLink = IIF("GL" $ SYCCOMP.mComp_Mdl,.T.,.F.)
*  ENDIF
*ELSE
*  llGlLink   = IIF("GL" $ gcComp_Mdl,.T.,.F.)
*ENDIF
*lcCurr_yer = IIF(SEEK(gcPrnt_Cmp,'sycComp'),sycComp.cCurr_Yer,'')
SEEK(gcAct_Comp)
IF !EMPTY(SYCCOMP.cCompPrnt)
  lcCompPrnt = SYCCOMP.cCompPrnt
  =SEEK(lcCompPrnt,"SYCCOMP")
ELSE
  lcCompPrnt = SYCCOMP.cComp_ID
ENDIF  
*E300692,5 Get the data path of the parent company
*E301098,1 Hesham (Start)
*lcPrntDDir =  ALLTRIM(SYCCOMP.cCom_DDir)
lcPrntDDir =  gfGetDataDir(ALLTRIM(SYCCOMP.cCom_DDir))
*E301098,1 Hesham (End)
*E300692,5 end

llGlLink   = "GL" $ SYCCOMP.mComp_Mdl
lcCurr_yer = SYCCOMP.cCurr_Yer
*B601030,1 end.

lnCurr_yer = INT(VAL(lcCurr_yer))
lcCurr_prd = SYCCOMP.cCurr_prd
lnCurr_prd = INT(VAL(SYCCOMP.cCurr_prd))

*B601030,1 Reposition the file pointer on the active company
=SEEK(gcAct_Comp)
*B601030,1 end.

*E300692,5 Open FISHD file from the parent company data directory
llUsd_FSHD = .T. 
IF !USED('FISHD')
  llUsd_FSHD = .F. 
  SELECT 0
  USE (lcPrntDDir+'FISHD') ORDER TAG COMPFYEAR
ELSE
  SELECT FISHD
  SET ORDER TO COMPFYEAR
ENDIF
*E300692,5 end

*B601030,1 Use lcCompPrnt instead of gcPrnt_Cmp
*ldFisBgDat = IIF(SEEK(gcPrnt_Cmp+lcCurr_yer,'sycFisHd'),sycFisHd.dFisBgDat,{})
*E300692,1 CHANGE FILE NAME FROM SYCFISHD TO FISHD
*ldFisBgDat = IIF(SEEK(lcCompPrnt+lcCurr_yer,'sycFisHd'),sycFisHd.dFisBgDat,{})
*B601030,1 end.
*ldFisEnDat = sycFisHd.dFisEnDat
*man
*ldFisBgDat = IIF(SEEK(lcCompPrnt+lcCurr_yer,'FisHd'),FisHd.dFisBgDat,{})
ldFisBgDat = IIF(SEEK(lcCurr_yer,'FisHd'),FisHd.dFisBgDat,{})
ldFisEnDat = FisHd.dFisEnDat
*E300692,1 end
SKIP -1
*E300692,1 CHANGE FILE NAME FROM SYCFISHD TO FISHD
*ldPyBgDate = sycFisHd.dfisbgdat
ldPyBgDate = FisHd.dfisbgdat
*E300692,1 end
SKIP 2
*E300692,1 CHANGE FILE NAME FROM SYCFISHD TO FISHD
*ldNyEnDate = sycFisHd.dFisEnDat
ldNyEnDate = FisHd.dFisEnDat
*E300692,1 end

*E300921,1 Update the Ap setup if Ap is Installed.
IF !llGlLink AND llApLink
  REPLACE APSETUP.cApsgllink WITH "N" ;
          APSETUP.cApsglcom  WITH "  "
ENDIF
*E300921,1 End.

******************************************************************************
*B600422,1 Checking to close files opened by the prg.
llUsd_ACOD = .T. 

*E300692,1 change file name from SYCACCOD to ACCOD
*IF !USED("SYCACCOD")
IF !USED("ACCOD")
*E300692,1 end
  *B600422,1 Checking to close files opened by the prg.
  llUsd_ACOD = .F. 
  SELECT 0
  *E300692,1 change file name and path from SYCACCOD to ACCOD
  *USE (gcSysHome+"SYCACCOD") 
 
  *E300861,1 Add AGAIN in USE Command because the file is already opened
  *E300861,1 WITH another alias name from the calling program SMGLREL
  *E300861,1 Start
  *USE (lcPrntDDir+"ACCOD") ORDER TAG ACCSEGNO
  USE (lcPrntDDir+"ACCOD") AGAIN ORDER TAG ACCSEGNO
  *E300861,1 end
  *E300692,1 end
ELSE
  SELECT ACCOD
  SET ORDER TO TAG ACCSEGNO
ENDIF  
*E300692,1 end

*E300692,1 change file name from SYCACCOD to ACCOD
*SELECT SYCACCOD
*SET ORDER TO TAG COMPID 
*=SEEK(gcAct_Comp) 
*E300692,1 end


*B603027,1 AKA (Start)
*=SEEK(lcCompPrnt) 
GO TOP
*B603027,1 AKA (End)



*E300692,1 CHANGE FILE NAME FROM SYCACCOD TO ACCOD
*lcApsAcMas = SYCACCOD.cAcsMask
lcApsAcMas = ACCOD.cAcsMask
*E300692,1 end
*E300921,1 Define variable depends on variable flag not on ApSetup.
*lcApsAcMas = STRTRAN(lcApsAcMas,'#',IIF(APSETUP.cApsgllink='Y','9','X'))
lcApsAcMas = STRTRAN(lcApsAcMas,'#',IIF(llApGlLink,'9','X'))
*E300921,1 End.
lcApsAcMas = ALLTRIM("X"+SUBSTR(lcApsAcMas,2))
lnApsAcLen = LEN(ALLTRIM(lcApsAcMas))
lcEmptyAcc = REPLICATE('0',lnApsAcLen)

*E300296,1 M.H 10/10/95 Get the exchange diffrence account from the GL setup file if
*E300296,1 M.H          the company is linked to the GL module.

llUsd_GLSTP = .T.
*B600922,1 M.H 01/09/96 Check if we are running the apsetup from the AP or SM.
*IF llGlLink
*SUBSTR(gcBaseWind,4,15) <> 'SMCMINF'
IF llGlLink .AND. !llNewSetup 
*B600922,1 M.H End.
  IF !USED("GLSETUP")
    llUsd_GLSTP = .F.
    SELECT 0
    *E300861,1 Add AGAIN in USE Command because the file is already opened
    *E300861,1 WITH another alias name from the calling program SMGLREL
    *E300861,1 Start
    *USE (gcDataDir+"GLSETUP")
    USE (gcDataDir+"GLSETUP")  AGAIN
    *E300861,1 End
  ENDIF
  SELECT GLSETUP
  GO TOP
ENDIF

*B601013,1 IF the company is not linked to a GL, get the exchange difference
*B601013,1 account from the Company setting. 
*lcExDifAcc = IIF(llApGlLink,GLSETUP.CSETEXMJ,APSETUP.CEXDIFACC)
*lcExDifAcc = IIF(llApGlLink,GLSETUP.CSETEXMJ, gfGetMemVar('LNEXRATACC', gcAct_Comp))
*B601993,1 Initialize with an empty account if the G/L exchange difference account
*B601993,1 is empty
*lcExDifAcc  = IIF(llApGlLink, ALLTRIM(GLSETUP.CSETEXMJ) + ;
                              STRTRAN(SUBSTR(lcApsAcMas,AT("-",lcApsAcMas)),"9","0"),;
                              gfGetMemVar('LNEXRATACC', gcAct_Comp))
lcExDifAcc  = IIF(llApGlLink, IIF(EMPTY(GLSETUP.CSETEXMJ), lcEmptyAcc,;
							  ALLTRIM(GLSETUP.CSETEXMJ) + ;
                              STRTRAN(SUBSTR(lcApsAcMas,AT("-",lcApsAcMas)),"9","0")),;
                              gfGetMemVar('LNEXRATACC', gcAct_Comp))
*B601993,1 end
*B601013,1 end.
*E300296,1 M.H End.

*!*************************************************************************

=lfOpenCh()

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

*E300683,6 Check if the file exists on disk.
lcPrgTmp = IIF(ATC(' ',lcProgName)>0,SUBSTR(lcProgName,1,ATC(' ',lcProgName)-1),lcProgName)
gcDosApps = gcAppHome+gcWinAppl+'\'
*gcDosApps = gcAppHome+'PS\'

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
*E300683,6 end

*B600422,1 Checking to close files opened by the prg.
IF !llUsd_COMP AND USED("SYCCOMP")
  USE IN SYCCOMP
ENDIF
*B600422,1 Checking to close files opened by the prg.
*E300692,1 CHANGE FILE NAME FROM SYCFISHD TO FISHD
*IF !llUsd_FSHD AND USED("SYCFISHD")
  *USE IN sycFISHD
IF !llUsd_FSHD AND USED("FISHD")
  USE IN FISHD
*E300692,1 end
ENDIF
*B600422,1 Checking to close files opened by the prg.
*E300692,1 CHANGE FILE NAME FROM SYCFISHD TO FISHD
*IF !llUsd_ACOD AND USED("SYCACCOD")
*  USE IN SYCACCOD
IF !llUsd_ACOD AND USED("ACCOD")  
  USE IN ACCOD
*E300692,1 end
ENDIF
*E300824,1 AMM close the file if Opened by me i.e. llOpAPS = .T.  .
*IF USED("APSETUP")
IF llOpAPS AND USED("APSETUP")
*E300824,1 AMM end
  USE IN APSETUP
ENDIF

*E300824,1 AMM IF the AP is called to run a certain function in it, don't 
*E300824,1 AMM close it here but the calling program will close it. 
*IF USED('lcLinkChar')
IF !llJumpAll AND USED('lcLinkChar')
*E300824,1 AMM end
  USE IN ('lcLinkChar')
ENDIF

IF llUsd_GLSTP .AND. USED("GLSETUP")
  USE IN GLSETUP
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

*********************************************************************


*!**************************************************************************
*!
*!      Function gfSetCode
*!
*!**************************************************************************
*
FUNCTION gfSetCode

PARAMETERS lcFld_Name,laDiscrip,laCodes
PRIVATE lcFld_Name,laDiscrip,laCodes
DECLARE laDiscrip[1,1],laCodes[1,1]

STORE " " TO laDiscrip,laCodes

lcFld_Name = ALLTRIM(UPPER(lcFld_Name))

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT CDISCREP;
*  FROM (gcSysHome+"SYCCODES");
* WHERE CCOMP_ID = gcAct_Comp;
* .AND. cfld_name = 'CDIVISION';
* ORDER BY CDISCREP;
*  INTO ARRAY laDiscrip

SELECT CDISCREP;
  FROM (gcDataDir+"CODES");
 WHERE CCOMP_ID = gcAct_Comp;
 .AND. cfld_name = 'CDIVISION';
 ORDER BY CDISCREP;
  INTO ARRAY laDiscrip

*E300643,1 Change this line for the changes we have made to SYCCODES [End]

*E300643,1 Change this line for the changes we have made to SYCCODES [Begin]
*SELECT CCODE_NO;
*  FROM (gcSysHome+"SYCCODES");
* WHERE CCOMP_ID = gcAct_Comp;
* .AND. cfld_name = 'CDIVISION';
* ORDER BY CDISCREP;
*  INTO ARRAY laCodes

SELECT CCODE_NO;
  FROM (gcDataDir+"CODES");
 WHERE CCOMP_ID = gcAct_Comp;
 .AND. cfld_name = 'CDIVISION';
 ORDER BY CDISCREP;
  INTO ARRAY laCodes

*E300643,1 Change this line for the changes we have made to SYCCODES [End]


FUNCTION lfVldSets
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
      *E301098,1 Hesham (Start)
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
      *E301148,1 AMM Set the flag with .T. to close file when exiting
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
IF llOpAcCh 
  USE IN lcLinkChar
ENDIF
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



*!**************************************************************************
*! Name      : lfOGRShow
*! Developer : Ahmed Mohammed
*! Date      : 02/12/1998
*! Purpose   : Control enabling or disabling objects in the setting grid before 
*!             displaying (in the current module 'IC' ).
*! REF       : *B601911,1
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

*E301467,1 Added Start.
=lfvAtomInt()
*E301467,1 End.

*-abdou
*B604071,1 ABD Disable the Def location if single warehouse setting is 'Y' [Begin]
llShowDfLc = (gfGetMemVar('M_WareHouse') = 'Y' .AND. OCCURS('NC',gcCmpModules)<>1)
=lfOGShowGet('M_PSDEFLOC',llShowDfLc)
*B604071,1 ABD

*E301467,1 Added Start.
*!*************************************************************
*! Name      : lfvAtomInt
*! Developer : Timour Abdalla
*! Date      : 09/17/00
*! Purpose   : Called from SMCMSET.PRG, Checks for the validity
*!             of the entered values in the setups grid
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : gfModalGen(),lfOGShowGet()
*!*************************************************************
FUNCTION lfvAtomInt

IF M_USEATOM = 'N' 
 M_ATOMDIR  = ' '
 M_ENTMETH  = 'M'
 M_ACTNTYPE = 'O'
 * disable the remainder items when deleting the link   
 =lfOGShowGet('M_ATOMDIR',.F.)
 =lfOGShowGet('M_ENTMETH',.F.)
 =lfOGShowGet('M_ACTNTYPE',.F.)
ELSE
 =lfOGShowGet('M_ATOMDIR',.T.)
 =lfOGShowGet('M_ENTMETH',.T.)
 =lfOGShowGet('M_ACTNTYPE',.T.)
ENDIF
=lfOGShowGet('M_USEATOM',.T.)
RETURN

*!*************************************************************
*! Name      : lfvAtomDir
*! Developer : Timour Abdalla
*! Date      : 09/17/00
*! Purpose   : Called from SMCMSET.PRG, Checks for the validity
*!             of the entered values in the setups grid
*!*************************************************************
FUNCTION lfvAtomDir

IF !EMPTY(M_ATOMDIR) AND FILE(M_ATOMDIR+'ATOM32.EXE')
  RETURN
ENDIF

M_ATOMDIR = GETDIR('','Atomic Software Directory')
IF EMPTY(M_ATOMDIR) OR ! FILE(M_ATOMDIR+'ATOM32.EXE')
  M_USEATOM = 'N'
  =lfvAtomInt()
ELSE
  M_ATOMDIR = ALLT(M_ATOMDIR)
ENDIF
RETURN
*E301467,1 End.
*:*************************************************************
*! Name     : lfvDefLoc
*! Developer: Abdou El-Gendy
*! Date     : 01/02/2001               
*! Purpose  : Vadidate the Def. warehouse in PS setup.
*! Refere   : B#604071.
*:*************************************************************
*! Calls     : 
*!           : Procedures : ....
*!           ; Functions  : gfOpenFile() , gfBrowWare() 
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvDefLoc()
*!*************************************************************

FUNCTION lfvDefLoc

IF !EMPTY(M_PSDEFLOC)
  lnAlias = SELECT()
  = gfOpenFile(gcDataDir+"WAREHOUS","WAREHOUS","SH")
  IF !SEEK(M_PSDEFLOC,'WAREHOUS')
    M_PSDEFLOC = gfBrowWare( .T. )
  ENDIF
  SELECT(lnAlias)
  SHOW GET M_PSDEFLOC
ENDIF
*-- End OF lfvDefLoc
*!*************************************************************