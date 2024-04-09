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
*E300930,1 AMM 11/04/98 Add some related fields validations ()
*E301098,1 HESHAM 12/16/98 Get company data path using gfGetDataDir(..)
*E301148,1 AMM 02/28/99 Enhance opening and closing files
*E300789,4  AMM 03/07/99 Remove field cComp_id from files Accod, FisHD, FsPrd, FsHld, Codes
*E301164,1 Hesham 03/08/99
*E301164,1 declare array to store all temprory files created by any program 
*E301077,80 IHB 03/10/1999 Enhance opening and closing files (GLSETUP)
*B602682,1 AMM 03/17/99 Fill related field "CADJACCT" with correct data.
*E301176,1 HDM 03/21/1999 Define new Variables as The Notepad Controled Globally
*C101459,1 Hesham 03/24/99
*C101459,1 define array laUsrFields to hold user defined fields for the program base file
*E301297,1  HS  07/22/99  Implement the Object/Events triggers, Add a new
*E301297,1                function (gfDoTriger) to handle the Triggers that
*E301297,1                need to be executed when a certain event takes
*E301297,1                place from a certain screen (object).
*E301434,1 Ramy 06/28/2000 Trap the F3 key to clear the current get field
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
            laCtrStat[15]   , laWobjects[1,3] , laArrayes[1,2]  , laSubProc[1,2],;
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
*C101459,1 Hesham (Start)

*  STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N      ,;
               laField_H    , laPrgNames      , laArrayes       , lcWindTitl     ,;
               lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields     ,;
               lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp        ,;
               lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr     ,;
               laWobjects   , lcButtNam       , lcCurr_yer      , lcCurr_prd     ,;
               laSubProc    , lcExDifAcc      , lcCompPrnt ,laOGFxFlt,laOGVrFlt,;
               lcReportID   ,laRepFltr

  STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N      ,;
               laField_H    , laPrgNames      , laArrayes       , lcWindTitl     ,;
               lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields     ,;
               lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp        ,;
               lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr     ,;
               laWobjects   , lcButtNam       , lcCurr_yer      , lcCurr_prd     ,;
               laSubProc    , lcExDifAcc      , lcCompPrnt ,laOGFxFlt,laOGVrFlt,;
               lcReportID   ,laRepFltr,laUsrFields
*C101459,1 Hesham (End)



*-- HDM E301176,1 03/21/1999 [Start]Define new Variables as The Notepad Controled Globally

*  STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N      ,;
               laField_H    , laPrgNames      , laArrayes       , lcWindTitl     ,;
               lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields     ,;
               lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp        ,;
               lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr     ,;
               laWobjects   , lcButtNam       , lcCurr_yer      , lcCurr_prd     ,;
               laSubProc    , lcExDifAcc      , lcCompPrnt ,laOGFxFlt,laOGVrFlt

  STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N      ,;
               laField_H    , laPrgNames      , laArrayes       , lcWindTitl     ,;
               lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields     ,;
               lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp        ,;
               lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr     ,;
               laWobjects   , lcButtNam       , lcCurr_yer      , lcCurr_prd     ,;
               laSubProc    , lcExDifAcc      , lcCompPrnt , laOGFxFlt ,laOGVrFlt  ,;
               lcReportID   , laRepFltr       , laPrgTemps , lcMenUnProce ,;
               lcSydKey     , lcNoteType      , lcNoteKey

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
GO TOP
IF !EOF()
  GO 1
  llApGlLink = (CAPSGLLINK = 'Y')
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
*E300789,4  AMM Adjust to fit the new structure
*ldFisBgDat = IIF(SEEK(lcCompPrnt+lcCurr_yer,'FisHd'),FisHd.dFisBgDat,{})
ldFisBgDat = IIF(SEEK(lcCurr_yer,'FisHd'),FisHd.dFisBgDat,{})
*E300789,4  AMM end
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

IF !llGlLink
  REPLACE APSETUP.cApsgllink WITH "N" ;
          APSETUP.cApsglcom  WITH "  "
ENDIF

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
*E300789,4  AMM Adjust to fit the new structure
*=SEEK(lcCompPrnt) 
GO TOP
*E300789,4  AMM end

*E300692,1 CHANGE FILE NAME FROM SYCACCOD TO ACCOD
*lcApsAcMas = SYCACCOD.cAcsMask
lcApsAcMas = ACCOD.cAcsMask
*E300692,1 end
lcApsAcMas = STRTRAN(lcApsAcMas,'#',IIF(APSETUP.cApsgllink='Y','9','X'))
lcApsAcMas = ALLTRIM("X"+SUBSTR(lcApsAcMas,2))
lnApsAcLen = LEN(ALLTRIM(lcApsAcMas))
lcEmptyAcc = REPLICATE('0',lnApsAcLen)

*E300296,1 M.H 10/10/95 Get the exchange diffrence account from the GL setup file if
*E300296,1 M.H          the company is linked to the GL module.

*E301077,80 IHB Enhance opening and closing files (GLSETUP) [start]
*-- normally the file is not open unless it is actually used 
STORE .F. TO llUsd_GLSTP
*llUsd_GLSTP = .T.
*E301077,80 IHB [end]

*B600922,1 M.H 01/09/96 Check if we are running the apsetup from the AP or SM.
*IF llGlLink
*SUBSTR(gcBaseWind,4,15) <> 'SMCMINF'
IF llGlLink .AND. !llNewSetup 
*B600922,1 M.H End.

  *E301077,80 IHB Enhance opening and closing files (GLSETUP) [start]
  *IF !USED("GLSETUP")
  *  llUsd_GLSTP = .F.
  *  SELECT 0
  *  *E300861,1 Add AGAIN in USE Command because the file is already opened
  *  *E300861,1 WITH another alias name from the calling program SMGLREL
  *  *E300861,1 Start
  *  *USE (gcDataDir+"GLSETUP")
  *  USE (gcDataDir+"GLSETUP")  AGAIN
  *  *E300861,1 End
  *ENDIF
  llUsd_GLSTP = gfOpenFile(gcDataDir+'GLSETUP' ,' ','SH')
  *E301077,80 IHB [end]

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
*gcDosApps = gcAppHome+gcWinAppl+'\'
gcDosApps = gcAppHome+'AP\'

*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Start]
IF GCMULTIINST
  gcCDosApps = gcCappHome+'AP\'
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
IF USED("APSETUP") .AND. llOpAPS
*E300824,1 AMM end
  USE IN APSETUP
ENDIF

*E300824,1 AMM IF the AP is called to run a certain function in it, don't 
*E300824,1 AMM close it here but the calling program will close it. 
*IF USED('lcLinkChar')
IF USED('lcLinkChar') .AND. !llJumpAll
*E300824,1 AMM end
  USE IN ('lcLinkChar')
ENDIF

*E301077,80 IHB Enhance opening and closing files (GLSETUP) [start]
*IF llUsd_GLSTP .AND. USED("GLSETUP")
*  USE IN GLSETUP
*ENDIF
IF USED("GLSETUP") .AND. llUsd_GLSTP
  *=gfCloseFile('GLSETUP')
  USE IN GLSETUP
ENDIF
*E301077,80 IHB [end]

*E301434,1 Ramy [start]
*ON KEY LABEL ESC 
ON KEY LABEL ESC &lcESC
ON KEY LABEL F3 &lcF3Key
*E301434,1 Ramy [end]

SET FULLPATH OFF
#REGION 1

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
*!      Function gfSetCode
*!
*!**************************************************************************
*
FUNCTION gfSetCode

PARAMETERS lcFld_Name,laDiscrip,laCodes
PRIVATE lcFld_Name,laDiscrip,laCodes
DECLARE laDiscrip[1,1],laCodes[1,1]

STORE " " TO laDiscrip,laCodes
*E301148,1 AMM Open the codes file 
llOpCode = gfOpenFile(gcDataDir+'CODES','','SH')
*E301148,1 AMM end

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
*E301148,1 AMM Close file 
IF USED('CODES') .AND. llOpCode
  =gfCloseFile('CODES')
ENDIF
*E301148,1 AMM end

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
*!      Function : lfGLAcct
*!
*!**************************************************************************
*
FUNCTION lfGLAcct

*B601598,1 Change this line to add the new parameter to the statment 
*          and change the name of an old parameter [Begin] 
*PARAMETERS lcCurVal,lcVarName,lcCompanyId
PARAMETERS lcCurVal , lcLgVarNam , lcCompanyId , lcOldVal
*B601598,1 Change this line [End]

lcSavSelct  = ALIAS()   && Variable to save the currently selected file.

STORE .F. TO llOpenComp, llOpenACod, llOpenAcHar
IF !USED("SYCCOMP")
  USE (gcSyshome+"SYCCOMP") IN 0
  llOpenComp = .T.
ENDIF

SELECT SYCCOMP
SET ORDER TO TAG CCOMP_ID
=SEEK(lcCompanyId) 
*E301098,1 Hesham (Start)
*USE (ALLTRIM(SYCCOMP.cCom_dDir)+"APSETUP") AGAIN ALIAS lcSetUpAls IN 0
USE (gfGetDataDir(ALLTRIM(SYCCOMP.cCom_dDir))+"APSETUP") AGAIN ALIAS lcSetUpAls IN 0
*E301098,1 Hesham (End)
*E300692,1 CHANGE FILE NAME AND PATH FROM SYCACCOD TO ACCOD
*IF !USED("SYCACCOD")
  *USE (gcSyshome+"SYCACCOD") IN 0
IF !USED("ACCOD")  
  *E300861,1 Add AGAIN in USE Command because the file is already opened
  *E300861,1 WITH another alias name from the calling program SMGLREL
  *E300861,1 Start
  *USE (gcDataDir+"ACCOD") IN 0
  USE (gcDataDir+"ACCOD") IN 0 AGAIN
  *E300861,1 End
*E300692,1 end
  llOpenACod = .T.
ENDIF

*E300692,1 CHANGE FILE NAME FROM SYCACCOD TO ACCOD
*SELECT SYCACCOD
SELECT ACCOD
*E300692,1 end
*E300789,4  AMM Adjust to fit the new indexes
*SET ORDER TO TAG COMPID 
*=SEEK(lcCompanyId) 
SET ORDER TO TAG Accsegno
GO TOP
*E300789,4  AMM end

llApGlLink = lcSetUpAls.CAPSGLLINK = 'Y'
*E300692,1 CHANGE FILE NAME FROM SYCACCOD TO ACCOD
*lcApsAcMas = SYCACCOD.cAcsMask
lcApsAcMas = ACCOD.cAcsMask
*E300692,1 end
lcApsAcMas = STRTRAN(lcApsAcMas,'#',IIF(lcSetUpAls.cApsgllink='Y','9','X'))
lcApsAcMas = ALLTRIM("X"+SUBSTR(lcApsAcMas,2))
lnApsAcLen = LEN(ALLTRIM(lcApsAcMas))

IF lcSetUpAls.cApSglLink = 'Y'
  lcDataDir = IIF(EMPTY(SYCCOMP.cCompPrnt)              , ;
              ALLTRIM(SYCCOMP.cCom_dDir)+"GLACCHAR" , ;
              ALLTRIM(LOOKUP(SYCCOMP.CCOM_DDIR,lcSetUpAls.cApSgLCom,SYCCOMP.CCOMP_ID,"CCOMP_ID")))
    *E301098,1 Hesham (Start)
    lcDataDir = gfGetDataDir(lcDataDir)
    *E301098,1 Hesham (End)            
  IF FILE(lcDataDir+'GLACCHAR.DBF')
    SELECT 0  
    USE &lcDataDir.GLACCHAR AGAIN ALIAS lcLinkChar
    SET ORDER TO ACCTCODE
    llOpenAcHar = .T.
  ENDIF
ENDIF 

lcExAcDes  = ''

*B601598,1 Change this line [Begin]
*&lcVarName = lfApAcs(@lcExAcDes,.F.)
&lcLgVarNam = lfApAcs(@lcExAcDes,.F.)
*B601598,1 Change this line [End]

*B601598,1 Add this lines [Begin]
*B601598,1 IF Stament to check if the user did not fill the field or selected cansel
*B601598,1 selected cansel from the brwose          
IF EMPTY(&lcCurVal) .OR. !EVALUATE(lcLgVarNam)
  *B601598,1 IF Stament to check if the old value of the field was empty 
  IF EMPTY(lcOldVal)
    &lcLgVarNam = .F.

    ** MESSAGE : " You have to enter the ð              "
    **           "                  ® Ok ¯              "
    **
    =gfModalGen("TRM04066B00000" , "DIALOG" , "account")
  ELSE     && Else
    &lcCurVal = lcOldVal
    &lcLgVarNam = .T.
    _CUROBJ = _CUROBJ + 1
  ENDIF     && End of IF  
ELSE     && Else
  &lcLgVarNam = .T.
  _CUROBJ = _CUROBJ + 1
ENDIF     && End of IF    
*B601598,1 Add this lines [End]

IF llOpenComp .AND. USED('SYCCOMP')
  USE IN ('SYCCOMP')
ENDIF

*E300692,1 CHANGE FILE NAME FROM SYCACCOD TO ACCOD
*IF llOpenACod .AND. USED('SYCACCOD')
  *USE IN ('SYCACCOD')
IF llOpenACod .AND. USED('ACCOD')  
  USE IN ('ACCOD')
  *E300692,1 end
ENDIF

IF llOpenAcHar .AND. USED('lcLinkChar')
  USE IN ('lcLinkChar')    
ENDIF

IF USED('lcSetUpAls')
  USE IN ('lcSetUpAls')
ENDIF

SELECT IIF(EMPTY(lcSavSelct),0,lcSavSelct)

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
*! Name      : lfv1099C                              
*! Developer : Haytham El_Sheltawi
*! Date      : 01/29/1997
*! Purpose   : Valid function for the field [c1099code] 
*!*************************************************************
*! Calls     : gfModalGen()
*!*************************************************************
*! Called From  : Codes screen [System maneger]
*!*************************************************************
*! Passed Parameters  : lcCurVal , lcLgVarNam , lcCompanyId , lcOldVal
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfv1099C()
*!*************************************************************
*N42,1  HS  01/29/97 This function was added by HS for the new devlopment
*!**************************************************************************
*
FUNCTION lfv1099C

PARAMETERS lcCurVal , lcLgVarNam , lcCompanyId , lcOldVal

*IF Statment to check if the field was left empty and ther is  old value 
IF EMPTY (&lcCurVal) .AND. !EMPTY(lcOldVal)

  ** MESSAGE : " You have to enter the ð              "
  **           "                  ® Ok ¯              "
  **
  =gfModalGen("TRM04066B00000" , "DIALOG" , "1099 code  .  Valid range is 01 - 12")
  &lcCurVal = lcOldVal
  &lcLgVarNam = .T.
ELSE       && Else
  *IF Statment to check if the field value is between 1 and 12
  IF BETWEEN(VAL(&lcCurVal) , 1 , 11)
    &lcCurVal = PADL(ALLTRIM(STR(VAL(&lcCurVal))) , 2 , '0') 
    &lcLgVarNam = .T.
  ELSE      && Else

    ** MESSAGE : " You have to enter the ð              "
    **           "                  ® Ok ¯              "
    **
    =gfModalGen("TRM04066B00000" , "DIALOG" , "1099 code  .  Valid range is 01 - 12")
    &lcLgVarNam = IIF(EMPTY(lcOldVal) , .F. , .T.)
    &lcCurVal = lcOldVal
  ENDIF     && End of IF
ENDIF     && End of IF

*!**************************************************************************
*! Name      : lfvRelBnk
*! Developer : Ahmed Mohammed
*! Date      : 11/04/1998
*! Purpose   : Valid function of the Bank Code
*! REF       : *E300930,1 AMM
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : gfOpenFile(), gfBrows()
*!**************************************************************************
*! Parameters: lcCurVal , lcRet , lcCompID, lcOldRltd
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvRelBnk()
*!**************************************************************************
FUNCTION lfvRelBnk
PARAMETER lcVar , lcReturn , lcComp , lcOldRltd


STORE SPACE(0) TO lcOldBnk
lnPos = ASUBSCRIPT(laRelFld,ASCAN(laRelFld,'CCHKACCT',1),1 )
IF EMPTY(EVAL(lcVar))
  laRelFld[lnPos,6] = SPACE(12)
  RETURN
ENDIF
DIMENSION laTemp[1]
IF !USED('APBANKS')
  =gfOpenFile(ALLTRIM(laComp[lnComp,3])+'APBANKS','BankCode','SH')
ELSE
  IF DBF('APBANKS') # UPPER(ALLTRIM(laComp[lnComp,3])+'APBANKS.DBF')
    lcOldBnk = DBF('APBANKS')
    USE IN APBANKS
    =gfOpenFile(ALLTRIM(laComp[lnComp,3])+'APBANKS','BankCode','SH')
  ENDIF
ENDIF
SELECT APBANKS
IF !SEEK(EVAL(lcVar),'APBANKS')
  lcBrFields = "cBnkCode :H='Bank code',cBnklndes :H='Long description' "
  =gfBrows("FOR cBnkType = 'B' ",'cBnkCode','laTemp')
  IF !EMPTY(laTemp[1])
    &lcVar = laTemp[1]
  ELSE 
    &lcVar = SPACE(8)
  ENDIF
  *E300930,1 AMM Initialize the checking account
  laRelFld[lnPos,6] = SPACE(12)
  SHOW GET laRelFld[lnPos,6]
ENDIF


IF !EMPTY(lcOldBnk)
  USE IN APBANKS
  =gfOpenFile(lcOldBnk,'','SH')
ENDIF

*!**************************************************************************
*! Name      : lfvRelchk
*! Developer : Ahmed Mohammed
*! Date      : 11/04/1998
*! Purpose   : Valid function of the Bank Checking account
*! REF       : *E300930,1 AMM
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : gfOpenFile(), gfBrows()
*!**************************************************************************
*! Parameters: lcCurVal , lcRet , lcCompID, lcOldRltd
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvRelchk()
*!**************************************************************************
FUNCTION lfvRelchk
PARAMETER lcVar , lcReturn , lcComp , lcOldRltd
STORE SPACE(0) TO lcOldBnk,lcOldchk
DIMENSION laTemp[1]
IF !USED('APCHECKS')
  =gfOpenFile(ALLTRIM(laComp[lnComp,3])+'APCHECKS','BankCheck','SH')
ELSE
  IF DBF('APCHECKS') # UPPER(ALLTRIM(laComp[lnComp,3])+'APCHECKS.DBF')
    lcOldchk = DBF('APCHECKS')
    USE IN APBANKS
    =gfOpenFile(ALLTRIM(laComp[lnComp,3])+'APCHECKS','BankCheck','SH')
  ENDIF
ENDIF

IF !USED('APBANKS')
  =gfOpenFile(ALLTRIM(laComp[lnComp,3])+'APBANKS','BankCode','SH')
ELSE
  IF DBF('APBANKS') # UPPER(ALLTRIM(laComp[lnComp,3])+'APBANKS.DBF')
    lcOldBnk = DBF('APBANKS')
    USE IN APBANKS
    =gfOpenFile(ALLTRIM(laComp[lnComp,3])+'APBANKS','BankCode','SH')
  ENDIF
ENDIF
SELECT APCHECKS
lnPos  = ASUBSCRIPT(laRelFld,ASCAN(laRelFld,'CBNKCODE',1),1 )
lnPos1 = ASUBSCRIPT(laRelFld,ASCAN(laRelFld,'CADJACCT',1),1 )
lcBrFields = "cBnkCode :H='Bank code',cChkAcct :H='Bank checking account',"+;
             "cChkShDes :H='Short description', cChkGlAcc :H='GL checking account' "

IF EMPTY(laRelFld[lnPos,6])
  *E300930,1 AMM If bank code field is empty, browse all checking accounts
  *B602682,1 AMM browse and Get the GL account
  *=gfBrows(.F.,'cChkAcct,CBnkCode','laTemp')
  =gfBrows(.F.,'cChkAcct,CBnkCode,cChkGlAcc','laTemp')
  *B602682,1 AMM end
  
  IF !EMPTY(laTemp[1])
    &lcVar = laTemp[1]
    laRelFld[lnPos,6] = laTemp[2]
    *B602682,1 AMM Fill the GL account
    laRelFld[lnPos1,6] = laTemp[3]
    *B602682,1 AMM end
  ELSE
    &lcVar = SPACE(12)
    *B602682,1 AMM Empty the GL account
    laRelFld[lnPos1,6] = SPACE(12)
    *B602682,1 AMM end
  ENDIF
  SHOW GET laRelFld[lnPos,6]
ELSE
  *E300930,1 AMM Browse all checking account for the chosen bank code
  IF !SEEK(PADR(laRelFld[lnPos,6],8)+EVAL(lcVar),'APCHECKS')
    *B602682,1 AMM Browse and get the GL account
    *=gfBrows("FOR cBnkCode = "+"'"+laRelFld[lnPos,6]+"'",'cChkAcct','laTemp')  
    =gfBrows("FOR cBnkCode = "+"'"+laRelFld[lnPos,6]+"'",'cChkAcct,cChkGlAcc','laTemp')
    *B602682,1 AMM end
    IF !EMPTY(laTemp[1])
      &lcVar = laTemp[1]
      *B602682,1 AMM Empty the GL account
      laRelFld[lnPos1,6] = laTemp[2]
      *B602682,1 AMM end
    ELSE
      &lcVar = SPACE(12)
      *B602682,1 AMM Empty the GL account
      laRelFld[lnPos1,6] = SPACE(12)
      *B602682,1 AMM end
    ENDIF
  ENDIF
ENDIF
IF EMPTY(&lcVar)
  SHOW GET laRelFld[lnPos1,6] ENABLE
ELSE
  *B602682,1 AMM cOMMENT, gl account has the right account
  *laRelFld[lnPos1,6] = &lcVar
  *B602682,1 AMM end
  SHOW GET laRelFld[lnPos1,6] DISABLE 
ENDIF

USE IN APBANKS
IF !EMPTY(lcOldBnk)
  =gfOpenFile(lcOldBnk,'','SH')
ENDIF

USE IN APCHECKS
IF !EMPTY(lcOldchk)
  =gfOpenFile(lcOldchk,'','SH')
ENDIF